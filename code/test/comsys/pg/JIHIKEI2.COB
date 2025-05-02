      ******************************************************************
      *            IDENTIFICATION      DIVISION                        *
      ******************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             JIHIKEI2.
       AUTHOR.                 山田 浩之
      *
      *----------------------------------------------------------------*
      *   会計領収Ｆ、会計データＦ再計算
      *
      *----------------------------------------------------------------*
       DATE-WRITTEN.           20121207
       DATE-COMPILED.          20121207
      *----------------------------------------------------------------*
      *
      ******************************************************************
      *            ENVIRONMENT         DIVISION                        *
      ******************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
       OBJECT-COMPUTER.        FMV-DESKPOWER.
       SPECIAL-NAMES.          CONSOLE  IS  CONS
                               SYSERR   IS  MSGBOX.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
001861     SELECT  会計領収Ｆ      ASSIGN      TO        RYOSYUL
001862                             ORGANIZATION             IS  INDEXED
001863                             ACCESS MODE              IS  DYNAMIC
001864                             RECORD KEY               IS  領－施術区分
001865                                                          領－会計領収区分
001866                                                          領－施術和暦年月日
001867                                                          領－患者コード
001868                             ALTERNATE RECORD KEY     IS  領－施術区分
001869                                                          領－患者コード
001870                                                          領－会計領収区分
001871                                                          領－施術和暦年月日
001872                             FILE STATUS              IS  状態キー
001873                             LOCK        MODE         IS  AUTOMATIC.
001420     SELECT  会計データＦ    ASSIGN      TO        KAIKEIL
001430                             ORGANIZATION             IS  INDEXED
001440                             ACCESS MODE              IS  DYNAMIC
001450                             RECORD KEY               IS  会－施術和暦年月日
001460                                                          会－患者コード
001470                             ALTERNATE RECORD KEY     IS  会－患者コード
001480                                                          会－施術和暦年月日
001490                             FILE STATUS              IS  状態キー
001500                             LOCK        MODE         IS  AUTOMATIC.
      ******************************************************************
      *                      DATA DIVISION                             *
      ******************************************************************
       DATA                    DIVISION.
       FILE                    SECTION.
002401*
002402 FD  会計領収Ｆ     BLOCK   CONTAINS   1   RECORDS.
002403     COPY RYOSYU    OF  XFDLIB  JOINING   領  AS  PREFIX.
002320*
002330 FD  会計データＦ        BLOCK   CONTAINS   1   RECORDS.
002340     COPY KAIKEI     OF  XFDLIB  JOINING   会   AS  PREFIX.
      *----------------------------------------------------------------*
      ******************************************************************
      *                WORKING-STORAGE SECTION                         *
      ******************************************************************
       WORKING-STORAGE         SECTION.
      *
       01 キー入力                           PIC X    VALUE SPACE.
       01 状態キー                           PIC X(2) VALUE SPACE.
       01 終了フラグ                         PIC X(3) VALUE SPACE.
       01 ファイル名Ｗ                       PIC N(2) VALUE SPACE.
      *****************************************************************
      *                          連結項目                             *
      *****************************************************************
      *
       01 自費再計算－キー IS EXTERNAL.
000430     03 自費再計算－施術和暦年月.
000440         05 自費再計算－施術和暦       PIC 9.
000450         05 自費再計算－施術年月.
000460             07 自費再計算－施術年     PIC 9(2).
000470             07 自費再計算－施術月     PIC 9(2).
000480     03 自費再計算－施術日             PIC 9(2).
      *
      ******************************************************************
      *                      PROCEDURE  DIVISION                       *
      ******************************************************************
       PROCEDURE               DIVISION.
      ************
      *           *
      * 初期処理   *
      *           *
      ************
           PERFORM 初期化.
      ************
      *           *
      * 主処理     *
      *           *
      ************
           MOVE SPACE TO 終了フラグ
           MOVE 自費再計算－施術和暦年月 TO 会－施術和暦年月
           MOVE ZERO                     TO 会－施術日
           MOVE SPACE                    TO 会－患者コード
           START 会計データＦ KEY IS >= 会－施術和暦年月日
                                        会－患者コード
           END-START.
           IF 状態キー = ZERO
               PERFORM 会計ファイル読込
               PERFORM UNTIL (自費再計算－施術和暦年月 NOT = 会－施術和暦年月) OR
                             (終了フラグ                   = "YES")
                   IF 会－入金額 NOT = 会－自費額 + 会－一部負担金
                       MOVE ZERO               TO 領－施術区分
                       MOVE 1                  TO 領－会計領収区分
                       MOVE 会－施術和暦年月日 TO 領－施術和暦年月日
                       MOVE 会－患者コード     TO 領－患者コード
                       READ 会計領収Ｆ
                       NOT INVALID KEY
                           IF (会－入金額          > 会－一部負担金) AND
000580                        (領－自費項目内容(1) = SPACE         ) AND
000590                        (領－自費項目単価(1) = ZERO          ) AND
000600                        (領－自費項目数量(1) = ZERO          ) AND
000610                        (領－自費項目金額(1) = ZERO          )
                               PERFORM 会計書込処理
                           END-IF
                       END-READ
                   END-IF
                   PERFORM 会計ファイル読込
               END-PERFORM
           END-IF
      ************
      *           *
      * 終了処理   *
      *           *
      ************
           PERFORM 終了処理.
           EXIT PROGRAM.
      *
      *<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
      *************
      **           *
      ** 初期処理   *
      **           *
      *************
      *     PERFORM 初期化.
      *************
      **           *
      ** 主処理     *
      **           *
      *************
      *     MOVE SPACE TO 終了フラグ
      *     MOVE ZERO                     TO 領－施術区分
      *     MOVE 1                        TO 領－会計領収区分
      *     MOVE 自費再計算－施術和暦年月 TO 領－施術和暦年月
      *     MOVE ZERO                     TO 領－施術日
      *     MOVE SPACE                    TO 領－患者コード
      *     START 会計領収Ｆ KEY IS >= 領－施術区分
      *                                領－会計領収区分
      *                                領－施術和暦年月日
      *                                領－患者コード
      *     END-START.
      *     IF 状態キー = ZERO
      *         PERFORM 領収ファイル読込
      *         PERFORM UNTIL (領－施術区分             NOT = ZERO ) OR
      *                       (領－会計領収区分         NOT = 1    ) OR
      *                       (自費再計算－施術和暦年月 NOT = 領－施術和暦年月) OR
      *                       (終了フラグ                   = "YES")
      *             IF 領－施術日 NOT = ZERO
      *                 PERFORM 会計ファイル読込
      *                 IF (領－一部負担金  NOT = 会－一部負担金) AND
      *                    (領－入金額          > 会－一部負担金) AND
000580*                    (領－自費項目内容(1) = SPACE         ) AND
000590*                    (領－自費項目単価(1) = ZERO          ) AND
000600*                    (領－自費項目数量(1) = ZERO          ) AND
000610*                    (領－自費項目金額(1) = ZERO          )
      *                     PERFORM 領収書込処理
      *                     PERFORM 会計書込処理
      *                 END-IF
      *             END-IF
      **
      *             PERFORM 領収ファイル読込
      *         END-PERFORM
      *     END-IF.
      *************
      **           *
      ** 終了処理   *
      **           *
      *************
      *     PERFORM 終了処理.
      *     EXIT PROGRAM.
      **
      **<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
      *================================================================*
       初期化 SECTION.
      *
005440     OPEN INPUT 会計領収Ｆ.
005450             MOVE NC"領収" TO ファイル名Ｗ.
005460             PERFORM オープンチェック.
005440     OPEN I-O 会計データＦ.
005450             MOVE NC"会計" TO ファイル名Ｗ.
005460             PERFORM オープンチェック.
      *
      *================================================================*
       オープンチェック SECTION.
      *
           IF 状態キー  NOT =  "00"
               DISPLAY ファイル名Ｗ NC"Ｆオープンエラー" UPON CONS
               DISPLAY NC"状態キー：" 状態キー           UPON CONS
               DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
               ACCEPT  キー入力 FROM CONS
               PERFORM ファイル閉鎖
               EXIT PROGRAM.
      *================================================================*
       ファイル閉鎖 SECTION.
      *
           CLOSE 会計領収Ｆ 会計データＦ.
      *================================================================*
       終了処理 SECTION.
      *
           PERFORM ファイル閉鎖.
      *================================================================*
       領収書込処理 SECTION.
      *
000550     MOVE 会－費用額     TO 領－費用額.
000550     MOVE 会－一部負担金 TO 領－一部負担金.
000560     COMPUTE 領－自費額 = 領－入金額 - 領－一部負担金.
      *
           REWRITE 領－レコード.
           IF 状態キー  NOT =  "00"
               MOVE NC"領収"  TO ファイル名Ｗ
               PERFORM エラー表示
           END-IF.
014200*================================================================*
       会計書込処理 SECTION.
      *
000560     COMPUTE 会－自費額 = 会－入金額 - 会－一部負担金
      *
           REWRITE 会－レコード.
           IF 状態キー  NOT =  "00"
               MOVE NC"会計"  TO ファイル名Ｗ
               PERFORM エラー表示
           END-IF.
014200*================================================================*
       会計ファイル読込 SECTION.
      *
            READ 会計データＦ NEXT
            AT END
                MOVE "YES" TO 終了フラグ
            END-READ.
014200**================================================================*
      * 領収ファイル読込 SECTION.
      **
030510*      READ 会計領収Ｆ NEXT
      *      AT END
      *          MOVE "YES" TO 終了フラグ
      *      END-READ.
014200**================================================================*
      * 会計ファイル読込 SECTION.
      **
000290*      MOVE 領－施術和暦年月日 TO 会－施術和暦年月日
000360*      MOVE 領－患者コード     TO 会－患者コード
030510*      READ 会計データＦ
      *      INVALID KEY
HILO  *          DISPLAY "1-3 " 領－施術和暦年月日 領－患者コード
038550*          MOVE NC"会計" TO ファイル名Ｗ
      *          PERFORM エラー表示Ｒ
      *      END-READ.
014200*================================================================*
014060 エラー表示 SECTION.
014070*
014080     DISPLAY NC"ファイル書込エラー：" ファイル名Ｗ UPON CONS.
014090     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
014100     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
014110     ACCEPT  キー入力 FROM CONS.
002105*================================================================*
018940 エラー表示Ｒ SECTION.
018950*
018960     DISPLAY ファイル名Ｗ NC"ファイル読込エラー"   UPON CONS.
018970     DISPLAY NC"状態キー：" 状態キー               UPON CONS.
018980     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
018990     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
019000*-----------------------------------------*
019010     CALL "actcshm"  WITH C LINKAGE.
019020*-----------------------------------------*
019030     ACCEPT  キー入力 FROM CONS.
019040     PERFORM ファイル閉鎖.
019050     MOVE ZERO TO PROGRAM-STATUS
019060     EXIT PROGRAM.
019070*================================================================*
      ******************************************************************
       END PROGRAM JIHIKEI2.
      ******************************************************************
