000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             HMSAIDO3.
000060 AUTHOR.                 岡田 憲和
000070*
000080*----------------------------------------------------------------*
000090*   鍼灸マ   【前回同意日取得】
000100*----------------------------------------------------------------*
000110 DATE-WRITTEN.           2018-12-04
000120 DATE-COMPILED.          2018-12-04
000130*----------------------------------------------------------------*
000140******************************************************************
000150*            ENVIRONMENT         DIVISION                        *
000160******************************************************************
000170 ENVIRONMENT             DIVISION.
000180 CONFIGURATION           SECTION.
000190 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000200 OBJECT-COMPUTER.        FMV-DESKPOWER.
000210 SPECIAL-NAMES.          CONSOLE  IS  CONS
000220                         SYSERR   IS  MSGBOX.
000230 INPUT-OUTPUT            SECTION.
000240 FILE-CONTROL.
000250     SELECT  ＨレセプトＦ    ASSIGN      TO        HRECEL
000260                             ORGANIZATION             IS  INDEXED
000270                             ACCESS MODE              IS  DYNAMIC
000280                             RECORD KEY               IS  Ｈレセ−施術区分
000290                                                          Ｈレセ−施術和暦年月
000300                                                          Ｈレセ−患者コード
000310                                                          Ｈレセ−レセ種別
000320                             ALTERNATE RECORD KEY     IS  Ｈレセ−施術区分
000330                                                          Ｈレセ−患者コード
000340                                                          Ｈレセ−施術和暦年月
000350                                                          Ｈレセ−レセ種別
000360                             ALTERNATE RECORD KEY     IS  Ｈレセ−施術和暦年月
000370                                                          Ｈレセ−患者コード
000380                                                          Ｈレセ−レセ種別
000390                                                          Ｈレセ−施術区分
000400                             ALTERNATE RECORD KEY     IS  Ｈレセ−請求対象区分
000410                                                          Ｈレセ−請求和暦年月
000420                                                          Ｈレセ−施術区分
000430                                                          Ｈレセ−施術和暦年月
000440                                                          Ｈレセ−患者コード
000450                                                          Ｈレセ−レセ種別
000460                             FILE STATUS              IS  状態キー
000470                             LOCK        MODE         IS  AUTOMATIC.
000480*
000490******************************************************************
000500*                      DATA DIVISION                             *
000510******************************************************************
000520 DATA                    DIVISION.
000530 FILE                    SECTION.
000540*
000550 FD  ＨレセプトＦ        BLOCK   CONTAINS   1   RECORDS.
000560     COPY H_RECE     OF  XFDLIB  JOINING   Ｈレセ  AS  PREFIX.
000570*
000580******************************************************************
000590*                WORKING-STORAGE SECTION                         *
000600******************************************************************
000610 WORKING-STORAGE         SECTION.
000620 01 キー入力                           PIC X    VALUE SPACE.
000630 01 状態キー                           PIC X(2) VALUE SPACE.
000640 01 終了フラグ                         PIC X(3) VALUE SPACE.
000650 01 ファイル名                         PIC N(10) VALUE SPACE.
000660*
000670 01 プログラム名Ｗ PIC X(8) VALUE SPACE.
000680*
000690** 前回同意日用
000700 01 前回同意情報Ｗ.
000710    03 前回同意和暦年月日Ｗ.
000720       05 前回同意和暦年月Ｗ.
000730          07 前回同意和暦Ｗ            PIC 9.
000740          07 前回同意年月Ｗ.
000750              09 前回同意年Ｗ          PIC 9(2).
000760              09 前回同意月Ｗ          PIC 9(2).
000770          07 前回同意日Ｗ              PIC 9(2).
000780    03 傷病コードＷ                    PIC 9(2).
000790    03 傷病名Ｗ                        PIC X(100).
000800    03 同意医師コードＷ                PIC 9(3).
000810*
          03 合成傷病名Ｗ                       PIC X(300).
          03 加療期間開始和暦年月日Ｗ.
             05 加療期間開始和暦年月Ｗ.
                07 加療期間開始和暦Ｗ           PIC 9.
                07 加療期間開始年月Ｗ.
                   09 加療期間開始年Ｗ          PIC 9(2).
                   09 加療期間開始月Ｗ          PIC 9(2).
             05 加療期間開始日Ｗ                PIC 9(2).
          03 加療期間終了和暦年月日Ｗ.
             05 加療期間終了和暦年月Ｗ.
                07 加療期間終了和暦Ｗ           PIC 9.
                07 加療期間終了年月Ｗ.
                   09 加療期間終了年Ｗ          PIC 9(2).
                   09 加療期間終了月Ｗ          PIC 9(2).
             05 加療期間終了日Ｗ                PIC 9(2).
      *
000820*/ 連結項目退避 /*
000830 01 施術区分ＷＲ                       PIC 9(1).
000840 01 施術和暦年月ＷＲ.
000850    03 施術和暦ＷＲ                    PIC 9.
000860    03 施術年月ＷＲ.
000870       05 施術年ＷＲ                   PIC 9(2).
000880       05 施術月ＷＲ                   PIC 9(2).
000890 01 患者コードＷＲ.
000900    03 患者番号ＷＲ                    PIC 9(6).
000910    03 枝番ＷＲ                        PIC X.
000920 01 レセ種別ＷＲ                       PIC 9(2).
000930 01 負傷主キーＷＲ                     PIC 9(8).
000940 01 再同意和暦年月日ＷＲ               PIC X(7).
000950*
      *
       01 文字列結合用ＬＷ.
          03 文字列１ＬＷ                    PIC X(4096) VALUE SPACE.
          03 文字列２ＬＷ                    PIC X(512) VALUE SPACE.
          03 プログラム名ＬＷ                PIC X(8)  VALUE "strmoji2".
      *
      ********************************
      * 再同意予定日算出(処理前保存) *
      ********************************
       01 保存連再同−キー.
      */ in
          03 保存連再同−再同意和暦年月日      PIC 9(7).
          03 保存連再同−負傷主キー            PIC 9(8).
      *
          03 保存連再同−施術区分              PIC 9(1).
          03 保存連再同−施術内容区分          PIC 9(2).
      */ out
          03 保存連再同−再同意予定和暦年月日.
             05 保存連再同−再同意予定和暦年月.
                07 保存連再同−再同意予定和暦  PIC 9.
                07 保存連再同−再同意予定年    PIC 9(2).
                07 保存連再同−再同意予定月    PIC 9(2).
             05 保存連再同−再同意予定日       PIC 9(2).
          03 保存連再同−初療和暦年月日.
             05 保存連再同−初療和暦年月.
                07 保存連再同−初療和暦        PIC 9.
                07 保存連再同−初療年          PIC 9(2).
                07 保存連再同−初療月          PIC 9(2).
             05 保存連再同−初療日             PIC 9(2).
      */  ( 1:変形徒手あり )
          03 保存連再同−変形徒手フラグ        PIC 9(1).
      *
000960******************************************************************
000970*                          連結項目                              *
000980******************************************************************
000990**************************
001000* 前回同意日取得連携キー *
001010**************************
001020 01 連再同３−キー IS EXTERNAL.
001030*/ in
001040    03 連再同３−施術区分              PIC 9.
001050    03 連再同３−施術和暦年月.
001060       05 連再同３−施術和暦           PIC 9.
001070       05 連再同３−施術年月.
001080         07 連再同３−施術年           PIC 9(2).
001090         07 連再同３−施術月           PIC 9(2).
001100    03 連再同３−患者コード.
001110       05 連再同３−患者番号           PIC 9(6).
001120       05 連再同３−枝番               PIC X.
001130    03 連再同３−レセ種別              PIC 9(2).
001140    03 連再同３−負傷主キー            PIC 9(8).
001150    03 連再同３−再同意和暦年月日.
001160       05 連再同３−再同意和暦年月.
001170          07 連再同３−再同意和暦      PIC 9.
001180          07 連再同３−再同意年月.
001190             09 連再同３−再同意年     PIC 9(2).
001200             09 連再同３−再同意月     PIC 9(2).
001210          07 連再同３−再同意日        PIC 9(2).
001220*
001230*/ out
001240    03 連再同３−前回同意和暦年月日.
001250       05 連再同３−前回同意和暦年月.
001260          07 連再同３−前回同意和暦    PIC 9.
001270          07 連再同３−前回同意年月.
001280             09 連再同３−前回同意年   PIC 9(2).
001290             09 連再同３−前回同意月   PIC 9(2).
001300          07 連再同３−前回同意日      PIC 9(2).
001310*
001320    03 連再同３−傷病コード            PIC 9(2).
001330    03 連再同３−傷病名                PIC X(100).
001340    03 連再同３−同意医師コード        PIC 9(3).
001350*
       01 連再同３−傷病詳細 IS EXTERNAL.
          03 連再同３−合成傷病名                   PIC X(300).
          03 連再同３−加療期間開始和暦年月日.
             05 連再同３−加療期間開始和暦年月.
                07 連再同３−加療期間開始和暦       PIC 9.
                07 連再同３−加療期間開始年月.
                   09 連再同３−加療期間開始年      PIC 9(2).
                   09 連再同３−加療期間開始月      PIC 9(2).
             05 連再同３−加療期間開始日            PIC 9(2).
          03 連再同３−加療期間終了和暦年月日.
             05 連再同３−加療期間終了和暦年月.
                07 連再同３−加療期間終了和暦       PIC 9.
                07 連再同３−加療期間終了年月.
                   09 連再同３−加療期間終了年      PIC 9(2).
                   09 連再同３−加療期間終了月      PIC 9(2).
             05 連再同３−加療期間終了日            PIC 9(2).
          03 連再同３−前回同意終了和暦年月日.
             05 連再同３−前回同意終了和暦年月.
                07 連再同３−前回同意終了和暦       PIC 9.
                07 連再同３−前回同意終了年         PIC 9(2).
                07 連再同３−前回同意終了月         PIC 9(2).
             05 連再同３−前回同意終了日            PIC 9(2).
          03 連再同３−前回変形徒手フラグ           PIC 9(1).
      *
      ********************
      * 再同意予定日算出 *
      ********************
       01 連再同−キー IS EXTERNAL.
      */ in
          03 連再同−再同意和暦年月日      PIC 9(7).
          03 連再同−負傷主キー            PIC 9(8).
      *
          03 連再同−施術区分              PIC 9(1).
          03 連再同−施術内容区分          PIC 9(2).
      */ out
          03 連再同−再同意予定和暦年月日.
             05 連再同−再同意予定和暦年月.
                07 連再同−再同意予定和暦  PIC 9.
                07 連再同−再同意予定年    PIC 9(2).
                07 連再同−再同意予定月    PIC 9(2).
             05 連再同−再同意予定日       PIC 9(2).
          03 連再同−初療和暦年月日.
             05 連再同−初療和暦年月.
                07 連再同−初療和暦        PIC 9.
                07 連再同−初療年          PIC 9(2).
                07 連再同−初療月          PIC 9(2).
             05 連再同−初療日             PIC 9(2).
      */  ( 1:変形徒手あり )
          03 連再同−変形徒手フラグ        PIC 9(1).
      *
001360******************************************************************
001370*                      PROCEDURE  DIVISION                       *
001380******************************************************************
001390 PROCEDURE               DIVISION.
001400************
001410*           *
001420* 初期処理   *
001430*           *
001440************
001450     PERFORM ファイルオープン.
001460     PERFORM 連結項目待避.
001470************
001480*           *
001490* 主処理     *
001500*           *
001510************
001520     INITIALIZE 前回同意情報Ｗ.
001530*
001540     PERFORM 前回同意日取得.
001550*
001560     MOVE 前回同意和暦年月日Ｗ TO 連再同３−前回同意和暦年月日.
001570     MOVE 傷病コードＷ         TO 連再同３−傷病コード.
001580     MOVE 傷病名Ｗ             TO 連再同３−傷病名.
001590     MOVE 同意医師コードＷ     TO 連再同３−同意医師コード.
           MOVE 加療期間開始和暦年月日Ｗ TO 連再同３−加療期間開始和暦年月日.
           MOVE 加療期間終了和暦年月日Ｗ TO 連再同３−加療期間終了和暦年月日.
           MOVE 合成傷病名Ｗ             TO 連再同３−合成傷病名.
001600*
001610************
001620*           *
001630* 終了処理   *
001640*           *
001650************
001660     PERFORM 終了処理.
      *
      *    HMSAIDOの値を復元(他処理で直接共通変数を読んでいてもおかしくならない様にする為)
           MOVE 保存連再同−キー           TO 連再同−キー.
      *
001670     MOVE ZERO TO PROGRAM-STATUS.
001680     EXIT PROGRAM.
001690*
001700*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
001710*================================================================*
001720 ファイルオープン SECTION.
001730*================================================================*
001740     OPEN INPUT ＨレセプトＦ.
001750         MOVE NC"ＨレセプトＦ" TO ファイル名.
001760         PERFORM オープンチェック.
001770*
001780*================================================================*
001790 オープンチェック SECTION.
001800*
001810     IF ( 状態キー  NOT =  "00" )
001820         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
001830         DISPLAY NC"状態キー：" 状態キー         UPON CONS
001840         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
001850                                                 UPON CONS
001860*-----------------------------------------*
001870         CALL "actcshm"  WITH C LINKAGE
001880*-----------------------------------------*
001890         ACCEPT  キー入力 FROM CONS
001900         PERFORM ファイル閉鎖
001910         MOVE 99 TO PROGRAM-STATUS
001920         EXIT PROGRAM.
001930*
001940*================================================================*
001950 連結項目待避 SECTION.
001960*================================================================*

      *    HMSAIDOの値を保存
           MOVE 連再同−キー             TO 保存連再同−キー. 

001970     MOVE 連再同３−施術区分         TO 施術区分ＷＲ.
001980     MOVE 連再同３−施術和暦年月     TO 施術和暦年月ＷＲ.
001990     MOVE 連再同３−患者コード       TO 患者コードＷＲ.
002000     MOVE 連再同３−レセ種別         TO レセ種別ＷＲ.
002010     MOVE 連再同３−負傷主キー       TO 負傷主キーＷＲ.
002020     MOVE 連再同３−再同意和暦年月日 TO 再同意和暦年月日ＷＲ.
002030*
002040*================================================================*
002050 前回同意日取得 SECTION.
002060*================================================================*
002070*
002080     MOVE 施術区分ＷＲ       TO Ｈレセ−施術区分.
002090     MOVE 患者番号ＷＲ       TO Ｈレセ−患者番号.
002100     MOVE 枝番ＷＲ           TO Ｈレセ−枝番.
002110     MOVE 施術和暦ＷＲ       TO Ｈレセ−施術和暦.
002120     MOVE 施術年ＷＲ         TO Ｈレセ−施術年.
002130     MOVE 施術月ＷＲ         TO Ｈレセ−施術月.
002140     MOVE レセ種別ＷＲ       TO Ｈレセ−レセ種別.
002150*
002160     START ＨレセプトＦ KEY IS <  Ｈレセ−施術区分
002170                                  Ｈレセ−患者コード
002180                                  Ｈレセ−施術和暦年月
002190                                  Ｈレセ−レセ種別
002200                                  REVERSED
002210     END-START.
002220     IF 状態キー = "00"
002230        MOVE SPACE TO 終了フラグ
002240        PERFORM ＨレセプトＦ読込
002250*       繰り返し（同一患者番号６けたで同一負傷主キー　枝番考慮）
002260        PERFORM UNTIL ( Ｈレセ−施術区分    NOT = 施術区分ＷＲ   ) OR
002270                      ( Ｈレセ−患者番号    NOT = 患者番号ＷＲ   ) OR
002280                      ( Ｈレセ−負傷主キー  NOT = 負傷主キーＷＲ ) OR
002290                      ( Ｈレセ−同意和暦年月日  NOT = 再同意和暦年月日ＷＲ ) OR
002300                      ( 終了フラグ          NOT = SPACE )
002310            PERFORM ＨレセプトＦ読込
002320        END-PERFORM
002330*
002340        IF ( Ｈレセ−施術区分    = 施術区分ＷＲ   ) AND
002350           ( Ｈレセ−患者番号    = 患者番号ＷＲ   ) AND
002360           ( Ｈレセ−負傷主キー  = 負傷主キーＷＲ ) AND
002370           ( 終了フラグ          = SPACE )
002380*
002390           IF ( Ｈレセ−同意和暦年月日 NOT = 再同意和暦年月日ＷＲ )
002400              MOVE Ｈレセ−同意和暦年月日 TO 前回同意和暦年月日Ｗ
002410              MOVE Ｈレセ−傷病コード     TO 傷病コードＷ
002420              MOVE Ｈレセ−傷病名         TO 傷病名Ｗ
                    IF Ｈレセ−施術区分 = 1
                        PERFORM はりきゅう傷病名編集
                    END-IF
002430              MOVE Ｈレセ−同意医師コード TO 同意医師コードＷ
                    MOVE Ｈレセ−加療期間開始和暦年月日 TO 加療期間開始和暦年月日Ｗ
                    MOVE Ｈレセ−加療期間終了和暦年月日 TO 加療期間終了和暦年月日Ｗ
003479*
      *             前同開の有効期限を取得
                    INITIALIZE 連再同−キー
                    MOVE Ｈレセ−同意和暦年月日 TO 連再同−再同意和暦年月日
                    MOVE Ｈレセ−負傷主キー     TO 連再同−負傷主キー
                    MOVE Ｈレセ−施術区分       TO 連再同−施術区分
                    MOVE Ｈレセ−施術内容区分   TO 連再同−施術内容区分
      *
                    MOVE "HMSAIDO" TO プログラム名Ｗ
                    CALL プログラム名Ｗ
                    CANCEL プログラム名Ｗ
                    MOVE 連再同−再同意予定和暦年月日 TO 連再同３−前回同意終了和暦年月日
                    MOVE 連再同−変形徒手フラグ       TO 連再同３−前回変形徒手フラグ
002440           END-IF
002450        END-IF
002460     END-IF.
002470*
      *================================================================*
       はりきゅう傷病名編集 SECTION.
      *
               MOVE SPACE                                TO 文字列１ＬＷ 文字列２ＬＷ
      *
               EVALUATE Ｈレセ−傷病コード
               WHEN 1
                   MOVE "神経痛"                         TO 文字列１ＬＷ
               WHEN 2
                   MOVE "リウマチ"                       TO 文字列１ＬＷ
               WHEN 3
                   MOVE "頸腕症候群"                     TO 文字列１ＬＷ
               WHEN 4
                   MOVE "五十肩"                         TO 文字列１ＬＷ
               WHEN 5
                   MOVE "腰痛症"                         TO 文字列１ＬＷ
               WHEN 6
                   MOVE "頸椎捻挫後遺症"                 TO 文字列１ＬＷ
               WHEN 7
                   MOVE "その他("                        TO 文字列１ＬＷ
                   MOVE 傷病名Ｗ                         TO 文字列２ＬＷ
                   CALL プログラム名ＬＷ   WITH C LINKAGE
                                     USING BY REFERENCE 文字列１ＬＷ
                                           BY REFERENCE 文字列２ＬＷ
                   MOVE ")"                              TO 文字列２ＬＷ
                   CALL プログラム名ＬＷ   WITH C LINKAGE
                                     USING BY REFERENCE 文字列１ＬＷ
                                           BY REFERENCE 文字列２ＬＷ
               WHEN 9
      *            複数傷病への○付け
                   IF Ｈレセ−複数傷病区分(1) = 1
                       MOVE "神経痛"                     TO 文字列１ＬＷ
                   END-IF
                   IF Ｈレセ−複数傷病区分(2) = 1
                       IF 文字列１ＬＷ = SPACE
                           MOVE "リウマチ"               TO 文字列１ＬＷ
                       ELSE
                           MOVE " リウマチ"              TO 文字列２ＬＷ
                           CALL プログラム名ＬＷ   WITH C LINKAGE
                                         USING BY REFERENCE 文字列１ＬＷ
                                               BY REFERENCE 文字列２ＬＷ
                       END-IF
                   END-IF
                   IF Ｈレセ−複数傷病区分(3) = 1
                       IF 文字列１ＬＷ = SPACE
                           MOVE "頸腕症候群"             TO 文字列１ＬＷ
                       ELSE
                           MOVE " 頸腕症候群"            TO 文字列２ＬＷ
                           CALL プログラム名ＬＷ   WITH C LINKAGE
                                         USING BY REFERENCE 文字列１ＬＷ
                                               BY REFERENCE 文字列２ＬＷ
                       END-IF
                   END-IF
                   IF Ｈレセ−複数傷病区分(4) = 1
                       IF 文字列１ＬＷ = SPACE
                           MOVE "五十肩"                 TO 文字列１ＬＷ
                       ELSE
                           MOVE " 五十肩"                TO 文字列２ＬＷ
                           CALL プログラム名ＬＷ   WITH C LINKAGE
                                         USING BY REFERENCE 文字列１ＬＷ
                                               BY REFERENCE 文字列２ＬＷ
                       END-IF
                   END-IF
                   IF Ｈレセ−複数傷病区分(5) = 1
                       IF 文字列１ＬＷ = SPACE
                           MOVE "腰痛症"                 TO 文字列１ＬＷ
                       ELSE
                           MOVE " 腰痛症"                TO 文字列２ＬＷ
                           CALL プログラム名ＬＷ   WITH C LINKAGE
                                         USING BY REFERENCE 文字列１ＬＷ
                                               BY REFERENCE 文字列２ＬＷ
                       END-IF
                   END-IF
                   IF Ｈレセ−複数傷病区分(6) = 1
                       IF 文字列１ＬＷ = SPACE
                           MOVE "頸椎捻挫後遺症"         TO 文字列１ＬＷ
                       ELSE
                           MOVE " 頸椎捻挫後遺症"        TO 文字列２ＬＷ
                           CALL プログラム名ＬＷ   WITH C LINKAGE
                                         USING BY REFERENCE 文字列１ＬＷ
                                               BY REFERENCE 文字列２ＬＷ
                       END-IF
                   END-IF
                   IF Ｈレセ−複数傷病区分(7) = 1
                       IF 文字列１ＬＷ = SPACE
                           MOVE "その他("                        TO 文字列１ＬＷ
                           MOVE 傷病名Ｗ                         TO 文字列２ＬＷ
                           CALL プログラム名ＬＷ   WITH C LINKAGE
                                         USING BY REFERENCE 文字列１ＬＷ
                                               BY REFERENCE 文字列２ＬＷ
                           MOVE ")"                              TO 文字列２ＬＷ
                           CALL プログラム名ＬＷ   WITH C LINKAGE
                                         USING BY REFERENCE 文字列１ＬＷ
                                               BY REFERENCE 文字列２ＬＷ
                       ELSE
                           MOVE " その他("               TO 文字列２ＬＷ
                           CALL プログラム名ＬＷ   WITH C LINKAGE
                                         USING BY REFERENCE 文字列１ＬＷ
                                               BY REFERENCE 文字列２ＬＷ
                           MOVE 傷病名Ｗ                 TO 文字列２ＬＷ
                           CALL プログラム名ＬＷ   WITH C LINKAGE
                                         USING BY REFERENCE 文字列１ＬＷ
                                               BY REFERENCE 文字列２ＬＷ
                           MOVE ")"                      TO 文字列２ＬＷ
                           CALL プログラム名ＬＷ   WITH C LINKAGE
                                         USING BY REFERENCE 文字列１ＬＷ
                                               BY REFERENCE 文字列２ＬＷ
                       END-IF
                   END-IF
               END-EVALUATE.
      *
               MOVE 文字列１ＬＷ                         TO 合成傷病名Ｗ.
002480*================================================================*
002490 ＨレセプトＦ読込 SECTION.
002500*
002510     READ ＨレセプトＦ NEXT
002520     AT END
002530        MOVE "YES" TO 終了フラグ
002540     END-READ.
002550*
002560*================================================================*
002570 終了処理 SECTION.
002580*================================================================*
002590     PERFORM ファイル閉鎖.
002600*
002610*================================================================*
002620 ファイル閉鎖 SECTION.
002630*
002640     CLOSE ＨレセプトＦ.
002650*
002660*================================================================*
002670******************************************************************
002680 END PROGRAM HMSAIDO3.
002690******************************************************************
002700
