000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             NJY101.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090* Ｎ・Ｊ 会提出フロッピー作成【FPD書込】
000100* 請求年月Ver.
000110*      MED = NJY580 
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2019-08-08
000140 DATE-COMPILED.          2019-08-08
000150*----------------------------------------------------------------*
000160******************************************************************
000170*            ENVIRONMENT         DIVISION                        *
000180******************************************************************
000190 ENVIRONMENT             DIVISION.
000200 CONFIGURATION           SECTION.
000210 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000220 OBJECT-COMPUTER.        FMV-DESKPOWER.
000230 SPECIAL-NAMES.          CONSOLE  IS  CONS
000240                         SYSERR   IS  MSGBOX.
000250 INPUT-OUTPUT            SECTION.
000260 FILE-CONTROL.
000270     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000280                             ORGANIZATION             IS  INDEXED
000290                             ACCESS MODE              IS  DYNAMIC
000300                             RECORD KEY               IS  受－施術和暦年月
000310                                                          受－患者コード
000320                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000330                                                          受－患者カナ
000340                                                          受－患者コード
000350                             ALTERNATE RECORD KEY     IS  受－患者コード
000360                                                          受－施術和暦年月
000370                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000380                                                          受－保険種別
000390                                                          受－保険者番号
000400                                                          受－患者コード
000410                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000420                                                          受－公費種別
000430                                                          受－費用負担者番号
000440                                                          受－患者コード
000450                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000460                                                          受－助成種別
000470                                                          受－費用負担者番号助成
000480                                                          受－患者コード
000490                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
000500                                                          受－施術和暦年月
000510                                                          受－患者コード
000520                             FILE STATUS              IS  状態キー
000530                             LOCK        MODE         IS  AUTOMATIC.
           SELECT  レセプトＦ      ASSIGN      TO        RECEPTL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  レセ－施術和暦年月
                                                                レセ－患者コード
                                                                レセ－レセ種別
                                   ALTERNATE RECORD KEY     IS  レセ－患者コード
                                                                レセ－施術和暦年月
                                                                レセ－レセ種別
                                   ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
                                                                レセ－施術和暦年月
                                                                レセ－患者コード
                                                                レセ－レセ種別
                                   ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
                                                                レセ－レセ種別
                                                                レセ－請求保険者番号
                                                                レセ－患者コード
                                                                レセ－施術和暦年月
                                   ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
                                                                レセ－請求保険者番号
                                                                レセ－患者コード
                                                                レセ－レセ種別
                                                                レセ－施術和暦年月
                                   FILE STATUS              IS  状態キー
                                   LOCK        MODE         IS  AUTOMATIC.
000630     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
000640                             ORGANIZATION             IS  INDEXED
000650                             ACCESS MODE              IS  DYNAMIC
000660                             RECORD KEY               IS  施記－施術和暦年月日
000670                                                          施記－患者コード
000680                             ALTERNATE RECORD KEY     IS  施記－患者コード
000690                                                          施記－施術和暦年月日
000700                             FILE STATUS              IS  状態キー
000710                             LOCK        MODE         IS  AUTOMATIC.
000720     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000730                             ORGANIZATION             IS  INDEXED
000740                             ACCESS MODE              IS  DYNAMIC
000750                             RECORD KEY               IS 施情－施術所番号
000760                             FILE STATUS              IS  状態キー
000770                             LOCK        MODE         IS  AUTOMATIC.
000780     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000790                             ORGANIZATION             IS  INDEXED
000800                             ACCESS MODE              IS  DYNAMIC
000810                             RECORD KEY               IS  元－元号区分
000820                             FILE STATUS              IS  状態キー
000830                             LOCK        MODE         IS  AUTOMATIC.
001290*
001520     SELECT 作業ファイル１ ASSIGN    TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1012L.DAT"
001530                             ORGANIZATION             IS  INDEXED
001540                             ACCESS MODE              IS  DYNAMIC
001250                             RECORD      KEY          IS  作１－保険者番号順
000908                                                          作１－返戻区分
000908                                                          作１－本人家族区分
001260                                                          作１－医コード
001300                                                          作１－施術和暦年月
001280                                                          作１－患者カナ
001290                                                          作１－患者コード
001590                             FILE STATUS              IS  状態キー
001600                             LOCK        MODE         IS  AUTOMATIC.
001601*
001602     SELECT 会提出ファイル   ASSIGN      TO     FD-NAME
001603                             ORGANIZATION             IS  LINE SEQUENTIAL
001604                             ACCESS MODE              IS  SEQUENTIAL
001605                             FILE STATUS              IS  状態キー
001606                             LOCK      MODE           IS  AUTOMATIC.
001610******************************************************************
001620*                      DATA DIVISION                             *
001630******************************************************************
001640 DATA                    DIVISION.
001650 FILE                    SECTION.
001660*                           ［ＲＬ＝  ３２０］
001670 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
001680     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
      *
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
001720*                           ［ＲＬ＝  ２５６］
001730 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
001740     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
001750*
001760 FD  施術所情報マスタ    BLOCK   CONTAINS   1   RECORDS.
001770     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
001780*                           ［ＲＬ＝  １２８］
001790 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001800     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
004330*                           ［ＲＬ＝  ３６０］
004340 FD  作業ファイル１ BLOCK  CONTAINS 360  RECORDS.
004350 01 作１－レコード.
004360    03 作１－レコードキー.
001840         05  作１－保険者番号順            PIC 9(8).
               05  作１－返戻区分                PIC 9(1).
001860         05  作１－本人家族区分            PIC 9.
               05  作１－医コード                PIC Z9.
001910         05  作１－施術和暦年月.
001920             07  作１－施術和暦            PIC 9.
001930             07  作１－施術年              PIC 9(2).
001940             07  作１－施術月              PIC 9(2).
001870         05  作１－患者カナ                PIC X(50).
001880         05  作１－患者コード.
001890             07 作１－患者番号             PIC 9(6).
001900             07 作１－枝番                 PIC X(1).
001950     03  作１－レコードデータ.
001840         05  作１－保険者番号              PIC X(8).
001970         05  作１－保険者番号親            PIC X(8).
004487         05  作１－記号                    PIC X(24).
004487         05  作１－番号                    PIC X(30).
001980         05  作１－患者氏名                PIC X(50).
001990         05  作１－被保険者カナ            PIC X(50).
001990         05  作１－被保険者氏名            PIC X(50).
002000         05  作１－合計金額                PIC ZZZZZ9.
002010         05  作１－一部負担金              PIC ZZZZZ9.
002020         05  作１－請求金額                PIC ZZZZZ9.
001910         05  作１－施術年月                PIC X(10).
001910         05  作１－提出年月                PIC X(10).
001910         05  作１－生年月日                PIC X(10).
002040         05  作１－負担割合                PIC Z9.
002040         05  作１－実日数                  PIC Z9.
002050         05  FILLER                        PIC X(13).
004471*
004477 FD  会提出ファイル RECORD IS VARYING IN SIZE
004478               FROM 1 TO 370 DEPENDING ON 文字カウンタ.
004479 01 会提－レコード.
004480   03 会提－レコードデータ.
            05 会提－データ PIC X OCCURS 1 TO 370 TIMES DEPENDING ON 文字カウンタ.
004508*
004509*----------------------------------------------------------------*
004510******************************************************************
004511*                WORKING-STORAGE SECTION                         *
004520******************************************************************
004530 WORKING-STORAGE         SECTION.
004540 01 キー入力                           PIC X    VALUE SPACE.
004550 01 状態キー                           PIC X(2) VALUE SPACE.
004560 01 終了フラグ                         PIC X(3) VALUE SPACE.
004570 01 終了フラグ２                       PIC X(3) VALUE SPACE.
004580 01 終了フラグ３                       PIC X(3) VALUE SPACE.
004581 01 終了フラグ４                       PIC X(3) VALUE SPACE.
004590 01 実行キーＷ                         PIC X(3)  VALUE SPACE.
004600 01 施術記録有Ｗ                       PIC X(3) VALUE SPACE.
004610 01 ファイル名                         PIC N(8) VALUE SPACE.
004620 01 カウンタ                           PIC 9(1) VALUE ZERO.
004621 01 カウンタ２                         PIC 9(1) VALUE ZERO.
004630 01 文字カウンタ                       PIC 9(4) VALUE ZERO.
003630 01 文字ＣＮＴ                         PIC 9(2) VALUE ZERO.
004630 01 桁位置Ｗ                           PIC 9(4) VALUE ZERO.
004640 01 通番Ｗ.
          03 年月ＷＰ                        PIC 9(4) VALUE ZERO.
          03 会員ＩＤＷＰ                    PIC 9(4) VALUE ZERO.
          03 通番ＷＰ                        PIC 9(4) VALUE ZERO.
004650 01 預金種別Ｗ                         PIC X(1) VALUE SPACE.
004730 01 請求和暦年月ＷＲ.
004740    03 請求和暦ＷＲ                    PIC 9(1) VALUE ZERO.
004750    03 請求年月ＷＲ.
004760       05 請求年ＷＲ                   PIC 9(2) VALUE ZERO.
004770       05 請求月ＷＲ                   PIC 9(2) VALUE ZERO.
004780*
004730 01 西暦年月日ＷＲ.
004730    03 西暦年月ＷＲ.
004760       05 西暦年ＷＲ                   PIC 9(4) VALUE ZERO.
             05 年区切ＷＲ                   PIC X(1) VALUE "/".
004770       05 西暦月ＷＲ                   PIC 9(2) VALUE ZERO.
          03 月区切ＷＲ                      PIC X(1) VALUE "/".
004770    03 西暦日ＷＲ                      PIC 9(2) VALUE ZERO.
004780*
004990 01 実日数Ｗ                           PIC 9(2) VALUE ZERO.
005140*
005150 01 英数字項目Ｗ.
005160   03 英数字項目ＸＷ                   PIC X(70) VALUE SPACE.
005170 01 日本語項目Ｗ.
005180   03 日本語項目ＮＷ                   PIC N(35) VALUE SPACE.
005190 01 英数字項目２Ｗ                     PIC X(70) VALUE SPACE.
005200*
005382 01 会員番号Ｗ.
          03 会員県番Ｗ                      PIC X(2)  VALUE SPACE.
          03 会員コードＷ                    PIC X(4)  VALUE SPACE.
      *
005580 01 負担率Ｗ                           PIC 9(3) VALUE ZERO.
005710*
002251 01 定期修正ＦＤＷ                     PIC X VALUE SPACE.
005723 01 FD-NAME                            PIC X(30) VALUE SPACE.
005724*
005725 01 保険種別Ｗ                         PIC X(2)  VALUE SPACE.
004780*
007570 01 記号Ｗ.
007580    03 印刷記号Ｗ                      PIC N(12)  VALUE SPACE.
007660*
007670 01 番号Ｗ.
007680    03 印刷番号Ｗ                      PIC X(30)  VALUE SPACE.
      *
002980** 保険者番号右詰め用
002990 01 保険者番号ＷＴ.
003000    03 保険者番号左詰めＷ.
003010      05 保険者番号左詰めＷ１          PIC X OCCURS 8 VALUE SPACE.
003020    03 保険者番号右詰めＷ.
003030      05 保険者番号右詰めＷ１          PIC X OCCURS 8 VALUE ZERO.
003040    03 保険者番号数字Ｗ                PIC 9(8)  VALUE ZERO.
003050    03 保険者番号Ｗ                    PIC X(8)  VALUE SPACE.
      *
002980**数字左詰め用
002990 01 数字ＷＴ.
003000    03 数字左詰めＷ.
003010      05 数字左詰めＷ１                PIC X OCCURS 6 VALUE SPACE.
003020    03 数字右詰めＷ.
003030      05 数字右詰めＷ１                PIC X OCCURS 6 VALUE ZERO.
003050    03 数字Ｗ                          PIC X(6)  VALUE SPACE.
003250*
       01 改行Ｗ                             PIC X(2)  VALUE X"0D0A" GLOBAL.
      *
002251 01 レコードデータＷ                   PIC X(370) VALUE SPACE.
014774*
       01 複合プログラム名Ｗ                 PIC X(8) VALUE "MOJI2".
006170*****************************************************************
003340** 西暦年月日ワーク用
003350 01 計算西暦年月日Ｗ.
003360    03 計算西暦年Ｗ                    PIC 9(4) VALUE ZERO.
003370    03 計算西暦月Ｗ                    PIC 9(2) VALUE ZERO.
003380    03 計算西暦日Ｗ                    PIC 9(2) VALUE ZERO.
003390 01 計算和暦年月日Ｗ.
003400    03 計算和暦Ｗ                      PIC 9 VALUE ZERO.
003410    03 計算年Ｗ                        PIC 9(2) VALUE ZERO.
003420    03 計算月Ｗ                        PIC 9(2) VALUE ZERO.
003430    03 計算日Ｗ                        PIC 9(2) VALUE ZERO.
006320*
005730******************************************************************
005740*                          連結項目                              *
005750******************************************************************
005760*
005770********************
005780* メッセージ表示キー *
005790********************
005800 01 連メ－キー IS EXTERNAL.
005810    03  連メ－メッセージ               PIC N(20).
005820*
005830 01 連メ３－キー IS EXTERNAL.
005840    03  連メ３－メッセージ             PIC N(20).
005850    03  連メ３－メッセージ１           PIC X(20).
005860*
004091 01 連メ７－キー IS EXTERNAL.
004092    03  連メ７－メッセージ１           PIC X(40).
004102    03  連メ７－メッセージ２           PIC X(40).
005860*
005870****************
005880* 画面入力情報 *
005890****************
       01 連入－画面情報ＮＪＹ１００ IS EXTERNAL.
          03 連入－請求和暦年月.
             05 連入－請求和暦                 PIC 9.
             05 連入－請求年月.
                07 連入－請求年                PIC 9(2).
                07 連入－請求月                PIC 9(2).
          03 連入－提出和暦年月.
             05 連入－提出和暦                 PIC 9.
             05 連入－提出年月.
                07 連入－提出年                PIC 9(2).
                07 連入－提出月                PIC 9(2).
          03 連入－ドライブ                    PIC X(1).
      *
      *0:通常 1:固定パス
       01 連入－固定フラグ３２１１ IS EXTERNAL.
          03 連入－固定フラグ                PIC 9(1).
      *
      * 暗号複合用
       01 連暗号複合－暗号情報 IS EXTERNAL.
          03 連暗号複合－入力情報.
             05 連暗号複合－記号               PIC X(24).
             05 連暗号複合－番号               PIC X(30).
             05 連暗号複合－暗号化項目.
                07 連暗号複合－暗号患者番号    PIC X(6).
                07 連暗号複合－暗号判定記号    PIC X.
                07 連暗号複合－暗号判定番号    PIC X.
                07 連暗号複合－暗号記号        PIC X(24).
                07 連暗号複合－暗号番号        PIC X(30).
          03 連暗号複合－出力情報.
             05 連暗号複合－複合した記号       PIC X(24).
             05 連暗号複合－複合した番号       PIC X(30).
006280*
006290******************************************************************
006300*                      PROCEDURE  DIVISION                       *
006310******************************************************************
006320 PROCEDURE               DIVISION.
006330************
006340*           *
006350* 初期処理   *
006360*           *
006370************
006380     PERFORM 初期化.
006390************
006400*           *
006410* 主処理     *
006420*           *
006430************
006450     PERFORM 連結項目退避.
006470     PERFORM 作業ファイル作成.
006480     PERFORM クライアントファイル作成.
006500************
006510*           *
006520* 終了処理   *
006530*           *
006540************
006550     PERFORM 終了処理.
006560     MOVE ZERO TO PROGRAM-STATUS.
006570     EXIT PROGRAM.
006580*
006590*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
006600*================================================================*
006610 初期化 SECTION.
006620*
006630     PERFORM ファイルオープン.
006640*
006650*================================================================*
006660 ファイルオープン SECTION.
006670*
006680     OPEN INPUT 受診者情報Ｆ
006690         MOVE NC"受" TO ファイル名.
006700         PERFORM オープンチェック.
004570     OPEN INPUT レセプトＦ.
004580         MOVE NC"レセ" TO ファイル名.
004590         PERFORM オープンチェック.
006740     OPEN INPUT 施術記録Ｆ.
006750             MOVE NC"施記" TO ファイル名.
006760             PERFORM オープンチェック.
006770     OPEN INPUT 施術所情報マスタ
006780         MOVE NC"施情" TO ファイル名.
006790         PERFORM オープンチェック.
006800     OPEN INPUT 元号マスタ.
006810             MOVE NC"元号" TO ファイル名.
006820             PERFORM オープンチェック.
006980*
007091
028320     PERFORM 施術所情報取得.
007094* フロッピー挿入確認
           IF 連入－固定フラグ = 1
              STRING "C:\makishisys\"   DELIMITED BY SIZE
                     会員コードＷ       DELIMITED BY SIZE
                     "Seikyu.txt"       DELIMITED BY SIZE
007098              INTO FD-NAME
              END-STRING
           ELSE
007096        STRING 連入－ドライブ     DELIMITED BY SIZE
                     ":\"               DELIMITED BY SIZE
                     会員コードＷ       DELIMITED BY SIZE
                     "Seikyu.txt"       DELIMITED BY SIZE
007098              INTO FD-NAME
007099        END-STRING
           END-IF.
007100
007101     OPEN OUTPUT 会提出ファイル.
003770     IF 状態キー  NOT =  "00"
002690         MOVE  "指定されたドライブがないか、" TO 連メ７－メッセージ１
               MOVE  "書き込むことが出来ません。" TO 連メ７－メッセージ２
002700         CALL   "MSG007"
002710         CANCEL "MSG007"
007240         PERFORM ファイル閉鎖
003820         MOVE 99 TO PROGRAM-STATUS
003830         EXIT PROGRAM
007108     ELSE
007109         PERFORM オープンチェック
007110         CLOSE 会提出ファイル
007111         CALL "delfile"  WITH C LINKAGE
007112                         USING BY REFERENCE FD-NAME
007113     END-IF.
007160*================================================================*
007170 オープンチェック SECTION.
007180*
007190     IF 状態キー  NOT =  "00"
007200         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
007210         DISPLAY NC"状態キー：" 状態キー         UPON CONS
007220         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
004380*-----------------------------------------*
004390         CALL "actcshm"  WITH C LINKAGE
004400*-----------------------------------------*
007230         ACCEPT  キー入力 FROM CONS
007240         PERFORM ファイル閉鎖
007250         MOVE 99 TO PROGRAM-STATUS
007260         EXIT PROGRAM.
007270*================================================================*
007280 ファイル閉鎖 SECTION.
007290*
007300     CLOSE 受診者情報Ｆ        施術所情報マスタ
007310           元号マスタ          レセプトＦ.
007340*================================================================*
007350 終了処理 SECTION.
007360*
007370     PERFORM ファイル閉鎖.
007380*================================================================*
007390 エラー表示 SECTION.
007400*
007410     DISPLAY NC"状態キー" 状態キー  UPON CONS.
007420     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
007430     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
007440     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
004910*-----------------------------------------*
004920     CALL "actcshm"  WITH C LINKAGE.
004930*-----------------------------------------*
007450     ACCEPT  キー入力 FROM CONS.
007460     PERFORM ファイル閉鎖.
007470     MOVE 99 TO PROGRAM-STATUS.
007480     EXIT PROGRAM.
009500*================================================================*
009510 施術所情報取得 SECTION.
009520*
009530     MOVE ZERO  TO 施情－施術所番号.
009540     READ 施術所情報マスタ
009550     INVALID KEY
009560          MOVE  NC"施術所情報マスタに登録後、実行して下さい" TO 連メ－メッセージ
009570          CALL   "MSG001"
009580          CANCEL "MSG001"
009590          PERFORM ファイル閉鎖
009600          MOVE 99 TO PROGRAM-STATUS
009610          EXIT PROGRAM
009620     
009621     NOT INVALID KEY
009622          MOVE 施情－接骨師会会員番号   TO 会員番号Ｗ
009624     END-READ.
009630*
009810*================================================================*
009820 連結項目退避 SECTION.
009830*
009840     MOVE 連入－請求和暦 TO 請求和暦ＷＲ.
009850     MOVE 連入－請求年   TO 請求年ＷＲ.
009860     MOVE 連入－請求月   TO 請求月ＷＲ.
010650*================================================================*
010740 レセプトＦ読込 SECTION.
010750*
010760     READ レセプトＦ NEXT
010770     AT END
010780         MOVE "YES" TO 終了フラグ
010790     END-READ.
010800*================================================================*
010810 健保レコードセット SECTION.
010820*
011870*/保険証記号番号
           MOVE SPACE TO 連暗号複合－暗号情報.
      *
      *    / 連暗号複合－入力情報セット /
           MOVE 受－記号       TO 連暗号複合－記号.
           MOVE 受－番号       TO 連暗号複合－番号.
           MOVE 受－暗号化項目 TO 連暗号複合－暗号化項目.
      *     
           CALL   複合プログラム名Ｗ.
           CANCEL 複合プログラム名Ｗ.
      *
           MOVE 連暗号複合－複合した記号 TO 記号Ｗ.
           MOVE 連暗号複合－複合した番号 TO 番号Ｗ.
011880     IF (印刷記号Ｗ(1:1) NOT = NC"＊")
011890         MOVE 記号Ｗ               TO 作１－記号
011900     END-IF.
011910     IF (番号Ｗ(1:1) NOT = "*") AND (番号Ｗ(1:2) NOT = "＊")
011920         MOVE 番号Ｗ               TO 作１－番号
011930     END-IF.
010830*/料金
010843     MOVE レセ－合計          TO 作１－合計金額.
010850     MOVE レセ－一部負担金    TO 作１－一部負担金.
010860     MOVE レセ－請求金額      TO 作１－請求金額.
011125*================================================================*
011130 助成レコードセット SECTION.
011140*
011920     MOVE 受－受益者番号助成  TO 作１－番号.
011150*/料金
011170     MOVE ZERO                TO 作１－合計金額.
011180     MOVE レセ－受給者負担額  TO 作１－一部負担金.
011190     MOVE レセ－助成請求金額  TO 作１－請求金額.
011405*
011406*================================================================*
011410 共通レコードセット SECTION.
011420*
026990     MOVE 受－患者番号               TO 作１－患者番号.
026990     MOVE 受－枝番                   TO 作１－枝番.
027000     MOVE 受－施術和暦年月           TO 作１－施術和暦年月.
           MOVE 受－施術和暦年月           TO 計算和暦年月日Ｗ.
015440     PERFORM 西暦年月日取得.
           MOVE 01                         TO 西暦日ＷＲ.
015450     MOVE 西暦年月日ＷＲ             TO 作１－施術年月.
           MOVE 連入－提出和暦年月         TO 計算和暦年月日Ｗ.
015440     PERFORM 西暦年月日取得.
           MOVE 01                         TO 西暦日ＷＲ.
015450     MOVE 西暦年月日ＷＲ             TO 作１－提出年月.
011520*/続柄０：本人、０以外の自然数：家族
011590     MOVE 受－本人家族区分           TO 作１－本人家族区分.
011585*/名前
011590     MOVE 受－患者カナ               TO 作１－患者カナ.
011600     MOVE 受－患者氏名               TO 作１－患者氏名.
011620     MOVE 受－被保険者カナ           TO 作１－被保険者カナ.
011620     MOVE 受－被保険者氏名           TO 作１－被保険者氏名.
           MOVE 受－患者生年月日           TO 計算和暦年月日Ｗ.
015440     PERFORM 西暦年月日取得.
015450     MOVE 西暦年月日ＷＲ             TO 作１－生年月日.
      *
           IF レセ－請求区分 = 2
              MOVE ZERO           TO 作１－返戻区分
           ELSE
              MOVE 1              TO 作１－返戻区分
           END-IF.
           MOVE レセ－レセ実日数         TO 作１－実日数.
           MOVE レセ－負担割合           TO 作１－負担割合.
006820     PERFORM 医コードセット.
026050*================================================================*
026060 西暦年月日取得 SECTION.
026070*
026080     MOVE ZERO  TO 計算西暦年月日Ｗ.
026090*
026100     IF 計算和暦Ｗ  NOT = ZERO
026110         MOVE 計算和暦Ｗ    TO 元－元号区分
026120         READ 元号マスタ
026130         NOT INVALID KEY
026140             MOVE 元－開始西暦年 TO 計算西暦年Ｗ
026150         END-READ
026160**
026170         IF 計算西暦年Ｗ = ZERO
026180              MOVE  NC"元号マスタに開始西暦年を登録して下さい３" TO 連メ－メッセージ
026190              CALL   "MSG001"
026200              CANCEL "MSG001"
026210              PERFORM ファイル閉鎖
026220              MOVE 99 TO PROGRAM-STATUS
026230              EXIT PROGRAM
026240         ELSE
026250              COMPUTE 計算西暦年Ｗ = 計算西暦年Ｗ + 計算年Ｗ - 1
026260              MOVE 計算月Ｗ TO 計算西暦月Ｗ
026270              MOVE 計算日Ｗ TO 計算西暦日Ｗ
026280         END-IF
026290     END-IF.
           MOVE 計算西暦年Ｗ      TO 西暦年ＷＲ.
           MOVE 計算西暦月Ｗ      TO 西暦月ＷＲ.
           MOVE 計算西暦日Ｗ      TO 西暦日ＷＲ.
026300*
007870*================================================================*
007880 医コードセット SECTION.
007890*
007900**///  順序の変更はこのSECTIONで   ////**
007920*
007930***************************************************
007940* 保険種別順序
007990*     1:社保    (2) 61:前１ 62:前２ 63:前３            *
008010*     2:組合    (3) 71:前１ 72:前２ 73:前３            *
007950*     3:国保    (1) 81:前１ 82:前２ 83:前３            *
007960*     4:退職    (8)                                    *
007960*     5:日雇    (6)                                    *
007960*     7:後期高齢(5)１割                                *
007960*     9:後期高齢(5)３割                                *
008030*    11:共済    (4) 自衛官(9) 41:前１ 42:前２ 43:前３  *
008040*    12:船員    (7)                                    *
008050*    13:就学前  (共通)２割                             *
008060*                                                      *
008070*    23:４１老  (51)                                   *
008070*    30:障害    (53) 負担なし                          *
008070*    31:障害    (53) 負担あり                          *
008090*    40:乳幼児  (55)                                   *
008070*    50:母子    (52) 負担なし                          *
008070*    51:母子    (52) 負担あり                          *
008090*    60:原爆    (54)                                   *
008090*    70:その他  (60)                                   *
008100*
008110***************************************************
008120*
008130      EVALUATE レセ－レセ種別
008140      WHEN 1
008150         EVALUATE  受－保険種別
008160* 国保
008170         WHEN  01
009791            EVALUATE 受－特別区分
009792            WHEN 1
009793               MOVE 81   TO 作１－医コード
009794            WHEN 2
009793               MOVE 82   TO 作１－医コード
009795            WHEN 3
009796               MOVE 83   TO 作１－医コード
009797            WHEN 6
009798               MOVE 13   TO 作１－医コード
009799            WHEN OTHER
009801               MOVE 3    TO 作１－医コード
009803            END-EVALUATE
008230* 社保
008260         WHEN  02
009791            EVALUATE 受－特別区分
009792            WHEN 1
009793               MOVE 61   TO 作１－医コード
009794            WHEN 2
009793               MOVE 62   TO 作１－医コード
009795            WHEN 3
009796               MOVE 63   TO 作１－医コード
009797            WHEN 6
009798               MOVE 13   TO 作１－医コード
009799            WHEN OTHER
009801               MOVE 1    TO 作１－医コード
009803            END-EVALUATE
008340* 組合
008350         WHEN  03
009791            EVALUATE 受－特別区分
009792            WHEN 1
009793               MOVE 71   TO 作１－医コード
009794            WHEN 2
009793               MOVE 72   TO 作１－医コード
009795            WHEN 3
009796               MOVE 73   TO 作１－医コード
009797            WHEN 6
009798               MOVE 13   TO 作１－医コード
009799            WHEN OTHER
009801               MOVE 2    TO 作１－医コード
009803            END-EVALUATE
008410* 共済・自衛官
008420         WHEN  04
008430         WHEN  09
009791            EVALUATE 受－特別区分
009792            WHEN 1
009793               MOVE 41   TO 作１－医コード
009794            WHEN 2
009793               MOVE 42   TO 作１－医コード
009795            WHEN 3
009796               MOVE 43   TO 作１－医コード
009797            WHEN 6
009798               MOVE 13   TO 作１－医コード
009799            WHEN OTHER
009801               MOVE 11   TO 作１－医コード
009803            END-EVALUATE
008240* 日雇
008270         WHEN  06
009801            MOVE 5       TO 作１－医コード
008250* 船員
008280         WHEN  07
009801            MOVE 12      TO 作１－医コード
008490* 退職国保
008500         WHEN  08
009801            MOVE 4       TO 作１－医コード
008560         END-EVALUATE
008570*
008580      WHEN 2
009791         EVALUATE 受－特別区分
009792         WHEN 1
008590            MOVE 7       TO  作１－医コード
009792         WHEN 3
008590            MOVE 9       TO  作１－医コード
009792         WHEN OTHER
008590            MOVE 99      TO  作１－医コード
008560         END-EVALUATE
008600      WHEN 3
009791         EVALUATE 受－助成種別
009792         WHEN 51
008610            MOVE 23      TO  作１－医コード
009792         WHEN 52
                  IF レセ－受給者負担額 = ZERO
008610               MOVE 50   TO  作１－医コード
                  ELSE
008610               MOVE 51   TO  作１－医コード
                  END-IF
009792         WHEN 53
                  IF レセ－受給者負担額 = ZERO
008610               MOVE 30   TO  作１－医コード
                  ELSE
008610               MOVE 31   TO  作１－医コード
                  END-IF
009792         WHEN 54
008610            MOVE  60     TO  作１－医コード
009792         WHEN 55
008610            MOVE  40     TO  作１－医コード
009792         WHEN 60
008610            MOVE  70     TO  作１－医コード
009792         WHEN OTHER
008610            MOVE  99     TO  作１－医コード
               END-EVALUATE
008620      WHEN 7
008630         MOVE  99  TO  作１－医コード
008640     END-EVALUATE.
008650*
016150*================================================================*
016160 データチェック SECTION.
016170*
016180     MOVE SPACE  TO 実行キーＷ.
005300     IF ( レセ－レセ印刷対象区分 NOT = 1 )
005310        MOVE "YES"  TO 実行キーＷ
005320     END-IF.
019520* *****************************************************************
019530* * レセプトＦの請求対象区分 = 0 の場合データ作成対象としない *
019540* *****************************************************************
005360     IF 実行キーＷ  = "YES"
005370*      (再度、実行キーＷ SPACE)
005380        MOVE SPACE  TO 実行キーＷ
019640        IF レセ－請求対象区分 NOT = ZERO
004090           MOVE レセ－施術和暦  TO 受－施術和暦
004100           MOVE レセ－施術年    TO 受－施術年
004110           MOVE レセ－施術月    TO 受－施術月
004120           MOVE レセ－患者番号  TO 受－患者番号
004130           MOVE レセ－枝番      TO 受－枝番
                 READ 受診者情報Ｆ
                 NOT INVALID KEY
019880              MOVE "YES"  TO 実行キーＷ
                 END-READ
019900        ELSE
019910           MOVE SPACE  TO 実行キーＷ
              END-IF
019950     END-IF.
016640*
026550*================================================================*
026560 作業ファイル作成 SECTION.
026570*
026580     OPEN OUTPUT 作業ファイル１.
026590         MOVE NC"作１" TO ファイル名.
026600         PERFORM オープンチェック.
026710*
005420     MOVE 請求和暦ＷＲ  TO レセ－請求和暦.
005430     MOVE 請求年ＷＲ    TO レセ－請求年.
005440     MOVE 請求月ＷＲ    TO レセ－請求月.
005450     MOVE 1             TO レセ－施術和暦.
005460     MOVE ZERO          TO レセ－施術年.
005470     MOVE ZERO          TO レセ－施術月.
005480     MOVE ZERO          TO レセ－患者番号.
005490     MOVE LOW-VALUE     TO レセ－枝番.
005500     MOVE ZERO          TO レセ－レセ種別.
005510     START レセプトＦ   KEY IS >= レセ－請求和暦年月
005520                                  レセ－施術和暦年月
005530                                  レセ－患者コード
005540                                  レセ－レセ種別
026830     END-START.
026840     IF 状態キー = "00"
026850         MOVE SPACE  TO 終了フラグ
026860         PERFORM レセプトＦ読込
026861
026874         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
005600                       ( レセ－請求和暦 NOT = 請求和暦ＷＲ ) OR
005610                       ( レセ－請求年   NOT = 請求年ＷＲ   ) OR
005620                       ( レセ－請求月   NOT = 請求月ＷＲ   )
026910*
026920            PERFORM データチェック
      */            労災・自賠・生保は含めない
                  IF レセ－レセ種別 = 4 OR 5 OR 6 OR 7
                     MOVE SPACE TO 実行キーＷ
                  END-IF
026930            IF 実行キーＷ = "YES"
026940                MOVE SPACE TO 作１－レコード
026950                INITIALIZE    作１－レコード
                      EVALUATE TRUE
004273                WHEN レセ－レセ種別 = 1
005580                    MOVE 受－保険者番号     TO 作１－保険者番号
005580                    MOVE 受－保険者番号     TO 保険者番号Ｗ
005596                    PERFORM 保険者番号右詰め
005600                    MOVE 保険者番号数字Ｗ   TO 作１－保険者番号順
028390                    PERFORM 健保レコードセット
      *
004525                WHEN レセ－レセ種別 = 2
005780                    MOVE 受－費用負担者番号  TO 作１－保険者番号
005780                    MOVE 受－費用負担者番号  TO 保険者番号Ｗ
005790                    PERFORM 保険者番号右詰め
005800                    MOVE 保険者番号数字Ｗ    TO 作１－保険者番号順
028420                    PERFORM 健保レコードセット
027510*
004525                WHEN レセ－レセ種別 = 3
005960                    MOVE 受－保険者番号          TO 作１－保険者番号親
005990                    MOVE 受－費用負担者番号助成  TO  作１－保険者番号 作１－保険者番号順
028460                    PERFORM 助成レコードセット
027810                END-EVALUATE
027820*
028350                PERFORM 共通レコードセット
027767                PERFORM 作業ファイル書込
027840            END-IF
027850            PERFORM レセプトＦ読込
027860         END-PERFORM
027861     ELSE
027862         MOVE  NC"　　データが０件です。確認して下さい" TO 連メ－メッセージ
027863         CALL   "MSG001"
027864         CANCEL "MSG001"
027865         PERFORM ファイル閉鎖
027866         MOVE 99 TO PROGRAM-STATUS
027867         EXIT PROGRAM
027870     END-IF.
027872
027880     CLOSE 作業ファイル１.
007170*================================================================*
007180 保険者番号右詰め SECTION.
007190*
007200     MOVE 保険者番号Ｗ    TO  保険者番号左詰めＷ.
007210     MOVE ZERO            TO  保険者番号右詰めＷ.
007220     MOVE ZERO            TO  保険者番号数字Ｗ.
007230*
007240     MOVE  9  TO  カウンタ.
007250*
007260     IF  保険者番号左詰めＷ１(8) NOT = SPACE
007270         COMPUTE カウンタ = カウンタ  -  1
007280         MOVE 保険者番号左詰めＷ１(8)  TO  保険者番号右詰めＷ１(カウンタ)
007290     END-IF.
007300     IF  保険者番号左詰めＷ１(7) NOT = SPACE
007310         COMPUTE カウンタ = カウンタ  -  1
007320         MOVE 保険者番号左詰めＷ１(7)  TO  保険者番号右詰めＷ１(カウンタ)
007330     END-IF.
007340     IF  保険者番号左詰めＷ１(6) NOT = SPACE
007350         COMPUTE カウンタ = カウンタ  -  1
007360         MOVE 保険者番号左詰めＷ１(6)  TO  保険者番号右詰めＷ１(カウンタ)
007370     END-IF.
007380     IF  保険者番号左詰めＷ１(5) NOT = SPACE
007390         COMPUTE カウンタ = カウンタ  -  1
007400         MOVE 保険者番号左詰めＷ１(5)  TO  保険者番号右詰めＷ１(カウンタ)
007410     END-IF.
007420     IF  保険者番号左詰めＷ１(4) NOT = SPACE
007430         COMPUTE カウンタ = カウンタ  -  1
007440         MOVE 保険者番号左詰めＷ１(4)  TO  保険者番号右詰めＷ１(カウンタ)
007450     END-IF.
007460     IF  保険者番号左詰めＷ１(3) NOT = SPACE
007470         COMPUTE カウンタ = カウンタ  -  1
007480         MOVE 保険者番号左詰めＷ１(3)  TO  保険者番号右詰めＷ１(カウンタ)
007490     END-IF.
007500     IF  保険者番号左詰めＷ１(2) NOT = SPACE
007510         COMPUTE カウンタ = カウンタ  -  1
007520         MOVE 保険者番号左詰めＷ１(2)  TO  保険者番号右詰めＷ１(カウンタ)
007530     END-IF.
007540     IF  保険者番号左詰めＷ１(1) NOT = SPACE
007550         COMPUTE カウンタ = カウンタ  -  1
007560         MOVE 保険者番号左詰めＷ１(1)  TO  保険者番号右詰めＷ１(カウンタ)
007570     END-IF.
007580*
007590     MOVE 保険者番号右詰めＷ TO 保険者番号数字Ｗ.
007600*
027890*================================================================*
027900 作業ファイル書込 SECTION.
027910*
027920     WRITE 作１－レコード
027930     IF 状態キー  NOT =  "00"
027940         MOVE NC"作１"  TO ファイル名
027950         PERFORM エラー表示
027960     END-IF.
027970*================================================================*
027980 クライアントファイル作成 SECTION.
027990*
028000     OPEN INPUT 作業ファイル１.
028010         MOVE NC"作１" TO ファイル名.
028020         PERFORM オープンチェック.
004260     OPEN OUTPUT 会提出ファイル.
004270         MOVE NC"会提出ファイル" TO ファイル名.
004280         PERFORM オープンチェック.
028070*
028080*/並び順を、保険種別、提出保険者番号、患者コード、施術年月順にする。
028100     MOVE ZERO  TO 作１－保険者番号順
028090     MOVE ZERO  TO 作１－返戻区分
028120     MOVE ZERO  TO 作１－施術和暦年月
028120     MOVE ZERO  TO 作１－医コード
028120     MOVE ZERO  TO 作１－本人家族区分
028110     MOVE SPACE TO 作１－患者カナ
028110     MOVE SPACE TO 作１－患者コード
028130     START 作業ファイル１ KEY IS >= 作１－保険者番号順
                                          作１－返戻区分    
028140                                    作１－本人家族区分
                                          作１－医コード
                                          作１－施術和暦年月
                                          作１－患者カナ
                                          作１－患者コード
028170     END-START.
028180     IF 状態キー = "00"
028190         MOVE SPACE  TO 終了フラグ
028200         PERFORM 作業ファイル１読込
002460         IF  終了フラグ = "YES"
002470             MOVE  NC"　データが０件です。確認して下さい。" TO 連メ－メッセージ
002480             CALL   "MSG001"
002490             CANCEL "MSG001"
002500             PERFORM ファイル閉鎖
002510             MOVE 99 TO PROGRAM-STATUS
002520             EXIT PROGRAM
002530         END-IF
028214         PERFORM UNTIL ( 終了フラグ = "YES" )
028220*
028052            MOVE 370   TO 文字カウンタ
011430            MOVE SPACE TO 会提－レコード
028320            PERFORM 提出レコードセット
                  MOVE レコードデータＷ TO 会提－レコード
                  COMPUTE 文字カウンタ = 桁位置Ｗ - 1
                  PERFORM 会提出ファイル書込
028340*
028530            PERFORM 作業ファイル１読込
028550         END-PERFORM
           ELSE
002470         MOVE  NC"　データが０件です。確認して下さい。" TO 連メ－メッセージ
002480         CALL   "MSG001"
002490         CANCEL "MSG001"
002500         PERFORM ファイル閉鎖
002510         MOVE 99 TO PROGRAM-STATUS
002520         EXIT PROGRAM
028571     END-IF.
028573*
028580     CLOSE 作業ファイル１ 会提出ファイル.
028590*================================================================*
028600 提出レコードセット SECTION.
028610*
           MOVE SPACE               TO レコードデータＷ.
           MOVE 1                   TO 桁位置Ｗ.
011450*/会員番号
005420     MOVE 会員番号Ｗ          TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ    DELIMITED BY SPACE
                  ","               DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
011450*/保険者番号
           MOVE 作１－保険者番号   TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
011870*/保険証記号番号
           IF 作１－記号 = SPACE
005440         STRING " ,"                         DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
           ELSE
               MOVE 作１－記号     TO 英数字項目Ｗ
005430         PERFORM 変換処理英数字
005440         STRING 英数字項目２Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
011900     END-IF.
           IF 作１－番号 = SPACE
005440         STRING " ,"                         DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
           ELSE
               MOVE 作１－番号     TO 英数字項目Ｗ
005430         PERFORM 変換処理英数字
005440         STRING 英数字項目２Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
011900     END-IF.
           MOVE 作１－被保険者カナ TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
           MOVE 作１－患者カナ     TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
005440     STRING 作１－本人家族区分           DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
           MOVE 作１－医コード     TO 数字Ｗ.
005430     PERFORM 数字左詰め.
           MOVE 数字左詰めＷ       TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
           MOVE 作１－合計金額     TO 数字Ｗ.
005430     PERFORM 数字左詰め.
           MOVE 数字左詰めＷ       TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
           MOVE 作１－一部負担金   TO 数字Ｗ.
005430     PERFORM 数字左詰め.
           MOVE 数字左詰めＷ       TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
           MOVE 作１－請求金額     TO 数字Ｗ.
005430     PERFORM 数字左詰め.
           MOVE 数字左詰めＷ       TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
005440     STRING 作１－施術年月               DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
005440     STRING 作１－提出年月               DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
011450*/通番
           MOVE 会員コードＷ   TO 会員ＩＤＷＰ.
           MOVE 連入－提出年月 TO 年月ＷＰ.
           COMPUTE 通番ＷＰ = 通番ＷＰ + 1.
005440     STRING 通番Ｗ                           DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
           MOVE 作１－被保険者氏名 TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
           MOVE 作１－患者氏名 TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
005440     STRING 作１－生年月日               DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
           MOVE 作１－負担割合 TO 数字Ｗ.
005430     PERFORM 数字左詰め.
           MOVE 数字左詰めＷ       TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
           MOVE 作１－保険者番号親   TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
           MOVE 作１－実日数   TO 数字Ｗ.
005430     PERFORM 数字左詰め.
           MOVE 数字左詰めＷ       TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
      *
008830*================================================================*
008840 変換処理英数字 SECTION.
008850*
008870     MOVE SPACE TO 終了フラグ４.
008890     MOVE 英数字項目Ｗ TO 英数字項目２Ｗ
008900     PERFORM VARYING 文字ＣＮＴ FROM 70 BY -1
008910             UNTIL (文字ＣＮＴ  <= ZERO) OR
008920                   (終了フラグ４ = "YES")
008930         IF 英数字項目２Ｗ(文字ＣＮＴ:1) NOT = SPACE
008940            COMPUTE 文字ＣＮＴ = 文字ＣＮＴ + 1
008960            MOVE "YES" TO 終了フラグ４
008970         END-IF
008980     END-PERFORM.
007170*================================================================*
007180 数字左詰め SECTION.
007190*
007200     MOVE 数字Ｗ    TO  数字右詰めＷ.
007210     MOVE SPACE     TO  数字左詰めＷ.
007230*
007240     MOVE  ZERO  TO  カウンタ２.
007250*
020350     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 6
020360          IF  数字右詰めＷ１(カウンタ) NOT = SPACE
020370              COMPUTE カウンタ２ = カウンタ２  +  1
020380              MOVE 数字右詰めＷ１(カウンタ)  TO  数字左詰めＷ１(カウンタ２)
020390          END-IF
020400     END-PERFORM.
007600*
028590*================================================================*
028600 作業ファイル１読込 SECTION.
028610*
028620     READ 作業ファイル１ NEXT
028630     AT END
028640         MOVE "YES" TO 終了フラグ
028650     END-READ.
028695*
028696*================================================================*
028697 会提出ファイル書込 SECTION.
028698*
028699     WRITE 会提－レコード
028700     END-WRITE.
028701     
028702     IF 状態キー  NOT =  "00"
028703         MOVE NC"会提出ファイル" TO ファイル名
028704         PERFORM エラー表示
028705     END-IF.
028706*================================================================*
028707******************************************************************
028708 END PROGRAM NJY101.
028709******************************************************************
