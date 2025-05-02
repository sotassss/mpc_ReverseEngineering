000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAW9228.
000060 AUTHOR.                 池田  幸子
000070*
000080*----------------------------------------------------------------*
      *         受診者リスト 【印刷】
000100*       MED = YAW922 YAW9228P 
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2015-10-30
000130 DATE-COMPILED.          2015-10-30
000140*----------------------------------------------------------------*
000150******************************************************************
000160*            ENVIRONMENT         DIVISION                        *
000170******************************************************************
000180 ENVIRONMENT             DIVISION.
000190 CONFIGURATION           SECTION.
000200 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000210 OBJECT-COMPUTER.        FMV-DESKPOWER.
000220 SPECIAL-NAMES.          CONSOLE  IS  CONS
000230                         SYSERR   IS  MSGBOX.
000240 INPUT-OUTPUT            SECTION.
000250 FILE-CONTROL.
000610     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000620                             ORGANIZATION             IS  INDEXED
000630                             ACCESS MODE              IS  DYNAMIC
000640                             RECORD KEY               IS  元－元号区分
000650                             FILE STATUS              IS  状態キー
000660                             LOCK        MODE         IS  AUTOMATIC.
000670     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000680                             ORGANIZATION             IS  INDEXED
000690                             ACCESS MODE              IS  DYNAMIC
000700                             RECORD KEY               IS  制－制御区分
000710                             FILE STATUS              IS  状態キー
000720                             LOCK        MODE         IS  AUTOMATIC.
000410     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000420                             ORGANIZATION             IS  INDEXED
000430                             ACCESS MODE              IS  DYNAMIC
000440                             RECORD KEY               IS  名－区分コード
000450                                                          名－名称コード
000460                             FILE STATUS              IS  状態キー
000470                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  作業ファイル１  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W9227L.DAT"
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS                   IS  DYNAMIC
000560                             RECORD      KEY          IS  作１－患者コード
                                                                作１－患者カナ
                                   ALTERNATE   RECORD  KEY  IS  作１－患者カナ
                                                                作１－患者コード
000620                             FILE        STATUS       IS  状態キー
000630                             LOCK        MODE         IS  AUTOMATIC.
000780     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF001
000790                             SYMBOLIC    DESTINATION  IS "PRT"
000800                             FORMAT                   IS  定義体名Ｐ
000810                             GROUP                    IS  項目群名Ｐ
000820                             PROCESSING  MODE         IS  処理種別Ｐ
000830                             UNIT        CONTROL      IS  拡張制御Ｐ
000840                             FILE        STATUS       IS  通知情報Ｐ.
000850******************************************************************
000860*                      DATA DIVISION                             *
000870******************************************************************
000880 DATA                    DIVISION.
000890 FILE                    SECTION.
000950*                           ［ＲＬ＝  １２８］
000960 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
000970     COPY GENGOU         OF  XFDLIB  JOINING    元   AS  PREFIX.
000980*                           ［ＲＬ＝  ２５６］
000990 FD  制御情報マスタ      BLOCK   CONTAINS   1   RECORDS.
001000     COPY SEIGYO         OF  XFDLIB  JOINING    制   AS  PREFIX.
000840*                           ［ＲＬ＝  １２８］
000850 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
000860     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
000700*                           ［ＲＬ＝  ３２０］
000710 FD  作業ファイル１ RECORD  CONTAINS 320 CHARACTERS.
000720 01 作１－レコード.
000730    03 作１－レコードキー.
000740       05 作１－患者コード.
000750          07 作１－患者番号                     PIC 9(6).
000760          07 作１－枝番                         PIC X.
000770       05 作１－患者カナ                        PIC X(50).
000780    03 作１－レコードデータ.
000790       05 作１－患者氏名                        PIC X(50).
000800       05 作１－被保険者氏名                    PIC X(50).
000810       05 作１－本人家族区分                    PIC 9.
000820       05 作１－続柄                            PIC 9(2).
000830       05 作１－保険種別                        PIC 9(2).
000840       05 作１－保険者番号.
000850          07 作１－法別番号                     PIC X(2).
000860          07 作１－保番.
000870             09 作１－都道府県番号              PIC X(2).
000880             09 作１－保険番号                  PIC X(3).
000890             09 作１－検証番号                  PIC X.
000900             09 FILLER                          PIC X(2).
000910       05 作１－公費種別                        PIC 9(2).
000920       05 作１－費用負担者番号.
000930          07 作１－法別番号老人                 PIC X(2).
000940          07 作１－助保険老人.
000950             09 作１－府県老人                  PIC X(2).
000960             09 作１－助番老人                  PIC X(3).
000970             09 作１－助ＣＤ老人                PIC X.
000980          07 FILLER                             PIC X(2).
000990       05 作１－助成種別                        PIC 9(2).
001000       05 作１－費用負担者番号助成.
001010          07 作１－法別番号助成                 PIC X(2).
001020          07 作１－助保険助成.
001030             09 作１－府県助成                  PIC X(2).
001040             09 作１－助番助成                  PIC X(3).
001050             09 作１－助ＣＤ助成                PIC X.
001060          07 FILLER                             PIC X(2).
             05 作１－患者郵便番号.
                07 作１－患者郵便番号１               PIC X(3).
                07 作１－患者郵便番号２               PIC X(4).
             05 作１－患者住所                        PIC X(50).
001090       05 作１－患者電話番号                    PIC X(15).
001100       05 作１－患者生年月日.
001110          07 作１－患者和暦                     PIC 9.
001120          07 作１－患者年                       PIC 9(2).
001130          07 作１－患者月                       PIC 9(2).
001140          07 作１－患者日                       PIC 9(2).
001150       05 作１－最新キー情報.
001160          07 作１－施術和暦年月Ｎ.
001170             09 作１－施術和暦Ｎ                PIC 9.
001180             09 作１－施術年月Ｎ.
001190                11 作１－施術年Ｎ               PIC 9(2).
001200                11 作１－施術月Ｎ               PIC 9(2).
001210       05 FILLER                                PIC X(40).
001040 FD  印刷ファイル.
001050     COPY YAW9228P        OF  XMDLIB.
001060******************************************************************
001070*                WORKING-STORAGE SECTION                         *
001080******************************************************************
001090 WORKING-STORAGE         SECTION.
       01 行カウンタ                         PIC 9(2) VALUE ZERO.
       01 頁カウンタ                         PIC 9(4) VALUE ZERO.
       01 最大行数                           PIC 9(2) VALUE ZERO.
001100 01 キー入力                           PIC X    VALUE SPACE.
001110 01 状態キー                           PIC X(2) VALUE SPACE.
001120 01 終了フラグ                         PIC X(3) VALUE SPACE.
002120 01 終了フラグ２                       PIC X(3) VALUE SPACE.
001140 01 終了フラグ３                       PIC X(3) VALUE SPACE.
001150 01 確認入力Ｗ                         PIC X(1) VALUE SPACE.
001160 01 ファイル名Ｗ                       PIC N(2) VALUE SPACE.
001170 01 カレント元号Ｗ                     PIC 9(1) VALUE ZERO.
001180 01 実行キーＷ                         PIC X(4) VALUE SPACE.
001190 01 前和暦Ｗ                           PIC 9 VALUE ZERO.
001200 01 行桁フラグ                         PIC X(3) VALUE SPACE.
001220 01 処理移動キー                       PIC X(4) VALUE SPACE.
002940 01 患者番号Ｗ                         PIC 9(6) VALUE ZERO.
001210 01 オープンフラグ                     PIC X(3)   VALUE SPACE.
001560*
001570 01 略称Ｗ.
001580   03 略称Ｗ１                         PIC N(12) VALUE SPACE.
001590   03 FILLER                           PIC N(2) VALUE SPACE.
001240 01 施術和暦年月ＷＲ.
001250    03 施術和暦ＷＲ                    PIC 9    VALUE ZERO.
001260    03 施術年月ＷＲ.
001270       05 施術年ＷＲ                   PIC 9(2) VALUE ZERO.
001280       05 施術月ＷＲ                   PIC 9(2) VALUE ZERO.
001260    03 施術年月Ｗ.
001270       05 施術年Ｗ                     PIC X(2) VALUE ZERO.
001280       05 施術月Ｗ                     PIC X(2) VALUE ZERO.
001240 01 請求和暦年月日Ｗ.
001250    03 請求和暦Ｗ                      PIC 9    VALUE ZERO.
001260    03 請求年月Ｗ.
002960       05 請求年Ｗ                     PIC 9(2) VALUE ZERO.
002970       05 請求月Ｗ                     PIC 9(2) VALUE ZERO.
002970    03 請求日Ｗ                        PIC 9(2) VALUE ZERO.
001240 01 生年月日Ｗ.
001250    03 生年和暦Ｗ                      PIC X(1) VALUE SPACE.
001260    03 生年年月Ｗ.
002960       05 生年Ｗ                       PIC 9(2) VALUE ZERO.
             05 年Ｗ                         PIC X(1) VALUE SPACE.
002970       05 生月Ｗ                       PIC 9(2) VALUE ZERO.
             05 月Ｗ                         PIC X(1) VALUE SPACE.
002970    03 生日Ｗ                          PIC 9(2) VALUE ZERO.
001320 01 元号名称Ｗ.
001320    03 和暦名称Ｗ                      PIC X(4) VALUE SPACE.
001230 01 保険種別略称Ｗ.
          03 保険種別略称ＷＰ                PIC X(4) VALUE SPACE.
001570*
001580 01 抽出条件コメントＷ                 PIC X(140) VALUE SPACE.
001580 01 抽出条件コメントＷ２               PIC X(140) VALUE SPACE.
001580 01 抽出条件コメントＷ３               PIC X(140) VALUE SPACE.
002433* エラーメッセージ用
002434 01 エラーメッセージＷ.
002435    03 エラー患者コードＷ              PIC X(7)  VALUE SPACE.
002436    03 エラー区切りＷ                  PIC X(1)  VALUE SPACE.
002437    03 エラー保険種別Ｗ                PIC X(2)  VALUE SPACE.
002438    03 FILLER                          PIC X(10) VALUE SPACE.
001530*
001540 01 印刷制御.
001550     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
001560     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
001570     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
001580     03 拡張制御Ｐ.
001590         05 端末制御Ｐ.
001600             07 移動方向Ｐ             PIC X(1).
001610             07 移動行数Ｐ             PIC 9(3).
001620         05 詳細制御Ｐ                 PIC X(2).
001630     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
001640     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
001650 01 計算機西暦年Ｗ                     PIC 9(2).
001660* 日付ＷＯＲＫ
001670 01 和暦終了年Ｗ                       PIC 9(4).
001680 01 計算機和暦年Ｗ                     PIC 9(2).
001690 01 計算機西暦.
001700    03 計算機西暦年                    PIC 9(4).
001710    03 計算機西暦月日                  PIC 9(4).
001720 01 計算機西暦Ｒ REDEFINES 計算機西暦.
001730    03 計算機世紀                      PIC 9(2).
001740    03 計算機日付                      PIC 9(6).
001750    03 計算機日付Ｒ REDEFINES 計算機日付.
001760       05 計算機年月                   PIC 9(4).
001770       05 計算機年月Ｒ REDEFINES 計算機年月.
001780         07 計算機年                   PIC 9(2).
001790         07 計算機月                   PIC 9(2).
001800       05 計算機日                     PIC 9(2).
001810*
003340******************************************************************
003350*                          連結項目                              *
003360******************************************************************
003370*
      ****************************
      * 元号日付チェック用ワーク *
      *****************************
       01 連元ＣＨＫ－キー IS EXTERNAL.
      */ in
          03 連元ＣＨＫ－和暦年月日.
             05 連元ＣＨＫ－和暦年月.
               07 連元ＣＨＫ－和暦           PIC 9.
               07 連元ＣＨＫ－年             PIC 9(2).
               07 連元ＣＨＫ－月             PIC 9(2).
             05 連元ＣＨＫ－日               PIC 9(2).
      *   / 確認メッセージを使用する場合："YES" /
          03 連元ＣＨＫ－確認フラグ          PIC X(3).
      */ out
      *   / 年月単位の年月日を作成 /
          03 連元ＣＨＫ－正式和暦年月日.
             05 連元ＣＨＫ－正式和暦年月.
               07 連元ＣＨＫ－正式和暦       PIC 9.
               07 連元ＣＨＫ－正式年         PIC 9(2).
               07 連元ＣＨＫ－正式月         PIC 9(2).
            05 連元ＣＨＫ－正式日            PIC 9(2).
      *   / 存在しない場合エラー："YES" /
         03 連元ＣＨＫ－エラーフラグ         PIC X(3).
      *
      *   / 正式日付の年月日を作成（元号が変更されていない場合は上記年月日と同じ） /
         03 連元ＣＨＫ－正式和暦年月日２.
            05 連元ＣＨＫ－正式和暦年月２.
              07 連元ＣＨＫ－正式和暦２     PIC 9.
              07 連元ＣＨＫ－正式年２       PIC 9(2).
              07 連元ＣＨＫ－正式月２       PIC 9(2).
            05 連元ＣＨＫ－正式日２          PIC 9(2).
      *   / 存在しない場合エラー："YES" /
         03 連元ＣＨＫ－エラーフラグ２       PIC X(3).
      *
      **********************
      * メッセージ表示キー *
      **********************
      *
       01 連メ－キー IS EXTERNAL.
          03  連メ－メッセージ                 PIC N(20).
      *
002634 01 連メ３－キー IS EXTERNAL.
002635    03  連メ３－メッセージ             PIC N(20).
002636    03  連メ３－メッセージ１           PIC X(20).
      *
002900****************
002910* 画面入力情報 *
002920****************
      *
       01 連入－印刷区分９２２７ IS EXTERNAL.
      */並び順 0:番号順 1:カナ順
          03 連入－並び順          PIC 9(1).
      */昇降順 0:昇順 1:降順
          03 連入－昇降順          PIC 9(1).
      *
001210************************************
001220* プリンタファイル作成用           *
001230************************************
001240 01 Ｈ連ＰＲＴＦ－作成データ IS EXTERNAL.
001250     03 Ｈ連ＰＲＴＦ－ファイル名           PIC X(8).
001260     03 Ｈ連ＰＲＴＦ－プレビュー区分       PIC 9.
000600     03 Ｈ連ＰＲＴＦ－帳票プログラム名     PIC X(8).
001270     03 Ｈ連ＰＲＴＦ－オーバレイ名         PIC X(8).
003750*
003890******************************************************************
003900*                      PROCEDURE  DIVISION                       *
003910******************************************************************
003920 PROCEDURE               DIVISION.
003930************
003940*           *
003950* 初期処理   *
003960*           *
003970************
001800     PERFORM プリンタファイル作成.
           MOVE SPACE TO オープンフラグ.
003980     PERFORM 初期化.
001790************
001800*           *
001810* 主処理     *
001820*           *
001830************
004550     PERFORM 印刷処理.
004620************
004630*           *
004640* 終了処理   *
004650*           *
004660************
004670     PERFORM 終了処理.
004680     EXIT PROGRAM.
004690*
004700*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
001980*================================================================*
001990 プリンタファイル作成 SECTION.
002000*
002010*   / 初期化 /
002020     MOVE SPACE TO Ｈ連ＰＲＴＦ－作成データ.
002030     INITIALIZE Ｈ連ＰＲＴＦ－作成データ.
002040*
002050*
002060*--↓↓ 変更箇所 ↓↓--------------------------------------*
002070*   使用するプリンタファイル名セット
002080     MOVE "PRTF001"             TO Ｈ連ＰＲＴＦ－ファイル名.
000680*
000690*   使用する帳票プログラム名セット
000700     MOVE "YAW9228P"            TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002090*
002100*   / プレビュー区分セット /
002110     MOVE ZERO TO Ｈ連ＰＲＴＦ－プレビュー区分.
002120*****     MOVE Ｈ連入－プレビュー区分  TO Ｈ連ＰＲＴＦ－プレビュー区分.
002130*
002140*--↑↑-----------------------------------------------------*
002150*
002160     CALL   "CRTPRTF".
002170     CANCEL "CRTPRTF".
002180*
004710*================================================================*
004720 初期化 SECTION.
004730*
004770     OPEN INPUT 元号マスタ.
004780             MOVE NC"元号" TO ファイル名Ｗ.
004790             PERFORM オープンチェック.
004800     OPEN INPUT 制御情報マスタ.
004810             MOVE NC"制御" TO ファイル名Ｗ.
004820             PERFORM オープンチェック.
004800     OPEN INPUT 名称マスタ.
004810             MOVE NC"名称" TO ファイル名Ｗ.
004820             PERFORM オープンチェック.
007640     OPEN INPUT 作業ファイル１.
007650             MOVE NC"作１" TO ファイル名Ｗ.
007660             PERFORM オープンチェック.
004900*
004910*    /* 現在日付取得 */
004920     ACCEPT 計算機日付 FROM DATE.
004930*    /* 1980～2079年の間で設定 */
004940     IF 計算機年 > 80
004950         MOVE 19 TO 計算機世紀
004960     ELSE
004970         MOVE 20 TO 計算機世紀
004980     END-IF.
004990*
005000     PERFORM カレント元号取得.
005010     PERFORM 和暦終了年取得.
003610     COMPUTE 計算機和暦年Ｗ = 計算機西暦年 - 和暦終了年Ｗ + 1.
           INITIALIZE 連元ＣＨＫ－キー.
           MOVE カレント元号Ｗ   TO 連元ＣＨＫ－和暦.
           MOVE 計算機和暦年Ｗ   TO 連元ＣＨＫ－年.
           MOVE 計算機月         TO 連元ＣＨＫ－月.
           MOVE ZERO             TO 連元ＣＨＫ－日.
           CALL "GENGOCHK".
           CANCEL "GENGOCHK".
005060*
005080*================================================================*
005090 オープンチェック SECTION.
005100*
005110     IF 状態キー  NOT =  "00"
005120         DISPLAY ファイル名Ｗ NC"Ｆオープンエラー" UPON CONS
005130         DISPLAY NC"状態キー：" 状態キー           UPON CONS
005140         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
005150                                                   UPON CONS
005160         ACCEPT  キー入力 FROM CONS
005170         PERFORM ファイル閉鎖
005180         EXIT PROGRAM.
005190*================================================================*
005200 カレント元号取得 SECTION.
005210*
005220     MOVE ZEROS TO 制－制御区分.
005230     READ 制御情報マスタ
005240     NOT INVALID KEY
005250         MOVE 制－カレント元号 TO カレント元号Ｗ
005260     END-READ.
005270*
005280*================================================================*
005290 和暦終了年取得 SECTION.
005300*
005320     MOVE カレント元号Ｗ TO 元－元号区分.
005330     READ 元号マスタ
005340     INVALID KEY
005350         DISPLAY NC"指定和暦が登録されていません" UPON CONS
005360         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
005370                                                  UPON CONS
005380         ACCEPT  キー入力 FROM CONS
005390         PERFORM 終了処理
005400         EXIT PROGRAM
005410     NOT INVALID KEY
               IF ( 元－正式開始西暦年 = ZERO OR SPACE )
005500             COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
005510             MOVE 前和暦Ｗ TO 元－元号区分
005520             READ 元号マスタ
005530             INVALID KEY
005540                 DISPLAY NC"指定和暦が登録されていません" UPON CONS
005550                 DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
005560                                                          UPON CONS
005570                 ACCEPT  キー入力 FROM CONS
005580                 PERFORM 終了処理
005590                 EXIT PROGRAM
005600             NOT INVALID KEY
                       IF ( 元－終了西暦年 = ZERO OR SPACE )
                           CONTINUE
                       ELSE
                           COMPUTE 和暦終了年Ｗ = 元－終了西暦年 + 1
                       END-IF
                   END-READ
               ELSE
                   MOVE 元－正式開始西暦年 TO 和暦終了年Ｗ
               END-IF
005550     END-READ.
005560*
005570*================================================================*
005940* 印刷オープン SECTION.
005950*
005960*     OPEN I-O   印刷ファイル.
005990*================================================================*
006120* 印刷クローズ SECTION.
006130*
006140*     CLOSE 印刷ファイル.
006150*================================================================*
006770 ファイル閉鎖 SECTION.
006780*
006790     CLOSE  元号マスタ  制御情報マスタ  名称マスタ  作業ファイル１
                  印刷ファイル.
006810*================================================================*
006820 終了処理 SECTION.
006830*
006840     PERFORM ファイル閉鎖.
      *================================================================*
010770 印刷処理 SECTION.
010780*
           MOVE 35    TO 最大行数.
           MOVE 99    TO 行カウンタ.
           MOVE ZERO  TO 頁カウンタ.
007740     MOVE SPACE TO 終了フラグ.
           IF 連入－昇降順 = ZERO
              MOVE ZERO  TO 作１－患者番号
              MOVE SPACE TO 作１－枝番
              MOVE SPACE TO 作１－患者カナ
              EVALUATE 連入－並び順
              WHEN ZERO
                  START 作業ファイル１ KEY IS >=  作１－患者コード
                                                  作１－患者カナ
                  END-START
              WHEN 1
                  START 作業ファイル１ KEY IS >=  作１－患者カナ
                                                  作１－患者コード
                  END-START
              END-EVALUATE
           ELSE
003100        MOVE 999999     TO 作１－患者番号
003110        MOVE HIGH-VALUE TO 作１－枝番
003110        MOVE SPACE      TO 作１－患者カナ
003120        START 作業ファイル１   KEY IS < 作１－患者コード
                                              作１－患者カナ
                                            REVERSED
              END-START
           END-IF.
003160     IF 状態キー = "00"
               PERFORM 作業ファイル１読込
               PERFORM UNTIL 終了フラグ = "YES"
                   IF ( 行カウンタ        >= 最大行数   )
                       COMPUTE 頁カウンタ = 頁カウンタ + 1
                       PERFORM ヘッダ印刷処理
                       MOVE ZERO TO 行カウンタ
                   ELSE
004540                 PERFORM 明細レコードセット
004550                 PERFORM 明細印刷処理
                       PERFORM 作業ファイル１読込
                   END-IF
               END-PERFORM
           END-IF.
      *================================================================*
007700 作業ファイル１読込 SECTION.
007710*
007720     READ 作業ファイル１ NEXT
007730     AT END
007740         MOVE "YES" TO 終了フラグ
007750     END-READ.
      *================================================================*
       ヘッダ印刷処理 SECTION.
      *
004490     PERFORM ヘッダセット.
004500     PERFORM ヘッダ印字.
      *
007760*================================================================*
       ヘッダセット SECTION.
      *
           MOVE SPACE TO YAW9228P.
           INITIALIZE YAW9228P.
      */並び順名称
           EVALUATE 連入－並び順
           WHEN ZERO
               MOVE NC"（番号順）" TO 並び順名称
           WHEN 1
               MOVE NC"（カナ順）" TO 並び順名称
           WHEN OTHER
               MOVE SPACE          TO 並び順名称
           END-EVALUATE.
           MOVE 頁カウンタ         TO ページ.
005300*
005220* 作成和暦を取得
005230     MOVE 連元ＣＨＫ－正式和暦  TO 元－元号区分.
005240     READ 元号マスタ
005250     INVALID KEY
005260         MOVE SPACE         TO 作成和暦名称
005270     NOT INVALID KEY
005280         MOVE 元－元号名称  TO 作成和暦名称
005290     END-READ.
005300*
005310     MOVE 連元ＣＨＫ－正式年 TO 作成年.
005320     MOVE 連元ＣＨＫ－正式月 TO 作成月.
005330     MOVE 計算機日           TO 作成日.
007700*================================================================*
010770 明細レコードセット SECTION.
010780*
           MOVE SPACE TO YAW9228P.
           INITIALIZE YAW9228P.
           MOVE 作１－患者コード         TO 患者コード.
           MOVE 作１－患者氏名           TO 患者氏名.
           MOVE 作１－被保険者氏名       TO 被保険者氏名.
           IF 作１－本人家族区分 = 1
               MOVE NC"本人" TO 続柄
           ELSE
               MOVE NC"家族" TO 続柄
           END-IF.
           MOVE 02                   TO 名－区分コード.
           MOVE 作１－保険種別       TO 名－名称コード.
           READ 名称マスタ
           NOT INVALID KEY
               MOVE 名－略称         TO 保険種別略称
           END-READ.
      *
           MOVE 12                   TO 名－区分コード.
           MOVE 作１－助成種別       TO 名－名称コード.
           READ 名称マスタ
           NOT INVALID KEY
               MOVE 名－略称         TO 助成種別略称
           END-READ.
           MOVE 作１－保険者番号     TO 保険者番号.
           IF ( 作１－施術年Ｎ NOT = ZERO ) OR
              ( 作１－施術月Ｎ NOT = ZERO )
               MOVE 作１－施術年Ｎ   TO 最終年
               MOVE 作１－施術月Ｎ   TO 最終月
               MOVE "."              TO 年月区切
           END-IF.
      *
           MOVE 作１－患者郵便番号１ TO 郵便番号１.
           MOVE 作１－患者郵便番号２ TO 郵便番号２.
           MOVE 作１－患者住所       TO 住所.
           IF ( 郵便番号１ NOT = SPACE ) OR
              ( 郵便番号２ NOT = SPACE ) OR
              ( 住所       NOT = SPACE )
               MOVE NC"〒"           TO 郵便マーク
               MOVE "-"              TO 郵便区切
           END-IF.
           MOVE 作１－患者電話番号   TO 電話番号.
      *
006960     IF (作１－患者和暦 NOT = ZERO) OR
006970        (作１－患者年   NOT = ZERO) OR
006980        (作１－患者月   NOT = ZERO) OR
006990        (作１－患者日   NOT = ZERO)
007010         EVALUATE 作１－患者和暦
007050         WHEN 1
                   MOVE "M"              TO 生年和暦Ｗ
007060         WHEN 2
007070             MOVE "T"              TO 生年和暦Ｗ
007080         WHEN 3
                   MOVE "S"              TO 生年和暦Ｗ
007060         WHEN 4
007070             MOVE "H"              TO 生年和暦Ｗ
               END-EVALUATE
               MOVE 作１－患者年         TO 生年Ｗ
               MOVE "."                  TO 年Ｗ
               MOVE 作１－患者月         TO 生月Ｗ
               MOVE "."                  TO 月Ｗ
               MOVE 作１－患者日         TO 生日Ｗ
           END-IF.
           MOVE 生年月日Ｗ               TO 生年月日.
      *
      *================================================================*
       ヘッダ印字 SECTION.
011790*
003270     IF ( オープンフラグ NOT = "YES" )
003280         MOVE "YES" TO オープンフラグ
003290         OPEN I-O  印刷ファイル
003300         PERFORM エラー処理Ｐ
003310     END-IF.
011800     MOVE "YAW9228P"  TO  定義体名Ｐ.
007800     MOVE SPACE      TO  処理種別Ｐ.
011810     MOVE "HEAD01"   TO  項目群名Ｐ.
011820     WRITE YAW9228P.
           PERFORM エラー処理Ｐ.
011800     MOVE "YAW9228P"  TO  定義体名Ｐ.
007800     MOVE SPACE      TO  処理種別Ｐ.
011810     MOVE "HEAD03"   TO  項目群名Ｐ.
011820     WRITE YAW9228P.
           PERFORM エラー処理Ｐ.
      *================================================================*
       明細印刷処理 SECTION.
011790*
011800     MOVE "YAW9228P"  TO  定義体名Ｐ.
007800     MOVE SPACE      TO  処理種別Ｐ.
011810     MOVE "GRP001"   TO  項目群名Ｐ.
011820     WRITE YAW9228P.
           PERFORM エラー処理Ｐ.
           COMPUTE 行カウンタ = 行カウンタ + 1.
      *================================================================*
       改頁処理  SECTION.
      *
011820     MOVE SPACE TO YAW9228P.
011820     INITIALIZE YAW9228P.
           MOVE "YAW9228P" TO  定義体名Ｐ.
           MOVE "CT"      TO  処理種別Ｐ.
           MOVE "PAGE"    TO  拡張制御Ｐ.
           MOVE SPACE     TO  項目群名Ｐ.
011820     WRITE YAW9228P.
           PERFORM エラー処理Ｐ.
           MOVE SPACE     TO  拡張制御Ｐ.
      *================================================================*
014060 エラー表示 SECTION.
014070*
014080     DISPLAY NC"ファイル書込エラー：" ファイル名Ｗ UPON CONS.
014090     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
014100     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
014110     ACCEPT  キー入力 FROM CONS.
      *================================================================*
       エラー処理Ｐ SECTION.
      *
           IF 通知情報Ｐ NOT = "00"
               DISPLAY NC"帳票エラー"              UPON CONS
               DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
               DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
               DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
               DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
                                                   UPON CONS
               ACCEPT  キー入力 FROM CONS
               PERFORM ファイル閉鎖
               MOVE 99 TO PROGRAM-STATUS
               EXIT PROGRAM
           END-IF.
      *================================================================*
014130 エラー表示Ｒ SECTION.
014140*
014150     DISPLAY NC"ファイル読込エラー" ファイル名Ｗ UPON CONS.
014160     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
014170     ACCEPT  キー入力 FROM CONS.
014180     PERFORM ファイル閉鎖.
014190     EXIT PROGRAM.
014200*================================================================*
014210 エラー表示その他 SECTION.
014220*
014230     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
014240     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
014250                                                   UPON CONS.
014260     ACCEPT  キー入力 FROM CONS.
014270     PERFORM ファイル閉鎖.
014280     EXIT PROGRAM.
001370*================================================================*
014300******************************************************************
014310 END PROGRAM YAW9228.
014320******************************************************************
