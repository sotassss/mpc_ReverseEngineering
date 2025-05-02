000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YGN723.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
      *         カルテ（裏）印刷対象者リスト 【印刷】
000100*       MED = YGN720 YGN723P 
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2016-04-07
000130 DATE-COMPILED.          2016-04-07
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
000730     SELECT  施術所情報マスタ ASSIGN     TO        SEJOHOL
000740                             ORGANIZATION             IS  INDEXED
000750                             ACCESS MODE              IS  DYNAMIC
000760                             RECORD KEY               IS  施情－施術所番号
                                   FILE STATUS              IS  状態キー
000770                             LOCK        MODE         IS  AUTOMATIC.
000410     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000420                             ORGANIZATION             IS  INDEXED
000430                             ACCESS MODE              IS  DYNAMIC
000440                             RECORD KEY               IS  名－区分コード
000450                                                          名－名称コード
000460                             FILE STATUS              IS  状態キー
000470                             LOCK        MODE         IS  AUTOMATIC.
           SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  受－施術和暦年月
                                                                受－患者コード
                                   ALTERNATE RECORD KEY     IS  受－施術和暦年月
                                                                受－患者カナ
                                                                受－患者コード
                                   ALTERNATE RECORD KEY     IS  受－患者コード
                                                                受－施術和暦年月
                                   ALTERNATE RECORD KEY     IS  受－施術和暦年月
                                                                受－保険種別
                                                                受－保険者番号
                                                                受－患者コード
                                   ALTERNATE RECORD KEY     IS  受－施術和暦年月
                                                                受－公費種別
                                                                受－費用負担者番号
                                                                受－患者コード
                                   ALTERNATE RECORD KEY     IS  受－施術和暦年月
                                                                受－助成種別
                                                                受－費用負担者番号助成
                                                                受－患者コード
                                   ALTERNATE RECORD KEY     IS  受－請求和暦年月
                                                                受－施術和暦年月
                                                                受－患者コード
                                   FILE STATUS              IS  状態キー
                                   LOCK        MODE         IS  AUTOMATIC.
000860     SELECT  作業ファイル１  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W7211L.DAT"
000960                             ORGANIZATION             IS  INDEXED
000970                             ACCESS                   IS  DYNAMIC
000980                             RECORD      KEY          IS  作１－施術和暦年月日
001030                                                          作１－患者コード
000910                             ALTERNATE RECORD KEY     IS  作１－施術和暦年月日
                                                                作１－患者カナ
                                                                作１－患者コード
000910                             ALTERNATE RECORD KEY     IS  作１－患者コード
                                                                作１－施術和暦年月日
000910                             ALTERNATE RECORD KEY     IS  作１－患者カナ
                                                                作１－患者コード
                                                                作１－施術和暦年月日
001040                             FILE        STATUS       IS  状態キー
001050                             LOCK        MODE         IS  AUTOMATIC.
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
001010*                           ［ＲＬ＝  ６４０］
001020 FD  施術所情報マスタ    BLOCK   CONTAINS   1   RECORDS.
001030     COPY SEJOHO         OF  XFDLIB  JOINING    施情 AS  PREFIX.
000840*                           ［ＲＬ＝  １２８］
000850 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
000860     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
      *                           ［ＲＬ＝  ３２０］
       FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
           COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
001310**************************
001320* 作業ファイル１／カルテ *
001330**************************
001900*                         ［ＲＬ＝  ２７２］
001910 FD  作業ファイル１ RECORD  CONTAINS 272 CHARACTERS.
001920 01 作１－レコード.
001930    03 作１－レコードキー.
001940       05 作１－施術和暦年月日.
001950          07 作１－施術和暦               PIC 9.
001960          07 作１－施術年月.
001970             09 作１－施術年              PIC 9(2).
001980             09 作１－施術月              PIC 9(2).
001990          07 作１－施術日                 PIC 9(2).
002000       05 作１－患者コード.
002010          07 作１－患者番号                PIC 9(6).
002020          07 作１－枝番                    PIC X(1).
002030    03 作１－レコードデータ.
002100       05 作１－患者カナ                   PIC X(50).
002110       05 作１－患者氏名                   PIC X(50).
002130       05 作１－料金.
001550          07 作１－初検金額                PIC 9(5).
                07 作１－整復                    OCCURS 4.
001550             09 作１－整復金額             PIC 9(5).
001550          07 作１－その他                  PIC 9(5).
001550          07 作１－罨法等                  PIC 9(5).
001550          07 作１－金属副子                PIC 9(5).
001550          07 作１－往療料                  PIC 9(5).
002300          07 作１－費用額                  PIC 9(6).
002270          07 作１－一部負担金              PIC 9(5).
002280          07 作１－コメント                PIC X(100).
002310       05 FILLER                           PIC X(2).
      *
001040 FD  印刷ファイル.
001050     COPY YGN723P        OF  XMDLIB.
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
001580 01 オープンフラグ                     PIC X(3) VALUE SPACE.
002940 01 患者番号Ｗ                         PIC 9(6) VALUE ZERO.
001460 01 患者コードＷ.
001470    03 患者番号ＷＰ                    PIC 9(6) VALUE ZERO.
001480    03 枝番ＷＰ                        PIC X(1) VALUE SPACE.
002940 01 部位コードＷ.
          03 負傷種別Ｗ                      PIC 9(2) VALUE ZERO.
          03 部位Ｗ                          PIC 9(2) VALUE ZERO.
       01 転帰区分Ｗ                         PIC 9(1) VALUE ZERO.
       01 入力データＷ.
002940    03 開始部位コードＷ.
             05 開始負傷種別Ｗ               PIC 9(2) VALUE ZERO.
             05 開始部位Ｗ                   PIC 9(2) VALUE ZERO.
          03 開始転帰区分Ｗ                  PIC 9    VALUE ZERO.
002940    03 終了部位コードＷ.
             05 終了負傷種別Ｗ               PIC 9(2) VALUE ZERO.
             05 終了部位Ｗ                   PIC 9(2) VALUE ZERO.
          03 終了転帰区分Ｗ                  PIC 9    VALUE ZERO.
001560*
001570 01 略称Ｗ.
001580   03 略称Ｗ１                         PIC N(12) VALUE SPACE.
001590   03 FILLER                           PIC N(2) VALUE SPACE.
001240 01 施術和暦年月ＷＲ.
001250    03 施術和暦ＷＲ                    PIC 9    VALUE ZERO.
001260    03 施術年月ＷＲ.
001270       05 施術年ＷＲ                   PIC 9(2) VALUE ZERO.
001280       05 施術月ＷＲ                   PIC 9(2) VALUE ZERO.
001240 01 請求和暦年月日Ｗ.
001250    03 請求和暦Ｗ                      PIC 9    VALUE ZERO.
001260    03 請求年月Ｗ.
002960       05 請求年Ｗ                     PIC 9(2) VALUE ZERO.
002970       05 請求月Ｗ                     PIC 9(2) VALUE ZERO.
002970    03 請求日Ｗ                        PIC 9(2) VALUE ZERO.
001320 01 元号名称Ｗ                         PIC N(2) VALUE SPACE.
002433* エラーメッセージ用
002434 01 エラーメッセージＷ.
002435    03 エラー患者コードＷ              PIC X(7)  VALUE SPACE.
002436    03 エラー区切りＷ                  PIC X(1)  VALUE SPACE.
002437    03 エラー保険種別Ｗ                PIC X(2)  VALUE SPACE.
002438    03 FILLER                          PIC X(10) VALUE SPACE.
       01 労災フラグ                         PIC X(3)  VALUE SPACE.
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
001793 01 連メ７－キー IS EXTERNAL.
001794    03  連メ７－メッセージ１               PIC X(40).
001795    03  連メ７－メッセージ２               PIC X(40).
001796*
003080****************
003090* 画面入力情報 *
003100****************
004960 01 連入－入力データＹＧＮ７２０ IS EXTERNAL.
004970    03 連入－開始和暦年月日.
004980       05 連入－開始和暦                  PIC 9(1).
004990       05 連入－開始年                    PIC 9(2).
005000       05 連入－開始月                    PIC 9(2).
005010       05 連入－開始日                    PIC 9(2).
005020    03 連入－終了和暦年月日.
005030       05 連入－終了和暦                  PIC 9(1).
005040       05 連入－終了年                    PIC 9(2).
005050       05 連入－終了月                    PIC 9(2).
005060       05 連入－終了日                    PIC 9(2).
005080    03 連入－保険種別                     PIC 9(2).
005090    03 連入－本人家族区分                 PIC 9(1).
005100    03 連入－患者コード.
005110       05 連入－患者番号                  PIC 9(6).
005120       05 連入－枝番                      PIC X(1).
005140    03 連入－印刷条件                     PIC 9(2).
      *
004140 01 連入－表示フラグＹＧＮ７２０ IS EXTERNAL GLOBAL.
          03 連入－プレビュー区分               PIC 9(1).
002870************
002880* 印刷キー *
002890************
       01 連印－対象データＹＧＮ７２０ IS EXTERNAL.
          03 連印－施術和暦年月日.
             05 連印－施術和暦                  PIC 9(1).
             05 連印－施術年                    PIC 9(2).
             05 連印－施術月                    PIC 9(2).
             05 連印－施術日                    PIC 9(2).
          03 連印－患者コード.
             05 連印－患者番号                  PIC 9(6).
             05 連印－枝番                      PIC X(1).
          03 連印－ワクモード                   PIC 9(1).
          03 連印－氏名モード                   PIC 9(1).
          03 連印－明細モード                   PIC 9(1).
          03 連印－合計モード                   PIC 9(1).
          03 連印－コメントモード               PIC 9(1).
          03 連印－読み順                       PIC X(4).
          03 連印－段数                         PIC 9(2).
          03 連印－処理キー                     PIC X(4).
004580*
000540************************************
000550* プリンタファイル作成用           *
000560************************************
000570 01 Ｈ連ＰＲＴＦ－作成データ IS EXTERNAL.
000580     03 Ｈ連ＰＲＴＦ－ファイル名           PIC X(8).
000590     03 Ｈ連ＰＲＴＦ－プレビュー区分       PIC 9.
000600     03 Ｈ連ＰＲＴＦ－帳票プログラム名     PIC X(8).
000610     03 Ｈ連ＰＲＴＦ－オーバレイ名         PIC X(8).
007308*
003890******************************************************************
003900*                      PROCEDURE  DIVISION                       *
003910******************************************************************
003920 PROCEDURE               DIVISION.
003930************
003940*           *
003950* 初期処理   *
003960*           *
003970************
002570     PERFORM プリンタファイル作成.
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
002860*================================================================*
002870 プリンタファイル作成 SECTION.
002880*================================================================*
002890*   / 初期化 /
002900     MOVE SPACE TO Ｈ連ＰＲＴＦ－作成データ.
002910     INITIALIZE Ｈ連ＰＲＴＦ－作成データ.
002920*
002930*
002940*--↓↓ 変更箇所 ↓↓--------------------------------------*
002970*   使用するプリンタファイル名セット
002231     MOVE "PRTF001"             TO Ｈ連ＰＲＴＦ－ファイル名.
002972*
002973*   使用する帳票プログラム名セット
002974     MOVE "YGN723"              TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分 TO Ｈ連ＰＲＴＦ－プレビュー区分.
003000*     MOVE 1 TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
004710*================================================================*
004720 初期化 SECTION.
004730*
004770     OPEN INPUT 元号マスタ.
004780             MOVE NC"元号" TO ファイル名Ｗ.
004790             PERFORM オープンチェック.
004800     OPEN INPUT 制御情報マスタ.
004810             MOVE NC"制御" TO ファイル名Ｗ.
004820             PERFORM オープンチェック.
004830     OPEN INPUT 施術所情報マスタ.
004840             MOVE NC"施情" TO ファイル名Ｗ.
004850             PERFORM オープンチェック.
004800     OPEN INPUT 名称マスタ.
004810             MOVE NC"名称" TO ファイル名Ｗ.
004820             PERFORM オープンチェック.
004800     OPEN INPUT 受診者情報Ｆ.
004810             MOVE NC"受診" TO ファイル名Ｗ.
004820             PERFORM オープンチェック.
007640     OPEN INPUT 作業ファイル１.
007650             MOVE NC"作１" TO ファイル名Ｗ.
007660             PERFORM オープンチェック.
005960*     OPEN I-O   印刷ファイル.
      *         MOVE NC"印刷ファイル" TO ファイル名Ｗ.
      *         PERFORM エラー処理Ｐ.
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
005020     COMPUTE 計算機和暦年Ｗ = 計算機西暦年 - 和暦終了年Ｗ.
005060*
005080*================================================================*
005090 オープンチェック SECTION.
005100*
005110     IF 状態キー  NOT =  "00"
005120         DISPLAY ファイル名Ｗ NC"Ｆオープンエラー" UPON CONS
005130         DISPLAY NC"状態キー：" 状態キー           UPON CONS
005140         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
005150                                                   UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
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
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
005380         ACCEPT  キー入力 FROM CONS
005390         PERFORM 終了処理
005400         EXIT PROGRAM
005410     NOT INVALID KEY
005420         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
005430         MOVE 前和暦Ｗ TO 元－元号区分
005440         READ 元号マスタ
005450         INVALID KEY
005460             DISPLAY NC"指定和暦が登録されていません" UPON CONS
005470             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
005480                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
005490             ACCEPT  キー入力 FROM CONS
005500             PERFORM 終了処理
005510             EXIT PROGRAM
005520         NOT INVALID KEY
005530             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
005540         END-READ
005550     END-READ.
005560*
006150*================================================================*
006770 ファイル閉鎖 SECTION.
006780*
006790     CLOSE  元号マスタ     制御情報マスタ  施術所情報マスタ
                  受診者情報Ｆ   名称マスタ      作業ファイル１.
002989*
002990     IF ( オープンフラグ = "YES" )
002991         CLOSE 印刷ファイル
003041     END-IF.
003042*
006810*================================================================*
006820 終了処理 SECTION.
006830*
006840     PERFORM ファイル閉鎖.
      *================================================================*
010770 印刷処理 SECTION.
010780*
           MOVE 50    TO 最大行数.
           MOVE 99    TO 行カウンタ.
           MOVE ZERO  TO 頁カウンタ.
007740     MOVE SPACE TO 終了フラグ.
      *
           MOVE ZERO  TO 作１－施術和暦年月日.
           MOVE SPACE TO 作１－患者カナ.
           MOVE SPACE TO 作１－患者コード.
      *
           IF 連印－読み順 = "BANG"
               START 作業ファイル１ KEY IS > 作１－患者コード
                                             作１－施術和暦年月日
               END-START
           ELSE
               START 作業ファイル１ KEY IS > 作１－患者カナ
                                             作１－患者コード
                                             作１－施術和暦年月日
           END-IF.
013500     IF 状態キー  =  "00"
               PERFORM 作業ファイル１読込
               PERFORM UNTIL 終了フラグ = "YES"
                   IF ( 行カウンタ        >= 最大行数   )
                       COMPUTE 頁カウンタ = 頁カウンタ + 1
                       PERFORM ヘッダレコードセット
                       PERFORM ヘッダ印刷処理
                       MOVE ZERO TO 行カウンタ
                   ELSE
004540                 PERFORM 明細レコードセット
004550                 PERFORM 明細印刷処理
                       MOVE 作１－患者コード TO 患者コードＷ
                       PERFORM UNTIL ( 作１－患者コード NOT = 患者コードＷ ) OR
                                     ( 終了フラグ = "YES" )
                           PERFORM 作業ファイル１読込
                       END-PERFORM
                   END-IF
               END-PERFORM
               IF 労災フラグ = "YES"
                   PERFORM 労災印刷処理
               END-IF
           END-IF.
      *================================================================*
007700 作業ファイル１読込 SECTION.
007710*
007720     READ 作業ファイル１ NEXT
007730     AT END
007740         MOVE "YES" TO 終了フラグ
007750     END-READ.
007760*================================================================*
       ヘッダレコードセット SECTION.
      *
           MOVE SPACE TO YGN723P.
           INITIALIZE YGN723P.
           MOVE 頁カウンタ         TO ページ.
      */開始和暦年月
006960     MOVE 連入－開始和暦    TO 元－元号区分.
006970     READ 元号マスタ
006980     INVALID KEY
006990         MOVE SPACE         TO 開始和暦
007000     NOT INVALID KEY
007020         MOVE 元－元号名称  TO 開始和暦
007030     END-READ.
003140     MOVE 連入－開始年      TO 開始年.
003150     MOVE 連入－開始月      TO 開始月.
      */終了和暦年月
006960     MOVE 連入－終了和暦    TO 元－元号区分.
006970     READ 元号マスタ
006980     INVALID KEY
006990         MOVE SPACE         TO 終了和暦
007000     NOT INVALID KEY
007020         MOVE 元－元号名称  TO 終了和暦
007030     END-READ.
003140     MOVE 連入－終了年      TO 終了年.
003150     MOVE 連入－終了月      TO 終了月.
007700*================================================================*
010770 明細レコードセット SECTION.
010780*
           MOVE SPACE TO YGN723P.
           INITIALIZE YGN723P.
      *
           MOVE 作１－患者コード TO 患者コード.
           MOVE 作１－患者コード TO 受－患者コード.
001390     MOVE 作１－施術和暦   TO 受－施術和暦.
001400     MOVE 作１－施術年     TO 受－施術年.
001410     MOVE 作１－施術月     TO 受－施術月.
009200     READ 受診者情報Ｆ
009210     INVALID KEY
009220         MOVE  NC"受診者情報が登録されていません" TO 連メ－メッセージ
009230         CALL   "MSG001"
009240         CANCEL "MSG001"
009250     NOT INVALID KEY
               MOVE 受－患者氏名     TO 患者氏名
               MOVE 受－被保険者氏名 TO 被保険者氏名
      */保険種別
               MOVE 02                   TO 名－区分コード
               MOVE 受－保険種別         TO 名－名称コード
               READ 名称マスタ
               NOT INVALID KEY
                   MOVE 名－略称         TO 保険種別略称
               END-READ
      *
               IF 受－施術和暦年月 < 42004
                   MOVE 02                   TO 名－区分コード
                   MOVE 受－公費種別         TO 名－名称コード
                   READ 名称マスタ
                   NOT INVALID KEY
                       MOVE 名－略称         TO 公費種別略称
                   END-READ
               END-IF
      *
               MOVE 12                   TO 名－区分コード
               MOVE 受－助成種別         TO 名－名称コード
               READ 名称マスタ
               NOT INVALID KEY
                   MOVE 名－略称         TO 助成種別略称
               END-READ
           END-READ.
      *================================================================*
       ヘッダ印刷処理 SECTION.
011790*
004310     IF ( オープンフラグ NOT = "YES" )
004320        MOVE "YES" TO オープンフラグ
004330        OPEN I-O  印刷ファイル
004340        PERFORM エラー処理Ｐ
004350     END-IF.
004360*
011800     MOVE "YGN723P"  TO  定義体名Ｐ.
011810     MOVE "HEAD01"   TO  項目群名Ｐ.
011820     WRITE YGN723P.
           PERFORM エラー処理Ｐ.
      *================================================================*
       明細印刷処理 SECTION.
011790*
011800     MOVE "YGN723P"  TO  定義体名Ｐ.
011810     MOVE "GRP001"   TO  項目群名Ｐ.
011820     WRITE YGN723P.
           PERFORM エラー処理Ｐ.
           COMPUTE 行カウンタ = 行カウンタ + 1.
      *================================================================*
       労災印刷処理 SECTION.
011790*
011800     MOVE "YGN723P"  TO  定義体名Ｐ.
011810     MOVE "GRP002"   TO  項目群名Ｐ.
011820     WRITE YGN723P.
           PERFORM エラー処理Ｐ.
      *================================================================*
       改頁処理  SECTION.
      *
004310     IF ( オープンフラグ NOT = "YES" )
004320        MOVE "YES" TO オープンフラグ
004330        OPEN I-O  印刷ファイル
004340        PERFORM エラー処理Ｐ
004350     END-IF.
004360*
011820     MOVE SPACE TO YGN723P.
011820     INITIALIZE YGN723P.
           MOVE "YGN723P" TO  定義体名Ｐ.
           MOVE "CT"      TO  処理種別Ｐ.
           MOVE "PAGE"    TO  拡張制御Ｐ.
           MOVE SPACE     TO  項目群名Ｐ.
011820     WRITE YGN723P.
           PERFORM エラー処理Ｐ.
           MOVE SPACE     TO  拡張制御Ｐ.
      *================================================================*
014060 エラー表示 SECTION.
014070*
014080     DISPLAY NC"ファイル書込エラー：" ファイル名Ｗ UPON CONS.
014090     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
014100     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
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
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
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
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
014170     ACCEPT  キー入力 FROM CONS.
014180     PERFORM ファイル閉鎖.
014190     EXIT PROGRAM.
014200*================================================================*
014210 エラー表示その他 SECTION.
014220*
014230     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
014240     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
014250                                                   UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
014260     ACCEPT  キー入力 FROM CONS.
014270     PERFORM ファイル閉鎖.
014280     EXIT PROGRAM.
001370*================================================================*
014300******************************************************************
014310 END PROGRAM YGN723.
014320******************************************************************
