000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAW9223.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*         受診者ラベル印刷リスト
000100*         MED = YAW922 YAW9223P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2009-08-20
000130 DATE-COMPILED.          2009-08-20
      *
      */2014.07.14 受診者カナ順印刷追加
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
000260     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY               IS  元－元号区分
000300                             FILE STATUS              IS  状態キー
000310                             LOCK        MODE         IS  AUTOMATIC.
000320     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000330                             ORGANIZATION             IS  INDEXED
000340                             ACCESS MODE              IS  DYNAMIC
000350                             RECORD KEY               IS  制－制御区分
000360                             FILE STATUS              IS  状態キー
000370                             LOCK        MODE         IS  AUTOMATIC.
000380     SELECT  検索内容Ｆ      ASSIGN      TO        KENSAKUL
000390                             ORGANIZATION             IS  INDEXED
000400                             ACCESS MODE              IS  DYNAMIC
000410                             RECORD KEY               IS  検索－制御区分
000420                             FILE STATUS              IS  状態キー
000430                             LOCK        MODE         IS  AUTOMATIC.
000440     SELECT  作業ファイル１  ASSIGN      TO        "C:\MAKISHISYS\HMOBJ\TEMP\H9221L.DAT"
000450                             ORGANIZATION             IS  INDEXED
000460                             ACCESS                   IS  DYNAMIC
000470                             RECORD      KEY          IS  作１－施術和暦年月Ｎ
000480                                                          作１－患者コード
000490                             ALTERNATE RECORD KEY     IS  作１－印刷区分
000500                                                          作１－施術和暦年月Ｎ
000510                                                          作１－患者コード
000520*/                                                       */並び順の変更/0404
000530                             ALTERNATE RECORD KEY     IS  作１－患者郵便番号
000540                                                          作１－患者カナ
000550                                                          作１－患者コード
000560                                                          作１－施術和暦年月Ｎ
000570                             ALTERNATE RECORD KEY     IS  作１－印刷区分
000580                                                          作１－患者郵便番号
000590                                                          作１－患者カナ
000600                                                          作１－患者コード
000610                                                          作１－施術和暦年月Ｎ
000930                             ALTERNATE RECORD KEY     IS  作１－印刷区分
000400                                                          作１－患者カナ
000950                                                          作１－患者コード
000960                                                          作１－施術和暦年月Ｎ
000620                             FILE        STATUS       IS  状態キー
000630                             LOCK        MODE         IS  AUTOMATIC.
000640     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF001
000650                             SYMBOLIC    DESTINATION  IS "PRT"
000660                             FORMAT                   IS  定義体名Ｐ
000670                             GROUP                    IS  項目群名Ｐ
000680                             PROCESSING  MODE         IS  処理種別Ｐ
000690                             UNIT        CONTROL      IS  拡張制御Ｐ
000700                             FILE        STATUS       IS  通知情報Ｐ.
000710******************************************************************
000720*                      DATA DIVISION                             *
000730******************************************************************
000740 DATA                    DIVISION.
000750 FILE                    SECTION.
000760*                           ［ＲＬ＝  １２８］
000770 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
000780     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
000790*                           ［ＲＬ＝  ２５６］
000800 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
000810     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
000820
000830 FD  検索内容Ｆ          BLOCK   CONTAINS   1   RECORDS.
000840     COPY KENSAKU         OF  XFDLIB  JOINING   検索   AS  PREFIX.
000850* 
000860 FD  作業ファイル１ RECORD  CONTAINS 320 CHARACTERS.
000870 01  作１－レコード.
000880     03  作１－レコードキー.
000890       05 作１－施術和暦年月Ｎ.
000900         07 作１－施術和暦Ｎ                 PIC 9.
000910         07 作１－施術年月Ｎ.
000920           09 作１－施術年Ｎ                 PIC 9(2).
000930           09 作１－施術月Ｎ                 PIC 9(2).
000940       05 作１－患者コード.
000950         07 作１－患者番号                   PIC 9(6).
000960         07 作１－枝番                       PIC X.
000970     03  作１－レコードデータ.
000980       05 作１－患者カナ                     PIC X(50).
000990       05 作１－患者氏名                     PIC X(50).
001000       05 作１－患者郵便番号.
001010         07 作１－患者郵便番号１             PIC X(3).
001020         07 作１－患者郵便番号２             PIC X(4).
001030       05 作１－患者住所１                   PIC X(50).
001040       05 作１－患者住所２                   PIC X(50).
001050       05 作１－患者生年月日.
001060         07 作１－患者和暦                   PIC 9.
001070         07 作１－患者年                     PIC 9(2).
001080         07 作１－患者月                     PIC 9(2).
001090         07 作１－患者日                     PIC 9(2).
001100       05 作１－患者電話番号                 PIC X(30).
001110       05 作１－患者性別                     PIC 9.
001120       05 作１－印刷区分                     PIC 9.
001130       05 作１－インデックス                 PIC 9(7).
001140       05 FILLER                             PIC X(55).
001150*
001160 FD  印刷ファイル.
001170     COPY YAW9223P         OF  XMDLIB.
001180*----------------------------------------------------------------*
001190******************************************************************
001200*                WORKING-STORAGE SECTION                         *
001210******************************************************************
001220 WORKING-STORAGE         SECTION.
001230 01 キー入力                           PIC X    VALUE SPACE.
001240 01 状態キー                           PIC X(2) VALUE SPACE.
001250 01 終了フラグ                         PIC X(3) VALUE SPACE.
001260 01 ファイル名                         PIC N(10).
001270 01 オープンフラグ                     PIC X(3) VALUE SPACE.
001280*
001290 01 行カウンタ                         PIC 9(2) VALUE 0.
001300 01 頁カウンタ                         PIC 9(4) VALUE 0.
001310 01 最大行数                           PIC 9(2) VALUE 0.
001320 01 ヘッダ行数                         PIC 9(2) VALUE 0.
001330*
001340 01 連番Ｗ                             PIC 9(6) VALUE ZERO.
001350 01 小計件数Ｗ                         PIC 9(4) VALUE ZERO.
001360 01 合計件数Ｗ                         PIC 9(4) VALUE ZERO.
001370*
001380 01 現在和暦年月Ｗ.
001390    03 現在和暦Ｗ                      PIC 9(1).
001400    03 現在年Ｗ                        PIC 9(2).
001410    03 現在月Ｗ                        PIC 9(2).
001420 01 前和暦Ｗ                           PIC 9 VALUE ZERO.
001430 01 カレント元号Ｗ                     PIC 9(1) VALUE ZERO.
001440*
001450 01 和暦名称Ｗ.
001460    03 和暦名称２Ｗ                    PIC N(2) VALUE SPACE.
001470 01 施術和暦年月Ｗ.
001480    03 施術和暦Ｗ                      PIC 9    VALUE ZERO.
001490    03 施術年月Ｗ.
001500       05 施術年Ｗ                     PIC 9(2) VALUE ZERO.
001510       05 施術月Ｗ                     PIC 9(2) VALUE ZERO.
001520 01 開始年齢Ｗ                         PIC Z(3) VALUE ZERO.
001530 01 終了年齢Ｗ                         PIC Z(3) VALUE ZERO.
001540*
001550 01 抽出条件コメントＷ                 PIC X(100) VALUE SPACE.
001560 01 抽出条件コメントＷ２               PIC X(100) VALUE SPACE.
001570 01 抽出条件コメントＷ３               PIC X(100) VALUE SPACE.
001580 01 抽出条件コメントＷ４               PIC X(100) VALUE SPACE.
001590 01 抽出条件コメントＷ５               PIC X(100) VALUE SPACE.
001600 01 抽出条件コメントＷ６               PIC X(100) VALUE SPACE.
001610 01 抽出条件コメントＷ７               PIC X(100) VALUE SPACE.
001620 01 抽出条件コメントＷ８               PIC X(100) VALUE SPACE.
001630*
001640 01 誕生日Ｗ                           PIC X(20)  VALUE SPACE.
001650 01 条件Ｗ                             PIC X(100) VALUE SPACE.
001660 01 指定年月Ｗ                         PIC X(40)  VALUE SPACE.
001670 01 和暦Ｗ                             PIC X(4)   VALUE SPACE.
001680 01 地名Ｗ                             PIC X(100) VALUE SPACE.
001690 01 年齢Ｗ                             PIC X(10)  VALUE SPACE.
001700*
001710 01 印刷制御.
001720     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
001730     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
001740     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
001750     03 拡張制御Ｐ.
001760         05 端末制御Ｐ.
001770             07 移動方向Ｐ             PIC X(1).
001780             07 移動行数Ｐ             PIC 9(3).
001790         05 詳細制御Ｐ                 PIC X(2).
001800     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
001810     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
001820*
001830 01 計算機西暦年Ｗ                     PIC 9(2).
001840* 日付ＷＯＲＫ
001850 01 和暦終了年Ｗ                       PIC 9(4).
001860 01 計算機西暦.
001870    03 計算機西暦年                    PIC 9(4).
001880    03 計算機西暦月日                  PIC 9(4).
001890 01 計算機西暦Ｒ REDEFINES 計算機西暦.
001900    03 計算機世紀                      PIC 9(2).
001910    03 計算機日付                      PIC 9(6).
001920    03 計算機日付Ｒ REDEFINES 計算機日付.
001930       05 計算機年月                   PIC 9(4).
001940       05 計算機年月Ｒ REDEFINES 計算機年月.
001950         07 計算機年                   PIC 9(2).
001960         07 計算機月                   PIC 9(2).
001970       05 計算機日                     PIC 9(2).
001980******************************************************************
001990*                          連結項目                              *
002000******************************************************************
002010************************************
002020* プリンタファイル作成用           *
002030************************************
002040 01 Ｈ連ＰＲＴＦ－作成データ IS EXTERNAL.
002050     03 Ｈ連ＰＲＴＦ－ファイル名           PIC X(8).
002060     03 Ｈ連ＰＲＴＦ－プレビュー区分       PIC 9.
002070     03 Ｈ連ＰＲＴＦ－帳票プログラム名     PIC X(8).
002080     03 Ｈ連ＰＲＴＦ－オーバレイ名         PIC X(8).
002090*
002100*
002110********************
002120* メッセージ表示キー *
002130********************
002140 01 連メ－キー IS EXTERNAL.
002150    03  連メ－メッセージ               PIC N(20).
002160*
002170****************
002180* 画面入力情報 *
002190****************
002200 01 連入－入力区分９２２ IS EXTERNAL GLOBAL.
002210    03 連入－表示区分                PIC 9(1).
002220*/0:受診者住所を出力、1:被保険者住所を出力。
002230    03 連入－住所区分                PIC 9(1).
002240*/0:ラベルを出力、1:リストを出力、3:ＣＳＶ。
002250    03 連入－印刷形式                PIC 9(1).
002260*/ラベルの開始位置
002270    03 連入－ラベル開始位置          PIC 9(2).
      *
      */0:郵便番号順 1:受診者カナ順
       01 連入－印刷区分９２２ IS EXTERNAL.
          03 連入－印刷順区分              PIC 9(1).
002280*
002290******************************************************************
002300*                      PROCEDURE  DIVISION                       *
002310******************************************************************
002320 PROCEDURE               DIVISION.
002330************
002340*           *
002350* 初期処理   *
002360*           *
002370************
002380     PERFORM プリンタファイル作成.
002390     PERFORM 初期化.
002400*     PERFORM 抽出条件セット.
002410************
002420*           *
002430* 主処理     *
002440*           *
002450************
002460     PERFORM 印刷処理.
002470************
002480*           *
002490* 終了処理   *
002500*           *
002510************
002520     PERFORM 終了処理.
002530     EXIT PROGRAM.
002540*
002550*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
002560*================================================================*
002570 プリンタファイル作成 SECTION.
002580*
002590*   / 初期化 /
002600     MOVE SPACE TO Ｈ連ＰＲＴＦ－作成データ.
002610     INITIALIZE Ｈ連ＰＲＴＦ－作成データ.
002620*
002630*
002640*--↓↓ 変更箇所 ↓↓--------------------------------------*
002650*   使用するプリンタファイル名セット
002660     MOVE "PRTF001"             TO Ｈ連ＰＲＴＦ－ファイル名.
002670*
002680*   使用する帳票プログラム名セット
002690     MOVE "YAW9223P"              TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002700*
002710*   / プレビュー区分セット /
002720     MOVE ZERO TO Ｈ連ＰＲＴＦ－プレビュー区分.
002730*****     MOVE Ｈ連入－プレビュー区分  TO Ｈ連ＰＲＴＦ－プレビュー区分.
002740*
002750*--↑↑-----------------------------------------------------*
002760*
002770     CALL   "CRTPRTF".
002780     CANCEL "CRTPRTF".
002790*
002800*================================================================*
002810 初期化 SECTION.
002820*
002830     PERFORM ファイルオープン.
002840*    /* 現在日付取得 */
002850     ACCEPT 計算機日付 FROM DATE.
002860*    /* 1980～2079年の間で設定 */
002870     IF ( 計算機年 > 80 )
002880         MOVE 19 TO 計算機世紀
002890     ELSE
002900         MOVE 20 TO 計算機世紀
002910     END-IF.
002920     PERFORM カレント元号取得.
002930     PERFORM 和暦終了年取得.
002940*     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
002950*
002960*     MOVE カレント元号Ｗ TO 現在和暦Ｗ.
002970*     MOVE 計算機西暦年Ｗ TO 現在年Ｗ.
002980*     MOVE 計算機月       TO 現在月Ｗ.
002990*================================================================*
003000 カレント元号取得 SECTION.
003010*
003020     MOVE ZEROS TO 制－制御区分.
003030     READ 制御情報マスタ
003040     NOT INVALID KEY
003050         MOVE 制－カレント元号 TO カレント元号Ｗ
003060     END-READ.
003070*
003080*================================================================*
003090 和暦終了年取得 SECTION.
003100*
003110     MOVE カレント元号Ｗ TO 元－元号区分.
003120     READ 元号マスタ
003130     INVALID KEY
003140         DISPLAY NC"指定和暦が登録されていません" UPON CONS
003150         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
003160                                                  UPON CONS
003170         ACCEPT  キー入力 FROM CONS
003180         PERFORM 終了処理
003190         EXIT PROGRAM
003200     NOT INVALID KEY
003210         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
003220         MOVE 前和暦Ｗ TO 元－元号区分
003230         READ 元号マスタ
003240         INVALID KEY
003250             DISPLAY NC"指定和暦が登録されていません" UPON CONS
003260             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
003270                                                      UPON CONS
003280             ACCEPT  キー入力 FROM CONS
003290             PERFORM 終了処理
003300             EXIT PROGRAM
003310         NOT INVALID KEY
003320             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
003330         END-READ
003340     END-READ.
003350*
003360*================================================================*
003370 ファイルオープン SECTION.
003380*
003390     OPEN INPUT 元号マスタ
003400         MOVE NC"元号" TO ファイル名.
003410         PERFORM オープンチェック.
003420     OPEN INPUT 制御情報マスタ
003430         MOVE NC"制御情報" TO ファイル名.
003440         PERFORM オープンチェック.
003450     OPEN INPUT 検索内容Ｆ
003460         MOVE NC"検索内容Ｆ" TO ファイル名.
003470         PERFORM オープンチェック.
003480     OPEN INPUT 作業ファイル１
003490         MOVE NC"作１" TO ファイル名.
003500         PERFORM オープンチェック.
003510* プレビューの時、OPENしてWRITEせずにCLOSEすると、エラーがでるので、データがあったらOPENする
003520*     OPEN I-O   印刷ファイル
003530*         PERFORM エラー処理Ｐ.
003540*================================================================*
003550 オープンチェック SECTION.
003560*
003570     IF ( 状態キー  NOT =  "00" )
003580         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
003590         DISPLAY NC"状態キー：" 状態キー         UPON CONS
003600         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
003610                                                 UPON CONS
003620         CALL "actcshm"  WITH C LINKAGE
003630         ACCEPT  キー入力 FROM CONS
003640         PERFORM ファイル閉鎖
003650         EXIT PROGRAM.
003660*================================================================*
003670 ファイル閉鎖 SECTION.
003680*
003690     IF ( オープンフラグ = "YES" )
003700         CLOSE 印刷ファイル
003710     ELSE
003720         MOVE  NC"　　データが０件です。確認して下さい。" TO 連メ－メッセージ
003730         CALL   "MSG001"
003740         CANCEL "MSG001"
003750     END-IF.
003760*
003770     CLOSE 元号マスタ 制御情報マスタ 作業ファイル１.
003780*================================================================*
003790 終了処理 SECTION.
003800*
003810     PERFORM ファイル閉鎖.
003820*================================================================*
003830 エラー表示 SECTION.
003840*
003850     DISPLAY NC"状態キー" 状態キー  UPON CONS.
003860     DISPLAY NC"５ファイル書込エラー：" ファイル名   UPON CONS.
003870     DISPLAY NC"システム管理者に連絡してください"    UPON CONS.
003880     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
003890                                                     UPON CONS.
003900     CALL "actcshm"  WITH C LINKAGE.
003910     ACCEPT  キー入力 FROM CONS.
003920     PERFORM ファイル閉鎖.
003930     EXIT PROGRAM.
003940*================================================================*
003950 印刷処理 SECTION.
003960*
003970     MOVE 60    TO 最大行数.
003980     MOVE 6     TO ヘッダ行数.
003990     MOVE 1     TO 連番Ｗ.
004000*
004010     MOVE SPACE TO 終了フラグ.
004020     MOVE ZERO  TO 行カウンタ.
004030     MOVE ZERO  TO 頁カウンタ.
004040*/並び順を郵便番号患者カナ順にする。/0404
004050     MOVE  1    TO 作１－印刷区分.
004060     MOVE SPACE TO 作１－患者郵便番号.
004070     MOVE SPACE TO 作１－患者カナ.
004080     MOVE ZERO  TO 作１－施術和暦Ｎ.
004090     MOVE ZERO  TO 作１－施術年Ｎ.
004100     MOVE ZERO  TO 作１－施術月Ｎ.
004110     MOVE ZERO  TO 作１－患者番号.
004120     MOVE SPACE TO 作１－枝番.
      */2014.07.14 受診者カナ順印刷追加
           IF 連入－印刷順区分 = ZERO
003170         START 作業ファイル１  KEY IS >= 作１－印刷区分
003180                                         作１－患者郵便番号
003190                                         作１－患者カナ
003200                                         作１－患者コード
003210                                         作１－施術和暦年月Ｎ
003220         END-START
           ELSE
003170         START 作業ファイル１  KEY IS >= 作１－印刷区分
003190                                         作１－患者カナ
003200                                         作１－患者コード
003210                                         作１－施術和暦年月Ｎ
003220         END-START
           END-IF.
004190     IF ( 状態キー = "00" )
004200         PERFORM 作業ファイル１読込
004210         COMPUTE 頁カウンタ = 頁カウンタ + 1
004220         IF ( 終了フラグ NOT = "YES" )
004230             MOVE "YES" TO オープンフラグ
004240             OPEN I-O  印刷ファイル
004250             PERFORM エラー処理Ｐ
004260*
004270             PERFORM ヘッダ印字処理
004280         END-IF
004290         PERFORM UNTIL ( 終了フラグ = "YES" ) OR ( 作１－印刷区分 NOT = 1 )
004300             IF ( 行カウンタ >= 最大行数 )
004310                 PERFORM 改頁処理
004320                 COMPUTE 頁カウンタ = 頁カウンタ + 1
004330                 PERFORM ヘッダ印字処理
004340             ELSE
004350                 PERFORM 明細印字処理
004360                 PERFORM 合計額累計
004370                 PERFORM 作業ファイル１読込
004380                 COMPUTE 連番Ｗ = 連番Ｗ + 1
004390             END-IF
004400         END-PERFORM
004410     END-IF.
004420     IF ( オープンフラグ = "YES" )
004430        PERFORM 改頁処理
004440     END-IF.
004450*
004460*================================================================*
004470 作業ファイル１読込 SECTION.
004480*
004490     READ 作業ファイル１ NEXT
004500     AT END
004510         MOVE "YES" TO 終了フラグ
004520     END-READ.
004530*
004540*================================================================*
004550 改頁処理  SECTION.
004560*
004570     MOVE "YAW9223P"  TO  定義体名Ｐ.
004580     MOVE "CT"      TO  処理種別Ｐ.
004590     MOVE "PAGE"    TO  拡張制御Ｐ.
004600     MOVE SPACE     TO  項目群名Ｐ.
004610     WRITE YAW9223P.
004620     PERFORM エラー処理Ｐ.
004630     MOVE SPACE     TO  拡張制御Ｐ.
004640*
004650     CLOSE  印刷ファイル.
004660     OPEN I-O   印刷ファイル.
004670     PERFORM エラー処理Ｐ.
004680*
004690*================================================================*
004700 合計額累計 SECTION.
004710*
004720     COMPUTE 小計件数Ｗ   = 小計件数Ｗ   + 1.
004730     COMPUTE 合計件数Ｗ   = 合計件数Ｗ   + 1.
004740*================================================================*
004750 ヘッダ印字処理 SECTION.
004760*
004770     PERFORM ヘッダセット.
004780     PERFORM ヘッダ印字.
004790*/0404
004800*     PERFORM 抽出条件印字.
004810     PERFORM ヘッダ３印字.
004820*================================================================*
004830 ヘッダセット SECTION.
004840*
004850     MOVE SPACE TO YAW9223P.
004860     INITIALIZE    YAW9223P.
004870     MOVE 頁カウンタ TO ページ.
004880*
004890*================================================================*
004900 抽出条件印字 SECTION.
004910*
004920     IF ( 抽出条件コメントＷ NOT = SPACE )
004930        MOVE 抽出条件コメントＷ TO 抽出条件コメント
004940        MOVE "YAW9223P"  TO  定義体名Ｐ
004950        MOVE SPACE     TO  処理種別Ｐ
004960        MOVE "HEAD02"  TO  項目群名Ｐ
004970        WRITE YAW9223P
004980        PERFORM エラー処理Ｐ
004990        INITIALIZE YAW9223P
005000     END-IF.
005010*
005020     IF ( 抽出条件コメントＷ２ NOT = SPACE )
005030        MOVE 抽出条件コメントＷ２ TO 抽出条件コメント
005040        MOVE "YAW9223P"  TO  定義体名Ｐ
005050        MOVE SPACE     TO  処理種別Ｐ
005060        MOVE "HEAD02"  TO  項目群名Ｐ
005070        WRITE YAW9223P
005080        PERFORM エラー処理Ｐ
005090        INITIALIZE YAW9223P
005100     END-IF.
005110*
005120     IF ( 抽出条件コメントＷ３ NOT = SPACE )
005130         MOVE 抽出条件コメントＷ３ TO 抽出条件コメント
005140         MOVE "YAW9223P"  TO  定義体名Ｐ
005150         MOVE SPACE     TO  処理種別Ｐ
005160         MOVE "HEAD02"  TO  項目群名Ｐ
005170         WRITE YAW9223P
005180         PERFORM エラー処理Ｐ
005190         INITIALIZE YAW9223P
005200     END-IF.
005210*
005220     IF ( 抽出条件コメントＷ４ NOT = SPACE )
005230        MOVE 抽出条件コメントＷ４ TO 抽出条件コメント
005240        MOVE "YAW9223P"  TO  定義体名Ｐ
005250        MOVE SPACE     TO  処理種別Ｐ
005260        MOVE "HEAD02"  TO  項目群名Ｐ
005270        WRITE YAW9223P
005280        PERFORM エラー処理Ｐ
005290        INITIALIZE YAW9223P
005300     END-IF.
005310*
005320     IF ( 抽出条件コメントＷ５ NOT = SPACE )
005330         MOVE 抽出条件コメントＷ５ TO 抽出条件コメント
005340         MOVE "YAW9223P"  TO  定義体名Ｐ
005350         MOVE SPACE     TO  処理種別Ｐ
005360         MOVE "HEAD02"  TO  項目群名Ｐ
005370         WRITE YAW9223P
005380         PERFORM エラー処理Ｐ
005390         INITIALIZE YAW9223P
005400     END-IF.
005410*
005420     IF ( 抽出条件コメントＷ６ NOT = SPACE )
005430        MOVE 抽出条件コメントＷ６ TO 抽出条件コメント
005440        MOVE "YAW9223P"  TO  定義体名Ｐ
005450        MOVE SPACE     TO  処理種別Ｐ
005460        MOVE "HEAD02"  TO  項目群名Ｐ
005470        WRITE YAW9223P
005480        PERFORM エラー処理Ｐ
005490        INITIALIZE YAW9223P
005500     END-IF.
005510*
005520     IF ( 抽出条件コメントＷ７ NOT = SPACE )
005530         MOVE 抽出条件コメントＷ７ TO 抽出条件コメント
005540         MOVE "YAW9223P"  TO  定義体名Ｐ
005550         MOVE SPACE     TO  処理種別Ｐ
005560         MOVE "HEAD02"  TO  項目群名Ｐ
005570         WRITE YAW9223P
005580         PERFORM エラー処理Ｐ
005590         INITIALIZE YAW9223P
005600     END-IF.
005610*
005620     IF ( 抽出条件コメントＷ８ NOT = SPACE )
005630        MOVE 抽出条件コメントＷ８ TO 抽出条件コメント
005640        MOVE "YAW9223P"  TO  定義体名Ｐ
005650        MOVE SPACE     TO  処理種別Ｐ
005660        MOVE "HEAD02"  TO  項目群名Ｐ
005670        WRITE YAW9223P
005680        PERFORM エラー処理Ｐ
005690        INITIALIZE YAW9223P
005700     END-IF.
005710*================================================================*
005720 抽出条件セット SECTION.
005730*
005740     MOVE SPACE TO 抽出条件コメントＷ.
005750     MOVE ZERO TO 検索－制御区分.
005760     READ 検索内容Ｆ
005770     NOT INVALID KEY
005780        IF 検索－誕生日以内 NOT = ZERO
005790            STRING 抽出条件コメントＷ         DELIMITED BY SPACE
005800                   検索－誕生日以内           DELIMITED BY SPACE
005810                   "日以内に誕生日が来る人。" DELIMITED BY SIZE
005820               INTO 抽出条件コメントＷ
005830            END-STRING
005840        END-IF
005850        IF 検索－誕生和暦 NOT = ZERO
005860            EVALUATE 検索－誕生和暦
005870            WHEN 1
005880                MOVE "明治"  TO 誕生日Ｗ
005890             WHEN 2
005900                MOVE "大正"  TO 誕生日Ｗ
005910            WHEN 3
005920                MOVE "昭和"  TO 誕生日Ｗ
005930            WHEN 4
005940                MOVE "平成"  TO 誕生日Ｗ
005950            END-EVALUATE
005960        END-IF
005970        IF 検索－誕生年 NOT = ZERO
005980            STRING 誕生日Ｗ      DELIMITED BY SPACE
005990                   検索－誕生年  DELIMITED BY SPACE
006000                   "年"          DELIMITED BY SIZE
006010               INTO 誕生日Ｗ
006020            END-STRING
006030        END-IF
006040        IF 検索－誕生月 NOT = ZERO
006050            STRING 誕生日Ｗ      DELIMITED BY SPACE
006060                   検索－誕生月  DELIMITED BY SPACE
006070                   "月"          DELIMITED BY SIZE
006080               INTO 誕生日Ｗ
006090            END-STRING
006100        END-IF
006110        IF 検索－誕生日 NOT = ZERO
006120            STRING 誕生日Ｗ      DELIMITED BY SPACE
006130                   検索－誕生日  DELIMITED BY SPACE
006140                   "日"          DELIMITED BY SIZE
006150               INTO 誕生日Ｗ
006160            END-STRING
006170        END-IF
006180        IF 誕生日Ｗ NOT = SPACE
006190            STRING 抽出条件コメントＷ  DELIMITED BY SPACE
006200                   誕生日Ｗ            DELIMITED BY SPACE
006210                   "生まれの人。"      DELIMITED BY SIZE
006220               INTO 抽出条件コメントＷ
006230            END-STRING
006240        END-IF
006250        IF 検索－初療 NOT = ZERO
006260            STRING 抽出条件コメントＷ       DELIMITED BY SPACE
006270                   検索－初療範囲日数       DELIMITED BY SPACE
006280                   "日以内に初療がある人。" DELIMITED BY SIZE
006290               INTO 抽出条件コメントＷ
006300            END-STRING
006310        END-IF
006320        IF 検索－初来院 NOT = ZERO
006330            STRING 抽出条件コメントＷ           DELIMITED BY SPACE
006340                   検索－初来院範囲日数         DELIMITED BY SPACE
006350                   "日以内に初めて来院した人。" DELIMITED BY SIZE
006360               INTO 抽出条件コメントＷ
006370            END-STRING
006380        END-IF
006390*
006400        INSPECT 抽出条件コメントＷ REPLACING ALL "01" BY " 1"
006410        INSPECT 抽出条件コメントＷ REPLACING ALL "02" BY " 2"
006420        INSPECT 抽出条件コメントＷ REPLACING ALL "03" BY " 3"
006430        INSPECT 抽出条件コメントＷ REPLACING ALL "04" BY " 4"
006440        INSPECT 抽出条件コメントＷ REPLACING ALL "05" BY " 5"
006450        INSPECT 抽出条件コメントＷ REPLACING ALL "06" BY " 6"
006460        INSPECT 抽出条件コメントＷ REPLACING ALL "07" BY " 7"
006470        INSPECT 抽出条件コメントＷ REPLACING ALL "08" BY " 8"
006480        INSPECT 抽出条件コメントＷ REPLACING ALL "09" BY " 9"
006490*
006500        IF 検索－初療後経過 NOT = ZERO
006510            STRING 抽出条件コメントＷ２     DELIMITED BY SPACE
006520                   "初療後"                 DELIMITED BY SIZE
006530                   検索－初療後経過日数     DELIMITED BY SPACE
006540                   "日経過しているが、再来院がない人。（ただし経過日数"
006550                                            DELIMITED BY SIZE
006560                   検索－初療後最大経過日数 DELIMITED BY SPACE
006570                   "日まで）"               DELIMITED BY SIZE
006580               INTO 抽出条件コメントＷ２
006590            END-STRING
006600        END-IF
006610*
006620        IF 検索－最終日後経過 NOT = ZERO
006630            STRING 抽出条件コメントＷ３     DELIMITED BY SPACE
006640                   "継続で最終来院日より"   DELIMITED BY SIZE
006650                   検索－最終日後経過日数   DELIMITED BY SPACE
006660                   "日経過しているが、再来院がない人。（ただし経過日数"
006670                                            DELIMITED BY SIZE
006680                   検索－最終日後最大経過日数 DELIMITED BY SPACE
006690                   "日まで）"               DELIMITED BY SIZE
006700               INTO 抽出条件コメントＷ３
006710            END-STRING
006720        END-IF
006730*
006740        IF 検索－治癒 NOT = ZERO
006750            STRING 抽出条件コメントＷ４ DELIMITED BY SPACE
006760                   検索－治癒範囲日数   DELIMITED BY SPACE
006770                   "日以内に治癒の人。" DELIMITED BY SIZE
006780               INTO 抽出条件コメントＷ４
006790            END-STRING
006800        END-IF
006810        IF 検索－中止 NOT = ZERO
006820            STRING 抽出条件コメントＷ４ DELIMITED BY SPACE
006830                   検索－中止範囲日数   DELIMITED BY SPACE
006840                   "日以内に中止の人。" DELIMITED BY SIZE
006850               INTO 抽出条件コメントＷ４
006860            END-STRING
006870        END-IF
006880*
006890        IF 検索－範囲内初療 NOT = ZERO
006900            STRING "初療がある人。" DELIMITED BY SIZE
006910               INTO 条件Ｗ
006920            END-STRING
006930        END-IF
006940        IF 検索－レセ合計額 NOT = ZERO
006950            STRING 条件Ｗ             DELIMITED BY SPACE
006960                   "レセ合計金額が"   DELIMITED BY SIZE
006970                   検索－レセ合計額   DELIMITED BY SPACE
006980                   "円以上。"         DELIMITED BY SIZE
006990               INTO 条件Ｗ
007000            END-STRING
007010        END-IF
007020        IF 検索－自費金額 NOT = ZERO
007030            STRING 条件Ｗ           DELIMITED BY SPACE
007040                   "自費金額が"     DELIMITED BY SIZE
007050                   検索－自費金額   DELIMITED BY SPACE
007060                   "円以上。"       DELIMITED BY SIZE
007070               INTO 条件Ｗ
007080            END-STRING
007090        END-IF
007100        IF 検索－通院回数以上 NOT = ZERO
007110            STRING 条件Ｗ              DELIMITED BY SPACE
007120                   "来院回数が"        DELIMITED BY SIZE
007130                   検索－通院回数以上  DELIMITED BY SPACE
007140                   "回以上"            DELIMITED BY SIZE
007150               INTO 条件Ｗ
007160            END-STRING
007170        END-IF
007180        IF 検索－通院回数以下 NOT = ZERO
007190            IF 検索－通院回数以上 = ZERO
007200                STRING 条件Ｗ              DELIMITED BY SPACE
007210                       "来院回数が"        DELIMITED BY SIZE
007220                       検索－通院回数以下  DELIMITED BY SPACE
007230                       "回以下。"          DELIMITED BY SIZE
007240                   INTO 条件Ｗ
007250                END-STRING
007260            ELSE
007270                STRING 条件Ｗ              DELIMITED BY SPACE
007280                       検索－通院回数以下  DELIMITED BY SPACE
007290                       "回以下。"          DELIMITED BY SIZE
007300                   INTO 条件Ｗ
007310               END-STRING
007320           END-IF
007330        ELSE
007340           IF 検索－通院回数以上 NOT = ZERO
007350               STRING 条件Ｗ              DELIMITED BY SPACE
007360                      "。"                DELIMITED BY SIZE
007370                  INTO 条件Ｗ
007380               END-STRING
007390           END-IF
007400        END-IF
007410        IF 条件Ｗ NOT = SPACE
007420           IF 検索－開始施術年 NOT = ZERO
007430               EVALUATE 検索－開始施術和暦
007440               WHEN 4
007450                   MOVE "平成"  TO 和暦Ｗ
007460               END-EVALUATE
007470               STRING 和暦Ｗ           DELIMITED BY SPACE
007480                      検索－開始施術年 DELIMITED BY SPACE
007490                      "年"             DELIMITED BY SIZE
007500                      検索－開始施術月 DELIMITED BY SPACE
007510                      "月"             DELIMITED BY SIZE
007520                   INTO 指定年月Ｗ
007530               END-STRING
007540            END-IF
007550            IF 検索－終了施術年 NOT = ZERO
007560               IF 指定年月Ｗ NOT = SPACE
007570                   STRING 指定年月Ｗ       DELIMITED BY SPACE
007580                          "～"             DELIMITED BY SIZE
007590                     INTO 指定年月Ｗ
007600                   END-STRING
007610               END-IF
007620               EVALUATE 検索－終了施術和暦
007630               WHEN 4
007640                   MOVE "平成"  TO 和暦Ｗ
007650               END-EVALUATE
007660               STRING 指定年月Ｗ       DELIMITED BY SPACE
007670                      和暦Ｗ           DELIMITED BY SPACE
007680                      検索－終了施術年 DELIMITED BY SPACE
007690                      "年"             DELIMITED BY SIZE
007700                      検索－終了施術月 DELIMITED BY SPACE
007710                      "月"             DELIMITED BY SIZE
007720                 INTO 指定年月Ｗ
007730               END-STRING
007740            END-IF
007750            STRING 指定年月Ｗ         DELIMITED BY SPACE
007760                   "に"               DELIMITED BY SIZE
007770              INTO 抽出条件コメントＷ５
007780            END-STRING
007790*
007800            STRING 条件Ｗ             DELIMITED BY SPACE
007810               INTO 抽出条件コメントＷ６
007820            END-STRING
007830        END-IF
007840*
007850        IF ( 検索－地名１ NOT = SPACE )
007860            MOVE 検索－地名１ TO 地名Ｗ
007870        END-IF
007880        IF ( 検索－地名２ NOT = SPACE )
007890            STRING 地名Ｗ         DELIMITED BY SPACE
007900                   "、"           DELIMITED BY SIZE
007910                   検索－地名２   DELIMITED BY SPACE
007920               INTO 地名Ｗ
007930            END-STRING
007940        END-IF
007950        IF ( 検索－地名３ NOT = SPACE )
007960            STRING 地名Ｗ         DELIMITED BY SPACE
007970                   "、"           DELIMITED BY SIZE
007980                   検索－地名３   DELIMITED BY SPACE
007990               INTO 地名Ｗ
008000            END-STRING
008010        END-IF
008020        IF 地名Ｗ NOT = SPACE
008030             STRING "住所が"      DELIMITED BY SIZE
008040                    地名Ｗ        DELIMITED BY SPACE
008050                    "の人。"      DELIMITED BY SIZE
008060               INTO 抽出条件コメントＷ７
008070            END-STRING
008080        END-IF
008090        IF 検索－開始年齢 NOT = ZERO
008100            STRING 検索－開始年齢 DELIMITED BY SPACE
008110                   "歳"           DELIMITED BY SIZE
008120               INTO 年齢Ｗ
008130            END-STRING
008140        END-IF
008150        IF 検索－終了年齢 NOT = ZERO
008160            STRING 年齢Ｗ         DELIMITED BY SPACE
008170                   "～"           DELIMITED BY SIZE
008180                   検索－終了年齢 DELIMITED BY SPACE
008190                   "歳"           DELIMITED BY SIZE
008200               INTO 年齢Ｗ
008210            END-STRING
008220        END-IF
008230        IF 年齢Ｗ NOT = SPACE
008240            STRING "年齢が"       DELIMITED BY SIZE
008250                   年齢Ｗ         DELIMITED BY SPACE
008260                   "の人。"       DELIMITED BY SIZE
008270               INTO 抽出条件コメントＷ８
008280            END-STRING
008290        END-IF
008300        IF 検索－男性 NOT = ZERO
008310            STRING 抽出条件コメントＷ８ DELIMITED BY SPACE
008320                   "男性の人。"       DELIMITED BY SIZE
008330               INTO 抽出条件コメントＷ８
008340            END-STRING
008350        END-IF
008360        IF 検索－女性 NOT = ZERO
008370             STRING 抽出条件コメントＷ８ DELIMITED BY SPACE
008380                    "女性の人。"       DELIMITED BY SIZE
008390               INTO 抽出条件コメントＷ８
008400            END-STRING
008410        END-IF
008420     END-READ.
008430*================================================================*
008440 ヘッダ印字  SECTION.
008450*
008460     MOVE "YAW9223P"  TO  定義体名Ｐ.
008470     MOVE SPACE     TO  処理種別Ｐ.
008480     MOVE "HEAD01"  TO  項目群名Ｐ.
008490     WRITE YAW9223P.
008500     PERFORM エラー処理Ｐ.
008510     INITIALIZE YAW9223P.
008520*/0404
008530*     MOVE ヘッダ行数 TO 行カウンタ.
008540     MOVE ZERO TO 行カウンタ.
008550*================================================================*
008560 ヘッダ３印字  SECTION.
008570*
008580     MOVE "YAW9223P"  TO  定義体名Ｐ.
008590     MOVE SPACE     TO  処理種別Ｐ.
008600     MOVE "HEAD03"  TO  項目群名Ｐ.
008610     WRITE YAW9223P.
008620     PERFORM エラー処理Ｐ.
008630     INITIALIZE YAW9223P.
008640*================================================================*
008650 明細印字処理 SECTION.
008660*
008670     PERFORM 明細部セット.
008680     PERFORM 明細印字.
008690*================================================================*
008700 明細部セット SECTION.
008710*
008720     MOVE SPACE TO YAW9223P.
008730     INITIALIZE    YAW9223P.
008740     MOVE 連番Ｗ               TO 連番.
008750*     MOVE 作１－患者コード     TO 患者コード.
008760     MOVE 作１－患者番号       TO 患者番号.
008770     MOVE 作１－枝番           TO 枝番.
008780     MOVE NC"〒"               TO 郵便マーク.
008790     MOVE 作１－患者郵便番号１ TO 郵便番号１.
008800     MOVE 作１－患者郵便番号２ TO 郵便番号２.
008810     IF 作１－患者郵便番号２ = SPACE
008820         MOVE SPACE            TO 区切
008830     ELSE
008840         MOVE "-"              TO 区切
008850     END-IF.
008860     MOVE SPACE                TO 受診者住所.
008870     STRING 作１－患者住所１ DELIMITED BY SPACE
008880            作１－患者住所２ DELIMITED BY SPACE
008890            INTO 受診者住所
008900     END-STRING.
008910     MOVE 作１－患者氏名 TO 患者氏名.
008920*
008930*================================================================*
008940 明細印字  SECTION.
008950*
008960     MOVE "YAW9223P"   TO  定義体名Ｐ.
008970     MOVE "GRP001"   TO  項目群名Ｐ.
008980     WRITE YAW9223P.
008990     PERFORM エラー処理Ｐ.
009000     INITIALIZE YAW9223P.
009010     COMPUTE 行カウンタ = 行カウンタ + 1.
009020*================================================================*
009030 エラー処理Ｐ SECTION.
009040*
009050     IF ( 通知情報Ｐ NOT = "00" )
009060         DISPLAY NC"帳票エラー"              UPON CONS
009070         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
009080         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
009090         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
009100         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
009110                                             UPON CONS
009120         CALL "actcshm"  WITH C LINKAGE
009130         ACCEPT  キー入力 FROM CONS
009140         PERFORM ファイル閉鎖
009150         EXIT PROGRAM
009160     END-IF.
009170*================================================================*
009180******************************************************************
009190 END PROGRAM YAW9223.
009200******************************************************************
