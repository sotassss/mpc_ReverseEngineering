000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAZ582.
000060 AUTHOR.                 池田 幸子
000070*
000080*----------------------------------------------------------------*
000090*         提出用並べ順リスト（柔+ｳｨﾝﾄﾞｳｽﾞ版）
000100*  請求年月バージョン
000110*         MED = YAZ582P
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2012-05-10
000140 DATE-COMPILED.          2012-05-10
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
000270     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000280                             ORGANIZATION             IS  INDEXED
000290                             ACCESS MODE              IS  DYNAMIC
000300                             RECORD KEY               IS  元－元号区分
000310                             FILE STATUS              IS  状態キー
000320                             LOCK        MODE         IS  AUTOMATIC.
000330     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000340                             ORGANIZATION             IS  INDEXED
000350                             ACCESS MODE              IS  DYNAMIC
000360                             RECORD KEY               IS  名－区分コード
000370                                                          名－名称コード
000380                             FILE STATUS              IS  状態キー
000390                             LOCK        MODE         IS  AUTOMATIC.
000450     SELECT  保険者マスタ    ASSIGN      TO        HOKENSL
000460                             ORGANIZATION             IS  INDEXED
000470                             ACCESS MODE              IS  DYNAMIC
000480                             RECORD KEY               IS  保－保険種別
000490                                                          保－保険者番号
000500                             ALTERNATE RECORD KEY     IS  保－保険種別
000510                                                          保－保険者名称
000520                                                          保－保険者番号
000530                             FILE STATUS              IS  状態キー
000540                             LOCK        MODE         IS  AUTOMATIC.
000450     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
000460                             ORGANIZATION             IS  INDEXED
000470                             ACCESS MODE              IS  DYNAMIC
000480                             RECORD KEY               IS  市－公費種別
000490                                                          市－市町村番号
000500                             ALTERNATE RECORD KEY     IS  市－公費種別
000510                                                          市－市町村名称
000520                                                          市－市町村番号
000530                             FILE STATUS              IS  状態キー
000540                             LOCK        MODE         IS  AUTOMATIC.
000400     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000410                             ORGANIZATION             IS  INDEXED
000420                             ACCESS MODE              IS  DYNAMIC
000430                             RECORD KEY               IS  制－制御区分
000440                             FILE STATUS              IS  状態キー
000450                             LOCK        MODE         IS  AUTOMATIC.
000330     SELECT  施術所情報マスタ ASSIGN     TO        SEJOHOL
000340                             ORGANIZATION             IS  INDEXED
000350                             ACCESS MODE              IS  DYNAMIC
000360                             RECORD KEY               IS  施情－施術所番号
000370                             FILE STATUS              IS  状態キー
000380                             LOCK        MODE         IS  AUTOMATIC.
000130     SELECT  レセプトＦ      ASSIGN      TO        RECEPTL
000140                             ORGANIZATION             IS  INDEXED
000150                             ACCESS MODE              IS  DYNAMIC
000160                             RECORD KEY               IS  レセ－施術和暦年月
000170                                                          レセ－患者コード
000180                                                          レセ－レセ種別
000190                             ALTERNATE RECORD KEY     IS  レセ－患者コード
000200                                                          レセ－施術和暦年月
000210                                                          レセ－レセ種別
000220                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
000230                                                          レセ－施術和暦年月
000240                                                          レセ－患者コード
000250                                                          レセ－レセ種別
000260                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
000270                                                          レセ－レセ種別
000280                                                          レセ－請求保険者番号
000290                                                          レセ－患者コード
000300                                                          レセ－施術和暦年月
000310                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
000320                                                          レセ－請求保険者番号
000330                                                          レセ－患者コード
000340                                                          レセ－レセ種別
000350                                                          レセ－施術和暦年月
000360                             FILE STATUS              IS  状態キー
000370                             LOCK        MODE         IS  AUTOMATIC.
000241     SELECT  生保情報Ｆ      ASSIGN      TO        SEIHOJL
000242                             ORGANIZATION           IS INDEXED
000243                             ACCESS MODE            IS DYNAMIC
000244                             RECORD KEY             IS 生保－施術和暦年月
000245                                                       生保－患者コード
000255                             ALTERNATE RECORD KEY   IS 生保－患者コード
000265                                                       生保－施術和暦年月
000277                             FILE STATUS            IS 状態キー
000278                             LOCK        MODE       IS AUTOMATIC.
000241     SELECT  労災情報Ｆ      ASSIGN      TO        ROUSAIJL
000242                             ORGANIZATION             IS INDEXED
000243                             ACCESS MODE              IS DYNAMIC
000244                             RECORD KEY               IS 労災－施術和暦年月
000245                                                         労災－患者コード
000255                             ALTERNATE RECORD KEY     IS 労災－患者コード
000265                                                         労災－施術和暦年月
000277                             FILE STATUS              IS 状態キー
000278                             LOCK        MODE         IS AUTOMATIC.
001130     SELECT  作業ファイル１  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5811L.DAT"
001140                             ORGANIZATION             IS  INDEXED
001150                             ACCESS                   IS  DYNAMIC
001160                             RECORD      KEY          IS  作１－印刷順序
001180                                                          作１－保番
001170                                                          作１－種別
001190                                                          作１－保険者番号
001170                                                          作１－本人家族区分
001200                                                          作１－患者コード
001210                                                          作１－施術和暦年月
000360                             FILE STATUS              IS  状態キー
000370                             LOCK        MODE         IS  AUTOMATIC.
000630     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF001
000640                             SYMBOLIC    DESTINATION  IS "PRT"
000650                             FORMAT                   IS  定義体名Ｐ
000660                             GROUP                    IS  項目群名Ｐ
000670                             PROCESSING  MODE         IS  処理種別Ｐ
000680                             UNIT        CONTROL      IS  拡張制御Ｐ
000690                             FILE        STATUS       IS  通知情報Ｐ.
000700******************************************************************
000710*                      DATA DIVISION                             *
000720******************************************************************
000730 DATA                    DIVISION.
000740 FILE                    SECTION.
000750*                           ［ＲＬ＝  １２８］
000760 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
000770     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
000780*                           ［ＲＬ＝  １２８］
000790 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
000800     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
001000*                           ［ＲＬ＝  ３２０］
001010 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001020     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
000970*                           ［ＲＬ＝  ２５６］
000980 FD  市町村マスタ          BLOCK   CONTAINS   1   RECORDS.
000990     COPY SITYOSN        OF  XFDLIB  JOINING   市   AS  PREFIX.
000810*                           ［ＲＬ＝  ２５６］
000820 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
000830     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
000970*                           ［ＲＬ＝  ６４０］
000980 FD  施術所情報マスタ    BLOCK   CONTAINS   1   RECORDS.
000990     COPY SEJOHO         OF  XFDLIB  JOINING    施情 AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
001080* 
000280 FD  生保情報Ｆ          BLOCK   CONTAINS   1   RECORDS.
000281     COPY SEIHOJ          OF  XFDLIB  JOINING   生保   AS  PREFIX.
001510*
000280 FD  労災情報Ｆ          BLOCK   CONTAINS   1   RECORDS.
000281     COPY ROUSAIJ         OF  XFDLIB  JOINING   労災   AS  PREFIX.
000870*
000880 FD  印刷ファイル.
000890     COPY YAZ582P       OF  XMDLIB.
000900*
001520 FD  作業ファイル１ RECORD  CONTAINS 176 CHARACTERS.
001530 01  作１－レコード.
001540     03  作１－レコードキー.
001590         05  作１－印刷順序.
                   07  作１－順番                PIC 9(2).
001570             07  作１－県                  PIC 9(2).
001580             07  作１－保種                PIC 9(1).
001720         05  作１－保番                    PIC X(6).
001600         05  作１－種別                    PIC X(2).
001710         05  作１－保険者番号              PIC X(10).
001700         05  作１－本人家族区分            PIC 9(1).
001620         05  作１－患者コード.
001630             07  作１－患者番号            PIC 9(6).
001640             07  作１－枝番                PIC X(1).
001650         05  作１－施術和暦年月.
001660             07  作１－施術和暦            PIC 9(1).
001670             07  作１－施術年              PIC 9(2).
001680             07  作１－施術月              PIC 9(2).
001690     03  作２－レコードデータ.
001600         05  作１－保険種別                PIC 9(2).
001600         05  作１－助成種別                PIC 9(2).
001740         05  作１－患者氏名                PIC X(50).
001750         05  作１－被保険者氏名            PIC X(50).
001880         05  作１－実日数                  PIC 9(2).
001790         05  作１－費用額                  PIC 9(7).
001800         05  作１－負担額                  PIC 9(7).
001810         05  作１－請求額                  PIC 9(7).
002360         05  作１－前期区分                PIC 9(1).
002370         05  FILLER                        PIC X(12).
001770*
001780*----------------------------------------------------------------*
001790******************************************************************
001800*                WORKING-STORAGE SECTION                         *
001810******************************************************************
001820 WORKING-STORAGE         SECTION.
001830 01 キー入力                           PIC X    VALUE SPACE.
001840 01 状態キー                           PIC X(2) VALUE SPACE.
001850 01 終了フラグ                         PIC X(3) VALUE SPACE.
001860 01 終了フラグ２                       PIC X(3) VALUE SPACE.
001870 01 終了フラグ３                       PIC X(3) VALUE SPACE.
001880 01 書込フラグ                         PIC X(4) VALUE SPACE.
001890 01 作業フラグ                         PIC X(3) VALUE SPACE.
001900 01 作業移動キー                       PIC X(4) VALUE SPACE.
001910 01 終了行フラグ                       PIC X(3) VALUE SPACE.
001920 01 ファイル名                         PIC N(2).
002000 01 保険種別Ｗ                         PIC 9(2) VALUE ZERO.
002000 01 県Ｗ                               PIC 9(3) VALUE ZERO.
002560 01 保険者番号Ｗ                       PIC X(10) VALUE SPACE.
002010 01 印刷順序Ｗ                         PIC 9(2) VALUE ZERO.
001210 01 オープンフラグ                     PIC X(3)   VALUE SPACE.
002040*
002050 01 行カウンタ                         PIC 9(2) VALUE 0.
002060 01 頁カウンタ                         PIC 9(4) VALUE 0.
002070 01 最大行数                           PIC 9(2) VALUE 0.
002080 01 ヘッダ行数                         PIC 9(2) VALUE 0.
002090 01 処理移動キー                       PIC X(4) VALUE SPACE.
002100 01 カレント元号Ｗ                     PIC 9(1) VALUE ZERO.
002110*
002330 01 会員番号Ｗ                         PIC X(10) VALUE SPACE.
001630 01 接骨院名Ｗ                         PIC X(50) VALUE SPACE.
001640 01 代表者名Ｗ                         PIC X(50) VALUE SPACE.
002120 01 合計集計Ｗ.
002130    03 件数合計Ｗ                      PIC 9(4) VALUE ZERO.
002170    03 請求額合計Ｗ                    PIC 9(8) VALUE ZERO.
002130    03 県件数合計Ｗ                    PIC 9(4) VALUE ZERO.
002170    03 県請求額合計Ｗ                  PIC 9(8) VALUE ZERO.
      *
       01 県名Ｗ                             PIC N(5) VALUE SPACE.
       01 県名ＷＲ                           PIC N(3) VALUE SPACE.
002610 01 請求先名称Ｗ.
002620    03 印刷請求先名称Ｗ                PIC X(40) VALUE SPACE.
002300*
002320 01  助成名称Ｗ                        PIC N(1) VALUE SPACE.
002480*
002490 01 請求和暦年月Ｗ.
002500     03 請求和暦Ｗ                     PIC 9.
002510     03 請求年月Ｗ.
002520        05 請求年Ｗ                    PIC 9(2).
002530        05 請求月Ｗ                    PIC 9(2).
002540*
002550 01 印刷施術年月Ｗ.
002560    03 印刷元号略Ｗ                    PIC X(1) VALUE SPACE.
002560    03 印刷施術年Ｗ                    PIC X(2) VALUE SPACE.
002570    03 印刷区切Ｗ                      PIC X    VALUE "/".
002580    03 印刷施術月Ｗ                    PIC X(2) VALUE SPACE.
002610*
002620* エラーメッセージ用
002630 01 エラーメッセージＷ.
002640    03 エラー患者コードＷ              PIC X(7) VALUE SPACE.
002650    03 エラー区切りＷ                  PIC X(1) VALUE SPACE.
002660    03 エラー部位コードＷ              PIC X(7) VALUE SPACE.
002670    03 FILLER                          PIC X(5) VALUE SPACE.
002680*
002690******
002700 01 印刷制御.
002710     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
002720     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
002730     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
002740     03 拡張制御Ｐ.
002750         05 端末制御Ｐ.
002760             07 移動方向Ｐ             PIC X(1).
002770             07 移動行数Ｐ             PIC 9(3).
002780         05 詳細制御Ｐ                 PIC X(2).
002790     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
002800     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
002810*
002980******************************************************************
002990*                          連結項目                              *
003000******************************************************************
003010*
003020********************
003030* メッセージ表示キー *
003040********************
003050 01 連メ－キー IS EXTERNAL.
003060    03  連メ－メッセージ               PIC N(20).
003070 01 連メ３－キー IS EXTERNAL.
003080    03  連メ３－メッセージ             PIC N(20).
003090    03  連メ３－メッセージ１           PIC X(20).
003100*
003910 01 連入－画面情報ＹＡＺ５８０   IS EXTERNAL.
003920    03 連入－請求和暦年月.
003930       05 連入－請求和暦               PIC 9.
003940       05 連入－請求年                 PIC 9(2).
003950       05 連入－請求月                 PIC 9(2).
          03 連入－プレビュー区分            PIC 9(1).
      *
       01 連印－印刷情報ＹＡＺ５８０ IS EXTERNAL GLOBAL.
          03 連印－提出和暦年月.
             05 連印－提出和暦               PIC 9.
             05 連印－提出年                 PIC 9(2).
             05 連印－提出月                 PIC 9(2).
004320*
003910 01 連印－印刷情報ＹＡＺ５８１   IS EXTERNAL.
003930    03 連印－国保件数               PIC 9(4).
003940    03 連印－国保請求額             PIC 9(8).
003930    03 連印－退職件数               PIC 9(4).
003940    03 連印－退職請求額             PIC 9(8).
003930    03 連印－老人件数               PIC 9(4).
003940    03 連印－老人請求額             PIC 9(8).
003930    03 連印－後高件数               PIC 9(4).
003940    03 連印－後高請求額             PIC 9(8).
003930    03 連印－国組件数               PIC 9(4).
003940    03 連印－国組請求額             PIC 9(8).
003930    03 連印－国保合計件数           PIC 9(4).
003940    03 連印－国保合計請求額         PIC 9(8).
003930    03 連印－社保件数               PIC 9(4).
003940    03 連印－社保請求額             PIC 9(8).
003930    03 連印－協会件数               PIC 9(4).
003940    03 連印－協会請求額             PIC 9(8).
003930    03 連印－船員件数               PIC 9(4).
003940    03 連印－船員請求額             PIC 9(8).
003930    03 連印－組合件数               PIC 9(4).
003940    03 連印－組合請求額             PIC 9(8).
003930    03 連印－自衛件数               PIC 9(4).
003940    03 連印－自衛請求額             PIC 9(8).
003930    03 連印－共済件数               PIC 9(4).
003940    03 連印－共済請求額             PIC 9(8).
003930    03 連印－被爆件数               PIC 9(4).
003940    03 連印－被爆請求額             PIC 9(8).
003930    03 連印－障害件数               PIC 9(4).
003940    03 連印－障害請求額             PIC 9(8).
003930    03 連印－母子件数               PIC 9(4).
003940    03 連印－母子請求額             PIC 9(8).
003930    03 連印－乳児件数               PIC 9(4).
003940    03 連印－乳児請求額             PIC 9(8).
003930    03 連印－福祉件数               PIC 9(4).
003940    03 連印－福祉請求額             PIC 9(8).
003930    03 連印－生保件数               PIC 9(4).
003940    03 連印－生保請求額             PIC 9(8).
003930    03 連印－労災件数               PIC 9(4).
003940    03 連印－労災請求額             PIC 9(8).
003930    03 連印－自賠件数               PIC 9(4).
003940    03 連印－自賠請求額             PIC 9(8).
003930    03 連印－自費件数               PIC 9(4).
003940    03 連印－自費請求額             PIC 9(8).
003930    03 連印－合計件数               PIC 9(5).
003940    03 連印－合計請求額             PIC 9(9).
003260*
000540************************************
000550* プリンタファイル作成用           *
000560************************************
000570 01 Ｈ連ＰＲＴＦ－作成データ IS EXTERNAL.
000580     03 Ｈ連ＰＲＴＦ－ファイル名           PIC X(8).
000590     03 Ｈ連ＰＲＴＦ－プレビュー区分       PIC 9.
000600     03 Ｈ連ＰＲＴＦ－帳票プログラム名     PIC X(8).
000610     03 Ｈ連ＰＲＴＦ－オーバレイ名         PIC X(8).
002980*
003270******************************************************************
003280*                      PROCEDURE  DIVISION                       *
003290******************************************************************
003300 PROCEDURE               DIVISION.
003310************
003320*           *
003330* 初期処理   *
003340*           *
003350************
002560     MOVE SPACE TO オープンフラグ.
002570     PERFORM プリンタファイル作成.
003360     PERFORM 初期化.
003370************
003380*           *
003390* 主処理     *
003400*           *
003410************
002790     PERFORM 施術所情報マスタ読込.
003420     PERFORM 印刷処理.
003430************
003440*           *
003450* 終了処理   *
003460*           *
003470************
003480     PERFORM 終了処理.
003490     EXIT PROGRAM.
003500*
003510*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002971     MOVE "PRTF001"             TO Ｈ連ＰＲＴＦ－ファイル名.
002972*
002973*   使用する帳票プログラム名セット
002974     MOVE "YAZ582"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分 TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
003520*================================================================*
003530 初期化 SECTION.
003540*
004080     OPEN INPUT  作業ファイル１.
004090         MOVE NC"作業" TO ファイル名.
004100         PERFORM オープンチェック.
004110     OPEN INPUT  元号マスタ
004120         MOVE NC"元号" TO ファイル名.
004130         PERFORM オープンチェック.
004140     OPEN INPUT  名称マスタ
004150         MOVE NC"名称" TO ファイル名.
004160         PERFORM オープンチェック.
004180     OPEN INPUT 保険者マスタ.
004190         MOVE NC"保険者マスタ" TO ファイル名.
004200         PERFORM オープンチェック.
003410     OPEN INPUT 市町村マスタ.
003420         MOVE NC"市町村マスタ" TO ファイル名.
003430         PERFORM オープンチェック.
004170     OPEN INPUT  制御情報マスタ
004180         MOVE NC"制御情報" TO ファイル名.
004190         PERFORM オープンチェック.
002960     OPEN INPUT 施術所情報マスタ.
002970         MOVE NC"施情" TO ファイル名.
002980         PERFORM オープンチェック.
006630     OPEN INPUT レセプトＦ.
006640         MOVE NC"レセ" TO ファイル名.
006650         PERFORM オープンチェック.
006630     OPEN INPUT 生保情報Ｆ.
006640         MOVE NC"生保" TO ファイル名.
006650         PERFORM オープンチェック.
006630     OPEN INPUT 労災情報Ｆ.
006640         MOVE NC"労災" TO ファイル名.
006650         PERFORM オープンチェック.
004250*================================================================*
004260 オープンチェック SECTION.
004270*
004280     IF 状態キー  NOT =  "00"
004290         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
004300         DISPLAY NC"状態キー：" 状態キー         UPON CONS
004310         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
004320                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
004330         ACCEPT  キー入力 FROM CONS
004340         PERFORM ファイル閉鎖
004350         EXIT PROGRAM.
004360*================================================================*
004370 ファイル閉鎖 SECTION.
004380*
003570     IF ( オープンフラグ = "YES" )
003580         CLOSE 印刷ファイル
003590     ELSE
003600         MOVE  NC"　　データが０件です。確認して下さい。" TO 連メ－メッセージ
003610         CALL   "MSG001"
003620         CANCEL "MSG001"
003630     END-IF.
003640*
004390     CLOSE 作業ファイル１  制御情報マスタ  名称マスタ  施術所情報マスタ
                 保険者マスタ    市町村マスタ    元号マスタ
                 レセプトＦ      生保情報Ｆ      労災情報Ｆ.
004410*================================================================*
004420 終了処理 SECTION.
004430*
004440     PERFORM ファイル閉鎖.
004450*================================================================*
004460 エラー表示 SECTION.
004470*
004480     DISPLAY NC"状態キー" 状態キー  UPON CONS.
004490     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
004500     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
004510     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
004520     ACCEPT  キー入力 FROM CONS.
004530     PERFORM ファイル閉鎖.
004540     EXIT PROGRAM.
005790*================================================================*
005800 エラー表示Ｒ SECTION.
005810*
005820     DISPLAY NC"ファイル読込エラー" ファイル名 UPON CONS.
005830     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
005840     ACCEPT  キー入力 FROM CONS.
005850     PERFORM ファイル閉鎖.
005860     EXIT PROGRAM.
004550*================================================================*
004560 印刷処理 SECTION.
004570*
004580     MOVE 45    TO 最大行数.
004590     MOVE 2     TO ヘッダ行数.
004600*
004610     MOVE SPACE TO 終了フラグ.
004620*
004630     MOVE ZERO  TO 行カウンタ.
004640     MOVE ZERO  TO 頁カウンタ.
004660     PERFORM 合計値初期化.
004660     PERFORM 県合計値初期化.
004670     PERFORM 作業ファイル１読込.
004680     IF  終了フラグ = "YES"
004690         MOVE  NC"　該当するデータがありません。" TO 連メ－メッセージ
004700         CALL   "MSG001"
004710         CANCEL "MSG001"
004720         PERFORM ファイル閉鎖
004730         MOVE 99 TO PROGRAM-STATUS
004740         EXIT PROGRAM
004750     END-IF.
004840*
004850     MOVE 1     TO 頁カウンタ
004860     PERFORM ヘッダ印刷処理
004870*
004880     PERFORM UNTIL 終了フラグ = "YES"
004890        IF ( 行カウンタ >= 最大行数 )
004970           PERFORM 改頁処理
004980           COMPUTE 頁カウンタ = 頁カウンタ + 1
004990           PERFORM ヘッダ印刷処理
005110        END-IF
004830        MOVE 作１－順番       TO 印刷順序Ｗ
005120        PERFORM 明細ヘッダ印刷処理
004890        IF ( 行カウンタ >= 最大行数 )
004970           PERFORM 改頁処理
004980           COMPUTE 頁カウンタ = 頁カウンタ + 1
004990           PERFORM ヘッダ印刷処理
005110        END-IF
004880        PERFORM UNTIL ( 終了フラグ = "YES" ) OR
                            ( 作１－順番 NOT = 印刷順序Ｗ)
004830           MOVE 作１－保険者番号 TO 保険者番号Ｗ
005120           PERFORM 明細１印刷処理
004890           IF ( 行カウンタ >= 最大行数 )
004970              PERFORM 改頁処理
004980              COMPUTE 頁カウンタ = 頁カウンタ + 1
004990              PERFORM ヘッダ印刷処理
005110           END-IF
004880           PERFORM UNTIL ( 終了フラグ = "YES" ) OR
                               ( 作１－保険者番号 NOT = 保険者番号Ｗ)
004890              IF ( 行カウンタ >= 最大行数 )
004970                 PERFORM 改頁処理
004980                 COMPUTE 頁カウンタ = 頁カウンタ + 1
004990                 PERFORM ヘッダ印刷処理
005110              END-IF
005120              PERFORM 明細２印刷処理
005130              PERFORM 合計額累計
005340*
005380              MOVE 作１－順番       TO 印刷順序Ｗ
005380              MOVE 作１－県         TO 県Ｗ
004830              MOVE 作１－保険者番号 TO 保険者番号Ｗ
005390*
005400              PERFORM 作業ファイル１読込
                 END-PERFORM
005630           PERFORM 合計印刷処理
004660           PERFORM 合計値初期化
                 IF ( 印刷順序Ｗ = 1 ) AND
                    (( 県Ｗ NOT = 作１－県 ) OR (印刷順序Ｗ NOT = 作１－順番))
005630              PERFORM 県合計印刷処理
004660              PERFORM 県合計値初期化
004890              IF ( 行カウンタ >= 最大行数 )
004970                 PERFORM 改頁処理
004980                 COMPUTE 頁カウンタ = 頁カウンタ + 1
004990                 PERFORM ヘッダ印刷処理
005110              END-IF
005380              MOVE 作１－県         TO 県Ｗ
                    IF ( 作１－順番 = 1 )
005120                 PERFORM 明細ヘッダ印刷処理
004890                 IF ( 行カウンタ >= 最大行数 )
004970                    PERFORM 改頁処理
004980                    COMPUTE 頁カウンタ = 頁カウンタ + 1
004990                    PERFORM ヘッダ印刷処理
                       END-IF
005110              END-IF
                 END-IF
005600*
005610        END-PERFORM
005630        PERFORM 国保合計印刷処理
005800     END-PERFORM.
005850*
005870*================================================================*
005880 改頁処理  SECTION.
005890*
005900     MOVE SPACE TO YAZ582P.
005910     INITIALIZE YAZ582P.
005920     MOVE "YAZ582P" TO  定義体名Ｐ.
005930     MOVE "CT"       TO  処理種別Ｐ.
005940     MOVE "PAGE"     TO  拡張制御Ｐ.
005950     MOVE SPACE      TO  項目群名Ｐ.
005960     WRITE YAZ582P.
005970     PERFORM エラー処理Ｐ.
005980     MOVE SPACE      TO  拡張制御Ｐ.
006030*
006040*================================================================*
006050 ヘッダ印刷処理 SECTION.
006060*
006120     MOVE SPACE TO YAZ582P.
006130     INITIALIZE YAZ582P.
006260     MOVE 頁カウンタ     TO 頁.
006270*
006280* 対象請求和暦を取得
006290     MOVE 連印－提出和暦  TO 元－元号区分.
006300     READ 元号マスタ
006310     INVALID KEY
006320         MOVE SPACE         TO 請求和暦名称
006330     NOT INVALID KEY
006340         MOVE 元－元号名称  TO 請求和暦名称
006350     END-READ.
006360     MOVE 連印－提出年      TO 請求年.
006370     MOVE 連印－提出月      TO 請求月.
006380*
006390     MOVE NC"請求分"        TO 請求表題.
006400*
           MOVE 接骨院名Ｗ        TO 接骨院名.
           MOVE 会員番号Ｗ        TO 会員番号.
006080     PERFORM ヘッダ印字１.
006420*
006430     IF 頁カウンタ = 1
007330         MOVE SPACE TO YAZ582P
007340         INITIALIZE YAZ582P
 06440         MOVE 連印－合計件数    TO 全件数
006450         MOVE 連印－合計請求額  TO 全請求額
               MOVE 代表者名Ｗ        TO 施術者名
006080         PERFORM ヘッダ印字２
006460     END-IF.
007320*
007330     MOVE SPACE TO YAZ582P.
007340     INITIALIZE YAZ582P.
006080     PERFORM ヘッダ印字３.
006510*
003230*================================================================*
003240 施術所情報マスタ読込 SECTION.
003250*
003260     MOVE ZERO TO 施情－施術所番号.
003270     READ 施術所情報マスタ
003280     INVALID KEY
003290         MOVE NC"施情" TO ファイル名
003300         PERFORM エラー表示Ｒ
003310         PERFORM ファイル閉鎖
003320         MOVE 99 TO PROGRAM-STATUS
003330         EXIT PROGRAM
003340     NOT INVALID KEY
003400         MOVE 施情－接骨院名         TO 接骨院名Ｗ
003430         MOVE 施情－接骨師会会員番号 TO 会員番号Ｗ
003410         MOVE 施情－代表者名         TO 代表者名Ｗ
003660*
003670     END-READ.
007160*================================================================*
007170 ヘッダ印字１  SECTION.
007180*
006050     IF ( オープンフラグ NOT = "YES" )
006060        MOVE "YES" TO オープンフラグ
006070        OPEN I-O  印刷ファイル
006080        PERFORM エラー処理Ｐ
006090     END-IF.
005410*
007190     MOVE "YAZ582P" TO  定義体名Ｐ.
007200     MOVE SPACE      TO  処理種別Ｐ.
007210     MOVE "HEAD01"   TO  項目群名Ｐ.
007220     WRITE YAZ582P.
007230     PERFORM エラー処理Ｐ.
007240     MOVE ヘッダ行数 TO 行カウンタ.
007160*================================================================*
007170 ヘッダ印字２  SECTION.
007180*
007190     MOVE "YAZ582P" TO  定義体名Ｐ.
007200     MOVE SPACE      TO  処理種別Ｐ.
007210     MOVE "HEAD02"   TO  項目群名Ｐ.
007220     WRITE YAZ582P.
007230     PERFORM エラー処理Ｐ.
008380     COMPUTE 行カウンタ = 行カウンタ + 1.
007160*================================================================*
007170 ヘッダ印字３  SECTION.
007180*
007190     MOVE "YAZ582P" TO  定義体名Ｐ.
007200     MOVE SPACE      TO  処理種別Ｐ.
007210     MOVE "HEAD03"   TO  項目群名Ｐ.
007220     WRITE YAZ582P.
007230     PERFORM エラー処理Ｐ.
008380     COMPUTE 行カウンタ = 行カウンタ + 1.
006520*================================================================*
006530 明細ヘッダ印刷処理 SECTION.
006540*
007280     PERFORM 明細ヘッダセット.
007290     PERFORM 明細ヘッダ印字.
007300*================================================================*
007310 明細ヘッダセット SECTION.
007320*
007330     MOVE SPACE TO YAZ582P.
007340     INITIALIZE YAZ582P.
           MOVE SPACE TO 県名Ｗ.
007320*
013500**********************************************
013510*  汎用印刷順序:保険名称(保険種別コード)     *
013520*    1:国保(1)、退職(8)、後高(5)             *
013530*    2:社保(2)                               *
013540*    3:協会(2)                               *
013550*    4:船員(7)                               *
013560*    5:組合(3)                               *
013570*    6:自衛官(9)                             *
013580*    7:共済(4)                               *
013590*    8:被爆(54)                              *
013590*    9:障害(53)                              *
013590*   10:母子(52)                              *
013590*   11:子供(55,60)                           *
013590*   12:４１老(51)                            *
013590*   13:生保(85)                              *
013590***   14:労災(70)                              *
013590***   15:自賠(80)                              *
013630*                                            *
013640*  ※保険種別 06:日雇は 02:協会へ含める。    *
      *                                            *
      *  ＋　県番号                                *
      *  国保系のみ 1:国退 2:国組 3:後高           *
013650**********************************************
007030*
           EVALUATE 印刷順序Ｗ
           WHEN 1
              MOVE 13                    TO 名－区分コード
              MOVE 作１－県              TO 名－名称コード
              READ 名称マスタ
              INVALID KEY
                  MOVE SPACE             TO 県名ＷＲ
              NOT INVALID KEY
                  MOVE 名－正式名称      TO 県名ＷＲ
              END-READ
              STRING NC"［"         DELIMITED BY SIZE
                     県名ＷＲ       DELIMITED BY SPACE
                     NC"］"         DELIMITED BY SIZE
                INTO 県名Ｗ
              END-STRING
              MOVE 県名Ｗ                TO 県名
           WHEN 2
              MOVE NC"社会保険"          TO 保険種別
              MOVE 連印－社保件数        TO 種別件数
              MOVE 連印－社保請求額      TO 種別請求額
           WHEN 3
              MOVE NC"協会けんぽ"        TO 保険種別
              MOVE 連印－協会件数        TO 種別件数
              MOVE 連印－協会請求額      TO 種別請求額
           WHEN 4
              MOVE NC"船員"              TO 保険種別
              MOVE 連印－船員件数        TO 種別件数
              MOVE 連印－船員請求額      TO 種別請求額
           WHEN 5
              MOVE NC"健康保険組合"      TO 保険種別
              MOVE 連印－組合件数        TO 種別件数
              MOVE 連印－組合請求額      TO 種別請求額
           WHEN 6
              MOVE NC"自衛官"            TO 保険種別
              MOVE 連印－自衛件数        TO 種別件数
              MOVE 連印－自衛請求額      TO 種別請求額
           WHEN 7
              MOVE NC"共済保険"          TO 保険種別
              MOVE 連印－共済件数        TO 種別件数
              MOVE 連印－共済請求額      TO 種別請求額
           WHEN 8
              MOVE NC"福祉（原爆）"      TO 保険種別
              MOVE 連印－被爆件数        TO 種別件数
              MOVE 連印－被爆請求額      TO 種別請求額
           WHEN 9
              MOVE NC"福祉（障）"        TO 保険種別
              MOVE 連印－障害件数        TO 種別件数
              MOVE 連印－障害請求額      TO 種別請求額
           WHEN 10
              MOVE NC"福祉（親）"        TO 保険種別
              MOVE 連印－母子件数        TO 種別件数
              MOVE 連印－母子請求額      TO 種別請求額
           WHEN 11
              MOVE NC"福祉（子）"        TO 保険種別
              MOVE 連印－乳児件数        TO 種別件数
              MOVE 連印－乳児請求額      TO 種別請求額
           WHEN 12
              MOVE NC"福祉（福）"        TO 保険種別
              MOVE 連印－福祉件数        TO 種別件数
              MOVE 連印－福祉請求額      TO 種別請求額
           WHEN 13
              MOVE NC"生活保護"          TO 保険種別
              MOVE 連印－生保件数        TO 種別件数
              MOVE 連印－生保請求額      TO 種別請求額
      *     WHEN 14
      *        MOVE NC"労災"              TO 保険種別
      *        MOVE 連印－労災件数        TO 種別件数
      *        MOVE 連印－労災請求額      TO 種別請求額
      *     WHEN 15
      *        MOVE NC"自賠責"            TO 保険種別
      *        MOVE 連印－自賠件数        TO 種別件数
      *        MOVE 連印－自賠請求額      TO 種別請求額
      *     WHEN 16
      *        MOVE NC"自費"              TO 保険種別
      *        MOVE 連印－自費件数        TO 種別件数
      *        MOVE 連印－自費請求額      TO 種別請求額
           END-EVALUATE.
007150*
008310*================================================================*
008320 明細ヘッダ印字  SECTION.
008330*
           IF 印刷順序Ｗ = 1
008350        MOVE "GRP006"    TO  項目群名Ｐ
           ELSE
008350        MOVE "GRP001"    TO  項目群名Ｐ
           END-IF.
008340     MOVE "YAZ582P"  TO  定義体名Ｐ.
008360     WRITE YAZ582P.
008370     PERFORM エラー処理Ｐ.
008380     COMPUTE 行カウンタ = 行カウンタ + 1.
007250*================================================================*
007260 明細１印刷処理 SECTION.
007270*
007280     PERFORM 明細１セット.
007290     PERFORM 明細１印字.
007300*================================================================*
007310 明細１セット SECTION.
007320*
007330     MOVE SPACE TO YAZ582P.
007340     INITIALIZE YAZ582P.
007350*
007360     MOVE 作１－保険者番号    TO 保険者番号.
           EVALUATE TRUE
           WHEN (作１－保険種別 = 05)
           WHEN (作１－保険種別 <= 60) AND (作１－保険種別 >= 50)
              PERFORM 市町村情報取得
      *     WHEN 作１－保険種別 = 70 
      *        MOVE 作１－施術和暦年月 TO 労災－施術和暦年月
      *        MOVE 作１－患者コード   TO 労災－患者コード
      *        READ 労災情報Ｆ
      *        NOT INVALID KEY
      *           MOVE 労災－労災事業所名称  TO 請求先名称Ｗ
      *        END-READ
           WHEN 作１－保険種別 = 85 
              MOVE 作１－施術和暦年月 TO 生保－施術和暦年月
              MOVE 作１－患者コード   TO 生保－患者コード
              READ 生保情報Ｆ
              NOT INVALID KEY
                 MOVE 生保－生保市町村名  TO 請求先名称Ｗ
              END-READ
           WHEN OTHER
              PERFORM 請求先情報取得
           END-EVALUATE.
           MOVE 請求先名称Ｗ        TO 保険者名称.
008080*
008310*================================================================*
008320 明細１印字  SECTION.
008330*
008340     MOVE "YAZ582P"  TO  定義体名Ｐ.
008350     MOVE "GRP002"    TO  項目群名Ｐ.
008360     WRITE YAZ582P.
008370     PERFORM エラー処理Ｐ.
008380     COMPUTE 行カウンタ = 行カウンタ + 1.
008390*================================================================*
008400 明細２印刷処理 SECTION.
008410*
008420     PERFORM 明細２セット.
008430     PERFORM 明細２印字.
008440*================================================================*
008450 明細２セット SECTION.
008460*
007330     MOVE SPACE TO YAZ582P.
007340     INITIALIZE YAZ582P.
007350*
007370     MOVE 作１－患者コード    TO 患者コード.
007380     MOVE 作１－患者氏名      TO 患者氏名.
           IF ( 作１－保険種別 NOT = 05 ) AND ( 作１－保険種別 NOT = 09 ) AND
              ( 作１－保険種別 < 50 )
007390        IF 作１－本人家族区分 = 1
007400            MOVE NC"（本人）"    TO 本人家族区分
007410        ELSE
007400            MOVE NC"（家族）"    TO 本人家族区分
007430        END-IF
007430     END-IF.
007390     IF 作１－本人家族区分 NOT = 1
007420         MOVE 作１－被保険者氏名  TO 被保険者氏名
007430     END-IF.
007530**
      *     IF 作１－施術和暦年月 NOT = ZERO
006290        MOVE 作１－施術和暦   TO 元－元号区分
006300        READ 元号マスタ
006310        INVALID KEY
006320            MOVE SPACE        TO 印刷元号略Ｗ
006330        NOT INVALID KEY
006340            MOVE 元－元号略   TO 印刷元号略Ｗ
006350        END-READ
              IF 作１－施術和暦 = 4
                 MOVE "H"           TO 印刷元号略Ｗ
              END-IF
              MOVE 作１－施術年     TO 印刷施術年Ｗ
              MOVE 作１－施術月     TO 印刷施術月Ｗ
              MOVE 印刷施術年月Ｗ   TO 施術年月
      *     END-IF.
007560     MOVE 作１－実日数        TO 実日数.
007560     MOVE 作１－費用額        TO 費用額.
007570     MOVE 作１－負担額        TO 負担額.
007580     MOVE 作１－請求額        TO 請求額.
007580     MOVE 作１－前期区分      TO 前期区分.
           IF ( 作１－保険種別 < 50 )
              EVALUATE 作１－助成種別
              WHEN 52
                 MOVE NC"親"        TO 助成
              WHEN 53
                 MOVE NC"障"        TO 助成
              WHEN 54
                 MOVE NC"原"        TO 助成
              WHEN 55
              WHEN 60
                 MOVE NC"子"        TO 助成
              END-EVALUATE
           END-IF.
008680*
008920*================================================================*
008930 明細２印字  SECTION.
008940*
008950     MOVE "YAZ582P"  TO  定義体名Ｐ.
008960     MOVE "GRP003"    TO  項目群名Ｐ.
008970     WRITE YAZ582P.
008980     PERFORM エラー処理Ｐ.
008990     COMPUTE 行カウンタ = 行カウンタ + 1.
010110*================================================================*
010120 作業ファイル１読込 SECTION.
010130*
010140     READ 作業ファイル１ NEXT
010150     AT END
010160         MOVE "YES" TO 終了フラグ
010170     END-READ.
010180*
010190*================================================================*
010200 合計額累計 SECTION.
010210*
010220     COMPUTE 件数合計Ｗ     = 件数合計Ｗ   + 1.
010260     COMPUTE 請求額合計Ｗ   = 請求額合計Ｗ + 作１－請求額.
010210*
010220     COMPUTE 県件数合計Ｗ   = 県件数合計Ｗ   + 1.
010260     COMPUTE 県請求額合計Ｗ = 県請求額合計Ｗ + 作１－請求額.
010430*
010440*================================================================*
010450 合計印刷処理 SECTION.
010460*
010520     MOVE SPACE TO YAZ582P.
010530     INITIALIZE YAZ582P.
010540*
           IF 件数合計Ｗ > 1
              MOVE NC"合計"             TO 合計ＣＭ
010550        MOVE 件数合計Ｗ           TO 件数合計
010590        MOVE 請求額合計Ｗ         TO 請求額合計
010480        PERFORM 合計印字
004890        IF ( 行カウンタ >= 最大行数 )
004970           PERFORM 改頁処理
004980           COMPUTE 頁カウンタ = 頁カウンタ + 1
004990           PERFORM ヘッダ印刷処理
005110        END-IF
           END-IF.
010600*================================================================*
010610 合計印字 SECTION.
010620*
010630     MOVE "YAZ582P"  TO  定義体名Ｐ.
010640     MOVE "GRP004"    TO  項目群名Ｐ.
010650     WRITE YAZ582P.
010660     PERFORM エラー処理Ｐ.
010670     COMPUTE 行カウンタ = 行カウンタ + 1.
010440*================================================================*
010450 県合計印刷処理 SECTION.
010460*
010520     MOVE SPACE TO YAZ582P.
010530     INITIALIZE YAZ582P.
010540*
           MOVE NC"都道府県　総合計" TO 合計ＣＭ.
010550     MOVE 県件数合計Ｗ         TO 件数合計.
010590     MOVE 県請求額合計Ｗ       TO 請求額合計.
010480     PERFORM 県合計印字.
010600*================================================================*
010610 県合計印字 SECTION.
010620*
010630     MOVE "YAZ582P"  TO  定義体名Ｐ.
010640     MOVE "GRP004"    TO  項目群名Ｐ.
010650     WRITE YAZ582P.
010660     PERFORM エラー処理Ｐ.
010670     COMPUTE 行カウンタ = 行カウンタ + 1.
010440*================================================================*
010450 国保合計印刷処理 SECTION.
010460*
010520     MOVE SPACE TO YAZ582P.
010530     INITIALIZE YAZ582P.
010540*
           IF 印刷順序Ｗ = 1
004890        IF ( 行カウンタ > 最大行数 - 4 )
004970           PERFORM 改頁処理
004980           COMPUTE 頁カウンタ = 頁カウンタ + 1
004990           PERFORM ヘッダ印刷処理
005110        END-IF
              MOVE 連印－国保件数       TO 国保件数
              MOVE 連印－国保請求額     TO 国保請求額
              MOVE 連印－退職件数       TO 退職件数
              MOVE 連印－退職請求額     TO 退職請求額
              MOVE 連印－老人件数       TO 老人件数
              MOVE 連印－老人請求額     TO 老人請求額
              MOVE 連印－後高件数       TO 後高件数
              MOVE 連印－後高請求額     TO 後高請求額
              MOVE 連印－国組件数       TO 国組件数
              MOVE 連印－国組請求額     TO 国組請求額
              MOVE 連印－国保合計件数   TO 国保合計件数
              MOVE 連印－国保合計請求額 TO 国保合計請求額
010480        PERFORM 国保合計印字
004890        IF ( 行カウンタ >= 最大行数 )
004970           PERFORM 改頁処理
004980           COMPUTE 頁カウンタ = 頁カウンタ + 1
004990           PERFORM ヘッダ印刷処理
005110        END-IF
           END-IF.
010600*================================================================*
010610 国保合計印字 SECTION.
010620*
010630     MOVE "YAZ582P"  TO  定義体名Ｐ.
010640     MOVE "GRP005"    TO  項目群名Ｐ.
010650     WRITE YAZ582P.
010660     PERFORM エラー処理Ｐ.
010670     COMPUTE 行カウンタ = 行カウンタ + 4.
011190*================================================================*
011200 合計値初期化 SECTION.
011210*
011220     MOVE ZERO  TO 件数合計Ｗ.
011260     MOVE ZERO  TO 請求額合計Ｗ.
011190*================================================================*
011200 県合計値初期化 SECTION.
011210*
011220     MOVE ZERO  TO 県件数合計Ｗ.
011260     MOVE ZERO  TO 県請求額合計Ｗ.
006180*================================================================*
006190 請求先情報取得 SECTION.
006200*
006210*********************************************************
006220* 連結データから保険者マスタより請求先を取得する。      *
006230* ● 請求先...... 請求先名称Ｗに格納                    *
006240*********************************************************
006250     MOVE 作１－保険種別      TO 保－保険種別.
006260     MOVE 作１－保険者番号    TO 保－保険者番号.
006270     MOVE SPACE               TO 請求先名称Ｗ.
006280     READ 保険者マスタ
006290     INVALID KEY
006300         MOVE SPACE           TO 請求先名称Ｗ
006310     NOT INVALID KEY
006320*         MOVE 保－保険者名称  TO 請求先名称Ｗ
029020          EVALUATE 保－保険種別 
029130* 組合は支部名まで印字
029140          WHEN  03
029150              STRING 保－保険者名称  DELIMITED BY SPACE
029160                     "健康保険組合"  DELIMITED BY SIZE
029170                     "  "            DELIMITED BY SIZE
029180                     保－支部部署名  DELIMITED BY SPACE
029190                     INTO 請求先名称Ｗ
029200              END-STRING
029210* 共済は支部名まで印字
029220          WHEN  04
                    IF 保－保険者番号 = "34130021"
                        MOVE 保－保険者名称 TO 請求先名称Ｗ
                    ELSE
029230                  STRING 保－保険者名称  DELIMITED BY SPACE
029240                         "共済組合"      DELIMITED BY SIZE
029250                         "  "            DELIMITED BY SIZE
029260                         保－支部部署名  DELIMITED BY SPACE
029270                         INTO 請求先名称Ｗ
029280                  END-STRING
                    END-IF
029290          WHEN OTHER
029300              MOVE 保－保険者名称    TO 請求先名称Ｗ
029310          END-EVALUATE
006330     END-READ.
005040*================================================================*
005050 市町村情報取得 SECTION.
005060*
005070****************************************************
005080* 連結データから保険者マスタより請求先を取得する。 *
005090* ● 請求先...... 請求先名称Ｗに格納               *
005100****************************************************
005110     MOVE 作１－保険種別     TO 市－公費種別.
005120     MOVE 作１－保険者番号   TO 市－市町村番号.
005130     READ 市町村マスタ
005140     INVALID KEY
005150         MOVE SPACE          TO 請求先名称Ｗ
005160     NOT INVALID KEY
005170         MOVE 市－市町村名称 TO 請求先名称Ｗ
005180     END-READ.
013010*================================================================*
013020 エラー処理Ｐ SECTION.
013030*
013040     IF 通知情報Ｐ NOT = "00"
013050         DISPLAY NC"帳票エラー"              UPON CONS
013060         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
013070         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
013080         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
013090         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
013100                                             UPON CONS
004380*-----------------------------------------*
004390         CALL "actcshm"  WITH C LINKAGE
004400*-----------------------------------------*
013110         ACCEPT  キー入力 FROM CONS
013120         PERFORM ファイル閉鎖
013130         EXIT PROGRAM
013140     END-IF.
013150*================================================================*
013160******************************************************************
013170 END PROGRAM YAZ582.
013180******************************************************************
