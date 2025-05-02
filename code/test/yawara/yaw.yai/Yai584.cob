000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAI584.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*  ６号用紙印刷      【柔+ｳｨﾝﾄﾞｳｽﾞ版】
000100*         MED = YAI584P
000101*  請求年月バージョン
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2013-06-27
000130 DATE-COMPILED.          2013-06-27
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
000450     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000460                             ORGANIZATION             IS  INDEXED
000470                             ACCESS MODE              IS  DYNAMIC
000480                             RECORD KEY               IS 施情－施術所番号
000490                             FILE STATUS              IS  状態キー
000500                             LOCK        MODE         IS  AUTOMATIC.
000510     SELECT  保険者マスタ    ASSIGN      TO        HOKENSL
000520                             ORGANIZATION             IS  INDEXED
000530                             ACCESS MODE              IS  DYNAMIC
000540                             RECORD KEY               IS  保－保険種別
000550                                                          保－保険者番号
000560                             ALTERNATE RECORD KEY     IS  保－保険種別
000570                                                          保－保険者名称
000580                                                          保－保険者番号
000590                             FILE STATUS              IS  状態キー
000600                             LOCK        MODE         IS  AUTOMATIC.
000610     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
000620                             ORGANIZATION             IS  INDEXED
000630                             ACCESS MODE              IS  DYNAMIC
000640                             RECORD KEY               IS  市－公費種別
000650                                                          市－市町村番号
000660                             ALTERNATE RECORD KEY     IS  市－公費種別
000670                                                          市－市町村名称
000680                                                          市－市町村番号
000690                             FILE STATUS              IS  状態キー
000700                             LOCK        MODE         IS  AUTOMATIC.
000701     SELECT  請求先マスタ    ASSIGN      TO        SEIKYUSL
000702                             ORGANIZATION             IS  INDEXED
000703                             ACCESS MODE              IS  DYNAMIC
000704                             RECORD KEY               IS  請先－保険種別
000705                                                          請先－保険者番号
000706                             FILE STATUS              IS  状態キー
000707                             LOCK    MODE             IS  AUTOMATIC.
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
000033     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000034                             ORGANIZATION             IS  INDEXED
000035                             ACCESS MODE              IS  DYNAMIC
000036                             RECORD KEY               IS 受－施術和暦年月
000037                                                          受－患者コード
000038                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000039                                                          受－患者カナ
000040                                                          受－患者コード
000041                             ALTERNATE RECORD KEY     IS  受－患者コード
000042                                                         受－施術和暦年月
000043                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000044                                                          受－保険種別
000045                                                          受－保険者番号
000046                                                          受－患者コード
000047                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000048                                                          受－公費種別
000049                                                     受－費用負担者番号
000050                                                          受－患者コード
000051                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000052                                                          受－助成種別
000053                                                  受－費用負担者番号助成
000054                                                          受－患者コード
000055                             ALTERNATE RECORD KEY  IS 受－請求和暦年月
000056                                                      受－施術和暦年月
000057                                                      受－患者コード
000058                             FILE STATUS              IS  状態キー
000059                             LOCK        MODE         IS  AUTOMATIC.
000108     SELECT  作業ファイル２  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5831L.DAT"
000109                             ORGANIZATION             IS  INDEXED
000110                             ACCESS                   IS  DYNAMIC
000111                             RECORD      KEY          IS  作２－保険種別
000113                                                          作２－保険者番号
000114                             ALTERNATE RECORD KEY     IS  作２－印刷順序１
000115                                                          作２－県
000116                                                          作２－印刷順序
000117                                                          作２－保険種別
000118                                                          作２－保険者番号
000119                             FILE        STATUS       IS  状態キー
000120                             LOCK        MODE         IS  AUTOMATIC.
000810     SELECT  印刷ファイル    ASSIGN      TO         GS-PRTF001
000820                             SYMBOLIC    DESTINATION  IS "PRT"
000830                             FORMAT                   IS  定義体名Ｐ
000840                             GROUP                    IS  項目群名Ｐ
000850                             PROCESSING  MODE         IS  処理種別Ｐ
000860                             UNIT        CONTROL      IS  拡張制御Ｐ
000870                             FILE        STATUS       IS  通知情報Ｐ.
000880*
000890******************************************************************
000900*                      DATA DIVISION                             *
000910******************************************************************
000920 DATA                    DIVISION.
000930 FILE                    SECTION.
000940*                           ［ＲＬ＝  １２８］
000950 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
000960     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001030*                           ［ＲＬ＝  １２８］
001040 FD  施術所情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001050     COPY SEJOHO          OF  XFDLIB  JOINING   施情   AS  PREFIX.
001060*                           ［ＲＬ＝  ３２０］
001070 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001080     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
001090*                           ［ＲＬ＝  ２５６］
001100 FD  市町村マスタ          BLOCK   CONTAINS   1   RECORDS.
001110     COPY SITYOSN        OF  XFDLIB  JOINING   市   AS  PREFIX.
001111*                           ［ＲＬ＝  １２８］
001112 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
001113     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
      *
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
000129*                           ［ＲＬ＝  ３２０］
000130 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
000131     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
001123*                           ［ＲＬ＝  １２８］
001124 FD  作業ファイル２ RECORD  CONTAINS 128 CHARACTERS.
001125 01  作２－レコード.
001126     03  作２－レコードキー.
001131         05  作２－保険種別                  PIC 9(2).
001132         05  作２－保険者番号                PIC X(10).
001137     03  作２－レコードデータ.
001133         05  作２－県                        PIC X(2).
001134         05  作２－印刷順序.
001135             07  作２－印刷順序１            PIC 9(2).
001136             07  作２－印刷順序２            PIC 9.
001138         05  作２－件数                      PIC 9(4).
001139         05  作２－費用額                    PIC 9(9).
001140         05  作２－負担額                    PIC 9(9).
001141         05  作２－請求額                    PIC 9(9).
001142         05  作２－本人件数                  PIC 9(3).
001143         05  作２－本人費用額                PIC 9(7).
001144         05  作２－本人負担額                PIC 9(7).
001145         05  作２－本人請求額                PIC 9(7).
001146         05  作２－家族件数                  PIC 9(3).
001147         05  作２－家族費用額                PIC 9(7).
001148         05  作２－家族負担額                PIC 9(7).
001149         05  作２－家族請求額                PIC 9(7).
001150         05  FILLER                          PIC X(32).
001350* 
001360 FD  印刷ファイル.
001370     COPY YAI584P         OF  XMDLIB.
001371*
001380******************************************************************
001390*                WORKING-STORAGE SECTION                         *
001400******************************************************************
001410 WORKING-STORAGE         SECTION.
001420 01 キー入力                           PIC X    VALUE SPACE.
001430 01 状態キー                           PIC X(2) VALUE SPACE.
001440 01 終了フラグ                         PIC X(3) VALUE SPACE.
001450 01 終了フラグ２                       PIC X(3) VALUE SPACE.
001450 01 終了フラグ３                       PIC X(3) VALUE SPACE.
001470 01 ファイル名Ｗ                       PIC N(6) VALUE SPACE.
001490 01 実行キーＷ                         PIC X(4) VALUE SPACE.
001590 01 ファイル名                         PIC N(4) VALUE SPACE.
001630 01 行カウンタ                         PIC 9(2) VALUE ZERO.
001640 01 合計行カウンタ                     PIC 9(3) VALUE ZERO.
001650 01 ページカウンタ                     PIC 9(3) VALUE ZERO.
001651 01 印刷フラグ                         PIC X(3)  VALUE SPACE.
001652 01 スキップフラグ                     PIC X(3)  VALUE SPACE.
001210 01 オープンフラグ                     PIC X(3)   VALUE SPACE.
001660*
001661 01 合計用Ｗ.
001662    03 本人件数カウント                PIC 9(9) VALUE ZERO.
001670    03 家族件数カウント                PIC 9(9) VALUE ZERO.
001680    03 計件数カウント                  PIC 9(9) VALUE ZERO.
001690    03 本人費用額カウント              PIC 9(9) VALUE ZERO.
001700    03 家族費用額カウント              PIC 9(9) VALUE ZERO.
001710    03 計費用額カウント                PIC 9(9) VALUE ZERO.
001720    03 本人請求額カウント              PIC 9(9) VALUE ZERO.
001730    03 家族請求額カウント              PIC 9(9) VALUE ZERO.
001740    03 計請求額カウント                PIC 9(9) VALUE ZERO.
001770 01 県Ｗ                               PIC X(2) VALUE SPACE.
001771 01 印刷順序Ｗ                         PIC 9(2) VALUE ZERO.
001772*
001773 01 保険者名称Ｗ.
001774    03 保険者名称１Ｗ                  PIC X(20) VALUE SPACE.
001775    03 保険者名称２Ｗ                  PIC X(20) VALUE SPACE.
001776    03 保険者名称３Ｗ                  PIC X(20) VALUE SPACE.
001777*
001801 01 全角空白                           PIC X(2)  VALUE X"8140".
001802 01 半角空白                           PIC X(2)  VALUE X"2020".
001920***
001921***
001922 01 画面情報Ｗ.
001923    03 請求年月Ｗ.
001924       05 請求和暦Ｗ                   PIC 9     VALUE ZERO.
001925       05 請求年Ｗ                     PIC 9(2)  VALUE ZERO.
001926       05 請求月Ｗ                     PIC 9(2)  VALUE ZERO.
001927    03 提出年月日Ｗ.
001928       05 提出和暦Ｗ                   PIC 9     VALUE ZERO.
001929       05 提出年Ｗ                     PIC 9(2)  VALUE ZERO.
001930       05 提出月Ｗ                     PIC 9(2)  VALUE ZERO.
001931       05 提出日Ｗ                     PIC 9(2)  VALUE ZERO.
001933*
001934**************
001935* 施術所情報 *
001936**************
001937 01 施術所情報Ｗ.
001938    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
001939    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
001940    03 柔整師番号Ｗ.
             05 柔整師記号Ｗ                 PIC X(2)   VALUE SPACE.
             05 柔整師番号１Ｗ               PIC X(7)   VALUE SPACE.
             05 柔整師区切１Ｗ               PIC X(1)   VALUE SPACE.
             05 柔整師番号２Ｗ               PIC X(1)   VALUE SPACE.
             05 柔整師区切２Ｗ               PIC X(1)   VALUE SPACE.
             05 柔整師番号３Ｗ               PIC X(1)   VALUE SPACE.
001941    03 施術所住所Ｗ.
001942       05 施術所住所１Ｗ               PIC X(40)  VALUE SPACE.
001943       05 施術所住所２Ｗ               PIC X(40)  VALUE SPACE.
001944    03 施術所郵便番号Ｗ.
001945       05 施術所郵便番号１Ｗ           PIC X(3)   VALUE SPACE.
001946       05 施術所郵便番号区切Ｗ         PIC X(1)   VALUE SPACE.
001947       05 施術所郵便番号２Ｗ           PIC X(4)   VALUE SPACE.
001948    03 施術所電話番号Ｗ                PIC X(15)  VALUE SPACE.
001949    03 取引先情報Ｗ.
001950        05 取引先銀行名Ｗ              PIC X(40)  VALUE SPACE.
001951        05 取引先銀行支店名Ｗ          PIC X(40)  VALUE SPACE.
001952        05 預金種別Ｗ                  PIC 9(1)   VALUE ZERO.
001953        05 銀行番号Ｗ                  PIC X(4)   VALUE SPACE.
001954        05 店番号Ｗ                    PIC X(3)   VALUE SPACE.
001955        05 口座番号Ｗ                  PIC X(10)  VALUE SPACE.
001956        05 口座名義人カナＷ            PIC X(40)  VALUE SPACE.
001957        05 口座名義人Ｗ                PIC X(40)  VALUE SPACE.
001958*
001959 01 連番Ｗ                             PIC 9(3)   VALUE ZERO.
001960 01 銀行名支店名Ｗ                     PIC X(40)  VALUE SPACE.
001961 01 預金種別コメントＷ                 PIC N(2)   VALUE SPACE.
001962 01 控えＷ                             PIC N(4)   VALUE SPACE.
001963*
001964 01 保険種別Ｗ                         PIC 9(2)  VALUE ZERO.
001965 01 保険者番号Ｗ                       PIC X(10) VALUE SPACE.
001966 01 請求先名称Ｗ                       PIC X(40) VALUE SPACE.
001967 01 支部部署名Ｗ                       PIC X(40) VALUE SPACE.
001968 01 宛名Ｗ                             PIC X(24) VALUE SPACE.
001969 01 保険者宛名Ｗ.
001970     03 保険者宛名１Ｗ                 PIC X(40) VALUE SPACE.
001971     03 保険者宛名２Ｗ                 PIC X(40) VALUE SPACE.
001972*
001973* 社保用
001974 01 接尾語区分Ｗ                       PIC 9     VALUE ZERO.
002007*
002008*********************************************************************
002009 01 印刷制御.
002010     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
002011     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
002012     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
002013     03 拡張制御Ｐ.
002014         05 端末制御Ｐ.
002015             07 移動方向Ｐ             PIC X(1) VALUE SPACE.
002016             07 移動行数Ｐ             PIC 9(3) VALUE ZERO.
002020         05 詳細制御Ｐ                 PIC X(2) VALUE SPACE.
002030     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
002040     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
002050*********************************************************************
002230*
       01 遅延フラグ                         PIC X(3) VALUE SPACE.
       01 遅延ＣＮＴ                         PIC 9(5) VALUE ZERO.
       01 遅延カウンタ                       PIC 9(4) VALUE ZERO.
       01 遅延回数Ｗ                         PIC 9(4) VALUE ZERO.
002240******************************************************************
002250*                          連結項目                              *
002260******************************************************************
002270*
002280********************
002290* メッセージ表示キー *
002300********************
002310 01 連メ－キー IS EXTERNAL.
002320    03  連メ－メッセージ                 PIC N(20).
002330*
       01 連入－画面情報ＹＡＩ５８０ IS EXTERNAL.
          03 連入－請求和暦年月.
             05 連入－請求和暦               PIC 9(1).
             05 連入－請求年月.
                07 連入－請求年              PIC 9(2).
                07 連入－請求月              PIC 9(2).
          03 連入－作成和暦年月日.
             05 連入－作成和暦年月.
                07 連入－作成和暦            PIC 9(1).
                07 連入－作成年              PIC 9(2).
                07 連入－作成月              PIC 9(2).
             05 連入－作成日                 PIC 9(2).
002351*
       01 連印－プレビュー IS EXTERNAL.
          03 連印－プレビュー区分            PIC 9.
002354*
000540************************************
000550* プリンタファイル作成用           *
000560************************************
000570 01 Ｈ連ＰＲＴＦ－作成データ IS EXTERNAL.
000580     03 Ｈ連ＰＲＴＦ－ファイル名           PIC X(8).
000590     03 Ｈ連ＰＲＴＦ－プレビュー区分       PIC 9.
000600     03 Ｈ連ＰＲＴＦ－帳票プログラム名     PIC X(8).
000610     03 Ｈ連ＰＲＴＦ－オーバレイ名         PIC X(8).
001772*
002355******************************************************************
002356*                      PROCEDURE  DIVISION                       *
002360******************************************************************
002370 PROCEDURE               DIVISION.
002380************
002390*           *
002400* 初期処理   *
002410*           *
002420************
002570     PERFORM プリンタファイル作成.
002430     PERFORM 初期化.
002440************
002450*           *
002460* 主処理     *
002470*           *
002480************
002484     PERFORM 印刷処理.
002500************
002510*           *
002520* 終了処理   *
002530*           *
002540************
002550     PERFORM 終了処理.
002560     EXIT PROGRAM.
002570*
002580*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YAI584"              TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連印－プレビュー区分 TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
002590*================================================================*
002600 初期化 SECTION.
002610*
002620     OPEN INPUT 元号マスタ.
002630             MOVE NC"元号" TO ファイル名Ｗ.
002640             PERFORM オープンチェック.
002710     OPEN INPUT 施術所情報マスタ
002720             MOVE NC"施情" TO ファイル名Ｗ.
002730             PERFORM オープンチェック.
002740     OPEN INPUT 保険者マスタ
002750             MOVE NC"保険者" TO ファイル名.
002760             PERFORM オープンチェック.
002770     OPEN INPUT 市町村マスタ
002780             MOVE NC"市町村" TO ファイル名Ｗ.
002790             PERFORM オープンチェック.
002791     OPEN INPUT 請求先マスタ
002792             MOVE NC"請求先" TO ファイル名Ｗ.
002793             PERFORM オープンチェック.
002800     OPEN INPUT 作業ファイル２.
002810             MOVE NC"作２" TO ファイル名Ｗ.
002820             PERFORM オープンチェック.
003250     OPEN INPUT レセプトＦ.
003260         MOVE NC"レセプトＦ" TO ファイル名.
003270         PERFORM オープンチェック.
002590     OPEN INPUT 受診者情報Ｆ.
002600         MOVE NC"受診者情報Ｆ" TO ファイル名Ｗ.
002610         PERFORM オープンチェック.
002830*
003001     PERFORM 連結項目退避.
003002     PERFORM 施術所情報取得.
003003*
003010*================================================================*
003020 オープンチェック SECTION.
003030*
003040     IF 状態キー  NOT =  "00"
003050         DISPLAY ファイル名Ｗ NC"Ｆオープンエラー" UPON CONS
003060         DISPLAY NC"状態キー：" 状態キー           UPON CONS
003070         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
003080                                                   UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
003090         ACCEPT  キー入力 FROM CONS
003100         PERFORM ファイル閉鎖
003110         EXIT PROGRAM.
003491*================================================================*
003492 連結項目退避 SECTION.
003493*
003494     MOVE 連入－請求和暦  TO 請求和暦Ｗ.
003495     MOVE 連入－請求年    TO 請求年Ｗ.
003496     MOVE 連入－請求月    TO 請求月Ｗ.
003497     MOVE 連入－作成和暦  TO 提出和暦Ｗ.
003498     MOVE 連入－作成年    TO 提出年Ｗ.
003499     MOVE 連入－作成月    TO 提出月Ｗ.
003500     MOVE 連入－作成日    TO 提出日Ｗ.
003502*
003503*================================================================*
003504 施術所情報取得 SECTION.
003505*
003506     MOVE ZERO  TO 施情－施術所番号.
003507     READ 施術所情報マスタ
003508     INVALID KEY
003509         CONTINUE
003510     NOT INVALID KEY
003511*
003512         MOVE 施情－郵便番号１       TO 施術所郵便番号１Ｗ
003513         MOVE "-"                    TO 施術所郵便番号区切Ｗ
003514         MOVE 施情－郵便番号２       TO 施術所郵便番号２Ｗ
003515         MOVE 施情－代表者名         TO 代表者名Ｗ
003516         MOVE 施情－接骨院名         TO 接骨院名Ｗ
003517         STRING 施情－住所１  DELIMITED BY SPACE
003518                施情－住所２  DELIMITED BY SPACE
003519           INTO 施術所住所Ｗ
003520         END-STRING
003521         MOVE 施情－電話番号         TO 施術所電話番号Ｗ
003522         MOVE 施情－新柔整師番号     TO 柔整師番号Ｗ
003523*
003524         MOVE 施情－取引先銀行名     TO 取引先銀行名Ｗ
003525         MOVE 施情－取引先銀行支店名 TO 取引先銀行支店名Ｗ
003526         MOVE 施情－預金種別         TO 預金種別Ｗ
003527         MOVE 施情－銀行番号         TO 銀行番号Ｗ
003528         MOVE 施情－店番号           TO 店番号Ｗ
003529         MOVE 施情－口座番号         TO 口座番号Ｗ
003530         MOVE 施情－口座名義人カナ   TO 口座名義人カナＷ
003531         MOVE 施情－口座名義人       TO 口座名義人Ｗ
003532         STRING 取引先銀行名Ｗ     DELIMITED BY SPACE
003533                " "                DELIMITED BY SIZE
003534                取引先銀行支店名Ｗ DELIMITED BY SPACE
003535                INTO 銀行名支店名Ｗ
003536         END-STRING
003537         EVALUATE 預金種別Ｗ
003538         WHEN 1
003539             MOVE NC"普通" TO 預金種別コメントＷ
003540         WHEN 2
003541             MOVE NC"当座" TO 預金種別コメントＷ
003542         WHEN OTHER
003543             MOVE SPACE    TO 預金種別コメントＷ
003544         END-EVALUATE
003545*
003546     END-READ.
003547*================================================================*
003548 ファイル閉鎖 SECTION.
003549*
003550     CLOSE 元号マスタ   作業ファイル２  施術所情報マスタ 市町村マスタ 
003552           保険者マスタ 請求先マスタ    受診者情報Ｆ     レセプトＦ.
003553*================================================================*
003560 終了処理 SECTION.
003570*
003580     PERFORM ファイル閉鎖.
003590*================================================================*
003600*================================================================*
003610 エラー表示 SECTION.
003620*
003630     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
003640     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
003650     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
003660     ACCEPT  キー入力 FROM CONS.
003670*================================================================*
003680 エラー表示Ｒ SECTION.
003690*
003700     DISPLAY NC"ファイル読込エラー" ファイル名     UPON CONS.
003710     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
003720     ACCEPT  キー入力 FROM CONS.
003730*================================================================*
003740 作業ファイル２読込 SECTION.
003750*
003760     READ 作業ファイル２ NEXT
003770     AT END
003780         MOVE "YES" TO 終了フラグ
003790     END-READ.
003800*================================================================*
003810 印刷処理 SECTION.
003820*
003856     MOVE LOW-VALUE TO  作２－県.
003857     MOVE ZERO      TO  作２－印刷順序.
003858     MOVE ZERO      TO  作２－保険種別.
003859     MOVE LOW-VALUE TO  作２－保険者番号.
005617     START 作業ファイル２   KEY IS >=  作２－印刷順序１
005621                                       作２－県
005622                                       作２－印刷順序
005623                                       作２－保険種別
005624                                       作２－保険者番号
005625     END-START.
003866     IF 状態キー = "00"
003867         MOVE SPACE TO 終了フラグ
003868         PERFORM 作業ファイル２読込
003869         IF  終了フラグ = "YES"
003870             MOVE  NC"　該当データなしです。確認して下さい。" TO 連メ－メッセージ
003871             CALL   "MSG001"
003872             CANCEL "MSG001"
003873             PERFORM ファイル閉鎖
003874             MOVE 99 TO PROGRAM-STATUS
003875             EXIT PROGRAM
003876         END-IF
003877*
003878         PERFORM UNTIL 終了フラグ = "YES"
003882*
003883             MOVE SPACE TO YAI584P
003884             INITIALIZE    YAI584P
003885             MOVE 作２－県         TO 県Ｗ
003886             MOVE 作２－印刷順序１ TO 印刷順序Ｗ
003887             PERFORM  表セット１
003888* 
003889             PERFORM VARYING 行カウンタ FROM 1 BY 1
003890                     UNTIL ( 行カウンタ > 8       ) OR
003891                           ( 県Ｗ       NOT = 作２－県       ) OR
003892                           ( 印刷順序Ｗ NOT = 作２－印刷順序１ ) OR
003893                           ( 終了フラグ = "YES" )
003897                  PERFORM 表セット２
003901                  PERFORM 作業ファイル２読込
003903             END-PERFORM
003904*
003905             IF ( 終了フラグ =  "YES" ) OR 
003906                ( 印刷順序Ｗ NOT = 作２－印刷順序１ ) OR
003907                ( 県Ｗ       NOT = 作２－県 )
003908                 MOVE 本人件数カウント   TO     本人件数合計
003909                 MOVE 家族件数カウント   TO     家族件数合計
003910                 MOVE 計件数カウント     TO     計件数合計
003911                 MOVE 本人費用額カウント TO     本人費用額合計
003912                 MOVE 家族費用額カウント TO     家族費用額合計
003913                 MOVE 計費用額カウント   TO     計費用額合計
003914                 MOVE 本人請求額カウント TO     本人請求額合計
003915                 MOVE 家族請求額カウント TO     家族請求額合計
003916                 MOVE 計請求額カウント   TO     計請求額合計
003917                 IF 本人請求額カウント NOT = ZERO
003918                    MOVE "(" TO 本人左括弧合計
003919                    MOVE ")" TO 本人右括弧合計
003920                 ELSE
003921                    MOVE SPACE TO 本人左括弧合計
003922                    MOVE SPACE TO 本人右括弧合計
003923                 END-IF
003924                 IF 家族請求額カウント NOT = ZERO
003925                    MOVE "(" TO 家族左括弧合計
003926                    MOVE ")" TO 家族右括弧合計
003927                 ELSE
003928                    MOVE SPACE TO 家族左括弧合計
003929                    MOVE SPACE TO 家族右括弧合計
003930                 END-IF
003931                 IF 計請求額カウント NOT = ZERO
003932                    MOVE "(" TO 計左括弧合計
003933                    MOVE ")" TO 計右括弧合計
003934                 ELSE
003935                    MOVE SPACE TO 計左括弧合計
003936                    MOVE SPACE TO 計右括弧合計
003937                 END-IF
003938*
003940             ELSE
003941                 MOVE ZERO     TO     本人件数合計
003942                 MOVE ZERO     TO     家族件数合計
003943                 MOVE ZERO     TO     計件数合計
003944                 MOVE ZERO     TO     本人費用額合計
003945                 MOVE ZERO     TO     家族費用額合計
003946                 MOVE ZERO     TO     計費用額合計
003947                 MOVE ZERO     TO     本人請求額合計
003948                 MOVE ZERO     TO     家族請求額合計
003949                 MOVE ZERO     TO     計請求額合計
003950             END-IF
003953*
003954             PERFORM 印字処理
003955             PERFORM 改頁処理
                   INITIALIZE 合計用Ｗ
003966         END-PERFORM
003967*
003968     ELSE
003969         MOVE  NC"　該当データなしです。確認して下さい。" TO 連メ－メッセージ
003970         CALL   "MSG001"
003971         CANCEL "MSG001"
003972         PERFORM ファイル閉鎖
003973         MOVE 99 TO PROGRAM-STATUS
003974         EXIT PROGRAM
003975     END-IF.
004610*
002990     IF ( オープンフラグ = "YES" )
002991         CLOSE 印刷ファイル
003041     END-IF.
004621*
004630*================================================================*
004640 表セット１ SECTION.
004650*
004944* 当月の和暦を取得
004945*     MOVE 請求和暦Ｗ         TO 元－元号区分.
004946*     READ 元号マスタ
004947*     INVALID KEY
004948*         MOVE SPACE          TO 請求和暦名称
004949*     NOT INVALID KEY
004950*         MOVE 元－元号名称   TO 請求和暦名称
004951*     END-READ.
004952*
      */元号修正/↓↓↓20190514
037370     IF 請求和暦Ｗ > 4
              MOVE 請求和暦Ｗ         TO 元－元号区分
037380        READ 元号マスタ
037390        NOT INVALID KEY
037400            MOVE 元－元号名称   TO 施術和暦
037410        END-READ
              MOVE "===="             TO 施術和暦訂正
           END-IF.
      */元号修正/↑↑↑20190514
004953     MOVE 請求年Ｗ           TO 請求年.
004954     MOVE 請求月Ｗ           TO 請求月.
004955*
004968*     MOVE 施術所住所Ｗ       TO 住所.
004969*     MOVE 施術所郵便番号Ｗ   TO 郵便番号.
004970     MOVE 代表者名Ｗ         TO 代表者名.
004971     MOVE 接骨院名Ｗ         TO 接骨院名.
004972     MOVE 柔整師番号１Ｗ     TO 柔整師番号１.
004972     MOVE 柔整師番号２Ｗ     TO 柔整師番号２.
004972     MOVE 柔整師番号３Ｗ     TO 柔整師番号３.
004973*     MOVE 施術所電話番号Ｗ   TO 電話番号.
004974*     MOVE 銀行名支店名Ｗ     TO 銀行名支店名.
004975*     MOVE 預金種別コメントＷ TO 預金種別.
004976*     MOVE 口座番号Ｗ         TO 口座番号.
004977*     MOVE 口座名義人カナＷ   TO 口座名義人カナ.
004978*     MOVE 口座名義人Ｗ       TO 口座名義人.
004979*
004980*================================================================*
004981 表セット２ SECTION.
004982*
004983     MOVE 作２－本人件数     TO 本人件数(行カウンタ).
004990     MOVE 作２－本人費用額   TO 本人費用額(行カウンタ).
005000     MOVE 作２－本人請求額   TO 本人請求額(行カウンタ).
005010     IF 作２－本人請求額 NOT = ZERO
005020        MOVE "(" TO 本人左括弧(行カウンタ)
005030        MOVE ")" TO 本人右括弧(行カウンタ)
005040     ELSE
005050        MOVE SPACE TO 本人左括弧(行カウンタ)
005060        MOVE SPACE TO 本人右括弧(行カウンタ)
005070     END-IF.
005080*
005090     MOVE 作２－家族件数     TO 家族件数(行カウンタ).
005100     MOVE 作２－家族費用額   TO 家族費用額(行カウンタ).
005110     MOVE 作２－家族請求額   TO 家族請求額(行カウンタ).
005120     IF 作２－家族請求額 NOT = ZERO
005130        MOVE "(" TO 家族左括弧(行カウンタ)
005140        MOVE ")" TO 家族右括弧(行カウンタ)
005150     ELSE
005160        MOVE SPACE TO 家族左括弧(行カウンタ)
005170        MOVE SPACE TO 家族右括弧(行カウンタ)
005180     END-IF.
005190*
005230     MOVE 作２－件数    TO  計件数(行カウンタ).
005240     MOVE 作２－費用額  TO  計費用額(行カウンタ).
005250     MOVE 作２－請求額  TO  計請求額(行カウンタ).
005260     IF 作２－請求額 NOT = ZERO
005270        MOVE "(" TO 計左括弧(行カウンタ)
005280        MOVE ")" TO 計右括弧(行カウンタ)
005290     ELSE
005300        MOVE SPACE TO 計左括弧(行カウンタ)
005310        MOVE SPACE TO 計右括弧(行カウンタ)
005320     END-IF.
005330*
005340     ADD 作２－本人件数   TO  本人件数カウント.
005350     ADD 作２－家族件数   TO  家族件数カウント.
005360     ADD 作２－件数       TO  計件数カウント.
005370     ADD 作２－本人費用額 TO  本人費用額カウント.
005380     ADD 作２－家族費用額 TO  家族費用額カウント.
005390     ADD 作２－費用額     TO  計費用額カウント.
005400     ADD 作２－本人請求額 TO  本人請求額カウント.
005410     ADD 作２－家族請求額 TO  家族請求額カウント.
005420     ADD 作２－請求額     TO  計請求額カウント.
005430**
005440* 保険者
005441     MOVE 作２－保険種別     TO 保険種別Ｗ.
005442     MOVE 作２－保険者番号   TO 保険者番号Ｗ.
005443     EVALUATE 保険種別Ｗ
005444     WHEN 1 THRU 4
005445     WHEN 6 THRU 9
005446         PERFORM 保険者情報取得
005447     WHEN 5
005448     WHEN 50 THRU 60
005449         PERFORM 市町村情報取得
005450     END-EVALUATE.
005451     PERFORM 結合宛名取得.
005452*
005453     MOVE 保険者宛名Ｗ  TO 保険者名称Ｗ.
005970*
006000*
006010     COMPUTE 合計行カウンタ = 行カウンタ * 3.
006020     MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 1).
006030     IF 作２－保険種別 = "50" OR "51" OR "52" OR "53" OR
006040                         "54" OR "55" OR "60" OR "05" OR "08"
006050        IF 保険者名称２Ｗ NOT = SPACE AND 保険者名称３Ｗ = SPACE
006060           MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 2)
006070           MOVE 保険者名称２Ｗ TO 保険者名(合計行カウンタ - 1)
006080        END-IF
006090     ELSE
006100        IF 保険者名称２Ｗ NOT = SPACE AND 保険者名称３Ｗ = SPACE
006110           MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 1)
006120           MOVE 保険者名称２Ｗ TO 保険者名(合計行カウンタ)
006130        END-IF
006140     END-IF.
006150     IF 作２－保険種別 = "50" OR "51" OR "52" OR "53" OR
006160                         "54" OR "55" OR "60" OR "05" OR "08"
006170        IF 保険者名称２Ｗ NOT = SPACE AND 保険者名称３Ｗ NOT = SPACE
006180           MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 2)
006190           MOVE 保険者名称２Ｗ TO 保険者名(合計行カウンタ - 1)
006200        END-IF
006210     ELSE
006220        IF 保険者名称２Ｗ NOT = SPACE AND 保険者名称３Ｗ NOT = SPACE
006230           MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 2)
006240           MOVE 保険者名称２Ｗ TO 保険者名(合計行カウンタ - 1)
006250           MOVE 保険者名称３Ｗ TO 保険者名(合計行カウンタ)
006260        END-IF
006270     END-IF.
006280     IF 作２－保険種別 = "50" OR "51" OR "52" OR "53" OR
006290                         "54" OR "55" OR "60" 
006300        MOVE "（助成）" TO 保険者名(合計行カウンタ)
006310     END-IF.
006320     IF 作２－保険種別 = "05"
006330        MOVE "（老人）" TO 保険者名(合計行カウンタ)
006340     END-IF.
006350     IF 作２－保険種別 = "08"
006360        MOVE "（退職）" TO 保険者名(合計行カウンタ)
006370     END-IF.
      */保険者名称印刷を保険者番号印刷に変更/1311
           MOVE SPACE            TO 保険者名(合計行カウンタ - 2).
           MOVE SPACE            TO 保険者名(合計行カウンタ - 1).
           MOVE SPACE            TO 保険者名(合計行カウンタ).
           MOVE 作２－保険者番号 TO 保険者番号(行カウンタ).
006380*
006390*================================================================*
006400 エラー処理Ｐ SECTION.
006410*
006420     IF 通知情報Ｐ NOT = "00"
006430         DISPLAY NC"帳票エラー"              UPON CONS
006440         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
006450         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
006460         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
006470         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
006480                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
006490         ACCEPT  キー入力 FROM CONS
006500         PERFORM ファイル閉鎖
006510         MOVE 99  TO PROGRAM-STATUS
006520         EXIT PROGRAM
006530     END-IF.
006540*================================================================*
006550 印字処理  SECTION.
006560*
004310     IF ( オープンフラグ NOT = "YES" )
004320        MOVE "YES" TO オープンフラグ
004330        OPEN I-O  印刷ファイル
004340        PERFORM エラー処理Ｐ
004350     END-IF.
013440*
006570     MOVE "YAI584P" TO  定義体名Ｐ.
006580     MOVE SPACE     TO  処理種別Ｐ.
006590     MOVE "SCREEN"  TO  項目群名Ｐ.
006610     WRITE YAI584P.
006620     PERFORM エラー処理Ｐ.
006650*================================================================*
006660 改頁処理  SECTION.
006670
006680     MOVE "YAI584P" TO  定義体名Ｐ.
006690     MOVE "CT"      TO  処理種別Ｐ.
006700     MOVE "PAGE"    TO  拡張制御Ｐ.
006710     MOVE SPACE     TO  項目群名Ｐ.
006730     WRITE YAI584P.
006740     PERFORM エラー処理Ｐ.
006750     MOVE SPACE     TO  拡張制御Ｐ.
006760*
006770*     CLOSE  印刷ファイル.
006780*     OPEN I-O   印刷ファイル.
006790*     PERFORM エラー処理Ｐ.
006800*
006810*================================================================*
006811*================================================================*
006812 保険者情報取得 SECTION.
006813*
006814     MOVE  SPACE         TO 請求先名称Ｗ.
006815     MOVE  SPACE         TO 支部部署名Ｗ.
006816     MOVE  ZERO          TO 接尾語区分Ｗ.
006817*
006818     MOVE 保険種別Ｗ     TO 保－保険種別.
006819     MOVE 保険者番号Ｗ   TO 保－保険者番号.
006820     READ 保険者マスタ
006821     INVALID KEY
006822         MOVE SPACE      TO 請求先名称Ｗ
006823         MOVE SPACE      TO 支部部署名Ｗ
006824     NOT INVALID KEY
006825         IF 保－請求先情報区分 = 1
006826             MOVE 保－保険種別   TO 請先－保険種別
006827             MOVE 保－保険者番号 TO 請先－保険者番号
006828             READ 請求先マスタ
006829             INVALID KEY
006830                 MOVE SPACE             TO 請求先名称Ｗ
006831                 MOVE SPACE             TO 支部部署名Ｗ
006832             NOT INVALID KEY
006833                 MOVE 請先－保険者名称  TO 請求先名称Ｗ
006834                 MOVE 請先－支部部署名  TO 支部部署名Ｗ
006835             END-READ
006836         ELSE
006837             MOVE 保－保険者名称        TO 請求先名称Ｗ
006838             MOVE 保－支部部署名        TO 支部部署名Ｗ
006839             MOVE 保－接尾語区分        TO 接尾語区分Ｗ
006840         END-IF
006841     END-READ.
006842*================================================================*
006843 市町村情報取得 SECTION.
006844*
006845     MOVE  SPACE         TO 請求先名称Ｗ.
006846     MOVE  SPACE         TO 支部部署名Ｗ.
006847*
006848     MOVE 保険種別Ｗ               TO 市－公費種別.
006849     MOVE 保険者番号Ｗ             TO 市－市町村番号.
006850     READ 市町村マスタ
006851     INVALID KEY
006852         MOVE SPACE                TO 請求先名称Ｗ
006853         MOVE SPACE                TO 支部部署名Ｗ
006854     NOT INVALID KEY
006855         IF 市－請求先区分 = 1
006856             MOVE 保険種別Ｗ       TO 請先－保険種別
006857             MOVE 保険者番号Ｗ     TO 請先－保険者番号
006858             READ 請求先マスタ
006859             INVALID KEY
006860                 MOVE SPACE        TO 請求先名称Ｗ
006861                 MOVE SPACE        TO 支部部署名Ｗ
006862             NOT INVALID KEY
006863                 MOVE 請先－保険者名称   TO 請求先名称Ｗ
006864                 MOVE 請先－支部部署名   TO 支部部署名Ｗ
006865             END-READ
006866          ELSE
006867             MOVE 市－市町村名称   TO 請求先名称Ｗ
006868             MOVE 市－支部部署名   TO 支部部署名Ｗ
006869          END-IF
006870      END-READ.
006871*================================================================*
006872 結合宛名取得 SECTION.
006873*
006874     MOVE SPACE TO 保険者宛名Ｗ.
006875     IF 請求先名称Ｗ NOT = SPACE
006876         EVALUATE 保険種別Ｗ
006877         WHEN 2
006878             IF 接尾語区分Ｗ = 1
006879                MOVE SPACE            TO 宛名Ｗ
006880             ELSE
006881                MOVE "社会保険事務所" TO 宛名Ｗ
006882             END-IF
006883         WHEN 6
006884             IF 接尾語区分Ｗ = 1
006885                MOVE "（日雇）"               TO 宛名Ｗ
006887             ELSE
006888                MOVE "社会保険事務所（日雇）" TO 宛名Ｗ
006889             END-IF
006890         WHEN 7
006891             MOVE "（船員）"       TO 宛名Ｗ
006892         WHEN 3
006893             MOVE "健康保険組合"   TO 宛名Ｗ
006894         WHEN 4
006895             MOVE "共済組合"       TO 宛名Ｗ
006896         WHEN OTHER
006897             MOVE SPACE            TO 宛名Ｗ
006898         END-EVALUATE
006899*
006900         IF 支部部署名Ｗ = SPACE
006901             STRING  請求先名称Ｗ  DELIMITED BY SPACE
006902                     宛名Ｗ        DELIMITED BY SPACE
006903                    INTO 保険者宛名Ｗ
006904             END-STRING
006905         ELSE
006906             STRING  請求先名称Ｗ  DELIMITED BY SPACE
006907                     宛名Ｗ        DELIMITED BY SPACE
006908                     支部部署名Ｗ  DELIMITED BY SPACE
006909                    INTO 保険者宛名Ｗ
006910             END-STRING
006911         END-IF
006912     END-IF.
006913*
007011*================================================================*
004812 データチェック SECTION.
004813*
004814     MOVE SPACE          TO 実行キーＷ.
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
004860*
004868*================================================================*
006930 レセプトＦ読込 SECTION.
006940*
006950     READ レセプトＦ NEXT
004790     AT END
004800         MOVE "YES" TO 終了フラグ３
004810     END-READ.
004868*================================================================*
007012******************************************************************
007013 END PROGRAM YAI584.
007014******************************************************************
