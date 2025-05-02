000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YJK436.
000060 AUTHOR.                 岡田 憲和
000070*
000080*----------------------------------------------------------------*
000090*  日本柔整共済会用 ７号用紙 請求書【印刷】柔ｳｨﾝﾄﾞｳｽﾞ95版
000100*         MED = YJK436P
000110*
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2012-09-06
000140 DATE-COMPILED.          2012-09-06
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
000330     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000340                             ORGANIZATION             IS  INDEXED
000350                             ACCESS MODE              IS  DYNAMIC
000360                             RECORD KEY               IS  制－制御区分
000370                             FILE STATUS              IS  状態キー
000380                             LOCK        MODE         IS  AUTOMATIC.
000390     SELECT  保険者マスタ    ASSIGN      TO        HOKENSL
000400                             ORGANIZATION             IS  INDEXED
000410                             ACCESS MODE              IS  DYNAMIC
000420                             RECORD KEY               IS  保－保険種別
000430                                                          保－保険者番号
000440                             ALTERNATE RECORD KEY     IS  保－保険種別
000450                                                          保－保険者名称
000460                                                          保－保険者番号
000470                             FILE STATUS              IS  状態キー
000480                             LOCK        MODE         IS  AUTOMATIC.
000490     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
000500                             ORGANIZATION             IS  INDEXED
000510                             ACCESS MODE              IS  DYNAMIC
000520                             RECORD KEY               IS  市－公費種別
000530                                                          市－市町村番号
000540                             ALTERNATE RECORD KEY     IS  市－公費種別
000550                                                          市－市町村名称
000560                                                          市－市町村番号
000570                             FILE STATUS              IS  状態キー
000580                             LOCK        MODE         IS  AUTOMATIC.
000660     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000670                             ORGANIZATION             IS  INDEXED
000680                             ACCESS MODE              IS  DYNAMIC
000690                             RECORD KEY               IS 施情－施術所番号
000700                             FILE STATUS              IS  状態キー
000710                             LOCK        MODE         IS  AUTOMATIC.
000720     SELECT  請求先マスタ    ASSIGN      TO        SEIKYUSL
000730                             ORGANIZATION             IS  INDEXED
000740                             ACCESS MODE              IS  DYNAMIC
000750                             RECORD KEY               IS  請先－保険種別
000760                                                          請先－保険者番号
000770                             FILE STATUS              IS  状態キー
000780                             LOCK    MODE             IS  AUTOMATIC.
000910     SELECT  会情報マスタ    ASSIGN      TO        KAIJOHOL
000911                             ORGANIZATION             IS  INDEXED
000912                             ACCESS MODE              IS  DYNAMIC
000913                             RECORD KEY               IS  会情－柔整鍼灸区分
000914                                                          会情－協会コード
000915                                                          会情－保険種別
000916                                                          会情－変更和暦年月
000917                             ALTERNATE RECORD KEY     IS  会情－柔整鍼灸区分
000918                                                          会情－接骨師会カナ
000919                                                          会情－協会コード
000920                                                          会情－保険種別
000921                                                          会情－変更和暦年月
000922                             FILE STATUS              IS  状態キー
000923                             LOCK        MODE         IS  AUTOMATIC.
000924*
000925     SELECT  作業ファイル２  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W4312L.DAT"
000930                             ORGANIZATION             IS  INDEXED
000940                             ACCESS                   IS  DYNAMIC
000950                             RECORD      KEY          IS  作２－請求和暦年月
000960                                                          作２－保険種別
000970                                                          作２－保険者番号
000980                             ALTERNATE RECORD KEY     IS  作２－請求和暦年月
000990                                                          作２－県
001000                                                          作２－印刷順序
001010                                                          作２－保険種別
001020                                                          作２－保険者番号
001030                             ALTERNATE RECORD KEY     IS  作２－請求和暦年月
001040                                                          作２－印刷順序
001050                                                          作２－保険種別
001060                                                          作２－保険者番号
001070*                                                          作２－県
001080*/県別、社保日雇いをまとめ、船員を別に集計/081021
001090                             ALTERNATE RECORD KEY     IS  作２－請求和暦年月
001100                                                          作２－県２
001110                                                          作２－印刷順序
001120                                                          作２－保険種別
001130                                                          作２－保険者番号
001140                             FILE        STATUS       IS  状態キー
001150                             LOCK        MODE         IS  AUTOMATIC.
001160*
001170     SELECT  保険者拡張マスタ ASSIGN     TO        HOKENEXL
001180                             ORGANIZATION             IS  INDEXED
001190                             ACCESS MODE              IS  DYNAMIC
001200                             RECORD KEY               IS  保拡－保険種別
001210                                                          保拡－保険者番号
001220                             FILE STATUS              IS  状態キー
001230                             LOCK        MODE         IS  AUTOMATIC.
001240     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF001
001250                             SYMBOLIC    DESTINATION  IS "PRT"
001260                             FORMAT                   IS  定義体名Ｐ
001270                             GROUP                    IS  項目群名Ｐ
001280                             PROCESSING  MODE         IS  処理種別Ｐ
001290                             UNIT        CONTROL      IS  拡張制御Ｐ
001300                             FILE        STATUS       IS  通知情報Ｐ.
001310******************************************************************
001320*                      DATA DIVISION                             *
001330******************************************************************
001340 DATA                    DIVISION.
001350 FILE                    SECTION.
001360*                           ［ＲＬ＝  １２８］
001370 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001380     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001390*                           ［ＲＬ＝  ２５６］
001400 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001410     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
001420     COPY SEIGYO01        OF  XFDLIB  JOINING   制０１   AS  PREFIX.
001430*                           ［ＲＬ＝  ３２０］
001440 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001450     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
001460*                           ［ＲＬ＝  ２５６］
001470 FD  市町村マスタ        BLOCK   CONTAINS   1   RECORDS.
001480     COPY SITYOSN         OF  XFDLIB  JOINING   市   AS  PREFIX.
001520*                           ［ＲＬ＝  １２８］
001530 FD  施術所情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001540     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
001550*                           ［ＲＬ＝  １２８］
001560 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
001570     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
001580*                          ［ＲＬ＝  ６４０］
001590 FD  会情報マスタ        BLOCK   CONTAINS   1   RECORDS.
001600     COPY KAIJOHO         OF  XFDLIB  JOINING   会情   AS  PREFIX.
001610*                           ［ＲＬ＝  ８００］
001620 FD  保険者拡張マスタ        BLOCK   CONTAINS   1   RECORDS.
001630     COPY HOKENEX        OF  XFDLIB  JOINING   保拡   AS  PREFIX.
001640*
001650*
001660* 施術/請求  (請求和暦年月は、施術/請求 共用) 
001670*                           ［ＲＬ＝  １２８］
001680 FD  作業ファイル２ RECORD  CONTAINS 128 CHARACTERS.
001690 01  作２－レコード.
001700     03  作２－レコードキー.
001710         05  作２－請求和暦年月.
001720             07  作２－請求和暦              PIC 9.
001730             07  作２－請求年                PIC 9(2).
001740             07  作２－請求月                PIC 9(2).
001750         05  作２－保険種別                  PIC 9(2).
001760         05  作２－保険者番号                PIC X(10).
001770         05  作２－県                        PIC X(2).
001780         05  作２－印刷順序.
001790             07  作２－印刷順序１            PIC 9(2).
001800             07  作２－印刷順序２            PIC 9.
001810     03  作２－レコードデータ.
001820         05  作２－保険者番号２              PIC X(10).
001830         05  作２－件数                      PIC 9(4).
001840         05  作２－費用額                    PIC 9(9).
001850         05  作２－負担額                    PIC 9(9).
001860         05  作２－請求額                    PIC 9(9).
001870         05  作２－本人件数                  PIC 9(3).
001880         05  作２－本人費用額                PIC 9(7).
001890         05  作２－本人負担額                PIC 9(7).
001900         05  作２－本人請求額                PIC 9(7).
001910         05  作２－家族件数                  PIC 9(3).
001920         05  作２－家族費用額                PIC 9(7).
001930         05  作２－家族負担額                PIC 9(7).
001940         05  作２－家族請求額                PIC 9(7).
001950         05  作２－県２                      PIC X(2).
001960         05  FILLER                          PIC X(15).
001970*         05  FILLER                          PIC X(17).
001980*
001990 FD  印刷ファイル.
002000     COPY YJK436P        OF  XMDLIB.
002010*----------------------------------------------------------------*
002020******************************************************************
002030*                WORKING-STORAGE SECTION                         *
002040******************************************************************
002050 WORKING-STORAGE         SECTION.
002060 01 キー入力                           PIC X     VALUE SPACE.
002070 01 状態キー                           PIC X(2)  VALUE SPACE.
002080 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002090 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002100 01 書込フラグ                         PIC X(4)  VALUE SPACE.
002110 01 作業フラグ                         PIC X(3)  VALUE SPACE.
002120 01 作業移動キー                       PIC X(4)  VALUE SPACE.
002130 01 終了行フラグ                       PIC X(3)  VALUE SPACE.
002140 01 ファイル名                         PIC N(2)  VALUE SPACE.
002150 01 備考Ｗ                             PIC X(20) VALUE SPACE.
002160 01 前和暦Ｗ                           PIC 9(1)  VALUE ZERO.
002170 01 保険種別Ｗ                         PIC 9(2)  VALUE ZERO.
002180 01 保険者番号Ｗ                       PIC X(10) VALUE SPACE.
002190 01 保険者番号１Ｗ                     PIC X(10) VALUE SPACE.
002200 01 保険者番号２Ｗ                     PIC X(10) VALUE SPACE.
002210 01 印刷フラグ                         PIC X(3)  VALUE SPACE.
002211 01 オープンフラグ                     PIC X(3)  VALUE SPACE.
002220*
002230 01 行カウンタ                         PIC 9(2)  VALUE ZERO.
002240 01 頁カウンタ                         PIC 9(4)  VALUE ZERO.
002250 01 最大行数                           PIC 9(2)  VALUE ZERO.
002260 01 ヘッダ行数                         PIC 9(2)  VALUE ZERO.
002270 01 移動行数Ｗ                         PIC 9(2)  VALUE ZERO.
002280 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
002290 01 保険名称Ｗ                         PIC N(2) VALUE SPACE.
002300*
002310 01 施術和暦年月Ｗ.
002320     03 施術和暦Ｗ                     PIC 9(1)  VALUE ZERO.
002330     03 施術年月Ｗ.
002340        05 施術年Ｗ                    PIC 9(2)  VALUE ZERO.
002350        05 施術月Ｗ                    PIC 9(2)  VALUE ZERO.
002360**
002370**************
002380* 施術所情報 *
002390**************
002400 01 施術所情報Ｗ.
002410    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
002420    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
002430    03 柔整師番号Ｗ                    PIC X(20)  VALUE SPACE.
002440    03 施術所住所Ｗ.
002450       05 施術所住所１Ｗ               PIC X(40)  VALUE SPACE.
002460       05 施術所住所２Ｗ               PIC X(40)  VALUE SPACE.
002470    03 施術所郵便番号Ｗ.
002480       05 施術所郵便番号１Ｗ           PIC X(3)   VALUE SPACE.
002490       05 施術所郵便番号区切Ｗ         PIC X(1)   VALUE SPACE.
002500       05 施術所郵便番号２Ｗ           PIC X(4)   VALUE SPACE.
002510    03 施術所電話番号Ｗ                PIC X(15)  VALUE SPACE.
002520    03 取引先情報Ｗ.
002530        05 取引先銀行名Ｗ              PIC X(40)  VALUE SPACE.
002540        05 取引先銀行支店名Ｗ          PIC X(40)  VALUE SPACE.
002550        05 預金種別Ｗ                  PIC 9(1)   VALUE ZERO.
002560        05 銀行番号Ｗ                  PIC X(4)   VALUE SPACE.
002570        05 店番号Ｗ                    PIC X(3)   VALUE SPACE.
002580        05 口座番号Ｗ                  PIC X(10)  VALUE SPACE.
002590        05 口座名義人カナＷ            PIC X(40)  VALUE SPACE.
002600        05 口座名義人Ｗ                PIC X(40)  VALUE SPACE.
002610*
002620 01 連番Ｗ                             PIC 9(3)   VALUE ZERO.
002630 01 銀行名支店名Ｗ                     PIC X(40)  VALUE SPACE.
002640 01 預金種別コメントＷ                 PIC N(2)   VALUE SPACE.
002650 01 控えＷ                             PIC N(4)   VALUE SPACE.
002660**
002670* 社保用
002680 01 接尾語区分Ｗ                       PIC 9     VALUE ZERO.
002690*
002700********************
002710* 保険者別合計金額 *
002720********************
002730 01 保険者別合計金額.
002740    03 金額Ｗ                          PIC N(2)  VALUE SPACE.
002750    03 請求メッセージＷ                PIC N(15) VALUE SPACE.
002760    03 円Ｗ                            PIC N(1)  VALUE SPACE.
002770    03 件数Ｗ                          PIC 9(3)  VALUE ZERO.
002780    03 費用額Ｗ                        PIC 9(8)  VALUE ZERO.
002790    03 請求額Ｗ                        PIC 9(7)  VALUE ZERO.
002800    03 請求先名称Ｗ                    PIC X(40) VALUE SPACE.
002810    03 支部部署名Ｗ                    PIC X(40) VALUE SPACE.
002820    03 宛名Ｗ                          PIC X(24) VALUE SPACE.
002830    03 保険者宛名Ｗ.
002840       05 保険者宛名１Ｗ               PIC X(40) VALUE SPACE.
002850       05 保険者宛名２Ｗ               PIC X(40) VALUE SPACE.
002860**
002870 01 画面情報４３０Ｗ.
002880    03 請求年月Ｗ.
002890       05 請求和暦Ｗ                   PIC 9     VALUE ZERO.
002900       05 請求年Ｗ                     PIC 9(2)  VALUE ZERO.
002910       05 請求月Ｗ                     PIC 9(2)  VALUE ZERO.
002920    03 提出年月日Ｗ.
002930       05 提出和暦Ｗ                   PIC 9     VALUE ZERO.
002940       05 提出年Ｗ                     PIC 9(2)  VALUE ZERO.
002950       05 提出月Ｗ                     PIC 9(2)  VALUE ZERO.
002960       05 提出日Ｗ                     PIC 9(2)  VALUE ZERO.
002970    03 印刷種類Ｗ                      PIC 9     VALUE ZERO.
002980***
002990* エラーメッセージ用
003000 01 エラーメッセージＷ.
003010    03 エラー保険種別Ｗ                PIC X(2) VALUE SPACE.
003020    03 エラー区切りＷ                  PIC X(1) VALUE SPACE.
003030    03 エラー保険者番号Ｗ              PIC X(10) VALUE SPACE.
003040    03 FILLER                          PIC X(7) VALUE SPACE.
003050*
003060***
003070* 請求書印刷パラメタ用
003080*  ６・７号請求書（印刷区分 0:印刷 1:印刷しない、振込先 0:自分 1:会 9:印刷しない）
003090*  当社用紙初期値1:印刷しない
003100***
003110 01 請求書関連Ｗ.
003120        07 国保６号印刷区分Ｗ          PIC 9 VALUE ZERO.
003130        07 国保７号印刷区分Ｗ          PIC 9 VALUE ZERO.
003140        07 国保当社印刷区分Ｗ          PIC 9 VALUE 1.
003150        07 国保振込先区分Ｗ            PIC 9 VALUE ZERO.
003160        07 社保６号印刷区分Ｗ          PIC 9 VALUE ZERO.
003170        07 社保７号印刷区分Ｗ          PIC 9 VALUE ZERO.
003180        07 社保当社印刷区分Ｗ          PIC 9 VALUE 1.
003190        07 社保振込先区分Ｗ            PIC 9 VALUE ZERO.
003200        07 組合６号印刷区分Ｗ          PIC 9 VALUE ZERO.
003210        07 組合７号印刷区分Ｗ          PIC 9 VALUE ZERO.
003220        07 組合当社印刷区分Ｗ          PIC 9 VALUE 1.
003230        07 組合振込先区分Ｗ            PIC 9 VALUE ZERO.
003240        07 共済６号印刷区分Ｗ          PIC 9 VALUE ZERO.
003250        07 共済７号印刷区分Ｗ          PIC 9 VALUE ZERO.
003260        07 共済当社印刷区分Ｗ          PIC 9 VALUE 1.
003270        07 共済振込先区分Ｗ            PIC 9 VALUE ZERO.
003280        07 老人６号印刷区分Ｗ          PIC 9 VALUE ZERO.
003290        07 老人７号印刷区分Ｗ          PIC 9 VALUE ZERO.
003300        07 老人当社印刷区分Ｗ          PIC 9 VALUE 1.
003310        07 老人振込先区分Ｗ            PIC 9 VALUE ZERO.
003320        07 助成６号印刷区分Ｗ          PIC 9 VALUE 1.
003330        07 助成７号印刷区分Ｗ          PIC 9 VALUE ZERO.
003340        07 助成当社印刷区分Ｗ          PIC 9 VALUE 1.
003350        07 助成振込先区分Ｗ            PIC 9 VALUE ZERO.
003360*
003370***********************************************************************
003380 01 印刷制御.
003390     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
003400     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
003410     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
003420     03 拡張制御Ｐ.
003430         05 端末制御Ｐ.
003440             07 移動方向Ｐ             PIC X(1).
003450             07 移動行数Ｐ             PIC 9(3).
003460         05 詳細制御Ｐ                 PIC X(2).
003470     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
003480     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
003490*
003500 01 計算機西暦年Ｗ                     PIC 9(2).
003510* 日付ＷＯＲＫ
003520 01 和暦終了年Ｗ                       PIC 9(4).
003530 01 計算機西暦.
003540    03 計算機西暦年                    PIC 9(4).
003550    03 計算機西暦月日                  PIC 9(4).
003560 01 計算機西暦Ｒ REDEFINES 計算機西暦.
003570    03 計算機世紀                      PIC 9(2).
003580    03 計算機日付                      PIC 9(6).
003590    03 計算機日付Ｒ REDEFINES 計算機日付.
003600       05 計算機年月                   PIC 9(4).
003610       05 計算機年月Ｒ REDEFINES 計算機年月.
003620         07 計算機年                   PIC 9(2).
003630         07 計算機月                   PIC 9(2).
003640       05 計算機日                     PIC 9(2).
003650*
003660******************************************************************
003670*                          連結項目                              *
003680******************************************************************
003690*
003700********************
003710* メッセージ表示キー *
003720********************
003730 01 連メ－キー IS EXTERNAL.
003740    03  連メ－メッセージ               PIC N(20).
003750*
003760 01 連メ３－キー IS EXTERNAL.
003770    03  連メ３－メッセージ             PIC N(20).
003780    03  連メ３－メッセージ１           PIC X(20).
003790*
003990****************
003991* 画面入力情報 *
003992****************
003993**
003994 01 連入－画面情報ＹＪＫ４３０   IS EXTERNAL.
003995    03 連入－請求年月.
003996       05 連入－請求和暦               PIC 9.
003997       05 連入－請求年                 PIC 9(2).
003998       05 連入－請求月                 PIC 9(2).
003999    03 連入－提出年月日.
004000       05 連入－提出和暦               PIC 9.
004001       05 連入－提出年                 PIC 9(2).
004002       05 連入－提出月                 PIC 9(2).
004003       05 連入－提出日                 PIC 9(2).
004004    03 連入－レセプト種類              PIC X(4).
004005    03 連入－保険種別                  PIC 9(2).
004006    03 連入－印刷種類                  PIC 9.
004007    03 連入－本人家族                  PIC 9.
004008    03 連入－用紙種類                  PIC 9.
004009    03 連入－県内県外                  PIC 9.
004010    03 連入－県ＪＩＳ                  PIC X(2).
004011    03 連入－政管ＪＩＳ                PIC X(2).
004012*
004013*
004015 01 連入－画面情報ＹＪＫ４３０追加   IS EXTERNAL.
004016    03 連入－一括区分    PIC 9.
004020    03 連入－プレビュー区分            PIC 9.
004021*
004030************************************
004031* プリンタファイル作成用           *
004032************************************
004033 01 Ｈ連ＰＲＴＦ－作成データ IS EXTERNAL.
004034     03 Ｈ連ＰＲＴＦ－ファイル名           PIC X(8).
004035     03 Ｈ連ＰＲＴＦ－プレビュー区分       PIC 9.
004036     03 Ｈ連ＰＲＴＦ－帳票プログラム名     PIC X(8).
004037     03 Ｈ連ＰＲＴＦ－オーバレイ名         PIC X(8).
004038*
004039******************************************************************
004040*                      PROCEDURE  DIVISION                       *
004050******************************************************************
004060 PROCEDURE               DIVISION.
004070************
004080*           *
004090* 初期処理   *
004100*           *
004110************
004120     PERFORM プリンタファイル作成.
004121     PERFORM 初期化.
004130     PERFORM 制御情報取得２.
004140************
004150*           *
004160* 主処理     *
004170*           *
004180************
004190     PERFORM 印刷処理.
004200************
004210*           *
004220* 終了処理   *
004230*           *
004240************
004250     PERFORM 終了処理.
004260     EXIT PROGRAM.
004270*
004280*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
004290*================================================================*
004291 プリンタファイル作成 SECTION.
004292*================================================================*
004293*   / 初期化 /
004294     MOVE SPACE TO Ｈ連ＰＲＴＦ－作成データ.
004295     INITIALIZE Ｈ連ＰＲＴＦ－作成データ.
004296*
004297*
004298*--↓↓ 変更箇所 ↓↓--------------------------------------*
004299*   使用するプリンタファイル名セット
004300     MOVE "PRTF001"             TO Ｈ連ＰＲＴＦ－ファイル名.
004301*
004302*   使用する帳票プログラム名セット
004303     MOVE "YJK436"              TO Ｈ連ＰＲＴＦ－帳票プログラム名.
004304*
004305*--↑↑-----------------------------------------------------*
004306*
004307*   / プレビュー区分セット /
004308     MOVE 連入－プレビュー区分 TO Ｈ連ＰＲＴＦ－プレビュー区分.
004309*
004310     CALL   "CRTPRTF".
004311     CANCEL "CRTPRTF".
004312*
004313*================================================================*
004314 初期化 SECTION.
004315*
004320     PERFORM ファイルオープン.
004330*    /* 現在日付取得 */
004340     ACCEPT 計算機日付 FROM DATE.
004350*    /* 1980～2079年の間で設定 */
004360     IF 計算機年 > 80
004370         MOVE 19 TO 計算機世紀
004380     ELSE
004390         MOVE 20 TO 計算機世紀
004400     END-IF.
004410*
004420     PERFORM 連結項目退避.
004430     PERFORM 施術所情報取得.
004440     PERFORM カレント元号取得.
004450     PERFORM 和暦終了年取得.
004460***     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
004470*================================================================*
004480 カレント元号取得 SECTION.
004490*
004500     MOVE ZEROS TO 制－制御区分.
004510     READ 制御情報マスタ
004520     NOT INVALID KEY
004530         MOVE 制－カレント元号 TO カレント元号Ｗ
004540     END-READ.
004550*
004560*================================================================*
004570 和暦終了年取得 SECTION.
004580*
004590*     DISPLAY NC"カレント元号Ｗ"  カレント元号Ｗ UPON MSGBOX.
004600     MOVE カレント元号Ｗ TO 元－元号区分.
004610     READ 元号マスタ
004620     INVALID KEY
004630         DISPLAY NC"指定和暦が登録されていません" UPON CONS
004640         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
004650                                                  UPON CONS
004660         ACCEPT  キー入力 FROM CONS
004670         PERFORM 終了処理
004680         EXIT PROGRAM
004690     NOT INVALID KEY
004700         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
004710         MOVE 前和暦Ｗ TO 元－元号区分
004720         READ 元号マスタ
004730         INVALID KEY
004740             DISPLAY NC"指定和暦が登録されていません" UPON CONS
004750             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
004760                                                      UPON CONS
004770             ACCEPT  キー入力 FROM CONS
004780             PERFORM 終了処理
004790             EXIT PROGRAM
004800         NOT INVALID KEY
004810             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
004820         END-READ
004830     END-READ.
004840*
004850*================================================================*
004860 連結項目退避 SECTION.
004870*
004880     MOVE 連入－請求和暦  TO 請求和暦Ｗ.
004890     MOVE 連入－請求年    TO 請求年Ｗ.
004900     MOVE 連入－請求月    TO 請求月Ｗ.
004910     MOVE 連入－提出和暦  TO 提出和暦Ｗ.
004920     MOVE 連入－提出年    TO 提出年Ｗ.
004930     MOVE 連入－提出月    TO 提出月Ｗ.
004940     MOVE 連入－提出日    TO 提出日Ｗ.
004950     MOVE 連入－印刷種類  TO 印刷種類Ｗ.
004960*
004970*================================================================*
004980 施術所情報取得 SECTION.
004990*
005000     MOVE ZERO  TO 施情－施術所番号.
005010     READ 施術所情報マスタ
005020     INVALID KEY
005030         CONTINUE
005040     NOT INVALID KEY
005050*
005060         MOVE 施情－郵便番号１       TO 施術所郵便番号１Ｗ
005070         MOVE "-"                    TO 施術所郵便番号区切Ｗ
005080         MOVE 施情－郵便番号２       TO 施術所郵便番号２Ｗ
005090         MOVE 施情－代表者名         TO 代表者名Ｗ
005100         MOVE 施情－接骨院名         TO 接骨院名Ｗ
005110         STRING 施情－住所１  DELIMITED BY SPACE
005120                施情－住所２  DELIMITED BY SPACE
005130           INTO 施術所住所Ｗ
005140         END-STRING
005150         MOVE 施情－電話番号         TO 施術所電話番号Ｗ
005160         MOVE 施情－新柔整師番号     TO 柔整師番号Ｗ
005170*
005180         MOVE 施情－取引先銀行名     TO 取引先銀行名Ｗ
005190         MOVE 施情－取引先銀行支店名 TO 取引先銀行支店名Ｗ
005200         MOVE 施情－預金種別         TO 預金種別Ｗ
005210         MOVE 施情－銀行番号         TO 銀行番号Ｗ
005220         MOVE 施情－店番号           TO 店番号Ｗ
005230         MOVE 施情－口座番号         TO 口座番号Ｗ
005240         MOVE 施情－口座名義人カナ   TO 口座名義人カナＷ
005250         MOVE 施情－口座名義人       TO 口座名義人Ｗ
005260         STRING 取引先銀行名Ｗ     DELIMITED BY SPACE
005270                " "                DELIMITED BY SIZE
005280                取引先銀行支店名Ｗ DELIMITED BY SPACE
005290                INTO 銀行名支店名Ｗ
005300         END-STRING
005310         EVALUATE 預金種別Ｗ
005320         WHEN 1
005330             MOVE NC"普通" TO 預金種別コメントＷ
005340         WHEN 2
005350             MOVE NC"当座" TO 預金種別コメントＷ
005360         WHEN OTHER
005370             MOVE SPACE    TO 預金種別コメントＷ
005380         END-EVALUATE
005390*
005400     END-READ.
005410*================================================================*
005420 ファイルオープン SECTION.
005430*
005440     OPEN INPUT   元号マスタ
005450         MOVE NC"元号" TO ファイル名.
005460         PERFORM オープンチェック.
005470     OPEN INPUT  制御情報マスタ.
005480         MOVE NC"制御情報" TO ファイル名.
005490         PERFORM オープンチェック.
005500     OPEN INPUT   保険者マスタ
005510         MOVE NC"保険" TO ファイル名.
005520         PERFORM オープンチェック.
005530     OPEN INPUT   市町村マスタ
005540         MOVE NC"市町" TO ファイル名.
005550         PERFORM オープンチェック.
005590     OPEN INPUT   施術所情報マスタ
005600         MOVE NC"施情" TO ファイル名.
005610         PERFORM オープンチェック.
005620     OPEN INPUT   請求先マスタ
005630         MOVE NC"請先" TO ファイル名.
005640         PERFORM オープンチェック.
005650     OPEN INPUT   会情報マスタ
005660         MOVE NC"会情報" TO ファイル名.
005670         PERFORM オープンチェック.
005680     OPEN INPUT 保険者拡張マスタ.
005690         MOVE NC"保拡" TO ファイル名.
005700         PERFORM オープンチェック.
005730*================================================================*
005740 オープンチェック SECTION.
005750*
005760     IF 状態キー  NOT =  "00"
005770         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
005780         DISPLAY NC"状態キー：" 状態キー         UPON CONS
005790         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
005800                                                 UPON CONS
005810         ACCEPT  キー入力 FROM CONS
005820         PERFORM ファイル閉鎖
005830         EXIT PROGRAM.
005840*================================================================*
005850 ファイル閉鎖 SECTION.
005860*
005910     IF ( オープンフラグ = "YES" )
005911         CLOSE 印刷ファイル
005912     END-IF.
005913     CLOSE 元号マスタ     制御情報マスタ   保険者マスタ
005914           市町村マスタ   施術所情報マスタ 請求先マスタ  
005915           保険者拡張マスタ 会情報マスタ.
005916*================================================================*
005920 終了処理 SECTION.
005930*
005940     PERFORM ファイル閉鎖.
005950*================================================================*
005960 エラー表示 SECTION.
005970*
005980     DISPLAY NC"状態キー" 状態キー  UPON CONS.
005990     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
006000     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
006010     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
006020     ACCEPT  キー入力 FROM CONS.
006030     PERFORM ファイル閉鎖.
006040     EXIT PROGRAM.
006050*================================================================*
006060 印刷処理 SECTION.
006070*
006080     OPEN INPUT  作業ファイル２.
006090         MOVE NC"作２" TO ファイル名.
006100         PERFORM オープンチェック.
006110* / 副キー(印刷順序+保険種別+保険者番号)で読み込む./
006120     MOVE ZERO      TO  作２－請求和暦.
006130     MOVE ZERO      TO  作２－請求年.
006140     MOVE ZERO      TO  作２－請求月.
006150*     MOVE LOW-VALUE TO  作２－県.
006160     MOVE ZERO      TO  作２－印刷順序.
006170     MOVE ZERO      TO  作２－保険種別.
006180     MOVE LOW-VALUE TO  作２－保険者番号.
006190     START 作業ファイル２   KEY IS >=  作２－請求和暦年月
006200                                       作２－印刷順序
006210                                       作２－保険種別
006220                                       作２－保険者番号
006230*                                       作２－県
006240     END-START.
006250     IF 状態キー = "00"
006260         MOVE SPACE TO 終了フラグ
006270         PERFORM 作業ファイル２読込
006280         IF  終了フラグ = "YES"
006290             MOVE  NC"　該当データなしです。確認して下さい。" TO 連メ－メッセージ
006300             CALL   "MSG001"
006310             CANCEL "MSG001"
006320             PERFORM ファイル閉鎖
006330             MOVE 99 TO PROGRAM-STATUS
006340             EXIT PROGRAM
006350         END-IF
006360*
006370         PERFORM UNTIL  終了フラグ = "YES"
006380                PERFORM 印刷対象チェック
006390                IF 印刷フラグ = "YES"
006400                   MOVE SPACE TO YJK436P
006410****                INITIALIZE YJK436P
006420                   PERFORM ヘッダセット
006430                   PERFORM 明細セット
006440                   PERFORM 印字処理
006450                   PERFORM 改頁処理
006460                END-IF
006470                PERFORM 作業ファイル２読込
006480         END-PERFORM
006490*
006500     ELSE
006510         MOVE  NC"　該当データなしです。確認して下さい。" TO 連メ－メッセージ
006520         CALL   "MSG001"
006530         CANCEL "MSG001"
006540         PERFORM ファイル閉鎖
006550         MOVE 99 TO PROGRAM-STATUS
006560         EXIT PROGRAM
006570     END-IF.
006580*
006590     CLOSE 作業ファイル２.
006600*
006610*================================================================*
006620 作業ファイル２読込 SECTION.
006630*
006640     READ 作業ファイル２ NEXT
006650     AT END
006660         MOVE "YES" TO 終了フラグ
006670     END-READ.
006680*================================================================*
006690 印字処理  SECTION.
006700*
006701     IF ( オープンフラグ NOT = "YES" )
006702        MOVE "YES" TO オープンフラグ
006703        OPEN I-O  印刷ファイル
006704        PERFORM エラー処理Ｐ
006705     END-IF.
006706*
006710     MOVE "YJK436P" TO  定義体名Ｐ.
006720     MOVE SPACE     TO  処理種別Ｐ.
006730     MOVE "SCREEN"  TO  項目群名Ｐ.
006740     WRITE YJK436P.
006750     PERFORM エラー処理Ｐ.
006760*================================================================*
006770 改頁処理  SECTION.
006780*
006790     MOVE "YJK436P" TO  定義体名Ｐ.
006800     MOVE "CT"      TO  処理種別Ｐ.
006810     MOVE "PAGE"    TO  拡張制御Ｐ.
006820     MOVE SPACE     TO  項目群名Ｐ.
006830     WRITE YJK436P.
006840     PERFORM エラー処理Ｐ.
006850     MOVE SPACE     TO  拡張制御Ｐ.
006860*
006870     CLOSE  印刷ファイル.
           MOVE SPACE  TO オープンフラグ.
006880*     OPEN I-O   印刷ファイル.
006890*     PERFORM エラー処理Ｐ.
006900*
006910*================================================================*
006920 ヘッダセット SECTION.
006930*
006940* 当月の和暦を取得
006950     MOVE 請求和暦Ｗ         TO 元－元号区分.
006960     READ 元号マスタ
006970     INVALID KEY
006980         MOVE SPACE          TO 請求和暦名称
006990     NOT INVALID KEY
007000         MOVE 元－元号名称   TO 請求和暦名称
007010     END-READ.
007020*
007030     MOVE 請求年Ｗ           TO 請求年.
007040     MOVE 請求月Ｗ           TO 請求月.
007050     MOVE 代表者名Ｗ         TO 代表者名.
007060     MOVE 接骨院名Ｗ         TO 接骨院名.
007070     MOVE 柔整師番号Ｗ       TO 柔整師番号.
007080*
007090* / 保険者/
007100     MOVE 作２－保険種別     TO 保険種別Ｗ.
007110     IF (作２－保険種別 = 05) AND (作２－保険者番号(1:2) = "39")
007120         MOVE 作２－保険者番号(1:4) TO 保険者番号
007130     ELSE
007140         MOVE 作２－保険者番号      TO 保険者番号
007150     END-IF.
007160     MOVE 作２－保険者番号２ TO 保険者番号Ｗ.
007170     PERFORM 県名セット.
007180     PERFORM 保険種別名セット.
007190     EVALUATE 保険種別Ｗ
007200     WHEN 1 THRU 4
007210     WHEN 6 THRU 9
007220*     WHEN 70 
007230*     WHEN 80
007240         PERFORM 保険者情報取得
007250     WHEN 5
007260     WHEN 50 THRU 60
007270         PERFORM 市町村情報取得
007280     END-EVALUATE.
007290     PERFORM 結合宛名取得.
007300*
007310     MOVE 保険者宛名Ｗ  TO 保険者名称.
007320
007330*
007340*================================================================*
007350 明細セット SECTION.
007360*
007370     IF 作２－本人件数 NOT = ZERO
007380        MOVE 作２－本人件数     TO 本人件数
007390        MOVE 作２－本人費用額   TO 本人費用額
007400        MOVE 作２－本人負担額   TO 本人負担額
007410        MOVE 作２－本人請求額   TO 本人請求額
007420     END-IF.
007430     IF 作２－家族件数 NOT = ZERO
007440        MOVE 作２－家族件数     TO 家族件数
007450        MOVE 作２－家族費用額   TO 家族費用額
007460        MOVE 作２－家族負担額   TO 家族負担額
007470        MOVE 作２－家族請求額   TO 家族請求額
007480     END-IF.
007490*
007500*================================================================*
007510 エラー処理Ｐ SECTION.
007520*
007530     IF 通知情報Ｐ NOT = "00"
007540         DISPLAY NC"帳票エラー"              UPON CONS
007550         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
007560         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
007570         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
007580         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
007590         ACCEPT  キー入力 FROM CONS
007600         PERFORM ファイル閉鎖
007610         EXIT PROGRAM
007620     END-IF.
007630*================================================================*
007640 保険者情報取得 SECTION.
007650*
007660     MOVE  SPACE         TO 請求先名称Ｗ.
007670     MOVE  SPACE         TO 支部部署名Ｗ.
007680     MOVE  ZERO          TO 接尾語区分Ｗ.
007690*
007700     MOVE 保険種別Ｗ     TO 保－保険種別.
007710     MOVE 保険者番号Ｗ   TO 保－保険者番号.
007720     READ 保険者マスタ
007730     INVALID KEY
007740         MOVE SPACE      TO 請求先名称Ｗ
007750         MOVE SPACE      TO 支部部署名Ｗ
007760     NOT INVALID KEY
007770         IF 保－請求先情報区分 = 1
007780             MOVE 保－保険種別   TO 請先－保険種別
007790             MOVE 保－保険者番号 TO 請先－保険者番号
007800             READ 請求先マスタ
007810             INVALID KEY
007820                 MOVE SPACE             TO 請求先名称Ｗ
007830                 MOVE SPACE             TO 支部部署名Ｗ
007840             NOT INVALID KEY
007850                 MOVE 請先－保険者名称  TO 請求先名称Ｗ
007860                 MOVE 請先－支部部署名  TO 支部部署名Ｗ
007870             END-READ
007880         ELSE
007890             MOVE 保－保険者名称        TO 請求先名称Ｗ
007900             MOVE 保－支部部署名        TO 支部部署名Ｗ
007910             MOVE 保－接尾語区分        TO 接尾語区分Ｗ
007920         END-IF
007930     END-READ.
007940*================================================================*
007950 市町村情報取得 SECTION.
007960*
007970     MOVE  SPACE         TO 請求先名称Ｗ.
007980     MOVE  SPACE         TO 支部部署名Ｗ.
007990*
008000     MOVE 保険種別Ｗ               TO 市－公費種別.
008010     MOVE 保険者番号Ｗ             TO 市－市町村番号.
008020     READ 市町村マスタ
008030     INVALID KEY
008040         MOVE SPACE                TO 請求先名称Ｗ
008050         MOVE SPACE                TO 支部部署名Ｗ
008060     NOT INVALID KEY
008070         IF 市－請求先区分 = 1
008080             MOVE 保険種別Ｗ       TO 請先－保険種別
008090             MOVE 保険者番号Ｗ     TO 請先－保険者番号
008100             READ 請求先マスタ
008110             INVALID KEY
008120                 MOVE SPACE        TO 請求先名称Ｗ
008130                 MOVE SPACE        TO 支部部署名Ｗ
008140             NOT INVALID KEY
008150                 MOVE 請先－保険者名称   TO 請求先名称Ｗ
008160                 MOVE 請先－支部部署名   TO 支部部署名Ｗ
008170             END-READ
008180          ELSE
008190             MOVE 市－市町村名称   TO 請求先名称Ｗ
008200             MOVE 市－支部部署名   TO 支部部署名Ｗ
008210          END-IF
008220      END-READ.
008230*/後期高齢を代表番号でまとめる/080516
008240      IF 作２－保険者番号(1:2) = "39"
008250          MOVE SPACE TO 支部部署名Ｗ
008260      END-IF.
008270*================================================================*
008280 結合宛名取得 SECTION.
008290*
008300     MOVE SPACE TO 保険者宛名Ｗ.
008310     IF 請求先名称Ｗ NOT = SPACE
008320         EVALUATE 保険種別Ｗ
008330         WHEN 2
008340             IF 接尾語区分Ｗ = 1
008350                MOVE SPACE            TO 宛名Ｗ
008360             ELSE
008370                MOVE "社会保険事務所" TO 宛名Ｗ
008380             END-IF
008390         WHEN 6
008400             IF 接尾語区分Ｗ = 1
008410                MOVE "（日雇）"               TO 宛名Ｗ
008420             ELSE
008430                MOVE "社会保険事務所（日雇）" TO 宛名Ｗ
008440             END-IF
008450         WHEN 7
008460             MOVE "（船員）"       TO 宛名Ｗ
008470         WHEN 3
008480             MOVE "健康保険組合"   TO 宛名Ｗ
008490         WHEN 4
008500             MOVE "共済組合"       TO 宛名Ｗ
008510         WHEN 8
008520             MOVE "（退職）"       TO 宛名Ｗ
008530         WHEN OTHER
008540             MOVE SPACE            TO 宛名Ｗ
008550         END-EVALUATE
008560*
008570         IF 支部部署名Ｗ = SPACE
008580             STRING  請求先名称Ｗ  DELIMITED BY SPACE
008590                     宛名Ｗ        DELIMITED BY SPACE
008600                     "  殿"        DELIMITED BY SIZE
008610                    INTO 保険者宛名Ｗ
008620             END-STRING
008630         ELSE
008640             STRING  請求先名称Ｗ  DELIMITED BY SPACE
008650                     宛名Ｗ        DELIMITED BY SPACE
008660                     " "           DELIMITED BY SIZE
008670                     支部部署名Ｗ  DELIMITED BY SPACE
008680                     "  殿"        DELIMITED BY SIZE
008690                    INTO 保険者宛名Ｗ
008700             END-STRING
008710         END-IF
008720     END-IF.
008730*
008740*================================================================*
008750 制御情報取得２ SECTION.
008760* 初期値設定
008770       MOVE ZERO  TO 国保７号印刷区分Ｗ. 
008780       MOVE ZERO  TO 社保７号印刷区分Ｗ. 
008790       MOVE ZERO  TO 組合７号印刷区分Ｗ. 
008800       MOVE ZERO  TO 共済７号印刷区分Ｗ. 
008810       MOVE ZERO  TO 老人７号印刷区分Ｗ. 
008820       MOVE ZERO  TO 助成７号印刷区分Ｗ. 
008830*
008840*================================================================*
008850 印刷対象チェック  SECTION.
008860*
008870*  印刷区分による振り分け → ６・７号請求書（印刷区分 0:印刷 1:印刷しない）
008880* （一括印刷のみ）
008890* 7号用紙では、社保、国保、退職、老人、助成、組合、共済の印刷を行う
008900*
008910     MOVE SPACE TO 印刷フラグ.
008920*
008930     IF 連入－一括区分 NOT = 1
008940        MOVE "YES" TO 印刷フラグ
008950     ELSE
008960        EVALUATE 作２－保険種別
008970        WHEN 01
008980        WHEN 08
008990           IF 国保７号印刷区分Ｗ NOT = 1
009000              MOVE "YES" TO 印刷フラグ
009010           END-IF
009020        WHEN 02
009030        WHEN 06
009040        WHEN 07
009050           IF 社保７号印刷区分Ｗ NOT = 1
009060              MOVE "YES" TO 印刷フラグ
009070           END-IF
009080        WHEN 03
009090           IF 組合７号印刷区分Ｗ NOT = 1
009100              MOVE "YES" TO 印刷フラグ
009110           END-IF
009120        WHEN 04
009130        WHEN 09
009140           IF 共済７号印刷区分Ｗ NOT = 1
009150              MOVE "YES" TO 印刷フラグ
009160           END-IF
009170        WHEN 05
009180           IF 老人７号印刷区分Ｗ NOT = 1
009190              MOVE "YES" TO 印刷フラグ
009200           END-IF
009210* 生活保護は除く
009220        WHEN 51 THRU 60
009230           IF 助成７号印刷区分Ｗ NOT = 1
009240              MOVE "YES" TO 印刷フラグ
009250           END-IF
009260        WHEN OTHER
009270           MOVE "YES" TO 印刷フラグ
009280        END-EVALUATE
009290     END-IF.
009300*
009310*/東京都の後高は印刷しない(別用紙)/080516
009320     EVALUATE TRUE
009330     WHEN (作２－保険種別 = 05) AND (作２－保険者番号(3:2) = "13")
009340         MOVE SPACE TO 印刷フラグ
009350     END-EVALUATE.
009360*================================================================*
009370 県名セット SECTION.
009380*
009390     EVALUATE  作２－県
009400*北海道
009410      WHEN  01
009420         MOVE NC"北海道" TO 都道府県名
009430*青森
009440      WHEN  02
009450         MOVE NC"青森県" TO 都道府県名
009460*岩手
009470      WHEN  03
009480         MOVE NC"岩手県" TO 都道府県名
009490*宮城
009500      WHEN  04
009510         MOVE NC"宮城県" TO 都道府県名
009520*秋田
009530      WHEN  05
009540         MOVE NC"秋田県" TO 都道府県名
009550*山形
009560      WHEN  06
009570         MOVE NC"山形県" TO 都道府県名
009580*福島
009590      WHEN  07
009600         MOVE NC"福島県" TO 都道府県名
009610*茨城
009620      WHEN  08
009630         MOVE NC"茨城県" TO 都道府県名
009640*栃木
009650      WHEN  09
009660         MOVE NC"栃木県" TO 都道府県名
009670*群馬
009680      WHEN  10
009690         MOVE NC"群馬県" TO 都道府県名
009700*埼玉
009710      WHEN  11
009720         MOVE NC"埼玉県" TO 都道府県名
009730*千葉
009740      WHEN  12
009750         MOVE NC"千葉県" TO 都道府県名
009760*東京
009770      WHEN  13
009780         MOVE NC"東京都" TO 都道府県名
009790*神奈川
009800      WHEN  14
009810         MOVE NC"神奈川県" TO 都道府県名
009820*新潟
009830      WHEN  15
009840         MOVE NC"新潟県" TO 都道府県名
009850*富山
009860      WHEN  16
009870         MOVE NC"富山県" TO 都道府県名
009880*石川
009890      WHEN  17
009900         MOVE NC"石川県" TO 都道府県名
009910*福井
009920      WHEN  18
009930         MOVE NC"福井県" TO 都道府県名
009940*山梨
009950      WHEN  19
009960         MOVE NC"山梨県" TO 都道府県名
009970*長野
009980      WHEN  20
009990         MOVE NC"長野県" TO 都道府県名
010000*岐阜
010010      WHEN  21
010020         MOVE NC"岐阜県" TO 都道府県名
010030*静岡
010040      WHEN  22
010050         MOVE NC"静岡県" TO 都道府県名
010060*愛知
010070      WHEN  23
010080         MOVE NC"愛知県" TO 都道府県名
010090*三重
010100      WHEN  24
010110         MOVE NC"三重県" TO 都道府県名
010120*滋賀
010130      WHEN  25
010140         MOVE NC"滋賀県" TO 都道府県名
010150*京都
010160      WHEN  26
010170         MOVE NC"京都府" TO 都道府県名
010180*大阪
010190      WHEN  27
010200         MOVE NC"大阪府" TO 都道府県名
010210*兵庫
010220      WHEN  28
010230         MOVE NC"兵庫県" TO 都道府県名
010240*奈良
010250      WHEN  29
010260         MOVE NC"奈良県" TO 都道府県名
010270*和歌山
010280      WHEN  30
010290         MOVE NC"和歌山県" TO 都道府県名
010300*鳥取
010310      WHEN  31
010320         MOVE NC"鳥取県" TO 都道府県名
010330*島根
010340      WHEN  32
010350         MOVE NC"島根県" TO 都道府県名
010360*岡山
010370      WHEN  33
010380         MOVE NC"岡山県" TO 都道府県名
010390*広島
010400      WHEN  34
010410         MOVE NC"広島県" TO 都道府県名
010420*山口
010430      WHEN  35
010440         MOVE NC"山口県" TO 都道府県名
010450*徳島
010460      WHEN  36
010470         MOVE NC"徳島県" TO 都道府県名
010480*香川
010490      WHEN  37
010500         MOVE NC"香川県" TO 都道府県名
010510*愛媛
010520      WHEN  38
010530         MOVE NC"愛媛県" TO 都道府県名
010540*高知
010550      WHEN  39
010560         MOVE NC"高知県" TO 都道府県名
010570*福岡
010580      WHEN  40
010590         MOVE NC"福岡県" TO 都道府県名
010600*佐賀
010610      WHEN  41
010620         MOVE NC"佐賀県" TO 都道府県名
010630*長崎
010640      WHEN  42
010650         MOVE NC"長崎県" TO 都道府県名
010660*熊本
010670      WHEN  43
010680         MOVE NC"熊本県" TO 都道府県名
010690*大分
010700      WHEN  44
010710         MOVE NC"大分県" TO 都道府県名
010720*宮崎
010730      WHEN  45
010740         MOVE NC"宮崎県" TO 都道府県名
010750*鹿児島
010760      WHEN  46
010770         MOVE NC"鹿児島県" TO 都道府県名
010780*沖縄
010790      WHEN  47
010800         MOVE NC"沖縄県" TO 都道府県名
010810*その他
010820      WHEN  OTHER
010830         CONTINUE
010840     END-EVALUATE.
010850*================================================================*
010860 保険種別名セット SECTION.
010870*
010880**///  印刷順序の変更はこのSECTIONで   ////**
010890*
010900*************************************************************************
010910*  印刷順序:保険名称(保険種別コード)                                    *
010920*                                                                       *
010930*    10:社保(2)                                                         *
010940*    11:船員(7)                                                         *
010950*    12:日雇(6)                                                         *
010960*    50:国保(1)                                                         *
010970*    51:国保組合(1)                                                     *
010980*    52:退職(8)                                                         *
010990*    53:老人(5)                                                         *
011000*    60:４１老人(51)                                                    *
011010*    61:母子(52)                                                        *
011020*    62:乳幼児(55)                                                      *
011030*    63:障害(53)                                                        *
011040*    64:被爆(54)                                                        *
011050*    65:その他(60)                                                      *
011060*    80:組合(3)                                                         *
011070*    90:共済(4)                                                         *
011080*    91:自衛官(9)                                                       *
011090*                                                                       *
011100*   労災、自賠責、自費はのぞく                                          *
011110*                                                                       *
011120*                                                                       *
011130*************************************************************************
011140*
011150*
011160      EVALUATE  作２－印刷順序
011170* 国保
011180      WHEN  050
011190          MOVE NC"国民保険" TO 保険種別名
011200* 国保組合条件
011210      WHEN  051
011220          MOVE NC"国保組合" TO 保険種別名
011230* 社保
011240      WHEN  010
011250*          MOVE NC"社会保険" TO 保険種別名
011260          MOVE NC"協保" TO 保険種別名
011270* 船員は社保で表示
011280*      WHEN  011
011290      WHEN  020
011300          MOVE NC"社会保険" TO 保険種別名
011310* 日雇は社保で表示
011320      WHEN  012
011330*          MOVE NC"社会保険" TO 保険種別名
011340          MOVE NC"協保" TO 保険種別名
011350* 組合
011360      WHEN  080
011370          MOVE NC"健保組合" TO 保険種別名
011380* 共済
011390      WHEN  090
011400          MOVE NC"共済組合" TO 保険種別名
011410* 自衛官は共済組合で表示
011420      WHEN  091
011430          MOVE NC"共済組合" TO 保険種別名
011440* ２７老人
011450      WHEN  053
011460          IF 作２－保険者番号(1:2) = "39"
011470              MOVE NC"後期"     TO 保険種別名
011480          ELSE
011490              MOVE NC"老健老人" TO 保険種別名
011500          END-IF
011510* 退職国保
011520      WHEN  052
011530          MOVE NC"退職国保" TO 保険種別名
011540* ４１老人
011550      WHEN  060
011560          MOVE NC"公費老人" TO 保険種別名
011570* 母子
011580      WHEN  061
011590          MOVE NC"母子家庭" TO 保険種別名
011600* 乳幼児
011610      WHEN  062
011620          MOVE NC"乳幼児" TO 保険種別名
011630* 障害
011640      WHEN  063
011650          MOVE NC"障害者" TO 保険種別名
011660* 被爆
011670      WHEN  064
011680          MOVE NC"被爆者" TO 保険種別名
011690* その他
011700* 東京の８８１３のみ「子」を印字し、それ以外は空白とする
011710      WHEN  065
011720          EVALUATE 作２－保険者番号(1:4)
011730          WHEN "8813"
011740              MOVE NC"子" TO 保険種別名
011750          WHEN OTHER
011760              MOVE SPACE TO 保険種別名
011770          END-EVALUATE
011780      WHEN OTHER
011790          MOVE SPACE TO 保険種別名
011800     END-EVALUATE.
011810*
011820*================================================================*
011830******************************************************************
011840 END PROGRAM YJK436.
011850******************************************************************
