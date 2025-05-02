000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YJK437.
000060 AUTHOR.                 岡田　憲和
000070*
000080*----------------------------------------------------------------*
000090* 日本柔整共済会用 ６号用紙 請求書【印刷】柔ｳｨﾝﾄﾞｳｽﾞ95版
000100*         MED = YJK437P
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
000390     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000400                             ORGANIZATION             IS  INDEXED
000410                             ACCESS MODE              IS  DYNAMIC
000420                             RECORD KEY               IS  名－区分コード
000430                                                          名－名称コード
000440                             FILE STATUS              IS  状態キー
000450                             LOCK        MODE         IS  AUTOMATIC.
000460     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000470                             ORGANIZATION             IS  INDEXED
000480                             ACCESS MODE              IS  DYNAMIC
000490                             RECORD KEY               IS 施情－施術所番号
000500                             FILE STATUS              IS  状態キー
000510                             LOCK        MODE         IS  AUTOMATIC.
000520     SELECT  保険者マスタ    ASSIGN      TO        HOKENSL
000530                             ORGANIZATION             IS  INDEXED
000540                             ACCESS MODE              IS  DYNAMIC
000550                             RECORD KEY               IS  保－保険種別
000560                                                          保－保険者番号
000570                             ALTERNATE RECORD KEY     IS  保－保険種別
000580                                                          保－保険者名称
000590                                                          保－保険者番号
000600                             FILE STATUS              IS  状態キー
000610                             LOCK        MODE         IS  AUTOMATIC.
000620     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
000630                             ORGANIZATION             IS  INDEXED
000640                             ACCESS MODE              IS  DYNAMIC
000650                             RECORD KEY               IS  市－公費種別
000660                                                          市－市町村番号
000670                             ALTERNATE RECORD KEY     IS  市－公費種別
000680                                                          市－市町村名称
000690                                                          市－市町村番号
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
000924     SELECT  作業ファイル２  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W4312L.DAT"
000925                             ORGANIZATION             IS  INDEXED
000930                             ACCESS                   IS  DYNAMIC
000940                             RECORD      KEY          IS  作２－請求和暦年月
000950                                                          作２－保険種別
000960                                                          作２－保険者番号
000970                             ALTERNATE RECORD KEY     IS  作２－請求和暦年月
000980                                                          作２－県
000990                                                          作２－印刷順序
001000                                                          作２－保険種別
001010                                                          作２－保険者番号
001020                             ALTERNATE RECORD KEY     IS  作２－請求和暦年月
001030                                                          作２－印刷順序
001040                                                          作２－保険種別
001050                                                          作２－保険者番号
001060*                                                          作２－県
001070*/県別、社保日雇いをまとめ、船員を別に集計/081021
001080                             ALTERNATE RECORD KEY     IS  作２－請求和暦年月
001090                                                          作２－県２
001100                                                          作２－印刷順序
001110                                                          作２－保険種別
001120                                                          作２－保険者番号
001130                             FILE        STATUS       IS  状態キー
001140                             LOCK        MODE         IS  AUTOMATIC.
001150     SELECT  印刷ファイル    ASSIGN      TO         GS-PRTF001
001160                             SYMBOLIC    DESTINATION  IS "PRT"
001170                             FORMAT                   IS  定義体名Ｐ
001180                             GROUP                    IS  項目群名Ｐ
001190                             PROCESSING  MODE         IS  処理種別Ｐ
001200                             UNIT        CONTROL      IS  拡張制御Ｐ
001210                             FILE        STATUS       IS  通知情報Ｐ.
001220*
001230******************************************************************
001240*                      DATA DIVISION                             *
001250******************************************************************
001260 DATA                    DIVISION.
001270 FILE                    SECTION.
001280*                           ［ＲＬ＝  １２８］
001290 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001300     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001310*                           ［ＲＬ＝  ２５６］
001320 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001330     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
001340     COPY SEIGYO01        OF  XFDLIB  JOINING   制０１   AS  PREFIX.
001350*                           ［ＲＬ＝  １２８］
001360 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
001370     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
001380*                           ［ＲＬ＝  １２８］
001390 FD  施術所情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001400     COPY SEJOHO          OF  XFDLIB  JOINING   施情   AS  PREFIX.
001410*                           ［ＲＬ＝  ３２０］
001420 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001430     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
001440*                           ［ＲＬ＝  ２５６］
001450 FD  市町村マスタ          BLOCK   CONTAINS   1   RECORDS.
001460     COPY SITYOSN        OF  XFDLIB  JOINING   市   AS  PREFIX.
001470*                           ［ＲＬ＝  １２８］
001480 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
001490     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
001500*                           ［ＲＬ＝  ６４０］
001510 FD  会情報マスタ        BLOCK   CONTAINS   1   RECORDS.
001520     COPY KAIJOHO         OF  XFDLIB  JOINING   会情 AS  PREFIX.
001530*
001540*
001550* 施術/請求  (請求和暦年月は、施術/請求 共用) 
001560*                           ［ＲＬ＝  １２８］
001570 FD  作業ファイル２ RECORD  CONTAINS 128 CHARACTERS.
001580 01  作２－レコード.
001590     03  作２－レコードキー.
001600         05  作２－請求和暦年月.
001610             07  作２－請求和暦              PIC 9.
001620             07  作２－請求年                PIC 9(2).
001630             07  作２－請求月                PIC 9(2).
001640         05  作２－保険種別                  PIC 9(2).
001650         05  作２－保険者番号                PIC X(10).
001660         05  作２－県                        PIC X(2).
001670         05  作２－印刷順序.
001680             07  作２－印刷順序１            PIC 9(2).
001690             07  作２－印刷順序２            PIC 9.
001700     03  作２－レコードデータ.
001710         05  作２－保険者番号２              PIC X(10).
001720         05  作２－件数                      PIC 9(4).
001730         05  作２－費用額                    PIC 9(9).
001740         05  作２－負担額                    PIC 9(9).
001750         05  作２－請求額                    PIC 9(9).
001760         05  作２－本人件数                  PIC 9(3).
001770         05  作２－本人費用額                PIC 9(7).
001780         05  作２－本人負担額                PIC 9(7).
001790         05  作２－本人請求額                PIC 9(7).
001800         05  作２－家族件数                  PIC 9(3).
001810         05  作２－家族費用額                PIC 9(7).
001820         05  作２－家族負担額                PIC 9(7).
001830         05  作２－家族請求額                PIC 9(7).
001840         05  作２－県２                      PIC X(2).
001850         05  FILLER                          PIC X(15).
001860*         05  FILLER                          PIC X(17).
001870* 
001880 FD  印刷ファイル.
001890     COPY YJK437P         OF  XMDLIB.
001900*
001910******************************************************************
001920*                WORKING-STORAGE SECTION                         *
001930******************************************************************
001940 WORKING-STORAGE         SECTION.
001950 01 キー入力                           PIC X    VALUE SPACE.
001960 01 状態キー                           PIC X(2) VALUE SPACE.
001970 01 終了フラグ                         PIC X(3) VALUE SPACE.
001980 01 終了フラグ１                       PIC X(3) VALUE SPACE.
001990 01 確認入力Ｗ                         PIC X(1) VALUE SPACE.
002000 01 ファイル名Ｗ                       PIC N(6) VALUE SPACE.
002010 01 カレント元号Ｗ                     PIC 9(1) VALUE ZERO.
002020 01 実行キーＷ                         PIC X(4) VALUE SPACE.
002030 01 地区Ｗ                             PIC X(2) VALUE SPACE.
002040 01 公費番号Ｗ                         PIC X(8) VALUE SPACE.
002050 01 前和暦Ｗ                           PIC 9    VALUE ZERO.
002060 01 行桁フラグ                         PIC X(3) VALUE SPACE.
002070 01 検索対象フラグ                     PIC X(4) VALUE SPACE.
002080 01 ファイル名                         PIC N(4) VALUE SPACE.
002090 01 処理移動キー                       PIC X(4) VALUE SPACE.
002100 01 検索確認区分Ｗ                     PIC 9    VALUE ZERO.
002110 01 検索通過キーＷ                     PIC X(3) VALUE SPACE.
002120 01 行カウンタ                         PIC 9(2) VALUE ZERO.
002130 01 合計行カウンタ                     PIC 9(3) VALUE ZERO.
002140 01 ページカウンタ                     PIC 9(3) VALUE ZERO.
002150 01 印刷フラグ                         PIC X(3) VALUE SPACE.
002160 01 スキップフラグ                     PIC X(3) VALUE SPACE.
002170 01 オープンフラグ                     PIC X(3) VALUE SPACE.
002171*
002180 01 合計用Ｗ.
002190    03 本人件数カウント                PIC 9(9) VALUE ZERO.
002200    03 家族件数カウント                PIC 9(9) VALUE ZERO.
002210    03 計件数カウント                  PIC 9(9) VALUE ZERO.
002220    03 本人費用額カウント              PIC 9(9) VALUE ZERO.
002230    03 家族費用額カウント              PIC 9(9) VALUE ZERO.
002240    03 計費用額カウント                PIC 9(9) VALUE ZERO.
002250    03 本人請求額カウント              PIC 9(9) VALUE ZERO.
002260    03 家族請求額カウント              PIC 9(9) VALUE ZERO.
002270    03 計請求額カウント                PIC 9(9) VALUE ZERO.
002280 01 県Ｗ                               PIC X(2) VALUE SPACE.
002290 01 印刷順序Ｗ                         PIC 9(3) VALUE ZERO.
002300 01 頁Ｗ                               PIC 9(2) VALUE ZERO.
002310*
002320 01 保険者名称Ｗ.
002330    03 保険者名称１Ｗ                  PIC X(20) VALUE SPACE.
002340    03 保険者名称２Ｗ                  PIC X(20) VALUE SPACE.
002350    03 保険者名称３Ｗ                  PIC X(20) VALUE SPACE.
002360*
002370 01 件数ＷＫ                           PIC 9(9) VALUE ZERO.
002380 01 費用ＷＫ                           PIC 9(9) VALUE ZERO.
002390 01 請求ＷＫ                           PIC 9(9) VALUE ZERO.
002400 01 全角空白                           PIC X(2)  VALUE X"8140".
002410 01 半角空白                           PIC X(2)  VALUE X"2020".
002420*
002430 01 施術和暦年月Ｗ.
002440     03 施術和暦Ｗ                     PIC 9    VALUE ZERO.
002450     03 施術年月Ｗ.
002460        05 施術年Ｗ                    PIC 9(2) VALUE ZERO.
002470        05 施術月Ｗ                    PIC 9(2) VALUE ZERO.
002480        05 施術日Ｗ                    PIC 9(2) VALUE ZERO.
002490     03 受理年月Ｗ.
002500        05 受理年Ｗ                    PIC 9(2) VALUE ZERO.
002510        05 受理月Ｗ                    PIC 9(2) VALUE ZERO.
002520        05 受理日Ｗ                    PIC 9(2) VALUE ZERO.
002530***
002540***
002550 01 画面情報４３０Ｗ.
002560    03 請求年月Ｗ.
002570       05 請求和暦Ｗ                   PIC 9     VALUE ZERO.
002580       05 請求年Ｗ                     PIC 9(2)  VALUE ZERO.
002590       05 請求月Ｗ                     PIC 9(2)  VALUE ZERO.
002600    03 提出年月日Ｗ.
002610       05 提出和暦Ｗ                   PIC 9     VALUE ZERO.
002620       05 提出年Ｗ                     PIC 9(2)  VALUE ZERO.
002630       05 提出月Ｗ                     PIC 9(2)  VALUE ZERO.
002640       05 提出日Ｗ                     PIC 9(2)  VALUE ZERO.
002650    03 印刷種類Ｗ                      PIC 9     VALUE ZERO.
002660*
002670**************
002680* 施術所情報 *
002690**************
002700 01 施術所情報Ｗ.
002710    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
002720    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
002730    03 柔整師番号Ｗ                    PIC X(20)  VALUE SPACE.
002740    03 施術所住所Ｗ.
002750       05 施術所住所１Ｗ               PIC X(40)  VALUE SPACE.
002760       05 施術所住所２Ｗ               PIC X(40)  VALUE SPACE.
002770    03 施術所郵便番号Ｗ.
002780       05 施術所郵便番号１Ｗ           PIC X(3)   VALUE SPACE.
002790       05 施術所郵便番号区切Ｗ         PIC X(1)   VALUE SPACE.
002800       05 施術所郵便番号２Ｗ           PIC X(4)   VALUE SPACE.
002810    03 施術所電話番号Ｗ                PIC X(15)  VALUE SPACE.
002820    03 取引先情報Ｗ.
002830        05 取引先銀行名Ｗ              PIC X(40)  VALUE SPACE.
002840        05 取引先銀行支店名Ｗ          PIC X(40)  VALUE SPACE.
002850        05 預金種別Ｗ                  PIC 9(1)   VALUE ZERO.
002860        05 銀行番号Ｗ                  PIC X(4)   VALUE SPACE.
002870        05 店番号Ｗ                    PIC X(3)   VALUE SPACE.
002880        05 口座番号Ｗ                  PIC X(10)  VALUE SPACE.
002890        05 口座名義人カナＷ            PIC X(40)  VALUE SPACE.
002900        05 口座名義人Ｗ                PIC X(40)  VALUE SPACE.
002910*
002920 01 連番Ｗ                             PIC 9(3)   VALUE ZERO.
002930 01 銀行名支店名Ｗ                     PIC X(40)  VALUE SPACE.
002940 01 預金種別コメントＷ                 PIC N(2)   VALUE SPACE.
002950 01 控えＷ                             PIC N(4)   VALUE SPACE.
002960*
002970 01 保険種別Ｗ                         PIC 9(2)  VALUE ZERO.
002980 01 保険者番号Ｗ                       PIC X(10) VALUE SPACE.
002990 01 請求先名称Ｗ                       PIC X(40) VALUE SPACE.
003000 01 支部部署名Ｗ                       PIC X(40) VALUE SPACE.
003010 01 宛名Ｗ                             PIC X(24) VALUE SPACE.
003020 01 保険者宛名Ｗ.
003030     03 保険者宛名１Ｗ                 PIC X(40) VALUE SPACE.
003040     03 保険者宛名２Ｗ                 PIC X(40) VALUE SPACE.
003050*
003060* 社保用
003070 01 接尾語区分Ｗ                       PIC 9     VALUE ZERO.
003080*
003090 01 協会コードＷ                       PIC 9(2)  VALUE ZERO.
003100***
003110* 請求書印刷パラメタ用
003120*  ６・７号請求書（印刷区分 0:印刷 1:印刷しない、振込先 0:自分 1:会 9:印刷しない）
003130*  当社用紙初期値1:印刷しない
003140***
003150 01 請求書関連Ｗ.
003160        07 国保６号印刷区分Ｗ          PIC 9 VALUE ZERO.
003170        07 国保７号印刷区分Ｗ          PIC 9 VALUE ZERO.
003180        07 国保当社印刷区分Ｗ          PIC 9 VALUE 1.
003190        07 国保振込先区分Ｗ            PIC 9 VALUE ZERO.
003200        07 社保６号印刷区分Ｗ          PIC 9 VALUE ZERO.
003210        07 社保７号印刷区分Ｗ          PIC 9 VALUE ZERO.
003220        07 社保当社印刷区分Ｗ          PIC 9 VALUE 1.
003230        07 社保振込先区分Ｗ            PIC 9 VALUE ZERO.
003240        07 組合６号印刷区分Ｗ          PIC 9 VALUE 1.
003250        07 組合７号印刷区分Ｗ          PIC 9 VALUE ZERO.
003260        07 組合当社印刷区分Ｗ          PIC 9 VALUE 1.
003270        07 組合振込先区分Ｗ            PIC 9 VALUE ZERO.
003280        07 共済６号印刷区分Ｗ          PIC 9 VALUE 1.
003290        07 共済７号印刷区分Ｗ          PIC 9 VALUE ZERO.
003300        07 共済当社印刷区分Ｗ          PIC 9 VALUE 1.
003310        07 共済振込先区分Ｗ            PIC 9 VALUE ZERO.
003320        07 老人６号印刷区分Ｗ          PIC 9 VALUE ZERO.
003330        07 老人７号印刷区分Ｗ          PIC 9 VALUE ZERO.
003340        07 老人当社印刷区分Ｗ          PIC 9 VALUE 1.
003350        07 老人振込先区分Ｗ            PIC 9 VALUE ZERO.
003360        07 助成６号印刷区分Ｗ          PIC 9 VALUE 1.
003370        07 助成７号印刷区分Ｗ          PIC 9 VALUE 1.
003380        07 助成当社印刷区分Ｗ          PIC 9 VALUE 1.
003390        07 助成振込先区分Ｗ            PIC 9 VALUE ZERO.
003400*
003410*********************************************************************
003420 01 印刷制御.
003430     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
003440     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
003450     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
003460     03 拡張制御Ｐ.
003470         05 端末制御Ｐ.
003480             07 移動方向Ｐ             PIC X(1) VALUE SPACE.
003490             07 移動行数Ｐ             PIC 9(3) VALUE ZERO.
003500         05 詳細制御Ｐ                 PIC X(2) VALUE SPACE.
003510     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
003520     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
003530*********************************************************************
003540*
003550 01 計算機西暦年Ｗ                     PIC 9(2).
003560* 日付ＷＯＲＫ
003570 01 和暦終了年Ｗ                       PIC 9(4).
003580 01 計算機和暦年Ｗ                     PIC 9(2).
003590 01 計算機西暦.
003600    03 計算機西暦年                    PIC 9(4).
003610    03 計算機西暦月日                  PIC 9(4).
003620 01 計算機西暦Ｒ REDEFINES 計算機西暦.
003630    03 計算機世紀                      PIC 9(2).
003640    03 計算機日付                      PIC 9(6).
003650    03 計算機日付Ｒ REDEFINES 計算機日付.
003660       05 計算機年月                   PIC 9(4).
003670       05 計算機年月Ｒ REDEFINES 計算機年月.
003680         07 計算機年                   PIC 9(2).
003690         07 計算機月                   PIC 9(2).
003700       05 計算機日                     PIC 9(2).
003710*
003720******************************************************************
003730*                          連結項目                              *
003740******************************************************************
003750*
003760********************
003770* メッセージ表示キー *
003780********************
003790 01 連メ－キー IS EXTERNAL.
003800    03  連メ－メッセージ                 PIC N(20).
003810*
003820*
003830 01 連入－画面情報ＹＪＫ４３０   IS EXTERNAL.
003840    03 連入－請求年月.
003850       05 連入－請求和暦               PIC 9.
003860       05 連入－請求年                 PIC 9(2).
003870       05 連入－請求月                 PIC 9(2).
003880    03 連入－提出年月日.
003890       05 連入－提出和暦               PIC 9.
003900       05 連入－提出年                 PIC 9(2).
003910       05 連入－提出月                 PIC 9(2).
003920       05 連入－提出日                 PIC 9(2).
003930    03 連入－レセプト種類              PIC X(4).
003940    03 連入－保険種別                  PIC 9(2).
003950    03 連入－印刷種類                  PIC 9.
003960    03 連入－本人家族                  PIC 9.
003970    03 連入－用紙種類                  PIC 9.
003980    03 連入－県内県外                  PIC 9.
003990    03 連入－県ＪＩＳ                  PIC X(2).
004000    03 連入－政管ＪＩＳ                PIC X(2).
004010*
004020 01 連入－画面情報ＹＪＫ４３０追加   IS EXTERNAL.
004030    03 連入－一括区分    PIC 9.
004050    03 連入－プレビュー区分            PIC 9.
004051*
004052************************************
004053* プリンタファイル作成用           *
004054************************************
004055 01 Ｈ連ＰＲＴＦ－作成データ IS EXTERNAL.
004056     03 Ｈ連ＰＲＴＦ－ファイル名           PIC X(8).
004057     03 Ｈ連ＰＲＴＦ－プレビュー区分       PIC 9.
004058     03 Ｈ連ＰＲＴＦ－帳票プログラム名     PIC X(8).
004059     03 Ｈ連ＰＲＴＦ－オーバレイ名         PIC X(8).
004060*
004061******************************************************************
004062*                      PROCEDURE  DIVISION                       *
004070******************************************************************
004080 PROCEDURE               DIVISION.
004090************
004100*           *
004110* 初期処理   *
004120*           *
004130************
004140     PERFORM プリンタファイル作成.
004141     PERFORM 初期化.
004150     PERFORM 制御情報取得２.
004160************
004170*           *
004180* 主処理     *
004190*           *
004200************
004210     PERFORM 印刷処理.
004220************
004230*           *
004240* 終了処理   *
004250*           *
004260************
004270     PERFORM 終了処理.
004280     EXIT PROGRAM.
004290*
004300*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
004310*================================================================*
004311 プリンタファイル作成 SECTION.
004312*================================================================*
004313*   / 初期化 /
004314     MOVE SPACE TO Ｈ連ＰＲＴＦ－作成データ.
004315     INITIALIZE Ｈ連ＰＲＴＦ－作成データ.
004316*
004317*
004318*--↓↓ 変更箇所 ↓↓--------------------------------------*
004319*   使用するプリンタファイル名セット
004320     MOVE "PRTF001"             TO Ｈ連ＰＲＴＦ－ファイル名.
004321*
004322*   使用する帳票プログラム名セット
004323     MOVE "YJK437"              TO Ｈ連ＰＲＴＦ－帳票プログラム名.
004324*
004325*--↑↑-----------------------------------------------------*
004326*
004327*   / プレビュー区分セット /
004328     MOVE 連入－プレビュー区分 TO Ｈ連ＰＲＴＦ－プレビュー区分.
004329*
004330     CALL   "CRTPRTF".
004331     CANCEL "CRTPRTF".
004332*
004333*================================================================*
004334 初期化 SECTION.
004335*
004340     OPEN INPUT 元号マスタ.
004350             MOVE NC"元号" TO ファイル名Ｗ.
004360             PERFORM オープンチェック.
004370     OPEN INPUT 制御情報マスタ.
004380             MOVE NC"制御" TO ファイル名Ｗ.
004390             PERFORM オープンチェック.
004400     OPEN INPUT 名称マスタ.
004410             MOVE NC"名称" TO ファイル名Ｗ.
004420             PERFORM オープンチェック.
004430     OPEN INPUT 施術所情報マスタ
004440             MOVE NC"施情" TO ファイル名Ｗ.
004450             PERFORM オープンチェック.
004460     OPEN INPUT 保険者マスタ
004470             MOVE NC"保険者" TO ファイル名.
004480             PERFORM オープンチェック.
004490     OPEN INPUT 市町村マスタ
004500             MOVE NC"市町村" TO ファイル名Ｗ.
004510             PERFORM オープンチェック.
004520     OPEN INPUT 請求先マスタ
004530             MOVE NC"請求先" TO ファイル名Ｗ.
004540             PERFORM オープンチェック.
004550     OPEN INPUT   会情報マスタ
004560             MOVE NC"会情報" TO ファイル名Ｗ.
004570             PERFORM オープンチェック.
004580     OPEN INPUT 作業ファイル２.
004590             MOVE NC"作２" TO ファイル名Ｗ.
004600             PERFORM オープンチェック.
004610*
004620*
004630*    /* 現在日付取得 */
004640     ACCEPT 計算機日付 FROM DATE.
004650*    /* 1980～2079年の間で設定 */
004660     IF 計算機年 > 80
004670         MOVE 19 TO 計算機世紀
004680     ELSE
004690         MOVE 20 TO 計算機世紀
004700     END-IF.
004710*
004720     PERFORM カレント元号取得.
004730     PERFORM 和暦終了年取得.
004740     COMPUTE 計算機和暦年Ｗ = 計算機西暦年 - 和暦終了年Ｗ.
004750     MOVE 計算機和暦年Ｗ TO 施術年Ｗ.
004760     MOVE 計算機月       TO 施術月Ｗ.
004770     MOVE 計算機日       TO 施術日Ｗ.
004780*
004790     PERFORM 連結項目退避.
004800     PERFORM 施術所情報取得.
004810*
004820*================================================================*
004830 オープンチェック SECTION.
004840*
004850     IF 状態キー  NOT =  "00"
004860         DISPLAY ファイル名Ｗ NC"Ｆオープンエラー" UPON CONS
004870         DISPLAY NC"状態キー：" 状態キー           UPON CONS
004880         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
004890                                                   UPON CONS
004900         ACCEPT  キー入力 FROM CONS
004910         PERFORM ファイル閉鎖
004920         EXIT PROGRAM.
004930*================================================================*
004940 カレント元号取得 SECTION.
004950*
004960     MOVE ZEROS TO 制－制御区分.
004970     READ 制御情報マスタ
004980     NOT INVALID KEY
004990         MOVE 制－カレント元号 TO カレント元号Ｗ
005000     END-READ.
005010*
005020*================================================================*
005030 和暦終了年取得 SECTION.
005040*
005050*     DISPLAY NC"カレント元号Ｗ"  カレント元号Ｗ UPON MSGBOX.
005060     MOVE カレント元号Ｗ TO 元－元号区分.
005070     READ 元号マスタ
005080     INVALID KEY
005090         DISPLAY NC"指定和暦が登録されていません" UPON CONS
005100         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
005110                                                  UPON CONS
005120         ACCEPT  キー入力 FROM CONS
005130         PERFORM 終了処理
005140         EXIT PROGRAM
005150     NOT INVALID KEY
005160         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
005170         MOVE 前和暦Ｗ TO 元－元号区分
005180         READ 元号マスタ
005190         INVALID KEY
005200             DISPLAY NC"指定和暦が登録されていません" UPON CONS
005210             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
005220                                                      UPON CONS
005230             ACCEPT  キー入力 FROM CONS
005240             PERFORM 終了処理
005250             EXIT PROGRAM
005260         NOT INVALID KEY
005270             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
005280         END-READ
005290     END-READ.
005300*
005310*================================================================*
005320 連結項目退避 SECTION.
005330*
005340     MOVE 連入－請求和暦  TO 請求和暦Ｗ.
005350     MOVE 連入－請求年    TO 請求年Ｗ.
005360     MOVE 連入－請求月    TO 請求月Ｗ.
005370     MOVE 連入－提出和暦  TO 提出和暦Ｗ.
005380     MOVE 連入－提出年    TO 提出年Ｗ.
005390     MOVE 連入－提出月    TO 提出月Ｗ.
005400     MOVE 連入－提出日    TO 提出日Ｗ.
005410     MOVE 連入－印刷種類  TO 印刷種類Ｗ.
005420*
005430*================================================================*
005440 施術所情報取得 SECTION.
005450*
005460     MOVE ZERO  TO 施情－施術所番号.
005470     READ 施術所情報マスタ
005480     INVALID KEY
005490         CONTINUE
005500     NOT INVALID KEY
005510*
005520         MOVE 施情－郵便番号１       TO 施術所郵便番号１Ｗ
005530         MOVE "-"                    TO 施術所郵便番号区切Ｗ
005540         MOVE 施情－郵便番号２       TO 施術所郵便番号２Ｗ
005550         MOVE 施情－代表者名         TO 代表者名Ｗ
005560         MOVE 施情－接骨院名         TO 接骨院名Ｗ
005570         STRING 施情－住所１  DELIMITED BY SPACE
005580                施情－住所２  DELIMITED BY SPACE
005590           INTO 施術所住所Ｗ
005600         END-STRING
005610         MOVE 施情－電話番号         TO 施術所電話番号Ｗ
005620         MOVE 施情－新柔整師番号     TO 柔整師番号Ｗ
005630*
005640         MOVE 施情－取引先銀行名     TO 取引先銀行名Ｗ
005650         MOVE 施情－取引先銀行支店名 TO 取引先銀行支店名Ｗ
005660         MOVE 施情－預金種別         TO 預金種別Ｗ
005670         MOVE 施情－銀行番号         TO 銀行番号Ｗ
005680         MOVE 施情－店番号           TO 店番号Ｗ
005690         MOVE 施情－口座番号         TO 口座番号Ｗ
005700         MOVE 施情－口座名義人カナ   TO 口座名義人カナＷ
005710         MOVE 施情－口座名義人       TO 口座名義人Ｗ
005720         STRING 取引先銀行名Ｗ     DELIMITED BY SPACE
005730                " "                DELIMITED BY SIZE
005740                取引先銀行支店名Ｗ DELIMITED BY SPACE
005750                INTO 銀行名支店名Ｗ
005760         END-STRING
005770         EVALUATE 預金種別Ｗ
005780         WHEN 1
005790             MOVE NC"普通" TO 預金種別コメントＷ
005800         WHEN 2
005810             MOVE NC"当座" TO 預金種別コメントＷ
005820         WHEN OTHER
005830             MOVE SPACE    TO 預金種別コメントＷ
005840         END-EVALUATE
005850*
005860     END-READ.
005870*================================================================*
005880 ファイル閉鎖 SECTION.
005890*
005900     CLOSE 元号マスタ 制御情報マスタ   作業ファイル２
005910           名称マスタ 施術所情報マスタ 市町村マスタ 
005920           保険者マスタ 請求先マスタ   会情報マスタ.
005930*================================================================*
005940 終了処理 SECTION.
005950*
005960     PERFORM ファイル閉鎖.
005970*
005980*================================================================*
005990 エラー表示 SECTION.
006000*
006010     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
006020     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
006030     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
006040     ACCEPT  キー入力 FROM CONS.
006050*================================================================*
006060 エラー表示Ｒ SECTION.
006070*
006080     DISPLAY NC"ファイル読込エラー" ファイル名     UPON CONS.
006090     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
006100     ACCEPT  キー入力 FROM CONS.
006110*================================================================*
006120 作業ファイル２読込 SECTION.
006130*
006140     READ 作業ファイル２ NEXT
006150     AT END
006160         MOVE "YES" TO 終了フラグ
006170     END-READ.
006180*================================================================*
006190 印刷処理 SECTION.
006200*
006240     MOVE 1       TO 頁Ｗ.
006250*
006260* / 副キー(印刷順序+保険種別+保険者番号)で読み込む./
006270     MOVE ZERO      TO  作２－請求和暦.
006280     MOVE ZERO      TO  作２－請求年.
006290     MOVE ZERO      TO  作２－請求月.
006300*     MOVE LOW-VALUE TO  作２－県.
006310     MOVE ZERO      TO  作２－県.
006320     MOVE ZERO      TO  作２－印刷順序.
006330     MOVE ZERO      TO  作２－保険種別.
006340     MOVE LOW-VALUE TO  作２－保険者番号.
006350*/県別、社保日雇いをまとめ、船員を別に集計↓↓/081021
006360     MOVE ZERO      TO  作２－県２.
006370*     START 作業ファイル２   KEY IS >=  作２－請求和暦年月
006380*                                       作２－印刷順序
006390*                                       作２－保険種別
006400*                                       作２－保険者番号
006410**                                       作２－県
006420     START 作業ファイル２   KEY IS >=  作２－請求和暦年月
006430                                       作２－県２
006440                                       作２－印刷順序
006450                                       作２－保険種別
006460                                       作２－保険者番号
006470*/県別、社保日雇いをまとめ、船員を別に集計↑↑/081021
006480     END-START.
006490     IF 状態キー = "00"
006500         MOVE SPACE TO 終了フラグ
006510         PERFORM 作業ファイル２読込
006520         IF  終了フラグ = "YES"
006530             MOVE  NC"　該当データなしです。確認して下さい。" TO 連メ－メッセージ
006540             CALL   "MSG001"
006550             CANCEL "MSG001"
006560             PERFORM ファイル閉鎖
006570             MOVE 99 TO PROGRAM-STATUS
006580             EXIT PROGRAM
006590         END-IF
006600*
006610         PERFORM UNTIL 終了フラグ = "YES"
006620             MOVE SPACE TO スキップフラグ
006630             PERFORM 印刷対象チェック
006640             IF 印刷フラグ = "YES"
006650*
006660             MOVE SPACE TO YJK437P
006670             INITIALIZE    YJK437P
006680             MOVE 作２－県         TO 県Ｗ
006690             MOVE 作２－印刷順序 TO 印刷順序Ｗ
006700             PERFORM  表セット１
006710
006720* 社保、船員、日雇と、共済、自衛官はまとめて印字する
006730             IF 印刷順序Ｗ(2:1) = 1 OR 9
006740* 
006750                 PERFORM VARYING 行カウンタ FROM 1 BY 1
006760                         UNTIL ( 行カウンタ > 8       ) OR
006770                               ( 県Ｗ       NOT = 作２－県       ) OR
006780                               ( 印刷順序Ｗ(2:1) NOT = 作２－印刷順序(2:1) ) OR
006790                               ( 終了フラグ = "YES" )
006800                      PERFORM 印刷対象チェック
006810                      IF 印刷フラグ = "YES"
006820                         PERFORM 表セット２
006830*/↓↓改頁後、レコードがあり、印刷対象がない場合、合計が印刷されない/0510
006840*                  ELSE
006850*                     COMPUTE 行カウンタ =  行カウンタ - 1
006860                      END-IF
006870                      PERFORM 作業ファイル２読込
006880                      PERFORM 印刷対象チェック
006890                      IF 印刷フラグ = SPACE
006900                         COMPUTE 行カウンタ =  行カウンタ - 1
006910                      END-IF
006920*/↑↑改頁後、レコードがあり、印刷対象がない場合、合計が印刷されない/0510
006930                      MOVE "YES" TO スキップフラグ
006940                 END-PERFORM
006950*
006960                 IF ( 終了フラグ =  "YES" ) OR 
006970                    ( 印刷順序Ｗ(2:1) NOT = 作２－印刷順序(2:1) ) OR
006980                    ( 県Ｗ       NOT = 作２－県 )
006990                     MOVE 本人件数カウント   TO     本人件数合計
007000                     MOVE 家族件数カウント   TO     家族件数合計
007010                     MOVE 計件数カウント     TO     計件数合計
007020                     MOVE 本人費用額カウント TO     本人費用額合計
007030                     MOVE 家族費用額カウント TO     家族費用額合計
007040                     MOVE 計費用額カウント   TO     計費用額合計
007050                     MOVE ZERO   TO 頁Ｗ
007060*
007070                     INITIALIZE 合計用Ｗ
007080                 ELSE
007090                     MOVE ZERO     TO     本人件数合計
007100                     MOVE ZERO     TO     家族件数合計
007110                     MOVE ZERO     TO     計件数合計
007120                     MOVE ZERO     TO     本人費用額合計
007130                     MOVE ZERO     TO     家族費用額合計
007140                     MOVE ZERO     TO     計費用額合計
007150                     MOVE ZERO     TO     本人請求額合計
007160                     MOVE ZERO     TO     家族請求額合計
007170                     MOVE ZERO     TO     計請求額合計
007180                 END-IF
007190
007200             ELSE
007210* 社保、船員、日雇、共済、自衛官以外
007220* 
007230                 PERFORM VARYING 行カウンタ FROM 1 BY 1
007240                         UNTIL ( 行カウンタ > 8       ) OR
007250                               ( 県Ｗ       NOT = 作２－県       ) OR
007260                               ( 印刷順序Ｗ NOT = 作２－印刷順序 ) OR
007270                               ( 終了フラグ = "YES" )
007280                      PERFORM 印刷対象チェック
007290                      IF 印刷フラグ = "YES"
007300                          PERFORM 表セット２
007310*/↓↓改頁後、レコードがあり、印刷対象がない場合、合計が印刷されない/0510
007320*                  ELSE
007330*                     COMPUTE 行カウンタ =  行カウンタ - 1
007340                      END-IF
007350                      PERFORM 作業ファイル２読込
007360                      PERFORM 印刷対象チェック
007370                      IF 印刷フラグ = SPACE
007380                          COMPUTE 行カウンタ =  行カウンタ - 1
007390                      END-IF
007400*/↑↑改頁後、レコードがあり、印刷対象がない場合、合計が印刷されない/0510
007410                      MOVE "YES" TO スキップフラグ
007420                 END-PERFORM
007430*
007440                 IF ( 終了フラグ =  "YES" ) OR 
007450                    ( 印刷順序Ｗ NOT = 作２－印刷順序 ) OR
007460                    ( 県Ｗ       NOT = 作２－県 )
007470                     MOVE 本人件数カウント   TO     本人件数合計
007480                     MOVE 家族件数カウント   TO     家族件数合計
007490                     MOVE 計件数カウント     TO     計件数合計
007500                     MOVE 本人費用額カウント TO     本人費用額合計
007510                     MOVE 家族費用額カウント TO     家族費用額合計
007520                     MOVE 計費用額カウント   TO     計費用額合計
007530                     MOVE ZERO               TO     頁Ｗ
007540*
007550                     INITIALIZE 合計用Ｗ
007560                 ELSE
007570                     MOVE ZERO     TO     本人件数合計
007580                     MOVE ZERO     TO     家族件数合計
007590                     MOVE ZERO     TO     計件数合計
007600                     MOVE ZERO     TO     本人費用額合計
007610                     MOVE ZERO     TO     家族費用額合計
007620                     MOVE ZERO     TO     計費用額合計
007630                     MOVE ZERO     TO     本人請求額合計
007640                     MOVE ZERO     TO     家族請求額合計
007650                     MOVE ZERO     TO     計請求額合計
007660                 END-IF
007670
007680             END-IF
007690*
007700             PERFORM 印字処理
007710             PERFORM 改頁処理
007720*
007730             COMPUTE 頁Ｗ = 頁Ｗ + 1
007740           END-IF
007750*
007760           IF スキップフラグ NOT = "YES"
007770              PERFORM 作業ファイル２読込
007780           END-IF
007790         END-PERFORM
007800*
007810     ELSE
007820         MOVE  NC"　該当データなしです。確認して下さい。" TO 連メ－メッセージ
007830         CALL   "MSG001"
007840         CANCEL "MSG001"
007850         PERFORM ファイル閉鎖
007860         MOVE 99 TO PROGRAM-STATUS
007870         EXIT PROGRAM
007880     END-IF.
007890*
007900     CLOSE 印刷ファイル.
007910*
007920*================================================================*
007930 表セット１ SECTION.
007940*
007950     MOVE 頁Ｗ                 TO 頁.
007960
007970* 当月の和暦を取得
007980     MOVE 請求和暦Ｗ         TO 元－元号区分.
007990     READ 元号マスタ
008000     INVALID KEY
008010         MOVE SPACE          TO 請求和暦名称
008020     NOT INVALID KEY
008030         MOVE 元－元号名称   TO 請求和暦名称
008040     END-READ.
008050*
008060     MOVE 請求年Ｗ           TO 請求年.
008070     MOVE 請求月Ｗ           TO 請求月.
008080*
008090     MOVE 代表者名Ｗ         TO 代表者名.
008100     MOVE 接骨院名Ｗ         TO 接骨院名.
008110     MOVE 柔整師番号Ｗ(3:7)  TO 柔整師番号１.
008120     MOVE 柔整師番号Ｗ(11:1) TO 柔整師番号２.
008130     MOVE 柔整師番号Ｗ(13:1) TO 柔整師番号３.
008140     PERFORM 県名セット.
008150     PERFORM 保険種別名セット.
008160*
008170*================================================================*
008180 表セット２ SECTION.
008190*
008200     MOVE 作２－本人件数     TO 本人件数(行カウンタ).
008210     MOVE 作２－本人費用額   TO 本人費用額(行カウンタ).
008220*
008230     MOVE 作２－家族件数     TO 家族件数(行カウンタ).
008240     MOVE 作２－家族費用額   TO 家族費用額(行カウンタ).
008250*
008260     MOVE 作２－件数    TO  計件数(行カウンタ).
008270     MOVE 作２－費用額  TO  計費用額(行カウンタ).
008280*
008290     ADD 作２－本人件数   TO  本人件数カウント.
008300     ADD 作２－家族件数   TO  家族件数カウント.
008310     ADD 作２－件数       TO  計件数カウント.
008320     ADD 作２－本人費用額 TO  本人費用額カウント.
008330     ADD 作２－家族費用額 TO  家族費用額カウント.
008340     ADD 作２－費用額     TO  計費用額カウント.
008350     ADD 作２－本人請求額 TO  本人請求額カウント.
008360     ADD 作２－家族請求額 TO  家族請求額カウント.
008370*     ADD 作２－請求額     TO  計請求額カウント.
008380**
008390* 保険者
008400     MOVE 作２－保険種別     TO 保険種別Ｗ.
008410     MOVE 作２－保険者番号   TO 保険者番号Ｗ.
008420     EVALUATE 保険種別Ｗ
008430     WHEN 1 THRU 4
008440     WHEN 6 THRU 9
008450         PERFORM 保険者情報取得
008460     WHEN 5
008470     WHEN 50 THRU 60
008480         IF (作２－保険種別 = 05) AND (作２－保険者番号(1:2) = "39")
008490             PERFORM 後期高齢情報取得
008500         ELSE
008510             PERFORM 市町村情報取得
008520         END-IF
008530     END-EVALUATE.
008540     PERFORM 結合宛名取得.
008550*
008560     MOVE 保険者宛名Ｗ  TO 保険者名称Ｗ.
008570*
008580*
008590     COMPUTE 合計行カウンタ = 行カウンタ * 3.
008600     MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 1).
008610     IF 作２－保険種別 = "50" OR "51" OR "52" OR "53" OR
008620                         "54" OR "55" OR "60" OR "05" OR "08"
008630        IF 保険者名称２Ｗ NOT = SPACE AND 保険者名称３Ｗ = SPACE
008640           MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 2)
008650           MOVE 保険者名称２Ｗ TO 保険者名(合計行カウンタ - 1)
008660        END-IF
008670     ELSE
008680        IF 保険者名称２Ｗ NOT = SPACE AND 保険者名称３Ｗ = SPACE
008690           MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 1)
008700           MOVE 保険者名称２Ｗ TO 保険者名(合計行カウンタ)
008710        END-IF
008720     END-IF.
008730     IF 作２－保険種別 = "50" OR "51" OR "52" OR "53" OR
008740                         "54" OR "55" OR "60" OR "05" OR "08"
008750        IF 保険者名称２Ｗ NOT = SPACE AND 保険者名称３Ｗ NOT = SPACE
008760           MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 2)
008770           MOVE 保険者名称２Ｗ TO 保険者名(合計行カウンタ - 1)
008780        END-IF
008790     ELSE
008800        IF 保険者名称２Ｗ NOT = SPACE AND 保険者名称３Ｗ NOT = SPACE
008810           MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 2)
008820           MOVE 保険者名称２Ｗ TO 保険者名(合計行カウンタ - 1)
008830           MOVE 保険者名称３Ｗ TO 保険者名(合計行カウンタ)
008840        END-IF
008850     END-IF.
008860*
008870*================================================================*
008880 エラー処理Ｐ SECTION.
008890*
008900     IF 通知情報Ｐ NOT = "00"
008910         DISPLAY NC"帳票エラー"              UPON CONS
008920         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
008930         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
008940         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
008950         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008960                                             UPON CONS
008970         ACCEPT  キー入力 FROM CONS
008980         PERFORM ファイル閉鎖
008990         MOVE 99  TO PROGRAM-STATUS
009000         EXIT PROGRAM
009010     END-IF.
009020*================================================================*
009030 印字処理  SECTION.
009040*
009051     IF ( オープンフラグ NOT = "YES" )
009052        MOVE "YES" TO オープンフラグ
009053        OPEN I-O  印刷ファイル
009054        PERFORM エラー処理Ｐ
009056     END-IF.
009057*
009058     MOVE "YJK437P" TO  定義体名Ｐ.
009060     MOVE SPACE     TO  処理種別Ｐ.
009070     MOVE "SCREEN"  TO  項目群名Ｐ.
009081     WRITE YJK437P.
009091     PERFORM エラー処理Ｐ.
009100*================================================================*
009110 改頁処理  SECTION.
009120
009130     MOVE "YJK437P" TO  定義体名Ｐ.
009140     MOVE "CT"      TO  処理種別Ｐ.
009150     MOVE "PAGE"    TO  拡張制御Ｐ.
009160     MOVE SPACE     TO  項目群名Ｐ.
009170     WRITE YJK437P.
009180     PERFORM エラー処理Ｐ.
009190     MOVE SPACE     TO  拡張制御Ｐ.
009200*
009210*================================================================*
009220 保険者情報取得 SECTION.
009230*
009240     MOVE  SPACE         TO 請求先名称Ｗ.
009250     MOVE  SPACE         TO 支部部署名Ｗ.
009260     MOVE  ZERO          TO 接尾語区分Ｗ.
009270*
009280     MOVE 保険種別Ｗ     TO 保－保険種別.
009290     MOVE 保険者番号Ｗ   TO 保－保険者番号.
009300     READ 保険者マスタ
009310     INVALID KEY
009320         MOVE SPACE      TO 請求先名称Ｗ
009330         MOVE SPACE      TO 支部部署名Ｗ
009340     NOT INVALID KEY
009350         IF 保－請求先情報区分 = 1
009360             MOVE 保－保険種別   TO 請先－保険種別
009370             MOVE 保－保険者番号 TO 請先－保険者番号
009380             READ 請求先マスタ
009390             INVALID KEY
009400                 MOVE SPACE             TO 請求先名称Ｗ
009410                 MOVE SPACE             TO 支部部署名Ｗ
009420             NOT INVALID KEY
009430                 MOVE 請先－保険者名称  TO 請求先名称Ｗ
009440                 MOVE 請先－支部部署名  TO 支部部署名Ｗ
009450             END-READ
009460         ELSE
009470             MOVE 保－保険者名称        TO 請求先名称Ｗ
009480             MOVE 保－支部部署名        TO 支部部署名Ｗ
009490             MOVE 保－接尾語区分        TO 接尾語区分Ｗ
009500         END-IF
009510     END-READ.
009520*================================================================*
009530 市町村情報取得 SECTION.
009540*
009550     MOVE  SPACE         TO 請求先名称Ｗ.
009560     MOVE  SPACE         TO 支部部署名Ｗ.
009570*
009580     MOVE 保険種別Ｗ               TO 市－公費種別.
009590     MOVE 保険者番号Ｗ             TO 市－市町村番号.
009600     READ 市町村マスタ
009610     INVALID KEY
009620         MOVE SPACE                TO 請求先名称Ｗ
009630         MOVE SPACE                TO 支部部署名Ｗ
009640     NOT INVALID KEY
009650         IF 市－請求先区分 = 1
009660             MOVE 保険種別Ｗ       TO 請先－保険種別
009670             MOVE 保険者番号Ｗ     TO 請先－保険者番号
009680             READ 請求先マスタ
009690             INVALID KEY
009700                 MOVE SPACE        TO 請求先名称Ｗ
009710                 MOVE SPACE        TO 支部部署名Ｗ
009720             NOT INVALID KEY
009730                 MOVE 請先－保険者名称   TO 請求先名称Ｗ
009740                 MOVE 請先－支部部署名   TO 支部部署名Ｗ
009750             END-READ
009760          ELSE
009770             MOVE 市－市町村名称   TO 請求先名称Ｗ
009780             MOVE 市－支部部署名   TO 支部部署名Ｗ
009790          END-IF
009800      END-READ.
009810*================================================================*
009820 結合宛名取得 SECTION.
009830*
009840     MOVE SPACE TO 保険者宛名Ｗ.
009850     IF 請求先名称Ｗ NOT = SPACE
009860         EVALUATE 保険種別Ｗ
009870         WHEN 2
009880             IF 接尾語区分Ｗ = 1
009890                MOVE SPACE            TO 宛名Ｗ
009900             ELSE
009910                MOVE "社会保険事務所" TO 宛名Ｗ
009920             END-IF
009930         WHEN 6
009940             IF 接尾語区分Ｗ = 1
009950                MOVE "（日雇）"               TO 宛名Ｗ
009960             ELSE
009970                MOVE "社会保険事務所（日雇）" TO 宛名Ｗ
009980             END-IF
009990         WHEN 7
010000             MOVE "（船員）"       TO 宛名Ｗ
010010         WHEN 3
010020             MOVE "健康保険組合"   TO 宛名Ｗ
010030         WHEN 4
010040             MOVE "共済組合"       TO 宛名Ｗ
010050         WHEN OTHER
010060             MOVE SPACE            TO 宛名Ｗ
010070         END-EVALUATE
010080*
010090         IF 支部部署名Ｗ = SPACE
010100             STRING  請求先名称Ｗ  DELIMITED BY SPACE
010110                     宛名Ｗ        DELIMITED BY SPACE
010120                    INTO 保険者宛名Ｗ
010130             END-STRING
010140         ELSE
010150             STRING  請求先名称Ｗ  DELIMITED BY SPACE
010160                     宛名Ｗ        DELIMITED BY SPACE
010170                     支部部署名Ｗ  DELIMITED BY SPACE
010180                    INTO 保険者宛名Ｗ
010190             END-STRING
010200         END-IF
010210     END-IF.
010220*
010230*================================================================*
010240 制御情報取得２ SECTION.
010250* 初期値設定
010260     MOVE ZERO  TO 国保６号印刷区分Ｗ. 
010270     MOVE ZERO  TO 社保６号印刷区分Ｗ. 
010280     MOVE 1     TO 組合６号印刷区分Ｗ. 
010290     MOVE 1     TO 共済６号印刷区分Ｗ. 
010300     MOVE ZERO  TO 老人６号印刷区分Ｗ. 
010310     MOVE 1     TO 助成６号印刷区分Ｗ. 
010320*
010330*================================================================*
010340 印刷対象チェック  SECTION.
010350*
010360*  印刷区分による振り分け → ６・７号請求書（印刷区分 0:印刷 1:印刷しない）
010370* （一括印刷のみ）
010380*
010390     MOVE SPACE TO 印刷フラグ.
010400*
010410     IF 連入－一括区分 NOT = 1
010420        MOVE "YES" TO 印刷フラグ
010430     ELSE
010440        EVALUATE 作２－保険種別
010450        WHEN 01
010460        WHEN 08
010470           IF 国保６号印刷区分Ｗ NOT = 1
010480              MOVE "YES" TO 印刷フラグ
010490           END-IF
010500        WHEN 02
010510        WHEN 06
010520        WHEN 07
010530           IF 社保６号印刷区分Ｗ NOT = 1
010540              MOVE "YES" TO 印刷フラグ
010550           END-IF
010560        WHEN 03
010570           IF 組合６号印刷区分Ｗ NOT = 1
010580              MOVE "YES" TO 印刷フラグ
010590           END-IF
010600        WHEN 04
010610        WHEN 09
010620           IF 共済６号印刷区分Ｗ NOT = 1
010630              MOVE "YES" TO 印刷フラグ
010640           END-IF
010650        WHEN 05
010660           IF 老人６号印刷区分Ｗ NOT = 1
010670              MOVE "YES" TO 印刷フラグ
010680           END-IF
010690        WHEN 50 THRU 60
010700           IF 助成６号印刷区分Ｗ NOT = 1
010710              MOVE "YES" TO 印刷フラグ
010720           END-IF
010730        WHEN OTHER
010740           MOVE "YES" TO 印刷フラグ
010750        END-EVALUATE
010760     END-IF.
010770*
010780*/東京都の国保退職老人後高は印刷しない(別用紙)/080516
010790     EVALUATE TRUE
010800     WHEN (作２－保険種別 = 01)       AND (作２－保険者番号(1:2) = "13")
010810     WHEN (作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = "13")
010820         MOVE SPACE TO 印刷フラグ
010830     END-EVALUATE.
010840*================================================================*
010850 県名セット SECTION.
010860*
010870     EVALUATE  作２－県
010880*北海道
010890      WHEN  01
010900         MOVE NC"北海道" TO 都道府県名
010910*青森
010920      WHEN  02
010930         MOVE NC"青森県" TO 都道府県名
010940*岩手
010950      WHEN  03
010960         MOVE NC"岩手県" TO 都道府県名
010970*宮城
010980      WHEN  04
010990         MOVE NC"宮城県" TO 都道府県名
011000*秋田
011010      WHEN  05
011020         MOVE NC"秋田県" TO 都道府県名
011030*山形
011040      WHEN  06
011050         MOVE NC"山形県" TO 都道府県名
011060*福島
011070      WHEN  07
011080         MOVE NC"福島県" TO 都道府県名
011090*茨城
011100      WHEN  08
011110         MOVE NC"茨城県" TO 都道府県名
011120*栃木
011130      WHEN  09
011140         MOVE NC"栃木県" TO 都道府県名
011150*群馬
011160      WHEN  10
011170         MOVE NC"群馬県" TO 都道府県名
011180*埼玉
011190      WHEN  11
011200         MOVE NC"埼玉県" TO 都道府県名
011210*千葉
011220      WHEN  12
011230         MOVE NC"千葉県" TO 都道府県名
011240*東京
011250      WHEN  13
011260         MOVE NC"東京都" TO 都道府県名
011270*神奈川
011280      WHEN  14
011290         MOVE NC"神奈川県" TO 都道府県名
011300*新潟
011310      WHEN  15
011320         MOVE NC"新潟県" TO 都道府県名
011330*富山
011340      WHEN  16
011350         MOVE NC"富山県" TO 都道府県名
011360*石川
011370      WHEN  17
011380         MOVE NC"石川県" TO 都道府県名
011390*福井
011400      WHEN  18
011410         MOVE NC"福井県" TO 都道府県名
011420*山梨
011430      WHEN  19
011440         MOVE NC"山梨県" TO 都道府県名
011450*長野
011460      WHEN  20
011470         MOVE NC"長野県" TO 都道府県名
011480*岐阜
011490      WHEN  21
011500         MOVE NC"岐阜県" TO 都道府県名
011510*静岡
011520      WHEN  22
011530         MOVE NC"静岡県" TO 都道府県名
011540*愛知
011550      WHEN  23
011560         MOVE NC"愛知県" TO 都道府県名
011570*三重
011580      WHEN  24
011590         MOVE NC"三重県" TO 都道府県名
011600*滋賀
011610      WHEN  25
011620         MOVE NC"滋賀県" TO 都道府県名
011630*京都
011640      WHEN  26
011650         MOVE NC"京都府" TO 都道府県名
011660*大阪
011670      WHEN  27
011680         MOVE NC"大阪府" TO 都道府県名
011690*兵庫
011700      WHEN  28
011710         MOVE NC"兵庫県" TO 都道府県名
011720*奈良
011730      WHEN  29
011740         MOVE NC"奈良県" TO 都道府県名
011750*和歌山
011760      WHEN  30
011770         MOVE NC"和歌山県" TO 都道府県名
011780*鳥取
011790      WHEN  31
011800         MOVE NC"鳥取県" TO 都道府県名
011810*島根
011820      WHEN  32
011830         MOVE NC"島根県" TO 都道府県名
011840*岡山
011850      WHEN  33
011860         MOVE NC"岡山県" TO 都道府県名
011870*広島
011880      WHEN  34
011890         MOVE NC"広島県" TO 都道府県名
011900*山口
011910      WHEN  35
011920         MOVE NC"山口県" TO 都道府県名
011930*徳島
011940      WHEN  36
011950         MOVE NC"徳島県" TO 都道府県名
011960*香川
011970      WHEN  37
011980         MOVE NC"香川県" TO 都道府県名
011990*愛媛
012000      WHEN  38
012010         MOVE NC"愛媛県" TO 都道府県名
012020*高知
012030      WHEN  39
012040         MOVE NC"高知県" TO 都道府県名
012050*福岡
012060      WHEN  40
012070         MOVE NC"福岡県" TO 都道府県名
012080*佐賀
012090      WHEN  41
012100         MOVE NC"佐賀県" TO 都道府県名
012110*長崎
012120      WHEN  42
012130         MOVE NC"長崎県" TO 都道府県名
012140*熊本
012150      WHEN  43
012160         MOVE NC"熊本県" TO 都道府県名
012170*大分
012180      WHEN  44
012190         MOVE NC"大分県" TO 都道府県名
012200*宮崎
012210      WHEN  45
012220         MOVE NC"宮崎県" TO 都道府県名
012230*鹿児島
012240      WHEN  46
012250         MOVE NC"鹿児島県" TO 都道府県名
012260*沖縄
012270      WHEN  47
012280         MOVE NC"沖縄県" TO 都道府県名
012290*その他
012300      WHEN  OTHER
012310         CONTINUE
012320     END-EVALUATE.
012330*================================================================*
012340 保険種別名セット SECTION.
012350*
012360**///  印刷順序の変更はこのSECTIONで   ////**
012370*
012380*************************************************************************
012390*  印刷順序:保険名称(保険種別コード)                                    *
012400*                                                                       *
012410*    10:社保(2)                                                         *
012420*    11:船員(7)                                                         *
012430*    12:日雇(6)                                                         *
012440*    50:国保(1)                                                         *
012450*    51:国保組合(1)                                                     *
012460*    52:退職(8)                                                         *
012470*    53:老人(5)                                                         *
012480*    60:４１老人(51)                                                    *
012490*    61:母子(52)                                                        *
012500*    62:乳幼児(55)                                                      *
012510*    63:障害(53)                                                        *
012520*    64:被爆(54)                                                        *
012530*    65:その他(60)                                                      *
012540*    80:組合(3)                                                         *
012550*    90:共済(4)                                                         *
012560*    91:自衛官(9)                                                       *
012570*                                                                       *
012580*   労災、自賠責、自費はのぞく                                          *
012590*                                                                       *
012600*                                                                       *
012610*************************************************************************
012620*
012630*
012640      EVALUATE  作２－印刷順序
012650* 国保
012660      WHEN  050
012670          MOVE NC"国民保険" TO 保険種別名
012680* 国保組合条件
012690      WHEN  051
012700          MOVE NC"国保組合" TO 保険種別名
012710* 社保
012720      WHEN  010
012730*          MOVE NC"社会保険" TO 保険種別名
012740          MOVE NC"協保" TO 保険種別名
012750* 船員は社保で表示
012760*      WHEN  011
012770      WHEN  020
012780          MOVE NC"社会保険" TO 保険種別名
012790* 日雇は社保で表示
012800      WHEN  012
012810*          MOVE NC"社会保険" TO 保険種別名
012820          MOVE NC"協保" TO 保険種別名
012830* 組合
012840      WHEN  080
012850          MOVE NC"健保組合" TO 保険種別名
012860* 共済
012870      WHEN  090
012880          MOVE NC"共済組合" TO 保険種別名
012890* 自衛官は共済組合で表示
012900      WHEN  091
012910          MOVE NC"共済組合" TO 保険種別名
012920* ２７老人
012930      WHEN  053
012940          IF 作２－保険者番号(1:2) = "39"
012950              MOVE NC"後期"     TO 保険種別名
012960          ELSE
012970              MOVE NC"老健老人" TO 保険種別名
012980          END-IF
012990* 退職国保
013000      WHEN  052
013010          MOVE NC"退職国保" TO 保険種別名
013020* ４１老人
013030      WHEN  060
013040          MOVE NC"公費老人" TO 保険種別名
013050* 母子
013060      WHEN  061
013070          MOVE NC"母子家庭" TO 保険種別名
013080* 乳幼児
013090      WHEN  062
013100          MOVE NC"乳幼児" TO 保険種別名
013110* 障害
013120      WHEN  063
013130          MOVE NC"障害者" TO 保険種別名
013140* 被爆
013150      WHEN  064
013160          MOVE NC"被爆者" TO 保険種別名
013170* その他
013180* 東京の８８１３のみ「子」を印字し、それ以外は空白とする
013190      WHEN  065
013200          EVALUATE 作２－保険者番号(1:4)
013210          WHEN "8813"
013220              MOVE NC"子" TO 保険種別名
013230          WHEN OTHER
013240              MOVE SPACE TO 保険種別名
013250          END-EVALUATE
013260      WHEN OTHER
013270          MOVE SPACE TO 保険種別名
013280     END-EVALUATE.
013290*
013300*================================================================*
013310 後期高齢情報取得 SECTION.
013320*
013330     MOVE SPACE        TO 請求先名称Ｗ.
013340     MOVE SPACE        TO 支部部署名Ｗ.
013350     MOVE 保険種別Ｗ   TO 市－公費種別.
013360     MOVE 保険者番号Ｗ TO 市－市町村番号.
013370     START 市町村マスタ KEY IS >= 市－公費種別 市－市町村番号
013380     END-START.
013390     IF 状態キー = "00"
013400         READ 市町村マスタ NEXT
013410         AT END
013420             MOVE SPACE              TO 請求先名称Ｗ
013430             MOVE SPACE              TO 支部部署名Ｗ
013440         NOT AT END
013450             IF (保険種別Ｗ        = 市－公費種別       ) AND
013460                (保険者番号Ｗ(1:4) = 市－市町村番号(1:4))
013470                 MOVE 市－市町村名称 TO 請求先名称Ｗ
013480                 MOVE SPACE          TO 支部部署名Ｗ
013490             END-IF
013500         END-READ
013510     END-IF.
013520*================================================================*
013530******************************************************************
013540 END PROGRAM YJK437.
013550******************************************************************
