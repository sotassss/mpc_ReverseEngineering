000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YJK4361.
000060 AUTHOR.                 岡田 憲和
000070*
000080*----------------------------------------------------------------*
000090*  日本柔整共済会用 ７号用紙 請求書【印刷】柔ｳｨﾝﾄﾞｳｽﾞ95版
000100*         MED = YJK436P
000110*  2007/5/28 　6，7号並び替え対応
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
001170     SELECT  作業ファイル３  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\YJK4361.DAT"
001180                             ORGANIZATION             IS  INDEXED
001190                             ACCESS                   IS  DYNAMIC
001200                             RECORD      KEY          IS  作３－請求和暦年月
001210                                                          作３－保険種別
001220                                                          作３－保険者番号
001230                             ALTERNATE RECORD KEY     IS  作３－請求和暦年月
001240                                                          作３－県
001250                                                          作３－印刷順序
001260                                                          作３－保険種別
001270                                                          作３－保険者番号
001280                             ALTERNATE RECORD KEY     IS  作３－請求和暦年月
001290                                                          作３－印刷順序
001300                                                          作３－保険種別
001310                                                          作３－保険者番号
001320*/県別、社保日雇いをまとめ、船員を別に集計/081021
001330                             ALTERNATE RECORD KEY     IS  作３－請求和暦年月
001340                                                          作３－県２
001350                                                          作３－印刷順序
001360                                                          作３－保険種別
001370                                                          作３－保険者番号
001380                             FILE        STATUS       IS  状態キー
001390                             LOCK        MODE         IS  AUTOMATIC.
001400*
001410     SELECT  保険者拡張マスタ ASSIGN     TO        HOKENEXL
001420                             ORGANIZATION             IS  INDEXED
001430                             ACCESS MODE              IS  DYNAMIC
001440                             RECORD KEY               IS  保拡－保険種別
001450                                                          保拡－保険者番号
001460                             FILE STATUS              IS  状態キー
001470                             LOCK        MODE         IS  AUTOMATIC.
001480     SELECT  印刷ファイル    ASSIGN      TO        GS-PRTF001
001490                             SYMBOLIC    DESTINATION  IS "PRT"
001500                             FORMAT                   IS  定義体名Ｐ
001510                             GROUP                    IS  項目群名Ｐ
001520                             PROCESSING  MODE         IS  処理種別Ｐ
001530                             UNIT        CONTROL      IS  拡張制御Ｐ
001540                             FILE        STATUS       IS  通知情報Ｐ.
001550
001560     SELECT  印刷ファイル２  ASSIGN      TO        GS-PRTF001
001570                             SYMBOLIC    DESTINATION  IS "PRT"
001580                             FORMAT                   IS  定義体名Ｐ
001590                             GROUP                    IS  項目群名Ｐ
001600                             PROCESSING  MODE         IS  処理種別Ｐ
001610                             UNIT        CONTROL      IS  拡張制御Ｐ
001620                             FILE        STATUS       IS  通知情報Ｐ.
001630******************************************************************
001640*                      DATA DIVISION                             *
001650******************************************************************
001660 DATA                    DIVISION.
001670 FILE                    SECTION.
001680*                           ［ＲＬ＝  １２８］
001690 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001700     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001710*                           ［ＲＬ＝  ２５６］
001720 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001730     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
001740     COPY SEIGYO01        OF  XFDLIB  JOINING   制０１   AS  PREFIX.
001750*                           ［ＲＬ＝  ３２０］
001760 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001770     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
001780*                           ［ＲＬ＝  ２５６］
001790 FD  市町村マスタ        BLOCK   CONTAINS   1   RECORDS.
001800     COPY SITYOSN         OF  XFDLIB  JOINING   市   AS  PREFIX.
001840*                           ［ＲＬ＝  １２８］
001850 FD  施術所情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001860     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
001870*                           ［ＲＬ＝  １２８］
001880 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
001890     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
001900*                          ［ＲＬ＝  ６４０］
001910 FD  会情報マスタ        BLOCK   CONTAINS   1   RECORDS.
001920     COPY KAIJOHO         OF  XFDLIB  JOINING   会情   AS  PREFIX.
001930*                           ［ＲＬ＝  ８００］
001940 FD  保険者拡張マスタ        BLOCK   CONTAINS   1   RECORDS.
001950     COPY HOKENEX        OF  XFDLIB  JOINING   保拡   AS  PREFIX.
001960*
001970*
001980* 施術/請求  (請求和暦年月は、施術/請求 共用) 
001990*                           ［ＲＬ＝  １２８］
002000 FD  作業ファイル２ RECORD  CONTAINS 128 CHARACTERS.
002010 01  作２－レコード.
002020     03  作２－レコードキー.
002030         05  作２－請求和暦年月.
002040             07  作２－請求和暦              PIC 9.
002050             07  作２－請求年                PIC 9(2).
002060             07  作２－請求月                PIC 9(2).
002070         05  作２－保険種別                  PIC 9(2).
002080         05  作２－保険者番号                PIC X(10).
002090         05  作２－県                        PIC X(2).
002100         05  作２－印刷順序.
002110             07  作２－印刷順序１            PIC 9(2).
002120             07  作２－印刷順序２            PIC 9.
002130     03  作２－レコードデータ.
002140         05  作２－保険者番号２              PIC X(10).
002150         05  作２－件数                      PIC 9(4).
002160         05  作２－費用額                    PIC 9(9).
002170         05  作２－負担額                    PIC 9(9).
002180         05  作２－請求額                    PIC 9(9).
002190         05  作２－本人件数                  PIC 9(3).
002200         05  作２－本人費用額                PIC 9(7).
002210         05  作２－本人負担額                PIC 9(7).
002220         05  作２－本人請求額                PIC 9(7).
002230         05  作２－家族件数                  PIC 9(3).
002240         05  作２－家族費用額                PIC 9(7).
002250         05  作２－家族負担額                PIC 9(7).
002260         05  作２－家族請求額                PIC 9(7).
002270         05  作２－県２                      PIC X(2).
002280         05  FILLER                          PIC X(15).
002290*         05  FILLER                          PIC X(17).
002300
002310 FD  作業ファイル３ RECORD  CONTAINS 128 CHARACTERS.
002320 01  作３－レコード.
002330     03  作３－レコードキー.
002340         05  作３－請求和暦年月.
002350             07  作３－請求和暦              PIC 9.
002360             07  作３－請求年                PIC 9(2).
002370             07  作３－請求月                PIC 9(2).
002380         05  作３－保険種別                  PIC 9(2).
002390         05  作３－保険者番号                PIC X(10).
002400         05  作３－県                        PIC X(2).
002410         05  作３－印刷順序.
002420             07  作３－印刷順序１            PIC 9(2).
002430             07  作３－印刷順序２            PIC 9.
002440     03  作３－レコードデータ.
002450         05  作３－保険者番号２              PIC X(10).
002460         05  作３－件数                      PIC 9(4).
002470         05  作３－費用額                    PIC 9(9).
002480         05  作３－負担額                    PIC 9(9).
002490         05  作３－請求額                    PIC 9(9).
002500         05  作３－本人件数                  PIC 9(3).
002510         05  作３－本人費用額                PIC 9(7).
002520         05  作３－本人負担額                PIC 9(7).
002530         05  作３－本人請求額                PIC 9(7).
002540         05  作３－家族件数                  PIC 9(3).
002550         05  作３－家族費用額                PIC 9(7).
002560         05  作３－家族負担額                PIC 9(7).
002570         05  作３－家族請求額                PIC 9(7).
002580         05  作３－県２                      PIC X(2).
002590         05  FILLER                          PIC X(15).
002600
002610*
002620 FD  印刷ファイル.
002630     COPY YJK436P         OF  XMDLIB  JOINING   ７号   AS  PREFIX.
002640
002650* 
002660 FD  印刷ファイル２.
002670     COPY YJK437P         OF  XMDLIB.
002680
002690*----------------------------------------------------------------*
002700******************************************************************
002710*                WORKING-STORAGE SECTION                         *
002720******************************************************************
002730 WORKING-STORAGE         SECTION.
002740 01 キー入力                           PIC X     VALUE SPACE.
002750 01 状態キー                           PIC X(2)  VALUE SPACE.
002760 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002770 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002780 01 終了フラグ３                       PIC X(3)  VALUE SPACE.
002790 01 書込フラグ                         PIC X(4)  VALUE SPACE.
002800 01 作業フラグ                         PIC X(3)  VALUE SPACE.
002810 01 作業移動キー                       PIC X(4)  VALUE SPACE.
002820 01 終了行フラグ                       PIC X(3)  VALUE SPACE.
002830 01 ファイル名                         PIC N(2)  VALUE SPACE.
002840 01 備考Ｗ                             PIC X(20) VALUE SPACE.
002850 01 前和暦Ｗ                           PIC 9(1)  VALUE ZERO.
002860 01 保険種別Ｗ                         PIC 9(2)  VALUE ZERO.
002870 01 保険者番号Ｗ                       PIC X(10) VALUE SPACE.
002880 01 保険者番号１Ｗ                     PIC X(10) VALUE SPACE.
002890 01 保険者番号２Ｗ                     PIC X(10) VALUE SPACE.
002900 01 印刷フラグ                         PIC X(3)  VALUE SPACE.
002901 01 オープンフラグ                     PIC X(3)  VALUE SPACE.
002911 01 オープンフラグ２                   PIC X(3)  VALUE SPACE.
002912*
002920 01 行カウンタ                         PIC 9(2)  VALUE ZERO.
002930 01 頁カウンタ                         PIC 9(4)  VALUE ZERO.
002940 01 最大行数                           PIC 9(2)  VALUE ZERO.
002950 01 ヘッダ行数                         PIC 9(2)  VALUE ZERO.
002960 01 移動行数Ｗ                         PIC 9(2)  VALUE ZERO.
002970 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
002980 01 保険名称Ｗ                         PIC N(2) VALUE SPACE.
002990*
003000 01 施術和暦年月Ｗ.
003010     03 施術和暦Ｗ                     PIC 9(1)  VALUE ZERO.
003020     03 施術年月Ｗ.
003030        05 施術年Ｗ                    PIC 9(2)  VALUE ZERO.
003040        05 施術月Ｗ                    PIC 9(2)  VALUE ZERO.
003050     03 受理年月Ｗ.
003060        05 受理年Ｗ                    PIC 9(2) VALUE ZERO.
003070        05 受理月Ｗ                    PIC 9(2) VALUE ZERO.
003080        05 受理日Ｗ                    PIC 9(2) VALUE ZERO.
003090
003100
003110* ６号用
003120 01 合計用Ｗ.
003130    03 本人件数カウント                PIC 9(9) VALUE ZERO.
003140    03 家族件数カウント                PIC 9(9) VALUE ZERO.
003150    03 計件数カウント                  PIC 9(9) VALUE ZERO.
003160    03 本人費用額カウント              PIC 9(9) VALUE ZERO.
003170    03 家族費用額カウント              PIC 9(9) VALUE ZERO.
003180    03 計費用額カウント                PIC 9(9) VALUE ZERO.
003190    03 本人請求額カウント              PIC 9(9) VALUE ZERO.
003200    03 家族請求額カウント              PIC 9(9) VALUE ZERO.
003210    03 計請求額カウント                PIC 9(9) VALUE ZERO.
003220 01 県Ｗ                               PIC X(2) VALUE SPACE.
003230 01 印刷順序Ｗ                         PIC 9(3) VALUE ZERO.
003240 01 頁Ｗ                               PIC 9(2) VALUE ZERO.
003250
003260 01 県Ｗ２                               PIC X(2) VALUE SPACE.
003270 01 印刷順序Ｗ２                         PIC 9(3) VALUE ZERO.
003280 01 印刷順序Ｗ３                         PIC 9(3) VALUE ZERO.
003290 01 県Ｗ３                               PIC X(2) VALUE SPACE.
003300* 01 頁Ｗ２                               PIC 9(2) VALUE ZERO.
003310
003320 01 保険種別Ｗ３                       PIC 9(2)  VALUE ZERO.
003330 01 保険者番号Ｗ３                     PIC X(10) VALUE SPACE.
003340
003350 01 合計行カウンタ                     PIC 9(3)  VALUE ZERO.
003360 01 ページカウンタ                     PIC 9(3)  VALUE ZERO.
003370 01 スキップフラグ                     PIC X(3)  VALUE SPACE.
003390
003400 01 保険者名称Ｗ.
003410    03 保険者名称１Ｗ                  PIC X(20) VALUE SPACE.
003420    03 保険者名称２Ｗ                  PIC X(20) VALUE SPACE.
003430    03 保険者名称３Ｗ                  PIC X(20) VALUE SPACE.
003440
003450 01 印刷フラグ２                       PIC X(3)  VALUE SPACE.
003460*
003470 01 件数ＷＫ                           PIC 9(9) VALUE ZERO.
003480 01 費用ＷＫ                           PIC 9(9) VALUE ZERO.
003490 01 請求ＷＫ                           PIC 9(9) VALUE ZERO.
003500 01 全角空白                           PIC X(2)  VALUE X"8140".
003510 01 半角空白                           PIC X(2)  VALUE X"2020".
003520
003530 01 レセ並び順昇降Ｗ                   PIC 9    VALUE ZERO.
003540
003550**
003560**************
003570* 施術所情報 *
003580**************
003590 01 施術所情報Ｗ.
003600    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
003610    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
003620    03 柔整師番号Ｗ                    PIC X(20)  VALUE SPACE.
003630    03 施術所住所Ｗ.
003640       05 施術所住所１Ｗ               PIC X(40)  VALUE SPACE.
003650       05 施術所住所２Ｗ               PIC X(40)  VALUE SPACE.
003660    03 施術所郵便番号Ｗ.
003670       05 施術所郵便番号１Ｗ           PIC X(3)   VALUE SPACE.
003680       05 施術所郵便番号区切Ｗ         PIC X(1)   VALUE SPACE.
003690       05 施術所郵便番号２Ｗ           PIC X(4)   VALUE SPACE.
003700    03 施術所電話番号Ｗ                PIC X(15)  VALUE SPACE.
003710    03 取引先情報Ｗ.
003720        05 取引先銀行名Ｗ              PIC X(40)  VALUE SPACE.
003730        05 取引先銀行支店名Ｗ          PIC X(40)  VALUE SPACE.
003740        05 預金種別Ｗ                  PIC 9(1)   VALUE ZERO.
003750        05 銀行番号Ｗ                  PIC X(4)   VALUE SPACE.
003760        05 店番号Ｗ                    PIC X(3)   VALUE SPACE.
003770        05 口座番号Ｗ                  PIC X(10)  VALUE SPACE.
003780        05 口座名義人カナＷ            PIC X(40)  VALUE SPACE.
003790        05 口座名義人Ｗ                PIC X(40)  VALUE SPACE.
003800*
003810 01 連番Ｗ                             PIC 9(3)   VALUE ZERO.
003820 01 銀行名支店名Ｗ                     PIC X(40)  VALUE SPACE.
003830 01 預金種別コメントＷ                 PIC N(2)   VALUE SPACE.
003840 01 控えＷ                             PIC N(4)   VALUE SPACE.
003850**
003860* 社保用
003870 01 接尾語区分Ｗ                       PIC 9     VALUE ZERO.
003880
003890
003900 01 施術所情報Ｗ２.
003910    03 代表者名Ｗ２                      PIC N(10)  VALUE SPACE.
003920    03 接骨院名Ｗ２                      PIC N(15)  VALUE SPACE.
003930    03 柔整師番号Ｗ２                    PIC X(20)  VALUE SPACE.
003940    03 施術所住所Ｗ２.
003950       05 施術所住所１Ｗ２               PIC X(40)  VALUE SPACE.
003960       05 施術所住所２Ｗ２               PIC X(40)  VALUE SPACE.
003970    03 施術所郵便番号Ｗ２.
003980       05 施術所郵便番号１Ｗ２           PIC X(3)   VALUE SPACE.
003990       05 施術所郵便番号区切Ｗ２         PIC X(1)   VALUE SPACE.
004000       05 施術所郵便番号２Ｗ２           PIC X(4)   VALUE SPACE.
004010    03 施術所電話番号Ｗ２                PIC X(15)  VALUE SPACE.
004020    03 取引先情報Ｗ２.
004030        05 取引先銀行名Ｗ２              PIC X(40)  VALUE SPACE.
004040        05 取引先銀行支店名Ｗ２          PIC X(40)  VALUE SPACE.
004050        05 預金種別Ｗ２                  PIC 9(1)   VALUE ZERO.
004060        05 銀行番号Ｗ２                  PIC X(4)   VALUE SPACE.
004070        05 店番号Ｗ２                    PIC X(3)   VALUE SPACE.
004080        05 口座番号Ｗ２                  PIC X(10)  VALUE SPACE.
004090        05 口座名義人カナＷ２            PIC X(40)  VALUE SPACE.
004100        05 口座名義人Ｗ２                PIC X(40)  VALUE SPACE.
004110*
004120 01 連番Ｗ２                             PIC 9(3)   VALUE ZERO.
004130 01 銀行名支店名Ｗ２                     PIC X(40)  VALUE SPACE.
004140 01 預金種別コメントＷ２                 PIC N(2)   VALUE SPACE.
004150 01 控えＷ２                             PIC N(4)   VALUE SPACE.
004160*
004170 01 保険種別Ｗ２                         PIC 9(2)  VALUE ZERO.
004180 01 保険者番号Ｗ２                       PIC X(10) VALUE SPACE.
004190 01 請求先名称Ｗ２                       PIC X(40) VALUE SPACE.
004200 01 支部部署名Ｗ２                       PIC X(40) VALUE SPACE.
004210 01 宛名Ｗ２                             PIC X(24) VALUE SPACE.
004220 01 保険者宛名Ｗ２.
004230     03 保険者宛名１Ｗ２                 PIC X(40) VALUE SPACE.
004240     03 保険者宛名２Ｗ２                 PIC X(40) VALUE SPACE.
004250*
004260* 社保用
004270 01 接尾語区分Ｗ２                       PIC 9     VALUE ZERO.
004280*
004290 01 協会コードＷ２                       PIC 9(2)  VALUE ZERO.
004300
004310*
004320********************
004330* 保険者別合計金額 *
004340********************
004350 01 保険者別合計金額.
004360    03 金額Ｗ                          PIC N(2)  VALUE SPACE.
004370    03 請求メッセージＷ                PIC N(15) VALUE SPACE.
004380    03 円Ｗ                            PIC N(1)  VALUE SPACE.
004390    03 件数Ｗ                          PIC 9(3)  VALUE ZERO.
004400    03 費用額Ｗ                        PIC 9(8)  VALUE ZERO.
004410    03 請求額Ｗ                        PIC 9(7)  VALUE ZERO.
004420    03 請求先名称Ｗ                    PIC X(40) VALUE SPACE.
004430    03 支部部署名Ｗ                    PIC X(40) VALUE SPACE.
004440    03 宛名Ｗ                          PIC X(24) VALUE SPACE.
004450    03 保険者宛名Ｗ.
004460       05 保険者宛名１Ｗ               PIC X(40) VALUE SPACE.
004470       05 保険者宛名２Ｗ               PIC X(40) VALUE SPACE.
004480**
004490 01 画面情報４３０Ｗ.
004500    03 請求年月Ｗ.
004510       05 請求和暦Ｗ                   PIC 9     VALUE ZERO.
004520       05 請求年Ｗ                     PIC 9(2)  VALUE ZERO.
004530       05 請求月Ｗ                     PIC 9(2)  VALUE ZERO.
004540    03 提出年月日Ｗ.
004550       05 提出和暦Ｗ                   PIC 9     VALUE ZERO.
004560       05 提出年Ｗ                     PIC 9(2)  VALUE ZERO.
004570       05 提出月Ｗ                     PIC 9(2)  VALUE ZERO.
004580       05 提出日Ｗ                     PIC 9(2)  VALUE ZERO.
004590    03 印刷種類Ｗ                      PIC 9     VALUE ZERO.
004600***
004610* エラーメッセージ用
004620 01 エラーメッセージＷ.
004630    03 エラー保険種別Ｗ                PIC X(2) VALUE SPACE.
004640    03 エラー区切りＷ                  PIC X(1) VALUE SPACE.
004650    03 エラー保険者番号Ｗ              PIC X(10) VALUE SPACE.
004660    03 FILLER                          PIC X(7) VALUE SPACE.
004670*
004680***
004690* 請求書印刷パラメタ用
004700*  ６・７号請求書（印刷区分 0:印刷 1:印刷しない、振込先 0:自分 1:会 9:印刷しない）
004710*  当社用紙初期値1:印刷しない
004720***
004730 01 請求書関連Ｗ.
004740        07 国保６号印刷区分Ｗ          PIC 9 VALUE ZERO.
004750        07 国保７号印刷区分Ｗ          PIC 9 VALUE ZERO.
004760        07 国保当社印刷区分Ｗ          PIC 9 VALUE 1.
004770        07 国保振込先区分Ｗ            PIC 9 VALUE ZERO.
004780        07 社保６号印刷区分Ｗ          PIC 9 VALUE ZERO.
004790        07 社保７号印刷区分Ｗ          PIC 9 VALUE ZERO.
004800        07 社保当社印刷区分Ｗ          PIC 9 VALUE 1.
004810        07 社保振込先区分Ｗ            PIC 9 VALUE ZERO.
004820        07 組合６号印刷区分Ｗ          PIC 9 VALUE ZERO.
004830        07 組合７号印刷区分Ｗ          PIC 9 VALUE ZERO.
004840        07 組合当社印刷区分Ｗ          PIC 9 VALUE 1.
004850        07 組合振込先区分Ｗ            PIC 9 VALUE ZERO.
004860        07 共済６号印刷区分Ｗ          PIC 9 VALUE ZERO.
004870        07 共済７号印刷区分Ｗ          PIC 9 VALUE ZERO.
004880        07 共済当社印刷区分Ｗ          PIC 9 VALUE 1.
004890        07 共済振込先区分Ｗ            PIC 9 VALUE ZERO.
004900        07 老人６号印刷区分Ｗ          PIC 9 VALUE ZERO.
004910        07 老人７号印刷区分Ｗ          PIC 9 VALUE ZERO.
004920        07 老人当社印刷区分Ｗ          PIC 9 VALUE 1.
004930        07 老人振込先区分Ｗ            PIC 9 VALUE ZERO.
004940        07 助成６号印刷区分Ｗ          PIC 9 VALUE 1.
004950        07 助成７号印刷区分Ｗ          PIC 9 VALUE ZERO.
004960        07 助成当社印刷区分Ｗ          PIC 9 VALUE 1.
004970        07 助成振込先区分Ｗ            PIC 9 VALUE ZERO.
004980*
004990***********************************************************************
005000 01 印刷制御.
005010     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
005020     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
005030     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
005040     03 拡張制御Ｐ.
005050         05 端末制御Ｐ.
005060             07 移動方向Ｐ             PIC X(1).
005070             07 移動行数Ｐ             PIC 9(3).
005080         05 詳細制御Ｐ                 PIC X(2).
005090     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
005100     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
005110*
005120 01 計算機西暦年Ｗ                     PIC 9(2).
005130* 日付ＷＯＲＫ
005140 01 和暦終了年Ｗ                       PIC 9(4).
005150 01 計算機西暦.
005160    03 計算機西暦年                    PIC 9(4).
005170    03 計算機西暦月日                  PIC 9(4).
005180 01 計算機西暦Ｒ REDEFINES 計算機西暦.
005190    03 計算機世紀                      PIC 9(2).
005200    03 計算機日付                      PIC 9(6).
005210    03 計算機日付Ｒ REDEFINES 計算機日付.
005220       05 計算機年月                   PIC 9(4).
005230       05 計算機年月Ｒ REDEFINES 計算機年月.
005240         07 計算機年                   PIC 9(2).
005250         07 計算機月                   PIC 9(2).
005260       05 計算機日                     PIC 9(2).
005270*
005280******************************************************************
005290*                          連結項目                              *
005300******************************************************************
005310*
005320********************
005330* メッセージ表示キー *
005340********************
005350 01 連メ－キー IS EXTERNAL.
005360    03  連メ－メッセージ               PIC N(20).
005370*
005380 01 連メ３－キー IS EXTERNAL.
005390    03  連メ３－メッセージ             PIC N(20).
005400    03  連メ３－メッセージ１           PIC X(20).
005410*
005420*
005430 01 連入－画面情報ＹＪＫ４３０   IS EXTERNAL.
005440    03 連入－請求年月.
005450       05 連入－請求和暦               PIC 9.
005460       05 連入－請求年                 PIC 9(2).
005470       05 連入－請求月                 PIC 9(2).
005480    03 連入－提出年月日.
005490       05 連入－提出和暦               PIC 9.
005500       05 連入－提出年                 PIC 9(2).
005510       05 連入－提出月                 PIC 9(2).
005520       05 連入－提出日                 PIC 9(2).
005530    03 連入－レセプト種類              PIC X(4).
005540    03 連入－保険種別                  PIC 9(2).
005550    03 連入－印刷種類                  PIC 9.
005560    03 連入－本人家族                  PIC 9.
005570    03 連入－用紙種類                  PIC 9.
005580    03 連入－県内県外                  PIC 9.
005590    03 連入－県ＪＩＳ                  PIC X(2).
005600    03 連入－政管ＪＩＳ                PIC X(2).
005610*
005620 01 連入－画面情報ＹＪＫ４３０追加   IS EXTERNAL.
005630    03 連入－一括区分    PIC 9.
005640    03 連入－プレビュー区分            PIC 9.
005642*
005643************************************
005644* プリンタファイル作成用           *
005645************************************
005646 01 Ｈ連ＰＲＴＦ－作成データ IS EXTERNAL.
005647     03 Ｈ連ＰＲＴＦ－ファイル名           PIC X(8).
005648     03 Ｈ連ＰＲＴＦ－プレビュー区分       PIC 9.
005649     03 Ｈ連ＰＲＴＦ－帳票プログラム名     PIC X(8).
005650     03 Ｈ連ＰＲＴＦ－オーバレイ名         PIC X(8).
005651*
005652******************************************************************
005660*                      PROCEDURE  DIVISION                       *
005670******************************************************************
005680 PROCEDURE               DIVISION.
005690************
005700*           *
005710* 初期処理   *
005720*           *
005730************
005740     PERFORM プリンタファイル作成.
005741     PERFORM プリンタファイル作成２.
005742     PERFORM 初期化.
005750     PERFORM 制御情報取得２.
005760     PERFORM 作業ファイル３作成.
005770************
005780*           *
005790* 主処理     *
005800*           *
005810************
005820     IF レセ並び順昇降Ｗ = ZERO
005830         PERFORM 印刷処理２
005840     ELSE
005850         PERFORM 印刷処理
005860     END-IF.
005870************
005880*           *
005890* 終了処理   *
005900*           *
005910************
005920     PERFORM 終了処理.
005930     EXIT PROGRAM.
005940*
005950*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
005960*================================================================*
005961 プリンタファイル作成 SECTION.
005962*================================================================*
005963*   / 初期化 /
005964     MOVE SPACE TO Ｈ連ＰＲＴＦ－作成データ.
005965     INITIALIZE Ｈ連ＰＲＴＦ－作成データ.
005966*
005967*
005968*--↓↓ 変更箇所 ↓↓--------------------------------------*
005969*   使用するプリンタファイル名セット
005970     MOVE "PRTF001"             TO Ｈ連ＰＲＴＦ－ファイル名.
005971*
005972*   使用する帳票プログラム名セット
005973     MOVE "YJK436"              TO Ｈ連ＰＲＴＦ－帳票プログラム名.
005974*
005975*--↑↑-----------------------------------------------------*
005976*
005977*   / プレビュー区分セット /
005978     MOVE 連入－プレビュー区分 TO Ｈ連ＰＲＴＦ－プレビュー区分.
005979*
005980     CALL   "CRTPRTF".
005981     CANCEL "CRTPRTF".
005982*
006006*================================================================*
006007 プリンタファイル作成２ SECTION.
006008*================================================================*
006009*   / 初期化 /
006010     MOVE SPACE TO Ｈ連ＰＲＴＦ－作成データ.
006011     INITIALIZE Ｈ連ＰＲＴＦ－作成データ.
006012*
006013*
006014*--↓↓ 変更箇所 ↓↓--------------------------------------*
006015*   使用するプリンタファイル名セット
006016     MOVE "PRTF001"             TO Ｈ連ＰＲＴＦ－ファイル名.
006017*
006018*   使用する帳票プログラム名セット
006019     MOVE "YJK437"              TO Ｈ連ＰＲＴＦ－帳票プログラム名.
006020*
006021*--↑↑-----------------------------------------------------*
006022*
006023*   / プレビュー区分セット /
006024     MOVE 連入－プレビュー区分 TO Ｈ連ＰＲＴＦ－プレビュー区分.
006025*
006026     CALL   "CRTPRTF".
006027     CANCEL "CRTPRTF".
006028*
006029*================================================================*
006030 初期化 SECTION.
006031*
006032     PERFORM ファイルオープン.
006033*    /* 現在日付取得 */
006034     ACCEPT 計算機日付 FROM DATE.
006035*    /* 1980～2079年の間で設定 */
006036     IF 計算機年 > 80
006040         MOVE 19 TO 計算機世紀
006050     ELSE
006060         MOVE 20 TO 計算機世紀
006070     END-IF.
006080*
006090     PERFORM 連結項目退避.
006100     PERFORM 施術所情報取得.
006110     PERFORM カレント元号取得.
006120     PERFORM 和暦終了年取得.
006130***     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
006140*================================================================*
006150 カレント元号取得 SECTION.
006160*
006170     MOVE ZEROS TO 制－制御区分.
006180     READ 制御情報マスタ
006190     NOT INVALID KEY
006200         MOVE 制－カレント元号   TO カレント元号Ｗ
006210         MOVE 制－レセ並び順昇降 TO レセ並び順昇降Ｗ
006220     END-READ.
006230*
006240*================================================================*
006250 和暦終了年取得 SECTION.
006260*
006270*     DISPLAY NC"カレント元号Ｗ"  カレント元号Ｗ UPON MSGBOX.
006280     MOVE カレント元号Ｗ TO 元－元号区分.
006290     READ 元号マスタ
006300     INVALID KEY
006310         DISPLAY NC"指定和暦が登録されていません" UPON CONS
006320         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
006330                                                  UPON CONS
006340         ACCEPT  キー入力 FROM CONS
006350         PERFORM 終了処理
006360         EXIT PROGRAM
006370     NOT INVALID KEY
006380         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
006390         MOVE 前和暦Ｗ TO 元－元号区分
006400         READ 元号マスタ
006410         INVALID KEY
006420             DISPLAY NC"指定和暦が登録されていません" UPON CONS
006430             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
006440                                                      UPON CONS
006450             ACCEPT  キー入力 FROM CONS
006460             PERFORM 終了処理
006470             EXIT PROGRAM
006480         NOT INVALID KEY
006490             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
006500         END-READ
006510     END-READ.
006520*
006530*================================================================*
006540 連結項目退避 SECTION.
006550*
006560     MOVE 連入－請求和暦  TO 請求和暦Ｗ.
006570     MOVE 連入－請求年    TO 請求年Ｗ.
006580     MOVE 連入－請求月    TO 請求月Ｗ.
006590     MOVE 連入－提出和暦  TO 提出和暦Ｗ.
006600     MOVE 連入－提出年    TO 提出年Ｗ.
006610     MOVE 連入－提出月    TO 提出月Ｗ.
006620     MOVE 連入－提出日    TO 提出日Ｗ.
006630     MOVE 連入－印刷種類  TO 印刷種類Ｗ.
006640*
006650*================================================================*
006660 施術所情報取得 SECTION.
006670*
006680     MOVE ZERO  TO 施情－施術所番号.
006690     READ 施術所情報マスタ
006700     INVALID KEY
006710         CONTINUE
006720     NOT INVALID KEY
006730*
006740         MOVE 施情－郵便番号１       TO 施術所郵便番号１Ｗ
006750         MOVE "-"                    TO 施術所郵便番号区切Ｗ
006760         MOVE 施情－郵便番号２       TO 施術所郵便番号２Ｗ
006770         MOVE 施情－代表者名         TO 代表者名Ｗ
006780         MOVE 施情－接骨院名         TO 接骨院名Ｗ
006790         STRING 施情－住所１  DELIMITED BY SPACE
006800                施情－住所２  DELIMITED BY SPACE
006810           INTO 施術所住所Ｗ
006820         END-STRING
006830         MOVE 施情－電話番号         TO 施術所電話番号Ｗ
006840         MOVE 施情－新柔整師番号     TO 柔整師番号Ｗ
006850*
006860         MOVE 施情－取引先銀行名     TO 取引先銀行名Ｗ
006870         MOVE 施情－取引先銀行支店名 TO 取引先銀行支店名Ｗ
006880         MOVE 施情－預金種別         TO 預金種別Ｗ
006890         MOVE 施情－銀行番号         TO 銀行番号Ｗ
006900         MOVE 施情－店番号           TO 店番号Ｗ
006910         MOVE 施情－口座番号         TO 口座番号Ｗ
006920         MOVE 施情－口座名義人カナ   TO 口座名義人カナＷ
006930         MOVE 施情－口座名義人       TO 口座名義人Ｗ
006940         STRING 取引先銀行名Ｗ     DELIMITED BY SPACE
006950                " "                DELIMITED BY SIZE
006960                取引先銀行支店名Ｗ DELIMITED BY SPACE
006970                INTO 銀行名支店名Ｗ
006980         END-STRING
006990         EVALUATE 預金種別Ｗ
007000         WHEN 1
007010             MOVE NC"普通" TO 預金種別コメントＷ
007020         WHEN 2
007030             MOVE NC"当座" TO 預金種別コメントＷ
007040         WHEN OTHER
007050             MOVE SPACE    TO 預金種別コメントＷ
007060         END-EVALUATE
007070*
007080     END-READ.
007090*================================================================*
007100 ファイルオープン SECTION.
007110*
007120     OPEN INPUT   元号マスタ
007130         MOVE NC"元号" TO ファイル名.
007140         PERFORM オープンチェック.
007150     OPEN INPUT  制御情報マスタ.
007160         MOVE NC"制御情報" TO ファイル名.
007170         PERFORM オープンチェック.
007180     OPEN INPUT   保険者マスタ
007190         MOVE NC"保険" TO ファイル名.
007200         PERFORM オープンチェック.
007210     OPEN INPUT   市町村マスタ
007220         MOVE NC"市町" TO ファイル名.
007230         PERFORM オープンチェック.
007270     OPEN INPUT   施術所情報マスタ
007280         MOVE NC"施情" TO ファイル名.
007290         PERFORM オープンチェック.
007300     OPEN INPUT   請求先マスタ
007310         MOVE NC"請先" TO ファイル名.
007320         PERFORM オープンチェック.
007330     OPEN INPUT   会情報マスタ
007340         MOVE NC"会情報" TO ファイル名.
007350         PERFORM オープンチェック.
007360     OPEN INPUT 保険者拡張マスタ.
007370         MOVE NC"保拡" TO ファイル名.
007380         PERFORM オープンチェック.
007410*================================================================*
007420 オープンチェック SECTION.
007430*
007440     IF 状態キー  NOT =  "00"
007450         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
007460         DISPLAY NC"状態キー：" 状態キー         UPON CONS
007470         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
007480                                                 UPON CONS
007490         ACCEPT  キー入力 FROM CONS
007500         PERFORM ファイル閉鎖
007510         EXIT PROGRAM.
007520*================================================================*
007530 ファイル閉鎖 SECTION.
007540*
007550     CLOSE 印刷ファイル.
007560     CLOSE 元号マスタ     制御情報マスタ   保険者マスタ
007570           市町村マスタ   施術所情報マスタ 請求先マスタ  
007580           保険者拡張マスタ 会情報マスタ.
007590*================================================================*
007600 終了処理 SECTION.
007610*
007620     PERFORM ファイル閉鎖.
007630*================================================================*
007640 エラー表示 SECTION.
007650*
007660     DISPLAY NC"状態キー" 状態キー  UPON CONS.
007670     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
007680     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
007690     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
007700     ACCEPT  キー入力 FROM CONS.
007710     PERFORM ファイル閉鎖.
007720     EXIT PROGRAM.
007730*
007740*================================================================*
007750 印刷処理 SECTION.
007760*
007770* ７号用紙を降順に印刷し、印刷順番の区切りで６号を印刷する
007780
007790     OPEN INPUT  作業ファイル２.
007800         MOVE NC"作２" TO ファイル名.
007810         PERFORM オープンチェック.
007820     OPEN INPUT  作業ファイル３.
007830         MOVE NC"作３" TO ファイル名.
007840         PERFORM オープンチェック.
007850
007860* / 副キー(印刷順序+保険種別+保険者番号)で読み込む./
      */20190603
007870*     MOVE 4      TO  作２－請求和暦.
007870     MOVE 9      TO  作２－請求和暦.
007880     MOVE 99      TO  作２－請求年.
007890     MOVE 12      TO  作２－請求月.
007900*     MOVE LOW-VALUE TO  作２－県.
007910     MOVE 99      TO  作２－印刷順序.
007920     MOVE 99      TO  作２－保険種別.
007930     MOVE HIGH-VALUE TO  作２－保険者番号.
007940*/県別、社保日雇いをまとめ、船員を別に集計↓↓/081021
007950     MOVE 99      TO  作２－県２.
007960*     START 作業ファイル２   KEY IS >=  作２－請求和暦年月
007970*     START 作業ファイル２   KEY IS <=  作２－請求和暦年月
007980*                                       作２－印刷順序
007990*                                       作２－保険種別
008000*                                       作２－保険者番号
008010*                                       REVERSED
008020**                                       作２－県
008030     START 作業ファイル２   KEY IS <=  作２－請求和暦年月
008040                                       作２－県２
008050                                       作２－印刷順序
008060                                       作２－保険種別
008070                                       作２－保険者番号
008080                                       REVERSED
008090*/県別、社保日雇いをまとめ、船員を別に集計↑↑/081021
008100     END-START.
008110     IF 状態キー = "00"
008120         MOVE SPACE TO 終了フラグ
008130         PERFORM 作業ファイル２読込
008140         IF  終了フラグ = "YES"
008150             MOVE  NC"１　該当データなしです。確認して下さい。" TO 連メ－メッセージ
008160             CALL   "MSG001"
008170             CANCEL "MSG001"
008180             PERFORM ファイル閉鎖
008190             MOVE 99 TO PROGRAM-STATUS
008200             EXIT PROGRAM
008210         END-IF
008220*
008230
008240         PERFORM UNTIL  終了フラグ = "YES"
008250                PERFORM 印刷対象チェック
008260
008270             MOVE 作２－県         TO 県Ｗ
008280             MOVE 作２－印刷順序 TO 印刷順序Ｗ
008290
008300* 社保、船員、日雇と、共済、自衛官はまとめて印字する
008310            IF 印刷順序Ｗ(2:1) = 1 OR 9
008320
008330                PERFORM UNTIL ( 終了フラグ =  "YES" ) OR
008340                    ( 印刷順序Ｗ(2:1) NOT = 作２－印刷順序(2:1) ) OR
008350                    ( 県Ｗ       NOT = 作２－県 )
008360                    IF 印刷フラグ = "YES"
008370                        MOVE SPACE TO YJK436P
008380****                INITIALIZE YJK436P
008390                        PERFORM ヘッダセット
008400                        PERFORM 明細セット
008410                        PERFORM 印字処理
008420                        PERFORM 改頁処理
008430
008440*/印刷対象でない時もセットするコメント↓↓/081023
008450*                        MOVE 作２－印刷順序    TO 印刷順序Ｗ２
008460*                        MOVE 作２－保険種別    TO 保険種別Ｗ２
008470*                        MOVE 作２－保険者番号  TO 保険者番号Ｗ２
008480*                        MOVE 作２－県２        TO 県Ｗ３
008490*/印刷対象でない時もセットするコメント↑↑/081023
008500
008510                    END-IF
008520*/印刷対象でない時もセットする↓↓/081023
008530                    MOVE 作２－印刷順序    TO 印刷順序Ｗ２
008540                    MOVE 作２－保険種別    TO 保険種別Ｗ２
008550                    MOVE 作２－保険者番号  TO 保険者番号Ｗ２
008560*/県別、社保日雇いをまとめ、船員を別に集計/081020
008570                    MOVE 作２－県２        TO 県Ｗ３
008580*/印刷対象でない時もセットする↑↑/081023
008590
008600                    PERFORM 作業ファイル２読込
008610
008620                END-PERFORM
008630
008640                PERFORM ６号印刷処理
008650
008660            ELSE
008670                PERFORM UNTIL ( 終了フラグ =  "YES" ) OR
008680                    ( 印刷順序Ｗ NOT = 作２－印刷順序 ) OR
008690                    ( 県Ｗ       NOT = 作２－県 )
008700                    IF 印刷フラグ = "YES"
008710                        MOVE SPACE TO YJK436P
008720****                INITIALIZE YJK436P
008730                        PERFORM ヘッダセット
008740                        PERFORM 明細セット
008750                        PERFORM 印字処理
008760                        PERFORM 改頁処理
008770
008780*/印刷対象でない時もセットするコメント↓↓/081023
008790*                        MOVE 作２－印刷順序    TO 印刷順序Ｗ２
008800*                        MOVE 作２－保険種別    TO 保険種別Ｗ２
008810*                        MOVE 作２－保険者番号  TO 保険者番号Ｗ２
008820*                        MOVE 作２－県２        TO 県Ｗ３
008830*/印刷対象でない時もセットするコメント↑↑/081023
008840                    END-IF
008850*/印刷対象でない時もセットする↓↓/081023
008860                    MOVE 作２－印刷順序    TO 印刷順序Ｗ２
008870                    MOVE 作２－保険種別    TO 保険種別Ｗ２
008880                    MOVE 作２－保険者番号  TO 保険者番号Ｗ２
008890*/県別、社保日雇いをまとめ、船員を別に集計/081020
008900                    MOVE 作２－県２        TO 県Ｗ３
008910*/印刷対象でない時もセットする↑↑/081023
008920                    PERFORM 作業ファイル２読込
008930                
008940                END-PERFORM
008950                PERFORM ６号印刷処理
008960            END-IF
008970                
008980*                PERFORM 作業ファイル２読込
008990                
009000
009010         END-PERFORM
009020*
009030     ELSE
009040         MOVE  NC"２　該当データなしです。確認して下さい。" TO 連メ－メッセージ
009050         CALL   "MSG001"
009060         CANCEL "MSG001"
009070         PERFORM ファイル閉鎖
009080         MOVE 99 TO PROGRAM-STATUS
009090         EXIT PROGRAM
009100     END-IF.
009110*
009120     CLOSE 作業ファイル２ 作業ファイル３.
009130*
009140*================================================================*
009150 印刷処理２ SECTION.
009160*
009170* ６号用紙を昇順に印刷し、印刷順番の区切りで７号用紙を印刷する
009180
009190     OPEN INPUT  作業ファイル２.
009200         MOVE NC"作２" TO ファイル名.
009210         PERFORM オープンチェック.
009220     OPEN INPUT  作業ファイル３.
009230         MOVE NC"作３" TO ファイル名.
009240         PERFORM オープンチェック.
009250
009260* / 副キー(印刷順序+保険種別+保険者番号)で読み込む./
009270     MOVE ZERO       TO  作２－請求和暦.
009280     MOVE ZERO      TO  作２－請求年.
009290     MOVE ZERO      TO  作２－請求月.
009300*     MOVE LOW-VALUE TO  作２－県.
009310     MOVE ZERO      TO  作２－印刷順序.
009320     MOVE ZERO      TO  作２－保険種別.
009330     MOVE LOW-VALUE TO  作２－保険者番号.
009340*/県別、社保日雇いをまとめ、船員を別に集計↓↓/081021
009350*     MOVE ZERO      TO  作２－県２.
009360*     START 作業ファイル２   KEY IS >=  作２－請求和暦年月
009370*                                       作２－印刷順序
009380*                                       作２－保険種別
009390*                                       作２－保険者番号
009400     START 作業ファイル２   KEY IS >=  作２－請求和暦年月
009410                                       作２－県２
009420                                       作２－印刷順序
009430                                       作２－保険種別
009440                                       作２－保険者番号
009450*/県別、社保日雇いをまとめ、船員を別に集計↑↑/081021
009460     END-START.
009470     IF 状態キー = "00"
009480         MOVE SPACE TO 終了フラグ
009490         PERFORM 作業ファイル２読込
009500         IF  終了フラグ = "YES"
009510             MOVE  NC"３　該当データなしです。確認して下さい。" TO 連メ－メッセージ
009520             CALL   "MSG001"
009530             CANCEL "MSG001"
009540             PERFORM ファイル閉鎖
009550             MOVE 99 TO PROGRAM-STATUS
009560             EXIT PROGRAM
009570         END-IF
009580*
009590
009600         PERFORM UNTIL  終了フラグ = "YES"
009610                PERFORM 印刷対象チェック
009620
009630             MOVE 作２－県         TO 県Ｗ
009640             MOVE 作２－印刷順序 TO 印刷順序Ｗ 印刷順序Ｗ２
009650*/県別、社保日雇いをまとめ、船員を別に集計/081020
009660             MOVE 作２－県２     TO 県Ｗ３
009670
009680             MOVE 作２－保険種別    TO 保険種別Ｗ２
009690             MOVE 作２－保険者番号  TO 保険者番号Ｗ２
009700
009710             PERFORM ６号印刷処理
009720
009730
009740* 社保、船員、日雇と、共済、自衛官はまとめて印字する
009750            IF 印刷順序Ｗ(2:1) = 1 OR 9
009760
009770                PERFORM UNTIL ( 終了フラグ =  "YES" ) OR
009780                    ( 印刷順序Ｗ(2:1) NOT = 作２－印刷順序(2:1) ) OR
009790                    ( 県Ｗ       NOT = 作２－県 )
009800                    IF 印刷フラグ = "YES"
009810                        MOVE SPACE TO YJK436P
009820****                INITIALIZE YJK436P
009830                        PERFORM ヘッダセット
009840                        PERFORM 明細セット
009850                        PERFORM 印字処理
009860                        PERFORM 改頁処理
009870                        
009880*                        MOVE 作２－印刷順序    TO 印刷順序Ｗ２
009890*                        MOVE 作２－保険種別    TO 保険種別Ｗ２
009900*                        MOVE 作２－保険者番号  TO 保険者番号Ｗ２
009910                        
009920                    END-IF
009930                
009940                    PERFORM 作業ファイル２読込
009950                
009960                END-PERFORM
009970
009980
009990            ELSE
010000                PERFORM UNTIL ( 終了フラグ =  "YES" ) OR
010010                    ( 印刷順序Ｗ NOT = 作２－印刷順序 ) OR
010020                    ( 県Ｗ       NOT = 作２－県 )
010030                    IF 印刷フラグ = "YES"
010040                        MOVE SPACE TO YJK436P
010050****                INITIALIZE YJK436P
010060                        PERFORM ヘッダセット
010070                        PERFORM 明細セット
010080                        PERFORM 印字処理
010090                        PERFORM 改頁処理
010100                        
010110*                        MOVE 作２－印刷順序    TO 印刷順序Ｗ２
010120*                        MOVE 作２－保険種別    TO 保険種別Ｗ２
010130*                        MOVE 作２－保険者番号  TO 保険者番号Ｗ２
010140                        
010150                    END-IF
010160                    PERFORM 作業ファイル２読込
010170                
010180                END-PERFORM
010190*                PERFORM ６号印刷処理
010200            END-IF
010210                
010220*                PERFORM 作業ファイル２読込
010230                
010240
010250         END-PERFORM
010260*
010270     ELSE
010280         MOVE  NC"４　該当データなしです。確認して下さい。" TO 連メ－メッセージ
010290         CALL   "MSG001"
010300         CANCEL "MSG001"
010310         PERFORM ファイル閉鎖
010320         MOVE 99 TO PROGRAM-STATUS
010330         EXIT PROGRAM
010340     END-IF.
010350*
010360     CLOSE 作業ファイル２ 作業ファイル３.
010370*
010380*================================================================*
010390 ６号印刷処理 SECTION.
010400*
010440     MOVE 1       TO 頁Ｗ.
010450*
010460* / 副キー(印刷順序+保険種別+保険者番号)で読み込む./
010470     MOVE 作２－請求和暦    TO  作３－請求和暦.
010480     MOVE 作２－請求年      TO  作３－請求年.
010490     MOVE 作２－請求月      TO  作３－請求月.
010500*     MOVE LOW-VALUE TO  作２－県.
010510     MOVE 印刷順序Ｗ２      TO  作３－印刷順序.
010520     MOVE 保険種別Ｗ２      TO  作３－保険種別.
010530     MOVE 保険者番号Ｗ２    TO  作３－保険者番号.
010540*/県別、社保日雇いをまとめ、船員を別に集計↓↓/081021
010550     MOVE 県Ｗ３            TO  作３－県２.
010560*     START 作業ファイル３   KEY IS >=  作３－請求和暦年月
010570*                                       作３－印刷順序
010580*                                       作３－保険種別
010590*                                       作３－保険者番号
010600**                                       作２－県
010610     START 作業ファイル３   KEY IS >=  作３－請求和暦年月
010620                                       作３－県２
010630                                       作３－印刷順序
010640                                       作３－保険種別
010650                                       作３－保険者番号
010660*/県別、社保日雇いをまとめ、船員を別に集計↑↑/081021
010670     END-START.
010680     IF 状態キー = "00"
010690         MOVE SPACE TO 終了フラグ３
010700         PERFORM 作業ファイル３読込
010710         IF  終了フラグ３ = "YES"
010720             MOVE  NC"５　該当データなしです。確認して下さい。" TO 連メ－メッセージ
010730             CALL   "MSG001"
010740             CANCEL "MSG001"
010750             PERFORM ファイル閉鎖
010760             MOVE 99 TO PROGRAM-STATUS
010770             EXIT PROGRAM
010780         END-IF
010790*
010800         PERFORM UNTIL 終了フラグ３ = "YES" 
010810             MOVE SPACE TO スキップフラグ
010820             PERFORM 印刷対象チェック２
010830             IF 印刷フラグ２ = "YES"
010840*
010850                 MOVE SPACE TO YJK437P
010860                 INITIALIZE    YJK437P
010870                 MOVE 作３－県         TO 県Ｗ２
010880                 MOVE 作３－印刷順序 TO 印刷順序Ｗ３
010890                 PERFORM  表セット１
010900*                        DISPLAY 印刷順序Ｗ３
010910*                        DISPLAY 作３－保険種別
010920*                        DISPLAY 作３－保険者番号
010930
010940* 社保、船員、日雇と、共済、自衛官はまとめて印字する
010950                 IF 印刷順序Ｗ３(2:1) = 1 OR 9
010960* 
010970                     PERFORM VARYING 行カウンタ FROM 1 BY 1
010980                             UNTIL ( 行カウンタ > 8       ) OR
010990                                   ( 県Ｗ２       NOT = 作３－県       ) OR
011000                                   ( 印刷順序Ｗ３(2:1) NOT = 作３－印刷順序(2:1) ) OR
011010                                   ( 終了フラグ３ = "YES" )
011020*/県別、社保日雇いをまとめ、船員を別に集計/081022
011030                                OR ( 県Ｗ３   NOT = 作３－県２)
011040                          PERFORM 印刷対象チェック２
011050                          IF 印刷フラグ２ = "YES"
011060                              PERFORM 表セット２
011070*/↓↓改頁後、レコードがあり、印刷対象がない場合、合計が印刷されない/0510
011080*                  ELSE
011090*                     COMPUTE 行カウンタ =  行カウンタ - 1
011100                          END-IF
011110                          PERFORM 作業ファイル３読込
011120                          PERFORM 印刷対象チェック２
011130                          IF 印刷フラグ２ = SPACE
011140                              COMPUTE 行カウンタ =  行カウンタ - 1
011150                          END-IF
011160*/↑↑改頁後、レコードがあり、印刷対象がない場合、合計が印刷されない/0510
011170                          MOVE "YES" TO スキップフラグ
011180                     END-PERFORM
011190*
011200                     IF ( 終了フラグ３ =  "YES" ) OR 
011210                        ( 印刷順序Ｗ３(2:1) NOT = 作３－印刷順序(2:1) ) OR
011220                        ( 県Ｗ２       NOT = 作３－県 )
011230*/県別、社保日雇いをまとめ、船員を別に集計/081022
011240                     OR ( 県Ｗ３       NOT = 作３－県２)
011250                         MOVE 本人件数カウント   TO     本人件数合計
011260                         MOVE 家族件数カウント   TO     家族件数合計
011270                         MOVE 計件数カウント     TO     計件数合計
011280                         MOVE 本人費用額カウント TO     本人費用額合計
011290                         MOVE 家族費用額カウント TO     家族費用額合計
011300                         MOVE 計費用額カウント   TO     計費用額合計
011310                         MOVE ZERO   TO 頁Ｗ
011320                         
011330                         MOVE "YES"  TO 終了フラグ３
011340*
011350                         INITIALIZE 合計用Ｗ
011360                     ELSE
011370                         MOVE ZERO     TO     本人件数合計
011380                         MOVE ZERO     TO     家族件数合計
011390                         MOVE ZERO     TO     計件数合計
011400                         MOVE ZERO     TO     本人費用額合計
011410                         MOVE ZERO     TO     家族費用額合計
011420                         MOVE ZERO     TO     計費用額合計
011430                         MOVE ZERO     TO     本人請求額合計
011440                         MOVE ZERO     TO     家族請求額合計
011450                         MOVE ZERO     TO     計請求額合計
011460                     END-IF
011470
011480                 ELSE
011490* 社保、船員、日雇、共済、自衛官以外
011500*                
011510                     PERFORM VARYING 行カウンタ FROM 1 BY 1
011520                             UNTIL ( 行カウンタ > 8       ) OR
011530                                   ( 県Ｗ２       NOT = 作３－県       ) OR
011540                                   ( 印刷順序Ｗ３ NOT = 作３－印刷順序 ) OR
011550                                   ( 終了フラグ３ = "YES" )
011560                         PERFORM 印刷対象チェック２
011570                         IF 印刷フラグ２ = "YES"
011580                             PERFORM 表セット２
011590*/↓↓改頁後、レコードがあり、印刷対象がない場合、合計が印刷されない/0510
011600*                  ELSE
011610*                     COMPUTE 行カウンタ =  行カウンタ - 1
011620                         END-IF
011630                         PERFORM 作業ファイル３読込
011640                         PERFORM 印刷対象チェック２
011650                         IF 印刷フラグ２ = SPACE
011660                             COMPUTE 行カウンタ =  行カウンタ - 1
011670                         END-IF
011680*/↑↑改頁後、レコードがあり、印刷対象がない場合、合計が印刷されない/0510
011690                         MOVE "YES" TO スキップフラグ
011700                     END-PERFORM
011710*
011720                     IF ( 終了フラグ３ =  "YES" ) OR 
011730                        ( 印刷順序Ｗ３ NOT = 作３－印刷順序 ) OR
011740                        ( 県Ｗ２       NOT = 作３－県 )
011750                         MOVE 本人件数カウント   TO     本人件数合計
011760                         MOVE 家族件数カウント   TO     家族件数合計
011770                         MOVE 計件数カウント     TO     計件数合計
011780                         MOVE 本人費用額カウント TO     本人費用額合計
011790                         MOVE 家族費用額カウント TO     家族費用額合計
011800                         MOVE 計費用額カウント   TO     計費用額合計
011810                         MOVE ZERO                 TO     頁Ｗ
011820                         
011830                         MOVE "YES"  TO 終了フラグ３
011840*
011850                         INITIALIZE 合計用Ｗ
011860                     ELSE
011870                         MOVE ZERO     TO     本人件数合計
011880                         MOVE ZERO     TO     家族件数合計
011890                         MOVE ZERO     TO     計件数合計
011900                         MOVE ZERO     TO     本人費用額合計
011910                         MOVE ZERO     TO     家族費用額合計
011920                         MOVE ZERO     TO     計費用額合計
011930                         MOVE ZERO     TO     本人請求額合計
011940                         MOVE ZERO     TO     家族請求額合計
011950                         MOVE ZERO     TO     計請求額合計
011960                     END-IF
011970
011980                 END-IF
011990*
012000                 PERFORM 印字処理２
012010                 PERFORM 改頁処理２
012020*
012030                 COMPUTE 頁Ｗ = 頁Ｗ + 1
012040*/印刷対象でない場合終わる/081023
012050             ELSE
012060                 MOVE "YES" TO 終了フラグ３
012070             END-IF
012080*
012090             IF スキップフラグ NOT = "YES"
012100                 PERFORM 作業ファイル３読込
012110             END-IF
012120         END-PERFORM
012130*
012140     ELSE
012150         MOVE  NC"６　該当データなしです。確認して下さい。" TO 連メ－メッセージ
012160         CALL   "MSG001"
012170         CANCEL "MSG001"
012180         PERFORM ファイル閉鎖
012190         MOVE 99 TO PROGRAM-STATUS
012200         EXIT PROGRAM
012210     END-IF.
012220*
012230     CLOSE 印刷ファイル２.
012240     MOVE SPACE TO オープンフラグ２.
012241*
012250*================================================================*
012260 作業ファイル２読込 SECTION.
012270*
012280     READ 作業ファイル２ NEXT
012290     AT END
012300         MOVE "YES" TO 終了フラグ
012310     END-READ.
012320*
012330*================================================================*
012340 作業ファイル３読込 SECTION.
012350*
012360     READ 作業ファイル３ NEXT
012370     AT END
012380         MOVE "YES" TO 終了フラグ３
012390     END-READ.
012400*================================================================*
012410 印字処理  SECTION.
012420*
012430     IF ( オープンフラグ NOT = "YES" )
012431        MOVE "YES" TO オープンフラグ
012433        OPEN I-O  印刷ファイル
012434        PERFORM エラー処理Ｐ
012436     END-IF.
012437*
012440     MOVE "YJK436P" TO  定義体名Ｐ.
012441     MOVE SPACE     TO  処理種別Ｐ.
012450     MOVE "SCREEN"  TO  項目群名Ｐ.
012460     WRITE YJK436P.
012470     PERFORM エラー処理Ｐ.
012480*================================================================*
012490 印字処理２  SECTION.
012500*
012510     IF ( オープンフラグ２ NOT = "YES" )
012511        MOVE "YES" TO オープンフラグ２
012513        OPEN I-O  印刷ファイル２
012515        PERFORM エラー処理Ｐ
012517     END-IF.
012518*
012519     MOVE "YJK437P" TO  定義体名Ｐ.
012520     MOVE SPACE     TO  処理種別Ｐ.
012530     MOVE "SCREEN"  TO  項目群名Ｐ.
012540     WRITE YJK437P.
012550     PERFORM エラー処理Ｐ.
012560*
012570*================================================================*
012580 改頁処理  SECTION.
012590*
012600     MOVE "YJK436P" TO  定義体名Ｐ.
012610     MOVE "CT"      TO  処理種別Ｐ.
012620     MOVE "PAGE"    TO  拡張制御Ｐ.
012630     MOVE SPACE     TO  項目群名Ｐ.
012640     WRITE YJK436P.
012650     PERFORM エラー処理Ｐ.
012660     MOVE SPACE     TO  拡張制御Ｐ.
012670*
012680     CLOSE  印刷ファイル.
012710     MOVE SPACE     TO オープンフラグ.
012711*================================================================*
012720 改頁処理２  SECTION.
012730
012740     MOVE "YJK437P" TO  定義体名Ｐ.
012750     MOVE "CT"      TO  処理種別Ｐ.
012760     MOVE "PAGE"    TO  拡張制御Ｐ.
012770     MOVE SPACE     TO  項目群名Ｐ.
012780     WRITE YJK437P.
012790     PERFORM エラー処理Ｐ.
012800     MOVE SPACE     TO  拡張制御Ｐ.
012810*
012820*================================================================*
012830 ヘッダセット SECTION.
012840*
012850* 当月の和暦を取得
012860     MOVE 請求和暦Ｗ         TO 元－元号区分.
012870     READ 元号マスタ
012880     INVALID KEY
012890         MOVE SPACE          TO ７号－請求和暦名称
012900     NOT INVALID KEY
012910         MOVE 元－元号名称   TO ７号－請求和暦名称
012920     END-READ.
012930*
012940     MOVE 請求年Ｗ           TO ７号－請求年.
012950     MOVE 請求月Ｗ           TO ７号－請求月.
012960     MOVE 代表者名Ｗ         TO ７号－代表者名.
012970     MOVE 接骨院名Ｗ         TO ７号－接骨院名.
012980     MOVE 柔整師番号Ｗ       TO ７号－柔整師番号.
012990*
013000* / 保険者/
013010     MOVE 作２－保険種別     TO 保険種別Ｗ.
013020     IF (作２－保険種別 = 05) AND (作２－保険者番号(1:2) = "39")
013030         MOVE 作２－保険者番号(1:4) TO ７号－保険者番号
013040     ELSE
013050         MOVE 作２－保険者番号      TO ７号－保険者番号
013060     END-IF.
013070     MOVE 作２－保険者番号２ TO 保険者番号Ｗ.
013080     PERFORM 県名セット.
013090     PERFORM 保険種別名セット.
013100     EVALUATE 保険種別Ｗ
013110     WHEN 01 THRU 04
013120     WHEN 06 THRU 09
013130*     WHEN 70 
013140*     WHEN 80
013150         PERFORM 保険者情報取得
013160     WHEN 05
013170     WHEN 50 THRU 60
013180         PERFORM 市町村情報取得
013190     END-EVALUATE.
013200     PERFORM 結合宛名取得.
013210*
013220     MOVE 保険者宛名Ｗ  TO ７号－保険者名称.
013230
013240*
013250*================================================================*
013260 明細セット SECTION.
013270*
013280     IF 作２－本人件数 NOT = ZERO
013290        MOVE 作２－本人件数     TO ７号－本人件数
013300        MOVE 作２－本人費用額   TO ７号－本人費用額
013310        MOVE 作２－本人負担額   TO ７号－本人負担額
013320        MOVE 作２－本人請求額   TO ７号－本人請求額
013330     END-IF.
013340     IF 作２－家族件数 NOT = ZERO
013350        MOVE 作２－家族件数     TO ７号－家族件数
013360        MOVE 作２－家族費用額   TO ７号－家族費用額
013370        MOVE 作２－家族負担額   TO ７号－家族負担額
013380        MOVE 作２－家族請求額   TO ７号－家族請求額
013390     END-IF.
013400*
013410*================================================================*
013420 エラー処理Ｐ SECTION.
013430*
013440     IF 通知情報Ｐ NOT = "00"
013450         DISPLAY NC"帳票エラー"              UPON CONS
013460         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
013470         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
013480         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
013490         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
013500                                             UPON CONS
013510         ACCEPT  キー入力 FROM CONS
013520         PERFORM ファイル閉鎖
013530         EXIT PROGRAM
013540     END-IF.
013550*================================================================*
013560 保険者情報取得 SECTION.
013570*
013580     MOVE  SPACE         TO 請求先名称Ｗ.
013590     MOVE  SPACE         TO 支部部署名Ｗ.
013600     MOVE  ZERO          TO 接尾語区分Ｗ.
013610*
013620     MOVE 保険種別Ｗ     TO 保－保険種別.
013630     MOVE 保険者番号Ｗ   TO 保－保険者番号.
013640     READ 保険者マスタ
013650     INVALID KEY
013660         MOVE SPACE      TO 請求先名称Ｗ
013670         MOVE SPACE      TO 支部部署名Ｗ
013680     NOT INVALID KEY
013690         IF 保－請求先情報区分 = 1
013700             MOVE 保－保険種別   TO 請先－保険種別
013710             MOVE 保－保険者番号 TO 請先－保険者番号
013720             READ 請求先マスタ
013730             INVALID KEY
013740                 MOVE SPACE             TO 請求先名称Ｗ
013750                 MOVE SPACE             TO 支部部署名Ｗ
013760             NOT INVALID KEY
013770                 MOVE 請先－保険者名称  TO 請求先名称Ｗ
013780                 MOVE 請先－支部部署名  TO 支部部署名Ｗ
013790             END-READ
013800         ELSE
013810             MOVE 保－保険者名称        TO 請求先名称Ｗ
013820             MOVE 保－支部部署名        TO 支部部署名Ｗ
013830             MOVE 保－接尾語区分        TO 接尾語区分Ｗ
013840         END-IF
013850     END-READ.
013860*================================================================*
013870 保険者情報取得２ SECTION.
013880*
013890     MOVE  SPACE         TO 請求先名称Ｗ２.
013900     MOVE  SPACE         TO 支部部署名Ｗ２.
013910     MOVE  ZERO          TO 接尾語区分Ｗ２.
013920*
013930     MOVE 保険種別Ｗ３     TO 保－保険種別.
013940     MOVE 保険者番号Ｗ３   TO 保－保険者番号.
013950     READ 保険者マスタ
013960     INVALID KEY
013970         MOVE SPACE      TO 請求先名称Ｗ２
013980         MOVE SPACE      TO 支部部署名Ｗ２
013990     NOT INVALID KEY
014000         IF 保－請求先情報区分 = 1
014010             MOVE 保－保険種別   TO 請先－保険種別
014020             MOVE 保－保険者番号 TO 請先－保険者番号
014030             READ 請求先マスタ
014040             INVALID KEY
014050                 MOVE SPACE             TO 請求先名称Ｗ２
014060                 MOVE SPACE             TO 支部部署名Ｗ２
014070             NOT INVALID KEY
014080                 MOVE 請先－保険者名称  TO 請求先名称Ｗ２
014090                 MOVE 請先－支部部署名  TO 支部部署名Ｗ２
014100             END-READ
014110         ELSE
014120             MOVE 保－保険者名称        TO 請求先名称Ｗ２
014130             MOVE 保－支部部署名        TO 支部部署名Ｗ２
014140*             MOVE 保－接尾語区分        TO 接尾語区分Ｗ２
014150             MOVE 保－接尾語区分        TO 接尾語区分Ｗ
014160         END-IF
014170     END-READ.
014180*================================================================*
014190 市町村情報取得 SECTION.
014200*
014210     MOVE  SPACE         TO 請求先名称Ｗ.
014220     MOVE  SPACE         TO 支部部署名Ｗ.
014230*
014240     MOVE 保険種別Ｗ               TO 市－公費種別.
014250     MOVE 保険者番号Ｗ             TO 市－市町村番号.
014260     READ 市町村マスタ
014270     INVALID KEY
014280         MOVE SPACE                TO 請求先名称Ｗ
014290         MOVE SPACE                TO 支部部署名Ｗ
014300     NOT INVALID KEY
014310         IF 市－請求先区分 = 1
014320             MOVE 保険種別Ｗ       TO 請先－保険種別
014330             MOVE 保険者番号Ｗ     TO 請先－保険者番号
014340             READ 請求先マスタ
014350             INVALID KEY
014360                 MOVE SPACE        TO 請求先名称Ｗ
014370                 MOVE SPACE        TO 支部部署名Ｗ
014380             NOT INVALID KEY
014390                 MOVE 請先－保険者名称   TO 請求先名称Ｗ
014400                 MOVE 請先－支部部署名   TO 支部部署名Ｗ
014410             END-READ
014420          ELSE
014430             MOVE 市－市町村名称   TO 請求先名称Ｗ
014440             MOVE 市－支部部署名   TO 支部部署名Ｗ
014450          END-IF
014460      END-READ.
014470*/後期高齢を代表番号でまとめる/080516
014480      IF 作２－保険者番号(1:2) = "39"
014490          MOVE SPACE TO 支部部署名Ｗ
014500      END-IF.
014510*================================================================*
014520 市町村情報取得２ SECTION.
014530*
014540     MOVE  SPACE         TO 請求先名称Ｗ２.
014550     MOVE  SPACE         TO 支部部署名Ｗ２.
014560*
014570     MOVE 保険種別Ｗ３               TO 市－公費種別.
014580     MOVE 保険者番号Ｗ３             TO 市－市町村番号.
014590     READ 市町村マスタ
014600     INVALID KEY
014610         MOVE SPACE                TO 請求先名称Ｗ２
014620         MOVE SPACE                TO 支部部署名Ｗ２
014630     NOT INVALID KEY
014640         IF 市－請求先区分 = 1
014650             MOVE 保険種別Ｗ３       TO 請先－保険種別
014660             MOVE 保険者番号Ｗ３     TO 請先－保険者番号
014670             READ 請求先マスタ
014680             INVALID KEY
014690                 MOVE SPACE        TO 請求先名称Ｗ２
014700                 MOVE SPACE        TO 支部部署名Ｗ２
014710             NOT INVALID KEY
014720                 MOVE 請先－保険者名称   TO 請求先名称Ｗ２
014730                 MOVE 請先－支部部署名   TO 支部部署名Ｗ２
014740             END-READ
014750          ELSE
014760             MOVE 市－市町村名称   TO 請求先名称Ｗ２
014770             MOVE 市－支部部署名   TO 支部部署名Ｗ２
014780          END-IF
014790      END-READ.
014800*================================================================*
014810 結合宛名取得 SECTION.
014820*
014830     MOVE SPACE TO 保険者宛名Ｗ.
014840     IF 請求先名称Ｗ NOT = SPACE
014850         EVALUATE 保険種別Ｗ
014860         WHEN 02
014870             IF 接尾語区分Ｗ = 1
014880                MOVE SPACE            TO 宛名Ｗ
014890             ELSE
014900                MOVE "社会保険事務所" TO 宛名Ｗ
014910             END-IF
014920         WHEN 06
014930             IF 接尾語区分Ｗ = 1
014940                MOVE "（日雇）"               TO 宛名Ｗ
014950             ELSE
014960                MOVE "社会保険事務所（日雇）" TO 宛名Ｗ
014970             END-IF
014980         WHEN 07
014990             MOVE "（船員）"       TO 宛名Ｗ
015000         WHEN 03
015010             MOVE "健康保険組合"   TO 宛名Ｗ
015020         WHEN 04
015030             MOVE "共済組合"       TO 宛名Ｗ
015040         WHEN 08
015050             MOVE "（退職）"       TO 宛名Ｗ
015060         WHEN OTHER
015070             MOVE SPACE            TO 宛名Ｗ
015080         END-EVALUATE
015090*
015100         IF 支部部署名Ｗ = SPACE
015110             STRING  請求先名称Ｗ  DELIMITED BY SPACE
015120                     宛名Ｗ        DELIMITED BY SPACE
015130                     "  殿"        DELIMITED BY SIZE
015140                    INTO 保険者宛名Ｗ
015150             END-STRING
015160         ELSE
015170             STRING  請求先名称Ｗ  DELIMITED BY SPACE
015180                     宛名Ｗ        DELIMITED BY SPACE
015190                     " "           DELIMITED BY SIZE
015200                     支部部署名Ｗ  DELIMITED BY SPACE
015210                     "  殿"        DELIMITED BY SIZE
015220                    INTO 保険者宛名Ｗ
015230             END-STRING
015240         END-IF
015250     END-IF.
015260*================================================================*
015270 結合宛名取得２ SECTION.
015280*
015290     MOVE SPACE TO 保険者宛名Ｗ２.
015300     IF 請求先名称Ｗ２ NOT = SPACE
015310         EVALUATE 保険種別Ｗ３
015320         WHEN 02
015330             IF 接尾語区分Ｗ = 1
015340                MOVE SPACE            TO 宛名Ｗ２
015350             ELSE
015360                MOVE "社会保険事務所" TO 宛名Ｗ２
015370             END-IF
015380         WHEN 06
015390             IF 接尾語区分Ｗ = 1
015400                MOVE "（日雇）"               TO 宛名Ｗ２
015410             ELSE
015420                MOVE "社会保険事務所（日雇）" TO 宛名Ｗ２
015430             END-IF
015440         WHEN 07
015450             MOVE "（船員）"       TO 宛名Ｗ２
015460         WHEN 03
015470             MOVE "健康保険組合"   TO 宛名Ｗ２
015480         WHEN 04
015490             MOVE "共済組合"       TO 宛名Ｗ２
015500         WHEN OTHER
015510             MOVE SPACE            TO 宛名Ｗ２
015520         END-EVALUATE
015530*
015540         IF 支部部署名Ｗ２ = SPACE
015550             STRING  請求先名称Ｗ２  DELIMITED BY SPACE
015560                     宛名Ｗ２        DELIMITED BY SPACE
015570                    INTO 保険者宛名Ｗ２
015580             END-STRING
015590         ELSE
015600             STRING  請求先名称Ｗ２  DELIMITED BY SPACE
015610                     宛名Ｗ２        DELIMITED BY SPACE
015620                     支部部署名Ｗ２  DELIMITED BY SPACE
015630                    INTO 保険者宛名Ｗ２
015640             END-STRING
015650         END-IF
015660     END-IF.
015670*
015680*================================================================*
015690 制御情報取得２ SECTION.
015700* 初期値設定
015710     MOVE ZERO  TO 国保７号印刷区分Ｗ. 
015720     MOVE ZERO  TO 社保７号印刷区分Ｗ. 
015730     MOVE ZERO  TO 組合７号印刷区分Ｗ. 
015740*/共済は印刷しないように変更/120117
015750     MOVE 1     TO 共済７号印刷区分Ｗ. 
015760     MOVE ZERO  TO 老人７号印刷区分Ｗ. 
015770     MOVE ZERO  TO 助成７号印刷区分Ｗ. 
015780
015790     MOVE ZERO  TO 国保６号印刷区分Ｗ. 
015800     MOVE ZERO  TO 社保６号印刷区分Ｗ. 
015810     MOVE 1     TO 組合６号印刷区分Ｗ. 
015820     MOVE 1     TO 共済６号印刷区分Ｗ. 
015830     MOVE ZERO  TO 老人６号印刷区分Ｗ. 
015840     MOVE 1     TO 助成６号印刷区分Ｗ. 
015850
015860*
015870*================================================================*
015880 印刷対象チェック  SECTION.
015890*
015900*  印刷区分による振り分け → ６・７号請求書（印刷区分 0:印刷 1:印刷しない）
015910* （一括印刷のみ）
015920* 7号用紙では、社保、国保、退職、老人、助成、組合、共済の印刷を行う
015930*
015940     MOVE SPACE TO 印刷フラグ.
015950*
015960     IF 連入－一括区分 NOT = 1
015970        MOVE "YES" TO 印刷フラグ
015980     ELSE
015990        EVALUATE 作２－保険種別
016000        WHEN 01
016010        WHEN 08
016020           IF 国保７号印刷区分Ｗ NOT = 1
016030              MOVE "YES" TO 印刷フラグ
016040           END-IF
016050        WHEN 02
016060        WHEN 06
016070        WHEN 07
016080           IF 社保７号印刷区分Ｗ NOT = 1
016090              MOVE "YES" TO 印刷フラグ
016100           END-IF
016110        WHEN 03
016120           IF 組合７号印刷区分Ｗ NOT = 1
016130              MOVE "YES" TO 印刷フラグ
016140           END-IF
016150        WHEN 04
016160        WHEN 09
016170           IF 共済７号印刷区分Ｗ NOT = 1
016180              MOVE "YES" TO 印刷フラグ
016190           END-IF
016200        WHEN 05
016210           IF 老人７号印刷区分Ｗ NOT = 1
016220              MOVE "YES" TO 印刷フラグ
016230           END-IF
016240* 生活保護は除く
016250        WHEN 51 THRU 60
016260           IF 助成７号印刷区分Ｗ NOT = 1
016270              MOVE "YES" TO 印刷フラグ
016280           END-IF
016290        WHEN OTHER
016300           MOVE "YES" TO 印刷フラグ
016310        END-EVALUATE
016320     END-IF.
016330*
016340*/東京都の後高は印刷しない(別用紙)/080516
016350     EVALUATE TRUE
016360     WHEN (作２－保険種別 = 05) AND (作２－保険者番号(3:2) = "13")
016370         MOVE SPACE TO 印刷フラグ
016380     END-EVALUATE.
016390*================================================================*
016400 印刷対象チェック２  SECTION.
016410*
016420*  印刷区分による振り分け → ６・７号請求書（印刷区分 0:印刷 1:印刷しない）
016430* （一括印刷のみ）
016440*
016450     MOVE SPACE TO 印刷フラグ２.
016460*
016470     IF 連入－一括区分 NOT = 1
016480        MOVE "YES" TO 印刷フラグ２
016490     ELSE
016500        EVALUATE 作３－保険種別
016510        WHEN 01
016520        WHEN 08
016530           IF 国保６号印刷区分Ｗ NOT = 1
016540              MOVE "YES" TO 印刷フラグ２
016550           END-IF
016560        WHEN 02
016570        WHEN 06
016580        WHEN 07
016590           IF 社保６号印刷区分Ｗ NOT = 1
016600              MOVE "YES" TO 印刷フラグ２
016610           END-IF
016620        WHEN 03
016630           IF 組合６号印刷区分Ｗ NOT = 1
016640              MOVE "YES" TO 印刷フラグ２
016650           END-IF
016660        WHEN 04
016670        WHEN 09
016680           IF 共済６号印刷区分Ｗ NOT = 1
016690              MOVE "YES" TO 印刷フラグ２
016700           END-IF
016710        WHEN 05
016720           IF 老人６号印刷区分Ｗ NOT = 1
016730              MOVE "YES" TO 印刷フラグ２
016740           END-IF
016750        WHEN 50 THRU 60
016760           IF 助成６号印刷区分Ｗ NOT = 1
016770              MOVE "YES" TO 印刷フラグ２
016780           END-IF
016790        WHEN OTHER
016800           MOVE "YES" TO 印刷フラグ２
016810        END-EVALUATE
016820     END-IF.
016830*
016840*/東京都の国保退職老人後高は印刷しない(別用紙)/080516
016850     EVALUATE TRUE
016860*/３では？/
016870*     WHEN (作２－保険種別 = 01)       AND (作２－保険者番号(1:2) = "13")
016880*     WHEN (作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = "13")
016890     WHEN (作３－保険種別 = 01)       AND (作３－保険者番号(1:2) = "13")
016900     WHEN (作３－保険種別 = 08 OR 05) AND (作３－保険者番号(3:2) = "13")
016910         MOVE SPACE TO 印刷フラグ２
016920     END-EVALUATE.
016930*================================================================*
016940 県名セット SECTION.
016950*
016960     EVALUATE  作２－県
016970*北海道
016980      WHEN  01
016990         MOVE NC"北海道" TO ７号－都道府県名
017000*青森
017010      WHEN  02
017020         MOVE NC"青森県" TO ７号－都道府県名
017030*岩手
017040      WHEN  03
017050         MOVE NC"岩手県" TO ７号－都道府県名
017060*宮城
017070      WHEN  04
017080         MOVE NC"宮城県" TO ７号－都道府県名
017090*秋田
017100      WHEN  05
017110         MOVE NC"秋田県" TO ７号－都道府県名
017120*山形
017130      WHEN  06
017140         MOVE NC"山形県" TO ７号－都道府県名
017150*福島
017160      WHEN  07
017170         MOVE NC"福島県" TO ７号－都道府県名
017180*茨城
017190      WHEN  08
017200         MOVE NC"茨城県" TO ７号－都道府県名
017210*栃木
017220      WHEN  09
017230         MOVE NC"栃木県" TO ７号－都道府県名
017240*群馬
017250      WHEN  10
017260         MOVE NC"群馬県" TO ７号－都道府県名
017270*埼玉
017280      WHEN  11
017290         MOVE NC"埼玉県" TO ７号－都道府県名
017300*千葉
017310      WHEN  12
017320         MOVE NC"千葉県" TO ７号－都道府県名
017330*東京
017340      WHEN  13
017350         MOVE NC"東京都" TO ７号－都道府県名
017360*神奈川
017370      WHEN  14
017380         MOVE NC"神奈川県" TO ７号－都道府県名
017390*新潟
017400      WHEN  15
017410         MOVE NC"新潟県" TO ７号－都道府県名
017420*富山
017430      WHEN  16
017440         MOVE NC"富山県" TO ７号－都道府県名
017450*石川
017460      WHEN  17
017470         MOVE NC"石川県" TO ７号－都道府県名
017480*福井
017490      WHEN  18
017500         MOVE NC"福井県" TO ７号－都道府県名
017510*山梨
017520      WHEN  19
017530         MOVE NC"山梨県" TO ７号－都道府県名
017540*長野
017550      WHEN  20
017560         MOVE NC"長野県" TO ７号－都道府県名
017570*岐阜
017580      WHEN  21
017590         MOVE NC"岐阜県" TO ７号－都道府県名
017600*静岡
017610      WHEN  22
017620         MOVE NC"静岡県" TO ７号－都道府県名
017630*愛知
017640      WHEN  23
017650         MOVE NC"愛知県" TO ７号－都道府県名
017660*三重
017670      WHEN  24
017680         MOVE NC"三重県" TO ７号－都道府県名
017690*滋賀
017700      WHEN  25
017710         MOVE NC"滋賀県" TO ７号－都道府県名
017720*京都
017730      WHEN  26
017740         MOVE NC"京都府" TO ７号－都道府県名
017750*大阪
017760      WHEN  27
017770         MOVE NC"大阪府" TO ７号－都道府県名
017780*兵庫
017790      WHEN  28
017800         MOVE NC"兵庫県" TO ７号－都道府県名
017810*奈良
017820      WHEN  29
017830         MOVE NC"奈良県" TO ７号－都道府県名
017840*和歌山
017850      WHEN  30
017860         MOVE NC"和歌山県" TO ７号－都道府県名
017870*鳥取
017880      WHEN  31
017890         MOVE NC"鳥取県" TO ７号－都道府県名
017900*島根
017910      WHEN  32
017920         MOVE NC"島根県" TO ７号－都道府県名
017930*岡山
017940      WHEN  33
017950         MOVE NC"岡山県" TO ７号－都道府県名
017960*広島
017970      WHEN  34
017980         MOVE NC"広島県" TO ７号－都道府県名
017990*山口
018000      WHEN  35
018010         MOVE NC"山口県" TO ７号－都道府県名
018020*徳島
018030      WHEN  36
018040         MOVE NC"徳島県" TO ７号－都道府県名
018050*香川
018060      WHEN  37
018070         MOVE NC"香川県" TO ７号－都道府県名
018080*愛媛
018090      WHEN  38
018100         MOVE NC"愛媛県" TO ７号－都道府県名
018110*高知
018120      WHEN  39
018130         MOVE NC"高知県" TO ７号－都道府県名
018140*福岡
018150      WHEN  40
018160         MOVE NC"福岡県" TO ７号－都道府県名
018170*佐賀
018180      WHEN  41
018190         MOVE NC"佐賀県" TO ７号－都道府県名
018200*長崎
018210      WHEN  42
018220         MOVE NC"長崎県" TO ７号－都道府県名
018230*熊本
018240      WHEN  43
018250         MOVE NC"熊本県" TO ７号－都道府県名
018260*大分
018270      WHEN  44
018280         MOVE NC"大分県" TO ７号－都道府県名
018290*宮崎
018300      WHEN  45
018310         MOVE NC"宮崎県" TO ７号－都道府県名
018320*鹿児島
018330      WHEN  46
018340         MOVE NC"鹿児島県" TO ７号－都道府県名
018350*沖縄
018360      WHEN  47
018370         MOVE NC"沖縄県" TO ７号－都道府県名
018380*その他
018390      WHEN  OTHER
018400         CONTINUE
018410     END-EVALUATE.
018420
018430*
018440*================================================================*
018450 県名セット２ SECTION.
018460*
018470     EVALUATE  作３－県
018480*北海道
018490      WHEN  01
018500         MOVE NC"北海道" TO 都道府県名
018510*青森
018520      WHEN  02
018530         MOVE NC"青森県" TO 都道府県名
018540*岩手
018550      WHEN  03
018560         MOVE NC"岩手県" TO 都道府県名
018570*宮城
018580      WHEN  04
018590         MOVE NC"宮城県" TO 都道府県名
018600*秋田
018610      WHEN  05
018620         MOVE NC"秋田県" TO 都道府県名
018630*山形
018640      WHEN  06
018650         MOVE NC"山形県" TO 都道府県名
018660*福島
018670      WHEN  07
018680         MOVE NC"福島県" TO 都道府県名
018690*茨城
018700      WHEN  08
018710         MOVE NC"茨城県" TO 都道府県名
018720*栃木
018730      WHEN  09
018740         MOVE NC"栃木県" TO 都道府県名
018750*群馬
018760      WHEN  10
018770         MOVE NC"群馬県" TO 都道府県名
018780*埼玉
018790      WHEN  11
018800         MOVE NC"埼玉県" TO 都道府県名
018810*千葉
018820      WHEN  12
018830         MOVE NC"千葉県" TO 都道府県名
018840*東京
018850      WHEN  13
018860         MOVE NC"東京都" TO 都道府県名
018870*神奈川
018880      WHEN  14
018890         MOVE NC"神奈川県" TO 都道府県名
018900*新潟
018910      WHEN  15
018920         MOVE NC"新潟県" TO 都道府県名
018930*富山
018940      WHEN  16
018950         MOVE NC"富山県" TO 都道府県名
018960*石川
018970      WHEN  17
018980         MOVE NC"石川県" TO 都道府県名
018990*福井
019000      WHEN  18
019010         MOVE NC"福井県" TO 都道府県名
019020*山梨
019030      WHEN  19
019040         MOVE NC"山梨県" TO 都道府県名
019050*長野
019060      WHEN  20
019070         MOVE NC"長野県" TO 都道府県名
019080*岐阜
019090      WHEN  21
019100         MOVE NC"岐阜県" TO 都道府県名
019110*静岡
019120      WHEN  22
019130         MOVE NC"静岡県" TO 都道府県名
019140*愛知
019150      WHEN  23
019160         MOVE NC"愛知県" TO 都道府県名
019170*三重
019180      WHEN  24
019190         MOVE NC"三重県" TO 都道府県名
019200*滋賀
019210      WHEN  25
019220         MOVE NC"滋賀県" TO 都道府県名
019230*京都
019240      WHEN  26
019250         MOVE NC"京都府" TO 都道府県名
019260*大阪
019270      WHEN  27
019280         MOVE NC"大阪府" TO 都道府県名
019290*兵庫
019300      WHEN  28
019310         MOVE NC"兵庫県" TO 都道府県名
019320*奈良
019330      WHEN  29
019340         MOVE NC"奈良県" TO 都道府県名
019350*和歌山
019360      WHEN  30
019370         MOVE NC"和歌山県" TO 都道府県名
019380*鳥取
019390      WHEN  31
019400         MOVE NC"鳥取県" TO 都道府県名
019410*島根
019420      WHEN  32
019430         MOVE NC"島根県" TO 都道府県名
019440*岡山
019450      WHEN  33
019460         MOVE NC"岡山県" TO 都道府県名
019470*広島
019480      WHEN  34
019490         MOVE NC"広島県" TO 都道府県名
019500*山口
019510      WHEN  35
019520         MOVE NC"山口県" TO 都道府県名
019530*徳島
019540      WHEN  36
019550         MOVE NC"徳島県" TO 都道府県名
019560*香川
019570      WHEN  37
019580         MOVE NC"香川県" TO 都道府県名
019590*愛媛
019600      WHEN  38
019610         MOVE NC"愛媛県" TO 都道府県名
019620*高知
019630      WHEN  39
019640         MOVE NC"高知県" TO 都道府県名
019650*福岡
019660      WHEN  40
019670         MOVE NC"福岡県" TO 都道府県名
019680*佐賀
019690      WHEN  41
019700         MOVE NC"佐賀県" TO 都道府県名
019710*長崎
019720      WHEN  42
019730         MOVE NC"長崎県" TO 都道府県名
019740*熊本
019750      WHEN  43
019760         MOVE NC"熊本県" TO 都道府県名
019770*大分
019780      WHEN  44
019790         MOVE NC"大分県" TO 都道府県名
019800*宮崎
019810      WHEN  45
019820         MOVE NC"宮崎県" TO 都道府県名
019830*鹿児島
019840      WHEN  46
019850         MOVE NC"鹿児島県" TO 都道府県名
019860*沖縄
019870      WHEN  47
019880         MOVE NC"沖縄県" TO 都道府県名
019890*その他
019900      WHEN  OTHER
019910         CONTINUE
019920     END-EVALUATE.
019930
019940*
019950*================================================================*
019960 保険種別名セット SECTION.
019970*
019980**///  印刷順序の変更はこのSECTIONで   ////**
019990*
020000*************************************************************************
020010*  印刷順序:保険名称(保険種別コード)                                    *
020020*                                                                       *
020030*    10:社保(2)                                                         *
020040*    11:船員(7)                                                         *
020050*    12:日雇(6)                                                         *
020060*    50:国保(1)                                                         *
020070*    51:国保組合(1)                                                     *
020080*    52:退職(8)                                                         *
020090*    53:老人(5)                                                         *
020100*    60:４１老人(51)                                                    *
020110*    61:母子(52)                                                        *
020120*    62:乳幼児(55)                                                      *
020130*    63:障害(53)                                                        *
020140*    64:被爆(54)                                                        *
020150*    65:その他(60)                                                      *
020160*    80:組合(3)                                                         *
020170*    90:共済(4)                                                         *
020180*    91:自衛官(9)                                                       *
020190*                                                                       *
020200*   労災、自賠責、自費はのぞく                                          *
020210*                                                                       *
020220*                                                                       *
020230*************************************************************************
020240*
020250*
020260      EVALUATE  作２－印刷順序
020270* 国保
020280      WHEN  050
020290          MOVE NC"国民保険" TO ７号－保険種別名
020300* 国保組合条件
020310      WHEN  051
020320          MOVE NC"国保組合" TO ７号－保険種別名
020330* 社保
020340      WHEN  010
020350*          MOVE NC"社会保険" TO ７号－保険種別名
020360          MOVE NC"協保" TO ７号－保険種別名
020370* 船員は社保で表示
020380*      WHEN  011
020390      WHEN  020
020400          MOVE NC"社会保険" TO ７号－保険種別名
020410* 日雇は社保で表示
020420      WHEN  012
020430*          MOVE NC"社会保険" TO ７号－保険種別名
020440          MOVE NC"協保" TO ７号－保険種別名
020450* 組合
020460      WHEN  080
020470          MOVE NC"健保組合" TO ７号－保険種別名
020480* 共済
020490      WHEN  090
020500          MOVE NC"共済組合" TO ７号－保険種別名
020510* 自衛官は共済組合で表示
020520      WHEN  091
020530          MOVE NC"共済組合" TO ７号－保険種別名
020540* ２７老人
020550      WHEN  053
020560          IF 作２－保険者番号(1:2) = "39"
020570              MOVE NC"後期"     TO ７号－保険種別名
020580          ELSE
020590              MOVE NC"老健老人" TO ７号－保険種別名
020600          END-IF
020610* 退職国保
020620      WHEN  052
020630          MOVE NC"退職国保" TO ７号－保険種別名
020640* ４１老人
020650      WHEN  060
020660          MOVE NC"公費老人" TO ７号－保険種別名
020670* 母子
020680      WHEN  061
020690          MOVE NC"母子家庭" TO ７号－保険種別名
020700* 乳幼児
020710      WHEN  062
020720          MOVE NC"乳幼児" TO ７号－保険種別名
020730* 障害
020740      WHEN  063
020750          MOVE NC"障害者" TO ７号－保険種別名
020760* 被爆
020770      WHEN  064
020780          MOVE NC"被爆者" TO ７号－保険種別名
020790* その他
020800* 東京の８８１３のみ「子」を印字し、それ以外は空白とする
020810      WHEN  065
020820          EVALUATE 作２－保険者番号(1:4)
020830          WHEN "8813"
020840              MOVE NC"子" TO ７号－保険種別名
020850          WHEN OTHER
020860              MOVE SPACE TO ７号－保険種別名
020870          END-EVALUATE
020880      WHEN OTHER
020890          MOVE SPACE TO ７号－保険種別名
020900     END-EVALUATE.
020910*
020920*================================================================*
020930 保険種別名セット２ SECTION.
020940*
020950**///  印刷順序の変更はこのSECTIONで   ////**
020960*
020970*************************************************************************
020980*  印刷順序:保険名称(保険種別コード)                                    *
020990*                                                                       *
021000*    10:社保(2)                                                         *
021010*    11:船員(7)                                                         *
021020*    12:日雇(6)                                                         *
021030*    50:国保(1)                                                         *
021040*    51:国保組合(1)                                                     *
021050*    52:退職(8)                                                         *
021060*    53:老人(5)                                                         *
021070*    60:４１老人(51)                                                    *
021080*    61:母子(52)                                                        *
021090*    62:乳幼児(55)                                                      *
021100*    63:障害(53)                                                        *
021110*    64:被爆(54)                                                        *
021120*    65:その他(60)                                                      *
021130*    80:組合(3)                                                         *
021140*    90:共済(4)                                                         *
021150*    91:自衛官(9)                                                       *
021160*                                                                       *
021170*   労災、自賠責、自費はのぞく                                          *
021180*                                                                       *
021190*                                                                       *
021200*************************************************************************
021210*
021220*
021230      EVALUATE  作３－印刷順序
021240* 国保
021250      WHEN  050
021260          MOVE NC"国民保険" TO 保険種別名
021270* 国保組合条件
021280      WHEN  051
021290          MOVE NC"国保組合" TO 保険種別名
021300* 社保
021310      WHEN  010
021320*          MOVE NC"社会保険" TO 保険種別名
021330          MOVE NC"協保"     TO 保険種別名
021340* 船員は社保で表示
021350*      WHEN  011
021360      WHEN  020
021370          MOVE NC"社会保険" TO 保険種別名
021380* 日雇は社保で表示
021390      WHEN  012
021400*          MOVE NC"社会保険" TO 保険種別名
021410          MOVE NC"協保"     TO 保険種別名
021420* 組合
021430      WHEN  080
021440          MOVE NC"健保組合" TO 保険種別名
021450* 共済
021460      WHEN  090
021470          MOVE NC"共済組合" TO 保険種別名
021480* 自衛官は共済組合で表示
021490      WHEN  091
021500          MOVE NC"共済組合" TO 保険種別名
021510* ２７老人
021520      WHEN  053
021530          IF 作３－保険者番号(1:2) = "39"
021540              MOVE NC"後期"     TO 保険種別名
021550          ELSE
021560              MOVE NC"老健老人" TO 保険種別名
021570          END-IF
021580* 退職国保
021590      WHEN  052
021600          MOVE NC"退職国保" TO 保険種別名
021610* ４１老人
021620      WHEN  060
021630          MOVE NC"公費老人" TO 保険種別名
021640* 母子
021650      WHEN  061
021660          MOVE NC"母子家庭" TO 保険種別名
021670* 乳幼児
021680      WHEN  062
021690          MOVE NC"乳幼児" TO 保険種別名
021700* 障害
021710      WHEN  063
021720          MOVE NC"障害者" TO 保険種別名
021730* 被爆
021740      WHEN  064
021750          MOVE NC"被爆者" TO 保険種別名
021760* その他
021770* 東京の８８１３のみ「子」を印字し、それ以外は空白とする
021780      WHEN  065
021790          EVALUATE 作３－保険者番号(1:4)
021800          WHEN "8813"
021810              MOVE NC"子" TO 保険種別名
021820          WHEN OTHER
021830              MOVE SPACE TO 保険種別名
021840          END-EVALUATE
021850      WHEN OTHER
021860          MOVE SPACE TO 保険種別名
021870     END-EVALUATE.
021880*
021890*================================================================*
021900 作業ファイル３作成  SECTION.
021910*
021920** ６号用紙印刷用作業ファイルを作成する
021930*
021940     OPEN INPUT 作業ファイル２.
021950         MOVE NC"作２" TO ファイル名.
021960         PERFORM オープンチェック.
021970
021980     OPEN OUTPUT 作業ファイル３.
021990         MOVE NC"作３" TO ファイル名.
022000         PERFORM オープンチェック.
022010
022020
022030* / 副キー(印刷順序+保険種別+保険者番号)で読み込む./
022040     MOVE ZERO      TO  作２－請求和暦.
022050     MOVE ZERO      TO  作２－請求年.
022060     MOVE ZERO      TO  作２－請求月.
022070*     MOVE LOW-VALUE TO  作２－県.
022080     MOVE ZERO      TO  作２－印刷順序.
022090     MOVE ZERO      TO  作２－保険種別.
022100     MOVE LOW-VALUE TO  作２－保険者番号.
022110*/県別、社保日雇いをまとめ、船員を別に集計↓↓/081021
022120*     MOVE ZERO      TO  作２－県２.
022130*     START 作業ファイル２   KEY IS >=  作２－請求和暦年月
022140*                                       作２－印刷順序
022150*                                       作２－保険種別
022160*                                       作２－保険者番号
022170**                                       作２－県
022180     START 作業ファイル２   KEY IS >=  作２－請求和暦年月
022190                                       作２－県２
022200                                       作２－印刷順序
022210                                       作２－保険種別
022220                                       作２－保険者番号
022230*/県別、社保日雇いをまとめ、船員を別に集計↑↑/081021
022240     END-START.
022250     IF 状態キー = "00"
022260         MOVE SPACE TO 終了フラグ
022270         PERFORM 作業ファイル２読込
022280         IF  終了フラグ = "YES"
022290             MOVE  NC"７　該当データなしです。確認して下さい。" TO 連メ－メッセージ
022300             CALL   "MSG001"
022310             CANCEL "MSG001"
022320             PERFORM ファイル閉鎖
022330             MOVE 99 TO PROGRAM-STATUS
022340             EXIT PROGRAM
022350         END-IF
022360
022370*
022380         PERFORM UNTIL ( 終了フラグ = "YES" )
022390*
022400                 MOVE SPACE TO 作３－レコード
022410                 INITIALIZE    作３－レコード
022420                 MOVE 作２－レコード  TO  作３－レコード
022430                 WRITE 作３－レコード
022440                 INVALID KEY
022450                    MOVE NC"作３"  TO ファイル名
022460                 PERFORM エラー表示
022470                 END-WRITE
022480*
022490                 PERFORM 作業ファイル２読込
022500         END-PERFORM
022510     END-IF.
022520*
022530     CLOSE 作業ファイル２ 作業ファイル３.
022540
022550*
022560*================================================================*
022570 表セット１ SECTION.
022580*
022590     MOVE 頁Ｗ                 TO 頁.
022600
022610* 当月の和暦を取得
022620     MOVE 請求和暦Ｗ         TO 元－元号区分.
022630     READ 元号マスタ
022640     INVALID KEY
022650         MOVE SPACE          TO 請求和暦名称
022660     NOT INVALID KEY
022670         MOVE 元－元号名称   TO 請求和暦名称
022680     END-READ.
022690*
022700     MOVE 請求年Ｗ           TO 請求年.
022710     MOVE 請求月Ｗ           TO 請求月.
022720*
022730     MOVE 代表者名Ｗ         TO 代表者名.
022740     MOVE 接骨院名Ｗ         TO 接骨院名.
022750     MOVE 柔整師番号Ｗ(3:7)  TO 柔整師番号１.
022760     MOVE 柔整師番号Ｗ(11:1) TO 柔整師番号２.
022770     MOVE 柔整師番号Ｗ(13:1) TO 柔整師番号３.
022780     PERFORM 県名セット２.
022790     PERFORM 保険種別名セット２.
022800*
022810*================================================================*
022820 表セット２ SECTION.
022830*
022840     MOVE 作３－本人件数     TO 本人件数(行カウンタ).
022850     MOVE 作３－本人費用額   TO 本人費用額(行カウンタ).
022860*
022870     MOVE 作３－家族件数     TO 家族件数(行カウンタ).
022880     MOVE 作３－家族費用額   TO 家族費用額(行カウンタ).
022890*
022900     MOVE 作３－件数         TO  計件数(行カウンタ).
022910     MOVE 作３－費用額       TO  計費用額(行カウンタ).
022920*
022930     ADD 作３－本人件数      TO  本人件数カウント.
022940     ADD 作３－家族件数      TO  家族件数カウント.
022950     ADD 作３－件数          TO  計件数カウント.
022960     ADD 作３－本人費用額    TO  本人費用額カウント.
022970     ADD 作３－家族費用額    TO  家族費用額カウント.
022980     ADD 作３－費用額        TO  計費用額カウント.
022990     ADD 作３－本人請求額    TO  本人請求額カウント.
023000     ADD 作３－家族請求額    TO  家族請求額カウント.
023010*     ADD 作２－請求額     TO  計請求額カウント.
023020**
023030* 保険者
023040     MOVE 作３－保険種別     TO 保険種別Ｗ３.
023050     MOVE 作３－保険者番号   TO 保険者番号Ｗ３.
023060     EVALUATE 保険種別Ｗ３
023070     WHEN 01 THRU 04
023080     WHEN 06 THRU 09
023090         PERFORM 保険者情報取得２
023100     WHEN 05
023110     WHEN 50 THRU 60
023120         IF (作３－保険種別 = 05) AND (作３－保険者番号(1:2) = "39")
023130             PERFORM 後期高齢情報取得
023140         ELSE
023150             PERFORM 市町村情報取得２
023160         END-IF
023170     END-EVALUATE.
023180     PERFORM 結合宛名取得２.
023190*
023200     MOVE 保険者宛名Ｗ２  TO 保険者名称Ｗ.
023210*
023220*
023230     COMPUTE 合計行カウンタ = 行カウンタ * 3.
023240     MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 1).
023250     IF 作３－保険種別 = "50" OR "51" OR "52" OR "53" OR
023260                         "54" OR "55" OR "60" OR "05" OR "08"
023270        IF 保険者名称２Ｗ NOT = SPACE AND 保険者名称３Ｗ = SPACE
023280           MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 2)
023290           MOVE 保険者名称２Ｗ TO 保険者名(合計行カウンタ - 1)
023300        END-IF
023310     ELSE
023320        IF 保険者名称２Ｗ NOT = SPACE AND 保険者名称３Ｗ = SPACE
023330           MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 1)
023340           MOVE 保険者名称２Ｗ TO 保険者名(合計行カウンタ)
023350        END-IF
023360     END-IF.
023370     IF 作３－保険種別 = "50" OR "51" OR "52" OR "53" OR
023380                         "54" OR "55" OR "60" OR "05" OR "08"
023390        IF 保険者名称２Ｗ NOT = SPACE AND 保険者名称３Ｗ NOT = SPACE
023400           MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 2)
023410           MOVE 保険者名称２Ｗ TO 保険者名(合計行カウンタ - 1)
023420        END-IF
023430     ELSE
023440        IF 保険者名称２Ｗ NOT = SPACE AND 保険者名称３Ｗ NOT = SPACE
023450           MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 2)
023460           MOVE 保険者名称２Ｗ TO 保険者名(合計行カウンタ - 1)
023470           MOVE 保険者名称３Ｗ TO 保険者名(合計行カウンタ)
023480        END-IF
023490     END-IF.
023500*
023510*================================================================*
023520 後期高齢情報取得 SECTION.
023530*
023540     MOVE SPACE          TO 請求先名称Ｗ２.
023550     MOVE SPACE          TO 支部部署名Ｗ２.
023560     MOVE 保険種別Ｗ３   TO 市－公費種別.
023570     MOVE 保険者番号Ｗ３ TO 市－市町村番号.
023580     START 市町村マスタ KEY IS >= 市－公費種別 市－市町村番号
023590     END-START.
023600     IF 状態キー = "00"
023610         READ 市町村マスタ NEXT
023620         AT END
023630             MOVE SPACE              TO 請求先名称Ｗ２
023640             MOVE SPACE              TO 支部部署名Ｗ２
023650         NOT AT END
023660             IF (保険種別Ｗ２        = 市－公費種別       ) AND
023670                (保険者番号Ｗ２(1:4) = 市－市町村番号(1:4))
023680                 MOVE 市－市町村名称 TO 請求先名称Ｗ２
023690                 MOVE SPACE          TO 支部部署名Ｗ２
023700             END-IF
023710         END-READ
023720     END-IF.
023730*================================================================*
023740******************************************************************
023750 END PROGRAM YJK4361.
023760******************************************************************
