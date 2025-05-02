000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCB6125.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*     中部柔整師協会 レセプト印刷（柔+ｳｨﾝﾄﾞｳｽﾞ版）
000100*         MED = YAW610 YCB6125P
000110*
      */平成２７年１０月施術分より会員番号を印刷/150922
      */金属副子・運動後療の変更・追加/1805
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2012-11-09
000140 DATE-COMPILED.          2012-11-09
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
000460     SELECT  経過マスタ      ASSIGN      TO        KEIKAL
000470                             ORGANIZATION             IS  INDEXED
000480                             ACCESS MODE              IS  DYNAMIC
000490                             RECORD KEY               IS  経－区分コード
000500                                                          経－経過コード
000510                             FILE STATUS              IS  状態キー
000520                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS MODE              IS  DYNAMIC
000560                             RECORD KEY               IS  制－制御区分
000570                             FILE STATUS              IS  状態キー
000580                             LOCK        MODE         IS  AUTOMATIC.
000590     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000600                             ORGANIZATION             IS  INDEXED
000610                             ACCESS MODE              IS  DYNAMIC
000620                             RECORD KEY               IS  施情－施術所番号
000630                             FILE STATUS              IS  状態キー
000640                             LOCK        MODE         IS  AUTOMATIC.
001410     SELECT  会情報マスタ    ASSIGN      TO        KAIJOHOL
001420                             ORGANIZATION             IS  INDEXED
001430                             ACCESS MODE              IS  DYNAMIC
000130                             RECORD KEY               IS  会情－柔整鍼灸区分
000131                                                          会情－協会コード
000132                                                          会情－保険種別
000133                                                          会情－変更和暦年月
000134                             ALTERNATE RECORD KEY     IS  会情－柔整鍼灸区分
000135                                                          会情－接骨師会カナ
000136                                                          会情－協会コード
000137                                                          会情－保険種別
000138                                                          会情－変更和暦年月
000151                             FILE STATUS              IS  状態キー
001520                             LOCK        MODE         IS  AUTOMATIC.
000770     SELECT  保険者マスタ    ASSIGN      TO        HOKENSL
000780                             ORGANIZATION             IS  INDEXED
000790                             ACCESS MODE              IS  DYNAMIC
000800                             RECORD KEY               IS  保－保険種別
000810                                                          保－保険者番号
000820* 将来は、キー項目の保険者名称を保険者カナにする
000830                             ALTERNATE RECORD KEY     IS  保－保険種別
000840                                                          保－保険者名称
000850                                                          保－保険者番号
000860                             FILE STATUS              IS  状態キー
000870                             LOCK        MODE         IS  AUTOMATIC.
000950     SELECT  ＩＤ管理マスタ    ASSIGN      TO      IDKANRL
000960                             ORGANIZATION             IS  INDEXED
000970                             ACCESS MODE              IS  DYNAMIC
000980                             RECORD KEY               IS  ＩＤ管－ＩＤ区分
000990                                                          ＩＤ管－施術所番号
001000                                                          ＩＤ管－保険種別
001010                                                          ＩＤ管－保険者番号
001020                             ALTERNATE RECORD KEY     IS  ＩＤ管－施術ＩＤ番号
001030                                                          ＩＤ管－ＩＤ区分
001040                                                          ＩＤ管－施術所番号
001050                                                          ＩＤ管－保険種別
001060                                                          ＩＤ管－保険者番号
001070                             FILE STATUS              IS  状態キー
001080                             LOCK        MODE         IS  AUTOMATIC.
001090     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
001100                             ORGANIZATION             IS  INDEXED
001110                             ACCESS MODE              IS  DYNAMIC
001120                             RECORD KEY               IS  市－公費種別
001130                                                          市－市町村番号
001140                             ALTERNATE RECORD KEY     IS  市－公費種別
001150                                                          市－市町村名称
001160                                                          市－市町村番号
001170                             FILE STATUS              IS  状態キー
001180                             LOCK        MODE         IS  AUTOMATIC.
001190     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
001200                             ORGANIZATION             IS  INDEXED
001210                             ACCESS MODE              IS  DYNAMIC
001220                             RECORD KEY               IS  受－施術和暦年月
001230                                                          受－患者コード
001240                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001250                                                          受－患者カナ
001260                                                          受－患者コード
001270                             ALTERNATE RECORD KEY     IS  受－患者コード
001280                                                          受－施術和暦年月
001290                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001300                                                          受－保険種別
001310                                                          受－保険者番号
001320                                                          受－患者コード
001330                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001340                                                          受－公費種別
001350                                                          受－費用負担者番号
001360                                                          受－患者コード
001370                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001380                                                          受－助成種別
001390                                                          受－費用負担者番号助成
001400                                                          受－患者コード
001410                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
001420                                                          受－施術和暦年月
001430                                                          受－患者コード
001440                             FILE STATUS              IS  状態キー
001450                             LOCK        MODE         IS  AUTOMATIC.
001460     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
001470                             ORGANIZATION             IS  INDEXED
001480                             ACCESS MODE              IS  DYNAMIC
001490                             RECORD KEY               IS  施記－施術和暦年月日
001500                                                          施記－患者コード
001510                             ALTERNATE RECORD KEY     IS  施記－患者コード
001520                                                          施記－施術和暦年月日
001530                             FILE STATUS              IS  状態キー
001540                             LOCK        MODE         IS  AUTOMATIC.
001550     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
001560                             ORGANIZATION             IS  INDEXED
001570                             ACCESS MODE              IS  DYNAMIC
001580                             RECORD KEY               IS  負－施術和暦年月
001590                                                          負－患者コード
001600                             ALTERNATE RECORD KEY     IS  負－患者コード
001610                                                          負－施術和暦年月
001620                             FILE STATUS              IS  状態キー
001630                             LOCK        MODE         IS  AUTOMATIC.
001640     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
001650                             ORGANIZATION             IS  INDEXED
001660                             ACCESS MODE              IS  DYNAMIC
001670                             RECORD KEY               IS  負原－区分コード
001680                                                          負原－負傷原因コード
001690                             FILE STATUS              IS  状態キー
001700                             LOCK        MODE         IS  AUTOMATIC.
001860* 並び順印字用
001870     SELECT  作業ファイル２  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001880                             ORGANIZATION             IS  INDEXED
001890                             ACCESS                   IS  DYNAMIC
001900                             RECORD      KEY          IS  作２－施術和暦年月
001910                                                          作２－患者コード
001920                                                          作２－保険種別
001930                             FILE        STATUS       IS  状態キー
001940                             LOCK        MODE         IS  AUTOMATIC.
001850     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF002
001860                             SYMBOLIC    DESTINATION  IS "PRT"
001870                             FORMAT                   IS  定義体名Ｐ
001880                             GROUP                    IS  項目群名Ｐ
001890                             PROCESSING  MODE         IS  処理種別Ｐ
001900                             UNIT        CONTROL      IS  拡張制御Ｐ
001910                             FILE        STATUS       IS  通知情報Ｐ.
001920******************************************************************
001930*                      DATA DIVISION                             *
001940******************************************************************
001950 DATA                    DIVISION.
001960 FILE                    SECTION.
001970*                           ［ＲＬ＝  １２８］
001980 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001990     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
002000*                           ［ＲＬ＝  １２８］
002010 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
002020     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
002060*                           ［ＲＬ＝  １２８］
002070 FD  経過マスタ          BLOCK   CONTAINS   1   RECORDS.
002080     COPY KEIKA           OF  XFDLIB  JOINING   経   AS  PREFIX.
002090*                           ［ＲＬ＝  ２５６］
002100 FD  制御情報マスタ      BLOCK   CONTAINS   1   RECORDS.
002110     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
002120*                           ［ＲＬ＝  １２８］
002130 FD  施術所情報マスタ    BLOCK   CONTAINS   1   RECORDS.
002140     COPY SEJOHO          OF  XFDLIB  JOINING   施情 AS  PREFIX.
002150*                           ［ＲＬ＝  ６４０］
002160 FD  会情報マスタ        BLOCK   CONTAINS   1   RECORDS.
002170     COPY KAIJOHO         OF  XFDLIB  JOINING   会情 AS  PREFIX.
002180*                           ［ＲＬ＝  ３２０］
002190 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
002200     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
002240*                           ［ＲＬ＝  １２８］
002250 FD  ＩＤ管理マスタ      BLOCK   CONTAINS   1   RECORDS.
002260     COPY IDKANR          OF  XFDLIB  JOINING   ＩＤ管 AS  PREFIX.
002270*                           ［ＲＬ＝  ２５６］
002280 FD  市町村マスタ        BLOCK   CONTAINS   1   RECORDS.
002290     COPY SITYOSN         OF  XFDLIB  JOINING   市   AS  PREFIX.
002300*                           ［ＲＬ＝  ３２０］
002310 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
002320     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
002330*                           ［ＲＬ＝  ２５６］
002340 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
002350     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
002360*                           ［ＲＬ＝  １２８］
002370 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002380     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
002390*                           ［ＲＬ＝  １２８］
002400 FD  負傷原因Ｆ         BLOCK   CONTAINS    1   RECORDS.
002410     COPY HUGEIN          OF  XFDLIB  JOINING   負原 AS  PREFIX.
002570*
002580 FD  作業ファイル２ RECORD  CONTAINS 32 CHARACTERS.
002590 01  作２－レコード.
002600     03  作２－レコードキー.
002610         05  作２－施術和暦年月.
002620             07  作２－施術和暦            PIC 9.
002630             07  作２－施術年              PIC 9(2).
002640             07  作２－施術月              PIC 9(2).
002650         05  作２－患者コード.
002660             07 作２－患者番号             PIC 9(6).
002670             07 作２－枝番                 PIC X(1).
002680         05  作２－保険種別                PIC 9(2).
002690     03  作２－レコードデータ.
002700         05  作２－順番                    PIC 9(4).
002710         05  FILLER                        PIC X(14).
002450*
002460 FD  印刷ファイル.
002470     COPY YCB6125P         OF  XMDLIB.
002480*----------------------------------------------------------------*
002490******************************************************************
002500*                WORKING-STORAGE SECTION                         *
002510******************************************************************
002520 WORKING-STORAGE         SECTION.
002530 01 キー入力                           PIC X     VALUE SPACE.
002540 01 状態キー                           PIC X(2)  VALUE SPACE.
002550 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002560 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002570 01 ファイル名                         PIC N(6)  VALUE SPACE.
002580 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002590*
002600*--- 制御マスタ退避 ---*
002610 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
002620*
002630** 負傷原因・長期理由印刷区分用
002640 01 負傷原因印刷区分Ｗ                 PIC 9     VALUE ZERO.
002650 01 長期理由印刷区分Ｗ                 PIC 9     VALUE ZERO.
002660*
002670** レセ下段の日付区分用 (0:最終通院日、1:月末日、9:印字なし)
002680 01 レセプト日付区分Ｗ                 PIC 9     VALUE ZERO.
002690 01 レセプト患者日付区分Ｗ             PIC 9     VALUE ZERO.
002700*
002710*--- カウンタ ---*
002720 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002730*
002740*--- 保険者番号比較用 ---*
002750 01 保険者番号比較Ｗ                   PIC X(6)  VALUE SPACE.
002760*
002770*--- 負傷データ取得用 ---*
002780 01 負傷名称Ｗ                         PIC N(10) VALUE SPACE.
002790 01 部位名称Ｗ                         PIC N(20) VALUE SPACE.
002800 01 部位長Ｗ                           PIC 9(2)  VALUE 1.
002810 01 経過部位Ｗ                         PIC N(1)  VALUE SPACE.
002820*
002830** 枝番判定用
002840 01 開始診療日手動区分Ｗ               PIC 9     VALUE ZERO.
002850*
002860* 負傷原因印刷区分
002870 01 レセ負傷原因印刷区分Ｗ             PIC 9     VALUE ZERO.
002580 01 レセ長期理由印刷区分Ｗ             PIC 9    VALUE ZERO.
002880*
002890*--- 施術記録取得用 ---*
002900 01 初日再検フラグ                     PIC X(3)  VALUE SPACE.
002910 01 前月フラグ                         PIC X(3)  VALUE SPACE.
002920*
002930 01 終了年月日ＷＴ.
002940    03 終了年ＷＴ                      PIC 9(2)  VALUE ZERO.
002950    03 終了月ＷＴ                      PIC 9(2)  VALUE ZERO.
002960    03 終了日ＷＴ                      PIC 9(2)  VALUE ZERO.
002970*
002980** 前月判定用
002990 01 計算年月日Ｗ.
003000    03 計算和暦Ｗ                      PIC 9(1)  VALUE ZERO.
003010    03 計算年Ｗ                        PIC S9(2) VALUE ZERO.
003020    03 計算月Ｗ                        PIC S9(2) VALUE ZERO.
003030    03 計算日Ｗ                        PIC S9(2) VALUE ZERO.
003040 01 開始年月日２Ｗ.
003050    03 開始和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003060    03 開始年２Ｗ                      PIC 9(2)  VALUE ZERO.
003070    03 開始月２Ｗ                      PIC 9(2)  VALUE ZERO.
003080    03 開始日２Ｗ                      PIC 9(2)  VALUE ZERO.
003090    03 開始西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003100 01 終了年月日２Ｗ.
003110    03 終了和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003120    03 終了年２Ｗ                      PIC 9(2)  VALUE ZERO.
003130    03 終了月２Ｗ                      PIC 9(2)  VALUE ZERO.
003140    03 終了日２Ｗ                      PIC 9(2)  VALUE ZERO.
003150    03 終了西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003160*
003170*--- 初検日退避用 ---*
003180 01 初検フラグ                         PIC X(3)  VALUE SPACE.
003190*
003200 01 初検年月日ＷＴ.
003210    03 初検和暦ＷＴ                    PIC 9     VALUE ZERO.
003220    03 初検年ＷＴ                      PIC 9(2)  VALUE ZERO.
003230    03 初検月ＷＴ                      PIC 9(2)  VALUE ZERO.
003240    03 初検日ＷＴ                      PIC 9(2)  VALUE ZERO.
003250*
003260*--- 初検加算時刻用 ---*
003270 01 初検加算ＷＴ.
003280    03 初検加算カウント                PIC 9     VALUE ZERO.
003290    03 番号カウンタ                    PIC 9     VALUE ZERO.
003300    03 初検加算集団ＷＴ  OCCURS 3.
003310       05 初検加算区分ＷＴ             PIC 9     VALUE ZERO.
003320       05 初検加算時ＷＴ               PIC 9(2)  VALUE ZERO.
003330       05 初検加算分ＷＴ               PIC 9(2)  VALUE ZERO.
003340    03 初検加算集団ＮＷ  OCCURS 3.
003350       05 加算区切Ｗ                   PIC N(1)  VALUE SPACE.
003360       05 加算内容Ｗ                   PIC N(3)  VALUE SPACE.
003370       05 初検加算時ＮＷ１             PIC N(1)  VALUE SPACE.
003380       05 初検加算時ＮＷ２             PIC N(1)  VALUE SPACE.
003390       05 時固定Ｗ                     PIC N(1)  VALUE SPACE.
003400       05 初検加算分ＮＷ１             PIC N(1)  VALUE SPACE.
003410       05 初検加算分ＮＷ２             PIC N(1)  VALUE SPACE.
003420       05 分固定Ｗ                     PIC N(1)  VALUE SPACE.
003430    03 初検加算時刻１Ｗ                PIC N(10) VALUE SPACE.
003440    03 初検加算時刻２Ｗ                PIC N(10) VALUE SPACE.
003450    03 初検加算時刻３Ｗ                PIC N(10) VALUE SPACE.
003070    03 初検加算区切Ｗ                  PIC X     VALUE SPACE.
003080    03 初検加算時Ｗ                    PIC 9(2)  VALUE ZERO.
003090    03 初検加算分Ｗ                    PIC 9(2)  VALUE ZERO.
003460*
003470** 数字→日本語変換
003480 01 数字Ｗ                             PIC 9(2).
003490 01 数字Ｒ REDEFINES 数字Ｗ.
003500    03 数字Ｗ１                        PIC X(1).
003510    03 数字Ｗ２                        PIC X(1).
003520*
003530 01 負傷番号Ｗ                         PIC 9.
003540 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
003550    03 負傷番号Ｗ１                    PIC X.
003560*
003570 01 全角負傷番号Ｗ                     PIC N.
003580 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
003590    03 全角負傷番号Ｗ１                PIC X(2).
003600*
003610*--- 負傷原因用 ---*
003620 01 カウンタ                           PIC 9(2)  VALUE ZERO.
003630 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
003640 01 負傷原因ＷＴ.
003650    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
003660    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
003670    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
003680    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
003690    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
003700    03 負傷原因ナンバーＷＴ.
003710       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
003720    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
003730 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
003740 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
003750 01 負傷原因ＴＢＬ.
003760    03 負傷原因コードＴＢＬ            OCCURS 9.
003770       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
003780       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
003790       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
003800 01 負傷原因内容Ｗ.
003810    03 負傷原因内容合成Ｗ              PIC X(318) OCCURS 9 VALUE SPACE.
003620    03 負傷原因内容分解ＸＷ.
003630       05 負傷原因内容１ＸＷ           PIC X(80)  VALUE SPACE.
003640       05 負傷原因内容２ＸＷ           PIC X(80)  VALUE SPACE.
003640       05 負傷原因内容３ＸＷ           PIC X(80)  VALUE SPACE.
003650       05 負傷原因内容４ＸＷ           PIC X(78)  VALUE SPACE.
003800 01 負傷原因ＷＰ                       PIC N(225) VALUE SPACE.
       01 負傷原因ＷＲＰ.
003810    03 負傷原因ＷＲ                    PIC N(45) OCCURS 5 VALUE SPACE.
003860*
003870*--- 委任年月日用 ---*
003880 01 受理年月日Ｗ.
003890    03 受理年Ｗ                        PIC 9(2)  VALUE ZERO.
003900    03 受理月Ｗ                        PIC 9(2)  VALUE ZERO.
003910    03 受理日Ｗ                        PIC 9(2)  VALUE ZERO.
003920 01 最終通院年月日Ｗ.
003930    03 最終通院年Ｗ                    PIC 9(2)  VALUE ZERO.
003940    03 最終通院月Ｗ                    PIC 9(2)  VALUE ZERO.
003950    03 最終通院日Ｗ                    PIC 9(2)  VALUE ZERO.
003960** 月末日用
003970 01 施術西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
003980 01 商Ｗ                               PIC 9(3)  VALUE ZERO.
003990 01 余Ｗ                               PIC 9(3)  VALUE ZERO.
004000*
004010*--- 会長委任用 ---*
004020 01 会長委任フラグ                     PIC X(3)  VALUE SPACE.
004030 01 月末日Ｗ                           PIC 9(2)  VALUE ZERO.
004040*
004050 01 日付編集Ｗ.
004060   03 元号編集Ｗ                       PIC N(2)  VALUE SPACE.
004070   03 年編集Ｗ                         PIC ZZ    VALUE ZERO.
004080   03 FILLER                           PIC X(2)  VALUE "年".
004090   03 月編集Ｗ                         PIC ZZ    VALUE ZERO.
004100   03 FILLER                           PIC X(2)  VALUE "月".
004110   03 日編集Ｗ                         PIC ZZ    VALUE ZERO.
004120   03 FILLER                           PIC X(2)  VALUE "日".
004130*
004140*--- 負担給付割合用 ---*
004150 01 負担割合Ｗ                         PIC 9(2)  VALUE ZERO.
004160 01 給付割合Ｗ                         PIC 9(2)  VALUE ZERO.
004170*
004180*--- レセプト回数用 ---*
004190 01 回数Ｗ                             PIC 9(2)  VALUE ZERO.
004200*
004210 01 最小開始和暦年月Ｗ.
004220    03 最小開始和暦Ｗ                  PIC 9(1)  VALUE ZERO.
004230    03 最小開始年Ｗ                    PIC 9(2)  VALUE ZERO.
004240    03 最小開始月Ｗ                    PIC 9(2)  VALUE ZERO.
004250*
004260*--- 施術ＩＤ用 ---*
004270 01 施術ＩＤ固定Ｗ                     PIC X(14) VALUE "施術機関番号：".
004280*
004290*--- 助成レセまとめ用 ---*
004300 01 助成レセまとめフラグ               PIC X(3)  VALUE SPACE.
004310 01 助成種別略称Ｗ                     PIC N(4)  VALUE SPACE.
004320 01 助成種別略称Ｗ２                   PIC N(4)  VALUE SPACE.
003920*
003930*--- 共済・自衛官用 ---*
003940* 共済番号用
003950 01 脱出フラグ                         PIC X(3)  VALUE SPACE.
003960 01 共済連番号集団Ｗ.
003970    03 共済連番号名Ｗ                  PIC X(14) VALUE SPACE.
003980    03 共済連番号名ＮＷ REDEFINES  共済連番号名Ｗ  PIC N(7).
          03 共済連番号ＷＰ.
003990       05 共済連番号Ｗ                 PIC X(6)  VALUE SPACE.
004000       05 共済連番号単位Ｗ             PIC X(2)  VALUE SPACE.
004010       05 共済連番号単位ＮＷ REDEFINES  共済連番号単位Ｗ  PIC N.
004020* 自衛官番号用
004030 01 自衛官番号集団Ｗ.
004040    03 自衛官番号名Ｗ                  PIC X(8)  VALUE SPACE.
004050    03 自衛官番号名ＮＷ REDEFINES  自衛官番号名Ｗ  PIC N(4).
          03 自衛官番号ＷＰ.
004060       05 自衛官番号Ｗ                 PIC X(6)  VALUE SPACE.
004070       05 自衛官番号単位Ｗ             PIC X(2)  VALUE SPACE.
004080       05 自衛官番号単位ＮＷ REDEFINES  自衛官番号単位Ｗ  PIC N.
      *
      */金属副子・運動後療の変更・追加/1805
       01 金属副子ＣＭ                       PIC X(140) VALUE SPACE.
       01 運動後療ＣＭ                       PIC X(68)  VALUE SPACE.
004330*
004340**--- ５部位  摘要欄印字  編集用 ---*
004350* 01 部位５Ｗ.
004360*   03 FILLER                           PIC X(1).
004370*   03 逓減固定５Ｗ                     PIC X(6).
004380**   03 FILLER                           PIC X(2).
004390*   03 逓減開始月日５Ｗ.
004400*      05 逓減開始月５Ｗ                PIC ZZ.
004410*      05 FILLER                        PIC X(2).
004420*      05 逓減開始日５Ｗ                PIC ZZ.
004430*   03 FILLER                           PIC X(2).
004440*   03 後療５Ｗ.
004450*      05 後療単価５Ｗ                  PIC ZZZZ.
004460*      05 FILLER                        PIC X(2).
004470*      05 後療回数５Ｗ                  PIC ZZ.
004480*      05 FILLER                        PIC X(2).
004490*      05 後療料５Ｗ                    PIC ZZ,ZZZ.
004500*   03 FILLER                           PIC X(3).
004510*   03 冷罨法５Ｗ.
004520*      05 冷罨法回数５Ｗ                PIC ZZ.
004530*      05 FILLER                        PIC X(2).
004540*      05 冷罨法料５Ｗ                  PIC ZZZZ.
004550*   03 FILLER                           PIC X(3).
004560*   03 温罨法５Ｗ.
004570*      05 温罨法回数５Ｗ                PIC ZZ.
004580*      05 FILLER                        PIC X(2).
004590*      05 温罨法料５Ｗ                  PIC ZZZZ.
004600*   03 FILLER                           PIC X(3).
004610*   03 電療５Ｗ.
004620*      05 電療回数５Ｗ                  PIC ZZ.
004630*      05 FILLER                        PIC X(2).
004640*      05 電療料５Ｗ                    PIC ZZZZ.
004650*   03 FILLER                           PIC X(2).
004660*   03 小計５Ｗ                         PIC ZZ,ZZZ.
004670*   03 FILLER                           PIC X(1).
004680*   03 多部位率５Ｗ                     PIC X(4).
004690*   03 FILLER                           PIC X(3).
004700*   03 多部位込小計５Ｗ                 PIC ZZ,ZZZ.
004710*   03 FILLER                           PIC X(3).
004720*   03 長期逓減率５Ｗ                   PIC 9.9.
004730*   03 FILLER                           PIC X(3).
004740*   03 長期込小計５Ｗ                   PIC ZZ,ZZZ.
004750*   03 FILLER                           PIC X(4).
004760*
004770******************************
004780* ５部位  摘要欄印字  編集用 *
004790******************************
004800 01 部位５Ｗ.
004840   03 逓減開始月日５Ｗ.
004850      05 逓減開始月５Ｗ                PIC ZZ.
            05 月ＣＭ                        PIC X(2).
004870      05 逓減開始日５Ｗ                PIC ZZ.
            05 日ＣＭ                        PIC X(2).
         03 括弧１Ｗ                         PIC X(1).
004890   03 後療５Ｗ.
            05 括弧２Ｗ                      PIC X(1).
004900      05 後療単価５Ｗ                  PIC ZZZZ.
            05 乗算記号１Ｗ                  PIC X(1).
004920      05 後療回数５Ｗ                  PIC ZZ.
            05 イコール１Ｗ                  PIC X(1).
004940      05 後療料５Ｗ                    PIC ZZ,ZZZ.
         03 括弧３Ｗ                         PIC X(1).
         03 加算記号１Ｗ                     PIC X(1).
         03 括弧４Ｗ                         PIC X(1).
004960   03 冷罨法５Ｗ.
            05 冷罨法単価５Ｗ                PIC Z(2).
            05 乗算記号２Ｗ                  PIC X(1).
004970      05 冷罨法回数５Ｗ                PIC ZZ.
            05 イコール２Ｗ                  PIC X(1).
004990      05 冷罨法料５Ｗ                  PIC ZZZZ.
         03 括弧５Ｗ                         PIC X(1).
         03 加算記号２Ｗ                     PIC X(1).
         03 括弧６Ｗ                         PIC X(1).
005010   03 温罨法５Ｗ.
            05 温罨法単価５Ｗ                PIC Z(2).
            05 乗算記号３Ｗ                  PIC X(1).
005020      05 温罨法回数５Ｗ                PIC ZZ.
            05 イコール３Ｗ                  PIC X(1).
005040      05 温罨法料５Ｗ                  PIC ZZZZ.
         03 括弧７Ｗ                         PIC X(1).
         03 加算記号３Ｗ                     PIC X(1).
         03 括弧８Ｗ                         PIC X(1).
005060   03 電療５Ｗ.
            05 電療単価５Ｗ                  PIC Z(2).
            05 乗算記号４Ｗ                  PIC X(1).
005070      05 電療回数５Ｗ                  PIC ZZ.
            05 イコール４Ｗ                  PIC X(1).
005090      05 電療料５Ｗ                    PIC ZZZZ.
            05 括弧９Ｗ                      PIC X(1).
         03 括弧１０Ｗ                       PIC X(1).
         03 乗算記号５Ｗ                     PIC X(1).
005130   03 多部位率５Ｗ                     PIC X(3).
         03 乗算記号６Ｗ                     PIC X(1).
005170   03 長期逓減率５Ｗ                   PIC 9.9.
         03 イコール５Ｗ                     PIC X(1).
005190   03 長期込小計５Ｗ                   PIC ZZ,ZZZ.
005210*
004770****************
004780* 連結項目待避 *
004790****************
004800*    ************
004810*    * 印刷キー *
004820*    ************
004830 01 対象データＷＲ.
004840    03 施術和暦年月ＷＲ.
004850       05 施術和暦ＷＲ                 PIC 9(1)  VALUE ZERO.
004860       05 施術年ＷＲ                   PIC 9(2)  VALUE ZERO.
004870       05 施術月ＷＲ                   PIC 9(2)  VALUE ZERO.
004880    03 保険種別ＷＲ                    PIC 9(2)  VALUE ZERO.
004890    03 保険者番号ＷＲ                  PIC X(10) VALUE SPACE.
004900    03 公費種別ＷＲ                    PIC 9(2)  VALUE ZERO.
004910    03 費用負担者番号ＷＲ              PIC X(10) VALUE SPACE.
004920    03 助成種別ＷＲ                    PIC 9(2)  VALUE ZERO.
004930    03 費用負担者番号助成ＷＲ          PIC X(10) VALUE SPACE.
004940    03 本人家族区分ＷＲ                PIC 9(1)  VALUE ZERO.
004950    03 患者カナＷＲ                    PIC X(50) VALUE SPACE.
004960    03 患者コードＷＲ.
004970       05 患者番号ＷＲ                 PIC 9(6)  VALUE ZERO.
004980       05 枝番ＷＲ                     PIC X(1)  VALUE SPACE.
004990*    ************
005000*    * 料金情報 *
005010*    ************
005020*    月毎の料金
005030***********************
005040 01 料金１ＷＲ.
005050   03 初検ＷＲ.
005060      05 負担割合ＷＲ                  PIC 9(3)  VALUE ZERO.
005070      05 初検料ＷＲ                    PIC 9(5)  VALUE ZERO.
005080      05 初検加算料ＷＲ                PIC 9(5)  VALUE ZERO.
005090   03 相談料ＷＲ                       PIC 9(4)  VALUE ZERO.
005100   03 再検料ＷＲ                       PIC 9(5)  VALUE ZERO.
005110   03 往療ＷＲ.
005120      05 往療距離ＷＲ                  PIC 9(2)V9 VALUE ZERO.
005130      05 往療回数ＷＲ                  PIC 9(2)  VALUE ZERO.
005140      05 往療料ＷＲ                    PIC 9(5)  VALUE ZERO.
005150      05 往療加算料ＷＲ                PIC 9(5)  VALUE ZERO.
005160   03 金属副子加算料ＷＲ               PIC 9(5)  VALUE ZERO.
005170   03 施術情報提供料ＷＲ               PIC 9(5)  VALUE ZERO.
005180   03 合計ＷＲ                         PIC 9(6)  VALUE ZERO.
005190   03 一部負担金ＷＲ                   PIC 9(6)  VALUE ZERO.
005200   03 請求金額ＷＲ                     PIC 9(6)  VALUE ZERO.
005210   03 給付割合ＷＲ                     PIC 9(1)  VALUE ZERO.
005220   03 受給者負担額ＷＲ                 PIC 9(6)  VALUE ZERO.
005230   03 助成請求金額ＷＲ                 PIC 9(6)  VALUE ZERO.
005240*
005250* 負傷部位毎の料金
005260***********************
005270 01 料金２ＷＲ.
005280   03 初回処置ＷＲ    OCCURS   9.
005290      05 初回処置料ＷＲ                PIC 9(5)  VALUE ZERO.
005300*
005310* 逓減毎の料金
005320***********************
005330 01 料金３ＷＲ.
005340**********
005350* １部位 *
005360**********
005370   03 部位１ＷＲ.
005380      05 後療１ＷＲ.
005390         07 後療単価１ＷＲ             PIC 9(4)  VALUE ZERO.
005400         07 後療回数１ＷＲ             PIC 9(2)  VALUE ZERO.
005410         07 後療料１ＷＲ               PIC 9(5)  VALUE ZERO.
005420      05 冷罨法１ＷＲ.
005430         07 冷罨法回数１ＷＲ           PIC 9(2)  VALUE ZERO.
005440         07 冷罨法料１ＷＲ             PIC 9(4)  VALUE ZERO.
005450      05 温罨法１ＷＲ.
005460         07 温罨法回数１ＷＲ           PIC 9(2)  VALUE ZERO.
005470         07 温罨法料１ＷＲ             PIC 9(4)  VALUE ZERO.
005480      05 電療１ＷＲ.
005490         07 電療回数１ＷＲ             PIC 9(2)  VALUE ZERO.
005500         07 電療料１ＷＲ               PIC 9(4)  VALUE ZERO.
005510      05 小計１ＷＲ                    PIC 9(6)  VALUE ZERO.
005520      05 長期逓減率１ＷＲ              PIC 9(3)  VALUE ZERO.
005530      05 長期込小計１ＷＲ              PIC 9(6)  VALUE ZERO.
005540**********
005550* ２部位 *
005560**********
005570   03 部位２ＷＲ.
005580      05 後療２ＷＲ.
005590         07 後療単価２ＷＲ             PIC 9(4)  VALUE ZERO.
005600         07 後療回数２ＷＲ             PIC 9(2)  VALUE ZERO.
005610         07 後療料２ＷＲ               PIC 9(5)  VALUE ZERO.
005620      05 冷罨法２ＷＲ.
005630         07 冷罨法回数２ＷＲ           PIC 9(2)  VALUE ZERO.
005640         07 冷罨法料２ＷＲ             PIC 9(4)  VALUE ZERO.
005650      05 温罨法２ＷＲ.
005660         07 温罨法回数２ＷＲ           PIC 9(2)  VALUE ZERO.
005670         07 温罨法料２ＷＲ             PIC 9(4)  VALUE ZERO.
005680      05 電療２ＷＲ.
005690         07 電療回数２ＷＲ             PIC 9(2)  VALUE ZERO.
005700         07 電療料２ＷＲ               PIC 9(4)  VALUE ZERO.
005710      05 小計２ＷＲ                    PIC 9(6)  VALUE ZERO.
005720      05 長期逓減率２ＷＲ              PIC 9(3)  VALUE ZERO.
005730      05 長期込小計２ＷＲ              PIC 9(6)  VALUE ZERO.
005740******************
005750* ３部位／８割 *
005760******************
005770   03 部位３８ＷＲ.
005780      05 後療３８ＷＲ.
005790         07 後療単価３８ＷＲ           PIC 9(4)  VALUE ZERO.
005800         07 後療回数３８ＷＲ           PIC 9(2)  VALUE ZERO.
005810         07 後療料３８ＷＲ             PIC 9(5)  VALUE ZERO.
005820      05 冷罨法３８ＷＲ.
005830         07 冷罨法回数３８ＷＲ         PIC 9(2)  VALUE ZERO.
005840         07 冷罨法料３８ＷＲ           PIC 9(4)  VALUE ZERO.
005850      05 温罨法３８ＷＲ.
005860         07 温罨法回数３８ＷＲ         PIC 9(2)  VALUE ZERO.
005870         07 温罨法料３８ＷＲ           PIC 9(4)  VALUE ZERO.
005880      05 電療３８ＷＲ.
005890         07 電療回数３８ＷＲ           PIC 9(2)  VALUE ZERO.
005900         07 電療料３８ＷＲ             PIC 9(4)  VALUE ZERO.
005910      05 小計３８ＷＲ                  PIC 9(6)  VALUE ZERO.
005920      05 多部位込小計３８ＷＲ          PIC 9(6)  VALUE ZERO.
005930      05 長期逓減率３８ＷＲ            PIC 9(3)  VALUE ZERO.
005940      05 長期込小計３８ＷＲ            PIC 9(6)  VALUE ZERO.
005950******************
005960* ３部位／１０割 *
005970******************
005980   03 部位３０ＷＲ.
005990      05 逓減開始月日３０ＷＲ.
006000         07 逓減開始月３０ＷＲ         PIC 9(2)  VALUE ZERO.
006010         07 逓減開始日３０ＷＲ         PIC 9(2)  VALUE ZERO.
006020      05 後療３０ＷＲ.
006030         07 後療単価３０ＷＲ           PIC 9(4)  VALUE ZERO.
006040         07 後療回数３０ＷＲ           PIC 9(2)  VALUE ZERO.
006050         07 後療料３０ＷＲ             PIC 9(5)  VALUE ZERO.
006060      05 冷罨法３０ＷＲ.
006070         07 冷罨法回数３０ＷＲ         PIC 9(2)  VALUE ZERO.
006080         07 冷罨法料３０ＷＲ           PIC 9(4)  VALUE ZERO.
006090      05 温罨法３０ＷＲ.
006100         07 温罨法回数３０ＷＲ         PIC 9(2)  VALUE ZERO.
006110         07 温罨法料３０ＷＲ           PIC 9(4)  VALUE ZERO.
006120      05 電療３０ＷＲ.
006130         07 電療回数３０ＷＲ           PIC 9(2)  VALUE ZERO.
006140         07 電療料３０ＷＲ             PIC 9(4)  VALUE ZERO.
006150      05 小計３０ＷＲ                  PIC 9(6)  VALUE ZERO.
006160      05 長期逓減率３０ＷＲ            PIC 9(3)  VALUE ZERO.
006170      05 長期込小計３０ＷＲ            PIC 9(6)  VALUE ZERO.
006180****************
006190* ４部位／５割 *
006200****************
006210   03 部位４５ＷＲ.
006220      05 後療４５ＷＲ.
006230         07 後療単価４５ＷＲ           PIC 9(4)  VALUE ZERO.
006240         07 後療回数４５ＷＲ           PIC 9(2)  VALUE ZERO.
006250         07 後療料４５ＷＲ             PIC 9(5)  VALUE ZERO.
006260      05 冷罨法４５ＷＲ.
006270         07 冷罨法回数４５ＷＲ         PIC 9(2)  VALUE ZERO.
006280         07 冷罨法料４５ＷＲ           PIC 9(4)  VALUE ZERO.
006290      05 温罨法４５ＷＲ.
006300         07 温罨法回数４５ＷＲ         PIC 9(2)  VALUE ZERO.
006310         07 温罨法料４５ＷＲ           PIC 9(4)  VALUE ZERO.
006320      05 電療４５ＷＲ.
006330         07 電療回数４５ＷＲ           PIC 9(2)  VALUE ZERO.
006340         07 電療料４５ＷＲ             PIC 9(4)  VALUE ZERO.
006350      05 小計４５ＷＲ                  PIC 9(6)  VALUE ZERO.
006360      05 多部位込小計４５ＷＲ          PIC 9(6)  VALUE ZERO.
006370      05 長期逓減率４５ＷＲ            PIC 9(3)  VALUE ZERO.
006380      05 長期込小計４５ＷＲ            PIC 9(6)  VALUE ZERO.
006390****************
006400* ４部位／８割 *
006410****************
006420   03 部位４８ＷＲ.
006430      05 逓減開始月日４８ＷＲ.
006440         07 逓減開始月４８ＷＲ         PIC 9(2)  VALUE ZERO.
006450         07 逓減開始日４８ＷＲ         PIC 9(2)  VALUE ZERO.
006460      05 後療４８ＷＲ.
006470         07 後療単価４８ＷＲ           PIC 9(4)  VALUE ZERO.
006480         07 後療回数４８ＷＲ           PIC 9(2)  VALUE ZERO.
006490         07 後療料４８ＷＲ             PIC 9(5)  VALUE ZERO.
006500      05 冷罨法４８ＷＲ.
006510         07 冷罨法回数４８ＷＲ         PIC 9(2)  VALUE ZERO.
006520         07 冷罨法料４８ＷＲ           PIC 9(4)  VALUE ZERO.
006530      05 温罨法４８ＷＲ.
006540         07 温罨法回数４８ＷＲ         PIC 9(2)  VALUE ZERO.
006550         07 温罨法料４８ＷＲ           PIC 9(4)  VALUE ZERO.
006560      05 電療４８ＷＲ.
006570         07 電療回数４８ＷＲ           PIC 9(2)  VALUE ZERO.
006580         07 電療料４８ＷＲ             PIC 9(4)  VALUE ZERO.
006590      05 小計４８ＷＲ                  PIC 9(6)  VALUE ZERO.
006600      05 多部位込小計４８ＷＲ          PIC 9(6)  VALUE ZERO.
006610      05 長期逓減率４８ＷＲ            PIC 9(3)  VALUE ZERO.
006620      05 長期込小計４８ＷＲ            PIC 9(6)  VALUE ZERO.
006630******************
006640* ４部位／１０割 *
006650******************
006660   03 部位４０ＷＲ.
006670      05 逓減開始月日４０ＷＲ.
006680         07 逓減開始月４０ＷＲ         PIC 9(2)  VALUE ZERO.
006690         07 逓減開始日４０ＷＲ         PIC 9(2)  VALUE ZERO.
006700      05 後療４０ＷＲ.
006710         07 後療単価４０ＷＲ           PIC 9(4)  VALUE ZERO.
006720         07 後療回数４０ＷＲ           PIC 9(2)  VALUE ZERO.
006730         07 後療料４０ＷＲ             PIC 9(5)  VALUE ZERO.
006740      05 冷罨法４０ＷＲ.
006750         07 冷罨法回数４０ＷＲ         PIC 9(2)  VALUE ZERO.
006760         07 冷罨法料４０ＷＲ           PIC 9(4)  VALUE ZERO.
006770      05 温罨法４０ＷＲ.
006780         07 温罨法回数４０ＷＲ         PIC 9(2)  VALUE ZERO.
006790         07 温罨法料４０ＷＲ           PIC 9(4)  VALUE ZERO.
006800      05 電療４０ＷＲ.
006810         07 電療回数４０ＷＲ           PIC 9(2)  VALUE ZERO.
006820         07 電療料４０ＷＲ             PIC 9(4)  VALUE ZERO.
006830      05 小計４０ＷＲ                  PIC 9(6)  VALUE ZERO.
006840      05 長期逓減率４０ＷＲ            PIC 9(3)  VALUE ZERO.
006850      05 長期込小計４０ＷＲ            PIC 9(6)  VALUE ZERO.
006860********************
006870* ５部位／２．５割 *
006880********************
006890   03 部位５２ＷＲ.
006900      05 後療５２ＷＲ.
006910         07 後療単価５２ＷＲ           PIC 9(4)  VALUE ZERO.
006920         07 後療回数５２ＷＲ           PIC 9(2)  VALUE ZERO.
006930         07 後療料５２ＷＲ             PIC 9(5)  VALUE ZERO.
006940      05 冷罨法５２ＷＲ.
006950         07 冷罨法回数５２ＷＲ         PIC 9(2)  VALUE ZERO.
006960         07 冷罨法料５２ＷＲ           PIC 9(4)  VALUE ZERO.
006970      05 温罨法５２ＷＲ.
006980         07 温罨法回数５２ＷＲ         PIC 9(2)  VALUE ZERO.
006990         07 温罨法料５２ＷＲ           PIC 9(4)  VALUE ZERO.
007000      05 電療５２ＷＲ.
007010         07 電療回数５２ＷＲ           PIC 9(2)  VALUE ZERO.
007020         07 電療料５２ＷＲ             PIC 9(4)  VALUE ZERO.
007030      05 小計５２ＷＲ                  PIC 9(6)  VALUE ZERO.
007040      05 多部位込小計５２ＷＲ          PIC 9(6)  VALUE ZERO.
007050      05 長期逓減率５２ＷＲ            PIC 9(3)  VALUE ZERO.
007060      05 長期込小計５２ＷＲ            PIC 9(6)  VALUE ZERO.
007070****************
007080* ５部位／５割 *
007090****************
007100   03 部位５５ＷＲ.
007110      05 逓減開始月日５５ＷＲ.
007120         07 逓減開始月５５ＷＲ         PIC 9(2)  VALUE ZERO.
007130         07 逓減開始日５５ＷＲ         PIC 9(2)  VALUE ZERO.
007140      05 後療５５ＷＲ.
007150         07 後療単価５５ＷＲ           PIC 9(4)  VALUE ZERO.
007160         07 後療回数５５ＷＲ           PIC 9(2)  VALUE ZERO.
007170         07 後療料５５ＷＲ             PIC 9(5)  VALUE ZERO.
007180      05 冷罨法５５ＷＲ.
007190         07 冷罨法回数５５ＷＲ         PIC 9(2)  VALUE ZERO.
007200         07 冷罨法料５５ＷＲ           PIC 9(4)  VALUE ZERO.
007210      05 温罨法５５ＷＲ.
007220         07 温罨法回数５５ＷＲ         PIC 9(2)  VALUE ZERO.
007230         07 温罨法料５５ＷＲ           PIC 9(4)  VALUE ZERO.
007240      05 電療５５ＷＲ.
007250         07 電療回数５５ＷＲ           PIC 9(2)  VALUE ZERO.
007260         07 電療料５５ＷＲ             PIC 9(4)  VALUE ZERO.
007270      05 小計５５ＷＲ                  PIC 9(6)  VALUE ZERO.
007280      05 多部位込小計５５ＷＲ          PIC 9(6)  VALUE ZERO.
007290      05 長期逓減率５５ＷＲ            PIC 9(3)  VALUE ZERO.
007300      05 長期込小計５５ＷＲ            PIC 9(6)  VALUE ZERO.
007310****************
007320* ５部位／８割 *
007330****************
007340   03 部位５８ＷＲ.
007350      05 逓減開始月日５８ＷＲ.
007360         07 逓減開始月５８ＷＲ         PIC 9(2)  VALUE ZERO.
007370         07 逓減開始日５８ＷＲ         PIC 9(2)  VALUE ZERO.
007380      05 後療５８ＷＲ.
007390         07 後療単価５８ＷＲ           PIC 9(4)  VALUE ZERO.
007400         07 後療回数５８ＷＲ           PIC 9(2)  VALUE ZERO.
007410         07 後療料５８ＷＲ             PIC 9(5)  VALUE ZERO.
007420      05 冷罨法５８ＷＲ.
007430         07 冷罨法回数５８ＷＲ         PIC 9(2)  VALUE ZERO.
007440         07 冷罨法料５８ＷＲ           PIC 9(4)  VALUE ZERO.
007450      05 温罨法５８ＷＲ.
007460         07 温罨法回数５８ＷＲ         PIC 9(2)  VALUE ZERO.
007470         07 温罨法料５８ＷＲ           PIC 9(4)  VALUE ZERO.
007480      05 電療５８ＷＲ.
007490         07 電療回数５８ＷＲ           PIC 9(2)  VALUE ZERO.
007500         07 電療料５８ＷＲ             PIC 9(4)  VALUE ZERO.
007510      05 小計５８ＷＲ                  PIC 9(6)  VALUE ZERO.
007520      05 多部位込小計５８ＷＲ          PIC 9(6)  VALUE ZERO.
007530      05 長期逓減率５８ＷＲ            PIC 9(3)  VALUE ZERO.
007540      05 長期込小計５８ＷＲ            PIC 9(6)  VALUE ZERO.
007550******************
007560* ５部位／１０割 *
007570******************
007580   03 部位５０ＷＲ.
007590      05 逓減開始月日５０ＷＲ.
007600         07 逓減開始月５０ＷＲ         PIC 9(2)  VALUE ZERO.
007610         07 逓減開始日５０ＷＲ         PIC 9(2)  VALUE ZERO.
007620      05 後療５０ＷＲ.
007630         07 後療単価５０ＷＲ           PIC 9(4)  VALUE ZERO.
007640         07 後療回数５０ＷＲ           PIC 9(2)  VALUE ZERO.
007650         07 後療料５０ＷＲ             PIC 9(5)  VALUE ZERO.
007660      05 冷罨法５０ＷＲ.
007670         07 冷罨法回数５０ＷＲ         PIC 9(2)  VALUE ZERO.
007680         07 冷罨法料５０ＷＲ           PIC 9(4)  VALUE ZERO.
007690      05 温罨法５０ＷＲ.
007700         07 温罨法回数５０ＷＲ         PIC 9(2)  VALUE ZERO.
007710         07 温罨法料５０ＷＲ           PIC 9(4)  VALUE ZERO.
007720      05 電療５０ＷＲ.
007730         07 電療回数５０ＷＲ           PIC 9(2)  VALUE ZERO.
007740         07 電療料５０ＷＲ             PIC 9(4)  VALUE ZERO.
007750      05 小計５０ＷＲ                  PIC 9(6)  VALUE ZERO.
007760      05 長期逓減率５０ＷＲ            PIC 9(3)  VALUE ZERO.
007770      05 長期込小計５０ＷＲ            PIC 9(6)  VALUE ZERO.
007780*
007790**************
007800* 施術所情報 *
007810**************
007820 01 施術所情報Ｗ.
007830    03 柔整師番号Ｗ                    PIC X(22) VALUE SPACE.
007840    03 接骨師会会員番号Ｗ              PIC X(10) VALUE SPACE.
007850    03 代表者カナＷ                    PIC X(50) VALUE SPACE.
007860    03 代表者名Ｗ.
007870       05 印刷代表者名Ｗ               PIC X(50) VALUE SPACE.
007880    03 接骨院名Ｗ                      PIC X(50) VALUE SPACE.
          03 都道府県ＪＩＳＷ                PIC X(2)   VALUE SPACE.
007890    03 施術所住所Ｗ.
007900       05 施術所住所１Ｗ               PIC X(50) VALUE SPACE.
007910       05 施術所住所２Ｗ               PIC X(50) VALUE SPACE.
007920    03 施術所郵便番号Ｗ.
007930       05 施術所郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
007940       05 施術所郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
007950    03 施術所電話番号Ｗ                PIC X(15) VALUE SPACE.
007960    03 接骨師会会長名Ｗ.
007970       05 印刷接骨師会会長名Ｗ         PIC N(7)  VALUE SPACE.
007980       05 FILLER                       PIC N(3)  VALUE SPACE.
007990    03 定額制受理番号Ｗ                PIC X(15) VALUE SPACE.
008000    03 柔整師年月日Ｗ.
008010       05 柔整師年Ｗ                   PIC 9(2)  VALUE ZERO.
008020       05 柔整師月Ｗ                   PIC 9(2)  VALUE ZERO.
008030       05 柔整師日Ｗ                   PIC 9(2)  VALUE ZERO.
008040    03 患者委任年月日Ｗ.
008050       05 患者委任年Ｗ                 PIC 9(2)  VALUE ZERO.
008060       05 患者委任月Ｗ                 PIC 9(2)  VALUE ZERO.
008070       05 患者委任日Ｗ                 PIC 9(2)  VALUE ZERO.
008080    03 取引先情報Ｗ.
008090        05 取引先銀行名Ｗ              PIC X(40) VALUE SPACE.
008100        05 取引先銀行支店名Ｗ          PIC X(40) VALUE SPACE.
008110        05 預金種別Ｗ                  PIC 9(1)  VALUE ZERO.
008120        05 銀行番号Ｗ                  PIC X(4)  VALUE ZERO.
008130        05 店番号Ｗ                    PIC X(3)  VALUE ZERO.
008140        05 口座番号Ｗ                  PIC X(10) VALUE SPACE.
008150        05 口座名義人Ｗ                PIC X(40) VALUE SPACE.
008160        05 口座名義人カナＷ            PIC X(40) VALUE SPACE.
008170        05 銀行名支店名Ｗ              PIC X(60) VALUE SPACE.
008180        05 預金種別名称Ｗ              PIC X(4)  VALUE SPACE.
008190        05 預金種別コメントＷ          PIC X(15) VALUE SPACE.
008200    03 県施術ＩＤＷ                    PIC X(15) VALUE SPACE.
008210    03 市町村施術ＩＤＷ                PIC X(15) VALUE SPACE.
008220    03 コメントＷ.
008230        05 コメント１Ｗ                PIC X(40) VALUE SPACE.
008240        05 コメント２Ｗ                PIC X(40) VALUE SPACE.
008250        05 コメント３Ｗ                PIC X(40) VALUE SPACE.
008260        05 コメント４Ｗ                PIC X(40) VALUE SPACE.
008270        05 コメント５Ｗ                PIC X(40) VALUE SPACE.
008280        05 コメント６Ｗ                PIC X(40) VALUE SPACE.
008290        05 コメント７Ｗ                PIC X(40) VALUE SPACE.
007330    03 共済番号Ｗ                      PIC X(28) VALUE SPACE.
008300**************
008310* 受診者情報 *
008320**************
008330 01 受診者情報Ｗ.
008340    03 患者番号Ｗ                      PIC 9(6)  VALUE ZERO.
008350    03 施術年月Ｗ.
008360       05 施術年Ｗ                     PIC 9(2)  VALUE ZERO.
008370       05 施術月Ｗ                     PIC 9(2)  VALUE ZERO.
008380*    03 記号Ｗ                          PIC N(12) VALUE SPACE.
007570    03 記号Ｗ.
007580       05 印刷記号Ｗ                   PIC N(12)  VALUE SPACE.
          03 記号番号Ｗ.
             05 記号番号ＸＷ                 PIC X(40) VALUE SPACE.
008390    03 番号Ｗ.
008400       05 印刷番号Ｗ                   PIC X(20) VALUE SPACE.
008410       05 FILLER                       PIC X(10) VALUE SPACE.
008420    03 保険者番号Ｗ.
008430       05 印刷保険者番号Ｗ             PIC X(8)  VALUE SPACE.
008440       05 FILLER                       PIC X(2)  VALUE SPACE.
008450*
008460    03 請求先名称Ｗ.
008470       05 印刷請求先名称１Ｗ           PIC X(40) VALUE SPACE.
008480       05 印刷請求先名称２Ｗ           PIC X(40) VALUE SPACE.
008490*
008500    03 保険種別Ｗ                      PIC 9(2)  VALUE ZERO.
007390    03 保険種別チェックＷ.
007400       05 社保チェックＷ               PIC N(1)  VALUE SPACE.
007410       05 船員チェックＷ               PIC N(1)  VALUE SPACE.
007420       05 組合チェックＷ               PIC N(1)  VALUE SPACE.
007430       05 国保チェックＷ               PIC N(1)  VALUE SPACE.
             05 共済チェックＷ               PIC N(1)  VALUE SPACE.
             05 自チェックＷ                 PIC N(1)  VALUE SPACE.
             05 退職チェックＷ               PIC N(1)  VALUE SPACE.
             05 後期チェックＷ               PIC N(1)  VALUE SPACE.
          03 本人チェックＷ                  PIC N(1)   VALUE SPACE.
          03 家族チェックＷ                  PIC N(1)   VALUE SPACE.
          03 単独チェックＷ                  PIC N(1)   VALUE SPACE.
          03 ２併チェックＷ                  PIC N(1)   VALUE SPACE.
          03 高一チェックＷ                  PIC N(1)   VALUE SPACE.
          03 高７チェックＷ                  PIC N(1)   VALUE SPACE.
          03 ６歳チェックＷ                  PIC N(1)   VALUE SPACE.
007750    03 給付割合チェックＷ.
007760       05 ７割チェックＷ               PIC N(1)  VALUE SPACE.
007770       05 ８割チェックＷ               PIC N(1)  VALUE SPACE.
007780       05 ９割チェックＷ               PIC N(1)  VALUE SPACE.
007790       05 １０割チェックＷ             PIC N(1)  VALUE SPACE.
008510    03 保険種別親Ｗ                    PIC N(2)  VALUE SPACE.
008520    03 保険種別編集Ｗ                  PIC N(5)  VALUE SPACE.
008530    03 被保険者情報Ｗ.
008540       05 被保険者カナＷ               PIC X(50) VALUE SPACE.
008550       05 被保険者氏名Ｗ               PIC X(50) VALUE SPACE.
008560       05 郵便番号Ｗ.
008570          07 郵便番号１Ｗ              PIC X(3)  VALUE SPACE.
008580          07 郵便番号２Ｗ              PIC X(4)  VALUE SPACE.
008590       05 被保険者住所Ｗ.
008600          07 被保険者住所１Ｗ          PIC X(50) VALUE SPACE.
008610          07 被保険者住所２Ｗ          PIC X(50) VALUE SPACE.
008990       05 電話番号Ｗ                   PIC X(35)  VALUE SPACE.
008620    03 患者情報Ｗ.
008630       05 患者カナＷ                   PIC X(50) VALUE SPACE.
008640       05 患者氏名Ｗ                   PIC X(50) VALUE SPACE.
008650       05 性別チェックＷ.
008660          07 男チェックＷ              PIC N(1)  VALUE SPACE.
008670          07 女チェックＷ              PIC N(1)  VALUE SPACE.
008680          07 性別Ｗ                    PIC N(2)  VALUE SPACE.
008690       05 和暦チェックＷ.
008700          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
008710          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
008720          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
008730          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
008740          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
008750       05 患者年Ｗ                     PIC 9(2)  VALUE ZERO.
008760       05 患者月Ｗ                     PIC 9(2)  VALUE ZERO.
008770       05 患者日Ｗ                     PIC 9(2)  VALUE ZERO.
008780       05 続柄Ｗ.
008790          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
008800          07 FILLER                    PIC X(4)  VALUE SPACE.
008810*       05 続柄チェックＷ.
008820*          07 本人チェックＷ            PIC N(1)  VALUE SPACE.
008830*          07 家族チェックＷ            PIC N(1)  VALUE SPACE.
008840*
008850*       05 負傷原因Ｗ                   PIC N(40) OCCURS 29 VALUE SPACE.
      */半角対応/110421
             05 負傷原因Ｗ OCCURS 29.
                07 負傷原因ＸＷ              PIC X(80)  VALUE SPACE.
008860*
008870    03 特別区分チェックＷ.
008880       05 ７０歳以上チェックＷ         PIC N(1)  VALUE SPACE.
008890       05 未就学チェックＷ             PIC N(1)  VALUE SPACE.
008900       05 高齢割合Ｗ                   PIC X(1)  VALUE SPACE.
008910*
008920    03 保険種別チェックＷ.
008930       05 退チェックＷ                 PIC N(1)  VALUE SPACE.
008940       05 特国チェックＷ               PIC N(1)  VALUE SPACE.
008950*
008960    03 助成種別Ｗ.
008970       05 助成印Ｗ                     PIC N(1)  VALUE SPACE.
008980       05 助成印チェックＷ             PIC N(1)  VALUE SPACE.
008990       05 助成番号Ｗ                   PIC X(2)  VALUE SPACE.
009000*
009010*    03 給付割合チェックＷ.
009020*       05 ７割チェックＷ               PIC N(1)  VALUE SPACE.
009030*       05 ８割チェックＷ               PIC N(1)  VALUE SPACE.
009040*       05 ９割チェックＷ               PIC N(1)  VALUE SPACE.
009050*       05 １０割チェックＷ             PIC N(1)  VALUE SPACE.
009060*
009070    03 特別コメントＷ                  PIC X(16) VALUE SPACE.
          03 災Ｗ                            PIC N(1)  VALUE SPACE.
          03 災丸印Ｗ                        PIC N(1)  VALUE SPACE.
009080*
009090****************
009100* 負傷データＦ *
009110****************
009120 01 負傷情報Ｗ.
009130    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
009140    03 部位情報Ｗ  OCCURS   9.
009150       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
009160       05 部位コードＷ.
009170          07 負傷種別Ｗ                PIC 9(2)  VALUE ZERO.
009180          07 部位Ｗ                    PIC 9(2)  VALUE ZERO.
009190          07 左右区分Ｗ                PIC 9(1)  VALUE ZERO.
009200          07 負傷位置番号Ｗ            PIC 9(2)  VALUE ZERO.
009210       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
009220       05 負傷年月日Ｗ.
009230          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
009240          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
009250          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
009260       05 初検年月日Ｗ.
009270          07 初検年Ｗ                  PIC 9(2)  VALUE ZERO.
009280          07 初検月Ｗ                  PIC 9(2)  VALUE ZERO.
009290          07 初検日Ｗ                  PIC 9(2)  VALUE ZERO.
009300       05 開始年月日Ｗ.
009310          07 開始年Ｗ                  PIC 9(2)  VALUE ZERO.
009320          07 開始月Ｗ                  PIC 9(2)  VALUE ZERO.
009330          07 開始日Ｗ                  PIC 9(2)  VALUE ZERO.
009340       05 終了年月日Ｗ.
009350          07 終了年Ｗ                  PIC 9(2)  VALUE ZERO.
009360          07 終了月Ｗ                  PIC 9(2)  VALUE ZERO.
009370          07 終了日Ｗ                  PIC 9(2)  VALUE ZERO.
009380       05 実日数Ｗ                     PIC 9(2)  VALUE ZERO.
009390       05 転帰区分Ｗ                   PIC 9(1)  VALUE ZERO.
009400       05 転帰区分チェックＷ.
009410          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
009420          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
009430          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
009440       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
009450       05 部位区切Ｗ                   PIC X(1)  VALUE SPACE.
009460       05 経過略称Ｗ.
009470          07 印刷経過略称Ｗ            PIC N(5)  VALUE SPACE.
009480          07 FILLER                    PIC X(2)  VALUE SPACE.
009490    03 新規チェックＷ                  PIC N(1)  VALUE SPACE.
009500    03 継続チェックＷ                  PIC N(1)  VALUE SPACE.
          03 施術日Ｗ.
             05 施術日チェックＷ   OCCURS 31 PIC N(1)  VALUE SPACE.
009510*
009520************
009530* 料金情報 *
009540************
009550 01 料金情報Ｗ.
009560    03 初検加算Ｗ.
009570       05 時間外チェックＷ             PIC N(1)  VALUE SPACE.
009580       05 休日チェックＷ               PIC N(1)  VALUE SPACE.
009590       05 深夜チェックＷ               PIC N(1)  VALUE SPACE.
009600    03 往療加算Ｗ.
009610       05 夜間チェックＷ               PIC N(1)  VALUE SPACE.
009620       05 難路チェックＷ               PIC N(1)  VALUE SPACE.
009630       05 暴風雨雪チェックＷ           PIC N(1)  VALUE SPACE.
009640    03 金属副子チェックＷ.
009650       05 大チェックＷ                 PIC N(1)  VALUE SPACE.
009660       05 中チェックＷ                 PIC N(1)  VALUE SPACE.
009670       05 小チェックＷ                 PIC N(1)  VALUE SPACE.
009680    03 小計Ｗ                          PIC 9(7)  VALUE ZERO.
009690    03 初回処置料合計Ｗ                PIC 9(6)  VALUE ZERO.
      */金属副子・運動後療の変更・追加/1805
          03 金属回数Ｗ                         PIC 9(2)  VALUE ZERO.
          03 運動料Ｗ                           PIC 9(4)  VALUE ZERO.
009700************
009710* 備考情報 *
009720************
009730 01 備考情報Ｗ.
009740    03 適用１Ｗ                        PIC N(38) VALUE SPACE.
009750    03 適用２Ｗ                        PIC N(38) VALUE SPACE.
009760    03 経過コメントＷ                  PIC N(60) VALUE SPACE.
009770*
009780***************************
009790** レセ摘要用( N(38)固定）*
009800***************************
009810 01 負傷の経過Ｗ.
009820    03 負傷の経過行Ｗ                  PIC X(76) OCCURS 2 VALUE SPACE.
009830 01 負傷の経過ＮＷ REDEFINES 負傷の経過Ｗ.
009840    03 負傷の経過行ＮＷ                PIC N(38) OCCURS 2.
009850*
       01 摘要施術日Ｗ                       PIC X(100) VALUE SPACE.
       01 施術日Ｗ.
          03 施術日２Ｗ                      PIC X(1)  VALUE SPACE.
          03 施術日１Ｗ                      PIC X(1)  VALUE SPACE.
004460* レセプト並び順 *
004470 01 順番Ｗ                             PIC 9(4) VALUE ZERO.
004480*
009860*************************************************************************
009870 01 印刷制御.
009880     03 定義体名Ｐ                     PIC X(8)  VALUE SPACE.
009890     03 項目群名Ｐ                     PIC X(8)  VALUE SPACE.
009900     03 処理種別Ｐ                     PIC X(2)  VALUE SPACE.
009910     03 拡張制御Ｐ.
009920         05 端末制御Ｐ.
009930             07 移動方向Ｐ             PIC X(1)  VALUE SPACE.
009940             07 移動行数Ｐ             PIC 9(3)  VALUE ZERO.
009950         05 詳細制御Ｐ                 PIC X(2)  VALUE SPACE.
009960     03 通知情報Ｐ                     PIC X(2)  VALUE SPACE.
009970     03 ユニット名Ｐ                   PIC X(8)  VALUE SPACE.
009980*
009990 01 計算機西暦年Ｗ                     PIC 9(2)  VALUE ZERO.
010000* 日付ＷＯＲＫ
010010 01 和暦終了年Ｗ                       PIC 9(4)  VALUE ZERO.
010020 01 計算機西暦.
010030    03 計算機西暦年                    PIC 9(4)  VALUE ZERO.
010040    03 計算機西暦月日                  PIC 9(4)  VALUE ZERO.
010050 01 計算機西暦Ｒ REDEFINES 計算機西暦.
010060    03 計算機世紀                      PIC 9(2).
010070    03 計算機日付                      PIC 9(6).
010080    03 計算機日付Ｒ REDEFINES 計算機日付.
010090       05 計算機年月                   PIC 9(4).
010100       05 計算機年月Ｒ REDEFINES 計算機年月.
010110         07 計算機年                   PIC 9(2).
010120         07 計算機月                   PIC 9(2).
010130       05 計算機日                     PIC 9(2).
010140*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
010150******************************************************************
010160*                          連結項目                              *
010170******************************************************************
010180**  画面入力データ
010190 01 連入－入力データ委任印刷 IS EXTERNAL.
010200    03 連入－委任印刷                  PIC 9.
       01 連入－入力データ電話印刷 IS EXTERNAL.
          03 連入－電話印刷                     PIC 9.
010210*
       01 連入－プレビュー IS EXTERNAL.
          03 連入－プレビュー区分          PIC 9.
010300*
010220******************
010230* ３カ月長期判定 *
010240******************
010250 01 連期間－キー IS EXTERNAL.
010260    03 連期間－施術年月.
010270       05 連期間－施術和暦             PIC 9.
010280       05 連期間－施術年               PIC 9(2).
010290       05 連期間－施術月               PIC 9(2).
010300    03  連期間－患者コード.
010310       05 連期間－患者番号             PIC 9(6).
010320       05 連期間－枝番                 PIC X.
010330    03 連期間－対象フラグ              PIC X(3).
010340    03 連期間－期間月Ｗ.
010350       05 連期間－期間Ｗ               PIC 9(2) OCCURS 9.
010360*
010370************
010380* 印刷キー *
010390************
010400*
010410*
010420 01 連レ印－対象データ IS EXTERNAL.
010430    03 連レ印－施術年月日.
010440       05 連レ印－施術和暦             PIC 9(1).
010450       05 連レ印－施術年               PIC 9(2).
010460       05 連レ印－施術月               PIC 9(2).
010470    03 連レ印－患者コード.
010480       05 連レ印－患者番号             PIC 9(6).
010490       05 連レ印－枝番                 PIC X(1).
010500    03 連レ印－保険種別                PIC 9(2).
010510    03 連レ印－保険者番号              PIC X(10).
010520    03 連レ印－公費種別                PIC 9(2).
010530    03 連レ印－費用負担者番号          PIC X(10).
010540    03 連レ印－助成種別                PIC 9(2).
010550    03 連レ印－費用負担者番号助成      PIC X(10).
010560    03 連レ印－患者カナ                PIC X(20).
010570    03 連レ印－本人家族区分            PIC 9(1).
014020*
014030 01 連レ－キー IS EXTERNAL.
014040    03 連レ－保険種別                  PIC 9(2).
014050*
014230************************
014240* 摘要文セット     *
014250************************
014260 01 連摘文－キー IS EXTERNAL.
014270    03 連摘文－施術年月.
014280       05 連摘文－施術和暦               PIC 9.
014290       05 連摘文－施術年                 PIC 9(2).
014300       05 連摘文－施術月                 PIC 9(2).
014310    03  連摘文－患者コード.
014320       05 連摘文－患者番号               PIC 9(6).
014330       05 連摘文－枝番                   PIC X.
014340    03 連摘文－文桁数                    PIC 9(2).
014350    03 連摘文－摘要文                    PIC X(126) OCCURS 30.
014340    03 連摘文－長期区分                  PIC 9(1).
014370*
014060************************
014070* 長期理由文セット     *
014080************************
014090 01 連長文－キー IS EXTERNAL.
014100    03 連長文－施術年月.
014110       05 連長文－施術和暦             PIC 9.
014120       05 連長文－施術年               PIC 9(2).
014130       05 連長文－施術月               PIC 9(2).
014140    03  連長文－患者コード.
014150       05 連長文－患者番号             PIC 9(6).
014160       05 連長文－枝番                 PIC X.
014170    03 連長文－文桁数                  PIC 9(2).
014180    03 連長文－理由文                  PIC N(63) OCCURS 15.
014190*
007670* 負担率取得用14/10～
007680 01 連率－負担率取得キー IS EXTERNAL.
007690    03 連率－施術和暦年月.
007700       05 連率－施術和暦               PIC 9.
007710       05 連率－施術年月.
007720          07 連率－施術年              PIC 9(2).
007730          07 連率－施術月              PIC 9(2).
007740    03 連率－患者コード.
007750       05 連率－患者番号               PIC 9(6).
007760       05 連率－枝番                   PIC X.
007770    03 連率－実際負担率                PIC 9(3).
007780    03 連率－実際本体負担率            PIC 9(3).
007790    03 連率－健保負担率                PIC 9(3).
007800    03 連率－２７老負担率              PIC 9(3).
007810    03 連率－助成負担率                PIC 9(3).
007820    03 連率－特別用負担率              PIC 9(3).
007100*
014761*
014762************************
014763* レセ負傷原因印刷判定
014764************************
014765 01 連レセ負原印－キー IS EXTERNAL.
014766    03 連レセ負原印－施術年月.
014767       05 連レセ負原印－施術和暦               PIC 9.
014768       05 連レセ負原印－施術年                 PIC 9(2).
014769       05 連レセ負原印－施術月                 PIC 9(2).
014770    03  連レセ負原印－患者コード.
014771       05 連レセ負原印－患者番号               PIC 9(6).
014772       05 連レセ負原印－枝番                   PIC X.
014773    03 連レセ負原印－対象フラグ                PIC X(3).
014774*
000540************************************
000550* プリンタファイル作成用           *
000560************************************
000570 01 Ｈ連ＰＲＴＦ－作成データ IS EXTERNAL.
000580     03 Ｈ連ＰＲＴＦ－ファイル名           PIC X(8).
000590     03 Ｈ連ＰＲＴＦ－プレビュー区分       PIC 9.
000600     03 Ｈ連ＰＲＴＦ－帳票プログラム名     PIC X(8).
000610     03 Ｈ連ＰＲＴＦ－オーバレイ名         PIC X(8).
000993************************************
000994* プリンタファイル作成特殊用       *
000995************************************
000996 01 Ｈ連特殊ＰＲＴＦ－作成データ IS EXTERNAL.
000997     03 Ｈ連特殊ＰＲＴＦ－用紙種類         PIC X(8).
006490*
      * 暗号複合用
       01 連暗号複合－暗号情報 IS EXTERNAL.
          03 連暗号複合－入力情報.
             05 連暗号複合－記号               PIC X(24).
             05 連暗号複合－番号               PIC X(30).
             05 連暗号複合－暗号化項目.
               07 連暗号複合－暗号患者番号     PIC X(6).
               07 連暗号複合－暗号判定記号     PIC X.
               07 連暗号複合－暗号判定番号     PIC X.
               07 連暗号複合－暗号記号         PIC X(24).
               07 連暗号複合－暗号番号         PIC X(30).
          03 連暗号複合－出力情報.
             05 連暗号複合－複合した記号       PIC X(24).
             05 連暗号複合－複合した番号       PIC X(30).
      * 
      */金属副子・運動後療の変更・追加/1805
       01 連金運－キー IS EXTERNAL.
          03 連金運－施術和暦年月.
             05 連金運－施術和暦                  PIC 9(1).
             05 連金運－施術年月.
                07 連金運－施術年                 PIC 9(2).
                07 連金運－施術月                 PIC 9(2).
          03 連金運－患者コード.
             05 連金運－患者番号                  PIC 9(6).
             05 連金運－枝番                      PIC X(1).
          03 連金運－保険種別                     PIC 9(2).
          03 連金運－会コード                     PIC 9(2).
          03 連金運－用紙種別                     PIC 9(1).
          03 連金運－金属副子.
             05 連金運－金属副子ＣＭ              PIC X(200).
             05 連金運－金属副子部位              OCCURS 5.
                07 連金運－金属副子和暦年月日     OCCURS 3.
                   09 連金運－金属副子和暦年月.
                      11 連金運－金属副子和暦     PIC 9(1).
                      11 連金運－金属副子年月.
                         13 連金運－金属副子年    PIC 9(2).
                         13 連金運－金属副子月    PIC 9(2).
                   09 連金運－金属副子日          PIC 9(2).
          03 連金運－運動後療.
             05 連金運－運動後療ＣＭ              PIC X(100).
             05 連金運－運動日                    PIC 9(2)    OCCURS 5.
      * 
014200******************************************************************
014210*                      PROCEDURE  DIVISION                       *
014220******************************************************************
014230 PROCEDURE               DIVISION.
014240************
014250*           *
014260* 初期処理   *
014270*           *
014280************
002570     PERFORM プリンタファイル作成.
014290     PERFORM 初期化.
014300************
014310*           *
014320* 主処理     *
014330*           *
014340************
014350* 印刷
014360     PERFORM 連結項目待避.
014370     PERFORM 印刷セット.
014380     PERFORM 印刷処理.
014390************
014400*           *
014410* 終了処理   *
014420*           *
014430************
014440     PERFORM 受診者印刷区分更新.
014450     PERFORM 終了処理.
014460     MOVE ZERO  TO PROGRAM-STATUS.
014470     EXIT PROGRAM.
014480*
014490*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
014500*=== 初期処理 ===================================================*
002860*================================================================*
002870 プリンタファイル作成 SECTION.
002880*================================================================*
002890*   / 初期化 /
002900     MOVE SPACE TO Ｈ連ＰＲＴＦ－作成データ.
002910     INITIALIZE Ｈ連ＰＲＴＦ－作成データ.
002225     MOVE SPACE TO Ｈ連特殊ＰＲＴＦ－作成データ.
002226     INITIALIZE Ｈ連特殊ＰＲＴＦ－作成データ.
002920*
002930*
002940*--↓↓ 変更箇所 ↓↓--------------------------------------*
002230*   使用する用紙種別セット
           MOVE "RECE"                TO Ｈ連特殊ＰＲＴＦ－用紙種類.
002970*   使用するプリンタファイル名セット
002971     MOVE "PRTF002"             TO Ｈ連ＰＲＴＦ－ファイル名.
002972*
002973*   使用する帳票プログラム名セット
002974     MOVE "YCB6125"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分  TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
014510*================================================================*
014520 初期化 SECTION.
014530*================================================================*
014540     PERFORM ファイルオープン.
014550*    /* 現在日付取得 */
014560     ACCEPT 計算機日付 FROM DATE.
014570*    /* 1980～2079年の間で設定 */
014580     IF ( 計算機年 > 80 )
014590        MOVE 19 TO 計算機世紀
014600     ELSE
014610        MOVE 20 TO 計算機世紀
014620     END-IF.
014630     PERFORM カレント元号取得.
014640     PERFORM 和暦終了年取得.
014650     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
014660*
014670*================================================================*
014680 ファイルオープン SECTION.
014690*
014700     OPEN INPUT   元号マスタ
014710         MOVE NC"元号" TO ファイル名.
014720         PERFORM オープンチェック.
014730     OPEN INPUT   名称マスタ
014740         MOVE NC"名称" TO ファイル名.
014750         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
014790     OPEN INPUT   経過マスタ
014800         MOVE NC"経過" TO ファイル名.
014810         PERFORM オープンチェック.
014820     OPEN INPUT   制御情報マスタ
014830         MOVE NC"制御情報" TO ファイル名.
014840         PERFORM オープンチェック.
014850     OPEN INPUT   施術所情報マスタ
014860         MOVE NC"施情" TO ファイル名.
014870         PERFORM オープンチェック.
014880     OPEN INPUT   会情報マスタ.
014890         MOVE NC"会情報マスタ" TO ファイル名.
014900         PERFORM オープンチェック.
014910     OPEN INPUT   保険者マスタ
014920         MOVE NC"保険者" TO ファイル名.
014930         PERFORM オープンチェック.
014970     OPEN INPUT   ＩＤ管理マスタ
014980         MOVE NC"ＩＤ" TO ファイル名.
014990         PERFORM オープンチェック.
015000     OPEN INPUT 市町村マスタ.
015010         MOVE NC"市町村" TO ファイル名.
015020         PERFORM オープンチェック.
015030     OPEN INPUT   施術記録Ｆ.
015040         MOVE NC"施記Ｆ" TO ファイル名.
015050         PERFORM オープンチェック.
015060     OPEN INPUT   負傷データＦ.
015070         MOVE NC"負傷" TO ファイル名.
015080         PERFORM オープンチェック.
015090     OPEN INPUT   負傷原因Ｆ.
015100         MOVE NC"負傷原因" TO ファイル名.
015110         PERFORM オープンチェック.
016210     OPEN INPUT 作業ファイル２.
016220         MOVE NC"作２" TO ファイル名.
016230         PERFORM オープンチェック.
015150*
015160     OPEN I-O   受診者情報Ｆ.
015170         MOVE NC"受情" TO ファイル名.
015180         PERFORM オープンチェック.
015190     OPEN I-O   印刷ファイル
015200         PERFORM エラー処理Ｐ.
015210*
015220*================================================================*
015230 オープンチェック SECTION.
015240*
015250     IF ( 状態キー  NOT =  "00" )
015260        DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
015270        DISPLAY NC"状態キー：" 状態キー         UPON CONS
015280        DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
015290                                                UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015300        ACCEPT  キー入力 FROM CONS
015310        PERFORM ファイル閉鎖
015320        EXIT PROGRAM.
015330*
015340*================================================================*
015350 カレント元号取得 SECTION.
015360*
015370     MOVE ZEROS TO 制－制御区分.
015380     READ 制御情報マスタ
015390     NOT INVALID KEY
015400         MOVE 制－カレント元号         TO カレント元号Ｗ
015410         MOVE 制－レセ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
015420         MOVE 制－レセ長期理由印刷区分 TO 長期理由印刷区分Ｗ
015430         MOVE 制－レセプト日付区分     TO レセプト日付区分Ｗ
015440         MOVE 制－レセプト患者日付区分 TO レセプト患者日付区分Ｗ
015450     END-READ.
015460*
015470*================================================================*
015480 和暦終了年取得 SECTION.
015490*
015500*     DISPLAY NC"カレント元号Ｗ"  カレント元号Ｗ UPON MSGBOX.
015510     MOVE カレント元号Ｗ TO 元－元号区分.
015520     READ 元号マスタ
015530     INVALID KEY
015540         DISPLAY NC"指定和暦が登録されていません" UPON CONS
015550         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
015560                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015570         ACCEPT  キー入力 FROM CONS
015580         PERFORM 終了処理
015590         EXIT PROGRAM
015600     NOT INVALID KEY
015610         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
015620         MOVE 前和暦Ｗ TO 元－元号区分
015630         READ 元号マスタ
015640         INVALID KEY
015650             DISPLAY NC"指定和暦が登録されていません" UPON CONS
015660             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
015670                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015680             ACCEPT  キー入力 FROM CONS
015690             PERFORM 終了処理
015700             EXIT PROGRAM
015710         NOT INVALID KEY
015720             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
015730         END-READ
015740     END-READ.
015750*
015760*=== 主処理 =====================================================*
015770*================================================================*
015780 連結項目待避 SECTION.
015790*================================================================*
015800     MOVE 連レ印－施術和暦           TO 施術和暦ＷＲ.
015810     MOVE 連レ印－施術年             TO 施術年ＷＲ.
015820     MOVE 連レ印－施術月             TO 施術月ＷＲ.
015830     MOVE 連レ印－保険種別           TO 保険種別ＷＲ.
015840     MOVE 連レ印－保険者番号         TO 保険者番号ＷＲ.
015850     MOVE 連レ印－公費種別           TO 公費種別ＷＲ.
015860     MOVE 連レ印－費用負担者番号     TO 費用負担者番号ＷＲ.
015870     MOVE 連レ印－助成種別           TO 助成種別ＷＲ.
015880     MOVE 連レ印－費用負担者番号助成 TO 費用負担者番号助成ＷＲ.
015890     MOVE 連レ印－本人家族区分       TO 本人家族区分ＷＲ.
015900     MOVE 連レ印－患者カナ           TO 患者カナＷＲ.
015910     MOVE 連レ印－患者番号           TO 患者番号ＷＲ.
015920     MOVE 連レ印－枝番               TO 枝番ＷＲ.
015930*
015940*================================================================*
015950 印刷セット SECTION.
015960*================================================================*
015970     PERFORM 項目初期化.
           PERFORM 基本情報取得.
015980     PERFORM 施術所情報取得.
015990     PERFORM 請求先情報取得.
016010     PERFORM 受診者情報取得.
016020     PERFORM 負傷データ取得.
016030     PERFORM 料金情報取得.
016040     PERFORM 施術記録取得.
016050***     PERFORM 長期判定取得.
016070     PERFORM 初検加算時刻取得.
016080     PERFORM 委任年月日取得.
           PERFORM 施術日取得.
      */並び順印刷/1105
           PERFORM レセプト並び順取得.
016090*
016100* / 制御マスタ・負傷データＦの印刷区分を確認し取得 /
016791*-----------------------------------------------*
016800     IF ( 負傷原因印刷区分Ｗ  NOT = 1 ) AND ( レセ負傷原因印刷区分Ｗ NOT = 1 )
016813        IF ( 負傷原因印刷区分Ｗ = 3 OR 4 )
016815           PERFORM 負傷原因印刷対象判定処理
016817        ELSE
016820           PERFORM 負傷原因取得
016821        END-IF
016830     END-IF.
016831*-----------------------------------------------*
016150*
015940     IF ( 長期理由印刷区分Ｗ NOT = 1 )
               MOVE 長期理由印刷区分Ｗ TO 連摘文－長期区分
016210     END-IF.
016220*
016230     PERFORM 施術ＩＤ取得.
016240*     PERFORM レセプト回数取得.
016250*     PERFORM 給付割合取得.
016260*
016270********************
016280* 受診者情報セット *
016290********************
016300*     MOVE 回数Ｗ              TO 回数.
016340*     MOVE 特別コメントＷ      TO 特別コメント.
016390*     MOVE 保険種別編集Ｗ       TO 保険種別.
016400*     MOVE 保険種別親Ｗ         TO 保険種別２.
016410*     MOVE 未就学チェックＷ     TO 未就学チェック.
016420*     MOVE ７０歳以上チェックＷ TO ７０歳以上チェック.
016430*     MOVE 高齢割合Ｗ           TO 高齢割合.
           MOVE 災Ｗ               TO 災.
           MOVE 災丸印Ｗ           TO 災丸印.
015190     MOVE 社保チェックＷ     TO 社保チェック.
015210     MOVE 組合チェックＷ     TO 組合チェック.
015220     MOVE 国保チェックＷ     TO 国保チェック.
           MOVE 共済チェックＷ     TO 共済チェック.
           MOVE 自チェックＷ       TO 自チェック.
           MOVE 退職チェックＷ     TO 退職チェック.
           MOVE 後期チェックＷ     TO 後期チェック.
015230     MOVE ７割チェックＷ     TO ７割チェック.
015240     MOVE ８割チェックＷ     TO ８割チェック.
015250     MOVE ９割チェックＷ     TO ９割チェック.
015260     MOVE １０割チェックＷ   TO １０割チェック.
      *
           MOVE 本人チェックＷ     TO 本人チェック.
           MOVE 家族チェックＷ     TO 家族チェック.
           MOVE 単独チェックＷ     TO 単独チェック.
           MOVE ２併チェックＷ     TO ２併チェック.
           MOVE 高一チェックＷ     TO 高一チェック.
           MOVE 高７チェックＷ     TO 高７チェック.
           MOVE ６歳チェックＷ     TO ６歳チェック.
016440     EVALUATE 受－保険種別
016450     WHEN 04
016460         PERFORM 共済番号セット
016580*         MOVE 共済連番号名Ｗ   TO 施術ＩＤ固定
016470*         MOVE 県施術ＩＤＷ     TO 県施術ＩＤ
               STRING 共済連番号名Ｗ DELIMITED BY SPACE
                      県施術ＩＤＷ   DELIMITED BY SIZE
                 INTO 共済番号Ｗ
               END-STRING
               MOVE 共済番号Ｗ       TO 共済番号
016480     WHEN 09
016490         PERFORM 自衛官番号セット
016580*         MOVE 自衛官番号名Ｗ   TO 施術ＩＤ固定
016500*         MOVE 県施術ＩＤＷ     TO 県施術ＩＤ
               STRING 自衛官番号名Ｗ DELIMITED BY SPACE
                      県施術ＩＤＷ   DELIMITED BY SIZE
                 INTO 共済番号Ｗ
               END-STRING
               MOVE 共済番号Ｗ       TO 共済番号
016510     WHEN 02
016520     WHEN 03
016530     WHEN 06
016540     WHEN 07
016550         MOVE SPACE            TO 県施術ＩＤ
016560     WHEN OTHER
016570         IF ( 県施術ＩＤＷ NOT = SPACE )
016580*            MOVE 施術ＩＤ固定Ｗ   TO 施術ＩＤ固定
016590            MOVE 県施術ＩＤＷ     TO 県施術ＩＤ
016600         END-IF
016610     END-EVALUATE.
016620*
016630     MOVE 施術年Ｗ            TO 施術年.
016640     MOVE 施術月Ｗ            TO 施術月.
016650*
016660*     IF ( 記号Ｗ(1:1) = NC"＊" )
016670*        MOVE  SPACE           TO  記号
016680*     ELSE
016690*        MOVE 記号Ｗ           TO  記号
016700*     END-IF.
016710*     IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
016720*        ( 印刷番号Ｗ(1:2) = "＊" )
016730*        MOVE  SPACE           TO  番号
016740*     ELSE
016750*        MOVE 印刷番号Ｗ       TO  番号
016760*     END-IF.
           IF ( 印刷記号Ｗ(1:1) = NC"＊" )
              MOVE  SPACE          TO  記号Ｗ
           END-IF.
           IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
              ( 印刷番号Ｗ(1:2) = "＊" )
              MOVE  SPACE          TO  番号Ｗ
           END-IF.
      *
           INSPECT 記号Ｗ  REPLACING ALL "　" BY "  ".
           EVALUATE TRUE
           WHEN (記号Ｗ NOT = SPACE) AND (番号Ｗ NOT = SPACE)
               MOVE SPACE TO 終了フラグ２
               PERFORM VARYING カウンタ FROM 24 BY -1
                 UNTIL (カウンタ <= ZERO) OR (終了フラグ２ NOT = SPACE)
                   IF 記号Ｗ(カウンタ:1) NOT = SPACE
                       MOVE 記号Ｗ TO 記号番号Ｗ
                       MOVE "  "   TO 記号番号Ｗ(カウンタ + 1:2)
                       MOVE 番号Ｗ TO 記号番号Ｗ(カウンタ + 3:40 - カウンタ - 2)
                       MOVE "YES"  TO 終了フラグ２
                   END-IF
               END-PERFORM
               MOVE 記号番号Ｗ TO 記号番号
           WHEN 記号Ｗ NOT = SPACE
               MOVE 記号Ｗ TO 記号番号
           WHEN 番号Ｗ NOT = SPACE
               MOVE 番号Ｗ TO 記号番号
           END-EVALUATE.
016770*
016780     MOVE 印刷保険者番号Ｗ    TO 保険者番号.
016790*     MOVE 印刷請求先名称１Ｗ  TO 請求先名称.
016800***     MOVE 印刷請求先名称２Ｗ  TO 請求先名称２.
016810***     MOVE 被保険者カナＷ      TO 被保険者カナ.
016820     MOVE 被保険者氏名Ｗ      TO 被保険者氏名.
      */ 郵便番号・電話番号追加 /42505
           IF (施術和暦年月ＷＲ >= 42505) AND (連入－電話印刷 = 1)
              IF (受－柔整郵便電話番号印刷 = 0 OR 2) AND
                 ((郵便番号１Ｗ NOT = SPACE) OR (郵便番号２Ｗ NOT = SPACE))
017280*           MOVE "〒"          TO 郵便
017260           MOVE 郵便番号１Ｗ  TO 郵便番号１
017270           MOVE 郵便番号２Ｗ  TO 郵便番号２
017280           MOVE "-"           TO 郵便番号区切
              END-IF
              IF 受－柔整郵便電話番号印刷 = 0 OR 3
017260           MOVE 電話番号Ｗ    TO 電話番号
              END-IF
           END-IF.
016870     MOVE 被保険者住所１Ｗ    TO 住所１.
016880     MOVE 被保険者住所２Ｗ    TO 住所２.
016890***     MOVE 患者カナＷ          TO 患者カナ.
016900     MOVE 患者氏名Ｗ          TO 患者氏名.
016910     MOVE 男チェックＷ        TO 男チェック.
016920     MOVE 女チェックＷ        TO 女チェック.
016930*     MOVE 性別Ｗ              TO 性別.
016940     MOVE 明治チェックＷ      TO 明治チェック.
016950     MOVE 大正チェックＷ      TO 大正チェック.
016960     MOVE 昭和チェックＷ      TO 昭和チェック.
016970     MOVE 平成チェックＷ      TO 平成チェック.
016980*     MOVE 元号Ｗ              TO 元号.
016990     MOVE 患者年Ｗ            TO 患者年.
017000     MOVE 患者月Ｗ            TO 患者月.
017010     MOVE 患者日Ｗ            TO 患者日.
017020*     MOVE NC"年"              TO 年.
017030*     MOVE NC"月"              TO 月.
017040*     MOVE NC"日"              TO 日.
017050*     MOVE 印刷続柄Ｗ          TO 続柄 続柄２.
017060*
017070***     MOVE １０割チェックＷ    TO １０割チェック.
017080***     MOVE ９割チェックＷ      TO ９割チェック.
017090***     MOVE ８割チェックＷ      TO ８割チェック.
017100***     MOVE ７割チェックＷ      TO ７割チェック.
017110*     MOVE 給付割合Ｗ          TO 給付割合.
017120*
017130     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
017140     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
017150     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
017150     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
017150     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
017150     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
017330*
017340********************
017350* 負傷データセット *
017360********************
017370* １部位 *
017380**********
017390     MOVE 負傷名Ｗ(1)       TO 負傷名１.
017400     MOVE 負傷年Ｗ(1)       TO 負傷年１.
017410     MOVE 負傷月Ｗ(1)       TO 負傷月１.
017420     MOVE 負傷日Ｗ(1)       TO 負傷日１.
017430     MOVE 初検年Ｗ(1)       TO 初検年１.
017440     MOVE 初検月Ｗ(1)       TO 初検月１.
017450     MOVE 初検日Ｗ(1)       TO 初検日１.
017460     MOVE 開始年Ｗ(1)       TO 開始年１.
017470     MOVE 開始月Ｗ(1)       TO 開始月１.
017480     MOVE 開始日Ｗ(1)       TO 開始日１.
017490     MOVE 終了年Ｗ(1)       TO 終了年１.
017500     MOVE 終了月Ｗ(1)       TO 終了月１.
017510     MOVE 終了日Ｗ(1)       TO 終了日１.
017520     MOVE 実日数Ｗ(1)       TO 実日数１.
017530     MOVE 治癒チェックＷ(1) TO 治癒チェック１.
017540     MOVE 中止チェックＷ(1) TO 中止チェック１.
017550     MOVE 転医チェックＷ(1) TO 転医チェック１.
017560**********
017570* ２部位 *
017580**********
017590     MOVE 負傷名Ｗ(2)       TO 負傷名２.
017600     MOVE 負傷年Ｗ(2)       TO 負傷年２.
017610     MOVE 負傷月Ｗ(2)       TO 負傷月２.
017620     MOVE 負傷日Ｗ(2)       TO 負傷日２.
017630     MOVE 初検年Ｗ(2)       TO 初検年２.
017640     MOVE 初検月Ｗ(2)       TO 初検月２.
017650     MOVE 初検日Ｗ(2)       TO 初検日２.
017660     MOVE 開始年Ｗ(2)       TO 開始年２.
017670     MOVE 開始月Ｗ(2)       TO 開始月２.
017680     MOVE 開始日Ｗ(2)       TO 開始日２.
017690     MOVE 終了年Ｗ(2)       TO 終了年２.
017700     MOVE 終了月Ｗ(2)       TO 終了月２.
017710     MOVE 終了日Ｗ(2)       TO 終了日２.
017720     MOVE 実日数Ｗ(2)       TO 実日数２.
017730     MOVE 治癒チェックＷ(2) TO 治癒チェック２.
017740     MOVE 中止チェックＷ(2) TO 中止チェック２.
017750     MOVE 転医チェックＷ(2) TO 転医チェック２.
017760**********
017770* ３部位 *
017780**********
017790     MOVE 負傷名Ｗ(3)       TO 負傷名３.
017800     MOVE 負傷年Ｗ(3)       TO 負傷年３.
017810     MOVE 負傷月Ｗ(3)       TO 負傷月３.
017820     MOVE 負傷日Ｗ(3)       TO 負傷日３.
017830     MOVE 初検年Ｗ(3)       TO 初検年３.
017840     MOVE 初検月Ｗ(3)       TO 初検月３.
017850     MOVE 初検日Ｗ(3)       TO 初検日３.
017860     MOVE 開始年Ｗ(3)       TO 開始年３.
017870     MOVE 開始月Ｗ(3)       TO 開始月３.
017880     MOVE 開始日Ｗ(3)       TO 開始日３.
017890     MOVE 終了年Ｗ(3)       TO 終了年３.
017900     MOVE 終了月Ｗ(3)       TO 終了月３.
017910     MOVE 終了日Ｗ(3)       TO 終了日３.
017920     MOVE 実日数Ｗ(3)       TO 実日数３.
017930     MOVE 治癒チェックＷ(3) TO 治癒チェック３.
017940     MOVE 中止チェックＷ(3) TO 中止チェック３.
017950     MOVE 転医チェックＷ(3) TO 転医チェック３.
017960**********
017970* ４部位 *
017980**********
017990     MOVE 負傷名Ｗ(4)       TO 負傷名４.
018000     MOVE 負傷年Ｗ(4)       TO 負傷年４.
018010     MOVE 負傷月Ｗ(4)       TO 負傷月４.
018020     MOVE 負傷日Ｗ(4)       TO 負傷日４.
018030     MOVE 初検年Ｗ(4)       TO 初検年４.
018040     MOVE 初検月Ｗ(4)       TO 初検月４.
018050     MOVE 初検日Ｗ(4)       TO 初検日４.
018060     MOVE 開始年Ｗ(4)       TO 開始年４.
018070     MOVE 開始月Ｗ(4)       TO 開始月４.
018080     MOVE 開始日Ｗ(4)       TO 開始日４.
018090     MOVE 終了年Ｗ(4)       TO 終了年４.
018100     MOVE 終了月Ｗ(4)       TO 終了月４.
018110     MOVE 終了日Ｗ(4)       TO 終了日４.
018120     MOVE 実日数Ｗ(4)       TO 実日数４.
018130     MOVE 治癒チェックＷ(4) TO 治癒チェック４.
018140     MOVE 中止チェックＷ(4) TO 中止チェック４.
018150     MOVE 転医チェックＷ(4) TO 転医チェック４.
018160**********
018170* ５部位 *
018180**********
018190     MOVE 負傷名Ｗ(5)       TO 負傷名５.
018200     MOVE 負傷年Ｗ(5)       TO 負傷年５.
018210     MOVE 負傷月Ｗ(5)       TO 負傷月５.
018220     MOVE 負傷日Ｗ(5)       TO 負傷日５.
018230     MOVE 初検年Ｗ(5)       TO 初検年５.
018240     MOVE 初検月Ｗ(5)       TO 初検月５.
018250     MOVE 初検日Ｗ(5)       TO 初検日５.
018260     MOVE 開始年Ｗ(5)       TO 開始年５.
018270     MOVE 開始月Ｗ(5)       TO 開始月５.
018280     MOVE 開始日Ｗ(5)       TO 開始日５.
018290     MOVE 終了年Ｗ(5)       TO 終了年５.
018300     MOVE 終了月Ｗ(5)       TO 終了月５.
018310     MOVE 終了日Ｗ(5)       TO 終了日５.
018320     MOVE 実日数Ｗ(5)       TO 実日数５.
018330     MOVE 治癒チェックＷ(5) TO 治癒チェック５.
018340     MOVE 中止チェックＷ(5) TO 中止チェック５.
018350     MOVE 転医チェックＷ(5) TO 転医チェック５.
018360**************
018370* 経過セット *
018380**************
018390***     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018400***             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
018410***             UNTIL ( 部位ＣＮＴ > 5 )
018420**         MOVE 部位ＣＮＴＷ(部位ＣＮＴ)   TO 経過部位ＣＮＴ(部位ＣＮＴ)
018430**         MOVE 部位区切Ｗ(部位ＣＮＴ)     TO 部位区切(部位ＣＮＴ)
018440***         MOVE 印刷経過略称Ｗ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
018450***     END-PERFORM.
018460*****************************************
018470*     新規・継続チェックについて        *
018480*   ●新規...初検有り ●継続...初検なし *
018490*****************************************
018500     MOVE 新規チェックＷ    TO 新規チェック.
018510     MOVE 継続チェックＷ    TO 継続チェック.
018520********************
018530* 料金データセット *
018540********************
018550*    ****************************************************************
018560*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
018570*    ****************************************************************
018580     MOVE 初検料ＷＲ                   TO  初検料.
018590     MOVE 相談料ＷＲ                   TO  初検時相談料.
018600     MOVE 時間外チェックＷ             TO  時間外チェック.
018610     MOVE 休日チェックＷ               TO  休日チェック.
018620     MOVE 深夜チェックＷ               TO  深夜チェック.
018630     MOVE 初検加算料ＷＲ               TO  初検加算料.
           IF (時間外チェックＷ NOT = SPACE) OR (深夜チェックＷ NOT = SPACE) OR
              (休日チェックＷ NOT = SPACE)
              MOVE 初検加算時Ｗ                 TO  初検加算時
              MOVE 初検加算区切Ｗ               TO  初検加算区切
              MOVE 初検加算分Ｗ                 TO  初検加算分
           END-IF.
018640     MOVE 再検料ＷＲ                   TO  再検料.
018650     MOVE 往療距離ＷＲ                 TO  往療距離.
018660     MOVE 往療回数ＷＲ                 TO  往療回数.
018670     MOVE 往療料ＷＲ                   TO  往療料.
018680     MOVE 夜間チェックＷ               TO  夜間チェック.
018690     MOVE 難路チェックＷ               TO  難路チェック.
018700     MOVE 暴風雨雪チェックＷ           TO  暴風雨雪チェック.
018710     MOVE 往療加算料ＷＲ               TO  往療加算料.
      */金属副子・運動後療の変更・追加/1805
           IF ( 施術和暦年月ＷＲ < 43006 )
018050        MOVE 大チェックＷ              TO  大チェック
018060        MOVE 中チェックＷ              TO  中チェック
018070        MOVE 小チェックＷ              TO  小チェック
           END-IF.
           IF ( 施術和暦年月ＷＲ >= 43006 ) AND ( 金属副子加算料ＷＲ NOT = ZERO )
              MOVE ALL NC"＝"                TO  金属訂正
      *        MOVE 金属回数Ｗ                TO  金属回数
      *        MOVE NC"回"                    TO  金属回
           END-IF.
018750     MOVE 金属副子加算料ＷＲ           TO  金属副子加算料.
018760     MOVE 施術情報提供料ＷＲ           TO  施術情報提供料.
018770     MOVE 小計Ｗ                       TO 小計.
018780********************
018790* 初回処置料セット *
018800********************
018810     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018820***             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
018830             UNTIL ( 部位ＣＮＴ > 5 )
018840        MOVE 初回処置料ＷＲ(部位ＣＮＴ) TO 初回処置料(部位ＣＮＴ)
018850     END-PERFORM.
018860     MOVE 初回処置料合計Ｗ             TO 初回処置料合計
018870********************
018880* 逓減毎料金セット *
018890********************
018900*    **********
018910*    * １部位 *
018920*    **********
018930     MOVE 後療単価１ＷＲ               TO 後療単価１.
018940     MOVE 後療回数１ＷＲ               TO 後療回数１.
018950     MOVE 後療料１ＷＲ                 TO 後療料１.
018960     MOVE 冷罨法回数１ＷＲ             TO 冷罨法回数１.
018970     MOVE 冷罨法料１ＷＲ               TO 冷罨法料１.
018980     MOVE 温罨法回数１ＷＲ             TO 温罨法回数１.
018990     MOVE 温罨法料１ＷＲ               TO 温罨法料１.
019000     MOVE 電療回数１ＷＲ               TO 電療回数１.
019010     MOVE 電療料１ＷＲ                 TO 電療料１.
019020     MOVE 小計１ＷＲ                   TO 小計１.
019030     IF ( 長期逓減率１ＷＲ NOT = ZERO )
019040        COMPUTE 長期逓減率１ = 長期逓減率１ＷＲ / 100
019050     END-IF.
019060     MOVE 長期込小計１ＷＲ             TO 長期込小計１.
019070*    **********
019080*    * ２部位 *
019090*    **********
019100     MOVE 後療単価２ＷＲ               TO 後療単価２.
019110     MOVE 後療回数２ＷＲ               TO 後療回数２.
019120     MOVE 後療料２ＷＲ                 TO 後療料２.
019130     MOVE 冷罨法回数２ＷＲ             TO 冷罨法回数２.
019140     MOVE 冷罨法料２ＷＲ               TO 冷罨法料２.
019150     MOVE 温罨法回数２ＷＲ             TO 温罨法回数２.
019160     MOVE 温罨法料２ＷＲ               TO 温罨法料２.
019170     MOVE 電療回数２ＷＲ               TO 電療回数２.
019180     MOVE 電療料２ＷＲ                 TO 電療料２.
019190     MOVE 小計２ＷＲ                   TO 小計２.
019200     IF ( 長期逓減率２ＷＲ NOT = ZERO )
019210        COMPUTE 長期逓減率２ = 長期逓減率２ＷＲ / 100
019220     END-IF.
019230     MOVE 長期込小計２ＷＲ             TO 長期込小計２.
019240*    ****************
019250*    * ３部位／８割 *
019260*    ****************
019270     MOVE 後療単価３８ＷＲ             TO 後療単価３８.
019280     MOVE 後療回数３８ＷＲ             TO 後療回数３８.
019290     MOVE 後療料３８ＷＲ               TO 後療料３８.
019300     MOVE 冷罨法回数３８ＷＲ           TO 冷罨法回数３８.
019310     MOVE 冷罨法料３８ＷＲ             TO 冷罨法料３８.
019320     MOVE 温罨法回数３８ＷＲ           TO 温罨法回数３８.
019330     MOVE 温罨法料３８ＷＲ             TO 温罨法料３８.
019340     MOVE 電療回数３８ＷＲ             TO 電療回数３８.
019350     MOVE 電療料３８ＷＲ               TO 電療料３８.
019360     MOVE 小計３８ＷＲ                 TO 小計３８.
019370     MOVE 多部位込小計３８ＷＲ         TO 多部位込小計３８.
019380     IF ( 長期逓減率３８ＷＲ NOT = ZERO )
019390        COMPUTE 長期逓減率３８ = 長期逓減率３８ＷＲ / 100
019400     END-IF.
019410     MOVE 長期込小計３８ＷＲ           TO 長期込小計３８.
      */ 逓減率 0.7→0.6 /42505  /*出さない /42610
      *     IF (施術和暦年月ＷＲ >= 42505)
      *        MOVE "60"                      TO 逓減３８
      *        MOVE "0.6"                     TO 多部位３８
      *        MOVE "==="                     TO 逓減訂正３８ 多部位訂正３８
      *     END-IF.
019420*    ****************
019430*    * ３部位／10割 *
019440*    ****************
019450     MOVE 逓減開始月３０ＷＲ           TO 逓減開始月３０.
019460     MOVE 逓減開始日３０ＷＲ           TO 逓減開始日３０.
019470     MOVE 後療単価３０ＷＲ             TO 後療単価３０.
019480     MOVE 後療回数３０ＷＲ             TO 後療回数３０.
019490     MOVE 後療料３０ＷＲ               TO 後療料３０.
019500     MOVE 冷罨法回数３０ＷＲ           TO 冷罨法回数３０.
019510     MOVE 冷罨法料３０ＷＲ             TO 冷罨法料３０.
019520     MOVE 温罨法回数３０ＷＲ           TO 温罨法回数３０.
019530     MOVE 温罨法料３０ＷＲ             TO 温罨法料３０.
019540     MOVE 電療回数３０ＷＲ             TO 電療回数３０.
019550     MOVE 電療料３０ＷＲ               TO 電療料３０.
019560     MOVE 小計３０ＷＲ                 TO 小計３０.
019570     IF ( 長期逓減率３０ＷＲ NOT = ZERO )
019580        COMPUTE 長期逓減率３０ = 長期逓減率３０ＷＲ / 100
019590     END-IF.
019600     MOVE 長期込小計３０ＷＲ           TO 長期込小計３０.
019610*    ****************
019620*    * ４部位／５割 *
019630*    ****************
019640     MOVE 後療単価４５ＷＲ             TO 後療単価４５.
019650     MOVE 後療回数４５ＷＲ             TO 後療回数４５.
019660     MOVE 後療料４５ＷＲ               TO 後療料４５.
019670     MOVE 冷罨法回数４５ＷＲ           TO 冷罨法回数４５.
019680     MOVE 冷罨法料４５ＷＲ             TO 冷罨法料４５.
019690     MOVE 温罨法回数４５ＷＲ           TO 温罨法回数４５.
019700     MOVE 温罨法料４５ＷＲ             TO 温罨法料４５.
019710     MOVE 電療回数４５ＷＲ             TO 電療回数４５.
019720     MOVE 電療料４５ＷＲ               TO 電療料４５.
019730     MOVE 小計４５ＷＲ                 TO 小計４５.
019740     MOVE 多部位込小計４５ＷＲ         TO 多部位込小計４５.
019750     IF ( 長期逓減率４５ＷＲ NOT = ZERO )
019760        COMPUTE 長期逓減率４５ = 長期逓減率４５ＷＲ / 100
019770     END-IF.
019780     MOVE 長期込小計４５ＷＲ           TO 長期込小計４５.
019790*    ****************
019800*    * ４部位／８割 *
019810*    ****************
019820     MOVE 逓減開始月４８ＷＲ           TO 逓減開始月４８.
019830     MOVE 逓減開始日４８ＷＲ           TO 逓減開始日４８.
019840     MOVE 後療単価４８ＷＲ             TO 後療単価４８.
019850     MOVE 後療回数４８ＷＲ             TO 後療回数４８.
019860     MOVE 後療料４８ＷＲ               TO 後療料４８.
019870     MOVE 冷罨法回数４８ＷＲ           TO 冷罨法回数４８.
019880     MOVE 冷罨法料４８ＷＲ             TO 冷罨法料４８.
019890     MOVE 温罨法回数４８ＷＲ           TO 温罨法回数４８.
019900     MOVE 温罨法料４８ＷＲ             TO 温罨法料４８.
019910     MOVE 電療回数４８ＷＲ             TO 電療回数４８.
019920     MOVE 電療料４８ＷＲ               TO 電療料４８.
019930     MOVE 小計４８ＷＲ                 TO 小計４８.
019940     MOVE 多部位込小計４８ＷＲ         TO 多部位込小計４８.
019950     IF ( 長期逓減率４８ＷＲ NOT = ZERO )
019960        COMPUTE 長期逓減率４８ = 長期逓減率４８ＷＲ / 100
019970     END-IF.
019980     MOVE 長期込小計４８ＷＲ           TO 長期込小計４８.
      */ 逓減率 0.7→0.6 /42505  /*出さない /42610
      *     IF (施術和暦年月ＷＲ >= 42505)
      *        MOVE "60"                      TO 逓減４８
      *        MOVE "0.6"                     TO 多部位４８
      *        MOVE "==="                     TO 逓減訂正４８ 多部位訂正４８
      *     END-IF.
019990*    ****************
020000*    * ４部位／10割 *
020010*    ****************
020020     MOVE 逓減開始月４０ＷＲ           TO 逓減開始月４０.
020030     MOVE 逓減開始日４０ＷＲ           TO 逓減開始日４０.
020040     MOVE 後療単価４０ＷＲ             TO 後療単価４０.
020050     MOVE 後療回数４０ＷＲ             TO 後療回数４０.
020060     MOVE 後療料４０ＷＲ               TO 後療料４０.
020070     MOVE 冷罨法回数４０ＷＲ           TO 冷罨法回数４０.
020080     MOVE 冷罨法料４０ＷＲ             TO 冷罨法料４０.
020090     MOVE 温罨法回数４０ＷＲ           TO 温罨法回数４０.
020100     MOVE 温罨法料４０ＷＲ             TO 温罨法料４０.
020110     MOVE 電療回数４０ＷＲ             TO 電療回数４０.
020120     MOVE 電療料４０ＷＲ               TO 電療料４０.
020130     MOVE 小計４０ＷＲ                 TO 小計４０.
020140     IF ( 長期逓減率４０ＷＲ NOT = ZERO )
020150        COMPUTE 長期逓減率４０ = 長期逓減率４０ＷＲ / 100
020160     END-IF.
020170     MOVE 長期込小計４０ＷＲ           TO 長期込小計４０.
020180*
020190*↓***********************************************************************
020200* ５部位／2.5割の印字は必要ない。
020210*------------------------------------------------------------------------*
020220*    *****************
020230*    * ５部位／2.5割 *
020240*    *****************
020250*     MOVE 後療単価５２ＷＲ             TO 後療単価５２.
020260*     MOVE 後療回数５２ＷＲ             TO 後療回数５２.
020270*     MOVE 後療料５２ＷＲ               TO 後療料５２.
020280*     MOVE 冷罨法回数５２ＷＲ           TO 冷罨法回数５２.
020290*     MOVE 冷罨法料５２ＷＲ             TO 冷罨法料５２.
020300*     MOVE 温罨法回数５２ＷＲ           TO 温罨法回数５２.
020310*     MOVE 温罨法料５２ＷＲ             TO 温罨法料５２.
020320*     MOVE 電療回数５２ＷＲ             TO 電療回数５２.
020330*     MOVE 電療料５２ＷＲ               TO 電療料５２.
020340*     MOVE 小計５２ＷＲ                 TO 小計５２.
020350*     MOVE 多部位込小計５２ＷＲ         TO 多部位込小計５２.
020360*     IF ( 長期逓減率５２ＷＲ NOT = ZERO )
020370*        COMPUTE 長期逓減率５２ = 長期逓減率５２ＷＲ / 100
020380*     END-IF.
020390*     MOVE 長期込小計５２ＷＲ           TO 長期込小計５２.
020400*↑***********************************************************************
020410*
020420*    ****************
020430*    * ５部位／５割 *
020440*    ****************
020450*     MOVE SPACE TO 部位５Ｗ.
020460*     IF ( 小計５５ＷＲ NOT = ZERO )
020470*       MOVE "5) 33 "                  TO 逓減固定５Ｗ
020480*       MOVE "0.33"                    TO 多部位率５Ｗ
020490*       MOVE 逓減開始月５５ＷＲ        TO 逓減開始月５Ｗ
020500*       MOVE 逓減開始日５５ＷＲ        TO 逓減開始日５Ｗ
020510*       MOVE 後療単価５５ＷＲ          TO 後療単価５Ｗ
020520*       MOVE 後療回数５５ＷＲ          TO 後療回数５Ｗ
020530*       MOVE 後療料５５ＷＲ            TO 後療料５Ｗ
020540*       MOVE 冷罨法回数５５ＷＲ        TO 冷罨法回数５Ｗ
020550*       MOVE 冷罨法料５５ＷＲ          TO 冷罨法料５Ｗ
020560*       MOVE 温罨法回数５５ＷＲ        TO 温罨法回数５Ｗ
020570*       MOVE 温罨法料５５ＷＲ          TO 温罨法料５Ｗ
020580*       MOVE 電療回数５５ＷＲ          TO 電療回数５Ｗ
020590*       MOVE 電療料５５ＷＲ            TO 電療料５Ｗ
020600*       MOVE 小計５５ＷＲ              TO 小計５Ｗ
020610*       MOVE 多部位込小計５５ＷＲ      TO 多部位込小計５Ｗ
020620*       IF ( 長期逓減率５５ＷＲ NOT = ZERO )
020630*          COMPUTE 長期逓減率５Ｗ = 長期逓減率５５ＷＲ / 100
020640*       END-IF
020650*       MOVE 長期込小計５５ＷＲ        TO 長期込小計５Ｗ
020660**------------------------------------------------------------------------------------*
020760**       MOVE 部位５Ｗ                  TO 部位５５
020770*     END-IF.
020780*    ****************
020790*    * ５部位／８割 *
020800*    ****************
020810     MOVE SPACE TO 部位５Ｗ.
020820     IF ( 小計５８ＷＲ NOT = ZERO )
      */日付
021560         MOVE 逓減開始月５８ＷＲ           TO 逓減開始月５Ｗ
               MOVE "月"                         TO 月ＣＭ
021570         MOVE 逓減開始日５８ＷＲ           TO 逓減開始日５Ｗ
               MOVE "日"                         TO 日ＣＭ
               MOVE "("                          TO 括弧１Ｗ
      */後療料
               IF 後療料５８ＷＲ NOT = ZERO
                   MOVE "("                      TO 括弧２Ｗ
021580             MOVE 後療単価５８ＷＲ         TO 後療単価５Ｗ
                   MOVE "x"                      TO 乗算記号１Ｗ
021590             MOVE 後療回数５８ＷＲ         TO 後療回数５Ｗ
                   MOVE "="                      TO イコール１Ｗ
021600             MOVE 後療料５８ＷＲ           TO 後療料５Ｗ
                   MOVE ")"                      TO 括弧３Ｗ
               END-IF
      */冷罨法
               IF 冷罨法料５８ＷＲ NOT = ZERO
                   MOVE "+"                      TO 加算記号１Ｗ
                   MOVE "("                      TO 括弧４Ｗ
                   COMPUTE 冷罨法単価５Ｗ        =  冷罨法料５８ＷＲ / 冷罨法回数５８ＷＲ
                   MOVE "x"                      TO 乗算記号２Ｗ
021610             MOVE 冷罨法回数５８ＷＲ       TO 冷罨法回数５Ｗ
                   MOVE "="                      TO イコール２Ｗ
021620             MOVE 冷罨法料５８ＷＲ         TO 冷罨法料５Ｗ
                   MOVE ")"                      TO 括弧５Ｗ
               END-IF
      */温罨法
               IF 温罨法料５８ＷＲ NOT = ZERO
                   MOVE "+"                      TO 加算記号２Ｗ
                   MOVE "("                      TO 括弧６Ｗ
                   COMPUTE 温罨法単価５Ｗ        =  温罨法料５８ＷＲ / 温罨法回数５８ＷＲ
                   MOVE "x"                      TO 乗算記号３Ｗ
021630             MOVE 温罨法回数５８ＷＲ       TO 温罨法回数５Ｗ
                   MOVE "="                      TO イコール３Ｗ
021640             MOVE 温罨法料５８ＷＲ         TO 温罨法料５Ｗ
                   MOVE ")"                      TO 括弧７Ｗ
               END-IF
      */電療料
               IF 電療料５８ＷＲ NOT = ZERO
                   MOVE "+"                      TO 加算記号３Ｗ
                   MOVE "("                      TO 括弧８Ｗ
                   COMPUTE 電療単価５Ｗ          =  電療料５８ＷＲ / 電療回数５８ＷＲ
                   MOVE "x"                      TO 乗算記号４Ｗ
021650             MOVE 電療回数５８ＷＲ         TO 電療回数５Ｗ
                   MOVE "="                      TO イコール４Ｗ
021660             MOVE 電療料５８ＷＲ           TO 電療料５Ｗ
                   MOVE ")"                      TO 括弧９Ｗ
               END-IF
      *
               MOVE ")"                          TO 括弧１０Ｗ
      */多部位
               MOVE "x"                          TO 乗算記号５Ｗ
      */ 逓減率 0.7→0.6 /42505
              IF (施術和暦年月ＷＲ >= 42505)
021290           MOVE "0.6 "                    TO 多部位率５Ｗ
              ELSE
021290           MOVE "0.7 "                    TO 多部位率５Ｗ
              END-IF
      */長期
021680         IF 長期逓減率５８ＷＲ NOT = ZERO
                  MOVE "x"                       TO 乗算記号６Ｗ
021690            COMPUTE 長期逓減率５Ｗ = 長期逓減率５８ＷＲ / 100
021700         END-IF
      */合計
               MOVE "="                          TO イコール５Ｗ
021710         MOVE 長期込小計５８ＷＲ           TO 長期込小計５Ｗ
021020         MOVE 部位５Ｗ                  TO 部位５８
021030     END-IF.
021040*    ****************
021050*    * ５部位／10割 *
021060*    ****************
021070     MOVE SPACE TO 部位５Ｗ.
021080     IF ( 小計５０ＷＲ NOT = ZERO )
      */日付
021560         MOVE 逓減開始月５０ＷＲ           TO 逓減開始月５Ｗ
               MOVE "月"                         TO 月ＣＭ
021570         MOVE 逓減開始日５０ＷＲ           TO 逓減開始日５Ｗ
               MOVE "日"                         TO 日ＣＭ
               MOVE "("                          TO 括弧１Ｗ
      */後療料
               IF 後療料５０ＷＲ NOT = ZERO
                   MOVE "("                      TO 括弧２Ｗ
021580             MOVE 後療単価５０ＷＲ         TO 後療単価５Ｗ
                   MOVE "x"                      TO 乗算記号１Ｗ
021590             MOVE 後療回数５０ＷＲ         TO 後療回数５Ｗ
                   MOVE "="                      TO イコール１Ｗ
021600             MOVE 後療料５０ＷＲ           TO 後療料５Ｗ
                   MOVE ")"                      TO 括弧３Ｗ
               END-IF
      */冷罨法
               IF 冷罨法料５０ＷＲ NOT = ZERO
                   MOVE "+"                      TO 加算記号１Ｗ
                   MOVE "("                      TO 括弧４Ｗ
                   COMPUTE 冷罨法単価５Ｗ        =  冷罨法料５０ＷＲ / 冷罨法回数５０ＷＲ
                   MOVE "x"                      TO 乗算記号２Ｗ
021610             MOVE 冷罨法回数５０ＷＲ       TO 冷罨法回数５Ｗ
                   MOVE "="                      TO イコール２Ｗ
021620             MOVE 冷罨法料５０ＷＲ         TO 冷罨法料５Ｗ
                   MOVE ")"                      TO 括弧５Ｗ
               END-IF
      */温罨法
               IF 温罨法料５０ＷＲ NOT = ZERO
                   MOVE "+"                      TO 加算記号２Ｗ
                   MOVE "("                      TO 括弧６Ｗ
                   COMPUTE 温罨法単価５Ｗ        =  温罨法料５０ＷＲ / 温罨法回数５０ＷＲ
                   MOVE "x"                      TO 乗算記号３Ｗ
021630             MOVE 温罨法回数５０ＷＲ       TO 温罨法回数５Ｗ
                   MOVE "="                      TO イコール３Ｗ
021640             MOVE 温罨法料５０ＷＲ         TO 温罨法料５Ｗ
                   MOVE ")"                      TO 括弧７Ｗ
               END-IF
      */電療料
               IF 電療料５０ＷＲ NOT = ZERO
                   MOVE "+"                      TO 加算記号３Ｗ
                   MOVE "("                      TO 括弧８Ｗ
                   COMPUTE 電療単価５Ｗ          =  電療料５０ＷＲ / 電療回数５０ＷＲ
                   MOVE "x"                      TO 乗算記号４Ｗ
021650             MOVE 電療回数５０ＷＲ         TO 電療回数５Ｗ
                   MOVE "="                      TO イコール４Ｗ
021660             MOVE 電療料５０ＷＲ           TO 電療料５Ｗ
                   MOVE ")"                      TO 括弧９Ｗ
               END-IF
      *
               MOVE ")"                          TO 括弧１０Ｗ
      */多部位
      *        乗算記号５Ｗ 多部位率５Ｗ
      */長期
021680         IF 長期逓減率５０ＷＲ NOT = ZERO
                  MOVE "x"                       TO 乗算記号６Ｗ
021690            COMPUTE 長期逓減率５Ｗ = 長期逓減率５０ＷＲ / 100
021700         END-IF
      */合計
               MOVE "="                          TO イコール５Ｗ
021710         MOVE 長期込小計５０ＷＲ           TO 長期込小計５Ｗ
021260         MOVE 部位５Ｗ                  TO 部位５０
021270     END-IF.
021280*
021290     MOVE 適用１Ｗ                     TO 適用１.
021300     MOVE 適用２Ｗ                     TO 適用２.
      *
      */金属副子・運動後療の変更・追加/1805
           IF ( 施術和暦年月ＷＲ >= 43006 )
              INITIALIZE 連金運－キー
019550        MOVE 施術和暦ＷＲ TO 連金運－施術和暦
019560        MOVE 施術年ＷＲ   TO 連金運－施術年
019570        MOVE 施術月ＷＲ   TO 連金運－施術月
019580        MOVE 患者番号ＷＲ TO 連金運－患者番号
019590        MOVE 枝番ＷＲ     TO 連金運－枝番
              MOVE 保険種別ＷＲ TO 連金運－保険種別
              MOVE 27           TO 連金運－会コード
              MOVE ZERO         TO 連金運－用紙種別
              CALL "KINUNRYO"
              CANCEL "KINUNRYO"
              MOVE 連金運－金属副子ＣＭ           TO 金属副子ＣＭ
              MOVE 連金運－運動後療ＣＭ           TO 運動後療ＣＭ
              IF ( 金属副子加算料ＷＲ NOT = ZERO )
                 MOVE 金属副子ＣＭ                TO 金属副子
              END-IF
              IF ( 運動料Ｗ NOT = ZERO )
                 MOVE 運動後療ＣＭ                TO 運動後療
              END-IF
           END-IF.
021310     MOVE レセ－合計                   TO 合計.
021320     MOVE レセ－一部負担金             TO 一部負担金.
021330     MOVE レセ－請求金額               TO 請求金額.
021340*
021350*------------------------------------------------------------------------------------*
021360* 特別（助成レセなしで、本体レセにまとめる時、金額は助成込み・適用２に助成種別印字）
021370     IF ( 助成レセまとめフラグ = "YES" )
021390        MOVE レセ－合計                TO 合計
021400        MOVE レセ－受給者負担額        TO 一部負担金
021410*     / 引き算する/
021420        COMPUTE 請求金額 = レセ－合計 - レセ－受給者負担額
      */湯河原町の母子は負担額助成を記載する/130418
      */座間市の障害は負担額助成を記載する/
      */横浜川崎の乳児は負担額助成を記載する/170217
               IF ((受－助成種別 = 52) AND (受－費用負担者番号助成 = "85140630")) OR
                  ((受－助成種別 = 53) AND (受－費用負担者番号助成 = "80140171")) OR
                  ((受－助成種別 = 55) AND (受－費用負担者番号助成(1:5) = "81144" OR "81145"))
019830             MOVE レセ－一部負担金 TO 一部負担金
019840             MOVE レセ－請求金額   TO 請求金額
               END-IF
021430*
021440***        MOVE NC"○"  TO １０割チェック
021450***        MOVE SPACE   TO ９割チェック ８割チェック ７割チェック
021460*        MOVE 10      TO 給付割合
021470*
021480*/深＿夜の空白にストリングしてしまうためNOT SPACEの時は最後に転記する。
021490*/初険加算が３回の時は余白無く転記される。
021500        IF 助成種別略称Ｗ NOT = SPACE
021510           IF ( 適用２Ｗ NOT = SPACE )
021520              MOVE SPACE TO 助成種別略称Ｗ２
021530              STRING NC"※"             DELIMITED BY SIZE
021540                     助成種別略称Ｗ     DELIMITED BY SPACE
021550                     INTO 助成種別略称Ｗ２
021560              END-STRING
021570              MOVE 助成種別略称Ｗ２       TO 適用２(35:4)
021580           ELSE
021590              STRING 適用２Ｗ           DELIMITED BY SPACE
021600                     NC"※"             DELIMITED BY SIZE
021610                     助成種別略称Ｗ     DELIMITED BY SPACE
021620                     INTO 適用２
021630              END-STRING
021640           END-IF
021650        END-IF
021660     END-IF.
021860*
021870**********************
021880* 施術所データセット *
021890**********************
           MOVE 都道府県ＪＩＳＷ       TO 都道府県番号.
021900     MOVE 柔整師番号Ｗ           TO 柔整師番号.
021910*     MOVE 定額制受理番号Ｗ       TO 定額制受理番号.
021920     MOVE 施術所郵便番号１Ｗ     TO 施術所郵便番号１.
021930     MOVE 施術所郵便番号２Ｗ     TO 施術所郵便番号２.
021940*     MOVE 施術所住所Ｗ           TO 施術所住所１.
021950     MOVE 施術所住所１Ｗ         TO 施術所住所１.
021960     MOVE 施術所住所２Ｗ         TO 施術所住所２.
      */平成２７年１０月施術分より会員番号を印刷/150922
021970     MOVE 接骨師会会員番号Ｗ     TO 接骨師会会員番号.
021980     MOVE 接骨院名Ｗ             TO 接骨院名.
021990     MOVE 代表者カナＷ           TO 代表者カナ.
022000     MOVE 代表者名Ｗ             TO 代表者名.
022010     MOVE 施術所電話番号Ｗ       TO 施術所電話番号.
022020*
022030* / 柔整師・患者委任日 /
022040     MOVE 柔整師年Ｗ             TO 受理年.
022050     MOVE 柔整師月Ｗ             TO 受理月.
022060     MOVE 柔整師日Ｗ             TO 受理日.
022070* ( 委任年月日 印刷するか )
022080     IF ( 連入－委任印刷  = ZERO )
022090        MOVE 患者委任年Ｗ        TO 委任年
022100        MOVE 患者委任月Ｗ        TO 委任月
022110        MOVE 患者委任日Ｗ        TO 委任日
022120     END-IF.
022130*
022140***     MOVE コメント１Ｗ           TO コメント１.
022150***     MOVE コメント２Ｗ           TO コメント２.
022160***     MOVE コメント３Ｗ           TO コメント３.
022170***     MOVE コメント４Ｗ           TO コメント４.
022180***     MOVE コメント５Ｗ           TO コメント５.
022190***     MOVE コメント６Ｗ           TO コメント６.
022200***     MOVE コメント７Ｗ           TO コメント７.
022210*
022220***     MOVE 銀行名支店名Ｗ         TO 銀行名支店名.
022230***     MOVE 預金種別コメントＷ     TO 預金種別.
022240***     MOVE 口座番号Ｗ             TO 口座番号.
022250***     MOVE 口座名義人カナＷ       TO 口座名義人カナ.
022260***     MOVE 口座名義人Ｗ           TO 口座名義人.
             MOVE NC"○"                  TO 振込チェック 普通チェック.
022270*
022280* 最下欄に患者コード
022290***     MOVE 患者番号ＷＲ           TO 患者番号.
022300***     MOVE 枝番ＷＲ               TO 枝番.
022310*
022750* レセプト並び順セット *
022760     MOVE 順番Ｗ                 TO 順番.
022770*
022320*-------------------------------------------------------------------------*
022330*--- ※ レセ摘要再セットは、この印刷セットSECTION の最後にやること！ -----*
022340     PERFORM レセ摘要再セット.
022350*-------------------------------------------------------------------------*
022360*
022370*     PERFORM テスト印字処理.
022380*
022390*=== 印刷セット =================================================*
022400*================================================================*
022410 項目初期化 SECTION.
022420*================================================================*
022430     INITIALIZE 施術所情報Ｗ.
022440     INITIALIZE 受診者情報Ｗ.
022450     INITIALIZE 負傷情報Ｗ.
022460     INITIALIZE 料金情報Ｗ.
022470     INITIALIZE 備考情報Ｗ.
022480     INITIALIZE 料金１ＷＲ.
022490     INITIALIZE 料金２ＷＲ.
022500     INITIALIZE 料金３ＷＲ.
022510     MOVE SPACE TO YCB6125P.
022520*****     INITIALIZE YCB6125P.
022530*
021920*================================================================*
021930 基本情報取得 SECTION.
021920*================================================================*
023130*
           EVALUATE 公費種別ＷＲ
           WHEN 05
               MOVE 2          TO レセ－レセ種別
           WHEN OTHER
               MOVE 1          TO レセ－レセ種別
           END-EVALUATE.
019550     MOVE 施術和暦ＷＲ   TO レセ－施術和暦.
019560     MOVE 施術年ＷＲ     TO レセ－施術年.
019570     MOVE 施術月ＷＲ     TO レセ－施術月.
019580     MOVE 患者番号ＷＲ   TO レセ－患者番号.
019590     MOVE 枝番ＷＲ       TO レセ－枝番.
019600     READ レセプトＦ
019630     INVALID KEY
              MOVE SPACE     TO レセ－レコード
              INITIALIZE        レセ－レコード
           END-READ.
      *
028780     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
028790     MOVE 施術年ＷＲ         TO 受－施術年.
028800     MOVE 施術月ＷＲ         TO 受－施術月.
028810     MOVE 患者コードＷＲ     TO 受－患者コード.
028820     READ 受診者情報Ｆ
019630     INVALID KEY
              MOVE SPACE     TO 受－レコード
              INITIALIZE        受－レコード
           END-READ.
      *
027790     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
027800     MOVE 施術年ＷＲ         TO 負－施術年.
027810     MOVE 施術月ＷＲ         TO 負－施術月.
027820     MOVE 患者コードＷＲ     TO 負－患者コード.
027830     READ 負傷データＦ
019630     INVALID KEY
              MOVE SPACE     TO 負－レコード
              INITIALIZE        負－レコード
027870     NOT INVALID KEY
027900         MOVE 負－部位数                   TO 部位数Ｗ
           END-READ.
021940*
022540*================================================================*
022550 施術所情報取得 SECTION.
022560*================================================================*
022570**************************************************
022580* 本院データを使用し、以下の情報を取得           *
022590* ● 柔整師番号.. 柔整師番号Ｗに格納             *
022600* ● 会員番号 ... 接骨師会会員番号Ｗに格納       *
022610* ● 代表者名 ... 代表者名Ｗに格納               *
022620* ● 住所1,2   ...施術所住所1,2Ｗに格納          *
022630* ● 電話番号 ... 施術所電話番号Ｗに格納         *
022640**************************************************
022650     MOVE ZERO  TO 施情－施術所番号.
022660     READ 施術所情報マスタ
022670     INVALID KEY
022680         CONTINUE
022690     NOT INVALID KEY
022700*
               MOVE 施情－都道府県ＪＩＳ    TO 都道府県ＪＩＳＷ
022740         MOVE 施情－新柔整師番号      TO 柔整師番号Ｗ
022760*
022770         MOVE 施情－接骨師会会員番号  TO 接骨師会会員番号Ｗ
022780         MOVE 施情－郵便番号１        TO 施術所郵便番号１Ｗ
022790         MOVE 施情－郵便番号２        TO 施術所郵便番号２Ｗ
022800         MOVE 施情－接骨院名          TO 接骨院名Ｗ
022810         MOVE 施情－代表者カナ        TO 代表者カナＷ
022820         MOVE 施情－代表者名          TO 代表者名Ｗ
022830*
022840*         STRING 施情－住所１  DELIMITED BY SPACE
022850*                施情－住所２  DELIMITED BY SPACE
022860*           INTO 施術所住所Ｗ
022870*         END-STRING
022880         MOVE 施情－住所１            TO 施術所住所１Ｗ
022890         MOVE 施情－住所２            TO 施術所住所２Ｗ
022900         MOVE 施情－電話番号          TO 施術所電話番号Ｗ
022910* 振込先情報
022920         MOVE 施情－取引先銀行名      TO 取引先銀行名Ｗ
022930         MOVE 施情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
022940         MOVE 施情－預金種別          TO 預金種別Ｗ
022950         MOVE 施情－銀行番号          TO 銀行番号Ｗ
022960         MOVE 施情－店番号            TO 店番号Ｗ
022970         MOVE 施情－口座番号          TO 口座番号Ｗ
022980         MOVE 施情－口座名義人        TO 口座名義人Ｗ
022990         MOVE 施情－口座名義人カナ    TO 口座名義人カナＷ
023000****         MOVE 施情－接骨師会会長名    TO 接骨師会会長名Ｗ
023010     END-READ.
023020*
023030     PERFORM 会長委任判定.
023040*
023050     IF ( 会長委任フラグ = "YES" )
023520        MOVE ZERO  TO  会情－柔整鍼灸区分
023060        MOVE 27    TO  会情－協会コード
023070        MOVE ZERO  TO  会情－保険種別
023530        MOVE ZERO  TO  会情－変更和暦年月
023090        READ 会情報マスタ
023100        NOT INVALID KEY
023110            MOVE 会情－取引先銀行名      TO 取引先銀行名Ｗ
023120            MOVE 会情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
023130            MOVE 会情－預金種別          TO 預金種別Ｗ
023140            MOVE 会情－銀行番号          TO 銀行番号Ｗ
023150            MOVE 会情－店番号            TO 店番号Ｗ
023160            MOVE 会情－口座番号          TO 口座番号Ｗ
023170            MOVE 会情－口座名義人カナ    TO 口座名義人カナＷ
023180            MOVE 会情－口座名義人        TO 口座名義人Ｗ
023190            MOVE 会情－接骨師会会長名    TO 接骨師会会長名Ｗ
023200        END-READ
023210     END-IF.
023220*
023230* 振込先情報
023240     STRING 取引先銀行名Ｗ     DELIMITED BY SPACE
023250            "  "               DELIMITED BY SIZE
023260            取引先銀行支店名Ｗ DELIMITED BY SPACE
023270            INTO 銀行名支店名Ｗ
023280     END-STRING.
023290     EVALUATE 預金種別Ｗ
023300     WHEN 1
023310         MOVE "普通" TO 預金種別名称Ｗ
023320     WHEN 2
023330         MOVE "当座" TO 預金種別名称Ｗ
023340     WHEN OTHER
023350         MOVE SPACE  TO 預金種別名称Ｗ
023360     END-EVALUATE.
023370     IF ( 会長委任フラグ = "YES" )
023380        IF ( 預金種別名称Ｗ NOT = SPACE )
023390           STRING 預金種別名称Ｗ DELIMITED BY SPACE
023400                  "預金"         DELIMITED BY SIZE
023410                  INTO 預金種別コメントＷ
023420           END-STRING
023430        END-IF
023440     ELSE
023450        STRING 銀行番号Ｗ     DELIMITED BY SPACE
023460               " "            DELIMITED BY SIZE
023470               店番号Ｗ       DELIMITED BY SPACE
023480               " "            DELIMITED BY SIZE
023490               預金種別名称Ｗ DELIMITED BY SPACE
023500               INTO 預金種別コメントＷ
023510        END-STRING
023520     END-IF.
023530*
023540* コメント印字
023550     MOVE SPACE TO コメントＷ.
023560     INITIALIZE    コメントＷ.
023570*
023580     IF ( 会長委任フラグ = "YES" )
023590        MOVE "私が取得した上記金額の受領権を" TO コメント１Ｗ
023600        STRING "中部柔整師協会"     DELIMITED BY SIZE
023610               " 会長 "             DELIMITED BY SIZE
023620               接骨師会会長名Ｗ     DELIMITED BY SIZE
023630               INTO コメント２Ｗ
023640        END-STRING
023650        MOVE "に再委任します。"     TO コメント３Ｗ
023660        PERFORM 日付編集
023670        MOVE 日付編集Ｗ             TO コメント４Ｗ
023680        MOVE "柔道整復師"           TO コメント５Ｗ
023690        STRING "(氏名) "            DELIMITED BY SIZE
023700               代表者名Ｗ           DELIMITED BY SIZE
023710               "      (印)"         DELIMITED BY SIZE
023720               INTO コメント６Ｗ
023730        END-STRING
023740        MOVE "(住所) 施術証明書と同じ" TO コメント７Ｗ
023750     ELSE
023760        MOVE "【 備考 】"              TO コメント１Ｗ
023770     END-IF.
023780*
023790*================================================================*
023800 会長委任判定 SECTION.
023810*
023820**************************************************************************
023830*  社保・日雇・船員・組合・共済・自衛官は、すべて会長委任
023840*  国保は、国保組合の全国組織(全国土木 133033、中央建設 133264 )が会長委任
023850***************************************************************************
023860     MOVE  SPACE  TO  会長委任フラグ.
023870*
023880     IF ( 公費種別ＷＲ = 05 )
023890        CONTINUE
023900     ELSE
023910        EVALUATE 保険種別ＷＲ
023920        WHEN 02
023930        WHEN 06
023940        WHEN 07
023950        WHEN 03
023960        WHEN 04
023970        WHEN 09
023980            MOVE  "YES"  TO  会長委任フラグ
023990        WHEN 01
024000            IF ( 保険者番号ＷＲ(1:6) = "133033" ) OR
024010               ( 保険者番号ＷＲ = "133264" )
024020               MOVE  "YES"  TO  会長委任フラグ
024030            END-IF
024040        WHEN 08
024050            CONTINUE
024060        WHEN OTHER
024070            CONTINUE
024080        END-EVALUATE
024090     END-IF.
024100*/ すべて会長委任に変更 /0710
024110     MOVE  "YES"  TO  会長委任フラグ.
024120*
024130*================================================================*
024140 日付編集 SECTION.
024150*
024160     MOVE 施術和暦ＷＲ TO 元－元号区分.
024170     READ 元号マスタ
024180     INVALID KEY
024190         MOVE SPACE TO 元－レコード
024200         INITIALIZE    元－レコード
024210     NOT INVALID KEY
024220         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
024230     END-READ.
024240     IF ( 施術西暦年Ｗ NOT = ZERO )
024250        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
024260     END-IF.
024270*
024280     EVALUATE 施術月ＷＲ
024290     WHEN 4
024300     WHEN 6
024310     WHEN 9
024320     WHEN 11
024330         MOVE 30   TO 月末日Ｗ
024340     WHEN 2
024350         DIVIDE 4 INTO 施術西暦年Ｗ GIVING    商Ｗ
024360                                    REMAINDER 余Ｗ
024370         END-DIVIDE
024380         IF ( 余Ｗ = ZERO )
024390            MOVE 29 TO 月末日Ｗ
024400         ELSE
024410            MOVE 28 TO 月末日Ｗ
024420         END-IF
024430     WHEN 1
024440     WHEN 3
024450     WHEN 5
024460     WHEN 7
024470     WHEN 8
024480     WHEN 10
024490     WHEN 12
024500         MOVE 31   TO 月末日Ｗ
024510     WHEN OTHER
024520         MOVE ZERO TO 月末日Ｗ
024530     END-EVALUATE.
024540*
024550     MOVE 元－元号名称 TO 元号編集Ｗ.
024560     MOVE 施術年ＷＲ   TO 年編集Ｗ.
024570     MOVE 施術月ＷＲ   TO 月編集Ｗ.
024580     MOVE 月末日Ｗ     TO 日編集Ｗ.
024590*
024600*================================================================*
024610 請求先情報取得 SECTION.
024620*================================================================*
024630****************************************************
024640* 連結データから保険者マスタより請求先を取得する。 *
024660* ● 請求先...... 請求先名称Ｗに格納               *
024670****************************************************
024680     MOVE 保険種別ＷＲ   TO 保－保険種別.
024690     MOVE 保険者番号ＷＲ TO 保－保険者番号.
024700     READ 保険者マスタ
024710     INVALID KEY
024720         IF 保険種別ＷＲ = 05
024730             MOVE 保険種別ＷＲ       TO 市－公費種別
024740             MOVE 費用負担者番号ＷＲ TO 市－市町村番号
024750             READ 市町村マスタ
024760             INVALID KEY
024770                 MOVE SPACE          TO 請求先名称Ｗ
024780             NOT INVALID KEY
024790                 MOVE 市－市町村名称 TO 請求先名称Ｗ
024800             END-READ
024810         ELSE
024820             MOVE SPACE          TO 請求先名称Ｗ
024830         END-IF
024840     NOT INVALID KEY
024850** 組合・共済は支部名まで印字
024860         EVALUATE 保険種別ＷＲ
024870         WHEN 1
024880         WHEN 8
024890             MOVE 保－保険者名称      TO 請求先名称Ｗ
024900***             STRING 保－保険者名称    DELIMITED BY SPACE
024910***                    "長"              DELIMITED BY SIZE
024920***                    INTO 請求先名称Ｗ
024930***             END-STRING
024940         WHEN 2
024950         WHEN 6
024960             IF ( 保－接尾語区分 = 1 )
024970                MOVE 保－保険者名称    TO 請求先名称Ｗ
024980             ELSE
024990                STRING 保－保険者名称    DELIMITED BY SPACE
025000                       "社会保険事務所"  DELIMITED BY SIZE
025010                       INTO 請求先名称Ｗ
025020                END-STRING
025030             END-IF
025040         WHEN 3
025050             STRING 保－保険者名称    DELIMITED BY SPACE
025060                    "健康保険組合"    DELIMITED BY SIZE
025070                     保－支部部署名    DELIMITED BY SPACE
025080                     INTO 請求先名称Ｗ
025090             END-STRING
025100         WHEN 4
025110             STRING 保－保険者名称    DELIMITED BY SPACE
025120                    "共済組合"        DELIMITED BY SIZE
025130                    保－支部部署名    DELIMITED BY SPACE
025140                    INTO 請求先名称Ｗ
025150             END-STRING
025160         WHEN OTHER
025170             MOVE 保－保険者名称      TO 請求先名称Ｗ
025180         END-EVALUATE
025190     END-READ.
025200*
025210*================================================================*
025220 受診者情報取得 SECTION.
025230*================================================================*
025240**************************************************
025250* 連結データから受診者情報Ｆより以下の情報を取得 *
025260* ● 施術年 ..... 施術年Ｗに格納                 *
025270* ● 施術月 ..... 施術月Ｗに格納                 *
025280* ● 患者番号.... 患者番号Ｗに格納※ＦＤ連番用   *
025290* ● 記号 ....... 記号Ｗに格納                   *
025300* ● 番号 ....... 番号Ｗに格納                   *
025310* ● 保険者番号 . 保険者番号Ｗに格納             *
025320* ● 保険種別 ... 保険種別Ｗに格納               *
025330* ● 被保険者カナ.被保険者カナＷに格納           *
025340* ● 被保険者氏名.被保険者氏名Ｗに格納           *
025350* ● 住所１ ......被保険者住所１Ｗに格納         *
025360* ● 住所２ ......被保険者住所２Ｗに格納         *
025370* ● 患者カナ ....患者カナＷに格納               *
025380* ● 患者氏名 ....患者氏名Ｗに格納               *
025390* ● 患者性別 ....区分によりチェックに"○"を格納 *
025400* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
025410* ● 患者年 ......患者年Ｗに格納                 *
025420* ● 患者月 ......患者月Ｗに格納                 *
025430* ● 患者日 ......患者日Ｗに格納                 *
025440* ● 続柄 ........名称マスタより続柄Ｗに取得     *
025450**************************************************
           IF 受－レコード NOT = SPACE
      */被災者対応/110811
               IF 受－資格証明区分 = 9
                   MOVE NC"災"       TO 災Ｗ
                   MOVE NC"○"       TO 災丸印Ｗ
               END-IF
022660         EVALUATE 受－保険種別
022670         WHEN 01
022690            MOVE NC"○"        TO 国保チェックＷ
022700         WHEN 02
022710         WHEN 06
022750         WHEN 07
022720            MOVE NC"○"        TO 社保チェックＷ
022730         WHEN 03
022740            MOVE NC"○"        TO 組合チェックＷ
022750*         WHEN 07
022760*            MOVE NC"○"        TO 船員チェックＷ
               WHEN 04
               WHEN 09
                  MOVE NC"○"        TO 共済チェックＷ
      *         WHEN 09
      *            MOVE NC"○"        TO 自チェックＷ
               WHEN 08
                  MOVE NC"○"        TO 退職チェックＷ
               WHEN 05
                  MOVE NC"○"        TO 後期チェックＷ
022770         END-EVALUATE
      */全て単独
      *         IF 受－助成種別 = ZERO
                   MOVE NC"○" TO 単独チェックＷ
      *         ELSE
      *             MOVE NC"○" TO ２併チェックＷ
      *         END-IF
      */本家区分はどれか１つに○をする。
               IF 受－保険種別 = 05
                   EVALUATE 受－特別区分
                   WHEN 1
                   WHEN 2
                       MOVE NC"○" TO 高一チェックＷ
                   WHEN 3
                       MOVE NC"○" TO 高７チェックＷ
                   END-EVALUATE
               ELSE
028984             EVALUATE 受－特別区分
                   WHEN 1
                   WHEN 2
                       MOVE NC"○" TO 高一チェックＷ
                   WHEN 3
                       MOVE NC"○" TO 高７チェックＷ
028991             WHEN 6
                       MOVE NC"○" TO ６歳チェックＷ
                   WHEN OTHER
                       IF 受－本人家族区分 = 1
                           MOVE NC"○" TO 本人チェックＷ
                       ELSE
                           MOVE NC"○" TO 家族チェックＷ
                       END-IF
028999             END-EVALUATE
               END-IF
               EVALUATE レセ－負担割合
               WHEN ZERO
                   MOVE NC"○" TO １０割チェックＷ
               WHEN 1
                   MOVE NC"○" TO ９割チェックＷ
      */神奈川県の場合、前期高齢者１割は、給付割合を８割にする。(国が１割負担するため、患者１割、保険者８割、国１割となる)
                   IF (受－保険種別     = 01 AND 受－保険者番号(1:2) = "14") OR
                      (受－保険種別 NOT = 01 AND 受－保険者番号(3:2) = "14")
                       IF (受－保険種別 NOT = 05 ) AND (受－特別区分 = 1)
                           MOVE SPACE  TO ９割チェックＷ
                           MOVE NC"○" TO ８割チェックＷ
                       END-IF
                   END-IF
               WHEN 2
                   MOVE NC"○" TO ８割チェックＷ
               WHEN 3
                   MOVE NC"○" TO ７割チェックＷ
               END-EVALUATE
      *
025550         MOVE 受－施術年       TO 施術年Ｗ
025560         MOVE 受－施術月       TO 施術月Ｗ
025570         MOVE 受－患者番号     TO 患者番号Ｗ
025580*         MOVE 受－記号         TO 記号Ｗ
025590*         MOVE 受－番号         TO 番号Ｗ
      *                                          
               MOVE SPACE TO 連暗号複合－暗号情報
      *
      *    / 連暗号複合－入力情報セット /
               MOVE 受－記号       TO 連暗号複合－記号
               MOVE 受－番号       TO 連暗号複合－番号
               MOVE 受－暗号化項目 TO 連暗号複合－暗号化項目
      *     
               CALL   複合プログラム名Ｗ
               CANCEL 複合プログラム名Ｗ
      *
               MOVE 連暗号複合－複合した記号 TO 記号Ｗ
               MOVE 連暗号複合－複合した番号 TO 番号Ｗ
      *
025600         MOVE 受－保険者番号   TO 保険者番号Ｗ
025610         MOVE 受－保険種別     TO 保険種別Ｗ
025620*         PERFORM 保険種別編集
025630** 全国土木の枝番削除
025640         IF ( 受－保険種別 = 01 ) AND ( 受－保険者番号(1:6) = "133033" )
025650            MOVE 受－保険者番号(1:6)  TO 保険者番号Ｗ
025660         END-IF
025670**
025680         EVALUATE 受－保険種別
025690* 国保
025700         WHEN 01
025710            MOVE 受－保険者番号      TO 保険者番号比較Ｗ
025720* 退職
025720* 後高
               WHEN 05
025730         WHEN 08
025740            MOVE 受－保険者番号(3:6) TO 保険者番号比較Ｗ
025750         END-EVALUATE
025760         MOVE 受－保険者番号   TO 保険者番号Ｗ
025770**
025780         MOVE 受－被保険者カナ TO 被保険者カナＷ
025790         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
025800         MOVE 受－郵便番号１   TO 郵便番号１Ｗ
025810         MOVE 受－郵便番号２   TO 郵便番号２Ｗ
025820*         STRING 受－住所１  DELIMITED BY SPACE
025830*                受－住所２  DELIMITED BY SPACE
025840*           INTO 被保険者住所Ｗ
025850*         END-STRING
025860         MOVE 受－住所１       TO 被保険者住所１Ｗ
025870         MOVE 受－住所２       TO 被保険者住所２Ｗ
      */ 電話番号追加 /42505
               IF 受－電話番号 NOT = SPACE
                  MOVE 受－電話番号 TO 電話番号Ｗ
               ELSE
                  IF 受－患者電話番号 NOT = SPACE
                  MOVE 受－患者電話番号 TO 電話番号Ｗ
                  END-IF
               END-IF
025880         MOVE 受－患者カナ     TO 患者カナＷ
025890         MOVE 受－患者氏名     TO 患者氏名Ｗ
025900         EVALUATE 受－患者性別
025910         WHEN 1
025920             MOVE NC"○"  TO 男チェックＷ
025930         WHEN 2
025940             MOVE NC"○"  TO 女チェックＷ
025950         END-EVALUATE
025960*         EVALUATE 受－患者性別
025970*         WHEN 1
025980*             MOVE NC"男"  TO 性別Ｗ
025990*         WHEN 2
026000*             MOVE NC"女"  TO 性別Ｗ
026010*         END-EVALUATE
026020         EVALUATE 受－患者和暦
026030         WHEN 1
026040             MOVE NC"○"  TO 明治チェックＷ
026050         WHEN 2
026060             MOVE NC"○"  TO 大正チェックＷ
026070         WHEN 3
026080             MOVE NC"○"  TO 昭和チェックＷ
026090         WHEN 4
026100             MOVE NC"○"  TO 平成チェックＷ
026110         END-EVALUATE
026120         EVALUATE 受－患者和暦
026130         WHEN 1
026140             MOVE NC"明治"  TO 元号Ｗ
026150         WHEN 2
026160             MOVE NC"大正"  TO 元号Ｗ
026170         WHEN 3
026180             MOVE NC"昭和"  TO 元号Ｗ
026190         WHEN 4
026200             MOVE NC"平成"  TO 元号Ｗ
026210         END-EVALUATE
026220*
026230         MOVE 受－患者年  TO 患者年Ｗ
026240         MOVE 受－患者月  TO 患者月Ｗ
026250         MOVE 受－患者日  TO 患者日Ｗ
026260*
026680         IF  本人家族区分ＷＲ = 1 
026690             MOVE NC"本人"    TO 続柄Ｗ
026700         ELSE
026710             MOVE NC"家族"    TO 続柄Ｗ
026720         END-IF
026730**
026740         IF ( 受－助成種別 NOT = ZERO )
026750            PERFORM 助成レセまとめ判定
026760         ELSE
026770            MOVE SPACE TO 助成レセまとめフラグ
026780         END-IF
026790*
027110     END-IF.
027120*
027130*================================================================*
027140 家族続柄セット SECTION.
027150*
027160     MOVE 05       TO 名－区分コード.
027170     MOVE 受－続柄 TO 名－名称コード.
027180     READ 名称マスタ
027190     INVALID KEY
027200         MOVE SPACE    TO 続柄Ｗ
027210     NOT INVALID KEY
027220         MOVE 名－略称 TO 続柄Ｗ
027230     END-READ.
027240*
027250*================================================================*
027260 助成レセまとめ判定 SECTION.
027270*---------------------------------------------------------------------------*
027280* 本体まとめ区分＝１
027290* の時は、フラグYES (金額を助成込みで印字,適用２に助成種別印字）
027300*（例：横浜市の障害は、本体保険（国保系）のレセプト１枚で請求、助成レセはなし）
027310*---------------------------------------------------------------------------*
027320*
027330     MOVE SPACE TO 助成レセまとめフラグ.
027340     MOVE SPACE TO 助成種別略称Ｗ.
009201     IF レセ－本体まとめ区分 = 1 
009202        MOVE "YES" TO 助成レセまとめフラグ
027450        MOVE 02            TO 名－区分コード
027460        MOVE 受－助成種別  TO 名－名称コード
027470        READ 名称マスタ
027480        NOT INVALID KEY
027490           MOVE 名－略称  TO 助成種別略称Ｗ
027500        END-READ
009203     END-IF.
027540*
027550*================================================================*
028020 負傷データ取得 SECTION.
028030*================================================================*
028040**************************************************
028050* 連結データから負傷データＦより以下の情報を取得 *
028060* ● 負傷名...部位＋負傷種別にて加工して格納     *
028070* ● 負傷年.......負傷年Ｗ                       *
028080* ● 負傷月.......負傷月Ｗ                       *
028090* ● 負傷日.......負傷日Ｗ                       *
028100* ● 開始年.......初検年Ｗ                       *
028110* ● 開始月.......初検月Ｗ                       *
028120* ● 開始日.......初検日Ｗ                       *
028130* ● 終了年.......終了年Ｗ                       *
028140* ● 終了月.......終了月Ｗ                       *
028150* ● 終了日.......終了日Ｗ                       *
028160* ● 実日数.......実日数Ｗ                       *
028170* ● 転帰区分 ....区分によりチェックに"○"を格納 *
028180* ● 金属副子 ....区分によりチェックに"○"を格納 *
028190* ● 経過コード...経過マスタより取得             *
028200**************************************************
           IF 負－レコード NOT = SPACE
028300         MOVE 負－部位数                   TO 部位数Ｗ
028310         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
028320                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
028330             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別Ｗ(部位ＣＮＴ)
028340             MOVE 負－部位(部位ＣＮＴ)     TO 部位Ｗ(部位ＣＮＴ)
028350             MOVE 負－左右区分(部位ＣＮＴ) TO 左右区分Ｗ(部位ＣＮＴ)
028360             MOVE 負－負傷位置番号(部位ＣＮＴ)
028370                                           TO 負傷位置番号Ｗ(部位ＣＮＴ)
028380********************************************************
028390* 注）全柔...部位名1+負傷種別＋部位名2にて加工して格納 *
028400********************************************************
028410* 負傷種別
028420             MOVE SPACE                     TO 負傷名称Ｗ
028430             MOVE 03                        TO 名－区分コード
028440             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
028450             READ 名称マスタ
028460             INVALID KEY
028470                 MOVE SPACE        TO 負傷名称Ｗ
028480             NOT INVALID KEY
028490                 MOVE 名－正式名称 TO 負傷名称Ｗ
028500             END-READ
028510* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
032680*
032690             PERFORM 部位名称埋込処理
028700*
028710             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
028720             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
028730             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
028740             MOVE 負－開始年(部位ＣＮＴ)   TO 初検年Ｗ(部位ＣＮＴ)
028750             MOVE 負－開始月(部位ＣＮＴ)   TO 初検月Ｗ(部位ＣＮＴ)
028760             MOVE 負－開始日(部位ＣＮＴ)   TO 初検日Ｗ(部位ＣＮＴ)
028770             IF ( 負－転帰区分(部位ＣＮＴ) = 9 )
028780                 MOVE 99                   TO 終了年Ｗ(部位ＣＮＴ)
028790                 MOVE 99                   TO 終了月Ｗ(部位ＣＮＴ)
028800                 MOVE 99                   TO 終了日Ｗ(部位ＣＮＴ)
028810             ELSE
028820                 MOVE 負－終了年(部位ＣＮＴ)   TO 終了年Ｗ(部位ＣＮＴ)
028830                 MOVE 負－終了月(部位ＣＮＴ)   TO 終了月Ｗ(部位ＣＮＴ)
028840                 MOVE 負－終了日(部位ＣＮＴ)   TO 終了日Ｗ(部位ＣＮＴ)
028850             END-IF
028860* 経過略称取得
028870             MOVE 01                         TO 経－区分コード
028880             MOVE 負－経過コード(部位ＣＮＴ) TO 経－経過コード
028890             READ 経過マスタ
028900             INVALID KEY
028910                 MOVE ZERO            TO 部位ＣＮＴＷ(部位ＣＮＴ)
028920                 MOVE SPACE           TO 部位区切Ｗ(部位ＣＮＴ)
028930                 MOVE SPACE           TO 経過略称Ｗ(部位ＣＮＴ)
028940             NOT INVALID KEY
028950                 EVALUATE 部位ＣＮＴ
028960                 WHEN 1
028970                     MOVE NC"①" TO 経過部位Ｗ
028980                 WHEN 2
028990                     MOVE NC"②" TO 経過部位Ｗ
029000                 WHEN 3
029010                     MOVE NC"③" TO 経過部位Ｗ
029020                 WHEN 4
029030                     MOVE NC"④" TO 経過部位Ｗ
029040                 WHEN 5
029050                     MOVE NC"⑤" TO 経過部位Ｗ
029060                 END-EVALUATE
029070                 STRING  経過部位Ｗ     DELIMITED BY SPACE
029080                         経－経過略称   DELIMITED BY SPACE
029090                        INTO 印刷経過略称Ｗ(部位ＣＮＴ)
029100                 END-STRING
029110             END-READ
029120*
029130             MOVE 負－転帰区分(部位ＣＮＴ) TO 転帰区分Ｗ(部位ＣＮＴ)
029140             EVALUATE 負－転帰区分(部位ＣＮＴ)
029150             WHEN 1
029160             WHEN 2
029170                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
029180             WHEN 3
029190                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
029200             WHEN 4
029210                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
029220             END-EVALUATE
029230*
                    MOVE レセ－部位実日数(部位ＣＮＴ) TO 実日数Ｗ(部位ＣＮＴ)
029240         END-PERFORM
029250* 新規/継続 チェック
033380         EVALUATE レセ－レセ請求区分
               WHEN 1
033390             MOVE NC"○"                   TO 新規チェックＷ
               WHEN 2
033410             MOVE NC"○"                   TO 継続チェックＷ
033400         WHEN 3
033390             MOVE NC"○"                   TO 新規チェックＷ
033410             MOVE NC"○"                   TO 継続チェックＷ
               WHEN OTHER
033410             MOVE NC"○"                   TO 継続チェックＷ
033420         END-EVALUATE
029310* 枝番判定用
029320         MOVE 負－開始診療日手動区分   TO  開始診療日手動区分Ｗ
029330*
029340* 負傷原因印刷区分
029350         MOVE 負－レセ負傷原因印刷区分 TO レセ負傷原因印刷区分Ｗ
027880         MOVE 負－レセ長期理由印刷区分 TO レセ長期理由印刷区分Ｗ
029360*
029370     END-IF.
029380*
029390*================================================================*
029400 部位名称埋込処理 SECTION.
029410*
006490     STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
009980            負傷名称Ｗ                    DELIMITED BY SPACE
006500            レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
006520       INTO 負傷名Ｗ(部位ＣＮＴ)
006570     END-STRING.
029550*
029560*================================================================*
029570 料金情報取得 SECTION.
029580*================================================================*
029590********************
029600* 料金データセット *
029610********************
029620*    ****************************************************************
029630*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
029640*    ****************************************************************
029650     MOVE レセ－初検料                 TO 初検料ＷＲ.
029660     IF ( レセ－時間外 = 1 )
029670         MOVE NC"○"                   TO 時間外チェックＷ
029680     END-IF.
029690     IF ( レセ－休日 = 1 )
029700         MOVE NC"○"                   TO 休日チェックＷ
029710     END-IF.
029720     IF ( レセ－深夜 = 1 )
029730         MOVE NC"○"                   TO 深夜チェックＷ
029740     END-IF.
029750     MOVE レセ－初検時相談料           TO 相談料ＷＲ.
029760*
029770     MOVE レセ－初検加算料             TO  初検加算料ＷＲ.
029780     MOVE レセ－再検料                 TO  再検料ＷＲ.
029790     MOVE レセ－往療距離               TO  往療距離ＷＲ.
029800     MOVE レセ－往療回数               TO  往療回数ＷＲ.
029810     MOVE レセ－往療料                 TO  往療料ＷＲ.
029820     MOVE レセ－往療加算料             TO  往療加算料ＷＲ.
029830*
029840     IF ( レセ－夜間 = 1 )
029850         MOVE NC"○"                   TO 夜間チェックＷ
029860     END-IF.
029870     IF ( レセ－難路 = 1 )
029880         MOVE NC"○"                   TO 難路チェックＷ
029890     END-IF.
029900     IF ( レセ－暴風雨雪 = 1 )
029910         MOVE NC"○"                   TO 暴風雨雪チェックＷ
029920     END-IF.
029930*
029940     MOVE レセ－金属副子加算料         TO  金属副子加算料ＷＲ.
029950*
      */金属副子・運動後療の変更・追加/1805
021850     IF ( レセ－大 >= 1 )
021860         MOVE NC"○"                   TO 大チェックＷ
021870     END-IF.
021880     IF ( レセ－中 >= 1 )
021890         MOVE NC"○"                   TO 中チェックＷ
021900     END-IF.
021910     IF ( レセ－小 >= 1 )
021920         MOVE NC"○"                   TO 小チェックＷ
021930     END-IF.
           IF ( 施術和暦年月ＷＲ >= 43006 )
              MOVE レセ－金属副子回数        TO 金属回数Ｗ
           END-IF.
           MOVE レセ－運動後療料              TO 運動料Ｗ.
030050*
030060     MOVE レセ－施術情報提供料         TO  施術情報提供料ＷＲ.
030070* 小計
030080     MOVE レセ－小計                   TO 小計Ｗ.
030090********************
030100* 初回処置料セット *
030110********************
030120     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
030130             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
030140         MOVE レセ－初回処置料(部位ＣＮＴ) TO 初回処置料ＷＲ(部位ＣＮＴ)
030150     END-PERFORM.
030160     MOVE レセ－初回処置料合計         TO 初回処置料合計Ｗ.
030170********************
030180* 逓減毎料金セット *
030190********************
030200*    **********
030210*    * １部位 *
030220*    **********
030230     MOVE レセ－後療単価１             TO 後療単価１ＷＲ.
030240     MOVE レセ－後療回数１             TO 後療回数１ＷＲ.
030250     MOVE レセ－後療料１               TO 後療料１ＷＲ.
030260     MOVE レセ－冷罨法回数１           TO 冷罨法回数１ＷＲ.
030270     MOVE レセ－冷罨法料１             TO 冷罨法料１ＷＲ.
030280     MOVE レセ－温罨法回数１           TO 温罨法回数１ＷＲ.
030290     MOVE レセ－温罨法料１             TO 温罨法料１ＷＲ.
030300     MOVE レセ－電療回数１             TO 電療回数１ＷＲ.
030310     MOVE レセ－電療料１               TO 電療料１ＷＲ.
030320     MOVE レセ－小計１                 TO 小計１ＷＲ.
030330     MOVE レセ－長期逓減率１           TO 長期逓減率１ＷＲ.
030340     MOVE レセ－長期込小計１           TO 長期込小計１ＷＲ.
030350*    **********
030360*    * ２部位 *
030370*    **********
030380     MOVE レセ－後療単価２             TO 後療単価２ＷＲ.
030390     MOVE レセ－後療回数２             TO 後療回数２ＷＲ.
030400     MOVE レセ－後療料２               TO 後療料２ＷＲ.
030410     MOVE レセ－冷罨法回数２           TO 冷罨法回数２ＷＲ.
030420     MOVE レセ－冷罨法料２             TO 冷罨法料２ＷＲ.
030430     MOVE レセ－温罨法回数２           TO 温罨法回数２ＷＲ.
030440     MOVE レセ－温罨法料２             TO 温罨法料２ＷＲ.
030450     MOVE レセ－電療回数２             TO 電療回数２ＷＲ.
030460     MOVE レセ－電療料２               TO 電療料２ＷＲ.
030470     MOVE レセ－小計２                 TO 小計２ＷＲ.
030480     MOVE レセ－長期逓減率２           TO 長期逓減率２ＷＲ.
030490     MOVE レセ－長期込小計２           TO 長期込小計２ＷＲ.
030500*    ****************
030510*    * ３部位／８割 *
030520*    ****************
030530     MOVE レセ－後療単価３８             TO 後療単価３８ＷＲ.
030540     MOVE レセ－後療回数３８             TO 後療回数３８ＷＲ.
030550     MOVE レセ－後療料３８               TO 後療料３８ＷＲ.
030560     MOVE レセ－冷罨法回数３８           TO 冷罨法回数３８ＷＲ.
030570     MOVE レセ－冷罨法料３８             TO 冷罨法料３８ＷＲ.
030580     MOVE レセ－温罨法回数３８           TO 温罨法回数３８ＷＲ.
030590     MOVE レセ－温罨法料３８             TO 温罨法料３８ＷＲ.
030600     MOVE レセ－電療回数３８             TO 電療回数３８ＷＲ.
030610     MOVE レセ－電療料３８               TO 電療料３８ＷＲ.
030620     MOVE レセ－小計３８                 TO 小計３８ＷＲ.
030630     MOVE レセ－多部位込小計３８         TO 多部位込小計３８ＷＲ.
030640     MOVE レセ－長期逓減率３８           TO 長期逓減率３８ＷＲ.
030650     MOVE レセ－長期込小計３８           TO 長期込小計３８ＷＲ.
030660*    ****************
030670*    * ３部位／10割 *
030680*    ****************
030690     MOVE レセ－逓減開始月３０           TO 逓減開始月３０ＷＲ.
030700     MOVE レセ－逓減開始日３０           TO 逓減開始日３０ＷＲ.
030710     MOVE レセ－後療単価３０             TO 後療単価３０ＷＲ.
030720     MOVE レセ－後療回数３０             TO 後療回数３０ＷＲ.
030730     MOVE レセ－後療料３０               TO 後療料３０ＷＲ.
030740     MOVE レセ－冷罨法回数３０           TO 冷罨法回数３０ＷＲ.
030750     MOVE レセ－冷罨法料３０             TO 冷罨法料３０ＷＲ.
030760     MOVE レセ－温罨法回数３０           TO 温罨法回数３０ＷＲ.
030770     MOVE レセ－温罨法料３０             TO 温罨法料３０ＷＲ.
030780     MOVE レセ－電療回数３０             TO 電療回数３０ＷＲ.
030790     MOVE レセ－電療料３０               TO 電療料３０ＷＲ.
030800     MOVE レセ－小計３０                 TO 小計３０ＷＲ.
030810     MOVE レセ－長期逓減率３０           TO 長期逓減率３０ＷＲ.
030820     MOVE レセ－長期込小計３０           TO 長期込小計３０ＷＲ.
030830*    ****************
030840*    * ４部位／５割 *
030850*    ****************
030860     MOVE レセ－後療単価４５             TO 後療単価４５ＷＲ.
030870     MOVE レセ－後療回数４５             TO 後療回数４５ＷＲ.
030880     MOVE レセ－後療料４５               TO 後療料４５ＷＲ.
030890     MOVE レセ－冷罨法回数４５           TO 冷罨法回数４５ＷＲ.
030900     MOVE レセ－冷罨法料４５             TO 冷罨法料４５ＷＲ.
030910     MOVE レセ－温罨法回数４５           TO 温罨法回数４５ＷＲ.
030920     MOVE レセ－温罨法料４５             TO 温罨法料４５ＷＲ.
030930     MOVE レセ－電療回数４５             TO 電療回数４５ＷＲ.
030940     MOVE レセ－電療料４５               TO 電療料４５ＷＲ.
030950     MOVE レセ－小計４５                 TO 小計４５ＷＲ.
030960     MOVE レセ－多部位込小計４５         TO 多部位込小計４５ＷＲ.
030970     MOVE レセ－長期逓減率４５           TO 長期逓減率４５ＷＲ.
030980     MOVE レセ－長期込小計４５           TO 長期込小計４５ＷＲ.
030990*    ****************
031000*    * ４部位／８割 *
031010*    ****************
031020     MOVE レセ－逓減開始月４８           TO 逓減開始月４８ＷＲ.
031030     MOVE レセ－逓減開始日４８           TO 逓減開始日４８ＷＲ.
031040     MOVE レセ－後療単価４８             TO 後療単価４８ＷＲ.
031050     MOVE レセ－後療回数４８             TO 後療回数４８ＷＲ.
031060     MOVE レセ－後療料４８               TO 後療料４８ＷＲ.
031070     MOVE レセ－冷罨法回数４８           TO 冷罨法回数４８ＷＲ.
031080     MOVE レセ－冷罨法料４８             TO 冷罨法料４８ＷＲ.
031090     MOVE レセ－温罨法回数４８           TO 温罨法回数４８ＷＲ.
031100     MOVE レセ－温罨法料４８             TO 温罨法料４８ＷＲ.
031110     MOVE レセ－電療回数４８             TO 電療回数４８ＷＲ.
031120     MOVE レセ－電療料４８               TO 電療料４８ＷＲ.
031130     MOVE レセ－小計４８                 TO 小計４８ＷＲ.
031140     MOVE レセ－多部位込小計４８         TO 多部位込小計４８ＷＲ.
031150     MOVE レセ－長期逓減率４８           TO 長期逓減率４８ＷＲ.
031160     MOVE レセ－長期込小計４８           TO 長期込小計４８ＷＲ.
031170*    ****************
031180*    * ４部位／10割 *
031190*    ****************
031200     MOVE レセ－逓減開始月４０           TO 逓減開始月４０ＷＲ.
031210     MOVE レセ－逓減開始日４０           TO 逓減開始日４０ＷＲ.
031220     MOVE レセ－後療単価４０             TO 後療単価４０ＷＲ.
031230     MOVE レセ－後療回数４０             TO 後療回数４０ＷＲ.
031240     MOVE レセ－後療料４０               TO 後療料４０ＷＲ.
031250     MOVE レセ－冷罨法回数４０           TO 冷罨法回数４０ＷＲ.
031260     MOVE レセ－冷罨法料４０             TO 冷罨法料４０ＷＲ.
031270     MOVE レセ－温罨法回数４０           TO 温罨法回数４０ＷＲ.
031280     MOVE レセ－温罨法料４０             TO 温罨法料４０ＷＲ.
031290     MOVE レセ－電療回数４０             TO 電療回数４０ＷＲ.
031300     MOVE レセ－電療料４０               TO 電療料４０ＷＲ.
031310     MOVE レセ－小計４０                 TO 小計４０ＷＲ.
031320     MOVE レセ－長期逓減率４０           TO 長期逓減率４０ＷＲ.
031330     MOVE レセ－長期込小計４０           TO 長期込小計４０ＷＲ.
031340*    *****************
031350*    * ５部位／2.5割 *
031360*    *****************
031370     MOVE レセ－後療単価５２             TO 後療単価５２ＷＲ.
031380     MOVE レセ－後療回数５２             TO 後療回数５２ＷＲ.
031390     MOVE レセ－後療料５２               TO 後療料５２ＷＲ.
031400     MOVE レセ－冷罨法回数５２           TO 冷罨法回数５２ＷＲ.
031410     MOVE レセ－冷罨法料５２             TO 冷罨法料５２ＷＲ.
031420     MOVE レセ－温罨法回数５２           TO 温罨法回数５２ＷＲ.
031430     MOVE レセ－温罨法料５２             TO 温罨法料５２ＷＲ.
031440     MOVE レセ－電療回数５２             TO 電療回数５２ＷＲ.
031450     MOVE レセ－電療料５２               TO 電療料５２ＷＲ.
031460     MOVE レセ－小計５２                 TO 小計５２ＷＲ.
031470     MOVE レセ－多部位込小計５２         TO 多部位込小計５２ＷＲ.
031480     MOVE レセ－長期逓減率５２           TO 長期逓減率５２ＷＲ.
031490     MOVE レセ－長期込小計５２           TO 長期込小計５２ＷＲ.
031500*    ****************
031510*    * ５部位／５割 *
031520*    ****************
031530     MOVE レセ－逓減開始月５５           TO 逓減開始月５５ＷＲ.
031540     MOVE レセ－逓減開始日５５           TO 逓減開始日５５ＷＲ.
031550     MOVE レセ－後療単価５５             TO 後療単価５５ＷＲ.
031560     MOVE レセ－後療回数５５             TO 後療回数５５ＷＲ.
031570     MOVE レセ－後療料５５               TO 後療料５５ＷＲ.
031580     MOVE レセ－冷罨法回数５５           TO 冷罨法回数５５ＷＲ.
031590     MOVE レセ－冷罨法料５５             TO 冷罨法料５５ＷＲ.
031600     MOVE レセ－温罨法回数５５           TO 温罨法回数５５ＷＲ.
031610     MOVE レセ－温罨法料５５             TO 温罨法料５５ＷＲ.
031620     MOVE レセ－電療回数５５             TO 電療回数５５ＷＲ.
031630     MOVE レセ－電療料５５               TO 電療料５５ＷＲ.
031640     MOVE レセ－小計５５                 TO 小計５５ＷＲ.
031650     MOVE レセ－多部位込小計５５         TO 多部位込小計５５ＷＲ.
031660     MOVE レセ－長期逓減率５５           TO 長期逓減率５５ＷＲ.
031670     MOVE レセ－長期込小計５５           TO 長期込小計５５ＷＲ.
031680*    ****************
031690*    * ５部位／８割 *
031700*    ****************
031710     MOVE レセ－逓減開始月５８           TO 逓減開始月５８ＷＲ.
031720     MOVE レセ－逓減開始日５８           TO 逓減開始日５８ＷＲ.
031730     MOVE レセ－後療単価５８             TO 後療単価５８ＷＲ.
031740     MOVE レセ－後療回数５８             TO 後療回数５８ＷＲ.
031750     MOVE レセ－後療料５８               TO 後療料５８ＷＲ.
031760     MOVE レセ－冷罨法回数５８           TO 冷罨法回数５８ＷＲ.
031770     MOVE レセ－冷罨法料５８             TO 冷罨法料５８ＷＲ.
031780     MOVE レセ－温罨法回数５８           TO 温罨法回数５８ＷＲ.
031790     MOVE レセ－温罨法料５８             TO 温罨法料５８ＷＲ.
031800     MOVE レセ－電療回数５８             TO 電療回数５８ＷＲ.
031810     MOVE レセ－電療料５８               TO 電療料５８ＷＲ.
031820     MOVE レセ－小計５８                 TO 小計５８ＷＲ.
031830     MOVE レセ－多部位込小計５８         TO 多部位込小計５８ＷＲ.
031840     MOVE レセ－長期逓減率５８           TO 長期逓減率５８ＷＲ.
031850     MOVE レセ－長期込小計５８           TO 長期込小計５８ＷＲ.
031860*    ****************
031870*    * ５部位／10割 *
031880*    ****************
031890     MOVE レセ－逓減開始月５０           TO 逓減開始月５０ＷＲ.
031900     MOVE レセ－逓減開始日５０           TO 逓減開始日５０ＷＲ.
031910     MOVE レセ－後療単価５０             TO 後療単価５０ＷＲ.
031920     MOVE レセ－後療回数５０             TO 後療回数５０ＷＲ.
031930     MOVE レセ－後療料５０               TO 後療料５０ＷＲ.
031940     MOVE レセ－冷罨法回数５０           TO 冷罨法回数５０ＷＲ.
031950     MOVE レセ－冷罨法料５０             TO 冷罨法料５０ＷＲ.
031960     MOVE レセ－温罨法回数５０           TO 温罨法回数５０ＷＲ.
031970     MOVE レセ－温罨法料５０             TO 温罨法料５０ＷＲ.
031980     MOVE レセ－電療回数５０             TO 電療回数５０ＷＲ.
031990     MOVE レセ－電療料５０               TO 電療料５０ＷＲ.
032000     MOVE レセ－小計５０                 TO 小計５０ＷＲ.
032010     MOVE レセ－長期逓減率５０           TO 長期逓減率５０ＷＲ.
032020     MOVE レセ－長期込小計５０           TO 長期込小計５０ＷＲ.
032030*
032040*================================================================*
032050 施術記録取得 SECTION.
032060*================================================================*
032070************************************************************
032080* 作１データから負傷データＦより以下の情報を取得           *
032090* ● 初検加算 .....区分によりチェックに"○"を格納...複数可 *
032100* ● 往療加算 .....区分によりチェックに"○"を格納...複数可 *
032110************************************************************
032120     MOVE  SPACE  TO  初日再検フラグ.
032130     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 部位数Ｗ
032140         IF ( 施術年Ｗ = 初検年Ｗ(部位ＣＮＴ) ) AND
032150            ( 施術月Ｗ = 初検月Ｗ(部位ＣＮＴ) )
032160             MOVE 患者番号ＷＲ          TO 施記－患者番号
032170             MOVE 枝番ＷＲ              TO 施記－枝番
032180             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
032190             MOVE 初検年Ｗ(部位ＣＮＴ)  TO 開始年Ｗ(部位ＣＮＴ) 施記－施術年
032200             MOVE 初検月Ｗ(部位ＣＮＴ)  TO 開始月Ｗ(部位ＣＮＴ) 施記－施術月
032210             MOVE 初検日Ｗ(部位ＣＮＴ)  TO 開始日Ｗ(部位ＣＮＴ) 施記－施術日
032220         ELSE
032230             MOVE 患者番号ＷＲ          TO 施記－患者番号
032240             MOVE 枝番ＷＲ              TO 施記－枝番
032250             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
032260             MOVE 施術年ＷＲ            TO 施記－施術年
032270             MOVE 施術月ＷＲ            TO 施記－施術月
032280             MOVE ZERO                  TO 施記－施術日
032290         END-IF
032300         START 施術記録Ｆ   KEY IS >= 施記－患者コード
032310                                      施記－施術和暦年月日
032320         END-START
032330         IF ( 状態キー = "00" )
032350             MOVE ZERO  TO 終了年ＷＴ
032360             MOVE ZERO  TO 終了月ＷＴ
032370             MOVE ZERO  TO 終了日ＷＴ
032380             MOVE SPACE TO 終了フラグ２
032390             PERFORM 施術記録Ｆ読込
032400             IF  ( 終了フラグ２      = SPACE   ) AND
032410                 ( 施記－患者コード  = 患者コードＷＲ ) AND
032420                 ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
032430                 ( 施記－施術年      = 施術年ＷＲ     ) AND
032440                 ( 施記－施術月      = 施術月ＷＲ     ) 
032450*
032460*        *****************************************************************
032470*        * 開始年月日 ( その部位が当月初検でないか、
032480*                       当月初検でも枝番がある時は、最初の施術日を開始日)*
032490*        *****************************************************************
032500                 IF ( 施術年Ｗ NOT = 初検年Ｗ(部位ＣＮＴ) ) OR
032510                    ( 施術月Ｗ NOT = 初検月Ｗ(部位ＣＮＴ) ) OR
032520                    ( 開始診療日手動区分Ｗ = 1 )
032530                     MOVE 施記－施術年   TO 開始年Ｗ(部位ＣＮＴ)
032540                     MOVE 施記－施術月   TO 開始月Ｗ(部位ＣＮＴ)
032550                     MOVE 施記－施術日   TO 開始日Ｗ(部位ＣＮＴ)
032560                 END-IF
032570             END-IF
032580             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
032590                           ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
032600                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
032610                           ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
032620                           ( 施記－施術月     NOT = 施術月ＷＲ       ) OR
032630                           ( 施記－施術日         > 終了日Ｗ(部位ＣＮＴ))
032680                MOVE 施記－施術年               TO 終了年ＷＴ
032690                MOVE 施記－施術月               TO 終了月ＷＴ
032700                MOVE 施記－施術日               TO 終了日ＷＴ
032710*
032720                PERFORM 施術記録Ｆ読込
032730            END-PERFORM
032740        END-IF
032750*       **************************
032760*       * 継続：終了年月日セット *
032770*       **************************
032780        IF ( 転帰区分Ｗ(部位ＣＮＴ) = 9 )
032790            MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
032800            MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
032810            MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
032820        END-IF
032830        IF ( 終了年月日Ｗ(部位ＣＮＴ) > 受理年月日Ｗ )
032840            MOVE 終了年Ｗ(部位ＣＮＴ) TO 受理年Ｗ
032850            MOVE 終了月Ｗ(部位ＣＮＴ) TO 受理月Ｗ
032860            MOVE 終了日Ｗ(部位ＣＮＴ) TO 受理日Ｗ
032870        END-IF
032880     END-PERFORM.
032890*
032900** ----- 前月初検のみかを判定 -----------*
032910*
032920*     MOVE 患者番号ＷＲ          TO 施記－患者番号.
032930*     MOVE 枝番ＷＲ              TO 施記－枝番.
032940*     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
032950*     MOVE 施術年ＷＲ            TO 施記－施術年.
032960*     MOVE 施術月ＷＲ            TO 施記－施術月.
032970*     MOVE ZERO                  TO 施記－施術日.
032980*     START 施術記録Ｆ   KEY IS >= 施記－患者コード
032990*                                  施記－施術和暦年月日
033000*     END-START.
033010*     IF ( 状態キー = "00" )
033020*             MOVE SPACE TO 終了フラグ２
033030*             PERFORM 施術記録Ｆ読込
033040*             IF  ( 終了フラグ２      = SPACE   ) AND
033050*                 ( 施記－患者コード  = 患者コードＷＲ ) AND
033060*                 ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
033070*                 ( 施記－施術年      = 施術年ＷＲ     ) AND
033080*                 ( 施記－施術月      = 施術月ＷＲ     ) 
033090** 当月施術開始日が再検かどうか判定
033100*                 IF   ( 施記－再検料請求 = 1 )
033110*                      MOVE "YES"  TO  初日再検フラグ
033120*                 END-IF
033130**
033140*             END-IF
033150*     END-IF.
033160*     IF ( 初日再検フラグ = "YES" )
033170*        PERFORM 前月初検のみ判定
033180*     END-IF.
033190*
033200*================================================================*
033210 前月初検のみ判定 SECTION.
033220*
033230*** 前月の通院日が初検か判定 
033240     MOVE  SPACE            TO 前月フラグ.
033250     MOVE 受－患者コード    TO 施記－患者コード.
033260     MOVE 受－施術和暦      TO 施記－施術和暦.
033270     MOVE 受－施術年        TO 施記－施術年.
033280     MOVE 受－施術月        TO 施記－施術月.
033290     MOVE 1                 TO 施記－施術日.
033300     START 施術記録Ｆ   KEY IS <  施記－患者コード
033310                                  施記－施術和暦年月日
033320                                  REVERSED
033330     END-START.
033340     IF ( 状態キー = "00" )
033350         MOVE SPACE  TO 終了フラグ２
033360         PERFORM 施術記録Ｆ読込
033370         IF ( 終了フラグ２      = SPACE  ) AND
033380            ( 施記－患者コード  = 受－患者コード ) AND
033390            ( 施記－診療区分    = 2 ) 
033400*
033410            PERFORM 前月判定
033420**** 適用１を使用
033430            IF ( 前月フラグ = "YES" )
033440               MOVE NC"※前月初検のみ"    TO  適用１Ｗ
033450            END-IF
033460**
033470         END-IF
033480     END-IF.
033490*
033500*================================================================*
033510 前月判定  SECTION.
033520* 
033530*** 読み込んだ施術記録の年月が、前月かどうか判定 (年月の差が 1 か?)
033540      MOVE  SPACE  TO  前月フラグ.
033550      INITIALIZE  計算年月日Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
033560**
033570      MOVE 受－施術和暦    TO 終了和暦２Ｗ.
033580      MOVE 受－施術年      TO 終了年２Ｗ.
033590      MOVE 受－施術月      TO 終了月２Ｗ.
033600      MOVE 施記－施術和暦  TO 開始和暦２Ｗ.
033610      MOVE 施記－施術年    TO 開始年２Ｗ.
033620      MOVE 施記－施術月    TO 開始月２Ｗ.
033630*
033640      EVALUATE TRUE
033650       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ = 終了年２Ｗ)
033660            PERFORM  前月比較月
033670       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ NOT = 終了年２Ｗ)
033680            PERFORM  前月比較年
033690       WHEN  開始和暦２Ｗ NOT = 終了和暦２Ｗ 
033700            PERFORM  前月比較元号
033710      END-EVALUATE.
033720*
033730      IF ( 計算月Ｗ = 1 )
033740         MOVE  "YES"  TO  前月フラグ
033750      END-IF.
033760*
033770*================================================================*
033780 前月比較元号  SECTION.
033790*
033800     MOVE 開始和暦２Ｗ TO 元－元号区分.
033810     READ 元号マスタ
033820     NOT INVALID KEY
033830         MOVE 元－開始西暦年 TO 開始西暦年Ｗ
033840     END-READ.
033850     MOVE 終了和暦２Ｗ TO 元－元号区分.
033860     READ 元号マスタ
033870     NOT INVALID KEY
033880         MOVE 元－開始西暦年 TO 終了西暦年Ｗ
033890     END-READ.
033900**
033910     IF ( 開始西暦年Ｗ NOT = ZERO ) AND ( 終了西暦年Ｗ NOT = ZERO )
033920        COMPUTE 開始西暦年Ｗ = 開始西暦年Ｗ + 開始年２Ｗ - 1
033930        COMPUTE 終了西暦年Ｗ = 終了西暦年Ｗ + 終了年２Ｗ - 1
033940*
033950        IF ( 終了西暦年Ｗ =  開始西暦年Ｗ )
033960           PERFORM  前月比較月
033970        ELSE
033980           IF  ( 終了西暦年Ｗ >  開始西暦年Ｗ )
033990               COMPUTE 計算年Ｗ = 終了西暦年Ｗ - 開始西暦年Ｗ
034000               COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
034010           ELSE
034020               MOVE ZERO TO 計算月Ｗ
034030           END-IF
034040        END-IF
034050     ELSE
034060        MOVE ZERO TO 計算月Ｗ
034070     END-IF.
034080*
034090*================================================================*
034100 前月比較年  SECTION.
034110*
034120     IF  ( 終了年２Ｗ >  開始年２Ｗ )
034130         COMPUTE 計算年Ｗ = 終了年２Ｗ - 開始年２Ｗ
034140         COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
034150     ELSE
034160        MOVE ZERO TO 計算月Ｗ
034170     END-IF.
034180*
034190*================================================================*
034200 前月比較月  SECTION.
034210*
034220     IF  ( 終了月２Ｗ >  開始月２Ｗ )
034230         COMPUTE 計算月Ｗ = 終了月２Ｗ - 開始月２Ｗ
034240     ELSE
034250        MOVE ZERO TO 計算月Ｗ
034260     END-IF.
034270*
034280*================================================================*
034290 長期判定取得 SECTION.
034300*================================================================*
034310* ３カ月以上の長期判定は "CHOUKI" を呼ぶ. 
034320     MOVE  SPACE TO  連期間－キー.
034330     INITIALIZE      連期間－キー.
034340     MOVE 施術和暦ＷＲ  TO  連期間－施術和暦.
034350     MOVE 施術年ＷＲ    TO  連期間－施術年.
034360     MOVE 施術月ＷＲ    TO  連期間－施術月.
034370     MOVE 患者番号ＷＲ  TO  連期間－患者番号.
034380     MOVE 枝番ＷＲ      TO  連期間－枝番.
034390*
034400     CALL   "CHOUKI".
034410     CANCEL "CHOUKI".
034420*
034430*
034440*     IF ( 連期間－対象フラグ  = "YES" )
034450*        MOVE NC"（長期施術継続必要理由）" TO 長期理由固定
034460*     END-IF.
034470*
035190*================================================================*
035200 初検加算時刻取得 SECTION.
035210*================================================================*
035220*****************************************************************
035230** 初検加算が時間外と深夜の時、適用に「受付時間」を印字する。
035240**   時刻の印字は月3回まで可能
035250*****************************************************************
035260     IF ( レセ－時間外 = 1 ) OR ( レセ－深夜 = 1 ) OR ( レセ－休日 = 1 )
035270*
035280         MOVE 患者番号ＷＲ          TO 施記－患者番号
035290         MOVE 枝番ＷＲ              TO 施記－枝番
035300         MOVE 施術和暦ＷＲ          TO 施記－施術和暦
035310         MOVE 施術年ＷＲ            TO 施記－施術年
035320         MOVE 施術月ＷＲ            TO 施記－施術月
035330         MOVE ZERO                  TO 施記－施術日
035340         START 施術記録Ｆ   KEY IS >= 施記－患者コード
035350                                      施記－施術和暦年月日
035360         END-START
035370         IF ( 状態キー = "00" )
035380             MOVE ZERO  TO 初検加算カウント
035390             MOVE SPACE TO 終了フラグ２
035400             PERFORM 施術記録Ｆ読込
035410             PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
035420                           ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
035430                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
035440                           ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
035450                           ( 施記－施術月     NOT = 施術月ＷＲ      ) 
035460                   IF  ( 施記－初検加算 = 1 OR 2 OR 3 ) AND ( 施記－診療区分 = 2 )
035470                       COMPUTE 初検加算カウント = 初検加算カウント  + 1
035480                       IF  初検加算カウント <= 3
035490                           MOVE 施記－初検加算 TO 初検加算区分ＷＴ(初検加算カウント)
035500                           MOVE 施記－受付時   TO 初検加算時ＷＴ(初検加算カウント)
035510                           MOVE 施記－受付分   TO 初検加算分ＷＴ(初検加算カウント)
035520                       END-IF
035530                   END-IF
035540                   PERFORM 施術記録Ｆ読込
035550             END-PERFORM
035560** 初検加算の時刻を適用にセット
033380            IF ( 初検加算時ＷＴ(1) NOT = ZERO ) OR ( 初検加算分ＷＴ(1) NOT = ZERO ) 
                     MOVE 初検加算時ＷＴ(1) TO 初検加算時Ｗ
                     MOVE ":"               TO 初検加算区切Ｗ
                     MOVE 初検加算分ＷＴ(1) TO 初検加算分Ｗ
                  END-IF
033380            IF ( 初検加算時ＷＴ(2) NOT = ZERO ) OR ( 初検加算分ＷＴ(2) NOT = ZERO ) 
031910               PERFORM 初検加算適用セット
                  END-IF
035580         END-IF
035590*
035600     END-IF.
035610*
035620*================================================================*
035630 初検加算適用セット SECTION.
035640*
035650     PERFORM VARYING 番号カウンタ FROM 1 BY 1
035660              UNTIL  番号カウンタ > 3
035670         IF ( 初検加算時ＷＴ(番号カウンタ)  = ZERO )  AND 
035680            ( 初検加算分ＷＴ(番号カウンタ)  = ZERO ) 
035690             CONTINUE
035700         ELSE
035710* 固定項目
035720             EVALUATE 初検加算区分ＷＴ(番号カウンタ) 
035730             WHEN 1
035740                MOVE NC"時間外"   TO 加算内容Ｗ(番号カウンタ)
033320             WHEN 2
033330                MOVE NC"休　日"   TO 加算内容Ｗ(番号カウンタ)
035750             WHEN 3
035760                MOVE NC"深　夜"   TO 加算内容Ｗ(番号カウンタ)
035770             END-EVALUATE
035780*
035790             MOVE NC"："          TO 加算区切Ｗ(番号カウンタ)
035800             MOVE NC"時"          TO 時固定Ｗ(番号カウンタ)
035810             MOVE NC"分"          TO 分固定Ｗ(番号カウンタ)
035820*
035830**** 数字→日本語変換
035840* 時間
035850             MOVE 初検加算時ＷＴ(番号カウンタ)  TO  数字Ｗ
035860             IF ( 数字Ｗ >= 10 )
035870                 MOVE 数字Ｗ１    TO 負傷番号Ｗ１
035880                 PERFORM 日本語変換
035890                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ１(番号カウンタ)
035900                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
035910                 PERFORM 日本語変換
035920                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
035930             ELSE
035940                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
035950                 PERFORM 日本語変換
035960                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
035970             END-IF
035980* 分
035990             MOVE 初検加算分ＷＴ(番号カウンタ)  TO  数字Ｗ
036000             MOVE 数字Ｗ１    TO 負傷番号Ｗ１
036010             PERFORM 日本語変換
036020             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ１(番号カウンタ)
036030             MOVE 数字Ｗ２    TO 負傷番号Ｗ１
036040             PERFORM 日本語変換
036050             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ２(番号カウンタ)
036060** 
036070        END-IF
036080     END-PERFORM.
036090*
036100     MOVE  初検加算集団ＮＷ(1)   TO 初検加算時刻１Ｗ. 
036110     MOVE  初検加算集団ＮＷ(2)   TO 初検加算時刻２Ｗ. 
036120     MOVE  初検加算集団ＮＷ(3)   TO 初検加算時刻３Ｗ. 
036130*
036140**** 適用１か２を使用（長期理由記載で適用１を使っている時は、適用２）
036150     IF ( 初検加算時ＷＴ(2)  = ZERO ) AND ( 初検加算分ＷＴ(2)  = ZERO ) 
036160         CONTINUE
036170     ELSE
036180         IF ( 適用１Ｗ  = SPACE )
036190               STRING NC"初検加算"       DELIMITED BY SIZE
036200                      初検加算時刻１Ｗ   DELIMITED BY SIZE
036210                      初検加算時刻２Ｗ   DELIMITED BY SIZE
036220                      初検加算時刻３Ｗ   DELIMITED BY SIZE
036230                      INTO 適用１Ｗ
036240               END-STRING
036250         ELSE
036260               STRING NC"初検加算"       DELIMITED BY SIZE
036270                      初検加算時刻１Ｗ   DELIMITED BY SIZE
036280                      初検加算時刻２Ｗ   DELIMITED BY SIZE
036290                      初検加算時刻３Ｗ   DELIMITED BY SIZE
036300                      INTO 適用２Ｗ
036310               END-STRING
036320         END-IF
036330     END-IF.
036340*
036350*================================================================*
036360 日本語変換 SECTION.
036370*
036380     MOVE NC"０"     TO 全角負傷番号Ｗ.
036390     CALL "htoz" WITH C LINKAGE
036400                        USING 負傷番号Ｗ１ 全角負傷番号Ｗ１.
036410*
036420*================================================================*
036430 委任年月日取得 SECTION.
036440*================================================================*
036450** ---// ここの受理年には、最終通院日が入っている為、退避する //----
036460     MOVE 受理年Ｗ   TO 最終通院年Ｗ.
036470     MOVE 受理月Ｗ   TO 最終通院月Ｗ.
036480     MOVE 受理日Ｗ   TO 最終通院日Ｗ.
036490***
036500* (柔整師側)
036510     EVALUATE レセプト日付区分Ｗ 
036520*    /  最終通院日 /
036530     WHEN ZERO
036540         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
036550         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
036560         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
036570*    /  月末日 /
036580     WHEN 1 
036590         PERFORM 月末日取得
036600         MOVE 受理年Ｗ     TO 柔整師年Ｗ
036610         MOVE 受理月Ｗ     TO 柔整師月Ｗ
036620         MOVE 受理日Ｗ     TO 柔整師日Ｗ
036630*    /  印字なし /
036640     WHEN 9
036650         MOVE ZERO         TO 柔整師年Ｗ
036660         MOVE ZERO         TO 柔整師月Ｗ
036670         MOVE ZERO         TO 柔整師日Ｗ
036680*    /  その他は、最終通院日 /
036690     WHEN OTHER
036700         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
036710         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
036720         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
036730     END-EVALUATE.
036740**
036750* (患者側)
036760     EVALUATE レセプト患者日付区分Ｗ 
036770*    /  最終通院日 /
036780     WHEN ZERO
036790         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
036800         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
036810         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
036820*    /  月末日 /
036830     WHEN 1 
036840         PERFORM 月末日取得
036850         MOVE 受理年Ｗ     TO 患者委任年Ｗ
036860         MOVE 受理月Ｗ     TO 患者委任月Ｗ
036870         MOVE 受理日Ｗ     TO 患者委任日Ｗ
036880*    /  印字なし /
036890     WHEN 9
036900         MOVE ZERO         TO 患者委任年Ｗ
036910         MOVE ZERO         TO 患者委任月Ｗ
036920         MOVE ZERO         TO 患者委任日Ｗ
036930*    /  その他は、最終通院日 /
036940     WHEN OTHER
036950         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
036960         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
036970         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
036980     END-EVALUATE.
036990*
037000*================================================================*
037010 月末日取得 SECTION.
037020*
037030     MOVE 施術年ＷＲ   TO 受理年Ｗ.
037040     MOVE 施術月ＷＲ   TO 受理月Ｗ.
037050     MOVE 施術和暦ＷＲ TO 元－元号区分.
037060     READ 元号マスタ
037070     NOT INVALID KEY
037080         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
037090     END-READ.
037100     IF ( 施術西暦年Ｗ NOT = ZERO )
037110        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
037120     END-IF.
037130*
037140     EVALUATE 施術月ＷＲ
037150     WHEN 4
037160     WHEN 6
037170     WHEN 9
037180     WHEN 11
037190         MOVE 30 TO 受理日Ｗ
037200     WHEN 2
037210         DIVIDE 4 INTO 施術西暦年Ｗ GIVING    商Ｗ
037220                                    REMAINDER 余Ｗ
037230         END-DIVIDE
037240         IF ( 余Ｗ = ZERO )
037250             MOVE 29 TO 受理日Ｗ
037260         ELSE
037270             MOVE 28 TO 受理日Ｗ
037280         END-IF
037290     WHEN 1
037300     WHEN 3
037310     WHEN 5
037320     WHEN 7
037330     WHEN 8
037340     WHEN 10
037350     WHEN 12
037360         MOVE 31 TO 受理日Ｗ
037370     WHEN OTHER
037380          CONTINUE
037390     END-EVALUATE.
037400*
037410*================================================================*
037420 負傷原因取得 SECTION.
037430*================================================================*
037440********************************************************************
037450*  負傷原因コードが同じものは、1行にまとめて印字する。
037460*  例: ①② 家で転んだ.
037470*     負傷原因コードが同じものをまとめ、テーブルにセット
037480*     (ただし、部位を飛んで同じものは、2行になる)
037490********************************************************************
037500     MOVE  ZERO   TO  カウンタ カウンタ２.
037510     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
037520             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
037530*
037540****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
037550        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
037560*
037570           IF ( カウンタ = ZERO )
037580               MOVE 1   TO  カウンタ カウンタ２
037590               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
037600               MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
037610               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
037620           ELSE
037630              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
037640                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
037650                 COMPUTE カウンタ２ = カウンタ２  +  1
037660                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
037670              ELSE
037680                 COMPUTE カウンタ = カウンタ  +  1
037690                 MOVE 1   TO  カウンタ２
037700                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
037710                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
037720                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
037730              END-IF
037740           END-IF
037750        END-IF
037760     END-PERFORM.
037770**************************************************************************
037780*  負傷原因マスタより文章取得
037790**************************************************************************
037800     MOVE  ZERO   TO  カウンタ カウンタ２.
037810     PERFORM VARYING カウンタ FROM 1 BY 1
037820             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
037830** 健保は 区分 01
037840         MOVE 01                        TO 負原－区分コード
037850         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
037860         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
037870         READ 負傷原因Ｆ
037880         NOT INVALID KEY
037890             INITIALIZE 負傷原因ＷＴ
037900             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
037910             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
037920             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
037930             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
037940             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
037950             PERFORM VARYING カウンタ２ FROM 1 BY 1
037960                     UNTIL ( カウンタ２ > 9 )  OR 
037970                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
037980                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
037990                WHEN 1
038000                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
038010                WHEN 2
038020                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
038030                WHEN 3
038040                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
038050                WHEN 4
038060                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
038070                WHEN 5
038080                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
038050                WHEN 6
038060                   MOVE "⑥"  TO  負傷原因ナンバーＷ１(カウンタ２)
038070                WHEN 7
038080                   MOVE "⑦"  TO  負傷原因ナンバーＷ１(カウンタ２)
038090                WHEN OTHER
038100                   CONTINUE
038110                END-EVALUATE
038120             END-PERFORM
038130*
038140             IF 負原－負傷原因入力区分 = 1
038150                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
038160                        負傷原因１ＷＴ  DELIMITED BY SIZE
038170                        負傷原因２ＷＴ  DELIMITED BY SIZE
038180                        負傷原因３ＷＴ  DELIMITED BY SIZE
038190                        負傷原因４ＷＴ  DELIMITED BY SIZE
038200                        負傷原因５ＷＴ  DELIMITED BY SIZE
038210                        INTO 負傷原因内容合成Ｗ(カウンタ)
038220                 END-STRING
038230             ELSE
005946                 INSPECT 負傷原因ＷＴ REPLACING ALL 全角空白 BY 半角空白
                       MOVE SPACE TO 文字１Ｗ 文字２Ｗ
                       MOVE 負傷原因ナンバーＮＷ TO 文字１Ｗ
                       MOVE 負傷原因１ＷＴ       TO 文字２Ｗ
                       CALL プログラム名Ｗ WITH C LINKAGE
                            USING BY REFERENCE 文字１Ｗ
                                  BY REFERENCE 文字２Ｗ
                       MOVE 負傷原因２ＷＴ       TO 文字２Ｗ
                       CALL プログラム名Ｗ WITH C LINKAGE
                            USING BY REFERENCE 文字１Ｗ
                                  BY REFERENCE 文字２Ｗ
                       MOVE 負傷原因３ＷＴ       TO 文字２Ｗ
                       CALL プログラム名Ｗ WITH C LINKAGE
                            USING BY REFERENCE 文字１Ｗ
                                  BY REFERENCE 文字２Ｗ
                       MOVE 負傷原因４ＷＴ       TO 文字２Ｗ
                       CALL プログラム名Ｗ WITH C LINKAGE
                            USING BY REFERENCE 文字１Ｗ
                                  BY REFERENCE 文字２Ｗ
                       MOVE 負傷原因５ＷＴ       TO 文字２Ｗ
                       CALL プログラム名Ｗ WITH C LINKAGE
                            USING BY REFERENCE 文字１Ｗ
                                  BY REFERENCE 文字２Ｗ
                        MOVE 文字１Ｗ TO 負傷原因内容合成Ｗ(カウンタ)
038320             END-IF
038330*
038340         END-READ
038350     END-PERFORM.
038360*
038370     PERFORM 負傷原因セット.
038380*
038390*================================================================*
038400 負傷原因セット SECTION.
038410*
038420**************************************************************************
038430*  文章が1行を超える時は、複数行に分解する。
038440**************************************************************************
038450     MOVE  ZERO   TO  カウンタ カウンタ２.
038460     PERFORM VARYING カウンタ FROM 1 BY 1
038470             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
038480*
040520        INITIALIZE 負傷原因内容分解ＸＷ
040530        MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
040540        IF ( 負傷原因内容１ＸＷ  NOT = SPACE )
040550           COMPUTE カウンタ２ = カウンタ２  +  1
040560           MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
040570        END-IF
040580        IF ( 負傷原因内容２ＸＷ  NOT = SPACE )
040590           COMPUTE カウンタ２ = カウンタ２  +  1
040600           MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
040610        END-IF
034690        IF  負傷原因内容３ＸＷ  NOT = SPACE
034700            COMPUTE カウンタ２ = カウンタ２  +  1
034710            MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
034720        END-IF
034690        IF  負傷原因内容４ＸＷ  NOT = SPACE
034700            COMPUTE カウンタ２ = カウンタ２  +  1
034710            MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
034720        END-IF
038630*
038640     END-PERFORM.
038650*
038660*================================================================*
038670 長期理由文取得 SECTION.
038680*================================================================*
038690* 長期理由文取得は "CHOUBUN" を呼ぶ. 
038700     MOVE  SPACE TO  連長文－キー.
038710     INITIALIZE      連長文－キー.
038720     MOVE 施術和暦ＷＲ  TO  連長文－施術和暦.
038730     MOVE 施術年ＷＲ    TO  連長文－施術年.
038740     MOVE 施術月ＷＲ    TO  連長文－施術月.
038750     MOVE 患者番号ＷＲ  TO  連長文－患者番号.
038760     MOVE 枝番ＷＲ      TO  連長文－枝番.
038780     MOVE 56            TO  連長文－文桁数.
038790*
038800     CALL   "CHOUBUN".
038810     CANCEL "CHOUBUN".
038820*
038830*================================================================*
038840 施術ＩＤ取得 SECTION.
038850*================================================================*
038860*********************************************
038870** ＩＤ管理マスタより　県施術ＩＤを取得する。
038880*********************************************
038890**   / 県施術ID /
038900     MOVE 01                     TO ＩＤ管－ＩＤ区分.
038910     MOVE ZERO                   TO ＩＤ管－施術所番号.
038920     MOVE 保険者番号比較Ｗ(1:2)  TO ＩＤ管－保険種別.
038930     MOVE SPACE                  TO ＩＤ管－保険者番号.
038940     READ ＩＤ管理マスタ
038950     NOT INVALID KEY
038960         MOVE ＩＤ管－施術ＩＤ番号   TO 県施術ＩＤＷ
038970     END-READ.
038980*
039090*================================================================*
039100* レセプト回数取得 SECTION.
039110**================================================================*
039120**************************************************************************
039130***-------- レセプトの第 XX 回目 の回数を求める。----------**
039140**  部位の開始年月で、一番小さい(古い)年月と施術年月との差に1を足す
039150**  (例) 開始年月10年7月  で施術年月10年10月は、4回目
039160**  (例) 開始年月10年10月 で施術年月10年10月は、1回目
039170**************************************************************************
039180**
039190*     MOVE ZERO     TO 回数Ｗ.
039200**
039210*     PERFORM 開始年月最小取得.
039220*     PERFORM 差の月取得.
039230*     MOVE 計算月Ｗ TO 回数Ｗ.
039240**
039250**================================================================*
039260* 開始年月最小取得  SECTION.
039270**
039280*** --// 部位の開始年月で、一番小さい(古い)年月を求める. //--**
039290**
039300*     INITIALIZE 最小開始和暦年月Ｗ.
039310** 1部位目と2部位目を比較
039320*     IF ( 負－開始和暦年月(2) NOT = ZERO )
039330*        IF ( 負－開始和暦年月(1)  <  負－開始和暦年月(2) )
039340*           MOVE 負－開始和暦年月(1) TO 最小開始和暦年月Ｗ
039350*        ELSE
039360*           MOVE 負－開始和暦年月(2) TO 最小開始和暦年月Ｗ
039370*        END-IF
039380*     ELSE
039390*        MOVE 負－開始和暦年月(1) TO 最小開始和暦年月Ｗ
039400*     END-IF.
039410** 3部位目以降を比較
039420*     PERFORM VARYING 部位ＣＮＴ FROM 3 BY 1
039430*             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
039440*         IF ( 負－開始和暦年月(部位ＣＮＴ) <  最小開始和暦年月Ｗ )
039450*            MOVE 負－開始和暦年月(部位ＣＮＴ) TO 最小開始和暦年月Ｗ
039460*         END-IF
039470*     END-PERFORM.
039480**
039490**================================================================*
039500 差の月取得  SECTION.
039510*********************************************************** 
039520*   開始年月と施術年月との差の月を求める。
039530*    (前月判定のロジック、セクションを利用)
039540*********************************************************** 
039550*
039560      INITIALIZE  計算年月日Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
039570*
039580      IF ( 最小開始和暦年月Ｗ NOT = ZERO )
039590*
039600          MOVE 施術和暦ＷＲ    TO 終了和暦２Ｗ
039610          MOVE 施術年ＷＲ      TO 終了年２Ｗ
039620          MOVE 施術月ＷＲ      TO 終了月２Ｗ
039630          MOVE 最小開始和暦Ｗ  TO 開始和暦２Ｗ
039640          MOVE 最小開始年Ｗ    TO 開始年２Ｗ
039650          MOVE 最小開始月Ｗ    TO 開始月２Ｗ
039660*
039670          EVALUATE TRUE
039680           WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ = 終了年２Ｗ)
039690                PERFORM  前月比較月
039700           WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ NOT = 終了年２Ｗ)
039710                PERFORM  前月比較年
039720           WHEN  開始和暦２Ｗ NOT = 終了和暦２Ｗ 
039730                PERFORM  前月比較元号
039740          END-EVALUATE
039750*
039760          COMPUTE 計算月Ｗ =  計算月Ｗ + 1
039770*
039780      END-IF.
039790*
039800*================================================================*
039810* 助成印取得 SECTION.
039820**================================================================*
039830*     MOVE SPACE TO 助成印Ｗ 助成印チェックＷ.
039840**
039850*     EVALUATE 助成種別ＷＲ 
039860**** 生保 (生保はその他扱いで、該当なし)
039870*     WHEN  50
039880*         CONTINUE
039970**** 母子
039980*     WHEN  52
040030*            MOVE NC"母"    TO 助成印Ｗ
040050**** 身障
040060*     WHEN  53
040070*            MOVE NC"障"    TO 助成印Ｗ
040110**** 乳幼児 
040120*     WHEN  55
040140*            MOVE NC"子"    TO 助成印Ｗ
040150**** その他
040160*     WHEN  60
040171*         IF 費用負担者番号助成ＷＲ(1:4) = "8923"
040172*             MOVE NC"福"    TO 助成印Ｗ
040173*         END-IF
040180*     WHEN  OTHER
040190*            CONTINUE
040200*     END-EVALUATE.
040210**
040211*     IF (( 保険種別ＷＲ = 05 ) AND ( 保険者番号ＷＲ(1:5) = "39231" ) AND
040212*         ( 受－助成負担金免除 = 1 ))
040213*         MOVE NC"福"    TO 助成印Ｗ
040214*     END-IF.
040215**
040220*     IF ( 助成印Ｗ NOT = SPACE )
040230*        MOVE NC"○" TO 助成印チェックＷ
040240*     END-IF.
040380*
040390*================================================================*
040400* 保険種別編集 SECTION.
040410**================================================================*
040420*     EVALUATE 保険種別Ｗ
040430*     WHEN 1
040440*         IF 受－保険者番号(3:1) = 3
040450*             MOVE NC"国組"   TO 保険種別親Ｗ
040460*         ELSE
040470*             MOVE NC"国"     TO 保険種別親Ｗ
040480*         END-IF
040490*     WHEN 2
040500*         IF (受－保険者番号(1:2) = 01) AND
040510*            (受－保険者番号(5:4) NOT = SPACE)
040520*             MOVE NC"協"     TO 保険種別親Ｗ
040530*         ELSE
040540*             MOVE NC"政"     TO 保険種別親Ｗ
040550*         END-IF
040560*     WHEN 3
040570*         MOVE NC"組"         TO 保険種別親Ｗ
040580*     WHEN 4
040590*         MOVE NC"共"         TO 保険種別親Ｗ
040600*     WHEN 5
040610*         MOVE NC"後期"       TO 保険種別親Ｗ
040620*     WHEN 6
040630*         MOVE NC"日"         TO 保険種別親Ｗ
040640*     WHEN 7
040650*         MOVE NC"船"         TO 保険種別親Ｗ
040660*     WHEN 8
040670*         MOVE NC"国退"       TO 保険種別親Ｗ
040680*     WHEN 9
040690*         MOVE NC"自"         TO 保険種別親Ｗ
040700*     END-EVALUATE.
040710**
016000*     PERFORM 助成印取得.
040720*     IF 助成印Ｗ NOT = SPACE
040730*         STRING 保険種別親Ｗ   DELIMITED BY SPACE
040740*                NC"（"         DELIMITED BY SIZE
040750*                助成印Ｗ       DELIMITED BY SPACE
040760*                NC"）"         DELIMITED BY SIZE
040770*           INTO 保険種別編集Ｗ
040780*         END-STRING
040790*     ELSE
040800*         MOVE 保険種別親Ｗ   TO 保険種別編集Ｗ
040810*     END-IF.
040820**
040830*================================================================*
040840* 給付割合取得 SECTION.
040850**================================================================*
040860*     MOVE ZERO  TO 負担割合Ｗ   給付割合Ｗ.
040870**
      **/負担率取得ＰＧを使うように変更/090404
040880**     COMPUTE 負担割合Ｗ = ( 連計－負担率 / 10 ).
040890**     COMPUTE 給付割合Ｗ = 10 - ( 連計－負担率 / 10 ).
015800*     MOVE SPACE TO 連率－負担率取得キー.
015810*     INITIALIZE 連率－負担率取得キー.
015820*     MOVE 受－施術和暦年月 TO 連率－施術和暦年月.
015830*     MOVE 受－患者コード   TO 連率－患者コード.
015840**
015850*     CALL   "HUTANRIT".
015860*     CANCEL "HUTANRIT".
040880*     COMPUTE 負担割合Ｗ = ( 連率－実際本体負担率 / 10 ).
040890*     COMPUTE 給付割合Ｗ = 10 - ( 連率－実際本体負担率 / 10 ).
040900**
040910*     EVALUATE 給付割合Ｗ
040920*     WHEN 7
040930*        MOVE NC"○"  TO  ７割チェックＷ
040940*     WHEN 8
040950*        MOVE NC"○"  TO  ８割チェックＷ
040960*     WHEN 9
040970*        MOVE NC"○"  TO  ９割チェックＷ
040980*     WHEN 10
040990*        MOVE NC"○"  TO  １０割チェックＷ
041000*     END-EVALUATE.
041010**
040580*================================================================*
040590 摘要文取得 SECTION.
040600*
040610* 摘要文取得は "TEKIYBUN" を呼ぶ. 
040620     MOVE  SPACE TO  連摘文－キー.
040630     INITIALIZE      連摘文－キー.
040640     MOVE 施術和暦ＷＲ  TO  連摘文－施術和暦.
040650     MOVE 施術年ＷＲ    TO  連摘文－施術年.
040660     MOVE 施術月ＷＲ    TO  連摘文－施術月.
040670     MOVE 患者番号ＷＲ  TO  連摘文－患者番号.
040680     MOVE 枝番ＷＲ      TO  連摘文－枝番.
040700*     MOVE 63            TO  連摘文－文桁数.
039370     MOVE 56            TO  連摘文－文桁数.
015000     IF (レセ長期理由印刷区分Ｗ NOT = 1 )
               MOVE 長期理由印刷区分Ｗ TO 連摘文－長期区分
           ELSE
               MOVE 1                  TO 連摘文－長期区分
015050     END-IF.
040710*
040720     CALL   "TEKIYBUN".
040730     CANCEL "TEKIYBUN".
040740*
041290*================================================================*
041300 レセ摘要再セット SECTION.
041310*================================================================*
043230*---------------------------------------------------------------*
043240* 摘要ファイルがあれば長期理由の前に再セットする。
043250* （無ければ何もしない、つまり長期理由はそのまま）
043260*---------------------------------------------------------------*
           PERFORM 摘要文取得.
           MOVE 連摘文－摘要文(1)    TO 長期理由文１.
           MOVE 連摘文－摘要文(2)    TO 長期理由文２.
           MOVE 連摘文－摘要文(3)    TO 長期理由文３.
           MOVE 連摘文－摘要文(4)    TO 長期理由文４.
           MOVE 連摘文－摘要文(5)    TO 長期理由文５.
           MOVE 連摘文－摘要文(6)    TO 長期理由文６.
           MOVE 連摘文－摘要文(7)    TO 長期理由文７.
           MOVE 連摘文－摘要文(8)    TO 長期理由文８.
041490*
044960*================================================================*
044961 負傷原因印刷対象判定処理 SECTION.
044963*------------------------------------------------------------------------------------*
044964* 制御マスタの「負傷原因印刷区分」が 3 （３部位以上印刷）の時、３部位以上か判定して、
044965* その時のみ、負傷原因を印刷する。
044966*------------------------------------------------------------------------------------*
044967*
044979     MOVE  SPACE TO  連レセ負原印－キー.
044980     INITIALIZE      連レセ負原印－キー.
044981     MOVE 施術和暦ＷＲ  TO  連レセ負原印－施術和暦.
044982     MOVE 施術年ＷＲ    TO  連レセ負原印－施術年.
044983     MOVE 施術月ＷＲ    TO  連レセ負原印－施術月.
044984     MOVE 患者番号ＷＲ  TO  連レセ負原印－患者番号.
044985     MOVE 枝番ＷＲ      TO  連レセ負原印－枝番.
044986     CALL   "RECEHUGE".
044987     CANCEL "RECEHUGE".
044989*
044990     IF 連レセ負原印－対象フラグ = "YES"
044991        PERFORM 負傷原因取得
044992     END-IF.
044993*
041500*================================================================*
041510 共済番号セット SECTION.
041520*
041530**************************************************************
041540* 保険者番号により、共済の番号を印字するか、柔整師番号か判定
041550**************************************************************
041560** 1.共済組合連盟
041570     MOVE SPACE  TO  脱出フラグ.
041580     IF ( 施情－共済連番号 NOT = ZERO )
041590** 条件(保険者番号)
041600        IF ( 保険者番号ＷＲ(1:2) = "31" )  OR
041610           ( 保険者番号ＷＲ = "34130021" )
041620*
041630           MOVE  NC"共済組合連盟第"   TO 共済連番号名ＮＷ 
041640           MOVE  NC"号"               TO 共済連番号単位ＮＷ 
041650           MOVE  施情－共済連番号     TO 共済連番号Ｗ
041660           IF    (共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
041670                 MOVE SPACE TO  共済連番号Ｗ(1:1)
041680           ELSE
041690                 MOVE "YES" TO  脱出フラグ
041700           END-IF
041710           IF    (共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
041720                 MOVE SPACE TO  共済連番号Ｗ(2:1)
041730           ELSE
041740                 MOVE "YES" TO  脱出フラグ
041750           END-IF
041760           IF    (共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
041770                 MOVE SPACE TO  共済連番号Ｗ(3:1)
041780           ELSE
041790                 MOVE "YES" TO  脱出フラグ
041800           END-IF
041810           IF    (共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
041820                 MOVE SPACE TO  共済連番号Ｗ(4:1)
041830           ELSE
041840                 MOVE "YES" TO  脱出フラグ
041850           END-IF
041860           IF    (共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
041870                 MOVE SPACE TO  共済連番号Ｗ(5:1)
041880           ELSE
041890                 MOVE "YES" TO  脱出フラグ
041900           END-IF
041910           IF    (共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
041920                 MOVE SPACE TO  共済連番号Ｗ(6:1)
041930           ELSE
041940                 MOVE "YES" TO  脱出フラグ
041950           END-IF
041960           MOVE  共済連番号ＷＰ     TO 県施術ＩＤＷ
041970        END-IF
041980     END-IF.
041990*
042000** 2. 地共済協議会
042010     MOVE SPACE  TO  脱出フラグ.
042020     IF ( 施情－地共済連番号 NOT = ZERO )
042030** 条件(保険者番号)
042040        IF ( 保険者番号ＷＲ(1:2) = "32" OR "33" OR "34" )  AND
042050           ( 保険者番号ＷＲ NOT = "34130021" )
042060*
042070           MOVE  NC"地共済協議会"     TO 共済連番号名ＮＷ 
042080           MOVE  NC"号"               TO 共済連番号単位ＮＷ 
042090           MOVE  施情－地共済連番号   TO 共済連番号Ｗ
042100           IF    (共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
042110                 MOVE SPACE TO  共済連番号Ｗ(1:1)
042120           ELSE
042130                 MOVE "YES" TO  脱出フラグ
042140           END-IF
042150           IF    (共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
042160                 MOVE SPACE TO  共済連番号Ｗ(2:1)
042170           ELSE
042180                 MOVE "YES" TO  脱出フラグ
042190           END-IF
042200           IF    (共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
042210                 MOVE SPACE TO  共済連番号Ｗ(3:1)
042220           ELSE
042230                 MOVE "YES" TO  脱出フラグ
042240           END-IF
042250           IF    (共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
042260                 MOVE SPACE TO  共済連番号Ｗ(4:1)
042270           ELSE
042280                 MOVE "YES" TO  脱出フラグ
042290           END-IF
042300           IF    (共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
042310                 MOVE SPACE TO  共済連番号Ｗ(5:1)
042320           ELSE
042330                 MOVE "YES" TO  脱出フラグ
042340           END-IF
042350           IF    (共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
042360                 MOVE SPACE TO  共済連番号Ｗ(6:1)
042370           ELSE
042380                 MOVE "YES" TO  脱出フラグ
042390           END-IF
042400           MOVE  共済連番号ＷＰ     TO 県施術ＩＤＷ
042410        END-IF
042420     END-IF.
042430*
042440*================================================================*
042450 自衛官番号セット SECTION.
042460*
042470     MOVE SPACE  TO  脱出フラグ.
042480     IF ( 施情－自衛官番号 NOT = ZERO )
042490           IF 施情－防衛省区分 = 1
042500              MOVE  NC"防衛省第"      TO 自衛官番号名ＮＷ 
042510           ELSE
042520              MOVE  NC"防衛庁第"      TO 自衛官番号名ＮＷ 
042530           END-IF
042540*           MOVE  NC"防衛庁第"         TO 自衛官番号名ＮＷ 
042550           MOVE  NC"号"               TO 自衛官番号単位ＮＷ 
042560           MOVE  施情－自衛官番号     TO 自衛官番号Ｗ
042570           IF    (自衛官番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
042580                 MOVE SPACE TO  自衛官番号Ｗ(1:1)
042590           ELSE
042600                 MOVE "YES" TO  脱出フラグ
042610           END-IF
042620           IF    (自衛官番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
042630                 MOVE SPACE TO  自衛官番号Ｗ(2:1)
042640           ELSE
042650                 MOVE "YES" TO  脱出フラグ
042660           END-IF
042670           IF    (自衛官番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
042680                 MOVE SPACE TO  自衛官番号Ｗ(3:1)
042690           ELSE
042700                 MOVE "YES" TO  脱出フラグ
042710           END-IF
042720           IF    (自衛官番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
042730                 MOVE SPACE TO  自衛官番号Ｗ(4:1)
042740           ELSE
042750                 MOVE "YES" TO  脱出フラグ
042760           END-IF
042770           IF    (自衛官番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
042780                 MOVE SPACE TO  自衛官番号Ｗ(5:1)
042790           ELSE
042800                 MOVE "YES" TO  脱出フラグ
042810           END-IF
042820           IF    (自衛官番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
042830                 MOVE SPACE TO  自衛官番号Ｗ(6:1)
042840           ELSE
042850                 MOVE "YES" TO  脱出フラグ
042860           END-IF
042870           MOVE  自衛官番号ＷＰ     TO 県施術ＩＤＷ
042880     END-IF.
042890*
042900*================================================================*
042910 施術記録Ｆ読込 SECTION.
042920*================================================================*
042930*
042940     READ 施術記録Ｆ NEXT
042950     AT END
042960         MOVE "YES" TO 終了フラグ２
042970     END-READ.
042980*
042990*----------------------------------------------------------------*
043000*================================================================*
043010 印刷処理 SECTION.
043020*================================================================*
043030     MOVE "YCB6125P" TO  定義体名Ｐ.
043040     MOVE "SCREEN"  TO  項目群名Ｐ.
043050     WRITE YCB6125P.
043060***     WRITE 印刷レコード.
043070     PERFORM エラー処理Ｐ.
043080*================================================================*
043090 エラー処理Ｐ SECTION.
043100*
043110     IF 通知情報Ｐ NOT = "00"
043120         DISPLAY NC"帳票エラー"              UPON CONS
043130         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
043140         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
043150         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
043160         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
043170                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
043180         ACCEPT  キー入力 FROM CONS
043190         PERFORM ファイル閉鎖
043200         MOVE 99  TO PROGRAM-STATUS
043210         EXIT PROGRAM
043220     END-IF.
043230*
043240*=== 終了処理 ===================================================*
043250*================================================================*
043260 受診者印刷区分更新 SECTION.
043270*================================================================*
043280** //  受診者情報Ｆの印刷区分に１をセットし、更新する。//  
043290*
043300     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
043310     MOVE 施術年ＷＲ         TO 受－施術年.
043320     MOVE 施術月ＷＲ         TO 受－施術月.
043330     MOVE 患者コードＷＲ     TO 受－患者コード.
043340     READ 受診者情報Ｆ
043350     NOT INVALID KEY
043360         MOVE  1  TO  受－レセ印刷区分
043370         REWRITE  受－レコード
043380         END-REWRITE
043390         IF ( 状態キー NOT = "00" )
043400            MOVE NC"受診者" TO ファイル名
043410            PERFORM エラー表示
043420         END-IF
043430     END-READ.
043440*
043450*================================================================*
043460 終了処理 SECTION.
043470*================================================================*
043480     PERFORM ファイル閉鎖.
043490*
043500*================================================================*
043510 ファイル閉鎖 SECTION.
043520*
043530     CLOSE 元号マスタ     名称マスタ       レセプトＦ     経過マスタ
043540           制御情報マスタ 施術所情報マスタ 会情報マスタ
043550           保険者マスタ   ＩＤ管理マスタ   市町村マスタ
043560           受診者情報Ｆ   施術記録Ｆ       負傷データＦ   負傷原因Ｆ
043570           作業ファイル２.
043580     CLOSE 印刷ファイル.
043590*
043600*================================================================*
043610*================================================================*
043620 エラー表示 SECTION.
043630*
043640     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
043650     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
043660     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
043670     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
043680                                                   UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
043690     ACCEPT  キー入力 FROM CONS
043700     PERFORM ファイル閉鎖.
043710     EXIT PROGRAM.
043720*
043730*================================================================*
043750 テスト印字処理 SECTION.
044930*
           MOVE ALL "9" TO
           都道府県番号 施術月 施術年 患者年 患者月 患者日 開始年１ 開始月１ 開始日１ 終了年１ 
           終了月１ 終了日１ 負傷年１ 負傷月１ 負傷日１ 初検年１ 初検月１ 初検日１ 実日数１ 
           開始年２ 開始月２ 開始日２ 終了年２ 終了月２ 終了日２ 負傷年２ 負傷月２ 負傷日２ 
           初検年２ 初検月２ 初検日２ 実日数２ 開始年３ 開始月３ 開始日３ 終了年３ 終了月３ 
           終了日３ 負傷年３ 負傷月３ 負傷日３ 初検年３ 初検月３ 初検日３ 実日数３ 開始年４ 
           開始月４ 開始日４ 終了年４ 終了月４ 終了日４ 負傷年４ 負傷月４ 負傷日４ 初検年４ 
           初検月４ 初検日４ 実日数４ 開始年５ 開始月５ 開始日５ 終了年５ 終了月５ 終了日５ 
           負傷年５ 負傷月５ 負傷日５ 初検年５ 初検月５ 初検日５ 実日数５ 初検料 初検時相談料 
           往療距離 再検料 金属副子加算料 往療回数 往療料 初検加算料 施術情報提供料 往療加算料 
           初検加算時 初検加算分 初検加算区切  小計 初回処置料合計 後療単価１ 
           初回処置料(1) 初回処置料(2) 初回処置料(3) 初回処置料(4) 初回処置料(5)
           後療回数１ 後療料１ 冷罨法回数１ 冷罨法料１ 温罨法回数１ 温罨法料１ 電療回数１ 
           電療料１ 小計１ 長期逓減率１ 長期込小計１ 後療単価２ 後療回数２ 後療料２ 冷罨法回数２ 
           冷罨法料２ 温罨法回数２ 温罨法料２ 電療回数２ 電療料２ 小計２ 長期逓減率２ 
           長期込小計２ 後療単価３８ 後療回数３８ 後療料３８ 冷罨法回数３８ 冷罨法料３８ 
           温罨法回数３８ 温罨法料３８ 電療回数３８ 電療料３８ 小計３８ 多部位込小計３８ 
           長期逓減率３８ 長期込小計３８ 逓減開始月３０ 逓減開始日３０ 後療単価３０ 後療回数３０ 
           後療料３０ 冷罨法回数３０ 冷罨法料３０ 温罨法回数３０ 温罨法料３０ 電療回数３０ 
           電療料３０ 小計３０ 長期逓減率３０ 長期込小計３０ 逓減開始月４８ 逓減開始日４８ 
           後療単価４８ 後療回数４８ 後療料４８ 冷罨法回数４８ 冷罨法料４８ 温罨法回数４８ 
           温罨法料４８ 電療回数４８ 電療料４８ 小計４８ 多部位込小計４８ 長期逓減率４８ 
           長期込小計４８ 逓減開始月４０ 逓減開始日４０ 後療単価４０ 後療回数４０ 後療料４０ 
           冷罨法回数４０ 冷罨法料４０ 温罨法回数４０ 温罨法料４０ 電療回数４０ 電療料４０ 
           小計４０ 長期逓減率４０ 長期込小計４０ 合計 一部負担金 負担割合 請求金額 
           受理年 受理月 受理日 委任年 委任月 委任日
           .
           MOVE ALL "X" TO
           県施術ＩＤ 保険者番号 公費負担者番号 受給者番号 
           金融機関名１ 金融機関名２ 金融機関名３ 金融機関名４ 支店名１ 支店名２ 支店名３ 
           支店名４ 口座名義人カナ 口座名義人 柔整師番号 口座番号 施術所郵便番号１  
           施術所郵便番号２ 施術所住所１ 施術所住所２ 施術所電話番号 代表者カナ
           負傷原因１ 負傷原因２ 負傷原因３ 負傷原因４ 負傷原因５ 負傷原因６
      *
           MOVE ALL NC"Ｎ" TO
           負傷名１ 負傷名２ 負傷名３ 負傷名４ 負傷名５ 経過略称(1) 
           経過略称(2) 経過略称(3) 経過略称(4) 経過略称(5) 適用１ 適用２
           .
           MOVE ALL "静" TO
           長期理由文１ 長期理由文２ 長期理由文３ 長期理由文４ 長期理由文５ 長期理由文６ 
           接骨院名 被保険者氏名 患者氏名 代表者名 
           .
      *
           MOVE NC"○" TO
           普通チェック 振込チェック 当座チェック 本店チェック 支店チェック 本支所チェック 
           銀行チェック 金庫チェック 農協チェック 深夜チェック 時間外チェック 
           休日チェック 固定料チェック 整復料チェック 施療料チェック 夜間チェック 暴風雨雪チェック 
           難路チェック 大チェック 中チェック 小チェック 治癒チェック１ 中止チェック１ 転医チェック１ 
           治癒チェック２ 中止チェック２ 転医チェック２ 治癒チェック３ 中止チェック３ 転医チェック３ 
           治癒チェック４ 中止チェック４ 転医チェック４ 治癒チェック５ 中止チェック５ 転医チェック５ 
           新規チェック 継続チェック 男チェック 明治チェック 大正チェック 女チェック 昭和チェック 
           平成チェック 単独チェック 本人チェック 高一チェック 共済チェック 自チェック 社保チェック 
           組合チェック １０割チェック ９割チェック ２併チェック ６歳チェック ８割チェック ７割チェック 
           後期チェック 退職チェック 国保チェック 家族チェック 高７チェック
           .
044940*================================================================*
       施術日取得 SECTION.
      *
028350     MOVE 患者番号ＷＲ          TO 施記－患者番号
028360     MOVE 枝番ＷＲ              TO 施記－枝番
028370     MOVE 施術和暦ＷＲ          TO 施記－施術和暦
028380     MOVE 施術年ＷＲ            TO 施記－施術年
028390     MOVE 施術月ＷＲ            TO 施記－施術月
028400     MOVE ZERO                  TO 施記－施術日
028420     START 施術記録Ｆ   KEY IS >= 施記－患者コード
028430                                  施記－施術和暦年月日
028440     END-START
028450     IF 状態キー = "00"
030910         MOVE SPACE TO 終了フラグ２
030920         PERFORM 施術記録Ｆ読込
030930         PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
030940                       ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
030950                       ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
030960                       ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
030970                       ( 施記－施術月     NOT = 施術月ＷＲ      )
                   MOVE NC"○" TO 施術日チェック(施記－施術日)
                   PERFORM 施術記録Ｆ読込
               END-PERFORM
           END-IF.
           PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 31
               MOVE カウンタ TO 施術日(カウンタ)
           END-PERFORM.
037520*================================================================*
043420 レセプト並び順取得 SECTION.
043430*
043440     MOVE 施術和暦ＷＲ       TO 作２－施術和暦.
043450     MOVE 施術年ＷＲ         TO 作２－施術年.
043460     MOVE 施術月ＷＲ         TO 作２－施術月.
043470     MOVE 患者コードＷＲ     TO 作２－患者コード.
043480     MOVE 保険種別ＷＲ       TO 作２－保険種別.
043490     READ 作業ファイル２
043500     NOT INVALID KEY
043510          MOVE 作２－順番    TO 順番Ｗ
043520     END-READ.
043530*
043540*================================================================*
044950******************************************************************
044960 END PROGRAM YCB6125.
044970******************************************************************
