000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCB6425.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*     中部柔整師協会 助成レセプト印刷（柔+ｳｨﾝﾄﾞｳｽﾞ版）
000100*         MED = YAW610 YCB6425P
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
000650     SELECT  保険者マスタ    ASSIGN      TO        HOKENSL
000660                             ORGANIZATION             IS  INDEXED
000670                             ACCESS MODE              IS  DYNAMIC
000680                             RECORD KEY               IS  保－保険種別
000690                                                          保－保険者番号
000700* 将来は、キー項目の保険者名称を保険者カナにする
000710                             ALTERNATE RECORD KEY     IS  保－保険種別
000720                                                          保－保険者名称
000730                                                          保－保険者番号
000740                             FILE STATUS              IS  状態キー
000750                             LOCK        MODE         IS  AUTOMATIC.
000760     SELECT  請求先マスタ    ASSIGN      TO        SEIKYUSL
000770                             ORGANIZATION             IS  INDEXED
000780                             ACCESS MODE              IS  DYNAMIC
000790                             RECORD KEY               IS  請先－保険種別
000800                                                          請先－保険者番号
000810                             FILE STATUS              IS  状態キー
000820                             LOCK    MODE             IS  AUTOMATIC.
000830     SELECT  ＩＤ管理マスタ    ASSIGN      TO      IDKANRL
000840                             ORGANIZATION             IS  INDEXED
000850                             ACCESS MODE              IS  DYNAMIC
000860                             RECORD KEY               IS  ＩＤ管－ＩＤ区分
000870                                                          ＩＤ管－施術所番号
000880                                                          ＩＤ管－保険種別
000890                                                          ＩＤ管－保険者番号
000900                             ALTERNATE RECORD KEY     IS  ＩＤ管－施術ＩＤ番号
000910                                                          ＩＤ管－ＩＤ区分
000920                                                          ＩＤ管－施術所番号
000930                                                          ＩＤ管－保険種別
000940                                                          ＩＤ管－保険者番号
000950                             FILE STATUS              IS  状態キー
000960                             LOCK        MODE         IS  AUTOMATIC.
000970     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
000980                             ORGANIZATION             IS  INDEXED
000990                             ACCESS MODE              IS  DYNAMIC
001000                             RECORD KEY               IS  市－公費種別
001010                                                          市－市町村番号
001020                             ALTERNATE RECORD KEY     IS  市－公費種別
001030                                                          市－市町村名称
001040                                                          市－市町村番号
001050                             FILE STATUS              IS  状態キー
001060                             LOCK        MODE         IS  AUTOMATIC.
001070     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
001080                             ORGANIZATION             IS  INDEXED
001090                             ACCESS MODE              IS  DYNAMIC
001100                             RECORD KEY               IS  受－施術和暦年月
001110                                                          受－患者コード
001120                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001130                                                          受－患者カナ
001140                                                          受－患者コード
001150                             ALTERNATE RECORD KEY     IS  受－患者コード
001160                                                          受－施術和暦年月
001170                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001180                                                          受－保険種別
001190                                                          受－保険者番号
001200                                                          受－患者コード
001210                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001220                                                          受－公費種別
001230                                                          受－費用負担者番号
001240                                                          受－患者コード
001250                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001260                                                          受－助成種別
001270                                                          受－費用負担者番号助成
001280                                                          受－患者コード
001290                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
001300                                                          受－施術和暦年月
001310                                                          受－患者コード
001320                             FILE STATUS              IS  状態キー
001330                             LOCK        MODE         IS  AUTOMATIC.
000180     SELECT  受診者情報２Ｆ  ASSIGN      TO        JUSINJ2L
000190                             ORGANIZATION             IS INDEXED
000200                             ACCESS MODE              IS DYNAMIC
000210                             RECORD KEY               IS 受２－施術和暦年月
000220                                                         受２－患者コード
000230                             ALTERNATE RECORD KEY     IS 受２－請求対象区分
000240                                                         受２－請求和暦年月
000250                                                         受２－施術和暦年月
000260                                                         受２－患者コード
000270                             ALTERNATE RECORD KEY     IS 受２－助成請求対象区分
000280                                                         受２－助成請求和暦年月
000290                                                         受２－施術和暦年月
000300                                                         受２－患者コード
000310                             FILE STATUS              IS  状態キー
000320                             LOCK        MODE         IS  AUTOMATIC.
001340     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
001350                             ORGANIZATION             IS  INDEXED
001360                             ACCESS MODE              IS  DYNAMIC
001370                             RECORD KEY               IS  施記－施術和暦年月日
001380                                                          施記－患者コード
001390                             ALTERNATE RECORD KEY     IS  施記－患者コード
001400                                                          施記－施術和暦年月日
001410                             FILE STATUS              IS  状態キー
001420                             LOCK        MODE         IS  AUTOMATIC.
001430     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
001440                             ORGANIZATION             IS  INDEXED
001450                             ACCESS MODE              IS  DYNAMIC
001460                             RECORD KEY               IS  負－施術和暦年月
001470                                                          負－患者コード
001480                             ALTERNATE RECORD KEY     IS  負－患者コード
001490                                                          負－施術和暦年月
001500                             FILE STATUS              IS  状態キー
001510                             LOCK        MODE         IS  AUTOMATIC.
001520     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
001530                             ORGANIZATION             IS  INDEXED
001540                             ACCESS MODE              IS  DYNAMIC
001550                             RECORD KEY               IS  負原－区分コード
001560                                                          負原－負傷原因コード
001570                             FILE STATUS              IS  状態キー
001580                             LOCK        MODE         IS  AUTOMATIC.
001860* 並び順印字用
001870     SELECT  作業ファイル２  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001880                             ORGANIZATION             IS  INDEXED
001890                             ACCESS                   IS  DYNAMIC
001900                             RECORD      KEY          IS  作２－施術和暦年月
001910                                                          作２－患者コード
001920                                                          作２－保険種別
001930                             FILE        STATUS       IS  状態キー
001940                             LOCK        MODE         IS  AUTOMATIC.
001730     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF002
001740                             SYMBOLIC    DESTINATION  IS "PRT"
001750                             FORMAT                   IS  定義体名Ｐ
001760                             GROUP                    IS  項目群名Ｐ
001770                             PROCESSING  MODE         IS  処理種別Ｐ
001780                             UNIT        CONTROL      IS  拡張制御Ｐ
001790                             FILE        STATUS       IS  通知情報Ｐ.
001800******************************************************************
001810*                      DATA DIVISION                             *
001820******************************************************************
001830 DATA                    DIVISION.
001840 FILE                    SECTION.
001850*                           ［ＲＬ＝  １２８］
001860 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001870     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001880*                           ［ＲＬ＝  １２８］
001890 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
001900     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
001940*                           ［ＲＬ＝  １２８］
001950 FD  経過マスタ          BLOCK   CONTAINS   1   RECORDS.
001960     COPY KEIKA           OF  XFDLIB  JOINING   経   AS  PREFIX.
001970*                           ［ＲＬ＝  ２５６］
001980 FD  制御情報マスタ      BLOCK   CONTAINS   1   RECORDS.
001990     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
002000*                           ［ＲＬ＝  １２８］
002010 FD  施術所情報マスタ    BLOCK   CONTAINS   1   RECORDS.
002020     COPY SEJOHO          OF  XFDLIB  JOINING   施情 AS  PREFIX.
002150*                           ［ＲＬ＝  ６４０］
002160 FD  会情報マスタ        BLOCK   CONTAINS   1   RECORDS.
002170     COPY KAIJOHO         OF  XFDLIB  JOINING   会情 AS  PREFIX.
002030*                           ［ＲＬ＝  ３２０］
002040 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
002050     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
002060*                           ［ＲＬ＝  １２８］
002070 FD  請求先マスタ        BLOCK   CONTAINS   1   RECORDS.
002080     COPY SEIKYUS         OF  XFDLIB  JOINING   請先 AS  PREFIX.
002090*                           ［ＲＬ＝  １２８］
002100 FD  ＩＤ管理マスタ      BLOCK   CONTAINS   1   RECORDS.
002110     COPY IDKANR          OF  XFDLIB  JOINING   ＩＤ管 AS  PREFIX.
002120*                           ［ＲＬ＝  ２５６］
002130 FD  市町村マスタ        BLOCK   CONTAINS   1   RECORDS.
002140     COPY SITYOSN         OF  XFDLIB  JOINING   市   AS  PREFIX.
002150*                           ［ＲＬ＝  ３２０］
002160 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
002170     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
002560*                          ［ＲＬ＝  1024］
000340 FD  受診者情報２Ｆ        BLOCK   CONTAINS   1   RECORDS.
000350     COPY JUSINJ2          OF  XFDLIB  JOINING   受２   AS  PREFIX.
002180*                           ［ＲＬ＝  ２５６］
002190 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
002200     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
002210*                           ［ＲＬ＝  １２８］
002220 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002230     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
002240*                           ［ＲＬ＝  １２８］
002250 FD  負傷原因Ｆ         BLOCK   CONTAINS    1   RECORDS.
002260     COPY HUGEIN          OF  XFDLIB  JOINING   負原 AS  PREFIX.
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
002300*
002310 FD  印刷ファイル.
002320     COPY YCB6425P         OF  XMDLIB.
002330*----------------------------------------------------------------*
002340******************************************************************
002350*                WORKING-STORAGE SECTION                         *
002360******************************************************************
002370 WORKING-STORAGE         SECTION.
002380 01 キー入力                           PIC X     VALUE SPACE.
002390 01 状態キー                           PIC X(2)  VALUE SPACE.
002400 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002410 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002420 01 ファイル名                         PIC N(6)  VALUE SPACE.
002430 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002440*
002450*--- 制御マスタ退避 ---*
002460 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
002470*
002480** 負傷原因・長期理由印刷区分用
002490 01 負傷原因印刷区分Ｗ                 PIC 9     VALUE ZERO.
002500 01 長期理由印刷区分Ｗ                 PIC 9     VALUE ZERO.
002510*
002520** レセ下段の日付区分用 (0:最終通院日、1:月末日、9:印字なし)
002530 01 レセプト日付区分Ｗ                 PIC 9     VALUE ZERO.
002540 01 レセプト患者日付区分Ｗ             PIC 9     VALUE ZERO.
002550*
002560*--- カウンタ ---*
002570 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002580*
002590*--- 負傷データ取得用 ---*
002600 01 負傷名称Ｗ                         PIC N(10) VALUE SPACE.
002610 01 部位名称Ｗ                         PIC N(20) VALUE SPACE.
002620 01 部位長Ｗ                           PIC 9(2)  VALUE 1.
002630 01 経過部位Ｗ                         PIC N(1)  VALUE SPACE.
002640*
002650** 枝番判定用
002660 01 開始診療日手動区分Ｗ               PIC 9     VALUE ZERO.
002670*
002680* 負傷原因印刷区分
002690 01 レセ負傷原因印刷区分Ｗ             PIC 9     VALUE ZERO.
002580 01 レセ長期理由印刷区分Ｗ             PIC 9    VALUE ZERO.
002700*
002710*--- 施術記録取得用 ---*
002720 01 初日再検フラグ                     PIC X(3)  VALUE SPACE.
002730 01 前月フラグ                         PIC X(3)  VALUE SPACE.
002740*
002750 01 終了年月日ＷＴ.
002760    03 終了年ＷＴ                      PIC 9(2)  VALUE ZERO.
002770    03 終了月ＷＴ                      PIC 9(2)  VALUE ZERO.
002780    03 終了日ＷＴ                      PIC 9(2)  VALUE ZERO.
002790*
002800** 前月判定用
002810 01 計算年月日Ｗ.
002820    03 計算和暦Ｗ                      PIC 9(1)  VALUE ZERO.
002830    03 計算年Ｗ                        PIC S9(2) VALUE ZERO.
002840    03 計算月Ｗ                        PIC S9(2) VALUE ZERO.
002850    03 計算日Ｗ                        PIC S9(2) VALUE ZERO.
002860 01 開始年月日２Ｗ.
002870    03 開始和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
002880    03 開始年２Ｗ                      PIC 9(2)  VALUE ZERO.
002890    03 開始月２Ｗ                      PIC 9(2)  VALUE ZERO.
002900    03 開始日２Ｗ                      PIC 9(2)  VALUE ZERO.
002910    03 開始西暦年Ｗ                    PIC S9(4) VALUE ZERO.
002920 01 終了年月日２Ｗ.
002930    03 終了和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
002940    03 終了年２Ｗ                      PIC 9(2)  VALUE ZERO.
002950    03 終了月２Ｗ                      PIC 9(2)  VALUE ZERO.
002960    03 終了日２Ｗ                      PIC 9(2)  VALUE ZERO.
002970    03 終了西暦年Ｗ                    PIC S9(4) VALUE ZERO.
002980*
002990*--- 初検日退避用 ---*
003000 01 初検フラグ                         PIC X(3)  VALUE SPACE.
003010*
003020 01 初検年月日ＷＴ.
003030    03 初検和暦ＷＴ                    PIC 9     VALUE ZERO.
003040    03 初検年ＷＴ                      PIC 9(2)  VALUE ZERO.
003050    03 初検月ＷＴ                      PIC 9(2)  VALUE ZERO.
003060    03 初検日ＷＴ                      PIC 9(2)  VALUE ZERO.
003070*
003080*--- 初検加算時刻用 ---*
003090 01 初検加算ＷＴ.
003100    03 初検加算カウント                PIC 9     VALUE ZERO.
003110    03 番号カウンタ                    PIC 9     VALUE ZERO.
003120    03 初検加算集団ＷＴ  OCCURS 3.
003130       05 初検加算区分ＷＴ             PIC 9     VALUE ZERO.
003140       05 初検加算時ＷＴ               PIC 9(2)  VALUE ZERO.
003150       05 初検加算分ＷＴ               PIC 9(2)  VALUE ZERO.
003160    03 初検加算集団ＮＷ  OCCURS 3.
003170       05 加算区切Ｗ                   PIC N(1)  VALUE SPACE.
003180       05 加算内容Ｗ                   PIC N(3)  VALUE SPACE.
003190       05 初検加算時ＮＷ１             PIC N(1)  VALUE SPACE.
003200       05 初検加算時ＮＷ２             PIC N(1)  VALUE SPACE.
003210       05 時固定Ｗ                     PIC N(1)  VALUE SPACE.
003220       05 初検加算分ＮＷ１             PIC N(1)  VALUE SPACE.
003230       05 初検加算分ＮＷ２             PIC N(1)  VALUE SPACE.
003240       05 分固定Ｗ                     PIC N(1)  VALUE SPACE.
003250    03 初検加算時刻１Ｗ                PIC N(10) VALUE SPACE.
003260    03 初検加算時刻２Ｗ                PIC N(10) VALUE SPACE.
003270    03 初検加算時刻３Ｗ                PIC N(10) VALUE SPACE.
003070    03 初検加算区切Ｗ                  PIC X     VALUE SPACE.
003080    03 初検加算時Ｗ                    PIC 9(2)  VALUE ZERO.
003090    03 初検加算分Ｗ                    PIC 9(2)  VALUE ZERO.
003280*
003290** 数字→日本語変換
003300 01 数字Ｗ                             PIC 9(2).
003310 01 数字Ｒ REDEFINES 数字Ｗ.
003320    03 数字Ｗ１                        PIC X(1).
003330    03 数字Ｗ２                        PIC X(1).
003340*
003350 01 負傷番号Ｗ                         PIC 9.
003360 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
003370    03 負傷番号Ｗ１                    PIC X.
003380*
003390 01 全角負傷番号Ｗ                     PIC N.
003400 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
003410    03 全角負傷番号Ｗ１                PIC X(2).
003420*
003430*--- 負傷原因用 ---*
003440 01 カウンタ                           PIC 9(2)  VALUE ZERO.
003450 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
003460 01 負傷原因ＷＴ.
003470    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
003480    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
003490    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
003500    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
003510    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
003520    03 負傷原因ナンバーＷＴ.
003530       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
003540    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
003550 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
003560 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
003570 01 負傷原因ＴＢＬ.
003580    03 負傷原因コードＴＢＬ            OCCURS 9.
003590       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
003600       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
003610       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
003620 01 負傷原因内容Ｗ.
003630    03 負傷原因内容合成Ｗ              PIC X(318) OCCURS 9 VALUE SPACE.
003620    03 負傷原因内容分解ＸＷ.
003630       05 負傷原因内容１ＸＷ           PIC X(80)  VALUE SPACE.
003640       05 負傷原因内容２ＸＷ           PIC X(80)  VALUE SPACE.
003640       05 負傷原因内容３ＸＷ           PIC X(80)  VALUE SPACE.
003650       05 負傷原因内容４ＸＷ           PIC X(78)  VALUE SPACE.
003800 01 負傷原因ＷＰ                       PIC N(225) VALUE SPACE.
       01 負傷原因ＷＲＰ.
003810    03 負傷原因ＷＲ                    PIC N(45) OCCURS 5 VALUE SPACE.
003680*
003690*--- 委任年月日用 ---*
003700 01 受理年月日Ｗ.
003710    03 受理年Ｗ                        PIC 9(2)  VALUE ZERO.
003720    03 受理月Ｗ                        PIC 9(2)  VALUE ZERO.
003730    03 受理日Ｗ                        PIC 9(2)  VALUE ZERO.
003740 01 最終通院年月日Ｗ.
003750    03 最終通院年Ｗ                    PIC 9(2)  VALUE ZERO.
003760    03 最終通院月Ｗ                    PIC 9(2)  VALUE ZERO.
003770    03 最終通院日Ｗ                    PIC 9(2)  VALUE ZERO.
003780** 月末日用
003790 01 施術西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
003800 01 商Ｗ                               PIC 9(3)  VALUE ZERO.
003810 01 余Ｗ                               PIC 9(3)  VALUE ZERO.
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
003820*
003830*--- 負担給付割合用 ---*
003840 01 負担割合Ｗ                         PIC 9(2)  VALUE ZERO.
003850 01 給付割合Ｗ                         PIC 9(2)  VALUE ZERO.
003860 01 負担率Ｗ                           PIC 9(3)  VALUE ZERO.
003870*
003880*--- レセプト回数用 ---*
003890 01 回数Ｗ                             PIC 9(2)  VALUE ZERO.
003900*
003910 01 最小開始和暦年月Ｗ.
003920    03 最小開始和暦Ｗ                  PIC 9(1)  VALUE ZERO.
003930    03 最小開始年Ｗ                    PIC 9(2)  VALUE ZERO.
003940    03 最小開始月Ｗ                    PIC 9(2)  VALUE ZERO.
003950*
003960*--- 施術ＩＤ用 ---*
003970 01 施術ＩＤ固定Ｗ                     PIC X(14) VALUE "施術機関番号：".
      *
      */金属副子・運動後療の変更・追加/1805
       01 金属副子ＣＭ                       PIC X(140) VALUE SPACE.
       01 運動後療ＣＭ                       PIC X(68)  VALUE SPACE.
003980*
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
004410*
004420****************
004430* 連結項目待避 *
004440****************
004450*    ************
004460*    * 印刷キー *
004470*    ************
004480 01 対象データＷＲ.
004490    03 施術和暦年月ＷＲ.
004500       05 施術和暦ＷＲ                 PIC 9(1)  VALUE ZERO.
004510       05 施術年ＷＲ                   PIC 9(2)  VALUE ZERO.
004520       05 施術月ＷＲ                   PIC 9(2)  VALUE ZERO.
004530    03 保険種別ＷＲ                    PIC 9(2)  VALUE ZERO.
004540    03 保険者番号ＷＲ                  PIC X(10) VALUE SPACE.
004550    03 公費種別ＷＲ                    PIC 9(2)  VALUE ZERO.
004560    03 費用負担者番号ＷＲ              PIC X(10) VALUE SPACE.
004570    03 助成種別ＷＲ                    PIC 9(2)  VALUE ZERO.
004580    03 費用負担者番号助成ＷＲ          PIC X(10) VALUE SPACE.
004590    03 本人家族区分ＷＲ                PIC 9(1)  VALUE ZERO.
004600    03 患者カナＷＲ                    PIC X(50) VALUE SPACE.
004610    03 患者コードＷＲ.
004620       05 患者番号ＷＲ                 PIC 9(6)  VALUE ZERO.
004630       05 枝番ＷＲ                     PIC X(1)  VALUE SPACE.
004640*    ************
004650*    * 料金情報 *
004660*    ************
004670*    月毎の料金
004680***********************
004690 01 料金１ＷＲ.
004700   03 初検ＷＲ.
004710      05 負担割合ＷＲ                  PIC 9(3)  VALUE ZERO.
004720      05 初検料ＷＲ                    PIC 9(5)  VALUE ZERO.
004730      05 初検加算料ＷＲ                PIC 9(5)  VALUE ZERO.
         03 相談料ＷＲ                       PIC 9(4)  VALUE ZERO.
004740   03 再検料ＷＲ                       PIC 9(5)  VALUE ZERO.
004750   03 往療ＷＲ.
004760      05 往療距離ＷＲ                  PIC 9(2)V9 VALUE ZERO.
004770      05 往療回数ＷＲ                  PIC 9(2)  VALUE ZERO.
004780      05 往療料ＷＲ                    PIC 9(5)  VALUE ZERO.
004790      05 往療加算料ＷＲ                PIC 9(5)  VALUE ZERO.
004800   03 金属副子加算料ＷＲ               PIC 9(5)  VALUE ZERO.
004810   03 施術情報提供料ＷＲ               PIC 9(5)  VALUE ZERO.
004820   03 合計ＷＲ                         PIC 9(6)  VALUE ZERO.
004830   03 一部負担金ＷＲ                   PIC 9(6)  VALUE ZERO.
004840   03 請求金額ＷＲ                     PIC 9(6)  VALUE ZERO.
004850   03 給付割合ＷＲ                     PIC 9(1)  VALUE ZERO.
004860   03 受給者負担額ＷＲ                 PIC 9(6)  VALUE ZERO.
004870   03 助成請求金額ＷＲ                 PIC 9(6)  VALUE ZERO.
004880*
004890* 負傷部位毎の料金
004900***********************
004910 01 料金２ＷＲ.
004920   03 初回処置ＷＲ    OCCURS   9.
004930      05 初回処置料ＷＲ                PIC 9(5)  VALUE ZERO.
004940*
004950* 逓減毎の料金
004960***********************
004970 01 料金３ＷＲ.
004980**********
004990* １部位 *
005000**********
005010   03 部位１ＷＲ.
005020      05 後療１ＷＲ.
005030         07 後療単価１ＷＲ             PIC 9(4)  VALUE ZERO.
005040         07 後療回数１ＷＲ             PIC 9(2)  VALUE ZERO.
005050         07 後療料１ＷＲ               PIC 9(5)  VALUE ZERO.
005060      05 冷罨法１ＷＲ.
005070         07 冷罨法回数１ＷＲ           PIC 9(2)  VALUE ZERO.
005080         07 冷罨法料１ＷＲ             PIC 9(4)  VALUE ZERO.
005090      05 温罨法１ＷＲ.
005100         07 温罨法回数１ＷＲ           PIC 9(2)  VALUE ZERO.
005110         07 温罨法料１ＷＲ             PIC 9(4)  VALUE ZERO.
005120      05 電療１ＷＲ.
005130         07 電療回数１ＷＲ             PIC 9(2)  VALUE ZERO.
005140         07 電療料１ＷＲ               PIC 9(4)  VALUE ZERO.
005150      05 小計１ＷＲ                    PIC 9(6)  VALUE ZERO.
005160      05 長期逓減率１ＷＲ              PIC 9(3)  VALUE ZERO.
005170      05 長期込小計１ＷＲ              PIC 9(6)  VALUE ZERO.
005180**********
005190* ２部位 *
005200**********
005210   03 部位２ＷＲ.
005220      05 後療２ＷＲ.
005230         07 後療単価２ＷＲ             PIC 9(4)  VALUE ZERO.
005240         07 後療回数２ＷＲ             PIC 9(2)  VALUE ZERO.
005250         07 後療料２ＷＲ               PIC 9(5)  VALUE ZERO.
005260      05 冷罨法２ＷＲ.
005270         07 冷罨法回数２ＷＲ           PIC 9(2)  VALUE ZERO.
005280         07 冷罨法料２ＷＲ             PIC 9(4)  VALUE ZERO.
005290      05 温罨法２ＷＲ.
005300         07 温罨法回数２ＷＲ           PIC 9(2)  VALUE ZERO.
005310         07 温罨法料２ＷＲ             PIC 9(4)  VALUE ZERO.
005320      05 電療２ＷＲ.
005330         07 電療回数２ＷＲ             PIC 9(2)  VALUE ZERO.
005340         07 電療料２ＷＲ               PIC 9(4)  VALUE ZERO.
005350      05 小計２ＷＲ                    PIC 9(6)  VALUE ZERO.
005360      05 長期逓減率２ＷＲ              PIC 9(3)  VALUE ZERO.
005370      05 長期込小計２ＷＲ              PIC 9(6)  VALUE ZERO.
005380******************
005390* ３部位／８割 *
005400******************
005410   03 部位３８ＷＲ.
005420      05 後療３８ＷＲ.
005430         07 後療単価３８ＷＲ           PIC 9(4)  VALUE ZERO.
005440         07 後療回数３８ＷＲ           PIC 9(2)  VALUE ZERO.
005450         07 後療料３８ＷＲ             PIC 9(5)  VALUE ZERO.
005460      05 冷罨法３８ＷＲ.
005470         07 冷罨法回数３８ＷＲ         PIC 9(2)  VALUE ZERO.
005480         07 冷罨法料３８ＷＲ           PIC 9(4)  VALUE ZERO.
005490      05 温罨法３８ＷＲ.
005500         07 温罨法回数３８ＷＲ         PIC 9(2)  VALUE ZERO.
005510         07 温罨法料３８ＷＲ           PIC 9(4)  VALUE ZERO.
005520      05 電療３８ＷＲ.
005530         07 電療回数３８ＷＲ           PIC 9(2)  VALUE ZERO.
005540         07 電療料３８ＷＲ             PIC 9(4)  VALUE ZERO.
005550      05 小計３８ＷＲ                  PIC 9(6)  VALUE ZERO.
005560      05 多部位込小計３８ＷＲ          PIC 9(6)  VALUE ZERO.
005570      05 長期逓減率３８ＷＲ            PIC 9(3)  VALUE ZERO.
005580      05 長期込小計３８ＷＲ            PIC 9(6)  VALUE ZERO.
005590******************
005600* ３部位／１０割 *
005610******************
005620   03 部位３０ＷＲ.
005630      05 逓減開始月日３０ＷＲ.
005640         07 逓減開始月３０ＷＲ         PIC 9(2)  VALUE ZERO.
005650         07 逓減開始日３０ＷＲ         PIC 9(2)  VALUE ZERO.
005660      05 後療３０ＷＲ.
005670         07 後療単価３０ＷＲ           PIC 9(4)  VALUE ZERO.
005680         07 後療回数３０ＷＲ           PIC 9(2)  VALUE ZERO.
005690         07 後療料３０ＷＲ             PIC 9(5)  VALUE ZERO.
005700      05 冷罨法３０ＷＲ.
005710         07 冷罨法回数３０ＷＲ         PIC 9(2)  VALUE ZERO.
005720         07 冷罨法料３０ＷＲ           PIC 9(4)  VALUE ZERO.
005730      05 温罨法３０ＷＲ.
005740         07 温罨法回数３０ＷＲ         PIC 9(2)  VALUE ZERO.
005750         07 温罨法料３０ＷＲ           PIC 9(4)  VALUE ZERO.
005760      05 電療３０ＷＲ.
005770         07 電療回数３０ＷＲ           PIC 9(2)  VALUE ZERO.
005780         07 電療料３０ＷＲ             PIC 9(4)  VALUE ZERO.
005790      05 小計３０ＷＲ                  PIC 9(6)  VALUE ZERO.
005800      05 長期逓減率３０ＷＲ            PIC 9(3)  VALUE ZERO.
005810      05 長期込小計３０ＷＲ            PIC 9(6)  VALUE ZERO.
005820****************
005830* ４部位／５割 *
005840****************
005850   03 部位４５ＷＲ.
005860      05 後療４５ＷＲ.
005870         07 後療単価４５ＷＲ           PIC 9(4)  VALUE ZERO.
005880         07 後療回数４５ＷＲ           PIC 9(2)  VALUE ZERO.
005890         07 後療料４５ＷＲ             PIC 9(5)  VALUE ZERO.
005900      05 冷罨法４５ＷＲ.
005910         07 冷罨法回数４５ＷＲ         PIC 9(2)  VALUE ZERO.
005920         07 冷罨法料４５ＷＲ           PIC 9(4)  VALUE ZERO.
005930      05 温罨法４５ＷＲ.
005940         07 温罨法回数４５ＷＲ         PIC 9(2)  VALUE ZERO.
005950         07 温罨法料４５ＷＲ           PIC 9(4)  VALUE ZERO.
005960      05 電療４５ＷＲ.
005970         07 電療回数４５ＷＲ           PIC 9(2)  VALUE ZERO.
005980         07 電療料４５ＷＲ             PIC 9(4)  VALUE ZERO.
005990      05 小計４５ＷＲ                  PIC 9(6)  VALUE ZERO.
006000      05 多部位込小計４５ＷＲ          PIC 9(6)  VALUE ZERO.
006010      05 長期逓減率４５ＷＲ            PIC 9(3)  VALUE ZERO.
006020      05 長期込小計４５ＷＲ            PIC 9(6)  VALUE ZERO.
006030****************
006040* ４部位／８割 *
006050****************
006060   03 部位４８ＷＲ.
006070      05 逓減開始月日４８ＷＲ.
006080         07 逓減開始月４８ＷＲ         PIC 9(2)  VALUE ZERO.
006090         07 逓減開始日４８ＷＲ         PIC 9(2)  VALUE ZERO.
006100      05 後療４８ＷＲ.
006110         07 後療単価４８ＷＲ           PIC 9(4)  VALUE ZERO.
006120         07 後療回数４８ＷＲ           PIC 9(2)  VALUE ZERO.
006130         07 後療料４８ＷＲ             PIC 9(5)  VALUE ZERO.
006140      05 冷罨法４８ＷＲ.
006150         07 冷罨法回数４８ＷＲ         PIC 9(2)  VALUE ZERO.
006160         07 冷罨法料４８ＷＲ           PIC 9(4)  VALUE ZERO.
006170      05 温罨法４８ＷＲ.
006180         07 温罨法回数４８ＷＲ         PIC 9(2)  VALUE ZERO.
006190         07 温罨法料４８ＷＲ           PIC 9(4)  VALUE ZERO.
006200      05 電療４８ＷＲ.
006210         07 電療回数４８ＷＲ           PIC 9(2)  VALUE ZERO.
006220         07 電療料４８ＷＲ             PIC 9(4)  VALUE ZERO.
006230      05 小計４８ＷＲ                  PIC 9(6)  VALUE ZERO.
006240      05 多部位込小計４８ＷＲ          PIC 9(6)  VALUE ZERO.
006250      05 長期逓減率４８ＷＲ            PIC 9(3)  VALUE ZERO.
006260      05 長期込小計４８ＷＲ            PIC 9(6)  VALUE ZERO.
006270******************
006280* ４部位／１０割 *
006290******************
006300   03 部位４０ＷＲ.
006310      05 逓減開始月日４０ＷＲ.
006320         07 逓減開始月４０ＷＲ         PIC 9(2)  VALUE ZERO.
006330         07 逓減開始日４０ＷＲ         PIC 9(2)  VALUE ZERO.
006340      05 後療４０ＷＲ.
006350         07 後療単価４０ＷＲ           PIC 9(4)  VALUE ZERO.
006360         07 後療回数４０ＷＲ           PIC 9(2)  VALUE ZERO.
006370         07 後療料４０ＷＲ             PIC 9(5)  VALUE ZERO.
006380      05 冷罨法４０ＷＲ.
006390         07 冷罨法回数４０ＷＲ         PIC 9(2)  VALUE ZERO.
006400         07 冷罨法料４０ＷＲ           PIC 9(4)  VALUE ZERO.
006410      05 温罨法４０ＷＲ.
006420         07 温罨法回数４０ＷＲ         PIC 9(2)  VALUE ZERO.
006430         07 温罨法料４０ＷＲ           PIC 9(4)  VALUE ZERO.
006440      05 電療４０ＷＲ.
006450         07 電療回数４０ＷＲ           PIC 9(2)  VALUE ZERO.
006460         07 電療料４０ＷＲ             PIC 9(4)  VALUE ZERO.
006470      05 小計４０ＷＲ                  PIC 9(6)  VALUE ZERO.
006480      05 長期逓減率４０ＷＲ            PIC 9(3)  VALUE ZERO.
006490      05 長期込小計４０ＷＲ            PIC 9(6)  VALUE ZERO.
006500********************
006510* ５部位／２．５割 *
006520********************
006530   03 部位５２ＷＲ.
006540      05 後療５２ＷＲ.
006550         07 後療単価５２ＷＲ           PIC 9(4)  VALUE ZERO.
006560         07 後療回数５２ＷＲ           PIC 9(2)  VALUE ZERO.
006570         07 後療料５２ＷＲ             PIC 9(5)  VALUE ZERO.
006580      05 冷罨法５２ＷＲ.
006590         07 冷罨法回数５２ＷＲ         PIC 9(2)  VALUE ZERO.
006600         07 冷罨法料５２ＷＲ           PIC 9(4)  VALUE ZERO.
006610      05 温罨法５２ＷＲ.
006620         07 温罨法回数５２ＷＲ         PIC 9(2)  VALUE ZERO.
006630         07 温罨法料５２ＷＲ           PIC 9(4)  VALUE ZERO.
006640      05 電療５２ＷＲ.
006650         07 電療回数５２ＷＲ           PIC 9(2)  VALUE ZERO.
006660         07 電療料５２ＷＲ             PIC 9(4)  VALUE ZERO.
006670      05 小計５２ＷＲ                  PIC 9(6)  VALUE ZERO.
006680      05 多部位込小計５２ＷＲ          PIC 9(6)  VALUE ZERO.
006690      05 長期逓減率５２ＷＲ            PIC 9(3)  VALUE ZERO.
006700      05 長期込小計５２ＷＲ            PIC 9(6)  VALUE ZERO.
006710****************
006720* ５部位／５割 *
006730****************
006740   03 部位５５ＷＲ.
006750      05 逓減開始月日５５ＷＲ.
006760         07 逓減開始月５５ＷＲ         PIC 9(2)  VALUE ZERO.
006770         07 逓減開始日５５ＷＲ         PIC 9(2)  VALUE ZERO.
006780      05 後療５５ＷＲ.
006790         07 後療単価５５ＷＲ           PIC 9(4)  VALUE ZERO.
006800         07 後療回数５５ＷＲ           PIC 9(2)  VALUE ZERO.
006810         07 後療料５５ＷＲ             PIC 9(5)  VALUE ZERO.
006820      05 冷罨法５５ＷＲ.
006830         07 冷罨法回数５５ＷＲ         PIC 9(2)  VALUE ZERO.
006840         07 冷罨法料５５ＷＲ           PIC 9(4)  VALUE ZERO.
006850      05 温罨法５５ＷＲ.
006860         07 温罨法回数５５ＷＲ         PIC 9(2)  VALUE ZERO.
006870         07 温罨法料５５ＷＲ           PIC 9(4)  VALUE ZERO.
006880      05 電療５５ＷＲ.
006890         07 電療回数５５ＷＲ           PIC 9(2)  VALUE ZERO.
006900         07 電療料５５ＷＲ             PIC 9(4)  VALUE ZERO.
006910      05 小計５５ＷＲ                  PIC 9(6)  VALUE ZERO.
006920      05 多部位込小計５５ＷＲ          PIC 9(6)  VALUE ZERO.
006930      05 長期逓減率５５ＷＲ            PIC 9(3)  VALUE ZERO.
006940      05 長期込小計５５ＷＲ            PIC 9(6)  VALUE ZERO.
006950****************
006960* ５部位／８割 *
006970****************
006980   03 部位５８ＷＲ.
006990      05 逓減開始月日５８ＷＲ.
007000         07 逓減開始月５８ＷＲ         PIC 9(2)  VALUE ZERO.
007010         07 逓減開始日５８ＷＲ         PIC 9(2)  VALUE ZERO.
007020      05 後療５８ＷＲ.
007030         07 後療単価５８ＷＲ           PIC 9(4)  VALUE ZERO.
007040         07 後療回数５８ＷＲ           PIC 9(2)  VALUE ZERO.
007050         07 後療料５８ＷＲ             PIC 9(5)  VALUE ZERO.
007060      05 冷罨法５８ＷＲ.
007070         07 冷罨法回数５８ＷＲ         PIC 9(2)  VALUE ZERO.
007080         07 冷罨法料５８ＷＲ           PIC 9(4)  VALUE ZERO.
007090      05 温罨法５８ＷＲ.
007100         07 温罨法回数５８ＷＲ         PIC 9(2)  VALUE ZERO.
007110         07 温罨法料５８ＷＲ           PIC 9(4)  VALUE ZERO.
007120      05 電療５８ＷＲ.
007130         07 電療回数５８ＷＲ           PIC 9(2)  VALUE ZERO.
007140         07 電療料５８ＷＲ             PIC 9(4)  VALUE ZERO.
007150      05 小計５８ＷＲ                  PIC 9(6)  VALUE ZERO.
007160      05 多部位込小計５８ＷＲ          PIC 9(6)  VALUE ZERO.
007170      05 長期逓減率５８ＷＲ            PIC 9(3)  VALUE ZERO.
007180      05 長期込小計５８ＷＲ            PIC 9(6)  VALUE ZERO.
007190******************
007200* ５部位／１０割 *
007210******************
007220   03 部位５０ＷＲ.
007230      05 逓減開始月日５０ＷＲ.
007240         07 逓減開始月５０ＷＲ         PIC 9(2)  VALUE ZERO.
007250         07 逓減開始日５０ＷＲ         PIC 9(2)  VALUE ZERO.
007260      05 後療５０ＷＲ.
007270         07 後療単価５０ＷＲ           PIC 9(4)  VALUE ZERO.
007280         07 後療回数５０ＷＲ           PIC 9(2)  VALUE ZERO.
007290         07 後療料５０ＷＲ             PIC 9(5)  VALUE ZERO.
007300      05 冷罨法５０ＷＲ.
007310         07 冷罨法回数５０ＷＲ         PIC 9(2)  VALUE ZERO.
007320         07 冷罨法料５０ＷＲ           PIC 9(4)  VALUE ZERO.
007330      05 温罨法５０ＷＲ.
007340         07 温罨法回数５０ＷＲ         PIC 9(2)  VALUE ZERO.
007350         07 温罨法料５０ＷＲ           PIC 9(4)  VALUE ZERO.
007360      05 電療５０ＷＲ.
007370         07 電療回数５０ＷＲ           PIC 9(2)  VALUE ZERO.
007380         07 電療料５０ＷＲ             PIC 9(4)  VALUE ZERO.
007390      05 小計５０ＷＲ                  PIC 9(6)  VALUE ZERO.
007400      05 長期逓減率５０ＷＲ            PIC 9(3)  VALUE ZERO.
007410      05 長期込小計５０ＷＲ            PIC 9(6)  VALUE ZERO.
007420*
007430**************
007440* 施術所情報 *
007450**************
007460 01 施術所情報Ｗ.
007470    03 柔整師番号Ｗ                    PIC X(22) VALUE SPACE.
007480    03 接骨師会会員番号Ｗ              PIC X(10) VALUE SPACE.
007490    03 代表者カナＷ                    PIC X(50) VALUE SPACE.
007500    03 代表者名Ｗ.
007510       05 印刷代表者名Ｗ               PIC X(50) VALUE SPACE.
007520    03 接骨院名Ｗ                      PIC X(50) VALUE SPACE.
          03 都道府県ＪＩＳＷ                PIC X(2)   VALUE SPACE.
007530    03 施術所住所Ｗ.
007540       05 施術所住所１Ｗ               PIC X(50) VALUE SPACE.
007550       05 施術所住所２Ｗ               PIC X(50) VALUE SPACE.
007560    03 施術所郵便番号Ｗ.
007570       05 施術所郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
007580       05 施術所郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
007590    03 施術所電話番号Ｗ                PIC X(15) VALUE SPACE.
007600    03 接骨師会会長名Ｗ.
007610       05 印刷接骨師会会長名Ｗ         PIC N(7)  VALUE SPACE.
007620       05 FILLER                       PIC N(3)  VALUE SPACE.
007630    03 定額制受理番号Ｗ                PIC X(15) VALUE SPACE.
007640    03 柔整師年月日Ｗ.
007650       05 柔整師年Ｗ                   PIC 9(2)  VALUE ZERO.
007660       05 柔整師月Ｗ                   PIC 9(2)  VALUE ZERO.
007670       05 柔整師日Ｗ                   PIC 9(2)  VALUE ZERO.
007680    03 患者委任年月日Ｗ.
007690       05 患者委任年Ｗ                 PIC 9(2)  VALUE ZERO.
007700       05 患者委任月Ｗ                 PIC 9(2)  VALUE ZERO.
007710       05 患者委任日Ｗ                 PIC 9(2)  VALUE ZERO.
007720    03 取引先情報Ｗ.
007730        05 取引先銀行名Ｗ              PIC X(40) VALUE SPACE.
007740        05 取引先銀行支店名Ｗ          PIC X(40) VALUE SPACE.
007750        05 預金種別Ｗ                  PIC 9(1)  VALUE ZERO.
007760        05 銀行番号Ｗ                  PIC X(4)  VALUE ZERO.
007770        05 店番号Ｗ                    PIC X(3)  VALUE ZERO.
007780        05 口座番号Ｗ                  PIC X(10) VALUE SPACE.
007790        05 口座名義人Ｗ                PIC X(40) VALUE SPACE.
007800        05 口座名義人カナＷ            PIC X(40) VALUE SPACE.
007810        05 銀行名支店名Ｗ              PIC X(60) VALUE SPACE.
007820        05 預金種別名称Ｗ              PIC X(4)  VALUE SPACE.
007830        05 預金種別コメントＷ          PIC X(15) VALUE SPACE.
007840    03 県施術ＩＤＷ                    PIC X(15) VALUE SPACE.
007850    03 市町村施術ＩＤＷ                PIC X(15) VALUE SPACE.
007860    03 コメントＷ.
007870        05 コメント１Ｗ                PIC X(40) VALUE SPACE.
007880        05 コメント２Ｗ                PIC X(40) VALUE SPACE.
007890        05 コメント３Ｗ                PIC X(40) VALUE SPACE.
007900        05 コメント４Ｗ                PIC X(40) VALUE SPACE.
007910        05 コメント５Ｗ                PIC X(40) VALUE SPACE.
007920        05 コメント６Ｗ                PIC X(40) VALUE SPACE.
007930        05 コメント７Ｗ                PIC X(40) VALUE SPACE.
007940**************
007950* 受診者情報 *
007960**************
007970 01 受診者情報Ｗ.
007980    03 患者番号Ｗ                      PIC 9(6)  VALUE ZERO.
007990    03 施術年月Ｗ.
008000       05 施術年Ｗ                     PIC 9(2)  VALUE ZERO.
008010       05 施術月Ｗ                     PIC 9(2)  VALUE ZERO.
008020*    03 記号Ｗ                          PIC N(12) VALUE SPACE.
007570    03 記号Ｗ.
007580       05 印刷記号Ｗ                   PIC N(12)  VALUE SPACE.
          03 記号番号Ｗ.
             05 記号番号ＸＷ                 PIC X(40) VALUE SPACE.
008030    03 番号Ｗ.
008040       05 印刷番号Ｗ                   PIC X(15) VALUE SPACE.
008050       05 FILLER                       PIC X(15) VALUE SPACE.
008060*
008070    03 保険種別Ｗ                      PIC 9(2)  VALUE ZERO.
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
004880    03 保険種別親Ｗ                    PIC N(2)  VALUE SPACE.
004880    03 保険種別編集Ｗ                  PIC N(5)  VALUE SPACE.
008080    03 保険者番号Ｗ.
008090       05 印刷保険者番号Ｗ             PIC X(8)  VALUE SPACE.
008100       05 FILLER                       PIC X(2)  VALUE SPACE.
008110    03 保険者名称Ｗ.
008120       05 印刷保険者名称１Ｗ           PIC X(40) VALUE SPACE.
008130       05 印刷保険者名称２Ｗ           PIC X(40) VALUE SPACE.
008140*
008150    03 市町村番号Ｗ.
008160       05 印刷市町村番号Ｗ             PIC X(8)  VALUE SPACE.
008170       05 FILLER                       PIC X(2).
008180    03 市町村名称Ｗ                    PIC X(40) VALUE SPACE.
008190    03 受給者番号Ｗ.
008200       05 印刷受給者番号Ｗ             PIC X(15)  VALUE SPACE.
008210       05 FILLER                       PIC X(5).
008220*
008230    03 請求先名称Ｗ.
008240       05 印刷請求先名称１Ｗ           PIC X(40) VALUE SPACE.
008250       05 印刷請求先名称２Ｗ           PIC X(40) VALUE SPACE.
008260*
008270    03 被保険者情報Ｗ.
008280       05 被保険者カナＷ               PIC X(50) VALUE SPACE.
008290       05 被保険者氏名Ｗ               PIC X(50) VALUE SPACE.
008560       05 被保険者住所Ｗ.
008300          07 被保険者住所１Ｗ          PIC X(50) VALUE SPACE.
008310          07 被保険者住所２Ｗ          PIC X(50) VALUE SPACE.
008990       05 電話番号Ｗ                   PIC X(35)  VALUE SPACE.
008320*
008330    03 患者情報Ｗ.
008340       05 郵便番号Ｗ.
008350          07 郵便番号１Ｗ              PIC X(3)  VALUE SPACE.
008360          07 郵便番号２Ｗ              PIC X(4)  VALUE SPACE.
             05 患者住所Ｗ.
008370          07 患者住所１Ｗ              PIC X(50)  VALUE SPACE.
008380          07 患者住所２Ｗ              PIC X(50)  VALUE SPACE.
008390       05 患者カナＷ                   PIC X(50) VALUE SPACE.
008400       05 患者氏名Ｗ                   PIC X(50) VALUE SPACE.
008410       05 性別チェックＷ.
008420          07 男チェックＷ              PIC N(1)  VALUE SPACE.
008430          07 女チェックＷ              PIC N(1)  VALUE SPACE.
008690          07 性別Ｗ                    PIC N(2)  VALUE SPACE.
008440       05 和暦チェックＷ.
008450          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
008460          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
008470          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
008480          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
008490          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
008500       05 患者年Ｗ                     PIC 9(2)  VALUE ZERO.
008510       05 患者月Ｗ                     PIC 9(2)  VALUE ZERO.
008520       05 患者日Ｗ                     PIC 9(2)  VALUE ZERO.
008530       05 続柄Ｗ.
008540          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
008550          07 FILLER                    PIC X(4)  VALUE SPACE.
008560*       05 続柄チェックＷ.
008570*          07 本人チェックＷ            PIC N(1)  VALUE SPACE.
008580*          07 家族チェックＷ            PIC N(1)  VALUE SPACE.
008590*
008600*       05 負傷原因Ｗ                   PIC N(40) OCCURS 29 VALUE SPACE.
      */半角対応/110421
             05 負傷原因Ｗ OCCURS 29.
                07 負傷原因ＸＷ              PIC X(80)  VALUE SPACE.
008610*
008620    03 保険種別チェックＷ.
008630       05 政チェックＷ                 PIC N(1)  VALUE SPACE.
008640       05 組チェックＷ                 PIC N(1)  VALUE SPACE.
008650       05 日チェックＷ                 PIC N(1)  VALUE SPACE.
008660       05 船チェックＷ                 PIC N(1)  VALUE SPACE.
008670       05 共チェックＷ                 PIC N(1)  VALUE SPACE.
008680       05 国チェックＷ                 PIC N(1)  VALUE SPACE.
008690       05 退チェックＷ                 PIC N(1)  VALUE SPACE.
008690       05 高チェックＷ                 PIC N(1)  VALUE SPACE.
008700*
008710    03 助成種別Ｗ.
008720       05 老チェックＷ                 PIC N(1)  VALUE SPACE.
008730       05 乳チェックＷ                 PIC N(1)  VALUE SPACE.
008740       05 障チェックＷ                 PIC N(1)  VALUE SPACE.
008750       05 母チェックＷ                 PIC N(1)  VALUE SPACE.
008760       05 傷チェックＷ                 PIC N(1)  VALUE SPACE.
008760       05 福チェックＷ                 PIC N(1)  VALUE SPACE.
             05 助成チェックＷ               PIC N(1)  VALUE SPACE.
008770*
008780    03 給付割合チェックＷ.
008790       05 給付７割チェックＷ           PIC N(1)  VALUE SPACE.
008800       05 給付８割チェックＷ           PIC N(1)  VALUE SPACE.
008810       05 給付９割チェックＷ           PIC N(1)  VALUE SPACE.
008820       05 給付老人チェックＷ           PIC N(1)  VALUE SPACE.
008830       05 給付老人Ｗ                   PIC N(1)  VALUE SPACE.
008810*
008820    03 特別区分チェックＷ.
008830       05 ７０歳以上チェックＷ         PIC N(1)  VALUE SPACE.
008840       05 未就学チェックＷ             PIC N(1)  VALUE SPACE.
008890       05 高齢割合Ｗ                   PIC X(1)  VALUE SPACE.
008840*
008850    03 特別マークＷ                    PIC N(1)  VALUE SPACE.
008860    03 特別コメントＷ                  PIC X(16) VALUE SPACE.
008870    03 特別コメント２Ｗ                PIC X(16) VALUE SPACE.
007910    03 助成印Ｗ                        PIC N(1)  VALUE SPACE.
008880*
008890****************
008900* 負傷データＦ *
008910****************
008920 01 負傷情報Ｗ.
008930    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
008940    03 部位情報Ｗ  OCCURS   9.
008950       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
008960       05 部位コードＷ.
008970          07 負傷種別Ｗ                PIC 9(2)  VALUE ZERO.
008980          07 部位Ｗ                    PIC 9(2)  VALUE ZERO.
008990          07 左右区分Ｗ                PIC 9(1)  VALUE ZERO.
009000          07 負傷位置番号Ｗ            PIC 9(2)  VALUE ZERO.
009010       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
009020       05 負傷年月日Ｗ.
009030          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
009040          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
009050          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
009060       05 初検年月日Ｗ.
009070          07 初検年Ｗ                  PIC 9(2)  VALUE ZERO.
009080          07 初検月Ｗ                  PIC 9(2)  VALUE ZERO.
009090          07 初検日Ｗ                  PIC 9(2)  VALUE ZERO.
009100       05 開始年月日Ｗ.
009110          07 開始年Ｗ                  PIC 9(2)  VALUE ZERO.
009120          07 開始月Ｗ                  PIC 9(2)  VALUE ZERO.
009130          07 開始日Ｗ                  PIC 9(2)  VALUE ZERO.
009140       05 終了年月日Ｗ.
009150          07 終了年Ｗ                  PIC 9(2)  VALUE ZERO.
009160          07 終了月Ｗ                  PIC 9(2)  VALUE ZERO.
009170          07 終了日Ｗ                  PIC 9(2)  VALUE ZERO.
009180       05 実日数Ｗ                     PIC 9(2)  VALUE ZERO.
009190       05 転帰区分Ｗ                   PIC 9(1)  VALUE ZERO.
009200       05 転帰区分チェックＷ.
009210          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
009220          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
009230          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
009240       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
009250       05 部位区切Ｗ                   PIC X(1)  VALUE SPACE.
009260       05 経過略称Ｗ.
009270          07 印刷経過略称Ｗ            PIC N(5)  VALUE SPACE.
009280          07 FILLER                    PIC X(2)  VALUE SPACE.
009290    03 新規チェックＷ                  PIC N(1)  VALUE SPACE.
009300    03 継続チェックＷ                  PIC N(1)  VALUE SPACE.
009310*
009320************
009330* 料金情報 *
009340************
009350 01 料金情報Ｗ.
009360    03 初検加算Ｗ.
009370       05 時間外チェックＷ             PIC N(1)  VALUE SPACE.
009380       05 休日チェックＷ               PIC N(1)  VALUE SPACE.
009390       05 深夜チェックＷ               PIC N(1)  VALUE SPACE.
009400    03 往療加算Ｗ.
009410       05 夜間チェックＷ               PIC N(1)  VALUE SPACE.
009620       05 難路チェックＷ               PIC N(1)  VALUE SPACE.
009420       05 暴風雨雪チェックＷ           PIC N(1)  VALUE SPACE.
009430    03 金属副子チェックＷ.
009440       05 大チェックＷ                 PIC N(1)  VALUE SPACE.
009450       05 中チェックＷ                 PIC N(1)  VALUE SPACE.
009460       05 小チェックＷ                 PIC N(1)  VALUE SPACE.
009470    03 小計Ｗ                          PIC 9(7)  VALUE ZERO.
009480    03 初回処置料合計Ｗ                PIC 9(6)  VALUE ZERO.
      */金属副子・運動後療の変更・追加/1805
          03 金属回数Ｗ                         PIC 9(2)  VALUE ZERO.
          03 運動料Ｗ                           PIC 9(4)  VALUE ZERO.
009490************
009500* 備考情報 *
009510************
009520 01 備考情報Ｗ.
009530    03 適用１Ｗ                        PIC N(38) VALUE SPACE.
009540    03 適用２Ｗ                        PIC N(38) VALUE SPACE.
009550    03 経過コメントＷ                  PIC N(60) VALUE SPACE.
009560*
009570***************************
009580** レセ摘要用( N(38)固定）*
009590***************************
009600 01 負傷の経過Ｗ.
009610    03 負傷の経過行Ｗ                  PIC X(76) OCCURS 2 VALUE SPACE.
009620 01 負傷の経過ＮＷ REDEFINES 負傷の経過Ｗ.
009630    03 負傷の経過行ＮＷ                PIC N(38) OCCURS 2.
009640*
       01 摘要施術日Ｗ                       PIC X(100) VALUE SPACE.
       01 施術日Ｗ.
          03 施術日２Ｗ                      PIC X(1)  VALUE SPACE.
          03 施術日１Ｗ                      PIC X(1)  VALUE SPACE.
004460* レセプト並び順 *
004470 01 順番Ｗ                             PIC 9(4) VALUE ZERO.
004480*
009650*************************************************************************
009660 01 印刷制御.
009670     03 定義体名Ｐ                     PIC X(8)  VALUE SPACE.
009680     03 項目群名Ｐ                     PIC X(8)  VALUE SPACE.
009690     03 処理種別Ｐ                     PIC X(2)  VALUE SPACE.
009700     03 拡張制御Ｐ.
009710         05 端末制御Ｐ.
009720             07 移動方向Ｐ             PIC X(1)  VALUE SPACE.
009730             07 移動行数Ｐ             PIC 9(3)  VALUE ZERO.
009740         05 詳細制御Ｐ                 PIC X(2)  VALUE SPACE.
009750     03 通知情報Ｐ                     PIC X(2)  VALUE SPACE.
009760     03 ユニット名Ｐ                   PIC X(8)  VALUE SPACE.
009770*
009780 01 計算機西暦年Ｗ                     PIC 9(2)  VALUE ZERO.
009790* 日付ＷＯＲＫ
009800 01 和暦終了年Ｗ                       PIC 9(4)  VALUE ZERO.
009810 01 計算機西暦.
009820    03 計算機西暦年                    PIC 9(4)  VALUE ZERO.
009830    03 計算機西暦月日                  PIC 9(4)  VALUE ZERO.
009840 01 計算機西暦Ｒ REDEFINES 計算機西暦.
009850    03 計算機世紀                      PIC 9(2).
009860    03 計算機日付                      PIC 9(6).
009870    03 計算機日付Ｒ REDEFINES 計算機日付.
009880       05 計算機年月                   PIC 9(4).
009890       05 計算機年月Ｒ REDEFINES 計算機年月.
009900         07 計算機年                   PIC 9(2).
009910         07 計算機月                   PIC 9(2).
009920       05 計算機日                     PIC 9(2).
009930*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
009940******************************************************************
009950*                          連結項目                              *
009960******************************************************************
009970**  画面入力データ
009980 01 連入－入力データ委任印刷 IS EXTERNAL.
009990    03 連入－委任印刷                  PIC 9.
       01 連入－入力データ電話印刷 IS EXTERNAL.
          03 連入－電話印刷                     PIC 9.
010000*
       01 連入－プレビュー IS EXTERNAL.
          03 連入－プレビュー区分          PIC 9.
010300*
010010******************
010020* ３カ月長期判定 *
010030******************
010040 01 連期間－キー IS EXTERNAL.
010050    03 連期間－施術年月.
010060       05 連期間－施術和暦             PIC 9.
010070       05 連期間－施術年               PIC 9(2).
010080       05 連期間－施術月               PIC 9(2).
010090    03  連期間－患者コード.
010100       05 連期間－患者番号             PIC 9(6).
010110       05 連期間－枝番                 PIC X.
010120    03 連期間－対象フラグ              PIC X(3).
010130    03 連期間－期間月Ｗ.
010140       05 連期間－期間Ｗ               PIC 9(2) OCCURS 9.
010150*
010160************
010170* 印刷キー *
010180************
010190*
010200*
010210 01 連レ印－対象データ IS EXTERNAL.
010220    03 連レ印－施術年月日.
010230       05 連レ印－施術和暦             PIC 9(1).
010240       05 連レ印－施術年               PIC 9(2).
010250       05 連レ印－施術月               PIC 9(2).
010260    03 連レ印－患者コード.
010270       05 連レ印－患者番号             PIC 9(6).
010280       05 連レ印－枝番                 PIC X(1).
010290    03 連レ印－保険種別                PIC 9(2).
010300    03 連レ印－保険者番号              PIC X(10).
010310    03 連レ印－公費種別                PIC 9(2).
010320    03 連レ印－費用負担者番号          PIC X(10).
010330    03 連レ印－助成種別                PIC 9(2).
010340    03 連レ印－費用負担者番号助成      PIC X(10).
010350    03 連レ印－患者カナ                PIC X(20).
010360    03 連レ印－本人家族区分            PIC 9(1).
013790*
013800 01 連レ－キー IS EXTERNAL.
013810    03 連レ－保険種別                  PIC 9(2).
013820*
013830*================================================================*
013840* 負担率取得用14/10～
013850 01 連率－負担率取得キー IS EXTERNAL.
013860    03 連率－施術和暦年月.
013870       05 連率－施術和暦               PIC 9.
013880       05 連率－施術年月.
013890          07 連率－施術年              PIC 9(2).
013900          07 連率－施術月              PIC 9(2).
013910    03 連率－患者コード.
013920       05 連率－患者番号               PIC 9(6).
013930       05 連率－枝番                   PIC X.
013940    03 連率－実際負担率                PIC 9(3).
013950    03 連率－実際本体負担率            PIC 9(3).
013960    03 連率－健保負担率                PIC 9(3).
013970    03 連率－２７老負担率              PIC 9(3).
013980    03 連率－助成負担率                PIC 9(3).
013990    03 連率－特別用負担率              PIC 9(3).
014000*
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
014010************************
014020* 長期理由文セット     *
014030************************
014040 01 連長文－キー IS EXTERNAL.
014050    03 連長文－施術年月.
014060       05 連長文－施術和暦             PIC 9.
014070       05 連長文－施術年               PIC 9(2).
014080       05 連長文－施術月               PIC 9(2).
014090    03  連長文－患者コード.
014100       05 連長文－患者番号             PIC 9(6).
014110       05 連長文－枝番                 PIC X.
014120    03 連長文－文桁数                  PIC 9(2).
014130    03 連長文－理由文                  PIC N(63) OCCURS 15.
014140*
013022*************
013023* 助成名称
013024*************
013025 01 連助成名称－キー IS EXTERNAL.
013026    03 連助成名称－助成種別             PIC 9(2).
013027    03 連助成名称－費用負担者番号助成   PIC X(10).
013028*   / OUT /
013029    03 連助成名称－名称集団.
013030       05 連助成名称－１文字            PIC N.
013031       05 連助成名称－略称              PIC N(4).
013032       05 連助成名称－正式名称          PIC N(10).
013033*
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
014150******************************************************************
014160*                      PROCEDURE  DIVISION                       *
014170******************************************************************
014180 PROCEDURE               DIVISION.
014190************
014200*           *
014210* 初期処理   *
014220*           *
014230************
002570     PERFORM プリンタファイル作成.
014240     PERFORM 初期化.
014250************
014260*           *
014270* 主処理     *
014280*           *
014290************
014300* 印刷
014310     PERFORM 連結項目待避.
014320     PERFORM 印刷セット.
014330     PERFORM 印刷処理.
014340************
014350*           *
014360* 終了処理   *
014370*           *
014380************
014390     PERFORM 受診者印刷区分更新.
014400     PERFORM 終了処理.
014410     MOVE ZERO  TO PROGRAM-STATUS.
014420     EXIT PROGRAM.
014430*
014440*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
014450*=== 初期処理 ===================================================*
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
002974     MOVE "YCB6425"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分  TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
014460*================================================================*
014470 初期化 SECTION.
014480*================================================================*
014490     PERFORM ファイルオープン.
014500*    /* 現在日付取得 */
014510     ACCEPT 計算機日付 FROM DATE.
014520*    /* 1980～2079年の間で設定 */
014530     IF ( 計算機年 > 80 )
014540        MOVE 19 TO 計算機世紀
014550     ELSE
014560        MOVE 20 TO 計算機世紀
014570     END-IF.
014580     PERFORM カレント元号取得.
014590     PERFORM 和暦終了年取得.
014600     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
014610*
014620*================================================================*
014630 ファイルオープン SECTION.
014640*
014650     OPEN INPUT   元号マスタ
014660         MOVE NC"元号" TO ファイル名.
014670         PERFORM オープンチェック.
014680     OPEN INPUT   名称マスタ
014690         MOVE NC"名称" TO ファイル名.
014700         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
014740     OPEN INPUT   経過マスタ
014750         MOVE NC"経過" TO ファイル名.
014760         PERFORM オープンチェック.
014770     OPEN INPUT   制御情報マスタ
014780         MOVE NC"制御情報" TO ファイル名.
014790         PERFORM オープンチェック.
014800     OPEN INPUT   施術所情報マスタ
014810         MOVE NC"施情" TO ファイル名.
014820         PERFORM オープンチェック.
014750     OPEN INPUT   会情報マスタ.
014760         MOVE NC"会情報マスタ" TO ファイル名.
014770         PERFORM オープンチェック.
014830     OPEN INPUT   保険者マスタ
014840         MOVE NC"保険者" TO ファイル名.
014850         PERFORM オープンチェック.
014860     OPEN INPUT   請求先マスタ
014870         MOVE NC"請先" TO ファイル名.
014880         PERFORM オープンチェック.
014890     OPEN INPUT   ＩＤ管理マスタ
014900         MOVE NC"ＩＤ" TO ファイル名.
014910         PERFORM オープンチェック.
014920     OPEN INPUT 市町村マスタ.
014930         MOVE NC"市町村" TO ファイル名.
014940         PERFORM オープンチェック.
014950     OPEN INPUT   施術記録Ｆ.
014960         MOVE NC"施記Ｆ" TO ファイル名.
014970         PERFORM オープンチェック.
014980     OPEN INPUT   負傷データＦ.
014990         MOVE NC"負傷" TO ファイル名.
015000         PERFORM オープンチェック.
015010     OPEN INPUT   負傷原因Ｆ.
015020         MOVE NC"負傷原因" TO ファイル名.
015030         PERFORM オープンチェック.
015560     OPEN INPUT   受診者情報２Ｆ.
015570         MOVE NC"受診者情報２Ｆ" TO ファイル名.
015580         PERFORM オープンチェック.
016210     OPEN INPUT 作業ファイル２.
016220         MOVE NC"作２" TO ファイル名.
016230         PERFORM オープンチェック.
015070*
015080     OPEN I-O   受診者情報Ｆ.
015090         MOVE NC"受情" TO ファイル名.
015100         PERFORM オープンチェック.
015110     OPEN I-O   印刷ファイル
015120         PERFORM エラー処理Ｐ.
015130*
015140*================================================================*
015150 オープンチェック SECTION.
015160*
015170     IF ( 状態キー  NOT =  "00" )
015180        DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
015190        DISPLAY NC"状態キー：" 状態キー         UPON CONS
015200        DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
015210                                                UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015220        ACCEPT  キー入力 FROM CONS
015230        PERFORM ファイル閉鎖
015240        EXIT PROGRAM.
015250*
015260*================================================================*
015270 カレント元号取得 SECTION.
015280*
015290     MOVE ZEROS TO 制－制御区分.
015300     READ 制御情報マスタ
015310     NOT INVALID KEY
015320         MOVE 制－カレント元号         TO カレント元号Ｗ
015330         MOVE 制－レセ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
015340         MOVE 制－レセ長期理由印刷区分 TO 長期理由印刷区分Ｗ
015350         MOVE 制－レセプト日付区分     TO レセプト日付区分Ｗ
015360         MOVE 制－レセプト患者日付区分 TO レセプト患者日付区分Ｗ
015370     END-READ.
015380*
015390*================================================================*
015400 和暦終了年取得 SECTION.
015410*
015420*     DISPLAY NC"カレント元号Ｗ"  カレント元号Ｗ UPON MSGBOX.
015430     MOVE カレント元号Ｗ TO 元－元号区分.
015440     READ 元号マスタ
015450     INVALID KEY
015460         DISPLAY NC"指定和暦が登録されていません" UPON CONS
015470         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
015480                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015490         ACCEPT  キー入力 FROM CONS
015500         PERFORM 終了処理
015510         EXIT PROGRAM
015520     NOT INVALID KEY
015530         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
015540         MOVE 前和暦Ｗ TO 元－元号区分
015550         READ 元号マスタ
015560         INVALID KEY
015570             DISPLAY NC"指定和暦が登録されていません" UPON CONS
015580             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
015590                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015600             ACCEPT  キー入力 FROM CONS
015610             PERFORM 終了処理
015620             EXIT PROGRAM
015630         NOT INVALID KEY
015640             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
015650         END-READ
015660     END-READ.
015670*
015680*=== 主処理 =====================================================*
015690*================================================================*
015700 連結項目待避 SECTION.
015710*================================================================*
015720     MOVE 連レ印－施術和暦           TO 施術和暦ＷＲ.
015730     MOVE 連レ印－施術年             TO 施術年ＷＲ.
015740     MOVE 連レ印－施術月             TO 施術月ＷＲ.
015750     MOVE 連レ印－保険種別           TO 保険種別ＷＲ.
015760     MOVE 連レ印－保険者番号         TO 保険者番号ＷＲ.
015770     MOVE 連レ印－公費種別           TO 公費種別ＷＲ.
015780     MOVE 連レ印－費用負担者番号     TO 費用負担者番号ＷＲ.
015790     MOVE 連レ印－助成種別           TO 助成種別ＷＲ.
015800     MOVE 連レ印－費用負担者番号助成 TO 費用負担者番号助成ＷＲ.
015810     MOVE 連レ印－本人家族区分       TO 本人家族区分ＷＲ.
015820     MOVE 連レ印－患者カナ           TO 患者カナＷＲ.
015830     MOVE 連レ印－患者番号           TO 患者番号ＷＲ.
015840     MOVE 連レ印－枝番               TO 枝番ＷＲ.
015850*
015860*================================================================*
015870 印刷セット SECTION.
015880*================================================================*
015890     PERFORM 項目初期化.
           PERFORM 基本情報取得.
015900     PERFORM 施術所情報取得.
015910     PERFORM 請求先情報取得.
015920     PERFORM 受診者情報取得.
015930     PERFORM 負傷データ取得.
015940     PERFORM 料金情報取得.
015950     PERFORM 施術記録取得.
015960***     PERFORM 長期判定取得.
015980     PERFORM 初検加算時刻取得.
015990     PERFORM 委任年月日取得.
           PERFORM 施術日取得.
      */並び順印刷/1105
           PERFORM レセプト並び順取得.
016000*
016010* / 制御マスタ・負傷データＦの印刷区分を確認し取得 /
016791*-----------------------------------------------*
016800     IF ( 負傷原因印刷区分Ｗ  NOT = 1 ) AND ( レセ負傷原因印刷区分Ｗ NOT = 1 )
016813        IF ( 負傷原因印刷区分Ｗ = 3 OR 4 )
016815           PERFORM 負傷原因印刷対象判定処理
016817        ELSE
016820           PERFORM 負傷原因取得
016821        END-IF
016830     END-IF.
016831*-----------------------------------------------*
016060*
015940     IF ( 長期理由印刷区分Ｗ NOT = 1 )
               MOVE 長期理由印刷区分Ｗ TO 連摘文－長期区分
016120     END-IF.
016130*
016140     PERFORM 施術ＩＤ取得.
016150***     PERFORM レセプト回数取得.
016160     PERFORM 保険者名称取得.
016170     PERFORM 給付割合取得.
016180*
016190********************
016200* 受診者情報セット *
016210********************
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
016220     IF ( 県施術ＩＤＷ NOT = SPACE )
016230*        MOVE 施術ＩＤ固定Ｗ   TO 施術ＩＤ固定
016240        MOVE 県施術ＩＤＷ     TO 県施術ＩＤ
016250     END-IF.
      *     MOVE 保険種別編集Ｗ       TO 保険種別.
           IF 助成印Ｗ NOT = SPACE
               MOVE 助成印Ｗ             TO 保険種別２
               MOVE NC"○"               TO 助成印丸
           END-IF.
      *     MOVE 未就学チェックＷ     TO 未就学チェック.
      *     MOVE ７０歳以上チェックＷ TO ７０歳以上チェック.
      *     MOVE 高齢割合Ｗ           TO 高齢割合.
016320*
016330     MOVE 施術年Ｗ            TO 施術年.
016340     MOVE 施術月Ｗ            TO 施術月.
016350*
016360*     IF ( 記号Ｗ(1:1) = NC"＊" )
016370*        MOVE  SPACE           TO  記号
016380*     ELSE
016390*        MOVE 記号Ｗ           TO  記号
016400*     END-IF.
016410*     IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
016420*        ( 印刷番号Ｗ(1:2) = "＊" )
016430*        MOVE  SPACE           TO  番号
016440*     ELSE
016450*        MOVE 印刷番号Ｗ       TO  番号
016460*     END-IF.
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
016470*
016480     IF ( 印刷市町村番号Ｗ(1:2) = "99" )
016490         MOVE SPACE            TO 公費負担者番号
016500     ELSE
      */後期高齢89～の時のみ記載する
               IF ( 印刷市町村番号Ｗ(1:2) = "89" )
016510             MOVE 市町村番号Ｗ     TO 公費負担者番号
               END-IF
016520     END-IF.
016530***     MOVE 市町村名称Ｗ        TO 市町村名称.
016540     MOVE 印刷請求先名称１Ｗ  TO 請求先名称 請求先名称２.
016550***     MOVE 印刷請求先名称２Ｗ  TO 請求先名称２.
016560*
016570     IF ( 印刷受給者番号Ｗ(1:1) = "*"  ) OR
016580        ( 印刷受給者番号Ｗ(1:2) = "＊" )
016590        MOVE  SPACE           TO 受給者番号
016600     ELSE
016610        MOVE 受給者番号Ｗ     TO 受給者番号
016620     END-IF.
016630*
016640***     MOVE 政チェックＷ        TO 政チェック.
016650***     MOVE 組チェックＷ        TO 組チェック.
016660***     MOVE 日チェックＷ        TO 日チェック.
016670***     MOVE 船チェックＷ        TO 船チェック.
016680***     MOVE 共チェックＷ        TO 共チェック.
016690***     MOVE 国チェックＷ        TO 国チェック.
016700***     MOVE 退チェックＷ        TO 退チェック.
016700***     MOVE 高チェックＷ        TO 高チェック.
016710*
016720     MOVE 保険者番号Ｗ        TO 保険者番号.
016730***     MOVE 保険者名称Ｗ        TO 保険者名称.
016740***     MOVE 被保険者カナＷ      TO 被保険者カナ.
      */子ども医療の場合は対象の児童名を記載
           IF 助成種別ＷＲ = 55
               MOVE 患者氏名Ｗ      TO 被保険者氏名
           ELSE
016750         MOVE 被保険者氏名Ｗ  TO 被保険者氏名
           END-IF.
016550*     MOVE 被保険者住所Ｗ      TO 住所１.
           MOVE 患者住所１Ｗ        TO 住所１.
           MOVE 患者住所２Ｗ        TO 住所２.
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
016850     MOVE 患者氏名Ｗ          TO 患者氏名.
      *     MOVE 性別Ｗ              TO 性別.
016860     MOVE 男チェックＷ        TO 男チェック.
016870     MOVE 女チェックＷ        TO 女チェック.
016880     MOVE 明治チェックＷ      TO 明治チェック.
016890     MOVE 大正チェックＷ      TO 大正チェック.
016900     MOVE 昭和チェックＷ      TO 昭和チェック.
016910     MOVE 平成チェックＷ      TO 平成チェック.
016920*     MOVE 元号Ｗ              TO 元号.
016930     MOVE 患者年Ｗ            TO 患者年.
016940     MOVE 患者月Ｗ            TO 患者月.
016950     MOVE 患者日Ｗ            TO 患者日.
      *     MOVE NC"年"              TO 年.
      *     MOVE NC"月"              TO 月.
      *     MOVE NC"日"              TO 日.
016960*     MOVE 印刷続柄Ｗ          TO 続柄.
      *     MOVE 給付割合Ｗ          TO 給付割合.
      *
           IF 受２－助成被保険者氏名 NOT = SPACE
016940        MOVE 受２－助成被保険者氏名 TO 被保険者氏名
           END-IF.
016970*
016980     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
016990     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
017000     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
017000     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
017000     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
017000     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
017040*
017190********************
017200* 負傷データセット *
017210********************
017220* １部位 *
017230**********
017240     MOVE 負傷名Ｗ(1)       TO 負傷名１.
017250     MOVE 負傷年Ｗ(1)       TO 負傷年１.
017260     MOVE 負傷月Ｗ(1)       TO 負傷月１.
017270     MOVE 負傷日Ｗ(1)       TO 負傷日１.
017280     MOVE 初検年Ｗ(1)       TO 初検年１.
017290     MOVE 初検月Ｗ(1)       TO 初検月１.
017300     MOVE 初検日Ｗ(1)       TO 初検日１.
017310     MOVE 開始年Ｗ(1)       TO 開始年１.
017320     MOVE 開始月Ｗ(1)       TO 開始月１.
017330     MOVE 開始日Ｗ(1)       TO 開始日１.
017340     MOVE 終了年Ｗ(1)       TO 終了年１.
017350     MOVE 終了月Ｗ(1)       TO 終了月１.
017360     MOVE 終了日Ｗ(1)       TO 終了日１.
017370     MOVE 実日数Ｗ(1)       TO 実日数１.
017380     MOVE 治癒チェックＷ(1) TO 治癒チェック１.
017390     MOVE 中止チェックＷ(1) TO 中止チェック１.
017400     MOVE 転医チェックＷ(1) TO 転医チェック１.
017410**********
017420* ２部位 *
017430**********
017440     MOVE 負傷名Ｗ(2)       TO 負傷名２.
017450     MOVE 負傷年Ｗ(2)       TO 負傷年２.
017460     MOVE 負傷月Ｗ(2)       TO 負傷月２.
017470     MOVE 負傷日Ｗ(2)       TO 負傷日２.
017480     MOVE 初検年Ｗ(2)       TO 初検年２.
017490     MOVE 初検月Ｗ(2)       TO 初検月２.
017500     MOVE 初検日Ｗ(2)       TO 初検日２.
017510     MOVE 開始年Ｗ(2)       TO 開始年２.
017520     MOVE 開始月Ｗ(2)       TO 開始月２.
017530     MOVE 開始日Ｗ(2)       TO 開始日２.
017540     MOVE 終了年Ｗ(2)       TO 終了年２.
017550     MOVE 終了月Ｗ(2)       TO 終了月２.
017560     MOVE 終了日Ｗ(2)       TO 終了日２.
017570     MOVE 実日数Ｗ(2)       TO 実日数２.
017580     MOVE 治癒チェックＷ(2) TO 治癒チェック２.
017590     MOVE 中止チェックＷ(2) TO 中止チェック２.
017600     MOVE 転医チェックＷ(2) TO 転医チェック２.
017610**********
017620* ３部位 *
017630**********
017640     MOVE 負傷名Ｗ(3)       TO 負傷名３.
017650     MOVE 負傷年Ｗ(3)       TO 負傷年３.
017660     MOVE 負傷月Ｗ(3)       TO 負傷月３.
017670     MOVE 負傷日Ｗ(3)       TO 負傷日３.
017680     MOVE 初検年Ｗ(3)       TO 初検年３.
017690     MOVE 初検月Ｗ(3)       TO 初検月３.
017700     MOVE 初検日Ｗ(3)       TO 初検日３.
017710     MOVE 開始年Ｗ(3)       TO 開始年３.
017720     MOVE 開始月Ｗ(3)       TO 開始月３.
017730     MOVE 開始日Ｗ(3)       TO 開始日３.
017740     MOVE 終了年Ｗ(3)       TO 終了年３.
017750     MOVE 終了月Ｗ(3)       TO 終了月３.
017760     MOVE 終了日Ｗ(3)       TO 終了日３.
017770     MOVE 実日数Ｗ(3)       TO 実日数３.
017780     MOVE 治癒チェックＷ(3) TO 治癒チェック３.
017790     MOVE 中止チェックＷ(3) TO 中止チェック３.
017800     MOVE 転医チェックＷ(3) TO 転医チェック３.
017810**********
017820* ４部位 *
017830**********
017840     MOVE 負傷名Ｗ(4)       TO 負傷名４.
017850     MOVE 負傷年Ｗ(4)       TO 負傷年４.
017860     MOVE 負傷月Ｗ(4)       TO 負傷月４.
017870     MOVE 負傷日Ｗ(4)       TO 負傷日４.
017880     MOVE 初検年Ｗ(4)       TO 初検年４.
017890     MOVE 初検月Ｗ(4)       TO 初検月４.
017900     MOVE 初検日Ｗ(4)       TO 初検日４.
017910     MOVE 開始年Ｗ(4)       TO 開始年４.
017920     MOVE 開始月Ｗ(4)       TO 開始月４.
017930     MOVE 開始日Ｗ(4)       TO 開始日４.
017940     MOVE 終了年Ｗ(4)       TO 終了年４.
017950     MOVE 終了月Ｗ(4)       TO 終了月４.
017960     MOVE 終了日Ｗ(4)       TO 終了日４.
017970     MOVE 実日数Ｗ(4)       TO 実日数４.
017980     MOVE 治癒チェックＷ(4) TO 治癒チェック４.
017990     MOVE 中止チェックＷ(4) TO 中止チェック４.
018000     MOVE 転医チェックＷ(4) TO 転医チェック４.
018010**********
018020* ５部位 *
018030**********
018040     MOVE 負傷名Ｗ(5)       TO 負傷名５.
018050     MOVE 負傷年Ｗ(5)       TO 負傷年５.
018060     MOVE 負傷月Ｗ(5)       TO 負傷月５.
018070     MOVE 負傷日Ｗ(5)       TO 負傷日５.
018080     MOVE 初検年Ｗ(5)       TO 初検年５.
018090     MOVE 初検月Ｗ(5)       TO 初検月５.
018100     MOVE 初検日Ｗ(5)       TO 初検日５.
018110     MOVE 開始年Ｗ(5)       TO 開始年５.
018120     MOVE 開始月Ｗ(5)       TO 開始月５.
018130     MOVE 開始日Ｗ(5)       TO 開始日５.
018140     MOVE 終了年Ｗ(5)       TO 終了年５.
018150     MOVE 終了月Ｗ(5)       TO 終了月５.
018160     MOVE 終了日Ｗ(5)       TO 終了日５.
018170     MOVE 実日数Ｗ(5)       TO 実日数５.
018180     MOVE 治癒チェックＷ(5) TO 治癒チェック５.
018190     MOVE 中止チェックＷ(5) TO 中止チェック５.
018200     MOVE 転医チェックＷ(5) TO 転医チェック５.
018210**************
018220* 経過セット *
018230**************
018240     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018250***             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
018260             UNTIL ( 部位ＣＮＴ > 5 )
018270**         MOVE 部位ＣＮＴＷ(部位ＣＮＴ)   TO 経過部位ＣＮＴ(部位ＣＮＴ)
018280**         MOVE 部位区切Ｗ(部位ＣＮＴ)     TO 部位区切(部位ＣＮＴ)
018290         MOVE 印刷経過略称Ｗ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
018300     END-PERFORM.
018310*****************************************
018320*     新規・継続チェックについて        *
018330*   ●新規...初検有り ●継続...初検なし *
018340*****************************************
018350     MOVE 新規チェックＷ    TO 新規チェック.
018360     MOVE 継続チェックＷ    TO 継続チェック.
018370********************
018380* 料金データセット *
018390********************
018400*    ****************************************************************
018410*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
018420*    ****************************************************************
018430     MOVE 初検料ＷＲ                   TO  初検料.
           MOVE 相談料ＷＲ                   TO  初検時相談料.
018440     MOVE 時間外チェックＷ             TO  時間外チェック.
018450     MOVE 休日チェックＷ               TO  休日チェック.
018460     MOVE 深夜チェックＷ               TO  深夜チェック.
018470     MOVE 初検加算料ＷＲ               TO  初検加算料.
           IF (時間外チェックＷ NOT = SPACE) OR (深夜チェックＷ NOT = SPACE) OR
              (休日チェックＷ NOT = SPACE)
              MOVE 初検加算時Ｗ              TO  初検加算時
              MOVE 初検加算区切Ｗ            TO  初検加算区切
              MOVE 初検加算分Ｗ              TO  初検加算分
           END-IF.
018480     MOVE 再検料ＷＲ                   TO  再検料.
018490     MOVE 往療距離ＷＲ                 TO  往療距離.
018500     MOVE 往療回数ＷＲ                 TO  往療回数.
018510     MOVE 往療料ＷＲ                   TO  往療料.
018520     MOVE 夜間チェックＷ               TO  夜間チェック.
018690     MOVE 難路チェックＷ               TO  難路チェック.
018530     MOVE 暴風雨雪チェックＷ           TO  暴風雨雪チェック.
018540     MOVE 往療加算料ＷＲ               TO  往療加算料.
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
018580     MOVE 金属副子加算料ＷＲ           TO  金属副子加算料.
018590     MOVE 施術情報提供料ＷＲ           TO  施術情報提供料.
018600     MOVE 小計Ｗ                       TO 小計.
018610********************
018620* 初回処置料セット *
018630********************
018640     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018650***             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
018660             UNTIL ( 部位ＣＮＴ > 5 )
018670        MOVE 初回処置料ＷＲ(部位ＣＮＴ) TO 初回処置料(部位ＣＮＴ)
018680     END-PERFORM.
018690     MOVE 初回処置料合計Ｗ             TO 初回処置料合計
018700********************
018710* 逓減毎料金セット *
018720********************
018730*    **********
018740*    * １部位 *
018750*    **********
018760     MOVE 後療単価１ＷＲ               TO 後療単価１.
018770     MOVE 後療回数１ＷＲ               TO 後療回数１.
018780     MOVE 後療料１ＷＲ                 TO 後療料１.
018790     MOVE 冷罨法回数１ＷＲ             TO 冷罨法回数１.
018800     MOVE 冷罨法料１ＷＲ               TO 冷罨法料１.
018810     MOVE 温罨法回数１ＷＲ             TO 温罨法回数１.
018820     MOVE 温罨法料１ＷＲ               TO 温罨法料１.
018830     MOVE 電療回数１ＷＲ               TO 電療回数１.
018840     MOVE 電療料１ＷＲ                 TO 電療料１.
018850     MOVE 小計１ＷＲ                   TO 小計１.
018860     IF ( 長期逓減率１ＷＲ NOT = ZERO )
018870        COMPUTE 長期逓減率１ = 長期逓減率１ＷＲ / 100
018880     END-IF.
018890     MOVE 長期込小計１ＷＲ             TO 長期込小計１.
018900*    **********
018910*    * ２部位 *
018920*    **********
018930     MOVE 後療単価２ＷＲ               TO 後療単価２.
018940     MOVE 後療回数２ＷＲ               TO 後療回数２.
018950     MOVE 後療料２ＷＲ                 TO 後療料２.
018960     MOVE 冷罨法回数２ＷＲ             TO 冷罨法回数２.
018970     MOVE 冷罨法料２ＷＲ               TO 冷罨法料２.
018980     MOVE 温罨法回数２ＷＲ             TO 温罨法回数２.
018990     MOVE 温罨法料２ＷＲ               TO 温罨法料２.
019000     MOVE 電療回数２ＷＲ               TO 電療回数２.
019010     MOVE 電療料２ＷＲ                 TO 電療料２.
019020     MOVE 小計２ＷＲ                   TO 小計２.
019030     IF ( 長期逓減率２ＷＲ NOT = ZERO )
019040        COMPUTE 長期逓減率２ = 長期逓減率２ＷＲ / 100
019050     END-IF.
019060     MOVE 長期込小計２ＷＲ             TO 長期込小計２.
019070*    ****************
019080*    * ３部位／８割 *
019090*    ****************
019100     MOVE 後療単価３８ＷＲ             TO 後療単価３８.
019110     MOVE 後療回数３８ＷＲ             TO 後療回数３８.
019120     MOVE 後療料３８ＷＲ               TO 後療料３８.
019130     MOVE 冷罨法回数３８ＷＲ           TO 冷罨法回数３８.
019140     MOVE 冷罨法料３８ＷＲ             TO 冷罨法料３８.
019150     MOVE 温罨法回数３８ＷＲ           TO 温罨法回数３８.
019160     MOVE 温罨法料３８ＷＲ             TO 温罨法料３８.
019170     MOVE 電療回数３８ＷＲ             TO 電療回数３８.
019180     MOVE 電療料３８ＷＲ               TO 電療料３８.
019190     MOVE 小計３８ＷＲ                 TO 小計３８.
019200     MOVE 多部位込小計３８ＷＲ         TO 多部位込小計３８.
019210     IF ( 長期逓減率３８ＷＲ NOT = ZERO )
019220        COMPUTE 長期逓減率３８ = 長期逓減率３８ＷＲ / 100
019230     END-IF.
019240     MOVE 長期込小計３８ＷＲ           TO 長期込小計３８.
      */ 逓減率 0.7→0.6 /42505  /*出さない /42610
      *     IF (施術和暦年月ＷＲ >= 42505)
      *        MOVE "60"                      TO 逓減３８
      *        MOVE "0.6"                     TO 多部位３８
      *        MOVE "==="                     TO 逓減訂正３８ 多部位訂正３８
      *     END-IF.
019250*    ****************
019260*    * ３部位／10割 *
019270*    ****************
019280     MOVE 逓減開始月３０ＷＲ           TO 逓減開始月３０.
019290     MOVE 逓減開始日３０ＷＲ           TO 逓減開始日３０.
019300     MOVE 後療単価３０ＷＲ             TO 後療単価３０.
019310     MOVE 後療回数３０ＷＲ             TO 後療回数３０.
019320     MOVE 後療料３０ＷＲ               TO 後療料３０.
019330     MOVE 冷罨法回数３０ＷＲ           TO 冷罨法回数３０.
019340     MOVE 冷罨法料３０ＷＲ             TO 冷罨法料３０.
019350     MOVE 温罨法回数３０ＷＲ           TO 温罨法回数３０.
019360     MOVE 温罨法料３０ＷＲ             TO 温罨法料３０.
019370     MOVE 電療回数３０ＷＲ             TO 電療回数３０.
019380     MOVE 電療料３０ＷＲ               TO 電療料３０.
019390     MOVE 小計３０ＷＲ                 TO 小計３０.
019400     IF ( 長期逓減率３０ＷＲ NOT = ZERO )
019410        COMPUTE 長期逓減率３０ = 長期逓減率３０ＷＲ / 100
019420     END-IF.
019430     MOVE 長期込小計３０ＷＲ           TO 長期込小計３０.
019440*    ****************
019450*    * ４部位／５割 *
019460*    ****************
019470     MOVE 後療単価４５ＷＲ             TO 後療単価４５.
019480     MOVE 後療回数４５ＷＲ             TO 後療回数４５.
019490     MOVE 後療料４５ＷＲ               TO 後療料４５.
019500     MOVE 冷罨法回数４５ＷＲ           TO 冷罨法回数４５.
019510     MOVE 冷罨法料４５ＷＲ             TO 冷罨法料４５.
019520     MOVE 温罨法回数４５ＷＲ           TO 温罨法回数４５.
019530     MOVE 温罨法料４５ＷＲ             TO 温罨法料４５.
019540     MOVE 電療回数４５ＷＲ             TO 電療回数４５.
019550     MOVE 電療料４５ＷＲ               TO 電療料４５.
019560     MOVE 小計４５ＷＲ                 TO 小計４５.
019570     MOVE 多部位込小計４５ＷＲ         TO 多部位込小計４５.
019580     IF ( 長期逓減率４５ＷＲ NOT = ZERO )
019590        COMPUTE 長期逓減率４５ = 長期逓減率４５ＷＲ / 100
019600     END-IF.
019610     MOVE 長期込小計４５ＷＲ           TO 長期込小計４５.
019620*    ****************
019630*    * ４部位／８割 *
019640*    ****************
019650     MOVE 逓減開始月４８ＷＲ           TO 逓減開始月４８.
019660     MOVE 逓減開始日４８ＷＲ           TO 逓減開始日４８.
019670     MOVE 後療単価４８ＷＲ             TO 後療単価４８.
019680     MOVE 後療回数４８ＷＲ             TO 後療回数４８.
019690     MOVE 後療料４８ＷＲ               TO 後療料４８.
019700     MOVE 冷罨法回数４８ＷＲ           TO 冷罨法回数４８.
019710     MOVE 冷罨法料４８ＷＲ             TO 冷罨法料４８.
019720     MOVE 温罨法回数４８ＷＲ           TO 温罨法回数４８.
019730     MOVE 温罨法料４８ＷＲ             TO 温罨法料４８.
019740     MOVE 電療回数４８ＷＲ             TO 電療回数４８.
019750     MOVE 電療料４８ＷＲ               TO 電療料４８.
019760     MOVE 小計４８ＷＲ                 TO 小計４８.
019770     MOVE 多部位込小計４８ＷＲ         TO 多部位込小計４８.
019780     IF ( 長期逓減率４８ＷＲ NOT = ZERO )
019790        COMPUTE 長期逓減率４８ = 長期逓減率４８ＷＲ / 100
019800     END-IF.
019810     MOVE 長期込小計４８ＷＲ           TO 長期込小計４８.
      */ 逓減率 0.7→0.6 /42505  /*出さない /42610
      *     IF (施術和暦年月ＷＲ >= 42505)
      *        MOVE "60"                      TO 逓減４８
      *        MOVE "0.6"                     TO 多部位４８
      *        MOVE "==="                     TO 逓減訂正４８ 多部位訂正４８
      *     END-IF.
019820*    ****************
019830*    * ４部位／10割 *
019840*    ****************
019850     MOVE 逓減開始月４０ＷＲ           TO 逓減開始月４０.
019860     MOVE 逓減開始日４０ＷＲ           TO 逓減開始日４０.
019870     MOVE 後療単価４０ＷＲ             TO 後療単価４０.
019880     MOVE 後療回数４０ＷＲ             TO 後療回数４０.
019890     MOVE 後療料４０ＷＲ               TO 後療料４０.
019900     MOVE 冷罨法回数４０ＷＲ           TO 冷罨法回数４０.
019910     MOVE 冷罨法料４０ＷＲ             TO 冷罨法料４０.
019920     MOVE 温罨法回数４０ＷＲ           TO 温罨法回数４０.
019930     MOVE 温罨法料４０ＷＲ             TO 温罨法料４０.
019940     MOVE 電療回数４０ＷＲ             TO 電療回数４０.
019950     MOVE 電療料４０ＷＲ               TO 電療料４０.
019960     MOVE 小計４０ＷＲ                 TO 小計４０.
019970     IF ( 長期逓減率４０ＷＲ NOT = ZERO )
019980        COMPUTE 長期逓減率４０ = 長期逓減率４０ＷＲ / 100
019990     END-IF.
020000     MOVE 長期込小計４０ＷＲ           TO 長期込小計４０.
020010*
020020*↓***********************************************************************
020030* ５部位／2.5割の印字は必要ない。
020040*------------------------------------------------------------------------*
020050*    *****************
020060*    * ５部位／2.5割 *
020070*    *****************
020080*     MOVE 後療単価５２ＷＲ             TO 後療単価５２.
020090*     MOVE 後療回数５２ＷＲ             TO 後療回数５２.
020100*     MOVE 後療料５２ＷＲ               TO 後療料５２.
020110*     MOVE 冷罨法回数５２ＷＲ           TO 冷罨法回数５２.
020120*     MOVE 冷罨法料５２ＷＲ             TO 冷罨法料５２.
020130*     MOVE 温罨法回数５２ＷＲ           TO 温罨法回数５２.
020140*     MOVE 温罨法料５２ＷＲ             TO 温罨法料５２.
020150*     MOVE 電療回数５２ＷＲ             TO 電療回数５２.
020160*     MOVE 電療料５２ＷＲ               TO 電療料５２.
020170*     MOVE 小計５２ＷＲ                 TO 小計５２.
020180*     MOVE 多部位込小計５２ＷＲ         TO 多部位込小計５２.
020190*     IF ( 長期逓減率５２ＷＲ NOT = ZERO )
020200*        COMPUTE 長期逓減率５２ = 長期逓減率５２ＷＲ / 100
020210*     END-IF.
020220*     MOVE 長期込小計５２ＷＲ           TO 長期込小計５２.
020230*↑***********************************************************************
020240*
020250**    ****************
020260**    * ５部位／５割 *
020270**    ****************
020280*     MOVE SPACE TO 部位５Ｗ.
020290*     IF ( 小計５５ＷＲ NOT = ZERO )
020300*        MOVE "5) 33 "                  TO 逓減固定５Ｗ
020310*        MOVE "0.33"                    TO 多部位率５Ｗ
020320*        MOVE 逓減開始月５５ＷＲ        TO 逓減開始月５Ｗ
020330*        MOVE 逓減開始日５５ＷＲ        TO 逓減開始日５Ｗ
020340*        MOVE 後療単価５５ＷＲ          TO 後療単価５Ｗ
020350*        MOVE 後療回数５５ＷＲ          TO 後療回数５Ｗ
020360*        MOVE 後療料５５ＷＲ            TO 後療料５Ｗ
020370*        MOVE 冷罨法回数５５ＷＲ        TO 冷罨法回数５Ｗ
020380*        MOVE 冷罨法料５５ＷＲ          TO 冷罨法料５Ｗ
020390*        MOVE 温罨法回数５５ＷＲ        TO 温罨法回数５Ｗ
020400*        MOVE 温罨法料５５ＷＲ          TO 温罨法料５Ｗ
020410*        MOVE 電療回数５５ＷＲ          TO 電療回数５Ｗ
020420*        MOVE 電療料５５ＷＲ            TO 電療料５Ｗ
020430*        MOVE 小計５５ＷＲ              TO 小計５Ｗ
020440*        MOVE 多部位込小計５５ＷＲ      TO 多部位込小計５Ｗ
020450*        IF ( 長期逓減率５５ＷＲ NOT = ZERO )
020460*           COMPUTE 長期逓減率５Ｗ = 長期逓減率５５ＷＲ / 100
020470*        END-IF
020480*        MOVE 長期込小計５５ＷＲ        TO 長期込小計５Ｗ
020490**------------------------------------------------------------------------------------*
020500** 平成14年6月から4部位目・5部位目の逓減率が45→33に変更。
020510** それにより、5部位目（欄外）印字について、平成14年6月より前の場合、45を設定する。
020520**
020530*        IF ( 施術和暦年月ＷＲ < 41406 )
020540*           MOVE "5) 45 "               TO 逓減固定５Ｗ
020550*           MOVE "0.45"                 TO 多部位率５Ｗ
020560*        END-IF
020570**------------------------------------------------------------------------------------*
020580**
020590*        MOVE 部位５Ｗ                  TO 部位５８
020600*     END-IF.
020610*    ****************
020620*    * ５部位／８割 *
020630*    ****************
020640     MOVE SPACE TO 部位５Ｗ.
020650     IF ( 小計５８ＷＲ NOT = ZERO )
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
020860     END-IF.
020870*    ****************
020880*    * ５部位／10割 *
020890*    ****************
020900     MOVE SPACE TO 部位５Ｗ.
020910     IF ( 小計５０ＷＲ NOT = ZERO )
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
021100     END-IF.
021110*
021120     MOVE 適用１Ｗ                     TO 適用１.
021130     MOVE 適用２Ｗ                     TO 適用２.
      *
      */金属副子・運動後療の変更・追加/1805
           IF ( 施術和暦年月ＷＲ >= 43006 )
              INITIALIZE 連金運－キー
019550        MOVE 施術和暦ＷＲ TO 連金運－施術和暦
019560        MOVE 施術年ＷＲ   TO 連金運－施術年
019570        MOVE 施術月ＷＲ   TO 連金運－施術月
019580        MOVE 患者番号ＷＲ TO 連金運－患者番号
019590        MOVE 枝番ＷＲ     TO 連金運－枝番
              MOVE 助成種別ＷＲ TO 連金運－保険種別
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
      *
021140     MOVE レセ－合計                   TO 合計.
021150***     MOVE レセ－一部負担金             TO 一部負担金.
021160*     MOVE レセ－請求金額               TO 請求金額.
021170     MOVE レセ－受給者負担額             TO 受給者負担額.
021180     MOVE レセ－助成請求金額             TO 助成請求金額.
021190*
021202**********************
021210* 給付割合チェック   *
021220**********************
021230*     MOVE 給付老人Ｗ             TO  給付老人.
021240*     MOVE 給付老人チェックＷ     TO  給付老人チェック.
021250*     MOVE 給付７割チェックＷ     TO  給付７割チェック.
021260*     MOVE 給付８割チェックＷ     TO  給付８割チェック.
021270*     MOVE 給付９割チェックＷ     TO  給付９割チェック.
021280*
021290**********************
021300* 施術所データセット *
021310**********************
           MOVE 都道府県ＪＩＳＷ       TO 都道府県番号.
021320     MOVE 柔整師番号Ｗ           TO 柔整師番号.
021330*     MOVE 定額制受理番号Ｗ       TO 定額制受理番号.
021340     MOVE 施術所郵便番号１Ｗ     TO 施術所郵便番号１.
021350     MOVE 施術所郵便番号２Ｗ     TO 施術所郵便番号２.
021360*     MOVE 施術所住所Ｗ           TO 施術所住所１.
021370     MOVE 施術所住所１Ｗ         TO 施術所住所１.
021380     MOVE 施術所住所２Ｗ         TO 施術所住所２.
      */平成２７年１０月施術分より会員番号を印刷/150922
021390     MOVE 接骨師会会員番号Ｗ     TO 接骨師会会員番号.
021400     MOVE 接骨院名Ｗ             TO 接骨院名.
021410     MOVE 代表者カナＷ           TO 代表者カナ.
021420     MOVE 代表者名Ｗ             TO 代表者名.
021430     MOVE 施術所電話番号Ｗ       TO 施術所電話番号.
021440*
021450* / 柔整師・患者委任日 /
021460     MOVE 柔整師年Ｗ             TO 受理年.
021470     MOVE 柔整師月Ｗ             TO 受理月.
021480     MOVE 柔整師日Ｗ             TO 受理日.
021490* ( 委任年月日 印刷するか )
021500     IF ( 連入－委任印刷  = ZERO )
021510        MOVE 患者委任年Ｗ        TO 委任年
021520        MOVE 患者委任月Ｗ        TO 委任月
021530        MOVE 患者委任日Ｗ        TO 委任日
021540     END-IF.
021550*
021560***     MOVE コメント１Ｗ           TO コメント１.
021570***     MOVE コメント２Ｗ           TO コメント２.
021580***     MOVE コメント３Ｗ           TO コメント３.
021590***     MOVE コメント４Ｗ           TO コメント４.
021600***     MOVE コメント５Ｗ           TO コメント５.
021610***     MOVE コメント６Ｗ           TO コメント６.
021620***     MOVE コメント７Ｗ           TO コメント７.
021630*
021640***     MOVE 銀行名支店名Ｗ         TO 銀行名支店名.
021650***     MOVE 預金種別コメントＷ     TO 預金種別.
021660***     MOVE 口座番号Ｗ             TO 口座番号.
021670***     MOVE 口座名義人カナＷ       TO 口座名義人カナ.
021680***     MOVE 口座名義人Ｗ           TO 口座名義人.
             MOVE NC"○"                  TO 振込チェック 普通チェック.
021690*
021700* 最下欄に患者コード
021710***     MOVE 患者番号ＷＲ           TO 患者番号.
021720***     MOVE 枝番ＷＲ               TO 枝番.
021730*
021740* 特別コメント
021750*     MOVE 特別コメントＷ         TO 特別コメント.
021760*
021770* 東京都　右上に「前」印字（高齢者） 14/10～
021780*     MOVE 特別マークＷ           TO 特別マーク.
021790*
021800* 愛知県　特別コメント（４１老）14/10～
021810*     MOVE 特別コメント２Ｗ       TO 特別コメント２.
021820*
022750* レセプト並び順セット *
022760     MOVE 順番Ｗ                 TO 順番.
022770*
021830*-------------------------------------------------------------------------*
021840*--- ※ レセ摘要再セットは、この印刷セットSECTION の最後にやること！ -----*
021850     PERFORM レセ摘要再セット.
021860*-------------------------------------------------------------------------*
021870*
021880*     PERFORM テスト印字処理.
021890*
021900*=== 印刷セット =================================================*
021910*================================================================*
021920 項目初期化 SECTION.
021930*================================================================*
021940     INITIALIZE 施術所情報Ｗ.
021950     INITIALIZE 受診者情報Ｗ.
021960     INITIALIZE 負傷情報Ｗ.
021970     INITIALIZE 料金情報Ｗ.
021980     INITIALIZE 備考情報Ｗ.
021990     INITIALIZE 料金１ＷＲ.
022000     INITIALIZE 料金２ＷＲ.
022010     INITIALIZE 料金３ＷＲ.
022020     MOVE SPACE TO YCB6425P.
022030*****     INITIALIZE YCB6425P.
022040*
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
026460     MOVE 施術和暦ＷＲ       TO 受２－施術和暦.
026470     MOVE 施術年ＷＲ         TO 受２－施術年.
026480     MOVE 施術月ＷＲ         TO 受２－施術月.
026490     MOVE 患者コードＷＲ     TO 受２－患者コード.
026500     READ 受診者情報２Ｆ
           INVALID KEY
              MOVE SPACE           TO 受２－レコード
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
022050*================================================================*
022060 施術所情報取得 SECTION.
022070*================================================================*
022080**************************************************
022090* 本院データを使用し、以下の情報を取得           *
022100* ● 柔整師番号.. 柔整師番号Ｗに格納             *
022110* ● 会員番号 ... 接骨師会会員番号Ｗに格納       *
022120* ● 代表者名 ... 代表者名Ｗに格納               *
022130* ● 住所1,2   ...施術所住所1,2Ｗに格納          *
022140* ● 電話番号 ... 施術所電話番号Ｗに格納         *
022150**************************************************
022160     MOVE ZERO  TO 施情－施術所番号.
022170     READ 施術所情報マスタ
022180     INVALID KEY
022190         CONTINUE
022200     NOT INVALID KEY
022210*
               MOVE 施情－都道府県ＪＩＳ    TO 都道府県ＪＩＳＷ
022250         MOVE 施情－新柔整師番号   TO 柔整師番号Ｗ
022270*
022280         MOVE 施情－接骨師会会員番号  TO 接骨師会会員番号Ｗ
022290         MOVE 施情－郵便番号１        TO 施術所郵便番号１Ｗ
022300         MOVE 施情－郵便番号２        TO 施術所郵便番号２Ｗ
022310         MOVE 施情－接骨院名          TO 接骨院名Ｗ
022320         MOVE 施情－代表者カナ        TO 代表者カナＷ
022330         MOVE 施情－代表者名          TO 代表者名Ｗ
022340*
022350*         STRING 施情－住所１  DELIMITED BY SPACE
022360*                施情－住所２  DELIMITED BY SPACE
022370*           INTO 施術所住所Ｗ
022380*         END-STRING
022390         MOVE 施情－住所１            TO 施術所住所１Ｗ
022400         MOVE 施情－住所２            TO 施術所住所２Ｗ
022410         MOVE 施情－電話番号          TO 施術所電話番号Ｗ
022420* 振込先情報
022430         MOVE 施情－取引先銀行名      TO 取引先銀行名Ｗ
022440         MOVE 施情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
022450         MOVE 施情－預金種別          TO 預金種別Ｗ
022460         MOVE 施情－銀行番号          TO 銀行番号Ｗ
022470         MOVE 施情－店番号            TO 店番号Ｗ
022480         MOVE 施情－口座番号          TO 口座番号Ｗ
022490         MOVE 施情－口座名義人        TO 口座名義人Ｗ
022500         MOVE 施情－口座名義人カナ    TO 口座名義人カナＷ
022510****         MOVE 施情－接骨師会会長名    TO 接骨師会会長名Ｗ
022520     END-READ.
022530*
023520        MOVE ZERO  TO  会情－柔整鍼灸区分
022460        MOVE 27    TO  会情－協会コード.
022470        MOVE ZERO  TO  会情－保険種別.
023530        MOVE ZERO  TO  会情－変更和暦年月
022490        READ 会情報マスタ
022500        NOT INVALID KEY
022510            MOVE 会情－取引先銀行名      TO 取引先銀行名Ｗ
022520            MOVE 会情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
022530            MOVE 会情－預金種別          TO 預金種別Ｗ
022540            MOVE 会情－銀行番号          TO 銀行番号Ｗ
022550            MOVE 会情－店番号            TO 店番号Ｗ
022560            MOVE 会情－口座番号          TO 口座番号Ｗ
022570            MOVE 会情－口座名義人カナ    TO 口座名義人カナＷ
022580            MOVE 会情－口座名義人        TO 口座名義人Ｗ
022590            MOVE 会情－接骨師会会長名    TO 接骨師会会長名Ｗ
022600        END-READ.
022620*
022540* 振込先情報
022550     STRING 取引先銀行名Ｗ     DELIMITED BY SPACE
022560            "  "               DELIMITED BY SIZE
022570            取引先銀行支店名Ｗ DELIMITED BY SPACE
022580            INTO 銀行名支店名Ｗ
022590     END-STRING.
022600     EVALUATE 預金種別Ｗ
022610     WHEN 1
022620         MOVE "普通" TO 預金種別名称Ｗ
022630     WHEN 2
022640         MOVE "当座" TO 預金種別名称Ｗ
022650     WHEN OTHER
022660         MOVE SPACE  TO 預金種別名称Ｗ
022670     END-EVALUATE.
022680     STRING 銀行番号Ｗ     DELIMITED BY SPACE
022690            " "            DELIMITED BY SIZE
022700            店番号Ｗ       DELIMITED BY SPACE
022710            " "            DELIMITED BY SIZE
022720            預金種別名称Ｗ DELIMITED BY SPACE
022730            INTO 預金種別コメントＷ
022740     END-STRING.
022750*
022760* コメント印字
022770     MOVE SPACE TO コメントＷ.
022780     INITIALIZE    コメントＷ.
022970*
022990        MOVE "私が取得した上記金額の受領権を" TO コメント１Ｗ.
023000        STRING "中部柔整師協会"     DELIMITED BY SIZE
023010               " 会長 "             DELIMITED BY SIZE
023020               接骨師会会長名Ｗ     DELIMITED BY SIZE
023030               INTO コメント２Ｗ
023040        END-STRING.
023050        MOVE "に再委任します。"     TO コメント３Ｗ.
023060        PERFORM 日付編集.
023070        MOVE 日付編集Ｗ             TO コメント４Ｗ.
023080        MOVE "柔道整復師"           TO コメント５Ｗ.
023090        STRING "(氏名) "            DELIMITED BY SIZE
023100               代表者名Ｗ           DELIMITED BY SIZE
023110               "      (印)"         DELIMITED BY SIZE
023120               INTO コメント６Ｗ
023130        END-STRING.
023140        MOVE "(住所) 施術証明書と同じ" TO コメント７Ｗ.
022790*
022800*     MOVE "【 備考 】" TO コメント１Ｗ.
022810*
023510*================================================================*
023520 日付編集 SECTION.
023530*
023540     MOVE 施術和暦ＷＲ TO 元－元号区分.
023550     READ 元号マスタ
023560     INVALID KEY
023570         MOVE SPACE TO 元－レコード
023580         INITIALIZE    元－レコード
023590     NOT INVALID KEY
023600         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
023610     END-READ.
023620     IF ( 施術西暦年Ｗ NOT = ZERO )
023630        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
023640     END-IF.
023650*
023660     EVALUATE 施術月ＷＲ
023670     WHEN 4
023680     WHEN 6
023690     WHEN 9
023700     WHEN 11
023710         MOVE 30   TO 月末日Ｗ
023720     WHEN 2
023730         DIVIDE 4 INTO 施術西暦年Ｗ GIVING    商Ｗ
023740                                    REMAINDER 余Ｗ
023750         END-DIVIDE
023760         IF ( 余Ｗ = ZERO )
023770            MOVE 29 TO 月末日Ｗ
023780         ELSE
023790            MOVE 28 TO 月末日Ｗ
023800         END-IF
023810     WHEN 1
023820     WHEN 3
023830     WHEN 5
023840     WHEN 7
023850     WHEN 8
023860     WHEN 10
023870     WHEN 12
023880         MOVE 31   TO 月末日Ｗ
023890     WHEN OTHER
023900         MOVE ZERO TO 月末日Ｗ
023910     END-EVALUATE.
023920*
023930     MOVE 元－元号名称 TO 元号編集Ｗ.
023940     MOVE 施術年ＷＲ   TO 年編集Ｗ.
023950     MOVE 施術月ＷＲ   TO 月編集Ｗ.
023960     MOVE 月末日Ｗ     TO 日編集Ｗ.
023970*
022820*================================================================*
022830 請求先情報取得 SECTION.
022840*================================================================*
022850****************************************************
022860* 連結データから保険者マスタより請求先を取得する。 *
022870* ※市－請求先情報区分=1の場合請求先マスタを使用   *
022880* ● 請求先...... 請求先名称Ｗに格納               *
022890****************************************************
022900     MOVE 助成種別ＷＲ           TO 市－公費種別.
022910     MOVE 費用負担者番号助成ＷＲ TO 市－市町村番号.
022920*
022930     READ 市町村マスタ
022940     INVALID KEY
022950         MOVE SPACE                     TO 請求先名称Ｗ 市町村名称Ｗ
022960     NOT INVALID KEY
022970         IF ( 市－請求先区分 = 1 )
022980            MOVE 助成種別ＷＲ           TO 請先－保険種別
022990            MOVE 費用負担者番号助成ＷＲ TO 請先－保険者番号
023000            READ 請求先マスタ
023010            INVALID KEY
023020                MOVE SPACE              TO 請求先名称Ｗ 市町村名称Ｗ
023030            NOT INVALID KEY
023040                MOVE 請先－保険者名称   TO 請求先名称Ｗ 市町村名称Ｗ
023050            END-READ
023060         ELSE
023070            MOVE 市－市町村名称         TO 請求先名称Ｗ 市町村名称Ｗ
023080         END-IF
023090     END-READ.
023100*
023110*================================================================*
023120 受診者情報取得 SECTION.
023130*================================================================*
023140**************************************************
023150* 連結データから受診者情報Ｆより以下の情報を取得 *
023160* ● 施術年 ..... 施術年Ｗに格納                 *
023170* ● 施術月 ..... 施術月Ｗに格納                 *
023180* ● 患者番号.... 患者番号Ｗに格納※ＦＤ連番用   *
023190* ● 記号 ....... 記号Ｗに格納                   *
023200* ● 番号 ....... 番号Ｗに格納                   *
023210* ● 保険者番号 . 保険者番号Ｗに格納             *
023220* ● 保険種別 ... 保険種別Ｗに格納               *
023230* ● 被保険者カナ.被保険者カナＷに格納           *
023240* ● 被保険者氏名.被保険者氏名Ｗに格納           *
023250* ● 住所１ ......被保険者住所１Ｗに格納         *
023260* ● 住所２ ......被保険者住所２Ｗに格納         *
023270* ● 患者カナ ....患者カナＷに格納               *
023280* ● 患者氏名 ....患者氏名Ｗに格納               *
023290* ● 患者性別 ....区分によりチェックに"○"を格納 *
023300* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
023310* ● 患者年 ......患者年Ｗに格納                 *
023320* ● 患者月 ......患者月Ｗに格納                 *
023330* ● 患者日 ......患者日Ｗに格納                 *
023340* ● 続柄 ........名称マスタより続柄Ｗに取得     *
023350**************************************************
           IF 受－レコード NOT = SPACE
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
      *         END-IF
      *
023450         MOVE 受－施術年       TO 施術年Ｗ
023460         MOVE 受－施術月       TO 施術月Ｗ
023470         MOVE 受－患者番号     TO 患者番号Ｗ
023480*         MOVE 受－記号         TO 記号Ｗ
023490*         MOVE 受－番号         TO 番号Ｗ
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
023500         MOVE 受－保険者番号   TO 保険者番号Ｗ
023510         MOVE 受－保険種別     TO 保険種別Ｗ
               PERFORM 保険種別編集
023520** 全国土木の枝番除
023530         IF ( 受－保険種別 = 01 ) AND ( 受－保険者番号(1:6) = "133033" )
023540            MOVE 受－保険者番号(1:6)  TO 保険者番号Ｗ
023550         END-IF
023560**
023570         MOVE 受－費用負担者番号助成 TO 市町村番号Ｗ
023580         MOVE 受－受益者番号助成     TO 受給者番号Ｗ
023590         MOVE 受－被保険者カナ TO 被保険者カナＷ
023600         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
022240         STRING 受－住所１  DELIMITED BY SPACE
022250                受－住所２  DELIMITED BY SPACE
022260           INTO 被保険者住所Ｗ
022270         END-STRING
023610*         MOVE 受－住所１       TO 被保険者住所１Ｗ
023620*         MOVE 受－住所２       TO 被保険者住所２Ｗ
023630         MOVE 受－患者カナ     TO 患者カナＷ
023640         MOVE 受－患者氏名     TO 患者氏名Ｗ
023650         MOVE 受－患者郵便番号１ TO 郵便番号１Ｗ
023660         MOVE 受－患者郵便番号２ TO 郵便番号２Ｗ
022240*         STRING 受－患者住所１  DELIMITED BY SPACE
022250*                受－患者住所２  DELIMITED BY SPACE
022260*           INTO 患者住所Ｗ
022270*         END-STRING
023670         MOVE 受－患者住所１   TO 患者住所１Ｗ
023680         MOVE 受－患者住所２   TO 患者住所２Ｗ
      */ 電話番号追加 /42505
               IF 受－患者電話番号 NOT = SPACE
      *            STRING "電話:"            DELIMITED BY SIZE
      *                   受－患者電話番号   DELIMITED BY SPACE
      *              INTO 電話番号Ｗ
      *            END-STRING
                  MOVE 受－患者電話番号 TO 電話番号Ｗ
               END-IF
023690         EVALUATE 受－患者性別
023700         WHEN 1
023710             MOVE NC"○"  TO 男チェックＷ
023720         WHEN 2
023730             MOVE NC"○"  TO 女チェックＷ
023740         END-EVALUATE
025110         EVALUATE 受－患者性別
025120         WHEN 1
025130             MOVE NC"男"  TO 性別Ｗ
025140         WHEN 2
025150             MOVE NC"女"  TO 性別Ｗ
025160         END-EVALUATE
023750         EVALUATE 受－患者和暦
023760         WHEN 1
023770             MOVE NC"○"  TO 明治チェックＷ
023780         WHEN 2
023790             MOVE NC"○"  TO 大正チェックＷ
023800         WHEN 3
023810             MOVE NC"○"  TO 昭和チェックＷ
023820         WHEN 4
023830             MOVE NC"○"  TO 平成チェックＷ
023840         END-EVALUATE
023850         EVALUATE 受－患者和暦
023860         WHEN 1
023870             MOVE NC"明治"  TO 元号Ｗ
023880         WHEN 2
023890             MOVE NC"大正"  TO 元号Ｗ
023900         WHEN 3
023910             MOVE NC"昭和"  TO 元号Ｗ
023920         WHEN 4
023930             MOVE NC"平成"  TO 元号Ｗ
023940         END-EVALUATE
023950*
023960         MOVE 受－患者年  TO 患者年Ｗ
023970         MOVE 受－患者月  TO 患者月Ｗ
023980         MOVE 受－患者日  TO 患者日Ｗ
023990* 続柄
024000***         EVALUATE 保険種別ＷＲ
024010* 自衛官は無条件に"本人"
024020***         WHEN  09
024030***             MOVE NC"本人" TO 続柄Ｗ
024040* 退職
024050***         WHEN  08
024060***             IF ( 本人家族区分ＷＲ = 1 ) AND ( 受－世帯主続柄 = 1 )
024070***                MOVE NC"世帯主" TO 続柄Ｗ
024080***             ELSE
024090***                PERFORM 家族続柄セット
024100***             END-IF
024110* その他
024120***         WHEN OTHER
024130***             IF ( 本人家族区分ＷＲ = 1 )
024140***                MOVE NC"本人"   TO 続柄Ｗ
024150***             ELSE
024160***                PERFORM 家族続柄セット
024170***             END-IF
024180***         END-EVALUATE
024190**
025660         IF  本人家族区分ＷＲ = 1 
025670             MOVE NC"本人"    TO 続柄Ｗ
025680         ELSE
025690             MOVE NC"家族"    TO 続柄Ｗ
025700         END-IF
024190**
024200***         PERFORM 特別区分コメントセット
024210**
026580         EVALUATE 受－特別区分
026590         WHEN 1
026600             MOVE NC"○"              TO ７０歳以上チェックＷ
                   MOVE 1                   TO 高齢割合Ｗ
                   IF 受－保険種別 = 05
026500                 MOVE "後期１割負担"  TO 特別コメントＷ
                   ELSE
026500                 MOVE "高齢１割負担"  TO 特別コメントＷ
                   END-IF
026610         WHEN 2
026600             MOVE NC"○"              TO ７０歳以上チェックＷ
                   MOVE 2                   TO 高齢割合Ｗ
                   IF 受－保険種別 = 05
026500                 MOVE "後期２割負担"  TO 特別コメントＷ
                   ELSE
026500                 MOVE "高齢２割負担"  TO 特別コメントＷ
                   END-IF
026621         WHEN 3
026600             MOVE NC"○"              TO ７０歳以上チェックＷ
                   MOVE 3                   TO 高齢割合Ｗ
                   IF 受－保険種別 = 05
026500                 MOVE "後期３割負担"  TO 特別コメントＷ
                   ELSE
026500                 MOVE "高齢３割負担"  TO 特別コメントＷ
                   END-IF
026630         WHEN 6
026600             MOVE NC"○"              TO 未就学チェックＷ
026500             MOVE "未就学２割負担"    TO 特別コメントＷ
026650         END-EVALUATE
024220     END-IF.
024230*
024240* 保険種別チェック
024250     EVALUATE 保険種別ＷＲ
024260     WHEN 02
024270         MOVE NC"○" TO 政チェックＷ
024280     WHEN 03
024290         MOVE NC"○" TO 組チェックＷ
024300     WHEN 06
024310         MOVE NC"○" TO 日チェックＷ
024320     WHEN 07
024330         MOVE NC"○" TO 船チェックＷ
024340     WHEN 04
024350     WHEN 09
024360         MOVE NC"○" TO 共チェックＷ
024370     WHEN 01
024380         MOVE NC"○" TO 国チェックＷ
024390     WHEN 08
024400         MOVE NC"○" TO 退チェックＷ
024390     WHEN 05
024400         MOVE NC"○" TO 高チェックＷ
024410     END-EVALUATE.
024420*
024430* 助成種別チェック
024440     EVALUATE 助成種別ＷＲ
024450     WHEN  50
024460         CONTINUE
024470     WHEN  51
024480        MOVE NC"○" TO 老チェックＷ
024490     WHEN  52
024500        MOVE NC"○" TO 母チェックＷ
024510     WHEN  53
024520        MOVE NC"○" TO 障チェックＷ
024530     WHEN  54
024540        MOVE NC"○" TO 傷チェックＷ
024550     WHEN  55
024560        MOVE NC"○" TO 乳チェックＷ
024570     WHEN  OTHER
024580            CONTINUE
024590     END-EVALUATE.
024600*
      ***     IF 受－助成種別 = 60
014760***         PERFORM 助成印取得
      ***         IF 助成印Ｗ = NC"福"
      ***             MOVE NC"○" TO 福チェックＷ
      ***             MOVE SPACE  TO 助成印Ｗ
      ***         ELSE
      ***             MOVE NC"○" TO 助成チェックＷ
      ***         END-IF
      ***     END-IF.
038330*================================================================*
038340 保険種別編集 SECTION.
038350*================================================================*
           EVALUATE 保険種別Ｗ
           WHEN 1
               IF 受－保険者番号(3:1) = 3
                   MOVE NC"国組"   TO 保険種別親Ｗ
               ELSE
                   MOVE NC"国"     TO 保険種別親Ｗ
               END-IF
           WHEN 2
               IF (受－保険者番号(1:2) = 01) AND
                  (受－保険者番号(5:4) NOT = SPACE)
                   MOVE NC"協"     TO 保険種別親Ｗ
               ELSE
                   MOVE NC"政"     TO 保険種別親Ｗ
               END-IF
           WHEN 3
               MOVE NC"組"         TO 保険種別親Ｗ
           WHEN 4
               MOVE NC"共"         TO 保険種別親Ｗ
           WHEN 5
               MOVE NC"後期"       TO 保険種別親Ｗ
           WHEN 6
               MOVE NC"日"         TO 保険種別親Ｗ
           WHEN 7
               MOVE NC"船"         TO 保険種別親Ｗ
           WHEN 8
               MOVE NC"国退"       TO 保険種別親Ｗ
           WHEN 9
               MOVE NC"自"         TO 保険種別親Ｗ
           END-EVALUATE.
      *
           PERFORM 助成印取得２.
           IF 助成印Ｗ NOT = SPACE
               STRING 保険種別親Ｗ   DELIMITED BY SPACE
                      NC"（"         DELIMITED BY SIZE
                      助成印Ｗ       DELIMITED BY SPACE
                      NC"）"         DELIMITED BY SIZE
                 INTO 保険種別編集Ｗ
               END-STRING
           ELSE
               MOVE 保険種別親Ｗ   TO 保険種別編集Ｗ
           END-IF.
      *
038330*================================================================*
038340 助成印取得２ SECTION.
038350*================================================================*
039830     MOVE SPACE TO 助成印Ｗ.
039840*
039850     EVALUATE 助成種別ＷＲ 
039860*** 生保 (生保はその他扱いで、該当なし)
039870     WHEN  50
039880         CONTINUE
039970*** 母子
039980     WHEN  52
040030         MOVE NC"母"    TO 助成印Ｗ
040050*** 身障
040060     WHEN  53
040070            MOVE NC"障"    TO 助成印Ｗ
040110*** 乳幼児 
040120     WHEN  55
040140            MOVE NC"子"    TO 助成印Ｗ
040150*** その他
040160     WHEN  60
040170***            MOVE NC"他"    TO 助成印Ｗ
040171         IF 費用負担者番号助成ＷＲ(1:4) = "8923"
040172             MOVE NC"福"    TO 助成印Ｗ
040173         END-IF
040180     WHEN  OTHER
040190            CONTINUE
040200     END-EVALUATE.
040210*
040211     IF (( 保険種別ＷＲ = 05 ) AND ( 保険者番号ＷＲ(1:5) = "39231" ) AND
040212         ( 受－助成負担金免除 = 1 ))
040213         MOVE NC"福"    TO 助成印Ｗ
040214     END-IF.
040215*
024610*================================================================*
024620 家族続柄セット SECTION.
024630*
024640     MOVE 05       TO 名－区分コード.
024650     MOVE 受－続柄 TO 名－名称コード.
024660     READ 名称マスタ
024670     INVALID KEY
024680         MOVE SPACE    TO 続柄Ｗ
024690     NOT INVALID KEY
024700         MOVE 名－略称 TO 続柄Ｗ
024710     END-READ.
024720*
024730*================================================================*
024740 特別区分コメントセット SECTION.
024750*----------------------------------------------------------------*
024760* 14/10～　特別区分コメント印字
024770*----------------------------------------------------------------*
024780     IF ( 受－施術和暦年月 >= 41410 )
024790        IF ( 受－公費種別 = ZERO )
024800           EVALUATE 受－特別区分
024810           WHEN 1
024820              MOVE "70歳以上 1割"  TO 特別コメントＷ
024830           WHEN 2
024840              MOVE "70歳以上 2割"  TO 特別コメントＷ
024841           WHEN 3
024842              MOVE "70歳以上 3割"  TO 特別コメントＷ
024850           WHEN 6
024861              IF 受－施術和暦年月 < 42004
024863                 MOVE "3歳未満"       TO 特別コメントＷ
024864              ELSE
025063                 MOVE "未就学２割負担"  TO 特別コメント２Ｗ
024867              END-IF
024870           END-EVALUATE
024880        END-IF
024890     END-IF.
024900*
024910*---  市町村独自仕様 -----*
024920* 東京都のみ→ 特別区分1,2,3(高齢者）の時、「前」を右上に印字
024930*              親が老人の時、保険者番号欄には、２７番号を印字
024940     IF ( 受－施術和暦年月 >= 41410 )
024950        IF ( 受－費用負担者番号助成(3:2) = "13" )
024960           IF ( 受－公費種別 = ZERO )
024970              IF ( 受－特別区分 = 1 OR 2 OR 3 )
024980                 MOVE NC"前" TO 特別マークＷ
024990              END-IF
025000           ELSE
025010              MOVE 受－費用負担者番号  TO 保険者番号Ｗ
025020           END-IF
025030        END-IF
025040     END-IF.
025050*
025060* 愛知県のみ→ 41老人の負担率を適用欄に印字
025070     IF ( 受－施術和暦年月 >= 41410 )
025080        IF ( 受－費用負担者番号助成(3:2) = "23" ) AND
025090           ( 受－助成種別 = 51 )
025142               EVALUATE 受－助成負担金免除
025143               WHEN 2
025144                  MOVE "41老人 ２割"   TO 特別コメント２Ｗ
025145               WHEN 3
025146                  MOVE "41老人 ３割"   TO 特別コメント２Ｗ
025147               WHEN OTHER
025148                  MOVE "41老人 １割"   TO 特別コメント２Ｗ
025149               END-EVALUATE
025152        END-IF
025160     END-IF.
025170*
027472* 20/04～　後期高齢特別区分コメント印字
027476     IF 受－施術和暦年月 >= 42004
027477         IF 受－保険種別 = 05
027478            EVALUATE 受－特別区分
027479            WHEN 1
027480               MOVE "高齢者１割"  TO 特別コメント２Ｗ
027481            WHEN 2
027482               MOVE "高齢者２割"  TO 特別コメント２Ｗ
027483            WHEN 3
027484               MOVE "高齢者３割"  TO 特別コメント２Ｗ
027492            END-EVALUATE
027493         END-IF
027494     END-IF.
      *
025180*================================================================*
025190 負傷データ取得 SECTION.
025200*================================================================*
025210**************************************************
025220* 連結データから負傷データＦより以下の情報を取得 *
025230* ● 負傷名...部位＋負傷種別にて加工して格納     *
025240* ● 負傷年.......負傷年Ｗ                       *
025250* ● 負傷月.......負傷月Ｗ                       *
025260* ● 負傷日.......負傷日Ｗ                       *
025270* ● 開始年.......初検年Ｗ                       *
025280* ● 開始月.......初検月Ｗ                       *
025290* ● 開始日.......初検日Ｗ                       *
025300* ● 終了年.......終了年Ｗ                       *
025310* ● 終了月.......終了月Ｗ                       *
025320* ● 終了日.......終了日Ｗ                       *
025330* ● 実日数.......実日数Ｗ                       *
025340* ● 転帰区分 ....区分によりチェックに"○"を格納 *
025350* ● 金属副子 ....区分によりチェックに"○"を格納 *
025360* ● 経過コード...経過マスタより取得             *
025370**************************************************
           IF 負－レコード NOT = SPACE
025470         MOVE 負－部位数                   TO 部位数Ｗ
025480         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
025490                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
025500             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別Ｗ(部位ＣＮＴ)
025510             MOVE 負－部位(部位ＣＮＴ)     TO 部位Ｗ(部位ＣＮＴ)
025520             MOVE 負－左右区分(部位ＣＮＴ) TO 左右区分Ｗ(部位ＣＮＴ)
025530             MOVE 負－負傷位置番号(部位ＣＮＴ)
025540                                           TO 負傷位置番号Ｗ(部位ＣＮＴ)
025550********************************************************
025560* 注）全柔...部位名1+負傷種別＋部位名2にて加工して格納 *
025570********************************************************
025580* 負傷種別
025590             MOVE SPACE                     TO 負傷名称Ｗ
025600             MOVE 03                        TO 名－区分コード
025610             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
025620             READ 名称マスタ
025630             INVALID KEY
025640                 MOVE SPACE        TO 負傷名称Ｗ
025650             NOT INVALID KEY
025660                 MOVE 名－正式名称 TO 負傷名称Ｗ
025670             END-READ
025680* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
032680*
032690             PERFORM 部位名称埋込処理
025870*
025880             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
025890             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
025900             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
025910             MOVE 負－開始年(部位ＣＮＴ)   TO 初検年Ｗ(部位ＣＮＴ)
025920             MOVE 負－開始月(部位ＣＮＴ)   TO 初検月Ｗ(部位ＣＮＴ)
025930             MOVE 負－開始日(部位ＣＮＴ)   TO 初検日Ｗ(部位ＣＮＴ)
025940             IF ( 負－転帰区分(部位ＣＮＴ) = 9 )
025950                 MOVE 99                   TO 終了年Ｗ(部位ＣＮＴ)
025960                 MOVE 99                   TO 終了月Ｗ(部位ＣＮＴ)
025970                 MOVE 99                   TO 終了日Ｗ(部位ＣＮＴ)
025980             ELSE
025990                 MOVE 負－終了年(部位ＣＮＴ)   TO 終了年Ｗ(部位ＣＮＴ)
026000                 MOVE 負－終了月(部位ＣＮＴ)   TO 終了月Ｗ(部位ＣＮＴ)
026010                 MOVE 負－終了日(部位ＣＮＴ)   TO 終了日Ｗ(部位ＣＮＴ)
026020             END-IF
026030* 経過略称取得
026040             MOVE 01                         TO 経－区分コード
026050             MOVE 負－経過コード(部位ＣＮＴ) TO 経－経過コード
026060             READ 経過マスタ
026070             INVALID KEY
026080                 MOVE ZERO            TO 部位ＣＮＴＷ(部位ＣＮＴ)
026090                 MOVE SPACE           TO 部位区切Ｗ(部位ＣＮＴ)
026100                 MOVE SPACE           TO 経過略称Ｗ(部位ＣＮＴ)
026110             NOT INVALID KEY
026120                 EVALUATE 部位ＣＮＴ
026130                 WHEN 1
026140                     MOVE NC"①" TO 経過部位Ｗ
026150                 WHEN 2
026160                     MOVE NC"②" TO 経過部位Ｗ
026170                 WHEN 3
026180                     MOVE NC"③" TO 経過部位Ｗ
026190                 WHEN 4
026200                     MOVE NC"④" TO 経過部位Ｗ
026210                 WHEN 5
026220                     MOVE NC"⑤" TO 経過部位Ｗ
026230                 END-EVALUATE
026240                 STRING  経過部位Ｗ     DELIMITED BY SPACE
026250                         経－経過略称   DELIMITED BY SPACE
026260                        INTO 印刷経過略称Ｗ(部位ＣＮＴ)
026270                 END-STRING
026280             END-READ
026290*
026300             MOVE 負－転帰区分(部位ＣＮＴ) TO 転帰区分Ｗ(部位ＣＮＴ)
026310             EVALUATE 負－転帰区分(部位ＣＮＴ)
026320             WHEN 1
026330             WHEN 2
026340                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
026350             WHEN 3
026360                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
026370             WHEN 4
026380                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
026390             END-EVALUATE
026400*
                   MOVE レセ－部位実日数(部位ＣＮＴ) TO 実日数Ｗ(部位ＣＮＴ)
026410         END-PERFORM
026420* 新規/継続 チェック
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
026480* 枝番判定用
026490         MOVE 負－開始診療日手動区分   TO  開始診療日手動区分Ｗ
026500*
026510* 負傷原因印刷区分
026520         MOVE 負－レセ負傷原因印刷区分 TO レセ負傷原因印刷区分Ｗ
027880         MOVE 負－レセ長期理由印刷区分 TO レセ長期理由印刷区分Ｗ
026530*
026540     END-IF.
026550*
026560*================================================================*
030910 部位名称埋込処理 SECTION.
030920*
006490     STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
009980            負傷名称Ｗ                    DELIMITED BY SPACE
006500            レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
006520       INTO 負傷名Ｗ(部位ＣＮＴ)
006570     END-STRING.
026720*
026730*================================================================*
026740 料金情報取得 SECTION.
026750*================================================================*
026760********************
026770* 料金データセット *
026780********************
026790*    ****************************************************************
026800*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
026810*    ****************************************************************
026820     MOVE レセ－初検料                 TO 初検料ＷＲ.
026830     IF ( レセ－時間外 = 1 )
026840         MOVE NC"○"                   TO 時間外チェックＷ
026850     END-IF.
026860     IF ( レセ－休日 = 1 )
026870         MOVE NC"○"                   TO 休日チェックＷ
026880     END-IF.
026890     IF ( レセ－深夜 = 1 )
026900         MOVE NC"○"                   TO 深夜チェックＷ
026910     END-IF.
           MOVE レセ－初検時相談料           TO 相談料ＷＲ.
026920*
026930     MOVE レセ－初検加算料             TO  初検加算料ＷＲ.
026940     MOVE レセ－再検料                 TO  再検料ＷＲ.
026950     MOVE レセ－往療距離               TO  往療距離ＷＲ.
026960     MOVE レセ－往療回数               TO  往療回数ＷＲ.
026970     MOVE レセ－往療料                 TO  往療料ＷＲ.
026980     MOVE レセ－往療加算料             TO  往療加算料ＷＲ.
026990*
027000     IF ( レセ－夜間 = 1 )
027010         MOVE NC"○"                   TO 夜間チェックＷ
027020     END-IF.
029870     IF ( レセ－難路 = 1 )
029880         MOVE NC"○"                   TO 難路チェックＷ
029890     END-IF.
027030     IF ( レセ－暴風雨雪 = 1 )
027040         MOVE NC"○"                   TO 暴風雨雪チェックＷ
027050     END-IF.
027060*
027070     MOVE レセ－金属副子加算料         TO  金属副子加算料ＷＲ.
027080*
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
027180*
027190     MOVE レセ－施術情報提供料         TO  施術情報提供料ＷＲ.
027200* 小計
027210     MOVE レセ－小計                   TO 小計Ｗ.
027220********************
027230* 初回処置料セット *
027240********************
027250     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
027260             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
027270         MOVE レセ－初回処置料(部位ＣＮＴ) TO 初回処置料ＷＲ(部位ＣＮＴ)
027280     END-PERFORM.
027290     MOVE レセ－初回処置料合計       TO 初回処置料合計Ｗ.
027300********************
027310* 逓減毎料金セット *
027320********************
027330*    **********
027340*    * １部位 *
027350*    **********
027360     MOVE レセ－後療単価１             TO 後療単価１ＷＲ.
027370     MOVE レセ－後療回数１             TO 後療回数１ＷＲ.
027380     MOVE レセ－後療料１               TO 後療料１ＷＲ.
027390     MOVE レセ－冷罨法回数１           TO 冷罨法回数１ＷＲ.
027400     MOVE レセ－冷罨法料１             TO 冷罨法料１ＷＲ.
027410     MOVE レセ－温罨法回数１           TO 温罨法回数１ＷＲ.
027420     MOVE レセ－温罨法料１             TO 温罨法料１ＷＲ.
027430     MOVE レセ－電療回数１             TO 電療回数１ＷＲ.
027440     MOVE レセ－電療料１               TO 電療料１ＷＲ.
027450     MOVE レセ－小計１                 TO 小計１ＷＲ.
027460     MOVE レセ－長期逓減率１           TO 長期逓減率１ＷＲ.
027470     MOVE レセ－長期込小計１           TO 長期込小計１ＷＲ.
027480*    **********
027490*    * ２部位 *
027500*    **********
027510     MOVE レセ－後療単価２             TO 後療単価２ＷＲ.
027520     MOVE レセ－後療回数２             TO 後療回数２ＷＲ.
027530     MOVE レセ－後療料２               TO 後療料２ＷＲ.
027540     MOVE レセ－冷罨法回数２           TO 冷罨法回数２ＷＲ.
027550     MOVE レセ－冷罨法料２             TO 冷罨法料２ＷＲ.
027560     MOVE レセ－温罨法回数２           TO 温罨法回数２ＷＲ.
027570     MOVE レセ－温罨法料２             TO 温罨法料２ＷＲ.
027580     MOVE レセ－電療回数２             TO 電療回数２ＷＲ.
027590     MOVE レセ－電療料２               TO 電療料２ＷＲ.
027600     MOVE レセ－小計２                 TO 小計２ＷＲ.
027610     MOVE レセ－長期逓減率２           TO 長期逓減率２ＷＲ.
027620     MOVE レセ－長期込小計２           TO 長期込小計２ＷＲ.
027630*    ****************
027640*    * ３部位／８割 *
027650*    ****************
027660     MOVE レセ－後療単価３８             TO 後療単価３８ＷＲ.
027670     MOVE レセ－後療回数３８             TO 後療回数３８ＷＲ.
027680     MOVE レセ－後療料３８               TO 後療料３８ＷＲ.
027690     MOVE レセ－冷罨法回数３８           TO 冷罨法回数３８ＷＲ.
027700     MOVE レセ－冷罨法料３８             TO 冷罨法料３８ＷＲ.
027710     MOVE レセ－温罨法回数３８           TO 温罨法回数３８ＷＲ.
027720     MOVE レセ－温罨法料３８             TO 温罨法料３８ＷＲ.
027730     MOVE レセ－電療回数３８             TO 電療回数３８ＷＲ.
027740     MOVE レセ－電療料３８               TO 電療料３８ＷＲ.
027750     MOVE レセ－小計３８                 TO 小計３８ＷＲ.
027760     MOVE レセ－多部位込小計３８         TO 多部位込小計３８ＷＲ.
027770     MOVE レセ－長期逓減率３８           TO 長期逓減率３８ＷＲ.
027780     MOVE レセ－長期込小計３８           TO 長期込小計３８ＷＲ.
027790*    ****************
027800*    * ３部位／10割 *
027810*    ****************
027820     MOVE レセ－逓減開始月３０           TO 逓減開始月３０ＷＲ.
027830     MOVE レセ－逓減開始日３０           TO 逓減開始日３０ＷＲ.
027840     MOVE レセ－後療単価３０             TO 後療単価３０ＷＲ.
027850     MOVE レセ－後療回数３０             TO 後療回数３０ＷＲ.
027860     MOVE レセ－後療料３０               TO 後療料３０ＷＲ.
027870     MOVE レセ－冷罨法回数３０           TO 冷罨法回数３０ＷＲ.
027880     MOVE レセ－冷罨法料３０             TO 冷罨法料３０ＷＲ.
027890     MOVE レセ－温罨法回数３０           TO 温罨法回数３０ＷＲ.
027900     MOVE レセ－温罨法料３０             TO 温罨法料３０ＷＲ.
027910     MOVE レセ－電療回数３０             TO 電療回数３０ＷＲ.
027920     MOVE レセ－電療料３０               TO 電療料３０ＷＲ.
027930     MOVE レセ－小計３０                 TO 小計３０ＷＲ.
027940     MOVE レセ－長期逓減率３０           TO 長期逓減率３０ＷＲ.
027950     MOVE レセ－長期込小計３０           TO 長期込小計３０ＷＲ.
027960*    ****************
027970*    * ４部位／５割 *
027980*    ****************
027990     MOVE レセ－後療単価４５             TO 後療単価４５ＷＲ.
028000     MOVE レセ－後療回数４５             TO 後療回数４５ＷＲ.
028010     MOVE レセ－後療料４５               TO 後療料４５ＷＲ.
028020     MOVE レセ－冷罨法回数４５           TO 冷罨法回数４５ＷＲ.
028030     MOVE レセ－冷罨法料４５             TO 冷罨法料４５ＷＲ.
028040     MOVE レセ－温罨法回数４５           TO 温罨法回数４５ＷＲ.
028050     MOVE レセ－温罨法料４５             TO 温罨法料４５ＷＲ.
028060     MOVE レセ－電療回数４５             TO 電療回数４５ＷＲ.
028070     MOVE レセ－電療料４５               TO 電療料４５ＷＲ.
028080     MOVE レセ－小計４５                 TO 小計４５ＷＲ.
028090     MOVE レセ－多部位込小計４５         TO 多部位込小計４５ＷＲ.
028100     MOVE レセ－長期逓減率４５           TO 長期逓減率４５ＷＲ.
028110     MOVE レセ－長期込小計４５           TO 長期込小計４５ＷＲ.
028120*    ****************
028130*    * ４部位／８割 *
028140*    ****************
028150     MOVE レセ－逓減開始月４８           TO 逓減開始月４８ＷＲ.
028160     MOVE レセ－逓減開始日４８           TO 逓減開始日４８ＷＲ.
028170     MOVE レセ－後療単価４８             TO 後療単価４８ＷＲ.
028180     MOVE レセ－後療回数４８             TO 後療回数４８ＷＲ.
028190     MOVE レセ－後療料４８               TO 後療料４８ＷＲ.
028200     MOVE レセ－冷罨法回数４８           TO 冷罨法回数４８ＷＲ.
028210     MOVE レセ－冷罨法料４８             TO 冷罨法料４８ＷＲ.
028220     MOVE レセ－温罨法回数４８           TO 温罨法回数４８ＷＲ.
028230     MOVE レセ－温罨法料４８             TO 温罨法料４８ＷＲ.
028240     MOVE レセ－電療回数４８             TO 電療回数４８ＷＲ.
028250     MOVE レセ－電療料４８               TO 電療料４８ＷＲ.
028260     MOVE レセ－小計４８                 TO 小計４８ＷＲ.
028270     MOVE レセ－多部位込小計４８         TO 多部位込小計４８ＷＲ.
028280     MOVE レセ－長期逓減率４８           TO 長期逓減率４８ＷＲ.
028290     MOVE レセ－長期込小計４８           TO 長期込小計４８ＷＲ.
028300*    ****************
028310*    * ４部位／10割 *
028320*    ****************
028330     MOVE レセ－逓減開始月４０           TO 逓減開始月４０ＷＲ.
028340     MOVE レセ－逓減開始日４０           TO 逓減開始日４０ＷＲ.
028350     MOVE レセ－後療単価４０             TO 後療単価４０ＷＲ.
028360     MOVE レセ－後療回数４０             TO 後療回数４０ＷＲ.
028370     MOVE レセ－後療料４０               TO 後療料４０ＷＲ.
028380     MOVE レセ－冷罨法回数４０           TO 冷罨法回数４０ＷＲ.
028390     MOVE レセ－冷罨法料４０             TO 冷罨法料４０ＷＲ.
028400     MOVE レセ－温罨法回数４０           TO 温罨法回数４０ＷＲ.
028410     MOVE レセ－温罨法料４０             TO 温罨法料４０ＷＲ.
028420     MOVE レセ－電療回数４０             TO 電療回数４０ＷＲ.
028430     MOVE レセ－電療料４０               TO 電療料４０ＷＲ.
028440     MOVE レセ－小計４０                 TO 小計４０ＷＲ.
028450     MOVE レセ－長期逓減率４０           TO 長期逓減率４０ＷＲ.
028460     MOVE レセ－長期込小計４０           TO 長期込小計４０ＷＲ.
028470*    *****************
028480*    * ５部位／2.5割 *
028490*    *****************
028500     MOVE レセ－後療単価５２             TO 後療単価５２ＷＲ.
028510     MOVE レセ－後療回数５２             TO 後療回数５２ＷＲ.
028520     MOVE レセ－後療料５２               TO 後療料５２ＷＲ.
028530     MOVE レセ－冷罨法回数５２           TO 冷罨法回数５２ＷＲ.
028540     MOVE レセ－冷罨法料５２             TO 冷罨法料５２ＷＲ.
028550     MOVE レセ－温罨法回数５２           TO 温罨法回数５２ＷＲ.
028560     MOVE レセ－温罨法料５２             TO 温罨法料５２ＷＲ.
028570     MOVE レセ－電療回数５２             TO 電療回数５２ＷＲ.
028580     MOVE レセ－電療料５２               TO 電療料５２ＷＲ.
028590     MOVE レセ－小計５２                 TO 小計５２ＷＲ.
028600     MOVE レセ－多部位込小計５２         TO 多部位込小計５２ＷＲ.
028610     MOVE レセ－長期逓減率５２           TO 長期逓減率５２ＷＲ.
028620     MOVE レセ－長期込小計５２           TO 長期込小計５２ＷＲ.
028630*    ****************
028640*    * ５部位／５割 *
028650*    ****************
028660     MOVE レセ－逓減開始月５５           TO 逓減開始月５５ＷＲ.
028670     MOVE レセ－逓減開始日５５           TO 逓減開始日５５ＷＲ.
028680     MOVE レセ－後療単価５５             TO 後療単価５５ＷＲ.
028690     MOVE レセ－後療回数５５             TO 後療回数５５ＷＲ.
028700     MOVE レセ－後療料５５               TO 後療料５５ＷＲ.
028710     MOVE レセ－冷罨法回数５５           TO 冷罨法回数５５ＷＲ.
028720     MOVE レセ－冷罨法料５５             TO 冷罨法料５５ＷＲ.
028730     MOVE レセ－温罨法回数５５           TO 温罨法回数５５ＷＲ.
028740     MOVE レセ－温罨法料５５             TO 温罨法料５５ＷＲ.
028750     MOVE レセ－電療回数５５             TO 電療回数５５ＷＲ.
028760     MOVE レセ－電療料５５               TO 電療料５５ＷＲ.
028770     MOVE レセ－小計５５                 TO 小計５５ＷＲ.
028780     MOVE レセ－多部位込小計５５         TO 多部位込小計５５ＷＲ.
028790     MOVE レセ－長期逓減率５５           TO 長期逓減率５５ＷＲ.
028800     MOVE レセ－長期込小計５５           TO 長期込小計５５ＷＲ.
028810*    ****************
028820*    * ５部位／８割 *
028830*    ****************
028840     MOVE レセ－逓減開始月５８           TO 逓減開始月５８ＷＲ.
028850     MOVE レセ－逓減開始日５８           TO 逓減開始日５８ＷＲ.
028860     MOVE レセ－後療単価５８             TO 後療単価５８ＷＲ.
028870     MOVE レセ－後療回数５８             TO 後療回数５８ＷＲ.
028880     MOVE レセ－後療料５８               TO 後療料５８ＷＲ.
028890     MOVE レセ－冷罨法回数５８           TO 冷罨法回数５８ＷＲ.
028900     MOVE レセ－冷罨法料５８             TO 冷罨法料５８ＷＲ.
028910     MOVE レセ－温罨法回数５８           TO 温罨法回数５８ＷＲ.
028920     MOVE レセ－温罨法料５８             TO 温罨法料５８ＷＲ.
028930     MOVE レセ－電療回数５８             TO 電療回数５８ＷＲ.
028940     MOVE レセ－電療料５８               TO 電療料５８ＷＲ.
028950     MOVE レセ－小計５８                 TO 小計５８ＷＲ.
028960     MOVE レセ－多部位込小計５８         TO 多部位込小計５８ＷＲ.
028970     MOVE レセ－長期逓減率５８           TO 長期逓減率５８ＷＲ.
028980     MOVE レセ－長期込小計５８           TO 長期込小計５８ＷＲ.
028990*    ****************
029000*    * ５部位／10割 *
029010*    ****************
029020     MOVE レセ－逓減開始月５０           TO 逓減開始月５０ＷＲ.
029030     MOVE レセ－逓減開始日５０           TO 逓減開始日５０ＷＲ.
029040     MOVE レセ－後療単価５０             TO 後療単価５０ＷＲ.
029050     MOVE レセ－後療回数５０             TO 後療回数５０ＷＲ.
029060     MOVE レセ－後療料５０               TO 後療料５０ＷＲ.
029070     MOVE レセ－冷罨法回数５０           TO 冷罨法回数５０ＷＲ.
029080     MOVE レセ－冷罨法料５０             TO 冷罨法料５０ＷＲ.
029090     MOVE レセ－温罨法回数５０           TO 温罨法回数５０ＷＲ.
029100     MOVE レセ－温罨法料５０             TO 温罨法料５０ＷＲ.
029110     MOVE レセ－電療回数５０             TO 電療回数５０ＷＲ.
029120     MOVE レセ－電療料５０               TO 電療料５０ＷＲ.
029130     MOVE レセ－小計５０                 TO 小計５０ＷＲ.
029140     MOVE レセ－長期逓減率５０           TO 長期逓減率５０ＷＲ.
029150     MOVE レセ－長期込小計５０           TO 長期込小計５０ＷＲ.
029160*
029170*================================================================*
029180 施術記録取得 SECTION.
029190*================================================================*
029200************************************************************
029210* 作１データから負傷データＦより以下の情報を取得           *
029220* ● 初検加算 .....区分によりチェックに"○"を格納...複数可 *
029230* ● 往療加算 .....区分によりチェックに"○"を格納...複数可 *
029240************************************************************
029250     MOVE  SPACE  TO  初日再検フラグ.
029260     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 部位数Ｗ
029270         IF ( 施術年Ｗ = 初検年Ｗ(部位ＣＮＴ) ) AND
029280            ( 施術月Ｗ = 初検月Ｗ(部位ＣＮＴ) )
029290             MOVE 患者番号ＷＲ          TO 施記－患者番号
029300             MOVE 枝番ＷＲ              TO 施記－枝番
029310             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
029320             MOVE 初検年Ｗ(部位ＣＮＴ)  TO 開始年Ｗ(部位ＣＮＴ) 施記－施術年
029330             MOVE 初検月Ｗ(部位ＣＮＴ)  TO 開始月Ｗ(部位ＣＮＴ) 施記－施術月
029340             MOVE 初検日Ｗ(部位ＣＮＴ)  TO 開始日Ｗ(部位ＣＮＴ) 施記－施術日
029350         ELSE
029360             MOVE 患者番号ＷＲ          TO 施記－患者番号
029370             MOVE 枝番ＷＲ              TO 施記－枝番
029380             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
029390             MOVE 施術年ＷＲ            TO 施記－施術年
029400             MOVE 施術月ＷＲ            TO 施記－施術月
029410             MOVE ZERO                  TO 施記－施術日
029420         END-IF
029430         START 施術記録Ｆ   KEY IS >= 施記－患者コード
029440                                      施記－施術和暦年月日
029450         END-START
029460         IF ( 状態キー = "00" )
029480             MOVE ZERO  TO 終了年ＷＴ
029490             MOVE ZERO  TO 終了月ＷＴ
029500             MOVE ZERO  TO 終了日ＷＴ
029510             MOVE SPACE TO 終了フラグ２
029520             PERFORM 施術記録Ｆ読込
029530             IF  ( 終了フラグ２      = SPACE   ) AND
029540                 ( 施記－患者コード  = 患者コードＷＲ ) AND
029550                 ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
029560                 ( 施記－施術年      = 施術年ＷＲ     ) AND
029570                 ( 施記－施術月      = 施術月ＷＲ     ) 
029580*
029590*        *****************************************************************
029600*        * 開始年月日 ( その部位が当月初検でないか、
029610*                       当月初検でも枝番がある時は、最初の施術日を開始日)*
029620*        *****************************************************************
029630                 IF ( 施術年Ｗ NOT = 初検年Ｗ(部位ＣＮＴ) ) OR
029640                    ( 施術月Ｗ NOT = 初検月Ｗ(部位ＣＮＴ) ) OR
029650                    ( 開始診療日手動区分Ｗ = 1 )
029660                     MOVE 施記－施術年   TO 開始年Ｗ(部位ＣＮＴ)
029670                     MOVE 施記－施術月   TO 開始月Ｗ(部位ＣＮＴ)
029680                     MOVE 施記－施術日   TO 開始日Ｗ(部位ＣＮＴ)
029690                 END-IF
029700             END-IF
029710             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
029720                           ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
029730                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
029740                           ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
029750                           ( 施記－施術月     NOT = 施術月ＷＲ       ) OR
029760                           ( 施記－施術日         > 終了日Ｗ(部位ＣＮＴ))
029770*               **********
029780*               * 実日数 *
029790*               **********
029810                MOVE 施記－施術年               TO 終了年ＷＴ
029820                MOVE 施記－施術月               TO 終了月ＷＴ
029830                MOVE 施記－施術日               TO 終了日ＷＴ
029840*
029850                PERFORM 施術記録Ｆ読込
029860            END-PERFORM
029870        END-IF
029880*       **************************
029890*       * 継続：終了年月日セット *
029900*       **************************
029910        IF ( 転帰区分Ｗ(部位ＣＮＴ) = 9 )
029920            MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
029930            MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
029940            MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
029950        END-IF
029960        IF ( 終了年月日Ｗ(部位ＣＮＴ) > 受理年月日Ｗ )
029970            MOVE 終了年Ｗ(部位ＣＮＴ) TO 受理年Ｗ
029980            MOVE 終了月Ｗ(部位ＣＮＴ) TO 受理月Ｗ
029990            MOVE 終了日Ｗ(部位ＣＮＴ) TO 受理日Ｗ
030000        END-IF
030010     END-PERFORM.
030020*
030030** ----- 前月初検のみかを判定 -----------*
030040*
030050*     MOVE 患者番号ＷＲ          TO 施記－患者番号.
030060*     MOVE 枝番ＷＲ              TO 施記－枝番.
030070*     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
030080*     MOVE 施術年ＷＲ            TO 施記－施術年.
030090*     MOVE 施術月ＷＲ            TO 施記－施術月.
030100*     MOVE ZERO                  TO 施記－施術日.
030110*     START 施術記録Ｆ   KEY IS >= 施記－患者コード
030120*                                  施記－施術和暦年月日
030130*     END-START.
030140*     IF ( 状態キー = "00" )
030150*             MOVE SPACE TO 終了フラグ２
030160*             PERFORM 施術記録Ｆ読込
030170*             IF  ( 終了フラグ２      = SPACE   ) AND
030180*                 ( 施記－患者コード  = 患者コードＷＲ ) AND
030190*                 ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
030200*                 ( 施記－施術年      = 施術年ＷＲ     ) AND
030210*                 ( 施記－施術月      = 施術月ＷＲ     ) 
030220** 当月施術開始日が再検かどうか判定
030230*                 IF   ( 施記－再検料請求 = 1 )
030240*                      MOVE "YES"  TO  初日再検フラグ
030250*                 END-IF
030260**
030270*             END-IF
030280*     END-IF.
030290*     IF ( 初日再検フラグ = "YES" )
030300*        PERFORM 前月初検のみ判定
030310*     END-IF.
030320*
030330*================================================================*
030340 前月初検のみ判定 SECTION.
030350*
030360*** 前月の通院日が初検か判定 
030370     MOVE  SPACE            TO 前月フラグ.
030380     MOVE 受－患者コード    TO 施記－患者コード.
030390     MOVE 受－施術和暦      TO 施記－施術和暦.
030400     MOVE 受－施術年        TO 施記－施術年.
030410     MOVE 受－施術月        TO 施記－施術月.
030420     MOVE 1                 TO 施記－施術日.
030430     START 施術記録Ｆ   KEY IS <  施記－患者コード
030440                                  施記－施術和暦年月日
030450                                  REVERSED
030460     END-START.
030470     IF ( 状態キー = "00" )
030480         MOVE SPACE  TO 終了フラグ２
030490         PERFORM 施術記録Ｆ読込
030500         IF ( 終了フラグ２      = SPACE  ) AND
030510            ( 施記－患者コード  = 受－患者コード ) AND
030520            ( 施記－診療区分    = 2 ) 
030530*
030540            PERFORM 前月判定
030550**** 適用１を使用
030560            IF ( 前月フラグ = "YES" )
030570               MOVE NC"※前月初検のみ"    TO  適用１Ｗ
030580            END-IF
030590**
030600         END-IF
030610     END-IF.
030620*
030630*================================================================*
030640 前月判定  SECTION.
030650* 
030660*** 読み込んだ施術記録の年月が、前月かどうか判定 (年月の差が 1 か?)
030670      MOVE  SPACE  TO  前月フラグ.
030680      INITIALIZE  計算年月日Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
030690**
030700      MOVE 受－施術和暦    TO 終了和暦２Ｗ.
030710      MOVE 受－施術年      TO 終了年２Ｗ.
030720      MOVE 受－施術月      TO 終了月２Ｗ.
030730      MOVE 施記－施術和暦  TO 開始和暦２Ｗ.
030740      MOVE 施記－施術年    TO 開始年２Ｗ.
030750      MOVE 施記－施術月    TO 開始月２Ｗ.
030760*
030770      EVALUATE TRUE
030780       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ = 終了年２Ｗ)
030790            PERFORM  前月比較月
030800       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ NOT = 終了年２Ｗ)
030810            PERFORM  前月比較年
030820       WHEN  開始和暦２Ｗ NOT = 終了和暦２Ｗ 
030830            PERFORM  前月比較元号
030840      END-EVALUATE.
030850*
030860      IF ( 計算月Ｗ = 1 )
030870         MOVE  "YES"  TO  前月フラグ
030880      END-IF.
030890*
030900*================================================================*
030910 前月比較元号  SECTION.
030920*
030930     MOVE 開始和暦２Ｗ TO 元－元号区分.
030940     READ 元号マスタ
030950     NOT INVALID KEY
030960         MOVE 元－開始西暦年 TO 開始西暦年Ｗ
030970     END-READ.
030980     MOVE 終了和暦２Ｗ TO 元－元号区分.
030990     READ 元号マスタ
031000     NOT INVALID KEY
031010         MOVE 元－開始西暦年 TO 終了西暦年Ｗ
031020     END-READ.
031030**
031040     IF ( 開始西暦年Ｗ NOT = ZERO ) AND ( 終了西暦年Ｗ NOT = ZERO )
031050        COMPUTE 開始西暦年Ｗ = 開始西暦年Ｗ + 開始年２Ｗ - 1
031060        COMPUTE 終了西暦年Ｗ = 終了西暦年Ｗ + 終了年２Ｗ - 1
031070*
031080        IF ( 終了西暦年Ｗ =  開始西暦年Ｗ )
031090           PERFORM  前月比較月
031100        ELSE
031110           IF  ( 終了西暦年Ｗ >  開始西暦年Ｗ )
031120               COMPUTE 計算年Ｗ = 終了西暦年Ｗ - 開始西暦年Ｗ
031130               COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
031140           ELSE
031150               MOVE ZERO TO 計算月Ｗ
031160           END-IF
031170        END-IF
031180     ELSE
031190        MOVE ZERO TO 計算月Ｗ
031200     END-IF.
031210*
031220*================================================================*
031230 前月比較年  SECTION.
031240*
031250     IF  ( 終了年２Ｗ >  開始年２Ｗ )
031260         COMPUTE 計算年Ｗ = 終了年２Ｗ - 開始年２Ｗ
031270         COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
031280     ELSE
031290        MOVE ZERO TO 計算月Ｗ
031300     END-IF.
031310*
031320*================================================================*
031330 前月比較月  SECTION.
031340*
031350     IF  ( 終了月２Ｗ >  開始月２Ｗ )
031360         COMPUTE 計算月Ｗ = 終了月２Ｗ - 開始月２Ｗ
031370     ELSE
031380        MOVE ZERO TO 計算月Ｗ
031390     END-IF.
031400*
031410*================================================================*
031420 長期判定取得 SECTION.
031430*================================================================*
031440* ３カ月以上の長期判定は "CHOUKI" を呼ぶ. 
031450     MOVE  SPACE TO  連期間－キー.
031460     INITIALIZE      連期間－キー.
031470     MOVE 施術和暦ＷＲ  TO  連期間－施術和暦.
031480     MOVE 施術年ＷＲ    TO  連期間－施術年.
031490     MOVE 施術月ＷＲ    TO  連期間－施術月.
031500     MOVE 患者番号ＷＲ  TO  連期間－患者番号.
031510     MOVE 枝番ＷＲ      TO  連期間－枝番.
031520*
031530     CALL   "CHOUKI".
031540     CANCEL "CHOUKI".
031600*
032320*================================================================*
032330 初検加算時刻取得 SECTION.
032340*================================================================*
032350*****************************************************************
032360** 初検加算が時間外と深夜の時、適用に「受付時間」を印字する。
032370**   時刻の印字は月3回まで可能
032380*****************************************************************
032390     IF ( レセ－時間外 = 1 ) OR ( レセ－深夜 = 1 ) OR ( レセ－休日 = 1 )
032400*
032410         MOVE 患者番号ＷＲ          TO 施記－患者番号
032420         MOVE 枝番ＷＲ              TO 施記－枝番
032430         MOVE 施術和暦ＷＲ          TO 施記－施術和暦
032440         MOVE 施術年ＷＲ            TO 施記－施術年
032450         MOVE 施術月ＷＲ            TO 施記－施術月
032460         MOVE ZERO                  TO 施記－施術日
032470         START 施術記録Ｆ   KEY IS >= 施記－患者コード
032480                                      施記－施術和暦年月日
032490         END-START
032500         IF ( 状態キー = "00" )
032510             MOVE ZERO  TO 初検加算カウント
032520             MOVE SPACE TO 終了フラグ２
032530             PERFORM 施術記録Ｆ読込
032540             PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
032550                           ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
032560                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
032570                           ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
032580                           ( 施記－施術月     NOT = 施術月ＷＲ      ) 
032590                   IF  ( 施記－初検加算 = 1 OR 2 OR 3 ) AND ( 施記－診療区分 = 2 )
032600                       COMPUTE 初検加算カウント = 初検加算カウント  + 1
032610                       IF  初検加算カウント <= 3
032620                           MOVE 施記－初検加算 TO 初検加算区分ＷＴ(初検加算カウント)
032630                           MOVE 施記－受付時   TO 初検加算時ＷＴ(初検加算カウント)
032640                           MOVE 施記－受付分   TO 初検加算分ＷＴ(初検加算カウント)
032650                       END-IF
032660                   END-IF
032670                   PERFORM 施術記録Ｆ読込
032680             END-PERFORM
032690** 初検加算の時刻を適用にセット
033380            IF ( 初検加算時ＷＴ(1) NOT = ZERO ) OR ( 初検加算分ＷＴ(1) NOT = ZERO ) 
                     MOVE 初検加算時ＷＴ(1) TO 初検加算時Ｗ
                     MOVE ":"               TO 初検加算区切Ｗ
                     MOVE 初検加算分ＷＴ(1) TO 初検加算分Ｗ
                  END-IF
033380            IF ( 初検加算時ＷＴ(2) NOT = ZERO ) OR ( 初検加算分ＷＴ(2) NOT = ZERO ) 
031910               PERFORM 初検加算適用セット
                  END-IF
032710         END-IF
032720*
032730     END-IF.
032740*
032750*================================================================*
032760 初検加算適用セット SECTION.
032770*
032780     PERFORM VARYING 番号カウンタ FROM 1 BY 1
032790              UNTIL  番号カウンタ > 3
032800         IF ( 初検加算時ＷＴ(番号カウンタ)  = ZERO )  AND 
032810            ( 初検加算分ＷＴ(番号カウンタ)  = ZERO ) 
032820             CONTINUE
032830         ELSE
032840* 固定項目
032850             EVALUATE 初検加算区分ＷＴ(番号カウンタ) 
032860             WHEN 1
032870                MOVE NC"時間外"   TO 加算内容Ｗ(番号カウンタ)
033320             WHEN 2
033330                MOVE NC"休　日"   TO 加算内容Ｗ(番号カウンタ)
032880             WHEN 3
032890                MOVE NC"深　夜"   TO 加算内容Ｗ(番号カウンタ)
032900             END-EVALUATE
032910*
032920             MOVE NC"："          TO 加算区切Ｗ(番号カウンタ)
032930             MOVE NC"時"          TO 時固定Ｗ(番号カウンタ)
032940             MOVE NC"分"          TO 分固定Ｗ(番号カウンタ)
032950*
032960**** 数字→日本語変換
032970* 時間
032980             MOVE 初検加算時ＷＴ(番号カウンタ)  TO  数字Ｗ
032990             IF ( 数字Ｗ >= 10 )
033000                 MOVE 数字Ｗ１    TO 負傷番号Ｗ１
033010                 PERFORM 日本語変換
033020                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ１(番号カウンタ)
033030                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
033040                 PERFORM 日本語変換
033050                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
033060             ELSE
033070                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
033080                 PERFORM 日本語変換
033090                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
033100             END-IF
033110* 分
033120             MOVE 初検加算分ＷＴ(番号カウンタ)  TO  数字Ｗ
033130             MOVE 数字Ｗ１    TO 負傷番号Ｗ１
033140             PERFORM 日本語変換
033150             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ１(番号カウンタ)
033160             MOVE 数字Ｗ２    TO 負傷番号Ｗ１
033170             PERFORM 日本語変換
033180             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ２(番号カウンタ)
033190** 
033200        END-IF
033210     END-PERFORM.
033220*
033230     MOVE  初検加算集団ＮＷ(1)   TO 初検加算時刻１Ｗ. 
033240     MOVE  初検加算集団ＮＷ(2)   TO 初検加算時刻２Ｗ. 
033250     MOVE  初検加算集団ＮＷ(3)   TO 初検加算時刻３Ｗ. 
033260*
033270**** 適用１か２を使用（長期理由記載で適用１を使っている時は、適用２）
033280     IF ( 初検加算時ＷＴ(2)  = ZERO ) AND ( 初検加算分ＷＴ(2)  = ZERO ) 
033290         CONTINUE
033300     ELSE
033310         IF ( 適用１Ｗ  = SPACE )
033320               STRING NC"初検加算"       DELIMITED BY SIZE
033330                      初検加算時刻１Ｗ   DELIMITED BY SIZE
033340                      初検加算時刻２Ｗ   DELIMITED BY SIZE
033350                      初検加算時刻３Ｗ   DELIMITED BY SIZE
033360                      INTO 適用１Ｗ
033370               END-STRING
033380         ELSE
033390               STRING NC"初検加算"       DELIMITED BY SIZE
033400                      初検加算時刻１Ｗ   DELIMITED BY SIZE
033410                      初検加算時刻２Ｗ   DELIMITED BY SIZE
033420                      初検加算時刻３Ｗ   DELIMITED BY SIZE
033430                      INTO 適用２Ｗ
033440               END-STRING
033450         END-IF
033460     END-IF.
033470*
033480*================================================================*
033490 日本語変換 SECTION.
033500*
033510     MOVE NC"０"     TO 全角負傷番号Ｗ.
033520     CALL "htoz" WITH C LINKAGE
033530                        USING 負傷番号Ｗ１ 全角負傷番号Ｗ１.
033540*
033550*================================================================*
033560 委任年月日取得 SECTION.
033570*================================================================*
033580** ---// ここの受理年には、最終通院日が入っている為、退避する //----
033590     MOVE 受理年Ｗ   TO 最終通院年Ｗ.
033600     MOVE 受理月Ｗ   TO 最終通院月Ｗ.
033610     MOVE 受理日Ｗ   TO 最終通院日Ｗ.
033620***
033630* (柔整師側)
033640     EVALUATE レセプト日付区分Ｗ 
033650*    /  最終通院日 /
033660     WHEN ZERO
033670         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
033680         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
033690         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
033700*    /  月末日 /
033710     WHEN 1 
033720         PERFORM 月末日取得
033730         MOVE 受理年Ｗ     TO 柔整師年Ｗ
033740         MOVE 受理月Ｗ     TO 柔整師月Ｗ
033750         MOVE 受理日Ｗ     TO 柔整師日Ｗ
033760*    /  印字なし /
033770     WHEN 9
033780         MOVE ZERO         TO 柔整師年Ｗ
033790         MOVE ZERO         TO 柔整師月Ｗ
033800         MOVE ZERO         TO 柔整師日Ｗ
033810*    /  その他は、最終通院日 /
033820     WHEN OTHER
033830         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
033840         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
033850         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
033860     END-EVALUATE.
033870**
033880* (患者側)
033890     EVALUATE レセプト患者日付区分Ｗ 
033900*    /  最終通院日 /
033910     WHEN ZERO
033920         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
033930         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
033940         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
033950*    /  月末日 /
033960     WHEN 1 
033970         PERFORM 月末日取得
033980         MOVE 受理年Ｗ     TO 患者委任年Ｗ
033990         MOVE 受理月Ｗ     TO 患者委任月Ｗ
034000         MOVE 受理日Ｗ     TO 患者委任日Ｗ
034010*    /  印字なし /
034020     WHEN 9
034030         MOVE ZERO         TO 患者委任年Ｗ
034040         MOVE ZERO         TO 患者委任月Ｗ
034050         MOVE ZERO         TO 患者委任日Ｗ
034060*    /  その他は、最終通院日 /
034070     WHEN OTHER
034080         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
034090         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
034100         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
034110     END-EVALUATE.
034120*
034130*================================================================*
034140 月末日取得 SECTION.
034150*
034160     MOVE 施術年ＷＲ   TO 受理年Ｗ.
034170     MOVE 施術月ＷＲ   TO 受理月Ｗ.
034180     MOVE 施術和暦ＷＲ TO 元－元号区分.
034190     READ 元号マスタ
034200     NOT INVALID KEY
034210         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
034220     END-READ.
034230     IF ( 施術西暦年Ｗ NOT = ZERO )
034240        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
034250     END-IF.
034260*
034270     EVALUATE 施術月ＷＲ
034280     WHEN 4
034290     WHEN 6
034300     WHEN 9
034310     WHEN 11
034320         MOVE 30 TO 受理日Ｗ
034330     WHEN 2
034340         DIVIDE 4 INTO 施術西暦年Ｗ GIVING    商Ｗ
034350                                    REMAINDER 余Ｗ
034360         END-DIVIDE
034370         IF ( 余Ｗ = ZERO )
034380             MOVE 29 TO 受理日Ｗ
034390         ELSE
034400             MOVE 28 TO 受理日Ｗ
034410         END-IF
034420     WHEN 1
034430     WHEN 3
034440     WHEN 5
034450     WHEN 7
034460     WHEN 8
034470     WHEN 10
034480     WHEN 12
034490         MOVE 31 TO 受理日Ｗ
034500     WHEN OTHER
034510          CONTINUE
034520     END-EVALUATE.
034530*
034540*================================================================*
034550 負傷原因取得 SECTION.
034560*================================================================*
034570********************************************************************
034580*  負傷原因コードが同じものは、1行にまとめて印字する。
034590*  例: ①② 家で転んだ.
034600*     負傷原因コードが同じものをまとめ、テーブルにセット
034610*     (ただし、部位を飛んで同じものは、2行になる)
034620********************************************************************
034630     MOVE  ZERO   TO  カウンタ カウンタ２.
034640     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
034650             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
034660*
034670****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
034680        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
034690*
034700           IF ( カウンタ = ZERO )
034710               MOVE 1   TO  カウンタ カウンタ２
034720               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
034730               MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
034740               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
034750           ELSE
034760              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
034770                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
034780                 COMPUTE カウンタ２ = カウンタ２  +  1
034790                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
034800              ELSE
034810                 COMPUTE カウンタ = カウンタ  +  1
034820                 MOVE 1   TO  カウンタ２
034830                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
034840                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
034850                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
034860              END-IF
034870           END-IF
034880        END-IF
034890     END-PERFORM.
034900**************************************************************************
034910*  負傷原因マスタより文章取得
034920**************************************************************************
034930     MOVE  ZERO   TO  カウンタ カウンタ２.
034940     PERFORM VARYING カウンタ FROM 1 BY 1
034950             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
034960** 健保は 区分 01
034970         MOVE 01                        TO 負原－区分コード
034980         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
034990         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
035000         READ 負傷原因Ｆ
035010         NOT INVALID KEY
035020             INITIALIZE 負傷原因ＷＴ
035030             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
035040             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
035050             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
035060             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
035070             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
035080             PERFORM VARYING カウンタ２ FROM 1 BY 1
035090                     UNTIL ( カウンタ２ > 9 )  OR 
035100                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
035110                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
035120                WHEN 1
035130                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
035140                WHEN 2
035150                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
035160                WHEN 3
035170                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
035180                WHEN 4
035190                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
035200                WHEN 5
035210                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
035180                WHEN 6
035190                   MOVE "⑥"  TO  負傷原因ナンバーＷ１(カウンタ２)
035200                WHEN 7
035210                   MOVE "⑦"  TO  負傷原因ナンバーＷ１(カウンタ２)
035220                WHEN OTHER
035230                   CONTINUE
035240                END-EVALUATE
035250             END-PERFORM
035260*
035342             IF 負原－負傷原因入力区分 = 1
035343                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
035344                        負傷原因１ＷＴ  DELIMITED BY SIZE
035345                        負傷原因２ＷＴ  DELIMITED BY SIZE
035346                        負傷原因３ＷＴ  DELIMITED BY SIZE
035347                        負傷原因４ＷＴ  DELIMITED BY SIZE
035348                        負傷原因５ＷＴ  DELIMITED BY SIZE
035349                        INTO 負傷原因内容合成Ｗ(カウンタ)
035350                 END-STRING
035351             ELSE
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
035360             END-IF
035361*
035362         END-READ
035363     END-PERFORM.
035370*
035380     PERFORM 負傷原因セット.
035390*
035400*================================================================*
035410 負傷原因セット SECTION.
035420*
035430**************************************************************************
035440*  文章が1行を超える時は、複数行に分解する。
035450**************************************************************************
035460     MOVE  ZERO   TO  カウンタ カウンタ２.
035470     PERFORM VARYING カウンタ FROM 1 BY 1
035480             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
035490*
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
035640*
035650     END-PERFORM.
035660*
035670*================================================================*
035680 長期理由文取得 SECTION.
035690*================================================================*
035700* 長期理由文取得は "CHOUBUN" を呼ぶ. 
035710     MOVE  SPACE TO  連長文－キー.
035720     INITIALIZE      連長文－キー.
035730     MOVE 施術和暦ＷＲ  TO  連長文－施術和暦.
035740     MOVE 施術年ＷＲ    TO  連長文－施術年.
035750     MOVE 施術月ＷＲ    TO  連長文－施術月.
035760     MOVE 患者番号ＷＲ  TO  連長文－患者番号.
035770     MOVE 枝番ＷＲ      TO  連長文－枝番.
035780** 中部柔整師協会用は56桁
035790     MOVE 56            TO  連長文－文桁数.
035800*
035810     CALL   "CHOUBUN".
035820     CANCEL "CHOUBUN".
035830*
035840*================================================================*
035850 施術ＩＤ取得 SECTION.
035860*================================================================*
035870*********************************************
035880** ＩＤ管理マスタより　県施術ＩＤを取得する。
035890*********************************************
035900**   / 県施術ID /
035910     MOVE 01                     TO ＩＤ管－ＩＤ区分.
035920     MOVE ZERO                   TO ＩＤ管－施術所番号.
035930     MOVE 費用負担者番号助成ＷＲ(3:2) TO ＩＤ管－保険種別.
035940     MOVE SPACE                  TO ＩＤ管－保険者番号.
035950     READ ＩＤ管理マスタ
035960     NOT INVALID KEY
035970         MOVE ＩＤ管－施術ＩＤ番号   TO 県施術ＩＤＷ
035980     END-READ.
035990*
036000**   / 市町村施術ID /
036010*****     MOVE 02                     TO ＩＤ管－ＩＤ区分.
036020*****     MOVE ZERO                   TO ＩＤ管－施術所番号.
036030*****     MOVE 助成種別ＷＲ           TO ＩＤ管－保険種別.
036040*****     MOVE 費用負担者番号助成ＷＲ TO ＩＤ管－保険者番号.
036050*****     READ ＩＤ管理マスタ
036060*****     NOT INVALID KEY
036070*****          MOVE ＩＤ管－施術ＩＤ番号   TO 市町村施術ＩＤＷ
036080*****     END-READ.
036090*
036100*================================================================*
036110 レセプト回数取得 SECTION.
036120*================================================================*
036130*************************************************************************
036140**-------- レセプトの第 XX 回目 の回数を求める。----------**
036150*  部位の開始年月で、一番小さい(古い)年月と施術年月との差に1を足す
036160*  (例) 開始年月10年7月  で施術年月10年10月は、4回目
036170*  (例) 開始年月10年10月 で施術年月10年10月は、1回目
036180*************************************************************************
036190*
036200     MOVE ZERO     TO 回数Ｗ.
036210*
036220     PERFORM 開始年月最小取得.
036230     PERFORM 差の月取得.
036240     MOVE 計算月Ｗ TO 回数Ｗ.
036250*
036260*================================================================*
036270 開始年月最小取得  SECTION.
036280*
036290** --// 部位の開始年月で、一番小さい(古い)年月を求める. //--**
036300*
036310     INITIALIZE 最小開始和暦年月Ｗ.
036320* 1部位目と2部位目を比較
036330     IF ( 負－開始和暦年月(2) NOT = ZERO )
036340        IF ( 負－開始和暦年月(1)  <  負－開始和暦年月(2) )
036350           MOVE 負－開始和暦年月(1) TO 最小開始和暦年月Ｗ
036360        ELSE
036370           MOVE 負－開始和暦年月(2) TO 最小開始和暦年月Ｗ
036380        END-IF
036390     ELSE
036400        MOVE 負－開始和暦年月(1) TO 最小開始和暦年月Ｗ
036410     END-IF.
036420* 3部位目以降を比較
036430     PERFORM VARYING 部位ＣＮＴ FROM 3 BY 1
036440             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
036450         IF ( 負－開始和暦年月(部位ＣＮＴ) <  最小開始和暦年月Ｗ )
036460            MOVE 負－開始和暦年月(部位ＣＮＴ) TO 最小開始和暦年月Ｗ
036470         END-IF
036480     END-PERFORM.
036490*
036500*================================================================*
036510 差の月取得  SECTION.
036520*********************************************************** 
036530*   開始年月と施術年月との差の月を求める。
036540*    (前月判定のロジック、セクションを利用)
036550*********************************************************** 
036560*
036570      INITIALIZE  計算年月日Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
036580*
036590      IF ( 最小開始和暦年月Ｗ NOT = ZERO )
036600*
036610          MOVE 施術和暦ＷＲ    TO 終了和暦２Ｗ
036620          MOVE 施術年ＷＲ      TO 終了年２Ｗ
036630          MOVE 施術月ＷＲ      TO 終了月２Ｗ
036640          MOVE 最小開始和暦Ｗ  TO 開始和暦２Ｗ
036650          MOVE 最小開始年Ｗ    TO 開始年２Ｗ
036660          MOVE 最小開始月Ｗ    TO 開始月２Ｗ
036670*
036680          EVALUATE TRUE
036690           WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ = 終了年２Ｗ)
036700                PERFORM  前月比較月
036710           WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ NOT = 終了年２Ｗ)
036720                PERFORM  前月比較年
036730           WHEN  開始和暦２Ｗ NOT = 終了和暦２Ｗ 
036740                PERFORM  前月比較元号
036750          END-EVALUATE
036760*
036770          COMPUTE 計算月Ｗ =  計算月Ｗ + 1
036780*
036790      END-IF.
036800*
036810*================================================================*
036820 保険者名称取得 SECTION.
036830*================================================================*
036840     MOVE 保険種別ＷＲ   TO 保－保険種別.
036850     MOVE 保険者番号ＷＲ TO 保－保険者番号.
036860     READ 保険者マスタ
036870     INVALID KEY
               IF 保険種別ＷＲ = 05
030800             MOVE 保険種別ＷＲ   TO 市－公費種別
030810             MOVE 保険者番号ＷＲ TO 市－市町村番号
030820             READ 市町村マスタ
030830             INVALID KEY
030840                 MOVE SPACE      TO 保険者名称Ｗ
030850             NOT INVALID KEY
031330                 MOVE 市－市町村名称    TO 保険者名称Ｗ
                   END-READ
               ELSE
030840             MOVE SPACE      TO 保険者名称Ｗ
               END-IF
036890     NOT INVALID KEY
036900** 組合・共済は支部名まで印字
036910                 EVALUATE 保険種別ＷＲ
036920                 WHEN 2
036930                 WHEN 6
036940                     IF ( 保－接尾語区分 = 1 )
036950                        MOVE 保－保険者名称    TO 保険者名称Ｗ
036960                     ELSE
036970                        STRING 保－保険者名称    DELIMITED BY SPACE
036980                               "社会保険事務所"  DELIMITED BY SIZE
036990                               INTO 保険者名称Ｗ
037000                        END-STRING
037010                     END-IF
037020                 WHEN 3
037030                     STRING 保－保険者名称    DELIMITED BY SPACE
037040                            "健康保険組合"    DELIMITED BY SIZE
037050                            保－支部部署名    DELIMITED BY SPACE
037060                            INTO 保険者名称Ｗ
037070                     END-STRING
037080                 WHEN 4
037090                     STRING 保－保険者名称    DELIMITED BY SPACE
037100                            "共済組合"        DELIMITED BY SIZE
037110                            保－支部部署名    DELIMITED BY SPACE
037120                            INTO 保険者名称Ｗ
037130                     END-STRING
037140                 WHEN OTHER
037150                     MOVE 保－保険者名称      TO 保険者名称Ｗ
037160                 END-EVALUATE
037170     END-READ.
037180*
037190*================================================================*
037200 給付割合取得 SECTION.
037210*================================================================*
037220     MOVE ZERO  TO 負担割合Ｗ   給付割合Ｗ.
037230*
037240     IF ( 公費種別ＷＲ = 05 )
037250        IF ( 施術和暦年月ＷＲ >= 41410 )
037260           PERFORM 負担率取得１４１０
037270           COMPUTE 負担割合Ｗ = ( 負担率Ｗ / 10 )
037280           COMPUTE 給付割合Ｗ = ( 10 - 負担割合Ｗ )
037290        ELSE
037300           CONTINUE
037310        END-IF
037320     ELSE
037330        MOVE レセ－負担割合 TO 負担割合Ｗ
037340        MOVE レセ－給付割合 TO 給付割合Ｗ
037350     END-IF.
037360*
037370*     PERFORM 給付割合チェック.
037380*
037390*================================================================*
037400 負担率取得１４１０ SECTION.
037410*
037420* 平成14/10～
037430     MOVE ZERO  TO 負担率Ｗ.
037440     MOVE SPACE TO 連率－負担率取得キー.
037450     INITIALIZE 連率－負担率取得キー.
037460     MOVE 施術和暦年月ＷＲ TO 連率－施術和暦年月.
037470     MOVE 患者コードＷＲ   TO 連率－患者コード.
037480*
037490     CALL   "HUTANRIT".
037500     CANCEL "HUTANRIT".
037510*
037520***     MOVE 連率－実際負担率 TO 負担率Ｗ.
037530*
037540*** / 老人レセの時は以下
037550     MOVE 連率－２７老負担率 TO 負担率Ｗ.
037560*
037570**================================================================*
037580* 給付割合チェック SECTION.
037590**
037600**** ２７身障、被爆（３ペア）の時は、給付老人チェックに○
037610*     IF ( 公費種別ＷＲ NOT = ZERO )  AND
037620*        ( 助成種別ＷＲ NOT = ZERO )
037630*        MOVE NC"老"   TO  給付老人Ｗ 
037640*        MOVE NC"○"   TO  給付老人チェックＷ 
037650*     ELSE
037660**
037670*        EVALUATE  給付割合Ｗ
037680*        WHEN  7
037690*           MOVE NC"○"   TO  給付７割チェックＷ 
037700*        WHEN  8
037710*           MOVE NC"○"   TO  給付８割チェックＷ 
037720*        WHEN  9
037730*           MOVE NC"○"   TO  給付９割チェックＷ 
037740*        WHEN  OTHER
037750*           CONTINUE
037760*        END-EVALUATE
037770*     END-IF.
037780*
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
037790*================================================================*
037800 レセ摘要再セット SECTION.
037810*================================================================*
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
037990*
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
038000*================================================================*
038010 施術記録Ｆ読込 SECTION.
038020*================================================================*
038030*
038040     READ 施術記録Ｆ NEXT
038050     AT END
038060         MOVE "YES" TO 終了フラグ２
038070     END-READ.
038080*
038090*----------------------------------------------------------------*
038100*================================================================*
038110 印刷処理 SECTION.
038120*================================================================*
038130     MOVE "YCB6425P" TO  定義体名Ｐ.
038140     MOVE "SCREEN"  TO  項目群名Ｐ.
038150     WRITE YCB6425P.
038160***     WRITE 印刷レコード.
038170     PERFORM エラー処理Ｐ.
038180*================================================================*
038190 エラー処理Ｐ SECTION.
038200*
038210     IF 通知情報Ｐ NOT = "00"
038220         DISPLAY NC"帳票エラー"              UPON CONS
038230         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
038240         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
038250         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
038260         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
038270                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
038280         ACCEPT  キー入力 FROM CONS
038290         PERFORM ファイル閉鎖
038300         MOVE 99  TO PROGRAM-STATUS
038310         EXIT PROGRAM
038320     END-IF.
038330*
038340*=== 終了処理 ===================================================*
038350*================================================================*
038360 受診者印刷区分更新 SECTION.
038370*================================================================*
038380** //  受診者情報Ｆの印刷区分に１をセットし、更新する。//  
038390*
038400     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
038410     MOVE 施術年ＷＲ         TO 受－施術年.
038420     MOVE 施術月ＷＲ         TO 受－施術月.
038430     MOVE 患者コードＷＲ     TO 受－患者コード.
038440     READ 受診者情報Ｆ
038450     NOT INVALID KEY
038460         MOVE  1  TO  受－レセ印刷区分助成
038470         REWRITE  受－レコード
038480         END-REWRITE
038490         IF ( 状態キー NOT = "00" )
038500            MOVE NC"受診者" TO ファイル名
038510            PERFORM エラー表示
038520         END-IF
038530     END-READ.
038540*
038550*================================================================*
038560 終了処理 SECTION.
038570*================================================================*
038580     PERFORM ファイル閉鎖.
038590*
038600*================================================================*
038610 ファイル閉鎖 SECTION.
038620*
038630     CLOSE 元号マスタ     名称マスタ       レセプトＦ     経過マスタ
038640           制御情報マスタ 施術所情報マスタ 受診者情報２Ｆ
038650           保険者マスタ   請求先マスタ     ＩＤ管理マスタ 市町村マスタ
038660           受診者情報Ｆ   施術記録Ｆ       負傷データＦ   負傷原因Ｆ
038670           作業ファイル２.
038680     CLOSE 印刷ファイル.
038690*
038700*================================================================*
038710*================================================================*
038720 エラー表示 SECTION.
038730*
038740     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
038750     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
038760     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
038770     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
038780                                                   UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
038790     ACCEPT  キー入力 FROM CONS
038800     PERFORM ファイル閉鎖.
038810     EXIT PROGRAM.
038820*
038830*================================================================*
038840*================================================================*
038850 テスト印字処理 SECTION.
      *
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
           小計４０ 長期逓減率４０ 長期込小計４０ 合計 負担割合  
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
      *
           MOVE ALL "静" TO
           被保険者氏名 患者氏名 接骨院名 代表者名
           長期理由文１ 長期理由文２ 長期理由文３ 長期理由文４ 長期理由文５ 長期理由文６ 
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
040030*================================================================*
030430 助成印取得 SECTION.
030440*
030442* 助成印は "JOSEIMEI" を呼ぶ. 
030443     MOVE SPACE TO  連助成名称－キー.
030444     INITIALIZE     連助成名称－キー.
030445     MOVE 助成種別ＷＲ           TO 連助成名称－助成種別.
030446     MOVE 費用負担者番号助成ＷＲ TO 連助成名称－費用負担者番号助成.
030447*
030448     CALL   "JOSEIMEI".
030449     CANCEL "JOSEIMEI".
030450*
030451     MOVE 連助成名称－１文字 TO 助成印Ｗ.
030452*
030420*================================================================*
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
043540*================================================================*
043420 レセプト並び順取得 SECTION.
043430*
043440     MOVE 施術和暦ＷＲ       TO 作２－施術和暦.
043450     MOVE 施術年ＷＲ         TO 作２－施術年.
043460     MOVE 施術月ＷＲ         TO 作２－施術月.
043470     MOVE 患者コードＷＲ     TO 作２－患者コード.
039550** 助成は、助成種別をセット
039560     MOVE 助成種別ＷＲ       TO 作２－保険種別.
043490     READ 作業ファイル２
043500     NOT INVALID KEY
043510          MOVE 作２－順番    TO 順番Ｗ
043520     END-READ.
043530*
043540*================================================================*
040040******************************************************************
040050 END PROGRAM YCB6425.
040060******************************************************************
