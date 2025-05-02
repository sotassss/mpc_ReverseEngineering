000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHP6425.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*    ホープ  助成 レセプト印刷（柔+ｳｨﾝﾄﾞｳｽﾞ版）
000100*         MED = YAW610 YHP6425P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2012-08-07
000130 DATE-COMPILED.          2012-08-07
      */金属副子・運動後療の変更・追加/1805
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
000360     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
000370                             ORGANIZATION             IS  INDEXED
000380                             ACCESS MODE              IS  DYNAMIC
000390                             RECORD KEY               IS  市－公費種別
000400                                                          市－市町村番号
000410                             ALTERNATE RECORD KEY     IS  市－公費種別
000420                                                          市－市町村名称
000430                                                          市－市町村番号
000440                             FILE STATUS              IS  状態キー
000450                             LOCK        MODE         IS  AUTOMATIC.
000460     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000470                             ORGANIZATION             IS  INDEXED
000480                             ACCESS MODE              IS  DYNAMIC
000490                             RECORD KEY               IS  元－元号区分
000500                             FILE STATUS              IS  状態キー
000510                             LOCK        MODE         IS  AUTOMATIC.
000520     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000530                             ORGANIZATION             IS  INDEXED
000540                             ACCESS MODE              IS  DYNAMIC
000550                             RECORD KEY               IS  名－区分コード
000560                                                          名－名称コード
000570                             FILE STATUS              IS  状態キー
000580                             LOCK        MODE         IS  AUTOMATIC.
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
000650     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000660                             ORGANIZATION             IS  INDEXED
000670                             ACCESS MODE              IS  DYNAMIC
000680                             RECORD KEY               IS  制－制御区分
000690                             FILE STATUS              IS  状態キー
000700                             LOCK        MODE         IS  AUTOMATIC.
000710     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000720                             ORGANIZATION             IS  INDEXED
000730                             ACCESS MODE              IS  DYNAMIC
000740                             RECORD KEY               IS 施情－施術所番号
000750                             FILE STATUS              IS  状態キー
000760                             LOCK        MODE         IS  AUTOMATIC.
000770     SELECT  請求先マスタ    ASSIGN      TO        SEIKYUSL
000780                             ORGANIZATION           IS  INDEXED
000790                             ACCESS MODE            IS  DYNAMIC
000800                             RECORD KEY             IS 請先－保険種別
000810                                                       請先－保険者番号
000820                             FILE STATUS            IS  状態キー
000830                             LOCK    MODE           IS  AUTOMATIC.
000840     SELECT  経過マスタ      ASSIGN      TO        KEIKAL
000850                             ORGANIZATION             IS  INDEXED
000860                             ACCESS MODE              IS  DYNAMIC
000870                             RECORD KEY               IS  経－区分コード
000880                                                          経－経過コード
000890                             FILE STATUS              IS  状態キー
000900                             LOCK        MODE         IS  AUTOMATIC.
000910     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
000920                             ORGANIZATION             IS  INDEXED
000930                             ACCESS MODE              IS  DYNAMIC
000940                             RECORD KEY               IS  負原－区分コード
000950                                                          負原－負傷原因コード
000960                             FILE STATUS              IS  状態キー
000970                             LOCK        MODE         IS  AUTOMATIC.
000980     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000990                             ORGANIZATION             IS  INDEXED
001000                             ACCESS MODE              IS  DYNAMIC
001010                             RECORD KEY               IS 受－施術和暦年月
001020                                                          受－患者コード
001030                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
001040                                                          受－患者カナ
001050                                                          受－患者コード
001060                             ALTERNATE RECORD KEY     IS  受－患者コード
001070                                                         受－施術和暦年月
001080                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
001090                                                          受－保険種別
001100                                                          受－保険者番号
001110                                                          受－患者コード
001120                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
001130                                                          受－公費種別
001140                                                     受－費用負担者番号
001150                                                          受－患者コード
001160                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
001170                                                          受－助成種別
001180                                                  受－費用負担者番号助成
001190                                                          受－患者コード
001200                             ALTERNATE RECORD KEY  IS 受－請求和暦年月
001210                                                      受－施術和暦年月
001220                                                      受－患者コード
001230                             FILE STATUS              IS  状態キー
001240                             LOCK        MODE         IS  AUTOMATIC.
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
001250     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
001260                             ORGANIZATION             IS  INDEXED
001270                             ACCESS MODE              IS  DYNAMIC
001280                             RECORD KEY           IS 施記－施術和暦年月日
001290                                                     施記－患者コード
001300                             ALTERNATE RECORD KEY IS 施記－患者コード
001310                                                     施記－施術和暦年月日
001320                             FILE STATUS              IS  状態キー
001330                             LOCK        MODE         IS  AUTOMATIC.
001340     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
001350                             ORGANIZATION             IS  INDEXED
001360                             ACCESS MODE              IS  DYNAMIC
001370                             RECORD KEY               IS 負－施術和暦年月
001380                                                         負－患者コード
001390                             ALTERNATE RECORD KEY     IS 負－患者コード
001400                                                         負－施術和暦年月
001410                             FILE STATUS              IS  状態キー
001420                             LOCK        MODE         IS  AUTOMATIC.
001430     SELECT  料金マスタ      ASSIGN      TO        RYOUKINL
001440                             ORGANIZATION             IS  INDEXED
001450                             ACCESS MODE              IS  DYNAMIC
001460                             RECORD KEY               IS  料－区分コード
001470                                                          料－部位コード
001480                                                          料－開始和暦年月.
001490     SELECT  会情報マスタ    ASSIGN      TO        KAIJOHOL
001500                             ORGANIZATION             IS  INDEXED
001510                             ACCESS MODE              IS  DYNAMIC
000130                             RECORD KEY               IS  会情－柔整鍼灸区分
000131                                                          会情－協会コード
000132                                                          会情－保険種別
000133                                                          会情－変更和暦年月
000134                             ALTERNATE RECORD KEY     IS  会情－柔整鍼灸区分
000135                                                          会情－接骨師会カナ
000136                                                          会情－協会コード
000137                                                          会情－保険種別
000138                                                          会情－変更和暦年月
001590                             FILE STATUS              IS  状態キー
001600                             LOCK        MODE         IS  AUTOMATIC.
001610     SELECT  ＩＤ管理マスタ    ASSIGN      TO        IDKANRL
001620                             ORGANIZATION             IS  INDEXED
001630                             ACCESS MODE              IS  DYNAMIC
001640                             RECORD KEY               IS  ＩＤ管－ＩＤ区分
001650                                                          ＩＤ管－施術所番号
001660                                                          ＩＤ管－保険種別
001670                                                          ＩＤ管－保険者番号
001680                             ALTERNATE RECORD KEY     IS  ＩＤ管－施術ＩＤ番号
001690                                                          ＩＤ管－ＩＤ区分
001700                                                          ＩＤ管－施術所番号
001710                                                          ＩＤ管－保険種別
001720                                                          ＩＤ管－保険者番号
001730                             FILE STATUS              IS  状態キー
001740                             LOCK        MODE         IS  AUTOMATIC.
001755* 並び順印字用
001760     SELECT  作業ファイル２  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001770                             ORGANIZATION             IS  INDEXED
001780                             ACCESS                   IS  DYNAMIC
001790                             RECORD      KEY          IS  作２－施術和暦年月
001800                                                          作２－患者コード
001810                                                          作２－保険種別
001820                             FILE        STATUS       IS  状態キー
001830                             LOCK        MODE         IS  AUTOMATIC.
001840     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF002
001850                             SYMBOLIC    DESTINATION  IS "PRT"
001860                             FORMAT                   IS  定義体名Ｐ
001870                             GROUP                    IS  項目群名Ｐ
001880                             PROCESSING  MODE         IS  処理種別Ｐ
001890                             UNIT        CONTROL      IS  拡張制御Ｐ
001900                             FILE        STATUS       IS  通知情報Ｐ.
001910******************************************************************
001920*                      DATA DIVISION                             *
001930******************************************************************
001940 DATA                    DIVISION.
001950 FILE                    SECTION.
001990*                           ［ＲＬ＝  ２５６］
002000 FD  市町村マスタ          BLOCK   CONTAINS   1   RECORDS.
002010     COPY SITYOSN        OF  XFDLIB  JOINING   市   AS  PREFIX.
002020*                           ［ＲＬ＝  １２８］
002030 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
002040     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
002050*                           ［ＲＬ＝  １２８］
002060 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
002070     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
002110*                           ［ＲＬ＝  ２５６］
002120 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
002130     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
002140*                           ［ＲＬ＝  １２８］
002150 FD  施術所情報マスタ          BLOCK   CONTAINS   1   RECORDS.
002160     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
002170*                           ［ＲＬ＝  １２８］
002180 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
002190     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
002200*                           ［ＲＬ＝  １２８］
002210 FD  経過マスタ          BLOCK   CONTAINS   1   RECORDS.
002220     COPY KEIKA          OF  XFDLIB  JOINING   経   AS  PREFIX.
002230*                           ［ＲＬ＝  ３２０］
002240 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
002250     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
002560*                          ［ＲＬ＝  1024］
000340 FD  受診者情報２Ｆ        BLOCK   CONTAINS   1   RECORDS.
000350     COPY JUSINJ2          OF  XFDLIB  JOINING   受２   AS  PREFIX.
002260*                           ［ＲＬ＝  ２５６］
002270 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
002280     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
002290*                           ［ＲＬ＝  １２８］
002300 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002310     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
002320*                           ［ＲＬ＝  １２８］
002330 FD  負傷原因Ｆ         BLOCK   CONTAINS   1   RECORDS.
002340     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
002350*
002360 FD  料金マスタ         BLOCK   CONTAINS   1   RECORDS.
002370     COPY RYOUKIN         OF  XFDLIB  JOINING   料   AS  PREFIX.
002380     COPY RYOUKNA         OF  XFDLIB  JOINING   料Ａ AS  PREFIX.
002390     COPY RYOUKNB         OF  XFDLIB  JOINING   料Ｂ AS  PREFIX.
002400     COPY RYOUKNC         OF  XFDLIB  JOINING   料Ｃ AS  PREFIX.
002410     COPY RYOUKND         OF  XFDLIB  JOINING   料Ｄ AS  PREFIX.
002420     COPY RYOUKNE         OF  XFDLIB  JOINING   料Ｅ AS  PREFIX.
002430     COPY RYOUKNF         OF  XFDLIB  JOINING   料Ｆ AS  PREFIX.
002440*                           ［ＲＬ＝  ６４０］
002450 FD  会情報マスタ        BLOCK   CONTAINS   1   RECORDS.
002460     COPY KAIJOHO         OF  XFDLIB  JOINING   会情   AS  PREFIX.
002470*                           ［ＲＬ＝  １２８］
002480 FD  ＩＤ管理マスタ          BLOCK   CONTAINS   1   RECORDS.
002490     COPY IDKANR    OF  XFDLIB  JOINING   ＩＤ管   AS  PREFIX.
002500**
002510 FD  作業ファイル２ RECORD  CONTAINS 32 CHARACTERS.
002520 01  作２－レコード.
002530     03  作２－レコードキー.
002540         05  作２－施術和暦年月.
002550             07  作２－施術和暦            PIC 9.
002560             07  作２－施術年              PIC 9(2).
002570             07  作２－施術月              PIC 9(2).
002580         05  作２－患者コード.
002590             07 作２－患者番号             PIC 9(6).
002600             07 作２－枝番                 PIC X(1).
002610         05  作２－保険種別                PIC 9(2).
002620     03  作２－レコードデータ.
002630         05  作２－順番                    PIC 9(4).
002640         05  FILLER                        PIC X(14).
002650*
002660 FD  印刷ファイル.
002670     COPY YHP6425P        OF  XMDLIB.
002680*----------------------------------------------------------------*
002690******************************************************************
002700*                WORKING-STORAGE SECTION                         *
002710******************************************************************
002720 WORKING-STORAGE         SECTION.
002730 01 キー入力                           PIC X     VALUE SPACE.
002740 01 状態キー                           PIC X(2)  VALUE SPACE.
002750 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002760 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002770 01 初検フラグ                         PIC X(3)  VALUE SPACE.
002780 01 継続フラグ                         PIC X(3)  VALUE SPACE.
002790 01 ファイル名                         PIC N(6)  VALUE SPACE.
002800 01 レセプトＰＧＷ                     PIC X(8)  VALUE SPACE.
002810 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
002820 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
002830 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002840 01 患者番号Ｗ                         PIC 9(6)  VALUE ZERO.
002850 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002860 01 部位名称Ｗ                         PIC N(12) VALUE SPACE.
002870 01 部位長Ｗ                           PIC 9(2) VALUE 1.
002880 01 脱出フラグ                         PIC X(3)  VALUE SPACE.
002890 01 空白Ｗ                             PIC X(2)  VALUE SPACE.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002900*
002910** 数字→日本語変換
002920 01 数字Ｗ                             PIC 9(2).
002930 01 数字Ｒ REDEFINES 数字Ｗ.
002940    03 数字Ｗ１                        PIC X(1).
002950    03 数字Ｗ２                        PIC X(1).
002960*
002970 01 負傷番号Ｗ                         PIC 9.
002980 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
002990    03 負傷番号Ｗ１                    PIC X.
003000*
003010 01 全角負傷番号Ｗ                     PIC N.
003020 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
003030    03 全角負傷番号Ｗ１                PIC X(2).
003040*
003050 01 カウンタ                           PIC 9(2)  VALUE ZERO.
003060 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
003070 01 保険名称Ｗ                         PIC N(12) VALUE SPACE.
003080*
003090* 退避用
003100 01 終了年月日ＷＴ.
003110    03 終了年ＷＴ                      PIC 9(2)  VALUE ZERO.
003120    03 終了月ＷＴ                      PIC 9(2)  VALUE ZERO.
003130    03 終了日ＷＴ                      PIC 9(2)  VALUE ZERO.
003140* 初検日退避用
003150 01 初検年月日ＷＴ.
003160    03 初検和暦ＷＴ                    PIC 9     VALUE ZERO.
003170    03 初検年ＷＴ                      PIC 9(2)  VALUE ZERO.
003180    03 初検月ＷＴ                      PIC 9(2)  VALUE ZERO.
003190    03 初検日ＷＴ                      PIC 9(2)  VALUE ZERO.
003200* 負傷原因用
003210 01 負傷原因ＷＴ.
003220    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
003230    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
003240    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
003250    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
003260    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
003270    03 負傷原因ナンバーＷＴ.
003280       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
003290    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
003300 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
003310 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
003320 01 負傷原因ＴＢＬ.
003330    03 負傷原因コードＴＢＬ            OCCURS 9.
003340       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
003350       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
003360       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
003370 01 負傷原因内容Ｗ.
003380    03 負傷原因内容合成Ｗ              PIC X(318) OCCURS 9 VALUE SPACE.
003620    03 負傷原因内容分解ＸＷ.
003630       05 負傷原因内容１ＸＷ           PIC X(80)  VALUE SPACE.
003640       05 負傷原因内容２ＸＷ           PIC X(80)  VALUE SPACE.
003640       05 負傷原因内容３ＸＷ           PIC X(80)  VALUE SPACE.
003650       05 負傷原因内容４ＸＷ           PIC X(78)  VALUE SPACE.
003430*
003440* 初検加算時刻用
003450 01 初検加算ＷＴ.
003460    03 初検加算カウント                PIC 9    VALUE ZERO.
003470    03 番号カウンタ                    PIC 9    VALUE ZERO.
003480    03 初検加算集団ＷＴ  OCCURS 3.
003490       05 初検加算区分ＷＴ             PIC 9    VALUE ZERO.
003500       05 初検加算時ＷＴ               PIC 9(2) VALUE ZERO.
003510       05 初検加算分ＷＴ               PIC 9(2) VALUE ZERO.
003520    03 初検加算集団ＮＷ  OCCURS 3.
003530       05 加算区切Ｗ                   PIC N(1) VALUE SPACE.
003540       05 加算内容Ｗ                   PIC N(3) VALUE SPACE.
003550       05 初検加算時ＮＷ１             PIC N(1) VALUE SPACE.
003560       05 初検加算時ＮＷ２             PIC N(1) VALUE SPACE.
003570       05 時固定Ｗ                     PIC N(1) VALUE SPACE.
003580       05 初検加算分ＮＷ１             PIC N(1) VALUE SPACE.
003590       05 初検加算分ＮＷ２             PIC N(1) VALUE SPACE.
003600       05 分固定Ｗ                     PIC N(1) VALUE SPACE.
003610    03 初検加算時刻１Ｗ                PIC N(10) VALUE SPACE.
003620    03 初検加算時刻２Ｗ                PIC N(10) VALUE SPACE.
003630    03 初検加算時刻３Ｗ                PIC N(10) VALUE SPACE.
003070    03 初検加算区切Ｗ                  PIC X     VALUE SPACE.
003080    03 初検加算時Ｗ                    PIC 9(2)  VALUE ZERO.
003090    03 初検加算分Ｗ                    PIC 9(2)  VALUE ZERO.
003640*
003650** 前月初検のみ用
003660 01 初日再検フラグ                     PIC X(3)  VALUE SPACE.
003670 01 前月フラグ                         PIC X(3)  VALUE SPACE.
003680*
003690 01 計算年月日Ｗ.
003700    03 計算和暦Ｗ                      PIC 9(1)  VALUE ZERO.
003710    03 計算年Ｗ                        PIC S9(2)  VALUE ZERO.
003720    03 計算月Ｗ                        PIC S9(2)  VALUE ZERO.
003730    03 計算日Ｗ                        PIC S9(2)  VALUE ZERO.
003740 01 開始年月日２Ｗ.
003750    03 開始和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003760    03 開始年２Ｗ                      PIC 9(2)  VALUE ZERO.
003770    03 開始月２Ｗ                      PIC 9(2)  VALUE ZERO.
003780    03 開始日２Ｗ                      PIC 9(2)  VALUE ZERO.
003790    03 開始西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003800 01 終了年月日２Ｗ.
003810    03 終了和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003820    03 終了年２Ｗ                      PIC 9(2)  VALUE ZERO.
003830    03 終了月２Ｗ                      PIC 9(2)  VALUE ZERO.
003840    03 終了日２Ｗ                      PIC 9(2)  VALUE ZERO.
003850    03 終了西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003860***
003870** 負傷原因・長期理由印刷区分用
003880 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
003890 01 長期理由印刷区分Ｗ                 PIC 9 VALUE ZERO.
003900*
003910** レセ下段の日付区分用 (0:最終通院日、1:月末日、9:印字なし)
003920 01 レセプト日付区分Ｗ                 PIC 9 VALUE ZERO.
003930 01 レセプト患者日付区分Ｗ             PIC 9 VALUE ZERO.
003940*
003950** 月末日用
003960 01 施術西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
003970 01 商Ｗ                               PIC 9(3)  VALUE ZERO.
003980 01 余Ｗ                               PIC 9(3)  VALUE ZERO.
003990*
004000** 給付割合用
004010 01 負担割合数字Ｗ                     PIC 9     VALUE ZERO.
004020 01 給付割合ＷＰ.
          03 桁合せＷ                        PIC X     VALUE SPACE.
          03 給付割合Ｗ                      PIC X     VALUE SPACE.
004030 01 負担割合Ｗ                         PIC 9(2)  VALUE ZERO.
004040 01 割合固定Ｗ                         PIC N     VALUE SPACE.
       01 後印字Ｗ                           PIC N     VALUE SPACE.
004050*
004060** 枝番判定用
004070 01 開始診療日手動区分Ｗ               PIC 9    VALUE ZERO.
004080*
004090** 請求先名称用
004100 01 請求先名称ＴＢＬ.
004110    03 請求先名称ＷＴ                  PIC X(2)  OCCURS 20 VALUE SPACE.
004120 01 請求先名称ＷＴ１                   PIC X(2)  VALUE SPACE.
004130 01 支部部署名Ｗ                       PIC X(40) VALUE SPACE.
004140*
004150* 帳票固定印字用
004160 01 生年月日固定Ｗ                     PIC N(4)   VALUE SPACE.
004170 01 県共済固定Ｗ                       PIC N(15)  VALUE SPACE.
004180*
004190* 会長委任文用
004200* 01 会長委任文１Ｗ                     PIC N(18)  VALUE SPACE.
004210* 01 会長委任文２Ｗ.
004220*    03 会長委任文２１Ｗ                PIC N(4)   VALUE SPACE.
004230*    03 会長名Ｗ                        PIC N(5)   VALUE SPACE.
004240*    03 会長委任文２２Ｗ                PIC N(9)   VALUE SPACE.
       01 会長委任文Ｗ.
          03 会長委任文１Ｗ                  PIC X(45)  VALUE SPACE.
          03 会長委任文２Ｗ                  PIC X(45)  VALUE SPACE.
          03 会長委任文３Ｗ                  PIC X(45)  VALUE SPACE.
004250*
004251*
004252** レセ摘要用( N(38)固定） /
004253 01 負傷の経過Ｗ.
004254*    03 負傷の経過行Ｗ                  PIC X(76) OCCURS 2 VALUE SPACE.
004254    03 負傷の経過行Ｗ                  PIC X(64) OCCURS 2 VALUE SPACE.
004255 01 負傷の経過ＮＷ REDEFINES 負傷の経過Ｗ.
004256*    03 負傷の経過行ＮＷ                PIC N(38) OCCURS 2.
004256    03 負傷の経過行ＮＷ                PIC N(32) OCCURS 2.
004257*
004258* 負傷原因印刷区分
004259 01 レセ負傷原因印刷区分Ｗ             PIC 9    VALUE ZERO.
002580 01 レセ長期理由印刷区分Ｗ             PIC 9    VALUE ZERO.
      *
      */金属副子・運動後療の変更・追加/1805
       01 金属副子ＣＭ                       PIC X(200) VALUE SPACE.
       01 運動後療ＣＭ                       PIC X(68)  VALUE SPACE.
004260*
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
004261****************
004270* 連結項目待避 *
004280****************
004290*    ************
004300*    * 印刷キー *
004310*    ************
004320 01 対象データＷＲ.
004330    03 施術和暦年月ＷＲ.
004340       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
004350       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
004360       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
004370    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004380    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
004390    03 公費種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004400    03 費用負担者番号ＷＲ               PIC X(10) VALUE SPACE.
004410    03 助成種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004420    03 費用負担者番号助成ＷＲ           PIC X(10) VALUE SPACE.
004430    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
004440    03 患者カナＷＲ                     PIC X(50) VALUE SPACE.
004450    03 患者コードＷＲ.
004460       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
004470       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
004480*
004490*    ****************
004500*    * 基本料金情報 *
004510*    ****************
004520 01 基本料金Ｗ.
004530   03 冷罨法単価Ｗ                      PIC 9(4)  VALUE ZERO.
004540   03 温罨法単価Ｗ                      PIC 9(4)  VALUE ZERO.
004550   03 電療単価Ｗ                        PIC 9(4)  VALUE ZERO.
004560*    ************
004570*    * 料金情報 *
004580*    ************
004590*    月毎の料金
004600***********************
004610 01 料金１ＷＲ.
004620   03 初検ＷＲ.
004630      05 負担割合ＷＲ               PIC 9(3)    VALUE ZERO.
004640      05 初検料ＷＲ                 PIC 9(5)    VALUE ZERO.
004650      05 初検加算料ＷＲ             PIC 9(5)    VALUE ZERO.
         03 初検時相談料ＷＲ              PIC 9(4)    VALUE ZERO.
004660   03 再検料ＷＲ                    PIC 9(5)    VALUE ZERO.
004670   03 往療ＷＲ.
004680      05 往療距離ＷＲ               PIC 9(2)V9  VALUE ZERO.
004690      05 往療回数ＷＲ               PIC 9(2)    VALUE ZERO.
004700      05 往療料ＷＲ                 PIC 9(5)    VALUE ZERO.
004710      05 往療加算料ＷＲ             PIC 9(5)    VALUE ZERO.
004720   03 金属副子加算料ＷＲ            PIC 9(5)    VALUE ZERO.
004730   03 施術情報提供料ＷＲ            PIC 9(5)    VALUE ZERO.
004740   03 合計ＷＲ                      PIC 9(6)    VALUE ZERO.
004750   03 一部負担金ＷＲ                PIC 9(6)    VALUE ZERO.
004760   03 請求金額ＷＲ                  PIC 9(6)    VALUE ZERO.
004770   03 給付割合ＷＲ                  PIC 9(1)    VALUE ZERO.
004780   03 受給者負担額ＷＲ              PIC 9(6)    VALUE ZERO.
004790   03 助成請求金額ＷＲ              PIC 9(6)    VALUE ZERO.
004800*
004810* 負傷部位毎の料金
004820***********************
004830 01 料金２ＷＲ.
004840   03 初回処置ＷＲ    OCCURS   9.
004850      05 初回処置料ＷＲ             PIC 9(5)    VALUE ZERO.
004860*
004870* 逓減毎の料金
004880***********************
004890 01 料金３ＷＲ.
004900**********
004910* １部位 *
004920**********
004930   03 部位１ＷＲ.
004940      05 後療１ＷＲ.
004950         07 後療単価１ＷＲ              PIC 9(4)    VALUE ZERO.
004960         07 後療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
004970         07 後療料１ＷＲ                PIC 9(5)    VALUE ZERO.
004980      05 冷罨法１ＷＲ.
004990         07 冷罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
005000         07 冷罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
005010      05 温罨法１ＷＲ.
005020         07 温罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
005030         07 温罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
005040      05 電療１ＷＲ.
005050         07 電療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
005060         07 電療料１ＷＲ                PIC 9(4)    VALUE ZERO.
005070      05 小計１ＷＲ                     PIC 9(6)    VALUE ZERO.
005080      05 長期逓減率１ＷＲ               PIC 9(3)    VALUE ZERO.
005090      05 長期込小計１ＷＲ               PIC 9(6)    VALUE ZERO.
005100**********
005110* ２部位 *
005120**********
005130   03 部位２ＷＲ.
005140      05 後療２ＷＲ.
005150         07 後療単価２ＷＲ              PIC 9(4)    VALUE ZERO.
005160         07 後療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
005170         07 後療料２ＷＲ                PIC 9(5)    VALUE ZERO.
005180      05 冷罨法２ＷＲ.
005190         07 冷罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
005200         07 冷罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
005210      05 温罨法２ＷＲ.
005220         07 温罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
005230         07 温罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
005240      05 電療２ＷＲ.
005250         07 電療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
005260         07 電療料２ＷＲ                PIC 9(4)    VALUE ZERO.
005270      05 小計２ＷＲ                     PIC 9(6)    VALUE ZERO.
005280      05 長期逓減率２ＷＲ               PIC 9(3)    VALUE ZERO.
005290      05 長期込小計２ＷＲ               PIC 9(6)    VALUE ZERO.
005300******************
005310* ３部位／８割 *
005320******************
005330   03 部位３８ＷＲ.
005340      05 後療３８ＷＲ.
005350         07 後療単価３８ＷＲ              PIC 9(4)  VALUE ZERO.
005360         07 後療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
005370         07 後療料３８ＷＲ                PIC 9(5)  VALUE ZERO.
005380      05 冷罨法３８ＷＲ.
005390         07 冷罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
005400         07 冷罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
005410      05 温罨法３８ＷＲ.
005420         07 温罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
005430         07 温罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
005440      05 電療３８ＷＲ.
005450         07 電療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
005460         07 電療料３８ＷＲ                PIC 9(4)  VALUE ZERO.
005470      05 小計３８ＷＲ                     PIC 9(6)  VALUE ZERO.
005480      05 多部位込小計３８ＷＲ             PIC 9(6)  VALUE ZERO.
005490      05 長期逓減率３８ＷＲ               PIC 9(3)  VALUE ZERO.
005500      05 長期込小計３８ＷＲ               PIC 9(6)  VALUE ZERO.
005510******************
005520* ３部位／１０割 *
005530******************
005540   03 部位３０ＷＲ.
005550      05 逓減開始月日３０ＷＲ.
005560         07 逓減開始月３０ＷＲ            PIC 9(2)  VALUE ZERO.
005570         07 逓減開始日３０ＷＲ            PIC 9(2)  VALUE ZERO.
005580      05 後療３０ＷＲ.
005590         07 後療単価３０ＷＲ              PIC 9(4)  VALUE ZERO.
005600         07 後療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
005610         07 後療料３０ＷＲ                PIC 9(5)  VALUE ZERO.
005620      05 冷罨法３０ＷＲ.
005630         07 冷罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
005640         07 冷罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
005650      05 温罨法３０ＷＲ.
005660         07 温罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
005670         07 温罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
005680      05 電療３０ＷＲ.
005690         07 電療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
005700         07 電療料３０ＷＲ                PIC 9(4)  VALUE ZERO.
005710      05 小計３０ＷＲ                     PIC 9(6)  VALUE ZERO.
005720      05 長期逓減率３０ＷＲ               PIC 9(3)  VALUE ZERO.
005730      05 長期込小計３０ＷＲ               PIC 9(6)  VALUE ZERO.
005740****************
005750* ４部位／５割 *
005760****************
005770   03 部位４５ＷＲ.
005780      05 後療４５ＷＲ.
005790         07 後療単価４５ＷＲ              PIC 9(4)  VALUE ZERO.
005800         07 後療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005810         07 後療料４５ＷＲ                PIC 9(5)  VALUE ZERO.
005820      05 冷罨法４５ＷＲ.
005830         07 冷罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005840         07 冷罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
005850      05 温罨法４５ＷＲ.
005860         07 温罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005870         07 温罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
005880      05 電療４５ＷＲ.
005890         07 電療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005900         07 電療料４５ＷＲ                PIC 9(4)  VALUE ZERO.
005910      05 小計４５ＷＲ                     PIC 9(6)  VALUE ZERO.
005920      05 多部位込小計４５ＷＲ             PIC 9(6)  VALUE ZERO.
005930      05 長期逓減率４５ＷＲ               PIC 9(3)  VALUE ZERO.
005940      05 長期込小計４５ＷＲ               PIC 9(6)  VALUE ZERO.
005950****************
005960* ４部位／８割 *
005970****************
005980   03 部位４８ＷＲ.
005990      05 逓減開始月日４８ＷＲ.
006000         07 逓減開始月４８ＷＲ            PIC 9(2)  VALUE ZERO.
006010         07 逓減開始日４８ＷＲ            PIC 9(2)  VALUE ZERO.
006020      05 後療４８ＷＲ.
006030         07 後療単価４８ＷＲ              PIC 9(4)  VALUE ZERO.
006040         07 後療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
006050         07 後療料４８ＷＲ                PIC 9(5)  VALUE ZERO.
006060      05 冷罨法４８ＷＲ.
006070         07 冷罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
006080         07 冷罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
006090      05 温罨法４８ＷＲ.
006100         07 温罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
006110         07 温罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
006120      05 電療４８ＷＲ.
006130         07 電療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
006140         07 電療料４８ＷＲ                PIC 9(4)  VALUE ZERO.
006150      05 小計４８ＷＲ                     PIC 9(6)  VALUE ZERO.
006160      05 多部位込小計４８ＷＲ             PIC 9(6)  VALUE ZERO.
006170      05 長期逓減率４８ＷＲ               PIC 9(3)  VALUE ZERO.
006180      05 長期込小計４８ＷＲ               PIC 9(6)  VALUE ZERO.
006190******************
006200* ４部位／１０割 *
006210******************
006220   03 部位４０ＷＲ.
006230      05 逓減開始月日４０ＷＲ.
006240         07 逓減開始月４０ＷＲ            PIC 9(2)  VALUE ZERO.
006250         07 逓減開始日４０ＷＲ            PIC 9(2)  VALUE ZERO.
006260      05 後療４０ＷＲ.
006270         07 後療単価４０ＷＲ              PIC 9(4)  VALUE ZERO.
006280         07 後療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
006290         07 後療料４０ＷＲ                PIC 9(5)  VALUE ZERO.
006300      05 冷罨法４０ＷＲ.
006310         07 冷罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
006320         07 冷罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
006330      05 温罨法４０ＷＲ.
006340         07 温罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
006350         07 温罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
006360      05 電療４０ＷＲ.
006370         07 電療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
006380         07 電療料４０ＷＲ                PIC 9(4)  VALUE ZERO.
006390      05 小計４０ＷＲ                     PIC 9(6)  VALUE ZERO.
006400      05 長期逓減率４０ＷＲ               PIC 9(3)  VALUE ZERO.
006410      05 長期込小計４０ＷＲ               PIC 9(6)  VALUE ZERO.
006420********************
006430* ５部位／２．５割 *
006440********************
006450   03 部位５２ＷＲ.
006460      05 後療５２ＷＲ.
006470         07 後療単価５２ＷＲ              PIC 9(4)  VALUE ZERO.
006480         07 後療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
006490         07 後療料５２ＷＲ                PIC 9(5)  VALUE ZERO.
006500      05 冷罨法５２ＷＲ.
006510         07 冷罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
006520         07 冷罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
006530      05 温罨法５２ＷＲ.
006540         07 温罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
006550         07 温罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
006560      05 電療５２ＷＲ.
006570         07 電療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
006580         07 電療料５２ＷＲ                PIC 9(4)  VALUE ZERO.
006590      05 小計５２ＷＲ                     PIC 9(6)  VALUE ZERO.
006600      05 多部位込小計５２ＷＲ             PIC 9(6)  VALUE ZERO.
006610      05 長期逓減率５２ＷＲ               PIC 9(3)  VALUE ZERO.
006620      05 長期込小計５２ＷＲ               PIC 9(6)  VALUE ZERO.
006630****************
006640* ５部位／５割 *
006650****************
006660   03 部位５５ＷＲ.
006670      05 逓減開始月日５５ＷＲ.
006680         07 逓減開始月５５ＷＲ            PIC 9(2)  VALUE ZERO.
006690         07 逓減開始日５５ＷＲ            PIC 9(2)  VALUE ZERO.
006700      05 後療５５ＷＲ.
006710         07 後療単価５５ＷＲ              PIC 9(4)  VALUE ZERO.
006720         07 後療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
006730         07 後療料５５ＷＲ                PIC 9(5)  VALUE ZERO.
006740      05 冷罨法５５ＷＲ.
006750         07 冷罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
006760         07 冷罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
006770      05 温罨法５５ＷＲ.
006780         07 温罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
006790         07 温罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
006800      05 電療５５ＷＲ.
006810         07 電療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
006820         07 電療料５５ＷＲ                PIC 9(4)  VALUE ZERO.
006830      05 小計５５ＷＲ                     PIC 9(6)  VALUE ZERO.
006840      05 多部位込小計５５ＷＲ             PIC 9(6)  VALUE ZERO.
006850      05 長期逓減率５５ＷＲ               PIC 9(3)  VALUE ZERO.
006860      05 長期込小計５５ＷＲ               PIC 9(6)  VALUE ZERO.
006870****************
006880* ５部位／８割 *
006890****************
006900   03 部位５８ＷＲ.
006910      05 逓減開始月日５８ＷＲ.
006920         07 逓減開始月５８ＷＲ            PIC 9(2)  VALUE ZERO.
006930         07 逓減開始日５８ＷＲ            PIC 9(2)  VALUE ZERO.
006940      05 後療５８ＷＲ.
006950         07 後療単価５８ＷＲ              PIC 9(4)  VALUE ZERO.
006960         07 後療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
006970         07 後療料５８ＷＲ                PIC 9(5)  VALUE ZERO.
006980      05 冷罨法５８ＷＲ.
006990         07 冷罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
007000         07 冷罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
007010      05 温罨法５８ＷＲ.
007020         07 温罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
007030         07 温罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
007040      05 電療５８ＷＲ.
007050         07 電療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
007060         07 電療料５８ＷＲ                PIC 9(4)  VALUE ZERO.
007070      05 小計５８ＷＲ                     PIC 9(6)  VALUE ZERO.
007080      05 多部位込小計５８ＷＲ             PIC 9(6)  VALUE ZERO.
007090      05 長期逓減率５８ＷＲ               PIC 9(3)  VALUE ZERO.
007100      05 長期込小計５８ＷＲ               PIC 9(6)  VALUE ZERO.
007110******************
007120* ５部位／１０割 *
007130******************
007140   03 部位５０ＷＲ.
007150      05 逓減開始月日５０ＷＲ.
007160         07 逓減開始月５０ＷＲ            PIC 9(2)  VALUE ZERO.
007170         07 逓減開始日５０ＷＲ            PIC 9(2)  VALUE ZERO.
007180      05 後療５０ＷＲ.
007190         07 後療単価５０ＷＲ              PIC 9(4)  VALUE ZERO.
007200         07 後療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
007210         07 後療料５０ＷＲ                PIC 9(5)  VALUE ZERO.
007220      05 冷罨法５０ＷＲ.
007230         07 冷罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
007240         07 冷罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
007250      05 温罨法５０ＷＲ.
007260         07 温罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
007270         07 温罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
007280      05 電療５０ＷＲ.
007290         07 電療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
007300         07 電療料５０ＷＲ                PIC 9(4)  VALUE ZERO.
007310      05 小計５０ＷＲ                     PIC 9(6)  VALUE ZERO.
007320      05 長期逓減率５０ＷＲ               PIC 9(3)  VALUE ZERO.
007330      05 長期込小計５０ＷＲ               PIC 9(6)  VALUE ZERO.
007340*
007350**************
007360* 施術所情報 *
007370**************
007380 01 施術所情報Ｗ.
007390    03 柔整師番号Ｗ                    PIC X(22)  VALUE SPACE.
007400    03 印刷接骨師会会員番号Ｗ.
007410       05 接骨師会名Ｗ                 PIC X(8)   VALUE SPACE.
007420       05 接骨師会会員番号Ｗ           PIC X(10)  VALUE SPACE.
007430    03 代表者カナＷ                    PIC X(50)  VALUE SPACE.
007440    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
007450    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
          03 都道府県ＪＩＳＷ                PIC X(2)   VALUE SPACE.
007460    03 施術所住所Ｗ.
007470       05 施術所住所１Ｗ               PIC X(50)  VALUE SPACE.
007480       05 施術所住所２Ｗ               PIC X(50)  VALUE SPACE.
007490*    03 施術所住所Ｗ.
007500*       05 施術所住所１Ｗ               PIC X(28)  VALUE SPACE.
007510*       05 施術所住所２Ｗ               PIC X(28)  VALUE SPACE.
007520*       05 施術所住所３Ｗ               PIC X(28)  VALUE SPACE.
007530*
007540    03 施術所郵便番号Ｗ.
007550       05 施術所郵便番号１Ｗ           PIC X(3)   VALUE SPACE.
007560       05 施術所郵便番号２Ｗ           PIC X(4)   VALUE SPACE.
007570    03 施術所電話番号Ｗ                PIC X(15)  VALUE SPACE.
007580    03 定額制受理番号Ｗ                PIC X(15)  VALUE SPACE.
007590    03 受理年月日Ｗ.
007600       05 受理年Ｗ                     PIC 9(2)   VALUE ZERO.
007610       05 受理月Ｗ                     PIC 9(2)   VALUE ZERO.
007620       05 受理日Ｗ                     PIC 9(2)   VALUE ZERO.
007630    03 最終通院年月日Ｗ.
007640       05 最終通院年Ｗ                 PIC 9(2)   VALUE ZERO.
007650       05 最終通院月Ｗ                 PIC 9(2)   VALUE ZERO.
007660       05 最終通院日Ｗ                 PIC 9(2)   VALUE ZERO.
007670    03 柔整師年月日Ｗ.
007680       05 柔整師年Ｗ                   PIC 9(2)   VALUE ZERO.
007690       05 柔整師月Ｗ                   PIC 9(2)   VALUE ZERO.
007700       05 柔整師日Ｗ                   PIC 9(2)   VALUE ZERO.
007710    03 患者委任年月日Ｗ.
007720       05 患者委任年Ｗ                 PIC 9(2)   VALUE ZERO.
007730       05 患者委任月Ｗ                 PIC 9(2)   VALUE ZERO.
007740       05 患者委任日Ｗ                 PIC 9(2)   VALUE ZERO.
007750    03 取引先情報Ｗ.
007760       05 取引先銀行名Ｗ               PIC X(40)  VALUE SPACE.
007770       05 取引先銀行支店名Ｗ           PIC X(40)  VALUE SPACE.
007780       05 預金種別Ｗ                   PIC 9(1)   VALUE ZERO.
007790       05 口座番号Ｗ                   PIC X(10)  VALUE SPACE.
007800       05 口座名義人Ｗ                 PIC X(40)  VALUE SPACE.
007810       05 口座名義人カナＷ             PIC X(40)  VALUE SPACE.
007820       05 銀行名支店名Ｗ               PIC X(60)  VALUE SPACE.
007830       05 預金種別コメントＷ           PIC N(4)   VALUE SPACE.
          03 支払機関.
             05 金融機関名Ｗ.
                07 金融機関名１Ｗ            PIC X(8) VALUE SPACE.
                07 金融機関名２Ｗ            PIC X(8) VALUE SPACE.
                07 金融機関名３Ｗ            PIC X(8) VALUE SPACE.
                07 金融機関名４Ｗ            PIC X(8) VALUE SPACE.
             05 支店名Ｗ.
                07 支店名１Ｗ                PIC X(8) VALUE SPACE.
                07 支店名２Ｗ                PIC X(8) VALUE SPACE.
                07 支店名３Ｗ                PIC X(8) VALUE SPACE.
                07 支店名４Ｗ                PIC X(8) VALUE SPACE.
             05 振込チェックＷ               PIC N(1)  VALUE SPACE.
             05 普通チェックＷ               PIC N(1)  VALUE SPACE.
             05 当座チェックＷ               PIC N(1)  VALUE SPACE.
             05 銀行チェックＷ               PIC N(1)  VALUE SPACE.
             05 金庫チェックＷ               PIC N(1)  VALUE SPACE.
             05 農協チェックＷ               PIC N(1)  VALUE SPACE.
             05 本店チェックＷ               PIC N(1)  VALUE SPACE.
             05 支店チェックＷ               PIC N(1)  VALUE SPACE.
             05 本支所チェックＷ             PIC N(1)  VALUE SPACE.
007840    03 県施術ＩＤＷ                    PIC X(15)  VALUE SPACE.
007850    03 市町村施術ＩＤＷ                PIC X(15)  VALUE SPACE.
007860**************
007870* 受診者情報 *
007880**************
007890 01 受診者情報Ｗ.
007900*    03 施術和暦Ｗ                      PIC N(2)  VALUE SPACE.
      */元号修正/20190408
          03 施術和暦Ｗ                      PIC 9(1)   VALUE ZERO.
007910    03 施術年月Ｗ.
007920       05 施術年Ｗ                     PIC 9(2)   VALUE ZERO.
007930       05 施術月Ｗ                     PIC 9(2)   VALUE ZERO.
007940*    03 記号Ｗ                          PIC N(12)  VALUE SPACE.
007570    03 記号Ｗ.
007580       05 印刷記号Ｗ                   PIC N(12)  VALUE SPACE.
007950*
007960    03 番号Ｗ.
007970       05 印刷番号Ｗ                   PIC X(15)  VALUE SPACE.
007980       05 FILLER                       PIC X(15)  VALUE SPACE.
007990*    03 番号Ｗ.
008000*       05 印刷番号１Ｗ                 PIC X(10)  VALUE SPACE.
008010*       05 印刷番号２Ｗ                 PIC X(10)  VALUE SPACE.
008020*       05 FILLER                       PIC X(10)  VALUE SPACE.
008030*
          03 記号番号Ｗ.
             05 記号番号ＸＷ                 PIC X(40) VALUE SPACE.
008040    03 保険者番号Ｗ.
008050       05 印刷保険者番号Ｗ             PIC X(8)   VALUE SPACE.
008060       05 FILLER                       PIC X(2)   VALUE SPACE.
008070*
008080    03 市町村番号Ｗ.
008090       05 印刷市町村番号Ｗ             PIC X(8)   VALUE SPACE.
008100       05 FILLER                       PIC X(2)   VALUE SPACE.
008110*    03 受給者番号Ｗ.
008120*       05 印刷受給者番号Ｗ             PIC X(8)   VALUE SPACE.
008130*       05 FILLER                       PIC X(12).
           03 受給者番号Ｗ.
              05 印刷受給者番号Ｗ            PIC X(7)  VALUE SPACE.
              05 印刷受給者番号２Ｗ          PIC X(8)  VALUE SPACE.
008140*
008150    03 請求先名称Ｗ.
008160       05 印刷請求先名称１Ｗ           PIC X(48)  VALUE SPACE.
008170       05 印刷請求先名称２Ｗ           PIC X(48)  VALUE SPACE.
008180*
008190    03 保険種別Ｗ                      PIC 9(2)   VALUE ZERO.
008200    03 被保険者情報Ｗ.
008210       05 被保険者カナＷ               PIC X(50)  VALUE SPACE.
008220       05 被保険者氏名Ｗ               PIC X(50)  VALUE SPACE.
008230       05 被保険者性別Ｗ               PIC N(1)   VALUE SPACE.
008240       05 郵便番号Ｗ.
008250          07 郵便番号１Ｗ              PIC X(3)   VALUE SPACE.
008260          07 郵便番号２Ｗ              PIC X(4)   VALUE SPACE.
008270       05 被保険者住所１Ｗ             PIC X(50)  VALUE SPACE.
008280       05 被保険者住所２Ｗ             PIC X(50)  VALUE SPACE.
008290    03 患者情報Ｗ.
008300       05 患者住所Ｗ.
008310          07 患者住所１Ｗ              PIC X(50)  VALUE SPACE.
008320          07 患者住所２Ｗ              PIC X(50)  VALUE SPACE.
008990       05 電話番号Ｗ                   PIC X(35)  VALUE SPACE.
008330       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
008340       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
008350       05 患者性別Ｗ                   PIC X(4)   VALUE SPACE.
008360       05 性別チェックＷ.
008370          07 男チェックＷ              PIC N(1)  VALUE SPACE.
008380          07 女チェックＷ              PIC N(1)  VALUE SPACE.
008390       05 和暦チェックＷ.
008400          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
008410          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
008420          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
008430          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
008440          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
      */元号修正/↓↓↓20190408
008210          07 令和チェックＷ            PIC N(1)  VALUE SPACE.
                07 令和ＣＭＷ                PIC X(4)  VALUE SPACE.
009110*          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
      */元号修正/↑↑↑20190408
008450       05 患者年Ｗ                     PIC 9(2)  VALUE ZERO.
008460       05 患者月Ｗ                     PIC 9(2)  VALUE ZERO.
008470       05 患者日Ｗ                     PIC 9(2)  VALUE ZERO.
008480       05 続柄Ｗ.
008490          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
008500          07 FILLER                    PIC X(4)  VALUE SPACE.
008510*
008520*       05 負傷原因Ｗ                   PIC N(40) OCCURS 27 VALUE SPACE.
      */半角対応/110421
             05 負傷原因Ｗ OCCURS 29.
                07 負傷原因ＸＷ              PIC X(80)  VALUE SPACE.
008530*
008540       05 保険種別名称Ｗ               PIC N(3)  VALUE SPACE.
008540       05 保険種別名称２Ｗ             PIC N(5)  VALUE SPACE.
008910       05 保険種別チェックＷ.
                07 国保チェックＷ            PIC N(1)   VALUE SPACE.
                07 協会チェックＷ            PIC N(1)   VALUE SPACE.
                07 組合チェックＷ            PIC N(1)   VALUE SPACE.
                07 共済チェックＷ            PIC N(1)   VALUE SPACE.
                07 後期チェックＷ            PIC N(1)   VALUE SPACE.
                07 退職チェックＷ            PIC N(1)   VALUE SPACE.
             05 本人チェックＷ               PIC N(1)   VALUE SPACE.
             05 家族チェックＷ               PIC N(1)   VALUE SPACE.
             05 単独チェックＷ               PIC N(1)   VALUE SPACE.
             05 ２併チェックＷ               PIC N(1)   VALUE SPACE.
             05 高一チェックＷ               PIC N(1)   VALUE SPACE.
             05 高７チェックＷ               PIC N(1)   VALUE SPACE.
             05 ６歳チェックＷ               PIC N(1)   VALUE SPACE.
             05 ７割チェックＷ               PIC N(1)   VALUE SPACE.
             05 ８割チェックＷ               PIC N(1)   VALUE SPACE.
             05 ９割チェックＷ               PIC N(1)   VALUE SPACE.
             05 １０割チェックＷ             PIC N(1)   VALUE SPACE.
008550*
008560    03 助成印Ｗ                        PIC N(1)  VALUE SPACE.
008561    03 特別マークＷ                    PIC N(1)  VALUE SPACE.
008562    03 特別コメントＷ                  PIC X(16) VALUE SPACE.
008570*
008580****************
008590* 負傷データＦ *
008600****************
008610 01 負傷情報Ｗ.
008620    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
008630    03 部位情報Ｗ  OCCURS   9.
008640       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
008650       05 部位コードＷ.
008660          07 負傷種別Ｗ                PIC 9(2)  VALUE ZERO.
008670          07 部位Ｗ                    PIC 9(2)  VALUE ZERO.
008680          07 左右区分Ｗ                PIC 9(1)  VALUE ZERO.
008690          07 負傷位置番号Ｗ            PIC 9(2)  VALUE ZERO.
008700       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
008710       05 負傷年月日Ｗ.
008720          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
008730          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
008740          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
008750       05 初検年月日Ｗ.
008760          07 初検年Ｗ                  PIC 9(2)  VALUE ZERO.
008770          07 初検月Ｗ                  PIC 9(2)  VALUE ZERO.
008780          07 初検日Ｗ                  PIC 9(2)  VALUE ZERO.
008790       05 開始年月日Ｗ.
008800          07 開始年Ｗ                  PIC 9(2)  VALUE ZERO.
008810          07 開始月Ｗ                  PIC 9(2)  VALUE ZERO.
008820          07 開始日Ｗ                  PIC 9(2)  VALUE ZERO.
008830       05 終了年月日Ｗ.
008840          07 終了年Ｗ                  PIC 9(2)  VALUE ZERO.
008850          07 終了月Ｗ                  PIC 9(2)  VALUE ZERO.
008860          07 終了日Ｗ                  PIC 9(2)  VALUE ZERO.
008870       05 実日数Ｗ                     PIC 9(2)  VALUE ZERO.
008880       05 転帰区分Ｗ                   PIC 9(1)  VALUE ZERO.
008890       05 転帰区分チェックＷ.
008900          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
008910          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
008920          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
008930       05 転帰Ｗ                       PIC N(2)  VALUE SPACE.
008940       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
008950       05 部位区切Ｗ                   PIC X(1)  VALUE SPACE.
008960       05 経過略称Ｗ.
008970          07 印刷経過略称Ｗ            PIC N(5)  VALUE SPACE.
008980          07 FILLER                    PIC X(2)  VALUE SPACE.
008990    03 経過部位Ｗ                      PIC N(1)  VALUE SPACE.
009000    03 新規チェックＷ                  PIC N(1)  VALUE SPACE.
009010    03 継続チェックＷ                  PIC N(1)  VALUE SPACE.
009020    03 請求区分Ｗ                      PIC N(2)  VALUE SPACE.
009030*
009040************
009050* 料金情報 *
009060************
009070 01 料金情報Ｗ.
009080    03 初検加算Ｗ.
009090       05 時間外チェックＷ                PIC N(1) VALUE SPACE.
009100       05 休日チェックＷ                  PIC N(1) VALUE SPACE.
009110       05 深夜チェックＷ                  PIC N(1) VALUE SPACE.
009120       05 時間外Ｗ                        PIC N(3) VALUE SPACE.
009130       05 休日Ｗ                          PIC N(2) VALUE SPACE.
009140       05 深夜Ｗ                          PIC N(2) VALUE SPACE.
009150       05 初検加算内容Ｗ                  PIC N(10) VALUE SPACE.
009160    03 往療加算Ｗ.
009170       05 夜間チェックＷ                  PIC N(1) VALUE SPACE.
009180       05 往療深夜チェックＷ              PIC N(1) VALUE SPACE.
009190       05 難路チェックＷ                  PIC N(1) VALUE SPACE.
009200       05 暴風雨雪チェックＷ              PIC N(1) VALUE SPACE.
009210    03 金属副子チェックＷ.
009220       05 大チェックＷ                    PIC N(1) VALUE SPACE.
009230       05 中チェックＷ                    PIC N(1) VALUE SPACE.
009240       05 小チェックＷ                    PIC N(1) VALUE SPACE.
009250       05 金属大Ｗ                        PIC N(1) VALUE SPACE.
009260       05 金属中Ｗ                        PIC N(1) VALUE SPACE.
009270       05 金属小Ｗ                        PIC N(1) VALUE SPACE.
009280    03 小計Ｗ                             PIC 9(7) VALUE ZERO.
009290    03 初回処置料合計Ｗ                   PIC 9(6) VALUE ZERO.
009300    03 初回処置料チェックＷ.
009310       05 整復料チェックＷ                PIC N(1) VALUE SPACE.
009320       05 固定料チェックＷ                PIC N(1) VALUE SPACE.
009330       05 施療料チェックＷ                PIC N(1) VALUE SPACE.
      */金属副子・運動後療の変更・追加/1805
          03 金属回数Ｗ                         PIC 9(2)  VALUE ZERO.
          03 運動料Ｗ                           PIC 9(4)  VALUE ZERO.
009340************
009350* 備考情報 *
009360************
009370 01 備考情報Ｗ.
009380    03 適用１Ｗ                           PIC N(38) VALUE SPACE.
009390    03 適用２Ｗ                           PIC N(38) VALUE SPACE.
009400*    03 適用３Ｗ                        PIC N(38) VALUE SPACE.
009410*    03 適用４Ｗ                        PIC N(38) VALUE SPACE.
009420*    03 経過コメントＷ                     PIC N(60) VALUE SPACE.
009430*
009440* 欄外項目 *
009450    03 レセプト管理年Ｗ.
009460       05 レセ管理世紀Ｗ                  PIC 9(2)  VALUE ZERO.
009470       05 レセ管理西暦Ｗ                  PIC 9(2)  VALUE ZERO.
009480    03 総括表順番Ｗ                       PIC 9(4)  VALUE ZERO.
009870*
002060** 制御マスタ用
002140 01 レセプト新旧区分.
002150    03 助成レセＷ                      PIC 9 VALUE ZERO.
009490***
009500 01 印刷制御.
009510     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
009520     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
009530     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
009540     03 拡張制御Ｐ.
009550         05 端末制御Ｐ.
009560             07 移動方向Ｐ             PIC X(1) VALUE SPACE.
009570             07 移動行数Ｐ             PIC 9(3) VALUE ZERO.
009580         05 詳細制御Ｐ                 PIC X(2) VALUE SPACE.
009590     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
009600     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
009610*
009620 01 計算機西暦年Ｗ                     PIC 9(2) VALUE ZERO.
009630* 日付ＷＯＲＫ
009640 01 和暦終了年Ｗ                       PIC 9(4) VALUE ZERO.
009650 01 計算機西暦.
009660    03 計算機西暦年                    PIC 9(4) VALUE ZERO.
009670    03 計算機西暦月日                  PIC 9(4) VALUE ZERO.
009680 01 計算機西暦Ｒ REDEFINES 計算機西暦.
009690    03 計算機世紀                      PIC 9(2).
009700    03 計算機日付                      PIC 9(6).
009710    03 計算機日付Ｒ REDEFINES 計算機日付.
009720       05 計算機年月                   PIC 9(4).
009730       05 計算機年月Ｒ REDEFINES 計算機年月.
009740         07 計算機年                   PIC 9(2).
009750         07 計算機月                   PIC 9(2).
009760       05 計算機日                     PIC 9(2).
009770*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
009780******************************************************************
009790*                          連結項目                              *
009800******************************************************************
009810*
009820**  画面入力データ
009830 01 連入－入力データ委任印刷 IS EXTERNAL.
009840    03 連入－委任印刷                     PIC 9.
014620*
       01 連入－入力データ電話印刷 IS EXTERNAL.
          03 連入－電話印刷                     PIC 9.
009190*
       01 連入－プレビュー IS EXTERNAL.
          03 連入－プレビュー区分          PIC 9.
009860*
009870************
009880* 印刷キー *
009890************
009900*
009910 01 連レ印－対象データ IS EXTERNAL.
009920    03 連レ印－施術年月日.
009930       05 連レ印－施術和暦                  PIC 9(1).
009940       05 連レ印－施術年                    PIC 9(2).
009950       05 連レ印－施術月                    PIC 9(2).
009960    03 連レ印－患者コード.
009970       05 連レ印－患者番号                  PIC 9(6).
009980       05 連レ印－枝番                      PIC X(1).
009990    03 連レ印－保険種別                     PIC 9(2).
010000    03 連レ印－保険者番号                   PIC X(10).
010010    03 連レ印－公費種別                     PIC 9(2).
010020    03 連レ印－費用負担者番号               PIC X(10).
010030    03 連レ印－助成種別                     PIC 9(2).
010040    03 連レ印－費用負担者番号助成           PIC X(10).
010050    03 連レ印－患者カナ                     PIC X(20).
010060    03 連レ印－本人家族区分                 PIC 9(1).
013490*
013500 01 連レ－キー IS EXTERNAL.
013510    03 連レ－保険種別                  PIC 9(2).
013520************************
013530** ３カ月長期判定
013540************************
013550 01 連期間－キー IS EXTERNAL.
013560    03 連期間－施術年月.
013570       05 連期間－施術和暦               PIC 9.
013580       05 連期間－施術年                 PIC 9(2).
013590       05 連期間－施術月                 PIC 9(2).
013600    03  連期間－患者コード.
013610       05 連期間－患者番号               PIC 9(6).
013620       05 連期間－枝番                   PIC X.
013630    03 連期間－対象フラグ                PIC X(3).
013640    03 連期間－期間月Ｗ.
013650       05 連期間－期間Ｗ                 PIC 9(2) OCCURS 9.
013660*
013670************************
013680* 長期理由文セット     *
013690************************
013700 01 連長文－キー IS EXTERNAL.
013710    03 連長文－施術年月.
013720       05 連長文－施術和暦               PIC 9.
013730       05 連長文－施術年                 PIC 9(2).
013740       05 連長文－施術月                 PIC 9(2).
013750    03  連長文－患者コード.
013760       05 連長文－患者番号               PIC 9(6).
013770       05 連長文－枝番                   PIC X.
013780    03 連長文－文桁数                    PIC 9(2).
013790    03 連長文－理由文                    PIC N(63) OCCURS 15.
013800*
016640* 負担率取得用14/10～
016650 01 連率－負担率取得キー IS EXTERNAL.
016660    03 連率－施術和暦年月.
016670       05 連率－施術和暦               PIC 9.
016680       05 連率－施術年月.
016690          07 連率－施術年              PIC 9(2).
016700          07 連率－施術月              PIC 9(2).
016710    03 連率－患者コード.
016720       05 連率－患者番号               PIC 9(6).
016730       05 連率－枝番                   PIC X.
016740    03 連率－実際負担率                PIC 9(3).
016750    03 連率－実際本体負担率            PIC 9(3).
016760    03 連率－健保負担率                PIC 9(3).
016770    03 連率－２７老負担率              PIC 9(3).
016780    03 連率－助成負担率                PIC 9(3).
016790    03 連率－特別用負担率              PIC 9(3).
016800*
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
013702*************
013703* 助成名称
013704*************
013705 01 連助成名称－キー IS EXTERNAL.
013706    03 連助成名称－助成種別             PIC 9(2).
013707    03 連助成名称－費用負担者番号助成   PIC X(10).
013708*   / OUT /
013709    03 連助成名称－名称集団.
013710       05 連助成名称－１文字            PIC N.
013711       05 連助成名称－略称              PIC N(4).
013712       05 連助成名称－正式名称          PIC N(10).
013713*
013810******************************************************************
013820*                      PROCEDURE  DIVISION                       *
013830******************************************************************
013840 PROCEDURE               DIVISION.
013850************
013860*           *
013870* 初期処理   *
013880*           *
013890************
002570     PERFORM プリンタファイル作成.
013900     PERFORM 初期化.
013910************
013920*           *
013930* 主処理     *
013940*           *
013950************
013960* 印刷
013970     PERFORM 連結項目待避.
013980     PERFORM 印刷セット.
013990     PERFORM 印刷処理.
014000************
014010*           *
014020* 終了処理   *
014030*           *
014040************
014050     PERFORM 受診者印刷区分更新.
014060     PERFORM 終了処理.
014070     MOVE ZERO  TO PROGRAM-STATUS.
014080     EXIT PROGRAM.
014090*
014100*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YHP6425"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分  TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
014110*================================================================*
014120 初期化 SECTION.
014130*================================================================*
014140*
014150     PERFORM ファイルオープン.
014160*    /* 現在日付取得 */
014170     ACCEPT 計算機日付 FROM DATE.
014180*    /* 1980～2079年の間で設定 */
014190     IF 計算機年 > 80
014200         MOVE 19 TO 計算機世紀
014210     ELSE
014220         MOVE 20 TO 計算機世紀
014230     END-IF.
014240     PERFORM カレント元号取得.
014250     PERFORM 和暦終了年取得.
014260     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
014270*================================================================*
014280 ファイルオープン SECTION.
014290*
014330     OPEN INPUT   市町村マスタ
014340         MOVE NC"市町村" TO ファイル名.
014350     OPEN INPUT   元号マスタ
014360         MOVE NC"元号" TO ファイル名.
014370         PERFORM オープンチェック.
014380     OPEN INPUT   名称マスタ
014390         MOVE NC"名称" TO ファイル名.
014400         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
014440     OPEN INPUT   制御情報マスタ
014450         MOVE NC"制御情報" TO ファイル名.
014460         PERFORM オープンチェック.
014470     OPEN INPUT   施術所情報マスタ
014480         MOVE NC"施情" TO ファイル名.
014490         PERFORM オープンチェック.
014500     OPEN INPUT   請求先マスタ
014510         MOVE NC"請先" TO ファイル名.
014520         PERFORM オープンチェック.
014530     OPEN INPUT   経過マスタ
014540         MOVE NC"経過" TO ファイル名.
014550         PERFORM オープンチェック.
014560     OPEN INPUT   施術記録Ｆ.
014570         MOVE NC"施記Ｆ" TO ファイル名.
014580         PERFORM オープンチェック.
014590     OPEN INPUT   負傷データＦ.
014600         MOVE NC"負傷" TO ファイル名.
014610         PERFORM オープンチェック.
014620     OPEN INPUT   負傷原因Ｆ.
014630         MOVE NC"負傷原因" TO ファイル名.
014640         PERFORM オープンチェック.
014650     OPEN INPUT  ＩＤ管理マスタ.
014660         MOVE NC"ＩＤ" TO ファイル名.
014670         PERFORM オープンチェック.
014680     OPEN I-O   受診者情報Ｆ.
014690         MOVE NC"受情" TO ファイル名.
014700         PERFORM オープンチェック.
014710     OPEN INPUT 料金マスタ.
014720         MOVE NC"料金" TO ファイル名.
014730         PERFORM オープンチェック.
014740     OPEN INPUT   会情報マスタ.
014750         MOVE NC"会情" TO ファイル名.
014760         PERFORM オープンチェック.
015560     OPEN INPUT   受診者情報２Ｆ.
015570         MOVE NC"受診者情報２Ｆ" TO ファイル名.
015580         PERFORM オープンチェック.
014770     OPEN INPUT   作業ファイル２.
014780         MOVE NC"作２" TO ファイル名.
014790         PERFORM オープンチェック.
014800     OPEN I-O   印刷ファイル
014810         PERFORM エラー処理Ｐ.
014820*================================================================*
014830 オープンチェック SECTION.
014840*
014850     IF 状態キー  NOT =  "00"
014860         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
014870         DISPLAY NC"状態キー：" 状態キー         UPON CONS
014880         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
014890                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
014900         ACCEPT  キー入力 FROM CONS
014910         PERFORM ファイル閉鎖
014920         EXIT PROGRAM.
014930*================================================================*
014940 カレント元号取得 SECTION.
014950*
014960     MOVE ZEROS TO 制－制御区分.
014970     READ 制御情報マスタ
014980     NOT INVALID KEY
014990         MOVE 制－カレント元号         TO カレント元号Ｗ
015000         MOVE 制－レセ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
015010         MOVE 制－レセ長期理由印刷区分 TO 長期理由印刷区分Ｗ
015020         MOVE 制－レセプト日付区分     TO レセプト日付区分Ｗ
015030         MOVE 制－レセプト患者日付区分 TO レセプト患者日付区分Ｗ
017300         MOVE 制－助成レセ             TO 助成レセＷ
015040     END-READ.
015050*
015060*================================================================*
015070 和暦終了年取得 SECTION.
015080*
015090*     DISPLAY NC"カレント元号Ｗ"  カレント元号Ｗ UPON MSGBOX.
015100     MOVE カレント元号Ｗ TO 元－元号区分.
015110     READ 元号マスタ
015120     INVALID KEY
015130         DISPLAY NC"指定和暦が登録されていません" UPON CONS
015140         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
015150                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015160         ACCEPT  キー入力 FROM CONS
015170         PERFORM 終了処理
015180         EXIT PROGRAM
015190     NOT INVALID KEY
015200         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
015210         MOVE 前和暦Ｗ TO 元－元号区分
015220         READ 元号マスタ
015230         INVALID KEY
015240             DISPLAY NC"指定和暦が登録されていません" UPON CONS
015250             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
015260                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015270             ACCEPT  キー入力 FROM CONS
015280             PERFORM 終了処理
015290             EXIT PROGRAM
015300         NOT INVALID KEY
015310             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
015320         END-READ
015330     END-READ.
015340*
015350*================================================================*
015360 連結項目待避 SECTION.
015370*
015380     MOVE 連レ印－施術和暦           TO 施術和暦ＷＲ.
015390     MOVE 連レ印－施術年             TO 施術年ＷＲ.
015400     MOVE 連レ印－施術月             TO 施術月ＷＲ.
015410     MOVE 連レ印－保険種別           TO 保険種別ＷＲ.
015420     MOVE 連レ印－保険者番号         TO 保険者番号ＷＲ.
015430     MOVE 連レ印－公費種別           TO 公費種別ＷＲ.
015440     MOVE 連レ印－費用負担者番号     TO 費用負担者番号ＷＲ.
015450     MOVE 連レ印－助成種別           TO 助成種別ＷＲ.
015460     MOVE 連レ印－費用負担者番号助成 TO 費用負担者番号助成ＷＲ.
015470     MOVE 連レ印－本人家族区分       TO 本人家族区分ＷＲ.
015480     MOVE 連レ印－患者カナ           TO 患者カナＷＲ.
015490     MOVE 連レ印－患者番号           TO 患者番号ＷＲ.
015500     MOVE 連レ印－枝番               TO 枝番ＷＲ.
015510*================================================================*
015520 印刷セット SECTION.
015530*
015540     PERFORM 項目初期化.
           PERFORM 基本情報取得.
015550     PERFORM 施術所情報取得.
015560     PERFORM 請求先情報取得.
015570     PERFORM 受診者情報取得.
015580     PERFORM 負傷データ取得.
015590     PERFORM 料金情報取得.
015600     PERFORM 施術記録取得.
015610*******     PERFORM 長期判定取得.
015620*******     PERFORM 初検日以前のデータ判定.
015630     PERFORM 初検加算時刻取得.
015640*
015650     PERFORM 給付割合取得.
015660     PERFORM 負担割合取得.
015670     PERFORM 助成印取得.
015680     PERFORM 基本料取得.
015690     PERFORM 施術西暦年取得.
015700     PERFORM レセプト並び順取得.
015710     PERFORM 委任年月日取得.
           PERFORM 施術日取得.
016791*-----------------------------------------------*
016800     IF ( 負傷原因印刷区分Ｗ  NOT = 1 ) AND ( レセ負傷原因印刷区分Ｗ NOT = 1 )
016813        IF ( 負傷原因印刷区分Ｗ = 3 OR 4 )
016815           PERFORM 負傷原因印刷対象判定処理
016817        ELSE
016820           PERFORM 負傷原因取得
016821        END-IF
016830     END-IF.
016831*-----------------------------------------------*
015940     IF ( 長期理由印刷区分Ｗ NOT = 1 ) AND
015950        ( 連期間－対象フラグ = "YES" )
               MOVE 長期理由印刷区分Ｗ TO 連摘文－長期区分
015820     END-IF.
015830**
015840********************
015850* 受診者情報セット *
015860********************
015870*
015880*     MOVE 施術和暦Ｗ          TO 施術和暦.
015890     MOVE 施術年Ｗ            TO 施術年.
015900     MOVE 施術月Ｗ            TO 施術月.
016130*
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
                       MOVE "・"   TO 記号番号Ｗ(カウンタ + 1:2)
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
           MOVE 国保チェックＷ   TO 国保チェック.
           MOVE 協会チェックＷ   TO 協会チェック.
           MOVE 組合チェックＷ   TO 組合チェック.
           MOVE 共済チェックＷ   TO 共済チェック.
           MOVE 後期チェックＷ   TO 後期チェック.
           MOVE 退職チェックＷ   TO 退職チェック.
           MOVE 本人チェックＷ   TO 本人チェック.
           MOVE 家族チェックＷ   TO 家族チェック.
           MOVE 単独チェックＷ   TO 単独チェック.
           MOVE ２併チェックＷ   TO ２併チェック.
           MOVE 高一チェックＷ   TO 高一チェック.
           MOVE 高７チェックＷ   TO 高７チェック.
           MOVE ６歳チェックＷ   TO ６歳チェック.
           MOVE ７割チェックＷ   TO ７割チェック.
           MOVE ８割チェックＷ   TO ８割チェック.
           MOVE ９割チェックＷ   TO ９割チェック.
           MOVE １０割チェックＷ TO １０割チェック.
      */元号修正/↓↓↓20190408
037370     IF 施術和暦Ｗ > 4
              MOVE 施術和暦Ｗ         TO 元－元号区分
037380        READ 元号マスタ
037390        NOT INVALID KEY
037400            MOVE 元－元号名称   TO 施術和暦
037410        END-READ
              MOVE "===="             TO 施術和暦訂正
           END-IF.
      */元号修正/↑↑↑20190408
016140     IF  印刷市町村番号Ｗ(1:2) = "99"
016150         MOVE SPACE              TO 公費負担者番号
016160     ELSE
016170         MOVE 印刷市町村番号Ｗ   TO 公費負担者番号
016180     END-IF.
016190*
016200     IF ( 印刷受給者番号Ｗ(1:1) = "*"  ) OR
016210        ( 印刷受給者番号Ｗ(1:2) = "＊" )
016220        MOVE  SPACE              TO 受給者番号
016230     ELSE
      */受給者番号が８文字以上の場合枠を無視して印刷する/110425
               IF 印刷受給者番号２Ｗ = SPACE
016830             MOVE 印刷受給者番号Ｗ TO 受給者番号
               ELSE
                   MOVE 受給者番号Ｗ     TO 受給者番号２
               END-IF
016250     END-IF.
016260*
016270     MOVE 印刷保険者番号Ｗ    TO 保険者番号.
016280*     MOVE 請求先名称Ｗ        TO 保険者名称.
016290     MOVE 印刷請求先名称１Ｗ  TO 保険者名称１.
016300     MOVE 印刷請求先名称２Ｗ  TO 保険者名称２.
016310*     MOVE 保険種別名称Ｗ      TO 保険種別.
016310*     MOVE 保険種別名称２Ｗ    TO 保険種別２.
016320*     MOVE 被保険者カナＷ      TO 被保険者カナ.
016330*     MOVE 被保険者性別Ｗ      TO 被保険者性別.
016340     MOVE 被保険者氏名Ｗ      TO 被保険者氏名.
      */ 郵便番号・電話番号追加 /42505
           IF (施術和暦年月ＷＲ >= 42505) AND (連入－電話印刷 = 1)
              IF (受－柔整郵便電話番号印刷 = 0 OR 2) AND
                 ((郵便番号１Ｗ NOT = SPACE) OR (郵便番号２Ｗ NOT = SPACE))
017280           MOVE "〒"          TO 郵便
017260           MOVE 郵便番号１Ｗ  TO 郵便番号１
017270           MOVE 郵便番号２Ｗ  TO 郵便番号２
017280           MOVE "-"           TO 郵便番号区切
              END-IF
              IF 受－柔整郵便電話番号印刷 = 0 OR 3
017260           MOVE 電話番号Ｗ    TO 電話番号
              END-IF
           END-IF.
016400     MOVE 患者住所１Ｗ        TO 住所１.
016410     MOVE 患者住所２Ｗ        TO 住所２.
016420*     MOVE 患者カナＷ          TO 受給者カナ.
016420     MOVE 患者カナＷ          TO 患者カナ.
016430     MOVE 患者氏名Ｗ          TO 患者氏名.
016440*     MOVE 患者性別Ｗ          TO 患者性別.
016450     MOVE 男チェックＷ        TO 男チェック.
016460     MOVE 女チェックＷ        TO 女チェック.
016470     MOVE 明治チェックＷ      TO 明治チェック.
016480     MOVE 大正チェックＷ      TO 大正チェック.
016490     MOVE 昭和チェックＷ      TO 昭和チェック.
016500     MOVE 平成チェックＷ      TO 平成チェック.
      */元号修正↓↓↓/20190408
           MOVE 令和ＣＭＷ         TO 令和ＣＭ.
023070     MOVE 令和チェックＷ     TO 令和チェック.
017390*     MOVE 元号Ｗ              TO 患者和暦.
      */元号修正↑↑↑/20190408
016510*     MOVE 生年月日固定Ｗ      TO 生年月日固定.
016520*     MOVE 元号Ｗ              TO 受給者元号   元号.
016530*     MOVE 患者年Ｗ            TO 受給者年     患者年.
016540*     MOVE NC"年"              TO 受給者年固定 患者年固定.
016550*     MOVE 患者月Ｗ            TO 受給者月     患者月.
016560*     MOVE NC"月"              TO 受給者月固定 患者月固定.
016570*     MOVE 患者日Ｗ            TO 受給者日     患者日.
016580*     MOVE NC"日"              TO 受給者日固定 患者日固定.
016920     MOVE 患者年Ｗ            TO 患者年.
016940     MOVE 患者月Ｗ            TO 患者月.
016960     MOVE 患者日Ｗ            TO 患者日.
      *
           IF 受２－助成被保険者氏名 NOT = SPACE
016940        MOVE 受２－助成被保険者氏名 TO 被保険者氏名
           END-IF.
016590*
016600*     MOVE 印刷続柄Ｗ          TO 続柄.
016610* 
016620*     MOVE NC"（業務災害・通勤災害又は第三者行為以外の原因による）" 
016630*                              TO 負傷原因０.
016640     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
016650     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
016660     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
016670     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
016680     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
016680     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
016690*
016700*     MOVE 助成印Ｗ            TO 助成印.
016710*
016850********************
016860* 負傷データセット *
016870********************
016880* １部位 *
016890**********
016900     MOVE 負傷名Ｗ(1)       TO 負傷名１.
016910     MOVE 負傷年Ｗ(1)       TO 負傷年１.
016920     MOVE 負傷月Ｗ(1)       TO 負傷月１.
016930     MOVE 負傷日Ｗ(1)       TO 負傷日１.
016940     MOVE 初検年Ｗ(1)       TO 初検年１.
016950     MOVE 初検月Ｗ(1)       TO 初検月１.
016960     MOVE 初検日Ｗ(1)       TO 初検日１.
016970     MOVE 開始年Ｗ(1)       TO 開始年１.
016980     MOVE 開始月Ｗ(1)       TO 開始月１.
016990     MOVE 開始日Ｗ(1)       TO 開始日１.
017000     MOVE 終了年Ｗ(1)       TO 終了年１.
017010     MOVE 終了月Ｗ(1)       TO 終了月１.
017020     MOVE 終了日Ｗ(1)       TO 終了日１.
017030     MOVE 実日数Ｗ(1)       TO 実日数１.
017040     MOVE 治癒チェックＷ(1) TO 治癒チェック１.
017050     MOVE 中止チェックＷ(1) TO 中止チェック１.
017060     MOVE 転医チェックＷ(1) TO 転医チェック１.
017070*
017080*     MOVE 転帰Ｗ(1)         TO 転帰１.
017090*     IF 負傷年Ｗ(1) NOT = ZERO
017100*        MOVE "."            TO 区切１１ 区切１２
017110*     END-IF.
017120*     IF 初検年Ｗ(1) NOT = ZERO
017130*        MOVE "."            TO 区切１３ 区切１４
017140*     END-IF.
017150*     IF 開始年Ｗ(1) NOT = ZERO
017160*        MOVE "."            TO 区切１５ 区切１６
017170*     END-IF.
017180*     IF 終了年Ｗ(1) NOT = ZERO
017190*        MOVE "."            TO 区切１７ 区切１８
017200*     END-IF.
017210**********
017220* ２部位 *
017230**********
017240     MOVE 負傷名Ｗ(2)       TO 負傷名２.
017250     MOVE 負傷年Ｗ(2)       TO 負傷年２.
017260     MOVE 負傷月Ｗ(2)       TO 負傷月２.
017270     MOVE 負傷日Ｗ(2)       TO 負傷日２.
017280     MOVE 初検年Ｗ(2)       TO 初検年２.
017290     MOVE 初検月Ｗ(2)       TO 初検月２.
017300     MOVE 初検日Ｗ(2)       TO 初検日２.
017310     MOVE 開始年Ｗ(2)       TO 開始年２.
017320     MOVE 開始月Ｗ(2)       TO 開始月２.
017330     MOVE 開始日Ｗ(2)       TO 開始日２.
017340     MOVE 終了年Ｗ(2)       TO 終了年２.
017350     MOVE 終了月Ｗ(2)       TO 終了月２.
017360     MOVE 終了日Ｗ(2)       TO 終了日２.
017370     MOVE 実日数Ｗ(2)       TO 実日数２.
017380     MOVE 治癒チェックＷ(2) TO 治癒チェック２.
017390     MOVE 中止チェックＷ(2) TO 中止チェック２.
017400     MOVE 転医チェックＷ(2) TO 転医チェック２.
017410*     MOVE 転帰Ｗ(2)         TO 転帰２.
017420*     IF 負傷年Ｗ(2) NOT = ZERO
017430*        MOVE "."            TO 区切２１ 区切２２
017440*     END-IF.
017450*     IF 初検年Ｗ(2) NOT = ZERO
017460*        MOVE "."            TO 区切２３ 区切２４
017470*     END-IF.
017480*     IF 開始年Ｗ(2) NOT = ZERO
017490*        MOVE "."            TO 区切２５ 区切２６
017500*     END-IF.
017510*     IF 終了年Ｗ(2) NOT = ZERO
017520*        MOVE "."            TO 区切２７ 区切２８
017530*     END-IF.
017540**********
017550* ３部位 *
017560**********
017570     MOVE 負傷名Ｗ(3)       TO 負傷名３.
017580     MOVE 負傷年Ｗ(3)       TO 負傷年３.
017590     MOVE 負傷月Ｗ(3)       TO 負傷月３.
017600     MOVE 負傷日Ｗ(3)       TO 負傷日３.
017610     MOVE 初検年Ｗ(3)       TO 初検年３.
017620     MOVE 初検月Ｗ(3)       TO 初検月３.
017630     MOVE 初検日Ｗ(3)       TO 初検日３.
017640     MOVE 開始年Ｗ(3)       TO 開始年３.
017650     MOVE 開始月Ｗ(3)       TO 開始月３.
017660     MOVE 開始日Ｗ(3)       TO 開始日３.
017670     MOVE 終了年Ｗ(3)       TO 終了年３.
017680     MOVE 終了月Ｗ(3)       TO 終了月３.
017690     MOVE 終了日Ｗ(3)       TO 終了日３.
017700     MOVE 実日数Ｗ(3)       TO 実日数３.
017710     MOVE 治癒チェックＷ(3) TO 治癒チェック３.
017720     MOVE 中止チェックＷ(3) TO 中止チェック３.
017730     MOVE 転医チェックＷ(3) TO 転医チェック３.
017740*     MOVE 転帰Ｗ(3)         TO 転帰３.
017750*     IF 負傷年Ｗ(3) NOT = ZERO
017760*        MOVE "."            TO 区切３１ 区切３２
017770*     END-IF.
017780*     IF 初検年Ｗ(3) NOT = ZERO
017790*        MOVE "."            TO 区切３３ 区切３４
017800*     END-IF.
017810*     IF 開始年Ｗ(3) NOT = ZERO
017820*        MOVE "."            TO 区切３５ 区切３６
017830*     END-IF.
017840*     IF 終了年Ｗ(3) NOT = ZERO
017850*        MOVE "."            TO 区切３７ 区切３８
017860*     END-IF.
017870**********
017880* ４部位 *
017890**********
017900     MOVE 負傷名Ｗ(4)       TO 負傷名４.
017910     MOVE 負傷年Ｗ(4)       TO 負傷年４.
017920     MOVE 負傷月Ｗ(4)       TO 負傷月４.
017930     MOVE 負傷日Ｗ(4)       TO 負傷日４.
017940     MOVE 初検年Ｗ(4)       TO 初検年４.
017950     MOVE 初検月Ｗ(4)       TO 初検月４.
017960     MOVE 初検日Ｗ(4)       TO 初検日４.
017970     MOVE 開始年Ｗ(4)       TO 開始年４.
017980     MOVE 開始月Ｗ(4)       TO 開始月４.
017990     MOVE 開始日Ｗ(4)       TO 開始日４.
018000     MOVE 終了年Ｗ(4)       TO 終了年４.
018010     MOVE 終了月Ｗ(4)       TO 終了月４.
018020     MOVE 終了日Ｗ(4)       TO 終了日４.
018030     MOVE 実日数Ｗ(4)       TO 実日数４.
018040     MOVE 治癒チェックＷ(4) TO 治癒チェック４.
018050     MOVE 中止チェックＷ(4) TO 中止チェック４.
018060     MOVE 転医チェックＷ(4) TO 転医チェック４.
018070*     MOVE 転帰Ｗ(4)         TO 転帰４.
018080*     IF 負傷年Ｗ(4) NOT = ZERO
018090*        MOVE "."            TO 区切４１ 区切４２
018100*     END-IF.
018110*     IF 初検年Ｗ(4) NOT = ZERO
018120*        MOVE "."            TO 区切４３ 区切４４
018130*     END-IF.
018140*     IF 開始年Ｗ(4) NOT = ZERO
018150*        MOVE "."            TO 区切４５ 区切４６
018160*     END-IF.
018170*     IF 終了年Ｗ(4) NOT = ZERO
018180*        MOVE "."            TO 区切４７ 区切４８
018190*     END-IF.
018200**********
018210* ５部位 *
018220**********
018230     MOVE 負傷名Ｗ(5)       TO 負傷名５.
018240     MOVE 負傷年Ｗ(5)       TO 負傷年５.
018250     MOVE 負傷月Ｗ(5)       TO 負傷月５.
018260     MOVE 負傷日Ｗ(5)       TO 負傷日５.
018270     MOVE 初検年Ｗ(5)       TO 初検年５.
018280     MOVE 初検月Ｗ(5)       TO 初検月５.
018290     MOVE 初検日Ｗ(5)       TO 初検日５.
018300     MOVE 開始年Ｗ(5)       TO 開始年５.
018310     MOVE 開始月Ｗ(5)       TO 開始月５.
018320     MOVE 開始日Ｗ(5)       TO 開始日５.
018330     MOVE 終了年Ｗ(5)       TO 終了年５.
018340     MOVE 終了月Ｗ(5)       TO 終了月５.
018350     MOVE 終了日Ｗ(5)       TO 終了日５.
018360     MOVE 実日数Ｗ(5)       TO 実日数５.
018370     MOVE 治癒チェックＷ(5) TO 治癒チェック５.
018380     MOVE 中止チェックＷ(5) TO 中止チェック５.
018390     MOVE 転医チェックＷ(5) TO 転医チェック５.
018400*     MOVE 転帰Ｗ(5)         TO 転帰５.
018410*     IF 負傷年Ｗ(5) NOT = ZERO
018420*        MOVE "."            TO 区切５１ 区切５２
018430*     END-IF.
018440*     IF 初検年Ｗ(5) NOT = ZERO
018450*        MOVE "."            TO 区切５３ 区切５４
018460*     END-IF.
018470*     IF 開始年Ｗ(5) NOT = ZERO
018480*        MOVE "."            TO 区切５５ 区切５６
018490*     END-IF.
018500*     IF 終了年Ｗ(5) NOT = ZERO
018510*        MOVE "."            TO 区切５７ 区切５８
018520*     END-IF.
018530**************
018540* 経過セット *
018550**************
018560     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018570***             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
018580             UNTIL ( 部位ＣＮＴ > 5 )
018590**         MOVE 部位ＣＮＴＷ(部位ＣＮＴ)   TO 経過部位ＣＮＴ(部位ＣＮＴ)
018600**         MOVE 部位区切Ｗ(部位ＣＮＴ)     TO 部位区切(部位ＣＮＴ)
018610         MOVE 印刷経過略称Ｗ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
018620     END-PERFORM.
018630*****************************************
018640*     新規・継続チェックについて        *
018650*   ●新規...初検有り ●継続...初検なし *
018660*****************************************
018670     MOVE 新規チェックＷ    TO 新規チェック.
018680     MOVE 継続チェックＷ    TO 継続チェック.
018690*     MOVE 請求区分Ｗ        TO  請求区分.
018700********************
018710* 料金データセット *
018720********************
018730*    ****************************************************************
018740*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
018750*    ****************************************************************
018760     MOVE 初検料ＷＲ                   TO  初検料.
018770     MOVE 時間外チェックＷ             TO  時間外チェック.
018780     MOVE 休日チェックＷ               TO  休日チェック.
018790     MOVE 深夜チェックＷ               TO  深夜チェック.
018800*     MOVE 初検加算内容Ｗ               TO  初検加算内容.
018810     MOVE 初検加算料ＷＲ               TO  初検加算料.
           MOVE 初検時相談料ＷＲ             TO  初検時相談料.
           IF (時間外チェックＷ NOT = SPACE) OR (深夜チェックＷ NOT = SPACE) OR
              (休日チェックＷ NOT = SPACE)
              MOVE 初検加算時Ｗ                 TO  初検加算時
              MOVE 初検加算区切Ｗ               TO  初検加算区切
              MOVE 初検加算分Ｗ                 TO  初検加算分
           END-IF.
019150*     END-IF.
018820     MOVE 再検料ＷＲ                   TO  再検料.
018830     MOVE 往療距離ＷＲ                 TO  往療距離.
018840     MOVE 往療回数ＷＲ                 TO  往療回数.
018850     MOVE 往療料ＷＲ                   TO  往療料.
018860     MOVE 夜間チェックＷ               TO  夜間チェック.
018870     MOVE 難路チェックＷ               TO  難路チェック.
018880*     MOVE 往療深夜チェックＷ           TO  往療深夜チェック.
018890     MOVE 暴風雨雪チェックＷ           TO  暴風雨雪チェック.
018900     MOVE 往療加算料ＷＲ               TO  往療加算料.
      */金属副子・運動後療の変更・追加/1805
           IF ( 施術和暦年月ＷＲ < 43006 )
018050        MOVE 大チェックＷ              TO  大チェック
018060        MOVE 中チェックＷ              TO  中チェック
018070        MOVE 小チェックＷ              TO  小チェック
           ELSE
              MOVE ALL NC"＝"                TO  金属訂正
           END-IF.
      *     IF ( 施術和暦年月ＷＲ >= 43006 ) AND ( 金属副子加算料ＷＲ NOT = ZERO )
      *        MOVE 金属回数Ｗ                TO  金属回数
      *        MOVE NC"回"                    TO  金属回
      *     END-IF.
018940*     MOVE 金属大Ｗ                     TO  金属大.
018950*     MOVE 金属中Ｗ                     TO  金属中.
018960*     MOVE 金属小Ｗ                     TO  金属小.
018970     MOVE 金属副子加算料ＷＲ           TO  金属副子加算料.
018980     MOVE 施術情報提供料ＷＲ           TO  施術情報提供料.
018990     MOVE 小計Ｗ                       TO  小計.
019000********************
019010* 初回処置料セット *
019020********************
019030     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
019040***             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
019050             UNTIL ( 部位ＣＮＴ > 5 )
019060         MOVE 初回処置料ＷＲ(部位ＣＮＴ) TO 初回処置料(部位ＣＮＴ)
019070     END-PERFORM.
019080     MOVE 初回処置料合計Ｗ         TO 初回処置料合計.
019090*
019100     MOVE 施療料チェックＷ            TO 施療料チェック.
019110     MOVE 整復料チェックＷ            TO 整復料チェック.
019120     MOVE 固定料チェックＷ            TO 固定料チェック.
019130******************
019140* 基本料金セット *
019150******************
019160*     MOVE 冷罨法単価Ｗ                TO  冷罨法単価.
019170*     MOVE 温罨法単価Ｗ                TO  温罨法単価.
019180*     MOVE 電療単価Ｗ                  TO  電療単価.
019190********************
019200* 逓減毎料金セット *
019210********************
019220*    **********
019230*    * １部位 *
019240*    **********
019250     MOVE 後療単価１ＷＲ             TO 後療単価１.
019260     MOVE 後療回数１ＷＲ             TO 後療回数１.
019270     MOVE 後療料１ＷＲ               TO 後療料１.
019280     MOVE 冷罨法回数１ＷＲ           TO 冷罨法回数１.
019290     MOVE 冷罨法料１ＷＲ             TO 冷罨法料１.
019300     MOVE 温罨法回数１ＷＲ           TO 温罨法回数１.
019310     MOVE 温罨法料１ＷＲ             TO 温罨法料１.
019320     MOVE 電療回数１ＷＲ             TO 電療回数１.
019330     MOVE 電療料１ＷＲ               TO 電療料１.
019340     MOVE 小計１ＷＲ                 TO 小計１.
019350     IF 長期逓減率１ＷＲ NOT = ZERO
019360         COMPUTE 長期逓減率１ = 長期逓減率１ＷＲ / 100
019370     END-IF.
019380     MOVE 長期込小計１ＷＲ           TO 長期込小計１.
019390*    **********
019400*    * ２部位 *
019410*    **********
019420     MOVE 後療単価２ＷＲ             TO 後療単価２.
019430     MOVE 後療回数２ＷＲ             TO 後療回数２.
019440     MOVE 後療料２ＷＲ               TO 後療料２.
019450     MOVE 冷罨法回数２ＷＲ           TO 冷罨法回数２.
019460     MOVE 冷罨法料２ＷＲ             TO 冷罨法料２.
019470     MOVE 温罨法回数２ＷＲ           TO 温罨法回数２.
019480     MOVE 温罨法料２ＷＲ             TO 温罨法料２.
019490     MOVE 電療回数２ＷＲ             TO 電療回数２.
019500     MOVE 電療料２ＷＲ               TO 電療料２.
019510     MOVE 小計２ＷＲ                 TO 小計２.
019520     IF 長期逓減率２ＷＲ NOT = ZERO
019530         COMPUTE 長期逓減率２ = 長期逓減率２ＷＲ / 100
019540     END-IF.
019550     MOVE 長期込小計２ＷＲ           TO 長期込小計２.
019560*    ****************
019570*    * ３部位／８割 *
019580*    ****************
019590     MOVE 後療単価３８ＷＲ             TO 後療単価３８.
019600     MOVE 後療回数３８ＷＲ             TO 後療回数３８.
019610     MOVE 後療料３８ＷＲ               TO 後療料３８.
019620     MOVE 冷罨法回数３８ＷＲ           TO 冷罨法回数３８.
019630     MOVE 冷罨法料３８ＷＲ             TO 冷罨法料３８.
019640     MOVE 温罨法回数３８ＷＲ           TO 温罨法回数３８.
019650     MOVE 温罨法料３８ＷＲ             TO 温罨法料３８.
019660     MOVE 電療回数３８ＷＲ             TO 電療回数３８.
019670     MOVE 電療料３８ＷＲ               TO 電療料３８.
019680     MOVE 小計３８ＷＲ                 TO 小計３８.
019690     MOVE 多部位込小計３８ＷＲ         TO 多部位込小計３８.
019700     IF 長期逓減率３８ＷＲ NOT = ZERO
019710         COMPUTE 長期逓減率３８ = 長期逓減率３８ＷＲ / 100
019720     END-IF.
019730     MOVE 長期込小計３８ＷＲ           TO 長期込小計３８.
      */ 逓減率 0.7→0.6 /42505
           IF (施術和暦年月ＷＲ >= 42505)
              MOVE "60"                      TO 逓減３８
              MOVE "0.6"                     TO 多部位３８
      *        MOVE "==="                     TO 逓減訂正３８ 多部位訂正３８
           END-IF.
019740*    ****************
019750*    * ３部位／10割 *
019760*    ****************
019770     MOVE 逓減開始月３０ＷＲ           TO 逓減開始月３０.
019780     MOVE 逓減開始日３０ＷＲ           TO 逓減開始日３０.
019790     MOVE 後療単価３０ＷＲ             TO 後療単価３０.
019800     MOVE 後療回数３０ＷＲ             TO 後療回数３０.
019810     MOVE 後療料３０ＷＲ               TO 後療料３０.
019820     MOVE 冷罨法回数３０ＷＲ           TO 冷罨法回数３０.
019830     MOVE 冷罨法料３０ＷＲ             TO 冷罨法料３０.
019840     MOVE 温罨法回数３０ＷＲ           TO 温罨法回数３０.
019850     MOVE 温罨法料３０ＷＲ             TO 温罨法料３０.
019860     MOVE 電療回数３０ＷＲ             TO 電療回数３０.
019870     MOVE 電療料３０ＷＲ               TO 電療料３０.
019880     MOVE 小計３０ＷＲ                 TO 小計３０.
019890     IF 長期逓減率３０ＷＲ NOT = ZERO
019900         COMPUTE 長期逓減率３０ = 長期逓減率３０ＷＲ / 100
019910     END-IF.
019920     MOVE 長期込小計３０ＷＲ           TO 長期込小計３０.
019930*    ****************
019940*    * ４部位／５割 *
019950*    ****************
019960*     MOVE 後療単価４５ＷＲ             TO 後療単価４５.
019970*     MOVE 後療回数４５ＷＲ             TO 後療回数４５.
019980*     MOVE 後療料４５ＷＲ               TO 後療料４５.
019990*     MOVE 冷罨法回数４５ＷＲ           TO 冷罨法回数４５.
020000*     MOVE 冷罨法料４５ＷＲ             TO 冷罨法料４５.
020010*     MOVE 温罨法回数４５ＷＲ           TO 温罨法回数４５.
020020*     MOVE 温罨法料４５ＷＲ             TO 温罨法料４５.
020030*     MOVE 電療回数４５ＷＲ             TO 電療回数４５.
020040*     MOVE 電療料４５ＷＲ               TO 電療料４５.
020050*     MOVE 小計４５ＷＲ                 TO 小計４５.
020060*     MOVE 多部位込小計４５ＷＲ         TO 多部位込小計４５.
020070*     IF 長期逓減率４５ＷＲ NOT = ZERO
020080*         COMPUTE 長期逓減率４５ = 長期逓減率４５ＷＲ / 100
020090*     END-IF.
020100*     MOVE 長期込小計４５ＷＲ           TO 長期込小計４５.
020110*    ****************
020120*    * ４部位／８割 *
020130*    ****************
020140     MOVE 逓減開始月４８ＷＲ           TO 逓減開始月４８.
020150     MOVE 逓減開始日４８ＷＲ           TO 逓減開始日４８.
020160     MOVE 後療単価４８ＷＲ             TO 後療単価４８.
020170     MOVE 後療回数４８ＷＲ             TO 後療回数４８.
020180     MOVE 後療料４８ＷＲ               TO 後療料４８.
020190     MOVE 冷罨法回数４８ＷＲ           TO 冷罨法回数４８.
020200     MOVE 冷罨法料４８ＷＲ             TO 冷罨法料４８.
020210     MOVE 温罨法回数４８ＷＲ           TO 温罨法回数４８.
020220     MOVE 温罨法料４８ＷＲ             TO 温罨法料４８.
020230     MOVE 電療回数４８ＷＲ             TO 電療回数４８.
020240     MOVE 電療料４８ＷＲ               TO 電療料４８.
020250     MOVE 小計４８ＷＲ                 TO 小計４８.
020260     MOVE 多部位込小計４８ＷＲ         TO 多部位込小計４８.
020270     IF 長期逓減率４８ＷＲ NOT = ZERO
020280         COMPUTE 長期逓減率４８ = 長期逓減率４８ＷＲ / 100
020290     END-IF.
020300     MOVE 長期込小計４８ＷＲ           TO 長期込小計４８.
      */ 逓減率 0.7→0.6 /42505
           IF (施術和暦年月ＷＲ >= 42505)
              MOVE "60"                      TO 逓減４８
              MOVE "0.6"                     TO 多部位４８
      *        MOVE "==="                     TO 逓減訂正４８ 多部位訂正４８
           END-IF.
020310*    ****************
020320*    * ４部位／10割 *
020330*    ****************
020340     MOVE 逓減開始月４０ＷＲ           TO 逓減開始月４０.
020350     MOVE 逓減開始日４０ＷＲ           TO 逓減開始日４０.
020360     MOVE 後療単価４０ＷＲ             TO 後療単価４０.
020370     MOVE 後療回数４０ＷＲ             TO 後療回数４０.
020380     MOVE 後療料４０ＷＲ               TO 後療料４０.
020390     MOVE 冷罨法回数４０ＷＲ           TO 冷罨法回数４０.
020400     MOVE 冷罨法料４０ＷＲ             TO 冷罨法料４０.
020410     MOVE 温罨法回数４０ＷＲ           TO 温罨法回数４０.
020420     MOVE 温罨法料４０ＷＲ             TO 温罨法料４０.
020430     MOVE 電療回数４０ＷＲ             TO 電療回数４０.
020440     MOVE 電療料４０ＷＲ               TO 電療料４０.
020450     MOVE 小計４０ＷＲ                 TO 小計４０.
020460     IF 長期逓減率４０ＷＲ NOT = ZERO
020470         COMPUTE 長期逓減率４０ = 長期逓減率４０ＷＲ / 100
020480     END-IF.
020490     MOVE 長期込小計４０ＷＲ           TO 長期込小計４０.
020500*
020510*↓***********************************************************************
020520* 帳票定義の変更により、５部位の印字は必要ない。
020530*------------------------------------------------------------------------*
020540*    *****************
020550*    * ５部位／2.5割 *
020560*    *****************
020570*     MOVE 後療単価５２ＷＲ             TO 後療単価５２.
020580*     MOVE 後療回数５２ＷＲ             TO 後療回数５２.
020590*     MOVE 後療料５２ＷＲ               TO 後療料５２.
020600*     MOVE 冷罨法回数５２ＷＲ           TO 冷罨法回数５２.
020610*     MOVE 冷罨法料５２ＷＲ             TO 冷罨法料５２.
020620*     MOVE 温罨法回数５２ＷＲ           TO 温罨法回数５２.
020630*     MOVE 温罨法料５２ＷＲ             TO 温罨法料５２.
020640*     MOVE 電療回数５２ＷＲ             TO 電療回数５２.
020650*     MOVE 電療料５２ＷＲ               TO 電療料５２.
020660*     MOVE 小計５２ＷＲ                 TO 小計５２.
020670*     MOVE 多部位込小計５２ＷＲ         TO 多部位込小計５２.
020680*     IF 長期逓減率５２ＷＲ NOT = ZERO
020690*         COMPUTE 長期逓減率５２ = 長期逓減率５２ＷＲ / 100
020700*     END-IF.
020710*     MOVE 長期込小計５２ＷＲ           TO 長期込小計５２.
020720*    ****************
020730*    * ５部位／５割 *
020740*    ****************
020750*     MOVE 逓減開始月５５ＷＲ           TO 逓減開始月５５.
020760*     MOVE 逓減開始日５５ＷＲ           TO 逓減開始日５５.
020770*     MOVE 後療単価５５ＷＲ             TO 後療単価５５.
020780*     MOVE 後療回数５５ＷＲ             TO 後療回数５５.
020790*     MOVE 後療料５５ＷＲ               TO 後療料５５.
020800*     MOVE 冷罨法回数５５ＷＲ           TO 冷罨法回数５５.
020810*     MOVE 冷罨法料５５ＷＲ             TO 冷罨法料５５.
020820*     MOVE 温罨法回数５５ＷＲ           TO 温罨法回数５５.
020830*     MOVE 温罨法料５５ＷＲ             TO 温罨法料５５.
020840*     MOVE 電療回数５５ＷＲ             TO 電療回数５５.
020850*     MOVE 電療料５５ＷＲ               TO 電療料５５.
020860*     MOVE 小計５５ＷＲ                 TO 小計５５.
020870*     MOVE 多部位込小計５５ＷＲ         TO 多部位込小計５５.
020880*     IF 長期逓減率５５ＷＲ NOT = ZERO
020890*         COMPUTE 長期逓減率５５ = 長期逓減率５５ＷＲ / 100
020900*     END-IF.
020910*     MOVE 長期込小計５５ＷＲ           TO 長期込小計５５.
020920*    ****************
020930*    * ５部位／８割 *
020940*    ****************
021220     MOVE SPACE TO 部位５Ｗ.
021230     IF 小計５８ＷＲ NOT = ZERO
      */日付
021560        MOVE 逓減開始月５８ＷＲ           TO 逓減開始月５Ｗ
              MOVE "月"                         TO 月ＣＭ
021570        MOVE 逓減開始日５８ＷＲ           TO 逓減開始日５Ｗ
              MOVE "日"                         TO 日ＣＭ
              MOVE "("                          TO 括弧１Ｗ
      */後療料
              IF 後療料５８ＷＲ NOT = ZERO
                  MOVE "("                      TO 括弧２Ｗ
021580            MOVE 後療単価５８ＷＲ         TO 後療単価５Ｗ
                  MOVE "x"                      TO 乗算記号１Ｗ
021590            MOVE 後療回数５８ＷＲ         TO 後療回数５Ｗ
                  MOVE "="                      TO イコール１Ｗ
021600            MOVE 後療料５８ＷＲ           TO 後療料５Ｗ
                  MOVE ")"                      TO 括弧３Ｗ
              END-IF
      */冷罨法
              IF 冷罨法料５８ＷＲ NOT = ZERO
                  MOVE "+"                      TO 加算記号１Ｗ
                  MOVE "("                      TO 括弧４Ｗ
                  COMPUTE 冷罨法単価５Ｗ        =  冷罨法料５８ＷＲ / 冷罨法回数５８ＷＲ
                  MOVE "x"                      TO 乗算記号２Ｗ
021610            MOVE 冷罨法回数５８ＷＲ       TO 冷罨法回数５Ｗ
                  MOVE "="                      TO イコール２Ｗ
021620            MOVE 冷罨法料５８ＷＲ         TO 冷罨法料５Ｗ
                  MOVE ")"                      TO 括弧５Ｗ
              END-IF
      */温罨法
              IF 温罨法料５８ＷＲ NOT = ZERO
                  MOVE "+"                      TO 加算記号２Ｗ
                  MOVE "("                      TO 括弧６Ｗ
                  COMPUTE 温罨法単価５Ｗ        =  温罨法料５８ＷＲ / 温罨法回数５８ＷＲ
                  MOVE "x"                      TO 乗算記号３Ｗ
021630            MOVE 温罨法回数５８ＷＲ       TO 温罨法回数５Ｗ
                  MOVE "="                      TO イコール３Ｗ
021640            MOVE 温罨法料５８ＷＲ         TO 温罨法料５Ｗ
                  MOVE ")"                      TO 括弧７Ｗ
              END-IF
      */電療料
              IF 電療料５８ＷＲ NOT = ZERO
                  MOVE "+"                      TO 加算記号３Ｗ
                  MOVE "("                      TO 括弧８Ｗ
                  COMPUTE 電療単価５Ｗ          =  電療料５８ＷＲ / 電療回数５８ＷＲ
                  MOVE "x"                      TO 乗算記号４Ｗ
021650            MOVE 電療回数５８ＷＲ         TO 電療回数５Ｗ
                  MOVE "="                      TO イコール４Ｗ
021660            MOVE 電療料５８ＷＲ           TO 電療料５Ｗ
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
021680        IF 長期逓減率５８ＷＲ NOT = ZERO
                 MOVE "x"                       TO 乗算記号６Ｗ
021690           COMPUTE 長期逓減率５Ｗ = 長期逓減率５８ＷＲ / 100
021700        END-IF
      */合計
              MOVE "="                          TO イコール５Ｗ
021710        MOVE 長期込小計５８ＷＲ           TO 長期込小計５Ｗ
021720        MOVE 部位５Ｗ                     TO 部位５８
021490     END-IF.
021120*    ****************
021130*    * ５部位／10割 *
021140*    ****************
021530     MOVE SPACE TO 部位５Ｗ.
021540     IF 小計５０ＷＲ NOT = ZERO
      */日付
021560        MOVE 逓減開始月５０ＷＲ           TO 逓減開始月５Ｗ
              MOVE "月"                         TO 月ＣＭ
021570        MOVE 逓減開始日５０ＷＲ           TO 逓減開始日５Ｗ
              MOVE "日"                         TO 日ＣＭ
              MOVE "("                          TO 括弧１Ｗ
      */後療料
              IF 後療料５０ＷＲ NOT = ZERO
                  MOVE "("                      TO 括弧２Ｗ
021580            MOVE 後療単価５０ＷＲ         TO 後療単価５Ｗ
                  MOVE "x"                      TO 乗算記号１Ｗ
021590            MOVE 後療回数５０ＷＲ         TO 後療回数５Ｗ
                  MOVE "="                      TO イコール１Ｗ
021600            MOVE 後療料５０ＷＲ           TO 後療料５Ｗ
                  MOVE ")"                      TO 括弧３Ｗ
              END-IF
      */冷罨法
              IF 冷罨法料５０ＷＲ NOT = ZERO
                  MOVE "+"                      TO 加算記号１Ｗ
                  MOVE "("                      TO 括弧４Ｗ
                  COMPUTE 冷罨法単価５Ｗ        =  冷罨法料５０ＷＲ / 冷罨法回数５０ＷＲ
                  MOVE "x"                      TO 乗算記号２Ｗ
021610            MOVE 冷罨法回数５０ＷＲ       TO 冷罨法回数５Ｗ
                  MOVE "="                      TO イコール２Ｗ
021620            MOVE 冷罨法料５０ＷＲ         TO 冷罨法料５Ｗ
                  MOVE ")"                      TO 括弧５Ｗ
              END-IF
      */温罨法
              IF 温罨法料５０ＷＲ NOT = ZERO
                  MOVE "+"                      TO 加算記号２Ｗ
                  MOVE "("                      TO 括弧６Ｗ
                  COMPUTE 温罨法単価５Ｗ        =  温罨法料５０ＷＲ / 温罨法回数５０ＷＲ
                  MOVE "x"                      TO 乗算記号３Ｗ
021630            MOVE 温罨法回数５０ＷＲ       TO 温罨法回数５Ｗ
                  MOVE "="                      TO イコール３Ｗ
021640            MOVE 温罨法料５０ＷＲ         TO 温罨法料５Ｗ
                  MOVE ")"                      TO 括弧７Ｗ
              END-IF
      */電療料
              IF 電療料５０ＷＲ NOT = ZERO
                  MOVE "+"                      TO 加算記号３Ｗ
                  MOVE "("                      TO 括弧８Ｗ
                  COMPUTE 電療単価５Ｗ          =  電療料５０ＷＲ / 電療回数５０ＷＲ
                  MOVE "x"                      TO 乗算記号４Ｗ
021650            MOVE 電療回数５０ＷＲ         TO 電療回数５Ｗ
                  MOVE "="                      TO イコール４Ｗ
021660            MOVE 電療料５０ＷＲ           TO 電療料５Ｗ
                  MOVE ")"                      TO 括弧９Ｗ
              END-IF
      *
              MOVE ")"                          TO 括弧１０Ｗ
      */多部位
      *        乗算記号５Ｗ 多部位率５Ｗ
      */長期
021680        IF 長期逓減率５０ＷＲ NOT = ZERO
                 MOVE "x"                       TO 乗算記号６Ｗ
021690           COMPUTE 長期逓減率５Ｗ = 長期逓減率５０ＷＲ / 100
021700        END-IF
      */合計
              MOVE "="                          TO イコール５Ｗ
021710        MOVE 長期込小計５０ＷＲ           TO 長期込小計５Ｗ
021720        MOVE 部位５Ｗ                     TO 部位５０
021730     END-IF.
021320*
021330*------------------------------------------------------------------------------------*
021340** 特別（５部位目の明細がある場合は、適用欄にコメントを印字する）
021350*     IF ( 小計５５ＷＲ NOT = ZERO ) OR
021360*        ( 小計５８ＷＲ NOT = ZERO ) OR
021370*        ( 小計５０ＷＲ NOT = ZERO )
021380*        MOVE  NC"※５部位目請求あり" TO 部位５適用
021390*     END-IF.
021400*------------------------------------------------------------------------------------*
021410*
021420     MOVE 適用１Ｗ                     TO 適用１.
021430     MOVE 適用２Ｗ                     TO 適用２.
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
              MOVE 37           TO 連金運－会コード
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
021440     MOVE レセ－合計                   TO 合計.
021450*     MOVE レセ－一部負担金             TO 一部負担金.
021460     MOVE レセ－請求金額               TO 請求金額.
021470     MOVE レセ－受給者負担額           TO 受給者負担額.
021480     MOVE レセ－助成請求金額           TO 助成請求額.
           MOVE NC"割相当額"                 TO 割相当額ＣＭ.
           MOVE NC"受給者負担金"             TO 受給者負担金ＣＭ.
           MOVE NC"請求金額"                 TO 請求金額ＣＭ.
021490*
021500**********************
021510* 給付割合セット *
021520**********************
021530     MOVE 給付割合Ｗ             TO 給付割合.
021540*     MOVE 負担割合Ｗ             TO 負担割合.
021550*     MOVE 割合固定Ｗ             TO 割合固定.
      *     MOVE 後印字Ｗ               TO 後印字.
      *
      */都道府県別処理
           EVALUATE TRUE
      */東京都
           WHEN 市町村番号Ｗ(3:2) = "13"
021370         MOVE レセ－受給者負担額               TO 受給者負担額２
021380         MOVE レセ－助成請求金額               TO 助成請求額２
               MOVE "一部負担金相当額（医療助成費）" TO 一部負担金ＣＭ
               MOVE "請求金額（医療助成費）"         TO 請求金額ＣＭ２
               MOVE "円"                             TO 一部負担金円ＣＭ 請求金額円ＣＭ
               MOVE NC"一部負担金"                   TO 東京用一部負担金ＣＭ
               MOVE NC"（医療保険）"                 TO 東京用一部負担金ＣＭ２ 東京用受給者負担金ＣＭ２
               MOVE NC"請求金額"                     TO 東京用受給者負担金ＣＭ
               MOVE SPACE                            TO 給付割合 割相当額ＣＭ 請求金額ＣＭ 受給者負担金ＣＭ
               MOVE ZERO                             TO 助成請求額
021450         MOVE レセ－一部負担金                 TO 請求金額
021460         MOVE レセ－請求金額                   TO 受給者負担額
      */助成印
017080         MOVE 助成印Ｗ                         TO 助成印１ 助成印２
017090         IF 助成印Ｗ NOT = SPACE
017100            MOVE NC"○"                        TO 助成印固定１ 助成印固定２
017110         END-IF
      */５部位目の金額がある場合は印字行をずらす
               IF 部位５０ NOT = SPACE
                   IF 部位５８ NOT = SPACE
                       IF 長期理由文６ NOT = SPACE
                           MOVE SPACE    TO 長期理由文６
                           MOVE 部位５８ TO 部位５８２
                           MOVE 部位５０ TO 部位５８
                           MOVE SPACE    TO 部位５０
                       ELSE
                           MOVE 部位５８ TO 部位５８２
                           MOVE 部位５０ TO 部位５８
                           MOVE SPACE    TO 部位５０
                       END-IF
                   ELSE
                       MOVE 部位５０ TO 部位５８
                       MOVE SPACE    TO 部位５０
                   END-IF
               END-IF
      */愛知県/181204
           WHEN 市町村番号Ｗ(3:2) = "23"
      */助成印
017080         MOVE 助成印Ｗ                         TO 助成印１ 助成印２
017090         IF 助成印Ｗ NOT = SPACE
017100            MOVE NC"○"                        TO 助成印固定１ 助成印固定２
017110         END-IF
           WHEN OTHER
　             MOVE "X" TO EDIT-MODE OF 受給者負担額２ EDIT-MODE OF 助成請求額２
           END-EVALUATE.
      *
      */大阪府内の助成は合計、一部負担金、請求金額の３つを記載する。
      */本体の請求額の欄に助成負担額を転記し４行目を空白にする。
           IF 市町村番号Ｗ(3:2) = "27"
               MOVE NC"一部負担金"     TO 大阪用一部負担金ＣＭ
               MOVE NC"請求金額"       TO 受給者負担金ＣＭ
               MOVE SPACE              TO 給付割合 割相当額ＣＭ 請求金額ＣＭ
               MOVE ZERO               TO 助成請求額
               MOVE レセ－受給者負担額 TO 請求金額
               MOVE レセ－助成請求金額 TO 受給者負担額
           END-IF.
021560*
021570**********************
021580* 施術所データセット *
021590**********************
021600     MOVE 柔整師番号Ｗ           TO 柔整師番号.
021610*     MOVE 定額制受理番号Ｗ       TO 定額制受理番号.
021620     MOVE 施術所郵便番号１Ｗ     TO 施術所郵便番号１.
021630     MOVE 施術所郵便番号２Ｗ     TO 施術所郵便番号２.
021640*
021650     MOVE 施術所住所１Ｗ         TO 施術所住所１.
021660     MOVE 施術所住所２Ｗ         TO 施術所住所２.
021670*     MOVE 施術所住所３Ｗ         TO 施術所住所３.
021680*
021690     MOVE 印刷接骨師会会員番号Ｗ TO 接骨師会会員番号.
021700     MOVE 代表者カナＷ           TO 代表者カナ.
021710     MOVE 代表者名Ｗ             TO 代表者名.
021720     MOVE 施術所電話番号Ｗ       TO 施術所電話番号.
021730     MOVE 接骨院名Ｗ             TO 接骨院名.
           MOVE 都道府県ＪＩＳＷ       TO 都道府県番号.
021740*
021750*     MOVE 銀行名支店名Ｗ         TO 銀行名支店名.
021760*     MOVE 預金種別コメントＷ     TO 預金種別.
021770     MOVE 口座番号Ｗ             TO 口座番号.
021780     MOVE 口座名義人カナＷ       TO 口座名義人カナ.
021790     MOVE 口座名義人Ｗ           TO 口座名義人.
           MOVE 金融機関名１Ｗ   TO 金融機関名１.
           MOVE 金融機関名２Ｗ   TO 金融機関名２.
           MOVE 金融機関名３Ｗ   TO 金融機関名３.
           MOVE 金融機関名４Ｗ   TO 金融機関名４.
           MOVE 支店名１Ｗ       TO 支店名１.
           MOVE 支店名２Ｗ       TO 支店名２.
           MOVE 支店名３Ｗ       TO 支店名３.
           MOVE 支店名４Ｗ       TO 支店名４.
      *     MOVE 振込チェックＷ   TO 振込チェック.
      *     MOVE 普通チェックＷ   TO 普通チェック.
      *     MOVE 当座チェックＷ   TO 当座チェック.
      *     MOVE 銀行チェックＷ   TO 銀行チェック.
      *     MOVE 金庫チェックＷ   TO 金庫チェック.
      *     MOVE 農協チェックＷ   TO 農協チェック.
      *     MOVE 本店チェックＷ   TO 本店チェック.
      *     MOVE 支店チェックＷ   TO 支店チェック.
      *     MOVE 本支所チェックＷ TO 本支所チェック.
021800     MOVE 会長委任文１Ｗ         TO 会長委任文１.
021810     MOVE 会長委任文２Ｗ         TO 会長委任文２.
021810     MOVE 会長委任文３Ｗ         TO 会長委任文３.
021811*
021812*     MOVE NC"再委任は承諾します。" TO 委任分補足.
021820*
021830****
021840*
021850* / 柔整師・患者委任日 /
      */元号修正/↓↓↓20190408
037370     IF 施術和暦Ｗ > 4
               MOVE 施術和暦Ｗ         TO 元－元号区分
037380         READ 元号マスタ
037390         NOT INVALID KEY
037400             MOVE 元－元号名称   TO 受理和暦
037410         END-READ
               MOVE "===="             TO 受理和暦訂正
           END-IF.
      */元号修正/↑↑↑20190408
021860     MOVE 柔整師年Ｗ             TO 受理年.
021870     MOVE 柔整師月Ｗ             TO 受理月.
021880     MOVE 柔整師日Ｗ             TO 受理日.
021890* ( 委任年月日 印刷するか )
021900     IF 連入－委任印刷  = ZERO
      */元号修正/↓↓↓20190408
037370         IF 施術和暦Ｗ > 4
                   MOVE 施術和暦Ｗ         TO 元－元号区分
037380             READ 元号マスタ
037390             NOT INVALID KEY
037400                 MOVE 元－元号名称   TO 委任和暦
037410             END-READ
                   MOVE "===="             TO 委任和暦訂正
               END-IF
      */元号修正/↑↑↑20190408
021910        MOVE 患者委任年Ｗ       TO 委任年
021920        MOVE 患者委任月Ｗ       TO 委任月
021930        MOVE 患者委任日Ｗ       TO 委任日
021940     END-IF.
021950*
021960*
021970********************
021980* 欄外データセット *
021990********************
022000     MOVE 患者氏名Ｗ          TO 受診者氏名.
022010     STRING レセ管理西暦Ｗ       DELIMITED BY SPACE
022020            "-"                  DELIMITED BY SIZE
022030            施術月Ｗ             DELIMITED BY SPACE
022040            "-"                  DELIMITED BY SIZE
022050            接骨師会会員番号Ｗ   DELIMITED BY SPACE
022060            "-"                  DELIMITED BY SIZE
022070            患者コードＷＲ       DELIMITED BY SPACE
022080            INTO レセプト管理番号
022090     END-STRING.
022100     MOVE 総括表順番Ｗ        TO 総括表順番.
022670*
      *     IF 助成レセＷ NOT = 1
      *         MOVE ALL NC"＝"      TO 取消線１ 取消線２
      *         MOVE "〒510-0075 三重県四日市市安島１丁目4-16 ＫＡＮＥＮＩビル７Ｆ　TEL  059-359-0333  FAX  059-359-0335"
      *                              TO 会住所
      *     END-IF.
022110*
022120* 施術ID
022130     MOVE 県共済固定Ｗ        TO 県共済固定.
022140     MOVE 県施術ＩＤＷ        TO 県施術ＩＤ.
022150*
022151*
022152* 東京都　右上に「前」印字（高齢者） 14/10～
022153     MOVE 特別マークＷ           TO 特別マーク.
022154*
022155* 愛知県　特別コメント（４１老）14/10～
022156*     MOVE 特別コメントＷ         TO 特別コメント.
022157*
022158*
022160*****     PERFORM テスト印字処理.
022170*
022172*-------------------------------------------------------------------------*
022173*--- ※ レセ摘要再セットは、この印刷セットSECTION の最後にやること！ -----*
022174     PERFORM レセ摘要再セット.
022175*-------------------------------------------------------------------------*
022176*
022180*================================================================*
022190 項目初期化 SECTION.
022200*
022210     INITIALIZE 施術所情報Ｗ.
022220     INITIALIZE 受診者情報Ｗ.
022230     INITIALIZE 負傷情報Ｗ.
022240     INITIALIZE 備考情報Ｗ.
022250     INITIALIZE 料金１ＷＲ.
022260     INITIALIZE 料金２ＷＲ.
022270     INITIALIZE 料金３ＷＲ.
022290     INITIALIZE YHP6425P.
022280     MOVE SPACE TO YHP6425P.
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
022300*================================================================*
022310 施術所情報取得 SECTION.
022320*
022330**************************************************
022340* 本院データを使用し、以下の情報を取得           *
022350* ● 柔整師番号.. 柔整師番号Ｗに格納             *
022360* ● 会員番号 ... 接骨師会会員番号Ｗに格納       *
022370* ● 代表者名 ... 代表者名Ｗに格納               *
022380* ● 住所1,2   ...施術所住所1,2Ｗに格納          *
022390* ● 電話番号 ... 施術所電話番号Ｗに格納         *
022400**************************************************
022410     MOVE ZERO  TO 施情－施術所番号.
022420     READ 施術所情報マスタ
022430     INVALID KEY
022440         CONTINUE
022450     NOT INVALID KEY
022490         MOVE 施情－新柔整師番号   TO 柔整師番号Ｗ
022510*
022520*         MOVE   "あさひ・"             TO 接骨師会名Ｗ
022530         MOVE   施情－接骨師会会員番号 TO 接骨師会会員番号Ｗ
022540*
022550         MOVE 施情－郵便番号１        TO 施術所郵便番号１Ｗ
022560         MOVE 施情－郵便番号２        TO 施術所郵便番号２Ｗ
022570         MOVE 施情－代表者カナ        TO 代表者カナＷ
022580         MOVE 施情－代表者名          TO 代表者名Ｗ
022590         MOVE 施情－接骨院名          TO 接骨院名Ｗ
               MOVE 施情－都道府県ＪＩＳ    TO 都道府県ＪＩＳＷ
022600*
022610         MOVE 施情－住所１            TO 施術所住所１Ｗ
022620         MOVE 施情－住所２            TO 施術所住所２Ｗ
022630*         STRING 施情－住所１     DELIMITED BY SPACE
022640*                施情－住所２     DELIMITED BY SPACE
022650*                INTO 施術所住所Ｗ
022660*         END-STRING
022670*
022680         MOVE 施情－電話番号          TO 施術所電話番号Ｗ
022690** 振込先情報
022700*         MOVE 施情－取引先銀行名      TO 取引先銀行名Ｗ
022710*         MOVE 施情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
022720*         MOVE 施情－預金種別          TO 預金種別Ｗ
022730*         MOVE 施情－口座番号          TO 口座番号Ｗ
022740*         MOVE 施情－口座名義人        TO 口座名義人Ｗ
022750*         MOVE 施情－口座名義人カナ    TO 口座名義人カナＷ
022760*         STRING 取引先銀行名Ｗ     DELIMITED BY SPACE
022770*                " "                DELIMITED BY SIZE
022780*                取引先銀行支店名Ｗ DELIMITED BY SPACE
022790*                INTO 銀行名支店名Ｗ
022800*         END-STRING
022810** 振込先情報  / 会情報マスタより振込先情報を取得 /
023520         MOVE ZERO  TO  会情－柔整鍼灸区分
022820         MOVE 37    TO  会情－協会コード
022830         MOVE ZERO  TO  会情－保険種別
023530         MOVE ZERO  TO  会情－変更和暦年月
022850         READ 会情報マスタ
022860         NOT INVALID KEY
022870             MOVE 会情－取引先銀行名      TO 取引先銀行名Ｗ
022880             MOVE 会情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
022890             MOVE 会情－預金種別          TO 預金種別Ｗ
022900             MOVE 会情－口座番号          TO 口座番号Ｗ
022910             MOVE 会情－口座名義人        TO 口座名義人Ｗ
022920             MOVE 会情－口座名義人カナ    TO 口座名義人カナＷ
022930*
022940             STRING 取引先銀行名Ｗ     DELIMITED BY SPACE
022950                    " "                DELIMITED BY SIZE
022960                    取引先銀行支店名Ｗ DELIMITED BY SPACE
022970                    INTO 銀行名支店名Ｗ
022980             END-STRING
022990             EVALUATE 預金種別Ｗ
023000             WHEN 1
023010                 MOVE NC"（普通）" TO 預金種別コメントＷ
023020             WHEN 2
023030                 MOVE NC"（当座）" TO 預金種別コメントＷ
023040             WHEN OTHER
023050                 MOVE SPACE        TO 預金種別コメントＷ
023060             END-EVALUATE
023070*
023080*             MOVE 会情－接骨師会会長名    TO 会長名Ｗ
023090         END-READ
      */現状は振込のみ対応
               MOVE NC"○" TO 振込チェックＷ
      *
               EVALUATE 預金種別Ｗ
               WHEN 1
                   MOVE NC"○" TO 普通チェックＷ
               WHEN 2
                   MOVE NC"○" TO 当座チェックＷ
               END-EVALUATE
      *
               MOVE ZERO  TO カウンタ
               MOVE 取引先銀行名Ｗ TO 金融機関名Ｗ
               INSPECT 取引先銀行名Ｗ TALLYING カウンタ FOR ALL "銀行"
               IF ( カウンタ >= 1 )
                   MOVE NC"○" TO 銀行チェックＷ
                   MOVE SPACE  TO 金融機関名Ｗ
                   UNSTRING 取引先銀行名Ｗ DELIMITED BY "銀行"
                       INTO 金融機関名Ｗ
                   END-UNSTRING
               END-IF
               MOVE ZERO TO カウンタ
               INSPECT 取引先銀行名Ｗ TALLYING カウンタ FOR ALL "金庫"
               IF ( カウンタ >= 1 )
                   MOVE NC"○" TO 金庫チェックＷ
                   MOVE SPACE  TO 金融機関名Ｗ
                   UNSTRING 取引先銀行名Ｗ DELIMITED BY "金庫"
                       INTO 金融機関名Ｗ
                   END-UNSTRING
               END-IF
               MOVE ZERO TO カウンタ
               INSPECT 取引先銀行名Ｗ TALLYING カウンタ FOR ALL "農協"
               IF ( カウンタ >= 1 )
                   MOVE NC"○" TO 農協チェックＷ
                   MOVE SPACE  TO 金融機関名Ｗ
                   UNSTRING 取引先銀行名Ｗ DELIMITED BY "農協"
                       INTO 金融機関名Ｗ
                   END-UNSTRING
               END-IF
      *
               MOVE 取引先銀行支店名Ｗ TO 支店名Ｗ
               MOVE ZERO TO カウンタ
               INSPECT 取引先銀行支店名Ｗ TALLYING カウンタ FOR ALL "本店"
               IF ( カウンタ >= 1 )
                   MOVE NC"○" TO 本店チェックＷ
                   MOVE SPACE  TO 支店名Ｗ
                   UNSTRING 取引先銀行支店名Ｗ DELIMITED BY "本店"
                       INTO 支店名Ｗ
                   END-UNSTRING
               END-IF
               MOVE ZERO TO カウンタ
               INSPECT 取引先銀行支店名Ｗ TALLYING カウンタ FOR ALL "支店"
               IF ( カウンタ >= 1 )
                   MOVE NC"○" TO 支店チェックＷ
                   MOVE SPACE  TO 支店名Ｗ
                   UNSTRING 取引先銀行支店名Ｗ DELIMITED BY "支店"
                       INTO 支店名Ｗ
                   END-UNSTRING
               END-IF
               MOVE ZERO TO カウンタ
               INSPECT 取引先銀行支店名Ｗ TALLYING カウンタ FOR ALL "本所"
               IF ( カウンタ >= 1 )
                   MOVE NC"○" TO 本支所チェックＷ
                   MOVE SPACE  TO 支店名Ｗ
                   UNSTRING 取引先銀行支店名Ｗ DELIMITED BY "本所"
                       INTO 支店名Ｗ
                   END-UNSTRING
               END-IF
               MOVE ZERO TO カウンタ
               INSPECT 取引先銀行支店名Ｗ TALLYING カウンタ FOR ALL "支所"
               IF ( カウンタ >= 1 )
                   MOVE NC"○" TO 本支所チェックＷ
                   MOVE SPACE  TO 支店名Ｗ
                   UNSTRING 取引先銀行支店名Ｗ DELIMITED BY "支所"
                       INTO 支店名Ｗ
                   END-UNSTRING
               END-IF
023100*
023110     END-READ.
023120*
023130* 固定印字
023140*     MOVE NC"療養費支給金額の受領をあさひ接骨師会" TO 会長委任文１Ｗ.
023150*     MOVE NC"　会長　"           TO 会長委任文２１Ｗ.
023160*     MOVE NC"　殿に委任します。" TO 会長委任文２２Ｗ.
           MOVE "また、療養費の受領を　ホープ接骨師会"      TO 会長委任文１Ｗ
           MOVE "会長 高田徳雄 殿(三重県四日市市安島"       TO 会長委任文２Ｗ
      */会住所変更/20190311
      *     MOVE "1丁目2-24 T･Kﾋﾞﾙﾃﾞｨﾝｸﾞ5階)に委任します。"  TO 会長委任文３Ｗ
           MOVE "1丁目6-14 ラ・テラビル 1F）に委任します。" TO 会長委任文３Ｗ
      */平成31年4月請求分以降会住所変更/20190311
           MOVE "三重県四日市市安島1丁目6-14 ラ・テラビル 1F" TO 会住所
           MOVE ALL "=" TO 旧住所訂正線
023170*
023180*********************************************
023190** ＩＤ管理マスタより　県施術ＩＤを取得する。
023200*********************************************
023210** 県施術ID
023220     MOVE 01                   TO ＩＤ管－ＩＤ区分.
023230     MOVE ZERO                 TO ＩＤ管－施術所番号.
023240     MOVE 費用負担者番号助成ＷＲ(3:2)  TO ＩＤ管－保険種別.
023250     MOVE SPACE                TO ＩＤ管－保険者番号.
023260     READ ＩＤ管理マスタ
023270     NOT INVALID KEY
023280*        MOVE NC"県番号　施術機関番号"  TO 県共済固定Ｗ
023290        STRING 費用負担者番号助成ＷＲ(3:2) DELIMITED BY SPACE
023300                     "   "                 DELIMITED BY SIZE
023310                     ＩＤ管－施術ＩＤ番号  DELIMITED BY SPACE
023320                     INTO 県施術ＩＤＷ
023330        END-STRING
023340     END-READ.
023350*
023360*================================================================*
023370 請求先情報取得 SECTION.
023380*
023390****************************************************
023400* 連結データから市町村マスタより請求先を取得する。 *
023410* ※市－請求先情報区分=1の場合請求先マスタを使用   *
023420* ● 請求先...... 請求先名称Ｗに格納               *
023421*
023422* 2001/10/26 修正：兵庫28のみ支部部署名をつける
023423*
023430****************************************************
023440     MOVE 助成種別ＷＲ           TO 市－公費種別.
023450     MOVE 費用負担者番号助成ＷＲ TO 市－市町村番号.
023460****     MOVE 印刷市町村番号Ｗ       TO 市－市町村番号.
023470*
023480     READ 市町村マスタ
023490     INVALID KEY
023500         MOVE SPACE              TO 請求先名称ＴＢＬ
023510         MOVE SPACE              TO 支部部署名Ｗ
023520     NOT INVALID KEY
023530         IF 市－請求先区分 = 1
023540             MOVE 助成種別ＷＲ           TO 請先－保険種別
023550             MOVE 費用負担者番号助成ＷＲ TO 請先－保険者番号
023560             READ 請求先マスタ
023570             INVALID KEY
023580                 MOVE SPACE        TO 請求先名称ＴＢＬ
023590                 MOVE SPACE        TO 支部部署名Ｗ
023600             NOT INVALID KEY
023610                 MOVE 請先－保険者名称  TO 請求先名称ＴＢＬ
023620                 MOVE 請先－支部部署名  TO 支部部署名Ｗ
023621*
023630                 IF 費用負担者番号助成ＷＲ(3:2) NOT = "28"
023640                    MOVE SPACE TO 支部部署名Ｗ
023650                 END-IF
023651*
023660             END-READ
023670         ELSE
023680             MOVE 市－市町村名称  TO 請求先名称ＴＢＬ
023690             MOVE SPACE           TO 支部部署名Ｗ
023700         END-IF
023710     END-READ.
023720*
023730     IF 請求先名称ＴＢＬ NOT = SPACE
023740        PERFORM VARYING カウンタ FROM 1 BY 1
023750                UNTIL ( カウンタ > 20 )  OR
023760                      ( 請求先名称ＷＴ(カウンタ) = SPACE )
023770           MOVE 請求先名称ＷＴ(カウンタ) TO 請求先名称ＷＴ１
023780        END-PERFORM
023790        IF 請求先名称ＷＴ１ = "市" OR "町" OR "村" OR "区"
023800           STRING 請求先名称ＴＢＬ  DELIMITED BY SPACE
023810                  "長"              DELIMITED BY SIZE
023820                  支部部署名Ｗ      DELIMITED BY SPACE
023830                  "殿"              DELIMITED BY SIZE
023840                  INTO 請求先名称Ｗ
023850           END-STRING
023860        ELSE
023870           STRING 請求先名称ＴＢＬ  DELIMITED BY SPACE
023880                  "　"              DELIMITED BY SIZE
023890                  支部部署名Ｗ      DELIMITED BY SPACE
023900                  "殿"              DELIMITED BY SIZE
023910                  INTO 請求先名称Ｗ
023920           END-STRING
023930        END-IF
023940     END-IF.
023950*
023960*================================================================*
023970 受診者情報取得 SECTION.
023980*
023990**************************************************
024000* 連結データから受診者情報Ｆより以下の情報を取得 *
024010* ● 施術年 ..... 施術年Ｗに格納                 *
024020* ● 施術月 ..... 施術月Ｗに格納                 *
024030* ● 患者番号.... 患者番号Ｗに格納※ＦＤ連番用   *
024040* ● 記号 ....... 記号Ｗに格納                   *
024050* ● 番号 ....... 番号Ｗに格納                   *
024060* ● 保険者番号 . 保険者番号Ｗに格納             *
024070* ● 保険種別 ... 保険種別Ｗに格納               *
024080* ● 被保険者カナ.被保険者カナＷに格納           *
024090* ● 被保険者氏名.被保険者氏名Ｗに格納           *
024100* ● 住所１ ......被保険者住所１Ｗに格納         *
024110* ● 住所２ ......被保険者住所２Ｗに格納         *
024120* ● 患者住所１ ..患者住所１Ｗに格納             *
024130* ● 患者住所２ ..患者住所２Ｗに格納             *
024140* ● 患者カナ ....患者カナＷに格納               *
024150* ● 患者氏名 ....患者氏名Ｗに格納               *
024160* ● 患者性別 ....区分によりチェックに"○"を格納 *
024170* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
024180* ● 患者年 ......患者年Ｗに格納                 *
024190* ● 患者月 ......患者月Ｗに格納                 *
024200* ● 患者日 ......患者日Ｗに格納                 *
024210* ● 続柄 ........名称マスタより続柄Ｗに取得     *
024220**************************************************
           IF 受－レコード NOT = SPACE
022660         EVALUATE 受－保険種別
022670         WHEN 01
022690            MOVE NC"○"        TO 国保チェックＷ
022680         WHEN 08
022690            MOVE NC"○"        TO 退職チェックＷ
022700         WHEN 02
022710         WHEN 06
022750         WHEN 07
022720            MOVE NC"○"        TO 協会チェックＷ
022730         WHEN 03
022740            MOVE NC"○"        TO 組合チェックＷ
               WHEN 04
               WHEN 09
                  MOVE NC"○"        TO 共済チェックＷ
               WHEN 05
                  MOVE NC"○"        TO 後期チェックＷ
022770         END-EVALUATE
               IF 受－助成種別 = ZERO
                   MOVE NC"○" TO 単独チェックＷ
               ELSE
                   MOVE NC"○" TO ２併チェックＷ
               END-IF
      */本家区分はどれか１つに○をする。
               IF 受－保険種別 = 05
                   EVALUATE 受－特別区分
                   WHEN 1
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
024320*         EVALUATE 受－施術和暦
024330*         WHEN 1
024340*             MOVE NC"明治"  TO 施術和暦Ｗ
024350*         WHEN 2
024360*             MOVE NC"大正"  TO 施術和暦Ｗ
024370*         WHEN 3
024380*             MOVE NC"昭和"  TO 施術和暦Ｗ
024390*         WHEN 4
024400*             MOVE NC"平成"  TO 施術和暦Ｗ
024410*         END-EVALUATE
      */元号修正/20190408
               MOVE 受－施術和暦     TO 施術和暦Ｗ
024420         MOVE 受－施術年       TO 施術年Ｗ
024430         MOVE 受－施術月       TO 施術月Ｗ
024440         MOVE 受－患者番号     TO 患者番号Ｗ
024450*         MOVE 受－記号         TO 記号Ｗ
024460*         MOVE 受－番号         TO 番号Ｗ
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
024470         MOVE 受－保険者番号   TO 保険者番号Ｗ
024480         MOVE 受－保険種別     TO 保険種別Ｗ
024490** 全国土木の枝番削除
024500         IF ( 受－保険種別 = 01 ) AND ( 受－保険者番号(1:6) = "133033" )
024510            MOVE 受－保険者番号(1:6)  TO 保険者番号Ｗ
024520         END-IF
024530**
024540         MOVE 受－被保険者カナ TO 被保険者カナＷ
024550         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
024560*         MOVE 受－郵便番号１   TO 郵便番号１Ｗ
024570*         MOVE 受－郵便番号２   TO 郵便番号２Ｗ
024580         MOVE 受－住所１       TO 被保険者住所１Ｗ
024590         MOVE 受－住所２       TO 被保険者住所２Ｗ
024560         MOVE 受－患者郵便番号１   TO 郵便番号１Ｗ
024570         MOVE 受－患者郵便番号２   TO 郵便番号２Ｗ
024600         MOVE 受－患者住所１   TO 患者住所１Ｗ
024610         MOVE 受－患者住所２   TO 患者住所２Ｗ
      */ 電話番号追加 /42505
               IF 受－患者電話番号 NOT = SPACE
                  STRING "電話:"            DELIMITED BY SIZE
                         受－患者電話番号   DELIMITED BY SPACE
                    INTO 電話番号Ｗ
                  END-STRING
               END-IF
024620         MOVE 受－患者カナ     TO 患者カナＷ
024630         MOVE 受－患者氏名     TO 患者氏名Ｗ
024640* 助成用
024650         MOVE 受－費用負担者番号助成 TO 市町村番号Ｗ
024660         MOVE 受－受益者番号助成     TO 受給者番号Ｗ
024670*
024680*
024690         EVALUATE 受－被保険者性別
024700         WHEN 1
024710             MOVE NC"男"  TO 被保険者性別Ｗ
024720         WHEN 2
024730             MOVE NC"女"  TO 被保険者性別Ｗ
024740         END-EVALUATE
024750*
024760         EVALUATE 受－患者性別
024770         WHEN 1
024780             MOVE "(男)"  TO 患者性別Ｗ
024790         WHEN 2
024800             MOVE "(女)"  TO 患者性別Ｗ
024810         END-EVALUATE
024820         EVALUATE 受－患者性別
024830         WHEN 1
024840             MOVE NC"○"  TO 男チェックＷ
024850         WHEN 2
024860             MOVE NC"○"  TO 女チェックＷ
024870         END-EVALUATE
024880
024890         EVALUATE 受－患者和暦
024900         WHEN 1
024910             MOVE NC"○"  TO 明治チェックＷ
024920         WHEN 2
024930             MOVE NC"○"  TO 大正チェックＷ
024940         WHEN 3
024950             MOVE NC"○"  TO 昭和チェックＷ
024960         WHEN 4
024970             MOVE NC"○"  TO 平成チェックＷ
      */元号修正/20190408
023060         WHEN 5
                   MOVE "5令"   TO 令和ＣＭＷ
023070             MOVE NC"○"  TO 令和チェックＷ
024980         END-EVALUATE
024990         EVALUATE 受－患者和暦
025000         WHEN 1
025010             MOVE NC"明治"  TO 元号Ｗ
025020         WHEN 2
025030             MOVE NC"大正"  TO 元号Ｗ
025040         WHEN 3
025050             MOVE NC"昭和"  TO 元号Ｗ
025060         WHEN 4
025070             MOVE NC"平成"  TO 元号Ｗ
025080         END-EVALUATE
025090*
      */元号修正/↓↓↓20190408
029310         IF 受－患者和暦 > 4
037370             MOVE 受－患者和暦     TO 元－元号区分
037380             READ 元号マスタ
037390             NOT INVALID KEY
037400                 MOVE 元－元号名称 TO 元号Ｗ
037410             END-READ
029330         END-IF
      */元号修正/↑↑↑20190408
025100         MOVE 受－患者年  TO 患者年Ｗ
025110         MOVE 受－患者月  TO 患者月Ｗ
025120         MOVE 受－患者日  TO 患者日Ｗ
025130* 続柄
025140         EVALUATE 保険種別ＷＲ 
025150* 国保・退職
025160         WHEN 01
025170         WHEN 08
025180             IF 本人家族区分ＷＲ = 1
025190                 MOVE NC"本人"    TO 続柄Ｗ
025200             ELSE
025210                 MOVE NC"―"      TO 続柄Ｗ
025220             END-IF
025230         WHEN OTHER
025240             IF 本人家族区分ＷＲ = 1
025250                  MOVE NC"本人"    TO 続柄Ｗ
025260             ELSE
025270                  MOVE 05          TO 名－区分コード
025280                  MOVE 受－続柄    TO 名－名称コード
025290                  READ 名称マスタ
025300                  INVALID KEY
025310                      MOVE SPACE    TO 続柄Ｗ
025320                  NOT INVALID KEY
025330                      MOVE 名－略称 TO 続柄Ｗ
025340                  END-READ
025350             END-IF
025360         END-EVALUATE
025370*
025380** 保険種別チェックを設定
025390         EVALUATE 保険種別ＷＲ
025400         WHEN  01
025410             MOVE NC"国保"   TO 保険種別名称Ｗ
025420         WHEN  02
025430         WHEN  06
025440         WHEN  07
025450             MOVE NC"政管"   TO 保険種別名称Ｗ
025460         WHEN  03
025470             MOVE NC"組合"   TO 保険種別名称Ｗ
025480         WHEN  04
025490             MOVE NC"共済"   TO 保険種別名称Ｗ
025500         WHEN  08
025510             MOVE NC"退職"   TO 保険種別名称Ｗ
025520         WHEN  09
025530             MOVE NC"自衛官" TO 保険種別名称Ｗ
025540         END-EVALUATE
025550         IF ( 公費種別ＷＲ NOT = ZERO )  AND
025560            ( 助成種別ＷＲ NOT = ZERO )
                   IF 受－施術和暦年月 < 42004
025570                 MOVE NC"老人" TO 保険種別名称Ｗ
                   ELSE
025570                 MOVE SPACE          TO 保険種別名称Ｗ
025570                 MOVE NC"後期高齢者" TO 保険種別名称２Ｗ
                   END-IF
025580         END-IF
025590*
025600         MOVE NC"生年月日" TO 生年月日固定Ｗ
025610*
025611*
025612*---  市町村独自仕様 -----*
025613* 14/10～　東京都のみ→ 特別区分1,2,3(高齢者）の時、「前」を右上に印字
025614*                       親が老人の時、保険者番号欄には、２７番号を印字
025615         IF 受－施術和暦年月 >= 41410
025616            IF 受－費用負担者番号助成(3:2) = "13"
025617               IF 受－公費種別 = ZERO
025618                  IF 受－特別区分 = 1 OR 2 OR 3
025619                     MOVE NC"前" TO 特別マークＷ
025620                  END-IF
025621               ELSE
025622                  MOVE 受－費用負担者番号  TO 保険者番号Ｗ
025623               END-IF
025624            END-IF
025625         END-IF
025626*
025627* 14/10～　愛知県のみ→ 41老人の負担率を右上に印字
025628         IF 受－施術和暦年月 >= 41410
025629            IF ( 受－費用負担者番号助成(3:2) = "23" ) AND
025630               ( 受－助成種別 = 51 )
025637               EVALUATE 受－助成負担金免除
025638               WHEN 2
025639                  MOVE "41老人 ２割"   TO 特別コメントＷ
025640               WHEN 3
025641                  MOVE "41老人 ３割"   TO 特別コメントＷ
025642               WHEN OTHER
025643                  MOVE "41老人 １割"   TO 特別コメントＷ
025644               END-EVALUATE
025646            END-IF
025647         END-IF
025648*
025649     END-IF.
025650*================================================================*
025651 負傷データ取得 SECTION.
025652*
025660**************************************************
025670* 連結データから負傷データＦより以下の情報を取得 *
025680* ● 負傷名...部位＋負傷種別にて加工して格納     *
025690* ● 負傷年.......負傷年Ｗ                       *
025700* ● 負傷月.......負傷月Ｗ                       *
025710* ● 負傷日.......負傷日Ｗ                       *
025720* ● 開始年.......初検年Ｗ                       *
025730* ● 開始月.......初検月Ｗ                       *
025740* ● 開始日.......初検日Ｗ                       *
025750* ● 終了年.......終了年Ｗ                       *
025760* ● 終了月.......終了月Ｗ                       *
025770* ● 終了日.......終了日Ｗ                       *
025780* ● 実日数.......実日数Ｗ                       *
025790* ● 転帰区分 ....区分によりチェックに"○"を格納 *
025800* ● 金属副子 ....区分によりチェックに"○"を格納 *
025810* ● 経過コード...経過マスタより取得             *
025820**************************************************
           IF 負－レコード NOT = SPACE
025920         MOVE 負－部位数                   TO 部位数Ｗ
025930         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
025940                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
025950             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別Ｗ(部位ＣＮＴ)
025960             MOVE 負－部位(部位ＣＮＴ)     TO 部位Ｗ(部位ＣＮＴ)
025970             MOVE 負－左右区分(部位ＣＮＴ) TO 左右区分Ｗ(部位ＣＮＴ)
025980             MOVE 負－負傷位置番号(部位ＣＮＴ)
025990                                           TO 負傷位置番号Ｗ(部位ＣＮＴ)
026000********************************************************
026010* 注）全柔...部位名1+負傷種別＋部位名2にて加工して格納 *
026020********************************************************
026030* 負傷種別
026040             MOVE SPACE                     TO 負傷名称Ｗ
026050             MOVE 03                        TO 名－区分コード
026060             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
026070             READ 名称マスタ
026080             INVALID KEY
026090                 MOVE SPACE        TO 負傷名称Ｗ
026100             NOT INVALID KEY
026110                 MOVE 名－正式名称 TO 負傷名称Ｗ
026120             END-READ
026130* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
032680*
032690             PERFORM 部位名称埋込処理
026320*
026330             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
026340             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
026350             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
026360             MOVE 負－開始年(部位ＣＮＴ)   TO 初検年Ｗ(部位ＣＮＴ)
026370             MOVE 負－開始月(部位ＣＮＴ)   TO 初検月Ｗ(部位ＣＮＴ)
026380             MOVE 負－開始日(部位ＣＮＴ)   TO 初検日Ｗ(部位ＣＮＴ)
026390             IF 負－転帰区分(部位ＣＮＴ) = 9
026400                 MOVE 99                   TO 終了年Ｗ(部位ＣＮＴ)
026410                 MOVE 99                   TO 終了月Ｗ(部位ＣＮＴ)
026420                 MOVE 99                   TO 終了日Ｗ(部位ＣＮＴ)
026430             ELSE
026440                 MOVE 負－終了年(部位ＣＮＴ)   TO 終了年Ｗ(部位ＣＮＴ)
026450                 MOVE 負－終了月(部位ＣＮＴ)   TO 終了月Ｗ(部位ＣＮＴ)
026460                 MOVE 負－終了日(部位ＣＮＴ)   TO 終了日Ｗ(部位ＣＮＴ)
026470             END-IF
026480* 経過略称取得
026490             MOVE 01                         TO 経－区分コード
026500             MOVE 負－経過コード(部位ＣＮＴ) TO 経－経過コード
026510             READ 経過マスタ
026520             INVALID KEY
026530                 MOVE ZERO            TO 部位ＣＮＴＷ(部位ＣＮＴ)
026540                 MOVE SPACE           TO 部位区切Ｗ(部位ＣＮＴ)
026550                 MOVE SPACE           TO 経過略称Ｗ(部位ＣＮＴ)
026560             NOT INVALID KEY
026570                 EVALUATE 部位ＣＮＴ
026580                 WHEN 1
026590                     MOVE NC"①" TO 経過部位Ｗ
026600                 WHEN 2
026610                     MOVE NC"②" TO 経過部位Ｗ
026620                 WHEN 3
026630                     MOVE NC"③" TO 経過部位Ｗ
026640                 WHEN 4
026650                     MOVE NC"④" TO 経過部位Ｗ
026660                 WHEN 5
026670                     MOVE NC"⑤" TO 経過部位Ｗ
026680                 END-EVALUATE
026690                 STRING  経過部位Ｗ     DELIMITED BY SPACE
026700                         経－経過略称   DELIMITED BY SPACE
026710                        INTO 印刷経過略称Ｗ(部位ＣＮＴ)
026720                 END-STRING
026730             END-READ
026740*
026750             MOVE 負－転帰区分(部位ＣＮＴ) TO 転帰区分Ｗ(部位ＣＮＴ)
026760             EVALUATE 負－転帰区分(部位ＣＮＴ)
026770             WHEN 1
026780             WHEN 2
026790                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
026800             WHEN 3
026810                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
026820             WHEN 4
026830                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
026840             END-EVALUATE
026850*
026860*             EVALUATE 負－転帰区分(部位ＣＮＴ)
026870*             WHEN 1
026880*             WHEN 2
026890*                 MOVE NC"治癒"               TO 転帰Ｗ(部位ＣＮＴ)
026900*             WHEN 3
026910*                 MOVE NC"中止"               TO 転帰Ｗ(部位ＣＮＴ)
026920*             WHEN 4
026930*                 MOVE NC"転医"               TO 転帰Ｗ(部位ＣＮＴ)
026940*             WHEN OTHER
026950*                 MOVE NC"継続"               TO 転帰Ｗ(部位ＣＮＴ)
026960*             END-EVALUATE
026970*
                   MOVE レセ－部位実日数(部位ＣＮＴ) TO 実日数Ｗ(部位ＣＮＴ)
026980         END-PERFORM
026990* 新規/継続 チェック
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
027110* 枝番判定用
027120         MOVE 負－開始診療日手動区分 TO  開始診療日手動区分Ｗ
027130*
027131* 負傷原因印刷区分
027132         MOVE 負－レセ負傷原因印刷区分 TO レセ負傷原因印刷区分Ｗ
027880         MOVE 負－レセ長期理由印刷区分 TO レセ長期理由印刷区分Ｗ
027133*
027140     END-IF.
027150*================================================================*
030910 部位名称埋込処理 SECTION.
030920*
006490     STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
009980            負傷名称Ｗ                    DELIMITED BY SPACE
006500            レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
006520       INTO 負傷名Ｗ(部位ＣＮＴ)
006570     END-STRING.
027310*
027320*================================================================*
027330 料金情報取得 SECTION.
027340*
027350********************
027360* 料金データセット *
027370********************
027380*    ****************************************************************
027390*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
027400*    ****************************************************************
027410     MOVE レセ－初検料                 TO 初検料ＷＲ.
027420     IF レセ－時間外 = 1
027430         MOVE NC"○"                   TO 時間外チェックＷ
027440     END-IF.
027450     IF レセ－休日 = 1
027460         MOVE NC"○"                   TO 休日チェックＷ
027470     END-IF.
027480     IF レセ－深夜 = 1
027490         MOVE NC"○"                   TO 深夜チェックＷ
027500     END-IF.
027510*
027520*     IF レセ－時間外 = 1
027530*         MOVE NC"時間外"               TO 時間外Ｗ
027540*     END-IF.
027550*     IF レセ－休日 = 1
027560*         MOVE NC"休日"                 TO 休日Ｗ
027570*     END-IF.
027580*     IF レセ－深夜 = 1
027590*         MOVE NC"深夜"                 TO 深夜Ｗ
027600*     END-IF.
027610*
027620*     STRING 時間外Ｗ     DELIMITED BY SPACE
027630*            NC"　"       DELIMITED BY SIZE
027640*            休日Ｗ       DELIMITED BY SPACE
027650*            NC"　"       DELIMITED BY SIZE
027660*            深夜Ｗ       DELIMITED BY SPACE
027670*            INTO 初検加算内容Ｗ
027680*     END-STRING.
027690*
027700     MOVE レセ－初検加算料             TO  初検加算料ＷＲ.
           MOVE レセ－初検時相談料           TO  初検時相談料ＷＲ.
027710     MOVE レセ－再検料                 TO  再検料ＷＲ.
027720     MOVE レセ－往療距離               TO  往療距離ＷＲ.
027730     MOVE レセ－往療回数               TO  往療回数ＷＲ.
027740     MOVE レセ－往療料                 TO  往療料ＷＲ.
027750     MOVE レセ－往療加算料             TO  往療加算料ＷＲ.
027760*
027770     IF レセ－夜間 = 1
027780         MOVE NC"○"                   TO 夜間チェックＷ
027790     END-IF.
027800     IF レセ－時間外 = 1
027810         MOVE NC"○"                   TO 往療深夜チェックＷ
027820     END-IF.
027830     IF レセ－暴風雨雪 = 1
027840         MOVE NC"○"                   TO 暴風雨雪チェックＷ
027850     END-IF.
027860     IF レセ－難路 = 1
027870         MOVE NC"○"                   TO 難路チェックＷ
027880     END-IF.
027890*
027900     MOVE レセ－金属副子加算料         TO  金属副子加算料ＷＲ.
027910*
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
028010*
028020*     IF レセ－大 = 1
028030*         MOVE NC"大"                   TO 金属大Ｗ
028040*     END-IF.
028050*     IF レセ－中 = 1
028060*         MOVE NC"中"                   TO 金属中Ｗ
028070*     END-IF.
028080*     IF レセ－小 = 1
028090*         MOVE NC"小"                   TO 金属小Ｗ
028100*     END-IF.
028110*
028120     MOVE レセ－施術情報提供料         TO 施術情報提供料ＷＲ.
028130* 小計
028140     MOVE レセ－小計                   TO 小計Ｗ.
028150********************
028160* 初回処置料セット *
028170********************
028180     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
028190             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
028200         MOVE レセ－初回処置料(部位ＣＮＴ) TO 初回処置料ＷＲ(部位ＣＮＴ)
028210         IF レセ－初回処置料(部位ＣＮＴ) NOT = ZERO
028220            EVALUATE 負－負傷種別(部位ＣＮＴ)
028230* 捻挫・打撲・挫傷
028240            WHEN 1
028250            WHEN 2
028260            WHEN 3
028270                MOVE NC"○"       TO 施療料チェックＷ
028280* 脱臼・骨折・骨折拘縮
028290            WHEN 4
028300            WHEN 5
028310            WHEN 7
028320                MOVE NC"○"       TO 整復料チェックＷ
028330* 不全骨折・不全骨折拘縮
028340            WHEN 6
028350            WHEN 8
028360                MOVE NC"○"       TO 固定料チェックＷ
028370            END-EVALUATE
028380         END-IF
028390     END-PERFORM.
028400     MOVE レセ－初回処置料合計    TO 初回処置料合計Ｗ.
028410********************
028420* 逓減毎料金セット *
028430********************
028440*    **********
028450*    * １部位 *
028460*    **********
028470     MOVE レセ－後療単価１             TO 後療単価１ＷＲ.
028480     MOVE レセ－後療回数１             TO 後療回数１ＷＲ.
028490     MOVE レセ－後療料１               TO 後療料１ＷＲ.
028500     MOVE レセ－冷罨法回数１           TO 冷罨法回数１ＷＲ.
028510     MOVE レセ－冷罨法料１             TO 冷罨法料１ＷＲ.
028520     MOVE レセ－温罨法回数１           TO 温罨法回数１ＷＲ.
028530     MOVE レセ－温罨法料１             TO 温罨法料１ＷＲ.
028540     MOVE レセ－電療回数１             TO 電療回数１ＷＲ.
028550     MOVE レセ－電療料１               TO 電療料１ＷＲ.
028560     MOVE レセ－小計１                 TO 小計１ＷＲ.
028570     MOVE レセ－長期逓減率１           TO 長期逓減率１ＷＲ.
028580     MOVE レセ－長期込小計１           TO 長期込小計１ＷＲ.
028590*    **********
028600*    * ２部位 *
028610*    **********
028620     MOVE レセ－後療単価２             TO 後療単価２ＷＲ.
028630     MOVE レセ－後療回数２             TO 後療回数２ＷＲ.
028640     MOVE レセ－後療料２               TO 後療料２ＷＲ.
028650     MOVE レセ－冷罨法回数２           TO 冷罨法回数２ＷＲ.
028660     MOVE レセ－冷罨法料２             TO 冷罨法料２ＷＲ.
028670     MOVE レセ－温罨法回数２           TO 温罨法回数２ＷＲ.
028680     MOVE レセ－温罨法料２             TO 温罨法料２ＷＲ.
028690     MOVE レセ－電療回数２             TO 電療回数２ＷＲ.
028700     MOVE レセ－電療料２               TO 電療料２ＷＲ.
028710     MOVE レセ－小計２                 TO 小計２ＷＲ.
028720     MOVE レセ－長期逓減率２           TO 長期逓減率２ＷＲ.
028730     MOVE レセ－長期込小計２           TO 長期込小計２ＷＲ.
028740*    ****************
028750*    * ３部位／８割 *
028760*    ****************
028770     MOVE レセ－後療単価３８             TO 後療単価３８ＷＲ.
028780     MOVE レセ－後療回数３８             TO 後療回数３８ＷＲ.
028790     MOVE レセ－後療料３８               TO 後療料３８ＷＲ.
028800     MOVE レセ－冷罨法回数３８           TO 冷罨法回数３８ＷＲ.
028810     MOVE レセ－冷罨法料３８             TO 冷罨法料３８ＷＲ.
028820     MOVE レセ－温罨法回数３８           TO 温罨法回数３８ＷＲ.
028830     MOVE レセ－温罨法料３８             TO 温罨法料３８ＷＲ.
028840     MOVE レセ－電療回数３８             TO 電療回数３８ＷＲ.
028850     MOVE レセ－電療料３８               TO 電療料３８ＷＲ.
028860     MOVE レセ－小計３８                 TO 小計３８ＷＲ.
028870     MOVE レセ－多部位込小計３８         TO 多部位込小計３８ＷＲ.
028880     MOVE レセ－長期逓減率３８           TO 長期逓減率３８ＷＲ.
028890     MOVE レセ－長期込小計３８           TO 長期込小計３８ＷＲ.
028900*    ****************
028910*    * ３部位／10割 *
028920*    ****************
028930     MOVE レセ－逓減開始月３０           TO 逓減開始月３０ＷＲ.
028940     MOVE レセ－逓減開始日３０           TO 逓減開始日３０ＷＲ.
028950     MOVE レセ－後療単価３０             TO 後療単価３０ＷＲ.
028960     MOVE レセ－後療回数３０             TO 後療回数３０ＷＲ.
028970     MOVE レセ－後療料３０               TO 後療料３０ＷＲ.
028980     MOVE レセ－冷罨法回数３０           TO 冷罨法回数３０ＷＲ.
028990     MOVE レセ－冷罨法料３０             TO 冷罨法料３０ＷＲ.
029000     MOVE レセ－温罨法回数３０           TO 温罨法回数３０ＷＲ.
029010     MOVE レセ－温罨法料３０             TO 温罨法料３０ＷＲ.
029020     MOVE レセ－電療回数３０             TO 電療回数３０ＷＲ.
029030     MOVE レセ－電療料３０               TO 電療料３０ＷＲ.
029040     MOVE レセ－小計３０                 TO 小計３０ＷＲ.
029050     MOVE レセ－長期逓減率３０           TO 長期逓減率３０ＷＲ.
029060     MOVE レセ－長期込小計３０           TO 長期込小計３０ＷＲ.
029070*    ****************
029080*    * ４部位／５割 *
029090*    ****************
029100     MOVE レセ－後療単価４５             TO 後療単価４５ＷＲ.
029110     MOVE レセ－後療回数４５             TO 後療回数４５ＷＲ.
029120     MOVE レセ－後療料４５               TO 後療料４５ＷＲ.
029130     MOVE レセ－冷罨法回数４５           TO 冷罨法回数４５ＷＲ.
029140     MOVE レセ－冷罨法料４５             TO 冷罨法料４５ＷＲ.
029150     MOVE レセ－温罨法回数４５           TO 温罨法回数４５ＷＲ.
029160     MOVE レセ－温罨法料４５             TO 温罨法料４５ＷＲ.
029170     MOVE レセ－電療回数４５             TO 電療回数４５ＷＲ.
029180     MOVE レセ－電療料４５               TO 電療料４５ＷＲ.
029190     MOVE レセ－小計４５                 TO 小計４５ＷＲ.
029200     MOVE レセ－多部位込小計４５         TO 多部位込小計４５ＷＲ.
029210     MOVE レセ－長期逓減率４５           TO 長期逓減率４５ＷＲ.
029220     MOVE レセ－長期込小計４５           TO 長期込小計４５ＷＲ.
029230*    ****************
029240*    * ４部位／８割 *
029250*    ****************
029260     MOVE レセ－逓減開始月４８           TO 逓減開始月４８ＷＲ.
029270     MOVE レセ－逓減開始日４８           TO 逓減開始日４８ＷＲ.
029280     MOVE レセ－後療単価４８             TO 後療単価４８ＷＲ.
029290     MOVE レセ－後療回数４８             TO 後療回数４８ＷＲ.
029300     MOVE レセ－後療料４８               TO 後療料４８ＷＲ.
029310     MOVE レセ－冷罨法回数４８           TO 冷罨法回数４８ＷＲ.
029320     MOVE レセ－冷罨法料４８             TO 冷罨法料４８ＷＲ.
029330     MOVE レセ－温罨法回数４８           TO 温罨法回数４８ＷＲ.
029340     MOVE レセ－温罨法料４８             TO 温罨法料４８ＷＲ.
029350     MOVE レセ－電療回数４８             TO 電療回数４８ＷＲ.
029360     MOVE レセ－電療料４８               TO 電療料４８ＷＲ.
029370     MOVE レセ－小計４８                 TO 小計４８ＷＲ.
029380     MOVE レセ－多部位込小計４８         TO 多部位込小計４８ＷＲ.
029390     MOVE レセ－長期逓減率４８           TO 長期逓減率４８ＷＲ.
029400     MOVE レセ－長期込小計４８           TO 長期込小計４８ＷＲ.
029410*    ****************
029420*    * ４部位／10割 *
029430*    ****************
029440     MOVE レセ－逓減開始月４０           TO 逓減開始月４０ＷＲ.
029450     MOVE レセ－逓減開始日４０           TO 逓減開始日４０ＷＲ.
029460     MOVE レセ－後療単価４０             TO 後療単価４０ＷＲ.
029470     MOVE レセ－後療回数４０             TO 後療回数４０ＷＲ.
029480     MOVE レセ－後療料４０               TO 後療料４０ＷＲ.
029490     MOVE レセ－冷罨法回数４０           TO 冷罨法回数４０ＷＲ.
029500     MOVE レセ－冷罨法料４０             TO 冷罨法料４０ＷＲ.
029510     MOVE レセ－温罨法回数４０           TO 温罨法回数４０ＷＲ.
029520     MOVE レセ－温罨法料４０             TO 温罨法料４０ＷＲ.
029530     MOVE レセ－電療回数４０             TO 電療回数４０ＷＲ.
029540     MOVE レセ－電療料４０               TO 電療料４０ＷＲ.
029550     MOVE レセ－小計４０                 TO 小計４０ＷＲ.
029560     MOVE レセ－長期逓減率４０           TO 長期逓減率４０ＷＲ.
029570     MOVE レセ－長期込小計４０           TO 長期込小計４０ＷＲ.
029580*    *****************
029590*    * ５部位／2.5割 *
029600*    *****************
029610     MOVE レセ－後療単価５２             TO 後療単価５２ＷＲ.
029620     MOVE レセ－後療回数５２             TO 後療回数５２ＷＲ.
029630     MOVE レセ－後療料５２               TO 後療料５２ＷＲ.
029640     MOVE レセ－冷罨法回数５２           TO 冷罨法回数５２ＷＲ.
029650     MOVE レセ－冷罨法料５２             TO 冷罨法料５２ＷＲ.
029660     MOVE レセ－温罨法回数５２           TO 温罨法回数５２ＷＲ.
029670     MOVE レセ－温罨法料５２             TO 温罨法料５２ＷＲ.
029680     MOVE レセ－電療回数５２             TO 電療回数５２ＷＲ.
029690     MOVE レセ－電療料５２               TO 電療料５２ＷＲ.
029700     MOVE レセ－小計５２                 TO 小計５２ＷＲ.
029710     MOVE レセ－多部位込小計５２         TO 多部位込小計５２ＷＲ.
029720     MOVE レセ－長期逓減率５２           TO 長期逓減率５２ＷＲ.
029730     MOVE レセ－長期込小計５２           TO 長期込小計５２ＷＲ.
029740*    ****************
029750*    * ５部位／５割 *
029760*    ****************
029770     MOVE レセ－逓減開始月５５           TO 逓減開始月５５ＷＲ.
029780     MOVE レセ－逓減開始日５５           TO 逓減開始日５５ＷＲ.
029790     MOVE レセ－後療単価５５             TO 後療単価５５ＷＲ.
029800     MOVE レセ－後療回数５５             TO 後療回数５５ＷＲ.
029810     MOVE レセ－後療料５５               TO 後療料５５ＷＲ.
029820     MOVE レセ－冷罨法回数５５           TO 冷罨法回数５５ＷＲ.
029830     MOVE レセ－冷罨法料５５             TO 冷罨法料５５ＷＲ.
029840     MOVE レセ－温罨法回数５５           TO 温罨法回数５５ＷＲ.
029850     MOVE レセ－温罨法料５５             TO 温罨法料５５ＷＲ.
029860     MOVE レセ－電療回数５５             TO 電療回数５５ＷＲ.
029870     MOVE レセ－電療料５５               TO 電療料５５ＷＲ.
029880     MOVE レセ－小計５５                 TO 小計５５ＷＲ.
029890     MOVE レセ－多部位込小計５５         TO 多部位込小計５５ＷＲ.
029900     MOVE レセ－長期逓減率５５           TO 長期逓減率５５ＷＲ.
029910     MOVE レセ－長期込小計５５           TO 長期込小計５５ＷＲ.
029920*    ****************
029930*    * ５部位／８割 *
029940*    ****************
029950     MOVE レセ－逓減開始月５８           TO 逓減開始月５８ＷＲ.
029960     MOVE レセ－逓減開始日５８           TO 逓減開始日５８ＷＲ.
029970     MOVE レセ－後療単価５８             TO 後療単価５８ＷＲ.
029980     MOVE レセ－後療回数５８             TO 後療回数５８ＷＲ.
029990     MOVE レセ－後療料５８               TO 後療料５８ＷＲ.
030000     MOVE レセ－冷罨法回数５８           TO 冷罨法回数５８ＷＲ.
030010     MOVE レセ－冷罨法料５８             TO 冷罨法料５８ＷＲ.
030020     MOVE レセ－温罨法回数５８           TO 温罨法回数５８ＷＲ.
030030     MOVE レセ－温罨法料５８             TO 温罨法料５８ＷＲ.
030040     MOVE レセ－電療回数５８             TO 電療回数５８ＷＲ.
030050     MOVE レセ－電療料５８               TO 電療料５８ＷＲ.
030060     MOVE レセ－小計５８                 TO 小計５８ＷＲ.
030070     MOVE レセ－多部位込小計５８         TO 多部位込小計５８ＷＲ.
030080     MOVE レセ－長期逓減率５８           TO 長期逓減率５８ＷＲ.
030090     MOVE レセ－長期込小計５８           TO 長期込小計５８ＷＲ.
030100*    ****************
030110*    * ５部位／10割 *
030120*    ****************
030130     MOVE レセ－逓減開始月５０           TO 逓減開始月５０ＷＲ.
030140     MOVE レセ－逓減開始日５０           TO 逓減開始日５０ＷＲ.
030150     MOVE レセ－後療単価５０             TO 後療単価５０ＷＲ.
030160     MOVE レセ－後療回数５０             TO 後療回数５０ＷＲ.
030170     MOVE レセ－後療料５０               TO 後療料５０ＷＲ.
030180     MOVE レセ－冷罨法回数５０           TO 冷罨法回数５０ＷＲ.
030190     MOVE レセ－冷罨法料５０             TO 冷罨法料５０ＷＲ.
030200     MOVE レセ－温罨法回数５０           TO 温罨法回数５０ＷＲ.
030210     MOVE レセ－温罨法料５０             TO 温罨法料５０ＷＲ.
030220     MOVE レセ－電療回数５０             TO 電療回数５０ＷＲ.
030230     MOVE レセ－電療料５０               TO 電療料５０ＷＲ.
030240     MOVE レセ－小計５０                 TO 小計５０ＷＲ.
030250     MOVE レセ－長期逓減率５０           TO 長期逓減率５０ＷＲ.
030260     MOVE レセ－長期込小計５０           TO 長期込小計５０ＷＲ.
030270*
030280*================================================================*
030290 施術記録取得 SECTION.
030300*
030310************************************************************
030320* 作１データから負傷データＦより以下の情報を取得           *
030330* ● 初検加算 .....区分によりチェックに"○"を格納...複数可 *
030340* ● 往療加算 .....区分によりチェックに"○"を格納...複数可 *
030350************************************************************
030360     MOVE  SPACE  TO  初日再検フラグ.
030370     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 部位数Ｗ
030380         IF ( 施術年Ｗ = 初検年Ｗ(部位ＣＮＴ) ) AND
030390            ( 施術月Ｗ = 初検月Ｗ(部位ＣＮＴ) )
030400             MOVE 患者番号ＷＲ          TO 施記－患者番号
030410             MOVE 枝番ＷＲ              TO 施記－枝番
030420             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
030430             MOVE 初検年Ｗ(部位ＣＮＴ)  TO 開始年Ｗ(部位ＣＮＴ) 施記－施術年
030440             MOVE 初検月Ｗ(部位ＣＮＴ)  TO 開始月Ｗ(部位ＣＮＴ) 施記－施術月
030450             MOVE 初検日Ｗ(部位ＣＮＴ)  TO 開始日Ｗ(部位ＣＮＴ) 施記－施術日
030460         ELSE
030470             MOVE 患者番号ＷＲ          TO 施記－患者番号
030480             MOVE 枝番ＷＲ              TO 施記－枝番
030490             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
030500             MOVE 施術年ＷＲ            TO 施記－施術年
030510             MOVE 施術月ＷＲ            TO 施記－施術月
030520             MOVE ZERO                  TO 施記－施術日
030530         END-IF
030540         START 施術記録Ｆ   KEY IS >= 施記－患者コード
030550                                      施記－施術和暦年月日
030560         END-START
030570         IF 状態キー = "00"
030590             MOVE ZERO  TO 終了年ＷＴ
030600             MOVE ZERO  TO 終了月ＷＴ
030610             MOVE ZERO  TO 終了日ＷＴ
030620             MOVE SPACE TO 終了フラグ２
030630             PERFORM 施術記録Ｆ読込
030640             IF  ( 終了フラグ２      = SPACE   ) AND
030650                 ( 施記－患者コード  = 患者コードＷＲ ) AND
030660                 ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
030670                 ( 施記－施術年      = 施術年ＷＲ     ) AND
030680                 ( 施記－施術月      = 施術月ＷＲ     ) 
030690*
030700*        *****************************************************************
030710*        * 開始年月日 ( その部位が当月初検でないか、
030720*                       当月初検でも枝番がある時は、最初の施術日を開始日)*
030730*        *****************************************************************
030740                 IF ( 施術年Ｗ NOT = 初検年Ｗ(部位ＣＮＴ) ) OR
030750                    ( 施術月Ｗ NOT = 初検月Ｗ(部位ＣＮＴ) ) OR
030760                    ( 開始診療日手動区分Ｗ = 1 )
030770                     MOVE 施記－施術年   TO 開始年Ｗ(部位ＣＮＴ)
030780                     MOVE 施記－施術月   TO 開始月Ｗ(部位ＣＮＴ)
030790                     MOVE 施記－施術日   TO 開始日Ｗ(部位ＣＮＴ)
030800                 END-IF
030810             END-IF
030820             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
030830                           ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
030840                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
030850                           ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
030860                           ( 施記－施術月     NOT = 施術月ＷＲ       ) OR
030870                           ( 施記－施術日         > 終了日Ｗ(部位ＣＮＴ))
030920                MOVE 施記－施術年               TO 終了年ＷＴ
030930                MOVE 施記－施術月               TO 終了月ＷＴ
030940                MOVE 施記－施術日               TO 終了日ＷＴ
030950*
030960                PERFORM 施術記録Ｆ読込
030970            END-PERFORM
030980        END-IF
030990*       **************************
031000*       * 継続：終了年月日セット *
031010*       **************************
031020        IF 転帰区分Ｗ(部位ＣＮＴ) = 9
031030            MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
031040            MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
031050            MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
031060        END-IF
031070        IF 終了年月日Ｗ(部位ＣＮＴ) > 受理年月日Ｗ
031080            MOVE 終了年Ｗ(部位ＣＮＴ) TO 受理年Ｗ
031090            MOVE 終了月Ｗ(部位ＣＮＴ) TO 受理月Ｗ
031100            MOVE 終了日Ｗ(部位ＣＮＴ) TO 受理日Ｗ
031110        END-IF
031120     END-PERFORM.
031130*
031140** ----- 前月初検のみかを判定 -----------*
031150*
031160*     MOVE 患者番号ＷＲ          TO 施記－患者番号.
031170*     MOVE 枝番ＷＲ              TO 施記－枝番.
031180*     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
031190*     MOVE 施術年ＷＲ            TO 施記－施術年.
031200*     MOVE 施術月ＷＲ            TO 施記－施術月.
031210*     MOVE ZERO                  TO 施記－施術日.
031220*     START 施術記録Ｆ   KEY IS >= 施記－患者コード
031230*                                  施記－施術和暦年月日
031240*     END-START.
031250*     IF 状態キー = "00"
031260*             MOVE SPACE TO 終了フラグ２
031270*             PERFORM 施術記録Ｆ読込
031280*             IF  ( 終了フラグ２      = SPACE   ) AND
031290*                 ( 施記－患者コード  = 患者コードＷＲ ) AND
031300*                 ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
031310*                 ( 施記－施術年      = 施術年ＷＲ     ) AND
031320*                 ( 施記－施術月      = 施術月ＷＲ     ) 
031330** 当月施術開始日が再検かどうか判定
031340*                 IF   施記－再検料請求 = 1
031350*                      MOVE "YES"  TO  初日再検フラグ
031360*                 END-IF
031370**
031380*             END-IF
031390*     END-IF.
031400*     IF 初日再検フラグ = "YES"
031410*        PERFORM 前月初検のみ判定
031420*     END-IF.
031430*
031440*================================================================*
031450 前月初検のみ判定 SECTION.
031460*
031470*** 前月の通院日が初検か判定 
031480     MOVE  SPACE            TO 前月フラグ.
031490     MOVE 受－患者コード    TO 施記－患者コード.
031500     MOVE 受－施術和暦      TO 施記－施術和暦.
031510     MOVE 受－施術年        TO 施記－施術年.
031520     MOVE 受－施術月        TO 施記－施術月.
031530     MOVE 1                 TO 施記－施術日.
031540     START 施術記録Ｆ   KEY IS <  施記－患者コード
031550                                  施記－施術和暦年月日
031560                                  REVERSED
031570     END-START.
031580     IF 状態キー = "00"
031590         MOVE SPACE  TO 終了フラグ２
031600         PERFORM 施術記録Ｆ読込
031610         IF ( 終了フラグ２      = SPACE  ) AND
031620            ( 施記－患者コード  = 受－患者コード ) AND
031630            ( 施記－診療区分    = 2 ) 
031640*
031650            PERFORM 前月判定
031660**** 適用１を使用
031670            IF 前月フラグ = "YES"
031680               MOVE NC"※前月初検のみ"    TO  適用１Ｗ
031690            END-IF
031700**
031710         END-IF
031720     END-IF.
031730*
031740*================================================================*
031750 前月判定  SECTION.
031760* 
031770*** 読み込んだ施術記録の年月が、前月かどうか判定 (年月の差が 1 か?)
031780      MOVE  SPACE  TO  前月フラグ.
031790      INITIALIZE  計算年月日Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
031800**
031810      MOVE 受－施術和暦    TO 終了和暦２Ｗ.
031820      MOVE 受－施術年      TO 終了年２Ｗ.
031830      MOVE 受－施術月      TO 終了月２Ｗ.
031840      MOVE 施記－施術和暦  TO 開始和暦２Ｗ.
031850      MOVE 施記－施術年    TO 開始年２Ｗ.
031860      MOVE 施記－施術月    TO 開始月２Ｗ.
031870*
031880      EVALUATE TRUE
031890       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ = 終了年２Ｗ)
031900            PERFORM  前月比較月
031910       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ NOT = 終了年２Ｗ)
031920            PERFORM  前月比較年
031930       WHEN  開始和暦２Ｗ NOT = 終了和暦２Ｗ 
031940            PERFORM  前月比較元号
031950      END-EVALUATE.
031960*
031970      IF 計算月Ｗ = 1
031980         MOVE  "YES"  TO  前月フラグ
031990      END-IF.
032000*
032010*================================================================*
032020 レセプト並び順取得 SECTION.
032030*
032040     MOVE 施術和暦ＷＲ       TO 作２－施術和暦.
032050     MOVE 施術年ＷＲ         TO 作２－施術年.
032060     MOVE 施術月ＷＲ         TO 作２－施術月.
032070     MOVE 患者コードＷＲ     TO 作２－患者コード.
032080** 助成は、助成種別をセット
032090     MOVE 助成種別ＷＲ       TO 作２－保険種別.
032100*
032110     READ 作業ファイル２
032120     NOT INVALID KEY
032130          MOVE 作２－順番    TO 総括表順番Ｗ
032140     END-READ.
032150*
032160*================================================================*
032170 長期判定取得 SECTION.
032180*
032190* ３カ月以上の長期判定は "CHOUKI" を呼ぶ. 
032200     MOVE  SPACE TO  連期間－キー.
032210     INITIALIZE      連期間－キー.
032220     MOVE 施術和暦ＷＲ  TO  連期間－施術和暦.
032230     MOVE 施術年ＷＲ    TO  連期間－施術年.
032240     MOVE 施術月ＷＲ    TO  連期間－施術月.
032250     MOVE 患者番号ＷＲ  TO  連期間－患者番号.
032260     MOVE 枝番ＷＲ      TO  連期間－枝番.
032270*
032280     CALL   "CHOUKI".
032290     CANCEL "CHOUKI".
032300*
032310**** 適用１を使用 (「前月初検のみ」がある時は、くっつける)
032320     IF 連期間－対象フラグ  = "YES"
032330        IF 適用１Ｗ  = SPACE
032340           MOVE NC"※長期施術継続理由裏面に記載"  TO 適用１Ｗ
032350        ELSE
032360           STRING 適用１Ｗ           DELIMITED BY SPACE
032370                  NC"，"             DELIMITED BY SIZE
032380                  NC"※長期施術継続理由裏面に記載"   DELIMITED BY SIZE
032390                  INTO 適用１Ｗ
032400           END-STRING
032410        END-IF
032420     END-IF.
032430*
032440*================================================================*
032450 初検日以前のデータ判定 SECTION.
032460*
032470*********************************************************************************
032480*  最初の初検日以前の当月中に施術記録レコードがあった時(治癒、中止)は、請求区分の
032490*  継続にもチェックする。(新規と継続の両方)
032500*********************************************************************************
032510** 最初の初検日を取得
032520     MOVE SPACE                 TO 初検フラグ.
032530     MOVE 患者番号ＷＲ          TO 施記－患者番号.
032540     MOVE 枝番ＷＲ              TO 施記－枝番.
032550     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
032560     MOVE 施術年ＷＲ            TO 施記－施術年.
032570     MOVE 施術月ＷＲ            TO 施記－施術月.
032580     MOVE ZERO                  TO 施記－施術日.
032590     START 施術記録Ｆ   KEY IS >= 施記－患者コード
032600                                  施記－施術和暦年月日
032610     END-START.
032620     IF 状態キー = "00"
032630         MOVE ZERO  TO 初検和暦ＷＴ
032640         MOVE ZERO  TO 初検年ＷＴ
032650         MOVE ZERO  TO 初検月ＷＴ
032660         MOVE ZERO  TO 初検日ＷＴ
032670         MOVE SPACE TO 終了フラグ２
032680         PERFORM 施術記録Ｆ読込
032690         PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
032700                       ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
032710                       ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
032720                       ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
032730                       ( 施記－施術月     NOT = 施術月ＷＲ      ) OR
032740                       ( 初検フラグ           = "YES"           ) 
032750               IF  施記－診療区分 = 2
032760                   MOVE 施記－施術和暦           TO 初検和暦ＷＴ
032770                   MOVE 施記－施術年             TO 初検年ＷＴ
032780                   MOVE 施記－施術月             TO 初検月ＷＴ
032790                   MOVE 施記－施術日             TO 初検日ＷＴ
032800                   MOVE "YES"                    TO 初検フラグ
032810               END-IF
032820               PERFORM 施術記録Ｆ読込
032830         END-PERFORM
032840     END-IF.
032850*
032860* 初検日以前のデータ判定
032870     IF 初検フラグ = "YES"
032880        MOVE 患者番号ＷＲ          TO 施記－患者番号
032890        MOVE 枝番ＷＲ              TO 施記－枝番
032900        MOVE 初検和暦ＷＴ          TO 施記－施術和暦
032910        MOVE 初検年ＷＴ            TO 施記－施術年
032920        MOVE 初検月ＷＴ            TO 施記－施術月
032930        MOVE 初検日ＷＴ            TO 施記－施術日
032940        START 施術記録Ｆ   KEY IS <  施記－患者コード
032950                                     施記－施術和暦年月日
032960                                     REVERSED
032970        END-START
032980        IF 状態キー = "00"
032990           MOVE SPACE  TO 終了フラグ２
033000           PERFORM 施術記録Ｆ読込
033010           IF ( 終了フラグ２    = SPACE        ) AND
033020              ( 施記－患者番号  = 患者番号ＷＲ ) AND
033030              ( 施記－枝番      = 枝番ＷＲ     ) AND
033040              ( 施記－施術和暦  = 初検和暦ＷＴ ) AND
033050              ( 施記－施術年    = 初検年ＷＴ   ) AND
033060              ( 施記－施術月    = 初検月ＷＴ   )
033070*  初検日以前の当月中に施術記録レコードがあった時
033080                IF 継続チェックＷ = SPACE
033090                   MOVE NC"○"    TO 継続チェックＷ
033100                END-IF
033110           END-IF
033120         END-IF
033130     END-IF.
033140*
033150*================================================================*
033160 初検加算時刻取得 SECTION.
033170*****************************************************************
033180** 初検加算が時間外と深夜の時、適用に「受付時間」を印字する。
033190**   時刻の印字は月3回まで可能
033200*****************************************************************
033210     IF ( レセ－時間外 = 1 ) OR ( レセ－深夜 = 1 ) OR ( レセ－休日 = 1 )
033220*
033230         MOVE 患者番号ＷＲ          TO 施記－患者番号
033240         MOVE 枝番ＷＲ              TO 施記－枝番
033250         MOVE 施術和暦ＷＲ          TO 施記－施術和暦
033260         MOVE 施術年ＷＲ            TO 施記－施術年
033270         MOVE 施術月ＷＲ            TO 施記－施術月
033280         MOVE ZERO                  TO 施記－施術日
033290         START 施術記録Ｆ   KEY IS >= 施記－患者コード
033300                                      施記－施術和暦年月日
033310         END-START
033320         IF 状態キー = "00"
033330             MOVE ZERO  TO 初検加算カウント
033340             MOVE SPACE TO 終了フラグ２
033350             PERFORM 施術記録Ｆ読込
033360             PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
033370                           ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
033380                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
033390                           ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
033400                           ( 施記－施術月     NOT = 施術月ＷＲ      ) 
033410                   IF  ( 施記－初検加算 = 1 OR 2 OR 3 ) AND ( 施記－診療区分 = 2 )
033420                       COMPUTE 初検加算カウント = 初検加算カウント  + 1
033430                       IF  初検加算カウント <= 3
033440                           MOVE 施記－初検加算 TO 初検加算区分ＷＴ(初検加算カウント)
033450                           MOVE 施記－受付時   TO 初検加算時ＷＴ(初検加算カウント)
033460                           MOVE 施記－受付分   TO 初検加算分ＷＴ(初検加算カウント)
033470                       END-IF
033480                   END-IF
033490                   PERFORM 施術記録Ｆ読込
033500             END-PERFORM
033510** 初検加算の時刻を適用にセット
033380            IF ( 初検加算時ＷＴ(1) NOT = ZERO ) OR ( 初検加算分ＷＴ(1) NOT = ZERO )
                     MOVE 初検加算時ＷＴ(1) TO 初検加算時Ｗ
                     MOVE ":"               TO 初検加算区切Ｗ
                     MOVE 初検加算分ＷＴ(1) TO 初検加算分Ｗ
                  END-IF
033380            IF ( 初検加算時ＷＴ(2) NOT = ZERO ) OR ( 初検加算分ＷＴ(2) NOT = ZERO ) 
031910               PERFORM 初検加算適用セット
                  END-IF
033530         END-IF
033540*
033550     END-IF.
033560*
033570*================================================================*
033580 初検加算適用セット SECTION.
033590*
033600     PERFORM VARYING 番号カウンタ FROM 1 BY 1
033610              UNTIL  番号カウンタ > 3
033620         IF ( 初検加算時ＷＴ(番号カウンタ)  = ZERO )  AND 
033630            ( 初検加算分ＷＴ(番号カウンタ)  = ZERO ) 
033640             CONTINUE
033650         ELSE
033660* 固定項目
033670             EVALUATE 初検加算区分ＷＴ(番号カウンタ) 
033680             WHEN 1
033690                MOVE NC"時間外"   TO 加算内容Ｗ(番号カウンタ)
033320             WHEN 2
033330                MOVE NC"休　日"   TO 加算内容Ｗ(番号カウンタ)
033700             WHEN 3
033710                MOVE NC"深　夜"   TO 加算内容Ｗ(番号カウンタ)
033720             END-EVALUATE
033730*
033740             MOVE NC"："          TO 加算区切Ｗ(番号カウンタ)
033750             MOVE NC"時"          TO 時固定Ｗ(番号カウンタ)
033760             MOVE NC"分"          TO 分固定Ｗ(番号カウンタ)
033770*
033780**** 数字→日本語変換
033790* 時間
033800             MOVE 初検加算時ＷＴ(番号カウンタ)  TO  数字Ｗ
033810             IF 数字Ｗ >= 10
033820                 MOVE 数字Ｗ１    TO 負傷番号Ｗ１
033830                 PERFORM 日本語変換
033840                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ１(番号カウンタ)
033850                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
033860                 PERFORM 日本語変換
033870                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
033880             ELSE
033890                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
033900                 PERFORM 日本語変換
033910                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
033920             END-IF
033930* 分
033940             MOVE 初検加算分ＷＴ(番号カウンタ)  TO  数字Ｗ
033950             MOVE 数字Ｗ１    TO 負傷番号Ｗ１
033960             PERFORM 日本語変換
033970             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ１(番号カウンタ)
033980             MOVE 数字Ｗ２    TO 負傷番号Ｗ１
033990             PERFORM 日本語変換
034000             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ２(番号カウンタ)
034010** 
034020        END-IF
034030     END-PERFORM.
034040*
034050     MOVE  初検加算集団ＮＷ(1)   TO 初検加算時刻１Ｗ. 
034060     MOVE  初検加算集団ＮＷ(2)   TO 初検加算時刻２Ｗ. 
034070     MOVE  初検加算集団ＮＷ(3)   TO 初検加算時刻３Ｗ. 
034080*
034090**** 適用１か２を使用（長期理由記載で適用１を使っている時は、適用２）
034100     IF ( 初検加算時ＷＴ(2)  = ZERO ) AND ( 初検加算分ＷＴ(2)  = ZERO ) 
034110         CONTINUE
034120     ELSE
034130         IF 適用１Ｗ  = SPACE
034140               STRING NC"初検加算"       DELIMITED BY SIZE
034150                      初検加算時刻１Ｗ   DELIMITED BY SIZE
034160                      初検加算時刻２Ｗ   DELIMITED BY SIZE
034170                      初検加算時刻３Ｗ   DELIMITED BY SIZE
034180                      INTO 適用１Ｗ
034190               END-STRING
034200         ELSE
034210               STRING NC"初検加算"       DELIMITED BY SIZE
034220                      初検加算時刻１Ｗ   DELIMITED BY SIZE
034230                      初検加算時刻２Ｗ   DELIMITED BY SIZE
034240                      初検加算時刻３Ｗ   DELIMITED BY SIZE
034250                      INTO 適用２Ｗ
034260               END-STRING
034270         END-IF
034280     END-IF.
034290*
034300*================================================================*
034310 日本語変換 SECTION.
034320*
034330     MOVE NC"０"     TO 全角負傷番号Ｗ.
034340     CALL "htoz" WITH C LINKAGE
034350                        USING 負傷番号Ｗ１ 全角負傷番号Ｗ１.
034360*
034370*================================================================*
034380 給付割合取得 SECTION.
034390*
034400* ※ 本人負担割合ではなく、保険者の負担割合
034410*
034420*** ２７身障、被爆の時は、「老」と印字
      */後高助成の場合は「後」と印字/100413
034430     IF ( 公費種別ＷＲ NOT = ZERO )  AND
034440        ( 助成種別ＷＲ NOT = ZERO )
034450         MOVE SPACE     TO  給付割合ＷＰ
034460*         MOVE NC"老"    TO  割合固定Ｗ
034460         MOVE NC"後"    TO  後印字Ｗ
034470     ELSE
034500         MOVE レセ－給付割合   TO  給付割合ＷＰ
034510*         MOVE NC"割"          TO  割合固定Ｗ
034520     END-IF.
034530*
034540*================================================================*
034550 負担割合取得 SECTION.
034560*
034570* ※ 本人負担割合ではなく、保険者の負担率
034580*
      */後高助成の時も負担率を印字する。(負担率取得PGを使用)/100413
           IF 受－施術和暦年月 >= 41410
               MOVE レセ－負担割合 TO 負担割合Ｗ
040726         MOVE NC"割" TO 割合固定Ｗ
           ELSE
034590         IF ( 公費種別ＷＲ NOT = ZERO )  AND
034600            ( 助成種別ＷＲ NOT = ZERO )
034610             MOVE SPACE     TO  割合固定Ｗ
034620             MOVE ZERO      TO  負担割合Ｗ
034630         ELSE
                  MOVE レセ－負担割合 TO 負担割合Ｗ
040726            MOVE NC"割"        TO  割合固定Ｗ
034670         END-IF
034670     END-IF.
034680*
034690*================================================================*
034700 助成印取得 SECTION.
034710*
034720*****************************************
034730*  助成がある時、助成種別を印字する。
034740*****************************************
034750*
034760     EVALUATE 助成種別ＷＲ 
034770*** 生保 (生保はその他扱いで、該当なし)
034780     WHEN  50
034790         CONTINUE
034800*** 41老人
034810     WHEN  51
034820*********** 頭4桁が "4113"東京 "4108"茨城 "4132"島根 の時は、「福」。それ以外は「老」
034830        IF  ( 印刷市町村番号Ｗ(1:4) = "4113" )  OR
034840            ( 印刷市町村番号Ｗ(1:4) = "4108" )  OR
034850            ( 印刷市町村番号Ｗ(1:4) = "4132" )  
034860            MOVE NC"福"    TO 助成印Ｗ
034870        ELSE
034880            MOVE NC"老"    TO 助成印Ｗ
034890        END-IF
034900*** 母子
034910     WHEN  52
034920            MOVE NC"母"    TO 助成印Ｗ
034930***            MOVE NC"親"    TO 助成印Ｗ
034940*** 身障
034950     WHEN  53
034960            MOVE NC"障"    TO 助成印Ｗ
034970*** 被爆
034980     WHEN  54
034990            MOVE NC"爆"    TO 助成印Ｗ
035000*** 乳幼児 
035010     WHEN  55
035020            MOVE NC"乳"    TO 助成印Ｗ
035030*** その他
035040     WHEN  60
035050            CONTINUE
035060     WHEN  OTHER
035070            CONTINUE
035080     END-EVALUATE.
      *
      */助成印が空白の場合にJOSEIMEIでセットする/181204
           IF 助成印Ｗ = SPACE
033913         MOVE SPACE TO  連助成名称－キー
033914         INITIALIZE     連助成名称－キー
033915         MOVE 助成種別ＷＲ     TO 連助成名称－助成種別
033916         MOVE 印刷市町村番号Ｗ TO 連助成名称－費用負担者番号助成
033917*
033918         CALL   "JOSEIMEI"
033919         CANCEL "JOSEIMEI"
033920*
033921         MOVE 連助成名称－１文字 TO 助成印Ｗ
           END-IF.
035090*
035100*================================================================*
035110 基本料取得 SECTION.
035120*
035130     MOVE 01                TO 料Ａ－区分コード.
035140     MOVE ZERO              TO 料Ａ－負傷種別.
035150     MOVE ZERO              TO 料Ａ－部位.
035160     MOVE ZERO              TO 料Ａ－左右区分.
035170     MOVE ZERO              TO 料Ａ－負傷位置番号.
035180     MOVE 施術和暦ＷＲ      TO 料Ａ－開始和暦.
035190     MOVE 施術年ＷＲ        TO 料Ａ－開始年.
035200     MOVE 施術月ＷＲ        TO 料Ａ－開始月.
035210     START 料金マスタ KEY IS <= 料－区分コード
035220                                料－部位コード
035230                                料－開始和暦年月
035240                                REVERSED
035250     END-START.
035260     READ 料金マスタ NEXT
035270     NOT AT END
035280         MOVE 料Ａ－冷罨法料          TO 冷罨法単価Ｗ
035290         MOVE 料Ａ－温罨法料          TO 温罨法単価Ｗ
035300         MOVE 料Ａ－電療料            TO 電療単価Ｗ
035310     END-READ.
035320*
035330*================================================================*
035340 施術西暦年取得 SECTION.
035350*
035360     MOVE 施術和暦ＷＲ TO 元－元号区分.
035370     READ 元号マスタ
035380     NOT INVALID KEY
035390         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
035400     END-READ.
035410     IF 施術西暦年Ｗ NOT = ZERO
035420        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
035430     END-IF.
035440     MOVE 施術西暦年Ｗ  TO レセプト管理年Ｗ.
035450*
035460*================================================================*
035470 委任年月日取得 SECTION.
035480*
035490** ---// ここの受理年には、最終通院日が入っている為、退避する //----
035500     MOVE 受理年Ｗ   TO 最終通院年Ｗ.
035510     MOVE 受理月Ｗ   TO 最終通院月Ｗ.
035520     MOVE 受理日Ｗ   TO 最終通院日Ｗ.
035530***
035540* (柔整師側)
035550     EVALUATE レセプト日付区分Ｗ 
035560*    /  最終通院日 /
035570     WHEN ZERO
035580         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
035590         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
035600         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
035610*    /  月末日 /
035620     WHEN 1 
035630         PERFORM 月末日取得
035640         MOVE 受理年Ｗ     TO 柔整師年Ｗ
035650         MOVE 受理月Ｗ     TO 柔整師月Ｗ
035660         MOVE 受理日Ｗ     TO 柔整師日Ｗ
035670*    /  印字なし /
035680     WHEN 9
035690         MOVE ZERO         TO 柔整師年Ｗ
035700         MOVE ZERO         TO 柔整師月Ｗ
035710         MOVE ZERO         TO 柔整師日Ｗ
035720*    /  その他は、最終通院日 /
035730     WHEN OTHER
035740         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
035750         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
035760         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
035770     END-EVALUATE.
035780**
035790* (患者側)
035800     EVALUATE レセプト患者日付区分Ｗ 
035810*    /  最終通院日 /
035820     WHEN ZERO
035830         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
035840         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
035850         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
035860*    /  月末日 /
035870     WHEN 1 
035880         PERFORM 月末日取得
035890         MOVE 受理年Ｗ     TO 患者委任年Ｗ
035900         MOVE 受理月Ｗ     TO 患者委任月Ｗ
035910         MOVE 受理日Ｗ     TO 患者委任日Ｗ
035920*    /  印字なし /
035930     WHEN 9
035940         MOVE ZERO         TO 患者委任年Ｗ
035950         MOVE ZERO         TO 患者委任月Ｗ
035960         MOVE ZERO         TO 患者委任日Ｗ
035970*    /  その他は、最終通院日 /
035980     WHEN OTHER
035990         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
036000         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
036010         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
036020     END-EVALUATE.
036030*
036040*================================================================*
036050 月末日取得 SECTION.
036060*
036070     MOVE 施術年ＷＲ   TO 受理年Ｗ.
036080     MOVE 施術月ＷＲ   TO 受理月Ｗ.
036090     MOVE 施術和暦ＷＲ TO 元－元号区分.
036100     READ 元号マスタ
036110     NOT INVALID KEY
036120         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
036130     END-READ.
036140     IF 施術西暦年Ｗ NOT = ZERO
036150        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
036160     END-IF.
036170*
036180     EVALUATE 施術月ＷＲ
036190     WHEN 4
036200     WHEN 6
036210     WHEN 9
036220     WHEN 11
036230         MOVE 30 TO 受理日Ｗ
036240     WHEN 2
036250         DIVIDE 4 INTO 施術西暦年Ｗ GIVING    商Ｗ
036260                                    REMAINDER 余Ｗ
036270         END-DIVIDE
036280         IF 余Ｗ = ZERO
036290             MOVE 29 TO 受理日Ｗ
036300         ELSE
036310             MOVE 28 TO 受理日Ｗ
036320         END-IF
036330     WHEN 1
036340     WHEN 3
036350     WHEN 5
036360     WHEN 7
036370     WHEN 8
036380     WHEN 10
036390     WHEN 12
036400         MOVE 31 TO 受理日Ｗ
036410     WHEN OTHER
036420          CONTINUE
036430     END-EVALUATE.
036440*
036450*================================================================*
036460 負傷原因取得 SECTION.
036470*
036480********************************************************************
036490*  負傷原因コードが同じものは、1行にまとめて印字する。
036500*  例: ①② 家で転んだ.
036510*     負傷原因コードが同じものをまとめ、テーブルにセット
036520*     (ただし、部位を飛んで同じものは、2行になる)
036530********************************************************************
036540     MOVE  ZERO   TO  カウンタ カウンタ２.
036550     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
036560             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
036570*
036580****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
036590        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
036600*
036610           IF カウンタ = ZERO
036620               MOVE 1   TO  カウンタ カウンタ２
036630               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
036640               MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
036650               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
036660           ELSE
036670              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
036680                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
036690                 COMPUTE カウンタ２ = カウンタ２  +  1
036700                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
036710              ELSE
036720                 COMPUTE カウンタ = カウンタ  +  1
036730                 MOVE 1   TO  カウンタ２
036740                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
036750                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
036760                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
036770              END-IF
036780           END-IF
036790        END-IF
036800     END-PERFORM.
036810**************************************************************************
036820*  負傷原因マスタより文章取得
036830**************************************************************************
036840     MOVE  ZERO   TO  カウンタ カウンタ２.
036850     PERFORM VARYING カウンタ FROM 1 BY 1
036860             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
036870** 健保は 区分 01
036880         MOVE 01                        TO 負原－区分コード
036890         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
036900         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
036910         READ 負傷原因Ｆ
036920         NOT INVALID KEY
036930             INITIALIZE 負傷原因ＷＴ
036940             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
036950             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
036960             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
036970             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
036980             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
036990             PERFORM VARYING カウンタ２ FROM 1 BY 1
037000                     UNTIL ( カウンタ２ > 9 )  OR 
037010                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
037020                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
037030                WHEN 1
037040                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
037050                WHEN 2
037060                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
037070                WHEN 3
037080                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
037090                WHEN 4
037100                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
037110                WHEN 5
037120                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
037090                WHEN 6
037100                   MOVE "⑥"  TO  負傷原因ナンバーＷ１(カウンタ２)
037110                WHEN 7
037120                   MOVE "⑦"  TO  負傷原因ナンバーＷ１(カウンタ２)
037130                WHEN OTHER
037140                   CONTINUE
037150                END-EVALUATE
037160             END-PERFORM
037161*
037162             IF 負原－負傷原因入力区分 = 1
037163                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
037164                        負傷原因１ＷＴ  DELIMITED BY SIZE
037165                        負傷原因２ＷＴ  DELIMITED BY SIZE
037166                        負傷原因３ＷＴ  DELIMITED BY SIZE
037167                        負傷原因４ＷＴ  DELIMITED BY SIZE
037168                        負傷原因５ＷＴ  DELIMITED BY SIZE
037169                        INTO 負傷原因内容合成Ｗ(カウンタ)
037170                 END-STRING
037171             ELSE
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
037180             END-IF
037181*
037260         END-READ
037270     END-PERFORM.
037280*
037290     PERFORM 負傷原因セット.
037300*
037310*================================================================*
037320 負傷原因セット SECTION.
037330*
037340**************************************************************************
037350*  文章が1行を超える時は、複数行に分解する。
037360**************************************************************************
037370     MOVE  ZERO   TO  カウンタ カウンタ２.
037380     PERFORM VARYING カウンタ FROM 1 BY 1
037390             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
037400*
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
037550*
037560     END-PERFORM.
037570*================================================================*
037580 長期理由文取得 SECTION.
037590*
037600* 長期理由文取得は "CHOUBUN" を呼ぶ. 
037610     MOVE  SPACE TO  連長文－キー.
037620     INITIALIZE      連長文－キー.
037630     MOVE 施術和暦ＷＲ  TO  連長文－施術和暦.
037640     MOVE 施術年ＷＲ    TO  連長文－施術年.
037650     MOVE 施術月ＷＲ    TO  連長文－施術月.
037660     MOVE 患者番号ＷＲ  TO  連長文－患者番号.
037670     MOVE 枝番ＷＲ      TO  連長文－枝番.
037680** 日接用は56桁
037690     MOVE 56            TO  連長文－文桁数.
037700*
037710     CALL   "CHOUBUN".
037720     CANCEL "CHOUBUN".
037730*
037740*================================================================*
037750*================================================================*
037760 施術記録Ｆ読込 SECTION.
037770*
037780     READ 施術記録Ｆ NEXT
037790     AT END
037800         MOVE "YES" TO 終了フラグ２
037810     END-READ.
037820*================================================================*
037830 前月比較月  SECTION.
037840*
037850     IF  終了月２Ｗ >  開始月２Ｗ
037860         COMPUTE 計算月Ｗ = 終了月２Ｗ - 開始月２Ｗ
037870     ELSE
037880        MOVE ZERO TO 計算月Ｗ
037890     END-IF.
037900*
037910*================================================================*
037920 前月比較年  SECTION.
037930*
037940     IF  終了年２Ｗ >  開始年２Ｗ
037950         COMPUTE 計算年Ｗ = 終了年２Ｗ - 開始年２Ｗ
037960         COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
037970     ELSE
037980        MOVE ZERO TO 計算月Ｗ
037990     END-IF.
038000*
038010*================================================================*
038020 前月比較元号  SECTION.
038030*
038040     MOVE 開始和暦２Ｗ TO 元－元号区分.
038050     READ 元号マスタ
038060     NOT INVALID KEY
038070         MOVE 元－開始西暦年 TO 開始西暦年Ｗ
038080     END-READ.
038090     MOVE 終了和暦２Ｗ TO 元－元号区分.
038100     READ 元号マスタ
038110     NOT INVALID KEY
038120         MOVE 元－開始西暦年 TO 終了西暦年Ｗ
038130     END-READ.
038140**
038150     IF (開始西暦年Ｗ NOT = ZERO) AND (終了西暦年Ｗ NOT = ZERO)
038160        COMPUTE 開始西暦年Ｗ = 開始西暦年Ｗ + 開始年２Ｗ - 1
038170        COMPUTE 終了西暦年Ｗ = 終了西暦年Ｗ + 終了年２Ｗ - 1
038180*
038190        IF 終了西暦年Ｗ =  開始西暦年Ｗ
038200           PERFORM  前月比較月
038210        ELSE
038220           IF  終了西暦年Ｗ >  開始西暦年Ｗ
038230               COMPUTE 計算年Ｗ = 終了西暦年Ｗ - 開始西暦年Ｗ
038240               COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
038250           ELSE
038260               MOVE ZERO TO 計算月Ｗ
038270           END-IF
038280        END-IF
038290     ELSE
038300        MOVE ZERO TO 計算月Ｗ
038310     END-IF.
038320*
038330*================================================================*
038340 印刷処理 SECTION.
038350*
      */会員番号等印刷/110720
041530        MOVE "YHP6425P"  TO  定義体名Ｐ
041540        MOVE "GRP002"   TO  項目群名Ｐ
041550        WRITE YHP6425P
041570        PERFORM エラー処理Ｐ
      *
038360     MOVE "YHP6425P"  TO  定義体名Ｐ.
038370     MOVE "SCREEN"   TO  項目群名Ｐ.
038380     WRITE YHP6425P.
038390****     WRITE 印刷レコード.
038400     PERFORM エラー処理Ｐ.
038410*================================================================*
038420 エラー処理Ｐ SECTION.
038430*
038440     IF 通知情報Ｐ NOT = "00"
038450         DISPLAY NC"帳票エラー"              UPON CONS
038460         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
038470         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
038480         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
038490         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
038500                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
038510         ACCEPT  キー入力 FROM CONS
038520         PERFORM ファイル閉鎖
038530         MOVE 99  TO PROGRAM-STATUS
038540         EXIT PROGRAM
038550     END-IF.
038560*================================================================*
038570 受診者印刷区分更新 SECTION.
038580*
038590** //  受診者情報Ｆの印刷区分に１をセットし、更新する。//  
038600*
038610     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
038620     MOVE 施術年ＷＲ         TO 受－施術年.
038630     MOVE 施術月ＷＲ         TO 受－施術月.
038640     MOVE 患者コードＷＲ     TO 受－患者コード.
038650     READ 受診者情報Ｆ
038660     NOT INVALID KEY
038670         MOVE  1  TO  受－レセ印刷区分助成
038680         REWRITE  受－レコード
038690         END-REWRITE
038700         IF 状態キー NOT = "00"
038710            MOVE NC"受診者" TO ファイル名
038720            PERFORM エラー表示
038730         END-IF
038740     END-READ.
038750*
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
038751*================================================================*
038752 レセ摘要再セット SECTION.
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
038770*
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
038771*================================================================*
038772*================================================================*
038773 エラー表示 SECTION.
038780*
038790     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
038800     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
038810     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
038820     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
038830     ACCEPT  キー入力 FROM CONS
038840     PERFORM ファイル閉鎖.
038850     EXIT PROGRAM.
038860*================================================================*
038870 終了処理 SECTION.
038880*
038890     PERFORM ファイル閉鎖.
038900*================================================================*
038910 ファイル閉鎖 SECTION.
038920*
038930     CLOSE 印刷ファイル.
038940     CLOSE 元号マスタ       名称マスタ
038950           レセプトＦ       制御情報マスタ      施術所情報マスタ
038960           経過マスタ       受診者情報Ｆ        市町村マスタ
038970           施術記録Ｆ       負傷データＦ        負傷原因Ｆ
038980           料金マスタ       請求先マスタ        ＩＤ管理マスタ
038990           会情報マスタ     作業ファイル２      受診者情報２Ｆ.
039000*
039010*================================================================*
039020*================================================================*
039030 テスト印字処理 SECTION.
039040*
039050     MOVE ALL "X"    TO 県施術ＩＤ.
039060     MOVE ALL NC"Ｎ" TO 県共済固定.
039070*     MOVE ALL NC"Ｎ" TO 助成印.
039080     MOVE 99         TO 施術年 施術月.
039090*     MOVE ALL NC"Ｎ" TO 記号.
039100*     MOVE ALL "X"    TO 番号.
039110     MOVE ALL "X"    TO 保険者番号 公費負担者番号 受給者番号.
039120*     MOVE ALL NC"Ｎ" TO 保険種別 割合固定.
039130*     MOVE 99         TO 負担割合.
039140     MOVE ALL "X"    TO 住所１ 住所２.
039150*     MOVE ALL "X"    TO 受給者カナ.
039160     MOVE ALL "Ｎ" TO 患者氏名.
039170*     MOVE "(男)"     TO 患者性別.
039180*     MOVE ALL NC"Ｎ" TO 生年月日固定  元号.
      *受給者元号
039190*     MOVE 99         TO 受給者年 受給者月 受給者日 患者年 患者月 患者日.
039200     MOVE ALL "M"    TO 負傷原因１ 負傷原因２ 負傷原因３ 負傷原因４
                              負傷原因５ 負傷原因６.
039220     MOVE ALL NC"Ｎ" TO 負傷名１.
039230     MOVE 99 TO 負傷年１ 負傷月１ 負傷日１ 初検年１ 初検月１ 初検日１
039240                開始年１ 開始月１ 開始日１ 終了年１ 終了月１ 終了日１
039250                実日数１.
039260     MOVE NC"○" TO 治癒チェック１ 中止チェック１ 転医チェック１.
039270     MOVE ALL NC"Ｎ" TO 負傷名２.
039280     MOVE 99 TO 負傷年２ 負傷月２ 負傷日２ 初検年２ 初検月２ 初検日２
039290                開始年２ 開始月２ 開始日２ 終了年２ 終了月２ 終了日２
039300                実日数２.
039310     MOVE NC"○" TO 治癒チェック２ 中止チェック２ 転医チェック２.
039320     MOVE ALL NC"Ｎ" TO 負傷名３.
039330     MOVE 99 TO 負傷年３ 負傷月３ 負傷日３ 初検年３ 初検月３ 初検日３
039340                開始年３ 開始月３ 開始日３ 終了年３ 終了月３ 終了日３
039350                実日数３.
039360     MOVE NC"○" TO 治癒チェック３ 中止チェック３ 転医チェック３.
039370     MOVE ALL NC"Ｎ" TO 負傷名４.
039380     MOVE 99 TO 負傷年４ 負傷月４ 負傷日４ 初検年４ 初検月４ 初検日４
039390                開始年４ 開始月４ 開始日４ 終了年４ 終了月４ 終了日４
039400                実日数４.
039410     MOVE NC"○" TO 治癒チェック４ 中止チェック４ 転医チェック４.
039420     MOVE ALL NC"Ｎ" TO 負傷名５.
039430     MOVE 99 TO 負傷年５ 負傷月５ 負傷日５ 初検年５ 初検月５ 初検日５
039440                開始年５ 開始月５ 開始日５ 終了年５ 終了月５ 終了日５
039450                実日数５.
039460     MOVE NC"○" TO 治癒チェック５ 中止チェック５ 転医チェック５.
039470     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
039480             UNTIL ( 部位ＣＮＴ > 5 )
039490         MOVE ALL NC"Ｎ" TO 経過略称(部位ＣＮＴ)
039500     END-PERFORM.
039510     MOVE NC"○" TO 新規チェック 継続チェック.
039520     MOVE 99999 TO  初検料.
039530     MOVE 99999 TO  再検料.
039540     MOVE 99.9 TO  往療距離.
039550     MOVE 99 TO  往療回数.
039560     MOVE 99999 TO  往療料.
039570     MOVE NC"○" TO  大チェック 中チェック 小チェック.
039580     MOVE 99999 TO  金属副子加算料.
039590     MOVE 999999 TO  小計.
039600     MOVE NC"○" TO  時間外チェック 休日チェック 深夜チェック.
039610     MOVE 99999 TO  初検加算料.
039620     MOVE NC"○" TO  夜間チェック 難路チェック 暴風雨雪チェック.
039630     MOVE 99999 TO  往療加算料.
039640     MOVE 99999 TO  施術情報提供料.
039650     MOVE NC"○" TO 整復料チェック 固定料チェック 施療料チェック.
039660     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
039670             UNTIL ( 部位ＣＮＴ > 5 )
039680         MOVE 99999 TO 初回処置料(部位ＣＮＴ)
039690     END-PERFORM.
039700     MOVE 999999 TO 初回処置料合計.
039710     MOVE 99    TO 後療回数１ 冷罨法回数１ 温罨法回数１ 電療回数１.
039720     MOVE 9999  TO 後療単価１ 冷罨法料１   温罨法料１   電療料１.
039730     MOVE 99999 TO 後療料１   小計１       長期込小計１.
039740     MOVE 9.9 TO 長期逓減率１.
039750     MOVE 99 TO 後療回数２ 冷罨法回数２ 温罨法回数２ 電療回数２.
039760     MOVE 9999  TO 後療単価２ 冷罨法料２   温罨法料２   電療料２.
039770     MOVE 99999 TO 後療料２   小計２       長期込小計２.
039780     MOVE 9.9 TO 長期逓減率２.
039790     MOVE 99 TO 後療回数３８ 冷罨法回数３８ 温罨法回数３８ 電療回数３８.
039800     MOVE 9999  TO 後療単価３８ 冷罨法料３８   温罨法料３８   電療料３８.
039810     MOVE 99999 TO 後療料３８ 小計３８ 長期込小計３８ 多部位込小計３８.
039820     MOVE 9.9 TO 長期逓減率３８.
039830     MOVE 99 TO 逓減開始月３０ 逓減開始日３０.
039840     MOVE 99 TO 後療回数３０ 冷罨法回数３０ 温罨法回数３０ 電療回数３０.
039850     MOVE 9999  TO 後療単価３０ 冷罨法料３０   温罨法料３０   電療料３０.
039860     MOVE 99999 TO 後療料３０ 小計３０ 長期込小計３０.
039870     MOVE 9.9 TO 長期逓減率３０.
039880*     MOVE 99 TO 後療回数４５ 冷罨法回数４５ 温罨法回数４５ 電療回数４５.
039890*     MOVE 9999  TO 後療単価４５ 冷罨法料４５   温罨法料４５   電療料４５.
039900*     MOVE 99999 TO 後療料４５ 小計４５ 長期込小計４５ 多部位込小計４５.
039910*     MOVE 9.9 TO 長期逓減率４５.
039920     MOVE 99 TO 逓減開始月４８ 逓減開始日４８.
039930     MOVE 99 TO 後療回数４８ 冷罨法回数４８ 温罨法回数４８ 電療回数４８.
039940     MOVE 9999  TO 後療単価４８ 冷罨法料４８   温罨法料４８   電療料４８.
039950     MOVE 99999 TO 後療料４８ 小計４８ 長期込小計４８ 多部位込小計４８.
039960     MOVE 9.9 TO 長期逓減率４８.
039970     MOVE 99 TO 逓減開始月４０ 逓減開始日４０.
039980     MOVE 99 TO 後療回数４０ 冷罨法回数４０ 温罨法回数４０ 電療回数４０.
039990     MOVE 9999  TO 後療単価４０ 冷罨法料４０   温罨法料４０   電療料４０.
040000     MOVE 99999 TO 後療料４０ 小計４０ 長期込小計４０.
040010     MOVE 9.9 TO 長期逓減率４０.
040090     MOVE ALL "X" TO 部位５０ 部位５８.
040020*     MOVE NC"※５部位目請求あり" TO 部位５適用.
040030     MOVE ALL NC"Ｎ" TO 適用１ 適用２.
040040     MOVE ALL "Ｎ" TO 長期理由文１ 長期理由文２ 長期理由文３
040050                        長期理由文４ 長期理由文５ 長期理由文６.
040060     MOVE 999999 TO 合計 請求金額.
040070     MOVE 999999 TO 受給者負担額 助成請求額.
040080*     MOVE ALL "X"    TO 保険者名称１ 保険者名称２.
040090     MOVE ALL "X" TO 柔整師番号.
040100     MOVE 99 TO 受理年 受理月 受理日.
040110     MOVE 99 TO 委任年 委任月 委任日.
040120     MOVE 999  TO 施術所郵便番号１.
040130     MOVE 9999 TO 施術所郵便番号２.
040140     MOVE ALL "X" TO 施術所住所１.
040150     MOVE ALL "Ｎ" TO 接骨院名.
040160     MOVE ALL "X" TO 代表者カナ.
040170     MOVE ALL "Ｎ" TO 代表者名.
040180     MOVE ALL "X" TO 施術所電話番号.
040190*
040200*================================================================*
       施術日取得 SECTION.
      *
      *     MOVE SPACE TO 施術日Ｗ.
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
040210******************************************************************
040220 END PROGRAM YHP6425.
040230******************************************************************
