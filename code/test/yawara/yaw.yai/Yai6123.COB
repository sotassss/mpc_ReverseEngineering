000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAI6123.
000060 AUTHOR.                 山田浩之
000070*
000080*----------------------------------------------------------------*
000090*         日整愛知       レセプト印刷
000100*                       （ 一般レセプト）
000110*         MED = YAW610 YAI6123P
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2022-10-28
000140 DATE-COMPILED.          2022-10-28
      */金属副子・運動後療の変更・追加/1805
      */明細書発行加算を適用２に追加/2022
      */新用紙対応/2022.10
      */2024.10  長期頻回を適用に追加/2407
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
000530     SELECT  料金マスタ      ASSIGN      TO        RYOUKINL
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS MODE              IS  DYNAMIC
000560                             RECORD KEY               IS  料－区分コード
000570                                                          料－部位コード
000580                                                          料－開始和暦年月.
000590     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000600                             ORGANIZATION             IS  INDEXED
000610                             ACCESS MODE              IS  DYNAMIC
000620                             RECORD KEY               IS  制－制御区分
000630                             FILE STATUS              IS  状態キー
000640                             LOCK        MODE         IS  AUTOMATIC.
000650     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000660                             ORGANIZATION             IS  INDEXED
000670                             ACCESS MODE              IS  DYNAMIC
000680                             RECORD KEY               IS  施情－施術所番号
000690                             FILE STATUS              IS  状態キー
000700                             LOCK        MODE         IS  AUTOMATIC.
000710     SELECT  保険者マスタ    ASSIGN      TO        HOKENSL
000720                             ORGANIZATION             IS  INDEXED
000730                             ACCESS MODE              IS  DYNAMIC
000740                             RECORD KEY               IS  保－保険種別
000750                                                          保－保険者番号
000760                             ALTERNATE RECORD KEY     IS  保－保険種別
000770                                                          保－保険者名称
000780                                                          保－保険者番号
000790                             FILE STATUS              IS  状態キー
000800                             LOCK        MODE         IS  AUTOMATIC.
000810     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
000820                             ORGANIZATION             IS  INDEXED
000830                             ACCESS MODE              IS  DYNAMIC
000840                             RECORD KEY               IS  市－公費種別
000850                                                          市－市町村番号
000860                             ALTERNATE RECORD KEY     IS  市－公費種別
000870                                                          市－市町村名称
000880                                                          市－市町村番号
000890                             FILE STATUS              IS  状態キー
000900                             LOCK        MODE         IS  AUTOMATIC.
000910     SELECT  請求先マスタ    ASSIGN      TO        SEIKYUSL
000920                             ORGANIZATION             IS  INDEXED
000930                             ACCESS MODE              IS  DYNAMIC
000940                             RECORD KEY               IS  請先－保険種別
000950                                                          請先－保険者番号
000960                             FILE STATUS              IS  状態キー
000970                             LOCK    MODE             IS  AUTOMATIC.
000980     SELECT  ＩＤ管理マスタ  ASSIGN      TO        IDKANRL
000990                             ORGANIZATION             IS  INDEXED
001000                             ACCESS MODE              IS  DYNAMIC
001010                             RECORD KEY               IS  ＩＤ管－ＩＤ区分
001020                                                          ＩＤ管－施術所番号
001030                                                          ＩＤ管－保険種別
001040                                                          ＩＤ管－保険者番号
001050                             ALTERNATE RECORD KEY     IS  ＩＤ管－施術ＩＤ番号
001060                                                          ＩＤ管－ＩＤ区分
001070                                                          ＩＤ管－施術所番号
001080                                                          ＩＤ管－保険種別
001090                                                          ＩＤ管－保険者番号
001100                             FILE STATUS              IS  状態キー
001110                             LOCK        MODE         IS  AUTOMATIC.
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
001120     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
001130                             ORGANIZATION             IS  INDEXED
001140                             ACCESS MODE              IS  DYNAMIC
001150                             RECORD KEY               IS  受－施術和暦年月
001160                                                          受－患者コード
001170                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001180                                                          受－患者カナ
001190                                                          受－患者コード
001200                             ALTERNATE RECORD KEY     IS  受－患者コード
001210                                                          受－施術和暦年月
001220                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001230                                                          受－保険種別
001240                                                          受－保険者番号
001250                                                          受－患者コード
001260                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001270                                                          受－公費種別
001280                                                          受－費用負担者番号
001290                                                          受－患者コード
001300                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001310                                                          受－助成種別
001320                                                          受－費用負担者番号助成
001330                                                          受－患者コード
001340                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
001350                                                          受－施術和暦年月
001360                                                          受－患者コード
001370                             FILE STATUS              IS  状態キー
001380                             LOCK        MODE         IS  AUTOMATIC.
001390     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
001400                             ORGANIZATION             IS  INDEXED
001410                             ACCESS MODE              IS  DYNAMIC
001420                             RECORD KEY               IS  施記－施術和暦年月日
001430                                                          施記－患者コード
001440                             ALTERNATE RECORD KEY     IS  施記－患者コード
001450                                                          施記－施術和暦年月日
001460                             FILE STATUS              IS  状態キー
001470                             LOCK        MODE         IS  AUTOMATIC.
001480     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
001490                             ORGANIZATION             IS  INDEXED
001500                             ACCESS MODE              IS  DYNAMIC
001510                             RECORD KEY               IS  負－施術和暦年月
001520                                                          負－患者コード
001530                             ALTERNATE RECORD KEY     IS  負－患者コード
001540                                                          負－施術和暦年月
001550                             FILE STATUS              IS  状態キー
001560                             LOCK        MODE         IS  AUTOMATIC.
001570     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
001580                             ORGANIZATION             IS  INDEXED
001590                             ACCESS MODE              IS  DYNAMIC
001600                             RECORD KEY               IS  負原－区分コード
001610                                                          負原－負傷原因コード
001620                             FILE STATUS              IS  状態キー
001630                             LOCK        MODE         IS  AUTOMATIC.
001780     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF002
001790                             SYMBOLIC    DESTINATION  IS  "PRT"
001800                             FORMAT                   IS  定義体名Ｐ
001810                             GROUP                    IS  項目群名Ｐ
001820                             PROCESSING  MODE         IS  処理種別Ｐ
001830                             UNIT        CONTROL      IS  拡張制御Ｐ
001840                             FILE        STATUS       IS  通知情報Ｐ.
001850******************************************************************
001860*                      DATA DIVISION                             *
001870******************************************************************
001880 DATA                    DIVISION.
001890 FILE                    SECTION.
001900*                           ［ＲＬ＝  １２８］
001910 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001920     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001930*                           ［ＲＬ＝  １２８］
001940 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
001950     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
001990*                           ［ＲＬ＝  １２８］
002000 FD  経過マスタ          BLOCK   CONTAINS   1   RECORDS.
002010     COPY KEIKA           OF  XFDLIB  JOINING   経   AS  PREFIX.
002020*
002030 FD  料金マスタ         BLOCK   CONTAINS   1   RECORDS.
002040     COPY RYOUKIN         OF  XFDLIB  JOINING   料   AS  PREFIX.
002050     COPY RYOUKNA         OF  XFDLIB  JOINING   料Ａ AS  PREFIX.
002060     COPY RYOUKNB         OF  XFDLIB  JOINING   料Ｂ AS  PREFIX.
002070     COPY RYOUKNC         OF  XFDLIB  JOINING   料Ｃ AS  PREFIX.
002080     COPY RYOUKND         OF  XFDLIB  JOINING   料Ｄ AS  PREFIX.
002090     COPY RYOUKNE         OF  XFDLIB  JOINING   料Ｅ AS  PREFIX.
002100     COPY RYOUKNF         OF  XFDLIB  JOINING   料Ｆ AS  PREFIX.
002110*                           ［ＲＬ＝  ２５６］
002120 FD  制御情報マスタ      BLOCK   CONTAINS   1   RECORDS.
002130     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
002140*                           ［ＲＬ＝  １２８］
002150 FD  施術所情報マスタ    BLOCK   CONTAINS   1   RECORDS.
002160     COPY SEJOHO          OF  XFDLIB  JOINING   施情   AS  PREFIX.
002170*                           ［ＲＬ＝  ３２０］
002180 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
002190     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
002200*                           ［ＲＬ＝  ２５６］
002210 FD  市町村マスタ        BLOCK   CONTAINS   1   RECORDS.
002220     COPY SITYOSN         OF  XFDLIB  JOINING   市   AS  PREFIX.
002230*                           ［ＲＬ＝  １２８］
002240 FD  請求先マスタ        BLOCK   CONTAINS   1   RECORDS.
002250     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
002260*                           ［ＲＬ＝  １２８］
002270 FD  ＩＤ管理マスタ          BLOCK   CONTAINS   1   RECORDS.
002280     COPY IDKANR          OF  XFDLIB  JOINING   ＩＤ管   AS  PREFIX.
002470*                           ［ＲＬ＝  ６４０］
002480 FD  会情報マスタ        BLOCK   CONTAINS   1   RECORDS.
002490     COPY KAIJOHO         OF  XFDLIB  JOINING   会情   AS  PREFIX.
002290*                           ［ＲＬ＝  ３２０］
002300 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
002310     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
002320*                           ［ＲＬ＝  ２５６］
002330 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
002340     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
002350*                           ［ＲＬ＝  １２８］
002360 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002370     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
002380*                           ［ＲＬ＝  １２８］
002390 FD  負傷原因Ｆ          BLOCK   CONTAINS   1   RECORDS.
002400     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
002440*
002450 FD  印刷ファイル.
002460     COPY YAI6123P       OF  XMDLIB.
002470*----------------------------------------------------------------*
002480******************************************************************
002490*                WORKING-STORAGE SECTION                         *
002500******************************************************************
002510 WORKING-STORAGE         SECTION.
002520 01 キー入力                           PIC X     VALUE SPACE.
002530 01 状態キー                           PIC X(2)  VALUE SPACE.
002540 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002550 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002560 01 ファイル名                         PIC N(6)  VALUE SPACE.
002570 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002580*
002590*--- 制御マスタ退避 ---*
002600 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
002610*
002620** 負傷原因・長期理由印刷区分用
002630 01 負傷原因印刷区分Ｗ                 PIC 9     VALUE ZERO.
002640 01 長期理由印刷区分Ｗ                 PIC 9     VALUE ZERO.
002650*
002660** レセ下段の日付区分用 (0:最終通院日、1:月末日、9:印字なし)
002670 01 レセプト日付区分Ｗ                 PIC 9     VALUE ZERO.
002680 01 レセプト患者日付区分Ｗ             PIC 9     VALUE ZERO.
002690*
002700*--- カウンタ ---*
002710 01 カウンタ                           PIC 9(2)  VALUE ZERO.
002720 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
002730 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002740*
002750*--- 負傷データ取得用 ---*
002760 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002770 01 部位名称Ｗ                         PIC N(12) VALUE SPACE.
002780 01 部位長Ｗ                           PIC 9(2)  VALUE 1.
002790 01 経過部位Ｗ                         PIC N(1)  VALUE SPACE.
002800*
002810** 枝番判定用
002820 01 開始診療日手動区分Ｗ               PIC 9     VALUE ZERO.
002830*
002840*--- 施術記録取得用 ---*
002850** 前月初検のみ用
002860 01 初日再検フラグ                     PIC X(3)  VALUE SPACE.
002870 01 前月フラグ                         PIC X(3)  VALUE SPACE.
002880*
002890 01 終了年月日ＷＴ.
002980    03 終了和暦ＷＴ                    PIC 9     VALUE ZERO.
002900    03 終了年ＷＴ                      PIC 9(2)  VALUE ZERO.
002910    03 終了月ＷＴ                      PIC 9(2)  VALUE ZERO.
002920    03 終了日ＷＴ                      PIC 9(2)  VALUE ZERO.
002930*
002940** 前月判定用
002950 01 計算年月日Ｗ.
002960    03 計算和暦Ｗ                      PIC 9(1)  VALUE ZERO.
002970    03 計算年Ｗ                        PIC S9(2) VALUE ZERO.
002980    03 計算月Ｗ                        PIC S9(2) VALUE ZERO.
002990    03 計算日Ｗ                        PIC S9(2) VALUE ZERO.
003000 01 開始年月日２Ｗ.
003010    03 開始和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003020    03 開始年２Ｗ                      PIC 9(2)  VALUE ZERO.
003030    03 開始月２Ｗ                      PIC 9(2)  VALUE ZERO.
003040    03 開始日２Ｗ                      PIC 9(2)  VALUE ZERO.
003050    03 開始西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003060 01 終了年月日２Ｗ.
003070    03 終了和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003080    03 終了年２Ｗ                      PIC 9(2)  VALUE ZERO.
003090    03 終了月２Ｗ                      PIC 9(2)  VALUE ZERO.
003100    03 終了日２Ｗ                      PIC 9(2)  VALUE ZERO.
003110    03 終了西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003120*
003130*--- 初検日退避用 ---*
003140 01 初検フラグ                         PIC X(3)  VALUE SPACE.
003150*
003160 01 初検年月日ＷＴ.
003170    03 初検和暦ＷＴ                    PIC 9     VALUE ZERO.
003180    03 初検年ＷＴ                      PIC 9(2)  VALUE ZERO.
003190    03 初検月ＷＴ                      PIC 9(2)  VALUE ZERO.
003200    03 初検日ＷＴ                      PIC 9(2)  VALUE ZERO.
003210*
003220*--- 初検加算時刻用 ---*
003230* 初検加算時刻用
003240 01 初検加算ＷＴ.
003250    03 初検加算カウント                PIC 9     VALUE ZERO.
003260    03 番号カウンタ                    PIC 9     VALUE ZERO.
003270    03 初検加算集団ＷＴ  OCCURS 3.
003280       05 初検加算区分ＷＴ             PIC 9     VALUE ZERO.
003290       05 初検加算時ＷＴ               PIC 9(2)  VALUE ZERO.
003300       05 初検加算分ＷＴ               PIC 9(2)  VALUE ZERO.
003310    03 初検加算集団ＮＷ  OCCURS 3.
003320       05 加算区切Ｗ                   PIC N(1)  VALUE SPACE.
003330       05 加算内容Ｗ                   PIC N(3)  VALUE SPACE.
003340       05 初検加算時ＮＷ１             PIC N(1)  VALUE SPACE.
003350       05 初検加算時ＮＷ２             PIC N(1)  VALUE SPACE.
003360       05 時固定Ｗ                     PIC N(1)  VALUE SPACE.
003370       05 初検加算分ＮＷ１             PIC N(1)  VALUE SPACE.
003380       05 初検加算分ＮＷ２             PIC N(1)  VALUE SPACE.
003390       05 分固定Ｗ                     PIC N(1)  VALUE SPACE.
003400    03 初検加算時刻１Ｗ                PIC N(10) VALUE SPACE.
003410    03 初検加算時刻２Ｗ                PIC N(10) VALUE SPACE.
003420    03 初検加算時刻３Ｗ                PIC N(10) VALUE SPACE.
003070    03 初検加算区切Ｗ                  PIC X     VALUE SPACE.
003080    03 初検加算時Ｗ                    PIC 9(2)  VALUE ZERO.
003090    03 初検加算分Ｗ                    PIC 9(2)  VALUE ZERO.
003430*
003440 01 負傷番号Ｗ                         PIC 9.
003450 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
003460    03 負傷番号Ｗ１                    PIC X.
003470*
003480 01 全角負傷番号Ｗ                     PIC N.
003490 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
003500    03 全角負傷番号Ｗ１                PIC X(2).
003510*
003520** 数字→日本語変換
003530 01 数字Ｗ                             PIC 9(2).
003540 01 数字Ｒ REDEFINES 数字Ｗ.
003550    03 数字Ｗ１                        PIC X(1).
003560    03 数字Ｗ２                        PIC X(1).
003570*
003580*--- 負傷原因用 ---*
003590 01 負傷原因ＷＴ.
003600    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
003610    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
003620    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
003630    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
003640    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
003650    03 負傷原因ナンバーＷＴ.
003660       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
003670    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
003680 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
003690 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
003700 01 負傷原因ＴＢＬ.
003710    03 負傷原因コードＴＢＬ            OCCURS 9.
003720       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
003730       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
003740       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
003750 01 負傷原因内容Ｗ.
003760    03 負傷原因内容合成Ｗ              PIC X(318) OCCURS 9 VALUE SPACE.
003620    03 負傷原因内容分解ＸＷ.
003630       05 負傷原因内容１ＸＷ           PIC X(80)  VALUE SPACE.
003640       05 負傷原因内容２ＸＷ           PIC X(80)  VALUE SPACE.
003640       05 負傷原因内容３ＸＷ           PIC X(80)  VALUE SPACE.
003650       05 負傷原因内容４ＸＷ           PIC X(78)  VALUE SPACE.
003810*
003820*--- 委任年月日用 ---*
003830 01 受理年月日Ｗ.
007350    03 受理和暦Ｗ                      PIC 9     VALUE ZERO.
003840    03 受理年Ｗ                        PIC 9(2)  VALUE ZERO.
003850    03 受理月Ｗ                        PIC 9(2)  VALUE ZERO.
003860    03 受理日Ｗ                        PIC 9(2)  VALUE ZERO.
003870 01 最終通院年月日Ｗ.
007390    03 最終通院和暦Ｗ                  PIC 9     VALUE ZERO.
003880    03 最終通院年Ｗ                    PIC 9(2)  VALUE ZERO.
003890    03 最終通院月Ｗ                    PIC 9(2)  VALUE ZERO.
003900    03 最終通院日Ｗ                    PIC 9(2)  VALUE ZERO.
003910*
003920** 月末日用
003930 01 施術西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
003940 01 商Ｗ                               PIC 9(3)  VALUE ZERO.
003950 01 余Ｗ                               PIC 9(3)  VALUE ZERO.
003960*
003970*--- 会長委任用 ---*
003980 01 会長委任フラグ                     PIC X(3)  VALUE SPACE.
003980 01 口座印字フラグ                     PIC X(3)  VALUE SPACE.
003990 01 組合委任区分Ｗ                     PIC 9     VALUE ZERO.
004000*
004010*--- 給付割合用 ---*
004020 01 負担割合Ｗ                         PIC 9(2)  VALUE ZERO.
004030 01 給付割合Ｗ                         PIC 9(2)  VALUE ZERO.
004040 01 負担率Ｗ                           PIC 9(3)  VALUE ZERO.
004050*
004060*
004070* 負傷原因印刷区分
004080 01 レセ負傷原因印刷区分Ｗ             PIC 9    VALUE ZERO.
004440 01 レセ長期理由印刷区分Ｗ             PIC 9    VALUE ZERO.
      *
      */金属副子・運動後療の変更・追加/1805
       01 金属副子ＣＭ                       PIC X(200) VALUE SPACE.
       01 運動後療ＣＭ                       PIC X(68)  VALUE SPACE.
004090*
      */長期頻回の追加/2407
       01 長期頻回Ｗ.
          03 長期頻回ＣＭ                    PIC X(280) VALUE SPACE.
          03 長期頻回１ＷＴ                  PIC X(54)  VALUE SPACE.
          03 長期頻回２ＷＴ                  PIC X(54)  VALUE SPACE.
          03 長期頻回３ＷＴ                  PIC X(54)  VALUE SPACE.
          03 長期頻回４ＷＴ                  PIC X(54)  VALUE SPACE.
          03 長期頻回５ＷＴ                  PIC X(54)  VALUE SPACE.
          03 負傷名ＷＲ                      OCCURS 5.
             05 負傷名ＷＰ                   PIC X(36)  VALUE SPACE.
       01 月数Ｗ                             PIC Z9     VALUE ZERO.
004730*
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
004100****************
004110* 連結項目待避 *
004120****************
004130*    ************
004140*    * 印刷キー *
004150*    ************
004160 01 対象データＷＲ.
004170    03 施術和暦年月ＷＲ.
004180       05 施術和暦ＷＲ                 PIC 9(1)  VALUE ZERO.
004190       05 施術年ＷＲ                   PIC 9(2)  VALUE ZERO.
004200       05 施術月ＷＲ                   PIC 9(2)  VALUE ZERO.
004210    03 保険種別ＷＲ                    PIC 9(2)  VALUE ZERO.
004220    03 保険者番号ＷＲ                  PIC X(10) VALUE SPACE.
004230    03 公費種別ＷＲ                    PIC 9(2)  VALUE ZERO.
004240    03 費用負担者番号ＷＲ              PIC X(10) VALUE SPACE.
004250    03 助成種別ＷＲ                    PIC 9(2)  VALUE ZERO.
004260    03 費用負担者番号助成ＷＲ          PIC X(10) VALUE SPACE.
004270    03 本人家族区分ＷＲ                PIC 9(1)  VALUE ZERO.
004280    03 患者カナＷＲ                    PIC X(50) VALUE SPACE.
004290    03 患者コードＷＲ.
004300       05 患者番号ＷＲ                 PIC 9(6)  VALUE ZERO.
004310       05 枝番ＷＲ                     PIC X(1)  VALUE SPACE.
004320*    ************
004330*    * 料金情報 *
004340*    ************
004350*--- 月毎の料金 ---*
004360 01 料金１ＷＲ.
004370   03 初検ＷＲ.
004380      05 負担割合ＷＲ                  PIC 9(3)  VALUE ZERO.
004390      05 初検料ＷＲ                    PIC 9(5)  VALUE ZERO.
004400      05 初検加算料ＷＲ                PIC 9(5)  VALUE ZERO.
         03 相談料ＷＲ                       PIC 9(4)  VALUE ZERO.
004410   03 再検料ＷＲ                       PIC 9(5)  VALUE ZERO.
004420   03 往療ＷＲ.
004430      05 往療距離ＷＲ                  PIC 9(2)V9 VALUE ZERO.
004440      05 往療回数ＷＲ                  PIC 9(2)  VALUE ZERO.
004450      05 往療料ＷＲ                    PIC 9(5)  VALUE ZERO.
004460      05 往療加算料ＷＲ                PIC 9(5)  VALUE ZERO.
004470   03 金属副子加算料ＷＲ               PIC 9(5)  VALUE ZERO.
004480   03 施術情報提供料ＷＲ               PIC 9(5)  VALUE ZERO.
004490   03 合計ＷＲ                         PIC 9(6)  VALUE ZERO.
004500   03 一部負担金ＷＲ                   PIC 9(6)  VALUE ZERO.
004510   03 請求金額ＷＲ                     PIC 9(6)  VALUE ZERO.
004520   03 給付割合ＷＲ                     PIC 9(1)  VALUE ZERO.
004530   03 受給者負担額ＷＲ                 PIC 9(6)  VALUE ZERO.
004540   03 助成請求金額ＷＲ                 PIC 9(6)  VALUE ZERO.
004550*
004560*--- 負傷部位毎の料金 ---*
004570 01 料金２ＷＲ.
004580   03 初回処置ＷＲ    OCCURS   9.
004590      05 初回処置料ＷＲ                PIC 9(5)  VALUE ZERO.
004600*
004610*--- 逓減毎の料金 ---*
004620 01 料金３ＷＲ.
004630**********
004640* １部位 *
004650**********
004660   03 部位１ＷＲ.
004670      05 後療１ＷＲ.
004680         07 後療単価１ＷＲ                PIC 9(4)  VALUE ZERO.
004690         07 後療回数１ＷＲ                PIC 9(2)  VALUE ZERO.
004700         07 後療料１ＷＲ                  PIC 9(5)  VALUE ZERO.
004710      05 冷罨法１ＷＲ.
004720         07 冷罨法回数１ＷＲ              PIC 9(2)  VALUE ZERO.
004730         07 冷罨法料１ＷＲ                PIC 9(4)  VALUE ZERO.
004740      05 温罨法１ＷＲ.
004750         07 温罨法回数１ＷＲ              PIC 9(2)  VALUE ZERO.
004760         07 温罨法料１ＷＲ                PIC 9(4)  VALUE ZERO.
004770      05 電療１ＷＲ.
004780         07 電療回数１ＷＲ                PIC 9(2)  VALUE ZERO.
004790         07 電療料１ＷＲ                  PIC 9(4)  VALUE ZERO.
004800      05 小計１ＷＲ                       PIC 9(6)  VALUE ZERO.
004810      05 長期逓減率１ＷＲ                 PIC 9(3)  VALUE ZERO.
004820      05 長期込小計１ＷＲ                 PIC 9(6)  VALUE ZERO.
004830**********
004840* ２部位 *
004850**********
004860   03 部位２ＷＲ.
004870      05 後療２ＷＲ.
004880         07 後療単価２ＷＲ                PIC 9(4)  VALUE ZERO.
004890         07 後療回数２ＷＲ                PIC 9(2)  VALUE ZERO.
004900         07 後療料２ＷＲ                  PIC 9(5)  VALUE ZERO.
004910      05 冷罨法２ＷＲ.
004920         07 冷罨法回数２ＷＲ              PIC 9(2)  VALUE ZERO.
004930         07 冷罨法料２ＷＲ                PIC 9(4)  VALUE ZERO.
004940      05 温罨法２ＷＲ.
004950         07 温罨法回数２ＷＲ              PIC 9(2)  VALUE ZERO.
004960         07 温罨法料２ＷＲ                PIC 9(4)  VALUE ZERO.
004970      05 電療２ＷＲ.
004980         07 電療回数２ＷＲ                PIC 9(2)  VALUE ZERO.
004990         07 電療料２ＷＲ                  PIC 9(4)  VALUE ZERO.
005000      05 小計２ＷＲ                       PIC 9(6)  VALUE ZERO.
005010      05 長期逓減率２ＷＲ                 PIC 9(3)  VALUE ZERO.
005020      05 長期込小計２ＷＲ                 PIC 9(6)  VALUE ZERO.
005030****************
005040* ３部位／８割 *
005050****************
005060   03 部位３８ＷＲ.
005070      05 後療３８ＷＲ.
005080         07 後療単価３８ＷＲ              PIC 9(4)  VALUE ZERO.
005090         07 後療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
005100         07 後療料３８ＷＲ                PIC 9(5)  VALUE ZERO.
005110      05 冷罨法３８ＷＲ.
005120         07 冷罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
005130         07 冷罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
005140      05 温罨法３８ＷＲ.
005150         07 温罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
005160         07 温罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
005170      05 電療３８ＷＲ.
005180         07 電療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
005190         07 電療料３８ＷＲ                PIC 9(4)  VALUE ZERO.
005200      05 小計３８ＷＲ                     PIC 9(6)  VALUE ZERO.
005210      05 多部位込小計３８ＷＲ             PIC 9(6)  VALUE ZERO.
005220      05 長期逓減率３８ＷＲ               PIC 9(3)  VALUE ZERO.
005230      05 長期込小計３８ＷＲ               PIC 9(6)  VALUE ZERO.
005240******************
005250* ３部位／１０割 *
005260******************
005270   03 部位３０ＷＲ.
005280      05 逓減開始月日３０ＷＲ.
005290         07 逓減開始月３０ＷＲ            PIC 9(2)  VALUE ZERO.
005300         07 逓減開始日３０ＷＲ            PIC 9(2)  VALUE ZERO.
005310      05 後療３０ＷＲ.
005320         07 後療単価３０ＷＲ              PIC 9(4)  VALUE ZERO.
005330         07 後療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
005340         07 後療料３０ＷＲ                PIC 9(5)  VALUE ZERO.
005350      05 冷罨法３０ＷＲ.
005360         07 冷罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
005370         07 冷罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
005380      05 温罨法３０ＷＲ.
005390         07 温罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
005400         07 温罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
005410      05 電療３０ＷＲ.
005420         07 電療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
005430         07 電療料３０ＷＲ                PIC 9(4)  VALUE ZERO.
005440      05 小計３０ＷＲ                     PIC 9(6)  VALUE ZERO.
005450      05 長期逓減率３０ＷＲ               PIC 9(3)  VALUE ZERO.
005460      05 長期込小計３０ＷＲ               PIC 9(6)  VALUE ZERO.
005470****************
005480* ４部位／５割 *
005490****************
005500   03 部位４５ＷＲ.
005510      05 後療４５ＷＲ.
005520         07 後療単価４５ＷＲ              PIC 9(4)  VALUE ZERO.
005530         07 後療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005540         07 後療料４５ＷＲ                PIC 9(5)  VALUE ZERO.
005550      05 冷罨法４５ＷＲ.
005560         07 冷罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005570         07 冷罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
005580      05 温罨法４５ＷＲ.
005590         07 温罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005600         07 温罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
005610      05 電療４５ＷＲ.
005620         07 電療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005630         07 電療料４５ＷＲ                PIC 9(4)  VALUE ZERO.
005640      05 小計４５ＷＲ                     PIC 9(6)  VALUE ZERO.
005650      05 多部位込小計４５ＷＲ             PIC 9(6)  VALUE ZERO.
005660      05 長期逓減率４５ＷＲ               PIC 9(3)  VALUE ZERO.
005670      05 長期込小計４５ＷＲ               PIC 9(6)  VALUE ZERO.
005680****************
005690* ４部位／８割 *
005700****************
005710   03 部位４８ＷＲ.
005720      05 逓減開始月日４８ＷＲ.
005730         07 逓減開始月４８ＷＲ            PIC 9(2)  VALUE ZERO.
005740         07 逓減開始日４８ＷＲ            PIC 9(2)  VALUE ZERO.
005750      05 後療４８ＷＲ.
005760         07 後療単価４８ＷＲ              PIC 9(4)  VALUE ZERO.
005770         07 後療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
005780         07 後療料４８ＷＲ                PIC 9(5)  VALUE ZERO.
005790      05 冷罨法４８ＷＲ.
005800         07 冷罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
005810         07 冷罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
005820      05 温罨法４８ＷＲ.
005830         07 温罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
005840         07 温罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
005850      05 電療４８ＷＲ.
005860         07 電療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
005870         07 電療料４８ＷＲ                PIC 9(4)  VALUE ZERO.
005880      05 小計４８ＷＲ                     PIC 9(6)  VALUE ZERO.
005890      05 多部位込小計４８ＷＲ             PIC 9(6)  VALUE ZERO.
005900      05 長期逓減率４８ＷＲ               PIC 9(3)  VALUE ZERO.
005910      05 長期込小計４８ＷＲ               PIC 9(6)  VALUE ZERO.
005920******************
005930* ４部位／１０割 *
005940******************
005950   03 部位４０ＷＲ.
005960      05 逓減開始月日４０ＷＲ.
005970         07 逓減開始月４０ＷＲ            PIC 9(2)  VALUE ZERO.
005980         07 逓減開始日４０ＷＲ            PIC 9(2)  VALUE ZERO.
005990      05 後療４０ＷＲ.
006000         07 後療単価４０ＷＲ              PIC 9(4)  VALUE ZERO.
006010         07 後療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
006020         07 後療料４０ＷＲ                PIC 9(5)  VALUE ZERO.
006030      05 冷罨法４０ＷＲ.
006040         07 冷罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
006050         07 冷罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
006060      05 温罨法４０ＷＲ.
006070         07 温罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
006080         07 温罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
006090      05 電療４０ＷＲ.
006100         07 電療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
006110         07 電療料４０ＷＲ                PIC 9(4)  VALUE ZERO.
006120      05 小計４０ＷＲ                     PIC 9(6)  VALUE ZERO.
006130      05 長期逓減率４０ＷＲ               PIC 9(3)  VALUE ZERO.
006140      05 長期込小計４０ＷＲ               PIC 9(6)  VALUE ZERO.
006150********************
006160* ５部位／２．５割 *
006170********************
006180   03 部位５２ＷＲ.
006190      05 後療５２ＷＲ.
006200         07 後療単価５２ＷＲ              PIC 9(4)  VALUE ZERO.
006210         07 後療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
006220         07 後療料５２ＷＲ                PIC 9(5)  VALUE ZERO.
006230      05 冷罨法５２ＷＲ.
006240         07 冷罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
006250         07 冷罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
006260      05 温罨法５２ＷＲ.
006270         07 温罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
006280         07 温罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
006290      05 電療５２ＷＲ.
006300         07 電療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
006310         07 電療料５２ＷＲ                PIC 9(4)  VALUE ZERO.
006320      05 小計５２ＷＲ                     PIC 9(6)  VALUE ZERO.
006330      05 多部位込小計５２ＷＲ             PIC 9(6)  VALUE ZERO.
006340      05 長期逓減率５２ＷＲ               PIC 9(3)  VALUE ZERO.
006350      05 長期込小計５２ＷＲ               PIC 9(6)  VALUE ZERO.
006360****************
006370* ５部位／５割 *
006380****************
006390   03 部位５５ＷＲ.
006400      05 逓減開始月日５５ＷＲ.
006410         07 逓減開始月５５ＷＲ            PIC 9(2)  VALUE ZERO.
006420         07 逓減開始日５５ＷＲ            PIC 9(2)  VALUE ZERO.
006430      05 後療５５ＷＲ.
006440         07 後療単価５５ＷＲ              PIC 9(4)  VALUE ZERO.
006450         07 後療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
006460         07 後療料５５ＷＲ                PIC 9(5)  VALUE ZERO.
006470      05 冷罨法５５ＷＲ.
006480         07 冷罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
006490         07 冷罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
006500      05 温罨法５５ＷＲ.
006510         07 温罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
006520         07 温罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
006530      05 電療５５ＷＲ.
006540         07 電療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
006550         07 電療料５５ＷＲ                PIC 9(4)  VALUE ZERO.
006560      05 小計５５ＷＲ                     PIC 9(6)  VALUE ZERO.
006570      05 多部位込小計５５ＷＲ             PIC 9(6)  VALUE ZERO.
006580      05 長期逓減率５５ＷＲ               PIC 9(3)  VALUE ZERO.
006590      05 長期込小計５５ＷＲ               PIC 9(6)  VALUE ZERO.
006600****************
006610* ５部位／８割 *
006620****************
006630   03 部位５８ＷＲ.
006640      05 逓減開始月日５８ＷＲ.
006650         07 逓減開始月５８ＷＲ            PIC 9(2)  VALUE ZERO.
006660         07 逓減開始日５８ＷＲ            PIC 9(2)  VALUE ZERO.
006670      05 後療５８ＷＲ.
006680         07 後療単価５８ＷＲ              PIC 9(4)  VALUE ZERO.
006690         07 後療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
006700         07 後療料５８ＷＲ                PIC 9(5)  VALUE ZERO.
006710      05 冷罨法５８ＷＲ.
006720         07 冷罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
006730         07 冷罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
006740      05 温罨法５８ＷＲ.
006750         07 温罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
006760         07 温罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
006770      05 電療５８ＷＲ.
006780         07 電療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
006790         07 電療料５８ＷＲ                PIC 9(4)  VALUE ZERO.
006800      05 小計５８ＷＲ                     PIC 9(6)  VALUE ZERO.
006810      05 多部位込小計５８ＷＲ             PIC 9(6)  VALUE ZERO.
006820      05 長期逓減率５８ＷＲ               PIC 9(3)  VALUE ZERO.
006830      05 長期込小計５８ＷＲ               PIC 9(6)  VALUE ZERO.
006840******************
006850* ５部位／１０割 *
006860******************
006870   03 部位５０ＷＲ.
006880      05 逓減開始月日５０ＷＲ.
006890         07 逓減開始月５０ＷＲ            PIC 9(2)  VALUE ZERO.
006900         07 逓減開始日５０ＷＲ            PIC 9(2)  VALUE ZERO.
006910      05 後療５０ＷＲ.
006920         07 後療単価５０ＷＲ              PIC 9(4)  VALUE ZERO.
006930         07 後療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
006940         07 後療料５０ＷＲ                PIC 9(5)  VALUE ZERO.
006950      05 冷罨法５０ＷＲ.
006960         07 冷罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
006970         07 冷罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
006980      05 温罨法５０ＷＲ.
006990         07 温罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
007000         07 温罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
007010      05 電療５０ＷＲ.
007020         07 電療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
007030         07 電療料５０ＷＲ                PIC 9(4)  VALUE ZERO.
007040      05 小計５０ＷＲ                     PIC 9(6)  VALUE ZERO.
007050      05 長期逓減率５０ＷＲ               PIC 9(3)  VALUE ZERO.
007060      05 長期込小計５０ＷＲ               PIC 9(6)  VALUE ZERO.
008000*******************
008010*  明細書発行加算 */202206
008020*******************
008030   03 明細書発行加算料ＷＲ                PIC ZZZ   VALUE ZERO.
008030   03 明細書発行加算日ＷＲ                PIC ZZ    VALUE ZERO.
007070*
007080**************
007090* 施術所情報 *
007100**************
007110 01 施術所情報Ｗ.
          03 都道府県ＪＩＳＷ                PIC X(2)   VALUE SPACE.
007120    03 柔整師番号Ｗ                    PIC X(22)  VALUE SPACE.
007130    03 接骨師会会員番号Ｗ              PIC X(10)  VALUE SPACE.
007140    03 代表者カナＷ                    PIC X(50)  VALUE SPACE.
007150    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
007160    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
007170    03 施術所住所Ｗ.
007180       05 施術所住所１Ｗ               PIC X(50)  VALUE SPACE.
007190       05 施術所住所２Ｗ               PIC X(50)  VALUE SPACE.
007200    03 施術所郵便番号Ｗ.
007210       05 施術所郵便番号１Ｗ           PIC X(3)   VALUE SPACE.
007220       05 施術所郵便番号２Ｗ           PIC X(4)   VALUE SPACE.
007230    03 施術所電話番号Ｗ                PIC X(15)  VALUE SPACE.
007240    03 定額制受理番号Ｗ                PIC X(15)  VALUE SPACE.
007250    03 柔整師年月日Ｗ.
007350       05 柔整師和暦Ｗ                 PIC 9      VALUE ZERO.
007260       05 柔整師年Ｗ                   PIC 9(2)   VALUE ZERO.
007270       05 柔整師月Ｗ                   PIC 9(2)   VALUE ZERO.
007280       05 柔整師日Ｗ                   PIC 9(2)   VALUE ZERO.
007290    03 患者委任年月日Ｗ.
007350       05 患者委任和暦Ｗ               PIC 9      VALUE ZERO.
007300       05 患者委任年Ｗ                 PIC 9(2)   VALUE ZERO.
007310       05 患者委任月Ｗ                 PIC 9(2)   VALUE ZERO.
007320       05 患者委任日Ｗ                 PIC 9(2)   VALUE ZERO.
007330    03 県施術ＩＤＷ                    PIC X(12)  VALUE SPACE.
007340    03 市町村施術ＩＤＷ                PIC X(12)  VALUE SPACE.
007350    03 接骨師会会長名Ｗ                PIC N(10)  VALUE SPACE.
007360    03 取引先情報Ｗ.
007030       05 取引先銀行名Ｗ               PIC X(40) VALUE SPACE.
007040       05 取引先銀行支店名Ｗ           PIC X(40) VALUE SPACE.
007370*       05 取引先銀行名Ｗ.
007380*          07 印刷銀行名Ｗ              PIC X(26)  VALUE SPACE.
007390*          07 FILLER                    PIC X(14)  VALUE SPACE.
007400*       05 取引先銀行支店名Ｗ.
007410*          07 印刷銀行支店名１Ｗ        PIC X(14)  VALUE SPACE.
007420*          07 印刷銀行支店名２Ｗ        PIC X(14)  VALUE SPACE.
007430*          07 FILLER                    PIC X(12)  VALUE SPACE.
007440       05 預金種別Ｗ                   PIC 9(1)   VALUE ZERO.
007450       05 口座番号Ｗ                   PIC X(10)  VALUE SPACE.
007460       05 口座名義人Ｗ                 PIC X(40)  VALUE SPACE.
007470       05 口座名義人カナＷ             PIC X(40)  VALUE SPACE.
007480       05 預金種別コメントＷ           PIC N(4)   VALUE SPACE.
          03 支払機関.
             05 金融機関名Ｗ.
                07 金融機関名１Ｗ            PIC X(10)  VALUE SPACE.
                07 金融機関名２Ｗ            PIC X(10)  VALUE SPACE.
                07 金融機関名３Ｗ            PIC X(10)  VALUE SPACE.
                07 金融機関名４Ｗ            PIC X(10)  VALUE SPACE.
                07 金融機関名５Ｗ            PIC X(10)  VALUE SPACE.
             05 支店名Ｗ.
                07 支店名１Ｗ                PIC X(12) VALUE SPACE.
                07 支店名２Ｗ                PIC X(12) VALUE SPACE.
                07 支店名３Ｗ                PIC X(12) VALUE SPACE.
                07 支店名４Ｗ                PIC X(12) VALUE SPACE.
             05 振込チェックＷ               PIC N(1)  VALUE SPACE.
             05 普通チェックＷ               PIC N(1)  VALUE SPACE.
             05 当座チェックＷ               PIC N(1)  VALUE SPACE.
             05 銀行チェックＷ               PIC N(1)  VALUE SPACE.
             05 金庫チェックＷ               PIC N(1)  VALUE SPACE.
             05 農協チェックＷ               PIC N(1)  VALUE SPACE.
             05 本店チェックＷ               PIC N(1)  VALUE SPACE.
             05 支店チェックＷ               PIC N(1)  VALUE SPACE.
             05 本支所チェックＷ             PIC N(1)  VALUE SPACE.
007490*
007500**************
007510* 受診者情報 *
007520**************
007530 01 受診者情報Ｗ.
007540    03 施術年月Ｗ.
             05 施術和暦Ｗ                   PIC 9(1)   VALUE ZERO.
007550       05 施術年Ｗ                     PIC 9(2)   VALUE ZERO.
007560       05 施術月Ｗ                     PIC 9(2)   VALUE ZERO.
007570    03 記号Ｗ.
007580       05 印刷記号Ｗ                   PIC N(11)  VALUE SPACE.
007590       05 FILLER                       PIC N(1)   VALUE SPACE.
          03 記号番号Ｗ.
             05 記号番号ＸＷ                 PIC X(40) VALUE SPACE.
007600    03 番号Ｗ.
007610       05 印刷番号Ｗ                   PIC X(15)  VALUE SPACE.
007620       05 FILLER                       PIC X(15)  VALUE SPACE.
007630    03 保険者番号Ｗ.
007640       05 印刷保険者番号Ｗ             PIC X(8)   VALUE SPACE.
007650       05 FILLER                       PIC X(2)   VALUE SPACE.
007660    03 市町村番号Ｗ.
007670       05 印刷市町村番号Ｗ             PIC X(8)   VALUE SPACE.
007680       05 FILLER                       PIC X(2)   VALUE SPACE.
007690    03 受給者番号Ｗ.
007700       05 印刷受給者番号Ｗ             PIC X(7)   VALUE SPACE.
007710       05 FILLER                       PIC X(13)  VALUE SPACE.
007720    03 請求先名称Ｗ.
007730       05 請求先名称１Ｗ               PIC X(48)  VALUE SPACE.
007740       05 請求先名称２Ｗ               PIC X(32)  VALUE SPACE.
          03 請求先名称３Ｗ                  PIC X(50)  VALUE SPACE.
007750*       05 FILLER                       PIC X(28)  VALUE SPACE.
007760    03 保険種別Ｗ                      PIC 9(2)   VALUE ZERO.
007770*    03 保険種別チェックＷ.
007780*       05 国チェックＷ                 PIC N(1)  VALUE SPACE.
007790*       05 退チェックＷ                 PIC N(1)  VALUE SPACE.
007800*       05 老チェックＷ                 PIC N(1)  VALUE SPACE.
007810*       05 政チェックＷ                 PIC N(1)  VALUE SPACE.
007820*       05 船チェックＷ                 PIC N(1)  VALUE SPACE.
007830*       05 組チェックＷ                 PIC N(1)  VALUE SPACE.
007840*       05 自チェックＷ                 PIC N(1)  VALUE SPACE.
007850*       05 共チェックＷ                 PIC N(1)  VALUE SPACE.
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
007860    03 助成種別Ｗ                      PIC 9(2)   VALUE ZERO.
007870    03 助成種別チェックＷ.
007880       05 身チェックＷ                 PIC N(1)  VALUE SPACE.
007890       05 乳チェックＷ                 PIC N(1)  VALUE SPACE.
007900       05 母チェックＷ                 PIC N(1)  VALUE SPACE.
007910    03 被保険者情報Ｗ.
007920       05 被保険者カナＷ               PIC X(50)  VALUE SPACE.
007930       05 被保険者氏名Ｗ               PIC X(50)  VALUE SPACE.
007940       05 郵便番号Ｗ.
007950          07 郵便番号１Ｗ              PIC X(3)   VALUE SPACE.
007960          07 郵便番号２Ｗ              PIC X(4)   VALUE SPACE.
007970       05 被保険者住所１Ｗ             PIC X(50)  VALUE SPACE.
007980       05 被保険者住所２Ｗ             PIC X(50)  VALUE SPACE.
008990       05 電話番号Ｗ                   PIC X(35)  VALUE SPACE.
007990    03 患者情報Ｗ.
008000       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
008010       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
007940       05 患者郵便番号Ｗ.
007950          07 患者郵便番号１Ｗ          PIC X(3)   VALUE SPACE.
007960          07 患者郵便番号２Ｗ          PIC X(4)   VALUE SPACE.
008020       05 患者住所Ｗ.
008030          07 患者住所１Ｗ              PIC X(50)  VALUE SPACE.
008040          07 患者住所２Ｗ              PIC X(50)  VALUE SPACE.
008990       05 患者電話番号Ｗ               PIC X(35)  VALUE SPACE.
008050       05 性別チェックＷ.
008060          07 男チェックＷ              PIC N(1)  VALUE SPACE.
008070          07 女チェックＷ              PIC N(1)  VALUE SPACE.
008080       05 和暦チェックＷ.
008090          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
008100          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
008110          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
008120          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
      */元号修正/↓↓↓20190426
008210          07 令和チェックＷ            PIC N(1)  VALUE SPACE.
                07 令和ＣＭＷ                PIC X(4)  VALUE SPACE.
009110          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
      */元号修正/↑↑↑20190426
008130       05 患者年Ｗ                     PIC 9(2)  VALUE ZERO.
008140       05 患者月Ｗ                     PIC 9(2)  VALUE ZERO.
008150       05 患者日Ｗ                     PIC 9(2)  VALUE ZERO.
008160       05 続柄Ｗ.
008170          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
008180          07 FILLER                    PIC X(4)  VALUE SPACE.
008190       05 続柄チェックＷ.
008200          07 続柄本人チェックＷ        PIC N(1)  VALUE SPACE.
008210          07 続柄家族チェックＷ        PIC N(1)  VALUE SPACE.
008220       05 特別区分チェックＷ.
008230          07 ３歳未満チェックＷ        PIC N(1)  VALUE SPACE.
008240          07 ７０歳９チェックＷ        PIC N(1)  VALUE SPACE.
008250          07 ７０歳８チェックＷ        PIC N(1)  VALUE SPACE.
008260*
008270*       05 負傷原因Ｗ                   PIC N(40) OCCURS 29 VALUE SPACE.
      */半角対応/110421
             05 負傷原因Ｗ OCCURS 29.
                07 負傷原因ＸＷ              PIC X(80)  VALUE SPACE.
008280*
008290    03 負担割合チェックＷ              PIC N(2)  VALUE SPACE.
008300    03 給付割合チェックＷ.
008310       05 １０割チェックＷ             PIC N(1)  VALUE SPACE.
008320       05 ９割チェックＷ               PIC N(1)  VALUE SPACE.
008330       05 ８割チェックＷ               PIC N(1)  VALUE SPACE.
008340       05 ７割チェックＷ               PIC N(1)  VALUE SPACE.
008350*
008360    03 助成負担金免除チェックＷ.
008370       05 負担金有チェックＷ           PIC N(1)  VALUE SPACE.
008380       05 負担金無チェックＷ           PIC N(1)  VALUE SPACE.
008390*
008400****************
008410* 負傷データＦ *
008420****************
008430 01 負傷情報Ｗ.
008440    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
008450    03 部位情報Ｗ  OCCURS   9.
008460       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
008470       05 部位コードＷ.
008480          07 負傷種別Ｗ                PIC 9(2)  VALUE ZERO.
008490          07 部位Ｗ                    PIC 9(2)  VALUE ZERO.
008500          07 左右区分Ｗ                PIC 9(1)  VALUE ZERO.
008510          07 負傷位置番号Ｗ            PIC 9(2)  VALUE ZERO.
008520       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
008530       05 負傷年月日Ｗ.
008540          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
008550          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
008560          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
008570       05 初検年月日Ｗ.
008580          07 初検年Ｗ                  PIC 9(2)  VALUE ZERO.
008590          07 初検月Ｗ                  PIC 9(2)  VALUE ZERO.
008600          07 初検日Ｗ                  PIC 9(2)  VALUE ZERO.
008610       05 開始年月日Ｗ.
008620          07 開始年Ｗ                  PIC 9(2)  VALUE ZERO.
008630          07 開始月Ｗ                  PIC 9(2)  VALUE ZERO.
008640          07 開始日Ｗ                  PIC 9(2)  VALUE ZERO.
008650       05 終了年月日Ｗ.
002980          07 終了和暦Ｗ                PIC 9     VALUE ZERO.
008660          07 終了年Ｗ                  PIC 9(2)  VALUE ZERO.
008670          07 終了月Ｗ                  PIC 9(2)  VALUE ZERO.
008680          07 終了日Ｗ                  PIC 9(2)  VALUE ZERO.
008690       05 実日数Ｗ                     PIC 9(2)  VALUE ZERO.
             05 部位継続月数Ｗ               PIC 9(3)  VALUE ZERO.
008700       05 転帰区分Ｗ                   PIC 9(1)  VALUE ZERO.
008710       05 転帰区分チェックＷ.
008720          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
008730          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
008740          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
008750       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
008760       05 部位区切Ｗ                   PIC X(1)  VALUE SPACE.
008770       05 経過略称Ｗ.
008780          07 印刷経過略称Ｗ            PIC N(5)  VALUE SPACE.
008790          07 FILLER                    PIC X(2)  VALUE SPACE.
          03 経過部位番号Ｗ                  PIC N(1)  VALUE SPACE.
008800    03 新規チェックＷ                  PIC N(1)  VALUE SPACE.
008810    03 継続チェックＷ                  PIC N(1)  VALUE SPACE.
          03 施術日Ｗ.
             05 施術日チェックＷ   OCCURS 31 PIC N(1)  VALUE SPACE.
008820*
008830****************
008840* 基本料金情報 *
008850****************
008860 01 基本料金Ｗ.
008870    03 冷罨法単価Ｗ                    PIC 9(4)  VALUE ZERO.
008880    03 温罨法単価Ｗ                    PIC 9(4)  VALUE ZERO.
008890    03 電療単価Ｗ                      PIC 9(4)  VALUE ZERO.
008900*
008910************
008920* 料金情報 *
008930************
008940 01 料金情報Ｗ.
008950    03 初検加算Ｗ.
008960       05 時間外チェックＷ             PIC N(1)  VALUE SPACE.
008970       05 休日チェックＷ               PIC N(1)  VALUE SPACE.
008980       05 深夜チェックＷ               PIC N(1)  VALUE SPACE.
008990    03 往療加算Ｗ.
009000       05 夜間チェックＷ               PIC N(1)  VALUE SPACE.
009010       05 難路チェックＷ               PIC N(1)  VALUE SPACE.
009020       05 暴風雨雪チェックＷ           PIC N(1)  VALUE SPACE.
009030    03 金属副子チェックＷ.
009040       05 大チェックＷ                 PIC N(1)  VALUE SPACE.
009050       05 中チェックＷ                 PIC N(1)  VALUE SPACE.
009060       05 小チェックＷ                 PIC N(1)  VALUE SPACE.
009070    03 初回処置料チェックＷ.
009080       05 整復料チェックＷ             PIC N(1)  VALUE SPACE.
009090       05 固定料チェックＷ             PIC N(1)  VALUE SPACE.
009100       05 施療料チェックＷ             PIC N(1)  VALUE SPACE.
009110    03 小計Ｗ                          PIC 9(7)   VALUE ZERO.
009120    03 初回処置料合計Ｗ                PIC 9(6)  VALUE ZERO.
      */金属副子・運動後療の変更・追加/1805
          03 金属回数Ｗ                         PIC 9(2)  VALUE ZERO.
          03 運動回数Ｗ                         PIC 9(1)  VALUE ZERO.
          03 運動料Ｗ                           PIC 9(5)  VALUE ZERO.
009130*
009140************
009150* 備考情報 *
009160************
009170 01 備考情報Ｗ.
010010    03 適用１Ｗ                        PIC N(48) VALUE SPACE.
010020    03 適用２Ｗ                        PIC X(40) VALUE SPACE.
009200*
009210** レセ摘要用( N(38)固定） /
009220 01 負傷の経過Ｗ.
009230    03 負傷の経過行Ｗ                  PIC X(76) OCCURS 2 VALUE SPACE.
009240 01 負傷の経過ＮＷ REDEFINES 負傷の経過Ｗ.
009250    03 負傷の経過行ＮＷ                PIC N(38) OCCURS 2.
009260*
009270**
002410*
002420** 保険者番号右詰め用
002430 01 保険者番号ＷＴ.
002440    03 保険者番号左詰めＷ.
002450      05 保険者番号左詰めＷ１          PIC X OCCURS 8 VALUE SPACE.
002460    03 保険者番号右詰めＷ.
002470      05 保険者番号右詰めＷ１          PIC X OCCURS 8 VALUE SPACE.
      *
       01 摘要施術日Ｗ                       PIC X(100) VALUE SPACE.
       01 施術日Ｗ.
          03 施術日２Ｗ                      PIC X(1)  VALUE SPACE.
          03 施術日１Ｗ                      PIC X(1)  VALUE SPACE.
002480*
009280 01 印刷制御.
009290     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
009300     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
009310     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
009320     03 拡張制御Ｐ.
009330         05 端末制御Ｐ.
009340             07 移動方向Ｐ             PIC X(1) VALUE SPACE.
009350             07 移動行数Ｐ             PIC 9(3) VALUE ZERO.
009360         05 詳細制御Ｐ                 PIC X(2) VALUE SPACE.
009370     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
009380     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
009390*
009400 01 計算機西暦年Ｗ                     PIC 9(2) VALUE ZERO.
009410* 日付ＷＯＲＫ
009420 01 和暦終了年Ｗ                       PIC 9(4) VALUE ZERO.
009430 01 計算機西暦.
009440    03 計算機西暦年                    PIC 9(4) VALUE ZERO.
009450    03 計算機西暦月日                  PIC 9(4) VALUE ZERO.
009460 01 計算機西暦Ｒ REDEFINES 計算機西暦.
009470    03 計算機世紀                      PIC 9(2).
009480    03 計算機日付                      PIC 9(6).
009490    03 計算機日付Ｒ REDEFINES 計算機日付.
009500       05 計算機年月                   PIC 9(4).
009510       05 計算機年月Ｒ REDEFINES 計算機年月.
009520         07 計算機年                   PIC 9(2).
009530         07 計算機月                   PIC 9(2).
009540       05 計算機日                     PIC 9(2).
009550*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
009560******************************************************************
009570*                          連結項目                              *
009580******************************************************************
009590************
009600* 印刷キー *
009610************
009620*
009630 01 連レ印－対象データ IS EXTERNAL.
009640    03 連レ印－施術年月日.
009650       05 連レ印－施術和暦                  PIC 9(1).
009660       05 連レ印－施術年                    PIC 9(2).
009670       05 連レ印－施術月                    PIC 9(2).
009680    03 連レ印－患者コード.
009690       05 連レ印－患者番号                  PIC 9(6).
009700       05 連レ印－枝番                      PIC X(1).
009710    03 連レ印－保険種別                     PIC 9(2).
009720    03 連レ印－保険者番号                   PIC X(10).
009730    03 連レ印－公費種別                     PIC 9(2).
009740    03 連レ印－費用負担者番号               PIC X(10).
009750    03 連レ印－助成種別                     PIC 9(2).
009760    03 連レ印－費用負担者番号助成           PIC X(10).
009770    03 連レ印－患者カナ                     PIC X(20).
009780    03 連レ印－本人家族区分                 PIC 9(1).
013210*
013220 01 連レ－キー IS EXTERNAL.
013230    03 連レ－保険種別                  PIC 9(2).
013240*
013250*================================================================*
013260* 負担率取得用14/10～
013270 01 連率－負担率取得キー IS EXTERNAL.
013280    03 連率－施術和暦年月.
013290       05 連率－施術和暦               PIC 9.
013300       05 連率－施術年月.
013310          07 連率－施術年              PIC 9(2).
013320          07 連率－施術月              PIC 9(2).
013330    03 連率－患者コード.
013340       05 連率－患者番号               PIC 9(6).
013350       05 連率－枝番                   PIC X.
013360    03 連率－実際負担率                PIC 9(3).
013370    03 連率－実際本体負担率            PIC 9(3).
013380    03 連率－健保負担率                PIC 9(3).
013390    03 連率－２７老負担率              PIC 9(3).
013400    03 連率－助成負担率                PIC 9(3).
013410    03 連率－特別用負担率              PIC 9(3).
013420*
013430** ３カ月長期判定
013440 01 連期間－キー IS EXTERNAL.
013450    03 連期間－施術年月.
013460       05 連期間－施術和暦               PIC 9.
013470       05 連期間－施術年                 PIC 9(2).
013480       05 連期間－施術月                 PIC 9(2).
013490    03  連期間－患者コード.
013500       05 連期間－患者番号               PIC 9(6).
013510       05 連期間－枝番                   PIC X.
013520    03 連期間－対象フラグ                PIC X(3).
013530    03 連期間－期間月Ｗ.
013540       05 連期間－期間Ｗ                 PIC 9(2) OCCURS 9.
013550*
013560************************
013570* 長期理由文セット     *
013580************************
013590 01 連長文－キー IS EXTERNAL.
013600    03 連長文－施術年月.
013610       05 連長文－施術和暦               PIC 9.
013620       05 連長文－施術年                 PIC 9(2).
013630       05 連長文－施術月                 PIC 9(2).
013640    03  連長文－患者コード.
013650       05 連長文－患者番号               PIC 9(6).
013660       05 連長文－枝番                   PIC X.
013670    03 連長文－文桁数                    PIC 9(2).
013680    03 連長文－理由文                    PIC N(63) OCCURS 15.
013480*
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
013700*
013710**  画面入力データ
       01 連入－プレビュー IS EXTERNAL.
          03 連入－プレビュー区分          PIC 9.
010440*
013720 01 連入－入力データ委任印刷 IS EXTERNAL.
013730    03 連入－委任印刷                     PIC 9.
       01 連入－入力データ電話印刷 IS EXTERNAL.
          03 連入－電話印刷                     PIC 9.
013740*
       01 連保険者番号変換－キー IS EXTERNAL.
          03 連保険者番号変換－施術和暦年月.
             05 連保険者番号変換－施術和暦               PIC 9.
             05 連保険者番号変換－施術年月.
                07 連保険者番号変換－施術年              PIC 9(2).
                07 連保険者番号変換－施術月              PIC 9(2).
          03 連保険者番号変換－保険種別                  PIC 9(2).
          03 連保険者番号変換－元保険者番号              PIC X(10).
      * 0:通常（施術年月関係なし）
          03 連保険者番号変換－呼出区分                  PIC 9(2).
      *  / OUT /
          03 連保険者番号変換－後保険者番号              PIC X(10).
      *
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
013834*
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
013750******************************************************************
013760*                      PROCEDURE  DIVISION                       *
013770******************************************************************
013780 PROCEDURE               DIVISION.
013790************
013800*           *
013810* 初期処理   *
013820*           *
013830************
002570     PERFORM プリンタファイル作成.
013840     PERFORM 初期化.
013850************
013860*           *
013870* 主処理     *
013880*           *
013890************
013900* 印刷
013910     PERFORM 連結項目待避.
013920     PERFORM 印刷セット.
013930     PERFORM 印刷処理.
013940************
013950*           *
013960* 終了処理   *
013970*           *
013980************
013990     PERFORM 受診者印刷区分更新.
014000     PERFORM 終了処理.
014010     MOVE ZERO  TO PROGRAM-STATUS.
014020     EXIT PROGRAM.
014030*
014040*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YAI6123"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分  TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
014050*
014060************
014070* 初期処理 *
014080************
014090******************************************************************
014100 初期化 SECTION.
014110******************************************************************
014120     PERFORM ファイルオープン.
014130*    /* 現在日付取得 */
014140     ACCEPT 計算機日付 FROM DATE.
014150*    /* 1980～2079年の間で設定 */
014160     IF ( 計算機年 > 80 )
014170        MOVE 19 TO 計算機世紀
014180     ELSE
014190        MOVE 20 TO 計算機世紀
014200     END-IF.
014210     PERFORM カレント元号取得.
014240*
014250*================================================================*
014260 ファイルオープン SECTION.
014270*================================================================*
014280     OPEN INPUT   元号マスタ.
014290         MOVE NC"元号" TO ファイル名.
014300         PERFORM オープンチェック.
014310     OPEN INPUT   名称マスタ.
014320         MOVE NC"名称" TO ファイル名.
014330         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
014370     OPEN INPUT   経過マスタ.
014380         MOVE NC"経過" TO ファイル名.
014390         PERFORM オープンチェック.
014400     OPEN INPUT 料金マスタ.
014410         MOVE NC"料金" TO ファイル名.
014420         PERFORM オープンチェック.
014430*
014440     OPEN INPUT   制御情報マスタ.
014450         MOVE NC"制御情報" TO ファイル名.
014460         PERFORM オープンチェック.
014470     OPEN INPUT   施術所情報マスタ.
014480         MOVE NC"施情" TO ファイル名.
014490         PERFORM オープンチェック.
014500*
014510     OPEN INPUT   保険者マスタ.
014520         MOVE NC"保険者" TO ファイル名.
014530         PERFORM オープンチェック.
014540     OPEN INPUT 市町村マスタ.
014550         MOVE NC"市町村" TO ファイル名.
014560         PERFORM オープンチェック.
014570     OPEN INPUT   請求先マスタ.
014580         MOVE NC"請先" TO ファイル名.
014590         PERFORM オープンチェック.
014600     OPEN INPUT   ＩＤ管理マスタ.
014610         MOVE NC"ＩＤ" TO ファイル名.
014620         PERFORM オープンチェック.
015160     OPEN INPUT   会情報マスタ.
015170         MOVE NC"会情" TO ファイル名.
015180         PERFORM オープンチェック.
014630*
014640     OPEN INPUT   施術記録Ｆ.
014650         MOVE NC"施記Ｆ" TO ファイル名.
014660         PERFORM オープンチェック.
014670     OPEN INPUT   負傷データＦ.
014680         MOVE NC"負傷" TO ファイル名.
014690         PERFORM オープンチェック.
014700     OPEN INPUT   負傷原因Ｆ.
014710         MOVE NC"負傷原因" TO ファイル名.
014720         PERFORM オープンチェック.
014760*
014770     OPEN I-O   受診者情報Ｆ.
014780         MOVE NC"受情" TO ファイル名.
014790         PERFORM オープンチェック.
014800     OPEN I-O   印刷ファイル.
014810         PERFORM エラー処理Ｐ.
014820*================================================================*
014830 オープンチェック SECTION.
014840*
014850     IF ( 状態キー  NOT =  "00" )
014860        DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
014870        DISPLAY NC"状態キー：" 状態キー         UPON CONS
014880        DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
014890                                                UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
014900        ACCEPT  キー入力 FROM CONS
014910        PERFORM ファイル閉鎖
014920        EXIT PROGRAM.
014930*================================================================*
014940 カレント元号取得 SECTION.
014950*================================================================*
014960     MOVE ZEROS TO 制－制御区分.
014970     READ 制御情報マスタ
014980     NOT INVALID KEY
014990         MOVE 制－カレント元号         TO カレント元号Ｗ
015000         MOVE 制－レセ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
015010         MOVE 制－レセ長期理由印刷区分 TO 長期理由印刷区分Ｗ
015020         MOVE 制－レセプト日付区分     TO レセプト日付区分Ｗ
015030         MOVE 制－レセプト患者日付区分 TO レセプト患者日付区分Ｗ
015040     END-READ.
015050*
021980*================================================================*
015350************
015360* 主処理   *
015370************
015380******************************************************************
015390 連結項目待避 SECTION.
015400******************************************************************
015410     MOVE 連レ印－施術和暦           TO 施術和暦ＷＲ.
015420     MOVE 連レ印－施術年             TO 施術年ＷＲ.
015430     MOVE 連レ印－施術月             TO 施術月ＷＲ.
015440     MOVE 連レ印－保険種別           TO 保険種別ＷＲ.
015450     MOVE 連レ印－保険者番号         TO 保険者番号ＷＲ.
015460     MOVE 連レ印－公費種別           TO 公費種別ＷＲ.
015470     MOVE 連レ印－費用負担者番号     TO 費用負担者番号ＷＲ.
015480     MOVE 連レ印－助成種別           TO 助成種別ＷＲ.
015490     MOVE 連レ印－費用負担者番号助成 TO 費用負担者番号助成ＷＲ.
015500     MOVE 連レ印－本人家族区分       TO 本人家族区分ＷＲ.
015510     MOVE 連レ印－患者カナ           TO 患者カナＷＲ.
015520     MOVE 連レ印－患者番号           TO 患者番号ＷＲ.
015530     MOVE 連レ印－枝番               TO 枝番ＷＲ.
015540*
      *
           INITIALIZE 連保険者番号変換－キー.
           MOVE  施術和暦年月ＷＲ  TO 連保険者番号変換－施術和暦年月.
           MOVE  保険種別ＷＲ      TO 連保険者番号変換－保険種別.
           MOVE  保険者番号ＷＲ    TO 連保険者番号変換－元保険者番号.
           MOVE  ZERO              TO 連保険者番号変換－呼出区分.
           CALL   "CHGHOKNO".
           CANCEL "CHGHOKNO".
      *   / 置換え /
           MOVE 連保険者番号変換－後保険者番号  TO  保険者番号ＷＲ.
      *
021980*================================================================*
015550******************************************************************
015560 印刷セット SECTION.
015570******************************************************************
015580     PERFORM 項目初期化.
           PERFORM 基本情報取得.
015590*
015640     PERFORM 施術所情報取得.
015650*
015660     PERFORM 請求先情報取得.
015670     PERFORM 受診者情報取得.
015680     PERFORM 負傷データ取得.
015690     PERFORM 料金情報取得.
015700     PERFORM 施術記録取得.
015730     PERFORM 初検加算時刻取得.
015740     PERFORM 委任年月日取得.
           PERFORM 施術日取得.
015750*
016791*-----------------------------------------------*
016800     IF ( 負傷原因印刷区分Ｗ  NOT = 1 ) AND ( レセ負傷原因印刷区分Ｗ NOT = 1 )
016813        IF ( 負傷原因印刷区分Ｗ = 3 OR 4 )
016815           PERFORM 負傷原因印刷対象判定処理
016817        ELSE
016820           PERFORM 負傷原因取得
016821        END-IF
016830     END-IF.
016831*-----------------------------------------------*
015800*
015810     IF ( 長期理由印刷区分Ｗ  NOT = 1 )
               MOVE 長期理由印刷区分Ｗ TO 連摘文－長期区分
015860     END-IF.
015870*
015880     PERFORM 基本料取得.
015890     PERFORM 負担給付割合取得.
015900     PERFORM 会長委任判定.
015910*
015920********************
015930* 受診者情報セット *
015940********************
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
016030*
037370     MOVE 施術和暦Ｗ         TO 元－元号区分.
037380     READ 元号マスタ
037390     NOT INVALID KEY
037400         MOVE 元－元号名称   TO 施術和暦
037410     END-READ.
016240     MOVE 施術年Ｗ            TO 施術年.
016250     MOVE 施術月Ｗ            TO 施術月.
016310*
016320     MOVE 印刷保険者番号Ｗ    TO 保険者番号.
016480     MOVE 請求先名称１Ｗ      TO 保険者名称１.
016490*     MOVE 請求先名称２Ｗ      TO 保険者名称２.
016590*
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
      *
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
016260     MOVE 被保険者氏名Ｗ      TO 被保険者氏名.
016300     MOVE 被保険者住所１Ｗ    TO 住所１.
016310     MOVE 被保険者住所２Ｗ    TO 住所２.
016700     MOVE 患者カナＷ          TO 患者カナ.
016710     MOVE 患者氏名Ｗ          TO 患者氏名.
016720     MOVE 男チェックＷ        TO 男チェック.
016730     MOVE 女チェックＷ        TO 女チェック.
016740     MOVE 明治チェックＷ      TO 明治チェック.
016750     MOVE 大正チェックＷ      TO 大正チェック.
016760     MOVE 昭和チェックＷ      TO 昭和チェック.
016770     MOVE 平成チェックＷ      TO 平成チェック.
      */元号修正↓↓↓/20190426
           MOVE 令和ＣＭＷ         TO 令和ＣＭ.
023070     MOVE 令和チェックＷ     TO 令和チェック.
017390*     MOVE 元号Ｗ              TO 患者和暦.
      */元号修正↑↑↑/20190426
016780     MOVE 患者年Ｗ            TO 患者年.
016790     MOVE 患者月Ｗ            TO 患者月.
016800     MOVE 患者日Ｗ            TO 患者日.
016810***     MOVE 印刷続柄Ｗ          TO 続柄.
016820*
016830     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
016840     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
016850     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
016850     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
016850     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
016850     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
016860*
017010********************
017020* 負傷データセット *
017030********************
017040* １部位 *
017050**********
017060     MOVE 負傷名Ｗ(1)       TO 負傷名１.
017070     MOVE 負傷年Ｗ(1)       TO 負傷年１.
017080     MOVE 負傷月Ｗ(1)       TO 負傷月１.
017090     MOVE 負傷日Ｗ(1)       TO 負傷日１.
017100     MOVE 初検年Ｗ(1)       TO 初検年１.
017110     MOVE 初検月Ｗ(1)       TO 初検月１.
017120     MOVE 初検日Ｗ(1)       TO 初検日１.
017130     MOVE 開始年Ｗ(1)       TO 開始年１.
017140     MOVE 開始月Ｗ(1)       TO 開始月１.
017150     MOVE 開始日Ｗ(1)       TO 開始日１.
017160     MOVE 終了年Ｗ(1)       TO 終了年１.
017170     MOVE 終了月Ｗ(1)       TO 終了月１.
017180     MOVE 終了日Ｗ(1)       TO 終了日１.
017190     MOVE 実日数Ｗ(1)       TO 実日数１.
017200     MOVE 治癒チェックＷ(1) TO 治癒チェック１.
017210     MOVE 中止チェックＷ(1) TO 中止チェック１.
017220     MOVE 転医チェックＷ(1) TO 転医チェック１.
017230**********
017240* ２部位 *
017250**********
017260     MOVE 負傷名Ｗ(2)       TO 負傷名２.
017270     MOVE 負傷年Ｗ(2)       TO 負傷年２.
017280     MOVE 負傷月Ｗ(2)       TO 負傷月２.
017290     MOVE 負傷日Ｗ(2)       TO 負傷日２.
017300     MOVE 初検年Ｗ(2)       TO 初検年２.
017310     MOVE 初検月Ｗ(2)       TO 初検月２.
017320     MOVE 初検日Ｗ(2)       TO 初検日２.
017330     MOVE 開始年Ｗ(2)       TO 開始年２.
017340     MOVE 開始月Ｗ(2)       TO 開始月２.
017350     MOVE 開始日Ｗ(2)       TO 開始日２.
017360     MOVE 終了年Ｗ(2)       TO 終了年２.
017370     MOVE 終了月Ｗ(2)       TO 終了月２.
017380     MOVE 終了日Ｗ(2)       TO 終了日２.
017390     MOVE 実日数Ｗ(2)       TO 実日数２.
017400     MOVE 治癒チェックＷ(2) TO 治癒チェック２.
017410     MOVE 中止チェックＷ(2) TO 中止チェック２.
017420     MOVE 転医チェックＷ(2) TO 転医チェック２.
017430**********
017440* ３部位 *
017450**********
017460     MOVE 負傷名Ｗ(3)       TO 負傷名３.
017470     MOVE 負傷年Ｗ(3)       TO 負傷年３.
017480     MOVE 負傷月Ｗ(3)       TO 負傷月３.
017490     MOVE 負傷日Ｗ(3)       TO 負傷日３.
017500     MOVE 初検年Ｗ(3)       TO 初検年３.
017510     MOVE 初検月Ｗ(3)       TO 初検月３.
017520     MOVE 初検日Ｗ(3)       TO 初検日３.
017530     MOVE 開始年Ｗ(3)       TO 開始年３.
017540     MOVE 開始月Ｗ(3)       TO 開始月３.
017550     MOVE 開始日Ｗ(3)       TO 開始日３.
017560     MOVE 終了年Ｗ(3)       TO 終了年３.
017570     MOVE 終了月Ｗ(3)       TO 終了月３.
017580     MOVE 終了日Ｗ(3)       TO 終了日３.
017590     MOVE 実日数Ｗ(3)       TO 実日数３.
017600     MOVE 治癒チェックＷ(3) TO 治癒チェック３.
017610     MOVE 中止チェックＷ(3) TO 中止チェック３.
017620     MOVE 転医チェックＷ(3) TO 転医チェック３.
017630**********
017640* ４部位 *
017650**********
017660     MOVE 負傷名Ｗ(4)       TO 負傷名４.
017670     MOVE 負傷年Ｗ(4)       TO 負傷年４.
017680     MOVE 負傷月Ｗ(4)       TO 負傷月４.
017690     MOVE 負傷日Ｗ(4)       TO 負傷日４.
017700     MOVE 初検年Ｗ(4)       TO 初検年４.
017710     MOVE 初検月Ｗ(4)       TO 初検月４.
017720     MOVE 初検日Ｗ(4)       TO 初検日４.
017730     MOVE 開始年Ｗ(4)       TO 開始年４.
017740     MOVE 開始月Ｗ(4)       TO 開始月４.
017750     MOVE 開始日Ｗ(4)       TO 開始日４.
017760     MOVE 終了年Ｗ(4)       TO 終了年４.
017770     MOVE 終了月Ｗ(4)       TO 終了月４.
017780     MOVE 終了日Ｗ(4)       TO 終了日４.
017790     MOVE 実日数Ｗ(4)       TO 実日数４.
017800     MOVE 治癒チェックＷ(4) TO 治癒チェック４.
017810     MOVE 中止チェックＷ(4) TO 中止チェック４.
017820     MOVE 転医チェックＷ(4) TO 転医チェック４.
017830**********
017840* ５部位 *
017850**********
017860     MOVE 負傷名Ｗ(5)       TO 負傷名５.
017870     MOVE 負傷年Ｗ(5)       TO 負傷年５.
017880     MOVE 負傷月Ｗ(5)       TO 負傷月５.
017890     MOVE 負傷日Ｗ(5)       TO 負傷日５.
017900     MOVE 初検年Ｗ(5)       TO 初検年５.
017910     MOVE 初検月Ｗ(5)       TO 初検月５.
017920     MOVE 初検日Ｗ(5)       TO 初検日５.
017930     MOVE 開始年Ｗ(5)       TO 開始年５.
017940     MOVE 開始月Ｗ(5)       TO 開始月５.
017950     MOVE 開始日Ｗ(5)       TO 開始日５.
017960     MOVE 終了年Ｗ(5)       TO 終了年５.
017970     MOVE 終了月Ｗ(5)       TO 終了月５.
017980     MOVE 終了日Ｗ(5)       TO 終了日５.
017990     MOVE 実日数Ｗ(5)       TO 実日数５.
018000     MOVE 治癒チェックＷ(5) TO 治癒チェック５.
018010     MOVE 中止チェックＷ(5) TO 中止チェック５.
018020     MOVE 転医チェックＷ(5) TO 転医チェック５.
018030**************
018040* 経過セット *
018050**************
018060*     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018080*             UNTIL ( 部位ＣＮＴ > 5 )
018110*         MOVE 印刷経過略称Ｗ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
018120*     END-PERFORM.
018130*****************************************
018140*     新規・継続チェックについて        *
018150*   ●新規...初検有り ●継続...初検なし *
018160*****************************************
018170     MOVE 新規チェックＷ    TO 新規チェック.
018180     MOVE 継続チェックＷ    TO 継続チェック.
018190********************
018200* 料金データセット *
018210********************
018220*    ****************************************************************
018230*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
018240*    ****************************************************************
018250     MOVE 初検料ＷＲ                   TO  初検料.
           MOVE 相談料ＷＲ                   TO  初検時相談料.
018260     MOVE 時間外チェックＷ             TO  時間外チェック.
018270     MOVE 休日チェックＷ               TO  休日チェック.
018280     MOVE 深夜チェックＷ               TO  深夜チェック.
018290     MOVE 初検加算料ＷＲ               TO  初検加算料.
           IF ((時間外チェックＷ NOT = SPACE) OR (深夜チェックＷ NOT = SPACE) OR
              (休日チェックＷ NOT = SPACE)) AND
              ((初検加算時Ｗ NOT = ZERO) OR (初検加算分Ｗ NOT = ZERO))
019130        MOVE 初検加算時Ｗ              TO  初検加算時
019140        MOVE 初検加算分Ｗ              TO  初検加算分
      *        MOVE "施術時間"                TO 初検加算ＣＭ
              MOVE ":"                       TO 初検加算区切
019150     END-IF.
018400     MOVE 再検料ＷＲ                   TO  再検料.
018410     MOVE 往療距離ＷＲ                 TO  往療距離.
018420     MOVE 往療回数ＷＲ                 TO  往療回数.
018430     MOVE 往療料ＷＲ                   TO  往療料.
018440     MOVE 夜間チェックＷ               TO  夜間チェック.
018450     MOVE 難路チェックＷ               TO  難路チェック.
018460     MOVE 暴風雨雪チェックＷ           TO  暴風雨雪チェック.
018470     MOVE 往療加算料ＷＲ               TO  往療加算料.
      */金属副子・運動後療の変更・追加/1805
           MOVE 金属回数Ｗ                   TO  金属回数.
018160     MOVE 金属副子加算料ＷＲ           TO  金属副子加算料.
           MOVE 運動回数Ｗ                   TO  運動回数.
           MOVE 運動料Ｗ                     TO  運動後療料.
018520     MOVE 施術情報提供料ＷＲ           TO  施術情報提供料.
018090     MOVE 明細書発行加算料ＷＲ         TO  明細書発行加算料.
018090     MOVE 明細書発行加算日ＷＲ         TO  明細書発行加算日.
018530     MOVE 小計Ｗ                       TO 小計.
018540********************
018550* 初回処置料セット *
018560********************
018570     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018580****             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
018590             UNTIL ( 部位ＣＮＴ > 5 )
018600         MOVE 初回処置料ＷＲ(部位ＣＮＴ) TO 初回処置料(部位ＣＮＴ)
018610     END-PERFORM.
018620     MOVE 初回処置料合計Ｗ         TO 初回処置料合計
018630*
018640     MOVE 施療料チェックＷ             TO 施療料チェック.
018650     MOVE 整復料チェックＷ             TO 整復料チェック.
018660     MOVE 固定料チェックＷ             TO 固定料チェック.
018670******************
018680* 基本料金セット *
018690******************
018700*     MOVE 冷罨法単価Ｗ                TO  冷罨法単価.
018710*     MOVE 温罨法単価Ｗ                TO  温罨法単価.
018720*     MOVE 電療単価Ｗ                  TO  電療単価.
018730********************
018740* 逓減毎料金セット *
018750********************
018760*    **********
018770*    * １部位 *
018780*    **********
018790     MOVE 後療単価１ＷＲ               TO 後療単価１.
018800     MOVE 後療回数１ＷＲ               TO 後療回数１.
018810     MOVE 後療料１ＷＲ                 TO 後療料１.
018820     MOVE 冷罨法回数１ＷＲ             TO 冷罨法回数１.
018830     MOVE 冷罨法料１ＷＲ               TO 冷罨法料１.
018840     MOVE 温罨法回数１ＷＲ             TO 温罨法回数１.
018850     MOVE 温罨法料１ＷＲ               TO 温罨法料１.
018860     MOVE 電療回数１ＷＲ               TO 電療回数１.
018870     MOVE 電療料１ＷＲ                 TO 電療料１.
018880     MOVE 小計１ＷＲ                   TO 小計１.
018890     IF ( 長期逓減率１ＷＲ NOT = ZERO )
018900        COMPUTE 長期逓減率１ = 長期逓減率１ＷＲ / 100
018910     END-IF.
018920     MOVE 長期込小計１ＷＲ             TO 長期込小計１.
018930*    **********
018940*    * ２部位 *
018950*    **********
018960     MOVE 後療単価２ＷＲ               TO 後療単価２.
018970     MOVE 後療回数２ＷＲ               TO 後療回数２.
018980     MOVE 後療料２ＷＲ                 TO 後療料２.
018990     MOVE 冷罨法回数２ＷＲ             TO 冷罨法回数２.
019000     MOVE 冷罨法料２ＷＲ               TO 冷罨法料２.
019010     MOVE 温罨法回数２ＷＲ             TO 温罨法回数２.
019020     MOVE 温罨法料２ＷＲ               TO 温罨法料２.
019030     MOVE 電療回数２ＷＲ               TO 電療回数２.
019040     MOVE 電療料２ＷＲ                 TO 電療料２.
019050     MOVE 小計２ＷＲ                   TO 小計２.
019060     IF ( 長期逓減率２ＷＲ NOT = ZERO )
019070        COMPUTE 長期逓減率２ = 長期逓減率２ＷＲ / 100
019080     END-IF.
019090     MOVE 長期込小計２ＷＲ             TO 長期込小計２.
019100*    ****************
019110*    * ３部位／８割 *
019120*    ****************
019130     MOVE 後療単価３８ＷＲ             TO 後療単価３８.
019140     MOVE 後療回数３８ＷＲ             TO 後療回数３８.
019150     MOVE 後療料３８ＷＲ               TO 後療料３８.
019160     MOVE 冷罨法回数３８ＷＲ           TO 冷罨法回数３８.
019170     MOVE 冷罨法料３８ＷＲ             TO 冷罨法料３８.
019180     MOVE 温罨法回数３８ＷＲ           TO 温罨法回数３８.
019190     MOVE 温罨法料３８ＷＲ             TO 温罨法料３８.
019200     MOVE 電療回数３８ＷＲ             TO 電療回数３８.
019210     MOVE 電療料３８ＷＲ               TO 電療料３８.
019220     MOVE 小計３８ＷＲ                 TO 小計３８.
019230     MOVE 多部位込小計３８ＷＲ         TO 多部位込小計３８.
019240     IF ( 長期逓減率３８ＷＲ NOT = ZERO )
019250        COMPUTE 長期逓減率３８ = 長期逓減率３８ＷＲ / 100
019260     END-IF.
019270     MOVE 長期込小計３８ＷＲ           TO 長期込小計３８.
      */ 逓減率 0.7→0.6 /42505
      *     IF (施術和暦年月ＷＲ >= 42505) AND (小計３８ＷＲ NOT = ZERO)
      *        MOVE "60"                      TO 逓減３８
      *        MOVE "0.6"                     TO 多部位３８
      *        MOVE "==="                     TO 逓減訂正３８ 多部位訂正３８
      *     END-IF.
019280*    ****************
019290*    * ３部位／10割 *
019300*    ****************
019310     MOVE 逓減開始月３０ＷＲ           TO 逓減開始月３０.
019320     MOVE 逓減開始日３０ＷＲ           TO 逓減開始日３０.
019330     MOVE 後療単価３０ＷＲ             TO 後療単価３０.
019340     MOVE 後療回数３０ＷＲ             TO 後療回数３０.
019350     MOVE 後療料３０ＷＲ               TO 後療料３０.
019360     MOVE 冷罨法回数３０ＷＲ           TO 冷罨法回数３０.
019370     MOVE 冷罨法料３０ＷＲ             TO 冷罨法料３０.
019380     MOVE 温罨法回数３０ＷＲ           TO 温罨法回数３０.
019390     MOVE 温罨法料３０ＷＲ             TO 温罨法料３０.
019400     MOVE 電療回数３０ＷＲ             TO 電療回数３０.
019410     MOVE 電療料３０ＷＲ               TO 電療料３０.
019420     MOVE 小計３０ＷＲ                 TO 小計３０.
019430     IF ( 長期逓減率３０ＷＲ NOT = ZERO )
019440        COMPUTE 長期逓減率３０ = 長期逓減率３０ＷＲ / 100
019450     END-IF.
019460     MOVE 長期込小計３０ＷＲ           TO 長期込小計３０.
019470*    ****************
019480*    * ４部位／５割 *
019490*    ****************
019500     MOVE 後療単価４５ＷＲ             TO 後療単価４５.
019510     MOVE 後療回数４５ＷＲ             TO 後療回数４５.
019520     MOVE 後療料４５ＷＲ               TO 後療料４５.
019530     MOVE 冷罨法回数４５ＷＲ           TO 冷罨法回数４５.
019540     MOVE 冷罨法料４５ＷＲ             TO 冷罨法料４５.
019550     MOVE 温罨法回数４５ＷＲ           TO 温罨法回数４５.
019560     MOVE 温罨法料４５ＷＲ             TO 温罨法料４５.
019570     MOVE 電療回数４５ＷＲ             TO 電療回数４５.
019580     MOVE 電療料４５ＷＲ               TO 電療料４５.
019590     MOVE 小計４５ＷＲ                 TO 小計４５.
019600     MOVE 多部位込小計４５ＷＲ         TO 多部位込小計４５.
019610     IF ( 長期逓減率４５ＷＲ NOT = ZERO )
019620        COMPUTE 長期逓減率４５ = 長期逓減率４５ＷＲ / 100
019630     END-IF.
019640     MOVE 長期込小計４５ＷＲ           TO 長期込小計４５.
019650*    ****************
019660*    * ４部位／８割 *
019670*    ****************
019680     MOVE 逓減開始月４８ＷＲ           TO 逓減開始月４８.
019690     MOVE 逓減開始日４８ＷＲ           TO 逓減開始日４８.
019700     MOVE 後療単価４８ＷＲ             TO 後療単価４８.
019710     MOVE 後療回数４８ＷＲ             TO 後療回数４８.
019720     MOVE 後療料４８ＷＲ               TO 後療料４８.
019730     MOVE 冷罨法回数４８ＷＲ           TO 冷罨法回数４８.
019740     MOVE 冷罨法料４８ＷＲ             TO 冷罨法料４８.
019750     MOVE 温罨法回数４８ＷＲ           TO 温罨法回数４８.
019760     MOVE 温罨法料４８ＷＲ             TO 温罨法料４８.
019770     MOVE 電療回数４８ＷＲ             TO 電療回数４８.
019780     MOVE 電療料４８ＷＲ               TO 電療料４８.
019790     MOVE 小計４８ＷＲ                 TO 小計４８.
019800     MOVE 多部位込小計４８ＷＲ         TO 多部位込小計４８.
019810     IF ( 長期逓減率４８ＷＲ NOT = ZERO )
019820        COMPUTE 長期逓減率４８ = 長期逓減率４８ＷＲ / 100
019830     END-IF.
019840     MOVE 長期込小計４８ＷＲ           TO 長期込小計４８.
      */ 逓減率 0.7→0.6 /42505
      *     IF (施術和暦年月ＷＲ >= 42505) AND (小計４８ＷＲ NOT = ZERO)
      *        MOVE "60"                      TO 逓減４８
      *        MOVE "0.6"                     TO 多部位４８
      *        MOVE "==="                     TO 逓減訂正４８ 多部位訂正４８
      *     END-IF.
019850*    ****************
019860*    * ４部位／10割 *
019870*    ****************
019880     MOVE 逓減開始月４０ＷＲ           TO 逓減開始月４０.
019890     MOVE 逓減開始日４０ＷＲ           TO 逓減開始日４０.
019900     MOVE 後療単価４０ＷＲ             TO 後療単価４０.
019910     MOVE 後療回数４０ＷＲ             TO 後療回数４０.
019920     MOVE 後療料４０ＷＲ               TO 後療料４０.
019930     MOVE 冷罨法回数４０ＷＲ           TO 冷罨法回数４０.
019940     MOVE 冷罨法料４０ＷＲ             TO 冷罨法料４０.
019950     MOVE 温罨法回数４０ＷＲ           TO 温罨法回数４０.
019960     MOVE 温罨法料４０ＷＲ             TO 温罨法料４０.
019970     MOVE 電療回数４０ＷＲ             TO 電療回数４０.
019980     MOVE 電療料４０ＷＲ               TO 電療料４０.
019990     MOVE 小計４０ＷＲ                 TO 小計４０.
020000     IF ( 長期逓減率４０ＷＲ NOT = ZERO )
020010        COMPUTE 長期逓減率４０ = 長期逓減率４０ＷＲ / 100
020020     END-IF.
020030     MOVE 長期込小計４０ＷＲ           TO 長期込小計４０.
020040*
020050*↓***********************************************************************
020060* ５部位／2.5割の印字は必要ない。
020070*------------------------------------------------------------------------*
020080*    *****************
020090*    * ５部位／2.5割 *
020100*    *****************
020110*     MOVE 後療単価５２ＷＲ             TO 後療単価５２.
020120*     MOVE 後療回数５２ＷＲ             TO 後療回数５２.
020130*     MOVE 後療料５２ＷＲ               TO 後療料５２.
020140*     MOVE 冷罨法回数５２ＷＲ           TO 冷罨法回数５２.
020150*     MOVE 冷罨法料５２ＷＲ             TO 冷罨法料５２.
020160*     MOVE 温罨法回数５２ＷＲ           TO 温罨法回数５２.
020170*     MOVE 温罨法料５２ＷＲ             TO 温罨法料５２.
020180*     MOVE 電療回数５２ＷＲ             TO 電療回数５２.
020190*     MOVE 電療料５２ＷＲ               TO 電療料５２.
020200*     MOVE 小計５２ＷＲ                 TO 小計５２.
020210*     MOVE 多部位込小計５２ＷＲ         TO 多部位込小計５２.
020220*     IF ( 長期逓減率５２ＷＲ NOT = ZERO )
020230*        COMPUTE 長期逓減率５２ = 長期逓減率５２ＷＲ / 100
020240*     END-IF.
020250*     MOVE 長期込小計５２ＷＲ           TO 長期込小計５２.
020260*↑***********************************************************************
020270*    ****************
020280*    * ５部位／５割 *
020290*    ****************
020340     MOVE 逓減開始月５５ＷＲ           TO 逓減開始月５５.
020350     MOVE 逓減開始日５５ＷＲ           TO 逓減開始日５５.
020360     MOVE 後療単価５５ＷＲ             TO 後療単価５５.
020370     MOVE 後療回数５５ＷＲ             TO 後療回数５５.
020380     MOVE 後療料５５ＷＲ               TO 後療料５５.
020390     MOVE 冷罨法回数５５ＷＲ           TO 冷罨法回数５５.
020400     MOVE 冷罨法料５５ＷＲ             TO 冷罨法料５５.
020410     MOVE 温罨法回数５５ＷＲ           TO 温罨法回数５５.
020420     MOVE 温罨法料５５ＷＲ             TO 温罨法料５５.
020430     MOVE 電療回数５５ＷＲ             TO 電療回数５５.
020440     MOVE 電療料５５ＷＲ               TO 電療料５５.
020450     MOVE 小計５５ＷＲ                 TO 小計５５.
020460     MOVE 多部位込小計５５ＷＲ         TO 多部位込小計５５.
020470     IF ( 長期逓減率５５ＷＲ NOT = ZERO )
020480        COMPUTE 長期逓減率５５ = 長期逓減率５５ＷＲ / 100
020490     END-IF.
020500     MOVE 長期込小計５５ＷＲ           TO 長期込小計５５.
020510*    ****************
020520*    * ５部位／８割 *
020530*    ****************
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
020540*     IF ( 小計５８ＷＲ NOT = ZERO )
020580*     MOVE 逓減開始月５８ＷＲ           TO 逓減開始月５８.
020590*     MOVE 逓減開始日５８ＷＲ           TO 逓減開始日５８.
020600*     MOVE 後療単価５８ＷＲ             TO 後療単価５８.
020610*     MOVE 後療回数５８ＷＲ             TO 後療回数５８.
020620*     MOVE 後療料５８ＷＲ               TO 後療料５８.
020630*     MOVE 冷罨法回数５８ＷＲ           TO 冷罨法回数５８.
020640*     MOVE 冷罨法料５８ＷＲ             TO 冷罨法料５８.
020650*     MOVE 温罨法回数５８ＷＲ           TO 温罨法回数５８.
020660*     MOVE 温罨法料５８ＷＲ             TO 温罨法料５８.
020670*     MOVE 電療回数５８ＷＲ             TO 電療回数５８.
020680*     MOVE 電療料５８ＷＲ               TO 電療料５８.
020690*     MOVE 小計５８ＷＲ                 TO 小計５８.
020700*     MOVE 多部位込小計５８ＷＲ         TO 多部位込小計５８.
020710*     IF ( 長期逓減率５８ＷＲ NOT = ZERO )
020720*        COMPUTE 長期逓減率５８ = 長期逓減率５８ＷＲ / 100
020730*     END-IF.
020740*     MOVE 長期込小計５８ＷＲ           TO 長期込小計５８.
020750*    ****************
020760*    * ５部位／10割 *
020770*    ****************
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
020830*     MOVE 逓減開始月５０ＷＲ           TO 逓減開始月５０.
020840*     MOVE 逓減開始日５０ＷＲ           TO 逓減開始日５０.
020850*     MOVE 後療単価５０ＷＲ             TO 後療単価５０.
020860*     MOVE 後療回数５０ＷＲ             TO 後療回数５０.
020870*     MOVE 後療料５０ＷＲ               TO 後療料５０.
020880*     MOVE 冷罨法回数５０ＷＲ           TO 冷罨法回数５０.
020890*     MOVE 冷罨法料５０ＷＲ             TO 冷罨法料５０.
020900*     MOVE 温罨法回数５０ＷＲ           TO 温罨法回数５０.
020910*     MOVE 温罨法料５０ＷＲ             TO 温罨法料５０.
020920*     MOVE 電療回数５０ＷＲ             TO 電療回数５０.
020930*     MOVE 電療料５０ＷＲ               TO 電療料５０.
020940*     MOVE 小計５０ＷＲ                 TO 小計５０.
020950*     MOVE 長期逓減率５０ＷＲ           TO 長期逓減率５０.
020960*     IF ( 長期逓減率５０ＷＲ NOT = ZERO )
020970*        COMPUTE 長期逓減率５０ = 長期逓減率５０ＷＲ / 100
020980*     END-IF.
020990*     MOVE 長期込小計５０ＷＲ           TO 長期込小計５０.
021000*
021010     MOVE 適用１Ｗ                     TO 適用１.
021020     MOVE 適用２Ｗ                     TO 適用２.
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
              MOVE 14           TO 連金運－会コード
              MOVE 1            TO 連金運－用紙種別
              CALL "KINUNRYO"
              CANCEL "KINUNRYO"
              MOVE 連金運－金属副子ＣＭ           TO 金属副子ＣＭ
              IF ( 金属副子加算料ＷＲ NOT = ZERO )
                 MOVE 金属副子ＣＭ                TO 金属副子
              END-IF
              PERFORM VARYING カウンタ FROM 1 BY 1
                        UNTIL カウンタ > 3
                 MOVE 連金運－金属副子月(1 カウンタ) TO 金属月(カウンタ)
                 MOVE 連金運－金属副子日(1 カウンタ) TO 金属日(カウンタ)
                 IF 連金運－金属副子月(1 カウンタ) NOT = ZERO
                    MOVE "月"                        TO 月(カウンタ)
                 END-IF
              END-PERFORM
              PERFORM VARYING カウンタ FROM 1 BY 1
                        UNTIL カウンタ > 5
                 MOVE 連金運－運動日(カウンタ)     TO 運動日(カウンタ)
              END-PERFORM
           END-IF.
021030*
021040     MOVE レセ－合計                   TO 合計.
021050*
021060*     IF ( 公費種別ＷＲ = 05 )
021070*        IF ( 施術和暦年月ＷＲ >= 41410 )
021080*           MOVE 負担割合チェックＷ     TO 負担割合
021090*        ELSE
021100*           MOVE レセ－一部負担金       TO 一部負担金
021110*        END-IF
021120*     ELSE
021130*        MOVE 負担割合チェックＷ        TO 負担割合
021140*     END-IF.
021150*
      *     MOVE レセ－一部負担金             TO 一部負担金.
021920     MOVE 負担割合Ｗ                   TO 負担割合.
      */被災者の場合、金額欄の負担割合は０にする。
      *     IF 受－資格証明区分 = 9
      *         MOVE ZERO TO 負担割合
      *     END-IF.
021930     MOVE NC"割"                       TO 負担割合固定.
021160     MOVE レセ－請求金額               TO 請求金額.
021170*
022410*------------------------------------------------------------------------*
      */長期頻回20241007/↓↓↓
           MOVE 部位継続月数Ｗ(1) TO 継続月数１.
           MOVE 部位継続月数Ｗ(2) TO 継続月数２.
           MOVE 部位継続月数Ｗ(3) TO 継続月数３.
           MOVE 部位継続月数Ｗ(4) TO 継続月数４.
           MOVE 部位継続月数Ｗ(5) TO 継続月数５.
      *
           IF レセ－長期頻回逓減率１ NOT = ZERO
               MOVE ZERO TO 長期逓減率１
018410         COMPUTE 頻回逓減率１ = 長期逓減率１ＷＲ / 100
           END-IF
           IF レセ－長期頻回逓減率２ NOT = ZERO
               MOVE ZERO TO 長期逓減率２
018410         COMPUTE 頻回逓減率２ = 長期逓減率２ＷＲ / 100
           END-IF
           IF レセ－長期頻回逓減率３８ NOT = ZERO
               MOVE ZERO TO 長期逓減率３８
018410         COMPUTE 頻回逓減率３８ = 長期逓減率３８ＷＲ / 100
           END-IF
           IF レセ－長期頻回逓減率３０ NOT = ZERO
               MOVE ZERO TO 長期逓減率３０
018410         COMPUTE 頻回逓減率３０ = 長期逓減率３０ＷＲ / 100
           END-IF
           IF レセ－長期頻回逓減率４８ NOT = ZERO
               MOVE ZERO TO 長期逓減率４８
018410         COMPUTE 頻回逓減率４８ = 長期逓減率４８ＷＲ / 100
           END-IF
           IF レセ－長期頻回逓減率４０ NOT = ZERO
               MOVE ZERO TO 長期逓減率４０
018410         COMPUTE 頻回逓減率４０ = 長期逓減率４０ＷＲ / 100
           END-IF
      */長期頻回20241007/↑↑↑
      */長期頻回コメント/20241007↓↓↓
022420** 長期頻回の時、摘要欄に内容を記載
      **
      *     MOVE SPACE                     TO 長期頻回Ｗ.
      *     IF (レセ－部位継続月数(1) > 5) OR (レセ－部位継続月数(2) > 5) OR
      *        (レセ－部位継続月数(3) > 5) OR (レセ－部位継続月数(4) > 5) OR
      *        (レセ－部位継続月数(5) > 5)
      *        MOVE "長期頻回該当："       TO 長期頻回ＣＭ
      *     END-IF.
      *     IF (レセ－部位継続月数(1) > 5)
      *        MOVE レセ－部位継続月数(1)  TO 月数Ｗ
      *        MOVE 負傷名Ｗ(1)            TO 負傷名ＷＲ(1)
      *        STRING "(1)"                DELIMITED BY SIZE
      *               負傷名ＷＰ(1)        DELIMITED BY "　"
      *               "、継続月数"         DELIMITED BY SIZE
      *               月数Ｗ               DELIMITED BY SIZE
      *               "月"                 DELIMITED BY SIZE
      *          INTO 長期頻回１ＷＴ
      *        END-STRING
      *     END-IF.
      *     IF (レセ－部位継続月数(2) > 5)
      *        MOVE レセ－部位継続月数(2)  TO 月数Ｗ
      *        MOVE 負傷名Ｗ(2)            TO 負傷名ＷＲ(2)
      *        STRING "(2)"                DELIMITED BY SIZE
      *               負傷名ＷＰ(2)        DELIMITED BY "　"
      *               "、継続月数"         DELIMITED BY SIZE
      *               月数Ｗ               DELIMITED BY SIZE
      *               "月"                 DELIMITED BY SIZE
      *          INTO 長期頻回２ＷＴ
      *        END-STRING
      *     END-IF.
      *     IF (レセ－部位継続月数(3) > 5)
      *        MOVE レセ－部位継続月数(3)  TO 月数Ｗ
      *        MOVE 負傷名Ｗ(3)            TO 負傷名ＷＲ(3)
      *        STRING "(3)"                DELIMITED BY SIZE
      *               負傷名ＷＰ(3)        DELIMITED BY "　"
      *               "、継続月数"         DELIMITED BY SIZE
      *               月数Ｗ               DELIMITED BY SIZE
      *               "月"                 DELIMITED BY SIZE
      *          INTO 長期頻回３ＷＴ
      *        END-STRING
      *     END-IF.
      *     IF (レセ－部位継続月数(4) > 5)
      *        MOVE レセ－部位継続月数(4)  TO 月数Ｗ
      *        MOVE 負傷名Ｗ(4)            TO 負傷名ＷＲ(4)
      *        STRING "(4)"                DELIMITED BY SIZE
      *               負傷名ＷＰ(4)        DELIMITED BY "　"
      *               "、継続月数"         DELIMITED BY SIZE
      *               月数Ｗ               DELIMITED BY SIZE
      *               "月"                 DELIMITED BY SIZE
      *          INTO 長期頻回４ＷＴ
      *        END-STRING
      *     END-IF.
      *     IF (レセ－部位継続月数(5) > 5)
      *        MOVE レセ－部位継続月数(5)  TO 月数Ｗ
      *        MOVE 負傷名Ｗ(5)            TO 負傷名ＷＲ(5)
      *        STRING "(5)"                DELIMITED BY SIZE
      *               負傷名ＷＰ(5)        DELIMITED BY "　"
      *               "、継続月数"         DELIMITED BY SIZE
      *               月数Ｗ               DELIMITED BY SIZE
      *               "月"                 DELIMITED BY SIZE
      *          INTO 長期頻回５ＷＴ
      *        END-STRING
      *     END-IF.
      *     MOVE 長期頻回ＣＭ   TO 文字１Ｗ.
      *     MOVE 長期頻回１ＷＴ TO 文字２Ｗ.
      *     CALL プログラム名Ｗ WITH C LINKAGE
      *                   USING BY REFERENCE 文字１Ｗ
      *                         BY REFERENCE 文字２Ｗ.
      *     MOVE 長期頻回２ＷＴ TO 文字２Ｗ.
      *     CALL プログラム名Ｗ WITH C LINKAGE
      *                   USING BY REFERENCE 文字１Ｗ
      *                         BY REFERENCE 文字２Ｗ.
      *     MOVE 長期頻回３ＷＴ TO 文字２Ｗ.
      *     CALL プログラム名Ｗ WITH C LINKAGE
      *                   USING BY REFERENCE 文字１Ｗ
      *                         BY REFERENCE 文字２Ｗ.
      *     MOVE 長期頻回４ＷＴ TO 文字２Ｗ.
      *     CALL プログラム名Ｗ WITH C LINKAGE
      *                   USING BY REFERENCE 文字１Ｗ
      *                         BY REFERENCE 文字２Ｗ.
      *     MOVE 長期頻回５ＷＴ TO 文字２Ｗ.
      *     CALL プログラム名Ｗ WITH C LINKAGE
      *                   USING BY REFERENCE 文字１Ｗ
      *                         BY REFERENCE 文字２Ｗ.
      *     MOVE 文字１Ｗ       TO 長期頻回.
      **
      */長期頻回コメント/20241007↑↑↑
021180**********************
021190* 施術所データセット *
021200**********************
           MOVE 都道府県ＪＩＳＷ       TO 都道府県番号.
021210* 施術ID
           IF 保険種別Ｗ = 01 OR 08 OR 05
021220        MOVE 県施術ＩＤＷ           TO 県施術ＩＤ
           ELSE
021260        MOVE 接骨師会会員番号Ｗ     TO 県施術ＩＤ
           END-IF.
021240*
021250     MOVE 柔整師番号Ｗ           TO 柔整師番号.
021270***     MOVE 定額制受理番号Ｗ       TO 定額制受理番号.
021280*
021290     MOVE 施術所郵便番号１Ｗ     TO 施術所郵便番号１.
021300     MOVE 施術所郵便番号２Ｗ     TO 施術所郵便番号２.
021310     MOVE 施術所住所１Ｗ         TO 施術所住所１.
021320     MOVE 施術所住所２Ｗ         TO 施術所住所２.
021330     MOVE 接骨院名Ｗ             TO 接骨院名.
021340     MOVE 施術所電話番号Ｗ       TO 施術所電話番号.
021350     MOVE 代表者カナＷ           TO 代表者カナ.
021360     MOVE 代表者名Ｗ             TO 代表者名.
021370*
021380* / 柔整師・患者委任日 /
037370     MOVE 柔整師和暦Ｗ           TO 元－元号区分.
037380     READ 元号マスタ
037390     NOT INVALID KEY
037400         MOVE 元－元号名称       TO 受理和暦
037410     END-READ.
021390     MOVE 柔整師年Ｗ             TO 受理年.
021400     MOVE 柔整師月Ｗ             TO 受理月.
021410     MOVE 柔整師日Ｗ             TO 受理日.
021420* ( 委任年月日 印刷するか )
021430     IF ( 連入－委任印刷  = ZERO )
037370        MOVE 患者委任和暦Ｗ     TO 元－元号区分
037380        READ 元号マスタ
037390        NOT INVALID KEY
037400            MOVE 元－元号名称   TO 委任和暦
037410        END-READ
021440        MOVE 患者委任年Ｗ       TO 委任年
021450        MOVE 患者委任月Ｗ       TO 委任月
021460        MOVE 患者委任日Ｗ       TO 委任日
021470     END-IF.
021480*
           IF (会長委任フラグ = SPACE)
021500*        MOVE NC"○" TO 委任しない
010110        MOVE SPACE  TO 会長委任コメント１
010110        MOVE SPACE  TO 会長委任コメント２
010110        MOVE SPACE  TO 会長委任コメント３
           ELSE
021500        MOVE NC"○" TO 委任する
010110        MOVE "「療養費の受領を、協定に基づき、" TO 会長委任コメント１
010110        MOVE "(公社)愛知県柔道整復師会会長に"   TO 会長委任コメント２
010110        MOVE "1.委任します　2.委任しません」"   TO 会長委任コメント３
           END-IF.
           IF (口座印字フラグ = "YES")
              MOVE 金融機関名１Ｗ   TO 金融機関名１
              MOVE 金融機関名２Ｗ   TO 金融機関名２
              MOVE 金融機関名３Ｗ   TO 金融機関名３
              MOVE 金融機関名４Ｗ   TO 金融機関名４
              MOVE 支店名１Ｗ       TO 支店名１
              MOVE 支店名２Ｗ       TO 支店名２
              MOVE 支店名３Ｗ       TO 支店名３
              MOVE 支店名４Ｗ       TO 支店名４
              MOVE 振込チェックＷ   TO 振込チェック
              MOVE 普通チェックＷ   TO 普通チェック
              MOVE 当座チェックＷ   TO 当座チェック
              MOVE 銀行チェックＷ   TO 銀行チェック
              MOVE 金庫チェックＷ   TO 金庫チェック
              MOVE 農協チェックＷ   TO 農協チェック
              MOVE 本店チェックＷ   TO 本店チェック
              MOVE 支店チェックＷ   TO 支店チェック
              MOVE 本支所チェックＷ TO 本支所チェック
021650        MOVE 口座番号Ｗ       TO 口座番号
021660        MOVE 口座名義人カナＷ TO 口座名義人カナ
021670        MOVE 口座名義人Ｗ     TO 口座名義人
           END-IF.
021720*
021730**-------------------------------------------------------------------------**
021740*   平成14年6月から4部位目・5部位目の逓減率が45→33に変更。
021750*   それに伴い、平成14年6月より前のレセプトを出力する場合、
021760*   印刷されている33を45に、0.33を0.45に上から印字する。
021770*   (４部位目・５部位目があった時のみ）
021910*-------------------------------------------------------------------------*
021920*--- ※ レセ摘要再セットは、この印刷セットSECTION の最後にやること！ -----*
021930     PERFORM レセ摘要再セット.
021940*-------------------------------------------------------------------------*
021950*
021960*****     PERFORM テスト印字処理.
021970*
021980*================================================================*
021990 項目初期化 SECTION.
022000*================================================================*
022010     INITIALIZE 施術所情報Ｗ.
022020     INITIALIZE 受診者情報Ｗ.
022030     INITIALIZE 負傷情報Ｗ.
022040     INITIALIZE 料金情報Ｗ.
022050     INITIALIZE 備考情報Ｗ.
022060     INITIALIZE 料金１ＷＲ.
022070     INITIALIZE 料金２ＷＲ.
022080     INITIALIZE 料金３ＷＲ.
022090     MOVE SPACE TO YAI6123P.
022100***     INITIALIZE YAI6123P.
022110*
021920*================================================================*
021930 基本情報取得 SECTION.
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
022390*================================================================*
022400 施術所情報取得 SECTION.
022410*================================================================*
022420**************************************************
022430* 本院データを使用し、以下の情報を取得           *
022440* ● 柔整師番号.. 柔整師番号Ｗに格納             *
022450* ● 会員番号 ... 接骨師会会員番号Ｗに格納       *
022460* ● 代表者名 ... 代表者名Ｗに格納               *
022470* ● 住所1,2   ...施術所住所1,2Ｗに格納          *
022480* ● 電話番号 ... 施術所電話番号Ｗに格納         *
022490**************************************************
022500     MOVE ZERO  TO 施情－施術所番号.
022510     READ 施術所情報マスタ
022520     INVALID KEY
022530         CONTINUE
022540     NOT INVALID KEY
022550*
022590         MOVE 施情－新柔整師番号      TO 柔整師番号Ｗ
022610*
022620         MOVE 施情－接骨師会会員番号  TO 接骨師会会員番号Ｗ
022630*
022640         MOVE 施情－郵便番号１        TO 施術所郵便番号１Ｗ
022650         MOVE 施情－郵便番号２        TO 施術所郵便番号２Ｗ
022660         MOVE 施情－代表者カナ        TO 代表者カナＷ
022670         MOVE 施情－代表者名          TO 代表者名Ｗ
022680         MOVE 施情－接骨院名          TO 接骨院名Ｗ
               MOVE 施情－都道府県ＪＩＳ    TO 都道府県ＪＩＳＷ
022690*
022700         MOVE 施情－住所１            TO 施術所住所１Ｗ
022710         MOVE 施情－住所２            TO 施術所住所２Ｗ
022720*
022730         MOVE 施情－電話番号          TO 施術所電話番号Ｗ
022750**
022760** 振込先情報
022770         MOVE 施情－取引先銀行名      TO 取引先銀行名Ｗ
022780*         UNSTRING 施情－取引先銀行名  DELIMITED BY "銀行"
022790*           INTO 取引先銀行名Ｗ
022800*         END-UNSTRING
022810*
022820         MOVE 施情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
022830*         UNSTRING 施情－取引先銀行支店名  DELIMITED BY "支店"
022840*           INTO 取引先銀行支店名Ｗ
022850*         END-UNSTRING
022860*
022870         MOVE 施情－預金種別          TO 預金種別Ｗ
022880         MOVE 施情－口座番号          TO 口座番号Ｗ
022890         MOVE 施情－口座名義人        TO 口座名義人Ｗ
022900         MOVE 施情－口座名義人カナ    TO 口座名義人カナＷ
022910*
022920         EVALUATE 預金種別Ｗ
022930         WHEN 1
022940             MOVE NC"（普通）" TO 預金種別コメントＷ
022950         WHEN 2
022960             MOVE NC"（当座）" TO 預金種別コメントＷ
022970         WHEN OTHER
022980             MOVE SPACE        TO 預金種別コメントＷ
022990         END-EVALUATE
023000*
023010     END-READ.
023020*
      */組合・共済・自衛官は全て会口座を印刷する/190702
      *     IF ((保険者番号ＷＲ(3:2) NOT = "23") AND
            IF (保険種別ＷＲ = 3 OR 4 OR 9) OR
              ((保険種別ＷＲ = 1) AND
               (保険者番号ＷＲ = "133280" OR "133231" OR "133033"))
023500** 振込先情報  / 会情報マスタより振込先情報を取得 /
023520         MOVE ZERO  TO  会情－柔整鍼灸区分
023510         MOVE 14    TO  会情－協会コード
023520         MOVE ZERO  TO  会情－保険種別
023530         MOVE ZERO  TO  会情－変更和暦年月
023540         READ 会情報マスタ
023550         NOT INVALID KEY
023560             MOVE 会情－取引先銀行名      TO 取引先銀行名Ｗ
023570             MOVE 会情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
023580             MOVE 会情－預金種別          TO 預金種別Ｗ
023590             MOVE 会情－口座番号          TO 口座番号Ｗ
023600             MOVE 会情－口座名義人        TO 口座名義人Ｗ
023610             MOVE 会情－口座名義人カナ    TO 口座名義人カナＷ
023620*
023680             EVALUATE 預金種別Ｗ
023690             WHEN 1
023700                 MOVE NC"（普通）" TO 預金種別コメントＷ
023710             WHEN 2
023720                 MOVE NC"（当座）" TO 預金種別コメントＷ
023730             WHEN OTHER
023740                 MOVE SPACE        TO 預金種別コメントＷ
023750             END-EVALUATE
023760*
023770*             MOVE 会情－接骨師会会長名    TO 会長名Ｗ
023780         END-READ
           END-IF.
023030*--------------------------------------------------------------------------*      */現状は振込のみ対応
           MOVE NC"○" TO 振込チェックＷ.
      *
           EVALUATE 預金種別Ｗ
           WHEN 1
               MOVE NC"○" TO 普通チェックＷ
           WHEN 2
               MOVE NC"○" TO 当座チェックＷ
           END-EVALUATE.
      *
009745     IF 取引先銀行名Ｗ NOT = SPACE
009746        PERFORM VARYING カウンタ FROM 40 BY -1
009747                  UNTIL (取引先銀行名Ｗ(カウンタ:1) NOT = SPACE) OR
009748                        (カウンタ <= ZERO)
009749            CONTINUE
009750        END-PERFORM
009751        IF カウンタ > 4
009752           IF 取引先銀行名Ｗ(カウンタ - 3 : 4)  = "銀行"
009753              MOVE  取引先銀行名Ｗ(1:カウンタ - 4)   TO 金融機関名Ｗ
009754              MOVE NC"○" TO 銀行チェックＷ
009755           ELSE
009756              IF 取引先銀行名Ｗ(カウンタ - 3 : 4)  = "金庫"
009757                 MOVE  取引先銀行名Ｗ(1:カウンタ - 4)   TO 金融機関名Ｗ
009758                 MOVE NC"○" TO 金庫チェックＷ
009759              ELSE
009760                 IF 取引先銀行名Ｗ(カウンタ - 3 : 4)  = "農協"
009761                    MOVE  取引先銀行名Ｗ(1:カウンタ - 4)   TO 金融機関名Ｗ
009762                    MOVE NC"○" TO 農協チェックＷ
009763                 ELSE
009764                    MOVE  取引先銀行名Ｗ  TO 金融機関名Ｗ
      */省略時は銀行とする
                          MOVE NC"○" TO 銀行チェックＷ
009765                 END-IF
009766              END-IF
009767           END-IF
009768        ELSE
009769           MOVE  取引先銀行名Ｗ  TO 金融機関名Ｗ
      */省略時は銀行とする
                 MOVE NC"○" TO 銀行チェックＷ
009770        END-IF
009771     END-IF.
009779*
009780     IF 取引先銀行支店名Ｗ NOT = SPACE
009781        PERFORM VARYING カウンタ FROM 40 BY -1
009782                  UNTIL (取引先銀行支店名Ｗ(カウンタ:1) NOT = SPACE) OR
009783                        (カウンタ <= ZERO)
009784            CONTINUE
009785        END-PERFORM
009786        IF カウンタ >= 4
009787           IF 取引先銀行支店名Ｗ(カウンタ - 3 : 4)  = "本店"
009788              MOVE  取引先銀行支店名Ｗ(1:カウンタ - 4)   TO 支店名Ｗ
009789              MOVE NC"○" TO 本店チェックＷ
009790           ELSE
009791              IF 取引先銀行支店名Ｗ(カウンタ - 3 : 4)  = "支店"
009792                 MOVE  取引先銀行支店名Ｗ(1:カウンタ - 4)   TO 支店名Ｗ
009793                 MOVE NC"○" TO 支店チェックＷ
009794              ELSE
009791                 IF 取引先銀行支店名Ｗ(カウンタ - 3 : 4)  = "支所"
009792                    MOVE  取引先銀行支店名Ｗ(1:カウンタ - 4)   TO 支店名Ｗ
009793                    MOVE NC"○" TO 本支所チェックＷ
009794                 ELSE
009791                     IF 取引先銀行支店名Ｗ(カウンタ - 3 : 4)  = "本所"
009792                        MOVE  取引先銀行支店名Ｗ(1:カウンタ - 4)   TO 支店名Ｗ
009793                        MOVE NC"○" TO 本支所チェックＷ
009794                     ELSE
009800                         MOVE  取引先銀行支店名Ｗ  TO 支店名Ｗ
      */省略時は支店とする
                               MOVE NC"○" TO 支店チェックＷ
009801                     END-IF
009804                 END-IF
009805              END-IF
009806           END-IF
009807        ELSE
009808           MOVE  取引先銀行支店名Ｗ  TO 支店名Ｗ
      */省略時は支店とする
                 MOVE NC"○" TO 支店チェックＷ
009809        END-IF
009810     END-IF.
023040* ＩＤ管理マスタより　施術ＩＤを取得する。
023050*
023060     MOVE "23"  TO ＩＤ管－保険種別.
023070     PERFORM 県施術ＩＤセット.
023080*
023090*================================================================*
023100 県施術ＩＤセット SECTION.
023110*
023120*/ MOVE <保険者番号ＷＲ(X:X) 又は、固定値> TO ＩＤ管－保険種別.
023130     MOVE 01                   TO ＩＤ管－ＩＤ区分.
023140     MOVE ZERO                 TO ＩＤ管－施術所番号.
023150     MOVE SPACE                TO ＩＤ管－保険者番号.
023160     READ ＩＤ管理マスタ
023170     NOT INVALID KEY
023180         MOVE ＩＤ管－施術ＩＤ番号   TO 県施術ＩＤＷ
023190     END-READ.
023200*
024390*================================================================*
024400 受診者情報取得 SECTION.
024410*================================================================*
024420**************************************************
024430* 連結データから受診者情報Ｆより以下の情報を取得 *
024440* ● 施術年 ..... 施術年Ｗに格納                 *
024450* ● 施術月 ..... 施術月Ｗに格納                 *
024460* ● 記号 ....... 記号Ｗに格納                   *
024470* ● 番号 ....... 番号Ｗに格納                   *
024480* ● 保険者番号 . 保険者番号Ｗに格納             *
024490* ● 保険種別 ... 保険種別Ｗに格納               *
024500* ● 被保険者カナ.被保険者カナＷに格納           *
024510* ● 被保険者氏名.被保険者氏名Ｗに格納           *
024520* ● 住所１ ......被保険者住所１Ｗに格納         *
024530* ● 住所２ ......被保険者住所２Ｗに格納         *
024540* ● 患者カナ ....患者カナＷに格納               *
024550* ● 患者氏名 ....患者氏名Ｗに格納               *
024560* ● 患者性別 ....区分によりチェックに"○"を格納 *
024570* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
024580* ● 患者年 ......患者年Ｗに格納                 *
024590* ● 患者月 ......患者月Ｗに格納                 *
024600* ● 患者日 ......患者日Ｗに格納                 *
024610* ● 続柄 ........名称マスタより続柄Ｗに取得     *
024620**************************************************
024630     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
024640     MOVE 施術年ＷＲ         TO 受－施術年.
024650     MOVE 施術月ＷＲ         TO 受－施術月.
024660     MOVE 患者コードＷＲ     TO 受－患者コード.
024670     READ 受診者情報Ｆ
024680     INVALID KEY
024690         CONTINUE
024700*            /* ありえない */
024710     NOT INVALID KEY
               MOVE 受－施術和暦       TO 施術和暦Ｗ
024720         MOVE 受－施術年         TO 施術年Ｗ
024730         MOVE 受－施術月         TO 施術月Ｗ
024740*         MOVE 受－記号           TO 記号Ｗ
024750*         MOVE 受－番号           TO 番号Ｗ
      *-----------------------------------------------------------------*
               MOVE SPACE TO 連暗号複合－暗号情報
      *
      *        / 連暗号複合－入力情報セット /
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
      *-----------------------------------------------------------------*
024760         MOVE 受－保険者番号     TO 保険者番号Ｗ
      */国保のみ左詰め*/070410
               IF 受－保険種別 NOT = 01
                   PERFORM 保険者番号右詰め
                   MOVE 保険者番号右詰めＷ TO 保険者番号Ｗ
               END-IF
024770         MOVE 受－保険種別       TO 保険種別Ｗ
      */全国土木建設・全国左官タイル塗装業、全国板金業は組合
               IF 受－保険者番号(1:6) = "133033" OR "133231" OR "133280"
008480             MOVE 03  TO 保険種別Ｗ
               END-IF
024780*
024790** 全国土木の枝番削除
024800         IF ( 受－保険種別 = 01 ) AND ( 受－保険者番号(1:6) = "133033" )
024810            MOVE 受－保険者番号(1:6)  TO 保険者番号Ｗ
024820         END-IF
029750         MOVE 受－費用負担者番号助成 TO 市町村番号Ｗ
029760         MOVE 受－受益者番号助成     TO 受給者番号Ｗ
024850**
024860         MOVE 受－被保険者カナ   TO 被保険者カナＷ
024870         MOVE 受－被保険者氏名   TO 被保険者氏名Ｗ
024880         MOVE 受－郵便番号１     TO 郵便番号１Ｗ
024890         MOVE 受－郵便番号２     TO 郵便番号２Ｗ
024900         MOVE 受－住所１         TO 被保険者住所１Ｗ
024910         MOVE 受－住所２         TO 被保険者住所２Ｗ
      */ 電話番号追加 /42505
               IF 受－電話番号 NOT = SPACE
                  STRING "電話:"        DELIMITED BY SIZE
                         受－電話番号   DELIMITED BY SPACE
                    INTO 電話番号Ｗ
                  END-STRING
               ELSE
                  IF 受－患者電話番号 NOT = SPACE
                     STRING "電話:"            DELIMITED BY SIZE
                            受－患者電話番号   DELIMITED BY SPACE
                       INTO 電話番号Ｗ
                     END-STRING
                  END-IF
               END-IF
024880         MOVE 受－患者郵便番号１ TO 患者郵便番号１Ｗ
024890         MOVE 受－患者郵便番号２ TO 患者郵便番号２Ｗ
024920         MOVE 受－患者住所１     TO 患者住所１Ｗ
024930         MOVE 受－患者住所２     TO 患者住所２Ｗ
024940         MOVE 受－患者カナ       TO 患者カナＷ
024950         MOVE 受－患者氏名       TO 患者氏名Ｗ
024960         EVALUATE 受－患者性別
024970         WHEN 1
024980             MOVE NC"○"  TO 男チェックＷ
024990         WHEN 2
025000             MOVE NC"○"  TO 女チェックＷ
025010         END-EVALUATE
025020         EVALUATE 受－患者和暦
025030         WHEN 1
025040             MOVE NC"○"  TO 明治チェックＷ
025050         WHEN 2
025060             MOVE NC"○"  TO 大正チェックＷ
025070         WHEN 3
025080             MOVE NC"○"  TO 昭和チェックＷ
025090         WHEN 4
025100             MOVE NC"○"  TO 平成チェックＷ
      */元号修正/20190426
023060         WHEN 5
                   MOVE "5令"   TO 令和ＣＭＷ
023070             MOVE NC"○"  TO 令和チェックＷ
025110         END-EVALUATE
      */元号修正/↓↓↓20190426
029310         IF 受－患者和暦 > 4
037370             MOVE 受－患者和暦     TO 元－元号区分
037380             READ 元号マスタ
037390             NOT INVALID KEY
037400                 MOVE 元－元号名称 TO 元号Ｗ
037410             END-READ
029330         END-IF
      */元号修正/↑↑↑20190426
025120         MOVE 受－患者年  TO 患者年Ｗ
025130         MOVE 受－患者月  TO 患者月Ｗ
025140         MOVE 受－患者日  TO 患者日Ｗ
025150*
025160         MOVE SPACE       TO 続柄Ｗ
025170         IF ( 本人家族区分ＷＲ = 1 )
025180            MOVE NC"本人" TO 続柄Ｗ
025190         ELSE
025200            PERFORM 家族続柄セット
025210         END-IF
025220**
025230         PERFORM 保険種別セット
025330**
025340         EVALUATE 受－助成負担金免除
025350         WHEN 0
025360             IF ( レセ－受給者負担額 = ZERO )
025370                MOVE NC"○" TO 負担金無チェックＷ
025380             ELSE
025390                MOVE NC"○" TO 負担金有チェックＷ
025400             END-IF
025410         WHEN OTHER
025420             MOVE NC"○"    TO 負担金無チェックＷ
025430         END-EVALUATE
025440*
025450     END-READ.
025460**
025470*
025480     EVALUATE 助成種別ＷＲ
025490     WHEN  53
025500         MOVE NC"○"  TO 身チェックＷ
025510     WHEN  55
025520         MOVE NC"○"  TO 乳チェックＷ
025530     WHEN  52
025540         MOVE NC"○"  TO 母チェックＷ
025550     END-EVALUATE.
025560*
025570*================================================================*
025580 家族続柄セット SECTION.
025590*
025600     MOVE 05          TO 名－区分コード.
025610     MOVE 受－続柄    TO 名－名称コード.
025620     READ 名称マスタ
025630     INVALID KEY
025640         MOVE SPACE    TO 続柄Ｗ
025650     NOT INVALID KEY
025660         MOVE 名－略称 TO 続柄Ｗ
025670     END-READ.
025680*
025690*================================================================*
025700 保険種別セット SECTION.
025710*
022660     EVALUATE 受－保険種別
022670     WHEN 01
      */全国土木建設・全国左官タイル塗装業、全国板金業は組合
               IF 受－保険者番号(1:6) = "133033" OR "133231" OR "133280"
008480             MOVE NC"○"  TO 組合チェックＷ
               ELSE
016860             MOVE NC"○"  TO 国保チェックＷ
               END-IF
022700     WHEN 02
022710     WHEN 06
022750     WHEN 07
022720        MOVE NC"○"        TO 社保チェックＷ
022730     WHEN 03
022740        MOVE NC"○"        TO 組合チェックＷ
           WHEN 04
              MOVE NC"○"        TO 共済チェックＷ
           WHEN 09
              MOVE NC"○"        TO 自チェックＷ
           WHEN 08
              MOVE NC"○"        TO 退職チェックＷ
           WHEN 05
              MOVE NC"○"        TO 後期チェックＷ
022770     END-EVALUATE.
025970*
           IF 受－助成種別 = ZERO
              MOVE NC"○" TO 単独チェックＷ
           ELSE
              MOVE NC"○" TO ２併チェックＷ
           END-IF.
      **/本家区分はどれか１つに○をする。←訂正/110216
      */本人家族６歳と高一高７を独立して○付けする(下記以外)/110216
           IF 受－本人家族区分 = 1
               MOVE NC"○" TO 本人チェックＷ
           ELSE
               MOVE NC"○" TO 家族チェックＷ
           END-IF
           IF (受－保険種別 = 01 OR 08) AND
              (受－特別区分 = 1 OR 2 OR 3 OR 6)
               MOVE SPACE TO 本人チェックＷ 家族チェックＷ
           END-IF
           IF 受－保険種別 = 05
               EVALUATE 受－特別区分
               WHEN 1
               WHEN 2
                   MOVE NC"○" TO 高一チェックＷ
               WHEN 3
                   MOVE NC"○" TO 高７チェックＷ
               END-EVALUATE
               MOVE SPACE TO 本人チェックＷ 家族チェックＷ
           ELSE
028984         EVALUATE 受－特別区分
               WHEN 1
               WHEN 2
                   MOVE NC"○" TO 高一チェックＷ
               WHEN 3
                   MOVE NC"○" TO 高７チェックＷ
028991         WHEN 6
                   MOVE NC"○" TO ６歳チェックＷ
028999         END-EVALUATE
           END-IF.
           EVALUATE レセ－負担割合
           WHEN ZERO
               MOVE NC"○" TO １０割チェックＷ
           WHEN 1
               MOVE NC"○" TO ９割チェックＷ
      */前期高齢者１割は、給付割合を８割にする。(国が１割負担するため、患者１割、保険者８割、国１割となる)
               IF (受－保険種別 NOT = 05 ) AND (受－特別区分 = 1) AND (受－施術和暦年月 >= 42004)
                   MOVE SPACE  TO ９割チェックＷ
                   MOVE NC"○" TO ８割チェックＷ
               END-IF
           WHEN 2
               MOVE NC"○" TO ８割チェックＷ
           WHEN 3
               MOVE NC"○" TO ７割チェックＷ
           END-EVALUATE.
025980*================================================================*
025990 老人負担金免除セット SECTION.
026000*
026052     EVALUATE 受－老人負担金免除
026053     WHEN 2
026054     WHEN 3
026055        MOVE NC"○"  TO ７０歳８チェックＷ
026058     WHEN OTHER
026060        MOVE NC"○"  TO ７０歳９チェックＷ
026061     END-EVALUATE.
026062*
026070*================================================================*
026080 特別区分セット SECTION.
026090*
026100     EVALUATE 受－特別区分
026110     WHEN 1
026120          MOVE NC"○"  TO ７０歳９チェックＷ
026130          IF ( 保険種別ＷＲ = 01 OR 08 )
026140             CONTINUE
026150          ELSE
026160             PERFORM 本人家族セット
026170          END-IF
026171     WHEN 2
026180     WHEN 3
026190          MOVE NC"○"  TO ７０歳８チェックＷ
026200          IF ( 保険種別ＷＲ = 01 OR 08 )
026210             CONTINUE
026220          ELSE
026230             PERFORM 本人家族セット
026240          END-IF
026250     WHEN 6
026260          MOVE NC"○"  TO ３歳未満チェックＷ
026270     WHEN OTHER
026280          PERFORM 本人家族セット
026290     END-EVALUATE.
026300*
026310*================================================================*
026320 本人家族セット SECTION.
026330*
026340     IF ( 本人家族区分ＷＲ = 1 )
026350        MOVE NC"○"   TO 続柄本人チェックＷ
026360     ELSE
026370        MOVE NC"○"   TO 続柄家族チェックＷ
026380     END-IF.
026390*
026400*================================================================*
026410 負傷データ取得 SECTION.
026420*================================================================*
026430**************************************************
026440* 連結データから負傷データＦより以下の情報を取得 *
026450* ● 負傷名...部位＋負傷種別にて加工して格納     *
026460* ● 負傷年.......負傷年Ｗ                       *
026470* ● 負傷月.......負傷月Ｗ                       *
026480* ● 負傷日.......負傷日Ｗ                       *
026490* ● 開始年.......初検年Ｗ                       *
026500* ● 開始月.......初検月Ｗ                       *
026510* ● 開始日.......初検日Ｗ                       *
026520* ● 終了年.......終了年Ｗ                       *
026530* ● 終了月.......終了月Ｗ                       *
026540* ● 終了日.......終了日Ｗ                       *
026550* ● 実日数.......実日数Ｗ                       *
026560* ● 転帰区分 ....区分によりチェックに"○"を格納 *
026570* ● 金属副子 ....区分によりチェックに"○"を格納 *
026580* ● 経過コード...経過マスタより取得             *
026590**************************************************
026600     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
026610     MOVE 施術年ＷＲ         TO 負－施術年.
026620     MOVE 施術月ＷＲ         TO 負－施術月.
026630     MOVE 患者コードＷＲ     TO 負－患者コード.
026640     READ 負傷データＦ
026650     INVALID KEY
026660         CONTINUE
026670*            /* ありえない */
026680     NOT INVALID KEY
026690         MOVE 負－部位数                   TO 部位数Ｗ
026700         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
026710                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
026720             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別Ｗ(部位ＣＮＴ)
026730             MOVE 負－部位(部位ＣＮＴ)     TO 部位Ｗ(部位ＣＮＴ)
026740             MOVE 負－左右区分(部位ＣＮＴ) TO 左右区分Ｗ(部位ＣＮＴ)
026750             MOVE 負－負傷位置番号(部位ＣＮＴ)
026760                                           TO 負傷位置番号Ｗ(部位ＣＮＴ)
026770********************************************************
026780* 注）全柔...部位名1+負傷種別＋部位名2にて加工して格納 *
026790********************************************************
026800* 負傷種別
026810             MOVE SPACE                     TO 負傷名称Ｗ
026820             MOVE 03                        TO 名－区分コード
026830             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
026840             READ 名称マスタ
026850             INVALID KEY
026860                 MOVE SPACE        TO 負傷名称Ｗ
026870             NOT INVALID KEY
026880                 MOVE 名－正式名称 TO 負傷名称Ｗ
026890             END-READ
026900* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
032680*
032690             PERFORM 部位名称埋込処理
027090*
027100             MOVE 負－負傷年(部位ＣＮＴ)    TO 負傷年Ｗ(部位ＣＮＴ)
027110             MOVE 負－負傷月(部位ＣＮＴ)    TO 負傷月Ｗ(部位ＣＮＴ)
027120             MOVE 負－負傷日(部位ＣＮＴ)    TO 負傷日Ｗ(部位ＣＮＴ)
027130             MOVE 負－開始年(部位ＣＮＴ)    TO 初検年Ｗ(部位ＣＮＴ)
027140             MOVE 負－開始月(部位ＣＮＴ)    TO 初検月Ｗ(部位ＣＮＴ)
027150             MOVE 負－開始日(部位ＣＮＴ)    TO 初検日Ｗ(部位ＣＮＴ)
027160             IF ( 負－転帰区分(部位ＣＮＴ) = 9 )
032900                MOVE 9                      TO 終了和暦Ｗ(部位ＣＮＴ)
027170                MOVE 99                     TO 終了年Ｗ(部位ＣＮＴ)
027180                MOVE 99                     TO 終了月Ｗ(部位ＣＮＴ)
027190                MOVE 99                     TO 終了日Ｗ(部位ＣＮＴ)
027200             ELSE
032940                MOVE 負－終了和暦(部位ＣＮＴ) TO 終了和暦Ｗ(部位ＣＮＴ)
027210                MOVE 負－終了年(部位ＣＮＴ) TO 終了年Ｗ(部位ＣＮＴ)
027220                MOVE 負－終了月(部位ＣＮＴ) TO 終了月Ｗ(部位ＣＮＴ)
027230                MOVE 負－終了日(部位ＣＮＴ) TO 終了日Ｗ(部位ＣＮＴ)
027240             END-IF
027250* 経過略称取得
027550*
      */経過欄の転帰に部位番号を追加
                   EVALUATE 部位ＣＮＴ
                   WHEN 1
                       MOVE NC"①" TO 経過部位番号Ｗ
                   WHEN 2
                       MOVE NC"②" TO 経過部位番号Ｗ
                   WHEN 3
                       MOVE NC"③" TO 経過部位番号Ｗ
                   WHEN 4
                       MOVE NC"④" TO 経過部位番号Ｗ
                   WHEN 5
                       MOVE NC"⑤" TO 経過部位番号Ｗ
                   END-EVALUATE
027560             MOVE 負－転帰区分(部位ＣＮＴ) TO 転帰区分Ｗ(部位ＣＮＴ)
027570             EVALUATE 負－転帰区分(部位ＣＮＴ)
027580             WHEN 1
027590             WHEN 2
027600                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
027610*                 MOVE NC"治癒"             TO 印刷経過略称Ｗ(部位ＣＮＴ)
                       MOVE SPACE TO 印刷経過略称Ｗ(部位ＣＮＴ)
                       STRING 経過部位番号Ｗ DELIMITED BY SIZE
                              NC"治癒"       DELIMITED BY SIZE
                         INTO 印刷経過略称Ｗ(部位ＣＮＴ)
                       END-STRING
027620             WHEN 3
027630                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
027640                 MOVE NC"中止"             TO 印刷経過略称Ｗ(部位ＣＮＴ)
                       MOVE SPACE TO 印刷経過略称Ｗ(部位ＣＮＴ)
                       STRING 経過部位番号Ｗ DELIMITED BY SIZE
                              NC"中止"       DELIMITED BY SIZE
                         INTO 印刷経過略称Ｗ(部位ＣＮＴ)
                       END-STRING
027650             WHEN 4
027660                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
027670                 MOVE NC"転医"             TO 印刷経過略称Ｗ(部位ＣＮＴ)
                       MOVE SPACE TO 印刷経過略称Ｗ(部位ＣＮＴ)
                       STRING 経過部位番号Ｗ DELIMITED BY SIZE
                              NC"転医"       DELIMITED BY SIZE
                         INTO 印刷経過略称Ｗ(部位ＣＮＴ)
                       END-STRING
027680             WHEN 9
027690*                 MOVE NC"継続"             TO 印刷経過略称Ｗ(部位ＣＮＴ)
                       MOVE SPACE TO 印刷経過略称Ｗ(部位ＣＮＴ)
                       STRING 経過部位番号Ｗ DELIMITED BY SIZE
                              NC"継続"       DELIMITED BY SIZE
                         INTO 印刷経過略称Ｗ(部位ＣＮＴ)
                       END-STRING
027700             END-EVALUATE
027710*
031230             MOVE レセ－部位実日数(部位ＣＮＴ) TO 実日数Ｗ(部位ＣＮＴ)
      */長期頻回20241007/
                   MOVE レセ－部位継続月数(部位ＣＮＴ) TO 部位継続月数Ｗ(部位ＣＮＴ)
027720         END-PERFORM
027730* 新規/継続 チェック
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
027790* 枝番判定用
027800         MOVE 負－開始診療日手動区分 TO  開始診療日手動区分Ｗ
027810*
027820* 負傷原因印刷区分
027830         MOVE 負－レセ負傷原因印刷区分 TO レセ負傷原因印刷区分Ｗ
027880         MOVE 負－レセ長期理由印刷区分 TO レセ長期理由印刷区分Ｗ
027840*
027850     END-READ.
027860*
032350*================================================================*
032360 部位名称埋込処理 SECTION.
032370*
006490     STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
009980            負傷名称Ｗ                    DELIMITED BY SPACE
006500            レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
006520       INTO 負傷名Ｗ(部位ＣＮＴ)
006570     END-STRING.
032570*
027870*================================================================*
027880 料金情報取得 SECTION.
027890*================================================================*
027900********************
027910* 料金データセット *
027920********************
027930*    ****************************************************************
027940*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
027950*    ****************************************************************
027960     MOVE レセ－初検料                TO 初検料ＷＲ.
027970     IF ( レセ－時間外 = 1 )
027980        MOVE NC"○"                   TO 時間外チェックＷ
027990     END-IF.
028000     IF ( レセ－休日 = 1 )
028010        MOVE NC"○"                   TO 休日チェックＷ
028020     END-IF.
028030     IF ( レセ－深夜 = 1 )
028040        MOVE NC"○"                   TO 深夜チェックＷ
028050     END-IF.
           MOVE レセ－初検時相談料           TO 相談料ＷＲ.
028060*
028070     MOVE レセ－初検加算料            TO  初検加算料ＷＲ.
028080     MOVE レセ－再検料                TO  再検料ＷＲ.
028090     MOVE レセ－往療距離              TO  往療距離ＷＲ.
028100     MOVE レセ－往療回数              TO  往療回数ＷＲ.
028110     MOVE レセ－往療料                TO  往療料ＷＲ.
028120     MOVE レセ－往療加算料            TO  往療加算料ＷＲ.
028130*
028140     IF ( レセ－夜間 = 1 )
028150        MOVE NC"○"                   TO 夜間チェックＷ
028160     END-IF.
028170     IF ( レセ－暴風雨雪 = 1 )
028180        MOVE NC"○"                   TO 暴風雨雪チェックＷ
028190     END-IF.
028200     IF ( レセ－難路 = 1 )
028210        MOVE NC"○"                   TO 難路チェックＷ
028220     END-IF.
028230*
028240     MOVE レセ－金属副子加算料        TO  金属副子加算料ＷＲ.
028250*
      */金属副子・運動後療の変更・追加/1805
           MOVE レセ－金属副子回数            TO 金属回数Ｗ.
           MOVE レセ－運動後療回数            TO 運動回数Ｗ.
           MOVE レセ－運動後療料              TO 運動料Ｗ.
028350*
028360     MOVE レセ－施術情報提供料        TO  施術情報提供料ＷＲ.
      */2022
           MOVE レセ－明細書発行加算料         TO 明細書発行加算料ＷＲ.
           MOVE レセ－明細書発行加算日         TO 明細書発行加算日ＷＲ.
028370* 小計
022420     COMPUTE 小計Ｗ = レセ－小計 + レセ－運動後療料 + レセ－明細書発行加算料.
028390********************
028400* 初回処置料セット *
028410********************
028420     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
028430             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
028440         MOVE レセ－初回処置料(部位ＣＮＴ) TO 初回処置料ＷＲ(部位ＣＮＴ)
028450         IF ( レセ－初回処置料(部位ＣＮＴ) NOT = ZERO )
028460            EVALUATE 負－負傷種別(部位ＣＮＴ)
028470* 捻挫・打撲・挫傷
028480            WHEN 1
028490            WHEN 2
028500            WHEN 3
028510                MOVE NC"○"       TO 施療料チェックＷ
028520* 脱臼・骨折・骨折拘縮
028530            WHEN 4
028540            WHEN 5
028550            WHEN 7
028560                MOVE NC"○"       TO 整復料チェックＷ
028570* 不全骨折・不全骨折拘縮
028580            WHEN 6
028590            WHEN 8
028600                MOVE NC"○"       TO 固定料チェックＷ
028610            END-EVALUATE
028620         END-IF
028630     END-PERFORM.
028640     MOVE レセ－初回処置料合計    TO 初回処置料合計Ｗ.
028650********************
028660* 逓減毎料金セット *
028670********************
028680*    **********
028690*    * １部位 *
028700*    **********
028710     MOVE レセ－後療単価１             TO 後療単価１ＷＲ.
028720     MOVE レセ－後療回数１             TO 後療回数１ＷＲ.
028730     MOVE レセ－後療料１               TO 後療料１ＷＲ.
028740     MOVE レセ－冷罨法回数１           TO 冷罨法回数１ＷＲ.
028750     MOVE レセ－冷罨法料１             TO 冷罨法料１ＷＲ.
028760     MOVE レセ－温罨法回数１           TO 温罨法回数１ＷＲ.
028770     MOVE レセ－温罨法料１             TO 温罨法料１ＷＲ.
028780     MOVE レセ－電療回数１             TO 電療回数１ＷＲ.
028790     MOVE レセ－電療料１               TO 電療料１ＷＲ.
028800     MOVE レセ－小計１                 TO 小計１ＷＲ.
           IF レセ－長期頻回逓減率１ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率１   TO 長期逓減率１ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率１       TO 長期逓減率１ＷＲ
           END-IF.
028820     MOVE レセ－長期込小計１           TO 長期込小計１ＷＲ.
028830*    **********
028840*    * ２部位 *
028850*    **********
028860     MOVE レセ－後療単価２             TO 後療単価２ＷＲ.
028870     MOVE レセ－後療回数２             TO 後療回数２ＷＲ.
028880     MOVE レセ－後療料２               TO 後療料２ＷＲ.
028890     MOVE レセ－冷罨法回数２           TO 冷罨法回数２ＷＲ.
028900     MOVE レセ－冷罨法料２             TO 冷罨法料２ＷＲ.
028910     MOVE レセ－温罨法回数２           TO 温罨法回数２ＷＲ.
028920     MOVE レセ－温罨法料２             TO 温罨法料２ＷＲ.
028930     MOVE レセ－電療回数２             TO 電療回数２ＷＲ.
028940     MOVE レセ－電療料２               TO 電療料２ＷＲ.
028950     MOVE レセ－小計２                 TO 小計２ＷＲ.
           IF レセ－長期頻回逓減率２ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率２   TO 長期逓減率２ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率２       TO 長期逓減率２ＷＲ
           END-IF.
028970     MOVE レセ－長期込小計２           TO 長期込小計２ＷＲ.
028980*    ****************
028990*    * ３部位／８割 *
029000*    ****************
029010     MOVE レセ－後療単価３８             TO 後療単価３８ＷＲ.
029020     MOVE レセ－後療回数３８             TO 後療回数３８ＷＲ.
029030     MOVE レセ－後療料３８               TO 後療料３８ＷＲ.
029040     MOVE レセ－冷罨法回数３８           TO 冷罨法回数３８ＷＲ.
029050     MOVE レセ－冷罨法料３８             TO 冷罨法料３８ＷＲ.
029060     MOVE レセ－温罨法回数３８           TO 温罨法回数３８ＷＲ.
029070     MOVE レセ－温罨法料３８             TO 温罨法料３８ＷＲ.
029080     MOVE レセ－電療回数３８             TO 電療回数３８ＷＲ.
029090     MOVE レセ－電療料３８               TO 電療料３８ＷＲ.
029100     MOVE レセ－小計３８                 TO 小計３８ＷＲ.
029110     MOVE レセ－多部位込小計３８         TO 多部位込小計３８ＷＲ.
           IF レセ－長期頻回逓減率３８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率３８   TO 長期逓減率３８ＷＲ
           ELSE
024160         MOVE レセ－長期逓減率３８       TO 長期逓減率３８ＷＲ
           END-IF.
029130     MOVE レセ－長期込小計３８           TO 長期込小計３８ＷＲ.
029140*    ****************
029150*    * ３部位／10割 *
029160*    ****************
029170     MOVE レセ－逓減開始月３０           TO 逓減開始月３０ＷＲ.
029180     MOVE レセ－逓減開始日３０           TO 逓減開始日３０ＷＲ.
029190     MOVE レセ－後療単価３０             TO 後療単価３０ＷＲ.
029200     MOVE レセ－後療回数３０             TO 後療回数３０ＷＲ.
029210     MOVE レセ－後療料３０               TO 後療料３０ＷＲ.
029220     MOVE レセ－冷罨法回数３０           TO 冷罨法回数３０ＷＲ.
029230     MOVE レセ－冷罨法料３０             TO 冷罨法料３０ＷＲ.
029240     MOVE レセ－温罨法回数３０           TO 温罨法回数３０ＷＲ.
029250     MOVE レセ－温罨法料３０             TO 温罨法料３０ＷＲ.
029260     MOVE レセ－電療回数３０             TO 電療回数３０ＷＲ.
029270     MOVE レセ－電療料３０               TO 電療料３０ＷＲ.
029280     MOVE レセ－小計３０                 TO 小計３０ＷＲ.
           IF レセ－長期頻回逓減率３０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率３０   TO 長期逓減率３０ＷＲ
           ELSE
024330         MOVE レセ－長期逓減率３０       TO 長期逓減率３０ＷＲ
           END-IF.
029300     MOVE レセ－長期込小計３０           TO 長期込小計３０ＷＲ.
029310*    ****************
029320*    * ４部位／５割 *
029330*    ****************
029340*     MOVE レセ－後療単価４５             TO 後療単価４５ＷＲ.
029350*     MOVE レセ－後療回数４５             TO 後療回数４５ＷＲ.
029360*     MOVE レセ－後療料４５               TO 後療料４５ＷＲ.
029370*     MOVE レセ－冷罨法回数４５           TO 冷罨法回数４５ＷＲ.
029380*     MOVE レセ－冷罨法料４５             TO 冷罨法料４５ＷＲ.
029390*     MOVE レセ－温罨法回数４５           TO 温罨法回数４５ＷＲ.
029400*     MOVE レセ－温罨法料４５             TO 温罨法料４５ＷＲ.
029410*     MOVE レセ－電療回数４５             TO 電療回数４５ＷＲ.
029420*     MOVE レセ－電療料４５               TO 電療料４５ＷＲ.
029430*     MOVE レセ－小計４５                 TO 小計４５ＷＲ.
029440*     MOVE レセ－多部位込小計４５         TO 多部位込小計４５ＷＲ.
029450*     MOVE レセ－長期逓減率４５           TO 長期逓減率４５ＷＲ.
029460*     MOVE レセ－長期込小計４５           TO 長期込小計４５ＷＲ.
029470*    ****************
029480*    * ４部位／８割 *
029490*    ****************
029500     MOVE レセ－逓減開始月４８           TO 逓減開始月４８ＷＲ.
029510     MOVE レセ－逓減開始日４８           TO 逓減開始日４８ＷＲ.
029520     MOVE レセ－後療単価４８             TO 後療単価４８ＷＲ.
029530     MOVE レセ－後療回数４８             TO 後療回数４８ＷＲ.
029540     MOVE レセ－後療料４８               TO 後療料４８ＷＲ.
029550     MOVE レセ－冷罨法回数４８           TO 冷罨法回数４８ＷＲ.
029560     MOVE レセ－冷罨法料４８             TO 冷罨法料４８ＷＲ.
029570     MOVE レセ－温罨法回数４８           TO 温罨法回数４８ＷＲ.
029580     MOVE レセ－温罨法料４８             TO 温罨法料４８ＷＲ.
029590     MOVE レセ－電療回数４８             TO 電療回数４８ＷＲ.
029600     MOVE レセ－電療料４８               TO 電療料４８ＷＲ.
029610     MOVE レセ－小計４８                 TO 小計４８ＷＲ.
029620     MOVE レセ－多部位込小計４８         TO 多部位込小計４８ＷＲ.
           IF レセ－長期頻回逓減率４８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率４８   TO 長期逓減率４８ＷＲ
           ELSE
024670         MOVE レセ－長期逓減率４８       TO 長期逓減率４８ＷＲ
           END-IF.
029640     MOVE レセ－長期込小計４８           TO 長期込小計４８ＷＲ.
029650*    ****************
029660*    * ４部位／10割 *
029670*    ****************
029680     MOVE レセ－逓減開始月４０           TO 逓減開始月４０ＷＲ.
029690     MOVE レセ－逓減開始日４０           TO 逓減開始日４０ＷＲ.
029700     MOVE レセ－後療単価４０             TO 後療単価４０ＷＲ.
029710     MOVE レセ－後療回数４０             TO 後療回数４０ＷＲ.
029720     MOVE レセ－後療料４０               TO 後療料４０ＷＲ.
029730     MOVE レセ－冷罨法回数４０           TO 冷罨法回数４０ＷＲ.
029740     MOVE レセ－冷罨法料４０             TO 冷罨法料４０ＷＲ.
029750     MOVE レセ－温罨法回数４０           TO 温罨法回数４０ＷＲ.
029760     MOVE レセ－温罨法料４０             TO 温罨法料４０ＷＲ.
029770     MOVE レセ－電療回数４０             TO 電療回数４０ＷＲ.
029780     MOVE レセ－電療料４０               TO 電療料４０ＷＲ.
029790     MOVE レセ－小計４０                 TO 小計４０ＷＲ.
           IF レセ－長期頻回逓減率４０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率４０   TO 長期逓減率４０ＷＲ
           ELSE
024840         MOVE レセ－長期逓減率４０       TO 長期逓減率４０ＷＲ
           END-IF.
029810     MOVE レセ－長期込小計４０           TO 長期込小計４０ＷＲ.
029820**    *****************
029830**    * ５部位／2.5割 *
029840**    *****************
029850*     MOVE レセ－後療単価５２             TO 後療単価５２ＷＲ.
029860*     MOVE レセ－後療回数５２             TO 後療回数５２ＷＲ.
029870*     MOVE レセ－後療料５２               TO 後療料５２ＷＲ.
029880*     MOVE レセ－冷罨法回数５２           TO 冷罨法回数５２ＷＲ.
029890*     MOVE レセ－冷罨法料５２             TO 冷罨法料５２ＷＲ.
029900*     MOVE レセ－温罨法回数５２           TO 温罨法回数５２ＷＲ.
029910*     MOVE レセ－温罨法料５２             TO 温罨法料５２ＷＲ.
029920*     MOVE レセ－電療回数５２             TO 電療回数５２ＷＲ.
029930*     MOVE レセ－電療料５２               TO 電療料５２ＷＲ.
029940*     MOVE レセ－小計５２                 TO 小計５２ＷＲ.
029950*     MOVE レセ－多部位込小計５２         TO 多部位込小計５２ＷＲ.
029960*     MOVE レセ－長期逓減率５２           TO 長期逓減率５２ＷＲ.
029970*     MOVE レセ－長期込小計５２           TO 長期込小計５２ＷＲ.
029980**    ****************
029990**    * ５部位／５割 *
030000**    ****************
030010*     MOVE レセ－逓減開始月５５           TO 逓減開始月５５ＷＲ.
030020*     MOVE レセ－逓減開始日５５           TO 逓減開始日５５ＷＲ.
030030*     MOVE レセ－後療単価５５             TO 後療単価５５ＷＲ.
030040*     MOVE レセ－後療回数５５             TO 後療回数５５ＷＲ.
030050*     MOVE レセ－後療料５５               TO 後療料５５ＷＲ.
030060*     MOVE レセ－冷罨法回数５５           TO 冷罨法回数５５ＷＲ.
030070*     MOVE レセ－冷罨法料５５             TO 冷罨法料５５ＷＲ.
030080*     MOVE レセ－温罨法回数５５           TO 温罨法回数５５ＷＲ.
030090*     MOVE レセ－温罨法料５５             TO 温罨法料５５ＷＲ.
030100*     MOVE レセ－電療回数５５             TO 電療回数５５ＷＲ.
030110*     MOVE レセ－電療料５５               TO 電療料５５ＷＲ.
030120*     MOVE レセ－小計５５                 TO 小計５５ＷＲ.
030130*     MOVE レセ－多部位込小計５５         TO 多部位込小計５５ＷＲ.
030140*     MOVE レセ－長期逓減率５５           TO 長期逓減率５５ＷＲ.
030150*     MOVE レセ－長期込小計５５           TO 長期込小計５５ＷＲ.
030160*    ****************
030170*    * ５部位／８割 *
030180*    ****************
030190     MOVE レセ－逓減開始月５８           TO 逓減開始月５８ＷＲ.
030200     MOVE レセ－逓減開始日５８           TO 逓減開始日５８ＷＲ.
030210     MOVE レセ－後療単価５８             TO 後療単価５８ＷＲ.
030220     MOVE レセ－後療回数５８             TO 後療回数５８ＷＲ.
030230     MOVE レセ－後療料５８               TO 後療料５８ＷＲ.
030240     MOVE レセ－冷罨法回数５８           TO 冷罨法回数５８ＷＲ.
030250     MOVE レセ－冷罨法料５８             TO 冷罨法料５８ＷＲ.
030260     MOVE レセ－温罨法回数５８           TO 温罨法回数５８ＷＲ.
030270     MOVE レセ－温罨法料５８             TO 温罨法料５８ＷＲ.
030280     MOVE レセ－電療回数５８             TO 電療回数５８ＷＲ.
030290     MOVE レセ－電療料５８               TO 電療料５８ＷＲ.
030300     MOVE レセ－小計５８                 TO 小計５８ＷＲ.
030310     MOVE レセ－多部位込小計５８         TO 多部位込小計５８ＷＲ.
           IF レセ－長期頻回逓減率５８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率５８   TO 長期逓減率５８ＷＲ
           ELSE
025360         MOVE レセ－長期逓減率５８       TO 長期逓減率５８ＷＲ
           END-IF.
030330     MOVE レセ－長期込小計５８           TO 長期込小計５８ＷＲ.
030340*    ****************
030350*    * ５部位／10割 *
030360*    ****************
030370     MOVE レセ－逓減開始月５０           TO 逓減開始月５０ＷＲ.
030380     MOVE レセ－逓減開始日５０           TO 逓減開始日５０ＷＲ.
030390     MOVE レセ－後療単価５０             TO 後療単価５０ＷＲ.
030400     MOVE レセ－後療回数５０             TO 後療回数５０ＷＲ.
030410     MOVE レセ－後療料５０               TO 後療料５０ＷＲ.
030420     MOVE レセ－冷罨法回数５０           TO 冷罨法回数５０ＷＲ.
030430     MOVE レセ－冷罨法料５０             TO 冷罨法料５０ＷＲ.
030440     MOVE レセ－温罨法回数５０           TO 温罨法回数５０ＷＲ.
030450     MOVE レセ－温罨法料５０             TO 温罨法料５０ＷＲ.
030460     MOVE レセ－電療回数５０             TO 電療回数５０ＷＲ.
030470     MOVE レセ－電療料５０               TO 電療料５０ＷＲ.
030480     MOVE レセ－小計５０                 TO 小計５０ＷＲ.
           IF レセ－長期頻回逓減率５０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率５０   TO 長期逓減率５０ＷＲ
           ELSE
025530         MOVE レセ－長期逓減率５０       TO 長期逓減率５０ＷＲ
           END-IF.
030500     MOVE レセ－長期込小計５０           TO 長期込小計５０ＷＲ.
030510*
030520*================================================================*
030530 施術記録取得 SECTION.
030540*================================================================*
030550************************************************************
030560* 作１データから負傷データＦより以下の情報を取得           *
030570* ● 初検加算 .....区分によりチェックに"○"を格納...複数可 *
030580* ● 往療加算 .....区分によりチェックに"○"を格納...複数可 *
030590************************************************************
030600     MOVE  SPACE  TO  初日再検フラグ.
030610     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 部位数Ｗ
030620         IF ( 施術年Ｗ = 初検年Ｗ(部位ＣＮＴ) ) AND
030630            ( 施術月Ｗ = 初検月Ｗ(部位ＣＮＴ) )
030640             MOVE 患者番号ＷＲ          TO 施記－患者番号
030650             MOVE 枝番ＷＲ              TO 施記－枝番
030660             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
030670             MOVE 初検年Ｗ(部位ＣＮＴ)  TO 開始年Ｗ(部位ＣＮＴ) 施記－施術年
030680             MOVE 初検月Ｗ(部位ＣＮＴ)  TO 開始月Ｗ(部位ＣＮＴ) 施記－施術月
030690             MOVE 初検日Ｗ(部位ＣＮＴ)  TO 開始日Ｗ(部位ＣＮＴ) 施記－施術日
030700         ELSE
030710             MOVE 患者番号ＷＲ          TO 施記－患者番号
030720             MOVE 枝番ＷＲ              TO 施記－枝番
030730             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
030740             MOVE 施術年ＷＲ            TO 施記－施術年
030750             MOVE 施術月ＷＲ            TO 施記－施術月
030760             MOVE ZERO                  TO 施記－施術日
030770         END-IF
030780         START 施術記録Ｆ   KEY IS >= 施記－患者コード
030790                                      施記－施術和暦年月日
030800         END-START
030810         IF ( 状態キー = "00" )
030830             MOVE ZERO  TO 終了和暦ＷＴ
030830             MOVE ZERO  TO 終了年ＷＴ
030840             MOVE ZERO  TO 終了月ＷＴ
030850             MOVE ZERO  TO 終了日ＷＴ
030860             MOVE SPACE TO 終了フラグ２
030870             PERFORM 施術記録Ｆ読込
030880             IF  ( 終了フラグ２      = SPACE   ) AND
030890                 ( 施記－患者コード  = 患者コードＷＲ ) AND
030900                 ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
030910                 ( 施記－施術年      = 施術年ＷＲ     ) AND
030920                 ( 施記－施術月      = 施術月ＷＲ     ) 
030930*
030940*        *****************************************************************
030950*        * 開始年月日 ( その部位が当月初検でないか、
030960*                       当月初検でも枝番がある時は、最初の施術日を開始日)*
030970*        *****************************************************************
030980                 IF ( 施術年Ｗ NOT = 初検年Ｗ(部位ＣＮＴ) ) OR
030990                    ( 施術月Ｗ NOT = 初検月Ｗ(部位ＣＮＴ) ) OR
031000                    ( 開始診療日手動区分Ｗ = 1 )
031010                     MOVE 施記－施術年   TO 開始年Ｗ(部位ＣＮＴ)
031020                     MOVE 施記－施術月   TO 開始月Ｗ(部位ＣＮＴ)
031030                     MOVE 施記－施術日   TO 開始日Ｗ(部位ＣＮＴ)
031040                 END-IF
031050             END-IF
031060             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
031070                           ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
031080                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
031090                           ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
031100                           ( 施記－施術月     NOT = 施術月ＷＲ       ) OR
031110                           ( 施記－施術日         > 終了日Ｗ(部位ＣＮＴ))
031120*               **********
031130*               * 実日数 *
031140*               **********
031150*                /　後療なしの時以外 /
031160                      IF ( 施記－診療区分  = 1 ) AND
031170                         ( 施記－整復施療区分(部位ＣＮＴ)   = ZERO ) AND
031180                         ( 施記－後療料請求区分(部位ＣＮＴ) = ZERO ) AND
031190                         ( 施記－罨法区分(部位ＣＮＴ)       = ZERO ) AND
031200                         ( 施記－電療区分(部位ＣＮＴ)       = ZERO )
031210                          CONTINUE
031220                      ELSE
031240                          MOVE 施記－施術和暦             TO 終了和暦ＷＴ
031240                          MOVE 施記－施術年               TO 終了年ＷＴ
031250                          MOVE 施記－施術月               TO 終了月ＷＴ
031260                          MOVE 施記－施術日               TO 終了日ＷＴ
031270                      END-IF
031280*
031290                      PERFORM 施術記録Ｆ読込
031300            END-PERFORM
031310        END-IF
031320*       **************************
031330*       * 継続：終了年月日セット *
031340*       **************************
031350        IF ( 転帰区分Ｗ(部位ＣＮＴ) = 9 )
032090            MOVE 終了和暦ＷＴ  TO 終了和暦Ｗ(部位ＣＮＴ)
031360            MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
031370            MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
031380            MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
031390        END-IF
031400        IF ( 終了年月日Ｗ(部位ＣＮＴ) > 受理年月日Ｗ )
032140            MOVE 終了和暦Ｗ(部位ＣＮＴ) TO 受理和暦Ｗ
031410            MOVE 終了年Ｗ(部位ＣＮＴ) TO 受理年Ｗ
031420            MOVE 終了月Ｗ(部位ＣＮＴ) TO 受理月Ｗ
031430            MOVE 終了日Ｗ(部位ＣＮＴ) TO 受理日Ｗ
031440        END-IF
031450     END-PERFORM.
031460*
031470** ----- 前月初検のみかを判定 -----------*
031480*
031490*     MOVE 患者番号ＷＲ          TO 施記－患者番号.
031500*     MOVE 枝番ＷＲ              TO 施記－枝番.
031510*     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
031520*     MOVE 施術年ＷＲ            TO 施記－施術年.
031530*     MOVE 施術月ＷＲ            TO 施記－施術月.
031540*     MOVE ZERO                  TO 施記－施術日.
031550*     START 施術記録Ｆ   KEY IS >= 施記－患者コード
031560*                                  施記－施術和暦年月日
031570*     END-START.
031580*     IF ( 状態キー = "00" )
031590*             MOVE SPACE TO 終了フラグ２
031600*             PERFORM 施術記録Ｆ読込
031610*             IF  ( 終了フラグ２      = SPACE   ) AND
031620*                 ( 施記－患者コード  = 患者コードＷＲ ) AND
031630*                 ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
031640*                 ( 施記－施術年      = 施術年ＷＲ     ) AND
031650*                 ( 施記－施術月      = 施術月ＷＲ     ) 
031660** 当月施術開始日が再検かどうか判定
031670*                 IF ( 施記－再検料請求 = 1 )
031680*                    MOVE "YES"  TO  初日再検フラグ
031690*                 END-IF
031700**
031710*             END-IF
031720*     END-IF.
031730*/前月初険のみはいらない。コメントに/060707
031740*     IF ( 初日再検フラグ = "YES" )
031750*        PERFORM 前月初検のみ判定
031760*     END-IF.
031770*
031780*================================================================*
031790 前月初検のみ判定 SECTION.
031800*
031810*** 前月の通院日が初検か判定 
031820     MOVE  SPACE            TO 前月フラグ.
031830     MOVE 受－患者コード    TO 施記－患者コード.
031840     MOVE 受－施術和暦      TO 施記－施術和暦.
031850     MOVE 受－施術年        TO 施記－施術年.
031860     MOVE 受－施術月        TO 施記－施術月.
031870     MOVE 1                 TO 施記－施術日.
031880     START 施術記録Ｆ   KEY IS <  施記－患者コード
031890                                  施記－施術和暦年月日
031900                                  REVERSED
031910     END-START.
031920     IF ( 状態キー = "00" )
031930         MOVE SPACE  TO 終了フラグ２
031940         PERFORM 施術記録Ｆ読込
031950         IF ( 終了フラグ２      = SPACE  ) AND
031960            ( 施記－患者コード  = 受－患者コード ) AND
031970            ( 施記－診療区分    = 2 ) 
031980*
031990            PERFORM 前月判定
032000**** 適用１を使用
032010            IF ( 前月フラグ = "YES" )
032020               MOVE NC"※前月初検のみ"    TO  適用１Ｗ
032030            END-IF
032040**
032050         END-IF
032060     END-IF.
032070*
032080*================================================================*
032090 前月判定  SECTION.
032100* 
032110*** 読み込んだ施術記録の年月が、前月かどうか判定 (年月の差が 1 か?)
032120      MOVE  SPACE  TO  前月フラグ.
032130      INITIALIZE  計算年月日Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
032140**
032150      MOVE 受－施術和暦    TO 終了和暦２Ｗ.
032160      MOVE 受－施術年      TO 終了年２Ｗ.
032170      MOVE 受－施術月      TO 終了月２Ｗ.
032180      MOVE 施記－施術和暦  TO 開始和暦２Ｗ.
032190      MOVE 施記－施術年    TO 開始年２Ｗ.
032200      MOVE 施記－施術月    TO 開始月２Ｗ.
032210*
032220      EVALUATE TRUE
032230       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ = 終了年２Ｗ)
032240            PERFORM  前月比較月
032250       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ NOT = 終了年２Ｗ)
032260            PERFORM  前月比較年
032270       WHEN  開始和暦２Ｗ NOT = 終了和暦２Ｗ 
032280            PERFORM  前月比較元号
032290      END-EVALUATE.
032300*
032310      IF ( 計算月Ｗ = 1 )
032320         MOVE  "YES"  TO  前月フラグ
032330      END-IF.
032340*
032350*================================================================*
032360 前月比較月  SECTION.
032370*
032380     IF ( 終了月２Ｗ >  開始月２Ｗ )
032390         COMPUTE 計算月Ｗ = 終了月２Ｗ - 開始月２Ｗ
032400     ELSE
032410        MOVE ZERO TO 計算月Ｗ
032420     END-IF.
032430*
032440*================================================================*
032450 前月比較年  SECTION.
032460*
032470     IF ( 終了年２Ｗ >  開始年２Ｗ )
032480         COMPUTE 計算年Ｗ = 終了年２Ｗ - 開始年２Ｗ
032490         COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
032500     ELSE
032510        MOVE ZERO TO 計算月Ｗ
032520     END-IF.
032530*
032540*================================================================*
032550 前月比較元号  SECTION.
032560*
032570     MOVE 開始和暦２Ｗ TO 元－元号区分.
032580     READ 元号マスタ
032590     NOT INVALID KEY
032600         MOVE 元－開始西暦年 TO 開始西暦年Ｗ
032610     END-READ.
032620     MOVE 終了和暦２Ｗ TO 元－元号区分.
032630     READ 元号マスタ
032640     NOT INVALID KEY
032650         MOVE 元－開始西暦年 TO 終了西暦年Ｗ
032660     END-READ.
032670**
032680     IF (開始西暦年Ｗ NOT = ZERO) AND (終了西暦年Ｗ NOT = ZERO)
032690        COMPUTE 開始西暦年Ｗ = 開始西暦年Ｗ + 開始年２Ｗ - 1
032700        COMPUTE 終了西暦年Ｗ = 終了西暦年Ｗ + 終了年２Ｗ - 1
032710*
032720        IF ( 終了西暦年Ｗ =  開始西暦年Ｗ )
032730           PERFORM  前月比較月
032740        ELSE
032750           IF ( 終了西暦年Ｗ >  開始西暦年Ｗ )
032760               COMPUTE 計算年Ｗ = 終了西暦年Ｗ - 開始西暦年Ｗ
032770               COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
032780           ELSE
032790               MOVE ZERO TO 計算月Ｗ
032800           END-IF
032810        END-IF
032820     ELSE
032830        MOVE ZERO TO 計算月Ｗ
032840     END-IF.
032850*
032860*================================================================*
032870 長期判定取得 SECTION.
032880*================================================================*
032890* ３カ月以上の長期判定は "CHOUKI" を呼ぶ. 
032900     MOVE  SPACE TO  連期間－キー.
032910     INITIALIZE      連期間－キー.
032920     MOVE 施術和暦ＷＲ  TO  連期間－施術和暦.
032930     MOVE 施術年ＷＲ    TO  連期間－施術年.
032940     MOVE 施術月ＷＲ    TO  連期間－施術月.
032950     MOVE 患者番号ＷＲ  TO  連期間－患者番号.
032960     MOVE 枝番ＷＲ      TO  連期間－枝番.
032970*
032980     CALL   "CHOUKI".
032990     CANCEL "CHOUKI".
033000*
033010**** 適用１を使用 (「前月初検のみ」がある時は、くっつける)
033020     IF ( 連期間－対象フラグ  = "YES" )
033030        IF ( 適用１Ｗ  = SPACE )
033040           MOVE NC"※長期施術継続理由裏面に記載"  TO 適用１Ｗ
033050        ELSE
033060           STRING 適用１Ｗ           DELIMITED BY SPACE
033070                  NC"，"             DELIMITED BY SIZE
033080                  NC"※長期施術継続理由裏面に記載"   DELIMITED BY SIZE
033090                  INTO 適用１Ｗ
033100           END-STRING
033110        END-IF
033120     END-IF.
033130*
033850*================================================================*
033860 初検加算時刻取得 SECTION.
033870*================================================================*
033880*****************************************************************
033890** 初検加算がある時、適用に「受付時間」を印字する。
033900**   時刻の印字は月3回まで可能
033910*****************************************************************
033920     IF ( レセ－時間外 = 1 ) OR ( レセ－深夜 = 1 ) OR
033930        ( レセ－休日   = 1 )
033940*
033950         MOVE 患者番号ＷＲ          TO 施記－患者番号
033960         MOVE 枝番ＷＲ              TO 施記－枝番
033970         MOVE 施術和暦ＷＲ          TO 施記－施術和暦
033980         MOVE 施術年ＷＲ            TO 施記－施術年
033990         MOVE 施術月ＷＲ            TO 施記－施術月
034000         MOVE ZERO                  TO 施記－施術日
034010         START 施術記録Ｆ   KEY IS >= 施記－患者コード
034020                                      施記－施術和暦年月日
034030         END-START
034040         IF ( 状態キー = "00" )
034050             MOVE ZERO  TO 初検加算カウント
034060             MOVE SPACE TO 終了フラグ２
034070             PERFORM 施術記録Ｆ読込
034080             PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
034090                           ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
034100                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
034110                           ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
034120                           ( 施記－施術月     NOT = 施術月ＷＲ      ) 
034130                   IF ( 施記－初検加算 = 1 OR 2 OR 3 ) AND
034140                      ( 施記－診療区分 = 2 )
034150                       COMPUTE 初検加算カウント = 初検加算カウント  + 1
034160                       IF ( 初検加算カウント <= 3 )
034170                           MOVE 施記－初検加算 TO 初検加算区分ＷＴ(初検加算カウント)
034180                           MOVE 施記－受付時   TO 初検加算時ＷＴ(初検加算カウント)
034190                           MOVE 施記－受付分   TO 初検加算分ＷＴ(初検加算カウント)
034200                       END-IF
034210                   END-IF
034220                   PERFORM 施術記録Ｆ読込
034230             END-PERFORM
034240******* 初検加算の時刻を適用にセット
033380             IF ( 初検加算時ＷＴ(1) NOT = ZERO ) OR ( 初検加算分ＷＴ(1) NOT = ZERO ) 
                      MOVE 初検加算時ＷＴ(1) TO 初検加算時Ｗ
                      MOVE ":"               TO 初検加算区切Ｗ
                      MOVE 初検加算分ＷＴ(1) TO 初検加算分Ｗ
                   END-IF
033380             IF ( 初検加算時ＷＴ(2) NOT = ZERO ) OR ( 初検加算分ＷＴ(2) NOT = ZERO ) 
031910                PERFORM 初検加算適用セット
                   END-IF
034260         END-IF
034270*
034280     END-IF.
034290*
034300*================================================================*
034310 初検加算適用セット SECTION.
034320*
034330     PERFORM VARYING 番号カウンタ FROM 1 BY 1
034340              UNTIL  番号カウンタ > 3
034350         IF ( 初検加算時ＷＴ(番号カウンタ)  = ZERO )  AND 
034360            ( 初検加算分ＷＴ(番号カウンタ)  = ZERO ) 
034370             CONTINUE
034380         ELSE
034390* 固定項目
034400             EVALUATE 初検加算区分ＷＴ(番号カウンタ) 
034410             WHEN 1
034420                MOVE NC"時間外"   TO 加算内容Ｗ(番号カウンタ)
034430             WHEN 2
034440                MOVE NC"休　日"   TO 加算内容Ｗ(番号カウンタ)
034450             WHEN 3
034460                MOVE NC"深　夜"   TO 加算内容Ｗ(番号カウンタ)
034470             END-EVALUATE
034480*
034490             MOVE NC"："          TO 加算区切Ｗ(番号カウンタ)
034500             MOVE NC"時"          TO 時固定Ｗ(番号カウンタ)
034510             MOVE NC"分"          TO 分固定Ｗ(番号カウンタ)
034520*
034530**** 数字→日本語変換
034540* 時間
034550             MOVE 初検加算時ＷＴ(番号カウンタ)  TO  数字Ｗ
034560             IF ( 数字Ｗ >= 10 )
034570                 MOVE 数字Ｗ１    TO 負傷番号Ｗ１
034580                 PERFORM 日本語変換
034590                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ１(番号カウンタ)
034600                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
034610                 PERFORM 日本語変換
034620                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
034630             ELSE
034640                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
034650                 PERFORM 日本語変換
034660                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
034670             END-IF
034680* 分
034690             MOVE 初検加算分ＷＴ(番号カウンタ)  TO  数字Ｗ
034700             MOVE 数字Ｗ１    TO 負傷番号Ｗ１
034710             PERFORM 日本語変換
034720             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ１(番号カウンタ)
034730             MOVE 数字Ｗ２    TO 負傷番号Ｗ１
034740             PERFORM 日本語変換
034750             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ２(番号カウンタ)
034760** 
034770        END-IF
034780     END-PERFORM.
034790*
034800     MOVE  初検加算集団ＮＷ(1)   TO 初検加算時刻１Ｗ. 
034810     MOVE  初検加算集団ＮＷ(2)   TO 初検加算時刻２Ｗ. 
034820     MOVE  初検加算集団ＮＷ(3)   TO 初検加算時刻３Ｗ. 
034830*
034840**** 適用１か２を使用（長期理由記載で適用１を使っている時は、適用２）
034850     IF ( 初検加算時ＷＴ(2)  = ZERO ) AND ( 初検加算分ＷＴ(2)  = ZERO ) 
034860         CONTINUE
034870     ELSE
034880         IF ( 適用１Ｗ  = SPACE )
034890               STRING NC"初検加算"       DELIMITED BY SIZE
034900                      初検加算時刻１Ｗ   DELIMITED BY SIZE
034910                      初検加算時刻２Ｗ   DELIMITED BY SIZE
034920                      初検加算時刻３Ｗ   DELIMITED BY SIZE
034930                      INTO 適用１Ｗ
034940               END-STRING
034950         ELSE
034960               STRING NC"初検加算"       DELIMITED BY SIZE
034970                      初検加算時刻１Ｗ   DELIMITED BY SIZE
034980                      初検加算時刻２Ｗ   DELIMITED BY SIZE
034990                      初検加算時刻３Ｗ   DELIMITED BY SIZE
035000                      INTO 適用１Ｗ
035010               END-STRING
035020         END-IF
035030     END-IF.
035040*
035050*================================================================*
035060 日本語変換 SECTION.
035070*
035080     MOVE NC"０"     TO 全角負傷番号Ｗ.
035090     CALL "htoz" WITH C LINKAGE
035100                        USING 負傷番号Ｗ１ 全角負傷番号Ｗ１.
035110*
035120*================================================================*
035130 委任年月日取得 SECTION.
035140*================================================================*
035150** ---// ここの受理年には、最終通院日が入っている為、退避する //----
036770     MOVE 受理和暦Ｗ TO 最終通院和暦Ｗ.
035160     MOVE 受理年Ｗ   TO 最終通院年Ｗ.
035170     MOVE 受理月Ｗ   TO 最終通院月Ｗ.
035180     MOVE 受理日Ｗ   TO 最終通院日Ｗ.
035190***
035200* (柔整師側)
035210     EVALUATE レセプト日付区分Ｗ 
035220*    /  最終通院日 /
035230     WHEN ZERO
036850         MOVE 最終通院和暦Ｗ TO 柔整師和暦Ｗ
035240         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
035250         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
035260         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
035270*    /  月末日 /
035280     WHEN 1 
035290         PERFORM 月末日取得
036910         MOVE 受理和暦Ｗ   TO 柔整師和暦Ｗ
035300         MOVE 受理年Ｗ     TO 柔整師年Ｗ
035310         MOVE 受理月Ｗ     TO 柔整師月Ｗ
035320         MOVE 受理日Ｗ     TO 柔整師日Ｗ
035330*    /  印字なし /
035340     WHEN 9
036960         MOVE ZERO         TO 柔整師和暦Ｗ
035350         MOVE ZERO         TO 柔整師年Ｗ
035360         MOVE ZERO         TO 柔整師月Ｗ
035370         MOVE ZERO         TO 柔整師日Ｗ
035380*    /  その他は、最終通院日 /
035390     WHEN OTHER
037010         MOVE 最終通院和暦Ｗ TO 柔整師和暦Ｗ
035400         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
035410         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
035420         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
035430     END-EVALUATE.
035440**
035450* (患者側)
035460     EVALUATE レセプト患者日付区分Ｗ 
035470*    /  最終通院日 /
035480     WHEN ZERO
037100         MOVE 最終通院和暦Ｗ TO 患者委任和暦Ｗ
035490         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
035500         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
035510         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
035520*    /  月末日 /
035530     WHEN 1 
035540         PERFORM 月末日取得
037160         MOVE 受理和暦Ｗ   TO 患者委任和暦Ｗ
035550         MOVE 受理年Ｗ     TO 患者委任年Ｗ
035560         MOVE 受理月Ｗ     TO 患者委任月Ｗ
035570         MOVE 受理日Ｗ     TO 患者委任日Ｗ
035580*    /  印字なし /
035590     WHEN 9
037210         MOVE ZERO         TO 患者委任和暦Ｗ
035600         MOVE ZERO         TO 患者委任年Ｗ
035610         MOVE ZERO         TO 患者委任月Ｗ
035620         MOVE ZERO         TO 患者委任日Ｗ
035630*    /  その他は、最終通院日 /
035640     WHEN OTHER
037260         MOVE 最終通院和暦Ｗ TO 患者委任和暦Ｗ
035650         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
035660         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
035670         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
035680     END-EVALUATE.
035690*
035700*================================================================*
035710 月末日取得 SECTION.
035720*
037350     MOVE 施術和暦ＷＲ TO 受理和暦Ｗ.
035730     MOVE 施術年ＷＲ   TO 受理年Ｗ.
035740     MOVE 施術月ＷＲ   TO 受理月Ｗ.
035750     MOVE 施術和暦ＷＲ TO 元－元号区分.
035760     READ 元号マスタ
035770     NOT INVALID KEY
035780         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
035790     END-READ.
035800     IF ( 施術西暦年Ｗ NOT = ZERO )
035810        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
035820     END-IF.
035830*
035840     EVALUATE 施術月ＷＲ
035850     WHEN 4
035860     WHEN 6
035870     WHEN 9
035880     WHEN 11
035890         MOVE 30 TO 受理日Ｗ
035900     WHEN 2
035910         DIVIDE 4 INTO 施術西暦年Ｗ GIVING    商Ｗ
035920                                    REMAINDER 余Ｗ
035930         END-DIVIDE
035940         IF ( 余Ｗ = ZERO )
035950             MOVE 29 TO 受理日Ｗ
035960         ELSE
035970             MOVE 28 TO 受理日Ｗ
035980         END-IF
035990     WHEN 1
036000     WHEN 3
036010     WHEN 5
036020     WHEN 7
036030     WHEN 8
036040     WHEN 10
036050     WHEN 12
036060         MOVE 31 TO 受理日Ｗ
036070     WHEN OTHER
036080          CONTINUE
036090     END-EVALUATE.
036100*
036110*================================================================*
036120 負傷原因取得 SECTION.
036130*================================================================*
036140********************************************************************
036150*  負傷原因コードが同じものは、1行にまとめて印字する。
036160*  例: ①② 家で転んだ.
036170*     負傷原因コードが同じものをまとめ、テーブルにセット
036180*     (ただし、部位を飛んで同じものは、2行になる)
036190********************************************************************
036200     MOVE  ZERO   TO  カウンタ カウンタ２.
036210     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
036220             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
036230*
036240****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
036250        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
036260*
036270           IF ( カウンタ = ZERO )
036280              MOVE 1   TO  カウンタ カウンタ２
036290              MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
036300              MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
036310              MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
036320           ELSE
036330              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
036340                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
036350                 COMPUTE カウンタ２ = カウンタ２  +  1
036360                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
036370              ELSE
036380                 COMPUTE カウンタ = カウンタ  +  1
036390                 MOVE 1   TO  カウンタ２
036400                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
036410                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
036420                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
036430              END-IF
036440           END-IF
036450        END-IF
036460     END-PERFORM.
036470**************************************************************************
036480*  負傷原因マスタより文章取得
036490**************************************************************************
036500     MOVE  ZERO   TO  カウンタ カウンタ２.
036510     PERFORM VARYING カウンタ FROM 1 BY 1
036520             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
036530** 健保は 区分 01
036540         MOVE 01                        TO 負原－区分コード
036550         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
036560         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
036570         READ 負傷原因Ｆ
036580         NOT INVALID KEY
036590             INITIALIZE 負傷原因ＷＴ
036600             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
036610             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
036620             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
036630             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
036640             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
036650             PERFORM VARYING カウンタ２ FROM 1 BY 1
036660                     UNTIL ( カウンタ２ > 9 )  OR 
036670                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
036680                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
036690                WHEN 1
036700                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
036710                WHEN 2
036720                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
036730                WHEN 3
036740                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
036750                WHEN 4
036760                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
036770                WHEN 5
036780                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
036750                WHEN 6
036760                   MOVE "⑥"  TO  負傷原因ナンバーＷ１(カウンタ２)
036770                WHEN 7
036780                   MOVE "⑦"  TO  負傷原因ナンバーＷ１(カウンタ２)
036790                WHEN OTHER
036800                   CONTINUE
036810                END-EVALUATE
036820             END-PERFORM
036830*
036840             IF 負原－負傷原因入力区分 = 1
036850                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
036860                        負傷原因１ＷＴ  DELIMITED BY SIZE
036870                        負傷原因２ＷＴ  DELIMITED BY SIZE
036880                        負傷原因３ＷＴ  DELIMITED BY SIZE
036890                        負傷原因４ＷＴ  DELIMITED BY SIZE
036900                        負傷原因５ＷＴ  DELIMITED BY SIZE
036910                        INTO 負傷原因内容合成Ｗ(カウンタ)
036920                 END-STRING
036930             ELSE
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
037020             END-IF
037030*
037040         END-READ
037050     END-PERFORM.
037060*
037070     PERFORM 負傷原因セット.
037080*
037090*================================================================*
037100 負傷原因セット SECTION.
037110*
037120**************************************************************************
037130*  文章が1行を超える時は、複数行に分解する。
037140**************************************************************************
037150     MOVE  ZERO   TO  カウンタ カウンタ２.
037160     PERFORM VARYING カウンタ FROM 1 BY 1
037170             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
037180*
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
037330*
037340     END-PERFORM.
037350*
037360*================================================================*
037370 長期理由文取得 SECTION.
037380*================================================================*
037390*
037400* 長期理由文取得は "CHOUBUN" を呼ぶ. 
037410     MOVE  SPACE TO  連長文－キー.
037420     INITIALIZE      連長文－キー.
037430     MOVE 施術和暦ＷＲ  TO  連長文－施術和暦.
037440     MOVE 施術年ＷＲ    TO  連長文－施術年.
037450     MOVE 施術月ＷＲ    TO  連長文－施術月.
037460     MOVE 患者番号ＷＲ  TO  連長文－患者番号.
037470     MOVE 枝番ＷＲ      TO  連長文－枝番.
037480** 日整山形用は56桁
037490     MOVE 56            TO  連長文－文桁数.
037500*
037510     CALL   "CHOUBUN".
037520     CANCEL "CHOUBUN".
037530*
037540*================================================================*
037550 基本料取得 SECTION.
037560*================================================================*
037570     MOVE 01                TO 料Ａ－区分コード.
037580     MOVE ZERO              TO 料Ａ－負傷種別.
037590     MOVE ZERO              TO 料Ａ－部位.
037600     MOVE ZERO              TO 料Ａ－左右区分.
037610     MOVE ZERO              TO 料Ａ－負傷位置番号.
037620     MOVE 施術和暦ＷＲ      TO 料Ａ－開始和暦.
037630     MOVE 施術年ＷＲ        TO 料Ａ－開始年.
037640     MOVE 施術月ＷＲ        TO 料Ａ－開始月.
037650     START 料金マスタ KEY IS <= 料－区分コード
037660                                料－部位コード
037670                                料－開始和暦年月
037680                                REVERSED
037690     END-START.
037700     READ 料金マスタ NEXT
037710     NOT AT END
037720         MOVE 料Ａ－冷罨法料   TO 冷罨法単価Ｗ
037730         MOVE 料Ａ－温罨法料   TO 温罨法単価Ｗ
037740         MOVE 料Ａ－電療料     TO 電療単価Ｗ
037750     END-READ.
037760*
037770*================================================================*
037780 負担給付割合取得 SECTION.
037790*================================================================*
037800     MOVE ZERO  TO 負担割合Ｗ         給付割合Ｗ.
037810     MOVE SPACE TO 負担割合チェックＷ 給付割合チェックＷ.
037820*
040724     MOVE レセ－負担割合 TO 負担割合Ｗ.
040724     MOVE レセ－給付割合 TO 給付割合Ｗ.
037950     PERFORM 負担給付割合セット.
037970*
038160*================================================================*
038170 負担給付割合セット SECTION.
038180*
038190     EVALUATE 給付割合Ｗ
038200     WHEN 10
038210         MOVE NC"○"  TO １０割チェックＷ
038220     WHEN 9
038230         MOVE NC"○"  TO ９割チェックＷ
      */前期高齢者１割は、給付割合を８割にする。(国が１割負担するため、患者１割、保険者８割、国１割となる)
               IF (受－保険種別 NOT = 05 ) AND (受－特別区分 = 1) AND (受－施術和暦年月 >= 42004)
                   MOVE SPACE  TO ９割チェックＷ
                   MOVE NC"○" TO ８割チェックＷ
               END-IF
038240     WHEN 8
038250         MOVE NC"○"  TO ８割チェックＷ
038260     WHEN 7
038270         MOVE NC"○"  TO ７割チェックＷ
038280     END-EVALUATE.
038290*
038300     EVALUATE 負担割合Ｗ
038310     WHEN ZERO
038320         MOVE NC"０割" TO 負担割合チェックＷ
038330     WHEN 1
038340         MOVE NC"１割" TO 負担割合チェックＷ
038350     WHEN 2
038360         MOVE NC"２割" TO 負担割合チェックＷ
038370     WHEN 3
038380         MOVE NC"３割" TO 負担割合チェックＷ
038390     END-EVALUATE.
038400*
038410*================================================================*
038420 会長委任判定 SECTION.
038430*================================================================*
038440*  会長委任フラグは、"YES"：会長委任 
038450*                    SPACE：委任しない
038440*  口座印字フラグは、"YES"：印字する 
038450*                    SPACE：印字しない
038460*----------------------------------------------------------------*
038530*  共済・自衛官    ：すべて会長委任。口座印字。
038540*  国保・退職・後高：委任しない。県外は口座印字。
038630*  社保・日雇・船員：県内(23)は、会長委任。口座印字なし。
038640*                    県以外は、委任しない。口座印字
038650*  組合            ：会長委任。
038660*                    口座印字。
038650*  助成（県内のみ）：委任しない。口座印字。
038670******************************************************************
038680     MOVE  SPACE  TO  会長委任フラグ.
038690*
038700     IF ( 公費種別ＷＲ = 05 )
038760         MOVE  SPACE          TO  会長委任フラグ
               IF ( 費用負担者番号ＷＲ(3:2) NOT = "23" )
038740             MOVE  "YES"      TO  口座印字フラグ
038750         ELSE
038760             MOVE  SPACE      TO  口座印字フラグ
038780         END-IF
038790     ELSE
038800         EVALUATE 保険種別ＷＲ
038810         WHEN 04
038820         WHEN 09
038830             MOVE  "YES"      TO  会長委任フラグ
038950*             IF ( 保険者番号ＷＲ(3:2) NOT = "23" )
038980                 MOVE  "YES"  TO  口座印字フラグ
038990*             ELSE
039000*                 MOVE  SPACE  TO  口座印字フラグ
039010*             END-IF
038840         WHEN 01
038760             MOVE  SPACE      TO  会長委任フラグ
038850             IF ( 保険者番号ＷＲ(1:2) NOT = "23" )
03889                  MOVE  "YES"  TO  口座印字フラグ
03890              ELSE
038910                 MOVE  SPACE  TO  口座印字フラグ
038930             END-IF
038850             IF ( 保険者番号ＷＲ = "133280" OR "133231" OR "133033" )
038830                 MOVE  "YES"  TO  会長委任フラグ
03889                  MOVE  "YES"  TO  口座印字フラグ
                   END-IF
038940         WHEN 08
038760             MOVE  SPACE      TO  会長委任フラグ
038950             IF ( 保険者番号ＷＲ(3:2) NOT = "23" )
03889                  MOVE  "YES"  TO  口座印字フラグ
03890              ELSE
038910                 MOVE  SPACE  TO  口座印字フラグ
039010             END-IF
039030         WHEN 02
                   IF (保険者番号ＷＲ(5:5)     = SPACE AND 保険者番号ＷＲ(1:2) = "23" ) OR
                      (保険者番号ＷＲ(5:5) NOT = SPACE AND 保険者番号ＷＲ(3:2) = "23" )
039050                 MOVE  "YES"  TO  会長委任フラグ
038910                 MOVE  SPACE  TO  口座印字フラグ
039060             ELSE
039070                 MOVE  SPACE  TO  会長委任フラグ
03889                  MOVE  "YES"  TO  口座印字フラグ
039080             END-IF
039090         WHEN 06
039100         WHEN 07
039110             IF ( 保険者番号ＷＲ(3:2) = "23" )
039120                 MOVE  "YES"  TO  会長委任フラグ
038910                 MOVE  SPACE  TO  口座印字フラグ
039130             ELSE
039140                 MOVE  SPACE  TO  会長委任フラグ
03889                  MOVE  "YES"  TO  口座印字フラグ
039150             END-IF
039160         WHEN 03
039180             MOVE  "YES"      TO  会長委任フラグ
039110*             IF ( 保険者番号ＷＲ(3:2) NOT = "23" )
03889                  MOVE  "YES"  TO  口座印字フラグ
038900*             ELSE
038910*                 MOVE  SPACE  TO  口座印字フラグ
039210*             END-IF
039220         WHEN OTHER
039230             CONTINUE
039240         END-EVALUATE
039250     END-IF.
039260*
039270*================================================================*
039280*================================================================*
039290 施術記録Ｆ読込 SECTION.
039300*
039310     READ 施術記録Ｆ NEXT
039320     AT END
039330         MOVE "YES" TO 終了フラグ２
039340     END-READ.
039350*
039610*================================================================*
039540 印刷処理 SECTION.
039610*================================================================*
039560     MOVE "YAI6123P" TO  定義体名Ｐ.
039570     MOVE "SCREEN"   TO  項目群名Ｐ.
039580     WRITE YAI6123P.
039590     PERFORM エラー処理Ｐ.
039600*
039610*================================================================*
039620 エラー処理Ｐ SECTION.
039630*================================================================*
039640     IF ( 通知情報Ｐ NOT = "00" )
039650         DISPLAY NC"帳票エラー"              UPON CONS
039660         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
039670         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
039680         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
039690         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
039700                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
039710         ACCEPT  キー入力 FROM CONS
039720         PERFORM ファイル閉鎖
039730         MOVE 99  TO PROGRAM-STATUS
039740         EXIT PROGRAM
039750     END-IF.
039760*
039770************
039780* 終了処理 *
039790************
039800******************************************************************
039810 受診者印刷区分更新 SECTION.
039820******************************************************************
039830** //  受診者情報Ｆの印刷区分に１をセットし、更新する。//  
039840*
039850     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
039860     MOVE 施術年ＷＲ         TO 受－施術年.
039870     MOVE 施術月ＷＲ         TO 受－施術月.
039880     MOVE 患者コードＷＲ     TO 受－患者コード.
039890     READ 受診者情報Ｆ
039900     NOT INVALID KEY
039910         MOVE  1  TO  受－レセ印刷区分
039920         REWRITE  受－レコード
039930         END-REWRITE
039940         IF ( 状態キー NOT = "00" )
039950            MOVE NC"受診者" TO ファイル名
039960            PERFORM エラー表示
039970         END-IF
039980     END-READ.
039990*
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
040000*================================================================*
040010 レセ摘要再セット SECTION.
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
      *     MOVE 連摘文－摘要文(7)    TO 長期理由文７.
      *     MOVE 連摘文－摘要文(8)    TO 長期理由文８.
040190*
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
040200*================================================================*
040210*================================================================*
040220 エラー表示 SECTION.
040230*================================================================*
040240     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
040250     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
040260     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
040270     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
040280                                                   UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
040290     ACCEPT  キー入力 FROM CONS
040300     PERFORM ファイル閉鎖.
040310     EXIT PROGRAM.
040320*
040330******************************************************************
040340 終了処理 SECTION.
040350******************************************************************
040360     PERFORM ファイル閉鎖.
040370*
040380*================================================================*
040390 ファイル閉鎖 SECTION.
040400*================================================================*
040410     CLOSE 印刷ファイル.
040420     CLOSE 元号マスタ 名称マスタ レセプトＦ 経過マスタ 料金マスタ
040430           制御情報マスタ 施術所情報マスタ  会情報マスタ
040440           保険者マスタ 市町村マスタ 請求先マスタ ＩＤ管理マスタ
040450           受診者情報Ｆ 施術記録Ｆ 負傷データＦ 負傷原因Ｆ.
040470*================================================================*
040480*================================================================*
040490 テスト印字処理 SECTION.
040500*
042550     MOVE ALL "X"    TO 県施術ＩＤ.
042640     MOVE ALL "Ｎ" TO 被保険者氏名.
042670     MOVE ALL "X"    TO 住所１ 住所２.
042700     MOVE ALL "X"    TO 保険者番号.
042740     MOVE ALL "Ｎ" TO 患者氏名.
042770     MOVE 99         TO 患者年 患者月 患者日.
042780     MOVE ALL "M"    TO 負傷原因１ 負傷原因２ 負傷原因３ 負傷原因４
                              負傷原因５ 負傷原因６.
042800     MOVE ALL NC"Ｎ" TO 負傷名１.
042810     MOVE 99 TO 負傷年１ 負傷月１ 負傷日１ 初検年１ 初検月１ 初検日１
042820                開始年１ 開始月１ 開始日１ 終了年１ 終了月１ 終了日１
042830                実日数１.
042840     MOVE NC"○" TO 治癒チェック１ 中止チェック１ 転医チェック１.
042850     MOVE ALL NC"Ｎ" TO 負傷名２.
042860     MOVE 99 TO 負傷年２ 負傷月２ 負傷日２ 初検年２ 初検月２ 初検日２
042870                開始年２ 開始月２ 開始日２ 終了年２ 終了月２ 終了日２
042880                実日数２.
042890     MOVE NC"○" TO 治癒チェック２ 中止チェック２ 転医チェック２.
042900     MOVE ALL NC"Ｎ" TO 負傷名３.
042910     MOVE 99 TO 負傷年３ 負傷月３ 負傷日３ 初検年３ 初検月３ 初検日３
042920                開始年３ 開始月３ 開始日３ 終了年３ 終了月３ 終了日３
042930                実日数３.
042940     MOVE NC"○" TO 治癒チェック３ 中止チェック３ 転医チェック３.
042950     MOVE ALL NC"Ｎ" TO 負傷名４.
042960     MOVE 99 TO 負傷年４ 負傷月４ 負傷日４ 初検年４ 初検月４ 初検日４
042970                開始年４ 開始月４ 開始日４ 終了年４ 終了月４ 終了日４
042980                実日数４.
042990     MOVE NC"○" TO 治癒チェック４ 中止チェック４ 転医チェック４.
043000     MOVE ALL NC"Ｎ" TO 負傷名５.
043010     MOVE 99 TO 負傷年５ 負傷月５ 負傷日５ 初検年５ 初検月５ 初検日５
043020                開始年５ 開始月５ 開始日５ 終了年５ 終了月５ 終了日５
043030                実日数５.
043040     MOVE NC"○" TO 治癒チェック５ 中止チェック５ 転医チェック５.
043050     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
043060             UNTIL ( 部位ＣＮＴ > 5 )
043070         MOVE ALL NC"Ｎ" TO 経過略称(部位ＣＮＴ)
043080     END-PERFORM.
           MOVE NC"○" TO 男チェック 女チェック 
           明治チェック 大正チェック 昭和チェック 平成チェック 
           １０割チェック ９割チェック ８割チェック ７割チェック 
           組合チェック 共済チェック 国保チェック 退職チェック 後期チェック 
           単独チェック ２併チェック 
           本人チェック ６歳チェック 家族チェック 高７チェック 高一チェック
           振込チェック 普通チェック 当座チェック 銀行チェック 金庫チェック
           農協チェック 本店チェック 支店チェック 本支所チェック

           MOVE ALL NC"□" TO 適用１
           MOVE ALL "X" TO 公費負担者番号 受給者番号 記号番号 適用２ 
      *     柔整師番号
      *
043090     MOVE NC"○" TO 新規チェック 継続チェック.
043100     MOVE 99999 TO  初検料.
043110     MOVE 99999 TO  再検料.
043120     MOVE 99.9 TO  往療距離.
043130     MOVE 99 TO  往療回数 金属回数 運動回数
                       金属月(1) 金属月(2) 金属月(3) 月(1) 月(2) 月(3) 金属日(1) 金属日(2) 金属日(3)
                       運動日(1) 運動日(2) 運動日(3) 運動日(4) 運動日(5) 明細書発行加算日.
043140     MOVE 99999 TO  往療料.
043160     MOVE 99999 TO  金属副子加算料 運動後療料.
043170     MOVE 999999 TO  小計.
043180     MOVE NC"○" TO  時間外チェック 休日チェック 深夜チェック.
043190     MOVE 99999 TO  初検加算料.
043200     MOVE NC"○" TO  夜間チェック 難路チェック 暴風雨雪チェック.
043210     MOVE 99999 TO  往療加算料.
043220     MOVE 99999 TO  施術情報提供料.
043220     MOVE 9999  TO  明細書発行加算料.
043230     MOVE NC"○" TO 整復料チェック 固定料チェック 施療料チェック.
043240     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
043250             UNTIL ( 部位ＣＮＴ > 5 )
043260         MOVE 99999 TO 初回処置料(部位ＣＮＴ)
043270     END-PERFORM.
043280     MOVE 999999 TO 初回処置料合計.
043290     MOVE 99    TO 後療回数１ 冷罨法回数１ 温罨法回数１ 電療回数１.
043300     MOVE 9999  TO 後療単価１ 冷罨法料１   温罨法料１   電療料１.
043310     MOVE 99999 TO 後療料１   小計１       長期込小計１.
043320     MOVE 9.9 TO 長期逓減率１.
043330     MOVE 99 TO 後療回数２ 冷罨法回数２ 温罨法回数２ 電療回数２.
043340     MOVE 9999  TO 後療単価２ 冷罨法料２   温罨法料２   電療料２.
043350     MOVE 99999 TO 後療料２   小計２       長期込小計２.
043360     MOVE 9.9 TO 長期逓減率２.
043370     MOVE 99 TO 後療回数３８ 冷罨法回数３８ 温罨法回数３８ 電療回数３８.
043380     MOVE 9999  TO 後療単価３８ 冷罨法料３８   温罨法料３８   電療料３８.
043390     MOVE 99999 TO 後療料３８ 小計３８ 長期込小計３８ 多部位込小計３８.
043400     MOVE 9.9 TO 長期逓減率３８.
043410     MOVE 99 TO 逓減開始月３０ 逓減開始日３０.
043420     MOVE 99 TO 後療回数３０ 冷罨法回数３０ 温罨法回数３０ 電療回数３０.
043430     MOVE 9999  TO 後療単価３０ 冷罨法料３０   温罨法料３０   電療料３０.
043440     MOVE 99999 TO 後療料３０ 小計３０ 長期込小計３０.
043450     MOVE 9.9 TO 長期逓減率３０.
043460     MOVE 99 TO 後療回数４５ 冷罨法回数４５ 温罨法回数４５ 電療回数４５.
043470     MOVE 9999  TO 後療単価４５ 冷罨法料４５   温罨法料４５   電療料４５.
043480     MOVE 99999 TO 後療料４５ 小計４５ 長期込小計４５ 多部位込小計４５.
043490     MOVE 9.9 TO 長期逓減率４５.
043500     MOVE 99 TO 逓減開始月４８ 逓減開始日４８.
043510     MOVE 99 TO 後療回数４８ 冷罨法回数４８ 温罨法回数４８ 電療回数４８.
043520     MOVE 9999  TO 後療単価４８ 冷罨法料４８   温罨法料４８   電療料４８.
043530     MOVE 99999 TO 後療料４８ 小計４８ 長期込小計４８ 多部位込小計４８.
043540     MOVE 9.9 TO 長期逓減率４８.
043550     MOVE 99 TO 逓減開始月４０ 逓減開始日４０.
043560     MOVE 99 TO 後療回数４０ 冷罨法回数４０ 温罨法回数４０ 電療回数４０.
043570     MOVE 9999  TO 後療単価４０ 冷罨法料４０   温罨法料４０   電療料４０.
043580     MOVE 99999 TO 後療料４０ 小計４０ 長期込小計４０.
043590     MOVE 9.9 TO 長期逓減率４０.
043860     MOVE ALL "X" TO 部位５０ 部位５８.
043600*
043630     MOVE 11 TO 逓減開始月５５ 逓減開始日５５.
043640     MOVE 22 TO 後療回数５５ 冷罨法回数５５ 温罨法回数５５ 電療回数５５.
043650     MOVE 3335  TO 後療単価５５ 冷罨法料５５   温罨法料５５   電療料５５.
043660     MOVE 99995 TO 後療料５５ 小計５５ 長期込小計５５ 多部位込小計５５.
043670     MOVE 8.8 TO 長期逓減率５５.
043700     MOVE 11 TO 逓減開始月５８ 逓減開始日５８.
043710     MOVE 22 TO 後療回数５８ 冷罨法回数５８ 温罨法回数５８ 電療回数５８.
043720     MOVE 3338  TO 後療単価５８ 冷罨法料５８   温罨法料５８   電療料５８.
043730     MOVE 99998 TO 後療料５８ 小計５８ 長期込小計５８ 多部位込小計５８.
043740     MOVE 8.8 TO 長期逓減率５８.
043760     MOVE 11 TO 逓減開始月５０ 逓減開始日５０.
043770     MOVE 22 TO 後療回数５０ 冷罨法回数５０ 温罨法回数５０ 電療回数５０.
043780     MOVE 3330  TO 後療単価５０ 冷罨法料５０   温罨法料５０   電療料５０.
043790     MOVE 99990 TO 後療料５０ 小計５０ 長期込小計５０.
043800     MOVE 8.8 TO 長期逓減率５０.
043820     MOVE ALL "Ｎ" TO 長期理由文１ 長期理由文２ 長期理由文３ 長期頻回
043830                      長期理由文４ 長期理由文５ 長期理由文６ 適用２.
043840     MOVE 999999 TO 合計 一部負担金 請求金額.
043870     MOVE 99 TO 受理年 受理月 受理日.
043880     MOVE 99 TO 委任年 委任月 委任日.
043890     MOVE 999  TO 施術所郵便番号１.
043900     MOVE 9999 TO 施術所郵便番号２.
043910     MOVE ALL "X" TO 施術所住所１ 施術所住所２.
043920     MOVE ALL "Ｎ" TO 接骨院名.
043930     MOVE ALL "X" TO 代表者カナ.
043940     MOVE ALL "Ｎ" TO 代表者名.
043950     MOVE ALL "X" TO 施術所電話番号.
041710*
041720*================================================================*
024380 保険者番号右詰め SECTION.
024390*
024400     MOVE 保険者番号Ｗ  TO  保険者番号左詰めＷ.
024410     MOVE SPACE         TO  保険者番号右詰めＷ.
024420*
024430     MOVE  9  TO  カウンタ.
024440*
024450     IF  保険者番号左詰めＷ１(8) NOT = SPACE
024460         COMPUTE カウンタ = カウンタ  -  1
024470         MOVE 保険者番号左詰めＷ１(8)  TO  保険者番号右詰めＷ１(カウンタ)
024480     END-IF.
024490     IF  保険者番号左詰めＷ１(7) NOT = SPACE
024500         COMPUTE カウンタ = カウンタ  -  1
024510         MOVE 保険者番号左詰めＷ１(7)  TO  保険者番号右詰めＷ１(カウンタ)
024520     END-IF.
024530     IF  保険者番号左詰めＷ１(6) NOT = SPACE
024540         COMPUTE カウンタ = カウンタ  -  1
024550         MOVE 保険者番号左詰めＷ１(6)  TO  保険者番号右詰めＷ１(カウンタ)
024560     END-IF.
024570     IF  保険者番号左詰めＷ１(5) NOT = SPACE
024580         COMPUTE カウンタ = カウンタ  -  1
024590         MOVE 保険者番号左詰めＷ１(5)  TO  保険者番号右詰めＷ１(カウンタ)
024600     END-IF.
024610     IF  保険者番号左詰めＷ１(4) NOT = SPACE
024620         COMPUTE カウンタ = カウンタ  -  1
024630         MOVE 保険者番号左詰めＷ１(4)  TO  保険者番号右詰めＷ１(カウンタ)
024640     END-IF.
024650     IF  保険者番号左詰めＷ１(3) NOT = SPACE
024660         COMPUTE カウンタ = カウンタ  -  1
024670         MOVE 保険者番号左詰めＷ１(3)  TO  保険者番号右詰めＷ１(カウンタ)
024680     END-IF.
024690     IF  保険者番号左詰めＷ１(2) NOT = SPACE
024700         COMPUTE カウンタ = カウンタ  -  1
024710         MOVE 保険者番号左詰めＷ１(2)  TO  保険者番号右詰めＷ１(カウンタ)
024720     END-IF.
024730     IF  保険者番号左詰めＷ１(1) NOT = SPACE
024740         COMPUTE カウンタ = カウンタ  -  1
024750         MOVE 保険者番号左詰めＷ１(1)  TO  保険者番号右詰めＷ１(カウンタ)
024760     END-IF.
024770*
024780*================================================================*
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
                   MOVE NC"○" TO 施術日チェックＷ(施記－施術日)
                   PERFORM 施術記録Ｆ読込
               END-PERFORM
           END-IF.
           MOVE 施術日チェックＷ(1)  TO 施術日チェック１.
           MOVE 施術日チェックＷ(2)  TO 施術日チェック２.
           MOVE 施術日チェックＷ(3)  TO 施術日チェック３.
           MOVE 施術日チェックＷ(4)  TO 施術日チェック４.
           MOVE 施術日チェックＷ(5)  TO 施術日チェック５.
           MOVE 施術日チェックＷ(6)  TO 施術日チェック６.
           MOVE 施術日チェックＷ(7)  TO 施術日チェック７.
           MOVE 施術日チェックＷ(8)  TO 施術日チェック８.
           MOVE 施術日チェックＷ(9)  TO 施術日チェック９.
           MOVE 施術日チェックＷ(10) TO 施術日チェック１０.
           MOVE 施術日チェックＷ(11) TO 施術日チェック１１.
           MOVE 施術日チェックＷ(12) TO 施術日チェック１２.
           MOVE 施術日チェックＷ(13) TO 施術日チェック１３.
           MOVE 施術日チェックＷ(14) TO 施術日チェック１４.
           MOVE 施術日チェックＷ(15) TO 施術日チェック１５.
           MOVE 施術日チェックＷ(16) TO 施術日チェック１６.
           MOVE 施術日チェックＷ(17) TO 施術日チェック１７.
           MOVE 施術日チェックＷ(18) TO 施術日チェック１８.
           MOVE 施術日チェックＷ(19) TO 施術日チェック１９.
           MOVE 施術日チェックＷ(20) TO 施術日チェック２０.
           MOVE 施術日チェックＷ(21) TO 施術日チェック２１.
           MOVE 施術日チェックＷ(22) TO 施術日チェック２２.
           MOVE 施術日チェックＷ(23) TO 施術日チェック２３.
           MOVE 施術日チェックＷ(24) TO 施術日チェック２４.
           MOVE 施術日チェックＷ(25) TO 施術日チェック２５.
           MOVE 施術日チェックＷ(26) TO 施術日チェック２６.
           MOVE 施術日チェックＷ(27) TO 施術日チェック２７.
           MOVE 施術日チェックＷ(28) TO 施術日チェック２８.
           MOVE 施術日チェックＷ(29) TO 施術日チェック２９.
           MOVE 施術日チェックＷ(30) TO 施術日チェック３０.
           MOVE 施術日チェックＷ(31) TO 施術日チェック３１.
022270*================================================================*
023220 請求先情報取得 SECTION.
023230*================================================================*
023240     MOVE ZERO  TO 組合委任区分Ｗ.
023250*
023260*/ 老人
023270     IF ( 公費種別ＷＲ = 05 )
023280        MOVE 公費種別ＷＲ       TO 市－公費種別
023290        MOVE 費用負担者番号ＷＲ TO 市－市町村番号
023300        READ 市町村マスタ
023310        INVALID KEY
023320            MOVE SPACE          TO 請求先名称Ｗ
023330        NOT INVALID KEY
023340            STRING 市－市町村名称  DELIMITED BY SPACE
023350                   "長　殿"        DELIMITED BY SIZE
023360                   INTO 請求先名称Ｗ
023370            END-STRING
023380        END-READ
023390     ELSE
023400        MOVE 保険種別ＷＲ   TO 保－保険種別
023410        MOVE 保険者番号ＷＲ TO 保－保険者番号
023420        READ 保険者マスタ
023430        INVALID KEY
023440            MOVE SPACE      TO 請求先名称Ｗ
023450        NOT INVALID KEY
023460            EVALUATE 保険種別ＷＲ 
023470*/ 国保
023480            WHEN  01
023490               IF ( 保険者番号ＷＲ(3:1) NOT = "3" )
023500                  STRING 保－保険者名称 DELIMITED BY SPACE
023510                         "長　殿"       DELIMITED BY SIZE
023520                         INTO 請求先名称Ｗ
023530                  END-STRING
                        IF 保険者番号ＷＲ(1:3) = "234"
                           MOVE "名古屋市長　殿"   TO 請求先名称Ｗ
                        END-IF
023540               ELSE
023550*/(国保組合)
023560                  IF ( 保険者番号ＷＲ(1:3)  = "063" )
023570                     STRING 保－保険者名称 DELIMITED BY SPACE
023580                            "理事長　殿"   DELIMITED BY SIZE
023590                            INTO 請求先名称Ｗ
023600                     END-STRING
023610                  ELSE
023500                      STRING 保－保険者名称 DELIMITED BY SPACE
023510                             "　殿"         DELIMITED BY SIZE
023520                             INTO 請求先名称Ｗ
023530                      END-STRING
023630                  END-IF
023640               END-IF
023650*/ 退職
023660            WHEN  08
023670               STRING 保－保険者名称 DELIMITED BY SPACE
023680                      "長　殿"       DELIMITED BY SIZE
023690                      INTO 請求先名称Ｗ
023700               END-STRING
                     IF 保険者番号ＷＲ(3:3) = "234"
                        MOVE "名古屋市長　殿"   TO 請求先名称Ｗ
                     END-IF
023710*/ 社保
023720            WHEN  02
023730*               IF ( 保険者番号ＷＲ(1:2) = "06" )
023740*                  MOVE NC"山形社会保険事務局長" TO 請求先名称Ｗ
023750*               ELSE
023760*                  IF ( 保－接尾語区分 = 1 )
023770*                     MOVE 保－保険者名称     TO 請求先名称Ｗ
023780*                  ELSE
023790*                     STRING 保－保険者名称   DELIMITED BY SPACE
023800*                            "社会保険事務所" DELIMITED BY SIZE
023810*                            INTO 請求先名称Ｗ
023820*                     END-STRING
023830*                  END-IF
023840*               END-IF
      */県内外問わず長を付ける/081027
023790               STRING 保－保険者名称 DELIMITED BY SPACE
023800                      "長　殿"       DELIMITED BY SIZE
023810                 INTO 請求先名称Ｗ
023820               END-STRING
023850*/ 日雇
023860            WHEN  06
023870*               IF ( 保険者番号ＷＲ(3:2) = "06" )
023880*                  MOVE NC"山形社会保険事務局長" TO 請求先名称Ｗ
023890*               ELSE
023900*                  IF ( 保－接尾語区分 = 1 )
023910*                     MOVE 保－保険者名称     TO 請求先名称Ｗ
023920*                  ELSE
023930*                     STRING 保－保険者名称   DELIMITED BY SPACE
023940*                            "社会保険事務所" DELIMITED BY SIZE
023950*                            INTO 請求先名称Ｗ
023960*                     END-STRING
023970*                  END-IF
023980*               END-IF
      */県内外問わず長を付ける/081027
023790               STRING 保－保険者名称 DELIMITED BY SPACE
023800                      "長　殿"       DELIMITED BY SIZE
023810                 INTO 請求先名称Ｗ
023820               END-STRING
023990*/ 船員
024000            WHEN  07
024010               IF ( 保険者番号ＷＲ(3:2) = "06" )
024020                  MOVE NC"山形社会保険事務局長" TO 請求先名称Ｗ
024030               ELSE
023790                   STRING 保－保険者名称 DELIMITED BY SPACE
023800                          "　殿"         DELIMITED BY SIZE
023810                     INTO 請求先名称Ｗ
023820                   END-STRING
024050               END-IF
024060*/ 組合
024070            WHEN  03
024080               STRING 保－保険者名称  DELIMITED BY SPACE
024090                      "健康保険組合"  DELIMITED BY SIZE
024100                      "　"            DELIMITED BY SIZE
024110                      保－支部部署名  DELIMITED BY SPACE
023800                      "　殿"          DELIMITED BY SIZE
024120                      INTO 請求先名称Ｗ
024130               END-STRING
024140*/(組合委任区分)
024150               MOVE 保－共済区分    TO 組合委任区分Ｗ
024160*/ 共済
024170            WHEN  04
024180               IF ( 保－支部部署名 NOT = SPACE )
024190                  STRING 保－保険者名称  DELIMITED BY SPACE
024200                         "共済組合"      DELIMITED BY SIZE
024210                         "　"            DELIMITED BY SIZE
024220                         保－支部部署名  DELIMITED BY SPACE
024230                         "支部長　殿"    DELIMITED BY SIZE
024240                         INTO 請求先名称Ｗ
024250                  END-STRING
024260               ELSE
024270                  STRING 保－保険者名称  DELIMITED BY SPACE
024280                         "共済組合　殿"  DELIMITED BY SIZE
024290                         INTO 請求先名称Ｗ
024300                  END-STRING
024310               END-IF
024320*/ その他
024330            WHEN OTHER
024270                STRING 保－保険者名称  DELIMITED BY SPACE
024280                       "　殿"          DELIMITED BY SIZE
024290                       INTO 請求先名称Ｗ
024300                END-STRING
024350            END-EVALUATE
024360        END-READ
024370     END-IF.
024380*
           MOVE ZERO TO カウンタ２
           PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 48
               IF (請求先名称１Ｗ(カウンタ:1) >= X"81" ) AND (請求先名称１Ｗ(カウンタ:1) <= X"9F" ) OR
                  (請求先名称１Ｗ(カウンタ:1) >= X"E0" ) AND (請求先名称１Ｗ(カウンタ:1) <= X"FC" )
                   COMPUTE カウンタ   = カウンタ   + 1
               ELSE
                   COMPUTE カウンタ２ = カウンタ２ + 1
               END-IF
           END-PERFORM
           DIVIDE 2 INTO カウンタ２ GIVING 商Ｗ REMAINDER 余Ｗ
           END-DIVIDE
           IF 余Ｗ = 1
               MOVE SPACE TO 請求先名称３Ｗ
               MOVE 請求先名称Ｗ(48:33) TO 請求先名称３Ｗ
               MOVE SPACE               TO 請求先名称Ｗ(48:1) 請求先名称２Ｗ
               MOVE 請求先名称３Ｗ TO 請求先名称２Ｗ
           END-IF.
      */右詰処理
           IF 請求先名称２Ｗ NOT = SPACE
               MOVE 請求先名称２Ｗ TO 請求先名称３Ｗ
               MOVE SPACE          TO 請求先名称２Ｗ
               MOVE SPACE TO 終了フラグ２
               PERFORM VARYING カウンタ FROM 32 BY -1
                       UNTIL  (カウンタ <= ZERO) OR (終了フラグ２ NOT = SPACE)
                   IF 請求先名称３Ｗ(カウンタ:1) NOT = SPACE
                       MOVE "YES" TO 終了フラグ２
                   END-IF
               END-PERFORM
               MOVE 請求先名称３Ｗ TO 請求先名称２Ｗ(32 - カウンタ:カウンタ + 1)
           END-IF.
024390*================================================================*
041730******************************************************************
041740 END PROGRAM YAI6123.
041750******************************************************************
