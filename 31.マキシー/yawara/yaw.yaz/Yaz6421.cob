000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAZ6421.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090* 安全保障整復   助成レセプト印刷（新柔ｳｨﾝﾄﾞｳｽﾞ版）*
000100*         MED = YAW610 YAZ6421P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2018-08-03
000130 DATE-COMPILED.          2018-08-03
      */東京13、宮城04の場合、前期高齢者１割は、給付割合を８割にする。(国が１割負担するため、患者１割、保険者８割、国１割となる)↓↓↓/160817
      */金属副子・運動後療の変更・追加/1805
      */明細書発行加算を適用２に追加/2022
      */2024.10  長期頻回を適用に追加/2407
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
000260     SELECT  保険者マスタ    ASSIGN      TO        HOKENSL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY               IS  保－保険種別
000300                                                          保－保険者番号
000310* 将来は、キー項目の保険者名称を保険者カナにする
000320                             ALTERNATE RECORD KEY     IS  保－保険種別
000330                                                          保－保険者名称
000340                                                          保－保険者番号
000350                             FILE STATUS              IS  状態キー
000360                             LOCK        MODE         IS  AUTOMATIC.
000370     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000380                             ORGANIZATION             IS  INDEXED
000390                             ACCESS MODE              IS  DYNAMIC
000400                             RECORD KEY               IS  元－元号区分
000410                             FILE STATUS              IS  状態キー
000420                             LOCK        MODE         IS  AUTOMATIC.
000430     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000440                             ORGANIZATION             IS  INDEXED
000450                             ACCESS MODE              IS  DYNAMIC
000460                             RECORD KEY               IS  名－区分コード
000470                                                          名－名称コード
000480                             FILE STATUS              IS  状態キー
000490                             LOCK        MODE         IS  AUTOMATIC.
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
000560     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000570                             ORGANIZATION             IS  INDEXED
000580                             ACCESS MODE              IS  DYNAMIC
000590                             RECORD KEY               IS  制－制御区分
000600                             FILE STATUS              IS  状態キー
000610                             LOCK        MODE         IS  AUTOMATIC.
000620     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000630                             ORGANIZATION             IS  INDEXED
000640                             ACCESS MODE              IS  DYNAMIC
000650                             RECORD KEY               IS  施情－施術所番号
000660                             FILE STATUS              IS  状態キー
000670                             LOCK        MODE         IS  AUTOMATIC.
000750     SELECT  経過マスタ      ASSIGN      TO        KEIKAL
000760                             ORGANIZATION             IS  INDEXED
000770                             ACCESS MODE              IS  DYNAMIC
000780                             RECORD KEY               IS  経－区分コード
000790                                                          経－経過コード
000800                             FILE STATUS              IS  状態キー
000810                             LOCK        MODE         IS  AUTOMATIC.
000820     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000830                             ORGANIZATION             IS  INDEXED
000840                             ACCESS MODE              IS  DYNAMIC
000850                             RECORD KEY               IS  受－施術和暦年月
000860                                                          受－患者コード
000870                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000880                                                          受－患者カナ
000890                                                          受－患者コード
000900                             ALTERNATE RECORD KEY     IS  受－患者コード
000910                                                          受－施術和暦年月
000920                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000930                                                          受－保険種別
000940                                                          受－保険者番号
000950                                                          受－患者コード
000960                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000970                                                          受－公費種別
000980                                                          受－費用負担者番号
000990                                                          受－患者コード
001000                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001010                                                          受－助成種別
001020                                                          受－費用負担者番号助成
001030                                                          受－患者コード
001040                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
001050                                                          受－施術和暦年月
001060                                                          受－患者コード
001070                             FILE STATUS              IS  状態キー
001080                             LOCK        MODE         IS  AUTOMATIC.
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
001090     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
001100                             ORGANIZATION             IS  INDEXED
001110                             ACCESS MODE              IS  DYNAMIC
001120                             RECORD KEY               IS  施記－施術和暦年月日
001130                                                          施記－患者コード
001140                             ALTERNATE RECORD KEY     IS  施記－患者コード
001150                                                          施記－施術和暦年月日
001160                             FILE STATUS              IS  状態キー
001170                             LOCK        MODE         IS  AUTOMATIC.
001180     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
001190                             ORGANIZATION             IS  INDEXED
001200                             ACCESS MODE              IS  DYNAMIC
001210                             RECORD KEY               IS  負－施術和暦年月
001220                                                          負－患者コード
001230                             ALTERNATE RECORD KEY     IS  負－患者コード
001240                                                          負－施術和暦年月
001250                             FILE STATUS              IS  状態キー
001260                             LOCK        MODE         IS  AUTOMATIC.
001270     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
001280                             ORGANIZATION             IS  INDEXED
001290                             ACCESS MODE              IS  DYNAMIC
001300                             RECORD KEY               IS  負原－区分コード
001310                                                          負原－負傷原因コード
001320                             FILE STATUS              IS  状態キー
001330                             LOCK        MODE         IS  AUTOMATIC.
001340     SELECT  ＩＤ管理マスタ    ASSIGN      TO        IDKANRL
001350                             ORGANIZATION             IS  INDEXED
001360                             ACCESS MODE              IS  DYNAMIC
001370                             RECORD KEY               IS  ＩＤ管－ＩＤ区分
001380                                                          ＩＤ管－施術所番号
001390                                                          ＩＤ管－保険種別
001400                                                          ＩＤ管－保険者番号
001410                             ALTERNATE RECORD KEY     IS  ＩＤ管－施術ＩＤ番号
001420                                                          ＩＤ管－ＩＤ区分
001430                                                          ＩＤ管－施術所番号
001440                                                          ＩＤ管－保険種別
001450                                                          ＩＤ管－保険者番号
001460                             FILE STATUS              IS  状態キー
001470                             LOCK        MODE         IS  AUTOMATIC.
001480     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
001490                             ORGANIZATION             IS  INDEXED
001500                             ACCESS MODE              IS  DYNAMIC
001510                             RECORD KEY               IS  市－公費種別
001520                                                          市－市町村番号
001530                             ALTERNATE RECORD KEY     IS  市－公費種別
001540                                                          市－市町村名称
001550                                                          市－市町村番号
001560                             FILE STATUS              IS  状態キー
001570                             LOCK        MODE         IS  AUTOMATIC.
000790     SELECT  請求先マスタ    ASSIGN      TO        SEIKYUSL
000800                             ORGANIZATION           IS  INDEXED
000810                             ACCESS MODE            IS  DYNAMIC
000820                             RECORD KEY             IS 請先－保険種別
000830                                                       請先－保険者番号
000840                             FILE STATUS            IS  状態キー
000850                             LOCK    MODE           IS  AUTOMATIC.
001720* 並び順印字用
001730     SELECT  作業ファイル４  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001740                             ORGANIZATION             IS  INDEXED
001750                             ACCESS                   IS  DYNAMIC
001760                             RECORD      KEY          IS  作４－施術和暦年月
001770                                                          作４－患者コード
001780                                                          作４－保険種別
001790                             FILE        STATUS       IS  状態キー
001800                             LOCK        MODE         IS  AUTOMATIC.
001810*
001820     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF002
001830                             SYMBOLIC    DESTINATION  IS "PRT"
001840                             FORMAT                   IS  定義体名Ｐ
001850                             GROUP                    IS  項目群名Ｐ
001860                             PROCESSING  MODE         IS  処理種別Ｐ
001870                             UNIT        CONTROL      IS  拡張制御Ｐ
001880                             FILE        STATUS       IS  通知情報Ｐ.
001890******************************************************************
001900*                      DATA DIVISION                             *
001910******************************************************************
001920 DATA                    DIVISION.
001930 FILE                    SECTION.
001940*                           ［ＲＬ＝  ３２０］
001950 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001960     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
001970*                           ［ＲＬ＝  １２８］
001980 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001990     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
002000*                           ［ＲＬ＝  １２８］
002010 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
002020     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
002060*                           ［ＲＬ＝  ２５６］
002070 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
002080     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
002090*                           ［ＲＬ＝  １２８］
002100 FD  施術所情報マスタ          BLOCK   CONTAINS   1   RECORDS.
002110     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
002150*                           ［ＲＬ＝  １２８］
002160 FD  経過マスタ          BLOCK   CONTAINS   1   RECORDS.
002170     COPY KEIKA          OF  XFDLIB  JOINING   経   AS  PREFIX.
002180*                           ［ＲＬ＝  ３２０］
002190 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
002200     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
002560*                          ［ＲＬ＝  1024］
000340 FD  受診者情報２Ｆ        BLOCK   CONTAINS   1   RECORDS.
000350     COPY JUSINJ2          OF  XFDLIB  JOINING   受２   AS  PREFIX.
002210*                           ［ＲＬ＝  ２５６］
002220 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
002230     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
002240*                           ［ＲＬ＝  １２８］
002250 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002260     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
002270*                           ［ＲＬ＝  １２８］
002280 FD  負傷原因Ｆ          BLOCK   CONTAINS   1   RECORDS.
002290     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
002300*                           ［ＲＬ＝  １２８］
002310 FD  ＩＤ管理マスタ          BLOCK   CONTAINS   1   RECORDS.
002320     COPY IDKANR    OF  XFDLIB  JOINING   ＩＤ管   AS  PREFIX.
002330*                           ［ＲＬ＝  ２５６］
002340 FD  市町村マスタ          BLOCK   CONTAINS   1   RECORDS.
002350     COPY SITYOSN        OF  XFDLIB  JOINING   市   AS  PREFIX.
002280*                           ［ＲＬ＝  １２８］
002290 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
002300     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
002390**
002400**
002410 FD  作業ファイル４ RECORD  CONTAINS 32 CHARACTERS.
002420 01  作４－レコード.
002430     03  作４－レコードキー.
002440         05  作４－施術和暦年月.
002450             07  作４－施術和暦            PIC 9.
002460             07  作４－施術年              PIC 9(2).
002470             07  作４－施術月              PIC 9(2).
002480         05  作４－患者コード.
002490             07 作４－患者番号             PIC 9(6).
002500             07 作４－枝番                 PIC X(1).
002510         05  作４－保険種別                PIC 9(2).
002520     03  作４－レコードデータ.
002530         05  作４－順番                    PIC 9(4).
002540         05  FILLER                        PIC X(14).
002550*
002560*
002570 FD  印刷ファイル.
002580     COPY YAZ6421P        OF  XMDLIB.
002590*----------------------------------------------------------------*
002600******************************************************************
002610*                WORKING-STORAGE SECTION                         *
002620******************************************************************
002630 WORKING-STORAGE         SECTION.
002640 01 キー入力                           PIC X     VALUE SPACE.
002650 01 状態キー                           PIC X(2)  VALUE SPACE.
002660 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002670 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002680 01 初検フラグ                         PIC X(3)  VALUE SPACE.
002690 01 ファイル名                         PIC N(6)  VALUE SPACE.
002700 01 レセプトＰＧＷ                     PIC X(8)  VALUE SPACE.
002710 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
002720 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
002730 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002740 01 患者番号Ｗ                         PIC 9(6)  VALUE ZERO.
002750 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002760 01 部位名称Ｗ                         PIC N(12) VALUE SPACE.
002770 01 部位長Ｗ                           PIC 9(2) VALUE 1.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002780**
002790 01 遅延フラグ                         PIC X(3) VALUE SPACE.
002800 01 遅延回数Ｗ                         PIC 9(4) VALUE ZERO.
002810 01 遅延ＣＮＴ                         PIC 9(5) VALUE ZERO.
002820 01 最大登録数Ｗ                       PIC 9 VALUE ZERO.
002830 01 負傷連続登録Ｗ                     PIC 9 VALUE ZERO.
002840**
002850** 数字→日本語変換
002860 01 数字Ｗ                             PIC 9(2).
002870 01 数字Ｒ REDEFINES 数字Ｗ.
002880    03 数字Ｗ１                        PIC X(1).
002890    03 数字Ｗ２                        PIC X(1).
002900*
002910 01 負傷番号Ｗ                         PIC 9.
002920 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
002930    03 負傷番号Ｗ１                    PIC X.
002940*
002950 01 全角負傷番号Ｗ                     PIC N.
002960 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
002970    03 全角負傷番号Ｗ１                PIC X(2).
002980*
002990 01 カウンタ                           PIC 9(2)  VALUE ZERO.
003000 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
003010*
003020* 退避用
003030 01 終了年月日ＷＴ.
002980    03 終了和暦ＷＴ                    PIC 9     VALUE ZERO.
003040    03 終了年ＷＴ                      PIC 9(2)  VALUE ZERO.
003050    03 終了月ＷＴ                      PIC 9(2)  VALUE ZERO.
003060    03 終了日ＷＴ                      PIC 9(2)  VALUE ZERO.
003070* 初検日退避用
003080 01 初検年月日ＷＴ.
003090    03 初検和暦ＷＴ                    PIC 9     VALUE ZERO.
003100    03 初検年ＷＴ                      PIC 9(2)  VALUE ZERO.
003110    03 初検月ＷＴ                      PIC 9(2)  VALUE ZERO.
003120    03 初検日ＷＴ                      PIC 9(2)  VALUE ZERO.
003130* 初検加算時刻用
003140 01 初検加算ＷＴ.
003150    03 初検加算カウント                PIC 9    VALUE ZERO.
003160    03 番号カウンタ                    PIC 9    VALUE ZERO.
003170    03 初検加算集団ＷＴ  OCCURS 3.
003180       05 初検加算区分ＷＴ             PIC 9    VALUE ZERO.
003190       05 初検加算時ＷＴ               PIC 9(2) VALUE ZERO.
003200       05 初検加算分ＷＴ               PIC 9(2) VALUE ZERO.
003210    03 初検加算集団ＮＷ  OCCURS 3.
003220       05 加算区切Ｗ                   PIC N(1) VALUE SPACE.
003230       05 加算内容Ｗ                   PIC N(3) VALUE SPACE.
003240       05 初検加算時ＮＷ１             PIC N(1) VALUE SPACE.
003250       05 初検加算時ＮＷ２             PIC N(1) VALUE SPACE.
003260       05 時固定Ｗ                     PIC N(1) VALUE SPACE.
003270       05 初検加算分ＮＷ１             PIC N(1) VALUE SPACE.
003280       05 初検加算分ＮＷ２             PIC N(1) VALUE SPACE.
003290       05 分固定Ｗ                     PIC N(1) VALUE SPACE.
003300    03 初検加算時刻１Ｗ                PIC N(10) VALUE SPACE.
003310    03 初検加算時刻２Ｗ                PIC N(10) VALUE SPACE.
003320    03 初検加算時刻３Ｗ                PIC N(10) VALUE SPACE.
003070    03 初検加算区切Ｗ                  PIC X     VALUE SPACE.
003080    03 初検加算時Ｗ                    PIC 9(2)  VALUE ZERO.
003090    03 初検加算分Ｗ                    PIC 9(2)  VALUE ZERO.
003330* 負傷原因用
003340 01 負傷原因ＷＴ.
003350    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
003360    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
003370    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
003380    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
003390    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
003400    03 負傷原因ナンバーＷＴ.
003410       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
003420    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
003430 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
003440 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
003450 01 負傷原因ＴＢＬ.
003460    03 負傷原因コードＴＢＬ            OCCURS 9.
003470       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
003480       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
003490       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
003500 01 負傷原因内容Ｗ.
003510    03 負傷原因内容合成Ｗ              PIC X(318) OCCURS 9 VALUE SPACE.
003620    03 負傷原因内容分解ＸＷ.
003630       05 負傷原因内容１ＸＷ           PIC X(70)  VALUE SPACE.
003640       05 負傷原因内容２ＸＷ           PIC X(70)  VALUE SPACE.
003640       05 負傷原因内容３ＸＷ           PIC X(70)  VALUE SPACE.
003650       05 負傷原因内容４ＸＷ           PIC X(70)  VALUE SPACE.
003650       05 負傷原因内容５ＸＷ           PIC X(38)  VALUE SPACE.
003560*
003570*************
003580* 共済番号用
003590 01 共済連番号集団Ｗ.
003600    03 共済連番号名Ｗ                  PIC X(14)  VALUE SPACE.
003610    03 共済連番号名ＮＷ REDEFINES  共済連番号名Ｗ  PIC N(7).
003620    03 共済連番号Ｗ                    PIC X(6)  VALUE SPACE.
003630    03 共済連番号単位Ｗ                PIC X(2)  VALUE SPACE.
003640    03 共済連番号単位ＮＷ REDEFINES  共済連番号単位Ｗ  PIC N.
003650* 自衛官番号用
003660 01 自衛官番号集団Ｗ.
003670    03 自衛官番号名Ｗ                  PIC X(8)  VALUE SPACE.
003680    03 自衛官番号名ＮＷ REDEFINES  自衛官番号名Ｗ  PIC N(4).
003690    03 自衛官番号Ｗ                    PIC X(6)  VALUE SPACE.
003700    03 自衛官番号単位Ｗ                PIC X(2)  VALUE SPACE.
003710    03 自衛官番号単位ＮＷ REDEFINES  自衛官番号単位Ｗ  PIC N.
003720 01 脱出フラグ                         PIC X(3)  VALUE SPACE.
003730*
003740* 保険者番号
003750 01 保険者番号比較Ｗ                   PIC X(6)   VALUE SPACE.
003760*
003770** 前月初検のみ用
003780 01 初日再検フラグ                     PIC X(3)  VALUE SPACE.
003790 01 前月フラグ                         PIC X(3)  VALUE SPACE.
003800*
003810 01 計算年月日Ｗ.
003820    03 計算和暦Ｗ                      PIC 9(1)  VALUE ZERO.
003830    03 計算年Ｗ                        PIC S9(2)  VALUE ZERO.
003840    03 計算月Ｗ                        PIC S9(2)  VALUE ZERO.
003850    03 計算日Ｗ                        PIC S9(2)  VALUE ZERO.
003860 01 開始年月日２Ｗ.
003870    03 開始和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003880    03 開始年２Ｗ                      PIC 9(2)  VALUE ZERO.
003890    03 開始月２Ｗ                      PIC 9(2)  VALUE ZERO.
003900    03 開始日２Ｗ                      PIC 9(2)  VALUE ZERO.
003910    03 開始西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003920 01 終了年月日２Ｗ.
003930    03 終了和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003940    03 終了年２Ｗ                      PIC 9(2)  VALUE ZERO.
003950    03 終了月２Ｗ                      PIC 9(2)  VALUE ZERO.
003960    03 終了日２Ｗ                      PIC 9(2)  VALUE ZERO.
003970    03 終了西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003980***
003990** 負傷原因・長期理由印刷区分用
004000 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
004010 01 長期理由印刷区分Ｗ                 PIC 9 VALUE ZERO.
004020*
004030** レセ下段の日付区分用 (0:最終通院日、1:月末日、9:印字なし)
004040 01 レセプト日付区分Ｗ                 PIC 9 VALUE ZERO.
004050 01 レセプト患者日付区分Ｗ             PIC 9 VALUE ZERO.
004060*
004070** 月末日用
004080 01 施術西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
004090 01 商Ｗ                               PIC 9(3)  VALUE ZERO.
004100 01 余Ｗ                               PIC 9(3)  VALUE ZERO.
004110*
004120** 枝番判定用
004130 01 開始診療日手動区分Ｗ               PIC 9    VALUE ZERO.
004140*
004150*
004160** 助成レセまとめ用
004170 01 助成レセまとめフラグ               PIC X(3)  VALUE SPACE.
004180 01 助成種別略称Ｗ                     PIC N(4)  VALUE SPACE.
004190 01 助成種別略称Ｗ２                   PIC N(4)  VALUE SPACE.
004200*
004210* レセ摘要用( N(38)固定） /
004220 01 負傷の経過Ｗ.
004230    03 負傷の経過行Ｗ                  PIC X(76) OCCURS 2 VALUE SPACE.
004240 01 負傷の経過ＮＷ REDEFINES 負傷の経過Ｗ.
004250    03 負傷の経過行ＮＷ                PIC N(38) OCCURS 2.
004260*
004320*
004330* 負傷原因印刷区分
004340 01 レセ負傷原因印刷区分Ｗ             PIC 9    VALUE ZERO.
004440 01 レセ長期理由印刷区分Ｗ             PIC 9    VALUE ZERO.
004350*
004351*
004352* 福岡の経過固定印字用に使用
004353 01 全柔ＦＰＤ区分Ｗ                   PIC 9     VALUE ZERO.
004354 01 経過部位数字Ｗ                     PIC N(1)  VALUE SPACE.
      *
      */金属副子・運動後療の変更・追加/1805
       01 金属副子ＣＭ                       PIC X(200) VALUE SPACE.
004355*
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
          03 長期頻回ＣＭ２                  PIC X(280) VALUE SPACE.
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
004356*
004357*
004360****************
004370* 連結項目待避 *
004380****************
004390*    ************
004400*    * 印刷キー *
004410*    ************
004420 01 対象データＷＲ.
004430    03 施術和暦年月ＷＲ.
004440       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
004450       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
004460       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
004470    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004480    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
004490    03 公費種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004500    03 費用負担者番号ＷＲ               PIC X(10) VALUE SPACE.
004510    03 助成種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004520    03 費用負担者番号助成ＷＲ           PIC X(10) VALUE SPACE.
004530    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
004540    03 患者カナＷＲ                     PIC X(20) VALUE SPACE.
004550    03 患者コードＷＲ.
004560       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
004570       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
004580*    ************
004590*    * 料金情報 *
004600*    ************
004610*    月毎の料金
004620***********************
004630 01 料金１ＷＲ.
004640   03 初検ＷＲ.
004650      05 負担割合ＷＲ               PIC 9(3)    VALUE ZERO.
004660      05 初検料ＷＲ                 PIC 9(5)    VALUE ZERO.
004670      05 初検加算料ＷＲ             PIC 9(5)    VALUE ZERO.
         03 相談料ＷＲ                    PIC 9(4)    VALUE ZERO.
004680   03 再検料ＷＲ                    PIC 9(5)    VALUE ZERO.
004690   03 往療ＷＲ.
004700      05 往療距離ＷＲ               PIC 9(2)V9  VALUE ZERO.
004710      05 往療回数ＷＲ               PIC 9(2)    VALUE ZERO.
004720      05 往療料ＷＲ                 PIC 9(5)    VALUE ZERO.
004730      05 往療加算料ＷＲ             PIC 9(5)    VALUE ZERO.
004740   03 金属副子加算料ＷＲ            PIC 9(5)    VALUE ZERO.
004750   03 施術情報提供料ＷＲ            PIC 9(5)    VALUE ZERO.
004760   03 合計ＷＲ                      PIC 9(6)    VALUE ZERO.
004770   03 一部負担金ＷＲ                PIC 9(6)    VALUE ZERO.
004780   03 請求金額ＷＲ                  PIC 9(6)    VALUE ZERO.
004790   03 給付割合ＷＲ                  PIC 9(1)    VALUE ZERO.
004800   03 受給者負担額ＷＲ              PIC 9(6)    VALUE ZERO.
004810   03 助成請求金額ＷＲ              PIC 9(6)    VALUE ZERO.
004820*
004830* 負傷部位毎の料金
004840***********************
004850 01 料金２ＷＲ.
004860   03 初回処置ＷＲ    OCCURS   9.
004870      05 初回処置料ＷＲ             PIC 9(5)    VALUE ZERO.
004880*
004890* 逓減毎の料金
004900***********************
004910 01 料金３ＷＲ.
004920**********
004930* １部位 *
004940**********
004950   03 部位１ＷＲ.
004960      05 後療１ＷＲ.
004970         07 後療単価１ＷＲ              PIC 9(4)    VALUE ZERO.
004980         07 後療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
004990         07 後療料１ＷＲ                PIC 9(5)    VALUE ZERO.
005000      05 冷罨法１ＷＲ.
005010         07 冷罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
005020         07 冷罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
005030      05 温罨法１ＷＲ.
005040         07 温罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
005050         07 温罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
005060      05 電療１ＷＲ.
005070         07 電療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
005080         07 電療料１ＷＲ                PIC 9(4)    VALUE ZERO.
005090      05 小計１ＷＲ                     PIC 9(6)    VALUE ZERO.
005100      05 長期逓減率１ＷＲ               PIC 9(3)    VALUE ZERO.
005110      05 長期込小計１ＷＲ               PIC 9(6)    VALUE ZERO.
005120**********
005130* ２部位 *
005140**********
005150   03 部位２ＷＲ.
005160      05 後療２ＷＲ.
005170         07 後療単価２ＷＲ              PIC 9(4)    VALUE ZERO.
005180         07 後療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
005190         07 後療料２ＷＲ                PIC 9(5)    VALUE ZERO.
005200      05 冷罨法２ＷＲ.
005210         07 冷罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
005220         07 冷罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
005230      05 温罨法２ＷＲ.
005240         07 温罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
005250         07 温罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
005260      05 電療２ＷＲ.
005270         07 電療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
005280         07 電療料２ＷＲ                PIC 9(4)    VALUE ZERO.
005290      05 小計２ＷＲ                     PIC 9(6)    VALUE ZERO.
005300      05 長期逓減率２ＷＲ               PIC 9(3)    VALUE ZERO.
005310      05 長期込小計２ＷＲ               PIC 9(6)    VALUE ZERO.
005320******************
005330* ３部位／８割 *
005340******************
005350   03 部位３８ＷＲ.
005360      05 後療３８ＷＲ.
005370         07 後療単価３８ＷＲ              PIC 9(4)  VALUE ZERO.
005380         07 後療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
005390         07 後療料３８ＷＲ                PIC 9(5)  VALUE ZERO.
005400      05 冷罨法３８ＷＲ.
005410         07 冷罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
005420         07 冷罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
005430      05 温罨法３８ＷＲ.
005440         07 温罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
005450         07 温罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
005460      05 電療３８ＷＲ.
005470         07 電療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
005480         07 電療料３８ＷＲ                PIC 9(4)  VALUE ZERO.
005490      05 小計３８ＷＲ                     PIC 9(6)  VALUE ZERO.
005500      05 多部位込小計３８ＷＲ             PIC 9(6)  VALUE ZERO.
005510      05 長期逓減率３８ＷＲ               PIC 9(3)  VALUE ZERO.
005520      05 長期込小計３８ＷＲ               PIC 9(6)  VALUE ZERO.
005530******************
005540* ３部位／１０割 *
005550******************
005560   03 部位３０ＷＲ.
005570      05 逓減開始月日３０ＷＲ.
005580         07 逓減開始月３０ＷＲ            PIC 9(2)  VALUE ZERO.
005590         07 逓減開始日３０ＷＲ            PIC 9(2)  VALUE ZERO.
005600      05 後療３０ＷＲ.
005610         07 後療単価３０ＷＲ              PIC 9(4)  VALUE ZERO.
005620         07 後療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
005630         07 後療料３０ＷＲ                PIC 9(5)  VALUE ZERO.
005640      05 冷罨法３０ＷＲ.
005650         07 冷罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
005660         07 冷罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
005670      05 温罨法３０ＷＲ.
005680         07 温罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
005690         07 温罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
005700      05 電療３０ＷＲ.
005710         07 電療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
005720         07 電療料３０ＷＲ                PIC 9(4)  VALUE ZERO.
005730      05 小計３０ＷＲ                     PIC 9(6)  VALUE ZERO.
005740      05 長期逓減率３０ＷＲ               PIC 9(3)  VALUE ZERO.
005750      05 長期込小計３０ＷＲ               PIC 9(6)  VALUE ZERO.
005760****************
005770* ４部位／５割 *
005780****************
005790   03 部位４５ＷＲ.
005800      05 後療４５ＷＲ.
005810         07 後療単価４５ＷＲ              PIC 9(4)  VALUE ZERO.
005820         07 後療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005830         07 後療料４５ＷＲ                PIC 9(5)  VALUE ZERO.
005840      05 冷罨法４５ＷＲ.
005850         07 冷罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005860         07 冷罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
005870      05 温罨法４５ＷＲ.
005880         07 温罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005890         07 温罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
005900      05 電療４５ＷＲ.
005910         07 電療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005920         07 電療料４５ＷＲ                PIC 9(4)  VALUE ZERO.
005930      05 小計４５ＷＲ                     PIC 9(6)  VALUE ZERO.
005940      05 多部位込小計４５ＷＲ             PIC 9(6)  VALUE ZERO.
005950      05 長期逓減率４５ＷＲ               PIC 9(3)  VALUE ZERO.
005960      05 長期込小計４５ＷＲ               PIC 9(6)  VALUE ZERO.
005970****************
005980* ４部位／８割 *
005990****************
006000   03 部位４８ＷＲ.
006010      05 逓減開始月日４８ＷＲ.
006020         07 逓減開始月４８ＷＲ            PIC 9(2)  VALUE ZERO.
006030         07 逓減開始日４８ＷＲ            PIC 9(2)  VALUE ZERO.
006040      05 後療４８ＷＲ.
006050         07 後療単価４８ＷＲ              PIC 9(4)  VALUE ZERO.
006060         07 後療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
006070         07 後療料４８ＷＲ                PIC 9(5)  VALUE ZERO.
006080      05 冷罨法４８ＷＲ.
006090         07 冷罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
006100         07 冷罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
006110      05 温罨法４８ＷＲ.
006120         07 温罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
006130         07 温罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
006140      05 電療４８ＷＲ.
006150         07 電療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
006160         07 電療料４８ＷＲ                PIC 9(4)  VALUE ZERO.
006170      05 小計４８ＷＲ                     PIC 9(6)  VALUE ZERO.
006180      05 多部位込小計４８ＷＲ             PIC 9(6)  VALUE ZERO.
006190      05 長期逓減率４８ＷＲ               PIC 9(3)  VALUE ZERO.
006200      05 長期込小計４８ＷＲ               PIC 9(6)  VALUE ZERO.
006210******************
006220* ４部位／１０割 *
006230******************
006240   03 部位４０ＷＲ.
006250      05 逓減開始月日４０ＷＲ.
006260         07 逓減開始月４０ＷＲ            PIC 9(2)  VALUE ZERO.
006270         07 逓減開始日４０ＷＲ            PIC 9(2)  VALUE ZERO.
006280      05 後療４０ＷＲ.
006290         07 後療単価４０ＷＲ              PIC 9(4)  VALUE ZERO.
006300         07 後療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
006310         07 後療料４０ＷＲ                PIC 9(5)  VALUE ZERO.
006320      05 冷罨法４０ＷＲ.
006330         07 冷罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
006340         07 冷罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
006350      05 温罨法４０ＷＲ.
006360         07 温罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
006370         07 温罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
006380      05 電療４０ＷＲ.
006390         07 電療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
006400         07 電療料４０ＷＲ                PIC 9(4)  VALUE ZERO.
006410      05 小計４０ＷＲ                     PIC 9(6)  VALUE ZERO.
006420      05 長期逓減率４０ＷＲ               PIC 9(3)  VALUE ZERO.
006430      05 長期込小計４０ＷＲ               PIC 9(6)  VALUE ZERO.
006440********************
006450* ５部位／２．５割 *
006460********************
006470   03 部位５２ＷＲ.
006480      05 後療５２ＷＲ.
006490         07 後療単価５２ＷＲ              PIC 9(4)  VALUE ZERO.
006500         07 後療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
006510         07 後療料５２ＷＲ                PIC 9(5)  VALUE ZERO.
006520      05 冷罨法５２ＷＲ.
006530         07 冷罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
006540         07 冷罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
006550      05 温罨法５２ＷＲ.
006560         07 温罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
006570         07 温罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
006580      05 電療５２ＷＲ.
006590         07 電療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
006600         07 電療料５２ＷＲ                PIC 9(4)  VALUE ZERO.
006610      05 小計５２ＷＲ                     PIC 9(6)  VALUE ZERO.
006620      05 多部位込小計５２ＷＲ             PIC 9(6)  VALUE ZERO.
006630      05 長期逓減率５２ＷＲ               PIC 9(3)  VALUE ZERO.
006640      05 長期込小計５２ＷＲ               PIC 9(6)  VALUE ZERO.
006650****************
006660* ５部位／５割 *
006670****************
006680   03 部位５５ＷＲ.
006690      05 逓減開始月日５５ＷＲ.
006700         07 逓減開始月５５ＷＲ            PIC 9(2)  VALUE ZERO.
006710         07 逓減開始日５５ＷＲ            PIC 9(2)  VALUE ZERO.
006720      05 後療５５ＷＲ.
006730         07 後療単価５５ＷＲ              PIC 9(4)  VALUE ZERO.
006740         07 後療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
006750         07 後療料５５ＷＲ                PIC 9(5)  VALUE ZERO.
006760      05 冷罨法５５ＷＲ.
006770         07 冷罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
006780         07 冷罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
006790      05 温罨法５５ＷＲ.
006800         07 温罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
006810         07 温罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
006820      05 電療５５ＷＲ.
006830         07 電療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
006840         07 電療料５５ＷＲ                PIC 9(4)  VALUE ZERO.
006850      05 小計５５ＷＲ                     PIC 9(6)  VALUE ZERO.
006860      05 多部位込小計５５ＷＲ             PIC 9(6)  VALUE ZERO.
006870      05 長期逓減率５５ＷＲ               PIC 9(3)  VALUE ZERO.
006880      05 長期込小計５５ＷＲ               PIC 9(6)  VALUE ZERO.
006890****************
006900* ５部位／８割 *
006910****************
006920   03 部位５８ＷＲ.
006930      05 逓減開始月日５８ＷＲ.
006940         07 逓減開始月５８ＷＲ            PIC 9(2)  VALUE ZERO.
006950         07 逓減開始日５８ＷＲ            PIC 9(2)  VALUE ZERO.
006960      05 後療５８ＷＲ.
006970         07 後療単価５８ＷＲ              PIC 9(4)  VALUE ZERO.
006980         07 後療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
006990         07 後療料５８ＷＲ                PIC 9(5)  VALUE ZERO.
007000      05 冷罨法５８ＷＲ.
007010         07 冷罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
007020         07 冷罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
007030      05 温罨法５８ＷＲ.
007040         07 温罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
007050         07 温罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
007060      05 電療５８ＷＲ.
007070         07 電療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
007080         07 電療料５８ＷＲ                PIC 9(4)  VALUE ZERO.
007090      05 小計５８ＷＲ                     PIC 9(6)  VALUE ZERO.
007100      05 多部位込小計５８ＷＲ             PIC 9(6)  VALUE ZERO.
007110      05 長期逓減率５８ＷＲ               PIC 9(3)  VALUE ZERO.
007120      05 長期込小計５８ＷＲ               PIC 9(6)  VALUE ZERO.
007130******************
007140* ５部位／１０割 *
007150******************
007160   03 部位５０ＷＲ.
007170      05 逓減開始月日５０ＷＲ.
007180         07 逓減開始月５０ＷＲ            PIC 9(2)  VALUE ZERO.
007190         07 逓減開始日５０ＷＲ            PIC 9(2)  VALUE ZERO.
007200      05 後療５０ＷＲ.
007210         07 後療単価５０ＷＲ              PIC 9(4)  VALUE ZERO.
007220         07 後療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
007230         07 後療料５０ＷＲ                PIC 9(5)  VALUE ZERO.
007240      05 冷罨法５０ＷＲ.
007250         07 冷罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
007260         07 冷罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
007270      05 温罨法５０ＷＲ.
007280         07 温罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
007290         07 温罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
007300      05 電療５０ＷＲ.
007310         07 電療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
007320         07 電療料５０ＷＲ                PIC 9(4)  VALUE ZERO.
007330      05 小計５０ＷＲ                     PIC 9(6)  VALUE ZERO.
007340      05 長期逓減率５０ＷＲ               PIC 9(3)  VALUE ZERO.
007350      05 長期込小計５０ＷＲ               PIC 9(6)  VALUE ZERO.
008000*******************
008010*  明細書発行加算 */202206
008020*******************
008030   03 明細書発行加算料ＷＲ                PIC ZZZ   VALUE ZERO.
008030   03 明細書発行加算日ＷＲ                PIC ZZ    VALUE ZERO.
007360*
007370**************
007380* 施術所情報 *
007390**************
007400 01 施術所情報Ｗ.
007410    03 柔整師番号Ｗ                    PIC X(16)  VALUE SPACE.
007420    03 接骨師会会員番号Ｗ              PIC X(16)  VALUE SPACE.
007430    03 代表者カナＷ                    PIC X(50)  VALUE SPACE.
007440    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
007450    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
          03 都道府県ＪＩＳＷ                PIC X(2)   VALUE SPACE.
007460    03 施術所住所Ｗ.
007470       05 施術所住所１Ｗ               PIC X(50)  VALUE SPACE.
007480       05 施術所住所２Ｗ               PIC X(50)  VALUE SPACE.
007490    03 施術所郵便番号Ｗ.
007500       05 施術所郵便番号１Ｗ           PIC X(3)   VALUE SPACE.
007510       05 施術所郵便番号２Ｗ           PIC X(4)   VALUE SPACE.
007520    03 施術所電話番号Ｗ                PIC X(15)  VALUE SPACE.
007530    03 定額制受理番号Ｗ                PIC X(15)  VALUE SPACE.
007540    03 受理年月日Ｗ.
007350       05 受理和暦Ｗ                   PIC 9      VALUE ZERO.
007550       05 受理年Ｗ                     PIC 9(2)   VALUE ZERO.
007560       05 受理月Ｗ                     PIC 9(2)   VALUE ZERO.
007570       05 受理日Ｗ                     PIC 9(2)   VALUE ZERO.
007580    03 最終通院年月日Ｗ.
007390       05 最終通院和暦Ｗ               PIC 9      VALUE ZERO.
007590       05 最終通院年Ｗ                 PIC 9(2)   VALUE ZERO.
007600       05 最終通院月Ｗ                 PIC 9(2)   VALUE ZERO.
007610       05 最終通院日Ｗ                 PIC 9(2)   VALUE ZERO.
007620    03 柔整師年月日Ｗ.
007430       05 柔整師和暦Ｗ                 PIC 9      VALUE ZERO.
007630       05 柔整師年Ｗ                   PIC 9(2)   VALUE ZERO.
007640       05 柔整師月Ｗ                   PIC 9(2)   VALUE ZERO.
007650       05 柔整師日Ｗ                   PIC 9(2)   VALUE ZERO.
007660    03 患者委任年月日Ｗ.
007470       05 患者委任和暦Ｗ               PIC 9      VALUE ZERO.
007670       05 患者委任年Ｗ                 PIC 9(2)   VALUE ZERO.
007680       05 患者委任月Ｗ                 PIC 9(2)   VALUE ZERO.
007690       05 患者委任日Ｗ                 PIC 9(2)   VALUE ZERO.
007700    03 取引先情報Ｗ.
007710        05 取引先銀行名Ｗ.
007720           07 取引先銀行名１Ｗ         PIC X(10)  VALUE SPACE.
007730           07 取引先銀行名２Ｗ         PIC X(10)  VALUE SPACE.
007740           07 FILLER                   PIC X(20)  VALUE SPACE.
007750        05 取引先銀行支店名Ｗ.
007760           07 取引先銀行支店名１Ｗ     PIC X(10)  VALUE SPACE.
007770           07 取引先銀行支店名２Ｗ     PIC X(10)  VALUE SPACE.
007780           07 FILLER                   PIC X(20)  VALUE SPACE.
007790        05 預金種別Ｗ                  PIC 9(1)   VALUE ZERO.
007800        05 口座番号Ｗ                  PIC X(10)  VALUE SPACE.
007810        05 口座名義人Ｗ                PIC X(40)  VALUE SPACE.
007820        05 口座名義人カナＷ            PIC X(40)  VALUE SPACE.
007830        05 預金種別コメントＷ          PIC N(3)   VALUE SPACE.
007840        05 預金種別コメントＸＷ        PIC X(4)   VALUE SPACE.
007850*
007860    03 県施術ＩＤＷ                    PIC X(15)  VALUE SPACE.
007870    03 市町村施術ＩＤＷ                PIC X(15)  VALUE SPACE.
007880    03 共済番号Ｗ                      PIC X(28)  VALUE SPACE.
007880    03 地共済番号Ｗ                    PIC X(28)  VALUE SPACE.
007890**************
007900* 受診者情報 *
007910**************
007920 01 受診者情報Ｗ.
          03 施術和暦Ｗ                      PIC 9(1)   VALUE ZERO.
007930    03 施術年月Ｗ.
007940       05 施術年Ｗ                     PIC 9(2)   VALUE ZERO.
007950       05 施術月Ｗ                     PIC 9(2)   VALUE ZERO.
007960*    03 記号Ｗ                          PIC N(12)  VALUE SPACE.
007570    03 記号Ｗ.
007580       05 印刷記号Ｗ                   PIC N(12)  VALUE SPACE.
          03 記号番号Ｗ.
             05 記号番号ＸＷ                 PIC X(40) VALUE SPACE.
007970*    03 番号Ｗ                          PIC X(30)  VALUE SPACE.
008770    03 番号Ｗ.
008780       05 印刷番号Ｗ                   PIC X(15)  VALUE SPACE.
008790       05 FILLER                       PIC X(15)  VALUE SPACE.
007980    03 保険者番号Ｗ.
007990       05 印刷保険者番号Ｗ             PIC X(8)   VALUE SPACE.
008000       05 FILLER                       PIC X(2)   VALUE SPACE.
008010    03 市町村番号Ｗ.
008020       05 印刷市町村番号Ｗ             PIC X(8)   VALUE SPACE.
008030       05 FILLER                       PIC X(2)   VALUE SPACE.
          03 受給者番号Ｗ.
             05 印刷受給者番号Ｗ             PIC X(7)  VALUE SPACE.
             05 印刷受給者番号２Ｗ           PIC X(8)  VALUE SPACE.
008040    03 請求先名称Ｗ.
008050       05 請求先名称１Ｗ               PIC X(40)  VALUE SPACE.
008060       05 請求先名称２Ｗ               PIC X(40)  VALUE SPACE.
008070    03 保険種別Ｗ                      PIC 9(2)   VALUE ZERO.
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
008080    03 被保険者情報Ｗ.
008090       05 被保険者カナＷ               PIC X(50)  VALUE SPACE.
008100       05 被保険者氏名Ｗ               PIC X(50)  VALUE SPACE.
008110       05 郵便番号Ｗ.
008120          07 郵便番号１Ｗ              PIC X(3)   VALUE SPACE.
008130          07 郵便番号２Ｗ              PIC X(4)   VALUE SPACE.
008990       05 電話番号Ｗ                   PIC X(35)  VALUE SPACE.
008140       05 被保険者住所Ｗ.
008150          07 被保険者住所１Ｗ          PIC X(50)  VALUE SPACE.
008160          07 被保険者住所２Ｗ          PIC X(50)  VALUE SPACE.
008170    03 患者情報Ｗ.
             05 患者住所Ｗ.
008370          07 患者住所１Ｗ              PIC X(50)  VALUE SPACE.
008380          07 患者住所２Ｗ              PIC X(50)  VALUE SPACE.
008180       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
008190       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
008200       05 性別チェックＷ.
008210          07 男チェックＷ              PIC N(1)  VALUE SPACE.
008220          07 女チェックＷ              PIC N(1)  VALUE SPACE.
008230       05 患者性別Ｗ.
008240          07 性別Ｗ                    PIC N(1)  VALUE SPACE.
008250       05 和暦チェックＷ.
008260          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
008270          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
008280          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
008290          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
008300          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
      */元号修正/↓↓↓20190426
008210          07 令和チェックＷ            PIC N(1)  VALUE SPACE.
                07 令和ＣＭＷ                PIC X(4)  VALUE SPACE.
009110*          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
      */元号修正/↑↑↑20190426
008310       05 患者年Ｗ                     PIC 9(2)  VALUE ZERO.
008320       05 患者月Ｗ                     PIC 9(2)  VALUE ZERO.
008330       05 患者日Ｗ                     PIC 9(2)  VALUE ZERO.
008340       05 続柄Ｗ.
008350          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
008360          07 FILLER                    PIC X(4)  VALUE SPACE.
008370*
008380*       05 負傷原因Ｗ                   PIC X(80) OCCURS 29 VALUE SPACE.
      */半角対応/110421
             05 負傷原因Ｗ OCCURS 29.
                07 負傷原因ＸＷ              PIC X(70)  VALUE SPACE.
       01 負傷原因１文Ｗ.
          03 負傷原因１文ＷＲ                OCCURS 7.
             05 負傷原因１文ＷＰ             PIC X(70) VALUE SPACE.
008390*
      */助成印が印刷されない場合がある↓↓↓/20201006
008400*    03 保険種別名称Ｗ                  PIC N(1)  VALUE SPACE.
008410*    03 助成印Ｗ                        PIC N(1)  VALUE SPACE.
008420*    03 特別コメントＷ                  PIC X(16) VALUE SPACE.
008400 01 保険種別名称Ｗ                     PIC N(1)  VALUE SPACE.
008410 01 助成印Ｗ                           PIC N(1)  VALUE SPACE.
008420 01 特別コメントＷ                     PIC X(16) VALUE SPACE.
      */助成印が印刷されない場合がある↑↑↑/20201006
008430*
008440****************
008450* 負傷データＦ *
008460****************
008470 01 負傷情報Ｗ.
008480    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
008490    03 部位情報Ｗ  OCCURS   9.
008500       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
008510       05 部位コードＷ.
008520          07 負傷種別Ｗ                PIC 9(2)  VALUE ZERO.
008530          07 部位Ｗ                    PIC 9(2)  VALUE ZERO.
008540          07 左右区分Ｗ                PIC 9(1)  VALUE ZERO.
008550          07 負傷位置番号Ｗ            PIC 9(2)  VALUE ZERO.
008560       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
008570       05 負傷年月日Ｗ.
008580          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
008590          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
008600          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
008610       05 初検年月日Ｗ.
008620          07 初検年Ｗ                  PIC 9(2)  VALUE ZERO.
008630          07 初検月Ｗ                  PIC 9(2)  VALUE ZERO.
008640          07 初検日Ｗ                  PIC 9(2)  VALUE ZERO.
008650       05 開始年月日Ｗ.
008660          07 開始年Ｗ                  PIC 9(2)  VALUE ZERO.
008670          07 開始月Ｗ                  PIC 9(2)  VALUE ZERO.
008680          07 開始日Ｗ                  PIC 9(2)  VALUE ZERO.
008690       05 終了年月日Ｗ.
002980          07 終了和暦Ｗ                PIC 9     VALUE ZERO.
008700          07 終了年Ｗ                  PIC 9(2)  VALUE ZERO.
008710          07 終了月Ｗ                  PIC 9(2)  VALUE ZERO.
008720          07 終了日Ｗ                  PIC 9(2)  VALUE ZERO.
008730       05 実日数Ｗ                     PIC 9(2)  VALUE ZERO.
008740       05 転帰区分Ｗ                   PIC 9(1)  VALUE ZERO.
008750       05 転帰区分チェックＷ.
008760          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
008770          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
008780          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
008790       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
008800       05 部位区切Ｗ                   PIC X(1)  VALUE SPACE.
008810       05 経過略称Ｗ.
008820          07 印刷経過略称Ｗ            PIC N(6)  VALUE SPACE.
008830          07 FILLER                    PIC X(2)  VALUE SPACE.
008840    03 経過部位Ｗ                      PIC N(1)  VALUE SPACE.
008850    03 新規チェックＷ                  PIC N(1)  VALUE SPACE.
008860    03 継続チェックＷ                  PIC N(1)  VALUE SPACE.
008870*
008880************
008890* 料金情報 *
008900************
008910 01 料金情報Ｗ.
008920    03 初検加算Ｗ.
008930       05 時間外チェックＷ                PIC N(1) VALUE SPACE.
008940       05 休日チェックＷ                  PIC N(1) VALUE SPACE.
008950       05 深夜チェックＷ                  PIC N(1) VALUE SPACE.
008960    03 往療加算Ｗ.
008970       05 夜間チェックＷ                  PIC N(1) VALUE SPACE.
008980       05 難路チェックＷ                  PIC N(1) VALUE SPACE.
008990       05 暴風雨雪チェックＷ              PIC N(1) VALUE SPACE.
009000    03 金属副子チェックＷ.
009010       05 大チェックＷ                    PIC N(1) VALUE SPACE.
009020       05 中チェックＷ                    PIC N(1) VALUE SPACE.
009030       05 小チェックＷ                    PIC N(1) VALUE SPACE.
009040    03 小計Ｗ                             PIC 9(7) VALUE ZERO.
009050    03 初回処置料合計Ｗ                   PIC 9(6) VALUE ZERO.
009060    03 初回処置料チェックＷ.
009070       05 整復料チェックＷ                PIC N(1) VALUE SPACE.
009080       05 固定料チェックＷ                PIC N(1) VALUE SPACE.
009090       05 施療料チェックＷ                PIC N(1) VALUE SPACE.
      */金属副子・運動後療の変更・追加/1805
          03 金属回数Ｗ                         PIC 9(2)  VALUE ZERO.
          03 運動回数Ｗ                         PIC 9(1)  VALUE ZERO.
          03 運動料Ｗ                           PIC 9(5)  VALUE ZERO.
009100************
009110* 備考情報 *
009120************
010000 01 備考情報Ｗ.
010010    03 適用１Ｗ                        PIC N(48) VALUE SPACE.
010020    03 適用２Ｗ                        PIC X(40) VALUE SPACE.
009250*
009260    03 経過コメントＷ                  PIC N(60) VALUE SPACE.
009270*
009280*****************
009290* レセプト並び順 *
009300*****************
009310 01 順番固定Ｗ                         PIC N(1) VALUE SPACE.
009320 01 順番Ｗ                             PIC 9(4) VALUE ZERO.
009330*
003720*--- 負担給付割合用 ---*
003730 01 負担割合Ｗ                         PIC 9(2)  VALUE ZERO.
003740 01 給付割合Ｗ                         PIC 9(2)  VALUE ZERO.
      *
       01 摘要施術日Ｗ                       PIC X(100) VALUE SPACE.
       01 施術日Ｗ.
          03 施術日２Ｗ                      PIC X(1)  VALUE SPACE.
          03 施術日１Ｗ                      PIC X(1)  VALUE SPACE.
009340*******************************************************************
009350 01 印刷制御.
009360     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
009370     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
009380     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
009390     03 拡張制御Ｐ.
009400         05 端末制御Ｐ.
009410             07 移動方向Ｐ             PIC X(1) VALUE SPACE.
009420             07 移動行数Ｐ             PIC 9(3) VALUE ZERO.
009430         05 詳細制御Ｐ                 PIC X(2) VALUE SPACE.
009440     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
009450     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
009460*
009470 01 計算機西暦年Ｗ                     PIC 9(2) VALUE ZERO.
009480* 日付ＷＯＲＫ
009490 01 和暦終了年Ｗ                       PIC 9(4) VALUE ZERO.
009500 01 計算機西暦.
009510    03 計算機西暦年                    PIC 9(4) VALUE ZERO.
009520    03 計算機西暦月日                  PIC 9(4) VALUE ZERO.
009530 01 計算機西暦Ｒ REDEFINES 計算機西暦.
009540    03 計算機世紀                      PIC 9(2).
009550    03 計算機日付                      PIC 9(6).
009560    03 計算機日付Ｒ REDEFINES 計算機日付.
009570       05 計算機年月                   PIC 9(4).
009580       05 計算機年月Ｒ REDEFINES 計算機年月.
009590         07 計算機年                   PIC 9(2).
009600         07 計算機月                   PIC 9(2).
009610       05 計算機日                     PIC 9(2).
009620*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
014774*
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
009630******************************************************************
009640*                          連結項目                              *
009650******************************************************************
009660**  画面入力データ
010430*
       01 連入－プレビュー IS EXTERNAL.
          03 連入－プレビュー区分          PIC 9.
010440*
009670 01 連入－入力データ委任印刷 IS EXTERNAL.
009680    03 連入－委任印刷                     PIC 9.
       01 連入－入力データ電話印刷 IS EXTERNAL.
          03 連入－電話印刷                     PIC 9.
009690*
009700** ３カ月長期判定
009710 01 連期間－キー IS EXTERNAL.
009720    03 連期間－施術年月.
009730       05 連期間－施術和暦               PIC 9.
009740       05 連期間－施術年                 PIC 9(2).
009750       05 連期間－施術月                 PIC 9(2).
009760    03  連期間－患者コード.
009770       05 連期間－患者番号               PIC 9(6).
009780       05 連期間－枝番                   PIC X.
009790    03 連期間－対象フラグ                PIC X(3).
009800    03 連期間－期間月Ｗ.
009810       05 連期間－期間Ｗ                 PIC 9(2) OCCURS 9.
009820************
009830* 印刷キー *
009840************
009850*
009860*
009870 01 連レ印－対象データ IS EXTERNAL.
009880    03 連レ印－施術年月日.
009890       05 連レ印－施術和暦                  PIC 9(1).
009900       05 連レ印－施術年                    PIC 9(2).
009910       05 連レ印－施術月                    PIC 9(2).
009920    03 連レ印－患者コード.
009930       05 連レ印－患者番号                  PIC 9(6).
009940       05 連レ印－枝番                      PIC X(1).
009950    03 連レ印－保険種別                     PIC 9(2).
009960    03 連レ印－保険者番号                   PIC X(10).
009970    03 連レ印－公費種別                     PIC 9(2).
009980    03 連レ印－費用負担者番号               PIC X(10).
009990    03 連レ印－助成種別                     PIC 9(2).
010000    03 連レ印－費用負担者番号助成           PIC X(10).
010010    03 連レ印－患者カナ                     PIC X(20).
010020    03 連レ印－本人家族区分                 PIC 9(1).
010030*
013460 01 連レ－キー IS EXTERNAL.
013470    03 連レ－保険種別                  PIC 9(2).
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
013620*
013630************************
013640* 助成レセまとめ
013650************************
013660 01 連レセまとめ－キー IS EXTERNAL.
013670    03 連レセまとめ－施術和暦年月.
013680       05 連レセまとめ－施術和暦               PIC 9.
013690       05 連レセまとめ－施術年月.
013700          07 連レセまとめ－施術年              PIC 9(2).
013710          07 連レセまとめ－施術月              PIC 9(2).
013720    03 連レセまとめ－患者コード.
013730       05 連レセまとめ－患者番号               PIC 9(6).
013740       05 連レセまとめ－枝番                   PIC X(1).
013750**-------------------------------------------------------**
013760*   1:助成レセプトなしの本体まとめの判定
013770*   2:横浜・川崎用の社保助成レセかの判定
013780    03 連レセまとめ－判定区分                  PIC 9.
013790**-------------------------------------------------------**
013800*  / OUT /　 0:対象外、1:対象
013810    03 連レセまとめ－判定結果                  PIC 9.
013820**
013821*
013822*************
013823* 助成名称
013824*************
013825 01 連助成名称－キー IS EXTERNAL.
013826    03 連助成名称－助成種別             PIC 9(2).
013827    03 連助成名称－費用負担者番号助成   PIC X(10).
013828*   / OUT /
013829    03 連助成名称－名称集団.
013830       05 連助成名称－１文字            PIC N.
013831       05 連助成名称－略称              PIC N(4).
013832       05 連助成名称－正式名称          PIC N(10).
013833*
      * 暗号複合用
       01 連暗号複合－暗号情報 IS EXTERNAL.
          03 連暗号複合－入力情報.
             05 連暗号複合－記号               PIC X(24).
             05 連暗号複合－番号               PIC X(30).
             05 連暗号複合－暗号化項目.
                07 連暗号複合－暗号患者番号    PIC X(6).
                07 連暗号複合－暗号判定記号    PIC X.
                07 連暗号複合－暗号判定番号    PIC X.
                07 連暗号複合－暗号記号        PIC X(24).
                07 連暗号複合－暗号番号        PIC X(30).
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
013835******************************************************************
013840*                      PROCEDURE  DIVISION                       *
013850******************************************************************
013860 PROCEDURE               DIVISION.
013870************
013880*           *
013890* 初期処理   *
013900*           *
013910************
002570     PERFORM プリンタファイル作成.
013920     PERFORM 初期化.
013930     PERFORM 制御情報取得.
013940************
013950*           *
013960* 主処理     *
013970*           *
013980************
013990* 印刷
014000     PERFORM 連結項目待避.
014010     PERFORM 印刷セット.
014020     PERFORM 印刷処理.
014030************
014040*           *
014050* 終了処理   *
014060*           *
014070************
014080     PERFORM 受診者印刷区分更新.
014090     PERFORM 終了処理.
014100*     PERFORM 遅延処理.
014110     MOVE ZERO  TO PROGRAM-STATUS.
014120     EXIT PROGRAM.
014130*
014140*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YAZ6421"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分  TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
014150*================================================================*
014160 初期化 SECTION.
014170*
014180     PERFORM ファイルオープン.
014190*    /* 現在日付取得 */
014200     ACCEPT 計算機日付 FROM DATE.
014210*    /* 1980～2079年の間で設定 */
014220     IF ( 計算機年 > 80 )
014230         MOVE 19 TO 計算機世紀
014240     ELSE
014250         MOVE 20 TO 計算機世紀
014260     END-IF.
014270     PERFORM カレント元号取得.
014280     PERFORM 和暦終了年取得.
014290     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
014300*================================================================*
014310 カレント元号取得 SECTION.
014320*
014330     MOVE ZEROS TO 制－制御区分.
014340     READ 制御情報マスタ
014350     NOT INVALID KEY
014360         MOVE 制－カレント元号         TO カレント元号Ｗ
014370         MOVE 制－レセ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
014380         MOVE 制－レセ長期理由印刷区分 TO 長期理由印刷区分Ｗ
014390         MOVE 制－レセプト日付区分     TO レセプト日付区分Ｗ
014400         MOVE 制－レセプト患者日付区分 TO レセプト患者日付区分Ｗ
014401         MOVE 制－全柔ＦＰＤ区分       TO 全柔ＦＰＤ区分Ｗ
014410     END-READ.
014420*
014430*================================================================*
014440 和暦終了年取得 SECTION.
014450*
014460*     DISPLAY NC"カレント元号Ｗ"  カレント元号Ｗ UPON MSGBOX.
014470     MOVE カレント元号Ｗ TO 元－元号区分.
014480     READ 元号マスタ
014490     INVALID KEY
014500         DISPLAY NC"指定和暦が登録されていません" UPON CONS
014510         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
014520                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
014530         ACCEPT  キー入力 FROM CONS
014540         PERFORM 終了処理
014550         EXIT PROGRAM
014560     NOT INVALID KEY
014570         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
014580         MOVE 前和暦Ｗ TO 元－元号区分
014590         READ 元号マスタ
014600         INVALID KEY
014610             DISPLAY NC"指定和暦が登録されていません" UPON CONS
014620             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
014630                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
014640             ACCEPT  キー入力 FROM CONS
014650             PERFORM 終了処理
014660             EXIT PROGRAM
014670         NOT INVALID KEY
014680             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
014690         END-READ
014700     END-READ.
014710*
014720*================================================================*
014730 ファイルオープン SECTION.
014740*
014750     OPEN INPUT   保険者マスタ
014760         MOVE NC"保険者" TO ファイル名.
014770         PERFORM オープンチェック.
014780     OPEN INPUT   元号マスタ
014790         MOVE NC"元号" TO ファイル名.
014800         PERFORM オープンチェック.
014810     OPEN INPUT   名称マスタ
014820         MOVE NC"名称" TO ファイル名.
014830         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
014870     OPEN INPUT   制御情報マスタ
014880         MOVE NC"制御情報" TO ファイル名.
014890         PERFORM オープンチェック.
014900     OPEN INPUT   施術所情報マスタ
014910         MOVE NC"施情" TO ファイル名.
014920         PERFORM オープンチェック.
014960     OPEN INPUT   経過マスタ
014970         MOVE NC"経過" TO ファイル名.
014980         PERFORM オープンチェック.
014990     OPEN INPUT   施術記録Ｆ.
015000         MOVE NC"施記Ｆ" TO ファイル名.
015010         PERFORM オープンチェック.
015020     OPEN INPUT   負傷データＦ.
015030         MOVE NC"負傷" TO ファイル名.
015040         PERFORM オープンチェック.
015050     OPEN INPUT   負傷原因Ｆ.
015060         MOVE NC"負傷原因" TO ファイル名.
015070         PERFORM オープンチェック.
015080     OPEN INPUT   ＩＤ管理マスタ
015090         MOVE NC"ＩＤ" TO ファイル名.
015100         PERFORM オープンチェック.
015110     OPEN INPUT 市町村マスタ.
015120         MOVE NC"市町村" TO ファイル名.
015130         PERFORM オープンチェック.
015560     OPEN INPUT   受診者情報２Ｆ.
015570         MOVE NC"受診者情報２Ｆ" TO ファイル名.
015580         PERFORM オープンチェック.
015170     OPEN INPUT  作業ファイル４.
015170         IF ( 状態キー  NOT =  "00" )
015060            OPEN OUTPUT  作業ファイル４
                  CLOSE 作業ファイル４
015060            OPEN INPUT  作業ファイル４
               END-IF.
015200     OPEN I-O   受診者情報Ｆ.
015210         MOVE NC"受情" TO ファイル名.
015220         PERFORM オープンチェック.
015230     OPEN I-O   印刷ファイル
015240         PERFORM エラー処理Ｐ.
015410     OPEN INPUT   請求先マスタ
015420         MOVE NC"請先" TO ファイル名.
015430         PERFORM オープンチェック.
015250*================================================================*
015260 オープンチェック SECTION.
015270*
015280     IF ( 状態キー  NOT =  "00" )
015290         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
015300         DISPLAY NC"状態キー：" 状態キー         UPON CONS
015310         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
015320                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015330         ACCEPT  キー入力 FROM CONS
015340         PERFORM ファイル閉鎖
015350         EXIT PROGRAM.
015360*================================================================*
015370 制御情報取得 SECTION.
015380*
015390     MOVE ZERO TO 制－制御区分
015400     READ 制御情報マスタ
015410     NOT INVALID KEY
015420         MOVE 制－最大登録部位数 TO 最大登録数Ｗ
015430         MOVE 制－負傷連続登録   TO 負傷連続登録Ｗ
015440         MOVE 制－遅延回数       TO 遅延回数Ｗ
015450     END-READ.
015460*
015470*================================================================*
015480 遅延処理 SECTION.
015490*
015500     PERFORM VARYING 遅延ＣＮＴ FROM 1 BY 1
015510                                UNTIL 遅延ＣＮＴ > 遅延回数Ｗ
015520         MOVE SPACE TO 遅延フラグ
015530     END-PERFORM.
015540*
015550*================================================================*
015560 連結項目待避 SECTION.
015570*
015580     MOVE 連レ印－施術和暦           TO 施術和暦ＷＲ.
015590     MOVE 連レ印－施術年             TO 施術年ＷＲ.
015600     MOVE 連レ印－施術月             TO 施術月ＷＲ.
015610     MOVE 連レ印－保険種別           TO 保険種別ＷＲ.
015620     MOVE 連レ印－保険者番号         TO 保険者番号ＷＲ.
015630     MOVE 連レ印－公費種別           TO 公費種別ＷＲ.
015640     MOVE 連レ印－費用負担者番号     TO 費用負担者番号ＷＲ.
015650     MOVE 連レ印－助成種別           TO 助成種別ＷＲ.
015660     MOVE 連レ印－費用負担者番号助成 TO 費用負担者番号助成ＷＲ.
015670     MOVE 連レ印－本人家族区分       TO 本人家族区分ＷＲ.
015680     MOVE 連レ印－患者カナ           TO 患者カナＷＲ.
015690     MOVE 連レ印－患者番号           TO 患者番号ＷＲ.
015700     MOVE 連レ印－枝番               TO 枝番ＷＲ.
015710*================================================================*
015720 印刷セット SECTION.
015730*
015740     PERFORM 項目初期化.
           PERFORM 基本情報取得.
015750     PERFORM 施術所情報取得.
015760     PERFORM 請求先情報取得.
015770     PERFORM 受診者情報取得.
015780     PERFORM 負傷データ取得.
015790     PERFORM 料金情報取得.
015800     PERFORM 施術記録取得.
           PERFORM 開始日取得.
015810     PERFORM レセプト並び順取得.
015820***     PERFORM 長期判定取得.
015840     PERFORM 初検加算時刻取得.
015850     PERFORM 助成印取得.
015860     PERFORM 委任年月日取得.
           PERFORM 施術日取得.
015870*
016791*-----------------------------------------------*
016800     IF ( 負傷原因印刷区分Ｗ  NOT = 1 ) AND ( レセ負傷原因印刷区分Ｗ NOT = 1 )
016813        IF ( 負傷原因印刷区分Ｗ = 3 OR 4 )
016815           PERFORM 負傷原因印刷対象判定処理
016817        ELSE
016820           PERFORM 負傷原因取得
016821        END-IF
016830     END-IF.
016831*-----------------------------------------------*
015920*
015930     IF ( 長期理由印刷区分Ｗ  NOT = 1 )
               MOVE 長期理由印刷区分Ｗ TO 連摘文－長期区分
015980     END-IF.
015990*
016000********************
016010* 受診者情報セット *
016020********************
015190     MOVE 社保チェックＷ     TO 社保チェック.
015210     MOVE 組合チェックＷ     TO 組合チェック.
015220     MOVE 国保チェックＷ     TO 国保チェック.
           MOVE 共済チェックＷ     TO 共済チェック.
           MOVE 自チェックＷ       TO 自チェック.
           IF 自チェックＷ NOT = SPACE
               MOVE NC"自"         TO 自マーク
           END-IF.
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
037370     MOVE 施術和暦Ｗ         TO 元－元号区分.
037380     READ 元号マスタ
037390     NOT INVALID KEY
037400         MOVE 元－元号名称   TO 施術和暦
037410     END-READ.
016030     MOVE 施術年Ｗ           TO 施術年 施術年２.
016040     MOVE 施術月Ｗ           TO 施術月 施術月２.
           EVALUATE 施術和暦Ｗ
           WHEN 4
               MOVE NC"Ｈ"         TO 施術和暦２
           END-EVALUATE
016050*
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
016170*
016180     MOVE 印刷保険者番号Ｗ    TO 保険者番号.
016200     MOVE 請求先名称Ｗ        TO 保険者名称 保険者名称２.
016660*
016680     IF 市町村番号Ｗ(1:2) = "99"
016690         MOVE SPACE        TO 公費負担者番号
016700     ELSE
016720         MOVE 市町村番号Ｗ TO 公費負担者番号
016760     END-IF.
016780*
016790     IF ( 印刷受給者番号Ｗ(1:1) = "*"  ) OR
016800        ( 印刷受給者番号Ｗ(1:2) = "＊" )
016810        MOVE  SPACE                TO 受給者番号
016820     ELSE
      */受給者番号が８文字以上の場合枠を無視して印刷する/210331
               IF 印刷受給者番号２Ｗ = SPACE
016830             MOVE 印刷受給者番号Ｗ TO 受給者番号
               ELSE
                   MOVE 受給者番号Ｗ     TO 受給者番号２
               END-IF
016840     END-IF.
016250***     MOVE 被保険者カナＷ      TO 被保険者カナ.
016260     MOVE 被保険者氏名Ｗ      TO 被保険者氏名.
016300     MOVE 被保険者住所１Ｗ    TO 住所１.
016310     MOVE 被保険者住所２Ｗ    TO 住所２.
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
      *     MOVE 患者住所１Ｗ        TO 住所１.
      *     MOVE 患者住所２Ｗ        TO 住所２.
016320***     MOVE 患者カナＷ          TO 患者カナ.
016330     MOVE 患者氏名Ｗ          TO 患者氏名 患者氏名２.
016340     MOVE 男チェックＷ        TO 男チェック.
016350     MOVE 女チェックＷ        TO 女チェック.
016360*     MOVE 性別Ｗ               TO 性別.
           MOVE "1.明　2.大　3.昭　4.平　5.令"  TO 生和暦ＣＭ.
016370     MOVE 明治チェックＷ      TO 生和暦チェック１.
016380     MOVE 大正チェックＷ      TO 生和暦チェック２.
016390     MOVE 昭和チェックＷ      TO 生和暦チェック３.
016400     MOVE 平成チェックＷ      TO 生和暦チェック４.
      */元号修正↓↓↓/20190426
023070     MOVE 令和チェックＷ     TO 令和チェック.
017390*     MOVE 元号Ｗ              TO 患者和暦.
      */元号修正↑↑↑/20190426
016410*     MOVE 元号Ｗ              TO 元号.
016420     MOVE 患者年Ｗ            TO 患者年.
016430     MOVE 患者月Ｗ            TO 患者月.
016440     MOVE 患者日Ｗ            TO 患者日.
016450*     MOVE 印刷続柄Ｗ          TO 続柄.
016460     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
016470     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
016480     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
016490     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
016500     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
016500     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
016500     MOVE 負傷原因Ｗ(7)       TO 負傷原因７.
016500*     MOVE 負傷原因Ｗ(8)       TO 負傷原因８.
016510*
016520     MOVE 助成印Ｗ            TO 助成印.
016530***     MOVE 保険種別名称Ｗ      TO 保険種別.
      *
           IF 受２－助成被保険者氏名 NOT = SPACE
016940        MOVE 受２－助成被保険者氏名 TO 被保険者氏名
           END-IF.
016680*
016690********************
016700* 負傷データセット *
016710********************
016720* １部位 *
016730**********
016740     MOVE 負傷名Ｗ(1)       TO 負傷名１.
016750     MOVE 負傷年Ｗ(1)       TO 負傷年１.
016760     MOVE 負傷月Ｗ(1)       TO 負傷月１.
016770     MOVE 負傷日Ｗ(1)       TO 負傷日１.
016780     MOVE 初検年Ｗ(1)       TO 初検年１.
016790     MOVE 初検月Ｗ(1)       TO 初検月１.
016800     MOVE 初検日Ｗ(1)       TO 初検日１.
016810     MOVE 開始年Ｗ(1)       TO 開始年１.
016820     MOVE 開始月Ｗ(1)       TO 開始月１.
016830     MOVE 開始日Ｗ(1)       TO 開始日１.
016840     MOVE 終了年Ｗ(1)       TO 終了年１.
016850     MOVE 終了月Ｗ(1)       TO 終了月１.
016860     MOVE 終了日Ｗ(1)       TO 終了日１.
016870     MOVE 実日数Ｗ(1)       TO 実日数１.
016880     MOVE 治癒チェックＷ(1) TO 治癒チェック１.
016890     MOVE 中止チェックＷ(1) TO 中止チェック１.
016900     MOVE 転医チェックＷ(1) TO 転医チェック１.
016910**********
016920* ２部位 *
016930**********
016940     MOVE 負傷名Ｗ(2)       TO 負傷名２.
016950     MOVE 負傷年Ｗ(2)       TO 負傷年２.
016960     MOVE 負傷月Ｗ(2)       TO 負傷月２.
016970     MOVE 負傷日Ｗ(2)       TO 負傷日２.
016980     MOVE 初検年Ｗ(2)       TO 初検年２.
016990     MOVE 初検月Ｗ(2)       TO 初検月２.
017000     MOVE 初検日Ｗ(2)       TO 初検日２.
017010     MOVE 開始年Ｗ(2)       TO 開始年２.
017020     MOVE 開始月Ｗ(2)       TO 開始月２.
017030     MOVE 開始日Ｗ(2)       TO 開始日２.
017040     MOVE 終了年Ｗ(2)       TO 終了年２.
017050     MOVE 終了月Ｗ(2)       TO 終了月２.
017060     MOVE 終了日Ｗ(2)       TO 終了日２.
017070     MOVE 実日数Ｗ(2)       TO 実日数２.
017080     MOVE 治癒チェックＷ(2) TO 治癒チェック２.
017090     MOVE 中止チェックＷ(2) TO 中止チェック２.
017100     MOVE 転医チェックＷ(2) TO 転医チェック２.
017110**********
017120* ３部位 *
017130**********
017140     MOVE 負傷名Ｗ(3)       TO 負傷名３.
017150     MOVE 負傷年Ｗ(3)       TO 負傷年３.
017160     MOVE 負傷月Ｗ(3)       TO 負傷月３.
017170     MOVE 負傷日Ｗ(3)       TO 負傷日３.
017180     MOVE 初検年Ｗ(3)       TO 初検年３.
017190     MOVE 初検月Ｗ(3)       TO 初検月３.
017200     MOVE 初検日Ｗ(3)       TO 初検日３.
017210     MOVE 開始年Ｗ(3)       TO 開始年３.
017220     MOVE 開始月Ｗ(3)       TO 開始月３.
017230     MOVE 開始日Ｗ(3)       TO 開始日３.
017240     MOVE 終了年Ｗ(3)       TO 終了年３.
017250     MOVE 終了月Ｗ(3)       TO 終了月３.
017260     MOVE 終了日Ｗ(3)       TO 終了日３.
017270     MOVE 実日数Ｗ(3)       TO 実日数３.
017280     MOVE 治癒チェックＷ(3) TO 治癒チェック３.
017290     MOVE 中止チェックＷ(3) TO 中止チェック３.
017300     MOVE 転医チェックＷ(3) TO 転医チェック３.
017310**********
017320* ４部位 *
017330**********
017340     MOVE 負傷名Ｗ(4)       TO 負傷名４.
017350     MOVE 負傷年Ｗ(4)       TO 負傷年４.
017360     MOVE 負傷月Ｗ(4)       TO 負傷月４.
017370     MOVE 負傷日Ｗ(4)       TO 負傷日４.
017380     MOVE 初検年Ｗ(4)       TO 初検年４.
017390     MOVE 初検月Ｗ(4)       TO 初検月４.
017400     MOVE 初検日Ｗ(4)       TO 初検日４.
017410     MOVE 開始年Ｗ(4)       TO 開始年４.
017420     MOVE 開始月Ｗ(4)       TO 開始月４.
017430     MOVE 開始日Ｗ(4)       TO 開始日４.
017440     MOVE 終了年Ｗ(4)       TO 終了年４.
017450     MOVE 終了月Ｗ(4)       TO 終了月４.
017460     MOVE 終了日Ｗ(4)       TO 終了日４.
017470     MOVE 実日数Ｗ(4)       TO 実日数４.
017480     MOVE 治癒チェックＷ(4) TO 治癒チェック４.
017490     MOVE 中止チェックＷ(4) TO 中止チェック４.
017500     MOVE 転医チェックＷ(4) TO 転医チェック４.
017510**********
017520* ５部位 *
017530**********
017540     MOVE 負傷名Ｗ(5)       TO 負傷名５.
017550     MOVE 負傷年Ｗ(5)       TO 負傷年５.
017560     MOVE 負傷月Ｗ(5)       TO 負傷月５.
017570     MOVE 負傷日Ｗ(5)       TO 負傷日５.
017580     MOVE 初検年Ｗ(5)       TO 初検年５.
017590     MOVE 初検月Ｗ(5)       TO 初検月５.
017600     MOVE 初検日Ｗ(5)       TO 初検日５.
017610     MOVE 開始年Ｗ(5)       TO 開始年５.
017620     MOVE 開始月Ｗ(5)       TO 開始月５.
017630     MOVE 開始日Ｗ(5)       TO 開始日５.
017640     MOVE 終了年Ｗ(5)       TO 終了年５.
017650     MOVE 終了月Ｗ(5)       TO 終了月５.
017660     MOVE 終了日Ｗ(5)       TO 終了日５.
017670     MOVE 実日数Ｗ(5)       TO 実日数５.
017680     MOVE 治癒チェックＷ(5) TO 治癒チェック５.
017690     MOVE 中止チェックＷ(5) TO 中止チェック５.
017700     MOVE 転医チェックＷ(5) TO 転医チェック５.
017710**************
017720* 経過セット *
017730**************
017740     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL ( 部位ＣＮＴ > 5 )
017750***             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
017760***         MOVE 部位ＣＮＴＷ(部位ＣＮＴ)   TO 経過部位ＣＮＴ(部位ＣＮＴ)
017770***         MOVE 部位区切Ｗ(部位ＣＮＴ)     TO 部位区切(部位ＣＮＴ)
017780         MOVE 印刷経過略称Ｗ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
017790     END-PERFORM.
017800*****************************************
017810*     新規・継続チェックについて        *
017820*   ●新規...初検有り ●継続...初検なし *
017830*****************************************
017840     MOVE 新規チェックＷ    TO 新規チェック.
017850     MOVE 継続チェックＷ    TO 継続チェック.
017860********************
017870* 料金データセット *
017880********************
017890*    ****************************************************************
017900*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
017910*    ****************************************************************
017920     MOVE 初検料ＷＲ                   TO  初検料.
017930     MOVE 時間外チェックＷ             TO  時間外チェック.
017940     MOVE 休日チェックＷ               TO  休日チェック.
017950     MOVE 深夜チェックＷ               TO  深夜チェック.
017960     MOVE 初検加算料ＷＲ               TO  初検加算料.
      *
           IF (時間外チェックＷ NOT = SPACE) OR (深夜チェックＷ NOT = SPACE) OR
              (休日チェックＷ NOT = SPACE)
              MOVE 初検加算時Ｗ                 TO  初検加算時
              MOVE 初検加算区切Ｗ               TO  初検加算区切
              MOVE 初検加算分Ｗ                 TO  初検加算分
           END-IF.
      *
017970     MOVE 再検料ＷＲ                   TO  再検料.
017980     MOVE 往療距離ＷＲ                 TO  往療距離.
017990     MOVE 往療回数ＷＲ                 TO  往療回数.
018000     MOVE 往療料ＷＲ                   TO  往療料.
018010     MOVE 夜間チェックＷ               TO  夜間チェック.
018020     MOVE 難路チェックＷ               TO  難路チェック.
018030     MOVE 暴風雨雪チェックＷ           TO  暴風雨雪チェック.
018040     MOVE 往療加算料ＷＲ               TO  往療加算料.
      */金属副子・運動後療の変更・追加/1805
           MOVE 金属回数Ｗ                   TO  金属回数.
018160     MOVE 金属副子加算料ＷＲ           TO  金属副子加算料.
           MOVE 運動回数Ｗ                   TO  運動回数.
           MOVE 運動料Ｗ                     TO  運動後療料.
018090     MOVE 施術情報提供料ＷＲ           TO  施術情報提供料.
018100     MOVE 小計Ｗ                       TO 小計.
           MOVE 相談料ＷＲ                   TO 初検時相談料.
018110********************
018120* 初回処置料セット *
018130********************
018140     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL ( 部位ＣＮＴ > 5 )
018150***             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
018160         MOVE 初回処置料ＷＲ(部位ＣＮＴ) TO 初回処置料(部位ＣＮＴ)
018170     END-PERFORM.
018180     MOVE 初回処置料合計Ｗ         TO 初回処置料合計
018190*
018200     MOVE 施療料チェックＷ            TO 施療料チェック.
018210     MOVE 整復料チェックＷ            TO 整復料チェック.
018220     MOVE 固定料チェックＷ            TO 固定料チェック.
018230********************
018240* 逓減毎料金セット *
018250********************
018260*    **********
018270*    * １部位 *
018280*    **********
018290     MOVE 後療単価１ＷＲ             TO 後療単価１.
018300     MOVE 後療回数１ＷＲ             TO 後療回数１.
018310     MOVE 後療料１ＷＲ               TO 後療料１.
018320     MOVE 冷罨法回数１ＷＲ           TO 冷罨法回数１.
018330     MOVE 冷罨法料１ＷＲ             TO 冷罨法料１.
018340     MOVE 温罨法回数１ＷＲ           TO 温罨法回数１.
018350     MOVE 温罨法料１ＷＲ             TO 温罨法料１.
018360     MOVE 電療回数１ＷＲ             TO 電療回数１.
018370     MOVE 電療料１ＷＲ               TO 電療料１.
018380     MOVE 小計１ＷＲ                 TO 小計１.
018390     IF ( 長期逓減率１ＷＲ NOT = ZERO )
018400         COMPUTE 長期逓減率１ = 長期逓減率１ＷＲ / 100
018410     END-IF.
018420     MOVE 長期込小計１ＷＲ           TO 長期込小計１.
018430*    **********
018440*    * ２部位 *
018450*    **********
018460     MOVE 後療単価２ＷＲ             TO 後療単価２.
018470     MOVE 後療回数２ＷＲ             TO 後療回数２.
018480     MOVE 後療料２ＷＲ               TO 後療料２.
018490     MOVE 冷罨法回数２ＷＲ           TO 冷罨法回数２.
018500     MOVE 冷罨法料２ＷＲ             TO 冷罨法料２.
018510     MOVE 温罨法回数２ＷＲ           TO 温罨法回数２.
018520     MOVE 温罨法料２ＷＲ             TO 温罨法料２.
018530     MOVE 電療回数２ＷＲ             TO 電療回数２.
018540     MOVE 電療料２ＷＲ               TO 電療料２.
018550     MOVE 小計２ＷＲ                 TO 小計２.
018560     IF ( 長期逓減率２ＷＲ NOT = ZERO )
018570         COMPUTE 長期逓減率２ = 長期逓減率２ＷＲ / 100
018580     END-IF.
018590     MOVE 長期込小計２ＷＲ           TO 長期込小計２.
018600*    ****************
018610*    * ３部位／８割 *
018620*    ****************
018630     IF ( 長期込小計３８ＷＲ NOT = ZERO )
018670     MOVE 後療単価３８ＷＲ             TO 後療単価３８.
018680     MOVE 後療回数３８ＷＲ             TO 後療回数３８.
018690     MOVE 後療料３８ＷＲ               TO 後療料３８.
018700     MOVE 冷罨法回数３８ＷＲ           TO 冷罨法回数３８.
018710     MOVE 冷罨法料３８ＷＲ             TO 冷罨法料３８.
018720     MOVE 温罨法回数３８ＷＲ           TO 温罨法回数３８.
018730     MOVE 温罨法料３８ＷＲ             TO 温罨法料３８.
018740     MOVE 電療回数３８ＷＲ             TO 電療回数３８.
018750     MOVE 電療料３８ＷＲ               TO 電療料３８.
018760     MOVE 小計３８ＷＲ                 TO 小計３８.
018770     MOVE 多部位込小計３８ＷＲ         TO 多部位込小計３８.
018780     IF ( 長期逓減率３８ＷＲ NOT = ZERO )
018790         COMPUTE 長期逓減率３８ = 長期逓減率３８ＷＲ / 100
018800     END-IF.
018810     MOVE 長期込小計３８ＷＲ           TO 長期込小計３８.
      */25年06月より新用紙に強制切り替えの為訂正をしない/130614
      **/ 逓減率 0.7→0.6 /42505
      *     IF (施術和暦年月ＷＲ >= 42505)
      *        MOVE "60"                      TO 逓減３８
      *        MOVE "0.6"                     TO 多部位３８
      *        MOVE "==="                     TO 逓減訂正３８ 多部位訂正３８
      *     END-IF.
018820*    ****************
018830*    * ３部位／10割 *
018840*    ****************
018880     MOVE 逓減開始月３０ＷＲ           TO 逓減開始月３０.
018890     MOVE 逓減開始日３０ＷＲ           TO 逓減開始日３０.
018900     MOVE 後療単価３０ＷＲ             TO 後療単価３０.
018910     MOVE 後療回数３０ＷＲ             TO 後療回数３０.
018920     MOVE 後療料３０ＷＲ               TO 後療料３０.
018930     MOVE 冷罨法回数３０ＷＲ           TO 冷罨法回数３０.
018940     MOVE 冷罨法料３０ＷＲ             TO 冷罨法料３０.
018950     MOVE 温罨法回数３０ＷＲ           TO 温罨法回数３０.
018960     MOVE 温罨法料３０ＷＲ             TO 温罨法料３０.
018970     MOVE 電療回数３０ＷＲ             TO 電療回数３０.
018980     MOVE 電療料３０ＷＲ               TO 電療料３０.
018990     MOVE 小計３０ＷＲ                 TO 小計３０.
019000     IF ( 長期逓減率３０ＷＲ NOT = ZERO )
019010         COMPUTE 長期逓減率３０ = 長期逓減率３０ＷＲ / 100
019020     END-IF.
019030     MOVE 長期込小計３０ＷＲ           TO 長期込小計３０.
019040*    ****************
019050*    * ４部位／５割 *
019060*    ****************
019070*     IF ( 長期込小計４５ＷＲ NOT = ZERO )
019080*        MOVE "33"                      TO 逓減４５
019090*        MOVE "0.33"                    TO 逓減４５少数
019100*     END-IF.
019110*     MOVE 後療単価４５ＷＲ             TO 後療単価４５.
019120*     MOVE 後療回数４５ＷＲ             TO 後療回数４５.
019130*     MOVE 後療料４５ＷＲ               TO 後療料４５.
019140*     MOVE 冷罨法回数４５ＷＲ           TO 冷罨法回数４５.
019150*     MOVE 冷罨法料４５ＷＲ             TO 冷罨法料４５.
019160*     MOVE 温罨法回数４５ＷＲ           TO 温罨法回数４５.
019170*     MOVE 温罨法料４５ＷＲ             TO 温罨法料４５.
019180*     MOVE 電療回数４５ＷＲ             TO 電療回数４５.
019190*     MOVE 電療料４５ＷＲ               TO 電療料４５.
019200*     MOVE 小計４５ＷＲ                 TO 小計４５.
019210*     MOVE 多部位込小計４５ＷＲ         TO 多部位込小計４５.
019220*     IF ( 長期逓減率４５ＷＲ NOT = ZERO )
019230*         COMPUTE 長期逓減率４５ = 長期逓減率４５ＷＲ / 100
019240*     END-IF.
019250*     MOVE 長期込小計４５ＷＲ           TO 長期込小計４５.
019260*    ****************
019270*    * ４部位／８割 *
019280*    ****************
019290     IF ( 長期込小計４８ＷＲ NOT = ZERO )
019330     MOVE 逓減開始月４８ＷＲ           TO 逓減開始月４８.
019340     MOVE 逓減開始日４８ＷＲ           TO 逓減開始日４８.
019350     MOVE 後療単価４８ＷＲ             TO 後療単価４８.
019360     MOVE 後療回数４８ＷＲ             TO 後療回数４８.
019370     MOVE 後療料４８ＷＲ               TO 後療料４８.
019380     MOVE 冷罨法回数４８ＷＲ           TO 冷罨法回数４８.
019390     MOVE 冷罨法料４８ＷＲ             TO 冷罨法料４８.
019400     MOVE 温罨法回数４８ＷＲ           TO 温罨法回数４８.
019410     MOVE 温罨法料４８ＷＲ             TO 温罨法料４８.
019420     MOVE 電療回数４８ＷＲ             TO 電療回数４８.
019430     MOVE 電療料４８ＷＲ               TO 電療料４８.
019440     MOVE 小計４８ＷＲ                 TO 小計４８.
019450     MOVE 多部位込小計４８ＷＲ         TO 多部位込小計４８.
019460     IF ( 長期逓減率４８ＷＲ NOT = ZERO )
019470         COMPUTE 長期逓減率４８ = 長期逓減率４８ＷＲ / 100
019480     END-IF.
019490     MOVE 長期込小計４８ＷＲ           TO 長期込小計４８.
      */25年06月より新用紙に強制切り替えの為訂正をしない/130614
      **/ 逓減率 0.7→0.6 /42505
      *     IF (施術和暦年月ＷＲ >= 42505)
      *        MOVE "60"                      TO 逓減４８
      *        MOVE "0.6"                     TO 多部位４８
      *        MOVE "==="                     TO 逓減訂正４８ 多部位訂正４８
      *     END-IF.
019500*    ****************
019510*    * ４部位／10割 *
019520*    ****************
019560     MOVE 逓減開始月４０ＷＲ           TO 逓減開始月４０.
019570     MOVE 逓減開始日４０ＷＲ           TO 逓減開始日４０.
019580     MOVE 後療単価４０ＷＲ             TO 後療単価４０.
019590     MOVE 後療回数４０ＷＲ             TO 後療回数４０.
019600     MOVE 後療料４０ＷＲ               TO 後療料４０.
019610     MOVE 冷罨法回数４０ＷＲ           TO 冷罨法回数４０.
019620     MOVE 冷罨法料４０ＷＲ             TO 冷罨法料４０.
019630     MOVE 温罨法回数４０ＷＲ           TO 温罨法回数４０.
019640     MOVE 温罨法料４０ＷＲ             TO 温罨法料４０.
019650     MOVE 電療回数４０ＷＲ             TO 電療回数４０.
019660     MOVE 電療料４０ＷＲ               TO 電療料４０.
019670     MOVE 小計４０ＷＲ                 TO 小計４０.
019680     IF ( 長期逓減率４０ＷＲ NOT = ZERO )
019690         COMPUTE 長期逓減率４０ = 長期逓減率４０ＷＲ / 100
019700     END-IF.
019710     MOVE 長期込小計４０ＷＲ           TO 長期込小計４０.
019720*
019730*↓***********************************************************************
019740* ５部位／2.5割の印字は必要ない。
019750*------------------------------------------------------------------------*
019760*    *****************
019770*    * ５部位／2.5割 *
019780*    *****************
019790*     MOVE 後療単価５２ＷＲ             TO 後療単価５２.
019800*     MOVE 後療回数５２ＷＲ             TO 後療回数５２.
019810*     MOVE 後療料５２ＷＲ               TO 後療料５２.
019820*     MOVE 冷罨法回数５２ＷＲ           TO 冷罨法回数５２.
019830*     MOVE 冷罨法料５２ＷＲ             TO 冷罨法料５２.
019840*     MOVE 温罨法回数５２ＷＲ           TO 温罨法回数５２.
019850*     MOVE 温罨法料５２ＷＲ             TO 温罨法料５２.
019860*     MOVE 電療回数５２ＷＲ             TO 電療回数５２.
019870*     MOVE 電療料５２ＷＲ               TO 電療料５２.
019880*     MOVE 小計５２ＷＲ                 TO 小計５２.
019890*     MOVE 多部位込小計５２ＷＲ         TO 多部位込小計５２.
019900*     IF ( 長期逓減率５２ＷＲ NOT = ZERO )
019910*         COMPUTE 長期逓減率５２ = 長期逓減率５２ＷＲ / 100
019920*     END-IF.
019930*     MOVE 長期込小計５２ＷＲ           TO 長期込小計５２.
019940*↑***********************************************************************
019950*
019960*    ****************
019970*    * ５部位／５割 *
019980*    ****************
019990*     IF ( 長期込小計５５ＷＲ NOT = ZERO )
020000*        MOVE "33"                      TO 逓減５５
020010*        MOVE "0.33"                    TO 逓減５５少数
020020*     END-IF.
020030*     MOVE 逓減開始月５５ＷＲ           TO 逓減開始月５５.
020040*     MOVE 逓減開始日５５ＷＲ           TO 逓減開始日５５.
020050*     MOVE 後療単価５５ＷＲ             TO 後療単価５５.
020060*     MOVE 後療回数５５ＷＲ             TO 後療回数５５.
020070*     MOVE 後療料５５ＷＲ               TO 後療料５５.
020080*     MOVE 冷罨法回数５５ＷＲ           TO 冷罨法回数５５.
020090*     MOVE 冷罨法料５５ＷＲ             TO 冷罨法料５５.
020100*     MOVE 温罨法回数５５ＷＲ           TO 温罨法回数５５.
020110*     MOVE 温罨法料５５ＷＲ             TO 温罨法料５５.
020120*     MOVE 電療回数５５ＷＲ             TO 電療回数５５.
020130*     MOVE 電療料５５ＷＲ               TO 電療料５５.
020140*     MOVE 小計５５ＷＲ                 TO 小計５５.
020150*     MOVE 多部位込小計５５ＷＲ         TO 多部位込小計５５.
020160*     IF ( 長期逓減率５５ＷＲ NOT = ZERO )
020170*         COMPUTE 長期逓減率５５ = 長期逓減率５５ＷＲ / 100
020180*     END-IF.
020190*     MOVE 長期込小計５５ＷＲ           TO 長期込小計５５.
020200*    ****************
020210*    * ５部位／８割 *
020220*    ****************
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
021720        MOVE 部位５Ｗ                     TO 部位５８２
021490     END-IF.
020440*    ****************
020450*    * ５部位／10割 *
020460*    ****************
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
021720        MOVE 部位５Ｗ                     TO 部位５０２
021730     END-IF.
020690*
021750     MOVE 適用１Ｗ                       TO 適用１.
021760     MOVE 適用２Ｗ                       TO 適用２.
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
              MOVE 36           TO 連金運－会コード
              MOVE 1            TO 連金運－用紙種別
              CALL "KINUNRYO"
              CANCEL "KINUNRYO"
              MOVE 連金運－金属副子ＣＭ           TO 金属副子ＣＭ
              IF ( 金属副子加算料ＷＲ NOT = ZERO )
                 MOVE 金属副子ＣＭ                TO 金属副子
              END-IF
              PERFORM VARYING カウンタ FROM 1 BY 1
                        UNTIL カウンタ > 3
                 MOVE 連金運－金属副子日(1 カウンタ) TO 金属日(カウンタ)
              END-PERFORM
              PERFORM VARYING カウンタ FROM 1 BY 1
                        UNTIL カウンタ > 5
                 MOVE 連金運－運動日(カウンタ)     TO 運動日(カウンタ)
              END-PERFORM
           END-IF.
      *
020740     MOVE レセ－合計                     TO 合計.
020750     MOVE レセ－一部負担金               TO 一部負担金.
020760     MOVE レセ－請求金額                 TO 請求金額.
021370     MOVE レセ－受給者負担額             TO 受給者負担額.
021380     MOVE レセ－助成請求金額             TO 助成請求額.
020770*
020780**------------------------------------------------------------------------------------*
020790** 特別（助成レセなしで、本体レセにまとめる時、金額は助成込み・適用２に助成種別印字）
020800*     IF ( 助成レセまとめフラグ = "YES" )
020810*         PERFORM 助成料金計算
020820*         MOVE レセ－合計                 TO 合計
020830*         MOVE レセ－受給者負担額         TO 一部負担金
020840*     / 引き算する/
020850*         COMPUTE 請求金額 = レセ－合計 - レセ－受給者負担額
020860**
020870**/深＿夜の空白にストリングしてしまうためNOT SPACEの時は最後に転記する。
021910**/初険加算が３回の時は余白無く転記される。
021920*         IF 助成種別略称Ｗ NOT = SPACE
021930*            IF 適用２Ｗ NOT = SPACE
021940*                MOVE SPACE TO 助成種別略称Ｗ２
021950*                STRING NC"※"             DELIMITED BY SIZE
021960*                       助成種別略称Ｗ     DELIMITED BY SPACE
021970*                       INTO 助成種別略称Ｗ２
021980*                END-STRING
021990*                MOVE 助成種別略称Ｗ２ TO 適用２(35:4)
022000*            ELSE
022010*                STRING 適用２Ｗ           DELIMITED BY SPACE
022020*                       NC"※"             DELIMITED BY SIZE
022030*                       助成種別略称Ｗ     DELIMITED BY SPACE
022040*                       INTO 適用２
022050*                END-STRING
022060*            END-IF
022070*         END-IF
021050*     END-IF.
022410*------------------------------------------------------------------------*
022420* 長期頻回の時、摘要欄に内容を記載
      *
           MOVE SPACE                     TO 長期頻回Ｗ.
      *     IF (レセ－部位継続月数(1) > 5) OR (レセ－部位継続月数(2) > 5) OR
      *        (レセ－部位継続月数(3) > 5) OR (レセ－部位継続月数(4) > 5) OR
      *        (レセ－部位継続月数(5) > 5)
      *        MOVE "長期頻回該当："       TO 長期頻回ＣＭ
      *     END-IF.
           IF (レセ－部位継続月数(1) >= 1) OR (レセ－部位継続月数(2) >= 1) OR
              (レセ－部位継続月数(3) >= 1) OR (レセ－部位継続月数(4) >= 1) OR
              (レセ－部位継続月数(5) >= 1)
              MOVE "長期頻回該当："       TO 長期頻回ＣＭ
           END-IF.
           MOVE SPACE                     TO 長期頻回ＣＭ２.
      *     IF (レセ－部位継続月数(1) > 5)
      *        MOVE "長期頻回該当："       TO 長期頻回ＣＭ２
      *     END-IF.
           IF (レセ－部位継続月数(1) > 0)
              MOVE レセ－部位継続月数(1)  TO 月数Ｗ
              MOVE 負傷名Ｗ(1)            TO 負傷名ＷＲ(1)
              STRING 長期頻回ＣＭ２       DELIMITED BY SPACE
                     "(1)"                DELIMITED BY SIZE
                     負傷名ＷＰ(1)        DELIMITED BY "　"
                     "、継続月数"         DELIMITED BY SIZE
                     月数Ｗ               DELIMITED BY SIZE
                     "月"                 DELIMITED BY SIZE
                INTO 長期頻回１ＷＴ
              END-STRING
           END-IF.
           MOVE SPACE                     TO 長期頻回ＣＭ２.
      *     IF (レセ－部位継続月数(2) > 5)
      *        MOVE "長期頻回該当："       TO 長期頻回ＣＭ２
      *     END-IF.
           IF (レセ－部位継続月数(2) > 0)
              MOVE レセ－部位継続月数(2)  TO 月数Ｗ
              MOVE 負傷名Ｗ(2)            TO 負傷名ＷＲ(2)
              STRING 長期頻回ＣＭ２       DELIMITED BY SPACE
                     "(2)"                DELIMITED BY SIZE
                     負傷名ＷＰ(2)        DELIMITED BY "　"
                     "、継続月数"         DELIMITED BY SIZE
                     月数Ｗ               DELIMITED BY SIZE
                     "月"                 DELIMITED BY SIZE
                INTO 長期頻回２ＷＴ
              END-STRING
           END-IF.
           MOVE SPACE                     TO 長期頻回ＣＭ２.
      *     IF (レセ－部位継続月数(3) > 5)
      *        MOVE "長期頻回該当："       TO 長期頻回ＣＭ２
      *     END-IF.
           IF (レセ－部位継続月数(3) > 0)
              MOVE レセ－部位継続月数(3)  TO 月数Ｗ
              MOVE 負傷名Ｗ(3)            TO 負傷名ＷＲ(3)
              STRING 長期頻回ＣＭ２       DELIMITED BY SPACE
                     "(3)"                DELIMITED BY SIZE
                     負傷名ＷＰ(3)        DELIMITED BY "　"
                     "、継続月数"         DELIMITED BY SIZE
                     月数Ｗ               DELIMITED BY SIZE
                     "月"                 DELIMITED BY SIZE
                INTO 長期頻回３ＷＴ
              END-STRING
           END-IF.
           MOVE SPACE                     TO 長期頻回ＣＭ２.
      *     IF (レセ－部位継続月数(4) > 5)
      *        MOVE "長期頻回該当："       TO 長期頻回ＣＭ２
      *     END-IF.
           IF (レセ－部位継続月数(4) > 0)
              MOVE レセ－部位継続月数(4)  TO 月数Ｗ
              MOVE 負傷名Ｗ(4)            TO 負傷名ＷＲ(4)
              STRING 長期頻回ＣＭ２       DELIMITED BY SPACE
                     "(4)"                DELIMITED BY SIZE
                     負傷名ＷＰ(4)        DELIMITED BY "　"
                     "、継続月数"         DELIMITED BY SIZE
                     月数Ｗ               DELIMITED BY SIZE
                     "月"                 DELIMITED BY SIZE
                INTO 長期頻回４ＷＴ
              END-STRING
           END-IF.
           MOVE SPACE                     TO 長期頻回ＣＭ２.
      *     IF (レセ－部位継続月数(5) > 5)
      *        MOVE "長期頻回該当："       TO 長期頻回ＣＭ２
      *     END-IF.
           IF (レセ－部位継続月数(5) > 0)
              MOVE レセ－部位継続月数(5)  TO 月数Ｗ
              MOVE 負傷名Ｗ(5)            TO 負傷名ＷＲ(5)
              STRING 長期頻回ＣＭ２       DELIMITED BY SPACE
                     "(5)"                DELIMITED BY SIZE
                     負傷名ＷＰ(5)        DELIMITED BY "　"
                     "、継続月数"         DELIMITED BY SIZE
                     月数Ｗ               DELIMITED BY SIZE
                     "月"                 DELIMITED BY SIZE
                INTO 長期頻回５ＷＴ
              END-STRING
           END-IF.
           MOVE 長期頻回ＣＭ   TO 文字１Ｗ.
           MOVE 長期頻回１ＷＴ TO 文字２Ｗ.
           CALL プログラム名Ｗ WITH C LINKAGE
                         USING BY REFERENCE 文字１Ｗ
                               BY REFERENCE 文字２Ｗ.
           MOVE 長期頻回２ＷＴ TO 文字２Ｗ.
           CALL プログラム名Ｗ WITH C LINKAGE
                         USING BY REFERENCE 文字１Ｗ
                               BY REFERENCE 文字２Ｗ.
           MOVE 長期頻回３ＷＴ TO 文字２Ｗ.
           CALL プログラム名Ｗ WITH C LINKAGE
                         USING BY REFERENCE 文字１Ｗ
                               BY REFERENCE 文字２Ｗ.
           MOVE 長期頻回４ＷＴ TO 文字２Ｗ.
           CALL プログラム名Ｗ WITH C LINKAGE
                         USING BY REFERENCE 文字１Ｗ
                               BY REFERENCE 文字２Ｗ.
           MOVE 長期頻回５ＷＴ TO 文字２Ｗ.
           CALL プログラム名Ｗ WITH C LINKAGE
                         USING BY REFERENCE 文字１Ｗ
                               BY REFERENCE 文字２Ｗ.
           MOVE 文字１Ｗ       TO 長期頻回.
      *
021060**------------------------------------------------------------------------------------*
021087*
021088**********************
021090* 施術所データセット *
021100**********************
           MOVE 都道府県ＪＩＳＷ       TO 都道府県番号.
021110     MOVE 柔整師番号Ｗ           TO 柔整師番号.
021120     MOVE 接骨師会会員番号Ｗ     TO 接骨師会会員番号.
021130***     MOVE 定額制受理番号Ｗ       TO 定額制受理番号.
021140     MOVE 施術所郵便番号１Ｗ     TO 施術所郵便番号１.
021150     MOVE 施術所郵便番号２Ｗ     TO 施術所郵便番号２.
021160***     MOVE 施術所住所Ｗ           TO 施術所住所１.
021170     MOVE 施術所住所１Ｗ         TO 施術所住所１.
021180     MOVE 施術所住所２Ｗ         TO 施術所住所２.
021190     MOVE 代表者カナＷ           TO 代表者カナ.
021200     MOVE 代表者名Ｗ             TO 代表者名.
021210     MOVE 施術所電話番号Ｗ       TO 施術所電話番号.
021220*
021230     MOVE 接骨院名Ｗ             TO 接骨院名.
021240*
021250*     IF ( 取引先銀行名２Ｗ = SPACE )
021260*        MOVE SPACE               TO 銀行名１
021270*        MOVE 取引先銀行名１Ｗ    TO 銀行名２
021280*     ELSE
021290*        MOVE 取引先銀行名１Ｗ    TO 銀行名１
021300*        MOVE 取引先銀行名２Ｗ    TO 銀行名２
021310*     END-IF.
021320*     IF ( 取引先銀行支店名２Ｗ = SPACE )
021330*        MOVE SPACE                TO 銀行支店名１
021340*        MOVE 取引先銀行支店名１Ｗ TO 銀行支店名２
021350*     ELSE
021360*        MOVE 取引先銀行支店名１Ｗ TO 銀行支店名１
021370*        MOVE 取引先銀行支店名２Ｗ TO 銀行支店名２
021380*     END-IF.
           MOVE 取引先銀行名１Ｗ       TO 銀行名１.
           MOVE 取引先銀行名２Ｗ       TO 銀行名２.
           MOVE 取引先銀行支店名１Ｗ   TO 銀行支店名１.
           MOVE 取引先銀行支店名２Ｗ   TO 銀行支店名２.
021390***     MOVE 預金種別コメントＷ     TO 預金種別.
021400     MOVE 口座番号Ｗ             TO 口座番号.
021410***     MOVE 口座名義人カナＷ       TO 口座名義人カナ.
021420***     MOVE 口座名義人Ｗ           TO 口座名義人.
021430*
021440* / 柔整師・患者委任日 /
037370     MOVE 柔整師和暦Ｗ           TO 元－元号区分.
037380     READ 元号マスタ
037390     NOT INVALID KEY
037400         MOVE 元－元号名称       TO 受理和暦
037410     END-READ.
021450     MOVE 柔整師年Ｗ             TO 受理年.
021460     MOVE 柔整師月Ｗ             TO 受理月.
021470     MOVE 柔整師日Ｗ             TO 受理日.
021480* ( 委任年月日 印刷するか )
021490     IF ( 連入－委任印刷  = ZERO )
037370         MOVE 患者委任和暦Ｗ     TO 元－元号区分
037380         READ 元号マスタ
037390         NOT INVALID KEY
037400             MOVE 元－元号名称   TO 委任和暦
037410         END-READ
021500         MOVE 患者委任年Ｗ       TO 委任年
021510         MOVE 患者委任月Ｗ       TO 委任月
021520         MOVE 患者委任日Ｗ       TO 委任日
021530     END-IF.
021540*
021550* 施術ID
021560     MOVE 県施術ＩＤＷ           TO 県施術ＩＤ.
021570*
021580* 共済番号
021590     MOVE 共済番号Ｗ             TO 共済番号.
021590     MOVE 地共済番号Ｗ           TO 地共済番号.
021600*
021610************************
021620* レセプト並び順セット *
021630************************
021640*     MOVE 順番固定Ｗ          TO 順番固定.
021650*     MOVE 順番Ｗ              TO 順番.
021660*     MOVE 患者番号ＷＲ        TO 患者番号.
021670*     MOVE 枝番ＷＲ            TO 枝番.
021660     MOVE 患者番号ＷＲ        TO 患者番号２.
021670     MOVE 枝番ＷＲ            TO 枝番２.
021680*
021690*
021700* 特別コメント
021710*     MOVE 特別コメントＷ      TO 特別コメント.
021720*
021730*-------------------------------------------------------------------------*
021740*--- ※ レセ摘要再セットは、この印刷セットSECTION の最後にやること！ -----*
021750     PERFORM レセ摘要再セット.
021760*-------------------------------------------------------------------------*
021770*
021772*-------------------------------------------------------------------------*
021773*--- ※ 地域特有処理は、この印刷セットSECTION の最後にやること！   　-----*
021774*     PERFORM 地域特有処理.
021775*-------------------------------------------------------------------------*
021776*
021780****     PERFORM テスト印字処理.
021790*
021800*================================================================*
021810 項目初期化 SECTION.
021820*
021830     INITIALIZE 施術所情報Ｗ.
021840     INITIALIZE 受診者情報Ｗ.
021850     INITIALIZE 負傷情報Ｗ.
021860     INITIALIZE 備考情報Ｗ.
021870     INITIALIZE 料金１ＷＲ.
021880     INITIALIZE 料金２ＷＲ.
021890     INITIALIZE 料金３ＷＲ.
021910     INITIALIZE YAZ6421P.
021900     MOVE SPACE TO YAZ6421P.
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
026460     MOVE 施術和暦ＷＲ       TO 受２－施術和暦.
026470     MOVE 施術年ＷＲ         TO 受２－施術年.
026480     MOVE 施術月ＷＲ         TO 受２－施術月.
026490     MOVE 患者コードＷＲ     TO 受２－患者コード.
026500     READ 受診者情報２Ｆ
           INVALID KEY
              MOVE SPACE           TO 受２－レコード
           END-READ.
      *
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
021920*================================================================*
021930 料金情報取得 SECTION.
021940*
021950********************
021960* 料金データセット *
021970********************
021980*    ****************************************************************
021990*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
022000*    ****************************************************************
022010     MOVE レセ－初検料                 TO 初検料ＷＲ.
022020     IF ( レセ－時間外 = 1 )
022030         MOVE NC"○"                   TO 時間外チェックＷ
022040     END-IF.
022050     IF ( レセ－休日 = 1 )
022060         MOVE NC"○"                   TO 休日チェックＷ
022070     END-IF.
022080     IF ( レセ－深夜 = 1 )
022090         MOVE NC"○"                   TO 深夜チェックＷ
022100     END-IF.
022110*
022120     MOVE レセ－初検加算料             TO 初検加算料ＷＲ.
022130     MOVE レセ－再検料                 TO 再検料ＷＲ.
022140     MOVE レセ－往療距離               TO 往療距離ＷＲ.
022150     MOVE レセ－往療回数               TO 往療回数ＷＲ.
022160     MOVE レセ－往療料                 TO 往療料ＷＲ.
022170     MOVE レセ－往療加算料             TO 往療加算料ＷＲ.
           MOVE レセ－初検時相談料           TO 相談料ＷＲ.
022180*
022190     IF ( レセ－夜間 = 1 )
022200         MOVE NC"○"                   TO 夜間チェックＷ
022210     END-IF.
022220     IF ( レセ－難路 = 1 )
022230         MOVE NC"○"                   TO 難路チェックＷ
022240     END-IF.
022250     IF ( レセ－暴風雨雪 = 1 )
022260         MOVE NC"○"                   TO 暴風雨雪チェックＷ
022270     END-IF.
022280*
022290     MOVE レセ－金属副子加算料         TO 金属副子加算料ＷＲ.
022300*
      */金属副子・運動後療の変更・追加/1805
           MOVE レセ－金属副子回数            TO 金属回数Ｗ.
           MOVE レセ－運動後療回数            TO 運動回数Ｗ.
           MOVE レセ－運動後療料              TO 運動料Ｗ.
022400*
022410     MOVE レセ－施術情報提供料         TO 施術情報提供料ＷＲ.
022420* 小計
022420     COMPUTE 小計Ｗ = レセ－小計 + レセ－運動後療料.
022440********************
022450* 初回処置料セット *
022460********************
022470     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
022480             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
022490         MOVE レセ－初回処置料(部位ＣＮＴ) TO 初回処置料ＷＲ(部位ＣＮＴ)
022500         IF ( レセ－初回処置料(部位ＣＮＴ) NOT = ZERO )
022510            EVALUATE 負－負傷種別(部位ＣＮＴ)
022520* 捻挫・打撲・挫傷
022530            WHEN 1
022540            WHEN 2
022550            WHEN 3
022560                MOVE NC"○"       TO 施療料チェックＷ
022570* 脱臼・骨折・骨折拘縮
022580            WHEN 4
022590            WHEN 5
022600            WHEN 7
022610                MOVE NC"○"       TO 整復料チェックＷ
022620* 不全骨折・不全骨折拘縮
022630            WHEN 6
022640            WHEN 8
022650                MOVE NC"○"       TO 固定料チェックＷ
022660            END-EVALUATE
022670         END-IF
022680     END-PERFORM.
022690*
022700     MOVE レセ－初回処置料合計    TO 初回処置料合計Ｗ.
022710********************
022720* 逓減毎料金セット *
022730********************
022740*    **********
022750*    * １部位 *
022760*    **********
022770     MOVE レセ－後療単価１             TO 後療単価１ＷＲ.
022780     MOVE レセ－後療回数１             TO 後療回数１ＷＲ.
022790     MOVE レセ－後療料１               TO 後療料１ＷＲ.
022800     MOVE レセ－冷罨法回数１           TO 冷罨法回数１ＷＲ.
022810     MOVE レセ－冷罨法料１             TO 冷罨法料１ＷＲ.
022820     MOVE レセ－温罨法回数１           TO 温罨法回数１ＷＲ.
022830     MOVE レセ－温罨法料１             TO 温罨法料１ＷＲ.
022840     MOVE レセ－電療回数１             TO 電療回数１ＷＲ.
022850     MOVE レセ－電療料１               TO 電療料１ＷＲ.
022860     MOVE レセ－小計１                 TO 小計１ＷＲ.
           IF レセ－長期頻回逓減率１ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率１   TO 長期逓減率１ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率１       TO 長期逓減率１ＷＲ
           END-IF.
022880     MOVE レセ－長期込小計１           TO 長期込小計１ＷＲ.
022890*    **********
022900*    * ２部位 *
022910*    **********
022920     MOVE レセ－後療単価２             TO 後療単価２ＷＲ.
022930     MOVE レセ－後療回数２             TO 後療回数２ＷＲ.
022940     MOVE レセ－後療料２               TO 後療料２ＷＲ.
022950     MOVE レセ－冷罨法回数２           TO 冷罨法回数２ＷＲ.
022960     MOVE レセ－冷罨法料２             TO 冷罨法料２ＷＲ.
022970     MOVE レセ－温罨法回数２           TO 温罨法回数２ＷＲ.
022980     MOVE レセ－温罨法料２             TO 温罨法料２ＷＲ.
022990     MOVE レセ－電療回数２             TO 電療回数２ＷＲ.
023000     MOVE レセ－電療料２               TO 電療料２ＷＲ.
023010     MOVE レセ－小計２                 TO 小計２ＷＲ.
           IF レセ－長期頻回逓減率２ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率２   TO 長期逓減率２ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率２       TO 長期逓減率２ＷＲ
           END-IF.
023030     MOVE レセ－長期込小計２           TO 長期込小計２ＷＲ.
023040*    ****************
023050*    * ３部位／８割 *
023060*    ****************
023070     MOVE レセ－後療単価３８             TO 後療単価３８ＷＲ.
023080     MOVE レセ－後療回数３８             TO 後療回数３８ＷＲ.
023090     MOVE レセ－後療料３８               TO 後療料３８ＷＲ.
023100     MOVE レセ－冷罨法回数３８           TO 冷罨法回数３８ＷＲ.
023110     MOVE レセ－冷罨法料３８             TO 冷罨法料３８ＷＲ.
023120     MOVE レセ－温罨法回数３８           TO 温罨法回数３８ＷＲ.
023130     MOVE レセ－温罨法料３８             TO 温罨法料３８ＷＲ.
023140     MOVE レセ－電療回数３８             TO 電療回数３８ＷＲ.
023150     MOVE レセ－電療料３８               TO 電療料３８ＷＲ.
023160     MOVE レセ－小計３８                 TO 小計３８ＷＲ.
023170     MOVE レセ－多部位込小計３８         TO 多部位込小計３８ＷＲ.
           IF レセ－長期頻回逓減率３８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率３８   TO 長期逓減率３８ＷＲ
           ELSE
024160         MOVE レセ－長期逓減率３８       TO 長期逓減率３８ＷＲ
           END-IF.
023190     MOVE レセ－長期込小計３８           TO 長期込小計３８ＷＲ.
023200*    ****************
023210*    * ３部位／10割 *
023220*    ****************
023230     MOVE レセ－逓減開始月３０           TO 逓減開始月３０ＷＲ.
023240     MOVE レセ－逓減開始日３０           TO 逓減開始日３０ＷＲ.
023250     MOVE レセ－後療単価３０             TO 後療単価３０ＷＲ.
023260     MOVE レセ－後療回数３０             TO 後療回数３０ＷＲ.
023270     MOVE レセ－後療料３０               TO 後療料３０ＷＲ.
023280     MOVE レセ－冷罨法回数３０           TO 冷罨法回数３０ＷＲ.
023290     MOVE レセ－冷罨法料３０             TO 冷罨法料３０ＷＲ.
023300     MOVE レセ－温罨法回数３０           TO 温罨法回数３０ＷＲ.
023310     MOVE レセ－温罨法料３０             TO 温罨法料３０ＷＲ.
023320     MOVE レセ－電療回数３０             TO 電療回数３０ＷＲ.
023330     MOVE レセ－電療料３０               TO 電療料３０ＷＲ.
023340     MOVE レセ－小計３０                 TO 小計３０ＷＲ.
           IF レセ－長期頻回逓減率３０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率３０   TO 長期逓減率３０ＷＲ
           ELSE
024330         MOVE レセ－長期逓減率３０       TO 長期逓減率３０ＷＲ
           END-IF.
023360     MOVE レセ－長期込小計３０           TO 長期込小計３０ＷＲ.
023370*    ****************
023380*    * ４部位／５割 *
023390*    ****************
023400     MOVE レセ－後療単価４５             TO 後療単価４５ＷＲ.
023410     MOVE レセ－後療回数４５             TO 後療回数４５ＷＲ.
023420     MOVE レセ－後療料４５               TO 後療料４５ＷＲ.
023430     MOVE レセ－冷罨法回数４５           TO 冷罨法回数４５ＷＲ.
023440     MOVE レセ－冷罨法料４５             TO 冷罨法料４５ＷＲ.
023450     MOVE レセ－温罨法回数４５           TO 温罨法回数４５ＷＲ.
023460     MOVE レセ－温罨法料４５             TO 温罨法料４５ＷＲ.
023470     MOVE レセ－電療回数４５             TO 電療回数４５ＷＲ.
023480     MOVE レセ－電療料４５               TO 電療料４５ＷＲ.
023490     MOVE レセ－小計４５                 TO 小計４５ＷＲ.
023500     MOVE レセ－多部位込小計４５         TO 多部位込小計４５ＷＲ.
023510     MOVE レセ－長期逓減率４５           TO 長期逓減率４５ＷＲ.
023520     MOVE レセ－長期込小計４５           TO 長期込小計４５ＷＲ.
023530*    ****************
023540*    * ４部位／８割 *
023550*    ****************
023560     MOVE レセ－逓減開始月４８           TO 逓減開始月４８ＷＲ.
023570     MOVE レセ－逓減開始日４８           TO 逓減開始日４８ＷＲ.
023580     MOVE レセ－後療単価４８             TO 後療単価４８ＷＲ.
023590     MOVE レセ－後療回数４８             TO 後療回数４８ＷＲ.
023600     MOVE レセ－後療料４８               TO 後療料４８ＷＲ.
023610     MOVE レセ－冷罨法回数４８           TO 冷罨法回数４８ＷＲ.
023620     MOVE レセ－冷罨法料４８             TO 冷罨法料４８ＷＲ.
023630     MOVE レセ－温罨法回数４８           TO 温罨法回数４８ＷＲ.
023640     MOVE レセ－温罨法料４８             TO 温罨法料４８ＷＲ.
023650     MOVE レセ－電療回数４８             TO 電療回数４８ＷＲ.
023660     MOVE レセ－電療料４８               TO 電療料４８ＷＲ.
023670     MOVE レセ－小計４８                 TO 小計４８ＷＲ.
023680     MOVE レセ－多部位込小計４８         TO 多部位込小計４８ＷＲ.
           IF レセ－長期頻回逓減率４８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率４８   TO 長期逓減率４８ＷＲ
           ELSE
024670         MOVE レセ－長期逓減率４８       TO 長期逓減率４８ＷＲ
           END-IF.
023700     MOVE レセ－長期込小計４８           TO 長期込小計４８ＷＲ.
023710*    ****************
023720*    * ４部位／10割 *
023730*    ****************
023740     MOVE レセ－逓減開始月４０           TO 逓減開始月４０ＷＲ.
023750     MOVE レセ－逓減開始日４０           TO 逓減開始日４０ＷＲ.
023760     MOVE レセ－後療単価４０             TO 後療単価４０ＷＲ.
023770     MOVE レセ－後療回数４０             TO 後療回数４０ＷＲ.
023780     MOVE レセ－後療料４０               TO 後療料４０ＷＲ.
023790     MOVE レセ－冷罨法回数４０           TO 冷罨法回数４０ＷＲ.
023800     MOVE レセ－冷罨法料４０             TO 冷罨法料４０ＷＲ.
023810     MOVE レセ－温罨法回数４０           TO 温罨法回数４０ＷＲ.
023820     MOVE レセ－温罨法料４０             TO 温罨法料４０ＷＲ.
023830     MOVE レセ－電療回数４０             TO 電療回数４０ＷＲ.
023840     MOVE レセ－電療料４０               TO 電療料４０ＷＲ.
023850     MOVE レセ－小計４０                 TO 小計４０ＷＲ.
           IF レセ－長期頻回逓減率４０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率４０   TO 長期逓減率４０ＷＲ
           ELSE
024840         MOVE レセ－長期逓減率４０       TO 長期逓減率４０ＷＲ
           END-IF.
023870     MOVE レセ－長期込小計４０           TO 長期込小計４０ＷＲ.
023880*    *****************
023890*    * ５部位／2.5割 *
023900*    *****************
023910     MOVE レセ－後療単価５２             TO 後療単価５２ＷＲ.
023920     MOVE レセ－後療回数５２             TO 後療回数５２ＷＲ.
023930     MOVE レセ－後療料５２               TO 後療料５２ＷＲ.
023940     MOVE レセ－冷罨法回数５２           TO 冷罨法回数５２ＷＲ.
023950     MOVE レセ－冷罨法料５２             TO 冷罨法料５２ＷＲ.
023960     MOVE レセ－温罨法回数５２           TO 温罨法回数５２ＷＲ.
023970     MOVE レセ－温罨法料５２             TO 温罨法料５２ＷＲ.
023980     MOVE レセ－電療回数５２             TO 電療回数５２ＷＲ.
023990     MOVE レセ－電療料５２               TO 電療料５２ＷＲ.
024000     MOVE レセ－小計５２                 TO 小計５２ＷＲ.
024010     MOVE レセ－多部位込小計５２         TO 多部位込小計５２ＷＲ.
024020     MOVE レセ－長期逓減率５２           TO 長期逓減率５２ＷＲ.
024030     MOVE レセ－長期込小計５２           TO 長期込小計５２ＷＲ.
024040*    ****************
024050*    * ５部位／５割 *
024060*    ****************
024070     MOVE レセ－逓減開始月５５           TO 逓減開始月５５ＷＲ.
024080     MOVE レセ－逓減開始日５５           TO 逓減開始日５５ＷＲ.
024090     MOVE レセ－後療単価５５             TO 後療単価５５ＷＲ.
024100     MOVE レセ－後療回数５５             TO 後療回数５５ＷＲ.
024110     MOVE レセ－後療料５５               TO 後療料５５ＷＲ.
024120     MOVE レセ－冷罨法回数５５           TO 冷罨法回数５５ＷＲ.
024130     MOVE レセ－冷罨法料５５             TO 冷罨法料５５ＷＲ.
024140     MOVE レセ－温罨法回数５５           TO 温罨法回数５５ＷＲ.
024150     MOVE レセ－温罨法料５５             TO 温罨法料５５ＷＲ.
024160     MOVE レセ－電療回数５５             TO 電療回数５５ＷＲ.
024170     MOVE レセ－電療料５５               TO 電療料５５ＷＲ.
024180     MOVE レセ－小計５５                 TO 小計５５ＷＲ.
024190     MOVE レセ－多部位込小計５５         TO 多部位込小計５５ＷＲ.
024200     MOVE レセ－長期逓減率５５           TO 長期逓減率５５ＷＲ.
024210     MOVE レセ－長期込小計５５           TO 長期込小計５５ＷＲ.
024220*    ****************
024230*    * ５部位／８割 *
024240*    ****************
024250     MOVE レセ－逓減開始月５８           TO 逓減開始月５８ＷＲ.
024260     MOVE レセ－逓減開始日５８           TO 逓減開始日５８ＷＲ.
024270     MOVE レセ－後療単価５８             TO 後療単価５８ＷＲ.
024280     MOVE レセ－後療回数５８             TO 後療回数５８ＷＲ.
024290     MOVE レセ－後療料５８               TO 後療料５８ＷＲ.
024300     MOVE レセ－冷罨法回数５８           TO 冷罨法回数５８ＷＲ.
024310     MOVE レセ－冷罨法料５８             TO 冷罨法料５８ＷＲ.
024320     MOVE レセ－温罨法回数５８           TO 温罨法回数５８ＷＲ.
024330     MOVE レセ－温罨法料５８             TO 温罨法料５８ＷＲ.
024340     MOVE レセ－電療回数５８             TO 電療回数５８ＷＲ.
024350     MOVE レセ－電療料５８               TO 電療料５８ＷＲ.
024360     MOVE レセ－小計５８                 TO 小計５８ＷＲ.
024370     MOVE レセ－多部位込小計５８         TO 多部位込小計５８ＷＲ.
           IF レセ－長期頻回逓減率５８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率５８   TO 長期逓減率５８ＷＲ
           ELSE
025360         MOVE レセ－長期逓減率５８       TO 長期逓減率５８ＷＲ
           END-IF.
024390     MOVE レセ－長期込小計５８           TO 長期込小計５８ＷＲ.
024400*    ****************
024410*    * ５部位／10割 *
024420*    ****************
024430     MOVE レセ－逓減開始月５０           TO 逓減開始月５０ＷＲ.
024440     MOVE レセ－逓減開始日５０           TO 逓減開始日５０ＷＲ.
024450     MOVE レセ－後療単価５０             TO 後療単価５０ＷＲ.
024460     MOVE レセ－後療回数５０             TO 後療回数５０ＷＲ.
024470     MOVE レセ－後療料５０               TO 後療料５０ＷＲ.
024480     MOVE レセ－冷罨法回数５０           TO 冷罨法回数５０ＷＲ.
024490     MOVE レセ－冷罨法料５０             TO 冷罨法料５０ＷＲ.
024500     MOVE レセ－温罨法回数５０           TO 温罨法回数５０ＷＲ.
024510     MOVE レセ－温罨法料５０             TO 温罨法料５０ＷＲ.
024520     MOVE レセ－電療回数５０             TO 電療回数５０ＷＲ.
024530     MOVE レセ－電療料５０               TO 電療料５０ＷＲ.
024540     MOVE レセ－小計５０                 TO 小計５０ＷＲ.
           IF レセ－長期頻回逓減率５０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率５０   TO 長期逓減率５０ＷＲ
           ELSE
025530         MOVE レセ－長期逓減率５０       TO 長期逓減率５０ＷＲ
           END-IF.
024560     MOVE レセ－長期込小計５０           TO 長期込小計５０ＷＲ.
      */2022
           MOVE レセ－明細書発行加算料         TO 明細書発行加算料ＷＲ.
           MOVE レセ－明細書発行加算日         TO 明細書発行加算日ＷＲ.
           IF レセ－明細書発行加算料 NOT = ZERO
               STRING "明細書発行体制加算"     DELIMITED BY SIZE
                      明細書発行加算料ＷＲ     DELIMITED BY SIZE
                      "円 加算日"              DELIMITED BY SIZE
                      明細書発行加算日ＷＲ     DELIMITED BY SIZE
                      "日"                     DELIMITED BY SIZE
                 INTO 適用２Ｗ
               END-STRING
           END-IF.
024570*
024580*================================================================*
024590 施術所情報取得 SECTION.
024600*
024610**************************************************
024620* 本院データを使用し、以下の情報を取得           *
024630* ● 柔整師番号.. 柔整師番号Ｗに格納             *
024640* ● 会員番号 ... 接骨師会会員番号Ｗに格納       *
024650* ● 代表者名 ... 代表者名Ｗに格納               *
024660* ● 住所1,2   ...施術所住所1,2Ｗに格納          *
024670* ● 電話番号 ... 施術所電話番号Ｗに格納         *
024680**************************************************
024690     MOVE ZERO  TO 施情－施術所番号.
024700     READ 施術所情報マスタ
024710     INVALID KEY
024720         CONTINUE
024730     NOT INVALID KEY
024740*
               MOVE 施情－都道府県ＪＩＳ    TO 都道府県ＪＩＳＷ
024741         MOVE 施情－新柔整師番号      TO 柔整師番号Ｗ
024741         MOVE 施情－接骨師会会員番号  TO 接骨師会会員番号Ｗ
024800*
024830         MOVE 施情－郵便番号１        TO 施術所郵便番号１Ｗ
024840         MOVE 施情－郵便番号２        TO 施術所郵便番号２Ｗ
024850         MOVE 施情－代表者カナ        TO 代表者カナＷ
024860         MOVE 施情－代表者名          TO 代表者名Ｗ
024870*
024880         MOVE 施情－接骨院名          TO 接骨院名Ｗ
024890*
024900         MOVE 施情－住所１            TO 施術所住所１Ｗ
024910         MOVE 施情－住所２            TO 施術所住所２Ｗ
024920***         STRING 施情－住所１  DELIMITED BY SPACE
024930***                施情－住所２  DELIMITED BY SPACE
024940***           INTO 施術所住所Ｗ
024950***         END-STRING
024960*
024970         MOVE 施情－電話番号          TO 施術所電話番号Ｗ
024980*
024990***         MOVE 施情－取引先銀行名      TO 取引先銀行名Ｗ
025000***         MOVE 施情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
025010***         MOVE 施情－預金種別          TO 預金種別Ｗ
025020***         MOVE 施情－口座番号          TO 口座番号Ｗ
025030***         MOVE 施情－口座名義人カナ    TO 口座名義人カナＷ
025040***         MOVE 施情－口座名義人        TO 口座名義人Ｗ
025050*
025060***         EVALUATE 預金種別Ｗ
025070***         WHEN 1
025080***             MOVE NC"（普）" TO 預金種別コメントＷ
025090***         WHEN 2
025100***             MOVE NC"（当）" TO 預金種別コメントＷ
025110***         WHEN OTHER
025120***             MOVE SPACE      TO 預金種別コメントＷ
025130***         END-EVALUATE
025140*/ 口座情報固定 /*
025210*
025330*         MOVE "5610002" TO 口座番号Ｗ
               PERFORM 口座番号取得
      */機関コードコメント↓↓↓/20230817
025350*------------------------------------------------------------------------*
025360*         EVALUATE 保険種別ＷＲ
025370*         WHEN 01
025380*             MOVE 保険者番号ＷＲ       TO 保険者番号比較Ｗ
025390*             PERFORM 県施術ＩＤセット
025400*         WHEN 08
      *         WHEN 05
025410*             MOVE 保険者番号ＷＲ(3:6)  TO 保険者番号比較Ｗ
025420*             PERFORM 県施術ＩＤセット
025430*         WHEN 04
025440*             PERFORM 共済番号セット
025450*         WHEN 09
025460*             PERFORM 自衛官番号セット
025470*         END-EVALUATE
      */機関コードコメント↑↑↑/20230817
025480*
025490     END-READ.
025500*
      */機関コード個人参考↓↓↓/20230817
025840*
025850*********************************************
025860** ＩＤ管理マスタより　施術ＩＤを取得する。
025870*********************************************
025880** 県施術ID
025890     MOVE 01                   TO ＩＤ管－ＩＤ区分.
025900     MOVE ZERO                 TO ＩＤ管－施術所番号.
025910     MOVE 費用負担者番号助成ＷＲ(3:2)  TO ＩＤ管－保険種別.
025920     MOVE SPACE                TO ＩＤ管－保険者番号.
025930     READ ＩＤ管理マスタ
025940     NOT INVALID KEY
025950          MOVE ＩＤ管－施術ＩＤ番号   TO 県施術ＩＤＷ
025960     END-READ.
025970*
025980** 市町村ID
025990     MOVE 02                     TO ＩＤ管－ＩＤ区分.
026000     MOVE ZERO                   TO ＩＤ管－施術所番号.
026010     MOVE 助成種別ＷＲ           TO ＩＤ管－保険種別.
026020     MOVE 費用負担者番号助成ＷＲ TO ＩＤ管－保険者番号.
      */京都市の重度障害/120711
           IF 費用負担者番号助成ＷＲ(1:5) = "39261"
026020         MOVE "264"              TO ＩＤ管－保険者番号
           END-IF.
      *
026030     READ ＩＤ管理マスタ
           INVALID KEY
              IF 費用負担者番号助成ＷＲ(1:5) = "39261"
025890           MOVE 01                   TO ＩＤ管－ＩＤ区分
025900           MOVE ZERO                 TO ＩＤ管－施術所番号
025910           MOVE 50                   TO ＩＤ管－保険種別
025920           MOVE SPACE                TO ＩＤ管－保険者番号
025930           READ ＩＤ管理マスタ
025940           NOT INVALID KEY
026050              MOVE ＩＤ管－施術ＩＤ番号   TO 市町村施術ＩＤＷ
                 END-READ
              END-IF
026040     NOT INVALID KEY
026050        MOVE ＩＤ管－施術ＩＤ番号   TO 市町村施術ＩＤＷ
026060     END-READ.
      */機関コード個人参考↑↑↑/20230817
026070*
026080***
025510*================================================================*
025520 県施術ＩＤセット SECTION.
025530*
025540*********************************************
025550** ＩＤ管理マスタより  県施術ＩＤを取得する。
025561*   (国保組合は、対象外　→　対象！2005/09 )
025570*********************************************
025580**   / 県施術ID /
025600     MOVE 01                     TO ＩＤ管－ＩＤ区分.
025610     MOVE ZERO                   TO ＩＤ管－施術所番号.
025620     MOVE 保険者番号比較Ｗ(1:2)  TO ＩＤ管－保険種別.
025630     MOVE SPACE                  TO ＩＤ管－保険者番号.
025640     READ ＩＤ管理マスタ
025650     NOT INVALID KEY
025660         MOVE ＩＤ管－施術ＩＤ番号   TO 県施術ＩＤＷ
025670     END-READ.
025690*
025700*================================================================*
025710 共済番号セット SECTION.
025720*
025730**************************************************************
025740* 保険者番号により、共済の番号を印字するか判定
025750* 中央特有 追加 99/10
025760**************************************************************
025770** 1.共済組合連盟
025780     MOVE SPACE  TO  脱出フラグ.
025790     IF ( 施情－共済連番号 NOT = ZERO )
025800** 条件(保険者番号)
025810*        IF ( 保険者番号ＷＲ(1:2) = "31" )  OR
025820*           ( 保険者番号ＷＲ = "34130021" )
025830*
025840           MOVE  NC"共済組合連盟第"   TO 共済連番号名ＮＷ 
025850           MOVE  NC"号"               TO 共済連番号単位ＮＷ 
025860           MOVE  施情－共済連番号     TO 共済連番号Ｗ
025870           IF ( 共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
025880                 MOVE SPACE TO  共済連番号Ｗ(1:1)
025890           ELSE
025900                 MOVE "YES" TO  脱出フラグ
025910           END-IF
025920           IF ( 共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
025930                 MOVE SPACE TO  共済連番号Ｗ(2:1)
025940           ELSE
025950                 MOVE "YES" TO  脱出フラグ
025960           END-IF
025970           IF ( 共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
025980                 MOVE SPACE TO  共済連番号Ｗ(3:1)
025990           ELSE
026000                 MOVE "YES" TO  脱出フラグ
026010           END-IF
026020           IF ( 共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
026030                 MOVE SPACE TO  共済連番号Ｗ(4:1)
026040           ELSE
026050                 MOVE "YES" TO  脱出フラグ
026060           END-IF
026070           IF ( 共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
026080                 MOVE SPACE TO  共済連番号Ｗ(5:1)
026090           ELSE
026100                 MOVE "YES" TO  脱出フラグ
026110           END-IF
026120           IF ( 共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
026130                 MOVE SPACE TO  共済連番号Ｗ(6:1)
026140           ELSE
026150                 MOVE "YES" TO  脱出フラグ
026160           END-IF
026170           MOVE  共済連番号集団Ｗ     TO 共済番号Ｗ
026180*        END-IF
026190     END-IF.
026200*
026210** 2. 地共済協議会
026220     MOVE SPACE  TO  脱出フラグ.
026230     IF ( 施情－地共済連番号 NOT = ZERO )
026240** 条件(保険者番号)
026250*        IF ( 保険者番号ＷＲ(1:2) = "32" OR "33" OR "34" )  AND
026260*           ( 保険者番号ＷＲ NOT = "34130021" )
026270*
026280           MOVE  NC"地共済協議会第"   TO 共済連番号名ＮＷ 
026290           MOVE  NC"号"               TO 共済連番号単位ＮＷ 
026300           MOVE  施情－地共済連番号   TO 共済連番号Ｗ
026310           IF ( 共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
026320                 MOVE SPACE TO  共済連番号Ｗ(1:1)
026330           ELSE
026340                 MOVE "YES" TO  脱出フラグ
026350           END-IF
026360           IF ( 共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
026370                 MOVE SPACE TO  共済連番号Ｗ(2:1)
026380           ELSE
026390                 MOVE "YES" TO  脱出フラグ
026400           END-IF
026410           IF ( 共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
026420                 MOVE SPACE TO  共済連番号Ｗ(3:1)
026430           ELSE
026440                 MOVE "YES" TO  脱出フラグ
026450           END-IF
026460           IF ( 共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
026470                 MOVE SPACE TO  共済連番号Ｗ(4:1)
026480           ELSE
026490                 MOVE "YES" TO  脱出フラグ
026500           END-IF
026510           IF ( 共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
026520                 MOVE SPACE TO  共済連番号Ｗ(5:1)
026530           ELSE
026540                 MOVE "YES" TO  脱出フラグ
026550           END-IF
026560           IF ( 共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
026570                 MOVE SPACE TO  共済連番号Ｗ(6:1)
026580           ELSE
026590                 MOVE "YES" TO  脱出フラグ
026600           END-IF
026610           MOVE  共済連番号集団Ｗ     TO 地共済番号Ｗ
026620*        END-IF
027050     END-IF.
027060*
027070*================================================================*
027080 自衛官番号セット SECTION.
027090*
027100     MOVE SPACE  TO  脱出フラグ.
027110     IF ( 施情－自衛官番号 NOT = ZERO )
027111           IF 施情－防衛省区分 = 1
027112              MOVE  NC"防衛省第"      TO 自衛官番号名ＮＷ 
027113           ELSE
027114              MOVE  NC"防衛庁第"      TO 自衛官番号名ＮＷ 
027115           END-IF
027120*           MOVE  NC"防衛庁第"         TO 自衛官番号名ＮＷ 
027130           MOVE  NC"号"               TO 自衛官番号単位ＮＷ 
027140           MOVE  施情－自衛官番号     TO 自衛官番号Ｗ
027150           IF ( 自衛官番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
027160                 MOVE SPACE TO  自衛官番号Ｗ(1:1)
027170           ELSE
027180                 MOVE "YES" TO  脱出フラグ
027190           END-IF
027200           IF ( 自衛官番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
027210                 MOVE SPACE TO  自衛官番号Ｗ(2:1)
027220           ELSE
027230                 MOVE "YES" TO  脱出フラグ
027240           END-IF
027250           IF ( 自衛官番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
027260                 MOVE SPACE TO  自衛官番号Ｗ(3:1)
027270           ELSE
027280                 MOVE "YES" TO  脱出フラグ
027290           END-IF
027300           IF ( 自衛官番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
027310                 MOVE SPACE TO  自衛官番号Ｗ(4:1)
027320           ELSE
027330                 MOVE "YES" TO  脱出フラグ
027340           END-IF
027350           IF ( 自衛官番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
027360                 MOVE SPACE TO  自衛官番号Ｗ(5:1)
027370           ELSE
027380                 MOVE "YES" TO  脱出フラグ
027390           END-IF
027400           IF ( 自衛官番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
027410                 MOVE SPACE TO  自衛官番号Ｗ(6:1)
027420           ELSE
027430                 MOVE "YES" TO  脱出フラグ
027440           END-IF
027450           MOVE  自衛官番号集団Ｗ     TO 共済番号Ｗ
027460     END-IF.
027470*
027480*================================================================*
027490 受診者情報取得 SECTION.
027500*
027510**************************************************
027520* 連結データから受診者情報Ｆより以下の情報を取得 *
027530* ● 施術年 ..... 施術年Ｗに格納                 *
027540* ● 施術月 ..... 施術月Ｗに格納                 *
027550* ● 患者番号.... 患者番号Ｗに格納※ＦＤ連番用   *
027560* ● 記号 ....... 記号Ｗに格納                   *
027570* ● 番号 ....... 番号Ｗに格納                   *
027580* ● 保険者番号 . 保険者番号Ｗに格納             *
027590* ● 保険種別 ... 保険種別Ｗに格納               *
027600* ● 被保険者カナ.被保険者カナＷに格納           *
027610* ● 被保険者氏名.被保険者氏名Ｗに格納           *
027620* ● 住所１ ......被保険者住所１Ｗに格納         *
027630* ● 住所２ ......被保険者住所２Ｗに格納         *
027640* ● 患者カナ ....患者カナＷに格納               *
027650* ● 患者氏名 ....患者氏名Ｗに格納               *
027660* ● 患者性別 ....区分によりチェックに"○"を格納 *
027670* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
027680* ● 患者年 ......患者年Ｗに格納                 *
027690* ● 患者月 ......患者月Ｗに格納                 *
027700* ● 患者日 ......患者日Ｗに格納                 *
027710* ● 続柄 ........名称マスタより続柄Ｗに取得     *
027720**************************************************
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
      *         WHEN 09
                  MOVE NC"○"        TO 共済チェックＷ
               WHEN 09
                  MOVE NC"○"        TO 自チェックＷ
               WHEN 08
                  MOVE NC"○"        TO 退職チェックＷ
               WHEN 05
                  MOVE NC"○"        TO 後期チェックＷ
022770         END-EVALUATE
      *
               IF 受－助成種別 = ZERO
                   MOVE NC"○" TO 単独チェックＷ
               ELSE
                   MOVE NC"○" TO ２併チェックＷ
               END-IF
      */助成レセは本人のみに○
      *         MOVE NC"○" TO 本人チェックＷ
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
               EVALUATE レセ－給付割合
               WHEN 10
                   MOVE NC"○" TO １０割チェックＷ
               WHEN 9
                   MOVE NC"○" TO ９割チェックＷ
      */○付け変更/160318
      */前期高齢１割は８割給付に○/110721
      *             IF (受－保険種別 NOT = 05 ) AND (受－特別区分 = 1)
      *                 MOVE SPACE  TO ９割チェックＷ
      *                 MOVE NC"○" TO ８割チェックＷ
      *             END-IF
      */東京13、宮城04の場合、前期高齢者１割は、給付割合を８割にする。(国が１割負担するため、患者１割、保険者８割、国１割となる)/160817
                   IF ((受－保険種別     = 01) AND (受－保険者番号(1:2) = "13" OR "04")) OR
                      ((受－保険種別 NOT = 01) AND (受－保険者番号(3:2) = "13" OR "04"))
                       IF (受－保険種別 NOT = 05 ) AND (受－特別区分 = 1)
                           MOVE SPACE  TO ９割チェックＷ
                           MOVE NC"○" TO ８割チェックＷ
                       END-IF
                   END-IF
               WHEN 8
                   MOVE NC"○" TO ８割チェックＷ
               WHEN 7
                   MOVE NC"○" TO ７割チェックＷ
               END-EVALUATE
               MOVE 受－施術和暦     TO 施術和暦Ｗ
027820         MOVE 受－施術年       TO 施術年Ｗ
027830         MOVE 受－施術月       TO 施術月Ｗ
027840         MOVE 受－患者番号     TO 患者番号Ｗ
027850*         MOVE 受－記号         TO 記号Ｗ
027860*         MOVE 受－番号         TO 番号Ｗ
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
027870         MOVE 受－保険者番号   TO 保険者番号Ｗ
027880         MOVE 受－保険種別     TO 保険種別Ｗ
027890** 全国土木の枝番削除
027900         IF ( 受－保険種別 = 01 ) AND ( 受－保険者番号(1:6) = "133033" )
027910            MOVE 受－保険者番号(1:6)  TO 保険者番号Ｗ
027920         END-IF
027930**
027940         MOVE 受－被保険者カナ TO 被保険者カナＷ
027950         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
027980         MOVE 受－住所１       TO 被保険者住所１Ｗ
027990         MOVE 受－住所２       TO 被保険者住所２Ｗ
027960         MOVE 受－患者郵便番号１   TO 郵便番号１Ｗ
027970         MOVE 受－患者郵便番号２   TO 郵便番号２Ｗ
027980         MOVE 受－患者住所１       TO 患者住所１Ｗ
027990         MOVE 受－患者住所２       TO 患者住所２Ｗ
      */ 電話番号追加 /42505
               IF 受－患者電話番号 NOT = SPACE
                  STRING "電話:"            DELIMITED BY SIZE
                         受－患者電話番号   DELIMITED BY SPACE
                    INTO 電話番号Ｗ
                  END-STRING
               END-IF
028000         MOVE 受－患者カナ     TO 患者カナＷ
028010         MOVE 受－患者氏名     TO 患者氏名Ｗ
028020         MOVE 受－費用負担者番号助成 TO 市町村番号Ｗ
               MOVE 受－受益者番号助成     TO 受給者番号Ｗ
028030*
028040         EVALUATE 受－患者性別
028050         WHEN 1
028060             MOVE NC"男"  TO 性別Ｗ
028070             MOVE NC"○"  TO 男チェックＷ
028080         WHEN 2
028090             MOVE NC"女"  TO 性別Ｗ
028100             MOVE NC"○"  TO 女チェックＷ
028110         END-EVALUATE
028120*
028130         EVALUATE 受－患者和暦
028140         WHEN 1
028150             MOVE NC"明治"  TO 元号Ｗ
028160             MOVE NC"○"    TO 明治チェックＷ
028170         WHEN 2
028180             MOVE NC"大正"  TO 元号Ｗ
028190             MOVE NC"○"    TO 大正チェックＷ
028200         WHEN 3
028210             MOVE NC"昭和"  TO 元号Ｗ
028220             MOVE NC"○"    TO 昭和チェックＷ
028230         WHEN 4
028240             MOVE NC"平成"  TO 元号Ｗ
028250             MOVE NC"○"    TO 平成チェックＷ
      */元号修正/20190426
023060         WHEN 5
                   MOVE "5令"   TO 令和ＣＭＷ
023070             MOVE NC"○"  TO 令和チェックＷ
028260         END-EVALUATE
028270*
      */元号修正/↓↓↓20190426
029310         IF 受－患者和暦 > 4
037370             MOVE 受－患者和暦     TO 元－元号区分
037380             READ 元号マスタ
037390             NOT INVALID KEY
037400                 MOVE 元－元号名称 TO 元号Ｗ
037410             END-READ
029330         END-IF
      */元号修正/↑↑↑20190426
028280         MOVE 受－患者年  TO 患者年Ｗ
028290         MOVE 受－患者月  TO 患者月Ｗ
028300         MOVE 受－患者日  TO 患者日Ｗ
028310*
028320* 続柄設定
028330         IF ( 本人家族区分ＷＲ = 1 )
028340            MOVE NC"本人"    TO 続柄Ｗ
028350         ELSE
028360            MOVE 05          TO 名－区分コード
028370            MOVE 受－続柄    TO 名－名称コード
028380            READ 名称マスタ
028390            INVALID KEY
028400                MOVE SPACE    TO 続柄Ｗ
028410            NOT INVALID KEY
028420                MOVE 名－略称 TO 続柄Ｗ
028430            END-READ
028440         END-IF
028520**
028530* 14/10～　特別区分コメント印字
028540         IF ( 受－施術和暦年月 >= 41410 )
028550             IF ( 受－公費種別 = ZERO )
028560                EVALUATE 受－特別区分
028570                WHEN 1
028580                   MOVE "70才以上 1割"  TO 特別コメントＷ
028590                WHEN 2
028600                   MOVE "70才以上 2割"  TO 特別コメントＷ
028601                WHEN 3
028602                   MOVE "70才以上 3割"  TO 特別コメントＷ
028610                WHEN 6
028622                   IF 受－施術和暦年月 < 42004
028624                      MOVE "3才未満"       TO 特別コメントＷ
028625                   ELSE
028626                      MOVE "義務教育就学前"  TO 特別コメントＷ
028628                   END-IF
028631                END-EVALUATE
028640             END-IF
028650         END-IF
028660*
028670     END-IF.
028680*
028690     EVALUATE 保険種別ＷＲ
028700     WHEN 01
028710         MOVE NC"国" TO 保険種別名称Ｗ
028720     WHEN 02
028730         MOVE NC"政" TO 保険種別名称Ｗ
028740     WHEN 03
028750         MOVE NC"組" TO 保険種別名称Ｗ
028760     WHEN 04
028770         MOVE NC"共" TO 保険種別名称Ｗ
028780     WHEN 06
028790         MOVE NC"日" TO 保険種別名称Ｗ
028800     WHEN 07
028810         MOVE NC"船" TO 保険種別名称Ｗ
028820     WHEN 08
028830         MOVE NC"退" TO 保険種別名称Ｗ
028840     WHEN 09
028850         MOVE NC"自" TO 保険種別名称Ｗ
028860     END-EVALUATE.
028870*================================================================*
028880* 請求先情報取得 SECTION.
028890**
028900*****************************************************
028910** 連結データから保険者マスタより請求先を取得する。 *
028920** ※保－請求先情報区分=1の場合請求先マスタを使用   *
028930** ● 請求先...... 請求先名称Ｗに格納               *
028940*****************************************************
028950*     MOVE 保険種別ＷＲ   TO 保－保険種別.
028960*     MOVE 保険者番号ＷＲ TO 保－保険者番号.
028970*     READ 保険者マスタ
028980*     INVALID KEY
      *         IF ( 保険種別ＷＲ = 05 ) AND ( 施術和暦年月ＷＲ >= 42004 )
030800*             MOVE 保険種別ＷＲ   TO 市－公費種別
030810*             MOVE 保険者番号ＷＲ TO 市－市町村番号
030820*             READ 市町村マスタ
030830*             INVALID KEY
030840*                 MOVE SPACE      TO 請求先名称Ｗ
030850*             NOT INVALID KEY
031330*                 MOVE 市－市町村名称    TO 請求先名称Ｗ
      *             END-READ
      *         ELSE
030840*             MOVE SPACE      TO 請求先名称Ｗ
      *         END-IF
029000*     NOT INVALID KEY
029010** 社保、日雇は「社会保険事務所」をつける
029020*                 EVALUATE 保険種別ＷＲ 
029030*                 WHEN  02
029040*                 WHEN  06
029050*                     IF ( 保－接尾語区分 = 1 )
029060*                        MOVE 保－保険者名称    TO 請求先名称Ｗ
029070*                     ELSE
029080*                        STRING 保－保険者名称    DELIMITED BY SPACE
029090*                               "社会保険事務所"  DELIMITED BY SIZE
029100*                               INTO 請求先名称Ｗ
029110*                        END-STRING
029120*                     END-IF
029130** 組合は支部名まで印字
029140*                 WHEN  03
029150*                     STRING 保－保険者名称  DELIMITED BY SPACE
029160*                            "健康保険組合"  DELIMITED BY SIZE
029170*                            "  "            DELIMITED BY SIZE
029180*                            保－支部部署名  DELIMITED BY SPACE
029190*                            INTO 請求先名称Ｗ
029200*                     END-STRING
029210** 共済は支部名まで印字
029220*                 WHEN  04
029230*                     STRING 保－保険者名称  DELIMITED BY SPACE
029240*                            "共済組合"      DELIMITED BY SIZE
029250*                            "  "            DELIMITED BY SIZE
029260*                            保－支部部署名  DELIMITED BY SPACE
029270*                            INTO 請求先名称Ｗ
029280*                     END-STRING
029290*                 WHEN OTHER
029300*                     MOVE 保－保険者名称    TO 請求先名称Ｗ
029310*                 END-EVALUATE
029320*     END-READ.
029330**
029340*================================================================*
029350 負傷データ取得 SECTION.
029360*
029370**************************************************
029380* 連結データから負傷データＦより以下の情報を取得 *
029390* ● 負傷名...部位＋負傷種別にて加工して格納     *
029400* ● 負傷年.......負傷年Ｗ                       *
029410* ● 負傷月.......負傷月Ｗ                       *
029420* ● 負傷日.......負傷日Ｗ                       *
029430* ● 開始年.......初検年Ｗ                       *
029440* ● 開始月.......初検月Ｗ                       *
029450* ● 開始日.......初検日Ｗ                       *
029460* ● 終了年.......終了年Ｗ                       *
029470* ● 終了月.......終了月Ｗ                       *
029480* ● 終了日.......終了日Ｗ                       *
029490* ● 実日数.......実日数Ｗ                       *
029500* ● 転帰区分 ....区分によりチェックに"○"を格納 *
029510* ● 金属副子 ....区分によりチェックに"○"を格納 *
029520* ● 経過コード...経過マスタより取得             *
029530**************************************************
           IF 負－レコード NOT = SPACE
029630         MOVE 負－部位数                   TO 部位数Ｗ
029640         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
029650                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
029660             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別Ｗ(部位ＣＮＴ)
029670             MOVE 負－部位(部位ＣＮＴ)     TO 部位Ｗ(部位ＣＮＴ)
029680             MOVE 負－左右区分(部位ＣＮＴ) TO 左右区分Ｗ(部位ＣＮＴ)
029690             MOVE 負－負傷位置番号(部位ＣＮＴ)
029700                                           TO 負傷位置番号Ｗ(部位ＣＮＴ)
029710*********************************************
029720* 注）全柔...負傷種別＋部位にて加工して格納 *
029730*********************************************
029740* 負傷種別
029750             MOVE SPACE                     TO 負傷名称Ｗ
029760             MOVE 03                        TO 名－区分コード
029770             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
029780             READ 名称マスタ
029790             INVALID KEY
029800                 MOVE SPACE        TO 負傷名称Ｗ
029810             NOT INVALID KEY
029820                 MOVE 名－正式名称 TO 負傷名称Ｗ
029830             END-READ
029840* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
032680*
032690             PERFORM 部位名称埋込処理
030030*
030040             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
030050             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
030060             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
030070             MOVE 負－開始年(部位ＣＮＴ)   TO 初検年Ｗ(部位ＣＮＴ)
030080             MOVE 負－開始月(部位ＣＮＴ)   TO 初検月Ｗ(部位ＣＮＴ)
030090             MOVE 負－開始日(部位ＣＮＴ)   TO 初検日Ｗ(部位ＣＮＴ)
030100             IF ( 負－転帰区分(部位ＣＮＴ) = 9 )
      */受理和暦・委任和暦が印刷されない/20181207
030150                 MOVE 9                    TO 終了和暦Ｗ(部位ＣＮＴ)
030110                 MOVE 99                   TO 終了年Ｗ(部位ＣＮＴ)
030120                 MOVE 99                   TO 終了月Ｗ(部位ＣＮＴ)
030130                 MOVE 99                   TO 終了日Ｗ(部位ＣＮＴ)
030140             ELSE
      */受理和暦・委任和暦が印刷されない/20181207
030150                 MOVE 負－終了和暦(部位ＣＮＴ) TO 終了和暦Ｗ(部位ＣＮＴ)
030150                 MOVE 負－終了年(部位ＣＮＴ)   TO 終了年Ｗ(部位ＣＮＴ)
030160                 MOVE 負－終了月(部位ＣＮＴ)   TO 終了月Ｗ(部位ＣＮＴ)
030170                 MOVE 負－終了日(部位ＣＮＴ)   TO 終了日Ｗ(部位ＣＮＴ)
030180             END-IF
030190* 経過略称取得
030200             MOVE 01                         TO 経－区分コード
030210             MOVE 負－経過コード(部位ＣＮＴ) TO 経－経過コード
030220             READ 経過マスタ
030230             INVALID KEY
030240                 MOVE ZERO            TO 部位ＣＮＴＷ(部位ＣＮＴ)
030250                 MOVE SPACE           TO 部位区切Ｗ(部位ＣＮＴ)
030260                 MOVE SPACE           TO 経過略称Ｗ(部位ＣＮＴ)
030270             NOT INVALID KEY
030280*
030290                 EVALUATE 部位ＣＮＴ
030300                 WHEN 1
030310                     MOVE NC"①" TO 経過部位Ｗ
030320                 WHEN 2
030330                     MOVE NC"②" TO 経過部位Ｗ
030340                 WHEN 3
030350                     MOVE NC"③" TO 経過部位Ｗ
030360                 WHEN 4
030370                     MOVE NC"④" TO 経過部位Ｗ
030380                 WHEN 5
030390                     MOVE NC"⑤" TO 経過部位Ｗ
030400                 END-EVALUATE
030410                 STRING  経過部位Ｗ     DELIMITED BY SPACE
030420                         経－経過略称   DELIMITED BY SPACE
030430                        INTO 印刷経過略称Ｗ(部位ＣＮＴ)
030440                 END-STRING
030450*
030460             END-READ
030470*
030480             MOVE 負－転帰区分(部位ＣＮＴ) TO 転帰区分Ｗ(部位ＣＮＴ)
030490             EVALUATE 負－転帰区分(部位ＣＮＴ)
030500             WHEN 1
030510             WHEN 2
030520                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
030530             WHEN 3
030540                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
030550             WHEN 4
030560                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
030570             END-EVALUATE
030580*
030590         END-PERFORM
033370* 新規/継続 チェック
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
030660*
030670* 枝番判定用
030680         MOVE 負－開始診療日手動区分   TO 開始診療日手動区分Ｗ
030690* 負傷原因印刷区分
030700         MOVE 負－レセ負傷原因印刷区分 TO レセ負傷原因印刷区分Ｗ
027880         MOVE 負－レセ長期理由印刷区分 TO レセ長期理由印刷区分Ｗ
030710*
030720     END-IF.
030730*================================================================*
030740*================================================================*
030750 施術記録取得 SECTION.
030760*
030770************************************************************
030780* 作１データから負傷データＦより以下の情報を取得           *
030790* ● 初検加算 .....区分によりチェックに"○"を格納...複数可 *
030800* ● 往療加算 .....区分によりチェックに"○"を格納...複数可 *
030810************************************************************
030820     MOVE  SPACE  TO  初日再検フラグ.
030830     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 部位数Ｗ
030840         IF ( 施術年Ｗ = 初検年Ｗ(部位ＣＮＴ) ) AND
030850            ( 施術月Ｗ = 初検月Ｗ(部位ＣＮＴ) )
030860             MOVE 患者番号ＷＲ          TO 施記－患者番号
030870             MOVE 枝番ＷＲ              TO 施記－枝番
030880             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
030890             MOVE 初検年Ｗ(部位ＣＮＴ)  TO 開始年Ｗ(部位ＣＮＴ) 施記－施術年
030900             MOVE 初検月Ｗ(部位ＣＮＴ)  TO 開始月Ｗ(部位ＣＮＴ) 施記－施術月
030910             MOVE 初検日Ｗ(部位ＣＮＴ)  TO 開始日Ｗ(部位ＣＮＴ) 施記－施術日
030920         ELSE
030930             MOVE 患者番号ＷＲ          TO 施記－患者番号
030940             MOVE 枝番ＷＲ              TO 施記－枝番
030950             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
030960             MOVE 施術年ＷＲ            TO 施記－施術年
030970             MOVE 施術月ＷＲ            TO 施記－施術月
030980             MOVE ZERO                  TO 施記－施術日
030990         END-IF
031000         START 施術記録Ｆ   KEY IS >= 施記－患者コード
031010                                      施記－施術和暦年月日
031020         END-START
031030         IF ( 状態キー = "00" )
031040             MOVE ZERO  TO 実日数Ｗ(部位ＣＮＴ)
031050             MOVE ZERO  TO 終了年ＷＴ
031060             MOVE ZERO  TO 終了月ＷＴ
031070             MOVE ZERO  TO 終了日ＷＴ
031080             MOVE SPACE TO 終了フラグ２
031090             PERFORM 施術記録Ｆ読込
031100             IF ( 終了フラグ２      = SPACE   ) AND
031110                ( 施記－患者コード  = 患者コードＷＲ ) AND
031120                ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
031130                ( 施記－施術年      = 施術年ＷＲ     ) AND
031140                ( 施記－施術月      = 施術月ＷＲ     ) 
031150*
031160*        *****************************************************************
031170*        * 開始年月日 ( その部位が当月初検でないか、
031180*                       当月初検でも枝番がある時は、最初の施術日を開始日)*
031190*        *****************************************************************
031200                 IF ( 施術年Ｗ NOT = 初検年Ｗ(部位ＣＮＴ) ) OR
031210                    ( 施術月Ｗ NOT = 初検月Ｗ(部位ＣＮＴ) ) OR
031220                    ( 開始診療日手動区分Ｗ = 1 )
031230                     MOVE 施記－施術年   TO 開始年Ｗ(部位ＣＮＴ)
031240                     MOVE 施記－施術月   TO 開始月Ｗ(部位ＣＮＴ)
031250                     MOVE 施記－施術日   TO 開始日Ｗ(部位ＣＮＴ)
031260                 END-IF
031270             END-IF
031280             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
031290                           ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
031300                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
031310                           ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
031320                           ( 施記－施術月     NOT = 施術月ＷＲ       ) OR
031330                           ( 施記－施術日         > 終了日Ｗ(部位ＣＮＴ))
031340*               **********
031350*               * 実日数 *
031360*               **********
      */料金が発生しない日はカウントしない/121024
                      IF (施記－整復施療区分  (部位ＣＮＴ) NOT = ZERO) OR
                         (施記－罨法区分      (部位ＣＮＴ) NOT = ZERO) OR
                         (施記－電療区分      (部位ＣＮＴ) NOT = ZERO) OR
                         (施記－後療料請求区分(部位ＣＮＴ) NOT = ZERO) OR
                         (施記－金属副子区分  (部位ＣＮＴ) NOT = ZERO) OR
                         (施記－情報提供区分  (部位ＣＮＴ) NOT = ZERO)
031370                    COMPUTE 実日数Ｗ(部位ＣＮＴ) = 実日数Ｗ(部位ＣＮＴ) + 1
                      END-IF
031980                MOVE 施記－施術和暦             TO 終了和暦ＷＴ
031380                MOVE 施記－施術年               TO 終了年ＷＴ
031390                MOVE 施記－施術月               TO 終了月ＷＴ
031400                MOVE 施記－施術日               TO 終了日ＷＴ
031410*
031420                PERFORM 施術記録Ｆ読込
031430            END-PERFORM
031440        END-IF
031160*       ********************************************************************
031170*       * 負傷が無病で、その部位が当月初検の時は、実日数を１にする/20150908*
031190*       ********************************************************************
031200        IF ( 施術年Ｗ = 初検年Ｗ(部位ＣＮＴ) ) AND
031210           ( 施術月Ｗ = 初検月Ｗ(部位ＣＮＴ) ) AND
                 ( 負－負傷種別(部位ＣＮＴ) = 9)
                  MOVE 1             TO 実日数Ｗ(部位ＣＮＴ)
              END-IF
031450*       **************************
031460*       * 継続：終了年月日セット *
031470*       **************************
031480        IF ( 転帰区分Ｗ(部位ＣＮＴ) = 9 )
032090            MOVE 終了和暦ＷＴ  TO 終了和暦Ｗ(部位ＣＮＴ)
031490            MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
031500            MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
031510            MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
031520        END-IF
031530        IF ( 終了年月日Ｗ(部位ＣＮＴ) > 受理年月日Ｗ )
032140            MOVE 終了和暦Ｗ(部位ＣＮＴ) TO 受理和暦Ｗ
031540            MOVE 終了年Ｗ(部位ＣＮＴ) TO 受理年Ｗ
031550            MOVE 終了月Ｗ(部位ＣＮＴ) TO 受理月Ｗ
031560            MOVE 終了日Ｗ(部位ＣＮＴ) TO 受理日Ｗ
031570        END-IF
031580     END-PERFORM.
031590*
031600** ----- 前月初検のみかを判定 -----------*
031610*
031620*     MOVE 患者番号ＷＲ          TO 施記－患者番号.
031630*     MOVE 枝番ＷＲ              TO 施記－枝番.
031640*     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
031650*     MOVE 施術年ＷＲ            TO 施記－施術年.
031660*     MOVE 施術月ＷＲ            TO 施記－施術月.
031670*     MOVE ZERO                  TO 施記－施術日.
031680*     START 施術記録Ｆ   KEY IS >= 施記－患者コード
031690*                                  施記－施術和暦年月日
031700*     END-START.
031710*     IF ( 状態キー = "00" )
031720*             MOVE SPACE TO 終了フラグ２
031730*             PERFORM 施術記録Ｆ読込
031740*             IF ( 終了フラグ２      = SPACE   ) AND
031750*                ( 施記－患者コード  = 患者コードＷＲ ) AND
031760*                ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
031770*                ( 施記－施術年      = 施術年ＷＲ     ) AND
031780*                ( 施記－施術月      = 施術月ＷＲ     ) 
031790** 当月施術開始日が再検かどうか判定
031800*                 IF ( 施記－再検料請求 = 1 )
031810*                      MOVE "YES"  TO  初日再検フラグ
031820*                 END-IF
031830**
031840*             END-IF
031850*     END-IF.
031860*     IF ( 初日再検フラグ = "YES" )
031870*        PERFORM 前月初検のみ判定
031880*     END-IF.
031890*
031900*================================================================*
031910*================================================================*
031920 レセプト並び順取得 SECTION.
031930*================================================================*
031940     MOVE 施術和暦ＷＲ       TO 作４－施術和暦.
031950     MOVE 施術年ＷＲ         TO 作４－施術年.
031960     MOVE 施術月ＷＲ         TO 作４－施術月.
031970     MOVE 患者コードＷＲ     TO 作４－患者コード.
031980     MOVE 保険種別ＷＲ       TO 作４－保険種別.
031990     READ 作業ファイル４
032000     NOT INVALID KEY
032010          MOVE NC"№"        TO 順番固定Ｗ
032020          MOVE 作４－順番    TO 順番Ｗ
032030     END-READ.
032040*
032050*================================================================*
032060 施術記録Ｆ読込 SECTION.
032070*
032080     READ 施術記録Ｆ NEXT
032090     AT END
032100         MOVE "YES" TO 終了フラグ２
032110     END-READ.
032120*================================================================*
032130 印刷処理 SECTION.
032140*
032150     MOVE "YAZ6421P"  TO  定義体名Ｐ.
032160     MOVE "SCREEN"   TO  項目群名Ｐ.
032170     WRITE YAZ6421P.
032180***     WRITE 印刷レコード.
032190     PERFORM エラー処理Ｐ.
032200*================================================================*
032210 エラー処理Ｐ SECTION.
032220*
032230     IF ( 通知情報Ｐ NOT = "00" )
032240         DISPLAY NC"帳票エラー"              UPON CONS
032250         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
032260         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
032270         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
032280         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
032290                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
032300         ACCEPT  キー入力 FROM CONS
032310         PERFORM ファイル閉鎖
032320         MOVE 99 TO PROGRAM-STATUS
032330         EXIT PROGRAM
032340     END-IF.
032350*================================================================*
032360 部位名称埋込処理 SECTION.
032370*
006490     STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
009980            負傷名称Ｗ                    DELIMITED BY SPACE
006500            レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
006520       INTO 負傷名Ｗ(部位ＣＮＴ)
006570     END-STRING.
032570*
033290*================================================================*
033300 長期判定取得 SECTION.
033310*
033320* ３カ月以上の長期判定は "CHOUKI" を呼ぶ. 
033330     MOVE  SPACE TO  連期間－キー.
033340     INITIALIZE      連期間－キー.
033350     MOVE 施術和暦ＷＲ  TO  連期間－施術和暦.
033360     MOVE 施術年ＷＲ    TO  連期間－施術年.
033370     MOVE 施術月ＷＲ    TO  連期間－施術月.
033380     MOVE 患者番号ＷＲ  TO  連期間－患者番号.
033390     MOVE 枝番ＷＲ      TO  連期間－枝番.
033400*
033410     CALL   "CHOUKI".
033420     CANCEL "CHOUKI".
033430*
033440**** 適用１を使用 (「前月初検のみ」がある時は、くっつける)
033450     IF ( 連期間－対象フラグ  = "YES" )
033460        IF ( 適用１Ｗ  = SPACE )
033470           MOVE NC"※長期施術継続理由裏面に記載"  TO 適用１Ｗ
033480        ELSE
033490           STRING 適用１Ｗ           DELIMITED BY SPACE
033500                  NC"，"             DELIMITED BY SIZE
033510                  NC"※長期施術継続理由裏面に記載"   DELIMITED BY SIZE
033520                  INTO 適用１Ｗ
033530           END-STRING
033540        END-IF
033550     END-IF.
033560*
033570*================================================================*
033580 初検加算時刻取得 SECTION.
033590*****************************************************************
033600** 初検加算が時間外と深夜の時、適用に「受付時間」を印字する。
033610**   時刻の印字は月3回まで可能
033620*****************************************************************
033630     IF ( レセ－時間外 = 1 ) OR ( レセ－深夜 = 1 ) OR ( レセ－休日 = 1 )
033640*
033650         MOVE 患者番号ＷＲ          TO 施記－患者番号
033660         MOVE 枝番ＷＲ              TO 施記－枝番
033670         MOVE 施術和暦ＷＲ          TO 施記－施術和暦
033680         MOVE 施術年ＷＲ            TO 施記－施術年
033690         MOVE 施術月ＷＲ            TO 施記－施術月
033700         MOVE ZERO                  TO 施記－施術日
033710         START 施術記録Ｆ   KEY IS >= 施記－患者コード
033720                                      施記－施術和暦年月日
033730         END-START
033740         IF ( 状態キー = "00" )
033750             MOVE ZERO  TO 初検加算カウント
033760             MOVE SPACE TO 終了フラグ２
033770             PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
033780                           ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
033790                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
033800                           ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
033810                           ( 施記－施術月     NOT = 施術月ＷＲ      ) 
033820               IF ( 施記－初検加算 = 1 OR 2 OR 3 ) AND ( 施記－診療区分 = 2 )
033830                  COMPUTE 初検加算カウント = 初検加算カウント  + 1
037200                  IF  初検加算カウント <= 3
037210                      MOVE 施記－初検加算 TO 初検加算区分ＷＴ(初検加算カウント)
037220                      MOVE 施記－受付時   TO 初検加算時ＷＴ(初検加算カウント)
037230                      MOVE 施記－受付分   TO 初検加算分ＷＴ(初検加算カウント)
033880                  END-IF
033890               END-IF
033900               PERFORM 施術記録Ｆ読込
033910            END-PERFORM
037280** 初検加算の時刻をセット
033380            IF ( 初検加算時ＷＴ(1) NOT = ZERO ) OR ( 初検加算分ＷＴ(1) NOT = ZERO ) 
                      MOVE 初検加算時ＷＴ(1) TO 初検加算時Ｗ
                      MOVE ":"               TO 初検加算区切Ｗ
                      MOVE 初検加算分ＷＴ(1) TO 初検加算分Ｗ
                  END-IF
033380            IF ( 初検加算時ＷＴ(2) NOT = ZERO ) OR ( 初検加算分ＷＴ(2) NOT = ZERO ) 
031910               PERFORM 初検加算適用セット
                  END-IF
033940         END-IF
033950*
033960     END-IF.
033970*
037340*================================================================*
037350 初検加算適用セット SECTION.
037360*
037370     PERFORM VARYING 番号カウンタ FROM 1 BY 1
037380              UNTIL  番号カウンタ > 3
037390         IF ( 初検加算時ＷＴ(番号カウンタ)  = ZERO )  AND 
037400            ( 初検加算分ＷＴ(番号カウンタ)  = ZERO ) 
037410             CONTINUE
037420         ELSE
037430* 固定項目
037440             EVALUATE 初検加算区分ＷＴ(番号カウンタ) 
037450             WHEN 1
037460                MOVE NC"時間外"   TO 加算内容Ｗ(番号カウンタ)
033320             WHEN 2
033330                MOVE NC"休　日"   TO 加算内容Ｗ(番号カウンタ)
037470             WHEN 3
037480                MOVE NC"深　夜"   TO 加算内容Ｗ(番号カウンタ)
037490             END-EVALUATE
037500*
037510             MOVE NC"："          TO 加算区切Ｗ(番号カウンタ)
037520             MOVE NC"時"          TO 時固定Ｗ(番号カウンタ)
037530             MOVE NC"分"          TO 分固定Ｗ(番号カウンタ)
037540*
037550**** 数字→日本語変換
037560* 時間
037570             MOVE 初検加算時ＷＴ(番号カウンタ)  TO  数字Ｗ
037580             IF 数字Ｗ >= 10
037590                 MOVE 数字Ｗ１    TO 負傷番号Ｗ１
037600                 PERFORM 日本語変換
037610                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ１(番号カウンタ)
037620                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
037630                 PERFORM 日本語変換
037640                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
037650             ELSE
037660                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
037670                 PERFORM 日本語変換
037680                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
037690             END-IF
037700* 分
037710             MOVE 初検加算分ＷＴ(番号カウンタ)  TO  数字Ｗ
037720             MOVE 数字Ｗ１    TO 負傷番号Ｗ１
037730             PERFORM 日本語変換
037740             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ１(番号カウンタ)
037750             MOVE 数字Ｗ２    TO 負傷番号Ｗ１
037760             PERFORM 日本語変換
037770             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ２(番号カウンタ)
037780** 
037790        END-IF
037800     END-PERFORM.
037810*
037820     MOVE  初検加算集団ＮＷ(1)   TO 初検加算時刻１Ｗ. 
037830     MOVE  初検加算集団ＮＷ(2)   TO 初検加算時刻２Ｗ. 
037840     MOVE  初検加算集団ＮＷ(3)   TO 初検加算時刻３Ｗ. 
037850*
037860**** 適用１か２を使用（長期理由記載で適用１を使っている時は、適用２）
037870     IF ( 初検加算時ＷＴ(2)  = ZERO ) AND ( 初検加算分ＷＴ(2)  = ZERO ) 
037880         CONTINUE
037890     ELSE
037900         IF 適用１Ｗ  = SPACE
037910               STRING NC"初検加算"       DELIMITED BY SIZE
037920                      初検加算時刻１Ｗ   DELIMITED BY SIZE
037930                      初検加算時刻２Ｗ   DELIMITED BY SIZE
037940                      初検加算時刻３Ｗ   DELIMITED BY SIZE
037950                      INTO 適用１Ｗ
037960               END-STRING
037970         ELSE
033830               STRING 適用１Ｗ           DELIMITED BY SPACE
036850                      NC"，"             DELIMITED BY SIZE
036860                      NC"初検加算"       DELIMITED BY SIZE
033840                      初検加算時刻１Ｗ   DELIMITED BY SIZE
033850                      初検加算時刻２Ｗ   DELIMITED BY SIZE
033860                      初検加算時刻３Ｗ   DELIMITED BY SIZE
033870                      INTO 適用１Ｗ
038030               END-STRING
038040         END-IF
038050     END-IF.
038060*
038070*================================================================*
038080 日本語変換 SECTION.
038090*
038100     MOVE NC"０"     TO 全角負傷番号Ｗ.
038110     CALL "htoz" WITH C LINKAGE
038120                        USING 負傷番号Ｗ１ 全角負傷番号Ｗ１.
038130*
034790*================================================================*
034800 負傷原因取得 SECTION.
034810*
034820********************************************************************
034830*  負傷原因コードが同じものは、1行にまとめて印字する。
034840*  例: ①② 家で転んだ.
034850*     負傷原因コードが同じものをまとめ、テーブルにセット
034860*     (ただし、部位を飛んで同じものは、2行になる)
034870********************************************************************
034880     MOVE  ZERO   TO  カウンタ カウンタ２.
034890     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
034900             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
034910*
034920****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
034930        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
034940*
034950           IF ( カウンタ = ZERO )
034960               MOVE 1   TO  カウンタ カウンタ２
034970               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
034980               MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
034990               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
035000           ELSE
035010              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
035020                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
035030                 COMPUTE カウンタ２ = カウンタ２  +  1
035040                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
035050              ELSE
035060                 COMPUTE カウンタ = カウンタ  +  1
035070                 MOVE 1   TO  カウンタ２
035080                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
035090                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
035100                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
035110              END-IF
035120           END-IF
035130        END-IF
035140     END-PERFORM.
035150**************************************************************************
035160*  負傷原因マスタより文章取得
035170**************************************************************************
035180     MOVE  ZERO   TO  カウンタ カウンタ２.
035190     PERFORM VARYING カウンタ FROM 1 BY 1
035200             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
035210** 健保は 区分 01
035220         MOVE 01                        TO 負原－区分コード
035230         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
035240         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
035250         READ 負傷原因Ｆ
035260         NOT INVALID KEY
035270             INITIALIZE 負傷原因ＷＴ
035280             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
035290             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
035300             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
035310             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
035320             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
035330             PERFORM VARYING カウンタ２ FROM 1 BY 1
035340                     UNTIL ( カウンタ２ > 9 )  OR 
035350                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
035360                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
035370                WHEN 1
035380                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
035390                WHEN 2
035400                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
035410                WHEN 3
035420                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
035430                WHEN 4
035440                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
035450                WHEN 5
035460                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
035430                WHEN 6
035440                   MOVE "⑥"  TO  負傷原因ナンバーＷ１(カウンタ２)
035450                WHEN 7
035460                   MOVE "⑦"  TO  負傷原因ナンバーＷ１(カウンタ２)
035470                WHEN OTHER
035480                   CONTINUE
035490                END-EVALUATE
035500             END-PERFORM
035510*
035520             IF 負原－負傷原因入力区分 = 1
035530                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
035540                        負傷原因１ＷＴ  DELIMITED BY SIZE
035550                        負傷原因２ＷＴ  DELIMITED BY SIZE
035560                        負傷原因３ＷＴ  DELIMITED BY SIZE
035570                        負傷原因４ＷＴ  DELIMITED BY SIZE
035580                        負傷原因５ＷＴ  DELIMITED BY SIZE
035590                        INTO 負傷原因内容合成Ｗ(カウンタ)
035600                 END-STRING
035610             ELSE
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
035700             END-IF
035710*
035720         END-READ
035730     END-PERFORM.
035740*
035750*     PERFORM 負傷原因セット.
035680     PERFORM 全負傷原因合体セット.
035760*
035770*================================================================*
035780 負傷原因セット SECTION.
035790*
035800**************************************************************************
035810*  文章が1行を超える時は、複数行に分解する。
035820**************************************************************************
035830     MOVE  ZERO   TO  カウンタ カウンタ２.
035840     PERFORM VARYING カウンタ FROM 1 BY 1
035850             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
035860*
035870          INITIALIZE 負傷原因内容分解ＸＷ
035880          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
035890          IF ( 負傷原因内容１ＸＷ  NOT = SPACE )
035900              COMPUTE カウンタ２ = カウンタ２  +  1
035910              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
035920          END-IF
035930          IF ( 負傷原因内容２ＸＷ  NOT = SPACE )
035940              COMPUTE カウンタ２ = カウンタ２  +  1
035950              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
035960          END-IF
035970          IF ( 負傷原因内容３ＸＷ  NOT = SPACE )
035980              COMPUTE カウンタ２ = カウンタ２  +  1
035990              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
036000          END-IF
034690          IF  負傷原因内容４ＸＷ  NOT = SPACE
034700              COMPUTE カウンタ２ = カウンタ２  +  1
034710              MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
034720          END-IF
036010*
036020     END-PERFORM.
035700*================================================================*
035710 全負傷原因合体セット SECTION.
035720*
035730**************************************************************************
035740*  文章が1行を超える時は、複数行に分解する。
035750**************************************************************************
           MOVE 負傷原因内容合成Ｗ(1) TO 文字１Ｗ.
007270     PERFORM VARYING カウンタ FROM 2 BY 1
007280             UNTIL ( カウンタ > 9 )  OR  ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
               MOVE 負傷原因内容合成Ｗ(カウンタ) TO 文字２Ｗ
006966         CALL プログラム名Ｗ WITH C LINKAGE
006967                             USING BY REFERENCE 文字１Ｗ
006968                                   BY REFERENCE 文字２Ｗ
           END-PERFORM.
035760     MOVE  文字１Ｗ   TO  負傷原因１文Ｗ.
035760     MOVE  ZERO   TO  カウンタ.
035770     PERFORM VARYING カウンタ FROM 1 BY 1
035780             UNTIL ( カウンタ > 7 )
035790*
035910        MOVE 負傷原因１文ＷＰ(カウンタ)  TO 負傷原因Ｗ(カウンタ)
035980*
035990     END-PERFORM.
036000*
036030*================================================================*
036040 助成印取得 SECTION.
036050*
036060* 2006/04 変更
036070* 助成印は "JOSEIMEI" を呼ぶ. 
036080     MOVE SPACE TO  連助成名称－キー.
036090     INITIALIZE     連助成名称－キー.
036100     MOVE 助成種別ＷＲ           TO 連助成名称－助成種別.
036110     MOVE 費用負担者番号助成ＷＲ TO 連助成名称－費用負担者番号助成.
036120*
036130     CALL   "JOSEIMEI".
036140     CANCEL "JOSEIMEI".
036150*
036160     MOVE 連助成名称－１文字 TO 助成印Ｗ.
036170*
036430*
036440*================================================================*
036450 前月初検のみ判定 SECTION.
036460*
036470*** 前月の通院日が初検か判定 
036480     MOVE  SPACE            TO 前月フラグ.
036490     MOVE 受－患者コード    TO 施記－患者コード.
036500     MOVE 受－施術和暦      TO 施記－施術和暦.
036510     MOVE 受－施術年        TO 施記－施術年.
036520     MOVE 受－施術月        TO 施記－施術月.
036530     MOVE 1                 TO 施記－施術日.
036540     START 施術記録Ｆ   KEY IS <  施記－患者コード
036550                                  施記－施術和暦年月日
036560                                  REVERSED
036570     END-START.
036580     IF ( 状態キー = "00" )
036590         MOVE SPACE  TO 終了フラグ２
036600         PERFORM 施術記録Ｆ読込
036610         IF ( 終了フラグ２      = SPACE  ) AND
036620            ( 施記－患者コード  = 受－患者コード ) AND
036630            ( 施記－診療区分    = 2 ) 
036640*
036650            PERFORM 前月判定
036660**** 適用１を使用
036670            IF ( 前月フラグ = "YES" )
036680               MOVE NC"※前月初検のみ"    TO  適用１Ｗ
036690            END-IF
036700**
036710         END-IF
036720     END-IF.
036730*
036740*================================================================*
036750 前月判定  SECTION.
036760* 
036770*** 読み込んだ施術記録の年月が、前月かどうか判定 (年月の差が 1 か?)
036780      MOVE  SPACE  TO  前月フラグ.
036790      INITIALIZE  計算年月日Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
036800**
036810      MOVE 受－施術和暦    TO 終了和暦２Ｗ.
036820      MOVE 受－施術年      TO 終了年２Ｗ.
036830      MOVE 受－施術月      TO 終了月２Ｗ.
036840      MOVE 施記－施術和暦  TO 開始和暦２Ｗ.
036850      MOVE 施記－施術年    TO 開始年２Ｗ.
036860      MOVE 施記－施術月    TO 開始月２Ｗ.
036870*
036880      EVALUATE TRUE
036890       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ = 終了年２Ｗ)
036900            PERFORM  前月比較月
036910       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ NOT = 終了年２Ｗ)
036920            PERFORM  前月比較年
036930       WHEN  開始和暦２Ｗ NOT = 終了和暦２Ｗ 
036940            PERFORM  前月比較元号
036950      END-EVALUATE.
036960*
036970      IF ( 計算月Ｗ = 1 )
036980         MOVE  "YES"  TO  前月フラグ
036990      END-IF.
037000*
037010*================================================================*
037020 前月比較月  SECTION.
037030*
037040     IF ( 終了月２Ｗ >  開始月２Ｗ )
037050         COMPUTE 計算月Ｗ = 終了月２Ｗ - 開始月２Ｗ
037060     ELSE
037070        MOVE ZERO TO 計算月Ｗ
037080     END-IF.
037090*
037100*================================================================*
037110 前月比較年  SECTION.
037120*
037130     IF ( 終了年２Ｗ >  開始年２Ｗ )
037140         COMPUTE 計算年Ｗ = 終了年２Ｗ - 開始年２Ｗ
037150         COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
037160     ELSE
037170        MOVE ZERO TO 計算月Ｗ
037180     END-IF.
037190*
037200*================================================================*
037210 前月比較元号  SECTION.
037220*
037230     MOVE 開始和暦２Ｗ TO 元－元号区分.
037240     READ 元号マスタ
037250     NOT INVALID KEY
037260         MOVE 元－開始西暦年 TO 開始西暦年Ｗ
037270     END-READ.
037280     MOVE 終了和暦２Ｗ TO 元－元号区分.
037290     READ 元号マスタ
037300     NOT INVALID KEY
037310         MOVE 元－開始西暦年 TO 終了西暦年Ｗ
037320     END-READ.
037330**
037340     IF ( 開始西暦年Ｗ NOT = ZERO ) AND ( 終了西暦年Ｗ NOT = ZERO )
037350        COMPUTE 開始西暦年Ｗ = 開始西暦年Ｗ + 開始年２Ｗ - 1
037360        COMPUTE 終了西暦年Ｗ = 終了西暦年Ｗ + 終了年２Ｗ - 1
037370*
037380        IF ( 終了西暦年Ｗ =  開始西暦年Ｗ )
037390           PERFORM  前月比較月
037400        ELSE
037410           IF ( 終了西暦年Ｗ >  開始西暦年Ｗ )
037420               COMPUTE 計算年Ｗ = 終了西暦年Ｗ - 開始西暦年Ｗ
037430               COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
037440           ELSE
037450               MOVE ZERO TO 計算月Ｗ
037460           END-IF
037470        END-IF
037480     ELSE
037490        MOVE ZERO TO 計算月Ｗ
037500     END-IF.
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
039370*     MOVE 56            TO  連摘文－文桁数.
039370     MOVE 52            TO  連摘文－文桁数.
015000     IF (レセ長期理由印刷区分Ｗ NOT = 1 )
               MOVE 長期理由印刷区分Ｗ TO 連摘文－長期区分
           ELSE
               MOVE 1                  TO 連摘文－長期区分
015050     END-IF.
040710*
040720     CALL   "TEKIYBUN".
040730     CANCEL "TEKIYBUN".
040740*
037680*================================================================*
037690 受診者印刷区分更新 SECTION.
037700*
037710** //  受診者情報Ｆの印刷区分に１をセットし、更新する。//  
037720*
037730     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
037740     MOVE 施術年ＷＲ         TO 受－施術年.
037750     MOVE 施術月ＷＲ         TO 受－施術月.
037760     MOVE 患者コードＷＲ     TO 受－患者コード.
037770     READ 受診者情報Ｆ
037780     NOT INVALID KEY
037790         MOVE  1  TO  受－レセ印刷区分助成
037800         REWRITE  受－レコード
037810         END-REWRITE
037820         IF ( 状態キー NOT = "00" )
037830            MOVE NC"受診者" TO ファイル名
037840            PERFORM エラー表示
037850         END-IF
037860     END-READ.
037870*
037880*================================================================*
037890 月末日取得 SECTION.
037900*
037350     MOVE 施術和暦ＷＲ TO 受理和暦Ｗ.
037910     MOVE 施術年ＷＲ   TO 受理年Ｗ.
037920     MOVE 施術月ＷＲ   TO 受理月Ｗ.
037930     MOVE 施術和暦ＷＲ TO 元－元号区分.
037940     READ 元号マスタ
037950     NOT INVALID KEY
037960         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
037970     END-READ.
037980     IF ( 施術西暦年Ｗ NOT = ZERO )
037990        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
038000     END-IF.
038010*
038020     EVALUATE 施術月ＷＲ
038030     WHEN 4
038040     WHEN 6
038050     WHEN 9
038060     WHEN 11
038070         MOVE 30 TO 受理日Ｗ
038080     WHEN 2
038090         DIVIDE 4 INTO 施術西暦年Ｗ GIVING    商Ｗ
038100                                    REMAINDER 余Ｗ
038110         END-DIVIDE
038120         IF ( 余Ｗ = ZERO )
038130             MOVE 29 TO 受理日Ｗ
038140         ELSE
038150             MOVE 28 TO 受理日Ｗ
038160         END-IF
038170     WHEN 1
038180     WHEN 3
038190     WHEN 5
038200     WHEN 7
038210     WHEN 8
038220     WHEN 10
038230     WHEN 12
038240         MOVE 31 TO 受理日Ｗ
038250     WHEN OTHER
038260          CONTINUE
038270     END-EVALUATE.
038280*
038290*================================================================*
038300 委任年月日取得 SECTION.
038310*
038320** ---// ここの受理年には、最終通院日が入っている為、退避する //----
036770     MOVE 受理和暦Ｗ TO 最終通院和暦Ｗ.
038330     MOVE 受理年Ｗ   TO 最終通院年Ｗ.
038340     MOVE 受理月Ｗ   TO 最終通院月Ｗ.
038350     MOVE 受理日Ｗ   TO 最終通院日Ｗ.
038360***
038370* (柔整師側)
038380     EVALUATE レセプト日付区分Ｗ 
038390*    /  最終通院日 /
038400     WHEN ZERO
036850         MOVE 最終通院和暦Ｗ TO 柔整師和暦Ｗ
038410         MOVE 最終通院年Ｗ   TO 柔整師年Ｗ
038420         MOVE 最終通院月Ｗ   TO 柔整師月Ｗ
038430         MOVE 最終通院日Ｗ   TO 柔整師日Ｗ
038440*    /  月末日 /
038450     WHEN 1 
038460         PERFORM 月末日取得
036910         MOVE 受理和暦Ｗ     TO 柔整師和暦Ｗ
038470         MOVE 受理年Ｗ       TO 柔整師年Ｗ
038480         MOVE 受理月Ｗ       TO 柔整師月Ｗ
038490         MOVE 受理日Ｗ       TO 柔整師日Ｗ
038500*    /  印字なし /
038510     WHEN 9
036960         MOVE ZERO           TO 柔整師和暦Ｗ
038520         MOVE ZERO           TO 柔整師年Ｗ
038530         MOVE ZERO           TO 柔整師月Ｗ
038540         MOVE ZERO           TO 柔整師日Ｗ
038550*    /  その他は、最終通院日 /
038560     WHEN OTHER
037010         MOVE 最終通院和暦Ｗ TO 柔整師和暦Ｗ
038570         MOVE 最終通院年Ｗ   TO 柔整師年Ｗ
038580         MOVE 最終通院月Ｗ   TO 柔整師月Ｗ
038590         MOVE 最終通院日Ｗ   TO 柔整師日Ｗ
038600     END-EVALUATE.
038610**
038620* (患者側)
038630     EVALUATE レセプト患者日付区分Ｗ 
038640*    /  最終通院日 /
038650     WHEN ZERO
037100         MOVE 最終通院和暦Ｗ TO 患者委任和暦Ｗ
038660         MOVE 最終通院年Ｗ   TO 患者委任年Ｗ
038670         MOVE 最終通院月Ｗ   TO 患者委任月Ｗ
038680         MOVE 最終通院日Ｗ   TO 患者委任日Ｗ
038690*    /  月末日 /
038700     WHEN 1 
038710         PERFORM 月末日取得
037160         MOVE 受理和暦Ｗ     TO 患者委任和暦Ｗ
003872         MOVE 受理年Ｗ       TO 患者委任年Ｗ
038730         MOVE 受理月Ｗ       TO 患者委任月Ｗ
038740         MOVE 受理日Ｗ       TO 患者委任日Ｗ
038750*    /  印字なし /
038760     WHEN 9
037210         MOVE ZERO           TO 患者委任和暦Ｗ
038770         MOVE ZERO           TO 患者委任年Ｗ
038780         MOVE ZERO           TO 患者委任月Ｗ
038790         MOVE ZERO           TO 患者委任日Ｗ
038800*    /  その他は、最終通院日 /
038810     WHEN OTHER
037260         MOVE 最終通院和暦Ｗ TO 患者委任和暦Ｗ
038820         MOVE 最終通院年Ｗ   TO 患者委任年Ｗ
038830         MOVE 最終通院月Ｗ   TO 患者委任月Ｗ
038840         MOVE 最終通院日Ｗ   TO 患者委任日Ｗ
038850     END-EVALUATE.
038860*
039790*================================================================*
039800 レセ摘要再セット SECTION.
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
      *     MOVE 連摘文－摘要文(8)    TO 長期理由文８.
      *     MOVE 連摘文－摘要文(9)    TO 長期理由文９.
      *     MOVE 連摘文－摘要文(10)   TO 長期理由文１０.
040000*
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
040010*================================================================*
040011*================================================================*
040012 地域特有処理 SECTION.
040013*
040014*--------------------------------------------------------*
040015*  福岡県：経過欄の固定印字 (全柔ＦＰＤ区分Ｗ 1 使用)
040016*  長期以外の部位は、「順調」
040017*  長期の部位は、「緩慢」
040018*--------------------------------------------------------*
040019*
040020     IF 全柔ＦＰＤ区分Ｗ = 1
040021*      まず「順調」セット
040022        PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
040023                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
040024*
040025                 EVALUATE 部位ＣＮＴ
040026                 WHEN 1
040027                     MOVE NC"①" TO 経過部位数字Ｗ
040028                 WHEN 2
040029                     MOVE NC"②" TO 経過部位数字Ｗ
040030                 WHEN 3
040031                     MOVE NC"③" TO 経過部位数字Ｗ
040032                 WHEN 4
040033                     MOVE NC"④" TO 経過部位数字Ｗ
040034                 WHEN 5
040035                     MOVE NC"⑤" TO 経過部位数字Ｗ
040036                 END-EVALUATE
040037                 MOVE SPACE TO 経過略称(部位ＣＮＴ)
040038                 STRING  経過部位数字Ｗ   DELIMITED BY SPACE
040039                         NC"順調"         DELIMITED BY SPACE
040040                        INTO 経過略称(部位ＣＮＴ)
040041                 END-STRING
040042        END-PERFORM
040043*
040044*      次に、３カ月以上の長期判定
040045        MOVE  SPACE TO  連期間－キー
040046        INITIALIZE      連期間－キー
040047        MOVE 施術和暦ＷＲ  TO  連期間－施術和暦
040048        MOVE 施術年ＷＲ    TO  連期間－施術年
040049        MOVE 施術月ＷＲ    TO  連期間－施術月
040050        MOVE 患者番号ＷＲ  TO  連期間－患者番号
040051        MOVE 枝番ＷＲ      TO  連期間－枝番
040052        CALL   "CHOUKI"
040053        CANCEL "CHOUKI"
040054*
040055        IF 連期間－対象フラグ  = "YES"
040056           PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
040057                    UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
040058*
040059               IF 連期間－期間Ｗ(部位ＣＮＴ)  >  ZERO
040060
040061                   EVALUATE 部位ＣＮＴ
040062                   WHEN 1
040063                       MOVE NC"①" TO 経過部位数字Ｗ
040064                   WHEN 2
040065                       MOVE NC"②" TO 経過部位数字Ｗ
040066                   WHEN 3
040067                       MOVE NC"③" TO 経過部位数字Ｗ
040068                   WHEN 4
040069                       MOVE NC"④" TO 経過部位数字Ｗ
040070                   WHEN 5
040071                       MOVE NC"⑤" TO 経過部位数字Ｗ
040072                   END-EVALUATE
040073                   MOVE SPACE TO 経過略称(部位ＣＮＴ)
040074                   STRING  経過部位数字Ｗ   DELIMITED BY SPACE
040075                           NC"緩慢"         DELIMITED BY SPACE
040076                          INTO 経過略称(部位ＣＮＴ)
040077                   END-STRING
040078               END-IF
040079           END-PERFORM
040080        END-IF
040081*
040082     END-IF.
040083*
040084*
040085*================================================================*
040086*================================================================*
040087*================================================================*
040088 エラー表示 SECTION.
040089*
040090     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
040091     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
040092     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
040093     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
040100                                                   UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
040110     ACCEPT  キー入力 FROM CONS
040120     PERFORM ファイル閉鎖.
040130     EXIT PROGRAM.
040140*================================================================*
040150*================================================================*
040160 ファイル閉鎖 SECTION.
040170*
040180     CLOSE 印刷ファイル.
040190     CLOSE 保険者マスタ     元号マスタ          名称マスタ
040200           レセプトＦ       制御情報マスタ      施術所情報マスタ
040210           経過マスタ       受診者情報Ｆ        受診者情報２Ｆ
040220           施術記録Ｆ       負傷データＦ        負傷原因Ｆ
040230           ＩＤ管理マスタ   市町村マスタ
040240           作業ファイル４   請求先マスタ.
040250*================================================================*
040260 終了処理 SECTION.
040270*
040280     PERFORM ファイル閉鎖.
040290*================================================================*
040300*================================================================*
040310 テスト印字処理 SECTION.
040320*
           MOVE ALL "9" TO
           都道府県番号 施術月 施術年 患者年 患者月 患者日 開始年１ 開始月１ 開始日１ 終了年１ 
           終了月１ 終了日１ 負傷年１ 負傷月１ 負傷日１ 初検年１ 初検月１ 初検日１ 実日数１ 
           開始年２ 開始月２ 開始日２ 終了年２ 終了月２ 終了日２ 負傷年２ 負傷月２ 負傷日２ 
           初検年２ 初検月２ 初検日２ 実日数２ 開始年３ 開始月３ 開始日３ 終了年３ 終了月３ 
           終了日３ 負傷年３ 負傷月３ 負傷日３ 初検年３ 初検月３ 初検日３ 実日数３ 開始年４ 
           開始月４ 開始日４ 終了年４ 終了月４ 終了日４ 負傷年４ 負傷月４ 負傷日４ 初検年４ 
           初検月４ 初検日４ 実日数４ 開始年５ 開始月５ 開始日５ 終了年５ 終了月５ 終了日５ 
           負傷年５ 負傷月５ 負傷日５ 初検年５ 初検月５ 初検日５ 実日数５ 初検料 初検時相談料 
           往療距離 再検料 金属副子加算料 往療回数 往療料 小計 初検加算料 施術情報提供料 
           往療加算料 初検加算時 初検加算分 初検加算区切 初回処置料(1) 初回処置料(2) 
           初回処置料(3) 初回処置料(4) 初回処置料(5) 初回処置料合計 後療単価１ 後療回数１ 
           後療料１ 冷罨法回数１ 冷罨法料１ 温罨法回数１ 温罨法料１ 電療回数１ 電療料１ 小計１ 
           長期逓減率１ 長期込小計１ 後療単価２ 後療回数２ 後療料２ 冷罨法回数２ 冷罨法料２ 
           温罨法回数２ 温罨法料２ 電療回数２ 電療料２ 小計２ 長期逓減率２ 長期込小計２ 
           後療単価３８ 後療回数３８ 後療料３８ 冷罨法回数３８ 冷罨法料３８ 温罨法回数３８ 
           温罨法料３８ 電療回数３８ 電療料３８ 小計３８ 多部位込小計３８ 長期逓減率３８ 
           長期込小計３８ 逓減開始月３０ 逓減開始日３０ 後療単価３０ 後療回数３０ 後療料３０ 
           冷罨法回数３０ 冷罨法料３０ 温罨法回数３０ 温罨法料３０ 電療回数３０ 電療料３０ 小計３０
           長期逓減率３０ 長期込小計３０ 逓減開始月４８ 逓減開始日４８ 後療単価４８ 後療回数４８ 
           後療料４８ 冷罨法回数４８ 冷罨法料４８ 温罨法回数４８ 温罨法料４８ 電療回数４８ 電療料４８ 
           小計４８ 多部位込小計４８ 長期逓減率４８ 長期込小計４８ 逓減開始月４０ 逓減開始日４０ 
           後療単価４０ 後療回数４０ 後療料４０ 冷罨法回数４０ 冷罨法料４０ 温罨法回数４０ 温罨法料４０ 
           電療回数４０ 電療料４０ 小計４０ 長期逓減率４０ 長期込小計４０ 合計 一部負担金 請求金額 
           受理年 受理月 受理日 委任年 委任月 委任日 受給者負担額 助成請求額
           .
           MOVE ALL "X" TO
           共済番号 地共済番号 県施術ＩＤ 保険者番号 記号番号 公費負担者番号 受給者番号 住所１ 住所２ 
           柔整師番号 口座番号 接骨師会会員番号
      *     金融機関名１ 金融機関名２ 金融機関名３ 金融機関名４ 支店名１ 支店名２ 支店名３ 支店名４
      *     口座名義人カナ１ 口座名義人
           施術所郵便番号１ 施術所郵便番号２ 
           施術所住所１ 施術所住所２ 施術所電話番号 代表者カナ 代表者名 保険者名称 保険者名称２
           負傷原因１ 負傷原因２ 負傷原因３ 負傷原因４ 負傷原因５ 負傷原因６ 負傷原因７ 負傷原因８
           長期理由文１ 長期理由文２ 長期理由文３ 長期理由文４ 長期理由文５
           長期理由文６ 長期理由文７ 部位５８２ 部位５０２
           接骨院名 代表者名 被保険者氏名 患者氏名 適用２ 長期頻回 
           .
           MOVE ALL NC"Ｎ" TO
           負傷名１ 負傷名２ 負傷名３ 負傷名４ 負傷名５ 経過略称(1) 経過略称(2) 経過略称(3) 
           経過略称(4) 経過略称(5) 適用１
           .
           MOVE NC"○" TO
           単独チェック 本人チェック 高一チェック 共済チェック 自チェック 社保チェック 
           組合チェック １０割チェック ９割チェック ２併チェック ６歳チェック ８割チェック 
           ７割チェック 後期チェック 退職チェック 国保チェック 家族チェック 高７チェック 
           男チェック 女チェック 生和暦チェック１ 生和暦チェック２ 生和暦チェック３ 生和暦チェック４
           治癒チェック１ 中止チェック１ 転医チェック１ 治癒チェック２ 中止チェック２ 
           転医チェック２ 治癒チェック３ 中止チェック３ 転医チェック３ 治癒チェック４ 
           中止チェック４ 転医チェック４ 治癒チェック５ 中止チェック５ 転医チェック５ 新規チェック 
           継続チェック 深夜チェック 時間外チェック 休日チェック 固定料チェック 整復料チェック 
           施療料チェック 夜間チェック 暴風雨雪チェック 難路チェック 
      *     普通チェック 振込チェック 当座チェック 銀行チェック 金庫チェック 農協チェック 
      *     本店チェック 支店チェック 本支所チェック
           .
041760*
041770*================================================================*
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
       口座番号取得 SECTION.
      *
           EVALUATE 受－費用負担者番号助成
      */平成25年11月施術分より変更↓↓↓/131126
           WHEN "83121095"   MOVE "5610002" TO 口座番号Ｗ
           WHEN "120014"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120022"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120030"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120048"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120055"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120063"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120071"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120089"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120097"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120105"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120113"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120121"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120139"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120147"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120162"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120170"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120188"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120196"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120204"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120212"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120220"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120238"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120246"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120253"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120261"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120451"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120519"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120527"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120535"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120543"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120550"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120568"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120576"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120584"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120592"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120600"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120618"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120626"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120634"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120642"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120659"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120667"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120675"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120683"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120691"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120709"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120717"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120725"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120733"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120741"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120758"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120766"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120774"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120782"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120790"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120808"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120816"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120824"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120832"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120840"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120857"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120865"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120873"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120881"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120899"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120907"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120915"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120923"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120931"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120949"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120956"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120964"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120972"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120980"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "120998"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "121004"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "121012"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "121020"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "121038"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "121046"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "124008"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "124016"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "124024"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "124032"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "124040"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "124057"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "124065"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "123018"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "123026"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "123034"     MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120014"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120022"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120030"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120048"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120055"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120063"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120071"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120089"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120097"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120105"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120113"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120121"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120139"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120147"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120154"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120162"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120170"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120188"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120196"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120204"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120212"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120220"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120238"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120246"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120253"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120261"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120519"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120527"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120535"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120543"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120550"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120568"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120576"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120584"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120592"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120600"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120618"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120626"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120634"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120642"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120659"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120667"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120675"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120683"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120691"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120709"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120717"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120725"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120733"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120741"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120758"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120766"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120774"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120782"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120790"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120808"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120816"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120824"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120832"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120840"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120857"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120865"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120873"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120881"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120899"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120907"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120915"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120923"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120931"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120949"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120956"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120964"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120972"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120980"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67120998"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67121004"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67121012"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67121020"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67121038"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67121046"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67124016"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67124024"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67124032"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67124040"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67124057"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "67124065"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120013"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120021"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120039"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120047"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120054"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120062"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120070"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120088"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120096"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120104"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120112"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120120"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120138"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120146"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120153"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120161"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120179"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120187"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120195"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120203"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120211"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120229"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120237"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120245"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120252"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120260"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120419"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120518"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120526"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120534"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120542"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120559"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120567"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120575"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120583"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120591"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120609"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120617"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120625"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120633"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120641"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120658"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120666"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120674"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120682"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120690"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120708"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120716"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120724"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120732"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120740"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120757"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120765"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120773"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120781"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120799"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120807"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120815"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120823"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120831"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120849"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120856"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120864"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120872"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120880"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120898"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120906"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120914"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120922"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120930"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120948"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120955"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120963"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120971"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120989"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27120997"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27121003"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27121011"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27121029"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27121037"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27121045"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27124007"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27124015"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27124023"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27124031"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27124049"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27124056"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "27124064"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120023"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120031"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120049"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120056"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120064"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120072"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120080"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120098"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120106"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120114"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120122"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120130"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120148"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120155"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120163"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120171"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120189"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120197"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120205"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120213"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120221"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120239"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120247"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120254"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120262"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120510"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120528"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120536"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120544"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120551"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120569"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120577"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120585"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120593"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120601"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120619"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120627"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120635"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120643"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120650"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120668"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120676"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120684"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120692"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120700"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120718"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120726"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120734"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120742"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120759"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120767"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120775"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120783"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120791"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120809"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120817"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120825"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120833"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120841"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120858"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120866"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120874"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120882"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120890"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120908"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120916"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120924"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120932"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120940"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120957"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120965"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120973"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120981"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41120999"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41121005"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41121013"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41121021"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41121039"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41121047"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41124009"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41124017"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41124025"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41124033"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41124041"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41124058"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "41124066"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "19126010"   MOVE "5610010" TO 口座番号Ｗ
           WHEN "110684"     MOVE "5610029" TO 口座番号Ｗ
           WHEN "110700"     MOVE "5610037" TO 口座番号Ｗ
           WHEN "110783"     MOVE "5610045" TO 口座番号Ｗ
           WHEN "138529"     MOVE "5610053" TO 口座番号Ｗ
           WHEN "138586"     MOVE "5610061" TO 口座番号Ｗ
           WHEN "114108"     MOVE "5610088" TO 口座番号Ｗ
           WHEN "06139521"   MOVE "5610096" TO 口座番号Ｗ
           WHEN "120279"     MOVE "5610110" TO 口座番号Ｗ
           WHEN "110015"     MOVE "5610126" TO 口座番号Ｗ
           WHEN "110023"     MOVE "5610134" TO 口座番号Ｗ
           WHEN "110031"     MOVE "5610142" TO 口座番号Ｗ
           WHEN "67110031"   MOVE "5610142" TO 口座番号Ｗ
           WHEN "110080"     MOVE "5610150" TO 口座番号Ｗ
           WHEN "110098"     MOVE "5610169" TO 口座番号Ｗ
           WHEN "110106"     MOVE "5610177" TO 口座番号Ｗ
           WHEN "06210595"   MOVE "5610185" TO 口座番号Ｗ
           WHEN "110148"     MOVE "5610193" TO 口座番号Ｗ
           WHEN "110213"     MOVE "5610207" TO 口座番号Ｗ
           WHEN "110221"     MOVE "5610215" TO 口座番号Ｗ
           WHEN "110296"     MOVE "5610223" TO 口座番号Ｗ
           WHEN "110346"     MOVE "5610231" TO 口座番号Ｗ
           WHEN "110353"     MOVE "5610258" TO 口座番号Ｗ
           WHEN "110361"     MOVE "5610266" TO 口座番号Ｗ
           WHEN "110379"     MOVE "5610274" TO 口座番号Ｗ
           WHEN "110403"     MOVE "5610282" TO 口座番号Ｗ
           WHEN "110411"     MOVE "5610290" TO 口座番号Ｗ
           WHEN "110429"     MOVE "5610304" TO 口座番号Ｗ
           WHEN "67110429"   MOVE "5610304" TO 口座番号Ｗ
           WHEN "27110428"   MOVE "5610304" TO 口座番号Ｗ
           WHEN "41110420"   MOVE "5610304" TO 口座番号Ｗ
           WHEN "110437"     MOVE "5610312" TO 口座番号Ｗ
           WHEN "06380257"   MOVE "5610320" TO 口座番号Ｗ
           WHEN "06400113"   MOVE "5610339" TO 口座番号Ｗ
           WHEN "06401095"   MOVE "5610347" TO 口座番号Ｗ
           WHEN "110841"     MOVE "5610355" TO 口座番号Ｗ
           WHEN "110890"     MOVE "5610363" TO 口座番号Ｗ
           WHEN "110908"     MOVE "5610371" TO 口座番号Ｗ
           WHEN "06330393"   MOVE "5610398" TO 口座番号Ｗ
           WHEN "06340061"   MOVE "5610401" TO 口座番号Ｗ
           WHEN "138016"     MOVE "5610428" TO 口座番号Ｗ
           WHEN "138024"     MOVE "5610436" TO 口座番号Ｗ
           WHEN "138032"     MOVE "5610444" TO 口座番号Ｗ
           WHEN "138040"     MOVE "5610452" TO 口座番号Ｗ
           WHEN "138057"     MOVE "5610460" TO 口座番号Ｗ
           WHEN "138065"     MOVE "5610479" TO 口座番号Ｗ
           WHEN "138073"     MOVE "5610487" TO 口座番号Ｗ
           WHEN "138081"     MOVE "5610495" TO 口座番号Ｗ
           WHEN "138099"     MOVE "5610509" TO 口座番号Ｗ
           WHEN "138107"     MOVE "5610517" TO 口座番号Ｗ
           WHEN "138115"     MOVE "5610525" TO 口座番号Ｗ
           WHEN "138123"     MOVE "5610533" TO 口座番号Ｗ
           WHEN "138131"     MOVE "5610541" TO 口座番号Ｗ
           WHEN "138149"     MOVE "5610568" TO 口座番号Ｗ
           WHEN "138156"     MOVE "5610576" TO 口座番号Ｗ
           WHEN "67138156"   MOVE "5610576" TO 口座番号Ｗ
           WHEN "138164"     MOVE "5610584" TO 口座番号Ｗ
           WHEN "138172"     MOVE "5610592" TO 口座番号Ｗ
           WHEN "138180"     MOVE "5610606" TO 口座番号Ｗ
           WHEN "138198"     MOVE "5610614" TO 口座番号Ｗ
           WHEN "67138198"   MOVE "5610614" TO 口座番号Ｗ
           WHEN "138206"     MOVE "5610622" TO 口座番号Ｗ
           WHEN "138214"     MOVE "5610630" TO 口座番号Ｗ
           WHEN "138222"     MOVE "5610649" TO 口座番号Ｗ
           WHEN "67138222"   MOVE "5610649" TO 口座番号Ｗ
           WHEN "27138221"   MOVE "5610649" TO 口座番号Ｗ
           WHEN "81136228"   MOVE "5610649" TO 口座番号Ｗ
           WHEN "81137226"   MOVE "5610649" TO 口座番号Ｗ
           WHEN "138230"     MOVE "5610657" TO 口座番号Ｗ
           WHEN "67138230"   MOVE "5610657" TO 口座番号Ｗ
           WHEN "27138239"   MOVE "5610657" TO 口座番号Ｗ
           WHEN "81136236"   MOVE "5610657" TO 口座番号Ｗ
           WHEN "81137234"   MOVE "5610657" TO 口座番号Ｗ
           WHEN "138248"     MOVE "5610665" TO 口座番号Ｗ
           WHEN "138313"     MOVE "5610673" TO 口座番号Ｗ
           WHEN "138321"     MOVE "5610681" TO 口座番号Ｗ
           WHEN "138347"     MOVE "5610703" TO 口座番号Ｗ
           WHEN "138354"     MOVE "5610711" TO 口座番号Ｗ
           WHEN "138396"     MOVE "5610738" TO 口座番号Ｗ
           WHEN "138479"     MOVE "5610746" TO 口座番号Ｗ
           WHEN "138487"     MOVE "5610754" TO 口座番号Ｗ
           WHEN "140038"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140046"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140053"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140061"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140079"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140087"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140095"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140103"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140111"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140129"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140137"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140145"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140152"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140160"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140178"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140186"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140517"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140525"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140533"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140541"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140558"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140566"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140574"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140582"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140590"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140608"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140616"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140624"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140632"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140640"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140657"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140665"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140673"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140681"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "140699"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "144006"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "144014"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "144022"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "144030"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "144048"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "144055"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "144063"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "144071"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "144089"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "144097"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "144105"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "144113"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "144121"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "144139"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "144147"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "144154"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "144162"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "144170"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "144188"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "145003"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "145011"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "145029"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "145037"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "145045"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "145052"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "145060"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "145078"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "146001"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "146019"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "146027"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "146035"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "143016"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "143024"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "143032"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "143040"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "143057"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "143065"     MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140038"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140046"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140053"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140061"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140079"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140087"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140095"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140103"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140111"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140129"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140137"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140145"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140152"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140160"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140178"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140186"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140517"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140525"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140533"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140541"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140558"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140566"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140574"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140582"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140590"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140608"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140616"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140624"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140632"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140640"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140657"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140665"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140673"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140681"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67140699"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67144014"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67144022"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67144030"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67144048"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67144055"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67144063"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67144071"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67144089"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67144097"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67144105"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67144113"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67144121"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67144139"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67144147"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67144154"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67144162"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67144170"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67144188"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67145011"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67145029"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67145037"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67145045"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67145060"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67145078"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67146019"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67146027"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "67146035"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140037"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140045"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140052"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140060"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140078"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140086"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140094"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140102"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140110"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140128"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140136"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140144"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140151"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140169"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140177"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140185"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140516"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140524"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140532"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140540"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140557"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140565"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140573"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140581"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140599"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140607"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140615"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140623"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140631"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140649"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140656"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140664"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140672"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140680"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27140698"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27144005"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27144013"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27144021"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27144039"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27144047"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27144054"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27144062"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27144070"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27144088"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27144096"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27144104"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27144112"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27144120"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27144138"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27144146"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27144153"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27144161"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27144179"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27144187"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27145002"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27145010"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27145028"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27145036"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27145044"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27145051"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27145069"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "27145077"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "1914601"    MOVE "5610762" TO 口座番号Ｗ
           WHEN "19146018"   MOVE "5610762" TO 口座番号Ｗ
           WHEN "113027"     MOVE "5610770" TO 口座番号Ｗ
           WHEN "113043"     MOVE "5610789" TO 口座番号Ｗ
           WHEN "113050"     MOVE "5610797" TO 口座番号Ｗ
           WHEN "113068"     MOVE "5610800" TO 口座番号Ｗ
           WHEN "133033"     MOVE "5610819" TO 口座番号Ｗ
           WHEN "133041"     MOVE "5610827" TO 口座番号Ｗ
           WHEN "133066"     MOVE "5610835" TO 口座番号Ｗ
           WHEN "133074"     MOVE "5610843" TO 口座番号Ｗ
           WHEN "133090"     MOVE "5610851" TO 口座番号Ｗ
           WHEN "133132"     MOVE "5610878" TO 口座番号Ｗ
           WHEN "133140"     MOVE "5610886" TO 口座番号Ｗ
           WHEN "133157"     MOVE "5610894" TO 口座番号Ｗ
           WHEN "133165"     MOVE "5610908" TO 口座番号Ｗ
           WHEN "133173"     MOVE "5610916" TO 口座番号Ｗ
           WHEN "133199"     MOVE "5610924" TO 口座番号Ｗ
           WHEN "133207"     MOVE "5610932" TO 口座番号Ｗ
           WHEN "133223"     MOVE "5610940" TO 口座番号Ｗ
           WHEN "133231"     MOVE "5610959" TO 口座番号Ｗ
           WHEN "133249"     MOVE "5610967" TO 口座番号Ｗ
           WHEN "133256"     MOVE "5610975" TO 口座番号Ｗ
           WHEN "133264"     MOVE "5610983" TO 口座番号Ｗ
           WHEN "133272"     MOVE "5610991" TO 口座番号Ｗ
           WHEN "133298"     MOVE "5611009" TO 口座番号Ｗ
           WHEN "67110106"   MOVE "5611017" TO 口座番号Ｗ
           WHEN "67110148"   MOVE "5611025" TO 口座番号Ｗ
           WHEN "67110213"   MOVE "5611033" TO 口座番号Ｗ
           WHEN "67110221"   MOVE "5611041" TO 口座番号Ｗ
           WHEN "06272512"   MOVE "5611068" TO 口座番号Ｗ
           WHEN "67110361"   MOVE "5611076" TO 口座番号Ｗ
           WHEN "67110387"   MOVE "5611084" TO 口座番号Ｗ
           WHEN "06272843"   MOVE "5611092" TO 口座番号Ｗ
           WHEN "67110841"   MOVE "5611106" TO 口座番号Ｗ
           WHEN "06272868"   MOVE "5611114" TO 口座番号Ｗ
           WHEN "06273262"   MOVE "5611122" TO 口座番号Ｗ
           WHEN "67138016"   MOVE "5611130" TO 口座番号Ｗ
           WHEN "67138024"   MOVE "5611149" TO 口座番号Ｗ
           WHEN "67138032"   MOVE "5611157" TO 口座番号Ｗ
           WHEN "67138040"   MOVE "5611165" TO 口座番号Ｗ
           WHEN "67138057"   MOVE "5611173" TO 口座番号Ｗ
           WHEN "67138065"   MOVE "5611181" TO 口座番号Ｗ
           WHEN "67138073"   MOVE "5611203" TO 口座番号Ｗ
           WHEN "67138081"   MOVE "5611211" TO 口座番号Ｗ
           WHEN "67138099"   MOVE "5611238" TO 口座番号Ｗ
           WHEN "67138107"   MOVE "5611246" TO 口座番号Ｗ
           WHEN "67138115"   MOVE "5611254" TO 口座番号Ｗ
           WHEN "67138123"   MOVE "5611262" TO 口座番号Ｗ
           WHEN "67138131"   MOVE "5611270" TO 口座番号Ｗ
           WHEN "67138149"   MOVE "5611289" TO 口座番号Ｗ
           WHEN "67138164"   MOVE "5611297" TO 口座番号Ｗ
           WHEN "67138172"   MOVE "5611300" TO 口座番号Ｗ
           WHEN "67138180"   MOVE "5611319" TO 口座番号Ｗ
           WHEN "67138206"   MOVE "5611327" TO 口座番号Ｗ
           WHEN "67138214"   MOVE "5611335" TO 口座番号Ｗ
           WHEN "67110023"   MOVE "5611343" TO 口座番号Ｗ
           WHEN "67110072"   MOVE "5611351" TO 口座番号Ｗ
           WHEN "67138313"   MOVE "5611378" TO 口座番号Ｗ
           WHEN "67138354"   MOVE "5611386" TO 口座番号Ｗ
           WHEN "67138362"   MOVE "5611394" TO 口座番号Ｗ
           WHEN "67138479"   MOVE "5611408" TO 口座番号Ｗ
           WHEN "67138487"   MOVE "5611416" TO 口座番号Ｗ
           WHEN "06141519"   MOVE "5611424" TO 口座番号Ｗ
           WHEN "67110098"   MOVE "5611432" TO 口座番号Ｗ
           WHEN "67110320"   MOVE "5611440" TO 口座番号Ｗ
           WHEN "67110445"   MOVE "5611459" TO 口座番号Ｗ
           WHEN "67110551"   MOVE "5611467" TO 口座番号Ｗ
           WHEN "06110084"   MOVE "5611475" TO 口座番号Ｗ
           WHEN "06110449"   MOVE "5611483" TO 口座番号Ｗ
           WHEN "06120018"   MOVE "5611491" TO 口座番号Ｗ
           WHEN "06120695"   MOVE "5611505" TO 口座番号Ｗ
           WHEN "63120695"   MOVE "5611505" TO 口座番号Ｗ
           WHEN "06120760"   MOVE "5611513" TO 口座番号Ｗ
           WHEN "06130173"   MOVE "5611521" TO 口座番号Ｗ
           WHEN "06130538"   MOVE "5611548" TO 口座番号Ｗ
           WHEN "06130835"   MOVE "5611556" TO 口座番号Ｗ
           WHEN "27110378"   MOVE "5611564" TO 口座番号Ｗ
           WHEN "06131320"   MOVE "5611580" TO 口座番号Ｗ
           WHEN "06271696"   MOVE "5611599" TO 口座番号Ｗ
           WHEN "06272587"   MOVE "5611602" TO 口座番号Ｗ
           WHEN "06273148"   MOVE "5611610" TO 口座番号Ｗ
           WHEN "06280838"   MOVE "5611629" TO 口座番号Ｗ
           WHEN "06281281"   MOVE "5611637" TO 口座番号Ｗ
           WHEN "06281448"   MOVE "5611645" TO 口座番号Ｗ
           WHEN "06281596"   MOVE "5611653" TO 口座番号Ｗ
           WHEN "06230023"   MOVE "5611661" TO 口座番号Ｗ
           WHEN "06230205"   MOVE "5611688" TO 口座番号Ｗ
           WHEN "06230221"   MOVE "5611696" TO 口座番号Ｗ
           WHEN "06230239"   MOVE "5611718" TO 口座番号Ｗ
           WHEN "06230395"   MOVE "5611726" TO 口座番号Ｗ
           WHEN "06230684"   MOVE "5611734" TO 口座番号Ｗ
           WHEN "06230692"   MOVE "5611742" TO 口座番号Ｗ
           WHEN "06230890"   MOVE "5611750" TO 口座番号Ｗ
           WHEN "06231518"   MOVE "5611769" TO 口座番号Ｗ
           WHEN "06231534"   MOVE "5611777" TO 口座番号Ｗ
           WHEN "06231591"   MOVE "5611785" TO 口座番号Ｗ
           WHEN "06231625"   MOVE "5611793" TO 口座番号Ｗ
           WHEN "06231773"   MOVE "5611807" TO 口座番号Ｗ
           WHEN "27138213"   MOVE "5611815" TO 口座番号Ｗ
           WHEN "81136210"   MOVE "5611815" TO 口座番号Ｗ
           WHEN "81137218"   MOVE "5611815" TO 口座番号Ｗ
           WHEN "88132212"   MOVE "5611815" TO 口座番号Ｗ
           WHEN "88138219"   MOVE "5611815" TO 口座番号Ｗ
           WHEN "67110726"   MOVE "5611823" TO 口座番号Ｗ
           WHEN "27138320"   MOVE "5611831" TO 口座番号Ｗ
           WHEN "27138353"   MOVE "5611858" TO 口座番号Ｗ
           WHEN "27138395"   MOVE "5611866" TO 口座番号Ｗ
           WHEN "27138478"   MOVE "5611874" TO 口座番号Ｗ
           WHEN "67138248"   MOVE "5611882" TO 口座番号Ｗ
           WHEN "67138255"   MOVE "5611890" TO 口座番号Ｗ
           WHEN "41140104"   MOVE "5611904" TO 口座番号Ｗ
           WHEN "80140106"   MOVE "5611904" TO 口座番号Ｗ
           WHEN "67138297"   MOVE "5611912" TO 口座番号Ｗ
           WHEN "67138305"   MOVE "5611920" TO 口座番号Ｗ
           WHEN "67138453"   MOVE "5611939" TO 口座番号Ｗ
           WHEN "67138602"   MOVE "5611947" TO 口座番号Ｗ
           WHEN "67145052"   MOVE "5611955" TO 口座番号Ｗ
           WHEN "110155"     MOVE "5611963" TO 口座番号Ｗ
           WHEN "110197"     MOVE "5611971" TO 口座番号Ｗ
           WHEN "110239"     MOVE "5611998" TO 口座番号Ｗ
           WHEN "110247"     MOVE "5612005" TO 口座番号Ｗ
           WHEN "110288"     MOVE "5612013" TO 口座番号Ｗ
           WHEN "110320"     MOVE "5612021" TO 口座番号Ｗ
           WHEN "110858"     MOVE "5612048" TO 口座番号Ｗ
           WHEN "110866"     MOVE "5612056" TO 口座番号Ｗ
           WHEN "110882"     MOVE "5612064" TO 口座番号Ｗ
           WHEN "110916"     MOVE "5612072" TO 口座番号Ｗ
           WHEN "67110916"   MOVE "5612072" TO 口座番号Ｗ
           WHEN "110924"     MOVE "5612080" TO 口座番号Ｗ
           WHEN "133280"     MOVE "5612099" TO 口座番号Ｗ
           WHEN "138305"     MOVE "5612102" TO 口座番号Ｗ
           WHEN "138370"     MOVE "5612110" TO 口座番号Ｗ
           WHEN "138420"     MOVE "5612129" TO 口座番号Ｗ
           WHEN "138644"     MOVE "5612137" TO 口座番号Ｗ
           WHEN "06120212"   MOVE "5612145" TO 口座番号Ｗ
           WHEN "06132104"   MOVE "5612153" TO 口座番号Ｗ
           WHEN "06133003"   MOVE "5612188" TO 口座番号Ｗ
           WHEN "06133524"   MOVE "5612196" TO 口座番号Ｗ
           WHEN "06135479"   MOVE "5612218" TO 口座番号Ｗ
           WHEN "06136360"   MOVE "5612226" TO 口座番号Ｗ
           WHEN "06136618"   MOVE "5612234" TO 口座番号Ｗ
           WHEN "06136907"   MOVE "5612242" TO 口座番号Ｗ
           WHEN "67110262"   MOVE "5612250" TO 口座番号Ｗ
           WHEN "67110940"   MOVE "5612269" TO 口座番号Ｗ
           WHEN "67114041"   MOVE "5612277" TO 口座番号Ｗ
           WHEN "67138321"   MOVE "5612285" TO 口座番号Ｗ
           WHEN "110064"     MOVE "5612293" TO 口座番号Ｗ
           WHEN "110122"     MOVE "5612307" TO 口座番号Ｗ
           WHEN "110254"     MOVE "5612315" TO 口座番号Ｗ
           WHEN "110262"     MOVE "5612323" TO 口座番号Ｗ
           WHEN "110270"     MOVE "5612331" TO 口座番号Ｗ
           WHEN "110304"     MOVE "5612358" TO 口座番号Ｗ
           WHEN "110312"     MOVE "5612366" TO 口座番号Ｗ
           WHEN "110387"     MOVE "5612374" TO 口座番号Ｗ
           WHEN "110395"     MOVE "5612382" TO 口座番号Ｗ
           WHEN "110478"     MOVE "5612390" TO 口座番号Ｗ
           WHEN "110510"     MOVE "5612404" TO 口座番号Ｗ
           WHEN "06273718"   MOVE "5612412" TO 口座番号Ｗ
           WHEN "06280119"   MOVE "5612420" TO 口座番号Ｗ
           WHEN "06280127"   MOVE "5612439" TO 口座番号Ｗ
           WHEN "114025"     MOVE "5612447" TO 口座番号Ｗ
           WHEN "114033"     MOVE "5612455" TO 口座番号Ｗ
           WHEN "114041"     MOVE "5612463" TO 口座番号Ｗ
           WHEN "114058"     MOVE "5612471" TO 口座番号Ｗ
           WHEN "114066"     MOVE "5612498" TO 口座番号Ｗ
           WHEN "114074"     MOVE "5612501" TO 口座番号Ｗ
           WHEN "114082"     MOVE "5612528" TO 口座番号Ｗ
           WHEN "114090"     MOVE "5612536" TO 口座番号Ｗ
           WHEN "138255"     MOVE "5612544" TO 口座番号Ｗ
           WHEN "138263"     MOVE "5612552" TO 口座番号Ｗ
           WHEN "138271"     MOVE "5612560" TO 口座番号Ｗ
           WHEN "138289"     MOVE "5612579" TO 口座番号Ｗ
           WHEN "138297"     MOVE "5612587" TO 口座番号Ｗ
           WHEN "138339"     MOVE "5612595" TO 口座番号Ｗ
           WHEN "138412"     MOVE "5612609" TO 口座番号Ｗ
           WHEN "138438"     MOVE "5612617" TO 口座番号Ｗ
           WHEN "138453"     MOVE "5612625" TO 口座番号Ｗ
           WHEN "138503"     MOVE "5612633" TO 口座番号Ｗ
           WHEN "138552"     MOVE "5612641" TO 口座番号Ｗ
           WHEN "138602"     MOVE "5612668" TO 口座番号Ｗ
           WHEN "06137715"   MOVE "5612676" TO 口座番号Ｗ
           WHEN "06137780"   MOVE "5612684" TO 口座番号Ｗ
           WHEN "27110220"   MOVE "5612692" TO 口座番号Ｗ
           WHEN "06137988"   MOVE "5612706" TO 口座番号Ｗ
           WHEN "06138275"   MOVE "5612714" TO 口座番号Ｗ
           WHEN "06139166"   MOVE "5612722" TO 口座番号Ｗ
           WHEN "06139299"   MOVE "5612730" TO 口座番号Ｗ
           WHEN "06139356"   MOVE "5612749" TO 口座番号Ｗ
           WHEN "63139356"   MOVE "5612749" TO 口座番号Ｗ
           WHEN "06139406"   MOVE "5612757" TO 口座番号Ｗ
           WHEN "06139414"   MOVE "5612765" TO 口座番号Ｗ
           WHEN "67114108"   MOVE "5612773" TO 口座番号Ｗ
           WHEN "06139547"   MOVE "5612781" TO 口座番号Ｗ
           WHEN "06139554"   MOVE "5612803" TO 口座番号Ｗ
           WHEN "06140156"   MOVE "5612811" TO 口座番号Ｗ
           WHEN "06140248"   MOVE "5612838" TO 口座番号Ｗ
           WHEN "63140248"   MOVE "5612838" TO 口座番号Ｗ
           WHEN "06140305"   MOVE "5612846" TO 口座番号Ｗ
           WHEN "63140305"   MOVE "5612846" TO 口座番号Ｗ
           WHEN "06140859"   MOVE "5612854" TO 口座番号Ｗ
           WHEN "06141261"   MOVE "5612862" TO 口座番号Ｗ
           WHEN "06141303"   MOVE "5612870" TO 口座番号Ｗ
           WHEN "06141493"   MOVE "5612889" TO 口座番号Ｗ
           WHEN "06141550"   MOVE "5612897" TO 口座番号Ｗ
           WHEN "27138247"   MOVE "5612900" TO 口座番号Ｗ
           WHEN "27138312"   MOVE "5612919" TO 口座番号Ｗ
           WHEN "67110122"   MOVE "5612927" TO 口座番号Ｗ
           WHEN "67110130"   MOVE "5612935" TO 口座番号Ｗ
           WHEN "67110155"   MOVE "5612943" TO 口座番号Ｗ
           WHEN "67110197"   MOVE "5612951" TO 口座番号Ｗ
           WHEN "67110247"   MOVE "5612978" TO 口座番号Ｗ
           WHEN "67110270"   MOVE "5612986" TO 口座番号Ｗ
           WHEN "67110288"   MOVE "5612994" TO 口座番号Ｗ
           WHEN "67110304"   MOVE "5613001" TO 口座番号Ｗ
           WHEN "67110346"   MOVE "5613028" TO 口座番号Ｗ
           WHEN "67110353"   MOVE "5613036" TO 口座番号Ｗ
           WHEN "67110379"   MOVE "5613044" TO 口座番号Ｗ
           WHEN "67110437"   MOVE "5613052" TO 口座番号Ｗ
           WHEN "67110908"   MOVE "5613060" TO 口座番号Ｗ
           WHEN "67110924"   MOVE "5613079" TO 口座番号Ｗ
           WHEN "67114017"   MOVE "5613087" TO 口座番号Ｗ
           WHEN "67114025"   MOVE "5613095" TO 口座番号Ｗ
           WHEN "67114033"   MOVE "5613109" TO 口座番号Ｗ
           WHEN "67114058"   MOVE "5613117" TO 口座番号Ｗ
           WHEN "67114066"   MOVE "5613125" TO 口座番号Ｗ
           WHEN "67114074"   MOVE "5613133" TO 口座番号Ｗ
           WHEN "67114082"   MOVE "5613141" TO 口座番号Ｗ
           WHEN "67138347"   MOVE "5613176" TO 口座番号Ｗ
           WHEN "67138370"   MOVE "5613184" TO 口座番号Ｗ
           WHEN "67138461"   MOVE "5613192" TO 口座番号Ｗ
           WHEN "67138511"   MOVE "5613206" TO 口座番号Ｗ
           WHEN "3102"       MOVE "5613214" TO 口座番号Ｗ
           WHEN "02110104"   MOVE "5613222" TO 口座番号Ｗ
           WHEN "110072"     MOVE "5613230" TO 口座番号Ｗ
           WHEN "110114"     MOVE "5613249" TO 口座番号Ｗ
           WHEN "110163"     MOVE "5613257" TO 口座番号Ｗ
           WHEN "110171"     MOVE "5613265" TO 口座番号Ｗ
           WHEN "67110171"   MOVE "5613265" TO 口座番号Ｗ
           WHEN "110189"     MOVE "5613273" TO 口座番号Ｗ
           WHEN "110338"     MOVE "5613281" TO 口座番号Ｗ
           WHEN "110445"     MOVE "5613303" TO 口座番号Ｗ
           WHEN "110452"     MOVE "5613311" TO 口座番号Ｗ
           WHEN "110460"     MOVE "5613338" TO 口座番号Ｗ
           WHEN "06340186"   MOVE "5613346" TO 口座番号Ｗ
           WHEN "110494"     MOVE "5613354" TO 口座番号Ｗ
           WHEN "110502"     MOVE "5613362" TO 口座番号Ｗ
           WHEN "110528"     MOVE "5613370" TO 口座番号Ｗ
           WHEN "06340319"   MOVE "5613389" TO 口座番号Ｗ
           WHEN "110544"     MOVE "5613397" TO 口座番号Ｗ
           WHEN "110551"     MOVE "5613400" TO 口座番号Ｗ
           WHEN "110569"     MOVE "5613419" TO 口座番号Ｗ
           WHEN "06350086"   MOVE "5613427" TO 口座番号Ｗ
           WHEN "06360119"   MOVE "5613435" TO 口座番号Ｗ
           WHEN "06380208"   MOVE "5613443" TO 口座番号Ｗ
           WHEN "113019"     MOVE "5613451" TO 口座番号Ｗ
           WHEN "113035"     MOVE "5613478" TO 口座番号Ｗ
           WHEN "133116"     MOVE "5613486" TO 口座番号Ｗ
           WHEN "138362"     MOVE "5613494" TO 口座番号Ｗ
           WHEN "138446"     MOVE "5613508" TO 口座番号Ｗ
           WHEN "138461"     MOVE "5613516" TO 口座番号Ｗ
           WHEN "138495"     MOVE "5613524" TO 口座番号Ｗ
           WHEN "06260038"   MOVE "5613532" TO 口座番号Ｗ
           WHEN "27110196"   MOVE "5613540" TO 口座番号Ｗ
           WHEN "06141659"   MOVE "5613559" TO 口座番号Ｗ
           WHEN "06141881"   MOVE "5613567" TO 口座番号Ｗ
           WHEN "06142095"   MOVE "5613575" TO 口座番号Ｗ
           WHEN "06142152"   MOVE "5613583" TO 口座番号Ｗ
           WHEN "06160295"   MOVE "5613591" TO 口座番号Ｗ
           WHEN "27138262"   MOVE "5613605" TO 口座番号Ｗ
           WHEN "06221071"   MOVE "5613613" TO 口座番号Ｗ
           WHEN "67110015"   MOVE "5613621" TO 口座番号Ｗ
           WHEN "67110080"   MOVE "5613648" TO 口座番号Ｗ
           WHEN "67110296"   MOVE "5613656" TO 口座番号Ｗ
           WHEN "67110494"   MOVE "5613664" TO 口座番号Ｗ
           WHEN "67110544"   MOVE "5613672" TO 口座番号Ｗ
           WHEN "67110569"   MOVE "5613680" TO 口座番号Ｗ
           WHEN "06231948"   MOVE "5613699" TO 口座番号Ｗ
           WHEN "67110858"   MOVE "5613702" TO 口座番号Ｗ
           WHEN "67138271"   MOVE "5613710" TO 口座番号Ｗ
           WHEN "67138396"   MOVE "5613729" TO 口座番号Ｗ
           WHEN "67138420"   MOVE "5613737" TO 口座番号Ｗ
           WHEN "06139224"   MOVE "5613745" TO 口座番号Ｗ
           WHEN "06139240"   MOVE "5613753" TO 口座番号Ｗ
           WHEN "06120786"   MOVE "5613761" TO 口座番号Ｗ
           WHEN "06139273"   MOVE "5613788" TO 口座番号Ｗ
           WHEN "31130552"   MOVE "5613796" TO 口座番号Ｗ
           WHEN "31110364"   MOVE "5613818" TO 口座番号Ｗ
           WHEN "06250054"   MOVE "5613826" TO 口座番号Ｗ
           WHEN "27110212"   MOVE "5613834" TO 口座番号Ｗ
           WHEN "06231013"   MOVE "5613842" TO 口座番号Ｗ
           WHEN "63231013"   MOVE "5613842" TO 口座番号Ｗ
           WHEN "06230627"   MOVE "5613850" TO 口座番号Ｗ
           WHEN "06231062"   MOVE "5613869" TO 口座番号Ｗ
           WHEN "06231856"   MOVE "5613877" TO 口座番号Ｗ
           WHEN "06260384"   MOVE "5613885" TO 口座番号Ｗ
           WHEN "63260384"   MOVE "5613885" TO 口座番号Ｗ
           WHEN "27138296"   MOVE "5613893" TO 口座番号Ｗ
           WHEN "06260616"   MOVE "5613907" TO 口座番号Ｗ
           WHEN "06270052"   MOVE "5613915" TO 口座番号Ｗ
           WHEN "06270326"   MOVE "5613923" TO 口座番号Ｗ
           WHEN "06400634"   MOVE "5613168" TO 口座番号Ｗ
           WHEN "06400477"   MOVE "5613931" TO 口座番号Ｗ
           WHEN "06401160"   MOVE "5613958" TO 口座番号Ｗ
           WHEN "06450019"   MOVE "5613966" TO 口座番号Ｗ
           WHEN "31110281"   MOVE "5613974" TO 口座番号Ｗ
           WHEN "32140311"   MOVE "5613982" TO 口座番号Ｗ
           WHEN "32400327"   MOVE "5613990" TO 口座番号Ｗ
           WHEN "32270415"   MOVE "5614008" TO 口座番号Ｗ
           WHEN "06271704"   MOVE "5614016" TO 口座番号Ｗ
           WHEN "06271761"   MOVE "5614024" TO 口座番号Ｗ
           WHEN "06272017"   MOVE "5614032" TO 口座番号Ｗ
           WHEN "06272165"   MOVE "5614040" TO 口座番号Ｗ
           WHEN "06272272"   MOVE "5614059" TO 口座番号Ｗ
           WHEN "06272439"   MOVE "5614067" TO 口座番号Ｗ
           WHEN "06272454"   MOVE "5614075" TO 口座番号Ｗ
           WHEN "39011002"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39011010"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39011028"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39011036"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39011044"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39011051"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39011069"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39011077"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39011085"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39011093"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39011101"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012026"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012034"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012042"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012059"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012067"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012075"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012083"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012091"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012109"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012117"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012125"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012133"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012141"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012158"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012166"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012174"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012182"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012190"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012208"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012216"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012224"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012232"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012240"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012257"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012265"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012273"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012281"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012299"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012307"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012315"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012331"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012349"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012356"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39012364"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013032"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013040"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013313"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013321"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013339"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013347"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013370"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013438"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013453"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013461"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013479"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013610"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013628"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013636"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013644"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013677"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013701"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013719"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013917"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013925"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013933"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013941"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013958"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013966"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013974"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013982"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39013990"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014006"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014014"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014022"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014030"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014048"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014055"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014063"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014071"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014089"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014097"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014238"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014246"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014253"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014279"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014287"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014295"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014303"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014311"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014329"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014337"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014345"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014360"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014378"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014386"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014394"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014527"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014535"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014543"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014550"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014568"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014576"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014584"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014592"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014600"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014618"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014626"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014634"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014642"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014659"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014683"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014691"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014709"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014717"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014816"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014824"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014832"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014840"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014857"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014865"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014873"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39014881"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015110"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015128"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015136"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015144"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015169"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015177"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015185"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015193"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015433"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015441"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015458"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015466"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015474"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015490"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015508"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015524"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015557"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015581"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015599"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015607"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015615"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015623"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015631"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015649"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015714"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015755"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015789"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015813"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015847"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015854"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39015862"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016019"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016027"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016043"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016076"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016084"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016092"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016100"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016316"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016324"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016332"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016340"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016357"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016365"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016373"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016381"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016399"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016415"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016423"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016431"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016449"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016456"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016464"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016472"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016480"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016498"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016613"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016621"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016639"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016647"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016654"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016670"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016688"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016910"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016928"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016936"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39016944"   MOVE "5614083" TO 口座番号Ｗ
           WHEN "39041017"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39041025"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39041033"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39041041"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39041058"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39042023"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39042031"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39042056"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39042064"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39042072"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39042080"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39042098"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39042114"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39042122"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39042130"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39042148"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39042155"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39043013"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39043021"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39043211"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39043229"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39043237"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39043245"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39043419"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39043617"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39043625"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39044011"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39044045"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39044060"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39044219"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39044227"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39044235"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39044243"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39044441"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39044458"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39045018"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39045059"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39045810"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39046032"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39046065"   MOVE "5614091" TO 口座番号Ｗ
           WHEN "39062013"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39062021"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39062039"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39062047"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39062054"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39062062"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39062070"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39062088"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39062096"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39062104"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39062112"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39062120"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39062138"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39063011"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39063029"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39063219"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39063227"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39063235"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39063243"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39063417"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39063615"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39063623"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39063631"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39063649"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39063656"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39063664"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39063672"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39063813"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39063821"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39064019"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39064027"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39064035"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39064266"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39064282"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39064613"   MOVE "5614105" TO 口座番号Ｗ
           WHEN "39072012"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39072020"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39072038"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39072046"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39072053"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39072079"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39072087"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39072095"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39072103"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39072111"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39072129"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39072137"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39072145"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39073010"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39073036"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39073085"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39073093"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39073226"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39073424"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39073440"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39073622"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39073648"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39073671"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39073689"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39074026"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39074059"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39074075"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39074083"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39074216"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39074224"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39074232"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39074448"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39074455"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39074463"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39074471"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39074612"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39074646"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39074653"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39074661"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39074810"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39074828"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39074836"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39074844"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39075015"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39075023"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39075031"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39075049"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39075056"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39075213"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39075221"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39075411"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39075429"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39075437"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39075445"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39075452"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39075460"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39075478"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39075486"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39075619"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39075643"   MOVE "5614113" TO 口座番号Ｗ
           WHEN "39082011"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082029"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082037"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082045"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082052"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082078"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082086"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082102"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082110"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082128"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082144"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082151"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082169"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082177"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082193"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082201"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082219"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082227"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082235"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082243"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082250"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082268"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082276"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082284"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082292"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082300"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082318"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082326"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082334"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082342"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082359"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39082367"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39083027"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39083092"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39083100"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39083415"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39083647"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39084421"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39084439"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39084470"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39085212"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39085428"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39085469"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39085642"   MOVE "5614121" TO 口座番号Ｗ
           WHEN "39092010"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39092028"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39092036"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39092044"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39092051"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39092069"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39092085"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39092093"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39092101"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39092119"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39092135"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39092143"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39092150"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39092168"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39093018"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39093216"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39093414"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39093422"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39093430"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39093448"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39093455"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39093612"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39093646"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39093653"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39093661"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39093679"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39093687"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39093844"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39093869"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39094073"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39094115"   MOVE "5614148" TO 口座番号Ｗ
           WHEN "39102017"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39102025"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39102033"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39102041"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39102058"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39102066"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39102074"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39102082"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39102090"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39102108"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39102116"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39102124"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39103031"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39103445"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39103452"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39103635"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39103668"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39103676"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39103825"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39103833"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39103841"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39104211"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39104245"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39104252"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39104260"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39104278"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39104286"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39104294"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39104435"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39104443"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39104484"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39104492"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39104641"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39105218"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39105226"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39105234"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39105242"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39105259"   MOVE "5614156" TO 口座番号Ｗ
           WHEN "39121017"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39121025"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39121033"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39121041"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39121058"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39121066"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122023"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122031"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122049"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122056"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122064"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122072"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122080"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122106"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122114"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122122"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122130"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122155"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122163"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122171"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122189"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122197"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122205"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122213"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122221"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122239"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122247"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122254"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122262"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122270"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122288"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122296"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122304"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122312"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122320"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122338"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122346"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122353"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122361"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122379"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39122387"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39123229"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39123252"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39123286"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39123294"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39123427"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39123476"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39123492"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39124029"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39124037"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39124094"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39124102"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39124219"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39124227"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39124235"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39124243"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39124268"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39124276"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39124417"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39124433"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39124631"   MOVE "5614164" TO 口座番号Ｗ
           WHEN "39141015"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141023"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141031"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141049"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141056"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141064"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141072"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141080"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141098"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141106"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141114"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141122"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141130"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141148"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141155"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141163"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141171"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141189"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141312"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141320"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141338"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141346"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141353"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141361"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39141379"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39142013"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39142039"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39142047"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39142054"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39142062"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39142070"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39142088"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39142096"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39142104"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39142112"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39142120"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39142138"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39142146"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39142153"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39142161"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39142179"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39142187"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39143011"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39143219"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39143417"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39143425"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39143615"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39143623"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39143631"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39143649"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39143664"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39143821"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39143839"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39143847"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39144019"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39144027"   MOVE "5614172" TO 口座番号Ｗ
           WHEN "39192018"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39192026"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39192042"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39192059"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39192067"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39192075"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39192083"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39192091"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39192109"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39192117"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39192125"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39192133"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39192141"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39193461"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39193610"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39193628"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39193644"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39193651"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39193669"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39193842"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39194220"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39194238"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39194246"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39194253"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39194295"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39194303"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39194428"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39194436"   MOVE "5614180" TO 口座番号Ｗ
           WHEN "39202015"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39202023"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39202031"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39202049"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39202056"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39202064"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39202072"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39202080"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39202098"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39202106"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39202114"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39202122"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39202130"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39202148"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39202155"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39202171"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39202189"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39202197"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39202205"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203039"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203047"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203054"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203062"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203070"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203096"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203211"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203237"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203245"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203492"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203500"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203617"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203625"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203633"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203823"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203831"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203849"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203856"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203864"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39203880"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204029"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204037"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204045"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204060"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204078"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204094"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204102"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204110"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204128"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204136"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204144"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204151"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204169"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204177"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204227"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204235"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204250"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204292"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204300"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204326"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204466"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204482"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204490"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204508"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204516"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204524"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204813"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204821"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204854"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39204862"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39205216"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39205414"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39205430"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39205612"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39205620"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39205638"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39205810"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39205836"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39205885"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39205893"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39205901"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39206024"   MOVE "5614199" TO 口座番号Ｗ
           WHEN "39212014"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212022"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212030"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212048"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212055"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212063"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212071"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212089"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212097"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212105"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212113"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212121"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212139"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212147"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212154"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212162"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212170"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212188"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212196"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212204"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39212212"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39213020"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39213038"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39213418"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39213616"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39213624"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39213814"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39213822"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39213830"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39214010"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39214036"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39214044"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39214218"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39215017"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39215025"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39215033"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39215041"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39215058"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39215066"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39215074"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39215215"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39216049"   MOVE "5614202" TO 口座番号Ｗ
           WHEN "39231014"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39231022"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39231030"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39231048"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39231055"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39231063"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39231071"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39231089"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39231097"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39231105"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39231113"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39231121"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39231139"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39231147"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39231154"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39231162"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232012"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232020"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232038"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232046"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232053"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232061"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232079"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232087"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232095"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232103"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232111"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232129"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232137"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232145"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232152"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232160"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232178"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232194"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232202"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232210"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232228"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232236"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232244"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232251"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232269"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232277"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232285"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232293"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232301"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232319"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232327"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232335"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232343"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39232350"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39233028"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39233044"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39233424"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39233457"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39233614"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39233622"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39234216"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39234224"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39234232"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39234240"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39234257"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39234273"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39234414"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39234422"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39234455"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39234463"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39234471"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39234810"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39234828"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39234836"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39235015"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39235213"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39235619"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39235627"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39235635"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39236039"   MOVE "5614210" TO 口座番号Ｗ
           WHEN "39271028"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271036"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271044"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271069"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271077"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271085"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271093"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271119"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271135"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271143"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271150"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271168"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271176"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271184"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271192"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271200"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271218"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271226"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271234"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271242"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271259"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271267"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271275"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271283"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271416"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271424"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271432"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271440"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271457"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271465"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39271473"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272026"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272034"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272042"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272059"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272067"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272075"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272083"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272091"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272109"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272117"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272125"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272133"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272141"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272158"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272166"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272174"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272182"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272190"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272208"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272216"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272224"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272232"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272240"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272257"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272265"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272273"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272281"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272299"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272307"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272315"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39272323"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39273016"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39273214"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39273222"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39273412"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39273610"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39273628"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39273669"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39273818"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39273826"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39273834"   MOVE "5614229" TO 口座番号Ｗ
           WHEN "39401013"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39401039"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39401054"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39401062"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39401070"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39401088"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39401096"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39401310"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39401328"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39401336"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39401344"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39401351"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39401369"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39401377"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402029"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402037"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402045"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402052"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402060"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402078"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402102"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402110"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402128"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402136"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402144"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402151"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402169"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402177"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402185"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402193"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402201"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402219"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402227"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402235"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402243"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402250"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402268"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402276"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402284"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39402292"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39403050"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39403415"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39403423"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39403431"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39403449"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39403456"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39403480"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39403498"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39403811"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39403829"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39403837"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39403845"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39404017"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39404025"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39404215"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39404470"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39404488"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39404629"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39404637"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39405030"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39405220"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39405410"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39405436"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39405444"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39405451"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39405469"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39406012"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39406020"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39406046"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39406053"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39406087"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39406095"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39406103"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39406210"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39406251"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39406426"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39406467"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39406475"   MOVE "5614237" TO 口座番号Ｗ
           WHEN "39422019"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39422027"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39422035"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39422043"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39422050"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39422076"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39422084"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39422092"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39422100"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39422118"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39422126"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39422134"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39422142"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39423074"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39423082"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39423215"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39423223"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39423231"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39423835"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39423884"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39423892"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39423918"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39424114"   MOVE "5614245" TO 口座番号Ｗ
           WHEN "39432018"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39432026"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39432034"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39432042"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39432059"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39432067"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39432083"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39432109"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39432117"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39432125"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39432133"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39432141"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39432158"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39432166"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39433412"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39433420"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39433487"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39433644"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39433677"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39433685"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39433693"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39433859"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39434030"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39434048"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39434238"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39434246"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39434253"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39434287"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39434329"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39434337"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39434410"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39434428"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39434436"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39434444"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39434477"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39434683"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39434824"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39434840"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39435011"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39435052"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39435060"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39435078"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39435102"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39435110"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39435128"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39435136"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39435144"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39435318"   MOVE "5614253" TO 口座番号Ｗ
           WHEN "39442017"   MOVE "5614261" TO 口座番号Ｗ
           WHEN "39442025"   MOVE "5614261" TO 口座番号Ｗ
           WHEN "39442033"   MOVE "5614261" TO 口座番号Ｗ
           WHEN "39442041"   MOVE "5614261" TO 口座番号Ｗ
           WHEN "39442058"   MOVE "5614261" TO 口座番号Ｗ
           WHEN "39442066"   MOVE "5614261" TO 口座番号Ｗ
           WHEN "39442074"   MOVE "5614261" TO 口座番号Ｗ
           WHEN "39442082"   MOVE "5614261" TO 口座番号Ｗ
           WHEN "39442090"   MOVE "5614261" TO 口座番号Ｗ
           WHEN "39442108"   MOVE "5614261" TO 口座番号Ｗ
           WHEN "39442116"   MOVE "5614261" TO 口座番号Ｗ
           WHEN "39442124"   MOVE "5614261" TO 口座番号Ｗ
           WHEN "39442132"   MOVE "5614261" TO 口座番号Ｗ
           WHEN "39442140"   MOVE "5614261" TO 口座番号Ｗ
           WHEN "39443221"   MOVE "5614261" TO 口座番号Ｗ
           WHEN "39443411"   MOVE "5614261" TO 口座番号Ｗ
           WHEN "39444617"   MOVE "5614261" TO 口座番号Ｗ
           WHEN "39444625"   MOVE "5614261" TO 口座番号Ｗ
           WHEN "39452016"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39452024"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39452032"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39452040"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39452057"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39452065"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39452073"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39452081"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39452099"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39453014"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39453212"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39453220"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39453410"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39453618"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39453626"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39453824"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39453832"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39454012"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39454020"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39454038"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39454046"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39454053"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39454061"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39454210"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39454293"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39454301"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39454319"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39454418"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39454426"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39454434"   MOVE "5614288" TO 口座番号Ｗ
           WHEN "39462015"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39462031"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39462049"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39462064"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39462080"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39462098"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39462106"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39462130"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39462148"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39462155"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39462163"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39462171"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39462189"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39462197"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39462205"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39462213"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39462221"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39462239"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39463039"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39463047"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39463922"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39464045"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39464219"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39464417"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39464425"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39464433"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39464524"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39464680"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39464821"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39464904"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39464912"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39464920"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39465018"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39465026"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39465059"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39465232"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39465240"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39465257"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39465273"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39465299"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39465307"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39465315"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39465323"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39465331"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39465349"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39465356"   MOVE "5614296" TO 口座番号Ｗ
           WHEN "39111018"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39111026"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39111034"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39111042"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39111059"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39111067"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39111075"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39111083"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39111091"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39111109"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112016"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112024"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112032"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112065"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112073"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112081"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112099"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112107"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112115"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112123"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112149"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112156"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112164"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112172"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112180"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112198"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112214"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112222"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112230"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112248"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112255"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112263"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112271"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112289"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112297"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112305"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112313"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112321"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112339"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112347"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112354"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112370"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112388"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112396"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112404"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112412"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112420"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112438"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39112453"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39113014"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39113246"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39113261"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39113279"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39113410"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39113428"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39113436"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39113469"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39113477"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39113485"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39113493"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39113618"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39113626"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39113634"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39113659"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39113691"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39113816"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39113832"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39113857"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39114087"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39114210"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39114244"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39114251"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39114426"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39114459"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39114467"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39114616"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39114624"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39114640"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "39114657"   MOVE "5614318" TO 口座番号Ｗ
           WHEN "01010016"   MOVE "5614326" TO 口座番号Ｗ
           WHEN "03010014"   MOVE "5614326" TO 口座番号Ｗ
           WHEN "04010013"   MOVE "5614326" TO 口座番号Ｗ
           WHEN "01020015"   MOVE "5614334" TO 口座番号Ｗ
           WHEN "03020013"   MOVE "5614334" TO 口座番号Ｗ
           WHEN "04020012"   MOVE "5614334" TO 口座番号Ｗ
           WHEN "01030014"   MOVE "5614342" TO 口座番号Ｗ
           WHEN "03030012"   MOVE "5614342" TO 口座番号Ｗ
           WHEN "04030011"   MOVE "5614342" TO 口座番号Ｗ
           WHEN "01040013"   MOVE "5614350" TO 口座番号Ｗ
           WHEN "03040011"   MOVE "5614350" TO 口座番号Ｗ
           WHEN "04040010"   MOVE "5614350" TO 口座番号Ｗ
           WHEN "01050012"   MOVE "5614369" TO 口座番号Ｗ
           WHEN "03050010"   MOVE "5614369" TO 口座番号Ｗ
           WHEN "04050019"   MOVE "5614369" TO 口座番号Ｗ
           WHEN "01060011"   MOVE "5614377" TO 口座番号Ｗ
           WHEN "03060019"   MOVE "5614377" TO 口座番号Ｗ
           WHEN "04060018"   MOVE "5614377" TO 口座番号Ｗ
           WHEN "01070010"   MOVE "5614385" TO 口座番号Ｗ
           WHEN "03070018"   MOVE "5614385" TO 口座番号Ｗ
           WHEN "04070017"   MOVE "5614385" TO 口座番号Ｗ
           WHEN "01080019"   MOVE "5614393" TO 口座番号Ｗ
           WHEN "03080017"   MOVE "5614393" TO 口座番号Ｗ
           WHEN "04080016"   MOVE "5614393" TO 口座番号Ｗ
           WHEN "06272520"   MOVE "5614407" TO 口座番号Ｗ
           WHEN "06272595"   MOVE "5614415" TO 口座番号Ｗ
           WHEN "06273015"   MOVE "5614423" TO 口座番号Ｗ
           WHEN "06273478"   MOVE "5614431" TO 口座番号Ｗ
           WHEN "06273742"   MOVE "5614458" TO 口座番号Ｗ
           WHEN "06281380"   MOVE "5614466" TO 口座番号Ｗ
           WHEN "06133706"   MOVE "5614474" TO 口座番号Ｗ
           WHEN "06134290"   MOVE "5614482" TO 口座番号Ｗ
           WHEN "06135362"   MOVE "5614490" TO 口座番号Ｗ
           WHEN "06135412"   MOVE "5614504" TO 口座番号Ｗ
           WHEN "06137095"   MOVE "5614512" TO 口座番号Ｗ
           WHEN "06135461"   MOVE "5614520" TO 口座番号Ｗ
           WHEN "06138747"   MOVE "5614539" TO 口座番号Ｗ
           WHEN "27114107"   MOVE "5614547" TO 口座番号Ｗ
           WHEN "27114016"   MOVE "5614555" TO 口座番号Ｗ
           WHEN "01090018"   MOVE "5614563" TO 口座番号Ｗ
           WHEN "03090016"   MOVE "5614563" TO 口座番号Ｗ
           WHEN "04090015"   MOVE "5614563" TO 口座番号Ｗ
           WHEN "01100015"   MOVE "5614571" TO 口座番号Ｗ
           WHEN "03100013"   MOVE "5614571" TO 口座番号Ｗ
           WHEN "04100012"   MOVE "5614571" TO 口座番号Ｗ
           WHEN "01110014"   MOVE "5614598" TO 口座番号Ｗ
           WHEN "03110012"   MOVE "5614598" TO 口座番号Ｗ
           WHEN "04110011"   MOVE "5614598" TO 口座番号Ｗ
           WHEN "01120013"   MOVE "5614601" TO 口座番号Ｗ
           WHEN "03120011"   MOVE "5614601" TO 口座番号Ｗ
           WHEN "04120010"   MOVE "5614601" TO 口座番号Ｗ
           WHEN "01130012"   MOVE "5614628" TO 口座番号Ｗ
           WHEN "03130010"   MOVE "5614628" TO 口座番号Ｗ
           WHEN "04130019"   MOVE "5614628" TO 口座番号Ｗ
           WHEN "41140138"   MOVE "5614636" TO 口座番号Ｗ
           WHEN "06130058"   MOVE "5614644" TO 口座番号Ｗ
           WHEN "06130066"   MOVE "5614652" TO 口座番号Ｗ
           WHEN "06130074"   MOVE "5614660" TO 口座番号Ｗ
           WHEN "06130082"   MOVE "5614679" TO 口座番号Ｗ
           WHEN "06130090"   MOVE "5614687" TO 口座番号Ｗ
           WHEN "06130108"   MOVE "5614695" TO 口座番号Ｗ
           WHEN "63130108"   MOVE "5614695" TO 口座番号Ｗ
           WHEN "06130116"   MOVE "5614709" TO 口座番号Ｗ
           WHEN "06130124"   MOVE "5614717" TO 口座番号Ｗ
           WHEN "63130124"   MOVE "5614717" TO 口座番号Ｗ
           WHEN "06130132"   MOVE "5614725" TO 口座番号Ｗ
           WHEN "06130181"   MOVE "5614733" TO 口座番号Ｗ
           WHEN "06130199"   MOVE "5614741" TO 口座番号Ｗ
           WHEN "06130231"   MOVE "5614768" TO 口座番号Ｗ
           WHEN "06130298"   MOVE "5614776" TO 口座番号Ｗ
           WHEN "63130298"   MOVE "5614776" TO 口座番号Ｗ
           WHEN "06130306"   MOVE "5614784" TO 口座番号Ｗ
           WHEN "06130389"   MOVE "5614792" TO 口座番号Ｗ
           WHEN "63130389"   MOVE "5614792" TO 口座番号Ｗ
           WHEN "06130405"   MOVE "5614806" TO 口座番号Ｗ
           WHEN "06130439"   MOVE "5614814" TO 口座番号Ｗ
           WHEN "06130447"   MOVE "5614822" TO 口座番号Ｗ
           WHEN "06130454"   MOVE "5614830" TO 口座番号Ｗ
           WHEN "06130488"   MOVE "5614849" TO 口座番号Ｗ
           WHEN "06130553"   MOVE "5614857" TO 口座番号Ｗ
           WHEN "06130587"   MOVE "5614865" TO 口座番号Ｗ
           WHEN "06130637"   MOVE "5614873" TO 口座番号Ｗ
           WHEN "06130645"   MOVE "5614881" TO 口座番号Ｗ
           WHEN "06130660"   MOVE "5614903" TO 口座番号Ｗ
           WHEN "63130660"   MOVE "5614903" TO 口座番号Ｗ
           WHEN "06130686"   MOVE "5614911" TO 口座番号Ｗ
           WHEN "06130702"   MOVE "5614938" TO 口座番号Ｗ
           WHEN "63130702"   MOVE "5614938" TO 口座番号Ｗ
           WHEN "06130710"   MOVE "5614946" TO 口座番号Ｗ
           WHEN "06130728"   MOVE "5614954" TO 口座番号Ｗ
           WHEN "06130736"   MOVE "5614962" TO 口座番号Ｗ
           WHEN "06130769"   MOVE "5614970" TO 口座番号Ｗ
           WHEN "63130769"   MOVE "5614970" TO 口座番号Ｗ
           WHEN "06130777"   MOVE "5614989" TO 口座番号Ｗ
           WHEN "63130777"   MOVE "5614989" TO 口座番号Ｗ
           WHEN "06130785"   MOVE "5614997" TO 口座番号Ｗ
           WHEN "06130843"   MOVE "5615012" TO 口座番号Ｗ
           WHEN "06130868"   MOVE "5615020" TO 口座番号Ｗ
           WHEN "06130892"   MOVE "5615039" TO 口座番号Ｗ
           WHEN "63130892"   MOVE "5615039" TO 口座番号Ｗ
           WHEN "06130900"   MOVE "5615047" TO 口座番号Ｗ
           WHEN "06130926"   MOVE "5615055" TO 口座番号Ｗ
           WHEN "06130934"   MOVE "5615063" TO 口座番号Ｗ
           WHEN "06130975"   MOVE "5615071" TO 口座番号Ｗ
           WHEN "06131064"   MOVE "5615098" TO 口座番号Ｗ
           WHEN "06131114"   MOVE "5615101" TO 口座番号Ｗ
           WHEN "06131163"   MOVE "5615128" TO 口座番号Ｗ
           WHEN "06131189"   MOVE "5615136" TO 口座番号Ｗ
           WHEN "06131213"   MOVE "5615144" TO 口座番号Ｗ
           WHEN "06131288"   MOVE "5615152" TO 口座番号Ｗ
           WHEN "06131296"   MOVE "5615160" TO 口座番号Ｗ
           WHEN "06131338"   MOVE "5615179" TO 口座番号Ｗ
           WHEN "06131346"   MOVE "5615187" TO 口座番号Ｗ
           WHEN "06131379"   MOVE "5615195" TO 口座番号Ｗ
           WHEN "06131429"   MOVE "5615209" TO 口座番号Ｗ
           WHEN "06131452"   MOVE "5615217" TO 口座番号Ｗ
           WHEN "06131460"   MOVE "5615225" TO 口座番号Ｗ
           WHEN "06131551"   MOVE "5615233" TO 口座番号Ｗ
           WHEN "63131551"   MOVE "5615233" TO 口座番号Ｗ
           WHEN "06131569"   MOVE "5615241" TO 口座番号Ｗ
           WHEN "06131577"   MOVE "5615268" TO 口座番号Ｗ
           WHEN "06131585"   MOVE "5615276" TO 口座番号Ｗ
           WHEN "06131635"   MOVE "5615284" TO 口座番号Ｗ
           WHEN "06131668"   MOVE "5615292" TO 口座番号Ｗ
           WHEN "63131668"   MOVE "5615292" TO 口座番号Ｗ
           WHEN "06131676"   MOVE "5615306" TO 口座番号Ｗ
           WHEN "06131742"   MOVE "5615314" TO 口座番号Ｗ
           WHEN "06131783"   MOVE "5615322" TO 口座番号Ｗ
           WHEN "06131791"   MOVE "5615330" TO 口座番号Ｗ
           WHEN "06131817"   MOVE "5615349" TO 口座番号Ｗ
           WHEN "06131841"   MOVE "5615357" TO 口座番号Ｗ
           WHEN "06131882"   MOVE "5615365" TO 口座番号Ｗ
           WHEN "06131924"   MOVE "5615373" TO 口座番号Ｗ
           WHEN "06131932"   MOVE "5615381" TO 口座番号Ｗ
           WHEN "06131999"   MOVE "5615403" TO 口座番号Ｗ
           WHEN "63131999"   MOVE "5615403" TO 口座番号Ｗ
           WHEN "06132013"   MOVE "5615411" TO 口座番号Ｗ
           WHEN "06132039"   MOVE "5615438" TO 口座番号Ｗ
           WHEN "06132054"   MOVE "5615446" TO 口座番号Ｗ
           WHEN "06132088"   MOVE "5615454" TO 口座番号Ｗ
           WHEN "63132088"   MOVE "5615454" TO 口座番号Ｗ
           WHEN "06132112"   MOVE "5615462" TO 口座番号Ｗ
           WHEN "63132112"   MOVE "5615462" TO 口座番号Ｗ
           WHEN "06132120"   MOVE "5615470" TO 口座番号Ｗ
           WHEN "06132146"   MOVE "5615489" TO 口座番号Ｗ
           WHEN "63132146"   MOVE "5615489" TO 口座番号Ｗ
           WHEN "06132161"   MOVE "5615497" TO 口座番号Ｗ
           WHEN "06132179"   MOVE "5615500" TO 口座番号Ｗ
           WHEN "06132211"   MOVE "5615519" TO 口座番号Ｗ
           WHEN "06132229"   MOVE "5615527" TO 口座番号Ｗ
           WHEN "06132260"   MOVE "5615535" TO 口座番号Ｗ
           WHEN "63132260"   MOVE "5615535" TO 口座番号Ｗ
           WHEN "06132294"   MOVE "5615543" TO 口座番号Ｗ
           WHEN "06132302"   MOVE "5615551" TO 口座番号Ｗ
           WHEN "06132310"   MOVE "5615578" TO 口座番号Ｗ
           WHEN "06132328"   MOVE "5615586" TO 口座番号Ｗ
           WHEN "06132336"   MOVE "5615594" TO 口座番号Ｗ
           WHEN "06132344"   MOVE "5615608" TO 口座番号Ｗ
           WHEN "06132369"   MOVE "5615616" TO 口座番号Ｗ
           WHEN "06132377"   MOVE "5615624" TO 口座番号Ｗ
           WHEN "06132393"   MOVE "5615632" TO 口座番号Ｗ
           WHEN "06132419"   MOVE "5615640" TO 口座番号Ｗ
           WHEN "06132427"   MOVE "5615659" TO 口座番号Ｗ
           WHEN "06132443"   MOVE "5615667" TO 口座番号Ｗ
           WHEN "06132468"   MOVE "5615675" TO 口座番号Ｗ
           WHEN "06137673"   MOVE "5615675" TO 口座番号Ｗ
           WHEN "06137806"   MOVE "5615675" TO 口座番号Ｗ
           WHEN "06138671"   MOVE "5615675" TO 口座番号Ｗ
           WHEN "06132476"   MOVE "5615683" TO 口座番号Ｗ
           WHEN "06132484"   MOVE "5615691" TO 口座番号Ｗ
           WHEN "06132500"   MOVE "5615705" TO 口座番号Ｗ
           WHEN "06132518"   MOVE "5615713" TO 口座番号Ｗ
           WHEN "06132559"   MOVE "5615721" TO 口座番号Ｗ
           WHEN "06132567"   MOVE "5615748" TO 口座番号Ｗ
           WHEN "06132583"   MOVE "5615756" TO 口座番号Ｗ
           WHEN "63132583"   MOVE "5615756" TO 口座番号Ｗ
           WHEN "06132658"   MOVE "5615764" TO 口座番号Ｗ
           WHEN "06132682"   MOVE "5615772" TO 口座番号Ｗ
           WHEN "06132690"   MOVE "5615780" TO 口座番号Ｗ
           WHEN "06132765"   MOVE "5615799" TO 口座番号Ｗ
           WHEN "63132765"   MOVE "5615799" TO 口座番号Ｗ
           WHEN "06132773"   MOVE "5615802" TO 口座番号Ｗ
           WHEN "63132773"   MOVE "5615802" TO 口座番号Ｗ
           WHEN "06132781"   MOVE "5615810" TO 口座番号Ｗ
           WHEN "06132799"   MOVE "5615829" TO 口座番号Ｗ
           WHEN "06132807"   MOVE "5615837" TO 口座番号Ｗ
           WHEN "06132831"   MOVE "5615845" TO 口座番号Ｗ
           WHEN "06132849"   MOVE "5615853" TO 口座番号Ｗ
           WHEN "06132856"   MOVE "5615861" TO 口座番号Ｗ
           WHEN "06132864"   MOVE "5615888" TO 口座番号Ｗ
           WHEN "06132922"   MOVE "5615896" TO 口座番号Ｗ
           WHEN "63132922"   MOVE "5615896" TO 口座番号Ｗ
           WHEN "06132930"   MOVE "5615918" TO 口座番号Ｗ
           WHEN "06132948"   MOVE "5615926" TO 口座番号Ｗ
           WHEN "63132948"   MOVE "5615926" TO 口座番号Ｗ
           WHEN "06132963"   MOVE "5615934" TO 口座番号Ｗ
           WHEN "06132971"   MOVE "5615942" TO 口座番号Ｗ
           WHEN "63132971"   MOVE "5615942" TO 口座番号Ｗ
           WHEN "06133029"   MOVE "5615950" TO 口座番号Ｗ
           WHEN "06090419"   MOVE "5615969" TO 口座番号Ｗ
           WHEN "06133086"   MOVE "5615969" TO 口座番号Ｗ
           WHEN "63090419"   MOVE "5615969" TO 口座番号Ｗ
           WHEN "63133086"   MOVE "5615969" TO 口座番号Ｗ
           WHEN "06133094"   MOVE "5615977" TO 口座番号Ｗ
           WHEN "06133102"   MOVE "5615985" TO 口座番号Ｗ
           WHEN "06133110"   MOVE "5615993" TO 口座番号Ｗ
           WHEN "06133169"   MOVE "5616000" TO 口座番号Ｗ
           WHEN "63133169"   MOVE "5616000" TO 口座番号Ｗ
           WHEN "06133177"   MOVE "5616019" TO 口座番号Ｗ
           WHEN "06133185"   MOVE "5616027" TO 口座番号Ｗ
           WHEN "06133243"   MOVE "5616035" TO 口座番号Ｗ
           WHEN "06133250"   MOVE "5616043" TO 口座番号Ｗ
           WHEN "06133276"   MOVE "5616051" TO 口座番号Ｗ
           WHEN "06133300"   MOVE "5616078" TO 口座番号Ｗ
           WHEN "06133342"   MOVE "5616086" TO 口座番号Ｗ
           WHEN "63133342"   MOVE "5616086" TO 口座番号Ｗ
           WHEN "06133375"   MOVE "5616094" TO 口座番号Ｗ
           WHEN "06133391"   MOVE "5616108" TO 口座番号Ｗ
           WHEN "06133417"   MOVE "5616116" TO 口座番号Ｗ
           WHEN "63133417"   MOVE "5616116" TO 口座番号Ｗ
           WHEN "06133425"   MOVE "5616124" TO 口座番号Ｗ
           WHEN "06133433"   MOVE "5616132" TO 口座番号Ｗ
           WHEN "06133458"   MOVE "5616140" TO 口座番号Ｗ
           WHEN "63133458"   MOVE "5616140" TO 口座番号Ｗ
           WHEN "06133474"   MOVE "5616159" TO 口座番号Ｗ
           WHEN "06133516"   MOVE "5616167" TO 口座番号Ｗ
           WHEN "06133540"   MOVE "5616175" TO 口座番号Ｗ
           WHEN "06133565"   MOVE "5616183" TO 口座番号Ｗ
           WHEN "06133573"   MOVE "5616191" TO 口座番号Ｗ
           WHEN "06133607"   MOVE "5616205" TO 口座番号Ｗ
           WHEN "06133615"   MOVE "5616213" TO 口座番号Ｗ
           WHEN "06133623"   MOVE "5616221" TO 口座番号Ｗ
           WHEN "06133631"   MOVE "5616248" TO 口座番号Ｗ
           WHEN "06133649"   MOVE "5616256" TO 口座番号Ｗ
           WHEN "06133672"   MOVE "5616264" TO 口座番号Ｗ
           WHEN "06133714"   MOVE "5616272" TO 口座番号Ｗ
           WHEN "06133730"   MOVE "5616280" TO 口座番号Ｗ
           WHEN "06141766"   MOVE "5616280" TO 口座番号Ｗ
           WHEN "06231104"   MOVE "5616280" TO 口座番号Ｗ
           WHEN "63133730"   MOVE "5616280" TO 口座番号Ｗ
           WHEN "06133771"   MOVE "5616299" TO 口座番号Ｗ
           WHEN "06133821"   MOVE "5616302" TO 口座番号Ｗ
           WHEN "06133862"   MOVE "5616310" TO 口座番号Ｗ
           WHEN "06133870"   MOVE "5616329" TO 口座番号Ｗ
           WHEN "06133888"   MOVE "5616337" TO 口座番号Ｗ
           WHEN "06133920"   MOVE "5616345" TO 口座番号Ｗ
           WHEN "06133938"   MOVE "5616353" TO 口座番号Ｗ
           WHEN "63133938"   MOVE "5616353" TO 口座番号Ｗ
           WHEN "06133946"   MOVE "5616361" TO 口座番号Ｗ
           WHEN "63133946"   MOVE "5616361" TO 口座番号Ｗ
           WHEN "06133961"   MOVE "5616388" TO 口座番号Ｗ
           WHEN "06134001"   MOVE "5616396" TO 口座番号Ｗ
           WHEN "06134019"   MOVE "5616418" TO 口座番号Ｗ
           WHEN "06134035"   MOVE "5616426" TO 口座番号Ｗ
           WHEN "06134050"   MOVE "5616434" TO 口座番号Ｗ
           WHEN "06134076"   MOVE "5616442" TO 口座番号Ｗ
           WHEN "06134084"   MOVE "5616450" TO 口座番号Ｗ
           WHEN "06134134"   MOVE "5616469" TO 口座番号Ｗ
           WHEN "63134134"   MOVE "5616469" TO 口座番号Ｗ
           WHEN "06134159"   MOVE "5616477" TO 口座番号Ｗ
           WHEN "06134175"   MOVE "5616485" TO 口座番号Ｗ
           WHEN "06134183"   MOVE "5616493" TO 口座番号Ｗ
           WHEN "63134183"   MOVE "5616493" TO 口座番号Ｗ
           WHEN "06134217"   MOVE "5616507" TO 口座番号Ｗ
           WHEN "06134340"   MOVE "5616515" TO 口座番号Ｗ
           WHEN "06134357"   MOVE "5616523" TO 口座番号Ｗ
           WHEN "06134365"   MOVE "5616531" TO 口座番号Ｗ
           WHEN "06134373"   MOVE "5616558" TO 口座番号Ｗ
           WHEN "06134381"   MOVE "5616566" TO 口座番号Ｗ
           WHEN "06134431"   MOVE "5616574" TO 口座番号Ｗ
           WHEN "63134431"   MOVE "5616574" TO 口座番号Ｗ
           WHEN "06134464"   MOVE "5616582" TO 口座番号Ｗ
           WHEN "06134498"   MOVE "5616590" TO 口座番号Ｗ
           WHEN "06134522"   MOVE "5616604" TO 口座番号Ｗ
           WHEN "06134530"   MOVE "5616612" TO 口座番号Ｗ
           WHEN "06134548"   MOVE "5616620" TO 口座番号Ｗ
           WHEN "06134555"   MOVE "5616639" TO 口座番号Ｗ
           WHEN "63134555"   MOVE "5616639" TO 口座番号Ｗ
           WHEN "06134571"   MOVE "5616647" TO 口座番号Ｗ
           WHEN "06134613"   MOVE "5616655" TO 口座番号Ｗ
           WHEN "06134621"   MOVE "5616663" TO 口座番号Ｗ
           WHEN "06134688"   MOVE "5616671" TO 口座番号Ｗ
           WHEN "06134795"   MOVE "5616698" TO 口座番号Ｗ
           WHEN "06134803"   MOVE "5616701" TO 口座番号Ｗ
           WHEN "06134845"   MOVE "5616728" TO 口座番号Ｗ
           WHEN "06134886"   MOVE "5616736" TO 口座番号Ｗ
           WHEN "06134902"   MOVE "5616744" TO 口座番号Ｗ
           WHEN "06134910"   MOVE "5616752" TO 口座番号Ｗ
           WHEN "06134928"   MOVE "5616760" TO 口座番号Ｗ
           WHEN "06231807"   MOVE "5616779" TO 口座番号Ｗ
           WHEN "06134969"   MOVE "5616787" TO 口座番号Ｗ
           WHEN "06135024"   MOVE "5616795" TO 口座番号Ｗ
           WHEN "06135040"   MOVE "5616809" TO 口座番号Ｗ
           WHEN "63135040"   MOVE "5616809" TO 口座番号Ｗ
           WHEN "06135057"   MOVE "5616817" TO 口座番号Ｗ
           WHEN "06135123"   MOVE "5616825" TO 口座番号Ｗ
           WHEN "06135172"   MOVE "5616833" TO 口座番号Ｗ
           WHEN "06135180"   MOVE "5616841" TO 口座番号Ｗ
           WHEN "06135222"   MOVE "5616868" TO 口座番号Ｗ
           WHEN "06135248"   MOVE "5616876" TO 口座番号Ｗ
           WHEN "06135255"   MOVE "5616884" TO 口座番号Ｗ
           WHEN "06135354"   MOVE "5616892" TO 口座番号Ｗ
           WHEN "06135370"   MOVE "5616906" TO 口座番号Ｗ
           WHEN "06135388"   MOVE "5616914" TO 口座番号Ｗ
           WHEN "06135396"   MOVE "5616922" TO 口座番号Ｗ
           WHEN "06135404"   MOVE "5616930" TO 口座番号Ｗ
           WHEN "06135438"   MOVE "5616949" TO 口座番号Ｗ
           WHEN "06135453"   MOVE "5616957" TO 口座番号Ｗ
           WHEN "06135487"   MOVE "5616965" TO 口座番号Ｗ
           WHEN "63135487"   MOVE "5616965" TO 口座番号Ｗ
           WHEN "06135503"   MOVE "5616973" TO 口座番号Ｗ
           WHEN "06135545"   MOVE "5616981" TO 口座番号Ｗ
           WHEN "06135552"   MOVE "5617007" TO 口座番号Ｗ
           WHEN "06135578"   MOVE "5617015" TO 口座番号Ｗ
           WHEN "06135628"   MOVE "5617023" TO 口座番号Ｗ
           WHEN "06135669"   MOVE "5617031" TO 口座番号Ｗ
           WHEN "06135719"   MOVE "5617058" TO 口座番号Ｗ
           WHEN "06135727"   MOVE "5617066" TO 口座番号Ｗ
           WHEN "06135750"   MOVE "5617074" TO 口座番号Ｗ
           WHEN "06135768"   MOVE "5617082" TO 口座番号Ｗ
           WHEN "06135776"   MOVE "5617090" TO 口座番号Ｗ
           WHEN "06135784"   MOVE "5617104" TO 口座番号Ｗ
           WHEN "06135834"   MOVE "5617112" TO 口座番号Ｗ
           WHEN "06135859"   MOVE "5617120" TO 口座番号Ｗ
           WHEN "06135891"   MOVE "5617139" TO 口座番号Ｗ
           WHEN "06135909"   MOVE "5617147" TO 口座番号Ｗ
           WHEN "06135917"   MOVE "5617155" TO 口座番号Ｗ
           WHEN "06135990"   MOVE "5617163" TO 口座番号Ｗ
           WHEN "06136006"   MOVE "5617171" TO 口座番号Ｗ
           WHEN "06136063"   MOVE "5617198" TO 口座番号Ｗ
           WHEN "06136097"   MOVE "5617201" TO 口座番号Ｗ
           WHEN "06136162"   MOVE "5617228" TO 口座番号Ｗ
           WHEN "06136196"   MOVE "5617236" TO 口座番号Ｗ
           WHEN "63136196"   MOVE "5617236" TO 口座番号Ｗ
           WHEN "06136246"   MOVE "5617244" TO 口座番号Ｗ
           WHEN "06136279"   MOVE "5617252" TO 口座番号Ｗ
           WHEN "06136287"   MOVE "5617260" TO 口座番号Ｗ
           WHEN "63136287"   MOVE "5617260" TO 口座番号Ｗ
           WHEN "06136295"   MOVE "5617279" TO 口座番号Ｗ
           WHEN "06136345"   MOVE "5617287" TO 口座番号Ｗ
           WHEN "06136378"   MOVE "5617295" TO 口座番号Ｗ
           WHEN "06136394"   MOVE "5617309" TO 口座番号Ｗ
           WHEN "06136410"   MOVE "5617317" TO 口座番号Ｗ
           WHEN "06136428"   MOVE "5617325" TO 口座番号Ｗ
           WHEN "06136436"   MOVE "5617333" TO 口座番号Ｗ
           WHEN "06136477"   MOVE "5617341" TO 口座番号Ｗ
           WHEN "63136477"   MOVE "5617341" TO 口座番号Ｗ
           WHEN "06136493"   MOVE "5617368" TO 口座番号Ｗ
           WHEN "06136501"   MOVE "5617376" TO 口座番号Ｗ
           WHEN "06136519"   MOVE "5617384" TO 口座番号Ｗ
           WHEN "06136550"   MOVE "5617392" TO 口座番号Ｗ
           WHEN "06136568"   MOVE "5617406" TO 口座番号Ｗ
           WHEN "06136634"   MOVE "5617422" TO 口座番号Ｗ
           WHEN "06136642"   MOVE "5617430" TO 口座番号Ｗ
           WHEN "06136659"   MOVE "5617449" TO 口座番号Ｗ
           WHEN "06136709"   MOVE "5617457" TO 口座番号Ｗ
           WHEN "06136717"   MOVE "5617465" TO 口座番号Ｗ
           WHEN "06136741"   MOVE "5617473" TO 口座番号Ｗ
           WHEN "06136758"   MOVE "5617481" TO 口座番号Ｗ
           WHEN "06136774"   MOVE "5617503" TO 口座番号Ｗ
           WHEN "06136790"   MOVE "5617511" TO 口座番号Ｗ
           WHEN "06136881"   MOVE "5617538" TO 口座番号Ｗ
           WHEN "06136915"   MOVE "5617546" TO 口座番号Ｗ
           WHEN "06136923"   MOVE "5617554" TO 口座番号Ｗ
           WHEN "06136956"   MOVE "5617562" TO 口座番号Ｗ
           WHEN "06137079"   MOVE "5617570" TO 口座番号Ｗ
           WHEN "06137087"   MOVE "5617589" TO 口座番号Ｗ
           WHEN "06137103"   MOVE "5617597" TO 口座番号Ｗ
           WHEN "06137202"   MOVE "5617600" TO 口座番号Ｗ
           WHEN "06137210"   MOVE "5617619" TO 口座番号Ｗ
           WHEN "06137236"   MOVE "5617627" TO 口座番号Ｗ
           WHEN "06137251"   MOVE "5617635" TO 口座番号Ｗ
           WHEN "06137277"   MOVE "5617643" TO 口座番号Ｗ
           WHEN "06137301"   MOVE "5617651" TO 口座番号Ｗ
           WHEN "06137327"   MOVE "5617678" TO 口座番号Ｗ
           WHEN "06137335"   MOVE "5617686" TO 口座番号Ｗ
           WHEN "06137350"   MOVE "5617694" TO 口座番号Ｗ
           WHEN "06137368"   MOVE "5617708" TO 口座番号Ｗ
           WHEN "06137376"   MOVE "5617716" TO 口座番号Ｗ
           WHEN "06137384"   MOVE "5617724" TO 口座番号Ｗ
           WHEN "06137418"   MOVE "5617732" TO 口座番号Ｗ
           WHEN "06137442"   MOVE "5617740" TO 口座番号Ｗ
           WHEN "06137491"   MOVE "5617759" TO 口座番号Ｗ
           WHEN "06137525"   MOVE "5617767" TO 口座番号Ｗ
           WHEN "06137566"   MOVE "5617775" TO 口座番号Ｗ
           WHEN "06137582"   MOVE "5617783" TO 口座番号Ｗ
           WHEN "06137590"   MOVE "5617791" TO 口座番号Ｗ
           WHEN "06137608"   MOVE "5617805" TO 口座番号Ｗ
           WHEN "06137640"   MOVE "5617813" TO 口座番号Ｗ
           WHEN "06137665"   MOVE "5617821" TO 口座番号Ｗ
           WHEN "06137681"   MOVE "5617848" TO 口座番号Ｗ
           WHEN "06137723"   MOVE "5617856" TO 口座番号Ｗ
           WHEN "06137772"   MOVE "5617864" TO 口座番号Ｗ
           WHEN "06137798"   MOVE "5617880" TO 口座番号Ｗ
           WHEN "06137855"   MOVE "5617899" TO 口座番号Ｗ
           WHEN "63137855"   MOVE "5617899" TO 口座番号Ｗ
           WHEN "06137863"   MOVE "5617902" TO 口座番号Ｗ
           WHEN "06137897"   MOVE "5617910" TO 口座番号Ｗ
           WHEN "06137913"   MOVE "5617929" TO 口座番号Ｗ
           WHEN "06137947"   MOVE "5617937" TO 口座番号Ｗ
           WHEN "06137996"   MOVE "5617945" TO 口座番号Ｗ
           WHEN "06138010"   MOVE "5617953" TO 口座番号Ｗ
           WHEN "06138051"   MOVE "5617961" TO 口座番号Ｗ
           WHEN "06138077"   MOVE "5617988" TO 口座番号Ｗ
           WHEN "06138085"   MOVE "5617996" TO 口座番号Ｗ
           WHEN "06138093"   MOVE "5618003" TO 口座番号Ｗ
           WHEN "06138119"   MOVE "5618011" TO 口座番号Ｗ
           WHEN "06138127"   MOVE "5618038" TO 口座番号Ｗ
           WHEN "06138143"   MOVE "5618046" TO 口座番号Ｗ
           WHEN "06138150"   MOVE "5618054" TO 口座番号Ｗ
           WHEN "63138150"   MOVE "5618054" TO 口座番号Ｗ
           WHEN "06138168"   MOVE "5618062" TO 口座番号Ｗ
           WHEN "06138192"   MOVE "5618070" TO 口座番号Ｗ
           WHEN "06138226"   MOVE "5618089" TO 口座番号Ｗ
           WHEN "06231930"   MOVE "5618097" TO 口座番号Ｗ
           WHEN "06138309"   MOVE "5618100" TO 口座番号Ｗ
           WHEN "06138341"   MOVE "5618119" TO 口座番号Ｗ
           WHEN "06138424"   MOVE "5618127" TO 口座番号Ｗ
           WHEN "06138432"   MOVE "5618135" TO 口座番号Ｗ
           WHEN "06138440"   MOVE "5618143" TO 口座番号Ｗ
           WHEN "06138457"   MOVE "5618151" TO 口座番号Ｗ
           WHEN "06138465"   MOVE "5618178" TO 口座番号Ｗ
           WHEN "06138481"   MOVE "5618186" TO 口座番号Ｗ
           WHEN "06138499"   MOVE "5618194" TO 口座番号Ｗ
           WHEN "06138515"   MOVE "5618208" TO 口座番号Ｗ
           WHEN "06138549"   MOVE "5618216" TO 口座番号Ｗ
           WHEN "06138564"   MOVE "5618224" TO 口座番号Ｗ
           WHEN "06138580"   MOVE "5618232" TO 口座番号Ｗ
           WHEN "63138580"   MOVE "5618232" TO 口座番号Ｗ
           WHEN "06138663"   MOVE "5618240" TO 口座番号Ｗ
           WHEN "06138689"   MOVE "5618259" TO 口座番号Ｗ
           WHEN "06138705"   MOVE "5618267" TO 口座番号Ｗ
           WHEN "06138713"   MOVE "5618275" TO 口座番号Ｗ
           WHEN "06138721"   MOVE "5618283" TO 口座番号Ｗ
           WHEN "06138796"   MOVE "5618291" TO 口座番号Ｗ
           WHEN "63138796"   MOVE "5618291" TO 口座番号Ｗ
           WHEN "06139067"   MOVE "5618305" TO 口座番号Ｗ
           WHEN "06139083"   MOVE "5618313" TO 口座番号Ｗ
           WHEN "06139117"   MOVE "5618321" TO 口座番号Ｗ
           WHEN "06139216"   MOVE "5618348" TO 口座番号Ｗ
           WHEN "06271191"   MOVE "5618348" TO 口座番号Ｗ
           WHEN "06130025"   MOVE "5618356" TO 口座番号Ｗ
           WHEN "06130157"   MOVE "5618364" TO 口座番号Ｗ
           WHEN "06130512"   MOVE "5618372" TO 口座番号Ｗ
           WHEN "06131254"   MOVE "5618380" TO 口座番号Ｗ
           WHEN "06132815"   MOVE "5618399" TO 口座番号Ｗ
           WHEN "06133409"   MOVE "5618402" TO 口座番号Ｗ
           WHEN "06133532"   MOVE "5618410" TO 口座番号Ｗ
           WHEN "06133722"   MOVE "5618429" TO 口座番号Ｗ
           WHEN "06133854"   MOVE "5618437" TO 口座番号Ｗ
           WHEN "06134753"   MOVE "5618445" TO 口座番号Ｗ
           WHEN "06134787"   MOVE "5618453" TO 口座番号Ｗ
           WHEN "06231971"   MOVE "5618461" TO 口座番号Ｗ
           WHEN "06136535"   MOVE "5618488" TO 口座番号Ｗ
           WHEN "06137467"   MOVE "5618496" TO 口座番号Ｗ
           WHEN "06137905"   MOVE "5618518" TO 口座番号Ｗ
           WHEN "06138184"   MOVE "5618526" TO 口座番号Ｗ
           WHEN "06138382"   MOVE "5618534" TO 口座番号Ｗ
           WHEN "06138572"   MOVE "5618542" TO 口座番号Ｗ
           WHEN "06139075"   MOVE "5618550" TO 口座番号Ｗ
           WHEN "06139141"   MOVE "5618569" TO 口座番号Ｗ
           WHEN "06131528"   MOVE "5618577" TO 口座番号Ｗ
           WHEN "06133136"   MOVE "5618585" TO 口座番号Ｗ
           WHEN "06133334"   MOVE "5618593" TO 口座番号Ｗ
           WHEN "06134241"   MOVE "5618607" TO 口座番号Ｗ
           WHEN "06134670"   MOVE "5618615" TO 口座番号Ｗ
           WHEN "06134829"   MOVE "5618623" TO 口座番号Ｗ
           WHEN "06135636"   MOVE "5618631" TO 口座番号Ｗ
           WHEN "06135974"   MOVE "5618658" TO 口座番号Ｗ
           WHEN "06136691"   MOVE "5618666" TO 口座番号Ｗ
           WHEN "06136808"   MOVE "5618674" TO 口座番号Ｗ
           WHEN "06137004"   MOVE "5618682" TO 口座番号Ｗ
           WHEN "06137400"   MOVE "5618690" TO 口座番号Ｗ
           WHEN "06137541"   MOVE "5618704" TO 口座番号Ｗ
           WHEN "06137822"   MOVE "5618712" TO 口座番号Ｗ
           WHEN "06138523"   MOVE "5618720" TO 口座番号Ｗ
           WHEN "06139190"   MOVE "5618739" TO 口座番号Ｗ
           WHEN "31130016"   MOVE "5618747" TO 口座番号Ｗ
           WHEN "31130032"   MOVE "5618755" TO 口座番号Ｗ
           WHEN "31130073"   MOVE "5618763" TO 口座番号Ｗ
           WHEN "31130131"   MOVE "5618771" TO 口座番号Ｗ
           WHEN "31130222"   MOVE "5618798" TO 口座番号Ｗ
           WHEN "31130248"   MOVE "5618801" TO 口座番号Ｗ
           WHEN "31130305"   MOVE "5618828" TO 口座番号Ｗ
           WHEN "31130479"   MOVE "5618836" TO 口座番号Ｗ
           WHEN "31130511"   MOVE "5618844" TO 口座番号Ｗ
           WHEN "31130537"   MOVE "5618852" TO 口座番号Ｗ
           WHEN "31130594"   MOVE "5618860" TO 口座番号Ｗ
           WHEN "31130685"   MOVE "5618879" TO 口座番号Ｗ
           WHEN "31130842"   MOVE "5618887" TO 口座番号Ｗ
           WHEN "31110257"   MOVE "5618895" TO 口座番号Ｗ
           WHEN "31131105"   MOVE "5618895" TO 口座番号Ｗ
           WHEN "31170178"   MOVE "5618895" TO 口座番号Ｗ
           WHEN "31430192"   MOVE "5618895" TO 口座番号Ｗ
           WHEN "31131147"   MOVE "5618909" TO 口座番号Ｗ
           WHEN "31131188"   MOVE "5618917" TO 口座番号Ｗ
           WHEN "31131261"   MOVE "5618925" TO 口座番号Ｗ
           WHEN "31131295"   MOVE "5618933" TO 口座番号Ｗ
           WHEN "31131311"   MOVE "5618941" TO 口座番号Ｗ
           WHEN "31131394"   MOVE "5618968" TO 口座番号Ｗ
           WHEN "31131444"   MOVE "5618976" TO 口座番号Ｗ
           WHEN "31131535"   MOVE "5618984" TO 口座番号Ｗ
           WHEN "32130213"   MOVE "5618992" TO 口座番号Ｗ
           WHEN "32130411"   MOVE "5619018" TO 口座番号Ｗ
           WHEN "33130014"   MOVE "5619026" TO 口座番号Ｗ
           WHEN "33130030"   MOVE "5619034" TO 口座番号Ｗ
           WHEN "34130013"   MOVE "5619042" TO 口座番号Ｗ
           WHEN "34130021"   MOVE "5619050" TO 口座番号Ｗ
           WHEN "31131410"   MOVE "5619069" TO 口座番号Ｗ
           WHEN "31131741"   MOVE "5619077" TO 口座番号Ｗ
           WHEN "31131774"   MOVE "5619085" TO 口座番号Ｗ
           WHEN "06232193"   MOVE "5619093" TO 口座番号Ｗ
           WHEN "06271795"   MOVE "5619107" TO 口座番号Ｗ
           WHEN "06271829"   MOVE "5619115" TO 口座番号Ｗ
           WHEN "06271936"   MOVE "5619123" TO 口座番号Ｗ
           WHEN "06272322"   MOVE "5619131" TO 口座番号Ｗ
           WHEN "41145004"   MOVE "5619158" TO 口座番号Ｗ
           WHEN "41145012"   MOVE "5619158" TO 口座番号Ｗ
           WHEN "41145020"   MOVE "5619158" TO 口座番号Ｗ
           WHEN "41145038"   MOVE "5619158" TO 口座番号Ｗ
           WHEN "41145046"   MOVE "5619158" TO 口座番号Ｗ
           WHEN "41145053"   MOVE "5619158" TO 口座番号Ｗ
           WHEN "41145061"   MOVE "5619158" TO 口座番号Ｗ
           WHEN "41145079"   MOVE "5619158" TO 口座番号Ｗ
           WHEN "06270342"   MOVE "5619166" TO 口座番号Ｗ
           WHEN "63270342"   MOVE "5619166" TO 口座番号Ｗ
           WHEN "06270524"   MOVE "5619174" TO 口座番号Ｗ
           WHEN "06260533"   MOVE "5619182" TO 口座番号Ｗ
           WHEN "06270680"   MOVE "5619190" TO 口座番号Ｗ
           WHEN "06270748"   MOVE "5619204" TO 口座番号Ｗ
           WHEN "06270797"   MOVE "5619212" TO 口座番号Ｗ
           WHEN "63270797"   MOVE "5619212" TO 口座番号Ｗ
           WHEN "06270896"   MOVE "5619220" TO 口座番号Ｗ
           WHEN "06270912"   MOVE "5619239" TO 口座番号Ｗ
           WHEN "06270953"   MOVE "5619247" TO 口座番号Ｗ
           WHEN "06271225"   MOVE "5619255" TO 口座番号Ｗ
           WHEN "06271274"   MOVE "5619263" TO 口座番号Ｗ
           WHEN "06271563"   MOVE "5619271" TO 口座番号Ｗ
           WHEN "63271563"   MOVE "5619271" TO 口座番号Ｗ
           WHEN "06271654"   MOVE "5619298" TO 口座番号Ｗ
           WHEN "06271787"   MOVE "5619301" TO 口座番号Ｗ
           WHEN "63271787"   MOVE "5619301" TO 口座番号Ｗ
           WHEN "41140062"   MOVE "5619328" TO 口座番号Ｗ
           WHEN "41140179"   MOVE "5619336" TO 口座番号Ｗ
           WHEN "06132724"   MOVE "5619344" TO 口座番号Ｗ
           WHEN "06136220"   MOVE "5619352" TO 口座番号Ｗ
           WHEN "06139182"   MOVE "5619360" TO 口座番号Ｗ
           WHEN "67110312"   MOVE "5619379" TO 口座番号Ｗ
           WHEN "67110338"   MOVE "5619387" TO 口座番号Ｗ
           WHEN "67110478"   MOVE "5619395" TO 口座番号Ｗ
           WHEN "67110502"   MOVE "5619409" TO 口座番号Ｗ
           WHEN "67110791"   MOVE "5619417" TO 口座番号Ｗ
           WHEN "06273312"   MOVE "5619425" TO 口座番号Ｗ
           WHEN "67138263"   MOVE "5619433" TO 口座番号Ｗ
           WHEN "67138339"   MOVE "5619441" TO 口座番号Ｗ
           WHEN "67138495"   MOVE "5619468" TO 口座番号Ｗ
           WHEN "06139307"   MOVE "5619476" TO 口座番号Ｗ
           WHEN "06160444"   MOVE "5619484" TO 口座番号Ｗ
           WHEN "06170229"   MOVE "5619492" TO 口座番号Ｗ
           WHEN "06220057"   MOVE "5619506" TO 口座番号Ｗ
           WHEN "06220834"   MOVE "5619514" TO 口座番号Ｗ
           WHEN "27138304"   MOVE "5619522" TO 口座番号Ｗ
           WHEN "27138361"   MOVE "5619530" TO 口座番号Ｗ
           WHEN "81136244"   MOVE "5619549" TO 口座番号Ｗ
           WHEN "81137242"   MOVE "5619549" TO 口座番号Ｗ
           WHEN "88131248"   MOVE "5619549" TO 口座番号Ｗ
           WHEN "88138243"   MOVE "5619549" TO 口座番号Ｗ
           WHEN "81137358"   MOVE "5619557" TO 口座番号Ｗ
           WHEN "88131354"   MOVE "5619557" TO 口座番号Ｗ
           WHEN "88138359"   MOVE "5619557" TO 口座番号Ｗ
           WHEN "41139015"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139023"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139031"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139049"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139056"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139064"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139072"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139080"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139098"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139106"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139114"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139122"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139130"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139148"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139155"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139163"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139171"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139189"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139197"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139205"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139213"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139221"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139239"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139247"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139254"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139262"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139270"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139288"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139296"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139304"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139312"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139320"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139338"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139346"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139353"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139361"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139379"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139387"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139395"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139403"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139411"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139429"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139437"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139445"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139452"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139460"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139478"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139486"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139494"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139502"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139510"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139528"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139536"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139544"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139551"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139569"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139577"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139585"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139593"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139601"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139619"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139627"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139635"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "41139643"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "8013500"    MOVE "5619565" TO 口座番号Ｗ
           WHEN "80135015"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80135023"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80135031"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80135106"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80135122"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80135148"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80135155"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80135171"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80135197"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80135213"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80135221"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80135239"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80135353"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80135478"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136039"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136047"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136054"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136062"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136070"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136088"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136096"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136104"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136112"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136120"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136138"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136161"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136179"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136187"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136195"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136211"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136229"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136237"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136278"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136294"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136328"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80136427"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137028"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137029"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137037"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137045"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137052"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137060"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137078"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137086"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137094"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137110"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137128"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137144"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137169"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137177"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137185"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137193"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137201"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137219"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137227"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137235"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137250"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137276"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137318"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137326"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137342"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137359"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137383"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137391"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137425"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137433"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137458"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137474"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "80137482"   MOVE "5619565" TO 口座番号Ｗ
           WHEN "81136293"   MOVE "5619573" TO 口座番号Ｗ
           WHEN "81137291"   MOVE "5619573" TO 口座番号Ｗ
           WHEN "88132295"   MOVE "5619573" TO 口座番号Ｗ
           WHEN "88138292"   MOVE "5619573" TO 口座番号Ｗ
           WHEN "81136459"   MOVE "5619581" TO 口座番号Ｗ
           WHEN "81137457"   MOVE "5619581" TO 口座番号Ｗ
           WHEN "88131453"   MOVE "5619581" TO 口座番号Ｗ
           WHEN "88138458"   MOVE "5619581" TO 口座番号Ｗ
           WHEN "81136186"   MOVE "5619603" TO 口座番号Ｗ
           WHEN "81137184"   MOVE "5619603" TO 口座番号Ｗ
           WHEN "88135181"   MOVE "5619603" TO 口座番号Ｗ
           WHEN "88138185"   MOVE "5619603" TO 口座番号Ｗ
           WHEN "81136178"   MOVE "5619611" TO 口座番号Ｗ
           WHEN "81137176"   MOVE "5619611" TO 口座番号Ｗ
           WHEN "88132170"   MOVE "5619611" TO 口座番号Ｗ
           WHEN "88133178"   MOVE "5619611" TO 口座番号Ｗ
           WHEN "88138177"   MOVE "5619611" TO 口座番号Ｗ
           WHEN "81137010"   MOVE "5619638" TO 口座番号Ｗ
           WHEN "88133012"   MOVE "5619638" TO 口座番号Ｗ
           WHEN "88138011"   MOVE "5619638" TO 口座番号Ｗ
           WHEN "41405069"   MOVE "5619646" TO 口座番号Ｗ
           WHEN "80405012"   MOVE "5619646" TO 口座番号Ｗ
           WHEN "80405020"   MOVE "5619646" TO 口座番号Ｗ
           WHEN "80405038"   MOVE "5619646" TO 口座番号Ｗ
           WHEN "80405046"   MOVE "5619646" TO 口座番号Ｗ
           WHEN "80405053"   MOVE "5619646" TO 口座番号Ｗ
           WHEN "80405061"   MOVE "5619646" TO 口座番号Ｗ
           WHEN "80405079"   MOVE "5619646" TO 口座番号Ｗ
           WHEN "80405087"   MOVE "5619646" TO 口座番号Ｗ
           WHEN "90405010"   MOVE "5619646" TO 口座番号Ｗ
           WHEN "90405028"   MOVE "5619646" TO 口座番号Ｗ
           WHEN "90405036"   MOVE "5619646" TO 口座番号Ｗ
           WHEN "90405044"   MOVE "5619646" TO 口座番号Ｗ
           WHEN "90405051"   MOVE "5619646" TO 口座番号Ｗ
           WHEN "90405069"   MOVE "5619646" TO 口座番号Ｗ
           WHEN "90405077"   MOVE "5619646" TO 口座番号Ｗ
           WHEN "90405085"   MOVE "5619646" TO 口座番号Ｗ
           WHEN "90405093"   MOVE "5619646" TO 口座番号Ｗ
           WHEN "81405029"   MOVE "5619646" TO 口座番号Ｗ
           WHEN "01140011"   MOVE "5619654" TO 口座番号Ｗ
           WHEN "03140019"   MOVE "5619654" TO 口座番号Ｗ
           WHEN "04140018"   MOVE "5619654" TO 口座番号Ｗ
           WHEN "01150010"   MOVE "5619662" TO 口座番号Ｗ
           WHEN "03150018"   MOVE "5619662" TO 口座番号Ｗ
           WHEN "04150017"   MOVE "5619662" TO 口座番号Ｗ
           WHEN "01160019"   MOVE "5619670" TO 口座番号Ｗ
           WHEN "03160017"   MOVE "5619670" TO 口座番号Ｗ
           WHEN "04160016"   MOVE "5619670" TO 口座番号Ｗ
           WHEN "01170018"   MOVE "5619689" TO 口座番号Ｗ
           WHEN "03170016"   MOVE "5619689" TO 口座番号Ｗ
           WHEN "04170015"   MOVE "5619689" TO 口座番号Ｗ
           WHEN "01180017"   MOVE "5619697" TO 口座番号Ｗ
           WHEN "03180015"   MOVE "5619697" TO 口座番号Ｗ
           WHEN "04180014"   MOVE "5619697" TO 口座番号Ｗ
           WHEN "01190016"   MOVE "5619700" TO 口座番号Ｗ
           WHEN "03190014"   MOVE "5619700" TO 口座番号Ｗ
           WHEN "04190013"   MOVE "5619700" TO 口座番号Ｗ
           WHEN "01200013"   MOVE "5619719" TO 口座番号Ｗ
           WHEN "03200011"   MOVE "5619719" TO 口座番号Ｗ
           WHEN "04200010"   MOVE "5619719" TO 口座番号Ｗ
           WHEN "01210012"   MOVE "5619727" TO 口座番号Ｗ
           WHEN "03210010"   MOVE "5619727" TO 口座番号Ｗ
           WHEN "04210019"   MOVE "5619727" TO 口座番号Ｗ
           WHEN "01220011"   MOVE "5619735" TO 口座番号Ｗ
           WHEN "03220019"   MOVE "5619735" TO 口座番号Ｗ
           WHEN "04220018"   MOVE "5619735" TO 口座番号Ｗ
           WHEN "01230010"   MOVE "5619743" TO 口座番号Ｗ
           WHEN "03230018"   MOVE "5619743" TO 口座番号Ｗ
           WHEN "04230017"   MOVE "5619743" TO 口座番号Ｗ
           WHEN "01240019"   MOVE "5619751" TO 口座番号Ｗ
           WHEN "03240017"   MOVE "5619751" TO 口座番号Ｗ
           WHEN "04240016"   MOVE "5619751" TO 口座番号Ｗ
           WHEN "01250018"   MOVE "5619778" TO 口座番号Ｗ
           WHEN "03250016"   MOVE "5619778" TO 口座番号Ｗ
           WHEN "04250015"   MOVE "5619778" TO 口座番号Ｗ
           WHEN "01260017"   MOVE "5619786" TO 口座番号Ｗ
           WHEN "03260015"   MOVE "5619786" TO 口座番号Ｗ
           WHEN "04260014"   MOVE "5619786" TO 口座番号Ｗ
           WHEN "01270016"   MOVE "5619794" TO 口座番号Ｗ
           WHEN "03270014"   MOVE "5619794" TO 口座番号Ｗ
           WHEN "04270013"   MOVE "5619794" TO 口座番号Ｗ
           WHEN "01280015"   MOVE "5619808" TO 口座番号Ｗ
           WHEN "03280013"   MOVE "5619808" TO 口座番号Ｗ
           WHEN "04280012"   MOVE "5619808" TO 口座番号Ｗ
           WHEN "01290014"   MOVE "5619816" TO 口座番号Ｗ
           WHEN "03290012"   MOVE "5619816" TO 口座番号Ｗ
           WHEN "04290011"   MOVE "5619816" TO 口座番号Ｗ
           WHEN "01300011"   MOVE "5619824" TO 口座番号Ｗ
           WHEN "03300019"   MOVE "5619824" TO 口座番号Ｗ
           WHEN "04300018"   MOVE "5619824" TO 口座番号Ｗ
           WHEN "01310010"   MOVE "5619832" TO 口座番号Ｗ
           WHEN "03310018"   MOVE "5619832" TO 口座番号Ｗ
           WHEN "04310017"   MOVE "5619832" TO 口座番号Ｗ
           WHEN "01320019"   MOVE "5619840" TO 口座番号Ｗ
           WHEN "03320017"   MOVE "5619840" TO 口座番号Ｗ
           WHEN "04320016"   MOVE "5619840" TO 口座番号Ｗ
           WHEN "01330018"   MOVE "5619859" TO 口座番号Ｗ
           WHEN "03330016"   MOVE "5619859" TO 口座番号Ｗ
           WHEN "04330015"   MOVE "5619859" TO 口座番号Ｗ
           WHEN "01340017"   MOVE "5619867" TO 口座番号Ｗ
           WHEN "03340015"   MOVE "5619867" TO 口座番号Ｗ
           WHEN "04340014"   MOVE "5619867" TO 口座番号Ｗ
           WHEN "01350016"   MOVE "5619875" TO 口座番号Ｗ
           WHEN "03350014"   MOVE "5619875" TO 口座番号Ｗ
           WHEN "04350013"   MOVE "5619875" TO 口座番号Ｗ
           WHEN "01360015"   MOVE "5619883" TO 口座番号Ｗ
           WHEN "03360013"   MOVE "5619883" TO 口座番号Ｗ
           WHEN "04360012"   MOVE "5619883" TO 口座番号Ｗ
           WHEN "01370014"   MOVE "5619891" TO 口座番号Ｗ
           WHEN "03370012"   MOVE "5619891" TO 口座番号Ｗ
           WHEN "04370011"   MOVE "5619891" TO 口座番号Ｗ
           WHEN "01380013"   MOVE "5619905" TO 口座番号Ｗ
           WHEN "03380011"   MOVE "5619905" TO 口座番号Ｗ
           WHEN "04380010"   MOVE "5619905" TO 口座番号Ｗ
           WHEN "01390012"   MOVE "5619913" TO 口座番号Ｗ
           WHEN "03390010"   MOVE "5619913" TO 口座番号Ｗ
           WHEN "04390019"   MOVE "5619913" TO 口座番号Ｗ
           WHEN "01400019"   MOVE "5619921" TO 口座番号Ｗ
           WHEN "03400017"   MOVE "5619921" TO 口座番号Ｗ
           WHEN "04400016"   MOVE "5619921" TO 口座番号Ｗ
           WHEN "01410018"   MOVE "5619948" TO 口座番号Ｗ
           WHEN "03410016"   MOVE "5619948" TO 口座番号Ｗ
           WHEN "04410015"   MOVE "5619948" TO 口座番号Ｗ
           WHEN "01420017"   MOVE "5619956" TO 口座番号Ｗ
           WHEN "03420015"   MOVE "5619956" TO 口座番号Ｗ
           WHEN "04420014"   MOVE "5619956" TO 口座番号Ｗ
           WHEN "01430016"   MOVE "5619964" TO 口座番号Ｗ
           WHEN "03430014"   MOVE "5619964" TO 口座番号Ｗ
           WHEN "04430013"   MOVE "5619964" TO 口座番号Ｗ
           WHEN "01440015"   MOVE "5619972" TO 口座番号Ｗ
           WHEN "03440013"   MOVE "5619972" TO 口座番号Ｗ
           WHEN "04440012"   MOVE "5619972" TO 口座番号Ｗ
           WHEN "01450014"   MOVE "5619980" TO 口座番号Ｗ
           WHEN "03450012"   MOVE "5619980" TO 口座番号Ｗ
           WHEN "04450011"   MOVE "5619980" TO 口座番号Ｗ
           WHEN "01460013"   MOVE "5619999" TO 口座番号Ｗ
           WHEN "03460011"   MOVE "5619999" TO 口座番号Ｗ
           WHEN "04460010"   MOVE "5619999" TO 口座番号Ｗ
           WHEN "01470012"   MOVE "5620008" TO 口座番号Ｗ
           WHEN "03470010"   MOVE "5620008" TO 口座番号Ｗ
           WHEN "04470019"   MOVE "5620008" TO 口座番号Ｗ
           WHEN "39131016"   MOVE "5620326" TO 口座番号Ｗ
           WHEN "39131024"   MOVE "5620334" TO 口座番号Ｗ
           WHEN "39131032"   MOVE "5620342" TO 口座番号Ｗ
           WHEN "39131040"   MOVE "5620350" TO 口座番号Ｗ
           WHEN "39131057"   MOVE "5620369" TO 口座番号Ｗ
           WHEN "39131065"   MOVE "5620377" TO 口座番号Ｗ
           WHEN "39131073"   MOVE "5620385" TO 口座番号Ｗ
           WHEN "39131081"   MOVE "5620393" TO 口座番号Ｗ
           WHEN "39131099"   MOVE "5620407" TO 口座番号Ｗ
           WHEN "39131107"   MOVE "5620415" TO 口座番号Ｗ
           WHEN "39131115"   MOVE "5620423" TO 口座番号Ｗ
           WHEN "39131123"   MOVE "5620431" TO 口座番号Ｗ
           WHEN "39131131"   MOVE "5620458" TO 口座番号Ｗ
           WHEN "39131149"   MOVE "5620466" TO 口座番号Ｗ
           WHEN "39131156"   MOVE "5620474" TO 口座番号Ｗ
           WHEN "39131164"   MOVE "5620482" TO 口座番号Ｗ
           WHEN "39131172"   MOVE "5620490" TO 口座番号Ｗ
           WHEN "39131180"   MOVE "5620504" TO 口座番号Ｗ
           WHEN "39131198"   MOVE "5620512" TO 口座番号Ｗ
           WHEN "39131206"   MOVE "5620520" TO 口座番号Ｗ
           WHEN "39131214"   MOVE "5620539" TO 口座番号Ｗ
           WHEN "39131222"   MOVE "5620547" TO 口座番号Ｗ
           WHEN "39131230"   MOVE "5620555" TO 口座番号Ｗ
           WHEN "39132014"   MOVE "5620563" TO 口座番号Ｗ
           WHEN "39132022"   MOVE "5620571" TO 口座番号Ｗ
           WHEN "39132030"   MOVE "5620598" TO 口座番号Ｗ
           WHEN "39132048"   MOVE "5620601" TO 口座番号Ｗ
           WHEN "39132055"   MOVE "5620628" TO 口座番号Ｗ
           WHEN "39132063"   MOVE "5620636" TO 口座番号Ｗ
           WHEN "39132071"   MOVE "5620644" TO 口座番号Ｗ
           WHEN "39132089"   MOVE "5620652" TO 口座番号Ｗ
           WHEN "39132097"   MOVE "5620660" TO 口座番号Ｗ
           WHEN "39132105"   MOVE "5620679" TO 口座番号Ｗ
           WHEN "39132113"   MOVE "5620687" TO 口座番号Ｗ
           WHEN "39132121"   MOVE "5620695" TO 口座番号Ｗ
           WHEN "39132139"   MOVE "5620709" TO 口座番号Ｗ
           WHEN "39132147"   MOVE "5620717" TO 口座番号Ｗ
           WHEN "39132154"   MOVE "5620725" TO 口座番号Ｗ
           WHEN "39132188"   MOVE "5620733" TO 口座番号Ｗ
           WHEN "39132196"   MOVE "5620741" TO 口座番号Ｗ
           WHEN "39132204"   MOVE "5620768" TO 口座番号Ｗ
           WHEN "39132212"   MOVE "5620776" TO 口座番号Ｗ
           WHEN "39132220"   MOVE "5620784" TO 口座番号Ｗ
           WHEN "39132238"   MOVE "5620792" TO 口座番号Ｗ
           WHEN "39132246"   MOVE "5620806" TO 口座番号Ｗ
           WHEN "39132253"   MOVE "5620814" TO 口座番号Ｗ
           WHEN "39132279"   MOVE "5620822" TO 口座番号Ｗ
           WHEN "39132287"   MOVE "5620830" TO 口座番号Ｗ
           WHEN "39132295"   MOVE "5620849" TO 口座番号Ｗ
           WHEN "39133038"   MOVE "5620857" TO 口座番号Ｗ
           WHEN "39133053"   MOVE "5620865" TO 口座番号Ｗ
           WHEN "39133079"   MOVE "5620873" TO 口座番号Ｗ
           WHEN "39133087"   MOVE "5620881" TO 口座番号Ｗ
           WHEN "39352018"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39352026"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39352034"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39352042"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39352067"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39352075"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39352083"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39352109"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39352117"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39352125"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39352133"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39352158"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39352166"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39353057"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39353214"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39353412"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39353438"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39353446"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39355029"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39355045"   MOVE "5620946" TO 口座番号Ｗ
           WHEN "39412010"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "39412028"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "39412036"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "39412044"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "39412051"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "39412069"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "39412077"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "39412085"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "39412093"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "39412101"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "39413273"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "39413414"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "39413455"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "39413463"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "39413877"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "39414016"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "39414230"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "39414248"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "39414255"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "39414412"   MOVE "5620954" TO 口座番号Ｗ
           WHEN "07010135"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07010150"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07080120"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07090103"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07100118"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07110026"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07110117"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07110604"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07110612"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07120017"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07120108"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07120504"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07120603"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07130107"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07130123"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07130198"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07130511"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07130610"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07130636"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07140114"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07140122"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07140536"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07140544"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07350515"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07380017"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07420516"   MOVE "0115697" TO 口座番号Ｗ
           WHEN "07470115"   MOVE "0115697" TO 口座番号Ｗ
      */平成25年11月施術分より変更↑↑↑/131126
      *     WHEN "120014"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120022"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120030"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120048"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120055"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120063"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120071"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120089"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120097"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120105"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120113"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120121"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120139"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120147"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120154"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120162"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120170"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120188"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120196"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120204"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120212"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120220"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120238"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120246"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120253"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120261"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120451"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120519"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120527"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120535"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120543"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120550"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120568"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120576"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120584"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120592"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120600"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120618"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120626"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120634"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120642"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120659"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120667"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120675"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120683"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120691"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120709"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120717"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120725"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120733"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120741"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120758"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120766"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120774"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120782"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120790"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120808"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120816"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120824"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120832"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120840"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120857"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120865"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120873"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120881"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120899"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120907"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120915"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120923"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120931"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120949"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120956"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120964"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120972"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120980"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "120998"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "121004"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "121012"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "121020"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "121038"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "121046"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "123018"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "123026"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "123034"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "124008"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "124016"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "124024"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "124032"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "124040"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "124057"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "124065"     MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "19126010"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120013"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120021"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120039"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120047"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120054"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120062"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120070"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120088"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120096"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120104"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120112"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120120"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120138"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120146"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120153"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120161"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120179"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120187"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120195"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120203"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120211"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120229"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120237"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120245"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120252"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120260"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120419"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120518"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120526"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120534"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120542"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120559"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120567"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120575"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120583"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120591"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120609"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120617"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120625"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120633"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120641"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120658"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120666"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120674"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120682"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120690"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120708"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120716"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120724"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120732"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120740"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120757"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120765"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120773"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120781"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120799"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120807"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120815"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120823"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120831"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120849"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120856"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120864"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120872"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120880"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120898"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120906"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120914"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120922"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120930"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120948"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120955"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120963"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120971"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120989"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27120997"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27121003"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27121011"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27121029"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27121037"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27121045"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27124007"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27124015"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27124023"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27124031"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27124049"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27124056"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "27124064"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120023"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120031"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120049"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120056"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120064"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120072"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120080"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120098"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120106"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120114"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120122"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120130"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120148"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120155"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120163"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120171"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120189"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120197"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120205"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120213"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120221"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120239"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120247"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120254"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120262"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120510"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120528"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120536"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120544"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120551"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120569"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120577"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120585"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120593"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120601"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120619"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120627"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120635"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120643"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120650"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120668"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120676"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120684"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120692"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120700"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120718"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120726"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120734"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120742"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120759"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120767"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120775"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120783"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120791"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120809"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120817"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120825"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120833"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120841"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120858"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120866"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120874"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120882"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120890"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120908"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120916"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120924"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120932"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120940"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120957"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120965"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120973"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120981"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41120999"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41121005"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41121013"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41121021"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41121039"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41121047"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41124009"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41124017"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41124025"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41124033"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41124041"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41124058"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "41124066"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120014"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120022"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120030"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120048"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120055"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120063"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120071"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120089"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120097"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120105"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120113"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120121"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120139"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120147"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120154"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120162"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120170"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120188"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120196"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120204"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120212"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120220"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120238"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120246"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120253"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120261"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120519"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120527"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120535"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120543"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120550"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120568"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120576"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120584"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120592"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120600"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120618"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120626"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120634"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120642"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120659"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120667"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120675"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120683"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120691"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120709"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120717"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120725"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120733"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120741"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120758"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120766"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120774"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120782"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120790"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120808"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120816"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120824"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120832"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120840"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120857"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120865"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120873"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120881"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120899"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120907"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120915"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120923"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120931"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120949"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120956"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120964"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120972"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120980"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67120998"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67121004"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67121012"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67121020"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67121038"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67121046"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67124016"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67124024"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67124032"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67124040"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67124057"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "67124065"   MOVE "5610010" TO 口座番号Ｗ
      *     WHEN "110684"     MOVE "5610029" TO 口座番号Ｗ
      *     WHEN "110700"     MOVE "5610037" TO 口座番号Ｗ
      *     WHEN "110783"     MOVE "5610045" TO 口座番号Ｗ
      *     WHEN "138529"     MOVE "5610053" TO 口座番号Ｗ
      *     WHEN "138586"     MOVE "5610061" TO 口座番号Ｗ
      *     WHEN "114108"     MOVE "5610088" TO 口座番号Ｗ
      *     WHEN "138293"     MOVE "5610096" TO 口座番号Ｗ
      *     WHEN "120279"     MOVE "5610118" TO 口座番号Ｗ
      *     WHEN "110015"     MOVE "5610126" TO 口座番号Ｗ
      *     WHEN "110023"     MOVE "5610134" TO 口座番号Ｗ
      *     WHEN "110031"     MOVE "5610142" TO 口座番号Ｗ
      *     WHEN "67110031"   MOVE "5610142" TO 口座番号Ｗ
      *     WHEN "110080"     MOVE "5610150" TO 口座番号Ｗ
      *     WHEN "110098"     MOVE "5610169" TO 口座番号Ｗ
      *     WHEN "110106"     MOVE "5610177" TO 口座番号Ｗ
      *     WHEN "110130"     MOVE "5610185" TO 口座番号Ｗ
      *     WHEN "110148"     MOVE "5610193" TO 口座番号Ｗ
      *     WHEN "110213"     MOVE "5610207" TO 口座番号Ｗ
      *     WHEN "110221"     MOVE "5610215" TO 口座番号Ｗ
      *     WHEN "110296"     MOVE "5610223" TO 口座番号Ｗ
      *     WHEN "110346"     MOVE "5610231" TO 口座番号Ｗ
      *     WHEN "110353"     MOVE "5610258" TO 口座番号Ｗ
      *     WHEN "110361"     MOVE "5610266" TO 口座番号Ｗ
      *     WHEN "110379"     MOVE "5610274" TO 口座番号Ｗ
      *     WHEN "110403"     MOVE "5610282" TO 口座番号Ｗ
      *     WHEN "110411"     MOVE "5610290" TO 口座番号Ｗ
      *     WHEN "110429"     MOVE "5610304" TO 口座番号Ｗ
      *     WHEN "27110428"   MOVE "5610304" TO 口座番号Ｗ
      *     WHEN "41110420"   MOVE "5610304" TO 口座番号Ｗ
      *     WHEN "67110429"   MOVE "5610304" TO 口座番号Ｗ
      *     WHEN "110437"     MOVE "5610312" TO 口座番号Ｗ
      *     WHEN "110759"     MOVE "5610320" TO 口座番号Ｗ
      *     WHEN "110791"     MOVE "5610339" TO 口座番号Ｗ
      *     WHEN "110833"     MOVE "5610347" TO 口座番号Ｗ
      *     WHEN "110841"     MOVE "5610355" TO 口座番号Ｗ
      *     WHEN "110890"     MOVE "5610363" TO 口座番号Ｗ
      *     WHEN "110908"     MOVE "5610371" TO 口座番号Ｗ
      *     WHEN "110932"     MOVE "5610398" TO 口座番号Ｗ
      *     WHEN "110940"     MOVE "5610401" TO 口座番号Ｗ
      *     WHEN "138016"     MOVE "5610428" TO 口座番号Ｗ
      *     WHEN "138024"     MOVE "5610436" TO 口座番号Ｗ
      *     WHEN "138032"     MOVE "5610444" TO 口座番号Ｗ
      *     WHEN "138040"     MOVE "5610452" TO 口座番号Ｗ
      *     WHEN "138057"     MOVE "5610460" TO 口座番号Ｗ
      *     WHEN "138065"     MOVE "5610479" TO 口座番号Ｗ
      *     WHEN "138073"     MOVE "5610487" TO 口座番号Ｗ
      *     WHEN "138081"     MOVE "5610495" TO 口座番号Ｗ
      *     WHEN "138099"     MOVE "5610509" TO 口座番号Ｗ
      *     WHEN "138107"     MOVE "5610517" TO 口座番号Ｗ
      *     WHEN "138115"     MOVE "5610525" TO 口座番号Ｗ
      *     WHEN "138123"     MOVE "5610533" TO 口座番号Ｗ
      *     WHEN "138131"     MOVE "5610541" TO 口座番号Ｗ
      *     WHEN "138149"     MOVE "5610568" TO 口座番号Ｗ
      *     WHEN "138156"     MOVE "5610576" TO 口座番号Ｗ
      *     WHEN "67138156"   MOVE "5610576" TO 口座番号Ｗ
      *     WHEN "138164"     MOVE "5610584" TO 口座番号Ｗ
      *     WHEN "138172"     MOVE "5610592" TO 口座番号Ｗ
      *     WHEN "138180"     MOVE "5610606" TO 口座番号Ｗ
      *     WHEN "138198"     MOVE "5610614" TO 口座番号Ｗ
      *     WHEN "67138198"   MOVE "5610614" TO 口座番号Ｗ
      *     WHEN "138206"     MOVE "5610622" TO 口座番号Ｗ
      *     WHEN "138214"     MOVE "5610630" TO 口座番号Ｗ
      *     WHEN "138222"     MOVE "5610649" TO 口座番号Ｗ
      *     WHEN "27138221"   MOVE "5610649" TO 口座番号Ｗ
      *     WHEN "67138222"   MOVE "5610649" TO 口座番号Ｗ
      *     WHEN "81136228"   MOVE "5610649" TO 口座番号Ｗ
      *     WHEN "81137226"   MOVE "5610649" TO 口座番号Ｗ
      *     WHEN "138230"     MOVE "5610657" TO 口座番号Ｗ
      *     WHEN "27138239"   MOVE "5610657" TO 口座番号Ｗ
      *     WHEN "67138230"   MOVE "5610657" TO 口座番号Ｗ
      *     WHEN "81136236"   MOVE "5610657" TO 口座番号Ｗ
      *     WHEN "81137234"   MOVE "5610657" TO 口座番号Ｗ
      *     WHEN "138248"     MOVE "5610665" TO 口座番号Ｗ
      *     WHEN "138313"     MOVE "5610673" TO 口座番号Ｗ
      *     WHEN "138321"     MOVE "5610681" TO 口座番号Ｗ
      *     WHEN "138347"     MOVE "5610703" TO 口座番号Ｗ
      *     WHEN "138354"     MOVE "5610711" TO 口座番号Ｗ
      *     WHEN "138396"     MOVE "5610738" TO 口座番号Ｗ
      *     WHEN "138479"     MOVE "5610746" TO 口座番号Ｗ
      *     WHEN "138487"     MOVE "5610754" TO 口座番号Ｗ
      *     WHEN "140038"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140046"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140053"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140061"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140079"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140087"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140095"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140103"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140111"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140129"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140137"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140145"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140152"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140160"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140178"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140186"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140517"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140525"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140533"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140541"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140558"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140566"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140574"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140582"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140590"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140608"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140616"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140624"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140632"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140640"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140657"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140665"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140673"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140681"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "140699"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "143016"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "143024"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "143032"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "143040"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "143057"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "143065"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "144006"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "144014"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "144022"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "144030"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "144048"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "144055"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "144063"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "144071"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "144089"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "144097"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "144105"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "144113"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "144121"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "144139"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "144147"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "144154"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "144162"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "144170"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "144188"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "145003"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "145011"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "145029"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "145037"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "145045"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "145052"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "145060"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "145078"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "1914601"    MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "19146018"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140037"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140045"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140052"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140060"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140078"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140086"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140094"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140102"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140110"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140128"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140136"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140144"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140151"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140169"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140177"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140185"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140516"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140524"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140532"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140540"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140557"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140565"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140573"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140581"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140599"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140607"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140615"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140623"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140631"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140649"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140656"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140664"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140672"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140680"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27140698"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27144005"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27144013"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27144021"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27144039"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27144047"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27144054"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27144062"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27144070"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27144088"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27144096"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27144104"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27144112"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27144120"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27144138"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27144146"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27144153"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27144161"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27144179"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27144187"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27145002"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27145010"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27145028"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27145036"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27145044"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27145051"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27145069"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "27145077"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140038"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140046"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140053"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140061"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140079"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140087"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140095"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140103"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140111"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140129"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140137"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140145"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140152"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140160"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140178"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140186"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140517"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140525"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140533"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140541"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140558"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140566"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140574"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140582"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140590"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140608"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140616"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140624"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140632"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140640"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140657"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140665"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140673"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140681"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67140699"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67144014"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67144022"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67144030"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67144048"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67144055"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67144063"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67144071"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67144089"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67144097"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67144105"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67144113"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67144121"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67144139"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67144147"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67144154"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67144162"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67144170"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67144188"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67145011"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67145029"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67145037"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67145045"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67145060"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67145078"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "113027"     MOVE "5610770" TO 口座番号Ｗ
      *     WHEN "113043"     MOVE "5610789" TO 口座番号Ｗ
      *     WHEN "113050"     MOVE "5610797" TO 口座番号Ｗ
      *     WHEN "113068"     MOVE "5610800" TO 口座番号Ｗ
      *     WHEN "133033"     MOVE "5610819" TO 口座番号Ｗ
      *     WHEN "133041"     MOVE "5610827" TO 口座番号Ｗ
      *     WHEN "133066"     MOVE "5610835" TO 口座番号Ｗ
      *     WHEN "133074"     MOVE "5610843" TO 口座番号Ｗ
      *     WHEN "133090"     MOVE "5610851" TO 口座番号Ｗ
      *     WHEN "133132"     MOVE "5610878" TO 口座番号Ｗ
      *     WHEN "133140"     MOVE "5610886" TO 口座番号Ｗ
      *     WHEN "133157"     MOVE "5610894" TO 口座番号Ｗ
      *     WHEN "133165"     MOVE "5610908" TO 口座番号Ｗ
      *     WHEN "133173"     MOVE "5610916" TO 口座番号Ｗ
      *     WHEN "133199"     MOVE "5610924" TO 口座番号Ｗ
      *     WHEN "133207"     MOVE "5610932" TO 口座番号Ｗ
      *     WHEN "133223"     MOVE "5610940" TO 口座番号Ｗ
      *     WHEN "133231"     MOVE "5610959" TO 口座番号Ｗ
      *     WHEN "133249"     MOVE "5610967" TO 口座番号Ｗ
      *     WHEN "133256"     MOVE "5610975" TO 口座番号Ｗ
      *     WHEN "133264"     MOVE "5610983" TO 口座番号Ｗ
      *     WHEN "133272"     MOVE "5610991" TO 口座番号Ｗ
      *     WHEN "133298"     MOVE "5611009" TO 口座番号Ｗ
      *     WHEN "67110106"   MOVE "5611017" TO 口座番号Ｗ
      *     WHEN "67110148"   MOVE "5611025" TO 口座番号Ｗ
      *     WHEN "67110213"   MOVE "5611033" TO 口座番号Ｗ
      *     WHEN "67110221"   MOVE "5611041" TO 口座番号Ｗ
      *     WHEN "67110239"   MOVE "5611068" TO 口座番号Ｗ
      *     WHEN "67110361"   MOVE "5611076" TO 口座番号Ｗ
      *     WHEN "67110387"   MOVE "5611084" TO 口座番号Ｗ
      *     WHEN "67110411"   MOVE "5611092" TO 口座番号Ｗ
      *     WHEN "67110841"   MOVE "5611106" TO 口座番号Ｗ
      *     WHEN "67110890"   MOVE "5611114" TO 口座番号Ｗ
      *     WHEN "67110932"   MOVE "5611122" TO 口座番号Ｗ
      *     WHEN "67138016"   MOVE "5611130" TO 口座番号Ｗ
      *     WHEN "67138024"   MOVE "5611149" TO 口座番号Ｗ
      *     WHEN "67138032"   MOVE "5611157" TO 口座番号Ｗ
      *     WHEN "67138040"   MOVE "5611165" TO 口座番号Ｗ
      *     WHEN "67138057"   MOVE "5611173" TO 口座番号Ｗ
      *     WHEN "67138065"   MOVE "5611181" TO 口座番号Ｗ
      *     WHEN "67138073"   MOVE "5611203" TO 口座番号Ｗ
      *     WHEN "67138081"   MOVE "5611211" TO 口座番号Ｗ
      *     WHEN "67138099"   MOVE "5611238" TO 口座番号Ｗ
      *     WHEN "67138107"   MOVE "5611246" TO 口座番号Ｗ
      *     WHEN "67138115"   MOVE "5611254" TO 口座番号Ｗ
      *     WHEN "67138123"   MOVE "5611262" TO 口座番号Ｗ
      *     WHEN "67138131"   MOVE "5611270" TO 口座番号Ｗ
      *     WHEN "67138149"   MOVE "5611289" TO 口座番号Ｗ
      *     WHEN "67138164"   MOVE "5611297" TO 口座番号Ｗ
      *     WHEN "67138172"   MOVE "5611300" TO 口座番号Ｗ
      *     WHEN "67138180"   MOVE "5611319" TO 口座番号Ｗ
      *     WHEN "67138206"   MOVE "5611327" TO 口座番号Ｗ
      *     WHEN "67138214"   MOVE "5611335" TO 口座番号Ｗ
      *     WHEN "67110023"   MOVE "5611343" TO 口座番号Ｗ
      *     WHEN "67110072"   MOVE "5611351" TO 口座番号Ｗ
      *     WHEN "67138313"   MOVE "5611378" TO 口座番号Ｗ
      *     WHEN "67138354"   MOVE "5611386" TO 口座番号Ｗ
      *     WHEN "67138362"   MOVE "5611394" TO 口座番号Ｗ
      *     WHEN "67138479"   MOVE "5611408" TO 口座番号Ｗ
      *     WHEN "67138487"   MOVE "5611416" TO 口座番号Ｗ
      *     WHEN "67138586"   MOVE "5611424" TO 口座番号Ｗ
      *     WHEN "67110098"   MOVE "5611432" TO 口座番号Ｗ
      *     WHEN "67110320"   MOVE "5611440" TO 口座番号Ｗ
      *     WHEN "67110445"   MOVE "5611459" TO 口座番号Ｗ
      *     WHEN "67110551"   MOVE "5611467" TO 口座番号Ｗ
      *     WHEN "27110014"   MOVE "5611475" TO 口座番号Ｗ
      *     WHEN "27110089"   MOVE "5611483" TO 口座番号Ｗ
      *     WHEN "27110105"   MOVE "5611491" TO 口座番号Ｗ
      *     WHEN "27110139"   MOVE "5611505" TO 口座番号Ｗ
      *     WHEN "27110147"   MOVE "5611513" TO 口座番号Ｗ
      *     WHEN "27110345"   MOVE "5611521" TO 口座番号Ｗ
      *     WHEN "27110352"   MOVE "5611548" TO 口座番号Ｗ
      *     WHEN "27110360"   MOVE "5611556" TO 口座番号Ｗ
      *     WHEN "27110378"   MOVE "5611564" TO 口座番号Ｗ
      *     WHEN "27110840"   MOVE "5611572" TO 口座番号Ｗ
      *     WHEN "27110931"   MOVE "5611580" TO 口座番号Ｗ
      *     WHEN "27138015"   MOVE "5611599" TO 口座番号Ｗ
      *     WHEN "27138023"   MOVE "5611602" TO 口座番号Ｗ
      *     WHEN "27138031"   MOVE "5611610" TO 口座番号Ｗ
      *     WHEN "27138049"   MOVE "5611629" TO 口座番号Ｗ
      *     WHEN "27138056"   MOVE "5611637" TO 口座番号Ｗ
      *     WHEN "27138064"   MOVE "5611645" TO 口座番号Ｗ
      *     WHEN "27138072"   MOVE "5611653" TO 口座番号Ｗ
      *     WHEN "27138080"   MOVE "5611661" TO 口座番号Ｗ
      *     WHEN "27138098"   MOVE "5611688" TO 口座番号Ｗ
      *     WHEN "27138106"   MOVE "5611696" TO 口座番号Ｗ
      *     WHEN "27138114"   MOVE "5611718" TO 口座番号Ｗ
      *     WHEN "27138122"   MOVE "5611726" TO 口座番号Ｗ
      *     WHEN "27138130"   MOVE "5611734" TO 口座番号Ｗ
      *     WHEN "27138148"   MOVE "5611742" TO 口座番号Ｗ
      *     WHEN "27138155"   MOVE "5611750" TO 口座番号Ｗ
      *     WHEN "27138163"   MOVE "5611769" TO 口座番号Ｗ
      *     WHEN "27138171"   MOVE "5611777" TO 口座番号Ｗ
      *     WHEN "27138189"   MOVE "5611785" TO 口座番号Ｗ
      *     WHEN "27138197"   MOVE "5611793" TO 口座番号Ｗ
      *     WHEN "27138205"   MOVE "5611807" TO 口座番号Ｗ
      *     WHEN "27138213"   MOVE "5611815" TO 口座番号Ｗ
      *     WHEN "81136210"   MOVE "5611815" TO 口座番号Ｗ
      *     WHEN "81137218"   MOVE "5611815" TO 口座番号Ｗ
      *     WHEN "88132212"   MOVE "5611815" TO 口座番号Ｗ
      *     WHEN "88138219"   MOVE "5611815" TO 口座番号Ｗ
      *     WHEN "67110726"   MOVE "5611823" TO 口座番号Ｗ
      *     WHEN "27138320"   MOVE "5611831" TO 口座番号Ｗ
      *     WHEN "27138353"   MOVE "5611858" TO 口座番号Ｗ
      *     WHEN "27138395"   MOVE "5611866" TO 口座番号Ｗ
      *     WHEN "27138478"   MOVE "5611874" TO 口座番号Ｗ
      *     WHEN "67138248"   MOVE "5611882" TO 口座番号Ｗ
      *     WHEN "67138255"   MOVE "5611890" TO 口座番号Ｗ
      *     WHEN "41140104"   MOVE "5611904" TO 口座番号Ｗ
      *     WHEN "80140106"   MOVE "5611904" TO 口座番号Ｗ
      *     WHEN "67138297"   MOVE "5611912" TO 口座番号Ｗ
      *     WHEN "67138305"   MOVE "5611920" TO 口座番号Ｗ
      *     WHEN "67138453"   MOVE "5611939" TO 口座番号Ｗ
      *     WHEN "67138602"   MOVE "5611947" TO 口座番号Ｗ
      *     WHEN "67145052"   MOVE "5611955" TO 口座番号Ｗ
      *     WHEN "110155"     MOVE "5611963" TO 口座番号Ｗ
      *     WHEN "110197"     MOVE "5611971" TO 口座番号Ｗ
      *     WHEN "110239"     MOVE "5611998" TO 口座番号Ｗ
      *     WHEN "110247"     MOVE "5612005" TO 口座番号Ｗ
      *     WHEN "110288"     MOVE "5612013" TO 口座番号Ｗ
      *     WHEN "110320"     MOVE "5612021" TO 口座番号Ｗ
      *     WHEN "110858"     MOVE "5612048" TO 口座番号Ｗ
      *     WHEN "110866"     MOVE "5612056" TO 口座番号Ｗ
      *     WHEN "110882"     MOVE "5612064" TO 口座番号Ｗ
      *     WHEN "110916"     MOVE "5612072" TO 口座番号Ｗ
      *     WHEN "67110916"   MOVE "5612072" TO 口座番号Ｗ
      *     WHEN "110924"     MOVE "5612080" TO 口座番号Ｗ
      *     WHEN "133280"     MOVE "5612099" TO 口座番号Ｗ
      *     WHEN "138305"     MOVE "5612102" TO 口座番号Ｗ
      *     WHEN "138370"     MOVE "5612110" TO 口座番号Ｗ
      *     WHEN "138420"     MOVE "5612129" TO 口座番号Ｗ
      *     WHEN "138644"     MOVE "5612137" TO 口座番号Ｗ
      *     WHEN "06120212"   MOVE "5612145" TO 口座番号Ｗ
      *     WHEN "27110030"   MOVE "5612153" TO 口座番号Ｗ
      *     WHEN "27110238"   MOVE "5612161" TO 口座番号Ｗ
      *     WHEN "27110915"   MOVE "5612188" TO 口座番号Ｗ
      *     WHEN "27114024"   MOVE "5612196" TO 口座番号Ｗ
      *     WHEN "27114032"   MOVE "5612218" TO 口座番号Ｗ
      *     WHEN "27114040"   MOVE "5612226" TO 口座番号Ｗ
      *     WHEN "27138270"   MOVE "5612234" TO 口座番号Ｗ
      *     WHEN "27138338"   MOVE "5612242" TO 口座番号Ｗ
      *     WHEN "67110262"   MOVE "5612250" TO 口座番号Ｗ
      *     WHEN "67110940"   MOVE "5612269" TO 口座番号Ｗ
      *     WHEN "67114041"   MOVE "5612277" TO 口座番号Ｗ
      *     WHEN "67138321"   MOVE "5612285" TO 口座番号Ｗ
      *     WHEN "110064"     MOVE "5612293" TO 口座番号Ｗ
      *     WHEN "110122"     MOVE "5612307" TO 口座番号Ｗ
      *     WHEN "110254"     MOVE "5612315" TO 口座番号Ｗ
      *     WHEN "110262"     MOVE "5612323" TO 口座番号Ｗ
      *     WHEN "110270"     MOVE "5612331" TO 口座番号Ｗ
      *     WHEN "110304"     MOVE "5612358" TO 口座番号Ｗ
      *     WHEN "110312"     MOVE "5612366" TO 口座番号Ｗ
      *     WHEN "110387"     MOVE "5612374" TO 口座番号Ｗ
      *     WHEN "110395"     MOVE "5612382" TO 口座番号Ｗ
      *     WHEN "110478"     MOVE "5612390" TO 口座番号Ｗ
      *     WHEN "110510"     MOVE "5612404" TO 口座番号Ｗ
      *     WHEN "110726"     MOVE "5612412" TO 口座番号Ｗ
      *     WHEN "110734"     MOVE "5612420" TO 口座番号Ｗ
      *     WHEN "110767"     MOVE "5612439" TO 口座番号Ｗ
      *     WHEN "114025"     MOVE "5612447" TO 口座番号Ｗ
      *     WHEN "114033"     MOVE "5612455" TO 口座番号Ｗ
      *     WHEN "114041"     MOVE "5612463" TO 口座番号Ｗ
      *     WHEN "114058"     MOVE "5612471" TO 口座番号Ｗ
      *     WHEN "114066"     MOVE "5612498" TO 口座番号Ｗ
      *     WHEN "114074"     MOVE "5612501" TO 口座番号Ｗ
      *     WHEN "114082"     MOVE "5612528" TO 口座番号Ｗ
      *     WHEN "114090"     MOVE "5612536" TO 口座番号Ｗ
      *     WHEN "138255"     MOVE "5612544" TO 口座番号Ｗ
      *     WHEN "138263"     MOVE "5612552" TO 口座番号Ｗ
      *     WHEN "138271"     MOVE "5612560" TO 口座番号Ｗ
      *     WHEN "138289"     MOVE "5612579" TO 口座番号Ｗ
      *     WHEN "138297"     MOVE "5612587" TO 口座番号Ｗ
      *     WHEN "138339"     MOVE "5612595" TO 口座番号Ｗ
      *     WHEN "138412"     MOVE "5612609" TO 口座番号Ｗ
      *     WHEN "138438"     MOVE "5612617" TO 口座番号Ｗ
      *     WHEN "138453"     MOVE "5612625" TO 口座番号Ｗ
      *     WHEN "138503"     MOVE "5612633" TO 口座番号Ｗ
      *     WHEN "138552"     MOVE "5612641" TO 口座番号Ｗ
      *     WHEN "138602"     MOVE "5612668" TO 口座番号Ｗ
      *     WHEN "27110154"   MOVE "5612676" TO 口座番号Ｗ
      *     WHEN "27110162"   MOVE "5612684" TO 口座番号Ｗ
      *     WHEN "27110220"   MOVE "5612692" TO 口座番号Ｗ
      *     WHEN "27110261"   MOVE "5612706" TO 口座番号Ｗ
      *     WHEN "27110279"   MOVE "5612714" TO 口座番号Ｗ
      *     WHEN "27110287"   MOVE "5612722" TO 口座番号Ｗ
      *     WHEN "27110295"   MOVE "5612730" TO 口座番号Ｗ
      *     WHEN "27110303"   MOVE "5612749" TO 口座番号Ｗ
      *     WHEN "27110329"   MOVE "5612757" TO 口座番号Ｗ
      *     WHEN "27110337"   MOVE "5612765" TO 口座番号Ｗ
      *     WHEN "67114108"   MOVE "5612773" TO 口座番号Ｗ
      *     WHEN "27110436"   MOVE "5612781" TO 口座番号Ｗ
      *     WHEN "27110477"   MOVE "5612803" TO 口座番号Ｗ
      *     WHEN "27110824"   MOVE "5612811" TO 口座番号Ｗ
      *     WHEN "27110899"   MOVE "5612838" TO 口座番号Ｗ
      *     WHEN "27110907"   MOVE "5612846" TO 口座番号Ｗ
      *     WHEN "27110923"   MOVE "5612854" TO 口座番号Ｗ
      *     WHEN "27110949"   MOVE "5612862" TO 口座番号Ｗ
      *     WHEN "27114073"   MOVE "5612870" TO 口座番号Ｗ
      *     WHEN "27114081"   MOVE "5612889" TO 口座番号Ｗ
      *     WHEN "27114099"   MOVE "5612897" TO 口座番号Ｗ
      *     WHEN "27138247"   MOVE "5612900" TO 口座番号Ｗ
      *     WHEN "27138312"   MOVE "5612919" TO 口座番号Ｗ
      *     WHEN "67110122"   MOVE "5612927" TO 口座番号Ｗ
      *     WHEN "67110130"   MOVE "5612935" TO 口座番号Ｗ
      *     WHEN "67110155"   MOVE "5612943" TO 口座番号Ｗ
      *     WHEN "67110197"   MOVE "5612951" TO 口座番号Ｗ
      *     WHEN "67110247"   MOVE "5612978" TO 口座番号Ｗ
      *     WHEN "67110270"   MOVE "5612986" TO 口座番号Ｗ
      *     WHEN "67110288"   MOVE "5612994" TO 口座番号Ｗ
      *     WHEN "67110304"   MOVE "5613001" TO 口座番号Ｗ
      *     WHEN "67110346"   MOVE "5613028" TO 口座番号Ｗ
      *     WHEN "67110353"   MOVE "5613036" TO 口座番号Ｗ
      *     WHEN "67110379"   MOVE "5613044" TO 口座番号Ｗ
      *     WHEN "67110437"   MOVE "5613052" TO 口座番号Ｗ
      *     WHEN "67110908"   MOVE "5613060" TO 口座番号Ｗ
      *     WHEN "67110924"   MOVE "5613079" TO 口座番号Ｗ
      *     WHEN "67114017"   MOVE "5613087" TO 口座番号Ｗ
      *     WHEN "67114025"   MOVE "5613095" TO 口座番号Ｗ
      *     WHEN "67114033"   MOVE "5613109" TO 口座番号Ｗ
      *     WHEN "67114058"   MOVE "5613117" TO 口座番号Ｗ
      *     WHEN "67114066"   MOVE "5613125" TO 口座番号Ｗ
      *     WHEN "67114074"   MOVE "5613133" TO 口座番号Ｗ
      *     WHEN "67114082"   MOVE "5613141" TO 口座番号Ｗ
      *     WHEN "67114090"   MOVE "5613168" TO 口座番号Ｗ
      *     WHEN "67138347"   MOVE "5613176" TO 口座番号Ｗ
      *     WHEN "67138370"   MOVE "5613184" TO 口座番号Ｗ
      *     WHEN "67138461"   MOVE "5613192" TO 口座番号Ｗ
      *     WHEN "67138511"   MOVE "5613206" TO 口座番号Ｗ
      *     WHEN "3102"       MOVE "5613214" TO 口座番号Ｗ
      *     WHEN "02110104"   MOVE "5613222" TO 口座番号Ｗ
      *     WHEN "110072"     MOVE "5613230" TO 口座番号Ｗ
      *     WHEN "110114"     MOVE "5613249" TO 口座番号Ｗ
      *     WHEN "110163"     MOVE "5613257" TO 口座番号Ｗ
      *     WHEN "110171"     MOVE "5613265" TO 口座番号Ｗ
      *     WHEN "67110171"   MOVE "5613265" TO 口座番号Ｗ
      *     WHEN "110189"     MOVE "5613273" TO 口座番号Ｗ
      *     WHEN "110338"     MOVE "5613281" TO 口座番号Ｗ
      *     WHEN "110445"     MOVE "5613303" TO 口座番号Ｗ
      *     WHEN "110452"     MOVE "5613311" TO 口座番号Ｗ
      *     WHEN "110460"     MOVE "5613338" TO 口座番号Ｗ
      *     WHEN "110486"     MOVE "5613346" TO 口座番号Ｗ
      *     WHEN "110494"     MOVE "5613354" TO 口座番号Ｗ
      *     WHEN "110502"     MOVE "5613362" TO 口座番号Ｗ
      *     WHEN "110528"     MOVE "5613370" TO 口座番号Ｗ
      *     WHEN "110536"     MOVE "5613389" TO 口座番号Ｗ
      *     WHEN "110544"     MOVE "5613397" TO 口座番号Ｗ
      *     WHEN "110551"     MOVE "5613400" TO 口座番号Ｗ
      *     WHEN "110569"     MOVE "5613419" TO 口座番号Ｗ
      *     WHEN "110809"     MOVE "5613427" TO 口座番号Ｗ
      *     WHEN "110817"     MOVE "5613435" TO 口座番号Ｗ
      *     WHEN "110825"     MOVE "5613443" TO 口座番号Ｗ
      *     WHEN "113019"     MOVE "5613451" TO 口座番号Ｗ
      *     WHEN "113035"     MOVE "5613478" TO 口座番号Ｗ
      *     WHEN "133116"     MOVE "5613486" TO 口座番号Ｗ
      *     WHEN "138362"     MOVE "5613494" TO 口座番号Ｗ
      *     WHEN "138446"     MOVE "5613508" TO 口座番号Ｗ
      *     WHEN "138461"     MOVE "5613516" TO 口座番号Ｗ
      *     WHEN "138495"     MOVE "5613524" TO 口座番号Ｗ
      *     WHEN "27110121"   MOVE "5613532" TO 口座番号Ｗ
      *     WHEN "27110196"   MOVE "5613540" TO 口座番号Ｗ
      *     WHEN "27110246"   MOVE "5613559" TO 口座番号Ｗ
      *     WHEN "27110386"   MOVE "5613567" TO 口座番号Ｗ
      *     WHEN "27110543"   MOVE "5613575" TO 口座番号Ｗ
      *     WHEN "27110550"   MOVE "5613583" TO 口座番号Ｗ
      *     WHEN "27110568"   MOVE "5613591" TO 口座番号Ｗ
      *     WHEN "27138262"   MOVE "5613605" TO 口座番号Ｗ
      *     WHEN "27138429"   MOVE "5613613" TO 口座番号Ｗ
      *     WHEN "67110015"   MOVE "5613621" TO 口座番号Ｗ
      *     WHEN "67110080"   MOVE "5613648" TO 口座番号Ｗ
      *     WHEN "67110296"   MOVE "5613656" TO 口座番号Ｗ
      *     WHEN "67110494"   MOVE "5613664" TO 口座番号Ｗ
      *     WHEN "67110544"   MOVE "5613672" TO 口座番号Ｗ
      *     WHEN "67110569"   MOVE "5613680" TO 口座番号Ｗ
      *     WHEN "67110767"   MOVE "5613699" TO 口座番号Ｗ
      *     WHEN "67110858"   MOVE "5613702" TO 口座番号Ｗ
      *     WHEN "67138271"   MOVE "5613710" TO 口座番号Ｗ
      *     WHEN "67138396"   MOVE "5613729" TO 口座番号Ｗ
      *     WHEN "67138420"   MOVE "5613737" TO 口座番号Ｗ
      *     WHEN "06139224"   MOVE "5613745" TO 口座番号Ｗ
      *     WHEN "06139240"   MOVE "5613753" TO 口座番号Ｗ
      *     WHEN "06139257"   MOVE "5613761" TO 口座番号Ｗ
      *     WHEN "06139273"   MOVE "5613788" TO 口座番号Ｗ
      *     WHEN "31130552"   MOVE "5613796" TO 口座番号Ｗ
      *     WHEN "31110364"   MOVE "5613818" TO 口座番号Ｗ
      *     WHEN "27110170"   MOVE "5613826" TO 口座番号Ｗ
      *     WHEN "27110212"   MOVE "5613834" TO 口座番号Ｗ
      *     WHEN "27110410"   MOVE "5613842" TO 口座番号Ｗ
      *     WHEN "27110444"   MOVE "5613850" TO 口座番号Ｗ
      *     WHEN "27110790"   MOVE "5613869" TO 口座番号Ｗ
      *     WHEN "27110857"   MOVE "5613877" TO 口座番号Ｗ
      *     WHEN "27138254"   MOVE "5613885" TO 口座番号Ｗ
      *     WHEN "27138296"   MOVE "5613893" TO 口座番号Ｗ
      *     WHEN "27138379"   MOVE "5613907" TO 口座番号Ｗ
      *     WHEN "27138387"   MOVE "5613915" TO 口座番号Ｗ
      *     WHEN "27138437"   MOVE "5613923" TO 口座番号Ｗ
      *     WHEN "03110103"   MOVE "5613931" TO 口座番号Ｗ
      *     WHEN "04110102"   MOVE "5613931" TO 口座番号Ｗ
      *     WHEN "1101"       MOVE "5613931" TO 口座番号Ｗ
      *     WHEN "03110202"   MOVE "5613958" TO 口座番号Ｗ
      *     WHEN "04110201"   MOVE "5613958" TO 口座番号Ｗ
      *     WHEN "1102"       MOVE "5613958" TO 口座番号Ｗ
      *     WHEN "03110400"   MOVE "5613966" TO 口座番号Ｗ
      *     WHEN "04110409"   MOVE "5613966" TO 口座番号Ｗ
      *     WHEN "1104"       MOVE "5613966" TO 口座番号Ｗ
      *     WHEN "03110509"   MOVE "5613974" TO 口座番号Ｗ
      *     WHEN "04110508"   MOVE "5613974" TO 口座番号Ｗ
      *     WHEN "1105"       MOVE "5613974" TO 口座番号Ｗ
      *     WHEN "03110806"   MOVE "5613982" TO 口座番号Ｗ
      *     WHEN "04110805"   MOVE "5613982" TO 口座番号Ｗ
      *     WHEN "1108"       MOVE "5613982" TO 口座番号Ｗ
      *     WHEN "03113107"   MOVE "5613990" TO 口座番号Ｗ
      *     WHEN "04113106"   MOVE "5613990" TO 口座番号Ｗ
      *     WHEN "1131"       MOVE "5613990" TO 口座番号Ｗ
      *     WHEN "03113305"   MOVE "5614008" TO 口座番号Ｗ
      *     WHEN "04113304"   MOVE "5614008" TO 口座番号Ｗ
      *     WHEN "1133"       MOVE "5614008" TO 口座番号Ｗ
      *     WHEN "03120102"   MOVE "5614016" TO 口座番号Ｗ
      *     WHEN "04120101"   MOVE "5614016" TO 口座番号Ｗ
      *     WHEN "1201"       MOVE "5614016" TO 口座番号Ｗ
      *     WHEN "03120409"   MOVE "5614024" TO 口座番号Ｗ
      *     WHEN "04120408"   MOVE "5614024" TO 口座番号Ｗ
      *     WHEN "1204"       MOVE "5614024" TO 口座番号Ｗ
      *     WHEN "03120607"   MOVE "5614032" TO 口座番号Ｗ
      *     WHEN "04120606"   MOVE "5614032" TO 口座番号Ｗ
      *     WHEN "1206"       MOVE "5614032" TO 口座番号Ｗ
      *     WHEN "03120706"   MOVE "5614040" TO 口座番号Ｗ
      *     WHEN "04120705"   MOVE "5614040" TO 口座番号Ｗ
      *     WHEN "1207"       MOVE "5614040" TO 口座番号Ｗ
      *     WHEN "03120904"   MOVE "5614059" TO 口座番号Ｗ
      *     WHEN "04120903"   MOVE "5614059" TO 口座番号Ｗ
      *     WHEN "1209"       MOVE "5614059" TO 口座番号Ｗ
      *     WHEN "03124708"   MOVE "5614067" TO 口座番号Ｗ
      *     WHEN "04124707"   MOVE "5614067" TO 口座番号Ｗ
      *     WHEN "1247"       MOVE "5614067" TO 口座番号Ｗ
      *     WHEN "03130101"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03130507"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03131109"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03131505"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03132107"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03132503"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03132602"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03133105"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03133204"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03133501"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03133600"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03134103"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03134202"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03134509"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03134608"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03135100"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03135209"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03135506"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03135605"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03135704"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03136108"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03136207"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03136306"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03136504"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03137106"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03137502"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03137601"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03137700"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03138906"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "03139805"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "04130100"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "04130506"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "04131108"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "04131504"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "04132106"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "04132502"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "04135505"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "04135703"   MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2101"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2105"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2111"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2115"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2121"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2125"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2126"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2131"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2132"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2135"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2136"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2141"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2142"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2145"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2146"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2151"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2152"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2155"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2156"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2157"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2161"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2162"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2163"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2165"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2171"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2175"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2176"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2177"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2189"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "2198"       MOVE "5614075" TO 口座番号Ｗ
      *     WHEN "39011002"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39011010"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39011028"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39011036"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39011044"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39011051"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39011069"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39011077"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39011085"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39011093"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39011101"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012026"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012034"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012042"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012059"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012067"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012075"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012083"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012091"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012109"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012117"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012125"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012133"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012141"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012158"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012166"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012174"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012182"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012190"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012208"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012216"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012224"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012232"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012240"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012257"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012265"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012273"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012281"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012299"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012307"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012315"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012331"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012349"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012356"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39012364"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013032"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013040"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013313"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013321"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013339"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013347"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013370"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013438"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013453"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013461"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013479"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013610"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013628"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013636"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013644"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013677"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013701"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013719"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013917"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013925"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013933"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013941"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013958"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013966"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013974"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013982"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39013990"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014006"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014014"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014022"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014030"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014048"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014055"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014063"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014071"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014089"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014097"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014238"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014246"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014253"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014279"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014287"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014295"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014303"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014311"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014329"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014337"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014345"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014360"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014378"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014386"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014394"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014527"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014535"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014543"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014550"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014568"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014576"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014584"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014592"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014600"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014618"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014626"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014634"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014642"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014659"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014683"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014691"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014709"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014717"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014816"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014824"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014832"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014840"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014857"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014865"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014873"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39014881"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015110"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015128"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015136"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015144"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015169"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015177"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015185"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015193"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015433"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015441"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015458"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015466"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015474"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015490"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015508"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015524"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015557"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015581"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015599"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015607"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015615"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015623"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015631"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015649"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015714"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015755"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015789"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015813"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015847"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015854"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39015862"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016019"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016027"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016043"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016076"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016084"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016092"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016100"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016316"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016324"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016332"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016340"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016357"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016365"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016373"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016381"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016399"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016415"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016423"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016431"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016449"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016456"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016464"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016472"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016480"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016498"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016613"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016621"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016639"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016647"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016654"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016670"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016688"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016910"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016928"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016936"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39016944"   MOVE "5614083" TO 口座番号Ｗ
      *     WHEN "39041017"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39041025"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39041033"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39041041"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39041058"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39042023"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39042031"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39042056"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39042064"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39042072"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39042080"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39042098"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39042114"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39042122"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39042130"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39042148"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39042155"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39043013"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39043021"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39043211"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39043229"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39043237"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39043245"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39043419"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39043617"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39043625"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39044011"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39044045"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39044060"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39044219"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39044227"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39044235"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39044243"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39044441"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39044458"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39045018"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39045059"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39045810"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39046032"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39046065"   MOVE "5614091" TO 口座番号Ｗ
      *     WHEN "39062013"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39062021"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39062039"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39062047"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39062054"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39062062"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39062070"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39062088"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39062096"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39062104"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39062112"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39062120"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39062138"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39063011"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39063029"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39063219"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39063227"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39063235"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39063243"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39063417"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39063615"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39063623"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39063631"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39063649"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39063656"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39063664"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39063672"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39063813"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39063821"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39064019"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39064027"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39064035"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39064266"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39064282"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39064613"   MOVE "5614105" TO 口座番号Ｗ
      *     WHEN "39072012"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39072020"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39072038"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39072046"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39072053"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39072079"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39072087"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39072095"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39072103"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39072111"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39072129"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39072137"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39072145"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39073010"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39073036"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39073085"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39073093"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39073226"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39073424"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39073440"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39073622"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39073648"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39073671"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39073689"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39074026"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39074059"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39074075"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39074083"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39074216"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39074224"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39074232"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39074448"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39074455"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39074463"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39074471"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39074612"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39074646"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39074653"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39074661"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39074810"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39074828"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39074836"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39074844"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39075015"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39075023"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39075031"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39075049"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39075056"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39075213"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39075221"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39075411"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39075429"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39075437"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39075445"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39075452"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39075460"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39075478"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39075486"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39075619"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39075643"   MOVE "5614113" TO 口座番号Ｗ
      *     WHEN "39082011"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082029"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082037"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082045"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082052"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082078"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082086"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082102"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082110"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082128"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082144"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082151"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082169"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082177"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082193"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082201"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082219"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082227"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082235"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082243"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082250"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082268"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082276"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082284"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082292"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082300"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082318"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082326"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082334"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082342"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082359"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39082367"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39083027"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39083092"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39083100"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39083415"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39083647"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39084421"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39084439"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39084470"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39085212"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39085428"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39085469"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39085642"   MOVE "5614121" TO 口座番号Ｗ
      *     WHEN "39092010"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39092028"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39092036"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39092044"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39092051"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39092069"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39092085"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39092093"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39092101"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39092119"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39092135"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39092143"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39092150"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39092168"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39093018"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39093216"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39093414"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39093422"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39093430"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39093448"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39093455"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39093612"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39093646"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39093653"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39093661"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39093679"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39093687"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39093844"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39093869"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39094073"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39094115"   MOVE "5614148" TO 口座番号Ｗ
      *     WHEN "39102017"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39102025"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39102033"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39102041"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39102058"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39102066"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39102074"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39102082"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39102090"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39102108"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39102116"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39102124"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39103031"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39103445"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39103452"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39103635"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39103668"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39103676"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39103825"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39103833"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39103841"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39104211"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39104245"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39104252"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39104260"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39104278"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39104286"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39104294"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39104435"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39104443"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39104484"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39104492"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39104641"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39105218"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39105226"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39105234"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39105242"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39105259"   MOVE "5614156" TO 口座番号Ｗ
      *     WHEN "39121017"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39121025"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39121033"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39121041"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39121058"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39121066"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122023"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122031"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122049"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122056"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122064"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122072"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122080"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122106"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122114"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122122"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122130"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122155"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122163"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122171"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122189"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122197"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122205"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122213"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122221"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122239"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122247"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122254"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122262"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122270"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122288"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122296"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122304"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122312"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122320"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122338"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122346"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122353"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122361"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122379"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39122387"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39123229"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39123252"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39123286"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39123294"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39123427"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39123476"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39123492"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39124029"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39124037"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39124094"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39124102"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39124219"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39124227"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39124235"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39124243"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39124268"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39124276"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39124417"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39124433"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39124631"   MOVE "5614164" TO 口座番号Ｗ
      *     WHEN "39141015"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141023"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141031"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141049"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141056"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141064"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141072"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141080"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141098"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141106"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141114"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141122"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141130"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141148"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141155"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141163"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141171"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141189"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141312"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141320"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141338"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141346"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141353"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141361"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39141379"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39142013"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39142039"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39142047"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39142054"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39142062"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39142070"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39142088"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39142096"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39142104"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39142112"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39142120"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39142138"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39142146"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39142153"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39142161"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39142179"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39142187"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39143011"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39143219"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39143417"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39143425"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39143615"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39143623"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39143631"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39143649"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39143664"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39143821"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39143839"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39143847"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39144019"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39144027"   MOVE "5614172" TO 口座番号Ｗ
      *     WHEN "39192018"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39192026"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39192042"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39192059"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39192067"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39192075"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39192083"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39192091"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39192109"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39192117"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39192125"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39192133"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39192141"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39193461"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39193610"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39193628"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39193644"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39193651"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39193669"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39193842"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39194220"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39194238"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39194246"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39194253"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39194295"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39194303"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39194428"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39194436"   MOVE "5614180" TO 口座番号Ｗ
      *     WHEN "39202015"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39202023"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39202031"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39202049"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39202056"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39202064"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39202072"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39202080"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39202098"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39202106"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39202114"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39202122"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39202130"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39202148"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39202155"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39202171"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39202189"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39202197"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39202205"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203039"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203047"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203054"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203062"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203070"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203096"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203211"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203237"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203245"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203492"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203500"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203617"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203625"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203633"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203823"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203831"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203849"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203856"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203864"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39203880"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204029"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204037"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204045"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204060"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204078"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204094"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204102"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204110"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204128"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204136"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204144"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204151"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204169"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204177"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204227"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204235"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204250"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204292"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204300"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204326"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204466"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204482"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204490"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204508"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204516"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204524"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204813"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204821"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204854"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39204862"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39205216"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39205414"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39205430"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39205612"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39205620"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39205638"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39205810"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39205836"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39205885"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39205893"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39205901"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39206024"   MOVE "5614199" TO 口座番号Ｗ
      *     WHEN "39212014"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212022"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212030"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212048"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212055"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212063"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212071"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212089"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212097"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212105"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212113"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212121"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212139"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212147"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212154"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212162"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212170"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212188"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212196"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212204"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39212212"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39213020"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39213038"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39213418"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39213616"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39213624"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39213814"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39213822"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39213830"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39214010"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39214036"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39214044"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39214218"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39215017"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39215025"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39215033"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39215041"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39215058"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39215066"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39215074"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39215215"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39216049"   MOVE "5614202" TO 口座番号Ｗ
      *     WHEN "39231014"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39231022"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39231030"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39231048"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39231055"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39231063"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39231071"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39231089"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39231097"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39231105"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39231113"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39231121"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39231139"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39231147"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39231154"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39231162"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232012"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232020"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232038"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232046"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232053"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232061"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232079"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232087"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232095"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232103"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232111"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232129"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232137"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232145"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232152"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232160"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232178"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232194"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232202"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232210"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232228"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232236"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232244"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232251"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232269"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232277"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232285"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232293"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232301"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232319"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232327"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232335"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232343"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39232350"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39233028"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39233044"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39233424"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39233457"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39233614"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39233622"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39234216"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39234224"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39234232"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39234240"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39234257"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39234273"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39234414"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39234422"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39234455"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39234463"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39234471"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39234810"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39234828"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39234836"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39235015"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39235213"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39235619"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39235627"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39235635"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39236039"   MOVE "5614210" TO 口座番号Ｗ
      *     WHEN "39271028"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271036"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271044"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271069"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271077"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271085"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271093"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271119"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271135"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271143"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271150"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271168"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271176"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271184"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271192"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271200"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271218"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271226"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271234"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271242"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271259"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271267"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271275"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271283"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271416"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271424"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271432"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271440"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271457"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271465"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39271473"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272026"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272034"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272042"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272059"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272067"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272075"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272083"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272091"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272109"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272117"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272125"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272133"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272141"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272158"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272166"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272174"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272182"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272190"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272208"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272216"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272224"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272232"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272240"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272257"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272265"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272273"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272281"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272299"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272307"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272315"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39272323"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39273016"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39273214"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39273222"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39273412"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39273610"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39273628"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39273669"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39273818"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39273826"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39273834"   MOVE "5614229" TO 口座番号Ｗ
      *     WHEN "39401013"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39401039"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39401054"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39401062"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39401070"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39401088"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39401096"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39401310"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39401328"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39401336"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39401344"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39401351"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39401369"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39401377"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402029"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402037"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402045"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402052"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402060"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402078"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402102"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402110"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402128"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402136"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402144"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402151"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402169"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402177"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402185"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402193"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402201"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402219"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402227"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402235"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402243"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402250"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402268"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402276"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402284"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39402292"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39403050"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39403415"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39403423"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39403431"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39403449"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39403456"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39403480"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39403498"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39403811"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39403829"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39403837"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39403845"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39404017"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39404025"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39404215"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39404470"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39404488"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39404629"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39404637"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39405030"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39405220"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39405410"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39405436"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39405444"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39405451"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39405469"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39406012"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39406020"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39406046"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39406053"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39406087"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39406095"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39406103"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39406210"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39406251"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39406426"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39406467"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39406475"   MOVE "5614237" TO 口座番号Ｗ
      *     WHEN "39422019"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39422027"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39422035"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39422043"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39422050"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39422076"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39422084"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39422092"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39422100"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39422118"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39422126"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39422134"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39422142"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39423074"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39423082"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39423215"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39423223"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39423231"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39423835"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39423884"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39423892"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39423918"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39424114"   MOVE "5614245" TO 口座番号Ｗ
      *     WHEN "39432018"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39432026"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39432034"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39432042"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39432059"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39432067"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39432083"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39432109"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39432117"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39432125"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39432133"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39432141"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39432158"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39432166"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39433412"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39433420"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39433487"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39433644"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39433677"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39433685"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39433693"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39433859"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39434030"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39434048"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39434238"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39434246"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39434253"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39434287"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39434329"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39434337"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39434410"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39434428"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39434436"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39434444"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39434477"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39434683"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39434824"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39434840"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39435011"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39435052"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39435060"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39435078"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39435102"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39435110"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39435128"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39435136"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39435144"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39435318"   MOVE "5614253" TO 口座番号Ｗ
      *     WHEN "39442017"   MOVE "5614261" TO 口座番号Ｗ
      *     WHEN "39442025"   MOVE "5614261" TO 口座番号Ｗ
      *     WHEN "39442033"   MOVE "5614261" TO 口座番号Ｗ
      *     WHEN "39442041"   MOVE "5614261" TO 口座番号Ｗ
      *     WHEN "39442058"   MOVE "5614261" TO 口座番号Ｗ
      *     WHEN "39442066"   MOVE "5614261" TO 口座番号Ｗ
      *     WHEN "39442074"   MOVE "5614261" TO 口座番号Ｗ
      *     WHEN "39442082"   MOVE "5614261" TO 口座番号Ｗ
      *     WHEN "39442090"   MOVE "5614261" TO 口座番号Ｗ
      *     WHEN "39442108"   MOVE "5614261" TO 口座番号Ｗ
      *     WHEN "39442116"   MOVE "5614261" TO 口座番号Ｗ
      *     WHEN "39442124"   MOVE "5614261" TO 口座番号Ｗ
      *     WHEN "39442132"   MOVE "5614261" TO 口座番号Ｗ
      *     WHEN "39442140"   MOVE "5614261" TO 口座番号Ｗ
      *     WHEN "39443221"   MOVE "5614261" TO 口座番号Ｗ
      *     WHEN "39443411"   MOVE "5614261" TO 口座番号Ｗ
      *     WHEN "39444617"   MOVE "5614261" TO 口座番号Ｗ
      *     WHEN "39444625"   MOVE "5614261" TO 口座番号Ｗ
      *     WHEN "39452016"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39452024"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39452032"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39452040"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39452057"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39452065"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39452073"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39452081"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39452099"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39453014"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39453212"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39453220"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39453410"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39453618"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39453626"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39453824"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39453832"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39454012"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39454020"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39454038"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39454046"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39454053"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39454061"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39454210"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39454293"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39454301"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39454319"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39454418"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39454426"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39454434"   MOVE "5614288" TO 口座番号Ｗ
      *     WHEN "39462015"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39462031"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39462049"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39462064"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39462080"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39462098"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39462106"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39462130"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39462148"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39462155"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39462163"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39462171"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39462189"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39462197"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39462205"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39462213"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39462221"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39462239"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39463039"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39463047"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39463922"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39464045"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39464219"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39464417"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39464425"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39464433"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39464524"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39464680"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39464821"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39464904"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39464912"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39464920"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39465018"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39465026"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39465059"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39465232"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39465240"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39465257"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39465273"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39465299"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39465307"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39465315"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39465323"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39465331"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39465349"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39465356"   MOVE "5614296" TO 口座番号Ｗ
      *     WHEN "39111018"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39111026"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39111034"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39111042"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39111059"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39111067"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39111075"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39111083"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39111091"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39111109"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112016"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112024"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112032"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112065"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112073"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112081"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112099"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112107"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112115"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112123"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112149"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112156"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112164"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112172"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112180"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112198"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112214"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112222"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112230"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112248"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112255"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112263"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112271"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112289"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112297"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112305"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112313"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112321"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112339"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112347"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112354"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112370"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112388"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112396"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112404"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112412"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112420"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112438"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39112453"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39113014"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39113246"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39113261"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39113279"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39113410"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39113428"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39113436"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39113469"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39113477"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39113485"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39113493"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39113618"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39113626"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39113634"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39113659"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39113691"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39113816"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39113832"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39113857"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39114087"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39114210"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39114244"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39114251"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39114426"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39114459"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39114467"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39114616"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39114624"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39114640"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "39114657"   MOVE "5614318" TO 口座番号Ｗ
      *     WHEN "01010016"   MOVE "5614326" TO 口座番号Ｗ
      *     WHEN "03010014"   MOVE "5614326" TO 口座番号Ｗ
      *     WHEN "04010013"   MOVE "5614326" TO 口座番号Ｗ
      *     WHEN "01020015"   MOVE "5614334" TO 口座番号Ｗ
      *     WHEN "03020013"   MOVE "5614334" TO 口座番号Ｗ
      *     WHEN "04020012"   MOVE "5614334" TO 口座番号Ｗ
      *     WHEN "01030014"   MOVE "5614342" TO 口座番号Ｗ
      *     WHEN "03030012"   MOVE "5614342" TO 口座番号Ｗ
      *     WHEN "04030011"   MOVE "5614342" TO 口座番号Ｗ
      *     WHEN "01040013"   MOVE "5614350" TO 口座番号Ｗ
      *     WHEN "03040011"   MOVE "5614350" TO 口座番号Ｗ
      *     WHEN "04040010"   MOVE "5614350" TO 口座番号Ｗ
      *     WHEN "01050012"   MOVE "5614369" TO 口座番号Ｗ
      *     WHEN "03050010"   MOVE "5614369" TO 口座番号Ｗ
      *     WHEN "04050019"   MOVE "5614369" TO 口座番号Ｗ
      *     WHEN "01060011"   MOVE "5614377" TO 口座番号Ｗ
      *     WHEN "03060019"   MOVE "5614377" TO 口座番号Ｗ
      *     WHEN "04060018"   MOVE "5614377" TO 口座番号Ｗ
      *     WHEN "01070010"   MOVE "5614385" TO 口座番号Ｗ
      *     WHEN "03070018"   MOVE "5614385" TO 口座番号Ｗ
      *     WHEN "04070017"   MOVE "5614385" TO 口座番号Ｗ
      *     WHEN "01080019"   MOVE "5614393" TO 口座番号Ｗ
      *     WHEN "03080017"   MOVE "5614393" TO 口座番号Ｗ
      *     WHEN "04080016"   MOVE "5614393" TO 口座番号Ｗ
      *     WHEN "03140100"   MOVE "5614407" TO 口座番号Ｗ
      *     WHEN "04140109"   MOVE "5614407" TO 口座番号Ｗ
      *     WHEN "3101"       MOVE "5614407" TO 口座番号Ｗ
      *     WHEN "03140308"   MOVE "5614415" TO 口座番号Ｗ
      *     WHEN "04140307"   MOVE "5614415" TO 口座番号Ｗ
      *     WHEN "3103"       MOVE "5614415" TO 口座番号Ｗ
      *     WHEN "03141108"   MOVE "5614423" TO 口座番号Ｗ
      *     WHEN "04141107"   MOVE "5614423" TO 口座番号Ｗ
      *     WHEN "3111"       MOVE "5614423" TO 口座番号Ｗ
      *     WHEN "03141405"   MOVE "5614431" TO 口座番号Ｗ
      *     WHEN "04141404"   MOVE "5614431" TO 口座番号Ｗ
      *     WHEN "3114"       MOVE "5614431" TO 口座番号Ｗ
      *     WHEN "03142106"   MOVE "5614458" TO 口座番号Ｗ
      *     WHEN "04142105"   MOVE "5614458" TO 口座番号Ｗ
      *     WHEN "3121"       MOVE "5614458" TO 口座番号Ｗ
      *     WHEN "03142304"   MOVE "5614466" TO 口座番号Ｗ
      *     WHEN "04142303"   MOVE "5614466" TO 口座番号Ｗ
      *     WHEN "3123"       MOVE "5614466" TO 口座番号Ｗ
      *     WHEN "03143104"   MOVE "5614474" TO 口座番号Ｗ
      *     WHEN "04143103"   MOVE "5614474" TO 口座番号Ｗ
      *     WHEN "3131"       MOVE "5614474" TO 口座番号Ｗ
      *     WHEN "03143500"   MOVE "5614482" TO 口座番号Ｗ
      *     WHEN "04143509"   MOVE "5614482" TO 口座番号Ｗ
      *     WHEN "3135"       MOVE "5614482" TO 口座番号Ｗ
      *     WHEN "03145109"   MOVE "5614490" TO 口座番号Ｗ
      *     WHEN "04145108"   MOVE "5614490" TO 口座番号Ｗ
      *     WHEN "3151"       MOVE "5614490" TO 口座番号Ｗ
      *     WHEN "3152"       MOVE "5614504" TO 口座番号Ｗ
      *     WHEN "03145307"   MOVE "5614512" TO 口座番号Ｗ
      *     WHEN "04145306"   MOVE "5614512" TO 口座番号Ｗ
      *     WHEN "3153"       MOVE "5614512" TO 口座番号Ｗ
      *     WHEN "03145505"   MOVE "5614520" TO 口座番号Ｗ
      *     WHEN "04145504"   MOVE "5614520" TO 口座番号Ｗ
      *     WHEN "3155"       MOVE "5614520" TO 口座番号Ｗ
      *     WHEN "03145703"   MOVE "5614539" TO 口座番号Ｗ
      *     WHEN "04145702"   MOVE "5614539" TO 口座番号Ｗ
      *     WHEN "3157"       MOVE "5614539" TO 口座番号Ｗ
      *     WHEN "27114107"   MOVE "5614547" TO 口座番号Ｗ
      *     WHEN "27114016"   MOVE "5614555" TO 口座番号Ｗ
      *     WHEN "01090018"   MOVE "5614563" TO 口座番号Ｗ
      *     WHEN "03090016"   MOVE "5614563" TO 口座番号Ｗ
      *     WHEN "04090015"   MOVE "5614563" TO 口座番号Ｗ
      *     WHEN "01100015"   MOVE "5614571" TO 口座番号Ｗ
      *     WHEN "03100013"   MOVE "5614571" TO 口座番号Ｗ
      *     WHEN "04100012"   MOVE "5614571" TO 口座番号Ｗ
      *     WHEN "01110014"   MOVE "5614598" TO 口座番号Ｗ
      *     WHEN "03110012"   MOVE "5614598" TO 口座番号Ｗ
      *     WHEN "04110011"   MOVE "5614598" TO 口座番号Ｗ
      *     WHEN "01120013"   MOVE "5614601" TO 口座番号Ｗ
      *     WHEN "03120011"   MOVE "5614601" TO 口座番号Ｗ
      *     WHEN "04120010"   MOVE "5614601" TO 口座番号Ｗ
      *     WHEN "01130012"   MOVE "5614628" TO 口座番号Ｗ
      *     WHEN "03130010"   MOVE "5614628" TO 口座番号Ｗ
      *     WHEN "04130019"   MOVE "5614628" TO 口座番号Ｗ
      *     WHEN "41140138"   MOVE "5614636" TO 口座番号Ｗ
      *     WHEN "06130058"   MOVE "5614644" TO 口座番号Ｗ
      *     WHEN "06130066"   MOVE "5614652" TO 口座番号Ｗ
      *     WHEN "06130074"   MOVE "5614660" TO 口座番号Ｗ
      *     WHEN "06130082"   MOVE "5614679" TO 口座番号Ｗ
      *     WHEN "06130090"   MOVE "5614687" TO 口座番号Ｗ
      *     WHEN "06130108"   MOVE "5614695" TO 口座番号Ｗ
      *     WHEN "63130108"   MOVE "5614695" TO 口座番号Ｗ
      *     WHEN "06130116"   MOVE "5614709" TO 口座番号Ｗ
      *     WHEN "06130124"   MOVE "5614717" TO 口座番号Ｗ
      *     WHEN "63130124"   MOVE "5614717" TO 口座番号Ｗ
      *     WHEN "06130132"   MOVE "5614725" TO 口座番号Ｗ
      *     WHEN "06130181"   MOVE "5614733" TO 口座番号Ｗ
      *     WHEN "06130199"   MOVE "5614741" TO 口座番号Ｗ
      *     WHEN "06130231"   MOVE "5614768" TO 口座番号Ｗ
      *     WHEN "06130298"   MOVE "5614776" TO 口座番号Ｗ
      *     WHEN "63130298"   MOVE "5614776" TO 口座番号Ｗ
      *     WHEN "06130306"   MOVE "5614784" TO 口座番号Ｗ
      *     WHEN "06130389"   MOVE "5614792" TO 口座番号Ｗ
      *     WHEN "63130389"   MOVE "5614792" TO 口座番号Ｗ
      *     WHEN "06130405"   MOVE "5614806" TO 口座番号Ｗ
      *     WHEN "06130439"   MOVE "5614814" TO 口座番号Ｗ
      *     WHEN "06130447"   MOVE "5614822" TO 口座番号Ｗ
      *     WHEN "06130454"   MOVE "5614830" TO 口座番号Ｗ
      *     WHEN "06130488"   MOVE "5614849" TO 口座番号Ｗ
      *     WHEN "06130553"   MOVE "5614857" TO 口座番号Ｗ
      *     WHEN "06130587"   MOVE "5614865" TO 口座番号Ｗ
      *     WHEN "06130637"   MOVE "5614873" TO 口座番号Ｗ
      *     WHEN "06130645"   MOVE "5614881" TO 口座番号Ｗ
      *     WHEN "06130660"   MOVE "5614903" TO 口座番号Ｗ
      *     WHEN "63130660"   MOVE "5614903" TO 口座番号Ｗ
      *     WHEN "06130686"   MOVE "5614911" TO 口座番号Ｗ
      *     WHEN "06130702"   MOVE "5614938" TO 口座番号Ｗ
      *     WHEN "63130702"   MOVE "5614938" TO 口座番号Ｗ
      *     WHEN "06130710"   MOVE "5614946" TO 口座番号Ｗ
      *     WHEN "06130728"   MOVE "5614954" TO 口座番号Ｗ
      *     WHEN "06130736"   MOVE "5614962" TO 口座番号Ｗ
      *     WHEN "06130769"   MOVE "5614970" TO 口座番号Ｗ
      *     WHEN "63130769"   MOVE "5614970" TO 口座番号Ｗ
      *     WHEN "06130777"   MOVE "5614989" TO 口座番号Ｗ
      *     WHEN "63130777"   MOVE "5614989" TO 口座番号Ｗ
      *     WHEN "06130785"   MOVE "5614997" TO 口座番号Ｗ
      *     WHEN "06130835"   MOVE "5615004" TO 口座番号Ｗ
      *     WHEN "06130843"   MOVE "5615012" TO 口座番号Ｗ
      *     WHEN "06130868"   MOVE "5615020" TO 口座番号Ｗ
      *     WHEN "06130892"   MOVE "5615039" TO 口座番号Ｗ
      *     WHEN "63130892"   MOVE "5615039" TO 口座番号Ｗ
      *     WHEN "06130900"   MOVE "5615047" TO 口座番号Ｗ
      *     WHEN "06130926"   MOVE "5615055" TO 口座番号Ｗ
      *     WHEN "06130934"   MOVE "5615063" TO 口座番号Ｗ
      *     WHEN "06130975"   MOVE "5615071" TO 口座番号Ｗ
      *     WHEN "06131064"   MOVE "5615098" TO 口座番号Ｗ
      *     WHEN "06131114"   MOVE "5615101" TO 口座番号Ｗ
      *     WHEN "06131163"   MOVE "5615128" TO 口座番号Ｗ
      *     WHEN "06131189"   MOVE "5615136" TO 口座番号Ｗ
      *     WHEN "06131213"   MOVE "5615144" TO 口座番号Ｗ
      *     WHEN "06131288"   MOVE "5615152" TO 口座番号Ｗ
      *     WHEN "06131296"   MOVE "5615160" TO 口座番号Ｗ
      *     WHEN "06131338"   MOVE "5615179" TO 口座番号Ｗ
      *     WHEN "06131346"   MOVE "5615187" TO 口座番号Ｗ
      *     WHEN "06131379"   MOVE "5615195" TO 口座番号Ｗ
      *     WHEN "06131429"   MOVE "5615209" TO 口座番号Ｗ
      *     WHEN "06131452"   MOVE "5615217" TO 口座番号Ｗ
      *     WHEN "06131460"   MOVE "5615225" TO 口座番号Ｗ
      *     WHEN "06131551"   MOVE "5615233" TO 口座番号Ｗ
      *     WHEN "63131551"   MOVE "5615233" TO 口座番号Ｗ
      *     WHEN "06131569"   MOVE "5615241" TO 口座番号Ｗ
      *     WHEN "06131577"   MOVE "5615268" TO 口座番号Ｗ
      *     WHEN "06131585"   MOVE "5615276" TO 口座番号Ｗ
      *     WHEN "06131635"   MOVE "5615284" TO 口座番号Ｗ
      *     WHEN "06131668"   MOVE "5615292" TO 口座番号Ｗ
      *     WHEN "63131668"   MOVE "5615292" TO 口座番号Ｗ
      *     WHEN "06131676"   MOVE "5615306" TO 口座番号Ｗ
      *     WHEN "06131742"   MOVE "5615314" TO 口座番号Ｗ
      *     WHEN "06131783"   MOVE "5615322" TO 口座番号Ｗ
      *     WHEN "06131791"   MOVE "5615330" TO 口座番号Ｗ
      *     WHEN "06131817"   MOVE "5615349" TO 口座番号Ｗ
      *     WHEN "06131841"   MOVE "5615357" TO 口座番号Ｗ
      *     WHEN "06131882"   MOVE "5615365" TO 口座番号Ｗ
      *     WHEN "06131924"   MOVE "5615373" TO 口座番号Ｗ
      *     WHEN "06131932"   MOVE "5615381" TO 口座番号Ｗ
      *     WHEN "06131999"   MOVE "5615403" TO 口座番号Ｗ
      *     WHEN "63131999"   MOVE "5615403" TO 口座番号Ｗ
      *     WHEN "06132013"   MOVE "5615411" TO 口座番号Ｗ
      *     WHEN "06132039"   MOVE "5615438" TO 口座番号Ｗ
      *     WHEN "06132054"   MOVE "5615446" TO 口座番号Ｗ
      *     WHEN "06132088"   MOVE "5615454" TO 口座番号Ｗ
      *     WHEN "63132088"   MOVE "5615454" TO 口座番号Ｗ
      *     WHEN "06132112"   MOVE "5615462" TO 口座番号Ｗ
      *     WHEN "63132112"   MOVE "5615462" TO 口座番号Ｗ
      *     WHEN "06132120"   MOVE "5615470" TO 口座番号Ｗ
      *     WHEN "06132146"   MOVE "5615489" TO 口座番号Ｗ
      *     WHEN "63132146"   MOVE "5615489" TO 口座番号Ｗ
      *     WHEN "06132161"   MOVE "5615497" TO 口座番号Ｗ
      *     WHEN "06132179"   MOVE "5615500" TO 口座番号Ｗ
      *     WHEN "06132211"   MOVE "5615519" TO 口座番号Ｗ
      *     WHEN "06132229"   MOVE "5615527" TO 口座番号Ｗ
      *     WHEN "06132260"   MOVE "5615535" TO 口座番号Ｗ
      *     WHEN "63132260"   MOVE "5615535" TO 口座番号Ｗ
      *     WHEN "06132294"   MOVE "5615543" TO 口座番号Ｗ
      *     WHEN "06132302"   MOVE "5615551" TO 口座番号Ｗ
      *     WHEN "06132310"   MOVE "5615578" TO 口座番号Ｗ
      *     WHEN "06132328"   MOVE "5615586" TO 口座番号Ｗ
      *     WHEN "06132336"   MOVE "5615594" TO 口座番号Ｗ
      *     WHEN "06132344"   MOVE "5615608" TO 口座番号Ｗ
      *     WHEN "06132369"   MOVE "5615616" TO 口座番号Ｗ
      *     WHEN "06132377"   MOVE "5615624" TO 口座番号Ｗ
      *     WHEN "06132393"   MOVE "5615632" TO 口座番号Ｗ
      *     WHEN "06132419"   MOVE "5615640" TO 口座番号Ｗ
      *     WHEN "06132427"   MOVE "5615659" TO 口座番号Ｗ
      *     WHEN "06132443"   MOVE "5615667" TO 口座番号Ｗ
      *     WHEN "06132468"   MOVE "5615675" TO 口座番号Ｗ
      *     WHEN "06137673"   MOVE "5615675" TO 口座番号Ｗ
      *     WHEN "06137806"   MOVE "5615675" TO 口座番号Ｗ
      *     WHEN "06138671"   MOVE "5615675" TO 口座番号Ｗ
      *     WHEN "06132476"   MOVE "5615683" TO 口座番号Ｗ
      *     WHEN "06132484"   MOVE "5615691" TO 口座番号Ｗ
      *     WHEN "06132500"   MOVE "5615705" TO 口座番号Ｗ
      *     WHEN "06132518"   MOVE "5615713" TO 口座番号Ｗ
      *     WHEN "06132559"   MOVE "5615721" TO 口座番号Ｗ
      *     WHEN "06132567"   MOVE "5615748" TO 口座番号Ｗ
      *     WHEN "06132583"   MOVE "5615756" TO 口座番号Ｗ
      *     WHEN "63132583"   MOVE "5615756" TO 口座番号Ｗ
      *     WHEN "06132658"   MOVE "5615764" TO 口座番号Ｗ
      *     WHEN "06132682"   MOVE "5615772" TO 口座番号Ｗ
      *     WHEN "06132690"   MOVE "5615780" TO 口座番号Ｗ
      *     WHEN "06132765"   MOVE "5615799" TO 口座番号Ｗ
      *     WHEN "63132765"   MOVE "5615799" TO 口座番号Ｗ
      *     WHEN "06132773"   MOVE "5615802" TO 口座番号Ｗ
      *     WHEN "63132773"   MOVE "5615802" TO 口座番号Ｗ
      *     WHEN "06132781"   MOVE "5615810" TO 口座番号Ｗ
      *     WHEN "06132799"   MOVE "5615829" TO 口座番号Ｗ
      *     WHEN "06132807"   MOVE "5615837" TO 口座番号Ｗ
      *     WHEN "06132831"   MOVE "5615845" TO 口座番号Ｗ
      *     WHEN "06132849"   MOVE "5615853" TO 口座番号Ｗ
      *     WHEN "06132856"   MOVE "5615861" TO 口座番号Ｗ
      *     WHEN "06132864"   MOVE "5615888" TO 口座番号Ｗ
      *     WHEN "06132922"   MOVE "5615896" TO 口座番号Ｗ
      *     WHEN "63132922"   MOVE "5615896" TO 口座番号Ｗ
      *     WHEN "06132930"   MOVE "5615918" TO 口座番号Ｗ
      *     WHEN "06132948"   MOVE "5615926" TO 口座番号Ｗ
      *     WHEN "63132948"   MOVE "5615926" TO 口座番号Ｗ
      *     WHEN "06132963"   MOVE "5615934" TO 口座番号Ｗ
      *     WHEN "06132971"   MOVE "5615942" TO 口座番号Ｗ
      *     WHEN "63132971"   MOVE "5615942" TO 口座番号Ｗ
      *     WHEN "06133029"   MOVE "5615950" TO 口座番号Ｗ
      *     WHEN "06090419"   MOVE "5615969" TO 口座番号Ｗ
      *     WHEN "06133086"   MOVE "5615969" TO 口座番号Ｗ
      *     WHEN "63090419"   MOVE "5615969" TO 口座番号Ｗ
      *     WHEN "63133086"   MOVE "5615969" TO 口座番号Ｗ
      *     WHEN "06133094"   MOVE "5615977" TO 口座番号Ｗ
      *     WHEN "06133102"   MOVE "5615985" TO 口座番号Ｗ
      *     WHEN "06133110"   MOVE "5615993" TO 口座番号Ｗ
      *     WHEN "06133169"   MOVE "5616000" TO 口座番号Ｗ
      *     WHEN "63133169"   MOVE "5616000" TO 口座番号Ｗ
      *     WHEN "06133177"   MOVE "5616019" TO 口座番号Ｗ
      *     WHEN "06133185"   MOVE "5616027" TO 口座番号Ｗ
      *     WHEN "06133243"   MOVE "5616035" TO 口座番号Ｗ
      *     WHEN "06133250"   MOVE "5616043" TO 口座番号Ｗ
      *     WHEN "06133276"   MOVE "5616051" TO 口座番号Ｗ
      *     WHEN "06133300"   MOVE "5616078" TO 口座番号Ｗ
      *     WHEN "06133342"   MOVE "5616086" TO 口座番号Ｗ
      *     WHEN "63133342"   MOVE "5616086" TO 口座番号Ｗ
      *     WHEN "06133375"   MOVE "5616094" TO 口座番号Ｗ
      *     WHEN "06133391"   MOVE "5616108" TO 口座番号Ｗ
      *     WHEN "06133417"   MOVE "5616116" TO 口座番号Ｗ
      *     WHEN "63133417"   MOVE "5616116" TO 口座番号Ｗ
      *     WHEN "06133425"   MOVE "5616124" TO 口座番号Ｗ
      *     WHEN "06133433"   MOVE "5616132" TO 口座番号Ｗ
      *     WHEN "06133458"   MOVE "5616140" TO 口座番号Ｗ
      *     WHEN "63133458"   MOVE "5616140" TO 口座番号Ｗ
      *     WHEN "06133474"   MOVE "5616159" TO 口座番号Ｗ
      *     WHEN "06133516"   MOVE "5616167" TO 口座番号Ｗ
      *     WHEN "06133540"   MOVE "5616175" TO 口座番号Ｗ
      *     WHEN "06133565"   MOVE "5616183" TO 口座番号Ｗ
      *     WHEN "06133573"   MOVE "5616191" TO 口座番号Ｗ
      *     WHEN "06133607"   MOVE "5616205" TO 口座番号Ｗ
      *     WHEN "06133615"   MOVE "5616213" TO 口座番号Ｗ
      *     WHEN "06133623"   MOVE "5616221" TO 口座番号Ｗ
      *     WHEN "06133631"   MOVE "5616248" TO 口座番号Ｗ
      *     WHEN "06133649"   MOVE "5616256" TO 口座番号Ｗ
      *     WHEN "06133672"   MOVE "5616264" TO 口座番号Ｗ
      *     WHEN "06133714"   MOVE "5616272" TO 口座番号Ｗ
      *     WHEN "06133730"   MOVE "5616280" TO 口座番号Ｗ
      *     WHEN "06141766"   MOVE "5616280" TO 口座番号Ｗ
      *     WHEN "06231104"   MOVE "5616280" TO 口座番号Ｗ
      *     WHEN "63133730"   MOVE "5616280" TO 口座番号Ｗ
      *     WHEN "06133771"   MOVE "5616299" TO 口座番号Ｗ
      *     WHEN "06133821"   MOVE "5616302" TO 口座番号Ｗ
      *     WHEN "06133862"   MOVE "5616310" TO 口座番号Ｗ
      *     WHEN "06133870"   MOVE "5616329" TO 口座番号Ｗ
      *     WHEN "06133888"   MOVE "5616337" TO 口座番号Ｗ
      *     WHEN "06133920"   MOVE "5616345" TO 口座番号Ｗ
      *     WHEN "06133938"   MOVE "5616353" TO 口座番号Ｗ
      *     WHEN "63133938"   MOVE "5616353" TO 口座番号Ｗ
      *     WHEN "06133946"   MOVE "5616361" TO 口座番号Ｗ
      *     WHEN "63133946"   MOVE "5616361" TO 口座番号Ｗ
      *     WHEN "06133961"   MOVE "5616388" TO 口座番号Ｗ
      *     WHEN "06134001"   MOVE "5616396" TO 口座番号Ｗ
      *     WHEN "06134019"   MOVE "5616418" TO 口座番号Ｗ
      *     WHEN "06134035"   MOVE "5616426" TO 口座番号Ｗ
      *     WHEN "06134050"   MOVE "5616434" TO 口座番号Ｗ
      *     WHEN "06134076"   MOVE "5616442" TO 口座番号Ｗ
      *     WHEN "06134084"   MOVE "5616450" TO 口座番号Ｗ
      *     WHEN "06134134"   MOVE "5616469" TO 口座番号Ｗ
      *     WHEN "63134134"   MOVE "5616469" TO 口座番号Ｗ
      *     WHEN "06134159"   MOVE "5616477" TO 口座番号Ｗ
      *     WHEN "06134175"   MOVE "5616485" TO 口座番号Ｗ
      *     WHEN "06134183"   MOVE "5616493" TO 口座番号Ｗ
      *     WHEN "63134183"   MOVE "5616493" TO 口座番号Ｗ
      *     WHEN "06134217"   MOVE "5616507" TO 口座番号Ｗ
      *     WHEN "06134340"   MOVE "5616515" TO 口座番号Ｗ
      *     WHEN "06134357"   MOVE "5616523" TO 口座番号Ｗ
      *     WHEN "06134365"   MOVE "5616531" TO 口座番号Ｗ
      *     WHEN "06134373"   MOVE "5616558" TO 口座番号Ｗ
      *     WHEN "06134381"   MOVE "5616566" TO 口座番号Ｗ
      *     WHEN "06134431"   MOVE "5616574" TO 口座番号Ｗ
      *     WHEN "63134431"   MOVE "5616574" TO 口座番号Ｗ
      *     WHEN "06134464"   MOVE "5616582" TO 口座番号Ｗ
      *     WHEN "06134498"   MOVE "5616590" TO 口座番号Ｗ
      *     WHEN "06134522"   MOVE "5616604" TO 口座番号Ｗ
      *     WHEN "06134530"   MOVE "5616612" TO 口座番号Ｗ
      *     WHEN "06134548"   MOVE "5616620" TO 口座番号Ｗ
      *     WHEN "06134555"   MOVE "5616639" TO 口座番号Ｗ
      *     WHEN "63134555"   MOVE "5616639" TO 口座番号Ｗ
      *     WHEN "06134571"   MOVE "5616647" TO 口座番号Ｗ
      *     WHEN "06134613"   MOVE "5616655" TO 口座番号Ｗ
      *     WHEN "06134621"   MOVE "5616663" TO 口座番号Ｗ
      *     WHEN "06134688"   MOVE "5616671" TO 口座番号Ｗ
      *     WHEN "06134795"   MOVE "5616698" TO 口座番号Ｗ
      *     WHEN "06134803"   MOVE "5616701" TO 口座番号Ｗ
      *     WHEN "06134845"   MOVE "5616728" TO 口座番号Ｗ
      *     WHEN "06134886"   MOVE "5616736" TO 口座番号Ｗ
      *     WHEN "06134902"   MOVE "5616744" TO 口座番号Ｗ
      *     WHEN "06134910"   MOVE "5616752" TO 口座番号Ｗ
      *     WHEN "06134928"   MOVE "5616760" TO 口座番号Ｗ
      *     WHEN "06134944"   MOVE "5616779" TO 口座番号Ｗ
      *     WHEN "06134969"   MOVE "5616787" TO 口座番号Ｗ
      *     WHEN "06135024"   MOVE "5616795" TO 口座番号Ｗ
      *     WHEN "06135040"   MOVE "5616809" TO 口座番号Ｗ
      *     WHEN "63135040"   MOVE "5616809" TO 口座番号Ｗ
      *     WHEN "06135057"   MOVE "5616817" TO 口座番号Ｗ
      *     WHEN "06135123"   MOVE "5616825" TO 口座番号Ｗ
      *     WHEN "06135172"   MOVE "5616833" TO 口座番号Ｗ
      *     WHEN "06135180"   MOVE "5616841" TO 口座番号Ｗ
      *     WHEN "06135222"   MOVE "5616868" TO 口座番号Ｗ
      *     WHEN "06135248"   MOVE "5616876" TO 口座番号Ｗ
      *     WHEN "06135255"   MOVE "5616884" TO 口座番号Ｗ
      *     WHEN "06135354"   MOVE "5616892" TO 口座番号Ｗ
      *     WHEN "06135370"   MOVE "5616906" TO 口座番号Ｗ
      *     WHEN "06135388"   MOVE "5616914" TO 口座番号Ｗ
      *     WHEN "06135396"   MOVE "5616922" TO 口座番号Ｗ
      *     WHEN "06135404"   MOVE "5616930" TO 口座番号Ｗ
      *     WHEN "06135438"   MOVE "5616949" TO 口座番号Ｗ
      *     WHEN "06135453"   MOVE "5616957" TO 口座番号Ｗ
      *     WHEN "06135487"   MOVE "5616965" TO 口座番号Ｗ
      *     WHEN "63135487"   MOVE "5616965" TO 口座番号Ｗ
      *     WHEN "06135503"   MOVE "5616973" TO 口座番号Ｗ
      *     WHEN "06135545"   MOVE "5616981" TO 口座番号Ｗ
      *     WHEN "06135552"   MOVE "5617007" TO 口座番号Ｗ
      *     WHEN "06135578"   MOVE "5617015" TO 口座番号Ｗ
      *     WHEN "06135628"   MOVE "5617023" TO 口座番号Ｗ
      *     WHEN "06135669"   MOVE "5617031" TO 口座番号Ｗ
      *     WHEN "06135719"   MOVE "5617058" TO 口座番号Ｗ
      *     WHEN "06135727"   MOVE "5617066" TO 口座番号Ｗ
      *     WHEN "06135750"   MOVE "5617074" TO 口座番号Ｗ
      *     WHEN "06135768"   MOVE "5617082" TO 口座番号Ｗ
      *     WHEN "06135776"   MOVE "5617090" TO 口座番号Ｗ
      *     WHEN "06135784"   MOVE "5617104" TO 口座番号Ｗ
      *     WHEN "06135834"   MOVE "5617112" TO 口座番号Ｗ
      *     WHEN "06135859"   MOVE "5617120" TO 口座番号Ｗ
      *     WHEN "06135891"   MOVE "5617139" TO 口座番号Ｗ
      *     WHEN "06135909"   MOVE "5617147" TO 口座番号Ｗ
      *     WHEN "06135917"   MOVE "5617155" TO 口座番号Ｗ
      *     WHEN "06135990"   MOVE "5617163" TO 口座番号Ｗ
      *     WHEN "06136006"   MOVE "5617171" TO 口座番号Ｗ
      *     WHEN "06136063"   MOVE "5617198" TO 口座番号Ｗ
      *     WHEN "06136097"   MOVE "5617201" TO 口座番号Ｗ
      *     WHEN "06136162"   MOVE "5617228" TO 口座番号Ｗ
      *     WHEN "06136196"   MOVE "5617236" TO 口座番号Ｗ
      *     WHEN "63136196"   MOVE "5617236" TO 口座番号Ｗ
      *     WHEN "06136246"   MOVE "5617244" TO 口座番号Ｗ
      *     WHEN "06136279"   MOVE "5617252" TO 口座番号Ｗ
      *     WHEN "06136287"   MOVE "5617260" TO 口座番号Ｗ
      *     WHEN "63136287"   MOVE "5617260" TO 口座番号Ｗ
      *     WHEN "06136295"   MOVE "5617279" TO 口座番号Ｗ
      *     WHEN "06136345"   MOVE "5617287" TO 口座番号Ｗ
      *     WHEN "06136378"   MOVE "5617295" TO 口座番号Ｗ
      *     WHEN "06136394"   MOVE "5617309" TO 口座番号Ｗ
      *     WHEN "06136410"   MOVE "5617317" TO 口座番号Ｗ
      *     WHEN "06136428"   MOVE "5617325" TO 口座番号Ｗ
      *     WHEN "06136436"   MOVE "5617333" TO 口座番号Ｗ
      *     WHEN "06136477"   MOVE "5617341" TO 口座番号Ｗ
      *     WHEN "63136477"   MOVE "5617341" TO 口座番号Ｗ
      *     WHEN "06136493"   MOVE "5617368" TO 口座番号Ｗ
      *     WHEN "06136501"   MOVE "5617376" TO 口座番号Ｗ
      *     WHEN "06136519"   MOVE "5617384" TO 口座番号Ｗ
      *     WHEN "06136550"   MOVE "5617392" TO 口座番号Ｗ
      *     WHEN "06136568"   MOVE "5617406" TO 口座番号Ｗ
      *     WHEN "06136618"   MOVE "5617414" TO 口座番号Ｗ
      *     WHEN "06136634"   MOVE "5617422" TO 口座番号Ｗ
      *     WHEN "06136642"   MOVE "5617430" TO 口座番号Ｗ
      *     WHEN "06136659"   MOVE "5617449" TO 口座番号Ｗ
      *     WHEN "06136709"   MOVE "5617457" TO 口座番号Ｗ
      *     WHEN "06136717"   MOVE "5617465" TO 口座番号Ｗ
      *     WHEN "06136741"   MOVE "5617473" TO 口座番号Ｗ
      *     WHEN "06136758"   MOVE "5617481" TO 口座番号Ｗ
      *     WHEN "06136774"   MOVE "5617503" TO 口座番号Ｗ
      *     WHEN "06136790"   MOVE "5617511" TO 口座番号Ｗ
      *     WHEN "06136881"   MOVE "5617538" TO 口座番号Ｗ
      *     WHEN "06136915"   MOVE "5617546" TO 口座番号Ｗ
      *     WHEN "06136923"   MOVE "5617554" TO 口座番号Ｗ
      *     WHEN "06136956"   MOVE "5617562" TO 口座番号Ｗ
      *     WHEN "06137079"   MOVE "5617570" TO 口座番号Ｗ
      *     WHEN "06137087"   MOVE "5617589" TO 口座番号Ｗ
      *     WHEN "06137103"   MOVE "5617597" TO 口座番号Ｗ
      *     WHEN "06137202"   MOVE "5617600" TO 口座番号Ｗ
      *     WHEN "06137210"   MOVE "5617619" TO 口座番号Ｗ
      *     WHEN "06137236"   MOVE "5617627" TO 口座番号Ｗ
      *     WHEN "06137251"   MOVE "5617635" TO 口座番号Ｗ
      *     WHEN "06137277"   MOVE "5617643" TO 口座番号Ｗ
      *     WHEN "06137301"   MOVE "5617651" TO 口座番号Ｗ
      *     WHEN "06137327"   MOVE "5617678" TO 口座番号Ｗ
      *     WHEN "06137335"   MOVE "5617686" TO 口座番号Ｗ
      *     WHEN "06137350"   MOVE "5617694" TO 口座番号Ｗ
      *     WHEN "06137368"   MOVE "5617708" TO 口座番号Ｗ
      *     WHEN "06137376"   MOVE "5617716" TO 口座番号Ｗ
      *     WHEN "06137384"   MOVE "5617724" TO 口座番号Ｗ
      *     WHEN "06137418"   MOVE "5617732" TO 口座番号Ｗ
      *     WHEN "06137442"   MOVE "5617740" TO 口座番号Ｗ
      *     WHEN "06137491"   MOVE "5617759" TO 口座番号Ｗ
      *     WHEN "06137525"   MOVE "5617767" TO 口座番号Ｗ
      *     WHEN "06137566"   MOVE "5617775" TO 口座番号Ｗ
      *     WHEN "06137582"   MOVE "5617783" TO 口座番号Ｗ
      *     WHEN "06137590"   MOVE "5617791" TO 口座番号Ｗ
      *     WHEN "06137608"   MOVE "5617805" TO 口座番号Ｗ
      *     WHEN "06137640"   MOVE "5617813" TO 口座番号Ｗ
      *     WHEN "06137665"   MOVE "5617821" TO 口座番号Ｗ
      *     WHEN "06137681"   MOVE "5617848" TO 口座番号Ｗ
      *     WHEN "06137723"   MOVE "5617856" TO 口座番号Ｗ
      *     WHEN "06137772"   MOVE "5617864" TO 口座番号Ｗ
      *     WHEN "06137780"   MOVE "5617872" TO 口座番号Ｗ
      *     WHEN "06137798"   MOVE "5617880" TO 口座番号Ｗ
      *     WHEN "06137855"   MOVE "5617899" TO 口座番号Ｗ
      *     WHEN "63137855"   MOVE "5617899" TO 口座番号Ｗ
      *     WHEN "06137863"   MOVE "5617902" TO 口座番号Ｗ
      *     WHEN "06137897"   MOVE "5617910" TO 口座番号Ｗ
      *     WHEN "06137913"   MOVE "5617929" TO 口座番号Ｗ
      *     WHEN "06137947"   MOVE "5617937" TO 口座番号Ｗ
      *     WHEN "06137996"   MOVE "5617945" TO 口座番号Ｗ
      *     WHEN "06138010"   MOVE "5617953" TO 口座番号Ｗ
      *     WHEN "06138051"   MOVE "5617961" TO 口座番号Ｗ
      *     WHEN "06138077"   MOVE "5617988" TO 口座番号Ｗ
      *     WHEN "06138085"   MOVE "5617996" TO 口座番号Ｗ
      *     WHEN "06138093"   MOVE "5618003" TO 口座番号Ｗ
      *     WHEN "06138119"   MOVE "5618011" TO 口座番号Ｗ
      *     WHEN "06138127"   MOVE "5618038" TO 口座番号Ｗ
      *     WHEN "06138143"   MOVE "5618046" TO 口座番号Ｗ
      *     WHEN "06138150"   MOVE "5618054" TO 口座番号Ｗ
      *     WHEN "63138150"   MOVE "5618054" TO 口座番号Ｗ
      *     WHEN "06138168"   MOVE "5618062" TO 口座番号Ｗ
      *     WHEN "06138192"   MOVE "5618070" TO 口座番号Ｗ
      *     WHEN "06138226"   MOVE "5618089" TO 口座番号Ｗ
      *     WHEN "06138242"   MOVE "5618097" TO 口座番号Ｗ
      *     WHEN "06138309"   MOVE "5618100" TO 口座番号Ｗ
      *     WHEN "06138341"   MOVE "5618119" TO 口座番号Ｗ
      *     WHEN "06138424"   MOVE "5618127" TO 口座番号Ｗ
      *     WHEN "06138432"   MOVE "5618135" TO 口座番号Ｗ
      *     WHEN "06138440"   MOVE "5618143" TO 口座番号Ｗ
      *     WHEN "06138457"   MOVE "5618151" TO 口座番号Ｗ
      *     WHEN "06138465"   MOVE "5618178" TO 口座番号Ｗ
      *     WHEN "06138481"   MOVE "5618186" TO 口座番号Ｗ
      *     WHEN "06138499"   MOVE "5618194" TO 口座番号Ｗ
      *     WHEN "06138515"   MOVE "5618208" TO 口座番号Ｗ
      *     WHEN "06138549"   MOVE "5618216" TO 口座番号Ｗ
      *     WHEN "06138564"   MOVE "5618224" TO 口座番号Ｗ
      *     WHEN "06138580"   MOVE "5618232" TO 口座番号Ｗ
      *     WHEN "63138580"   MOVE "5618232" TO 口座番号Ｗ
      *     WHEN "06138663"   MOVE "5618240" TO 口座番号Ｗ
      *     WHEN "06138689"   MOVE "5618259" TO 口座番号Ｗ
      *     WHEN "06138705"   MOVE "5618267" TO 口座番号Ｗ
      *     WHEN "06138713"   MOVE "5618275" TO 口座番号Ｗ
      *     WHEN "06138721"   MOVE "5618283" TO 口座番号Ｗ
      *     WHEN "06138796"   MOVE "5618291" TO 口座番号Ｗ
      *     WHEN "63138796"   MOVE "5618291" TO 口座番号Ｗ
      *     WHEN "06139067"   MOVE "5618305" TO 口座番号Ｗ
      *     WHEN "06139083"   MOVE "5618313" TO 口座番号Ｗ
      *     WHEN "06139117"   MOVE "5618321" TO 口座番号Ｗ
      *     WHEN "06139216"   MOVE "5618348" TO 口座番号Ｗ
      *     WHEN "06271191"   MOVE "5618348" TO 口座番号Ｗ
      *     WHEN "06130025"   MOVE "5618356" TO 口座番号Ｗ
      *     WHEN "06130157"   MOVE "5618364" TO 口座番号Ｗ
      *     WHEN "06130512"   MOVE "5618372" TO 口座番号Ｗ
      *     WHEN "06131254"   MOVE "5618380" TO 口座番号Ｗ
      *     WHEN "06132815"   MOVE "5618399" TO 口座番号Ｗ
      *     WHEN "06133409"   MOVE "5618402" TO 口座番号Ｗ
      *     WHEN "06133532"   MOVE "5618410" TO 口座番号Ｗ
      *     WHEN "06133722"   MOVE "5618429" TO 口座番号Ｗ
      *     WHEN "06133854"   MOVE "5618437" TO 口座番号Ｗ
      *     WHEN "06134753"   MOVE "5618445" TO 口座番号Ｗ
      *     WHEN "06134787"   MOVE "5618453" TO 口座番号Ｗ
      *     WHEN "06135933"   MOVE "5618461" TO 口座番号Ｗ
      *     WHEN "06136535"   MOVE "5618488" TO 口座番号Ｗ
      *     WHEN "06137467"   MOVE "5618496" TO 口座番号Ｗ
      *     WHEN "06137905"   MOVE "5618518" TO 口座番号Ｗ
      *     WHEN "06138184"   MOVE "5618526" TO 口座番号Ｗ
      *     WHEN "06138382"   MOVE "5618534" TO 口座番号Ｗ
      *     WHEN "06138572"   MOVE "5618542" TO 口座番号Ｗ
      *     WHEN "06139075"   MOVE "5618550" TO 口座番号Ｗ
      *     WHEN "06139141"   MOVE "5618569" TO 口座番号Ｗ
      *     WHEN "06131528"   MOVE "5618577" TO 口座番号Ｗ
      *     WHEN "06133136"   MOVE "5618585" TO 口座番号Ｗ
      *     WHEN "06133334"   MOVE "5618593" TO 口座番号Ｗ
      *     WHEN "06134241"   MOVE "5618607" TO 口座番号Ｗ
      *     WHEN "06134670"   MOVE "5618615" TO 口座番号Ｗ
      *     WHEN "06134829"   MOVE "5618623" TO 口座番号Ｗ
      *     WHEN "06135636"   MOVE "5618631" TO 口座番号Ｗ
      *     WHEN "06135974"   MOVE "5618658" TO 口座番号Ｗ
      *     WHEN "06136691"   MOVE "5618666" TO 口座番号Ｗ
      *     WHEN "06136808"   MOVE "5618674" TO 口座番号Ｗ
      *     WHEN "06137004"   MOVE "5618682" TO 口座番号Ｗ
      *     WHEN "06137400"   MOVE "5618690" TO 口座番号Ｗ
      *     WHEN "06137541"   MOVE "5618704" TO 口座番号Ｗ
      *     WHEN "06137822"   MOVE "5618712" TO 口座番号Ｗ
      *     WHEN "06138523"   MOVE "5618720" TO 口座番号Ｗ
      *     WHEN "06139190"   MOVE "5618739" TO 口座番号Ｗ
      *     WHEN "31130016"   MOVE "5618747" TO 口座番号Ｗ
      *     WHEN "31130032"   MOVE "5618755" TO 口座番号Ｗ
      *     WHEN "31130073"   MOVE "5618763" TO 口座番号Ｗ
      *     WHEN "31130131"   MOVE "5618771" TO 口座番号Ｗ
      *     WHEN "31130222"   MOVE "5618798" TO 口座番号Ｗ
      *     WHEN "31130248"   MOVE "5618801" TO 口座番号Ｗ
      *     WHEN "31130305"   MOVE "5618828" TO 口座番号Ｗ
      *     WHEN "31130479"   MOVE "5618836" TO 口座番号Ｗ
      *     WHEN "31130511"   MOVE "5618844" TO 口座番号Ｗ
      *     WHEN "31130537"   MOVE "5618852" TO 口座番号Ｗ
      *     WHEN "31130594"   MOVE "5618860" TO 口座番号Ｗ
      *     WHEN "31130685"   MOVE "5618879" TO 口座番号Ｗ
      *     WHEN "31130842"   MOVE "5618887" TO 口座番号Ｗ
      *     WHEN "31110257"   MOVE "5618895" TO 口座番号Ｗ
      *     WHEN "31131105"   MOVE "5618895" TO 口座番号Ｗ
      *     WHEN "31170178"   MOVE "5618895" TO 口座番号Ｗ
      *     WHEN "31430192"   MOVE "5618895" TO 口座番号Ｗ
      *     WHEN "31131147"   MOVE "5618909" TO 口座番号Ｗ
      *     WHEN "31131188"   MOVE "5618917" TO 口座番号Ｗ
      *     WHEN "31131261"   MOVE "5618925" TO 口座番号Ｗ
      *     WHEN "31131295"   MOVE "5618933" TO 口座番号Ｗ
      *     WHEN "31131311"   MOVE "5618941" TO 口座番号Ｗ
      *     WHEN "31131394"   MOVE "5618968" TO 口座番号Ｗ
      *     WHEN "31131444"   MOVE "5618976" TO 口座番号Ｗ
      *     WHEN "31131535"   MOVE "5618984" TO 口座番号Ｗ
      *     WHEN "32130213"   MOVE "5618992" TO 口座番号Ｗ
      *     WHEN "32130411"   MOVE "5619018" TO 口座番号Ｗ
      *     WHEN "33130014"   MOVE "5619026" TO 口座番号Ｗ
      *     WHEN "33130030"   MOVE "5619034" TO 口座番号Ｗ
      *     WHEN "34130013"   MOVE "5619042" TO 口座番号Ｗ
      *     WHEN "34130021"   MOVE "5619050" TO 口座番号Ｗ
      *     WHEN "31131410"   MOVE "5619069" TO 口座番号Ｗ
      *     WHEN "31131741"   MOVE "5619077" TO 口座番号Ｗ
      *     WHEN "31131774"   MOVE "5619085" TO 口座番号Ｗ
      *     WHEN "41110149"   MOVE "5619093" TO 口座番号Ｗ
      *     WHEN "41110362"   MOVE "5619107" TO 口座番号Ｗ
      *     WHEN "41110412"   MOVE "5619115" TO 口座番号Ｗ
      *     WHEN "41110891"   MOVE "5619123" TO 口座番号Ｗ
      *     WHEN "41110933"   MOVE "5619131" TO 口座番号Ｗ
      *     WHEN "41145004"   MOVE "5619158" TO 口座番号Ｗ
      *     WHEN "41145012"   MOVE "5619158" TO 口座番号Ｗ
      *     WHEN "41145020"   MOVE "5619158" TO 口座番号Ｗ
      *     WHEN "41145038"   MOVE "5619158" TO 口座番号Ｗ
      *     WHEN "41145046"   MOVE "5619158" TO 口座番号Ｗ
      *     WHEN "41145053"   MOVE "5619158" TO 口座番号Ｗ
      *     WHEN "41145061"   MOVE "5619158" TO 口座番号Ｗ
      *     WHEN "41145079"   MOVE "5619158" TO 口座番号Ｗ
      *     WHEN "41110032"   MOVE "5619166" TO 口座番号Ｗ
      *     WHEN "41110842"   MOVE "5619174" TO 口座番号Ｗ
      *     WHEN "41114042"   MOVE "5619182" TO 口座番号Ｗ
      *     WHEN "41110016"   MOVE "5619190" TO 口座番号Ｗ
      *     WHEN "41110156"   MOVE "5619204" TO 口座番号Ｗ
      *     WHEN "41110222"   MOVE "5619212" TO 口座番号Ｗ
      *     WHEN "41110230"   MOVE "5619220" TO 口座番号Ｗ
      *     WHEN "41110263"   MOVE "5619239" TO 口座番号Ｗ
      *     WHEN "41110305"   MOVE "5619247" TO 口座番号Ｗ
      *     WHEN "41110370"   MOVE "5619255" TO 口座番号Ｗ
      *     WHEN "41110925"   MOVE "5619263" TO 口座番号Ｗ
      *     WHEN "41114026"   MOVE "5619271" TO 口座番号Ｗ
      *     WHEN "41114034"   MOVE "5619298" TO 口座番号Ｗ
      *     WHEN "41114083"   MOVE "5619301" TO 口座番号Ｗ
      *     WHEN "41140062"   MOVE "5619328" TO 口座番号Ｗ
      *     WHEN "41140179"   MOVE "5619336" TO 口座番号Ｗ
      *     WHEN "06132724"   MOVE "5619344" TO 口座番号Ｗ
      *     WHEN "06136220"   MOVE "5619352" TO 口座番号Ｗ
      *     WHEN "06139182"   MOVE "5619360" TO 口座番号Ｗ
      *     WHEN "67110312"   MOVE "5619379" TO 口座番号Ｗ
      *     WHEN "67110338"   MOVE "5619387" TO 口座番号Ｗ
      *     WHEN "67110478"   MOVE "5619395" TO 口座番号Ｗ
      *     WHEN "67110502"   MOVE "5619409" TO 口座番号Ｗ
      *     WHEN "67110791"   MOVE "5619417" TO 口座番号Ｗ
      *     WHEN "67110866"   MOVE "5619425" TO 口座番号Ｗ
      *     WHEN "67138263"   MOVE "5619433" TO 口座番号Ｗ
      *     WHEN "67138339"   MOVE "5619441" TO 口座番号Ｗ
      *     WHEN "67138495"   MOVE "5619468" TO 口座番号Ｗ
      *     WHEN "06139307"   MOVE "5619476" TO 口座番号Ｗ
      *     WHEN "27110501"   MOVE "5619484" TO 口座番号Ｗ
      *     WHEN "27110519"   MOVE "5619492" TO 口座番号Ｗ
      *     WHEN "27110527"   MOVE "5619506" TO 口座番号Ｗ
      *     WHEN "27114065"   MOVE "5619514" TO 口座番号Ｗ
      *     WHEN "27138304"   MOVE "5619522" TO 口座番号Ｗ
      *     WHEN "27138361"   MOVE "5619530" TO 口座番号Ｗ
      *     WHEN "81137242"   MOVE "5619549" TO 口座番号Ｗ
      *     WHEN "88131248"   MOVE "5619549" TO 口座番号Ｗ
      *     WHEN "88138243"   MOVE "5619549" TO 口座番号Ｗ
      *     WHEN "81137358"   MOVE "5619557" TO 口座番号Ｗ
      *     WHEN "88131354"   MOVE "5619557" TO 口座番号Ｗ
      *     WHEN "88138359"   MOVE "5619557" TO 口座番号Ｗ
      *     WHEN "41139015"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139023"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139031"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139049"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139056"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139064"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139072"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139080"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139098"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139106"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139114"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139122"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139130"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139148"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139155"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139163"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139171"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139189"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139197"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139205"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139213"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139221"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139239"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139247"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139254"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139262"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139270"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139288"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139296"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139304"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139312"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139320"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139338"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139346"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139353"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139361"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139379"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139387"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139395"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139403"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139411"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139429"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139437"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139445"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139452"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139460"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139478"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139486"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139494"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139502"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139510"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139528"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139536"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139544"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139551"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139569"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139577"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139585"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139593"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139601"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139619"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139627"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139635"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "41139643"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "8013500"    MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80135015"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80135023"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80135031"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80135106"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80135122"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80135148"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80135155"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80135171"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80135197"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80135213"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80135221"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80135239"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80135353"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80135478"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136039"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136047"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136054"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136062"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136070"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136088"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136096"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136104"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136112"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136120"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136138"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136161"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136179"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136187"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136195"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136211"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136229"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136237"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136278"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136294"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136328"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80136427"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137028"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137029"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137037"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137045"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137052"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137060"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137078"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137086"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137094"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137110"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137128"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137144"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137169"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137177"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137185"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137193"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137201"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137219"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137227"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137235"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137250"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137276"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137318"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137326"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137342"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137359"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137383"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137391"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137425"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137433"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137458"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137474"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "80137482"   MOVE "5619565" TO 口座番号Ｗ
      *     WHEN "81136293"   MOVE "5619573" TO 口座番号Ｗ
      *     WHEN "81137291"   MOVE "5619573" TO 口座番号Ｗ
      *     WHEN "88138292"   MOVE "5619573" TO 口座番号Ｗ
      *     WHEN "81136459"   MOVE "5619581" TO 口座番号Ｗ
      *     WHEN "81137457"   MOVE "5619581" TO 口座番号Ｗ
      *     WHEN "88131453"   MOVE "5619581" TO 口座番号Ｗ
      *     WHEN "88138458"   MOVE "5619581" TO 口座番号Ｗ
      *     WHEN "81136186"   MOVE "5619603" TO 口座番号Ｗ
      *     WHEN "81137184"   MOVE "5619603" TO 口座番号Ｗ
      *     WHEN "88135181"   MOVE "5619603" TO 口座番号Ｗ
      *     WHEN "88138185"   MOVE "5619603" TO 口座番号Ｗ
      *     WHEN "81136178"   MOVE "5619611" TO 口座番号Ｗ
      *     WHEN "81137176"   MOVE "5619611" TO 口座番号Ｗ
      *     WHEN "88132170"   MOVE "5619611" TO 口座番号Ｗ
      *     WHEN "88133178"   MOVE "5619611" TO 口座番号Ｗ
      *     WHEN "88138177"   MOVE "5619611" TO 口座番号Ｗ
      *     WHEN "81137010"   MOVE "5619638" TO 口座番号Ｗ
      *     WHEN "88133012"   MOVE "5619638" TO 口座番号Ｗ
      *     WHEN "88138011"   MOVE "5619638" TO 口座番号Ｗ
      *     WHEN "80405020"   MOVE "5619646" TO 口座番号Ｗ
      **     WHEN "904050"     MOVE "5619646" TO 口座番号Ｗ
      *     WHEN "90405028"   MOVE "5619646" TO 口座番号Ｗ
      *     WHEN "90405036"   MOVE "5619646" TO 口座番号Ｗ
      *     WHEN "90405069"   MOVE "5619646" TO 口座番号Ｗ
      *     WHEN "90405085"   MOVE "5619646" TO 口座番号Ｗ
      *     WHEN "01140011"   MOVE "5619654" TO 口座番号Ｗ
      *     WHEN "03140019"   MOVE "5619654" TO 口座番号Ｗ
      *     WHEN "04140018"   MOVE "5619654" TO 口座番号Ｗ
      *     WHEN "01150010"   MOVE "5619662" TO 口座番号Ｗ
      *     WHEN "03150018"   MOVE "5619662" TO 口座番号Ｗ
      *     WHEN "04150017"   MOVE "5619662" TO 口座番号Ｗ
      *     WHEN "01160019"   MOVE "5619670" TO 口座番号Ｗ
      *     WHEN "03160017"   MOVE "5619670" TO 口座番号Ｗ
      *     WHEN "04160016"   MOVE "5619670" TO 口座番号Ｗ
      *     WHEN "01170018"   MOVE "5619689" TO 口座番号Ｗ
      *     WHEN "03170016"   MOVE "5619689" TO 口座番号Ｗ
      *     WHEN "04170015"   MOVE "5619689" TO 口座番号Ｗ
      *     WHEN "01180017"   MOVE "5619697" TO 口座番号Ｗ
      *     WHEN "03180015"   MOVE "5619697" TO 口座番号Ｗ
      *     WHEN "04180014"   MOVE "5619697" TO 口座番号Ｗ
      *     WHEN "01190016"   MOVE "5619700" TO 口座番号Ｗ
      *     WHEN "03190014"   MOVE "5619700" TO 口座番号Ｗ
      *     WHEN "04190013"   MOVE "5619700" TO 口座番号Ｗ
      *     WHEN "01200013"   MOVE "5619719" TO 口座番号Ｗ
      *     WHEN "03200011"   MOVE "5619719" TO 口座番号Ｗ
      *     WHEN "04200010"   MOVE "5619719" TO 口座番号Ｗ
      *     WHEN "01210012"   MOVE "5619727" TO 口座番号Ｗ
      *     WHEN "03210010"   MOVE "5619727" TO 口座番号Ｗ
      *     WHEN "04210019"   MOVE "5619727" TO 口座番号Ｗ
      *     WHEN "01220011"   MOVE "5619735" TO 口座番号Ｗ
      *     WHEN "03220019"   MOVE "5619735" TO 口座番号Ｗ
      *     WHEN "04220018"   MOVE "5619735" TO 口座番号Ｗ
      *     WHEN "01230010"   MOVE "5619743" TO 口座番号Ｗ
      *     WHEN "03230018"   MOVE "5619743" TO 口座番号Ｗ
      *     WHEN "04230017"   MOVE "5619743" TO 口座番号Ｗ
      *     WHEN "01240019"   MOVE "5619751" TO 口座番号Ｗ
      *     WHEN "03240017"   MOVE "5619751" TO 口座番号Ｗ
      *     WHEN "04240016"   MOVE "5619751" TO 口座番号Ｗ
      *     WHEN "01250018"   MOVE "5619778" TO 口座番号Ｗ
      *     WHEN "03250016"   MOVE "5619778" TO 口座番号Ｗ
      *     WHEN "04250015"   MOVE "5619778" TO 口座番号Ｗ
      *     WHEN "01260017"   MOVE "5619786" TO 口座番号Ｗ
      *     WHEN "03260015"   MOVE "5619786" TO 口座番号Ｗ
      *     WHEN "04260014"   MOVE "5619786" TO 口座番号Ｗ
      *     WHEN "01270016"   MOVE "5619794" TO 口座番号Ｗ
      *     WHEN "03270014"   MOVE "5619794" TO 口座番号Ｗ
      *     WHEN "04270013"   MOVE "5619794" TO 口座番号Ｗ
      *     WHEN "01280015"   MOVE "5619808" TO 口座番号Ｗ
      *     WHEN "03280013"   MOVE "5619808" TO 口座番号Ｗ
      *     WHEN "04280012"   MOVE "5619808" TO 口座番号Ｗ
      *     WHEN "01290014"   MOVE "5619816" TO 口座番号Ｗ
      *     WHEN "03290012"   MOVE "5619816" TO 口座番号Ｗ
      *     WHEN "04290011"   MOVE "5619816" TO 口座番号Ｗ
      *     WHEN "01300011"   MOVE "5619824" TO 口座番号Ｗ
      *     WHEN "03300019"   MOVE "5619824" TO 口座番号Ｗ
      *     WHEN "04300018"   MOVE "5619824" TO 口座番号Ｗ
      *     WHEN "01310010"   MOVE "5619832" TO 口座番号Ｗ
      *     WHEN "03310018"   MOVE "5619832" TO 口座番号Ｗ
      *     WHEN "04310017"   MOVE "5619832" TO 口座番号Ｗ
      *     WHEN "01320019"   MOVE "5619840" TO 口座番号Ｗ
      *     WHEN "03320017"   MOVE "5619840" TO 口座番号Ｗ
      *     WHEN "04320016"   MOVE "5619840" TO 口座番号Ｗ
      *     WHEN "01330018"   MOVE "5619859" TO 口座番号Ｗ
      *     WHEN "03330016"   MOVE "5619859" TO 口座番号Ｗ
      *     WHEN "04330015"   MOVE "5619859" TO 口座番号Ｗ
      *     WHEN "01340017"   MOVE "5619867" TO 口座番号Ｗ
      *     WHEN "03340015"   MOVE "5619867" TO 口座番号Ｗ
      *     WHEN "04340014"   MOVE "5619867" TO 口座番号Ｗ
      *     WHEN "01350016"   MOVE "5619875" TO 口座番号Ｗ
      *     WHEN "03350014"   MOVE "5619875" TO 口座番号Ｗ
      *     WHEN "04350013"   MOVE "5619875" TO 口座番号Ｗ
      *     WHEN "01360015"   MOVE "5619883" TO 口座番号Ｗ
      *     WHEN "03360013"   MOVE "5619883" TO 口座番号Ｗ
      *     WHEN "04360012"   MOVE "5619883" TO 口座番号Ｗ
      *     WHEN "01370014"   MOVE "5619891" TO 口座番号Ｗ
      *     WHEN "03370012"   MOVE "5619891" TO 口座番号Ｗ
      *     WHEN "04370011"   MOVE "5619891" TO 口座番号Ｗ
      *     WHEN "01380013"   MOVE "5619905" TO 口座番号Ｗ
      *     WHEN "03380011"   MOVE "5619905" TO 口座番号Ｗ
      *     WHEN "04380010"   MOVE "5619905" TO 口座番号Ｗ
      *     WHEN "01390012"   MOVE "5619913" TO 口座番号Ｗ
      *     WHEN "03390010"   MOVE "5619913" TO 口座番号Ｗ
      *     WHEN "04390019"   MOVE "5619913" TO 口座番号Ｗ
      *     WHEN "01400019"   MOVE "5619921" TO 口座番号Ｗ
      *     WHEN "03400017"   MOVE "5619921" TO 口座番号Ｗ
      *     WHEN "04400016"   MOVE "5619921" TO 口座番号Ｗ
      *     WHEN "01410018"   MOVE "5619948" TO 口座番号Ｗ
      *     WHEN "03410016"   MOVE "5619948" TO 口座番号Ｗ
      *     WHEN "04410015"   MOVE "5619948" TO 口座番号Ｗ
      *     WHEN "01420017"   MOVE "5619956" TO 口座番号Ｗ
      *     WHEN "03420015"   MOVE "5619956" TO 口座番号Ｗ
      *     WHEN "04420014"   MOVE "5619956" TO 口座番号Ｗ
      *     WHEN "01430016"   MOVE "5619964" TO 口座番号Ｗ
      *     WHEN "03430014"   MOVE "5619964" TO 口座番号Ｗ
      *     WHEN "04430013"   MOVE "5619964" TO 口座番号Ｗ
      *     WHEN "01440015"   MOVE "5619972" TO 口座番号Ｗ
      *     WHEN "03440013"   MOVE "5619972" TO 口座番号Ｗ
      *     WHEN "04440012"   MOVE "5619972" TO 口座番号Ｗ
      *     WHEN "01450014"   MOVE "5619980" TO 口座番号Ｗ
      *     WHEN "03450012"   MOVE "5619980" TO 口座番号Ｗ
      *     WHEN "04450011"   MOVE "5619980" TO 口座番号Ｗ
      *     WHEN "01460013"   MOVE "5619999" TO 口座番号Ｗ
      *     WHEN "03460011"   MOVE "5619999" TO 口座番号Ｗ
      *     WHEN "04460010"   MOVE "5619999" TO 口座番号Ｗ
      *     WHEN "01470012"   MOVE "5620008" TO 口座番号Ｗ
      *     WHEN "03470010"   MOVE "5620008" TO 口座番号Ｗ
      *     WHEN "04470019"   MOVE "5620008" TO 口座番号Ｗ
      *     WHEN "39131016"   MOVE "5620326" TO 口座番号Ｗ
      *     WHEN "39131024"   MOVE "5620334" TO 口座番号Ｗ
      *     WHEN "39131032"   MOVE "5620342" TO 口座番号Ｗ
      *     WHEN "39131040"   MOVE "5620350" TO 口座番号Ｗ
      *     WHEN "39131057"   MOVE "5620369" TO 口座番号Ｗ
      *     WHEN "39131065"   MOVE "5620377" TO 口座番号Ｗ
      *     WHEN "39131073"   MOVE "5620385" TO 口座番号Ｗ
      *     WHEN "39131081"   MOVE "5620393" TO 口座番号Ｗ
      *     WHEN "39131099"   MOVE "5620407" TO 口座番号Ｗ
      *     WHEN "39131107"   MOVE "5620415" TO 口座番号Ｗ
      *     WHEN "39131115"   MOVE "5620423" TO 口座番号Ｗ
      *     WHEN "39131123"   MOVE "5620431" TO 口座番号Ｗ
      *     WHEN "39131131"   MOVE "5620458" TO 口座番号Ｗ
      *     WHEN "39131149"   MOVE "5620466" TO 口座番号Ｗ
      *     WHEN "39131156"   MOVE "5620474" TO 口座番号Ｗ
      *     WHEN "39131164"   MOVE "5620482" TO 口座番号Ｗ
      *     WHEN "39131172"   MOVE "5620490" TO 口座番号Ｗ
      *     WHEN "39131180"   MOVE "5620504" TO 口座番号Ｗ
      *     WHEN "39131198"   MOVE "5620512" TO 口座番号Ｗ
      *     WHEN "39131206"   MOVE "5620520" TO 口座番号Ｗ
      *     WHEN "39131214"   MOVE "5620539" TO 口座番号Ｗ
      *     WHEN "39131222"   MOVE "5620547" TO 口座番号Ｗ
      *     WHEN "39131230"   MOVE "5620555" TO 口座番号Ｗ
      *     WHEN "39132014"   MOVE "5620563" TO 口座番号Ｗ
      *     WHEN "39132022"   MOVE "5620571" TO 口座番号Ｗ
      *     WHEN "39132030"   MOVE "5620598" TO 口座番号Ｗ
      *     WHEN "39132048"   MOVE "5620601" TO 口座番号Ｗ
      *     WHEN "39132055"   MOVE "5620628" TO 口座番号Ｗ
      *     WHEN "39132063"   MOVE "5620636" TO 口座番号Ｗ
      *     WHEN "39132071"   MOVE "5620644" TO 口座番号Ｗ
      *     WHEN "39132089"   MOVE "5620652" TO 口座番号Ｗ
      *     WHEN "39132097"   MOVE "5620660" TO 口座番号Ｗ
      *     WHEN "39132105"   MOVE "5620679" TO 口座番号Ｗ
      *     WHEN "39132113"   MOVE "5620687" TO 口座番号Ｗ
      *     WHEN "39132121"   MOVE "5620695" TO 口座番号Ｗ
      *     WHEN "39132139"   MOVE "5620709" TO 口座番号Ｗ
      *     WHEN "39132147"   MOVE "5620717" TO 口座番号Ｗ
      *     WHEN "39132154"   MOVE "5620725" TO 口座番号Ｗ
      *     WHEN "39132188"   MOVE "5620733" TO 口座番号Ｗ
      *     WHEN "39132196"   MOVE "5620741" TO 口座番号Ｗ
      *     WHEN "39132204"   MOVE "5620768" TO 口座番号Ｗ
      *     WHEN "39132212"   MOVE "5620776" TO 口座番号Ｗ
      *     WHEN "39132220"   MOVE "5620784" TO 口座番号Ｗ
      *     WHEN "39132238"   MOVE "5620792" TO 口座番号Ｗ
      *     WHEN "39132246"   MOVE "5620806" TO 口座番号Ｗ
      *     WHEN "39132253"   MOVE "5620814" TO 口座番号Ｗ
      *     WHEN "39132279"   MOVE "5620822" TO 口座番号Ｗ
      *     WHEN "39132287"   MOVE "5620830" TO 口座番号Ｗ
      *     WHEN "39132295"   MOVE "5620849" TO 口座番号Ｗ
      *     WHEN "39133038"   MOVE "5620857" TO 口座番号Ｗ
      *     WHEN "39133053"   MOVE "5620865" TO 口座番号Ｗ
      *     WHEN "39133079"   MOVE "5620873" TO 口座番号Ｗ
      *     WHEN "39133087"   MOVE "5620881" TO 口座番号Ｗ
      *     WHEN "39352018"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39352026"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39352034"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39352042"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39352067"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39352075"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39352083"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39352109"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39352117"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39352125"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39352133"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39352158"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39352166"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39353057"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39353214"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39353412"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39353438"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39353446"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39355029"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39355045"   MOVE "5620946" TO 口座番号Ｗ
      *     WHEN "39412010"   MOVE "5620954" TO 口座番号Ｗ
      *     WHEN "39412028"   MOVE "5620954" TO 口座番号Ｗ
      *     WHEN "39412036"   MOVE "5620954" TO 口座番号Ｗ
      *     WHEN "39412044"   MOVE "5620954" TO 口座番号Ｗ
      *     WHEN "39412051"   MOVE "5620954" TO 口座番号Ｗ
      *     WHEN "39412069"   MOVE "5620954" TO 口座番号Ｗ
      *     WHEN "39412077"   MOVE "5620954" TO 口座番号Ｗ
      *     WHEN "39412085"   MOVE "5620954" TO 口座番号Ｗ
      *     WHEN "39412093"   MOVE "5620954" TO 口座番号Ｗ
      *     WHEN "39412101"   MOVE "5620954" TO 口座番号Ｗ
      *     WHEN "39413273"   MOVE "5620954" TO 口座番号Ｗ
      *     WHEN "39413414"   MOVE "5620954" TO 口座番号Ｗ
      *     WHEN "39413455"   MOVE "5620954" TO 口座番号Ｗ
      *     WHEN "39413463"   MOVE "5620954" TO 口座番号Ｗ
      *     WHEN "39413877"   MOVE "5620954" TO 口座番号Ｗ
      *     WHEN "39414016"   MOVE "5620954" TO 口座番号Ｗ
      *     WHEN "39414230"   MOVE "5620954" TO 口座番号Ｗ
      *     WHEN "39414248"   MOVE "5620954" TO 口座番号Ｗ
      *     WHEN "39414255"   MOVE "5620954" TO 口座番号Ｗ
      *     WHEN "39414412"   MOVE "5620954" TO 口座番号Ｗ
      **/平成25年5月施術分より追加↓↓↓/130520
      *     WHEN "146001"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "146019"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "146027"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "146035"     MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67146019"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67146027"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "67146035"   MOVE "5610762" TO 口座番号Ｗ
      *     WHEN "41405069"   MOVE "5619646" TO 口座番号Ｗ
      *     WHEN "80405012"   MOVE "5619646" TO 口座番号Ｗ
      *     WHEN "80405038"   MOVE "5619646" TO 口座番号Ｗ
      *     WHEN "80405046"   MOVE "5619646" TO 口座番号Ｗ
      *     WHEN "80405053"   MOVE "5619646" TO 口座番号Ｗ
      *     WHEN "80405061"   MOVE "5619646" TO 口座番号Ｗ
      *     WHEN "80405079"   MOVE "5619646" TO 口座番号Ｗ
      *     WHEN "80405087"   MOVE "5619646" TO 口座番号Ｗ
      *     WHEN "90405010"   MOVE "5619646" TO 口座番号Ｗ
      *     WHEN "90405044"   MOVE "5619646" TO 口座番号Ｗ
      *     WHEN "90405051"   MOVE "5619646" TO 口座番号Ｗ
      *     WHEN "90405077"   MOVE "5619646" TO 口座番号Ｗ
      *     WHEN "90405093"   MOVE "5619646" TO 口座番号Ｗ
      *     WHEN "81405029"   MOVE "5619646" TO 口座番号Ｗ
      **/平成25年5月施術分より追加↑↑↑/130520
           WHEN OTHER        MOVE "5610002" TO 口座番号Ｗ
           END-EVALUATE.
037520*================================================================*
       開始日取得 SECTION.
      *
      */負傷日以降で最初にフラグが立っている日をレセプトの開始日とする。
030830     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 部位数Ｗ
030840         IF ( 施術年Ｗ = 初検年Ｗ(部位ＣＮＴ) ) AND
030850            ( 施術月Ｗ = 初検月Ｗ(部位ＣＮＴ) )
030860             MOVE 患者番号ＷＲ          TO 施記－患者番号
030870             MOVE 枝番ＷＲ              TO 施記－枝番
030880             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
030890             MOVE 初検年Ｗ(部位ＣＮＴ)  TO 施記－施術年
030900             MOVE 初検月Ｗ(部位ＣＮＴ)  TO 施記－施術月
030910             MOVE 初検日Ｗ(部位ＣＮＴ)  TO 施記－施術日
030920         ELSE
030930             MOVE 患者番号ＷＲ          TO 施記－患者番号
030940             MOVE 枝番ＷＲ              TO 施記－枝番
030950             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
030960             MOVE 施術年ＷＲ            TO 施記－施術年
030970             MOVE 施術月ＷＲ            TO 施記－施術月
030980             MOVE ZERO                  TO 施記－施術日
030990         END-IF
031000         START 施術記録Ｆ   KEY IS >= 施記－患者コード
031010                                      施記－施術和暦年月日
031020         END-START
031030         IF 状態キー = "00"
                  MOVE SPACE TO 終了フラグ２
                  PERFORM 施術記録Ｆ読込
                  PERFORM UNTIL (施記－患者コード   NOT = 患者コードＷＲ  ) OR
                                (施記－施術和暦年月 NOT = 施術和暦年月ＷＲ) OR
                                (終了フラグ２           = "YES"           )
                      IF (施記－整復施療区分  (部位ＣＮＴ) NOT = ZERO) OR
                         (施記－罨法区分      (部位ＣＮＴ) NOT = ZERO) OR
                         (施記－電療区分      (部位ＣＮＴ) NOT = ZERO) OR
                         (施記－後療料請求区分(部位ＣＮＴ) NOT = ZERO) OR
                         (施記－金属副子区分  (部位ＣＮＴ) NOT = ZERO) OR
                         (施記－情報提供区分  (部位ＣＮＴ) NOT = ZERO)
                          MOVE 施記－施術年 TO 開始年Ｗ(部位ＣＮＴ)
                          MOVE 施記－施術月 TO 開始月Ｗ(部位ＣＮＴ)
                          MOVE 施記－施術日 TO 開始日Ｗ(部位ＣＮＴ)
                          MOVE "YES" TO 終了フラグ２
                      END-IF
                      PERFORM 施術記録Ｆ読込
                  END-PERFORM
               END-IF
           END-PERFORM.
037520*================================================================*
      */20230817
028370*================================================================*
028380 請求先情報取得 SECTION.
028390*
028400****************************************************
028410* 連結データから保険者マスタより請求先を取得する。 *
028420* ※市－請求先情報区分=1の場合請求先マスタを使用   *
028430* ● 請求先...... 請求先名称Ｗに格納               *
028440****************************************************
028450     MOVE 助成種別ＷＲ           TO 市－公費種別.
028460     MOVE 費用負担者番号助成ＷＲ TO 市－市町村番号.
028470*
028480     READ 市町村マスタ
028490     INVALID KEY
028500         MOVE SPACE              TO 請求先名称Ｗ
028510     NOT INVALID KEY
028520         IF 市－請求先区分 = 1
028530             MOVE 助成種別ＷＲ           TO 請先－保険種別
028540             MOVE 費用負担者番号助成ＷＲ TO 請先－保険者番号
028550             READ 請求先マスタ
028560             INVALID KEY
028570                 MOVE SPACE        TO 請求先名称Ｗ
028580             NOT INVALID KEY
028590                 MOVE 請先－保険者名称  TO 請求先名称Ｗ
028600             END-READ
028610         ELSE
028620             MOVE 市－市町村名称  TO 請求先名称Ｗ
028630         END-IF
028640     END-READ.
028650*
           STRING 請求先名称Ｗ DELIMITED BY SPACE
                  "　殿"       DELIMITED BY SIZE
                  INTO 請求先名称Ｗ
           END-STRING.
027590*================================================================*
041780******************************************************************
041790 END PROGRAM YAZ6421.
041800******************************************************************
