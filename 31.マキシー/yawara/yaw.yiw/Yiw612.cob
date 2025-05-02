000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YIW612.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090* アイワ   レセプト印刷（柔＋ｳｨﾝﾄﾞｳｽﾞ版）*
000100*         MED = YAW610 YIW612P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2015-09-10
000130 DATE-COMPILED.          2015-09-10
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
           SELECT  料金マスタ      ASSIGN      TO        RYOUKINL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  料－区分コード
                                                                料－部位コード
                                                                料－開始和暦年月
                                   FILE STATUS              IS  状態キー
                                   LOCK        MODE         IS  AUTOMATIC.
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
001739*  振込口座
001740     SELECT 振込口座Ｆ       ASSIGN      TO     "C:\MAKISHISYS\YAWOBJ\IWKOUZA.DAT"
001741                             ORGANIZATION             IS  LINE SEQUENTIAL
001742                             ACCESS MODE              IS  SEQUENTIAL
001743                             FILE STATUS              IS  状態キー
001744                             LOCK        MODE         IS  AUTOMATIC.
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
002210*                           ［ＲＬ＝  ２５６］
002220 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
002230     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
002240*                           ［ＲＬ＝  １２８］
002250 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002260     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
      *                           ［ＲＬ＝  ３２０］
       FD  料金マスタ          BLOCK   CONTAINS   1   RECORDS.
           COPY RYOUKIN         OF  XFDLIB  JOINING   料   AS  PREFIX.
           COPY RYOUKNA         OF  XFDLIB  JOINING   料Ａ AS  PREFIX.
           COPY RYOUKNB         OF  XFDLIB  JOINING   料Ｂ AS  PREFIX.
           COPY RYOUKNC         OF  XFDLIB  JOINING   料Ｃ AS  PREFIX.
           COPY RYOUKND         OF  XFDLIB  JOINING   料Ｄ AS  PREFIX.
           COPY RYOUKNE         OF  XFDLIB  JOINING   料Ｅ AS  PREFIX.
           COPY RYOUKNF         OF  XFDLIB  JOINING   料Ｆ AS  PREFIX.
002270*                           ［ＲＬ＝  １２８］
002280 FD  負傷原因Ｆ          BLOCK   CONTAINS   1   RECORDS.
002290     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
002300*                           ［ＲＬ＝  １２８］
002310 FD  ＩＤ管理マスタ          BLOCK   CONTAINS   1   RECORDS.
002320     COPY IDKANR    OF  XFDLIB  JOINING   ＩＤ管   AS  PREFIX.
002330*                           ［ＲＬ＝  ２５６］
002340 FD  市町村マスタ          BLOCK   CONTAINS   1   RECORDS.
002350     COPY SITYOSN        OF  XFDLIB  JOINING   市   AS  PREFIX.
002470*                           ［ＲＬ＝  ６４０］
002480 FD  会情報マスタ        BLOCK   CONTAINS   1   RECORDS.
002490     COPY KAIJOHO         OF  XFDLIB  JOINING   会情   AS  PREFIX.
002390**
002294 FD  振込口座Ｆ      BLOCK   CONTAINS   1   RECORDS.
002295 01  口座－レコード.
002296     03  口座－レコードデータ               PIC X(128).
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
002570 FD  印刷ファイル.
002580     COPY YIW612P        OF  XMDLIB.
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
003630       05 負傷原因内容１ＸＷ           PIC X(80)  VALUE SPACE.
003640       05 負傷原因内容２ＸＷ           PIC X(80)  VALUE SPACE.
003640       05 負傷原因内容３ＸＷ           PIC X(80)  VALUE SPACE.
003650       05 負傷原因内容４ＸＷ           PIC X(78)  VALUE SPACE.
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
       01 金属副子ＣＭ                       PIC X(140) VALUE SPACE.
       01 運動後療ＣＭ                       PIC X(68)  VALUE SPACE.
004355*
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
      */冷温罨法電療料単価印字/110824*
          03 冷罨法料単価Ｗ                  PIC 9(4)  VALUE ZERO.
          03 温罨法料単価Ｗ                  PIC 9(4)  VALUE ZERO.
          03 電療料単価Ｗ                    PIC 9(4)  VALUE ZERO.
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
007550       05 受理年Ｗ                     PIC 9(2)   VALUE ZERO.
007560       05 受理月Ｗ                     PIC 9(2)   VALUE ZERO.
007570       05 受理日Ｗ                     PIC 9(2)   VALUE ZERO.
007580    03 最終通院年月日Ｗ.
007590       05 最終通院年Ｗ                 PIC 9(2)   VALUE ZERO.
007600       05 最終通院月Ｗ                 PIC 9(2)   VALUE ZERO.
007610       05 最終通院日Ｗ                 PIC 9(2)   VALUE ZERO.
007620    03 柔整師年月日Ｗ.
007630       05 柔整師年Ｗ                   PIC 9(2)   VALUE ZERO.
007640       05 柔整師月Ｗ                   PIC 9(2)   VALUE ZERO.
007650       05 柔整師日Ｗ                   PIC 9(2)   VALUE ZERO.
007660    03 患者委任年月日Ｗ.
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
          03 支払機関.
             05 金融機関名Ｗ.
                07 金融機関名１Ｗ            PIC X(8)  VALUE SPACE.
                07 金融機関名２Ｗ            PIC X(8)  VALUE SPACE.
                07 金融機関名３Ｗ            PIC X(8)  VALUE SPACE.
                07 金融機関名４Ｗ            PIC X(8)  VALUE SPACE.
                07 金融機関名５Ｗ            PIC X(8)  VALUE SPACE.
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
          03 受給者番号Ｗ                    PIC X(15) VALUE SPACE.
008040    03 請求先名称Ｗ.
008050       05 請求先名称１Ｗ               PIC X(40)  VALUE SPACE.
008060       05 請求先名称２Ｗ               PIC X(40)  VALUE SPACE.
008070    03 保険種別Ｗ                      PIC 9(2)   VALUE ZERO.
008070    03 公費種別Ｗ                      PIC 9(2)   VALUE ZERO.
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
008140       05 被保険者住所Ｗ.
008150          07 被保険者住所１Ｗ          PIC X(50)  VALUE SPACE.
008160          07 被保険者住所２Ｗ          PIC X(50)  VALUE SPACE.
008990       05 電話番号Ｗ                   PIC X(35)  VALUE SPACE.
008170    03 患者情報Ｗ.
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
008290          07 令和チェックＷ            PIC N(1)  VALUE SPACE.
008300          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
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
                07 負傷原因ＸＷ              PIC X(80)  VALUE SPACE.
008390*
008400    03 保険種別名称Ｗ                  PIC N(1)  VALUE SPACE.
008410    03 助成印Ｗ                        PIC N(1)  VALUE SPACE.
008420    03 特別コメントＷ                  PIC X(16) VALUE SPACE.
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
          03 運動料Ｗ                           PIC 9(4)  VALUE ZERO.
009100************
009110* 備考情報 *
009120************
010000 01 備考情報Ｗ.
010010    03 適用１Ｗ                        PIC N(38) VALUE SPACE.
010020    03 適用２Ｗ                        PIC N(38) VALUE SPACE.
010020    03 適用３Ｗ                        PIC X(40) VALUE SPACE.
009250*
009260    03 経過コメントＷ                  PIC N(60) VALUE SPACE.
      *
       01 施術和暦年月日ＣＷ.
         03 施術和暦年月ＣＷ.
           05 施術和暦ＣＷ                   PIC 9.
           05 施術年月ＣＷ.
              07 施術年ＣＷ                  PIC 9(2).
              07 施術月ＣＷ                  PIC 9(2).
         03 施術日ＣＷ                       PIC 9(2).
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
005260 01 振込口座Ｗ.
005261    03 請求保険者番号Ｗ                PIC X(10)  VALUE SPACE.
005262    03 請求保険者名Ｗ                  PIC X(100) VALUE SPACE.
005263    03 請求口座番号Ｗ                  PIC X(10)  VALUE SPACE.
005263    03 金融機関コードＷ                PIC X(8)   VALUE SPACE.
005261 01 比較保険者番号Ｗ                   PIC X(10)  VALUE SPACE.
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
002974     MOVE "YIW612"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
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
           OPEN INPUT 料金マスタ.
               MOVE NC"料金" TO ファイル名.
               PERFORM オープンチェック.
015050     OPEN INPUT   負傷原因Ｆ.
015060         MOVE NC"負傷原因" TO ファイル名.
015070         PERFORM オープンチェック.
015080     OPEN INPUT   ＩＤ管理マスタ
015090         MOVE NC"ＩＤ" TO ファイル名.
015100         PERFORM オープンチェック.
015110     OPEN INPUT 市町村マスタ.
015120         MOVE NC"市町村" TO ファイル名.
015130         PERFORM オープンチェック.
015160     OPEN INPUT   会情報マスタ.
015170         MOVE NC"会情" TO ファイル名.
015180         PERFORM オープンチェック.
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
007556     PERFORM 振込口座セット.
026961**
026962     IF 受－助成種別 NOT = ZERO
026963        PERFORM 助成レセまとめ判定
026964     ELSE
026965        MOVE SPACE TO 助成レセまとめフラグ
026966     END-IF.
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
           IF ( 助成種別ＷＲ NOT = ZERO )
              MOVE NC"○"              TO 助成丸
016450        MOVE 助成印Ｗ            TO 助成印
           END-IF.
016000********************
016010* 受診者情報セット *
016020********************
      */千葉県子ども医療費助成事業
           IF (連レ－保険種別 >=  50  ) AND
              (受－助成種別    = "60" ) AND
              (受－費用負担者番号助成(1:4) = "8312" )
               MOVE "千葉県子ども医療費助成事業" TO タイトル２
           END-IF
      */千葉県重度心身障害医療費助成事業 本体も重心を書く150914
      *     IF (連レ－保険種別 >=  50  ) AND
           IF (受－助成種別    = "53" ) AND
              (受－費用負担者番号助成(1:4) = "8112" )
               IF (連レ－保険種別 >=  50  )
                   MOVE "千葉県重度心身障害者（児）医療費助成" TO タイトル２
               END-IF
               MOVE NC"重心"         TO 重心
               MOVE NC"○"           TO 重心丸
           END-IF
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
016030     MOVE 施術年Ｗ           TO 施術年.
016040     MOVE 施術月Ｗ           TO 施術月.
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
016200     MOVE 請求先名称Ｗ        TO 保険者名称.
016190*     IF ( 請求先名称２Ｗ = SPACE )
016200*        MOVE 請求先名称Ｗ     TO 保険者名称 保険者名称２.
016210*     ELSE
016220        MOVE 請求先名称１Ｗ   TO 保険者名称１
016230        MOVE 請求先名称２Ｗ   TO 保険者名称２
016240*     END-IF.
           IF 連レ－保険種別 > 50
               IF 市町村番号Ｗ(1:2) = "99"
                   MOVE SPACE            TO 公費負担者番号
               ELSE
                   MOVE 市町村番号Ｗ     TO 公費負担者番号
               END-IF
               MOVE 受給者番号Ｗ         TO 受給者番号
           END-IF.
016250***     MOVE 被保険者カナＷ      TO 被保険者カナ.
016260     MOVE 被保険者氏名Ｗ      TO 被保険者氏名.
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
016300     MOVE 被保険者住所１Ｗ    TO 住所１.
016310     MOVE 被保険者住所２Ｗ    TO 住所２.
016320***     MOVE 患者カナＷ          TO 患者カナ.
016330     MOVE 患者氏名Ｗ          TO 患者氏名.
016340     MOVE 男チェックＷ        TO 男チェック.
016350     MOVE 女チェックＷ        TO 女チェック.
016360*     MOVE 性別Ｗ               TO 性別.
016370     MOVE 明治チェックＷ      TO 明治チェック.
016380     MOVE 大正チェックＷ      TO 大正チェック.
016390     MOVE 昭和チェックＷ      TO 昭和チェック.
016400     MOVE 平成チェックＷ      TO 平成チェック.
016400     MOVE 令和チェックＷ      TO 令和チェック.
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
016500     MOVE 負傷原因Ｗ(8)       TO 負傷原因８.
016510*
016520***     MOVE 助成印Ｗ            TO 助成印.
016530***     MOVE 保険種別名称Ｗ      TO 保険種別.
016540*
      */助成の負担者番号、受給者番号を印刷する/131004
           IF ( 市町村番号Ｗ(1:2) NOT = "99" )
               MOVE 市町村番号Ｗ TO 公費負担者番号
           END-IF.
           IF ( 受給者番号Ｗ(1:1) = "*"  ) OR
              ( 受給者番号Ｗ(1:2) = "＊" )
               MOVE SPACE        TO 受給者番号
           ELSE
               MOVE 受給者番号Ｗ TO 受給者番号
           END-IF.
      **/大阪府内の国保退職後期高齢＋助成の場合は負担者受給者番号を記載する
      *     IF (受－保険種別 = 01      ) AND (受－保険者番号(1:2) = "27") OR
      *        (受－保険種別 = 05 OR 08) AND (受－保険者番号(3:2) = "27")
      *         IF ( 市町村番号Ｗ(1:2) NOT = "99" )
      *             MOVE 市町村番号Ｗ TO 公費負担者番号
      *         END-IF
      *         IF ( 受給者番号Ｗ(1:1) = "*"  ) OR
      *            ( 受給者番号Ｗ(1:2) = "＊" )
      *             MOVE SPACE        TO 受給者番号
      *         ELSE
      *             MOVE 受給者番号Ｗ TO 受給者番号
      *         END-IF
      *     END-IF.
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
019380     MOVE 金属副子加算料ＷＲ           TO  金属副子加算料.
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
      ********************************
      */冷温罨法電療料単価印字      /*
      ********************************
           MOVE 冷罨法料単価Ｗ            TO 冷罨法単価.
           MOVE 温罨法料単価Ｗ            TO 温罨法単価.
           MOVE 電療料単価Ｗ              TO 電療単価.
      *
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
021720        MOVE 部位５Ｗ                     TO 部位５８
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
021720        MOVE 部位５Ｗ                     TO 部位５０
021730     END-IF.
020690*
021750     MOVE 適用１Ｗ                       TO 適用１.
021760     MOVE 適用２Ｗ                       TO 適用２.
021760     MOVE 適用３Ｗ                       TO 適用３.
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
              MOVE 48           TO 連金運－会コード
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
020740     MOVE レセ－合計                     TO 合計.
           MOVE レセ－一部負担金               TO 一部負担金.
           MOVE レセ－請求金額                 TO 請求金額.
020770*
      */助成レセ時の金額欄
           IF 連レ－保険種別 > 50
               EVALUATE TRUE
      */千葉県の子ども医療費助成/120314
      */千葉県の重度心身障害医療費助成/150703
               WHEN ((助成種別ＷＲ = 60) AND (費用負担者番号助成ＷＲ(1:4) =  "8312")) OR
                    ((助成種別ＷＲ = 53) AND (費用負担者番号助成ＷＲ(1:4) =  "8112"))
                   MOVE "X" TO EDIT-MODE OF           一部負担金
                   MOVE レセ－一部負担金   TO 一部負担金２
                   MOVE レセ－受給者負担額 TO 受給者負担額２
                   MOVE レセ－助成請求金額 TO 請求金額
      *             MOVE "――――――――" TO 横線
               WHEN OTHER
      */通常金額欄
                   MOVE "一部負担金相当額（医療助成費）"  TO 受給者負担額ＣＭ
                   MOVE "請求金額（医療助成費）"          TO 助成請求額ＣＭ
                   MOVE "円、"               TO 円１
                   MOVE "円"                 TO 円２
                   MOVE レセ－受給者負担額   TO 受給者負担額
                   MOVE レセ－助成請求金額   TO 助成請求額
               END-EVALUATE
      *     ELSE
      */千葉県の重度心身障害医療費助成の本体に助成の金額/150914
      *         IF ((助成種別ＷＲ = 53) AND (費用負担者番号助成ＷＲ(1:4) =  "8112"))
      *             MOVE "自己負担金額："     TO 自己負担金ＣＭ
      *             MOVE "円"                 TO 円ＣＭ１
      *             MOVE レセ－受給者負担額   TO 自己負担金
      *             MOVE "公費負担金額："     TO 一部負担金ＣＭ
      *             MOVE "円"                 TO 円ＣＭ
      *             MOVE レセ－助成請求金額   TO 受給者負担額
      *         END-IF
           END-IF.
020780**------------------------------------------------------------------------------------*
020790** 特別（助成レセなしで、本体レセにまとめる時、金額は助成込み・適用２に助成種別印字）
020800*     IF ( 助成レセまとめフラグ = "YES" )
020810*         PERFORM 助成料金計算
020820*         MOVE 連計－費用額             TO 合計
020830*         MOVE 連計－負担額助成         TO 一部負担金
020840*     / 引き算する/
020850*         COMPUTE 請求金額 = 連計－費用額 - 連計－負担額助成
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
021060**------------------------------------------------------------------------------------*
021087*
021088**********************
021090* 施術所データセット *
021100**********************
           MOVE 都道府県ＪＩＳＷ       TO 都道府県番号.
021110     MOVE 柔整師番号Ｗ           TO 柔整師番号.
021120     STRING "ｱｲﾜ-"                    DELIMITED BY SIZE
                  接骨師会会員番号Ｗ(1:4)   DELIMITED BY SIZE
             INTO 会員番号
           END-STRING.
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
           MOVE 金融機関名１Ｗ           TO 銀行名１.
           MOVE 金融機関名２Ｗ           TO 銀行名２.
           MOVE 支店名１Ｗ               TO 支店名１.
           MOVE 支店名２Ｗ               TO 支店名２.
           MOVE 振込チェックＷ           TO 振込チェック.
           MOVE 普通チェックＷ           TO 普通チェック.
           MOVE 当座チェックＷ           TO 当座チェック.
           MOVE 銀行チェックＷ           TO 銀行チェック.
           MOVE 金庫チェックＷ           TO 金庫チェック.
           MOVE 農協チェックＷ           TO 農協チェック.
           MOVE 本店チェックＷ           TO 本店チェック.
           MOVE 支店チェックＷ           TO 支店チェック.
           MOVE 本支所チェックＷ         TO 本支所チェック.
021660     MOVE 口座名義人カナＷ         TO 口座名義人カナ１.
021670     MOVE 口座名義人Ｗ             TO 口座名義人.
021390***     MOVE 預金種別コメントＷ     TO 預金種別.
021400     MOVE 口座番号Ｗ             TO 口座番号.
021430*
021440* / 柔整師・患者委任日 /
021450     MOVE 柔整師年Ｗ             TO 受理年.
021460     MOVE 柔整師月Ｗ             TO 受理月.
021470     MOVE 柔整師日Ｗ             TO 受理日.
021480* ( 委任年月日 印刷するか )
021490     IF ( 連入－委任印刷  = ZERO )
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
021660     MOVE 患者番号ＷＲ        TO 患者番号.
021670     MOVE 枝番ＷＲ            TO 枝番.
021660*     MOVE 患者番号ＷＲ        TO 患者番号２.
021670*     MOVE 枝番ＷＲ            TO 枝番２.
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
021780*******     PERFORM テスト印字処理.
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
021910     INITIALIZE YIW612P.
021900     MOVE SPACE TO YIW612P.
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
22400*
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
      ********************************
      */冷温罨法電療料単価印字      /*
      ********************************
           MOVE 01             TO 料－区分コード.
           MOVE ZEROS          TO 料－負傷種別.
           MOVE ZEROS          TO 料－部位.
           MOVE ZEROS          TO 料－左右区分.
           MOVE ZEROS          TO 料－負傷位置番号.
           MOVE 施術和暦ＷＲ   TO 料－開始和暦 施術和暦ＣＷ.
           MOVE 施術年ＷＲ     TO 料－開始年   施術年ＣＷ.
           MOVE 施術月ＷＲ     TO 料－開始月   施術月ＣＷ.
      *
           START 料金マスタ KEY IS <= 料－区分コード 
                                      料－部位コード
                                      料－開始和暦年月
                                      REVERSED
           END-START.
      *
           IF 状態キー = "00"
               READ 料金マスタ NEXT
               AT END
      */エラー表示の修正
                   DISPLAY "施術年月に対応した料金がみつかりません"
                           " 受診者№=" レセ－患者コード
                           " 施術年月=" レセ－施術年 レセ－施術月   UPON CONS
                   PERFORM 終了処理
                   MOVE ZERO TO PROGRAM-STATUS
                   EXIT PROGRAM
               NOT AT END
      *
                   IF ( 施術和暦年月ＣＷ >= 料Ａ－開始和暦年月 ) AND
                      ( 施術和暦年月ＣＷ <= 料Ａ－終了和暦年月 )
                       MOVE 料Ａ－冷罨法料        TO 冷罨法料単価Ｗ
                       MOVE 料Ａ－温罨法料        TO 温罨法料単価Ｗ
                       MOVE 料Ａ－電療料          TO 電療料単価Ｗ
                   ELSE
                       DISPLAY "施術年月に対応した料金がみつかりません"
                               " 受診者№=" レセ－患者コード
                               " 施術年月=" レセ－施術年 レセ－施術月   UPON CONS
                       PERFORM 終了処理
                       MOVE ZERO TO PROGRAM-STATUS
                       EXIT PROGRAM
                   END-IF
               END-READ
           END-IF.
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
022870     MOVE レセ－長期逓減率１           TO 長期逓減率１ＷＲ.
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
023020     MOVE レセ－長期逓減率２           TO 長期逓減率２ＷＲ.
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
023180     MOVE レセ－長期逓減率３８           TO 長期逓減率３８ＷＲ.
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
023350     MOVE レセ－長期逓減率３０           TO 長期逓減率３０ＷＲ.
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
023690     MOVE レセ－長期逓減率４８           TO 長期逓減率４８ＷＲ.
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
023860     MOVE レセ－長期逓減率４０           TO 長期逓減率４０ＷＲ.
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
024380     MOVE レセ－長期逓減率５８           TO 長期逓減率５８ＷＲ.
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
024550     MOVE レセ－長期逓減率５０           TO 長期逓減率５０ＷＲ.
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
                 INTO 適用３Ｗ
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
024820*
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
025350*------------------------------------------------------------------------*
025360         EVALUATE 保険種別ＷＲ
025370         WHEN 01
025380             MOVE 保険者番号ＷＲ       TO 保険者番号比較Ｗ
025390             PERFORM 県施術ＩＤセット
025400         WHEN 08
               WHEN 05
025410             MOVE 保険者番号ＷＲ(3:6)  TO 保険者番号比較Ｗ
025420             PERFORM 県施術ＩＤセット
025430         WHEN 04
025440             PERFORM 共済番号セット
025450         WHEN 09
025460             PERFORM 自衛官番号セット
025470         END-EVALUATE
025480*
025490     END-READ.
023500** 振込先情報  / 会情報マスタより振込先情報を取得 /
023520     MOVE ZERO  TO  会情－柔整鍼灸区分
023510     MOVE 48    TO  会情－協会コード
023520     MOVE ZERO  TO  会情－保険種別
023530     MOVE ZERO  TO  会情－変更和暦年月
023540     READ 会情報マスタ
023550     NOT INVALID KEY
023560*         MOVE 会情－取引先銀行名      TO 取引先銀行名Ｗ
023570*         MOVE 会情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
023580*         MOVE 会情－預金種別          TO 預金種別Ｗ
023590*         MOVE 会情－口座番号          TO 口座番号Ｗ
023600         MOVE 会情－口座名義人        TO 口座名義人Ｗ
023610         MOVE 会情－口座名義人カナ    TO 口座名義人カナＷ
023780     END-READ.
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
025500*
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
025810        IF ( 保険者番号ＷＲ(1:2) = "31" )  OR
025820           ( 保険者番号ＷＲ = "34130021" )
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
026180        END-IF
026190     END-IF.
026200*
026210** 2. 地共済協議会
026220     MOVE SPACE  TO  脱出フラグ.
026230     IF ( 施情－地共済連番号 NOT = ZERO )
026240** 条件(保険者番号)
026250        IF ( 保険者番号ＷＲ(1:2) = "32" OR "33" OR "34" )  AND
026260           ( 保険者番号ＷＲ NOT = "34130021" )
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
026620        END-IF
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
      */前期高齢１割は８割給付に○/110721
                   IF (受－保険種別 NOT = 05 ) AND (受－特別区分 = 1)
                       MOVE SPACE  TO ９割チェックＷ
                       MOVE NC"○" TO ８割チェックＷ
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
027880         MOVE 受－公費種別     TO 公費種別Ｗ
027890** 全国土木の枝番削除
027900         IF ( 受－保険種別 = 01 ) AND ( 受－保険者番号(1:6) = "133033" )
027910            MOVE 受－保険者番号(1:6)  TO 保険者番号Ｗ
027920         END-IF
027930**
027940         MOVE 受－被保険者カナ TO 被保険者カナＷ
027950         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
027960         MOVE 受－郵便番号１   TO 郵便番号１Ｗ
027970         MOVE 受－郵便番号２   TO 郵便番号２Ｗ
027980         MOVE 受－住所１       TO 被保険者住所１Ｗ
027990         MOVE 受－住所２       TO 被保険者住所２Ｗ
      */ 電話番号追加 /42505
               IF 受－電話番号 NOT = SPACE
                  STRING "電話:"            DELIMITED BY SIZE
                         受－電話番号       DELIMITED BY SPACE
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
028230         WHEN 5
028240             MOVE NC"令和"  TO 元号Ｗ
028250             MOVE NC"○"    TO 令和チェックＷ
028260         END-EVALUATE
028270*
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
028450**
028460         IF ( 受－保険種別 = 01 OR 08 OR 05) AND
028470            ( 受－助成種別 NOT = ZERO )
028480            PERFORM 助成レセまとめ判定
028490         ELSE
028500            MOVE SPACE TO 助成レセまとめフラグ
028510         END-IF
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
028880 請求先情報取得 SECTION.
028890*
028900****************************************************
028910* 連結データから保険者マスタより請求先を取得する。 *
028920* ※保－請求先情報区分=1の場合請求先マスタを使用   *
028930* ● 請求先...... 請求先名称Ｗに格納               *
028940****************************************************
028950     MOVE 保険種別ＷＲ   TO 保－保険種別.
028960     MOVE 保険者番号ＷＲ TO 保－保険者番号.
028970     READ 保険者マスタ
028980     INVALID KEY
               IF ( 保険種別ＷＲ = 05 ) AND ( 施術和暦年月ＷＲ >= 42004 )
030800             MOVE 保険種別ＷＲ   TO 市－公費種別
030810             MOVE 保険者番号ＷＲ TO 市－市町村番号
030820             READ 市町村マスタ
030830             INVALID KEY
030840                 MOVE SPACE      TO 請求先名称Ｗ
030850             NOT INVALID KEY
031330                 MOVE 市－市町村名称    TO 請求先名称Ｗ
                   END-READ
               ELSE
030840             MOVE SPACE      TO 請求先名称Ｗ
               END-IF
029000     NOT INVALID KEY
029010* 社保、日雇は「社会保険事務所」をつける
029020                 EVALUATE 保険種別ＷＲ 
029030                 WHEN  02
029040                 WHEN  06
029050                     IF ( 保－接尾語区分 = 1 )
029060                        MOVE 保－保険者名称    TO 請求先名称Ｗ
029070                     ELSE
029080                        STRING 保－保険者名称    DELIMITED BY SPACE
029090                               "社会保険事務所"  DELIMITED BY SIZE
029100                               INTO 請求先名称Ｗ
029110                        END-STRING
029120                     END-IF
029130* 組合は支部名まで印字
029140                 WHEN  03
029150                     STRING 保－保険者名称  DELIMITED BY SPACE
029160                            "健康保険組合"  DELIMITED BY SIZE
029180                            保－支部部署名  DELIMITED BY SPACE
029190                            INTO 請求先名称Ｗ
029200                     END-STRING
029210* 共済は支部名まで印字
029220                 WHEN  04
                           IF 受－保険者番号 = "34130021"
                               MOVE 保－保険者名称 TO 請求先名称Ｗ
                           ELSE
029230                         STRING 保－保険者名称  DELIMITED BY SPACE
029240                                "共済組合"      DELIMITED BY SIZE
029260                                保－支部部署名  DELIMITED BY SPACE
029270                                INTO 請求先名称Ｗ
029280                         END-STRING
                           END-IF
029290                 WHEN OTHER
029300                     MOVE 保－保険者名称    TO 請求先名称Ｗ
029310                 END-EVALUATE
029320     END-READ.
           STRING 請求先名称Ｗ DELIMITED BY SPACE
                  "殿"         DELIMITED BY SIZE
             INTO 請求先名称Ｗ
           END-STRING. 
029330*
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
030110                 MOVE 99                   TO 終了年Ｗ(部位ＣＮＴ)
030120                 MOVE 99                   TO 終了月Ｗ(部位ＣＮＴ)
030130                 MOVE 99                   TO 終了日Ｗ(部位ＣＮＴ)
030140             ELSE
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
031490            MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
031500            MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
031510            MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
031520        END-IF
031530        IF ( 終了年月日Ｗ(部位ＣＮＴ) > 受理年月日Ｗ )
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
032150     MOVE "YIW612P"  TO  定義体名Ｐ.
032160     MOVE "SCREEN"   TO  項目群名Ｐ.
032170     WRITE YIW612P.
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
034780*================================================================*
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
037980               STRING NC"初検加算"       DELIMITED BY SIZE
037990                      初検加算時刻１Ｗ   DELIMITED BY SIZE
038000                      初検加算時刻２Ｗ   DELIMITED BY SIZE
038010                      初検加算時刻３Ｗ   DELIMITED BY SIZE
038020                      INTO 適用２Ｗ
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
035750     PERFORM 負傷原因セット.
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
039370     MOVE 56            TO  連摘文－文桁数.
039370*     MOVE 52            TO  連摘文－文桁数.
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
               IF 連レ－保険種別 > 50
036620             MOVE  1  TO  受－レセ印刷区分助成
               ELSE
036620             MOVE  1  TO  受－レセ印刷区分
               END-IF
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
038330     MOVE 受理年Ｗ   TO 最終通院年Ｗ.
038340     MOVE 受理月Ｗ   TO 最終通院月Ｗ.
038350     MOVE 受理日Ｗ   TO 最終通院日Ｗ.
038360***
038370* (柔整師側)
038380     EVALUATE レセプト日付区分Ｗ 
038390*    /  最終通院日 /
038400     WHEN ZERO
038410         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
038420         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
038430         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
038440*    /  月末日 /
038450     WHEN 1 
038460         PERFORM 月末日取得
038470         MOVE 受理年Ｗ     TO 柔整師年Ｗ
038480         MOVE 受理月Ｗ     TO 柔整師月Ｗ
038490         MOVE 受理日Ｗ     TO 柔整師日Ｗ
038500*    /  印字なし /
038510     WHEN 9
038520         MOVE ZERO         TO 柔整師年Ｗ
038530         MOVE ZERO         TO 柔整師月Ｗ
038540         MOVE ZERO         TO 柔整師日Ｗ
038550*    /  その他は、最終通院日 /
038560     WHEN OTHER
038570         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
038580         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
038590         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
038600     END-EVALUATE.
038610**
038620* (患者側)
038630     EVALUATE レセプト患者日付区分Ｗ 
038640*    /  最終通院日 /
038650     WHEN ZERO
038660         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
038670         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
038680         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
038690*    /  月末日 /
038700     WHEN 1 
038710         PERFORM 月末日取得
038720         MOVE 受理年Ｗ     TO 患者委任年Ｗ
038730         MOVE 受理月Ｗ     TO 患者委任月Ｗ
038740         MOVE 受理日Ｗ     TO 患者委任日Ｗ
038750*    /  印字なし /
038760     WHEN 9
038770         MOVE ZERO         TO 患者委任年Ｗ
038780         MOVE ZERO         TO 患者委任月Ｗ
038790         MOVE ZERO         TO 患者委任日Ｗ
038800*    /  その他は、最終通院日 /
038810     WHEN OTHER
038820         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
038830         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
038840         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
038850     END-EVALUATE.
038860*
038870*================================================================*
038880*================================================================*
038890 助成レセまとめ判定 SECTION.
038900**---------------------------------------------------------------------------*
038910** 市町村マスタを読み、レセまとめ区分＝１でかつ、本体保険が国保・退職
038920** の時は、フラグYES (金額を助成込みで印字）
038930**（例：横浜市の障害は、本体保険（国保系）のレセプト１枚で請求、助成レセはなし）
038940**---------------------------------------------------------------------------*
038950**
038960     MOVE SPACE TO 助成レセまとめフラグ.
038970*     MOVE SPACE TO 助成種別略称Ｗ.
038980**
038990*     MOVE 受－助成種別           TO 市－公費種別.
039000*     MOVE 受－費用負担者番号助成 TO 市－市町村番号.
039010*     READ 市町村マスタ
039020*     NOT INVALID KEY
039030*         IF ( 市－レセまとめ区分 = 1 )
039040*            IF (( 受－保険種別 = 01 ) AND ( 受－保険者番号(3:1) NOT = "3" )) OR
039050*               ( 受－保険種別 = 08 ) 
039060*                MOVE "YES" TO 助成レセまとめフラグ
039070**
039080*                MOVE 02            TO 名－区分コード
039090*                MOVE 受－助成種別  TO 名－名称コード
039100*                READ 名称マスタ
039110*                NOT INVALID KEY
039120*                    MOVE 名－略称  TO 助成種別略称Ｗ
039130*                END-READ
039140*            END-IF
039150*         END-IF
039160*     END-READ.
039170**
039180*** / CALL JRECEOFF /
039190*     IF ( 助成レセまとめフラグ = SPACE )
039200*        INITIALIZE 連レセまとめ－キー
039210*        MOVE 施術和暦ＷＲ TO 連レセまとめ－施術和暦
039220*        MOVE 施術年ＷＲ   TO 連レセまとめ－施術年
039230*        MOVE 施術月ＷＲ   TO 連レセまとめ－施術月
039240*        MOVE 患者番号ＷＲ TO 連レセまとめ－患者番号
039250*        MOVE 枝番ＷＲ     TO 連レセまとめ－枝番
039260**       1:助成レセプトなしの本体まとめの判定
039270*        MOVE 1            TO 連レセまとめ－判定区分
039280*        CALL   "JRECEOFF"
039290*        CANCEL "JRECEOFF"
039300**
039310*        IF ( 連レセまとめ－判定結果 = 1 )
           IF ( レセ－本体まとめ区分 = 1 )
039320           MOVE "YES" TO 助成レセまとめフラグ
039330*        END-IF
039340     END-IF.
039350*
039360*----------------------------------------------------------------------*
039370** / 神奈川県固有：摘要に負担者番号と受給者番号 /
039380     IF ( 助成レセまとめフラグ = "YES" ) AND
039390        ( 受－費用負担者番号助成(3:2) = "14" )
039400        IF ( 受－費用負担者番号助成(1:2) NOT = "99" )
039410*            MOVE ALL NC"￣" TO 横線１ 横線２ 横線３
039420*            MOVE ALL NC"｜" TO 縦線１ 縦線２
039430*            MOVE NC"｜"     TO 縦線３ 縦線４
039440*            MOVE NC"公費負担者番号"     TO 神奈川固定１
039450*            MOVE NC"受給者番号"         TO 神奈川固定２
039460*            MOVE NC"／"                 TO 神奈川固定３
039470            MOVE 受－費用負担者番号助成 TO 公費負担者番号
039480            MOVE 受－受益者番号助成     TO 受給者番号
039490        END-IF
039500     END-IF.
039510*
039520*================================================================*
039530*================================================================*
039540 助成料金計算 SECTION.
           EVALUATE 受－保険種別
           WHEN 05
               MOVE 2          TO レセ－レセ種別
           WHEN OTHER
               MOVE 1          TO レセ－レセ種別
           END-EVALUATE.
019550     MOVE 受－施術和暦 TO レセ－施術和暦.
019560     MOVE 受－施術年   TO レセ－施術年.
019570     MOVE 受－施術月   TO レセ－施術月.
019580     MOVE 受－患者番号 TO レセ－患者番号.
019590     MOVE 受－枝番     TO レセ－枝番.
019600     READ レセプトＦ
019630     INVALID KEY
              MOVE SPACE     TO レセ－レコード
              INITIALIZE        レセ－レコード
           END-READ.
039780*
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
           MOVE 連摘文－摘要文(8)    TO 長期理由文８.
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
040210           経過マスタ       受診者情報Ｆ        料金マスタ
040220           施術記録Ｆ       負傷データＦ        負傷原因Ｆ
040230           ＩＤ管理マスタ   市町村マスタ
040240           作業ファイル４.
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
           受理年 受理月 受理日 委任年 委任月 委任日 運動後療料 金属回数 運動回数
           .
           MOVE ALL "X" TO
           共済番号 地共済番号 県施術ＩＤ 保険者番号 記号番号 公費負担者番号 受給者番号 住所１ 住所２ 
           柔整師番号 口座番号
           銀行名１ 銀行名２ 支店名１ 支店名２ 口座名義人カナ１ 口座名義人
           施術所郵便番号１ 施術所郵便番号２ 
           施術所住所１ 施術所住所２ 施術所電話番号 代表者カナ 代表者名
           負傷原因１ 負傷原因２ 負傷原因３ 負傷原因４ 負傷原因５ 負傷原因６ 負傷原因７ 負傷原因８
           長期理由文１ 長期理由文２ 長期理由文３ 長期理由文４ 長期理由文５ 部位５８ 部位５０
           長期理由文６ 長期理由文７ 長期理由文８ 適用３
           接骨院名 代表者名 被保険者氏名 患者氏名  保険者名称 保険者名称１ 保険者名称２
           .
           MOVE ALL NC"Ｎ" TO
           負傷名１ 負傷名２ 負傷名３ 負傷名４ 負傷名５ 経過略称(1) 経過略称(2) 経過略称(3) 
           経過略称(4) 経過略称(5) 適用１ 適用２
           .
           MOVE NC"○" TO
           単独チェック 本人チェック 高一チェック 共済チェック 自チェック 社保チェック 
           組合チェック １０割チェック ９割チェック ２併チェック ６歳チェック ８割チェック 
           ７割チェック 後期チェック 退職チェック 国保チェック 家族チェック 高７チェック 
           男チェック 明治チェック 大正チェック 女チェック 昭和チェック 平成チェック 
           治癒チェック１ 中止チェック１ 転医チェック１ 治癒チェック２ 中止チェック２ 
           転医チェック２ 治癒チェック３ 中止チェック３ 転医チェック３ 治癒チェック４ 
           中止チェック４ 転医チェック４ 治癒チェック５ 中止チェック５ 転医チェック５ 新規チェック 
           継続チェック 深夜チェック 時間外チェック 休日チェック 固定料チェック 整復料チェック 
           施療料チェック 夜間チェック 暴風雨雪チェック 難路チェック 
           普通チェック 振込チェック 当座チェック 銀行チェック 金庫チェック 農協チェック 
           本店チェック 支店チェック 本支所チェック 令和チェック
           .
           MOVE "受給者負担額"                 TO 受給者負担額ＣＭ.
           MOVE "助成請求額"                   TO 助成請求額ＣＭ.
           MOVE "円"                           TO 円１ 円２.
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
023680*================================================================*
023691 振込口座セット SECTION.
023692*
023693*****************************************
023694*  保険者別に振込口座を設定する
023695*****************************************
023696*
023702     MOVE SPACE    TO 終了フラグ.
023703*
023716     OPEN INPUT 振込口座Ｆ.
023717             MOVE NC"振込" TO ファイル名.
023718             PERFORM オープンチェック.
023719*
023722     PERFORM 振込口座Ｆ読込.
023723     PERFORM UNTIL 終了フラグ NOT = SPACE
023724*        請求口座情報分解
023725         UNSTRING 口座－レコードデータ  DELIMITED BY ","
023726             INTO 請求保険者番号Ｗ 請求保険者名Ｗ 請求口座番号Ｗ 金融機関コードＷ
023728         END-UNSTRING
      *        金融機関コード(2004-135)の時は口座番号(1029444)固定
023731*        請求する保険者番号とマッチするか（先頭の保険者番号0は未登録時用なので無条件セット）
               IF 請求保険者番号Ｗ = 保険者番号Ｗ
                   IF 金融機関コードＷ = "2004-135"
023746                 MOVE "1029444"        TO 口座番号Ｗ
                   ELSE
023746                 MOVE 請求口座番号Ｗ   TO 口座番号Ｗ
023747             END-IF
                   MOVE "YES"                TO 終了フラグ
               ELSE
                   MOVE "3620000"        TO 口座番号Ｗ
023747         END-IF
023748         PERFORM 振込口座Ｆ読込
023749     END-PERFORM.
023719*
023752     CLOSE 振込口座Ｆ.
023719*
023703*/保険者番号が一致しなかった時、＠までの前方一致チェック/
023719*
           IF 口座番号Ｗ = "3620000"
023702         MOVE SPACE    TO 終了フラグ
023716         OPEN INPUT 振込口座Ｆ
023717             MOVE NC"振込" TO ファイル名
023718             PERFORM オープンチェック
023719*
023722         PERFORM 振込口座Ｆ読込
023723         PERFORM UNTIL 終了フラグ NOT = SPACE
023724*        請求口座情報分解
023725             UNSTRING 口座－レコードデータ  DELIMITED BY ","
023726                INTO 請求保険者番号Ｗ 請求保険者名Ｗ 請求口座番号Ｗ 金融機関コードＷ
023728             END-UNSTRING
      *
                   PERFORM VARYING カウンタ FROM 1 BY 1
                           UNTIL (請求保険者番号Ｗ(カウンタ:1) = "@") OR
                                 (カウンタ > 10)
                       CONTINUE
                   END-PERFORM
                   IF (請求保険者番号Ｗ(1:1) NOT = "@") AND
                      (請求保険者番号Ｗ(カウンタ:1) = "@") AND
                      (請求保険者番号Ｗ(1:カウンタ - 1) = 保険者番号Ｗ(1:カウンタ - 1))
023746                 MOVE 請求口座番号Ｗ   TO 口座番号Ｗ
                       MOVE "YES"            TO 終了フラグ
                   ELSE
                       MOVE "3620000"        TO 口座番号Ｗ
                   END-IF
023748             PERFORM 振込口座Ｆ読込
               END-PERFORM
023719*
023752         CLOSE 振込口座Ｆ
           END-IF.
023719*
023703*/保険者番号が一致しなかった時、＠終了からの後方一致チェック/
023719*
           IF 口座番号Ｗ = "3620000"
023702         MOVE SPACE    TO 終了フラグ
023716         OPEN INPUT 振込口座Ｆ
023717             MOVE NC"振込" TO ファイル名
023718             PERFORM オープンチェック
023719*
023722         PERFORM 振込口座Ｆ読込
023723         PERFORM UNTIL 終了フラグ NOT = SPACE
023724*        請求口座情報分解
023725             UNSTRING 口座－レコードデータ  DELIMITED BY ","
023726                INTO 請求保険者番号Ｗ 請求保険者名Ｗ 請求口座番号Ｗ 金融機関コードＷ
023728             END-UNSTRING
      *
                   PERFORM VARYING カウンタ FROM 1 BY 1
                           UNTIL (請求保険者番号Ｗ(カウンタ:1) NOT = "@") OR
                                 (カウンタ > 10)
                       CONTINUE
                   END-PERFORM
                   IF (請求保険者番号Ｗ(1:1) = "@") AND
                      (請求保険者番号Ｗ(カウンタ:10 - カウンタ) = 保険者番号Ｗ(カウンタ:10 - カウンタ))
023746                 MOVE 請求口座番号Ｗ   TO 口座番号Ｗ
                       MOVE "YES"            TO 終了フラグ
                   ELSE
                       MOVE "3620000"        TO 口座番号Ｗ
                   END-IF
023748             PERFORM 振込口座Ｆ読込
               END-PERFORM
023719*
023752         CLOSE 振込口座Ｆ
           END-IF.
      *
           IF 金融機関コードＷ = "2004-135"
023560         MOVE "商工組合中央"      TO 金融機関名Ｗ
009758         MOVE NC"○"              TO 金庫チェックＷ
023570         MOVE "押上"              TO 支店名Ｗ
               MOVE NC"○"              TO 支店チェックＷ
               MOVE NC"○"              TO 普通チェックＷ
           ELSE
023560         MOVE "千葉"              TO 金融機関名Ｗ
009758         MOVE NC"○"              TO 銀行チェックＷ
023570         MOVE "ひまわり第一"      TO 支店名Ｗ
               MOVE NC"○"              TO 支店チェックＷ
               MOVE NC"○"              TO 普通チェックＷ
           END-IF.
023754*
023755*================================================================*
023756 振込口座Ｆ読込 SECTION.
023757*
023761     READ 振込口座Ｆ
023762     AT END
023763         MOVE "YES"  TO 終了フラグ
023764     END-READ.
023767*
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
041780******************************************************************
041790 END PROGRAM YIW612.
041800******************************************************************
