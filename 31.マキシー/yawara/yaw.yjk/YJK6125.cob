000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YJK6125.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*      日本柔整共済会   一般レセプト印刷（柔+ｳｨﾝﾄﾞｳｽﾞ版）        *
000100*         MED = YAW610 YJK6125P                                  *
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2012-07-24
000130 DATE-COMPILED.          2012-07-24
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
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
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
002580     COPY YJK6125P        OF  XMDLIB.
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
003550*
003560*************
003570* 共済番号用
003580 01 共済連番号集団Ｗ.
003590    03 共済連番号名Ｗ                  PIC X(14)  VALUE SPACE.
003600    03 共済連番号名ＮＷ REDEFINES  共済連番号名Ｗ  PIC N(7).
003610    03 共済連番号Ｗ                    PIC X(6)  VALUE SPACE.
003620    03 共済連番号単位Ｗ                PIC X(2)  VALUE SPACE.
003630    03 共済連番号単位ＮＷ REDEFINES  共済連番号単位Ｗ  PIC N.
003640* 自衛官番号用
003650 01 自衛官番号集団Ｗ.
003660    03 自衛官番号名Ｗ                  PIC X(8)  VALUE SPACE.
003670    03 自衛官番号名ＮＷ REDEFINES  自衛官番号名Ｗ  PIC N(4).
003680    03 自衛官番号Ｗ                    PIC X(6)  VALUE SPACE.
003690    03 自衛官番号単位Ｗ                PIC X(2)  VALUE SPACE.
003700    03 自衛官番号単位ＮＷ REDEFINES  自衛官番号単位Ｗ  PIC N.
003710 01 脱出フラグ                         PIC X(3)  VALUE SPACE.
003720*
003730* 保険者番号
003740 01 保険者番号比較Ｗ                   PIC X(6)   VALUE SPACE.
003750*
003760** 前月初検のみ用
003770 01 初日再検フラグ                     PIC X(3)  VALUE SPACE.
003780 01 前月フラグ                         PIC X(3)  VALUE SPACE.
003790*
003800 01 計算年月日Ｗ.
003810    03 計算和暦Ｗ                      PIC 9(1)  VALUE ZERO.
003820    03 計算年Ｗ                        PIC S9(2)  VALUE ZERO.
003830    03 計算月Ｗ                        PIC S9(2)  VALUE ZERO.
003840    03 計算日Ｗ                        PIC S9(2)  VALUE ZERO.
003850 01 開始年月日２Ｗ.
003860    03 開始和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003870    03 開始年２Ｗ                      PIC 9(2)  VALUE ZERO.
003880    03 開始月２Ｗ                      PIC 9(2)  VALUE ZERO.
003890    03 開始日２Ｗ                      PIC 9(2)  VALUE ZERO.
003900    03 開始西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003910 01 終了年月日２Ｗ.
003920    03 終了和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003930    03 終了年２Ｗ                      PIC 9(2)  VALUE ZERO.
003940    03 終了月２Ｗ                      PIC 9(2)  VALUE ZERO.
003950    03 終了日２Ｗ                      PIC 9(2)  VALUE ZERO.
003960    03 終了西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003970***
003980** 負傷原因・長期理由印刷区分用
003990 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
004000 01 長期理由印刷区分Ｗ                 PIC 9 VALUE ZERO.
004010*
004020** レセ下段の日付区分用 (0:最終通院日、1:月末日、9:印字なし)
004030 01 レセプト日付区分Ｗ                 PIC 9 VALUE ZERO.
004040 01 レセプト患者日付区分Ｗ             PIC 9 VALUE ZERO.
004050*
004060** 月末日用
004070 01 施術西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
004080 01 商Ｗ                               PIC 9(3)  VALUE ZERO.
004090 01 余Ｗ                               PIC 9(3)  VALUE ZERO.
004100*
004110** 枝番判定用
004120 01 開始診療日手動区分Ｗ               PIC 9    VALUE ZERO.
004130*
004140*
004150** 助成レセまとめ用
004160 01 助成レセまとめフラグ               PIC X(3)  VALUE SPACE.
004170 01 助成種別略称Ｗ                     PIC N(4)  VALUE SPACE.
004180 01 助成種別略称Ｗ２                   PIC N(4)  VALUE SPACE.
004190*
004200** レセ摘要用( N(38)固定） /
004210 01 負傷の経過２Ｗ.
004220    03 負傷の経過行２Ｗ                PIC X(76) OCCURS 2 VALUE SPACE.
004230 01 負傷の経過２ＮＷ REDEFINES 負傷の経過２Ｗ.
004240    03 負傷の経過行２ＮＷ              PIC N(38) OCCURS 2.
004250*
004260** レセ摘要用( N(19)固定） /
004270 01 負傷の経過Ｗ.
004280    03 負傷の経過行Ｗ                  PIC X(76) OCCURS 4 VALUE SPACE.
004290 01 負傷の経過ＮＷ REDEFINES 負傷の経過Ｗ.
004300    03 負傷の経過行ＮＷ                PIC N(38) OCCURS 4.
004310*
004320* 負傷原因印刷区分
004330 01 レセ負傷原因印刷区分Ｗ             PIC 9    VALUE ZERO.
002580 01 レセ長期理由印刷区分Ｗ             PIC 9    VALUE ZERO.
      *
      */金属副子・運動後療の変更・追加/1805
       01 金属副子ＣＭ                       PIC X(200) VALUE SPACE.
       01 運動後療ＣＭ                       PIC X(68)  VALUE SPACE.
004340*
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
004350****************
004360* 連結項目待避 *
004370****************
004380*    ************
004390*    * 印刷キー *
004400*    ************
004410 01 対象データＷＲ.
004420    03 施術和暦年月ＷＲ.
004430       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
004440       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
004450       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
004460    03 施術和暦名称Ｗ                   PIC N(2)  VALUE SPACE.
004470    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004480    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
004490    03 公費種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004500    03 費用負担者番号ＷＲ               PIC X(10) VALUE SPACE.
004510    03 助成種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004520    03 費用負担者番号助成ＷＲ           PIC X(10) VALUE SPACE.
004530    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
004540    03 患者カナＷＲ                     PIC X(50) VALUE SPACE.
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
         03 初検時相談料ＷＲ              PIC 9(4)    VALUE ZERO.
004680   03 再検料ＷＲ                    PIC 9(5)    VALUE ZERO.
004690   03 往療ＷＲ.
004700      05 往療距離ＷＲ               PIC 9(2)V9  VALUE ZERO.
004710      05 往療回数ＷＲ               PIC 9(2)    VALUE ZERO.
004720      05 往療料ＷＲ                 PIC 9(5)    VALUE ZERO.
004730      05 往療加算料ＷＲ             PIC 9(5)    VALUE ZERO.
004740   03 金属副子加算料ＷＲ            PIC 9(5)    VALUE ZERO.
004750   03 施術情報提供料ＷＲ            PIC 9(5)    VALUE ZERO.
004760   03 合計ＷＲ                      PIC 9(7)    VALUE ZERO.
004770   03 一部負担金ＷＲ                PIC 9(6)    VALUE ZERO.
004780   03 請求金額ＷＲ                  PIC 9(7)    VALUE ZERO.
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
007720           07 取引先銀行名１Ｗ         PIC X(12)  VALUE SPACE.
007730           07 取引先銀行名２Ｗ         PIC X(12)  VALUE SPACE.
007740           07 FILLER                   PIC X(16)  VALUE SPACE.
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
007890**************
007900* 受診者情報 *
007910**************
007920 01 受診者情報Ｗ.
      */元号修正/20190426
          03 施術和暦Ｗ                      PIC 9(1)   VALUE ZERO.
007930    03 施術年月Ｗ.
007940       05 施術年Ｗ                     PIC 9(2)   VALUE ZERO.
007950       05 施術月Ｗ                     PIC 9(2)   VALUE ZERO.
007960*    03 記号Ｗ                          PIC N(12)  VALUE SPACE.
007570    03 記号Ｗ.
007580       05 印刷記号Ｗ                   PIC N(12)  VALUE SPACE.
          03 記号番号Ｗ.
             05 記号番号ＸＷ                 PIC X(40) VALUE SPACE.
008770    03 番号Ｗ.
008780       05 印刷番号Ｗ                   PIC X(15)  VALUE SPACE.
008790       05 FILLER                       PIC X(15)  VALUE SPACE.
007970*    03 番号Ｗ                          PIC X(30)  VALUE SPACE.
007980    03 保険者番号ＷＴ.
007990       05 括弧３                       PIC X(1)   VALUE "[".
008000       05 保険者番号ＷＰ               PIC X(8)   VALUE SPACE.
008010       05 括弧４                       PIC X(1)   VALUE "]".
008020    03 保険者番号Ｗ.
008030       05 印刷保険者番号Ｗ             PIC X(8)   VALUE SPACE.
008040       05 FILLER                       PIC X(2)   VALUE SPACE.
008050    03 市町村番号Ｗ.
008060       05 印刷市町村番号Ｗ             PIC X(8)   VALUE SPACE.
008070       05 FILLER                       PIC X(2)   VALUE SPACE.
008080*    03 受給者番号Ｗ.
008090*       05 印刷受給者番号Ｗ             PIC X(7)   VALUE SPACE.
008100*       05 FILLER                       PIC X(13).
          03 受給者番号Ｗ.
             05 印刷受給者番号Ｗ             PIC X(7)  VALUE SPACE.
             05 印刷受給者番号２Ｗ           PIC X(8)  VALUE SPACE.
008110    03 請求先名称Ｗ.
008120       05 請求先名称１Ｗ               PIC X(54)  VALUE SPACE.
008130       05 請求先名称２Ｗ               PIC X(32)  VALUE SPACE.
008140    03 請求先名称ＷＴ.
008150       05 請求先名称１ＷＴ             PIC X(40)  VALUE SPACE.
008160       05 請求先名称２ＷＴ             PIC X(30)  VALUE SPACE.
008170    03 保険種別Ｗ                      PIC 9(2)   VALUE ZERO.
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
008180    03 被保険者情報Ｗ.
008190       05 被保険者カナＷ               PIC X(50)  VALUE SPACE.
008200       05 被保険者氏名Ｗ               PIC X(50)  VALUE SPACE.
008210       05 郵便番号Ｗ.
008220          07 郵便番号１Ｗ              PIC X(3)   VALUE SPACE.
008230          07 郵便番号２Ｗ              PIC X(4)   VALUE SPACE.
008240       05 被保険者住所Ｗ.
008250          07 被保険者住所１Ｗ          PIC X(50)  VALUE SPACE.
008260          07 被保険者住所２Ｗ          PIC X(50)  VALUE SPACE.
008990       05 電話番号Ｗ                   PIC X(35)  VALUE SPACE.
008270    03 患者情報Ｗ.
008280       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
008290       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
008300       05 性別チェックＷ.
008310          07 男チェックＷ              PIC N(1)  VALUE SPACE.
008320          07 女チェックＷ              PIC N(1)  VALUE SPACE.
008330       05 患者性別Ｗ.
008340          07 性別Ｗ                    PIC N(1)  VALUE SPACE.
008350       05 和暦チェックＷ.
008360          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
008370          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
008380          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
008390          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
008400          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
      */元号修正/↓↓↓20190426
008210          07 令和チェックＷ            PIC N(1)  VALUE SPACE.
                07 令和ＣＭＷ                PIC X(4)  VALUE SPACE.
009110*          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
      */元号修正/↑↑↑20190426
008410       05 患者年Ｗ                     PIC 9(2)  VALUE ZERO.
008420       05 患者月Ｗ                     PIC 9(2)  VALUE ZERO.
008430       05 患者日Ｗ                     PIC 9(2)  VALUE ZERO.
008440       05 続柄Ｗ.
008450          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
008460          07 FILLER                    PIC X(4)  VALUE SPACE.
008470*       05 本人チェックＷ               PIC N(1)  VALUE SPACE.
008480*       05 家族チェックＷ               PIC N(1)  VALUE SPACE.
008490*
008500*       05 負傷原因Ｗ                   PIC N(40) OCCURS 29 VALUE SPACE.
      */半角対応/110421
             05 負傷原因Ｗ OCCURS 29.
                07 負傷原因ＸＷ              PIC X(80)  VALUE SPACE.
008510*
008520    03 保険種別名称Ｗ                  PIC N(2)  VALUE SPACE.
008530    03 助成印Ｗ                        PIC N(1)  VALUE SPACE.
008540    03 特別コメントＷ                  PIC X(16) VALUE SPACE.
008550*    03 社保チェックＷ                  PIC N(1)  VALUE SPACE.
008560*    03 船員チェックＷ                  PIC N(1)  VALUE SPACE.
008570*    03 組合チェックＷ                  PIC N(1)  VALUE SPACE.
008580*    03 自衛チェックＷ                  PIC N(1)  VALUE SPACE.
008590*    03 共済チェックＷ                  PIC N(1)  VALUE SPACE.
008600*    03 国保チェックＷ                  PIC N(1)  VALUE SPACE.
008610*    03 退職チェックＷ                  PIC N(1)  VALUE SPACE.
008620*    03 老人チェックＷ                  PIC N(1)  VALUE SPACE.
008630*    03 高齢チェックＷ                  PIC N(1)  VALUE SPACE.
008640*    03 ３歳チェックＷ                  PIC N(1)  VALUE SPACE.
008650*    03 ４１老チェックＷ                PIC N(1)  VALUE SPACE.
008660*    03 障害チェックＷ                  PIC N(1)  VALUE SPACE.
008670*    03 被爆チェックＷ                  PIC N(1)  VALUE SPACE.
008680*    03 母子チェックＷ                  PIC N(1)  VALUE SPACE.
008690*    03 乳児チェックＷ                  PIC N(1)  VALUE SPACE.
      *    03 子チェックＷ                    PIC N(1)  VALUE SPACE.
      *    03 子Ｗ                            PIC N(1)  VALUE SPACE.
008700*
008710    03 １０割チェックＷ                PIC N(1)  VALUE SPACE.
008720    03 ９割チェックＷ                  PIC N(1)  VALUE SPACE.
008730    03 ８割チェックＷ                  PIC N(1)  VALUE SPACE.
008740    03 ７割チェックＷ                  PIC N(1)  VALUE SPACE.
008750    03 負担率Ｗ                        PIC 9(3)  VALUE ZERO.
008760*
008770****************
008780* 負傷データＦ *
008790****************
008800 01 負傷情報Ｗ.
008810    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
008820    03 部位情報Ｗ  OCCURS   9.
008830       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
008840       05 部位コードＷ.
008850          07 負傷種別Ｗ                PIC 9(2)  VALUE ZERO.
008860          07 部位Ｗ                    PIC 9(2)  VALUE ZERO.
008870          07 左右区分Ｗ                PIC 9(1)  VALUE ZERO.
008880          07 負傷位置番号Ｗ            PIC 9(2)  VALUE ZERO.
008890       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
008900       05 負傷年月日Ｗ.
008910          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
008920          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
008930          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
008940       05 初検年月日Ｗ.
008950          07 初検年Ｗ                  PIC 9(2)  VALUE ZERO.
008960          07 初検月Ｗ                  PIC 9(2)  VALUE ZERO.
008970          07 初検日Ｗ                  PIC 9(2)  VALUE ZERO.
008980       05 開始年月日Ｗ.
008990          07 開始年Ｗ                  PIC 9(2)  VALUE ZERO.
009000          07 開始月Ｗ                  PIC 9(2)  VALUE ZERO.
009010          07 開始日Ｗ                  PIC 9(2)  VALUE ZERO.
009020       05 終了年月日Ｗ.
009030          07 終了年Ｗ                  PIC 9(2)  VALUE ZERO.
009040          07 終了月Ｗ                  PIC 9(2)  VALUE ZERO.
009050          07 終了日Ｗ                  PIC 9(2)  VALUE ZERO.
009060       05 実日数Ｗ                     PIC 9(2)  VALUE ZERO.
009070       05 転帰区分Ｗ                   PIC 9(1)  VALUE ZERO.
009080       05 転帰区分チェックＷ.
009090          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
009100          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
009110          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
009120       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
009130       05 部位区切Ｗ                   PIC X(1)  VALUE SPACE.
009140       05 経過略称Ｗ.
009150          07 印刷経過略称Ｗ            PIC N(6)  VALUE SPACE.
009160          07 FILLER                    PIC X(2)  VALUE SPACE.
009170    03 経過部位Ｗ                      PIC N(1)  VALUE SPACE.
009180    03 新規チェックＷ                  PIC N(1)  VALUE SPACE.
009190    03 継続チェックＷ                  PIC N(1)  VALUE SPACE.
009200*
009210************
009220* 料金情報 *
009230************
009240 01 料金情報Ｗ.
009250    03 初検加算Ｗ.
009260       05 時間外チェックＷ                PIC N(1) VALUE SPACE.
009270       05 休日チェックＷ                  PIC N(1) VALUE SPACE.
009280       05 深夜チェックＷ                  PIC N(1) VALUE SPACE.
009290    03 往療加算Ｗ.
009300       05 夜間チェックＷ                  PIC N(1) VALUE SPACE.
009310       05 難路チェックＷ                  PIC N(1) VALUE SPACE.
009320       05 暴風雨雪チェックＷ              PIC N(1) VALUE SPACE.
009330    03 金属副子チェックＷ.
009340       05 大チェックＷ                    PIC N(1) VALUE SPACE.
009350       05 中チェックＷ                    PIC N(1) VALUE SPACE.
009360       05 小チェックＷ                    PIC N(1) VALUE SPACE.
009370    03 小計Ｗ                             PIC 9(7) VALUE ZERO.
009380    03 初回処置料合計Ｗ                   PIC 9(6) VALUE ZERO.
009390    03 初回処置料チェックＷ.
009400       05 整復料チェックＷ                PIC N(1) VALUE SPACE.
009410       05 固定料チェックＷ                PIC N(1) VALUE SPACE.
009420       05 施療料チェックＷ                PIC N(1) VALUE SPACE.
      */金属副子・運動後療の変更・追加/1805
          03 金属回数Ｗ                         PIC 9(2)  VALUE ZERO.
          03 運動料Ｗ                           PIC 9(4)  VALUE ZERO.
009430************
009440* 備考情報 *
009450************
009460 01 備考情報Ｗ.
009470    03 適用１Ｗ                        PIC N(38) VALUE SPACE.
009480    03 適用１ＷＲ REDEFINES 適用１Ｗ.
009490       05 適用１１Ｗ                   PIC N(19).
009500       05 適用１２Ｗ                   PIC N(19).
009510*
009520    03 適用２Ｗ                        PIC N(38) VALUE SPACE.
009530    03 適用２ＷＲ REDEFINES 適用２Ｗ.
009540       05 適用２１Ｗ                   PIC N(19).
009550       05 適用２２Ｗ                   PIC N(19).
009560*    03 適用３Ｗ                        PIC N(38) VALUE SPACE.
009570*    03 適用４Ｗ                        PIC N(38) VALUE SPACE.
009580*
009590    03 経過コメントＷ                  PIC N(60) VALUE SPACE.
009600*
003720*--- 負担給付割合用 ---*
003730 01 負担割合Ｗ                         PIC 9(2)  VALUE ZERO.
003740 01 給付割合Ｗ                         PIC 9(2)  VALUE ZERO.
009610*****************
009620* レセプト並び順 *
009630*****************
009640 01 順番固定Ｗ                         PIC X(10) VALUE SPACE.
009650 01 順番Ｗ                             PIC 9(4) VALUE ZERO.
009660*
       01 摘要施術日Ｗ                       PIC X(100) VALUE SPACE.
       01 施術日Ｗ.
          03 施術日２Ｗ                      PIC X(1)  VALUE SPACE.
          03 施術日１Ｗ                      PIC X(1)  VALUE SPACE.
009670*******************************************************************
009680 01 印刷制御.
009690     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
009700     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
009710     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
009720     03 拡張制御Ｐ.
009730         05 端末制御Ｐ.
009740             07 移動方向Ｐ             PIC X(1) VALUE SPACE.
009750             07 移動行数Ｐ             PIC 9(3) VALUE ZERO.
009760         05 詳細制御Ｐ                 PIC X(2) VALUE SPACE.
009770     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
009780     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
009790*
009800 01 計算機西暦年Ｗ                     PIC 9(2) VALUE ZERO.
009810* 日付ＷＯＲＫ
009820 01 和暦終了年Ｗ                       PIC 9(4) VALUE ZERO.
009830 01 計算機西暦.
009840    03 計算機西暦年                    PIC 9(4) VALUE ZERO.
009850    03 計算機西暦月日                  PIC 9(4) VALUE ZERO.
009860 01 計算機西暦Ｒ REDEFINES 計算機西暦.
009870    03 計算機世紀                      PIC 9(2).
009880    03 計算機日付                      PIC 9(6).
009890    03 計算機日付Ｒ REDEFINES 計算機日付.
009900       05 計算機年月                   PIC 9(4).
009910       05 計算機年月Ｒ REDEFINES 計算機年月.
009920         07 計算機年                   PIC 9(2).
009930         07 計算機月                   PIC 9(2).
009940       05 計算機日                     PIC 9(2).
009950*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
009960******************************************************************
009970*                          連結項目                              *
009980******************************************************************
009990**  画面入力データ
010000 01 連入－入力データ委任印刷 IS EXTERNAL.
010010    03 連入－委任印刷                     PIC 9.
       01 連入－入力データ電話印刷 IS EXTERNAL.
          03 連入－電話印刷                     PIC 9.
009190*
       01 連入－プレビュー IS EXTERNAL.
          03 連入－プレビュー区分          PIC 9.
010020*
010030** ３カ月長期判定
010040 01 連期間－キー IS EXTERNAL.
010050    03 連期間－施術年月.
010060       05 連期間－施術和暦               PIC 9.
010070       05 連期間－施術年                 PIC 9(2).
010080       05 連期間－施術月                 PIC 9(2).
010090    03  連期間－患者コード.
010100       05 連期間－患者番号               PIC 9(6).
010110       05 連期間－枝番                   PIC X.
010120    03 連期間－対象フラグ                PIC X(3).
010130    03 連期間－期間月Ｗ.
010140       05 連期間－期間Ｗ                 PIC 9(2) OCCURS 9.
010150************
010160* 印刷キー *
010170************
010190*
010200 01 連レ印－対象データ IS EXTERNAL.
010210    03 連レ印－施術年月日.
010220       05 連レ印－施術和暦                  PIC 9(1).
010230       05 連レ印－施術年                    PIC 9(2).
010240       05 連レ印－施術月                    PIC 9(2).
010250    03 連レ印－患者コード.
010260       05 連レ印－患者番号                  PIC 9(6).
010270       05 連レ印－枝番                      PIC X(1).
010280    03 連レ印－保険種別                     PIC 9(2).
010290    03 連レ印－保険者番号                   PIC X(10).
010300    03 連レ印－公費種別                     PIC 9(2).
010310    03 連レ印－費用負担者番号               PIC X(10).
010320    03 連レ印－助成種別                     PIC 9(2).
010330    03 連レ印－費用負担者番号助成           PIC X(10).
010340    03 連レ印－患者カナ                     PIC X(20).
010350    03 連レ印－本人家族区分                 PIC 9(1).
013780*
013790 01 連レ－キー IS EXTERNAL.
013800    03 連レ－保険種別                  PIC 9(2).
013810*
013820************************
013830* 長期理由文セット     *
013840************************
013850 01 連長文－キー IS EXTERNAL.
013860    03 連長文－施術年月.
013870       05 連長文－施術和暦               PIC 9.
013880       05 連長文－施術年                 PIC 9(2).
013890       05 連長文－施術月                 PIC 9(2).
013900    03  連長文－患者コード.
013910       05 連長文－患者番号               PIC 9(6).
013920       05 連長文－枝番                   PIC X.
013930    03 連長文－文桁数                    PIC 9(2).
013940    03 連長文－理由文                    PIC N(63) OCCURS 15.
013950*
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
013960************************
013970* 助成レセまとめ
013980************************
013990 01 連レセまとめ－キー IS EXTERNAL.
014000    03 連レセまとめ－施術和暦年月.
014010       05 連レセまとめ－施術和暦               PIC 9.
014020       05 連レセまとめ－施術年月.
014030          07 連レセまとめ－施術年              PIC 9(2).
014040          07 連レセまとめ－施術月              PIC 9(2).
014050    03 連レセまとめ－患者コード.
014060       05 連レセまとめ－患者番号               PIC 9(6).
014070       05 連レセまとめ－枝番                   PIC X(1).
014080**-------------------------------------------------------**
014090*   1:助成レセプトなしの本体まとめの判定
014100*   2:横浜・川崎用の社保助成レセかの判定
014110    03 連レセまとめ－判定区分                  PIC 9.
014120**-------------------------------------------------------**
014130*  / OUT /　 0:対象外、1:対象
014140    03 連レセまとめ－判定結果                  PIC 9.
014150**
014160*
014170*************
014180* 助成名称
014190*************
014200 01 連助成名称－キー IS EXTERNAL.
014210    03 連助成名称－助成種別             PIC 9(2).
014220    03 連助成名称－費用負担者番号助成   PIC X(10).
014230*   / OUT /
014240    03 連助成名称－名称集団.
014250       05 連助成名称－１文字            PIC N.
014260       05 連助成名称－略称              PIC N(4).
014270       05 連助成名称－正式名称          PIC N(10).
014280*
014290* 負担率取得用14/10～
014300 01 連率－負担率取得キー IS EXTERNAL.
014310    03 連率－施術和暦年月.
014320       05 連率－施術和暦               PIC 9.
014330       05 連率－施術年月.
014340          07 連率－施術年              PIC 9(2).
014350          07 連率－施術月              PIC 9(2).
014360    03 連率－患者コード.
014370       05 連率－患者番号               PIC 9(6).
014380       05 連率－枝番                   PIC X.
014390    03 連率－実際負担率                PIC 9(3).
014400    03 連率－実際本体負担率            PIC 9(3).
014410    03 連率－健保負担率                PIC 9(3).
014420    03 連率－２７老負担率              PIC 9(3).
014430    03 連率－助成負担率                PIC 9(3).
014440    03 連率－特別用負担率              PIC 9(3).
014450*
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
014460******************************************************************
014470*                      PROCEDURE  DIVISION                       *
014480******************************************************************
014490 PROCEDURE               DIVISION.
014500************
014510*           *
014520* 初期処理   *
014530*           *
014540************
002570     PERFORM プリンタファイル作成.
014550     PERFORM 初期化.
014560     PERFORM 制御情報取得.
014570************
014580*           *
014590* 主処理     *
014600*           *
014610************
014620* 印刷
014630     PERFORM 連結項目待避.
014640     PERFORM 印刷セット.
014650     PERFORM 印刷処理.
014660************
014670*           *
014680* 終了処理   *
014690*           *
014700************
014710     PERFORM 受診者印刷区分更新.
014720     PERFORM 終了処理.
014730*     PERFORM 遅延処理.
014740     MOVE ZERO  TO PROGRAM-STATUS.
014750     EXIT PROGRAM.
014760*
014770*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YJK6125"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分  TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
014780*================================================================*
014790 初期化 SECTION.
014800*
014810     PERFORM ファイルオープン.
014820*    /* 現在日付取得 */
014830     ACCEPT 計算機日付 FROM DATE.
014840*    /* 1980～2079年の間で設定 */
014850     IF ( 計算機年 > 80 )
014860         MOVE 19 TO 計算機世紀
014870     ELSE
014880         MOVE 20 TO 計算機世紀
014890     END-IF.
014900     PERFORM カレント元号取得.
014910     PERFORM 和暦終了年取得.
014920     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
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
015360 ファイルオープン SECTION.
015370*
015380     OPEN INPUT   保険者マスタ
015390         MOVE NC"保険者" TO ファイル名.
015400         PERFORM オープンチェック.
015410     OPEN INPUT   元号マスタ
015420         MOVE NC"元号" TO ファイル名.
015430         PERFORM オープンチェック.
015440     OPEN INPUT   名称マスタ
015450         MOVE NC"名称" TO ファイル名.
015460         PERFORM オープンチェック.
015500     OPEN INPUT   制御情報マスタ
015510         MOVE NC"制御情報" TO ファイル名.
015520         PERFORM オープンチェック.
015530     OPEN INPUT   施術所情報マスタ
015540         MOVE NC"施情" TO ファイル名.
015550         PERFORM オープンチェック.
015590     OPEN INPUT   経過マスタ
015600         MOVE NC"経過" TO ファイル名.
015610         PERFORM オープンチェック.
015620     OPEN INPUT   施術記録Ｆ.
015630         MOVE NC"施記Ｆ" TO ファイル名.
015640         PERFORM オープンチェック.
015650     OPEN INPUT   負傷データＦ.
015660         MOVE NC"負傷" TO ファイル名.
015670         PERFORM オープンチェック.
015680     OPEN INPUT   負傷原因Ｆ.
015690         MOVE NC"負傷原因" TO ファイル名.
015700         PERFORM オープンチェック.
015710     OPEN INPUT   ＩＤ管理マスタ
015720         MOVE NC"ＩＤ" TO ファイル名.
015730         PERFORM オープンチェック.
015740     OPEN INPUT 市町村マスタ.
015750         MOVE NC"市町村" TO ファイル名.
015760         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
015800     OPEN INPUT  作業ファイル４.
015810         MOVE NC"作４" TO ファイル名.
015820         PERFORM オープンチェック.
015830     OPEN I-O   受診者情報Ｆ.
015840         MOVE NC"受情" TO ファイル名.
015850         PERFORM オープンチェック.
015860     OPEN I-O   印刷ファイル
015870         PERFORM エラー処理Ｐ.
015880*================================================================*
015890 オープンチェック SECTION.
015900*
015910     IF ( 状態キー  NOT =  "00" )
015920         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
015930         DISPLAY NC"状態キー：" 状態キー         UPON CONS
015940         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
015950                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015960         ACCEPT  キー入力 FROM CONS
015970         PERFORM ファイル閉鎖
015980         EXIT PROGRAM.
015990*================================================================*
016000 制御情報取得 SECTION.
016010*
016020     MOVE ZERO TO 制－制御区分
016030     READ 制御情報マスタ
016040     NOT INVALID KEY
016050         MOVE 制－最大登録部位数 TO 最大登録数Ｗ
016060         MOVE 制－負傷連続登録   TO 負傷連続登録Ｗ
016070         MOVE 制－遅延回数       TO 遅延回数Ｗ
016080     END-READ.
016090*
016100*================================================================*
016110 遅延処理 SECTION.
016120*
016130     PERFORM VARYING 遅延ＣＮＴ FROM 1 BY 1
016140                                UNTIL 遅延ＣＮＴ > 遅延回数Ｗ
016150         MOVE SPACE TO 遅延フラグ
016160     END-PERFORM.
016170*
016180*================================================================*
016190 連結項目待避 SECTION.
016200*
016210     MOVE 連レ印－施術和暦           TO 施術和暦ＷＲ.
016220     MOVE 連レ印－施術年             TO 施術年ＷＲ.
016230     MOVE 連レ印－施術月             TO 施術月ＷＲ.
016240     MOVE 連レ印－保険種別           TO 保険種別ＷＲ.
016250     MOVE 連レ印－保険者番号         TO 保険者番号ＷＲ.
016260     MOVE 連レ印－公費種別           TO 公費種別ＷＲ.
016270     MOVE 連レ印－費用負担者番号     TO 費用負担者番号ＷＲ.
016280     MOVE 連レ印－助成種別           TO 助成種別ＷＲ.
016290     MOVE 連レ印－費用負担者番号助成 TO 費用負担者番号助成ＷＲ.
016300     MOVE 連レ印－本人家族区分       TO 本人家族区分ＷＲ.
016310     MOVE 連レ印－患者カナ           TO 患者カナＷＲ.
016320     MOVE 連レ印－患者番号           TO 患者番号ＷＲ.
016330     MOVE 連レ印－枝番               TO 枝番ＷＲ.
016340*================================================================*
016350 印刷セット SECTION.
016360*
016370     PERFORM 項目初期化.
           PERFORM 基本情報取得.
016380     PERFORM 施術所情報取得.
016390     PERFORM 請求先情報取得.
016400     PERFORM 受診者情報取得.
016410     PERFORM 負傷データ取得.
016420     PERFORM 料金情報取得.
016430     PERFORM 施術記録取得.
016440     PERFORM レセプト並び順取得.
016460***     PERFORM 初検日以前のデータ判定.
016470     PERFORM 初検加算時刻取得.
016480*     PERFORM 助成印取得.
016490     PERFORM 委任年月日取得.
           PERFORM 施術日取得.
016500*
016791*-----------------------------------------------*
016800     IF ( 負傷原因印刷区分Ｗ  NOT = 1 ) AND ( レセ負傷原因印刷区分Ｗ NOT = 1 )
016813        IF ( 負傷原因印刷区分Ｗ = 3 OR 4 )
016815           PERFORM 負傷原因印刷対象判定処理
016817        ELSE
016820           PERFORM 負傷原因取得
016821        END-IF
016830     END-IF.
016831*-----------------------------------------------*
016550*
015930* 長期対象の時のみ
015940     IF ( 長期理由印刷区分Ｗ NOT = 1 )
               MOVE 長期理由印刷区分Ｗ TO 連摘文－長期区分
016000     END-IF.
016620*
016630********************
016640* 受診者情報セット *
016650********************
      */元号修正/↓↓↓20190426
037370     IF 施術和暦Ｗ > 4
              MOVE 施術和暦Ｗ         TO 元－元号区分
037380        READ 元号マスタ
037390        NOT INVALID KEY
037400            MOVE 元－元号名称   TO 施術和暦
037410        END-READ
              MOVE "===="             TO 施術和暦訂正
           END-IF.
      */元号修正/↑↑↑20190426
016660     MOVE 施術年Ｗ            TO 施術年.
016670     MOVE 施術月Ｗ            TO 施術月.
016680*
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
016690*     MOVE 社保チェックＷ      TO 社保チェック.
016700*     MOVE 船員チェックＷ      TO 船員チェック.
016710*     MOVE 組合チェックＷ      TO 組合チェック.
016720*     MOVE 自衛チェックＷ      TO 自衛チェック.
016730*     MOVE 共済チェックＷ      TO 共済チェック.
016740*     MOVE 国保チェックＷ      TO 国保チェック.
016750*     MOVE 退職チェックＷ      TO 退職チェック.
016760*     MOVE 老人チェックＷ      TO 老人チェック.
016770*     MOVE 本人チェックＷ      TO 本人チェック.
016780*     MOVE 家族チェックＷ      TO 家族チェック.
016790**
016800*     MOVE 高齢チェックＷ      TO 高齢チェック.
016810*     MOVE ３歳チェックＷ      TO ３歳チェック.
016820*     MOVE ４１老チェックＷ    TO ４１老チェック.
016830*     MOVE 障害チェックＷ      TO 障害チェック.
016840*     MOVE 被爆チェックＷ      TO 被爆チェック.
016850*     MOVE 母子チェックＷ      TO 母子チェック.
016860*     MOVE 乳児チェックＷ      TO 乳児チェック.
      *     MOVE 子チェックＷ        TO 子チェック１.
      *     MOVE 子Ｗ                TO 子１.
016870*
016880*     IF ( 記号Ｗ(1:1) = NC"＊" )
016890*        MOVE  SPACE    TO  記号
016900*     ELSE
016910*        MOVE 記号Ｗ    TO  記号
016920*     END-IF.
016930*     IF ( 番号Ｗ(1:1) = "*"  ) OR
016940*        ( 番号Ｗ(1:2) = "＊" )
016950*        MOVE SPACE     TO  番号
016960*     ELSE
016970*        MOVE 番号Ｗ    TO  番号
016980*     END-IF.
016990*     IF (記号 NOT = SPACE) OR (番号 NOT = SPACE)
017000*         MOVE "/"   TO  区切り
017010*     END-IF.
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
017020*
017030*     IF ( 印刷市町村番号Ｗ(1:2) = "99" )
017040*         MOVE SPACE              TO 市町村番号
017050*     ELSE
017060*         MOVE 印刷市町村番号Ｗ   TO 市町村番号
017070*     END-IF.
017080*
017090*     IF ( 印刷受給者番号Ｗ(1:1) = "*"  ) OR
017100*        ( 印刷受給者番号Ｗ(1:2) = "＊" )
017110*        MOVE  SPACE              TO 受給者番号
017120*     ELSE
017130*        MOVE 受給者番号Ｗ        TO 受給者番号
017140*     END-IF.
017150     MOVE 印刷保険者番号Ｗ    TO 保険者番号.
017160     MOVE "["                TO 括弧３.
017170     MOVE "]"                TO 括弧４.
017180     MOVE 保険者番号ＷＴ      TO 保険者番号タイトル.
017190*     IF ( 請求先名称２Ｗ = SPACE )
017200*        MOVE 請求先名称Ｗ     TO 保険者名称
017210*     ELSE
017220*        MOVE 請求先名称１Ｗ   TO 保険者名称１.
017230*        MOVE 請求先名称２Ｗ   TO 保険者名称２.
              MOVE 請求先名称１ＷＴ TO 保険者名称１.
              MOVE 請求先名称２ＷＴ TO 保険者名称２.
017240*     END-IF.
017250***     MOVE 被保険者カナＷ      TO 被保険者カナ.
017280     MOVE 被保険者氏名Ｗ      TO 被保険者氏名
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
017340     MOVE 被保険者住所１Ｗ    TO 住所１.
017350     MOVE 被保険者住所２Ｗ    TO 住所２.
017280*     MOVE 被保険者氏名Ｗ      TO 受診者名.
017280     MOVE 被保険者氏名Ｗ      TO 被保険者氏名２.
017360*     MOVE 患者カナＷ          TO 患者カナ.
017370     MOVE 患者氏名Ｗ          TO 患者氏名.
017380     MOVE 男チェックＷ        TO 男チェック.
017390     MOVE 女チェックＷ        TO 女チェック.
017400***     MOVE 性別Ｗ               TO 性別.
017410     MOVE 明治チェックＷ      TO 明治チェック.
017420     MOVE 大正チェックＷ      TO 大正チェック.
017430     MOVE 昭和チェックＷ      TO 昭和チェック.
017440     MOVE 平成チェックＷ      TO 平成チェック.
017450***     MOVE 元号Ｗ              TO 元号.
      */元号修正↓↓↓/20190426
           MOVE 令和ＣＭＷ         TO 令和ＣＭ.
023070     MOVE 令和チェックＷ     TO 令和チェック.
017390*     MOVE 元号Ｗ              TO 患者和暦.
      */元号修正↑↑↑/20190426
017460     MOVE 患者年Ｗ            TO 患者年.
017470     MOVE 患者月Ｗ            TO 患者月.
017480     MOVE 患者日Ｗ            TO 患者日.
017490     MOVE 印刷続柄Ｗ          TO 続柄.
017500*     MOVE NC"業務災害通勤災害又は第三者行為以外の原因による。" TO 負傷原因.
017510*     IF 負傷原因Ｗ(1) NOT = SPACE
017520*         MOVE NC"（負傷原因）"  TO 負傷原因固定
017530*     END-IF.
      *
017540     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
017550     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
017560     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
017570     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
017580     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
017580     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
      *
017590*
017600***     MOVE 助成印Ｗ            TO 助成印.
017610     MOVE 保険種別名称Ｗ      TO 保険種別.
017620*
017770********************
017780* 負傷データセット *
017790********************
017800* １部位 *
017810**********
017820     MOVE 負傷名Ｗ(1)       TO 負傷名１.
017830     MOVE 負傷年Ｗ(1)       TO 負傷年１.
017840     MOVE 負傷月Ｗ(1)       TO 負傷月１.
017850     MOVE 負傷日Ｗ(1)       TO 負傷日１.
017860     MOVE 初検年Ｗ(1)       TO 初検年１.
017870     MOVE 初検月Ｗ(1)       TO 初検月１.
017880     MOVE 初検日Ｗ(1)       TO 初検日１.
017890     MOVE 開始年Ｗ(1)       TO 開始年１.
017900     MOVE 開始月Ｗ(1)       TO 開始月１.
017910     MOVE 開始日Ｗ(1)       TO 開始日１.
017920     MOVE 終了年Ｗ(1)       TO 終了年１.
017930     MOVE 終了月Ｗ(1)       TO 終了月１.
017940     MOVE 終了日Ｗ(1)       TO 終了日１.
017950     MOVE 実日数Ｗ(1)       TO 実日数１.
017960*     IF 実日数Ｗ(1) NOT = ZERO
017970*         MOVE NC"日"        TO 日１
017980*     END-IF.
017990     MOVE 治癒チェックＷ(1) TO 治癒チェック１.
018000     MOVE 中止チェックＷ(1) TO 中止チェック１.
018010     MOVE 転医チェックＷ(1) TO 転医チェック１.
018020**********
018030* ２部位 *
018040**********
018050     MOVE 負傷名Ｗ(2)       TO 負傷名２.
018060     MOVE 負傷年Ｗ(2)       TO 負傷年２.
018070     MOVE 負傷月Ｗ(2)       TO 負傷月２.
018080     MOVE 負傷日Ｗ(2)       TO 負傷日２.
018090     MOVE 初検年Ｗ(2)       TO 初検年２.
018100     MOVE 初検月Ｗ(2)       TO 初検月２.
018110     MOVE 初検日Ｗ(2)       TO 初検日２.
018120     MOVE 開始年Ｗ(2)       TO 開始年２.
018130     MOVE 開始月Ｗ(2)       TO 開始月２.
018140     MOVE 開始日Ｗ(2)       TO 開始日２.
018150     MOVE 終了年Ｗ(2)       TO 終了年２.
018160     MOVE 終了月Ｗ(2)       TO 終了月２.
018170     MOVE 終了日Ｗ(2)       TO 終了日２.
018180     MOVE 実日数Ｗ(2)       TO 実日数２.
018190*     IF 実日数Ｗ(2) NOT = ZERO
018200*         MOVE NC"日"        TO 日２
018210*     END-IF.
018220     MOVE 治癒チェックＷ(2) TO 治癒チェック２.
018230     MOVE 中止チェックＷ(2) TO 中止チェック２.
018240     MOVE 転医チェックＷ(2) TO 転医チェック２.
018250**********
018260* ３部位 *
018270**********
018280     MOVE 負傷名Ｗ(3)       TO 負傷名３.
018290     MOVE 負傷年Ｗ(3)       TO 負傷年３.
018300     MOVE 負傷月Ｗ(3)       TO 負傷月３.
018310     MOVE 負傷日Ｗ(3)       TO 負傷日３.
018320     MOVE 初検年Ｗ(3)       TO 初検年３.
018330     MOVE 初検月Ｗ(3)       TO 初検月３.
018340     MOVE 初検日Ｗ(3)       TO 初検日３.
018350     MOVE 開始年Ｗ(3)       TO 開始年３.
018360     MOVE 開始月Ｗ(3)       TO 開始月３.
018370     MOVE 開始日Ｗ(3)       TO 開始日３.
018380     MOVE 終了年Ｗ(3)       TO 終了年３.
018390     MOVE 終了月Ｗ(3)       TO 終了月３.
018400     MOVE 終了日Ｗ(3)       TO 終了日３.
018410     MOVE 実日数Ｗ(3)       TO 実日数３.
018420*     IF 実日数Ｗ(3) NOT = ZERO
018430*         MOVE NC"日"        TO 日３
018440*     END-IF.
018450     MOVE 治癒チェックＷ(3) TO 治癒チェック３.
018460     MOVE 中止チェックＷ(3) TO 中止チェック３.
018470     MOVE 転医チェックＷ(3) TO 転医チェック３.
018480**********
018490* ４部位 *
018500**********
018510     MOVE 負傷名Ｗ(4)       TO 負傷名４.
018520     MOVE 負傷年Ｗ(4)       TO 負傷年４.
018530     MOVE 負傷月Ｗ(4)       TO 負傷月４.
018540     MOVE 負傷日Ｗ(4)       TO 負傷日４.
018550     MOVE 初検年Ｗ(4)       TO 初検年４.
018560     MOVE 初検月Ｗ(4)       TO 初検月４.
018570     MOVE 初検日Ｗ(4)       TO 初検日４.
018580     MOVE 開始年Ｗ(4)       TO 開始年４.
018590     MOVE 開始月Ｗ(4)       TO 開始月４.
018600     MOVE 開始日Ｗ(4)       TO 開始日４.
018610     MOVE 終了年Ｗ(4)       TO 終了年４.
018620     MOVE 終了月Ｗ(4)       TO 終了月４.
018630     MOVE 終了日Ｗ(4)       TO 終了日４.
018640     MOVE 実日数Ｗ(4)       TO 実日数４.
018650*     IF 実日数Ｗ(4) NOT = ZERO
018660*         MOVE NC"日"        TO 日４
018670*     END-IF.
018680     MOVE 治癒チェックＷ(4) TO 治癒チェック４.
018690     MOVE 中止チェックＷ(4) TO 中止チェック４.
018700     MOVE 転医チェックＷ(4) TO 転医チェック４.
018710**********
018720* ５部位 *
018730**********
018740     MOVE 負傷名Ｗ(5)       TO 負傷名５.
018750     MOVE 負傷年Ｗ(5)       TO 負傷年５.
018760     MOVE 負傷月Ｗ(5)       TO 負傷月５.
018770     MOVE 負傷日Ｗ(5)       TO 負傷日５.
018780     MOVE 初検年Ｗ(5)       TO 初検年５.
018790     MOVE 初検月Ｗ(5)       TO 初検月５.
018800     MOVE 初検日Ｗ(5)       TO 初検日５.
018810     MOVE 開始年Ｗ(5)       TO 開始年５.
018820     MOVE 開始月Ｗ(5)       TO 開始月５.
018830     MOVE 開始日Ｗ(5)       TO 開始日５.
018840     MOVE 終了年Ｗ(5)       TO 終了年５.
018850     MOVE 終了月Ｗ(5)       TO 終了月５.
018860     MOVE 終了日Ｗ(5)       TO 終了日５.
018870     MOVE 実日数Ｗ(5)       TO 実日数５.
018880*     IF 実日数Ｗ(5) NOT = ZERO
018890*         MOVE NC"日"        TO 日５
018900*     END-IF.
018910     MOVE 治癒チェックＷ(5) TO 治癒チェック５.
018920     MOVE 中止チェックＷ(5) TO 中止チェック５.
018930     MOVE 転医チェックＷ(5) TO 転医チェック５.
018940**************
018950* 経過セット *
018960**************
018970     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL ( 部位ＣＮＴ > 5 )
018980***             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
018990***         MOVE 部位ＣＮＴＷ(部位ＣＮＴ)   TO 経過部位ＣＮＴ(部位ＣＮＴ)
019000***         MOVE 部位区切Ｗ(部位ＣＮＴ)     TO 部位区切(部位ＣＮＴ)
019010         MOVE 印刷経過略称Ｗ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
019020     END-PERFORM.
019030*****************************************
019040*     新規・継続チェックについて        *
019050*   ●新規...初検有り ●継続...初検なし *
019060*****************************************
019070     MOVE 新規チェックＷ    TO 新規チェック.
019080     MOVE 継続チェックＷ    TO 継続チェック.
019090********************
019100* 料金データセット *
019110********************
019120*    ****************************************************************
019130*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
019140*    ****************************************************************
019150     MOVE 初検料ＷＲ                   TO  初検料.
           MOVE 初検時相談料ＷＲ             TO  初検時相談料.
019160     MOVE 時間外チェックＷ             TO  時間外チェック.
019170     MOVE 休日チェックＷ               TO  休日チェック.
019180     MOVE 深夜チェックＷ               TO  深夜チェック.
019190     MOVE 初検加算料ＷＲ               TO  初検加算料.
019110     IF ( 初検加算時ＷＴ(1) NOT = ZERO ) OR
019120        ( 初検加算分ＷＴ(1) NOT = ZERO )
019130        MOVE 初検加算時ＷＴ(1)         TO  初検加算時
019140        MOVE 初検加算分ＷＴ(1)         TO  初検加算分
              MOVE "施術時間"                TO 初検加算ＣＭ
              MOVE ":"                       TO 初検加算区切
019150     END-IF.
019200     MOVE 再検料ＷＲ                   TO  再検料.
019210     MOVE 往療距離ＷＲ                 TO  往療距離.
019220     MOVE 往療回数ＷＲ                 TO  往療回数.
019230     MOVE 往療料ＷＲ                   TO  往療料.
019240     MOVE 夜間チェックＷ               TO  夜間チェック.
019250     MOVE 難路チェックＷ               TO  難路チェック.
019260     MOVE 暴風雨雪チェックＷ           TO  暴風雨雪チェック.
019270     MOVE 往療加算料ＷＲ               TO  往療加算料.
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
019310     MOVE 金属副子加算料ＷＲ           TO  金属副子加算料.
019320     MOVE 施術情報提供料ＷＲ           TO  施術情報提供料.
019330     MOVE 小計Ｗ                       TO 小計.
019340********************
019350* 初回処置料セット *
019360********************
019370     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL ( 部位ＣＮＴ > 5 )
019380***             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
019390         MOVE 初回処置料ＷＲ(部位ＣＮＴ) TO 初回処置料(部位ＣＮＴ)
019400     END-PERFORM.
019410     MOVE 初回処置料合計Ｗ         TO 初回処置料合計
019420*
019430     MOVE 施療料チェックＷ            TO 施療料チェック.
019440     MOVE 整復料チェックＷ            TO 整復料チェック.
019450     MOVE 固定料チェックＷ            TO 固定料チェック.
019460********************
019470* 逓減毎料金セット *
019480********************
019490*    **********
019500*    * １部位 *
019510*    **********
019520     MOVE 後療単価１ＷＲ             TO 後療単価１.
019530     MOVE 後療回数１ＷＲ             TO 後療回数１.
019540     MOVE 後療料１ＷＲ               TO 後療料１.
019550     MOVE 冷罨法回数１ＷＲ           TO 冷罨法回数１.
019560     MOVE 冷罨法料１ＷＲ             TO 冷罨法料１.
019570     MOVE 温罨法回数１ＷＲ           TO 温罨法回数１.
019580     MOVE 温罨法料１ＷＲ             TO 温罨法料１.
019590     MOVE 電療回数１ＷＲ             TO 電療回数１.
019600     MOVE 電療料１ＷＲ               TO 電療料１.
019610     MOVE 小計１ＷＲ                 TO 小計１.
019620     IF ( 長期逓減率１ＷＲ NOT = ZERO )
019630         COMPUTE 長期逓減率１ = 長期逓減率１ＷＲ / 100
019640     END-IF.
019650     MOVE 長期込小計１ＷＲ           TO 長期込小計１.
019660*    **********
019670*    * ２部位 *
019680*    **********
019690     MOVE 後療単価２ＷＲ             TO 後療単価２.
019700     MOVE 後療回数２ＷＲ             TO 後療回数２.
019710     MOVE 後療料２ＷＲ               TO 後療料２.
019720     MOVE 冷罨法回数２ＷＲ           TO 冷罨法回数２.
019730     MOVE 冷罨法料２ＷＲ             TO 冷罨法料２.
019740     MOVE 温罨法回数２ＷＲ           TO 温罨法回数２.
019750     MOVE 温罨法料２ＷＲ             TO 温罨法料２.
019760     MOVE 電療回数２ＷＲ             TO 電療回数２.
019770     MOVE 電療料２ＷＲ               TO 電療料２.
019780     MOVE 小計２ＷＲ                 TO 小計２.
019790     IF ( 長期逓減率２ＷＲ NOT = ZERO )
019800         COMPUTE 長期逓減率２ = 長期逓減率２ＷＲ / 100
019810     END-IF.
019820     MOVE 長期込小計２ＷＲ           TO 長期込小計２.
019830*    ****************
019840*    * ３部位／８割 *
019850*    ****************
019860     MOVE 後療単価３８ＷＲ             TO 後療単価３８.
019870     MOVE 後療回数３８ＷＲ             TO 後療回数３８.
019880     MOVE 後療料３８ＷＲ               TO 後療料３８.
019890     MOVE 冷罨法回数３８ＷＲ           TO 冷罨法回数３８.
019900     MOVE 冷罨法料３８ＷＲ             TO 冷罨法料３８.
019910     MOVE 温罨法回数３８ＷＲ           TO 温罨法回数３８.
019920     MOVE 温罨法料３８ＷＲ             TO 温罨法料３８.
019930     MOVE 電療回数３８ＷＲ             TO 電療回数３８.
019940     MOVE 電療料３８ＷＲ               TO 電療料３８.
019950     MOVE 小計３８ＷＲ                 TO 小計３８.
019960     MOVE 多部位込小計３８ＷＲ         TO 多部位込小計３８.
019970     IF ( 長期逓減率３８ＷＲ NOT = ZERO )
019980         COMPUTE 長期逓減率３８ = 長期逓減率３８ＷＲ / 100
019990     END-IF.
020000     MOVE 長期込小計３８ＷＲ           TO 長期込小計３８.
      */ 逓減率 0.7→0.6 /42505
           IF (施術和暦年月ＷＲ >= 42505)
              MOVE "60"                      TO 逓減３８
              MOVE "0.6"                     TO 多部位３８
              MOVE "==="                     TO 逓減訂正３８ 多部位訂正３８
           END-IF.
020010*    ****************
020020*    * ３部位／10割 *
020030*    ****************
020040     MOVE 逓減開始月３０ＷＲ           TO 逓減開始月３０.
020050     MOVE 逓減開始日３０ＷＲ           TO 逓減開始日３０.
020060     MOVE 後療単価３０ＷＲ             TO 後療単価３０.
020070     MOVE 後療回数３０ＷＲ             TO 後療回数３０.
020080     MOVE 後療料３０ＷＲ               TO 後療料３０.
020090     MOVE 冷罨法回数３０ＷＲ           TO 冷罨法回数３０.
020100     MOVE 冷罨法料３０ＷＲ             TO 冷罨法料３０.
020110     MOVE 温罨法回数３０ＷＲ           TO 温罨法回数３０.
020120     MOVE 温罨法料３０ＷＲ             TO 温罨法料３０.
020130     MOVE 電療回数３０ＷＲ             TO 電療回数３０.
020140     MOVE 電療料３０ＷＲ               TO 電療料３０.
020150     MOVE 小計３０ＷＲ                 TO 小計３０.
020160     IF ( 長期逓減率３０ＷＲ NOT = ZERO )
020170         COMPUTE 長期逓減率３０ = 長期逓減率３０ＷＲ / 100
020180     END-IF.
020190     MOVE 長期込小計３０ＷＲ           TO 長期込小計３０.
020200*    ****************
020210*    * ４部位／５割 *
020220*    ****************
020230*     MOVE 後療単価４５ＷＲ             TO 後療単価４５.
020240*     MOVE 後療回数４５ＷＲ             TO 後療回数４５.
020250*     MOVE 後療料４５ＷＲ               TO 後療料４５.
020260*     MOVE 冷罨法回数４５ＷＲ           TO 冷罨法回数４５.
020270*     MOVE 冷罨法料４５ＷＲ             TO 冷罨法料４５.
020280*     MOVE 温罨法回数４５ＷＲ           TO 温罨法回数４５.
020290*     MOVE 温罨法料４５ＷＲ             TO 温罨法料４５.
020300*     MOVE 電療回数４５ＷＲ             TO 電療回数４５.
020310*     MOVE 電療料４５ＷＲ               TO 電療料４５.
020320*     MOVE 小計４５ＷＲ                 TO 小計４５.
020330*     MOVE 多部位込小計４５ＷＲ         TO 多部位込小計４５.
020340*     IF ( 長期逓減率４５ＷＲ NOT = ZERO )
020350*         COMPUTE 長期逓減率４５ = 長期逓減率４５ＷＲ / 100
020360*     END-IF.
020370*     MOVE 長期込小計４５ＷＲ           TO 長期込小計４５.
020380*    ****************
020390*    * ４部位／８割 *
020400*    ****************
020410     MOVE 逓減開始月４８ＷＲ           TO 逓減開始月４８.
020420     MOVE 逓減開始日４８ＷＲ           TO 逓減開始日４８.
020430     MOVE 後療単価４８ＷＲ             TO 後療単価４８.
020440     MOVE 後療回数４８ＷＲ             TO 後療回数４８.
020450     MOVE 後療料４８ＷＲ               TO 後療料４８.
020460     MOVE 冷罨法回数４８ＷＲ           TO 冷罨法回数４８.
020470     MOVE 冷罨法料４８ＷＲ             TO 冷罨法料４８.
020480     MOVE 温罨法回数４８ＷＲ           TO 温罨法回数４８.
020490     MOVE 温罨法料４８ＷＲ             TO 温罨法料４８.
020500     MOVE 電療回数４８ＷＲ             TO 電療回数４８.
020510     MOVE 電療料４８ＷＲ               TO 電療料４８.
020520     MOVE 小計４８ＷＲ                 TO 小計４８.
020530     MOVE 多部位込小計４８ＷＲ         TO 多部位込小計４８.
020540     IF ( 長期逓減率４８ＷＲ NOT = ZERO )
020550         COMPUTE 長期逓減率４８ = 長期逓減率４８ＷＲ / 100
020560     END-IF.
020570     MOVE 長期込小計４８ＷＲ           TO 長期込小計４８.
      */ 逓減率 0.7→0.6 /42505
           IF (施術和暦年月ＷＲ >= 42505)
              MOVE "60"                      TO 逓減４８
              MOVE "0.6"                     TO 多部位４８
              MOVE "==="                     TO 逓減訂正４８ 多部位訂正４８
           END-IF.
020580*    ****************
020590*    * ４部位／10割 *
020600*    ****************
020610     MOVE 逓減開始月４０ＷＲ           TO 逓減開始月４０.
020620     MOVE 逓減開始日４０ＷＲ           TO 逓減開始日４０.
020630     MOVE 後療単価４０ＷＲ             TO 後療単価４０.
020640     MOVE 後療回数４０ＷＲ             TO 後療回数４０.
020650     MOVE 後療料４０ＷＲ               TO 後療料４０.
020660     MOVE 冷罨法回数４０ＷＲ           TO 冷罨法回数４０.
020670     MOVE 冷罨法料４０ＷＲ             TO 冷罨法料４０.
020680     MOVE 温罨法回数４０ＷＲ           TO 温罨法回数４０.
020690     MOVE 温罨法料４０ＷＲ             TO 温罨法料４０.
020700     MOVE 電療回数４０ＷＲ             TO 電療回数４０.
020710     MOVE 電療料４０ＷＲ               TO 電療料４０.
020720     MOVE 小計４０ＷＲ                 TO 小計４０.
020730     IF ( 長期逓減率４０ＷＲ NOT = ZERO )
020740         COMPUTE 長期逓減率４０ = 長期逓減率４０ＷＲ / 100
020750     END-IF.
020760     MOVE 長期込小計４０ＷＲ           TO 長期込小計４０.
020770*
020780*↓***********************************************************************
020790* ５部位の印字枠なし。
020800*------------------------------------------------------------------------*
020810* ５部位／2.5割の印字は必要ない。
020820*------------------------------------------------------------------------*
020830*    *****************
020840*    * ５部位／2.5割 *
020850*    *****************
020860*     MOVE 後療単価５２ＷＲ             TO 後療単価５２.
020870*     MOVE 後療回数５２ＷＲ             TO 後療回数５２.
020880*     MOVE 後療料５２ＷＲ               TO 後療料５２.
020890*     MOVE 冷罨法回数５２ＷＲ           TO 冷罨法回数５２.
020900*     MOVE 冷罨法料５２ＷＲ             TO 冷罨法料５２.
020910*     MOVE 温罨法回数５２ＷＲ           TO 温罨法回数５２.
020920*     MOVE 温罨法料５２ＷＲ             TO 温罨法料５２.
020930*     MOVE 電療回数５２ＷＲ             TO 電療回数５２.
020940*     MOVE 電療料５２ＷＲ               TO 電療料５２.
020950*     MOVE 小計５２ＷＲ                 TO 小計５２.
020960*     MOVE 多部位込小計５２ＷＲ         TO 多部位込小計５２.
020970*     IF ( 長期逓減率５２ＷＲ NOT = ZERO )
020980*         COMPUTE 長期逓減率５２ = 長期逓減率５２ＷＲ / 100
020990*     END-IF.
021000*     MOVE 長期込小計５２ＷＲ           TO 長期込小計５２.
021010*
021020*    ****************
021030*    * ５部位／５割 *
021040*    ****************
021050*     IF ( 長期込小計５５ＷＲ NOT = ZERO )
021060*        MOVE "33"                      TO 逓減５５
021070*        MOVE "0.33"                    TO 逓減５５少数
021080*     END-IF.
021090*     MOVE 逓減開始月５５ＷＲ           TO 逓減開始月５５.
021100*     MOVE 逓減開始日５５ＷＲ           TO 逓減開始日５５.
021110*     MOVE 後療単価５５ＷＲ             TO 後療単価５５.
021120*     MOVE 後療回数５５ＷＲ             TO 後療回数５５.
021130*     MOVE 後療料５５ＷＲ               TO 後療料５５.
021140*     MOVE 冷罨法回数５５ＷＲ           TO 冷罨法回数５５.
021150*     MOVE 冷罨法料５５ＷＲ             TO 冷罨法料５５.
021160*     MOVE 温罨法回数５５ＷＲ           TO 温罨法回数５５.
021170*     MOVE 温罨法料５５ＷＲ             TO 温罨法料５５.
021180*     MOVE 電療回数５５ＷＲ             TO 電療回数５５.
021190*     MOVE 電療料５５ＷＲ               TO 電療料５５.
021200*     MOVE 小計５５ＷＲ                 TO 小計５５.
021210*     MOVE 多部位込小計５５ＷＲ         TO 多部位込小計５５.
021220*     IF ( 長期逓減率５５ＷＲ NOT = ZERO )
021230*         COMPUTE 長期逓減率５５ = 長期逓減率５５ＷＲ / 100
021240*     END-IF.
021250*     MOVE 長期込小計５５ＷＲ           TO 長期込小計５５.
021260*    ****************
021270*    * ５部位／８割 *
021280*    ****************
021290*     IF ( 長期込小計５８ＷＲ NOT = ZERO )
021300*        MOVE "80"                      TO 逓減５８
021310*        MOVE "0.8"                     TO 逓減５８少数
021320*     END-IF.
021330*     MOVE 逓減開始月５８ＷＲ           TO 逓減開始月５８.
021340*     MOVE 逓減開始日５８ＷＲ           TO 逓減開始日５８.
021350*     MOVE 後療単価５８ＷＲ             TO 後療単価５８.
021360*     MOVE 後療回数５８ＷＲ             TO 後療回数５８.
021370*     MOVE 後療料５８ＷＲ               TO 後療料５８.
021380*     MOVE 冷罨法回数５８ＷＲ           TO 冷罨法回数５８.
021390*     MOVE 冷罨法料５８ＷＲ             TO 冷罨法料５８.
021400*     MOVE 温罨法回数５８ＷＲ           TO 温罨法回数５８.
021410*     MOVE 温罨法料５８ＷＲ             TO 温罨法料５８.
021420*     MOVE 電療回数５８ＷＲ             TO 電療回数５８.
021430*     MOVE 電療料５８ＷＲ               TO 電療料５８.
021440*     MOVE 小計５８ＷＲ                 TO 小計５８.
021450*     MOVE 多部位込小計５８ＷＲ         TO 多部位込小計５８.
021460*     IF ( 長期逓減率５８ＷＲ NOT = ZERO )
021470*         COMPUTE 長期逓減率５８ = 長期逓減率５８ＷＲ / 100
021480*     END-IF.
021490*     MOVE 長期込小計５８ＷＲ           TO 長期込小計５８.
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
021500*    ****************
021510*    * ５部位／10割 *
021520*    ****************
021530*     IF ( 長期込小計５０ＷＲ NOT = ZERO )
021540*        MOVE "100"                     TO 逓減５０
021550*        MOVE "－"                      TO 逓減５０少数
021560*        MOVE ALL "―"                  TO 多部位込小計５０
021570*     END-IF.
021580*     MOVE 逓減開始月５０ＷＲ           TO 逓減開始月５０.
021590*     MOVE 逓減開始日５０ＷＲ           TO 逓減開始日５０.
021600*     MOVE 後療単価５０ＷＲ             TO 後療単価５０.
021610*     MOVE 後療回数５０ＷＲ             TO 後療回数５０.
021620*     MOVE 後療料５０ＷＲ               TO 後療料５０.
021630*     MOVE 冷罨法回数５０ＷＲ           TO 冷罨法回数５０.
021640*     MOVE 冷罨法料５０ＷＲ             TO 冷罨法料５０.
021650*     MOVE 温罨法回数５０ＷＲ           TO 温罨法回数５０.
021660*     MOVE 温罨法料５０ＷＲ             TO 温罨法料５０.
021670*     MOVE 電療回数５０ＷＲ             TO 電療回数５０.
021680*     MOVE 電療料５０ＷＲ               TO 電療料５０.
021690*     MOVE 小計５０ＷＲ                 TO 小計５０.
021700*     MOVE 長期逓減率５０ＷＲ           TO 長期逓減率５０.
021710*     IF ( 長期逓減率５０ＷＲ NOT = ZERO )
021720*         COMPUTE 長期逓減率５０ = 長期逓減率５０ＷＲ / 100
021730*     END-IF.
021740*     MOVE 長期込小計５０ＷＲ           TO 長期込小計５０.
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
021750*↑***********************************************************************
021760*
021770     MOVE 適用１Ｗ                      TO 適用１.
021780     MOVE 適用２Ｗ                      TO 適用２.
021770*     MOVE 適用１１Ｗ                     TO 適用１１.
021780*     MOVE 適用１２Ｗ                     TO 適用１２.
021790*     MOVE 適用２１Ｗ                     TO 適用２１.
021800*     MOVE 適用２２Ｗ                     TO 適用２２.
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
              MOVE 31           TO 連金運－会コード
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
021810     MOVE レセ－合計                     TO 合計.
021820     MOVE レセ－一部負担金               TO 一部負担金.
021830     MOVE レセ－請求金額                 TO 請求金額.
021840*
021850*     PERFORM 負担率取得１４１０.
021860*     EVALUATE 負担率Ｗ
021870*     WHEN 0
021880*         MOVE NC"○"                     TO １０割チェック
021890*     WHEN 10
021900*         MOVE NC"○"                     TO ９割チェック
      **/前期高齢者１割は、給付割合を８割にする。(国が１割負担するため、患者１割、保険者８割、国１割となる)
      *         IF (受－保険種別 NOT = 05 ) AND (受－特別区分 = 1) AND (受－施術和暦年月 >= 42004)
      **/神奈川県の国保、退職み対象に変更/080724
      *             IF ((受－保険種別 = 01 ) AND (受－保険者番号(1:2) = "14")) OR
      *                ((受－保険種別 = 08 ) AND (受－保険者番号(3:2) = "14"))
      *                 MOVE SPACE  TO ９割チェック
      *                 MOVE NC"○" TO ８割チェック
      *             END-IF
      *         END-IF
021910*     WHEN 20
021920*         MOVE NC"○"                     TO ８割チェック
021930*     WHEN 30
021940*         MOVE NC"○"                     TO ７割チェック
021950*     END-EVALUATE.
021960*
021970*------------------------------------------------------------------------------------*
021980* 特別（助成レセなしで、本体レセにまとめる時、金額は助成込み・適用２に助成種別印字）
021990     IF ( 助成レセまとめフラグ = "YES" )
022010         MOVE レセ－合計         TO 合計
      */レセまとめ時は本体の金額を記載する↓↓↓/130725
022020**     / 引き算する/
022030*         COMPUTE 請求金額 = レセ－合計 - レセ－助成請求金額
022040*         MOVE レセ－助成請求金額         TO 一部負担金
021820         MOVE レセ－一部負担金           TO 一部負担金
021830         MOVE レセ－請求金額             TO 請求金額
      */レセまとめ時は本体の金額を記載する↑↑↑/130725
022050*
022060*/深＿夜の空白にストリングしてしまうためNOT SPACEの時は最後に転記する。
021920         IF 助成種別略称Ｗ NOT = SPACE
021930            IF 適用２Ｗ NOT = SPACE
021940                MOVE SPACE TO 助成種別略称Ｗ２
021950                STRING NC"※"             DELIMITED BY SIZE
021960                       助成種別略称Ｗ     DELIMITED BY SPACE
021970                       INTO 助成種別略称Ｗ２
021980                END-STRING
021990                MOVE 助成種別略称Ｗ２ TO 適用２(35:4)
022000            ELSE
022010                STRING 適用２Ｗ           DELIMITED BY SPACE
022020                       NC"※"             DELIMITED BY SIZE
022030                       助成種別略称Ｗ     DELIMITED BY SPACE
022040                       INTO 適用２
022050                END-STRING
022060            END-IF
022070         END-IF
022070*         IF ( 助成種別略称Ｗ NOT = SPACE )
022080*            MOVE SPACE TO 助成種別略称Ｗ２
022090*            STRING NC"※"             DELIMITED BY SIZE
022100*                   助成種別略称Ｗ     DELIMITED BY SPACE
022110*                   INTO 助成種別略称Ｗ２
022120*            END-STRING
022130**
022140*            IF ( 適用２１Ｗ = SPACE )
022150*               MOVE 助成種別略称Ｗ２ TO 適用２１
022160*            ELSE
022170*               IF ( 適用２２Ｗ = SPACE )
022180*                  MOVE 助成種別略称Ｗ２ TO 適用２２
022190*               ELSE
022200*                  MOVE 助成種別略称Ｗ２ TO 適用２２(16:4)
022210*               END-IF
022220*            END-IF
022230*         END-IF
022240     END-IF.
022250*------------------------------------------------------------------------------------*
022480*
022490**********************
022500* 施術所データセット *
022510**********************
           MOVE 都道府県ＪＩＳＷ       TO 都道府県番号.
022520     MOVE 柔整師番号Ｗ           TO 柔整師番号.
022530*     MOVE 接骨師会会員番号Ｗ     TO 接骨師会会員番号.
022540***     MOVE 定額制受理番号Ｗ       TO 定額制受理番号.
022550     MOVE 施術所郵便番号１Ｗ     TO 施術所郵便番号１.
022560     MOVE 施術所郵便番号２Ｗ     TO 施術所郵便番号２.
022570*     MOVE 施術所住所Ｗ           TO 施術所住所.
022580     MOVE 施術所住所１Ｗ         TO 施術所住所１.
022590     MOVE 施術所住所２Ｗ         TO 施術所住所２.
022600     MOVE 代表者カナＷ           TO 代表者カナ.
022610     MOVE 代表者名Ｗ             TO 代表者名.
022620     MOVE 施術所電話番号Ｗ       TO 施術所電話番号.
022630*
022640     MOVE 接骨院名Ｗ             TO 接骨院名.
022650*
      *     MOVE "〒160-0004東京都新宿区四谷2-10 松本館501" TO 代理人住所.
022680*     MOVE "三井住友銀行 梅田支店(普)8675863"         TO 銀行名支店名.
022690*     MOVE "日本柔整共済 会長 細川 雅史(ﾆﾎﾝｼﾞｭｳｾｲｷｮｳｻｲ ｶｲﾁｮｳ ﾎｿｶﾜ ﾏｻﾌﾐ)" TO 口座名義人.
022660*     MOVE "日本柔整共済 会長 細川 雅史" TO 代理人氏名.
      *支払機関欄(会の口座)
           MOVE "8675863"                        TO 口座番号.
           MOVE "日本柔整共済 会長 細川 雅史"    TO 口座名義人.
           MOVE "ﾆﾎﾝｼﾞｭｳｾｲｷｮｳｻｲ ｶｲﾁｮｳ ﾎｿｶﾜ ﾏｻﾌﾐ" TO 口座名義人カナ.
           MOVE "三井住友"                       TO 金融機関名１
           MOVE "梅田"                           TO 支店名１.
           MOVE NC"○" TO 振込チェック.
           MOVE NC"○" TO 普通チェック.
           MOVE NC"○" TO 銀行チェック.
           MOVE NC"○" TO 支店チェック.
           MOVE "療養費の受領を日本柔整共済 会長 細川 雅史"         TO 会長委任コメント１
      *     MOVE "(東京都新宿区四谷2-10 松本館501)に委任します。"    TO 会長委任コメント２
           MOVE "(大阪府大阪市西区北堀江一丁目20-15長堀佐野ビル902)" TO 会長委任コメント２
           MOVE "に委任します。"                                     TO 会長委任コメント３
022700*
022710*     IF ( 取引先銀行名２Ｗ = SPACE )
022720*        MOVE SPACE               TO 銀行名１
022730*        MOVE 取引先銀行名１Ｗ    TO 銀行名２
022740*     ELSE
022750*        MOVE 取引先銀行名１Ｗ    TO 銀行名１
022760*        MOVE 取引先銀行名２Ｗ    TO 銀行名２
022770*     END-IF.
022780*     IF ( 取引先銀行支店名２Ｗ = SPACE )
022790*        MOVE SPACE                TO 銀行支店名１
022800*        MOVE 取引先銀行支店名１Ｗ TO 銀行支店名２
022810*     ELSE
022820*        MOVE 取引先銀行支店名１Ｗ TO 銀行支店名１
022830*        MOVE 取引先銀行支店名２Ｗ TO 銀行支店名２
022840*     END-IF.
022850***     MOVE 預金種別コメントＷ     TO 預金種別.
022860*     MOVE 口座番号Ｗ             TO 口座番号.
022870***     MOVE 口座名義人カナＷ       TO 口座名義人カナ.
022880***     MOVE 口座名義人Ｗ           TO 口座名義人.
022890*
022900      MOVE 施術和暦ＷＲ TO 元－元号区分
022910      READ 元号マスタ
022920      NOT INVALID KEY
022930          MOVE 元－元号名称 TO 施術和暦名称Ｗ
022940      END-READ
022950* / 柔整師・患者委任日 /
022960*     MOVE 施術和暦名称Ｗ         TO 受理和暦.
      */元号修正/↓↓↓20190426
037370     IF 施術和暦Ｗ > 4
               MOVE 施術和暦Ｗ         TO 元－元号区分
037380         READ 元号マスタ
037390         NOT INVALID KEY
037400             MOVE 元－元号名称   TO 受理和暦
037410         END-READ
               MOVE "===="             TO 受理和暦訂正
           END-IF.
      */元号修正/↑↑↑20190426
022970     MOVE 柔整師年Ｗ             TO 受理年.
022980     MOVE 柔整師月Ｗ             TO 受理月.
022990     MOVE 柔整師日Ｗ             TO 受理日.
023000* ( 委任年月日 印刷するか )
023010     IF ( 連入－委任印刷  = ZERO )
023020*         MOVE 施術和暦名称Ｗ     TO 委任和暦
      */元号修正/↓↓↓20190426
037370         IF 施術和暦Ｗ > 4
                   MOVE 施術和暦Ｗ         TO 元－元号区分
037380             READ 元号マスタ
037390             NOT INVALID KEY
037400                 MOVE 元－元号名称   TO 委任和暦
037410             END-READ
                   MOVE "===="             TO 委任和暦訂正
               END-IF
      */元号修正/↑↑↑20190426
023030         MOVE 患者委任年Ｗ       TO 委任年
023040         MOVE 患者委任月Ｗ       TO 委任月
023050         MOVE 患者委任日Ｗ       TO 委任日
023060*         MOVE 請求先名称ＷＴ     TO 保険者名称委任
023070*         MOVE NC"殿"             TO 殿
023080     END-IF.
023090*
      */徳島県対応/100618
023100* 施術ID
023110     MOVE 県施術ＩＤＷ           TO 県施術ＩＤ.
023120*
023130* 共済番号
023140     MOVE 共済番号Ｗ             TO 共済番号.
023150*
023160************************
023170* レセプト並び順セット *
023180************************
023190     MOVE 順番固定Ｗ          TO 順番固定.
023200     MOVE 順番Ｗ              TO 順番.
023210     MOVE "("                 TO 括弧１.
023220     MOVE 患者番号ＷＲ        TO 患者番号.
023230     MOVE 枝番ＷＲ            TO 枝番.
023240     MOVE ")"                 TO 括弧２.
023250*
023260*
023270* 特別コメント
023280*     MOVE 特別コメントＷ      TO 特別コメント.
023300*-------------------------------------------------------------------------*
023310*--- ※ レセ摘要再セットは、この印刷セットSECTION の最後にやること！ -----*
023320     PERFORM レセ摘要再セット.
023330*-------------------------------------------------------------------------*
023340*
023350***    PERFORM テスト印字処理.
023360*
023370*================================================================*
023380 項目初期化 SECTION.
023390*
023400     INITIALIZE 施術所情報Ｗ.
023410     INITIALIZE 受診者情報Ｗ.
023420     INITIALIZE 負傷情報Ｗ.
023430     INITIALIZE 備考情報Ｗ.
023440     INITIALIZE 料金１ＷＲ.
023450     INITIALIZE 料金２ＷＲ.
023460     INITIALIZE 料金３ＷＲ.
023480     INITIALIZE YJK6125P.
023470     MOVE SPACE TO YJK6125P.
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
023490*================================================================*
023500 料金情報取得 SECTION.
023510*
023520********************
023530* 料金データセット *
023540********************
023550*    ****************************************************************
023560*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
023570*    ****************************************************************
023580     MOVE レセ－初検料                 TO 初検料ＷＲ.
023590     IF ( レセ－時間外 = 1 )
023600         MOVE NC"○"                   TO 時間外チェックＷ
023610     END-IF.
023620     IF ( レセ－休日 = 1 )
023630         MOVE NC"○"                   TO 休日チェックＷ
023640     END-IF.
023650     IF ( レセ－深夜 = 1 )
023660         MOVE NC"○"                   TO 深夜チェックＷ
023670     END-IF.
023680*
023690     MOVE レセ－初検加算料             TO 初検加算料ＷＲ.
           MOVE レセ－初検時相談料           TO 初検時相談料ＷＲ.
023700     MOVE レセ－再検料                 TO 再検料ＷＲ.
023710     MOVE レセ－往療距離               TO 往療距離ＷＲ.
023720     MOVE レセ－往療回数               TO 往療回数ＷＲ.
023730     MOVE レセ－往療料                 TO 往療料ＷＲ.
023740     MOVE レセ－往療加算料             TO 往療加算料ＷＲ.
023750*
023760     IF ( レセ－夜間 = 1 )
023770         MOVE NC"○"                   TO 夜間チェックＷ
023780     END-IF.
023790     IF ( レセ－難路 = 1 )
023800         MOVE NC"○"                   TO 難路チェックＷ
023810     END-IF.
023820     IF ( レセ－暴風雨雪 = 1 )
023830         MOVE NC"○"                   TO 暴風雨雪チェックＷ
023840     END-IF.
023850*
023860     MOVE レセ－金属副子加算料         TO 金属副子加算料ＷＲ.
023870*
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
023970*
023980     MOVE レセ－施術情報提供料         TO 施術情報提供料ＷＲ.
023990* 小計
024000     MOVE レセ－小計                   TO 小計Ｗ.
024010********************
024020* 初回処置料セット *
024030********************
024040     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
024050             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
024060         MOVE レセ－初回処置料(部位ＣＮＴ) TO 初回処置料ＷＲ(部位ＣＮＴ)
024070         IF ( レセ－初回処置料(部位ＣＮＴ) NOT = ZERO )
024080            EVALUATE 負－負傷種別(部位ＣＮＴ)
024090* 捻挫・打撲・挫傷
024100            WHEN 1
024110            WHEN 2
024120            WHEN 3
024130                MOVE NC"○"       TO 施療料チェックＷ
024140* 脱臼・骨折・骨折拘縮
024150            WHEN 4
024160            WHEN 5
024170            WHEN 7
024180                MOVE NC"○"       TO 整復料チェックＷ
024190* 不全骨折・不全骨折拘縮
024200            WHEN 6
024210            WHEN 8
024220                MOVE NC"○"       TO 固定料チェックＷ
024230            END-EVALUATE
024240         END-IF
024250     END-PERFORM.
024260*
024270     MOVE レセ－初回処置料合計    TO 初回処置料合計Ｗ.
024280********************
024290* 逓減毎料金セット *
024300********************
024310*    **********
024320*    * １部位 *
024330*    **********
024340     MOVE レセ－後療単価１             TO 後療単価１ＷＲ.
024350     MOVE レセ－後療回数１             TO 後療回数１ＷＲ.
024360     MOVE レセ－後療料１               TO 後療料１ＷＲ.
024370     MOVE レセ－冷罨法回数１           TO 冷罨法回数１ＷＲ.
024380     MOVE レセ－冷罨法料１             TO 冷罨法料１ＷＲ.
024390     MOVE レセ－温罨法回数１           TO 温罨法回数１ＷＲ.
024400     MOVE レセ－温罨法料１             TO 温罨法料１ＷＲ.
024410     MOVE レセ－電療回数１             TO 電療回数１ＷＲ.
024420     MOVE レセ－電療料１               TO 電療料１ＷＲ.
024430     MOVE レセ－小計１                 TO 小計１ＷＲ.
024440     MOVE レセ－長期逓減率１           TO 長期逓減率１ＷＲ.
024450     MOVE レセ－長期込小計１           TO 長期込小計１ＷＲ.
024460*    **********
024470*    * ２部位 *
024480*    **********
024490     MOVE レセ－後療単価２             TO 後療単価２ＷＲ.
024500     MOVE レセ－後療回数２             TO 後療回数２ＷＲ.
024510     MOVE レセ－後療料２               TO 後療料２ＷＲ.
024520     MOVE レセ－冷罨法回数２           TO 冷罨法回数２ＷＲ.
024530     MOVE レセ－冷罨法料２             TO 冷罨法料２ＷＲ.
024540     MOVE レセ－温罨法回数２           TO 温罨法回数２ＷＲ.
024550     MOVE レセ－温罨法料２             TO 温罨法料２ＷＲ.
024560     MOVE レセ－電療回数２             TO 電療回数２ＷＲ.
024570     MOVE レセ－電療料２               TO 電療料２ＷＲ.
024580     MOVE レセ－小計２                 TO 小計２ＷＲ.
024590     MOVE レセ－長期逓減率２           TO 長期逓減率２ＷＲ.
024600     MOVE レセ－長期込小計２           TO 長期込小計２ＷＲ.
024610*    ****************
024620*    * ３部位／８割 *
024630*    ****************
024640     MOVE レセ－後療単価３８             TO 後療単価３８ＷＲ.
024650     MOVE レセ－後療回数３８             TO 後療回数３８ＷＲ.
024660     MOVE レセ－後療料３８               TO 後療料３８ＷＲ.
024670     MOVE レセ－冷罨法回数３８           TO 冷罨法回数３８ＷＲ.
024680     MOVE レセ－冷罨法料３８             TO 冷罨法料３８ＷＲ.
024690     MOVE レセ－温罨法回数３８           TO 温罨法回数３８ＷＲ.
024700     MOVE レセ－温罨法料３８             TO 温罨法料３８ＷＲ.
024710     MOVE レセ－電療回数３８             TO 電療回数３８ＷＲ.
024720     MOVE レセ－電療料３８               TO 電療料３８ＷＲ.
024730     MOVE レセ－小計３８                 TO 小計３８ＷＲ.
024740     MOVE レセ－多部位込小計３８         TO 多部位込小計３８ＷＲ.
024750     MOVE レセ－長期逓減率３８           TO 長期逓減率３８ＷＲ.
024760     MOVE レセ－長期込小計３８           TO 長期込小計３８ＷＲ.
024770*    ****************
024780*    * ３部位／10割 *
024790*    ****************
024800     MOVE レセ－逓減開始月３０           TO 逓減開始月３０ＷＲ.
024810     MOVE レセ－逓減開始日３０           TO 逓減開始日３０ＷＲ.
024820     MOVE レセ－後療単価３０             TO 後療単価３０ＷＲ.
024830     MOVE レセ－後療回数３０             TO 後療回数３０ＷＲ.
024840     MOVE レセ－後療料３０               TO 後療料３０ＷＲ.
024850     MOVE レセ－冷罨法回数３０           TO 冷罨法回数３０ＷＲ.
024860     MOVE レセ－冷罨法料３０             TO 冷罨法料３０ＷＲ.
024870     MOVE レセ－温罨法回数３０           TO 温罨法回数３０ＷＲ.
024880     MOVE レセ－温罨法料３０             TO 温罨法料３０ＷＲ.
024890     MOVE レセ－電療回数３０             TO 電療回数３０ＷＲ.
024900     MOVE レセ－電療料３０               TO 電療料３０ＷＲ.
024910     MOVE レセ－小計３０                 TO 小計３０ＷＲ.
024920     MOVE レセ－長期逓減率３０           TO 長期逓減率３０ＷＲ.
024930     MOVE レセ－長期込小計３０           TO 長期込小計３０ＷＲ.
024940*    ****************
024950*    * ４部位／５割 *
024960*    ****************
024970     MOVE レセ－後療単価４５             TO 後療単価４５ＷＲ.
024980     MOVE レセ－後療回数４５             TO 後療回数４５ＷＲ.
024990     MOVE レセ－後療料４５               TO 後療料４５ＷＲ.
025000     MOVE レセ－冷罨法回数４５           TO 冷罨法回数４５ＷＲ.
025010     MOVE レセ－冷罨法料４５             TO 冷罨法料４５ＷＲ.
025020     MOVE レセ－温罨法回数４５           TO 温罨法回数４５ＷＲ.
025030     MOVE レセ－温罨法料４５             TO 温罨法料４５ＷＲ.
025040     MOVE レセ－電療回数４５             TO 電療回数４５ＷＲ.
025050     MOVE レセ－電療料４５               TO 電療料４５ＷＲ.
025060     MOVE レセ－小計４５                 TO 小計４５ＷＲ.
025070     MOVE レセ－多部位込小計４５         TO 多部位込小計４５ＷＲ.
025080     MOVE レセ－長期逓減率４５           TO 長期逓減率４５ＷＲ.
025090     MOVE レセ－長期込小計４５           TO 長期込小計４５ＷＲ.
025100*    ****************
025110*    * ４部位／８割 *
025120*    ****************
025130     MOVE レセ－逓減開始月４８           TO 逓減開始月４８ＷＲ.
025140     MOVE レセ－逓減開始日４８           TO 逓減開始日４８ＷＲ.
025150     MOVE レセ－後療単価４８             TO 後療単価４８ＷＲ.
025160     MOVE レセ－後療回数４８             TO 後療回数４８ＷＲ.
025170     MOVE レセ－後療料４８               TO 後療料４８ＷＲ.
025180     MOVE レセ－冷罨法回数４８           TO 冷罨法回数４８ＷＲ.
025190     MOVE レセ－冷罨法料４８             TO 冷罨法料４８ＷＲ.
025200     MOVE レセ－温罨法回数４８           TO 温罨法回数４８ＷＲ.
025210     MOVE レセ－温罨法料４８             TO 温罨法料４８ＷＲ.
025220     MOVE レセ－電療回数４８             TO 電療回数４８ＷＲ.
025230     MOVE レセ－電療料４８               TO 電療料４８ＷＲ.
025240     MOVE レセ－小計４８                 TO 小計４８ＷＲ.
025250     MOVE レセ－多部位込小計４８         TO 多部位込小計４８ＷＲ.
025260     MOVE レセ－長期逓減率４８           TO 長期逓減率４８ＷＲ.
025270     MOVE レセ－長期込小計４８           TO 長期込小計４８ＷＲ.
025280*    ****************
025290*    * ４部位／10割 *
025300*    ****************
025310     MOVE レセ－逓減開始月４０           TO 逓減開始月４０ＷＲ.
025320     MOVE レセ－逓減開始日４０           TO 逓減開始日４０ＷＲ.
025330     MOVE レセ－後療単価４０             TO 後療単価４０ＷＲ.
025340     MOVE レセ－後療回数４０             TO 後療回数４０ＷＲ.
025350     MOVE レセ－後療料４０               TO 後療料４０ＷＲ.
025360     MOVE レセ－冷罨法回数４０           TO 冷罨法回数４０ＷＲ.
025370     MOVE レセ－冷罨法料４０             TO 冷罨法料４０ＷＲ.
025380     MOVE レセ－温罨法回数４０           TO 温罨法回数４０ＷＲ.
025390     MOVE レセ－温罨法料４０             TO 温罨法料４０ＷＲ.
025400     MOVE レセ－電療回数４０             TO 電療回数４０ＷＲ.
025410     MOVE レセ－電療料４０               TO 電療料４０ＷＲ.
025420     MOVE レセ－小計４０                 TO 小計４０ＷＲ.
025430     MOVE レセ－長期逓減率４０           TO 長期逓減率４０ＷＲ.
025440     MOVE レセ－長期込小計４０           TO 長期込小計４０ＷＲ.
025450*    *****************
025460*    * ５部位／2.5割 *
025470*    *****************
025480     MOVE レセ－後療単価５２             TO 後療単価５２ＷＲ.
025490     MOVE レセ－後療回数５２             TO 後療回数５２ＷＲ.
025500     MOVE レセ－後療料５２               TO 後療料５２ＷＲ.
025510     MOVE レセ－冷罨法回数５２           TO 冷罨法回数５２ＷＲ.
025520     MOVE レセ－冷罨法料５２             TO 冷罨法料５２ＷＲ.
025530     MOVE レセ－温罨法回数５２           TO 温罨法回数５２ＷＲ.
025540     MOVE レセ－温罨法料５２             TO 温罨法料５２ＷＲ.
025550     MOVE レセ－電療回数５２             TO 電療回数５２ＷＲ.
025560     MOVE レセ－電療料５２               TO 電療料５２ＷＲ.
025570     MOVE レセ－小計５２                 TO 小計５２ＷＲ.
025580     MOVE レセ－多部位込小計５２         TO 多部位込小計５２ＷＲ.
025590     MOVE レセ－長期逓減率５２           TO 長期逓減率５２ＷＲ.
025600     MOVE レセ－長期込小計５２           TO 長期込小計５２ＷＲ.
025610*    ****************
025620*    * ５部位／５割 *
025630*    ****************
025640     MOVE レセ－逓減開始月５５           TO 逓減開始月５５ＷＲ.
025650     MOVE レセ－逓減開始日５５           TO 逓減開始日５５ＷＲ.
025660     MOVE レセ－後療単価５５             TO 後療単価５５ＷＲ.
025670     MOVE レセ－後療回数５５             TO 後療回数５５ＷＲ.
025680     MOVE レセ－後療料５５               TO 後療料５５ＷＲ.
025690     MOVE レセ－冷罨法回数５５           TO 冷罨法回数５５ＷＲ.
025700     MOVE レセ－冷罨法料５５             TO 冷罨法料５５ＷＲ.
025710     MOVE レセ－温罨法回数５５           TO 温罨法回数５５ＷＲ.
025720     MOVE レセ－温罨法料５５             TO 温罨法料５５ＷＲ.
025730     MOVE レセ－電療回数５５             TO 電療回数５５ＷＲ.
025740     MOVE レセ－電療料５５               TO 電療料５５ＷＲ.
025750     MOVE レセ－小計５５                 TO 小計５５ＷＲ.
025760     MOVE レセ－多部位込小計５５         TO 多部位込小計５５ＷＲ.
025770     MOVE レセ－長期逓減率５５           TO 長期逓減率５５ＷＲ.
025780     MOVE レセ－長期込小計５５           TO 長期込小計５５ＷＲ.
025790*    ****************
025800*    * ５部位／８割 *
025810*    ****************
025820     MOVE レセ－逓減開始月５８           TO 逓減開始月５８ＷＲ.
025830     MOVE レセ－逓減開始日５８           TO 逓減開始日５８ＷＲ.
025840     MOVE レセ－後療単価５８             TO 後療単価５８ＷＲ.
025850     MOVE レセ－後療回数５８             TO 後療回数５８ＷＲ.
025860     MOVE レセ－後療料５８               TO 後療料５８ＷＲ.
025870     MOVE レセ－冷罨法回数５８           TO 冷罨法回数５８ＷＲ.
025880     MOVE レセ－冷罨法料５８             TO 冷罨法料５８ＷＲ.
025890     MOVE レセ－温罨法回数５８           TO 温罨法回数５８ＷＲ.
025900     MOVE レセ－温罨法料５８             TO 温罨法料５８ＷＲ.
025910     MOVE レセ－電療回数５８             TO 電療回数５８ＷＲ.
025920     MOVE レセ－電療料５８               TO 電療料５８ＷＲ.
025930     MOVE レセ－小計５８                 TO 小計５８ＷＲ.
025940     MOVE レセ－多部位込小計５８         TO 多部位込小計５８ＷＲ.
025950     MOVE レセ－長期逓減率５８           TO 長期逓減率５８ＷＲ.
025960     MOVE レセ－長期込小計５８           TO 長期込小計５８ＷＲ.
025970*    ****************
025980*    * ５部位／10割 *
025990*    ****************
026000     MOVE レセ－逓減開始月５０           TO 逓減開始月５０ＷＲ.
026010     MOVE レセ－逓減開始日５０           TO 逓減開始日５０ＷＲ.
026020     MOVE レセ－後療単価５０             TO 後療単価５０ＷＲ.
026030     MOVE レセ－後療回数５０             TO 後療回数５０ＷＲ.
026040     MOVE レセ－後療料５０               TO 後療料５０ＷＲ.
026050     MOVE レセ－冷罨法回数５０           TO 冷罨法回数５０ＷＲ.
026060     MOVE レセ－冷罨法料５０             TO 冷罨法料５０ＷＲ.
026070     MOVE レセ－温罨法回数５０           TO 温罨法回数５０ＷＲ.
026080     MOVE レセ－温罨法料５０             TO 温罨法料５０ＷＲ.
026090     MOVE レセ－電療回数５０             TO 電療回数５０ＷＲ.
026100     MOVE レセ－電療料５０               TO 電療料５０ＷＲ.
026110     MOVE レセ－小計５０                 TO 小計５０ＷＲ.
026120     MOVE レセ－長期逓減率５０           TO 長期逓減率５０ＷＲ.
026130     MOVE レセ－長期込小計５０           TO 長期込小計５０ＷＲ.
026140*
026150*================================================================*
026160 施術所情報取得 SECTION.
026170*
026180**************************************************
026190* 本院データを使用し、以下の情報を取得           *
026200* ● 柔整師番号.. 柔整師番号Ｗに格納             *
026210* ● 会員番号 ... 接骨師会会員番号Ｗに格納       *
026220* ● 代表者名 ... 代表者名Ｗに格納               *
026230* ● 住所1,2   ...施術所住所1,2Ｗに格納          *
026240* ● 電話番号 ... 施術所電話番号Ｗに格納         *
026250**************************************************
026260     MOVE ZERO  TO 施情－施術所番号.
026270     READ 施術所情報マスタ
026280     INVALID KEY
026290         CONTINUE
026300     NOT INVALID KEY
026310*
               MOVE 施情－都道府県ＪＩＳ    TO 都道府県ＪＩＳＷ
026320         MOVE 施情－新柔整師番号      TO 柔整師番号Ｗ
026330*
026340         STRING "JM-"                  DELIMITED BY SIZE
026350                施情－接骨師会会員番号 DELIMITED BY SIZE
026360           INTO 接骨師会会員番号Ｗ
026370         END-STRING
026380*
026390         MOVE 施情－郵便番号１        TO 施術所郵便番号１Ｗ
026400         MOVE 施情－郵便番号２        TO 施術所郵便番号２Ｗ
026410         MOVE 施情－代表者カナ        TO 代表者カナＷ
026420         MOVE 施情－代表者名          TO 代表者名Ｗ
026430*
026440         MOVE 施情－接骨院名          TO 接骨院名Ｗ
026450*
026460*         MOVE 施情－住所１            TO 施術所住所１Ｗ
026470*         MOVE 施情－住所２            TO 施術所住所２Ｗ
026480         STRING 施情－住所１  DELIMITED BY SPACE
026490                施情－住所２  DELIMITED BY SPACE
026500           INTO 施術所住所Ｗ
026510         END-STRING
026520*
026530         MOVE 施情－電話番号          TO 施術所電話番号Ｗ
026540*
026550***         MOVE 施情－取引先銀行名      TO 取引先銀行名Ｗ
026560***         MOVE 施情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
026570***         MOVE 施情－預金種別          TO 預金種別Ｗ
026580***         MOVE 施情－口座番号          TO 口座番号Ｗ
026590***         MOVE 施情－口座名義人カナ    TO 口座名義人カナＷ
026600***         MOVE 施情－口座名義人        TO 口座名義人Ｗ
026610*
026620***         EVALUATE 預金種別Ｗ
026630***         WHEN 1
026640***             MOVE NC"（普）" TO 預金種別コメントＷ
026650***         WHEN 2
026660***             MOVE NC"（当）" TO 預金種別コメントＷ
026670***         WHEN OTHER
026680***             MOVE SPACE      TO 預金種別コメントＷ
026690***         END-EVALUATE
026700*
026710*------------------------------------------------------------------------*
026720         EVALUATE 保険種別ＷＲ
026730         WHEN 01
026740             MOVE 保険者番号ＷＲ       TO 保険者番号比較Ｗ
026750             PERFORM 県施術ＩＤセット
               WHEN 05
026760         WHEN 08
026770             MOVE 保険者番号ＷＲ(3:6)  TO 保険者番号比較Ｗ
026780             PERFORM 県施術ＩＤセット
026790         WHEN 04
026800             PERFORM 共済番号セット
026810         WHEN 09
026820             PERFORM 自衛官番号セット
026830         END-EVALUATE
026840*
026850     END-READ.
026860*
026870*================================================================*
026880 県施術ＩＤセット SECTION.
026890*
026900*********************************************
026910** ＩＤ管理マスタより  県施術ＩＤを取得する。
026920*   (国保組合は、対象外　→　対象！2005/09 )
026930*********************************************
026940**   / 県施術ID /
026950     MOVE 01                     TO ＩＤ管－ＩＤ区分.
026960     MOVE ZERO                   TO ＩＤ管－施術所番号.
026970     MOVE 保険者番号比較Ｗ(1:2)  TO ＩＤ管－保険種別.
026980     MOVE SPACE                  TO ＩＤ管－保険者番号.
026990     READ ＩＤ管理マスタ
027000     NOT INVALID KEY
027010         MOVE ＩＤ管－施術ＩＤ番号   TO 県施術ＩＤＷ
027020     END-READ.
027030*
027040*================================================================*
027050 共済番号セット SECTION.
027060*
027070**************************************************************
027080* 保険者番号により、共済の番号を印字するか判定
027090* 中央特有 追加 99/10
027100**************************************************************
027110** 1.共済組合連盟
027120     MOVE SPACE  TO  脱出フラグ.
027130     IF ( 施情－共済連番号 NOT = ZERO )
027140** 条件(保険者番号)
027150        IF ( 保険者番号ＷＲ(1:2) = "31" )  OR
027160           ( 保険者番号ＷＲ = "34130021" )
027170*
027180           MOVE  NC"共済組合連盟第"   TO 共済連番号名ＮＷ 
027190           MOVE  NC"号"               TO 共済連番号単位ＮＷ 
027200           MOVE  施情－共済連番号     TO 共済連番号Ｗ
027210           IF ( 共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
027220                 MOVE SPACE TO  共済連番号Ｗ(1:1)
027230           ELSE
027240                 MOVE "YES" TO  脱出フラグ
027250           END-IF
027260           IF ( 共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
027270                 MOVE SPACE TO  共済連番号Ｗ(2:1)
027280           ELSE
027290                 MOVE "YES" TO  脱出フラグ
027300           END-IF
027310           IF ( 共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
027320                 MOVE SPACE TO  共済連番号Ｗ(3:1)
027330           ELSE
027340                 MOVE "YES" TO  脱出フラグ
027350           END-IF
027360           IF ( 共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
027370                 MOVE SPACE TO  共済連番号Ｗ(4:1)
027380           ELSE
027390                 MOVE "YES" TO  脱出フラグ
027400           END-IF
027410           IF ( 共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
027420                 MOVE SPACE TO  共済連番号Ｗ(5:1)
027430           ELSE
027440                 MOVE "YES" TO  脱出フラグ
027450           END-IF
027460           IF ( 共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
027470                 MOVE SPACE TO  共済連番号Ｗ(6:1)
027480           ELSE
027490                 MOVE "YES" TO  脱出フラグ
027500           END-IF
027510           MOVE  共済連番号集団Ｗ     TO 共済番号Ｗ
027520        END-IF
027530     END-IF.
027540*
027550** 2. 地共済協議会
027560     MOVE SPACE  TO  脱出フラグ.
027570     IF ( 施情－地共済連番号 NOT = ZERO )
027580** 条件(保険者番号)
027590        IF ( 保険者番号ＷＲ(1:2) = "32" OR "33" OR "34" )  AND
027600           ( 保険者番号ＷＲ NOT = "34130021" )
027610*
027620           MOVE  NC"地共済協議会第"   TO 共済連番号名ＮＷ 
027630           MOVE  NC"号"               TO 共済連番号単位ＮＷ 
027640           MOVE  施情－地共済連番号   TO 共済連番号Ｗ
027650           IF ( 共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
027660                 MOVE SPACE TO  共済連番号Ｗ(1:1)
027670           ELSE
027680                 MOVE "YES" TO  脱出フラグ
027690           END-IF
027700           IF ( 共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
027710                 MOVE SPACE TO  共済連番号Ｗ(2:1)
027720           ELSE
027730                 MOVE "YES" TO  脱出フラグ
027740           END-IF
027750           IF ( 共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
027760                 MOVE SPACE TO  共済連番号Ｗ(3:1)
027770           ELSE
027780                 MOVE "YES" TO  脱出フラグ
027790           END-IF
027800           IF ( 共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
027810                 MOVE SPACE TO  共済連番号Ｗ(4:1)
027820           ELSE
027830                 MOVE "YES" TO  脱出フラグ
027840           END-IF
027850           IF ( 共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
027860                 MOVE SPACE TO  共済連番号Ｗ(5:1)
027870           ELSE
027880                 MOVE "YES" TO  脱出フラグ
027890           END-IF
027900           IF ( 共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
027910                 MOVE SPACE TO  共済連番号Ｗ(6:1)
027920           ELSE
027930                 MOVE "YES" TO  脱出フラグ
027940           END-IF
027950           MOVE  共済連番号集団Ｗ     TO 共済番号Ｗ
027960        END-IF
027970**
027980**------/  地共済連番号が未入力で、共済連番号が入力されている時は、無条件に共済連番号
027990**         をセットする。(中央特有)   /
028000     ELSE
028010        IF ( 施情－共済連番号 NOT = ZERO ) AND ( 共済番号Ｗ = SPACE )
028020*
028030           MOVE  NC"共済組合連盟第"   TO 共済連番号名ＮＷ 
028040           MOVE  NC"号"               TO 共済連番号単位ＮＷ 
028050           MOVE  施情－共済連番号     TO 共済連番号Ｗ
028060           IF ( 共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
028070                 MOVE SPACE TO  共済連番号Ｗ(1:1)
028080           ELSE
028090                 MOVE "YES" TO  脱出フラグ
028100           END-IF
028110           IF ( 共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
028120                 MOVE SPACE TO  共済連番号Ｗ(2:1)
028130           ELSE
028140                 MOVE "YES" TO  脱出フラグ
028150           END-IF
028160           IF ( 共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
028170                 MOVE SPACE TO  共済連番号Ｗ(3:1)
028180           ELSE
028190                 MOVE "YES" TO  脱出フラグ
028200           END-IF
028210           IF ( 共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
028220                 MOVE SPACE TO  共済連番号Ｗ(4:1)
028230           ELSE
028240                 MOVE "YES" TO  脱出フラグ
028250           END-IF
028260           IF ( 共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
028270                 MOVE SPACE TO  共済連番号Ｗ(5:1)
028280           ELSE
028290                 MOVE "YES" TO  脱出フラグ
028300           END-IF
028310           IF ( 共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
028320                 MOVE SPACE TO  共済連番号Ｗ(6:1)
028330           ELSE
028340                 MOVE "YES" TO  脱出フラグ
028350           END-IF
028360           MOVE  共済連番号集団Ｗ     TO 共済番号Ｗ
028370        END-IF
028380*
028390     END-IF.
028400*
028410*================================================================*
028420 自衛官番号セット SECTION.
028430*
028440     MOVE SPACE  TO  脱出フラグ.
028450     IF ( 施情－自衛官番号 NOT = ZERO )
028451           IF 施情－防衛省区分 = 1
028452              MOVE  NC"防衛省第"      TO 自衛官番号名ＮＷ 
028453           ELSE
028454              MOVE  NC"防衛庁第"      TO 自衛官番号名ＮＷ 
028455           END-IF
028460*           MOVE  NC"防衛庁第"         TO 自衛官番号名ＮＷ 
028470           MOVE  NC"号"               TO 自衛官番号単位ＮＷ 
028480           MOVE  施情－自衛官番号     TO 自衛官番号Ｗ
028490           IF ( 自衛官番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
028500                 MOVE SPACE TO  自衛官番号Ｗ(1:1)
028510           ELSE
028520                 MOVE "YES" TO  脱出フラグ
028530           END-IF
028540           IF ( 自衛官番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
028550                 MOVE SPACE TO  自衛官番号Ｗ(2:1)
028560           ELSE
028570                 MOVE "YES" TO  脱出フラグ
028580           END-IF
028590           IF ( 自衛官番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
028600                 MOVE SPACE TO  自衛官番号Ｗ(3:1)
028610           ELSE
028620                 MOVE "YES" TO  脱出フラグ
028630           END-IF
028640           IF ( 自衛官番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
028650                 MOVE SPACE TO  自衛官番号Ｗ(4:1)
028660           ELSE
028670                 MOVE "YES" TO  脱出フラグ
028680           END-IF
028690           IF ( 自衛官番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
028700                 MOVE SPACE TO  自衛官番号Ｗ(5:1)
028710           ELSE
028720                 MOVE "YES" TO  脱出フラグ
028730           END-IF
028740           IF ( 自衛官番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
028750                 MOVE SPACE TO  自衛官番号Ｗ(6:1)
028760           ELSE
028770                 MOVE "YES" TO  脱出フラグ
028780           END-IF
028790           MOVE  自衛官番号集団Ｗ     TO 共済番号Ｗ
028800     END-IF.
028810*
028820*================================================================*
028830 受診者情報取得 SECTION.
028840*
028850**************************************************
028860* 連結データから受診者情報Ｆより以下の情報を取得 *
028870* ● 施術年 ..... 施術年Ｗに格納                 *
028880* ● 施術月 ..... 施術月Ｗに格納                 *
028890* ● 患者番号.... 患者番号Ｗに格納※ＦＤ連番用   *
028900* ● 記号 ....... 記号Ｗに格納                   *
028910* ● 番号 ....... 番号Ｗに格納                   *
028920* ● 保険者番号 . 保険者番号Ｗに格納             *
028930* ● 保険種別 ... 保険種別Ｗに格納               *
028940* ● 被保険者カナ.被保険者カナＷに格納           *
028950* ● 被保険者氏名.被保険者氏名Ｗに格納           *
028960* ● 住所１ ......被保険者住所１Ｗに格納         *
028970* ● 住所２ ......被保険者住所２Ｗに格納         *
028980* ● 患者カナ ....患者カナＷに格納               *
028990* ● 患者氏名 ....患者氏名Ｗに格納               *
029000* ● 患者性別 ....区分によりチェックに"○"を格納 *
029010* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
029020* ● 患者年 ......患者年Ｗに格納                 *
029030* ● 患者月 ......患者月Ｗに格納                 *
029040* ● 患者日 ......患者日Ｗに格納                 *
029050* ● 続柄 ........名称マスタより続柄Ｗに取得     *
029060**************************************************
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
               WHEN 04
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
      *         IF 受－保険種別 = 01 OR 08
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
      */元号修正/20190426
               MOVE 受－施術和暦     TO 施術和暦Ｗ
029160         MOVE 受－施術年       TO 施術年Ｗ
029170         MOVE 受－施術月       TO 施術月Ｗ
029180         MOVE 受－患者番号     TO 患者番号Ｗ
027850*         MOVE 受－記号         TO 記号Ｗ
029200*         MOVE 受－番号         TO 番号Ｗ
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
029210         MOVE 受－保険者番号   TO 保険者番号Ｗ 保険者番号ＷＰ
029220         MOVE 受－保険種別     TO 保険種別Ｗ
029230         MOVE 受－公費種別     TO 公費種別ＷＲ
029240         MOVE 受－助成種別     TO 助成種別ＷＲ
029250** 全国土木の枝番削除
029260         IF ( 受－保険種別 = 01 ) AND ( 受－保険者番号(1:6) = "133033" )
029270            MOVE 受－保険者番号(1:6)  TO 保険者番号Ｗ 保険者番号ＷＰ
029280         END-IF
029290**
029300         MOVE 受－被保険者カナ TO 被保険者カナＷ
029310         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
029320         MOVE 受－郵便番号１   TO 郵便番号１Ｗ
029330         MOVE 受－郵便番号２   TO 郵便番号２Ｗ
029340         MOVE 受－住所１       TO 被保険者住所１Ｗ
029350         MOVE 受－住所２       TO 被保険者住所２Ｗ
029360*         STRING 受－住所１    DELIMITED BY SPACE
029370*                受－住所２    DELIMITED BY SPACE
029380*                INTO 被保険者住所Ｗ
029390*         END-STRING
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
029400         MOVE 受－患者カナ     TO 患者カナＷ
029410         MOVE 受－患者氏名     TO 患者氏名Ｗ
029420         MOVE 受－費用負担者番号 TO 市町村番号Ｗ
029430         MOVE 受－受益者番号老人 TO 受給者番号Ｗ
029440*
029450         EVALUATE 受－患者性別
029460         WHEN 1
029470*             MOVE NC"男"  TO 性別Ｗ
029480             MOVE NC"○"  TO 男チェックＷ
029490         WHEN 2
029500*             MOVE NC"女"  TO 性別Ｗ
029510             MOVE NC"○"  TO 女チェックＷ
029520         END-EVALUATE
029530*
029540         EVALUATE 受－患者和暦
029550         WHEN 1
029560*             MOVE NC"明治"  TO 元号Ｗ
029570             MOVE NC"○"    TO 明治チェックＷ
029580         WHEN 2
029590*             MOVE NC"大正"  TO 元号Ｗ
029600             MOVE NC"○"    TO 大正チェックＷ
029610         WHEN 3
029620*             MOVE NC"昭和"  TO 元号Ｗ
029630             MOVE NC"○"    TO 昭和チェックＷ
029640         WHEN 4
029650*             MOVE NC"平成"  TO 元号Ｗ
029660             MOVE NC"○"    TO 平成チェックＷ
      */元号修正/20190426
023060         WHEN 5
                   MOVE "5令"   TO 令和ＣＭＷ
023070             MOVE NC"○"  TO 令和チェックＷ
029670         END-EVALUATE
029680*
      */元号修正/↓↓↓20190426
029310         IF 受－患者和暦 > 4
037370             MOVE 受－患者和暦     TO 元－元号区分
037380             READ 元号マスタ
037390             NOT INVALID KEY
037400                 MOVE 元－元号名称 TO 元号Ｗ
037410             END-READ
029330         END-IF
      */元号修正/↑↑↑20190426
029690         MOVE 受－患者年  TO 患者年Ｗ
029700         MOVE 受－患者月  TO 患者月Ｗ
029710         MOVE 受－患者日  TO 患者日Ｗ
029720*
029730* 続柄設定
029740         IF ( 本人家族区分ＷＲ = 1 )
029750            MOVE NC"本人"    TO 続柄Ｗ
029770         ELSE
029780            MOVE NC"家族"    TO 続柄Ｗ
029880         END-IF
029890**
029900         IF ( 受－保険種別 = 01 OR 08 OR 05) AND
029910            ( 受－助成種別 NOT = ZERO )
029920            PERFORM 助成レセまとめ判定
029930         ELSE
029940            MOVE SPACE TO 助成レセまとめフラグ
029950         END-IF
030250     END-IF.
030260*
030270     EVALUATE 保険種別ＷＲ
030280     WHEN 01
030290         IF 受－保険者番号(3:1) = "3"
030300             MOVE NC"国組" TO 保険種別名称Ｗ
030310         ELSE
030320             MOVE NC"国保" TO 保険種別名称Ｗ
030330         END-IF
030340*         MOVE NC"○" TO 国保チェックＷ
030350     WHEN 02
030360         MOVE NC"社保" TO 保険種別名称Ｗ
030370*         MOVE NC"○" TO 社保チェックＷ
030380     WHEN 03
030390         MOVE NC"組合" TO 保険種別名称Ｗ
030400*         MOVE NC"○" TO 組合チェックＷ
030410     WHEN 04
030420         MOVE NC"共済" TO 保険種別名称Ｗ
030430*         MOVE NC"○" TO 共済チェックＷ
030440     WHEN 06
030450         MOVE NC"社保" TO 保険種別名称Ｗ
030460*         MOVE NC"○" TO 社保チェックＷ
030440     WHEN 05
029980         IF ( 受－施術和暦年月 >= 42004 )
030450             MOVE NC"後高" TO 保険種別名称Ｗ
030460*             MOVE NC"○" TO 老人チェックＷ
               END-IF
030470     WHEN 07
030480         MOVE NC"船員" TO 保険種別名称Ｗ
030490*         MOVE NC"○" TO 船員チェックＷ
030500     WHEN 08
030510         MOVE NC"退国" TO 保険種別名称Ｗ
030520*         MOVE NC"○" TO 退職チェックＷ
030530     WHEN 09
030540         MOVE NC"自衛" TO 保険種別名称Ｗ
030550*         MOVE NC"○" TO 自衛チェックＷ
030560     END-EVALUATE.
030720*================================================================*
030730 請求先情報取得 SECTION.
030740*
030750****************************************************
030760* 連結データから保険者マスタより請求先を取得する。 *
030780* ● 請求先...... 請求先名称Ｗに格納               *
030790****************************************************
030800     MOVE 保険種別ＷＲ   TO 保－保険種別.
030810     MOVE 保険者番号ＷＲ TO 保－保険者番号.
030820     READ 保険者マスタ
030830     INVALID KEY
               IF ( 保険種別ＷＲ = 05 ) AND ( 施術和暦年月ＷＲ >= 42004 )
030800             MOVE 保険種別ＷＲ   TO 市－公費種別
030810             MOVE 保険者番号ＷＲ TO 市－市町村番号
030820             READ 市町村マスタ
030830             INVALID KEY
030840                 MOVE SPACE      TO 請求先名称Ｗ 請求先名称ＷＴ
030850             NOT INVALID KEY
031330                 MOVE 市－市町村名称    TO 請求先名称Ｗ
030920                 STRING 市－市町村名称      DELIMITED BY SPACE
030930                        "長"                DELIMITED BY SIZE
                            "　殿"                DELIMITED BY SIZE
030940                        INTO 請求先名称ＷＴ
030950                 END-STRING
                   END-READ
               ELSE
030840             MOVE SPACE      TO 請求先名称Ｗ 請求先名称ＷＴ
               END-IF
030850     NOT INVALID KEY
030870                 EVALUATE 保険種別ＷＲ 
030880                 WHEN  01
030890                 WHEN  07
030900                 WHEN  08
030910                     MOVE 保－保険者名称    TO 請求先名称Ｗ
030920                     STRING 保－保険者名称      DELIMITED BY SPACE
030930                            "長"                DELIMITED BY SIZE
                                "　殿"                DELIMITED BY SIZE
030940                            INTO 請求先名称ＷＴ
030950                     END-STRING
030860* 社保、日雇は「社会保険事務所」をつける
030960                 WHEN  02
030970                 WHEN  06
030980                     IF ( 保－接尾語区分 = 1 )
030990                        MOVE 保－保険者名称    TO 請求先名称Ｗ
031000                        STRING 保－保険者名称      DELIMITED BY SPACE
031010                               "長"                DELIMITED BY SIZE
                                   "　殿"                DELIMITED BY SIZE
031020                               INTO 請求先名称ＷＴ
031030                        END-STRING
031040                     ELSE
031050*                        STRING 保－保険者名称    DELIMITED BY SPACE
031060*                               "社会保険事務所"  DELIMITED BY SIZE
031070*                               INTO 請求先名称Ｗ
031080*                        END-STRING
031090                        STRING 保－保険者名称      DELIMITED BY SPACE
031100                               "社会保険事務所長"  DELIMITED BY SIZE
                                   "　殿"                DELIMITED BY SIZE
031110                               INTO 請求先名称ＷＴ
031120                        END-STRING
031130                     END-IF
031140* 組合は支部名まで印字
031150                 WHEN  03
031160                     STRING 保－保険者名称  DELIMITED BY SPACE
031170                            "健康保険組合"  DELIMITED BY SIZE
031180                            "  "            DELIMITED BY SIZE
031190                            保－支部部署名  DELIMITED BY SPACE
                               "　殿"             DELIMITED BY SIZE
031200                            INTO 請求先名称Ｗ
031210                     END-STRING
031220                     MOVE 請求先名称Ｗ  TO 請求先名称ＷＴ
031230* 共済は支部名まで印字
031240                 WHEN  04
031250                     STRING 保－保険者名称  DELIMITED BY SPACE
031260                            "共済組合"      DELIMITED BY SIZE
031270                            "  "            DELIMITED BY SIZE
031280                            保－支部部署名  DELIMITED BY SPACE
                               "　殿"             DELIMITED BY SIZE
031290                            INTO 請求先名称Ｗ
031300                     END-STRING
031310                     MOVE 請求先名称Ｗ  TO 請求先名称ＷＴ
031320                 WHEN OTHER
031330                     MOVE 保－保険者名称    TO 請求先名称Ｗ 請求先名称ＷＴ
                           STRING 請求先名称Ｗ DELIMITED BY SPACE
                                  "　殿"       DELIMITED BY SIZE
                                  INTO 請求先名称Ｗ
                           END-STRING
031310                     MOVE 請求先名称Ｗ  TO 請求先名称ＷＴ
031340                 END-EVALUATE
031350     END-READ.
031360*
031370*================================================================*
031380 負傷データ取得 SECTION.
031390*
031400**************************************************
031410* 連結データから負傷データＦより以下の情報を取得 *
031420* ● 負傷名...部位＋負傷種別にて加工して格納     *
031430* ● 負傷年.......負傷年Ｗ                       *
031440* ● 負傷月.......負傷月Ｗ                       *
031450* ● 負傷日.......負傷日Ｗ                       *
031460* ● 開始年.......初検年Ｗ                       *
031470* ● 開始月.......初検月Ｗ                       *
031480* ● 開始日.......初検日Ｗ                       *
031490* ● 終了年.......終了年Ｗ                       *
031500* ● 終了月.......終了月Ｗ                       *
031510* ● 終了日.......終了日Ｗ                       *
031520* ● 実日数.......実日数Ｗ                       *
031530* ● 転帰区分 ....区分によりチェックに"○"を格納 *
031540* ● 金属副子 ....区分によりチェックに"○"を格納 *
031550* ● 経過コード...経過マスタより取得             *
031560**************************************************
           IF 負－レコード NOT = SPACE
031660         MOVE 負－部位数                   TO 部位数Ｗ
031670         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
031680                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
031690             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別Ｗ(部位ＣＮＴ)
031700             MOVE 負－部位(部位ＣＮＴ)     TO 部位Ｗ(部位ＣＮＴ)
031710             MOVE 負－左右区分(部位ＣＮＴ) TO 左右区分Ｗ(部位ＣＮＴ)
031720             MOVE 負－負傷位置番号(部位ＣＮＴ)
031730                                           TO 負傷位置番号Ｗ(部位ＣＮＴ)
031740*********************************************
031750* 注）全柔...負傷種別＋部位にて加工して格納 *
031760*********************************************
031770* 負傷種別
031780             MOVE SPACE                     TO 負傷名称Ｗ
031790             MOVE 03                        TO 名－区分コード
031800             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
031810             READ 名称マスタ
031820             INVALID KEY
031830                 MOVE SPACE        TO 負傷名称Ｗ
031840             NOT INVALID KEY
031850                 MOVE 名－正式名称 TO 負傷名称Ｗ
031860             END-READ
031870* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
032680*
032690             PERFORM 部位名称埋込処理
030170*
032070             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
032080             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
032090             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
032100             MOVE 負－開始年(部位ＣＮＴ)   TO 初検年Ｗ(部位ＣＮＴ)
032110             MOVE 負－開始月(部位ＣＮＴ)   TO 初検月Ｗ(部位ＣＮＴ)
032120             MOVE 負－開始日(部位ＣＮＴ)   TO 初検日Ｗ(部位ＣＮＴ)
032130             IF ( 負－転帰区分(部位ＣＮＴ) = 9 )
032140                 MOVE 99                   TO 終了年Ｗ(部位ＣＮＴ)
032150                 MOVE 99                   TO 終了月Ｗ(部位ＣＮＴ)
032160                 MOVE 99                   TO 終了日Ｗ(部位ＣＮＴ)
032170             ELSE
032180                 MOVE 負－終了年(部位ＣＮＴ)   TO 終了年Ｗ(部位ＣＮＴ)
032190                 MOVE 負－終了月(部位ＣＮＴ)   TO 終了月Ｗ(部位ＣＮＴ)
032200                 MOVE 負－終了日(部位ＣＮＴ)   TO 終了日Ｗ(部位ＣＮＴ)
032210             END-IF
032220* 経過略称取得
032230             MOVE 01                         TO 経－区分コード
032240             MOVE 負－経過コード(部位ＣＮＴ) TO 経－経過コード
032250             READ 経過マスタ
032260             INVALID KEY
032270                 MOVE ZERO            TO 部位ＣＮＴＷ(部位ＣＮＴ)
032280                 MOVE SPACE           TO 部位区切Ｗ(部位ＣＮＴ)
032290                 MOVE SPACE           TO 経過略称Ｗ(部位ＣＮＴ)
032300             NOT INVALID KEY
032310*
032320                 EVALUATE 部位ＣＮＴ
032330                 WHEN 1
032340                     MOVE NC"①" TO 経過部位Ｗ
032350                 WHEN 2
032360                     MOVE NC"②" TO 経過部位Ｗ
032370                 WHEN 3
032380                     MOVE NC"③" TO 経過部位Ｗ
032390                 WHEN 4
032400                     MOVE NC"④" TO 経過部位Ｗ
032410                 WHEN 5
032420                     MOVE NC"⑤" TO 経過部位Ｗ
032430                 END-EVALUATE
032440                 STRING  経過部位Ｗ     DELIMITED BY SPACE
032450                         経－経過略称   DELIMITED BY SPACE
032460                        INTO 印刷経過略称Ｗ(部位ＣＮＴ)
032470                 END-STRING
032480*
032490             END-READ
032500*
032510             MOVE 負－転帰区分(部位ＣＮＴ) TO 転帰区分Ｗ(部位ＣＮＴ)
032520             EVALUATE 負－転帰区分(部位ＣＮＴ)
032530             WHEN 1
032540             WHEN 2
032550                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
032560             WHEN 3
032570                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
032580             WHEN 4
032590                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
032600             END-EVALUATE
032610*
                   MOVE レセ－部位実日数(部位ＣＮＴ) TO 実日数Ｗ(部位ＣＮＴ)
032620         END-PERFORM
032630* 新規/継続 チェック
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
032690*
032700* 枝番判定用
032710         MOVE 負－開始診療日手動区分   TO 開始診療日手動区分Ｗ
032720* 負傷原因印刷区分
032730         MOVE 負－レセ負傷原因印刷区分 TO レセ負傷原因印刷区分Ｗ
027880         MOVE 負－レセ長期理由印刷区分 TO レセ長期理由印刷区分Ｗ
032740*
032750     END-IF.
032760*================================================================*
030910 部位名称埋込処理 SECTION.
030920*
006490     STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
009980            負傷名称Ｗ                    DELIMITED BY SPACE
006500            レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
006520       INTO 負傷名Ｗ(部位ＣＮＴ)
006570     END-STRING.
031050*
032770*================================================================*
032780 施術記録取得 SECTION.
032790*
032800************************************************************
032810* 作１データから負傷データＦより以下の情報を取得           *
032820* ● 初検加算 .....区分によりチェックに"○"を格納...複数可 *
032830* ● 往療加算 .....区分によりチェックに"○"を格納...複数可 *
032840************************************************************
032850     MOVE  SPACE  TO  初日再検フラグ.
032860     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 部位数Ｗ
032870         IF ( 施術年Ｗ = 初検年Ｗ(部位ＣＮＴ) ) AND
032880            ( 施術月Ｗ = 初検月Ｗ(部位ＣＮＴ) )
032890             MOVE 患者番号ＷＲ          TO 施記－患者番号
032900             MOVE 枝番ＷＲ              TO 施記－枝番
032910             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
032920             MOVE 初検年Ｗ(部位ＣＮＴ)  TO 開始年Ｗ(部位ＣＮＴ) 施記－施術年
032930             MOVE 初検月Ｗ(部位ＣＮＴ)  TO 開始月Ｗ(部位ＣＮＴ) 施記－施術月
032940             MOVE 初検日Ｗ(部位ＣＮＴ)  TO 開始日Ｗ(部位ＣＮＴ) 施記－施術日
032950         ELSE
032960             MOVE 患者番号ＷＲ          TO 施記－患者番号
032970             MOVE 枝番ＷＲ              TO 施記－枝番
032980             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
032990             MOVE 施術年ＷＲ            TO 施記－施術年
033000             MOVE 施術月ＷＲ            TO 施記－施術月
033010             MOVE ZERO                  TO 施記－施術日
033020         END-IF
033030         START 施術記録Ｆ   KEY IS >= 施記－患者コード
033040                                      施記－施術和暦年月日
033050         END-START
033060         IF ( 状態キー = "00" )
033080             MOVE ZERO  TO 終了年ＷＴ
033090             MOVE ZERO  TO 終了月ＷＴ
033100             MOVE ZERO  TO 終了日ＷＴ
033110             MOVE SPACE TO 終了フラグ２
033120             PERFORM 施術記録Ｆ読込
033130             IF ( 終了フラグ２      = SPACE   ) AND
033140                ( 施記－患者コード  = 患者コードＷＲ ) AND
033150                ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
033160                ( 施記－施術年      = 施術年ＷＲ     ) AND
033170                ( 施記－施術月      = 施術月ＷＲ     ) 
033180*
033190*        *****************************************************************
033200*        * 開始年月日 ( その部位が当月初検でないか、
033210*                       当月初検でも枝番がある時は、最初の施術日を開始日)*
033220*        *****************************************************************
033230                 IF ( 施術年Ｗ NOT = 初検年Ｗ(部位ＣＮＴ) ) OR
033240                    ( 施術月Ｗ NOT = 初検月Ｗ(部位ＣＮＴ) ) OR
033250                    ( 開始診療日手動区分Ｗ = 1 )
033260                     MOVE 施記－施術年   TO 開始年Ｗ(部位ＣＮＴ)
033270                     MOVE 施記－施術月   TO 開始月Ｗ(部位ＣＮＴ)
033280                     MOVE 施記－施術日   TO 開始日Ｗ(部位ＣＮＴ)
033290                 END-IF
033300             END-IF
033310             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
033320                           ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
033330                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
033340                           ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
033350                           ( 施記－施術月     NOT = 施術月ＷＲ       ) OR
033360                           ( 施記－施術日         > 終了日Ｗ(部位ＣＮＴ))
033370*               **********
033380*               * 実日数 *
033390*               **********
033410                MOVE 施記－施術年               TO 終了年ＷＴ
033420                MOVE 施記－施術月               TO 終了月ＷＴ
033430                MOVE 施記－施術日               TO 終了日ＷＴ
033440*
033450                PERFORM 施術記録Ｆ読込
033460            END-PERFORM
033470        END-IF
033480*       **************************
033490*       * 継続：終了年月日セット *
033500*       **************************
033510        IF ( 転帰区分Ｗ(部位ＣＮＴ) = 9 )
033520            MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
033530            MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
033540            MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
033550        END-IF
033560        IF ( 終了年月日Ｗ(部位ＣＮＴ) > 受理年月日Ｗ )
033570            MOVE 終了年Ｗ(部位ＣＮＴ) TO 受理年Ｗ
033580            MOVE 終了月Ｗ(部位ＣＮＴ) TO 受理月Ｗ
033590            MOVE 終了日Ｗ(部位ＣＮＴ) TO 受理日Ｗ
033600        END-IF
033610     END-PERFORM.
033620*
033630** ----- 前月初検のみかを判定 -----------*
033640*
033650*     MOVE 患者番号ＷＲ          TO 施記－患者番号.
033660*     MOVE 枝番ＷＲ              TO 施記－枝番.
033670*     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
033680*     MOVE 施術年ＷＲ            TO 施記－施術年.
033690*     MOVE 施術月ＷＲ            TO 施記－施術月.
033700*     MOVE ZERO                  TO 施記－施術日.
033710*     START 施術記録Ｆ   KEY IS >= 施記－患者コード
033720*                                  施記－施術和暦年月日
033730*     END-START.
033740*     IF ( 状態キー = "00" )
033750*             MOVE SPACE TO 終了フラグ２
033760*             PERFORM 施術記録Ｆ読込
033770*             IF ( 終了フラグ２      = SPACE   ) AND
033780*                ( 施記－患者コード  = 患者コードＷＲ ) AND
033790*                ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
033800*                ( 施記－施術年      = 施術年ＷＲ     ) AND
033810*                ( 施記－施術月      = 施術月ＷＲ     ) 
033820** 当月施術開始日が再検かどうか判定
033830*                 IF ( 施記－再検料請求 = 1 )
033840*                      MOVE "YES"  TO  初日再検フラグ
033850*                 END-IF
033860**
033870*             END-IF
033880*     END-IF.
033890*     IF ( 初日再検フラグ = "YES" )
033900*        PERFORM 前月初検のみ判定
033910*     END-IF.
033920*
033930*================================================================*
033940*================================================================*
033950 レセプト並び順取得 SECTION.
033960*================================================================*
033970     MOVE 施術和暦ＷＲ       TO 作４－施術和暦.
033980     MOVE 施術年ＷＲ         TO 作４－施術年.
033990     MOVE 施術月ＷＲ         TO 作４－施術月.
034000     MOVE 患者コードＷＲ     TO 作４－患者コード.
034010     MOVE 保険種別ＷＲ       TO 作４－保険種別.
034020     READ 作業ファイル４
034030     NOT INVALID KEY
034040          MOVE 作４－順番    TO 順番Ｗ
034050     END-READ.
034060     MOVE "DNo.=    :"       TO 順番固定Ｗ.
034070*
034080*================================================================*
034090 施術記録Ｆ読込 SECTION.
034100*
034110     READ 施術記録Ｆ NEXT
034120     AT END
034130         MOVE "YES" TO 終了フラグ２
034140     END-READ.
034150*================================================================*
034160 印刷処理 SECTION.
034170*
034180     MOVE "YJK6125P"  TO  定義体名Ｐ.
034190     MOVE "SCREEN"   TO  項目群名Ｐ.
034200     WRITE YJK6125P.
034210***     WRITE 印刷レコード.
034220     PERFORM エラー処理Ｐ.
034230*================================================================*
034240 エラー処理Ｐ SECTION.
034250*
034260     IF ( 通知情報Ｐ NOT = "00" )
034270         DISPLAY NC"帳票エラー"              UPON CONS
034280         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
034290         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
034300         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
034310         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
034320                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
034330         ACCEPT  キー入力 FROM CONS
034340         PERFORM ファイル閉鎖
034350         MOVE 99 TO PROGRAM-STATUS
034360         EXIT PROGRAM
034370     END-IF.
034610*================================================================*
034620 初検日以前のデータ判定 SECTION.
034630*
034640*********************************************************************************
034650*  最初の初検日以前の当月中に施術記録レコードがあった時(治癒、中止)は、請求区分の
034660*  継続にもチェックする。(新規と継続の両方)
034670*********************************************************************************
034680** 最初の初検日を取得
034690     MOVE SPACE                 TO 初検フラグ.
034700     MOVE 患者番号ＷＲ          TO 施記－患者番号.
034710     MOVE 枝番ＷＲ              TO 施記－枝番.
034720     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
034730     MOVE 施術年ＷＲ            TO 施記－施術年.
034740     MOVE 施術月ＷＲ            TO 施記－施術月.
034750     MOVE ZERO                  TO 施記－施術日.
034760     START 施術記録Ｆ   KEY IS >= 施記－患者コード
034770                                  施記－施術和暦年月日
034780     END-START.
034790     IF ( 状態キー = "00" )
034800         MOVE ZERO  TO 初検和暦ＷＴ
034810         MOVE ZERO  TO 初検年ＷＴ
034820         MOVE ZERO  TO 初検月ＷＴ
034830         MOVE ZERO  TO 初検日ＷＴ
034840         MOVE SPACE TO 終了フラグ２
034850         PERFORM 施術記録Ｆ読込
034860         PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
034870                       ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
034880                       ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
034890                       ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
034900                       ( 施記－施術月     NOT = 施術月ＷＲ      ) OR
034910                       ( 初検フラグ           = "YES"           ) 
034920               IF ( 施記－診療区分 = 2 )
034930                   MOVE 施記－施術和暦           TO 初検和暦ＷＴ
034940                   MOVE 施記－施術年             TO 初検年ＷＴ
034950                   MOVE 施記－施術月             TO 初検月ＷＴ
034960                   MOVE 施記－施術日             TO 初検日ＷＴ
034970                   MOVE "YES"                    TO 初検フラグ
034980               END-IF
034990               PERFORM 施術記録Ｆ読込
035000         END-PERFORM
035010     END-IF.
035020*
035030* 初検日以前のデータ判定
035040     IF ( 初検フラグ = "YES" )
035050        MOVE 患者番号ＷＲ          TO 施記－患者番号
035060        MOVE 枝番ＷＲ              TO 施記－枝番
035070        MOVE 初検和暦ＷＴ          TO 施記－施術和暦
035080        MOVE 初検年ＷＴ            TO 施記－施術年
035090        MOVE 初検月ＷＴ            TO 施記－施術月
035100        MOVE 初検日ＷＴ            TO 施記－施術日
035110        START 施術記録Ｆ   KEY IS <  施記－患者コード
035120                                     施記－施術和暦年月日
035130                                     REVERSED
035140        END-START
035150        IF ( 状態キー = "00" )
035160           MOVE SPACE  TO 終了フラグ２
035170           PERFORM 施術記録Ｆ読込
035180           IF ( 終了フラグ２    = SPACE        ) AND
035190              ( 施記－患者番号  = 患者番号ＷＲ ) AND
035200              ( 施記－枝番      = 枝番ＷＲ     ) AND
035210              ( 施記－施術和暦  = 初検和暦ＷＴ ) AND
035220              ( 施記－施術年    = 初検年ＷＴ   ) AND
035230              ( 施記－施術月    = 初検月ＷＴ   )
035240*  初検日以前の当月中に施術記録レコードがあった時
035250                IF ( 継続チェックＷ = SPACE )
035260                   MOVE NC"○"    TO 継続チェックＷ
035270                END-IF
035280           END-IF
035290         END-IF
035300     END-IF.
035310*
035320*================================================================*
035330 長期判定取得 SECTION.
035340*
035350* ３カ月以上の長期判定は "CHOUKI" を呼ぶ. 
035360     MOVE  SPACE TO  連期間－キー.
035370     INITIALIZE      連期間－キー.
035380     MOVE 施術和暦ＷＲ  TO  連期間－施術和暦.
035390     MOVE 施術年ＷＲ    TO  連期間－施術年.
035400     MOVE 施術月ＷＲ    TO  連期間－施術月.
035410     MOVE 患者番号ＷＲ  TO  連期間－患者番号.
035420     MOVE 枝番ＷＲ      TO  連期間－枝番.
035430*
035440     CALL   "CHOUKI".
035450     CANCEL "CHOUKI".
035460*
035470**** 適用１を使用 (「前月初検のみ」がある時は、くっつける)
035480     IF ( 連期間－対象フラグ  = "YES" )
035490        IF ( 適用１Ｗ  = SPACE )
035500           MOVE NC"※長期施術継続理由裏面に記載"  TO 適用１Ｗ
035510        ELSE
035520           STRING 適用１Ｗ           DELIMITED BY SPACE
035530                  NC"，"             DELIMITED BY SIZE
035540                  NC"※長期施術継続理由裏面に記載"   DELIMITED BY SIZE
035550                  INTO 適用１Ｗ
035560           END-STRING
035570        END-IF
035580     END-IF.
035590*
035600*================================================================*
035610 初検加算時刻取得 SECTION.
035620*****************************************************************
035630** 初検加算が時間外と深夜の時、適用に「受付時間」を印字する。
035640**   時刻の印字は月3回まで可能
035650*****************************************************************
035660     IF ( レセ－時間外 = 1 ) OR ( レセ－深夜 = 1 ) OR ( レセ－休日 = 1 )
035670*
035680         MOVE 患者番号ＷＲ          TO 施記－患者番号
035690         MOVE 枝番ＷＲ              TO 施記－枝番
035700         MOVE 施術和暦ＷＲ          TO 施記－施術和暦
035710         MOVE 施術年ＷＲ            TO 施記－施術年
035720         MOVE 施術月ＷＲ            TO 施記－施術月
035730         MOVE ZERO                  TO 施記－施術日
035740         START 施術記録Ｆ   KEY IS >= 施記－患者コード
035750                                      施記－施術和暦年月日
035760         END-START
035770         IF ( 状態キー = "00" )
035780             MOVE ZERO  TO 初検加算カウント
035790             MOVE SPACE TO 終了フラグ２
035800             PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
035810                           ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
035820                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
035830                           ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
035840                           ( 施記－施術月     NOT = 施術月ＷＲ      ) 
035850               IF ( 施記－初検加算 = 1 OR 2 OR 3 ) AND ( 施記－診療区分 = 2 )
035860                  COMPUTE 初検加算カウント = 初検加算カウント  + 1
035870                  IF ( 初検加算カウント <= 3 )
035880                     MOVE 施記－初検加算 TO 初検加算区分ＷＴ(初検加算カウント)
035890                     MOVE 施記－受付時   TO 初検加算時ＷＴ(初検加算カウント)
035900                     MOVE 施記－受付分   TO 初検加算分ＷＴ(初検加算カウント)
035910                  END-IF
035920               END-IF
035930               PERFORM 施術記録Ｆ読込
035940            END-PERFORM
035950** 初検加算の時刻を適用にセット
033380            IF ( 初検加算時ＷＴ(1) NOT = ZERO ) OR ( 初検加算分ＷＴ(1) NOT = ZERO ) 
                     MOVE 初検加算時ＷＴ(1) TO 初検加算時Ｗ
                     MOVE ":"               TO 初検加算区切Ｗ
                     MOVE 初検加算分ＷＴ(1) TO 初検加算分Ｗ
                  END-IF
033380            IF ( 初検加算時ＷＴ(2) NOT = ZERO ) OR ( 初検加算分ＷＴ(2) NOT = ZERO ) 
031910               PERFORM 初検加算適用セット
                  END-IF
035970         END-IF
035980*
035990     END-IF.
036000*
036010*================================================================*
036020 初検加算適用セット SECTION.
036030*
036040     PERFORM VARYING 番号カウンタ FROM 1 BY 1
036050              UNTIL  番号カウンタ > 3
036060         IF ( 初検加算時ＷＴ(番号カウンタ)  = ZERO )  AND 
036070            ( 初検加算分ＷＴ(番号カウンタ)  = ZERO ) 
036080             CONTINUE
036090         ELSE
036100* 固定項目
036110             EVALUATE 初検加算区分ＷＴ(番号カウンタ) 
036120             WHEN 1
036130                MOVE NC"時間外"   TO 加算内容Ｗ(番号カウンタ)
033320             WHEN 2
033330                MOVE NC"休　日"   TO 加算内容Ｗ(番号カウンタ)
036140             WHEN 3
036150                MOVE NC"深　夜"   TO 加算内容Ｗ(番号カウンタ)
036160             END-EVALUATE
036170*
036180             MOVE NC"："          TO 加算区切Ｗ(番号カウンタ)
036190             MOVE NC"時"          TO 時固定Ｗ(番号カウンタ)
036200             MOVE NC"分"          TO 分固定Ｗ(番号カウンタ)
036210*
036220**** 数字→日本語変換
036230* 時間
036240             MOVE 初検加算時ＷＴ(番号カウンタ)  TO  数字Ｗ
036250             IF ( 数字Ｗ >= 10 )
036260                 MOVE 数字Ｗ１    TO 負傷番号Ｗ１
036270                 PERFORM 日本語変換
036280                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ１(番号カウンタ)
036290                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
036300                 PERFORM 日本語変換
036310                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
036320             ELSE
036330                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
036340                 PERFORM 日本語変換
036350                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
036360             END-IF
036370* 分
036380             MOVE 初検加算分ＷＴ(番号カウンタ)  TO  数字Ｗ
036390             MOVE 数字Ｗ１    TO 負傷番号Ｗ１
036400             PERFORM 日本語変換
036410             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ１(番号カウンタ)
036420             MOVE 数字Ｗ２    TO 負傷番号Ｗ１
036430             PERFORM 日本語変換
036440             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ２(番号カウンタ)
036450** 
036460        END-IF
036470     END-PERFORM.
036480*
036490     MOVE  初検加算集団ＮＷ(1)   TO 初検加算時刻１Ｗ. 
036500     MOVE  初検加算集団ＮＷ(2)   TO 初検加算時刻２Ｗ. 
036510     MOVE  初検加算集団ＮＷ(3)   TO 初検加算時刻３Ｗ. 
036520*
036530**** 適用１か２を使用（長期理由記載で適用１を使っている時は、適用２）
036540     IF ( 初検加算時ＷＴ(2)  = ZERO ) AND ( 初検加算分ＷＴ(2)  = ZERO ) 
036550         CONTINUE
036560     ELSE
036570         IF ( 適用１Ｗ  = SPACE )
036580               STRING NC"初検加算"       DELIMITED BY SIZE
036590                      初検加算時刻１Ｗ   DELIMITED BY SIZE
036600                      初検加算時刻２Ｗ   DELIMITED BY SIZE
036610                      初検加算時刻３Ｗ   DELIMITED BY SIZE
036620                      INTO 適用１Ｗ
036630               END-STRING
036640         ELSE
036650               STRING NC"初検加算"       DELIMITED BY SIZE
036660                      初検加算時刻１Ｗ   DELIMITED BY SIZE
036670                      初検加算時刻２Ｗ   DELIMITED BY SIZE
036680                      初検加算時刻３Ｗ   DELIMITED BY SIZE
036690                      INTO 適用２Ｗ
036700               END-STRING
036710         END-IF
036720     END-IF.
036730*
036740*================================================================*
036750 日本語変換 SECTION.
036760*
036770     MOVE NC"０"     TO 全角負傷番号Ｗ.
036780     CALL "htoz" WITH C LINKAGE
036790                        USING 負傷番号Ｗ１ 全角負傷番号Ｗ１.
036800*
036810*================================================================*
036820*================================================================*
036830 負傷原因取得 SECTION.
036840*
036850********************************************************************
036860*  負傷原因コードが同じものは、1行にまとめて印字する。
036870*  例: ①② 家で転んだ.
036880*     負傷原因コードが同じものをまとめ、テーブルにセット
036890*     (ただし、部位を飛んで同じものは、2行になる)
036900********************************************************************
036910     MOVE  ZERO   TO  カウンタ カウンタ２.
036920     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
036930             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
036940*
036950****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
036960        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
036970*
036980           IF ( カウンタ = ZERO )
036990               MOVE 1   TO  カウンタ カウンタ２
037000               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
037010               MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
037020               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
037030           ELSE
037040              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
037050                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
037060                 COMPUTE カウンタ２ = カウンタ２  +  1
037070                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
037080              ELSE
037090                 COMPUTE カウンタ = カウンタ  +  1
037100                 MOVE 1   TO  カウンタ２
037110                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
037120                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
037130                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
037140              END-IF
037150           END-IF
037160        END-IF
037170     END-PERFORM.
037180**************************************************************************
037190*  負傷原因マスタより文章取得
037200**************************************************************************
037210     MOVE  ZERO   TO  カウンタ カウンタ２.
037220     PERFORM VARYING カウンタ FROM 1 BY 1
037230             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
037240** 健保は 区分 01
037250         MOVE 01                        TO 負原－区分コード
037260         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
037270         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
037280         READ 負傷原因Ｆ
037290         NOT INVALID KEY
037300             INITIALIZE 負傷原因ＷＴ
037310             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
037320             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
037330             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
037340             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
037350             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
037360             PERFORM VARYING カウンタ２ FROM 1 BY 1
037370                     UNTIL ( カウンタ２ > 9 )  OR 
037380                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
037390                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
037400                WHEN 1
037410                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
037420                WHEN 2
037430                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
037440                WHEN 3
037450                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
037460                WHEN 4
037470                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
037480                WHEN 5
037490                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
037460                WHEN 6
037470                   MOVE "⑥"  TO  負傷原因ナンバーＷ１(カウンタ２)
037480                WHEN 7
037490                   MOVE "⑦"  TO  負傷原因ナンバーＷ１(カウンタ２)
037500                WHEN OTHER
037510                   CONTINUE
037520                END-EVALUATE
037530             END-PERFORM
037540*
037550             IF 負原－負傷原因入力区分 = 1
037560                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
037570                        負傷原因１ＷＴ  DELIMITED BY SIZE
037580                        負傷原因２ＷＴ  DELIMITED BY SIZE
037590                        負傷原因３ＷＴ  DELIMITED BY SIZE
037600                        負傷原因４ＷＴ  DELIMITED BY SIZE
037610                        負傷原因５ＷＴ  DELIMITED BY SIZE
037620                        INTO 負傷原因内容合成Ｗ(カウンタ)
037630                 END-STRING
037640             ELSE
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
037730             END-IF
037740*
037750         END-READ
037760     END-PERFORM.
037770*
037780     PERFORM 負傷原因セット.
037790*
037800*================================================================*
037810 負傷原因セット SECTION.
037820*
037830**************************************************************************
037840*  文章が1行を超える時は、複数行に分解する。
037850**************************************************************************
037860     MOVE  ZERO   TO  カウンタ カウンタ２.
037870     PERFORM VARYING カウンタ FROM 1 BY 1
037880             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
037890*
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
038000*
038010     END-PERFORM.
038180*================================================================*
038190 前月初検のみ判定 SECTION.
038200*
038210*** 前月の通院日が初検か判定 
038220     MOVE  SPACE            TO 前月フラグ.
038230     MOVE 受－患者コード    TO 施記－患者コード.
038240     MOVE 受－施術和暦      TO 施記－施術和暦.
038250     MOVE 受－施術年        TO 施記－施術年.
038260     MOVE 受－施術月        TO 施記－施術月.
038270     MOVE 1                 TO 施記－施術日.
038280     START 施術記録Ｆ   KEY IS <  施記－患者コード
038290                                  施記－施術和暦年月日
038300                                  REVERSED
038310     END-START.
038320     IF ( 状態キー = "00" )
038330         MOVE SPACE  TO 終了フラグ２
038340         PERFORM 施術記録Ｆ読込
038350         IF ( 終了フラグ２      = SPACE  ) AND
038360            ( 施記－患者コード  = 受－患者コード ) AND
038370            ( 施記－診療区分    = 2 ) 
038380*
038390            PERFORM 前月判定
038400**** 適用１を使用
038410            IF ( 前月フラグ = "YES" )
038420               MOVE NC"※前月初検のみ"    TO  適用１Ｗ
038430            END-IF
038440**
038450         END-IF
038460     END-IF.
038470*
038480*================================================================*
038490 前月判定  SECTION.
038500* 
038510*** 読み込んだ施術記録の年月が、前月かどうか判定 (年月の差が 1 か?)
038520      MOVE  SPACE  TO  前月フラグ.
038530      INITIALIZE  計算年月日Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
038540**
038550      MOVE 受－施術和暦    TO 終了和暦２Ｗ.
038560      MOVE 受－施術年      TO 終了年２Ｗ.
038570      MOVE 受－施術月      TO 終了月２Ｗ.
038580      MOVE 施記－施術和暦  TO 開始和暦２Ｗ.
038590      MOVE 施記－施術年    TO 開始年２Ｗ.
038600      MOVE 施記－施術月    TO 開始月２Ｗ.
038610*
038620      EVALUATE TRUE
038630       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ = 終了年２Ｗ)
038640            PERFORM  前月比較月
038650       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ NOT = 終了年２Ｗ)
038660            PERFORM  前月比較年
038670       WHEN  開始和暦２Ｗ NOT = 終了和暦２Ｗ 
038680            PERFORM  前月比較元号
038690      END-EVALUATE.
038700*
038710      IF ( 計算月Ｗ = 1 )
038720         MOVE  "YES"  TO  前月フラグ
038730      END-IF.
038740*
038750*================================================================*
038760 前月比較月  SECTION.
038770*
038780     IF ( 終了月２Ｗ >  開始月２Ｗ )
038790         COMPUTE 計算月Ｗ = 終了月２Ｗ - 開始月２Ｗ
038800     ELSE
038810        MOVE ZERO TO 計算月Ｗ
038820     END-IF.
038830*
038840*================================================================*
038850 前月比較年  SECTION.
038860*
038870     IF ( 終了年２Ｗ >  開始年２Ｗ )
038880         COMPUTE 計算年Ｗ = 終了年２Ｗ - 開始年２Ｗ
038890         COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
038900     ELSE
038910        MOVE ZERO TO 計算月Ｗ
038920     END-IF.
038930*
038940*================================================================*
038950 前月比較元号  SECTION.
038960*
038970     MOVE 開始和暦２Ｗ TO 元－元号区分.
038980     READ 元号マスタ
038990     NOT INVALID KEY
039000         MOVE 元－開始西暦年 TO 開始西暦年Ｗ
039010     END-READ.
039020     MOVE 終了和暦２Ｗ TO 元－元号区分.
039030     READ 元号マスタ
039040     NOT INVALID KEY
039050         MOVE 元－開始西暦年 TO 終了西暦年Ｗ
039060     END-READ.
039070**
039080     IF ( 開始西暦年Ｗ NOT = ZERO ) AND ( 終了西暦年Ｗ NOT = ZERO )
039090        COMPUTE 開始西暦年Ｗ = 開始西暦年Ｗ + 開始年２Ｗ - 1
039100        COMPUTE 終了西暦年Ｗ = 終了西暦年Ｗ + 終了年２Ｗ - 1
039110*
039120        IF ( 終了西暦年Ｗ =  開始西暦年Ｗ )
039130           PERFORM  前月比較月
039140        ELSE
039150           IF ( 終了西暦年Ｗ >  開始西暦年Ｗ )
039160               COMPUTE 計算年Ｗ = 終了西暦年Ｗ - 開始西暦年Ｗ
039170               COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
039180           ELSE
039190               MOVE ZERO TO 計算月Ｗ
039200           END-IF
039210        END-IF
039220     ELSE
039230        MOVE ZERO TO 計算月Ｗ
039240     END-IF.
039250*================================================================*
039260 長期理由文取得 SECTION.
039270*
039280* 長期理由文取得は "CHOUBUN" を呼ぶ. 
039290     MOVE  SPACE TO  連長文－キー.
039300     INITIALIZE      連長文－キー.
039310     MOVE 施術和暦ＷＲ  TO  連長文－施術和暦.
039320     MOVE 施術年ＷＲ    TO  連長文－施術年.
039330     MOVE 施術月ＷＲ    TO  連長文－施術月.
039340     MOVE 患者番号ＷＲ  TO  連長文－患者番号.
039350     MOVE 枝番ＷＲ      TO  連長文－枝番.
039370     MOVE 61            TO  連長文－文桁数.
039370     MOVE 56            TO  連長文－文桁数.
039380*
039390     CALL   "CHOUBUN".
039400     CANCEL "CHOUBUN".
039410*
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
046490*================================================================*
046500 レセ摘要再セット SECTION.
046510*================================================================*
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
046680*
039420*================================================================*
039430 受診者印刷区分更新 SECTION.
039440*
039450** //  受診者情報Ｆの印刷区分に１をセットし、更新する。//  
039460*
039470     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
039480     MOVE 施術年ＷＲ         TO 受－施術年.
039490     MOVE 施術月ＷＲ         TO 受－施術月.
039500     MOVE 患者コードＷＲ     TO 受－患者コード.
039510     READ 受診者情報Ｆ
039520     NOT INVALID KEY
039530         MOVE  1  TO  受－レセ印刷区分
039540         REWRITE  受－レコード
039550         END-REWRITE
039560         IF ( 状態キー NOT = "00" )
039570            MOVE NC"受診者" TO ファイル名
039580            PERFORM エラー表示
039590         END-IF
039600     END-READ.
039610*
039620*================================================================*
039630 月末日取得 SECTION.
039640*
039650     MOVE 施術年ＷＲ   TO 受理年Ｗ.
039660     MOVE 施術月ＷＲ   TO 受理月Ｗ.
039670     MOVE 施術和暦ＷＲ TO 元－元号区分.
039680     READ 元号マスタ
039690     NOT INVALID KEY
039700         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
039710     END-READ.
039720     IF ( 施術西暦年Ｗ NOT = ZERO )
039730        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
039740     END-IF.
039750*
039760     EVALUATE 施術月ＷＲ
039770     WHEN 4
039780     WHEN 6
039790     WHEN 9
039800     WHEN 11
039810         MOVE 30 TO 受理日Ｗ
039820     WHEN 2
039830         DIVIDE 4 INTO 施術西暦年Ｗ GIVING    商Ｗ
039840                                    REMAINDER 余Ｗ
039850         END-DIVIDE
039860         IF ( 余Ｗ = ZERO )
039870             MOVE 29 TO 受理日Ｗ
039880         ELSE
039890             MOVE 28 TO 受理日Ｗ
039900         END-IF
039910     WHEN 1
039920     WHEN 3
039930     WHEN 5
039940     WHEN 7
039950     WHEN 8
039960     WHEN 10
039970     WHEN 12
039980         MOVE 31 TO 受理日Ｗ
039990     WHEN OTHER
040000          CONTINUE
040010     END-EVALUATE.
040020*
040030*================================================================*
040040 委任年月日取得 SECTION.
040050*
040060** ---// ここの受理年には、最終通院日が入っている為、退避する //----
040070     MOVE 受理年Ｗ   TO 最終通院年Ｗ.
040080     MOVE 受理月Ｗ   TO 最終通院月Ｗ.
040090     MOVE 受理日Ｗ   TO 最終通院日Ｗ.
040100***
040110* (柔整師側)
040120     EVALUATE レセプト日付区分Ｗ 
040130*    /  最終通院日 /
040140     WHEN ZERO
040150         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
040160         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
040170         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
040180         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
040190*    /  月末日 /
040200     WHEN 1 
040210         PERFORM 月末日取得
040220         MOVE 受理年Ｗ     TO 柔整師年Ｗ
040230         MOVE 受理月Ｗ     TO 柔整師月Ｗ
040240         MOVE 受理日Ｗ     TO 柔整師日Ｗ
040250*    /  印字なし /
040260     WHEN 9
040270         MOVE ZERO         TO 柔整師年Ｗ
040280         MOVE ZERO         TO 柔整師月Ｗ
040290         MOVE ZERO         TO 柔整師日Ｗ
040300*    /  その他は、最終通院日 /
040310     WHEN OTHER
040320         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
040330         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
040340         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
040350     END-EVALUATE.
040360**
040370* (患者側)
040380     EVALUATE レセプト患者日付区分Ｗ 
040390*    /  最終通院日 /
040400     WHEN ZERO
040410         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
040420         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
040430         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
040440*    /  月末日 /
040450     WHEN 1 
040460         PERFORM 月末日取得
040470         MOVE 受理年Ｗ     TO 患者委任年Ｗ
040480         MOVE 受理月Ｗ     TO 患者委任月Ｗ
040490         MOVE 受理日Ｗ     TO 患者委任日Ｗ
040500*    /  印字なし /
040510     WHEN 9
040520         MOVE ZERO         TO 患者委任年Ｗ
040530         MOVE ZERO         TO 患者委任月Ｗ
040540         MOVE ZERO         TO 患者委任日Ｗ
040550*    /  その他は、最終通院日 /
040560     WHEN OTHER
040570         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
040580         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
040590         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
040600     END-EVALUATE.
040610*
040620*================================================================*
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
040630*================================================================*
040640 助成レセまとめ判定 SECTION.
040650*---------------------------------------------------------------------------*
009194* 本体まとめ区分＝１
040670* の時は、フラグYES (金額を助成込みで印字）
040680*（例：横浜市の障害は、本体保険（国保系）のレセプト１枚で請求、助成レセはなし）
040690*---------------------------------------------------------------------------*
040700*
040710     MOVE SPACE TO 助成レセまとめフラグ.
007750*
009201     IF レセ－本体まとめ区分 = 1 
009202        MOVE "YES" TO 助成レセまとめフラグ
009203     END-IF.
041100*
041110*----------------------------------------------------------------------*
041120** / 神奈川県固有：摘要に負担者番号と受給者番号 /
041130     IF ( 助成レセまとめフラグ = "YES" ) AND
041140        ( 受－費用負担者番号助成(3:2) = "14" )
041150        IF ( 受－費用負担者番号助成(1:2) NOT = "99" )
041160*            MOVE ALL NC"￣" TO 横線１ 横線２ 横線３
041170*            MOVE ALL NC"｜" TO 縦線１ 縦線２
041180*            MOVE NC"｜"     TO 縦線３ 縦線４
041190*            MOVE NC"公費負担者番号"     TO 神奈川固定１
041200*            MOVE NC"受給者番号"         TO 神奈川固定２
041210*            MOVE NC"／"                 TO 神奈川固定３
041220            MOVE 受－費用負担者番号助成 TO 公費負担者番号
041230*            MOVE 受－受益者番号助成     TO 受給者番号
      */受給者番号が８文字以上の場合枠を無視して印刷する/110426
                  MOVE 受－受益者番号助成   TO 受給者番号Ｗ
                  IF 印刷受給者番号２Ｗ = SPACE
016830                MOVE 印刷受給者番号Ｗ TO 受給者番号
                  ELSE
                      MOVE 受給者番号Ｗ     TO 受給者番号２
                  END-IF
041240        END-IF
041250     END-IF.
041260*
041270*================================================================*
042020*================================================================*
042030 エラー表示 SECTION.
042040*
042050     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
042060     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
042070     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
042080     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
042090                                                   UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
042100     ACCEPT  キー入力 FROM CONS
042110     PERFORM ファイル閉鎖.
042120     EXIT PROGRAM.
042130*================================================================*
042140*================================================================*
042150 ファイル閉鎖 SECTION.
042160*
042170     CLOSE 印刷ファイル.
042180     CLOSE 保険者マスタ     元号マスタ          名称マスタ
042190           制御情報マスタ   施術所情報マスタ    施術記録Ｆ
042200           経過マスタ       受診者情報Ｆ        負傷データＦ
042220           ＩＤ管理マスタ   市町村マスタ        レセプトＦ
042230            負傷原因Ｆ      作業ファイル４.
042240*================================================================*
042250 終了処理 SECTION.
042260*
042270     PERFORM ファイル閉鎖.
042280*================================================================*
042290*================================================================*
042300 テスト印字処理 SECTION.
042310*
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
           受理年 受理月 受理日 委任年 委任月 委任日
           .
           MOVE ALL "X" TO
           共済番号 県施術ＩＤ 保険者番号 記号番号 公費負担者番号 受給者番号 住所１ 住所２ 
           口座名義人 柔整師番号 口座番号 金融機関名１ 金融機関名２ 金融機関名３ 
           金融機関名４ 支店名１ 支店名２ 支店名３ 支店名４ 施術所郵便番号１ 施術所郵便番号２ 
           施術所住所１ 施術所住所２ 施術所電話番号 代表者カナ 保険者名称１ 保険者名称２
           .
           MOVE ALL "Ｎ" TO
           被保険者氏名 患者氏名 接骨院名 代表者名
           負傷原因１ 負傷原因２ 負傷原因３ 負傷原因４ 負傷原因５ 負傷原因６ 
           長期理由文１  長期理由文２ 長期理由文３ 長期理由文４ 長期理由文５
           長期理由文６ 長期理由文７
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
           施療料チェック 夜間チェック 暴風雨雪チェック 難路チェック 大チェック 中チェック 
           小チェック 普通チェック 振込チェック 当座チェック 銀行チェック 金庫チェック 農協チェック 
           本店チェック 支店チェック 本支所チェック
           .
043590*
043600*================================================================*
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
043610******************************************************************
043620 END PROGRAM YJK6125.
043630******************************************************************
