000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             KARIKEI.
000060 AUTHOR.                 山田 浩之
000070*
000080*----------------------------------------------------------------*
000090*         仮計算用 費用額計算
000100*         MED = 
000110*----------------------------------------------------------------*
000120* 患者１人の施術日の費用額を算出。
000130* 
000140*----------------------------------------------------------------*
000150 DATE-WRITTEN.           2010-07-06
000160 DATE-COMPILED.          2010-07-06
000170*----------------------------------------------------------------*
000180******************************************************************
000190*            ENVIRONMENT         DIVISION                        *
000200******************************************************************
000210 ENVIRONMENT             DIVISION.
000220 CONFIGURATION           SECTION.
000230 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000240 OBJECT-COMPUTER.        FMV-DESKPOWER.
000250 SPECIAL-NAMES.          CONSOLE  IS  CONS
000260                         SYSERR   IS  MSGBOX.
000270 INPUT-OUTPUT            SECTION.
000280 FILE-CONTROL.
000290     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000300                             ORGANIZATION             IS  INDEXED
000310                             ACCESS MODE              IS  DYNAMIC
000320                             RECORD KEY               IS  受－施術和暦年月
000330                                                          受－患者コード
000340                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000350                                                          受－患者カナ
000360                                                          受－患者コード
000370                             ALTERNATE RECORD KEY     IS  受－患者コード
000380                                                          受－施術和暦年月
000390                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000400                                                          受－保険種別
000410                                                          受－保険者番号
000420                                                          受－患者コード
000430                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000440                                                          受－公費種別
000450                                                          受－費用負担者番号
000460                                                          受－患者コード
000470                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000480                                                          受－助成種別
000490                                                          受－費用負担者番号助成
000500                                                          受－患者コード
000510                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
000520                                                          受－施術和暦年月
000530                                                          受－患者コード
000540                             FILE STATUS              IS  状態キー
000550                             LOCK        MODE         IS  AUTOMATIC.
000560     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
000570                             ORGANIZATION             IS  INDEXED
000580                             ACCESS MODE              IS  DYNAMIC
000590                             RECORD KEY           IS 施記－施術和暦年月日
000600                                                     施記－患者コード
000610                             ALTERNATE RECORD KEY IS 施記－患者コード
000620                                                     施記－施術和暦年月日
000630                             FILE STATUS              IS  状態キー
000640                             LOCK        MODE         IS  AUTOMATIC.
000650     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
000660                             ORGANIZATION             IS  INDEXED
000670                             ACCESS MODE              IS  DYNAMIC
000680                             RECORD KEY               IS 負－施術和暦年月
000690                                                         負－患者コード
000700                             ALTERNATE RECORD KEY     IS 負－患者コード
000710                                                         負－施術和暦年月
000720                             FILE STATUS              IS  状態キー
000730                             LOCK        MODE         IS  AUTOMATIC.
000740     SELECT  計算マスタ      ASSIGN      TO        KEISANL
000750                             ORGANIZATION             IS  INDEXED
000760                             ACCESS MODE              IS  DYNAMIC
000770                             RECORD KEY               IS  計－制御区分
000780                                                          計－開始和暦年月
000790                             FILE STATUS              IS  状態キー
000800                             LOCK        MODE         IS  AUTOMATIC.
000810     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000820                             ORGANIZATION             IS  INDEXED
000830                             ACCESS MODE              IS  DYNAMIC
000840                             RECORD KEY               IS  元－元号区分
000850                             FILE STATUS              IS  状態キー
000860                             LOCK        MODE         IS  AUTOMATIC.
000870     SELECT  料金マスタ      ASSIGN      TO        RYOUKINL
000880                             ORGANIZATION             IS  INDEXED
000890                             ACCESS MODE              IS  DYNAMIC
000900                             RECORD KEY               IS  料－区分コード
000910                                                          料－部位コード
000920                                                          料－開始和暦年月
000930                             FILE STATUS              IS  状態キー
000940                             LOCK        MODE         IS  AUTOMATIC.
000950******************************************************************
000960*                      DATA DIVISION                             *
000970******************************************************************
000980 DATA                    DIVISION.
000990 FILE                    SECTION.
001000*                           ［ＲＬ＝  ３２０］
001010 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
001020     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
001030** EXTERNAL用
001040*                           ［ＲＬ＝  ２５６］
001050 FD  施術記録Ｆ  IS EXTERNAL        BLOCK   CONTAINS   1   RECORDS.
001060     COPY SEKIROK    OF  XFDLIB  JOINING   施記 AS  PREFIX.
001070*                           ［ＲＬ＝  １２８］
001080 FD  負傷データＦ IS EXTERNAL       BLOCK   CONTAINS   1   RECORDS.
001090     COPY HUSYOU     OF  XFDLIB  JOINING   負   AS  PREFIX.
001100*                           ［ＲＬ＝  ２５６］
001110 FD  計算マスタ  IS EXTERNAL        BLOCK   CONTAINS   1   RECORDS.
001120     COPY KEISAN          OF  XFDLIB  JOINING   計   AS  PREFIX.
001130     COPY KEISANA         OF  XFDLIB  JOINING   計Ａ AS  PREFIX.
001140*                           ［ＲＬ＝  １２８］
001150 FD  元号マスタ  IS EXTERNAL         BLOCK   CONTAINS   1   RECORDS.
001160     COPY GENGOU     OF  XFDLIB  JOINING   元   AS  PREFIX.
001170*                           ［ＲＬ＝  ３２０］
001180 FD  料金マスタ          BLOCK   CONTAINS   1   RECORDS.
001190     COPY RYOUKIN         OF  XFDLIB  JOINING   料   AS  PREFIX.
001200     COPY RYOUKNA         OF  XFDLIB  JOINING   料Ａ AS  PREFIX.
001210     COPY RYOUKNB         OF  XFDLIB  JOINING   料Ｂ AS  PREFIX.
001220     COPY RYOUKNC         OF  XFDLIB  JOINING   料Ｃ AS  PREFIX.
001230     COPY RYOUKND         OF  XFDLIB  JOINING   料Ｄ AS  PREFIX.
001240     COPY RYOUKNE         OF  XFDLIB  JOINING   料Ｅ AS  PREFIX.
001250     COPY RYOUKNF         OF  XFDLIB  JOINING   料Ｆ AS  PREFIX.
001260*----------------------------------------------------------------*
001270******************************************************************
001280*                WORKING-STORAGE SECTION                         *
001290******************************************************************
001300 WORKING-STORAGE         SECTION.
001310 01 キー入力                           PIC X    VALUE SPACE.
001320 01 状態キー                           PIC X(2) VALUE SPACE.
001330 01 ファイル名                         PIC N(6) VALUE SPACE.
001340 01 退避項目ＧＷ.
001350   03 レセプト種類Ｗ                   PIC X(4) VALUE SPACE.
001360   03 レセプト種類ＧＷ                 PIC X(4) VALUE SPACE.
001370   03 レセプト種別ＧＷ                 PIC 9(2) VALUE ZERO.
001380 01 一部負担金ＷＲ                     PIC 9(5) VALUE ZERO.
001390*
001400 01 再検料請求フラグ                   PIC X(3) VALUE SPACE.
001410 01 初検カウンタ                       PIC 9(4) VALUE ZERO.
001420 01 終了フラグ                         PIC X(3) VALUE SPACE.
001430 01 終了フラグ２                       PIC X(3) VALUE SPACE.
001440 01 終了フラグ３                       PIC X(3) VALUE SPACE.
001450 01 再検料初期化フラグ                 PIC X(3) VALUE SPACE.
001460 01 部位ＣＮＴ                         PIC 9(2) VALUE ZERO.
001470 01 終了和暦年月日ＣＷ.
001480   03 終了和暦年月ＣＷ.
001490     05 終了和暦ＣＷ                   PIC 9    VALUE ZERO.
001500     05 終了年月ＣＷ.
001510       07 終了年ＣＷ                   PIC 9(2) VALUE ZERO.
001520       07 終了月ＣＷ                   PIC 9(2) VALUE ZERO.
001530   03 終了日ＣＷ                       PIC 9(2) VALUE ZERO.
001540 01 施術和暦年月日ＣＷ.
001550   03 施術和暦年月ＣＷ.
001560     05 施術和暦ＣＷ                   PIC 9    VALUE ZERO.
001570     05 施術年月ＣＷ.
001580       07 施術年ＣＷ                   PIC 9(2) VALUE ZERO.
001590       07 施術月ＣＷ                   PIC 9(2) VALUE ZERO.
001600   03 施術日ＣＷ                       PIC 9(2) VALUE ZERO.
001610 01 負傷絶対値Ｗ                       PIC 9(6) VALUE ZERO.
001620 01 施術絶対値Ｗ                       PIC 9(6) VALUE ZERO.
001630 01 負傷経過日Ｗ                       PIC 9(4) VALUE ZERO.
001640 01 冷罨日数骨折Ｗ                     PIC 9(2) VALUE ZERO.
001650 01 冷罨日数脱臼Ｗ                     PIC 9(2) VALUE ZERO.
001660 01 冷罨日数打撲Ｗ                     PIC 9(2) VALUE ZERO.
001670 01 温罨待機骨折Ｗ                     PIC 9(2) VALUE ZERO.
001680 01 温罨待機脱臼Ｗ                     PIC 9(2) VALUE ZERO.
001690 01 西暦年Ｗ                           PIC 9(4) VALUE ZERO.
001700 01 計算和暦年月Ｗ.
001710     03 計算和暦Ｗ                     PIC 9    VALUE ZERO.
001720     03 計算年月Ｗ.
001730        05 計算年Ｗ                    PIC 9(2) VALUE ZERO.
001740        05 計算月Ｗ                    PIC 9(2) VALUE ZERO.
001750     03 計算日Ｗ                       PIC 9(2) VALUE ZERO.
001760 01 加算年Ｗ                           PIC 9(4) VALUE ZERO.
001770 01 閏年回数Ｗ                         PIC 9(2) VALUE ZERO.
001780 01 通常年回数Ｗ                       PIC 9(2) VALUE ZERO.
001790 01 西暦年差Ｗ                         PIC 9(2) VALUE ZERO.
001800 01 通常年絶対値Ｗ                     PIC 9(6) VALUE ZERO.
001810 01 閏年絶対値Ｗ                       PIC 9(6) VALUE ZERO.
001820 01 年絶対値Ｗ                         PIC 9(6) VALUE ZERO.
001830 01 商Ｗ                               PIC 9(4) VALUE ZERO.
001840 01 剰余Ｗ                             PIC 9(4) VALUE ZERO.
001850 01 絶対値月Ｗ                         PIC 9(3) VALUE ZERO.
001860 01 絶対値月Ｗ２                       PIC 9(3) VALUE ZERO.
001870 01 絶対値Ｗ                           PIC 9(6) VALUE ZERO.
001880 01 枝番チェックフラグ                 PIC X(3) VALUE SPACE.
001890 01 負傷ＣＮＴ                         PIC 9(2) VALUE ZERO.
001900 01 前月フラグ                         PIC X(3)  VALUE SPACE.
001910 01 計算年月日２Ｗ.
001920    03 計算和暦２Ｗ                    PIC 9(1)   VALUE ZERO.
001930    03 計算年２Ｗ                      PIC S9(2)  VALUE ZERO.
001940    03 計算月２Ｗ                      PIC S9(2)  VALUE ZERO.
001950    03 計算日２Ｗ                      PIC S9(2)  VALUE ZERO.
001960 01 開始年月日２Ｗ.
001970    03 開始和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
001980    03 開始年２Ｗ                      PIC 9(2)  VALUE ZERO.
001990    03 開始月２Ｗ                      PIC 9(2)  VALUE ZERO.
002000    03 開始日２Ｗ                      PIC 9(2)  VALUE ZERO.
002010    03 開始西暦年２Ｗ                  PIC S9(4) VALUE ZERO.
002020 01 終了年月日２Ｗ.
002030    03 終了和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
002040    03 終了年２Ｗ                      PIC 9(2)  VALUE ZERO.
002050    03 終了月２Ｗ                      PIC 9(2)  VALUE ZERO.
002060    03 終了日２Ｗ                      PIC 9(2)  VALUE ZERO.
002070    03 終了西暦年２Ｗ                  PIC S9(4) VALUE ZERO.
002080*
002090 01 患者コードＣＷ.
002100     03 患者番号ＣＷ                   PIC 9(6).
002110     03 枝番ＣＷ                       PIC X.
002120*
002130 01 部位合計長期Ｗ１                   PIC 9(6) VALUE ZERO.
002140 01 部位合計長期Ｗ２                   PIC 9(6) VALUE ZERO.
002150 01 部位合計長期Ｗ３                   PIC 9(6) VALUE ZERO.
002160 01 部位合計長期Ｗ３１                 PIC 9(6) VALUE ZERO.
002170 01 部位合計長期Ｗ４                   PIC 9(6) VALUE ZERO.
002180 01 部位合計長期Ｗ４１                 PIC 9(6) VALUE ZERO.
002190 01 部位合計長期Ｗ４２                 PIC 9(6) VALUE ZERO.
002200 01 部位合計長期Ｗ５                   PIC 9(6) VALUE ZERO.
002210 01 部位合計長期Ｗ５１                 PIC 9(6) VALUE ZERO.
002220 01 部位合計長期Ｗ５２                 PIC 9(6) VALUE ZERO.
002230 01 部位合計長期Ｗ５３                 PIC 9(6) VALUE ZERO.
002240*
002250 01 初検料ＧＷ                         PIC 9(5) VALUE ZERO.
002260 01 初検加算料ＧＷ                     PIC 9(6) VALUE ZERO.
002270 01 初回処置料ＧＷ                     PIC 9(5) VALUE ZERO.
002280 01 再検料ＧＷ                         PIC 9(5) VALUE ZERO.
002290 01 往療料ＧＷ                         PIC 9(6) VALUE ZERO.
002300 01 往療加算料ＧＷ                     PIC 9(6) VALUE ZERO.
002310 01 往療回数ＧＷ                       PIC 9(2) VALUE ZERO.
002320 01 後療料ＧＷ                         PIC 9(6) VALUE ZERO.
002330 01 冷罨法料ＧＷ                       PIC 9(4) VALUE ZERO.
002340 01 温罨法料ＧＷ                       PIC 9(4) VALUE ZERO.
002350 01 電療料ＧＷ                         PIC 9(4) VALUE ZERO.
002360 01 金属副子料ＧＷ                     PIC 9(5) VALUE ZERO.
002370 01 施術情報提供料ＧＷ                 PIC 9(5) VALUE ZERO.
002380*
002390 01 逓減率ＳＫＷ.
002400   03 逓減率ＫＷ  OCCURS 9.
002410     05 多部位逓減率ＫＷ               PIC 9(3) VALUE ZERO.
002420* 部位毎の多部位逓減部位毎のワーク（日計用）
002430* １部位目
002440 01 後療料１ＰＷ                       PIC 9(5) VALUE ZERO.
002450 01 冷罨法料１ＰＷ                     PIC 9(4) VALUE ZERO.
002460 01 温罨法料１ＰＷ                     PIC 9(4) VALUE ZERO.
002470 01 電療料１ＰＷ                       PIC 9(4) VALUE ZERO.
002480 01 小計１ＰＷ                         PIC 9(6) VALUE ZERO.
002490* ２部位目
002500 01 後療料２ＰＷ                       PIC 9(5) VALUE ZERO.
002510 01 冷罨法料２ＰＷ                     PIC 9(4) VALUE ZERO.
002520 01 温罨法料２ＰＷ                     PIC 9(4) VALUE ZERO.
002530 01 電療料２ＰＷ                       PIC 9(4) VALUE ZERO.
002540* ３部位目
002550 01 後療料３ＰＷ                       PIC 9(5) VALUE ZERO.
002560 01 冷罨法料３ＰＷ                     PIC 9(4) VALUE ZERO.
002570 01 温罨法料３ＰＷ                     PIC 9(4) VALUE ZERO.
002580 01 電療料３ＰＷ                       PIC 9(4) VALUE ZERO.
002590* ３部位目（繰上り）
002600 01 後療料３１ＰＷ                     PIC 9(5) VALUE ZERO.
002610 01 冷罨法料３１ＰＷ                   PIC 9(4) VALUE ZERO.
002620 01 温罨法料３１ＰＷ                   PIC 9(4) VALUE ZERO.
002630 01 電療料３１ＰＷ                     PIC 9(4) VALUE ZERO.
002640* ４部位目
002650 01 後療料４ＰＷ                       PIC 9(5) VALUE ZERO.
002660 01 冷罨法料４ＰＷ                     PIC 9(4) VALUE ZERO.
002670 01 温罨法料４ＰＷ                     PIC 9(4) VALUE ZERO.
002680 01 電療料４ＰＷ                       PIC 9(4) VALUE ZERO.
002690* ４部位目（繰上り１）
002700 01 後療料４１ＰＷ                     PIC 9(5) VALUE ZERO.
002710 01 冷罨法料４１ＰＷ                   PIC 9(4) VALUE ZERO.
002720 01 温罨法料４１ＰＷ                   PIC 9(4) VALUE ZERO.
002730 01 電療料４１ＰＷ                     PIC 9(4) VALUE ZERO.
002740* ４部位目（繰上り２）
002750 01 後療料４２ＰＷ                     PIC 9(5) VALUE ZERO.
002760 01 冷罨法料４２ＰＷ                   PIC 9(4) VALUE ZERO.
002770 01 温罨法料４２ＰＷ                   PIC 9(4) VALUE ZERO.
002780 01 電療料４２ＰＷ                     PIC 9(4) VALUE ZERO.
002790* ５部位目
002800 01 後療料５ＰＷ                       PIC 9(5) VALUE ZERO.
002810 01 冷罨法料５ＰＷ                     PIC 9(4) VALUE ZERO.
002820 01 温罨法料５ＰＷ                     PIC 9(4) VALUE ZERO.
002830 01 電療料５ＰＷ                       PIC 9(4) VALUE ZERO.
002840* ５部位目（繰上り１）
002850 01 後療料５１ＰＷ                     PIC 9(5) VALUE ZERO.
002860 01 冷罨法料５１ＰＷ                   PIC 9(4) VALUE ZERO.
002870 01 温罨法料５１ＰＷ                   PIC 9(4) VALUE ZERO.
002880 01 電療料５１ＰＷ                     PIC 9(4) VALUE ZERO.
002890* ５部位目（繰上り２）
002900 01 後療料５２ＰＷ                     PIC 9(5) VALUE ZERO.
002910 01 冷罨法料５２ＰＷ                   PIC 9(4) VALUE ZERO.
002920 01 温罨法料５２ＰＷ                   PIC 9(4) VALUE ZERO.
002930 01 電療料５２ＰＷ                     PIC 9(4) VALUE ZERO.
002940* ５部位目（繰上り３）
002950 01 後療料５３ＰＷ                     PIC 9(5) VALUE ZERO.
002960 01 冷罨法料５３ＰＷ                   PIC 9(4) VALUE ZERO.
002970 01 温罨法料５３ＰＷ                   PIC 9(4) VALUE ZERO.
002980 01 電療料５３ＰＷ                     PIC 9(4) VALUE ZERO.
002990*
003000 01 初検時間外Ｗ                       PIC 9 VALUE ZERO.
003010 01 初検休日Ｗ                         PIC 9 VALUE ZERO.
003020 01 初検深夜Ｗ                         PIC 9 VALUE ZERO.
003030*
003040 01 初検料Ｗ                           PIC 9(4) VALUE ZERO.
003050 01 初検加算料Ｗ                       PIC 9(5) VALUE ZERO.
003060 01 再検料Ｗ                           PIC 9(4) VALUE ZERO.
003070 01 往療料Ｗ                           PIC 9(5) VALUE ZERO.
003080 01 往療加算料Ｗ                       PIC 9(6) VALUE ZERO.
003090 01 後療料Ｗ                           PIC 9(5) VALUE ZERO.
003100 01 冷罨法料Ｗ                         PIC 9(4) VALUE ZERO.
003110 01 温罨法料Ｗ                         PIC 9(4) VALUE ZERO.
003120 01 電療料Ｗ                           PIC 9(4) VALUE ZERO.
003130*
003140 01 部位数Ｗ                           PIC 9(2) VALUE ZERO.
003150 01 継続部位数Ｗ                       PIC 9(2) VALUE ZERO.
003160 01 部位カウンタ                       PIC 9(2)    VALUE ZERO.
003170 01 部位カウンタ２                     PIC 9(2)    VALUE ZERO.
003180*
003190 01 開始和暦年月日Ｗ.
003200   03 開始和暦年月Ｗ.
003210     05 開始和暦Ｗ                     PIC 9.
003220     05 開始年月Ｗ.
003230        07 開始年Ｗ                    PIC 9(2).
003240        07 開始月Ｗ                    PIC 9(2).
003250   03 開始日Ｗ                         PIC 9(2).
003260*
003270 01 終了和暦年月日Ｗ.
003280   03 終了和暦年月Ｗ.
003290     05 終了和暦Ｗ                     PIC 9.
003300     05 終了年月Ｗ.
003310        07 終了年Ｗ                    PIC 9(2).
003320        07 終了月Ｗ                    PIC 9(2).
003330   03 終了日Ｗ                         PIC 9(2).
003340* 基本
003350 01 初検料ＡＷ                         PIC 9(4) VALUE ZERO.
003360 01 時間外ＡＷ                         PIC 9(4) VALUE ZERO.
003370 01 休日ＡＷ                           PIC 9(4) VALUE ZERO.
003380 01 深夜ＡＷ                           PIC 9(4) VALUE ZERO.
003390 01 往療基本料金ＡＷ                   PIC 9(4) VALUE ZERO.
003400 01 往療追加料金ＡＷ                   PIC 9(4) VALUE ZERO.
003410 01 往療加算率ＡＷ                     PIC 9(3) VALUE ZERO.
003420 01 往療深夜加算率ＡＷ                 PIC 9(3) VALUE ZERO.
003430 01 往療難路加算率ＡＷ                 PIC 9(3) VALUE ZERO.
003440 01 往療暴風雨加算率ＡＷ               PIC 9(3) VALUE ZERO.
003450 01 再検料ＡＷ                         PIC 9(4) VALUE ZERO.
003460 01 冷罨法料ＡＷ                       PIC 9(4) VALUE ZERO.
003470 01 温罨法料ＡＷ                       PIC 9(4) VALUE ZERO.
003480 01 電療料ＡＷ                         PIC 9(4) VALUE ZERO.
003490 01 大ＡＷ                             PIC 9(4) VALUE ZERO.
003500 01 中ＡＷ                             PIC 9(4) VALUE ZERO.
003510 01 小ＡＷ                             PIC 9(4) VALUE ZERO.
003520 01 施術情報提供料ＡＷ                 PIC 9(4) VALUE ZERO.
003530 01 初検時相談料ＧＷ                   PIC 9(5) VALUE ZERO.
003540 01 初検時相談料Ｗ                     PIC 9(4) VALUE ZERO.
003550 01 初検時相談料ＡＷ                   PIC 9(4) VALUE ZERO.
003560*
003570 01 部位コードＷ.
003580    03 負傷種別Ｗ                      PIC 9(2).
003590    03 部位Ｗ                          PIC 9(2).
003600    03 左右区分Ｗ                      PIC 9.
003610    03 負傷位置番号Ｗ                  PIC 9(2).
003620* 負傷別
003630 01 拘縮後療料ＢＷ                     PIC 9(5) VALUE ZERO.
003640 01 後療料ＢＷ                         PIC 9(5) VALUE ZERO.
003650 01 整復料ＢＷ                         PIC 9(5) VALUE ZERO.
003660*
003670 01 部位合計Ｗ１                       PIC 9(6) VALUE ZERO.
003680 01 部位合計Ｗ２                       PIC 9(6) VALUE ZERO.
003690 01 部位合計Ｗ３                       PIC 9(6) VALUE ZERO.
003700 01 部位合計Ｗ３１                     PIC 9(6) VALUE ZERO.
003710 01 部位合計Ｗ４                       PIC 9(6) VALUE ZERO.
003720 01 部位合計Ｗ４１                     PIC 9(6) VALUE ZERO.
003730 01 部位合計Ｗ４２                     PIC 9(6) VALUE ZERO.
003740 01 部位合計Ｗ５                       PIC 9(6) VALUE ZERO.
003750 01 部位合計Ｗ５１                     PIC 9(6) VALUE ZERO.
003760 01 部位合計Ｗ５２                     PIC 9(6) VALUE ZERO.
003770 01 部位合計Ｗ５３                     PIC 9(6) VALUE ZERO.
003780*
003790 01 長期区分ＳＷ.
003800    03 長期区分Ｗ                      PIC 9(2) OCCURS 9 VALUE ZERO.
003810*
003820 01 初回処置ＳＷ.
003830   03 初回処置ＯＷ OCCURS 9.
003840     05 初回処置料ＯＷ                 PIC 9(5) VALUE ZERO.
003850*
003860 01 後療単価ＳＷ.
003870   03 後療単価ＯＷ１ OCCURS 9.
003880     05 後療単価ＯＷ                   PIC 9(4) VALUE ZERO.
003890*
003900 01 後療料１００ＳＷ.
003910   03 後療料１００Ｗ１ OCCURS 9.
003920     05 後療料１００Ｗ                 PIC 9(5) OCCURS 9 VALUE ZERO.
003930*
003940 01 電療料１００ＳＷ.
003950   03 電療料１００Ｗ１ OCCURS 9.
003960     05 電療料１００Ｗ                 PIC 9(5) OCCURS 9 VALUE ZERO.
003970*
003980 01 冷罨法料１００ＳＷ.
003990   03 冷罨法料１００Ｗ１ OCCURS 9.
004000     05 冷罨法料１００Ｗ               PIC 9(5) OCCURS 9 VALUE ZERO.
004010*
004020 01 温罨法料１００ＳＷ.
004030   03 温罨法料１００Ｗ１ OCCURS 9.
004040     05 温罨法料１００Ｗ               PIC 9(5) OCCURS 9 VALUE ZERO.
004050* 部位毎の多部位逓減部位毎のワーク
004060* １部位目
004070 01 後療回数１ＲＷ                     PIC 9(2) VALUE ZERO.
004080 01 後療料１ＲＷ                       PIC 9(5) VALUE ZERO.
004090 01 冷罨法回数１ＲＷ                   PIC 9(2) VALUE ZERO.
004100 01 冷罨法料１ＲＷ                     PIC 9(4) VALUE ZERO.
004110 01 温罨法回数１ＲＷ                   PIC 9(2) VALUE ZERO.
004120 01 温罨法料１ＲＷ                     PIC 9(4) VALUE ZERO.
004130 01 電療回数１ＲＷ                     PIC 9(2) VALUE ZERO.
004140 01 電療料１ＲＷ                       PIC 9(4) VALUE ZERO.
004150 01 小計１ＲＷ                         PIC 9(6) VALUE ZERO.
004160* ２部位目
004170 01 後療回数２ＲＷ                     PIC 9(2) VALUE ZERO.
004180 01 後療料２ＲＷ                       PIC 9(5) VALUE ZERO.
004190 01 冷罨法回数２ＲＷ                   PIC 9(2) VALUE ZERO.
004200 01 冷罨法料２ＲＷ                     PIC 9(4) VALUE ZERO.
004210 01 温罨法回数２ＲＷ                   PIC 9(2) VALUE ZERO.
004220 01 温罨法料２ＲＷ                     PIC 9(4) VALUE ZERO.
004230 01 電療回数２ＲＷ                     PIC 9(2) VALUE ZERO.
004240 01 電療料２ＲＷ                       PIC 9(4) VALUE ZERO.
004250 01 小計２ＲＷ                         PIC 9(6) VALUE ZERO.
004260* ３部位目
004270 01 後療回数３ＲＷ                     PIC 9(2) VALUE ZERO.
004280 01 後療料３ＲＷ                       PIC 9(5) VALUE ZERO.
004290 01 冷罨法回数３ＲＷ                   PIC 9(2) VALUE ZERO.
004300 01 冷罨法料３ＲＷ                     PIC 9(4) VALUE ZERO.
004310 01 温罨法回数３ＲＷ                   PIC 9(2) VALUE ZERO.
004320 01 温罨法料３ＲＷ                     PIC 9(4) VALUE ZERO.
004330 01 電療回数３ＲＷ                     PIC 9(2) VALUE ZERO.
004340 01 電療料３ＲＷ                       PIC 9(4) VALUE ZERO.
004350 01 小計３ＲＷ                         PIC 9(6) VALUE ZERO.
004360* ３部位目（繰上り）
004370 01 後療回数３１ＲＷ                   PIC 9(2) VALUE ZERO.
004380 01 後療料３１ＲＷ                     PIC 9(5) VALUE ZERO.
004390 01 冷罨法回数３１ＲＷ                 PIC 9(2) VALUE ZERO.
004400 01 冷罨法料３１ＲＷ                   PIC 9(4) VALUE ZERO.
004410 01 温罨法回数３１ＲＷ                 PIC 9(2) VALUE ZERO.
004420 01 温罨法料３１ＲＷ                   PIC 9(4) VALUE ZERO.
004430 01 電療回数３１ＲＷ                   PIC 9(2) VALUE ZERO.
004440 01 電療料３１ＲＷ                     PIC 9(4) VALUE ZERO.
004450 01 小計３１ＲＷ                       PIC 9(6) VALUE ZERO.
004460 01 逓減開始和暦年月日３１ＲＷ.
004470   03 逓減開始和暦年月３１ＲＷ.
004480     05 逓減開始和暦３１ＲＷ           PIC 9 VALUE ZERO.
004490     05 逓減開始年月３１ＲＷ.
004500       07 逓減開始年３１ＲＷ           PIC 9(2) VALUE ZERO.
004510       07 逓減開始月３１ＲＷ           PIC 9(2) VALUE ZERO.
004520   03 逓減開始日３１ＲＷ               PIC 9(2) VALUE ZERO.
004530* ４部位目
004540 01 後療回数４ＲＷ                     PIC 9(2) VALUE ZERO.
004550 01 後療料４ＲＷ                       PIC 9(5) VALUE ZERO.
004560 01 冷罨法回数４ＲＷ                   PIC 9(2) VALUE ZERO.
004570 01 冷罨法料４ＲＷ                     PIC 9(4) VALUE ZERO.
004580 01 温罨法回数４ＲＷ                   PIC 9(2) VALUE ZERO.
004590 01 温罨法料４ＲＷ                     PIC 9(4) VALUE ZERO.
004600 01 電療回数４ＲＷ                     PIC 9(2) VALUE ZERO.
004610 01 電療料４ＲＷ                       PIC 9(4) VALUE ZERO.
004620 01 小計４ＲＷ                         PIC 9(6) VALUE ZERO.
004630* ４部位目（繰上り１）
004640 01 後療回数４１ＲＷ                   PIC 9(2) VALUE ZERO.
004650 01 後療料４１ＲＷ                     PIC 9(5) VALUE ZERO.
004660 01 冷罨法回数４１ＲＷ                 PIC 9(2) VALUE ZERO.
004670 01 冷罨法料４１ＲＷ                   PIC 9(4) VALUE ZERO.
004680 01 温罨法回数４１ＲＷ                 PIC 9(2) VALUE ZERO.
004690 01 温罨法料４１ＲＷ                   PIC 9(4) VALUE ZERO.
004700 01 電療回数４１ＲＷ                   PIC 9(2) VALUE ZERO.
004710 01 電療料４１ＲＷ                     PIC 9(4) VALUE ZERO.
004720 01 小計４１ＲＷ                       PIC 9(6) VALUE ZERO.
004730 01 逓減開始和暦年月日４１ＲＷ.
004740   03 逓減開始和暦年月４１ＲＷ.
004750     05 逓減開始和暦４１ＲＷ           PIC 9 VALUE ZERO.
004760     05 逓減開始年月４１ＲＷ.
004770       07 逓減開始年４１ＲＷ           PIC 9(2) VALUE ZERO.
004780       07 逓減開始月４１ＲＷ           PIC 9(2) VALUE ZERO.
004790   03 逓減開始日４１ＲＷ               PIC 9(2) VALUE ZERO.
004800* ４部位目（繰上り２）
004810 01 後療回数４２ＲＷ                   PIC 9(2) VALUE ZERO.
004820 01 後療料４２ＲＷ                     PIC 9(5) VALUE ZERO.
004830 01 冷罨法回数４２ＲＷ                 PIC 9(2) VALUE ZERO.
004840 01 冷罨法料４２ＲＷ                   PIC 9(4) VALUE ZERO.
004850 01 温罨法回数４２ＲＷ                 PIC 9(2) VALUE ZERO.
004860 01 温罨法料４２ＲＷ                   PIC 9(4) VALUE ZERO.
004870 01 電療回数４２ＲＷ                   PIC 9(2) VALUE ZERO.
004880 01 電療料４２ＲＷ                     PIC 9(4) VALUE ZERO.
004890 01 小計４２ＲＷ                       PIC 9(6) VALUE ZERO.
004900 01 逓減開始和暦年月日４２ＲＷ.
004910   03 逓減開始和暦年月４２ＲＷ.
004920     05 逓減開始和暦４２ＲＷ           PIC 9 VALUE ZERO.
004930     05 逓減開始年月４２ＲＷ.
004940       07 逓減開始年４２ＲＷ           PIC 9(2) VALUE ZERO.
004950       07 逓減開始月４２ＲＷ           PIC 9(2) VALUE ZERO.
004960   03 逓減開始日４２ＲＷ               PIC 9(2) VALUE ZERO.
004970* ５部位目
004980 01 後療回数５ＲＷ                     PIC 9(2) VALUE ZERO.
004990 01 後療料５ＲＷ                       PIC 9(5) VALUE ZERO.
005000 01 冷罨法回数５ＲＷ                   PIC 9(2) VALUE ZERO.
005010 01 冷罨法料５ＲＷ                     PIC 9(4) VALUE ZERO.
005020 01 温罨法回数５ＲＷ                   PIC 9(2) VALUE ZERO.
005030 01 温罨法料５ＲＷ                     PIC 9(4) VALUE ZERO.
005040 01 電療回数５ＲＷ                     PIC 9(2) VALUE ZERO.
005050 01 電療料５ＲＷ                       PIC 9(4) VALUE ZERO.
005060 01 小計５ＲＷ                         PIC 9(6) VALUE ZERO.
005070* ５部位目（繰上り１）
005080 01 後療回数５１ＲＷ                   PIC 9(2) VALUE ZERO.
005090 01 後療料５１ＲＷ                     PIC 9(5) VALUE ZERO.
005100 01 冷罨法回数５１ＲＷ                 PIC 9(2) VALUE ZERO.
005110 01 冷罨法料５１ＲＷ                   PIC 9(4) VALUE ZERO.
005120 01 温罨法回数５１ＲＷ                 PIC 9(2) VALUE ZERO.
005130 01 温罨法料５１ＲＷ                   PIC 9(4) VALUE ZERO.
005140 01 電療回数５１ＲＷ                   PIC 9(2) VALUE ZERO.
005150 01 電療料５１ＲＷ                     PIC 9(4) VALUE ZERO.
005160 01 小計５１ＲＷ                       PIC 9(6) VALUE ZERO.
005170 01 逓減開始和暦年月日５１ＲＷ.
005180   03 逓減開始和暦年月５１ＲＷ.
005190     05 逓減開始和暦５１ＲＷ           PIC 9 VALUE ZERO.
005200     05 逓減開始年月５１ＲＷ.
005210       07 逓減開始年５１ＲＷ           PIC 9(2) VALUE ZERO.
005220       07 逓減開始月５１ＲＷ           PIC 9(2) VALUE ZERO.
005230   03 逓減開始日５１ＲＷ               PIC 9(2) VALUE ZERO.
005240* ５部位目（繰上り２）
005250 01 後療回数５２ＲＷ                   PIC 9(2) VALUE ZERO.
005260 01 後療料５２ＲＷ                     PIC 9(5) VALUE ZERO.
005270 01 冷罨法回数５２ＲＷ                 PIC 9(2) VALUE ZERO.
005280 01 冷罨法料５２ＲＷ                   PIC 9(4) VALUE ZERO.
005290 01 温罨法回数５２ＲＷ                 PIC 9(2) VALUE ZERO.
005300 01 温罨法料５２ＲＷ                   PIC 9(4) VALUE ZERO.
005310 01 電療回数５２ＲＷ                   PIC 9(2) VALUE ZERO.
005320 01 電療料５２ＲＷ                     PIC 9(4) VALUE ZERO.
005330 01 小計５２ＲＷ                       PIC 9(6) VALUE ZERO.
005340 01 逓減開始和暦年月日５２ＲＷ.
005350   03 逓減開始和暦年月５２ＲＷ.
005360     05 逓減開始和暦５２ＲＷ           PIC 9 VALUE ZERO.
005370     05 逓減開始年月５２ＲＷ.
005380       07 逓減開始年５２ＲＷ           PIC 9(2) VALUE ZERO.
005390       07 逓減開始月５２ＲＷ           PIC 9(2) VALUE ZERO.
005400   03 逓減開始日５２ＲＷ               PIC 9(2) VALUE ZERO.
005410* ５部位目（繰上り３）
005420 01 後療回数５３ＲＷ                   PIC 9(2) VALUE ZERO.
005430 01 後療料５３ＲＷ                     PIC 9(5) VALUE ZERO.
005440 01 冷罨法回数５３ＲＷ                 PIC 9(2) VALUE ZERO.
005450 01 冷罨法料５３ＲＷ                   PIC 9(4) VALUE ZERO.
005460 01 温罨法回数５３ＲＷ                 PIC 9(2) VALUE ZERO.
005470 01 温罨法料５３ＲＷ                   PIC 9(4) VALUE ZERO.
005480 01 電療回数５３ＲＷ                   PIC 9(2) VALUE ZERO.
005490 01 電療料５３ＲＷ                     PIC 9(4) VALUE ZERO.
005500 01 小計５３ＲＷ                       PIC 9(6) VALUE ZERO.
005510 01 逓減開始和暦年月日５３ＲＷ.
005520   03 逓減開始和暦年月５３ＲＷ.
005530     05 逓減開始和暦５３ＲＷ           PIC 9 VALUE ZERO.
005540     05 逓減開始年月５３ＲＷ.
005550       07 逓減開始年５３ＲＷ           PIC 9(2) VALUE ZERO.
005560       07 逓減開始月５３ＲＷ           PIC 9(2) VALUE ZERO.
005570   03 逓減開始日５３ＲＷ               PIC 9(2) VALUE ZERO.
005580*
005590 01 部位日計多部位Ｗ１                 PIC 9(6) VALUE ZERO.
005600 01 部位日計多部位Ｗ２                 PIC 9(6) VALUE ZERO.
005610 01 部位日計多部位Ｗ３                 PIC 9(6) VALUE ZERO.
005620 01 部位日計多部位Ｗ３１               PIC 9(6) VALUE ZERO.
005630 01 部位日計多部位Ｗ４                 PIC 9(6) VALUE ZERO.
005640 01 部位日計多部位Ｗ４１               PIC 9(6) VALUE ZERO.
005650 01 部位日計多部位Ｗ５                 PIC 9(6) VALUE ZERO.
005660 01 部位日計多部位Ｗ５１               PIC 9(6) VALUE ZERO.
005670 01 部位日計多部位Ｗ５２               PIC 9(6) VALUE ZERO.
005680 01 部位日計多部位Ｗ５３               PIC 9(6) VALUE ZERO.
005690*
005700 01 部位合計多部位Ｗ１                 PIC 9(6) VALUE ZERO.
005710 01 部位合計多部位Ｗ２                 PIC 9(6) VALUE ZERO.
005720 01 部位合計多部位Ｗ３                 PIC 9(6) VALUE ZERO.
005730 01 部位合計多部位Ｗ３１               PIC 9(6) VALUE ZERO.
005740 01 部位合計多部位Ｗ４                 PIC 9(6) VALUE ZERO.
005750 01 部位合計多部位Ｗ４１               PIC 9(6) VALUE ZERO.
005760 01 部位合計多部位Ｗ５                 PIC 9(6) VALUE ZERO.
005770 01 部位合計多部位Ｗ５１               PIC 9(6) VALUE ZERO.
005780 01 部位合計多部位Ｗ５２               PIC 9(6) VALUE ZERO.
005790 01 部位合計多部位Ｗ５３               PIC 9(6) VALUE ZERO.
005800*
005810 01 長期逓減率中間ＫＷ                 PIC 9V9(3) VALUE ZERO.
005820 01 長期逓減率ＫＷ                     PIC 9(3) VALUE ZERO.
005830 01 長期逓減月境界日ＫＷ               PIC 9(2) VALUE ZERO.
005840 01 長期逓減月数ＫＷ                   PIC 9(2) VALUE ZERO.
005850 01 老人負担回数ＫＷ                   PIC 9(2) VALUE ZERO.
005860 01 逓減率中間結果Ｗ                   PIC 9V9(3) VALUE ZERO.
005870*
005880 01 最大登録部位数Ｗ                   PIC 9(2) VALUE ZERO.
005890*
005900 01 負担額ＧＷ                         PIC 9(5) VALUE ZERO.
005910 01 費用額老人ＧＷ                     PIC 9(6) VALUE ZERO.
005920 01 負担額老人ＧＷ                     PIC 9(5) VALUE ZERO.
005930 01 請求額老人ＧＷ                     PIC 9(6) VALUE ZERO.
005940 01 負担額助成ＧＷ                     PIC 9(6) VALUE ZERO.
005950 01 請求額助成ＧＷ                     PIC 9(6) VALUE ZERO.
005960*
005970 01 往療夜間Ｗ                         PIC 9 VALUE ZERO.
005980 01 往療難路Ｗ                         PIC 9 VALUE ZERO.
005990 01 往療暴風雨Ｗ                       PIC 9 VALUE ZERO.
006000 01 往療時間外Ｗ                       PIC 9 VALUE ZERO.
006010*
006020 01 金属副子大Ｗ                       PIC 9 VALUE ZERO.
006030 01 金属副子中Ｗ                       PIC 9 VALUE ZERO.
006040 01 金属副子小Ｗ                       PIC 9 VALUE ZERO.
006050*
006060* 基本
006070 01 往療基本距離ＫＷ                   PIC 9(2) VALUE ZERO.
006080 01 後療可能算定数ＫＷ                 PIC 9(2) VALUE ZERO.
006090*
006100 01 初検請求区分Ｗ                     PIC 9 VALUE ZERO.
006110 01 多部位ＣＮＴ                       PIC 9(2) VALUE ZERO.
006120 01 金属副子料Ｗ                       PIC 9(4) VALUE ZERO.
006130 01 初回処置料Ｗ                       PIC 9(5) VALUE ZERO.
006140 01 施術情報提供料Ｗ                   PIC 9(4) VALUE ZERO.
006150*
006160 COPY SEKIROK    OF  XFDLIB  JOINING   退避施記 AS  PREFIX.
006170*****************************************************************
006180*                          連結項目                             *
006190*****************************************************************
006191*
006193***********************
006194 01 連料－料金１ IS EXTERNAL.
006195   03 連料－初検.
006196      05 連料－負担率                 PIC 9(3).
006197      05 連料－初検料                 PIC 9(5).
006198      05 連料－初検加算.
006199        07 連料－初検時間外           PIC 9.
006200        07 連料－初検休日             PIC 9.
006201        07 連料－初検深夜             PIC 9.
006202      05 連料－初検加算料             PIC 9(5).
006203   03 連料－再検料                    PIC 9(5).
006204   03 連料－往療.
006205      05 連料－往療距離               PIC 9(2)V9.
006206      05 連料－往療回数               PIC 9(2).
006207      05 連料－往療料                 PIC 9(6).
006208      05 連料－往療加算料             PIC 9(5).
006209      05 連料－往療加算.
006210        07 連料－往療夜間             PIC 9.
006211        07 連料－往療難路             PIC 9.
006212        07 連料－往療暴風雨           PIC 9.
006213        07 連料－往療時間外           PIC 9.
006214   03 連料－金属副子加算料            PIC 9(5).
006215   03 連料－金属副子数.
006216      05 連料－大                     PIC 9.
006217      05 連料－中                     PIC 9.
006218      05 連料－小                     PIC 9.
006219   03 連料－施術情報提供料            PIC 9(5).
006220   03 連料－小計                      PIC 9(7).
006221   03 連料－費用額                    PIC 9(6).
006222   03 連料－一部負担金                PIC 9(6).
006223   03 連料－請求額                    PIC 9(6).
006224   03 連料－給付割合                  PIC 9(3).
006225   03 連料－受給者負担額              PIC 9(6).
006226   03 連料－助成請求金額              PIC 9(6).
006227*
006228   03 連料－受給者負担額内訳   OCCURS  10.
006229      05 連料－当日分負担額           PIC 9(4).
006230*
006231*
006232* 負傷部位毎の料金
006233***********************
006234 01 連料－料金２ IS EXTERNAL.
006235   03 連料－初回処置    OCCURS   9.
006236      05 連料－初回処置料             PIC 9(5).
006237   03 連料－初回処置料計              PIC 9(6).
006238*
006239* 逓減毎の料金
006240***********************
006241 01 連料－料金３ IS EXTERNAL.
006242**********
006243* １部位 *
006244**********
006245   03 連料－部位１.
006246      05 連料－後療１.
006247         07 連料－後療単価１          PIC 9(4).
006248         07 連料－後療回数１          PIC 9(2).
006249         07 連料－後療料１            PIC 9(5).
006250      05 連料－冷罨法１.
006251         07 連料－冷罨法回数１        PIC 9(2).
006252         07 連料－冷罨法料１          PIC 9(4).
006253      05 連料－温罨法１.
006254         07 連料－温罨法回数１        PIC 9(2).
006255         07 連料－温罨法料１          PIC 9(4).
006256      05 連料－電療１.
006257         07 連料－電療回数１          PIC 9(2).
006258         07 連料－電療料１            PIC 9(4).
006259      05 連料－小計１                 PIC 9(6).
006260      05 連料－長期逓減率１           PIC 9(3).
006261      05 連料－長期込小計１           PIC 9(6).
006262**********
006263* ２部位 *
006264**********
006265   03 連料－部位２.
006266      05 連料－後療２.
006267         07 連料－後療単価２          PIC 9(4).
006268         07 連料－後療回数２          PIC 9(2).
006269         07 連料－後療料２            PIC 9(5).
006270      05 連料－冷罨法２.
006271         07 連料－冷罨法回数２        PIC 9(2).
006272         07 連料－冷罨法料２          PIC 9(4).
006273      05 連料－温罨法２.
006274         07 連料－温罨法回数２        PIC 9(2).
006275         07 連料－温罨法料２          PIC 9(4).
006276      05 連料－電療２.
006277         07 連料－電療回数２          PIC 9(2).
006278         07 連料－電療料２            PIC 9(4).
006279      05 連料－小計２                 PIC 9(6).
006280      05 連料－長期逓減率２           PIC 9(3).
006281      05 連料－長期込小計２           PIC 9(6).
006282******************
006283* ３部位／８割 *
006284******************
006285   03 連料－部位３８.
006286      05 連料－後療３８.
006287         07 連料－後療単価３８          PIC 9(4).
006288         07 連料－後療回数３８          PIC 9(2).
006289         07 連料－後療料３８            PIC 9(5).
006290      05 連料－冷罨法３８.
006291         07 連料－冷罨法回数３８        PIC 9(2).
006292         07 連料－冷罨法料３８          PIC 9(4).
006293      05 連料－温罨法３８.
006294         07 連料－温罨法回数３８        PIC 9(2).
006295         07 連料－温罨法料３８          PIC 9(4).
006296      05 連料－電療３８.
006297         07 連料－電療回数３８          PIC 9(2).
006298         07 連料－電療料３８            PIC 9(4).
006299      05 連料－小計３８                 PIC 9(6).
006300      05 連料－多部位込小計３８         PIC 9(6).
006301      05 連料－長期逓減率３８           PIC 9(3).
006302      05 連料－長期込小計３８           PIC 9(6).
006303******************
006304* ３部位／１０割 *
006305******************
006306   03 連料－部位３０.
006307      05 連料－逓減開始月日３０.
006308         07 連料－逓減開始月３０        PIC 9(2).
006309         07 連料－逓減開始日３０        PIC 9(2).
006310      05 連料－後療３０.
006311         07 連料－後療単価３０          PIC 9(4).
006312         07 連料－後療回数３０          PIC 9(2).
006313         07 連料－後療料３０            PIC 9(5).
006314      05 連料－冷罨法３０.
006315         07 連料－冷罨法回数３０        PIC 9(2).
006316         07 連料－冷罨法料３０          PIC 9(4).
006317      05 連料－温罨法３０.
006318         07 連料－温罨法回数３０        PIC 9(2).
006319         07 連料－温罨法料３０          PIC 9(4).
006320      05 連料－電療３０.
006321         07 連料－電療回数３０          PIC 9(2).
006322         07 連料－電療料３０            PIC 9(4).
006323      05 連料－小計３０                 PIC 9(6).
006324      05 連料－長期逓減率３０           PIC 9(3).
006325      05 連料－長期込小計３０           PIC 9(6).
006326****************
006327* ４部位／５割 *
006328****************
006329   03 連料－部位４５.
006330      05 連料－後療４５.
006331         07 連料－後療単価４５          PIC 9(4).
006332         07 連料－後療回数４５          PIC 9(2).
006333         07 連料－後療料４５            PIC 9(5).
006334      05 連料－冷罨法４５.
006335         07 連料－冷罨法回数４５        PIC 9(2).
006336         07 連料－冷罨法料４５          PIC 9(4).
006337      05 連料－温罨法４５.
006338         07 連料－温罨法回数４５        PIC 9(2).
006339         07 連料－温罨法料４５          PIC 9(4).
006340      05 連料－電療４５.
006341         07 連料－電療回数４５          PIC 9(2).
006342         07 連料－電療料４５            PIC 9(4).
006343      05 連料－小計４５                 PIC 9(6).
006344      05 連料－多部位込小計４５         PIC 9(6).
006345      05 連料－長期逓減率４５           PIC 9(3).
006346      05 連料－長期込小計４５           PIC 9(6).
006347****************
006348* ４部位／８割 *
006349****************
006350   03 連料－部位４８.
006351      05 連料－逓減開始月日４８.
006352         07 連料－逓減開始月４８        PIC 9(2).
006353         07 連料－逓減開始日４８        PIC 9(2).
006354      05 連料－後療４８.
006355         07 連料－後療単価４８          PIC 9(4).
006356         07 連料－後療回数４８          PIC 9(2).
006357         07 連料－後療料４８            PIC 9(5).
006358      05 連料－冷罨法４８.
006359         07 連料－冷罨法回数４８        PIC 9(2).
006360         07 連料－冷罨法料４８          PIC 9(4).
006361      05 連料－温罨法４８.
006362         07 連料－温罨法回数４８        PIC 9(2).
006363         07 連料－温罨法料４８          PIC 9(4).
006364      05 連料－電療４８.
006365         07 連料－電療回数４８          PIC 9(2).
006366         07 連料－電療料４８            PIC 9(4).
006367      05 連料－小計４８                 PIC 9(6).
006368      05 連料－多部位込小計４８         PIC 9(6).
006369      05 連料－長期逓減率４８           PIC 9(3).
006370      05 連料－長期込小計４８           PIC 9(6).
006371******************
006372* ４部位／１０割 *
006373******************
006374   03 連料－部位４０.
006375      05 連料－逓減開始月日４０.
006376         07 連料－逓減開始月４０        PIC 9(2).
006377         07 連料－逓減開始日４０        PIC 9(2).
006378      05 連料－後療４０.
006379         07 連料－後療単価４０          PIC 9(4).
006380         07 連料－後療回数４０          PIC 9(2).
006381         07 連料－後療料４０            PIC 9(5).
006382      05 連料－冷罨法４０.
006383         07 連料－冷罨法回数４０        PIC 9(2).
006384         07 連料－冷罨法料４０          PIC 9(4).
006385      05 連料－温罨法４０.
006386         07 連料－温罨法回数４０        PIC 9(2).
006387         07 連料－温罨法料４０          PIC 9(4).
006388      05 連料－電療４０.
006389         07 連料－電療回数４０          PIC 9(2).
006390         07 連料－電療料４０            PIC 9(4).
006391      05 連料－小計４０                 PIC 9(6).
006392      05 連料－長期逓減率４０           PIC 9(3).
006393      05 連料－長期込小計４０           PIC 9(6).
006394********************
006395* ５部位／２．５割 *
006396********************
006397   03 連料－部位５２.
006398      05 連料－後療５２.
006399         07 連料－後療単価５２          PIC 9(4).
006400         07 連料－後療回数５２          PIC 9(2).
006401         07 連料－後療料５２            PIC 9(5).
006402      05 連料－冷罨法５２.
006403         07 連料－冷罨法回数５２        PIC 9(2).
006404         07 連料－冷罨法料５２          PIC 9(4).
006405      05 連料－温罨法５２.
006406         07 連料－温罨法回数５２        PIC 9(2).
006407         07 連料－温罨法料５２          PIC 9(4).
006408      05 連料－電療５２.
006409         07 連料－電療回数５２          PIC 9(2).
006410         07 連料－電療料５２            PIC 9(4).
006411      05 連料－小計５２                 PIC 9(6).
006412      05 連料－多部位込小計５２         PIC 9(6).
006413      05 連料－長期逓減率５２           PIC 9(3).
006414      05 連料－長期込小計５２           PIC 9(6).
006415****************
006416* ５部位／５割 *
006417****************
006418   03 連料－部位５５.
006419      05 連料－逓減開始月日５５.
006420         07 連料－逓減開始月５５        PIC 9(2).
006421         07 連料－逓減開始日５５        PIC 9(2).
006422      05 連料－後療５５.
006423         07 連料－後療単価５５          PIC 9(4).
006424         07 連料－後療回数５５          PIC 9(2).
006425         07 連料－後療料５５            PIC 9(5).
006426      05 連料－冷罨法５５.
006427         07 連料－冷罨法回数５５        PIC 9(2).
006428         07 連料－冷罨法料５５          PIC 9(4).
006429      05 連料－温罨法５５.
006430         07 連料－温罨法回数５５        PIC 9(2).
006431         07 連料－温罨法料５５          PIC 9(4).
006432      05 連料－電療５５.
006433         07 連料－電療回数５５          PIC 9(2).
006434         07 連料－電療料５５            PIC 9(4).
006435      05 連料－小計５５                 PIC 9(6).
006436      05 連料－多部位込小計５５         PIC 9(6).
006437      05 連料－長期逓減率５５           PIC 9(3).
006438      05 連料－長期込小計５５           PIC 9(6).
006439****************
006440* ５部位／８割 *
006441****************
006442   03 連料－部位５８.
006443      05 連料－逓減開始月日５８.
006444         07 連料－逓減開始月５８        PIC 9(2).
006445         07 連料－逓減開始日５８        PIC 9(2).
006446      05 連料－後療５８.
006447         07 連料－後療単価５８          PIC 9(4).
006448         07 連料－後療回数５８          PIC 9(2).
006449         07 連料－後療料５８            PIC 9(5).
006450      05 連料－冷罨法５８.
006451         07 連料－冷罨法回数５８        PIC 9(2).
006452         07 連料－冷罨法料５８          PIC 9(4).
006453      05 連料－温罨法５８.
006454         07 連料－温罨法回数５８        PIC 9(2).
006455         07 連料－温罨法料５８          PIC 9(4).
006456      05 連料－電療５８.
006457         07 連料－電療回数５８          PIC 9(2).
006458         07 連料－電療料５８            PIC 9(4).
006459      05 連料－小計５８                 PIC 9(6).
006460      05 連料－多部位込小計５８         PIC 9(6).
006461      05 連料－長期逓減率５８           PIC 9(3).
006462      05 連料－長期込小計５８           PIC 9(6).
006463******************
006464* ５部位／１０割 *
006465******************
006466   03 連料－部位５０.
006467      05 連料－逓減開始月日５０.
006468         07 連料－逓減開始月５０        PIC 9(2).
006469         07 連料－逓減開始日５０        PIC 9(2).
006470      05 連料－後療５０.
006471         07 連料－後療単価５０          PIC 9(4).
006472         07 連料－後療回数５０          PIC 9(2).
006473         07 連料－後療料５０            PIC 9(5).
006474      05 連料－冷罨法５０.
006475         07 連料－冷罨法回数５０        PIC 9(2).
006476         07 連料－冷罨法料５０          PIC 9(4).
006477      05 連料－温罨法５０.
006478         07 連料－温罨法回数５０        PIC 9(2).
006479         07 連料－温罨法料５０          PIC 9(4).
006480      05 連料－電療５０.
006481         07 連料－電療回数５０          PIC 9(2).
006482         07 連料－電療料５０            PIC 9(4).
006483      05 連料－小計５０                 PIC 9(6).
006484      05 連料－長期逓減率５０           PIC 9(3).
006485      05 連料－長期込小計５０           PIC 9(6).
006495*
009130*
009140 01 連レ－キー IS EXTERNAL.
009150    03 連レ－保険種別                  PIC 9(2).
009160*
009170* キー項目
009180 01 連計－キー IS EXTERNAL.
009190    03  連計－負担金計算区分           PIC 9.
009200    03  連計－本人家族区分             PIC 9.
009210    03  連計－保険種別                 PIC 9(2).
009220    03  連計－保険者番号               PIC X(10).
009230    03  連計－公費種別                 PIC 9(2).
009240    03  連計－市町村番号老人           PIC X(10).
009250    03  連計－助成種別                 PIC 9(2).
009260    03  連計－市町村番号助成           PIC X(10).
009270    03  連計－患者コード.
009280      05  連計－患者番号               PIC 9(6).
009290      05  連計－枝番                   PIC X.
009300    03  連計－施術和暦年月日.
009310      05  連計－施術和暦年月.
009320        07  連計－施術和暦             PIC 9.
009330        07  連計－施術年月.
009340          09  連計－施術年             PIC 9(2).
009350          09  連計－施術月             PIC 9(2).
009360      05  連計－施術日                 PIC 9(2).
009370    03  連計－費用額                   PIC 9(6).
009380    03  連計－負担額                   PIC 9(6).
009390    03  連計－請求額                   PIC 9(6).
009400    03  連計－費用額老人               PIC 9(6).
009410    03  連計－負担額老人               PIC 9(6).
009420    03  連計－請求額老人               PIC 9(6).
009430    03  連計－費用額助成               PIC 9(6).
009440    03  連計－負担額助成               PIC 9(5).
009450    03  連計－請求額助成               PIC 9(5).
009460    03  連計－実日数                   PIC 9(2).
009470    03  連計－負担率                   PIC 9(3).
009480    03  連計－負担率区分               PIC 9.
009490*
009500 01 連メ－キー IS EXTERNAL.
009510    03  連メ－メッセージ                 PIC N(20).
009520*
009530 01 連仮計算－キー IS EXTERNAL.
009540   03 連仮計算－患者コード.
009550     05 連仮計算－患者番号                    PIC 9(6).
009560     05 連仮計算－枝番                        PIC X(1).
009570   03 連仮計算－施術和暦年月日.
009580     05 連仮計算－施術和暦年月.
009590       07 連仮計算－施術和暦                  PIC 9.
009600       07 連仮計算－施術年月.
009610         09 連仮計算－施術年                  PIC 9(2).
009620         09 連仮計算－施術月                  PIC 9(2).
009630     05 連仮計算－施術日                      PIC 9(2).
009640   03 連仮計算－負傷和暦年月日.
009650     05 連仮計算－負傷和暦年月.
009660       07 連仮計算－負傷和暦                  PIC 9.
009670       07 連仮計算－負傷年月.
009680         09 連仮計算－負傷年                  PIC 9(2).
009690         09 連仮計算－負傷月                  PIC 9(2).
009700     05 連仮計算－負傷日                      PIC 9(2).
009710   03 連仮計算－部位数                        PIC 9(1).
009720   03 連仮計算－負担割合                      PIC 9(1).
009730   03 連仮計算－費用額                        PIC 9(6).
009740   03 連仮計算－負担額                        PIC 9(6).
009750*
009760 01 連施－キー IS EXTERNAL.
009770   03 連施－施記構築区分               PIC 9.
009780   03 連施－施術和暦年月日.
009790     05 連施－施術和暦                 PIC 9.
009800     05 連施－施術年月.
009810       07 連施－施術年                 PIC 9(2).
009820       07 連施－施術月                 PIC 9(2).
009830     05 連施－施術日                   PIC 9(2).
009840   03 連施－患者コード.
009850     05 連施－患者番号                 PIC 9(6).
009860     05 連施－枝番                     PIC X.
009870   03 連施－初検情報 OCCURS 5.
009880     05 連施－初検和暦年月日.
009890       07 連施－初検和暦               PIC 9.
009900       07 連施－初検年月.
009910         09 連施－初検年               PIC 9(2).
009920         09 連施－初検月               PIC 9(2).
009930       07 連施－初検日                 PIC 9(2).
009940*
009950 01 連ＳＥＫＩＲＯＫ－キー IS EXTERNAL.
009960   03 連ＳＥＫＩＲＯＫ－オープン区分   PIC 9.
009970*
009980* 受診者Ｆの枝番チェック
009990 01 連受診枝番ＣＨＫ－キー IS EXTERNAL.
010000    03 連受診枝番ＣＨＫ－施術和暦年月.
010010       05 連受診枝番ＣＨＫ－施術和暦          PIC 9.
010020       05 連受診枝番ＣＨＫ－施術年月.
010030          07 連受診枝番ＣＨＫ－施術年         PIC 9(2).
010040          07 連受診枝番ＣＨＫ－施術月         PIC 9(2).
010050    03 連受診枝番ＣＨＫ－患者コード.
010060       05 連受診枝番ＣＨＫ－患者番号          PIC 9(6).
010070       05 連受診枝番ＣＨＫ－枝番              PIC X.
010080*   OUT（0:なし、1:あり）
010090    03 連受診枝番ＣＨＫ－枝番あり             PIC 9.
010100    03 連受診枝番ＣＨＫ－後期高齢枝番あり     PIC 9.
010110******************************************************************
010120*                      PROCEDURE  DIVISION                       *
010130******************************************************************
010140 PROCEDURE               DIVISION.
010150************
010160*           *
010170* 初期処理   *
010180*           *
010190************
010200     PERFORM 初期化.
010210************
010220*           *
010230* 主処理     *
010240*           *
010250************
010260     MOVE 連仮計算－施術和暦年月 TO 受－施術和暦年月.
010270     MOVE 連仮計算－患者コード   TO 受－患者コード.
010280     READ 受診者情報Ｆ
010290     INVALID KEY
010300         MOVE  NC"　　　受診者Ｎｏがありません。" TO 連メ－メッセージ
010310         CALL   "MSG001"
010320         CANCEL "MSG001"
010330     NOT INVALID KEY
010340         PERFORM 連結項目セット施記
010350*/仮計算用開始日取得↓↓↓仮計算でセットされた負傷日以降最初の通院日を開始日とする。
010360         MOVE 連仮計算－患者番号  TO 施記－患者番号
010370         MOVE 連仮計算－枝番      TO 施記－枝番
010380         MOVE 連仮計算－負傷和暦  TO 施記－施術和暦
010390         MOVE 連仮計算－負傷年    TO 施記－施術年
010400         MOVE 連仮計算－負傷月    TO 施記－施術月
010410         MOVE 連仮計算－負傷日    TO 施記－施術日
010420         START 施術記録Ｆ   KEY IS >= 施記－患者コード
010430                                      施記－施術和暦年月日
010440         END-START
010450         IF 状態キー = "00"
010460             READ 施術記録Ｆ NEXT
010470             NOT AT END
010480                 IF (施記－患者コード   = 連仮計算－患者コード  ) AND
010490                    (施記－施術和暦年月 = 連仮計算－施術和暦年月)
010500                     MOVE 施記－施術和暦年月日 TO 連施－初検和暦年月日(1)
010510                 END-IF
010520             END-READ
010530         END-IF
010540*/仮計算用開始日取得↑↑↑
010550*        / オープン区分:0 (オープンあり）
010560         MOVE ZERO TO 連ＳＥＫＩＲＯＫ－オープン区分
010570*/SEKIRECを組み込む
010580*         CALL   "SEKIREC"
010590*         CANCEL "SEKIREC"
010600         PERFORM 施記レコード作成
010610     END-READ.
010620*
010630     PERFORM 料金取得.
010640*
010650     MOVE 連料－費用額 TO 連仮計算－費用額
010660     COMPUTE 連仮計算－負担額 = 連仮計算－費用額 * 連仮計算－負担割合 / 10
010670*窓口金 負担額の１の位を四捨五入
010680     COMPUTE 連仮計算－負担額 ROUNDED = 連仮計算－負担額 / 10
010690     COMPUTE 連仮計算－負担額         = 連仮計算－負担額 * 10
010700************
010710*           *
010720* 終了処理   *
010730*           *
010740************
010750     PERFORM 終了処理.
010760     MOVE 1 TO PROGRAM-STATUS.
010770     EXIT PROGRAM.
010780*
010790*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
010800*================================================================*
010810 初期化 SECTION.
010820*
010830     OPEN INPUT 施術記録Ｆ
010840         MOVE NC"施記" TO ファイル名
010850         PERFORM オープンチェック
010860     OPEN INPUT 負傷データＦ
010870         MOVE NC"負傷" TO ファイル名
010880         PERFORM オープンチェック
010890     OPEN INPUT 計算マスタ
010900         MOVE NC"計算" TO ファイル名
010910         PERFORM オープンチェック
010920     OPEN INPUT 元号マスタ
010930         MOVE NC"元号" TO ファイル名
010940         PERFORM オープンチェック
010950     OPEN INPUT 受診者情報Ｆ.
010960         MOVE NC"受診" TO ファイル名.
010970         PERFORM オープンチェック.
010980     OPEN INPUT 料金マスタ.
010990         MOVE NC"料金" TO ファイル名.
011000         PERFORM オープンチェック.
011010*================================================================*
011020 オープンチェック SECTION.
011030*
011040     IF 状態キー  NOT =  "00"
011050         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
011060         DISPLAY NC"状態キー：" 状態キー         UPON CONS
011070         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
011071*-----------------------------------------*
011072         CALL "actcshm"  WITH C LINKAGE
011073*-----------------------------------------*
011080         ACCEPT  キー入力 FROM CONS
011090         PERFORM ファイル閉鎖
011100         EXIT PROGRAM.
011110*================================================================*
011120 終了処理 SECTION.
011130*
011140     PERFORM ファイル閉鎖.
011150*================================================================*
011160 ファイル閉鎖 SECTION.
011170*
011180     CLOSE 受診者情報Ｆ 施術記録Ｆ 負傷データＦ 計算マスタ
011190           元号マスタ   料金マスタ.
011200*================================================================*
011210 エラー表示 SECTION.
011220*
011230     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
011240     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
011250     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
011260     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
011261*-----------------------------------------*
011262     CALL "actcshm"  WITH C LINKAGE.
011263*-----------------------------------------*
011270     ACCEPT  キー入力 FROM CONS.
011280*================================================================*
011290 エラー表示Ｒ SECTION.
011300*
011310     DISPLAY ファイル名 NC"ファイル読込エラー"     UPON CONS.
011320     DISPLAY NC"状態キー：" 状態キー               UPON CONS.
011330     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
011340     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
011341*-----------------------------------------*
011342     CALL "actcshm"  WITH C LINKAGE.
011343*-----------------------------------------*
011350     ACCEPT  キー入力 FROM CONS.
011360     PERFORM ファイル閉鎖.
011370     EXIT PROGRAM.
011380*================================================================*
011390 連結項目セット施記 SECTION.
011400*
011410     MOVE SPACE TO 連施－キー.
011420     INITIALIZE    連施－キー.
011430*
011440     MOVE ZERO                TO 連施－施記構築区分.    
011450     MOVE 連仮計算－施術和暦  TO 連施－施術和暦.    
011460     MOVE 連仮計算－施術年    TO 連施－施術年.      
011470     MOVE 連仮計算－施術月    TO 連施－施術月.      
011480     MOVE 連仮計算－施術日    TO 連施－施術日.      
011490     MOVE 連仮計算－患者番号  TO 連施－患者番号.
011500     MOVE 連仮計算－枝番      TO 連施－枝番.    
011510*
011520*================================================================*
011530 料金取得 SECTION.
011540*
011550     INITIALIZE 連料－料金１
011560                連料－料金２
011570                連料－料金３.
011580     INITIALIZE 退避項目ＧＷ.
011590*
011600*     PERFORM レセプト種別取得.
011610**
011620*     EVALUATE レセプト種類Ｗ
011630*         WHEN "KOKU"
011640*         WHEN "SYAH"
011650*         WHEN "SYAK"
011660*         WHEN "KUMH"
011670*         WHEN "KUMK"
011680*         WHEN "KYOH"
011690*         WHEN "KYOK"
011700*         WHEN "TAIS"
011710*         WHEN "JIEI"
011720*             MOVE 受－保険種別         TO レセプト種別ＧＷ
011730*         WHEN "ROUJ"
011740*             MOVE 受－公費種別         TO レセプト種別ＧＷ
011750*         WHEN "JYOS"
011760*             MOVE 受－助成種別         TO レセプト種別ＧＷ
011770*     END-EVALUATE.
011780*/国保データとして計算する
011790     MOVE 01 TO レセプト種別ＧＷ
011800*    ********
011810*    * 計算 *
011820*    ********
011830     PERFORM 計算連結項目セット２.
011840     MOVE 2   TO  連計－負担金計算区分.
011850*/レセプトＤの組込
011860*     CALL   "RESEPTD".
011870*     CANCEL "RESEPTD".
011880     PERFORM 費用額計算.
011890*
011900     IF レセプト種類Ｗ = "JYOS"
011910         MOVE 連料－受給者負担額 TO 一部負担金ＷＲ
011920     ELSE
011930         MOVE 連料－一部負担金   TO 一部負担金ＷＲ
011940     END-IF.
011950*
011960*================================================================*
011970 計算連結項目セット２ SECTION.
011980*
011990     MOVE SPACE TO 連計－キー.
012000     INITIALIZE    連計－キー.
012010*
012020     MOVE レセプト種別ＧＷ   TO 連レ－保険種別.
012030     MOVE 受－患者番号       TO 連計－患者番号.
012040     MOVE 受－枝番           TO 連計－枝番.
012050     MOVE 受－施術和暦       TO 連計－施術和暦.
012060     MOVE 受－施術年         TO 連計－施術年.
012070     MOVE 受－施術月         TO 連計－施術月.
012080     MOVE 連仮計算－施術日   TO 連計－施術日.
012090     MOVE ZERO               TO 連計－負担金計算区分.
012100     MOVE 受－本人家族区分   TO 連計－本人家族区分.
012110     MOVE 受－保険種別       TO 連計－保険種別.
012120     MOVE 01       TO 連計－保険種別.
012130     MOVE 受－保険者番号     TO 連計－保険者番号.
012140     MOVE 受－公費種別       TO 連計－公費種別.
012150     MOVE 受－費用負担者番号 TO 連計－市町村番号老人.
012160     IF レセプト種別ＧＷ NOT = 05
012170         MOVE 受－助成種別           TO 連計－助成種別      
012180         MOVE 受－費用負担者番号助成 TO 連計－市町村番号助成
012190     END-IF.
012200*
012210*================================================================*
012220* レセプト種別取得 SECTION.
012230**
012240*     IF 受－助成種別  = ZERO
012250*         EVALUATE TRUE
012260*         WHEN (受－保険種別 = 01) AND (受－公費種別 = ZERO)
012270*            MOVE "KOKU"    TO レセプト種類Ｗ
012280*         WHEN (受－保険種別 = 02 OR 06 OR 07) AND (受－本人家族区分 = 1) AND (受－公費種別 = ZERO)
012290*            MOVE "SYAH"    TO レセプト種類Ｗ
012300*         WHEN (受－保険種別 = 02 OR 06 OR 07) AND (受－本人家族区分 = 2) AND (受－公費種別 = ZERO)
012310*            MOVE "SYAK"    TO レセプト種類Ｗ
012320*         WHEN (受－保険種別 = 03) AND (受－本人家族区分 = 1) AND (受－公費種別 = ZERO)
012330*            MOVE "KUMH"    TO レセプト種類Ｗ
012340*         WHEN (受－保険種別 = 03) AND (受－本人家族区分 = 2) AND (受－公費種別 = ZERO)
012350*            MOVE "KUMK"    TO レセプト種類Ｗ
012360*         WHEN (受－保険種別 = 04) AND (受－本人家族区分 = 1) AND (受－公費種別 = ZERO)
012370*            MOVE "KYOH"    TO レセプト種類Ｗ
012380*         WHEN (受－保険種別 = 04) AND (受－本人家族区分 = 2) AND (受－公費種別 = ZERO)
012390*            MOVE "KYOK"    TO レセプト種類Ｗ
012400*         WHEN 受－公費種別 = 05
012410*            MOVE "ROUJ"    TO レセプト種類Ｗ
012420*         WHEN (受－保険種別 = 08) AND (受－公費種別 = ZERO)
012430*            MOVE "TAIS"    TO レセプト種類Ｗ
012440*         WHEN (受－保険種別 = 09) AND (受－公費種別 = ZERO)
012450*            MOVE "JIEI"    TO レセプト種類Ｗ
012460*         WHEN OTHER
012470*            CONTINUE
012480*         END-EVALUATE
012490*     ELSE
012500*** 助成がある時
012510*         MOVE "JYOS"    TO レセプト種類Ｗ
012520*     END-IF.
012530**
012540*================================================================*
012550 施記レコード作成 SECTION.
012560*
012570*/SEKIRECの組み込み
012580     MOVE 連施－施術和暦 TO 負－施術和暦 施術和暦ＣＷ.
012590     MOVE 連施－施術年   TO 負－施術年   施術年ＣＷ.
012600     MOVE 連施－施術月   TO 負－施術月   施術月ＣＷ.
012610     MOVE 連施－施術日   TO              施術日ＣＷ.
012620     MOVE 連施－患者番号 TO 負－患者番号.
012630     MOVE 連施－枝番     TO 負－枝番.
012640*
012650     READ 負傷データＦ
012660     INVALID KEY
012670         MOVE NC"負傷" TO ファイル名
012680         PERFORM エラー表示Ｒ
012690     NOT INVALID KEY
012700*
012710*/仮計算処理。全部位捻挫、同時負傷のデータとして計算する。
012720         MOVE 連仮計算－部位数 TO 負－部位数
012730         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 負－部位数
012740             MOVE 連仮計算－負傷和暦年月日 TO 負－負傷和暦年月日(部位ＣＮＴ)
012750             MOVE 01                       TO 負－負傷種別      (部位ＣＮＴ)
012760             MOVE 連施－初検和暦年月日(1)  TO 負－開始和暦年月日(部位ＣＮＴ)
012770         END-PERFORM
012780*
012790* 診療区分＝後療or初検のセットと再検料請求区分のセットを行う
012800         IF 負－初検請求区分 = 1 OR 2
012810             MOVE SPACE TO 再検料請求フラグ
012820         ELSE
012830             PERFORM 再検料請求セット
012840         END-IF
012850*
012860* 書換（ＲＥＷＲＩＴＥ）に対応できるようＲＥＡＤしておく（重要）
012870         MOVE 連施－施術和暦 TO 施記－施術和暦
012880         MOVE 連施－施術年   TO 施記－施術年
012890         MOVE 連施－施術月   TO 施記－施術月
012900         MOVE 連施－施術日   TO 施記－施術日
012910         MOVE 連施－患者番号 TO 施記－患者番号
012920         MOVE 連施－枝番     TO 施記－枝番
012930         READ 施術記録Ｆ
012940         INVALID KEY
012950             MOVE SPACE TO 施記－レコード
012960             INITIALIZE    施記－レコード
012970             MOVE 連施－施術和暦 TO 施記－施術和暦
012980             MOVE 連施－施術年   TO 施記－施術年
012990             MOVE 連施－施術月   TO 施記－施術月
013000             MOVE 連施－施術日   TO 施記－施術日
013010             MOVE 連施－患者番号 TO 施記－患者番号
013020             MOVE 連施－枝番     TO 施記－枝番
013030         NOT INVALID KEY
013040*
013050* 既存レコードの部位算定情報を更新
013060             PERFORM 修正算定情報初期化
013070         END-READ
013080* 初検料請求
013090         MOVE SPACE TO 終了フラグ
013100         MOVE 1    TO  施記－診療区分
013110         PERFORM VARYING 初検カウンタ FROM 1 BY 1
013120                 UNTIL ( 初検カウンタ     > 5      ) OR
013130                       ( 終了フラグ   NOT = SPACE  )
013140             IF ( 連施－初検和暦(初検カウンタ) = ZERO ) OR
013150                ( 連施－初検年(初検カウンタ)   = ZERO ) OR
013160                ( 連施－初検月(初検カウンタ)   = ZERO ) OR
013170                ( 連施－初検日(初検カウンタ)   = ZERO )
013180                 MOVE "YES" TO 終了フラグ
013190             ELSE
013200                 IF 連施－初検和暦年月日(初検カウンタ) = 連施－施術和暦年月日
013210                     MOVE 2    TO  施記－診療区分
013220                     IF 負－初検請求区分 = 1 OR 2
013230                         MOVE ZERO TO 施記－初検料請求区分
013240                     ELSE
013250                         MOVE 1    TO 施記－初検料請求区分
013260                     END-IF
013270*----------------------------------------------------------------------------------*
013280*                    / 2008/06 改定　初検時相談料区分 0：加算あり　1：加算なし
013290                     IF 負－初検時相談料区分 = 1
013300                         MOVE 1    TO 施記－初検時相談料区分
013310                     ELSE
013320                         MOVE ZERO TO 施記－初検時相談料区分
013330                     END-IF
013340*----------------------------------------------------------------------------------*
013350                     MOVE "YES" TO 終了フラグ
013360                 END-IF
013370             END-IF
013380         END-PERFORM
013390*
013400* 再検料請求
013410         MOVE SPACE TO 再検料初期化フラグ
013420         IF ( 再検料請求フラグ NOT = SPACE ) AND
013430            ( 施記－診療区分   NOT = 2     )
013440             MOVE 1     TO 施記－再検料請求
013450             MOVE "YES" TO 再検料初期化フラグ
013460         ELSE
013470             MOVE ZERO  TO 施記－再検料請求
013480             MOVE SPACE TO 再検料初期化フラグ
013490         END-IF
013500*
013510         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
013520                 UNTIL 部位ＣＮＴ > 負－部位数
013530* 無病の場合、以下の算定は行わない。
013540             IF 負－負傷種別(部位ＣＮＴ) NOT = 09
013550                 IF ( 負－終了和暦(部位ＣＮＴ) = ZERO ) OR
013560                    ( 負－終了年(部位ＣＮＴ)   = ZERO ) OR
013570                    ( 負－終了月(部位ＣＮＴ)   = ZERO ) OR
013580                    ( 負－終了日(部位ＣＮＴ)   = ZERO )
013590                     MOVE "9999999" TO 終了和暦年月日ＣＷ
013600                 ELSE
013610                     MOVE 負－終了和暦年月日(部位ＣＮＴ) TO 終了和暦年月日ＣＷ
013620                 END-IF
013630*
013640* 部位コード・転帰区分は、施術日に関係なく転記する。
013650                 MOVE 負－部位コード(部位ＣＮＴ) TO 施記－部位コード(部位ＣＮＴ)
013660                 MOVE 負－転帰区分(部位ＣＮＴ)   TO 施記－転帰区分(部位ＣＮＴ)
013670*
013680                 IF ( 負－開始和暦年月日(部位ＣＮＴ) <= 施術和暦年月日ＣＷ ) AND
013690                    ( 終了和暦年月日ＣＷ             >= 施術和暦年月日ＣＷ )
013700                     PERFORM 算定情報取得
013710                     PERFORM 罨法算定処理
013720                     IF ( 負－開始和暦年月日(部位ＣＮＴ) = 施術和暦年月日ＣＷ ) AND
013730                        ( 負－算定区分(部位ＣＮＴ)   NOT = 1  ) AND
013740                        ( 負－負傷種別(部位ＣＮＴ)   NOT = 07 ) AND
013750                        ( 負－負傷種別(部位ＣＮＴ)   NOT = 08 )
013760                         MOVE 1 TO 施記－整復施療区分(部位ＣＮＴ)
013770                     END-IF
013780                     PERFORM 後療算定処理
013790*                    ／＊ 医師拘縮　＊／
013800                     IF 負－負傷種別(部位ＣＮＴ) = 07 OR 08
013810                         MOVE 1 TO 施記－医師拘縮区分(部位ＣＮＴ)
013820                     END-IF
013830                 END-IF
013840             ELSE
013850* 部位コード・転帰区分は、施術日に関係なく転記する。
013860                 MOVE 負－部位コード(部位ＣＮＴ) TO 施記－部位コード(部位ＣＮＴ)
013870             END-IF
013880         END-PERFORM
013890*
013900*/仮計算用の為、書き込みせず退避する。
013910*         PERFORM 施記レコード書込
013920         MOVE 施記－レコード TO 退避施記－レコード
013930*
013940**    / 再検日以降の再検料初期化 /
013950*         IF 再検料初期化フラグ = "YES"
013960*            PERFORM 再検料初期化処理
013970*         END-IF
013980*
013990     END-READ.
014000*================================================================*
014010 罨法算定処理 SECTION.
014020*
014030     MOVE 負－負傷和暦(部位ＣＮＴ) TO 計算和暦Ｗ.
014040     MOVE 負－負傷年(部位ＣＮＴ)   TO 計算年Ｗ.
014050     MOVE 負－負傷月(部位ＣＮＴ)   TO 計算月Ｗ.
014060     MOVE 負－負傷日(部位ＣＮＴ)   TO 計算日Ｗ.
014070     PERFORM 西暦年取得.
014080     PERFORM 日付絶対値計算.
014090     MOVE 絶対値Ｗ                 TO 負傷絶対値Ｗ.
014100*
014110     MOVE 連施－施術和暦           TO 計算和暦Ｗ.
014120     MOVE 連施－施術年             TO 計算年Ｗ.
014130     MOVE 連施－施術月             TO 計算月Ｗ.
014140     MOVE 連施－施術日             TO 計算日Ｗ.
014150     PERFORM 西暦年取得.
014160     PERFORM 日付絶対値計算.
014170     MOVE 絶対値Ｗ                 TO 施術絶対値Ｗ.
014180*
014190     COMPUTE 負傷経過日Ｗ = 施術絶対値Ｗ - ( 負傷絶対値Ｗ - 1 ).
014200*
014210     EVALUATE 負－負傷種別(部位ＣＮＴ)
014220* 不全骨折・骨折（医師拘縮も含む）
014230     WHEN 05
014240     WHEN 06
014250     WHEN 07
014260     WHEN 08
014270         IF 負傷経過日Ｗ <= 冷罨日数骨折Ｗ
014280             MOVE 1 TO 施記－罨法区分(部位ＣＮＴ)
014290         END-IF
014300         IF 負傷経過日Ｗ > 温罨待機骨折Ｗ
014310* 　　　　　 ／＊　整復・固定を行った初検の日は温罨法料算定できない　＊／
014320             IF ( 負－算定区分(部位ＣＮＴ)    NOT = 1  ) AND
014330                ( 負－負傷種別(部位ＣＮＴ)    NOT = 07 ) AND
014340                ( 負－負傷種別(部位ＣＮＴ)    NOT = 08 ) AND
014350                ( 負－開始和暦年月日(部位ＣＮＴ)  = 施術和暦年月日ＣＷ )
014360                 MOVE ZERO TO 施記－罨法区分(部位ＣＮＴ)
014370                 MOVE ZERO TO 施記－電療区分(部位ＣＮＴ)
014380             ELSE
014390* 　　　　　　　　／＊　後療から行う場合は算定ＯＫ　＊／
014400                 MOVE 2 TO 施記－罨法区分(部位ＣＮＴ)
014410                 IF 負－電療請求区分(部位ＣＮＴ) = ZERO
014420                     MOVE 1 TO 施記－電療区分(部位ＣＮＴ)
014430                 END-IF
014440             END-IF
014450         END-IF
014460* 脱臼
014470     WHEN 04
014480         IF 負傷経過日Ｗ <= 冷罨日数脱臼Ｗ
014490             MOVE 1 TO 施記－罨法区分(部位ＣＮＴ)
014500         END-IF
014510         IF 負傷経過日Ｗ > 温罨待機脱臼Ｗ
014520* 　　　　　／＊　整復・施療を行った初検の日は温罨法料算定できない　＊／
014530             IF ( 負－算定区分(部位ＣＮＴ)    NOT = 1  ) AND
014540                ( 負－負傷種別(部位ＣＮＴ)    NOT = 07 ) AND
014550                ( 負－負傷種別(部位ＣＮＴ)    NOT = 08 ) AND
014560                ( 負－開始和暦年月日(部位ＣＮＴ)  = 施術和暦年月日ＣＷ )
014570                 MOVE ZERO TO 施記－罨法区分(部位ＣＮＴ)
014580                 MOVE ZERO TO 施記－電療区分(部位ＣＮＴ)
014590             ELSE
014600* 　　　　　　　　／＊　後療から行う場合は算定ＯＫ　＊／
014610                 MOVE 2 TO 施記－罨法区分(部位ＣＮＴ)
014620                 IF 負－電療請求区分(部位ＣＮＴ) NOT = ZERO
014630                     MOVE ZERO TO 施記－電療区分(部位ＣＮＴ)
014640                 ELSE
014650                     MOVE 1 TO 施記－電療区分(部位ＣＮＴ)
014660                 END-IF
014670             END-IF
014680         END-IF
014690* 打撲・捻挫・挫傷
014700     WHEN 01
014710     WHEN 02
014720     WHEN 03
014730         IF 負傷経過日Ｗ <= 冷罨日数打撲Ｗ
014740             EVALUATE 負傷経過日Ｗ
014750* 負傷日の場合は、冷罨法算定
014760             WHEN 1
014770                 MOVE 1 TO 施記－罨法区分(部位ＣＮＴ)
014780* 負傷日の翌日場合は、初検日のみ冷罨法算定
014790             WHEN 2
014800                 IF 負－開始和暦年月日(部位ＣＮＴ) = 施術和暦年月日ＣＷ
014810                      MOVE 1 TO 施記－罨法区分(部位ＣＮＴ)
014820                 END-IF
014830             WHEN OTHER
014840                 CONTINUE
014850             END-EVALUATE
014860         END-IF
014870         IF 負傷経過日Ｗ > 温罨待機脱臼Ｗ
014880* 　　　　　／＊　整復・施療を行った初検の日は温罨法料算定できない　＊／
014890             IF ( 負－算定区分(部位ＣＮＴ)    NOT = 1  ) AND
014900                ( 負－負傷種別(部位ＣＮＴ)    NOT = 07 ) AND
014910                ( 負－負傷種別(部位ＣＮＴ)    NOT = 08 ) AND
014920                ( 負－開始和暦年月日(部位ＣＮＴ)  = 施術和暦年月日ＣＷ )
014930                 MOVE ZERO TO 施記－罨法区分(部位ＣＮＴ)
014940                 MOVE ZERO TO 施記－電療区分(部位ＣＮＴ)
014950             ELSE
014960* 　　　　　　　　／＊　後療から行う場合は算定ＯＫ　＊／
014970                 MOVE 2 TO 施記－罨法区分(部位ＣＮＴ)
014980                 IF 負－電療請求区分(部位ＣＮＴ) = ZERO
014990                     MOVE 1 TO 施記－電療区分(部位ＣＮＴ)
015000                 END-IF
015010             END-IF
015020         END-IF
015030     WHEN OTHER
015040         CONTINUE
015050     END-EVALUATE.
015060*
015070*================================================================*
015080 西暦年取得 SECTION.
015090*
015100     MOVE 計算和暦Ｗ TO 元－元号区分.
015110     READ 元号マスタ
015120     INVALID KEY
015130         DISPLAY NC"指定和暦が登録されていません" UPON CONS
015140         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
015141*-----------------------------------------*
015142         CALL "actcshm"  WITH C LINKAGE
015143*-----------------------------------------*
015150         ACCEPT  キー入力 FROM CONS
015160         PERFORM 終了処理
015170         EXIT PROGRAM
015180     NOT INVALID KEY
015190         COMPUTE 西暦年Ｗ = 元－開始西暦年 + 計算年Ｗ - 1
015200     END-READ.
015210*
015220*================================================================*
015230 日付絶対値計算 SECTION.
015240*
015250** 1989年を基準とし、当年まで閏年が何回あるかを計算
015260** 元号マスタの西暦開始年確認。
015270*     COMPUTE 西暦年差Ｗ   = 西暦年Ｗ - 1989 - 1.
015280*     DIVIDE 4 INTO 西暦年差Ｗ GIVING 閏年回数Ｗ
015290*                            REMAINDER 剰余Ｗ
015300*     END-DIVIDE.
015310*/基準年1989年から当年までの閏年の回数を計算(当年は含まない)
015320*/始めのうるう年の1992年から数える
015330     COMPUTE 西暦年差Ｗ   = 西暦年Ｗ - 1989 - 1.
015340     MOVE 1992 TO 加算年Ｗ.
015350     MOVE ZERO TO 閏年回数Ｗ.
015360     PERFORM UNTIL 西暦年Ｗ <= 加算年Ｗ
015370         COMPUTE 加算年Ｗ   = 加算年Ｗ   + 4
015380         COMPUTE 閏年回数Ｗ = 閏年回数Ｗ + 1
015390     END-PERFORM.
015400*
015410     COMPUTE 通常年回数Ｗ     = 西暦年差Ｗ     - 閏年回数Ｗ.
015420     COMPUTE 通常年絶対値Ｗ   = 通常年回数Ｗ   * 365.
015430     COMPUTE 閏年絶対値Ｗ     = 閏年回数Ｗ     * 366.
015440     COMPUTE 年絶対値Ｗ       = 通常年絶対値Ｗ + 閏年絶対値Ｗ.
015450* 当年がうるう年かどうかを調べる（２月の末日計算）
015460*     COMPUTE 西暦年差２Ｗ     = 西暦年Ｗ + 1.
015470*     DIVIDE 4 INTO 西暦年差２Ｗ GIVING 閏年回数Ｗ
015480*                                REMAINDER 剰余Ｗ
015490*     END-DIVIDE.
015500     DIVIDE 4 INTO 西暦年Ｗ GIVING 閏年回数Ｗ
015510                            REMAINDER 剰余Ｗ
015520     END-DIVIDE.
015530*
015540     MOVE ZERO TO 絶対値月Ｗ.
015550*
015560* 前月までの絶対値を累計
015570     COMPUTE 絶対値月Ｗ２ = 計算月Ｗ * 30.
015580*
015590     EVALUATE 計算月Ｗ
015600     WHEN 3
015610         COMPUTE 絶対値月Ｗ   = 絶対値月Ｗ２ - 1
015620     WHEN 2
015630     WHEN 6
015640     WHEN 7
015650         COMPUTE 絶対値月Ｗ   = 絶対値月Ｗ２ + 1
015660     WHEN 8
015670         COMPUTE 絶対値月Ｗ   = 絶対値月Ｗ２ + 2
015680     WHEN 9
015690     WHEN 10
015700         COMPUTE 絶対値月Ｗ   = 絶対値月Ｗ２ + 3
015710     WHEN 11
015720     WHEN 12
015730         COMPUTE 絶対値月Ｗ   = 絶対値月Ｗ２ + 4
015740     WHEN OTHER
015750         MOVE 絶対値月Ｗ２ TO 絶対値月Ｗ
015760     END-EVALUATE.
015770*
015780     IF ( 剰余Ｗ = ZERO ) AND
015790        ( 計算月Ｗ > 2  )
015800         COMPUTE 絶対値月Ｗ = 絶対値月Ｗ + 1
015810     END-IF.
015820*
015830     COMPUTE 絶対値Ｗ   = 年絶対値Ｗ + 絶対値月Ｗ + 計算日Ｗ.
015840*
015850*================================================================*
015860 算定情報取得 SECTION.
015870*
015880* 健康保険：０１
015890*
015900     MOVE 01             TO 計－制御区分.
015910     MOVE 連施－施術和暦 TO 計－開始和暦 施術和暦ＣＷ.
015920     MOVE 連施－施術年   TO 計－開始年   施術年ＣＷ.
015930     MOVE 連施－施術月   TO 計－開始月   施術月ＣＷ.
015940*
015950     START 計算マスタ KEY IS <= 計－制御区分
015960                                計－開始和暦年月
015970                                REVERSED
015980     END-START.
015990*
016000     IF 状態キー = "00"
016010*
016020         READ 計算マスタ NEXT
016030         AT END
016040             DISPLAY NC"施術年月に対応した算定情報がみつかりません" UPON CONS
016041*-----------------------------------------*
016042             CALL "actcshm"  WITH C LINKAGE
016043*-----------------------------------------*
016050             PERFORM 終了処理
016060             EXIT PROGRAM
016070         NOT AT END
016080             IF ( 施術和暦年月ＣＷ >= 計Ａ－開始和暦年月 ) AND
016090                ( 施術和暦年月ＣＷ <= 計Ａ－終了和暦年月 )
016100*
016110                 MOVE 計Ａ－冷罨日数骨折 TO 冷罨日数骨折Ｗ
016120                 MOVE 計Ａ－冷罨日数脱臼 TO 冷罨日数脱臼Ｗ
016130                 MOVE 計Ａ－冷罨日数打撲 TO 冷罨日数打撲Ｗ
016140                 MOVE 計Ａ－温罨待機骨折 TO 温罨待機骨折Ｗ
016150                 MOVE 計Ａ－温罨待機脱臼 TO 温罨待機脱臼Ｗ
016160             ELSE
016170                 DISPLAY NC"施術年月に対応した算定情報がみつかりません" UPON CONS
016171*-----------------------------------------*
016172                 CALL "actcshm"  WITH C LINKAGE
016173*-----------------------------------------*
016180                 PERFORM 終了処理
016190                 EXIT PROGRAM
016200             END-IF
016210         END-READ
016220     END-IF.
016230*
016240*================================================================*
016250* 施記レコード書込 SECTION.
016260**
016270*     WRITE 施記－レコード
016280*     END-WRITE.
016290**
016300*     EVALUATE 状態キー
016310*     WHEN "00"
016320*         CONTINUE
016330*     WHEN "22"
016340*         REWRITE 施記－レコード
016350*         INVALID KEY
016360*             MOVE NC"施記" TO ファイル名
016370*             PERFORM エラー表示
016380*         END-REWRITE
016390*     WHEN OTHER
016400*         MOVE NC"施記" TO ファイル名
016410*         PERFORM エラー表示
016420*     END-EVALUATE.
016430**
016440*================================================================*
016450 再検料請求セット SECTION.
016460*
016470******************************************************************
016480* 初検後療日に再検料を算定：初検日の次診療日かを判定し、
016490* 再検日の場合、再検料請求フラグに"YES"をたて、
016500* 施記－再検料請求をセットする
016510******************************************************************
016520*
016530     MOVE SPACE TO 再検料請求フラグ.
016540     MOVE SPACE TO 枝番チェックフラグ.
016550*
016560     MOVE 連施－施術和暦 TO 施記－施術和暦.
016570     MOVE 連施－施術年   TO 施記－施術年.
016580     MOVE 連施－施術月   TO 施記－施術月.
016590     MOVE 連施－施術日   TO 施記－施術日.
016600     MOVE 連施－患者番号 TO 施記－患者番号.
016610     MOVE 連施－枝番     TO 施記－枝番.
016620     START 施術記録Ｆ KEY IS < 施記－患者コード 
016630                               施記－施術和暦年月日
016640                               REVERSED
016650     END-START.
016660*
016670     IF 状態キー = "00"
016680         READ 施術記録Ｆ NEXT
016690         AT END
016700             MOVE "YES" TO 枝番チェックフラグ
016710         NOT AT END
016720* 診療区分：２＝初検日（初検料算定の日）
016730             IF ( 連施－患者コード = 施記－患者コード ) AND
016740                ( 施記－診療区分 = 2                  )
016750*               */前回診療日が当月か前月の時のみ/080912
016760                 PERFORM 前月判定
016770                 IF (前月フラグ = "YES") OR
016780                   ((連施－施術和暦 = 施記－施術和暦) AND
016790                    (連施－施術年   = 施記－施術年  ) AND
016800                    (連施－施術月   = 施記－施術月  ))
016810                     MOVE "YES" TO 再検料請求フラグ
016820                 END-IF
016830             END-IF
016840             IF (連施－患者コード NOT = 施記－患者コード) OR
016850                (連施－施術和暦   NOT = 施記－施術和暦  ) OR
016860                (連施－施術年     NOT = 施記－施術年    ) OR
016870                (連施－施術月     NOT = 施記－施術月    )
016880                 MOVE "YES" TO 枝番チェックフラグ
016890             END-IF
016900         END-READ
016910     END-IF.
016920*
016930*/当月で最初の通院日の時枝番の存在を調べ
016940*/当月に枝番があったら前枝番をチェックする/080912
016950     IF 枝番チェックフラグ = "YES"
016960         MOVE SPACE TO 連受診枝番ＣＨＫ－キー
016970         INITIALIZE    連受診枝番ＣＨＫ－キー
016980         MOVE 連施－施術和暦 TO 連受診枝番ＣＨＫ－施術和暦
016990         MOVE 連施－施術年   TO 連受診枝番ＣＨＫ－施術年
017000         MOVE 連施－施術月   TO 連受診枝番ＣＨＫ－施術月
017010         MOVE 連施－患者番号 TO 連受診枝番ＣＨＫ－患者番号
017020         MOVE SPACE          TO 連受診枝番ＣＨＫ－枝番
017030         CALL   "EDACHK"
017040         CANCEL "EDACHK"
017050*        （0:なし、1:あり）
017060         IF 連受診枝番ＣＨＫ－枝番あり = 1
017070             MOVE 連施－施術和暦 TO 受－施術和暦
017080             MOVE 連施－施術年   TO 受－施術年
017090             MOVE 連施－施術月   TO 受－施術月
017100             MOVE 連施－患者番号 TO 受－患者番号
017110             IF 連施－枝番 = SPACE
017120                 MOVE HIGH-VALUE TO 受－枝番
017130             ELSE
017140                 MOVE 連施－枝番 TO 受－枝番
017150             END-IF
017160* 
017170             START 受診者情報Ｆ KEY IS < 受－施術和暦年月
017180                                         受－患者コード
017190                                         REVERSED
017200             END-START
017210             IF 状態キー = "00"
017220                 READ 受診者情報Ｆ NEXT
017230                 NOT AT END
017240                     MOVE 連施－施術和暦 TO 施記－施術和暦
017250                     MOVE 連施－施術年   TO 施記－施術年
017260                     MOVE 連施－施術月   TO 施記－施術月
017270                     MOVE 連施－施術日   TO 施記－施術日
017280                     MOVE 受－患者番号   TO 施記－患者番号
017290                     MOVE 受－枝番       TO 施記－枝番
017300                     START 施術記録Ｆ KEY IS < 施記－患者コード
017310                                               施記－施術和暦年月日
017320                                               REVERSED
017330                     END-START
017340*
017350                     IF 状態キー = "00"
017360*/枝番あり、前通院日ありの時フラグをクリアしセットしなおす/
017370                         MOVE SPACE TO 再検料請求フラグ
017380                         READ 施術記録Ｆ NEXT
017390                         NOT AT END
017400* 診療区分：２＝初検日（初検料算定の日）
017410                             IF ( 受－患者コード = 施記－患者コード ) AND
017420                                ( 施記－診療区分 = 2                )
017430* 前回診療日が前月の時のみ/080912
017440                                 PERFORM 前月判定
017450                                 IF (前月フラグ = "YES") OR
017460                                   ((連施－施術和暦 = 施記－施術和暦) AND
017470                                    (連施－施術年   = 施記－施術年  ) AND
017480                                    (連施－施術月   = 施記－施術月  ))
017490                                     MOVE "YES" TO 再検料請求フラグ
017500                                 END-IF
017510                             END-IF
017520                         END-READ
017530                     END-IF
017540                 END-READ
017550             END-IF
017560         END-IF
017570     END-IF.
017580*
017590*================================================================*
017600 受診者情報Ｆ読込 SECTION.
017610*
017620     READ 受診者情報Ｆ NEXT
017630     AT END
017640         MOVE "YES" TO 終了フラグ２
017650     END-READ.
017660*================================================================*
017670 修正算定情報初期化 SECTION.
017680*
017690     MOVE 1    TO 施記－診療区分.
017700     MOVE ZERO TO 施記－再検料請求.
017710*
017720*/初期化項目は部位数分だけでなく全て初期化するように変更。020423
017730     PERFORM VARYING 負傷ＣＮＴ FROM 1 BY 1
017740*             UNTIL   負傷ＣＮＴ > 負－部位数
017750             UNTIL   負傷ＣＮＴ > 9
017760         MOVE ZERO TO 施記－負傷種別(負傷ＣＮＴ)
017770         MOVE ZERO TO 施記－部位(負傷ＣＮＴ)
017780         MOVE ZERO TO 施記－左右区分(負傷ＣＮＴ)
017790         MOVE ZERO TO 施記－負傷位置番号(負傷ＣＮＴ)
017800*
017810         MOVE ZERO TO 施記－転帰区分(負傷ＣＮＴ)
017820         MOVE ZERO TO 施記－整復施療区分(負傷ＣＮＴ)
017830         MOVE ZERO TO 施記－医師拘縮区分(負傷ＣＮＴ)
017840         MOVE ZERO TO 施記－罨法区分(負傷ＣＮＴ)
017850         MOVE ZERO TO 施記－電療区分(負傷ＣＮＴ)
017860         MOVE ZERO TO 施記－後療料請求区分(負傷ＣＮＴ)
017870     END-PERFORM.
017880*
017890* 詳細項目については、あえて初期化しない。計算では、開始日終了日
017900* 判定をしている為、影響なし。労災・自賠の際、再検討。
017910*
017920*/一部変更
017930*/初期化項目は部位数分だけでなく全て初期化するように変更。
017940*/部位数、開始日、終了日で判定している為、計算には影響ないが
017950*/２号誌は負傷データを読ずに施術記録Ｆをそのままセットする為、
017960*/２号誌のみで不具合が出る。
017970*================================================================*
017980 後療算定処理 SECTION.
017990*
018000     EVALUATE 施記－診療区分
018010     WHEN 1
018020         IF 施記－整復施療区分(部位ＣＮＴ) = ZERO
018030             MOVE 1    TO 施記－後療料請求区分(部位ＣＮＴ)
018040         ELSE
018050             MOVE ZERO TO 施記－後療料請求区分(部位ＣＮＴ)
018060         END-IF
018070     WHEN 2
018080         IF 施記－整復施療区分(部位ＣＮＴ) = ZERO
018090             MOVE 1    TO 施記－後療料請求区分(部位ＣＮＴ)
018100         ELSE
018110             MOVE ZERO TO 施記－後療料請求区分(部位ＣＮＴ)
018120         END-IF
018130     WHEN OTHER
018140         MOVE 1    TO 施記－後療料請求区分(部位ＣＮＴ)
018150     END-EVALUATE.
018160*
018170*================================================================*
018180 再検料初期化処理 SECTION.
018190*
018200*  / 再検日以降で次の初検日まで(当月中)、再検料請求を初期化する。 /
018210*
018220      MOVE 連施－患者番号  TO 施記－患者番号.
018230      MOVE 連施－枝番      TO 施記－枝番.
018240      MOVE 連施－施術和暦  TO 施記－施術和暦.
018250      MOVE 連施－施術年    TO 施記－施術年.
018260      MOVE 連施－施術月    TO 施記－施術月.
018270      MOVE 連施－施術日    TO 施記－施術日.
018280      START 施術記録Ｆ   KEY IS >  施記－患者コード
018290                                   施記－施術和暦年月日
018300      END-START.
018310      IF 状態キー = "00"
018320             MOVE SPACE TO 終了フラグ３
018330             PERFORM 施術記録Ｆ読込
018340             PERFORM UNTIL ( 施記－施術和暦   NOT = 連施－施術和暦  ) OR
018350                           ( 施記－施術年     NOT = 連施－施術年    ) OR
018360                           ( 施記－施術月     NOT = 連施－施術月    ) OR
018370                           ( 施記－患者番号   NOT = 連施－患者番号  ) OR
018380                           ( 施記－枝番       NOT = 連施－枝番      ) OR
018390                           ( 施記－診療区分 = 2  )  OR
018400                           ( 終了フラグ３     NOT = SPACE )
018410*
018420                  IF 施記－再検料請求 NOT = ZERO
018430                      MOVE ZERO  TO 施記－再検料請求
018440*
018450                      REWRITE 施記－レコード
018460                      INVALID KEY
018470                         MOVE NC"施記" TO ファイル名
018480                         PERFORM エラー表示
018490                      END-REWRITE
018500                  END-IF
018510*
018520                  PERFORM 施術記録Ｆ読込
018530*
018540             END-PERFORM
018550      END-IF.
018560*
018570*================================================================*
018580 施術記録Ｆ読込 SECTION.
018590*
018600     READ 施術記録Ｆ NEXT
018610     AT END
018620         MOVE "YES" TO 終了フラグ３
018630     END-READ.
018640*================================================================*
018650 前月判定  SECTION.
018660*------------------------------------------------------------------------------* 
018670*-- 読み込んだ次月の負傷データの年月が、前月かどうか判定 (年月の差が 1 か?) ---*
018680*------------------------------------------------------------------------------* 
018690*
018700      MOVE  SPACE  TO  前月フラグ.
018710      INITIALIZE  計算年月日２Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
018720**
018730*計算月
018740      MOVE 連施－施術和暦    TO 終了和暦２Ｗ.
018750      MOVE 連施－施術年      TO 終了年２Ｗ.
018760      MOVE 連施－施術月      TO 終了月２Ｗ.
018770*前回通院月
018780      MOVE 施記－施術和暦    TO 開始和暦２Ｗ.
018790      MOVE 施記－施術年      TO 開始年２Ｗ.
018800      MOVE 施記－施術月      TO 開始月２Ｗ.
018810*
018820      EVALUATE TRUE
018830       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ = 終了年２Ｗ)
018840            PERFORM  前月比較月
018850       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ NOT = 終了年２Ｗ)
018860            PERFORM  前月比較年
018870       WHEN  開始和暦２Ｗ NOT = 終了和暦２Ｗ 
018880            PERFORM  前月比較元号
018890      END-EVALUATE.
018900*
018910      IF 計算月２Ｗ = 1
018920         MOVE  "YES"  TO  前月フラグ
018930      END-IF.
018940*
018950*================================================================*
018960 前月比較月  SECTION.
018970*
018980     IF  終了月２Ｗ >  開始月２Ｗ
018990         COMPUTE 計算月２Ｗ = 終了月２Ｗ - 開始月２Ｗ
019000     ELSE
019010        MOVE ZERO TO 計算月２Ｗ
019020     END-IF.
019030*
019040*================================================================*
019050 前月比較年  SECTION.
019060*
019070     IF  終了年２Ｗ >  開始年２Ｗ
019080         COMPUTE 計算年２Ｗ = 終了年２Ｗ - 開始年２Ｗ
019090         COMPUTE 計算月２Ｗ = (計算年２Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
019100     ELSE
019110        MOVE ZERO TO 計算月２Ｗ
019120     END-IF.
019130*
019140*================================================================*
019150 前月比較元号  SECTION.
019160*
019170     MOVE 開始和暦２Ｗ TO 元－元号区分.
019180     READ 元号マスタ
019190     NOT INVALID KEY
019200         MOVE 元－開始西暦年 TO 開始西暦年２Ｗ
019210     END-READ.
019220     MOVE 終了和暦２Ｗ TO 元－元号区分.
019230     READ 元号マスタ
019240     NOT INVALID KEY
019250         MOVE 元－開始西暦年 TO 終了西暦年２Ｗ
019260     END-READ.
019270**
019280     IF (開始西暦年２Ｗ NOT = ZERO) AND (終了西暦年２Ｗ NOT = ZERO)
019290        COMPUTE 開始西暦年２Ｗ = 開始西暦年２Ｗ + 開始年２Ｗ - 1
019300        COMPUTE 終了西暦年２Ｗ = 終了西暦年２Ｗ + 終了年２Ｗ - 1
019310*
019320        IF 終了西暦年２Ｗ =  開始西暦年２Ｗ
019330           PERFORM  前月比較月
019340        ELSE
019350           IF  終了西暦年２Ｗ >  開始西暦年２Ｗ
019360               COMPUTE 計算年２Ｗ = 終了西暦年２Ｗ - 開始西暦年２Ｗ
019370               COMPUTE 計算月２Ｗ = (計算年２Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
019380           ELSE
019390               MOVE ZERO TO 計算月２Ｗ
019400           END-IF
019410        END-IF
019420     ELSE
019430        MOVE ZERO TO 計算月２Ｗ
019440     END-IF.
019450*
019460*================================================================*
019470* ＸＸＸ費用額計算 SECTION.
019480**
019490**/RESEPTD～費用額計算までを組込み
019500**/(RESEPTDは費用額計算PGを振り分けるだけなので実質費用額計算のみ)
019510**/初期処理
019520*     PERFORM 累計項目初期化.
019530*     PERFORM 基本料金取得.
019540*     PERFORM 計算情報取得.
019550**/主処理
019560*     MOVE 連計－施術和暦 TO 施記－施術和暦 施術和暦ＣＷ.
019570*     MOVE 連計－施術年   TO 施記－施術年   施術年ＣＷ.
019580*     MOVE 連計－施術月   TO 施記－施術月   施術月ＣＷ.
019590*     MOVE 連計－施術日   TO 施記－施術日   施術日ＣＷ.
019600*     MOVE 連計－患者番号 TO 施記－患者番号 患者コードＣＷ.
019610*     MOVE 連計－枝番     TO 施記－枝番     枝番ＣＷ.
019620**
019630*     START 施術記録Ｆ KEY IS >= 施記－患者コード 施記－施術和暦年月日
019640*     END-START.
019650**
019660*     IF 状態キー = "00"
019670**         MOVE 1     TO 老人負担回数Ｗ
019680*         MOVE SPACE TO 終了フラグ３
019690*         PERFORM 施術記録Ｆ読込
019700**
019710*         PERFORM UNTIL ( 施記－患者コード     NOT = 患者コードＣＷ   ) OR
019720*                       ( 施記－施術和暦年月日 NOT = 施術和暦年月日ＣＷ ) OR
019730*                       ( 終了フラグ３         NOT = SPACE )
019740**
019750*             PERFORM 日計項目初期化
019760**
019770*             PERFORM 初検料算定
019780*             PERFORM 再検料算定
019790**             PERFORM 往療料算定
019800*             PERFORM 後療料算定
019810*             PERFORM その他算定
019820*             PERFORM 累計処理
019830**
019840*             PERFORM 施術記録Ｆ読込
019850*         END-PERFORM
019860*     END-IF.
019870**
019880******************************************************************
019890** 多部位関連項目の部位毎１００％の合計から逓減率、長期逓減率を乗じた
019900** 金額を部位毎に算出する。
019910******************************************************************
019920**
019930*     PERFORM 多部位合計処理.
019940**
019950** 各部位毎の（１００％の金額に）逓減率（多部位・長期）を
019960** 乗けた金額と部位以外項目を加算する。
019970**
019980*     COMPUTE 連計－費用額 = 部位合計長期Ｗ１ + 部位合計長期Ｗ２ +
019990*                            部位合計長期Ｗ３ + 部位合計長期Ｗ３１ +
020000*                            部位合計長期Ｗ４ + 部位合計長期Ｗ４１ +
020010*                            部位合計長期Ｗ４２ + 部位合計長期Ｗ５ + 
020020*                            部位合計長期Ｗ５１ + 部位合計長期Ｗ５２ + 
020030*                            部位合計長期Ｗ５３ + 初検料ＧＷ + 初検加算料ＧＷ +
020040*                            再検料ＧＷ + 往療料ＧＷ + 往療加算料ＧＷ +
020050*                            金属副子料ＧＷ + 初回処置料ＧＷ + 施術情報提供料ＧＷ +
020060*                            初検時相談料ＧＷ.
020070*****************************************************************
020080**     DISPLAY "部位合計長期Ｗ１  " 部位合計長期Ｗ１  
020090**     DISPLAY "部位合計長期Ｗ２  " 部位合計長期Ｗ２  
020100**     DISPLAY "部位合計長期Ｗ３  " 部位合計長期Ｗ３  
020110**     DISPLAY "部位合計長期Ｗ３１" 部位合計長期Ｗ３１
020120**     DISPLAY "部位合計長期Ｗ４  " 部位合計長期Ｗ４  
020130**     DISPLAY "部位合計長期Ｗ４１" 部位合計長期Ｗ４１
020140**     DISPLAY "部位合計長期Ｗ４２" 部位合計長期Ｗ４２
020150**     DISPLAY "部位合計長期Ｗ５  " 部位合計長期Ｗ５  
020160**     DISPLAY "部位合計長期Ｗ５１" 部位合計長期Ｗ５１
020170**     DISPLAY "部位合計長期Ｗ５２" 部位合計長期Ｗ５２
020180**     DISPLAY "部位合計長期Ｗ５３" 部位合計長期Ｗ５３
020190**     DISPLAY "初検料ＧＷ        " 初検料ＧＷ        
020200**     DISPLAY "初検加算料ＧＷ    " 初検加算料ＧＷ    
020210**     DISPLAY "再検料ＧＷ        " 再検料ＧＷ        
020220**     DISPLAY "往療料ＧＷ        " 往療料ＧＷ        
020230**     DISPLAY "往療加算料ＧＷ    " 往療加算料ＧＷ    
020240**     DISPLAY "金属副子料ＧＷ    " 金属副子料ＧＷ    
020250**     DISPLAY "初回処置料ＧＷ    " 初回処置料ＧＷ    
020260**     DISPLAY "施術情報提供料ＧＷ" 施術情報提供料ＧＷ
020270**     DISPLAY "初検時相談料ＧＷ  " 初検時相談料ＧＷ  
020280*****************************************************************
020290*     MOVE 連計－費用額 TO 連料－費用額
020300**
020310**           PERFORM 連結項目セット.
020320**
020330**/平成１２年６月より５部位目の後療料は算定しない
020340**/５部位目の多部位逓減率で判定する
020350*     IF 多部位逓減率ＫＷ(5) = ZERO
020360*         MOVE ZERO TO 連料－部位５２
020370*         INITIALIZE   連料－部位５２
020380*     END-IF.
020390*================================================================*
020400 日計項目初期化 SECTION.
020410*
020420     MOVE ZERO TO 後療料１ＰＷ 後療料２ＰＷ
020430                  後療料３ＰＷ 後療料３１ＰＷ
020440                  後療料４ＰＷ 後療料４１ＰＷ 後療料４２ＰＷ
020450                  後療料５ＰＷ 後療料５１ＰＷ 後療料５２ＰＷ 後療料５３ＰＷ.
020460     MOVE ZERO TO 冷罨法料１ＰＷ 冷罨法料２ＰＷ
020470                  冷罨法料３ＰＷ 冷罨法料３１ＰＷ
020480                  冷罨法料４ＰＷ 冷罨法料４１ＰＷ 冷罨法料４２ＰＷ
020490                  冷罨法料５ＰＷ 冷罨法料５１ＰＷ 冷罨法料５２ＰＷ 冷罨法料５３ＰＷ.
020500     MOVE ZERO TO 温罨法料１ＰＷ 温罨法料２ＰＷ
020510                  温罨法料３ＰＷ 温罨法料３１ＰＷ
020520                  温罨法料４ＰＷ 温罨法料４１ＰＷ 温罨法料４２ＰＷ
020530                  温罨法料５ＰＷ 温罨法料５１ＰＷ 温罨法料５２ＰＷ 温罨法料５３ＰＷ.
020540     MOVE ZERO TO 電療料１ＰＷ 電療料２ＰＷ
020550                  電療料３ＰＷ 電療料３１ＰＷ
020560                  電療料４ＰＷ 電療料４１ＰＷ 電療料４２ＰＷ
020570                  電療料５ＰＷ 電療料５１ＰＷ 電療料５２ＰＷ 電療料５３ＰＷ.
020580*
020590*================================================================*
020600 初検料算定 SECTION.
020610*
020620     MOVE ZERO TO 初検料Ｗ.
020630     MOVE ZERO TO 初検加算料Ｗ.
020640     MOVE ZERO TO 初検時相談料Ｗ.
020650*
020660     IF ( 退避施記－診療区分     = 2 ) AND
020670        ( 初検請求区分Ｗ NOT = 1 ) AND
020680        ( 退避施記－初検料請求区分  = 1 )
020690         COMPUTE 初検料Ｗ = 初検料Ｗ + 初検料ＡＷ
020700* 初検加算
020710         EVALUATE 退避施記－初検加算
020720         WHEN ZERO
020730             CONTINUE
020740         WHEN 1
020750             MOVE 1 TO 初検時間外Ｗ
020760             COMPUTE 初検加算料Ｗ = 初検加算料Ｗ + 時間外ＡＷ
020770         WHEN 2
020780             MOVE 1 TO 初検休日Ｗ
020790             COMPUTE 初検加算料Ｗ = 初検加算料Ｗ + 休日ＡＷ
020800         WHEN 3
020810             MOVE 1 TO 初検深夜Ｗ
020820             COMPUTE 初検加算料Ｗ = 初検加算料Ｗ + 深夜ＡＷ
020830         WHEN OTHER
020840             CONTINUE
020850         END-EVALUATE
020860     END-IF.
020870* 初検時相談料 /080527
020880     IF (退避施記－診療区分 = 2 ) AND (退避施記－初検時相談料区分 NOT = 1)
020890         COMPUTE 初検時相談料Ｗ = 初検時相談料Ｗ + 初検時相談料ＡＷ
020900     END-IF.
020910*================================================================*
020920 再検料算定 SECTION.
020930*
020940     MOVE ZERO TO 再検料Ｗ.
020950*
020960     IF 退避施記－再検料請求  = 1
020970         COMPUTE 再検料Ｗ = 再検料Ｗ + 再検料ＡＷ
020980     END-IF.
020990*
021000*================================================================*
021010 後療料算定 SECTION.
021020****************************************************************
021030* 逓減が関係する：後療料・冷罨法料・温罨法料を含む
021040* 継続以外（治癒・中止・転医等）で算定の繰上り有り（５部位まで）
021050* 治療中の部位の治癒等のケース。
021060* 後療料・罨法料・電療料を含む
021070****************************************************************
021080*
021090* 継続部位数Ｗのセット
021100*     PERFORM 継続部位数セット.
021110     MOVE 連仮計算－部位数 TO 部位数Ｗ 継続部位数Ｗ.
021120*
021130     MOVE ZERO TO 後療料Ｗ 冷罨法料Ｗ 温罨法料Ｗ 電療料Ｗ.
021140*
021150* 継続の部位のみ算定する
021160* 継続の部位のみ部位カウンタをカウントＵＰする
021170     MOVE 1 TO 部位カウンタ
021180*
021190* 部位カウンタは、継続部位数との比較のためと
021200* 逓減率の配列位置取得のための変数
021210     PERFORM VARYING 部位カウンタ２ FROM 1 BY 1
021220             UNTIL ( 部位カウンタ > 継続部位数Ｗ   ) OR
021230                   ( 部位カウンタ > 後療可能算定数ＫＷ ) OR 
021240                   ( 部位カウンタ２ > 部位数Ｗ )
021250*
021260* 負－終了和暦年月が未入力の場合は、ALL 9 をセットする
021270         PERFORM 開始終了日取得
021280*
021290         IF ( 開始和暦年月日Ｗ <= 退避施記－施術和暦年月日 ) AND
021300            ( 終了和暦年月日Ｗ >= 退避施記－施術和暦年月日 )
021310*
021320             MOVE 退避施記－負傷種別(部位カウンタ２)     TO 負傷種別Ｗ
021330             MOVE 退避施記－部位(部位カウンタ２)         TO 部位Ｗ
021340             MOVE 退避施記－左右区分(部位カウンタ２)     TO 左右区分Ｗ
021350             MOVE 退避施記－負傷位置番号(部位カウンタ２) TO 負傷位置番号Ｗ
021360             IF 負傷種別Ｗ = 01 OR 02 OR 03
021370                 PERFORM 捻挫等後療料取得
021380*             ELSE
021390*                 PERFORM 負傷別料金取得
021400             END-IF
021410* 後療単価の退避
021420             IF 負傷種別Ｗ = 07 OR 08
021430                 MOVE 拘縮後療料ＢＷ TO 後療単価ＯＷ(部位カウンタ２)
021440             ELSE
021450                 MOVE 後療料ＢＷ     TO 後療単価ＯＷ(部位カウンタ２)
021460             END-IF
021470* 後療の時のみ算定
021480             IF ( 退避施記－後療料請求区分(部位カウンタ２)   = 1 ) AND 
021490                ( 退避施記－整復施療区分(部位カウンタ２) NOT = 1 )
021500                 IF ( 退避施記－医師拘縮区分(部位カウンタ２) = 1 ) AND
021510                    ( 負傷種別Ｗ = 07 OR 08                  )
021520* レセプトの多部位逓減項目の出力の為、１００％の金額を部位毎合計する。
021530                     MOVE 拘縮後療料ＢＷ TO 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
021540                     PERFORM 後療多部位項目取得
021550                     PERFORM 後療多部位累計
021560                 ELSE
021570                     MOVE 後療料ＢＷ TO 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
021580                     PERFORM 後療多部位項目取得
021590                     PERFORM 後療多部位累計
021600                 END-IF
021610             ELSE
021620                 IF ( 退避施記－後療料請求区分(部位カウンタ２)   = 1 ) AND 
021630                    ( 退避施記－整復施療区分(部位カウンタ２)     = 1 )
021640*/エラー表示の修正
021650*                     PERFORM コンソールエラー確認
021660                     DISPLAY "整復料・後療料は同時に請求できません"
021670                     " 受診者№=" 連計－患者コード " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
021671*-----------------------------------------*
021672                     CALL "actcshm"  WITH C LINKAGE
021673*-----------------------------------------*
021680                  END-IF
021690             END-IF
021700*
021710             EVALUATE 退避施記－罨法区分(部位カウンタ２)
021720             WHEN 1
021730                 MOVE 冷罨法料ＡＷ TO 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
021740                 PERFORM 冷罨法多部位項目取得
021750                 PERFORM 冷罨法多部位累計
021760*
021770             WHEN 2
021780                 MOVE 温罨法料ＡＷ TO 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
021790                 PERFORM 温罨法多部位項目取得
021800                 PERFORM 温罨法多部位累計
021810             WHEN OTHER
021820                 CONTINUE
021830             END-EVALUATE
021840*
021850             IF 退避施記－電療区分(部位カウンタ２) = 1
021860                 MOVE 電療料ＡＷ TO 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
021870                 PERFORM 電療多部位項目取得
021880                 PERFORM 電療多部位累計
021890             END-IF
021900*
021910             COMPUTE 部位カウンタ = 部位カウンタ + 1
021920         END-IF
021930     END-PERFORM.
021940*
021950*================================================================*
021960 多部位合計処理 SECTION.
021970*
021980     COMPUTE 部位合計Ｗ１ = 後療料１ＲＷ + 冷罨法料１ＲＷ +
021990                            温罨法料１ＲＷ + 電療料１ＲＷ.
022000     COMPUTE 部位合計Ｗ２ = 後療料２ＲＷ + 冷罨法料２ＲＷ +
022010                            温罨法料２ＲＷ + 電療料２ＲＷ.
022020     COMPUTE 部位合計Ｗ３ = 後療料３ＲＷ + 冷罨法料３ＲＷ +
022030                            温罨法料３ＲＷ + 電療料３ＲＷ.
022040     COMPUTE 部位合計Ｗ３１ = 後療料３１ＲＷ + 冷罨法料３１ＲＷ +
022050                            温罨法料３１ＲＷ + 電療料３１ＲＷ.
022060     COMPUTE 部位合計Ｗ４ = 後療料４ＲＷ + 冷罨法料４ＲＷ +
022070                            温罨法料４ＲＷ + 電療料４ＲＷ.
022080     COMPUTE 部位合計Ｗ４１ = 後療料４１ＲＷ + 冷罨法料４１ＲＷ +
022090                            温罨法料４１ＲＷ + 電療料４１ＲＷ.
022100     COMPUTE 部位合計Ｗ４２ = 後療料４２ＲＷ + 冷罨法料４２ＲＷ +
022110                            温罨法料４２ＲＷ + 電療料４２ＲＷ.
022120     COMPUTE 部位合計Ｗ５ = 後療料５ＲＷ + 冷罨法料５ＲＷ +
022130                            温罨法料５ＲＷ + 電療料５ＲＷ.
022140     COMPUTE 部位合計Ｗ５１ = 後療料５１ＲＷ + 冷罨法料５１ＲＷ +
022150                            温罨法料５１ＲＷ + 電療料５１ＲＷ.
022160     COMPUTE 部位合計Ｗ５２ = 後療料５２ＲＷ + 冷罨法料５２ＲＷ +
022170                            温罨法料５２ＲＷ + 電療料５２ＲＷ.
022180     COMPUTE 部位合計Ｗ５３ = 後療料５３ＲＷ + 冷罨法料５３ＲＷ +
022190                            温罨法料５３ＲＷ + 電療料５３ＲＷ.
022200*
022210* 多部位逓減算定
022220* ３部位目逓減
022230     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(3) / 100
022240     COMPUTE 部位合計多部位Ｗ３ ROUNDED = 部位合計Ｗ３ * 逓減率中間結果Ｗ
022250*
022260* ４部位目逓減　５０％
022270     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(4) / 100
022280     COMPUTE 部位合計多部位Ｗ４ ROUNDED = 部位合計Ｗ４ * 逓減率中間結果Ｗ
022290*
022300* ４部位目逓減　８０％
022310     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(3) / 100
022320     COMPUTE 部位合計多部位Ｗ４１ ROUNDED = 部位合計Ｗ４１ * 逓減率中間結果Ｗ
022330*
022340* ５部位目逓減　２５％
022350     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(5) / 100
022360     COMPUTE 部位合計多部位Ｗ５ ROUNDED = 部位合計Ｗ５ * 逓減率中間結果Ｗ
022370*
022380* ５部位目逓減　５０％
022390     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(4) / 100
022400     COMPUTE 部位合計多部位Ｗ５１ ROUNDED = 部位合計Ｗ５１ * 逓減率中間結果Ｗ
022410*
022420* ５部位目逓減　８０％
022430     COMPUTE 逓減率中間結果Ｗ   = 多部位逓減率ＫＷ(3) / 100
022440     COMPUTE 部位合計多部位Ｗ５２ ROUNDED = 部位合計Ｗ５２ * 逓減率中間結果Ｗ
022450*
022460* 長期逓減含む部位毎の合計を計算
022470*
022480* １部位　１００％
022490     IF 長期区分Ｗ(1) = 1
022500         COMPUTE 部位合計長期Ｗ１ ROUNDED = 部位合計Ｗ１ * 長期逓減率中間ＫＷ
022510     ELSE IF 長期区分Ｗ(1) = ZERO
022520              MOVE 部位合計Ｗ１ TO 部位合計長期Ｗ１
022530          END-IF
022540     END-IF.
022550*
022560* ２部位　１００％
022570     IF 長期区分Ｗ(2) = 1
022580         COMPUTE 部位合計長期Ｗ２ ROUNDED = 部位合計Ｗ２ * 長期逓減率中間ＫＷ
022590     ELSE IF 長期区分Ｗ(2) = ZERO
022600              MOVE 部位合計Ｗ２ TO 部位合計長期Ｗ２
022610          END-IF
022620     END-IF.
022630*
022640* ３部位　１００％
022650     IF 長期区分Ｗ(3) = 1
022660         COMPUTE 部位合計長期Ｗ３１ ROUNDED = 部位合計Ｗ３１ * 長期逓減率中間ＫＷ
022670     ELSE IF 長期区分Ｗ(3) = ZERO
022680              MOVE 部位合計Ｗ３１ TO 部位合計長期Ｗ３１
022690     END-IF.
022700*
022710* ３部位　８０％
022720     IF 長期区分Ｗ(3) = 1
022730         COMPUTE 部位合計長期Ｗ３ ROUNDED = 部位合計多部位Ｗ３ * 長期逓減率中間ＫＷ
022740     ELSE IF 長期区分Ｗ(3) = ZERO
022750             MOVE 部位合計多部位Ｗ３ TO 部位合計長期Ｗ３
022760          END-IF
022770     END-IF.
022780*
022790* ４部位　１００％
022800     IF 長期区分Ｗ(4) = 1
022810         COMPUTE 部位合計長期Ｗ４２ ROUNDED = 部位合計Ｗ４２ * 長期逓減率中間ＫＷ
022820     ELSE IF 長期区分Ｗ(4) = ZERO
022830              MOVE 部位合計Ｗ４２ TO 部位合計長期Ｗ４２
022840          END-IF
022850     END-IF.
022860*
022870* ４部位　８０％
022880     IF 長期区分Ｗ(4) = 1
022890         COMPUTE 部位合計長期Ｗ４１ ROUNDED = 部位合計多部位Ｗ４１ * 長期逓減率中間ＫＷ
022900     ELSE IF 長期区分Ｗ(4) = ZERO
022910              MOVE 部位合計多部位Ｗ４１ TO 部位合計長期Ｗ４１
022920          END-IF
022930     END-IF
022940*
022950* ４部位　５０％
022960     IF 長期区分Ｗ(4) = 1
022970         COMPUTE 部位合計長期Ｗ４ ROUNDED = 部位合計多部位Ｗ４ * 長期逓減率中間ＫＷ
022980     ELSE IF 長期区分Ｗ(4) = ZERO
022990              MOVE 部位合計多部位Ｗ４ TO 部位合計長期Ｗ４
023000          END-IF
023010     END-IF.
023020*
023030* ５部位　１００％
023040*
023050     IF 長期区分Ｗ(5) = 1
023060         COMPUTE 部位合計長期Ｗ５３ ROUNDED = 部位合計多部位Ｗ５３ * 長期逓減率中間ＫＷ
023070     ELSE IF 長期区分Ｗ(5) = ZERO
023080              MOVE 部位合計Ｗ５３ TO 部位合計長期Ｗ５３
023090          END-IF
023100     END-IF.
023110*
023120* ５部位　８０％
023130     IF 長期区分Ｗ(5) = 1
023140         COMPUTE 部位合計長期Ｗ５２ ROUNDED = 部位合計多部位Ｗ５２ * 長期逓減率中間ＫＷ
023150     ELSE IF 長期区分Ｗ(5) = ZERO
023160              MOVE 部位合計多部位Ｗ５２ TO 部位合計長期Ｗ５２
023170          END-IF
023180     END-IF.
023190*
023200* ５部位　５０％
023210     IF 長期区分Ｗ(5) = 1
023220         COMPUTE 部位合計長期Ｗ５１ ROUNDED = 部位合計多部位Ｗ５１ * 長期逓減率中間ＫＷ
023230     ELSE IF 長期区分Ｗ(5) = ZERO
023240              MOVE 部位合計多部位Ｗ５１ TO 部位合計長期Ｗ５１
023250          END-IF
023260     END-IF.
023270*
023280* ５部位　２５％
023290     IF 長期区分Ｗ(5) = 1
023300         COMPUTE 部位合計長期Ｗ５ ROUNDED = 部位合計多部位Ｗ５ * 長期逓減率中間ＫＷ
023310     ELSE IF 長期区分Ｗ(5) = ZERO
023320              MOVE 部位合計多部位Ｗ５ TO 部位合計長期Ｗ５
023330          END-IF
023340     END-IF.
023350*
023360*================================================================*
023370 累計項目初期化 SECTION.
023380*
023390     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
023400            UNTIL   部位ＣＮＴ > 最大登録部位数Ｗ
023410         MOVE ZERO TO 初回処置料ＯＷ(部位ＣＮＴ)
023420     END-PERFORM.
023430*
023440     MOVE ZERO TO 初検料ＧＷ 初検加算料ＧＷ
023450                  再検料ＧＷ 往療料ＧＷ 往療加算料ＧＷ 往療回数ＧＷ
023460                  後療料ＧＷ 冷罨法料ＧＷ 温罨法料ＧＷ 電療料ＧＷ
023470                  金属副子料ＧＷ
023480                  初回処置料ＧＷ 施術情報提供料ＧＷ
023490                  費用額老人ＧＷ 負担額老人ＧＷ 負担額助成ＧＷ 負担額ＧＷ
023500                  初検時相談料ＧＷ.
023510*
023520     MOVE ZERO TO 後療料１ＲＷ 後療料２ＲＷ
023530                  後療料３ＲＷ 後療料３１ＲＷ
023540                  後療料４ＲＷ 後療料４１ＲＷ 後療料４２ＲＷ
023550                  後療料５ＲＷ 後療料５１ＲＷ 後療料５２ＲＷ 後療料５３ＲＷ.
023560     MOVE ZERO TO 冷罨法料１ＲＷ 冷罨法料２ＲＷ
023570                  冷罨法料３ＲＷ 冷罨法料３１ＲＷ
023580                  冷罨法料４ＲＷ 冷罨法料４１ＲＷ 冷罨法料４２ＲＷ
023590                  冷罨法料５ＲＷ 冷罨法料５１ＲＷ 冷罨法料５２ＲＷ 冷罨法料５３ＲＷ.
023600     MOVE ZERO TO 温罨法料１ＲＷ 温罨法料２ＲＷ
023610                  温罨法料３ＲＷ 温罨法料３１ＲＷ
023620                  温罨法料４ＲＷ 温罨法料４１ＲＷ 温罨法料４２ＲＷ
023630                  温罨法料５ＲＷ 温罨法料５１ＲＷ 温罨法料５２ＲＷ 温罨法料５３ＲＷ.
023640     MOVE ZERO TO 電療料１ＲＷ 電療料２ＲＷ
023650                  電療料３ＲＷ 電療料３１ＲＷ
023660                  電療料４ＲＷ 電療料４１ＲＷ 電療料４２ＲＷ
023670                  電療料５ＲＷ 電療料５１ＲＷ 電療料５２ＲＷ 電療料５３ＲＷ.
023680*
023690     MOVE ZERO TO 後療回数１ＲＷ 後療回数２ＲＷ
023700                  後療回数３ＲＷ 後療回数３１ＲＷ
023710                  後療回数４ＲＷ 後療回数４１ＲＷ 後療回数４２ＲＷ
023720                  後療回数５ＲＷ 後療回数５１ＲＷ 後療回数５２ＲＷ 後療回数５３ＲＷ.
023730     MOVE ZERO TO 冷罨法回数１ＲＷ 冷罨法回数２ＲＷ
023740                  冷罨法回数３ＲＷ 冷罨法回数３１ＲＷ
023750                  冷罨法回数４ＲＷ 冷罨法回数４１ＲＷ 冷罨法回数４２ＲＷ
023760                  冷罨法回数５ＲＷ 冷罨法回数５１ＲＷ 冷罨法回数５２ＲＷ 冷罨法回数５３ＲＷ.
023770     MOVE ZERO TO 温罨法回数１ＲＷ 温罨法回数２ＲＷ
023780                  温罨法回数３ＲＷ 温罨法回数３１ＲＷ
023790                  温罨法回数４ＲＷ 温罨法回数４１ＲＷ 温罨法回数４２ＲＷ
023800                  温罨法回数５ＲＷ 温罨法回数５１ＲＷ 温罨法回数５２ＲＷ 温罨法回数５３ＲＷ.
023810     MOVE ZERO TO 電療回数１ＲＷ 電療回数２ＲＷ
023820                  電療回数３ＲＷ 電療回数３１ＲＷ
023830                  電療回数４ＲＷ 電療回数４１ＲＷ 電療回数４２ＲＷ
023840                  電療回数５ＲＷ 電療回数５１ＲＷ 電療回数５２ＲＷ 電療回数５３ＲＷ.
023850*
023860     MOVE ZERO TO 初検時間外Ｗ 初検休日Ｗ 初検深夜Ｗ 
023870                  往療夜間Ｗ 往療難路Ｗ 往療暴風雨Ｗ 往療時間外Ｗ
023880                  金属副子大Ｗ 金属副子中Ｗ 金属副子小Ｗ.
023890*
023900*================================================================*
023910 基本料金取得 SECTION.
023920*
023930     MOVE 01             TO 料－区分コード.
023940     MOVE ZEROS          TO 料－負傷種別.
023950     MOVE ZEROS          TO 料－部位.
023960     MOVE ZEROS          TO 料－左右区分.
023970     MOVE ZEROS          TO 料－負傷位置番号.
023980     MOVE 連計－施術和暦 TO 料－開始和暦 施術和暦ＣＷ.
023990     MOVE 連計－施術年   TO 料－開始年   施術年ＣＷ.
024000     MOVE 連計－施術月   TO 料－開始月   施術月ＣＷ.
024010*
024020     START 料金マスタ KEY IS <= 料－区分コード
024030                                料－部位コード
024040                                料－開始和暦年月
024050                                REVERSED
024060     END-START.
024070*
024080     IF 状態キー = "00"
024090         READ 料金マスタ NEXT
024100         AT END
024110*/エラー表示の修正
024120*             PERFORM コンソールエラー確認
024130             DISPLAY "施術年月に対応した料金がみつかりません"
024140             " 受診者№=" 連計－患者コード " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
024141*-----------------------------------------*
024142             CALL "actcshm"  WITH C LINKAGE
024143*-----------------------------------------*
024150             PERFORM 終了処理
024160             MOVE ZERO TO PROGRAM-STATUS
024170             EXIT PROGRAM
024180         NOT AT END
024190*
024200             IF ( 施術和暦年月ＣＷ >= 料Ａ－開始和暦年月 ) AND
024210                ( 施術和暦年月ＣＷ <= 料Ａ－終了和暦年月 )
024220                 MOVE 料Ａ－初検料          TO 初検料ＡＷ             
024230                 MOVE 料Ａ－時間外          TO 時間外ＡＷ     
024240                 MOVE 料Ａ－休日            TO 休日ＡＷ       
024250                 MOVE 料Ａ－深夜            TO 深夜ＡＷ       
024260                 MOVE 料Ａ－往療基本料金    TO 往療基本料金ＡＷ       
024270                 MOVE 料Ａ－往療追加料金    TO 往療追加料金ＡＷ       
024280                 MOVE 料Ａ－往療加算率      TO 往療加算率ＡＷ         
024290                 MOVE 料Ａ－再検料          TO 再検料ＡＷ             
024300                 MOVE 料Ａ－冷罨法料        TO 冷罨法料ＡＷ           
024310                 MOVE 料Ａ－温罨法料        TO 温罨法料ＡＷ           
024320                 MOVE 料Ａ－電療料          TO 電療料ＡＷ             
024330                 MOVE 料Ａ－大              TO 大ＡＷ             
024340                 MOVE 料Ａ－中              TO 中ＡＷ             
024350                 MOVE 料Ａ－小              TO 小ＡＷ             
024360                 MOVE 料Ａ－施術情報提供料  TO 施術情報提供料ＡＷ     
024370                 MOVE 料Ａ－初検時相談料    TO 初検時相談料ＡＷ
024380             ELSE
024390*/エラー表示の修正
024400*             PERFORM コンソールエラー確認
024410             DISPLAY "施術年月に対応した料金がみつかりません"
024420                     " 受診者№=" 連計－患者コード " 施術年月=" 連計－施術年 連計－施術月   UPON CONS
024421*-----------------------------------------*
024422             CALL "actcshm"  WITH C LINKAGE
024423*-----------------------------------------*
024430                 PERFORM 終了処理
024440                 MOVE ZERO TO PROGRAM-STATUS
024450                 EXIT PROGRAM
024460             END-IF
024470         END-READ
024480     END-IF.
024490*
024500*================================================================*
024510 計算情報取得 SECTION.
024520*
024530* 制御区分：０１　健康保険
024540     MOVE 01             TO 計－制御区分.
024550     MOVE 連計－施術和暦 TO 計－開始和暦 施術和暦ＣＷ.
024560     MOVE 連計－施術年   TO 計－開始年   施術年ＣＷ.
024570     MOVE 連計－施術月   TO 計－開始月   施術月ＣＷ.
024580*
024590     START 計算マスタ KEY IS <= 計－制御区分
024600                                計－開始和暦年月
024610                                REVERSED
024620     END-START.
024630*
024640     IF 状態キー = "00"
024650         READ 計算マスタ NEXT
024660         AT END
024670*/エラー表示の修正
024680*             PERFORM コンソールエラー確認
024690             DISPLAY "施術年月に対応した料金がみつかりません"
024700             " 受診者№=" 連計－患者コード " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
024701*-----------------------------------------*
024702             CALL "actcshm"  WITH C LINKAGE
024703*-----------------------------------------*
024710             PERFORM 終了処理
024720             MOVE ZERO TO PROGRAM-STATUS
024730             EXIT PROGRAM
024740         NOT AT END
024750             IF ( 施術和暦年月ＣＷ >= 計Ａ－開始和暦年月 ) AND
024760                ( 施術和暦年月ＣＷ <= 計Ａ－終了和暦年月 )
024770                 MOVE 計Ａ－往療基本距離     TO 往療基本距離ＫＷ
024780                 MOVE 計Ａ－後療可能算定数   TO 後療可能算定数ＫＷ
024790                 PERFORM VARYING 多部位ＣＮＴ FROM 1 BY 1
024800                         UNTIL 多部位ＣＮＴ > 後療可能算定数ＫＷ
024810                     MOVE 計Ａ－多部位逓減率(多部位ＣＮＴ) TO 多部位逓減率ＫＷ(多部位ＣＮＴ)
024820                 END-PERFORM
024830                 MOVE 計Ａ－長期逓減率       TO 長期逓減率ＫＷ
024840                 COMPUTE 長期逓減率中間ＫＷ   = 長期逓減率ＫＷ / 100
024850                 MOVE 計Ａ－長期逓減月境界日 TO 長期逓減月境界日ＫＷ
024860                 MOVE 計Ａ－長期逓減月数     TO 長期逓減月数ＫＷ
024870                 MOVE 計Ａ－老人負担回数     TO 老人負担回数ＫＷ
024880             ELSE
024890*/エラー表示の修正
024900*                 PERFORM コンソールエラー確認
024910                 DISPLAY "施術年月に対応した料金がみつかりません"
024920                 " 受診者№=" 連計－患者コード " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
024921*-----------------------------------------*
024922                 CALL "actcshm"  WITH C LINKAGE
024923*-----------------------------------------*
024930                 PERFORM 終了処理
024940                 MOVE ZERO TO PROGRAM-STATUS
024950                 EXIT PROGRAM
024960             END-IF
024970         END-READ
024980     END-IF.
024990*
025000     PERFORM 初検請求区分取得.
025010*
025020*================================================================*
025030 捻挫等後療料取得 SECTION.
025040*
025050     MOVE 02               TO 料－区分コード.
025060     MOVE 負傷種別Ｗ       TO 料－負傷種別.
025070     MOVE ZERO             TO 料－部位.
025080     MOVE ZERO             TO 料－左右区分.
025090     MOVE ZERO             TO 料－負傷位置番号.
025100     MOVE 連計－施術和暦   TO 料－開始和暦 施術和暦ＣＷ.
025110     MOVE 連計－施術年     TO 料－開始年   施術年ＣＷ.
025120     MOVE 連計－施術月     TO 料－開始月   施術月ＣＷ.
025130*
025140     START 料金マスタ KEY IS <= 料－区分コード
025150                                料－部位コード
025160                                料－開始和暦年月
025170                                REVERSED
025180     END-START.
025190*
025200     IF 状態キー = "00"
025210         READ 料金マスタ NEXT
025220         AT END
025230*/エラー表示の修正
025240*             PERFORM コンソールエラー確認
025250             DISPLAY "施術年月に対応した料金がみつかりません"
025260             " 受診者№=" 連計－患者コード " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
025261*-----------------------------------------*
025262             CALL "actcshm"  WITH C LINKAGE
025263*-----------------------------------------*
025270             PERFORM 終了処理
025280             MOVE ZERO TO PROGRAM-STATUS
025290             EXIT PROGRAM
025300         NOT AT END
025310             IF ( 施術和暦年月ＣＷ >= 料Ｂ－開始和暦年月 ) AND
025320                ( 施術和暦年月ＣＷ <= 料Ｂ－終了和暦年月 )
025330                 MOVE 料Ｂ－後療料     TO 後療料ＢＷ    
025340                 MOVE 料Ｂ－整復料     TO 整復料ＢＷ    
025350             ELSE
025360*/エラー表示の修正
025370*                 PERFORM コンソールエラー確認
025380                 DISPLAY "施術年月に対応した料金がみつかりません"
025390                 " 受診者№=" 連計－患者コード " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
025400                 PERFORM 終了処理
025410                 MOVE ZERO TO PROGRAM-STATUS
025420                 EXIT PROGRAM
025430             END-IF
025440         END-READ
025450     END-IF.
025460*
025470*================================================================*
025480 冷罨法多部位項目取得 SECTION.
025490*
025500     EVALUATE 部位カウンタ ALSO 部位カウンタ２
025510     WHEN 1 ALSO 1
025520         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料１ＰＷ
025530*
025540     WHEN 1 ALSO 2
025550     WHEN 2 ALSO 2
025560         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料２ＰＷ
025570*
025580     WHEN 1 ALSO 3
025590     WHEN 2 ALSO 3
025600         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料３１ＰＷ
025610     WHEN 3 ALSO 3
025620         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料３ＰＷ
025630*
025640     WHEN 1 ALSO 4
025650     WHEN 2 ALSO 4
025660         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料４２ＰＷ
025670     WHEN 3 ALSO 4
025680         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料４１ＰＷ
025690     WHEN 4 ALSO 4
025700         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料４ＰＷ
025710*
025720     WHEN 1 ALSO 5
025730     WHEN 2 ALSO 5
025740         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料５３ＰＷ
025750     WHEN 3 ALSO 5
025760         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料５２ＰＷ
025770     WHEN 4 ALSO 5
025780         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料５１ＰＷ
025790     WHEN 5 ALSO 5
025800         MOVE 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 冷罨法料５ＰＷ
025810     WHEN OTHER
025820         CONTINUE
025830     END-EVALUATE.
025840*
025850*================================================================*
025860 温罨法多部位項目取得 SECTION.
025870*
025880     EVALUATE 部位カウンタ ALSO 部位カウンタ２
025890     WHEN 1 ALSO 1
025900         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料１ＰＷ
025910*
025920     WHEN 1 ALSO 2
025930     WHEN 2 ALSO 2
025940         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料２ＰＷ
025950*
025960     WHEN 1 ALSO 3
025970     WHEN 2 ALSO 3
025980         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料３１ＰＷ
025990     WHEN 3 ALSO 3
026000         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料３ＰＷ
026010*
026020     WHEN 1 ALSO 4
026030     WHEN 2 ALSO 4
026040         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料４２ＰＷ
026050     WHEN 3 ALSO 4
026060         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料４１ＰＷ
026070     WHEN 4 ALSO 4
026080         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料４ＰＷ
026090*
026100     WHEN 1 ALSO 5
026110     WHEN 2 ALSO 5
026120         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料５３ＰＷ
026130     WHEN 3 ALSO 5
026140         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料５２ＰＷ
026150     WHEN 4 ALSO 5
026160         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料５１ＰＷ
026170     WHEN 5 ALSO 5
026180         MOVE 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 温罨法料５ＰＷ
026190     WHEN OTHER
026200         CONTINUE
026210     END-EVALUATE.
026220*
026230*================================================================*
026240 電療多部位項目取得 SECTION.
026250*
026260     EVALUATE 部位カウンタ ALSO 部位カウンタ２
026270     WHEN 1 ALSO 1
026280         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料１ＰＷ
026290*
026300     WHEN 1 ALSO 2
026310     WHEN 2 ALSO 2
026320         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料２ＰＷ
026330*
026340     WHEN 1 ALSO 3
026350     WHEN 2 ALSO 3
026360         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料３１ＰＷ
026370     WHEN 3 ALSO 3
026380         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料３ＰＷ
026390*
026400     WHEN 1 ALSO 4
026410     WHEN 2 ALSO 4
026420         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料４２ＰＷ
026430     WHEN 3 ALSO 4
026440         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料４１ＰＷ
026450     WHEN 4 ALSO 4
026460         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料４ＰＷ
026470*
026480     WHEN 1 ALSO 5
026490     WHEN 2 ALSO 5
026500         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料５３ＰＷ
026510     WHEN 3 ALSO 5
026520         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料５２ＰＷ
026530     WHEN 4 ALSO 5
026540         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料５１ＰＷ
026550     WHEN 5 ALSO 5
026560         MOVE 電療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 電療料５ＰＷ
026570     WHEN OTHER
026580         CONTINUE
026590     END-EVALUATE.
026600*
026610*================================================================*
026620 冷罨法多部位累計 SECTION.
026630*
026640     EVALUATE 部位カウンタ ALSO 部位カウンタ２
026650     WHEN 1 ALSO 1
026660         COMPUTE 冷罨法回数１ＲＷ = 冷罨法回数１ＲＷ + 1
026670         COMPUTE 冷罨法料１ＲＷ   = 冷罨法料１ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
026680*
026690     WHEN 1 ALSO 2
026700     WHEN 2 ALSO 2
026710         COMPUTE 冷罨法回数２ＲＷ = 冷罨法回数２ＲＷ + 1
026720         COMPUTE 冷罨法料２ＲＷ   = 冷罨法料２ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
026730*
026740     WHEN 1 ALSO 3
026750     WHEN 2 ALSO 3
026760         COMPUTE 冷罨法回数３１ＲＷ = 冷罨法回数３１ＲＷ + 1
026770         COMPUTE 冷罨法料３１ＲＷ   = 冷罨法料３１ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
026780     WHEN 3 ALSO 3
026790         COMPUTE 冷罨法回数３ＲＷ = 冷罨法回数３ＲＷ + 1
026800         COMPUTE 冷罨法料３ＲＷ = 冷罨法料３ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
026810*
026820     WHEN 1 ALSO 4
026830     WHEN 2 ALSO 4
026840         COMPUTE 冷罨法回数４２ＲＷ = 冷罨法回数４２ＲＷ + 1
026850         COMPUTE 冷罨法料４２ＲＷ   = 冷罨法料４２ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
026860     WHEN 3 ALSO 4
026870         COMPUTE 冷罨法回数４１ＲＷ = 冷罨法回数４１ＲＷ + 1
026880         COMPUTE 冷罨法料４１ＲＷ   = 冷罨法料４１ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
026890     WHEN 4 ALSO 4
026900         COMPUTE 冷罨法回数４ＲＷ = 冷罨法回数４ＲＷ + 1
026910         COMPUTE 冷罨法料４ＲＷ   = 冷罨法料４ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
026920*
026930     WHEN 1 ALSO 5
026940     WHEN 2 ALSO 5
026950         COMPUTE 冷罨法回数５３ＲＷ = 冷罨法回数５３ＲＷ + 1
026960         COMPUTE 冷罨法料５３ＲＷ   = 冷罨法料５３ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
026970     WHEN 3 ALSO 5
026980         COMPUTE 冷罨法回数５２ＲＷ = 冷罨法回数５２ＲＷ + 1
026990         COMPUTE 冷罨法料５２ＲＷ   = 冷罨法料５２ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
027000     WHEN 4 ALSO 5
027010         COMPUTE 冷罨法回数５１ＲＷ = 冷罨法回数５１ＲＷ + 1
027020         COMPUTE 冷罨法料５１ＲＷ   = 冷罨法料５１ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
027030     WHEN 5 ALSO 5
027040         COMPUTE 冷罨法回数５ＲＷ = 冷罨法回数５ＲＷ + 1
027050         COMPUTE 冷罨法料５ＲＷ = 冷罨法料５ＲＷ + 冷罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
027060     WHEN OTHER
027070         CONTINUE
027080     END-EVALUATE.
027090*
027100*================================================================*
027110 温罨法多部位累計 SECTION.
027120*
027130     EVALUATE 部位カウンタ ALSO 部位カウンタ２
027140     WHEN 1 ALSO 1
027150         COMPUTE 温罨法回数１ＲＷ = 温罨法回数１ＲＷ + 1
027160         COMPUTE 温罨法料１ＲＷ   = 温罨法料１ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
027170*
027180     WHEN 1 ALSO 2
027190     WHEN 2 ALSO 2
027200         COMPUTE 温罨法回数２ＲＷ = 温罨法回数２ＲＷ + 1
027210         COMPUTE 温罨法料２ＲＷ   = 温罨法料２ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
027220*
027230     WHEN 1 ALSO 3
027240     WHEN 2 ALSO 3
027250         COMPUTE 温罨法回数３１ＲＷ = 温罨法回数３１ＲＷ + 1
027260         COMPUTE 温罨法料３１ＲＷ   = 温罨法料３１ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
027270     WHEN 3 ALSO 3
027280         COMPUTE 温罨法回数３ＲＷ = 温罨法回数３ＲＷ + 1
027290         COMPUTE 温罨法料３ＲＷ = 温罨法料３ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
027300*
027310     WHEN 1 ALSO 4
027320     WHEN 2 ALSO 4
027330         COMPUTE 温罨法回数４２ＲＷ = 温罨法回数４２ＲＷ + 1
027340         COMPUTE 温罨法料４２ＲＷ   = 温罨法料４２ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
027350     WHEN 3 ALSO 4
027360         COMPUTE 温罨法回数４１ＲＷ = 温罨法回数４１ＲＷ + 1
027370         COMPUTE 温罨法料４１ＲＷ   = 温罨法料４１ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
027380     WHEN 4 ALSO 4
027390         COMPUTE 温罨法回数４ＲＷ = 温罨法回数４ＲＷ + 1
027400         COMPUTE 温罨法料４ＲＷ   = 温罨法料４ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
027410*
027420     WHEN 1 ALSO 5
027430     WHEN 2 ALSO 5
027440         COMPUTE 温罨法回数５３ＲＷ = 温罨法回数５３ＲＷ + 1
027450         COMPUTE 温罨法料５３ＲＷ   = 温罨法料５３ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
027460     WHEN 3 ALSO 5
027470         COMPUTE 温罨法回数５２ＲＷ = 温罨法回数５２ＲＷ + 1
027480         COMPUTE 温罨法料５２ＲＷ   = 温罨法料５２ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
027490     WHEN 4 ALSO 5
027500         COMPUTE 温罨法回数５１ＲＷ = 温罨法回数５１ＲＷ + 1
027510         COMPUTE 温罨法料５１ＲＷ   = 温罨法料５１ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
027520     WHEN 5 ALSO 5
027530         COMPUTE 温罨法回数５ＲＷ = 温罨法回数５ＲＷ + 1
027540         COMPUTE 温罨法料５ＲＷ = 温罨法料５ＲＷ + 温罨法料１００Ｗ(部位カウンタ, 部位カウンタ２)
027550     WHEN OTHER
027560         CONTINUE
027570     END-EVALUATE.
027580*
027590*================================================================*
027600 電療多部位累計 SECTION.
027610*
027620     EVALUATE 部位カウンタ ALSO 部位カウンタ２
027630     WHEN 1 ALSO 1
027640         COMPUTE 電療回数１ＲＷ = 電療回数１ＲＷ + 1
027650         COMPUTE 電療料１ＲＷ   = 電療料１ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027660*
027670     WHEN 1 ALSO 2
027680     WHEN 2 ALSO 2
027690         COMPUTE 電療回数２ＲＷ = 電療回数２ＲＷ + 1
027700         COMPUTE 電療料２ＲＷ   = 電療料２ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027710*
027720     WHEN 1 ALSO 3
027730     WHEN 2 ALSO 3
027740         COMPUTE 電療回数３１ＲＷ = 電療回数３１ＲＷ + 1
027750         COMPUTE 電療料３１ＲＷ   = 電療料３１ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027760     WHEN 3 ALSO 3
027770         COMPUTE 電療回数３ＲＷ = 電療回数３ＲＷ + 1
027780         COMPUTE 電療料３ＲＷ = 電療料３ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027790*
027800     WHEN 1 ALSO 4
027810     WHEN 2 ALSO 4
027820         COMPUTE 電療回数４２ＲＷ = 電療回数４２ＲＷ + 1
027830         COMPUTE 電療料４２ＲＷ   = 電療料４２ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027840     WHEN 3 ALSO 4
027850         COMPUTE 電療回数４１ＲＷ = 電療回数４１ＲＷ + 1
027860         COMPUTE 電療料４１ＲＷ   = 電療料４１ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027870     WHEN 4 ALSO 4
027880         COMPUTE 電療回数４ＲＷ = 電療回数４ＲＷ + 1
027890         COMPUTE 電療料４ＲＷ   = 電療料４ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027900*
027910     WHEN 1 ALSO 5
027920     WHEN 2 ALSO 5
027930         COMPUTE 電療回数５３ＲＷ = 電療回数５３ＲＷ + 1
027940         COMPUTE 電療料５３ＲＷ   = 電療料５３ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027950     WHEN 3 ALSO 5
027960         COMPUTE 電療回数５２ＲＷ = 電療回数５２ＲＷ + 1
027970         COMPUTE 電療料５２ＲＷ   = 電療料５２ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
027980     WHEN 4 ALSO 5
027990         COMPUTE 電療回数５１ＲＷ = 電療回数５１ＲＷ + 1
028000         COMPUTE 電療料５１ＲＷ   = 電療料５１ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
028010     WHEN 5 ALSO 5
028020         COMPUTE 電療回数５ＲＷ = 電療回数５ＲＷ + 1
028030         COMPUTE 電療料５ＲＷ = 電療料５ＲＷ + 電療料１００Ｗ(部位カウンタ, 部位カウンタ２)
028040     WHEN OTHER
028050         CONTINUE
028060     END-EVALUATE.
028070*
028080*================================================================*
028090 累計処理 SECTION.
028100*
028110     COMPUTE 初検料ＧＷ         = 初検料ＧＷ     + 初検料Ｗ.
028120     COMPUTE 初検加算料ＧＷ     = 初検加算料ＧＷ + 初検加算料Ｗ.
028130     COMPUTE 再検料ＧＷ         = 再検料ＧＷ     + 再検料Ｗ.
028140     COMPUTE 往療料ＧＷ         = 往療料ＧＷ     + 往療料Ｗ.
028150     COMPUTE 往療加算料ＧＷ     = 往療加算料ＧＷ + 往療加算料Ｗ.
028160     COMPUTE 初検時相談料ＧＷ   = 初検時相談料ＧＷ + 初検時相談料Ｗ.
028170*
028180     COMPUTE 金属副子料ＧＷ     = 金属副子料ＧＷ + 金属副子料Ｗ.
028190     COMPUTE 初回処置料ＧＷ     = 初回処置料ＧＷ + 初回処置料Ｗ.
028200     COMPUTE 施術情報提供料ＧＷ = 施術情報提供料ＧＷ + 施術情報提供料Ｗ.
028210*
028220*================================================================*
028230 開始終了日取得 SECTION.
028240*
028250     MOVE 連計－施術和暦 TO 負－施術和暦.
028260     MOVE 連計－施術年   TO 負－施術年.
028270     MOVE 連計－施術月   TO 負－施術月.
028280     MOVE 連計－患者番号 TO 負－患者番号.
028290     MOVE 連計－枝番     TO 負－枝番.
028300     READ 負傷データＦ
028310     INVALID KEY
028320*/エラー表示の修正
028330*         PERFORM コンソールエラー確認
028340         DISPLAY "指定の受信者の負傷データがありません"
028350         " 受診者№=" 連計－患者コード " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
028351*-----------------------------------------*
028352         CALL "actcshm"  WITH C LINKAGE
028353*-----------------------------------------*
028360         PERFORM ファイル閉鎖
028370         MOVE ZERO TO PROGRAM-STATUS
028380         EXIT PROGRAM
028390     NOT INVALID KEY
028400*         MOVE 負－転帰区分(部位カウンタ２)       TO 転帰区分Ｗ
028410         MOVE 負－開始和暦年月日(部位カウンタ２) TO 開始和暦年月日Ｗ
028420         IF ( 負－終了和暦(部位カウンタ２) = ZERO ) OR
028430            ( 負－終了年(部位カウンタ２)   = ZERO ) OR
028440            ( 負－終了月(部位カウンタ２)   = ZERO ) OR
028450            ( 負－終了日(部位カウンタ２)   = ZERO )
028460             MOVE "9999999" TO 終了和暦年月日Ｗ
028470         ELSE
028480             MOVE 負－終了和暦年月日(部位カウンタ２) TO 終了和暦年月日Ｗ
028490         END-IF
028500     END-READ.
028510*
028520*================================================================*
028530 後療多部位累計 SECTION.
028540*
028550* １００％の金額を部位毎累計する。
028560* 逓減率は、部位毎の合計金額に対し、乗じる。
028570*
028580     EVALUATE 部位カウンタ ALSO 部位カウンタ２
028590     WHEN 1 ALSO 1
028600         COMPUTE 後療回数１ＲＷ = 後療回数１ＲＷ + 1
028610         COMPUTE 後療料１ＲＷ   = 後療料１ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
028620*
028630     WHEN 1 ALSO 2
028640     WHEN 2 ALSO 2
028650         COMPUTE 後療回数２ＲＷ = 後療回数２ＲＷ + 1
028660         COMPUTE 後療料２ＲＷ   = 後療料２ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
028670*
028680     WHEN 1 ALSO 3
028690     WHEN 2 ALSO 3
028700         IF 後療回数３１ＲＷ = ZERO
028710             MOVE 退避施記－施術和暦 TO 逓減開始和暦３１ＲＷ
028720             MOVE 退避施記－施術年   TO 逓減開始年３１ＲＷ
028730             MOVE 退避施記－施術月   TO 逓減開始月３１ＲＷ
028740             MOVE 退避施記－施術日   TO 逓減開始日３１ＲＷ
028750         END-IF
028760         COMPUTE 後療回数３１ＲＷ = 後療回数３１ＲＷ + 1
028770         COMPUTE 後療料３１ＲＷ   = 後療料３１ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
028780     WHEN 3 ALSO 3
028790         COMPUTE 後療回数３ＲＷ = 後療回数３ＲＷ + 1
028800         COMPUTE 後療料３ＲＷ = 後療料３ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
028810*
028820     WHEN 1 ALSO 4
028830     WHEN 2 ALSO 4
028840         IF 後療回数４２ＲＷ = ZERO
028850             MOVE 退避施記－施術和暦 TO 逓減開始和暦４２ＲＷ
028860             MOVE 退避施記－施術年   TO 逓減開始年４２ＲＷ
028870             MOVE 退避施記－施術月   TO 逓減開始月４２ＲＷ
028880             MOVE 退避施記－施術日   TO 逓減開始日４２ＲＷ
028890         END-IF
028900         COMPUTE 後療回数４２ＲＷ = 後療回数４２ＲＷ + 1
028910         COMPUTE 後療料４２ＲＷ   = 後療料４２ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
028920     WHEN 3 ALSO 4
028930         IF 後療回数４１ＲＷ = ZERO
028940             MOVE 退避施記－施術和暦 TO 逓減開始和暦４１ＲＷ
028950             MOVE 退避施記－施術年   TO 逓減開始年４１ＲＷ
028960             MOVE 退避施記－施術月   TO 逓減開始月４１ＲＷ
028970             MOVE 退避施記－施術日   TO 逓減開始日４１ＲＷ
028980         END-IF
028990         COMPUTE 後療回数４１ＲＷ = 後療回数４１ＲＷ + 1
029000         COMPUTE 後療料４１ＲＷ   = 後療料４１ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029010     WHEN 4 ALSO 4
029020         COMPUTE 後療回数４ＲＷ = 後療回数４ＲＷ + 1
029030         COMPUTE 後療料４ＲＷ   = 後療料４ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029040*
029050     WHEN 1 ALSO 5
029060     WHEN 2 ALSO 5
029070         IF 後療回数５３ＲＷ = ZERO
029080             MOVE 退避施記－施術和暦 TO 逓減開始和暦５３ＲＷ
029090             MOVE 退避施記－施術年   TO 逓減開始年５３ＲＷ
029100             MOVE 退避施記－施術月   TO 逓減開始月５３ＲＷ
029110             MOVE 退避施記－施術日   TO 逓減開始日５３ＲＷ
029120         END-IF
029130         COMPUTE 後療回数５３ＲＷ = 後療回数５３ＲＷ + 1
029140         COMPUTE 後療料５３ＲＷ   = 後療料５３ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029150     WHEN 3 ALSO 5
029160         IF 後療回数５２ＲＷ = ZERO
029170             MOVE 退避施記－施術和暦 TO 逓減開始和暦５２ＲＷ
029180             MOVE 退避施記－施術年   TO 逓減開始年５２ＲＷ
029190             MOVE 退避施記－施術月   TO 逓減開始月５２ＲＷ
029200             MOVE 退避施記－施術日   TO 逓減開始日５２ＲＷ
029210         END-IF
029220         COMPUTE 後療回数５２ＲＷ = 後療回数５２ＲＷ + 1
029230         COMPUTE 後療料５２ＲＷ   = 後療料５２ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029240     WHEN 4 ALSO 5
029250         IF 後療回数５１ＲＷ = ZERO
029260             MOVE 退避施記－施術和暦 TO 逓減開始和暦５１ＲＷ
029270             MOVE 退避施記－施術年   TO 逓減開始年５１ＲＷ
029280             MOVE 退避施記－施術月   TO 逓減開始月５１ＲＷ
029290             MOVE 退避施記－施術日   TO 逓減開始日５１ＲＷ
029300         END-IF
029310         COMPUTE 後療回数５１ＲＷ = 後療回数５１ＲＷ + 1
029320         COMPUTE 後療料５１ＲＷ   = 後療料５１ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029330     WHEN 5 ALSO 5
029340         COMPUTE 後療回数５ＲＷ = 後療回数５ＲＷ + 1
029350         COMPUTE 後療料５ＲＷ = 後療料５ＲＷ + 後療料１００Ｗ(部位カウンタ, 部位カウンタ２)
029360     WHEN OTHER
029370         CONTINUE
029380     END-EVALUATE.
029390*
029400*================================================================*
029410 後療多部位項目取得 SECTION.
029420*
029430* １００％の金額を部位毎累計する。
029440* 逓減率は、部位毎の合計金額に対し、乗じる。
029450*
029460     EVALUATE 部位カウンタ ALSO 部位カウンタ２
029470     WHEN 1 ALSO 1
029480         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料１ＰＷ
029490*
029500     WHEN 1 ALSO 2
029510     WHEN 2 ALSO 2
029520         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料２ＰＷ
029530*
029540     WHEN 1 ALSO 3
029550     WHEN 2 ALSO 3
029560         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料３１ＰＷ
029570     WHEN 3 ALSO 3
029580         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料３ＰＷ
029590*
029600     WHEN 1 ALSO 4
029610     WHEN 2 ALSO 4
029620         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料４２ＰＷ
029630     WHEN 3 ALSO 4
029640         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料４１ＰＷ
029650     WHEN 4 ALSO 4
029660         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料４ＰＷ
029670*
029680     WHEN 1 ALSO 5
029690     WHEN 2 ALSO 5
029700         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料５３ＰＷ
029710     WHEN 3 ALSO 5
029720         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料５２ＰＷ
029730     WHEN 4 ALSO 5
029740         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料５１ＰＷ
029750     WHEN 5 ALSO 5
029760         MOVE 後療料１００Ｗ(部位カウンタ, 部位カウンタ２) TO 後療料５ＰＷ
029770     WHEN OTHER
029780         CONTINUE
029790     END-EVALUATE.
029800*
029810*================================================================*
029820 初検請求区分取得 SECTION.
029830*
029840* 初検料・再検料を請求するかのフラグ取得
029850     MOVE 連計－施術和暦 TO 負－施術和暦. 
029860     MOVE 連計－施術年   TO 負－施術年.   
029870     MOVE 連計－施術月   TO 負－施術月.   
029880     MOVE 連計－患者番号 TO 負－患者番号. 
029890     MOVE 連計－枝番     TO 負－枝番.     
029900     READ 負傷データＦ
029910     INVALID KEY
029920         DISPLAY NC"該当月の情報がありません" UPON CONS
029930         DISPLAY NC"施術和暦" 連計－施術和暦  UPON CONS
029940         DISPLAY NC"施術年"   連計－施術年    UPON CONS 
029950         DISPLAY NC"施術月"   連計－施術月    UPON CONS 
029960         DISPLAY NC"患者番号" 連計－患者番号  UPON CONS
029970         DISPLAY NC"枝番"     連計－枝番      UPON CONS 
029980         MOVE NC"負傷" TO ファイル名
029990         PERFORM エラー表示Ｒ
030000     NOT INVALID KEY
030010         MOVE 負－初検請求区分 TO 初検請求区分Ｗ
030020     END-READ.
030030*
030040*================================================================*
030050 その他算定 SECTION.
030060****************************************************************
030070* 副子・初回処置・情報提供料
030080* 初回処置については、６部位目以降も算定あり。
030090****************************************************************
030100*
030110     MOVE ZERO TO 金属副子料Ｗ.
030120     MOVE ZERO TO 初回処置料Ｗ.
030130     MOVE ZERO TO 施術情報提供料Ｗ.
030140*
030150* 継続の部位のみ算定する
030160* 継続の部位のみ部位カウンタをカウントＵＰする
030170* 初回処置のみ後療可能算定数以上に算定可能
030180     MOVE 1 TO 部位カウンタ
030190     PERFORM VARYING 部位カウンタ２ FROM 1 BY 1
030200             UNTIL ( 部位カウンタ２ > 部位数Ｗ ) OR
030210                   ( 部位カウンタ > 継続部位数Ｗ )
030220*
030230* 負－終了和暦年月が未入力の場合は、ALL 9 をセットする
030240         PERFORM 開始終了日取得
030250*
030260         IF ( 開始和暦年月日Ｗ <= 退避施記－施術和暦年月日 ) AND
030270            ( 終了和暦年月日Ｗ >= 退避施記－施術和暦年月日 )
030280*
030290             MOVE 退避施記－負傷種別(部位カウンタ２)     TO 負傷種別Ｗ
030300             MOVE 退避施記－部位(部位カウンタ２)         TO 部位Ｗ
030310             MOVE 退避施記－左右区分(部位カウンタ２)     TO 左右区分Ｗ
030320             MOVE 退避施記－負傷位置番号(部位カウンタ２) TO 負傷位置番号Ｗ
030330             IF 負傷種別Ｗ = 01 OR 02 OR 03
030340                 PERFORM 捻挫等整復料取得
030350*             ELSE
030360*                 PERFORM 負傷別料金取得
030370             END-IF
030380*
030390* 初回処置料
030400*
030410             IF 退避施記－整復施療区分(部位カウンタ２) = 1
030420                 COMPUTE 初回処置料Ｗ = 初回処置料Ｗ + 整復料ＢＷ
030430                 COMPUTE 初回処置料ＯＷ(部位カウンタ２) = 初回処置料ＯＷ(部位カウンタ２) + 整復料ＢＷ
030440             END-IF
030450*
030460             IF 部位カウンタ <= 後療可能算定数ＫＷ
030470* 金属副子（脱臼・骨折・不全骨折）
030480                 EVALUATE 退避施記－金属副子区分(部位カウンタ２)
030490                 WHEN 1
030500*/仕様変更。金属副子は同じ種類を複数取れる。IF文をコメントに。0307
030510                     IF ( 退避施記－負傷種別(部位カウンタ２) = 04 OR 05 OR 06 ) AND
030520                        ( 連計－施術和暦年月日 = 開始和暦年月日Ｗ   )
030530                         MOVE 1 TO 金属副子大Ｗ
030540                         COMPUTE 金属副子料Ｗ = 金属副子料Ｗ + 大ＡＷ
030550                     END-IF
030560                 WHEN 2
030570*/仕様変更。金属副子は同じ種類を複数取れる。IF文をコメントに。0307
030580*                     IF 金属副子中Ｗ = ZERO
030590                         IF ( 退避施記－負傷種別(部位カウンタ２) = 04 OR 05 OR 06 ) AND
030600                            ( 連計－施術和暦年月日 = 開始和暦年月日Ｗ   )
030610                             MOVE 1 TO 金属副子中Ｗ
030620                             COMPUTE 金属副子料Ｗ = 金属副子料Ｗ + 中ＡＷ
030630                         END-IF
030640*                     END-IF
030650                 WHEN 3
030660*/仕様変更。金属副子は同じ種類を複数取れる。IF文をコメントに。0307
030670*                     IF 金属副子小Ｗ = ZERO
030680                         IF ( 退避施記－負傷種別(部位カウンタ２) = 04 OR 05 OR 06 ) AND
030690                            ( 連計－施術和暦年月日 = 開始和暦年月日Ｗ   )
030700                             MOVE 1 TO 金属副子小Ｗ
030710                             COMPUTE 金属副子料Ｗ = 金属副子料Ｗ + 小ＡＷ
030720                         END-IF
030730*                     END-IF
030740                 WHEN OTHER
030750                     CONTINUE
030760                 END-EVALUATE
030770* 施術情報提供料
030780* 骨折・不全骨折・脱臼の場合のみ
030790                 IF ( 退避施記－情報提供区分(部位カウンタ２) = 1 ) AND
030800                    ( 退避施記－負傷種別(部位カウンタ２) = 04 OR 05 OR 06 ) AND
030810                    ( 開始和暦年月日Ｗ = 退避施記－施術和暦年月日 )
030820                     COMPUTE 施術情報提供料Ｗ = 施術情報提供料Ｗ + 施術情報提供料ＡＷ
030830                 END-IF
030840             END-IF
030850             COMPUTE 部位カウンタ = 部位カウンタ + 1
030860*
030870         END-IF
030880     END-PERFORM.
030890*
030900*================================================================*
030910 捻挫等整復料取得 SECTION.
030920*
030930     MOVE 02               TO 料－区分コード.
030940     MOVE 負傷種別Ｗ       TO 料－負傷種別.
030950     MOVE ZERO             TO 料－部位.
030960     MOVE ZERO             TO 料－左右区分.
030970     MOVE ZERO             TO 料－負傷位置番号.
030980     MOVE 連計－施術和暦   TO 料－開始和暦 施術和暦ＣＷ.
030990     MOVE 連計－施術年     TO 料－開始年   施術年ＣＷ.
031000     MOVE 連計－施術月     TO 料－開始月   施術月ＣＷ.
031010*
031020     START 料金マスタ KEY IS <= 料－区分コード
031030                                料－部位コード
031040                                料－開始和暦年月
031050                                REVERSED
031060     END-START.
031070*
031080     IF 状態キー = "00"
031090         READ 料金マスタ NEXT
031100         AT END
031110*/エラー表示の修正
031120*             PERFORM コンソールエラー確認
031130             DISPLAY "施術年月に対応した料金がみつかりません"
031140             " 受診者№=" 連計－患者コード " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
031141*-----------------------------------------*
031142             CALL "actcshm"  WITH C LINKAGE
031143*-----------------------------------------*
031150             PERFORM 終了処理
031160             MOVE ZERO TO PROGRAM-STATUS
031170             EXIT PROGRAM
031180         NOT AT END
031190             IF ( 施術和暦年月ＣＷ >= 料Ｂ－開始和暦年月 ) AND
031200                ( 施術和暦年月ＣＷ <= 料Ｂ－終了和暦年月 )
031210                 MOVE 料Ｂ－整復料     TO 整復料ＢＷ    
031220             ELSE
031230*/エラー表示の修正
031240*                 PERFORM コンソールエラー確認
031250                 DISPLAY "施術年月に対応した料金がみつかりません"
031260                 " 受診者№=" 連計－患者コード " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
031261*-----------------------------------------*
031262                 CALL "actcshm"  WITH C LINKAGE
031263*-----------------------------------------*
031270                 PERFORM 終了処理
031280                 MOVE ZERO TO PROGRAM-STATUS
031290                 EXIT PROGRAM
031300             END-IF
031310         END-READ
031320     END-IF.
031330*================================================================*
031340 費用額計算 SECTION.
031350*
031360*/RESEPTD～費用額計算までを組込み
031370*/(RESEPTDは費用額計算PGを振り分けるだけなので実質費用額計算のみ)
031380*/初期処理
031390     PERFORM 累計項目初期化.
031400     PERFORM 基本料金取得.
031410     PERFORM 計算情報取得.
031420*/主処理
031430     MOVE 連計－施術和暦 TO 退避施記－施術和暦 施術和暦ＣＷ.
031440     MOVE 連計－施術年   TO 退避施記－施術年   施術年ＣＷ.
031450     MOVE 連計－施術月   TO 退避施記－施術月   施術月ＣＷ.
031460     MOVE 連計－施術日   TO 退避施記－施術日   施術日ＣＷ.
031470     MOVE 連計－患者番号 TO 退避施記－患者番号 患者コードＣＷ.
031480     MOVE 連計－枝番     TO 退避施記－枝番     枝番ＣＷ.
031490*
031500*/退避したワークで判定するため、リードしない。
031510     PERFORM 日計項目初期化.
031520*
031530     PERFORM 初検料算定.
031540     PERFORM 再検料算定.
031550*     PERFORM 往療料算定.
031560     PERFORM 後療料算定.
031570     PERFORM その他算定.
031580     PERFORM 累計処理.
031590*
031600*
031610*****************************************************************
031620* 多部位関連項目の部位毎１００％の合計から逓減率、長期逓減率を乗じた
031630* 金額を部位毎に算出する。
031640*****************************************************************
031650*
031660     PERFORM 多部位合計処理.
031670*
031680* 各部位毎の（１００％の金額に）逓減率（多部位・長期）を
031690* 乗けた金額と部位以外項目を加算する。
031700*
031710     COMPUTE 連計－費用額 = 部位合計長期Ｗ１ + 部位合計長期Ｗ２ +
031720                            部位合計長期Ｗ３ + 部位合計長期Ｗ３１ +
031730                            部位合計長期Ｗ４ + 部位合計長期Ｗ４１ +
031740                            部位合計長期Ｗ４２ + 部位合計長期Ｗ５ + 
031750                            部位合計長期Ｗ５１ + 部位合計長期Ｗ５２ + 
031760                            部位合計長期Ｗ５３ + 初検料ＧＷ + 初検加算料ＧＷ +
031770                            再検料ＧＷ + 往療料ＧＷ + 往療加算料ＧＷ +
031780                            金属副子料ＧＷ + 初回処置料ＧＷ + 施術情報提供料ＧＷ +
031790                            初検時相談料ＧＷ.
031800     MOVE 連計－費用額 TO 連料－費用額
031810*
031820*           PERFORM 連結項目セット.
031830*
031840*/平成１２年６月より５部位目の後療料は算定しない
031850*/５部位目の多部位逓減率で判定する
031860     IF 多部位逓減率ＫＷ(5) = ZERO
031870         MOVE ZERO TO 連料－部位５２
031880         INITIALIZE   連料－部位５２
031890     END-IF.
031900*================================================================*
031910******************************************************************
031920 END PROGRAM KARIKEI.
031930******************************************************************
