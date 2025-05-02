000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YKEISAN.
000060 AUTHOR.                 山田 浩之
000070*
000080*------------------------------------------------------------------*
000090*------------------------------------------------------------------*
000100 DATE-WRITTEN.           2010-03-04
000110 DATE-COMPILED.          2010-03-04
      * 初険日でも初険料を算定しない場合は再検料も算定しない/141209
      */初検日に初険料を算定しない場合は再検料を算定しない↓↓↓/141209
      */初検日以外は初検料請求区分をクリア↓↓↓/141209
      */枝番ありの判定から、自費x・生保h・自賠責a・労災rを除く↓↓↓/160706
      */特別材料算定初期値/160725
      */29年4月より社団静岡は労災の電療料は骨折不全骨折脱臼でも初検日に算定出来る/170408
      */初検日以外は初検料請求区分をクリア/170708
      */30年1月より労災自賠責の電療料は骨折不全骨折脱臼でも初検日に算定出来る↓↓↓/171101
000120*----------------------------------------------------------------*
000130******************************************************************
000140*            ENVIRONMENT         DIVISION                        *
000150******************************************************************
000160 ENVIRONMENT             DIVISION.
000170 CONFIGURATION           SECTION.
000180 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000190 OBJECT-COMPUTER.        FMV-DESKPOWER.
000200 SPECIAL-NAMES.          CONSOLE  IS  CONS
000210                         SYSERR   IS  MSGBOX.
000220 INPUT-OUTPUT            SECTION.
000230 FILE-CONTROL.
000240*
000250     SELECT  参照施術記録Ｆ      ASSIGN      TO        SEKIROKL
000260                             ORGANIZATION             IS  INDEXED
000270                             ACCESS MODE              IS  DYNAMIC
000280                             RECORD KEY           IS 参照施記－施術和暦年月日
000290                                                     参照施記－患者コード
000300                             ALTERNATE RECORD KEY IS 参照施記－患者コード
000310                                                     参照施記－施術和暦年月日
000320                             FILE STATUS              IS  状態キー
000330                             LOCK        MODE         IS  AUTOMATIC.
000340     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000350                             ORGANIZATION             IS  INDEXED
000360                             ACCESS MODE              IS  DYNAMIC
000370                             RECORD KEY               IS  受－施術和暦年月
000380                                                          受－患者コード
000390                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000400                                                          受－患者カナ
000410                                                          受－患者コード
000420                             ALTERNATE RECORD KEY     IS  受－患者コード
000430                                                          受－施術和暦年月
000440                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000450                                                          受－保険種別
000460                                                          受－保険者番号
000470                                                          受－患者コード
000480                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000490                                                          受－公費種別
000500                                                          受－費用負担者番号
000510                                                          受－患者コード
000520                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000530                                                          受－助成種別
000540                                                          受－費用負担者番号助成
000550                                                          受－患者コード
000560                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
000570                                                          受－施術和暦年月
000580                                                          受－患者コード
000590                             FILE STATUS              IS  状態キー
000600                             LOCK        MODE         IS  AUTOMATIC.
000610**
000620*
000630     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
000640                             ORGANIZATION             IS  INDEXED
000650                             ACCESS MODE              IS  DYNAMIC
000660                             RECORD KEY           IS 施記－施術和暦年月日
000670                                                     施記－患者コード
000680                             ALTERNATE RECORD KEY IS 施記－患者コード
000690                                                     施記－施術和暦年月日
000700                             FILE STATUS              IS  状態キー
000710                             LOCK        MODE         IS  AUTOMATIC.
000720     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
000730                             ORGANIZATION             IS  INDEXED
000740                             ACCESS MODE              IS  DYNAMIC
000750                             RECORD KEY               IS 負－施術和暦年月
000760                                                         負－患者コード
000770                             ALTERNATE RECORD KEY     IS 負－患者コード
000780                                                         負－施術和暦年月
000790                             FILE STATUS              IS  状態キー
000800                             LOCK        MODE         IS  AUTOMATIC.
000810     SELECT  計算マスタ      ASSIGN      TO        KEISANL
000820                             ORGANIZATION             IS  INDEXED
000830                             ACCESS MODE              IS  DYNAMIC
000840                             RECORD KEY               IS  計－制御区分
000850                                                          計－開始和暦年月
000860                             FILE STATUS              IS  状態キー
000870                             LOCK        MODE         IS  AUTOMATIC.
000340     SELECT 自賠責計算マスタ ASSIGN      TO        JKEISANL
000350                             ORGANIZATION             IS  INDEXED
000360                             ACCESS MODE              IS  DYNAMIC
000370                             RECORD KEY               IS  自計－制御区分
000380                                                          自計－開始和暦年月
000390                             FILE STATUS              IS  状態キー
000400                             LOCK        MODE         IS  AUTOMATIC.
000880     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000890                             ORGANIZATION             IS  INDEXED
000900                             ACCESS MODE              IS  DYNAMIC
000910                             RECORD KEY               IS  元－元号区分
000920                             FILE STATUS              IS  状態キー
000930                             LOCK        MODE         IS  AUTOMATIC.
000940     SELECT  料金マスタ      ASSIGN      TO        JRYOUKL
000950                             ORGANIZATION             IS  INDEXED
000960                             ACCESS MODE              IS  DYNAMIC
000970                             RECORD KEY               IS  料－区分コード
000980                                                          料－部位コード
000990                                                          料－開始和暦年月
001000                             FILE STATUS              IS  状態キー
001010                             LOCK        MODE         IS  AUTOMATIC.
001020     SELECT  自賠責情報Ｆ    ASSIGN      TO        JIBAIJL
001030                             ORGANIZATION             IS INDEXED
001040                             ACCESS MODE              IS DYNAMIC
001050                             RECORD KEY               IS 自賠－施術和暦年月
001060                                                         自賠－患者コード
001070                             ALTERNATE RECORD KEY     IS 自賠－患者コード
001080                                                         自賠－施術和暦年月
001090                             FILE STATUS              IS 状態キー
001100                             LOCK        MODE         IS AUTOMATIC.
001110*
001111     SELECT  レセプトＦ      ASSIGN      TO        RECEPTL
001112                             ORGANIZATION             IS  INDEXED
001113                             ACCESS MODE              IS  DYNAMIC
001114                             RECORD KEY               IS  レセ－施術和暦年月
001115                                                          レセ－患者コード
001116                                                          レセ－レセ種別
001117                             ALTERNATE RECORD KEY     IS  レセ－患者コード
001118                                                          レセ－施術和暦年月
001119                                                          レセ－レセ種別
001120                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
001121                                                          レセ－施術和暦年月
001122                                                          レセ－患者コード
001123                                                          レセ－レセ種別
001124                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
001125                                                          レセ－レセ種別
001126                                                          レセ－請求保険者番号
001127                                                          レセ－患者コード
001128                                                          レセ－施術和暦年月
001129                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
001130                                                          レセ－請求保険者番号
001131                                                          レセ－患者コード
001132                                                          レセ－レセ種別
001133                                                          レセ－施術和暦年月
001134                             FILE STATUS              IS  状態キー
001135                             LOCK        MODE         IS  AUTOMATIC.
001136*
001230     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
001240                             ORGANIZATION             IS  INDEXED
001250                             ACCESS MODE              IS  DYNAMIC
001260                             RECORD KEY               IS  制－制御区分
001270                             FILE STATUS              IS  状態キー
001280                             LOCK        MODE         IS  AUTOMATIC.
000240*
000250     SELECT  確認施術記録Ｆ      ASSIGN      TO        SEKIROKL
000260                             ORGANIZATION             IS  INDEXED
000270                             ACCESS MODE              IS  DYNAMIC
000280                             RECORD KEY           IS 確認施記－施術和暦年月日
000290                                                     確認施記－患者コード
000300                             ALTERNATE RECORD KEY IS 確認施記－患者コード
000310                                                     確認施記－施術和暦年月日
000320                             FILE STATUS              IS  状態キー
000330                             LOCK        MODE         IS  AUTOMATIC.
000250     SELECT  参照施術記録Ｆ２      ASSIGN      TO        SEKIROKL
000260                             ORGANIZATION             IS  INDEXED
000270                             ACCESS MODE              IS  DYNAMIC
000280                             RECORD KEY           IS 参２施記－施術和暦年月日
000290                                                     参２施記－患者コード
000300                             ALTERNATE RECORD KEY IS 参２施記－患者コード
000310                                                     参２施記－施術和暦年月日
000320                             FILE STATUS              IS  状態キー
000330                             LOCK        MODE         IS  AUTOMATIC.
001137******************************************************************
001138*                      DATA DIVISION                             *
001140******************************************************************
001150 DATA                    DIVISION.
001160 FILE                    SECTION.
001170*
001180*                           ［ＲＬ＝  ２５６］
001190 FD  施術記録Ｆ   BLOCK   CONTAINS   1   RECORDS.
001200     COPY SEKIROK    OF  XFDLIB  JOINING   施記 AS  PREFIX.
001210*                           ［ＲＬ＝  １２８］
001220 FD  負傷データＦ BLOCK   CONTAINS   1   RECORDS.
001230     COPY HUSYOU     OF  XFDLIB  JOINING   負   AS  PREFIX.
001240*                           ［ＲＬ＝  ２５６］
001250 FD  計算マスタ   BLOCK   CONTAINS   1   RECORDS.
001260     COPY KEISAN          OF  XFDLIB  JOINING   計   AS  PREFIX.
001270     COPY KEISANA         OF  XFDLIB  JOINING   計Ａ AS  PREFIX.
001280     COPY KEISANB         OF  XFDLIB  JOINING   計Ｂ AS  PREFIX.
000670*                           ［ＲＬ＝  ２５６］
000680 FD  自賠責計算マスタ BLOCK   CONTAINS   1   RECORDS.
000690     COPY KEISAN          OF  XFDLIB  JOINING   自計   AS  PREFIX.
000700     COPY KEISANA         OF  XFDLIB  JOINING   自計Ａ AS  PREFIX.
000710     COPY KEISANB         OF  XFDLIB  JOINING   自計Ｂ AS  PREFIX.
000720     COPY KEISANC         OF  XFDLIB  JOINING   自計Ｃ AS  PREFIX.
001290*                           ［ＲＬ＝  １２８］
001300 FD  元号マスタ   BLOCK   CONTAINS   1   RECORDS.
001310     COPY GENGOU     OF  XFDLIB  JOINING   元   AS  PREFIX.
001320*
001330*                           ［ＲＬ＝  ２５６］
001340 FD  参照施術記録Ｆ  BLOCK   CONTAINS   1   RECORDS.
001350     COPY SEKIROK    OF  XFDLIB  JOINING   参照施記 AS  PREFIX.
001360*                           ［ＲＬ＝  ３２０］
001370 FD  受診者情報Ｆ    BLOCK   CONTAINS   1   RECORDS.
001380     COPY JUSINJ   OF  XFDLIB  JOINING   受   AS  PREFIX.
001390*                           ［ＲＬ＝  ３２０］
001400 FD  料金マスタ          BLOCK   CONTAINS   1   RECORDS.
001410     COPY RYOUKIN         OF  XFDLIB  JOINING   料   AS  PREFIX.
001420     COPY RYOUKNA         OF  XFDLIB  JOINING   料Ａ AS  PREFIX.
001430     COPY RYOUKNB         OF  XFDLIB  JOINING   料Ｂ AS  PREFIX.
001440     COPY RYOUKNC         OF  XFDLIB  JOINING   料Ｃ AS  PREFIX.
001450     COPY RYOUKND         OF  XFDLIB  JOINING   料Ｄ AS  PREFIX.
001460     COPY RYOUKNE         OF  XFDLIB  JOINING   料Ｅ AS  PREFIX.
001470     COPY RYOUKNF         OF  XFDLIB  JOINING   料Ｆ AS  PREFIX.
001480*
001490 FD  自賠責情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
001500     COPY JIBAIJ     OF  XFDLIB  JOINING   自賠   AS  PREFIX.
001510*
001511 FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
001512     COPY RECEPT          OF  XFDLIB  JOINING   レセ   AS  PREFIX.
001950*                           ［ＲＬ＝  ２５６］
001960 FD  制御情報マスタ IS EXTERNAL         BLOCK   CONTAINS   1   RECORDS.
001970     COPY SEIGYO     OF  XFDLIB  JOINING   制   AS  PREFIX.
001500     COPY SEIGYO01   OF  XFDLIB  JOINING   制０１ AS  PREFIX.
001320*
001340 FD  確認施術記録Ｆ  BLOCK   CONTAINS   1   RECORDS.
001350     COPY SEKIROK    OF  XFDLIB  JOINING   確認施記 AS  PREFIX.
001513*
001330*                           ［ＲＬ＝  ２５６］
001340 FD  参照施術記録Ｆ２  BLOCK   CONTAINS   1   RECORDS.
001350     COPY SEKIROK    OF  XFDLIB  JOINING   参２施記 AS  PREFIX.
001520*---------------------------------------------------------------*
001530******************************************************************
001540*                WORKING-STORAGE SECTION                         *
001550******************************************************************
001560 WORKING-STORAGE         SECTION.
001570 01 キー入力                           PIC X    VALUE SPACE.
001580 01 状態キー                           PIC X(2) VALUE SPACE.
001590 01 終了フラグ                         PIC X(3) VALUE SPACE.
001600 01 終了フラグ２                       PIC X(3) VALUE SPACE.
001610 01 終了フラグ３                       PIC X(3) VALUE SPACE.
001610 01 終了フラグ４                       PIC X(3) VALUE SPACE.
001610 01 終了フラグ５                       PIC X(3) VALUE SPACE.
001610 01 終了フラグ６                       PIC X(3) VALUE SPACE.
001620 01 ファイル名Ｗ                       PIC N(10) VALUE SPACE.
001630 01 保険種別Ｗ                         PIC 9(2) VALUE ZERO.
001640**
001641 01 レセ種別Ｗ                         PIC 9(2)  VALUE ZERO.
001650*
001660*
001670 01 書込フラグ                         PIC X(4) VALUE SPACE.
001680 01 書込フラグ２                       PIC X(4) VALUE SPACE.
001690 01 書込許可フラグ                     PIC X(3) VALUE SPACE.
001700 01 年月フラグ                         PIC X(3) VALUE SPACE.
001710 01 追加フラグ                         PIC X(3) VALUE SPACE.
001720 01 行桁フラグ                         PIC X(3) VALUE SPACE.
001730 01 項目フラグ                         PIC X(3) VALUE SPACE.
001740 01 遅延フラグ                         PIC X(3) VALUE SPACE.
001750 01 選択終了フラグ                     PIC X(3) VALUE SPACE.
001760 01 再検料請求フラグ                   PIC X(3) VALUE SPACE.
001770 01 再検料初期化フラグ                 PIC X(3) VALUE SPACE.
001780 01 行カウンタ                         PIC 9(2) VALUE ZERO.
001790 01 初検カウンタ                       PIC 9(4) VALUE ZERO.
001800 01 遅延カウンタ                       PIC 9(4) VALUE ZERO.
001810 01 遅延回数Ｗ                         PIC 9(4) VALUE ZERO.
001820 01 保険者番号Ｗ                       PIC X(6) VALUE SPACE.
001830 01 エラー内容Ｗ                       PIC N(18) VALUE SPACE.
001840 01 会員名Ｗ                           PIC N(16) VALUE SPACE.
001850 01 画面名称Ｗ                         PIC N(2) VALUE SPACE.
001860 01 負傷ＣＮＴ                         PIC 9(2) VALUE ZERO.
001870 01 請求移動キー                       PIC X(4) VALUE SPACE.
001880 01 画面移動キー                       PIC X(4) VALUE SPACE.
001890 01 処理移動キー                       PIC X(4) VALUE SPACE.
001900 01 再入力キーＷ                       PIC X(4) VALUE SPACE.
001910 01 入力状態Ｗ                         PIC X(4) VALUE SPACE.
001920 01 再検請求区分Ｗ                     PIC 9 VALUE ZERO.
001930 01 日毎部位数Ｗ                       PIC 9 VALUE ZERO.
001940 01 部位名称Ｗ                         PIC N(12) VALUE SPACE.
001950 01 行Ｗ                               PIC 9(4) VALUE ZERO.
001960 01 桁Ｗ                               PIC 9(4) VALUE ZERO.
001970 01 最大登録数Ｗ                       PIC 9 VALUE ZERO.
001980 01 部位長Ｗ                           PIC 9(2) VALUE 1.
001990 01 商Ｗ                               PIC 9(4) VALUE ZERO.
002000 01 剰余Ｗ                             PIC 9(4) VALUE ZERO.
002010 01 負傷名Ｗ                           PIC N(6) VALUE SPACE.
002020 01 前詰フラグ                         PIC X(3) VALUE SPACE.
002030 01 部位ＣＮＴ                         PIC 9(2) VALUE ZERO.
002040 01 部位カウンタ                       PIC 9(2) VALUE ZERO.
002050 01 部位カウンタ２                     PIC 9(2) VALUE ZERO.
002060 01 部位カウンタＷ                     PIC 9(2) VALUE ZERO.
002070 01 部位カウンタＷ２                   PIC 9(2) VALUE ZERO.
002080 01 相談カウンタ                       PIC 9(2) VALUE ZERO.
002090 01 実日数Ｗ                           PIC 9(2) VALUE ZERO.
002100 01 部位数Ｗ                           PIC 9 VALUE ZERO.
002110*
002120 01 施術和暦年月ＮＷ.
002130   03 施術和暦ＮＷ                     PIC 9.
002140   03 施術年ＮＷ                       PIC 9(2).
002150   03 施術月ＮＷ                       PIC 9(2).
002160*
002170 01 施術和暦年月日ＣＷ.
002180   03 施術和暦年月ＣＷ.
002190     05 施術和暦ＣＷ                   PIC 9.
002200     05 施術年月ＣＷ.
002210       07 施術年ＣＷ                   PIC 9(2).
002220       07 施術月ＣＷ                   PIC 9(2).
002230   03 施術日ＣＷ                       PIC 9(2).
002240*
002250*
002260 01 開始和暦年月日ＣＷ.
002270   03 開始和暦年月ＣＷ.
002280     05 開始和暦ＣＷ                   PIC 9.
002290     05 開始年月ＣＷ.
002300       07 開始年ＣＷ                   PIC 9(2).
002310       07 開始月ＣＷ                   PIC 9(2).
002320   03 開始日ＣＷ                       PIC 9(2).
002330*
002340 01 終了和暦年月日ＣＷ.
002350   03 終了和暦年月ＣＷ.
002360     05 終了和暦ＣＷ                   PIC 9.
002370     05 終了年月ＣＷ.
002380       07 終了年ＣＷ                   PIC 9(2).
002390       07 終了月ＣＷ                   PIC 9(2).
002400   03 終了日ＣＷ                       PIC 9(2).
002410*
002420 01 現在時Ｗ                           PIC 9(2).
002430 01 現在分Ｗ                           PIC 9(2).
002440 01 現在秒Ｗ                           PIC 9(2).
002450 01 現在年Ｗ                           PIC 9(2).
002460 01 現在月Ｗ                           PIC 9(2).
002470 01 現在日Ｗ                           PIC 9(2).
002480*
002490 01 計算和暦年月Ｗ.
002500     03 計算和暦Ｗ                     PIC 9.
002510     03 計算年月Ｗ.
002520        05 計算年Ｗ                    PIC 9(2).
002530        05 計算月Ｗ                    PIC 9(2).
002540     03 計算日Ｗ                       PIC 9(2).
002550*
002560 01 絶対値Ｗ                           PIC 9(6).
002570 01 絶対値月Ｗ                         PIC 9(3).
002580 01 絶対値月Ｗ２                       PIC 9(3).
002590 01 負傷経過日Ｗ                       PIC 9(4).
002600 01 西暦年Ｗ                           PIC 9(4).
002610 01 西暦年差Ｗ                         PIC 9(2).
002620 01 西暦年差２Ｗ                       PIC 9(2).
002630 01 閏年回数Ｗ                         PIC 9(2).
002640 01 通常年回数Ｗ                       PIC 9(2).
002650 01 通常年絶対値Ｗ                     PIC 9(6).
002660 01 閏年絶対値Ｗ                       PIC 9(6).
002670 01 年絶対値Ｗ                         PIC 9(6).
002680 01 負傷絶対値Ｗ                       PIC 9(6).
002690 01 施術絶対値Ｗ                       PIC 9(6).
002700 01 加算年Ｗ                           PIC 9(4).
002710*
002720 01 冷罨日数骨折Ｗ                     PIC 9(2).
002730 01 冷罨日数脱臼Ｗ                     PIC 9(2).
002740 01 冷罨日数打撲Ｗ                     PIC 9(2).
002750 01 温罨待機骨折Ｗ                     PIC 9(2).
002760 01 温罨待機脱臼Ｗ                     PIC 9(2).
002770*
002780*
002790 01 患者コードＣＷ.
002800    03 患者番号ＣＷ                    PIC 9(6).
002810    03 枝番ＣＷ                        PIC X.
002820*
002830 01 開始西暦年Ｗ                       PIC 9(4) VALUE ZERO.
002840 01 選択日Ｗ                           PIC 9(2) VALUE ZERO.
002850*
002860 01 初検加算Ｗ                         PIC 9 VALUE ZERO.
002870 01 往療加算Ｗ                         PIC 9 VALUE ZERO.
002880*
002890 01 往療距離Ｗ.
002900    03 往療距離Ｗ１                    PIC 9(3).
002910    03 距離少数Ｗ                      PIC 9.
002920*
002930 01 名称区分１Ｗ.
002940    03 FILLER                          PIC 9(2).
002950    03 名称区分１Ｗ１                  PIC 9.
002960*
002970 01 元号名称Ｗ.
002980    03 元号名称Ｗ１                    PIC N(2).
002990    03 FILLER                          PIC N(2).
003000*
003010 01 部位コード退避ＴＷ .
003020   03 部位コードＴＷ OCCURS 9.
003030     05 負傷種別ＴＷ                    PIC 9(2).
003040     05 部位ＴＷ                        PIC 9(2).
003050     05 左右区分ＴＷ                    PIC 9.
003060     05 負傷位置番号ＴＷ                PIC 9(2).
003070*
003080 01 保険種別略称Ｗ.
003090   03 保険種別略称Ｗ１                 PIC N(4).
003100   03 FILLER                           PIC N(2).
003110*
003120 01 負傷種別略称Ｗ.
003130    03 負傷種別略称Ｗ１                PIC N(4).
003140    03 FILLER                          PIC N(2).
003150 01 名称区分略称２Ｗ.
003160    03 名称区分略称２Ｗ１              PIC N(2).
003170    03 FILLER                          PIC N(4).
003180 01 名称区分略称３Ｗ.
003190    03 名称区分略称３Ｗ１              PIC N(3).
003200    03 FILLER                          PIC N(3).
003210*
003220 01 負傷番号Ｗ                         PIC 9.
003230 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
003240    03 負傷番号Ｗ１                    PIC X.
003250*
003260 01 全角負傷番号Ｗ                     PIC N.
003270 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
003280    03 全角負傷番号Ｗ１                PIC X(2).
003290*
003300 01 日カウンタ                         PIC 9(2).
003310 01 日カウンタＲ REDEFINES 日カウンタ.
003320    03 日カウンタＷ                    PIC X(2).
003330*
003340* 01 患者番号Ｗ                         PIC 9(6).
003350* 01 枝番Ｗ                             PIC X.
003360 01 患者氏名Ｗ                         PIC N(10).
003370 01 施術和暦Ｗ                         PIC 9.
003380 01 施術年Ｗ                           PIC 9(2).
003390 01 施術月Ｗ                           PIC 9(2).
003400 01 施術日Ｗ                           PIC 9(2).
003410 01 受付時Ｗ                           PIC 9(2).
003420 01 受付分Ｗ                           PIC 9(2).
003430 01 受付秒Ｗ                           PIC 9(2).
003440*
003450 01 詳細項目ＴＷ.
003460    03  運動療法区分ＴＷ               PIC 9.
003470    03  宿泊区分ＴＷ                   PIC 9.
003480    03  食事区分ＴＷ                   PIC 9.
003490    03  施術証明区分ＴＷ               PIC 9.
003500    03  施術明細区分ＴＷ               PIC 9.
003510*
003520    03  部位記録 OCCURS 9.
003530        05  医師拘縮区分ＴＷ           PIC 9.
003540        05  金属副子区分ＴＷ           PIC 9.
003550        05  情報提供区分ＴＷ           PIC 9.
003560        05  指導管理区分ＴＷ           PIC 9.
003570        05  特別材料区分ＴＷ           PIC 9.
003580        05  包帯交換区分ＴＷ           PIC 9.
003590*
003600 01 一括入力Ｗ                         PIC X(50).
003610 01 一括入力Ｗ２.
003620    03 部位位置Ｗ                      PIC 9.
003630    03 整復施療区分Ｗ                  PIC 9.
003640    03 罨法区分Ｗ                      PIC 9.
003650    03 電療区分Ｗ                      PIC 9.
003660    03 経過コードＷ                    PIC 9(2).
003670    03 転帰区分Ｗ                      PIC 9.
003680    03 FILLER                          PIC X(43).
003690*
003700*
003710 01  保険者番号Ｗ２.
003720     03  保番１Ｗ                      PIC X(2).
003730     03  保番２Ｗ                      PIC X(8).
003740* 費用負担者番号（市町村番号）ＷＯＲＫ
003750 01 費用負担者番号Ｗ.
003760     03  法別番号Ｗ                      PIC X(2).
003770     03  助保険Ｗ.
003780          05  府県Ｗ                     PIC X(2).
003790          05  助番Ｗ                     PIC X     OCCURS 3.
003800          05  助ＣＤＷ                   PIC X.
003810     03  FILLER                        PIC X(2).
003820*
003830** 前月判定用
003840 01 前月フラグ                         PIC X(3)  VALUE SPACE.
003850 01 計算年月日２Ｗ.
003860    03 計算和暦２Ｗ                    PIC 9(1)   VALUE ZERO.
003870    03 計算年２Ｗ                      PIC S9(2)  VALUE ZERO.
003880    03 計算月２Ｗ                      PIC S9(2)  VALUE ZERO.
003890    03 計算日２Ｗ                      PIC S9(2)  VALUE ZERO.
003900 01 開始年月日２Ｗ.
003910    03 開始和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003920    03 開始年２Ｗ                      PIC 9(2)  VALUE ZERO.
003930    03 開始月２Ｗ                      PIC 9(2)  VALUE ZERO.
003940    03 開始日２Ｗ                      PIC 9(2)  VALUE ZERO.
003950    03 開始西暦年２Ｗ                  PIC S9(4) VALUE ZERO.
003960 01 終了年月日２Ｗ.
003970    03 終了和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003980    03 終了年２Ｗ                      PIC 9(2)  VALUE ZERO.
003990    03 終了月２Ｗ                      PIC 9(2)  VALUE ZERO.
004000    03 終了日２Ｗ                      PIC 9(2)  VALUE ZERO.
004010    03 終了西暦年２Ｗ                  PIC S9(4) VALUE ZERO.
004020*/枝番判定用
004030 01 患者コードＷ.
004040    03 患者番号Ｗ                      PIC 9(6) VALUE ZERO.
004050    03 枝番Ｗ                          PIC X    VALUE SPACE.
004060 01 患者カウンタ                       PIC 9(2) VALUE ZERO.
004070 01 枝番チェックフラグ                 PIC X(3) VALUE SPACE.
004080*/エラー表示の修正
004090 01 エラー表示Ｆ                     PIC 9(1)  VALUE ZERO.
004100 01 公費種別Ｗ                       PIC 9(2) VALUE ZERO.
004110 01 助成種別Ｗ                       PIC 9(2) VALUE ZERO.
004120*
004130 01 再検料回数Ｗ                       PIC 9(2)  VALUE ZERO.
004140*/運動療法料
004150 01 運動料回数Ｗ                       PIC 9(2)  VALUE ZERO.
004160 01 前回運動日数Ｗ                     PIC 9(2)  VALUE ZERO.
004170 01 運動療法フラグ                     PIC X(3)  VALUE SPACE.
004180*/指導管理料
004190 01 指導管理回数Ｗ                     PIC 9(2)  VALUE ZERO.
004200 01 前回指導日数Ｗ                     PIC 9(2)  VALUE ZERO.
004210 01 指導管理料フラグ                   PIC X(3)  VALUE SPACE.
004220 01 連施絶対値Ｗ                       PIC 9(6)  VALUE ZERO.
004230 01 施記絶対値Ｗ                       PIC 9(6)  VALUE ZERO.
004240 01 前回施術日数Ｗ                     PIC 9(2)  VALUE ZERO.
004250 01 初検絶対値Ｗ                       PIC 9(6)  VALUE ZERO.
004260 01 初検経過日数Ｗ                     PIC 9(4)  VALUE ZERO.
004270*/包帯交換料
004280* 01 包帯交換フラグ                     PIC X(3)  VALUE SPACE.
004290  01 包帯交換フラグ.
004300     03 部位包帯交換フラグ             PIC X(3)  VALUE SPACE OCCURS 9.
004310 01 対象日Ｗ                           PIC 9(2)  VALUE ZERO.
004320 01 対象回数Ｗ                         PIC 9(2)  VALUE ZERO.
004330 01 施術回数Ｗ                         PIC 9(2)  VALUE ZERO.
004340 01 前月和暦年月日Ｗ.
004350     03 前月和暦年月Ｗ.
004360        05 前月和暦Ｗ                  PIC 9    VALUE ZERO.
004370        05 前月年月Ｗ.
004380           07 前月年Ｗ                 PIC 9(2) VALUE ZERO.
004390           07 前月月Ｗ                 PIC 9(2) VALUE ZERO.
004400     03 前月日Ｗ                       PIC 9(2) VALUE ZERO.
004410 01 施術西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
004420 01 前月西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
004430* 01 商Ｗ                               PIC 9(3)  VALUE ZERO.
004440 01 余Ｗ                               PIC 9(3)  VALUE ZERO.
004450*/日整静岡の算定間隔を６日に変更。0304
004460 01 協会コードＷ                       PIC 9(2) VALUE ZERO.
004470 01 運動指導算定間隔Ｗ                 PIC 9(2) VALUE ZERO.
004480 01 月末日Ｗ                           PIC 9(2) VALUE ZERO.
004490*/指導運動、算定間隔と月の上限回数を任意に設定する/0505
004500* 運動療法を算定出来る間隔
004510 01 運動算定間隔Ｗ                     PIC 9(2) VALUE ZERO.
004520* 運動療法を算定出来るひと月の上限
004530 01 運動上限回数Ｗ                     PIC 9(1) VALUE ZERO.
004540* 指導管理を算定出来る間隔
004550 01 指導算定間隔Ｗ                     PIC 9(2) VALUE ZERO.
004560* 指導管理を算定出来るひと月の上限
004570 01 指導上限回数Ｗ                     PIC 9(1) VALUE ZERO.
004580* 月基準区分Ｗ：ゼロ＝前月の応答日の翌日から当日まで、１＝暦通り
004590 01 月基準区分Ｗ                       PIC 9(1) VALUE ZERO.
004600*
004610*****
004620* 労災・自賠責用
004630 01 施術証明区分Ｗ                     PIC 9 VALUE ZERO.
004640 01 施術明細区分Ｗ                     PIC 9 VALUE ZERO.
004650*
004660**
004670**
004680* 助成月途中開始日
004690 01 助成月途中開始日Ｗ                 PIC 9(2) VALUE ZERO.
004700*
004710*
      */特別材料算定初期値/160725
       01 特別材料区分Ｗ                     PIC 9    VALUE ZERO.
      */令和4年10月1日より明細書発行加算料を新設/20220704
       01 明細発行加算区分Ｗ                 PIC 9(1) VALUE ZERO.
      *初期値の追加(冷温電)/20230615
       01 冷罨法区分Ｗ                       PIC 9(1) VALUE ZERO.
       01 温罨法区分Ｗ                       PIC 9(1) VALUE ZERO.
       01 電療料区分Ｗ                       PIC 9(1) VALUE ZERO.
      */明細発行加算の変更和暦年月/20240724
000420 01 明細変更和暦年月Ｗ.
000430    03 明細変更和暦Ｗ                  PIC 9    VALUE ZERO.
000440    03 明細変更年月Ｗ.
000450        05 明細変更年Ｗ                PIC 9(2) VALUE ZERO.
000460        05 明細変更月Ｗ                PIC 9(2) VALUE ZERO.
004720******
004730* 日付ＷＯＲＫ
004740 01 計算機西暦                         PIC 9(8).
004750 01 計算機西暦Ｒ REDEFINES 計算機西暦.
004760    03 計算機世紀                      PIC 9(2).
004770    03 計算機日付                      PIC 9(6).
004780    03 計算機日付Ｒ REDEFINES 計算機日付.
004790       05 計算機年月                   PIC 9(4).
004800       05 計算機年月Ｒ REDEFINES 計算機年月.
004810         07 計算機年                   PIC 9(2).
004820         07 計算機月                   PIC 9(2).
004830       05 計算機日                     PIC 9(2).
004840*
004850 01 計算機時間.
004860    03 計算機時                        PIC 9(2).
004870    03 計算機分                        PIC 9(2).
004880    03 計算機秒                        PIC 9(2).
004890    03 FILLER                          PIC 9(2).
004900******************************************************************
004910*                          連結項目                              *
004920******************************************************************
004930*
004940* 施術記録レコード構築ＰＧ（ＳＥＫＩＲＥＣ）連携キー
004950 01 連施－キー IS EXTERNAL.
004960   03 連施－施記構築区分               PIC 9.
004970   03 連施－施術和暦年月日.
004980     05 連施－施術和暦                 PIC 9.
004990     05 連施－施術年月.
005000       07 連施－施術年                 PIC 9(2).
005010       07 連施－施術月                 PIC 9(2).
005020     05 連施－施術日                   PIC 9(2).
005030   03 連施－患者コード.
005040     05 連施－患者番号                 PIC 9(6).
005050     05 連施－枝番                     PIC X.
005060   03 連施－初検情報 OCCURS 5.
005070     05 連施－初検和暦年月日.
005080       07 連施－初検和暦               PIC 9.
005090       07 連施－初検年月.
005100         09 連施－初検年               PIC 9(2).
005110         09 連施－初検月               PIC 9(2).
005120       07 連施－初検日                 PIC 9(2).
005130*
005140*
005150* 01 連ＳＥＫＩＲＯＫ－キー IS EXTERNAL.
005160*   03 連ＳＥＫＩＲＯＫ－オープン区分   PIC 9.
005170*
005180* 受診者Ｆの枝番チェック
005190 01 連受診枝番ＣＨＫ－キー IS EXTERNAL.
005200    03 連受診枝番ＣＨＫ－施術和暦年月.
005210       05 連受診枝番ＣＨＫ－施術和暦          PIC 9.
005220       05 連受診枝番ＣＨＫ－施術年月.
005230          07 連受診枝番ＣＨＫ－施術年         PIC 9(2).
005240          07 連受診枝番ＣＨＫ－施術月         PIC 9(2).
005250    03 連受診枝番ＣＨＫ－患者コード.
005260       05 連受診枝番ＣＨＫ－患者番号          PIC 9(6).
005270       05 連受診枝番ＣＨＫ－枝番              PIC X.
005280*   OUT（0:なし、1:あり）
005290    03 連受診枝番ＣＨＫ－枝番あり             PIC 9.
005300    03 連受診枝番ＣＨＫ－後期高齢枝番あり     PIC 9.
005310*
005320 01 連計－キー IS EXTERNAL.
005330    03  連計－負担金計算区分           PIC 9.
005340    03  連計－本人家族区分             PIC 9.
005350    03  連計－保険種別                 PIC 9(2).
005360    03  連計－保険者番号               PIC X(10).
005370    03  連計－公費種別                 PIC 9(2).
005380    03  連計－市町村番号老人           PIC X(10).
005390    03  連計－助成種別                 PIC 9(2).
005400    03  連計－市町村番号助成           PIC X(10).
005410    03  連計－患者コード.
005420      05  連計－患者番号               PIC 9(6).
005430      05  連計－枝番                   PIC X.
005440    03  連計－施術和暦年月日.
005450      05  連計－施術和暦年月.
005460        07  連計－施術和暦             PIC 9.
005470        07  連計－施術年月.
005480          09  連計－施術年             PIC 9(2).
005490          09  連計－施術月             PIC 9(2).
005500      05  連計－施術日                 PIC 9(2).
005510    03  連計－費用額                   PIC 9(6).
005520    03  連計－負担額                   PIC 9(6).
005530    03  連計－請求額                   PIC 9(6).
005540    03  連計－費用額老人               PIC 9(6).
005550    03  連計－負担額老人               PIC 9(6).
005560    03  連計－請求額老人               PIC 9(6).
005570    03  連計－費用額助成               PIC 9(6).
005580    03  連計－負担額助成               PIC 9(5).
005590    03  連計－請求額助成               PIC 9(5).
005600    03  連計－実日数                   PIC 9(2).
005610    03  連計－負担率                   PIC 9(3).
005620    03  連計－負担率区分               PIC 9.
005630*
005640* 確認メッセージ用 (２行)
005650 01 連メ７－キー IS EXTERNAL.
005660    03  連メ７－メッセージ１             PIC X(40).
005670    03  連メ７－メッセージ２             PIC X(40).
005680*
005690 01 連レ－キー IS EXTERNAL.
005700    03 連レ－保険種別                  PIC 9(2).
005710
005720*/新計算連結
005730 01 連Ｙ計算－キー IS EXTERNAL.
005740   03 連Ｙ計算－施術和暦年月.
005750     05 連Ｙ計算－施術和暦           PIC 9.
005760     05 連Ｙ計算－施術年月.
005770       07 連Ｙ計算－施術年           PIC 9(2).
005780       07 連Ｙ計算－施術月           PIC 9(2).
005790   03 連Ｙ計算－患者コード.
005800     05 連Ｙ計算－患者番号           PIC 9(6).
005810     05 連Ｙ計算－枝番               PIC X.
005820*
005830*---------------------------------------------------------*
005840* 助成月途中用
005850 01 連計算助成月途中－キー IS EXTERNAL.
005860   03 連計算助成月途中－日計区分           PIC 9.
005870   03 連計算助成月途中－助成月途中開始日   PIC 9(2).
005880   03 連計算助成月途中－助成のみ処理区分   PIC 9.
005890*---------------------------------------------------------*
005900*
      */労災自賠責再計算からコールされた場合(コンバート時)は労災自賠責レコードセットを行わない/121206(労災自賠責のレコードセットは旧柔と仕様が違う部分がある為)
       01 連労自コ－キー IS EXTERNAL.
          03 連労自コ－コンバートＦ        PIC 9.
014320*
014330****************************
014340* 元号日付チェック用ワーク *
014350*****************************
014360 01 連元ＣＨＫ－キー IS EXTERNAL.
014370*/ in
014380    03 連元ＣＨＫ－和暦年月日.
014390       05 連元ＣＨＫ－和暦年月.
014400          07 連元ＣＨＫ－和暦           PIC 9.
014410          07 連元ＣＨＫ－年             PIC 9(2).
014420          07 連元ＣＨＫ－月             PIC 9(2).
014430       05 連元ＣＨＫ－日                PIC 9(2).
014440*   / 確認メッセージを使用する場合："YES" /
014450    03 連元ＣＨＫ－確認フラグ           PIC X(3).
014460*/ out
014470*   / 年月単位の年月日を作成 /
014480    03 連元ＣＨＫ－正式和暦年月日.
014490       05 連元ＣＨＫ－正式和暦年月.
014500          07 連元ＣＨＫ－正式和暦       PIC 9.
014510          07 連元ＣＨＫ－正式年         PIC 9(2).
014520          07 連元ＣＨＫ－正式月         PIC 9(2).
014530       05 連元ＣＨＫ－正式日            PIC 9(2).
014540*   / 存在しない場合エラー："YES" /
014550    03 連元ＣＨＫ－エラーフラグ         PIC X(3).
014560*
014570*   / 正式日付の年月日を作成（元号が変更されていない場合は上記年月日と同じ） /
014580    03 連元ＣＨＫ－正式和暦年月日２.
014590       05 連元ＣＨＫ－正式和暦年月２.
014600          07 連元ＣＨＫ－正式和暦２     PIC 9.
014610          07 連元ＣＨＫ－正式年２       PIC 9(2).
014620          07 連元ＣＨＫ－正式月２       PIC 9(2).
014630       05 連元ＣＨＫ－正式日２          PIC 9(2).
014640*   / 存在しない場合エラー："YES" /
014650    03 連元ＣＨＫ－エラーフラグ２       PIC X(3).
005910******************************************************************
005920*                      PROCEDURE  DIVISION                       *
005930******************************************************************
005940 PROCEDURE               DIVISION.
HILO  *     DISPLAY "Yk:St--- " 連Ｙ計算－キー " ----------"
005950************
005960*           *
005970* 初期処理   *
005980*           *
005990************
006000***
006010     IF 連Ｙ計算－施術和暦年月 < 41410
006020        MOVE  "　平成14年10月より前のデータは、" TO 連メ７－メッセージ１
006030        MOVE  "　計算できません。"               TO 連メ７－メッセージ２
006040        CALL   "MSG007"
006050        CANCEL "MSG007"
006060        EXIT PROGRAM
006070     END-IF.
006080***
006090*
006100     PERFORM 初期化.
006101*
006102*--------------------------------------------*
006103*  / 上記後 レセプトＦが無ければ終了 /
006104     PERFORM キー用レセ種別取得.
006107*
006116     MOVE 連Ｙ計算－施術和暦      TO レセ－施術和暦.
006117     MOVE 連Ｙ計算－施術年        TO レセ－施術年.
006118     MOVE 連Ｙ計算－施術月        TO レセ－施術月.
006119     MOVE 連Ｙ計算－患者番号      TO レセ－患者番号.
006120     MOVE 連Ｙ計算－枝番          TO レセ－枝番.
006121*     / キー用レセ種別で取得 /
006122     MOVE レセ種別Ｗ              TO レセ－レセ種別.
006123     READ レセプトＦ
006124     INVALID KEY
006125        PERFORM 終了処理
006126        EXIT PROGRAM
006135     END-READ.
006136*--------------------------------------------*
005490     PERFORM 制御情報取得.
006142*
006145************
006146*           *
006147* 主処理     *
006148*           *
006150************
006160     PERFORM 準備処理.
006170     PERFORM 呼び出し処理.
006180     IF 保険種別Ｗ = 70 OR 80
006190        PERFORM 労災自賠責用フラグ更新
006200     END-IF.
006210     PERFORM 月計処理.
006220************
006230*           *
006240* 終了処理   *
006250*           *
006260************
006270     PERFORM 終了処理.
006280     MOVE ZERO TO PROGRAM-STATUS.
006290     EXIT PROGRAM.
006300*
006310*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
006320*================================================================*
006330 初期化 SECTION.
006340*
006350     PERFORM ファイルオープン.
006360*
006370* 保険種別取得
006380*
006390*    / 助成月途中用クリアー /
006400     MOVE ZERO TO 助成月途中開始日Ｗ.
006410     INITIALIZE 連計算助成月途中－キー.
006420*
006430     MOVE 連Ｙ計算－患者番号   TO 受－患者番号.
006440     MOVE 連Ｙ計算－枝番       TO 受－枝番.
006450     MOVE 連Ｙ計算－施術和暦   TO 受－施術和暦.
006460     MOVE 連Ｙ計算－施術年     TO 受－施術年.
006470     MOVE 連Ｙ計算－施術月     TO 受－施術月.
006480     READ 受診者情報Ｆ
006490     NOT INVALID KEY
006500         MOVE 受－保険種別 TO 保険種別Ｗ
006510*
006520*------------------------------------------------------*
006530* 助成の月途中開始 
006540        IF ( 受－保険分類 = 1 ) AND ( 受－助成種別 NOT = ZERO ) AND ( 受－費用負担者番号助成 NOT = SPACE ) AND
006550           ( 受－助成月途中開始日 NOT = ZERO ) AND ( 受－助成月途中開始日 NOT = SPACE )
006560           MOVE 受－助成月途中開始日 TO 助成月途中開始日Ｗ
006570        END-IF
006580*------------------------------------------------------*
006590*
006600     END-READ.
006610*
006620*================================================================*
006630 ファイルオープン SECTION.
006640*
006650     OPEN INPUT 参照施術記録Ｆ.
006660         MOVE NC"参照施記" TO ファイル名Ｗ.
006670         PERFORM オープンチェック.
006680     OPEN INPUT 受診者情報Ｆ.
006690         MOVE NC"受診" TO ファイル名Ｗ.
006700         PERFORM オープンチェック.
006701     OPEN INPUT レセプトＦ.
006702         MOVE NC"レセＦ" TO ファイル名Ｗ.
006703         PERFORM オープンチェック.
006710*
006720     OPEN I-O 施術記録Ｆ.
006730         MOVE NC"施記" TO ファイル名Ｗ.
006740         PERFORM オープンチェック.
006750     OPEN INPUT 負傷データＦ.
006760         MOVE NC"負傷" TO ファイル名Ｗ.
006770         PERFORM オープンチェック.
006780     OPEN INPUT 計算マスタ.
006790         MOVE NC"計算" TO ファイル名Ｗ.
006800         PERFORM オープンチェック.
006780     OPEN INPUT 自賠責計算マスタ.
006790         MOVE NC"計算" TO ファイル名Ｗ.
006800         PERFORM オープンチェック.
006810     OPEN INPUT 元号マスタ.
006820         MOVE NC"元号" TO ファイル名Ｗ.
006830         PERFORM オープンチェック.
006840     OPEN INPUT 料金マスタ.
006850         MOVE NC"料金" TO ファイル名Ｗ.
006860         PERFORM オープンチェック.
006870     OPEN INPUT 自賠責情報Ｆ.
006880         MOVE NC"自賠" TO ファイル名Ｗ.
006890         PERFORM オープンチェック.
006870     OPEN INPUT 制御情報マスタ.
006880         MOVE NC"制御" TO ファイル名Ｗ.
006890         PERFORM オープンチェック.
006650     OPEN INPUT 確認施術記録Ｆ.
006660         MOVE NC"確認施記" TO ファイル名Ｗ.
006670         PERFORM オープンチェック.
006650     OPEN INPUT 参照施術記録Ｆ２.
006660         MOVE NC"参２施記" TO ファイル名Ｗ.
006670         PERFORM オープンチェック.
006900*
006910*================================================================*
006920 オープンチェック SECTION.
006930*
006940     IF 状態キー  NOT =  "00"
006950         DISPLAY ファイル名Ｗ NC"Ｆオープンエラー" UPON CONS
006960         DISPLAY NC"状態キー：" 状態キー           UPON CONS
006970         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
006980                                                   UPON CONS
006990         ACCEPT  キー入力 FROM CONS
007000         PERFORM ファイル閉鎖
007010         MOVE 99 TO PROGRAM-STATUS
007020         EXIT PROGRAM.
007030*
007040*================================================================*
007050 ファイル閉鎖 SECTION.
007060*
007070     CLOSE 参照施術記録Ｆ 施術記録Ｆ 受診者情報Ｆ 負傷データＦ レセプトＦ
007080           計算マスタ 自賠責計算マスタ 元号マスタ 料金マスタ 自賠責情報Ｆ 制御情報マスタ 確認施術記録Ｆ 参照施術記録Ｆ２.
007090*================================================================*
007100 終了処理 SECTION.
007110*
007120     PERFORM ファイル閉鎖.
007130*================================================================*
007131*================================================================*
007132 キー用レセ種別取得 SECTION.
007133*
007134* / レセプトＦのキー：レセ種別を決定する /
007135*
007136*   / 1:一般,2:老人,3:助成,4:労災,5:自賠責,6:自費,7:生保単独,8保険証忘れ /
007137*
007138     MOVE ZERO TO レセ種別Ｗ.
007139*
007140     IF 受－保険種別 = 91
007141        MOVE  8   TO レセ種別Ｗ
007142     ELSE
007154           IF 受－公費種別 NOT = ZERO
007155              MOVE  2   TO レセ種別Ｗ
007156           ELSE
007157              EVALUATE 受－保険種別
007158              WHEN 70
007159                  MOVE  4   TO レセ種別Ｗ
007160              WHEN 80
007161                  MOVE  5   TO レセ種別Ｗ
007162              WHEN 90
007163                  MOVE  6   TO レセ種別Ｗ
007164              WHEN 85
007165                  MOVE  7   TO レセ種別Ｗ
007166              WHEN 91
007167                  MOVE  8   TO レセ種別Ｗ
007168              WHEN OTHER
007169                  MOVE  1   TO レセ種別Ｗ
007170              END-EVALUATE
007171           END-IF
007173     END-IF.
007174*
007175*================================================================*
007176*================================================================*
007177*================================================================*
007178 準備処理 SECTION.
007179*
007180     MOVE SPACE TO 連施－キー.
007181     INITIALIZE    連施－キー.
007190*
007200     MOVE ZERO                 TO 連施－施記構築区分.    
007210     MOVE 連Ｙ計算－施術和暦   TO 連施－施術和暦.
007220     MOVE 連Ｙ計算－施術年     TO 連施－施術年.  
007230     MOVE 連Ｙ計算－施術月     TO 連施－施術月.  
007240     MOVE 連Ｙ計算－患者番号   TO 連施－患者番号.
007250     MOVE 連Ｙ計算－枝番       TO 連施－枝番.    
007260*
007270     CALL   "SYOKEN".
007280     CANCEL "SYOKEN".
007290*
007300*================================================================*
007310 呼び出し処理 SECTION.
007320*
007330     MOVE ZERO TO 相談カウンタ.
007340*
007350     MOVE 連Ｙ計算－施術和暦  TO  参照施記－施術和暦.
007360     MOVE 連Ｙ計算－施術年    TO  参照施記－施術年.
007370     MOVE 連Ｙ計算－施術月    TO  参照施記－施術月.
007380     MOVE 1                   TO  参照施記－施術日.
007390     MOVE 連Ｙ計算－患者番号  TO  参照施記－患者番号.
007400     MOVE 連Ｙ計算－枝番      TO  参照施記－枝番.
007410*
007420     START 参照施術記録Ｆ KEY IS >= 参照施記－患者コード 参照施記－施術和暦年月日
007430     END-START.
007440*
007450     IF 状態キー = "00"
007460               MOVE SPACE TO 終了フラグ３
007470               PERFORM 参照施術記録Ｆ読込
007480               PERFORM UNTIL ( 参照施記－施術和暦   NOT = 連Ｙ計算－施術和暦   ) OR
007490                             ( 参照施記－施術年     NOT = 連Ｙ計算－施術年     ) OR
007500                             ( 参照施記－施術月     NOT = 連Ｙ計算－施術月     ) OR
007510                             ( 参照施記－患者コード NOT = 連Ｙ計算－患者コード ) OR
007520                             ( 終了フラグ３         NOT = SPACE )
007530*
007540*          / 特殊設定の時、CALL しない /
007550                   IF 参照施記－特殊設定区分 NOT = 1
007560*                     施術記録レコード算定項目再構築
007570                          PERFORM 連結項目セット施記構築２
007580****
007590                          EVALUATE 保険種別Ｗ
007600*                     / 労災 /
007610                          WHEN  70
                                    IF 連労自コ－コンバートＦ NOT = 1
007620                                  PERFORM 労災レコードセット
                                    END-IF
007630*                     / 自賠責 /
007640                          WHEN  80
                                    IF 連労自コ－コンバートＦ NOT = 1
007650                                  PERFORM 自賠責レコードセット
                                    END-IF
007660*                     / 一般 /
007670                          WHEN  OTHER
007680                              PERFORM 一般レコードセット
007690                          END-EVALUATE
007700                   END-IF
007710****
007720                   PERFORM 参照施術記録Ｆ読込
007730               END-PERFORM
007740     END-IF.
007750*
007760*================================================================*
007770 参照施術記録Ｆ読込 SECTION.
007780*
007790     READ 参照施術記録Ｆ NEXT
007800     AT END
007810         MOVE "YES"  TO 終了フラグ３
007820     END-READ.
007830*
007840*================================================================*
007850 連結項目セット施記構築２ SECTION.
007860*
007870     MOVE ZERO                 TO 連施－施記構築区分.    
007880     MOVE 参照施記－施術和暦   TO 連施－施術和暦.
007890     MOVE 参照施記－施術年     TO 連施－施術年.  
007900     MOVE 参照施記－施術月     TO 連施－施術月.  
007910     MOVE 参照施記－施術日     TO 連施－施術日.  
007920     MOVE 参照施記－患者番号   TO 連施－患者番号.
007930     MOVE 参照施記－枝番       TO 連施－枝番.    
007940*
007950* オープン区分:1 (オープンなし）
007960*     MOVE 1 TO 連ＳＥＫＩＲＯＫ－オープン区分.
007970*
007980*================================================================*
007990 労災レコードセット SECTION.
008000**------------------------------------------------------------------------*
008010* 負－初検請求区分 
008020*   1:初検・再検・初回処置料なし
008030*   2:初検・再検なし
008040*   3:初回処置料なし
008050*   4:初検加算(時間外)
008060*   5:初検加算(休日)
008070*   6:初検加算(深夜)
008080**--- 本PGは、初検と再検の無しをセットするので、区分 1,2 が対象----*
008090**------------------------------------------------------------------------*
008100*
008110     MOVE 連施－施術和暦 TO 負－施術和暦 施術和暦ＣＷ.
008120     MOVE 連施－施術年   TO 負－施術年   施術年ＣＷ.
008130     MOVE 連施－施術月   TO 負－施術月   施術月ＣＷ.
008140     MOVE 連施－施術日   TO              施術日ＣＷ.
008150     MOVE 連施－患者番号 TO 負－患者番号.
008160     MOVE 連施－枝番     TO 負－枝番.
008170*
008180     READ 負傷データＦ
008190     INVALID KEY
008200         MOVE NC"負傷" TO ファイル名Ｗ
008210         PERFORM エラー表示Ｒ
008220     NOT INVALID KEY
008230*
008240* 診療区分＝後療or初検のセットと再検料請求区分のセットを行う
008250         IF 負－初検請求区分 = 1 OR 2
008260             MOVE SPACE TO 再検料請求フラグ
008270         ELSE
008280             PERFORM 労自再検料請求セット
008290         END-IF
008300*
008310* 運動療法料の判定 手動で設定
008320         IF 負－運動療法算定区分 = 1
008330             PERFORM 運動療法料判定
008340         ELSE
008350             MOVE SPACE TO 運動療法フラグ
008360         END-IF
008370* 指導管理料の判定
008380         IF 負－指導管理算定区分 = ZERO OR SPACE
008390             PERFORM 指導管理料判定
008400         ELSE
008410             MOVE SPACE TO 指導管理料フラグ
008420         END-IF
008430* 包帯交換料の判定
008440         IF 負－包帯交換算定区分 = ZERO OR SPACE
008450             PERFORM 包帯交換料判定
008460         ELSE
008470             MOVE SPACE TO 包帯交換フラグ
008480         END-IF
008490*
008500* 書換（ＲＥＷＲＩＴＥ）に対応できるようＲＥＡＤしておく（重要）
008510         MOVE 連施－施術和暦 TO 施記－施術和暦
008520         MOVE 連施－施術年   TO 施記－施術年
008530         MOVE 連施－施術月   TO 施記－施術月
008540         MOVE 連施－施術日   TO 施記－施術日
008550         MOVE 連施－患者番号 TO 施記－患者番号
008560         MOVE 連施－枝番     TO 施記－枝番
008570         READ 施術記録Ｆ
008580         INVALID KEY
008590             MOVE SPACE TO 施記－レコード
008600             INITIALIZE    施記－レコード
008610             MOVE 連施－施術和暦 TO 施記－施術和暦
008620             MOVE 連施－施術年   TO 施記－施術年
008630             MOVE 連施－施術月   TO 施記－施術月
008640             MOVE 連施－施術日   TO 施記－施術日
008650             MOVE 連施－患者番号 TO 施記－患者番号
008660             MOVE 連施－枝番     TO 施記－枝番
008670         NOT INVALID KEY
008680*
008690* 既存レコードの部位算定情報を更新
008700             PERFORM 修正算定情報初期化
008710*
008720         END-READ
008730*
008740* 初検料請求
008750         MOVE SPACE TO 終了フラグ
008760         MOVE 1    TO  施記－診療区分
008770         PERFORM VARYING 初検カウンタ FROM 1 BY 1
008780                 UNTIL ( 初検カウンタ     > 5      ) OR
008790                       ( 終了フラグ   NOT = SPACE  )
008800             IF ( 連施－初検和暦(初検カウンタ) = ZERO ) OR
008810                ( 連施－初検年(初検カウンタ)   = ZERO ) OR
008820                ( 連施－初検月(初検カウンタ)   = ZERO ) OR
008830                ( 連施－初検日(初検カウンタ)   = ZERO )
008840                 MOVE "YES" TO 終了フラグ
008850             ELSE
008860*
008870                 IF 連施－初検和暦年月日(初検カウンタ) = 連施－施術和暦年月日
008880                     MOVE 2    TO  施記－診療区分
008890                     IF 負－初検請求区分 = 1 OR 2
008900                         MOVE ZERO TO 施記－初検料請求区分
008910                     ELSE
008920                         MOVE 1    TO 施記－初検料請求区分
008930                     END-IF
008940*----------------------------------------------------------------------------------*
008950*                    / 2008/09 改定　初検時相談料区分 0：加算あり　1：加算なし
008960                     IF 負－初検時相談料区分 = 1
008970                         MOVE 1    TO 施記－初検時相談料区分
008980                     ELSE
008990                         MOVE ZERO TO 施記－初検時相談料区分
009000                     END-IF
009010*----------------------------------------------------------------------------------*
009020                     MOVE "YES" TO 終了フラグ
                       ELSE
      */初検日以外は初検料請求区分をクリア/141209
011150                     MOVE ZERO TO 施記－初検料請求区分
009030                 END-IF
009040             END-IF
009050         END-PERFORM
009060*
009070* 再検料請求
009080         IF ( 再検料請求フラグ NOT = SPACE ) AND
009090            ( 施記－診療区分   NOT = 2     )
009100             MOVE 1    TO 施記－再検料請求
009110         ELSE
009120             MOVE ZERO TO 施記－再検料請求
009130         END-IF
009140*/運動更新区分が１なら更新しない/111107
009150         IF 負－運動更新区分 = 1
009160             CONTINUE
009170         ELSE
009180* 運動療法区分
009190             IF ( 運動療法フラグ NOT = SPACE ) AND
009200*                ( 施記－診療区分 NOT = 2     )
009210                ( 施記－初検料請求区分 NOT = 1 )
009220                 MOVE 1    TO 施記－運動療法区分
009230             ELSE
009240                 MOVE ZERO TO 施記－運動療法区分
009250             END-IF
009260         END-IF
009270*/指導更新区分が１なら更新しない/111107
009280         IF 負－指導更新区分 = 1
009290             CONTINUE
009300         ELSE
009310* 指導管理区分
009320             IF ( 指導管理料フラグ NOT = SPACE ) AND
009330                ( 施記－初検料請求区分 NOT = 1 )
009340                 MOVE 1    TO 施記－指導管理区分２
009350             ELSE
009360                 MOVE ZERO TO 施記－指導管理区分２
009370             END-IF
009380         END-IF
009390*
009400         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
009410                 UNTIL 部位ＣＮＴ > 負－部位数
009420*
009430* 無病の場合、以下の算定は行わない。
009440             IF 負－負傷種別(部位ＣＮＴ) NOT = 09
009450                 IF ( 負－終了和暦(部位ＣＮＴ) = ZERO ) OR
009460                    ( 負－終了年(部位ＣＮＴ)   = ZERO ) OR
009470                    ( 負－終了月(部位ＣＮＴ)   = ZERO ) OR
009480                    ( 負－終了日(部位ＣＮＴ)   = ZERO )
009490*
009500                     MOVE "9999999" TO 終了和暦年月日ＣＷ
009510                 ELSE
009520*
009530                     MOVE 負－終了和暦年月日(部位ＣＮＴ) TO 終了和暦年月日ＣＷ
009540                 END-IF
009550*
009560* 部位コード・転帰区分は、施術日に関係なく転記する。
009570                 MOVE 負－部位コード(部位ＣＮＴ) TO 施記－部位コード(部位ＣＮＴ)
009580                 MOVE 負－転帰区分(部位ＣＮＴ)   TO 施記－転帰区分(部位ＣＮＴ)
009590*/開始日変更に対応する為クリアする/111104
009600                 MOVE ZERO TO 施記－整復施療区分(部位ＣＮＴ)
009610                              施記－医師拘縮区分(部位ＣＮＴ)
009620                              施記－特別材料区分(部位ＣＮＴ)
009630*/包帯更新区分が１なら更新しない/111107
009640                 IF 負－包帯更新区分(部位ＣＮＴ) = 1
009650                     CONTINUE
009660                 ELSE
009670                     MOVE ZERO TO 施記－包帯交換区分(部位ＣＮＴ)
009680                 END-IF
009690*
009700                 IF ( 負－開始和暦年月日(部位ＣＮＴ) <= 施術和暦年月日ＣＷ ) AND
009710                    ( 終了和暦年月日ＣＷ             >= 施術和暦年月日ＣＷ )
009720*
009730                     PERFORM 労災算定情報取得
009740                     IF ( 負－開始和暦年月日(部位ＣＮＴ) = 施術和暦年月日ＣＷ ) AND
009750                        ( 負－算定区分(部位ＣＮＴ)   NOT = 1  ) AND
009760                        ( 負－負傷種別(部位ＣＮＴ)   NOT = 07 ) AND
009770                        ( 負－負傷種別(部位ＣＮＴ)   NOT = 08 )
009780*
009790                         MOVE 1 TO 施記－整復施療区分(部位ＣＮＴ)
009800*
009810                     END-IF
009820                     PERFORM 労自罨法算定処理
009830                     PERFORM 後療算定処理
009840*                    ／＊ 医師拘縮　＊／
009850                     IF 負－負傷種別(部位ＣＮＴ) = 07 OR 08
009860                         MOVE 1 TO 施記－医師拘縮区分(部位ＣＮＴ)
009870                     END-IF
009880*                    ／＊特別材料区分 ＊／
009890*                     IF (施記－診療区分 = 2)
009900                     IF (負－開始和暦年月日(部位ＣＮＴ) = 施記－施術和暦年月日) AND
      */特別材料算定初期値/160725
                              (特別材料区分Ｗ                 = ZERO)
009910                         MOVE 1    TO 施記－特別材料区分(部位ＣＮＴ)
009920                     ELSE
009930                         MOVE ZERO TO 施記－特別材料区分(部位ＣＮＴ)
009940                     END-IF
009950                 END-IF
009960*/包帯更新区分が１なら更新しない/111107
009970                 IF 負－包帯更新区分(部位ＣＮＴ) = 1
009980                     CONTINUE
009990                 ELSE
010000*                    ／＊包帯交換料 ＊／
010010*                     IF (包帯交換フラグ     = "YES") AND
010020                     IF (部位包帯交換フラグ(部位ＣＮＴ)  = "YES"              ) AND
010030                        (施記－診療区分              NOT = 2                  ) AND
010040                        (負－開始和暦年月日(部位ＣＮＴ) <= 施術和暦年月日ＣＷ ) AND
010050                        ( 終了和暦年月日ＣＷ            >= 施術和暦年月日ＣＷ )
010060                         MOVE 1    TO 施記－包帯交換区分(部位ＣＮＴ)
010070                     ELSE
010080                         MOVE ZERO TO 施記－包帯交換区分(部位ＣＮＴ)
010090                     END-IF
010100                 END-IF
010110             ELSE
010120* 無病時、部位コードのみ転記する/090213
010130                 MOVE 負－部位コード(部位ＣＮＴ) TO 施記－部位コード(部位ＣＮＴ)
010140             END-IF
010150         END-PERFORM
010160*
010170         PERFORM 施記レコード書込
010180     END-READ.
010190*
010200*================================================================*
010210 自賠責レコードセット SECTION.
010220*
010230     PERFORM 自賠責情報取得.
010240*/指導運動、算定間隔と月の上限回数を任意に設定する/0505
010250     PERFORM 自賠責基本料金取得.
010260**------------------------------------------------------------------------*
010270* 負－初検請求区分 
010280*   1:初検・再検・初回処置料なし
010290*   2:初検・再検なし
010300*   3:初回処置料なし
010310*   4:初検加算(時間外)
010320*   5:初検加算(休日)
010330*   6:初検加算(深夜)
010340**--- 本PGは、初検と再検の無しをセットするので、区分 1,2 が対象----*
010350**------------------------------------------------------------------------*
010360*
010370     MOVE 連施－施術和暦 TO 負－施術和暦 施術和暦ＣＷ.
010380     MOVE 連施－施術年   TO 負－施術年   施術年ＣＷ.
010390     MOVE 連施－施術月   TO 負－施術月   施術月ＣＷ.
010400     MOVE 連施－施術日   TO              施術日ＣＷ.
010410     MOVE 連施－患者番号 TO 負－患者番号.
010420     MOVE 連施－枝番     TO 負－枝番.
010430*
010440     READ 負傷データＦ
010450     INVALID KEY
010460         MOVE NC"負傷" TO ファイル名Ｗ
010470         PERFORM エラー表示Ｒ
010480     NOT INVALID KEY
010490*
010500* 診療区分＝後療or初検のセットと再検料請求区分のセットを行う
010510         IF 負－初検請求区分 = 1 OR 2
010520             MOVE SPACE TO 再検料請求フラグ
010530         ELSE
010540             PERFORM 労自再検料請求セット
010550         END-IF
010560*
010570* 運動療法料の判定 手動で設定
010580         IF 負－運動療法算定区分 = 1
010590             PERFORM 自賠責運動療法料判定
010600         ELSE
010610             MOVE SPACE TO 運動療法フラグ
010620         END-IF
010630* 指導管理料の判定
010640         IF 負－指導管理算定区分 = ZERO OR SPACE
010650             PERFORM 自賠責指導管理料判定
010660         ELSE
010670             MOVE SPACE TO 指導管理料フラグ
010680         END-IF
010690* 包帯交換料の判定
010700         IF 負－包帯交換算定区分 = ZERO OR SPACE
010710             PERFORM 包帯交換料判定
010720         ELSE
010730             MOVE SPACE TO 包帯交換フラグ
010740         END-IF
010750* 書換（ＲＥＷＲＩＴＥ）に対応できるようＲＥＡＤしておく（重要）
010760         MOVE 連施－施術和暦 TO 施記－施術和暦
010770         MOVE 連施－施術年   TO 施記－施術年
010780         MOVE 連施－施術月   TO 施記－施術月
010790         MOVE 連施－施術日   TO 施記－施術日
010800         MOVE 連施－患者番号 TO 施記－患者番号
010810         MOVE 連施－枝番     TO 施記－枝番
010820         READ 施術記録Ｆ
010830         INVALID KEY
010840             MOVE SPACE TO 施記－レコード
010850             INITIALIZE    施記－レコード
010860             MOVE 連施－施術和暦 TO 施記－施術和暦
010870             MOVE 連施－施術年   TO 施記－施術年
010880             MOVE 連施－施術月   TO 施記－施術月
010890             MOVE 連施－施術日   TO 施記－施術日
010900             MOVE 連施－患者番号 TO 施記－患者番号
010910             MOVE 連施－枝番     TO 施記－枝番
010920         NOT INVALID KEY
010930*
010940* 既存レコードの部位算定情報を更新
010950             PERFORM 修正算定情報初期化
010960*
010970         END-READ
010980*
010990* 初検料請求
011000         MOVE SPACE TO 終了フラグ
011010         MOVE 1    TO  施記－診療区分
011020         PERFORM VARYING 初検カウンタ FROM 1 BY 1
011030                 UNTIL ( 初検カウンタ     > 5      ) OR
011040                       ( 終了フラグ   NOT = SPACE  )
011050             IF ( 連施－初検和暦(初検カウンタ) = ZERO ) OR
011060                ( 連施－初検年(初検カウンタ)   = ZERO ) OR
011070                ( 連施－初検月(初検カウンタ)   = ZERO ) OR
011080                ( 連施－初検日(初検カウンタ)   = ZERO )
011090                 MOVE "YES" TO 終了フラグ
011100             ELSE
011110*
011120                 IF 連施－初検和暦年月日(初検カウンタ) = 連施－施術和暦年月日
011130                     MOVE 2    TO  施記－診療区分
011140                     IF 負－初検請求区分 = 1 OR 2
011150                         MOVE ZERO TO 施記－初検料請求区分
011160                     ELSE
011170                         MOVE 1    TO 施記－初検料請求区分
011180                     END-IF
011190*----------------------------------------------------------------------------------*
011200*                    / 2008/09 改定　初検時相談料区分 0：加算あり　1：加算なし
011210                     IF 負－初検時相談料区分 = 1
011220                         MOVE 1    TO 施記－初検時相談料区分
011230                     ELSE
011240                         MOVE ZERO TO 施記－初検時相談料区分
011250                     END-IF
011260*----------------------------------------------------------------------------------*
011270                     MOVE "YES" TO 終了フラグ
                       ELSE
      */初検日以外は初検料請求区分をクリア/141209
011150                     MOVE ZERO TO 施記－初検料請求区分
011280                 END-IF
011290             END-IF
011300         END-PERFORM
011310*
011320* 再検料請求
011330         IF ( 再検料請求フラグ NOT = SPACE ) AND
011340            ( 施記－診療区分   NOT = 2     )
011350             MOVE 1    TO 施記－再検料請求
011360         ELSE
011370             MOVE ZERO TO 施記－再検料請求
011380         END-IF
011390*/運動更新区分が１なら更新しない/111107
011400         IF 負－運動更新区分 = 1
011410             CONTINUE
011420         ELSE
011430*     運動療法区分 運動療法は手動で設定
011440             IF ( 運動療法フラグ NOT = SPACE ) AND
011450                ( 施記－診療区分 NOT = 2     )
011460                 MOVE 1    TO 施記－運動療法区分
011470             ELSE
011480                 MOVE ZERO TO 施記－運動療法区分
011490             END-IF
011500         END-IF
011510*/指導更新区分が１なら更新しない/111107
011520         IF 負－指導更新区分 = 1
011530             CONTINUE
011540         ELSE
011550* 指導管理区分
011560             IF ( 指導管理料フラグ NOT = SPACE ) AND
011570                ( 施記－診療区分   NOT = 2     )
011580                 MOVE 1    TO 施記－指導管理区分２
011590             ELSE
011600                 MOVE ZERO TO 施記－指導管理区分２
011610             END-IF
011620         END-IF
011630         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
011640                 UNTIL 部位ＣＮＴ > 負－部位数
011650*
011660* 無病の場合、以下の算定は行わない。
011670             IF 負－負傷種別(部位ＣＮＴ) NOT = 09
011680                 IF ( 負－終了和暦(部位ＣＮＴ) = ZERO ) OR
011690                    ( 負－終了年(部位ＣＮＴ)   = ZERO ) OR
011700                    ( 負－終了月(部位ＣＮＴ)   = ZERO ) OR
011710                    ( 負－終了日(部位ＣＮＴ)   = ZERO )
011720*
011730                     MOVE "9999999" TO 終了和暦年月日ＣＷ
011740                 ELSE
011750*
011760                     MOVE 負－終了和暦年月日(部位ＣＮＴ) TO 終了和暦年月日ＣＷ
011770                 END-IF
011780*
011790* 部位コード・転帰区分は、施術日に関係なく転記する。
011800                 MOVE 負－部位コード(部位ＣＮＴ) TO 施記－部位コード(部位ＣＮＴ)
011810                 MOVE 負－転帰区分(部位ＣＮＴ)   TO 施記－転帰区分(部位ＣＮＴ)
011820*/開始日変更に対応する為クリアする/111104
011830                 MOVE ZERO TO 施記－整復施療区分(部位ＣＮＴ)
011840                              施記－医師拘縮区分(部位ＣＮＴ)
011850                              施記－特別材料区分(部位ＣＮＴ)
011860*/包帯更新区分が１なら更新しない/111107
011870                 IF 負－包帯更新区分(部位ＣＮＴ) = 1
011880                     CONTINUE
011890                 ELSE
011900                     MOVE ZERO TO 施記－包帯交換区分(部位ＣＮＴ)
011910                 END-IF
011920*
011930                 IF ( 負－開始和暦年月日(部位ＣＮＴ) <= 施術和暦年月日ＣＷ ) AND
011940                    ( 終了和暦年月日ＣＷ             >= 施術和暦年月日ＣＷ )
011950*
011960                     PERFORM 自賠責算定情報取得
011970                     IF ( 負－開始和暦年月日(部位ＣＮＴ) = 施術和暦年月日ＣＷ ) AND
011980                        ( 負－算定区分(部位ＣＮＴ)   NOT = 1  ) AND
011990                        ( 負－負傷種別(部位ＣＮＴ)   NOT = 07 ) AND
012000                        ( 負－負傷種別(部位ＣＮＴ)   NOT = 08 )
012010*
012020                         MOVE 1 TO 施記－整復施療区分(部位ＣＮＴ)
012030*
012040                     END-IF
012050                     PERFORM 労自罨法算定処理
012060                     PERFORM 後療算定処理
012070*                    ／＊ 医師拘縮　＊／
012080                     IF 負－負傷種別(部位ＣＮＴ) = 07 OR 08
012090                         MOVE 1 TO 施記－医師拘縮区分(部位ＣＮＴ)
012100                     END-IF
012110*
012120*                    ／＊特別材料区分 ＊／
012130*                     IF (施記－診療区分 = 2)
012140                     IF (負－開始和暦年月日(部位ＣＮＴ) = 施記－施術和暦年月日) AND
      */特別材料算定初期値/160725
                              (特別材料区分Ｗ                 = ZERO)
012150                         MOVE 1    TO 施記－特別材料区分(部位ＣＮＴ)
012160                     ELSE
012170                         MOVE ZERO TO 施記－特別材料区分(部位ＣＮＴ)
012180                     END-IF
012190*
012200                 END-IF
012210*/包帯更新区分が１なら更新しない/111107
012220                 IF 負－包帯更新区分(部位ＣＮＴ) = 1
012230                     CONTINUE
012240                 ELSE
012250*                    ／＊包帯交換料 ＊／
012260*                     IF (包帯交換フラグ     = "YES") AND
012270                     IF (部位包帯交換フラグ(部位ＣＮＴ)  = "YES"              ) AND
012280                        (施記－診療区分              NOT = 2                  ) AND
012290                        (負－開始和暦年月日(部位ＣＮＴ) <= 施術和暦年月日ＣＷ ) AND
012300                        ( 終了和暦年月日ＣＷ            >= 施術和暦年月日ＣＷ )
012310                         MOVE 1    TO 施記－包帯交換区分(部位ＣＮＴ)
012320                     ELSE
012330                         MOVE ZERO TO 施記－包帯交換区分(部位ＣＮＴ)
012340                     END-IF
012350                 END-IF
012360             ELSE
012370* 無病時、部位コードのみ転記する/090213
012380                 MOVE 負－部位コード(部位ＣＮＴ) TO 施記－部位コード(部位ＣＮＴ)
012390             END-IF
012400         END-PERFORM
012410*
012420         PERFORM 施記レコード書込
012430     END-READ.
012440*
012450*================================================================*
012460 一般レコードセット SECTION.
012470*
012480**------------------------------------------------------------------------*
012490* 負－初検請求区分 
012500*   1:初検・再検・初回処置料なし
012510*   2:初検・再検なし
012520*   3:初回処置料なし
012530*   4:初検加算(時間外)
012540*   5:初検加算(休日)
012550*   6:初検加算(深夜)
012560**--- 本PGは、初検と再検の無しをセットするので、区分 1,2 が対象----*
012570**------------------------------------------------------------------------*
012580*
HILO  *     DISPLAY "Yk-1 " 参照施記－患者コード 参照施記－施術和暦年月日 " " 連施－施術和暦年月日
012590     MOVE 連施－施術和暦 TO 負－施術和暦 施術和暦ＣＷ.
012600     MOVE 連施－施術年   TO 負－施術年   施術年ＣＷ.
012610     MOVE 連施－施術月   TO 負－施術月   施術月ＣＷ.
012620     MOVE 連施－施術日   TO              施術日ＣＷ.
012630     MOVE 連施－患者番号 TO 負－患者番号.
012640     MOVE 連施－枝番     TO 負－枝番.
012650*
012660     READ 負傷データＦ
012670     INVALID KEY
012680         MOVE NC"負傷" TO ファイル名Ｗ
012690         PERFORM エラー表示Ｒ
012700     NOT INVALID KEY
012710*
012720* 診療区分＝後療or初検のセットと再検料請求区分のセットを行う
012730         IF 負－初検請求区分 = 1 OR 2
012740             MOVE SPACE TO 再検料請求フラグ
012750         ELSE
012760             PERFORM 再検料請求セット
012770         END-IF
012780*
012790* 書換（ＲＥＷＲＩＴＥ）に対応できるようＲＥＡＤしておく（重要）
012800         MOVE 連施－施術和暦 TO 施記－施術和暦
012810         MOVE 連施－施術年   TO 施記－施術年
012820         MOVE 連施－施術月   TO 施記－施術月
012830         MOVE 連施－施術日   TO 施記－施術日
012840         MOVE 連施－患者番号 TO 施記－患者番号
012850         MOVE 連施－枝番     TO 施記－枝番
012860         READ 施術記録Ｆ
012870         INVALID KEY
HILO           DISPLAY "YKE-4 " 施記－明細書発行加算区分

012880             MOVE SPACE TO 施記－レコード
012890             INITIALIZE    施記－レコード
HILO           DISPLAY "YKE-5 " 施記－明細書発行加算区分

012900             MOVE 連施－施術和暦 TO 施記－施術和暦
012910             MOVE 連施－施術年   TO 施記－施術年
012920             MOVE 連施－施術月   TO 施記－施術月
012930             MOVE 連施－施術日   TO 施記－施術日
012940             MOVE 連施－患者番号 TO 施記－患者番号
012950             MOVE 連施－枝番     TO 施記－枝番
012960         NOT INVALID KEY
012970*
012980* 既存レコードの部位算定情報を更新
012990             PERFORM 修正算定情報初期化
013000         END-READ
013010*
013020* 初検料請求
013030         MOVE SPACE TO 終了フラグ
013040         MOVE 1    TO  施記－診療区分
      */初検日以外は初検料請求区分をクリア/170708
011150         MOVE ZERO TO 施記－初検料請求区分
      *
013050         PERFORM VARYING 初検カウンタ FROM 1 BY 1
013060                 UNTIL ( 初検カウンタ     > 5      ) OR
013070                       ( 終了フラグ   NOT = SPACE  )
013080             IF ( 連施－初検和暦(初検カウンタ) = ZERO ) OR
013090                ( 連施－初検年(初検カウンタ)   = ZERO ) OR
013100                ( 連施－初検月(初検カウンタ)   = ZERO ) OR
013110                ( 連施－初検日(初検カウンタ)   = ZERO )
013120                 MOVE "YES" TO 終了フラグ
013130             ELSE
013140                 IF 連施－初検和暦年月日(初検カウンタ) = 連施－施術和暦年月日
013150                     MOVE 2    TO  施記－診療区分
013160                     IF 負－初検請求区分 = 1 OR 2
013170                         MOVE ZERO TO 施記－初検料請求区分
013180                     ELSE
013190                         MOVE 1    TO 施記－初検料請求区分
013200                     END-IF
013210*----------------------------------------------------------------------------------*
013220*                    / 2008/06 改定　初検時相談料区分 0：加算あり　1：加算なし
013230                     IF 負－初検時相談料区分 = 1
013240                         MOVE 1    TO 施記－初検時相談料区分
013250                     ELSE
013260                         MOVE ZERO TO 施記－初検時相談料区分
013270                     END-IF
013280*----------------------------------------------------------------------------------*
013290                     MOVE "YES" TO 終了フラグ
013300                 END-IF
013310             END-IF
013320         END-PERFORM
013330*
013340*-----------------------------------------------------------------------------------*
013350*        / 初検でかつ 初検時相談料区分 0：加算ありか
013360         IF ( 施記－診療区分 = 2 ) AND ( 施記－初検時相談料区分 = ZERO )
013370*
013380                          COMPUTE 相談カウンタ = 相談カウンタ + 1
013390*
HILO***/特殊ありの日が対象にならない為別途カウントする/*20240513
                   PERFORM 相談回数取得
HILO  *            DISPLAY 施記－施術和暦年月日 " 施記－診療区分=" 施記－診療区分 " 相談カウンタ=" 相談カウンタ
      *↓↓↓/20240513
013400*                          IF 相談カウンタ > 1
013400                          IF 相談カウンタ >= 1
      *↑↑↑/20240513
013410                             MOVE 1   TO 施記－初検時相談料区分
013420                          ELSE
013430*                            / 無病は加算なし /
013440                             PERFORM VARYING 部位カウンタ FROM 1 BY 1  UNTIL  部位カウンタ > 7
013450                                IF ( 負－負傷種別(部位カウンタ) = 09 ) AND ( 負－部位(部位カウンタ) = 01 )
013460                                    IF  ( 負－開始和暦(部位カウンタ) = 施記－施術和暦 ) AND
013470                                        ( 負－開始年(部位カウンタ)   = 施記－施術年 ) AND
013480                                        ( 負－開始月(部位カウンタ)   = 施記－施術月 ) AND
013490                                        ( 負－開始日(部位カウンタ)   = 施記－施術日 )
013500*
013510                                           MOVE 1   TO 施記－初検時相談料区分
013520*                                          / カウンタ0にする
013530                                           MOVE ZERO TO 相談カウンタ
013540                                           MOVE 7    TO 部位カウンタ
013550                                    END-IF
013560                                END-IF
013570                             END-PERFORM
013580                          END-IF
013590         END-IF
013600*-----------------------------------------------------------------------------------*
013610*
013620*
013630* 再検料請求
013640         MOVE SPACE TO 再検料初期化フラグ
013650         IF ( 再検料請求フラグ NOT = SPACE ) AND
013660            ( 施記－診療区分   NOT = 2     )
013670             MOVE 1     TO 施記－再検料請求
013680             MOVE "YES" TO 再検料初期化フラグ
013690         ELSE
013700             MOVE ZERO  TO 施記－再検料請求
013710             MOVE SPACE TO 再検料初期化フラグ
013720         END-IF
013730*
      */令和4年10月より明細書発行加算料/20220704
020020         MOVE 連施－施術和暦 TO 受－施術和暦
020030         MOVE 連施－施術年   TO 受－施術年
020040         MOVE 連施－施術月   TO 受－施術月
020050         MOVE 連施－患者番号 TO 受－患者番号
020060         MOVE 連施－枝番     TO 受－枝番
020070         READ 受診者情報Ｆ
020080         INVALID KEY
                   MOVE SPACE TO 受－レコード
020210         END-READ
      */明細書発行加算。過去年月のフラグを変更しない。/20240725
               IF 施記－施術和暦年月 >= 明細変更和暦年月Ｗ
               IF (施記－施術和暦年月    >= 50410) AND 
                  (明細発行加算区分Ｗ     = 1    ) AND 
      *            (受－領収証必要区分 NOT = 3    )
                  (負－明細書算定区分 = ZERO)
013730             PERFORM 明細書加算算定
               ELSE
033900             MOVE ZERO TO 施記－明細書発行加算区分
               END-IF
      */明細書発行加算。過去年月のフラグを変更しない。/20240725
               END-IF
013730*
013740         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
013750                 UNTIL 部位ＣＮＴ > 負－部位数
013760*
013770* 無病の場合、以下の算定は行わない。
013780             IF 負－負傷種別(部位ＣＮＴ) NOT = 09
013790                 IF ( 負－終了和暦(部位ＣＮＴ) = ZERO ) OR
013800                    ( 負－終了年(部位ＣＮＴ)   = ZERO ) OR
013810                    ( 負－終了月(部位ＣＮＴ)   = ZERO ) OR
013820                    ( 負－終了日(部位ＣＮＴ)   = ZERO )
013830*
013840                     MOVE "9999999" TO 終了和暦年月日ＣＷ
013850                 ELSE
013860*
013870                     MOVE 負－終了和暦年月日(部位ＣＮＴ) TO 終了和暦年月日ＣＷ
013880                 END-IF
013890*
013900* 部位コード・転帰区分は、施術日に関係なく転記する。
013910                 MOVE 負－部位コード(部位ＣＮＴ) TO 施記－部位コード(部位ＣＮＴ)
013920                 MOVE 負－転帰区分(部位ＣＮＴ)   TO 施記－転帰区分(部位ＣＮＴ)
013930*
013940                 IF ( 負－開始和暦年月日(部位ＣＮＴ) <= 施術和暦年月日ＣＷ ) AND
013950                    ( 終了和暦年月日ＣＷ             >= 施術和暦年月日ＣＷ )
013960*
013970                     PERFORM 算定情報取得
013980                     PERFORM 罨法算定処理
013990*
014000                     IF ( 負－開始和暦年月日(部位ＣＮＴ) = 施術和暦年月日ＣＷ ) AND
014010                        ( 負－算定区分(部位ＣＮＴ)   NOT = 1  ) AND
014020                        ( 負－負傷種別(部位ＣＮＴ)   NOT = 07 ) AND
014030                        ( 負－負傷種別(部位ＣＮＴ)   NOT = 08 )
014040*
014050                         MOVE 1 TO 施記－整復施療区分(部位ＣＮＴ)
014060*
014070                     END-IF
014080*
014090                     PERFORM 後療算定処理
014100*
014110*                    ／＊ 医師拘縮　＊／
014120                     IF 負－負傷種別(部位ＣＮＴ) = 07 OR 08
014130                         MOVE 1 TO 施記－医師拘縮区分(部位ＣＮＴ)
014140                     END-IF
014150*
014160                 END-IF
014170             ELSE
014180* 部位コード・転帰区分は、施術日に関係なく転記する。
014190                 MOVE 負－部位コード(部位ＣＮＴ) TO 施記－部位コード(部位ＣＮＴ)
014200             END-IF
014210         END-PERFORM
014220*
014230         PERFORM 施記レコード書込
014240*
014250*    / 再検日以降の再検料初期化 /
014260         IF 再検料初期化フラグ = "YES"
014270            PERFORM 再検料初期化処理
014280         END-IF
014290*
014300     END-READ.
014310*================================================================*
014310*================================================================*
       相談回数取得 SECTION.
      *
007330     MOVE ZERO TO 相談カウンタ.
007340*
           MOVE 施記－レコードキー TO 参２施記－レコードキー
007410*
007420     START 参照施術記録Ｆ２ KEY IS < 参２施記－患者コード 参２施記－施術和暦年月日 REVERSED
007430     END-START.
007440*
007450     IF 状態キー = "00"
007460         MOVE SPACE TO 終了フラグ６
007470         PERFORM 参照施術記録Ｆ読込２
007480         PERFORM UNTIL ( 施記－施術和暦   NOT = 参２施記－施術和暦   ) OR
007490                       ( 施記－施術年     NOT = 参２施記－施術年     ) OR
007500                       ( 施記－施術月     NOT = 参２施記－施術月     ) OR
007510                       ( 施記－患者コード NOT = 参２施記－患者コード ) OR
007520                       ( 終了フラグ６     NOT = SPACE )
007530*
013220*            初検時相談料区分 0：加算あり　1：加算なし
013230             IF (負－初検時相談料区分 = ZERO) AND (参２施記－初検時相談料区分 = ZERO) AND ( 参２施記－診療区分 = 2 )
013240                 COMPUTE 相談カウンタ = 相談カウンタ + 1
013250             END-IF
007470             PERFORM 参照施術記録Ｆ読込２
               END-PERFORM
           END-IF.
HILO  *     DISPLAY "相談カウンタ " 相談カウンタ.
      *
007760*================================================================*
007770 参照施術記録Ｆ読込２ SECTION.
007780*
007790     READ 参照施術記録Ｆ２ NEXT
007800     AT END
007810         MOVE "YES"  TO 終了フラグ６
007820     END-READ.
007830*
007840*================================================================*
014310*================================================================*
014320 罨法算定処理 SECTION.
014330*
014340     MOVE 負－負傷和暦(部位ＣＮＴ) TO 計算和暦Ｗ.
014350     MOVE 負－負傷年(部位ＣＮＴ)   TO 計算年Ｗ.
014360     MOVE 負－負傷月(部位ＣＮＴ)   TO 計算月Ｗ.
014370     MOVE 負－負傷日(部位ＣＮＴ)   TO 計算日Ｗ.
014380     PERFORM 西暦年取得.
014390     PERFORM 日付絶対値計算.
014400     MOVE 絶対値Ｗ                 TO 負傷絶対値Ｗ.
014410*
014420     MOVE 連施－施術和暦           TO 計算和暦Ｗ.
014430     MOVE 連施－施術年             TO 計算年Ｗ.
014440     MOVE 連施－施術月             TO 計算月Ｗ.
014450     MOVE 連施－施術日             TO 計算日Ｗ.
014460     PERFORM 西暦年取得.
014470     PERFORM 日付絶対値計算.
014480     MOVE 絶対値Ｗ                 TO 施術絶対値Ｗ.
014490*
014500     COMPUTE 負傷経過日Ｗ = 施術絶対値Ｗ - ( 負傷絶対値Ｗ - 1 ).
014510*
014520     EVALUATE 負－負傷種別(部位ＣＮＴ)
014530* 不全骨折・骨折（医師拘縮も含む）
014540     WHEN 05
014550     WHEN 06
014560     WHEN 07
014570     WHEN 08
014580         IF 負傷経過日Ｗ <= 冷罨日数骨折Ｗ
014590             MOVE 1 TO 施記－罨法区分(部位ＣＮＴ)
014600         END-IF
014610         IF 負傷経過日Ｗ > 温罨待機骨折Ｗ
014620* 　　　　　 ／＊　整復・固定を行った初検の日は温罨法料算定できない　＊／
014630             IF ( 負－算定区分(部位ＣＮＴ)    NOT = 1  ) AND
014640                ( 負－負傷種別(部位ＣＮＴ)    NOT = 07 ) AND
014650                ( 負－負傷種別(部位ＣＮＴ)    NOT = 08 ) AND
014660                ( 負－開始和暦年月日(部位ＣＮＴ)  = 施術和暦年月日ＣＷ )
014670                 MOVE ZERO TO 施記－罨法区分(部位ＣＮＴ)
014680                 MOVE ZERO TO 施記－電療区分(部位ＣＮＴ)
014690             ELSE
014700* 　　　　　　　　／＊　後療から行う場合は算定ＯＫ　＊／
014710                 MOVE 2 TO 施記－罨法区分(部位ＣＮＴ)
014720                 IF 負－電療請求区分(部位ＣＮＴ) = ZERO
014730                     MOVE 1 TO 施記－電療区分(部位ＣＮＴ)
014740                 END-IF
014750             END-IF
014760         END-IF
014770* 脱臼
014780     WHEN 04
014790         IF 負傷経過日Ｗ <= 冷罨日数脱臼Ｗ
014800             MOVE 1 TO 施記－罨法区分(部位ＣＮＴ)
014810         END-IF
014820         IF 負傷経過日Ｗ > 温罨待機脱臼Ｗ
014830* 　　　　　／＊　整復・施療を行った初検の日は温罨法料算定できない　＊／
014840             IF ( 負－算定区分(部位ＣＮＴ)    NOT = 1  ) AND
014850                ( 負－負傷種別(部位ＣＮＴ)    NOT = 07 ) AND
014860                ( 負－負傷種別(部位ＣＮＴ)    NOT = 08 ) AND
014870                ( 負－開始和暦年月日(部位ＣＮＴ)  = 施術和暦年月日ＣＷ )
014880                 MOVE ZERO TO 施記－罨法区分(部位ＣＮＴ)
014890                 MOVE ZERO TO 施記－電療区分(部位ＣＮＴ)
014900             ELSE
014910* 　　　　　　　　／＊　後療から行う場合は算定ＯＫ　＊／
014920                 MOVE 2 TO 施記－罨法区分(部位ＣＮＴ)
014930                 IF 負－電療請求区分(部位ＣＮＴ) NOT = ZERO
014940                     MOVE ZERO TO 施記－電療区分(部位ＣＮＴ)
014950                 ELSE
014960                     MOVE 1 TO 施記－電療区分(部位ＣＮＴ)
014970                 END-IF
014980             END-IF
014990         END-IF
015000* 打撲・捻挫・挫傷
015010     WHEN 01
015020     WHEN 02
015030     WHEN 03
015040*
015050         IF 負傷経過日Ｗ <= 冷罨日数打撲Ｗ
015060             EVALUATE 負傷経過日Ｗ
015070* 負傷日の場合は、冷罨法算定
015080             WHEN 1
015090                 MOVE 1 TO 施記－罨法区分(部位ＣＮＴ)
015100* 負傷日の翌日場合は、初検日のみ冷罨法算定
015110             WHEN 2
015120                 IF 負－開始和暦年月日(部位ＣＮＴ) = 施術和暦年月日ＣＷ
015130                      MOVE 1 TO 施記－罨法区分(部位ＣＮＴ)
015140                 END-IF
015150             WHEN OTHER
015160                 CONTINUE
015170             END-EVALUATE
015180         END-IF
015190         IF 負傷経過日Ｗ > 温罨待機脱臼Ｗ
015200* 　　　　　／＊　整復・施療を行った初検の日は温罨法料算定できない　＊／
015210             IF ( 負－算定区分(部位ＣＮＴ)    NOT = 1  ) AND
015220                ( 負－負傷種別(部位ＣＮＴ)    NOT = 07 ) AND
015230                ( 負－負傷種別(部位ＣＮＴ)    NOT = 08 ) AND
015240                ( 負－開始和暦年月日(部位ＣＮＴ)  = 施術和暦年月日ＣＷ )
015250                 MOVE ZERO TO 施記－罨法区分(部位ＣＮＴ)
015260                 MOVE ZERO TO 施記－電療区分(部位ＣＮＴ)
015270             ELSE
015280* 　　　　　　　　／＊　後療から行う場合は算定ＯＫ　＊／
015290                 MOVE 2 TO 施記－罨法区分(部位ＣＮＴ)
015300                 IF 負－電療請求区分(部位ＣＮＴ) = ZERO
015310                     MOVE 1 TO 施記－電療区分(部位ＣＮＴ)
015320                 END-IF
015330             END-IF
015340         END-IF
015350     WHEN OTHER
015360         CONTINUE
015370     END-EVALUATE.
015380*
HILO  *     DISPLAY "罨法算定処理-1 " 施記－施術和暦年月日 "(" 部位ＣＮＴ ") " 冷罨法区分Ｗ 温罨法区分Ｗ 電療料区分Ｗ "  " 施記－罨法区分(部位ＣＮＴ) 施記－電療区分(部位ＣＮＴ).
      *初期値の追加(冷温電)/20230615
           IF 施記－罨法区分(部位ＣＮＴ) = 1 AND 冷罨法区分Ｗ = 1
               MOVE ZERO TO 施記－罨法区分(部位ＣＮＴ)
           END-IF.
           IF 施記－罨法区分(部位ＣＮＴ) = 2 AND 温罨法区分Ｗ = 1
               MOVE ZERO TO 施記－罨法区分(部位ＣＮＴ)
           END-IF.
           IF  施記－電療区分(部位ＣＮＴ) = 1 AND 電療料区分Ｗ = 1
               MOVE ZERO TO 施記－電療区分(部位ＣＮＴ)
           END-IF.
HILO  *     DISPLAY "罨法算定処理-2 " 施記－施術和暦年月日 "(" 部位ＣＮＴ ") " 冷罨法区分Ｗ 温罨法区分Ｗ 電療料区分Ｗ "  " 施記－罨法区分(部位ＣＮＴ) 施記－電療区分(部位ＣＮＴ).
015390*================================================================*
015400 西暦年取得 SECTION.
015410*
015420     MOVE 計算和暦Ｗ TO 元－元号区分.
015430     READ 元号マスタ
015440     INVALID KEY
015450         DISPLAY NC"指定和暦が登録されていません" UPON CONS
015460         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
015470         ACCEPT  キー入力 FROM CONS
015480         PERFORM 終了処理
015490         EXIT PROGRAM
015500     NOT INVALID KEY
015510         COMPUTE 西暦年Ｗ = 元－開始西暦年 + 計算年Ｗ - 1
015520     END-READ.
015530*
015540*================================================================*
015550 日付絶対値計算 SECTION.
015560*
015570** 1989年を基準とし、当年まで閏年が何回あるかを計算
015580** 元号マスタの西暦開始年確認。
015590*     COMPUTE 西暦年差Ｗ   = 西暦年Ｗ - 1989 - 1.
015600*     DIVIDE 4 INTO 西暦年差Ｗ GIVING 閏年回数Ｗ
015610*                            REMAINDER 剰余Ｗ
015620*     END-DIVIDE.
015630*/基準年1989年から当年までの閏年の回数を計算(当年は含まない)
015640*/始めのうるう年の1992年から数える
015650     COMPUTE 西暦年差Ｗ   = 西暦年Ｗ - 1989 - 1.
015660     MOVE 1992 TO 加算年Ｗ.
015670     MOVE ZERO TO 閏年回数Ｗ.
015680     PERFORM UNTIL 西暦年Ｗ <= 加算年Ｗ
015690         COMPUTE 加算年Ｗ   = 加算年Ｗ   + 4
015700         COMPUTE 閏年回数Ｗ = 閏年回数Ｗ + 1
015710     END-PERFORM.
015720*
015730     COMPUTE 通常年回数Ｗ     = 西暦年差Ｗ     - 閏年回数Ｗ.
015740     COMPUTE 通常年絶対値Ｗ   = 通常年回数Ｗ   * 365.
015750     COMPUTE 閏年絶対値Ｗ     = 閏年回数Ｗ     * 366.
015760     COMPUTE 年絶対値Ｗ       = 通常年絶対値Ｗ + 閏年絶対値Ｗ.
015770* 当年がうるう年かどうかを調べる（２月の末日計算）
015780*     COMPUTE 西暦年差２Ｗ     = 西暦年Ｗ + 1.
015790*     DIVIDE 4 INTO 西暦年差２Ｗ GIVING 閏年回数Ｗ
015800*                                REMAINDER 剰余Ｗ
015810*     END-DIVIDE.
015820     DIVIDE 4 INTO 西暦年Ｗ GIVING 閏年回数Ｗ
015830                            REMAINDER 剰余Ｗ
015840     END-DIVIDE.
015850*
015860     MOVE ZERO TO 絶対値月Ｗ.
015870*
015880* 前月までの絶対値を累計
015890     COMPUTE 絶対値月Ｗ２ = 計算月Ｗ * 30.
015900*
015910     EVALUATE 計算月Ｗ
015920     WHEN 3
015930         COMPUTE 絶対値月Ｗ   = 絶対値月Ｗ２ - 1
015940     WHEN 2
015950     WHEN 6
015960     WHEN 7
015970         COMPUTE 絶対値月Ｗ   = 絶対値月Ｗ２ + 1
015980     WHEN 8
015990         COMPUTE 絶対値月Ｗ   = 絶対値月Ｗ２ + 2
016000     WHEN 9
016010     WHEN 10
016020         COMPUTE 絶対値月Ｗ   = 絶対値月Ｗ２ + 3
016030     WHEN 11
016040     WHEN 12
016050         COMPUTE 絶対値月Ｗ   = 絶対値月Ｗ２ + 4
016060     WHEN OTHER
016070         MOVE 絶対値月Ｗ２ TO 絶対値月Ｗ
016080     END-EVALUATE.
016090*
016100     IF ( 剰余Ｗ = ZERO ) AND
016110        ( 計算月Ｗ > 2  )
016120         COMPUTE 絶対値月Ｗ = 絶対値月Ｗ + 1
016130     END-IF.
016140*
016150     COMPUTE 絶対値Ｗ   = 年絶対値Ｗ + 絶対値月Ｗ + 計算日Ｗ.
016160*
016170*================================================================*
016180 算定情報取得 SECTION.
016190*
016200* 健康保険：０１　労災・自賠はあとで考慮
016210*
016220     MOVE 01             TO 計－制御区分.
016230     MOVE 連施－施術和暦 TO 計－開始和暦 施術和暦ＣＷ.
016240     MOVE 連施－施術年   TO 計－開始年   施術年ＣＷ.
016250     MOVE 連施－施術月   TO 計－開始月   施術月ＣＷ.
016260*
016270     START 計算マスタ KEY IS <= 計－制御区分 計－開始和暦年月
016280                                                     REVERSED
016290     END-START.
016300*
016310     IF 状態キー = "00"
016320*
016330         READ 計算マスタ NEXT
016340         AT END
016350             PERFORM コンソールエラー確認
016360             DISPLAY NC"施術年月に対応した算定情報がみつかりません１" UPON CONS
016370             PERFORM エラー終了
016380         NOT AT END
016390             IF ( 施術和暦年月ＣＷ >= 計Ａ－開始和暦年月 ) AND
016400                ( 施術和暦年月ＣＷ <= 計Ａ－終了和暦年月 )
016410*
016420                 MOVE 計Ａ－冷罨日数骨折 TO 冷罨日数骨折Ｗ
016430                 MOVE 計Ａ－冷罨日数脱臼 TO 冷罨日数脱臼Ｗ
016440                 MOVE 計Ａ－冷罨日数打撲 TO 冷罨日数打撲Ｗ
016450                 MOVE 計Ａ－温罨待機骨折 TO 温罨待機骨折Ｗ
016460                 MOVE 計Ａ－温罨待機脱臼 TO 温罨待機脱臼Ｗ
016470             ELSE
016480                 PERFORM コンソールエラー確認
016490                 DISPLAY NC"施術年月に対応した算定情報がみつかりません２" UPON CONS
016500                 PERFORM エラー終了
016510             END-IF
016520         END-READ
016530     END-IF.
016540*
016550*================================================================*
016560 施記レコード書込 SECTION.
016570*
016580     WRITE 施記－レコード
016590     END-WRITE.
016600*
016610     EVALUATE 状態キー
016620     WHEN "00"
016630         CONTINUE
hilo  *         DISPLAY "yk-1 " 施記－レコードキー " 施記－明細書発行加算区分= " 施記－明細書発行加算区分
016640     WHEN "22"
016650         REWRITE 施記－レコード
016660         INVALID KEY
016670             MOVE NC"施記" TO ファイル名Ｗ
016680             PERFORM エラー表示
hilo  *         NOT INVALID KEY DISPLAY "yk-2 " 施記－レコードキー " 施記－明細書発行加算区分= " 施記－明細書発行加算区分
016690         END-REWRITE
016700     WHEN OTHER
016710         MOVE NC"施記" TO ファイル名Ｗ
016720         PERFORM エラー表示
016730     END-EVALUATE.
016740*
016750*================================================================*
016760 再検料請求セット SECTION.
016770*
016780******************************************************************
016790* 初検後療日に再検料を算定：初検日の次診療日かを判定し、
016800* 再検日の場合、再検料請求フラグに"YES"をたて、
016810* 施記－再検料請求をセットする
016820******************************************************************
016830*
016840     MOVE SPACE TO 再検料請求フラグ.
016850     MOVE SPACE TO 枝番チェックフラグ.
016860*
016870     MOVE 連施－施術和暦 TO 施記－施術和暦.
016880     MOVE 連施－施術年   TO 施記－施術年.
016890     MOVE 連施－施術月   TO 施記－施術月.
016900     MOVE 連施－施術日   TO 施記－施術日.
016910     MOVE 連施－患者番号 TO 施記－患者番号.
016920     MOVE 連施－枝番     TO 施記－枝番.
016930     START 施術記録Ｆ KEY IS < 施記－患者コード 施記－施術和暦年月日
016940                               REVERSED
016950     END-START.
016960*
016970     IF 状態キー = "00"
016980         READ 施術記録Ｆ NEXT
016990         AT END
017000             MOVE "YES" TO 枝番チェックフラグ
017010         NOT AT END
017020* 診療区分：２＝初検日（初検料算定の日）
      * 初険日でも初険料を算定しない場合は再検料も算定しない/141209
017030             IF  ( 連施－患者コード = 施記－患者コード ) AND
017040                 ( 施記－診療区分 = 2                  )
                   AND ( 施記－初検料請求区分 = 1            )          *>/初険料を算定しない場合/141209
017050*               */前回診療日が当月か前月の時のみ/080912
017060                 PERFORM 前月判定
017070                 IF (前月フラグ = "YES") OR
017080                   ((連施－施術和暦 = 施記－施術和暦) AND
017090                    (連施－施術年   = 施記－施術年  ) AND
017100                    (連施－施術月   = 施記－施術月  ))
017110                     MOVE "YES" TO 再検料請求フラグ
017120                 END-IF
017130             END-IF
017140             IF (連施－患者コード NOT = 施記－患者コード) OR
017150                (連施－施術和暦   NOT = 施記－施術和暦  ) OR
017160                (連施－施術年     NOT = 施記－施術年    ) OR
017170                (連施－施術月     NOT = 施記－施術月    )
017180                 MOVE "YES" TO 枝番チェックフラグ
017190             END-IF
017200         END-READ
017210     ELSE
017220         MOVE "YES" TO 枝番チェックフラグ
017230     END-IF.
017240*
017250*/当月で最初の通院日の時枝番の存在を調べ
017260*/当月に枝番があったら前枝番をチェックする/080912
017270     IF 枝番チェックフラグ = "YES"
017280         MOVE SPACE TO 連受診枝番ＣＨＫ－キー
017290         INITIALIZE    連受診枝番ＣＨＫ－キー
017300         MOVE 連施－施術和暦 TO 連受診枝番ＣＨＫ－施術和暦
017310         MOVE 連施－施術年   TO 連受診枝番ＣＨＫ－施術年
017320         MOVE 連施－施術月   TO 連受診枝番ＣＨＫ－施術月
017330         MOVE 連施－患者番号 TO 連受診枝番ＣＨＫ－患者番号
017340         MOVE SPACE          TO 連受診枝番ＣＨＫ－枝番
017350         CALL   "EDACHK"
017360         CANCEL "EDACHK"
017370*        （0:なし、1:あり）
017380         IF 連受診枝番ＣＨＫ－枝番あり = 1
017390             MOVE 連施－施術和暦 TO 受－施術和暦
017400             MOVE 連施－施術年   TO 受－施術年
017410             MOVE 連施－施術月   TO 受－施術月
017420             MOVE 連施－患者番号 TO 受－患者番号
017430             IF 連施－枝番 = SPACE
017440                 MOVE HIGH-VALUE TO 受－枝番
017450             ELSE
017460                 MOVE 連施－枝番 TO 受－枝番
017470             END-IF
017480* 
017490             START 受診者情報Ｆ KEY IS < 受－施術和暦年月 受－患者コード REVERSED
017500             END-START
017510             IF 状態キー = "00"
      */枝番ありの判定から、自費x・生保h・自賠責a・労災rを除く↓↓↓/160706
017520*                 READ 受診者情報Ｆ NEXT
017530*                 NOT AT END
                       MOVE SPACE TO 終了フラグ４
                       PERFORM 受診者情報Ｆ読込
                       PERFORM UNTIL (受－枝番     NOT = "x" AND "h" AND "a" AND "r") OR
                                     (終了フラグ４ NOT = SPACE)
                           PERFORM 受診者情報Ｆ読込
                       END-PERFORM
                       IF (連施－患者番号 = 受－患者番号) AND
                          (連施－施術和暦 = 受－施術和暦) AND
                          (連施－施術年月 = 受－施術年月)
      */枝番ありの判定から、自費x・生保h・自賠責a・労災rを除く↑↑↑/160706
017540                     MOVE 連施－施術和暦 TO 施記－施術和暦
017550                     MOVE 連施－施術年   TO 施記－施術年
017560                     MOVE 連施－施術月   TO 施記－施術月
017570                     MOVE 連施－施術日   TO 施記－施術日
017580                     MOVE 受－患者番号   TO 施記－患者番号
017590                     MOVE 受－枝番       TO 施記－枝番
017600                     START 施術記録Ｆ KEY IS < 施記－患者コード 施記－施術和暦年月日
017610                                                                            REVERSED
017620                     END-START
017630*
017640                     IF 状態キー = "00"
017650*/枝番あり、前通院日ありの時フラグをクリアしセットしなおす/
017660                         MOVE SPACE TO 再検料請求フラグ
017670                         READ 施術記録Ｆ NEXT
017680                         NOT AT END
017690* 診療区分：２＝初検日（初検料算定の日）
HILO  *                             DISPLAY "yk-4 前日判定" 施記－レコードキー
017700                             IF ( 受－患者コード       = 施記－患者コード ) AND
017710                                ( 施記－診療区分       = 2 ) AND
                                      ( 施記－初検料請求区分 = 1 )               *>/初険料を算定しない場合/141209/160706(修正漏)
017720* 前回診療日が前月の時のみ/080912
017730                                 PERFORM 前月判定
017740                                 IF (前月フラグ = "YES") OR
017750                                   ((連施－施術和暦 = 施記－施術和暦) AND
017760                                    (連施－施術年   = 施記－施術年  ) AND
017770                                    (連施－施術月   = 施記－施術月  ))
017780                                     MOVE "YES" TO 再検料請求フラグ
HILO  *                                     DISPLAY "再検算定する１" ELSE DISPLAY "再検算定しない１" *>HILO
017790                                 END-IF
HILO  *                             ELSE DISPLAY "再検算定しない２" *>HILO
017800                             END-IF
      */枝番ありの判定から、自費x・生保h・自賠責a・労災rを除く↓↓↓/160706
017810*                         END-READ
                               END-IF
      */枝番ありの判定から、自費x・生保h・自賠責a・労災rを除く↑↑↑/160706
017820                     END-IF
017830*                 END-READ
017840             END-IF
017850         END-IF
017860     END-IF.
017870*
017880*================================================================*
017890 受診者情報Ｆ読込 SECTION.
017900*
017910     READ 受診者情報Ｆ NEXT
017920     AT END
017930         MOVE "YES" TO 終了フラグ４
017940     END-READ.
017950*================================================================*
017960 修正算定情報初期化 SECTION.
017970*
017980     MOVE 1    TO 施記－診療区分.
017990     MOVE ZERO TO 施記－再検料請求.
018000*
018010*/初期化項目は部位数分だけでなく全て初期化するように変更。020423
018020     PERFORM VARYING 負傷ＣＮＴ FROM 1 BY 1
018030*             UNTIL   負傷ＣＮＴ > 負－部位数
018040             UNTIL   負傷ＣＮＴ > 9
018050         MOVE ZERO TO 施記－負傷種別(負傷ＣＮＴ)
018060         MOVE ZERO TO 施記－部位(負傷ＣＮＴ)
018070         MOVE ZERO TO 施記－左右区分(負傷ＣＮＴ)
018080         MOVE ZERO TO 施記－負傷位置番号(負傷ＣＮＴ)
018090*
018100         MOVE ZERO TO 施記－転帰区分(負傷ＣＮＴ)
018110         MOVE ZERO TO 施記－整復施療区分(負傷ＣＮＴ)
018120         MOVE ZERO TO 施記－医師拘縮区分(負傷ＣＮＴ)
018130         MOVE ZERO TO 施記－罨法区分(負傷ＣＮＴ)
018140         MOVE ZERO TO 施記－電療区分(負傷ＣＮＴ)
018150         MOVE ZERO TO 施記－後療料請求区分(負傷ＣＮＴ)
018160     END-PERFORM.
018170*
018180* 詳細項目については、あえて初期化しない。計算では、開始日終了日
018190* 判定をしている為、影響なし。労災・自賠の際、再検討。
018200*
018210*/一部変更
018220*/初期化項目は部位数分だけでなく全て初期化するように変更。
018230*/部位数、開始日、終了日で判定している為、計算には影響ないが
018240*/２号誌は負傷データを読ずに施術記録Ｆをそのままセットする為、
018250*/２号誌のみで不具合が出る。
018260*================================================================*
018270 後療算定処理 SECTION.
018280*
018290     EVALUATE 施記－診療区分
018300     WHEN 1
018310         IF 施記－整復施療区分(部位ＣＮＴ) = ZERO
018320             MOVE 1    TO 施記－後療料請求区分(部位ＣＮＴ)
018330         ELSE
018340             MOVE ZERO TO 施記－後療料請求区分(部位ＣＮＴ)
018350         END-IF
018360     WHEN 2
018370         IF 施記－整復施療区分(部位ＣＮＴ) = ZERO
018380             MOVE 1    TO 施記－後療料請求区分(部位ＣＮＴ)
018390         ELSE
018400             MOVE ZERO TO 施記－後療料請求区分(部位ＣＮＴ)
018410         END-IF
018420     WHEN OTHER
018430         MOVE 1    TO 施記－後療料請求区分(部位ＣＮＴ)
018440     END-EVALUATE.
018450*
018460*================================================================*
018470 再検料初期化処理 SECTION.
018480*
018490*  / 再検日以降で次の初検日まで(当月中)、再検料請求を初期化する。 /
018500*
018510      MOVE 連施－患者番号  TO 施記－患者番号.
018520      MOVE 連施－枝番      TO 施記－枝番.
018530      MOVE 連施－施術和暦  TO 施記－施術和暦.
018540      MOVE 連施－施術年    TO 施記－施術年.
018550      MOVE 連施－施術月    TO 施記－施術月.
018560      MOVE 連施－施術日    TO 施記－施術日.
018570      START 施術記録Ｆ   KEY IS >  施記－患者コード
018580                                   施記－施術和暦年月日
018590      END-START.
018600      IF 状態キー = "00"
018610             MOVE SPACE TO 終了フラグ２
018620             PERFORM 施術記録Ｆ読込
018630             PERFORM UNTIL ( 施記－施術和暦   NOT = 連施－施術和暦  ) OR
018640                           ( 施記－施術年     NOT = 連施－施術年    ) OR
018650                           ( 施記－施術月     NOT = 連施－施術月    ) OR
018660                           ( 施記－患者番号   NOT = 連施－患者番号  ) OR
018670                           ( 施記－枝番       NOT = 連施－枝番      ) OR
018680                           ( 施記－診療区分 = 2  )  OR
018690                           ( 終了フラグ２     NOT = SPACE )
018700*
018710                  IF 施記－再検料請求 NOT = ZERO
018720                      MOVE ZERO  TO 施記－再検料請求
018730*
018740                      REWRITE 施記－レコード
018750                      INVALID KEY
018760                         MOVE NC"施記" TO ファイル名Ｗ
018770                         PERFORM エラー表示
018780                      END-REWRITE
018790                  END-IF
018800*
018810                  PERFORM 施術記録Ｆ読込
018820*
018830             END-PERFORM
018840      END-IF.
018850*
018860*================================================================*
018870 施術記録Ｆ読込 SECTION.
018880*
018890     READ 施術記録Ｆ NEXT
018900     AT END
018910         MOVE "YES" TO 終了フラグ２
018920     END-READ.
018930*================================================================*
018940 前月判定  SECTION.
018950*------------------------------------------------------------------------------* 
018960*-- 読み込んだ次月の負傷データの年月が、前月かどうか判定 (年月の差が 1 か?) ---*
018970*------------------------------------------------------------------------------* 
018980*
018990      MOVE  SPACE  TO  前月フラグ.
019000      INITIALIZE  計算年月日２Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
019010**
019020*計算月
019030      MOVE 連施－施術和暦    TO 終了和暦２Ｗ.
019040      MOVE 連施－施術年      TO 終了年２Ｗ.
019050      MOVE 連施－施術月      TO 終了月２Ｗ.
019060*前回通院月
019070      MOVE 施記－施術和暦    TO 開始和暦２Ｗ.
019080      MOVE 施記－施術年      TO 開始年２Ｗ.
019090      MOVE 施記－施術月      TO 開始月２Ｗ.
019100*
019110      EVALUATE TRUE
019120       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ = 終了年２Ｗ)
019130            PERFORM  前月比較月
019140       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ NOT = 終了年２Ｗ)
019150            PERFORM  前月比較年
019160       WHEN  開始和暦２Ｗ NOT = 終了和暦２Ｗ 
019170            PERFORM  前月比較元号
019180      END-EVALUATE.
019190*
019200      IF 計算月２Ｗ = 1
019210         MOVE  "YES"  TO  前月フラグ
019220      END-IF.
019230*
019240*================================================================*
019250 前月比較月  SECTION.
019260*
019270     IF  終了月２Ｗ >  開始月２Ｗ
019280         COMPUTE 計算月２Ｗ = 終了月２Ｗ - 開始月２Ｗ
019290     ELSE
019300        MOVE ZERO TO 計算月２Ｗ
019310     END-IF.
019320*
019330*================================================================*
019340 前月比較年  SECTION.
019350*
019360     IF  終了年２Ｗ >  開始年２Ｗ
019370         COMPUTE 計算年２Ｗ = 終了年２Ｗ - 開始年２Ｗ
019380         COMPUTE 計算月２Ｗ = (計算年２Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
019390     ELSE
019400        MOVE ZERO TO 計算月２Ｗ
019410     END-IF.
019420*
019430*================================================================*
019440 前月比較元号  SECTION.
019450*
019460     MOVE 開始和暦２Ｗ TO 元－元号区分.
019470     READ 元号マスタ
019480     NOT INVALID KEY
019490         MOVE 元－開始西暦年 TO 開始西暦年２Ｗ
019500     END-READ.
019510     MOVE 終了和暦２Ｗ TO 元－元号区分.
019520     READ 元号マスタ
019530     NOT INVALID KEY
019540         MOVE 元－開始西暦年 TO 終了西暦年２Ｗ
019550     END-READ.
019560**
019570     IF (開始西暦年２Ｗ NOT = ZERO) AND (終了西暦年２Ｗ NOT = ZERO)
019580        COMPUTE 開始西暦年２Ｗ = 開始西暦年２Ｗ + 開始年２Ｗ - 1
019590        COMPUTE 終了西暦年２Ｗ = 終了西暦年２Ｗ + 終了年２Ｗ - 1
019600*
019610        IF 終了西暦年２Ｗ =  開始西暦年２Ｗ
019620           PERFORM  前月比較月
019630        ELSE
019640           IF  終了西暦年２Ｗ >  開始西暦年２Ｗ
019650               COMPUTE 計算年２Ｗ = 終了西暦年２Ｗ - 開始西暦年２Ｗ
019660               COMPUTE 計算月２Ｗ = (計算年２Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
019670           ELSE
019680               MOVE ZERO TO 計算月２Ｗ
019690           END-IF
019700        END-IF
019710     ELSE
019720        MOVE ZERO TO 計算月２Ｗ
019730     END-IF.
019740*
019750*================================================================*
019760 エラー表示 SECTION.
019770*
019780     DISPLAY NC"ファイル書込エラー：" ファイル名Ｗ              UPON CONS.
019790     DISPLAY NC"状態キー" 状態キー                              UPON CONS.
019800     DISPLAY NC"システム管理者に連絡してください"               UPON CONS.
019810     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
019820     ACCEPT  キー入力 FROM CONS.
019830*================================================================*
019840*================================================================*
019850 エラー表示Ｒ SECTION.
019860*
019870     DISPLAY ファイル名Ｗ NC"ファイル読込エラー"                UPON CONS.
019880     DISPLAY NC"状態キー：" 状態キー                            UPON CONS.
019890     DISPLAY NC"システム管理者に連絡してください"               UPON CONS.
019900     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
019910     ACCEPT  キー入力 FROM CONS.
019920     PERFORM ファイル閉鎖.
019930     EXIT PROGRAM.
019940*================================================================*
019950 月計処理  SECTION.
019960*
019970     MOVE SPACE  TO 連計－キー.
019980     INITIALIZE 連計－キー.
019990*
020000     INITIALIZE 連計算助成月途中－キー.
020010*
020020     MOVE 連Ｙ計算－施術和暦 TO 受－施術和暦
020030     MOVE 連Ｙ計算－施術年   TO 受－施術年
020040     MOVE 連Ｙ計算－施術月   TO 受－施術月
020050     MOVE 連Ｙ計算－患者番号 TO 受－患者番号
020060     MOVE 連Ｙ計算－枝番     TO 受－枝番
020070     READ 受診者情報Ｆ
020080     NOT INVALID KEY
020090         MOVE 受－施術和暦           TO 連計－施術和暦
020100         MOVE 受－施術年             TO 連計－施術年
020110         MOVE 受－施術月             TO 連計－施術月
020120         MOVE 受－本人家族区分       TO 連計－本人家族区分
020130         MOVE 受－保険種別           TO 連計－保険種別
020140         MOVE 受－保険者番号         TO 連計－保険者番号 
020150         MOVE 受－公費種別           TO 連計－公費種別
020160         MOVE 受－費用負担者番号     TO 連計－市町村番号老人
020170         MOVE 受－助成種別           TO 連計－助成種別
020180         MOVE 受－費用負担者番号助成 TO 連計－市町村番号助成
020190         MOVE 受－患者コード         TO 連計－患者コード
020200         MOVE 0                      TO 連計－負担金計算区分
020210     END-READ.
020220*
020230**
020240     IF 受－保険種別 = 85 OR 90 OR 91
020250        MOVE 連計－保険種別 TO 連レ－保険種別
020260     ELSE
020270        IF 連計－助成種別 NOT = ZERO
020280            MOVE 連計－助成種別 TO 連レ－保険種別
020290        ELSE
020300            MOVE 連計－保険種別 TO 連レ－保険種別
020310        END-IF
020320     END-IF.
020330**
020340     EVALUATE 連計－保険種別
020350* 健康保険
020360*     WHEN ZERO
020370* 　　　　／＊　生保単独　＊／
020380*         IF 助成種別Ｗ = 50 AND 公費種別Ｗ = ZERO
020390*             CALL   "HIY0210R"
020400*             CANCEL "HIY0210R"
020410*         ELSE
020420*             PERFORM コンソールエラー確認
020430*             DISPLAY "保険種別が不正です１" 
020440*                     " 受診者№=" 連計－患者コード 
020450*                     " 保険種別=" 連計－保険種別 
020460*                     " 施術年月=" 連計－施術年 連計－施術月 UPON CONS
020470*             PERFORM エラー終了
020480*         END-IF
020490     WHEN 01 THRU 09
020500         EVALUATE TRUE
020510*         WHEN (連Ｙ計算－施術和暦年月 >= 40909) AND (連Ｙ計算－施術和暦年月 <= 41212)
020520*             CALL   "HIY9709R"
020530*             CANCEL "HIY9709R"
020540*         WHEN (連Ｙ計算－施術和暦年月 >= 41301) AND (連Ｙ計算－施術和暦年月 <= 41409)
020550*             CALL   "HIY0101G"
020560*             CANCEL "HIY0101G"
020570         WHEN (連Ｙ計算－施術和暦年月 >= 41410) AND (連Ｙ計算－施術和暦年月 <= 99999)
020580*
020590             IF ( 助成月途中開始日Ｗ NOT = ZERO )
020600                MOVE 助成月途中開始日Ｗ TO 連計算助成月途中－助成月途中開始日
020610             END-IF
020620             CALL   "HIY0210R"
020630             CANCEL "HIY0210R"
020640*
020650*------------------------------------------------------*
020660* 助成の月途中開始ありの時、再度 "HIY0210R" CALL
020670             IF ( 助成月途中開始日Ｗ NOT = ZERO )
020680                MOVE 1 TO 連計算助成月途中－助成のみ処理区分
020690                CALL   "HIY0210R"
020700                CANCEL "HIY0210R"
020710             END-IF
020720*------------------------------------------------------*
020730*
020740         WHEN OTHER
020750             PERFORM コンソールエラー確認
020760             DISPLAY "健保：施術年月不正" 
020770                     " 受診者№="   連計－患者コード
020780                     " 保険種別="   連計－保険種別
020790                     " 施術年月="   連計－施術年 連計－施術月  UPON CONS
020800             PERFORM エラー終了
020810         END-EVALUATE
020820* 労災保険
020830     WHEN 70
020840         CALL   "ROU9709R"
020850         CANCEL "ROU9709R"
020860* 自賠責保険
020870     WHEN 80
020880         CALL   "JIB9709R"
020890         CANCEL "JIB9709R"
020900* 生保単独・保険証忘れ
020910     WHEN 85
020920     WHEN 91
020930         CALL   "HIY0210R"
020940         CANCEL "HIY0210R"
020950* 自費
020960     WHEN 90
020970         IF 受－保険外区分 = 9
020980            CALL   "HIY0210R"
020990            CANCEL "HIY0210R"
021000         ELSE
021010            CALL   "YKJIHI"
021020            CANCEL "YKJIHI"
021030         END-IF
021040     WHEN OTHER
021050*/エラー表示の修正
      */保険証画面からコンソールエラー表示を削除する↓↓↓/120928
021060*             PERFORM コンソールエラー確認
021070*             DISPLAY "保険種別が不正です"
021080*                     " 受診者№="   連計－患者コード
021090*                     " 保険種別="   連計－保険種別
021100*                     " 施術年月="   連計－施術年 連計－施術月 UPON CONS
                   MOVE  "保険種別が不正です" TO 連メ７－メッセージ１
                   MOVE  SPACE                TO 連メ７－メッセージ２
                   CALL   "MSG007"
                   CANCEL "MSG007"
      */保険証画面からコンソールエラー表示を削除する↑↑↑/120928
021110             PERFORM エラー終了
021120     END-EVALUATE.
021130*
021140*================================================================*
021150 エラー終了 SECTION.
021160*
021170     PERFORM ファイル閉鎖.
021180     MOVE ZERO TO PROGRAM-STATUS.
021190     EXIT PROGRAM.
021200*================================================================*
021210 エラー表示その他 SECTION.
021220*
021230     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
021240     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
021250     ACCEPT  キー入力 FROM CONS.
021260     PERFORM ファイル閉鎖.
021270     MOVE ZERO TO PROGRAM-STATUS.
021280     EXIT PROGRAM.
021290*================================================================*
021300 コンソールエラー確認 SECTION.
021310*
021320     IF エラー表示Ｆ = ZERO
021330         MOVE  "エラーが発生したのでコンソール画面の内容" TO 連メ７－メッセージ１
021340         MOVE  "を確認し、修正してください。"             TO 連メ７－メッセージ２
021350         CALL   "MSG007"
021360         CANCEL "MSG007"
021370     END-IF.
021380     MOVE 1 TO エラー表示Ｆ.
021390*================================================================*
021400 労自再検料請求セット SECTION.
021410*
021420******************************************************************
021430*労災の場合
021440*初検の月は１回
021450*翌月と翌々月は２回まで算定可
021460******************************************************************
021470*
021480     MOVE SPACE TO 再検料請求フラグ.
021490     MOVE ZERO  TO 再検料回数Ｗ.
021500*
021510*/初検の月から３ヶ月以内が再検料の対象
021520     PERFORM 初検経過月判定
021530     IF 計算月２Ｗ <= 2
      */初検日に初険料を算定しない場合は再検料を算定しない↓↓↓/141209
                MOVE 負－患者コード     TO 施記－患者コード
022050          MOVE 負－初検和暦年月日 TO 施記－施術和暦年月日
                READ 施術記録Ｆ
                INVALID KEY
                    CONTINUE
                NOT INVALID KEY
                    IF 施記－初検料請求区分 = 1
      */初検日に初険料を算定しない場合は再検料を算定しない↑↑↑/141209
021540*                /再検料の回数を調べる
021550                  MOVE 連施－施術和暦 TO 施記－施術和暦
021560                  MOVE 連施－施術年   TO 施記－施術年
021570                  MOVE 連施－施術月   TO 施記－施術月
021580                  MOVE 連施－施術日   TO 施記－施術日
021590                  MOVE 連施－患者番号 TO 施記－患者番号
021600                  MOVE 連施－枝番     TO 施記－枝番
021610                  START 施術記録Ｆ KEY IS < 施記－患者コード
021620                                            施記－施術和暦年月日
021630                                            REVERSED
021640                  END-START
021650                  IF 状態キー = "00"
021660                      MOVE SPACE TO 終了フラグ２
021670                      PERFORM 施術記録Ｆ読込
021680                      PERFORM UNTIL (終了フラグ２   = "YES") OR
021690                                    (連施－患者番号 NOT = 施記－患者番号) OR
021700                                    (連施－枝番     NOT = 施記－枝番    ) OR
021710                                    (連施－施術和暦 NOT = 施記－施術和暦) OR
021720                                    (連施－施術年   NOT = 施記－施術年  ) OR
021730                                    (連施－施術月   NOT = 施記－施術月  )
021740                         IF 施記－再検料請求 = 1
021750                             COMPUTE 再検料回数Ｗ = 再検料回数Ｗ + 1
021760                         END-IF
021770                         PERFORM 施術記録Ｆ読込
021780                     END-PERFORM
021790*                   /再検料フラグのセット
021800                     IF (連施－施術和暦 = 負－初検和暦) AND
021810                        (連施－施術年月 = 負－初検年月)
021820*                       /初検月の場合再検料は１回まで算定可
021830                         IF 再検料回数Ｗ = ZERO
021840                             MOVE "YES" TO 再検料請求フラグ
021850                         END-IF
021860                     ELSE
021870*                       /初検月でない場合再検料は２回まで算定可
021880                         IF 再検料回数Ｗ < 2
021890                             MOVE "YES" TO 再検料請求フラグ
021900                         END-IF
021910                     END-IF
021920                 END-IF
      */初検日に初険料を算定しない場合は再検料を算定しない↓↓↓/141209
021930             END-IF
               END-READ
      */初検日に初険料を算定しない場合は再検料を算定しない↑↑↑/141209
021930     END-IF.
021940*================================================================*
021950 初検経過月判定  SECTION.
021960* 
021970***当月が初険月から何ヶ月目か判定
021980      INITIALIZE  計算年月日２Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
021990**
022000*/当月
022010      MOVE 連施－施術和暦 TO 終了和暦２Ｗ.
022020      MOVE 連施－施術年   TO 終了年２Ｗ.
022030      MOVE 連施－施術月   TO 終了月２Ｗ.
022040*/初検月
022050      MOVE 負－初検和暦   TO 開始和暦２Ｗ.
022060      MOVE 負－初検年     TO 開始年２Ｗ.
022070      MOVE 負－初検月     TO 開始月２Ｗ.
022080*
022090      EVALUATE TRUE
022100       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ = 終了年２Ｗ)
022110            PERFORM  前月比較月
022120       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ NOT = 終了年２Ｗ)
022130            PERFORM  前月比較年
022140       WHEN  開始和暦２Ｗ NOT = 終了和暦２Ｗ 
022150            PERFORM  前月比較元号
022160      END-EVALUATE.
022170*
022180*      IF 計算月２Ｗ = 1
022190*         MOVE  "YES"  TO  前月フラグ
022200*      END-IF.
022210*
022220*================================================================*
022230 運動療法料判定 SECTION.
022240*
022250*１０日に一回及び１ヶ月に３回を限度に算定可
022260*その場合の１ヶ月は当日から前月の応答日の翌日まで
022270     MOVE ZERO  TO 運動料回数Ｗ.
022280     MOVE ZERO  TO 前回運動日数Ｗ.
022290     MOVE SPACE TO 運動療法フラグ.
022300*/１ヶ月前の日のセット
022310     MOVE 連施－施術和暦 TO 前月和暦Ｗ.
022320     MOVE 連施－施術年   TO 前月年Ｗ.
022330     MOVE 連施－施術月   TO 前月月Ｗ.
022340     MOVE 連施－施術日   TO 前月日Ｗ.
022350     COMPUTE 前月月Ｗ = 前月月Ｗ - 1
022360     IF 前月月Ｗ <= ZERO
022370        MOVE 12 TO 前月月Ｗ
022380        COMPUTE 前月年Ｗ = 前月年Ｗ - 1
022390     END-IF.
022400*
      */正式な和暦年月日をセットする↓↓↓/20190527
066860     INITIALIZE 連元ＣＨＫ－キー.
066870     MOVE 前月和暦年月日Ｗ TO 連元ＣＨＫ－和暦年月日.
066910     MOVE SPACE          TO 連元ＣＨＫ－確認フラグ.
066920*
066930     CALL   "GENGOCHK".
066940     CANCEL "GENGOCHK".
066950*
067110     MOVE 連元ＣＨＫ－正式和暦年月日 TO 前月和暦年月日Ｗ
      */正式な和暦年月日をセットする↑↑↑/20190527
022410**/各和暦の元年の開始月の判定
022420*     IF 前月年Ｗ = 1
022430*         MOVE 前月和暦Ｗ TO 元－元号区分
022440*         READ 元号マスタ
022450*         INVALID KEY
022460*             DISPLAY NC"指定和暦が登録されていません" UPON CONS
022470*             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
022480*             ACCEPT  キー入力 FROM CONS
022490*             PERFORM 終了処理
022500*             EXIT PROGRAM
022510*         END-READ
022520**/各和暦の１年で開始月より前月月の方が小さい場合は前和暦
022530*         IF 前月月Ｗ < 元－開始西暦月
022540*             COMPUTE 前月和暦Ｗ = 前月和暦Ｗ - 1
022550*             MOVE 前月和暦Ｗ TO 元－元号区分
022560*             READ 元号マスタ
022570*             INVALID KEY
022580*                 DISPLAY NC"指定和暦が登録されていません" UPON CONS
022590*                 DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
022600*                 ACCEPT  キー入力 FROM CONS
022610*                 PERFORM 終了処理
022620*                 EXIT PROGRAM
022630*             NOT INVALID KEY 
022640*                 COMPUTE 前月年Ｗ = 元－終了西暦年 - 元－開始西暦年 + 1
022650*             END-READ
022660*         END-IF
022670*     END-IF.
022680     COMPUTE 前月日Ｗ = 前月日Ｗ + 1.
022690     PERFORM 月末日取得.
022700*
022710     IF 前月日Ｗ > 月末日Ｗ
022720         MOVE 1 TO 前月日Ｗ
022730         COMPUTE 前月月Ｗ = 前月月Ｗ + 1
022740         IF 前月月Ｗ > 12
022750             MOVE 1 TO 前月月Ｗ
022760         END-IF
022770         MOVE 前月和暦Ｗ TO 元－元号区分
022780         READ 元号マスタ
022790         INVALID KEY
022800             DISPLAY NC"指定和暦が登録されていません" UPON CONS
022810             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
022820             ACCEPT  キー入力 FROM CONS
022830             PERFORM 終了処理
022840             EXIT PROGRAM
022850         NOT INVALID KEY
022860             COMPUTE 前月西暦年Ｗ = 元－開始西暦年 + 前月年Ｗ - 1
022870         END-READ
022880         IF 前月西暦年Ｗ = 元－終了西暦年
022890             IF 前月月Ｗ > 元－終了西暦月
022900                 COMPUTE 前月和暦Ｗ = 前月和暦Ｗ + 1
022910                 MOVE 1 TO 前月年Ｗ
022920             END-IF
022930         END-IF
022940     END-IF.
022950*/運動療法料の回数を調べる
022960     MOVE 連施－施術和暦 TO 施記－施術和暦.
022970     MOVE 連施－施術年   TO 施記－施術年.
022980     MOVE 連施－施術月   TO 施記－施術月.
022990     MOVE 連施－施術日   TO 施記－施術日.
023000     MOVE 連施－患者番号 TO 施記－患者番号.
023010     MOVE 連施－枝番     TO 施記－枝番.
023020     START 施術記録Ｆ KEY IS < 施記－患者コード
023030                               施記－施術和暦年月日
023040                               REVERSED
023050     END-START.
023060     IF 状態キー = "00"
023070         MOVE SPACE TO 終了フラグ２
023080         PERFORM 施術記録Ｆ読込
023090         PERFORM UNTIL (終了フラグ２   = "YES") OR
023100                       (連施－患者番号 NOT = 施記－患者番号) OR
023110                       (連施－枝番     NOT = 施記－枝番    ) OR
023120                       (前月和暦年月日Ｗ   > 施記－施術和暦年月日)
023130            IF 施記－運動療法区分 = 1
023140                COMPUTE 運動料回数Ｗ = 運動料回数Ｗ + 1
023150*              /前回の運動療法からの経過日数を計算する
023160                IF 前回運動日数Ｗ = ZERO
023170                    MOVE 連施－施術和暦 TO 計算和暦Ｗ
023180                    MOVE 連施－施術年   TO 計算年Ｗ
023190                    MOVE 連施－施術月   TO 計算月Ｗ
023200                    MOVE 連施－施術日   TO 計算日Ｗ
023210                    PERFORM 西暦年取得
023220                    PERFORM 日付絶対値計算
023230                    MOVE 絶対値Ｗ       TO 連施絶対値Ｗ
023240*    
023250                    MOVE 施記－施術和暦 TO 計算和暦Ｗ
023260                    MOVE 施記－施術年   TO 計算年Ｗ
023270                    MOVE 施記－施術月   TO 計算月Ｗ
023280                    MOVE 施記－施術日   TO 計算日Ｗ
023290                    PERFORM 西暦年取得
023300                    PERFORM 日付絶対値計算
023310                    MOVE 絶対値Ｗ       TO 施記絶対値Ｗ
023320                    COMPUTE 前回運動日数Ｗ = 連施絶対値Ｗ - 施記絶対値Ｗ
023330                END-IF
023340            END-IF
023350            PERFORM 施術記録Ｆ読込
023360         END-PERFORM
023370*/平成１４年８月より仕様変更020801
023380         IF 連施－施術和暦年月日 < 4140801
023390             IF (前回運動日数Ｗ = ZERO ) OR
023400                ((運動料回数Ｗ   < 3 ) AND
023410                 (前回運動日数Ｗ >= 10))
023420                 MOVE "YES" TO 運動療法フラグ
023430             END-IF
023440         ELSE
023450*/日整静岡の算定間隔を６日に変更。0304
023460             IF 協会コードＷ = 02
023470                 MOVE 6 TO 運動指導算定間隔Ｗ
023480             ELSE
023490                 MOVE 7 TO 運動指導算定間隔Ｗ
023500             END-IF
023510             IF (前回運動日数Ｗ = ZERO ) OR
023520                ((運動料回数Ｗ   < 5 ) AND
023530                 (前回運動日数Ｗ >= 運動指導算定間隔Ｗ))
023540                 MOVE "YES" TO 運動療法フラグ
023550             END-IF
023560         END-IF
023570     END-IF.
023580*================================================================*
023590 指導管理料判定 SECTION.
023600*７日に一回及び１ヶ月に４回を限度に算定可
023610     MOVE ZERO  TO 指導管理回数Ｗ.
023620     MOVE ZERO  TO 前回指導日数Ｗ.
023630     MOVE SPACE TO 指導管理料フラグ.
023640*/１ヶ月前の日のセット
023650     MOVE 連施－施術和暦 TO 前月和暦Ｗ.
023660     MOVE 連施－施術年   TO 前月年Ｗ.
023670     MOVE 連施－施術月   TO 前月月Ｗ.
023680     MOVE 連施－施術日   TO 前月日Ｗ.
023690     COMPUTE 前月月Ｗ = 前月月Ｗ - 1
023700     IF 前月月Ｗ <= ZERO
023710        MOVE 12 TO 前月月Ｗ
023720        COMPUTE 前月年Ｗ = 前月年Ｗ - 1
023730     END-IF.
      */正式な和暦年月日をセットする↓↓↓/20190527
066860     INITIALIZE 連元ＣＨＫ－キー.
066870     MOVE 前月和暦年月日Ｗ TO 連元ＣＨＫ－和暦年月日.
066910     MOVE SPACE          TO 連元ＣＨＫ－確認フラグ.
066920*
066930     CALL   "GENGOCHK".
066940     CANCEL "GENGOCHK".
066950*
067110     MOVE 連元ＣＨＫ－正式和暦年月日 TO 前月和暦年月日Ｗ
      */正式な和暦年月日をセットする↑↑↑/20190527
023740*
023750**/各和暦の元年の開始月の判定
023760*     IF 前月年Ｗ = 1
023770*         MOVE 前月和暦Ｗ TO 元－元号区分
023780*         READ 元号マスタ
023790*         INVALID KEY
023800*             DISPLAY NC"指定和暦が登録されていません" UPON CONS
023810*             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
023820*             ACCEPT  キー入力 FROM CONS
023830*             PERFORM 終了処理
023840*             EXIT PROGRAM
023850*         END-READ
023860**/各和暦の１年で開始月より前月月の方が小さい場合は前和暦
023870*         IF 前月月Ｗ < 元－開始西暦月
023880*             COMPUTE 前月和暦Ｗ = 前月和暦Ｗ - 1
023890*             MOVE 前月和暦Ｗ TO 元－元号区分
023900*             READ 元号マスタ
023910*             INVALID KEY
023920*                 DISPLAY NC"指定和暦が登録されていません" UPON CONS
023930*                 DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
023940*                 ACCEPT  キー入力 FROM CONS
023950*                 PERFORM 終了処理
023960*                 EXIT PROGRAM
023970*             NOT INVALID KEY 
023980*                 COMPUTE 前月年Ｗ = 元－終了西暦年 - 元－開始西暦年 + 1
023990*             END-READ
024000*         END-IF
024010*     END-IF.
024020     COMPUTE 前月日Ｗ = 前月日Ｗ + 1.
024030     PERFORM 月末日取得.
024040*
024050     IF 前月日Ｗ > 月末日Ｗ
024060         MOVE 1 TO 前月日Ｗ
024070         COMPUTE 前月月Ｗ = 前月月Ｗ + 1
024080         IF 前月月Ｗ > 12
024090             MOVE 1 TO 前月月Ｗ
024100         END-IF
024110         MOVE 前月和暦Ｗ TO 元－元号区分
024120         READ 元号マスタ
024130         INVALID KEY
024140             DISPLAY NC"指定和暦が登録されていません" UPON CONS
024150             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
024160             ACCEPT  キー入力 FROM CONS
024170             PERFORM 終了処理
024180             EXIT PROGRAM
024190         NOT INVALID KEY
024200             COMPUTE 前月西暦年Ｗ = 元－開始西暦年 + 前月年Ｗ - 1
024210         END-READ
024220         IF 前月西暦年Ｗ = 元－終了西暦年
024230             IF 前月月Ｗ > 元－終了西暦月
024240                 COMPUTE 前月和暦Ｗ = 前月和暦Ｗ + 1
024250                 MOVE 1 TO 前月年Ｗ
024260             END-IF
024270         END-IF
024280     END-IF.
024290*   /指導管理の回数を調べる
024300     MOVE 連施－施術和暦 TO 施記－施術和暦.
024310     MOVE 連施－施術年   TO 施記－施術年.
024320     MOVE 連施－施術月   TO 施記－施術月.
024330     MOVE 連施－施術日   TO 施記－施術日.
024340     MOVE 連施－患者番号 TO 施記－患者番号.
024350     MOVE 連施－枝番     TO 施記－枝番.
024360     START 施術記録Ｆ KEY IS < 施記－患者コード
024370                               施記－施術和暦年月日
024380                               REVERSED
024390     END-START.
024400     IF 状態キー = "00"
024410         MOVE SPACE TO 終了フラグ２
024420         PERFORM 施術記録Ｆ読込
024430         PERFORM UNTIL (終了フラグ２   = "YES") OR
024440                       (連施－患者番号 NOT = 施記－患者番号) OR
024450                       (連施－枝番     NOT = 施記－枝番    ) OR
024460                       (前月和暦年月日Ｗ   > 施記－施術和暦年月日)
024470            IF 施記－指導管理区分２ = 1
024480                COMPUTE 指導管理回数Ｗ = 指導管理回数Ｗ + 1
024490*              /前回の指導管理法からの経過日数を計算する
024500                IF 前回指導日数Ｗ = ZERO
024510                    MOVE 連施－施術和暦 TO 計算和暦Ｗ
024520                    MOVE 連施－施術年   TO 計算年Ｗ
024530                    MOVE 連施－施術月   TO 計算月Ｗ
024540                    MOVE 連施－施術日   TO 計算日Ｗ
024550                    PERFORM 西暦年取得
024560                    PERFORM 日付絶対値計算
024570                    MOVE 絶対値Ｗ       TO 連施絶対値Ｗ
024580*    
024590                    MOVE 施記－施術和暦 TO 計算和暦Ｗ
024600                    MOVE 施記－施術年   TO 計算年Ｗ
024610                    MOVE 施記－施術月   TO 計算月Ｗ
024620                    MOVE 施記－施術日   TO 計算日Ｗ
024630                    PERFORM 西暦年取得
024640                    PERFORM 日付絶対値計算
024650                    MOVE 絶対値Ｗ       TO 施記絶対値Ｗ
024660                    COMPUTE 前回指導日数Ｗ = 連施絶対値Ｗ - 施記絶対値Ｗ
024670                END-IF
024680            END-IF
024690            PERFORM 施術記録Ｆ読込
024700         END-PERFORM
024710*/平成１４年８月より仕様変更020801
024720         IF 連施－施術和暦年月日 < 4140801
024730             IF (前回指導日数Ｗ = ZERO) OR
024740                ((指導管理回数Ｗ  < 4 ) AND
024750                 (前回指導日数Ｗ >= 7))
024760                 MOVE "YES" TO 指導管理料フラグ
024770             END-IF
024780         ELSE
024790*/日整静岡の算定間隔を６日に変更。0304
024800             IF 協会コードＷ = 02
024810                 MOVE 6 TO 運動指導算定間隔Ｗ
024820             ELSE
024830                 MOVE 7 TO 運動指導算定間隔Ｗ
024840             END-IF
024850             IF (前回指導日数Ｗ = ZERO) OR
024860                ((指導管理回数Ｗ  < 5 ) AND
024870*0304                 (前回指導日数Ｗ >= 7))
024880                 (前回指導日数Ｗ >= 運動指導算定間隔Ｗ))
024890                 MOVE "YES" TO 指導管理料フラグ
024900             END-IF
024910         END-IF
024920     END-IF.
024930*================================================================*
024940* 包帯交換料判定 SECTION.
024950**
024960*     MOVE SPACE TO 包帯交換フラグ.
024970*     MOVE ZERO  TO 初検絶対値Ｗ.
024980*     MOVE ZERO  TO 連施絶対値Ｗ.
024990*     MOVE ZERO  TO 初検経過日数Ｗ.
025000*     MOVE ZERO  TO 施術回数Ｗ.
025010**
025020*     IF (負－初検和暦 NOT = ZERO) AND
025030*        (負－初検年   NOT = ZERO) AND
025040*        (負－初検月   NOT = ZERO) AND
025050*        (負－初検日   NOT = ZERO)
025060**/初検の次の施術日は包帯交換料を算定
025070*         MOVE 連施－施術和暦 TO 施記－施術和暦
025080*         MOVE 連施－施術年   TO 施記－施術年
025090*         MOVE 連施－施術月   TO 施記－施術月
025100*         MOVE 連施－施術日   TO 施記－施術日
025110*         MOVE 連施－患者番号 TO 施記－患者番号
025120*         MOVE 連施－枝番     TO 施記－枝番
025130*         START 施術記録Ｆ KEY IS < 施記－患者コード
025140*                                   施記－施術和暦年月日
025150*                                   REVERSED
025160*         END-START
025170**
025180*         IF 状態キー = "00"
025190*             READ 施術記録Ｆ NEXT
025200*             NOT AT END
025210** 診療区分：２＝初検日（初検料算定の日）
025220*                 IF ( 連施－患者コード = 施記－患者コード ) AND
025230*                    ( 施記－診療区分 = 2                  )
025240*                     MOVE "YES" TO 包帯交換フラグ
025250*                 END-IF
025260*             END-READ
025270*         END-IF
025280*         IF 包帯交換フラグ = SPACE
025290**           /初検日から何日目か計算
025300*             MOVE 負－初検和暦  TO 計算和暦Ｗ
025310*             MOVE 負－初検年    TO 計算年Ｗ
025320*             MOVE 負－初検月    TO 計算月Ｗ
025330*             MOVE 負－初検日    TO 計算日Ｗ
025340*             PERFORM 西暦年取得
025350*             PERFORM 日付絶対値計算
025360*             MOVE 絶対値Ｗ       TO 初検絶対値Ｗ
025370**
025380*             MOVE 連施－施術和暦 TO 計算和暦Ｗ
025390*             MOVE 連施－施術年   TO 計算年Ｗ
025400*             MOVE 連施－施術月   TO 計算月Ｗ
025410*             MOVE 連施－施術日   TO 計算日Ｗ
025420*             PERFORM 西暦年取得
025430*             PERFORM 日付絶対値計算
025440*             MOVE 絶対値Ｗ       TO 連施絶対値Ｗ
025450**
025460*             COMPUTE 初検経過日数Ｗ = 連施絶対値Ｗ - 初検絶対値Ｗ
025470*             EVALUATE 初検経過日数Ｗ
025480*             WHEN ZERO THRU  6
025490*                 MOVE  ZERO TO 対象日Ｗ
025500*                 MOVE  2    TO 対象回数Ｗ
025510*             WHEN  7   THRU 13
025520*                 MOVE  7    TO 対象日Ｗ
025530*                 MOVE  1    TO 対象回数Ｗ
025540*             WHEN 14   THRU 20
025550*                 MOVE  14   TO 対象日Ｗ
025560*                 MOVE  1    TO 対象回数Ｗ
025570*             WHEN 21   THRU 27
025580*                 MOVE  21   TO 対象日Ｗ
025590*                 MOVE  1    TO 対象回数Ｗ
025600*             WHEN OTHER
025610*                 MOVE  28   TO 対象日Ｗ
025620*                 MOVE  1    TO 対象回数Ｗ
025630*             END-EVALUATE
025640**       /対象期間内の施術回数を調べる
025650*             MOVE 連施－施術和暦 TO 施記－施術和暦
025660*             MOVE 連施－施術年   TO 施記－施術年
025670*             MOVE 連施－施術月   TO 施記－施術月
025680*             MOVE 連施－施術日   TO 施記－施術日
025690*             MOVE 連施－患者番号 TO 施記－患者番号
025700*             MOVE 連施－枝番     TO 施記－枝番
025710*             START 施術記録Ｆ KEY IS < 施記－患者コード
025720*                                       施記－施術和暦年月日
025730*                                       REVERSED
025740*             END-START
025750*             IF 状態キー = "00"
025760*                 MOVE SPACE TO 終了フラグ２
025770*                 PERFORM 施術記録Ｆ読込
025780*                 PERFORM UNTIL (終了フラグ２   = "YES") OR
025790*                               (連施－患者番号 NOT = 施記－患者番号) OR
025800*                               (連施－枝番     NOT = 施記－枝番    ) OR
025810*                               (初検経過日数Ｗ     < 対象日Ｗ      ) OR
025820*                               (対象回数Ｗ         < 施術回数Ｗ    ) OR
025830**/                            */初検日より前は考慮しない。
025840*                               (負－初検和暦年月日 > 施記－施術和暦年月日)
025850**            /初検日はカウントしない
025860*                     IF 負－初検和暦年月日 NOT = 施記－施術和暦年月日
025870**                       /施術日が初検日から何日目か計算
025880*                         MOVE 施記－施術和暦 TO 計算和暦Ｗ
025890*                         MOVE 施記－施術年   TO 計算年Ｗ
025900*                         MOVE 施記－施術月   TO 計算月Ｗ
025910*                         MOVE 施記－施術日   TO 計算日Ｗ
025920*                         PERFORM 西暦年取得
025930*                         PERFORM 日付絶対値計算
025940*                         MOVE 絶対値Ｗ       TO 施記絶対値Ｗ
025950**    
025960*                         COMPUTE 初検経過日数Ｗ = 施記絶対値Ｗ - 初検絶対値Ｗ
025970*                         IF 初検経過日数Ｗ >= 対象日Ｗ
025980*                             COMPUTE 施術回数Ｗ = 施術回数Ｗ + 1
025990*                         END-IF
026000*                     END-IF
026010*                     PERFORM 施術記録Ｆ読込
026020*                 END-PERFORM
026030*                 IF 対象回数Ｗ > 施術回数Ｗ
026040*                     MOVE "YES" TO 包帯交換フラグ
026050*                 END-IF
026060*             END-IF
026070*         END-IF
026080*     END-IF.
026090*================================================================*
026100*================================================================*
026110 労災算定情報取得 SECTION.
026120*
026130* 制御区分：０２　労災保険
026140*
026150     MOVE 2              TO 計－制御区分.
026160     MOVE 連施－施術和暦 TO 計－開始和暦 施術和暦ＣＷ.
026170     MOVE 連施－施術年   TO 計－開始年   施術年ＣＷ.
026180     MOVE 連施－施術月   TO 計－開始月   施術月ＣＷ.
026190*
026200     START 計算マスタ KEY IS <= 計－制御区分 計－開始和暦年月
026210                                                     REVERSED
026220     END-START.
026230*
026240     IF 状態キー = "00"
026250*
026260         READ 計算マスタ NEXT
026270         AT END
026280             PERFORM コンソールエラー確認
026290             DISPLAY NC"施術年月に対応した算定情報がみつかりません３" UPON CONS
026300             PERFORM エラー終了
026310         NOT AT END
026320             IF ( 施術和暦年月ＣＷ >= 計Ｂ－開始和暦年月 ) AND
026330                ( 施術和暦年月ＣＷ <= 計Ｂ－終了和暦年月 )
026340*
026350                 MOVE 計Ｂ－冷罨日数骨折 TO 冷罨日数骨折Ｗ
026360                 MOVE 計Ｂ－冷罨日数脱臼 TO 冷罨日数脱臼Ｗ
026370                 MOVE 計Ｂ－冷罨日数打撲 TO 冷罨日数打撲Ｗ
026380                 MOVE 計Ｂ－温罨待機骨折 TO 温罨待機骨折Ｗ
026390                 MOVE 計Ｂ－温罨待機脱臼 TO 温罨待機脱臼Ｗ
026400             ELSE
026410                 PERFORM コンソールエラー確認
026420                 DISPLAY NC"施術年月に対応した算定情報がみつかりません４" UPON CONS
026430                 PERFORM エラー終了
026440             END-IF
026450         END-READ
026460     END-IF.
026470*
026480*================================================================*
026490*================================================================*
026500 労自罨法算定処理 SECTION.
026510*
026520*/負－電療請求区分=ZEROの時に算定する
026530*/負－算定区分＝１の時算定する  ０：通常　１：初回処置料請求なし
026540*/部位の開始日には算定しない  ←－－－－－－－－－間違い
026550*/負傷種別が７OR８の時は開始日でも算定する
026560*/骨折、不全骨折、脱臼については初険日には算定出来ない。0108仕様もれ
026570     IF (負－電療請求区分  (部位ＣＮＴ)     = ZERO )
026580         IF (負－負傷種別(部位ＣＮＴ) = 04 OR 05 OR 06) AND
026590            ( 施記－診療区分          = 2       )       AND
026600*/骨折不全骨折の場合の電療料は初検日に算定不可だが、初回処置が無い場合は算定可/0410
026610            ( 施記－整復施療区分(部位ＣＮＴ) = 1)
      */29年4月より社団静岡は労災の電療料は骨折不全骨折脱臼でも初検日に算定出来る↓↓↓/170408
                   IF (協会コードＷ = 02) AND (施術和暦年月ＣＷ >= 42904)
026640                 MOVE 1    TO 施記－電療区分(部位ＣＮＴ)
026630             ELSE
      */29年4月より社団静岡は労災の電療料は骨折不全骨折脱臼でも初検日に算定出来る↑↑↑/170408
026620                 MOVE ZERO TO 施記－電療区分(部位ＣＮＴ)
026650             END-IF                                                            *>/170408
      */30年1月より労災自賠責の電療料は骨折不全骨折脱臼でも初検日に算定出来る↓↓↓/171101
                   IF 施術和暦年月ＣＷ >= 43001
026640                 MOVE 1    TO 施記－電療区分(部位ＣＮＴ)
026630             ELSE
026620                 MOVE ZERO TO 施記－電療区分(部位ＣＮＴ)
026650             END-IF
      */30年1月より労災自賠責の電療料は骨折不全骨折脱臼でも初検日に算定出来る↑↑↑/171101
026630         ELSE
026640             MOVE 1    TO 施記－電療区分(部位ＣＮＴ)
026650         END-IF
026660     ELSE
026670         MOVE ZERO TO 施記－電療区分(部位ＣＮＴ)
026680     END-IF.
026690*
026700     MOVE 負－負傷和暦(部位ＣＮＴ) TO 計算和暦Ｗ.
026710     MOVE 負－負傷年(部位ＣＮＴ)   TO 計算年Ｗ.
026720     MOVE 負－負傷月(部位ＣＮＴ)   TO 計算月Ｗ.
026730     MOVE 負－負傷日(部位ＣＮＴ)   TO 計算日Ｗ.
026740     PERFORM 西暦年取得.
026750     PERFORM 日付絶対値計算.
026760     MOVE 絶対値Ｗ                 TO 負傷絶対値Ｗ.
026770*
026780     MOVE 連施－施術和暦           TO 計算和暦Ｗ.
026790     MOVE 連施－施術年             TO 計算年Ｗ.
026800     MOVE 連施－施術月             TO 計算月Ｗ.
026810     MOVE 連施－施術日             TO 計算日Ｗ.
026820     PERFORM 西暦年取得.
026830     PERFORM 日付絶対値計算.
026840     MOVE 絶対値Ｗ                 TO 施術絶対値Ｗ.
026850*
026860     COMPUTE 負傷経過日Ｗ = 施術絶対値Ｗ - ( 負傷絶対値Ｗ - 1 ).
026870*
026880     EVALUATE 負－負傷種別(部位ＣＮＴ)
026890* 不全骨折・骨折（医師拘縮も含む）
026900     WHEN 05
026910     WHEN 06
026920     WHEN 07
026930     WHEN 08
026940         IF 負傷経過日Ｗ <= 冷罨日数骨折Ｗ
026950             MOVE 1 TO 施記－罨法区分(部位ＣＮＴ)
026960         END-IF
026970         IF 負傷経過日Ｗ > 温罨待機骨折Ｗ
026980*          ／＊ 労災の場合は開始日でも温罨法料は算定出来る
026990             IF ( 負－負傷種別(部位ＣＮＴ)     = 07 ) OR
027000                ( 負－負傷種別(部位ＣＮＴ)     = 08 )
027010                 MOVE ZERO TO 施記－罨法区分(部位ＣＮＴ)
027020             ELSE
027030* 　　　　　　　　／＊　後療から行う場合は算定ＯＫ　＊／
027040                 MOVE 2 TO 施記－罨法区分(部位ＣＮＴ)
027050             END-IF
027060         END-IF
027070* 脱臼
027080     WHEN 04
027090         IF 負傷経過日Ｗ <= 冷罨日数脱臼Ｗ
027100             MOVE 1 TO 施記－罨法区分(部位ＣＮＴ)
027110         END-IF
027120         IF 負傷経過日Ｗ > 温罨待機脱臼Ｗ
027130*          ／＊ 労災の場合は開始日でも温罨法料は算定出来る
027140             IF ( 負－負傷種別(部位ＣＮＴ)  = 07 ) OR
027150                ( 負－負傷種別(部位ＣＮＴ)  = 08 )
027160                 MOVE ZERO TO 施記－罨法区分(部位ＣＮＴ)
027170             ELSE
027180* 　　　　　　　　／＊　後療から行う場合は算定ＯＫ　＊／
027190                 MOVE 2 TO 施記－罨法区分(部位ＣＮＴ)
027200             END-IF
027210         END-IF
027220* 打撲・捻挫・挫傷
027230     WHEN 01
027240     WHEN 02
027250     WHEN 03
027260* 打撲・捻挫・挫傷は受傷日から５日以内は算定可
027270         IF 負傷経過日Ｗ <= 冷罨日数打撲Ｗ
027280             MOVE 1 TO 施記－罨法区分(部位ＣＮＴ)
027290         END-IF
027300         IF 負傷経過日Ｗ > 温罨待機脱臼Ｗ
027310*          ／＊ 労災の場合は開始日でも温罨法料は算定出来る
027320             IF ( 負－負傷種別(部位ＣＮＴ) = 07 ) OR
027330                ( 負－負傷種別(部位ＣＮＴ) = 08 )
027340                 MOVE ZERO TO 施記－罨法区分(部位ＣＮＴ)
027350             ELSE
027360* 　　　　　　　　／＊　後療から行う場合は算定ＯＫ　＊／
027370                 MOVE 2 TO 施記－罨法区分(部位ＣＮＴ)
027380             END-IF
027390         END-IF
027400     WHEN OTHER
027410         CONTINUE
027420     END-EVALUATE.
027430*
      *初期値の追加(冷温電)/20230726
           IF 施記－罨法区分(部位ＣＮＴ) = 1 AND 冷罨法区分Ｗ = 1
               MOVE ZERO TO 施記－罨法区分(部位ＣＮＴ)
           END-IF.
           IF 施記－罨法区分(部位ＣＮＴ) = 2 AND 温罨法区分Ｗ = 1
               MOVE ZERO TO 施記－罨法区分(部位ＣＮＴ)
           END-IF.
           IF  施記－電療区分(部位ＣＮＴ) = 1 AND 電療料区分Ｗ = 1
               MOVE ZERO TO 施記－電療区分(部位ＣＮＴ)
           END-IF.
027430*
027440*================================================================*
027450 月末日取得 SECTION.
027460*
027470     MOVE 前月和暦Ｗ TO 元－元号区分.
027480     READ 元号マスタ
027490     NOT INVALID KEY
027500         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
027510     END-READ.
027520     IF 施術西暦年Ｗ NOT = ZERO
027530        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 前月年Ｗ - 1
027540     END-IF.
027550*
027560     EVALUATE 前月月Ｗ
027570     WHEN 4
027580     WHEN 6
027590     WHEN 9
027600     WHEN 11
027610         MOVE 30 TO 月末日Ｗ
027620     WHEN 2
027630         DIVIDE 4 INTO 施術西暦年Ｗ GIVING    商Ｗ
027640                                    REMAINDER 余Ｗ
027650         END-DIVIDE
027660         IF 余Ｗ = ZERO
027670             MOVE 29 TO 月末日Ｗ
027680         ELSE
027690             MOVE 28 TO 月末日Ｗ
027700         END-IF
027710     WHEN 1
027720     WHEN 3
027730     WHEN 5
027740     WHEN 7
027750     WHEN 8
027760     WHEN 10
027770     WHEN 12
027780         MOVE 31 TO 月末日Ｗ
027790     WHEN OTHER
027800          CONTINUE
027810     END-EVALUATE.
027820*================================================================*
027830 自賠責運動療法料判定 SECTION.
027840*
027850*１０日に一回及び１ヶ月に３回を限度に算定可
027860*その場合の１ヶ月は当日から前月の応答日の翌日まで
027870     MOVE ZERO  TO 運動料回数Ｗ.
027880     MOVE ZERO  TO 前回運動日数Ｗ.
027890     MOVE SPACE TO 運動療法フラグ.
027900*/１ヶ月前の日のセット
027910     MOVE 連施－施術和暦 TO 前月和暦Ｗ.
027920     MOVE 連施－施術年   TO 前月年Ｗ.
027930     MOVE 連施－施術月   TO 前月月Ｗ.
027940     MOVE 連施－施術日   TO 前月日Ｗ.
027950     COMPUTE 前月月Ｗ = 前月月Ｗ - 1
027960     IF 前月月Ｗ <= ZERO
027970        MOVE 12 TO 前月月Ｗ
027980        COMPUTE 前月年Ｗ = 前月年Ｗ - 1
027990     END-IF.
028000*
      */正式な和暦年月日をセットする↓↓↓/20190527
066860     INITIALIZE 連元ＣＨＫ－キー.
066870     MOVE 前月和暦年月日Ｗ TO 連元ＣＨＫ－和暦年月日.
066910     MOVE SPACE          TO 連元ＣＨＫ－確認フラグ.
066920*
066930     CALL   "GENGOCHK".
066940     CANCEL "GENGOCHK".
066950*
067110     MOVE 連元ＣＨＫ－正式和暦年月日 TO 前月和暦年月日Ｗ
      */正式な和暦年月日をセットする↑↑↑/20190527
028010**/各和暦の元年の開始月の判定
028020*     IF 前月年Ｗ = 1
028030*         MOVE 前月和暦Ｗ TO 元－元号区分
028040*         READ 元号マスタ
028050*         INVALID KEY
028060*             DISPLAY NC"指定和暦が登録されていません" UPON CONS
028070*             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
028080*                                                      UPON CONS
028090*             ACCEPT  キー入力 FROM CONS
028100*             PERFORM 終了処理
028110*             EXIT PROGRAM
028120**         NOT INVALID KEY
028130**             COMPUTE 前月西暦年Ｗ = 元－開始西暦年 + 前月年Ｗ - 1
028140*         END-READ
028150**/各和暦の１年で開始月より前月月の方が小さい場合は前和暦
028160*         IF 前月月Ｗ < 元－開始西暦月
028170*             COMPUTE 前月和暦Ｗ = 前月和暦Ｗ - 1
028180*             MOVE 前月和暦Ｗ TO 元－元号区分
028190*             READ 元号マスタ
028200*             INVALID KEY
028210*                 DISPLAY NC"指定和暦が登録されていません" UPON CONS
028220*                 DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
028230*                                                          UPON CONS
028240*                 ACCEPT  キー入力 FROM CONS
028250*                 PERFORM 終了処理
028260*                 EXIT PROGRAM
028270*             NOT INVALID KEY 
028280*                 COMPUTE 前月年Ｗ = 元－終了西暦年 - 元－開始西暦年 + 1
028290*             END-READ
028300*         END-IF
028310*     END-IF.
028320     COMPUTE 前月日Ｗ = 前月日Ｗ + 1.
028330     PERFORM 月末日取得.
028340*
028350     IF 前月日Ｗ > 月末日Ｗ
028360         MOVE 1 TO 前月日Ｗ
028370         COMPUTE 前月月Ｗ = 前月月Ｗ + 1
028380         IF 前月月Ｗ > 12
028390             MOVE 1 TO 前月月Ｗ
028400         END-IF
028410         MOVE 前月和暦Ｗ TO 元－元号区分
028420         READ 元号マスタ
028430         INVALID KEY
028440             DISPLAY NC"指定和暦が登録されていません" UPON CONS
028450             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
028460                                                      UPON CONS
028470             ACCEPT  キー入力 FROM CONS
028480             PERFORM 終了処理
028490             EXIT PROGRAM
028500         NOT INVALID KEY
028510             COMPUTE 前月西暦年Ｗ = 元－開始西暦年 + 前月年Ｗ - 1
028520         END-READ
028530         IF 前月西暦年Ｗ = 元－終了西暦年
028540             IF 前月月Ｗ > 元－終了西暦月
028550                 COMPUTE 前月和暦Ｗ = 前月和暦Ｗ + 1
028560                 MOVE 1 TO 前月年Ｗ
028570             END-IF
028580         END-IF
028590     END-IF.
028600*/月基準区分が１なら前月年月日を当月の１日にセットし直す/0505
028610*月基準区分Ｗ：ゼロ＝前月の応答日の翌日から当日まで、１＝暦通り
028620     IF 月基準区分Ｗ = ZERO OR SPACE
028630         CONTINUE
028640     ELSE
028650         MOVE 連施－施術和暦 TO 前月和暦Ｗ
028660         MOVE 連施－施術年   TO 前月年Ｗ
028670         MOVE 連施－施術月   TO 前月月Ｗ
028680         MOVE 1              TO 前月日Ｗ
028690     END-IF.
028700*/運動療法料の回数を調べる
028710     MOVE 連施－施術和暦 TO 施記－施術和暦.
028720     MOVE 連施－施術年   TO 施記－施術年.
028730     MOVE 連施－施術月   TO 施記－施術月.
028740     MOVE 連施－施術日   TO 施記－施術日.
028750     MOVE 連施－患者番号 TO 施記－患者番号.
028760     MOVE 連施－枝番     TO 施記－枝番.
028770     START 施術記録Ｆ KEY IS < 施記－患者コード
028780                               施記－施術和暦年月日
028790                               REVERSED
028800     END-START.
028810     IF 状態キー = "00"
028820         MOVE SPACE TO 終了フラグ２
028830         PERFORM 施術記録Ｆ読込
028840         PERFORM UNTIL (終了フラグ２   = "YES") OR
028850                       (連施－患者番号 NOT = 施記－患者番号) OR
028860                       (連施－枝番     NOT = 施記－枝番    ) OR
028870                       (前月和暦年月日Ｗ   > 施記－施術和暦年月日)
028880            IF 施記－運動療法区分 = 1
028890                COMPUTE 運動料回数Ｗ = 運動料回数Ｗ + 1
028900*              /前回の運動療法からの経過日数を計算する
028910                IF 前回運動日数Ｗ = ZERO
028920                    MOVE 連施－施術和暦 TO 計算和暦Ｗ
028930                    MOVE 連施－施術年   TO 計算年Ｗ
028940                    MOVE 連施－施術月   TO 計算月Ｗ
028950                    MOVE 連施－施術日   TO 計算日Ｗ
028960                    PERFORM 西暦年取得
028970                    PERFORM 日付絶対値計算
028980                    MOVE 絶対値Ｗ       TO 連施絶対値Ｗ
028990*    
029000                    MOVE 施記－施術和暦 TO 計算和暦Ｗ
029010                    MOVE 施記－施術年   TO 計算年Ｗ
029020                    MOVE 施記－施術月   TO 計算月Ｗ
029030                    MOVE 施記－施術日   TO 計算日Ｗ
029040                    PERFORM 西暦年取得
029050                    PERFORM 日付絶対値計算
029060                    MOVE 絶対値Ｗ       TO 施記絶対値Ｗ
029070                    COMPUTE 前回運動日数Ｗ = 連施絶対値Ｗ - 施記絶対値Ｗ
029080                END-IF
029090            END-IF
029100            PERFORM 施術記録Ｆ読込
029110         END-PERFORM
029120*
029130*/指導運動、算定間隔と月の上限回数を任意に設定する/0505
029140         IF (運動算定間隔Ｗ NOT = ZERO AND SPACE) OR
029150            (運動上限回数Ｗ NOT = ZERO AND SPACE)
029160*/前回日数が０または設定日数以下、かつ月の上限回数に満たない場合に算定可。
029170*/(前回日数０は99日以内に算定していない事を示す)
029180             IF ((前回運動日数Ｗ =  ZERO) OR 
029190                 (前回運動日数Ｗ >= 運動算定間隔Ｗ)) AND
029200                 (運動料回数Ｗ   <  運動上限回数Ｗ)
029210                 MOVE "YES" TO 運動療法フラグ
029220             END-IF
029230         ELSE
029240*/平成１５年４月よりＪＢの算定間隔を７日、最大月５回に変更。0304
029250             IF (連施－施術和暦年月日 < 4150401) OR
029260                (協会コードＷ NOT = 08)
029270                 IF (前回運動日数Ｗ = ZERO ) OR
029280                    ((運動料回数Ｗ   < 3 ) AND
029290                     (前回運動日数Ｗ >= 10))
029300                     MOVE "YES" TO 運動療法フラグ
029310                 END-IF
029320*/平成１５年４月よりＪＢの算定間隔を７日、最大月５回に変更。0304
029330             ELSE
029340                 IF (前回運動日数Ｗ = ZERO) OR
029350                    ((運動料回数Ｗ   <  5 ) AND
029360                     (前回運動日数Ｗ >= 7))
029370                     MOVE "YES" TO 運動療法フラグ
029380                 END-IF
029390             END-IF
029400         END-IF
029410     END-IF.
029420*================================================================*
029430 自賠責指導管理料判定 SECTION.
029440*
029450*７日に一回及び１ヶ月に４回を限度に算定可
029460*その場合の１ヶ月は当日から前月の応答日の翌日まで
029470     MOVE ZERO  TO 指導管理回数Ｗ.
029480     MOVE ZERO  TO 前回指導日数Ｗ.
029490     MOVE SPACE TO 指導管理料フラグ.
029500*/１ヶ月前の日のセット
029510     MOVE 連施－施術和暦 TO 前月和暦Ｗ.
029520     MOVE 連施－施術年   TO 前月年Ｗ.
029530     MOVE 連施－施術月   TO 前月月Ｗ.
029540     MOVE 連施－施術日   TO 前月日Ｗ.
029550     COMPUTE 前月月Ｗ = 前月月Ｗ - 1
029560     IF 前月月Ｗ <= ZERO
029570        MOVE 12 TO 前月月Ｗ
029580        COMPUTE 前月年Ｗ = 前月年Ｗ - 1
029590     END-IF.
029600*
      */正式な和暦年月日をセットする↓↓↓/20190527
066860     INITIALIZE 連元ＣＨＫ－キー.
066870     MOVE 前月和暦年月日Ｗ TO 連元ＣＨＫ－和暦年月日.
066910     MOVE SPACE          TO 連元ＣＨＫ－確認フラグ.
066920*
066930     CALL   "GENGOCHK".
066940     CANCEL "GENGOCHK".
066950*
067110     MOVE 連元ＣＨＫ－正式和暦年月日 TO 前月和暦年月日Ｗ
      */正式な和暦年月日をセットする↑↑↑/20190527
029610**/各和暦の元年の開始月の判定
029620*     IF 前月年Ｗ = 1
029630*         MOVE 前月和暦Ｗ TO 元－元号区分
029640*         READ 元号マスタ
029650*         INVALID KEY
029660*             DISPLAY NC"指定和暦が登録されていません" UPON CONS
029670*             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
029680*             ACCEPT  キー入力 FROM CONS
029690*             PERFORM 終了処理
029700*             EXIT PROGRAM
029710*         END-READ
029720**/各和暦の１年で開始月より前月月の方が小さい場合は前和暦
029730*         IF 前月月Ｗ < 元－開始西暦月
029740*             COMPUTE 前月和暦Ｗ = 前月和暦Ｗ - 1
029750*             MOVE 前月和暦Ｗ TO 元－元号区分
029760*             READ 元号マスタ
029770*             INVALID KEY
029780*                 DISPLAY NC"指定和暦が登録されていません" UPON CONS
029790*                 DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
029800*                 ACCEPT  キー入力 FROM CONS
029810*                 PERFORM 終了処理
029820*                 EXIT PROGRAM
029830*             NOT INVALID KEY 
029840*                 COMPUTE 前月年Ｗ = 元－終了西暦年 - 元－開始西暦年 + 1
029850*             END-READ
029860*         END-IF
029870*     END-IF.
029880     COMPUTE 前月日Ｗ = 前月日Ｗ + 1.
029890     PERFORM 月末日取得.
029900*
029910     IF 前月日Ｗ > 月末日Ｗ
029920         MOVE 1 TO 前月日Ｗ
029930         COMPUTE 前月月Ｗ = 前月月Ｗ + 1
029940         IF 前月月Ｗ > 12
029950             MOVE 1 TO 前月月Ｗ
029960         END-IF
029970         MOVE 前月和暦Ｗ TO 元－元号区分
029980         READ 元号マスタ
029990         INVALID KEY
030000             DISPLAY NC"指定和暦が登録されていません" UPON CONS
030010             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
030020             ACCEPT  キー入力 FROM CONS
030030             PERFORM 終了処理
030040             EXIT PROGRAM
030050         NOT INVALID KEY
030060             COMPUTE 前月西暦年Ｗ = 元－開始西暦年 + 前月年Ｗ - 1
030070         END-READ
030080         IF 前月西暦年Ｗ = 元－終了西暦年
030090             IF 前月月Ｗ > 元－終了西暦月
030100                 COMPUTE 前月和暦Ｗ = 前月和暦Ｗ + 1
030110                 MOVE 1 TO 前月年Ｗ
030120             END-IF
030130         END-IF
030140     END-IF.
030150*月基準区分Ｗ：ゼロ＝前月の応答日の翌日から当日まで、１＝暦通り
030160     IF 月基準区分Ｗ = ZERO OR SPACE
030170         CONTINUE
030180     ELSE
030190         MOVE 連施－施術和暦 TO 前月和暦Ｗ
030200         MOVE 連施－施術年   TO 前月年Ｗ
030210         MOVE 連施－施術月   TO 前月月Ｗ
030220         MOVE 1              TO 前月日Ｗ
030230     END-IF.
030240*   /指導管理の回数を調べる
030250     MOVE 連施－施術和暦 TO 施記－施術和暦.
030260     MOVE 連施－施術年   TO 施記－施術年.
030270     MOVE 連施－施術月   TO 施記－施術月.
030280     MOVE 連施－施術日   TO 施記－施術日.
030290     MOVE 連施－患者番号 TO 施記－患者番号.
030300     MOVE 連施－枝番     TO 施記－枝番.
030310     START 施術記録Ｆ KEY IS < 施記－患者コード
030320                               施記－施術和暦年月日
030330                               REVERSED
030340     END-START.
030350     IF 状態キー = "00"
030360         MOVE SPACE TO 終了フラグ２
030370         PERFORM 施術記録Ｆ読込
030380         PERFORM UNTIL (終了フラグ２   = "YES") OR
030390                       (連施－患者番号 NOT = 施記－患者番号) OR
030400                       (連施－枝番     NOT = 施記－枝番    ) OR
030410                       (前月和暦年月日Ｗ   > 施記－施術和暦年月日)
030420            IF 施記－指導管理区分２ = 1
030430                COMPUTE 指導管理回数Ｗ = 指導管理回数Ｗ + 1
030440*          /前回の指導管理法からの経過日数を計算する
030450                IF 前回指導日数Ｗ = ZERO
030460                    MOVE 連施－施術和暦 TO 計算和暦Ｗ
030470                    MOVE 連施－施術年   TO 計算年Ｗ
030480                    MOVE 連施－施術月   TO 計算月Ｗ
030490                    MOVE 連施－施術日   TO 計算日Ｗ
030500                    PERFORM 西暦年取得
030510                    PERFORM 日付絶対値計算
030520                    MOVE 絶対値Ｗ       TO 連施絶対値Ｗ
030530*
030540                    MOVE 施記－施術和暦 TO 計算和暦Ｗ
030550                    MOVE 施記－施術年   TO 計算年Ｗ
030560                    MOVE 施記－施術月   TO 計算月Ｗ
030570                    MOVE 施記－施術日   TO 計算日Ｗ
030580                    PERFORM 西暦年取得
030590                    PERFORM 日付絶対値計算
030600                    MOVE 絶対値Ｗ       TO 施記絶対値Ｗ
030610                    COMPUTE 前回指導日数Ｗ = 連施絶対値Ｗ - 施記絶対値Ｗ
030620                END-IF
030630            END-IF
030640            PERFORM 施術記録Ｆ読込
030650         END-PERFORM
030660*/指導運動、算定間隔と月の上限回数を任意に設定する/0505
030670         IF (指導算定間隔Ｗ NOT = ZERO AND SPACE) OR
030680            (指導上限回数Ｗ NOT = ZERO AND SPACE)
030690*/前回日数が０または設定日数以下、かつ月の上限回数に満たない場合に算定可。
030700*/(前回日数０は99日以内に算定していない事を示す)
030710             IF ((前回指導日数Ｗ =  ZERO) OR 
030720                 (前回指導日数Ｗ >= 指導算定間隔Ｗ)) AND
030730                 (指導管理回数Ｗ <  指導上限回数Ｗ)
030740                 MOVE "YES" TO 指導管理料フラグ
030750             END-IF
030760         ELSE
030770*/平成１５年４月よりＪＢの算定間隔を７日、最大月５回に変更。0304
030780             IF (連施－施術和暦年月日 < 4150401) OR
030790                (協会コードＷ NOT = 08)
030800                 IF (前回指導日数Ｗ = ZERO) OR
030810                    ((指導管理回数Ｗ  < 4 ) AND
030820                     (前回指導日数Ｗ >= 7))
030830                     MOVE "YES" TO 指導管理料フラグ
030840                 END-IF
030850             ELSE
030860*/平成１５年４月よりＪＢの算定間隔を７日、最大月５回に変更。0304
030870                 IF (前回指導日数Ｗ = ZERO) OR
030880                    ((指導管理回数Ｗ  < 5 ) AND
030890                     (前回指導日数Ｗ >= 7))
030900                     MOVE "YES" TO 指導管理料フラグ
030910                 END-IF
030920             END-IF
030930         END-IF
030940     END-IF.
030950*================================================================*
030960*================================================================*
030970 労災自賠責用フラグ更新 SECTION.
030980*
030990     MOVE ZERO  TO 施術証明区分Ｗ. 
031000     MOVE ZERO  TO 施術明細区分Ｗ.
031010*
031020     IF 保険種別Ｗ = 70 OR 80
031030*-----------------------------------------------------------------------------------*
031040*  施術証明・施術明細 ( 施術記録Ｆの当月の施術証明区分・施術明細区分を合計して、
031050*  当月最初の日の施術証明区分・施術明細区分にセットする。
031060*  それ以外の日は、ゼロクリアー)
031070*-----------------------------------------------------------------------------------*
031080*      / 施術証明区分・施術明細区分を合計しながらゼロクリアーする /
031090            MOVE 連Ｙ計算－患者番号  TO 施記－患者番号
031100            MOVE 連Ｙ計算－枝番      TO 施記－枝番
031110            MOVE 連Ｙ計算－施術和暦  TO 施記－施術和暦
031120            MOVE 連Ｙ計算－施術年    TO 施記－施術年
031130            MOVE 連Ｙ計算－施術月    TO 施記－施術月
031140            MOVE ZERO                TO 施記－施術日
031150            START 施術記録Ｆ   KEY IS >  施記－患者コード
031160                                         施記－施術和暦年月日
031170            END-START
031180            IF 状態キー = "00"
031190                   MOVE SPACE TO 終了フラグ２
031200                   PERFORM 施術記録Ｆ読込
031210                   PERFORM UNTIL ( 施記－施術和暦   NOT = 連Ｙ計算－施術和暦  ) OR
031220                                 ( 施記－施術年     NOT = 連Ｙ計算－施術年    ) OR
031230                                 ( 施記－施術月     NOT = 連Ｙ計算－施術月    ) OR
031240                                 ( 施記－患者番号   NOT = 連Ｙ計算－患者番号  ) OR
031250                                 ( 施記－枝番       NOT = 連Ｙ計算－枝番      ) OR
031260                                 ( 終了フラグ２     NOT = SPACE )
031270*
031280                        COMPUTE 施術証明区分Ｗ = 施術証明区分Ｗ +  施記－施術証明区分
031290                        COMPUTE 施術明細区分Ｗ = 施術明細区分Ｗ +  施記－施術明細区分
031300                        MOVE ZERO  TO 施記－施術証明区分
031310                        MOVE ZERO  TO 施記－施術明細区分
031320*
031330                        REWRITE 施記－レコード
031340                        INVALID KEY
031350                               MOVE NC"施記" TO ファイル名Ｗ
031360                               PERFORM エラー表示
031370                        END-REWRITE
031380*
031390                        PERFORM 施術記録Ｆ読込
031400*
031410                   END-PERFORM
031420            END-IF
031430*
031440*  施術証明・施術明細 ( 施術記録Ｆの当月最初の日の施術証明区分・施術明細区分にセット
031450
031460            MOVE 連Ｙ計算－患者番号  TO 施記－患者番号
031470            MOVE 連Ｙ計算－枝番      TO 施記－枝番
031480            MOVE 連Ｙ計算－施術和暦  TO 施記－施術和暦
031490            MOVE 連Ｙ計算－施術年    TO 施記－施術年
031500            MOVE 連Ｙ計算－施術月    TO 施記－施術月
031510            MOVE ZERO                TO 施記－施術日
031520            START 施術記録Ｆ   KEY IS >  施記－患者コード
031530                                         施記－施術和暦年月日
031540            END-START
031550            IF 状態キー = "00"
031560                   MOVE SPACE TO 終了フラグ２
031570                   PERFORM 施術記録Ｆ読込
031580                   IF  ( 終了フラグ２        = SPACE      ) AND
031590                       ( 施記－患者番号      = 連Ｙ計算－患者番号   ) AND
031600                       ( 施記－枝番          = 連Ｙ計算－枝番       ) AND
031610                       ( 施記－施術和暦      = 連Ｙ計算－施術和暦   ) AND
031620                       ( 施記－施術年        = 連Ｙ計算－施術年     ) AND
031630                       ( 施記－施術月        = 連Ｙ計算－施術月     ) 
031640*
031650*                      / 自賠責の時、合計した証明区分・明細区分が０だったら、１にする（負－労災自賠施術証明無区分考慮） /
031660                         IF 保険種別Ｗ = 80
031670*
031680                            MOVE 連Ｙ計算－施術和暦 TO 負－施術和暦
031690                            MOVE 連Ｙ計算－施術年   TO 負－施術年
031700                            MOVE 連Ｙ計算－施術月   TO 負－施術月
031710                            MOVE 連Ｙ計算－患者番号 TO 負－患者番号
031720                            MOVE 連Ｙ計算－枝番     TO 負－枝番
031730                            READ 負傷データＦ
031740                            NOT INVALID KEY
031750                               IF 施術証明区分Ｗ = ZERO
031760                                  IF ( 負－労災自賠施術証明無区分 NOT = "0" )
031770                                       MOVE 1 TO 施術証明区分Ｗ
031780                                  END-IF
031790                               END-IF
031800                               IF 施術明細区分Ｗ = ZERO
031810                                  IF ( 負－労災自賠施術明細無区分 NOT = "0" )
031820                                      MOVE 1 TO 施術明細区分Ｗ
031830                                  END-IF
031840                               END-IF
031850                            END-READ
031860*
031870                         END-IF
031880*
031890                         MOVE 施術証明区分Ｗ  TO 施記－施術証明区分
031900                         MOVE 施術明細区分Ｗ  TO 施記－施術明細区分
031910*
031920                         REWRITE 施記－レコード
031930                         INVALID KEY
031940                             MOVE NC"施記" TO ファイル名Ｗ
031950                             PERFORM エラー表示
031960                         END-REWRITE
031970*
031980                   END-IF
031990            END-IF
032000*
032010      END-IF.
032020*
032030*================================================================*
032040 自賠責算定情報取得 SECTION.
032050*
032060* 制御区分：０３　自賠責保険
032070*
032080     MOVE 3              TO 自計－制御区分.
032090     MOVE 連施－施術和暦 TO 自計－開始和暦 施術和暦ＣＷ.
032100     MOVE 連施－施術年   TO 自計－開始年   施術年ＣＷ.
032110     MOVE 連施－施術月   TO 自計－開始月   施術月ＣＷ.
032120*
032130     START 自賠責計算マスタ KEY IS <= 自計－制御区分 自計－開始和暦年月
032140                                                     REVERSED
032150     END-START.
032160*
032170     IF 状態キー = "00"
032180*
032190         READ 自賠責計算マスタ NEXT
032200         AT END
032210             DISPLAY NC"施術年月に対応した算定情報がみつかりません５" UPON CONS
032220             PERFORM 終了処理
032230             EXIT PROGRAM
032240         NOT AT END
032250             IF ( 施術和暦年月ＣＷ >= 自計Ｃ－開始和暦年月 ) AND
032260                ( 施術和暦年月ＣＷ <= 自計Ｃ－終了和暦年月 )
032270*
032280                 MOVE 自計Ｃ－冷罨日数骨折 TO 冷罨日数骨折Ｗ
032290                 MOVE 自計Ｃ－冷罨日数脱臼 TO 冷罨日数脱臼Ｗ
032300                 MOVE 自計Ｃ－冷罨日数打撲 TO 冷罨日数打撲Ｗ
032310                 MOVE 自計Ｃ－温罨待機骨折 TO 温罨待機骨折Ｗ
032320                 MOVE 自計Ｃ－温罨待機脱臼 TO 温罨待機脱臼Ｗ
032330             ELSE
032340                 DISPLAY NC"施術年月に対応した算定情報がみつかりません６" UPON CONS
032350                 PERFORM 終了処理
032360                 EXIT PROGRAM
032370             END-IF
032380         END-READ
032390     END-IF.
032400*
032410*================================================================*
032420 自賠責基本料金取得 SECTION.
032430*
032440*自賠責基本料金用の区分コードは０５
032450     MOVE 05             TO 料－区分コード.
032460     MOVE ZEROS          TO 料－負傷種別.
032470     MOVE ZEROS          TO 料－部位.
032480     MOVE ZEROS          TO 料－左右区分.
032490     MOVE ZEROS          TO 料－負傷位置番号.
032500     MOVE 連施－施術和暦 TO 料－開始和暦 施術和暦ＣＷ.
032510     MOVE 連施－施術年   TO 料－開始年   施術年ＣＷ.
032520     MOVE 連施－施術月   TO 料－開始月   施術月ＣＷ.
032530*
032540     START 料金マスタ KEY IS <= 料－区分コード
032550                                料－部位コード
032560                                料－開始和暦年月
032570                                REVERSED
032580     END-START.
032590*
032600     IF 状態キー = "00"
032610         READ 料金マスタ NEXT
032620         AT END
032630*/エラー表示の修正
032640*             PERFORM コンソールエラー確認
032650             DISPLAY "施術年月に対応した基本料金がみつかりません７"
032660                     " 受診者№="   連施－患者コード
032670                     " 施術年月="   連施－施術年 連施－施術月 UPON CONS
032680             PERFORM 終了処理
032690             MOVE ZERO TO PROGRAM-STATUS
032700             EXIT PROGRAM
032710         NOT AT END
032720*/自賠責の料金か判定
032730             IF 料－区分コード NOT = 05
032740*/エラー表示の修正
032750*                 PERFORM コンソールエラー確認
032760                 DISPLAY "自賠責基本料金がみつかりません"
032770                         " 受診者№="   連施－患者コード
032780                         " 施術年月="   連施－施術年 連施－施術月 UPON CONS
032790                 PERFORM 終了処理
032800                 MOVE ZERO TO PROGRAM-STATUS
032810                 EXIT PROGRAM
032820             ELSE
032830*
032840                 IF ( 施術和暦年月ＣＷ >= 料Ｅ－開始和暦年月 ) AND
032850                    ( 施術和暦年月ＣＷ <= 料Ｅ－終了和暦年月 )
032860                     IF 料Ｅ－運動算定間隔 = ZERO OR SPACE
032870                         MOVE ZERO                TO 運動算定間隔Ｗ
032880                     ELSE
032890                         MOVE 料Ｅ－運動算定間隔  TO 運動算定間隔Ｗ
032900                     END-IF
032910                     IF 料Ｅ－運動上限回数 = ZERO OR SPACE
032920                         MOVE ZERO                TO 運動上限回数Ｗ
032930                     ELSE
032940                         MOVE 料Ｅ－運動上限回数  TO 運動上限回数Ｗ
032950                     END-IF
032960                     IF 料Ｅ－指導算定間隔 = ZERO OR SPACE
032970                         MOVE ZERO                TO 指導算定間隔Ｗ
032980                     ELSE
032990                         MOVE 料Ｅ－指導算定間隔  TO 指導算定間隔Ｗ
033000                     END-IF
033010                     IF 料Ｅ－指導上限回数 = ZERO OR SPACE
033020                         MOVE ZERO                TO 指導上限回数Ｗ
033030                     ELSE
033040                         MOVE 料Ｅ－指導上限回数  TO 指導上限回数Ｗ
033050                     END-IF
033060                     IF 料Ｅ－月基準区分 = ZERO OR SPACE
033070                         MOVE ZERO                TO 月基準区分Ｗ
033080                     ELSE
033090                         MOVE 料Ｅ－月基準区分    TO 月基準区分Ｗ
033100                     END-IF
033110                 ELSE
033120*/エラー表示の修正
033130*                     PERFORM コンソールエラー確認
033140                     DISPLAY "施術年月に対応した料金がみつかりません８"
033150                             " 受診者№="   連施－患者コード
033160                             " 施術年月="   連施－施術年 連施－施術月 UPON CONS
033170                     PERFORM 終了処理
033180                     MOVE ZERO TO PROGRAM-STATUS
033190                     EXIT PROGRAM
033200                 END-IF
033210             END-IF
033220         END-READ
033230     END-IF.
033240*
033250*/受診者固有単価
033260     IF 自賠－固有単価使用区分 = 1
033270         MOVE 自賠－運動算定間隔  TO 運動算定間隔Ｗ
033280         MOVE 自賠－運動上限回数  TO 運動上限回数Ｗ
033290         MOVE 自賠－指導算定間隔  TO 指導算定間隔Ｗ
033300         MOVE 自賠－指導上限回数  TO 指導上限回数Ｗ
033310         MOVE 自賠－月基準区分    TO 月基準区分Ｗ
033320     END-IF.
033330*================================================================*
033340 自賠責情報取得 SECTION.
033350*
033360     MOVE 連Ｙ計算－患者コード   TO 自賠－患者コード.
033370     MOVE 連Ｙ計算－施術和暦年月 TO 自賠－施術和暦年月.
033380     READ 自賠責情報Ｆ
033390     INVALID KEY
033400         INITIALIZE 自賠－レコード
033410*         PERFORM コンソールエラー確認
033420*         DISPLAY "指定の受診者情報がありません"
033430*                 " 受診者№="   連計－患者コード
033440*                 " 施術年月="   連計－施術年 連計－施術月 UPON CONS
033450**-----------------------------------------*
033460*         CALL "actcshm"  WITH C LINKAGE
033470**-----------------------------------------*
033480*         ACCEPT  キー入力 FROM CONS
033490*         PERFORM ファイル閉鎖
033500*         MOVE ZERO TO PROGRAM-STATUS
033510*         EXIT PROGRAM
033520     END-READ.
033530*================================================================*
033540 包帯交換料判定 SECTION.
033550*
033560*/部位毎に判定する
033570     MOVE SPACE TO 包帯交換フラグ.
033580     MOVE ZERO  TO 初検絶対値Ｗ.
033590     MOVE ZERO  TO 連施絶対値Ｗ.
033600     MOVE ZERO  TO 初検経過日数Ｗ.
033610     MOVE ZERO  TO 施術回数Ｗ.
033620*
033630     IF (負－初検和暦 NOT = ZERO) AND
033640        (負－初検年   NOT = ZERO) AND
033650        (負－初検月   NOT = ZERO) AND
033660        (負－初検日   NOT = ZERO)
033670         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 負－部位数
033680             MOVE ZERO  TO 初検絶対値Ｗ
033690             MOVE ZERO  TO 連施絶対値Ｗ
033700             MOVE ZERO  TO 初検経過日数Ｗ
033710             MOVE ZERO  TO 施術回数Ｗ
033720*/初検の次の施術日は包帯交換料を算定
033730             MOVE 連施－施術和暦 TO 施記－施術和暦
033740             MOVE 連施－施術年   TO 施記－施術年
033750             MOVE 連施－施術月   TO 施記－施術月
033760             MOVE 連施－施術日   TO 施記－施術日
033770             MOVE 連施－患者番号 TO 施記－患者番号
033780             MOVE 連施－枝番     TO 施記－枝番
033790             START 施術記録Ｆ KEY IS < 施記－患者コード
033800                                       施記－施術和暦年月日
033810                                       REVERSED
033820             END-START
033830*
033840             IF 状態キー = "00"
033850                 READ 施術記録Ｆ NEXT
033860                 NOT AT END
033870*    /部位の開始日
033880                     IF ( 連施－患者コード    = 施記－患者コード              ) AND
033890                        (施記－施術和暦年月日 = 負－開始和暦年月日(部位ＣＮＴ))
033900                         MOVE "YES" TO 部位包帯交換フラグ(部位ＣＮＴ)
033910                     END-IF
033920                 END-READ
033930             END-IF
033940             IF 部位包帯交換フラグ(部位ＣＮＴ) = SPACE
033950*               /開始日から何日目か計算
033960                 MOVE 負－開始和暦(部位ＣＮＴ)  TO 計算和暦Ｗ
033970                 MOVE 負－開始年  (部位ＣＮＴ)  TO 計算年Ｗ
033980                 MOVE 負－開始月  (部位ＣＮＴ)  TO 計算月Ｗ
033990                 MOVE 負－開始日  (部位ＣＮＴ)  TO 計算日Ｗ
034000                 PERFORM 西暦年取得
034010                 PERFORM 日付絶対値計算
034020                 MOVE 絶対値Ｗ       TO 初検絶対値Ｗ
034030*
034040                 MOVE 連施－施術和暦 TO 計算和暦Ｗ
034050                 MOVE 連施－施術年   TO 計算年Ｗ
034060                 MOVE 連施－施術月   TO 計算月Ｗ
034070                 MOVE 連施－施術日   TO 計算日Ｗ
034080                 PERFORM 西暦年取得
034090                 PERFORM 日付絶対値計算
034100                 MOVE 絶対値Ｗ       TO 連施絶対値Ｗ
034110*
034120                 COMPUTE 初検経過日数Ｗ = 連施絶対値Ｗ - 初検絶対値Ｗ
034130                 EVALUATE 初検経過日数Ｗ
034140                 WHEN ZERO 
034150                     MOVE  ZERO TO 対象日Ｗ
034160                     MOVE  ZERO TO 対象回数Ｗ
034170                 WHEN 1    THRU  6
034180                     MOVE  ZERO TO 対象日Ｗ
034190                     MOVE  2    TO 対象回数Ｗ
034200                 WHEN  7   THRU 13
034210                     MOVE  7    TO 対象日Ｗ
034220                     MOVE  1    TO 対象回数Ｗ
034230                 WHEN 14   THRU 20
034240                     MOVE  14   TO 対象日Ｗ
034250                     MOVE  1    TO 対象回数Ｗ
034260                 WHEN 21   THRU 27
034270                     MOVE  21   TO 対象日Ｗ
034280                     MOVE  1    TO 対象回数Ｗ
034290                 WHEN OTHER
034300                     MOVE  28   TO 対象日Ｗ
034310                     MOVE  1    TO 対象回数Ｗ
034320                 END-EVALUATE
034330*           /対象期間内の算定回数を調べる
034340                 MOVE 連施－施術和暦 TO 施記－施術和暦
034350                 MOVE 連施－施術年   TO 施記－施術年
034360                 MOVE 連施－施術月   TO 施記－施術月
034370                 MOVE 連施－施術日   TO 施記－施術日
034380                 MOVE 連施－患者番号 TO 施記－患者番号
034390                 MOVE 連施－枝番     TO 施記－枝番
034400                 START 施術記録Ｆ KEY IS < 施記－患者コード
034410                                           施記－施術和暦年月日
034420                                           REVERSED
034430                 END-START
034440                 IF 状態キー = "00"
034450                     MOVE SPACE TO 終了フラグ２
034460                     PERFORM 施術記録Ｆ読込
034470                     PERFORM UNTIL (終了フラグ２   = "YES") OR
034480                                   (連施－患者番号 NOT = 施記－患者番号) OR
034490                                   (連施－枝番     NOT = 施記－枝番    ) OR
034500                                   (初検経過日数Ｗ     < 対象日Ｗ      ) OR
034510                                   (対象回数Ｗ         < 施術回数Ｗ    ) OR
034520*/                            */開始日より前は考慮しない。
034530                                   (負－開始和暦年月日(部位ＣＮＴ) > 施記－施術和暦年月日) 
034540*                /開始日はカウントしない
034550                         IF 負－開始和暦年月日(部位ＣＮＴ) NOT = 施記－施術和暦年月日
034560*                           /施術日が初検日から何日目か計算
034570                             MOVE 施記－施術和暦 TO 計算和暦Ｗ
034580                             MOVE 施記－施術年   TO 計算年Ｗ
034590                             MOVE 施記－施術月   TO 計算月Ｗ
034600                             MOVE 施記－施術日   TO 計算日Ｗ
034610                             PERFORM 西暦年取得
034620                             PERFORM 日付絶対値計算
034630                             MOVE 絶対値Ｗ       TO 施記絶対値Ｗ
034640*
034650                             COMPUTE 初検経過日数Ｗ = 施記絶対値Ｗ - 初検絶対値Ｗ
034660                             IF 初検経過日数Ｗ >= 対象日Ｗ
034670                                 IF 施記－包帯交換区分(部位ＣＮＴ) = 1
034680                                     COMPUTE 施術回数Ｗ = 施術回数Ｗ + 1
034690                                 END-IF
034700                             END-IF
034710                         END-IF
034720                         PERFORM 施術記録Ｆ読込
034730                     END-PERFORM
034740                     IF 対象回数Ｗ > 施術回数Ｗ
034750                         MOVE "YES" TO 部位包帯交換フラグ(部位ＣＮＴ)
034760                     END-IF
034770                 END-IF
034780             END-IF
034790         END-PERFORM
034800     END-IF.
034810*================================================================*
026740 制御情報取得 SECTION.
026750*
      */20170406
017300     MOVE ZERO TO 協会コードＷ.
017310     MOVE ZERO TO 制－制御区分.
017320     READ 制御情報マスタ
017330     NOT INVALID KEY
017340         MOVE 制－協会コード       TO 協会コードＷ
017350     END-READ.
      *
026760     MOVE 01 TO 制０１－制御区分.
026770     READ 制御情報マスタ
026780     NOT INVALID KEY
               MOVE 制０１－特別材料区分     TO 特別材料区分Ｗ
               MOVE 制０１－明細発行加算区分 TO 明細発行加算区分Ｗ
      *初期値の追加(冷温電)/20230615
               MOVE 制０１－冷罨法区分       TO 冷罨法区分Ｗ
               MOVE 制０１－温罨法区分       TO 温罨法区分Ｗ
               MOVE 制０１－電療料区分       TO 電療料区分Ｗ
      */明細書発行加算。過去年月のフラグを変更しない/20240725
035910         MOVE 制０１－明細変更和暦年月 TO 明細変更和暦年月Ｗ
026800     END-READ.
026810*
026820**================================================================*
013730* 旧明細書加算算定 SECTION.
      **
HILO  **     DISPLAY "明細書加算算定 " 連施－施術和暦年月日
030770**/令和４年１０月より明細書発行加算を算定
030780*     IF 連施－施術和暦年月日 >= 5041001
033730*         MOVE 連施－施術和暦 TO 確認施記－施術和暦
033740*         MOVE 連施－施術年   TO 確認施記－施術年
033750*         MOVE 連施－施術月   TO 確認施記－施術月
033760*         MOVE 連施－施術日   TO 確認施記－施術日
033770*         MOVE 連施－患者番号 TO 確認施記－患者番号
033780*         MOVE 連施－枝番     TO 確認施記－枝番
033790*         START 確認施術記録Ｆ KEY IS < 確認施記－患者コード
033800*                                       確認施記－施術和暦年月日
033810*                                       REVERSED
033820*         END-START
033830**
033840*         IF 状態キー = "00"
033850*             READ 確認施術記録Ｆ NEXT
033860*             AT END
033900*                 MOVE 1 TO 施記－明細書発行加算区分
033860*             NOT AT END
033880*                 IF (連施－患者コード NOT = 確認施記－患者コード) OR
033890*                    (連施－施術和暦   NOT = 確認施記－施術和暦  ) OR
033890*                    (連施－施術年     NOT = 確認施記－施術年    ) OR
033890*                    (連施－施術月     NOT = 確認施記－施術月    )
033900*                     MOVE 1    TO 施記－明細書発行加算区分
      *                 ELSE
033900*                     MOVE ZERO TO 施記－明細書発行加算区分
033910*                 END-IF
033920*             END-READ
033930*         END-IF
HILO  **/毎日算定する場合
033900**         MOVE 1    TO 施記－明細書発行加算区分
      *     ELSE
033900*         MOVE ZERO TO 施記－明細書発行加算区分
033930*     END-IF.
026820*================================================================*
013730 明細書加算算定 SECTION.
      *
HILO  *     DISPLAY "yk-1 明細書加算算定 " 施記－施術日
      */枝番を含めて当月の最初の日に算定する
030770*/令和４年６月より明細書発行加算を算定
033900     MOVE 1 TO 施記－明細書発行加算区分.
030780     IF 連施－施術和暦年月日 >= 5041001
017390         MOVE 連施－施術和暦 TO 受－施術和暦
017400         MOVE 連施－施術年   TO 受－施術年
017410         MOVE 連施－施術月   TO 受－施術月
017420         MOVE 連施－患者番号 TO 受－患者番号
017440         MOVE HIGH-VALUE     TO 受－枝番
017480* 
017490         START 受診者情報Ｆ KEY IS < 受－施術和暦年月 受－患者コード REVERSED
017500         END-START
017510         IF 状態キー = "00"
                   MOVE SPACE TO 終了フラグ４ 終了フラグ５
                   PERFORM 受診者情報Ｆ読込
                   PERFORM UNTIL (連施－施術和暦 NOT = 受－施術和暦) OR
                                 (連施－施術年月 NOT = 受－施術年月) OR
                                 (連施－患者番号 NOT = 受－患者番号) OR
                                 (終了フラグ４   NOT = SPACE       ) OR
                                 (終了フラグ５   NOT = SPACE       )
      */枝番ありの判定から、自費x・生保h・自賠責a・労災rを除く↓↓↓/160706
                       IF 受－枝番 NOT = "x" AND "h" AND "a" AND "r"
033730                     MOVE 連施－施術和暦 TO 確認施記－施術和暦
033740                     MOVE 連施－施術年   TO 確認施記－施術年
033750                     MOVE 連施－施術月   TO 確認施記－施術月
033760                     MOVE 連施－施術日   TO 確認施記－施術日
033770                     MOVE 受－患者番号   TO 確認施記－患者番号
033780                     MOVE 受－枝番       TO 確認施記－枝番
033790                     START 確認施術記録Ｆ KEY IS < 確認施記－患者コード
033800                                                   確認施記－施術和暦年月日
033810                                                   REVERSED
033820                     END-START
033830*
033840                     IF 状態キー = "00"
033850                         READ 確認施術記録Ｆ NEXT
033860                         AT END
                                   CONTINUE
033900*                             MOVE 1 TO 施記－明細書発行加算区分
033860                         NOT AT END
033880                             IF (連施－患者番号 = 確認施記－患者番号) AND
033890                                (連施－施術和暦 = 確認施記－施術和暦) AND
033890                                (連施－施術年   = 確認施記－施術年  ) AND
033890                                (連施－施術月   = 確認施記－施術月  )
      *条件不十分の為条件追加/20221108
                                  AND (連施－施術日   > 確認施記－施術日  ) *>20221108
HILO  *                                 DISPLAY "yk-6 CLR " 施記－患者コード " " 施記－施術和暦年月日 "'" 確認施記－患者コード " " 確認施記－施術和暦年月日
033900                                 MOVE ZERO TO 施記－明細書発行加算区分
018910                                 MOVE "YES" TO 終了フラグ５
033910                             END-IF
033920                         END-READ
033930                     END-IF
                       END-IF
                       PERFORM 受診者情報Ｆ読込
                   END-PERFORM
               END-IF
           END-IF
HILO  *     DISPLAY "yk-7 " 施記－明細書発行加算区分 "'"
026820*================================================================*
018870* 確認施術記録Ｆ読込 SECTION.
018880**
018890*     READ 確認施術記録Ｆ NEXT
018900*     AT END
018910*         MOVE "YES" TO 終了フラグ５
HILO  *         DISPLAY "Yk-5 " 確認施記－患者コード " " 確認施記－施術日
HILO  *     NOT AT END DISPLAY "Yk-2 " 確認施記－患者コード " " 確認施記－施術日
018920*     END-READ.
026820*================================================================*
034830******************************************************************
034840 END PROGRAM YKEISAN.
034850******************************************************************
