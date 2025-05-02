000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCK6621.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*         中京用
000100*　　　　　カルテ印刷２（柔+ｳｨﾝﾄﾞｳｽﾞ版）
000110*         MED =  YCK6621P
000120* ※ 中央と同じ
000130*----------------------------------------------------------------*
000140 DATE-WRITTEN.           2012-11-28
000150 DATE-COMPILED.          2012-11-28
000160*----------------------------------------------------------------*
000170******************************************************************
000180*            ENVIRONMENT         DIVISION                        *
000190******************************************************************
000200 ENVIRONMENT             DIVISION.
000210 CONFIGURATION           SECTION.
000220 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000230 OBJECT-COMPUTER.        FMV-DESKPOWER.
000240 SPECIAL-NAMES.          CONSOLE  IS  CONS
000250                         SYSERR   IS  MSGBOX.
000260 INPUT-OUTPUT            SECTION.
000270 FILE-CONTROL.
000280     SELECT  保険者マスタ    ASSIGN      TO        HOKENSL
000290                             ORGANIZATION             IS  INDEXED
000300                             ACCESS MODE              IS  DYNAMIC
000310                             RECORD KEY               IS  保－保険種別
000320                                                          保－保険者番号
000330* 将来は、キー項目の保険者名称を保険者カナにする
000340                             ALTERNATE RECORD KEY     IS  保－保険種別
000350                                                          保－保険者名称
000360                                                          保－保険者番号
000370                             FILE STATUS              IS  状態キー
000380                             LOCK        MODE         IS  AUTOMATIC.
000390     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000400                             ORGANIZATION             IS  INDEXED
000410                             ACCESS MODE              IS  DYNAMIC
000420                             RECORD KEY               IS  元－元号区分
000430                             FILE STATUS              IS  状態キー
000440                             LOCK        MODE         IS  AUTOMATIC.
000450     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000460                             ORGANIZATION             IS  INDEXED
000470                             ACCESS MODE              IS  DYNAMIC
000480                             RECORD KEY               IS  名－区分コード
000490                                                          名－名称コード
000500                             FILE STATUS              IS  状態キー
000510                             LOCK        MODE         IS  AUTOMATIC.
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
000580     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000590                             ORGANIZATION             IS  INDEXED
000600                             ACCESS MODE              IS  DYNAMIC
000610                             RECORD KEY               IS  制－制御区分
000620                             FILE STATUS              IS  状態キー
000630                             LOCK        MODE         IS  AUTOMATIC.
000700     SELECT  請求先マスタ    ASSIGN      TO        SEIKYUSL
000710                             ORGANIZATION             IS  INDEXED
000720                             ACCESS MODE              IS  DYNAMIC
000730                             RECORD KEY               IS  請先－保険種別
000740                                                          請先－保険者番号
000750                             FILE STATUS              IS  状態キー
000760                             LOCK    MODE             IS  AUTOMATIC.
000840     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000850                             ORGANIZATION             IS  INDEXED
000860                             ACCESS MODE              IS  DYNAMIC
000870                             RECORD KEY               IS  受－施術和暦年月
000880                                                          受－患者コード
000890                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000900                                                          受－患者カナ
000910                                                          受－患者コード
000920                             ALTERNATE RECORD KEY     IS  受－患者コード
000930                                                          受－施術和暦年月
000940                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000950                                                          受－保険種別
000960                                                          受－保険者番号
000970                                                          受－患者コード
000980                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000990                                                          受－公費種別
001000                                                          受－費用負担者番号
001010                                                          受－患者コード
001020                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001030                                                          受－助成種別
001040                                                          受－費用負担者番号助成
001050                                                          受－患者コード
001060                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
001070                                                          受－施術和暦年月
001080                                                          受－患者コード
001090                             FILE STATUS              IS  状態キー
001100                             LOCK        MODE         IS  AUTOMATIC.
001110     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
001120                             ORGANIZATION             IS  INDEXED
001130                             ACCESS MODE              IS  DYNAMIC
001140                             RECORD KEY               IS  施記－施術和暦年月日
001150                                                          施記－患者コード
001160                             ALTERNATE RECORD KEY     IS  施記－患者コード
001170                                                          施記－施術和暦年月日
001180                             FILE STATUS              IS  状態キー
001190                             LOCK        MODE         IS  AUTOMATIC.
001200     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
001210                             ORGANIZATION             IS  INDEXED
001220                             ACCESS MODE              IS  DYNAMIC
001230                             RECORD KEY               IS 負－施術和暦年月
001240                                                         負－患者コード
001250                             ALTERNATE RECORD KEY     IS 負－患者コード
001260                                                         負－施術和暦年月
001270                             FILE STATUS              IS  状態キー
001280                             LOCK        MODE         IS  AUTOMATIC.
001360     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
001370                             ORGANIZATION             IS  INDEXED
001380                             ACCESS MODE              IS  DYNAMIC
001390                             RECORD KEY               IS  負原－区分コード
001400                                                          負原－負傷原因コード
001410                             FILE STATUS              IS  状態キー
001420                             LOCK        MODE         IS  AUTOMATIC.
001430     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
001440                             ORGANIZATION             IS  INDEXED
001450                             ACCESS MODE              IS  DYNAMIC
001460                             RECORD KEY               IS  市－公費種別
001470                                                          市－市町村番号
001480                             ALTERNATE RECORD KEY     IS  市－公費種別
001490                                                          市－市町村名称
001500                                                          市－市町村番号
001510                             FILE STATUS              IS  状態キー
001520                             LOCK        MODE         IS  AUTOMATIC.
           SELECT  事業所マスタ    ASSIGN      TO        JIGYOSL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  事－保険種別
                                                                事－保険者番号
                                                                事－記号
                                   FILE STATUS              IS  状態キー
                                   LOCK        MODE         IS  AUTOMATIC.
000241     SELECT  生保情報Ｆ      ASSIGN      TO        SEIHOJL
000242                             ORGANIZATION             IS INDEXED
000243                             ACCESS MODE              IS DYNAMIC
000244                             RECORD KEY               IS 生保－施術和暦年月
000245                                                         生保－患者コード
000255                             ALTERNATE RECORD KEY     IS 生保－患者コード
000265                                                         生保－施術和暦年月
000277                             FILE STATUS              IS 状態キー
000278                             LOCK        MODE         IS AUTOMATIC.
000241     SELECT  労災情報Ｆ      ASSIGN      TO        ROUSAIJL
000242                             ORGANIZATION             IS INDEXED
000243                             ACCESS MODE              IS DYNAMIC
000244                             RECORD KEY               IS 労災－施術和暦年月
000245                                                         労災－患者コード
000255                             ALTERNATE RECORD KEY     IS 労災－患者コード
000265                                                         労災－施術和暦年月
000277                             FILE STATUS              IS 状態キー
000278                             LOCK        MODE         IS AUTOMATIC.
000241     SELECT  自賠責情報Ｆ    ASSIGN      TO        JIBAIJL
000242                             ORGANIZATION             IS INDEXED
000243                             ACCESS MODE              IS DYNAMIC
000244                             RECORD KEY               IS 自賠－施術和暦年月
000245                                                         自賠－患者コード
000277                             ALTERNATE RECORD KEY     IS 自賠－患者コード
000278                                                         自賠－施術和暦年月
000279                             FILE STATUS              IS 状態キー
000280                             LOCK        MODE         IS AUTOMATIC.
000102     SELECT  保険会社マスタ  ASSIGN      TO           HOKCOML
000103                             ORGANIZATION             IS  INDEXED
000104                             ACCESS MODE              IS  DYNAMIC
000105                             RECORD KEY               IS  保険会－保険会社番号
000108                             ALTERNATE RECORD KEY     IS  保険会－保険会社カナ
000110                                                          保険会－保険会社番号
000112                             FILE STATUS              IS  状態キー
000113                             LOCK        MODE         IS  AUTOMATIC.
001610     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF002
001620                             SYMBOLIC    DESTINATION  IS "PRT"
001630                             FORMAT                   IS  定義体名Ｐ
001640                             GROUP                    IS  項目群名Ｐ
001650                             PROCESSING  MODE         IS  処理種別Ｐ
001660                             UNIT        CONTROL      IS  拡張制御Ｐ
001670                             FILE        STATUS       IS  通知情報Ｐ.
001680******************************************************************
001690*                      DATA DIVISION                             *
001700******************************************************************
001710 DATA                    DIVISION.
001720 FILE                    SECTION.
001730*                           ［ＲＬ＝  ３２０］
001740 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001750     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
001760*                           ［ＲＬ＝  １２８］
001770 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001780     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001790*                           ［ＲＬ＝  １２８］
001800 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
001810     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
001850*                           ［ＲＬ＝  ２５６］
001860 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001870     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
001910*                           ［ＲＬ＝  １２８］
001920 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
001930     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
001970*                           ［ＲＬ＝  ３２０］
001980 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
001990     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
002000*                           ［ＲＬ＝  ２５６］
002010 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
002020     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
002030*                           ［ＲＬ＝  １２８］
002040 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002050     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
002090*                           ［ＲＬ＝  １２８］
002100 FD  負傷原因Ｆ          BLOCK   CONTAINS   1   RECORDS.
002110     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
002120*                           ［ＲＬ＝  ２５６］
002130 FD  市町村マスタ        BLOCK   CONTAINS   1   RECORDS.
002140     COPY SITYOSN         OF  XFDLIB  JOINING   市   AS  PREFIX.
002140*
       FD  事業所マスタ        BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY JIGYOS          OF  XFDLIB  JOINING   事   AS  PREFIX.
001080* 
000280 FD  生保情報Ｆ          BLOCK   CONTAINS   1   RECORDS.
000281     COPY SEIHOJ          OF  XFDLIB  JOINING   生保   AS  PREFIX.
001700*
000280 FD  労災情報Ｆ          BLOCK   CONTAINS   1   RECORDS.
000281     COPY ROUSAIJ         OF  XFDLIB  JOINING   労災   AS  PREFIX.
000284*
000282 FD  自賠責情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
000283     COPY JIBAIJ      OF  XFDLIB  JOINING   自賠   AS  PREFIX.
000114*
000115 FD  保険会社マスタ   BLOCK   CONTAINS   1   RECORDS.
000116     COPY HOKENCOM    OF  XFDLIB  JOINING   保険会   AS  PREFIX.
002200*
002210 FD  印刷ファイル.
002220     COPY YCK6621P        OF  XMDLIB.
002230*----------------------------------------------------------------*
002240******************************************************************
002250*                WORKING-STORAGE SECTION                         *
002260******************************************************************
002270 WORKING-STORAGE         SECTION.
002280 01 キー入力                           PIC X     VALUE SPACE.
002290 01 状態キー                           PIC X(2)  VALUE SPACE.
002300 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002310 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002320 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002330 01 文字ＣＮＴ                         PIC 9(2)  VALUE ZERO.
002340 01 初検フラグ                         PIC X(3)  VALUE SPACE.
002350 01 ファイル名                         PIC N(2)  VALUE SPACE.
002360 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
002370 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002380 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002390 01 部位名称Ｗ                         PIC N(10) VALUE SPACE.
002400 01 負傷番号Ｗ                         PIC 9     VALUE ZERO.
002410 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
002420    03 負傷番号Ｗ１                    PIC X.
002430*
002440 01 全角負傷番号Ｗ                     PIC N     VALUE SPACE.
002450 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
002460    03 全角負傷番号Ｗ１                PIC X(2).
002470*
002480 01 負担率Ｗ                           PIC 9(3) VALUE ZERO.
002490 01 甲乙区分Ｗ                         PIC 9    VALUE ZERO.
002500 01 本人負担率Ｗ                       PIC 9(3) VALUE ZERO.
002510 01 家族負担率Ｗ                       PIC 9(3) VALUE ZERO.
002520*
002530 01 施術和暦年月日ＣＷ.
002540   03 施術和暦年月ＣＷ.
002550     05 施術和暦ＣＷ                   PIC 9    VALUE ZERO.
002560     05 施術年月ＣＷ.
002570        07 施術年ＣＷ                  PIC 9(2) VALUE ZERO.
002580        07 施術月ＣＷ                  PIC 9(2) VALUE ZERO.
002590   03 施術日ＣＷ                       PIC 9(2) VALUE ZERO.
002600 01 施術西暦年Ｗ                       PIC 9(4) VALUE ZERO.
002610 01 患者西暦年Ｗ                       PIC 9(4) VALUE ZERO.
002620 01 被保険者西暦年Ｗ                   PIC 9(4) VALUE ZERO.
002630*
002640** 負傷原因用
002650*
002660 01 カウンタ                           PIC 9(2)  VALUE ZERO.
002670 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
002680*
002690 01 負傷原因ＷＴ.
002700    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
002710    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
002720    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
002730    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
002740    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
002750    03 負傷原因ナンバーＷＴ.
002760       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
002770    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
002780 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
002790 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
002800 01 負傷原因ＴＢＬ.
002810    03 負傷原因コードＴＢＬ            OCCURS 9.
002820       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
002830       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
002840       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
002850 01 負傷原因内容Ｗ.
002860    03 負傷原因内容合成Ｗ              PIC X(316) OCCURS 9 VALUE SPACE.
002870    03 負傷原因内容分解ＸＷ.
002880       05 負傷原因内容１ＸＷ           PIC X(30)  VALUE SPACE.
002890       05 負傷原因内容２ＸＷ           PIC X(30)  VALUE SPACE.
002900       05 負傷原因内容３ＸＷ           PIC X(30)  VALUE SPACE.
002910       05 負傷原因内容４ＸＷ           PIC X(30)  VALUE SPACE.
002920       05 負傷原因内容５ＸＷ           PIC X(30)  VALUE SPACE.
002930       05 負傷原因内容６ＸＷ           PIC X(30)  VALUE SPACE.
002940       05 負傷原因内容７ＸＷ           PIC X(30)  VALUE SPACE.
002930       05 負傷原因内容８ＸＷ           PIC X(30)  VALUE SPACE.
002940       05 負傷原因内容９ＸＷ           PIC X(30)  VALUE SPACE.
002940       05 負傷原因内容１０ＸＷ         PIC X(30)  VALUE SPACE.
002950       05 負傷原因内容１１ＸＷ         PIC X(16)   VALUE SPACE.
002960*
002970** 負傷原因印刷区分用
002980 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
002990*
003000* 社保用
003010 01 接尾語区分Ｗ                       PIC 9     VALUE ZERO.
003020*
003030****************
003040* 連結項目待避 *
003050****************
003060*    ************
003070*    * 印刷キー *
003080*    ************
003090 01 対象データＷＲ.
003100    03 施術和暦年月ＷＲ.
003110       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
003120       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
003130       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
003140    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
003150    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
003160    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
003170    03 被保険者カナＷＲ                 PIC X(20) VALUE SPACE.
003180    03 患者コードＷＲ.
003190       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
003200       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
003210    03 印刷モードＦＷＲ                 PIC 9(1)  VALUE ZERO.
003220    03 提出年月日ＷＲ.
003230       05 提出年ＷＲ                    PIC 9(2)  VALUE ZERO.
003240       05 提出月ＷＲ                    PIC 9(2)  VALUE ZERO.
003250       05 提出日ＷＲ                    PIC 9(2)  VALUE ZERO.
003260    03 段数ＷＲ                         PIC 9(1)  VALUE ZERO.
003270************
003280* 料金情報 *
003290************
003300*
003310 01 印刷負担率Ｗ                       PIC 9(1)  VALUE ZERO.
003320**************
003330* 受診者情報 *
003340**************
003350 01 受診者情報Ｗ.
003360    03 患者コードＷ.
003370       05 患者番号Ｗ                   PIC 9(6)  VALUE ZERO.
003380       05 枝番Ｗ                       PIC X(1)  VALUE SPACE.
003390    03 市町村番号Ｗ.
003400       05 印刷市町村番号Ｗ             PIC X(8)  VALUE SPACE.
003410       05 FILLER                       PIC X(2).
003420    03 受益者番号Ｗ.
003430       05 印刷受益者番号Ｗ             PIC X(7)  VALUE SPACE.
003440       05 FILLER                       PIC X(13).
003450    03 費用負担者番号Ｗ.
003460       05 印刷費用負担者番号Ｗ         PIC X(8)  VALUE SPACE.
003470       05 FILLER                       PIC X(2).
003480    03 受益者番号助成Ｗ.
003490       05 印刷受益者番号助成Ｗ         PIC X(10)  VALUE SPACE.
003500       05 FILLER                       PIC X(10).
003510    03 記号Ｗ                          PIC X(24)  VALUE SPACE.
003520    03 番号Ｗ.
003530       05 印刷番号Ｗ                   PIC X(30)  VALUE SPACE.
003540*       05 FILLER                       PIC X(18).
003550    03 被保険者情報Ｗ.
003560       05 被保険者カナＷ               PIC X(50)  VALUE SPACE.
003570       05 被保険者氏名Ｗ               PIC X(50)  VALUE SPACE.
003580*
003590*       05 被保険者和暦チェックＷ.
003600*          07 被保険者明治Ｗ            PIC N(1)  VALUE SPACE.
003610*          07 被保険者大正Ｗ            PIC N(1)  VALUE SPACE.
003620*          07 被保険者昭和Ｗ            PIC N(1)  VALUE SPACE.
003630       05 被保険者性別チェックＷ.
003640          07 被保険者男チェックＷ      PIC N(1)  VALUE SPACE.
003650          07 被保険者女チェックＷ      PIC N(1)  VALUE SPACE.
003660       05 被保険者性別Ｗ               PIC 9     VALUE ZERO.
003670       05 被保険者和暦チェックＷ.
003680          07 被保険者明治チェックＷ    PIC N(1)  VALUE SPACE.
003690          07 被保険者大正チェックＷ    PIC N(1)  VALUE SPACE.
003700          07 被保険者昭和チェックＷ    PIC N(1)  VALUE SPACE.
003710          07 被保険者平成チェックＷ    PIC N(1)  VALUE SPACE.
003720       05 被保険者生年月日Ｗ.
003730          07 被保険者和暦Ｗ            PIC 9     VALUE ZERO.
003740          07 被保険者年Ｗ              PIC 9(2)  VALUE ZERO.
003750          07 被保険者月Ｗ              PIC 9(2)  VALUE ZERO.
003760          07 被保険者日Ｗ              PIC 9(2)  VALUE ZERO.
003770       05 被保険者年齢Ｗ               PIC 9(3) VALUE ZERO.
003780*
003790       05 被保険者郵便番号Ｗ.
003800          07 被保険者郵便番号１Ｗ      PIC X(3)  VALUE SPACE.
003810          07 被保険者郵便番号２Ｗ      PIC X(4)  VALUE SPACE.
003820       05 被保険者住所Ｗ.
003830          07 印刷被保険者住所Ｗ        PIC X(100)  VALUE SPACE.
003840       05 被保険者電話番号Ｗ           PIC X(15)  VALUE SPACE.
003850       05 分割電話番号Ｗ.
003860          07 分割電話番号１Ｗ          PIC X(5)  VALUE SPACE.
003870          07 分割電話番号２Ｗ          PIC X(5)  VALUE SPACE.
003880          07 分割電話番号３Ｗ          PIC X(5)  VALUE SPACE.
003890*          07 市外局番Ｗ                PIC X(5)  VALUE SPACE.
003900*          07 分割局番Ｗ                PIC X(5)  VALUE SPACE.
003910*          07 分割番号Ｗ                PIC X(5)  VALUE SPACE.
003920
003930    03 資格取得年月日Ｗ.
003940       05 資格取得元号Ｗ               PIC N(2)  VALUE SPACE.
003950       05 資格取得和暦Ｗ               PIC 9(1)  VALUE ZERO.
003960       05 資格取得年Ｗ                 PIC 9(2)  VALUE ZERO.
003970       05 資格取得月Ｗ                 PIC 9(2)  VALUE ZERO.
003980       05 資格取得日Ｗ                 PIC 9(2)  VALUE ZERO.
003990    03 有効期限年月日Ｗ.
004000       05 有効期限元号Ｗ               PIC N(2)  VALUE SPACE.
004010       05 有効期限和暦Ｗ               PIC 9(1)  VALUE ZERO.
004020       05 有効期限年Ｗ                 PIC 9(2)  VALUE ZERO.
004030       05 有効期限月Ｗ                 PIC 9(2)  VALUE ZERO.
004040       05 有効期限日Ｗ                 PIC 9(2)  VALUE ZERO.
004050       05 資格和暦チェックＷ.
004060          07 資格昭和チェックＷ        PIC N(1)  VALUE SPACE.
004070          07 資格平成チェックＷ        PIC N(1)  VALUE SPACE.
004080    03 患者情報Ｗ.
004090       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
004100       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
004110       05 患者性別チェックＷ.
004120          07 患者男チェックＷ          PIC N(1)  VALUE SPACE.
004130          07 患者女チェックＷ          PIC N(1)  VALUE SPACE.
004140       05 続柄Ｗ.
004150          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
004160          07 FILLER                    PIC X(4).
004170       05 和暦チェックＷ.
004180          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
004190          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
004200          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
004210          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
004220       05 患者年Ｗ                     PIC 9(2) VALUE ZERO.
004230       05 患者月Ｗ                     PIC 9(2) VALUE ZERO.
004240       05 患者日Ｗ                     PIC 9(2) VALUE ZERO.
004250       05 患者年齢Ｗ                   PIC 9(3) VALUE ZERO.
004260       05 患者性別Ｗ                   PIC N(4) VALUE SPACE.
004270    03 保険種別チェックＷ.
004280       05 社保チェックＷ               PIC N(1) VALUE SPACE.
004290       05 組合チェックＷ               PIC N(1) VALUE SPACE.
004300       05 共済チェックＷ               PIC N(1) VALUE SPACE.
004310       05 国保チェックＷ               PIC N(1) VALUE SPACE.
004320       05 日雇チェックＷ               PIC N(1) VALUE SPACE.
004330       05 船員チェックＷ               PIC N(1) VALUE SPACE.
004340       05 老人チェックＷ               PIC N(1) VALUE SPACE.
004350       05 福チェックＷ                 PIC N(1) VALUE SPACE.
004360       05 身障チェックＷ               PIC N(1) VALUE SPACE.
004370       05 退職チェックＷ               PIC N(1) VALUE SPACE.
004380       05 母子チェックＷ               PIC N(1) VALUE SPACE.
004390       05 乳幼児チェックＷ             PIC N(1) VALUE SPACE.
004400       05 国保組合チェックＷ           PIC N(1) VALUE SPACE.
004410       05 労災チェックＷ               PIC N(1) VALUE SPACE.
004420       05 自賠チェックＷ               PIC N(1) VALUE SPACE.
004430       05 生保チェックＷ               PIC N(1) VALUE SPACE.
004440       05 原爆チェックＷ               PIC N(1) VALUE SPACE.
004450       05 患者郵便番号Ｗ.
004460          07 患者郵便番号１Ｗ          PIC X(3)  VALUE SPACE.
004470          07 患者郵便番号２Ｗ          PIC X(4)  VALUE SPACE.
004480       05 患者住所Ｗ.
004490          07 患者住所１Ｗ              PIC X(50) VALUE SPACE.
004500          07 患者住所２Ｗ              PIC X(50) VALUE SPACE.
004510       05 患者電話番号Ｗ               PIC X(15) VALUE SPACE.
004520    03 負傷原因Ｗ                      PIC X(30) OCCURS 65 VALUE SPACE.
004530**************
004540* 事業所情報 *
004550**************
004560 01 事業所情報Ｗ.
004570    03 事業所名称Ｗ.
004580       05 印刷事業所名称Ｗ             PIC X(60)  VALUE SPACE.
004590    03 事業所郵便番号Ｗ.
004600       05 事業所郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
004610       05 事業所郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
004620    03 事業所住所Ｗ.
004630       05 印刷事業所住所Ｗ             PIC X(50)  VALUE SPACE.
004630       05 印刷事業所住所２Ｗ           PIC X(50)  VALUE SPACE.
004650**************
004660* 請求先情報 *
004670**************
004680 01 請求先情報Ｗ.
004690    03 保険者番号Ｗ.
004700       05 印刷保険者番号Ｗ             PIC X(8)  VALUE SPACE.
004710       05 FILLER                       PIC X(2).
004720    03 請求先名称Ｗ.
004730       05 印刷請求先名称Ｗ             PIC X(40) VALUE SPACE.
004740    03 支部名Ｗ.
004750       05 印刷支部名Ｗ                 PIC X(40) VALUE SPACE.
004760    03 請求先名支部名Ｗ.
004770       05 印刷請求先名支部名Ｗ         PIC X(54) VALUE SPACE.
004780       05 FILLER                       PIC X(26).
004790    03 保険者呼名Ｗ.
004800*       05 印刷保険者呼名Ｗ             PIC N(7)  VALUE SPACE.
004810       05 印刷保険者呼名Ｗ             PIC X(14)  VALUE SPACE.
004820    03 請求先郵便番号Ｗ.
004830       05 請求先郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
004840       05 請求先郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
004850    03 請求先住所１Ｗ.
004860       05 印刷請求先住所１Ｗ           PIC X(40) VALUE SPACE.
004870    03 請求先住所２Ｗ.
004880       05 印刷請求先住所２Ｗ           PIC X(35) VALUE SPACE.
004890       05 FILLER                       PIC X(5).
004900****************
004910* 負傷データＦ *
004920****************
004930 01 負傷情報Ｗ.
004940    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
004950    03 負傷データ情報Ｗ  OCCURS   9.
004960       05 会部位コードＷ               PIC X(4)  VALUE SPACE.
004970       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
004980       05 負傷種別Ｗ                   PIC 9(2)  VALUE ZERO.
004990       05 部位Ｗ                       PIC 9(2)  VALUE ZERO.
005000       05 左右区分Ｗ                   PIC 9(1)  VALUE ZERO.
005010       05 負傷位置番号Ｗ               PIC 9(2)  VALUE ZERO.
005020       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
005030       05 負傷年月日Ｗ.
005040          07 負傷和暦Ｗ                PIC 9(1)  VALUE ZERO.
005050          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
005060          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
005070          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
005080          07 負傷年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005090       05 施術開始年月日Ｗ.
005100          07 施術開始和暦Ｗ            PIC 9(1)  VALUE ZERO.
005110          07 施術開始年Ｗ              PIC 9(2)  VALUE ZERO.
005120          07 施術開始月Ｗ              PIC 9(2)  VALUE ZERO.
005130          07 施術開始日Ｗ              PIC 9(2)  VALUE ZERO.
005140          07 施開年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005150       05 施術終了年月日Ｗ.
005160          07 施術終了和暦Ｗ            PIC 9(1)  VALUE ZERO.
005170          07 施術終了年Ｗ              PIC 9(2)  VALUE ZERO.
005180          07 施術終了月Ｗ              PIC 9(2)  VALUE ZERO.
005190          07 施術終了日Ｗ              PIC 9(2)  VALUE ZERO.
005200          07 施終年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005210       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
005220       05 転帰Ｗ                       PIC N(4)  VALUE SPACE.
005230       05 転帰チェックＷ.
005240          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
005250          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
005260          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
005270*
005280******************
005290* 負担率チェック *
005300******************
005310 01 負担率チェックＷ.
005320    03 ０割チェックＷ                  PIC N(1)  VALUE SPACE.
005330    03 １割チェックＷ                  PIC N(1)  VALUE SPACE.
005340    03 ２割チェックＷ                  PIC N(1)  VALUE SPACE.
005350    03 ３割チェックＷ                  PIC N(1)  VALUE SPACE.
005360**********
005370* 合計行 *
005380**********
005390 01 合計行Ｗ.
005400    03 請求終了日Ｗ                    PIC 9(2) VALUE ZERO.
005410    03 対象西暦Ｗ                      PIC 9(4) VALUE ZERO.
005420    03 商Ｗ                            PIC 9(3) VALUE ZERO.
005430    03 余Ｗ                            PIC 9(3) VALUE ZERO.
005440*
005450****************************
005460* 詳細時の負傷原因印刷判定 *
005470****************************
005480 01 負傷原因転記Ｆ                     PIC X(3) VALUE SPACE.
005490
005500
005510****************
005520* 詳細用ワーク *
005530****************
005540 01 負傷情報ＳＷ.
005550    03 負傷データ情報ＳＷ  OCCURS   9.
005560       05 会部位コードＳＷ               PIC X(4)  VALUE SPACE.
005570       05 部位ＣＮＴＳＷ                 PIC 9(1)  VALUE ZERO.
005580       05 負傷種別ＳＷ                   PIC 9(2)  VALUE ZERO.
005590       05 部位ＳＷ                       PIC 9(2)  VALUE ZERO.
005600       05 左右区分ＳＷ                   PIC 9(1)  VALUE ZERO.
005610       05 負傷位置番号ＳＷ               PIC 9(2)  VALUE ZERO.
005620       05 負傷名ＳＷ                     PIC N(18) VALUE SPACE.
005630       05 負傷年月日ＳＷ.
005640          07 負傷和暦ＳＷ                PIC 9(1)  VALUE ZERO.
005650          07 負傷年ＳＷ                  PIC 9(2)  VALUE ZERO.
005660          07 負傷月ＳＷ                  PIC 9(2)  VALUE ZERO.
005670          07 負傷日ＳＷ                  PIC 9(2)  VALUE ZERO.
005680          07 負傷年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
005690       05 施術開始年月日ＳＷ.
005700          07 施術開始和暦ＳＷ            PIC 9(1)  VALUE ZERO.
005710          07 施術開始年ＳＷ              PIC 9(2)  VALUE ZERO.
005720          07 施術開始月ＳＷ              PIC 9(2)  VALUE ZERO.
005730          07 施術開始日ＳＷ              PIC 9(2)  VALUE ZERO.
005740          07 施開年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
005750       05 施術終了年月日ＳＷ.
005760          07 施術終了和暦ＳＷ            PIC 9(1)  VALUE ZERO.
005770          07 施術終了年ＳＷ              PIC 9(2)  VALUE ZERO.
005780          07 施術終了月ＳＷ              PIC 9(2)  VALUE ZERO.
005790          07 施術終了日ＳＷ              PIC 9(2)  VALUE ZERO.
005800          07 施終年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
005810       05 開始年月日取得フラグＳ         PIC X(3)  VALUE SPACE.
005820       05 転帰ＳＷ                       PIC N(4)  VALUE SPACE.
005830       05 転帰チェックＳＷ.
005840          07 治癒チェックＳＷ            PIC N(1)  VALUE SPACE.
005850          07 中止チェックＳＷ            PIC N(1)  VALUE SPACE.
005860          07 転医チェックＳＷ            PIC N(1)  VALUE SPACE.
005870       05 施術日数ＳＷ                   PIC 9(4)  VALUE ZERO.
005880*
005890*/年月毎持つ保険者データのワーク
005900 01 保負担率区分Ｗ                     PIC 9    VALUE ZERO.
005910 01 保本人負担率Ｗ                     PIC 9(3) VALUE ZERO.
005920 01 保家族負担率Ｗ                     PIC 9(3) VALUE ZERO.
005930 01 保本人負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
005940 01 保家族負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
005950*
005960 01 比較和暦年月Ｗ.
005970    03 比較和暦Ｗ                      PIC 9    VALUE ZERO.
005980    03 比較年Ｗ                        PIC 9(2) VALUE ZERO.
005990    03 比較月Ｗ                        PIC 9(2) VALUE ZERO.
006000*
006010*/老人保険改正の対応0101
006020 01 老人負担計算区分Ｗ                 PIC 9    VALUE ZERO.
006030 01 負担率用種別Ｗ                     PIC 9(2) VALUE ZERO.
006040 01 助成負担区分Ｗ                     PIC 9(2) VALUE ZERO.
006050*
006051*
006052** 助成負担率用(14/10～)
006053 01 助成負担率ＷＷ.
006054    03 助成負担率Ｗ                    PIC 9(3) VALUE ZERO.
006055    03 助成負担率Ｗ１                  PIC 9    VALUE ZERO.
006056    03 助成負担率文字Ｗ１              PIC X    VALUE SPACE.
006057    03 助成負担率表示Ｗ                PIC X(6) VALUE SPACE.
006058*
006060*******************************************************************
006070 01 印刷制御.
006080     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
006090     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
006100     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
006110     03 拡張制御Ｐ.
006120         05 端末制御Ｐ.
006130             07 移動方向Ｐ             PIC X(1)  VALUE SPACE.
006140             07 移動行数Ｐ             PIC 9(3)  VALUE ZERO.
006150         05 詳細制御Ｐ                 PIC X(2)  VALUE SPACE.
006160     03 通知情報Ｐ                     PIC X(2)  VALUE SPACE.
006170     03 ユニット名Ｐ                   PIC X(8)  VALUE SPACE.
006180*
006190 01 計算機西暦年Ｗ                     PIC 9(2)  VALUE ZERO.
006200* 日付ＷＯＲＫ
006210 01 和暦終了年Ｗ                       PIC 9(4)  VALUE ZERO.
006220 01 計算機西暦.
006230    03 計算機西暦年                    PIC 9(4)  VALUE ZERO.
006240    03 計算機西暦月日                  PIC 9(4)  VALUE ZERO.
006250 01 計算機西暦Ｒ REDEFINES 計算機西暦.
006260    03 計算機世紀                      PIC 9(2).
006270    03 計算機日付                      PIC 9(6).
006280    03 計算機日付Ｒ REDEFINES 計算機日付.
006290       05 計算機年月                   PIC 9(4).
006300       05 計算機年月Ｒ REDEFINES 計算機年月.
006310         07 計算機年                   PIC 9(2).
006320         07 計算機月                   PIC 9(2).
006330       05 計算機日                     PIC 9(2).
006340*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
006359******************************************************************
006360*                          連結項目                              *
006370******************************************************************
006380*
006390**********************
006400* メッセージ表示キー *
006410**********************
006420 01 連メ－キー IS EXTERNAL.
006430    03  連メ－メッセージ                 PIC N(20).
006440*
006450****************
006460* 連結項目待避 *
006470****************
006480*    ************
006490*    * 料金情報 *
006500*    ************
006510*    月毎の料金
006520***********************
006530 01 連施－料金１ IS EXTERNAL.
006540   03 連施－本人負担率                 PIC 9(3).
006550************
006560* 印刷キー *
006570************
006580 01 連印－対象データ IS EXTERNAL.
006590    03 連印－施術和暦年月.
006600       05 連印－施術和暦                  PIC 9(1).
006610       05 連印－施術年                    PIC 9(2).
006620       05 連印－施術月                    PIC 9(2).
006630    03 連印－保険種別                     PIC 9(2).
006640    03 連印－保険者番号                   PIC X(10).
006650    03 連印－本人家族区分                 PIC 9(1).
006660    03 連印－被保険者カナ                 PIC X(20).
006670    03 連印－患者コード.
006680       05 連印－患者番号                  PIC 9(6).
006690       05 連印－枝番                      PIC X(1).
006700    03 連印－印刷モードＦ                 PIC 9(1).
006760*/詳細モード用
006770    03 連印－保険証印刷区分               PIC 9(1).
006780    03 連印－負傷詳細 OCCURS 7.
006790       05 連印－負傷印刷行                PIC 9(1).
006800       05 連印－部位印刷区分              PIC 9(1).
006810       05 連印－転帰印刷区分              PIC 9(1).
006820       05 連印－原因印刷区分              PIC 9(1).
      */
       01 連印－対象データ料金 IS EXTERNAL GLOBAL.
          03 連印－金額 OCCURS 4.
             05 連印－金額和暦                  PIC 9.
             05 連印－金額年                    PIC 9(2).
             05 連印－金額月                    PIC 9(2).
             05 連印－金額請求年月日.
                07 連印－金額請求年             PIC 9(2).
                07 連印－金額請求月             PIC 9(2).
                07 連印－金額請求日             PIC 9(2).
          03 連印－提出年月日.
             05 連印－提出年                    PIC 9(2).
             05 連印－提出月                    PIC 9(2).
             05 連印－提出日                    PIC 9(2).
          03 連印－段数                         PIC 9(1).
006341*
       01 連入－表示フラグ６６０ IS EXTERNAL GLOBAL.
          03 連入－プレビュー区分               PIC 9(1).
006342* 負担率取得用14/10～
006343 01 連率－負担率取得キー IS EXTERNAL.
006344    03 連率－施術和暦年月.
006345       05 連率－施術和暦               PIC 9.
006346       05 連率－施術年月.
006347          07 連率－施術年              PIC 9(2).
006348          07 連率－施術月              PIC 9(2).
006349    03 連率－患者コード.
006350       05 連率－患者番号               PIC 9(6).
006351       05 連率－枝番                   PIC X.
006352    03 連率－実際負担率                PIC 9(3).
006353    03 連率－実際本体負担率            PIC 9(3).
006354    03 連率－健保負担率                PIC 9(3).
006355    03 連率－２７老負担率              PIC 9(3).
006356    03 連率－助成負担率                PIC 9(3).
006357    03 連率－特別用負担率              PIC 9(3).
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
006358*
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
007240******************************************************************
007250*                      PROCEDURE  DIVISION                       *
007260******************************************************************
007270 PROCEDURE               DIVISION.
007280************
007290*           *
007300* 初期処理   *
007310*           *
007320************
002570     PERFORM プリンタファイル作成.
007330     PERFORM 初期化.
007340************
007350*           *
007360* 主処理     *
007370*           *
007380************
007390* 印刷
007400     PERFORM 連結項目待避.
007410     PERFORM 印刷セット
007420     PERFORM 印刷処理
007430************
007440*           *
007450* 終了処理   *
007460*           *
007470************
007480     PERFORM 終了処理.
007490     EXIT PROGRAM.
007500*
007510*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
           MOVE "KARUTE"              TO Ｈ連特殊ＰＲＴＦ－用紙種類.
002970*   使用するプリンタファイル名セット
002231     MOVE "PRTF002"             TO Ｈ連ＰＲＴＦ－ファイル名.
002972*
002973*   使用する帳票プログラム名セット
002974     MOVE "YCK6621"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分 TO Ｈ連ＰＲＴＦ－プレビュー区分.
003000*     MOVE 1 TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
007520*================================================================*
007530 初期化 SECTION.
007540*
007550     PERFORM ファイルオープン.
007560*    /* 現在日付取得 */
007570     ACCEPT 計算機日付 FROM DATE.
007580*    /* 1980～2079年の間で設定 */
007590     IF 計算機年 > 80
007600         MOVE 19 TO 計算機世紀
007610     ELSE
007620         MOVE 20 TO 計算機世紀
007630     END-IF.
007640     PERFORM カレント元号取得.
007650     PERFORM 和暦終了年取得.
007660     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
007670*================================================================*
007680 カレント元号取得 SECTION.
007690*
007700     MOVE ZEROS TO 制－制御区分.
007710     READ 制御情報マスタ
007720     NOT INVALID KEY
007730         MOVE 制－カレント元号 TO カレント元号Ｗ
007740         MOVE 制－カルテ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
007750     END-READ.
007760*
007770*================================================================*
007780 和暦終了年取得 SECTION.
007790*
007800     MOVE カレント元号Ｗ TO 元－元号区分.
007810     READ 元号マスタ
007820     INVALID KEY
007830         DISPLAY NC"指定和暦が登録されていません" UPON CONS
007840         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
007850                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007860         ACCEPT  キー入力 FROM CONS
007870         PERFORM 終了処理
007880         EXIT PROGRAM
007890     NOT INVALID KEY
007900         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
007910         MOVE 前和暦Ｗ TO 元－元号区分
007920         READ 元号マスタ
007930         INVALID KEY
007940             DISPLAY NC"指定和暦が登録されていません" UPON CONS
007950             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
007960                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007970             ACCEPT  キー入力 FROM CONS
007980             PERFORM 終了処理
007990             EXIT PROGRAM
008000         NOT INVALID KEY
008010             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
008020         END-READ
008030     END-READ.
008040*
008050*================================================================*
008060 連結項目待避 SECTION.
008070*
008080     INITIALIZE 対象データＷＲ.
008090     MOVE 連印－施術和暦      TO 施術和暦ＷＲ.
008100     MOVE 連印－施術年        TO 施術年ＷＲ.
008110     MOVE 連印－施術月        TO 施術月ＷＲ.
008120     MOVE 連印－保険種別      TO 保険種別ＷＲ.
008130     MOVE 連印－保険者番号    TO 保険者番号ＷＲ.
008140     MOVE 連印－本人家族区分  TO 本人家族区分ＷＲ.
008150     MOVE 連印－被保険者カナ  TO 被保険者カナＷＲ.
008160     MOVE 連印－患者番号      TO 患者番号ＷＲ.
008170     MOVE 連印－枝番          TO 枝番ＷＲ.
008180     MOVE 連印－印刷モードＦ  TO 印刷モードＦＷＲ.
008190     MOVE 連印－提出年        TO 提出年ＷＲ.
008200     MOVE 連印－提出月        TO 提出月ＷＲ.
008210     MOVE 連印－提出日        TO 提出日ＷＲ.
008220     MOVE 連印－段数          TO 段数ＷＲ.
008230*================================================================*
008240 ファイルオープン SECTION.
008250*
008260     OPEN INPUT   保険者マスタ
008270         MOVE NC"保険者" TO ファイル名.
008280         PERFORM オープンチェック.
008290     OPEN INPUT   元号マスタ
008300         MOVE NC"元号" TO ファイル名.
008310         PERFORM オープンチェック.
008320     OPEN INPUT   名称マスタ
008330         MOVE NC"名称" TO ファイル名.
008340         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
008380     OPEN INPUT   制御情報マスタ
008390         MOVE NC"制御情報" TO ファイル名.
008400         PERFORM オープンチェック.
008440     OPEN INPUT   請求先マスタ
008450         MOVE NC"請先" TO ファイル名.
008460         PERFORM オープンチェック.
008500     OPEN INPUT   受診者情報Ｆ.
008510         MOVE NC"受情" TO ファイル名.
008520         PERFORM オープンチェック.
008530     OPEN INPUT   施術記録Ｆ.
008540         MOVE NC"施記Ｆ" TO ファイル名.
008550         PERFORM オープンチェック.
008560     OPEN INPUT   負傷データＦ.
008570         MOVE NC"負傷" TO ファイル名.
008580         PERFORM オープンチェック.
008620     OPEN INPUT   負傷原因Ｆ.
008630         MOVE NC"負傷原因" TO ファイル名.
008640         PERFORM オープンチェック.
008650     OPEN INPUT 市町村マスタ.
008660         MOVE NC"市町村" TO ファイル名.
008670         PERFORM オープンチェック.
007900     OPEN INPUT 事業所マスタ.
007910         MOVE NC"事業所" TO ファイル名.
007920         PERFORM オープンチェック.
006630     OPEN INPUT 生保情報Ｆ.
006640         MOVE NC"生保" TO ファイル名.
006650         PERFORM オープンチェック.
007900     OPEN INPUT 労災情報Ｆ.
007910         MOVE NC"労災情報Ｆ" TO ファイル名.
007920         PERFORM オープンチェック.
009290     OPEN INPUT 自賠責情報Ｆ.
009300         MOVE NC"自賠" TO ファイル名.
009310         PERFORM オープンチェック.
009290     OPEN INPUT 保険会社マスタ.
009300         MOVE NC"保会" TO ファイル名.
009310         PERFORM オープンチェック.
008720     OPEN I-O   印刷ファイル
008730         MOVE NC"印刷" TO ファイル名.
008740         PERFORM エラー処理Ｐ.
008750*================================================================*
008760 オープンチェック SECTION.
008770*
008780     IF 状態キー  NOT =  "00"
008790         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
008800         DISPLAY NC"状態キー：" 状態キー         UPON CONS
008810         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008820                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
008830         ACCEPT  キー入力 FROM CONS
008840         PERFORM ファイル閉鎖
008850         EXIT PROGRAM.
008860*================================================================*
008870 ファイル閉鎖 SECTION.
008880*
008890     CLOSE 印刷ファイル   保険者マスタ        元号マスタ
008900           制御情報マスタ 名称マスタ          請求先マスタ
008910           レセプトＦ     受診者情報Ｆ        施術記録Ｆ
008920           負傷データＦ   負傷原因Ｆ          生保情報Ｆ
008930           市町村マスタ   事業所マスタ        労災情報Ｆ
                 自賠責情報Ｆ   保険会社マスタ.
008940*================================================================*
008950 終了処理 SECTION.
008960*
008970     PERFORM ファイル閉鎖.
008980*================================================================*
008990 印刷セット SECTION.
009000*
009010     EVALUATE 印刷モードＦＷＲ
009020     WHEN 0
009030         PERFORM 印刷セット１
009040     WHEN 1
009050         PERFORM 印刷セット２
009060     WHEN 2
009070         PERFORM 印刷セット３
009080     WHEN 3
009090         PERFORM 印刷セット４
009100*/詳細モード用
009110     WHEN 4
009120         PERFORM 印刷セット５
009130     END-EVALUATE.
009140*
009150*================================================================*
009160 印刷セット１ SECTION.
009170*
009180     INITIALIZE YCK6621P.
009190     INITIALIZE 受診者情報Ｗ.
009200     INITIALIZE 事業所情報Ｗ.
009210     INITIALIZE 請求先情報Ｗ.
009220     INITIALIZE 負傷情報Ｗ.
009230     INITIALIZE 負担率チェックＷ.
009240     PERFORM 負担率取得.
009250     PERFORM 請求先情報取得.
009260     PERFORM 受診者情報取得.
009270     PERFORM 負傷データ取得.
009280*
009290     IF 負傷原因印刷区分Ｗ  NOT = 1 
009300        PERFORM 負傷原因取得
009310     END-IF.
009320*
009330     PERFORM 印刷上部セット.
009340     PERFORM 印刷下部セット.
009350*================================================================*
009360 印刷セット２ SECTION.
009370*
009380     INITIALIZE YCK6621P.
009390     INITIALIZE 受診者情報Ｗ.
009400     INITIALIZE 事業所情報Ｗ.
009410     INITIALIZE 請求先情報Ｗ.
009420     INITIALIZE 負傷情報Ｗ.
009430     INITIALIZE 負担率チェックＷ.
009440     PERFORM 負担率取得.
009450     PERFORM 請求先情報取得.
009460     PERFORM 受診者情報取得.
009470     PERFORM 印刷上部セット.
009480*================================================================*
009490 印刷セット３ SECTION.
009500*
009510     INITIALIZE YCK6621P.
009520     INITIALIZE 受診者情報Ｗ.
009530     INITIALIZE 事業所情報Ｗ.
009540     INITIALIZE 請求先情報Ｗ.
009550     INITIALIZE 負傷情報Ｗ.
009260     PERFORM 受診者情報取得.
009560     PERFORM 負傷データ取得.
009570*
009580     IF 負傷原因印刷区分Ｗ  NOT = 1 
009590        PERFORM 負傷原因取得
009600     END-IF.
009610*
009620     PERFORM 印刷下部セット.
009630*================================================================*
009640 印刷セット４ SECTION.
009650*
009660     INITIALIZE YCK6621P.
009670     INITIALIZE 受診者情報Ｗ.
009680     INITIALIZE 事業所情報Ｗ.
009690     INITIALIZE 請求先情報Ｗ.
009700     INITIALIZE 負傷情報Ｗ.
009710     INITIALIZE 合計行Ｗ.
009260     PERFORM 受診者情報取得.
009750     PERFORM 請求期間取得.
009760     PERFORM 合計金額行セット.
009770*================================================================*
009780 印刷上部セット SECTION.
009790*
009800     MOVE 患者番号Ｗ                 TO 患者番号.
009810     MOVE 枝番Ｗ                     TO 枝番.
009820**************************
009830* 被保険者証情報セット   *
009840**************************
009850     IF 記号Ｗ(1:2) = "＊" 
009860        MOVE  SPACE    TO  記号
009870     ELSE
009880        MOVE 記号Ｗ    TO  記号
009890     END-IF.
009900     IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
009910        ( 印刷番号Ｗ(1:2) = "＊" )
009920        MOVE  SPACE      TO  番号
009930     ELSE
009940        MOVE 印刷番号Ｗ  TO  番号
009950     END-IF.
009960************************
009970* 被保険者情報セット   *
009980************************
009990     MOVE 被保険者氏名Ｗ             TO 被保険者氏名.
010000     MOVE 被保険者カナＷ             TO 被保険者カナ.
010010*     MOVE 資格取得元号Ｗ             TO 資格取得元号.
010020     MOVE 資格取得年Ｗ               TO 資格取得年.
010030     MOVE 資格取得月Ｗ               TO 資格取得月.
010040     MOVE 資格取得日Ｗ               TO 資格取得日.
010050     MOVE 資格昭和チェックＷ         TO 資格昭和チェック.
010060     MOVE 資格平成チェックＷ         TO 資格平成チェック.
010070*/有効期限
010080     MOVE 有効期限年Ｗ               TO 有効年.
010090     MOVE 有効期限月Ｗ               TO 有効月.
010100     MOVE 有効期限日Ｗ               TO 有効日.
010110     IF ( 有効期限年Ｗ = ZERO ) AND
010120        ( 有効期限月Ｗ = ZERO ) AND
010130        ( 有効期限日Ｗ = ZERO )
010140         MOVE SPACE          TO 有効元号
010150         MOVE SPACE          TO 有効年月日表示
010160         MOVE SPACE          TO 有効年表示
010170         MOVE SPACE          TO 有効月表示
010180         MOVE SPACE          TO 有効日表示
010190     ELSE
010200         MOVE 有効期限元号Ｗ TO 有効元号
010210         MOVE NC"有効年月日" TO 有効年月日表示
010220         MOVE NC"年"         TO 有効年表示
010230         MOVE NC"月"         TO 有効月表示
010240         MOVE NC"日"         TO 有効日表示
010250     END-IF.
010260     MOVE 被保険者明治チェックＷ     TO 被保険者明治チェック.
010270     MOVE 被保険者大正チェックＷ     TO 被保険者大正チェック.
010280     MOVE 被保険者昭和チェックＷ     TO 被保険者昭和チェック.
010290     MOVE 被保険者平成チェックＷ     TO 被保険者平成チェック.
010300     MOVE 被保険者年Ｗ               TO 被保険者年.
010310     MOVE 被保険者月Ｗ               TO 被保険者月.
010320     MOVE 被保険者日Ｗ               TO 被保険者日.
010330     MOVE 被保険者年齢Ｗ             TO 被保険者年齢.
010340********************
010350* 患者情報セット   *
010360********************
010370     MOVE 印刷負担率Ｗ               TO 負担割合.
010371*
010372* 14/10～ (助成の負担率）
010373     MOVE 助成負担率表示Ｗ           TO 助成負担率.
010374*
010380*     MOVE ０割チェックＷ             TO ０割チェック.
010390*     MOVE １割チェックＷ             TO １割チェック.
010400*     MOVE ２割チェックＷ             TO ２割チェック.
010410*     MOVE ３割チェックＷ             TO ３割チェック.
010420     IF  印刷市町村番号Ｗ(1:2) = "99"
010430         MOVE SPACE                  TO 市町村番号
010440     ELSE
010450         MOVE 印刷市町村番号Ｗ       TO 市町村番号
010460     END-IF.
010470     IF (印刷受益者番号Ｗ(1:1) = "*") OR
010480        (印刷受益者番号Ｗ(1:2) = "＊") 
010490         MOVE SPACE                  TO 受益者番号
010500     ELSE
010510         MOVE 印刷受益者番号Ｗ       TO 受益者番号
010520     END-IF
010530*
010540     IF  印刷費用負担者番号Ｗ(1:2) = "99"
010550         MOVE SPACE                  TO 公費負担者番号
010560     ELSE
010570         MOVE 印刷費用負担者番号Ｗ   TO 公費負担者番号
010580     END-IF.
010590*
010600     IF (印刷受益者番号助成Ｗ(1:1) = "*") OR
010610        (印刷受益者番号助成Ｗ(1:2) = "＊")
010620         MOVE SPACE                  TO 受益者番号助成
010630     ELSE
010640         MOVE 印刷受益者番号助成Ｗ   TO 受益者番号助成
010650     END-IF.
010660     MOVE 患者カナＷ                 TO 患者カナ.
010670     MOVE 患者氏名Ｗ                 TO 患者氏名.
010680*     MOVE 患者性別Ｗ                 TO 患者性別.
010690     MOVE 被保険者男チェックＷ       TO 被保険者男チェック.
010700     MOVE 被保険者女チェックＷ       TO 被保険者女チェック.
010710     MOVE 被保険者郵便番号１Ｗ       TO 被保険者郵便番号１.
010720     MOVE 被保険者郵便番号２Ｗ       TO 被保険者郵便番号２.
010730     MOVE "-"                        TO 被保険者郵便区切.
010740     MOVE 被保険者住所Ｗ             TO 被保険者住所.
010750     MOVE 被保険者電話番号Ｗ         TO 被保険者電話番号.
010760     MOVE 患者男チェックＷ           TO 患者男チェック.
010770     MOVE 患者女チェックＷ           TO 患者女チェック.
010780     MOVE 明治チェックＷ             TO 明治チェック.
010790     MOVE 大正チェックＷ             TO 大正チェック.
010800     MOVE 昭和チェックＷ             TO 昭和チェック.
010810     MOVE 平成チェックＷ             TO 平成チェック.
010820     MOVE 患者年Ｗ                   TO 患者年.
010830     MOVE 患者月Ｗ                   TO 患者月.
010840     MOVE 患者日Ｗ                   TO 患者日.
010850     MOVE 患者年齢Ｗ                 TO 患者年齢.
010860     MOVE 印刷続柄Ｗ                 TO 続柄.
010870     MOVE 患者郵便番号１Ｗ           TO 患者郵便番号１.
010880     MOVE 患者郵便番号２Ｗ           TO 患者郵便番号２.
010890     MOVE "-"                        TO 患者郵便区切.
010900     MOVE 患者住所Ｗ                 TO 患者住所.
010910     MOVE 患者電話番号Ｗ             TO 患者電話番号.
010920*     MOVE 患者住所１Ｗ               TO 患者住所１.
010930*     MOVE 患者住所２Ｗ               TO 患者住所２.
010940********************
010950* 事業所情報セット *
010960********************
010970     MOVE 事業所名称Ｗ               TO 事業所名称.
010980     MOVE 事業所郵便番号１Ｗ         TO 事業所郵便番号１.
010990     MOVE 事業所郵便番号２Ｗ         TO 事業所郵便番号２.
011000     MOVE 印刷事業所住所Ｗ           TO 事業所住所.
011000     MOVE 印刷事業所住所２Ｗ         TO 事業所住所２.
011010************************************
011020* 都道府県・健康保険組合情報セット *
011030************************************
011040*労災、自費の時は保険者情報は印字しない
011050     IF 保険種別ＷＲ = 70 OR 90
011060         MOVE SPACE                  TO 保険者番号
011070         MOVE SPACE                  TO 請求先名支部名
011080     ELSE
011090         MOVE 印刷保険者番号Ｗ       TO 保険者番号
011100         MOVE 印刷請求先名支部名Ｗ   TO 請求先名支部名
011110     END-IF.
011120**************************
011130* 保険種別チェックセット *
011140**************************
011150     MOVE 社保チェックＷ             TO 社保チェック.
011160     MOVE 組合チェックＷ             TO 組合チェック.
011170     MOVE 日雇チェックＷ             TO 日雇チェック.
011180     MOVE 船員チェックＷ             TO 船員チェック.
011190     MOVE 共済チェックＷ             TO 共済チェック.
011200     MOVE 国保チェックＷ             TO 国保チェック.
011210     MOVE 退職チェックＷ             TO 退職チェック.
011220     MOVE 老人チェックＷ             TO 老人チェック.
011230     MOVE 乳幼児チェックＷ           TO 乳幼児チェック.
011240     MOVE 自賠チェックＷ             TO 自賠チェック.
011250     MOVE 生保チェックＷ             TO 生保チェック.
011260     MOVE 福チェックＷ               TO 福チェック.
011270     MOVE 身障チェックＷ             TO 身障チェック.
011280     MOVE 母子チェックＷ             TO 母子チェック.
011290     MOVE 生保チェックＷ             TO 生保チェック.
011300     MOVE 原爆チェックＷ             TO 原爆チェック.
011310*================================================================*
011320 印刷下部セット SECTION.
011330*
011340********************
011350* 負傷データセット *
011360********************
011370* １部位 *
011380**********
011390*     MOVE 会部位コードＷ(1)   TO 負傷コード１.
011400     MOVE 負傷名Ｗ(1)         TO 負傷名１.
011410     MOVE 負傷年Ｗ(1)         TO 負傷年１.
011420     MOVE 負傷月Ｗ(1)         TO 負傷月１.
011430     MOVE 負傷日Ｗ(1)         TO 負傷日１.
011440     MOVE 施術開始年Ｗ(1)     TO 施術開始年１.
011450     MOVE 施術開始月Ｗ(1)     TO 施術開始月１.
011460     MOVE 施術開始日Ｗ(1)     TO 施術開始日１.
011470     MOVE 施術開始年Ｗ(1)     TO 施術期間自年１.
011480     MOVE 施術開始月Ｗ(1)     TO 施術期間自月１.
011490     MOVE 施術開始日Ｗ(1)     TO 施術期間自日１.
011500     MOVE 施術終了年Ｗ(1)     TO 施術終了年１.
011510     MOVE 施術終了月Ｗ(1)     TO 施術終了月１.
011520     MOVE 施術終了日Ｗ(1)     TO 施術終了日１.
011530*     MOVE 負傷年月日区切Ｗ(1) TO 区切１１ 区切１２.
011540*     MOVE 施開年月日区切Ｗ(1) TO 区切１３ 区切１４.
011550*     MOVE 施終年月日区切Ｗ(1) TO 区切１５ 区切１６.
011560*     MOVE 転帰Ｗ(1)           TO 転帰１.
011570     MOVE 治癒チェックＷ(1)   TO 治癒チェック１.
011580     MOVE 中止チェックＷ(1)   TO 中止チェック１.
011590     MOVE 転医チェックＷ(1)   TO 転医チェック１.
011600*     ******************
011610*     * 日数は一時保留 *
011620*     ******************
011630*     **********************
011640*     * 施術回数は一時保留 *
011650*     **********************
011660*     **********************
011670*     * 転帰名称は一時保留 *
011680*     **********************
011690**********
011700* ２部位 *
011710**********
011720*     MOVE 会部位コードＷ(2)   TO 負傷コード２.
011730     MOVE 負傷名Ｗ(2)         TO 負傷名２.
011740     MOVE 負傷年Ｗ(2)         TO 負傷年２.
011750     MOVE 負傷月Ｗ(2)         TO 負傷月２.
011760     MOVE 負傷日Ｗ(2)         TO 負傷日２.
011770     MOVE 施術開始年Ｗ(2)     TO 施術開始年２.
011780     MOVE 施術開始月Ｗ(2)     TO 施術開始月２.
011790     MOVE 施術開始日Ｗ(2)     TO 施術開始日２.
011800     MOVE 施術開始年Ｗ(2)     TO 施術期間自年２.
011810     MOVE 施術開始月Ｗ(2)     TO 施術期間自月２.
011820     MOVE 施術開始日Ｗ(2)     TO 施術期間自日２.
011830     MOVE 施術終了年Ｗ(2)     TO 施術終了年２.
011840     MOVE 施術終了月Ｗ(2)     TO 施術終了月２.
011850     MOVE 施術終了日Ｗ(2)     TO 施術終了日２.
011860*     MOVE 負傷年月日区切Ｗ(2) TO 区切２１ 区切２２.
011870*     MOVE 施開年月日区切Ｗ(2) TO 区切２３ 区切２４.
011880*     MOVE 施終年月日区切Ｗ(2) TO 区切２５ 区切２６.
011890*     MOVE 転帰Ｗ(2)           TO 転帰２.
011900     MOVE 治癒チェックＷ(2)   TO 治癒チェック２.
011910     MOVE 中止チェックＷ(2)   TO 中止チェック２.
011920     MOVE 転医チェックＷ(2)   TO 転医チェック２.
011930*     ******************
011940*     * 日数は一時保留 *
011950*     ******************
011960*     **********************
011970*     * 施術回数は一時保留 *
011980*     **********************
011990*     **********************
012000*     * 転帰名称は一時保留 *
012010*     **********************
012020**********
012030* ３部位 *
012040**********
012050*     MOVE 会部位コードＷ(3)   TO 負傷コード３.
012060     MOVE 負傷名Ｗ(3)         TO 負傷名３.
012070     MOVE 負傷年Ｗ(3)         TO 負傷年３.
012080     MOVE 負傷月Ｗ(3)         TO 負傷月３.
012090     MOVE 負傷日Ｗ(3)         TO 負傷日３.
012100     MOVE 施術開始年Ｗ(3)     TO 施術開始年３.
012110     MOVE 施術開始月Ｗ(3)     TO 施術開始月３.
012120     MOVE 施術開始日Ｗ(3)     TO 施術開始日３.
012130     MOVE 施術開始年Ｗ(3)     TO 施術期間自年３.
012140     MOVE 施術開始月Ｗ(3)     TO 施術期間自月３.
012150     MOVE 施術開始日Ｗ(3)     TO 施術期間自日３.
012160     MOVE 施術終了年Ｗ(3)     TO 施術終了年３.
012170     MOVE 施術終了月Ｗ(3)     TO 施術終了月３.
012180*     MOVE 負傷年月日区切Ｗ(3) TO 区切３１ 区切３２.
012190*     MOVE 施開年月日区切Ｗ(3) TO 区切３３ 区切３４.
012200*     MOVE 施終年月日区切Ｗ(3) TO 区切３５ 区切３６.
012210*     MOVE 転帰Ｗ(3)           TO 転帰３.
012220     MOVE 治癒チェックＷ(3)   TO 治癒チェック３.
012230     MOVE 中止チェックＷ(3)   TO 中止チェック３.
012240     MOVE 転医チェックＷ(3)   TO 転医チェック３.
012250*     ******************
012260*     * 日数は一時保留 *
012270*     ******************
012280*     **********************
012290*     * 施術回数は一時保留 *
012300*     **********************
012310*     **********************
012320*     * 転帰名称は一時保留 *
012330*     **********************
012340**********
012350* ４部位 *
012360**********
012370*     MOVE 会部位コードＷ(4)   TO 負傷コード４.
012380     MOVE 負傷名Ｗ(4)         TO 負傷名４.
012390     MOVE 負傷年Ｗ(4)         TO 負傷年４.
012400     MOVE 負傷月Ｗ(4)         TO 負傷月４.
012410     MOVE 負傷日Ｗ(4)         TO 負傷日４.
012420     MOVE 施術開始年Ｗ(4)     TO 施術開始年４.
012430     MOVE 施術開始月Ｗ(4)     TO 施術開始月４.
012440     MOVE 施術開始日Ｗ(4)     TO 施術開始日４.
012450     MOVE 施術開始年Ｗ(4)     TO 施術期間自年４.
012460     MOVE 施術開始月Ｗ(4)     TO 施術期間自月４.
012470     MOVE 施術開始日Ｗ(4)     TO 施術期間自日４.
012480     MOVE 施術終了年Ｗ(4)     TO 施術終了年４.
012490     MOVE 施術終了月Ｗ(4)     TO 施術終了月４.
012500     MOVE 施術終了日Ｗ(4)     TO 施術終了日４.
012510*     MOVE 負傷年月日区切Ｗ(4) TO 区切４１ 区切４２.
012520*     MOVE 施開年月日区切Ｗ(4) TO 区切４３ 区切４４.
012530*     MOVE 施終年月日区切Ｗ(4) TO 区切４５ 区切４６.
012540*     MOVE 転帰Ｗ(4)           TO 転帰４.
012550     MOVE 治癒チェックＷ(4)   TO 治癒チェック４.
012560     MOVE 中止チェックＷ(4)   TO 中止チェック４.
012570     MOVE 転医チェックＷ(4)   TO 転医チェック４.
012580*     ******************
012590*     * 日数は一時保留 *
012600*     ******************
012610*     **********************
012620*     * 施術回数は一時保留 *
012630*     **********************
012640*     **********************
012650*     * 転帰名称は一時保留 *
012660*     **********************
012670**********
012680* ５部位 *
012690**********
012700*    MOVE 会部位コードＷ(5)   TO 負傷コード５.
012710*    MOVE 負傷名Ｗ(5)         TO 負傷名５.
012720*    MOVE 負傷年Ｗ(5)         TO 負傷年５.
012730*    MOVE 負傷月Ｗ(5)         TO 負傷月５.
012740*    MOVE 負傷日Ｗ(5)         TO 負傷日５.
012750*    MOVE 施術開始年Ｗ(5)     TO 施術開始年５.
012760*    MOVE 施術開始月Ｗ(5)     TO 施術開始月５.
012770*    MOVE 施術開始日Ｗ(5)     TO 施術開始日５.
012780*    MOVE 施術終了年Ｗ(5)     TO 施術終了年５.
012790*    MOVE 施術終了月Ｗ(5)     TO 施術終了月５.
012800*     MOVE 施術終了日Ｗ(5)     TO 施術終了日５.
012810*     MOVE 負傷年月日区切Ｗ(5) TO 区切５１ 区切５２.
012820*     MOVE 施開年月日区切Ｗ(5) TO 区切５３ 区切５４.
012830*     MOVE 施終年月日区切Ｗ(5) TO 区切５５ 区切５６.
012840*     MOVE 転帰Ｗ(5)           TO 転帰５.
012850*     MOVE 治癒チェックＷ(5)   TO 治癒チェック５.
012860*     MOVE 中止チェックＷ(5)   TO 中止チェック５.
012870*     MOVE 転医チェックＷ(5)   TO 転医チェック５.
012880*     ******************
012890*     * 日数は一時保留 *
012900*     ******************
012910*     **********************
012920*     * 施術回数は一時保留 *
012930*     **********************
012940*     **********************
012950*     * 転帰名称は一時保留 *
012960*     **********************
012970************
012980* 負傷原因 *
012990************
013000*-------------------------------------------------------------------*
013010* ４部位までを印字するため、５部位の負傷原因は印字しない            *
013020* 帳票定義体は１部位につき１１行の負傷原因が印字できるよう設定      *
013030*-------------------------------------------------------------------*
013040     MOVE  ZERO   TO  カウンタ.
013050     MOVE  ZERO   TO  カウンタ２
013060     PERFORM VARYING カウンタ FROM 1 BY 1
013070             UNTIL ( カウンタ > 44 ) OR ( 負傷原因Ｗ(カウンタ) = SPACE )
013090*
013100        IF ( 負傷原因Ｗ(カウンタ)(1:2) = "①" )
013110           MOVE  ZERO   TO  カウンタ２
013120        END-IF
013130        IF ( 負傷原因Ｗ(カウンタ)(1:2) = "②" )
013140           MOVE  11     TO  カウンタ２
013150        END-IF
013160        IF ( 負傷原因Ｗ(カウンタ)(1:2) = "③" )
013170           MOVE  22     TO  カウンタ２
013180        END-IF
013190        IF ( 負傷原因Ｗ(カウンタ)(1:2) = "④" )
013200           MOVE  33     TO  カウンタ２
013210        END-IF
013250*
013260        COMPUTE  カウンタ２  =  カウンタ２  +  1
013270        MOVE 負傷原因Ｗ(カウンタ)  TO 負傷原因(カウンタ２)
013280*
013290     END-PERFORM.
013300*
013310*================================================================*
013320 負担率取得 SECTION.
013330*
013340     MOVE ZERO TO 本人負担率Ｗ.
013350     MOVE ZERO TO 家族負担率Ｗ.
013360*
013370     MOVE 施術和暦ＷＲ   TO 受－施術和暦.
013380     MOVE 施術年ＷＲ     TO 受－施術年.
013390     MOVE 施術月ＷＲ     TO 受－施術月.
013400     MOVE 患者番号ＷＲ   TO 受－患者番号.
013410     MOVE 枝番ＷＲ       TO 受－枝番.
013420     READ 受診者情報Ｆ
013430     INVALID KEY
013440         MOVE  NC"　　施術月の受診者情報がありません" TO 連メ－メッセージ
013450         CALL   "MSG001"
013460         CANCEL "MSG001"
013470         PERFORM ファイル閉鎖
013480         MOVE ZERO TO PROGRAM-STATUS
013490         EXIT PROGRAM
013500     NOT INVALID KEY
013501* 14/10～　サブルーチン処理
013502      IF 受－施術和暦年月 >= 41410
013503         PERFORM 負担率取得１４１０
014690         COMPUTE 印刷負担率Ｗ = 負担率Ｗ / 10
014661       END-IF
014670     END-READ.
014691*
014692*================================================================*
014693 負担率取得１４１０ SECTION.
014694*
014695* 受診者Ｆ READ中
014696* 平成14/10～
014697     MOVE ZERO  TO 負担率Ｗ.
014698     INITIALIZE 助成負担率ＷＷ.
014699*
014700     MOVE SPACE TO 連率－負担率取得キー.
014701     INITIALIZE 連率－負担率取得キー.
014702     MOVE 受－施術和暦年月 TO 連率－施術和暦年月.
014703     MOVE 受－患者コード   TO 連率－患者コード.
014704*
014705     CALL   "HUTANRIT".
014706     CANCEL "HUTANRIT".
014707*
014708* / 本体 /
014709     MOVE 連率－実際本体負担率 TO 負担率Ｗ.
014710*
014711*
014712* / 助成 /
014713     IF 受－助成種別 NOT = ZERO
014714        MOVE 連率－助成負担率  TO 助成負担率Ｗ
014715*
014716        COMPUTE 助成負担率Ｗ１ = 助成負担率Ｗ / 10
014717        MOVE 助成負担率Ｗ１ TO  助成負担率文字Ｗ１
014718*
014719        STRING "("                 DELIMITED BY SIZE
014720               助成負担率文字Ｗ１  DELIMITED BY SIZE
014721               "割)"               DELIMITED BY SIZE
014722               INTO 助成負担率表示Ｗ
014723        END-STRING
014724*
014725     END-IF.
014726*
014880*================================================================*
014890 受診者情報取得 SECTION.
014900*
014910**************************************************
014920* 連結データから受診者情報Ｆより以下の情報を取得 *
014930* ● 患者番号.... 患者番号Ｗ                     *
014940* ● 枝番........ 枝番Ｗ                         *
014950* ● 記号 ....... 記号Ｗに格納                   *
014960* ● 番号 ....... 番号Ｗに格納                   *
014970* ● 被保険者カナ.被保険者カナＷに格納           *
014980* ● 被保険者氏名.被保険者氏名Ｗに格納           *
014990* ● 郵便番号１ ..郵便番号１Ｗに格納             *
015000* ● 郵便番号２ ..郵便番号２Ｗに格納             *
015010* ● 住所１ ......住所１Ｗに格納                 *
015020* ● 住所２ ......住所２Ｗに格納                 *
015030* ● 電話番号.....電話番号Ｗに格納               *
015040* ● 患者カナ ....患者カナＷに格納               *
015050* ● 患者氏名 ....患者氏名Ｗに格納               *
015060* ● 続柄 ........名称マスタより続柄Ｗに取得     *
015070* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
015080* ● 患者年 ......患者年Ｗに格納                 *
015090* ● 患者月 ......患者月Ｗに格納                 *
015100* ● 患者日 ......患者日Ｗに格納                 *
015110**************************************************
015120     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
015130     MOVE 施術年ＷＲ         TO 受－施術年.
015140     MOVE 施術月ＷＲ         TO 受－施術月.
015150     MOVE 患者コードＷＲ     TO 受－患者コード.
015160     READ 受診者情報Ｆ
015170     INVALID KEY
015180         CONTINUE
015190*            /* ありえない */
015200     NOT INVALID KEY
015210         MOVE 受－患者番号     TO 患者番号Ｗ
015220         MOVE 受－枝番         TO 枝番Ｗ
015230*         MOVE 受－記号         TO 記号Ｗ
015240*         MOVE 受－番号         TO 番号Ｗ
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
015250         MOVE 受－被保険者カナ TO 被保険者カナＷ
015260         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
015270*
015280         EVALUATE 受－被保険者性別
015290         WHEN 1
015300             MOVE NC"○"  TO 被保険者男チェックＷ
015310         WHEN 2
015320             MOVE NC"○"  TO 被保険者女チェックＷ
015330         END-EVALUATE
015340         EVALUATE 受－患者性別
015350         WHEN 1
015360             MOVE NC"○"  TO 患者男チェックＷ
015370         WHEN 2
015380             MOVE NC"○"  TO 患者女チェックＷ
015390         END-EVALUATE
015400*
015410         EVALUATE 受－被保険者和暦
015420         WHEN 1
015430             MOVE NC"○"  TO 被保険者明治チェックＷ
015440         WHEN 2
015450             MOVE NC"○"  TO 被保険者大正チェックＷ
015460         WHEN 3
015470             MOVE NC"○"  TO 被保険者昭和チェックＷ
015480         WHEN 4
015490             MOVE NC"○"  TO 被保険者平成チェックＷ
015500         END-EVALUATE
015510         MOVE 受－被保険者年   TO 被保険者年Ｗ
015520         MOVE 受－被保険者月   TO 被保険者月Ｗ
015530         MOVE 受－被保険者日   TO 被保険者日Ｗ
015540
015550*被保険者年齢
015560     MOVE 連印－施術和暦 TO 元－元号区分
015570     READ 元号マスタ
015580     NOT INVALID KEY
015590         COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
015600     END-READ
015610     IF ( 受－被保険者和暦 NOT = ZERO ) AND
015620        ( 受－被保険者年   NOT = ZERO ) AND
015630        ( 受－被保険者月   NOT = ZERO ) AND
015640        ( 受－被保険者日   NOT = ZERO )
015650
015660         MOVE ZERO TO 被保険者年齢Ｗ
015670*
015680         MOVE 受－被保険者和暦 TO 元－元号区分
015690         READ 元号マスタ
015700         NOT INVALID KEY
015710             COMPUTE 被保険者西暦年Ｗ = 元－開始西暦年 + ( 受－被保険者年 - 1 )
015720         END-READ
015730*
015740         COMPUTE 被保険者年齢Ｗ   = 施術西暦年Ｗ - 被保険者西暦年Ｗ
015750*
015760         IF 連印－施術月 < 受－被保険者月
015770             COMPUTE 被保険者年齢Ｗ = 被保険者年齢Ｗ - 1
015780         END-IF
015790*
015800         MOVE 被保険者年齢Ｗ     TO 被保険者年齢
015810     END-IF
015820*
015830*/郵便番号 ゼロの時はスペース
015840         IF 受－郵便番号１ = "000"
015850             MOVE SPACE          TO 被保険者郵便番号１Ｗ
015860             MOVE SPACE          TO 被保険者郵便番号２Ｗ
015870         ELSE
015880             MOVE 受－郵便番号１ TO 被保険者郵便番号１Ｗ
015890             MOVE 受－郵便番号２ TO 被保険者郵便番号２Ｗ
015900         END-IF
015910         MOVE 受－電話番号     TO 被保険者電話番号Ｗ
015920         STRING 受－住所１  DELIMITED BY SPACE
015930                受－住所２  DELIMITED BY SPACE
015940           INTO 被保険者住所Ｗ
015950         END-STRING
               IF 受－保険種別 = 70 OR 80 OR 85 OR 90
018370             MOVE 受－患者カナ       TO 被保険者カナＷ
018380             MOVE 受－患者氏名       TO 被保険者氏名Ｗ
018000             EVALUATE 受－患者性別
018010             WHEN 1
018020                 MOVE NC"○"  TO 被保険者男チェックＷ
018030             WHEN 2
018040                 MOVE NC"○"  TO 被保険者女チェックＷ
018050             END-EVALUATE
018070             EVALUATE 受－患者和暦
018080             WHEN 1
018090                 MOVE NC"○"  TO 被保険者明治チェックＷ
018100             WHEN 2
018110                 MOVE NC"○"  TO 被保険者大正チェックＷ
018120             WHEN 3
018130                 MOVE NC"○"  TO 被保険者昭和チェックＷ
      */被保険者平成を追加/080606
                   WHEN 4
                       MOVE NC"○"  TO 被保険者平成チェックＷ
018140             END-EVALUATE
018150             MOVE 受－患者年   TO 被保険者年Ｗ
018160             MOVE 受－患者月   TO 被保険者月Ｗ
018170             MOVE 受－患者日   TO 被保険者日Ｗ
019210             MOVE 受－患者郵便番号１ TO 被保険者郵便番号１Ｗ
019220             MOVE 受－患者郵便番号２ TO 被保険者郵便番号２Ｗ
016670             STRING 受－患者住所１  DELIMITED BY SPACE
016680                    受－患者住所２  DELIMITED BY SPACE
016690               INTO 被保険者住所Ｗ
016700             END-STRING
018220             MOVE 受－患者電話番号   TO 被保険者電話番号Ｗ
016040             IF ( 受－患者和暦 NOT = ZERO ) AND
016050                ( 受－患者年   NOT = ZERO ) AND
016060                ( 受－患者月   NOT = ZERO ) AND
016070                ( 受－患者日   NOT = ZERO )
016080
016090                 MOVE ZERO TO 被保険者年齢Ｗ
016100*
016110                 MOVE 受－患者和暦 TO 元－元号区分
016120                 READ 元号マスタ
016130                 NOT INVALID KEY
016140                     COMPUTE 被保険者西暦年Ｗ = 元－開始西暦年 + ( 受－患者年 - 1 )
016150                 END-READ
016160*
016170                 COMPUTE 被保険者年齢Ｗ   = 施術西暦年Ｗ - 被保険者西暦年Ｗ
016180*
016190                 IF 連印－施術月 < 受－患者月
016200                     COMPUTE 被保険者年齢Ｗ = 被保険者年齢Ｗ - 1
016210                 END-IF
016220*
016230*                MOVE 被保険者年齢Ｗ     TO 被保険者年齢
016240             END-IF
               END-IF
015960*
015970*/郵便番号 ゼロの時はスペース
015980         IF 受－患者郵便番号１ = "000"
015990             MOVE SPACE              TO 患者郵便番号１Ｗ
016000             MOVE SPACE              TO 患者郵便番号２Ｗ
016010         ELSE
016020             MOVE 受－患者郵便番号１ TO 患者郵便番号１Ｗ
016030             MOVE 受－患者郵便番号２ TO 患者郵便番号２Ｗ
016040         END-IF
016050*         MOVE 受－患者住所１     TO 患者住所１Ｗ
016060*         MOVE 受－患者住所２     TO 患者住所２Ｗ
016070         STRING 受－患者住所１  DELIMITED BY SPACE
016080                受－患者住所２  DELIMITED BY SPACE
016090           INTO 患者住所Ｗ
016100         END-STRING
016110         MOVE 受－患者電話番号   TO 患者電話番号Ｗ
016120*
016130*電話番号分割
016140*         UNSTRING 電話番号Ｗ  DELIMITED BY "-"
016150*             INTO 分割電話番号１Ｗ
016160*                  分割電話番号２Ｗ
016170*                  分割電話番号３Ｗ
016180*         END-UNSTRING
016190*電話番号右詰処理
016200*         IF 分割電話番号３Ｗ = SPACE
016210*             MOVE 分割電話番号２Ｗ TO 分割電話番号３Ｗ
016220*             MOVE 分割電話番号１Ｗ TO 分割電話番号２Ｗ
016230*             MOVE SPACE            TO 分割電話番号１Ｗ
016240*         END-IF
016250*
016260         MOVE 受－患者カナ     TO 患者カナＷ
016270         MOVE 受－患者氏名     TO 患者氏名Ｗ
016280*
               MOVE 受－保険種別     TO 事－保険種別
               MOVE 受－保険者番号   TO 事－保険者番号
      *         MOVE 受－記号         TO 事－記号
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
               MOVE 連暗号複合－複合した記号 TO 事－記号
      *
      *-----------------------------------------------------------------*
      *
               READ 事業所マスタ
               INVALID KEY
                  MOVE SPACE TO 事－レコード
                  INITIALIZE    事－レコード
               END-READ
               MOVE 事－事業所名称   TO 事業所名称Ｗ
015540         STRING 事－事業所住所１  DELIMITED BY SPACE
015550                事－事業所住所２  DELIMITED BY SPACE
015560           INTO 事業所住所Ｗ
015570         END-STRING
               MOVE 事－事業所住所１   TO 印刷事業所住所Ｗ
               MOVE 事－事業所住所２   TO 印刷事業所住所２Ｗ
016340* 続柄
016350         IF 受－本人家族区分 = 1
016360             MOVE NC"本人"     TO 続柄Ｗ
016370         ELSE
016380             MOVE 05          TO 名－区分コード
016390             MOVE 受－続柄    TO 名－名称コード
016400             READ 名称マスタ
016410             INVALID KEY
016420                 MOVE SPACE   TO 続柄Ｗ
016430             NOT INVALID KEY
016440                 MOVE 名－略称  TO 続柄Ｗ
016450             END-READ
016460         END-IF
016470* 患者和暦年月日
016480         EVALUATE 受－患者和暦
016490         WHEN 1
016500             MOVE NC"○"  TO 明治チェックＷ
016510         WHEN 2
016520             MOVE NC"○"  TO 大正チェックＷ
016530         WHEN 3
016540             MOVE NC"○"  TO 昭和チェックＷ
016550         WHEN 4
016560             MOVE NC"○"  TO 平成チェックＷ
016570         END-EVALUATE
016580         MOVE 受－患者年  TO 患者年Ｗ
016590         MOVE 受－患者月  TO 患者月Ｗ
016600         MOVE 受－患者日  TO 患者日Ｗ
016610*患者年齢
016620     MOVE 連印－施術和暦 TO 元－元号区分
016630     READ 元号マスタ
016640     NOT INVALID KEY
016650         COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
016660     END-READ
016670     IF ( 受－患者和暦 NOT = ZERO ) AND
016680        ( 受－患者年   NOT = ZERO ) AND
016690        ( 受－患者月   NOT = ZERO ) AND
016700        ( 受－患者日   NOT = ZERO )
016710*
016720         MOVE ZERO TO 患者年齢Ｗ
016730*
016740         MOVE 受－患者和暦 TO 元－元号区分
016750         READ 元号マスタ
016760         NOT INVALID KEY
016770             COMPUTE 患者西暦年Ｗ = 元－開始西暦年 + ( 受－患者年 - 1 )
016780         END-READ
016790*
016800         COMPUTE 患者年齢Ｗ   = 施術西暦年Ｗ - 患者西暦年Ｗ
016810*
016820         IF 連印－施術月 < 受－患者月
016830             COMPUTE 患者年齢Ｗ = 患者年齢Ｗ - 1
016840         END-IF
016850*
016860         MOVE 患者年齢Ｗ     TO 患者年齢
016870     END-IF
016880* 患者性別
016890*         EVALUATE 受－患者性別
016900*         WHEN 1
016910*             MOVE NC"（男）" TO 患者性別Ｗ
016920*         WHEN 2
016930*             MOVE NC"（女）" TO 患者性別Ｗ
016940*         END-EVALUATE
016950*/有効期限
016960         MOVE 受－有効和暦   TO 有効期限和暦Ｗ
016970         MOVE 受－有効年     TO 有効期限年Ｗ
016980         MOVE 受－有効月     TO 有効期限月Ｗ
016990         MOVE 受－有効日     TO 有効期限日Ｗ
017000         MOVE 有効期限和暦Ｗ TO 元－元号区分
017010         READ 元号マスタ
017020         INVALID KEY
017030             MOVE SPACE        TO 有効期限元号Ｗ
017040         NOT INVALID KEY
017050             MOVE 元－元号名称 TO 有効期限元号Ｗ
017060         END-READ
017070*/資格取得年月日
017080         MOVE 受－資格和暦 TO 資格取得和暦Ｗ
017090         MOVE 受－資格年   TO 資格取得年Ｗ
017100         MOVE 受－資格月   TO 資格取得月Ｗ
017110         MOVE 受－資格日   TO 資格取得日Ｗ
017120         IF (資格取得和暦Ｗ NOT = ZERO) AND
017130            (資格取得年Ｗ   NOT = ZERO) AND
017140            (資格取得月Ｗ   NOT = ZERO) AND
017150            (資格取得日Ｗ   NOT = ZERO)
017160             EVALUATE 資格取得和暦Ｗ
017170             WHEN 3
017180                 MOVE NC"○"  TO 資格昭和チェックＷ
017190             WHEN 4
017200                 MOVE NC"○"  TO 資格平成チェックＷ
017210             END-EVALUATE
017220         END-IF
017230**市町村国保､退職の場合は有効期限を出力
017240**業種別国保(国保組合)､共済､日雇､船員､社保､組合､自衛官は有効期限日を出力する
017250*         IF (( 受－保険種別 = 08 ) OR
017260*            (( 受－保険種別 = 01 ) AND
017270*            (  受－保険者番号(3:1) NOT = "3" )))
017280*            MOVE 受－有効和暦 TO 資格取得和暦Ｗ
017290*            MOVE 受－有効年   TO 資格取得年Ｗ
017300*            MOVE 受－有効月   TO 資格取得月Ｗ
017310*            MOVE 受－有効日   TO 資格取得日Ｗ
017320*         ELSE
017330*            MOVE 受－資格和暦 TO 資格取得和暦Ｗ
017340*            MOVE 受－資格年   TO 資格取得年Ｗ
017350*            MOVE 受－資格月   TO 資格取得月Ｗ
017360*            MOVE 受－資格日   TO 資格取得日Ｗ
017370*         END-IF
017380*         IF (資格取得和暦Ｗ NOT = ZERO) AND
017390*            (資格取得年Ｗ   NOT = ZERO) AND
017400*            (資格取得月Ｗ   NOT = ZERO) AND
017410*            (資格取得日Ｗ   NOT = ZERO)
017420*             EVALUATE 資格取得和暦Ｗ
017430*             WHEN 3
017440*                 MOVE NC"○"  TO 資格昭和チェックＷ
017450*             WHEN 4
017460*                 MOVE NC"○"  TO 資格平成チェックＷ
017470*             END-EVALUATE
017480*         END-IF
017490* 市町村番号 受給者番号
017500         IF 受－助成種別 NOT = ZERO
017510             MOVE 受－費用負担者番号助成 TO 費用負担者番号Ｗ
017520             MOVE 受－受益者番号助成     TO 受益者番号助成Ｗ
017530         END-IF
018290         IF ( 受－公費種別 = 05 ) AND ( 受－施術和暦年月 < 42004 )
017550             MOVE 受－費用負担者番号     TO 市町村番号Ｗ
017560             MOVE 受－受益者番号老人     TO 受益者番号Ｗ
017570         END-IF
      * 労災情報
               IF 受－保険種別 = 70
                  MOVE 受－施術和暦年月 TO 労災－施術和暦年月
                  MOVE 受－患者コード   TO 労災－患者コード
                  READ 労災情報Ｆ
                  INVALID KEY
                     MOVE SPACE TO 労災－レコード
                     INITIALIZE    労災－レコード
                  END-READ
                  MOVE 労災－労災事業所名称       TO 事業所名称Ｗ
014780            MOVE 労災－労災事業所郵便番号１ TO 事業所郵便番号１Ｗ
014790            MOVE 労災－労災事業所郵便番号２ TO 事業所郵便番号２Ｗ
      *            STRING 労災－労災事業所住所１ DELIMITED BY SPACE
014790*                   労災－労災事業所住所２ DELIMITED BY SPACE
      *               INTO 事業所住所Ｗ
      *            END-STRING
                  MOVE 労災－労災事業所住所１     TO 印刷事業所住所Ｗ
                  MOVE 労災－労災事業所住所２     TO 印刷事業所住所２Ｗ
012130            MOVE 労災－労働保険番号         TO 番号Ｗ
               END-IF
      *
017580*保険種別
017590         EVALUATE 受－助成種別
017600         WHEN 50
017610             MOVE NC"○"  TO 生保チェックＷ
017620         WHEN 51
017630*/頭4桁が "4113"東京 "4108"茨城 "4132"島根 の時は、「福」。それ以外は「老」
017640             IF  受－費用負担者番号助成(1:4) = "4113" OR
017650                 受－費用負担者番号助成(1:4) = "4108" OR
017660                 受－費用負担者番号助成(1:4) = "4132"
017670                 MOVE NC"○"    TO 福チェックＷ
017680             ELSE
017690                 MOVE NC"○"    TO 老人チェックＷ
017700             END-IF
017710*             MOVE NC"○"  TO 福チェックＷ
017720         WHEN 52
017730             MOVE NC"○"  TO 母子チェックＷ
017740         WHEN 53
017750             MOVE NC"○"  TO 身障チェックＷ
017760         WHEN 54
017770             MOVE NC"○"  TO 原爆チェックＷ
017780         WHEN 55
017790             MOVE NC"○"  TO 乳幼児チェックＷ
017800         END-EVALUATE
017810         IF 受－公費種別 = 05
017820             MOVE NC"○"  TO 老人チェックＷ
017830         ELSE
017840             EVALUATE 受－保険種別
017850             WHEN 01
017860                 MOVE NC"○"  TO 国保チェックＷ
017870             WHEN 02
017880                 MOVE NC"○"  TO 社保チェックＷ
017890             WHEN 03
017900                 MOVE NC"○"  TO 組合チェックＷ
017910             WHEN 04
017920                 MOVE NC"○"  TO 共済チェックＷ
017930             WHEN 06
017940                 MOVE NC"○"  TO 日雇チェックＷ
017950             WHEN 07
017960                 MOVE NC"○"  TO 船員チェックＷ
017970             WHEN 08
017980                 MOVE NC"○"  TO 退職チェックＷ
017990             WHEN 09
018000                 MOVE NC"○"  TO 共済チェックＷ
018010*             WHEN 70
018020*                 MOVE NC"○"  TO 労災チェックＷ
018030             WHEN 80
018040                 MOVE NC"○"  TO 自賠チェックＷ
018050             END-EVALUATE
018060         END-IF
018070     END-READ.
018080*TEST テスト－－－－－－－－－－－
018090*      MOVE NC"○"  TO 被保険者明治Ｗ.
018100*      MOVE NC"○"  TO 被保険者大正Ｗ.
018110*      MOVE NC"○"  TO 被保険者昭和Ｗ.
018120*      MOVE NC"○"  TO 被保険者男チェックＷ.
018130*      MOVE NC"○"  TO 被保険者女チェックＷ.
018140*      MOVE NC"○"  TO 患者男チェックＷ.
018150*      MOVE NC"○"  TO 患者女チェックＷ.
018160*      MOVE NC"○"  TO 明治チェックＷ.
018170*      MOVE NC"○"  TO 大正チェックＷ.
018180*      MOVE NC"○"  TO 昭和チェックＷ.
018190*      MOVE NC"○"  TO 平成チェックＷ.
018200*      MOVE NC"○"  TO 社保チェックＷ    .
018210*      MOVE NC"○"  TO 組合チェックＷ    .
018220*      MOVE NC"○"  TO 共済チェックＷ    .
018230*      MOVE NC"○"  TO 国保チェックＷ    .
018240*      MOVE NC"○"  TO 日雇チェックＷ    .
018250*      MOVE NC"○"  TO 船員チェックＷ    .
018260*      MOVE NC"○"  TO 老人チェックＷ    .
018270*      MOVE NC"○"  TO 福チェックＷ    .
018280*      MOVE NC"○"  TO 身障チェックＷ    .
018290*      MOVE NC"○"  TO 退職チェックＷ    .
018300*      MOVE NC"○"  TO 母子チェックＷ    .
018310*      MOVE NC"○"  TO 乳幼児チェックＷ  .
018320*      MOVE NC"○"  TO 国保組合チェックＷ.
018330*      MOVE NC"○"  TO 労災チェックＷ    .
018340*      MOVE NC"○"  TO 生保チェックＷ    .
018350*      MOVE NC"○"  TO 自賠チェックＷ    .
018360*      MOVE NC"○"  TO 原爆チェックＷ    .
018370*      MOVE NC"○" TO ０割チェックＷ.
018380*      MOVE NC"○" TO １割チェックＷ.
018390*      MOVE NC"○" TO ２割チェックＷ.
018400*      MOVE NC"○" TO ３割チェックＷ.
018410*      MOVE NC"○"  TO 資格昭和チェックＷ.
018420*      MOVE NC"○"  TO 資格平成チェックＷ.
018430*      MOVE NC"○"  TO 被保険者明治チェックＷ.
018440*      MOVE NC"○"  TO 被保険者大正チェックＷ.
018450*      MOVE NC"○"  TO 被保険者昭和チェックＷ.
018460*      MOVE NC"○"  TO 被保険者平成チェックＷ.
018470*-------------------------------------
018480*================================================================*
018490 請求先情報取得 SECTION.
018500*
018510****************************************************
018520* 連結データから保険者マスタより請求先を取得する。 *
018530* ※保－請求先情報区分=1の場合請求先マスタを使用   *
018540* ● 保険者番号...保険者番号Ｗに格納               *
018550* ● 名称........ 請求先名称Ｗに格納               *
018560* ● 郵便番号１.. 請求先郵便番号１Ｗに格納         *
018570* ● 郵便番号２.. 請求先郵便番号２Ｗに格納         *
018580* ● 住所１.......請求先住所１Ｗに格納             *
018590* ● 住所２.......請求先住所２Ｗに格納             *
018600****************************************************
018610     MOVE SPACE          TO 保険者番号Ｗ.
018620     MOVE SPACE          TO 請求先名称Ｗ.
018630     MOVE SPACE          TO 支部名Ｗ.
018640     MOVE SPACE          TO 請求先郵便番号１Ｗ.
018650     MOVE SPACE          TO 請求先郵便番号２Ｗ.
018660     MOVE SPACE          TO 請求先住所１Ｗ.
018670     MOVE SPACE          TO 請求先住所２Ｗ.
018680     MOVE SPACE          TO 印刷保険者呼名Ｗ.
018690     MOVE SPACE          TO 請求先名支部名Ｗ
018700*
018710     MOVE 保険種別ＷＲ   TO 保－保険種別.
018720     MOVE 保険者番号ＷＲ TO 保－保険者番号.
018730     READ 保険者マスタ
018740     INVALID KEY
               IF ( 受－施術和暦年月 >= 42004 ) AND ( 保険種別ＷＲ = 05 )
016990             MOVE 保険種別ＷＲ   TO 市－公費種別
017000             MOVE 保険者番号ＷＲ TO 市－市町村番号
017010             READ 市町村マスタ
017020             INVALID KEY
017030                 MOVE SPACE      TO 請求先名称Ｗ
019510                 MOVE SPACE      TO 支部名Ｗ
019520                 MOVE ZERO       TO 接尾語区分Ｗ
017040             NOT INVALID KEY
017090                 MOVE 市－市町村番号  TO 保険者番号Ｗ
017110                 MOVE 市－市町村名称  TO 請求先名称Ｗ
019560                 MOVE 市－支部部署名  TO 支部名Ｗ
017120                 MOVE 市－郵便番号１  TO 請求先郵便番号１Ｗ
017130                 MOVE 市－郵便番号２  TO 請求先郵便番号２Ｗ
017140                 MOVE 市－住所１      TO 請求先住所１Ｗ
017150                 MOVE 市－住所２      TO 請求先住所２Ｗ
020150                 MOVE ZERO            TO 接尾語区分Ｗ
                   END-READ
               ELSE
019500             MOVE SPACE      TO 請求先名称Ｗ
019510             MOVE SPACE      TO 支部名Ｗ
019520             MOVE ZERO       TO 接尾語区分Ｗ
               END-IF
018780     NOT INVALID KEY
018790         MOVE 保－保険者番号  TO 保険者番号Ｗ
018800         MOVE 保－保険者名称  TO 請求先名称Ｗ
018810         MOVE 保－支部部署名  TO 支部名Ｗ
018820         MOVE 保－郵便番号１  TO 請求先郵便番号１Ｗ
018830         MOVE 保－郵便番号２  TO 請求先郵便番号２Ｗ
018840         MOVE 保－住所１      TO 請求先住所１Ｗ
018850         MOVE 保－住所２      TO 請求先住所２Ｗ
018860         MOVE 保－接尾語区分  TO 接尾語区分Ｗ
018870     END-READ.
           IF 保険種別ＷＲ = 85
              MOVE 施術和暦年月ＷＲ TO 生保－施術和暦年月
              MOVE 患者コードＷＲ   TO 生保－患者コード
              READ 生保情報Ｆ
              NOT INVALID KEY
019480           MOVE 生保－負担者番号    TO 保険者番号Ｗ
019490           MOVE 生保－生保市町村名  TO 請求先名称Ｗ
019500           MOVE SPACE               TO 支部名Ｗ
019510           MOVE 生保－生保送付先郵便番号１  TO 請求先郵便番号１Ｗ
019520           MOVE 生保－生保送付先郵便番号２  TO 請求先郵便番号２Ｗ
020220           MOVE 生保－生保送付先住所１      TO 請求先住所１Ｗ
020230           MOVE 生保－生保送付先住所２      TO 請求先住所２Ｗ
                 MOVE SPACE              TO 記号Ｗ
                 MOVE 生保－生保記号番号 TO 番号Ｗ
              END-READ
           END-IF.
      *
022000* 自賠責情報
022010     IF 保険種別ＷＲ = 80
022020        MOVE 施術和暦年月ＷＲ TO 自賠－施術和暦年月
022030        MOVE 患者コードＷＲ   TO 自賠－患者コード
022040        READ 自賠責情報Ｆ
022050        INVALID KEY
022060           MOVE SPACE TO 自賠－レコード
022070           INITIALIZE    自賠－レコード
022080        END-READ
022090        MOVE 自賠－保険会社番号    TO 保険会－保険会社番号
022040        READ 保険会社マスタ
022050        NOT INVALID KEY
022060           MOVE 保険会－保険会社名 TO 請求先名称Ｗ
019500           MOVE SPACE              TO 支部名Ｗ
                 IF 自賠－保険会社送付先住所１ = SPACE
022450               MOVE 保険会－郵便番号１ TO 請求先郵便番号１Ｗ
022460               MOVE 保険会－郵便番号２ TO 請求先郵便番号２Ｗ
022470               MOVE 保険会－住所１     TO 請求先住所１Ｗ
022480               MOVE 保険会－住所２     TO 請求先住所２Ｗ
                  ELSE
022450               MOVE 自賠－保険会社送付先郵便番号１ TO 請求先郵便番号１Ｗ
022460               MOVE 自賠－保険会社送付先郵便番号２ TO 請求先郵便番号２Ｗ
019580               MOVE 自賠－保険会社送付先住所１     TO 請求先住所１Ｗ
019590               MOVE 自賠－保険会社送付先住所２     TO 請求先住所２Ｗ
                  END-IF
022080        END-READ
022170     END-IF.
017490*
018880     EVALUATE 保険種別ＷＲ
018890     WHEN 2
018900     WHEN 6
018910         IF 接尾語区分Ｗ = 1
018920            MOVE SPACE              TO 印刷保険者呼名Ｗ
018930         ELSE
018940            MOVE "社会保険事務所"   TO 印刷保険者呼名Ｗ
018950         END-IF
018960     WHEN 3
018970         MOVE "健康保険組合"   TO 印刷保険者呼名Ｗ
018980     WHEN OTHER
018990         MOVE SPACE TO 印刷保険者呼名Ｗ
019000     END-EVALUATE.
019010     IF 印刷保険者呼名Ｗ NOT = SPACE
019020         STRING 請求先名称Ｗ DELIMITED BY SPACE
019030                保険者呼名Ｗ DELIMITED BY SPACE
019040           INTO 請求先名称Ｗ
019050         END-STRING
019060     END-IF.
019070*組合と共済は支部名も印字する
019080     EVALUATE 保険種別ＷＲ
019090     WHEN 03
019100     WHEN 04
019110         STRING 請求先名称Ｗ DELIMITED BY SPACE
019120                "  "         DELIMITED BY SIZE
019130                支部名Ｗ     DELIMITED BY SPACE
019140           INTO 請求先名支部名Ｗ
019150         END-STRING
019160     WHEN OTHER
019170         MOVE 請求先名称Ｗ TO 請求先名支部名Ｗ
019180     END-EVALUATE.
019190*================================================================*
019200 負傷データ取得 SECTION.
019210*
019220**************************************************
019230* 連結データから負傷データＦより以下の情報を取得 *
019240* ● 負傷名...部位＋負傷種別にて加工して格納     *
019250* ● 負傷年.......負傷年Ｗ                       *
019260* ● 負傷月.......負傷月Ｗ                       *
019270* ● 負傷日.......負傷日Ｗ                       *
019280* ● 施術終了年...終了年Ｗ                       *
019290* ● 施術終了月...終了月Ｗ                       *
019300* ● 施術終了日...終了日Ｗ                       *
019310**************************************************
019320     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
019330     MOVE 施術年ＷＲ         TO 負－施術年.
019340     MOVE 施術月ＷＲ         TO 負－施術月.
019350     MOVE 患者コードＷＲ     TO 負－患者コード.
019360     READ 負傷データＦ
019370     INVALID KEY
019380         CONTINUE
019390*            /* ありえない */
019400     NOT INVALID KEY
019410         MOVE ZERO                         TO 部位数Ｗ
019420         MOVE 負－部位数                   TO 部位数Ｗ
019430         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
019440                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
019450*********************************************
019460* 注）全柔...負傷種別＋部位にて加工して格納 *
019470*********************************************
019480* 負傷種別
019490             MOVE SPACE                     TO 負傷名称Ｗ
019500             MOVE 03                        TO 名－区分コード
019510             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
019520             READ 名称マスタ
019530             INVALID KEY
019540                 MOVE SPACE    TO 負傷名称Ｗ
019550             NOT INVALID KEY
019560*                 MOVE 名－略称 TO 負傷名称Ｗ
019570                 MOVE 名－正式名称 TO 負傷名称Ｗ
019580             END-READ
019590* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
019610             PERFORM 名称埋込処理
019670             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
019680             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
019690             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
019700*
019710             MOVE 負－開始年(部位ＣＮＴ)   TO 施術開始年Ｗ(部位ＣＮＴ)
019720             MOVE 負－開始月(部位ＣＮＴ)   TO 施術開始月Ｗ(部位ＣＮＴ)
019730             MOVE 負－開始日(部位ＣＮＴ)   TO 施術開始日Ｗ(部位ＣＮＴ)
019740*
019750             MOVE 負－終了年(部位ＣＮＴ)   TO 施術終了年Ｗ(部位ＣＮＴ)
019760             MOVE 負－終了月(部位ＣＮＴ)   TO 施術終了月Ｗ(部位ＣＮＴ)
019770             MOVE 負－終了日(部位ＣＮＴ)   TO 施術終了日Ｗ(部位ＣＮＴ)
019780*転帰
019790             EVALUATE 負－転帰区分(部位ＣＮＴ)
019800             WHEN 1
019810             WHEN 2
019820                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
019830             WHEN 3
019840                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
019850             WHEN 4
019860                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
019870             END-EVALUATE
019880*             IF 負－転帰区分(部位ＣＮＴ) NOT = 9
019890*                 MOVE 04                        TO 名－区分コード
019900*                 MOVE 負－転帰区分(部位ＣＮＴ)  TO 名－名称コード
019910*                 READ 名称マスタ
019920*                 INVALID KEY
019930*                     MOVE SPACE    TO 転帰Ｗ(部位ＣＮＴ)
019940*                 NOT INVALID KEY
019950*                     MOVE 名－略称 TO 転帰Ｗ(部位ＣＮＴ)
019960*                 END-READ
019970*             END-IF
019980*TEST---------------------------------
019990*         MOVE NC"左指節関節捻挫左指節関節捻挫骨折挫傷" TO 負傷名Ｗ(部位ＣＮＴ)
020000*         MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
020010*         MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
020020*         MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
020030*-------------------------------------
020040         END-PERFORM
020050     END-READ.
020060*================================================================*
020070 施術記録取得 SECTION.
020080*
020090************************************************************
020100* 連結データから負傷データＦより以下の情報を取得           *
020110* ● 施術開始年月日...該当する部位に対して当月最初の施術日 *
020120************************************************************
020130     MOVE 施術和暦ＷＲ  TO 施記－施術和暦.
020140     MOVE 施術年ＷＲ    TO 施記－施術年.
020150     MOVE 施術月ＷＲ    TO 施記－施術月.
020160     MOVE ZERO            TO 施記－施術日.
020170     MOVE ZERO            TO 施記－患者番号.
020180     MOVE SPACE           TO 施記－枝番.
020190     START 施術記録Ｆ   KEY IS >= 施記－施術和暦年月日
020200                                  施記－患者コード.
020210     IF 状態キー = "00"
020220         MOVE SPACE  TO 終了フラグ２
020230         PERFORM 施術記録Ｆ読込
020240         PERFORM UNTIL ( 終了フラグ２       = "YES" ) OR
020250                       ( 施記－施術和暦 NOT = 施術和暦ＷＲ ) OR
020260                       ( 施記－施術年   NOT = 施術年ＷＲ   ) OR
020270                       ( 施記－施術月   NOT = 施術月ＷＲ   )
020280*            **************
020290*            * 開始年月日 *
020300*            **************
020310             PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
020320                     UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
020330            EVALUATE TRUE ALSO TRUE ALSO TRUE ALSO TRUE ALSO TRUE
020340            WHEN 施記－負傷種別(部位ＣＮＴ) = 負傷種別Ｗ(部位ＣＮＴ) ALSO
020350                 施記－部位(部位ＣＮＴ)     = 部位Ｗ(部位ＣＮＴ)     ALSO
020360                 施記－左右区分(部位ＣＮＴ) = 左右区分Ｗ(部位ＣＮＴ) ALSO
020370                 施記－負傷位置番号(部位ＣＮＴ)
020380                                       = 負傷位置番号Ｗ(部位ＣＮＴ)  ALSO
020390                 開始年月日取得フラグ(部位ＣＮＴ) = SPACE
020400                   MOVE 施記－施術年     TO 施術開始年Ｗ(部位ＣＮＴ)
020410                   MOVE 施記－施術月     TO 施術開始月Ｗ(部位ＣＮＴ)
020420                   MOVE 施記－施術日     TO 施術開始日Ｗ(部位ＣＮＴ)
020430                   MOVE "YES"       TO 開始年月日取得フラグ(部位ＣＮＴ)
020440*
020450            WHEN OTHER
020460                CONTINUE
020470            END-EVALUATE
020480             END-PERFORM
020490             PERFORM 施術記録Ｆ読込
020500         END-PERFORM
020510     END-IF.
020520*================================================================*
020530 施術記録Ｆ読込 SECTION.
020540*
020550     READ 施術記録Ｆ NEXT
020560     AT END
020570         MOVE "YES" TO 終了フラグ２
020580     END-READ.
020590*
020600*================================================================*
020610 負傷原因取得 SECTION.
020620*
020630********************************************************************
020640*  負傷原因コードが同じものは、1行にまとめて印字する。
020650*  例: ①② 家で転んだ.
020660*     負傷原因コードが同じものをまとめ、テーブルにセット
020670*     (ただし、部位を飛んで同じものは、2行になる)
020680*------------------------------------------------------------------*
020690*  ４部位までを印字するため、UNTIL条件に ( 部位ＣＮＴ > 4 ) を加える
020700*    ( "⑤"の付いた負傷原因は作成しない ）
020710********************************************************************
020720     MOVE  ZERO   TO  カウンタ カウンタ２.
020730     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
020740             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
020750                OR ( 部位ＣＮＴ > 4 )
020760*
020770****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
020780        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
020790*
020800           IF カウンタ = ZERO
020810               MOVE 1   TO  カウンタ カウンタ２
020820               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
020830               MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
020840               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
020850           ELSE
020860              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
020870                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
020880                 COMPUTE カウンタ２ = カウンタ２  +  1
020890                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
020900              ELSE
020910                 COMPUTE カウンタ = カウンタ  +  1
020920                 MOVE 1   TO  カウンタ２
020930                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
020940                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
020950                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
020960              END-IF
020970           END-IF
020980        END-IF
020990     END-PERFORM.
021000**************************************************************************
021010*  負傷原因マスタより文章取得
021020**************************************************************************
021030     MOVE  ZERO   TO  カウンタ カウンタ２.
021040     PERFORM VARYING カウンタ FROM 1 BY 1
021050             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
021060** 健保は 区分 01
021070*         MOVE 01                        TO 負原－区分コード
      */健保：０１、労災：０２、自賠責：０３
               EVALUATE 受－保険種別
               WHEN 70
                   MOVE 2 TO 負原－区分コード
               WHEN 80
                   MOVE 3 TO 負原－区分コード
               WHEN OTHER
                   MOVE 1 TO 負原－区分コード
               END-EVALUATE
021080         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
021090         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
021100         READ 負傷原因Ｆ
021110         NOT INVALID KEY
021120             INITIALIZE 負傷原因ＷＴ
021130             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
021140             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
021150             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
021160             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
021170             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
021180             PERFORM VARYING カウンタ２ FROM 1 BY 1
021190                     UNTIL ( カウンタ２ > 9 )  OR 
021200                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
021210                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
021220                WHEN 1
021230                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
021240                WHEN 2
021250                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
021260                WHEN 3
021270                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
021280                WHEN 4
021290                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
021300                WHEN 5
021310                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
021320                WHEN OTHER
021330                   CONTINUE
021340                END-EVALUATE
021350             END-PERFORM
021360*
021442             IF 負原－負傷原因入力区分 = 1
021443                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
021444                        負傷原因１ＷＴ  DELIMITED BY SIZE
021445                        負傷原因２ＷＴ  DELIMITED BY SIZE
021446                        負傷原因３ＷＴ  DELIMITED BY SIZE
021447                        負傷原因４ＷＴ  DELIMITED BY SIZE
021448                        負傷原因５ＷＴ  DELIMITED BY SIZE
021449                        INTO 負傷原因内容合成Ｗ(カウンタ)
021450                 END-STRING
021451             ELSE
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
021460             END-IF
021461*
021462         END-READ
021463     END-PERFORM.
021470*
021480     PERFORM 負傷原因セット.
021490*
021500*================================================================*
021510 負傷原因セット SECTION.
021520*
021530**************************************************************************
021540*  文章が1行を超える時は、複数行に分解する。
021550**************************************************************************
021560     MOVE  ZERO   TO  カウンタ カウンタ２.
021570     PERFORM VARYING カウンタ FROM 1 BY 1
021580             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
021590*
021600          INITIALIZE 負傷原因内容分解ＸＷ
021610          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
021620          IF  負傷原因内容１ＸＷ  NOT = SPACE
021630              COMPUTE カウンタ２ = カウンタ２  +  1
021640              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
021650          END-IF
021660          IF  負傷原因内容２ＸＷ  NOT = SPACE
021670              COMPUTE カウンタ２ = カウンタ２  +  1
021680              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
021690          END-IF
021700          IF  負傷原因内容３ＸＷ  NOT = SPACE
021710              COMPUTE カウンタ２ = カウンタ２  +  1
021720              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
021730          END-IF
021740          IF  負傷原因内容４ＸＷ  NOT = SPACE
021750              COMPUTE カウンタ２ = カウンタ２  +  1
021760              MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
021770          END-IF
021780          IF  負傷原因内容５ＸＷ  NOT = SPACE
021790              COMPUTE カウンタ２ = カウンタ２  +  1
021800              MOVE 負傷原因内容５ＸＷ  TO 負傷原因Ｗ(カウンタ２)
021810          END-IF
021820          IF  負傷原因内容６ＸＷ  NOT = SPACE
021830              COMPUTE カウンタ２ = カウンタ２  +  1
021840              MOVE 負傷原因内容６ＸＷ  TO 負傷原因Ｗ(カウンタ２)
021850          END-IF
021860          IF  負傷原因内容７ＸＷ  NOT = SPACE
021870              COMPUTE カウンタ２ = カウンタ２  +  1
021880              MOVE 負傷原因内容７ＸＷ  TO 負傷原因Ｗ(カウンタ２)
021890          END-IF
021900          IF  負傷原因内容８ＸＷ  NOT = SPACE
021910              COMPUTE カウンタ２ = カウンタ２  +  1
021920              MOVE 負傷原因内容８ＸＷ  TO 負傷原因Ｗ(カウンタ２)
021930          END-IF
021820          IF  負傷原因内容９ＸＷ  NOT = SPACE
021830              COMPUTE カウンタ２ = カウンタ２  +  1
021840              MOVE 負傷原因内容９ＸＷ  TO 負傷原因Ｗ(カウンタ２)
021850          END-IF
021860          IF  負傷原因内容１０ＸＷ  NOT = SPACE
021870              COMPUTE カウンタ２ = カウンタ２  +  1
021880              MOVE 負傷原因内容１０ＸＷ  TO 負傷原因Ｗ(カウンタ２)
021890          END-IF
021900          IF  負傷原因内容１１ＸＷ  NOT = SPACE
021910              COMPUTE カウンタ２ = カウンタ２  +  1
021920              MOVE 負傷原因内容１１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
021930          END-IF
021940*
021950     END-PERFORM.
021960*
021970*================================================================*
021980 印刷処理 SECTION.
021990*
022000     EVALUATE 印刷モードＦＷＲ
022010     WHEN 0
022020         PERFORM 印刷処理２
022030         PERFORM 印刷処理３
022040     WHEN 1
022050         PERFORM 印刷処理２
022060     WHEN 2
022070         PERFORM 印刷処理３
022080     WHEN 3
022090         PERFORM 印刷処理４
022100     WHEN 4
022110         IF 連印－保険証印刷区分 = 1
022120             PERFORM 印刷処理２
022130         END-IF
022140         PERFORM 印刷処理３
022150         PERFORM 印刷処理４
022160     END-EVALUATE.
022170*================================================================*
022180 印刷処理１ SECTION.
022190*
022200     MOVE "YCK6621P" TO  定義体名Ｐ.
022210*     MOVE "SCREEN"  TO  項目群名Ｐ.
022220     MOVE "GRP001"  TO  項目群名Ｐ.
022230     WRITE YCK6621P.
022240*     WRITE 印刷レコード.
022250     PERFORM エラー処理Ｐ.
022260*================================================================*
022270 印刷処理２ SECTION.
022280*
022290     MOVE "YCK6621P"  TO  定義体名Ｐ.
022300     MOVE "GRP001"   TO  項目群名Ｐ.
022310     WRITE YCK6621P.
022320*     WRITE 印刷レコード.
022330     PERFORM エラー処理Ｐ.
022340*================================================================*
022350 印刷処理３ SECTION.
022360*
022370     MOVE "YCK6621P"  TO  定義体名Ｐ.
022380     MOVE "GRP002"   TO  項目群名Ｐ.
022390     WRITE YCK6621P.
022400*     WRITE 印刷レコード.
022410     PERFORM エラー処理Ｐ.
022420*================================================================*
022430 印刷処理４ SECTION.
022440*
022450     MOVE "YCK6621P"  TO  定義体名Ｐ.
022460     MOVE "GRP003"   TO  項目群名Ｐ.
022470     WRITE YCK6621P.
022480     PERFORM エラー処理Ｐ.
022490*================================================================*
022500 エラー処理Ｐ SECTION.
022510*
022520     IF 通知情報Ｐ NOT = "00"
022530         DISPLAY NC"帳票エラー"              UPON CONS
022540         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
022550         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
022560         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
022570         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
022580                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
022590         ACCEPT  キー入力 FROM CONS
022600         PERFORM ファイル閉鎖
022610         EXIT PROGRAM
022620     END-IF.
022630*================================================================*
022640 名称埋込処理 SECTION.
022650*
           EVALUATE 受－保険種別
           WHEN 05
               MOVE 2          TO レセ－レセ種別
           WHEN 70
               MOVE 4          TO レセ－レセ種別
           WHEN 80
               MOVE 5          TO レセ－レセ種別
           WHEN 85
               MOVE 7          TO レセ－レセ種別
           WHEN 90
               MOVE 6          TO レセ－レセ種別
           WHEN 91
               MOVE 8          TO レセ－レセ種別
           WHEN OTHER
               MOVE 1          TO レセ－レセ種別
           END-EVALUATE.
019550     MOVE 受－施術和暦 TO レセ－施術和暦.
019560     MOVE 受－施術年   TO レセ－施術年.
019570     MOVE 受－施術月   TO レセ－施術月.
019580     MOVE 受－患者番号 TO レセ－患者番号.
019590     MOVE 受－枝番     TO レセ－枝番.
019600     READ レセプトＦ
019630     NOT INVALID KEY
006480*
006490         STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
006500                レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
009980                負傷名称Ｗ                    DELIMITED BY SPACE
006520           INTO 負傷名Ｗ(部位ＣＮＴ)
006570         END-STRING
022760     END-READ.
022770*
023060*================================================================*
023070 合計金額行セット SECTION.
023080*
           IF (受－助成種別 >= 50) AND (受－助成種別 >= 60)
               MOVE 3          TO レセ－レセ種別
           ELSE
               EVALUATE 受－保険種別
               WHEN 05
                   MOVE 2      TO レセ－レセ種別
               WHEN 70
                   MOVE 4      TO レセ－レセ種別
               WHEN 80
                   MOVE 5      TO レセ－レセ種別
               WHEN 85
                   MOVE 7      TO レセ－レセ種別
               WHEN 90
                   MOVE 6      TO レセ－レセ種別
               WHEN 91
                   MOVE 8      TO レセ－レセ種別
               WHEN OTHER
                   MOVE 1      TO レセ－レセ種別
               END-EVALUATE
           END-IF.
019550     MOVE 受－施術和暦 TO レセ－施術和暦.
019560     MOVE 受－施術年   TO レセ－施術年.
019570     MOVE 受－施術月   TO レセ－施術月.
019580     MOVE 受－患者番号 TO レセ－患者番号.
019590     MOVE 受－枝番     TO レセ－枝番.
019600     READ レセプトＦ
019630     NOT INVALID KEY
024670        MOVE レセ－合計     TO 費用額(段数ＷＲ)
              IF 受－助成種別 = ZERO
024800           MOVE レセ－一部負担金   TO 負担額(段数ＷＲ)
024810           MOVE レセ－請求金額 TO 請求額(段数ＷＲ)
              ELSE
024800           MOVE レセ－受給者負担額 TO 負担額(段数ＷＲ)
024810           COMPUTE 請求額(段数ＷＲ) = レセ－助成請求金額 + レセ－請求金額
              END-IF
           END-READ.
023090     MOVE 提出年ＷＲ   TO 提出年    (段数ＷＲ).
023100     MOVE 提出月ＷＲ   TO 提出月    (段数ＷＲ).
023110     MOVE 提出日ＷＲ   TO 提出日    (段数ＷＲ).
023120     MOVE 施術年ＷＲ   TO 請求開始年(段数ＷＲ) 請求終了年(段数ＷＲ).
023130     MOVE 施術月ＷＲ   TO 請求開始月(段数ＷＲ) 請求終了月(段数ＷＲ).
023140     MOVE 1            TO 請求開始日(段数ＷＲ).
023150     MOVE 請求終了日Ｗ TO 請求終了日(段数ＷＲ).
023190*================================================================*
023200  請求期間取得 SECTION.
023210*
023220     EVALUATE 施術月ＷＲ
023230     WHEN 4
023240     WHEN 6
023250     WHEN 9
023260     WHEN 11
023270         MOVE 30 TO 請求終了日Ｗ
023280     WHEN 2
023290         COMPUTE 対象西暦Ｗ = 和暦終了年Ｗ + 施術年ＷＲ
023300         DIVIDE 4 INTO 対象西暦Ｗ GIVING    商Ｗ
023310                                  REMAINDER 余Ｗ
023320         END-DIVIDE
023330         IF 余Ｗ = ZERO
023340             MOVE 29 TO 請求終了日Ｗ
023350         ELSE
023360             MOVE 28 TO 請求終了日Ｗ
023370         END-IF
023380     WHEN 1
023390     WHEN 3
023400     WHEN 5
023410     WHEN 7
023420     WHEN 8
023430     WHEN 10
023440     WHEN 12
023450         MOVE 31 TO 請求終了日Ｗ
023460     WHEN OTHER
023470         MOVE  NC"未知のエラー発生" TO 連メ－メッセージ
023480         CALL   "MSG001"
023490         CANCEL "MSG001"
023500     END-EVALUATE.
023510*================================================================*
023520 印刷セット５ SECTION.
023530*
023540*/詳細モード用
023550     INITIALIZE YCK6621P.
023560     INITIALIZE 受診者情報Ｗ.
023570     INITIALIZE 事業所情報Ｗ.
023580     INITIALIZE 請求先情報Ｗ.
023590     INITIALIZE 負傷情報Ｗ.
023600     INITIALIZE 負担率チェックＷ.
023610     PERFORM 負担率取得.
023620     PERFORM 請求先情報取得.
023630     PERFORM 受診者情報取得.
023640     PERFORM 負傷データ取得.
023650*
023660     IF 負傷原因印刷区分Ｗ  NOT = 1
023670        PERFORM 負傷原因取得
023680     END-IF.
023690*
023700     IF 連印－保険証印刷区分 = 1
023710         PERFORM 印刷上部セット
023720     END-IF.
023730     PERFORM 印刷下部セット詳細.
023740*================================================================*
023750 印刷下部セット詳細 SECTION.
023760*
023770********************
023780* 負傷データセット *
023790********************
023800**********
023810* １部位 *
023820**********
023830     IF 連印－負傷印刷行(1) NOT = ZERO
023840         IF 連印－部位印刷区分(1) = 1
023850             MOVE 負傷名Ｗ(1)         TO 負傷名ＳＷ      (連印－負傷印刷行(1))
023860             MOVE 負傷年Ｗ(1)         TO 負傷年ＳＷ      (連印－負傷印刷行(1))
023870             MOVE 負傷月Ｗ(1)         TO 負傷月ＳＷ      (連印－負傷印刷行(1))
023880             MOVE 負傷日Ｗ(1)         TO 負傷日ＳＷ      (連印－負傷印刷行(1))
023890             MOVE 施術開始年Ｗ(1)     TO 施術開始年ＳＷ  (連印－負傷印刷行(1))
023900             MOVE 施術開始月Ｗ(1)     TO 施術開始月ＳＷ  (連印－負傷印刷行(1))
023910             MOVE 施術開始日Ｗ(1)     TO 施術開始日ＳＷ  (連印－負傷印刷行(1))
023920             MOVE 施術開始年Ｗ(1)     TO 施術開始年ＳＷ  (連印－負傷印刷行(1))
023930             MOVE 施術開始月Ｗ(1)     TO 施術開始月ＳＷ  (連印－負傷印刷行(1))
023940             MOVE 施術開始日Ｗ(1)     TO 施術開始日ＳＷ  (連印－負傷印刷行(1))
023950         END-IF
023960         IF 連印－転帰印刷区分(1) = 1
023970             MOVE 施術終了年Ｗ(1)     TO 施術終了年ＳＷ  (連印－負傷印刷行(1))
023980             MOVE 施術終了月Ｗ(1)     TO 施術終了月ＳＷ  (連印－負傷印刷行(1))
023990             MOVE 施術終了日Ｗ(1)     TO 施術終了日ＳＷ  (連印－負傷印刷行(1))
024000             MOVE 治癒チェックＷ(1)   TO 治癒チェックＳＷ(連印－負傷印刷行(1))
024010             MOVE 中止チェックＷ(1)   TO 中止チェックＳＷ(連印－負傷印刷行(1))
024020             MOVE 転医チェックＷ(1)   TO 転医チェックＳＷ(連印－負傷印刷行(1))
024030         END-IF
024040     END-IF.
024050**********
024060* ２部位 *
024070**********
024080     IF 連印－負傷印刷行(2) NOT = ZERO
024090         IF 連印－部位印刷区分(2) = 1
024100             MOVE 負傷名Ｗ(2)         TO 負傷名ＳＷ      (連印－負傷印刷行(2))
024110             MOVE 負傷年Ｗ(2)         TO 負傷年ＳＷ      (連印－負傷印刷行(2))
024120             MOVE 負傷月Ｗ(2)         TO 負傷月ＳＷ      (連印－負傷印刷行(2))
024130             MOVE 負傷日Ｗ(2)         TO 負傷日ＳＷ      (連印－負傷印刷行(2))
024140             MOVE 施術開始年Ｗ(2)     TO 施術開始年ＳＷ  (連印－負傷印刷行(2))
024150             MOVE 施術開始月Ｗ(2)     TO 施術開始月ＳＷ  (連印－負傷印刷行(2))
024160             MOVE 施術開始日Ｗ(2)     TO 施術開始日ＳＷ  (連印－負傷印刷行(2))
024170             MOVE 施術開始年Ｗ(2)     TO 施術開始年ＳＷ  (連印－負傷印刷行(2))
024180             MOVE 施術開始月Ｗ(2)     TO 施術開始月ＳＷ  (連印－負傷印刷行(2))
024190             MOVE 施術開始日Ｗ(2)     TO 施術開始日ＳＷ  (連印－負傷印刷行(2))
024200         END-IF
024210         IF 連印－転帰印刷区分(2) = 1
024220             MOVE 施術終了年Ｗ(2)     TO 施術終了年ＳＷ  (連印－負傷印刷行(2))
024230             MOVE 施術終了月Ｗ(2)     TO 施術終了月ＳＷ  (連印－負傷印刷行(2))
024240             MOVE 施術終了日Ｗ(2)     TO 施術終了日ＳＷ  (連印－負傷印刷行(2))
024250             MOVE 治癒チェックＷ(2)   TO 治癒チェックＳＷ(連印－負傷印刷行(2))
024260             MOVE 中止チェックＷ(2)   TO 中止チェックＳＷ(連印－負傷印刷行(2))
024270             MOVE 転医チェックＷ(2)   TO 転医チェックＳＷ(連印－負傷印刷行(2))
024280     END-IF.
024290**********
024300* ３部位 *
024310**********
024320     IF 連印－負傷印刷行(3) NOT = ZERO
024330         IF 連印－部位印刷区分(3) = 1
024340             MOVE 負傷名Ｗ(3)         TO 負傷名ＳＷ      (連印－負傷印刷行(3))
024350             MOVE 負傷年Ｗ(3)         TO 負傷年ＳＷ      (連印－負傷印刷行(3))
024360             MOVE 負傷月Ｗ(3)         TO 負傷月ＳＷ      (連印－負傷印刷行(3))
024370             MOVE 負傷日Ｗ(3)         TO 負傷日ＳＷ      (連印－負傷印刷行(3))
024380             MOVE 施術開始年Ｗ(3)     TO 施術開始年ＳＷ  (連印－負傷印刷行(3))
024390             MOVE 施術開始月Ｗ(3)     TO 施術開始月ＳＷ  (連印－負傷印刷行(3))
024400             MOVE 施術開始日Ｗ(3)     TO 施術開始日ＳＷ  (連印－負傷印刷行(3))
024410             MOVE 施術開始年Ｗ(3)     TO 施術開始年ＳＷ  (連印－負傷印刷行(3))
024420             MOVE 施術開始月Ｗ(3)     TO 施術開始月ＳＷ  (連印－負傷印刷行(3))
024430             MOVE 施術開始日Ｗ(3)     TO 施術開始日ＳＷ  (連印－負傷印刷行(3))
024440         END-IF
024450         IF 連印－転帰印刷区分(3) = 1
024460             MOVE 施術終了年Ｗ(3)     TO 施術終了年ＳＷ  (連印－負傷印刷行(3))
024470             MOVE 施術終了月Ｗ(3)     TO 施術終了月ＳＷ  (連印－負傷印刷行(3))
024480             MOVE 施術終了日Ｗ(3)     TO 施術終了日ＳＷ  (連印－負傷印刷行(3))
024490             MOVE 治癒チェックＷ(3)   TO 治癒チェックＳＷ(連印－負傷印刷行(3))
024500             MOVE 中止チェックＷ(3)   TO 中止チェックＳＷ(連印－負傷印刷行(3))
024510             MOVE 転医チェックＷ(3)   TO 転医チェックＳＷ(連印－負傷印刷行(3))
024520     END-IF.
024530**********
024540* ４部位 *
024550**********
024560     IF 連印－負傷印刷行(4) NOT = ZERO
024570         IF 連印－部位印刷区分(4) = 1
024580             MOVE 負傷名Ｗ(4)         TO 負傷名ＳＷ      (連印－負傷印刷行(4))
024590             MOVE 負傷年Ｗ(4)         TO 負傷年ＳＷ      (連印－負傷印刷行(4))
024600             MOVE 負傷月Ｗ(4)         TO 負傷月ＳＷ      (連印－負傷印刷行(4))
024610             MOVE 負傷日Ｗ(4)         TO 負傷日ＳＷ      (連印－負傷印刷行(4))
024620             MOVE 施術開始年Ｗ(4)     TO 施術開始年ＳＷ  (連印－負傷印刷行(4))
024630             MOVE 施術開始月Ｗ(4)     TO 施術開始月ＳＷ  (連印－負傷印刷行(4))
024640             MOVE 施術開始日Ｗ(4)     TO 施術開始日ＳＷ  (連印－負傷印刷行(4))
024650             MOVE 施術開始年Ｗ(4)     TO 施術開始年ＳＷ  (連印－負傷印刷行(4))
024660             MOVE 施術開始月Ｗ(4)     TO 施術開始月ＳＷ  (連印－負傷印刷行(4))
024670             MOVE 施術開始日Ｗ(4)     TO 施術開始日ＳＷ  (連印－負傷印刷行(4))
024680         END-IF
024690         IF 連印－転帰印刷区分(4) = 1
024700             MOVE 施術終了年Ｗ(4)     TO 施術終了年ＳＷ  (連印－負傷印刷行(4))
024710             MOVE 施術終了月Ｗ(4)     TO 施術終了月ＳＷ  (連印－負傷印刷行(4))
024720             MOVE 施術終了日Ｗ(4)     TO 施術終了日ＳＷ  (連印－負傷印刷行(4))
024730             MOVE 治癒チェックＷ(4)   TO 治癒チェックＳＷ(連印－負傷印刷行(4))
024740             MOVE 中止チェックＷ(4)   TO 中止チェックＳＷ(連印－負傷印刷行(4))
024750             MOVE 転医チェックＷ(4)   TO 転医チェックＳＷ(連印－負傷印刷行(4))
024760     END-IF.
024770**********
024780* １部位 *
024790**********
024800     MOVE 負傷名ＳＷ(1)         TO 負傷名１       .
024810     MOVE 負傷年ＳＷ(1)         TO 負傷年１       .
024820     MOVE 負傷月ＳＷ(1)         TO 負傷月１       .
024830     MOVE 負傷日ＳＷ(1)         TO 負傷日１       .
024840     MOVE 施術開始年ＳＷ(1)     TO 施術開始年１   .
024850     MOVE 施術開始月ＳＷ(1)     TO 施術開始月１   .
024860     MOVE 施術開始日ＳＷ(1)     TO 施術開始日１   .
024870     MOVE 施術開始年ＳＷ(1)     TO 施術期間自年１ .
024880     MOVE 施術開始月ＳＷ(1)     TO 施術期間自月１ .
024890     MOVE 施術開始日ＳＷ(1)     TO 施術期間自日１ .
024900     MOVE 施術終了年ＳＷ(1)     TO 施術終了年１   .
024910     MOVE 施術終了月ＳＷ(1)     TO 施術終了月１   .
024920     MOVE 施術終了日ＳＷ(1)     TO 施術終了日１   .
024930     MOVE 治癒チェックＳＷ(1)   TO 治癒チェック１ .
024940     MOVE 中止チェックＳＷ(1)   TO 中止チェック１ .
024950     MOVE 転医チェックＳＷ(1)   TO 転医チェック１ .
024960**********
024970* ２部位 *
024980**********
024990     MOVE 負傷名ＳＷ(2)         TO 負傷名２       .
025000     MOVE 負傷年ＳＷ(2)         TO 負傷年２       .
025010     MOVE 負傷月ＳＷ(2)         TO 負傷月２       .
025020     MOVE 負傷日ＳＷ(2)         TO 負傷日２       .
025030     MOVE 施術開始年ＳＷ(2)     TO 施術開始年２   .
025040     MOVE 施術開始月ＳＷ(2)     TO 施術開始月２   .
025050     MOVE 施術開始日ＳＷ(2)     TO 施術開始日２   .
025060     MOVE 施術開始年ＳＷ(2)     TO 施術期間自年２ .
025070     MOVE 施術開始月ＳＷ(2)     TO 施術期間自月２ .
025080     MOVE 施術開始日ＳＷ(2)     TO 施術期間自日２ .
025090     MOVE 施術終了年ＳＷ(2)     TO 施術終了年２   .
025100     MOVE 施術終了月ＳＷ(2)     TO 施術終了月２   .
025110     MOVE 施術終了日ＳＷ(2)     TO 施術終了日２   .
025120     MOVE 治癒チェックＳＷ(2)   TO 治癒チェック２ .
025130     MOVE 中止チェックＳＷ(2)   TO 中止チェック２ .
025140     MOVE 転医チェックＳＷ(2)   TO 転医チェック２ .
025150**********
025160* ３部位 *
025170**********
025180     MOVE 負傷名ＳＷ(3)         TO 負傷名３       .
025190     MOVE 負傷年ＳＷ(3)         TO 負傷年３       .
025200     MOVE 負傷月ＳＷ(3)         TO 負傷月３       .
025210     MOVE 負傷日ＳＷ(3)         TO 負傷日３       .
025220     MOVE 施術開始年ＳＷ(3)     TO 施術開始年３   .
025230     MOVE 施術開始月ＳＷ(3)     TO 施術開始月３   .
025240     MOVE 施術開始日ＳＷ(3)     TO 施術開始日３   .
025250     MOVE 施術開始年ＳＷ(3)     TO 施術期間自年３ .
025260     MOVE 施術開始月ＳＷ(3)     TO 施術期間自月３ .
025270     MOVE 施術開始日ＳＷ(3)     TO 施術期間自日３ .
025280     MOVE 施術終了年ＳＷ(3)     TO 施術終了年３   .
025290     MOVE 施術終了月ＳＷ(3)     TO 施術終了月３   .
025300     MOVE 施術終了日ＳＷ(3)     TO 施術終了日３   .
025310     MOVE 治癒チェックＳＷ(3)   TO 治癒チェック３ .
025320     MOVE 中止チェックＳＷ(3)   TO 中止チェック３ .
025330     MOVE 転医チェックＳＷ(3)   TO 転医チェック３ .
025340**********
025350* ４部位 *
025360**********
025370     MOVE 負傷名ＳＷ(4)         TO 負傷名４       .
025380     MOVE 負傷年ＳＷ(4)         TO 負傷年４       .
025390     MOVE 負傷月ＳＷ(4)         TO 負傷月４       .
025400     MOVE 負傷日ＳＷ(4)         TO 負傷日４       .
025410     MOVE 施術開始年ＳＷ(4)     TO 施術開始年４   .
025420     MOVE 施術開始月ＳＷ(4)     TO 施術開始月４   .
025430     MOVE 施術開始日ＳＷ(4)     TO 施術開始日４   .
025440     MOVE 施術開始年ＳＷ(4)     TO 施術期間自年４ .
025450     MOVE 施術開始月ＳＷ(4)     TO 施術期間自月４ .
025460     MOVE 施術開始日ＳＷ(4)     TO 施術期間自日４ .
025470     MOVE 施術終了年ＳＷ(4)     TO 施術終了年４   .
025480     MOVE 施術終了月ＳＷ(4)     TO 施術終了月４   .
025490     MOVE 施術終了日ＳＷ(4)     TO 施術終了日４   .
025500     MOVE 治癒チェックＳＷ(4)   TO 治癒チェック４ .
025510     MOVE 中止チェックＳＷ(4)   TO 中止チェック４ .
025520     MOVE 転医チェックＳＷ(4)   TO 転医チェック４ .
025530************
025540* 負傷原因 *
025550************
025870     MOVE  ZERO   TO  カウンタ.
025880     MOVE  ZERO   TO  カウンタ２.
025890     MOVE SPACE TO 負傷原因転記Ｆ.
025900     PERFORM VARYING カウンタ FROM 1 BY 1
025910             UNTIL ( カウンタ > 44 ) OR ( 負傷原因Ｗ(カウンタ) = SPACE )
025920*
025930        IF ( 負傷原因Ｗ(カウンタ)(1:2) = "①" )
025940            IF ( 連印－負傷印刷行(1) NOT = ZERO) AND
025950               ( 連印－原因印刷区分(1)   = 1)
025960                MOVE "YES" TO 負傷原因転記Ｆ
025970                COMPUTE カウンタ２ = (連印－負傷印刷行(1) * 11) - 11
025980            ELSE
025990                MOVE SPACE TO 負傷原因転記Ｆ
026000            END-IF
026010        END-IF
026020*
026030        IF ( 負傷原因Ｗ(カウンタ)(1:2) = "②" )
026040            IF ( 連印－負傷印刷行(2) NOT = ZERO) AND
026050               ( 連印－原因印刷区分(2)   = 1)
026060                MOVE "YES" TO 負傷原因転記Ｆ
026070                COMPUTE カウンタ２ = (連印－負傷印刷行(2) * 11) - 11
026080            ELSE
026090                MOVE SPACE TO 負傷原因転記Ｆ
026100            END-IF
026110        END-IF
026120*
026130        IF ( 負傷原因Ｗ(カウンタ)(1:2) = "③" )
026140            IF ( 連印－負傷印刷行(3) NOT = ZERO) AND
026150               ( 連印－原因印刷区分(3)   = 1)
026160                MOVE "YES" TO 負傷原因転記Ｆ
026170                COMPUTE カウンタ２ = (連印－負傷印刷行(3) * 11) - 11
026180            ELSE
026190                MOVE SPACE TO 負傷原因転記Ｆ
026200            END-IF
026210        END-IF
026220*
026230        IF ( 負傷原因Ｗ(カウンタ)(1:2) = "④" )
026240            IF ( 連印－負傷印刷行(4) NOT = ZERO) AND
026250               ( 連印－原因印刷区分(4)   = 1)
026260                MOVE "YES" TO 負傷原因転記Ｆ
026270                COMPUTE カウンタ２ = (連印－負傷印刷行(4) * 11) - 11
026280            ELSE
026290                MOVE SPACE TO 負傷原因転記Ｆ
026300            END-IF
026310        END-IF
026320*
026330        IF 負傷原因転記Ｆ = "YES"
026340            COMPUTE  カウンタ２  =  カウンタ２  +  1
026350            MOVE 負傷原因Ｗ(カウンタ)  TO 負傷原因(カウンタ２)
026360        END-IF
026370*
026380     END-PERFORM.
027580*================================================================*
027590******************************************************************
027600 END PROGRAM YCK6621.
027610******************************************************************
