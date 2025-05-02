000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHP662.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*         ホープ用
000100*         カルテ印刷（柔+ｳｨﾝﾄﾞｳｽﾞ版）
000110*         MED = YAW660 YHP662P
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2012-08-07
000140 DATE-COMPILED.          2012-08-07
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
000270     SELECT  保険者マスタ    ASSIGN      TO        HOKENSL
000280                             ORGANIZATION             IS  INDEXED
000290                             ACCESS MODE              IS  DYNAMIC
000300                             RECORD KEY               IS  保－保険種別
000310                                                          保－保険者番号
000320* 将来は、キー項目の保険者名称を保険者カナにする
000330                             ALTERNATE RECORD KEY     IS  保－保険種別
000340                                                          保－保険者名称
000350                                                          保－保険者番号
000360                             FILE STATUS              IS  状態キー
000370                             LOCK        MODE         IS  AUTOMATIC.
000380     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000390                             ORGANIZATION             IS  INDEXED
000400                             ACCESS MODE              IS  DYNAMIC
000410                             RECORD KEY               IS  元－元号区分
000420                             FILE STATUS              IS  状態キー
000430                             LOCK        MODE         IS  AUTOMATIC.
000440     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000450                             ORGANIZATION             IS  INDEXED
000460                             ACCESS MODE              IS  DYNAMIC
000470                             RECORD KEY               IS  名－区分コード
000480                                                          名－名称コード
000490                             FILE STATUS              IS  状態キー
000500                             LOCK        MODE         IS  AUTOMATIC.
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
000570     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000580                             ORGANIZATION             IS  INDEXED
000590                             ACCESS MODE              IS  DYNAMIC
000600                             RECORD KEY               IS  制－制御区分
000610                             FILE STATUS              IS  状態キー
000620                             LOCK        MODE         IS  AUTOMATIC.
000690     SELECT  請求先マスタ    ASSIGN      TO        SEIKYUSL
000700                             ORGANIZATION             IS  INDEXED
000710                             ACCESS MODE              IS  DYNAMIC
000720                             RECORD KEY               IS  請先－保険種別
000730                                                          請先－保険者番号
000740                             FILE STATUS              IS  状態キー
000750                             LOCK    MODE             IS  AUTOMATIC.
000830     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000840                             ORGANIZATION             IS  INDEXED
000850                             ACCESS MODE              IS  DYNAMIC
000860                             RECORD KEY               IS  受－施術和暦年月
000870                                                          受－患者コード
000880                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000890                                                          受－患者カナ
000900                                                          受－患者コード
000910                             ALTERNATE RECORD KEY     IS  受－患者コード
000920                                                          受－施術和暦年月
000930                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000940                                                          受－保険種別
000950                                                          受－保険者番号
000960                                                          受－患者コード
000970                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000980                                                          受－公費種別
000990                                                          受－費用負担者番号
001000                                                          受－患者コード
001010                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001020                                                          受－助成種別
001030                                                          受－費用負担者番号助成
001040                                                          受－患者コード
001050                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
001060                                                          受－施術和暦年月
001070                                                          受－患者コード
001080                             FILE STATUS              IS  状態キー
001090                             LOCK        MODE         IS  AUTOMATIC.
001100     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
001110                             ORGANIZATION             IS  INDEXED
001120                             ACCESS MODE              IS  DYNAMIC
001130                             RECORD KEY               IS  施記－施術和暦年月日
001140                                                          施記－患者コード
001150                             ALTERNATE RECORD KEY     IS  施記－患者コード
001160                                                          施記－施術和暦年月日
001170                             FILE STATUS              IS  状態キー
001180                             LOCK        MODE         IS  AUTOMATIC.
001190     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
001200                             ORGANIZATION             IS  INDEXED
001210                             ACCESS MODE              IS  DYNAMIC
001220                             RECORD KEY               IS 負－施術和暦年月
001230                                                         負－患者コード
001240                             ALTERNATE RECORD KEY     IS 負－患者コード
001250                                                         負－施術和暦年月
001260                             FILE STATUS              IS  状態キー
001270                             LOCK        MODE         IS  AUTOMATIC.
001350     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
001360                             ORGANIZATION             IS  INDEXED
001370                             ACCESS MODE              IS  DYNAMIC
001380                             RECORD KEY               IS  負原－区分コード
001390                                                          負原－負傷原因コード
001400                             FILE STATUS              IS  状態キー
001410                             LOCK        MODE         IS  AUTOMATIC.
001420     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
001430                             ORGANIZATION             IS  INDEXED
001440                             ACCESS MODE              IS  DYNAMIC
001450                             RECORD KEY               IS  市－公費種別
001460                                                          市－市町村番号
001470                             ALTERNATE RECORD KEY     IS  市－公費種別
001480                                                          市－市町村名称
001490                                                          市－市町村番号
001500                             FILE STATUS              IS  状態キー
001510                             LOCK        MODE         IS  AUTOMATIC.
000241     SELECT  生保情報Ｆ      ASSIGN      TO        SEIHOJL
000242                             ORGANIZATION             IS INDEXED
000243                             ACCESS MODE              IS DYNAMIC
000244                             RECORD KEY               IS 生保－施術和暦年月
000245                                                         生保－患者コード
000255                             ALTERNATE RECORD KEY     IS 生保－患者コード
000265                                                         生保－施術和暦年月
000277                             FILE STATUS              IS 状態キー
000278                             LOCK        MODE         IS AUTOMATIC.
           SELECT  事業所マスタ    ASSIGN      TO        JIGYOSL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  事－保険種別
                                                                事－保険者番号
                                                                事－記号
                                   FILE STATUS              IS  状態キー
                                   LOCK        MODE         IS  AUTOMATIC.
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
001600     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF002
001610                             SYMBOLIC    DESTINATION  IS "PRT"
001620                             FORMAT                   IS  定義体名Ｐ
001630                             GROUP                    IS  項目群名Ｐ
001640                             PROCESSING  MODE         IS  処理種別Ｐ
001650                             UNIT        CONTROL      IS  拡張制御Ｐ
001660                             FILE        STATUS       IS  通知情報Ｐ.
001670******************************************************************
001680*                      DATA DIVISION                             *
001690******************************************************************
001700 DATA                    DIVISION.
001710 FILE                    SECTION.
001720*                           ［ＲＬ＝  ３２０］
001730 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001740     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
001750*                           ［ＲＬ＝  １２８］
001760 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001770     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001780*                           ［ＲＬ＝  １２８］
001790 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
001800     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
001840*                           ［ＲＬ＝  ２５６］
001850 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001860     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
001900*                           ［ＲＬ＝  １２８］
001910 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
001920     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
001960*                           ［ＲＬ＝  ３２０］
001970 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
001980     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
001990*                           ［ＲＬ＝  ２５６］
002000 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
002010     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
002020*                           ［ＲＬ＝  １２８］
002030 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002040     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
002080*                           ［ＲＬ＝  １２８］
002090 FD  負傷原因Ｆ          BLOCK   CONTAINS   1   RECORDS.
002100     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
002110*                           ［ＲＬ＝  ２５６］
002120 FD  市町村マスタ        BLOCK   CONTAINS   1   RECORDS.
002130     COPY SITYOSN         OF  XFDLIB  JOINING   市   AS  PREFIX.
001080* 
000280 FD  生保情報Ｆ          BLOCK   CONTAINS   1   RECORDS.
000281     COPY SEIHOJ          OF  XFDLIB  JOINING   生保   AS  PREFIX.
002140*
       FD  事業所マスタ        BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY JIGYOS          OF  XFDLIB  JOINING   事   AS  PREFIX.
001700*
000280 FD  労災情報Ｆ          BLOCK   CONTAINS   1   RECORDS.
000281     COPY ROUSAIJ         OF  XFDLIB  JOINING   労災   AS  PREFIX.
000284*
000282 FD  自賠責情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
000283     COPY JIBAIJ      OF  XFDLIB  JOINING   自賠   AS  PREFIX.
000114*
000115 FD  保険会社マスタ   BLOCK   CONTAINS   1   RECORDS.
000116     COPY HOKENCOM    OF  XFDLIB  JOINING   保険会   AS  PREFIX.
002190*
002200 FD  印刷ファイル.
002210     COPY YHP662P        OF  XMDLIB.
002220*----------------------------------------------------------------*
002230******************************************************************
002240*                WORKING-STORAGE SECTION                         *
002250******************************************************************
002260 WORKING-STORAGE         SECTION.
002270 01 キー入力                           PIC X     VALUE SPACE.
002280 01 状態キー                           PIC X(2)  VALUE SPACE.
002290 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002300 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002310 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002320 01 詳細ＣＮＴ                         PIC 9     VALUE ZERO.
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
002570 01 カルテ用紙種別Ｗ                   PIC 9(1).
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
002880       05 負傷原因内容１ＸＷ           PIC X(60)  VALUE SPACE.
002890       05 負傷原因内容２ＸＷ           PIC X(60)  VALUE SPACE.
002900       05 負傷原因内容３ＸＷ           PIC X(60)  VALUE SPACE.
002900       05 負傷原因内容４ＸＷ           PIC X(60)  VALUE SPACE.
002910       05 負傷原因内容５ＸＷ           PIC X(60)  VALUE SPACE.
002910       05 負傷原因内容６ＸＷ           PIC X(16)  VALUE SPACE.
002920*
      */負傷原因の詳細印刷/0610
       01 負傷原因Ｗテーブル２.
          03 負傷原因Ｗ２                    PIC X(60) OCCURS 54 VALUE SPACE.
002930** 負傷原因印刷区分用
002940 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
002950*
002960 01 負傷原因情報Ｗ.
002970    03 負傷原因コードＷ  OCCURS   9.
002980       09 負傷原因患者番号Ｗ           PIC 9(6) VALUE ZERO.
002990       09 負傷原因連番Ｗ               PIC 9(4) VALUE ZERO.
003000*
003010* 社保用
003020 01 接尾語区分Ｗ                       PIC 9     VALUE ZERO.
003030*
003040****************
003050* 連結項目待避 *
003060****************
003070*    ************
003080*    * 印刷キー *
003090*    ************
003100 01 対象データＷＲ.
003110    03 施術和暦年月ＷＲ.
003120       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
003130       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
003140       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
003150    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
003160    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
003170    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
003180    03 被保険者カナＷＲ                 PIC X(20) VALUE SPACE.
003190    03 患者コードＷＲ.
003200       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
003210       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
003220    03 印刷モードＦＷＲ                 PIC 9(1)  VALUE ZERO.
003230    03 提出年月日ＷＲ.
003240       05 提出年ＷＲ                    PIC 9(2)  VALUE ZERO.
003250       05 提出月ＷＲ                    PIC 9(2)  VALUE ZERO.
003260       05 提出日ＷＲ                    PIC 9(2)  VALUE ZERO.
003270    03 段数ＷＲ                         PIC 9(1)  VALUE ZERO.
003280************
003290* 料金情報 *
003300************
003310*
003320 01 印刷負担率Ｗ                        PIC 9(1)  VALUE ZERO.
003330**************
003340* 受診者情報 *
003350**************
003360 01 受診者情報Ｗ.
003370    03 患者コードＷ.
003380       05 患者番号Ｗ                   PIC 9(6)  VALUE ZERO.
003390       05 枝番Ｗ                       PIC X(1)  VALUE SPACE.
003400    03 市町村番号Ｗ.
003410       05 印刷市町村番号Ｗ             PIC X(8)  VALUE SPACE.
003420       05 FILLER                       PIC X(2).
003430    03 受益者番号Ｗ.
003440       05 印刷受益者番号Ｗ             PIC X(7)  VALUE SPACE.
003450       05 FILLER                       PIC X(13).
003460    03 費用負担者番号Ｗ.
003470       05 印刷費用負担者番号Ｗ         PIC X(8)  VALUE SPACE.
003480       05 FILLER                       PIC X(2).
003490    03 受益者番号助成Ｗ.
003500*/10桁印字/0412
003510       05 印刷受益者番号助成Ｗ         PIC X(10)  VALUE SPACE.
003520       05 FILLER                       PIC X(10).
003530    03 記号Ｗ                          PIC X(24)  VALUE SPACE.
003540    03 番号Ｗ.
003550       05 印刷番号Ｗ                   PIC X(30)  VALUE SPACE.
003560*       05 FILLER                       PIC X(18).
003570    03 被保険者情報Ｗ.
003580       05 被保険者カナＷ               PIC X(50)  VALUE SPACE.
003590       05 被保険者氏名Ｗ               PIC X(50)  VALUE SPACE.
003600*
003610*       05 被保険者和暦チェックＷ.
003620*          07 被保険者明治Ｗ            PIC N(1)  VALUE SPACE.
003630*          07 被保険者大正Ｗ            PIC N(1)  VALUE SPACE.
003640*          07 被保険者昭和Ｗ            PIC N(1)  VALUE SPACE.
003650*          07 被保険者平成Ｗ            PIC N(1)  VALUE SPACE.
003660       05 被保険者性別チェックＷ.
003670          07 被保険者男チェックＷ      PIC N(1)  VALUE SPACE.
003680          07 被保険者女チェックＷ      PIC N(1)  VALUE SPACE.
003690*       05 被保険者性別Ｗ               PIC 9     VALUE ZERO.
003700       05 被保険者和暦チェックＷ.
003710          07 被保険者明治チェックＷ    PIC N(1)  VALUE SPACE.
003720          07 被保険者大正チェックＷ    PIC N(1)  VALUE SPACE.
003730          07 被保険者昭和チェックＷ    PIC N(1)  VALUE SPACE.
003740          07 被保険者平成チェックＷ    PIC N(1)  VALUE SPACE.
004490       05 被保険者元号Ｗ               PIC N(2)  VALUE SPACE.
003750       05 被保険者生年月日Ｗ.
003760          07 被保険者和暦Ｗ            PIC 9     VALUE ZERO.
003770          07 被保険者年Ｗ              PIC 9(2)  VALUE ZERO.
003780          07 被保険者月Ｗ              PIC 9(2)  VALUE ZERO.
003790          07 被保険者日Ｗ              PIC 9(2)  VALUE ZERO.
003800       05 被保険者年齢Ｗ               PIC 9(3)  VALUE ZERO.
003810*
003820       05 被保険者郵便番号Ｗ.
003830          07 被保険者郵便番号１Ｗ      PIC X(3)  VALUE SPACE.
003840          07 被保険者郵便番号２Ｗ      PIC X(4)  VALUE SPACE.
003850*       05 被保険者住所Ｗ.
003860*          07 印刷被保険者住所Ｗ        PIC X(80) VALUE SPACE.
003870       05 被保険者住所Ｗ.
003880          07 印刷被保険者住所１Ｗ      PIC X(50) VALUE SPACE.
003890          07 印刷被保険者住所２Ｗ      PIC X(50) VALUE SPACE.
003900       05 被保険者電話番号Ｗ           PIC X(15) VALUE SPACE.
003910       05 分割電話番号Ｗ.
003920          07 分割電話番号１Ｗ          PIC X(5)  VALUE SPACE.
003930          07 分割電話番号２Ｗ          PIC X(5)  VALUE SPACE.
003940          07 分割電話番号３Ｗ          PIC X(5)  VALUE SPACE.
003950*          07 市外局番Ｗ                PIC X(5)  VALUE SPACE.
003960*          07 分割局番Ｗ                PIC X(5)  VALUE SPACE.
003970*          07 分割番号Ｗ                PIC X(5)  VALUE SPACE.
003980    03 資格取得年月日Ｗ.
003990       05 資格取得元号Ｗ               PIC N(2)  VALUE SPACE.
004000       05 資格取得和暦Ｗ               PIC 9(1)  VALUE ZERO.
004010       05 資格取得年Ｗ                 PIC 9(2)  VALUE ZERO.
004020       05 資格取得月Ｗ                 PIC 9(2)  VALUE ZERO.
004030       05 資格取得日Ｗ                 PIC 9(2)  VALUE ZERO.
004040    03 有効期限年月日Ｗ.
004050       05 有効期限元号Ｗ               PIC N(2)  VALUE SPACE.
004060       05 有効期限和暦Ｗ               PIC 9(1)  VALUE ZERO.
004070       05 有効期限年Ｗ                 PIC 9(2)  VALUE ZERO.
004080       05 有効期限月Ｗ                 PIC 9(2)  VALUE ZERO.
004090       05 有効期限日Ｗ                 PIC 9(2)  VALUE ZERO.
004100       05 資格和暦チェックＷ.
004110          07 資格昭和チェックＷ        PIC N(1)  VALUE SPACE.
004120          07 資格平成チェックＷ        PIC N(1)  VALUE SPACE.
004130    03 患者情報Ｗ.
004140       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
004150       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
004160       05 患者性別チェックＷ.
004170          07 患者男チェックＷ          PIC N(1)  VALUE SPACE.
004180          07 患者女チェックＷ          PIC N(1)  VALUE SPACE.
004190       05 続柄Ｗ.
004200          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
004210          07 FILLER                    PIC X(4).
004220       05 続柄チェックＷ.
004230          07 本人チェックＷ            PIC N(1)  VALUE SPACE.
004240          07 家族チェックＷ            PIC N(1)  VALUE SPACE.
004250       05 和暦チェックＷ.
004260          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
004270          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
004280          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
004290          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
004490       05 患者元号Ｗ                   PIC N(2) VALUE SPACE.
004300       05 患者年Ｗ                     PIC 9(2) VALUE ZERO.
004310       05 患者月Ｗ                     PIC 9(2) VALUE ZERO.
004320       05 患者日Ｗ                     PIC 9(2) VALUE ZERO.
004330       05 患者年齢Ｗ                   PIC 9(3) VALUE ZERO.
004340       05 患者性別Ｗ                   PIC N(4) VALUE SPACE.
004350    03 保険種別チェックＷ.
004360       05 健保チェックＷ               PIC N(1) VALUE SPACE.
004370       05 社保チェックＷ               PIC N(1) VALUE SPACE.
004380       05 組合チェックＷ               PIC N(1) VALUE SPACE.
004390       05 日雇チェックＷ               PIC N(1) VALUE SPACE.
004400       05 共済チェックＷ               PIC N(1) VALUE SPACE.
004410       05 自衛官チェックＷ             PIC N(1) VALUE SPACE.
004420       05 国保チェックＷ               PIC N(1) VALUE SPACE.
004430       05 船員チェックＷ               PIC N(1) VALUE SPACE.
004440       05 退職チェックＷ               PIC N(1) VALUE SPACE.
004450*
004460       05 老人チェックＷ               PIC N(1) VALUE SPACE.
004470*
004480       05 公費負担チェックＷ           PIC N(1) VALUE SPACE.
004490       05 福チェックＷ                 PIC N(1) VALUE SPACE.
004500       05 身障チェックＷ               PIC N(1) VALUE SPACE.
004510       05 母子チェックＷ               PIC N(1) VALUE SPACE.
004520       05 乳幼児チェックＷ             PIC N(1) VALUE SPACE.
004530       05 原爆チェックＷ               PIC N(1) VALUE SPACE.
004540       05 国保組合チェックＷ           PIC N(1) VALUE SPACE.
004550       05 生保チェックＷ               PIC N(1) VALUE SPACE.
004560*
004570       05 労災チェックＷ               PIC N(1) VALUE SPACE.
004580       05 自賠チェックＷ               PIC N(1) VALUE SPACE.
004590       05 自費チェックＷ               PIC N(1) VALUE SPACE.
004600*
004610       05 患者郵便番号Ｗ.
004620          07 患者郵便番号１Ｗ          PIC X(3)  VALUE SPACE.
004630          07 患者郵便番号２Ｗ          PIC X(4)  VALUE SPACE.
004640       05 患者住所Ｗ.
004650          07 患者住所１Ｗ              PIC X(50) VALUE SPACE.
004660          07 患者住所２Ｗ              PIC X(50) VALUE SPACE.
004670       05 患者電話番号Ｗ               PIC X(15) VALUE SPACE.
004680    03 負傷原因Ｗ                      PIC X(60) OCCURS 33 VALUE SPACE.
004690**************
004700* 事業所情報 *
004710**************
004720 01 事業所情報Ｗ.
004730    03 事業所名称Ｗ.
004740       05 印刷事業所名称Ｗ             PIC X(60)  VALUE SPACE.
004750    03 事業所郵便番号Ｗ.
004760       05 事業所郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
004770       05 事業所郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
004780*    03 事業所住所Ｗ.
004790*       05 印刷事業所住所Ｗ             PIC X(70)  VALUE SPACE.
004800*       05 FILLER                       PIC X(10).
004810    03 事業所住所Ｗ.
004820       05 印刷事業所住所１Ｗ           PIC X(50)  VALUE SPACE.
004830       05 印刷事業所住所２Ｗ           PIC X(50)  VALUE SPACE.
004840**************
004850* 請求先情報 *
004860**************
004870 01 請求先情報Ｗ.
004880    03 保険者番号Ｗ.
004890       05 印刷保険者番号Ｗ             PIC X(8)  VALUE SPACE.
004900       05 FILLER                       PIC X(2).
004910    03 請求先名称Ｗ.
004920*       05 印刷請求先名称Ｗ             PIC X(40) VALUE SPACE.
004930       05 印刷請求先名称Ｗ             PIC X(54) VALUE SPACE.
004940    03 支部名Ｗ.
004950       05 印刷支部名Ｗ                 PIC X(40) VALUE SPACE.
004960*    03 請求先名支部名Ｗ.
004970*       05 印刷請求先名支部名Ｗ         PIC X(54) VALUE SPACE.
004980*       05 FILLER                       PIC X(26).
004990    03 請求先名支部名Ｗ.
005000       05 印刷請求先名支部名１Ｗ       PIC X(40) VALUE SPACE.
005010       05 印刷請求先名支部名２Ｗ       PIC X(40) VALUE SPACE.
005020    03 保険者呼名Ｗ.
005030*       05 印刷保険者呼名Ｗ             PIC N(7)  VALUE SPACE.
005040       05 印刷保険者呼名Ｗ             PIC X(14)  VALUE SPACE.
005050    03 請求先郵便番号Ｗ.
005060       05 請求先郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
005070       05 請求先郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
005080    03 請求先住所１Ｗ.
005090       05 印刷請求先住所１Ｗ           PIC X(40) VALUE SPACE.
005100    03 請求先住所２Ｗ.
005110       05 印刷請求先住所２Ｗ           PIC X(35) VALUE SPACE.
005120       05 FILLER                       PIC X(5).
005130****************
005140* 詳細用ワーク *
005150****************
005160* 連印－負傷印刷行用ワーク
005170 01 印刷行ＳＷ                           PIC 9(1) VALUE ZERO.
005180*
005190 01 負傷原因情報ＳＷ.
005200    03 負傷原因コードＳＷ  OCCURS   9.
005210       09 負傷患者番号ＳＷ               PIC 9(6) VALUE ZERO.
005220       09 負傷連番ＳＷ                   PIC 9(4) VALUE ZERO.
005230*
005240 01 負傷情報ＳＷ.
005250    03 負傷データＳＷ  OCCURS   9.
005260       05 会部位コードＳＷ               PIC X(4)  VALUE SPACE.
005270       05 部位ＣＮＴＳＷ                 PIC 9(1)  VALUE ZERO.
005280       05 負傷種別ＳＷ                   PIC 9(2)  VALUE ZERO.
005290       05 部位ＳＷ                       PIC 9(2)  VALUE ZERO.
005300       05 左右区分ＳＷ                   PIC 9(1)  VALUE ZERO.
005310       05 負傷位置番号ＳＷ               PIC 9(2)  VALUE ZERO.
005320       05 負傷名ＳＷ                     PIC N(18) VALUE SPACE.
005330       05 負傷年月日ＳＷ.
005340          07 負傷和暦ＳＷ                PIC 9(1)  VALUE ZERO.
005350          07 負傷年ＳＷ                  PIC 9(2)  VALUE ZERO.
005360          07 負傷月ＳＷ                  PIC 9(2)  VALUE ZERO.
005370          07 負傷日ＳＷ                  PIC 9(2)  VALUE ZERO.
005380          07 負傷年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
005390       05 施術開始年月日ＳＷ.
005400          07 施術開始和暦ＳＷ            PIC 9(1)  VALUE ZERO.
005410          07 施術開始年ＳＷ              PIC 9(2)  VALUE ZERO.
005420          07 施術開始月ＳＷ              PIC 9(2)  VALUE ZERO.
005430          07 施術開始日ＳＷ              PIC 9(2)  VALUE ZERO.
005440          07 施開年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
005450       05 施術終了年月日ＳＷ.
005460          07 施術終了和暦ＳＷ            PIC 9(1)  VALUE ZERO.
005470          07 施術終了年ＳＷ              PIC 9(2)  VALUE ZERO.
005480          07 施術終了月ＳＷ              PIC 9(2)  VALUE ZERO.
005490          07 施術終了日ＳＷ              PIC 9(2)  VALUE ZERO.
005500          07 施終年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
005510       05 開始年月日取得フラグＳ         PIC X(3)  VALUE SPACE.
005520       05 転帰ＳＷ                       PIC N(4)  VALUE SPACE.
005530       05 転帰チェックＳＷ.
005540          07 治癒チェックＳＷ            PIC N(1)  VALUE SPACE.
005550          07 中止チェックＳＷ            PIC N(1)  VALUE SPACE.
005560          07 転医チェックＳＷ            PIC N(1)  VALUE SPACE.
005570       05 施術累計回数ＳＷ               PIC 9(4)  VALUE ZERO.
005580****************
005590* 負傷データＦ *
005600****************
005610 01 負傷情報Ｗ.
005620    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
005630    03 負傷データ情報Ｗ  OCCURS   9.
005640       05 会部位コードＷ               PIC X(4)  VALUE SPACE.
005650       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
005660       05 負傷種別Ｗ                   PIC 9(2)  VALUE ZERO.
005670       05 部位Ｗ                       PIC 9(2)  VALUE ZERO.
005680       05 左右区分Ｗ                   PIC 9(1)  VALUE ZERO.
005690       05 負傷位置番号Ｗ               PIC 9(2)  VALUE ZERO.
005700       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
005710       05 負傷年月日Ｗ.
005720          07 負傷和暦Ｗ                PIC 9(1)  VALUE ZERO.
005730          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
005740          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
005750          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
005760          07 負傷年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005770       05 施術開始年月日Ｗ.
005780          07 施術開始和暦Ｗ            PIC 9(1)  VALUE ZERO.
005790          07 施術開始年Ｗ              PIC 9(2)  VALUE ZERO.
005800          07 施術開始月Ｗ              PIC 9(2)  VALUE ZERO.
005810          07 施術開始日Ｗ              PIC 9(2)  VALUE ZERO.
005820          07 施開年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005830       05 施術終了年月日Ｗ.
005840          07 施術終了和暦Ｗ            PIC 9(1)  VALUE ZERO.
005850          07 施術終了年Ｗ              PIC 9(2)  VALUE ZERO.
005860          07 施術終了月Ｗ              PIC 9(2)  VALUE ZERO.
005870          07 施術終了日Ｗ              PIC 9(2)  VALUE ZERO.
005880          07 施終年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005890       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
005900       05 転帰Ｗ                       PIC N(4)  VALUE SPACE.
005910       05 転帰チェックＷ.
005920          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
005930          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
005940          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
005950*
005960******************
005970* 負担率チェック *
005980******************
005990 01 負担率チェックＷ.
006000    03 ０割チェックＷ                  PIC N(1)  VALUE SPACE.
006010    03 １割チェックＷ                  PIC N(1)  VALUE SPACE.
006020    03 ２割チェックＷ                  PIC N(1)  VALUE SPACE.
006030    03 ３割チェックＷ                  PIC N(1)  VALUE SPACE.
006040**********
006050* 合計行 *
006060**********
006070 01 合計行Ｗ.
006080    03 請求終了日Ｗ                    PIC 9(2) VALUE ZERO.
006090    03 対象西暦Ｗ                      PIC 9(4) VALUE ZERO.
006100    03 商Ｗ                            PIC 9(3) VALUE ZERO.
006110    03 余Ｗ                            PIC 9(3) VALUE ZERO.
006120*/年月毎持つ保険者データのワーク
006130 01 保負担率区分Ｗ                     PIC 9    VALUE ZERO.
006140 01 保本人負担率Ｗ                     PIC 9(3) VALUE ZERO.
006150 01 保家族負担率Ｗ                     PIC 9(3) VALUE ZERO.
006160 01 保本人負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
006170 01 保家族負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
006180*
006190 01 比較和暦年月Ｗ.
006200    03 比較和暦Ｗ                      PIC 9    VALUE ZERO.
006210    03 比較年Ｗ                        PIC 9(2) VALUE ZERO.
006220    03 比較月Ｗ                        PIC 9(2) VALUE ZERO.
006230*
006240*/老人保険改正の対応0101
006250 01 老人負担計算区分Ｗ                 PIC 9    VALUE ZERO.
006260 01 負担率用種別Ｗ                     PIC 9(2) VALUE ZERO.
006270 01 助成負担区分Ｗ                     PIC 9(2) VALUE ZERO.
006280*
006290** 助成負担率用(14/10～)
006300 01 助成負担率ＷＷ.
006310    03 助成負担率Ｗ                    PIC 9(3) VALUE ZERO.
006320    03 助成負担率Ｗ１                  PIC 9    VALUE ZERO.
006330    03 助成負担率文字Ｗ１              PIC X    VALUE SPACE.
006340    03 助成負担率表示Ｗ                PIC X(6) VALUE SPACE.
006350*
006360******************************************************************
006370 01 印刷制御.
006380     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
006390     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
006400     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
006410     03 拡張制御Ｐ.
006420         05 端末制御Ｐ.
006430             07 移動方向Ｐ             PIC X(1)  VALUE SPACE.
006440             07 移動行数Ｐ             PIC 9(3)  VALUE ZERO.
006450         05 詳細制御Ｐ                 PIC X(2)  VALUE SPACE.
006460     03 通知情報Ｐ                     PIC X(2)  VALUE SPACE.
006470     03 ユニット名Ｐ                   PIC X(8)  VALUE SPACE.
006480*
006490 01 計算機西暦年Ｗ                     PIC 9(2)  VALUE ZERO.
006500* 日付ＷＯＲＫ
006510 01 和暦終了年Ｗ                       PIC 9(4)  VALUE ZERO.
006520 01 計算機西暦.
006530    03 計算機西暦年                    PIC 9(4)  VALUE ZERO.
006540    03 計算機西暦月日                  PIC 9(4)  VALUE ZERO.
006550 01 計算機西暦Ｒ REDEFINES 計算機西暦.
006560    03 計算機世紀                      PIC 9(2).
006570    03 計算機日付                      PIC 9(6).
006580    03 計算機日付Ｒ REDEFINES 計算機日付.
006590       05 計算機年月                   PIC 9(4).
006600       05 計算機年月Ｒ REDEFINES 計算機年月.
006610         07 計算機年                   PIC 9(2).
006620         07 計算機月                   PIC 9(2).
006630       05 計算機日                     PIC 9(2).
006640*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
006650******************************************************************
006660*                          連結項目                              *
006670******************************************************************
006680*
006690**********************
006700* メッセージ表示キー *
006710**********************
006720 01 連メ－キー IS EXTERNAL.
006730    03  連メ－メッセージ                 PIC N(20).
006740*
006750****************
006760* 連結項目待避 *
006770****************
006780*    ************
006790*    * 料金情報 *
006800*    ************
006810*    月毎の料金
006820***********************
006830 01 連施－料金１ IS EXTERNAL.
006840   03 連施－本人負担率                 PIC 9(3).
006850************
006860* 印刷キー *
006870************
006880 01 連印－対象データ IS EXTERNAL.
006890    03 連印－施術和暦年月.
006900       05 連印－施術和暦                  PIC 9(1).
006910       05 連印－施術年                    PIC 9(2).
006920       05 連印－施術月                    PIC 9(2).
006930    03 連印－保険種別                     PIC 9(2).
006940    03 連印－保険者番号                   PIC X(10).
006950    03 連印－本人家族区分                 PIC 9(1).
006960    03 連印－被保険者カナ                 PIC X(20).
006970    03 連印－患者コード.
006980       05 連印－患者番号                  PIC 9(6).
006990       05 連印－枝番                      PIC X(1).
007000    03 連印－印刷モードＦ                 PIC 9(1).
007010*/詳細
007020    03 連印－保険証印刷区分               PIC 9(1).
007030    03 連印－負傷詳細 OCCURS 7.
007040       05 連印－負傷印刷行                PIC 9(1).
007050       05 連印－部位印刷区分              PIC 9(1).
007060       05 連印－転帰印刷区分              PIC 9(1).
007070       05 連印－原因印刷区分              PIC 9(1).
      */
       01 連印－対象データ追加 IS EXTERNAL GLOBAL.
          03 連印－部位丸付け                   PIC 9(1).
      */年齢印刷0304
      */入力画面(660,6601)と新用紙(6621)との連結項目。(旧用紙、特別用紙等のＰＧを変えたく無い為、新たに作ります)
          03 連印－年齢印刷区分                 PIC 9(1).
      */負傷原因の詳細モード対応/0610
          03 連印－原因開始行                   PIC 9(2).
      */相談支援記載追加/080812
          03 連印－相談支援記載                 PIC 9(1).
      */ワクごと印刷 ０：枠なし １：ワクあり ２：ワクのみ /0402
          03 連印－枠印刷Ｆ                     PIC 9(1).
      */長期理由0311
          03 連印－長期印刷Ｆ                   PIC 9(1).
007410*
       01 連入－表示フラグ６６０ IS EXTERNAL GLOBAL.
          03 連入－プレビュー区分               PIC 9(1).
007420* 負担率取得用14/10～
007430 01 連率－負担率取得キー IS EXTERNAL.
007440    03 連率－施術和暦年月.
007450       05 連率－施術和暦               PIC 9.
007460       05 連率－施術年月.
007470          07 連率－施術年              PIC 9(2).
007480          07 連率－施術月              PIC 9(2).
007490    03 連率－患者コード.
007500       05 連率－患者番号               PIC 9(6).
007510       05 連率－枝番                   PIC X.
007520    03 連率－実際負担率                PIC 9(3).
007530    03 連率－実際本体負担率            PIC 9(3).
007540    03 連率－健保負担率                PIC 9(3).
007550    03 連率－２７老負担率              PIC 9(3).
007560    03 連率－助成負担率                PIC 9(3).
007570    03 連率－特別用負担率              PIC 9(3).
007580*
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
007590******************************************************************
007600*                      PROCEDURE  DIVISION                       *
007610******************************************************************
007620 PROCEDURE               DIVISION.
007630************
007640*           *
007650* 初期処理   *
007660*           *
007670************
002570     PERFORM プリンタファイル作成.
007680     PERFORM 初期化.
007690************
007700*           *
007710* 主処理     *
007720*           *
007730************
007740* 印刷
007750     PERFORM 連結項目待避.
007760     PERFORM 印刷セット
007770     PERFORM 印刷処理
007780************
007790*           *
007800* 終了処理   *
007810*           *
007820************
007830     PERFORM 終了処理.
007840     EXIT PROGRAM.
007850*
007860*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YHP662"              TO Ｈ連ＰＲＴＦ－帳票プログラム名.
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
007870*================================================================*
007880 初期化 SECTION.
007890*
007900     PERFORM ファイルオープン.
007910*    /* 現在日付取得 */
007920     ACCEPT 計算機日付 FROM DATE.
007930*    /* 1980～2079年の間で設定 */
007940     IF 計算機年 > 80
007950         MOVE 19 TO 計算機世紀
007960     ELSE
007970         MOVE 20 TO 計算機世紀
007980     END-IF.
007990     PERFORM カレント元号取得.
008000     PERFORM 和暦終了年取得.
008010     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
008020*================================================================*
008030 カレント元号取得 SECTION.
008040*
008050     MOVE ZEROS TO 制－制御区分.
008060     READ 制御情報マスタ
008070     NOT INVALID KEY
008080         MOVE 制－カレント元号 TO カレント元号Ｗ
008090         MOVE 制－カルテ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
008760         MOVE 制－カルテ用紙種別 TO カルテ用紙種別Ｗ
008100     END-READ.
008110*
008120*================================================================*
008130 和暦終了年取得 SECTION.
008140*
008150     MOVE カレント元号Ｗ TO 元－元号区分.
008160     READ 元号マスタ
008170     INVALID KEY
008180         DISPLAY NC"指定和暦が登録されていません" UPON CONS
008190         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008200                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
008210         ACCEPT  キー入力 FROM CONS
008220         PERFORM 終了処理
008230         EXIT PROGRAM
008240     NOT INVALID KEY
008250         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
008260         MOVE 前和暦Ｗ TO 元－元号区分
008270         READ 元号マスタ
008280         INVALID KEY
008290             DISPLAY NC"指定和暦が登録されていません" UPON CONS
008300             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008310                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
008320             ACCEPT  キー入力 FROM CONS
008330             PERFORM 終了処理
008340             EXIT PROGRAM
008350         NOT INVALID KEY
008360             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
008370         END-READ
008380     END-READ.
008390*
008400*================================================================*
008410 連結項目待避 SECTION.
008420*
008430     INITIALIZE 対象データＷＲ.
008440     MOVE 連印－施術和暦      TO 施術和暦ＷＲ.
008450     MOVE 連印－施術年        TO 施術年ＷＲ.
008460     MOVE 連印－施術月        TO 施術月ＷＲ.
008470     MOVE 連印－保険種別      TO 保険種別ＷＲ.
008480     MOVE 連印－保険者番号    TO 保険者番号ＷＲ.
008490     MOVE 連印－本人家族区分  TO 本人家族区分ＷＲ.
008500     MOVE 連印－被保険者カナ  TO 被保険者カナＷＲ.
008510     MOVE 連印－患者番号      TO 患者番号ＷＲ.
008520     MOVE 連印－枝番          TO 枝番ＷＲ.
008530     MOVE 連印－印刷モードＦ  TO 印刷モードＦＷＲ.
008540*================================================================*
008550 ファイルオープン SECTION.
008560*
008570     OPEN INPUT   保険者マスタ
008580         MOVE NC"保険者" TO ファイル名.
008590         PERFORM オープンチェック.
008600     OPEN INPUT   元号マスタ
008610         MOVE NC"元号" TO ファイル名.
008620         PERFORM オープンチェック.
008630     OPEN INPUT   名称マスタ
008640         MOVE NC"名称" TO ファイル名.
008650         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
008690     OPEN INPUT   制御情報マスタ
008700         MOVE NC"制御情報" TO ファイル名.
008710         PERFORM オープンチェック.
008750     OPEN INPUT   請求先マスタ
008760         MOVE NC"請先" TO ファイル名.
008770         PERFORM オープンチェック.
008810     OPEN INPUT   受診者情報Ｆ.
008820         MOVE NC"受情" TO ファイル名.
008830         PERFORM オープンチェック.
008840     OPEN INPUT   施術記録Ｆ.
008850         MOVE NC"施記Ｆ" TO ファイル名.
008860         PERFORM オープンチェック.
008870     OPEN INPUT   負傷データＦ.
008880         MOVE NC"負傷" TO ファイル名.
008890         PERFORM オープンチェック.
008930     OPEN INPUT   負傷原因Ｆ.
008940         MOVE NC"負傷原因" TO ファイル名.
008950         PERFORM オープンチェック.
008960     OPEN INPUT 市町村マスタ.
008970         MOVE NC"市町村" TO ファイル名.
008980         PERFORM オープンチェック.
006630     OPEN INPUT 生保情報Ｆ.
006640         MOVE NC"生保" TO ファイル名.
006650         PERFORM オープンチェック.
007900     OPEN INPUT 事業所マスタ.
007910         MOVE NC"事業所" TO ファイル名.
007920         PERFORM オープンチェック.
007900     OPEN INPUT 労災情報Ｆ.
007910         MOVE NC"労災情報Ｆ" TO ファイル名.
007920         PERFORM オープンチェック.
009290     OPEN INPUT 自賠責情報Ｆ.
009300         MOVE NC"自賠" TO ファイル名.
009310         PERFORM オープンチェック.
009290     OPEN INPUT 保険会社マスタ.
009300         MOVE NC"保会" TO ファイル名.
009310         PERFORM オープンチェック.
009030     OPEN I-O   印刷ファイル
009040         MOVE NC"印刷" TO ファイル名.
009050         PERFORM エラー処理Ｐ.
009060*================================================================*
009070 オープンチェック SECTION.
009080*
009090     IF 状態キー  NOT =  "00"
009100         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
009110         DISPLAY NC"状態キー：" 状態キー         UPON CONS
009120         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
009130                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
009140         ACCEPT  キー入力 FROM CONS
009150         PERFORM ファイル閉鎖
009160         EXIT PROGRAM.
009170*================================================================*
009180 ファイル閉鎖 SECTION.
009190*
009200     CLOSE 印刷ファイル   保険者マスタ     元号マスタ
009210           制御情報マスタ 請求先マスタ     名称マスタ
009220           受診者情報Ｆ   施術記録Ｆ       生保情報Ｆ
009230           負傷データＦ   負傷原因Ｆ       レセプトＦ
009240           市町村マスタ   事業所マスタ     労災情報Ｆ
                 自賠責情報Ｆ   保険会社マスタ.
009250*================================================================*
009260 終了処理 SECTION.
009270*
009280     PERFORM ファイル閉鎖.
009290*================================================================*
009300 印刷セット SECTION.
009310*
009320     EVALUATE 印刷モードＦＷＲ
009330     WHEN 0
009340         PERFORM 印刷セット１
009350     WHEN 1
009360         PERFORM 印刷セット２
009370     WHEN 2
009380         PERFORM 印刷セット３
009410*/詳細モード用
009420     WHEN 4
009430         PERFORM 印刷セット５
009440     END-EVALUATE.
009450*
009460*================================================================*
009470 印刷セット１ SECTION.
009480*
009490     INITIALIZE YHP662P.
009500     INITIALIZE 受診者情報Ｗ.
009510     INITIALIZE 事業所情報Ｗ.
009520     INITIALIZE 請求先情報Ｗ.
009530     INITIALIZE 負傷情報Ｗ.
009540     INITIALIZE 負担率チェックＷ.
009550     PERFORM 負担率取得.
009560     PERFORM 請求先情報取得.
009570     PERFORM 受診者情報取得.
009580     PERFORM 負傷データ取得.
009590*
009600     IF 負傷原因印刷区分Ｗ  NOT = 1 
009610        PERFORM 負傷原因取得
009620     END-IF.
009630*
009640     PERFORM 印刷上部セット.
009650     PERFORM 印刷下部セット.
009660*================================================================*
009670 印刷セット２ SECTION.
009680*
009690     INITIALIZE YHP662P.
009700     INITIALIZE 受診者情報Ｗ.
009710     INITIALIZE 事業所情報Ｗ.
009720     INITIALIZE 請求先情報Ｗ.
009730     INITIALIZE 負傷情報Ｗ.
009740     INITIALIZE 負担率チェックＷ.
009750     PERFORM 負担率取得.
009760     PERFORM 請求先情報取得.
009770     PERFORM 受診者情報取得.
009780     PERFORM 印刷上部セット.
009790*================================================================*
009800 印刷セット３ SECTION.
009810*
009820     INITIALIZE YHP662P.
009830     INITIALIZE 受診者情報Ｗ.
009840     INITIALIZE 事業所情報Ｗ.
009850     INITIALIZE 請求先情報Ｗ.
009860     INITIALIZE 負傷情報Ｗ.
009770     PERFORM 受診者情報取得.
009870     PERFORM 負傷データ取得.
009880*
009890     IF 負傷原因印刷区分Ｗ  NOT = 1 
009900        PERFORM 負傷原因取得
009910     END-IF.
009920*
009930     PERFORM 印刷下部セット.
010080*================================================================*
010090 印刷セット５ SECTION.
010100*
010110*/詳細モード用
010120     INITIALIZE YHP662P.
010130     INITIALIZE 受診者情報Ｗ.
010140     INITIALIZE 事業所情報Ｗ.
010150     INITIALIZE 請求先情報Ｗ.
010160     INITIALIZE 負傷情報Ｗ.
010170     INITIALIZE 負傷原因情報Ｗ.
010180***     PERFORM 負担率取得.
010190     PERFORM 負担率取得.
010200     PERFORM 請求先情報取得.
010210     PERFORM 受診者情報取得.
010220     PERFORM 負傷データ取得.
010230*
010240     IF 負傷原因印刷区分Ｗ  NOT = 1
010250        PERFORM 詳細モード負傷原因処理
010260        PERFORM 負傷原因取得
010270     END-IF.
      */負傷原因の詳細印刷/0610
           PERFORM 原因行セット.
010280*
010290     IF 連印－保険証印刷区分 = 1
010300         PERFORM 印刷上部セット
010310     END-IF.
010320*
010330     PERFORM 詳細モード負傷処理.
010340     PERFORM 印刷下部セット.
010350*
010360*================================================================*
010370 印刷上部セット SECTION.
010380*
010390     MOVE 患者コードＷ               TO 患者コード.
010400     MOVE 患者番号Ｗ                 TO 患者番号.
010410     MOVE 枝番Ｗ                     TO 枝番.
010420**************************
010430* 被保険者証情報セット   *
010440**************************
010450     IF 記号Ｗ(1:1) = "＊" 
010460        MOVE  SPACE    TO  記号
010470     ELSE
010480        MOVE 記号Ｗ    TO  記号
010490     END-IF.
010500     IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
010510        ( 印刷番号Ｗ(1:2) = "＊" )
010520        MOVE  SPACE      TO  番号
010530     ELSE
010540        MOVE 印刷番号Ｗ  TO  番号
010550     END-IF.
010560************************
010570* 被保険者情報セット   *
010580************************
010590     MOVE 被保険者氏名Ｗ             TO 被保険者氏名.
010600     MOVE 被保険者カナＷ             TO 被保険者カナ.
010610*     MOVE 被保険者男チェックＷ       TO 被保険者男チェック.
010620*     MOVE 被保険者女チェックＷ       TO 被保険者女チェック.
010630     MOVE 被保険者郵便番号１Ｗ       TO 被保険者郵便番号１.
010640     MOVE 被保険者郵便番号２Ｗ       TO 被保険者郵便番号２.
010650     MOVE "-"                        TO 被保険者郵便区切.
010660*     MOVE 被保険者住所Ｗ             TO 被保険者住所.
010670     MOVE 印刷被保険者住所１Ｗ       TO 被保険者住所１.
010680     MOVE 印刷被保険者住所２Ｗ       TO 被保険者住所２.
010690*     MOVE 被保険者電話番号Ｗ         TO 被保険者電話番号.
011270     MOVE 分割電話番号１Ｗ           TO 被保険者電話番号１.
011270     MOVE 分割電話番号２Ｗ           TO 被保険者電話番号２.
011270     MOVE 分割電話番号３Ｗ           TO 被保険者電話番号３.
010710     MOVE 資格取得年Ｗ               TO 資格取得年.
010720     MOVE 資格取得月Ｗ               TO 資格取得月.
010730     MOVE 資格取得日Ｗ               TO 資格取得日.
012581     IF カルテ用紙種別Ｗ = 2
010700        MOVE 資格取得元号Ｗ          TO 資格取得和暦
           ELSE
010740        MOVE 資格昭和チェックＷ      TO 資格昭和チェック
010750        MOVE 資格平成チェックＷ      TO 資格平成チェック
           END-IF.
010760*/有効期限
010770     MOVE 有効期限年Ｗ               TO 有効年.
010780     MOVE 有効期限月Ｗ               TO 有効月.
010790     MOVE 有効期限日Ｗ               TO 有効日.
012581     IF カルテ用紙種別Ｗ = 2
011470        MOVE 有効期限元号Ｗ          TO 有効和暦
           END-IF.
010800*    IF ( 有効期限年Ｗ = ZERO ) AND
010810*       ( 有効期限月Ｗ = ZERO ) AND
010820*       ( 有効期限日Ｗ = ZERO )
010830*        MOVE SPACE          TO 有効元号
010840*        MOVE SPACE          TO 有効年月日表示
010850*        MOVE SPACE          TO 有効年表示
010860*        MOVE SPACE          TO 有効月表示
010870*        MOVE SPACE          TO 有効日表示
010880*    ELSE
010890*        MOVE 有効期限元号Ｗ TO 有効元号
010900*        MOVE NC"有効年月日" TO 有効年月日表示
010910*        MOVE NC"年"         TO 有効年表示
010920*        MOVE NC"月"         TO 有効月表示
010930*        MOVE NC"日"         TO 有効日表示
010940*    END-IF.
012581     IF カルテ用紙種別Ｗ = 2
011280        MOVE 被保険者元号Ｗ          TO 被保険者和暦
           ELSE
010950        MOVE 被保険者明治チェックＷ  TO 被保険者明治チェック
010960        MOVE 被保険者大正チェックＷ  TO 被保険者大正チェック
010970        MOVE 被保険者昭和チェックＷ  TO 被保険者昭和チェック
010980        MOVE 被保険者平成チェックＷ  TO 被保険者平成チェック
           END-IF.
010990     MOVE 被保険者年Ｗ               TO 被保険者年.
011000     MOVE 被保険者月Ｗ               TO 被保険者月.
011010     MOVE 被保険者日Ｗ               TO 被保険者日.
011020*    MOVE 被保険者年齢Ｗ             TO 被保険者年齢.
011030********************
011040* 患者情報セット   *
011050********************
011060     MOVE ０割チェックＷ             TO ０割チェック.
011070     MOVE １割チェックＷ             TO １割チェック.
011080     MOVE ２割チェックＷ             TO ２割チェック.
011090     MOVE ３割チェックＷ             TO ３割チェック.
011100*
011110* 14/10～ (助成の負担率）
011120     MOVE 助成負担率表示Ｗ           TO 助成負担率.
011130*
011140     IF  印刷市町村番号Ｗ(1:2) = "99"
011150         MOVE SPACE                  TO 市町村番号
011160     ELSE
011170         MOVE 印刷市町村番号Ｗ       TO 市町村番号
011180     END-IF.
011190     IF (印刷受益者番号Ｗ(1:1) = "*") OR
011200        (印刷受益者番号Ｗ(1:2) = "＊") 
011210         MOVE SPACE                  TO 受益者番号
011220     ELSE
011230         MOVE 印刷受益者番号Ｗ       TO 受益者番号
011240     END-IF
011250*
011260     IF  印刷費用負担者番号Ｗ(1:2) = "99"
011270         MOVE SPACE                  TO 公費負担者番号
011280     ELSE
011290         MOVE 印刷費用負担者番号Ｗ   TO 公費負担者番号
011300     END-IF.
011310*
011320     IF (印刷受益者番号助成Ｗ(1:1) = "*") OR
011330        (印刷受益者番号助成Ｗ(1:2) = "＊")
011340         MOVE SPACE                  TO 受益者番号助成
011350     ELSE
011360         MOVE 印刷受益者番号助成Ｗ   TO 受益者番号助成
011370     END-IF.
011380     MOVE 患者カナＷ                 TO 患者カナ.
011390     MOVE 患者氏名Ｗ                 TO 患者氏名.
011400     MOVE 患者男チェックＷ           TO 患者男チェック.
011410     MOVE 患者女チェックＷ           TO 患者女チェック.
012581     IF カルテ用紙種別Ｗ = 2
011280        MOVE 患者元号Ｗ              TO 患者和暦
           ELSE
011420        MOVE 明治チェックＷ          TO 明治チェック
011430        MOVE 大正チェックＷ          TO 大正チェック
011440        MOVE 昭和チェックＷ          TO 昭和チェック
011450        MOVE 平成チェックＷ          TO 平成チェック
           END-IF.
011460     MOVE 患者年Ｗ                   TO 患者年.
011470     MOVE 患者月Ｗ                   TO 患者月.
011480     MOVE 患者日Ｗ                   TO 患者日.
011490*     MOVE 患者年齢Ｗ                 TO 患者年齢.
011500     MOVE 本人チェックＷ             TO 本人チェック.
011510     MOVE 家族チェックＷ             TO 家族チェック.
011520     MOVE 印刷続柄Ｗ                 TO 続柄.
011530*     MOVE 患者郵便番号１Ｗ           TO 患者郵便番号１.
011540*     MOVE 患者郵便番号２Ｗ           TO 患者郵便番号２.
011550*     MOVE "-"                        TO 患者郵便区切.
011560*     MOVE 患者住所Ｗ                 TO 患者住所.
011570*     MOVE 患者電話番号Ｗ             TO 患者電話番号.
011580*     MOVE 患者住所１Ｗ               TO 患者住所１.
011590*     MOVE 患者住所２Ｗ               TO 患者住所２.
011600********************
011610* 事業所情報セット *
011620********************
011630     MOVE 事業所名称Ｗ               TO 事業所名称.
011640     MOVE 事業所郵便番号１Ｗ         TO 事業所郵便番号１.
011650     MOVE 事業所郵便番号２Ｗ         TO 事業所郵便番号２.
011660*     MOVE 印刷事業所住所Ｗ           TO 事業所住所.
011670     MOVE 印刷事業所住所１Ｗ         TO 事業所住所１.
011680     MOVE 印刷事業所住所２Ｗ         TO 事業所住所２.
011690************************************
011700* 都道府県・健康保険組合情報セット *
011710************************************
011720*労災、自費の時は保険者情報は印字しない
011730     IF 保険種別ＷＲ = 70 OR 90
011740         MOVE SPACE                  TO 保険者番号
011750*         MOVE SPACE                  TO 請求先名支部名
011760         MOVE SPACE                  TO 請求先名支部名１
011770         MOVE SPACE                  TO 請求先名支部名２
011780         MOVE SPACE                  TO 保険者住所１
011790         MOVE SPACE                  TO 保険者住所２
011800     ELSE
011810         MOVE 印刷保険者番号Ｗ       TO 保険者番号
011820*         MOVE 印刷請求先名支部名Ｗ   TO 請求先名支部名
011830         MOVE 印刷請求先名支部名１Ｗ TO 請求先名支部名１
011840         MOVE 印刷請求先名支部名２Ｗ TO 請求先名支部名２
011850         MOVE 請求先住所１Ｗ         TO 保険者住所１
011860         MOVE 請求先住所２Ｗ         TO 保険者住所２
011870     END-IF.
011880**************************
011890* 保険種別チェックセット *
011900**************************
011910     MOVE 健保チェックＷ             TO 健保チェック.
011920     MOVE 社保チェックＷ             TO 社保チェック.
011930     MOVE 組合チェックＷ             TO 組合チェック.
011940     MOVE 日雇チェックＷ             TO 日雇チェック.
011950     MOVE 船員チェックＷ             TO 船員チェック.
011960     MOVE 共済チェックＷ             TO 共済チェック.
011970     MOVE 自衛官チェックＷ           TO 自衛官チェック.
011980     MOVE 国保チェックＷ             TO 国保チェック.
011990     MOVE 退職チェックＷ             TO 退職チェック.
012000     MOVE 老人チェックＷ             TO 老人チェック.
012582     MOVE 労災チェックＷ             TO 公費負担チェック
012010*     MOVE 公費負担チェックＷ         TO 公費負担チェック.
012020*     MOVE 乳幼児チェックＷ           TO 乳幼児チェック.
012030*     MOVE 福チェックＷ               TO 福チェック.
012040*     MOVE 身障チェックＷ             TO 身障チェック.
012050*     MOVE 母子チェックＷ             TO 母子チェック.
012060*     MOVE 原爆チェックＷ             TO 原爆チェック.
012070     MOVE 生保チェックＷ             TO 生保チェック.
012080     MOVE 自賠チェックＷ             TO 自賠チェック.
012090     MOVE 自費チェックＷ             TO 自費チェック.
012100* テスト
012110***     MOVE ALL "X" TO 被保険者住所１ 被保険者住所２
012120***                     被保険者電話番号
012130***                     請求先名支部名１ 請求先名支部名２
012140***                     保険者住所１ 保険者住所２ 事業所名称
012150***                     事業所住所１ 公費負担者番号 受益者番号助成 
012160***                     保険者番号.
012170***     MOVE ALL "9" TO 有効年 有効月 有効日.
012180*================================================================*
012190 印刷下部セット SECTION.
012200*
012210********************
012220* 負傷データセット *
012230********************
012240* １部位 *
012250**********
012260*     MOVE 会部位コードＷ(1)   TO 負傷コード１.
012270     MOVE 負傷名Ｗ(1)         TO 負傷名１.
012280     MOVE 負傷年Ｗ(1)         TO 負傷年１.
012290     MOVE 負傷月Ｗ(1)         TO 負傷月１.
012300     MOVE 負傷日Ｗ(1)         TO 負傷日１.
012310     MOVE 施術開始年Ｗ(1)     TO 施術開始年１.
012320     MOVE 施術開始月Ｗ(1)     TO 施術開始月１.
012330     MOVE 施術開始日Ｗ(1)     TO 施術開始日１.
012340     MOVE 施術終了年Ｗ(1)     TO 施術終了年１.
012350     MOVE 施術終了月Ｗ(1)     TO 施術終了月１.
012360     MOVE 施術終了日Ｗ(1)     TO 施術終了日１.
012370*     MOVE 負傷年月日区切Ｗ(1) TO 区切１１ 区切１２.
012380*     MOVE 施開年月日区切Ｗ(1) TO 区切１３ 区切１４.
012390*     MOVE 施終年月日区切Ｗ(1) TO 区切１５ 区切１６.
012400*     MOVE 転帰Ｗ(1)           TO 転帰１.
012410     MOVE 治癒チェックＷ(1)   TO 治癒チェック１.
012420     MOVE 中止チェックＷ(1)   TO 中止チェック１.
012430     MOVE 転医チェックＷ(1)   TO 転医チェック１.
012440*     ******************
012450*     * 日数は一時保留 *
012460*     ******************
012470*     **********************
012480*     * 施術回数は一時保留 *
012490*     **********************
012500*     **********************
012510*     * 転帰名称は一時保留 *
012520*     **********************
012530**********
012540* ２部位 *
012550**********
012560*     MOVE 会部位コードＷ(2)   TO 負傷コード２.
012570     MOVE 負傷名Ｗ(2)         TO 負傷名２.
012580     MOVE 負傷年Ｗ(2)         TO 負傷年２.
012590     MOVE 負傷月Ｗ(2)         TO 負傷月２.
012600     MOVE 負傷日Ｗ(2)         TO 負傷日２.
012610     MOVE 施術開始年Ｗ(2)     TO 施術開始年２.
012620     MOVE 施術開始月Ｗ(2)     TO 施術開始月２.
012630     MOVE 施術開始日Ｗ(2)     TO 施術開始日２.
012640     MOVE 施術終了年Ｗ(2)     TO 施術終了年２.
012650     MOVE 施術終了月Ｗ(2)     TO 施術終了月２.
012660     MOVE 施術終了日Ｗ(2)     TO 施術終了日２.
012670*     MOVE 負傷年月日区切Ｗ(2) TO 区切２１ 区切２２.
012680*     MOVE 施開年月日区切Ｗ(2) TO 区切２３ 区切２４.
012690*     MOVE 施終年月日区切Ｗ(2) TO 区切２５ 区切２６.
012700*     MOVE 転帰Ｗ(2)           TO 転帰２.
012710     MOVE 治癒チェックＷ(2)   TO 治癒チェック２.
012720     MOVE 中止チェックＷ(2)   TO 中止チェック２.
012730     MOVE 転医チェックＷ(2)   TO 転医チェック２.
012740*     ******************
012750*     * 日数は一時保留 *
012760*     ******************
012770*     **********************
012780*     * 施術回数は一時保留 *
012790*     **********************
012800*     **********************
012810*     * 転帰名称は一時保留 *
012820*     **********************
012830**********
012840* ３部位 *
012850**********
012860*     MOVE 会部位コードＷ(3)   TO 負傷コード３.
012870     MOVE 負傷名Ｗ(3)         TO 負傷名３.
012880     MOVE 負傷年Ｗ(3)         TO 負傷年３.
012890     MOVE 負傷月Ｗ(3)         TO 負傷月３.
012900     MOVE 負傷日Ｗ(3)         TO 負傷日３.
012910     MOVE 施術開始年Ｗ(3)     TO 施術開始年３.
012920     MOVE 施術開始月Ｗ(3)     TO 施術開始月３.
012930     MOVE 施術開始日Ｗ(3)     TO 施術開始日３.
012940     MOVE 施術終了年Ｗ(3)     TO 施術終了年３.
012950     MOVE 施術終了月Ｗ(3)     TO 施術終了月３.
012960     MOVE 施術終了日Ｗ(3)     TO 施術終了日３.
012970*     MOVE 負傷年月日区切Ｗ(3) TO 区切３１ 区切３２.
012980*     MOVE 施開年月日区切Ｗ(3) TO 区切３３ 区切３４.
012990*     MOVE 施終年月日区切Ｗ(3) TO 区切３５ 区切３６.
013000*     MOVE 転帰Ｗ(3)           TO 転帰３.
013010     MOVE 治癒チェックＷ(3)   TO 治癒チェック３.
013020     MOVE 中止チェックＷ(3)   TO 中止チェック３.
013030     MOVE 転医チェックＷ(3)   TO 転医チェック３.
013040*     ******************
013050*     * 日数は一時保留 *
013060*     ******************
013070*     **********************
013080*     * 施術回数は一時保留 *
013090*     **********************
013100*     **********************
013110*     * 転帰名称は一時保留 *
013120*     **********************
013130**********
013140* ４部位 *
013150**********
013160*     MOVE 会部位コードＷ(4)   TO 負傷コード４.
013170     MOVE 負傷名Ｗ(4)         TO 負傷名４.
013180     MOVE 負傷年Ｗ(4)         TO 負傷年４.
013190     MOVE 負傷月Ｗ(4)         TO 負傷月４.
013200     MOVE 負傷日Ｗ(4)         TO 負傷日４.
013210     MOVE 施術開始年Ｗ(4)     TO 施術開始年４.
013220     MOVE 施術開始月Ｗ(4)     TO 施術開始月４.
013230     MOVE 施術開始日Ｗ(4)     TO 施術開始日４.
013240     MOVE 施術終了年Ｗ(4)     TO 施術終了年４.
013250     MOVE 施術終了月Ｗ(4)     TO 施術終了月４.
013260     MOVE 施術終了日Ｗ(4)     TO 施術終了日４.
013270*     MOVE 負傷年月日区切Ｗ(4) TO 区切４１ 区切４２.
013280*     MOVE 施開年月日区切Ｗ(4) TO 区切４３ 区切４４.
013290*     MOVE 施終年月日区切Ｗ(4) TO 区切４５ 区切４６.
013300*     MOVE 転帰Ｗ(4)           TO 転帰４.
013310     MOVE 治癒チェックＷ(4)   TO 治癒チェック４.
013320     MOVE 中止チェックＷ(4)   TO 中止チェック４.
013330     MOVE 転医チェックＷ(4)   TO 転医チェック４.
013340*     ******************
013350*     * 日数は一時保留 *
013360*     ******************
013370*     **********************
013380*     * 施術回数は一時保留 *
013390*     **********************
013400*     **********************
013410*     * 転帰名称は一時保留 *
013420*     **********************
013430**********
013440* ５部位 *
013450**********
013460*     MOVE 会部位コードＷ(5)   TO 負傷コード５.
013470     MOVE 負傷名Ｗ(5)         TO 負傷名５.
013480     MOVE 負傷年Ｗ(5)         TO 負傷年５.
013490     MOVE 負傷月Ｗ(5)         TO 負傷月５.
013500     MOVE 負傷日Ｗ(5)         TO 負傷日５.
013510     MOVE 施術開始年Ｗ(5)     TO 施術開始年５.
013520     MOVE 施術開始月Ｗ(5)     TO 施術開始月５.
013530     MOVE 施術開始日Ｗ(5)     TO 施術開始日５.
013540     MOVE 施術終了年Ｗ(5)     TO 施術終了年５.
013550     MOVE 施術終了月Ｗ(5)     TO 施術終了月５.
013560     MOVE 施術終了日Ｗ(5)     TO 施術終了日５.
013570*     MOVE 負傷年月日区切Ｗ(5) TO 区切５１ 区切５２.
013580*     MOVE 施開年月日区切Ｗ(5) TO 区切５３ 区切５４.
013590*     MOVE 施終年月日区切Ｗ(5) TO 区切５５ 区切５６.
013600*     MOVE 転帰Ｗ(5)           TO 転帰５.
013610     MOVE 治癒チェックＷ(5)   TO 治癒チェック５.
013620     MOVE 中止チェックＷ(5)   TO 中止チェック５.
013630     MOVE 転医チェックＷ(5)   TO 転医チェック５.
013640*     ******************
013650*     * 日数は一時保留 *
013660*     ******************
013670*     **********************
013680*     * 施術回数は一時保留 *
013690*     **********************
013700*     **********************
013710*     * 転帰名称は一時保留 *
013720*     **********************
013730************
013740* 負傷原因 *
013750************
013760     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
013770     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
013780     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
013790     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
013800     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
013810     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
013820     MOVE 負傷原因Ｗ(7)       TO 負傷原因７.
013830     MOVE 負傷原因Ｗ(8)       TO 負傷原因８.
013840     MOVE 負傷原因Ｗ(9)       TO 負傷原因９.
013850     MOVE 負傷原因Ｗ(10)      TO 負傷原因１０.
013860*
013870*================================================================*
013880 負担率取得 SECTION.
013890*
013900     MOVE ZERO TO 本人負担率Ｗ.
013910     MOVE ZERO TO 家族負担率Ｗ.
013920*
013930     MOVE 施術和暦ＷＲ   TO 受－施術和暦.
013940     MOVE 施術年ＷＲ     TO 受－施術年.
013950     MOVE 施術月ＷＲ     TO 受－施術月.
013960     MOVE 患者番号ＷＲ   TO 受－患者番号.
013970     MOVE 枝番ＷＲ       TO 受－枝番.
013980     READ 受診者情報Ｆ
013990     INVALID KEY
014000         MOVE  NC"　　施術月の受診者情報がありません" TO 連メ－メッセージ
014010         CALL   "MSG001"
014020         CANCEL "MSG001"
014030         PERFORM ファイル閉鎖
014040         MOVE ZERO TO PROGRAM-STATUS
014050         EXIT PROGRAM
014060     NOT INVALID KEY
014070* 14/10～　サブルーチン処理
014080         IF 受－施術和暦年月 >= 41410
014090            PERFORM 負担率取得１４１０
015260         END-IF
015270     END-READ.
015280*
015290     EVALUATE 負担率Ｗ
015300     WHEN 0
015310         MOVE NC"○" TO ０割チェックＷ
015320     WHEN 10
015330         MOVE NC"○" TO １割チェックＷ
015340     WHEN 20
015350         MOVE NC"○" TO ２割チェックＷ
015360     WHEN 30
015370         MOVE NC"○" TO ３割チェックＷ
015380     END-EVALUATE.
015390*
015400*================================================================*
015410 負担率取得１４１０ SECTION.
015420*
015430* 受診者Ｆ READ中
015440* 平成14/10～
015450     MOVE ZERO  TO 負担率Ｗ.
015460     INITIALIZE 助成負担率ＷＷ.
015470*
015480     MOVE SPACE TO 連率－負担率取得キー.
015490     INITIALIZE 連率－負担率取得キー.
015500     MOVE 受－施術和暦年月 TO 連率－施術和暦年月.
015510     MOVE 受－患者コード   TO 連率－患者コード.
015520*
015530     CALL   "HUTANRIT".
015540     CANCEL "HUTANRIT".
015550*
015560* / 本体 /
015570     MOVE 連率－実際本体負担率 TO 負担率Ｗ.
015580*
015590*
015600* / 助成 /
015610     IF 受－助成種別 NOT = ZERO
015620        MOVE 連率－助成負担率  TO 助成負担率Ｗ
015630*
015640        COMPUTE 助成負担率Ｗ１ = 助成負担率Ｗ / 10
015650        MOVE 助成負担率Ｗ１ TO  助成負担率文字Ｗ１
015660*
015670        STRING "("                 DELIMITED BY SIZE
015680               助成負担率文字Ｗ１  DELIMITED BY SIZE
015690               "割)"               DELIMITED BY SIZE
015700               INTO 助成負担率表示Ｗ
015710        END-STRING
015720*
015730     END-IF.
015740*
024750*================================================================*
       原因行セット SECTION.
      */0610
      * 指定された開始行から印刷するようにセットし直す。
      * １０行.
      *
           MOVE SPACE                  TO 負傷原因Ｗテーブル２.
           MOVE ZERO                   TO カウンタ.
           IF 連印－原因開始行 = ZERO
               MOVE 1                  TO カウンタ２
           ELSE
               MOVE 連印－原因開始行   TO カウンタ２
           END-IF.
           PERFORM VARYING カウンタ FROM 1 BY 1
                   UNTIL ( カウンタ > 10 ) OR (カウンタ２ > 10)
               MOVE 負傷原因Ｗ(カウンタ) TO 負傷原因Ｗ２(カウンタ２)
               COMPUTE カウンタ２ = カウンタ２ + 1
           END-PERFORM.
      *
           PERFORM VARYING カウンタ FROM 1 BY 1
                     UNTIL カウンタ > 10
               MOVE 負傷原因Ｗ２(カウンタ) TO 負傷原因Ｗ(カウンタ)
           END-PERFORM.
015940*================================================================*
015950 受診者情報取得 SECTION.
015960*
015970**************************************************
015980* 連結データから受診者情報Ｆより以下の情報を取得 *
015990* ● 患者番号.... 患者番号Ｗ                     *
016000* ● 枝番........ 枝番Ｗ                         *
016010* ● 記号 ....... 記号Ｗに格納                   *
016020* ● 番号 ....... 番号Ｗに格納                   *
016030* ● 被保険者カナ.被保険者カナＷに格納           *
016040* ● 被保険者氏名.被保険者氏名Ｗに格納           *
016050* ● 郵便番号１ ..郵便番号１Ｗに格納             *
016060* ● 郵便番号２ ..郵便番号２Ｗに格納             *
016070* ● 住所１ ......住所１Ｗに格納                 *
016080* ● 住所２ ......住所２Ｗに格納                 *
016090* ● 電話番号.....電話番号Ｗに格納               *
016100* ● 患者カナ ....患者カナＷに格納               *
016110* ● 患者氏名 ....患者氏名Ｗに格納               *
016120* ● 続柄 ........名称マスタより続柄Ｗに取得     *
016130* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
016140* ● 患者年 ......患者年Ｗに格納                 *
016150* ● 患者月 ......患者月Ｗに格納                 *
016160* ● 患者日 ......患者日Ｗに格納                 *
016170**************************************************
016180     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
016190     MOVE 施術年ＷＲ         TO 受－施術年.
016200     MOVE 施術月ＷＲ         TO 受－施術月.
016210     MOVE 患者コードＷＲ     TO 受－患者コード.
016220     READ 受診者情報Ｆ
016230     INVALID KEY
016240         CONTINUE
016250*            /* ありえない */
016260     NOT INVALID KEY
016270         MOVE 受－患者番号     TO 患者番号Ｗ
016280         MOVE 受－枝番         TO 枝番Ｗ
016290*         MOVE 受－記号         TO 記号Ｗ
016300*         MOVE 受－番号         TO 番号Ｗ
      *-----------------------------------------------------------------*
               MOVE SPACE TO 連暗号複合－暗号情報
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
016380         MOVE 受－被保険者カナ TO 被保険者カナＷ
016390         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
016400*
016410         EVALUATE 受－被保険者性別
016420         WHEN 1
016430             MOVE NC"○"  TO 被保険者男チェックＷ
016440         WHEN 2
016450             MOVE NC"○"  TO 被保険者女チェックＷ
016460         END-EVALUATE
016470*
016480         EVALUATE 受－被保険者和暦
016490         WHEN 1
016500             MOVE NC"○"  TO 被保険者明治チェックＷ
016510         WHEN 2
016520             MOVE NC"○"  TO 被保険者大正チェックＷ
016530         WHEN 3
016540             MOVE NC"○"  TO 被保険者昭和チェックＷ
016550         WHEN 4
016560             MOVE NC"○"  TO 被保険者平成チェックＷ
016570         END-EVALUATE
016160         MOVE 受－被保険者和暦 TO 元－元号区分
016170         READ 元号マスタ
016180         NOT INVALID KEY
016190             MOVE 元－元号名称 TO 被保険者元号Ｗ
016200         END-READ
016580         MOVE 受－被保険者年   TO 被保険者年Ｗ
016590         MOVE 受－被保険者月   TO 被保険者月Ｗ
016600         MOVE 受－被保険者日   TO 被保険者日Ｗ
016610
016620*被保険者年齢
016630     MOVE 連印－施術和暦 TO 元－元号区分
016640     READ 元号マスタ
016650     NOT INVALID KEY
016660         COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
016670     END-READ
016680     IF ( 受－被保険者和暦 NOT = ZERO ) AND
016690        ( 受－被保険者年   NOT = ZERO ) AND
016700        ( 受－被保険者月   NOT = ZERO ) AND
016710        ( 受－被保険者日   NOT = ZERO )
016720
016730         MOVE ZERO TO 被保険者年齢Ｗ
016740*
016750         MOVE 受－被保険者和暦 TO 元－元号区分
016760         READ 元号マスタ
016770         NOT INVALID KEY
016780             COMPUTE 被保険者西暦年Ｗ = 元－開始西暦年 + ( 受－被保険者年 - 1 )
016790         END-READ
016800*
016810         COMPUTE 被保険者年齢Ｗ   = 施術西暦年Ｗ - 被保険者西暦年Ｗ
016820*
016830         IF 連印－施術月 < 受－被保険者月
016840             COMPUTE 被保険者年齢Ｗ = 被保険者年齢Ｗ - 1
016850         END-IF
016860*
016870     END-IF
016880*
016890*/郵便番号 ゼロの時はスペース
016900         IF 受－郵便番号１ = "000"
016910             MOVE SPACE          TO 被保険者郵便番号１Ｗ
016920             MOVE SPACE          TO 被保険者郵便番号２Ｗ
016930         ELSE
016940             MOVE 受－郵便番号１ TO 被保険者郵便番号１Ｗ
016950             MOVE 受－郵便番号２ TO 被保険者郵便番号２Ｗ
016960         END-IF
016970         MOVE 受－電話番号     TO 被保険者電話番号Ｗ
016980         MOVE 受－住所１       TO 印刷被保険者住所１Ｗ
016990         MOVE 受－住所２       TO 印刷被保険者住所２Ｗ
017000*         STRING 受－住所１  DELIMITED BY SPACE
017010*                受－住所２  DELIMITED BY SPACE
017020*           INTO 被保険者住所Ｗ
017030*         END-STRING
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
016160             MOVE 受－患者和暦 TO 元－元号区分
016170             READ 元号マスタ
016180             NOT INVALID KEY
016190                 MOVE 元－元号名称 TO 被保険者元号Ｗ
016200             END-READ
018150             MOVE 受－患者年   TO 被保険者年Ｗ
018160             MOVE 受－患者月   TO 被保険者月Ｗ
018170             MOVE 受－患者日   TO 被保険者日Ｗ
019210             MOVE 受－患者郵便番号１ TO 被保険者郵便番号１Ｗ
019220             MOVE 受－患者郵便番号２ TO 被保険者郵便番号２Ｗ
016980             MOVE 受－患者住所１       TO 印刷被保険者住所１Ｗ
016990             MOVE 受－患者住所２       TO 印刷被保険者住所２Ｗ
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
017040*
017050*/郵便番号 ゼロの時はスペース
017060         IF 受－患者郵便番号１ = "000"
017070             MOVE SPACE              TO 患者郵便番号１Ｗ
017080             MOVE SPACE              TO 患者郵便番号２Ｗ
017090         ELSE
017100             MOVE 受－患者郵便番号１ TO 患者郵便番号１Ｗ
017110             MOVE 受－患者郵便番号２ TO 患者郵便番号２Ｗ
017120         END-IF
017130         MOVE 受－患者住所１     TO 患者住所１Ｗ
017140         MOVE 受－患者住所２     TO 患者住所２Ｗ
017150*         STRING 受－患者住所１  DELIMITED BY SPACE
017160*                受－患者住所２  DELIMITED BY SPACE
017170*           INTO 患者住所Ｗ
017180*         END-STRING
017190         MOVE 受－患者電話番号   TO 患者電話番号Ｗ
017200*
017210*電話番号分割
017220         UNSTRING 被保険者電話番号Ｗ  DELIMITED BY "-"
017230             INTO 分割電話番号１Ｗ
017240                  分割電話番号２Ｗ
017250                  分割電話番号３Ｗ
017260         END-UNSTRING
017270*電話番号右詰処理
017280         IF 分割電話番号３Ｗ = SPACE
017290             MOVE 分割電話番号２Ｗ TO 分割電話番号３Ｗ
017300             MOVE 分割電話番号１Ｗ TO 分割電話番号２Ｗ
017310             MOVE SPACE            TO 分割電話番号１Ｗ
017320         END-IF
017330*
017340         MOVE 受－患者カナ     TO 患者カナＷ
017350         MOVE 受－患者氏名     TO 患者氏名Ｗ
017360*
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
017370         MOVE 事－事業所名称   TO 事業所名称Ｗ
017380         MOVE 事－事業所住所１ TO 印刷事業所住所１Ｗ
017390         MOVE 事－事業所住所２ TO 印刷事業所住所２Ｗ
017440* 続柄
017450         IF 受－本人家族区分 = 1
017460             MOVE NC"本人"     TO 続柄Ｗ
017470             MOVE NC"○"       TO 本人チェックＷ
017480         ELSE
017490             MOVE 05          TO 名－区分コード
017500             MOVE 受－続柄    TO 名－名称コード
017510             READ 名称マスタ
017520             INVALID KEY
017530                 MOVE SPACE   TO 続柄Ｗ
017540             NOT INVALID KEY
017550                 MOVE 名－略称  TO 続柄Ｗ
017560             END-READ
017570             MOVE NC"○"       TO 家族チェックＷ
017580         END-IF
017590* 患者和暦年月日
017600         EVALUATE 受－患者和暦
017610         WHEN 1
017620             MOVE NC"○"  TO 明治チェックＷ
017630         WHEN 2
017640             MOVE NC"○"  TO 大正チェックＷ
017650         WHEN 3
017660             MOVE NC"○"  TO 昭和チェックＷ
017670         WHEN 4
017680             MOVE NC"○"  TO 平成チェックＷ
017690         END-EVALUATE
016160         MOVE 受－患者和暦 TO 元－元号区分
016170         READ 元号マスタ
016180         NOT INVALID KEY
016190             MOVE 元－元号名称 TO 患者元号Ｗ
016200         END-READ
017700         MOVE 受－患者年  TO 患者年Ｗ
017710         MOVE 受－患者月  TO 患者月Ｗ
017720         MOVE 受－患者日  TO 患者日Ｗ
017730*患者年齢
017740     MOVE 連印－施術和暦 TO 元－元号区分
017750     READ 元号マスタ
017760     NOT INVALID KEY
017770         COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
017780     END-READ
017790     IF ( 受－患者和暦 NOT = ZERO ) AND
017800        ( 受－患者年   NOT = ZERO ) AND
017810        ( 受－患者月   NOT = ZERO ) AND
017820        ( 受－患者日   NOT = ZERO )
017830*
017840         MOVE ZERO TO 患者年齢Ｗ
017850*
017860         MOVE 受－患者和暦 TO 元－元号区分
017870         READ 元号マスタ
017880         NOT INVALID KEY
017890             COMPUTE 患者西暦年Ｗ = 元－開始西暦年 + ( 受－患者年 - 1 )
017900         END-READ
017910*
017920         COMPUTE 患者年齢Ｗ   = 施術西暦年Ｗ - 患者西暦年Ｗ
017930*
017940         IF 連印－施術月 < 受－患者月
017950             COMPUTE 患者年齢Ｗ = 患者年齢Ｗ - 1
017960         END-IF
017970*
017980     END-IF
017990* 患者性別
018000*         EVALUATE 受－患者性別
018010*         WHEN 1
018020*             MOVE NC"（男）" TO 患者性別Ｗ
018030*         WHEN 2
018040*             MOVE NC"（女）" TO 患者性別Ｗ
018050*         END-EVALUATE
018060         EVALUATE 受－患者性別
018070         WHEN 1
018080             MOVE NC"○"  TO 患者男チェックＷ
018090         WHEN 2
018100             MOVE NC"○"  TO 患者女チェックＷ
018110         END-EVALUATE
018120*/有効期限
018130         MOVE 受－有効和暦   TO 有効期限和暦Ｗ
018140         MOVE 受－有効年     TO 有効期限年Ｗ
018150         MOVE 受－有効月     TO 有効期限月Ｗ
018160         MOVE 受－有効日     TO 有効期限日Ｗ
018510         IF (有効期限和暦Ｗ NOT = ZERO) AND
018520            (有効期限年Ｗ   NOT = ZERO) AND
018530            (有効期限月Ｗ   NOT = ZERO) AND
018540            (有効期限日Ｗ   NOT = ZERO)
018170            MOVE 有効期限和暦Ｗ TO 元－元号区分
018180            READ 元号マスタ
018190            INVALID KEY
018200               MOVE SPACE        TO 有効期限元号Ｗ
018210            NOT INVALID KEY
018220               MOVE 元－元号名称 TO 有効期限元号Ｗ
018230           END-READ
               END-IF
018240*/資格取得年月日
018250         MOVE 受－資格和暦 TO 資格取得和暦Ｗ
018260         MOVE 受－資格年   TO 資格取得年Ｗ
018270         MOVE 受－資格月   TO 資格取得月Ｗ
018280         MOVE 受－資格日   TO 資格取得日Ｗ
018290         IF (資格取得和暦Ｗ NOT = ZERO) AND
018300            (資格取得年Ｗ   NOT = ZERO) AND
018310            (資格取得月Ｗ   NOT = ZERO) AND
018320            (資格取得日Ｗ   NOT = ZERO)
018330             EVALUATE 資格取得和暦Ｗ
018340             WHEN 3
018350                 MOVE NC"○"  TO 資格昭和チェックＷ
018360             WHEN 4
018370                 MOVE NC"○"  TO 資格平成チェックＷ
018380             END-EVALUATE
016160             MOVE 資格取得和暦Ｗ TO 元－元号区分
016170             READ 元号マスタ
016180             NOT INVALID KEY
016190                 MOVE 元－元号名称 TO 資格取得元号Ｗ
016200             END-READ
018390         END-IF
018400**市町村国保､退職の場合は有効期限を出力
018410**業種別国保(国保組合)､共済､日雇､船員､社保､組合､自衛官は有効期限日を出力する
018420*         IF (( 受－保険種別 = 08 ) OR
018430*            (( 受－保険種別 = 01 ) AND
018440*            (  受－保険者番号(3:1) NOT = "3" )))
018450*             MOVE 受－有効和暦 TO 資格取得和暦Ｗ
018460*             MOVE 受－有効年   TO 資格取得年Ｗ
018470*             MOVE 受－有効月   TO 資格取得月Ｗ
018480*             MOVE 受－有効日   TO 資格取得日Ｗ
018490*         ELSE
018500*             MOVE 受－資格和暦 TO 資格取得和暦Ｗ
018510*             MOVE 受－資格年   TO 資格取得年Ｗ
018520*             MOVE 受－資格月   TO 資格取得月Ｗ
018530*             MOVE 受－資格日   TO 資格取得日Ｗ
018540*         END-IF
018550*         IF (資格取得和暦Ｗ NOT = ZERO) AND
018560*            (資格取得年Ｗ   NOT = ZERO) AND
018570*            (資格取得月Ｗ   NOT = ZERO) AND
018580*            (資格取得日Ｗ   NOT = ZERO)
018590*             EVALUATE 資格取得和暦Ｗ
018600*             WHEN 3
018610*                 MOVE NC"○"  TO 資格昭和チェックＷ
018620*             WHEN 4
018630*                 MOVE NC"○"  TO 資格平成チェックＷ
018640*             END-EVALUATE
018650*         END-IF
018660* 市町村番号 受給者番号
018670         IF 受－助成種別 NOT = ZERO
018680             MOVE 受－費用負担者番号助成 TO 費用負担者番号Ｗ
018690             MOVE 受－受益者番号助成     TO 受益者番号助成Ｗ
018700         END-IF
018710         IF ( 受－公費種別 = 05 ) AND ( 受－施術和暦年月 < 42004 )
018720             MOVE 受－費用負担者番号     TO 市町村番号Ｗ
018730             MOVE 受－受益者番号老人     TO 受益者番号Ｗ
018740         END-IF
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
017380            MOVE 労災－労災事業所住所１     TO 印刷事業所住所１Ｗ
017390            MOVE 労災－労災事業所住所２     TO 印刷事業所住所２Ｗ
012130            MOVE 労災－労働保険番号         TO 番号Ｗ
               END-IF
      *
018750*保険種別
018760         EVALUATE 受－助成種別
018770         WHEN 50
018780             MOVE NC"○"  TO 生保チェックＷ
018790         WHEN 51
018800*/頭4桁が "4113"東京 "4108"茨城 "4132"島根 の時は、「福」。それ以外は「老」
018810             IF  受－費用負担者番号助成(1:4) = "4113" OR
018820                 受－費用負担者番号助成(1:4) = "4108" OR
018830                 受－費用負担者番号助成(1:4) = "4132"
018840                 MOVE NC"○"  TO 公費負担チェックＷ
018850*                 MOVE NC"○"    TO 福チェックＷ
018860             ELSE
018870                 MOVE NC"○"  TO 公費負担チェックＷ
018880*                 MOVE NC"○"    TO 老人チェックＷ
018890             END-IF
018900         WHEN 52
018910             MOVE NC"○"  TO 公費負担チェックＷ
018920*             MOVE NC"○"  TO 母子チェックＷ
018930         WHEN 53
018940             MOVE NC"○"  TO 公費負担チェックＷ
018950*             MOVE NC"○"  TO 身障チェックＷ
018960         WHEN 54
018970             MOVE NC"○"  TO 公費負担チェックＷ
018980*             MOVE NC"○"  TO 原爆チェックＷ
018990         WHEN 55
019000             MOVE NC"○"  TO 公費負担チェックＷ
019010*             MOVE NC"○"  TO 乳幼児チェックＷ
019020         END-EVALUATE
019030
019040         IF 受－公費種別 = 05
019050             MOVE NC"○"  TO 老人チェックＷ
019060         ELSE
019070             EVALUATE 受－保険種別
019080             WHEN 01
019090                 MOVE NC"○"  TO 国保チェックＷ
019100             WHEN 02
019110                 MOVE NC"○"  TO 健保チェックＷ
019120*                 MOVE NC"○"  TO 社保チェックＷ
019130             WHEN 03
019140                 MOVE NC"○"  TO 健保チェックＷ
019150*                 MOVE NC"○"  TO 組合チェックＷ
019160             WHEN 04
019170                 MOVE NC"○"  TO 共済チェックＷ
019180             WHEN 06
019190                 MOVE NC"○"  TO 健保チェックＷ
019200*                 MOVE NC"○"  TO 日雇チェックＷ
019210             WHEN 07
019220                 MOVE NC"○"  TO 船員チェックＷ
019230             WHEN 08
019240                 MOVE NC"○"  TO 退職チェックＷ
019250             WHEN 09
019260                  MOVE NC"○"  TO 自衛官チェックＷ
019270*                 MOVE NC"○"  TO 共済チェックＷ
019280             WHEN 70
019290                 MOVE NC"○"  TO 労災チェックＷ
019300             WHEN 80
019310                 MOVE NC"○"  TO 自賠チェックＷ
019320             WHEN 90
019330                 MOVE NC"○"  TO 自費チェックＷ
019340             END-EVALUATE
019350         END-IF
019360     END-READ.
019370*HILO テスト－－－－－－－－－－－
019380**      MOVE NC"○"  TO 被保険者明治Ｗ.
019390**      MOVE NC"○"  TO 被保険者大正Ｗ.
019400**      MOVE NC"○"  TO 被保険者昭和Ｗ.
019410**      MOVE NC"○"  TO 被保険者男チェックＷ.
019420**      MOVE NC"○"  TO 被保険者女チェックＷ.
019430*      MOVE NC"○"  TO 患者男チェックＷ.
019440*      MOVE NC"○"  TO 患者女チェックＷ.
019450*      MOVE NC"○"  TO 明治チェックＷ.
019460*      MOVE NC"○"  TO 大正チェックＷ.
019470*      MOVE NC"○"  TO 昭和チェックＷ.
019480*      MOVE NC"○"  TO 平成チェックＷ.
019490*      MOVE NC"○"  TO 健保チェックＷ    .
019500*      MOVE NC"○"  TO 社保チェックＷ    .
019510*      MOVE NC"○"  TO 組合チェックＷ    .
019520*      MOVE NC"○"  TO 共済チェックＷ    .
019530*      MOVE NC"○"  TO 国保チェックＷ    .
019540*      MOVE NC"○"  TO 日雇チェックＷ    .
019550*      MOVE NC"○"  TO 船員チェックＷ    .
019560*      MOVE NC"○"  TO 老人チェックＷ    .
019570**      MOVE NC"○"  TO 福チェックＷ    .
019580**      MOVE NC"○"  TO 身障チェックＷ    .
019590*      MOVE NC"○"  TO 退職チェックＷ    .
019600**      MOVE NC"○"  TO 母子チェックＷ    .
019610**      MOVE NC"○"  TO 乳幼児チェックＷ  .
019620**      MOVE NC"○"  TO 国保組合チェックＷ.
019630*      MOVE NC"○"  TO 労災チェックＷ    .
019640*      MOVE NC"○"  TO 生保チェックＷ    .
019650*      MOVE NC"○"  TO 自賠チェックＷ    .
019660*      MOVE NC"○"  TO 自衛官チェックＷ    .
019670*      MOVE NC"○"  TO 公費負担チェックＷ    .
019680*      MOVE NC"○"  TO 自費チェックＷ    .
019690**      MOVE NC"○"  TO 原爆チェックＷ    .
019700*      MOVE NC"○" TO ０割チェックＷ.
019710*      MOVE NC"○" TO １割チェックＷ.
019720*      MOVE NC"○" TO ２割チェックＷ.
019730*      MOVE NC"○" TO ３割チェックＷ.
019740*      MOVE NC"○"  TO 本人チェックＷ    .
019750*      MOVE NC"○"  TO 家族チェックＷ    .
019760*      MOVE NC"○"  TO 資格昭和チェックＷ.
019770*      MOVE NC"○"  TO 資格平成チェックＷ.
019780*      MOVE NC"○"  TO 被保険者明治チェックＷ.
019790*      MOVE NC"○"  TO 被保険者大正チェックＷ.
019800*      MOVE NC"○"  TO 被保険者昭和チェックＷ.
019810*      MOVE NC"○"  TO 被保険者平成チェックＷ.
019820*-------------------------------------
019830*================================================================*
019840 請求先情報取得 SECTION.
019850*
019860****************************************************
019870* 連結データから保険者マスタより請求先を取得する。 *
019880* ※保－請求先情報区分=1の場合請求先マスタを使用   *
019890* ● 保険者番号...保険者番号Ｗに格納               *
019900* ● 名称........ 請求先名称Ｗに格納               *
019910* ● 郵便番号１.. 請求先郵便番号１Ｗに格納         *
019920* ● 郵便番号２.. 請求先郵便番号２Ｗに格納         *
019930* ● 住所１.......請求先住所１Ｗに格納             *
019940* ● 住所２.......請求先住所２Ｗに格納             *
019950****************************************************
019960     MOVE SPACE          TO 保険者番号Ｗ.
019970     MOVE SPACE          TO 請求先名称Ｗ.
019980     MOVE SPACE          TO 支部名Ｗ.
019990     MOVE SPACE          TO 請求先郵便番号１Ｗ.
020000     MOVE SPACE          TO 請求先郵便番号２Ｗ.
020010     MOVE SPACE          TO 請求先住所１Ｗ.
020020     MOVE SPACE          TO 請求先住所２Ｗ.
020030     MOVE SPACE          TO 印刷保険者呼名Ｗ.
020040     MOVE SPACE          TO 請求先名支部名Ｗ
020050*
020060     MOVE 保険種別ＷＲ   TO 保－保険種別.
020070     MOVE 保険者番号ＷＲ TO 保－保険者番号.
020080     READ 保険者マスタ
020090     INVALID KEY
               IF ( 保険種別ＷＲ = 05 ) AND ( 受－施術和暦年月 >= 42004 )
020060             MOVE 保険種別ＷＲ   TO 市－公費種別
020070             MOVE 保険者番号ＷＲ TO 市－市町村番号
020080             READ 市町村マスタ
020090             INVALID KEY
020100                 MOVE SPACE      TO 請求先名称Ｗ
020110                 MOVE SPACE      TO 支部名Ｗ
020120                 MOVE ZERO       TO 接尾語区分Ｗ
020130             NOT INVALID KEY
020140                 MOVE 市－市町村番号  TO 保険者番号Ｗ
020150                 MOVE 市－市町村名称  TO 請求先名称Ｗ
020160                 MOVE 市－支部部署名  TO 支部名Ｗ
020170                 MOVE 市－郵便番号１  TO 請求先郵便番号１Ｗ
020180                 MOVE 市－郵便番号２  TO 請求先郵便番号２Ｗ
020190                 MOVE 市－住所１      TO 請求先住所１Ｗ
020200                 MOVE 市－住所２      TO 請求先住所２Ｗ
                   END-READ
               ELSE
020100             MOVE SPACE      TO 請求先名称Ｗ
020110             MOVE SPACE      TO 支部名Ｗ
020120             MOVE ZERO       TO 接尾語区分Ｗ
               END-IF
020130     NOT INVALID KEY
020140         MOVE 保－保険者番号  TO 保険者番号Ｗ
020150         MOVE 保－保険者名称  TO 請求先名称Ｗ
020160         MOVE 保－支部部署名  TO 支部名Ｗ
020170         MOVE 保－郵便番号１  TO 請求先郵便番号１Ｗ
020180         MOVE 保－郵便番号２  TO 請求先郵便番号２Ｗ
020190         MOVE 保－住所１      TO 請求先住所１Ｗ
020200         MOVE 保－住所２      TO 請求先住所２Ｗ
020210         MOVE 保－接尾語区分  TO 接尾語区分Ｗ
020220     END-READ.
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
020230     EVALUATE 保険種別ＷＲ
020240     WHEN 2
020250     WHEN 6
020260         IF 接尾語区分Ｗ = 1
020270            MOVE SPACE              TO 印刷保険者呼名Ｗ
020280         ELSE
020290            MOVE "社会保険事務所"   TO 印刷保険者呼名Ｗ
020300         END-IF
020310     WHEN 3
020320         MOVE "健康保険組合"   TO 印刷保険者呼名Ｗ
020330     WHEN OTHER
020340         MOVE SPACE TO 印刷保険者呼名Ｗ
020350     END-EVALUATE.
020360     IF 印刷保険者呼名Ｗ NOT = SPACE
020370         STRING 請求先名称Ｗ DELIMITED BY SPACE
020380                保険者呼名Ｗ DELIMITED BY SPACE
020390           INTO 請求先名称Ｗ
020400         END-STRING
020410     END-IF.
020420*組合と共済は支部名も印字する
020430     EVALUATE 保険種別ＷＲ
020440     WHEN 03
020450     WHEN 04
020460         STRING 請求先名称Ｗ DELIMITED BY SPACE
020470                "  "         DELIMITED BY SIZE
020480                支部名Ｗ     DELIMITED BY SPACE
020490           INTO 請求先名支部名Ｗ
020500         END-STRING
020510     WHEN OTHER
020520         MOVE 請求先名称Ｗ TO 請求先名支部名Ｗ
020530     END-EVALUATE.
020540*================================================================*
020550 負傷データ取得 SECTION.
020560*
020570**************************************************
020580* 連結データから負傷データＦより以下の情報を取得 *
020590* ● 負傷名...部位＋負傷種別にて加工して格納     *
020600* ● 負傷年.......負傷年Ｗ                       *
020610* ● 負傷月.......負傷月Ｗ                       *
020620* ● 負傷日.......負傷日Ｗ                       *
020630* ● 施術終了年...終了年Ｗ                       *
020640* ● 施術終了月...終了月Ｗ                       *
020650* ● 施術終了日...終了日Ｗ                       *
020660**************************************************
020670     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
020680     MOVE 施術年ＷＲ         TO 負－施術年.
020690     MOVE 施術月ＷＲ         TO 負－施術月.
020700     MOVE 患者コードＷＲ     TO 負－患者コード.
020710     READ 負傷データＦ
020720     INVALID KEY
020730         CONTINUE
020740*            /* ありえない */
020750     NOT INVALID KEY
020760         MOVE ZERO                         TO 部位数Ｗ
020770         MOVE 負－部位数                   TO 部位数Ｗ
020780         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
020790                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
020800*********************************************
020810* 注）全柔...負傷種別＋部位にて加工して格納 *
020820*********************************************
020830* 負傷種別
020840             MOVE SPACE                     TO 負傷名称Ｗ
020850             MOVE 03                        TO 名－区分コード
020860             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
020870             READ 名称マスタ
020880             INVALID KEY
020890                 MOVE SPACE    TO 負傷名称Ｗ
020900             NOT INVALID KEY
020910*                 MOVE 名－略称 TO 負傷名称Ｗ
020920                 MOVE 名－正式名称 TO 負傷名称Ｗ
020930             END-READ
020940* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
020960             PERFORM 名称埋込処理
021020             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
021030             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
021040             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
021050*             IF 負－負傷年(部位ＣＮＴ) NOT = ZERO 
021060*                MOVE "."                   TO 負傷年月日区切Ｗ(部位ＣＮＴ)
021070*             END-IF
021080*
021090             MOVE 負－開始年(部位ＣＮＴ)   TO 施術開始年Ｗ(部位ＣＮＴ)
021100             MOVE 負－開始月(部位ＣＮＴ)   TO 施術開始月Ｗ(部位ＣＮＴ)
021110             MOVE 負－開始日(部位ＣＮＴ)   TO 施術開始日Ｗ(部位ＣＮＴ)
021120*             IF 負－開始年(部位ＣＮＴ) NOT = ZERO 
021130*                MOVE "."                   TO 施開年月日区切Ｗ(部位ＣＮＴ)
021140*             END-IF
021150*
021160             MOVE 負－終了年(部位ＣＮＴ)   TO 施術終了年Ｗ(部位ＣＮＴ)
021170             MOVE 負－終了月(部位ＣＮＴ)   TO 施術終了月Ｗ(部位ＣＮＴ)
021180             MOVE 負－終了日(部位ＣＮＴ)   TO 施術終了日Ｗ(部位ＣＮＴ)
021190*             IF 負－終了年(部位ＣＮＴ) NOT = ZERO 
021200*                MOVE "."                   TO 施終年月日区切Ｗ(部位ＣＮＴ)
021210*             END-IF
021220*転帰
021230             EVALUATE 負－転帰区分(部位ＣＮＴ)
021240             WHEN 1
021250             WHEN 2
021260                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
021270             WHEN 3
021280                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
021290             WHEN 4
021300                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
021310             END-EVALUATE
021320*             IF 負－転帰区分(部位ＣＮＴ) NOT = 9
021330*                 MOVE 04                        TO 名－区分コード
021340*                 MOVE 負－転帰区分(部位ＣＮＴ)  TO 名－名称コード
021350*                 READ 名称マスタ
021360*                 INVALID KEY
021370*                     MOVE SPACE    TO 転帰Ｗ(部位ＣＮＴ)
021380*                 NOT INVALID KEY
021390*                     MOVE 名－略称 TO 転帰Ｗ(部位ＣＮＴ)
021400*                 END-READ
021410*             END-IF
021420*hilo---------------------------------
021430*         MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
021440*         MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
021450*         MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
021460*-------------------------------------
021470             PERFORM 負傷データ退避
021480         END-PERFORM
021490     END-READ.
021500*================================================================*
021510 施術記録取得 SECTION.
021520*
021530************************************************************
021540* 連結データから負傷データＦより以下の情報を取得           *
021550* ● 施術開始年月日...該当する部位に対して当月最初の施術日 *
021560************************************************************
021570     MOVE 施術和暦ＷＲ  TO 施記－施術和暦.
021580     MOVE 施術年ＷＲ    TO 施記－施術年.
021590     MOVE 施術月ＷＲ    TO 施記－施術月.
021600     MOVE ZERO            TO 施記－施術日.
021610     MOVE ZERO            TO 施記－患者番号.
021620     MOVE SPACE           TO 施記－枝番.
021630     START 施術記録Ｆ   KEY IS >= 施記－施術和暦年月日
021640                                  施記－患者コード.
021650     IF 状態キー = "00"
021660         MOVE SPACE  TO 終了フラグ２
021670         PERFORM 施術記録Ｆ読込
021680         PERFORM UNTIL ( 終了フラグ２       = "YES" ) OR
021690                       ( 施記－施術和暦 NOT = 施術和暦ＷＲ ) OR
021700                       ( 施記－施術年   NOT = 施術年ＷＲ   ) OR
021710                       ( 施記－施術月   NOT = 施術月ＷＲ   )
021720*            **************
021730*            * 開始年月日 *
021740*            **************
021750             PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
021760                     UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
021770            EVALUATE TRUE ALSO TRUE ALSO TRUE ALSO TRUE ALSO TRUE
021780            WHEN 施記－負傷種別(部位ＣＮＴ) = 負傷種別Ｗ(部位ＣＮＴ) ALSO
021790                 施記－部位(部位ＣＮＴ)     = 部位Ｗ(部位ＣＮＴ)     ALSO
021800                 施記－左右区分(部位ＣＮＴ) = 左右区分Ｗ(部位ＣＮＴ) ALSO
021810                 施記－負傷位置番号(部位ＣＮＴ)
021820                                       = 負傷位置番号Ｗ(部位ＣＮＴ)  ALSO
021830                 開始年月日取得フラグ(部位ＣＮＴ) = SPACE
021840                   MOVE 施記－施術年     TO 施術開始年Ｗ(部位ＣＮＴ)
021850                   MOVE 施記－施術月     TO 施術開始月Ｗ(部位ＣＮＴ)
021860                   MOVE 施記－施術日     TO 施術開始日Ｗ(部位ＣＮＴ)
021870                   MOVE "YES"       TO 開始年月日取得フラグ(部位ＣＮＴ)
021880*
021890            WHEN OTHER
021900                CONTINUE
021910            END-EVALUATE
021920             END-PERFORM
021930             PERFORM 施術記録Ｆ読込
021940         END-PERFORM
021950     END-IF.
021960*================================================================*
021970 施術記録Ｆ読込 SECTION.
021980*
021990     READ 施術記録Ｆ NEXT
022000     AT END
022010         MOVE "YES" TO 終了フラグ２
022020     END-READ.
022030*================================================================*
022040 負傷原因取得 SECTION.
022050*
022060********************************************************************
022070*  負傷原因コードが同じものは、1行にまとめて印字する。
022080*  例: ①② 家で転んだ.
022090*     負傷原因コードが同じものをまとめ、テーブルにセット
022100*     (ただし、部位を飛んで同じものは、2行になる)
022110********************************************************************
022120     MOVE  ZERO   TO  カウンタ カウンタ２.
022130     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
      */登録部位数以上の行に印刷出来ない不具合の修正/090311
022140*             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
022140             UNTIL ( 部位ＣＮＴ > 7 )
022150*
022160****        IF ( 負傷原因患者番号Ｗ(部位ＣＮＴ)  NOT = ZERO )  AND
022170        IF ( 負傷原因連番Ｗ(部位ＣＮＴ)      NOT = ZERO )
022180*
022190           IF カウンタ = ZERO
022200               MOVE 1   TO  カウンタ カウンタ２
022210               MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
022220               MOVE 負傷原因連番Ｗ(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
022230               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
022240           ELSE
022250              IF ( 負傷原因患者番号Ｗ(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
022260                 ( 負傷原因連番Ｗ(部位ＣＮＴ)      = 負傷連番ＣＷ     )
022270                 COMPUTE カウンタ２ = カウンタ２  +  1
022280                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
022290              ELSE
022300                 COMPUTE カウンタ = カウンタ  +  1
022310                 MOVE 1   TO  カウンタ２
022320                 MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
022330                 MOVE 負傷原因連番Ｗ(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
022340                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
022350              END-IF
022360           END-IF
022370        END-IF
022380     END-PERFORM.
022390**************************************************************************
022400*  負傷原因マスタより文章取得
022410**************************************************************************
022420     MOVE  ZERO   TO  カウンタ カウンタ２.
022430     PERFORM VARYING カウンタ FROM 1 BY 1
022440             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
022450** 健保は 区分 01
022460*         MOVE 01                        TO 負原－区分コード
      */健保：０１、労災：０２、自賠責：０３
               EVALUATE 受－保険種別
               WHEN 70
                   MOVE 2 TO 負原－区分コード
               WHEN 80
                   MOVE 3 TO 負原－区分コード
               WHEN OTHER
                   MOVE 1 TO 負原－区分コード
               END-EVALUATE
022470         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
022480         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
022490         READ 負傷原因Ｆ
022500         NOT INVALID KEY
022510             INITIALIZE 負傷原因ＷＴ
022520             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
022530             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
022540             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
022550             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
022560             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
022570             PERFORM VARYING カウンタ２ FROM 1 BY 1
022580                     UNTIL ( カウンタ２ > 9 )  OR 
022590                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
022600                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
022610                WHEN 1
022620                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
022630                WHEN 2
022640                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
022650                WHEN 3
022660                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
022670                WHEN 4
022680                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
022690                WHEN 5
022700                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
022710                WHEN OTHER
022720                   CONTINUE
022730                END-EVALUATE
022740             END-PERFORM
022750*
022832             IF 負原－負傷原因入力区分 = 1
022833                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
022834                        負傷原因１ＷＴ  DELIMITED BY SIZE
022835                        負傷原因２ＷＴ  DELIMITED BY SIZE
022836                        負傷原因３ＷＴ  DELIMITED BY SIZE
022837                        負傷原因４ＷＴ  DELIMITED BY SIZE
022838                        負傷原因５ＷＴ  DELIMITED BY SIZE
022839                        INTO 負傷原因内容合成Ｗ(カウンタ)
022840                 END-STRING
022841             ELSE
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
022850             END-IF
022851*
022852         END-READ
022853     END-PERFORM.
022860*
022870     PERFORM 負傷原因セット.
022880*
022890*================================================================*
022900 詳細モード負傷原因処理 SECTION.
022910*
022920*******************************************************************
022930* 連印－負傷印刷行により、負傷原因データの並び替えを行う。
022940* ( 負傷原因情報Ｗを初期化し、退避済みの負傷原因情報ＳＷから
022950*         並び替えを行い、負傷原因情報Ｗを再設定する。)
022960*******************************************************************
022970*
022980     INITIALIZE 負傷原因情報Ｗ.
022990*
023000     PERFORM VARYING 詳細ＣＮＴ FROM 1 BY 1
023010                                UNTIL ( 詳細ＣＮＴ > 5)
023020*
023030        IF ( 連印－負傷印刷行(詳細ＣＮＴ) NOT = ZERO) AND
023040           ( 連印－原因印刷区分(詳細ＣＮＴ)   = 1)
023050*
023060           MOVE 連印－負傷印刷行(詳細ＣＮＴ) TO 印刷行ＳＷ
023070*
023080           MOVE 負傷患者番号ＳＷ(詳細ＣＮＴ) TO 負傷原因患者番号Ｗ(印刷行ＳＷ)
023090           MOVE 負傷連番ＳＷ(詳細ＣＮＴ) TO 負傷原因連番Ｗ(印刷行ＳＷ)
023100        END-IF
023110*
023120     END-PERFORM.
023130*
023140*================================================================*
023150 詳細モード負傷処理 SECTION.
023160*
023170*******************************************************************
023180* 連印－負傷印刷行により、負傷データの並び替えを行う。
023190* ( 負傷情報Ｗを初期化し、退避済みの負傷情報ＳＷから
023200*         並び替えを行い、負傷情報Ｗを再設定する。)
023210*******************************************************************
023220*
023230     INITIALIZE 負傷情報Ｗ.
023240*
023250     PERFORM VARYING 詳細ＣＮＴ FROM 1 BY 1
023260                                UNTIL ( 詳細ＣＮＴ > 5)
023270*
023280        IF 連印－負傷印刷行(詳細ＣＮＴ) NOT = ZERO
023290*
023300           MOVE 連印－負傷印刷行(詳細ＣＮＴ) TO 印刷行ＳＷ
023310*
023320           IF 連印－部位印刷区分(詳細ＣＮＴ) = 1
023330*
023340              MOVE 負傷名ＳＷ(詳細ＣＮＴ) TO 負傷名Ｗ(印刷行ＳＷ)
023350              MOVE 負傷年ＳＷ(詳細ＣＮＴ) TO 負傷年Ｗ(印刷行ＳＷ)
023360              MOVE 負傷月ＳＷ(詳細ＣＮＴ) TO 負傷月Ｗ(印刷行ＳＷ)
023370              MOVE 負傷日ＳＷ(詳細ＣＮＴ) TO 負傷日Ｗ(印刷行ＳＷ)
023380              MOVE 施術開始年ＳＷ(詳細ＣＮＴ) TO 施術開始年Ｗ(印刷行ＳＷ)
023390              MOVE 施術開始月ＳＷ(詳細ＣＮＴ) TO 施術開始月Ｗ(印刷行ＳＷ)
023400              MOVE 施術開始日ＳＷ(詳細ＣＮＴ) TO 施術開始日Ｗ(印刷行ＳＷ)
023410           END-IF
023420*
023430           IF 連印－転帰印刷区分(詳細ＣＮＴ) = 1
023440              MOVE 施術終了年ＳＷ(詳細ＣＮＴ) TO 施術終了年Ｗ(印刷行ＳＷ)
023450              MOVE 施術終了月ＳＷ(詳細ＣＮＴ) TO 施術終了月Ｗ(印刷行ＳＷ)
023460              MOVE 施術終了日ＳＷ(詳細ＣＮＴ) TO 施術終了日Ｗ(印刷行ＳＷ)
023470              MOVE 治癒チェックＳＷ(詳細ＣＮＴ) TO 治癒チェックＷ(印刷行ＳＷ)
023480              MOVE 中止チェックＳＷ(詳細ＣＮＴ) TO 中止チェックＷ(印刷行ＳＷ)
023490              MOVE 転医チェックＳＷ(詳細ＣＮＴ) TO 転医チェックＷ(印刷行ＳＷ)
023500           END-IF
023510        END-IF
023520*
023530     END-PERFORM.
023540*
023550*================================================================*
023560 負傷データ退避 SECTION.
023570*
023580*********************************************************************
023590* ① 負傷原因取得のため、負傷データＦから負傷原因Ｗに退避する。
023600* ② 詳細モード用  連印－負傷行印刷により、印刷位置を並び替えるため、
023610*                  負傷情報Ｗ  から負傷情報ＳＷに、
023620*                  負傷原因Ｗ  から負傷原因ＳＷに退避する。
023630*********************************************************************
023640*
023650     MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷原因患者番号Ｗ(部位ＣＮＴ).
023660     MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷原因連番Ｗ(部位ＣＮＴ).
023670*--------------------------------------------------------------------*
023680     MOVE 負傷名Ｗ(部位ＣＮＴ) TO 負傷名ＳＷ(部位ＣＮＴ).
023690*
023700     MOVE 負傷年Ｗ(部位ＣＮＴ) TO 負傷年ＳＷ(部位ＣＮＴ).
023710     MOVE 負傷月Ｗ(部位ＣＮＴ) TO 負傷月ＳＷ(部位ＣＮＴ).
023720     MOVE 負傷日Ｗ(部位ＣＮＴ) TO 負傷日ＳＷ(部位ＣＮＴ).
023730*
023740     MOVE 施術開始年Ｗ(部位ＣＮＴ) TO 施術開始年ＳＷ(部位ＣＮＴ).
023750     MOVE 施術開始月Ｗ(部位ＣＮＴ) TO 施術開始月ＳＷ(部位ＣＮＴ).
023760     MOVE 施術開始日Ｗ(部位ＣＮＴ) TO 施術開始日ＳＷ(部位ＣＮＴ).
023770*
023780     MOVE 施術終了年Ｗ(部位ＣＮＴ) TO 施術終了年ＳＷ(部位ＣＮＴ).
023790     MOVE 施術終了月Ｗ(部位ＣＮＴ) TO 施術終了月ＳＷ(部位ＣＮＴ).
023800     MOVE 施術終了日Ｗ(部位ＣＮＴ) TO 施術終了日ＳＷ(部位ＣＮＴ).
023810*
023820     MOVE 治癒チェックＷ(部位ＣＮＴ) TO 治癒チェックＳＷ(部位ＣＮＴ).
023830     MOVE 中止チェックＷ(部位ＣＮＴ) TO 中止チェックＳＷ(部位ＣＮＴ).
023840     MOVE 転医チェックＷ(部位ＣＮＴ) TO 転医チェックＳＷ(部位ＣＮＴ).
023850*     MOVE 施術累計回数Ｗ(部位ＣＮＴ) TO 施術累計回数ＳＷ(部位ＣＮＴ).
023860*
023870     MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号ＳＷ(部位ＣＮＴ).
023880     MOVE 負傷原因連番Ｗ(部位ＣＮＴ)     TO 負傷連番ＳＷ(部位ＣＮＴ).
023890*
023900*================================================================*
023910 負傷原因セット SECTION.
023920*
023930**************************************************************************
023940*  文章が1行を超える時は、複数行に分解する。
023950**************************************************************************
023960     MOVE  ZERO   TO  カウンタ カウンタ２.
023970     PERFORM VARYING カウンタ FROM 1 BY 1
023980             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
023990*
024000          INITIALIZE 負傷原因内容分解ＸＷ
024010          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
024020          IF  負傷原因内容１ＸＷ  NOT = SPACE
024030              COMPUTE カウンタ２ = カウンタ２  +  1
024040              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
024050          END-IF
024060          IF  負傷原因内容２ＸＷ  NOT = SPACE
024070              COMPUTE カウンタ２ = カウンタ２  +  1
024080              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
024090          END-IF
024100          IF  負傷原因内容３ＸＷ  NOT = SPACE
024110              COMPUTE カウンタ２ = カウンタ２  +  1
024120              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
024130          END-IF
024140          IF  負傷原因内容４ＸＷ  NOT = SPACE
024150              COMPUTE カウンタ２ = カウンタ２  +  1
024160              MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
024170          END-IF
024140          IF  負傷原因内容５ＸＷ  NOT = SPACE
024150              COMPUTE カウンタ２ = カウンタ２  +  1
024160              MOVE 負傷原因内容５ＸＷ  TO 負傷原因Ｗ(カウンタ２)
024170          END-IF
024140          IF  負傷原因内容６ＸＷ  NOT = SPACE
024150              COMPUTE カウンタ２ = カウンタ２  +  1
024160              MOVE 負傷原因内容６ＸＷ  TO 負傷原因Ｗ(カウンタ２)
024170          END-IF
024180*
024190     END-PERFORM.
024200*
024210*================================================================*
024220 印刷処理 SECTION.
024230*
024240     EVALUATE 印刷モードＦＷＲ
024250     WHEN 0
024260         PERFORM 印刷処理２
024270         PERFORM 印刷処理３
024280     WHEN 1
024290         PERFORM 印刷処理２
024300     WHEN 2
024310         PERFORM 印刷処理３
024340     WHEN 4
024350         IF 連印－保険証印刷区分 = 1
024360             PERFORM 印刷処理２
024370         END-IF
024380             PERFORM 印刷処理３
024390     END-EVALUATE.
024400*================================================================*
024410 印刷処理１ SECTION.
024420*
024430     MOVE "YHP662P" TO  定義体名Ｐ.
024440*     MOVE "SCREEN"  TO  項目群名Ｐ.
024450     MOVE "GRP001"  TO  項目群名Ｐ.
024460     WRITE YHP662P.
024470*     WRITE 印刷レコード.
024480     PERFORM エラー処理Ｐ.
024490*================================================================*
024500 印刷処理２ SECTION.
024510*
024520     MOVE "YHP662P"  TO  定義体名Ｐ.
024530     MOVE "GRP001"   TO  項目群名Ｐ.
024540     WRITE YHP662P.
024550*     WRITE 印刷レコード.
024560     PERFORM エラー処理Ｐ.
024570*================================================================*
024580 印刷処理３ SECTION.
024590*
024600     MOVE "YHP662P"  TO  定義体名Ｐ.
024610     MOVE "GRP002"   TO  項目群名Ｐ.
024620     WRITE YHP662P.
024630*     WRITE 印刷レコード.
024640     PERFORM エラー処理Ｐ.
024720*================================================================*
024730 エラー処理Ｐ SECTION.
024740*
024750     IF 通知情報Ｐ NOT = "00"
024760         DISPLAY NC"帳票エラー"              UPON CONS
024770         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
024780         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
024790         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
024800         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
024810                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
024820         ACCEPT  キー入力 FROM CONS
024830         PERFORM ファイル閉鎖
024840         EXIT PROGRAM
024850     END-IF.
024860*================================================================*
024870 名称埋込処理 SECTION.
024880*
           EVALUATE 受－保険種別
           WHEN 05
               MOVE 2          TO レセ－レセ種別
           WHEN 50 THRU 60
               MOVE 3          TO レセ－レセ種別
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
011180*
024990     END-READ.
025000*
026930*================================================================*
026940******************************************************************
026950 END PROGRAM YHP662.
026960******************************************************************
