000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCK662.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*         中京用
000100* 　　　　カルテ印刷（柔+ｳｨﾝﾄﾞｳｽﾞ版）再生紙
000110*         MED = YAW660 YCK662P
000120* ※ 中央と同じ
000130*----------------------------------------------------------------*
000140 DATE-WRITTEN.           2013-01-11
000150 DATE-COMPILED.          2013-01-11
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
000410     SELECT  部位マスタ      ASSIGN      TO        BUICODEL
000420                             ORGANIZATION             IS  INDEXED
000430                             ACCESS MODE              IS  DYNAMIC
000440                             RECORD KEY               IS  部－部位コード
000450                             FILE STATUS              IS  状態キー
000460                             LOCK        MODE         IS  AUTOMATIC.
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
000640     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000650                             ORGANIZATION             IS  INDEXED
000660                             ACCESS MODE              IS  DYNAMIC
000670                             RECORD KEY               IS  施情－施術所番号
000680                             FILE STATUS              IS  状態キー
000690                             LOCK        MODE         IS  AUTOMATIC.
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
001530*/0101老人負担区分
001540     SELECT  老人負担区分マスタ    ASSIGN      TO      ROUJINHL
001550                             ORGANIZATION             IS  INDEXED
001560                             ACCESS MODE              IS  DYNAMIC
001570                             RECORD KEY               IS  老負－保険種別
001580                                                          老負－開始和暦年月
001590                             FILE STATUS              IS  状態キー
001600                             LOCK        MODE         IS  AUTOMATIC.
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
001190*                           ［ＲＬ＝  １２８］
001200 FD  部位マスタ          BLOCK   CONTAINS   1   RECORDS.
001210     COPY BUICODE         OF  XFDLIB  JOINING   部   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
001850*                           ［ＲＬ＝  ２５６］
001860 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001870     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
001880*                           ［ＲＬ＝  １２８］
001890 FD  施術所情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001900     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
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
002150*
002160*/0101老人負担区分
002170 FD  老人負担区分マスタ      BLOCK   CONTAINS   1   RECORDS.
002180     COPY ROUJINH    OF  XFDLIB  JOINING   老負   AS  PREFIX.
002190*
002210 FD  印刷ファイル.
002220     COPY YCK662P        OF  XMDLIB.
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
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002340 01 初検フラグ                         PIC X(3)  VALUE SPACE.
002350 01 ファイル名                         PIC N(2)  VALUE SPACE.
002360 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
002370 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
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
002880       05 負傷原因内容１ＸＷ           PIC X(50)  VALUE SPACE.
002890       05 負傷原因内容２ＸＷ           PIC X(50)  VALUE SPACE.
002900       05 負傷原因内容３ＸＷ           PIC X(50)  VALUE SPACE.
002910       05 負傷原因内容４ＸＷ           PIC X(50)  VALUE SPACE.
002920       05 負傷原因内容５ＸＷ           PIC X(50)  VALUE SPACE.
002930       05 負傷原因内容６ＸＷ           PIC X(50)  VALUE SPACE.
002940       05 負傷原因内容７ＸＷ           PIC X(16)  VALUE SPACE.
002930*
002940** 負傷原因印刷区分用
002950 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
002960*
002970* 社保用
002980 01 接尾語区分Ｗ                       PIC 9     VALUE ZERO.
002990*
003000****************
003010* 連結項目待避 *
003020****************
003030*    ************
003040*    * 印刷キー *
003050*    ************
003060 01 対象データＷＲ.
003070    03 施術和暦年月ＷＲ.
003080       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
003090       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
003100       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
003110    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
003120    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
003130    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
003140    03 被保険者カナＷＲ                 PIC X(20) VALUE SPACE.
003150    03 患者コードＷＲ.
003160       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
003170       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
003180    03 印刷モードＦＷＲ                 PIC 9(1)  VALUE ZERO.
003190    03 提出年月日ＷＲ.
003200       05 提出年ＷＲ                    PIC 9(2)  VALUE ZERO.
003210       05 提出月ＷＲ                    PIC 9(2)  VALUE ZERO.
003220       05 提出日ＷＲ                    PIC 9(2)  VALUE ZERO.
003230    03 段数ＷＲ                         PIC 9(1)  VALUE ZERO.
003240************
003250* 料金情報 *
003260************
003270*
003280 01 印刷負担率Ｗ                        PIC 9(1)  VALUE ZERO.
003290**************
003300* 受診者情報 *
003310**************
003320 01 受診者情報Ｗ.
003330    03 患者コードＷ.
003340       05 患者番号Ｗ                   PIC 9(6)  VALUE ZERO.
003350       05 枝番Ｗ                       PIC X(1)  VALUE SPACE.
003360    03 市町村番号Ｗ.
003370       05 印刷市町村番号Ｗ             PIC X(8)  VALUE SPACE.
003380       05 FILLER                       PIC X(2).
003390    03 受益者番号Ｗ.
003400       05 印刷受益者番号Ｗ             PIC X(7)  VALUE SPACE.
003410       05 FILLER                       PIC X(13).
003420    03 費用負担者番号Ｗ.
003430       05 印刷費用負担者番号Ｗ         PIC X(8)  VALUE SPACE.
003440       05 FILLER                       PIC X(2).
003450    03 受益者番号助成Ｗ.
003460       05 印刷受益者番号助成Ｗ         PIC X(10)  VALUE SPACE.
003470       05 FILLER                       PIC X(10).
003480    03 記号Ｗ.
             05 印刷記号Ｗ                   PIC N(12)  VALUE SPACE.
003490    03 番号Ｗ.
003500       05 印刷番号Ｗ                   PIC X(30)  VALUE SPACE.
003510*       05 FILLER                       PIC X(18).
003520    03 被保険者情報Ｗ.
003530       05 被保険者カナＷ               PIC X(50)  VALUE SPACE.
003540       05 被保険者氏名Ｗ               PIC X(50)  VALUE SPACE.
003550*
003560*       05 被保険者和暦チェックＷ.
003570*          07 被保険者明治Ｗ            PIC N(1)  VALUE SPACE.
003580*          07 被保険者大正Ｗ            PIC N(1)  VALUE SPACE.
003590*          07 被保険者昭和Ｗ            PIC N(1)  VALUE SPACE.
003600       05 被保険者性別チェックＷ.
003610          07 被保険者男チェックＷ      PIC N(1)  VALUE SPACE.
003620          07 被保険者女チェックＷ      PIC N(1)  VALUE SPACE.
003630       05 被保険者性別Ｗ               PIC 9     VALUE ZERO.
003640       05 被保険者和暦チェックＷ.
003650          07 被保険者明治チェックＷ    PIC N(1)  VALUE SPACE.
003660          07 被保険者大正チェックＷ    PIC N(1)  VALUE SPACE.
003670          07 被保険者昭和チェックＷ    PIC N(1)  VALUE SPACE.
003680          07 被保険者平成チェックＷ    PIC N(1)  VALUE SPACE.
003690       05 被保険者生年月日Ｗ.
003700          07 被保険者和暦Ｗ            PIC 9     VALUE ZERO.
003710          07 被保険者年Ｗ              PIC 9(2)  VALUE ZERO.
003720          07 被保険者月Ｗ              PIC 9(2)  VALUE ZERO.
003730          07 被保険者日Ｗ              PIC 9(2)  VALUE ZERO.
003740       05 被保険者年齢Ｗ               PIC 9(3)  VALUE ZERO.
003750*
003760       05 被保険者郵便番号Ｗ.
003770          07 被保険者郵便番号１Ｗ      PIC X(3)  VALUE SPACE.
003780          07 被保険者郵便番号２Ｗ      PIC X(4)  VALUE SPACE.
003790       05 被保険者住所Ｗ.
003800          07 印刷被保険者住所Ｗ        PIC X(100) VALUE SPACE.
003810       05 被保険者電話番号Ｗ           PIC X(15) VALUE SPACE.
003820       05 分割電話番号Ｗ.
003830          07 分割電話番号１Ｗ          PIC X(5)  VALUE SPACE.
003840          07 分割電話番号２Ｗ          PIC X(5)  VALUE SPACE.
003850          07 分割電話番号３Ｗ          PIC X(5)  VALUE SPACE.
003860*          07 市外局番Ｗ                PIC X(5)  VALUE SPACE.
003870*          07 分割局番Ｗ                PIC X(5)  VALUE SPACE.
003880*          07 分割番号Ｗ                PIC X(5)  VALUE SPACE.
003890    03 資格取得年月日Ｗ.
003900       05 資格取得元号Ｗ               PIC N(2)  VALUE SPACE.
003910       05 資格取得和暦Ｗ               PIC 9(1)  VALUE ZERO.
003920       05 資格取得年Ｗ                 PIC 9(2)  VALUE ZERO.
003930       05 資格取得月Ｗ                 PIC 9(2)  VALUE ZERO.
003940       05 資格取得日Ｗ                 PIC 9(2)  VALUE ZERO.
003950    03 有効期限年月日Ｗ.
003960       05 有効期限元号Ｗ               PIC N(2)  VALUE SPACE.
003970       05 有効期限和暦Ｗ               PIC 9(1)  VALUE ZERO.
003980       05 有効期限年Ｗ                 PIC 9(2)  VALUE ZERO.
003990       05 有効期限月Ｗ                 PIC 9(2)  VALUE ZERO.
004000       05 有効期限日Ｗ                 PIC 9(2)  VALUE ZERO.
004010       05 資格和暦チェックＷ.
004020          07 資格昭和チェックＷ        PIC N(1)  VALUE SPACE.
004030          07 資格平成チェックＷ        PIC N(1)  VALUE SPACE.
004040    03 患者情報Ｗ.
004050       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
004060       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
004070       05 患者性別チェックＷ.
004080          07 患者男チェックＷ          PIC N(1)  VALUE SPACE.
004090          07 患者女チェックＷ          PIC N(1)  VALUE SPACE.
004100       05 続柄Ｗ.
004110          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
004120          07 FILLER                    PIC X(4).
004130       05 和暦チェックＷ.
004140          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
004150          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
004160          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
004170          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
004180       05 患者年Ｗ                     PIC 9(2) VALUE ZERO.
004190       05 患者月Ｗ                     PIC 9(2) VALUE ZERO.
004200       05 患者日Ｗ                     PIC 9(2) VALUE ZERO.
004210       05 患者年齢Ｗ                   PIC 9(3) VALUE ZERO.
004220       05 患者性別Ｗ                   PIC N(4) VALUE SPACE.
004230    03 保険種別チェックＷ.
004240       05 社保チェックＷ               PIC N(1) VALUE SPACE.
004250       05 組合チェックＷ               PIC N(1) VALUE SPACE.
004260       05 共済チェックＷ               PIC N(1) VALUE SPACE.
004270       05 国保チェックＷ               PIC N(1) VALUE SPACE.
004280       05 日雇チェックＷ               PIC N(1) VALUE SPACE.
004290       05 船員チェックＷ               PIC N(1) VALUE SPACE.
004300       05 老人チェックＷ               PIC N(1) VALUE SPACE.
004310       05 福チェックＷ                 PIC N(1) VALUE SPACE.
004320       05 身障チェックＷ               PIC N(1) VALUE SPACE.
004330       05 退職チェックＷ               PIC N(1) VALUE SPACE.
004340       05 母子チェックＷ               PIC N(1) VALUE SPACE.
004350       05 乳幼児チェックＷ             PIC N(1) VALUE SPACE.
004360       05 国保組合チェックＷ           PIC N(1) VALUE SPACE.
004370       05 労災チェックＷ               PIC N(1) VALUE SPACE.
004380       05 自賠チェックＷ               PIC N(1) VALUE SPACE.
004390       05 生保チェックＷ               PIC N(1) VALUE SPACE.
004400       05 原爆チェックＷ               PIC N(1) VALUE SPACE.
004410       05 患者郵便番号Ｗ.
004420          07 患者郵便番号１Ｗ          PIC X(3)  VALUE SPACE.
004430          07 患者郵便番号２Ｗ          PIC X(4)  VALUE SPACE.
004440       05 患者住所Ｗ.
004450          07 患者住所１Ｗ              PIC X(50) VALUE SPACE.
004460          07 患者住所２Ｗ              PIC X(50) VALUE SPACE.
004470       05 患者電話番号Ｗ               PIC X(15) VALUE SPACE.
004480    03 負傷原因Ｗ                      PIC X(50) OCCURS 39 VALUE SPACE.
004490**************
004500* 事業所情報 *
004510**************
004520 01 事業所情報Ｗ.
004530    03 事業所名称Ｗ.
004540       05 印刷事業所名称Ｗ             PIC X(60)  VALUE SPACE.
004550    03 事業所郵便番号Ｗ.
004560       05 事業所郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
004570       05 事業所郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
004580    03 事業所住所Ｗ.
004590       05 印刷事業所住所Ｗ             PIC X(50)  VALUE SPACE.
004590       05 印刷事業所住所２Ｗ           PIC X(50)  VALUE SPACE.
004610**************
004620* 請求先情報 *
004630**************
004640 01 請求先情報Ｗ.
004650    03 保険者番号Ｗ.
004660       05 印刷保険者番号Ｗ             PIC X(8)  VALUE SPACE.
004670       05 FILLER                       PIC X(2).
004680    03 請求先名称Ｗ.
004690       05 印刷請求先名称Ｗ             PIC X(40) VALUE SPACE.
004700    03 支部名Ｗ.
004710       05 印刷支部名Ｗ                 PIC X(40) VALUE SPACE.
004720    03 請求先名支部名Ｗ.
004730       05 印刷請求先名支部名Ｗ         PIC X(54) VALUE SPACE.
004740       05 FILLER                       PIC X(26).
004750    03 保険者呼名Ｗ.
004760*       05 印刷保険者呼名Ｗ             PIC N(7)  VALUE SPACE.
004770       05 印刷保険者呼名Ｗ             PIC X(14)  VALUE SPACE.
004780    03 請求先郵便番号Ｗ.
004790       05 請求先郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
004800       05 請求先郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
004810    03 請求先住所１Ｗ.
004820       05 印刷請求先住所１Ｗ           PIC X(40) VALUE SPACE.
004830    03 請求先住所２Ｗ.
004840       05 印刷請求先住所２Ｗ           PIC X(35) VALUE SPACE.
004850       05 FILLER                       PIC X(5).
004860****************
004870* 負傷データＦ *
004880****************
004890 01 負傷情報Ｗ.
004900    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
004910    03 負傷データ情報Ｗ  OCCURS   9.
004920       05 会部位コードＷ               PIC X(4)  VALUE SPACE.
004930       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
004940       05 負傷種別Ｗ                   PIC 9(2)  VALUE ZERO.
004950       05 部位Ｗ                       PIC 9(2)  VALUE ZERO.
004960       05 左右区分Ｗ                   PIC 9(1)  VALUE ZERO.
004970       05 負傷位置番号Ｗ               PIC 9(2)  VALUE ZERO.
004980       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
004990       05 負傷年月日Ｗ.
005000          07 負傷和暦Ｗ                PIC 9(1)  VALUE ZERO.
005010          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
005020          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
005030          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
005040          07 負傷年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005050       05 施術開始年月日Ｗ.
005060          07 施術開始和暦Ｗ            PIC 9(1)  VALUE ZERO.
005070          07 施術開始年Ｗ              PIC 9(2)  VALUE ZERO.
005080          07 施術開始月Ｗ              PIC 9(2)  VALUE ZERO.
005090          07 施術開始日Ｗ              PIC 9(2)  VALUE ZERO.
005100          07 施開年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005110       05 施術終了年月日Ｗ.
005120          07 施術終了和暦Ｗ            PIC 9(1)  VALUE ZERO.
005130          07 施術終了年Ｗ              PIC 9(2)  VALUE ZERO.
005140          07 施術終了月Ｗ              PIC 9(2)  VALUE ZERO.
005150          07 施術終了日Ｗ              PIC 9(2)  VALUE ZERO.
005160          07 施終年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005170       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
005180       05 転帰Ｗ                       PIC N(4)  VALUE SPACE.
005190       05 転帰チェックＷ.
005200          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
005210          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
005220          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
005230*
005240******************
005250* 負担率チェック *
005260******************
005270 01 負担率チェックＷ.
005280    03 ０割チェックＷ                  PIC N(1)  VALUE SPACE.
005290    03 １割チェックＷ                  PIC N(1)  VALUE SPACE.
005300    03 ２割チェックＷ                  PIC N(1)  VALUE SPACE.
005310    03 ３割チェックＷ                  PIC N(1)  VALUE SPACE.
005320**********
005330* 合計行 *
005340**********
005350 01 合計行Ｗ.
005360    03 請求終了日Ｗ                    PIC 9(2) VALUE ZERO.
005370    03 対象西暦Ｗ                      PIC 9(4) VALUE ZERO.
005380    03 商Ｗ                            PIC 9(3) VALUE ZERO.
005390    03 余Ｗ                            PIC 9(3) VALUE ZERO.
005400*/年月毎持つ保険者データのワーク
005410 01 保負担率区分Ｗ                     PIC 9    VALUE ZERO.
005420 01 保本人負担率Ｗ                     PIC 9(3) VALUE ZERO.
005430 01 保家族負担率Ｗ                     PIC 9(3) VALUE ZERO.
005440 01 保本人負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
005450 01 保家族負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
005460*
005470 01 比較和暦年月Ｗ.
005480    03 比較和暦Ｗ                      PIC 9    VALUE ZERO.
005490    03 比較年Ｗ                        PIC 9(2) VALUE ZERO.
005500    03 比較月Ｗ                        PIC 9(2) VALUE ZERO.
005510*
005520*/老人保険改正の対応0101
005530 01 老人負担計算区分Ｗ                 PIC 9    VALUE ZERO.
005540 01 負担率用種別Ｗ                     PIC 9(2) VALUE ZERO.
005550 01 助成負担区分Ｗ                     PIC 9(2) VALUE ZERO.
005560*
005561*
005562** 助成負担率用(14/10～)
005563 01 助成負担率ＷＷ.
005564    03 助成負担率Ｗ                    PIC 9(3) VALUE ZERO.
005565    03 助成負担率Ｗ１                  PIC 9    VALUE ZERO.
005566    03 助成負担率文字Ｗ１              PIC X    VALUE SPACE.
005567    03 助成負担率表示Ｗ                PIC X(6) VALUE SPACE.
005568*
005570****************************************************************
005580 01 印刷制御.
005590     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
005600     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
005610     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
005620     03 拡張制御Ｐ.
005630         05 端末制御Ｐ.
005640             07 移動方向Ｐ             PIC X(1)  VALUE SPACE.
005650             07 移動行数Ｐ             PIC 9(3)  VALUE ZERO.
005660         05 詳細制御Ｐ                 PIC X(2)  VALUE SPACE.
005670     03 通知情報Ｐ                     PIC X(2)  VALUE SPACE.
005680     03 ユニット名Ｐ                   PIC X(8)  VALUE SPACE.
005690*
005700 01 計算機西暦年Ｗ                     PIC 9(2)  VALUE ZERO.
005710* 日付ＷＯＲＫ
005720 01 和暦終了年Ｗ                       PIC 9(4)  VALUE ZERO.
005730 01 計算機西暦.
005740    03 計算機西暦年                    PIC 9(4)  VALUE ZERO.
005750    03 計算機西暦月日                  PIC 9(4)  VALUE ZERO.
005760 01 計算機西暦Ｒ REDEFINES 計算機西暦.
005770    03 計算機世紀                      PIC 9(2).
005780    03 計算機日付                      PIC 9(6).
005790    03 計算機日付Ｒ REDEFINES 計算機日付.
005800       05 計算機年月                   PIC 9(4).
005810       05 計算機年月Ｒ REDEFINES 計算機年月.
005820         07 計算機年                   PIC 9(2).
005830         07 計算機月                   PIC 9(2).
005840       05 計算機日                     PIC 9(2).
005850*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
005860******************************************************************
005870*                          連結項目                              *
005880******************************************************************
005890*
005900**********************
005910* メッセージ表示キー *
005920**********************
005930 01 連メ－キー IS EXTERNAL.
005940    03  連メ－メッセージ                 PIC N(20).
005950*
005960****************
005970* 連結項目待避 *
005980****************
006060************
006070* 印刷キー *
006080************
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
       01 連印－対象データ料金 IS EXTERNAL.
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
006741*
006742* 負担率取得用14/10～
006743 01 連率－負担率取得キー IS EXTERNAL.
006744    03 連率－施術和暦年月.
006745       05 連率－施術和暦               PIC 9.
006746       05 連率－施術年月.
006747          07 連率－施術年              PIC 9(2).
006748          07 連率－施術月              PIC 9(2).
006749    03 連率－患者コード.
006750       05 連率－患者番号               PIC 9(6).
006751       05 連率－枝番                   PIC X.
006752    03 連率－実際負担率                PIC 9(3).
006753    03 連率－実際本体負担率            PIC 9(3).
006754    03 連率－健保負担率                PIC 9(3).
006755    03 連率－２７老負担率              PIC 9(3).
006756    03 連率－助成負担率                PIC 9(3).
006757    03 連率－特別用負担率              PIC 9(3).
006758*
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
006759******************************************************************
006760*                      PROCEDURE  DIVISION                       *
006770******************************************************************
006780 PROCEDURE               DIVISION.
006790************
006800*           *
006810* 初期処理   *
006820*           *
006830************
002570     PERFORM プリンタファイル作成.
006840     PERFORM 初期化.
006850************
006860*           *
006870* 主処理     *
006880*           *
006890************
006900* 印刷
006910     PERFORM 連結項目待避.
006920     PERFORM 印刷セット
006930     PERFORM 印刷処理
006940************
006950*           *
006960* 終了処理   *
006970*           *
006980************
006990     PERFORM 終了処理.
007000     EXIT PROGRAM.
007010*
007020*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YCK662"              TO Ｈ連ＰＲＴＦ－帳票プログラム名.
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
007030*================================================================*
007040 初期化 SECTION.
007050*
007060     PERFORM ファイルオープン.
007070*    /* 現在日付取得 */
007080     ACCEPT 計算機日付 FROM DATE.
007090*    /* 1980～2079年の間で設定 */
007100     IF 計算機年 > 80
007110         MOVE 19 TO 計算機世紀
007120     ELSE
007130         MOVE 20 TO 計算機世紀
007140     END-IF.
007150     PERFORM カレント元号取得.
007160     PERFORM 和暦終了年取得.
007170     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
007180*================================================================*
007190 カレント元号取得 SECTION.
007200*
007210     MOVE ZEROS TO 制－制御区分.
007220     READ 制御情報マスタ
007230     NOT INVALID KEY
007240         MOVE 制－カレント元号 TO カレント元号Ｗ
007250         MOVE 制－カルテ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
007260     END-READ.
007270*
007280*================================================================*
007290 和暦終了年取得 SECTION.
007300*
007310     MOVE カレント元号Ｗ TO 元－元号区分.
007320     READ 元号マスタ
007330     INVALID KEY
007340         DISPLAY NC"指定和暦が登録されていません" UPON CONS
007350         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
007360                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007370         ACCEPT  キー入力 FROM CONS
007380         PERFORM 終了処理
007390         EXIT PROGRAM
007400     NOT INVALID KEY
007410         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
007420         MOVE 前和暦Ｗ TO 元－元号区分
007430         READ 元号マスタ
007440         INVALID KEY
007450             DISPLAY NC"指定和暦が登録されていません" UPON CONS
007460             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
007470                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007480             ACCEPT  キー入力 FROM CONS
007490             PERFORM 終了処理
007500             EXIT PROGRAM
007510         NOT INVALID KEY
007520             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
007530         END-READ
007540     END-READ.
007550*
007560*================================================================*
007570 連結項目待避 SECTION.
007580*
007590     INITIALIZE 対象データＷＲ.
007600     MOVE 連印－施術和暦      TO 施術和暦ＷＲ.
007610     MOVE 連印－施術年        TO 施術年ＷＲ.
007620     MOVE 連印－施術月        TO 施術月ＷＲ.
007630     MOVE 連印－保険種別      TO 保険種別ＷＲ.
007640     MOVE 連印－保険者番号    TO 保険者番号ＷＲ.
007650     MOVE 連印－本人家族区分  TO 本人家族区分ＷＲ.
007660     MOVE 連印－被保険者カナ  TO 被保険者カナＷＲ.
007670     MOVE 連印－患者番号      TO 患者番号ＷＲ.
007680     MOVE 連印－枝番          TO 枝番ＷＲ.
007690     MOVE 連印－印刷モードＦ  TO 印刷モードＦＷＲ.
007700     MOVE 連印－提出年        TO 提出年ＷＲ.
007710     MOVE 連印－提出月        TO 提出月ＷＲ.
007720     MOVE 連印－提出日        TO 提出日ＷＲ.
007730     MOVE 連印－段数          TO 段数ＷＲ.
007740*================================================================*
007750 ファイルオープン SECTION.
007760*
007770     OPEN INPUT   保険者マスタ
007780         MOVE NC"保険者" TO ファイル名.
007790         PERFORM オープンチェック.
007800     OPEN INPUT   元号マスタ
007810         MOVE NC"元号" TO ファイル名.
007820         PERFORM オープンチェック.
007830     OPEN INPUT   名称マスタ
007840         MOVE NC"名称" TO ファイル名.
007850         PERFORM オープンチェック.
005780     OPEN INPUT 部位マスタ.
005790         MOVE NC"部位" TO ファイル名.
005800         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
007890     OPEN INPUT   制御情報マスタ
007900         MOVE NC"制御情報" TO ファイル名.
007910         PERFORM オープンチェック.
007920     OPEN INPUT   施術所情報マスタ
007930         MOVE NC"施情" TO ファイル名.
007940         PERFORM オープンチェック.
008010     OPEN INPUT   受診者情報Ｆ.
008020         MOVE NC"受情" TO ファイル名.
008030         PERFORM オープンチェック.
008040     OPEN INPUT   施術記録Ｆ.
008050         MOVE NC"施記Ｆ" TO ファイル名.
008060         PERFORM オープンチェック.
008070     OPEN INPUT   負傷データＦ.
008080         MOVE NC"負傷" TO ファイル名.
008090         PERFORM オープンチェック.
008130     OPEN INPUT   負傷原因Ｆ.
008140         MOVE NC"負傷原因" TO ファイル名.
008150         PERFORM オープンチェック.
008160     OPEN INPUT 市町村マスタ.
008170         MOVE NC"市町村" TO ファイル名.
008180         PERFORM オープンチェック.
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
008190*/0101老人負担区分
008200     OPEN INPUT 老人負担区分マスタ.
008210         MOVE NC"老人負担" TO ファイル名.
008220         PERFORM オープンチェック.
008230     OPEN I-O   印刷ファイル
008240         MOVE NC"印刷" TO ファイル名.
008250         PERFORM エラー処理Ｐ.
008260*================================================================*
008270 オープンチェック SECTION.
008280*
008290     IF 状態キー  NOT =  "00"
008300         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
008310         DISPLAY NC"状態キー：" 状態キー         UPON CONS
008320         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008330                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
008340         ACCEPT  キー入力 FROM CONS
008350         PERFORM ファイル閉鎖
008360         EXIT PROGRAM.
008370*================================================================*
008380 ファイル閉鎖 SECTION.
008390*
008400     CLOSE 印刷ファイル   保険者マスタ        元号マスタ
008410           制御情報マスタ 施術所情報マスタ    部位マスタ
008420           受診者情報Ｆ   名称マスタ          施術記録Ｆ
008430           負傷データＦ   負傷原因Ｆ          レセプトＦ
008440           市町村マスタ   老人負担区分マスタ  生保情報Ｆ
008930           事業所マスタ   労災情報Ｆ          自賠責情報Ｆ    保険会社マスタ.
008450*================================================================*
008460 終了処理 SECTION.
008470*
008480     PERFORM ファイル閉鎖.
008490*================================================================*
008500 印刷セット SECTION.
008510*
008520     EVALUATE 印刷モードＦＷＲ
008530     WHEN 0
008540         PERFORM 印刷セット１
008550     WHEN 1
008560         PERFORM 印刷セット２
008570     WHEN 2
008580         PERFORM 印刷セット３
008590     WHEN 3
008600         PERFORM 印刷セット４
008610     END-EVALUATE.
008620*
008630*================================================================*
008640 印刷セット１ SECTION.
008650*
008660     INITIALIZE YCK662P.
008670     INITIALIZE 受診者情報Ｗ.
008680     INITIALIZE 事業所情報Ｗ.
008690     INITIALIZE 請求先情報Ｗ.
008700     INITIALIZE 負傷情報Ｗ.
008710     INITIALIZE 負担率チェックＷ.
008720     PERFORM 負担率取得.
008730     PERFORM 請求先情報取得.
008740     PERFORM 受診者情報取得.
008750     PERFORM 負傷データ取得.
008760*
008770     IF 負傷原因印刷区分Ｗ  NOT = 1 
008780        PERFORM 負傷原因取得
008790     END-IF.
008800*
008810     PERFORM 印刷上部セット.
008820     PERFORM 印刷下部セット.
008830*================================================================*
008840 印刷セット２ SECTION.
008850*
008860     INITIALIZE YCK662P.
008870     INITIALIZE 受診者情報Ｗ.
008880     INITIALIZE 事業所情報Ｗ.
008890     INITIALIZE 請求先情報Ｗ.
008900     INITIALIZE 負傷情報Ｗ.
008910     INITIALIZE 負担率チェックＷ.
008920     PERFORM 負担率取得.
008930     PERFORM 請求先情報取得.
008940     PERFORM 受診者情報取得.
008950     PERFORM 印刷上部セット.
008960*================================================================*
008970 印刷セット３ SECTION.
008980*
008990     INITIALIZE YCK662P.
009000     INITIALIZE 受診者情報Ｗ.
009010     INITIALIZE 事業所情報Ｗ.
009020     INITIALIZE 請求先情報Ｗ.
009030     INITIALIZE 負傷情報Ｗ.
009040     PERFORM 負傷データ取得.
009050*
009060     IF 負傷原因印刷区分Ｗ  NOT = 1 
009070        PERFORM 負傷原因取得
009080     END-IF.
009090*
009100     PERFORM 印刷下部セット.
009110*================================================================*
009120 印刷セット４ SECTION.
009130*
009140     INITIALIZE YCK662P.
009150     INITIALIZE 受診者情報Ｗ.
009160     INITIALIZE 事業所情報Ｗ.
009170     INITIALIZE 請求先情報Ｗ.
009180     INITIALIZE 負傷情報Ｗ.
009190     INITIALIZE 合計行Ｗ.
009260     PERFORM 受診者情報取得.
009230     PERFORM 請求期間取得.
009240     PERFORM 合計金額行セット.
009250*================================================================*
009260 印刷上部セット SECTION.
009270*
009280     MOVE 患者番号Ｗ                 TO 患者番号.
009290     MOVE 枝番Ｗ                     TO 枝番.
009300**************************
009310* 被保険者証情報セット   *
009320**************************
009330     IF 印刷記号Ｗ(1:1) = NC"＊" 
009340        MOVE  SPACE      TO  記号
009350     ELSE
009360        MOVE 印刷記号Ｗ  TO  記号
009370     END-IF.
009380     IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
009390        ( 印刷番号Ｗ(1:2) = "＊" )
009400        MOVE  SPACE      TO  番号
009410     ELSE
009420        MOVE 印刷番号Ｗ  TO  番号
009430     END-IF.
009440************************
009450* 被保険者情報セット   *
009460************************
009470     MOVE 被保険者氏名Ｗ             TO 被保険者氏名.
009480     MOVE 被保険者カナＷ             TO 被保険者カナ.
009490*     MOVE 資格取得元号Ｗ             TO 資格取得元号.
009500     MOVE 資格取得年Ｗ               TO 資格取得年.
009510     MOVE 資格取得月Ｗ               TO 資格取得月.
009520     MOVE 資格取得日Ｗ               TO 資格取得日.
009530     MOVE 資格昭和チェックＷ         TO 資格昭和チェック.
009540     MOVE 資格平成チェックＷ         TO 資格平成チェック.
009550*/有効期限
009560     MOVE 有効期限年Ｗ               TO 有効年.
009570     MOVE 有効期限月Ｗ               TO 有効月.
009580     MOVE 有効期限日Ｗ               TO 有効日.
009590     IF ( 有効期限年Ｗ = ZERO ) AND
009600        ( 有効期限月Ｗ = ZERO ) AND
009610        ( 有効期限日Ｗ = ZERO )
009620         MOVE SPACE          TO 有効元号
009630         MOVE SPACE          TO 有効年月日表示
009640         MOVE SPACE          TO 有効年表示
009650         MOVE SPACE          TO 有効月表示
009660         MOVE SPACE          TO 有効日表示
009670     ELSE
009680         MOVE 有効期限元号Ｗ TO 有効元号
009690         MOVE NC"有効年月日" TO 有効年月日表示
009700         MOVE NC"年"         TO 有効年表示
009710         MOVE NC"月"         TO 有効月表示
009720         MOVE NC"日"         TO 有効日表示
009730     END-IF.
009740     MOVE 被保険者明治チェックＷ     TO 被保険者明治チェック.
009750     MOVE 被保険者大正チェックＷ     TO 被保険者大正チェック.
009760     MOVE 被保険者昭和チェックＷ     TO 被保険者昭和チェック.
009770     MOVE 被保険者平成チェックＷ     TO 被保険者平成チェック.
009780     MOVE 被保険者年Ｗ               TO 被保険者年.
009790     MOVE 被保険者月Ｗ               TO 被保険者月.
009800     MOVE 被保険者日Ｗ               TO 被保険者日.
009810     MOVE 被保険者年齢Ｗ             TO 被保険者年齢.
009820********************
009830* 患者情報セット   *
009840********************
009850     MOVE ０割チェックＷ             TO ０割チェック.
009860     MOVE １割チェックＷ             TO １割チェック.
009870     MOVE ２割チェックＷ             TO ２割チェック.
009880     MOVE ３割チェックＷ             TO ３割チェック.
009881*
009882* 14/10～ (助成の負担率）
009883     MOVE 助成負担率表示Ｗ           TO 助成負担率.
009884*
009890     IF  印刷市町村番号Ｗ(1:2) = "99"
009900         MOVE SPACE                  TO 市町村番号
009910     ELSE
009920         MOVE 印刷市町村番号Ｗ       TO 市町村番号
009930     END-IF.
009940     IF (印刷受益者番号Ｗ(1:1) = "*") OR
009950        (印刷受益者番号Ｗ(1:2) = "＊") 
009960         MOVE SPACE                  TO 受益者番号
009970     ELSE
009980         MOVE 印刷受益者番号Ｗ       TO 受益者番号
009990     END-IF
010000*
010010     IF  印刷費用負担者番号Ｗ(1:2) = "99"
010020         MOVE SPACE                  TO 公費負担者番号
010030     ELSE
010040         MOVE 印刷費用負担者番号Ｗ   TO 公費負担者番号
010050     END-IF.
010060*
010070     IF (印刷受益者番号助成Ｗ(1:1) = "*") OR
010080        (印刷受益者番号助成Ｗ(1:2) = "＊")
010090         MOVE SPACE                  TO 受益者番号助成
010100     ELSE
010110         MOVE 印刷受益者番号助成Ｗ   TO 受益者番号助成
010120     END-IF.
010130     MOVE 患者カナＷ                 TO 患者カナ.
010140     MOVE 患者氏名Ｗ                 TO 患者氏名.
010150     MOVE 被保険者男チェックＷ       TO 被保険者男チェック.
010160     MOVE 被保険者女チェックＷ       TO 被保険者女チェック.
010170     MOVE 被保険者郵便番号１Ｗ       TO 被保険者郵便番号１.
010180     MOVE 被保険者郵便番号２Ｗ       TO 被保険者郵便番号２.
010190     MOVE "-"                        TO 被保険者郵便区切.
010200     MOVE 被保険者住所Ｗ             TO 被保険者住所.
010210     MOVE 被保険者電話番号Ｗ         TO 被保険者電話番号.
010220     MOVE 患者男チェックＷ           TO 患者男チェック.
010230     MOVE 患者女チェックＷ           TO 患者女チェック.
010240     MOVE 明治チェックＷ             TO 明治チェック.
010250     MOVE 大正チェックＷ             TO 大正チェック.
010260     MOVE 昭和チェックＷ             TO 昭和チェック.
010270     MOVE 平成チェックＷ             TO 平成チェック.
010280     MOVE 患者年Ｗ                   TO 患者年.
010290     MOVE 患者月Ｗ                   TO 患者月.
010300     MOVE 患者日Ｗ                   TO 患者日.
010310     MOVE 患者年齢Ｗ                 TO 患者年齢.
010320     MOVE 印刷続柄Ｗ                 TO 続柄.
010330     MOVE 患者郵便番号１Ｗ           TO 患者郵便番号１.
010340     MOVE 患者郵便番号２Ｗ           TO 患者郵便番号２.
010350     MOVE "-"                        TO 患者郵便区切.
010360     MOVE 患者住所Ｗ                 TO 患者住所.
010370     MOVE 患者電話番号Ｗ             TO 患者電話番号.
010380*     MOVE 患者住所１Ｗ               TO 患者住所１.
010390*     MOVE 患者住所２Ｗ               TO 患者住所２.
010400********************
010410* 事業所情報セット *
010420********************
010430     MOVE 事業所名称Ｗ               TO 事業所名称.
010440     MOVE 事業所郵便番号１Ｗ         TO 事業所郵便番号１.
010450     MOVE 事業所郵便番号２Ｗ         TO 事業所郵便番号２.
010460     MOVE 印刷事業所住所Ｗ           TO 事業所住所.
010460     MOVE 印刷事業所住所２Ｗ         TO 事業所住所２.
010470************************************
010480* 都道府県・健康保険組合情報セット *
010490************************************
010500*労災、自費の時は保険者情報は印字しない
010510     IF 保険種別ＷＲ = 70 OR 90
010520         MOVE SPACE                  TO 保険者番号
010530         MOVE SPACE                  TO 請求先名支部名
010540     ELSE
010550         MOVE 印刷保険者番号Ｗ       TO 保険者番号
010560         MOVE 印刷請求先名支部名Ｗ   TO 請求先名支部名
010570     END-IF.
010580**************************
010590* 保険種別チェックセット *
010600**************************
010610     MOVE 社保チェックＷ             TO 社保チェック.
010620     MOVE 組合チェックＷ             TO 組合チェック.
010630     MOVE 日雇チェックＷ             TO 日雇チェック.
010640     MOVE 船員チェックＷ             TO 船員チェック.
010650     MOVE 共済チェックＷ             TO 共済チェック.
010660     MOVE 国保チェックＷ             TO 国保チェック.
010670     MOVE 退職チェックＷ             TO 退職チェック.
010680     MOVE 老人チェックＷ             TO 老人チェック.
010690     MOVE 乳幼児チェックＷ           TO 乳幼児チェック.
010700     MOVE 自賠チェックＷ             TO 自賠チェック.
010710     MOVE 生保チェックＷ             TO 生保チェック.
010720     MOVE 福チェックＷ               TO 福チェック.
010730     MOVE 身障チェックＷ             TO 身障チェック.
010740     MOVE 母子チェックＷ             TO 母子チェック.
010750     MOVE 生保チェックＷ             TO 生保チェック.
010760     MOVE 原爆チェックＷ             TO 原爆チェック.
010770*================================================================*
010780 印刷下部セット SECTION.
010790*
010800********************
010810* 負傷データセット *
010820********************
010830* １部位 *
010840**********
010850     MOVE 会部位コードＷ(1)   TO 負傷コード１.
010860     MOVE 負傷名Ｗ(1)         TO 負傷名１.
010870     MOVE 負傷年Ｗ(1)         TO 負傷年１.
010880     MOVE 負傷月Ｗ(1)         TO 負傷月１.
010890     MOVE 負傷日Ｗ(1)         TO 負傷日１.
010900     MOVE 施術開始年Ｗ(1)     TO 施術開始年１.
010910     MOVE 施術開始月Ｗ(1)     TO 施術開始月１.
010920     MOVE 施術開始日Ｗ(1)     TO 施術開始日１.
010930     MOVE 施術終了年Ｗ(1)     TO 施術終了年１.
010940     MOVE 施術終了月Ｗ(1)     TO 施術終了月１.
010950     MOVE 施術終了日Ｗ(1)     TO 施術終了日１.
010960*     MOVE 負傷年月日区切Ｗ(1) TO 区切１１ 区切１２.
010970*     MOVE 施開年月日区切Ｗ(1) TO 区切１３ 区切１４.
010980*     MOVE 施終年月日区切Ｗ(1) TO 区切１５ 区切１６.
010990*     MOVE 転帰Ｗ(1)           TO 転帰１.
011000     MOVE 治癒チェックＷ(1)   TO 治癒チェック１.
011010     MOVE 中止チェックＷ(1)   TO 中止チェック１.
011020     MOVE 転医チェックＷ(1)   TO 転医チェック１.
011030*     ******************
011040*     * 日数は一時保留 *
011050*     ******************
011060*     **********************
011070*     * 施術回数は一時保留 *
011080*     **********************
011090*     **********************
011100*     * 転帰名称は一時保留 *
011110*     **********************
011120**********
011130* ２部位 *
011140**********
011150     MOVE 会部位コードＷ(2)   TO 負傷コード２.
011160     MOVE 負傷名Ｗ(2)         TO 負傷名２.
011170     MOVE 負傷年Ｗ(2)         TO 負傷年２.
011180     MOVE 負傷月Ｗ(2)         TO 負傷月２.
011190     MOVE 負傷日Ｗ(2)         TO 負傷日２.
011200     MOVE 施術開始年Ｗ(2)     TO 施術開始年２.
011210     MOVE 施術開始月Ｗ(2)     TO 施術開始月２.
011220     MOVE 施術開始日Ｗ(2)     TO 施術開始日２.
011230     MOVE 施術終了年Ｗ(2)     TO 施術終了年２.
011240     MOVE 施術終了月Ｗ(2)     TO 施術終了月２.
011250     MOVE 施術終了日Ｗ(2)     TO 施術終了日２.
011260*     MOVE 負傷年月日区切Ｗ(2) TO 区切２１ 区切２２.
011270*     MOVE 施開年月日区切Ｗ(2) TO 区切２３ 区切２４.
011280*     MOVE 施終年月日区切Ｗ(2) TO 区切２５ 区切２６.
011290*     MOVE 転帰Ｗ(2)           TO 転帰２.
011300     MOVE 治癒チェックＷ(2)   TO 治癒チェック２.
011310     MOVE 中止チェックＷ(2)   TO 中止チェック２.
011320     MOVE 転医チェックＷ(2)   TO 転医チェック２.
011330*     ******************
011340*     * 日数は一時保留 *
011350*     ******************
011360*     **********************
011370*     * 施術回数は一時保留 *
011380*     **********************
011390*     **********************
011400*     * 転帰名称は一時保留 *
011410*     **********************
011420**********
011430* ３部位 *
011440**********
011450     MOVE 会部位コードＷ(3)   TO 負傷コード３.
011460     MOVE 負傷名Ｗ(3)         TO 負傷名３.
011470     MOVE 負傷年Ｗ(3)         TO 負傷年３.
011480     MOVE 負傷月Ｗ(3)         TO 負傷月３.
011490     MOVE 負傷日Ｗ(3)         TO 負傷日３.
011500     MOVE 施術開始年Ｗ(3)     TO 施術開始年３.
011510     MOVE 施術開始月Ｗ(3)     TO 施術開始月３.
011520     MOVE 施術開始日Ｗ(3)     TO 施術開始日３.
011530     MOVE 施術終了年Ｗ(3)     TO 施術終了年３.
011540     MOVE 施術終了月Ｗ(3)     TO 施術終了月３.
011550     MOVE 施術終了日Ｗ(3)     TO 施術終了日３.
011560*     MOVE 負傷年月日区切Ｗ(3) TO 区切３１ 区切３２.
011570*     MOVE 施開年月日区切Ｗ(3) TO 区切３３ 区切３４.
011580*     MOVE 施終年月日区切Ｗ(3) TO 区切３５ 区切３６.
011590*     MOVE 転帰Ｗ(3)           TO 転帰３.
011600     MOVE 治癒チェックＷ(3)   TO 治癒チェック３.
011610     MOVE 中止チェックＷ(3)   TO 中止チェック３.
011620     MOVE 転医チェックＷ(3)   TO 転医チェック３.
011630*     ******************
011640*     * 日数は一時保留 *
011650*     ******************
011660*     **********************
011670*     * 施術回数は一時保留 *
011680*     **********************
011690*     **********************
011700*     * 転帰名称は一時保留 *
011710*     **********************
011720**********
011730* ４部位 *
011740**********
011750     MOVE 会部位コードＷ(4)   TO 負傷コード４.
011760     MOVE 負傷名Ｗ(4)         TO 負傷名４.
011770     MOVE 負傷年Ｗ(4)         TO 負傷年４.
011780     MOVE 負傷月Ｗ(4)         TO 負傷月４.
011790     MOVE 負傷日Ｗ(4)         TO 負傷日４.
011800     MOVE 施術開始年Ｗ(4)     TO 施術開始年４.
011810     MOVE 施術開始月Ｗ(4)     TO 施術開始月４.
011820     MOVE 施術開始日Ｗ(4)     TO 施術開始日４.
011830     MOVE 施術終了年Ｗ(4)     TO 施術終了年４.
011840     MOVE 施術終了月Ｗ(4)     TO 施術終了月４.
011850     MOVE 施術終了日Ｗ(4)     TO 施術終了日４.
011860*     MOVE 負傷年月日区切Ｗ(4) TO 区切４１ 区切４２.
011870*     MOVE 施開年月日区切Ｗ(4) TO 区切４３ 区切４４.
011880*     MOVE 施終年月日区切Ｗ(4) TO 区切４５ 区切４６.
011890*     MOVE 転帰Ｗ(4)           TO 転帰４.
011900     MOVE 治癒チェックＷ(4)   TO 治癒チェック４.
011910     MOVE 中止チェックＷ(4)   TO 中止チェック４.
011920     MOVE 転医チェックＷ(4)   TO 転医チェック４.
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
012030* ５部位 *
012040**********
012050     MOVE 会部位コードＷ(5)   TO 負傷コード５.
012060     MOVE 負傷名Ｗ(5)         TO 負傷名５.
012070     MOVE 負傷年Ｗ(5)         TO 負傷年５.
012080     MOVE 負傷月Ｗ(5)         TO 負傷月５.
012090     MOVE 負傷日Ｗ(5)         TO 負傷日５.
012100     MOVE 施術開始年Ｗ(5)     TO 施術開始年５.
012110     MOVE 施術開始月Ｗ(5)     TO 施術開始月５.
012120     MOVE 施術開始日Ｗ(5)     TO 施術開始日５.
012130     MOVE 施術終了年Ｗ(5)     TO 施術終了年５.
012140     MOVE 施術終了月Ｗ(5)     TO 施術終了月５.
012150     MOVE 施術終了日Ｗ(5)     TO 施術終了日５.
012160*     MOVE 負傷年月日区切Ｗ(5) TO 区切５１ 区切５２.
012170*     MOVE 施開年月日区切Ｗ(5) TO 区切５３ 区切５４.
012180*     MOVE 施終年月日区切Ｗ(5) TO 区切５５ 区切５６.
012190*     MOVE 転帰Ｗ(5)           TO 転帰５.
012200     MOVE 治癒チェックＷ(5)   TO 治癒チェック５.
012210     MOVE 中止チェックＷ(5)   TO 中止チェック５.
012220     MOVE 転医チェックＷ(5)   TO 転医チェック５.
012230*     ******************
012240*     * 日数は一時保留 *
012250*     ******************
012260*     **********************
012270*     * 施術回数は一時保留 *
012280*     **********************
012290*     **********************
012300*     * 転帰名称は一時保留 *
012310*     **********************
012320************
012330* 負傷原因 *
012340************
012350     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
012360     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
012370     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
012380     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
012390     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
012400     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
012410     MOVE 負傷原因Ｗ(7)       TO 負傷原因７.
012420     MOVE 負傷原因Ｗ(8)       TO 負傷原因８.
012430     MOVE 負傷原因Ｗ(9)       TO 負傷原因９.
012440     MOVE 負傷原因Ｗ(10)      TO 負傷原因１０.
012450     MOVE 負傷原因Ｗ(11)      TO 負傷原因１１.
012460     MOVE 負傷原因Ｗ(12)      TO 負傷原因１２.
012470     MOVE 負傷原因Ｗ(13)      TO 負傷原因１３.
012480     MOVE 負傷原因Ｗ(14)      TO 負傷原因１４.
012490     MOVE 負傷原因Ｗ(15)      TO 負傷原因１５.
012500*
012510*================================================================*
012520 負担率取得 SECTION.
012530*
012540     MOVE ZERO TO 本人負担率Ｗ.
012550     MOVE ZERO TO 家族負担率Ｗ.
012560*
012570     MOVE 施術和暦ＷＲ   TO 受－施術和暦.
012580     MOVE 施術年ＷＲ     TO 受－施術年.
012590     MOVE 施術月ＷＲ     TO 受－施術月.
012600     MOVE 患者番号ＷＲ   TO 受－患者番号.
012610     MOVE 枝番ＷＲ       TO 受－枝番.
012620     READ 受診者情報Ｆ
012630     INVALID KEY
012640         MOVE  NC"施術月の受診者情報がありません" TO 連メ－メッセージ
012650         CALL   "MSG001"
012660         CANCEL "MSG001"
012670         PERFORM ファイル閉鎖
012680         MOVE ZERO TO PROGRAM-STATUS
012690         EXIT PROGRAM
012700     NOT INVALID KEY
012701* 14/10～　サブルーチン処理
012702      IF 受－施術和暦年月 >= 41410
012703         PERFORM 負担率取得１４１０
013861       END-IF
013870     END-READ.
013880*
013890     EVALUATE 負担率Ｗ
013900     WHEN 0
013910         MOVE NC"○" TO ０割チェックＷ
013920     WHEN 10
013930         MOVE NC"○" TO １割チェックＷ
013940     WHEN 20
013950         MOVE NC"○" TO ２割チェックＷ
013960     WHEN 30
013970         MOVE NC"○" TO ３割チェックＷ
013980     END-EVALUATE.
013981*================================================================*
013982 負担率取得１４１０ SECTION.
013983*
013984* 受診者Ｆ READ中
013985* 平成14/10～
013986     MOVE ZERO  TO 負担率Ｗ.
013987     INITIALIZE 助成負担率ＷＷ.
013988*
013989     MOVE SPACE TO 連率－負担率取得キー.
013990     INITIALIZE 連率－負担率取得キー.
013991     MOVE 受－施術和暦年月 TO 連率－施術和暦年月.
013992     MOVE 受－患者コード   TO 連率－患者コード.
013993*
013994     CALL   "HUTANRIT".
013995     CANCEL "HUTANRIT".
013996*
013997* / 本体 /
013998     MOVE 連率－実際本体負担率 TO 負担率Ｗ.
013999*
014000*
014001* / 助成 /
014002     IF 受－助成種別 NOT = ZERO
014003        MOVE 連率－助成負担率  TO 助成負担率Ｗ
014004*
014005        COMPUTE 助成負担率Ｗ１ = 助成負担率Ｗ / 10
014006        MOVE 助成負担率Ｗ１ TO  助成負担率文字Ｗ１
014007*
014008        STRING "("                 DELIMITED BY SIZE
014009               助成負担率文字Ｗ１  DELIMITED BY SIZE
014010               "割)"               DELIMITED BY SIZE
014011               INTO 助成負担率表示Ｗ
014012        END-STRING
014013*
014014     END-IF.
014015*
014016*================================================================*
014017*================================================================*
014018* 施術所情報取得 SECTION.
014019*
014020**************************************************
014030* 本院データを使用し、以下の情報を取得           *
014040* ●資格取得年月日.資格取得年月日Ｗに格納        *
014050**************************************************
014060*     MOVE ZERO  TO 施情－施術所番号.
014070*     READ 施術所情報マスタ
014080*     INVALID KEY
014090*         CONTINUE
014100*     NOT INVALID KEY
014110* 資格取得年月日については一時保留
014120*         MOVE 施情－資格取得年        TO 資格取得年Ｗ
014130*         MOVE 施情－資格取得月        TO 資格取得月Ｗ
014140*         MOVE 施情－資格取得日        TO 資格取得日Ｗ
014150*         CONTINUE
014160*     END-READ.
014170*================================================================*
014180 受診者情報取得 SECTION.
014190*
014200**************************************************
014210* 連結データから受診者情報Ｆより以下の情報を取得 *
014220* ● 患者番号.... 患者番号Ｗ                     *
014230* ● 枝番........ 枝番Ｗ                         *
014240* ● 記号 ....... 記号Ｗに格納                   *
014250* ● 番号 ....... 番号Ｗに格納                   *
014260* ● 被保険者カナ.被保険者カナＷに格納           *
014270* ● 被保険者氏名.被保険者氏名Ｗに格納           *
014280* ● 郵便番号１ ..郵便番号１Ｗに格納             *
014290* ● 郵便番号２ ..郵便番号２Ｗに格納             *
014300* ● 住所１ ......住所１Ｗに格納                 *
014310* ● 住所２ ......住所２Ｗに格納                 *
014320* ● 電話番号.....電話番号Ｗに格納               *
014330* ● 患者カナ ....患者カナＷに格納               *
014340* ● 患者氏名 ....患者氏名Ｗに格納               *
014350* ● 続柄 ........名称マスタより続柄Ｗに取得     *
014360* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
014370* ● 患者年 ......患者年Ｗに格納                 *
014380* ● 患者月 ......患者月Ｗに格納                 *
014390* ● 患者日 ......患者日Ｗに格納                 *
014400**************************************************
014410     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
014420     MOVE 施術年ＷＲ         TO 受－施術年.
014430     MOVE 施術月ＷＲ         TO 受－施術月.
014440     MOVE 患者コードＷＲ     TO 受－患者コード.
014450     READ 受診者情報Ｆ
014460     INVALID KEY
014470         CONTINUE
014480*            /* ありえない */
014490     NOT INVALID KEY
014500         MOVE 受－患者番号     TO 患者番号Ｗ
014510         MOVE 受－枝番         TO 枝番Ｗ
014520*         MOVE 受－記号         TO 記号Ｗ
014530*         MOVE 受－番号         TO 番号Ｗ
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
014610         MOVE 受－被保険者カナ TO 被保険者カナＷ
014620         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
014630*
014640         EVALUATE 受－被保険者性別
014650         WHEN 1
014660             MOVE NC"○"  TO 被保険者男チェックＷ
014670         WHEN 2
014680             MOVE NC"○"  TO 被保険者女チェックＷ
014690         END-EVALUATE
014700         EVALUATE 受－患者性別
014710         WHEN 1
014720             MOVE NC"○"  TO 患者男チェックＷ
014730         WHEN 2
014740             MOVE NC"○"  TO 患者女チェックＷ
014750         END-EVALUATE
014760*
014770         EVALUATE 受－被保険者和暦
014780         WHEN 1
014790             MOVE NC"○"  TO 被保険者明治チェックＷ
014800         WHEN 2
014810             MOVE NC"○"  TO 被保険者大正チェックＷ
014820         WHEN 3
014830             MOVE NC"○"  TO 被保険者昭和チェックＷ
014840         WHEN 4
014850             MOVE NC"○"  TO 被保険者平成チェックＷ
014860         END-EVALUATE
014870         MOVE 受－被保険者年   TO 被保険者年Ｗ
014880         MOVE 受－被保険者月   TO 被保険者月Ｗ
014890         MOVE 受－被保険者日   TO 被保険者日Ｗ
014900
014910*被保険者年齢
014920     MOVE 連印－施術和暦 TO 元－元号区分
014930     READ 元号マスタ
014940     NOT INVALID KEY
014950         COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
014960     END-READ
014970     IF ( 受－被保険者和暦 NOT = ZERO ) AND
014980        ( 受－被保険者年   NOT = ZERO ) AND
014990        ( 受－被保険者月   NOT = ZERO ) AND
015000        ( 受－被保険者日   NOT = ZERO )
015010
015020         MOVE ZERO TO 被保険者年齢Ｗ
015030*
015040         MOVE 受－被保険者和暦 TO 元－元号区分
015050         READ 元号マスタ
015060         NOT INVALID KEY
015070             COMPUTE 被保険者西暦年Ｗ = 元－開始西暦年 + ( 受－被保険者年 - 1 )
015080         END-READ
015090*
015100         COMPUTE 被保険者年齢Ｗ   = 施術西暦年Ｗ - 被保険者西暦年Ｗ
015110*
015120         IF 連印－施術月 < 受－被保険者月
015130             COMPUTE 被保険者年齢Ｗ = 被保険者年齢Ｗ - 1
015140         END-IF
015150*
015160         MOVE 被保険者年齢Ｗ     TO 被保険者年齢
015170     END-IF
015180*
015190*/郵便番号 ゼロの時はスペース
015200         IF 受－郵便番号１ = "000"
015210             MOVE SPACE          TO 被保険者郵便番号１Ｗ
015220             MOVE SPACE          TO 被保険者郵便番号２Ｗ
015230         ELSE
015240             MOVE 受－郵便番号１ TO 被保険者郵便番号１Ｗ
015250             MOVE 受－郵便番号２ TO 被保険者郵便番号２Ｗ
015260         END-IF
015270         MOVE 受－電話番号     TO 被保険者電話番号Ｗ
015280         STRING 受－住所１  DELIMITED BY SPACE
015290                受－住所２  DELIMITED BY SPACE
015300           INTO 被保険者住所Ｗ
015310         END-STRING
015320*
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
015330*/郵便番号 ゼロの時はスペース
015340         IF 受－患者郵便番号１ = "000"
015350             MOVE SPACE              TO 患者郵便番号１Ｗ
015360             MOVE SPACE              TO 患者郵便番号２Ｗ
015370         ELSE
015380             MOVE 受－患者郵便番号１ TO 患者郵便番号１Ｗ
015390             MOVE 受－患者郵便番号２ TO 患者郵便番号２Ｗ
015400         END-IF
015410*         MOVE 受－患者住所１     TO 患者住所１Ｗ
015420*         MOVE 受－患者住所２     TO 患者住所２Ｗ
015430         STRING 受－患者住所１  DELIMITED BY SPACE
015440                受－患者住所２  DELIMITED BY SPACE
015450           INTO 患者住所Ｗ
015460         END-STRING
015470         MOVE 受－患者電話番号   TO 患者電話番号Ｗ
015480*
015490*電話番号分割
015500*         UNSTRING 電話番号Ｗ  DELIMITED BY "-"
015510*             INTO 分割電話番号１Ｗ
015520*                  分割電話番号２Ｗ
015530*                  分割電話番号３Ｗ
015540*         END-UNSTRING
015550*電話番号右詰処理
015560*         IF 分割電話番号３Ｗ = SPACE
015570*             MOVE 分割電話番号２Ｗ TO 分割電話番号３Ｗ
015580*             MOVE 分割電話番号１Ｗ TO 分割電話番号２Ｗ
015590*             MOVE SPACE            TO 分割電話番号１Ｗ
015600*         END-IF
015610*
015620         MOVE 受－患者カナ     TO 患者カナＷ
015630         MOVE 受－患者氏名     TO 患者氏名Ｗ
015640*
               MOVE 受－保険種別     TO 事－保険種別
               MOVE 受－保険者番号   TO 事－保険者番号
               MOVE 記号Ｗ           TO 事－記号
               READ 事業所マスタ
               INVALID KEY
                  MOVE SPACE TO 事－レコード
                  INITIALIZE    事－レコード
               END-READ
               MOVE 事－事業所名称   TO 事業所名称Ｗ
015540*         STRING 事－事業所住所１  DELIMITED BY SPACE
015550*                事－事業所住所２  DELIMITED BY SPACE
015560*           INTO 事業所住所Ｗ
015570*         END-STRING
               MOVE 事－事業所住所１ TO 印刷事業所住所Ｗ
               MOVE 事－事業所住所２ TO 印刷事業所住所２Ｗ
015700* 続柄
015710         IF 受－本人家族区分 = 1
015720             MOVE NC"本人"     TO 続柄Ｗ
015730         ELSE
015740             MOVE 05          TO 名－区分コード
015750             MOVE 受－続柄    TO 名－名称コード
015760             READ 名称マスタ
015770             INVALID KEY
015780                 MOVE SPACE   TO 続柄Ｗ
015790             NOT INVALID KEY
015800                 MOVE 名－略称  TO 続柄Ｗ
015810             END-READ
015820         END-IF
015830* 患者和暦年月日
015840         EVALUATE 受－患者和暦
015850         WHEN 1
015860             MOVE NC"○"  TO 明治チェックＷ
015870         WHEN 2
015880             MOVE NC"○"  TO 大正チェックＷ
015890         WHEN 3
015900             MOVE NC"○"  TO 昭和チェックＷ
015910         WHEN 4
015920             MOVE NC"○"  TO 平成チェックＷ
015930         END-EVALUATE
015940         MOVE 受－患者年  TO 患者年Ｗ
015950         MOVE 受－患者月  TO 患者月Ｗ
015960         MOVE 受－患者日  TO 患者日Ｗ
015970*患者年齢
015980     MOVE 連印－施術和暦 TO 元－元号区分
015990     READ 元号マスタ
016000     NOT INVALID KEY
016010         COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
016020     END-READ
016030     IF ( 受－患者和暦 NOT = ZERO ) AND
016040        ( 受－患者年   NOT = ZERO ) AND
016050        ( 受－患者月   NOT = ZERO ) AND
016060        ( 受－患者日   NOT = ZERO )
016070*
016080         MOVE ZERO TO 患者年齢Ｗ
016090*
016100         MOVE 受－患者和暦 TO 元－元号区分
016110         READ 元号マスタ
016120         NOT INVALID KEY
016130             COMPUTE 患者西暦年Ｗ = 元－開始西暦年 + ( 受－患者年 - 1 )
016140         END-READ
016150*
016160         COMPUTE 患者年齢Ｗ   = 施術西暦年Ｗ - 患者西暦年Ｗ
016170*
016180         IF 連印－施術月 < 受－患者月
016190             COMPUTE 患者年齢Ｗ = 患者年齢Ｗ - 1
016200         END-IF
016210*
016220         MOVE 患者年齢Ｗ     TO 患者年齢
016230     END-IF
016240* 患者性別
016250*         EVALUATE 受－患者性別
016260*         WHEN 1
016270*             MOVE NC"（男）" TO 患者性別Ｗ
016280*         WHEN 2
016290*             MOVE NC"（女）" TO 患者性別Ｗ
016300*         END-EVALUATE
016310*/有効期限
016320         MOVE 受－有効和暦   TO 有効期限和暦Ｗ
016330         MOVE 受－有効年     TO 有効期限年Ｗ
016340         MOVE 受－有効月     TO 有効期限月Ｗ
016350         MOVE 受－有効日     TO 有効期限日Ｗ
016360         MOVE 有効期限和暦Ｗ TO 元－元号区分
016370         READ 元号マスタ
016380         INVALID KEY
016390             MOVE SPACE        TO 有効期限元号Ｗ
016400         NOT INVALID KEY
016410             MOVE 元－元号名称 TO 有効期限元号Ｗ
016420         END-READ
016430*/資格取得年月日
016440         MOVE 受－資格和暦 TO 資格取得和暦Ｗ
016450         MOVE 受－資格年   TO 資格取得年Ｗ
016460         MOVE 受－資格月   TO 資格取得月Ｗ
016470         MOVE 受－資格日   TO 資格取得日Ｗ
016480         IF (資格取得和暦Ｗ NOT = ZERO) AND
016490            (資格取得年Ｗ   NOT = ZERO) AND
016500            (資格取得月Ｗ   NOT = ZERO) AND
016510            (資格取得日Ｗ   NOT = ZERO)
016520             EVALUATE 資格取得和暦Ｗ
016530             WHEN 3
016540                 MOVE NC"○"  TO 資格昭和チェックＷ
016550             WHEN 4
016560                 MOVE NC"○"  TO 資格平成チェックＷ
016570             END-EVALUATE
016580         END-IF
016850* 市町村番号 受給者番号
016860         IF 受－助成種別 NOT = ZERO
016870             MOVE 受－費用負担者番号助成 TO 費用負担者番号Ｗ
016880             MOVE 受－受益者番号助成     TO 受益者番号助成Ｗ
016890         END-IF
018290         IF ( 受－公費種別 = 05 ) AND ( 受－施術和暦年月 < 42004 )
016910             MOVE 受－費用負担者番号     TO 市町村番号Ｗ
016920             MOVE 受－受益者番号老人     TO 受益者番号Ｗ
016930         END-IF
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
016940*保険種別
016950         EVALUATE 受－助成種別
016960         WHEN 50
016970             MOVE NC"○"  TO 生保チェックＷ
016980         WHEN 51
016990*/頭4桁が "4113"東京 "4108"茨城 "4132"島根 の時は、「福」。それ以外は「老」
017000             IF  受－費用負担者番号助成(1:4) = "4113" OR
017010                 受－費用負担者番号助成(1:4) = "4108" OR
017020                 受－費用負担者番号助成(1:4) = "4132"
017030                 MOVE NC"○"    TO 福チェックＷ
017040             ELSE
017050                 MOVE NC"○"    TO 老人チェックＷ
017060             END-IF
017070         WHEN 52
017080             MOVE NC"○"  TO 母子チェックＷ
017090         WHEN 53
017100             MOVE NC"○"  TO 身障チェックＷ
017110         WHEN 54
017120             MOVE NC"○"  TO 原爆チェックＷ
017130         WHEN 55
017140             MOVE NC"○"  TO 乳幼児チェックＷ
017150         END-EVALUATE
017160
017170         IF 受－公費種別 = 05
017180             MOVE NC"○"  TO 老人チェックＷ
017190         ELSE
017200             EVALUATE 受－保険種別
017210             WHEN 01
017220                 MOVE NC"○"  TO 国保チェックＷ
017230             WHEN 02
017240                 MOVE NC"○"  TO 社保チェックＷ
017250             WHEN 03
017260                 MOVE NC"○"  TO 組合チェックＷ
017270             WHEN 04
017280                 MOVE NC"○"  TO 共済チェックＷ
017290             WHEN 06
017300                 MOVE NC"○"  TO 日雇チェックＷ
017310             WHEN 07
017320                 MOVE NC"○"  TO 船員チェックＷ
017330             WHEN 08
017340                 MOVE NC"○"  TO 退職チェックＷ
017350             WHEN 09
017360                 MOVE NC"○"  TO 共済チェックＷ
017370*             WHEN 70
017380*                 MOVE NC"○"  TO 労災チェックＷ
017390             WHEN 80
017400                 MOVE NC"○"  TO 自賠チェックＷ
017410             END-EVALUATE
017420         END-IF
017430     END-READ.
017440*HILO テスト－－－－－－－－－－－
017450*      MOVE NC"○"  TO 被保険者明治Ｗ.
017460*      MOVE NC"○"  TO 被保険者大正Ｗ.
017470*      MOVE NC"○"  TO 被保険者昭和Ｗ.
017480*      MOVE NC"○"  TO 被保険者男チェックＷ.
017490*      MOVE NC"○"  TO 被保険者女チェックＷ.
017500*      MOVE NC"○"  TO 患者男チェックＷ.
017510*      MOVE NC"○"  TO 患者女チェックＷ.
017520*      MOVE NC"○"  TO 明治チェックＷ.
017530*      MOVE NC"○"  TO 大正チェックＷ.
017540*      MOVE NC"○"  TO 昭和チェックＷ.
017550*      MOVE NC"○"  TO 平成チェックＷ.
017560*      MOVE NC"○"  TO 社保チェックＷ    .
017570*      MOVE NC"○"  TO 組合チェックＷ    .
017580*      MOVE NC"○"  TO 共済チェックＷ    .
017590*      MOVE NC"○"  TO 国保チェックＷ    .
017600*      MOVE NC"○"  TO 日雇チェックＷ    .
017610*      MOVE NC"○"  TO 船員チェックＷ    .
017620*      MOVE NC"○"  TO 老人チェックＷ    .
017630*      MOVE NC"○"  TO 福チェックＷ    .
017640*      MOVE NC"○"  TO 身障チェックＷ    .
017650*      MOVE NC"○"  TO 退職チェックＷ    .
017660*      MOVE NC"○"  TO 母子チェックＷ    .
017670*      MOVE NC"○"  TO 乳幼児チェックＷ  .
017680*      MOVE NC"○"  TO 国保組合チェックＷ.
017690*      MOVE NC"○"  TO 労災チェックＷ    .
017700*      MOVE NC"○"  TO 生保チェックＷ    .
017710*      MOVE NC"○"  TO 自賠チェックＷ    .
017720*      MOVE NC"○"  TO 原爆チェックＷ    .
017730*      MOVE NC"○" TO ０割チェックＷ.
017740*      MOVE NC"○" TO １割チェックＷ.
017750*      MOVE NC"○" TO ２割チェックＷ.
017760*      MOVE NC"○" TO ３割チェックＷ.
017770*      MOVE NC"○"  TO 資格昭和チェックＷ.
017780*      MOVE NC"○"  TO 資格平成チェックＷ.
017790*      MOVE NC"○"  TO 被保険者明治チェックＷ.
017800*      MOVE NC"○"  TO 被保険者大正チェックＷ.
017810*      MOVE NC"○"  TO 被保険者昭和チェックＷ.
017820*      MOVE NC"○"  TO 被保険者平成チェックＷ.
017830*-------------------------------------
017840*================================================================*
017850 請求先情報取得 SECTION.
017860*
017870****************************************************
017880* 連結データから保険者マスタより請求先を取得する。 *
017900* ● 保険者番号...保険者番号Ｗに格納               *
017910* ● 名称........ 請求先名称Ｗに格納               *
017920* ● 郵便番号１.. 請求先郵便番号１Ｗに格納         *
017930* ● 郵便番号２.. 請求先郵便番号２Ｗに格納         *
017940* ● 住所１.......請求先住所１Ｗに格納             *
017950* ● 住所２.......請求先住所２Ｗに格納             *
017960****************************************************
017970     MOVE SPACE          TO 保険者番号Ｗ.
017980     MOVE SPACE          TO 請求先名称Ｗ.
017990     MOVE SPACE          TO 支部名Ｗ.
018000     MOVE SPACE          TO 請求先郵便番号１Ｗ.
018010     MOVE SPACE          TO 請求先郵便番号２Ｗ.
018020     MOVE SPACE          TO 請求先住所１Ｗ.
018030     MOVE SPACE          TO 請求先住所２Ｗ.
018040     MOVE SPACE          TO 印刷保険者呼名Ｗ.
018050     MOVE SPACE          TO 請求先名支部名Ｗ
018060*
018070     MOVE 保険種別ＷＲ   TO 保－保険種別.
018080     MOVE 保険者番号ＷＲ TO 保－保険者番号.
018090     READ 保険者マスタ
018100     INVALID KEY
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
018140     NOT INVALID KEY
018150         MOVE 保－保険者番号  TO 保険者番号Ｗ
018160         MOVE 保－保険者名称  TO 請求先名称Ｗ
018170         MOVE 保－支部部署名  TO 支部名Ｗ
018180         MOVE 保－郵便番号１  TO 請求先郵便番号１Ｗ
018190         MOVE 保－郵便番号２  TO 請求先郵便番号２Ｗ
018200         MOVE 保－住所１      TO 請求先住所１Ｗ
018210         MOVE 保－住所２      TO 請求先住所２Ｗ
018220         MOVE 保－接尾語区分  TO 接尾語区分Ｗ
018230     END-READ.
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
018240     EVALUATE 保険種別ＷＲ
018250     WHEN 2
018260     WHEN 6
018270         IF 接尾語区分Ｗ = 1
018280            MOVE SPACE              TO 印刷保険者呼名Ｗ
018290         ELSE
018300            MOVE "社会保険事務所"   TO 印刷保険者呼名Ｗ
018310         END-IF
018320     WHEN 3
018330         MOVE "健康保険組合"   TO 印刷保険者呼名Ｗ
018340     WHEN OTHER
018350         MOVE SPACE TO 印刷保険者呼名Ｗ
018360     END-EVALUATE.
018370     IF 印刷保険者呼名Ｗ NOT = SPACE
018380         STRING 請求先名称Ｗ DELIMITED BY SPACE
018390                保険者呼名Ｗ DELIMITED BY SPACE
018400           INTO 請求先名称Ｗ
018410         END-STRING
018420     END-IF.
018430*組合と共済は支部名も印字する
018440     EVALUATE 保険種別ＷＲ
018450     WHEN 03
018460     WHEN 04
018470         STRING 請求先名称Ｗ DELIMITED BY SPACE
018480                "  "         DELIMITED BY SIZE
018490                支部名Ｗ     DELIMITED BY SPACE
018500           INTO 請求先名支部名Ｗ
018510         END-STRING
018520     WHEN OTHER
018530         MOVE 請求先名称Ｗ TO 請求先名支部名Ｗ
018540     END-EVALUATE.
018550*================================================================*
018560 負傷データ取得 SECTION.
018570*
018580**************************************************
018590* 連結データから負傷データＦより以下の情報を取得 *
018600* ● 負傷名...部位＋負傷種別にて加工して格納     *
018610* ● 負傷年.......負傷年Ｗ                       *
018620* ● 負傷月.......負傷月Ｗ                       *
018630* ● 負傷日.......負傷日Ｗ                       *
018640* ● 施術終了年...終了年Ｗ                       *
018650* ● 施術終了月...終了月Ｗ                       *
018660* ● 施術終了日...終了日Ｗ                       *
018670**************************************************
018680     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
018690     MOVE 施術年ＷＲ         TO 負－施術年.
018700     MOVE 施術月ＷＲ         TO 負－施術月.
018710     MOVE 患者コードＷＲ     TO 負－患者コード.
018720     READ 負傷データＦ
018730     INVALID KEY
018740         CONTINUE
018750*            /* ありえない */
018760     NOT INVALID KEY
018770         MOVE ZERO                         TO 部位数Ｗ
018780         MOVE 負－部位数                   TO 部位数Ｗ
018790         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018800                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
018810*********************************************
018820* 注）全柔...負傷種別＋部位にて加工して格納 *
018830*********************************************
018840* 負傷種別
018850             MOVE SPACE                     TO 負傷名称Ｗ
018860             MOVE 03                        TO 名－区分コード
018870             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
018880             READ 名称マスタ
018890             INVALID KEY
018900                 MOVE SPACE    TO 負傷名称Ｗ
018910             NOT INVALID KEY
018920*                 MOVE 名－略称 TO 負傷名称Ｗ
018930                 MOVE 名－正式名称 TO 負傷名称Ｗ
018940             END-READ
018950* 部位
018960             MOVE 負－部位コード(部位ＣＮＴ)  TO 部－部位コード
018970             PERFORM 名称埋込処理
019030             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
019040             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
019050             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
019060*             IF 負－負傷年(部位ＣＮＴ) NOT = ZERO 
019070*                MOVE "."                   TO 負傷年月日区切Ｗ(部位ＣＮＴ)
019080*             END-IF
019090*
019100             MOVE 負－開始年(部位ＣＮＴ)   TO 施術開始年Ｗ(部位ＣＮＴ)
019110             MOVE 負－開始月(部位ＣＮＴ)   TO 施術開始月Ｗ(部位ＣＮＴ)
019120             MOVE 負－開始日(部位ＣＮＴ)   TO 施術開始日Ｗ(部位ＣＮＴ)
019130*             IF 負－開始年(部位ＣＮＴ) NOT = ZERO 
019140*                MOVE "."                   TO 施開年月日区切Ｗ(部位ＣＮＴ)
019150*             END-IF
019160*
019170             MOVE 負－終了年(部位ＣＮＴ)   TO 施術終了年Ｗ(部位ＣＮＴ)
019180             MOVE 負－終了月(部位ＣＮＴ)   TO 施術終了月Ｗ(部位ＣＮＴ)
019190             MOVE 負－終了日(部位ＣＮＴ)   TO 施術終了日Ｗ(部位ＣＮＴ)
019200*             IF 負－終了年(部位ＣＮＴ) NOT = ZERO 
019210*                MOVE "."                   TO 施終年月日区切Ｗ(部位ＣＮＴ)
019220*             END-IF
019230*転帰
019240             EVALUATE 負－転帰区分(部位ＣＮＴ)
019250             WHEN 1
019260             WHEN 2
019270                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
019280             WHEN 3
019290                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
019300             WHEN 4
019310                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
019320             END-EVALUATE
019330*             IF 負－転帰区分(部位ＣＮＴ) NOT = 9
019340*                 MOVE 04                        TO 名－区分コード
019350*                 MOVE 負－転帰区分(部位ＣＮＴ)  TO 名－名称コード
019360*                 READ 名称マスタ
019370*                 INVALID KEY
019380*                     MOVE SPACE    TO 転帰Ｗ(部位ＣＮＴ)
019390*                 NOT INVALID KEY
019400*                     MOVE 名－略称 TO 転帰Ｗ(部位ＣＮＴ)
019410*                 END-READ
019420*             END-IF
019430*hilo---------------------------------
019440*         MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
019450*         MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
019460*         MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
019470*-------------------------------------
019480         END-PERFORM
019490     END-READ.
019500*================================================================*
019510 施術記録取得 SECTION.
019520*
019530************************************************************
019540* 連結データから負傷データＦより以下の情報を取得           *
019550* ● 施術開始年月日...該当する部位に対して当月最初の施術日 *
019560************************************************************
019570     MOVE 施術和暦ＷＲ  TO 施記－施術和暦.
019580     MOVE 施術年ＷＲ    TO 施記－施術年.
019590     MOVE 施術月ＷＲ    TO 施記－施術月.
019600     MOVE ZERO            TO 施記－施術日.
019610     MOVE ZERO            TO 施記－患者番号.
019620     MOVE SPACE           TO 施記－枝番.
019630     START 施術記録Ｆ   KEY IS >= 施記－施術和暦年月日
019640                                  施記－患者コード.
019650     IF 状態キー = "00"
019660         MOVE SPACE  TO 終了フラグ２
019670         PERFORM 施術記録Ｆ読込
019680         PERFORM UNTIL ( 終了フラグ２       = "YES" ) OR
019690                       ( 施記－施術和暦 NOT = 施術和暦ＷＲ ) OR
019700                       ( 施記－施術年   NOT = 施術年ＷＲ   ) OR
019710                       ( 施記－施術月   NOT = 施術月ＷＲ   )
019720*            **************
019730*            * 開始年月日 *
019740*            **************
019750             PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
019760                     UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
019770            EVALUATE TRUE ALSO TRUE ALSO TRUE ALSO TRUE ALSO TRUE
019780            WHEN 施記－負傷種別(部位ＣＮＴ) = 負傷種別Ｗ(部位ＣＮＴ) ALSO
019790                 施記－部位(部位ＣＮＴ)     = 部位Ｗ(部位ＣＮＴ)     ALSO
019800                 施記－左右区分(部位ＣＮＴ) = 左右区分Ｗ(部位ＣＮＴ) ALSO
019810                 施記－負傷位置番号(部位ＣＮＴ)
019820                                       = 負傷位置番号Ｗ(部位ＣＮＴ)  ALSO
019830                 開始年月日取得フラグ(部位ＣＮＴ) = SPACE
019840                   MOVE 施記－施術年     TO 施術開始年Ｗ(部位ＣＮＴ)
019850                   MOVE 施記－施術月     TO 施術開始月Ｗ(部位ＣＮＴ)
019860                   MOVE 施記－施術日     TO 施術開始日Ｗ(部位ＣＮＴ)
019870                   MOVE "YES"       TO 開始年月日取得フラグ(部位ＣＮＴ)
019880*
019890            WHEN OTHER
019900                CONTINUE
019910            END-EVALUATE
019920             END-PERFORM
019930             PERFORM 施術記録Ｆ読込
019940         END-PERFORM
019950     END-IF.
019960*================================================================*
019970 施術記録Ｆ読込 SECTION.
019980*
019990     READ 施術記録Ｆ NEXT
020000     AT END
020010         MOVE "YES" TO 終了フラグ２
020020     END-READ.
020030*
020040*================================================================*
020050 負傷原因取得 SECTION.
020060*
020070********************************************************************
020080*  負傷原因コードが同じものは、1行にまとめて印字する。
020090*  例: ①② 家で転んだ.
020100*     負傷原因コードが同じものをまとめ、テーブルにセット
020110*     (ただし、部位を飛んで同じものは、2行になる)
020120********************************************************************
020130     MOVE  ZERO   TO  カウンタ カウンタ２.
020140     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
020150             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
020160*
020170****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
020180        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
020190*
020200           IF カウンタ = ZERO
020210               MOVE 1   TO  カウンタ カウンタ２
020220               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
020230               MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
020240               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
020250           ELSE
020260              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
020270                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
020280                 COMPUTE カウンタ２ = カウンタ２  +  1
020290                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
020300              ELSE
020310                 COMPUTE カウンタ = カウンタ  +  1
020320                 MOVE 1   TO  カウンタ２
020330                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
020340                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
020350                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
020360              END-IF
020370           END-IF
020380        END-IF
020390     END-PERFORM.
020400**************************************************************************
020410*  負傷原因マスタより文章取得
020420**************************************************************************
020430     MOVE  ZERO   TO  カウンタ カウンタ２.
020440     PERFORM VARYING カウンタ FROM 1 BY 1
020450             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
020460** 健保は 区分 01
020470*         MOVE 01                        TO 負原－区分コード
      */健保：０１、労災：０２、自賠責：０３
               EVALUATE 受－保険種別
               WHEN 70
                   MOVE 2 TO 負原－区分コード
               WHEN 80
                   MOVE 3 TO 負原－区分コード
               WHEN OTHER
                   MOVE 1 TO 負原－区分コード
               END-EVALUATE
020480         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
020490         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
020500         READ 負傷原因Ｆ
020510         NOT INVALID KEY
020520             INITIALIZE 負傷原因ＷＴ
020530             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
020540             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
020550             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
020560             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
020570             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
020580             PERFORM VARYING カウンタ２ FROM 1 BY 1
020590                     UNTIL ( カウンタ２ > 9 )  OR 
020600                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
020610                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
020620                WHEN 1
020630                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
020640                WHEN 2
020650                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
020660                WHEN 3
020670                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
020680                WHEN 4
020690                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
020700                WHEN 5
020710                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
020720                WHEN OTHER
020730                   CONTINUE
020740                END-EVALUATE
020750             END-PERFORM
020760*
020842             IF 負原－負傷原因入力区分 = 1
020843                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
020844                        負傷原因１ＷＴ  DELIMITED BY SIZE
020845                        負傷原因２ＷＴ  DELIMITED BY SIZE
020846                        負傷原因３ＷＴ  DELIMITED BY SIZE
020847                        負傷原因４ＷＴ  DELIMITED BY SIZE
020848                        負傷原因５ＷＴ  DELIMITED BY SIZE
020849                        INTO 負傷原因内容合成Ｗ(カウンタ)
020850                 END-STRING
020851             ELSE
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
020860             END-IF
020861*
020862         END-READ
020863     END-PERFORM.
020870*
020880     PERFORM 負傷原因セット.
020890*
020900*================================================================*
020910 負傷原因セット SECTION.
020920*
020930**************************************************************************
020940*  文章が1行を超える時は、複数行に分解する。
020950**************************************************************************
020960     MOVE  ZERO   TO  カウンタ カウンタ２.
020970     PERFORM VARYING カウンタ FROM 1 BY 1
020980             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
020990*
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
021220*
021230     END-PERFORM.
021240*
021250*================================================================*
021260 印刷処理 SECTION.
021270*
021280     EVALUATE 印刷モードＦＷＲ
021290     WHEN 0
021300         PERFORM 印刷処理２
021310         PERFORM 印刷処理３
021320     WHEN 1
021330         PERFORM 印刷処理２
021340     WHEN 2
021350         PERFORM 印刷処理３
021360     WHEN 3
021370         PERFORM 印刷処理４
021380     END-EVALUATE.
021390*================================================================*
021400 印刷処理１ SECTION.
021410*
021420     MOVE "YCK662P" TO  定義体名Ｐ.
021430*     MOVE "SCREEN"  TO  項目群名Ｐ.
021440     MOVE "GRP001"  TO  項目群名Ｐ.
021450     WRITE YCK662P.
021460*     WRITE 印刷レコード.
021470     PERFORM エラー処理Ｐ.
021480*================================================================*
021490 印刷処理２ SECTION.
021500*
021510     MOVE "YCK662P"  TO  定義体名Ｐ.
021520     MOVE "GRP001"   TO  項目群名Ｐ.
021530     WRITE YCK662P.
021540*     WRITE 印刷レコード.
021550     PERFORM エラー処理Ｐ.
021560*================================================================*
021570 印刷処理３ SECTION.
021580*
021590     MOVE "YCK662P"  TO  定義体名Ｐ.
021600     MOVE "GRP002"   TO  項目群名Ｐ.
021610     WRITE YCK662P.
021620*     WRITE 印刷レコード.
021630     PERFORM エラー処理Ｐ.
021640*================================================================*
021650 印刷処理４ SECTION.
021660*
021670     MOVE "YCK662P"  TO  定義体名Ｐ.
021680     MOVE "GRP003"   TO  項目群名Ｐ.
021690     WRITE YCK662P.
021700     PERFORM エラー処理Ｐ.
021710*================================================================*
021720 エラー処理Ｐ SECTION.
021730*
021740     IF 通知情報Ｐ NOT = "00"
021750         DISPLAY NC"帳票エラー"              UPON CONS
021760         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
021770         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
021780         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
021790         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
021800                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
021810         ACCEPT  キー入力 FROM CONS
021820         PERFORM ファイル閉鎖
021830         EXIT PROGRAM
021840     END-IF.
021850*================================================================*
021860 名称埋込処理 SECTION.
021870*
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
021890     READ 部位マスタ
021920     NOT INVALID KEY
021970         MOVE 部－会部位コード TO 会部位コードＷ (部位ＣＮＴ)
021980     END-READ.
021990*
022280*================================================================*
022290 合計金額行セット SECTION.
022300*
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
019600     READ レセプトＦ.
022310     MOVE 提出年ＷＲ   TO 提出年    (段数ＷＲ).
022320     MOVE 提出月ＷＲ   TO 提出月    (段数ＷＲ).
022330     MOVE 提出日ＷＲ   TO 提出日    (段数ＷＲ).
022340     MOVE 施術年ＷＲ   TO 請求開始年(段数ＷＲ) 請求終了年(段数ＷＲ).
022350     MOVE 施術月ＷＲ   TO 請求開始月(段数ＷＲ) 請求終了月(段数ＷＲ).
022360     MOVE 1            TO 請求開始日(段数ＷＲ).
022370     MOVE 請求終了日Ｗ TO 請求終了日(段数ＷＲ).
022380     MOVE レセ－合計       TO 費用額    (段数ＷＲ).
022390     MOVE レセ－一部負担金 TO 負担額    (段数ＷＲ).
022400     MOVE レセ－請求金額   TO 請求額    (段数ＷＲ).
022410*================================================================*
022420  請求期間取得 SECTION.
022430*
022440     EVALUATE 施術月ＷＲ
022450     WHEN 4
022460     WHEN 6
022470     WHEN 9
022480     WHEN 11
022490         MOVE 30 TO 請求終了日Ｗ
022500     WHEN 2
022510         COMPUTE 対象西暦Ｗ = 和暦終了年Ｗ + 施術年ＷＲ
022520         DIVIDE 4 INTO 対象西暦Ｗ GIVING    商Ｗ
022530                                  REMAINDER 余Ｗ
022540         END-DIVIDE
022550         IF 余Ｗ = ZERO
022560             MOVE 29 TO 請求終了日Ｗ
022570         ELSE
022580             MOVE 28 TO 請求終了日Ｗ
022590         END-IF
022600     WHEN 1
022610     WHEN 3
022620     WHEN 5
022630     WHEN 7
022640     WHEN 8
022650     WHEN 10
022660     WHEN 12
022670         MOVE 31 TO 請求終了日Ｗ
022680     WHEN OTHER
022690         MOVE  NC"未知のエラー発生" TO 連メ－メッセージ
022700         CALL   "MSG001"
022710         CANCEL "MSG001"
022720     END-EVALUATE.
023900*
023910*================================================================*
023920******************************************************************
023930 END PROGRAM YCK662.
023940******************************************************************
