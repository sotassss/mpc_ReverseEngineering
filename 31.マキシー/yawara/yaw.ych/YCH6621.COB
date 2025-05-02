000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCH6621.
000060 AUTHOR.                 山田　浩之
000070*
000080*----------------------------------------------------------------*
000090*         中央用
000100*　　　　　カルテ印刷２（柔ｳｨﾝﾄﾞｳｽﾞ95版）
000110*         MED = YAW662G YCH6621P
      * 2014/10/15 受給者番号15桁対応
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           1998-10-22
000140 DATE-COMPILED.          1998-10-22
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
           SELECT  事業所マスタ    ASSIGN      TO        JIGYOSL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  事－保険種別
                                                                事－保険者番号
                                                                事－記号
                                   FILE STATUS              IS  状態キー
                                   LOCK        MODE         IS  AUTOMATIC.
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
001860     COPY SEIGYO01        OF  XFDLIB  JOINING   制０１   AS  PREFIX.
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
000284*
000282 FD  自賠責情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
000283     COPY JIBAIJ      OF  XFDLIB  JOINING   自賠   AS  PREFIX.
000114*
000115 FD  保険会社マスタ   BLOCK   CONTAINS   1   RECORDS.
000116     COPY HOKENCOM    OF  XFDLIB  JOINING   保険会   AS  PREFIX.
002140*
       FD  事業所マスタ        BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY JIGYOS          OF  XFDLIB  JOINING   事   AS  PREFIX.
002140*
002200 FD  印刷ファイル.
002210     COPY YCH6621P        OF  XMDLIB.
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
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002340*/負傷原因を部位毎の欄に印刷するように変更0302
002350 01 負傷原因ＣＮＴ                     PIC 9(2)  VALUE ZERO.
002360 01 部位番号Ｗ                         PIC 9(1)  VALUE ZERO.
002370*
002380 01 初検フラグ                         PIC X(3)  VALUE SPACE.
002390 01 ファイル名                         PIC N(2)  VALUE SPACE.
002400 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
002410 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
002400 01 用紙Ｗ                             PIC 9     VALUE ZERO.
002420 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002430 01 部位名称Ｗ                         PIC N(10) VALUE SPACE.
002440 01 負傷番号Ｗ                         PIC 9     VALUE ZERO.
002450 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
002460    03 負傷番号Ｗ１                    PIC X.
002470*
002480 01 全角負傷番号Ｗ                     PIC N     VALUE SPACE.
002490 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
002500    03 全角負傷番号Ｗ１                PIC X(2).
002510*
002520 01 負担率Ｗ                           PIC 9(3) VALUE ZERO.
002530 01 甲乙区分Ｗ                         PIC 9    VALUE ZERO.
002540 01 本人負担率Ｗ                       PIC 9(3) VALUE ZERO.
002550 01 家族負担率Ｗ                       PIC 9(3) VALUE ZERO.
002560*
002570 01 施術和暦年月日ＣＷ.
002580   03 施術和暦年月ＣＷ.
002590     05 施術和暦ＣＷ                   PIC 9    VALUE ZERO.
002600     05 施術年月ＣＷ.
002610        07 施術年ＣＷ                  PIC 9(2) VALUE ZERO.
002620        07 施術月ＣＷ                  PIC 9(2) VALUE ZERO.
002630   03 施術日ＣＷ                       PIC 9(2) VALUE ZERO.
002640 01 施術西暦年Ｗ                       PIC 9(4) VALUE ZERO.
002650 01 患者西暦年Ｗ                       PIC 9(4) VALUE ZERO.
002660 01 被保険者西暦年Ｗ                   PIC 9(4) VALUE ZERO.
002670*
002680** 負傷原因用
002690*
002700 01 カウンタ                           PIC 9(2)  VALUE ZERO.
002710 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
002720*
002730 01 負傷原因ＷＴ.
002740    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
002750    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
002760    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
002770    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
002780    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
002790    03 負傷原因ナンバーＷＴ.
002800       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
002810    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
002820 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
002830 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
002840 01 負傷原因ＴＢＬ.
002850    03 負傷原因コードＴＢＬ            OCCURS 9.
002860       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
002870       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
002880       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
002890 01 負傷原因内容Ｗ.
002900    03 負傷原因内容合成Ｗ              PIC X(316) OCCURS 9 VALUE SPACE.
002910    03 負傷原因内容分解ＸＷ.
002920       05 負傷原因内容１ＸＷ           PIC X(30)  VALUE SPACE.
002930       05 負傷原因内容２ＸＷ           PIC X(30)  VALUE SPACE.
002940       05 負傷原因内容３ＸＷ           PIC X(30)  VALUE SPACE.
002950       05 負傷原因内容４ＸＷ           PIC X(30)  VALUE SPACE.
002960       05 負傷原因内容５ＸＷ           PIC X(30)  VALUE SPACE.
002970       05 負傷原因内容６ＸＷ           PIC X(30)  VALUE SPACE.
002980       05 負傷原因内容７ＸＷ           PIC X(30)  VALUE SPACE.
002980       05 負傷原因内容８ＸＷ           PIC X(30)  VALUE SPACE.
002980       05 負傷原因内容９ＸＷ           PIC X(30)  VALUE SPACE.
002980       05 負傷原因内容１０ＸＷ         PIC X(30)  VALUE SPACE.
002990       05 負傷原因内容１１ＸＷ         PIC X(16)  VALUE SPACE.
003000*
003010** 負傷原因印刷区分用
003020 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
003030*
003040** バーコード区分用
003050 01 バーコード使用区分Ｗ               PIC 9 VALUE ZERO.
003060*
003070* 社保用
003080 01 接尾語区分Ｗ                       PIC 9     VALUE ZERO.
003090*
003100****************
003110* 連結項目待避 *
003120****************
003130*    ************
003140*    * 印刷キー *
003150*    ************
003160 01 対象データＷＲ.
003170    03 施術和暦年月ＷＲ.
003180       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
003190       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
003200       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
003210    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
003220    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
003230    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
003240    03 被保険者カナＷＲ                 PIC X(20) VALUE SPACE.
003250    03 患者コードＷＲ.
003260       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
003270       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
003280    03 印刷モードＦＷＲ                 PIC 9(1)  VALUE ZERO.
003290    03 提出年月日ＷＲ.
003300       05 提出年ＷＲ                    PIC 9(2)  VALUE ZERO.
003310       05 提出月ＷＲ                    PIC 9(2)  VALUE ZERO.
003320       05 提出日ＷＲ                    PIC 9(2)  VALUE ZERO.
003330    03 段数ＷＲ                         PIC 9(1)  VALUE ZERO.
003340************
003350* 料金情報 *
003360************
003370*
003380 01 印刷負担率Ｗ                       PIC 9(1)  VALUE ZERO.
003390**************
003400* 受診者情報 *
003410**************
003420 01 受診者情報Ｗ.
003430    03 患者コードＷ.
003440       05 患者番号Ｗ                   PIC 9(6)  VALUE ZERO.
003450       05 枝番Ｗ                       PIC X(1)  VALUE SPACE.
003460    03 市町村番号Ｗ.
003470       05 印刷市町村番号Ｗ             PIC X(8)  VALUE SPACE.
003480       05 FILLER                       PIC X(2).
003490    03 受益者番号Ｗ.
003500       05 印刷受益者番号Ｗ             PIC X(7)  VALUE SPACE.
003510       05 FILLER                       PIC X(13).
003520    03 費用負担者番号Ｗ.
003530       05 印刷費用負担者番号Ｗ         PIC X(8)  VALUE SPACE.
003540       05 FILLER                       PIC X(2).
004350*/15桁印字/1410
003550    03 受益者番号助成Ｗ.
003560       05 印刷受益者番号助成Ｗ         PIC X(15)  VALUE SPACE.
003570       05 FILLER                       PIC X(10).
003580    03 記号Ｗ                          PIC X(24)  VALUE SPACE.
003590    03 番号Ｗ.
003600       05 印刷番号Ｗ                   PIC X(30)  VALUE SPACE.
003610*       05 FILLER                       PIC X(18).
003620    03 被保険者情報Ｗ.
003630       05 被保険者カナＷ               PIC X(50)  VALUE SPACE.
003640       05 被保険者氏名Ｗ               PIC X(50)  VALUE SPACE.
003650*
003660*       05 被保険者和暦チェックＷ.
003670*          07 被保険者明治Ｗ            PIC N(1)  VALUE SPACE.
003680*          07 被保険者大正Ｗ            PIC N(1)  VALUE SPACE.
003690*          07 被保険者昭和Ｗ            PIC N(1)  VALUE SPACE.
003700       05 被保険者性別チェックＷ.
003710          07 被保険者男チェックＷ      PIC N(1)  VALUE SPACE.
003720          07 被保険者女チェックＷ      PIC N(1)  VALUE SPACE.
003730       05 被保険者性別Ｗ               PIC 9     VALUE ZERO.
003740       05 被保険者和暦チェックＷ.
003750          07 被保険者明治チェックＷ    PIC N(1)  VALUE SPACE.
003760          07 被保険者大正チェックＷ    PIC N(1)  VALUE SPACE.
003770          07 被保険者昭和チェックＷ    PIC N(1)  VALUE SPACE.
003780          07 被保険者平成チェックＷ    PIC N(1)  VALUE SPACE.
003780          07 被保険者令和チェックＷ    PIC N(1)  VALUE SPACE.
003790       05 被保険者生年月日Ｗ.
003800          07 被保険者和暦Ｗ            PIC 9     VALUE ZERO.
003810          07 被保険者年Ｗ              PIC 9(2)  VALUE ZERO.
003820          07 被保険者月Ｗ              PIC 9(2)  VALUE ZERO.
003830          07 被保険者日Ｗ              PIC 9(2)  VALUE ZERO.
003840       05 被保険者年齢Ｗ               PIC 9(3) VALUE ZERO.
003850*
003860       05 被保険者郵便番号Ｗ.
003870          07 被保険者郵便番号１Ｗ      PIC X(3)  VALUE SPACE.
003880          07 被保険者郵便番号２Ｗ      PIC X(4)  VALUE SPACE.
003890       05 被保険者住所Ｗ.
003900          07 印刷被保険者住所Ｗ        PIC X(80)  VALUE SPACE.
003910       05 被保険者電話番号Ｗ           PIC X(15)  VALUE SPACE.
003920       05 分割電話番号Ｗ.
003930          07 分割電話番号１Ｗ          PIC X(5)  VALUE SPACE.
003940          07 分割電話番号２Ｗ          PIC X(5)  VALUE SPACE.
003950          07 分割電話番号３Ｗ          PIC X(5)  VALUE SPACE.
003960*          07 市外局番Ｗ                PIC X(5)  VALUE SPACE.
003970*          07 分割局番Ｗ                PIC X(5)  VALUE SPACE.
003980*          07 分割番号Ｗ                PIC X(5)  VALUE SPACE.
003990
004000    03 資格取得年月日Ｗ.
004010       05 資格取得元号Ｗ               PIC N(2)  VALUE SPACE.
004020       05 資格取得和暦Ｗ               PIC 9(1)  VALUE ZERO.
004030       05 資格取得年Ｗ                 PIC 9(2)  VALUE ZERO.
004040       05 資格取得月Ｗ                 PIC 9(2)  VALUE ZERO.
004050       05 資格取得日Ｗ                 PIC 9(2)  VALUE ZERO.
004060    03 有効期限年月日Ｗ.
004070       05 有効期限元号Ｗ               PIC N(2)  VALUE SPACE.
004080       05 有効期限和暦Ｗ               PIC 9(1)  VALUE ZERO.
004090       05 有効期限年Ｗ                 PIC 9(2)  VALUE ZERO.
004100       05 有効期限月Ｗ                 PIC 9(2)  VALUE ZERO.
004110       05 有効期限日Ｗ                 PIC 9(2)  VALUE ZERO.
004120       05 資格和暦チェックＷ.
004130          07 資格昭和チェックＷ        PIC N(1)  VALUE SPACE.
004140          07 資格平成チェックＷ        PIC N(1)  VALUE SPACE.
004140          07 資格令和チェックＷ        PIC N(1)  VALUE SPACE.
004150    03 患者情報Ｗ.
004160       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
004170       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
004180       05 患者性別チェックＷ.
004190          07 患者男チェックＷ          PIC N(1)  VALUE SPACE.
004200          07 患者女チェックＷ          PIC N(1)  VALUE SPACE.
004210       05 続柄Ｗ.
004220          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
004230          07 FILLER                    PIC X(4).
004240       05 和暦チェックＷ.
004250          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
004260          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
004270          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
004280          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
004280          07 令和チェックＷ            PIC N(1)  VALUE SPACE.
004290       05 患者年Ｗ                     PIC 9(2) VALUE ZERO.
004300       05 患者月Ｗ                     PIC 9(2) VALUE ZERO.
004310       05 患者日Ｗ                     PIC 9(2) VALUE ZERO.
004320       05 患者年齢Ｗ                   PIC 9(3) VALUE ZERO.
004330       05 患者性別Ｗ                   PIC N(4) VALUE SPACE.
004340    03 保険種別チェックＷ.
004350       05 社保チェックＷ               PIC N(1) VALUE SPACE.
004360       05 組合チェックＷ               PIC N(1) VALUE SPACE.
004370       05 共済チェックＷ               PIC N(1) VALUE SPACE.
004380       05 国保チェックＷ               PIC N(1) VALUE SPACE.
004390       05 日雇チェックＷ               PIC N(1) VALUE SPACE.
004400       05 船員チェックＷ               PIC N(1) VALUE SPACE.
004410       05 老人チェックＷ               PIC N(1) VALUE SPACE.
004420       05 福チェックＷ                 PIC N(1) VALUE SPACE.
004430       05 身障チェックＷ               PIC N(1) VALUE SPACE.
004440       05 退職チェックＷ               PIC N(1) VALUE SPACE.
004450       05 母子チェックＷ               PIC N(1) VALUE SPACE.
004460       05 乳幼児チェックＷ             PIC N(1) VALUE SPACE.
004470       05 国保組合チェックＷ           PIC N(1) VALUE SPACE.
004480       05 労災チェックＷ               PIC N(1) VALUE SPACE.
004490       05 自賠チェックＷ               PIC N(1) VALUE SPACE.
004500       05 生保チェックＷ               PIC N(1) VALUE SPACE.
004510       05 原爆チェックＷ               PIC N(1) VALUE SPACE.
004520       05 患者郵便番号Ｗ.
004530          07 患者郵便番号１Ｗ          PIC X(3)  VALUE SPACE.
004540          07 患者郵便番号２Ｗ          PIC X(4)  VALUE SPACE.
004550       05 患者住所Ｗ.
004560          07 患者住所１Ｗ              PIC X(40) VALUE SPACE.
004570          07 患者住所２Ｗ              PIC X(40) VALUE SPACE.
004580       05 患者電話番号Ｗ               PIC X(15) VALUE SPACE.
004590    03 負傷原因Ｗ                      PIC X(30) OCCURS 65 VALUE SPACE.
004600**************
004610* 事業所情報 *
004620**************
004630 01 事業所情報Ｗ.
004640    03 事業所名称Ｗ.
004650       05 印刷事業所名称Ｗ             PIC X(60)  VALUE SPACE.
004660    03 事業所郵便番号Ｗ.
004670       05 事業所郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
004680       05 事業所郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
004690    03 事業所住所Ｗ.
004700       05 印刷事業所住所Ｗ             PIC X(50)  VALUE SPACE.
004700       05 印刷事業所住所２Ｗ           PIC X(50)  VALUE SPACE.
004720**************
004730* 請求先情報 *
004740**************
004750 01 請求先情報Ｗ.
004760    03 保険者番号Ｗ.
004770       05 印刷保険者番号Ｗ             PIC X(8)  VALUE SPACE.
004780       05 FILLER                       PIC X(2).
004790    03 請求先名称Ｗ.
004800       05 印刷請求先名称Ｗ             PIC X(40) VALUE SPACE.
004810    03 支部名Ｗ.
004820       05 印刷支部名Ｗ                 PIC X(40) VALUE SPACE.
004830    03 請求先名支部名Ｗ.
004840       05 印刷請求先名支部名Ｗ         PIC X(54) VALUE SPACE.
004850       05 FILLER                       PIC X(26).
004860    03 保険者呼名Ｗ.
004870*       05 印刷保険者呼名Ｗ             PIC N(7)  VALUE SPACE.
004880       05 印刷保険者呼名Ｗ             PIC X(14)  VALUE SPACE.
004890    03 請求先郵便番号Ｗ.
004900       05 請求先郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
004910       05 請求先郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
004920    03 請求先住所１Ｗ.
004930       05 印刷請求先住所１Ｗ           PIC X(40) VALUE SPACE.
004940    03 請求先住所２Ｗ.
004950       05 印刷請求先住所２Ｗ           PIC X(35) VALUE SPACE.
004960       05 FILLER                       PIC X(5).
004970****************
004980* 負傷データＦ *
004990****************
005000 01 負傷情報Ｗ.
005010    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
005020    03 負傷データ情報Ｗ  OCCURS   9.
005030       05 会部位コードＷ               PIC X(4)  VALUE SPACE.
005040       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
005050       05 負傷種別Ｗ                   PIC 9(2)  VALUE ZERO.
005060       05 部位Ｗ                       PIC 9(2)  VALUE ZERO.
005070       05 左右区分Ｗ                   PIC 9(1)  VALUE ZERO.
005080       05 負傷位置番号Ｗ               PIC 9(2)  VALUE ZERO.
005090       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
005100       05 負傷年月日Ｗ.
005110          07 負傷和暦Ｗ                PIC 9(1)  VALUE ZERO.
005120          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
005130          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
005140          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
005150          07 負傷年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005160       05 施術開始年月日Ｗ.
005170          07 施術開始和暦Ｗ            PIC 9(1)  VALUE ZERO.
005180          07 施術開始年Ｗ              PIC 9(2)  VALUE ZERO.
005190          07 施術開始月Ｗ              PIC 9(2)  VALUE ZERO.
005200          07 施術開始日Ｗ              PIC 9(2)  VALUE ZERO.
005210          07 施開年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005220       05 施術終了年月日Ｗ.
005230          07 施術終了和暦Ｗ            PIC 9(1)  VALUE ZERO.
005240          07 施術終了年Ｗ              PIC 9(2)  VALUE ZERO.
005250          07 施術終了月Ｗ              PIC 9(2)  VALUE ZERO.
005260          07 施術終了日Ｗ              PIC 9(2)  VALUE ZERO.
005270          07 施終年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005280       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
005290       05 転帰Ｗ                       PIC N(4)  VALUE SPACE.
005300       05 転帰チェックＷ.
005310          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
005320          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
005330          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
005340       05 施術累計回数Ｗ               PIC 9(4)  VALUE ZERO.
005350*
005360******************
005370* 負担率チェック *
005380******************
005390 01 負担率チェックＷ.
005400    03 ０割チェックＷ                  PIC N(1)  VALUE SPACE.
005410    03 １割チェックＷ                  PIC N(1)  VALUE SPACE.
005420    03 ２割チェックＷ                  PIC N(1)  VALUE SPACE.
005430    03 ３割チェックＷ                  PIC N(1)  VALUE SPACE.
005440**********
005450* 合計行 *
005460**********
005470 01 合計行Ｗ.
005480    03 請求終了日Ｗ                    PIC 9(2) VALUE ZERO.
005490    03 対象西暦Ｗ                      PIC 9(4) VALUE ZERO.
005500    03 商Ｗ                            PIC 9(3) VALUE ZERO.
005510    03 余Ｗ                            PIC 9(3) VALUE ZERO.
005520*
005530****************************
005540* 詳細時の負傷原因印刷判定 *
005550****************************
005560 01 負傷原因転記Ｆ                     PIC X(3) VALUE SPACE.
005570
005580
005590****************
005600* 詳細用ワーク *
005610****************
005620* 連印－負傷印刷行用ワーク
005630 01 印刷行ＳＷ                           PIC 9(1) VALUE ZERO.
005640*
005650 01 負傷原因情報Ｗ.
005660    03 負傷原因コードＷ  OCCURS   9.
005670       09 負傷原因患者番号Ｗ           PIC 9(6) VALUE ZERO.
005680       09 負傷原因連番Ｗ               PIC 9(4) VALUE ZERO.
005690*
005700 01 負傷原因情報ＳＷ.
005710    03 負傷原因コードＳＷ  OCCURS   9.
005720       09 負傷患者番号ＳＷ               PIC 9(6) VALUE ZERO.
005730       09 負傷連番ＳＷ                   PIC 9(4) VALUE ZERO.
005740*
005750 01 負傷情報ＳＷ.
005760    03 負傷データ情報ＳＷ  OCCURS   9.
005770       05 会部位コードＳＷ               PIC X(4)  VALUE SPACE.
005780       05 部位ＣＮＴＳＷ                 PIC 9(1)  VALUE ZERO.
005790       05 負傷種別ＳＷ                   PIC 9(2)  VALUE ZERO.
005800       05 部位ＳＷ                       PIC 9(2)  VALUE ZERO.
005810       05 左右区分ＳＷ                   PIC 9(1)  VALUE ZERO.
005820       05 負傷位置番号ＳＷ               PIC 9(2)  VALUE ZERO.
005830       05 負傷名ＳＷ                     PIC N(18) VALUE SPACE.
005840       05 負傷年月日ＳＷ.
005850          07 負傷和暦ＳＷ                PIC 9(1)  VALUE ZERO.
005860          07 負傷年ＳＷ                  PIC 9(2)  VALUE ZERO.
005870          07 負傷月ＳＷ                  PIC 9(2)  VALUE ZERO.
005880          07 負傷日ＳＷ                  PIC 9(2)  VALUE ZERO.
005890          07 負傷年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
005900       05 施術開始年月日ＳＷ.
005910          07 施術開始和暦ＳＷ            PIC 9(1)  VALUE ZERO.
005920          07 施術開始年ＳＷ              PIC 9(2)  VALUE ZERO.
005930          07 施術開始月ＳＷ              PIC 9(2)  VALUE ZERO.
005940          07 施術開始日ＳＷ              PIC 9(2)  VALUE ZERO.
005950          07 施開年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
005960       05 施術終了年月日ＳＷ.
005970          07 施術終了和暦ＳＷ            PIC 9(1)  VALUE ZERO.
005980          07 施術終了年ＳＷ              PIC 9(2)  VALUE ZERO.
005990          07 施術終了月ＳＷ              PIC 9(2)  VALUE ZERO.
006000          07 施術終了日ＳＷ              PIC 9(2)  VALUE ZERO.
006010          07 施終年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
006020       05 開始年月日取得フラグＳ         PIC X(3)  VALUE SPACE.
006030       05 転帰ＳＷ                       PIC N(4)  VALUE SPACE.
006040       05 転帰チェックＳＷ.
006050          07 治癒チェックＳＷ            PIC N(1)  VALUE SPACE.
006060          07 中止チェックＳＷ            PIC N(1)  VALUE SPACE.
006070          07 転医チェックＳＷ            PIC N(1)  VALUE SPACE.
006080       05 施術日数ＳＷ                   PIC 9(4)  VALUE ZERO.
006090*/負傷原因を部位毎の欄に印刷するように変更0302
006100       05 負傷原因ＳＷ.
006110          07 負傷原因１ＳＷ              PIC X(30) VALUE SPACE.
006120          07 負傷原因２ＳＷ              PIC X(30) VALUE SPACE.
006130          07 負傷原因３ＳＷ              PIC X(30) VALUE SPACE.
006140          07 負傷原因４ＳＷ              PIC X(30) VALUE SPACE.
006150          07 負傷原因５ＳＷ              PIC X(30) VALUE SPACE.
006160          07 負傷原因６ＳＷ              PIC X(30) VALUE SPACE.
006170          07 負傷原因７ＳＷ              PIC X(30) VALUE SPACE.
006180          07 負傷原因８ＳＷ              PIC X(30) VALUE SPACE.
006190          07 負傷原因９ＳＷ              PIC X(30) VALUE SPACE.
006200          07 負傷原因１０ＳＷ            PIC X(30) VALUE SPACE.
006200          07 負傷原因１１ＳＷ            PIC X(30) VALUE SPACE.
006210*
006220*/年月毎持つ保険者データのワーク
006230 01 保負担率区分Ｗ                     PIC 9    VALUE ZERO.
006240 01 保本人負担率Ｗ                     PIC 9(3) VALUE ZERO.
006250 01 保家族負担率Ｗ                     PIC 9(3) VALUE ZERO.
006260 01 保本人負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
006270 01 保家族負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
006280*
006290 01 比較和暦年月Ｗ.
006300    03 比較和暦Ｗ                      PIC 9    VALUE ZERO.
006310    03 比較年Ｗ                        PIC 9(2) VALUE ZERO.
006320    03 比較月Ｗ                        PIC 9(2) VALUE ZERO.
006330*
006340*/老人保険改正の対応0101
006350 01 老人負担計算区分Ｗ                 PIC 9    VALUE ZERO.
006360 01 負担率用種別Ｗ                     PIC 9(2) VALUE ZERO.
006370 01 助成負担区分Ｗ                     PIC 9(2) VALUE ZERO.
006380*
006390** 助成負担率用(14/10～)
006400 01 助成負担率ＷＷ.
006410    03 助成負担率Ｗ                    PIC 9(3) VALUE ZERO.
006420    03 助成負担率Ｗ１                  PIC 9    VALUE ZERO.
006430    03 助成負担率文字Ｗ１              PIC X    VALUE SPACE.
006440    03 助成負担率表示Ｗ                PIC X(6) VALUE SPACE.
006450*
006460********************************************************************
006470 01 印刷制御.
006480     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
006490     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
006500     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
006510     03 拡張制御Ｐ.
006520         05 端末制御Ｐ.
006530             07 移動方向Ｐ             PIC X(1)  VALUE SPACE.
006540             07 移動行数Ｐ             PIC 9(3)  VALUE ZERO.
006550         05 詳細制御Ｐ                 PIC X(2)  VALUE SPACE.
006560     03 通知情報Ｐ                     PIC X(2)  VALUE SPACE.
006570     03 ユニット名Ｐ                   PIC X(8)  VALUE SPACE.
006580*
006590 01 計算機西暦年Ｗ                     PIC 9(2)  VALUE ZERO.
006600* 日付ＷＯＲＫ
006610 01 和暦終了年Ｗ                       PIC 9(4)  VALUE ZERO.
006620 01 計算機西暦.
006630    03 計算機西暦年                    PIC 9(4)  VALUE ZERO.
006640    03 計算機西暦月日                  PIC 9(4)  VALUE ZERO.
006650 01 計算機西暦Ｒ REDEFINES 計算機西暦.
006660    03 計算機世紀                      PIC 9(2).
006670    03 計算機日付                      PIC 9(6).
006680    03 計算機日付Ｒ REDEFINES 計算機日付.
006690       05 計算機年月                   PIC 9(4).
006700       05 計算機年月Ｒ REDEFINES 計算機年月.
006710         07 計算機年                   PIC 9(2).
006720         07 計算機月                   PIC 9(2).
006730       05 計算機日                     PIC 9(2).
006740*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
006750******************************************************************
006760*                          連結項目                              *
006770******************************************************************
006780*
006790**********************
006800* メッセージ表示キー *
006810**********************
006820 01 連メ－キー IS EXTERNAL.
006830    03  連メ－メッセージ                 PIC N(20).
006840*
006850****************
006860* 連結項目待避 *
006870****************
006880*    ************
006890*    * 料金情報 *
006900*    ************
006910*    月毎の料金
006920***********************
006930 01 連施－料金１ IS EXTERNAL.
006940   03 連施－本人負担率                 PIC 9(3).
006950************
006960* 印刷キー *
006970************
006980 01 連印－対象データ IS EXTERNAL.
006990    03 連印－施術和暦年月.
007000       05 連印－施術和暦                  PIC 9(1).
007010       05 連印－施術年                    PIC 9(2).
007020       05 連印－施術月                    PIC 9(2).
007030    03 連印－保険種別                     PIC 9(2).
007040    03 連印－保険者番号                   PIC X(10).
007050    03 連印－本人家族区分                 PIC 9(1).
007060    03 連印－被保険者カナ                 PIC X(20).
007070    03 連印－患者コード.
007080       05 連印－患者番号                  PIC 9(6).
007090       05 連印－枝番                      PIC X(1).
007100    03 連印－印刷モードＦ                 PIC 9(1).
007160*/詳細モード用
007170    03 連印－保険証印刷区分               PIC 9(1).
007200    03 連印－負傷詳細 OCCURS 7.
007210       05 連印－負傷印刷行                PIC 9(1).
007220       05 連印－部位印刷区分              PIC 9(1).
007230       05 連印－転帰印刷区分              PIC 9(1).
007240       05 連印－原因印刷区分              PIC 9(1).
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
007660*
       01 連入－表示フラグ６６０ IS EXTERNAL GLOBAL.
          03 連入－プレビュー区分               PIC 9(1).
007670* 負担率取得用14/10～
007680 01 連率－負担率取得キー IS EXTERNAL.
007690    03 連率－施術和暦年月.
007700       05 連率－施術和暦               PIC 9.
007710       05 連率－施術年月.
007720          07 連率－施術年              PIC 9(2).
007730          07 連率－施術月              PIC 9(2).
007740    03 連率－患者コード.
007750       05 連率－患者番号               PIC 9(6).
007760       05 連率－枝番                   PIC X.
007770    03 連率－実際負担率                PIC 9(3).
007780    03 連率－実際本体負担率            PIC 9(3).
007790    03 連率－健保負担率                PIC 9(3).
007800    03 連率－２７老負担率              PIC 9(3).
007810    03 連率－助成負担率                PIC 9(3).
007820    03 連率－特別用負担率              PIC 9(3).
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
007830*
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
007840******************************************************************
007850*                      PROCEDURE  DIVISION                       *
007860******************************************************************
007870 PROCEDURE               DIVISION.
007880************
007890*           *
007900* 初期処理   *
007910*           *
007920************
002570     PERFORM プリンタファイル作成.
007930     PERFORM 初期化.
007940************
007950*           *
007960* 主処理     *
007970*           *
007980************
007990* 印刷
008000     PERFORM 連結項目待避.
008010     PERFORM 印刷セット
008020     PERFORM 印刷処理
008030************
008040*           *
008050* 終了処理   *
008060*           *
008070************
008080     PERFORM 終了処理.
008090     EXIT PROGRAM.
008100*
008110*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YCH6621"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
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
008120*================================================================*
008130 初期化 SECTION.
008140*
008150     PERFORM ファイルオープン.
008160*    /* 現在日付取得 */
008170     ACCEPT 計算機日付 FROM DATE.
008180*    /* 1980～2079年の間で設定 */
008190     IF 計算機年 > 80
008200         MOVE 19 TO 計算機世紀
008210     ELSE
008220         MOVE 20 TO 計算機世紀
008230     END-IF.
008240     PERFORM カレント元号取得.
008250     PERFORM 和暦終了年取得.
008260     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
008270*================================================================*
008280 カレント元号取得 SECTION.
008290*
008300     MOVE ZEROS TO 制－制御区分.
008310     READ 制御情報マスタ
008320     NOT INVALID KEY
008330         MOVE 制－カレント元号           TO カレント元号Ｗ
008340         MOVE 制－カルテ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
008350         MOVE 制－バーコード２３２Ｃ使用区分  TO バーコード使用区分Ｗ
008360     END-READ.
008290*
008300     MOVE 01 TO 制－制御区分.
008310     READ 制御情報マスタ
008320     NOT INVALID KEY
               MOVE 制０１－用紙１   TO 用紙Ｗ
008360     END-READ.
008370*
008380*================================================================*
008390 和暦終了年取得 SECTION.
008400*
008410     MOVE カレント元号Ｗ TO 元－元号区分.
008420     READ 元号マスタ
008430     INVALID KEY
008440         DISPLAY NC"指定和暦が登録されていません" UPON CONS
008450         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008460                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
008470         ACCEPT  キー入力 FROM CONS
008480         PERFORM 終了処理
008490         EXIT PROGRAM
008500     NOT INVALID KEY
008510         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
008520         MOVE 前和暦Ｗ TO 元－元号区分
008530         READ 元号マスタ
008540         INVALID KEY
008550             DISPLAY NC"指定和暦が登録されていません" UPON CONS
008560             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008570                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
008580             ACCEPT  キー入力 FROM CONS
008590             PERFORM 終了処理
008600             EXIT PROGRAM
008610         NOT INVALID KEY
008620             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
008630         END-READ
008640     END-READ.
008650*
008660*================================================================*
008670 連結項目待避 SECTION.
008680*
008690     INITIALIZE 対象データＷＲ.
008700     MOVE 連印－施術和暦      TO 施術和暦ＷＲ.
008710     MOVE 連印－施術年        TO 施術年ＷＲ.
008720     MOVE 連印－施術月        TO 施術月ＷＲ.
008730     MOVE 連印－保険種別      TO 保険種別ＷＲ.
008740     MOVE 連印－保険者番号    TO 保険者番号ＷＲ.
008750     MOVE 連印－本人家族区分  TO 本人家族区分ＷＲ.
008760     MOVE 連印－被保険者カナ  TO 被保険者カナＷＲ.
008770     MOVE 連印－患者番号      TO 患者番号ＷＲ.
008780     MOVE 連印－枝番          TO 枝番ＷＲ.
008790     MOVE 連印－印刷モードＦ  TO 印刷モードＦＷＲ.
008800     MOVE 連印－提出年        TO 提出年ＷＲ.
008810     MOVE 連印－提出月        TO 提出月ＷＲ.
008820     MOVE 連印－提出日        TO 提出日ＷＲ.
008830     MOVE 連印－段数          TO 段数ＷＲ.
008840*================================================================*
008850 ファイルオープン SECTION.
008860*
008870     OPEN INPUT   保険者マスタ
008880         MOVE NC"保険者" TO ファイル名.
008890         PERFORM オープンチェック.
008900     OPEN INPUT   元号マスタ
008910         MOVE NC"元号" TO ファイル名.
008920         PERFORM オープンチェック.
008930     OPEN INPUT   名称マスタ
008940         MOVE NC"名称" TO ファイル名.
008950         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
008990     OPEN INPUT   制御情報マスタ
009000         MOVE NC"制御情報" TO ファイル名.
009010         PERFORM オープンチェック.
009050     OPEN INPUT   請求先マスタ
009060         MOVE NC"請先" TO ファイル名.
009070         PERFORM オープンチェック.
009110     OPEN INPUT   受診者情報Ｆ.
009120         MOVE NC"受情" TO ファイル名.
009130         PERFORM オープンチェック.
009140     OPEN INPUT   施術記録Ｆ.
009150         MOVE NC"施記Ｆ" TO ファイル名.
009160         PERFORM オープンチェック.
009170     OPEN INPUT   負傷データＦ.
009180         MOVE NC"負傷" TO ファイル名.
009190         PERFORM オープンチェック.
009230     OPEN INPUT   負傷原因Ｆ.
009240         MOVE NC"負傷原因" TO ファイル名.
009250         PERFORM オープンチェック.
009260     OPEN INPUT 市町村マスタ.
009270         MOVE NC"市町村" TO ファイル名.
009280         PERFORM オープンチェック.
006630     OPEN INPUT 生保情報Ｆ.
006640         MOVE NC"生保" TO ファイル名.
006650         PERFORM オープンチェック.
009290     OPEN INPUT 自賠責情報Ｆ.
009300         MOVE NC"自賠" TO ファイル名.
009310         PERFORM オープンチェック.
009290     OPEN INPUT 保険会社マスタ.
009300         MOVE NC"保会" TO ファイル名.
009310         PERFORM オープンチェック.
007900     OPEN INPUT 事業所マスタ.
007910         MOVE NC"事業所" TO ファイル名.
007920         PERFORM オープンチェック.
009330     OPEN I-O   印刷ファイル
009340         MOVE NC"印刷" TO ファイル名.
009350         PERFORM エラー処理Ｐ.
009360*================================================================*
009370 オープンチェック SECTION.
009380*
009390     IF 状態キー  NOT =  "00"
009400         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
009410         DISPLAY NC"状態キー：" 状態キー         UPON CONS
009420         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
009430                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
009440         ACCEPT  キー入力 FROM CONS
009450         PERFORM ファイル閉鎖
009460         EXIT PROGRAM.
009470*================================================================*
009480 ファイル閉鎖 SECTION.
009490*
009500     CLOSE 印刷ファイル   保険者マスタ 元号マスタ 
009510           制御情報マスタ 名称マスタ   請求先マスタ
009520           受診者情報Ｆ   レセプトＦ   施術記録Ｆ
009530           負傷データＦ   負傷原因Ｆ   生保情報Ｆ
009540           市町村マスタ   事業所マスタ 自賠責情報Ｆ
                 保険会社マスタ.
009550*================================================================*
009560 終了処理 SECTION.
009570*
009580     PERFORM ファイル閉鎖.
009590*================================================================*
009600 印刷セット SECTION.
009610*
009620     EVALUATE 印刷モードＦＷＲ
009630     WHEN 0
009640         PERFORM 印刷セット１
009650     WHEN 1
009660         PERFORM 印刷セット２
009670     WHEN 2
009680         PERFORM 印刷セット３
009690     WHEN 3
009700         PERFORM 印刷セット４
009710*/詳細モード用
009720     WHEN 4
009730         PERFORM 印刷セット５
009740     END-EVALUATE.
009750*
009760*================================================================*
009770 印刷セット１ SECTION.
009780*
009790     INITIALIZE YCH6621P.
009800     MOVE "X" TO EDIT-MODE OF 患者コード.
009810     INITIALIZE 受診者情報Ｗ.
009820     INITIALIZE 事業所情報Ｗ.
009830     INITIALIZE 請求先情報Ｗ.
009840     INITIALIZE 負傷情報Ｗ.
009850     INITIALIZE 負担率チェックＷ.
009860     PERFORM 負担率取得.
009880     PERFORM 受診者情報取得.
009870     PERFORM 請求先情報取得.
009890     PERFORM 負傷データ取得.
009900*
009910     IF 負傷原因印刷区分Ｗ  NOT = 1 
009920        PERFORM 負傷原因取得
009930     END-IF.
009940*
009950     PERFORM 印刷上部セット.
009960     PERFORM 印刷下部セット.
009970*================================================================*
009980 印刷セット２ SECTION.
009990*
010000     INITIALIZE YCH6621P.
010010     MOVE "X" TO EDIT-MODE OF 患者コード.
010020     INITIALIZE 受診者情報Ｗ.
010030     INITIALIZE 事業所情報Ｗ.
010040     INITIALIZE 請求先情報Ｗ.
010050     INITIALIZE 負傷情報Ｗ.
010060     INITIALIZE 負担率チェックＷ.
010070     PERFORM 負担率取得.
010090     PERFORM 受診者情報取得.
010080     PERFORM 請求先情報取得.
010100     PERFORM 印刷上部セット.
010110*================================================================*
010120 印刷セット３ SECTION.
010130*
010140     INITIALIZE YCH6621P.
010150     MOVE "X" TO EDIT-MODE OF 患者コード.
010160     INITIALIZE 受診者情報Ｗ.
010170     INITIALIZE 事業所情報Ｗ.
010180     INITIALIZE 請求先情報Ｗ.
010190     INITIALIZE 負傷情報Ｗ.
010090     PERFORM 受診者情報取得.
010200     PERFORM 負傷データ取得.
010210*
010220     IF 負傷原因印刷区分Ｗ  NOT = 1 
010230        PERFORM 負傷原因取得
010240     END-IF.
010250*
010260     PERFORM 印刷下部セット.
010270*================================================================*
010280 印刷セット４ SECTION.
010290*
010300     INITIALIZE YCH6621P.
010310     MOVE "X" TO EDIT-MODE OF 患者コード.
010320     INITIALIZE 受診者情報Ｗ.
010330     INITIALIZE 事業所情報Ｗ.
010340     INITIALIZE 請求先情報Ｗ.
010350     INITIALIZE 負傷情報Ｗ.
010360     INITIALIZE 合計行Ｗ.
010090     PERFORM 受診者情報取得.
010400     PERFORM 請求期間取得.
010410     PERFORM 合計金額行セット.
010420***********************************************************>HILO
010430*     DISPLAY "患者コード" 連印－患者コード    UPON CONS. *>HILO
010440*     DISPLAY "費用額    " 連計－費用額        UPON CONS. *>HILO
010450*     DISPLAY "負担額    " 連計－負担額        UPON CONS. *>HILO
010460*     DISPLAY "請求額    " 連計－請求額        UPON CONS. *>HILO
010470*     DISPLAY "費用額老人" 連計－費用額老人    UPON CONS. *>HILO
010480*     DISPLAY "負担額老人" 連計－負担額老人    UPON CONS. *>HILO
010490*     DISPLAY "請求額老人" 連計－請求額老人    UPON CONS. *>HILO
010500*     DISPLAY "費用額助成" 連計－費用額助成    UPON CONS. *>HILO
010510*     DISPLAY "負担額助成" 連計－負担額助成    UPON CONS. *>HILO
010520*     DISPLAY "請求額助成" 連計－請求額助成    UPON CONS. *>HILO
010530*     DISPLAY "*"                        UPON CONS.       *>HILO
010540*     DISPLAY "費用額"  費用額(段数ＷＲ) UPON CONS.       *>HILO
010550*     DISPLAY "負担額"  負担額(段数ＷＲ) UPON CONS.       *>HILO
010560*     DISPLAY "請求額"  請求額(段数ＷＲ) UPON CONS.       *>HILO
010570***********************************************************>HILO
010580*================================================================*
010590 印刷上部セット SECTION.
010600*
010610     IF バーコード使用区分Ｗ NOT = ZERO
010620        MOVE 患者コードＷ     TO 患者コード
010630        MOVE SPACE            TO EDIT-MODE OF 患者コード
010640     END-IF.
010650*
010660     MOVE 患者番号Ｗ        TO 患者番号.
010670     MOVE 枝番Ｗ            TO 枝番.
010680**************************
010690* 被保険者証情報セット   *
010700**************************
010710     IF 記号Ｗ(1:2) = "＊" 
010720        MOVE  SPACE    TO  記号
010730     ELSE
010740        MOVE 記号Ｗ    TO  記号
010750     END-IF.
010760     IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
010770        ( 印刷番号Ｗ(1:2) = "＊" )
010780        MOVE  SPACE      TO  番号
010790     ELSE
010800        MOVE 印刷番号Ｗ  TO  番号
010810     END-IF.
010820************************
010830* 被保険者情報セット   *
010840************************
010850     MOVE 被保険者氏名Ｗ             TO 被保険者氏名.
010860     MOVE 被保険者カナＷ             TO 被保険者カナ.
010870*     MOVE 資格取得元号Ｗ             TO 資格取得元号.
010880     MOVE 資格取得年Ｗ               TO 資格取得年.
010890     MOVE 資格取得月Ｗ               TO 資格取得月.
010900     MOVE 資格取得日Ｗ               TO 資格取得日.
           IF 用紙Ｗ = 2
010970        IF ( 資格取得年Ｗ NOT = ZERO ) OR
010980           ( 資格取得月Ｗ NOT = ZERO ) OR
010990           ( 資格取得日Ｗ NOT = ZERO )
                 MOVE 資格取得元号Ｗ          TO 資格元号
              END-IF
           ELSE
010910        MOVE 資格昭和チェックＷ         TO 資格昭和チェック
010920        MOVE 資格平成チェックＷ         TO 資格平成チェック
              IF 資格令和チェックＷ NOT = SPACE
                 IF 用紙Ｗ = ZERO
                    MOVE NC"令和"             TO 資格令和ＣＭ
                    MOVE 資格令和チェックＷ   TO 資格令和チェック
                 ELSE
                    MOVE NC"令和"             TO 資格元号
                 END-IF
              END-IF
           END-IF.
010930*/有効期限
010940     MOVE 有効期限年Ｗ               TO 有効年.
010950     MOVE 有効期限月Ｗ               TO 有効月.
010960     MOVE 有効期限日Ｗ               TO 有効日.
010970     IF ( 有効期限年Ｗ = ZERO ) AND
010980        ( 有効期限月Ｗ = ZERO ) AND
010990        ( 有効期限日Ｗ = ZERO )
011000         MOVE SPACE          TO 有効元号
011010         MOVE SPACE          TO 有効年月日表示
011020         MOVE SPACE          TO 有効年表示
011030         MOVE SPACE          TO 有効月表示
011040         MOVE SPACE          TO 有効日表示
011050     ELSE
011060         MOVE 有効期限元号Ｗ TO 有効元号
011070         MOVE NC"有効年月日" TO 有効年月日表示
011080         MOVE NC"年"         TO 有効年表示
011090         MOVE NC"月"         TO 有効月表示
011100         MOVE NC"日"         TO 有効日表示
011110     END-IF.
011120     MOVE 被保険者明治チェックＷ     TO 被保険者明治チェック.
011130     MOVE 被保険者大正チェックＷ     TO 被保険者大正チェック.
011140     MOVE 被保険者昭和チェックＷ     TO 被保険者昭和チェック.
011150     MOVE 被保険者平成チェックＷ     TO 被保険者平成チェック.
           IF 被保険者令和チェックＷ NOT = SPACE
              IF 用紙Ｗ = ZERO
                 MOVE NC"令和"             TO 被保険者令和ＣＭ
              END-IF
              MOVE 被保険者令和チェックＷ  TO 被保険者令和チェック
           END-IF.
011160     MOVE 被保険者年Ｗ               TO 被保険者年.
011170     MOVE 被保険者月Ｗ               TO 被保険者月.
011180     MOVE 被保険者日Ｗ               TO 被保険者日.
011190     MOVE 被保険者年齢Ｗ             TO 被保険者年齢.
011200********************
011210* 患者情報セット   *
011220********************
011230     MOVE 印刷負担率Ｗ               TO 負担割合.
011240*
011250* 14/10～ (助成の負担率）
011260     MOVE 助成負担率表示Ｗ           TO 助成負担率.
011270*
011280*     MOVE ０割チェックＷ             TO ０割チェック.
011290*     MOVE １割チェックＷ             TO １割チェック.
011300*     MOVE ２割チェックＷ             TO ２割チェック.
011310*     MOVE ３割チェックＷ             TO ３割チェック.
011320     IF  印刷市町村番号Ｗ(1:2) = "99"
011330         MOVE SPACE                  TO 市町村番号
011340     ELSE
011350         MOVE 印刷市町村番号Ｗ       TO 市町村番号
011360     END-IF.
011370     IF (印刷受益者番号Ｗ(1:1) = "*") OR
011380        (印刷受益者番号Ｗ(1:2) = "＊") 
011390         MOVE SPACE                  TO 受益者番号
011400     ELSE
011410         MOVE 印刷受益者番号Ｗ       TO 受益者番号
011420     END-IF
011430*
011440     IF  印刷費用負担者番号Ｗ(1:2) = "99"
011450         MOVE SPACE                  TO 公費負担者番号
011460     ELSE
011470         MOVE 印刷費用負担者番号Ｗ   TO 公費負担者番号
011480     END-IF.
011490*
011500     IF (印刷受益者番号助成Ｗ(1:1) = "*") OR
011510        (印刷受益者番号助成Ｗ(1:2) = "＊")
011520         MOVE SPACE                  TO 受益者番号助成
011530     ELSE
      */1410
               IF 印刷受益者番号助成Ｗ(11:1) = SPACE
011450            MOVE 印刷受益者番号助成Ｗ TO 受益者番号助成
               ELSE
011450            MOVE 印刷受益者番号助成Ｗ TO 受益者番号助成２
               END-IF
011550     END-IF.
011560     MOVE 患者カナＷ                 TO 患者カナ.
011570     MOVE 患者氏名Ｗ                 TO 患者氏名.
011580*     MOVE 患者性別Ｗ                 TO 患者性別.
011590     MOVE 被保険者男チェックＷ       TO 被保険者男チェック.
011600     MOVE 被保険者女チェックＷ       TO 被保険者女チェック.
011610     MOVE 被保険者郵便番号１Ｗ       TO 被保険者郵便番号１.
011620     MOVE 被保険者郵便番号２Ｗ       TO 被保険者郵便番号２.
011630     MOVE "-"                        TO 被保険者郵便区切.
011640     MOVE 被保険者住所Ｗ             TO 被保険者住所.
011650     MOVE 被保険者電話番号Ｗ         TO 被保険者電話番号.
011660     MOVE 患者男チェックＷ           TO 患者男チェック.
011670     MOVE 患者女チェックＷ           TO 患者女チェック.
011680     MOVE 明治チェックＷ             TO 明治チェック.
011690     MOVE 大正チェックＷ             TO 大正チェック.
011700     MOVE 昭和チェックＷ             TO 昭和チェック.
011710     MOVE 平成チェックＷ             TO 平成チェック.
           IF 令和チェックＷ NOT = SPACE
              IF 用紙Ｗ = ZERO
                 MOVE NC"令和"             TO 令和ＣＭ
              END-IF
              MOVE 令和チェックＷ          TO 令和チェック
           END-IF.
011720     MOVE 患者年Ｗ                   TO 患者年.
011730     MOVE 患者月Ｗ                   TO 患者月.
011740     MOVE 患者日Ｗ                   TO 患者日.
011750     MOVE 患者年齢Ｗ                 TO 患者年齢.
011760     MOVE 印刷続柄Ｗ                 TO 続柄.
011770     MOVE 患者郵便番号１Ｗ           TO 患者郵便番号１.
011780     MOVE 患者郵便番号２Ｗ           TO 患者郵便番号２.
011790     MOVE "-"                        TO 患者郵便区切.
011800     MOVE 患者住所Ｗ                 TO 患者住所.
011810     MOVE 患者電話番号Ｗ             TO 患者電話番号.
011820*     MOVE 患者住所１Ｗ               TO 患者住所１.
011830*     MOVE 患者住所２Ｗ               TO 患者住所２.
011840********************
011850* 事業所情報セット *
011860********************
011870     MOVE 事業所名称Ｗ               TO 事業所名称.
011880     MOVE 事業所郵便番号１Ｗ         TO 事業所郵便番号１.
011890     MOVE 事業所郵便番号２Ｗ         TO 事業所郵便番号２.
011900     MOVE 印刷事業所住所Ｗ           TO 事業所住所.
011900     MOVE 印刷事業所住所２Ｗ         TO 事業所住所２.
011910************************************
011920* 都道府県・健康保険組合情報セット *
011930************************************
011940*労災、自費の時は保険者情報は印字しない
011950     IF 保険種別ＷＲ = 70 OR 90
011960         MOVE SPACE                  TO 保険者番号
011970         MOVE SPACE                  TO 請求先名支部名
011980     ELSE
011990         MOVE 印刷保険者番号Ｗ       TO 保険者番号
012000         MOVE 印刷請求先名支部名Ｗ   TO 請求先名支部名
012010     END-IF.
012020**************************
012030* 保険種別チェックセット *
012040**************************
012050     MOVE 社保チェックＷ             TO 社保チェック.
012060     MOVE 組合チェックＷ             TO 組合チェック.
012070     MOVE 日雇チェックＷ             TO 日雇チェック.
012080     MOVE 船員チェックＷ             TO 船員チェック.
012090     MOVE 共済チェックＷ             TO 共済チェック.
012100     MOVE 国保チェックＷ             TO 国保チェック.
012110     MOVE 退職チェックＷ             TO 退職チェック.
012120     MOVE 老人チェックＷ             TO 老人チェック.
012130     MOVE 乳幼児チェックＷ           TO 乳幼児チェック.
012140     MOVE 自賠チェックＷ             TO 自賠チェック.
012150     MOVE 生保チェックＷ             TO 生保チェック.
012160     MOVE 福チェックＷ               TO 福チェック.
012170     MOVE 身障チェックＷ             TO 身障チェック.
012180     MOVE 母子チェックＷ             TO 母子チェック.
012190     MOVE 生保チェックＷ             TO 生保チェック.
012200     MOVE 原爆チェックＷ             TO 原爆チェック.
012210*================================================================*
012220 印刷下部セット SECTION.
012230*
012240********************
012250* 負傷データセット *
012260********************
012270* １部位 *
012280**********
012290*     MOVE 会部位コードＷ(1)   TO 負傷コード１.
012300     MOVE 負傷名Ｗ(1)         TO 負傷名１.
           IF 負傷和暦Ｗ(1) = 5
              MOVE NC"令和"         TO 負傷和暦１
           END-IF.
012310     MOVE 負傷年Ｗ(1)         TO 負傷年１.
012320     MOVE 負傷月Ｗ(1)         TO 負傷月１.
012330     MOVE 負傷日Ｗ(1)         TO 負傷日１.
           IF 施術開始和暦Ｗ(1) = 5
              MOVE NC"令和"         TO 施術開始和暦１
           END-IF.
012340     MOVE 施術開始年Ｗ(1)     TO 施術開始年１.
012350     MOVE 施術開始月Ｗ(1)     TO 施術開始月１.
012360     MOVE 施術開始日Ｗ(1)     TO 施術開始日１.
           IF 施術開始和暦Ｗ(1) = 5
              MOVE NC"令和"         TO 施術期間自和暦１
           END-IF.
012370     MOVE 施術開始年Ｗ(1)     TO 施術期間自年１.
012380     MOVE 施術開始月Ｗ(1)     TO 施術期間自月１.
012390     MOVE 施術開始日Ｗ(1)     TO 施術期間自日１.
           IF 施術終了和暦Ｗ(1) = 5
              MOVE NC"令和"         TO 施術終了和暦１
           END-IF.
012400     MOVE 施術終了年Ｗ(1)     TO 施術終了年１.
012410     MOVE 施術終了月Ｗ(1)     TO 施術終了月１.
012420     MOVE 施術終了日Ｗ(1)     TO 施術終了日１.
012430*     MOVE 負傷年月日区切Ｗ(1) TO 区切１１ 区切１２.
012440*     MOVE 施開年月日区切Ｗ(1) TO 区切１３ 区切１４.
012450*     MOVE 施終年月日区切Ｗ(1) TO 区切１５ 区切１６.
012460*     MOVE 転帰Ｗ(1)           TO 転帰１.
012470     MOVE 治癒チェックＷ(1)   TO 治癒チェック１.
012480     MOVE 中止チェックＷ(1)   TO 中止チェック１.
012490     MOVE 転医チェックＷ(1)   TO 転医チェック１.
012500     MOVE 施術累計回数Ｗ(1)   TO 施術日数１.
012510*     ******************
012520*     * 日数は一時保留 *
012530*     ******************
012540*     **********************
012550*     * 施術回数は一時保留 *
012560*     **********************
012570*     **********************
012580*     * 転帰名称は一時保留 *
012590*     **********************
012600**********
012610* ２部位 *
012620**********
012630*     MOVE 会部位コードＷ(2)   TO 負傷コード２.
012640     MOVE 負傷名Ｗ(2)         TO 負傷名２.
           IF 負傷和暦Ｗ(2) = 5
              MOVE NC"令和"         TO 負傷和暦２
           END-IF.
012650     MOVE 負傷年Ｗ(2)         TO 負傷年２.
012660     MOVE 負傷月Ｗ(2)         TO 負傷月２.
012670     MOVE 負傷日Ｗ(2)         TO 負傷日２.
           IF 施術開始和暦Ｗ(2) = 5
              MOVE NC"令和"         TO 施術開始和暦２
           END-IF.
012680     MOVE 施術開始年Ｗ(2)     TO 施術開始年２.
012690     MOVE 施術開始月Ｗ(2)     TO 施術開始月２.
012700     MOVE 施術開始日Ｗ(2)     TO 施術開始日２.
           IF 施術開始和暦Ｗ(2) = 5
              MOVE NC"令和"         TO 施術期間自和暦２
           END-IF.
012710     MOVE 施術開始年Ｗ(2)     TO 施術期間自年２.
012720     MOVE 施術開始月Ｗ(2)     TO 施術期間自月２.
012730     MOVE 施術開始日Ｗ(2)     TO 施術期間自日２.
           IF 施術終了和暦Ｗ(2) = 5
              MOVE NC"令和"         TO 施術終了和暦２
           END-IF.
012740     MOVE 施術終了年Ｗ(2)     TO 施術終了年２.
012750     MOVE 施術終了月Ｗ(2)     TO 施術終了月２.
012760     MOVE 施術終了日Ｗ(2)     TO 施術終了日２.
012770*     MOVE 負傷年月日区切Ｗ(2) TO 区切２１ 区切２２.
012780*     MOVE 施開年月日区切Ｗ(2) TO 区切２３ 区切２４.
012790*     MOVE 施終年月日区切Ｗ(2) TO 区切２５ 区切２６.
012800*     MOVE 転帰Ｗ(2)           TO 転帰２.
012810     MOVE 治癒チェックＷ(2)   TO 治癒チェック２.
012820     MOVE 中止チェックＷ(2)   TO 中止チェック２.
012830     MOVE 転医チェックＷ(2)   TO 転医チェック２.
012840     MOVE 施術累計回数Ｗ(2)   TO 施術日数２.
012850*     ******************
012860*     * 日数は一時保留 *
012870*     ******************
012880*     **********************
012890*     * 施術回数は一時保留 *
012900*     **********************
012910*     **********************
012920*     * 転帰名称は一時保留 *
012930*     **********************
012940**********
012950* ３部位 *
012960**********
012970*     MOVE 会部位コードＷ(3)   TO 負傷コード３.
012980     MOVE 負傷名Ｗ(3)         TO 負傷名３.
           IF 負傷和暦Ｗ(3) = 5
              MOVE NC"令和"         TO 負傷和暦３
           END-IF.
012990     MOVE 負傷年Ｗ(3)         TO 負傷年３.
013000     MOVE 負傷月Ｗ(3)         TO 負傷月３.
013010     MOVE 負傷日Ｗ(3)         TO 負傷日３.
           IF 施術開始和暦Ｗ(3) = 5
              MOVE NC"令和"         TO 施術開始和暦３
           END-IF.
013020     MOVE 施術開始年Ｗ(3)     TO 施術開始年３.
013030     MOVE 施術開始月Ｗ(3)     TO 施術開始月３.
013040     MOVE 施術開始日Ｗ(3)     TO 施術開始日３.
           IF 施術開始和暦Ｗ(3) = 5
              MOVE NC"令和"         TO 施術期間自和暦３
           END-IF.
013050     MOVE 施術開始年Ｗ(3)     TO 施術期間自年３.
013060     MOVE 施術開始月Ｗ(3)     TO 施術期間自月３.
013070     MOVE 施術開始日Ｗ(3)     TO 施術期間自日３.
           IF 施術終了和暦Ｗ(3) = 5
              MOVE NC"令和"         TO 施術終了和暦３
           END-IF.
013080     MOVE 施術終了年Ｗ(3)     TO 施術終了年３.
013090     MOVE 施術終了月Ｗ(3)     TO 施術終了月３.
013100     MOVE 施術終了日Ｗ(3)     TO 施術終了日３.
013110*     MOVE 負傷年月日区切Ｗ(3) TO 区切３１ 区切３２.
013120*     MOVE 施開年月日区切Ｗ(3) TO 区切３３ 区切３４.
013130*     MOVE 施終年月日区切Ｗ(3) TO 区切３５ 区切３６.
013140*     MOVE 転帰Ｗ(3)           TO 転帰３.
013150     MOVE 治癒チェックＷ(3)   TO 治癒チェック３.
013160     MOVE 中止チェックＷ(3)   TO 中止チェック３.
013170     MOVE 転医チェックＷ(3)   TO 転医チェック３.
013180     MOVE 施術累計回数Ｗ(3)   TO 施術日数３.
013190*     ******************
013200*     * 日数は一時保留 *
013210*     ******************
013220*     **********************
013230*     * 施術回数は一時保留 *
013240*     **********************
013250*     **********************
013260*     * 転帰名称は一時保留 *
013270*     **********************
013280**********
013290* ４部位 *
013300**********
013310*     MOVE 会部位コードＷ(4)   TO 負傷コード４.
013320     MOVE 負傷名Ｗ(4)         TO 負傷名４.
           IF 負傷和暦Ｗ(4) = 5
              MOVE NC"令和"         TO 負傷和暦４
           END-IF.
013330     MOVE 負傷年Ｗ(4)         TO 負傷年４.
013340     MOVE 負傷月Ｗ(4)         TO 負傷月４.
013350     MOVE 負傷日Ｗ(4)         TO 負傷日４.
           IF 施術開始和暦Ｗ(4) = 5
              MOVE NC"令和"         TO 施術開始和暦４
           END-IF.
013360     MOVE 施術開始年Ｗ(4)     TO 施術開始年４.
013370     MOVE 施術開始月Ｗ(4)     TO 施術開始月４.
013380     MOVE 施術開始日Ｗ(4)     TO 施術開始日４.
           IF 施術開始和暦Ｗ(4) = 5
              MOVE NC"令和"         TO 施術期間自和暦４
           END-IF.
013390     MOVE 施術開始年Ｗ(4)     TO 施術期間自年４.
013400     MOVE 施術開始月Ｗ(4)     TO 施術期間自月４.
013410     MOVE 施術開始日Ｗ(4)     TO 施術期間自日４.
           IF 施術終了和暦Ｗ(4) = 5
              MOVE NC"令和"         TO 施術終了和暦４
           END-IF.
013420     MOVE 施術終了年Ｗ(4)     TO 施術終了年４.
013430     MOVE 施術終了月Ｗ(4)     TO 施術終了月４.
013440     MOVE 施術終了日Ｗ(4)     TO 施術終了日４.
013450*     MOVE 負傷年月日区切Ｗ(4) TO 区切４１ 区切４２.
013460*     MOVE 施開年月日区切Ｗ(4) TO 区切４３ 区切４４.
013470*     MOVE 施終年月日区切Ｗ(4) TO 区切４５ 区切４６.
013480*     MOVE 転帰Ｗ(4)           TO 転帰４.
013490     MOVE 治癒チェックＷ(4)   TO 治癒チェック４.
013500     MOVE 中止チェックＷ(4)   TO 中止チェック４.
013510     MOVE 転医チェックＷ(4)   TO 転医チェック４.
013520     MOVE 施術累計回数Ｗ(4)   TO 施術日数４.
013530*     ******************
013540*     * 日数は一時保留 *
013550*     ******************
013560*     **********************
013570*     * 施術回数は一時保留 *
013580*     **********************
013590*     **********************
013600*     * 転帰名称は一時保留 *
013610*     **********************
013620**********
013630* ５部位 *
013640**********
013650*    MOVE 会部位コードＷ(5)   TO 負傷コード５.
013660*    MOVE 負傷名Ｗ(5)         TO 負傷名５.
013670*    MOVE 負傷年Ｗ(5)         TO 負傷年５.
013680*    MOVE 負傷月Ｗ(5)         TO 負傷月５.
013690*    MOVE 負傷日Ｗ(5)         TO 負傷日５.
013700*    MOVE 施術開始年Ｗ(5)     TO 施術開始年５.
013710*    MOVE 施術開始月Ｗ(5)     TO 施術開始月５.
013720*    MOVE 施術開始日Ｗ(5)     TO 施術開始日５.
013730*    MOVE 施術終了年Ｗ(5)     TO 施術終了年５.
013740*    MOVE 施術終了月Ｗ(5)     TO 施術終了月５.
013750*     MOVE 施術終了日Ｗ(5)     TO 施術終了日５.
013760*     MOVE 負傷年月日区切Ｗ(5) TO 区切５１ 区切５２.
013770*     MOVE 施開年月日区切Ｗ(5) TO 区切５３ 区切５４.
013780*     MOVE 施終年月日区切Ｗ(5) TO 区切５５ 区切５６.
013790*     MOVE 転帰Ｗ(5)           TO 転帰５.
013800*     MOVE 治癒チェックＷ(5)   TO 治癒チェック５.
013810*     MOVE 中止チェックＷ(5)   TO 中止チェック５.
013820*     MOVE 転医チェックＷ(5)   TO 転医チェック５.
013830*     ******************
013840*     * 日数は一時保留 *
013850*     ******************
013860*     **********************
013870*     * 施術回数は一時保留 *
013880*     **********************
013890*     **********************
013900*     * 転帰名称は一時保留 *
013910*     **********************
013920************
013930* 負傷原因 *
013940************
013950*-------------------------------------------------------------------*
013960* ４部位までを印字するため、５部位の負傷原因は印字しない            *
013970* 帳票定義体は１部位につき１１行の負傷原因が印字できるよう設定      *
013980*-------------------------------------------------------------------*
013990     MOVE  ZERO   TO  カウンタ.
014000     MOVE  ZERO   TO  カウンタ２
014010     PERFORM VARYING カウンタ FROM 1 BY 1
014020             UNTIL ( カウンタ > 44 ) OR ( 負傷原因Ｗ(カウンタ) = SPACE )
014040*
014050        IF ( 負傷原因Ｗ(カウンタ)(1:2) = "①" )
014060           MOVE  ZERO   TO  カウンタ２
014070        END-IF
014080        IF ( 負傷原因Ｗ(カウンタ)(1:2) = "②" )
014090           MOVE  11     TO  カウンタ２
014100        END-IF
014110        IF ( 負傷原因Ｗ(カウンタ)(1:2) = "③" )
014120           MOVE  22     TO  カウンタ２
014130        END-IF
014140        IF ( 負傷原因Ｗ(カウンタ)(1:2) = "④" )
014150           MOVE  33     TO  カウンタ２
014160        END-IF
014170*        IF ( 負傷原因Ｗ(カウンタ)(1:2 = "⑤" )
014180*           MOVE  44     TO  カウンタ２
014190*        END-IF
014200*
014210        COMPUTE  カウンタ２  =  カウンタ２  +  1
014220        MOVE 負傷原因Ｗ(カウンタ)  TO 負傷原因(カウンタ２)
014230*
014240     END-PERFORM.
014250*
014260*================================================================*
014270 負担率取得 SECTION.
014280*
014290     MOVE ZERO TO 本人負担率Ｗ.
014300     MOVE ZERO TO 家族負担率Ｗ.
014310*
014320     MOVE 施術和暦ＷＲ   TO 受－施術和暦.
014330     MOVE 施術年ＷＲ     TO 受－施術年.
014340     MOVE 施術月ＷＲ     TO 受－施術月.
014350     MOVE 患者番号ＷＲ   TO 受－患者番号.
014360     MOVE 枝番ＷＲ       TO 受－枝番.
014370     READ 受診者情報Ｆ
014380     INVALID KEY
014390         MOVE  NC"　　施術月の受診者情報がありません" TO 連メ－メッセージ
014400         CALL   "MSG001"
014410         CANCEL "MSG001"
014420         PERFORM ファイル閉鎖
014430         MOVE ZERO TO PROGRAM-STATUS
014440         EXIT PROGRAM
014450     NOT INVALID KEY
014460* 14/10～　サブルーチン処理
014470      IF 受－施術和暦年月 >= 41410
014480         PERFORM 負担率取得１４１０
015700         COMPUTE 印刷負担率Ｗ = 負担率Ｗ / 10
015660       END-IF
015670     END-READ.
015710*
015720*================================================================*
015730 負担率取得１４１０ SECTION.
015740*
015750* 受診者Ｆ READ中
015760* 平成14/10～
015770     MOVE ZERO  TO 負担率Ｗ.
015780     INITIALIZE 助成負担率ＷＷ.
015790*
015800     MOVE SPACE TO 連率－負担率取得キー.
015810     INITIALIZE 連率－負担率取得キー.
015820     MOVE 受－施術和暦年月 TO 連率－施術和暦年月.
015830     MOVE 受－患者コード   TO 連率－患者コード.
015840*
015850     CALL   "HUTANRIT".
015860     CANCEL "HUTANRIT".
015870*
015880* / 本体 /
015890     MOVE 連率－実際本体負担率 TO 負担率Ｗ.
015900*
015910*
015920* / 助成 /
015930     IF 受－助成種別 NOT = ZERO
015940        MOVE 連率－助成負担率  TO 助成負担率Ｗ
015950*
015960        COMPUTE 助成負担率Ｗ１ = 助成負担率Ｗ / 10
015970        MOVE 助成負担率Ｗ１ TO  助成負担率文字Ｗ１
015980*
015990        STRING "("                 DELIMITED BY SIZE
016000               助成負担率文字Ｗ１  DELIMITED BY SIZE
016010               "割)"               DELIMITED BY SIZE
016020               INTO 助成負担率表示Ｗ
016030        END-STRING
016040*
016050     END-IF.
016060*
016260*================================================================*
016270 受診者情報取得 SECTION.
016280*
016290**************************************************
016300* 連結データから受診者情報Ｆより以下の情報を取得 *
016310* ● 患者番号.... 患者番号Ｗ                     *
016320* ● 枝番........ 枝番Ｗ                         *
016330* ● 記号 ....... 記号Ｗに格納                   *
016340* ● 番号 ....... 番号Ｗに格納                   *
016350* ● 被保険者カナ.被保険者カナＷに格納           *
016360* ● 被保険者氏名.被保険者氏名Ｗに格納           *
016370* ● 郵便番号１ ..郵便番号１Ｗに格納             *
016380* ● 郵便番号２ ..郵便番号２Ｗに格納             *
016390* ● 住所１ ......住所１Ｗに格納                 *
016400* ● 住所２ ......住所２Ｗに格納                 *
016410* ● 電話番号.....電話番号Ｗに格納               *
016420* ● 患者カナ ....患者カナＷに格納               *
016430* ● 患者氏名 ....患者氏名Ｗに格納               *
016440* ● 続柄 ........名称マスタより続柄Ｗに取得     *
016450* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
016460* ● 患者年 ......患者年Ｗに格納                 *
016470* ● 患者月 ......患者月Ｗに格納                 *
016480* ● 患者日 ......患者日Ｗに格納                 *
016490**************************************************
016500     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
016510     MOVE 施術年ＷＲ         TO 受－施術年.
016520     MOVE 施術月ＷＲ         TO 受－施術月.
016530     MOVE 患者コードＷＲ     TO 受－患者コード.
016540     READ 受診者情報Ｆ
016550     INVALID KEY
016560         CONTINUE
016570*            /* ありえない */
016580     NOT INVALID KEY
016590         MOVE 受－患者番号     TO 患者番号Ｗ
016600         MOVE 受－枝番         TO 枝番Ｗ
016610*         MOVE 受－記号         TO 記号Ｗ
016620*         MOVE 受－番号         TO 番号Ｗ
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
016630         MOVE 受－被保険者カナ TO 被保険者カナＷ
016640         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
016650*
016660         EVALUATE 受－被保険者性別
016670         WHEN 1
016680             MOVE NC"○"  TO 被保険者男チェックＷ
016690         WHEN 2
016700             MOVE NC"○"  TO 被保険者女チェックＷ
016710         END-EVALUATE
016720         EVALUATE 受－患者性別
016730         WHEN 1
016740             MOVE NC"○"  TO 患者男チェックＷ
016750         WHEN 2
016760             MOVE NC"○"  TO 患者女チェックＷ
016770         END-EVALUATE
016780*
016790         EVALUATE 受－被保険者和暦
016800         WHEN 1
016810             MOVE NC"○"  TO 被保険者明治チェックＷ
016820         WHEN 2
016830             MOVE NC"○"  TO 被保険者大正チェックＷ
016840         WHEN 3
016850             MOVE NC"○"  TO 被保険者昭和チェックＷ
016860         WHEN 4
016870             MOVE NC"○"  TO 被保険者平成チェックＷ
016860         WHEN 5
016870             MOVE NC"○"  TO 被保険者令和チェックＷ
016880         END-EVALUATE
016890         MOVE 受－被保険者年   TO 被保険者年Ｗ
016900         MOVE 受－被保険者月   TO 被保険者月Ｗ
016910         MOVE 受－被保険者日   TO 被保険者日Ｗ
016920
016930*被保険者年齢
016940     MOVE 連印－施術和暦 TO 元－元号区分
016950     READ 元号マスタ
016960     NOT INVALID KEY
016970         COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
016980     END-READ
016990     IF ( 受－被保険者和暦 NOT = ZERO ) AND
017000        ( 受－被保険者年   NOT = ZERO ) AND
017010        ( 受－被保険者月   NOT = ZERO ) AND
017020        ( 受－被保険者日   NOT = ZERO )
017030
017040         MOVE ZERO TO 被保険者年齢Ｗ
017050*
017060         MOVE 受－被保険者和暦 TO 元－元号区分
017070         READ 元号マスタ
017080         NOT INVALID KEY
017090             COMPUTE 被保険者西暦年Ｗ = 元－開始西暦年 + ( 受－被保険者年 - 1 )
017100         END-READ
017110*
017120         COMPUTE 被保険者年齢Ｗ   = 施術西暦年Ｗ - 被保険者西暦年Ｗ
017130*
017140         IF 連印－施術月 < 受－被保険者月
017150             COMPUTE 被保険者年齢Ｗ = 被保険者年齢Ｗ - 1
017160         END-IF
017170*
017180*         MOVE 被保険者年齢Ｗ     TO 被保険者年齢
017190     END-IF
017200*
017210*/郵便番号 ゼロの時はスペース
017220         IF 受－郵便番号１ = "000"
017230             MOVE SPACE          TO 被保険者郵便番号１Ｗ
017240             MOVE SPACE          TO 被保険者郵便番号２Ｗ
017250         ELSE
017260             MOVE 受－郵便番号１ TO 被保険者郵便番号１Ｗ
017270             MOVE 受－郵便番号２ TO 被保険者郵便番号２Ｗ
017280         END-IF
017290         MOVE 受－電話番号     TO 被保険者電話番号Ｗ
017300         STRING 受－住所１  DELIMITED BY SPACE
017310                受－住所２  DELIMITED BY SPACE
017320           INTO 被保険者住所Ｗ
017330         END-STRING
017340*
017350*/郵便番号 ゼロの時はスペース
017360         IF 受－患者郵便番号１ = "000"
017370             MOVE SPACE              TO 患者郵便番号１Ｗ
017380             MOVE SPACE              TO 患者郵便番号２Ｗ
017390         ELSE
017400             MOVE 受－患者郵便番号１ TO 患者郵便番号１Ｗ
017410             MOVE 受－患者郵便番号２ TO 患者郵便番号２Ｗ
017420         END-IF
017430*         MOVE 受－患者住所１     TO 患者住所１Ｗ
017440*         MOVE 受－患者住所２     TO 患者住所２Ｗ
017450         STRING 受－患者住所１  DELIMITED BY SPACE
017460                受－患者住所２  DELIMITED BY SPACE
017470           INTO 患者住所Ｗ
017480         END-STRING
017490         MOVE 受－患者電話番号   TO 患者電話番号Ｗ
017500*
017510*電話番号分割
017520*         UNSTRING 電話番号Ｗ  DELIMITED BY "-"
017530*             INTO 分割電話番号１Ｗ
017540*                  分割電話番号２Ｗ
017550*                  分割電話番号３Ｗ
017560*         END-UNSTRING
017570*電話番号右詰処理
017580*         IF 分割電話番号３Ｗ = SPACE
017590*             MOVE 分割電話番号２Ｗ TO 分割電話番号３Ｗ
017600*             MOVE 分割電話番号１Ｗ TO 分割電話番号２Ｗ
017610*             MOVE SPACE            TO 分割電話番号１Ｗ
017620*         END-IF
0               IF 受－保険種別 = 70 OR 80 OR 85 OR 90
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
                   WHEN 5
                       MOVE NC"○"  TO 被保険者令和チェックＷ
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
017630*
017640         MOVE 受－患者カナ     TO 患者カナＷ
017650         MOVE 受－患者氏名     TO 患者氏名Ｗ
017660*
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
015540*         STRING 事－事業所住所１  DELIMITED BY SPACE
015550*                事－事業所住所２  DELIMITED BY SPACE
015560*           INTO 事業所住所Ｗ
015570*         END-STRING
               MOVE 事－事業所住所１   TO 印刷事業所住所Ｗ
               MOVE 事－事業所住所２   TO 印刷事業所住所２Ｗ
017720* 続柄
017730         IF 受－本人家族区分 = 1
017740             MOVE NC"本人"      TO 続柄Ｗ
017750         ELSE
017760             MOVE 05            TO 名－区分コード
017770             MOVE 受－続柄      TO 名－名称コード
017780             READ 名称マスタ
017790             INVALID KEY
017800                 MOVE SPACE     TO 続柄Ｗ
017810             NOT INVALID KEY
017820                 MOVE 名－略称  TO 続柄Ｗ
017830             END-READ
017840         END-IF
017850* 患者和暦年月日
017860         EVALUATE 受－患者和暦
017870         WHEN 1
017880             MOVE NC"○"  TO 明治チェックＷ
017890         WHEN 2
017900             MOVE NC"○"  TO 大正チェックＷ
017910         WHEN 3
017920             MOVE NC"○"  TO 昭和チェックＷ
017930         WHEN 4
017940             MOVE NC"○"  TO 平成チェックＷ
017930         WHEN 5
017940             MOVE NC"○"  TO 令和チェックＷ
017950         END-EVALUATE
017960         MOVE 受－患者年  TO 患者年Ｗ
017970         MOVE 受－患者月  TO 患者月Ｗ
017980         MOVE 受－患者日  TO 患者日Ｗ
017990*患者年齢
018000     MOVE 連印－施術和暦 TO 元－元号区分
018010     READ 元号マスタ
018020     NOT INVALID KEY
018030         COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
018040     END-READ
018050     IF ( 受－患者和暦 NOT = ZERO ) AND
018060        ( 受－患者年   NOT = ZERO ) AND
018070        ( 受－患者月   NOT = ZERO ) AND
018080        ( 受－患者日   NOT = ZERO )
018090*
018100         MOVE ZERO TO 患者年齢Ｗ
018110*
018120         MOVE 受－患者和暦 TO 元－元号区分
018130         READ 元号マスタ
018140         NOT INVALID KEY
018150             COMPUTE 患者西暦年Ｗ = 元－開始西暦年 + ( 受－患者年 - 1 )
018160         END-READ
018170*
018180         COMPUTE 患者年齢Ｗ   = 施術西暦年Ｗ - 患者西暦年Ｗ
018190*
018200         IF 連印－施術月 < 受－患者月
018210             COMPUTE 患者年齢Ｗ = 患者年齢Ｗ - 1
018220         END-IF
018230*
018240*         MOVE 患者年齢Ｗ     TO 患者年齢
018250     END-IF
018260* 患者性別
018270*         EVALUATE 受－患者性別
018280*         WHEN 1
018290*             MOVE NC"（男）" TO 患者性別Ｗ
018300*         WHEN 2
018310*             MOVE NC"（女）" TO 患者性別Ｗ
018320*         END-EVALUATE
018330*/有効期限
018340         MOVE 受－有効和暦   TO 有効期限和暦Ｗ
018350         MOVE 受－有効年     TO 有効期限年Ｗ
018360         MOVE 受－有効月     TO 有効期限月Ｗ
018370         MOVE 受－有効日     TO 有効期限日Ｗ
018380         MOVE 有効期限和暦Ｗ TO 元－元号区分
018390         READ 元号マスタ
018400         INVALID KEY
018410             MOVE SPACE        TO 有効期限元号Ｗ
018420         NOT INVALID KEY
018430             MOVE 元－元号名称 TO 有効期限元号Ｗ
018440         END-READ
018450*/資格取得年月日
018460         MOVE 受－資格和暦 TO 資格取得和暦Ｗ
018470         MOVE 受－資格年   TO 資格取得年Ｗ
018480         MOVE 受－資格月   TO 資格取得月Ｗ
018490         MOVE 受－資格日   TO 資格取得日Ｗ
018500         IF (資格取得和暦Ｗ NOT = ZERO) AND
018510            (資格取得年Ｗ   NOT = ZERO) AND
018520            (資格取得月Ｗ   NOT = ZERO) AND
018530            (資格取得日Ｗ   NOT = ZERO)
018540             EVALUATE 資格取得和暦Ｗ
018550             WHEN 3
018560                 MOVE NC"○"  TO 資格昭和チェックＷ
018570             WHEN 4
018580                 MOVE NC"○"  TO 資格平成チェックＷ
018570             WHEN 5
018580                 MOVE NC"○"  TO 資格令和チェックＷ
018590             END-EVALUATE
018600         END-IF
018380         MOVE 資格取得和暦Ｗ TO 元－元号区分
018390         READ 元号マスタ
018400         INVALID KEY
018410             MOVE SPACE        TO 資格取得元号Ｗ
018420         NOT INVALID KEY
018430             MOVE 元－元号名称 TO 資格取得元号Ｗ
018440         END-READ
018610**市町村国保､退職の場合は有効期限を出力
018620**業種別国保(国保組合)､共済､日雇､船員､社保､組合､自衛官は有効期限日を出力する
018630*         IF (( 受－保険種別 = 08 ) OR
018640*            (( 受－保険種別 = 01 ) AND
018650*            (  受－保険者番号(3:1) NOT = "3" )))
018660*            MOVE 受－有効和暦 TO 資格取得和暦Ｗ
018670*            MOVE 受－有効年   TO 資格取得年Ｗ
018680*            MOVE 受－有効月   TO 資格取得月Ｗ
018690*            MOVE 受－有効日   TO 資格取得日Ｗ
018700*         ELSE
018710*            MOVE 受－資格和暦 TO 資格取得和暦Ｗ
018720*            MOVE 受－資格年   TO 資格取得年Ｗ
018730*            MOVE 受－資格月   TO 資格取得月Ｗ
018740*            MOVE 受－資格日   TO 資格取得日Ｗ
018750*         END-IF
018760*         IF (資格取得和暦Ｗ NOT = ZERO) AND
018770*            (資格取得年Ｗ   NOT = ZERO) AND
018780*            (資格取得月Ｗ   NOT = ZERO) AND
018790*            (資格取得日Ｗ   NOT = ZERO)
018800*             EVALUATE 資格取得和暦Ｗ
018810*             WHEN 3
018820*                 MOVE NC"○"  TO 資格昭和チェックＷ
018830*             WHEN 4
018840*                 MOVE NC"○"  TO 資格平成チェックＷ
018850*             END-EVALUATE
018860*         END-IF
018870* 市町村番号 受給者番号
018880         IF 受－助成種別 NOT = ZERO
018890             MOVE 受－費用負担者番号助成 TO 費用負担者番号Ｗ
018900             MOVE 受－受益者番号助成     TO 受益者番号助成Ｗ
018910         END-IF
018920         IF ( 受－公費種別 = 05 ) AND ( 受－施術和暦年月 < 42004 )
018930             MOVE 受－費用負担者番号     TO 市町村番号Ｗ
018940             MOVE 受－受益者番号老人     TO 受益者番号Ｗ
018950         END-IF
018960*保険種別
018970         EVALUATE 受－助成種別
018980         WHEN 50
018990             MOVE NC"○"  TO 生保チェックＷ
019000         WHEN 51
019010*/頭4桁が "4113"東京 "4108"茨城 "4132"島根 の時は、「福」。それ以外は「老」
019020             IF  受－費用負担者番号助成(1:4) = "4113" OR
019030                 受－費用負担者番号助成(1:4) = "4108" OR
019040                 受－費用負担者番号助成(1:4) = "4132"
019050                 MOVE NC"○"    TO 福チェックＷ
019060             ELSE
019070                 MOVE NC"○"    TO 老人チェックＷ
019080             END-IF
019090*             MOVE NC"○"  TO 福チェックＷ
019100         WHEN 52
019110             MOVE NC"○"  TO 母子チェックＷ
019120         WHEN 53
019130             MOVE NC"○"  TO 身障チェックＷ
019140         WHEN 54
019150             MOVE NC"○"  TO 原爆チェックＷ
019160         WHEN 55
019170             MOVE NC"○"  TO 乳幼児チェックＷ
019180         END-EVALUATE
019190         IF 受－公費種別 = 05
019200             MOVE NC"○"  TO 老人チェックＷ
019210         ELSE
019220             EVALUATE 受－保険種別
019230             WHEN 01
019240                 MOVE NC"○"  TO 国保チェックＷ
019250             WHEN 02
019260                 MOVE NC"○"  TO 社保チェックＷ
019270             WHEN 03
019280                 MOVE NC"○"  TO 組合チェックＷ
019290             WHEN 04
019300                 MOVE NC"○"  TO 共済チェックＷ
019310             WHEN 06
019320                 MOVE NC"○"  TO 日雇チェックＷ
019330             WHEN 07
019340                 MOVE NC"○"  TO 船員チェックＷ
019350             WHEN 08
019360                 MOVE NC"○"  TO 退職チェックＷ
019370             WHEN 09
019380                 MOVE NC"○"  TO 共済チェックＷ
019390*             WHEN 70
019400*                 MOVE NC"○"  TO 労災チェックＷ
019410             WHEN 80
019420                 MOVE NC"○"  TO 自賠チェックＷ
019430             END-EVALUATE
019440         END-IF
019450     END-READ.
019460*TEST テスト－－－－－－－－－－－
019470*      MOVE NC"○"  TO 被保険者明治Ｗ.
019480*      MOVE NC"○"  TO 被保険者大正Ｗ.
019490*      MOVE NC"○"  TO 被保険者昭和Ｗ.
019500*      MOVE NC"○"  TO 被保険者男チェックＷ.
019510*      MOVE NC"○"  TO 被保険者女チェックＷ.
019520*      MOVE NC"○"  TO 患者男チェックＷ.
019530*      MOVE NC"○"  TO 患者女チェックＷ.
019540*      MOVE NC"○"  TO 明治チェックＷ.
019550*      MOVE NC"○"  TO 大正チェックＷ.
019560*      MOVE NC"○"  TO 昭和チェックＷ.
019570*      MOVE NC"○"  TO 平成チェックＷ.
019570*      MOVE NC"○"  TO 令和チェックＷ.
019580*      MOVE NC"○"  TO 社保チェックＷ    .
019590*      MOVE NC"○"  TO 組合チェックＷ    .
019600*      MOVE NC"○"  TO 共済チェックＷ    .
019610*      MOVE NC"○"  TO 国保チェックＷ    .
019620*      MOVE NC"○"  TO 日雇チェックＷ    .
019630*      MOVE NC"○"  TO 船員チェックＷ    .
019640*      MOVE NC"○"  TO 老人チェックＷ    .
019650*      MOVE NC"○"  TO 福チェックＷ    .
019660*      MOVE NC"○"  TO 身障チェックＷ    .
019670*      MOVE NC"○"  TO 退職チェックＷ    .
019680*      MOVE NC"○"  TO 母子チェックＷ    .
019690*      MOVE NC"○"  TO 乳幼児チェックＷ  .
019700*      MOVE NC"○"  TO 国保組合チェックＷ.
019710*      MOVE NC"○"  TO 労災チェックＷ    .
019720*      MOVE NC"○"  TO 生保チェックＷ    .
019730*      MOVE NC"○"  TO 自賠チェックＷ    .
019740*      MOVE NC"○"  TO 原爆チェックＷ    .
019750*      MOVE NC"○" TO ０割チェックＷ.
019760*      MOVE NC"○" TO １割チェックＷ.
019770*      MOVE NC"○" TO ２割チェックＷ.
019780*      MOVE NC"○" TO ３割チェックＷ.
019790*      MOVE NC"○"  TO 資格昭和チェックＷ.
019800*      MOVE NC"○"  TO 資格平成チェックＷ.
019800*      MOVE NC"○"  TO 資格令和チェックＷ.
019810*      MOVE NC"○"  TO 被保険者明治チェックＷ.
019820*      MOVE NC"○"  TO 被保険者大正チェックＷ.
019830*      MOVE NC"○"  TO 被保険者昭和チェックＷ.
019840*      MOVE NC"○"  TO 被保険者平成チェックＷ.
019840*      MOVE NC"○"  TO 被保険者令和チェックＷ.
019850*-------------------------------------
019860*================================================================*
019870 請求先情報取得 SECTION.
019880*
019890****************************************************
019900* 連結データから保険者マスタより請求先を取得する。 *
019910* ※保－請求先情報区分=1の場合請求先マスタを使用   *
019920* ● 保険者番号...保険者番号Ｗに格納               *
019930* ● 名称........ 請求先名称Ｗに格納               *
019940* ● 郵便番号１.. 請求先郵便番号１Ｗに格納         *
019950* ● 郵便番号２.. 請求先郵便番号２Ｗに格納         *
019960* ● 住所１.......請求先住所１Ｗに格納             *
019970* ● 住所２.......請求先住所２Ｗに格納             *
019980****************************************************
019990     MOVE SPACE          TO 保険者番号Ｗ.
020000     MOVE SPACE          TO 請求先名称Ｗ.
020010     MOVE SPACE          TO 支部名Ｗ.
020020     MOVE SPACE          TO 請求先郵便番号１Ｗ.
020030     MOVE SPACE          TO 請求先郵便番号２Ｗ.
020040     MOVE SPACE          TO 請求先住所１Ｗ.
020050     MOVE SPACE          TO 請求先住所２Ｗ.
020060     MOVE SPACE          TO 印刷保険者呼名Ｗ.
020070     MOVE SPACE          TO 請求先名支部名Ｗ
020080*
020090     MOVE 保険種別ＷＲ   TO 保－保険種別.
020100     MOVE 保険者番号ＷＲ TO 保－保険者番号.
020110     READ 保険者マスタ
020120     INVALID KEY
               IF ( 受－施術和暦年月 >= 42004 ) AND ( 保険種別ＷＲ = 05 )
016990             MOVE 保険種別ＷＲ   TO 市－公費種別
017000             MOVE 保険者番号ＷＲ TO 市－市町村番号
017010             READ 市町村マスタ
017020             INVALID KEY
017030                 MOVE SPACE      TO 請求先名称Ｗ
020140                 MOVE SPACE      TO 支部名Ｗ
020150                 MOVE ZERO       TO 接尾語区分Ｗ
017040             NOT INVALID KEY
017090                 MOVE 市－市町村番号  TO 保険者番号Ｗ
017110                 MOVE 市－市町村名称  TO 請求先名称Ｗ
020190                 MOVE 市－支部部署名  TO 支部名Ｗ
017120                 MOVE 市－郵便番号１  TO 請求先郵便番号１Ｗ
017130                 MOVE 市－郵便番号２  TO 請求先郵便番号２Ｗ
017140                 MOVE 市－住所１      TO 請求先住所１Ｗ
017150                 MOVE 市－住所２      TO 請求先住所２Ｗ
020150                 MOVE ZERO            TO 接尾語区分Ｗ
                   END-READ
               ELSE
020130             MOVE SPACE      TO 請求先名称Ｗ
020140             MOVE SPACE      TO 支部名Ｗ
020150             MOVE ZERO       TO 接尾語区分Ｗ
               END-IF
020160     NOT INVALID KEY
020170         MOVE 保－保険者番号  TO 保険者番号Ｗ
020180         MOVE 保－保険者名称  TO 請求先名称Ｗ
020190         MOVE 保－支部部署名  TO 支部名Ｗ
020200         MOVE 保－郵便番号１  TO 請求先郵便番号１Ｗ
020210         MOVE 保－郵便番号２  TO 請求先郵便番号２Ｗ
020220         MOVE 保－住所１      TO 請求先住所１Ｗ
020230         MOVE 保－住所２      TO 請求先住所２Ｗ
020240         MOVE 保－接尾語区分  TO 接尾語区分Ｗ
020250     END-READ.
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
022180*
020260     EVALUATE 保険種別ＷＲ
020270     WHEN 2
020280     WHEN 6
020290         IF 接尾語区分Ｗ = 1
020300            MOVE SPACE              TO 印刷保険者呼名Ｗ
020310         ELSE
020320            MOVE "社会保険事務所"   TO 印刷保険者呼名Ｗ
020330         END-IF
020340     WHEN 3
020350         MOVE "健康保険組合"   TO 印刷保険者呼名Ｗ
020360     WHEN OTHER
020370         MOVE SPACE TO 印刷保険者呼名Ｗ
020380     END-EVALUATE.
020390     IF 印刷保険者呼名Ｗ NOT = SPACE
020400         STRING 請求先名称Ｗ DELIMITED BY SPACE
020410                保険者呼名Ｗ DELIMITED BY SPACE
020420           INTO 請求先名称Ｗ
020430         END-STRING
020440     END-IF.
020450*組合と共済は支部名も印字する
020460     EVALUATE 保険種別ＷＲ
020470     WHEN 03
020480     WHEN 04
020490         STRING 請求先名称Ｗ DELIMITED BY SPACE
020500                "  "         DELIMITED BY SIZE
020510                支部名Ｗ     DELIMITED BY SPACE
020520           INTO 請求先名支部名Ｗ
020530         END-STRING
020540     WHEN OTHER
020550         MOVE 請求先名称Ｗ TO 請求先名支部名Ｗ
020560     END-EVALUATE.
020570*================================================================*
020580 負傷データ取得 SECTION.
020590*
020600**************************************************
020610* 連結データから負傷データＦより以下の情報を取得 *
020620* ● 負傷名...部位＋負傷種別にて加工して格納     *
020630* ● 負傷年.......負傷年Ｗ                       *
020640* ● 負傷月.......負傷月Ｗ                       *
020650* ● 負傷日.......負傷日Ｗ                       *
020660* ● 施術終了年...終了年Ｗ                       *
020670* ● 施術終了月...終了月Ｗ                       *
020680* ● 施術終了日...終了日Ｗ                       *
020690**************************************************
020700     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
020710     MOVE 施術年ＷＲ         TO 負－施術年.
020720     MOVE 施術月ＷＲ         TO 負－施術月.
020730     MOVE 患者コードＷＲ     TO 負－患者コード.
020740     READ 負傷データＦ
020750     INVALID KEY
020760         CONTINUE
020770*            /* ありえない */
020780     NOT INVALID KEY
020790         MOVE ZERO                         TO 部位数Ｗ
020800         MOVE 負－部位数                   TO 部位数Ｗ
020810         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
020820                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
020830*********************************************
020840* 注）全柔...負傷種別＋部位にて加工して格納 *
020850*********************************************
020860* 負傷種別
020870             MOVE SPACE                     TO 負傷名称Ｗ
020880             MOVE 03                        TO 名－区分コード
020890             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
020900             READ 名称マスタ
020910             INVALID KEY
020920                 MOVE SPACE    TO 負傷名称Ｗ
020930             NOT INVALID KEY
020940*                 MOVE 名－略称 TO 負傷名称Ｗ
020950                 MOVE 名－正式名称 TO 負傷名称Ｗ
020960             END-READ
020970* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
020990             PERFORM 名称埋込処理
021050             MOVE 負－負傷和暦(部位ＣＮＴ) TO 負傷和暦Ｗ(部位ＣＮＴ)
021050             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
021060             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
021070             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
021080*
021090             MOVE 負－開始和暦(部位ＣＮＴ) TO 施術開始和暦Ｗ(部位ＣＮＴ)
021090             MOVE 負－開始年(部位ＣＮＴ)   TO 施術開始年Ｗ(部位ＣＮＴ)
021100             MOVE 負－開始月(部位ＣＮＴ)   TO 施術開始月Ｗ(部位ＣＮＴ)
021110             MOVE 負－開始日(部位ＣＮＴ)   TO 施術開始日Ｗ(部位ＣＮＴ)
021120*
021130             MOVE 負－終了和暦(部位ＣＮＴ) TO 施術終了和暦Ｗ(部位ＣＮＴ)
021130             MOVE 負－終了年(部位ＣＮＴ)   TO 施術終了年Ｗ(部位ＣＮＴ)
021140             MOVE 負－終了月(部位ＣＮＴ)   TO 施術終了月Ｗ(部位ＣＮＴ)
021150             MOVE 負－終了日(部位ＣＮＴ)   TO 施術終了日Ｗ(部位ＣＮＴ)
021160*転帰
021170             EVALUATE 負－転帰区分(部位ＣＮＴ)
021180             WHEN 1
021190             WHEN 2
021200                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
021210             WHEN 3
021220                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
021230             WHEN 4
021240                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
021250             END-EVALUATE
021260*             IF 負－転帰区分(部位ＣＮＴ) NOT = 9
021270*                 MOVE 04                        TO 名－区分コード
021280*                 MOVE 負－転帰区分(部位ＣＮＴ)  TO 名－名称コード
021290*                 READ 名称マスタ
021300*                 INVALID KEY
021310*                     MOVE SPACE    TO 転帰Ｗ(部位ＣＮＴ)
021320*                 NOT INVALID KEY
021330*                     MOVE 名－略称 TO 転帰Ｗ(部位ＣＮＴ)
021340*                 END-READ
021350*             END-IF
021360*TEST---------------------------------
021370*         MOVE NC"左指節関節捻挫左指節関節捻挫骨折挫傷" TO 負傷名Ｗ(部位ＣＮＴ)
021380*         MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
021390*         MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
021400*         MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
021410*-------------------------------------
021420*/施術回数取得
021430             IF 負－転帰区分(部位ＣＮＴ) NOT = 9
021440                 PERFORM 累計施術回数取得
021450             END-IF
021460             PERFORM 負傷データ退避
021470         END-PERFORM
021480     END-READ.
021490*================================================================*
021500 施術記録取得 SECTION.
021510*
021520************************************************************
021530* 連結データから負傷データＦより以下の情報を取得           *
021540* ● 施術開始年月日...該当する部位に対して当月最初の施術日 *
021550************************************************************
021560     MOVE 施術和暦ＷＲ  TO 施記－施術和暦.
021570     MOVE 施術年ＷＲ    TO 施記－施術年.
021580     MOVE 施術月ＷＲ    TO 施記－施術月.
021590     MOVE ZERO            TO 施記－施術日.
021600     MOVE ZERO            TO 施記－患者番号.
021610     MOVE SPACE           TO 施記－枝番.
021620     START 施術記録Ｆ   KEY IS >= 施記－施術和暦年月日
021630                                  施記－患者コード.
021640     IF 状態キー = "00"
021650         MOVE SPACE  TO 終了フラグ２
021660         PERFORM 施術記録Ｆ読込
021670         PERFORM UNTIL ( 終了フラグ２       = "YES" ) OR
021680                       ( 施記－施術和暦 NOT = 施術和暦ＷＲ ) OR
021690                       ( 施記－施術年   NOT = 施術年ＷＲ   ) OR
021700                       ( 施記－施術月   NOT = 施術月ＷＲ   )
021710*            **************
021720*            * 開始年月日 *
021730*            **************
021740             PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
021750                     UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
021760            EVALUATE TRUE ALSO TRUE ALSO TRUE ALSO TRUE ALSO TRUE
021770            WHEN 施記－負傷種別(部位ＣＮＴ) = 負傷種別Ｗ(部位ＣＮＴ) ALSO
021780                 施記－部位(部位ＣＮＴ)     = 部位Ｗ(部位ＣＮＴ)     ALSO
021790                 施記－左右区分(部位ＣＮＴ) = 左右区分Ｗ(部位ＣＮＴ) ALSO
021800                 施記－負傷位置番号(部位ＣＮＴ)
021810                                       = 負傷位置番号Ｗ(部位ＣＮＴ)  ALSO
021820                 開始年月日取得フラグ(部位ＣＮＴ) = SPACE
021830                   MOVE 施記－施術年     TO 施術開始年Ｗ(部位ＣＮＴ)
021840                   MOVE 施記－施術月     TO 施術開始月Ｗ(部位ＣＮＴ)
021850                   MOVE 施記－施術日     TO 施術開始日Ｗ(部位ＣＮＴ)
021860                   MOVE "YES"       TO 開始年月日取得フラグ(部位ＣＮＴ)
021870*
021880            WHEN OTHER
021890                CONTINUE
021900            END-EVALUATE
021910             END-PERFORM
021920             PERFORM 施術記録Ｆ読込
021930         END-PERFORM
021940     END-IF.
021950*================================================================*
021960 施術記録Ｆ読込 SECTION.
021970*
021980     READ 施術記録Ｆ NEXT
021990     AT END
022000         MOVE "YES" TO 終了フラグ２
022010     END-READ.
022020*
023000*================================================================*
023010 負傷原因セット SECTION.
023020*
023030**************************************************************************
023040*  文章が1行を超える時は、複数行に分解する。
023050**************************************************************************
023060     MOVE  ZERO   TO  カウンタ カウンタ２.
023070     PERFORM VARYING カウンタ FROM 1 BY 1
023080*/終了条件がおかしい/050203
023090             UNTIL ( カウンタ > 9 )
023100*             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
023110*
023120          INITIALIZE 負傷原因内容分解ＸＷ
023130          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
023140          IF  負傷原因内容１ＸＷ  NOT = SPACE
023150              COMPUTE カウンタ２ = カウンタ２  +  1
023160              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
023170          END-IF
023180          IF  負傷原因内容２ＸＷ  NOT = SPACE
023190              COMPUTE カウンタ２ = カウンタ２  +  1
023200              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
023210          END-IF
023220          IF  負傷原因内容３ＸＷ  NOT = SPACE
023230              COMPUTE カウンタ２ = カウンタ２  +  1
023240              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
023250          END-IF
023260          IF  負傷原因内容４ＸＷ  NOT = SPACE
023270              COMPUTE カウンタ２ = カウンタ２  +  1
023280              MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
023290          END-IF
023300          IF  負傷原因内容５ＸＷ  NOT = SPACE
023310              COMPUTE カウンタ２ = カウンタ２  +  1
023320              MOVE 負傷原因内容５ＸＷ  TO 負傷原因Ｗ(カウンタ２)
023330          END-IF
023340          IF  負傷原因内容６ＸＷ  NOT = SPACE
023350              COMPUTE カウンタ２ = カウンタ２  +  1
023360              MOVE 負傷原因内容６ＸＷ  TO 負傷原因Ｗ(カウンタ２)
023370          END-IF
023380          IF  負傷原因内容７ＸＷ  NOT = SPACE
023390              COMPUTE カウンタ２ = カウンタ２  +  1
023400              MOVE 負傷原因内容７ＸＷ  TO 負傷原因Ｗ(カウンタ２)
023410          END-IF
023420          IF  負傷原因内容８ＸＷ  NOT = SPACE
023430              COMPUTE カウンタ２ = カウンタ２  +  1
023440              MOVE 負傷原因内容８ＸＷ  TO 負傷原因Ｗ(カウンタ２)
023450          END-IF
023420          IF  負傷原因内容９ＸＷ  NOT = SPACE
023430              COMPUTE カウンタ２ = カウンタ２  +  1
023440              MOVE 負傷原因内容９ＸＷ  TO 負傷原因Ｗ(カウンタ２)
023450          END-IF
023420          IF  負傷原因内容１０ＸＷ  NOT = SPACE
023430              COMPUTE カウンタ２ = カウンタ２  +  1
023440              MOVE 負傷原因内容１０ＸＷ  TO 負傷原因Ｗ(カウンタ２)
023450          END-IF
023420          IF  負傷原因内容１１ＸＷ  NOT = SPACE
023430              COMPUTE カウンタ２ = カウンタ２  +  1
023440              MOVE 負傷原因内容１１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
023450          END-IF
023460*
023470     END-PERFORM.
023480*
023490*================================================================*
023500 印刷処理 SECTION.
023510*
023520     EVALUATE 印刷モードＦＷＲ
023530     WHEN 0
023540         PERFORM 印刷処理２
023550         PERFORM 印刷処理３
023560     WHEN 1
023570         PERFORM 印刷処理２
023580     WHEN 2
023590         PERFORM 印刷処理３
023600     WHEN 3
023610         PERFORM 印刷処理４
023620     WHEN 4
023630         IF 連印－保険証印刷区分 = 1
023640             PERFORM 印刷処理２
023650         END-IF
023660         PERFORM 印刷処理３
023670         PERFORM 印刷処理４
023680     END-EVALUATE.
023690*================================================================*
023700 印刷処理１ SECTION.
023710*
023720     MOVE "YCH6621P" TO  定義体名Ｐ.
023730*     MOVE "SCREEN"  TO  項目群名Ｐ.
023740     MOVE "GRP001"  TO  項目群名Ｐ.
023750     WRITE YCH6621P.
023760*     WRITE 印刷レコード.
023770     PERFORM エラー処理Ｐ.
023780*================================================================*
023790 印刷処理２ SECTION.
023800*
023810     MOVE "YCH6621P"  TO  定義体名Ｐ.
023820     MOVE "GRP001"   TO  項目群名Ｐ.
023830     WRITE YCH6621P.
023840*     WRITE 印刷レコード.
023850     PERFORM エラー処理Ｐ.
023860*================================================================*
023870 印刷処理３ SECTION.
023880*
023890     MOVE "YCH6621P"  TO  定義体名Ｐ.
023900     MOVE "GRP002"   TO  項目群名Ｐ.
023910     WRITE YCH6621P.
023920*     WRITE 印刷レコード.
023930     PERFORM エラー処理Ｐ.
023940*================================================================*
023950 印刷処理４ SECTION.
023960*
023970     MOVE "YCH6621P"  TO  定義体名Ｐ.
023980     MOVE "GRP003"   TO  項目群名Ｐ.
023990     WRITE YCH6621P.
024000     PERFORM エラー処理Ｐ.
024010*================================================================*
024020 エラー処理Ｐ SECTION.
024030*
024040     IF 通知情報Ｐ NOT = "00"
024050         DISPLAY NC"帳票エラー"              UPON CONS
024060         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
024070         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
024080         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
024090         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
024100                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
024110         ACCEPT  キー入力 FROM CONS
024120         PERFORM ファイル閉鎖
024130         EXIT PROGRAM
024140     END-IF.
024150*================================================================*
024160 名称埋込処理 SECTION.
024170*
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
024280     END-READ.
024290*
024580*================================================================*
024590 合計金額行セット SECTION.
024600*
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
024610        MOVE レセ－施術年   TO 金額行年       (段数ＷＲ)
024620        MOVE レセ－施術月   TO 金額行月       (段数ＷＲ)
024630        MOVE NC"年"         TO 金額行年表示   (段数ＷＲ)
024670        MOVE レセ－合計     TO 費用額(段数ＷＲ)
              IF 受－助成種別 = ZERO
024800           MOVE レセ－一部負担金   TO 負担額(段数ＷＲ)
024810           MOVE レセ－請求金額 TO 請求額(段数ＷＲ)
              ELSE
024800           MOVE レセ－受給者負担額 TO 負担額(段数ＷＲ)
024810           COMPUTE 請求額(段数ＷＲ) = レセ－助成請求金額 + レセ－請求金額
              END-IF
           END-READ.
024640     MOVE 提出年ＷＲ   TO 提出年         (段数ＷＲ).
024650     MOVE 提出月ＷＲ   TO 提出月         (段数ＷＲ).
024660     MOVE 提出日ＷＲ   TO 提出日         (段数ＷＲ).
025140*================================================================*
025150  請求期間取得 SECTION.
025160*
025170     EVALUATE 施術月ＷＲ
025180     WHEN 4
025190     WHEN 6
025200     WHEN 9
025210     WHEN 11
025220         MOVE 30 TO 請求終了日Ｗ
025230     WHEN 2
025240         COMPUTE 対象西暦Ｗ = 和暦終了年Ｗ + 施術年ＷＲ
025250         DIVIDE 4 INTO 対象西暦Ｗ GIVING    商Ｗ
025260                                  REMAINDER 余Ｗ
025270         END-DIVIDE
025280         IF 余Ｗ = ZERO
025290             MOVE 29 TO 請求終了日Ｗ
025300         ELSE
025310             MOVE 28 TO 請求終了日Ｗ
025320         END-IF
025330     WHEN 1
025340     WHEN 3
025350     WHEN 5
025360     WHEN 7
025370     WHEN 8
025380     WHEN 10
025390     WHEN 12
025400         MOVE 31 TO 請求終了日Ｗ
025410     WHEN OTHER
025420         MOVE  NC"未知のエラー発生" TO 連メ－メッセージ
025430         CALL   "MSG001"
025440         CANCEL "MSG001"
025450     END-EVALUATE.
025460*================================================================*
025470*================================================================*
025480 印刷セット５ SECTION.
025490*
025500*/詳細モード用
025510     INITIALIZE YCH6621P.
025520     MOVE "X" TO EDIT-MODE OF 患者コード.
025530     INITIALIZE 受診者情報Ｗ.
025540     INITIALIZE 事業所情報Ｗ.
025550     INITIALIZE 請求先情報Ｗ.
025560     INITIALIZE 負傷情報Ｗ.
025570     INITIALIZE 負担率チェックＷ.
025580     PERFORM 負担率取得.
025600     PERFORM 受診者情報取得.
025590     PERFORM 請求先情報取得.
025610     PERFORM 負傷データ取得.
025620*
025630     IF 負傷原因印刷区分Ｗ  NOT = 1
025640        PERFORM 詳細モード負傷原因処理
025650        PERFORM 負傷原因取得
025660     END-IF.
025670*
025680     IF 連印－保険証印刷区分 = 1
025690         PERFORM 印刷上部セット
025700     END-IF.
025710     PERFORM 印刷下部セット詳細.
025720     PERFORM 印刷金額セット詳細.
025730*================================================================*
025740*================================================================*
025750 印刷下部セット詳細 SECTION.
025760*
025770********************
025780* 負傷データセット *
025790********************
025800**********
025810* １部位 *
025820**********
025830     IF 連印－負傷印刷行(1) NOT = ZERO
025840         IF 連印－部位印刷区分(1) = 1
025850             MOVE 負傷名Ｗ(1)         TO 負傷名ＳＷ      (連印－負傷印刷行(1))
025860             MOVE 負傷和暦Ｗ(1)       TO 負傷和暦ＳＷ    (連印－負傷印刷行(1))
025860             MOVE 負傷年Ｗ(1)         TO 負傷年ＳＷ      (連印－負傷印刷行(1))
025870             MOVE 負傷月Ｗ(1)         TO 負傷月ＳＷ      (連印－負傷印刷行(1))
025880             MOVE 負傷日Ｗ(1)         TO 負傷日ＳＷ      (連印－負傷印刷行(1))
025890             MOVE 施術開始和暦Ｗ(1)   TO 施術開始和暦ＳＷ(連印－負傷印刷行(1))
025890             MOVE 施術開始年Ｗ(1)     TO 施術開始年ＳＷ  (連印－負傷印刷行(1))
025900             MOVE 施術開始月Ｗ(1)     TO 施術開始月ＳＷ  (連印－負傷印刷行(1))
025910             MOVE 施術開始日Ｗ(1)     TO 施術開始日ＳＷ  (連印－負傷印刷行(1))
025920             MOVE 施術開始和暦Ｗ(1)   TO 施術開始和暦ＳＷ(連印－負傷印刷行(1))
025920             MOVE 施術開始年Ｗ(1)     TO 施術開始年ＳＷ  (連印－負傷印刷行(1))
025930             MOVE 施術開始月Ｗ(1)     TO 施術開始月ＳＷ  (連印－負傷印刷行(1))
025940             MOVE 施術開始日Ｗ(1)     TO 施術開始日ＳＷ  (連印－負傷印刷行(1))
025950         END-IF
025960         IF 連印－転帰印刷区分(1) = 1
025970             MOVE 施術終了和暦Ｗ(1)   TO 施術終了和暦ＳＷ(連印－負傷印刷行(1))
025970             MOVE 施術終了年Ｗ(1)     TO 施術終了年ＳＷ  (連印－負傷印刷行(1))
025980             MOVE 施術終了月Ｗ(1)     TO 施術終了月ＳＷ  (連印－負傷印刷行(1))
025990             MOVE 施術終了日Ｗ(1)     TO 施術終了日ＳＷ  (連印－負傷印刷行(1))
026000             MOVE 治癒チェックＷ(1)   TO 治癒チェックＳＷ(連印－負傷印刷行(1))
026010             MOVE 中止チェックＷ(1)   TO 中止チェックＳＷ(連印－負傷印刷行(1))
026020             MOVE 転医チェックＷ(1)   TO 転医チェックＳＷ(連印－負傷印刷行(1))
026030             MOVE 施術累計回数Ｗ(1)   TO 施術日数ＳＷ    (連印－負傷印刷行(1))
026040         END-IF
026050     END-IF.
026060**********
026070* ２部位 *
026080**********
026090     IF 連印－負傷印刷行(2) NOT = ZERO
026100         IF 連印－部位印刷区分(2) = 1
026110             MOVE 負傷名Ｗ(2)         TO 負傷名ＳＷ      (連印－負傷印刷行(2))
025860             MOVE 負傷和暦Ｗ(2)       TO 負傷和暦ＳＷ    (連印－負傷印刷行(2))
026120             MOVE 負傷年Ｗ(2)         TO 負傷年ＳＷ      (連印－負傷印刷行(2))
026130             MOVE 負傷月Ｗ(2)         TO 負傷月ＳＷ      (連印－負傷印刷行(2))
026140             MOVE 負傷日Ｗ(2)         TO 負傷日ＳＷ      (連印－負傷印刷行(2))
025890             MOVE 施術開始和暦Ｗ(2)   TO 施術開始和暦ＳＷ(連印－負傷印刷行(2))
026150             MOVE 施術開始年Ｗ(2)     TO 施術開始年ＳＷ  (連印－負傷印刷行(2))
026160             MOVE 施術開始月Ｗ(2)     TO 施術開始月ＳＷ  (連印－負傷印刷行(2))
026170             MOVE 施術開始日Ｗ(2)     TO 施術開始日ＳＷ  (連印－負傷印刷行(2))
025920             MOVE 施術開始和暦Ｗ(2)   TO 施術開始和暦ＳＷ(連印－負傷印刷行(2))
026180             MOVE 施術開始年Ｗ(2)     TO 施術開始年ＳＷ  (連印－負傷印刷行(2))
026190             MOVE 施術開始月Ｗ(2)     TO 施術開始月ＳＷ  (連印－負傷印刷行(2))
026200             MOVE 施術開始日Ｗ(2)     TO 施術開始日ＳＷ  (連印－負傷印刷行(2))
026210         END-IF
026220         IF 連印－転帰印刷区分(2) = 1
025970             MOVE 施術終了和暦Ｗ(2)   TO 施術終了和暦ＳＷ(連印－負傷印刷行(2))
026230             MOVE 施術終了年Ｗ(2)     TO 施術終了年ＳＷ  (連印－負傷印刷行(2))
026240             MOVE 施術終了月Ｗ(2)     TO 施術終了月ＳＷ  (連印－負傷印刷行(2))
026250             MOVE 施術終了日Ｗ(2)     TO 施術終了日ＳＷ  (連印－負傷印刷行(2))
026260             MOVE 治癒チェックＷ(2)   TO 治癒チェックＳＷ(連印－負傷印刷行(2))
026270             MOVE 中止チェックＷ(2)   TO 中止チェックＳＷ(連印－負傷印刷行(2))
026280             MOVE 転医チェックＷ(2)   TO 転医チェックＳＷ(連印－負傷印刷行(2))
026290             MOVE 施術累計回数Ｗ(2)   TO 施術日数ＳＷ    (連印－負傷印刷行(2))
026300     END-IF.
026310**********
026320* ３部位 *
026330**********
026340     IF 連印－負傷印刷行(3) NOT = ZERO
026350         IF 連印－部位印刷区分(3) = 1
026360             MOVE 負傷名Ｗ(3)         TO 負傷名ＳＷ      (連印－負傷印刷行(3))
025860             MOVE 負傷和暦Ｗ(3)       TO 負傷和暦ＳＷ    (連印－負傷印刷行(3))
026370             MOVE 負傷年Ｗ(3)         TO 負傷年ＳＷ      (連印－負傷印刷行(3))
026380             MOVE 負傷月Ｗ(3)         TO 負傷月ＳＷ      (連印－負傷印刷行(3))
026390             MOVE 負傷日Ｗ(3)         TO 負傷日ＳＷ      (連印－負傷印刷行(3))
025890             MOVE 施術開始和暦Ｗ(3)   TO 施術開始和暦ＳＷ(連印－負傷印刷行(3))
026400             MOVE 施術開始年Ｗ(3)     TO 施術開始年ＳＷ  (連印－負傷印刷行(3))
026410             MOVE 施術開始月Ｗ(3)     TO 施術開始月ＳＷ  (連印－負傷印刷行(3))
026420             MOVE 施術開始日Ｗ(3)     TO 施術開始日ＳＷ  (連印－負傷印刷行(3))
025920             MOVE 施術開始和暦Ｗ(3)   TO 施術開始和暦ＳＷ(連印－負傷印刷行(3))
026430             MOVE 施術開始年Ｗ(3)     TO 施術開始年ＳＷ  (連印－負傷印刷行(3))
026440             MOVE 施術開始月Ｗ(3)     TO 施術開始月ＳＷ  (連印－負傷印刷行(3))
026450             MOVE 施術開始日Ｗ(3)     TO 施術開始日ＳＷ  (連印－負傷印刷行(3))
026460         END-IF
026470         IF 連印－転帰印刷区分(3) = 1
025970             MOVE 施術終了和暦Ｗ(3)   TO 施術終了和暦ＳＷ(連印－負傷印刷行(3))
026480             MOVE 施術終了年Ｗ(3)     TO 施術終了年ＳＷ  (連印－負傷印刷行(3))
026490             MOVE 施術終了月Ｗ(3)     TO 施術終了月ＳＷ  (連印－負傷印刷行(3))
026500             MOVE 施術終了日Ｗ(3)     TO 施術終了日ＳＷ  (連印－負傷印刷行(3))
026510             MOVE 治癒チェックＷ(3)   TO 治癒チェックＳＷ(連印－負傷印刷行(3))
026520             MOVE 中止チェックＷ(3)   TO 中止チェックＳＷ(連印－負傷印刷行(3))
026530             MOVE 転医チェックＷ(3)   TO 転医チェックＳＷ(連印－負傷印刷行(3))
026540             MOVE 施術累計回数Ｗ(3)   TO 施術日数ＳＷ    (連印－負傷印刷行(3))
026550     END-IF.
026560**********
026570* ４部位 *
026580**********
026590     IF 連印－負傷印刷行(4) NOT = ZERO
026600         IF 連印－部位印刷区分(4) = 1
026610             MOVE 負傷名Ｗ(4)         TO 負傷名ＳＷ      (連印－負傷印刷行(4))
025860             MOVE 負傷和暦Ｗ(4)       TO 負傷和暦ＳＷ    (連印－負傷印刷行(4))
026620             MOVE 負傷年Ｗ(4)         TO 負傷年ＳＷ      (連印－負傷印刷行(4))
026630             MOVE 負傷月Ｗ(4)         TO 負傷月ＳＷ      (連印－負傷印刷行(4))
026640             MOVE 負傷日Ｗ(4)         TO 負傷日ＳＷ      (連印－負傷印刷行(4))
025890             MOVE 施術開始和暦Ｗ(4)   TO 施術開始和暦ＳＷ(連印－負傷印刷行(4))
026650             MOVE 施術開始年Ｗ(4)     TO 施術開始年ＳＷ  (連印－負傷印刷行(4))
026660             MOVE 施術開始月Ｗ(4)     TO 施術開始月ＳＷ  (連印－負傷印刷行(4))
026670             MOVE 施術開始日Ｗ(4)     TO 施術開始日ＳＷ  (連印－負傷印刷行(4))
025920             MOVE 施術開始和暦Ｗ(4)   TO 施術開始和暦ＳＷ(連印－負傷印刷行(4))
026680             MOVE 施術開始年Ｗ(4)     TO 施術開始年ＳＷ  (連印－負傷印刷行(4))
026690             MOVE 施術開始月Ｗ(4)     TO 施術開始月ＳＷ  (連印－負傷印刷行(4))
026700             MOVE 施術開始日Ｗ(4)     TO 施術開始日ＳＷ  (連印－負傷印刷行(4))
026710         END-IF
026720         IF 連印－転帰印刷区分(4) = 1
025970             MOVE 施術終了和暦Ｗ(4)   TO 施術終了和暦ＳＷ(連印－負傷印刷行(4))
026730             MOVE 施術終了年Ｗ(4)     TO 施術終了年ＳＷ  (連印－負傷印刷行(4))
026740             MOVE 施術終了月Ｗ(4)     TO 施術終了月ＳＷ  (連印－負傷印刷行(4))
026750             MOVE 施術終了日Ｗ(4)     TO 施術終了日ＳＷ  (連印－負傷印刷行(4))
026760             MOVE 治癒チェックＷ(4)   TO 治癒チェックＳＷ(連印－負傷印刷行(4))
026770             MOVE 中止チェックＷ(4)   TO 中止チェックＳＷ(連印－負傷印刷行(4))
026780             MOVE 転医チェックＷ(4)   TO 転医チェックＳＷ(連印－負傷印刷行(4))
026790             MOVE 施術累計回数Ｗ(4)   TO 施術日数ＳＷ    (連印－負傷印刷行(4))
026800     END-IF.
026810**********
026820* １部位 *
026830**********
026840     MOVE 負傷名ＳＷ(1)         TO 負傷名１       .
           IF 負傷和暦ＳＷ(1) = 5
              MOVE NC"令和"           TO 負傷和暦１
           END-IF.
026850     MOVE 負傷年ＳＷ(1)         TO 負傷年１       .
026860     MOVE 負傷月ＳＷ(1)         TO 負傷月１       .
026870     MOVE 負傷日ＳＷ(1)         TO 負傷日１       .
           IF 施術開始和暦ＳＷ(1) = 5
              MOVE NC"令和"           TO 施術開始和暦１
           END-IF.
026880     MOVE 施術開始年ＳＷ(1)     TO 施術開始年１   .
026890     MOVE 施術開始月ＳＷ(1)     TO 施術開始月１   .
026900     MOVE 施術開始日ＳＷ(1)     TO 施術開始日１   .
           IF 施術開始和暦ＳＷ(1) = 5
              MOVE NC"令和"           TO 施術期間自和暦１
           END-IF.
026910     MOVE 施術開始年ＳＷ(1)     TO 施術期間自年１ .
026920     MOVE 施術開始月ＳＷ(1)     TO 施術期間自月１ .
026930     MOVE 施術開始日ＳＷ(1)     TO 施術期間自日１ .
           IF 施術終了和暦ＳＷ(1) = 5
              MOVE NC"令和"           TO 施術終了和暦１
           END-IF.
026940     MOVE 施術終了年ＳＷ(1)     TO 施術終了年１   .
026950     MOVE 施術終了月ＳＷ(1)     TO 施術終了月１   .
026960     MOVE 施術終了日ＳＷ(1)     TO 施術終了日１   .
026970     MOVE 治癒チェックＳＷ(1)   TO 治癒チェック１ .
026980     MOVE 中止チェックＳＷ(1)   TO 中止チェック１ .
026990     MOVE 転医チェックＳＷ(1)   TO 転医チェック１ .
027000     MOVE 施術日数ＳＷ(1)       TO 施術日数１.
027010**********
027020* ２部位 *
027030**********
027040     MOVE 負傷名ＳＷ(2)         TO 負傷名２       .
           IF 負傷和暦ＳＷ(2) = 5
              MOVE NC"令和"           TO 負傷和暦２
           END-IF.
027050     MOVE 負傷年ＳＷ(2)         TO 負傷年２       .
027060     MOVE 負傷月ＳＷ(2)         TO 負傷月２       .
027070     MOVE 負傷日ＳＷ(2)         TO 負傷日２       .
           IF 施術開始和暦ＳＷ(2) = 5
              MOVE NC"令和"           TO 施術開始和暦２
           END-IF.
027080     MOVE 施術開始年ＳＷ(2)     TO 施術開始年２   .
027090     MOVE 施術開始月ＳＷ(2)     TO 施術開始月２   .
027100     MOVE 施術開始日ＳＷ(2)     TO 施術開始日２   .
           IF 施術開始和暦ＳＷ(2) = 5
              MOVE NC"令和"           TO 施術期間自和暦２
           END-IF.
027110     MOVE 施術開始年ＳＷ(2)     TO 施術期間自年２ .
027120     MOVE 施術開始月ＳＷ(2)     TO 施術期間自月２ .
027130     MOVE 施術開始日ＳＷ(2)     TO 施術期間自日２ .
           IF 施術終了和暦ＳＷ(2) = 5
              MOVE NC"令和"           TO 施術終了和暦２
           END-IF.
027140     MOVE 施術終了年ＳＷ(2)     TO 施術終了年２   .
027150     MOVE 施術終了月ＳＷ(2)     TO 施術終了月２   .
027160     MOVE 施術終了日ＳＷ(2)     TO 施術終了日２   .
027170     MOVE 治癒チェックＳＷ(2)   TO 治癒チェック２ .
027180     MOVE 中止チェックＳＷ(2)   TO 中止チェック２ .
027190     MOVE 転医チェックＳＷ(2)   TO 転医チェック２ .
027200     MOVE 施術日数ＳＷ(2)       TO 施術日数２.
027210**********
027220* ３部位 *
027230**********
027240     MOVE 負傷名ＳＷ(3)         TO 負傷名３       .
           IF 負傷和暦ＳＷ(3) = 5
              MOVE NC"令和"           TO 負傷和暦３
           END-IF.
027250     MOVE 負傷年ＳＷ(3)         TO 負傷年３       .
027260     MOVE 負傷月ＳＷ(3)         TO 負傷月３       .
027270     MOVE 負傷日ＳＷ(3)         TO 負傷日３       .
           IF 施術開始和暦ＳＷ(3) = 5
              MOVE NC"令和"           TO 施術開始和暦３
           END-IF.
027280     MOVE 施術開始年ＳＷ(3)     TO 施術開始年３   .
027290     MOVE 施術開始月ＳＷ(3)     TO 施術開始月３   .
027300     MOVE 施術開始日ＳＷ(3)     TO 施術開始日３   .
           IF 施術開始和暦ＳＷ(3) = 5
              MOVE NC"令和"           TO 施術期間自和暦３
           END-IF.
027310     MOVE 施術開始年ＳＷ(3)     TO 施術期間自年３ .
027320     MOVE 施術開始月ＳＷ(3)     TO 施術期間自月３ .
027330     MOVE 施術開始日ＳＷ(3)     TO 施術期間自日３ .
           IF 施術終了和暦ＳＷ(3) = 5
              MOVE NC"令和"           TO 施術終了和暦３
           END-IF.
027340     MOVE 施術終了年ＳＷ(3)     TO 施術終了年３   .
027350     MOVE 施術終了月ＳＷ(3)     TO 施術終了月３   .
027360     MOVE 施術終了日ＳＷ(3)     TO 施術終了日３   .
027370     MOVE 治癒チェックＳＷ(3)   TO 治癒チェック３ .
027380     MOVE 中止チェックＳＷ(3)   TO 中止チェック３ .
027390     MOVE 転医チェックＳＷ(3)   TO 転医チェック３ .
027400     MOVE 施術日数ＳＷ(3)       TO 施術日数３.
027410**********
027420* ４部位 *
027430**********
027440     MOVE 負傷名ＳＷ(4)         TO 負傷名４       .
           IF 負傷和暦ＳＷ(4) = 5
              MOVE NC"令和"           TO 負傷和暦４
           END-IF.
027450     MOVE 負傷年ＳＷ(4)         TO 負傷年４       .
027460     MOVE 負傷月ＳＷ(4)         TO 負傷月４       .
027470     MOVE 負傷日ＳＷ(4)         TO 負傷日４       .
           IF 施術開始和暦ＳＷ(4) = 5
              MOVE NC"令和"           TO 施術開始和暦４
           END-IF.
027480     MOVE 施術開始年ＳＷ(4)     TO 施術開始年４   .
027490     MOVE 施術開始月ＳＷ(4)     TO 施術開始月４   .
027500     MOVE 施術開始日ＳＷ(4)     TO 施術開始日４   .
           IF 施術開始和暦ＳＷ(4) = 5
              MOVE NC"令和"           TO 施術期間自和暦４
           END-IF.
027510     MOVE 施術開始年ＳＷ(4)     TO 施術期間自年４ .
027520     MOVE 施術開始月ＳＷ(4)     TO 施術期間自月４ .
027530     MOVE 施術開始日ＳＷ(4)     TO 施術期間自日４ .
           IF 施術終了和暦ＳＷ(4) = 5
              MOVE NC"令和"           TO 施術終了和暦４
           END-IF.
027540     MOVE 施術終了年ＳＷ(4)     TO 施術終了年４   .
027550     MOVE 施術終了月ＳＷ(4)     TO 施術終了月４   .
027560     MOVE 施術終了日ＳＷ(4)     TO 施術終了日４   .
027570     MOVE 治癒チェックＳＷ(4)   TO 治癒チェック４ .
027580     MOVE 中止チェックＳＷ(4)   TO 中止チェック４ .
027590     MOVE 転医チェックＳＷ(4)   TO 転医チェック４ .
027600     MOVE 施術日数ＳＷ(4)       TO 施術日数４.
027610************
027620* 負傷原因 *
027630************
027640*/負傷原因の転記
027650*/負傷原因を部位毎の欄に印刷するように変更0302
027660     PERFORM VARYING 負傷原因ＣＮＴ FROM 1 BY 1 
027670               UNTIL 負傷原因ＣＮＴ > 4
027680         IF (連印－負傷印刷行  (負傷原因ＣＮＴ) NOT = ZERO) AND
027690            (連印－原因印刷区分(負傷原因ＣＮＴ)     = 1)
027700             MOVE 負傷原因内容合成Ｗ(負傷原因ＣＮＴ)  TO 負傷原因ＳＷ(連印－負傷印刷行(負傷原因ＣＮＴ))
027710         END-IF
027720     END-PERFORM.
027730*
027740     MOVE 負傷原因１ＳＷ  (1) TO 負傷原因(1).
027750     MOVE 負傷原因２ＳＷ  (1) TO 負傷原因(2).
027760     MOVE 負傷原因３ＳＷ  (1) TO 負傷原因(3).
027770     MOVE 負傷原因４ＳＷ  (1) TO 負傷原因(4).
027780     MOVE 負傷原因５ＳＷ  (1) TO 負傷原因(5).
027790     MOVE 負傷原因６ＳＷ  (1) TO 負傷原因(6).
027800     MOVE 負傷原因７ＳＷ  (1) TO 負傷原因(7).
027810     MOVE 負傷原因８ＳＷ  (1) TO 負傷原因(8).
027820     MOVE 負傷原因９ＳＷ  (1) TO 負傷原因(9).
027830     MOVE 負傷原因１０ＳＷ(1) TO 負傷原因(10).
027830     MOVE 負傷原因１１ＳＷ(1) TO 負傷原因(11).
027840*
027850     MOVE 負傷原因１ＳＷ  (2) TO 負傷原因(12).
027860     MOVE 負傷原因２ＳＷ  (2) TO 負傷原因(13).
027870     MOVE 負傷原因３ＳＷ  (2) TO 負傷原因(14).
027880     MOVE 負傷原因４ＳＷ  (2) TO 負傷原因(15).
027890     MOVE 負傷原因５ＳＷ  (2) TO 負傷原因(16).
027900     MOVE 負傷原因６ＳＷ  (2) TO 負傷原因(17).
027910     MOVE 負傷原因７ＳＷ  (2) TO 負傷原因(18).
027920     MOVE 負傷原因８ＳＷ  (2) TO 負傷原因(19).
027930     MOVE 負傷原因９ＳＷ  (2) TO 負傷原因(20).
027940     MOVE 負傷原因１０ＳＷ(2) TO 負傷原因(21).
027940     MOVE 負傷原因１１ＳＷ(2) TO 負傷原因(22).
027950*
027960     MOVE 負傷原因１ＳＷ  (3) TO 負傷原因(23).
027970     MOVE 負傷原因２ＳＷ  (3) TO 負傷原因(24).
027980     MOVE 負傷原因３ＳＷ  (3) TO 負傷原因(25).
027990     MOVE 負傷原因４ＳＷ  (3) TO 負傷原因(26).
028000     MOVE 負傷原因５ＳＷ  (3) TO 負傷原因(27).
028010     MOVE 負傷原因６ＳＷ  (3) TO 負傷原因(28).
028020     MOVE 負傷原因７ＳＷ  (3) TO 負傷原因(29).
028030     MOVE 負傷原因８ＳＷ  (3) TO 負傷原因(30).
028040     MOVE 負傷原因９ＳＷ  (3) TO 負傷原因(31).
028050     MOVE 負傷原因１０ＳＷ(3) TO 負傷原因(32).
028050     MOVE 負傷原因１１ＳＷ(3) TO 負傷原因(33).
028060*
028070     MOVE 負傷原因１ＳＷ  (4) TO 負傷原因(34).
028080     MOVE 負傷原因２ＳＷ  (4) TO 負傷原因(35).
028090     MOVE 負傷原因３ＳＷ  (4) TO 負傷原因(36).
028100     MOVE 負傷原因４ＳＷ  (4) TO 負傷原因(37).
028110     MOVE 負傷原因５ＳＷ  (4) TO 負傷原因(38).
028120     MOVE 負傷原因６ＳＷ  (4) TO 負傷原因(39).
028130     MOVE 負傷原因７ＳＷ  (4) TO 負傷原因(40).
028140     MOVE 負傷原因８ＳＷ  (4) TO 負傷原因(41).
028150     MOVE 負傷原因９ＳＷ  (4) TO 負傷原因(42).
028160     MOVE 負傷原因１０ＳＷ(4) TO 負傷原因(43).
028160     MOVE 負傷原因１１ＳＷ(4) TO 負傷原因(44).
028170*
029060*================================================================*
029070 詳細モード負傷原因処理 SECTION.
029080*
029090*******************************************************************
029100* 連印－負傷印刷行により、負傷原因データの並び替えを行う。
029110* ( 負傷原因情報Ｗを初期化し、退避済みの負傷原因情報ＳＷから
029120*         並び替えを行い、負傷原因情報Ｗを再設定する。)
029130*******************************************************************
029140*
029150     INITIALIZE 負傷原因情報Ｗ.
029160*
029170     PERFORM VARYING 詳細ＣＮＴ FROM 1 BY 1
029180                                UNTIL ( 詳細ＣＮＴ > 5)
029190*
029200        IF ( 連印－負傷印刷行(詳細ＣＮＴ) NOT = ZERO) AND
029210           ( 連印－原因印刷区分(詳細ＣＮＴ)   = 1)
029220*
029230           MOVE 連印－負傷印刷行(詳細ＣＮＴ) TO 印刷行ＳＷ
029240*
029250           MOVE 負傷患者番号ＳＷ(詳細ＣＮＴ) TO 負傷原因患者番号Ｗ(印刷行ＳＷ)
029260           MOVE 負傷連番ＳＷ(詳細ＣＮＴ) TO 負傷原因連番Ｗ(印刷行ＳＷ)
029270        END-IF
029280*
029290     END-PERFORM.
029300*
029310*================================================================*
029320 負傷データ退避 SECTION.
029330*
029340*********************************************************************
029350* ① 負傷原因取得のため、負傷データＦから負傷原因Ｗに退避する。
029360* ② 詳細モード用  連印－負傷行印刷により、印刷位置を並び替えるため、
029370*                  負傷情報Ｗ  から負傷情報ＳＷに、
029380*                  負傷原因Ｗ  から負傷原因ＳＷに退避する。
029390*********************************************************************
029400*
029410     MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷原因患者番号Ｗ(部位ＣＮＴ).
029420     MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷原因連番Ｗ(部位ＣＮＴ).
029430*
029440     MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号ＳＷ(部位ＣＮＴ).
029450     MOVE 負傷原因連番Ｗ(部位ＣＮＴ)     TO 負傷連番ＳＷ(部位ＣＮＴ).
029460*
029470*================================================================*
029480 印刷金額セット詳細 SECTION.
029490*
029500     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
029510               UNTIL 部位ＣＮＴ > 3
029520         PERFORM 金額詳細連結項目セット
029530     END-PERFORM.
029540*================================================================*
029550 金額詳細連結項目セット  SECTION.
029560*
029600     MOVE 連印－金額和暦(部位ＣＮＴ) TO 受－施術和暦.
029610     MOVE 連印－金額年  (部位ＣＮＴ) TO 受－施術年.
029620     MOVE 連印－金額月  (部位ＣＮＴ) TO 受－施術月.
029630     MOVE 患者コードＷＲ             TO 受－患者コード.
029640     READ 受診者情報Ｆ
029650     INVALID KEY
029660         CONTINUE
029670*            /* ありえない */
029680     NOT INVALID KEY
029840*
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
               END-IF
019550         MOVE 受－施術和暦 TO レセ－施術和暦
019560         MOVE 受－施術年   TO レセ－施術年
019570         MOVE 受－施術月   TO レセ－施術月
019580         MOVE 受－患者番号 TO レセ－患者番号
019590         MOVE 受－枝番     TO レセ－枝番
019600         READ レセプトＦ
019630         NOT INVALID KEY
024670            MOVE レセ－合計     TO 費用額(部位ＣＮＴ)
                  IF 受－助成種別 = ZERO
024800               MOVE レセ－一部負担金   TO 負担額(部位ＣＮＴ)
024810               MOVE レセ－請求金額 TO 請求額(部位ＣＮＴ)
                  ELSE
024800               MOVE レセ－受給者負担額 TO 負担額(部位ＣＮＴ)
024810               COMPUTE 請求額(部位ＣＮＴ) = レセ－助成請求金額 + レセ－請求金額
                  END-IF
               END-READ
030290*
024610         MOVE 連印－金額年  (部位ＣＮＴ)    TO 金額行年       (部位ＣＮＴ)
024620         MOVE 連印－金額月  (部位ＣＮＴ)    TO 金額行月       (部位ＣＮＴ)
024630         MOVE NC"年"                        TO 金額行年表示   (部位ＣＮＴ)
030300         MOVE 連印－金額請求年(部位ＣＮＴ)  TO 提出年  (部位ＣＮＴ)
030310         MOVE 連印－金額請求月(部位ＣＮＴ)  TO 提出月  (部位ＣＮＴ)
030320         MOVE 連印－金額請求日(部位ＣＮＴ)  TO 提出日  (部位ＣＮＴ)
030330*
030340         MOVE NC"年"                        TO 金額行年表示(部位ＣＮＴ)
030350     END-READ.
030360*================================================================*
030370 累計施術回数取得 SECTION.
030380*
030390     MOVE 負－開始和暦(部位ＣＮＴ) TO 施記－施術和暦.
030400     MOVE 負－開始年  (部位ＣＮＴ) TO 施記－施術年  .
030410     MOVE 負－開始月  (部位ＣＮＴ) TO 施記－施術月  .
030420     MOVE 負－開始日  (部位ＣＮＴ) TO 施記－施術日  .
030430     MOVE 負－患者番号             TO 施記－患者番号.
030440     MOVE 負－枝番                 TO 施記－枝番    .
030450*
030460     START 施術記録Ｆ KEY IS >= 施記－患者コード
030470                                施記－施術和暦年月日
030480     END-START.
030490*
030500     IF 状態キー = "00"
030510         MOVE ZERO  TO 施術累計回数Ｗ(部位ＣＮＴ)
030520         MOVE SPACE TO 終了フラグ２
030530         PERFORM 施術記録Ｆ読込
030540         PERFORM UNTIL ( 終了フラグ２     NOT = SPACE        ) OR
030550                       ( 施記－患者番号   NOT = 負－患者番号 ) OR
030560                       ( 施記－枝番       NOT = 負－枝番     ) OR
030570                       ( 施記－施術和暦年月日 > 負－終了和暦年月日(部位ＣＮＴ))
030580*
030590             COMPUTE 施術累計回数Ｗ(部位ＣＮＴ) = 施術累計回数Ｗ(部位ＣＮＴ) + 1
030600             PERFORM 施術記録Ｆ読込
030610         END-PERFORM
030620     END-IF.
030880*
031810*================================================================*
031820 負傷原因取得 SECTION.
031830*
031840*/負傷原因を部位毎の欄に印刷するように変更0302
031850     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
031860             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
031870                OR ( 部位ＣＮＴ > 4 )
031880** 健保は 区分 01 労災は 02 自賠責は 03
031890         EVALUATE 保険種別ＷＲ
031900         WHEN 80
031910             MOVE 03 TO 負原－区分コード
031920         WHEN 70
031930             MOVE 02 TO 負原－区分コード
031940         WHEN OTHER
031950             MOVE 01 TO 負原－区分コード
031960         END-EVALUATE
031970         MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負原－患者番号
031980         MOVE 負－負傷連番    (部位ＣＮＴ) TO 負原－負傷原因連番
031990         READ 負傷原因Ｆ
032000         NOT INVALID KEY
032010             INITIALIZE 負傷原因ＷＴ
032020             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
032030             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
032040             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
032050             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
032060             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
032070             IF 印刷モードＦＷＲ NOT = 4
032080                 MOVE 部位ＣＮＴ TO 部位番号Ｗ
032090             ELSE
032100                 MOVE 連印－負傷印刷行(部位ＣＮＴ) TO 部位番号Ｗ
032110             END-IF
032120*
032130             EVALUATE 部位番号Ｗ
032140             WHEN 1
032150                MOVE "①"  TO 負傷原因ナンバーＮＷ
032160             WHEN 2
032170                MOVE "②"  TO 負傷原因ナンバーＮＷ
032180             WHEN 3
032190                MOVE "③"  TO 負傷原因ナンバーＮＷ
032200             WHEN 4
032210                MOVE "④"  TO 負傷原因ナンバーＮＷ
032220             WHEN 5
032230                MOVE "⑤"  TO 負傷原因ナンバーＮＷ
032240             WHEN OTHER
032250                CONTINUE
032260             END-EVALUATE
032270*
032352             IF 負原－負傷原因入力区分 = 1
032353                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
032354                        負傷原因１ＷＴ  DELIMITED BY SIZE
032355                        負傷原因２ＷＴ  DELIMITED BY SIZE
032356                        負傷原因３ＷＴ  DELIMITED BY SIZE
032357                        負傷原因４ＷＴ  DELIMITED BY SIZE
032358                        負傷原因５ＷＴ  DELIMITED BY SIZE
032359                        INTO 負傷原因内容合成Ｗ(部位ＣＮＴ)
032360                 END-STRING
032361             ELSE
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
                        MOVE 文字１Ｗ TO 負傷原因内容合成Ｗ(部位ＣＮＴ)
032370             END-IF
032371*
032372         END-READ
032373     END-PERFORM.
032380*
032390     PERFORM 負傷原因セット.
032400*
032410*================================================================*
032420*================================================================*
032430*================================================================*
032440******************************************************************
032450 END PROGRAM YCH6621.
032460******************************************************************
