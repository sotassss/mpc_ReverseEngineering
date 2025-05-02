000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCB662.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*         中部柔整師協会 カルテ印刷
000100*         MED = YCB660G YCB662P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2006-04-25
000130 DATE-COMPILED.          2006-04-25
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
000680     SELECT  請求先マスタ    ASSIGN      TO        SEIKYUSL
000690                             ORGANIZATION             IS  INDEXED
000700                             ACCESS MODE              IS  DYNAMIC
000710                             RECORD KEY               IS  請先－保険種別
000720                                                          請先－保険者番号
000730                             FILE STATUS              IS  状態キー
000740                             LOCK    MODE             IS  AUTOMATIC.
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
001210                             RECORD KEY               IS 負－施術和暦年月
001220                                                         負－患者コード
001230                             ALTERNATE RECORD KEY     IS 負－患者コード
001240                                                         負－施術和暦年月
001250                             FILE STATUS              IS  状態キー
001260                             LOCK        MODE         IS  AUTOMATIC.
001340     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
001350                             ORGANIZATION             IS  INDEXED
001360                             ACCESS MODE              IS  DYNAMIC
001370                             RECORD KEY               IS  負原－区分コード
001380                                                          負原－負傷原因コード
001390                             FILE STATUS              IS  状態キー
001400                             LOCK        MODE         IS  AUTOMATIC.
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
001410     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF002
001420                             SYMBOLIC    DESTINATION  IS "PRT"
001430                             FORMAT                   IS  定義体名Ｐ
001440                             GROUP                    IS  項目群名Ｐ
001450                             PROCESSING  MODE         IS  処理種別Ｐ
001460                             UNIT        CONTROL      IS  拡張制御Ｐ
001470                             FILE        STATUS       IS  通知情報Ｐ.
001480******************************************************************
001490*                      DATA DIVISION                             *
001500******************************************************************
001510 DATA                    DIVISION.
001520 FILE                    SECTION.
001530*                           ［ＲＬ＝  ３２０］
001540 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001550     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
001560*                           ［ＲＬ＝  １２８］
001570 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001580     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001590*                           ［ＲＬ＝  １２８］
001600 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
001610     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
001650*                           ［ＲＬ＝  ２５６］
001660 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001670     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
001710*                           ［ＲＬ＝  １２８］
001720 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
001730     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
001770*                           ［ＲＬ＝  ３２０］
001780 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
001790     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
001800*                           ［ＲＬ＝  ２５６］
001810 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
001820     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
001830*                           ［ＲＬ＝  １２８］
001840 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
001850     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
001890*                           ［ＲＬ＝  １２８］
001900 FD  負傷原因Ｆ         BLOCK   CONTAINS   1   RECORDS.
001910     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
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
001920*
001930 FD  印刷ファイル.
001940     COPY YCB662P        OF  XMDLIB.
001950*----------------------------------------------------------------*
001960******************************************************************
001970*                WORKING-STORAGE SECTION                         *
001980******************************************************************
001990 WORKING-STORAGE         SECTION.
002000 01 キー入力                           PIC X     VALUE SPACE.
002010 01 状態キー                           PIC X(2)  VALUE SPACE.
002020 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002030 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002040 01 継続フラグ                         PIC X(3)  VALUE SPACE.
002050 01 詳細ＣＮＴ                         PIC 9     VALUE ZERO.
002060 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002070 01 文字ＣＮＴ                         PIC 9(2)  VALUE ZERO.
002080 01 初検フラグ                         PIC X(3)  VALUE SPACE.
002090 01 カウンタ３                         PIC 9(2)  VALUE ZERO.
002100 01 ファイル名                         PIC N(2)  VALUE SPACE.
002110 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
002120 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002130 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002140 01 部位名称Ｗ                         PIC N(10) VALUE SPACE.
002150 01 負傷番号Ｗ                         PIC 9(1)  VALUE ZERO.
002160 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
002170    03 負傷番号Ｗ１                    PIC X.
002180*
002190 01 全角負傷番号Ｗ                     PIC N(9) VALUE SPACE.
002200 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
002210    03 全角負傷番号Ｗ１                PIC X(2).
002220*
002230 01 負担率Ｗ                           PIC 9(3) VALUE ZERO.
002240 01 甲乙区分Ｗ                         PIC 9    VALUE ZERO.
002250 01 本人負担率Ｗ                       PIC 9(3) VALUE ZERO.
002260 01 家族負担率Ｗ                       PIC 9(3) VALUE ZERO.
002270*
002280 01 施術和暦年月日ＣＷ.
002290   03 施術和暦年月ＣＷ.
002300     05 施術和暦ＣＷ                   PIC 9(1) VALUE ZERO.
002310     05 施術年月ＣＷ.
002320        07 施術年ＣＷ                  PIC 9(2) VALUE ZERO.
002330        07 施術月ＣＷ                  PIC 9(2) VALUE ZERO.
002340   03 施術日ＣＷ                       PIC 9(2) VALUE ZERO.
002350 01 施術西暦年Ｗ                       PIC 9(4) VALUE ZERO.
002360 01 患者西暦年Ｗ                       PIC 9(4) VALUE ZERO.
002370*
002380** バーコード区分用
002390 01 バーコード使用区分Ｗ               PIC 9 VALUE ZERO.
002400*
002410** 社保用
002420 01 接尾語区分Ｗ                       PIC 9     VALUE ZERO.
002430*
002440*
002450****************
002460* 連結項目待避 *
002470****************
002480*    ************
002490*    * 印刷キー *
002500*    ************
002510 01 対象データＷＲ.
002520    03 施術和暦年月ＷＲ.
002530       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
002540       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
002550       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
002560    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
002570    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
002580    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
002590    03 被保険者カナＷＲ                 PIC X(20) VALUE SPACE.
002600    03 患者コードＷＲ.
002610       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
002620       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
002630    03 印刷モードＦＷＲ                 PIC 9(1)  VALUE ZERO.
002640    03 提出年月日ＷＲ.
002650       05 提出年ＷＲ                    PIC 9(2)  VALUE ZERO.
002660       05 提出月ＷＲ                    PIC 9(2)  VALUE ZERO.
002670       05 提出日ＷＲ                    PIC 9(2)  VALUE ZERO.
002680    03 段数ＷＲ                         PIC 9(1)  VALUE ZERO.
002690************
002700* 料金情報 *
002710************
002720*
002730* 01 本人負担率Ｗ                        PIC 9(1) VALUE ZERO.
002740 01 印刷負担率Ｗ                        PIC 9(1)  VALUE ZERO.
002750**************
002760* 施術所情報 *
002770**************
002780* 01 施術所情報Ｗ.
002790*    03 資格取得年月日Ｗ.
002800*       05 資格取得元号Ｗ               PIC 9(1) VALUE ZERO.
002810*       05 資格取得和暦Ｗ               PIC 9(1) VALUE ZERO.
002820*       05 資格取得年Ｗ                 PIC 9(2) VALUE ZERO.
002830*       05 資格取得月Ｗ                 PIC 9(2) VALUE ZERO.
002840*       05 資格取得日Ｗ                 PIC 9(2) VALUE ZERO.
002850**************
002860* 受診者情報 *
002870**************
002880 01 受診者情報Ｗ.
002890    03 患者コードＷ.
002900       05 患者番号Ｗ                   PIC 9(6) VALUE ZERO.
002910       05 枝番Ｗ                       PIC X(1) VALUE SPACE.
002920    03 市町村番号老人Ｗ.
002930*       05 印刷市町村番号Ｗ             PIC X(8) VALUE SPACE.
002940       05 印刷市町村番号Ｗ.
002950         07 市町村番号１Ｗ             PIC X(1) VALUE SPACE.
002960         07 市町村番号２Ｗ             PIC X(1) VALUE SPACE.
002970         07 市町村番号３Ｗ             PIC X(1) VALUE SPACE.
002980         07 市町村番号４Ｗ             PIC X(1) VALUE SPACE.
002990         07 市町村番号５Ｗ             PIC X(1) VALUE SPACE.
003000         07 市町村番号６Ｗ             PIC X(1) VALUE SPACE.
003010         07 市町村番号７Ｗ             PIC X(1) VALUE SPACE.
003020         07 市町村番号８Ｗ             PIC X(1) VALUE SPACE.
003030       05 FILLER                       PIC X(2).
003040    03 受益者番号老人Ｗ.
003050*       05 印刷受益者番号Ｗ             PIC X(7) VALUE SPACE.
003060       05 印刷受益者番号Ｗ.
003070         07 受益者番号１Ｗ             PIC X(1) VALUE SPACE.
003080         07 受益者番号２Ｗ             PIC X(1) VALUE SPACE.
003090         07 受益者番号３Ｗ             PIC X(1) VALUE SPACE.
003100         07 受益者番号４Ｗ             PIC X(1) VALUE SPACE.
003110         07 受益者番号５Ｗ             PIC X(1) VALUE SPACE.
003120         07 受益者番号６Ｗ             PIC X(1) VALUE SPACE.
003130         07 受益者番号７Ｗ             PIC X(1) VALUE SPACE.
003140       03 FILLER                       PIC X(13).
003150    03 市町村番号助成Ｗ                PIC X(10) VALUE SPACE.
003160    03 受益者番号助成Ｗ                PIC X(20) VALUE SPACE.
003170    03 記号Ｗ                          PIC X(24) VALUE SPACE.
003180    03 番号Ｗ.
003190       05 印刷番号Ｗ                   PIC X(12) VALUE SPACE.
003200       05 FILLER                       PIC X(18) VALUE SPACE.
003210    03 被保険者情報Ｗ.
003220       05 被保険者カナＷ               PIC X(50) VALUE SPACE.
003230       05 被保険者氏名Ｗ               PIC X(50) VALUE SPACE.
003240*
003250       05 被保険者和暦チェックＷ.
003260          07 被保険者明治Ｗ            PIC N(1) VALUE SPACE.
003270          07 被保険者大正Ｗ            PIC N(1) VALUE SPACE.
003280          07 被保険者昭和Ｗ            PIC N(1) VALUE SPACE.
003290       05 業務チェックＷ.
003300          07 業務外チェックＷ          PIC N(1) VALUE SPACE.
003310          07 自営業チェックＷ          PIC N(1) VALUE SPACE.
003320          07 会社員チェックＷ          PIC N(1) VALUE SPACE.
003330          07 職業Ｗ                    PIC N(3) VALUE SPACE.
003340       05 性別チェックＷ.
003350          07 男チェックＷ              PIC N(1) VALUE SPACE.
003360          07 女チェックＷ              PIC N(1) VALUE SPACE.
003370       05 被保険者性別Ｗ               PIC 9(1) VALUE ZERO.
003380       05 被保険者和暦名称Ｗ           PIC N(2) VALUE SPACE.
003390       05 被保険者生年月日Ｗ.
003400          07 被保険者和暦Ｗ            PIC 9(1) VALUE ZERO.
003410          07 被保険者年Ｗ              PIC 9(2) VALUE ZERO.
003420          07 被保険者月Ｗ              PIC 9(2) VALUE ZERO.
003430          07 被保険者日Ｗ              PIC 9(2) VALUE ZERO.
003440*
003450       05 郵便番号Ｗ.
003460          07 郵便番号１Ｗ              PIC X(3) VALUE SPACE.
003470          07 郵便番号２Ｗ              PIC X(4) VALUE SPACE.
003480       05 被保険者住所Ｗ.
003490          07 印刷住所１Ｗ              PIC X(40) VALUE SPACE.
003500          07 印刷住所２Ｗ              PIC X(40) VALUE SPACE.
003510       05 患者住所Ｗ.
003520          07 印刷患者住所１Ｗ          PIC X(30) VALUE SPACE.
003530          07 印刷患者住所２Ｗ          PIC X(30) VALUE SPACE.
003540          07 印刷患者住所３Ｗ          PIC X(30) VALUE SPACE.
003550       05 電話番号Ｗ                   PIC X(15) VALUE SPACE.
003560       05 分割電話番号Ｗ.
003570          07 分割電話番号１Ｗ          PIC X(5) VALUE SPACE.
003580          07 分割電話番号２Ｗ          PIC X(5) VALUE SPACE.
003590          07 分割電話番号３Ｗ          PIC X(5) VALUE SPACE.
003600    03 資格取得年月日Ｗ.
003610       05 資格取得元号Ｗ               PIC N(2) VALUE SPACE.
003620       05 資格取得和暦Ｗ               PIC 9(1) VALUE ZERO.
003630       05 資格取得年Ｗ                 PIC 9(2) VALUE ZERO.
003640       05 資格取得月Ｗ                 PIC 9(2) VALUE ZERO.
003650       05 資格取得日Ｗ                 PIC 9(2) VALUE ZERO.
003660       05 資格和暦チェックＷ.
003670          07 資格昭和チェックＷ        PIC N(1) VALUE SPACE.
003680          07 資格平成チェックＷ        PIC N(1) VALUE SPACE.
003690    03 有効期限年月日Ｗ.
003700*       05 有効期限元号Ｗ               PIC N(2) VALUE SPACE.
003710       05 有効期限和暦Ｗ               PIC 9(1) VALUE ZERO.
003720       05 有効期限年Ｗ                 PIC 9(2) VALUE ZERO.
003730       05 有効期限月Ｗ                 PIC 9(2) VALUE ZERO.
003740       05 有効期限日Ｗ                 PIC 9(2) VALUE ZERO.
003750    03 患者情報Ｗ.
003760       05 患者カナＷ                   PIC X(50) VALUE SPACE.
003770       05 患者氏名Ｗ                   PIC X(50) VALUE SPACE.
003780       05 続柄Ｗ.
003790          07 印刷続柄Ｗ                PIC N(4) VALUE SPACE.
003800          07 FILLER                    PIC X(4) VALUE SPACE.
003810       05 和暦チェックＷ.
003820          07 明治チェックＷ            PIC N(1) VALUE SPACE.
003830          07 大正チェックＷ            PIC N(1) VALUE SPACE.
003840          07 昭和チェックＷ            PIC N(1) VALUE SPACE.
003850          07 平成チェックＷ            PIC N(1) VALUE SPACE.
003860       05 患者和暦名称Ｗ               PIC N(2) VALUE SPACE.
003870       05 患者年Ｗ                     PIC 9(2) VALUE ZERO.
003880       05 患者月Ｗ                     PIC 9(2) VALUE ZERO.
003890       05 患者日Ｗ                     PIC 9(2) VALUE ZERO.
003900       05 患者年齢Ｗ                   PIC 9(3) VALUE ZERO.
003910       05 患者性別Ｗ                   PIC N(4) VALUE SPACE.
003920       05 性別チェックＷ.
003930          07 患者男チェックＷ          PIC N(1) VALUE SPACE.
003940          07 患者女チェックＷ          PIC N(1) VALUE SPACE.
003950**************
003960* 事業所情報 *
003970**************
003980 01 事業所情報Ｗ.
003990    03 事業所名称Ｗ.
004000       05 事業所名称１Ｗ               PIC X(30) VALUE SPACE.
004010       05 事業所名称２Ｗ               PIC X(30) VALUE SPACE.
004020    03 事業所郵便番号Ｗ.
004030       05 事業所郵便番号１Ｗ           PIC X(3) VALUE SPACE.
004040       05 事業所郵便番号２Ｗ           PIC X(4) VALUE SPACE.
004050*    03 事業所住所Ｗ                    PIC X(40) VALUE SPACE.
004060    03 事業所住所Ｗ.
004070       05 事業所住所１Ｗ               PIC X(50) VALUE SPACE.
004080       05 事業所住所２Ｗ               PIC X(50) VALUE SPACE.
004090**************
004100* 請求先情報 *
004110**************
004120 01 請求先情報Ｗ.
004130    03 保険者番号Ｗ.
004140       05 印刷保険者番号Ｗ             PIC X(8) VALUE SPACE.
004150       05 FILLER                       PIC X(2).
004160    03 請求先名称Ｗ.
004170       05 請求先名称１Ｗ               PIC X(30) VALUE SPACE.
004180       05 請求先名称２Ｗ               PIC X(30) VALUE SPACE.
004190    03 支部名Ｗ.
004200       05 支部名１Ｗ                   PIC X(30) VALUE SPACE.
004210       05 支部名２Ｗ                   PIC X(30) VALUE SPACE.
004220*       05 支部名１Ｗ   PIC X(1) OCCURS 10.
004230    03 印刷支部名Ｗ                    PIC X(10) VALUE SPACE.
004240    03 保険者呼名Ｗ.
004250       05 印刷保険者呼名Ｗ             PIC X(14)  VALUE SPACE.
004260    03 請求先郵便番号Ｗ.
004270       05 請求先郵便番号１Ｗ           PIC X(3) VALUE SPACE.
004280       05 請求先郵便番号２Ｗ           PIC X(4) VALUE SPACE.
004290    03 請求先住所Ｗ.
004300       05 請求先住所１Ｗ               PIC X(30) VALUE SPACE.
004310       05 請求先住所２Ｗ               PIC X(30) VALUE SPACE.
004320       05 請求先住所３Ｗ               PIC X(30) VALUE SPACE.
004330*    03 請求先住所１Ｗ.
004340*       05 印刷請求先住所１Ｗ           PIC X(40) VALUE SPACE.
004350*    03 請求先住所２Ｗ.
004360*       05 印刷請求先住所２Ｗ           PIC X(40) VALUE SPACE.
004370    03 保険種別チェックＷ.
004380       05 国保チェックＷ               PIC N(1) VALUE SPACE.
004390       05 社保チェックＷ               PIC N(1) VALUE SPACE.
004400       05 組合チェックＷ               PIC N(1) VALUE SPACE.
004410       05 共済チェックＷ               PIC N(1) VALUE SPACE.
004420       05 自衛官チェックＷ             PIC N(1) VALUE SPACE.
004430       05 日雇いチェックＷ             PIC N(1) VALUE SPACE.
004440       05 船員チェックＷ               PIC N(1) VALUE SPACE.
004450       05 退職チェックＷ               PIC N(1) VALUE SPACE.
004460       05 労災チェックＷ               PIC N(1) VALUE SPACE.
004470       05 自賠責チェックＷ             PIC N(1) VALUE SPACE.
004480       05 自費チェックＷ               PIC N(1) VALUE SPACE.
004490    03 ２７老人チェックＷ              PIC N(1) VALUE SPACE.
004500    03 助成チェックＷ                  PIC N(1) VALUE SPACE.
004510    03 助成種別チェックＷ.
004520       05 ４１老人チェックＷ           PIC N(1) VALUE SPACE.
004530       05 生保チェックＷ               PIC N(1) VALUE SPACE.
004540       05 乳幼児チェックＷ             PIC N(1) VALUE SPACE.
004550       05 障害チェックＷ               PIC N(1) VALUE SPACE.
004560       05 母子チェックＷ               PIC N(1) VALUE SPACE.
004570       05 被爆チェックＷ               PIC N(1) VALUE SPACE.
004580    03 負担割合.
004590       05 ０割チェックＷ               PIC N(1) VALUE SPACE.
004600       05 １割チェックＷ               PIC N(1) VALUE SPACE.
004610       05 ２割チェックＷ               PIC N(1) VALUE SPACE.
004620       05 ３割チェックＷ               PIC N(1) VALUE SPACE.
004630****************
004640* 負傷データＦ *
004650****************
004660 01 負傷情報Ｗ.
004670    03 部位数Ｗ                        PIC 9(1) VALUE ZERO.
004680    03 往療距離Ｗ                      PIC 99V9 VALUE ZERO.
004690    03 負傷データ情報Ｗ  OCCURS   9.
004700       05 会部位コードＷ               PIC X(4) VALUE SPACE.
004710       05 会ＯＣＲコードＷ             PIC X(4) VALUE ZERO.
004720       05 部位ＣＮＴＷ                 PIC 9(1) VALUE ZERO.
004730       05 負傷種別Ｗ                   PIC 9(2) VALUE ZERO.
004740       05 部位Ｗ                       PIC 9(2) VALUE ZERO.
004750       05 左右区分Ｗ                   PIC 9(1) VALUE ZERO.
004760       05 負傷位置番号Ｗ               PIC 9(2) VALUE ZERO.
004770       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
004780       05 負傷年月日Ｗ.
004790          07 負傷和暦Ｗ                PIC 9(1) VALUE ZERO.
004800          07 負傷年Ｗ                  PIC 9(2) VALUE ZERO.
004810          07 負傷月Ｗ                  PIC 9(2) VALUE ZERO.
004820          07 負傷日Ｗ                  PIC 9(2) VALUE ZERO.
004830          07 負傷年月日区切Ｗ          PIC X(1) VALUE SPACE.
004840       05 施術開始年月日Ｗ.
004850          07 施術開始和暦Ｗ            PIC 9(1) VALUE ZERO.
004860          07 施術開始年Ｗ              PIC 9(2) VALUE ZERO.
004870          07 施術開始月Ｗ              PIC 9(2) VALUE ZERO.
004880          07 施術開始日Ｗ              PIC 9(2) VALUE ZERO.
004890          07 施開年月日区切Ｗ          PIC X(1) VALUE ZERO.
004900       05 施術終了年月日Ｗ.
004910          07 施術終了和暦Ｗ            PIC 9(1) VALUE ZERO.
004920          07 施術終了年Ｗ              PIC 9(2) VALUE ZERO.
004930          07 施術終了月Ｗ              PIC 9(2) VALUE ZERO.
004940          07 施術終了日Ｗ              PIC 9(2) VALUE ZERO.
004950          07 施終年月日区切Ｗ          PIC X(1) VALUE SPACE.
004960       05 開始年月日取得フラグ         PIC X(3) VALUE SPACE.
004970       05 転帰チェックＷ.
004980          07 治癒チェックＷ            PIC N(1) VALUE SPACE.
004990          07 中止チェックＷ            PIC N(1) VALUE SPACE.
005000          07 転医チェックＷ            PIC N(1) VALUE SPACE.
005010       05 転帰名称Ｗ                   PIC N(4) VALUE SPACE.
005020       05 施術累計回数Ｗ               PIC 9(4)  VALUE ZERO.
005030************
005040* 負傷原因 *
005050************
005060 01 カウンタ                           PIC 9(2)  VALUE ZERO.
005070 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
005080* 負傷原因用
005090 01 負傷原因ＷＴ.
005100    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
005110    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
005120    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
005130    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
005140    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
005150    03 負傷原因ナンバーＷＴ.
005160       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
005170    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
005180 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
005190 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
005200 01 負傷原因ＴＢＬ.
005210    03 負傷原因コードＴＢＬ            OCCURS 9.
005220       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
005230       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
005240       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
005250*/印刷する負傷原因のワーク
005260 01 負傷原因内容Ｗ.
005270    03 負傷原因内容合成Ｗ              PIC X(316) OCCURS 9 VALUE SPACE.
005280    03 負傷原因内容分解ＸＷ.
005290       05 負傷原因内容１ＸＷ           PIC X(70)  VALUE SPACE.
005300       05 負傷原因内容２ＸＷ           PIC X(70)  VALUE SPACE.
005310       05 負傷原因内容３ＸＷ           PIC X(70)  VALUE SPACE.
005310       05 負傷原因内容４ＸＷ           PIC X(70)  VALUE SPACE.
005310       05 負傷原因内容５ＸＷ           PIC X(36)  VALUE SPACE.
005320*
005330 01 負傷原因Ｗテーブル.
005340    03 負傷原因Ｗ                      PIC X(70) OCCURS 45 VALUE SPACE.
005350*
      */負傷原因の詳細印刷/0610
       01 負傷原因Ｗテーブル２.
          03 負傷原因Ｗ２                    PIC X(70) OCCURS 27 VALUE SPACE.
005360** 負傷原因印刷区分用
005370 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
005380*
005390**********
005400* 合計行 *
005410**********
005420 01 合計行Ｗ.
005430    03 請求開始日Ｗ                    PIC 9(2) VALUE ZERO.
005440    03 請求終了日Ｗ                    PIC 9(2) VALUE ZERO.
005450    03 対象西暦Ｗ                      PIC 9(4) VALUE ZERO.
005460    03 商Ｗ                            PIC 9(3) VALUE ZERO.
005470    03 余Ｗ                            PIC 9(3) VALUE ZERO.
005480*
005490 01 負傷原因情報Ｗ.
005500    03 負傷原因コードＷ  OCCURS   9.
005510       09 負傷原因患者番号Ｗ           PIC 9(6) VALUE ZERO.
005520       09 負傷原因連番Ｗ               PIC 9(4) VALUE ZERO.
005530****************
005540* 詳細用ワーク *
005550****************
005560* 連印－負傷印刷行用ワーク
005570 01 印刷行ＳＷ                           PIC 9(1) VALUE ZERO.
005580*
005590 01 負傷原因情報ＳＷ.
005600    03 負傷原因コードＳＷ  OCCURS   9.
005610       09 負傷患者番号ＳＷ               PIC 9(6) VALUE ZERO.
005620       09 負傷連番ＳＷ                   PIC 9(4) VALUE ZERO.
005630*
005640 01 負傷情報ＳＷ.
005650    03 負傷データＳＷ  OCCURS   9.
005660       05 会部位コードＳＷ               PIC X(4)  VALUE SPACE.
005670       05 部位ＣＮＴＳＷ                 PIC 9(1)  VALUE ZERO.
005680       05 負傷種別ＳＷ                   PIC 9(2)  VALUE ZERO.
005690       05 部位ＳＷ                       PIC 9(2)  VALUE ZERO.
005700       05 左右区分ＳＷ                   PIC 9(1)  VALUE ZERO.
005710       05 負傷位置番号ＳＷ               PIC 9(2)  VALUE ZERO.
005720       05 負傷名ＳＷ                     PIC N(18) VALUE SPACE.
005730       05 負傷年月日ＳＷ.
005740          07 負傷和暦ＳＷ                PIC 9(1)  VALUE ZERO.
005750          07 負傷年ＳＷ                  PIC 9(2)  VALUE ZERO.
005760          07 負傷月ＳＷ                  PIC 9(2)  VALUE ZERO.
005770          07 負傷日ＳＷ                  PIC 9(2)  VALUE ZERO.
005780          07 負傷年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
005790       05 施術開始年月日ＳＷ.
005800          07 施術開始和暦ＳＷ            PIC 9(1)  VALUE ZERO.
005810          07 施術開始年ＳＷ              PIC 9(2)  VALUE ZERO.
005820          07 施術開始月ＳＷ              PIC 9(2)  VALUE ZERO.
005830          07 施術開始日ＳＷ              PIC 9(2)  VALUE ZERO.
005840          07 施開年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
005850       05 施術終了年月日ＳＷ.
005860          07 施術終了和暦ＳＷ            PIC 9(1)  VALUE ZERO.
005870          07 施術終了年ＳＷ              PIC 9(2)  VALUE ZERO.
005880          07 施術終了月ＳＷ              PIC 9(2)  VALUE ZERO.
005890          07 施術終了日ＳＷ              PIC 9(2)  VALUE ZERO.
005900          07 施終年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
005910       05 開始年月日取得フラグＳ         PIC X(3)  VALUE SPACE.
005920       05 転帰ＳＷ                       PIC N(4)  VALUE SPACE.
005930       05 転帰チェックＳＷ.
005940          07 治癒チェックＳＷ            PIC N(1)  VALUE SPACE.
005950          07 中止チェックＳＷ            PIC N(1)  VALUE SPACE.
005960          07 転医チェックＳＷ            PIC N(1)  VALUE SPACE.
005970       05 転帰名称ＳＷ                   PIC N(4)  VALUE SPACE.
005980       05 施術累計回数ＳＷ               PIC 9(4)  VALUE ZERO.
005990*
006000 01 印刷制御.
006010     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
006020     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
006030     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
006040     03 拡張制御Ｐ.
006050         05 端末制御Ｐ.
006060             07 移動方向Ｐ             PIC X(1).
006070             07 移動行数Ｐ             PIC 9(3).
006080         05 詳細制御Ｐ                 PIC X(2).
006090     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
006100     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
006110*
006120 01 計算機西暦年Ｗ                     PIC 9(2).
006130* 日付ＷＯＲＫ
006140 01 和暦終了年Ｗ                       PIC 9(4).
006150 01 計算機西暦.
006160    03 計算機西暦年                    PIC 9(4).
006170    03 計算機西暦月日                  PIC 9(4).
006180 01 計算機西暦Ｒ REDEFINES 計算機西暦.
006190    03 計算機世紀                      PIC 9(2).
006200    03 計算機日付                      PIC 9(6).
006210    03 計算機日付Ｒ REDEFINES 計算機日付.
006220       05 計算機年月                   PIC 9(4).
006230       05 計算機年月Ｒ REDEFINES 計算機年月.
006240         07 計算機年                   PIC 9(2).
006250         07 計算機月                   PIC 9(2).
006260       05 計算機日                     PIC 9(2).
006270*
006280*/年月毎持つ保険者データのワーク
006290 01 保負担率区分Ｗ                     PIC 9    VALUE ZERO.
006300 01 保本人負担率Ｗ                     PIC 9(3) VALUE ZERO.
006310 01 保家族負担率Ｗ                     PIC 9(3) VALUE ZERO.
006320 01 保本人負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
006330 01 保家族負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
006340*
006350 01 比較和暦年月Ｗ.
006360    03 比較和暦Ｗ                      PIC 9    VALUE ZERO.
006370    03 比較年Ｗ                        PIC 9(2) VALUE ZERO.
006380    03 比較月Ｗ                        PIC 9(2) VALUE ZERO.
006390*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
006400******************************************************************
006410*                          連結項目                              *
006420******************************************************************
006430*
006440**********************
006450* メッセージ表示キー *
006460**********************
006470 01 連メ－キー IS EXTERNAL.
006480    03  連メ－メッセージ                 PIC N(20).
006490*
006500****************
006510* 連結項目待避 *
006520****************
006530*    ************
006540*    * 料金情報 *
006550*    ************
006560*    月毎の料金
006570***********************
006580 01 連施－料金１ IS EXTERNAL.
006590   03 連施－本人負担率                 PIC 9(3).
006600************
006610* 印刷キー *
006620************
002880 01 連印－対象データ IS EXTERNAL.
002890    03 連印－施術和暦年月.
002900       05 連印－施術和暦                  PIC 9(1).
002910       05 連印－施術年                    PIC 9(2).
002920       05 連印－施術月                    PIC 9(2).
002930    03 連印－保険種別                     PIC 9(2).
002940    03 連印－保険者番号                   PIC X(10).
002950    03 連印－本人家族区分                 PIC 9(1).
002960    03 連印－被保険者カナ                 PIC X(20).
002970    03 連印－患者コード.
002980       05 連印－患者番号                  PIC 9(6).
002990       05 連印－枝番                      PIC X(1).
003000    03 連印－印刷モードＦ                 PIC 9(1).
003020*/詳細
003030    03 連印－保険証印刷区分               PIC 9(1).
003040    03 連印－負傷詳細 OCCURS 7.
003050       05 連印－負傷印刷行                PIC 9(1).
003060       05 連印－部位印刷区分              PIC 9(1).
003070       05 連印－転帰印刷区分              PIC 9(1).
003080       05 連印－原因印刷区分              PIC 9(1).
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
007300*
       01 連入－表示フラグ６６０ IS EXTERNAL GLOBAL.
          03 連入－プレビュー区分               PIC 9(1).
007310* 負担率取得用14/10～
007320 01 連率－負担率取得キー IS EXTERNAL.
007330    03 連率－施術和暦年月.
007340       05 連率－施術和暦               PIC 9.
007350       05 連率－施術年月.
007360          07 連率－施術年              PIC 9(2).
007370          07 連率－施術月              PIC 9(2).
007380    03 連率－患者コード.
007390       05 連率－患者番号               PIC 9(6).
007400       05 連率－枝番                   PIC X.
007410    03 連率－実際負担率                PIC 9(3).
007420    03 連率－実際本体負担率            PIC 9(3).
007430    03 連率－健保負担率                PIC 9(3).
007440    03 連率－２７老負担率              PIC 9(3).
007450    03 連率－助成負担率                PIC 9(3).
007460    03 連率－特別用負担率              PIC 9(3).
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
007470*
      *
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
007480******************************************************************
007490*                      PROCEDURE  DIVISION                       *
007500******************************************************************
007510 PROCEDURE               DIVISION.
007520************
007530*           *
007540* 初期処理   *
007550*           *
007560************
002570     PERFORM プリンタファイル作成.
007570     PERFORM 初期化.
007580************
007590*           *
007600* 主処理     *
007610*           *
007620************
007630* 印刷
007640     PERFORM 連結項目待避.
007650     PERFORM 印刷セット
007660     PERFORM 印刷処理
007670************
007680*           *
007690* 終了処理   *
007700*           *
007710************
007720     PERFORM 終了処理.
007730     EXIT PROGRAM.
007740*
007750*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YCB662"              TO Ｈ連ＰＲＴＦ－帳票プログラム名.
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
007760*================================================================*
007770 初期化 SECTION.
007780*
007790     PERFORM ファイルオープン.
007800*    /* 現在日付取得 */
007810     ACCEPT 計算機日付 FROM DATE.
007820*    /* 1980～2079年の間で設定 */
007830     IF 計算機年 > 80
007840         MOVE 19 TO 計算機世紀
007850     ELSE
007860         MOVE 20 TO 計算機世紀
007870     END-IF.
007880     PERFORM カレント元号取得.
007890     PERFORM 和暦終了年取得.
007900     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
007910*================================================================*
007920 カレント元号取得 SECTION.
007930*
007940     MOVE ZEROS TO 制－制御区分.
007950     READ 制御情報マスタ
007960     NOT INVALID KEY
007970         MOVE 制－カレント元号               TO カレント元号Ｗ
007980         MOVE 制－カルテ負傷原因印刷区分     TO 負傷原因印刷区分Ｗ
007990         MOVE 制－バーコード２３２Ｃ使用区分 TO バーコード使用区分Ｗ
008000     END-READ.
008010*
008020*================================================================*
008030 和暦終了年取得 SECTION.
008040*
008050     MOVE カレント元号Ｗ TO 元－元号区分.
008060     READ 元号マスタ
008070     INVALID KEY
008080         DISPLAY NC"指定和暦が登録されていません" UPON CONS
008090         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008100                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
008110         ACCEPT  キー入力 FROM CONS
008120         PERFORM 終了処理
008130         EXIT PROGRAM
008140     NOT INVALID KEY
008150         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
008160         MOVE 前和暦Ｗ TO 元－元号区分
008170         READ 元号マスタ
008180         INVALID KEY
008190             DISPLAY NC"指定和暦が登録されていません" UPON CONS
008200             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008210                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
008220             ACCEPT  キー入力 FROM CONS
008230             PERFORM 終了処理
008240             EXIT PROGRAM
008250         NOT INVALID KEY
008260             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
008270         END-READ
008280     END-READ.
008290*
008300*================================================================*
008310 連結項目待避 SECTION.
008320*
008330     INITIALIZE 対象データＷＲ.
008340     MOVE 連印－施術和暦      TO 施術和暦ＷＲ.
008350     MOVE 連印－施術年        TO 施術年ＷＲ.
008360     MOVE 連印－施術月        TO 施術月ＷＲ.
008370     MOVE 連印－保険種別      TO 保険種別ＷＲ.
008380     MOVE 連印－保険者番号    TO 保険者番号ＷＲ.
008390     MOVE 連印－本人家族区分  TO 本人家族区分ＷＲ.
008400     MOVE 連印－被保険者カナ  TO 被保険者カナＷＲ.
008410     MOVE 連印－患者番号      TO 患者番号ＷＲ.
008420     MOVE 連印－枝番          TO 枝番ＷＲ.
008430     MOVE 連印－印刷モードＦ  TO 印刷モードＦＷＲ.
008480*================================================================*
008490 ファイルオープン SECTION.
008500*
008510     OPEN INPUT   保険者マスタ
008520         MOVE NC"保険者" TO ファイル名.
008530         PERFORM オープンチェック.
008540     OPEN INPUT   元号マスタ
008550         MOVE NC"元号" TO ファイル名.
008560         PERFORM オープンチェック.
008570     OPEN INPUT   名称マスタ
008580         MOVE NC"名称" TO ファイル名.
008590         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
008630     OPEN INPUT   制御情報マスタ
008640         MOVE NC"制御情報" TO ファイル名.
008650         PERFORM オープンチェック.
008690     OPEN INPUT   請求先マスタ
008700         MOVE NC"請先" TO ファイル名.
008710         PERFORM オープンチェック.
008750     OPEN INPUT   受診者情報Ｆ.
008760         MOVE NC"受情" TO ファイル名.
008770         PERFORM オープンチェック.
008780     OPEN INPUT   施術記録Ｆ.
008790         MOVE NC"施記Ｆ" TO ファイル名.
008800         PERFORM オープンチェック.
008810     OPEN INPUT   負傷データＦ.
008820         MOVE NC"負傷" TO ファイル名.
008830         PERFORM オープンチェック.
008870     OPEN INPUT  負傷原因Ｆ.
008880         MOVE NC"負傷原因" TO ファイル名.
008890         PERFORM オープンチェック.
007860     OPEN INPUT 市町村マスタ.
007870         MOVE NC"市町村" TO ファイル名.
007880         PERFORM オープンチェック.
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
008900     OPEN I-O   印刷ファイル
008910         MOVE NC"印刷" TO ファイル名.
008920         PERFORM エラー処理Ｐ.
008930*================================================================*
008940 オープンチェック SECTION.
008950*
008960     IF 状態キー  NOT =  "00"
008970         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
008980         DISPLAY NC"状態キー：" 状態キー         UPON CONS
008990         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
009000                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
009010         ACCEPT  キー入力 FROM CONS
009020         PERFORM ファイル閉鎖
009030         EXIT PROGRAM.
009040*================================================================*
009050 ファイル閉鎖 SECTION.
009060*
009070     CLOSE 印刷ファイル  元号マスタ       保険者マスタ
009080           レセプトＦ    制御情報マスタ   請求先マスタ
009090           名称マスタ    受診者情報Ｆ     生保情報Ｆ
009100           負傷データＦ  施術記録Ｆ       労災情報Ｆ
009110           負傷原因Ｆ    市町村マスタ     事業所マスタ
                 自賠責情報Ｆ  保険会社マスタ.
009120*================================================================*
009130 終了処理 SECTION.
009140*
009150     PERFORM ファイル閉鎖.
009160*================================================================*
009170 印刷セット SECTION.
009180*
009190     EVALUATE 印刷モードＦＷＲ
009200     WHEN 0
009210         PERFORM 印刷セット１
009220     WHEN 1
009230         PERFORM 印刷セット２
009240     WHEN 2
009250         PERFORM 印刷セット３
009280*/詳細モード用
009290     WHEN 4
009300         PERFORM 印刷セット５
009310     END-EVALUATE.
009320*
009330*================================================================*
009340 印刷セット１ SECTION.
009350*
009360     INITIALIZE YCB662P.
009370     INITIALIZE 受診者情報Ｗ.
009380     INITIALIZE 事業所情報Ｗ.
009390     INITIALIZE 請求先情報Ｗ.
009400     INITIALIZE 負傷情報Ｗ.
009410     PERFORM 負担率取得.
009420     PERFORM 請求先情報取得.
009430     PERFORM 受診者情報取得.
009440     PERFORM 負傷データ取得.
009450*
009460     IF 負傷原因印刷区分Ｗ  NOT = 1 
009470        PERFORM 負傷原因取得
009480     END-IF.
009490*
009500     PERFORM 印刷上部セット.
009510     PERFORM 印刷下部セット.
009520*================================================================*
009530 印刷セット２ SECTION.
009540*
009550     INITIALIZE YCB662P.
009560     INITIALIZE 受診者情報Ｗ.
009570     INITIALIZE 事業所情報Ｗ.
009580     INITIALIZE 請求先情報Ｗ.
009590     INITIALIZE 負傷情報Ｗ.
009600     PERFORM 負担率取得.
009610     PERFORM 請求先情報取得.
009620     PERFORM 受診者情報取得.
009630     PERFORM 印刷上部セット.
009640*================================================================*
009650 印刷セット３ SECTION.
009660*
009670     INITIALIZE YCB662P.
009680     INITIALIZE 受診者情報Ｗ.
009690     INITIALIZE 事業所情報Ｗ.
009700     INITIALIZE 請求先情報Ｗ.
009710     INITIALIZE 負傷情報Ｗ.
009620     PERFORM 受診者情報取得.
009720     PERFORM 負傷データ取得.
009730*
009740     IF 負傷原因印刷区分Ｗ  NOT = 1 
009750        PERFORM 負傷原因取得
009760     END-IF.
009770*
009780     PERFORM 印刷下部セット.
009930*================================================================*
009940 印刷セット５ SECTION.
009950*
009960*/詳細モード用
009970     MOVE SPACE TO YCB662P.
009980     INITIALIZE YCB662P.
009990*     MOVE "X" TO EDIT-MODE OF 患者コード.
010000     INITIALIZE 受診者情報Ｗ.
010010     INITIALIZE 事業所情報Ｗ.
010020     INITIALIZE 請求先情報Ｗ.
010030     INITIALIZE 負傷情報Ｗ.
010040     INITIALIZE 負傷原因情報Ｗ.
010050     PERFORM 負担率取得.
010060     PERFORM 請求先情報取得.
010070     PERFORM 受診者情報取得.
010080     PERFORM 負傷データ取得.
010090*
010100     IF 負傷原因印刷区分Ｗ  NOT = 1
010110        PERFORM 詳細モード負傷原因処理
010120        PERFORM 負傷原因取得
010130     END-IF.
      */負傷原因の詳細印刷/0610
           PERFORM 原因行セット.
010140*
010150     IF 連印－保険証印刷区分 = 1
010160         PERFORM 印刷上部セット
010170     END-IF.
010180*
010190     PERFORM 詳細モード負傷処理.
010200     PERFORM 印刷下部セット.
010220*
010230*================================================================*
010240 印刷上部セット SECTION.
010250*
010260     IF バーコード使用区分Ｗ NOT = ZERO
010270        MOVE 患者コードＷ   TO 患者コード
010280        MOVE SPACE          TO EDIT-MODE OF 患者コード
           ELSE
               MOVE "X" TO EDIT-MODE OF 患者コード
010290     END-IF.
010300     MOVE 患者番号Ｗ        TO 患者番号.
010310     MOVE 枝番Ｗ            TO 枝番.
010320*     MOVE 往療距離Ｗ        TO 往療距離.
010330**************************
010340* 被保険者証情報セット   *
010350**************************
010360     IF 記号Ｗ(1:2) = "＊" 
010370        MOVE  SPACE    TO  記号
010380     ELSE
010390        MOVE 記号Ｗ    TO  記号
010400     END-IF.
010410     IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
010420        ( 印刷番号Ｗ(1:2) = "＊" )
010430        MOVE  SPACE      TO  番号
010440     ELSE
010450        MOVE 印刷番号Ｗ  TO  番号
010460     END-IF.
010470************************
010480* 被保険者情報セット   *
010490************************
010500     MOVE 被保険者カナＷ             TO 被保険者カナ.
010510     MOVE 被保険者氏名Ｗ             TO 被保険者氏名.
010520     MOVE 郵便番号１Ｗ               TO 郵便番号１.
010530     MOVE 郵便番号２Ｗ               TO 郵便番号２.
010540     MOVE "-"                        TO 被保険者郵便区切.
010550*     MOVE 印刷住所１Ｗ               TO 住所１.
010560*     MOVE 印刷住所２Ｗ               TO 住所２.
010570     MOVE 印刷患者住所１Ｗ           TO 住所１.
010580     MOVE 印刷患者住所２Ｗ           TO 住所２.
010590     MOVE 印刷患者住所３Ｗ           TO 住所３.
010600     MOVE 電話番号Ｗ                 TO 電話番号.
010610*     MOVE 分割電話番号１Ｗ           TO 電話番号１.
010620*     MOVE 分割電話番号２Ｗ           TO 電話番号２.
010630*     MOVE 分割電話番号３Ｗ           TO 電話番号３.
010640     IF (資格取得年Ｗ NOT = ZERO) AND
010650        (資格取得月Ｗ NOT = ZERO) AND
010660        (資格取得日Ｗ NOT = ZERO)
010670*         MOVE 資格取得元号Ｗ             TO 資格取得元号
010680         MOVE 資格昭和チェックＷ         TO 資格昭和チェック
010690         MOVE 資格平成チェックＷ         TO 資格平成チェック
010700         MOVE 資格取得年Ｗ               TO 資格取得年
010710         MOVE 資格取得月Ｗ               TO 資格取得月
010720         MOVE 資格取得日Ｗ               TO 資格取得日
010730     END-IF.
010740*/有効期限
010750*     MOVE 有効期限年Ｗ               TO 有効年.
010760*     MOVE 有効期限月Ｗ               TO 有効月.
010770*     MOVE 有効期限日Ｗ               TO 有効日.
010780*     MOVE 業務外チェックＷ           TO 業務外チェック.
010790*     MOVE 自営業チェックＷ           TO 自営業チェック.
010800*     MOVE 会社員チェックＷ           TO 会社員チェック.
010810*     MOVE 職業Ｗ                     TO 職業.
010820*     MOVE 男チェックＷ               TO 男チェック.
010830*     MOVE 女チェックＷ               TO 女チェック.
010840*     MOVE 被保険者明治Ｗ             TO 被保険者明治.
010850*     MOVE 被保険者大正Ｗ             TO 被保険者大正.
010860*     MOVE 被保険者昭和Ｗ             TO 被保険者昭和.
010870*     MOVE 被保険者和暦名称Ｗ         TO 被保険者和暦名称.
010880*     MOVE 被保険者年Ｗ               TO 被保険者年.
010890*     MOVE 被保険者月Ｗ               TO 被保険者月.
010900*     MOVE 被保険者日Ｗ               TO 被保険者日.
010910********************
010920* 患者情報セット   *
010930********************
010940     MOVE 患者カナＷ                 TO 患者カナ.
010950     MOVE 患者氏名Ｗ                 TO 患者氏名.
010960     MOVE 患者男チェックＷ           TO 患者男チェック.
010970     MOVE 患者女チェックＷ           TO 患者女チェック.
010980     MOVE 明治チェックＷ             TO 明治チェック.
010990     MOVE 大正チェックＷ             TO 大正チェック.
011000     MOVE 昭和チェックＷ             TO 昭和チェック.
011010     MOVE 平成チェックＷ             TO 平成チェック.
011020*     MOVE 患者和暦名称Ｗ             TO 患者和暦名称.
011030     MOVE 患者年Ｗ                   TO 患者年.
011040     MOVE 患者月Ｗ                   TO 患者月.
011050     MOVE 患者日Ｗ                   TO 患者日.
011060*     MOVE 患者年齢Ｗ                 TO 患者年齢.
011070     MOVE 印刷続柄Ｗ                 TO 続柄.
011080********************
011090* 事業所情報セット *
011100********************
011110     MOVE 事業所名称１Ｗ             TO 事業所名称１.
011120     MOVE 事業所名称２Ｗ             TO 事業所名称２.
011130     MOVE 事業所郵便番号１Ｗ         TO 事業所郵便番号１.
011140     MOVE 事業所郵便番号２Ｗ         TO 事業所郵便番号２.
011150     MOVE 事業所住所１Ｗ             TO 事業所住所１.
011160     MOVE 事業所住所２Ｗ             TO 事業所住所２.
011170************************************
011180* 都道府県・健康保険組合情報セット *
011190************************************
011200     MOVE 印刷保険者番号Ｗ           TO 保険者番号.
011210     MOVE 請求先名称１Ｗ             TO 請求先名称１.
011220     MOVE 請求先名称２Ｗ             TO 請求先名称２.
011230     MOVE 支部名１Ｗ                 TO 支部部署名１.
011240     MOVE 支部名２Ｗ                 TO 支部部署名２.
011250     MOVE 請求先郵便番号１Ｗ         TO 請求先郵便番号１.
011260     MOVE 請求先郵便番号２Ｗ         TO 請求先郵便番号２.
011270     IF (請求先郵便番号１Ｗ NOT = SPACE) OR
011280        (請求先郵便番号２Ｗ NOT = SPACE)
011290         MOVE "-"                    TO 保険者郵便区切
011300     END-IF.
011310     MOVE 請求先住所１Ｗ             TO 請求先住所１.
011320     MOVE 請求先住所２Ｗ             TO 請求先住所２.
011330     MOVE 請求先住所３Ｗ             TO 請求先住所３.
011340*     MOVE 国保チェックＷ             TO 国保チェック.
011350*     MOVE 社保チェックＷ             TO 社保チェック.
011360*     MOVE 組合チェックＷ             TO 組合チェック.
011370*     MOVE 共済チェックＷ             TO 共済チェック.
011380*     MOVE 自衛官チェックＷ           TO 自衛官チェック.
011390*     MOVE 日雇いチェックＷ           TO 日雇チェック.
011400*     MOVE 船員チェックＷ             TO 船員チェック.
011410*     MOVE 退職チェックＷ             TO 退職チェック.
011420*     MOVE 自費チェックＷ             TO 自費チェック.
011430*
011440*     MOVE ２７老人チェックＷ         TO ２７老人チェック.
011450*     MOVE 助成チェックＷ             TO 公費負担チェック.
011460*     MOVE 労災チェックＷ             TO 労災チェック.
011470*     MOVE 自賠責チェックＷ           TO 自賠責チェック.
011480*     MOVE 生保チェックＷ             TO 生保チェック  .
011490*     MOVE ４１老人チェックＷ         TO ４１老人チェック.
011500*     MOVE 母子チェックＷ             TO 母子チェック  .
011510*     MOVE 障害チェックＷ             TO 障害チェック  .
011520*     MOVE 被爆チェックＷ             TO 被爆チェック  .
011530*     MOVE 乳幼児チェックＷ           TO 乳幼児チェック.
011540*
011550*/負担割合
011560*      MOVE ０割チェックＷ  TO ０割チェック.
011570*      MOVE １割チェックＷ  TO １割チェック.
011580*      MOVE ２割チェックＷ  TO ２割チェック.
011590*      MOVE ３割チェックＷ  TO ３割チェック.
011600*/市町村番号
011610*     MOVE 市町村番号老人Ｗ TO 市町村番号老人.
011620*     MOVE 受益者番号老人Ｗ TO 受益者番号老人.
011630*     MOVE 市町村番号助成Ｗ TO 市町村番号助成.
011640*     MOVE 受益者番号助成Ｗ TO 受益者番号助成.
011650*================================================================*
011660 印刷下部セット SECTION.
011670*
011680********************
011690* 負傷データセット *
011700********************
011710* １部位 *
011720**********
011730*     MOVE 会ＯＣＲコードＷ(1) TO 負傷コード１.
011740     MOVE 負傷名Ｗ(1)         TO 負傷名１.
011750     MOVE 負傷年Ｗ(1)         TO 負傷年１.
011760     MOVE 負傷月Ｗ(1)         TO 負傷月１.
011770     MOVE 負傷日Ｗ(1)         TO 負傷日１.
011780     MOVE 施術開始年Ｗ(1)     TO 施術開始年１.
011790     MOVE 施術開始月Ｗ(1)     TO 施術開始月１.
011800     MOVE 施術開始日Ｗ(1)     TO 施術開始日１.
011810     MOVE 施術終了年Ｗ(1)     TO 施術終了年１.
011820     MOVE 施術終了月Ｗ(1)     TO 施術終了月１.
011830     MOVE 施術終了日Ｗ(1)     TO 施術終了日１.
011840*     MOVE 負傷年月日区切Ｗ(1) TO 負傷年月区切１ 負傷月日区切１.
011850*     MOVE 施開年月日区切Ｗ(1) TO 施開年月区切１ 施開月日区切１.
011860*     MOVE 施終年月日区切Ｗ(1) TO 施終年月区切１ 施終月日区切１.
011870*     MOVE 治癒チェックＷ(1)   TO 治癒チェック１.
011880*     MOVE 中止チェックＷ(1)   TO 中止チェック１.
011890*     MOVE 転医チェックＷ(1)   TO 転医チェック１.
011900     MOVE 転帰名称Ｗ    (1)   TO 転帰名称１.
011910**********
011920* ２部位 *
011930**********
011940*     MOVE 会ＯＣＲコードＷ(2) TO 負傷コード２.
011950     MOVE 負傷名Ｗ(2)         TO 負傷名２.
011960     MOVE 負傷年Ｗ(2)         TO 負傷年２.
011970     MOVE 負傷月Ｗ(2)         TO 負傷月２.
011980     MOVE 負傷日Ｗ(2)         TO 負傷日２.
011990     MOVE 施術開始年Ｗ(2)     TO 施術開始年２.
012000     MOVE 施術開始月Ｗ(2)     TO 施術開始月２.
012010     MOVE 施術開始日Ｗ(2)     TO 施術開始日２.
012020     MOVE 施術終了年Ｗ(2)     TO 施術終了年２.
012030     MOVE 施術終了月Ｗ(2)     TO 施術終了月２.
012040     MOVE 施術終了日Ｗ(2)     TO 施術終了日２.
012050*     MOVE 負傷年月日区切Ｗ(2) TO 負傷年月区切２ 負傷月日区切２.
012060*     MOVE 施開年月日区切Ｗ(2) TO 施開年月区切２ 施開月日区切２.
012070*     MOVE 施終年月日区切Ｗ(2) TO 施終年月区切２ 施終月日区切２.
012080*     MOVE 治癒チェックＷ(2)   TO 治癒チェック２.
012090*     MOVE 中止チェックＷ(2)   TO 中止チェック２.
012100*     MOVE 転医チェックＷ(2)   TO 転医チェック２.
012110     MOVE 転帰名称Ｗ    (2)   TO 転帰名称２.
012120**********
012130* ３部位 *
012140**********
012150*     MOVE 会ＯＣＲコードＷ(3) TO 負傷コード３.
012160     MOVE 負傷名Ｗ(3)         TO 負傷名３.
012170     MOVE 負傷年Ｗ(3)         TO 負傷年３.
012180     MOVE 負傷月Ｗ(3)         TO 負傷月３.
012190     MOVE 負傷日Ｗ(3)         TO 負傷日３.
012200     MOVE 施術開始年Ｗ(3)     TO 施術開始年３.
012210     MOVE 施術開始月Ｗ(3)     TO 施術開始月３.
012220     MOVE 施術開始日Ｗ(3)     TO 施術開始日３.
012230     MOVE 施術終了年Ｗ(3)     TO 施術終了年３.
012240     MOVE 施術終了月Ｗ(3)     TO 施術終了月３.
012250     MOVE 施術終了日Ｗ(3)     TO 施術終了日３.
012260*     MOVE 負傷年月日区切Ｗ(3) TO 負傷年月区切３ 負傷月日区切３.
012270*     MOVE 施開年月日区切Ｗ(3) TO 施開年月区切３ 施開月日区切３.
012280*     MOVE 施終年月日区切Ｗ(3) TO 施終年月区切３ 施終月日区切３.
012290*     MOVE 治癒チェックＷ(3)   TO 治癒チェック３.
012300*     MOVE 中止チェックＷ(3)   TO 中止チェック３.
012310*     MOVE 転医チェックＷ(3)   TO 転医チェック３.
012320     MOVE 転帰名称Ｗ    (3)   TO 転帰名称３.
012330**********
012340* ４部位 *
012350**********
012360*     MOVE 会ＯＣＲコードＷ(4) TO 負傷コード４.
012370     MOVE 負傷名Ｗ(4)         TO 負傷名４.
012380     MOVE 負傷年Ｗ(4)         TO 負傷年４.
012390     MOVE 負傷月Ｗ(4)         TO 負傷月４.
012400     MOVE 負傷日Ｗ(4)         TO 負傷日４.
012410     MOVE 施術開始年Ｗ(4)     TO 施術開始年４.
012420     MOVE 施術開始月Ｗ(4)     TO 施術開始月４.
012430     MOVE 施術開始日Ｗ(4)     TO 施術開始日４.
012440     MOVE 施術終了年Ｗ(4)     TO 施術終了年４.
012450     MOVE 施術終了月Ｗ(4)     TO 施術終了月４.
012460     MOVE 施術終了日Ｗ(4)     TO 施術終了日４.
012470*     MOVE 負傷年月日区切Ｗ(4) TO 負傷年月区切４ 負傷月日区切４.
012480*     MOVE 施開年月日区切Ｗ(4) TO 施開年月区切４ 施開月日区切４.
012490*     MOVE 施終年月日区切Ｗ(4) TO 施終年月区切４ 施終月日区切４.
012500*     MOVE 治癒チェックＷ(4)   TO 治癒チェック４.
012510*     MOVE 中止チェックＷ(4)   TO 中止チェック４.
012520*     MOVE 転医チェックＷ(4)   TO 転医チェック４.
012530     MOVE 転帰名称Ｗ    (4)   TO 転帰名称４.
012540**********
012550* ５部位 *
012560**********
012570*     MOVE 会ＯＣＲコードＷ(5) TO 負傷コード５.
012580     MOVE 負傷名Ｗ(5)         TO 負傷名５.
012590     MOVE 負傷年Ｗ(5)         TO 負傷年５.
012600     MOVE 負傷月Ｗ(5)         TO 負傷月５.
012610     MOVE 負傷日Ｗ(5)         TO 負傷日５.
012620     MOVE 施術開始年Ｗ(5)     TO 施術開始年５.
012630     MOVE 施術開始月Ｗ(5)     TO 施術開始月５.
012640     MOVE 施術開始日Ｗ(5)     TO 施術開始日５.
012650     MOVE 施術終了年Ｗ(5)     TO 施術終了年５.
012660     MOVE 施術終了月Ｗ(5)     TO 施術終了月５.
012670     MOVE 施術終了日Ｗ(5)     TO 施術終了日５.
012680*     MOVE 負傷年月日区切Ｗ(5) TO 負傷年月区切５ 負傷月日区切５.
012690*     MOVE 施開年月日区切Ｗ(5) TO 施開年月区切５ 施開月日区切５.
012700*     MOVE 施終年月日区切Ｗ(5) TO 施終年月区切５ 施終月日区切５.
012710*     MOVE 治癒チェックＷ(5)   TO 治癒チェック５.
012720*     MOVE 中止チェックＷ(5)   TO 中止チェック５.
012730*     MOVE 転医チェックＷ(5)   TO 転医チェック５.
012740     MOVE 転帰名称Ｗ    (5)   TO 転帰名称５.
012750*
012760*/負傷原因の印字
012770     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
012780     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
012790     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
012800     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
012810     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
012820     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
012830     MOVE 負傷原因Ｗ(7)       TO 負傷原因７.
012840     MOVE 負傷原因Ｗ(8)       TO 負傷原因８.
012850*================================================================*
012860*================================================================*
012870 負担率取得 SECTION.
012880*
012890     MOVE ZERO TO 本人負担率Ｗ.
012900     MOVE ZERO TO 家族負担率Ｗ.
012910*
012920     MOVE 施術和暦ＷＲ   TO 受－施術和暦.
012930     MOVE 施術年ＷＲ     TO 受－施術年.
012940     MOVE 施術月ＷＲ     TO 受－施術月.
012950     MOVE 患者番号ＷＲ   TO 受－患者番号.
012960     MOVE 枝番ＷＲ       TO 受－枝番.
012970     READ 受診者情報Ｆ
012980     INVALID KEY
012990         MOVE  NC"　　施術月の受診者情報がありません" TO 連メ－メッセージ
013000         CALL   "MSG001"
013010         CANCEL "MSG001"
013020         PERFORM ファイル閉鎖
013030         MOVE ZERO TO PROGRAM-STATUS
013040         EXIT PROGRAM
013050     NOT INVALID KEY
013060* 14/10～　サブルーチン処理
013070      IF 受－施術和暦年月 >= 41410
013080         PERFORM 負担率取得１４１０
014580         COMPUTE 印刷負担率Ｗ = 負担率Ｗ / 10
013820      END-IF
013830     END-READ.
013840*
013850     COMPUTE 印刷負担率Ｗ = 負担率Ｗ / 10.
013860*
013870     EVALUATE 負担率Ｗ
013880     WHEN ZERO
013890         MOVE NC"○" TO ０割チェックＷ
013900     WHEN 10
013910         MOVE NC"○" TO １割チェックＷ
013920     WHEN 20
013930         MOVE NC"○" TO ２割チェックＷ
013940     WHEN 30
013950         MOVE NC"○" TO ３割チェックＷ
013960     END-EVALUATE.
013970*================================================================*
013980 負担率取得１４１０ SECTION.
013990*
014000* 受診者Ｆ READ中
014010* 平成14/10～
014020     MOVE ZERO  TO 負担率Ｗ.
014030**     INITIALIZE 助成負担率ＷＷ.
014040*
014050     MOVE SPACE TO 連率－負担率取得キー.
014060     INITIALIZE 連率－負担率取得キー.
014070     MOVE 受－施術和暦年月 TO 連率－施術和暦年月.
014080     MOVE 受－患者コード   TO 連率－患者コード.
014090*
014100     CALL   "HUTANRIT".
014110     CANCEL "HUTANRIT".
014120*
014130* / 本体 /
014140     MOVE 連率－実際本体負担率 TO 負担率Ｗ.
014150*
014160*================================================================*
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
014350*================================================================*
014360 受診者情報取得 SECTION.
014370*
014380**************************************************
014390* 連結データから受診者情報Ｆより以下の情報を取得 *
014400* ● 患者番号.... 患者番号Ｗ                     *
014410* ● 枝番........ 枝番Ｗ                         *
014420* ● 記号 ....... 記号Ｗに格納                   *
014430* ● 番号 ....... 番号Ｗに格納                   *
014440* ● 被保険者カナ.被保険者カナＷに格納           *
014450* ● 被保険者氏名.被保険者氏名Ｗに格納           *
014460* ● 郵便番号１ ..郵便番号１Ｗに格納             *
014470* ● 郵便番号２ ..郵便番号２Ｗに格納             *
014480* ● 住所１+住所２ ..被保険者住所Ｗに格納        *
014490* ● 電話番号.....電話番号Ｗに格納               *
014500* ● 患者カナ ....患者カナＷに格納               *
014510* ● 患者氏名 ....患者氏名Ｗに格納               *
014520* ● 続柄 ........名称マスタより続柄Ｗに取得     *
014530* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
014540* ● 患者年 ......患者年Ｗに格納                 *
014550* ● 患者月 ......患者月Ｗに格納                 *
014560* ● 患者日 ......患者日Ｗに格納                 *
014570**************************************************
014580     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
014590     MOVE 施術年ＷＲ         TO 受－施術年.
014600     MOVE 施術月ＷＲ         TO 受－施術月.
014610     MOVE 患者コードＷＲ     TO 受－患者コード.
014620     READ 受診者情報Ｆ
014630     INVALID KEY
014640         CONTINUE
014650*            /* ありえない */
014660     NOT INVALID KEY
014670         MOVE 受－患者番号     TO 患者番号Ｗ
014680         MOVE 受－枝番         TO 枝番Ｗ
014690*         MOVE 受－記号         TO 記号Ｗ
014700*         MOVE 受－番号         TO 番号Ｗ
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
014710         MOVE 受－被保険者カナ TO 被保険者カナＷ
014720         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
014730*
014740*         IF 受－保険種別 NOT = 70
014750*             MOVE NC"○"     TO 業務外チェックＷ
014760*         END-IF
014770*
014780*         EVALUATE 受－保険種別
014790*         WHEN 01
014800*             MOVE NC"○"     TO 自営業チェックＷ
014810*         WHEN 02
014820*         WHEN 03
014830*         WHEN 06
014840*         WHEN 07
014850*             MOVE NC"○"     TO 会社員チェックＷ
014860*         WHEN 04
014870*             MOVE NC"公務員" TO 職業Ｗ
014880*         WHEN 09
014890*             MOVE NC"自衛官" TO 職業Ｗ
014900*         END-EVALUATE
014910*
014920         EVALUATE 受－被保険者性別
014930         WHEN 1
014940             MOVE NC"○"  TO 男チェックＷ
014950         WHEN 2
014960             MOVE NC"○"  TO 女チェックＷ
014970         END-EVALUATE
014980*
014990*         EVALUATE 受－被保険者和暦
015000*         WHEN 1
015010*             MOVE NC"○"  TO 被保険者明治Ｗ
015020*         WHEN 2
015030*             MOVE NC"○"  TO 被保険者大正Ｗ
015040*         WHEN 3
015050*             MOVE NC"○"  TO 被保険者昭和Ｗ
015060*         END-EVALUATE
015070         MOVE 受－被保険者和暦  TO 元－元号区分
015080         READ 元号マスタ
015090         INVALID KEY
015100             MOVE SPACE        TO 被保険者和暦名称Ｗ
015110         NOT INVALID KEY
015120             MOVE 元－元号名称 TO 被保険者和暦名称Ｗ
015130         END-READ
015140         MOVE 受－被保険者年     TO 被保険者年Ｗ
015150         MOVE 受－被保険者月     TO 被保険者月Ｗ
015160         MOVE 受－被保険者日     TO 被保険者日Ｗ
015170*/郵便番号 ゼロの時はスペース
015180         IF 受－郵便番号１ = "000"
015190             MOVE SPACE              TO 郵便番号１Ｗ
015200             MOVE SPACE              TO 郵便番号２Ｗ
015210         ELSE
015220             MOVE 受－患者郵便番号１ TO 郵便番号１Ｗ
015230             MOVE 受－患者郵便番号２ TO 郵便番号２Ｗ
015240         END-IF
015250*         MOVE 受－患者住所１         TO 印刷住所１Ｗ
015260*         MOVE 受－患者住所２         TO 印刷住所２Ｗ
015270*         MOVE 受－住所１             TO 印刷住所１Ｗ
015280*         MOVE 受－住所２             TO 印刷住所２Ｗ
015290         STRING 受－患者住所１ DELIMITED BY SPACE
015300                受－患者住所２ DELIMITED BY SPACE
015310                INTO 患者住所Ｗ
015320         END-STRING
015330         MOVE 受－電話番号       TO 電話番号Ｗ
               IF 受－保険種別 = 70 OR 80 OR 85 OR 90
018370             MOVE 受－患者カナ       TO 被保険者カナＷ
018380             MOVE 受－患者氏名       TO 被保険者氏名Ｗ
018000             EVALUATE 受－患者性別
018010             WHEN 1
018020                 MOVE NC"○"  TO 男チェックＷ
018030             WHEN 2
018040                 MOVE NC"○"  TO 女チェックＷ
018050             END-EVALUATE
015070             MOVE 受－患者和暦  TO 元－元号区分
015080             READ 元号マスタ
015090             INVALID KEY
015100                 MOVE SPACE        TO 被保険者和暦名称Ｗ
015110             NOT INVALID KEY
015120                 MOVE 元－元号名称 TO 被保険者和暦名称Ｗ
015130             END-READ
018150             MOVE 受－患者年   TO 被保険者年Ｗ
018160             MOVE 受－患者月   TO 被保険者月Ｗ
018170             MOVE 受－患者日   TO 被保険者日Ｗ
018220             MOVE 受－患者電話番号   TO 電話番号Ｗ
               END-IF
015340*
015350*電話番号分割
015360         UNSTRING 電話番号Ｗ  DELIMITED BY "-"
015370             INTO 分割電話番号１Ｗ
015380                  分割電話番号２Ｗ
015390                  分割電話番号３Ｗ
015400         END-UNSTRING
015410*電話番号右詰処理
015420         IF 分割電話番号３Ｗ = SPACE
015430             MOVE 分割電話番号２Ｗ TO 分割電話番号３Ｗ
015440             MOVE 分割電話番号１Ｗ TO 分割電話番号２Ｗ
015450             MOVE SPACE            TO 分割電話番号１Ｗ
015460         END-IF
015470*
015480         MOVE 受－患者カナ     TO 患者カナＷ
015490         MOVE 受－患者氏名     TO 患者氏名Ｗ
015500*
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
               MOVE 事－事業所住所１   TO 事業所住所１Ｗ
               MOVE 事－事業所住所２   TO 事業所住所２Ｗ
015560* 続柄
015570         IF 受－本人家族区分 = 1
015580             MOVE NC"本人"     TO 続柄Ｗ
015590         ELSE
015600             MOVE 05          TO 名－区分コード
015610             MOVE 受－続柄    TO 名－名称コード
015620             READ 名称マスタ
015630             INVALID KEY
015640                 MOVE SPACE   TO 続柄Ｗ
015650             NOT INVALID KEY
015660                 MOVE 名－略称  TO 続柄Ｗ
015670             END-READ
015680         END-IF
015690* 患者和暦年月日
015700         EVALUATE 受－患者和暦
015710         WHEN 1
015720             MOVE NC"○"  TO 明治チェックＷ
015730         WHEN 2
015740             MOVE NC"○"  TO 大正チェックＷ
015750         WHEN 3
015760             MOVE NC"○"  TO 昭和チェックＷ
015770         WHEN 4
015780             MOVE NC"○"  TO 平成チェックＷ
015790         END-EVALUATE
015800*         MOVE 受－患者和暦  TO 元－元号区分
015810*         READ 元号マスタ
015820*         INVALID KEY
015830*             MOVE SPACE        TO 患者和暦名称Ｗ
015840*         NOT INVALID KEY
015850*             MOVE 元－元号名称 TO 患者和暦名称Ｗ
015860*         END-READ
015870         MOVE 受－患者年  TO 患者年Ｗ
015880         MOVE 受－患者月  TO 患者月Ｗ
015890         MOVE 受－患者日  TO 患者日Ｗ
015900*患者年齢
015910     MOVE 連印－施術和暦 TO 元－元号区分
015920     READ 元号マスタ
015930     NOT INVALID KEY
015940         COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
015950     END-READ
015960*     IF ( 受－患者和暦 NOT = ZERO ) AND
015970*        ( 受－患者年   NOT = ZERO ) AND
015980*        ( 受－患者月   NOT = ZERO ) AND
015990*        ( 受－患者日   NOT = ZERO )
016000**
016010*         MOVE ZERO TO 患者年齢Ｗ
016020**
016030*         MOVE 受－患者和暦 TO 元－元号区分
016040*         READ 元号マスタ
016050*         NOT INVALID KEY
016060*             COMPUTE 患者西暦年Ｗ = 元－開始西暦年 + ( 受－患者年 - 1 )
016070*         END-READ
016080**
016090*         COMPUTE 患者年齢Ｗ   = 施術西暦年Ｗ - 患者西暦年Ｗ
016100**
016110*         IF 連印－施術月 < 受－患者月
016120*             COMPUTE 患者年齢Ｗ = 患者年齢Ｗ - 1
016130*         END-IF
016140**
016150*         MOVE 患者年齢Ｗ     TO 患者年齢
016160*     END-IF
016170* 患者性別
016180         EVALUATE 受－患者性別
016190         WHEN 1
016200             MOVE NC"○" TO 患者男チェックＷ
016210         WHEN 2
016220             MOVE NC"○" TO 患者女チェックＷ
016230         END-EVALUATE
016240*/有効期限
016250         MOVE 受－有効和暦 TO 有効期限和暦Ｗ
016260         MOVE 受－有効年   TO 有効期限年Ｗ
016270         MOVE 受－有効月   TO 有効期限月Ｗ
016280         MOVE 受－有効日   TO 有効期限日Ｗ
016290*         MOVE 有効期限和暦Ｗ TO 元－元号区分
016300*         READ 元号マスタ
016310*         INVALID KEY
016320*             MOVE SPACE        TO 有効期限元号Ｗ
016330*         NOT INVALID KEY
016340*             MOVE 元－元号名称 TO 有効期限元号Ｗ
016350*         END-READ
016360         MOVE 受－資格和暦 TO 資格取得和暦Ｗ
016370         MOVE 受－資格年   TO 資格取得年Ｗ
016380         MOVE 受－資格月   TO 資格取得月Ｗ
016390         MOVE 受－資格日   TO 資格取得日Ｗ
016400         IF (資格取得和暦Ｗ NOT = ZERO) AND
016410            (資格取得年Ｗ   NOT = ZERO) AND
016420            (資格取得月Ｗ   NOT = ZERO) AND
016430            (資格取得日Ｗ   NOT = ZERO)
016440             EVALUATE 資格取得和暦Ｗ
016450             WHEN 3
016460                 MOVE NC"○"  TO 資格昭和チェックＷ
016470             WHEN 4
016480                 MOVE NC"○"  TO 資格平成チェックＷ
016490             END-EVALUATE
016500         END-IF
016510*         IF (資格取得和暦Ｗ NOT = ZERO) AND
016520*            (資格取得年Ｗ   NOT = ZERO) AND
016530*            (資格取得月Ｗ   NOT = ZERO) AND
016540*            (資格取得日Ｗ   NOT = ZERO)
016550*             MOVE 資格取得和暦Ｗ  TO 元－元号区分
016560*             READ 元号マスタ
016570*             INVALID KEY
016580*                 MOVE SPACE        TO 資格取得元号Ｗ
016590*             NOT INVALID KEY
016600*                 MOVE 元－元号名称 TO 資格取得元号Ｗ
016610*             END-READ
016620*         END-IF
016630**市町村国保､退職の場合は有効期限を出力
016640**業種別国保(国保組合)､共済､日雇､船員､社保､組合､自衛官は有効期限日を出力する
016650*         IF (( 受－保険種別 = 08 ) OR
016660*            (( 受－保険種別 = 01 ) AND
016670*            (  受－保険者番号(3:1) NOT = "3" )))
016680*            MOVE 受－有効和暦 TO 資格取得和暦Ｗ
016690*            MOVE 受－有効年   TO 資格取得年Ｗ
016700*            MOVE 受－有効月   TO 資格取得月Ｗ
016710*            MOVE 受－有効日   TO 資格取得日Ｗ
016720*         ELSE
016730*            MOVE 受－資格和暦 TO 資格取得和暦Ｗ
016740*            MOVE 受－資格年   TO 資格取得年Ｗ
016750*            MOVE 受－資格月   TO 資格取得月Ｗ
016760*            MOVE 受－資格日   TO 資格取得日Ｗ
016770*         END-IF
016780* 市町村番号 受給者番号
016790* 老人 かつ 助成 の場合は老人の番号を印字する｡(今後どうなるかは解らない)
016800*         IF 受－助成種別 NOT = ZERO
016810*             MOVE 受－費用負担者番号助成 TO 市町村番号Ｗ
016820*             MOVE 受－受益者番号助成     TO 受益者番号Ｗ
016830*         END-IF
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
                  MOVE 労災－労災事業所住所１     TO 事業所住所１Ｗ
                  MOVE 労災－労災事業所住所２     TO 事業所住所２Ｗ
012130            MOVE 労災－労働保険番号         TO 番号Ｗ
               END-IF
016840*保険種別
016850         EVALUATE 受－保険種別
016860         WHEN 01
016870             MOVE NC"○" TO 国保チェックＷ
016880         WHEN 02
016890             MOVE NC"○" TO 社保チェックＷ
016900         WHEN 06
016910             MOVE NC"○" TO 日雇いチェックＷ
016920         WHEN 03
016930             MOVE NC"○" TO 組合チェックＷ
016940         WHEN 04
016950             MOVE NC"○" TO 共済チェックＷ
016960         WHEN 09
016970             MOVE NC"○" TO 自衛官チェックＷ
016980         WHEN 07
016990             MOVE NC"○" TO 船員チェックＷ
017000         WHEN 08
017010             MOVE NC"○" TO 退職チェックＷ
017020         WHEN 70
017030             MOVE NC"○" TO 労災チェックＷ
017040         WHEN 80
017050             MOVE NC"○" TO 自賠責チェックＷ
017060         WHEN 90
017070             MOVE NC"○" TO 自費チェックＷ
017080         END-EVALUATE
017090         IF 受－公費種別 = 05
017100             MOVE NC"○" TO ２７老人チェックＷ
017110         END-IF
017120         IF 受－助成種別 NOT = ZERO
017130             MOVE NC"○" TO 助成チェックＷ
017140         END-IF
017150*         EVALUATE 受－助成種別
017160*         WHEN 50
017170*             MOVE NC"○" TO 生保チェックＷ
017180*         WHEN 51 
017190*             MOVE NC"○" TO ４１老人チェックＷ
017200*         WHEN 52
017210*             MOVE NC"○" TO 母子チェックＷ
017220*         WHEN 53
017230*             MOVE NC"○" TO 障害チェックＷ
017240*         WHEN 54
017250*             MOVE NC"○" TO 被爆チェックＷ
017260*         WHEN 55
017270*             MOVE NC"○" TO 乳幼児チェックＷ
017280*         END-EVALUATE
017290     END-READ.
017300*/市町村番号
           IF 受－施術和暦年月 < 42004
017310         MOVE 受－費用負担者番号     TO 市町村番号老人Ｗ
017320         MOVE 受－受益者番号老人     TO 受益者番号老人Ｗ
           END-IF.
017330     MOVE 受－費用負担者番号助成 TO 市町村番号助成Ｗ.
017340     MOVE 受－受益者番号助成     TO 受益者番号助成Ｗ.
017350********************************************************HILO
017360*     MOVE NC"○"  TO 資格昭和チェックＷ 資格平成チェックＷ
017370*     患者男チェックＷ 患者女チェックＷ
017380*     明治チェックＷ 大正チェックＷ 昭和チェックＷ 平成チェックＷ
017390*     .
017400********************************************************HILO
017410*================================================================*
017420 請求先情報取得 SECTION.
017430*
017440****************************************************
017450* 連結データから保険者マスタより請求先を取得する。 *
017460* ※保－請求先情報区分=1の場合請求先マスタを使用   *
017470* ● 保険者番号...保険者番号Ｗに格納               *
017480* ● 名称........ 請求先名称Ｗに格納               *
017490* ● 郵便番号１.. 請求先郵便番号１Ｗに格納         *
017500* ● 郵便番号２.. 請求先郵便番号２Ｗに格納         *
017510* ● 住所１.......請求先住所１Ｗに格納             *
017520* ● 住所２.......請求先住所２Ｗに格納             *
017530****************************************************
017540     MOVE 保険種別ＷＲ   TO 保－保険種別.
017550     MOVE 保険者番号ＷＲ TO 保－保険者番号.
017560     READ 保険者マスタ
017570     INVALID KEY
               IF ( 受－施術和暦年月 >= 42004 ) AND ( 保険種別ＷＲ = 05 )
016990             MOVE 保険種別ＷＲ   TO 市－公費種別
017000             MOVE 保険者番号ＷＲ TO 市－市町村番号
017010             READ 市町村マスタ
017020             INVALID KEY
017030                 MOVE SPACE      TO 請求先名称Ｗ
017040             NOT INVALID KEY
017090                 MOVE 市－市町村番号  TO 保険者番号Ｗ
017110                 MOVE 市－市町村名称  TO 請求先名称Ｗ
017630                 STRING 市－支部名 DELIMITED BY SPACE
017640                        市－部署名 DELIMITED BY SPACE
017650                   INTO   支部名Ｗ
017730                 END-STRING
017120                 MOVE 市－郵便番号１  TO 請求先郵便番号１Ｗ
017130                 MOVE 市－郵便番号２  TO 請求先郵便番号２Ｗ
017700                 STRING 市－住所１ DELIMITED BY SPACE
017710                        市－住所２ DELIMITED BY SPACE
017720                   INTO 請求先住所Ｗ
017730                 END-STRING
                   END-READ
               ELSE
017580             MOVE SPACE           TO 請求先名称Ｗ
               END-IF
017590     NOT INVALID KEY
017600         MOVE 保－保険者番号  TO 保険者番号Ｗ
017610         MOVE 保－保険者名称  TO 請求先名称Ｗ
017620*         MOVE 保－支部部署名  TO 支部名Ｗ 
017630         STRING 保－支部名 DELIMITED BY SPACE
017640                保－部署名 DELIMITED BY SPACE
017650         INTO   支部名Ｗ
017660         MOVE 保－郵便番号１  TO 請求先郵便番号１Ｗ
017670         MOVE 保－郵便番号２  TO 請求先郵便番号２Ｗ
017680*         MOVE 保－住所１      TO 請求先住所１Ｗ
017690*         MOVE 保－住所２      TO 請求先住所２Ｗ
017700         STRING 保－住所１ DELIMITED BY SPACE
017710                保－住所２ DELIMITED BY SPACE
017720           INTO 請求先住所Ｗ
017730         END-STRING
017740     END-READ.
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
017750*
017760     EVALUATE 保険種別ＷＲ
017770     WHEN 2
017780     WHEN 6
017790         IF 保－接尾語区分 = 1
017800            MOVE SPACE              TO 印刷保険者呼名Ｗ
017810         ELSE
017820            MOVE "社会保険事務所"   TO 印刷保険者呼名Ｗ
017830         END-IF
017840     WHEN 3
017850         MOVE "健康保険組合"   TO 印刷保険者呼名Ｗ
017860     WHEN OTHER
017870         MOVE SPACE TO 印刷保険者呼名Ｗ
017880     END-EVALUATE.
017890     IF 印刷保険者呼名Ｗ NOT = SPACE
017900         STRING 請求先名称Ｗ DELIMITED BY SPACE
017910                保険者呼名Ｗ DELIMITED BY SPACE
017920           INTO 請求先名称Ｗ
017930         END-STRING
017940     END-IF.
017950*================================================================*
017960 負傷データ退避 SECTION.
017970*
017980*********************************************************************
017990* ① 負傷原因取得のため、負傷データＦから負傷原因Ｗに退避する。
018000* ② 詳細モード用  連印－負傷行印刷により、印刷位置を並び替えるため、
018010*                  負傷情報Ｗ  から負傷情報ＳＷに、
018020*                  負傷原因Ｗ  から負傷原因ＳＷに退避する。
018030*********************************************************************
018040*
018050     MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷原因患者番号Ｗ(部位ＣＮＴ).
018060     MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷原因連番Ｗ(部位ＣＮＴ).
018070*--------------------------------------------------------------------*
018080*/負傷コード
018090     MOVE 会ＯＣＲコードＷ(部位ＣＮＴ) TO 会部位コードＳＷ(部位ＣＮＴ).
018100     MOVE 負傷名Ｗ        (部位ＣＮＴ) TO 負傷名ＳＷ(部位ＣＮＴ).
018110*
018120     MOVE 負傷年Ｗ        (部位ＣＮＴ) TO 負傷年ＳＷ(部位ＣＮＴ).
018130     MOVE 負傷月Ｗ        (部位ＣＮＴ) TO 負傷月ＳＷ(部位ＣＮＴ).
018140     MOVE 負傷日Ｗ        (部位ＣＮＴ) TO 負傷日ＳＷ(部位ＣＮＴ).
018150*
018160     MOVE 施術開始年Ｗ(部位ＣＮＴ) TO 施術開始年ＳＷ(部位ＣＮＴ).
018170     MOVE 施術開始月Ｗ(部位ＣＮＴ) TO 施術開始月ＳＷ(部位ＣＮＴ).
018180     MOVE 施術開始日Ｗ(部位ＣＮＴ) TO 施術開始日ＳＷ(部位ＣＮＴ).
018190*
018200     MOVE 施術終了年Ｗ(部位ＣＮＴ) TO 施術終了年ＳＷ(部位ＣＮＴ).
018210     MOVE 施術終了月Ｗ(部位ＣＮＴ) TO 施術終了月ＳＷ(部位ＣＮＴ).
018220     MOVE 施術終了日Ｗ(部位ＣＮＴ) TO 施術終了日ＳＷ(部位ＣＮＴ).
018230*
018240     MOVE 治癒チェックＷ(部位ＣＮＴ) TO 治癒チェックＳＷ(部位ＣＮＴ).
018250     MOVE 中止チェックＷ(部位ＣＮＴ) TO 中止チェックＳＷ(部位ＣＮＴ).
018260     MOVE 転医チェックＷ(部位ＣＮＴ) TO 転医チェックＳＷ(部位ＣＮＴ).
018270     MOVE 転帰名称Ｗ    (部位ＣＮＴ) TO 転帰名称ＳＷ    (部位ＣＮＴ).
018280     MOVE 施術累計回数Ｗ(部位ＣＮＴ) TO 施術累計回数ＳＷ(部位ＣＮＴ).
018290*
018300     MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号ＳＷ(部位ＣＮＴ).
018310     MOVE 負傷原因連番Ｗ(部位ＣＮＴ)     TO 負傷連番ＳＷ(部位ＣＮＴ).
018320*
018330*================================================================*
018340 負傷データ取得 SECTION.
018350*
018360**************************************************
018370* 連結データから負傷データＦより以下の情報を取得 *
018380* ● 負傷名...部位＋負傷種別にて加工して格納     *
018390* ● 負傷年.......負傷年Ｗ                       *
018400* ● 負傷月.......負傷月Ｗ                       *
018410* ● 負傷日.......負傷日Ｗ                       *
018420* ● 施術終了年...終了年Ｗ                       *
018430* ● 施術終了月...終了月Ｗ                       *
018440* ● 施術終了日...終了日Ｗ                       *
018450**************************************************
018460     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
018470     MOVE 施術年ＷＲ         TO 負－施術年.
018480     MOVE 施術月ＷＲ         TO 負－施術月.
018490     MOVE 患者コードＷＲ     TO 負－患者コード.
018500     READ 負傷データＦ
018510     INVALID KEY
018520         CONTINUE
018530*            /* ありえない */
018540     NOT INVALID KEY
018550         MOVE 負－往療距離 TO 往療距離Ｗ
018560         MOVE ZERO         TO 部位数Ｗ
018570         MOVE 負－部位数   TO 部位数Ｗ
018580         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018590                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
018600*********************************************
018610* 注）全柔...負傷種別＋部位にて加工して格納 *
018620*********************************************
018630* 負傷種別
018640             MOVE SPACE                     TO 負傷名称Ｗ
018650             MOVE 03                        TO 名－区分コード
018660             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
018670             READ 名称マスタ
018680             INVALID KEY
018690                 MOVE SPACE    TO 負傷名称Ｗ
018700             NOT INVALID KEY
018710*                 MOVE 名－略称 TO 負傷名称Ｗ
018720                 MOVE 名－正式名称 TO 負傷名称Ｗ
018730             END-READ
018740* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
018760             PERFORM 名称埋込処理
018820             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
018830             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
018840             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
018850*
018860             MOVE 負－開始年(部位ＣＮＴ)   TO 施術開始年Ｗ(部位ＣＮＴ)
018870             MOVE 負－開始月(部位ＣＮＴ)   TO 施術開始月Ｗ(部位ＣＮＴ)
018880             MOVE 負－開始日(部位ＣＮＴ)   TO 施術開始日Ｗ(部位ＣＮＴ)
018890*
018900             MOVE 負－終了年(部位ＣＮＴ)   TO 施術終了年Ｗ(部位ＣＮＴ)
018910             MOVE 負－終了月(部位ＣＮＴ)   TO 施術終了月Ｗ(部位ＣＮＴ)
018920             MOVE 負－終了日(部位ＣＮＴ)   TO 施術終了日Ｗ(部位ＣＮＴ)
018930*転帰
018940*             EVALUATE 負－転帰区分(部位ＣＮＴ)
018950*             WHEN 1
018960*             WHEN 2
018970*                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
018980*             WHEN 3
018990*                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
019000*             WHEN 4
019010*                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
019020*             END-EVALUATE
                   IF 負－転帰区分(部位ＣＮＴ) NOT = 9
019030                 MOVE 04                        TO 名－区分コード
019040                 MOVE 負－転帰区分(部位ＣＮＴ)  TO 名－名称コード
019050                 READ 名称マスタ
019060                 INVALID KEY
019070                     MOVE SPACE        TO 転帰名称Ｗ(部位ＣＮＴ)
019080                 NOT INVALID KEY
019090                     MOVE 名－正式名称 TO 転帰名称Ｗ(部位ＣＮＴ)
019100                 END-READ
                   END-IF
019110*
019120             PERFORM 負傷データ退避
019130         END-PERFORM
019140     END-READ.
019150*================================================================*
019160 施術記録取得 SECTION.
019170*
019180************************************************************
019190* 連結データから負傷データＦより以下の情報を取得           *
019200* ● 施術開始年月日...該当する部位に対して当月最初の施術日 *
019210************************************************************
019220     MOVE 施術和暦ＷＲ  TO 施記－施術和暦.
019230     MOVE 施術年ＷＲ    TO 施記－施術年.
019240     MOVE 施術月ＷＲ    TO 施記－施術月.
019250     MOVE ZERO            TO 施記－施術日.
019260     MOVE ZERO            TO 施記－患者番号.
019270     MOVE SPACE           TO 施記－枝番.
019280     START 施術記録Ｆ   KEY IS >= 施記－施術和暦年月日
019290                                  施記－患者コード.
019300     IF 状態キー = "00"
019310         MOVE SPACE  TO 終了フラグ２
019320         PERFORM 施術記録Ｆ読込
019330         PERFORM UNTIL ( 終了フラグ２       = "YES" ) OR
019340                       ( 施記－施術和暦 NOT = 施術和暦ＷＲ ) OR
019350                       ( 施記－施術年   NOT = 施術年ＷＲ   ) OR
019360                       ( 施記－施術月   NOT = 施術月ＷＲ   )
019370*            **************
019380*            * 開始年月日 *
019390*            **************
019400             PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
019410                     UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
019420            EVALUATE TRUE ALSO TRUE ALSO TRUE ALSO TRUE ALSO TRUE
019430            WHEN 施記－負傷種別(部位ＣＮＴ) = 負傷種別Ｗ(部位ＣＮＴ) ALSO
019440                 施記－部位(部位ＣＮＴ)     = 部位Ｗ(部位ＣＮＴ)     ALSO
019450                 施記－左右区分(部位ＣＮＴ) = 左右区分Ｗ(部位ＣＮＴ) ALSO
019460                 施記－負傷位置番号(部位ＣＮＴ)
019470                                       = 負傷位置番号Ｗ(部位ＣＮＴ)  ALSO
019480                 開始年月日取得フラグ(部位ＣＮＴ) = SPACE
019490                   MOVE 施記－施術年     TO 施術開始年Ｗ(部位ＣＮＴ)
019500                   MOVE 施記－施術月     TO 施術開始月Ｗ(部位ＣＮＴ)
019510                   MOVE 施記－施術日     TO 施術開始日Ｗ(部位ＣＮＴ)
019520                   MOVE "YES"       TO 開始年月日取得フラグ(部位ＣＮＴ)
019530*
019540            WHEN OTHER
019550                CONTINUE
019560            END-EVALUATE
019570             END-PERFORM
019580             PERFORM 施術記録Ｆ読込
019590         END-PERFORM
019600     END-IF.
019610*================================================================*
019620 施術記録Ｆ読込 SECTION.
019630*
019640     READ 施術記録Ｆ NEXT
019650     AT END
019660         MOVE "YES" TO 終了フラグ２
019670     END-READ.
019680*================================================================*
019690 詳細モード負傷原因処理 SECTION.
019700*
019710*******************************************************************
019720* 連印－負傷印刷行により、負傷原因データの並び替えを行う。
019730* ( 負傷原因情報Ｗを初期化し、退避済みの負傷原因情報ＳＷから
019740*         並び替えを行い、負傷原因情報Ｗを再設定する。)
019750*******************************************************************
019760*
019770     INITIALIZE 負傷原因情報Ｗ.
019780*
019790     PERFORM VARYING 詳細ＣＮＴ FROM 1 BY 1
019800                                UNTIL ( 詳細ＣＮＴ > 5)
019810*
019820        IF ( 連印－負傷印刷行(詳細ＣＮＴ) NOT = ZERO) AND
019830           ( 連印－原因印刷区分(詳細ＣＮＴ)   = 1)
019840*
019850           MOVE 連印－負傷印刷行(詳細ＣＮＴ) TO 印刷行ＳＷ
019860*
019870           MOVE 負傷患者番号ＳＷ(詳細ＣＮＴ) TO 負傷原因患者番号Ｗ(印刷行ＳＷ)
019880           MOVE 負傷連番ＳＷ(詳細ＣＮＴ) TO 負傷原因連番Ｗ(印刷行ＳＷ)
019890        END-IF
019900*
019910     END-PERFORM.
019920*
019930*================================================================*
019940 詳細モード負傷処理 SECTION.
019950*
019960*******************************************************************
019970* 連印－負傷印刷行により、負傷データの並び替えを行う。
019980* ( 負傷情報Ｗを初期化し、退避済みの負傷情報ＳＷから
019990*         並び替えを行い、負傷情報Ｗを再設定する。)
020000*******************************************************************
020010*
020020     INITIALIZE 負傷情報Ｗ.
020030*
020040     PERFORM VARYING 詳細ＣＮＴ FROM 1 BY 1
020050                                UNTIL ( 詳細ＣＮＴ > 5)
020060*
020070        IF 連印－負傷印刷行(詳細ＣＮＴ) NOT = ZERO
020080*/転帰のみ印字が出来ない不具合の修正。
020090           MOVE 連印－負傷印刷行(詳細ＣＮＴ) TO 印刷行ＳＷ
020100           IF 連印－部位印刷区分(詳細ＣＮＴ) = 1
020110*
020120*              MOVE 連印－負傷印刷行(詳細ＣＮＴ) TO 印刷行ＳＷ
020130*
020140*/負傷コード
020150              MOVE 会部位コードＳＷ(詳細ＣＮＴ) TO 会ＯＣＲコードＷ(印刷行ＳＷ)
020160              MOVE 負傷名ＳＷ      (詳細ＣＮＴ) TO 負傷名Ｗ        (印刷行ＳＷ)
020170              MOVE 負傷年ＳＷ      (詳細ＣＮＴ) TO 負傷年Ｗ        (印刷行ＳＷ)
020180              MOVE 負傷月ＳＷ      (詳細ＣＮＴ) TO 負傷月Ｗ        (印刷行ＳＷ)
020190              MOVE 負傷日ＳＷ      (詳細ＣＮＴ) TO 負傷日Ｗ        (印刷行ＳＷ)
020200              MOVE 施術開始年ＳＷ  (詳細ＣＮＴ) TO 施術開始年Ｗ    (印刷行ＳＷ)
020210              MOVE 施術開始月ＳＷ  (詳細ＣＮＴ) TO 施術開始月Ｗ    (印刷行ＳＷ)
020220              MOVE 施術開始日ＳＷ  (詳細ＣＮＴ) TO 施術開始日Ｗ    (印刷行ＳＷ)
020230           END-IF
020240*
020250           IF 連印－転帰印刷区分(詳細ＣＮＴ) = 1
020260              MOVE 施術終了年ＳＷ  (詳細ＣＮＴ) TO 施術終了年Ｗ  (印刷行ＳＷ)
020270              MOVE 施術終了月ＳＷ  (詳細ＣＮＴ) TO 施術終了月Ｗ  (印刷行ＳＷ)
020280              MOVE 施術終了日ＳＷ  (詳細ＣＮＴ) TO 施術終了日Ｗ  (印刷行ＳＷ)
020290              MOVE 治癒チェックＳＷ(詳細ＣＮＴ) TO 治癒チェックＷ(印刷行ＳＷ)
020300              MOVE 中止チェックＳＷ(詳細ＣＮＴ) TO 中止チェックＷ(印刷行ＳＷ)
020310              MOVE 転医チェックＳＷ(詳細ＣＮＴ) TO 転医チェックＷ(印刷行ＳＷ)
020320              MOVE 転帰名称ＳＷ    (詳細ＣＮＴ) TO 転帰名称Ｗ    (印刷行ＳＷ)
020330              MOVE 施術累計回数ＳＷ(詳細ＣＮＴ) TO 施術累計回数Ｗ(印刷行ＳＷ)
020340           END-IF
020350        END-IF
020360*
020370     END-PERFORM.
020380*
020390*================================================================*
020400 印刷処理 SECTION.
020410*
020420     EVALUATE 印刷モードＦＷＲ
020430     WHEN 0
020440         PERFORM 印刷処理２
020450         PERFORM 印刷処理３
020460     WHEN 1
020470         PERFORM 印刷処理２
020480     WHEN 2
020490         PERFORM 印刷処理３
020520     WHEN 4
020530         IF 連印－保険証印刷区分 = 1
020540             PERFORM 印刷処理２
020550         END-IF
020560         PERFORM 印刷処理３
020580     END-EVALUATE.
020590*================================================================*
020600 印刷処理１ SECTION.
020610*
020620     MOVE "YCB662P" TO  定義体名Ｐ.
020630*     MOVE "SCREEN"  TO  項目群名Ｐ.
020640     MOVE "GRP001"  TO  項目群名Ｐ.
020650     WRITE YCB662P.
020660*     WRITE 印刷レコード.
020670     PERFORM エラー処理Ｐ.
020680*================================================================*
020690 印刷処理２ SECTION.
020700*
020710     MOVE "YCB662P"  TO  定義体名Ｐ.
020720     MOVE "GRP001"   TO  項目群名Ｐ.
020730     WRITE YCB662P.
020740*     WRITE 印刷レコード.
020750     PERFORM エラー処理Ｐ.
020760*================================================================*
020770 印刷処理３ SECTION.
020780*
020790     MOVE "YCB662P"  TO  定義体名Ｐ.
020800     MOVE "GRP002"   TO  項目群名Ｐ.
020810     WRITE YCB662P.
020820*     WRITE 印刷レコード.
020830     PERFORM エラー処理Ｐ.
020910*================================================================*
020920 エラー処理Ｐ SECTION.
020930*
020940     IF 通知情報Ｐ NOT = "00"
020950         DISPLAY NC"帳票エラー"              UPON CONS
020960         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
020970         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
020980         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
020990         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
021000                                             UPON CONS
021010         ACCEPT  キー入力 FROM CONS
021020         PERFORM ファイル閉鎖
021030         EXIT PROGRAM
021040     END-IF.
021050*================================================================*
021060 名称埋込処理 SECTION.
021070*
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
           END-READ.
021200*
022550*================================================================*
022560 負傷原因取得 SECTION.
022570*
022580********************************************************************
022590*  負傷原因コードが同じものは、1行にまとめて印字する。
022600*  例: ①② 家で転んだ.
022610*     負傷原因コードが同じものをまとめ、テーブルにセット
022620*     (ただし、部位を飛んで同じものは、2行になる)
022630********************************************************************
022640     MOVE  ZERO   TO  カウンタ カウンタ２.
022650     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
      */登録部位数以上の行に印刷出来ない不具合の修正/090311
022140             UNTIL ( 部位ＣＮＴ > 7 )
022660*             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
022670*
022680        IF ( 負傷原因連番Ｗ(部位ＣＮＴ)      NOT = ZERO )
022690*
022700           IF カウンタ = ZERO
022710               MOVE 1   TO  カウンタ カウンタ２
022720               MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
022730               MOVE 負傷原因連番Ｗ(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
022740               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
022750           ELSE
022760              IF ( 負傷原因患者番号Ｗ(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
022770                 ( 負傷原因連番Ｗ(部位ＣＮＴ)      = 負傷連番ＣＷ     )
022780                 COMPUTE カウンタ２ = カウンタ２  +  1
022790                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
022800              ELSE
022810                 COMPUTE カウンタ = カウンタ  +  1
022820                 MOVE 1   TO  カウンタ２
022830                 MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
022840                 MOVE 負傷原因連番Ｗ(部位ＣＮＴ)   TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
022850                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
022860              END-IF
022870           END-IF
022880        END-IF
022890     END-PERFORM.
022900**************************************************************************
022910*  負傷原因マスタより文章取得
022920**************************************************************************
022930     MOVE  ZERO   TO  カウンタ カウンタ２.
022940     PERFORM VARYING カウンタ FROM 1 BY 1
022950             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
022960** 健保は 区分 01
022970*         MOVE 01                        TO 負原－区分コード
      */健保：０１、労災：０２、自賠責：０３
               EVALUATE 受－保険種別
               WHEN 70
                   MOVE 2 TO 負原－区分コード
               WHEN 80
                   MOVE 3 TO 負原－区分コード
               WHEN OTHER
                   MOVE 1 TO 負原－区分コード
               END-EVALUATE
022980         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
022990         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
023000         READ 負傷原因Ｆ
023010         NOT INVALID KEY
023020             INITIALIZE 負傷原因ＷＴ
023030             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
023040             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
023050             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
023060             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
023070             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
023080             PERFORM VARYING カウンタ２ FROM 1 BY 1
023090                     UNTIL ( カウンタ２ > 9 )  OR 
023100                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
023110                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
023120                WHEN 1
023130                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
023140                WHEN 2
023150                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
023160                WHEN 3
023170                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
023180                WHEN 4
023190                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
023200                WHEN 5
023210                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
023220                WHEN 6
023230                   MOVE "⑥"  TO  負傷原因ナンバーＷ１(カウンタ２)
023240                WHEN OTHER
023250                   CONTINUE
023260                END-EVALUATE
023270             END-PERFORM
023280*
023362             IF 負原－負傷原因入力区分 = 1
023363                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
023364                        負傷原因１ＷＴ  DELIMITED BY SIZE
023365                        負傷原因２ＷＴ  DELIMITED BY SIZE
023366                        負傷原因３ＷＴ  DELIMITED BY SIZE
023367                        負傷原因４ＷＴ  DELIMITED BY SIZE
023368                        負傷原因５ＷＴ  DELIMITED BY SIZE
023369                        INTO 負傷原因内容合成Ｗ(カウンタ)
023370                 END-STRING
023371             ELSE
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
023380             END-IF
023381*
023382         END-READ
023383     END-PERFORM.
023390*
023400     PERFORM 負傷原因セット.
023410*
023420*================================================================*
023430 負傷原因セット SECTION.
023440*
023450**************************************************************************
023460*  文章が1行を超える時は、複数行に分解する。
023470**************************************************************************
023480     MOVE  ZERO   TO  カウンタ カウンタ２.
023490     PERFORM VARYING カウンタ FROM 1 BY 1
023500             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
023510*
023520          INITIALIZE 負傷原因内容分解ＸＷ
023530          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
023540          IF  負傷原因内容１ＸＷ  NOT = SPACE
023550              COMPUTE カウンタ２ = カウンタ２  +  1
023560              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
023570          END-IF
023580          IF  負傷原因内容２ＸＷ  NOT = SPACE
023590              COMPUTE カウンタ２ = カウンタ２  +  1
023600              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
023610          END-IF
023620          IF  負傷原因内容３ＸＷ  NOT = SPACE
023630              COMPUTE カウンタ２ = カウンタ２  +  1
023640              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
023650          END-IF
023660          IF  負傷原因内容４ＸＷ  NOT = SPACE
023670              COMPUTE カウンタ２ = カウンタ２  +  1
023680              MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
023690          END-IF
020690          IF  負傷原因内容５ＸＷ  NOT = SPACE
020700              COMPUTE カウンタ２ = カウンタ２  +  1
020710              MOVE 負傷原因内容５ＸＷ  TO 負傷原因Ｗ(カウンタ２)
020720          END-IF
023700*
023710     END-PERFORM.
025210*================================================================*
025220******************************************************************
025230 END PROGRAM YCB662.
025240******************************************************************
