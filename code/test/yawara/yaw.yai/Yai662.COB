000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAI662.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*         日整愛知
000100*　　　　　カルテ印刷（柔＋ｳｨﾝﾄﾞｳｽﾞ版）
000110*         MED = YAW660 YAI662P
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2013-06-11
000140 DATE-COMPILED.          2013-06-11
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
001420     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF002
001430                             SYMBOLIC    DESTINATION  IS "PRT"
001440                             FORMAT                   IS  定義体名Ｐ
001450                             GROUP                    IS  項目群名Ｐ
001460                             PROCESSING  MODE         IS  処理種別Ｐ
001470                             UNIT        CONTROL      IS  拡張制御Ｐ
001480                             FILE        STATUS       IS  通知情報Ｐ.
001490******************************************************************
001500*                      DATA DIVISION                             *
001510******************************************************************
001520 DATA                    DIVISION.
001530 FILE                    SECTION.
001540*                           ［ＲＬ＝  ３２０］
001550 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001560     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
002110*                           ［ＲＬ＝  ２５６］
002120 FD  市町村マスタ        BLOCK   CONTAINS   1   RECORDS.
002130     COPY SITYOSN         OF  XFDLIB  JOINING   市   AS  PREFIX.
001570*                           ［ＲＬ＝  １２８］
001580 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001590     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001600*                           ［ＲＬ＝  １２８］
001610 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
001620     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
001660*                           ［ＲＬ＝  ２５６］
001670 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001680     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
001690     COPY SEIGYO01        OF  XFDLIB  JOINING   制０１ AS  PREFIX.
001790*                           ［ＲＬ＝  ３２０］
001800 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
001810     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
001820*                           ［ＲＬ＝  ２５６］
001830 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
001840     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
001850*                           ［ＲＬ＝  １２８］
001860 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
001870     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
001910*                           ［ＲＬ＝  １２８］
001920 FD  負傷原因Ｆ         BLOCK   CONTAINS   1   RECORDS.
001930     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
      *                           ［ＲＬ＝  １２８］
       FD  事業所マスタ        BLOCK   CONTAINS   1   RECORDS.
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
001940*
001950 FD  印刷ファイル.
001960     COPY YAI662P        OF  XMDLIB.
001970*----------------------------------------------------------------*
001980******************************************************************
001990*                WORKING-STORAGE SECTION                         *
002000******************************************************************
002010 WORKING-STORAGE         SECTION.
002020 01 キー入力                           PIC X     VALUE SPACE.
002030 01 状態キー                           PIC X(2)  VALUE SPACE.
002040 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002050 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002060 01 継続フラグ                         PIC X(3)  VALUE SPACE.
002070 01 詳細ＣＮＴ                         PIC 9     VALUE ZERO.
002080 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002120 01 部位番号Ｗ                         PIC N(1)  VALUE SPACE.
002090 01 文字ＣＮＴ                         PIC 9(2)  VALUE ZERO.
002100 01 初検フラグ                         PIC X(3)  VALUE SPACE.
002110 01 カウンタ３                         PIC 9(2)  VALUE ZERO.
002120 01 ファイル名                         PIC N(2)  VALUE SPACE.
002130 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
002140 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002150 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002160 01 部位名称Ｗ                         PIC N(10) VALUE SPACE.
002170 01 負傷番号Ｗ                         PIC 9(1)  VALUE ZERO.
002180 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
002190    03 負傷番号Ｗ１                    PIC X.
002200*
002210 01 全角負傷番号Ｗ                     PIC N(9) VALUE SPACE.
002220 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
002230    03 全角負傷番号Ｗ１                PIC X(2).
002240*
002250 01 負担率Ｗ                           PIC 9(3) VALUE ZERO.
002260 01 甲乙区分Ｗ                         PIC 9    VALUE ZERO.
002270 01 本人負担率Ｗ                       PIC 9(3) VALUE ZERO.
002280 01 家族負担率Ｗ                       PIC 9(3) VALUE ZERO.
002290*
002370 01 施術西暦年Ｗ                       PIC 9(4) VALUE ZERO.
002380 01 患者西暦年Ｗ                       PIC 9(4) VALUE ZERO.
002390*
002400** バーコード区分用
002410 01 バーコード使用区分Ｗ               PIC 9 VALUE ZERO.
002420*
002430** 社保用
002440 01 接尾語区分Ｗ                       PIC 9     VALUE ZERO.
002450*
002460*
002470****************
002480* 連結項目待避 *
002490****************
002500*    ************
002510*    * 印刷キー *
002520*    ************
002530 01 対象データＷＲ.
002540    03 施術和暦年月ＷＲ.
002550       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
002560       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
002570       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
002580    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
002580    03 公費種別Ｗ                       PIC 9(2)  VALUE ZERO.
002580    03 助成種別Ｗ                       PIC 9(2)  VALUE ZERO.
002590    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
002600    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
002610    03 被保険者カナＷＲ                 PIC X(20) VALUE SPACE.
002620    03 患者コードＷＲ.
002630       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
002640       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
002650    03 印刷モードＦＷＲ                 PIC 9(1)  VALUE ZERO.
002910    03 丸付けＷＲ                       PIC 9(1)  VALUE ZERO.
002660    03 提出年月日ＷＲ.
002670       05 提出年ＷＲ                    PIC 9(2)  VALUE ZERO.
002680       05 提出月ＷＲ                    PIC 9(2)  VALUE ZERO.
002690       05 提出日ＷＲ                    PIC 9(2)  VALUE ZERO.
002700    03 段数ＷＲ                         PIC 9(1)  VALUE ZERO.
002710************
002720* 料金情報 *
002730************
002740*
002750* 01 本人負担率Ｗ                        PIC 9(1) VALUE ZERO.
002760 01 印刷負担率Ｗ                        PIC 9(2)  VALUE ZERO.
002870**************
002880* 受診者情報 *
002890**************
002900 01 受診者情報Ｗ.
002910    03 患者コードＷ.
002920       05 患者番号Ｗ                   PIC 9(6) VALUE ZERO.
002930       05 枝番Ｗ                       PIC X(1) VALUE SPACE.
002940    03 市町村番号Ｗ.
002950*       05 印刷市町村番号Ｗ             PIC X(8) VALUE SPACE.
002960       05 印刷市町村番号Ｗ.
002970         07 市町村番号１Ｗ             PIC X(1) VALUE SPACE.
002980         07 市町村番号２Ｗ             PIC X(1) VALUE SPACE.
002990         07 市町村番号３Ｗ             PIC X(1) VALUE SPACE.
003000         07 市町村番号４Ｗ             PIC X(1) VALUE SPACE.
003010         07 市町村番号５Ｗ             PIC X(1) VALUE SPACE.
003020         07 市町村番号６Ｗ             PIC X(1) VALUE SPACE.
003030         07 市町村番号７Ｗ             PIC X(1) VALUE SPACE.
003040         07 市町村番号８Ｗ             PIC X(1) VALUE SPACE.
003050       05 FILLER                       PIC X(2).
003060    03 受益者番号Ｗ.
003070       05 印刷受益者番号Ｗ             PIC X(11) VALUE SPACE.
003160       05 FILLER                       PIC X(9).
          03 負担者番号Ｗ                    PIC X(8)  VALUE SPACE.
          03 受益者番号助成Ｗ                PIC X(11) VALUE SPACE.
003170    03 記号Ｗ                          PIC X(24) VALUE SPACE.
003180    03 番号Ｗ.
003190       05 印刷番号Ｗ                   PIC X(20) VALUE SPACE.
003210    03 被保険者情報Ｗ.
003220       05 被保険者カナＷ               PIC X(50) VALUE SPACE.
003230       05 被保険者氏名Ｗ               PIC X(50) VALUE SPACE.
003240*
003250       05 被保険者和暦チェックＷ.
003260          07 被保険者明治Ｗ            PIC N(1) VALUE SPACE.
003270          07 被保険者大正Ｗ            PIC N(1) VALUE SPACE.
003280          07 被保険者昭和Ｗ            PIC N(1) VALUE SPACE.
003280          07 被保険者平成Ｗ            PIC N(1) VALUE SPACE.
003290       05 業務チェックＷ.
003300          07 業務外チェックＷ          PIC N(1) VALUE SPACE.
003310          07 自営業チェックＷ          PIC N(1) VALUE SPACE.
003320          07 会社員チェックＷ          PIC N(1) VALUE SPACE.
003330          07 職業Ｗ                    PIC N(3) VALUE SPACE.
003370       05 被保険者性別Ｗ               PIC N(4) VALUE SPACE.
003380       05 被保険者生年月日Ｗ.
003390          07 被保険者和暦Ｗ            PIC 9(1) VALUE ZERO.
003400          07 被保険者年Ｗ              PIC 9(2) VALUE ZERO.
003410          07 被保険者月Ｗ              PIC 9(2) VALUE ZERO.
003420          07 被保険者日Ｗ              PIC 9(2) VALUE ZERO.
003430*
003440       05 郵便番号Ｗ.
003450          07 郵便番号１Ｗ              PIC X(3) VALUE SPACE.
003460          07 郵便番号２Ｗ              PIC X(4) VALUE SPACE.
003470       05 被保険者住所Ｗ.
003480          07 印刷住所１Ｗ               PIC X(40) VALUE SPACE.
003490          07 印刷住所２Ｗ               PIC X(40) VALUE SPACE.
003500       05 電話番号Ｗ                   PIC X(15) VALUE SPACE.
003550    03 資格取得年月日Ｗ.
003560       05 資格取得元号Ｗ               PIC N(2) VALUE SPACE.
003570       05 資格取得和暦Ｗ               PIC 9(1) VALUE ZERO.
003580       05 資格取得年Ｗ                 PIC 9(2) VALUE ZERO.
003590       05 資格取得月Ｗ                 PIC 9(2) VALUE ZERO.
003600       05 資格取得日Ｗ                 PIC 9(2) VALUE ZERO.
003610       05 資格和暦チェックＷ.
003620          07 資格昭和チェックＷ        PIC N(1) VALUE SPACE.
003630          07 資格平成チェックＷ        PIC N(1) VALUE SPACE.
003640    03 有効期限年月日Ｗ.
003650       05 有効期限元号Ｗ               PIC N(2) VALUE SPACE.
003660       05 有効期限和暦Ｗ               PIC 9(1) VALUE ZERO.
003670       05 有効期限年Ｗ                 PIC 9(2) VALUE ZERO.
003680       05 有効期限月Ｗ                 PIC 9(2) VALUE ZERO.
003690       05 有効期限日Ｗ                 PIC 9(2) VALUE ZERO.
003700    03 患者情報Ｗ.
003710       05 患者カナＷ                   PIC X(50) VALUE SPACE.
003720       05 患者氏名Ｗ                   PIC X(50) VALUE SPACE.
003730       05 続柄Ｗ.
003740          07 印刷続柄Ｗ                PIC N(4) VALUE SPACE.
003750          07 FILLER                    PIC X(4) VALUE SPACE.
003760       05 和暦チェックＷ.
003770          07 明治チェックＷ            PIC N(1) VALUE SPACE.
003780          07 大正チェックＷ            PIC N(1) VALUE SPACE.
003790          07 昭和チェックＷ            PIC N(1) VALUE SPACE.
003800          07 平成チェックＷ            PIC N(1) VALUE SPACE.
003810       05 患者年Ｗ                     PIC 9(2) VALUE ZERO.
003820       05 患者月Ｗ                     PIC 9(2) VALUE ZERO.
003830       05 患者日Ｗ                     PIC 9(2) VALUE ZERO.
003840       05 患者年齢Ｗ                   PIC 9(3) VALUE ZERO.
003860       05 性別チェックＷ.
003870          07 男チェックＷ              PIC N(1) VALUE SPACE.
003880          07 女チェックＷ              PIC N(1) VALUE SPACE.
003890**************
003900* 事業所情報 *
003910**************
003920 01 事業所情報Ｗ.
003930    03 事業所名称Ｗ.
003940       05 印刷事業所名称Ｗ             PIC X(60) VALUE SPACE.
003950*       05 FILLER                       PIC X(5) VALUE SPACE.
003960    03 事業所郵便番号Ｗ.
003970       05 事業所郵便番号１Ｗ           PIC X(3) VALUE SPACE.
003980       05 事業所郵便番号２Ｗ           PIC X(4) VALUE SPACE.
003990    03 事業所住所Ｗ                    PIC X(50) VALUE SPACE.
003990    03 事業所住所２Ｗ                  PIC X(50) VALUE SPACE.
004000**************
004010* 請求先情報 *
004020**************
004030 01 請求先情報Ｗ.
004040    03 保険者番号Ｗ.
004050       05 印刷保険者番号Ｗ             PIC X(8) VALUE SPACE.
004060       05 FILLER                       PIC X(2).
004070    03 請求先名称Ｗ.
004080       05 請求先名称１Ｗ               PIC X(40) VALUE SPACE.
004080       05 請求先名称２Ｗ               PIC X(40) VALUE SPACE.
004100    03 支部名Ｗ                        PIC X(40) VALUE SPACE.
004110*       05 支部名１Ｗ                   PIC X(10) VALUE SPACE.
004120*       05 支部名２Ｗ                   PIC X(10) VALUE SPACE.
004140    03 印刷支部名Ｗ                    PIC X(10) VALUE SPACE.
004150    03 請求先郵便番号Ｗ.
004160       05 請求先郵便番号１Ｗ           PIC X(3) VALUE SPACE.
004170       05 請求先郵便番号２Ｗ           PIC X(4) VALUE SPACE.
004180    03 請求先住所１Ｗ.
004190       05 印刷請求先住所１Ｗ           PIC X(40) VALUE SPACE.
004200    03 請求先住所２Ｗ.
004210       05 印刷請求先住所２Ｗ           PIC X(40) VALUE SPACE.
004220    03 保険種別チェックＷ.
004230       05 社保チェックＷ               PIC N(1) VALUE SPACE.
004240       05 組合チェックＷ               PIC N(1) VALUE SPACE.
004250       05 共済チェックＷ               PIC N(1) VALUE SPACE.
004260       05 国保チェックＷ               PIC N(1) VALUE SPACE.
004230       05 退職チェックＷ               PIC N(1) VALUE SPACE.
004240       05 日雇チェックＷ               PIC N(1) VALUE SPACE.
004240       05 船員チェックＷ               PIC N(1) VALUE SPACE.
004250       05 自賠チェックＷ               PIC N(1) VALUE SPACE.
004260       05 自費チェックＷ               PIC N(1) VALUE SPACE.
004230       05 生保チェックＷ               PIC N(1) VALUE SPACE.
004250       05 前高チェックＷ               PIC N(1) VALUE SPACE.
004250       05 後高チェックＷ               PIC N(1) VALUE SPACE.
004260       05 労災チェックＷ               PIC N(1) VALUE SPACE.
004240       05 母子チェックＷ               PIC N(1) VALUE SPACE.
004250       05 乳児チェックＷ               PIC N(1) VALUE SPACE.
004260       05 障害チェックＷ               PIC N(1) VALUE SPACE.
004260       05 福祉チェックＷ               PIC N(1) VALUE SPACE.
004260       05 その他Ｗ                     PIC N(1) VALUE SPACE.
004220    03 負担割合チェックＷ.
004230       05 ０割チェックＷ               PIC N(1) VALUE SPACE.
004230       05 １割チェックＷ               PIC N(1) VALUE SPACE.
004230       05 ２割チェックＷ               PIC N(1) VALUE SPACE.
004230       05 ３割チェックＷ               PIC N(1) VALUE SPACE.
      *
004740    03 保険者呼名Ｗ.
004760       05 印刷保険者呼名Ｗ             PIC X(14) VALUE SPACE.
004710    03 請求先名支部名Ｗ.
004720       05 印刷請求先名支部名Ｗ         PIC X(40) VALUE SPACE.
      *
004270****************
004280* 負傷データＦ *
004290****************
004300 01 負傷情報Ｗ.
004310    03 部位数Ｗ                        PIC 9(1) VALUE ZERO.
004320    03 負傷データ情報Ｗ  OCCURS   9.
004330       05 会部位コードＷ               PIC X(4) VALUE SPACE.
004340       05 会ＯＣＲコードＷ             PIC X(4) VALUE ZERO.
004350       05 部位ＣＮＴＷ                 PIC 9(1) VALUE ZERO.
004360       05 負傷種別Ｗ                   PIC 9(2) VALUE ZERO.
004370       05 部位Ｗ                       PIC 9(2) VALUE ZERO.
004380       05 左右区分Ｗ                   PIC 9(1) VALUE ZERO.
004390       05 負傷位置番号Ｗ               PIC 9(2) VALUE ZERO.
004400       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
004410       05 負傷年月日Ｗ.
004420          07 負傷和暦Ｗ                PIC 9(1) VALUE ZERO.
004430          07 負傷年Ｗ                  PIC 9(2) VALUE ZERO.
004440          07 負傷月Ｗ                  PIC 9(2) VALUE ZERO.
004450          07 負傷日Ｗ                  PIC 9(2) VALUE ZERO.
004460          07 負傷年月日区切Ｗ          PIC X(1) VALUE SPACE.
004470       05 施術開始年月日Ｗ.
004480          07 施術開始和暦Ｗ            PIC 9(1) VALUE ZERO.
004490          07 施術開始年Ｗ              PIC 9(2) VALUE ZERO.
004500          07 施術開始月Ｗ              PIC 9(2) VALUE ZERO.
004510          07 施術開始日Ｗ              PIC 9(2) VALUE ZERO.
004520          07 施開年月日区切Ｗ          PIC X(1) VALUE ZERO.
004530       05 施術終了年月日Ｗ.
004540          07 施術終了和暦Ｗ            PIC 9(1) VALUE ZERO.
004550          07 施術終了年Ｗ              PIC 9(2) VALUE ZERO.
004560          07 施術終了月Ｗ              PIC 9(2) VALUE ZERO.
004570          07 施術終了日Ｗ              PIC 9(2) VALUE ZERO.
004580          07 施終年月日区切Ｗ          PIC X(1) VALUE SPACE.
004590       05 開始年月日取得フラグ         PIC X(3) VALUE SPACE.
004600       05 転帰チェックＷ.
004610          07 治癒チェックＷ            PIC N(1) VALUE SPACE.
004620          07 中止チェックＷ            PIC N(1) VALUE SPACE.
004630          07 転医チェックＷ            PIC N(1) VALUE SPACE.
004640       05 施術累計回数Ｗ               PIC 9(4)  VALUE ZERO.
004650************
004660* 負傷原因 *
004670************
004680 01 カウンタ                           PIC 9(2)  VALUE ZERO.
004690 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
004700* 負傷原因用
004710 01 負傷原因ＷＴ.
004720    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
004730    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
004740    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
004750    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
004760    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
004770    03 負傷原因ナンバーＷＴ.
004780       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
004790    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
004800 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
004810 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
004820 01 負傷原因ＴＢＬ.
004830    03 負傷原因コードＴＢＬ            OCCURS 9.
004840       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
004850       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
004860       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
004870*/印刷する負傷原因のワーク
004880 01 負傷原因内容Ｗ.
004890    03 負傷原因内容合成Ｗ              PIC X(310) OCCURS 9 VALUE SPACE.
004900    03 負傷原因内容分解ＸＷ.
004910       05 負傷原因内容１ＸＷ           PIC X(70)  VALUE SPACE.
004920       05 負傷原因内容２ＸＷ           PIC X(70)  VALUE SPACE.
004930       05 負傷原因内容３ＸＷ           PIC X(70)  VALUE SPACE.
004920       05 負傷原因内容４ＸＷ           PIC X(70)  VALUE SPACE.
004930       05 負傷原因内容５ＸＷ           PIC X(30)  VALUE SPACE.
004940*
004950 01 負傷原因Ｗテーブル.
004960    03 負傷原因Ｗ                      PIC X(70) OCCURS 45 VALUE SPACE.
      */負傷原因の詳細印刷/051125
       01 負傷原因Ｗテーブル２.
          03 負傷原因Ｗ２                    PIC X(70) OCCURS 45 VALUE SPACE.
      */症状の詳細印刷/051125
       01 症状ＷＲテーブル.
          03 症状ＷＲ                        PIC X(120) OCCURS  5 VALUE SPACE.
004970*
004980** 負傷原因印刷区分用
004990 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
      *
      ** 部位丸付け用
000320 01 負傷データＷＰ.
000330    03 部位コードＷＰ      OCCURS   9.
000340       05 負傷種別ＷＰ                 PIC 9(2).
             05 部位番号ＷＰ.
000350         07 部位ＷＰ                   PIC 9(2).
000360         07 左右区分ＷＰ               PIC 9.
000370       05 負傷位置番号ＷＰ             PIC 9(2).
005000*
004950 01 症状Ｗテーブル.
004960    03 症状Ｗ                          PIC N(35) OCCURS 10 VALUE SPACE.
004890    03 症状合成Ｗ                      PIC X(120) OCCURS 5 VALUE SPACE.
004900    03 症状分解Ｗ.
004910       05 症状内容１Ｗ                 PIC N(35)  VALUE SPACE.
004920       05 症状内容２Ｗ                 PIC N(25)  VALUE SPACE.
004970*
005010**********
005020* 合計行 *
005030**********
005040 01 合計行Ｗ.
005050    03 請求開始日Ｗ                    PIC 9(2) VALUE ZERO.
005060    03 請求終了日Ｗ                    PIC 9(2) VALUE ZERO.
005070    03 対象西暦Ｗ                      PIC 9(4) VALUE ZERO.
005080    03 商Ｗ                            PIC 9(3) VALUE ZERO.
005090    03 余Ｗ                            PIC 9(3) VALUE ZERO.
005100*
005110 01 負傷原因情報Ｗ.
005120    03 負傷原因コードＷ  OCCURS   9.
005130       09 負傷原因患者番号Ｗ           PIC 9(6) VALUE ZERO.
005140       09 負傷原因連番Ｗ               PIC 9(4) VALUE ZERO.
005150****************
005160* 詳細用ワーク *
005170****************
005180* 連印－負傷印刷行用ワーク
005190 01 印刷行ＳＷ                           PIC 9(1) VALUE ZERO.
005200*
005210 01 負傷原因情報ＳＷ.
005220    03 負傷原因コードＳＷ  OCCURS   9.
005230       09 負傷患者番号ＳＷ               PIC 9(6) VALUE ZERO.
005240       09 負傷連番ＳＷ                   PIC 9(4) VALUE ZERO.
005250*
005260 01 負傷情報ＳＷ.
005270    03 負傷データＳＷ  OCCURS   9.
005280       05 会部位コードＳＷ               PIC X(4)  VALUE SPACE.
005290       05 部位ＣＮＴＳＷ                 PIC 9(1)  VALUE ZERO.
005300       05 負傷種別ＳＷ                   PIC 9(2)  VALUE ZERO.
005310       05 部位ＳＷ                       PIC 9(2)  VALUE ZERO.
005320       05 左右区分ＳＷ                   PIC 9(1)  VALUE ZERO.
005330       05 負傷位置番号ＳＷ               PIC 9(2)  VALUE ZERO.
005340       05 負傷名ＳＷ                     PIC N(18) VALUE SPACE.
005350       05 負傷年月日ＳＷ.
005360          07 負傷和暦ＳＷ                PIC 9(1)  VALUE ZERO.
005370          07 負傷年ＳＷ                  PIC 9(2)  VALUE ZERO.
005380          07 負傷月ＳＷ                  PIC 9(2)  VALUE ZERO.
005390          07 負傷日ＳＷ                  PIC 9(2)  VALUE ZERO.
005400          07 負傷年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
005410       05 施術開始年月日ＳＷ.
005420          07 施術開始和暦ＳＷ            PIC 9(1)  VALUE ZERO.
005430          07 施術開始年ＳＷ              PIC 9(2)  VALUE ZERO.
005440          07 施術開始月ＳＷ              PIC 9(2)  VALUE ZERO.
005450          07 施術開始日ＳＷ              PIC 9(2)  VALUE ZERO.
005460          07 施開年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
005470       05 施術終了年月日ＳＷ.
005480          07 施術終了和暦ＳＷ            PIC 9(1)  VALUE ZERO.
005490          07 施術終了年ＳＷ              PIC 9(2)  VALUE ZERO.
005500          07 施術終了月ＳＷ              PIC 9(2)  VALUE ZERO.
005510          07 施術終了日ＳＷ              PIC 9(2)  VALUE ZERO.
005520          07 施終年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
005530       05 開始年月日取得フラグＳ         PIC X(3)  VALUE SPACE.
005540       05 転帰ＳＷ                       PIC N(4)  VALUE SPACE.
005550       05 転帰チェックＳＷ.
005560          07 治癒チェックＳＷ            PIC N(1)  VALUE SPACE.
005570          07 中止チェックＳＷ            PIC N(1)  VALUE SPACE.
005580          07 転医チェックＳＷ            PIC N(1)  VALUE SPACE.
005590       05 施術累計回数ＳＷ               PIC 9(4)  VALUE ZERO.
             05 部位コードＳＷ                 PIC 9(7)  VALUE ZERO.
005600*/保険種別名称を欄外に印字する/050318
005610 01 保険種別印刷Ｗ                       PIC 9    VALUE ZERO.
005620 01 保険種別名称Ｗ                       PIC N(4) VALUE SPACE.
005620 01 老健名称Ｗ.
          03 老名称Ｗ                          PIC N(1) VALUE SPACE.
          03 健保名称Ｗ                        PIC N(1) VALUE SPACE.
          03 FILLER                            PIC X(4) VALUE SPACE.
005620 01 助成種別名称Ｗ                       PIC N(3) VALUE SPACE.
005630*
005640 01 遅延フラグ                         PIC X(3) VALUE SPACE.
005650 01 遅延ＣＮＴ                         PIC 9(5) VALUE ZERO.
005660 01 遅延カウンタ                       PIC 9(4) VALUE ZERO.
005670 01 遅延回数Ｗ                         PIC 9(4) VALUE ZERO.
      *
      */詳細時の部位の印刷のあるなしを制御する/0511
       01 部位印刷有フラグ                   PIC X(3) VALUE SPACE.
      *
       01 複合プログラム名Ｗ               PIC X(8) VALUE "MOJI2".
005680*
005690 01 印刷制御.
005700     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
005710     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
005720     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
005730     03 拡張制御Ｐ.
005740         05 端末制御Ｐ.
005750             07 移動方向Ｐ             PIC X(1).
005760             07 移動行数Ｐ             PIC 9(3).
005770         05 詳細制御Ｐ                 PIC X(2).
005780     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
005790     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
005800*
005810 01 計算機西暦年Ｗ                     PIC 9(2).
005820* 日付ＷＯＲＫ
005830 01 和暦終了年Ｗ                       PIC 9(4).
005840 01 計算機西暦.
005850    03 計算機西暦年                    PIC 9(4).
005860    03 計算機西暦月日                  PIC 9(4).
005870 01 計算機西暦Ｒ REDEFINES 計算機西暦.
005880    03 計算機世紀                      PIC 9(2).
005890    03 計算機日付                      PIC 9(6).
005900    03 計算機日付Ｒ REDEFINES 計算機日付.
005910       05 計算機年月                   PIC 9(4).
005920       05 計算機年月Ｒ REDEFINES 計算機年月.
005930         07 計算機年                   PIC 9(2).
005940         07 計算機月                   PIC 9(2).
005950       05 計算機日                     PIC 9(2).
005960*
005970*/年月毎持つ保険者データのワーク
005980 01 保負担率区分Ｗ                     PIC 9    VALUE ZERO.
005990 01 保本人負担率Ｗ                     PIC 9(3) VALUE ZERO.
006000 01 保家族負担率Ｗ                     PIC 9(3) VALUE ZERO.
006010 01 保本人負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
006020 01 保家族負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
006030*
006040 01 比較和暦年月Ｗ.
006050    03 比較和暦Ｗ                      PIC 9    VALUE ZERO.
006060    03 比較年Ｗ                        PIC 9(2) VALUE ZERO.
006070    03 比較月Ｗ                        PIC 9(2) VALUE ZERO.
006080*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
006090******************************************************************
006100*                          連結項目                              *
006110******************************************************************
006120*
006130**********************
006140* メッセージ表示キー *
006150**********************
006160 01 連メ－キー IS EXTERNAL.
006170    03  連メ－メッセージ                 PIC N(20).
006180*
006290************
006300* 印刷キー *
006310************
006320 01 連印－対象データ IS EXTERNAL.
006330    03 連印－施術和暦年月.
006340       05 連印－施術和暦                  PIC 9(1).
006350       05 連印－施術年                    PIC 9(2).
006360       05 連印－施術月                    PIC 9(2).
006370    03 連印－保険種別                     PIC 9(2).
006380    03 連印－保険者番号                   PIC X(10).
006390    03 連印－本人家族区分                 PIC 9(1).
006400    03 連印－被保険者カナ                 PIC X(20).
006410    03 連印－患者コード.
006420       05 連印－患者番号                  PIC 9(6).
006430       05 連印－枝番                      PIC X(1).
006440    03 連印－印刷モードＦ                 PIC 9(1).
006450*/詳細
006460    03 連印－保険証印刷区分               PIC 9(1).
006470    03 連印－負傷詳細 OCCURS 7.
006480       05 連印－負傷印刷行                PIC 9(1).
006490       05 連印－部位印刷区分              PIC 9(1).
006500       05 連印－転帰印刷区分              PIC 9(1).
006510       05 連印－原因印刷区分              PIC 9(1).
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
      *
002900 01 連印新－対象データＹＮＳ６６０ IS EXTERNAL.
          03 連印新－症状文  OCCURS 5.
             05 連印新－症状                    PIC X(120).
005540    03 連印新－同意年月日.
005550       05 連印新－同意年                  PIC 9(2).
005560       05 連印新－同意月                  PIC 9(2).
005570       05 連印新－同意日                  PIC 9(2).
          03 連印新－同意医師名                 PIC N(10).
          03 連印新－症状開始行                 PIC 9(2).
          03 連印新－症状印刷区分               PIC 9(1) OCCURS 5.
006990*
       01 連入－表示フラグ６６０ IS EXTERNAL GLOBAL.
          03 連入－プレビュー区分               PIC 9(1).
      *
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
006810* 負担率取得用14/10～
006820 01 連率－負担率取得キー IS EXTERNAL.
006830    03 連率－施術和暦年月.
006840       05 連率－施術和暦               PIC 9.
006850       05 連率－施術年月.
006860          07 連率－施術年              PIC 9(2).
006870          07 連率－施術月              PIC 9(2).
006880    03 連率－患者コード.
006890       05 連率－患者番号               PIC 9(6).
006900       05 連率－枝番                   PIC X.
006910    03 連率－実際負担率                PIC 9(3).
006920    03 連率－実際本体負担率            PIC 9(3).
006930    03 連率－健保負担率                PIC 9(3).
006940    03 連率－２７老負担率              PIC 9(3).
006950    03 連率－助成負担率                PIC 9(3).
006960    03 連率－特別用負担率              PIC 9(3).
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
007160*
007170******************************************************************
007180*                      PROCEDURE  DIVISION                       *
007190******************************************************************
007200 PROCEDURE               DIVISION.
007210************
007220*           *
007230* 初期処理   *
007240*           *
007250************
002570     PERFORM プリンタファイル作成.
007260     PERFORM 初期化.
007270************
007280*           *
007290* 主処理     *
007300*           *
007310************
007320* 印刷
007330     PERFORM 連結項目待避.
007340     PERFORM 印刷セット
007350     PERFORM 印刷処理
007360************
007370*           *
007380* 終了処理   *
007390*           *
007400************
007410     PERFORM 終了処理.
007420     EXIT PROGRAM.
007430*
007440*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YAI662"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
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
007450*================================================================*
007460 初期化 SECTION.
007470*
007480     PERFORM ファイルオープン.
007490*    /* 現在日付取得 */
007500     ACCEPT 計算機日付 FROM DATE.
007510*    /* 1980～2079年の間で設定 */
007520     IF 計算機年 > 80
007530         MOVE 19 TO 計算機世紀
007540     ELSE
007550         MOVE 20 TO 計算機世紀
007560     END-IF.
007570*     PERFORM カレント元号取得.
007580     PERFORM 制御情報取得.
007590     PERFORM 和暦終了年取得.
007600     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
007610*================================================================*
007620* カレント元号取得 SECTION.
007630 制御情報取得 SECTION.
007640*
007650     MOVE ZEROS TO 制－制御区分.
007660     READ 制御情報マスタ
007670     NOT INVALID KEY
007680         MOVE 制－カレント元号               TO カレント元号Ｗ
007690         MOVE 制－カルテ負傷原因印刷区分     TO 負傷原因印刷区分Ｗ
007700         MOVE 制－バーコード２３２Ｃ使用区分 TO バーコード使用区分Ｗ
007710         MOVE 制－遅延回数                   TO 遅延回数Ｗ
007720     END-READ.
007730*
007810*================================================================*
007820 和暦終了年取得 SECTION.
007830*
007840     MOVE カレント元号Ｗ TO 元－元号区分.
007850     READ 元号マスタ
007860     INVALID KEY
007870         DISPLAY NC"指定和暦が登録されていません" UPON CONS
007880         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
007890                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007900         ACCEPT  キー入力 FROM CONS
007910         PERFORM 終了処理
007920         EXIT PROGRAM
007930     NOT INVALID KEY
007940         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
007950         MOVE 前和暦Ｗ TO 元－元号区分
007960         READ 元号マスタ
007970         INVALID KEY
007980             DISPLAY NC"指定和暦が登録されていません" UPON CONS
007990             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008000                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
008010             ACCEPT  キー入力 FROM CONS
008020             PERFORM 終了処理
008030             EXIT PROGRAM
008040         NOT INVALID KEY
008050             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
008060         END-READ
008070     END-READ.
008080*
008090*================================================================*
008100 連結項目待避 SECTION.
008110*
008120     INITIALIZE 対象データＷＲ.
008130     MOVE 連印－施術和暦      TO 施術和暦ＷＲ.
008140     MOVE 連印－施術年        TO 施術年ＷＲ.
008150     MOVE 連印－施術月        TO 施術月ＷＲ.
008160     MOVE 連印－保険種別      TO 保険種別ＷＲ.
008170     MOVE 連印－保険者番号    TO 保険者番号ＷＲ.
008180     MOVE 連印－本人家族区分  TO 本人家族区分ＷＲ.
008190     MOVE 連印－被保険者カナ  TO 被保険者カナＷＲ.
008200     MOVE 連印－患者番号      TO 患者番号ＷＲ.
008210     MOVE 連印－枝番          TO 枝番ＷＲ.
008220     MOVE 連印－印刷モードＦ  TO 印刷モードＦＷＲ.
006290     MOVE 連印－部位丸付け    TO 丸付けＷＲ.
008270*================================================================*
008280 ファイルオープン SECTION.
008290*
008300     OPEN INPUT   保険者マスタ
008310         MOVE NC"保険者" TO ファイル名.
008320         PERFORM オープンチェック.
009050     OPEN INPUT 市町村マスタ.
009060         MOVE NC"市町村" TO ファイル名.
009070         PERFORM オープンチェック.
008330     OPEN INPUT   元号マスタ
008340         MOVE NC"元号" TO ファイル名.
008350         PERFORM オープンチェック.
008360     OPEN INPUT   名称マスタ
008370         MOVE NC"名称" TO ファイル名.
008380         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
008420     OPEN INPUT   制御情報マスタ
008430         MOVE NC"制御情報" TO ファイル名.
008440         PERFORM オープンチェック.
008540     OPEN INPUT   受診者情報Ｆ.
008550         MOVE NC"受情" TO ファイル名.
008560         PERFORM オープンチェック.
008570     OPEN INPUT   施術記録Ｆ.
008580         MOVE NC"施記Ｆ" TO ファイル名.
008590         PERFORM オープンチェック.
008600     OPEN INPUT   負傷データＦ.
008610         MOVE NC"負傷" TO ファイル名.
008620         PERFORM オープンチェック.
008660     OPEN INPUT  負傷原因Ｆ.
008670         MOVE NC"負傷原因" TO ファイル名.
008680         PERFORM オープンチェック.
           OPEN INPUT 事業所マスタ.
                   MOVE NC"事業" TO ファイル名.
                   PERFORM オープンチェック.
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
008690     OPEN I-O   印刷ファイル
008700         MOVE NC"印刷" TO ファイル名.
008710         PERFORM エラー処理Ｐ.
008720*================================================================*
008730 オープンチェック SECTION.
008740*
008750     IF 状態キー  NOT =  "00"
008760         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
008770         DISPLAY NC"状態キー：" 状態キー         UPON CONS
008780         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008790                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
008800         ACCEPT  キー入力 FROM CONS
008810         PERFORM ファイル閉鎖
008820         EXIT PROGRAM.
008830*================================================================*
008840 ファイル閉鎖 SECTION.
008850*
008860     CLOSE 元号マスタ    保険者マスタ    市町村マスタ 
008870           レセプトＦ    制御情報マスタ  負傷原因Ｆ
008880           名称マスタ    受診者情報Ｆ    事業所マスタ
008890           負傷データＦ  施術記録Ｆ      労災情報Ｆ
                 生保情報Ｆ    自賠責情報Ｆ    保険会社マスタ
                 印刷ファイル.
008910*================================================================*
008920 終了処理 SECTION.
008930*
008940     PERFORM ファイル閉鎖.
008950*================================================================*
008960 印刷セット SECTION.
008970*
008980     EVALUATE 印刷モードＦＷＲ
008990     WHEN 0
009000         PERFORM 印刷セット１
009010     WHEN 1
009020         PERFORM 印刷セット２
009030     WHEN 2
009040         PERFORM 印刷セット３
009050*     WHEN 3
003970*         MOVE  NC"新用紙の金額行はカルテ裏で印刷して下さい" TO 連メ－メッセージ
013150*         CALL   "MSG001"
013160*         CANCEL "MSG001"
008820*         EXIT PROGRAM
009070*/詳細モード用
009080     WHEN 4
009090         PERFORM 印刷セット５
009100     END-EVALUATE.
      *
      ************     PERFORM テスト印字.
009110*
009120*================================================================*
009130 印刷セット１ SECTION.
009140*
009150     INITIALIZE YAI662P.
009160     INITIALIZE 受診者情報Ｗ.
009170     INITIALIZE 事業所情報Ｗ.
009180     INITIALIZE 請求先情報Ｗ.
009190     INITIALIZE 負傷情報Ｗ.
009220     PERFORM 受診者情報取得.
009200     PERFORM 負担率取得.
009210     PERFORM 請求先情報取得.
009230     PERFORM 負傷データ取得.
009240*
009250     IF 負傷原因印刷区分Ｗ  NOT = 1 
009260        PERFORM 負傷原因取得
009270     END-IF.
009280*
009290     PERFORM 印刷上部セット.
009300     PERFORM 印刷下部セット.
009310*================================================================*
009320 印刷セット２ SECTION.
009330*
009340     INITIALIZE YAI662P.
009350     INITIALIZE 受診者情報Ｗ.
009360     INITIALIZE 事業所情報Ｗ.
009370     INITIALIZE 請求先情報Ｗ.
009380     INITIALIZE 負傷情報Ｗ.
009410     PERFORM 受診者情報取得.
009390     PERFORM 負担率取得.
009400     PERFORM 請求先情報取得.
009420     PERFORM 印刷上部セット.
009430*================================================================*
009440 印刷セット３ SECTION.
009450*
009460     INITIALIZE YAI662P.
009470     INITIALIZE 受診者情報Ｗ.
009480     INITIALIZE 事業所情報Ｗ.
009490     INITIALIZE 請求先情報Ｗ.
009500     INITIALIZE 負傷情報Ｗ.
009410     PERFORM 受診者情報取得.
009510     PERFORM 負傷データ取得.
009520*
009530     IF 負傷原因印刷区分Ｗ  NOT = 1 
009540        PERFORM 負傷原因取得
009550     END-IF.
009560*
009570     PERFORM 印刷下部セット.
009720*================================================================*
009730 印刷セット５ SECTION.
009740*
009750*/詳細モード用
009760     MOVE SPACE TO YAI662P.
009770     INITIALIZE YAI662P.
009780*     MOVE "X" TO EDIT-MODE OF 患者コード.
009790     INITIALIZE 受診者情報Ｗ.
009800     INITIALIZE 事業所情報Ｗ.
009810     INITIALIZE 請求先情報Ｗ.
009820     INITIALIZE 負傷情報Ｗ.
009830     INITIALIZE 負傷原因情報Ｗ.
009860     PERFORM 受診者情報取得.
009840     PERFORM 負担率取得.
009850     PERFORM 請求先情報取得.
009870     PERFORM 負傷データ取得.
009880*
009890     IF 負傷原因印刷区分Ｗ  NOT = 1
009900        PERFORM 詳細モード負傷原因処理
009910        PERFORM 負傷原因取得
009920     END-IF.
      */負傷原因の詳細印刷/051125
           PERFORM 原因行セット.
009930*
009940     IF 連印－保険証印刷区分 = 1
009950         PERFORM 印刷上部セット
009960     END-IF.
009970*
009980     PERFORM 詳細モード負傷処理.
009990     PERFORM 印刷下部セット.
010010*
010020*================================================================*
010030 印刷上部セット SECTION.
010040*
010050*/バーコードは使用時のみ印刷する/050311
010060*     MOVE 患者コードＷ      TO 患者コード.
010070     IF バーコード使用区分Ｗ NOT = ZERO
010080        MOVE 患者コードＷ     TO 患者コード
010090        MOVE SPACE            TO EDIT-MODE OF 患者コード
010100     ELSE
010110         MOVE "X" TO EDIT-MODE OF 患者コード
010120     END-IF.
010130*
010140     MOVE 患者番号Ｗ        TO 患者番号.
010150     MOVE 枝番Ｗ            TO 枝番.
010160**************************
010170* 被保険者証情報セット   *
010180**************************
010190     IF 記号Ｗ(1:2) = "＊" 
010200        MOVE  SPACE    TO  記号
010210     ELSE
010220        MOVE 記号Ｗ    TO  記号
010230     END-IF.
010240     IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
010250        ( 印刷番号Ｗ(1:2) = "＊" )
010260        MOVE  SPACE      TO  番号
010270     ELSE
      */労働保険番号を印刷/0508
               IF 受－保険種別 = 70
                   MOVE 印刷番号Ｗ TO 労働保険番号
               ELSE
010280             MOVE 印刷番号Ｗ TO 番号
               END-IF
010290     END-IF.
010300************************
010310* 被保険者情報セット   *
010320************************
010330     MOVE 被保険者カナＷ             TO 被保険者カナ.
010330     MOVE 被保険者氏名Ｗ             TO 被保険者氏名.
010340     MOVE 郵便番号１Ｗ               TO 郵便番号１.
010350     MOVE 郵便番号２Ｗ               TO 郵便番号２.
           IF ( 郵便番号１Ｗ NOT = SPACE) OR
              ( 郵便番号２Ｗ NOT = SPACE)
010360         MOVE "-"                    TO 被保険者郵便区切
           END-IF.
010370     MOVE 印刷住所１Ｗ               TO 住所１.
010380     MOVE 印刷住所２Ｗ               TO 住所２.
010390     MOVE 電話番号Ｗ                 TO 電話番号.
010440     MOVE 資格昭和チェックＷ         TO 資格昭和チェック.
010450     MOVE 資格平成チェックＷ         TO 資格平成チェック.
010460     MOVE 資格取得年Ｗ               TO 資格取得年.
010470     MOVE 資格取得月Ｗ               TO 資格取得月.
010480     MOVE 資格取得日Ｗ               TO 資格取得日.
010490*/有効期限
010500     MOVE 有効期限年Ｗ               TO 有効年.
010510     MOVE 有効期限月Ｗ               TO 有効月.
010520     MOVE 有効期限日Ｗ               TO 有効日.
010800********************
010810* 患者情報セット   *
010820********************
010830     MOVE 患者カナＷ                 TO 患者カナ.
010840     MOVE 患者氏名Ｗ                 TO 患者氏名.
010860     MOVE 明治チェックＷ             TO 明治チェック.
010870     MOVE 大正チェックＷ             TO 大正チェック.
010880     MOVE 昭和チェックＷ             TO 昭和チェック.
010890     MOVE 平成チェックＷ             TO 平成チェック.
010900     MOVE 患者年Ｗ                   TO 患者年.
010910     MOVE 患者月Ｗ                   TO 患者月.
010920     MOVE 患者日Ｗ                   TO 患者日.
010930     MOVE 患者年齢Ｗ                 TO 患者年齢.
010890     MOVE 男チェックＷ               TO 男チェック.
010890     MOVE 女チェックＷ               TO 女チェック.
010940     MOVE 印刷続柄Ｗ                 TO 続柄.
010950********************
010960* 事業所情報セット *
010970********************
010980     MOVE 事業所名称Ｗ               TO 事業所名称.
010990     MOVE 事業所郵便番号１Ｗ         TO 事業所郵便番号１２.
011000     MOVE 事業所郵便番号２Ｗ         TO 事業所郵便番号２２.
           IF (事業所郵便番号１Ｗ NOT = SPACE) OR
              (事業所郵便番号２Ｗ NOT = SPACE)
               MOVE "-"                    TO 事業所郵便区切
           END-IF.
011010     MOVE 事業所住所Ｗ               TO 事業所住所.
011010     MOVE 事業所住所２Ｗ             TO 事業所住所２.
011020************************************
011030* 都道府県・健康保険組合情報セット *
011040************************************
011050*労災、自費の時は保険者情報は印字しない
011060     IF 保険種別ＷＲ = 70 OR 90
011070         MOVE SPACE                      TO 保険者番号
011080         MOVE SPACE                      TO 請求先名称１
011080         MOVE SPACE                      TO 請求先名称２
011110         MOVE SPACE                      TO 請求先郵便番号１
011120         MOVE SPACE                      TO 保険者郵便区切
011130         MOVE SPACE                      TO 請求先郵便番号２
011140         MOVE SPACE                      TO 請求先住所１
011150         MOVE SPACE                      TO 請求先住所２
011300         MOVE SPACE                      TO 福祉チェック
011310         MOVE SPACE                      TO 乳児チェック
011320         MOVE SPACE                      TO 母子チェック
011330         MOVE SPACE                      TO 障害チェック
011200     ELSE
011210         MOVE 印刷保険者番号Ｗ           TO 保険者番号
011220*         MOVE 印刷請求先名称Ｗ           TO 請求先名称
               IF 請求先名称２Ｗ = SPACE
                   MOVE 請求先名称１Ｗ         TO 請求先名称１
                   MOVE SPACE                  TO 請求先名称２
               ELSE
                   MOVE 請求先名称１Ｗ         TO 請求先名称２
                   MOVE 請求先名称２Ｗ         TO 請求先名称１
               END-IF
011250         MOVE 請求先郵便番号１Ｗ         TO 請求先郵便番号１
011270         MOVE 請求先郵便番号２Ｗ         TO 請求先郵便番号２
               IF (請求先郵便番号１Ｗ NOT = SPACE) OR
                  (請求先郵便番号２Ｗ NOT = SPACE)
011260             MOVE "-"                        TO 保険者郵便区切
               END-IF
011280         MOVE 印刷請求先住所１Ｗ         TO 請求先住所１
011290         MOVE 印刷請求先住所２Ｗ         TO 請求先住所２
011310         MOVE 乳児チェックＷ             TO 乳児チェック
011320         MOVE 母子チェックＷ             TO 母子チェック
011330         MOVE 障害チェックＷ             TO 障害チェック
011330         MOVE 福祉チェックＷ             TO 福祉チェック
011330         MOVE その他Ｗ                   TO その他
011340     END-IF.
011350*/保険種別名称を欄外に印字する/050318
011370*     MOVE 保険種別名称Ｗ                 TO 保険種別名称.
011370*     MOVE 助成種別名称Ｗ                 TO 助成種別名称.
011300     MOVE 国保チェックＷ                 TO 国保チェック.
011310     MOVE 社保チェックＷ                 TO 社保チェック.
011320     MOVE 組合チェックＷ                 TO 組合チェック.
011330     MOVE 共済チェックＷ                 TO 共済チェック.
011300     MOVE 船員チェックＷ                 TO 船員チェック.
011310     MOVE 労災チェックＷ                 TO 労災チェック.
011320     MOVE 自賠チェックＷ                 TO 自賠チェック.
011330     MOVE 退職チェックＷ                 TO 退職チェック.
011300     MOVE 後高チェックＷ                 TO 後高チェック.
011310     MOVE 前高チェックＷ                 TO 前高チェック.
011320     MOVE 日雇チェックＷ                 TO 日雇チェック.
011300     MOVE 生保チェックＷ                 TO 生保チェック.
011310     MOVE 自費チェックＷ                 TO 自費チェック.
      *     MOVE 印刷負担率Ｗ                   TO 負担率.
011310     MOVE ０割チェックＷ                 TO ０割チェック.
011310     MOVE １割チェックＷ                 TO １割チェック.
011310     MOVE ２割チェックＷ                 TO ２割チェック.
011310     MOVE ３割チェックＷ                 TO ３割チェック.
           IF ( 公費種別Ｗ = 5 ) AND ( 受－施術和暦年月 < 42004 )
               MOVE 市町村番号Ｗ               TO 負担者番号
               MOVE 受益者番号Ｗ               TO 受給者番号
           ELSE
               IF 助成種別Ｗ NOT = ZERO
                   MOVE 負担者番号Ｗ           TO 負担者番号
                   MOVE 受益者番号助成Ｗ       TO 受給者番号
               END-IF
           END-IF.
011410*================================================================*
011420 印刷下部セット SECTION.
011430*
011440********************
011450* 負傷データセット *
011460********************
011470* １部位 *
011480**********
011490*     MOVE 会ＯＣＲコードＷ(1) TO 負傷コード１.
011500     MOVE 負傷名Ｗ(1)         TO 負傷名１.
011510     MOVE 負傷年Ｗ(1)         TO 負傷年１.
011520     MOVE 負傷月Ｗ(1)         TO 負傷月１.
011530     MOVE 負傷日Ｗ(1)         TO 負傷日１.
011540     MOVE 施術開始年Ｗ(1)     TO 施術開始年１.
011550     MOVE 施術開始月Ｗ(1)     TO 施術開始月１.
011560     MOVE 施術開始日Ｗ(1)     TO 施術開始日１.
011570     MOVE 施術終了年Ｗ(1)     TO 施術終了年１.
011580     MOVE 施術終了月Ｗ(1)     TO 施術終了月１.
011590     MOVE 施術終了日Ｗ(1)     TO 施術終了日１.
011630     MOVE 治癒チェックＷ(1)   TO 治癒チェック１.
011640     MOVE 中止チェックＷ(1)   TO 中止チェック１.
011650     MOVE 転医チェックＷ(1)   TO 転医チェック１.
011660*     ******************
011670*     * 日数は一時保留 *
011680*     ******************
011690*     **********************
011700*     * 施術回数は一時保留 *
011710*     **********************
011720*     **********************
011730*     * 転帰名称は一時保留 *
011740*     **********************
011750**********
011760* ２部位 *
011770**********
011780*     MOVE 会ＯＣＲコードＷ(2) TO 負傷コード２.
011790     MOVE 負傷名Ｗ(2)         TO 負傷名２.
011800     MOVE 負傷年Ｗ(2)         TO 負傷年２.
011810     MOVE 負傷月Ｗ(2)         TO 負傷月２.
011820     MOVE 負傷日Ｗ(2)         TO 負傷日２.
011830     MOVE 施術開始年Ｗ(2)     TO 施術開始年２.
011840     MOVE 施術開始月Ｗ(2)     TO 施術開始月２.
011850     MOVE 施術開始日Ｗ(2)     TO 施術開始日２.
011860     MOVE 施術終了年Ｗ(2)     TO 施術終了年２.
011870     MOVE 施術終了月Ｗ(2)     TO 施術終了月２.
011880     MOVE 施術終了日Ｗ(2)     TO 施術終了日２.
011920     MOVE 治癒チェックＷ(2)   TO 治癒チェック２.
011930     MOVE 中止チェックＷ(2)   TO 中止チェック２.
011940     MOVE 転医チェックＷ(2)   TO 転医チェック２.
011950*     ******************
011960*     * 日数は一時保留 *
011970*     ******************
011980*     **********************
011990*     * 施術回数は一時保留 *
012000*     **********************
012010*     **********************
012020*     * 転帰名称は一時保留 *
012030*     **********************
012040**********
012050* ３部位 *
012060**********
012070*     MOVE 会ＯＣＲコードＷ(3) TO 負傷コード３.
012080     MOVE 負傷名Ｗ(3)         TO 負傷名３.
012090     MOVE 負傷年Ｗ(3)         TO 負傷年３.
012100     MOVE 負傷月Ｗ(3)         TO 負傷月３.
012110     MOVE 負傷日Ｗ(3)         TO 負傷日３.
012120     MOVE 施術開始年Ｗ(3)     TO 施術開始年３.
012130     MOVE 施術開始月Ｗ(3)     TO 施術開始月３.
012140     MOVE 施術開始日Ｗ(3)     TO 施術開始日３.
012150     MOVE 施術終了年Ｗ(3)     TO 施術終了年３.
012160     MOVE 施術終了月Ｗ(3)     TO 施術終了月３.
012170     MOVE 施術終了日Ｗ(3)     TO 施術終了日３.
012210     MOVE 治癒チェックＷ(3)   TO 治癒チェック３.
012220     MOVE 中止チェックＷ(3)   TO 中止チェック３.
012230     MOVE 転医チェックＷ(3)   TO 転医チェック３.
012240*     ******************
012250*     * 日数は一時保留 *
012260*     ******************
012270*     **********************
012280*     * 施術回数は一時保留 *
012290*     **********************
012300*     **********************
012310*     * 転帰名称は一時保留 *
012320*     **********************
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
012500     MOVE 治癒チェックＷ(4)   TO 治癒チェック４.
012510     MOVE 中止チェックＷ(4)   TO 中止チェック４.
012520     MOVE 転医チェックＷ(4)   TO 転医チェック４.
012530*     ******************
012540*     * 日数は一時保留 *
012550*     ******************
012560*     **********************
012570*     * 施術回数は一時保留 *
012580*     **********************
012590*     **********************
012600*     * 転帰名称は一時保留 *
012610*     **********************
012620**********
012630* ５部位 *
012640**********
012650*     MOVE 会ＯＣＲコードＷ(5) TO 負傷コード５.
012660     MOVE 負傷名Ｗ(5)         TO 負傷名５.
012670     MOVE 負傷年Ｗ(5)         TO 負傷年５.
012680     MOVE 負傷月Ｗ(5)         TO 負傷月５.
012690     MOVE 負傷日Ｗ(5)         TO 負傷日５.
012700     MOVE 施術開始年Ｗ(5)     TO 施術開始年５.
012710     MOVE 施術開始月Ｗ(5)     TO 施術開始月５.
012720     MOVE 施術開始日Ｗ(5)     TO 施術開始日５.
012730     MOVE 施術終了年Ｗ(5)     TO 施術終了年５.
012740     MOVE 施術終了月Ｗ(5)     TO 施術終了月５.
012750     MOVE 施術終了日Ｗ(5)     TO 施術終了日５.
012790     MOVE 治癒チェックＷ(5)   TO 治癒チェック５.
012800     MOVE 中止チェックＷ(5)   TO 中止チェック５.
012810     MOVE 転医チェックＷ(5)   TO 転医チェック５.
012820*/負傷原因の印字
012830     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
012840     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
012850     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
012860     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
012870     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
012880     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
012890     MOVE 負傷原因Ｗ(7)       TO 負傷原因７.
012900     MOVE 負傷原因Ｗ(8)       TO 負傷原因８.
012910*     ******************
012920*     * 日数は一時保留 *
012930*     ******************
012940*     **********************
012950*     * 施術回数は一時保留 *
012960*     **********************
012970*     **********************
012980*     * 転帰名称は一時保留 *
012990*     **********************
      * 症状印字
      */症状の詳細印刷/051125
      *     IF 印刷モードＦＷＲ = 4
      *         PERFORM 症状詳細セット
      *     END-IF.
      *     PERFORM 症状セット
      *     IF 連印新－症状開始行 NOT = ZERO
      *         MOVE 連印新－症状開始行 TO カウンタ２
      *     ELSE
      *         MOVE 1                  TO カウンタ２
      *     END-IF.
      *     PERFORM VARYING カウンタ FROM 1 BY 1
      *             UNTIL ( カウンタ > 7 ) OR (カウンタ２ > 7)
      *         MOVE 症状Ｗ(カウンタ) TO 症状(カウンタ２)
      *         COMPUTE カウンタ２ = カウンタ２ + 1
      *     END-PERFORM.
      */症状詳細印刷↑↑↑/051125
      * 同意印字
      */詳細時は印刷しない/0511
      *     IF 印刷モードＦＷＲ NOT = 4
      *         MOVE 連印新－同意年      TO 同意年
      *         MOVE 連印新－同意月      TO 同意月
      *         MOVE 連印新－同意日      TO 同意日
      *         MOVE 連印新－同意医師名  TO 医師名
      *     END-IF.
010910*
      **************
      * 部位丸付け *
      **************
      *     IF 丸付けＷＲ = 1
015090*         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
015100*                 UNTIL ( 部位ＣＮＴ > 5 )
      */↓↓↓詳細モード時、指定していない部位まで○が付く/0511
      * 詳細印刷の場合、ここで言う部位ＣＮＴは元の部位番号とは違う。
      * 連印－負傷印刷行に部位ＣＮＴの値が入っている部位がある場合に印刷する。
      * 重複はありえない(画面ＰＧでエラーとしてはじく為)
      *             MOVE "YES" TO 部位印刷有フラグ
      *             IF 印刷モードＦＷＲ = 4
      *                 MOVE SPACE TO 部位印刷有フラグ
      *                 PERFORM VARYING カウンタ FROM 1 BY 1
      *                         UNTIL ( カウンタ > 5 )
      *                      IF 連印－負傷印刷行(カウンタ) = 部位ＣＮＴ
      */○付けは負傷印刷と連動させる/051203
      *                      AND 連印－部位印刷区分(カウンタ) = 1
      *                          MOVE "YES" TO 部位印刷有フラグ
      *                      END-IF
      *                 END-PERFORM
      *             END-IF
      *             IF 部位印刷有フラグ = "YES"
      */↑↑↑詳細モード時、指定していない部位まで○が付く/0511
      */051203
      *                 EVALUATE 部位ＣＮＴ
      *                 WHEN 1
      *                     MOVE NC"１"          TO 部位番号Ｗ
      *                 WHEN 2
      *                     MOVE NC"２"          TO 部位番号Ｗ
      *                 WHEN 3
      *                     MOVE NC"３"          TO 部位番号Ｗ
      *                 WHEN 4
      *                     MOVE NC"４"          TO 部位番号Ｗ
      *                 WHEN 5
      *                     MOVE NC"５"          TO 部位番号Ｗ
      *                 WHEN OTHER
      *                     MOVE SPACE           TO 部位番号Ｗ
      *                 END-EVALUATE
      *                 EVALUATE 部位番号ＷＰ(部位ＣＮＴ)
      *                 WHEN 010
      *                     IF 負傷種別ＷＰ(部位ＣＮＴ) = 09
      *                         MOVE SPACE       TO 頭部
      *                     ELSE
      *                         MOVE NC"○"      TO 頭部
      *                         MOVE 部位番号Ｗ  TO 頭部番号
      *                     END-IF
      *                 WHEN 021
      *                 WHEN 022
      *                 WHEN 023
      *                     MOVE NC"○"      TO 顔面部
      *                     MOVE 部位番号Ｗ  TO 顔面番号
      *                 WHEN 041
      *                     MOVE NC"○"      TO 右顎関節
      *                     MOVE 部位番号Ｗ  TO 右顎番号
      *                 WHEN 042
      *                     MOVE NC"○"      TO 左顎関節
      *                     MOVE 部位番号Ｗ  TO 左顎番号
      *                 WHEN 050
      *                     MOVE NC"○"      TO 頚部
      *                     MOVE 部位番号Ｗ  TO 頚部番号
      *                 WHEN 060
      *                     MOVE NC"○"      TO 胸部
      *                     MOVE 部位番号Ｗ  TO 胸部番号
      *                 WHEN 061
      *                     MOVE NC"○"      TO 右胸部
      *                     MOVE 部位番号Ｗ  TO 右胸番号
      *                 WHEN 062
      *                     MOVE NC"○"      TO 左胸部
      *                     MOVE 部位番号Ｗ  TO 左胸番号
      *                 WHEN 081
      *                     MOVE NC"○"      TO 右肋骨
      *                     MOVE 部位番号Ｗ  TO 右肋骨番号
      *                 WHEN 082
      *                     MOVE NC"○"      TO 左肋骨
      *                     MOVE 部位番号Ｗ  TO 左肋骨番号
      *                 WHEN 111
      *                 WHEN 112
      *                     MOVE NC"○"      TO 背部上
      *                     MOVE 部位番号Ｗ  TO 背部上番号
      *                 WHEN 120
      *                     MOVE NC"○"      TO 背部
      *                     MOVE 部位番号Ｗ  TO 背部番号
      *                 WHEN 131
      *                 WHEN 132
      *                     MOVE NC"○"      TO 背部下
      *                     MOVE 部位番号Ｗ  TO 背部下番号
      *                 WHEN 140
      *                     MOVE NC"○"      TO 腰部
      *                     MOVE 部位番号Ｗ  TO 腰部番号
      *                 WHEN 160
      *                     MOVE NC"○"      TO 臀部
      *                     MOVE 部位番号Ｗ  TO 臀部番号
      *                 WHEN 161
      *                     MOVE NC"○"      TO 右臀部
      *                     MOVE 部位番号Ｗ  TO 右臀部番号
      *                 WHEN 162
      *                     MOVE NC"○"      TO 左臀部
      *                     MOVE 部位番号Ｗ  TO 左臀部番号
      *                 WHEN 201
      *                     MOVE NC"○"      TO 右肩部
      *                     MOVE 部位番号Ｗ  TO 右肩番号
      *                 WHEN 202
      *                     MOVE NC"○"      TO 左肩部
      *                     MOVE 部位番号Ｗ  TO 左肩番号
      *                 WHEN 221
      *                     MOVE NC"○"      TO 右上腕部上
      *                     MOVE 部位番号Ｗ  TO 右上腕上番号
      *                 WHEN 222
      *                     MOVE NC"○"      TO 左上腕部上
      *                     MOVE 部位番号Ｗ  TO 左上腕上番号
      *                 WHEN 231
      *                     MOVE NC"○"      TO 右上腕部
      *                     MOVE 部位番号Ｗ  TO 右上腕番号
      *                 WHEN 232
      *                     MOVE NC"○"      TO 左上腕部
      *                     MOVE 部位番号Ｗ  TO 左上腕番号
      *                 WHEN 241
      *                     MOVE NC"○"      TO 右上腕部下
      *                     MOVE 部位番号Ｗ  TO 右上腕下番号
      *                 WHEN 242
      *                     MOVE NC"○"      TO 左上腕部下
      *                     MOVE 部位番号Ｗ  TO 左上腕下番号
      *                 WHEN 251
      *                     MOVE NC"○"      TO 右肘部
      *                     MOVE 部位番号Ｗ  TO 右肘番号
      *                 WHEN 252
      *                     MOVE NC"○"      TO 左肘部
      *                     MOVE 部位番号Ｗ  TO 左肘番号
      *                 WHEN 271
      *                     MOVE NC"○"      TO 右前腕部上
      *                     MOVE 部位番号Ｗ  TO 右前腕上番号
      *                 WHEN 272
      *                     MOVE NC"○"      TO 左前腕部上
      *                     MOVE 部位番号Ｗ  TO 左前腕上番号
      *                 WHEN 281
      *                     MOVE NC"○"      TO 右前腕部
      *                     MOVE 部位番号Ｗ  TO 右前腕番号
      *                 WHEN 282
      *                     MOVE NC"○"      TO 左前腕部
      *                     MOVE 部位番号Ｗ  TO 左前腕番号
      *                 WHEN 291
      *                     MOVE NC"○"      TO 右前腕部下
      *                     MOVE 部位番号Ｗ  TO 右前腕下番号
      *                 WHEN 292
      *                     MOVE NC"○"      TO 左前腕部下
      *                     MOVE 部位番号Ｗ  TO 左前腕下番号
      *                 WHEN 301
      *                     MOVE NC"○"      TO 右手関節
      *                     MOVE 部位番号Ｗ  TO 右手関節番号
      *                 WHEN 302
      *                     MOVE NC"○"      TO 左手関節
      *                     MOVE 部位番号Ｗ  TO 左手関節番号
      *                 WHEN 311
      *                     MOVE NC"○"      TO 右手根部
      *                     MOVE 部位番号Ｗ  TO 右手根番号
      *                 WHEN 312
      *                     MOVE NC"○"      TO 左手根部
      *                     MOVE 部位番号Ｗ  TO 左手根番号
      *                 WHEN 321
      *                     MOVE NC"○"      TO 右中手指関節
      *                     MOVE 部位番号Ｗ  TO 右中手指番号
      *                 WHEN 322
      *                     MOVE NC"○"      TO 左中手指関節
      *                     MOVE 部位番号Ｗ  TO 左中手指番号
      *                 WHEN 351
      *                     MOVE NC"○"      TO 右指関節
      *                     MOVE 部位番号Ｗ  TO 右指番号
      *                 WHEN 352
      *                     MOVE NC"○"      TO 左指関節
      *                     MOVE 部位番号Ｗ  TO 左指番号
      *                 WHEN 411
      *                     MOVE NC"○"      TO 右股関節
      *                     MOVE 部位番号Ｗ  TO 右股番号
      *                 WHEN 412
      *                     MOVE NC"○"      TO 左股関節
      *                     MOVE 部位番号Ｗ  TO 左股番号
      *                 WHEN 421
      *                     MOVE NC"○"      TO 右大腿部上
      *                     MOVE 部位番号Ｗ  TO 右大腿上番号
      *                 WHEN 422
      *                     MOVE NC"○"      TO 左大腿部上
      *                     MOVE 部位番号Ｗ  TO 左大腿上番号
      *                 WHEN 431
      *                     MOVE NC"○"      TO 右大腿部
      *                     MOVE 部位番号Ｗ  TO 右大腿番号
      *                 WHEN 432
      *                     MOVE NC"○"      TO 左大腿部
      *                     MOVE 部位番号Ｗ  TO 左大腿番号
      *                 WHEN 441
      *                     MOVE NC"○"      TO 右大腿部下
      *                     MOVE 部位番号Ｗ  TO 右大腿下番号
      *                 WHEN 442
      *                     MOVE NC"○"      TO 左大腿部下
      *                     MOVE 部位番号Ｗ  TO 左大腿下番号
      *                 WHEN 451
      *                     MOVE NC"○"      TO 右膝部
      *                     MOVE 部位番号Ｗ  TO 右膝番号
      *                 WHEN 452
      *                     MOVE NC"○"      TO 左膝部
      *                     MOVE 部位番号Ｗ  TO 左膝番号
      *                 WHEN 471
      *                     MOVE NC"○"      TO 右下腿部上
      *                     MOVE 部位番号Ｗ  TO 右下腿上番号
      *                 WHEN 472
      *                     MOVE NC"○"      TO 左下腿部上
      *                     MOVE 部位番号Ｗ  TO 左下腿上番号
      *                 WHEN 481
      *                     MOVE NC"○"      TO 右下腿部
      *                     MOVE 部位番号Ｗ  TO 右下腿番号
      *                 WHEN 482
      *                     MOVE NC"○"      TO 左下腿部
      *                     MOVE 部位番号Ｗ  TO 左下腿番号
      *                 WHEN 491
      *                     MOVE NC"○"      TO 右下腿部下
      *                     MOVE 部位番号Ｗ  TO 右下腿下番号
      *                 WHEN 492
      *                     MOVE NC"○"      TO 左下腿部下
      *                     MOVE 部位番号Ｗ  TO 左下腿下番号
      *                 WHEN 501
      *                     MOVE NC"○"      TO 右足関節
      *                     MOVE 部位番号Ｗ  TO 右足関節番号
      *                 WHEN 502
      *                     MOVE NC"○"      TO 左足関節
      *                     MOVE 部位番号Ｗ  TO 左足関節番号
      *                 WHEN 511
      *                     MOVE NC"○"      TO 右足根部
      *                     MOVE 部位番号Ｗ  TO 右足根番号
      *                 WHEN 512
      *                     MOVE NC"○"      TO 左足根部
      *                     MOVE 部位番号Ｗ  TO 左足根番号
      *                 WHEN 521
      *                     MOVE NC"○"      TO 右中足趾関節
      *                     MOVE 部位番号Ｗ  TO 右中足趾番号
      *                 WHEN 522
      *                     MOVE NC"○"      TO 左中足趾関節
      *                     MOVE 部位番号Ｗ  TO 左中足趾番号
      *                 WHEN 551
      *                     MOVE NC"○"      TO 右趾部
      *                     MOVE 部位番号Ｗ  TO 右趾番号
      *                 WHEN 552
      *                     MOVE NC"○"      TO 左趾部
      *                     MOVE 部位番号Ｗ  TO 左趾番号
      *                 END-EVALUATE
      **
      *             END-IF
      *         END-PERFORM
      *     END-IF.
010910**
013000**================================================================*
013020* 症状セット SECTION.
           MOVE 1                   TO カウンタ.
           IF 連印新－症状(1) NOT = SPACE
               STRING "①"               DELIMITED BY SIZE
                      連印新－症状(1)    DELIMITED BY SIZE
                   INTO 症状合成Ｗ(カウンタ)
               END-STRING
               COMPUTE カウンタ = カウンタ + 1
           END-IF.
           IF 連印新－症状(2) NOT = SPACE
               STRING "②"               DELIMITED BY SIZE
                      連印新－症状(2)    DELIMITED BY SIZE
                   INTO 症状合成Ｗ(カウンタ)
               END-STRING
               COMPUTE カウンタ = カウンタ + 1
           END-IF.
           IF 連印新－症状(3) NOT = SPACE
               STRING "③"               DELIMITED BY SIZE
                      連印新－症状(3)    DELIMITED BY SIZE
                   INTO 症状合成Ｗ(カウンタ)
               END-STRING
               COMPUTE カウンタ = カウンタ + 1
           END-IF.
           IF 連印新－症状(4) NOT = SPACE
               STRING "④"               DELIMITED BY SIZE
                      連印新－症状(4)    DELIMITED BY SIZE
                   INTO 症状合成Ｗ(カウンタ)
               END-STRING
               COMPUTE カウンタ = カウンタ + 1
           END-IF.
           IF 連印新－症状(5) NOT = SPACE
               STRING "⑤"               DELIMITED BY SIZE
                      連印新－症状(5)    DELIMITED BY SIZE
                   INTO 症状合成Ｗ(カウンタ)
               END-STRING
               COMPUTE カウンタ = カウンタ + 1
           END-IF.
      *
022940     MOVE  ZERO   TO  カウンタ カウンタ２.
022950     PERFORM VARYING カウンタ FROM 1 BY 1
022960             UNTIL ( カウンタ > 5 )  OR ( 症状合成Ｗ(カウンタ) = SPACE )
022970*
022980          INITIALIZE 症状分解Ｗ
022990          MOVE 症状合成Ｗ(カウンタ)   TO 症状分解Ｗ
023000          IF  症状内容１Ｗ  NOT = SPACE
023010              COMPUTE カウンタ２ = カウンタ２  +  1
023020              MOVE 症状内容１Ｗ  TO 症状Ｗ(カウンタ２)
023030          END-IF
023040          IF  症状内容２Ｗ  NOT = SPACE
023050              COMPUTE カウンタ２ = カウンタ２  +  1
023060              MOVE 症状内容２Ｗ  TO 症状Ｗ(カウンタ２)
023070          END-IF
023160*
023170     END-PERFORM.
013010*================================================================*
013020 負担率取得 SECTION.
013030*
015770     MOVE ZERO  TO 負担率Ｗ.
015790*
015800     MOVE SPACE TO 連率－負担率取得キー.
015810     INITIALIZE 連率－負担率取得キー.
015820     MOVE 受－施術和暦年月 TO 連率－施術和暦年月.
015830     MOVE 受－患者コード   TO 連率－患者コード.
015840*
015850     CALL   "HUTANRIT".
015860     CANCEL "HUTANRIT".
015870*
015880* / 実際 /
015890     MOVE 連率－実際負担率 TO 負担率Ｗ.
015700     COMPUTE 負担率Ｗ = 負担率Ｗ / 10.
           EVALUATE 負担率Ｗ
           WHEN 0
               MOVE NC"○"     TO ０割チェックＷ
           WHEN 1
               MOVE NC"○"     TO １割チェックＷ
           WHEN 2
               MOVE NC"○"     TO ２割チェックＷ
           WHEN 3
               MOVE NC"○"     TO ３割チェックＷ
           END-EVALUATE.
014010*
014210*================================================================*
014400*================================================================*
014410 受診者情報取得 SECTION.
014420*
014430**************************************************
014440* 連結データから受診者情報Ｆより以下の情報を取得 *
014450* ● 患者番号.... 患者番号Ｗ                     *
014460* ● 枝番........ 枝番Ｗ                         *
014470* ● 記号 ....... 記号Ｗに格納                   *
014480* ● 番号 ....... 番号Ｗに格納                   *
014490* ● 被保険者カナ.被保険者カナＷに格納           *
014500* ● 被保険者氏名.被保険者氏名Ｗに格納           *
014510* ● 郵便番号１ ..郵便番号１Ｗに格納             *
014520* ● 郵便番号２ ..郵便番号２Ｗに格納             *
014530* ● 住所１+住所２ ..被保険者住所Ｗに格納        *
014540* ● 電話番号.....電話番号Ｗに格納               *
014550* ● 患者カナ ....患者カナＷに格納               *
014560* ● 患者氏名 ....患者氏名Ｗに格納               *
014570* ● 続柄 ........名称マスタより続柄Ｗに取得     *
014580* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
014590* ● 患者年 ......患者年Ｗに格納                 *
014600* ● 患者月 ......患者月Ｗに格納                 *
014610* ● 患者日 ......患者日Ｗに格納                 *
014620**************************************************
014630     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
014640     MOVE 施術年ＷＲ         TO 受－施術年.
014650     MOVE 施術月ＷＲ         TO 受－施術月.
014660     MOVE 患者コードＷＲ     TO 受－患者コード.
014670     READ 受診者情報Ｆ
014680     INVALID KEY
014690         CONTINUE
014700*            /* ありえない */
014710     NOT INVALID KEY
014720         MOVE 受－患者番号     TO 患者番号Ｗ
014730         MOVE 受－枝番         TO 枝番Ｗ
014740*         MOVE 受－記号         TO 記号Ｗ
014750*         MOVE 受－番号         TO 番号Ｗ
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
               MOVE 受－公費種別     TO 公費種別Ｗ
               MOVE 受－助成種別     TO 助成種別Ｗ
014760         MOVE 受－被保険者カナ TO 被保険者カナＷ
014770         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
               IF 受－保険種別 = 70 OR 80 OR 85 OR 90
018370             MOVE 受－患者カナ       TO 被保険者カナＷ
018380             MOVE 受－患者氏名       TO 被保険者氏名Ｗ
               END-IF
015300*
015440         MOVE 受－患者カナ     TO 患者カナＷ
015450         MOVE 受－患者氏名     TO 患者氏名Ｗ
015030*
015150*/郵便番号 ゼロの時はスペース
015160         IF 受－患者郵便番号１ = "000"
015170            MOVE SPACE          TO 郵便番号１Ｗ
015180            MOVE SPACE          TO 郵便番号２Ｗ
015190         ELSE
015200            MOVE 受－患者郵便番号１ TO 郵便番号１Ｗ
015210            MOVE 受－患者郵便番号２ TO 郵便番号２Ｗ
015220         END-IF
015230         MOVE 受－患者住所１         TO 印刷住所１Ｗ
015240         MOVE 受－患者住所２         TO 印刷住所２Ｗ
015290         MOVE 受－患者電話番号       TO 電話番号Ｗ
015460*
               MOVE 受－保険種別     TO 事－保険種別
               MOVE 受－保険者番号   TO 事－保険者番号
               MOVE 記号Ｗ           TO 事－記号
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
               MOVE 事－事業所住所１ TO 事業所住所Ｗ
               MOVE 事－事業所住所２ TO 事業所住所２Ｗ
               MOVE 事－郵便番号１ TO 事業所郵便番号１Ｗ
               MOVE 事－郵便番号２ TO 事業所郵便番号２Ｗ
015520* 続柄
015560         MOVE 05             TO 名－区分コード
015570         MOVE 受－続柄       TO 名－名称コード
015580         READ 名称マスタ
015590         INVALID KEY
015600             MOVE SPACE      TO 続柄Ｗ
015610         NOT INVALID KEY
015620             MOVE 名－略称   TO 続柄Ｗ
015630         END-READ
015650* 患者和暦年月日
015660         EVALUATE 受－患者和暦
015670         WHEN 1
015680             MOVE NC"○"  TO 明治チェックＷ
015690         WHEN 2
015700             MOVE NC"○"  TO 大正チェックＷ
015710         WHEN 3
015720             MOVE NC"○"  TO 昭和チェックＷ
015730         WHEN 4
015740             MOVE NC"○"  TO 平成チェックＷ
015750         END-EVALUATE
015760         MOVE 受－患者年  TO 患者年Ｗ
015770         MOVE 受－患者月  TO 患者月Ｗ
015780         MOVE 受－患者日  TO 患者日Ｗ
015790*患者年齢
015800         MOVE 連印－施術和暦 TO 元－元号区分
015810         READ 元号マスタ
015820         NOT INVALID KEY
015830             COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
015840         END-READ
015850         IF ( 受－患者和暦 NOT = ZERO ) AND
015860            ( 受－患者年   NOT = ZERO ) AND
015870            ( 受－患者月   NOT = ZERO ) AND
015880            ( 受－患者日   NOT = ZERO )
015890*
015900             MOVE ZERO TO 患者年齢Ｗ
015910*
015920             MOVE 受－患者和暦 TO 元－元号区分
015930             READ 元号マスタ
015940             NOT INVALID KEY
015950                 COMPUTE 患者西暦年Ｗ = 元－開始西暦年 + ( 受－患者年 - 1 )
015960             END-READ
015970*
015980             COMPUTE 患者年齢Ｗ   = 施術西暦年Ｗ - 患者西暦年Ｗ
015990*
016000             IF 連印－施術月 < 受－患者月
016010                 COMPUTE 患者年齢Ｗ = 患者年齢Ｗ - 1
016020             END-IF
016030*
016040             MOVE 患者年齢Ｗ     TO 患者年齢
016050         END-IF
016060* 患者性別
016070         EVALUATE 受－患者性別
014980         WHEN 1
014990             MOVE NC"○"       TO 男チェックＷ
015000         WHEN 2
015010             MOVE NC"○"       TO 女チェックＷ
016120         END-EVALUATE
016130*/有効期限
016140         MOVE 受－有効和暦 TO 有効期限和暦Ｗ
016150         MOVE 受－有効年   TO 有効期限年Ｗ
016160         MOVE 受－有効月   TO 有効期限月Ｗ
016170         MOVE 受－有効日   TO 有効期限日Ｗ
016180         MOVE 有効期限和暦Ｗ TO 元－元号区分
016190         READ 元号マスタ
016200         INVALID KEY
016210             MOVE SPACE        TO 有効期限元号Ｗ
016220         NOT INVALID KEY
016230             MOVE 元－元号名称 TO 有効期限元号Ｗ
016240         END-READ
016250         MOVE 受－資格和暦 TO 資格取得和暦Ｗ
016260         MOVE 受－資格年   TO 資格取得年Ｗ
016270         MOVE 受－資格月   TO 資格取得月Ｗ
016280         MOVE 受－資格日   TO 資格取得日Ｗ
               IF 受－資格年 NOT = ZERO
016290             EVALUATE 資格取得和暦Ｗ
016300             WHEN 3
016310                 MOVE NC"○"  TO 資格昭和チェックＷ
016320             WHEN 4
016330                 MOVE NC"○"  TO 資格平成チェックＷ
016340             END-EVALUATE
               END-IF
016730* 市町村番号 受給者番号
016740* 老人 かつ 助成 の場合は老人の番号を印字する｡(今後どうなるかは解らない)
016750         IF 受－助成種別 NOT = ZERO
016760             MOVE 受－費用負担者番号助成 TO 負担者番号Ｗ
016770             MOVE 受－受益者番号助成     TO 受益者番号助成Ｗ
016780         END-IF
018290         IF ( 受－公費種別 = 05 ) AND ( 受－施術和暦年月 < 42004 )
016800             MOVE 受－費用負担者番号 TO 市町村番号Ｗ
016810             MOVE 受－受益者番号老人 TO 受益者番号Ｗ
016820         END-IF
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
                  MOVE 労災－労災事業所住所１     TO 事業所住所Ｗ
                  MOVE 労災－労災事業所住所２     TO 事業所住所２Ｗ
012130            MOVE 労災－労働保険番号         TO 番号Ｗ
               END-IF
      *
      *前期高齢
               IF 受－特別区分 = 1 OR 2 OR 3
016860             MOVE NC"○"  TO 前高チェックＷ
               END-IF
016830*保険種別
               IF 受－公費種別 = 5
016860             MOVE NC"○"  TO 後高チェックＷ
               END-IF
      *
016840         EVALUATE 受－保険種別
016850         WHEN 01
      */全国土木建設・全国左官タイル塗装業、全国板金業は組合
                   IF 受－保険者番号(1:6) = "133033" OR "133231" OR "133280"
008480                 MOVE NC"○"  TO 組合チェックＷ
                   ELSE
016860                 MOVE NC"○"  TO 国保チェックＷ
                   END-IF
016850         WHEN 02
016860             MOVE NC"○"  TO 社保チェックＷ
016850         WHEN 06
016860             MOVE NC"○"  TO 日雇チェックＷ
016870         WHEN 03
016880             MOVE NC"○"  TO 組合チェックＷ
016890         WHEN 04
016890         WHEN 09
016900             MOVE NC"○"  TO 共済チェックＷ
016870         WHEN 07
016880             MOVE NC"○"  TO 船員チェックＷ
016870         WHEN 08
016880             MOVE NC"○"  TO 退職チェックＷ
016870         WHEN 70
016880             MOVE NC"○"  TO 労災チェックＷ
016870         WHEN 80
016880             MOVE NC"○"  TO 自賠チェックＷ
016870         WHEN 85
016880             MOVE NC"○"  TO 生保チェックＷ
016870         WHEN 90
016880             MOVE NC"○"  TO 自費チェックＷ
016950         END-EVALUATE
      *
016840         EVALUATE 受－助成種別
016850         WHEN 50
016860             MOVE NC"○"  TO 生保チェックＷ
016850         WHEN 52
016860             MOVE NC"○"  TO 母子チェックＷ
016850         WHEN 53
016860             MOVE NC"○"  TO 障害チェックＷ
016850         WHEN 55
016860             MOVE NC"○"  TO 乳児チェックＷ
016850         WHEN 60
016860             MOVE NC"○"  TO 福祉チェックＷ
               END-EVALUATE
016960*/保険種別名称を欄外に印字する/050318
016970         MOVE SPACE        TO 保険種別名称Ｗ
016980         MOVE 02           TO 名－区分コード
016990         MOVE 受－助成種別 TO 名－名称コード
017000         READ 名称マスタ
017010         NOT INVALID KEY
                   IF 受－助成種別 = 60
                       MOVE 名－略称 TO その他Ｗ
                   END-IF
017030         END-READ
017040     END-READ.
017220*================================================================*
017230 請求先情報取得 SECTION.
017240*
017250****************************************************
017260* 連結データから保険者マスタより請求先を取得する。 *
017270* ※保－請求先情報区分=1の場合請求先マスタを使用   *
017280* ● 保険者番号...保険者番号Ｗに格納               *
017290* ● 名称........ 請求先名称Ｗに格納               *
017300* ● 郵便番号１.. 請求先郵便番号１Ｗに格納         *
017310* ● 郵便番号２.. 請求先郵便番号２Ｗに格納         *
017320* ● 住所１.......請求先住所１Ｗに格納             *
017330* ● 住所２.......請求先住所２Ｗに格納             *
017340****************************************************
017350     MOVE 保険種別ＷＲ   TO 保－保険種別.
017360     MOVE 保険者番号ＷＲ TO 保－保険者番号.
017370     READ 保険者マスタ
017380     INVALID KEY
               IF ( 受－施術和暦年月 >= 42004 ) AND ( 保険種別ＷＲ = 05 )
016990             MOVE 保険種別ＷＲ   TO 市－公費種別
017000             MOVE 保険者番号ＷＲ TO 市－市町村番号
017010             READ 市町村マスタ
017020             INVALID KEY
017030                 MOVE SPACE      TO 請求先名称Ｗ
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
               END-IF
017400     NOT INVALID KEY
017410         MOVE 保－保険者番号  TO 保険者番号Ｗ
017420         MOVE 保－保険者名称  TO 請求先名称Ｗ
017430         MOVE 保－支部部署名  TO 支部名Ｗ 
017440         MOVE 保－郵便番号１  TO 請求先郵便番号１Ｗ
017450         MOVE 保－郵便番号２  TO 請求先郵便番号２Ｗ
017460         MOVE 保－住所１      TO 請求先住所１Ｗ
017470         MOVE 保－住所２      TO 請求先住所２Ｗ
               MOVE 保－接尾語区分  TO 接尾語区分Ｗ
017480     END-READ.
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
           EVALUATE 保険種別ＷＲ
           WHEN 2
           WHEN 6
               IF 接尾語区分Ｗ = 1
                  MOVE SPACE              TO 印刷保険者呼名Ｗ
               ELSE
                  MOVE "社会保険事務所"   TO 印刷保険者呼名Ｗ
               END-IF
           WHEN 3
               MOVE "健康保険組合"        TO 印刷保険者呼名Ｗ
           WHEN OTHER
               MOVE SPACE TO 印刷保険者呼名Ｗ
           END-EVALUATE.
           IF 印刷保険者呼名Ｗ NOT = SPACE
               STRING 請求先名称Ｗ DELIMITED BY SPACE
                      保険者呼名Ｗ DELIMITED BY SPACE
                 INTO 請求先名称Ｗ
               END-STRING
           END-IF.
      *組合と共済は支部名も印字する
           EVALUATE 保険種別ＷＲ
           WHEN 03
           WHEN 04
               MOVE 支部名Ｗ TO 請求先名支部名Ｗ
           WHEN OTHER
               MOVE SPACE    TO 請求先名支部名Ｗ
           END-EVALUATE.
      *
017500*================================================================*
017510 負傷データ退避 SECTION.
017520*
017530*********************************************************************
017540* ① 負傷原因取得のため、負傷データＦから負傷原因Ｗに退避する。
017550* ② 詳細モード用  連印－負傷行印刷により、印刷位置を並び替えるため、
017560*                  負傷情報Ｗ  から負傷情報ＳＷに、
017570*                  負傷原因Ｗ  から負傷原因ＳＷに退避する。
017540* ③ 負傷位置丸付けのため、部位コードを待避する。
017580*********************************************************************
017590*
017600     MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷原因患者番号Ｗ(部位ＣＮＴ).
017610     MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷原因連番Ｗ(部位ＣＮＴ).
017620*--------------------------------------------------------------------*
017630*/負傷コード
017640     MOVE 会ＯＣＲコードＷ(部位ＣＮＴ) TO 会部位コードＳＷ(部位ＣＮＴ).
017650     MOVE 負傷名Ｗ        (部位ＣＮＴ) TO 負傷名ＳＷ(部位ＣＮＴ).
017660*
017670     MOVE 負傷年Ｗ        (部位ＣＮＴ) TO 負傷年ＳＷ(部位ＣＮＴ).
017680     MOVE 負傷月Ｗ        (部位ＣＮＴ) TO 負傷月ＳＷ(部位ＣＮＴ).
017690     MOVE 負傷日Ｗ        (部位ＣＮＴ) TO 負傷日ＳＷ(部位ＣＮＴ).
017700*
017710     MOVE 施術開始年Ｗ(部位ＣＮＴ) TO 施術開始年ＳＷ(部位ＣＮＴ).
017720     MOVE 施術開始月Ｗ(部位ＣＮＴ) TO 施術開始月ＳＷ(部位ＣＮＴ).
017730     MOVE 施術開始日Ｗ(部位ＣＮＴ) TO 施術開始日ＳＷ(部位ＣＮＴ).
017740*
017750     MOVE 施術終了年Ｗ(部位ＣＮＴ) TO 施術終了年ＳＷ(部位ＣＮＴ).
017760     MOVE 施術終了月Ｗ(部位ＣＮＴ) TO 施術終了月ＳＷ(部位ＣＮＴ).
017770     MOVE 施術終了日Ｗ(部位ＣＮＴ) TO 施術終了日ＳＷ(部位ＣＮＴ).
017780*
017790     MOVE 治癒チェックＷ(部位ＣＮＴ) TO 治癒チェックＳＷ(部位ＣＮＴ).
017800     MOVE 中止チェックＷ(部位ＣＮＴ) TO 中止チェックＳＷ(部位ＣＮＴ).
017810     MOVE 転医チェックＷ(部位ＣＮＴ) TO 転医チェックＳＷ(部位ＣＮＴ).
017820     MOVE 施術累計回数Ｗ(部位ＣＮＴ) TO 施術累計回数ＳＷ(部位ＣＮＴ).
017830*
017840     MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号ＳＷ(部位ＣＮＴ).
017850     MOVE 負傷原因連番Ｗ(部位ＣＮＴ)     TO 負傷連番ＳＷ(部位ＣＮＴ).
017860*
017620*--------------------------------------------------------------------*
017630*/部位コード
           MOVE 部位コードＷＰ(部位ＣＮＴ) TO 部位コードＳＷ(部位ＣＮＴ).
017860*
017870*================================================================*
017880 負傷データ取得 SECTION.
017890*
017900**************************************************
017910* 連結データから負傷データＦより以下の情報を取得 *
017920* ● 負傷名...部位＋負傷種別にて加工して格納     *
017930* ● 負傷年.......負傷年Ｗ                       *
017940* ● 負傷月.......負傷月Ｗ                       *
017950* ● 負傷日.......負傷日Ｗ                       *
017960* ● 施術終了年...終了年Ｗ                       *
017970* ● 施術終了月...終了月Ｗ                       *
017980* ● 施術終了日...終了日Ｗ                       *
017990**************************************************
018000     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
018010     MOVE 施術年ＷＲ         TO 負－施術年.
018020     MOVE 施術月ＷＲ         TO 負－施術月.
018030     MOVE 患者コードＷＲ     TO 負－患者コード.
018040     READ 負傷データＦ
018050     INVALID KEY
018060         CONTINUE
018070*            /* ありえない */
018080     NOT INVALID KEY
018090         MOVE ZERO                         TO 部位数Ｗ
018100         MOVE 負－部位数                   TO 部位数Ｗ
018110         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018120                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR
                             ( 部位ＣＮＴ > 5 ) 
018130*********************************************
018140* 注）全柔...負傷種別＋部位にて加工して格納 *
018150*********************************************
018160* 負傷種別
018170             MOVE SPACE                     TO 負傷名称Ｗ
018180             MOVE 03                        TO 名－区分コード
018190             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
018200             READ 名称マスタ
018210             INVALID KEY
018220                 MOVE SPACE    TO 負傷名称Ｗ
018230             NOT INVALID KEY
018240*                 MOVE 名－略称 TO 負傷名称Ｗ
018250                 MOVE 名－正式名称 TO 負傷名称Ｗ
018260             END-READ
018270* 部位
018280             MOVE 負－部位コード(部位ＣＮＴ)  TO 部位番号ＷＰ(部位ＣＮＴ) 部位コードＷＰ(部位ＣＮＴ)
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
018290             PERFORM 名称埋込処理
018350             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
018360             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
018370             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
018380*
018390             MOVE 負－開始年(部位ＣＮＴ)   TO 施術開始年Ｗ(部位ＣＮＴ)
018400             MOVE 負－開始月(部位ＣＮＴ)   TO 施術開始月Ｗ(部位ＣＮＴ)
018410             MOVE 負－開始日(部位ＣＮＴ)   TO 施術開始日Ｗ(部位ＣＮＴ)
018420*
018430             MOVE 負－終了年(部位ＣＮＴ)   TO 施術終了年Ｗ(部位ＣＮＴ)
018440             MOVE 負－終了月(部位ＣＮＴ)   TO 施術終了月Ｗ(部位ＣＮＴ)
018450             MOVE 負－終了日(部位ＣＮＴ)   TO 施術終了日Ｗ(部位ＣＮＴ)
018460*転帰
018470             EVALUATE 負－転帰区分(部位ＣＮＴ)
018480             WHEN 1
018490             WHEN 2
018500                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
018510             WHEN 3
018520                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
018530             WHEN 4
018540                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
018550             END-EVALUATE
018610               PERFORM 負傷データ退避
018620         END-PERFORM
018630     END-READ.
018640*================================================================*
018650 施術記録取得 SECTION.
018660*
018670************************************************************
018680* 連結データから負傷データＦより以下の情報を取得           *
018690* ● 施術開始年月日...該当する部位に対して当月最初の施術日 *
018700************************************************************
018710     MOVE 施術和暦ＷＲ  TO 施記－施術和暦.
018720     MOVE 施術年ＷＲ    TO 施記－施術年.
018730     MOVE 施術月ＷＲ    TO 施記－施術月.
018740     MOVE ZERO            TO 施記－施術日.
018750     MOVE ZERO            TO 施記－患者番号.
018760     MOVE SPACE           TO 施記－枝番.
018770     START 施術記録Ｆ   KEY IS >= 施記－施術和暦年月日
018780                                  施記－患者コード.
018790     IF 状態キー = "00"
018800         MOVE SPACE  TO 終了フラグ２
018810         PERFORM 施術記録Ｆ読込
018820         PERFORM UNTIL ( 終了フラグ２       = "YES" ) OR
018830                       ( 施記－施術和暦 NOT = 施術和暦ＷＲ ) OR
018840                       ( 施記－施術年   NOT = 施術年ＷＲ   ) OR
018850                       ( 施記－施術月   NOT = 施術月ＷＲ   )
018860*            **************
018870*            * 開始年月日 *
018880*            **************
018890             PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018900                     UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
018910            EVALUATE TRUE ALSO TRUE ALSO TRUE ALSO TRUE ALSO TRUE
018920            WHEN 施記－負傷種別(部位ＣＮＴ) = 負傷種別Ｗ(部位ＣＮＴ) ALSO
018930                 施記－部位(部位ＣＮＴ)     = 部位Ｗ(部位ＣＮＴ)     ALSO
018940                 施記－左右区分(部位ＣＮＴ) = 左右区分Ｗ(部位ＣＮＴ) ALSO
018950                 施記－負傷位置番号(部位ＣＮＴ)
018960                                       = 負傷位置番号Ｗ(部位ＣＮＴ)  ALSO
018970                 開始年月日取得フラグ(部位ＣＮＴ) = SPACE
018980                   MOVE 施記－施術年     TO 施術開始年Ｗ(部位ＣＮＴ)
018990                   MOVE 施記－施術月     TO 施術開始月Ｗ(部位ＣＮＴ)
019000                   MOVE 施記－施術日     TO 施術開始日Ｗ(部位ＣＮＴ)
019010                   MOVE "YES"       TO 開始年月日取得フラグ(部位ＣＮＴ)
019020*
019030            WHEN OTHER
019040                CONTINUE
019050            END-EVALUATE
019060             END-PERFORM
019070             PERFORM 施術記録Ｆ読込
019080         END-PERFORM
019090     END-IF.
019100*================================================================*
019110 施術記録Ｆ読込 SECTION.
019120*
019130     READ 施術記録Ｆ NEXT
019140     AT END
019150         MOVE "YES" TO 終了フラグ２
019160     END-READ.
019170*================================================================*
019180 詳細モード負傷原因処理 SECTION.
019190*
019200*******************************************************************
019210* 連印－負傷印刷行により、負傷原因データの並び替えを行う。
019220* ( 負傷原因情報Ｗを初期化し、退避済みの負傷原因情報ＳＷから
019230*         並び替えを行い、負傷原因情報Ｗを再設定する。)
019240*******************************************************************
019250*
019260     INITIALIZE 負傷原因情報Ｗ.
019270*
019280     PERFORM VARYING 詳細ＣＮＴ FROM 1 BY 1
019290                                UNTIL ( 詳細ＣＮＴ > 5)
019300*
019310        IF ( 連印－負傷印刷行(詳細ＣＮＴ) NOT = ZERO) AND
019320           ( 連印－原因印刷区分(詳細ＣＮＴ)   = 1)
019330*
019340           MOVE 連印－負傷印刷行(詳細ＣＮＴ) TO 印刷行ＳＷ
019350*
019360           MOVE 負傷患者番号ＳＷ(詳細ＣＮＴ) TO 負傷原因患者番号Ｗ(印刷行ＳＷ)
019370           MOVE 負傷連番ＳＷ(詳細ＣＮＴ) TO 負傷原因連番Ｗ(印刷行ＳＷ)
019380        END-IF
019390*
019400     END-PERFORM.
019410*
019420*================================================================*
019430 詳細モード負傷処理 SECTION.
019440*
019450*******************************************************************
019460* 連印－負傷印刷行により、負傷データの並び替えを行う。
019470* ( 負傷情報Ｗを初期化し、退避済みの負傷情報ＳＷから
019480*         並び替えを行い、負傷情報Ｗを再設定する。)
019490*******************************************************************
019500*
019510     INITIALIZE 負傷情報Ｗ.
019520*
019530     PERFORM VARYING 詳細ＣＮＴ FROM 1 BY 1
019540                                UNTIL ( 詳細ＣＮＴ > 5)
019550*
019560        IF 連印－負傷印刷行(詳細ＣＮＴ) NOT = ZERO
019570*/転帰のみ印字が出来ない不具合の修正。
019580           MOVE 連印－負傷印刷行(詳細ＣＮＴ) TO 印刷行ＳＷ
019590           IF 連印－部位印刷区分(詳細ＣＮＴ) = 1
019600*
019610*              MOVE 連印－負傷印刷行(詳細ＣＮＴ) TO 印刷行ＳＷ
019620*
019630*/負傷コード
019640              MOVE 会部位コードＳＷ(詳細ＣＮＴ) TO 会ＯＣＲコードＷ(印刷行ＳＷ)
019650              MOVE 負傷名ＳＷ      (詳細ＣＮＴ) TO 負傷名Ｗ        (印刷行ＳＷ)
019660              MOVE 負傷年ＳＷ      (詳細ＣＮＴ) TO 負傷年Ｗ        (印刷行ＳＷ)
019670              MOVE 負傷月ＳＷ      (詳細ＣＮＴ) TO 負傷月Ｗ        (印刷行ＳＷ)
019680              MOVE 負傷日ＳＷ      (詳細ＣＮＴ) TO 負傷日Ｗ        (印刷行ＳＷ)
019690              MOVE 施術開始年ＳＷ  (詳細ＣＮＴ) TO 施術開始年Ｗ    (印刷行ＳＷ)
019700              MOVE 施術開始月ＳＷ  (詳細ＣＮＴ) TO 施術開始月Ｗ    (印刷行ＳＷ)
019710              MOVE 施術開始日ＳＷ  (詳細ＣＮＴ) TO 施術開始日Ｗ    (印刷行ＳＷ)
019720           END-IF
019730*
019740           IF 連印－転帰印刷区分(詳細ＣＮＴ) = 1
019750              MOVE 施術終了年ＳＷ(詳細ＣＮＴ) TO 施術終了年Ｗ(印刷行ＳＷ)
019760              MOVE 施術終了月ＳＷ(詳細ＣＮＴ) TO 施術終了月Ｗ(印刷行ＳＷ)
019770              MOVE 施術終了日ＳＷ(詳細ＣＮＴ) TO 施術終了日Ｗ(印刷行ＳＷ)
019780              MOVE 治癒チェックＳＷ(詳細ＣＮＴ) TO 治癒チェックＷ(印刷行ＳＷ)
019790              MOVE 中止チェックＳＷ(詳細ＣＮＴ) TO 中止チェックＷ(印刷行ＳＷ)
019800              MOVE 転医チェックＳＷ(詳細ＣＮＴ) TO 転医チェックＷ(印刷行ＳＷ)
019810              MOVE 施術累計回数ＳＷ(詳細ＣＮＴ) TO 施術累計回数Ｗ(印刷行ＳＷ)
019820           END-IF
      *
                 MOVE 部位コードＳＷ(詳細ＣＮＴ)    TO 部位コードＷＰ(印刷行ＳＷ)
019830        END-IF
019840*
019850     END-PERFORM.
019860*
019870*================================================================*
019880 印刷処理 SECTION.
019890*
019900     EVALUATE 印刷モードＦＷＲ
019910     WHEN 0
019920         PERFORM 印刷処理２
019930         PERFORM 印刷処理３
019940     WHEN 1
019950         PERFORM 印刷処理２
019960     WHEN 2
019970         PERFORM 印刷処理３
020000     WHEN 4
020010         IF 連印－保険証印刷区分 = 1
020020             PERFORM 印刷処理２
020030         END-IF
020040         PERFORM 印刷処理３
020050*         PERFORM 印刷処理４
020060     END-EVALUATE.
020160*================================================================*
020170 印刷処理２ SECTION.
020180*
020190     MOVE "YAI662P"  TO  定義体名Ｐ.
020200     MOVE "GRP001"   TO  項目群名Ｐ.
020210     WRITE YAI662P.
020220*     WRITE 印刷レコード.
020230     PERFORM エラー処理Ｐ.
020240*================================================================*
020250 印刷処理３ SECTION.
020260*
020270     MOVE "YAI662P"  TO  定義体名Ｐ.
020280     MOVE "GRP002"   TO  項目群名Ｐ.
020290     WRITE YAI662P.
020300*     WRITE 印刷レコード.
020310     PERFORM エラー処理Ｐ.
020390*================================================================*
020400 エラー処理Ｐ SECTION.
020410*
020420     IF 通知情報Ｐ NOT = "00"
020430         DISPLAY NC"帳票エラー"              UPON CONS
020440         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
020450         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
020460         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
020470         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
020480                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
020490         ACCEPT  キー入力 FROM CONS
020500         PERFORM ファイル閉鎖
020510         EXIT PROGRAM
020520     END-IF.
020530*================================================================*
020540 名称埋込処理 SECTION.
020550*
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
020670     END-READ.
020680*
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
      */詳細印刷の対応。部位数を超える印字位置もありえるため９部位分判定。(厳密には５部位分で良い)/060112
022140*             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
022140             UNTIL ( 部位ＣＮＴ >= 9 )
022150*
022160        IF ( 負傷原因連番Ｗ(部位ＣＮＴ)      NOT = ZERO )
022170*
022180           IF カウンタ = ZERO
022190               MOVE 1   TO  カウンタ カウンタ２
022200               MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
022210               MOVE 負傷原因連番Ｗ(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
022220               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
022230           ELSE
022240              IF ( 負傷原因患者番号Ｗ(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
022250                 ( 負傷原因連番Ｗ(部位ＣＮＴ)      = 負傷連番ＣＷ     )
022260                 COMPUTE カウンタ２ = カウンタ２  +  1
022270                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
022280              ELSE
022290                 COMPUTE カウンタ = カウンタ  +  1
022300                 MOVE 1   TO  カウンタ２
022310                 MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
022320                 MOVE 負傷原因連番Ｗ(部位ＣＮＴ)   TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
022330                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
022340              END-IF
022350           END-IF
022360        END-IF
022370     END-PERFORM.
022380**************************************************************************
022390*  負傷原因マスタより文章取得
022400**************************************************************************
022410     MOVE  ZERO   TO  カウンタ カウンタ２.
022420     PERFORM VARYING カウンタ FROM 1 BY 1
022430             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
022440*** 健保は 区分 01
022450*         MOVE 01                        TO 負原－区分コード
      */健保：０１、労災：０２、自賠責：０３
               EVALUATE 受－保険種別
               WHEN 70
                   MOVE 2 TO 負原－区分コード
               WHEN 80
                   MOVE 3 TO 負原－区分コード
               WHEN OTHER
                   MOVE 1 TO 負原－区分コード
               END-EVALUATE
022460         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
022470         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
022480         READ 負傷原因Ｆ
022490         NOT INVALID KEY
022500             INITIALIZE 負傷原因ＷＴ
022510             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
022520             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
022530             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
022540             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
022550             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
022560             PERFORM VARYING カウンタ２ FROM 1 BY 1
022570                     UNTIL ( カウンタ２ > 9 )  OR 
022580                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
022590                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
022600                WHEN 1
022610                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
022620                WHEN 2
022630                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
022640                WHEN 3
022650                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
022660                WHEN 4
022670                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
022680                WHEN 5
022690                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
022700                WHEN OTHER
022710                   CONTINUE
022720                END-EVALUATE
022730             END-PERFORM
022740*
022822             IF 負原－負傷原因入力区分 = 1
022823                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
022824                        負傷原因１ＷＴ  DELIMITED BY SIZE
022825                        負傷原因２ＷＴ  DELIMITED BY SIZE
022826                        負傷原因３ＷＴ  DELIMITED BY SIZE
022827                        負傷原因４ＷＴ  DELIMITED BY SIZE
022828                        負傷原因５ＷＴ  DELIMITED BY SIZE
022829                        INTO 負傷原因内容合成Ｗ(カウンタ)
022830                 END-STRING
022831             ELSE
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
022840             END-IF
022841*
022842         END-READ
022843     END-PERFORM.
022850*
022860     PERFORM 負傷原因セット.
022870*
022880*================================================================*
022890 負傷原因セット SECTION.
022900*
022910**************************************************************************
022920*  文章が1行を超える時は、複数行に分解する。
022930**************************************************************************
022940     MOVE  ZERO   TO  カウンタ カウンタ２.
022950     PERFORM VARYING カウンタ FROM 1 BY 1
022960             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
022970*
022980          INITIALIZE 負傷原因内容分解ＸＷ
022990          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
023000          IF  負傷原因内容１ＸＷ  NOT = SPACE
023010              COMPUTE カウンタ２ = カウンタ２  +  1
023020              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
023030          END-IF
023040          IF  負傷原因内容２ＸＷ  NOT = SPACE
023050              COMPUTE カウンタ２ = カウンタ２  +  1
023060              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
023070          END-IF
023080          IF  負傷原因内容３ＸＷ  NOT = SPACE
023090              COMPUTE カウンタ２ = カウンタ２  +  1
023100              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
023110          END-IF
023120          IF  負傷原因内容４ＸＷ  NOT = SPACE
023130              COMPUTE カウンタ２ = カウンタ２  +  1
023140              MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
023150          END-IF
020780          IF  負傷原因内容５ＸＷ  NOT = SPACE
020790              COMPUTE カウンタ２ = カウンタ２  +  1
020800              MOVE 負傷原因内容５ＸＷ  TO 負傷原因Ｗ(カウンタ２)
020810          END-IF
023160*
023170     END-PERFORM.
024670*================================================================*
024680 遅延処理 SECTION.
024690*
024700     PERFORM VARYING 遅延カウンタ FROM 1 BY 1
024710             UNTIL 遅延カウンタ > 遅延回数Ｗ
024720         MOVE "YES" TO 遅延フラグ
024730     END-PERFORM.
024740*
024750*================================================================*
       原因行セット SECTION.
      *
      * 指定された開始行から印刷するようにセットし直す。
      * ８行.
      *
           MOVE SPACE                  TO 負傷原因Ｗテーブル２.
           MOVE ZERO                   TO カウンタ.
           IF 連印－原因開始行 = ZERO
               MOVE 1                  TO カウンタ２
           ELSE
               MOVE 連印－原因開始行 TO カウンタ２
           END-IF.
           PERFORM VARYING カウンタ FROM 1 BY 1
                   UNTIL ( カウンタ > 8 ) OR (カウンタ２ > 8)
               MOVE 負傷原因Ｗ(カウンタ) TO 負傷原因Ｗ２(カウンタ２)
               COMPUTE カウンタ２ = カウンタ２ + 1
           END-PERFORM.
      *
           PERFORM VARYING カウンタ FROM 1 BY 1
                     UNTIL カウンタ > 8
               MOVE 負傷原因Ｗ２(カウンタ) TO 負傷原因Ｗ(カウンタ)
           END-PERFORM.
024750*================================================================*
       症状詳細セット SECTION.
      *
      * 詳細で指定した順序に入れ替える。
      *
           MOVE SPACE TO 症状ＷＲテーブル.
      */↑だけでは半角空白が入り空白と判定されないため↓を追加
           INITIALIZE    症状ＷＲテーブル.
           PERFORM VARYING カウンタ FROM 1 BY 1
                     UNTIL カウンタ > 5
               IF (連印－負傷印刷行    (カウンタ) NOT = ZERO) AND
                  (連印新－症状印刷区分(カウンタ)     = 1   )
                   MOVE 連印－負傷印刷行(カウンタ) TO カウンタ２
                   MOVE 連印新－症状    (カウンタ) TO 症状ＷＲ(カウンタ２)
               END-IF
           END-PERFORM.
      *
           PERFORM VARYING カウンタ FROM 1 BY 1
                     UNTIL カウンタ > 5
               MOVE 症状ＷＲ(カウンタ) TO 連印新－症状(カウンタ)
           END-PERFORM.
024750*================================================================*
       テスト印字 SECTION.
      *
           MOVE NC"○"  TO 
             国保チェック 社保チェック 組合チェック 共済チェック 船員チェック
             労災チェック 自賠チェック 退職チェック 後高チェック 前高チェック
             日雇チェック 生保チェック 自費チェック 福祉チェック 
             乳児チェック 母子チェック 障害チェック 資格昭和チェック 
             資格平成チェック 明治チェック 大正チェック 昭和チェック 平成チェック 
             男チェック 女チェック ０割チェック １割チェック ２割チェック ３割チェック
             明治チェック 大正チェック 昭和チェック 平成チェック.
017190     MOVE NC"他"  TO その他.
           MOVE NC"○" TO 
             治癒チェック１ 中止チェック１ 転医チェック１
             治癒チェック２ 中止チェック２ 転医チェック２
             治癒チェック３ 中止チェック３ 転医チェック３
             治癒チェック４ 中止チェック４ 転医チェック４
             治癒チェック５ 中止チェック５ 転医チェック５.
           MOVE ALL "静"  TO 
             住所１ 住所２ 患者氏名 被保険者氏名 記号 事業所住所 事業所名称
             請求先住所１ 請求先住所２ 請求先名称１ 請求先名称２ 負傷原因１ 負傷原因２
             負傷原因３ 負傷原因４ 負傷原因５ 負傷原因６ 負傷原因７ 負傷原因８.
           MOVE ALL "9" TO
             番号 労働保険番号 負担者番号 受給者番号 保険者番号 患者年 患者月 患者日
             電話番号 資格取得年 資格取得月 資格取得日 有効年 有効月 有効日 同意年 同意月 同意日.
           MOVE ALL NC"静" TO
             負傷名１ 負傷名２ 負傷名３ 負傷名４ 負傷名５ 続柄 医師名.
           MOVE ALL "9"  TO
             負傷年１ 負傷月１ 負傷日１ 負傷年２ 負傷月２ 負傷日２ 負傷年３ 負傷月３
             負傷日３ 負傷年４ 負傷月４ 負傷日４ 負傷年５ 負傷月５ 負傷日５
             施術開始年１ 施術開始月１ 施術開始日１ 施術開始年２ 施術開始月２ 施術開始日２
             施術開始年３ 施術開始月３ 施術開始日３ 施術開始年４ 施術開始月４ 施術開始日４
             施術開始年５ 施術開始月５ 施術開始日５ 施術終了年１ 施術終了月１ 施術終了日１
             施術終了年２ 施術終了月２ 施術終了日２ 施術終了年３ 施術終了月３ 施術終了日３
             施術終了年４ 施術終了月４ 施術終了日４ 施術終了年５ 施術終了月５ 施術終了日５.
            MOVE NC"○"   TO 治癒チェック１ 中止チェック１ 転医チェック１
                             治癒チェック２ 中止チェック２ 転医チェック２
                             治癒チェック３ 中止チェック３ 転医チェック３
                             治癒チェック４ 中止チェック４ 転医チェック４
                             治癒チェック５ 中止チェック５ 転医チェック５.

           MOVE NC"○"  TO 頭部 顔面部 右顎関節 左顎関節 頚部 胸部 右胸部 左胸部
                           右肋骨 左肋骨 背部上 背部 背部下 腰部 臀部 右臀部
                           左臀部 右肩部 左肩部 右上腕部上 左上腕部上 右上腕部
                           左上腕部 右上腕部下 左上腕部下 右肘部 左肘部 右前腕部上
                           左前腕部上 右前腕部 左前腕部 右前腕部下 左前腕部下
                           右手関節 左手関節 右手根部 左手根部 右中手指関節
                           左中手指関節 右指関節 左指関節 右股関節 左股関節
                           右大腿部上 左大腿部上 右大腿部 左大腿部 右大腿部下
                           左大腿部下 右膝部 左膝部 右下腿部上 左下腿部上
                           右下腿部 左下腿部 右下腿部下 左下腿部下 右足関節
                           左足関節 右足根部 左足根部 右中足趾関節 左中足趾関節
                           右趾部 左趾部.
024750*================================================================*
024760******************************************************************
024770 END PROGRAM YAI662.
024780******************************************************************
