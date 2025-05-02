000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCH6622.
000060 AUTHOR.                 井上　真由美
000070*
000080*----------------------------------------------------------------*
000090*         中央用
000100*　　　　　カルテ労災印刷（柔ｳｨﾝﾄﾞｳｽﾞ95版）再生紙
000110*         MED = YAW6622G YCH6622P
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2000-03-21
000140 DATE-COMPILED.          2000-03-21
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
001720*                           ［ＲＬ＝  １２８］
001730 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
001740     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
001780*                           ［ＲＬ＝  ３２０］
001790 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
001800     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
001810*                           ［ＲＬ＝  ２５６］
001820 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
001830     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
001840*                           ［ＲＬ＝  １２８］
001850 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
001860     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
001900*                           ［ＲＬ＝  １２８］
001910 FD  負傷原因Ｆ          BLOCK   CONTAINS   1   RECORDS.
001920     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
002140*
       FD  事業所マスタ        BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY JIGYOS          OF  XFDLIB  JOINING   事   AS  PREFIX.
001700*
000280 FD  労災情報Ｆ          BLOCK   CONTAINS   1   RECORDS.
000281     COPY ROUSAIJ         OF  XFDLIB  JOINING   労災   AS  PREFIX.
001930*
001940 FD  印刷ファイル.
001950     COPY YCH6622P        OF  XMDLIB.
001960*----------------------------------------------------------------*
001970******************************************************************
001980*                WORKING-STORAGE SECTION                         *
001990******************************************************************
002000 WORKING-STORAGE         SECTION.
002010 01 キー入力                           PIC X     VALUE SPACE.
002020 01 状態キー                           PIC X(2)  VALUE SPACE.
002030 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002040 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002050 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002060 01 文字ＣＮＴ                         PIC 9(2)  VALUE ZERO.
002070 01 初検フラグ                         PIC X(3)  VALUE SPACE.
002080 01 ファイル名                         PIC N(2)  VALUE SPACE.
002090 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
002100 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002110 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002120 01 部位名称Ｗ                         PIC N(10) VALUE SPACE.
002130 01 負傷番号Ｗ                         PIC 9     VALUE ZERO.
002140 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
002150    03 負傷番号Ｗ１                    PIC X.
002160*
002170 01 全角負傷番号Ｗ                     PIC N     VALUE SPACE.
002180 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
002190    03 全角負傷番号Ｗ１                PIC X(2).
002200*
002210 01 負担率Ｗ                           PIC 9(3) VALUE ZERO.
002220 01 甲乙区分Ｗ                         PIC 9    VALUE ZERO.
002230 01 本人負担率Ｗ                       PIC 9(3) VALUE ZERO.
002240 01 家族負担率Ｗ                       PIC 9(3) VALUE ZERO.
002250*
002260 01 施術和暦年月日ＣＷ.
002270   03 施術和暦年月ＣＷ.
002280     05 施術和暦ＣＷ                   PIC 9    VALUE ZERO.
002290     05 施術年月ＣＷ.
002300        07 施術年ＣＷ                  PIC 9(2) VALUE ZERO.
002310        07 施術月ＣＷ                  PIC 9(2) VALUE ZERO.
002320   03 施術日ＣＷ                       PIC 9(2) VALUE ZERO.
002330 01 施術西暦年Ｗ                       PIC 9(4) VALUE ZERO.
002340 01 患者西暦年Ｗ                       PIC 9(4) VALUE ZERO.
002350 01 被保険者西暦年Ｗ                   PIC 9(4) VALUE ZERO.
002360*
002370** 負傷原因用
002380*
002390 01 カウンタ                           PIC 9(2)  VALUE ZERO.
002400 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
002410*
002420 01 負傷原因ＷＴ.
002430    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
002440    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
002450    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
002460    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
002470    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
002480    03 負傷原因ナンバーＷＴ.
002490       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
002500    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
002510 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
002520 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
002530 01 負傷原因ＴＢＬ.
002540    03 負傷原因コードＴＢＬ            OCCURS 9.
002550       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
002560       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
002570       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
002580 01 負傷原因内容Ｗ.
002590    03 負傷原因内容合成Ｗ              PIC X(316) OCCURS 9 VALUE SPACE.
002600    03 負傷原因内容分解ＸＷ.
002610       05 負傷原因内容１ＸＷ           PIC X(210)  VALUE SPACE.
002620       05 負傷原因内容２ＸＷ           PIC X(106)   VALUE SPACE.
002660*
002670** 負傷原因印刷区分用
002680 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
002690*
002700** バーコード区分用
002710 01 バーコード使用区分Ｗ               PIC 9 VALUE ZERO.
002720*
002730****************
002740* 連結項目待避 *
002750****************
002760*    ************
002770*    * 印刷キー *
002780*    ************
002790 01 対象データＷＲ.
002800    03 施術和暦年月ＷＲ.
002810       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
002820       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
002830       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
002840    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
002850    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
002860    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
002870    03 被保険者カナＷＲ                 PIC X(20) VALUE SPACE.
002880    03 患者コードＷＲ.
002890       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
002900       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
002910    03 印刷モードＦＷＲ                 PIC 9(1)  VALUE ZERO.
002920    03 提出年月日ＷＲ.
002930       05 提出年ＷＲ                    PIC 9(2)  VALUE ZERO.
002940       05 提出月ＷＲ                    PIC 9(2)  VALUE ZERO.
002950       05 提出日ＷＲ                    PIC 9(2)  VALUE ZERO.
002960    03 段数ＷＲ                         PIC 9(1)  VALUE ZERO.
002970************
002980* 料金情報 *
002990************
003000*
003010 01 印刷負担率Ｗ                        PIC 9(1)  VALUE ZERO.
003020**************
003030* 受診者情報 *
003040**************
003050 01 受診者情報Ｗ.
003060    03 患者コードＷ.
003070       05 患者番号Ｗ                   PIC 9(6)  VALUE ZERO.
003080       05 枝番Ｗ                       PIC X(1)  VALUE SPACE.
003090    03 市町村番号Ｗ.
003100       05 印刷市町村番号Ｗ             PIC X(8)  VALUE SPACE.
003110       05 FILLER                       PIC X(2).
003120    03 受益者番号Ｗ.
003130       05 印刷受益者番号Ｗ             PIC X(7)  VALUE SPACE.
003140       05 FILLER                       PIC X(13).
003150    03 費用負担者番号Ｗ.
003160       05 印刷費用負担者番号Ｗ         PIC X(8)  VALUE SPACE.
003170       05 FILLER                       PIC X(2).
003180    03 受益者番号助成Ｗ.
003190       05 印刷受益者番号助成Ｗ         PIC X(7)  VALUE SPACE.
003200       05 FILLER                       PIC X(13).
003210    03 記号Ｗ                          PIC X(24)  VALUE SPACE.
003220    03 番号Ｗ.
003230       05 印刷番号Ｗ                   PIC X(30)  VALUE SPACE.
003240*       05 FILLER                       PIC X(18).
003250    03 被保険者情報Ｗ.
003260       05 被保険者カナＷ               PIC X(50)  VALUE SPACE.
003270       05 被保険者氏名Ｗ               PIC X(50)  VALUE SPACE.
003280*
003290*       05 被保険者和暦チェックＷ.
003300*          07 被保険者明治Ｗ            PIC N(1)  VALUE SPACE.
003310*          07 被保険者大正Ｗ            PIC N(1)  VALUE SPACE.
003320*          07 被保険者昭和Ｗ            PIC N(1)  VALUE SPACE.
003330       05 被保険者性別チェックＷ.
003340          07 被保険者男チェックＷ      PIC N(1)  VALUE SPACE.
003350          07 被保険者女チェックＷ      PIC N(1)  VALUE SPACE.
003360       05 被保険者性別Ｗ               PIC 9     VALUE ZERO.
003370       05 被保険者和暦チェックＷ.
003380          07 被保険者明治チェックＷ    PIC N(1)  VALUE SPACE.
003390          07 被保険者大正チェックＷ    PIC N(1)  VALUE SPACE.
003400          07 被保険者昭和チェックＷ    PIC N(1)  VALUE SPACE.
003410          07 被保険者平成チェックＷ    PIC N(1)  VALUE SPACE.
003420       05 被保険者生年月日Ｗ.
003430          07 被保険者和暦Ｗ            PIC 9     VALUE ZERO.
003440          07 被保険者年Ｗ              PIC 9(2)  VALUE ZERO.
003450          07 被保険者月Ｗ              PIC 9(2)  VALUE ZERO.
003460          07 被保険者日Ｗ              PIC 9(2)  VALUE ZERO.
003470       05 被保険者年齢Ｗ               PIC 9(3)  VALUE ZERO.
003480*
003490       05 被保険者郵便番号Ｗ.
003500          07 被保険者郵便番号１Ｗ      PIC X(3)  VALUE SPACE.
003510          07 被保険者郵便番号２Ｗ      PIC X(4)  VALUE SPACE.
003520       05 被保険者住所Ｗ.
003530          07 印刷被保険者住所Ｗ        PIC X(80) VALUE SPACE.
003540       05 被保険者電話番号Ｗ           PIC X(15) VALUE SPACE.
003550       05 分割電話番号Ｗ.
003560          07 分割電話番号１Ｗ          PIC X(5)  VALUE SPACE.
003570          07 分割電話番号２Ｗ          PIC X(5)  VALUE SPACE.
003580          07 分割電話番号３Ｗ          PIC X(5)  VALUE SPACE.
003590*          07 市外局番Ｗ                PIC X(5)  VALUE SPACE.
003600*          07 分割局番Ｗ                PIC X(5)  VALUE SPACE.
003610*          07 分割番号Ｗ                PIC X(5)  VALUE SPACE.
003620    03 資格取得年月日Ｗ.
003630       05 資格取得元号Ｗ               PIC N(2)  VALUE SPACE.
003640       05 資格取得和暦Ｗ               PIC 9(1)  VALUE ZERO.
003650       05 資格取得年Ｗ                 PIC 9(2)  VALUE ZERO.
003660       05 資格取得月Ｗ                 PIC 9(2)  VALUE ZERO.
003670       05 資格取得日Ｗ                 PIC 9(2)  VALUE ZERO.
003680    03 有効期限年月日Ｗ.
003690       05 有効期限元号Ｗ               PIC N(2)  VALUE SPACE.
003700       05 有効期限和暦Ｗ               PIC 9(1)  VALUE ZERO.
003710       05 有効期限年Ｗ                 PIC 9(2)  VALUE ZERO.
003720       05 有効期限月Ｗ                 PIC 9(2)  VALUE ZERO.
003730       05 有効期限日Ｗ                 PIC 9(2)  VALUE ZERO.
003740       05 資格和暦チェックＷ.
003750          07 資格昭和チェックＷ        PIC N(1)  VALUE SPACE.
003760          07 資格平成チェックＷ        PIC N(1)  VALUE SPACE.
003770    03 患者情報Ｗ.
003780       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
003790       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
003800       05 患者性別チェックＷ.
003810          07 患者男チェックＷ          PIC N(1)  VALUE SPACE.
003820          07 患者女チェックＷ          PIC N(1)  VALUE SPACE.
003830       05 続柄Ｗ.
003840          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
003850          07 FILLER                    PIC X(4).
003860       05 和暦チェックＷ.
003870          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
003880          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
003890          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
003900          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
003900          07 令和チェックＷ            PIC N(1)  VALUE SPACE.
003910       05 患者年Ｗ                     PIC 9(2) VALUE ZERO.
003920       05 患者月Ｗ                     PIC 9(2) VALUE ZERO.
003930       05 患者日Ｗ                     PIC 9(2) VALUE ZERO.
003940       05 患者年齢Ｗ                   PIC 9(3) VALUE ZERO.
003950       05 患者性別Ｗ                   PIC N(4) VALUE SPACE.
003960    03 保険種別チェックＷ.
003970       05 社保チェックＷ               PIC N(1) VALUE SPACE.
003980       05 組合チェックＷ               PIC N(1) VALUE SPACE.
003990       05 共済チェックＷ               PIC N(1) VALUE SPACE.
004000       05 国保チェックＷ               PIC N(1) VALUE SPACE.
004010       05 日雇チェックＷ               PIC N(1) VALUE SPACE.
004020       05 船員チェックＷ               PIC N(1) VALUE SPACE.
004030       05 老人チェックＷ               PIC N(1) VALUE SPACE.
004040       05 福チェックＷ                 PIC N(1) VALUE SPACE.
004050       05 身障チェックＷ               PIC N(1) VALUE SPACE.
004060       05 退職チェックＷ               PIC N(1) VALUE SPACE.
004070       05 母子チェックＷ               PIC N(1) VALUE SPACE.
004080       05 乳幼児チェックＷ             PIC N(1) VALUE SPACE.
004090       05 国保組合チェックＷ           PIC N(1) VALUE SPACE.
004100       05 労災チェックＷ               PIC N(1) VALUE SPACE.
004110       05 自賠チェックＷ               PIC N(1) VALUE SPACE.
004120       05 生保チェックＷ               PIC N(1) VALUE SPACE.
004130       05 原爆チェックＷ               PIC N(1) VALUE SPACE.
004140       05 患者郵便番号Ｗ.
004150          07 患者郵便番号１Ｗ          PIC X(3)  VALUE SPACE.
004160          07 患者郵便番号２Ｗ          PIC X(4)  VALUE SPACE.
004170       05 患者住所Ｗ.
004180          07 患者住所１Ｗ              PIC X(40) VALUE SPACE.
004190          07 患者住所２Ｗ              PIC X(40) VALUE SPACE.
004200       05 患者電話番号Ｗ               PIC X(15) VALUE SPACE.
004210    03 負傷原因Ｗ                      PIC X(210) OCCURS 27 VALUE SPACE.
004220**************
004230* 事業所情報 *
004240**************
004250 01 事業所情報Ｗ.
004260    03 事業所名称Ｗ.
004270       05 印刷事業所名称Ｗ             PIC X(60)  VALUE SPACE.
004280    03 事業所郵便番号Ｗ.
004290       05 事業所郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
004300       05 事業所郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
004310    03 事業所住所Ｗ.
004320       05 印刷事業所住所Ｗ             PIC X(50)  VALUE SPACE.
004320       05 印刷事業所住所２Ｗ           PIC X(50)  VALUE SPACE.
004340**************
004350* 請求先情報 *
004360**************
004370 01 請求先情報Ｗ.
004380    03 保険者番号Ｗ.
004390       05 印刷保険者番号Ｗ             PIC X(8)  VALUE SPACE.
004400       05 FILLER                       PIC X(2).
004410    03 請求先名称Ｗ.
004420       05 印刷請求先名称Ｗ             PIC X(40) VALUE SPACE.
004430    03 支部名Ｗ.
004440       05 印刷支部名Ｗ                 PIC X(40) VALUE SPACE.
004450    03 請求先名支部名Ｗ.
004460       05 印刷請求先名支部名Ｗ         PIC X(54) VALUE SPACE.
004470       05 FILLER                       PIC X(26).
004480    03 保険者呼名Ｗ.
004490*       05 印刷保険者呼名Ｗ             PIC N(7)  VALUE SPACE.
004500       05 印刷保険者呼名Ｗ             PIC X(14)  VALUE SPACE.
004510    03 請求先郵便番号Ｗ.
004520       05 請求先郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
004530       05 請求先郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
004540    03 請求先住所１Ｗ.
004550       05 印刷請求先住所１Ｗ           PIC X(40) VALUE SPACE.
004560    03 請求先住所２Ｗ.
004570       05 印刷請求先住所２Ｗ           PIC X(35) VALUE SPACE.
004580       05 FILLER                       PIC X(5).
004590****************
004600* 負傷データＦ *
004610****************
004620 01 負傷情報Ｗ.
004630    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
004640    03 負傷データ情報Ｗ  OCCURS   9.
004650       05 会部位コードＷ               PIC X(4)  VALUE SPACE.
004660       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
004670       05 負傷種別Ｗ                   PIC 9(2)  VALUE ZERO.
004680       05 部位Ｗ                       PIC 9(2)  VALUE ZERO.
004690       05 左右区分Ｗ                   PIC 9(1)  VALUE ZERO.
004700       05 負傷位置番号Ｗ               PIC 9(2)  VALUE ZERO.
004710       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
004720       05 負傷年月日Ｗ.
004730          07 負傷和暦Ｗ                PIC 9(1)  VALUE ZERO.
004740          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
004750          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
004760          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
004770          07 負傷年月日区切Ｗ          PIC X(1)  VALUE SPACE.
004780       05 施術開始年月日Ｗ.
004790          07 施術開始和暦Ｗ            PIC 9(1)  VALUE ZERO.
004800          07 施術開始年Ｗ              PIC 9(2)  VALUE ZERO.
004810          07 施術開始月Ｗ              PIC 9(2)  VALUE ZERO.
004820          07 施術開始日Ｗ              PIC 9(2)  VALUE ZERO.
004830          07 施開年月日区切Ｗ          PIC X(1)  VALUE SPACE.
004840       05 施術終了年月日Ｗ.
004850          07 施術終了和暦Ｗ            PIC 9(1)  VALUE ZERO.
004860          07 施術終了年Ｗ              PIC 9(2)  VALUE ZERO.
004870          07 施術終了月Ｗ              PIC 9(2)  VALUE ZERO.
004880          07 施術終了日Ｗ              PIC 9(2)  VALUE ZERO.
004890          07 施終年月日区切Ｗ          PIC X(1)  VALUE SPACE.
004900       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
004910       05 転帰Ｗ                       PIC N(4)  VALUE SPACE.
004920       05 転帰チェックＷ.
004930          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
004940          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
004950          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
004960*
004970******************
004980* 負担率チェック *
004990******************
005000 01 負担率チェックＷ.
005010    03 ０割チェックＷ                  PIC N(1)  VALUE SPACE.
005020    03 １割チェックＷ                  PIC N(1)  VALUE SPACE.
005030    03 ２割チェックＷ                  PIC N(1)  VALUE SPACE.
005040    03 ３割チェックＷ                  PIC N(1)  VALUE SPACE.
005050**********
005060* 合計行 *
005070**********
005080 01 合計行Ｗ.
005090    03 請求終了日Ｗ                    PIC 9(2) VALUE ZERO.
005100    03 対象西暦Ｗ                      PIC 9(4) VALUE ZERO.
005110    03 商Ｗ                            PIC 9(3) VALUE ZERO.
005120    03 余Ｗ                            PIC 9(3) VALUE ZERO.
005130*/年月毎持つ保険者データのワーク
005140 01 保負担率区分Ｗ                     PIC 9    VALUE ZERO.
005150 01 保本人負担率Ｗ                     PIC 9(3) VALUE ZERO.
005160 01 保家族負担率Ｗ                     PIC 9(3) VALUE ZERO.
005170 01 保本人負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
005180 01 保家族負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
005190*
005200 01 比較和暦年月Ｗ.
005210    03 比較和暦Ｗ                      PIC 9    VALUE ZERO.
005220    03 比較年Ｗ                        PIC 9(2) VALUE ZERO.
005230    03 比較月Ｗ                        PIC 9(2) VALUE ZERO.
005240*
005250 01 印刷制御.
005260     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
005270     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
005280     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
005290     03 拡張制御Ｐ.
005300         05 端末制御Ｐ.
005310             07 移動方向Ｐ             PIC X(1)  VALUE SPACE.
005320             07 移動行数Ｐ             PIC 9(3)  VALUE ZERO.
005330         05 詳細制御Ｐ                 PIC X(2)  VALUE SPACE.
005340     03 通知情報Ｐ                     PIC X(2)  VALUE SPACE.
005350     03 ユニット名Ｐ                   PIC X(8)  VALUE SPACE.
005360*
005370 01 計算機西暦年Ｗ                     PIC 9(2)  VALUE ZERO.
005380* 日付ＷＯＲＫ
005390 01 和暦終了年Ｗ                       PIC 9(4)  VALUE ZERO.
005400 01 計算機西暦.
005410    03 計算機西暦年                    PIC 9(4)  VALUE ZERO.
005420    03 計算機西暦月日                  PIC 9(4)  VALUE ZERO.
005430 01 計算機西暦Ｒ REDEFINES 計算機西暦.
005440    03 計算機世紀                      PIC 9(2).
005450    03 計算機日付                      PIC 9(6).
005460    03 計算機日付Ｒ REDEFINES 計算機日付.
005470       05 計算機年月                   PIC 9(4).
005480       05 計算機年月Ｒ REDEFINES 計算機年月.
005490         07 計算機年                   PIC 9(2).
005500         07 計算機月                   PIC 9(2).
005510       05 計算機日                     PIC 9(2).
005520*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
005530******************************************************************
005540*                          連結項目                              *
005550******************************************************************
005560*
005570**********************
005580* メッセージ表示キー *
005590**********************
005600 01 連メ－キー IS EXTERNAL.
005610    03  連メ－メッセージ                 PIC N(20).
005620*
005630****************
005640* 連結項目待避 *
005650****************
005660*    ************
005670*    * 料金情報 *
005680*    ************
005690*    月毎の料金
005700***********************
005710 01 連施－料金１ IS EXTERNAL.
005720   03 連施－本人負担率                 PIC 9(3).
005730************
005740* 印刷キー *
005750************
005760 01 連印－対象データ IS EXTERNAL.
005770    03 連印－施術和暦年月.
005780       05 連印－施術和暦                  PIC 9(1).
005790       05 連印－施術年                    PIC 9(2).
005800       05 連印－施術月                    PIC 9(2).
005810    03 連印－保険種別                     PIC 9(2).
005820    03 連印－保険者番号                   PIC X(10).
005830    03 連印－本人家族区分                 PIC 9(1).
005840    03 連印－被保険者カナ                 PIC X(20).
005850    03 連印－患者コード.
005860       05 連印－患者番号                  PIC 9(6).
005870       05 連印－枝番                      PIC X(1).
005880    03 連印－印刷モードＦ                 PIC 9(1).
005940*/詳細
005950    03 連印－保険証印刷区分               PIC 9(1).
005980    03 連印－負傷詳細 OCCURS 7.
005990       05 連印－負傷印刷行                PIC 9(1).
006000       05 連印－部位印刷区分              PIC 9(1).
006010       05 連印－転帰印刷区分              PIC 9(1).
006020       05 連印－原因印刷区分              PIC 9(1).
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
006110*
       01 連入－表示フラグ６６０ IS EXTERNAL GLOBAL.
          03 連入－プレビュー区分               PIC 9(1).
006330* 負担率取得用14/10～
006340 01 連率－負担率取得キー IS EXTERNAL.
006350    03 連率－施術和暦年月.
006360       05 連率－施術和暦               PIC 9.
006370       05 連率－施術年月.
006380          07 連率－施術年              PIC 9(2).
006390          07 連率－施術月              PIC 9(2).
006400    03 連率－患者コード.
006410       05 連率－患者番号               PIC 9(6).
006420       05 連率－枝番                   PIC X.
006430    03 連率－実際負担率                PIC 9(3).
006440    03 連率－実際本体負担率            PIC 9(3).
006450    03 連率－健保負担率                PIC 9(3).
006460    03 連率－２７老負担率              PIC 9(3).
006470    03 連率－助成負担率                PIC 9(3).
006480    03 連率－特別用負担率              PIC 9(3).
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
006440******************************************************************
006450*                      PROCEDURE  DIVISION                       *
006460******************************************************************
006470 PROCEDURE               DIVISION.
006480************
006490*           *
006500* 初期処理   *
006510*           *
006520************
002570     PERFORM プリンタファイル作成.
006530     PERFORM 初期化.
006540************
006550*           *
006560* 主処理     *
006570*           *
006580************
006590* 印刷
006600     PERFORM 連結項目待避.
006610     PERFORM 印刷セット
006620     PERFORM 印刷処理
006630************
006640*           *
006650* 終了処理   *
006660*           *
006670************
006680     PERFORM 終了処理.
006690     EXIT PROGRAM.
006700*
006710*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YCH6622"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
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
006720*================================================================*
006730 初期化 SECTION.
006740*
006750     PERFORM ファイルオープン.
006760*    /* 現在日付取得 */
006770     ACCEPT 計算機日付 FROM DATE.
006780*    /* 1980～2079年の間で設定 */
006790     IF 計算機年 > 80
006800         MOVE 19 TO 計算機世紀
006810     ELSE
006820         MOVE 20 TO 計算機世紀
006830     END-IF.
006840     PERFORM カレント元号取得.
006850     PERFORM 和暦終了年取得.
006860     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
006870*================================================================*
006880 カレント元号取得 SECTION.
006890*
006900     MOVE ZEROS TO 制－制御区分.
006910     READ 制御情報マスタ
006920     NOT INVALID KEY
006930         MOVE 制－カレント元号           TO カレント元号Ｗ
006940         MOVE 制－カルテ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
006950         MOVE 制－バーコード２３２Ｃ使用区分  TO バーコード使用区分Ｗ
006960     END-READ.
006970*
006980*================================================================*
006990 和暦終了年取得 SECTION.
007000*
007010     MOVE カレント元号Ｗ TO 元－元号区分.
007020     READ 元号マスタ
007030     INVALID KEY
007040         DISPLAY NC"指定和暦が登録されていません" UPON CONS
007050         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
007060                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007070         ACCEPT  キー入力 FROM CONS
007080         PERFORM 終了処理
007090         EXIT PROGRAM
007100     NOT INVALID KEY
007110         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
007120         MOVE 前和暦Ｗ TO 元－元号区分
007130         READ 元号マスタ
007140         INVALID KEY
007150             DISPLAY NC"指定和暦が登録されていません" UPON CONS
007160             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
007170                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007180             ACCEPT  キー入力 FROM CONS
007190             PERFORM 終了処理
007200             EXIT PROGRAM
007210         NOT INVALID KEY
007220             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
007230         END-READ
007240     END-READ.
007250*
007260*================================================================*
007270 連結項目待避 SECTION.
007280*
007290     INITIALIZE 対象データＷＲ.
007300     MOVE 連印－施術和暦      TO 施術和暦ＷＲ.
007310     MOVE 連印－施術年        TO 施術年ＷＲ.
007320     MOVE 連印－施術月        TO 施術月ＷＲ.
007330     MOVE 連印－保険種別      TO 保険種別ＷＲ.
007340     MOVE 連印－保険者番号    TO 保険者番号ＷＲ.
007350     MOVE 連印－本人家族区分  TO 本人家族区分ＷＲ.
007360     MOVE 連印－被保険者カナ  TO 被保険者カナＷＲ.
007370     MOVE 連印－患者番号      TO 患者番号ＷＲ.
007380     MOVE 連印－枝番          TO 枝番ＷＲ.
007390     MOVE 連印－印刷モードＦ  TO 印刷モードＦＷＲ.
007400     MOVE 連印－提出年        TO 提出年ＷＲ.
007410     MOVE 連印－提出月        TO 提出月ＷＲ.
007420     MOVE 連印－提出日        TO 提出日ＷＲ.
007430     MOVE 連印－段数          TO 段数ＷＲ.
007440*================================================================*
007450 ファイルオープン SECTION.
007460*
007470     OPEN INPUT   保険者マスタ
007480         MOVE NC"保険者" TO ファイル名.
007490         PERFORM オープンチェック.
007500     OPEN INPUT   元号マスタ
007510         MOVE NC"元号" TO ファイル名.
007520         PERFORM オープンチェック.
007530     OPEN INPUT   名称マスタ
007540         MOVE NC"名称" TO ファイル名.
007550         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
007590     OPEN INPUT   制御情報マスタ
007600         MOVE NC"制御情報" TO ファイル名.
007610         PERFORM オープンチェック.
007650     OPEN INPUT   請求先マスタ
007660         MOVE NC"請先" TO ファイル名.
007670         PERFORM オープンチェック.
007710     OPEN INPUT   受診者情報Ｆ.
007720         MOVE NC"受情" TO ファイル名.
007730         PERFORM オープンチェック.
007740     OPEN INPUT   施術記録Ｆ.
007750         MOVE NC"施記Ｆ" TO ファイル名.
007760         PERFORM オープンチェック.
007770     OPEN INPUT   負傷データＦ.
007780         MOVE NC"負傷" TO ファイル名.
007790         PERFORM オープンチェック.
007830     OPEN INPUT   負傷原因Ｆ.
007840         MOVE NC"負傷原因" TO ファイル名.
007850         PERFORM オープンチェック.
007900     OPEN INPUT 事業所マスタ.
007910         MOVE NC"事業所" TO ファイル名.
007920         PERFORM オープンチェック.
007900     OPEN INPUT 労災情報Ｆ.
007910         MOVE NC"労災情報Ｆ" TO ファイル名.
007920         PERFORM オープンチェック.
007860     OPEN I-O   印刷ファイル
007870         MOVE NC"印刷" TO ファイル名.
007880         PERFORM エラー処理Ｐ.
007890*================================================================*
007900 オープンチェック SECTION.
007910*
007920     IF 状態キー  NOT =  "00"
007930         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
007940         DISPLAY NC"状態キー：" 状態キー         UPON CONS
007950         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
007960                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007970         ACCEPT  キー入力 FROM CONS
007980         PERFORM ファイル閉鎖
007990         EXIT PROGRAM.
008000*================================================================*
008010 ファイル閉鎖 SECTION.
008020*
008030     CLOSE 印刷ファイル    保険者マスタ  元号マスタ
008040           制御情報マスタ  請求先マスタ  名称マスタ
008050           受診者情報Ｆ    施術記録Ｆ    レセプトＦ
008060           負傷データＦ    事業所マスタ  労災情報Ｆ
008070           負傷原因Ｆ.
008080*================================================================*
008090 終了処理 SECTION.
008100*
008110     PERFORM ファイル閉鎖.
008120*================================================================*
008130 印刷セット SECTION.
008140*
008150     EVALUATE 印刷モードＦＷＲ
008160     WHEN 0
008170         PERFORM 印刷セット１
008180     WHEN 1
008190         PERFORM 印刷セット２
008200     WHEN 2
008210         PERFORM 印刷セット３
008240     END-EVALUATE.
008250*
008260*================================================================*
008270 印刷セット１ SECTION.
008280*
008290     INITIALIZE YCH6622P.
008300     MOVE "X" TO EDIT-MODE OF 患者コード.
008310     INITIALIZE 受診者情報Ｗ.
008320     INITIALIZE 事業所情報Ｗ.
008330     INITIALIZE 請求先情報Ｗ.
008340     INITIALIZE 負傷情報Ｗ.
008350     INITIALIZE 負担率チェックＷ.
008360     PERFORM 負担率取得.
008370     PERFORM 請求先情報取得.
008380     PERFORM 受診者情報取得.
008390     PERFORM 負傷データ取得.
008400*
008410     IF 負傷原因印刷区分Ｗ  NOT = 1 
008420        PERFORM 負傷原因取得
008430     END-IF.
008440*
008450     PERFORM 印刷上部セット.
008460     PERFORM 印刷下部セット.
008470*================================================================*
008480 印刷セット２ SECTION.
008490*
008500     INITIALIZE YCH6622P.
008510     MOVE "X" TO EDIT-MODE OF 患者コード.
008520     INITIALIZE 受診者情報Ｗ.
008530     INITIALIZE 事業所情報Ｗ.
008540     INITIALIZE 請求先情報Ｗ.
008550     INITIALIZE 負傷情報Ｗ.
008560     INITIALIZE 負担率チェックＷ.
008570     PERFORM 負担率取得.
008580     PERFORM 請求先情報取得.
008590     PERFORM 受診者情報取得.
008600     PERFORM 印刷上部セット.
008610*================================================================*
008620 印刷セット３ SECTION.
008630*
008640     INITIALIZE YCH6622P.
008650     MOVE "X" TO EDIT-MODE OF 患者コード.
008660     INITIALIZE 受診者情報Ｗ.
008670     INITIALIZE 事業所情報Ｗ.
008680     INITIALIZE 請求先情報Ｗ.
008690     INITIALIZE 負傷情報Ｗ.
008590     PERFORM 受診者情報取得.
008700     PERFORM 負傷データ取得.
008710*
008720     IF 負傷原因印刷区分Ｗ  NOT = 1 
008730        PERFORM 負傷原因取得
008740     END-IF.
008750*
008760     PERFORM 印刷下部セット.
008920*================================================================*
008930 印刷上部セット SECTION.
008940*
008950     IF バーコード使用区分Ｗ = 1
008960        MOVE 患者コードＷ   TO 患者コード
008970        MOVE SPACE          TO EDIT-MODE OF 患者コード
008980     END-IF.
008990*
009000     MOVE 患者番号Ｗ                 TO 患者番号.
009010     MOVE 枝番Ｗ                     TO 枝番.
009020**************************
009030* 被保険者証情報セット   *
009040**************************
009050*     IF 記号Ｗ(1:2) = "＊" 
009060*        MOVE  SPACE    TO  記号
009070*     ELSE
009080*        MOVE 記号Ｗ    TO  記号
009090*     END-IF.
009100*     IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
009110*        ( 印刷番号Ｗ(1:2) = "＊" )
009120*        MOVE  SPACE      TO  番号
009130*     ELSE
009140*        MOVE 印刷番号Ｗ  TO  番号
009150*     END-IF.
009160************************
009170* 被保険者情報セット   *
009180************************
009190*     MOVE 被保険者氏名Ｗ             TO 被保険者氏名.
009200*     MOVE 被保険者カナＷ             TO 被保険者カナ.
009210*     MOVE 資格取得元号Ｗ             TO 資格取得元号.
009220*     MOVE 資格取得年Ｗ               TO 資格取得年.
009230*     MOVE 資格取得月Ｗ               TO 資格取得月.
009240*     MOVE 資格取得日Ｗ               TO 資格取得日.
009250*     MOVE 資格昭和チェックＷ         TO 資格昭和チェック.
009260*     MOVE 資格平成チェックＷ         TO 資格平成チェック.
009270*/有効期限
009280*     MOVE 有効期限年Ｗ               TO 有効年.
009290*     MOVE 有効期限月Ｗ               TO 有効月.
009300*     MOVE 有効期限日Ｗ               TO 有効日.
009310*    IF ( 有効期限年Ｗ = ZERO ) AND
009320*        ( 有効期限月Ｗ = ZERO ) AND
009330*        ( 有効期限日Ｗ = ZERO )
009340*         MOVE SPACE          TO 有効元号
009350*         MOVE SPACE          TO 有効年月日表示
009360*         MOVE SPACE          TO 有効年表示
009370*         MOVE SPACE          TO 有効月表示
009380*         MOVE SPACE          TO 有効日表示
009390*     ELSE
009400*         MOVE 有効期限元号Ｗ TO 有効元号
009410*         MOVE NC"有効年月日" TO 有効年月日表示
009420*         MOVE NC"年"         TO 有効年表示
009430*         MOVE NC"月"         TO 有効月表示
009440*         MOVE NC"日"         TO 有効日表示
009450*     END-IF.
009460*     MOVE 被保険者明治チェックＷ     TO 被保険者明治チェック.
009470*     MOVE 被保険者大正チェックＷ     TO 被保険者大正チェック.
009480*     MOVE 被保険者昭和チェックＷ     TO 被保険者昭和チェック.
009490*     MOVE 被保険者平成チェックＷ     TO 被保険者平成チェック.
009500*     MOVE 被保険者年Ｗ               TO 被保険者年.
009510*     MOVE 被保険者月Ｗ               TO 被保険者月.
009520*     MOVE 被保険者日Ｗ               TO 被保険者日.
009530*     MOVE 被保険者年齢Ｗ             TO 被保険者年齢.
009540********************
009550* 患者情報セット   *
009560********************
009570*     MOVE ０割チェックＷ             TO ０割チェック.
009580*     MOVE １割チェックＷ             TO １割チェック.
009590*     MOVE ２割チェックＷ             TO ２割チェック.
009600*     MOVE ３割チェックＷ             TO ３割チェック.
009610*     IF  印刷市町村番号Ｗ(1:2) = "99"
009620*         MOVE SPACE                  TO 市町村番号
009630*     ELSE
009640*         MOVE 印刷市町村番号Ｗ       TO 市町村番号
009650*     END-IF.
009660*     IF (印刷受益者番号Ｗ(1:1) = "*") OR
009670*        (印刷受益者番号Ｗ(1:2) = "＊") 
009680*         MOVE SPACE                  TO 受益者番号
009690*     ELSE
009700*         MOVE 印刷受益者番号Ｗ       TO 受益者番号
009710*     END-IF
009720*
009730*     IF  印刷費用負担者番号Ｗ(1:2) = "99"
009740*         MOVE SPACE                  TO 公費負担者番号
009750*     ELSE
009760*         MOVE 印刷費用負担者番号Ｗ   TO 公費負担者番号
009770*     END-IF.
009780*
009790*     IF (印刷受益者番号助成Ｗ(1:1) = "*") OR
009800*        (印刷受益者番号助成Ｗ(1:2) = "＊")
009810*         MOVE SPACE                  TO 受益者番号助成
009820*     ELSE
009830*         MOVE 印刷受益者番号助成Ｗ   TO 受益者番号助成
009840*     END-IF.
009850     MOVE 患者カナＷ                 TO 患者カナ.
009860     MOVE 患者氏名Ｗ                 TO 患者氏名.
009870*     MOVE 被保険者男チェックＷ       TO 被保険者男チェック.
009880*     MOVE 被保険者女チェックＷ       TO 被保険者女チェック.
009890*     MOVE 被保険者郵便番号１Ｗ       TO 被保険者郵便番号１.
009900*     MOVE 被保険者郵便番号２Ｗ       TO 被保険者郵便番号２.
009910*     MOVE "-"                        TO 被保険者郵便区切.
009920*     MOVE 被保険者住所Ｗ             TO 被保険者住所.
009930*     MOVE 被保険者電話番号Ｗ         TO 被保険者電話番号.
009940     MOVE 患者男チェックＷ           TO 患者男チェック.
009950     MOVE 患者女チェックＷ           TO 患者女チェック.
009960     MOVE 明治チェックＷ             TO 明治チェック.
009970     MOVE 大正チェックＷ             TO 大正チェック.
009980     MOVE 昭和チェックＷ             TO 昭和チェック.
009990     MOVE 平成チェックＷ             TO 平成チェック.
           IF 令和チェックＷ NOT = SPACE
              MOVE NC"令和"                TO 元号ＣＭ
              MOVE 令和チェックＷ          TO 平成チェック
           END-IF.
010000     MOVE 患者年Ｗ                   TO 患者年.
010010     MOVE 患者月Ｗ                   TO 患者月.
010020     MOVE 患者日Ｗ                   TO 患者日.
010030     MOVE 患者年齢Ｗ                 TO 患者年齢.
010040*     MOVE 印刷続柄Ｗ                 TO 続柄.
010050     MOVE 患者郵便番号１Ｗ           TO 患者郵便番号１.
010060     MOVE 患者郵便番号２Ｗ           TO 患者郵便番号２.
010070     MOVE "-"                        TO 患者郵便区切.
010080     MOVE 患者住所Ｗ                 TO 患者住所.
010090     MOVE 患者電話番号Ｗ             TO 患者電話番号.
010100*     MOVE 患者住所１Ｗ               TO 患者住所１.
010110*     MOVE 患者住所２Ｗ               TO 患者住所２.
010120********************
010130* 事業所情報セット *
010140********************
010150     MOVE 事業所名称Ｗ               TO 事業所名称.
010160*     MOVE 事業所郵便番号１Ｗ         TO 事業所郵便１.
010170*     MOVE "-"                        TO 事業所郵便２.
010180*     MOVE 事業所郵便番号２Ｗ         TO 事業所郵便３.
010190     MOVE 印刷事業所住所Ｗ           TO 事業所住所.
010190     MOVE 印刷事業所住所２Ｗ         TO 事業所住所２.
010200************************************
010210* 都道府県・健康保険組合情報セット *
010220************************************
010230*労災、自賠、生保の時は保険者情報は印字しない
010240*     IF 保険種別ＷＲ = 70 OR 80 OR 90
010250*         MOVE SPACE                  TO 保険者番号
010260*         MOVE SPACE                  TO 請求先名支部名
010270*     ELSE
010280*         MOVE 印刷保険者番号Ｗ       TO 保険者番号
010290*         MOVE 印刷請求先名支部名Ｗ   TO 請求先名支部名
010300*     END-IF.
010310**************************
010320* 保険種別チェックセット *
010330**************************
010340*     MOVE 社保チェックＷ             TO 社保チェック.
010350*     MOVE 組合チェックＷ             TO 組合チェック.
010360*     MOVE 日雇チェックＷ             TO 日雇チェック.
010370*     MOVE 船員チェックＷ             TO 船員チェック.
010380*     MOVE 共済チェックＷ             TO 共済チェック.
010390*     MOVE 国保チェックＷ             TO 国保チェック.
010400*     MOVE 退職チェックＷ             TO 退職チェック.
010410*     MOVE 老人チェックＷ             TO 老人チェック.
010420*     MOVE 乳幼児チェックＷ           TO 乳幼児チェック.
010430*     MOVE 自賠チェックＷ             TO 自賠チェック.
010440*     MOVE 生保チェックＷ             TO 生保チェック.
010450*     MOVE 福チェックＷ               TO 福チェック.
010460*     MOVE 身障チェックＷ             TO 身障チェック.
010470*     MOVE 母子チェックＷ             TO 母子チェック.
010480*     MOVE 生保チェックＷ             TO 生保チェック.
010490*     MOVE 原爆チェックＷ             TO 原爆チェック.
010500*================================================================*
010510 印刷下部セット SECTION.
010520*
010530********************
010540* 負傷データセット *
010550********************
010560* １部位 *
010570**********
010580*     MOVE 会部位コードＷ(1)   TO 負傷コード１.
010590     MOVE 負傷名Ｗ(1)         TO 負傷名１.
010600     MOVE 負傷年Ｗ(1)         TO 負傷年１.
010610     MOVE 負傷月Ｗ(1)         TO 負傷月１.
010620     MOVE 負傷日Ｗ(1)         TO 負傷日１.
010630     MOVE 施術開始年Ｗ(1)     TO 施術開始年１.
010640     MOVE 施術開始月Ｗ(1)     TO 施術開始月１.
010650     MOVE 施術開始日Ｗ(1)     TO 施術開始日１.
010660     MOVE 施術終了年Ｗ(1)     TO 施術終了年１.
010670     MOVE 施術終了月Ｗ(1)     TO 施術終了月１.
010680     MOVE 施術終了日Ｗ(1)     TO 施術終了日１.
010690*     MOVE 負傷年月日区切Ｗ(1) TO 区切１１ 区切１２.
010700*     MOVE 施開年月日区切Ｗ(1) TO 区切１３ 区切１４.
010710*     MOVE 施終年月日区切Ｗ(1) TO 区切１５ 区切１６.
010720*     MOVE 転帰Ｗ(1)           TO 転帰１.
010730     MOVE 治癒チェックＷ(1)   TO 治癒チェック１.
010740     MOVE 中止チェックＷ(1)   TO 中止チェック１.
010750     MOVE 転医チェックＷ(1)   TO 転医チェック１.
010760*     ******************
010770*     * 日数は一時保留 *
010780*     ******************
010790*     **********************
010800*     * 施術回数は一時保留 *
010810*     **********************
010820*     **********************
010830*     * 転帰名称は一時保留 *
010840*     **********************
010850**********
010860* ２部位 *
010870**********
010880*     MOVE 会部位コードＷ(2)   TO 負傷コード２.
010890     MOVE 負傷名Ｗ(2)         TO 負傷名２.
010900     MOVE 負傷年Ｗ(2)         TO 負傷年２.
010910     MOVE 負傷月Ｗ(2)         TO 負傷月２.
010920     MOVE 負傷日Ｗ(2)         TO 負傷日２.
010930     MOVE 施術開始年Ｗ(2)     TO 施術開始年２.
010940     MOVE 施術開始月Ｗ(2)     TO 施術開始月２.
010950     MOVE 施術開始日Ｗ(2)     TO 施術開始日２.
010960     MOVE 施術終了年Ｗ(2)     TO 施術終了年２.
010970     MOVE 施術終了月Ｗ(2)     TO 施術終了月２.
010980     MOVE 施術終了日Ｗ(2)     TO 施術終了日２.
010990*     MOVE 負傷年月日区切Ｗ(2) TO 区切２１ 区切２２.
011000*     MOVE 施開年月日区切Ｗ(2) TO 区切２３ 区切２４.
011010*     MOVE 施終年月日区切Ｗ(2) TO 区切２５ 区切２６.
011020*     MOVE 転帰Ｗ(2)           TO 転帰２.
011030     MOVE 治癒チェックＷ(2)   TO 治癒チェック２.
011040     MOVE 中止チェックＷ(2)   TO 中止チェック２.
011050     MOVE 転医チェックＷ(2)   TO 転医チェック２.
011060*     ******************
011070*     * 日数は一時保留 *
011080*     ******************
011090*     **********************
011100*     * 施術回数は一時保留 *
011110*     **********************
011120*     **********************
011130*     * 転帰名称は一時保留 *
011140*     **********************
011150**********
011160* ３部位 *
011170**********
011180*     MOVE 会部位コードＷ(3)   TO 負傷コード３.
011190     MOVE 負傷名Ｗ(3)         TO 負傷名３.
011200     MOVE 負傷年Ｗ(3)         TO 負傷年３.
011210     MOVE 負傷月Ｗ(3)         TO 負傷月３.
011220     MOVE 負傷日Ｗ(3)         TO 負傷日３.
011230     MOVE 施術開始年Ｗ(3)     TO 施術開始年３.
011240     MOVE 施術開始月Ｗ(3)     TO 施術開始月３.
011250     MOVE 施術開始日Ｗ(3)     TO 施術開始日３.
011260     MOVE 施術終了年Ｗ(3)     TO 施術終了年３.
011270     MOVE 施術終了月Ｗ(3)     TO 施術終了月３.
011280     MOVE 施術終了日Ｗ(3)     TO 施術終了日３.
011290*     MOVE 負傷年月日区切Ｗ(3) TO 区切３１ 区切３２.
011300*     MOVE 施開年月日区切Ｗ(3) TO 区切３３ 区切３４.
011310*     MOVE 施終年月日区切Ｗ(3) TO 区切３５ 区切３６.
011320*     MOVE 転帰Ｗ(3)           TO 転帰３.
011330     MOVE 治癒チェックＷ(3)   TO 治癒チェック３.
011340     MOVE 中止チェックＷ(3)   TO 中止チェック３.
011350     MOVE 転医チェックＷ(3)   TO 転医チェック３.
011360*     ******************
011370*     * 日数は一時保留 *
011380*     ******************
011390*     **********************
011400*     * 施術回数は一時保留 *
011410*     **********************
011420*     **********************
011430*     * 転帰名称は一時保留 *
011440*     **********************
011450**********
011460* ４部位 *
011470**********
011480*     MOVE 会部位コードＷ(4)   TO 負傷コード４.
011490     MOVE 負傷名Ｗ(4)         TO 負傷名４.
011500     MOVE 負傷年Ｗ(4)         TO 負傷年４.
011510     MOVE 負傷月Ｗ(4)         TO 負傷月４.
011520     MOVE 負傷日Ｗ(4)         TO 負傷日４.
011530     MOVE 施術開始年Ｗ(4)     TO 施術開始年４.
011540     MOVE 施術開始月Ｗ(4)     TO 施術開始月４.
011550     MOVE 施術開始日Ｗ(4)     TO 施術開始日４.
011560     MOVE 施術終了年Ｗ(4)     TO 施術終了年４.
011570     MOVE 施術終了月Ｗ(4)     TO 施術終了月４.
011580     MOVE 施術終了日Ｗ(4)     TO 施術終了日４.
011590*     MOVE 負傷年月日区切Ｗ(4) TO 区切４１ 区切４２.
011600*     MOVE 施開年月日区切Ｗ(4) TO 区切４３ 区切４４.
011610*     MOVE 施終年月日区切Ｗ(4) TO 区切４５ 区切４６.
011620*     MOVE 転帰Ｗ(4)           TO 転帰４.
011630     MOVE 治癒チェックＷ(4)   TO 治癒チェック４.
011640     MOVE 中止チェックＷ(4)   TO 中止チェック４.
011650     MOVE 転医チェックＷ(4)   TO 転医チェック４.
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
011760* ５部位 *
011770**********
011780*     MOVE 会部位コードＷ(5)   TO 負傷コード５.
011790     MOVE 負傷名Ｗ(5)         TO 負傷名５.
011800     MOVE 負傷年Ｗ(5)         TO 負傷年５.
011810     MOVE 負傷月Ｗ(5)         TO 負傷月５.
011820     MOVE 負傷日Ｗ(5)         TO 負傷日５.
011830     MOVE 施術開始年Ｗ(5)     TO 施術開始年５.
011840     MOVE 施術開始月Ｗ(5)     TO 施術開始月５.
011850     MOVE 施術開始日Ｗ(5)     TO 施術開始日５.
011860     MOVE 施術終了年Ｗ(5)     TO 施術終了年５.
011870     MOVE 施術終了月Ｗ(5)     TO 施術終了月５.
011880     MOVE 施術終了日Ｗ(5)     TO 施術終了日５.
011890*     MOVE 負傷年月日区切Ｗ(5) TO 区切５１ 区切５２.
011900*     MOVE 施開年月日区切Ｗ(5) TO 区切５３ 区切５４.
011910*     MOVE 施終年月日区切Ｗ(5) TO 区切５５ 区切５６.
011920*     MOVE 転帰Ｗ(5)           TO 転帰５.
011930     MOVE 治癒チェックＷ(5)   TO 治癒チェック５.
011940     MOVE 中止チェックＷ(5)   TO 中止チェック５.
011950     MOVE 転医チェックＷ(5)   TO 転医チェック５.
011960*     ******************
011970*     * 日数は一時保留 *
011980*     ******************
011990*     **********************
012000*     * 施術回数は一時保留 *
012010*     **********************
012020*     **********************
012030*     * 転帰名称は一時保留 *
012040*     **********************
012050************
012060* 負傷原因 *
012070************
012080     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
012090     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
012100     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
012110*     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
012120*     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
012130*     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
012140*     MOVE 負傷原因Ｗ(7)       TO 負傷原因７.
012150*     MOVE 負傷原因Ｗ(8)       TO 負傷原因８.
012160*     MOVE 負傷原因Ｗ(9)       TO 負傷原因９.
012170*     MOVE 負傷原因Ｗ(10)      TO 負傷原因１０.
012180*     MOVE 負傷原因Ｗ(11)      TO 負傷原因１１.
012190*     MOVE 負傷原因Ｗ(12)      TO 負傷原因１２.
012200*     MOVE 負傷原因Ｗ(13)      TO 負傷原因１３.
012210*     MOVE 負傷原因Ｗ(14)      TO 負傷原因１４.
012220*     MOVE 負傷原因Ｗ(15)      TO 負傷原因１５.
012230*
012240*================================================================*
012250 負担率取得 SECTION.
012260*
012270     MOVE ZERO TO 本人負担率Ｗ.
012280     MOVE ZERO TO 家族負担率Ｗ.
012290*
012300     MOVE 施術和暦ＷＲ   TO 受－施術和暦.
012310     MOVE 施術年ＷＲ     TO 受－施術年.
012320     MOVE 施術月ＷＲ     TO 受－施術月.
012330     MOVE 患者番号ＷＲ   TO 受－患者番号.
012340     MOVE 枝番ＷＲ       TO 受－枝番.
012350     READ 受診者情報Ｆ
012360     INVALID KEY
012370         MOVE  NC"施術月の受診者情報がありません" TO 連メ－メッセージ
012380         CALL   "MSG001"
012390         CANCEL "MSG001"
012400         PERFORM ファイル閉鎖
012410         MOVE ZERO TO PROGRAM-STATUS
012420         EXIT PROGRAM
012430     NOT INVALID KEY
013360* 14/10～　サブルーチン処理
013370         IF 受－施術和暦年月 >= 41410
013380            PERFORM 負担率取得１４１０
013530         END-IF
013540     END-READ.
013550     EVALUATE 負担率Ｗ
013560     WHEN 0
013570         MOVE NC"○" TO ０割チェックＷ
013580     WHEN 10
013590         MOVE NC"○" TO １割チェックＷ
013600     WHEN 20
013610         MOVE NC"○" TO ２割チェックＷ
013620     WHEN 30
013630         MOVE NC"○" TO ３割チェックＷ
013640     END-EVALUATE.
014600*================================================================*
014610 負担率取得１４１０ SECTION.
014620*
014630* 受診者Ｆ READ中
014640* 平成14/10～
014650     MOVE ZERO  TO 負担率Ｗ.
014670*
014680     MOVE SPACE TO 連率－負担率取得キー.
014690     INITIALIZE 連率－負担率取得キー.
014700     MOVE 受－施術和暦年月 TO 連率－施術和暦年月.
014710     MOVE 受－患者コード   TO 連率－患者コード.
014720*
014730     CALL   "HUTANRIT".
014740     CANCEL "HUTANRIT".
014750*
014760* / 本体 /
014770     MOVE 連率－実際本体負担率 TO 負担率Ｗ.
014780*
013830*================================================================*
013840 受診者情報取得 SECTION.
013850*
013860**************************************************
013870* 連結データから受診者情報Ｆより以下の情報を取得 *
013880* ● 患者番号.... 患者番号Ｗ                     *
013890* ● 枝番........ 枝番Ｗ                         *
013900* ● 記号 ....... 記号Ｗに格納                   *
013910* ● 番号 ....... 番号Ｗに格納                   *
013920* ● 被保険者カナ.被保険者カナＷに格納           *
013930* ● 被保険者氏名.被保険者氏名Ｗに格納           *
013940* ● 郵便番号１ ..郵便番号１Ｗに格納             *
013950* ● 郵便番号２ ..郵便番号２Ｗに格納             *
013960* ● 住所１ ......住所１Ｗに格納                 *
013970* ● 住所２ ......住所２Ｗに格納                 *
013980* ● 電話番号.....電話番号Ｗに格納               *
013990* ● 患者カナ ....患者カナＷに格納               *
014000* ● 患者氏名 ....患者氏名Ｗに格納               *
014010* ● 続柄 ........名称マスタより続柄Ｗに取得     *
014020* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
014030* ● 患者年 ......患者年Ｗに格納                 *
014040* ● 患者月 ......患者月Ｗに格納                 *
014050* ● 患者日 ......患者日Ｗに格納                 *
014060**************************************************
014070     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
014080     MOVE 施術年ＷＲ         TO 受－施術年.
014090     MOVE 施術月ＷＲ         TO 受－施術月.
014100     MOVE 患者コードＷＲ     TO 受－患者コード.
014110     READ 受診者情報Ｆ
014120     INVALID KEY
014130         CONTINUE
014140*            /* ありえない */
014150     NOT INVALID KEY
014160         MOVE 受－患者番号     TO 患者番号Ｗ
014170         MOVE 受－枝番         TO 枝番Ｗ
014180*         MOVE 受－記号         TO 記号Ｗ
014190*         MOVE 受－番号         TO 番号Ｗ
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
014270         MOVE 受－被保険者カナ TO 被保険者カナＷ
014280         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
014290*
014300         EVALUATE 受－被保険者性別
014310         WHEN 1
014320             MOVE NC"○"  TO 被保険者男チェックＷ
014330         WHEN 2
014340             MOVE NC"○"  TO 被保険者女チェックＷ
014350         END-EVALUATE
014360         EVALUATE 受－患者性別
014370         WHEN 1
014380             MOVE NC"○"  TO 患者男チェックＷ
014390         WHEN 2
014400             MOVE NC"○"  TO 患者女チェックＷ
014410         END-EVALUATE
014420*
014430         EVALUATE 受－被保険者和暦
014440         WHEN 1
014450             MOVE NC"○"  TO 被保険者明治チェックＷ
014460         WHEN 2
014470             MOVE NC"○"  TO 被保険者大正チェックＷ
014480         WHEN 3
014490             MOVE NC"○"  TO 被保険者昭和チェックＷ
014500         WHEN 4
014510             MOVE NC"○"  TO 被保険者平成チェックＷ
014520         END-EVALUATE
014530         MOVE 受－被保険者年   TO 被保険者年Ｗ
014540         MOVE 受－被保険者月   TO 被保険者月Ｗ
014550         MOVE 受－被保険者日   TO 被保険者日Ｗ
014560
014570*被保険者年齢
014580     MOVE 連印－施術和暦 TO 元－元号区分
014590     READ 元号マスタ
014600     NOT INVALID KEY
014610         COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
014620     END-READ
014630     IF ( 受－被保険者和暦 NOT = ZERO ) AND
014640        ( 受－被保険者年   NOT = ZERO ) AND
014650        ( 受－被保険者月   NOT = ZERO ) AND
014660        ( 受－被保険者日   NOT = ZERO )
014670
014680         MOVE ZERO TO 被保険者年齢Ｗ
014690*
014700         MOVE 受－被保険者和暦 TO 元－元号区分
014710         READ 元号マスタ
014720         NOT INVALID KEY
014730             COMPUTE 被保険者西暦年Ｗ = 元－開始西暦年 + ( 受－被保険者年 - 1 )
014740         END-READ
014750*
014760         COMPUTE 被保険者年齢Ｗ   = 施術西暦年Ｗ - 被保険者西暦年Ｗ
014770*
014780         IF 連印－施術月 < 受－被保険者月
014790             COMPUTE 被保険者年齢Ｗ = 被保険者年齢Ｗ - 1
014800         END-IF
014810*
014820*         MOVE 被保険者年齢Ｗ     TO 被保険者年齢
014830     END-IF
014840*
014850*/郵便番号 ゼロの時はスペース
014860         IF 受－郵便番号１ = "000"
014870             MOVE SPACE          TO 被保険者郵便番号１Ｗ
014880             MOVE SPACE          TO 被保険者郵便番号２Ｗ
014890         ELSE
014900             MOVE 受－郵便番号１ TO 被保険者郵便番号１Ｗ
014910             MOVE 受－郵便番号２ TO 被保険者郵便番号２Ｗ
014920         END-IF
014930         MOVE 受－電話番号     TO 被保険者電話番号Ｗ
014940         STRING 受－住所１  DELIMITED BY SPACE
014950                受－住所２  DELIMITED BY SPACE
014960           INTO 被保険者住所Ｗ
014970         END-STRING
014980*
014990*/郵便番号 ゼロの時はスペース
015000         IF 受－患者郵便番号１ = "000"
015010             MOVE SPACE              TO 患者郵便番号１Ｗ
015020             MOVE SPACE              TO 患者郵便番号２Ｗ
015030         ELSE
015040             MOVE 受－患者郵便番号１ TO 患者郵便番号１Ｗ
015050             MOVE 受－患者郵便番号２ TO 患者郵便番号２Ｗ
015060         END-IF
015070*         MOVE 受－患者住所１     TO 患者住所１Ｗ
015080*         MOVE 受－患者住所２     TO 患者住所２Ｗ
015090         STRING 受－患者住所１  DELIMITED BY SPACE
015100                受－患者住所２  DELIMITED BY SPACE
015110           INTO 患者住所Ｗ
015120         END-STRING
015130         MOVE 受－患者電話番号   TO 患者電話番号Ｗ
015140*
015150*電話番号分割
015160*         UNSTRING 電話番号Ｗ  DELIMITED BY "-"
015170*             INTO 分割電話番号１Ｗ
015180*                  分割電話番号２Ｗ
015190*                  分割電話番号３Ｗ
015200*         END-UNSTRING
015210*電話番号右詰処理
015220*         IF 分割電話番号３Ｗ = SPACE
015230*             MOVE 分割電話番号２Ｗ TO 分割電話番号３Ｗ
015240*             MOVE 分割電話番号１Ｗ TO 分割電話番号２Ｗ
015250*             MOVE SPACE            TO 分割電話番号１Ｗ
015260*         END-IF
015270*
015280         MOVE 受－患者カナ     TO 患者カナＷ
015290         MOVE 受－患者氏名     TO 患者氏名Ｗ
015300*
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
015360* 続柄
015370         IF 受－本人家族区分 = 1
015380             MOVE NC"本人"     TO 続柄Ｗ
015390         ELSE
015400             MOVE 05          TO 名－区分コード
015410             MOVE 受－続柄    TO 名－名称コード
015420             READ 名称マスタ
015430             INVALID KEY
015440                 MOVE SPACE   TO 続柄Ｗ
015450             NOT INVALID KEY
015460                 MOVE 名－略称  TO 続柄Ｗ
015470             END-READ
015480         END-IF
015490* 患者和暦年月日
015500         EVALUATE 受－患者和暦
015510         WHEN 1
015520             MOVE NC"○"  TO 明治チェックＷ
015530         WHEN 2
015540             MOVE NC"○"  TO 大正チェックＷ
015550         WHEN 3
015560             MOVE NC"○"  TO 昭和チェックＷ
015570         WHEN 4
015580             MOVE NC"○"  TO 平成チェックＷ
015570         WHEN 5
015580             MOVE NC"○"  TO 令和チェックＷ
015590         END-EVALUATE
015600         MOVE 受－患者年  TO 患者年Ｗ
015610         MOVE 受－患者月  TO 患者月Ｗ
015620         MOVE 受－患者日  TO 患者日Ｗ
015630*患者年齢
015640     MOVE 連印－施術和暦 TO 元－元号区分
015650     READ 元号マスタ
015660     NOT INVALID KEY
015670         COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
015680     END-READ
015690     IF ( 受－患者和暦 NOT = ZERO ) AND
015700        ( 受－患者年   NOT = ZERO ) AND
015710        ( 受－患者月   NOT = ZERO ) AND
015720        ( 受－患者日   NOT = ZERO )
015730*
015740         MOVE ZERO TO 患者年齢Ｗ
015750*
015760         MOVE 受－患者和暦 TO 元－元号区分
015770         READ 元号マスタ
015780         NOT INVALID KEY
015790             COMPUTE 患者西暦年Ｗ = 元－開始西暦年 + ( 受－患者年 - 1 )
015800         END-READ
015810*
015820         COMPUTE 患者年齢Ｗ   = 施術西暦年Ｗ - 患者西暦年Ｗ
015830*
015840         IF 連印－施術月 < 受－患者月
015850             COMPUTE 患者年齢Ｗ = 患者年齢Ｗ - 1
015860         END-IF
015870*
015880         MOVE 患者年齢Ｗ     TO 患者年齢
015890     END-IF
015900* 患者性別
015910*         EVALUATE 受－患者性別
015920*         WHEN 1
015930*             MOVE NC"（男）" TO 患者性別Ｗ
015940*         WHEN 2
015950*             MOVE NC"（女）" TO 患者性別Ｗ
015960*         END-EVALUATE
015970*/有効期限
015980         MOVE 受－有効和暦   TO 有効期限和暦Ｗ
015990         MOVE 受－有効年     TO 有効期限年Ｗ
016000         MOVE 受－有効月     TO 有効期限月Ｗ
016010         MOVE 受－有効日     TO 有効期限日Ｗ
016020         MOVE 有効期限和暦Ｗ TO 元－元号区分
016030         READ 元号マスタ
016040         INVALID KEY
016050             MOVE SPACE        TO 有効期限元号Ｗ
016060         NOT INVALID KEY
016070             MOVE 元－元号名称 TO 有効期限元号Ｗ
016080         END-READ
016090*/資格取得年月日
016100         MOVE 受－資格和暦 TO 資格取得和暦Ｗ
016110         MOVE 受－資格年   TO 資格取得年Ｗ
016120         MOVE 受－資格月   TO 資格取得月Ｗ
016130         MOVE 受－資格日   TO 資格取得日Ｗ
016140         IF (資格取得和暦Ｗ NOT = ZERO) AND
016150            (資格取得年Ｗ   NOT = ZERO) AND
016160            (資格取得月Ｗ   NOT = ZERO) AND
016170            (資格取得日Ｗ   NOT = ZERO)
016180             EVALUATE 資格取得和暦Ｗ
016190             WHEN 3
016200                 MOVE NC"○"  TO 資格昭和チェックＷ
016210             WHEN 4
016220                 MOVE NC"○"  TO 資格平成チェックＷ
016230             END-EVALUATE
016240         END-IF
016250**市町村国保､退職の場合は有効期限を出力
016260**業種別国保(国保組合)､共済､日雇､船員､社保､組合､自衛官は有効期限日を出力する
016270*         IF (( 受－保険種別 = 08 ) OR
016280*            (( 受－保険種別 = 01 ) AND
016290*            (  受－保険者番号(3:1) NOT = "3" )))
016300*             MOVE 受－有効和暦 TO 資格取得和暦Ｗ
016310*             MOVE 受－有効年   TO 資格取得年Ｗ
016320*             MOVE 受－有効月   TO 資格取得月Ｗ
016330*             MOVE 受－有効日   TO 資格取得日Ｗ
016340*         ELSE
016350*             MOVE 受－資格和暦 TO 資格取得和暦Ｗ
016360*             MOVE 受－資格年   TO 資格取得年Ｗ
016370*             MOVE 受－資格月   TO 資格取得月Ｗ
016380*             MOVE 受－資格日   TO 資格取得日Ｗ
016390*         END-IF
016400*         IF (資格取得和暦Ｗ NOT = ZERO) AND
016410*            (資格取得年Ｗ   NOT = ZERO) AND
016420*            (資格取得月Ｗ   NOT = ZERO) AND
016430*            (資格取得日Ｗ   NOT = ZERO)
016440*             EVALUATE 資格取得和暦Ｗ
016450*             WHEN 3
016460*                 MOVE NC"○"  TO 資格昭和チェックＷ
016470*             WHEN 4
016480*                 MOVE NC"○"  TO 資格平成チェックＷ
016490*             END-EVALUATE
016500*         END-IF
016510* 市町村番号 受給者番号
016520         IF 受－助成種別 NOT = ZERO
016530             MOVE 受－費用負担者番号助成 TO 費用負担者番号Ｗ
016540             MOVE 受－受益者番号助成     TO 受益者番号助成Ｗ
016550         END-IF
016560         IF 受－公費種別 = 05
016570             MOVE 受－費用負担者番号     TO 市町村番号Ｗ
016580             MOVE 受－受益者番号老人     TO 受益者番号Ｗ
016590         END-IF
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
016600*保険種別
016610         EVALUATE 受－助成種別
016620         WHEN 50
016630             MOVE NC"○"  TO 生保チェックＷ
016640         WHEN 51
016650*/頭4桁が "4113"東京 "4108"茨城 "4132"島根 の時は、「福」。それ以外は「老」
016660             IF  受－費用負担者番号助成(1:4) = "4113" OR
016670                 受－費用負担者番号助成(1:4) = "4108" OR
016680                 受－費用負担者番号助成(1:4) = "4132"
016690                 MOVE NC"○"    TO 福チェックＷ
016700             ELSE
016710                 MOVE NC"○"    TO 老人チェックＷ
016720             END-IF
016730         WHEN 52
016740             MOVE NC"○"  TO 母子チェックＷ
016750         WHEN 53
016760             MOVE NC"○"  TO 身障チェックＷ
016770         WHEN 54
016780             MOVE NC"○"  TO 原爆チェックＷ
016790         WHEN 55
016800             MOVE NC"○"  TO 乳幼児チェックＷ
016810         END-EVALUATE
016820
016830         IF 受－公費種別 = 05
016840             MOVE NC"○"  TO 老人チェックＷ
016850         ELSE
016860             EVALUATE 受－保険種別
016870             WHEN 01
016880                 MOVE NC"○"  TO 国保チェックＷ
016890             WHEN 02
016900                 MOVE NC"○"  TO 社保チェックＷ
016910             WHEN 03
016920                 MOVE NC"○"  TO 組合チェックＷ
016930             WHEN 04
016940                 MOVE NC"○"  TO 共済チェックＷ
016950             WHEN 06
016960                 MOVE NC"○"  TO 日雇チェックＷ
016970             WHEN 07
016980                 MOVE NC"○"  TO 船員チェックＷ
016990             WHEN 08
017000                 MOVE NC"○"  TO 退職チェックＷ
017010             WHEN 09
017020                 MOVE NC"○"  TO 共済チェックＷ
017030*             WHEN 70
017040*                 MOVE NC"○"  TO 労災チェックＷ
017050             WHEN 80
017060                 MOVE NC"○"  TO 自賠チェックＷ
017070             END-EVALUATE
017080         END-IF
017090     END-READ.
017100*HILO テスト－－－－－－－－－－－
017110*      MOVE NC"○"  TO 被保険者明治Ｗ.
017120*      MOVE NC"○"  TO 被保険者大正Ｗ.
017130*      MOVE NC"○"  TO 被保険者昭和Ｗ.
017140*      MOVE NC"○"  TO 被保険者男チェックＷ.
017150*      MOVE NC"○"  TO 被保険者女チェックＷ.
017160*      MOVE NC"○"  TO 患者男チェックＷ.
017170*      MOVE NC"○"  TO 患者女チェックＷ.
017180*      MOVE NC"○"  TO 明治チェックＷ.
017190*      MOVE NC"○"  TO 大正チェックＷ.
017200*      MOVE NC"○"  TO 昭和チェックＷ.
017210*      MOVE NC"○"  TO 平成チェックＷ.
017220*      MOVE NC"○"  TO 社保チェックＷ    .
017230*      MOVE NC"○"  TO 組合チェックＷ    .
017240*      MOVE NC"○"  TO 共済チェックＷ    .
017250*      MOVE NC"○"  TO 国保チェックＷ    .
017260*      MOVE NC"○"  TO 日雇チェックＷ    .
017270*      MOVE NC"○"  TO 船員チェックＷ    .
017280*      MOVE NC"○"  TO 老人チェックＷ    .
017290*      MOVE NC"○"  TO 福チェックＷ    .
017300*      MOVE NC"○"  TO 身障チェックＷ    .
017310*      MOVE NC"○"  TO 退職チェックＷ    .
017320*      MOVE NC"○"  TO 母子チェックＷ    .
017330*      MOVE NC"○"  TO 乳幼児チェックＷ  .
017340*      MOVE NC"○"  TO 国保組合チェックＷ.
017350*      MOVE NC"○"  TO 労災チェックＷ    .
017360*      MOVE NC"○"  TO 生保チェックＷ    .
017370*      MOVE NC"○"  TO 自賠チェックＷ    .
017380*      MOVE NC"○"  TO 原爆チェックＷ    .
017390*      MOVE NC"○" TO ０割チェックＷ.
017400*      MOVE NC"○" TO １割チェックＷ.
017410*      MOVE NC"○" TO ２割チェックＷ.
017420*      MOVE NC"○" TO ３割チェックＷ.
017430*      MOVE NC"○"  TO 資格昭和チェックＷ.
017440*      MOVE NC"○"  TO 資格平成チェックＷ.
017450*      MOVE NC"○"  TO 被保険者明治チェックＷ.
017460*      MOVE NC"○"  TO 被保険者大正チェックＷ.
017470*      MOVE NC"○"  TO 被保険者昭和チェックＷ.
017480*      MOVE NC"○"  TO 被保険者平成チェックＷ.
017490*-------------------------------------
017500*================================================================*
017510 請求先情報取得 SECTION.
017520*
017530****************************************************
017540* 連結データから保険者マスタより請求先を取得する。 *
017550* ※保－請求先情報区分=1の場合請求先マスタを使用   *
017560* ● 保険者番号...保険者番号Ｗに格納               *
017570* ● 名称........ 請求先名称Ｗに格納               *
017580* ● 郵便番号１.. 請求先郵便番号１Ｗに格納         *
017590* ● 郵便番号２.. 請求先郵便番号２Ｗに格納         *
017600* ● 住所１.......請求先住所１Ｗに格納             *
017610* ● 住所２.......請求先住所２Ｗに格納             *
017620****************************************************
017630     MOVE SPACE          TO 保険者番号Ｗ.
017640     MOVE SPACE          TO 請求先名称Ｗ.
017650     MOVE SPACE          TO 支部名Ｗ.
017660     MOVE SPACE          TO 請求先郵便番号１Ｗ.
017670     MOVE SPACE          TO 請求先郵便番号２Ｗ.
017680     MOVE SPACE          TO 請求先住所１Ｗ.
017690     MOVE SPACE          TO 請求先住所２Ｗ.
017700     MOVE SPACE          TO 印刷保険者呼名Ｗ.
017710     MOVE SPACE          TO 請求先名支部名Ｗ
017720*
017730     MOVE 保険種別ＷＲ   TO 保－保険種別.
017740     MOVE 保険者番号ＷＲ TO 保－保険者番号.
017750     READ 保険者マスタ
017760     INVALID KEY
017770         MOVE SPACE      TO 請求先名称Ｗ
017780         MOVE SPACE      TO 支部名Ｗ
017790     NOT INVALID KEY
017800         MOVE 保－保険者番号  TO 保険者番号Ｗ
017810         MOVE 保－保険者名称  TO 請求先名称Ｗ
017820         MOVE 保－支部部署名  TO 支部名Ｗ
017830         MOVE 保－郵便番号１  TO 請求先郵便番号１Ｗ
017840         MOVE 保－郵便番号２  TO 請求先郵便番号２Ｗ
017850         MOVE 保－住所１      TO 請求先住所１Ｗ
017860         MOVE 保－住所２      TO 請求先住所２Ｗ
017870     END-READ.
017880     EVALUATE 保険種別ＷＲ
017890     WHEN 2
017900     WHEN 6
017910         MOVE "社会保険事務所" TO 印刷保険者呼名Ｗ
017920     WHEN 3
017930         MOVE "健康保険組合"   TO 印刷保険者呼名Ｗ
017940     WHEN OTHER
017950         MOVE SPACE TO 印刷保険者呼名Ｗ
017960     END-EVALUATE.
017970     IF 印刷保険者呼名Ｗ NOT = SPACE
017980         STRING 請求先名称Ｗ DELIMITED BY SPACE
017990                保険者呼名Ｗ DELIMITED BY SPACE
018000           INTO 請求先名称Ｗ
018010         END-STRING
018020     END-IF.
018030*組合と共済は支部名も印字する
018040     EVALUATE 保険種別ＷＲ
018050     WHEN 03
018060     WHEN 04
018070         STRING 請求先名称Ｗ DELIMITED BY SPACE
018080                "  "         DELIMITED BY SIZE
018090                支部名Ｗ     DELIMITED BY SPACE
018100           INTO 請求先名支部名Ｗ
018110         END-STRING
018120     WHEN OTHER
018130         MOVE 請求先名称Ｗ TO 請求先名支部名Ｗ
018140     END-EVALUATE.
018150*================================================================*
018160 負傷データ取得 SECTION.
018170*
018180**************************************************
018190* 連結データから負傷データＦより以下の情報を取得 *
018200* ● 負傷名...部位＋負傷種別にて加工して格納     *
018210* ● 負傷年.......負傷年Ｗ                       *
018220* ● 負傷月.......負傷月Ｗ                       *
018230* ● 負傷日.......負傷日Ｗ                       *
018240* ● 施術終了年...終了年Ｗ                       *
018250* ● 施術終了月...終了月Ｗ                       *
018260* ● 施術終了日...終了日Ｗ                       *
018270**************************************************
018280     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
018290     MOVE 施術年ＷＲ         TO 負－施術年.
018300     MOVE 施術月ＷＲ         TO 負－施術月.
018310     MOVE 患者コードＷＲ     TO 負－患者コード.
018320     READ 負傷データＦ
018330     INVALID KEY
018340         CONTINUE
018350*            /* ありえない */
018360     NOT INVALID KEY
018370         MOVE ZERO                         TO 部位数Ｗ
018380         MOVE 負－部位数                   TO 部位数Ｗ
018390         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018400                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
018410*********************************************
018420* 注）全柔...負傷種別＋部位にて加工して格納 *
018430*********************************************
018440* 負傷種別
018450             MOVE SPACE                     TO 負傷名称Ｗ
018460             MOVE 03                        TO 名－区分コード
018470             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
018480             READ 名称マスタ
018490             INVALID KEY
018500                 MOVE SPACE    TO 負傷名称Ｗ
018510             NOT INVALID KEY
018520*                 MOVE 名－略称 TO 負傷名称Ｗ
018530                 MOVE 名－正式名称 TO 負傷名称Ｗ
018540             END-READ
018550* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
018570             PERFORM 名称埋込処理
018630             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
018640             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
018650             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
018660*             IF 負－負傷年(部位ＣＮＴ) NOT = ZERO 
018670*                MOVE "."                   TO 負傷年月日区切Ｗ(部位ＣＮＴ)
018680*             END-IF
018690*
018700             MOVE 負－開始年(部位ＣＮＴ)   TO 施術開始年Ｗ(部位ＣＮＴ)
018710             MOVE 負－開始月(部位ＣＮＴ)   TO 施術開始月Ｗ(部位ＣＮＴ)
018720             MOVE 負－開始日(部位ＣＮＴ)   TO 施術開始日Ｗ(部位ＣＮＴ)
018730*             IF 負－開始年(部位ＣＮＴ) NOT = ZERO 
018740*                MOVE "."                   TO 施開年月日区切Ｗ(部位ＣＮＴ)
018750*             END-IF
018760*
018770             MOVE 負－終了年(部位ＣＮＴ)   TO 施術終了年Ｗ(部位ＣＮＴ)
018780             MOVE 負－終了月(部位ＣＮＴ)   TO 施術終了月Ｗ(部位ＣＮＴ)
018790             MOVE 負－終了日(部位ＣＮＴ)   TO 施術終了日Ｗ(部位ＣＮＴ)
018800*             IF 負－終了年(部位ＣＮＴ) NOT = ZERO 
018810*                MOVE "."                   TO 施終年月日区切Ｗ(部位ＣＮＴ)
018820*             END-IF
018830*転帰
018840             EVALUATE 負－転帰区分(部位ＣＮＴ)
018850             WHEN 1
018860             WHEN 2
018870                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
018880             WHEN 3
018890                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
018900             WHEN 4
018910                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
018920             END-EVALUATE
018930*             IF 負－転帰区分(部位ＣＮＴ) NOT = 9
018940*                 MOVE 04                        TO 名－区分コード
018950*                 MOVE 負－転帰区分(部位ＣＮＴ)  TO 名－名称コード
018960*                 READ 名称マスタ
018970*                 INVALID KEY
018980*                     MOVE SPACE    TO 転帰Ｗ(部位ＣＮＴ)
018990*                 NOT INVALID KEY
019000*                     MOVE 名－略称 TO 転帰Ｗ(部位ＣＮＴ)
019010*                 END-READ
019020*             END-IF
019030*hilo---------------------------------
019040*         MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
019050*         MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
019060*         MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
019070*-------------------------------------
019080         END-PERFORM
019090     END-READ.
019100*================================================================*
019110 施術記録取得 SECTION.
019120*
019130************************************************************
019140* 連結データから負傷データＦより以下の情報を取得           *
019150* ● 施術開始年月日...該当する部位に対して当月最初の施術日 *
019160************************************************************
019170     MOVE 施術和暦ＷＲ  TO 施記－施術和暦.
019180     MOVE 施術年ＷＲ    TO 施記－施術年.
019190     MOVE 施術月ＷＲ    TO 施記－施術月.
019200     MOVE ZERO            TO 施記－施術日.
019210     MOVE ZERO            TO 施記－患者番号.
019220     MOVE SPACE           TO 施記－枝番.
019230     START 施術記録Ｆ   KEY IS >= 施記－施術和暦年月日
019240                                  施記－患者コード.
019250     IF 状態キー = "00"
019260         MOVE SPACE  TO 終了フラグ２
019270         PERFORM 施術記録Ｆ読込
019280         PERFORM UNTIL ( 終了フラグ２       = "YES" ) OR
019290                       ( 施記－施術和暦 NOT = 施術和暦ＷＲ ) OR
019300                       ( 施記－施術年   NOT = 施術年ＷＲ   ) OR
019310                       ( 施記－施術月   NOT = 施術月ＷＲ   )
019320*            **************
019330*            * 開始年月日 *
019340*            **************
019350             PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
019360                     UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
019370            EVALUATE TRUE ALSO TRUE ALSO TRUE ALSO TRUE ALSO TRUE
019380            WHEN 施記－負傷種別(部位ＣＮＴ) = 負傷種別Ｗ(部位ＣＮＴ) ALSO
019390                 施記－部位(部位ＣＮＴ)     = 部位Ｗ(部位ＣＮＴ)     ALSO
019400                 施記－左右区分(部位ＣＮＴ) = 左右区分Ｗ(部位ＣＮＴ) ALSO
019410                 施記－負傷位置番号(部位ＣＮＴ)
019420                                       = 負傷位置番号Ｗ(部位ＣＮＴ)  ALSO
019430                 開始年月日取得フラグ(部位ＣＮＴ) = SPACE
019440                   MOVE 施記－施術年     TO 施術開始年Ｗ(部位ＣＮＴ)
019450                   MOVE 施記－施術月     TO 施術開始月Ｗ(部位ＣＮＴ)
019460                   MOVE 施記－施術日     TO 施術開始日Ｗ(部位ＣＮＴ)
019470                   MOVE "YES"       TO 開始年月日取得フラグ(部位ＣＮＴ)
019480*
019490            WHEN OTHER
019500                CONTINUE
019510            END-EVALUATE
019520             END-PERFORM
019530             PERFORM 施術記録Ｆ読込
019540         END-PERFORM
019550     END-IF.
019560*================================================================*
019570 施術記録Ｆ読込 SECTION.
019580*
019590     READ 施術記録Ｆ NEXT
019600     AT END
019610         MOVE "YES" TO 終了フラグ２
019620     END-READ.
019630*
019640*================================================================*
019650 負傷原因取得 SECTION.
019660*
019670********************************************************************
019680*  負傷原因コードが同じものは、1行にまとめて印字する。
019690*  例: ①② 家で転んだ.
019700*     負傷原因コードが同じものをまとめ、テーブルにセット
019710*     (ただし、部位を飛んで同じものは、2行になる)
019720********************************************************************
019730     MOVE  ZERO   TO  カウンタ カウンタ２.
019740     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
019750             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
019760*
019770****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
019780        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
019790*
019800           IF カウンタ = ZERO
019810               MOVE 1   TO  カウンタ カウンタ２
019820               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
019830               MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
019840               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
019850           ELSE
019860              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
019870                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
019880                 COMPUTE カウンタ２ = カウンタ２  +  1
019890                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
019900              ELSE
019910                 COMPUTE カウンタ = カウンタ  +  1
019920                 MOVE 1   TO  カウンタ２
019930                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
019940                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
019950                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
019960              END-IF
019970           END-IF
019980        END-IF
019990     END-PERFORM.
020000**************************************************************************
020010*  負傷原因マスタより文章取得
020020**************************************************************************
020030     MOVE  ZERO   TO  カウンタ カウンタ２.
020040     PERFORM VARYING カウンタ FROM 1 BY 1
020050             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
020060** 労災は 区分 02
020070         MOVE 02                        TO 負原－区分コード
020080         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
020090         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
020100         READ 負傷原因Ｆ
020110         NOT INVALID KEY
020120             INITIALIZE 負傷原因ＷＴ
020130             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
020140             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
020150             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
020160             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
020170             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
020180             PERFORM VARYING カウンタ２ FROM 1 BY 1
020190                     UNTIL ( カウンタ２ > 9 )  OR 
020200                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
020210                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
020220                WHEN 1
020230                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
020240                WHEN 2
020250                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
020260                WHEN 3
020270                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
020280                WHEN 4
020290                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
020300                WHEN 5
020310                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
020320                WHEN OTHER
020330                   CONTINUE
020340                END-EVALUATE
020350             END-PERFORM
020360*
020442             IF 負原－負傷原因入力区分 = 1
020443                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
020444                        負傷原因１ＷＴ  DELIMITED BY SIZE
020445                        負傷原因２ＷＴ  DELIMITED BY SIZE
020446                        負傷原因３ＷＴ  DELIMITED BY SIZE
020447                        負傷原因４ＷＴ  DELIMITED BY SIZE
020448                        負傷原因５ＷＴ  DELIMITED BY SIZE
020449                        INTO 負傷原因内容合成Ｗ(カウンタ)
020450                 END-STRING
020451             ELSE
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
020460             END-IF
020461*
020462         END-READ
020463     END-PERFORM.
020470*
020480     PERFORM 負傷原因セット.
020490*
020500*================================================================*
020510 負傷原因セット SECTION.
020520*
020530**************************************************************************
020540*  文章が1行を超える時は、複数行に分解する。
020550**************************************************************************
020560     MOVE  ZERO   TO  カウンタ カウンタ２.
020570     PERFORM VARYING カウンタ FROM 1 BY 1
020580             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
020590*
020600          INITIALIZE 負傷原因内容分解ＸＷ
020610          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
020620          IF  負傷原因内容１ＸＷ  NOT = SPACE
020630              COMPUTE カウンタ２ = カウンタ２  +  1
020640              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
020650          END-IF
020660          IF  負傷原因内容２ＸＷ  NOT = SPACE
020670              COMPUTE カウンタ２ = カウンタ２  +  1
020680              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
020690          END-IF
020700*          IF  負傷原因内容３ＸＷ  NOT = SPACE
020710*              COMPUTE カウンタ２ = カウンタ２  +  1
020720*              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
020730*          END-IF
020820*
020830     END-PERFORM.
020840*
020850*================================================================*
020860 印刷処理 SECTION.
020870*
020880     EVALUATE 印刷モードＦＷＲ
020890     WHEN 0
020900         PERFORM 印刷処理２
020910         PERFORM 印刷処理３
020920     WHEN 1
020930         PERFORM 印刷処理２
020940     WHEN 2
020950         PERFORM 印刷処理３
020980     END-EVALUATE.
021080*================================================================*
021090 印刷処理２ SECTION.
021100*
021110     MOVE "YCH6622P"  TO  定義体名Ｐ.
021120     MOVE "GRP001"   TO  項目群名Ｐ.
021130     WRITE YCH6622P.
021140*     WRITE 印刷レコード.
021150     PERFORM エラー処理Ｐ.
021160*================================================================*
021170 印刷処理３ SECTION.
021180*
021190     MOVE "YCH6622P"  TO  定義体名Ｐ.
021200     MOVE "GRP002"   TO  項目群名Ｐ.
021210     WRITE YCH6622P.
021220*     WRITE 印刷レコード.
021230     PERFORM エラー処理Ｐ.
021310*================================================================*
021320 エラー処理Ｐ SECTION.
021330*
021340     IF 通知情報Ｐ NOT = "00"
021350         DISPLAY NC"帳票エラー"              UPON CONS
021360         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
021370         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
021380         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
021390         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
021400                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
021410         ACCEPT  キー入力 FROM CONS
021420         PERFORM ファイル閉鎖
021430         EXIT PROGRAM
021440     END-IF.
021450*================================================================*
021460 名称埋込処理 SECTION.
021470*
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
021580     END-READ.
021590*
022010*================================================================*
022020  請求期間取得 SECTION.
022030*
022040     EVALUATE 施術月ＷＲ
022050     WHEN 4
022060     WHEN 6
022070     WHEN 9
022080     WHEN 11
022090         MOVE 30 TO 請求終了日Ｗ
022100     WHEN 2
022110         COMPUTE 対象西暦Ｗ = 和暦終了年Ｗ + 施術年ＷＲ
022120         DIVIDE 4 INTO 対象西暦Ｗ GIVING    商Ｗ
022130                                  REMAINDER 余Ｗ
022140         END-DIVIDE
022150         IF 余Ｗ = ZERO
022160             MOVE 29 TO 請求終了日Ｗ
022170         ELSE
022180             MOVE 28 TO 請求終了日Ｗ
022190         END-IF
022200     WHEN 1
022210     WHEN 3
022220     WHEN 5
022230     WHEN 7
022240     WHEN 8
022250     WHEN 10
022260     WHEN 12
022270         MOVE 31 TO 請求終了日Ｗ
022280     WHEN OTHER
022290         MOVE  NC"未知のエラー発生" TO 連メ－メッセージ
022300         CALL   "MSG001"
022310         CANCEL "MSG001"
022320     END-EVALUATE.
022330*================================================================*
022340******************************************************************
022350 END PROGRAM YCH6622.
022360******************************************************************
