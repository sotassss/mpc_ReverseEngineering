000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YJB6621.
000060 AUTHOR.                 山田　浩之
000070*
000080*----------------------------------------------------------------*
000090*         ＪＢ用
000100*         労災 カルテ印刷（柔ｳｨﾝﾄﾞｳｽﾞ95版）再生紙
000110*         MED = YJB660G YJB6621P
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2000-09-26
000140 DATE-COMPILED.          2000-09-26
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
001950     COPY YJB6621P        OF  XMDLIB.
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
002110 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002120 01 部位名称Ｗ                         PIC N(10) VALUE SPACE.
002130 01 負傷番号Ｗ                         PIC 9     VALUE ZERO.
002140 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
002150    03 負傷番号Ｗ１                    PIC X.
002160*
002170 01 全角負傷番号Ｗ                     PIC N     VALUE SPACE.
002180 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
002190    03 全角負傷番号Ｗ１                PIC X(2).
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
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
002610       05 負傷原因内容１ＸＷ           PIC X(50)  VALUE SPACE.
002620       05 負傷原因内容２ＸＷ           PIC X(50)  VALUE SPACE.
002630       05 負傷原因内容３ＸＷ           PIC X(50)  VALUE SPACE.
002640       05 負傷原因内容４ＸＷ           PIC X(50)  VALUE SPACE.
002630       05 負傷原因内容５ＸＷ           PIC X(50)  VALUE SPACE.
002640       05 負傷原因内容６ＸＷ           PIC X(50)  VALUE SPACE.
002650       05 負傷原因内容７ＸＷ           PIC X(16)  VALUE SPACE.
002660*
002670** 負傷原因印刷区分用
002680 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
002690*
002700* 社保用
002710 01 接尾語区分Ｗ                       PIC 9     VALUE ZERO.
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
003530          07 被保険者住所１Ｗ          PIC X(40) VALUE SPACE.
003540          07 被保険者住所２Ｗ          PIC X(40) VALUE SPACE.
003550       05 被保険者電話番号Ｗ           PIC X(15) VALUE SPACE.
003560       05 分割電話番号Ｗ.
003570          07 分割電話番号１Ｗ          PIC X(7)  VALUE SPACE.
003580          07 分割電話番号２Ｗ          PIC X(8)  VALUE SPACE.
003590          07 分割電話番号３Ｗ          PIC X(8)  VALUE SPACE.
003600*          07 市外局番Ｗ                PIC X(5)  VALUE SPACE.
003610*          07 分割局番Ｗ                PIC X(5)  VALUE SPACE.
003620*          07 分割番号Ｗ                PIC X(5)  VALUE SPACE.
003630    03 資格取得年月日Ｗ.
003640       05 資格取得元号Ｗ               PIC N(2)  VALUE SPACE.
003650       05 資格取得和暦Ｗ               PIC 9(1)  VALUE ZERO.
003660       05 資格取得年Ｗ                 PIC 9(2)  VALUE ZERO.
003670       05 資格取得月Ｗ                 PIC 9(2)  VALUE ZERO.
003680       05 資格取得日Ｗ                 PIC 9(2)  VALUE ZERO.
003690    03 有効期限年月日Ｗ.
003700       05 有効期限元号Ｗ               PIC N(2)  VALUE SPACE.
003710       05 有効期限和暦Ｗ               PIC 9(1)  VALUE ZERO.
003720       05 有効期限年Ｗ                 PIC 9(2)  VALUE ZERO.
003730       05 有効期限月Ｗ                 PIC 9(2)  VALUE ZERO.
003740       05 有効期限日Ｗ                 PIC 9(2)  VALUE ZERO.
003750       05 資格和暦チェックＷ.
003760          07 資格昭和チェックＷ        PIC N(1)  VALUE SPACE.
003770          07 資格平成チェックＷ        PIC N(1)  VALUE SPACE.
003780    03 患者情報Ｗ.
003790       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
003800       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
003810       05 患者性別チェックＷ.
003820          07 患者男チェックＷ          PIC N(1)  VALUE SPACE.
003830          07 患者女チェックＷ          PIC N(1)  VALUE SPACE.
003840       05 続柄Ｗ.
003850          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
003860          07 FILLER                    PIC X(4).
003870       05 和暦チェックＷ.
003880          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
003890          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
003900          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
003910          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
003920       05 患者年Ｗ                     PIC 9(2) VALUE ZERO.
003930       05 患者月Ｗ                     PIC 9(2) VALUE ZERO.
003940       05 患者日Ｗ                     PIC 9(2) VALUE ZERO.
003950       05 患者年齢Ｗ                   PIC 9(3) VALUE ZERO.
003960       05 患者性別Ｗ                   PIC N(4) VALUE SPACE.
003970    03 保険種別チェックＷ.
003980       05 社保チェックＷ               PIC N(1) VALUE SPACE.
003990       05 組合チェックＷ               PIC N(1) VALUE SPACE.
004000       05 共済チェックＷ               PIC N(1) VALUE SPACE.
004010       05 国保チェックＷ               PIC N(1) VALUE SPACE.
004020       05 日雇チェックＷ               PIC N(1) VALUE SPACE.
004030       05 船員チェックＷ               PIC N(1) VALUE SPACE.
004040       05 老人チェックＷ               PIC N(1) VALUE SPACE.
004050       05 福チェックＷ                 PIC N(1) VALUE SPACE.
004060       05 身障チェックＷ               PIC N(1) VALUE SPACE.
004070       05 退職チェックＷ               PIC N(1) VALUE SPACE.
004080       05 母子チェックＷ               PIC N(1) VALUE SPACE.
004090       05 乳幼児チェックＷ             PIC N(1) VALUE SPACE.
004100       05 国保組合チェックＷ           PIC N(1) VALUE SPACE.
004110       05 労災チェックＷ               PIC N(1) VALUE SPACE.
004120       05 自賠チェックＷ               PIC N(1) VALUE SPACE.
004130       05 生保チェックＷ               PIC N(1) VALUE SPACE.
004140       05 原爆チェックＷ               PIC N(1) VALUE SPACE.
004150       05 自衛チェックＷ               PIC N(1) VALUE SPACE.
004160       05 公費チェックＷ               PIC N(1) VALUE SPACE.
004170       05 自費チェックＷ               PIC N(1) VALUE SPACE.
004180       05 患者郵便番号Ｗ.
004190          07 患者郵便番号１Ｗ          PIC X(3)  VALUE SPACE.
004200          07 患者郵便区切Ｗ            PIC X(1)  VALUE SPACE.
004210          07 患者郵便番号２Ｗ          PIC X(4)  VALUE SPACE.
004220       05 患者住所Ｗ.
004230          07 患者住所１Ｗ              PIC X(40) VALUE SPACE.
004240          07 患者住所２Ｗ              PIC X(40) VALUE SPACE.
004250       05 患者電話番号Ｗ               PIC X(15) VALUE SPACE.
004260    03 負傷原因Ｗ                      PIC X(50) OCCURS 63 VALUE SPACE.
004270**************
004280* 事業所情報 *
004290**************
004300 01 事業所情報Ｗ.
004310    03 事業所名称Ｗ.
004320       05 印刷事業所名称Ｗ             PIC X(60)  VALUE SPACE.
004330    03 事業所郵便番号Ｗ.
004340       05 事業所郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
004350       05 事業所郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
004360    03 事業所住所Ｗ.
004370       05 事業所住所１Ｗ               PIC X(50)  VALUE SPACE.
004380       05 事業所住所２Ｗ               PIC X(50)  VALUE SPACE.
004390*    03 事業所住所Ｗ.
004400*       05 印刷事業所住所Ｗ             PIC X(70)  VALUE SPACE.
004410*       05 FILLER                       PIC X(10).
004420**************
004430* 請求先情報 *
004440**************
004450 01 請求先情報Ｗ.
004460    03 保険者番号Ｗ.
004470       05 印刷保険者番号Ｗ             PIC X(8)  VALUE SPACE.
004480       05 FILLER                       PIC X(2).
004490    03 請求先名称Ｗ.
004500       05 印刷請求先名称Ｗ             PIC X(40) VALUE SPACE.
004510    03 支部名Ｗ.
004520       05 印刷支部名Ｗ                 PIC X(40) VALUE SPACE.
004530    03 請求先名支部名Ｗ.
004540       05 印刷請求先名支部名１Ｗ       PIC X(40) VALUE SPACE.
004550       05 印刷請求先名支部名２Ｗ       PIC X(40) VALUE SPACE.
004560    03 保険者呼名Ｗ.
004570*       05 印刷保険者呼名Ｗ             PIC N(7)  VALUE SPACE.
004580       05 印刷保険者呼名Ｗ             PIC X(14)  VALUE SPACE.
004590    03 請求先郵便番号Ｗ.
004600       05 請求先郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
004610       05 請求先郵便番号区切Ｗ         PIC X(1)  VALUE SPACE.
004620       05 請求先郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
004630    03 請求先住所１Ｗ.
004640       05 印刷請求先住所１Ｗ           PIC X(40) VALUE SPACE.
004650    03 請求先住所２Ｗ.
004660       05 印刷請求先住所２Ｗ           PIC X(40) VALUE SPACE.
004670*       05 FILLER                       PIC X(5).
004680    03 保険者住所Ｗ.
004690       05 保険者住所１Ｗ               PIC X(40) VALUE SPACE.
004700       05 保険者住所２Ｗ               PIC X(40) VALUE SPACE.
004710****************
004720* 負傷データＦ *
004730****************
004740 01 負傷情報Ｗ.
004750    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
004760    03 負傷データ情報Ｗ  OCCURS   9.
004770       05 会部位コードＷ               PIC X(4)  VALUE SPACE.
004780       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
004790       05 負傷種別Ｗ                   PIC 9(2)  VALUE ZERO.
004800       05 部位Ｗ                       PIC 9(2)  VALUE ZERO.
004810       05 左右区分Ｗ                   PIC 9(1)  VALUE ZERO.
004820       05 負傷位置番号Ｗ               PIC 9(2)  VALUE ZERO.
004830       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
004840       05 負傷年月日Ｗ.
004850          07 負傷和暦Ｗ                PIC 9(1)  VALUE ZERO.
004860          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
004870          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
004880          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
004890          07 負傷年月日区切Ｗ          PIC X(1)  VALUE SPACE.
004900       05 初検年月日Ｗ.
004910          07 初検和暦Ｗ                PIC 9(1)  VALUE ZERO.
004920          07 初検年Ｗ                  PIC 9(2)  VALUE ZERO.
004930          07 初検月Ｗ                  PIC 9(2)  VALUE ZERO.
004940          07 初検日Ｗ                  PIC 9(2)  VALUE ZERO.
004950          07 初検年月日区切Ｗ          PIC X(1)  VALUE SPACE.
004960       05 施術開始年月日Ｗ.
004970          07 施術開始和暦Ｗ            PIC 9(1)  VALUE ZERO.
004980          07 施術開始年Ｗ              PIC 9(2)  VALUE ZERO.
004990          07 施術開始月Ｗ              PIC 9(2)  VALUE ZERO.
005000          07 施術開始日Ｗ              PIC 9(2)  VALUE ZERO.
005010          07 施開年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005020       05 施術終了年月日Ｗ.
005030          07 施術終了和暦Ｗ            PIC 9(1)  VALUE ZERO.
005040          07 施術終了年Ｗ              PIC 9(2)  VALUE ZERO.
005050          07 施術終了月Ｗ              PIC 9(2)  VALUE ZERO.
005060          07 施術終了日Ｗ              PIC 9(2)  VALUE ZERO.
005070          07 施終年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005080       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
005090       05 転帰Ｗ                       PIC N(4)  VALUE SPACE.
005100       05 転帰チェックＷ.
005110          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
005120          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
005130          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
005140          07 継続チェックＷ            PIC N(1)  VALUE SPACE.
005150*
005160******************
005170* 負担率チェック *
005180******************
005190 01 負担率チェックＷ.
005200    03 ０割チェックＷ                  PIC N(1)  VALUE SPACE.
005210    03 １割チェックＷ                  PIC N(1)  VALUE SPACE.
005220    03 ２割チェックＷ                  PIC N(1)  VALUE SPACE.
005230    03 ３割チェックＷ                  PIC N(1)  VALUE SPACE.
005240**********
005250* 合計行 *
005260**********
005270 01 合計行Ｗ.
005280    03 請求終了日Ｗ                    PIC 9(2) VALUE ZERO.
005290    03 対象西暦Ｗ                      PIC 9(4) VALUE ZERO.
005300    03 商Ｗ                            PIC 9(3) VALUE ZERO.
005310    03 余Ｗ                            PIC 9(3) VALUE ZERO.
005320*/年月毎持つ保険者データのワーク
005330 01 保負担率区分Ｗ                     PIC 9    VALUE ZERO.
005340 01 保本人負担率Ｗ                     PIC 9(3) VALUE ZERO.
005350 01 保家族負担率Ｗ                     PIC 9(3) VALUE ZERO.
005360 01 保本人負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
005370 01 保家族負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
005380*
005390 01 比較和暦年月Ｗ.
005400    03 比較和暦Ｗ                      PIC 9    VALUE ZERO.
005410    03 比較年Ｗ                        PIC 9(2) VALUE ZERO.
005420    03 比較月Ｗ                        PIC 9(2) VALUE ZERO.
005430*
005440 01 印刷制御.
005450     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
005460     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
005470     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
005480     03 拡張制御Ｐ.
005490         05 端末制御Ｐ.
005500             07 移動方向Ｐ             PIC X(1)  VALUE SPACE.
005510             07 移動行数Ｐ             PIC 9(3)  VALUE ZERO.
005520         05 詳細制御Ｐ                 PIC X(2)  VALUE SPACE.
005530     03 通知情報Ｐ                     PIC X(2)  VALUE SPACE.
005540     03 ユニット名Ｐ                   PIC X(8)  VALUE SPACE.
005550*
005560 01 計算機西暦年Ｗ                     PIC 9(2)  VALUE ZERO.
005570* 日付ＷＯＲＫ
005580 01 和暦終了年Ｗ                       PIC 9(4)  VALUE ZERO.
005590 01 計算機西暦.
005600    03 計算機西暦年                    PIC 9(4)  VALUE ZERO.
005610    03 計算機西暦月日                  PIC 9(4)  VALUE ZERO.
005620 01 計算機西暦Ｒ REDEFINES 計算機西暦.
005630    03 計算機世紀                      PIC 9(2).
005640    03 計算機日付                      PIC 9(6).
005650    03 計算機日付Ｒ REDEFINES 計算機日付.
005660       05 計算機年月                   PIC 9(4).
005670       05 計算機年月Ｒ REDEFINES 計算機年月.
005680         07 計算機年                   PIC 9(2).
005690         07 計算機月                   PIC 9(2).
005700       05 計算機日                     PIC 9(2).
005710*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
005720******************************************************************
005730*                          連結項目                              *
005740******************************************************************
005750*
005760**********************
005770* メッセージ表示キー *
005780**********************
005790 01 連メ－キー IS EXTERNAL.
005800    03  連メ－メッセージ                 PIC N(20).
005810*
005820****************
005830* 連結項目待避 *
005840****************
005850*    ************
005860*    * 料金情報 *
005870*    ************
005880*    月毎の料金
005890***********************
005900 01 連施－料金１ IS EXTERNAL.
005910   03 連施－本人負担率                 PIC 9(3).
005920************
005930* 印刷キー *
005940************
005950 01 連印－対象データ IS EXTERNAL.
005960    03 連印－施術和暦年月.
005970       05 連印－施術和暦                  PIC 9(1).
005980       05 連印－施術年                    PIC 9(2).
005990       05 連印－施術月                    PIC 9(2).
006000    03 連印－保険種別                     PIC 9(2).
006010    03 連印－保険者番号                   PIC X(10).
006020    03 連印－本人家族区分                 PIC 9(1).
006030    03 連印－被保険者カナ                 PIC X(20).
006040    03 連印－患者コード.
006050       05 連印－患者番号                  PIC 9(6).
006060       05 連印－枝番                      PIC X(1).
006070    03 連印－印刷モードＦ                 PIC 9(1).
006080*/詳細
006090    03 連印－保険証印刷区分               PIC 9(1).
006100    03 連印－負傷詳細 OCCURS 7.
006110       05 連印－負傷印刷行                PIC 9(1).
006120       05 連印－部位印刷区分              PIC 9(1).
006130       05 連印－転帰印刷区分              PIC 9(1).
006140       05 連印－原因印刷区分              PIC 9(1).
007291*
       01 連入－表示フラグ６６０ IS EXTERNAL GLOBAL.
          03 連入－プレビュー区分               PIC 9(1).
007292* 負担率取得用14/10～
007293 01 連率－負担率取得キー IS EXTERNAL.
007294    03 連率－施術和暦年月.
007295       05 連率－施術和暦               PIC 9.
007296       05 連率－施術年月.
007297          07 連率－施術年              PIC 9(2).
007298          07 連率－施術月              PIC 9(2).
007299    03 連率－患者コード.
007300       05 連率－患者番号               PIC 9(6).
007301       05 連率－枝番                   PIC X.
007302    03 連率－実際負担率                PIC 9(3).
007303    03 連率－実際本体負担率            PIC 9(3).
007304    03 連率－健保負担率                PIC 9(3).
007305    03 連率－２７老負担率              PIC 9(3).
007306    03 連率－助成負担率                PIC 9(3).
007307    03 連率－特別用負担率              PIC 9(3).
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
006470*
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
006480******************************************************************
006490*                      PROCEDURE  DIVISION                       *
006500******************************************************************
006510 PROCEDURE               DIVISION.
006520************
006530*           *
006540* 初期処理   *
006550*           *
006560************
002570     PERFORM プリンタファイル作成.
006570     PERFORM 初期化.
006580************
006590*           *
006600* 主処理     *
006610*           *
006620************
006630* 印刷
006640     PERFORM 連結項目待避.
006650     PERFORM 印刷セット
006660     PERFORM 印刷処理
006670************
006680*           *
006690* 終了処理   *
006700*           *
006710************
006720     PERFORM 終了処理.
006730     EXIT PROGRAM.
006740*
006750*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YJB6621"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
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
006760*================================================================*
006770 初期化 SECTION.
006780*
006790     PERFORM ファイルオープン.
006800*    /* 現在日付取得 */
006810     ACCEPT 計算機日付 FROM DATE.
006820*    /* 1980～2079年の間で設定 */
006830     IF 計算機年 > 80
006840         MOVE 19 TO 計算機世紀
006850     ELSE
006860         MOVE 20 TO 計算機世紀
006870     END-IF.
006880     PERFORM カレント元号取得.
006890     PERFORM 和暦終了年取得.
006900     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
006910*================================================================*
006920 カレント元号取得 SECTION.
006930*
006940     MOVE ZEROS TO 制－制御区分.
006950     READ 制御情報マスタ
006960     NOT INVALID KEY
006970         MOVE 制－カレント元号 TO カレント元号Ｗ
006980         MOVE 制－カルテ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
006990     END-READ.
007000*
007010*================================================================*
007020 和暦終了年取得 SECTION.
007030*
007040     MOVE カレント元号Ｗ TO 元－元号区分.
007050     READ 元号マスタ
007060     INVALID KEY
007070         DISPLAY NC"指定和暦が登録されていません" UPON CONS
007080         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
007090                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007100         ACCEPT  キー入力 FROM CONS
007110         PERFORM 終了処理
007120         EXIT PROGRAM
007130     NOT INVALID KEY
007140         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
007150         MOVE 前和暦Ｗ TO 元－元号区分
007160         READ 元号マスタ
007170         INVALID KEY
007180             DISPLAY NC"指定和暦が登録されていません" UPON CONS
007190             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
007200                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007210             ACCEPT  キー入力 FROM CONS
007220             PERFORM 終了処理
007230             EXIT PROGRAM
007240         NOT INVALID KEY
007250             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
007260         END-READ
007270     END-READ.
007280*
007290*================================================================*
007300 連結項目待避 SECTION.
007310*
007320     INITIALIZE 対象データＷＲ.
007330     MOVE 連印－施術和暦      TO 施術和暦ＷＲ.
007340     MOVE 連印－施術年        TO 施術年ＷＲ.
007350     MOVE 連印－施術月        TO 施術月ＷＲ.
007360     MOVE 連印－保険種別      TO 保険種別ＷＲ.
007370     MOVE 連印－保険者番号    TO 保険者番号ＷＲ.
007380     MOVE 連印－本人家族区分  TO 本人家族区分ＷＲ.
007390     MOVE 連印－被保険者カナ  TO 被保険者カナＷＲ.
007400     MOVE 連印－患者番号      TO 患者番号ＷＲ.
007410     MOVE 連印－枝番          TO 枝番ＷＲ.
007420     MOVE 連印－印刷モードＦ  TO 印刷モードＦＷＲ.
007430*================================================================*
007440 ファイルオープン SECTION.
007450*
007460     OPEN INPUT   保険者マスタ
007470         MOVE NC"保険者" TO ファイル名.
007480         PERFORM オープンチェック.
007490     OPEN INPUT   元号マスタ
007500         MOVE NC"元号" TO ファイル名.
007510         PERFORM オープンチェック.
007520     OPEN INPUT   名称マスタ
007530         MOVE NC"名称" TO ファイル名.
007540         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
007580     OPEN INPUT   制御情報マスタ
007590         MOVE NC"制御情報" TO ファイル名.
007600         PERFORM オープンチェック.
007640     OPEN INPUT   請求先マスタ
007650         MOVE NC"請先" TO ファイル名.
007660         PERFORM オープンチェック.
007700     OPEN INPUT   受診者情報Ｆ.
007710         MOVE NC"受情" TO ファイル名.
007720         PERFORM オープンチェック.
007730     OPEN INPUT   施術記録Ｆ.
007740         MOVE NC"施記Ｆ" TO ファイル名.
007750         PERFORM オープンチェック.
007760     OPEN INPUT   負傷データＦ.
007770         MOVE NC"負傷" TO ファイル名.
007780         PERFORM オープンチェック.
007820     OPEN INPUT   負傷原因Ｆ.
007830         MOVE NC"負傷原因" TO ファイル名.
007840         PERFORM オープンチェック.
007900     OPEN INPUT 事業所マスタ.
007910         MOVE NC"事業所" TO ファイル名.
007920         PERFORM オープンチェック.
007900     OPEN INPUT 労災情報Ｆ.
007910         MOVE NC"労災情報Ｆ" TO ファイル名.
007920         PERFORM オープンチェック.
007850     OPEN I-O   印刷ファイル
007860         MOVE NC"印刷" TO ファイル名.
007870         PERFORM エラー処理Ｐ.
007880*================================================================*
007890 オープンチェック SECTION.
007900*
007910     IF 状態キー  NOT =  "00"
007920         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
007930         DISPLAY NC"状態キー：" 状態キー         UPON CONS
007940         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
007950                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007960         ACCEPT  キー入力 FROM CONS
007970         PERFORM ファイル閉鎖
007980         EXIT PROGRAM.
007990*================================================================*
008000 ファイル閉鎖 SECTION.
008010*
008020     CLOSE 印刷ファイル   保険者マスタ 元号マスタ
008030           制御情報マスタ 名称マスタ   請求先マスタ
008040           レセプトＦ     受診者情報Ｆ 施術記録Ｆ
008050           負傷データＦ   事業所マスタ 負傷原因Ｆ
                 労災情報Ｆ.
008070*================================================================*
008080 終了処理 SECTION.
008090*
008100     PERFORM ファイル閉鎖.
008110*================================================================*
008120 印刷セット SECTION.
008130*
008140     EVALUATE 印刷モードＦＷＲ
008150     WHEN 0
008160         PERFORM 印刷セット１
008170     WHEN 1
008180         PERFORM 印刷セット２
008190     WHEN 2
008200         PERFORM 印刷セット３
008230     END-EVALUATE.
008240*
008250*================================================================*
008260 印刷セット１ SECTION.
008270*
008280     INITIALIZE YJB6621P.
008290     INITIALIZE 受診者情報Ｗ.
008300     INITIALIZE 事業所情報Ｗ.
008310     INITIALIZE 請求先情報Ｗ.
008320     INITIALIZE 負傷情報Ｗ.
008330     INITIALIZE 負担率チェックＷ.
008340     PERFORM 負担率取得.
008350     PERFORM 請求先情報取得.
008360     PERFORM 受診者情報取得.
008370     PERFORM 負傷データ取得.
008380*
008390     IF 負傷原因印刷区分Ｗ  NOT = 1 
008400        PERFORM 負傷原因取得
008410     END-IF.
008420*
008430     PERFORM 印刷上部セット.
008440     PERFORM 印刷下部セット.
008450*================================================================*
008460 印刷セット２ SECTION.
008470*
008480     INITIALIZE YJB6621P.
008490     INITIALIZE 受診者情報Ｗ.
008500     INITIALIZE 事業所情報Ｗ.
008510     INITIALIZE 請求先情報Ｗ.
008520     INITIALIZE 負傷情報Ｗ.
008530     INITIALIZE 負担率チェックＷ.
008540     PERFORM 負担率取得.
008550     PERFORM 請求先情報取得.
008560     PERFORM 受診者情報取得.
008570     PERFORM 印刷上部セット.
008580*================================================================*
008590 印刷セット３ SECTION.
008600*
008610     INITIALIZE YJB6621P.
008620     INITIALIZE 受診者情報Ｗ.
008630     INITIALIZE 事業所情報Ｗ.
008640     INITIALIZE 請求先情報Ｗ.
008650     INITIALIZE 負傷情報Ｗ.
008560     PERFORM 受診者情報取得.
008660     PERFORM 負傷データ取得.
008670*
008680     IF 負傷原因印刷区分Ｗ  NOT = 1 
008690        PERFORM 負傷原因取得
008700     END-IF.
008710*
008720     PERFORM 印刷下部セット.
008870*================================================================*
008880 印刷上部セット SECTION.
008890*
008900     MOVE 患者番号Ｗ                 TO 患者番号.
008910     MOVE 枝番Ｗ                     TO 枝番.
008920**************************
008930* 被保険者証情報セット   *
008940**************************
008950*     IF 記号Ｗ(1:2) = "＊" 
008960*        MOVE  SPACE    TO  記号
008970*     ELSE
008980*        MOVE 記号Ｗ    TO  記号
008990*     END-IF.
009000*     IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
009010*        ( 印刷番号Ｗ(1:2) = "＊" )
009020*        MOVE  SPACE      TO  番号
009030*     ELSE
009040*        MOVE 印刷番号Ｗ  TO  番号
009050*     END-IF.
009060************************
009070* 被保険者情報セット   *
009080************************
009090*     MOVE 被保険者氏名Ｗ             TO 被保険者氏名.
009100**     MOVE 被保険者カナＷ             TO 被保険者カナ.
009110**     MOVE 資格取得元号Ｗ             TO 資格取得元号.
009120*     MOVE 資格取得年Ｗ               TO 資格取得年.
009130*     MOVE 資格取得月Ｗ               TO 資格取得月.
009140*     MOVE 資格取得日Ｗ               TO 資格取得日.
009150*     MOVE 資格昭和チェックＷ         TO 資格昭和チェック.
009160*     MOVE 資格平成チェックＷ         TO 資格平成チェック.
009170*/有効期限
009180*     MOVE 有効期限年Ｗ               TO 有効年.
009190*     MOVE 有効期限月Ｗ               TO 有効月.
009200*     MOVE 有効期限日Ｗ               TO 有効日.
009210**     IF ( 有効期限年Ｗ = ZERO ) AND
009220**        ( 有効期限月Ｗ = ZERO ) AND
009230**        ( 有効期限日Ｗ = ZERO )
009240**         MOVE SPACE          TO 有効元号
009250**         MOVE SPACE          TO 有効年月日表示
009260**         MOVE SPACE          TO 有効年表示
009270**         MOVE SPACE          TO 有効月表示
009280**         MOVE SPACE          TO 有効日表示
009290**     ELSE
009300**         MOVE 有効期限元号Ｗ TO 有効元号
009310**         MOVE NC"有効年月日" TO 有効年月日表示
009320**         MOVE NC"年"         TO 有効年表示
009330**         MOVE NC"月"         TO 有効月表示
009340**         MOVE NC"日"         TO 有効日表示
009350**     END-IF.
009360*     MOVE 被保険者明治チェックＷ     TO 被保険者明治チェック.
009370*     MOVE 被保険者大正チェックＷ     TO 被保険者大正チェック.
009380*     MOVE 被保険者昭和チェックＷ     TO 被保険者昭和チェック.
009390*     MOVE 被保険者平成チェックＷ     TO 被保険者平成チェック.
009400*     MOVE 被保険者年Ｗ               TO 被保険者年.
009410*     MOVE 被保険者月Ｗ               TO 被保険者月.
009420*     MOVE 被保険者日Ｗ               TO 被保険者日.
009430**     MOVE 被保険者年齢Ｗ             TO 被保険者年齢.
009440*     MOVE 被保険者男チェックＷ       TO 被保険者男チェック.
009450*     MOVE 被保険者女チェックＷ       TO 被保険者女チェック.
009460*     MOVE 被保険者郵便番号１Ｗ       TO 被保険者郵便番号１.
009470*     MOVE 被保険者郵便番号２Ｗ       TO 被保険者郵便番号２.
009480*     MOVE "-"                        TO 被保険者郵便区切.
009490*     MOVE 被保険者住所１Ｗ           TO 被保険者住所１.
009500*     MOVE 被保険者住所２Ｗ           TO 被保険者住所２.
009510**     MOVE 被保険者電話番号Ｗ         TO 被保険者電話番号.
009520*     MOVE 分割電話番号１Ｗ           TO 電話番号１.
009530*     MOVE 分割電話番号２Ｗ           TO 電話番号２.
009540*     MOVE 分割電話番号３Ｗ           TO 電話番号３.
009550********************
009560* 患者情報セット   *
009570********************
009580*     MOVE ０割チェックＷ             TO ０割チェック.
009590*     MOVE １割チェックＷ             TO １割チェック.
009600*     MOVE ２割チェックＷ             TO ２割チェック.
009610*     MOVE ３割チェックＷ             TO ３割チェック.
009620*     IF  印刷市町村番号Ｗ(1:2) = "99"
009630*         MOVE SPACE                  TO 市町村番号
009640*     ELSE
009650*         MOVE 印刷市町村番号Ｗ       TO 市町村番号
009660*     END-IF.
009670*     IF (印刷受益者番号Ｗ(1:1) = "*") OR
009680*        (印刷受益者番号Ｗ(1:2) = "＊") 
009690*         MOVE SPACE                  TO 受益者番号
009700*     ELSE
009710*         MOVE 印刷受益者番号Ｗ       TO 受益者番号
009720*     END-IF
009730*
009740*     IF  印刷費用負担者番号Ｗ(1:2) = "99"
009750*         MOVE SPACE                  TO 公費負担者番号
009760*     ELSE
009770*         MOVE 印刷費用負担者番号Ｗ   TO 公費負担者番号
009780*     END-IF.
009790*
009800*     IF (印刷受益者番号助成Ｗ(1:1) = "*") OR
009810*        (印刷受益者番号助成Ｗ(1:2) = "＊")
009820*         MOVE SPACE                  TO 受益者番号助成
009830*     ELSE
009840*         MOVE 印刷受益者番号助成Ｗ   TO 受益者番号助成
009850*     END-IF.
009860     MOVE 患者カナＷ                 TO 患者カナ.
009870     MOVE 患者氏名Ｗ                 TO 患者氏名.
009880     MOVE 患者男チェックＷ           TO 患者男チェック.
009890     MOVE 患者女チェックＷ           TO 患者女チェック.
009900     MOVE 明治チェックＷ             TO 明治チェック.
009910     MOVE 大正チェックＷ             TO 大正チェック.
009920     MOVE 昭和チェックＷ             TO 昭和チェック.
009930*     MOVE 平成チェックＷ             TO 平成チェック.
009940     MOVE 患者年Ｗ                   TO 患者年.
009950     MOVE 患者月Ｗ                   TO 患者月.
009960     MOVE 患者日Ｗ                   TO 患者日.
009970     MOVE 患者年齢Ｗ                 TO 患者年齢.
009980*     MOVE 印刷続柄Ｗ                 TO 続柄.
009990     MOVE 患者郵便番号１Ｗ           TO 患者郵便番号１.
010000     MOVE 患者郵便番号２Ｗ           TO 患者郵便番号２.
010010     MOVE 患者郵便区切Ｗ             TO 患者郵便区切.
010020*     MOVE 患者住所Ｗ                 TO 患者住所.
010030     MOVE 患者電話番号Ｗ             TO 患者電話番号.
010040     MOVE 患者住所１Ｗ               TO 患者住所１.
010050     MOVE 患者住所２Ｗ               TO 患者住所２.
010060********************
010070* 事業所情報セット *
010080********************
010090     MOVE 事業所名称Ｗ               TO 事業所名称.
010100*     MOVE 事業所郵便番号１Ｗ         TO 事業所郵便番号１.
010110*     MOVE 事業所郵便番号２Ｗ         TO 事業所郵便番号２.
010120     MOVE 事業所住所１Ｗ             TO 事業所住所１.
010130     MOVE 事業所住所２Ｗ             TO 事業所住所２.
010140*     MOVE 印刷事業所住所Ｗ           TO 事業所住所.
010150************************************
010160* 都道府県・健康保険組合情報セット *
010170************************************
010180*労災、自賠、生保の時は保険者情報は印字しない
010190*     IF 保険種別ＷＲ = 70 OR 80 OR 90
010200*         MOVE SPACE                  TO 保険者番号
010210*         MOVE SPACE                  TO 請求先名支部名１
010220*         MOVE SPACE                  TO 請求先名支部名２
010230*         MOVE SPACE                  TO 保険者郵便番号
010240*         MOVE SPACE                  TO 保険者住所１
010250*         MOVE SPACE                  TO 保険者住所２
010260*     ELSE
010270*         MOVE 印刷保険者番号Ｗ       TO 保険者番号
010280*         MOVE 印刷請求先名支部名１Ｗ TO 請求先名支部名１
010290*         MOVE 印刷請求先名支部名２Ｗ TO 請求先名支部名２
010300*         MOVE 請求先郵便番号Ｗ       TO 保険者郵便番号
010310**         MOVE 請求先住所１Ｗ         TO 保険者住所１
010320**         MOVE 請求先住所２Ｗ         TO 保険者住所２
010330*         MOVE 保険者住所１Ｗ         TO 保険者住所１
010340*         MOVE 保険者住所２Ｗ         TO 保険者住所２
010350*     END-IF.
010360**************************
010370* 保険種別チェックセット *
010380**************************
010390*     MOVE 社保チェックＷ             TO 社保チェック.
010400*     MOVE 組合チェックＷ             TO 組合チェック.
010410*     MOVE 日雇チェックＷ             TO 日雇チェック.
010420*     MOVE 船員チェックＷ             TO 船員チェック.
010430*     MOVE 共済チェックＷ             TO 共済チェック.
010440*     MOVE 国保チェックＷ             TO 国保チェック.
010450*     MOVE 退職チェックＷ             TO 退職チェック.
010460*     MOVE 老人チェックＷ             TO 老人チェック.
010470**     MOVE 乳幼児チェックＷ           TO 乳幼児チェック.
010480**     MOVE 自賠チェックＷ             TO 自賠チェック.
010490**     MOVE 生保チェックＷ             TO 生保チェック.
010500**     MOVE 福チェックＷ               TO 福チェック.
010510**     MOVE 身障チェックＷ             TO 身障チェック.
010520**     MOVE 母子チェックＷ             TO 母子チェック.
010530**     MOVE 生保チェックＷ             TO 生保チェック.
010540**     MOVE 原爆チェックＷ             TO 原爆チェック.
010550*     MOVE 自衛チェックＷ             TO 自衛チェック.
010560*     MOVE 公費チェックＷ             TO 公費チェック.
010570*     MOVE 自費チェックＷ             TO 自費チェック.
010580*================================================================*
010590 印刷下部セット SECTION.
010600*
010610********************
010620* 負傷データセット *
010630********************
010640* １部位 *
010650**********
010660*     MOVE 会部位コードＷ(1)   TO 負傷コード１.
010670     MOVE 負傷名Ｗ(1)         TO 負傷名１.
010680     MOVE 負傷年Ｗ(1)         TO 負傷年１.
010690     MOVE 負傷月Ｗ(1)         TO 負傷月１.
010700     MOVE 負傷日Ｗ(1)         TO 負傷日１.
010710     MOVE 初検年Ｗ(1)         TO 初検年１.
010720     MOVE 初検月Ｗ(1)         TO 初検月１.
010730     MOVE 初検日Ｗ(1)         TO 初検日１.
010740     MOVE 施術開始年Ｗ(1)     TO 施術開始年１.
010750     MOVE 施術開始月Ｗ(1)     TO 施術開始月１.
010760     MOVE 施術開始日Ｗ(1)     TO 施術開始日１.
010770     MOVE 施術終了年Ｗ(1)     TO 施術終了年１.
010780     MOVE 施術終了月Ｗ(1)     TO 施術終了月１.
010790     MOVE 施術終了日Ｗ(1)     TO 施術終了日１.
010800*     MOVE 負傷年月日区切Ｗ(1) TO 区切１１ 区切１２.
010810*     MOVE 施開年月日区切Ｗ(1) TO 区切１３ 区切１４.
010820*     MOVE 施終年月日区切Ｗ(1) TO 区切１５ 区切１６.
010830*     MOVE 転帰Ｗ(1)           TO 転帰１.
010840     MOVE 治癒チェックＷ(1)   TO 治癒チェック１.
010850     MOVE 中止チェックＷ(1)   TO 中止チェック１.
010860     MOVE 転医チェックＷ(1)   TO 転医チェック１.
010870     MOVE 継続チェックＷ(1)   TO 継続チェック１.
010880*     ******************
010890*     * 日数は一時保留 *
010900*     ******************
010910*     **********************
010920*     * 施術回数は一時保留 *
010930*     **********************
010940*     **********************
010950*     * 転帰名称は一時保留 *
010960*     **********************
010970**********
010980* ２部位 *
010990**********
011000*     MOVE 会部位コードＷ(2)   TO 負傷コード２.
011010     MOVE 負傷名Ｗ(2)         TO 負傷名２.
011020     MOVE 負傷年Ｗ(2)         TO 負傷年２.
011030     MOVE 負傷月Ｗ(2)         TO 負傷月２.
011040     MOVE 負傷日Ｗ(2)         TO 負傷日２.
011050     MOVE 初検年Ｗ(2)         TO 初検年２.
011060     MOVE 初検月Ｗ(2)         TO 初検月２.
011070     MOVE 初検日Ｗ(2)         TO 初検日２.
011080     MOVE 施術開始年Ｗ(2)     TO 施術開始年２.
011090     MOVE 施術開始月Ｗ(2)     TO 施術開始月２.
011100     MOVE 施術開始日Ｗ(2)     TO 施術開始日２.
011110     MOVE 施術終了年Ｗ(2)     TO 施術終了年２.
011120     MOVE 施術終了月Ｗ(2)     TO 施術終了月２.
011130     MOVE 施術終了日Ｗ(2)     TO 施術終了日２.
011140*     MOVE 負傷年月日区切Ｗ(2) TO 区切２１ 区切２２.
011150*     MOVE 施開年月日区切Ｗ(2) TO 区切２３ 区切２４.
011160*     MOVE 施終年月日区切Ｗ(2) TO 区切２５ 区切２６.
011170*     MOVE 転帰Ｗ(2)           TO 転帰２.
011180     MOVE 治癒チェックＷ(2)   TO 治癒チェック２.
011190     MOVE 中止チェックＷ(2)   TO 中止チェック２.
011200     MOVE 転医チェックＷ(2)   TO 転医チェック２.
011210     MOVE 継続チェックＷ(2)   TO 継続チェック２.
011220*     ******************
011230*     * 日数は一時保留 *
011240*     ******************
011250*     **********************
011260*     * 施術回数は一時保留 *
011270*     **********************
011280*     **********************
011290*     * 転帰名称は一時保留 *
011300*     **********************
011310**********
011320* ３部位 *
011330**********
011340*     MOVE 会部位コードＷ(3)   TO 負傷コード３.
011350     MOVE 負傷名Ｗ(3)         TO 負傷名３.
011360     MOVE 負傷年Ｗ(3)         TO 負傷年３.
011370     MOVE 負傷月Ｗ(3)         TO 負傷月３.
011380     MOVE 負傷日Ｗ(3)         TO 負傷日３.
011390     MOVE 初検年Ｗ(3)         TO 初検年３.
011400     MOVE 初検月Ｗ(3)         TO 初検月３.
011410     MOVE 初検日Ｗ(3)         TO 初検日３.
011420     MOVE 施術開始年Ｗ(3)     TO 施術開始年３.
011430     MOVE 施術開始月Ｗ(3)     TO 施術開始月３.
011440     MOVE 施術開始日Ｗ(3)     TO 施術開始日３.
011450     MOVE 施術終了年Ｗ(3)     TO 施術終了年３.
011460     MOVE 施術終了月Ｗ(3)     TO 施術終了月３.
011470     MOVE 施術終了日Ｗ(3)     TO 施術終了日３.
011480*     MOVE 負傷年月日区切Ｗ(3) TO 区切３１ 区切３２.
011490*     MOVE 施開年月日区切Ｗ(3) TO 区切３３ 区切３４.
011500*     MOVE 施終年月日区切Ｗ(3) TO 区切３５ 区切３６.
011510*     MOVE 転帰Ｗ(3)           TO 転帰３.
011520     MOVE 治癒チェックＷ(3)   TO 治癒チェック３.
011530     MOVE 中止チェックＷ(3)   TO 中止チェック３.
011540     MOVE 転医チェックＷ(3)   TO 転医チェック３.
011550     MOVE 継続チェックＷ(3)   TO 継続チェック３.
011560*     ******************
011570*     * 日数は一時保留 *
011580*     ******************
011590*     **********************
011600*     * 施術回数は一時保留 *
011610*     **********************
011620*     **********************
011630*     * 転帰名称は一時保留 *
011640*     **********************
011650**********
011660* ４部位 *
011670**********
011680*     MOVE 会部位コードＷ(4)   TO 負傷コード４.
011690*     MOVE 負傷名Ｗ(4)         TO 負傷名４.
011700*     MOVE 負傷年Ｗ(4)         TO 負傷年４.
011710*     MOVE 負傷月Ｗ(4)         TO 負傷月４.
011720*     MOVE 負傷日Ｗ(4)         TO 負傷日４.
011730*     MOVE 施術開始年Ｗ(4)     TO 施術開始年４.
011740*     MOVE 施術開始月Ｗ(4)     TO 施術開始月４.
011750*     MOVE 施術開始日Ｗ(4)     TO 施術開始日４.
011760*     MOVE 施術終了年Ｗ(4)     TO 施術終了年４.
011770*     MOVE 施術終了月Ｗ(4)     TO 施術終了月４.
011780*     MOVE 施術終了日Ｗ(4)     TO 施術終了日４.
011790**     MOVE 負傷年月日区切Ｗ(4) TO 区切４１ 区切４２.
011800**     MOVE 施開年月日区切Ｗ(4) TO 区切４３ 区切４４.
011810**     MOVE 施終年月日区切Ｗ(4) TO 区切４５ 区切４６.
011820**     MOVE 転帰Ｗ(4)           TO 転帰４.
011830*     MOVE 治癒チェックＷ(4)   TO 治癒チェック４.
011840*     MOVE 中止チェックＷ(4)   TO 中止チェック４.
011850*     MOVE 転医チェックＷ(4)   TO 転医チェック４.
011860*     ******************
011870*     * 日数は一時保留 *
011880*     ******************
011890*     **********************
011900*     * 施術回数は一時保留 *
011910*     **********************
011920*     **********************
011930*     * 転帰名称は一時保留 *
011940*     **********************
011950**********
011960* ５部位 *
011970**********
011980*     MOVE 会部位コードＷ(5)   TO 負傷コード５.
011990*     MOVE 負傷名Ｗ(5)         TO 負傷名５.
012000*     MOVE 負傷年Ｗ(5)         TO 負傷年５.
012010*     MOVE 負傷月Ｗ(5)         TO 負傷月５.
012020*     MOVE 負傷日Ｗ(5)         TO 負傷日５.
012030*     MOVE 施術開始年Ｗ(5)     TO 施術開始年５.
012040*     MOVE 施術開始月Ｗ(5)     TO 施術開始月５.
012050*     MOVE 施術開始日Ｗ(5)     TO 施術開始日５.
012060*     MOVE 施術終了年Ｗ(5)     TO 施術終了年５.
012070*     MOVE 施術終了月Ｗ(5)     TO 施術終了月５.
012080*     MOVE 施術終了日Ｗ(5)     TO 施術終了日５.
012090**     MOVE 負傷年月日区切Ｗ(5) TO 区切５１ 区切５２.
012100**     MOVE 施開年月日区切Ｗ(5) TO 区切５３ 区切５４.
012110**     MOVE 施終年月日区切Ｗ(5) TO 区切５５ 区切５６.
012120**     MOVE 転帰Ｗ(5)           TO 転帰５.
012130*     MOVE 治癒チェックＷ(5)   TO 治癒チェック５.
012140*     MOVE 中止チェックＷ(5)   TO 中止チェック５.
012150*     MOVE 転医チェックＷ(5)   TO 転医チェック５.
012160*     ******************
012170*     * 日数は一時保留 *
012180*     ******************
012190*     **********************
012200*     * 施術回数は一時保留 *
012210*     **********************
012220*     **********************
012230*     * 転帰名称は一時保留 *
012240*     **********************
012250************
012260* 負傷原因 *
012270************
012280     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
012290     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
012300     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
012310     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
012320     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
012330     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
012340     MOVE 負傷原因Ｗ(7)       TO 負傷原因７.
012350     MOVE 負傷原因Ｗ(8)       TO 負傷原因８.
012360     MOVE 負傷原因Ｗ(9)       TO 負傷原因９.
012370     MOVE 負傷原因Ｗ(10)      TO 負傷原因１０.
012380     MOVE 負傷原因Ｗ(11)      TO 負傷原因１１.
012390     MOVE 負傷原因Ｗ(12)      TO 負傷原因１２.
012400     MOVE 負傷原因Ｗ(13)      TO 負傷原因１３.
012410     MOVE 負傷原因Ｗ(14)      TO 負傷原因１４.
012420     MOVE 負傷原因Ｗ(15)      TO 負傷原因１５.
012430*HILO テスト－－－－－－－－－－－
012440***     MOVE ALL NC"Ｎ" TO
012450***          負傷原因１   負傷原因２   負傷原因３   負傷原因４   負傷原因５
012460***          負傷原因６   負傷原因７   負傷原因８   負傷原因９   負傷原因１０
012470***          負傷原因１１ 負傷原因１２ 負傷原因１３ 負傷原因１４ 負傷原因１５.
012480*
012490*================================================================*
012500 負担率取得 SECTION.
012510*
012520     MOVE ZERO TO 本人負担率Ｗ.
012530     MOVE ZERO TO 家族負担率Ｗ.
012540*
012550     MOVE 施術和暦ＷＲ   TO 受－施術和暦.
012560     MOVE 施術年ＷＲ     TO 受－施術年.
012570     MOVE 施術月ＷＲ     TO 受－施術月.
012580     MOVE 患者番号ＷＲ   TO 受－患者番号.
012590     MOVE 枝番ＷＲ       TO 受－枝番.
012600     READ 受診者情報Ｆ
012610     INVALID KEY
012620         MOVE  NC"施術月の受診者情報がありません" TO 連メ－メッセージ
012630         CALL   "MSG001"
012640         CANCEL "MSG001"
012650         PERFORM ファイル閉鎖
012660         MOVE ZERO TO PROGRAM-STATUS
012670         EXIT PROGRAM
012680     NOT INVALID KEY
013771* 14/10～　サブルーチン処理
013772         IF 受－施術和暦年月 >= 41410
013773            PERFORM 負担率取得１４１０
               END-IF
013790     END-READ.
013800     EVALUATE 負担率Ｗ
013810     WHEN 0
013820         MOVE NC"○" TO ０割チェックＷ
013830     WHEN 10
013840         MOVE NC"○" TO １割チェックＷ
013850     WHEN 20
013860         MOVE NC"○" TO ２割チェックＷ
013870     WHEN 30
013880         MOVE NC"○" TO ３割チェックＷ
013890     END-EVALUATE.
014692*================================================================*
014693 負担率取得１４１０ SECTION.
014694*
014695* 受診者Ｆ READ中
014696* 平成14/10～
014697     MOVE ZERO  TO 負担率Ｗ.
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
014726*
014080*================================================================*
014090 受診者情報取得 SECTION.
014100*
014110**************************************************
014120* 連結データから受診者情報Ｆより以下の情報を取得 *
014130* ● 患者番号.... 患者番号Ｗ                     *
014140* ● 枝番........ 枝番Ｗ                         *
014150* ● 記号 ....... 記号Ｗに格納                   *
014160* ● 番号 ....... 番号Ｗに格納                   *
014170* ● 被保険者カナ.被保険者カナＷに格納           *
014180* ● 被保険者氏名.被保険者氏名Ｗに格納           *
014190* ● 郵便番号１ ..郵便番号１Ｗに格納             *
014200* ● 郵便番号２ ..郵便番号２Ｗに格納             *
014210* ● 住所１ ......住所１Ｗに格納                 *
014220* ● 住所２ ......住所２Ｗに格納                 *
014230* ● 電話番号.....電話番号Ｗに格納               *
014240* ● 患者カナ ....患者カナＷに格納               *
014250* ● 患者氏名 ....患者氏名Ｗに格納               *
014260* ● 続柄 ........名称マスタより続柄Ｗに取得     *
014270* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
014280* ● 患者年 ......患者年Ｗに格納                 *
014290* ● 患者月 ......患者月Ｗに格納                 *
014300* ● 患者日 ......患者日Ｗに格納                 *
014310**************************************************
014320     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
014330     MOVE 施術年ＷＲ         TO 受－施術年.
014340     MOVE 施術月ＷＲ         TO 受－施術月.
014350     MOVE 患者コードＷＲ     TO 受－患者コード.
014360     READ 受診者情報Ｆ
014370     INVALID KEY
014380         CONTINUE
014390*            /* ありえない */
014400     NOT INVALID KEY
014410         MOVE 受－患者番号     TO 患者番号Ｗ
014420         MOVE 受－枝番         TO 枝番Ｗ
014430*         MOVE 受－記号         TO 記号Ｗ
014440*         MOVE 受－番号         TO 番号Ｗ
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
014520         MOVE 受－被保険者カナ TO 被保険者カナＷ
014530         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
014540*
014550         EVALUATE 受－被保険者性別
014560         WHEN 1
014570             MOVE NC"○"  TO 被保険者男チェックＷ
014580         WHEN 2
014590             MOVE NC"○"  TO 被保険者女チェックＷ
014600         END-EVALUATE
014610*
014620         EVALUATE 受－被保険者和暦
014630         WHEN 1
014640             MOVE NC"○"  TO 被保険者明治チェックＷ
014650         WHEN 2
014660             MOVE NC"○"  TO 被保険者大正チェックＷ
014670         WHEN 3
014680             MOVE NC"○"  TO 被保険者昭和チェックＷ
014690         WHEN 4
014700             MOVE NC"○"  TO 被保険者平成チェックＷ
014710         END-EVALUATE
014720         MOVE 受－被保険者年   TO 被保険者年Ｗ
014730         MOVE 受－被保険者月   TO 被保険者月Ｗ
014740         MOVE 受－被保険者日   TO 被保険者日Ｗ
014750
014760*被保険者年齢
014770     MOVE 連印－施術和暦 TO 元－元号区分
014780     READ 元号マスタ
014790     NOT INVALID KEY
014800         COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
014810     END-READ
014820     IF ( 受－被保険者和暦 NOT = ZERO ) AND
014830        ( 受－被保険者年   NOT = ZERO ) AND
014840        ( 受－被保険者月   NOT = ZERO ) AND
014850        ( 受－被保険者日   NOT = ZERO )
014860
014870         MOVE ZERO TO 被保険者年齢Ｗ
014880*
014890         MOVE 受－被保険者和暦 TO 元－元号区分
014900         READ 元号マスタ
014910         NOT INVALID KEY
014920             COMPUTE 被保険者西暦年Ｗ = 元－開始西暦年 + ( 受－被保険者年 - 1 )
014930         END-READ
014940*
014950         COMPUTE 被保険者年齢Ｗ   = 施術西暦年Ｗ - 被保険者西暦年Ｗ
014960*
014970         IF 連印－施術月 < 受－被保険者月
014980             COMPUTE 被保険者年齢Ｗ = 被保険者年齢Ｗ - 1
014990         END-IF
015000*
015010*         MOVE 被保険者年齢Ｗ     TO 被保険者年齢
015020     END-IF
015030*
015040*/郵便番号 ゼロの時はスペース
015050         IF 受－郵便番号１ = "000"
015060             MOVE SPACE          TO 被保険者郵便番号１Ｗ
015070             MOVE SPACE          TO 被保険者郵便番号２Ｗ
015080         ELSE
015090             MOVE 受－郵便番号１ TO 被保険者郵便番号１Ｗ
015100             MOVE 受－郵便番号２ TO 被保険者郵便番号２Ｗ
015110         END-IF
015120*         MOVE 受－電話番号     TO 被保険者電話番号Ｗ
015130*/電話番号を分割
015140         UNSTRING 受－電話番号 DELIMITED BY "-"
015150             INTO 分割電話番号１Ｗ
015160                  分割電話番号２Ｗ
015170                  分割電話番号３Ｗ
015180         END-UNSTRING
015190*電話番号右詰処理
015200         IF 分割電話番号３Ｗ = SPACE
015210             MOVE 分割電話番号２Ｗ TO 分割電話番号３Ｗ
015220             MOVE 分割電話番号１Ｗ TO 分割電話番号２Ｗ
015230             MOVE SPACE            TO 分割電話番号１Ｗ
015240         END-IF
015250         IF 分割電話番号３Ｗ = SPACE
015260             MOVE SPACE            TO 分割電話番号Ｗ
015270             MOVE 受－電話番号     TO 分割電話番号Ｗ
015280         END-IF
015290*
015300*         STRING 受－住所１  DELIMITED BY SPACE
015310*                受－住所２  DELIMITED BY SPACE
015320*           INTO 被保険者住所Ｗ
015330*         END-STRING
015340         MOVE 受－住所１ TO 被保険者住所１Ｗ
015350         MOVE 受－住所２ TO 被保険者住所２Ｗ
015360*
015370*/郵便番号 ゼロの時はスペース
015380         IF 受－患者郵便番号１ = "000"
015390             MOVE SPACE              TO 患者郵便番号１Ｗ
015400             MOVE SPACE              TO 患者郵便番号２Ｗ
015410         ELSE
015420             MOVE 受－患者郵便番号１ TO 患者郵便番号１Ｗ
015430             MOVE "-"                TO 患者郵便区切Ｗ
015440             MOVE 受－患者郵便番号２ TO 患者郵便番号２Ｗ
015450         END-IF
015460         MOVE 受－患者住所１     TO 患者住所１Ｗ
015470         MOVE 受－患者住所２     TO 患者住所２Ｗ
015480*         STRING 受－患者住所１  DELIMITED BY SPACE
015490*                受－患者住所２  DELIMITED BY SPACE
015500*           INTO 患者住所Ｗ
015510*         END-STRING
015520         MOVE 受－患者電話番号   TO 患者電話番号Ｗ
015530*
015540*電話番号分割
015550*         UNSTRING 電話番号Ｗ  DELIMITED BY "-"
015560*             INTO 分割電話番号１Ｗ
015570*                  分割電話番号２Ｗ
015580*                  分割電話番号３Ｗ
015590*         END-UNSTRING
015600*電話番号右詰処理
015610*         IF 分割電話番号３Ｗ = SPACE
015620*             MOVE 分割電話番号２Ｗ TO 分割電話番号３Ｗ
015630*             MOVE 分割電話番号１Ｗ TO 分割電話番号２Ｗ
015640*             MOVE SPACE            TO 分割電話番号１Ｗ
015650*         END-IF
015660*
015670         MOVE 受－患者カナ     TO 患者カナＷ
015680         MOVE 受－患者氏名     TO 患者氏名Ｗ
015690*
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
               MOVE 事－事業所名称   TO 事業所情報Ｗ
015710         MOVE 事－事業所住所１ TO 事業所住所１Ｗ
015720         MOVE 事－事業所住所２ TO 事業所住所２Ｗ
015770* 続柄
015780         IF 受－本人家族区分 = 1
015790             MOVE NC"本人"     TO 続柄Ｗ
015800         ELSE
015810             MOVE 05          TO 名－区分コード
015820             MOVE 受－続柄    TO 名－名称コード
015830             READ 名称マスタ
015840             INVALID KEY
015850                 MOVE SPACE   TO 続柄Ｗ
015860             NOT INVALID KEY
015870                 MOVE 名－略称  TO 続柄Ｗ
015880             END-READ
015890         END-IF
015900* 患者和暦年月日
015910         EVALUATE 受－患者和暦
015920         WHEN 1
015930             MOVE NC"○"  TO 明治チェックＷ
015940         WHEN 2
015950             MOVE NC"○"  TO 大正チェックＷ
015960         WHEN 3
015970             MOVE NC"○"  TO 昭和チェックＷ
015980         WHEN 4
015990             MOVE NC"○"  TO 平成チェックＷ
016000         END-EVALUATE
016010         MOVE 受－患者年  TO 患者年Ｗ
016020         MOVE 受－患者月  TO 患者月Ｗ
016030         MOVE 受－患者日  TO 患者日Ｗ
016040*患者年齢
016050     MOVE 連印－施術和暦 TO 元－元号区分
016060     READ 元号マスタ
016070     NOT INVALID KEY
016080         COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
016090     END-READ
016100     IF ( 受－患者和暦 NOT = ZERO ) AND
016110        ( 受－患者年   NOT = ZERO ) AND
016120        ( 受－患者月   NOT = ZERO ) AND
016130        ( 受－患者日   NOT = ZERO )
016140*
016150         MOVE ZERO TO 患者年齢Ｗ
016160*
016170         MOVE 受－患者和暦 TO 元－元号区分
016180         READ 元号マスタ
016190         NOT INVALID KEY
016200             COMPUTE 患者西暦年Ｗ = 元－開始西暦年 + ( 受－患者年 - 1 )
016210         END-READ
016220*
016230         COMPUTE 患者年齢Ｗ   = 施術西暦年Ｗ - 患者西暦年Ｗ
016240*
016250         IF 連印－施術月 < 受－患者月
016260             COMPUTE 患者年齢Ｗ = 患者年齢Ｗ - 1
016270         END-IF
016280*
016290*         MOVE 患者年齢Ｗ     TO 患者年齢
016300     END-IF
016310* 患者性別
016320         EVALUATE 受－患者性別
016330         WHEN 1
016340             MOVE NC"○"  TO 患者男チェックＷ
016350         WHEN 2
016360             MOVE NC"○"  TO 患者女チェックＷ
016370         END-EVALUATE
016380*         EVALUATE 受－患者性別
016390*         WHEN 1
016400*             MOVE NC"（男）" TO 患者性別Ｗ
016410*         WHEN 2
016420*             MOVE NC"（女）" TO 患者性別Ｗ
016430*         END-EVALUATE
016440*/有効期限
016450         MOVE 受－有効和暦   TO 有効期限和暦Ｗ
016460         MOVE 受－有効年     TO 有効期限年Ｗ
016470         MOVE 受－有効月     TO 有効期限月Ｗ
016480         MOVE 受－有効日     TO 有効期限日Ｗ
016490         MOVE 有効期限和暦Ｗ TO 元－元号区分
016500         READ 元号マスタ
016510         INVALID KEY
016520             MOVE SPACE        TO 有効期限元号Ｗ
016530         NOT INVALID KEY
016540             MOVE 元－元号名称 TO 有効期限元号Ｗ
016550         END-READ
016560*/資格取得年月日
016570         MOVE 受－資格和暦 TO 資格取得和暦Ｗ
016580         MOVE 受－資格年   TO 資格取得年Ｗ
016590         MOVE 受－資格月   TO 資格取得月Ｗ
016600         MOVE 受－資格日   TO 資格取得日Ｗ
016610         IF (資格取得和暦Ｗ NOT = ZERO) AND
016620            (資格取得年Ｗ   NOT = ZERO) AND
016630            (資格取得月Ｗ   NOT = ZERO) AND
016640            (資格取得日Ｗ   NOT = ZERO)
016650             EVALUATE 資格取得和暦Ｗ
016660             WHEN 3
016670                 MOVE NC"○"  TO 資格昭和チェックＷ
016680             WHEN 4
016690                 MOVE NC"○"  TO 資格平成チェックＷ
016700             END-EVALUATE
016710         END-IF
016720**市町村国保､退職の場合は有効期限を出力
016730**業種別国保(国保組合)､共済､日雇､船員､社保､組合､自衛官は有効期限日を出力する
016740*         IF (( 受－保険種別 = 08 ) OR
016750*            (( 受－保険種別 = 01 ) AND
016760*            (  受－保険者番号(3:1) NOT = "3" )))
016770*             MOVE 受－有効和暦 TO 資格取得和暦Ｗ
016780*             MOVE 受－有効年   TO 資格取得年Ｗ
016790*             MOVE 受－有効月   TO 資格取得月Ｗ
016800*             MOVE 受－有効日   TO 資格取得日Ｗ
016810*         ELSE
016820*             MOVE 受－資格和暦 TO 資格取得和暦Ｗ
016830*             MOVE 受－資格年   TO 資格取得年Ｗ
016840*             MOVE 受－資格月   TO 資格取得月Ｗ
016850*             MOVE 受－資格日   TO 資格取得日Ｗ
016860*         END-IF
016870*         IF (資格取得和暦Ｗ NOT = ZERO) AND
016880*            (資格取得年Ｗ   NOT = ZERO) AND
016890*            (資格取得月Ｗ   NOT = ZERO) AND
016900*            (資格取得日Ｗ   NOT = ZERO)
016910*             EVALUATE 資格取得和暦Ｗ
016920*             WHEN 3
016930*                 MOVE NC"○"  TO 資格昭和チェックＷ
016940*             WHEN 4
016950*                 MOVE NC"○"  TO 資格平成チェックＷ
016960*             END-EVALUATE
016970*         END-IF
016980* 市町村番号 受給者番号
016990         IF 受－助成種別 NOT = ZERO
017000             MOVE 受－費用負担者番号助成 TO 費用負担者番号Ｗ
017010             MOVE 受－受益者番号助成     TO 受益者番号助成Ｗ
017020         END-IF
017030         IF 受－公費種別 = 05
017040             MOVE 受－費用負担者番号     TO 市町村番号Ｗ
017050             MOVE 受－受益者番号老人     TO 受益者番号Ｗ
017060         END-IF
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
015710            MOVE 労災－労災事業所住所１     TO 事業所住所１Ｗ
015720            MOVE 労災－労災事業所住所２     TO 事業所住所２Ｗ
012130            MOVE 労災－労働保険番号         TO 番号Ｗ
               END-IF
      *
017070*/助成種別
017080         IF 受－助成種別 NOT = ZERO
017090             MOVE NC"○"   TO 公費チェックＷ
017100         END-IF
017110*         EVALUATE 受－助成種別
017120*         WHEN 50
017130*             MOVE NC"○"  TO 生保チェックＷ
017140*         WHEN 51
017150**/頭4桁が "4113"東京 "4108"茨城 "4132"島根 の時は、「福」。それ以外は「老」
017160*             IF  受－費用負担者番号助成(1:4) = "4113" OR
017170*                 受－費用負担者番号助成(1:4) = "4108" OR
017180*                 受－費用負担者番号助成(1:4) = "4132"
017190*                 MOVE NC"○"    TO 副チェックＷ
017200*             ELSE
017210*                 MOVE NC"○"    TO 老人チェックＷ
017220*             END-IF
017230*         WHEN 52
017240*             MOVE NC"○"  TO 母子チェックＷ
017250*         WHEN 53
017260*             MOVE NC"○"  TO 身障チェックＷ
017270*         WHEN 54
017280*             MOVE NC"○"  TO 原爆チェックＷ
017290*         WHEN 55
017300*             MOVE NC"○"  TO 乳幼児チェックＷ
017310*         END-EVALUATE
017320*
017330         IF 受－公費種別 = 05
017340             MOVE NC"○"  TO 老人チェックＷ
017350         ELSE
017360             EVALUATE 受－保険種別
017370             WHEN 01
017380                 MOVE NC"○"  TO 国保チェックＷ
017390             WHEN 02
017400                 MOVE NC"○"  TO 社保チェックＷ
017410             WHEN 03
017420                 MOVE NC"○"  TO 組合チェックＷ
017430             WHEN 04
017440                 MOVE NC"○"  TO 共済チェックＷ
017450             WHEN 06
017460                 MOVE NC"○"  TO 日雇チェックＷ
017470             WHEN 07
017480                 MOVE NC"○"  TO 船員チェックＷ
017490             WHEN 08
017500                 MOVE NC"○"  TO 退職チェックＷ
017510             WHEN 09
017520                 MOVE NC"○"  TO 自衛チェックＷ
017530             WHEN 90
017540                 MOVE NC"○"  TO 自費チェックＷ
017550*             WHEN 70
017560*                 MOVE NC"○"  TO 労災チェックＷ
017570*             WHEN 80
017580*                 MOVE NC"○"  TO 自賠チェックＷ
017590             END-EVALUATE
017600         END-IF
017610     END-READ.
017620*HILO テスト－－－－－－－－－－－
017630*      MOVE NC"○"  TO 被保険者明治Ｗ.
017640*      MOVE NC"○"  TO 被保険者大正Ｗ.
017650*      MOVE NC"○"  TO 被保険者昭和Ｗ.
017660*      MOVE NC"○"  TO 被保険者男チェックＷ.
017670*      MOVE NC"○"  TO 被保険者女チェックＷ.
017680***      MOVE NC"○"  TO 患者男チェックＷ.
017690***      MOVE NC"○"  TO 患者女チェックＷ.
017700***      MOVE NC"○"  TO 明治チェックＷ.
017710***      MOVE NC"○"  TO 大正チェックＷ.
017720***      MOVE NC"○"  TO 昭和チェックＷ.
017730*      MOVE NC"○"  TO 平成チェックＷ.
017740*      MOVE NC"○"  TO 社保チェックＷ    .
017750*      MOVE NC"○"  TO 組合チェックＷ    .
017760*      MOVE NC"○"  TO 共済チェックＷ    .
017770*      MOVE NC"○"  TO 国保チェックＷ    .
017780*      MOVE NC"○"  TO 日雇チェックＷ    .
017790*      MOVE NC"○"  TO 船員チェックＷ    .
017800*      MOVE NC"○"  TO 老人チェックＷ    .
017810*      MOVE NC"○"  TO 福チェックＷ    .
017820*      MOVE NC"○"  TO 身障チェックＷ    .
017830*      MOVE NC"○"  TO 退職チェックＷ    .
017840*      MOVE NC"○"  TO 母子チェックＷ    .
017850*      MOVE NC"○"  TO 乳幼児チェックＷ  .
017860*      MOVE NC"○"  TO 国保組合チェックＷ.
017870*      MOVE NC"○"  TO 労災チェックＷ    .
017880*      MOVE NC"○"  TO 生保チェックＷ    .
017890*      MOVE NC"○"  TO 自賠チェックＷ    .
017900*      MOVE NC"○"  TO 原爆チェックＷ    .
017910*      MOVE NC"○" TO ０割チェックＷ.
017920*      MOVE NC"○" TO １割チェックＷ.
017930*      MOVE NC"○" TO ２割チェックＷ.
017940*      MOVE NC"○" TO ３割チェックＷ.
017950*      MOVE NC"○"  TO 資格昭和チェックＷ.
017960*      MOVE NC"○"  TO 資格平成チェックＷ.
017970*      MOVE NC"○"  TO 被保険者明治チェックＷ.
017980*      MOVE NC"○"  TO 被保険者大正チェックＷ.
017990*      MOVE NC"○"  TO 被保険者昭和チェックＷ.
018000*      MOVE NC"○"  TO 被保険者平成チェックＷ.
018010*-------------------------------------
018020*================================================================*
018030 請求先情報取得 SECTION.
018040*
018050****************************************************
018060* 連結データから保険者マスタより請求先を取得する。 *
018070* ※保－請求先情報区分=1の場合請求先マスタを使用   *
018080* ● 保険者番号...保険者番号Ｗに格納               *
018090* ● 名称........ 請求先名称Ｗに格納               *
018100* ● 郵便番号１.. 請求先郵便番号１Ｗに格納         *
018110* ● 郵便番号２.. 請求先郵便番号２Ｗに格納         *
018120* ● 住所１.......請求先住所１Ｗに格納             *
018130* ● 住所２.......請求先住所２Ｗに格納             *
018140****************************************************
018150     MOVE SPACE          TO 保険者番号Ｗ.
018160     MOVE SPACE          TO 請求先名称Ｗ.
018170     MOVE SPACE          TO 支部名Ｗ.
018180     MOVE SPACE          TO 請求先郵便番号Ｗ.
018190     MOVE SPACE          TO 請求先郵便番号１Ｗ.
018200     MOVE SPACE          TO 請求先郵便番号２Ｗ.
018210     MOVE SPACE          TO 請求先住所１Ｗ.
018220     MOVE SPACE          TO 請求先住所２Ｗ.
018230     MOVE SPACE          TO 保険者住所１Ｗ.
018240     MOVE SPACE          TO 保険者住所２Ｗ.
018250     MOVE SPACE          TO 印刷保険者呼名Ｗ.
018260     MOVE SPACE          TO 請求先名支部名Ｗ
018270     MOVE ZERO           TO 接尾語区分Ｗ.
018280*
018290     MOVE 保険種別ＷＲ   TO 保－保険種別.
018300     MOVE 保険者番号ＷＲ TO 保－保険者番号.
018310     READ 保険者マスタ
018320     INVALID KEY
018330         CONTINUE
018340     NOT INVALID KEY
018350         MOVE 保－保険者番号  TO 保険者番号Ｗ
018360         MOVE 保－保険者名称  TO 請求先名称Ｗ
018370         MOVE 保－支部部署名  TO 支部名Ｗ
018380         MOVE 保－郵便番号１  TO 請求先郵便番号１Ｗ
018390         MOVE 保－郵便番号２  TO 請求先郵便番号２Ｗ
018400         IF 請求先郵便番号Ｗ NOT = SPACE
018410             MOVE "-"         TO 請求先郵便番号区切Ｗ
018420         END-IF
018430*         MOVE 保－住所１      TO 請求先住所１Ｗ
018440*         MOVE 保－住所２      TO 請求先住所２Ｗ
018450         STRING 保－住所１ DELIMITED BY SPACE
018460                保－住所２ DELIMITED BY SPACE
018470           INTO 保険者住所Ｗ
018480         END-STRING
018490         MOVE 保－接尾語区分  TO 接尾語区分Ｗ
018500     END-READ.
018510     EVALUATE 保険種別ＷＲ
018520     WHEN 2
018530     WHEN 6
018540         IF 接尾語区分Ｗ = 1
018550            MOVE SPACE              TO 印刷保険者呼名Ｗ
018560         ELSE
018570            MOVE "社会保険事務所"   TO 印刷保険者呼名Ｗ
018580         END-IF
018590     WHEN 3
018600         MOVE "健康保険組合"   TO 印刷保険者呼名Ｗ
018610     WHEN OTHER
018620         MOVE SPACE TO 印刷保険者呼名Ｗ
018630     END-EVALUATE.
018640     IF 印刷保険者呼名Ｗ NOT = SPACE
018650         STRING 請求先名称Ｗ DELIMITED BY SPACE
018660                保険者呼名Ｗ DELIMITED BY SPACE
018670           INTO 請求先名称Ｗ
018680         END-STRING
018690     END-IF.
018700*組合と共済は支部名も印字する
018710     EVALUATE 保険種別ＷＲ
018720     WHEN 03
018730     WHEN 04
018740         STRING 請求先名称Ｗ DELIMITED BY SPACE
018750                "  "         DELIMITED BY SIZE
018760                支部名Ｗ     DELIMITED BY SPACE
018770           INTO 請求先名支部名Ｗ
018780         END-STRING
018790     WHEN OTHER
018800         MOVE 請求先名称Ｗ TO 請求先名支部名Ｗ
018810     END-EVALUATE.
018820*================================================================*
018830 負傷データ取得 SECTION.
018840*
018850**************************************************
018860* 連結データから負傷データＦより以下の情報を取得 *
018870* ● 負傷名...部位＋負傷種別にて加工して格納     *
018880* ● 負傷年.......負傷年Ｗ                       *
018890* ● 負傷月.......負傷月Ｗ                       *
018900* ● 負傷日.......負傷日Ｗ                       *
018910* ● 施術終了年...終了年Ｗ                       *
018920* ● 施術終了月...終了月Ｗ                       *
018930* ● 施術終了日...終了日Ｗ                       *
018940**************************************************
018950     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
018960     MOVE 施術年ＷＲ         TO 負－施術年.
018970     MOVE 施術月ＷＲ         TO 負－施術月.
018980     MOVE 患者コードＷＲ     TO 負－患者コード.
018990     READ 負傷データＦ
019000     INVALID KEY
019010         CONTINUE
019020*            /* ありえない */
019030     NOT INVALID KEY
019040         MOVE ZERO                         TO 部位数Ｗ
019050         MOVE 負－部位数                   TO 部位数Ｗ
019060         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
019070                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
019080*********************************************
019090* 注）全柔...負傷種別＋部位にて加工して格納 *
019100*********************************************
019110* 負傷種別
019120             MOVE SPACE                     TO 負傷名称Ｗ
019130             MOVE 03                        TO 名－区分コード
019140             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
019150             READ 名称マスタ
019160             INVALID KEY
019170                 MOVE SPACE    TO 負傷名称Ｗ
019180             NOT INVALID KEY
019190*                 MOVE 名－略称 TO 負傷名称Ｗ
019200                 MOVE 名－正式名称 TO 負傷名称Ｗ
019210             END-READ
019220* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
019240             PERFORM 名称埋込処理
019300             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
019310             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
019320             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
019330*             IF 負－負傷年(部位ＣＮＴ) NOT = ZERO 
019340*                MOVE "."                   TO 負傷年月日区切Ｗ(部位ＣＮＴ)
019350*             END-IF
019360*
019370             MOVE 負－開始年(部位ＣＮＴ)   TO 初検年Ｗ(部位ＣＮＴ)
019380             MOVE 負－開始月(部位ＣＮＴ)   TO 初検月Ｗ(部位ＣＮＴ)
019390             MOVE 負－開始日(部位ＣＮＴ)   TO 初検日Ｗ(部位ＣＮＴ)
019400*             IF 負－開始年(部位ＣＮＴ) NOT = ZERO 
019410*                MOVE "."                   TO 初検年月日区切Ｗ(部位ＣＮＴ)
019420*             END-IF
019430*
019440             MOVE 負－開始年(部位ＣＮＴ)   TO 施術開始年Ｗ(部位ＣＮＴ)
019450             MOVE 負－開始月(部位ＣＮＴ)   TO 施術開始月Ｗ(部位ＣＮＴ)
019460             MOVE 負－開始日(部位ＣＮＴ)   TO 施術開始日Ｗ(部位ＣＮＴ)
019470*             IF 負－開始年(部位ＣＮＴ) NOT = ZERO 
019480*                MOVE "."                   TO 施開年月日区切Ｗ(部位ＣＮＴ)
019490*             END-IF
019500*
019510             MOVE 負－終了年(部位ＣＮＴ)   TO 施術終了年Ｗ(部位ＣＮＴ)
019520             MOVE 負－終了月(部位ＣＮＴ)   TO 施術終了月Ｗ(部位ＣＮＴ)
019530             MOVE 負－終了日(部位ＣＮＴ)   TO 施術終了日Ｗ(部位ＣＮＴ)
019540*             IF 負－終了年(部位ＣＮＴ) NOT = ZERO 
019550*                MOVE "."                   TO 施終年月日区切Ｗ(部位ＣＮＴ)
019560*             END-IF
019570*転帰
019580             EVALUATE 負－転帰区分(部位ＣＮＴ)
019590             WHEN 1
019600             WHEN 2
019610                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
019620             WHEN 3
019630                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
019640             WHEN 4
019650                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
019660             WHEN 9
019670                 MOVE NC"○"               TO 継続チェックＷ(部位ＣＮＴ)
019680             END-EVALUATE
019690*             IF 負－転帰区分(部位ＣＮＴ) NOT = 9
019700*                 MOVE 04                        TO 名－区分コード
019710*                 MOVE 負－転帰区分(部位ＣＮＴ)  TO 名－名称コード
019720*                 READ 名称マスタ
019730*                 INVALID KEY
019740*                     MOVE SPACE    TO 転帰Ｗ(部位ＣＮＴ)
019750*                 NOT INVALID KEY
019760*                     MOVE 名－略称 TO 転帰Ｗ(部位ＣＮＴ)
019770*                 END-READ
019780*             END-IF
019790*hilo テスト--------------------------
019800***         MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
019810***         MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
019820***         MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
019830***         MOVE NC"○"               TO 継続チェックＷ(部位ＣＮＴ)
019840*-------------------------------------
019850         END-PERFORM
019860     END-READ.
019870*================================================================*
019880 施術記録取得 SECTION.
019890*
019900************************************************************
019910* 連結データから負傷データＦより以下の情報を取得           *
019920* ● 施術開始年月日...該当する部位に対して当月最初の施術日 *
019930************************************************************
019940     MOVE 施術和暦ＷＲ  TO 施記－施術和暦.
019950     MOVE 施術年ＷＲ    TO 施記－施術年.
019960     MOVE 施術月ＷＲ    TO 施記－施術月.
019970     MOVE ZERO            TO 施記－施術日.
019980     MOVE ZERO            TO 施記－患者番号.
019990     MOVE SPACE           TO 施記－枝番.
020000     START 施術記録Ｆ   KEY IS >= 施記－施術和暦年月日
020010                                  施記－患者コード.
020020     IF 状態キー = "00"
020030         MOVE SPACE  TO 終了フラグ２
020040         PERFORM 施術記録Ｆ読込
020050         PERFORM UNTIL ( 終了フラグ２       = "YES" ) OR
020060                       ( 施記－施術和暦 NOT = 施術和暦ＷＲ ) OR
020070                       ( 施記－施術年   NOT = 施術年ＷＲ   ) OR
020080                       ( 施記－施術月   NOT = 施術月ＷＲ   )
020090*            **************
020100*            * 開始年月日 *
020110*            **************
020120             PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
020130                     UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
020140            EVALUATE TRUE ALSO TRUE ALSO TRUE ALSO TRUE ALSO TRUE
020150            WHEN 施記－負傷種別(部位ＣＮＴ) = 負傷種別Ｗ(部位ＣＮＴ) ALSO
020160                 施記－部位(部位ＣＮＴ)     = 部位Ｗ(部位ＣＮＴ)     ALSO
020170                 施記－左右区分(部位ＣＮＴ) = 左右区分Ｗ(部位ＣＮＴ) ALSO
020180                 施記－負傷位置番号(部位ＣＮＴ)
020190                                       = 負傷位置番号Ｗ(部位ＣＮＴ)  ALSO
020200                 開始年月日取得フラグ(部位ＣＮＴ) = SPACE
020210                   MOVE 施記－施術年     TO 施術開始年Ｗ(部位ＣＮＴ)
020220                   MOVE 施記－施術月     TO 施術開始月Ｗ(部位ＣＮＴ)
020230                   MOVE 施記－施術日     TO 施術開始日Ｗ(部位ＣＮＴ)
020240                   MOVE "YES"       TO 開始年月日取得フラグ(部位ＣＮＴ)
020250*
020260            WHEN OTHER
020270                CONTINUE
020280            END-EVALUATE
020290             END-PERFORM
020300             PERFORM 施術記録Ｆ読込
020310         END-PERFORM
020320     END-IF.
020330*================================================================*
020340 施術記録Ｆ読込 SECTION.
020350*
020360     READ 施術記録Ｆ NEXT
020370     AT END
020380         MOVE "YES" TO 終了フラグ２
020390     END-READ.
020400*
020410*================================================================*
020420 負傷原因取得 SECTION.
020430*
020440********************************************************************
020450*  負傷原因コードが同じものは、1行にまとめて印字する。
020460*  例: ①② 家で転んだ.
020470*     負傷原因コードが同じものをまとめ、テーブルにセット
020480*     (ただし、部位を飛んで同じものは、2行になる)
020490********************************************************************
020500     MOVE  ZERO   TO  カウンタ カウンタ２.
020510     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
020520             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
020530*
020540****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
020550        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
020560*
020570           IF カウンタ = ZERO
020580               MOVE 1   TO  カウンタ カウンタ２
020590               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
020600               MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
020610               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
020620           ELSE
020630              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
020640                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
020650                 COMPUTE カウンタ２ = カウンタ２  +  1
020660                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
020670              ELSE
020680                 COMPUTE カウンタ = カウンタ  +  1
020690                 MOVE 1   TO  カウンタ２
020700                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
020710                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
020720                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
020730              END-IF
020740           END-IF
020750        END-IF
020760     END-PERFORM.
020770**************************************************************************
020780*  負傷原因マスタより文章取得
020790**************************************************************************
020800     MOVE  ZERO   TO  カウンタ カウンタ２.
020810     PERFORM VARYING カウンタ FROM 1 BY 1
020820             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
020830** 労災は 区分 02
020840         MOVE 02                        TO 負原－区分コード
020850         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
020860         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
020870         READ 負傷原因Ｆ
020880         NOT INVALID KEY
020890             INITIALIZE 負傷原因ＷＴ
020900             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
020910             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
020920             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
020930             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
020940             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
020950             PERFORM VARYING カウンタ２ FROM 1 BY 1
020960                     UNTIL ( カウンタ２ > 9 )  OR 
020970                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
020980                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
020990                WHEN 1
021000                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
021010                WHEN 2
021020                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
021030                WHEN 3
021040                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
021050                WHEN 4
021060                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
021070                WHEN 5
021080                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
021090                WHEN OTHER
021100                   CONTINUE
021110                END-EVALUATE
021120             END-PERFORM
021130*
021212             IF 負原－負傷原因入力区分 = 1
021213                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
021214                        負傷原因１ＷＴ  DELIMITED BY SIZE
021215                        負傷原因２ＷＴ  DELIMITED BY SIZE
021216                        負傷原因３ＷＴ  DELIMITED BY SIZE
021217                        負傷原因４ＷＴ  DELIMITED BY SIZE
021218                        負傷原因５ＷＴ  DELIMITED BY SIZE
021219                        INTO 負傷原因内容合成Ｗ(カウンタ)
021220                 END-STRING
021221             ELSE
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
021230             END-IF
021231*
021232         END-READ
021233     END-PERFORM.
021240*
021250     PERFORM 負傷原因セット.
021260*
021270*================================================================*
021280 負傷原因セット SECTION.
021290*
021300**************************************************************************
021310*  文章が1行を超える時は、複数行に分解する。
021320**************************************************************************
021330     MOVE  ZERO   TO  カウンタ カウンタ２.
021340     PERFORM VARYING カウンタ FROM 1 BY 1
021350             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
021360*
021370          INITIALIZE 負傷原因内容分解ＸＷ
021380          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
021390          IF  負傷原因内容１ＸＷ  NOT = SPACE
021400              COMPUTE カウンタ２ = カウンタ２  +  1
021410              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
021420          END-IF
021430          IF  負傷原因内容２ＸＷ  NOT = SPACE
021440              COMPUTE カウンタ２ = カウンタ２  +  1
021450              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
021460          END-IF
021470          IF  負傷原因内容３ＸＷ  NOT = SPACE
021480              COMPUTE カウンタ２ = カウンタ２  +  1
021490              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
021500          END-IF
021510          IF  負傷原因内容４ＸＷ  NOT = SPACE
021520              COMPUTE カウンタ２ = カウンタ２  +  1
021530              MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
021540          END-IF
021550          IF  負傷原因内容５ＸＷ  NOT = SPACE
021560              COMPUTE カウンタ２ = カウンタ２  +  1
021570              MOVE 負傷原因内容５ＸＷ  TO 負傷原因Ｗ(カウンタ２)
021580          END-IF
021510          IF  負傷原因内容６ＸＷ  NOT = SPACE
021520              COMPUTE カウンタ２ = カウンタ２  +  1
021530              MOVE 負傷原因内容６ＸＷ  TO 負傷原因Ｗ(カウンタ２)
021540          END-IF
021550          IF  負傷原因内容７ＸＷ  NOT = SPACE
021560              COMPUTE カウンタ２ = カウンタ２  +  1
021570              MOVE 負傷原因内容７ＸＷ  TO 負傷原因Ｗ(カウンタ２)
021580          END-IF
021590*
021600     END-PERFORM.
021610*
021620*================================================================*
021630 印刷処理 SECTION.
021640*
021650     EVALUATE 印刷モードＦＷＲ
021660     WHEN 0
021670         PERFORM 印刷処理２
021680         PERFORM 印刷処理３
021690     WHEN 1
021700         PERFORM 印刷処理２
021710     WHEN 2
021720         PERFORM 印刷処理３
021750     END-EVALUATE.
021850*================================================================*
021860 印刷処理２ SECTION.
021870*
021880     MOVE "YJB6621P"  TO  定義体名Ｐ.
021890     MOVE "GRP001"    TO  項目群名Ｐ.
021900     WRITE YJB6621P.
021910*     WRITE 印刷レコード.
021920     PERFORM エラー処理Ｐ.
021930*================================================================*
021940 印刷処理３ SECTION.
021950*
021960     MOVE "YJB6621P"  TO  定義体名Ｐ.
021970     MOVE "GRP002"    TO  項目群名Ｐ.
021980     WRITE YJB6621P.
021990*     WRITE 印刷レコード.
022000     PERFORM エラー処理Ｐ.
022080*================================================================*
022090 エラー処理Ｐ SECTION.
022100*
022110     IF 通知情報Ｐ NOT = "00"
022120         DISPLAY NC"帳票エラー"              UPON CONS
022130         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
022140         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
022150         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
022160         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
022170                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
022180         ACCEPT  キー入力 FROM CONS
022190         PERFORM ファイル閉鎖
022200         EXIT PROGRAM
022210     END-IF.
022220*================================================================*
022230 名称埋込処理 SECTION.
022240*
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
022350     END-READ.
022360*
023130*================================================================*
023140******************************************************************
023150 END PROGRAM YJB6621.
023160******************************************************************
