000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAS662.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*         あさひ用
000100*         カルテ印刷（柔+ｳｨﾝﾄﾞｳｽﾞ版）
000110*         MED = YAW660 YAS662P
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2012-08-01
000140 DATE-COMPILED.          2012-08-01
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
000510     SELECT  レセプトＦ      ASSIGN      TO        RECEPTL
000520                             ORGANIZATION             IS  INDEXED
000530                             ACCESS MODE              IS  DYNAMIC
000540                             RECORD KEY               IS  レセ－施術和暦年月
000550                                                          レセ－患者コード
000560                                                          レセ－レセ種別
000570                             ALTERNATE RECORD KEY     IS  レセ－患者コード
000580                                                          レセ－施術和暦年月
000590                                                          レセ－レセ種別
000600                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
000610                                                          レセ－施術和暦年月
000620                                                          レセ－患者コード
000630                                                          レセ－レセ種別
000640                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
000650                                                          レセ－レセ種別
000660                                                          レセ－請求保険者番号
000670                                                          レセ－患者コード
000680                                                          レセ－施術和暦年月
000690                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
000700                                                          レセ－請求保険者番号
000710                                                          レセ－患者コード
000720                                                          レセ－レセ種別
000730                                                          レセ－施術和暦年月
000740                             FILE STATUS              IS  状態キー
000750                             LOCK        MODE         IS  AUTOMATIC.
000760     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000770                             ORGANIZATION             IS  INDEXED
000780                             ACCESS MODE              IS  DYNAMIC
000790                             RECORD KEY               IS  制－制御区分
000800                             FILE STATUS              IS  状態キー
000810                             LOCK        MODE         IS  AUTOMATIC.
000820     SELECT  請求先マスタ    ASSIGN      TO        SEIKYUSL
000830                             ORGANIZATION             IS  INDEXED
000840                             ACCESS MODE              IS  DYNAMIC
000850                             RECORD KEY               IS  請先－保険種別
000860                                                          請先－保険者番号
000870                             FILE STATUS              IS  状態キー
000880                             LOCK    MODE             IS  AUTOMATIC.
000890     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000900                             ORGANIZATION             IS  INDEXED
000910                             ACCESS MODE              IS  DYNAMIC
000920                             RECORD KEY               IS  受－施術和暦年月
000930                                                          受－患者コード
000940                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000950                                                          受－患者カナ
000960                                                          受－患者コード
000970                             ALTERNATE RECORD KEY     IS  受－患者コード
000980                                                          受－施術和暦年月
000990                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001000                                                          受－保険種別
001010                                                          受－保険者番号
001020                                                          受－患者コード
001030                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001040                                                          受－公費種別
001050                                                          受－費用負担者番号
001060                                                          受－患者コード
001070                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001080                                                          受－助成種別
001090                                                          受－費用負担者番号助成
001100                                                          受－患者コード
001110                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
001120                                                          受－施術和暦年月
001130                                                          受－患者コード
001140                             FILE STATUS              IS  状態キー
001150                             LOCK        MODE         IS  AUTOMATIC.
001160     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
001170                             ORGANIZATION             IS  INDEXED
001180                             ACCESS MODE              IS  DYNAMIC
001190                             RECORD KEY               IS  施記－施術和暦年月日
001200                                                          施記－患者コード
001210                             ALTERNATE RECORD KEY     IS  施記－患者コード
001220                                                          施記－施術和暦年月日
001230                             FILE STATUS              IS  状態キー
001240                             LOCK        MODE         IS  AUTOMATIC.
001250     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
001260                             ORGANIZATION             IS  INDEXED
001270                             ACCESS MODE              IS  DYNAMIC
001280                             RECORD KEY               IS 負－施術和暦年月
001290                                                         負－患者コード
001300                             ALTERNATE RECORD KEY     IS 負－患者コード
001310                                                         負－施術和暦年月
001320                             FILE STATUS              IS  状態キー
001330                             LOCK        MODE         IS  AUTOMATIC.
001340     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
001350                             ORGANIZATION             IS  INDEXED
001360                             ACCESS MODE              IS  DYNAMIC
001370                             RECORD KEY               IS  負原－区分コード
001380                                                          負原－負傷原因コード
001390                             FILE STATUS              IS  状態キー
001400                             LOCK        MODE         IS  AUTOMATIC.
001410     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
001420                             ORGANIZATION             IS  INDEXED
001430                             ACCESS MODE              IS  DYNAMIC
001440                             RECORD KEY               IS  市－公費種別
001450                                                          市－市町村番号
001460                             ALTERNATE RECORD KEY     IS  市－公費種別
001470                                                          市－市町村名称
001480                                                          市－市町村番号
001490                             FILE STATUS              IS  状態キー
001500                             LOCK        MODE         IS  AUTOMATIC.
001510     SELECT  生保情報Ｆ      ASSIGN      TO        SEIHOJL
001520                             ORGANIZATION             IS INDEXED
001530                             ACCESS MODE              IS DYNAMIC
001540                             RECORD KEY               IS 生保－施術和暦年月
001550                                                         生保－患者コード
001560                             ALTERNATE RECORD KEY     IS 生保－患者コード
001570                                                         生保－施術和暦年月
001580                             FILE STATUS              IS 状態キー
001590                             LOCK        MODE         IS AUTOMATIC.
001600     SELECT  事業所マスタ    ASSIGN      TO        JIGYOSL
001610                             ORGANIZATION             IS  INDEXED
001620                             ACCESS MODE              IS  DYNAMIC
001630                             RECORD KEY               IS  事－保険種別
001640                                                          事－保険者番号
001650                                                          事－記号
001660                             FILE STATUS              IS  状態キー
001670                             LOCK        MODE         IS  AUTOMATIC.
001680     SELECT  労災情報Ｆ      ASSIGN      TO        ROUSAIJL
001690                             ORGANIZATION             IS INDEXED
001700                             ACCESS MODE              IS DYNAMIC
001710                             RECORD KEY               IS 労災－施術和暦年月
001720                                                         労災－患者コード
001730                             ALTERNATE RECORD KEY     IS 労災－患者コード
001740                                                         労災－施術和暦年月
001750                             FILE STATUS              IS 状態キー
001760                             LOCK        MODE         IS AUTOMATIC.
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
001770     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF002
001780                             SYMBOLIC    DESTINATION  IS "PRT"
001790                             FORMAT                   IS  定義体名Ｐ
001800                             GROUP                    IS  項目群名Ｐ
001810                             PROCESSING  MODE         IS  処理種別Ｐ
001820                             UNIT        CONTROL      IS  拡張制御Ｐ
001830                             FILE        STATUS       IS  通知情報Ｐ.
001840******************************************************************
001850*                      DATA DIVISION                             *
001860******************************************************************
001870 DATA                    DIVISION.
001880 FILE                    SECTION.
001890*                           ［ＲＬ＝  ３２０］
001900 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001910     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
001920*                           ［ＲＬ＝  １２８］
001930 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001940     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001950*                           ［ＲＬ＝  １２８］
001960 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
001970     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
001980*                          ［ＲＬ＝  １５３６］
001990 FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
002000     COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
002010*                           ［ＲＬ＝  ２５６］
002020 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
002030     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
002040*                           ［ＲＬ＝  １２８］
002050 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
002060     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
002070*                           ［ＲＬ＝  ３２０］
002080 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
002090     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
002100*                           ［ＲＬ＝  ２５６］
002110 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
002120     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
002130*                           ［ＲＬ＝  １２８］
002140 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002150     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
002160*                           ［ＲＬ＝  １２８］
002170 FD  負傷原因Ｆ          BLOCK   CONTAINS   1   RECORDS.
002180     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
002190*                           ［ＲＬ＝  ２５６］
002200 FD  市町村マスタ        BLOCK   CONTAINS   1   RECORDS.
002210     COPY SITYOSN         OF  XFDLIB  JOINING   市   AS  PREFIX.
002220* 
002230 FD  生保情報Ｆ          BLOCK   CONTAINS   1   RECORDS.
002240     COPY SEIHOJ          OF  XFDLIB  JOINING   生保   AS  PREFIX.
002250*
002260 FD  事業所マスタ        BLOCK   CONTAINS   1   RECORDS GLOBAL.
002270     COPY JIGYOS          OF  XFDLIB  JOINING   事   AS  PREFIX.
002280*
002290 FD  労災情報Ｆ          BLOCK   CONTAINS   1   RECORDS.
002300     COPY ROUSAIJ         OF  XFDLIB  JOINING   労災   AS  PREFIX.
000284*
000282 FD  自賠責情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
000283     COPY JIBAIJ      OF  XFDLIB  JOINING   自賠   AS  PREFIX.
000114*
000115 FD  保険会社マスタ   BLOCK   CONTAINS   1   RECORDS.
000116     COPY HOKENCOM    OF  XFDLIB  JOINING   保険会   AS  PREFIX.
002310*
002320 FD  印刷ファイル.
002330     COPY YAS662P        OF  XMDLIB.
002340*----------------------------------------------------------------*
002350******************************************************************
002360*                WORKING-STORAGE SECTION                         *
002370******************************************************************
002380 WORKING-STORAGE         SECTION.
002390 01 キー入力                           PIC X     VALUE SPACE.
002400 01 状態キー                           PIC X(2)  VALUE SPACE.
002410 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002420 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002430 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002440 01 詳細ＣＮＴ                         PIC 9     VALUE ZERO.
002450 01 文字ＣＮＴ                         PIC 9(2)  VALUE ZERO.
002460 01 初検フラグ                         PIC X(3)  VALUE SPACE.
002470 01 ファイル名                         PIC N(2)  VALUE SPACE.
002480 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
002490 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
002500 01 全角空白                           PIC X(2)  VALUE X"8140".
002510 01 半角空白                           PIC X(2)  VALUE X"2020".
002520 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002530 01 部位名称Ｗ                         PIC N(10) VALUE SPACE.
002540 01 負傷番号Ｗ                         PIC 9     VALUE ZERO.
002550 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
002560    03 負傷番号Ｗ１                    PIC X.
002570 01 カルテ用紙種別Ｗ                   PIC 9(1).
002571*
002580 01 全角負傷番号Ｗ                     PIC N     VALUE SPACE.
002590 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
002600    03 全角負傷番号Ｗ１                PIC X(2).
002610*
002620 01 負担率Ｗ                           PIC 9(3) VALUE ZERO.
002630 01 甲乙区分Ｗ                         PIC 9    VALUE ZERO.
002640 01 本人負担率Ｗ                       PIC 9(3) VALUE ZERO.
002650 01 家族負担率Ｗ                       PIC 9(3) VALUE ZERO.
002660*
002670 01 施術和暦年月日ＣＷ.
002680   03 施術和暦年月ＣＷ.
002690     05 施術和暦ＣＷ                   PIC 9    VALUE ZERO.
002700     05 施術年月ＣＷ.
002710        07 施術年ＣＷ                  PIC 9(2) VALUE ZERO.
002720        07 施術月ＣＷ                  PIC 9(2) VALUE ZERO.
002730   03 施術日ＣＷ                       PIC 9(2) VALUE ZERO.
002740 01 施術西暦年Ｗ                       PIC 9(4) VALUE ZERO.
002750 01 患者西暦年Ｗ                       PIC 9(4) VALUE ZERO.
002760 01 被保険者西暦年Ｗ                   PIC 9(4) VALUE ZERO.
002770*
002780** 負傷原因用
002790*
002800 01 カウンタ                           PIC 9(2)  VALUE ZERO.
002810 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
002820*
002830 01 負傷原因ＷＴ.
002840    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
002850    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
002860    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
002870    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
002880    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
002890    03 負傷原因ナンバーＷＴ.
002900       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
002910    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
002920 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
002930 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
002940 01 負傷原因ＴＢＬ.
002950    03 負傷原因コードＴＢＬ            OCCURS 9.
002960       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
002970       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
002980       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
002990 01 負傷原因内容Ｗ.
003000    03 負傷原因内容合成Ｗ              PIC X(316) OCCURS 9 VALUE SPACE.
003010    03 負傷原因内容分解ＸＷ.
003020       05 負傷原因内容１ＸＷ           PIC X(60)  VALUE SPACE.
003030       05 負傷原因内容２ＸＷ           PIC X(60)  VALUE SPACE.
003040       05 負傷原因内容３ＸＷ           PIC X(60)  VALUE SPACE.
003050       05 負傷原因内容４ＸＷ           PIC X(60)  VALUE SPACE.
003060       05 負傷原因内容５ＸＷ           PIC X(60)  VALUE SPACE.
003070       05 負傷原因内容６ＸＷ           PIC X(16)  VALUE SPACE.
003080*
003090*/負傷原因の詳細印刷/0610
003100 01 負傷原因Ｗテーブル２.
003110    03 負傷原因Ｗ２                    PIC X(60) OCCURS 54 VALUE SPACE.
003120** 負傷原因印刷区分用
003130 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
003140*
003150 01 負傷原因情報Ｗ.
003160    03 負傷原因コードＷ  OCCURS   9.
003170       09 負傷原因患者番号Ｗ           PIC 9(6) VALUE ZERO.
003180       09 負傷原因連番Ｗ               PIC 9(4) VALUE ZERO.
003190*
003200* 社保用
003210 01 接尾語区分Ｗ                       PIC 9     VALUE ZERO.
003220*
003230****************
003240* 連結項目待避 *
003250****************
003260*    ************
003270*    * 印刷キー *
003280*    ************
003290 01 対象データＷＲ.
003300    03 施術和暦年月ＷＲ.
003310       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
003320       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
003330       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
003340    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
003350    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
003360    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
003370    03 被保険者カナＷＲ                 PIC X(20) VALUE SPACE.
003380    03 患者コードＷＲ.
003390       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
003400       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
003410    03 印刷モードＦＷＲ                 PIC 9(1)  VALUE ZERO.
003420    03 提出年月日ＷＲ.
003430       05 提出年ＷＲ                    PIC 9(2)  VALUE ZERO.
003440       05 提出月ＷＲ                    PIC 9(2)  VALUE ZERO.
003450       05 提出日ＷＲ                    PIC 9(2)  VALUE ZERO.
003460    03 段数ＷＲ                         PIC 9(1)  VALUE ZERO.
003470************
003480* 料金情報 *
003490************
003500*
003510 01 印刷負担率Ｗ                        PIC 9(1)  VALUE ZERO.
003520**************
003530* 受診者情報 *
003540**************
003550 01 受診者情報Ｗ.
003560    03 患者コードＷ.
003570       05 患者番号Ｗ                   PIC 9(6)  VALUE ZERO.
003580       05 枝番Ｗ                       PIC X(1)  VALUE SPACE.
003590    03 市町村番号Ｗ.
003600       05 印刷市町村番号Ｗ             PIC X(8)  VALUE SPACE.
003610       05 FILLER                       PIC X(2).
003620    03 受益者番号Ｗ.
003630       05 印刷受益者番号Ｗ             PIC X(7)  VALUE SPACE.
003640       05 FILLER                       PIC X(13).
003650    03 費用負担者番号Ｗ.
003660       05 印刷費用負担者番号Ｗ         PIC X(8)  VALUE SPACE.
003670       05 FILLER                       PIC X(2).
003680    03 受益者番号助成Ｗ.
003690*/10桁印字/0412
003700       05 印刷受益者番号助成Ｗ         PIC X(10)  VALUE SPACE.
003710       05 FILLER                       PIC X(10).
003720    03 記号Ｗ                          PIC X(24)  VALUE SPACE.
003730    03 番号Ｗ.
003740       05 印刷番号Ｗ                   PIC X(30)  VALUE SPACE.
003750*       05 FILLER                       PIC X(18).
003760    03 被保険者情報Ｗ.
003770       05 被保険者カナＷ               PIC X(50)  VALUE SPACE.
003780       05 被保険者氏名Ｗ               PIC X(50)  VALUE SPACE.
003790*
003800*       05 被保険者和暦チェックＷ.
003810*          07 被保険者明治Ｗ            PIC N(1)  VALUE SPACE.
003820*          07 被保険者大正Ｗ            PIC N(1)  VALUE SPACE.
003830*          07 被保険者昭和Ｗ            PIC N(1)  VALUE SPACE.
003840*          07 被保険者平成Ｗ            PIC N(1)  VALUE SPACE.
003850       05 被保険者性別チェックＷ.
003860          07 被保険者男チェックＷ      PIC N(1)  VALUE SPACE.
003870          07 被保険者女チェックＷ      PIC N(1)  VALUE SPACE.
003880*       05 被保険者性別Ｗ               PIC 9     VALUE ZERO.
003890       05 被保険者和暦チェックＷ.
003900          07 被保険者明治チェックＷ    PIC N(1)  VALUE SPACE.
003910          07 被保険者大正チェックＷ    PIC N(1)  VALUE SPACE.
003920          07 被保険者昭和チェックＷ    PIC N(1)  VALUE SPACE.
003930          07 被保険者平成チェックＷ    PIC N(1)  VALUE SPACE.
004490       05 被保険者元号Ｗ               PIC N(2)  VALUE SPACE.
003940       05 被保険者生年月日Ｗ.
003950          07 被保険者和暦Ｗ            PIC 9     VALUE ZERO.
003960          07 被保険者年Ｗ              PIC 9(2)  VALUE ZERO.
003970          07 被保険者月Ｗ              PIC 9(2)  VALUE ZERO.
003980          07 被保険者日Ｗ              PIC 9(2)  VALUE ZERO.
003990       05 被保険者年齢Ｗ               PIC 9(3)  VALUE ZERO.
004000*
004010       05 被保険者郵便番号Ｗ.
004020          07 被保険者郵便番号１Ｗ      PIC X(3)  VALUE SPACE.
004030          07 被保険者郵便番号２Ｗ      PIC X(4)  VALUE SPACE.
004040*       05 被保険者住所Ｗ.
004050*          07 印刷被保険者住所Ｗ        PIC X(80) VALUE SPACE.
004060       05 被保険者住所Ｗ.
004070          07 印刷被保険者住所１Ｗ      PIC X(40) VALUE SPACE.
004080          07 印刷被保険者住所２Ｗ      PIC X(40) VALUE SPACE.
004090       05 被保険者電話番号Ｗ           PIC X(15) VALUE SPACE.
004100       05 分割電話番号Ｗ.
004110          07 分割電話番号１Ｗ          PIC X(5)  VALUE SPACE.
004120          07 分割電話番号２Ｗ          PIC X(5)  VALUE SPACE.
004130          07 分割電話番号３Ｗ          PIC X(5)  VALUE SPACE.
004140*          07 市外局番Ｗ                PIC X(5)  VALUE SPACE.
004150*          07 分割局番Ｗ                PIC X(5)  VALUE SPACE.
004160*          07 分割番号Ｗ                PIC X(5)  VALUE SPACE.
004170    03 資格取得年月日Ｗ.
004180       05 資格取得元号Ｗ               PIC N(2)  VALUE SPACE.
004190       05 資格取得和暦Ｗ               PIC 9(1)  VALUE ZERO.
004200       05 資格取得年Ｗ                 PIC 9(2)  VALUE ZERO.
004210       05 資格取得月Ｗ                 PIC 9(2)  VALUE ZERO.
004220       05 資格取得日Ｗ                 PIC 9(2)  VALUE ZERO.
004230    03 有効期限年月日Ｗ.
004240       05 有効期限元号Ｗ               PIC N(2)  VALUE SPACE.
004250       05 有効期限和暦Ｗ               PIC 9(1)  VALUE ZERO.
004260       05 有効期限年Ｗ                 PIC 9(2)  VALUE ZERO.
004270       05 有効期限月Ｗ                 PIC 9(2)  VALUE ZERO.
004280       05 有効期限日Ｗ                 PIC 9(2)  VALUE ZERO.
004290       05 資格和暦チェックＷ.
004300          07 資格昭和チェックＷ        PIC N(1)  VALUE SPACE.
004310          07 資格平成チェックＷ        PIC N(1)  VALUE SPACE.
004320    03 患者情報Ｗ.
004330       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
004340       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
004350       05 患者性別チェックＷ.
004360          07 患者男チェックＷ          PIC N(1)  VALUE SPACE.
004370          07 患者女チェックＷ          PIC N(1)  VALUE SPACE.
004380       05 続柄Ｗ.
004390          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
004400          07 FILLER                    PIC X(4).
004410       05 続柄チェックＷ.
004420          07 本人チェックＷ            PIC N(1)  VALUE SPACE.
004430          07 家族チェックＷ            PIC N(1)  VALUE SPACE.
004440       05 和暦チェックＷ.
004450          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
004460          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
004470          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
004480          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
004490       05 患者元号Ｗ                   PIC N(2) VALUE SPACE.
004490       05 患者年Ｗ                     PIC 9(2) VALUE ZERO.
004500       05 患者月Ｗ                     PIC 9(2) VALUE ZERO.
004510       05 患者日Ｗ                     PIC 9(2) VALUE ZERO.
004520       05 患者年齢Ｗ                   PIC 9(3) VALUE ZERO.
004530       05 患者性別Ｗ                   PIC N(4) VALUE SPACE.
004540    03 保険種別チェックＷ.
004550       05 健保チェックＷ               PIC N(1) VALUE SPACE.
004560       05 社保チェックＷ               PIC N(1) VALUE SPACE.
004570       05 組合チェックＷ               PIC N(1) VALUE SPACE.
004580       05 日雇チェックＷ               PIC N(1) VALUE SPACE.
004590       05 共済チェックＷ               PIC N(1) VALUE SPACE.
004600       05 自衛官チェックＷ             PIC N(1) VALUE SPACE.
004610       05 国保チェックＷ               PIC N(1) VALUE SPACE.
004620       05 船員チェックＷ               PIC N(1) VALUE SPACE.
004630       05 退職チェックＷ               PIC N(1) VALUE SPACE.
004640*
004650       05 老人チェックＷ               PIC N(1) VALUE SPACE.
004660*
004670       05 公費負担チェックＷ           PIC N(1) VALUE SPACE.
004680       05 福チェックＷ                 PIC N(1) VALUE SPACE.
004690       05 身障チェックＷ               PIC N(1) VALUE SPACE.
004700       05 母子チェックＷ               PIC N(1) VALUE SPACE.
004710       05 乳幼児チェックＷ             PIC N(1) VALUE SPACE.
004720       05 原爆チェックＷ               PIC N(1) VALUE SPACE.
004730       05 国保組合チェックＷ           PIC N(1) VALUE SPACE.
004740       05 生保チェックＷ               PIC N(1) VALUE SPACE.
004750*
004760       05 労災チェックＷ               PIC N(1) VALUE SPACE.
004770       05 自賠チェックＷ               PIC N(1) VALUE SPACE.
004780       05 自費チェックＷ               PIC N(1) VALUE SPACE.
004790*
004800       05 患者郵便番号Ｗ.
004810          07 患者郵便番号１Ｗ          PIC X(3)  VALUE SPACE.
004820          07 患者郵便番号２Ｗ          PIC X(4)  VALUE SPACE.
004830       05 患者住所Ｗ.
004840          07 患者住所１Ｗ              PIC X(40) VALUE SPACE.
004850          07 患者住所２Ｗ              PIC X(40) VALUE SPACE.
004860       05 患者電話番号Ｗ               PIC X(15) VALUE SPACE.
004870    03 負傷原因Ｗ                      PIC X(60) OCCURS 33 VALUE SPACE.
004880**************
004890* 事業所情報 *
004900**************
004910 01 事業所情報Ｗ.
004920    03 事業所名称Ｗ.
004930       05 印刷事業所名称Ｗ             PIC X(60)  VALUE SPACE.
004940    03 事業所郵便番号Ｗ.
004950       05 事業所郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
004960       05 事業所郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
004970*    03 事業所住所Ｗ.
004980*       05 印刷事業所住所Ｗ             PIC X(70)  VALUE SPACE.
004990*       05 FILLER                       PIC X(10).
005000    03 事業所住所Ｗ.
005010       05 印刷事業所住所１Ｗ           PIC X(50)  VALUE SPACE.
005020       05 印刷事業所住所２Ｗ           PIC X(50)  VALUE SPACE.
005030**************
005040* 請求先情報 *
005050**************
005060 01 請求先情報Ｗ.
005070    03 保険者番号Ｗ.
005080       05 印刷保険者番号Ｗ             PIC X(8)  VALUE SPACE.
005090       05 FILLER                       PIC X(2).
005100    03 請求先名称Ｗ.
005110*       05 印刷請求先名称Ｗ             PIC X(40) VALUE SPACE.
005120       05 印刷請求先名称Ｗ             PIC X(54) VALUE SPACE.
005130    03 支部名Ｗ.
005140       05 印刷支部名Ｗ                 PIC X(40) VALUE SPACE.
005150*    03 請求先名支部名Ｗ.
005160*       05 印刷請求先名支部名Ｗ         PIC X(54) VALUE SPACE.
005170*       05 FILLER                       PIC X(26).
005180    03 請求先名支部名Ｗ.
005190       05 印刷請求先名支部名１Ｗ       PIC X(40) VALUE SPACE.
005200       05 印刷請求先名支部名２Ｗ       PIC X(40) VALUE SPACE.
005210    03 保険者呼名Ｗ.
005220*       05 印刷保険者呼名Ｗ             PIC N(7)  VALUE SPACE.
005230       05 印刷保険者呼名Ｗ             PIC X(14)  VALUE SPACE.
005240    03 請求先郵便番号Ｗ.
005250       05 請求先郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
005260       05 請求先郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
005270    03 請求先住所１Ｗ.
005280       05 印刷請求先住所１Ｗ           PIC X(40) VALUE SPACE.
005290    03 請求先住所２Ｗ.
005300       05 印刷請求先住所２Ｗ           PIC X(35) VALUE SPACE.
005310       05 FILLER                       PIC X(5).
005320****************
005330* 詳細用ワーク *
005340****************
005350* 連印－負傷印刷行用ワーク
005360 01 印刷行ＳＷ                           PIC 9(1) VALUE ZERO.
005370*
005380 01 負傷原因情報ＳＷ.
005390    03 負傷原因コードＳＷ  OCCURS   9.
005400       09 負傷患者番号ＳＷ               PIC 9(6) VALUE ZERO.
005410       09 負傷連番ＳＷ                   PIC 9(4) VALUE ZERO.
005420*
005430 01 負傷情報ＳＷ.
005440    03 負傷データＳＷ  OCCURS   9.
005450       05 会部位コードＳＷ               PIC X(4)  VALUE SPACE.
005460       05 部位ＣＮＴＳＷ                 PIC 9(1)  VALUE ZERO.
005470       05 負傷種別ＳＷ                   PIC 9(2)  VALUE ZERO.
005480       05 部位ＳＷ                       PIC 9(2)  VALUE ZERO.
005490       05 左右区分ＳＷ                   PIC 9(1)  VALUE ZERO.
005500       05 負傷位置番号ＳＷ               PIC 9(2)  VALUE ZERO.
005510       05 負傷名ＳＷ                     PIC N(18) VALUE SPACE.
005520       05 負傷年月日ＳＷ.
005530          07 負傷和暦ＳＷ                PIC 9(1)  VALUE ZERO.
005540          07 負傷年ＳＷ                  PIC 9(2)  VALUE ZERO.
005550          07 負傷月ＳＷ                  PIC 9(2)  VALUE ZERO.
005560          07 負傷日ＳＷ                  PIC 9(2)  VALUE ZERO.
005570          07 負傷年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
005580       05 施術開始年月日ＳＷ.
005590          07 施術開始和暦ＳＷ            PIC 9(1)  VALUE ZERO.
005600          07 施術開始年ＳＷ              PIC 9(2)  VALUE ZERO.
005610          07 施術開始月ＳＷ              PIC 9(2)  VALUE ZERO.
005620          07 施術開始日ＳＷ              PIC 9(2)  VALUE ZERO.
005630          07 施開年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
005640       05 施術終了年月日ＳＷ.
005650          07 施術終了和暦ＳＷ            PIC 9(1)  VALUE ZERO.
005660          07 施術終了年ＳＷ              PIC 9(2)  VALUE ZERO.
005670          07 施術終了月ＳＷ              PIC 9(2)  VALUE ZERO.
005680          07 施術終了日ＳＷ              PIC 9(2)  VALUE ZERO.
005690          07 施終年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
005700       05 開始年月日取得フラグＳ         PIC X(3)  VALUE SPACE.
005710       05 転帰ＳＷ                       PIC N(4)  VALUE SPACE.
005720       05 転帰チェックＳＷ.
005730          07 治癒チェックＳＷ            PIC N(1)  VALUE SPACE.
005740          07 中止チェックＳＷ            PIC N(1)  VALUE SPACE.
005750          07 転医チェックＳＷ            PIC N(1)  VALUE SPACE.
005760       05 施術累計回数ＳＷ               PIC 9(4)  VALUE ZERO.
005770****************
005780* 負傷データＦ *
005790****************
005800 01 負傷情報Ｗ.
005810    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
005820    03 負傷データ情報Ｗ  OCCURS   9.
005830       05 会部位コードＷ               PIC X(4)  VALUE SPACE.
005840       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
005850       05 負傷種別Ｗ                   PIC 9(2)  VALUE ZERO.
005860       05 部位Ｗ                       PIC 9(2)  VALUE ZERO.
005870       05 左右区分Ｗ                   PIC 9(1)  VALUE ZERO.
005880       05 負傷位置番号Ｗ               PIC 9(2)  VALUE ZERO.
005890       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
005900       05 負傷年月日Ｗ.
005910          07 負傷和暦Ｗ                PIC 9(1)  VALUE ZERO.
005920          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
005930          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
005940          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
005950          07 負傷年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005960       05 施術開始年月日Ｗ.
005970          07 施術開始和暦Ｗ            PIC 9(1)  VALUE ZERO.
005980          07 施術開始年Ｗ              PIC 9(2)  VALUE ZERO.
005990          07 施術開始月Ｗ              PIC 9(2)  VALUE ZERO.
006000          07 施術開始日Ｗ              PIC 9(2)  VALUE ZERO.
006010          07 施開年月日区切Ｗ          PIC X(1)  VALUE SPACE.
006020       05 施術終了年月日Ｗ.
006030          07 施術終了和暦Ｗ            PIC 9(1)  VALUE ZERO.
006040          07 施術終了年Ｗ              PIC 9(2)  VALUE ZERO.
006050          07 施術終了月Ｗ              PIC 9(2)  VALUE ZERO.
006060          07 施術終了日Ｗ              PIC 9(2)  VALUE ZERO.
006070          07 施終年月日区切Ｗ          PIC X(1)  VALUE SPACE.
006080       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
006090       05 転帰Ｗ                       PIC N(4)  VALUE SPACE.
006100       05 転帰チェックＷ.
006110          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
006120          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
006130          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
006140*
006150******************
006160* 負担率チェック *
006170******************
006180 01 負担率チェックＷ.
006190    03 ０割チェックＷ                  PIC N(1)  VALUE SPACE.
006200    03 １割チェックＷ                  PIC N(1)  VALUE SPACE.
006210    03 ２割チェックＷ                  PIC N(1)  VALUE SPACE.
006220    03 ３割チェックＷ                  PIC N(1)  VALUE SPACE.
006230**********
006240* 合計行 *
006250**********
006260 01 合計行Ｗ.
006270    03 請求終了日Ｗ                    PIC 9(2) VALUE ZERO.
006280    03 対象西暦Ｗ                      PIC 9(4) VALUE ZERO.
006290    03 商Ｗ                            PIC 9(3) VALUE ZERO.
006300    03 余Ｗ                            PIC 9(3) VALUE ZERO.
006310*/年月毎持つ保険者データのワーク
006320 01 保負担率区分Ｗ                     PIC 9    VALUE ZERO.
006330 01 保本人負担率Ｗ                     PIC 9(3) VALUE ZERO.
006340 01 保家族負担率Ｗ                     PIC 9(3) VALUE ZERO.
006350 01 保本人負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
006360 01 保家族負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
006370*
006380 01 比較和暦年月Ｗ.
006390    03 比較和暦Ｗ                      PIC 9    VALUE ZERO.
006400    03 比較年Ｗ                        PIC 9(2) VALUE ZERO.
006410    03 比較月Ｗ                        PIC 9(2) VALUE ZERO.
006420*
006430*/老人保険改正の対応0101
006440 01 老人負担計算区分Ｗ                 PIC 9    VALUE ZERO.
006450 01 負担率用種別Ｗ                     PIC 9(2) VALUE ZERO.
006460 01 助成負担区分Ｗ                     PIC 9(2) VALUE ZERO.
006470*
006480** 助成負担率用(14/10～)
006490 01 助成負担率ＷＷ.
006500    03 助成負担率Ｗ                    PIC 9(3) VALUE ZERO.
006510    03 助成負担率Ｗ１                  PIC 9    VALUE ZERO.
006520    03 助成負担率文字Ｗ１              PIC X    VALUE SPACE.
006530    03 助成負担率表示Ｗ                PIC X(6) VALUE SPACE.
006540*
006550******************************************************************
006560 01 印刷制御.
006570     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
006580     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
006590     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
006600     03 拡張制御Ｐ.
006610         05 端末制御Ｐ.
006620             07 移動方向Ｐ             PIC X(1)  VALUE SPACE.
006630             07 移動行数Ｐ             PIC 9(3)  VALUE ZERO.
006640         05 詳細制御Ｐ                 PIC X(2)  VALUE SPACE.
006650     03 通知情報Ｐ                     PIC X(2)  VALUE SPACE.
006660     03 ユニット名Ｐ                   PIC X(8)  VALUE SPACE.
006670*
006680 01 計算機西暦年Ｗ                     PIC 9(2)  VALUE ZERO.
006690* 日付ＷＯＲＫ
006700 01 和暦終了年Ｗ                       PIC 9(4)  VALUE ZERO.
006710 01 計算機西暦.
006720    03 計算機西暦年                    PIC 9(4)  VALUE ZERO.
006730    03 計算機西暦月日                  PIC 9(4)  VALUE ZERO.
006740 01 計算機西暦Ｒ REDEFINES 計算機西暦.
006750    03 計算機世紀                      PIC 9(2).
006760    03 計算機日付                      PIC 9(6).
006770    03 計算機日付Ｒ REDEFINES 計算機日付.
006780       05 計算機年月                   PIC 9(4).
006790       05 計算機年月Ｒ REDEFINES 計算機年月.
006800         07 計算機年                   PIC 9(2).
006810         07 計算機月                   PIC 9(2).
006820       05 計算機日                     PIC 9(2).
006830*
006840* C 連携用
006850 01  文字１Ｗ        PIC X(4096).
006860 01  文字２Ｗ        PIC X(512).
006870 01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
006880*
006890 01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
006900******************************************************************
006910*                          連結項目                              *
006920******************************************************************
006930*
006940**********************
006950* メッセージ表示キー *
006960**********************
006970 01 連メ－キー IS EXTERNAL.
006980    03  連メ－メッセージ                 PIC N(20).
006990*
007000****************
007010* 連結項目待避 *
007020****************
007030*    ************
007040*    * 料金情報 *
007050*    ************
007060*    月毎の料金
007070***********************
007080 01 連施－料金１ IS EXTERNAL.
007090   03 連施－本人負担率                 PIC 9(3).
007100************
007110* 印刷キー *
007120************
007130 01 連印－対象データ IS EXTERNAL.
007140    03 連印－施術和暦年月.
007150       05 連印－施術和暦                  PIC 9(1).
007160       05 連印－施術年                    PIC 9(2).
007170       05 連印－施術月                    PIC 9(2).
007180    03 連印－保険種別                     PIC 9(2).
007190    03 連印－保険者番号                   PIC X(10).
007200    03 連印－本人家族区分                 PIC 9(1).
007210    03 連印－被保険者カナ                 PIC X(20).
007220    03 連印－患者コード.
007230       05 連印－患者番号                  PIC 9(6).
007240       05 連印－枝番                      PIC X(1).
007250    03 連印－印刷モードＦ                 PIC 9(1).
007260*/詳細
007270    03 連印－保険証印刷区分               PIC 9(1).
007280    03 連印－負傷詳細 OCCURS 7.
007290       05 連印－負傷印刷行                PIC 9(1).
007300       05 連印－部位印刷区分              PIC 9(1).
007310       05 連印－転帰印刷区分              PIC 9(1).
007320       05 連印－原因印刷区分              PIC 9(1).
007330*/
007340 01 連印－対象データ追加 IS EXTERNAL GLOBAL.
007350    03 連印－部位丸付け                   PIC 9(1).
007360*/年齢印刷0304
007370*/入力画面(660,6601)と新用紙(6621)との連結項目。(旧用紙、特別用紙等のＰＧを変えたく無い為、新たに作ります)
007380    03 連印－年齢印刷区分                 PIC 9(1).
007390*/負傷原因の詳細モード対応/0610
007400    03 連印－原因開始行                   PIC 9(2).
007410*/相談支援記載追加/080812
007420    03 連印－相談支援記載                 PIC 9(1).
007430*/ワクごと印刷 ０：枠なし １：ワクあり ２：ワクのみ /0402
007440    03 連印－枠印刷Ｆ                     PIC 9(1).
007450*/長期理由0311
007460    03 連印－長期印刷Ｆ                   PIC 9(1).
007470*
007480 01 連入－表示フラグ６６０ IS EXTERNAL GLOBAL.
007490    03 連入－プレビュー区分               PIC 9(1).
007500* 負担率取得用14/10～
007510 01 連率－負担率取得キー IS EXTERNAL.
007520    03 連率－施術和暦年月.
007530       05 連率－施術和暦               PIC 9.
007540       05 連率－施術年月.
007550          07 連率－施術年              PIC 9(2).
007560          07 連率－施術月              PIC 9(2).
007570    03 連率－患者コード.
007580       05 連率－患者番号               PIC 9(6).
007590       05 連率－枝番                   PIC X.
007600    03 連率－実際負担率                PIC 9(3).
007610    03 連率－実際本体負担率            PIC 9(3).
007620    03 連率－健保負担率                PIC 9(3).
007630    03 連率－２７老負担率              PIC 9(3).
007640    03 連率－助成負担率                PIC 9(3).
007650    03 連率－特別用負担率              PIC 9(3).
007660*
007670************************************
007680* プリンタファイル作成用           *
007690************************************
007700 01 Ｈ連ＰＲＴＦ－作成データ IS EXTERNAL.
007710     03 Ｈ連ＰＲＴＦ－ファイル名           PIC X(8).
007720     03 Ｈ連ＰＲＴＦ－プレビュー区分       PIC 9.
007730     03 Ｈ連ＰＲＴＦ－帳票プログラム名     PIC X(8).
007740     03 Ｈ連ＰＲＴＦ－オーバレイ名         PIC X(8).
007750************************************
007760* プリンタファイル作成特殊用       *
007770************************************
007780 01 Ｈ連特殊ＰＲＴＦ－作成データ IS EXTERNAL.
007790     03 Ｈ連特殊ＰＲＴＦ－用紙種類         PIC X(8).
007800*
007810* 暗号複合用
007820 01 連暗号複合－暗号情報 IS EXTERNAL.
007830    03 連暗号複合－入力情報.
007840       05 連暗号複合－記号               PIC X(24).
007850       05 連暗号複合－番号               PIC X(30).
007860       05 連暗号複合－暗号化項目.
007870         07 連暗号複合－暗号患者番号     PIC X(6).
007880         07 連暗号複合－暗号判定記号     PIC X.
007890         07 連暗号複合－暗号判定番号     PIC X.
007900         07 連暗号複合－暗号記号         PIC X(24).
007910         07 連暗号複合－暗号番号         PIC X(30).
007920    03 連暗号複合－出力情報.
007930       05 連暗号複合－複合した記号       PIC X(24).
007940       05 連暗号複合－複合した番号       PIC X(30).
007950* 
007960******************************************************************
007970*                      PROCEDURE  DIVISION                       *
007980******************************************************************
007990 PROCEDURE               DIVISION.
008000************
008010*           *
008020* 初期処理   *
008030*           *
008040************
008050     PERFORM プリンタファイル作成.
008060     PERFORM 初期化.
008070************
008080*           *
008090* 主処理     *
008100*           *
008110************
008120* 印刷
008130     PERFORM 連結項目待避.
008140     PERFORM 印刷セット
008150     PERFORM 印刷処理
008160************
008170*           *
008180* 終了処理   *
008190*           *
008200************
008210     PERFORM 終了処理.
008220     EXIT PROGRAM.
008230*
008240*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
008250*================================================================*
008260 プリンタファイル作成 SECTION.
008270*================================================================*
008280*   / 初期化 /
008290     MOVE SPACE TO Ｈ連ＰＲＴＦ－作成データ.
008300     INITIALIZE Ｈ連ＰＲＴＦ－作成データ.
008310     MOVE SPACE TO Ｈ連特殊ＰＲＴＦ－作成データ.
008320     INITIALIZE Ｈ連特殊ＰＲＴＦ－作成データ.
008330*
008340*
008350*--↓↓ 変更箇所 ↓↓--------------------------------------*
008360*   使用する用紙種別セット
008370     MOVE "KARUTE"              TO Ｈ連特殊ＰＲＴＦ－用紙種類.
008380*   使用するプリンタファイル名セット
008390     MOVE "PRTF002"             TO Ｈ連ＰＲＴＦ－ファイル名.
008400*
008410*   使用する帳票プログラム名セット
008420     MOVE "YAS662"              TO Ｈ連ＰＲＴＦ－帳票プログラム名.
008430*
008440*--↑↑-----------------------------------------------------*
008450*
008460*   / プレビュー区分セット /
008470     MOVE 連入－プレビュー区分 TO Ｈ連ＰＲＴＦ－プレビュー区分.
008480*     MOVE 1 TO Ｈ連ＰＲＴＦ－プレビュー区分.
008490*
008500     CALL   "CRTPRTF".
008510     CANCEL "CRTPRTF".
008520*
008530*================================================================*
008540 初期化 SECTION.
008550*
008560     PERFORM ファイルオープン.
008570*    /* 現在日付取得 */
008580     ACCEPT 計算機日付 FROM DATE.
008590*    /* 1980～2079年の間で設定 */
008600     IF 計算機年 > 80
008610         MOVE 19 TO 計算機世紀
008620     ELSE
008630         MOVE 20 TO 計算機世紀
008640     END-IF.
008650     PERFORM カレント元号取得.
008660     PERFORM 和暦終了年取得.
008670     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
008680*================================================================*
008690 カレント元号取得 SECTION.
008700*
008710     MOVE ZEROS TO 制－制御区分.
008720     READ 制御情報マスタ
008730     NOT INVALID KEY
008740         MOVE 制－カレント元号 TO カレント元号Ｗ
008750         MOVE 制－カルテ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
008760         MOVE 制－カルテ用紙種別 TO カルテ用紙種別Ｗ
008761     END-READ.
008770*
008780*================================================================*
008790 和暦終了年取得 SECTION.
008800*
008810     MOVE カレント元号Ｗ TO 元－元号区分.
008820     READ 元号マスタ
008830     INVALID KEY
008840         DISPLAY NC"指定和暦が登録されていません" UPON CONS
008850         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008860                                                  UPON CONS
008870*-----------------------------------------*
008880         CALL "actcshm"  WITH C LINKAGE
008890*-----------------------------------------*
008900         ACCEPT  キー入力 FROM CONS
008910         PERFORM 終了処理
008920         EXIT PROGRAM
008930     NOT INVALID KEY
008940         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
008950         MOVE 前和暦Ｗ TO 元－元号区分
008960         READ 元号マスタ
008970         INVALID KEY
008980             DISPLAY NC"指定和暦が登録されていません" UPON CONS
008990             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
009000                                                      UPON CONS
009010*-----------------------------------------*
009020             CALL "actcshm"  WITH C LINKAGE
009030*-----------------------------------------*
009040             ACCEPT  キー入力 FROM CONS
009050             PERFORM 終了処理
009060             EXIT PROGRAM
009070         NOT INVALID KEY
009080             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
009090         END-READ
009100     END-READ.
009110*
009120*================================================================*
009130 連結項目待避 SECTION.
009140*
009150     INITIALIZE 対象データＷＲ.
009160     MOVE 連印－施術和暦      TO 施術和暦ＷＲ.
009170     MOVE 連印－施術年        TO 施術年ＷＲ.
009180     MOVE 連印－施術月        TO 施術月ＷＲ.
009190     MOVE 連印－保険種別      TO 保険種別ＷＲ.
009200     MOVE 連印－保険者番号    TO 保険者番号ＷＲ.
009210     MOVE 連印－本人家族区分  TO 本人家族区分ＷＲ.
009220     MOVE 連印－被保険者カナ  TO 被保険者カナＷＲ.
009230     MOVE 連印－患者番号      TO 患者番号ＷＲ.
009240     MOVE 連印－枝番          TO 枝番ＷＲ.
009250     MOVE 連印－印刷モードＦ  TO 印刷モードＦＷＲ.
009260*================================================================*
009270 ファイルオープン SECTION.
009280*
009290     OPEN INPUT   保険者マスタ
009300         MOVE NC"保険者" TO ファイル名.
009310         PERFORM オープンチェック.
009320     OPEN INPUT   元号マスタ
009330         MOVE NC"元号" TO ファイル名.
009340         PERFORM オープンチェック.
009350     OPEN INPUT   名称マスタ
009360         MOVE NC"名称" TO ファイル名.
009370         PERFORM オープンチェック.
009380     OPEN INPUT   レセプトＦ
009390         MOVE NC"レセ" TO ファイル名.
009400         PERFORM オープンチェック.
009410     OPEN INPUT   制御情報マスタ
009420         MOVE NC"制御情報" TO ファイル名.
009430         PERFORM オープンチェック.
009440     OPEN INPUT   請求先マスタ
009450         MOVE NC"請先" TO ファイル名.
009460         PERFORM オープンチェック.
009470     OPEN INPUT   受診者情報Ｆ.
009480         MOVE NC"受情" TO ファイル名.
009490         PERFORM オープンチェック.
009500     OPEN INPUT   施術記録Ｆ.
009510         MOVE NC"施記Ｆ" TO ファイル名.
009520         PERFORM オープンチェック.
009530     OPEN INPUT   負傷データＦ.
009540         MOVE NC"負傷" TO ファイル名.
009550         PERFORM オープンチェック.
009560     OPEN INPUT   負傷原因Ｆ.
009570         MOVE NC"負傷原因" TO ファイル名.
009580         PERFORM オープンチェック.
009590     OPEN INPUT 市町村マスタ.
009600         MOVE NC"市町村" TO ファイル名.
009610         PERFORM オープンチェック.
009620     OPEN INPUT 生保情報Ｆ.
009630         MOVE NC"生保" TO ファイル名.
009640         PERFORM オープンチェック.
009650     OPEN INPUT 事業所マスタ.
009660         MOVE NC"事業所" TO ファイル名.
009670         PERFORM オープンチェック.
009680     OPEN INPUT 労災情報Ｆ.
009690         MOVE NC"労災情報Ｆ" TO ファイル名.
009700         PERFORM オープンチェック.
009290     OPEN INPUT 自賠責情報Ｆ.
009300         MOVE NC"自賠" TO ファイル名.
009310         PERFORM オープンチェック.
009290     OPEN INPUT 保険会社マスタ.
009300         MOVE NC"保会" TO ファイル名.
009310         PERFORM オープンチェック.
009710     OPEN I-O   印刷ファイル
009720         MOVE NC"印刷" TO ファイル名.
009730         PERFORM エラー処理Ｐ.
009740*================================================================*
009750 オープンチェック SECTION.
009760*
009770     IF 状態キー  NOT =  "00"
009780         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
009790         DISPLAY NC"状態キー：" 状態キー         UPON CONS
009800         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
009810                                                 UPON CONS
009820*-----------------------------------------*
009830         CALL "actcshm"  WITH C LINKAGE
009840*-----------------------------------------*
009850         ACCEPT  キー入力 FROM CONS
009860         PERFORM ファイル閉鎖
009870         EXIT PROGRAM.
009880*================================================================*
009890 ファイル閉鎖 SECTION.
009900*
009910     CLOSE 印刷ファイル   保険者マスタ     元号マスタ
009920           制御情報マスタ 請求先マスタ     名称マスタ
009930           受診者情報Ｆ   施術記録Ｆ       生保情報Ｆ
009940           負傷データＦ   負傷原因Ｆ       レセプトＦ
009950           市町村マスタ   事業所マスタ     労災情報Ｆ
                 自賠責情報Ｆ   保険会社マスタ.
009960*================================================================*
009970 終了処理 SECTION.
009980*
009990     PERFORM ファイル閉鎖.
010000*================================================================*
010010 印刷セット SECTION.
010020*
010030     EVALUATE 印刷モードＦＷＲ
010040     WHEN 0
010050         PERFORM 印刷セット１
010060     WHEN 1
010070         PERFORM 印刷セット２
010080     WHEN 2
010090         PERFORM 印刷セット３
010100*/詳細モード用
010110     WHEN 4
010120         PERFORM 印刷セット５
010130     END-EVALUATE.
010140*
010150*================================================================*
010160 印刷セット１ SECTION.
010170*
010180     INITIALIZE YAS662P.
010190     INITIALIZE 受診者情報Ｗ.
010200     INITIALIZE 事業所情報Ｗ.
010210     INITIALIZE 請求先情報Ｗ.
010220     INITIALIZE 負傷情報Ｗ.
010230     INITIALIZE 負担率チェックＷ.
010240     PERFORM 負担率取得.
010250     PERFORM 請求先情報取得.
010260     PERFORM 受診者情報取得.
010270     PERFORM 負傷データ取得.
010280*
010290     IF 負傷原因印刷区分Ｗ  NOT = 1 
010300        PERFORM 負傷原因取得
010310     END-IF.
010320*
010330     PERFORM 印刷上部セット.
010340     PERFORM 印刷下部セット.
010350*================================================================*
010360 印刷セット２ SECTION.
010370*
010380     INITIALIZE YAS662P.
010390     INITIALIZE 受診者情報Ｗ.
010400     INITIALIZE 事業所情報Ｗ.
010410     INITIALIZE 請求先情報Ｗ.
010420     INITIALIZE 負傷情報Ｗ.
010430     INITIALIZE 負担率チェックＷ.
010440     PERFORM 負担率取得.
010450     PERFORM 請求先情報取得.
010460     PERFORM 受診者情報取得.
010470     PERFORM 印刷上部セット.
010480*================================================================*
010490 印刷セット３ SECTION.
010500*
010510     INITIALIZE YAS662P.
010520     INITIALIZE 受診者情報Ｗ.
010530     INITIALIZE 事業所情報Ｗ.
010540     INITIALIZE 請求先情報Ｗ.
010550     INITIALIZE 負傷情報Ｗ.
010560     PERFORM 受診者情報取得.
010570     PERFORM 負傷データ取得.
010580*
010590     IF 負傷原因印刷区分Ｗ  NOT = 1 
010600        PERFORM 負傷原因取得
010610     END-IF.
010620*
010630     PERFORM 印刷下部セット.
010640*================================================================*
010650 印刷セット５ SECTION.
010660*
010670*/詳細モード用
010680     INITIALIZE YAS662P.
010690     INITIALIZE 受診者情報Ｗ.
010700     INITIALIZE 事業所情報Ｗ.
010710     INITIALIZE 請求先情報Ｗ.
010720     INITIALIZE 負傷情報Ｗ.
010730     INITIALIZE 負傷原因情報Ｗ.
010740***     PERFORM 負担率取得.
010750     PERFORM 負担率取得.
010760     PERFORM 請求先情報取得.
010770     PERFORM 受診者情報取得.
010780     PERFORM 負傷データ取得.
010790*
010800     IF 負傷原因印刷区分Ｗ  NOT = 1
010810        PERFORM 詳細モード負傷原因処理
010820        PERFORM 負傷原因取得
010830     END-IF.
010840*/負傷原因の詳細印刷/0610
010850     PERFORM 原因行セット.
010860*
010870     IF 連印－保険証印刷区分 = 1
010880         PERFORM 印刷上部セット
010890     END-IF.
010900*
010910     PERFORM 詳細モード負傷処理.
010920     PERFORM 印刷下部セット.
010930*
010940*================================================================*
010950 印刷上部セット SECTION.
010960*
010970     MOVE 患者コードＷ               TO 患者コード.
010980     MOVE 患者番号Ｗ                 TO 患者番号.
010990     MOVE 枝番Ｗ                     TO 枝番.
011000**************************
011010* 被保険者証情報セット   *
011020**************************
011030     IF 記号Ｗ(1:1) = "＊" 
011040        MOVE  SPACE    TO  記号
011050     ELSE
011060        MOVE 記号Ｗ    TO  記号
011070     END-IF.
011080     IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
011090        ( 印刷番号Ｗ(1:2) = "＊" )
011100        MOVE  SPACE      TO  番号
011110     ELSE
011120        MOVE 印刷番号Ｗ  TO  番号
011130     END-IF.
011140************************
011150* 被保険者情報セット   *
011160************************
011170     MOVE 被保険者氏名Ｗ             TO 被保険者氏名.
011180     MOVE 被保険者カナＷ             TO 被保険者カナ.
011190*     MOVE 被保険者男チェックＷ       TO 被保険者男チェック.
011200*     MOVE 被保険者女チェックＷ       TO 被保険者女チェック.
011210     MOVE 被保険者郵便番号１Ｗ       TO 被保険者郵便番号１.
011220     MOVE 被保険者郵便番号２Ｗ       TO 被保険者郵便番号２.
011230     MOVE "-"                        TO 被保険者郵便区切.
011240*     MOVE 被保険者住所Ｗ             TO 被保険者住所.
011250     MOVE 印刷被保険者住所１Ｗ       TO 被保険者住所１.
011260     MOVE 印刷被保険者住所２Ｗ       TO 被保険者住所２.
011270*     MOVE 被保険者電話番号Ｗ         TO 被保険者電話番号.
011270     MOVE 分割電話番号１Ｗ           TO 被保険者電話番号１.
011270     MOVE 分割電話番号２Ｗ           TO 被保険者電話番号２.
011270     MOVE 分割電話番号３Ｗ           TO 被保険者電話番号３.
011290     MOVE 資格取得年Ｗ               TO 資格取得年.
011300     MOVE 資格取得月Ｗ               TO 資格取得月.
011310     MOVE 資格取得日Ｗ               TO 資格取得日.
012581     IF カルテ用紙種別Ｗ = 3
011280        MOVE 資格取得元号Ｗ          TO 資格取得和暦
           ELSE
011320        MOVE 資格昭和チェックＷ      TO 資格昭和チェック
011330        MOVE 資格平成チェックＷ      TO 資格平成チェック
           END-IF.
011340*/有効期限
011350     MOVE 有効期限年Ｗ               TO 有効年.
011360     MOVE 有効期限月Ｗ               TO 有効月.
011370     MOVE 有効期限日Ｗ               TO 有効日.
012581     IF カルテ用紙種別Ｗ = 3
011470        MOVE 有効期限元号Ｗ          TO 有効和暦
           END-IF.
011380*    IF ( 有効期限年Ｗ = ZERO ) AND
011390*       ( 有効期限月Ｗ = ZERO ) AND
011400*       ( 有効期限日Ｗ = ZERO )
011410*        MOVE SPACE          TO 有効元号
011420*        MOVE SPACE          TO 有効年月日表示
011430*        MOVE SPACE          TO 有効年表示
011440*        MOVE SPACE          TO 有効月表示
011450*        MOVE SPACE          TO 有効日表示
011460*    ELSE
011470*        MOVE 有効期限元号Ｗ TO 有効元号
011480*        MOVE NC"有効年月日" TO 有効年月日表示
011490*        MOVE NC"年"         TO 有効年表示
011500*        MOVE NC"月"         TO 有効月表示
011510*        MOVE NC"日"         TO 有効日表示
011520*    END-IF.
012581     IF カルテ用紙種別Ｗ = 3
011280        MOVE 被保険者元号Ｗ          TO 被保険者和暦
           ELSE
011530        MOVE 被保険者明治チェックＷ  TO 被保険者明治チェック
011540        MOVE 被保険者大正チェックＷ  TO 被保険者大正チェック
011550        MOVE 被保険者昭和チェックＷ  TO 被保険者昭和チェック
011560        MOVE 被保険者平成チェックＷ  TO 被保険者平成チェック
           END-IF.
011570     MOVE 被保険者年Ｗ               TO 被保険者年.
011580     MOVE 被保険者月Ｗ               TO 被保険者月.
011590     MOVE 被保険者日Ｗ               TO 被保険者日.
011600*    MOVE 被保険者年齢Ｗ             TO 被保険者年齢.
011610********************
011620* 患者情報セット   *
011630********************
011640     MOVE ０割チェックＷ             TO ０割チェック.
011650     MOVE １割チェックＷ             TO １割チェック.
011660     MOVE ２割チェックＷ             TO ２割チェック.
011670     MOVE ３割チェックＷ             TO ３割チェック.
011680*
011690* 14/10～ (助成の負担率）
011700     MOVE 助成負担率表示Ｗ           TO 助成負担率.
011710*
011720     IF  印刷市町村番号Ｗ(1:2) = "99"
011730         MOVE SPACE                  TO 市町村番号
011740     ELSE
011750         MOVE 印刷市町村番号Ｗ       TO 市町村番号
011760     END-IF.
011770     IF (印刷受益者番号Ｗ(1:1) = "*") OR
011780        (印刷受益者番号Ｗ(1:2) = "＊") 
011790         MOVE SPACE                  TO 受益者番号
011800     ELSE
011810         MOVE 印刷受益者番号Ｗ       TO 受益者番号
011820     END-IF
011830*
011840     IF  印刷費用負担者番号Ｗ(1:2) = "99"
011850         MOVE SPACE                  TO 公費負担者番号
011860     ELSE
011870         MOVE 印刷費用負担者番号Ｗ   TO 公費負担者番号
011880     END-IF.
011890*
011900     IF (印刷受益者番号助成Ｗ(1:1) = "*") OR
011910        (印刷受益者番号助成Ｗ(1:2) = "＊")
011920         MOVE SPACE                  TO 受益者番号助成
011930     ELSE
011940         MOVE 印刷受益者番号助成Ｗ   TO 受益者番号助成
011950     END-IF.
011960     MOVE 患者カナＷ                 TO 患者カナ.
011970     MOVE 患者氏名Ｗ                 TO 患者氏名.
011980     MOVE 患者男チェックＷ           TO 患者男チェック.
011990     MOVE 患者女チェックＷ           TO 患者女チェック.
012581     IF カルテ用紙種別Ｗ = 3
011280        MOVE 患者元号Ｗ              TO 患者和暦
           ELSE
012000        MOVE 明治チェックＷ          TO 明治チェック
012010        MOVE 大正チェックＷ          TO 大正チェック
012020        MOVE 昭和チェックＷ          TO 昭和チェック
012030        MOVE 平成チェックＷ          TO 平成チェック
           END-IF.
012040     MOVE 患者年Ｗ                   TO 患者年.
012050     MOVE 患者月Ｗ                   TO 患者月.
012060     MOVE 患者日Ｗ                   TO 患者日.
012070*     MOVE 患者年齢Ｗ                 TO 患者年齢.
012080     MOVE 本人チェックＷ             TO 本人チェック.
012090     MOVE 家族チェックＷ             TO 家族チェック.
012100     MOVE 印刷続柄Ｗ                 TO 続柄.
012110*     MOVE 患者郵便番号１Ｗ           TO 患者郵便番号１.
012120*     MOVE 患者郵便番号２Ｗ           TO 患者郵便番号２.
012130*     MOVE "-"                        TO 患者郵便区切.
012140*     MOVE 患者住所Ｗ                 TO 患者住所.
012150*     MOVE 患者電話番号Ｗ             TO 患者電話番号.
012160*     MOVE 患者住所１Ｗ               TO 患者住所１.
012170*     MOVE 患者住所２Ｗ               TO 患者住所２.
012180********************
012190* 事業所情報セット *
012200********************
012210     MOVE 事業所名称Ｗ               TO 事業所名称.
012220     MOVE 事業所郵便番号１Ｗ         TO 事業所郵便番号１.
012230     MOVE 事業所郵便番号２Ｗ         TO 事業所郵便番号２.
012240*     MOVE 印刷事業所住所Ｗ           TO 事業所住所.
012250     MOVE 印刷事業所住所１Ｗ         TO 事業所住所１.
012260     MOVE 印刷事業所住所２Ｗ         TO 事業所住所２.
012270************************************
012280* 都道府県・健康保険組合情報セット *
012290************************************
012300*労災、自費の時は保険者情報は印字しない
012310     IF 保険種別ＷＲ = 70 OR 90
012320         MOVE SPACE                  TO 保険者番号
012330*         MOVE SPACE                  TO 請求先名支部名
012340         MOVE SPACE                  TO 請求先名支部名１
012350         MOVE SPACE                  TO 請求先名支部名２
012360         MOVE SPACE                  TO 保険者住所１
012370         MOVE SPACE                  TO 保険者住所２
012380     ELSE
012390         MOVE 印刷保険者番号Ｗ       TO 保険者番号
012400*         MOVE 印刷請求先名支部名Ｗ   TO 請求先名支部名
012410         MOVE 印刷請求先名支部名１Ｗ TO 請求先名支部名１
012420         MOVE 印刷請求先名支部名２Ｗ TO 請求先名支部名２
012430         MOVE 請求先住所１Ｗ         TO 保険者住所１
012440         MOVE 請求先住所２Ｗ         TO 保険者住所２
012450     END-IF.
012460**************************
012470* 保険種別チェックセット *
012480**************************
012490     MOVE 健保チェックＷ             TO 健保チェック.
012500     MOVE 社保チェックＷ             TO 社保チェック.
012510     MOVE 組合チェックＷ             TO 組合チェック.
012520     MOVE 日雇チェックＷ             TO 日雇チェック.
012530     MOVE 船員チェックＷ             TO 船員チェック.
012540     MOVE 共済チェックＷ             TO 共済チェック.
012550     MOVE 自衛官チェックＷ           TO 自衛官チェック.
012560     MOVE 国保チェックＷ             TO 国保チェック.
012570     MOVE 退職チェックＷ             TO 退職チェック.
012580     MOVE 老人チェックＷ             TO 老人チェック.
012581     IF カルテ用紙種別Ｗ = 2 OR 3
012582        MOVE 労災チェックＷ          TO 公費負担チェック
012583     ELSE
012590        MOVE 公費負担チェックＷ      TO 公費負担チェック
012591     END-IF.
012600*     MOVE 乳幼児チェックＷ           TO 乳幼児チェック.
012610*     MOVE 福チェックＷ               TO 福チェック.
012620*     MOVE 身障チェックＷ             TO 身障チェック.
012630*     MOVE 母子チェックＷ             TO 母子チェック.
012640*     MOVE 原爆チェックＷ             TO 原爆チェック.
012650     MOVE 生保チェックＷ             TO 生保チェック.
012660     MOVE 自賠チェックＷ             TO 自賠チェック.
012670     MOVE 自費チェックＷ             TO 自費チェック.
012680* テスト
012690***     MOVE ALL "X" TO 被保険者住所１ 被保険者住所２
012700***                     被保険者電話番号
012710***                     請求先名支部名１ 請求先名支部名２
012720***                     保険者住所１ 保険者住所２ 事業所名称
012730***                     事業所住所１ 公費負担者番号 受益者番号助成 
012740***                     保険者番号.
012750***     MOVE ALL "9" TO 有効年 有効月 有効日.
012760*================================================================*
012770 印刷下部セット SECTION.
012780*
012790********************
012800* 負傷データセット *
012810********************
012820* １部位 *
012830**********
012840*     MOVE 会部位コードＷ(1)   TO 負傷コード１.
012850     MOVE 負傷名Ｗ(1)         TO 負傷名１.
012860     MOVE 負傷年Ｗ(1)         TO 負傷年１.
012870     MOVE 負傷月Ｗ(1)         TO 負傷月１.
012880     MOVE 負傷日Ｗ(1)         TO 負傷日１.
012890     MOVE 施術開始年Ｗ(1)     TO 施術開始年１.
012900     MOVE 施術開始月Ｗ(1)     TO 施術開始月１.
012910     MOVE 施術開始日Ｗ(1)     TO 施術開始日１.
012920     MOVE 施術終了年Ｗ(1)     TO 施術終了年１.
012930     MOVE 施術終了月Ｗ(1)     TO 施術終了月１.
012940     MOVE 施術終了日Ｗ(1)     TO 施術終了日１.
012950*     MOVE 負傷年月日区切Ｗ(1) TO 区切１１ 区切１２.
012960*     MOVE 施開年月日区切Ｗ(1) TO 区切１３ 区切１４.
012970*     MOVE 施終年月日区切Ｗ(1) TO 区切１５ 区切１６.
012980*     MOVE 転帰Ｗ(1)           TO 転帰１.
012990     MOVE 治癒チェックＷ(1)   TO 治癒チェック１.
013000     MOVE 中止チェックＷ(1)   TO 中止チェック１.
013010     MOVE 転医チェックＷ(1)   TO 転医チェック１.
013020*     ******************
013030*     * 日数は一時保留 *
013040*     ******************
013050*     **********************
013060*     * 施術回数は一時保留 *
013070*     **********************
013080*     **********************
013090*     * 転帰名称は一時保留 *
013100*     **********************
013110**********
013120* ２部位 *
013130**********
013140*     MOVE 会部位コードＷ(2)   TO 負傷コード２.
013150     MOVE 負傷名Ｗ(2)         TO 負傷名２.
013160     MOVE 負傷年Ｗ(2)         TO 負傷年２.
013170     MOVE 負傷月Ｗ(2)         TO 負傷月２.
013180     MOVE 負傷日Ｗ(2)         TO 負傷日２.
013190     MOVE 施術開始年Ｗ(2)     TO 施術開始年２.
013200     MOVE 施術開始月Ｗ(2)     TO 施術開始月２.
013210     MOVE 施術開始日Ｗ(2)     TO 施術開始日２.
013220     MOVE 施術終了年Ｗ(2)     TO 施術終了年２.
013230     MOVE 施術終了月Ｗ(2)     TO 施術終了月２.
013240     MOVE 施術終了日Ｗ(2)     TO 施術終了日２.
013250*     MOVE 負傷年月日区切Ｗ(2) TO 区切２１ 区切２２.
013260*     MOVE 施開年月日区切Ｗ(2) TO 区切２３ 区切２４.
013270*     MOVE 施終年月日区切Ｗ(2) TO 区切２５ 区切２６.
013280*     MOVE 転帰Ｗ(2)           TO 転帰２.
013290     MOVE 治癒チェックＷ(2)   TO 治癒チェック２.
013300     MOVE 中止チェックＷ(2)   TO 中止チェック２.
013310     MOVE 転医チェックＷ(2)   TO 転医チェック２.
013320*     ******************
013330*     * 日数は一時保留 *
013340*     ******************
013350*     **********************
013360*     * 施術回数は一時保留 *
013370*     **********************
013380*     **********************
013390*     * 転帰名称は一時保留 *
013400*     **********************
013410**********
013420* ３部位 *
013430**********
013440*     MOVE 会部位コードＷ(3)   TO 負傷コード３.
013450     MOVE 負傷名Ｗ(3)         TO 負傷名３.
013460     MOVE 負傷年Ｗ(3)         TO 負傷年３.
013470     MOVE 負傷月Ｗ(3)         TO 負傷月３.
013480     MOVE 負傷日Ｗ(3)         TO 負傷日３.
013490     MOVE 施術開始年Ｗ(3)     TO 施術開始年３.
013500     MOVE 施術開始月Ｗ(3)     TO 施術開始月３.
013510     MOVE 施術開始日Ｗ(3)     TO 施術開始日３.
013520     MOVE 施術終了年Ｗ(3)     TO 施術終了年３.
013530     MOVE 施術終了月Ｗ(3)     TO 施術終了月３.
013540     MOVE 施術終了日Ｗ(3)     TO 施術終了日３.
013550*     MOVE 負傷年月日区切Ｗ(3) TO 区切３１ 区切３２.
013560*     MOVE 施開年月日区切Ｗ(3) TO 区切３３ 区切３４.
013570*     MOVE 施終年月日区切Ｗ(3) TO 区切３５ 区切３６.
013580*     MOVE 転帰Ｗ(3)           TO 転帰３.
013590     MOVE 治癒チェックＷ(3)   TO 治癒チェック３.
013600     MOVE 中止チェックＷ(3)   TO 中止チェック３.
013610     MOVE 転医チェックＷ(3)   TO 転医チェック３.
013620*     ******************
013630*     * 日数は一時保留 *
013640*     ******************
013650*     **********************
013660*     * 施術回数は一時保留 *
013670*     **********************
013680*     **********************
013690*     * 転帰名称は一時保留 *
013700*     **********************
013710**********
013720* ４部位 *
013730**********
013740*     MOVE 会部位コードＷ(4)   TO 負傷コード４.
013750     MOVE 負傷名Ｗ(4)         TO 負傷名４.
013760     MOVE 負傷年Ｗ(4)         TO 負傷年４.
013770     MOVE 負傷月Ｗ(4)         TO 負傷月４.
013780     MOVE 負傷日Ｗ(4)         TO 負傷日４.
013790     MOVE 施術開始年Ｗ(4)     TO 施術開始年４.
013800     MOVE 施術開始月Ｗ(4)     TO 施術開始月４.
013810     MOVE 施術開始日Ｗ(4)     TO 施術開始日４.
013820     MOVE 施術終了年Ｗ(4)     TO 施術終了年４.
013830     MOVE 施術終了月Ｗ(4)     TO 施術終了月４.
013840     MOVE 施術終了日Ｗ(4)     TO 施術終了日４.
013850*     MOVE 負傷年月日区切Ｗ(4) TO 区切４１ 区切４２.
013860*     MOVE 施開年月日区切Ｗ(4) TO 区切４３ 区切４４.
013870*     MOVE 施終年月日区切Ｗ(4) TO 区切４５ 区切４６.
013880*     MOVE 転帰Ｗ(4)           TO 転帰４.
013890     MOVE 治癒チェックＷ(4)   TO 治癒チェック４.
013900     MOVE 中止チェックＷ(4)   TO 中止チェック４.
013910     MOVE 転医チェックＷ(4)   TO 転医チェック４.
013920*     ******************
013930*     * 日数は一時保留 *
013940*     ******************
013950*     **********************
013960*     * 施術回数は一時保留 *
013970*     **********************
013980*     **********************
013990*     * 転帰名称は一時保留 *
014000*     **********************
014010**********
014020* ５部位 *
014030**********
014040*     MOVE 会部位コードＷ(5)   TO 負傷コード５.
014050     MOVE 負傷名Ｗ(5)         TO 負傷名５.
014060     MOVE 負傷年Ｗ(5)         TO 負傷年５.
014070     MOVE 負傷月Ｗ(5)         TO 負傷月５.
014080     MOVE 負傷日Ｗ(5)         TO 負傷日５.
014090     MOVE 施術開始年Ｗ(5)     TO 施術開始年５.
014100     MOVE 施術開始月Ｗ(5)     TO 施術開始月５.
014110     MOVE 施術開始日Ｗ(5)     TO 施術開始日５.
014120     MOVE 施術終了年Ｗ(5)     TO 施術終了年５.
014130     MOVE 施術終了月Ｗ(5)     TO 施術終了月５.
014140     MOVE 施術終了日Ｗ(5)     TO 施術終了日５.
014150*     MOVE 負傷年月日区切Ｗ(5) TO 区切５１ 区切５２.
014160*     MOVE 施開年月日区切Ｗ(5) TO 区切５３ 区切５４.
014170*     MOVE 施終年月日区切Ｗ(5) TO 区切５５ 区切５６.
014180*     MOVE 転帰Ｗ(5)           TO 転帰５.
014190     MOVE 治癒チェックＷ(5)   TO 治癒チェック５.
014200     MOVE 中止チェックＷ(5)   TO 中止チェック５.
014210     MOVE 転医チェックＷ(5)   TO 転医チェック５.
014220*     ******************
014230*     * 日数は一時保留 *
014240*     ******************
014250*     **********************
014260*     * 施術回数は一時保留 *
014270*     **********************
014280*     **********************
014290*     * 転帰名称は一時保留 *
014300*     **********************
014310************
014320* 負傷原因 *
014330************
014340     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
014350     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
014360     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
014370     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
014380     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
014390     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
014400     MOVE 負傷原因Ｗ(7)       TO 負傷原因７.
014410     MOVE 負傷原因Ｗ(8)       TO 負傷原因８.
014420     MOVE 負傷原因Ｗ(9)       TO 負傷原因９.
014430     MOVE 負傷原因Ｗ(10)      TO 負傷原因１０.
014440*
014450*================================================================*
014460 負担率取得 SECTION.
014470*
014480     MOVE ZERO TO 本人負担率Ｗ.
014490     MOVE ZERO TO 家族負担率Ｗ.
014500*
014510     MOVE 施術和暦ＷＲ   TO 受－施術和暦.
014520     MOVE 施術年ＷＲ     TO 受－施術年.
014530     MOVE 施術月ＷＲ     TO 受－施術月.
014540     MOVE 患者番号ＷＲ   TO 受－患者番号.
014550     MOVE 枝番ＷＲ       TO 受－枝番.
014560     READ 受診者情報Ｆ
014570     INVALID KEY
014580         MOVE  NC"　　施術月の受診者情報がありません" TO 連メ－メッセージ
014590         CALL   "MSG001"
014600         CANCEL "MSG001"
014610         PERFORM ファイル閉鎖
014620         MOVE ZERO TO PROGRAM-STATUS
014630         EXIT PROGRAM
014640     NOT INVALID KEY
014650* 14/10～　サブルーチン処理
014660         IF 受－施術和暦年月 >= 41410
014670            PERFORM 負担率取得１４１０
014680         END-IF
014690     END-READ.
014700*
014710     EVALUATE 負担率Ｗ
014720     WHEN 0
014730         MOVE NC"○" TO ０割チェックＷ
014740     WHEN 10
014750         MOVE NC"○" TO １割チェックＷ
014760     WHEN 20
014770         MOVE NC"○" TO ２割チェックＷ
014780     WHEN 30
014790         MOVE NC"○" TO ３割チェックＷ
014800     END-EVALUATE.
014810*
014820*================================================================*
014830 負担率取得１４１０ SECTION.
014840*
014850* 受診者Ｆ READ中
014860* 平成14/10～
014870     MOVE ZERO  TO 負担率Ｗ.
014880     INITIALIZE 助成負担率ＷＷ.
014890*
014900     MOVE SPACE TO 連率－負担率取得キー.
014910     INITIALIZE 連率－負担率取得キー.
014920     MOVE 受－施術和暦年月 TO 連率－施術和暦年月.
014930     MOVE 受－患者コード   TO 連率－患者コード.
014940*
014950     CALL   "HUTANRIT".
014960     CANCEL "HUTANRIT".
014970*
014980* / 本体 /
014990     MOVE 連率－実際本体負担率 TO 負担率Ｗ.
015000*
015010*
015020* / 助成 /
015030     IF 受－助成種別 NOT = ZERO
015040        MOVE 連率－助成負担率  TO 助成負担率Ｗ
015050*
015060        COMPUTE 助成負担率Ｗ１ = 助成負担率Ｗ / 10
015070        MOVE 助成負担率Ｗ１ TO  助成負担率文字Ｗ１
015080*
015090        STRING "("                 DELIMITED BY SIZE
015100               助成負担率文字Ｗ１  DELIMITED BY SIZE
015110               "割)"               DELIMITED BY SIZE
015120               INTO 助成負担率表示Ｗ
015130        END-STRING
015140*
015150     END-IF.
015160*
015170*================================================================*
015180 原因行セット SECTION.
015190*/0610
015200* 指定された開始行から印刷するようにセットし直す。
015210* １０行.
015220*
015230     MOVE SPACE                  TO 負傷原因Ｗテーブル２.
015240     MOVE ZERO                   TO カウンタ.
015250     IF 連印－原因開始行 = ZERO
015260         MOVE 1                  TO カウンタ２
015270     ELSE
015280         MOVE 連印－原因開始行   TO カウンタ２
015290     END-IF.
015300     PERFORM VARYING カウンタ FROM 1 BY 1
015310             UNTIL ( カウンタ > 10 ) OR (カウンタ２ > 10)
015320         MOVE 負傷原因Ｗ(カウンタ) TO 負傷原因Ｗ２(カウンタ２)
015330         COMPUTE カウンタ２ = カウンタ２ + 1
015340     END-PERFORM.
015350*
015360     PERFORM VARYING カウンタ FROM 1 BY 1
015370               UNTIL カウンタ > 10
015380         MOVE 負傷原因Ｗ２(カウンタ) TO 負傷原因Ｗ(カウンタ)
015390     END-PERFORM.
015400*================================================================*
015410 受診者情報取得 SECTION.
015420*
015430**************************************************
015440* 連結データから受診者情報Ｆより以下の情報を取得 *
015450* ● 患者番号.... 患者番号Ｗ                     *
015460* ● 枝番........ 枝番Ｗ                         *
015470* ● 記号 ....... 記号Ｗに格納                   *
015480* ● 番号 ....... 番号Ｗに格納                   *
015490* ● 被保険者カナ.被保険者カナＷに格納           *
015500* ● 被保険者氏名.被保険者氏名Ｗに格納           *
015510* ● 郵便番号１ ..郵便番号１Ｗに格納             *
015520* ● 郵便番号２ ..郵便番号２Ｗに格納             *
015530* ● 住所１ ......住所１Ｗに格納                 *
015540* ● 住所２ ......住所２Ｗに格納                 *
015550* ● 電話番号.....電話番号Ｗに格納               *
015560* ● 患者カナ ....患者カナＷに格納               *
015570* ● 患者氏名 ....患者氏名Ｗに格納               *
015580* ● 続柄 ........名称マスタより続柄Ｗに取得     *
015590* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
015600* ● 患者年 ......患者年Ｗに格納                 *
015610* ● 患者月 ......患者月Ｗに格納                 *
015620* ● 患者日 ......患者日Ｗに格納                 *
015630**************************************************
015640     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
015650     MOVE 施術年ＷＲ         TO 受－施術年.
015660     MOVE 施術月ＷＲ         TO 受－施術月.
015670     MOVE 患者コードＷＲ     TO 受－患者コード.
015680     READ 受診者情報Ｆ
015690     INVALID KEY
015700         CONTINUE
015710*            /* ありえない */
015720     NOT INVALID KEY
015730         MOVE 受－患者番号     TO 患者番号Ｗ
015740         MOVE 受－枝番         TO 枝番Ｗ
015750*         MOVE 受－記号         TO 記号Ｗ
015760*         MOVE 受－番号         TO 番号Ｗ
015770*-----------------------------------------------------------------*
015780         MOVE SPACE TO 連暗号複合－暗号情報
015790*        / 連暗号複合－入力情報セット /
015800         MOVE 受－記号       TO 連暗号複合－記号
015810         MOVE 受－番号       TO 連暗号複合－番号
015820         MOVE 受－暗号化項目 TO 連暗号複合－暗号化項目
015830*
015840         CALL   複合プログラム名Ｗ
015850         CANCEL 複合プログラム名Ｗ
015860*
015870         MOVE 連暗号複合－複合した記号 TO 記号Ｗ
015880         MOVE 連暗号複合－複合した番号 TO 番号Ｗ
015890*
015900*-----------------------------------------------------------------*
015910         MOVE 受－被保険者カナ TO 被保険者カナＷ
015920         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
015930*
015940         EVALUATE 受－被保険者性別
015950         WHEN 1
015960             MOVE NC"○"  TO 被保険者男チェックＷ
015970         WHEN 2
015980             MOVE NC"○"  TO 被保険者女チェックＷ
015990         END-EVALUATE
016000*
016010         EVALUATE 受－被保険者和暦
016020         WHEN 1
016030             MOVE NC"○"  TO 被保険者明治チェックＷ
016040         WHEN 2
016050             MOVE NC"○"  TO 被保険者大正チェックＷ
016060         WHEN 3
016070             MOVE NC"○"  TO 被保険者昭和チェックＷ
016080         WHEN 4
016090             MOVE NC"○"  TO 被保険者平成チェックＷ
016100         END-EVALUATE
016160         MOVE 受－被保険者和暦 TO 元－元号区分
016170         READ 元号マスタ
016180         NOT INVALID KEY
016190             MOVE 元－元号名称 TO 被保険者元号Ｗ
016200         END-READ
016110         MOVE 受－被保険者年   TO 被保険者年Ｗ
016120         MOVE 受－被保険者月   TO 被保険者月Ｗ
016130         MOVE 受－被保険者日   TO 被保険者日Ｗ
016140
016150*被保険者年齢
016160     MOVE 連印－施術和暦 TO 元－元号区分
016170     READ 元号マスタ
016180     NOT INVALID KEY
016190         COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
016200     END-READ
016210     IF ( 受－被保険者和暦 NOT = ZERO ) AND
016220        ( 受－被保険者年   NOT = ZERO ) AND
016230        ( 受－被保険者月   NOT = ZERO ) AND
016240        ( 受－被保険者日   NOT = ZERO )
016250
016260         MOVE ZERO TO 被保険者年齢Ｗ
016270*
016280         MOVE 受－被保険者和暦 TO 元－元号区分
016290         READ 元号マスタ
016300         NOT INVALID KEY
016310             COMPUTE 被保険者西暦年Ｗ = 元－開始西暦年 + ( 受－被保険者年 - 1 )
016320         END-READ
016330*
016340         COMPUTE 被保険者年齢Ｗ   = 施術西暦年Ｗ - 被保険者西暦年Ｗ
016350*
016360         IF 連印－施術月 < 受－被保険者月
016370             COMPUTE 被保険者年齢Ｗ = 被保険者年齢Ｗ - 1
016380         END-IF
016390*
016400     END-IF
016410*
016420*/郵便番号 ゼロの時はスペース
016430         IF 受－郵便番号１ = "000"
016440             MOVE SPACE          TO 被保険者郵便番号１Ｗ
016450             MOVE SPACE          TO 被保険者郵便番号２Ｗ
016460         ELSE
016470             MOVE 受－郵便番号１ TO 被保険者郵便番号１Ｗ
016480             MOVE 受－郵便番号２ TO 被保険者郵便番号２Ｗ
016490         END-IF
016500         MOVE 受－電話番号     TO 被保険者電話番号Ｗ
016510         MOVE 受－住所１       TO 印刷被保険者住所１Ｗ
016520         MOVE 受－住所２       TO 印刷被保険者住所２Ｗ
016530*         STRING 受－住所１  DELIMITED BY SPACE
016540*                受－住所２  DELIMITED BY SPACE
016550*           INTO 被保険者住所Ｗ
016560*         END-STRING
016570         IF 受－保険種別 = 70 OR 80 OR 85 OR 90
016580             MOVE 受－患者カナ       TO 被保険者カナＷ
016590             MOVE 受－患者氏名       TO 被保険者氏名Ｗ
016600             EVALUATE 受－患者性別
016610             WHEN 1
016620                 MOVE NC"○"  TO 被保険者男チェックＷ
016630             WHEN 2
016640                 MOVE NC"○"  TO 被保険者女チェックＷ
016650             END-EVALUATE
016660             EVALUATE 受－患者和暦
016670             WHEN 1
016680                 MOVE NC"○"  TO 被保険者明治チェックＷ
016690             WHEN 2
016700                 MOVE NC"○"  TO 被保険者大正チェックＷ
016710             WHEN 3
016720                 MOVE NC"○"  TO 被保険者昭和チェックＷ
016730*/被保険者平成を追加/080606
016740             WHEN 4
016750                 MOVE NC"○"  TO 被保険者平成チェックＷ
016760             END-EVALUATE
016160             MOVE 受－患者和暦 TO 元－元号区分
016170             READ 元号マスタ
016180             NOT INVALID KEY
016190                 MOVE 元－元号名称 TO 被保険者元号Ｗ
016200             END-READ
016770             MOVE 受－患者年   TO 被保険者年Ｗ
016780             MOVE 受－患者月   TO 被保険者月Ｗ
016790             MOVE 受－患者日   TO 被保険者日Ｗ
016800             MOVE 受－患者郵便番号１ TO 被保険者郵便番号１Ｗ
016810             MOVE 受－患者郵便番号２ TO 被保険者郵便番号２Ｗ
016820             MOVE 受－患者住所１       TO 印刷被保険者住所１Ｗ
016830             MOVE 受－患者住所２       TO 印刷被保険者住所２Ｗ
016840             MOVE 受－患者電話番号   TO 被保険者電話番号Ｗ
016850             IF ( 受－患者和暦 NOT = ZERO ) AND
016860                ( 受－患者年   NOT = ZERO ) AND
016870                ( 受－患者月   NOT = ZERO ) AND
016880                ( 受－患者日   NOT = ZERO )
016890
016900                 MOVE ZERO TO 被保険者年齢Ｗ
016910*
016920                 MOVE 受－患者和暦 TO 元－元号区分
016930                 READ 元号マスタ
016940                 NOT INVALID KEY
016950                     COMPUTE 被保険者西暦年Ｗ = 元－開始西暦年 + ( 受－患者年 - 1 )
016960                 END-READ
016970*
016980                 COMPUTE 被保険者年齢Ｗ   = 施術西暦年Ｗ - 被保険者西暦年Ｗ
016990*
017000                 IF 連印－施術月 < 受－患者月
017010                     COMPUTE 被保険者年齢Ｗ = 被保険者年齢Ｗ - 1
017020                 END-IF
017030*
017040*                MOVE 被保険者年齢Ｗ     TO 被保険者年齢
017050             END-IF
017060         END-IF
017070*
017080*/郵便番号 ゼロの時はスペース
017090         IF 受－患者郵便番号１ = "000"
017100             MOVE SPACE              TO 患者郵便番号１Ｗ
017110             MOVE SPACE              TO 患者郵便番号２Ｗ
017120         ELSE
017130             MOVE 受－患者郵便番号１ TO 患者郵便番号１Ｗ
017140             MOVE 受－患者郵便番号２ TO 患者郵便番号２Ｗ
017150         END-IF
017160         MOVE 受－患者住所１     TO 患者住所１Ｗ
017170         MOVE 受－患者住所２     TO 患者住所２Ｗ
017180*         STRING 受－患者住所１  DELIMITED BY SPACE
017190*                受－患者住所２  DELIMITED BY SPACE
017200*           INTO 患者住所Ｗ
017210*         END-STRING
017220         MOVE 受－患者電話番号   TO 患者電話番号Ｗ
017230*
017240*電話番号分割
017250         UNSTRING 被保険者電話番号Ｗ  DELIMITED BY "-"
017260             INTO 分割電話番号１Ｗ
017270                  分割電話番号２Ｗ
017280                  分割電話番号３Ｗ
017290         END-UNSTRING
017300*電話番号右詰処理
017310         IF 分割電話番号３Ｗ = SPACE
017320             MOVE 分割電話番号２Ｗ TO 分割電話番号３Ｗ
017330             MOVE 分割電話番号１Ｗ TO 分割電話番号２Ｗ
017340             MOVE SPACE            TO 分割電話番号１Ｗ
017350         END-IF
017360*
017370         MOVE 受－患者カナ     TO 患者カナＷ
017380         MOVE 受－患者氏名     TO 患者氏名Ｗ
017390*
017400         MOVE 受－保険種別     TO 事－保険種別
017410         MOVE 受－保険者番号   TO 事－保険者番号
017420*         MOVE 受－記号         TO 事－記号
017430*-----------------------------------------------------------------*
017440         MOVE SPACE TO 連暗号複合－暗号情報
017450*
017460*        / 連暗号複合－入力情報セット /
017470         MOVE 受－記号       TO 連暗号複合－記号
017480         MOVE 受－番号       TO 連暗号複合－番号
017490         MOVE 受－暗号化項目 TO 連暗号複合－暗号化項目
017500*
017510         CALL   複合プログラム名Ｗ
017520         CANCEL 複合プログラム名Ｗ
017530*
017540         MOVE 連暗号複合－複合した記号 TO 事－記号
017550*
017560*-----------------------------------------------------------------*
017570*
017580         READ 事業所マスタ
017590         INVALID KEY
017600            MOVE SPACE TO 事－レコード
017610            INITIALIZE    事－レコード
017620         END-READ
017630         MOVE 事－事業所名称   TO 事業所名称Ｗ
017640         MOVE 事－事業所住所１ TO 印刷事業所住所１Ｗ
017650         MOVE 事－事業所住所２ TO 印刷事業所住所２Ｗ
017660* 続柄
017670         IF 受－本人家族区分 = 1
017680             MOVE NC"本人"     TO 続柄Ｗ
017690             MOVE NC"○"       TO 本人チェックＷ
017700         ELSE
017710             MOVE 05          TO 名－区分コード
017720             MOVE 受－続柄    TO 名－名称コード
017730             READ 名称マスタ
017740             INVALID KEY
017750                 MOVE SPACE   TO 続柄Ｗ
017760             NOT INVALID KEY
017770                 MOVE 名－略称  TO 続柄Ｗ
017780             END-READ
017790             MOVE NC"○"       TO 家族チェックＷ
017800         END-IF
017810* 患者和暦年月日
017820         EVALUATE 受－患者和暦
017830         WHEN 1
017840             MOVE NC"○"  TO 明治チェックＷ
017850         WHEN 2
017860             MOVE NC"○"  TO 大正チェックＷ
017870         WHEN 3
017880             MOVE NC"○"  TO 昭和チェックＷ
017890         WHEN 4
017900             MOVE NC"○"  TO 平成チェックＷ
017910         END-EVALUATE
016160         MOVE 受－患者和暦 TO 元－元号区分
016170         READ 元号マスタ
016180         NOT INVALID KEY
016190             MOVE 元－元号名称 TO 患者元号Ｗ
016200         END-READ
017920         MOVE 受－患者年  TO 患者年Ｗ
017930         MOVE 受－患者月  TO 患者月Ｗ
017940         MOVE 受－患者日  TO 患者日Ｗ
017950*患者年齢
017960     MOVE 連印－施術和暦 TO 元－元号区分
017970     READ 元号マスタ
017980     NOT INVALID KEY
017990         COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
018000     END-READ
018010     IF ( 受－患者和暦 NOT = ZERO ) AND
018020        ( 受－患者年   NOT = ZERO ) AND
018030        ( 受－患者月   NOT = ZERO ) AND
018040        ( 受－患者日   NOT = ZERO )
018050*
018060         MOVE ZERO TO 患者年齢Ｗ
018070*
018080         MOVE 受－患者和暦 TO 元－元号区分
018090         READ 元号マスタ
018100         NOT INVALID KEY
018110             COMPUTE 患者西暦年Ｗ = 元－開始西暦年 + ( 受－患者年 - 1 )
018120         END-READ
018130*
018140         COMPUTE 患者年齢Ｗ   = 施術西暦年Ｗ - 患者西暦年Ｗ
018150*
018160         IF 連印－施術月 < 受－患者月
018170             COMPUTE 患者年齢Ｗ = 患者年齢Ｗ - 1
018180         END-IF
018190*
018200     END-IF
018210* 患者性別
018220*         EVALUATE 受－患者性別
018230*         WHEN 1
018240*             MOVE NC"（男）" TO 患者性別Ｗ
018250*         WHEN 2
018260*             MOVE NC"（女）" TO 患者性別Ｗ
018270*         END-EVALUATE
018280         EVALUATE 受－患者性別
018290         WHEN 1
018300             MOVE NC"○"  TO 患者男チェックＷ
018310         WHEN 2
018320             MOVE NC"○"  TO 患者女チェックＷ
018330         END-EVALUATE
018340*/有効期限
018350         MOVE 受－有効和暦   TO 有効期限和暦Ｗ
018360         MOVE 受－有効年     TO 有効期限年Ｗ
018370         MOVE 受－有効月     TO 有効期限月Ｗ
018380         MOVE 受－有効日     TO 有効期限日Ｗ
018510         IF (有効期限和暦Ｗ NOT = ZERO) AND
018520            (有効期限年Ｗ   NOT = ZERO) AND
018530            (有効期限月Ｗ   NOT = ZERO) AND
018540            (有効期限日Ｗ   NOT = ZERO)
018390            MOVE 有効期限和暦Ｗ TO 元－元号区分
018400            READ 元号マスタ
018410            INVALID KEY
018420                MOVE SPACE        TO 有効期限元号Ｗ
018430            NOT INVALID KEY
018440                MOVE 元－元号名称 TO 有効期限元号Ｗ
018450            END-READ
               END-IF
018460*/資格取得年月日
018470         MOVE 受－資格和暦 TO 資格取得和暦Ｗ
018480         MOVE 受－資格年   TO 資格取得年Ｗ
018490         MOVE 受－資格月   TO 資格取得月Ｗ
018500         MOVE 受－資格日   TO 資格取得日Ｗ
018510         IF (資格取得和暦Ｗ NOT = ZERO) AND
018520            (資格取得年Ｗ   NOT = ZERO) AND
018530            (資格取得月Ｗ   NOT = ZERO) AND
018540            (資格取得日Ｗ   NOT = ZERO)
018550             EVALUATE 資格取得和暦Ｗ
018560             WHEN 3
018570                 MOVE NC"○"  TO 資格昭和チェックＷ
018580             WHEN 4
018590                 MOVE NC"○"  TO 資格平成チェックＷ
018600             END-EVALUATE
016160             MOVE 資格取得和暦Ｗ TO 元－元号区分
016170             READ 元号マスタ
016180             NOT INVALID KEY
016190                 MOVE 元－元号名称 TO 資格取得元号Ｗ
016200             END-READ
018610         END-IF
018620**市町村国保､退職の場合は有効期限を出力
018630**業種別国保(国保組合)､共済､日雇､船員､社保､組合､自衛官は有効期限日を出力する
018640*         IF (( 受－保険種別 = 08 ) OR
018650*            (( 受－保険種別 = 01 ) AND
018660*            (  受－保険者番号(3:1) NOT = "3" )))
018670*             MOVE 受－有効和暦 TO 資格取得和暦Ｗ
018680*             MOVE 受－有効年   TO 資格取得年Ｗ
018690*             MOVE 受－有効月   TO 資格取得月Ｗ
018700*             MOVE 受－有効日   TO 資格取得日Ｗ
018710*         ELSE
018720*             MOVE 受－資格和暦 TO 資格取得和暦Ｗ
018730*             MOVE 受－資格年   TO 資格取得年Ｗ
018740*             MOVE 受－資格月   TO 資格取得月Ｗ
018750*             MOVE 受－資格日   TO 資格取得日Ｗ
018760*         END-IF
018770*         IF (資格取得和暦Ｗ NOT = ZERO) AND
018780*            (資格取得年Ｗ   NOT = ZERO) AND
018790*            (資格取得月Ｗ   NOT = ZERO) AND
018800*            (資格取得日Ｗ   NOT = ZERO)
018810*             EVALUATE 資格取得和暦Ｗ
018820*             WHEN 3
018830*                 MOVE NC"○"  TO 資格昭和チェックＷ
018840*             WHEN 4
018850*                 MOVE NC"○"  TO 資格平成チェックＷ
018860*             END-EVALUATE
018870*         END-IF
018880* 市町村番号 受給者番号
018890         IF 受－助成種別 NOT = ZERO
018900             MOVE 受－費用負担者番号助成 TO 費用負担者番号Ｗ
018910             MOVE 受－受益者番号助成     TO 受益者番号助成Ｗ
018920         END-IF
018930         IF ( 受－公費種別 = 05 ) AND ( 受－施術和暦年月 < 42004 )
018940             MOVE 受－費用負担者番号     TO 市町村番号Ｗ
018950             MOVE 受－受益者番号老人     TO 受益者番号Ｗ
018960         END-IF
018970* 労災情報
018980         IF 受－保険種別 = 70
018990            MOVE 受－施術和暦年月 TO 労災－施術和暦年月
019000            MOVE 受－患者コード   TO 労災－患者コード
019010            READ 労災情報Ｆ
019020            INVALID KEY
019030               MOVE SPACE TO 労災－レコード
019040               INITIALIZE    労災－レコード
019050            END-READ
019060            MOVE 労災－労災事業所名称       TO 事業所名称Ｗ
019070            MOVE 労災－労災事業所郵便番号１ TO 事業所郵便番号１Ｗ
019080            MOVE 労災－労災事業所郵便番号２ TO 事業所郵便番号２Ｗ
019090            MOVE 労災－労災事業所住所１     TO 印刷事業所住所１Ｗ
019100            MOVE 労災－労災事業所住所２     TO 印刷事業所住所２Ｗ
019110            MOVE 労災－労働保険番号         TO 番号Ｗ
019120         END-IF
019130*
019140*保険種別
019150         EVALUATE 受－助成種別
019160         WHEN 50
019170             MOVE NC"○"  TO 生保チェックＷ
019180         WHEN 51
019190*/頭4桁が "4113"東京 "4108"茨城 "4132"島根 の時は、「福」。それ以外は「老」
019200             IF  受－費用負担者番号助成(1:4) = "4113" OR
019210                 受－費用負担者番号助成(1:4) = "4108" OR
019220                 受－費用負担者番号助成(1:4) = "4132"
019230                 MOVE NC"○"  TO 公費負担チェックＷ
019240*                 MOVE NC"○"    TO 福チェックＷ
019250             ELSE
019260                 MOVE NC"○"  TO 公費負担チェックＷ
019270*                 MOVE NC"○"    TO 老人チェックＷ
019280             END-IF
019290         WHEN 52
019300             MOVE NC"○"  TO 公費負担チェックＷ
019310*             MOVE NC"○"  TO 母子チェックＷ
019320         WHEN 53
019330             MOVE NC"○"  TO 公費負担チェックＷ
019340*             MOVE NC"○"  TO 身障チェックＷ
019350         WHEN 54
019360             MOVE NC"○"  TO 公費負担チェックＷ
019370*             MOVE NC"○"  TO 原爆チェックＷ
019380         WHEN 55
019390             MOVE NC"○"  TO 公費負担チェックＷ
019400*             MOVE NC"○"  TO 乳幼児チェックＷ
019410         END-EVALUATE
019420
019430         IF 受－公費種別 = 05
019440             MOVE NC"○"  TO 老人チェックＷ
019450         ELSE
019460             EVALUATE 受－保険種別
019470             WHEN 01
019480                 MOVE NC"○"  TO 国保チェックＷ
019490             WHEN 02
019500                 MOVE NC"○"  TO 健保チェックＷ
019510*                 MOVE NC"○"  TO 社保チェックＷ
019520             WHEN 03
019530                 MOVE NC"○"  TO 健保チェックＷ
019540*                 MOVE NC"○"  TO 組合チェックＷ
019550             WHEN 04
019560                 MOVE NC"○"  TO 共済チェックＷ
019570             WHEN 06
019580                 MOVE NC"○"  TO 健保チェックＷ
019590*                 MOVE NC"○"  TO 日雇チェックＷ
019600             WHEN 07
019610                 MOVE NC"○"  TO 船員チェックＷ
019620             WHEN 08
019630                 MOVE NC"○"  TO 退職チェックＷ
019640             WHEN 09
019650                  MOVE NC"○"  TO 自衛官チェックＷ
019660*                 MOVE NC"○"  TO 共済チェックＷ
019670             WHEN 70
019680                 MOVE NC"○"  TO 労災チェックＷ
019690             WHEN 80
019700                 MOVE NC"○"  TO 自賠チェックＷ
019710             WHEN 90
019720                 MOVE NC"○"  TO 自費チェックＷ
019730             END-EVALUATE
019740         END-IF
019750     END-READ.
019760*HILO テスト－－－－－－－－－－－
019770**      MOVE NC"○"  TO 被保険者明治Ｗ.
019780**      MOVE NC"○"  TO 被保険者大正Ｗ.
019790**      MOVE NC"○"  TO 被保険者昭和Ｗ.
019800**      MOVE NC"○"  TO 被保険者男チェックＷ.
019810**      MOVE NC"○"  TO 被保険者女チェックＷ.
019820*      MOVE NC"○"  TO 患者男チェックＷ.
019830*      MOVE NC"○"  TO 患者女チェックＷ.
019840*      MOVE NC"○"  TO 明治チェックＷ.
019850*      MOVE NC"○"  TO 大正チェックＷ.
019860*      MOVE NC"○"  TO 昭和チェックＷ.
019870*      MOVE NC"○"  TO 平成チェックＷ.
019880*      MOVE NC"○"  TO 健保チェックＷ    .
019890*      MOVE NC"○"  TO 社保チェックＷ    .
019900*      MOVE NC"○"  TO 組合チェックＷ    .
019910*      MOVE NC"○"  TO 共済チェックＷ    .
019920*      MOVE NC"○"  TO 国保チェックＷ    .
019930*      MOVE NC"○"  TO 日雇チェックＷ    .
019940*      MOVE NC"○"  TO 船員チェックＷ    .
019950*      MOVE NC"○"  TO 老人チェックＷ    .
019960**      MOVE NC"○"  TO 福チェックＷ    .
019970**      MOVE NC"○"  TO 身障チェックＷ    .
019980*      MOVE NC"○"  TO 退職チェックＷ    .
019990**      MOVE NC"○"  TO 母子チェックＷ    .
020000**      MOVE NC"○"  TO 乳幼児チェックＷ  .
020010**      MOVE NC"○"  TO 国保組合チェックＷ.
020020**      MOVE NC"○"  TO 労災チェックＷ    .
020030*      MOVE NC"○"  TO 生保チェックＷ    .
020040*      MOVE NC"○"  TO 自賠チェックＷ    .
020050*      MOVE NC"○"  TO 自衛官チェックＷ    .
020060*      MOVE NC"○"  TO 公費負担チェックＷ    .
020070*      MOVE NC"○"  TO 自費チェックＷ    .
020080**      MOVE NC"○"  TO 原爆チェックＷ    .
020090*      MOVE NC"○" TO ０割チェックＷ.
020100*      MOVE NC"○" TO １割チェックＷ.
020110*      MOVE NC"○" TO ２割チェックＷ.
020120*      MOVE NC"○" TO ３割チェックＷ.
020130*      MOVE NC"○"  TO 本人チェックＷ    .
020140*      MOVE NC"○"  TO 家族チェックＷ    .
020150*      MOVE NC"○"  TO 資格昭和チェックＷ.
020160*      MOVE NC"○"  TO 資格平成チェックＷ.
020170*      MOVE NC"○"  TO 被保険者明治チェックＷ.
020180*      MOVE NC"○"  TO 被保険者大正チェックＷ.
020190*      MOVE NC"○"  TO 被保険者昭和チェックＷ.
020200*      MOVE NC"○"  TO 被保険者平成チェックＷ.
020210*-------------------------------------
020220*================================================================*
020230 請求先情報取得 SECTION.
020240*
020250****************************************************
020260* 連結データから保険者マスタより請求先を取得する。 *
020270* ※保－請求先情報区分=1の場合請求先マスタを使用   *
020280* ● 保険者番号...保険者番号Ｗに格納               *
020290* ● 名称........ 請求先名称Ｗに格納               *
020300* ● 郵便番号１.. 請求先郵便番号１Ｗに格納         *
020310* ● 郵便番号２.. 請求先郵便番号２Ｗに格納         *
020320* ● 住所１.......請求先住所１Ｗに格納             *
020330* ● 住所２.......請求先住所２Ｗに格納             *
020340****************************************************
020350     MOVE SPACE          TO 保険者番号Ｗ.
020360     MOVE SPACE          TO 請求先名称Ｗ.
020370     MOVE SPACE          TO 支部名Ｗ.
020380     MOVE SPACE          TO 請求先郵便番号１Ｗ.
020390     MOVE SPACE          TO 請求先郵便番号２Ｗ.
020400     MOVE SPACE          TO 請求先住所１Ｗ.
020410     MOVE SPACE          TO 請求先住所２Ｗ.
020420     MOVE SPACE          TO 印刷保険者呼名Ｗ.
020430     MOVE SPACE          TO 請求先名支部名Ｗ
020440*
020450     MOVE 保険種別ＷＲ   TO 保－保険種別.
020460     MOVE 保険者番号ＷＲ TO 保－保険者番号.
020470     READ 保険者マスタ
020480     INVALID KEY
020490         IF ( 保険種別ＷＲ = 05 ) AND ( 受－施術和暦年月 >= 42004 )
020500             MOVE 保険種別ＷＲ   TO 市－公費種別
020510             MOVE 保険者番号ＷＲ TO 市－市町村番号
020520             READ 市町村マスタ
020530             INVALID KEY
020540                 MOVE SPACE      TO 請求先名称Ｗ
020550                 MOVE SPACE      TO 支部名Ｗ
020560                 MOVE ZERO       TO 接尾語区分Ｗ
020570             NOT INVALID KEY
020580                 MOVE 市－市町村番号  TO 保険者番号Ｗ
020590                 MOVE 市－市町村名称  TO 請求先名称Ｗ
020600                 MOVE 市－支部部署名  TO 支部名Ｗ
020610                 MOVE 市－郵便番号１  TO 請求先郵便番号１Ｗ
020620                 MOVE 市－郵便番号２  TO 請求先郵便番号２Ｗ
020630                 MOVE 市－住所１      TO 請求先住所１Ｗ
020640                 MOVE 市－住所２      TO 請求先住所２Ｗ
020650             END-READ
020660         ELSE
020670             MOVE SPACE      TO 請求先名称Ｗ
020680             MOVE SPACE      TO 支部名Ｗ
020690             MOVE ZERO       TO 接尾語区分Ｗ
020700         END-IF
020710     NOT INVALID KEY
020720         MOVE 保－保険者番号  TO 保険者番号Ｗ
020730         MOVE 保－保険者名称  TO 請求先名称Ｗ
020740         MOVE 保－支部部署名  TO 支部名Ｗ
020750         MOVE 保－郵便番号１  TO 請求先郵便番号１Ｗ
020760         MOVE 保－郵便番号２  TO 請求先郵便番号２Ｗ
020770         MOVE 保－住所１      TO 請求先住所１Ｗ
020780         MOVE 保－住所２      TO 請求先住所２Ｗ
020790         MOVE 保－接尾語区分  TO 接尾語区分Ｗ
020800     END-READ.
020810     IF 保険種別ＷＲ = 85
020820        MOVE 施術和暦年月ＷＲ TO 生保－施術和暦年月
020830        MOVE 患者コードＷＲ   TO 生保－患者コード
020840        READ 生保情報Ｆ
020850        NOT INVALID KEY
020860           MOVE 生保－負担者番号    TO 保険者番号Ｗ
020870           MOVE 生保－生保市町村名  TO 請求先名称Ｗ
020880           MOVE SPACE               TO 支部名Ｗ
020890           MOVE 生保－生保送付先郵便番号１  TO 請求先郵便番号１Ｗ
020900           MOVE 生保－生保送付先郵便番号２  TO 請求先郵便番号２Ｗ
020910           MOVE 生保－生保送付先住所１      TO 請求先住所１Ｗ
020920           MOVE 生保－生保送付先住所２      TO 請求先住所２Ｗ
020930           MOVE SPACE              TO 記号Ｗ
020940           MOVE 生保－生保記号番号 TO 番号Ｗ
020950        END-READ
020960     END-IF.
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
020970     EVALUATE 保険種別ＷＲ
020980     WHEN 2
020990     WHEN 6
021000         IF 接尾語区分Ｗ = 1
021010            MOVE SPACE              TO 印刷保険者呼名Ｗ
021020         ELSE
021030            MOVE "社会保険事務所"   TO 印刷保険者呼名Ｗ
021040         END-IF
021050     WHEN 3
021060         MOVE "健康保険組合"   TO 印刷保険者呼名Ｗ
021070     WHEN OTHER
021080         MOVE SPACE TO 印刷保険者呼名Ｗ
021090     END-EVALUATE.
021100     IF 印刷保険者呼名Ｗ NOT = SPACE
021110         STRING 請求先名称Ｗ DELIMITED BY SPACE
021120                保険者呼名Ｗ DELIMITED BY SPACE
021130           INTO 請求先名称Ｗ
021140         END-STRING
021150     END-IF.
021160*組合と共済は支部名も印字する
021170     EVALUATE 保険種別ＷＲ
021180     WHEN 03
021190     WHEN 04
021200         STRING 請求先名称Ｗ DELIMITED BY SPACE
021210                "  "         DELIMITED BY SIZE
021220                支部名Ｗ     DELIMITED BY SPACE
021230           INTO 請求先名支部名Ｗ
021240         END-STRING
021250     WHEN OTHER
021260         MOVE 請求先名称Ｗ TO 請求先名支部名Ｗ
021270     END-EVALUATE.
021280*================================================================*
021290 負傷データ取得 SECTION.
021300*
021310**************************************************
021320* 連結データから負傷データＦより以下の情報を取得 *
021330* ● 負傷名...部位＋負傷種別にて加工して格納     *
021340* ● 負傷年.......負傷年Ｗ                       *
021350* ● 負傷月.......負傷月Ｗ                       *
021360* ● 負傷日.......負傷日Ｗ                       *
021370* ● 施術終了年...終了年Ｗ                       *
021380* ● 施術終了月...終了月Ｗ                       *
021390* ● 施術終了日...終了日Ｗ                       *
021400**************************************************
021410     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
021420     MOVE 施術年ＷＲ         TO 負－施術年.
021430     MOVE 施術月ＷＲ         TO 負－施術月.
021440     MOVE 患者コードＷＲ     TO 負－患者コード.
021450     READ 負傷データＦ
021460     INVALID KEY
021470         CONTINUE
021480*            /* ありえない */
021490     NOT INVALID KEY
021500         MOVE ZERO                         TO 部位数Ｗ
021510         MOVE 負－部位数                   TO 部位数Ｗ
021520         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
021530                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
021540*********************************************
021550* 注）全柔...負傷種別＋部位にて加工して格納 *
021560*********************************************
021570* 負傷種別
021580             MOVE SPACE                     TO 負傷名称Ｗ
021590             MOVE 03                        TO 名－区分コード
021600             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
021610             READ 名称マスタ
021620             INVALID KEY
021630                 MOVE SPACE    TO 負傷名称Ｗ
021640             NOT INVALID KEY
021650*                 MOVE 名－略称 TO 負傷名称Ｗ
021660                 MOVE 名－正式名称 TO 負傷名称Ｗ
021670             END-READ
021680* 部位
021690             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
021700             PERFORM 名称埋込処理
021710             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
021720             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
021730             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
021740*             IF 負－負傷年(部位ＣＮＴ) NOT = ZERO 
021750*                MOVE "."                   TO 負傷年月日区切Ｗ(部位ＣＮＴ)
021760*             END-IF
021770*
021780             MOVE 負－開始年(部位ＣＮＴ)   TO 施術開始年Ｗ(部位ＣＮＴ)
021790             MOVE 負－開始月(部位ＣＮＴ)   TO 施術開始月Ｗ(部位ＣＮＴ)
021800             MOVE 負－開始日(部位ＣＮＴ)   TO 施術開始日Ｗ(部位ＣＮＴ)
021810*             IF 負－開始年(部位ＣＮＴ) NOT = ZERO 
021820*                MOVE "."                   TO 施開年月日区切Ｗ(部位ＣＮＴ)
021830*             END-IF
021840*
021850             MOVE 負－終了年(部位ＣＮＴ)   TO 施術終了年Ｗ(部位ＣＮＴ)
021860             MOVE 負－終了月(部位ＣＮＴ)   TO 施術終了月Ｗ(部位ＣＮＴ)
021870             MOVE 負－終了日(部位ＣＮＴ)   TO 施術終了日Ｗ(部位ＣＮＴ)
021880*             IF 負－終了年(部位ＣＮＴ) NOT = ZERO 
021890*                MOVE "."                   TO 施終年月日区切Ｗ(部位ＣＮＴ)
021900*             END-IF
021910*転帰
021920             EVALUATE 負－転帰区分(部位ＣＮＴ)
021930             WHEN 1
021940             WHEN 2
021950                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
021960             WHEN 3
021970                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
021980             WHEN 4
021990                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
022000             END-EVALUATE
022010*             IF 負－転帰区分(部位ＣＮＴ) NOT = 9
022020*                 MOVE 04                        TO 名－区分コード
022030*                 MOVE 負－転帰区分(部位ＣＮＴ)  TO 名－名称コード
022040*                 READ 名称マスタ
022050*                 INVALID KEY
022060*                     MOVE SPACE    TO 転帰Ｗ(部位ＣＮＴ)
022070*                 NOT INVALID KEY
022080*                     MOVE 名－略称 TO 転帰Ｗ(部位ＣＮＴ)
022090*                 END-READ
022100*             END-IF
022110*hilo---------------------------------
022120*         MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
022130*         MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
022140*         MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
022150*-------------------------------------
022160             PERFORM 負傷データ退避
022170         END-PERFORM
022180     END-READ.
022190*================================================================*
022200 施術記録取得 SECTION.
022210*
022220************************************************************
022230* 連結データから負傷データＦより以下の情報を取得           *
022240* ● 施術開始年月日...該当する部位に対して当月最初の施術日 *
022250************************************************************
022260     MOVE 施術和暦ＷＲ  TO 施記－施術和暦.
022270     MOVE 施術年ＷＲ    TO 施記－施術年.
022280     MOVE 施術月ＷＲ    TO 施記－施術月.
022290     MOVE ZERO            TO 施記－施術日.
022300     MOVE ZERO            TO 施記－患者番号.
022310     MOVE SPACE           TO 施記－枝番.
022320     START 施術記録Ｆ   KEY IS >= 施記－施術和暦年月日
022330                                  施記－患者コード.
022340     IF 状態キー = "00"
022350         MOVE SPACE  TO 終了フラグ２
022360         PERFORM 施術記録Ｆ読込
022370         PERFORM UNTIL ( 終了フラグ２       = "YES" ) OR
022380                       ( 施記－施術和暦 NOT = 施術和暦ＷＲ ) OR
022390                       ( 施記－施術年   NOT = 施術年ＷＲ   ) OR
022400                       ( 施記－施術月   NOT = 施術月ＷＲ   )
022410*            **************
022420*            * 開始年月日 *
022430*            **************
022440             PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
022450                     UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
022460            EVALUATE TRUE ALSO TRUE ALSO TRUE ALSO TRUE ALSO TRUE
022470            WHEN 施記－負傷種別(部位ＣＮＴ) = 負傷種別Ｗ(部位ＣＮＴ) ALSO
022480                 施記－部位(部位ＣＮＴ)     = 部位Ｗ(部位ＣＮＴ)     ALSO
022490                 施記－左右区分(部位ＣＮＴ) = 左右区分Ｗ(部位ＣＮＴ) ALSO
022500                 施記－負傷位置番号(部位ＣＮＴ)
022510                                       = 負傷位置番号Ｗ(部位ＣＮＴ)  ALSO
022520                 開始年月日取得フラグ(部位ＣＮＴ) = SPACE
022530                   MOVE 施記－施術年     TO 施術開始年Ｗ(部位ＣＮＴ)
022540                   MOVE 施記－施術月     TO 施術開始月Ｗ(部位ＣＮＴ)
022550                   MOVE 施記－施術日     TO 施術開始日Ｗ(部位ＣＮＴ)
022560                   MOVE "YES"       TO 開始年月日取得フラグ(部位ＣＮＴ)
022570*
022580            WHEN OTHER
022590                CONTINUE
022600            END-EVALUATE
022610             END-PERFORM
022620             PERFORM 施術記録Ｆ読込
022630         END-PERFORM
022640     END-IF.
022650*================================================================*
022660 施術記録Ｆ読込 SECTION.
022670*
022680     READ 施術記録Ｆ NEXT
022690     AT END
022700         MOVE "YES" TO 終了フラグ２
022710     END-READ.
022720*================================================================*
022730 負傷原因取得 SECTION.
022740*
022750********************************************************************
022760*  負傷原因コードが同じものは、1行にまとめて印字する。
022770*  例: ①② 家で転んだ.
022780*     負傷原因コードが同じものをまとめ、テーブルにセット
022790*     (ただし、部位を飛んで同じものは、2行になる)
022800********************************************************************
022810     MOVE  ZERO   TO  カウンタ カウンタ２.
022820     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
022830*/登録部位数以上の行に印刷出来ない不具合の修正/090311
022840*             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
022850             UNTIL ( 部位ＣＮＴ > 7 )
022860*
022870****        IF ( 負傷原因患者番号Ｗ(部位ＣＮＴ)  NOT = ZERO )  AND
022880        IF ( 負傷原因連番Ｗ(部位ＣＮＴ)      NOT = ZERO )
022890*
022900           IF カウンタ = ZERO
022910               MOVE 1   TO  カウンタ カウンタ２
022920               MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
022930               MOVE 負傷原因連番Ｗ(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
022940               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
022950           ELSE
022960              IF ( 負傷原因患者番号Ｗ(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
022970                 ( 負傷原因連番Ｗ(部位ＣＮＴ)      = 負傷連番ＣＷ     )
022980                 COMPUTE カウンタ２ = カウンタ２  +  1
022990                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
023000              ELSE
023010                 COMPUTE カウンタ = カウンタ  +  1
023020                 MOVE 1   TO  カウンタ２
023030                 MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
023040                 MOVE 負傷原因連番Ｗ(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
023050                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
023060              END-IF
023070           END-IF
023080        END-IF
023090     END-PERFORM.
023100**************************************************************************
023110*  負傷原因マスタより文章取得
023120**************************************************************************
023130     MOVE  ZERO   TO  カウンタ カウンタ２.
023140     PERFORM VARYING カウンタ FROM 1 BY 1
023150             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
023160** 健保は 区分 01
023170*         MOVE 01                        TO 負原－区分コード
023180*/健保：０１、労災：０２、自賠責：０３
023190         EVALUATE 受－保険種別
023200         WHEN 70
023210             MOVE 2 TO 負原－区分コード
023220         WHEN 80
023230             MOVE 3 TO 負原－区分コード
023240         WHEN OTHER
023250             MOVE 1 TO 負原－区分コード
023260         END-EVALUATE
023270         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
023280         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
023290         READ 負傷原因Ｆ
023300         NOT INVALID KEY
023310             INITIALIZE 負傷原因ＷＴ
023320             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
023330             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
023340             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
023350             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
023360             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
023370             PERFORM VARYING カウンタ２ FROM 1 BY 1
023380                     UNTIL ( カウンタ２ > 9 )  OR 
023390                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
023400                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
023410                WHEN 1
023420                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
023430                WHEN 2
023440                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
023450                WHEN 3
023460                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
023470                WHEN 4
023480                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
023490                WHEN 5
023500                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
023510                WHEN OTHER
023520                   CONTINUE
023530                END-EVALUATE
023540             END-PERFORM
023550*
023560             IF 負原－負傷原因入力区分 = 1
023570                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
023580                        負傷原因１ＷＴ  DELIMITED BY SIZE
023590                        負傷原因２ＷＴ  DELIMITED BY SIZE
023600                        負傷原因３ＷＴ  DELIMITED BY SIZE
023610                        負傷原因４ＷＴ  DELIMITED BY SIZE
023620                        負傷原因５ＷＴ  DELIMITED BY SIZE
023630                        INTO 負傷原因内容合成Ｗ(カウンタ)
023640                 END-STRING
023650             ELSE
023660                 INSPECT 負傷原因ＷＴ REPLACING ALL 全角空白 BY 半角空白
023670                 MOVE SPACE TO 文字１Ｗ 文字２Ｗ
023680                 MOVE 負傷原因ナンバーＮＷ TO 文字１Ｗ
023690                 MOVE 負傷原因１ＷＴ       TO 文字２Ｗ
023700                 CALL プログラム名Ｗ WITH C LINKAGE
023710                      USING BY REFERENCE 文字１Ｗ
023720                            BY REFERENCE 文字２Ｗ
023730                 MOVE 負傷原因２ＷＴ       TO 文字２Ｗ
023740                 CALL プログラム名Ｗ WITH C LINKAGE
023750                      USING BY REFERENCE 文字１Ｗ
023760                            BY REFERENCE 文字２Ｗ
023770                 MOVE 負傷原因３ＷＴ       TO 文字２Ｗ
023780                 CALL プログラム名Ｗ WITH C LINKAGE
023790                      USING BY REFERENCE 文字１Ｗ
023800                            BY REFERENCE 文字２Ｗ
023810                 MOVE 負傷原因４ＷＴ       TO 文字２Ｗ
023820                 CALL プログラム名Ｗ WITH C LINKAGE
023830                      USING BY REFERENCE 文字１Ｗ
023840                            BY REFERENCE 文字２Ｗ
023850                 MOVE 負傷原因５ＷＴ       TO 文字２Ｗ
023860                 CALL プログラム名Ｗ WITH C LINKAGE
023870                      USING BY REFERENCE 文字１Ｗ
023880                            BY REFERENCE 文字２Ｗ
023890                  MOVE 文字１Ｗ TO 負傷原因内容合成Ｗ(カウンタ)
023900             END-IF
023910*
023920         END-READ
023930     END-PERFORM.
023940*
023950     PERFORM 負傷原因セット.
023960*
023970*================================================================*
023980 詳細モード負傷原因処理 SECTION.
023990*
024000*******************************************************************
024010* 連印－負傷印刷行により、負傷原因データの並び替えを行う。
024020* ( 負傷原因情報Ｗを初期化し、退避済みの負傷原因情報ＳＷから
024030*         並び替えを行い、負傷原因情報Ｗを再設定する。)
024040*******************************************************************
024050*
024060     INITIALIZE 負傷原因情報Ｗ.
024070*
024080     PERFORM VARYING 詳細ＣＮＴ FROM 1 BY 1
024090                                UNTIL ( 詳細ＣＮＴ > 5)
024100*
024110        IF ( 連印－負傷印刷行(詳細ＣＮＴ) NOT = ZERO) AND
024120           ( 連印－原因印刷区分(詳細ＣＮＴ)   = 1)
024130*
024140           MOVE 連印－負傷印刷行(詳細ＣＮＴ) TO 印刷行ＳＷ
024150*
024160           MOVE 負傷患者番号ＳＷ(詳細ＣＮＴ) TO 負傷原因患者番号Ｗ(印刷行ＳＷ)
024170           MOVE 負傷連番ＳＷ(詳細ＣＮＴ) TO 負傷原因連番Ｗ(印刷行ＳＷ)
024180        END-IF
024190*
024200     END-PERFORM.
024210*
024220*================================================================*
024230 詳細モード負傷処理 SECTION.
024240*
024250*******************************************************************
024260* 連印－負傷印刷行により、負傷データの並び替えを行う。
024270* ( 負傷情報Ｗを初期化し、退避済みの負傷情報ＳＷから
024280*         並び替えを行い、負傷情報Ｗを再設定する。)
024290*******************************************************************
024300*
024310     INITIALIZE 負傷情報Ｗ.
024320*
024330     PERFORM VARYING 詳細ＣＮＴ FROM 1 BY 1
024340                                UNTIL ( 詳細ＣＮＴ > 5)
024350*
024360        IF 連印－負傷印刷行(詳細ＣＮＴ) NOT = ZERO
024370*
024380           MOVE 連印－負傷印刷行(詳細ＣＮＴ) TO 印刷行ＳＷ
024390*
024400           IF 連印－部位印刷区分(詳細ＣＮＴ) = 1
024410*
024420              MOVE 負傷名ＳＷ(詳細ＣＮＴ) TO 負傷名Ｗ(印刷行ＳＷ)
024430              MOVE 負傷年ＳＷ(詳細ＣＮＴ) TO 負傷年Ｗ(印刷行ＳＷ)
024440              MOVE 負傷月ＳＷ(詳細ＣＮＴ) TO 負傷月Ｗ(印刷行ＳＷ)
024450              MOVE 負傷日ＳＷ(詳細ＣＮＴ) TO 負傷日Ｗ(印刷行ＳＷ)
024460              MOVE 施術開始年ＳＷ(詳細ＣＮＴ) TO 施術開始年Ｗ(印刷行ＳＷ)
024470              MOVE 施術開始月ＳＷ(詳細ＣＮＴ) TO 施術開始月Ｗ(印刷行ＳＷ)
024480              MOVE 施術開始日ＳＷ(詳細ＣＮＴ) TO 施術開始日Ｗ(印刷行ＳＷ)
024490           END-IF
024500*
024510           IF 連印－転帰印刷区分(詳細ＣＮＴ) = 1
024520              MOVE 施術終了年ＳＷ(詳細ＣＮＴ) TO 施術終了年Ｗ(印刷行ＳＷ)
024530              MOVE 施術終了月ＳＷ(詳細ＣＮＴ) TO 施術終了月Ｗ(印刷行ＳＷ)
024540              MOVE 施術終了日ＳＷ(詳細ＣＮＴ) TO 施術終了日Ｗ(印刷行ＳＷ)
024550              MOVE 治癒チェックＳＷ(詳細ＣＮＴ) TO 治癒チェックＷ(印刷行ＳＷ)
024560              MOVE 中止チェックＳＷ(詳細ＣＮＴ) TO 中止チェックＷ(印刷行ＳＷ)
024570              MOVE 転医チェックＳＷ(詳細ＣＮＴ) TO 転医チェックＷ(印刷行ＳＷ)
024580           END-IF
024590        END-IF
024600*
024610     END-PERFORM.
024620*
024630*================================================================*
024640 負傷データ退避 SECTION.
024650*
024660*********************************************************************
024670* ① 負傷原因取得のため、負傷データＦから負傷原因Ｗに退避する。
024680* ② 詳細モード用  連印－負傷行印刷により、印刷位置を並び替えるため、
024690*                  負傷情報Ｗ  から負傷情報ＳＷに、
024700*                  負傷原因Ｗ  から負傷原因ＳＷに退避する。
024710*********************************************************************
024720*
024730     MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷原因患者番号Ｗ(部位ＣＮＴ).
024740     MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷原因連番Ｗ(部位ＣＮＴ).
024750*--------------------------------------------------------------------*
024760     MOVE 負傷名Ｗ(部位ＣＮＴ) TO 負傷名ＳＷ(部位ＣＮＴ).
024770*
024780     MOVE 負傷年Ｗ(部位ＣＮＴ) TO 負傷年ＳＷ(部位ＣＮＴ).
024790     MOVE 負傷月Ｗ(部位ＣＮＴ) TO 負傷月ＳＷ(部位ＣＮＴ).
024800     MOVE 負傷日Ｗ(部位ＣＮＴ) TO 負傷日ＳＷ(部位ＣＮＴ).
024810*
024820     MOVE 施術開始年Ｗ(部位ＣＮＴ) TO 施術開始年ＳＷ(部位ＣＮＴ).
024830     MOVE 施術開始月Ｗ(部位ＣＮＴ) TO 施術開始月ＳＷ(部位ＣＮＴ).
024840     MOVE 施術開始日Ｗ(部位ＣＮＴ) TO 施術開始日ＳＷ(部位ＣＮＴ).
024850*
024860     MOVE 施術終了年Ｗ(部位ＣＮＴ) TO 施術終了年ＳＷ(部位ＣＮＴ).
024870     MOVE 施術終了月Ｗ(部位ＣＮＴ) TO 施術終了月ＳＷ(部位ＣＮＴ).
024880     MOVE 施術終了日Ｗ(部位ＣＮＴ) TO 施術終了日ＳＷ(部位ＣＮＴ).
024890*
024900     MOVE 治癒チェックＷ(部位ＣＮＴ) TO 治癒チェックＳＷ(部位ＣＮＴ).
024910     MOVE 中止チェックＷ(部位ＣＮＴ) TO 中止チェックＳＷ(部位ＣＮＴ).
024920     MOVE 転医チェックＷ(部位ＣＮＴ) TO 転医チェックＳＷ(部位ＣＮＴ).
024930*     MOVE 施術累計回数Ｗ(部位ＣＮＴ) TO 施術累計回数ＳＷ(部位ＣＮＴ).
024940*
024950     MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号ＳＷ(部位ＣＮＴ).
024960     MOVE 負傷原因連番Ｗ(部位ＣＮＴ)     TO 負傷連番ＳＷ(部位ＣＮＴ).
024970*
024980*================================================================*
024990 負傷原因セット SECTION.
025000*
025010**************************************************************************
025020*  文章が1行を超える時は、複数行に分解する。
025030**************************************************************************
025040     MOVE  ZERO   TO  カウンタ カウンタ２.
025050     PERFORM VARYING カウンタ FROM 1 BY 1
025060             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
025070*
025080          INITIALIZE 負傷原因内容分解ＸＷ
025090          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
025100          IF  負傷原因内容１ＸＷ  NOT = SPACE
025110              COMPUTE カウンタ２ = カウンタ２  +  1
025120              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
025130          END-IF
025140          IF  負傷原因内容２ＸＷ  NOT = SPACE
025150              COMPUTE カウンタ２ = カウンタ２  +  1
025160              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
025170          END-IF
025180          IF  負傷原因内容３ＸＷ  NOT = SPACE
025190              COMPUTE カウンタ２ = カウンタ２  +  1
025200              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
025210          END-IF
025220          IF  負傷原因内容４ＸＷ  NOT = SPACE
025230              COMPUTE カウンタ２ = カウンタ２  +  1
025240              MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
025250          END-IF
025260          IF  負傷原因内容５ＸＷ  NOT = SPACE
025270              COMPUTE カウンタ２ = カウンタ２  +  1
025280              MOVE 負傷原因内容５ＸＷ  TO 負傷原因Ｗ(カウンタ２)
025290          END-IF
025300          IF  負傷原因内容６ＸＷ  NOT = SPACE
025310              COMPUTE カウンタ２ = カウンタ２  +  1
025320              MOVE 負傷原因内容６ＸＷ  TO 負傷原因Ｗ(カウンタ２)
025330          END-IF
025340*
025350     END-PERFORM.
025360*
025370*================================================================*
025380 印刷処理 SECTION.
025390*
025400     EVALUATE 印刷モードＦＷＲ
025410     WHEN 0
025420         PERFORM 印刷処理２
025430         PERFORM 印刷処理３
025440     WHEN 1
025450         PERFORM 印刷処理２
025460     WHEN 2
025470         PERFORM 印刷処理３
025480     WHEN 4
025490         IF 連印－保険証印刷区分 = 1
025500             PERFORM 印刷処理２
025510         END-IF
025520             PERFORM 印刷処理３
025530     END-EVALUATE.
025540*================================================================*
025550 印刷処理１ SECTION.
025560*
025570     MOVE "YAS662P" TO  定義体名Ｐ.
025580*     MOVE "SCREEN"  TO  項目群名Ｐ.
025590     MOVE "GRP001"  TO  項目群名Ｐ.
025600     WRITE YAS662P.
025610*     WRITE 印刷レコード.
025620     PERFORM エラー処理Ｐ.
025630*================================================================*
025640 印刷処理２ SECTION.
025650*
025660     MOVE "YAS662P"  TO  定義体名Ｐ.
025670     MOVE "GRP001"   TO  項目群名Ｐ.
025680     WRITE YAS662P.
025690*     WRITE 印刷レコード.
025700     PERFORM エラー処理Ｐ.
025710*================================================================*
025720 印刷処理３ SECTION.
025730*
025740     MOVE "YAS662P"  TO  定義体名Ｐ.
025750     MOVE "GRP002"   TO  項目群名Ｐ.
025760     WRITE YAS662P.
025770*     WRITE 印刷レコード.
025780     PERFORM エラー処理Ｐ.
025790*================================================================*
025800 エラー処理Ｐ SECTION.
025810*
025820     IF 通知情報Ｐ NOT = "00"
025830         DISPLAY NC"帳票エラー"              UPON CONS
025840         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
025850         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
025860         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
025870         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
025880                                             UPON CONS
025890*-----------------------------------------*
025900         CALL "actcshm"  WITH C LINKAGE
025910*-----------------------------------------*
025920         ACCEPT  キー入力 FROM CONS
025930         PERFORM ファイル閉鎖
025940         EXIT PROGRAM
025950     END-IF.
025960*================================================================*
025970 名称埋込処理 SECTION.
025980*
025990     EVALUATE 受－保険種別
026000     WHEN 05
026010         MOVE 2          TO レセ－レセ種別
026020     WHEN 50 THRU 60
026030         MOVE 3          TO レセ－レセ種別
026040     WHEN 70
026050         MOVE 4          TO レセ－レセ種別
026060     WHEN 80
026070         MOVE 5          TO レセ－レセ種別
026080     WHEN 85
026090         MOVE 7          TO レセ－レセ種別
026100     WHEN 90
026110         MOVE 6          TO レセ－レセ種別
026120     WHEN 91
026130         MOVE 8          TO レセ－レセ種別
026140     WHEN OTHER
026150         MOVE 1          TO レセ－レセ種別
026160     END-EVALUATE.
026170     MOVE 受－施術和暦 TO レセ－施術和暦.
026180     MOVE 受－施術年   TO レセ－施術年.
026190     MOVE 受－施術月   TO レセ－施術月.
026200     MOVE 受－患者番号 TO レセ－患者番号.
026210     MOVE 受－枝番     TO レセ－枝番.
026220     READ レセプトＦ
026230     NOT INVALID KEY
026240*
026250         STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
026260                レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
026270                負傷名称Ｗ                    DELIMITED BY SPACE
026280           INTO 負傷名Ｗ(部位ＣＮＴ)
026290         END-STRING
026300*
026310     END-READ.
026320*
026330*================================================================*
026340******************************************************************
026350 END PROGRAM YAS662.
026360******************************************************************
