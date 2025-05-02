000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YJB6622.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*         ＪＢ用
000100*　　　　　カルテ印刷（柔+ｳｨﾝﾄﾞｳｽﾞ版）
000110*         MED = YAW660 YJB6622P
      */2014/10/15 受給者番号15桁対応
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2020-07-01
000140 DATE-COMPILED.          2020-07-01
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
002190*
002200 FD  印刷ファイル.
002210     COPY YJB6622P        OF  XMDLIB.
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
      */負傷原因を部位毎の欄に印刷するように変更0302
       01 負傷原因ＣＮＴ                     PIC 9(2)  VALUE ZERO.
       01 部位番号Ｗ                         PIC 9(1)  VALUE ZERO.
      *
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
002880       05 負傷原因内容１ＸＷ           PIC X(44)  VALUE SPACE.
002890       05 負傷原因内容２ＸＷ           PIC X(44)  VALUE SPACE.
002900       05 負傷原因内容３ＸＷ           PIC X(44)  VALUE SPACE.
002890       05 負傷原因内容４ＸＷ           PIC X(44)  VALUE SPACE.
002900       05 負傷原因内容５ＸＷ           PIC X(44)  VALUE SPACE.
002900       05 負傷原因内容６ＸＷ           PIC X(44)  VALUE SPACE.
002900       05 負傷原因内容７ＸＷ           PIC X(44)  VALUE SPACE.
002910       05 負傷原因内容８ＸＷ           PIC X(8)  VALUE SPACE.
002870    03 負傷原因内容分解２ＸＷ.
002880       05 負傷原因内容２１ＸＷ         PIC X(44)  VALUE SPACE.
002890       05 負傷原因内容２２ＸＷ         PIC X(44)  VALUE SPACE.
002900       05 負傷原因内容２３ＸＷ         PIC X(44)  VALUE SPACE.
002910       05 負傷原因内容２４ＸＷ         PIC X(44)  VALUE SPACE.
002890       05 負傷原因内容２５ＸＷ         PIC X(44)  VALUE SPACE.
002900       05 負傷原因内容２６ＸＷ         PIC X(44)  VALUE SPACE.
002910       05 負傷原因内容２７ＸＷ         PIC X(44)  VALUE SPACE.
002910       05 負傷原因内容２８ＸＷ         PIC X(8)   VALUE SPACE.
002870    03 負傷原因内容分解３ＸＷ.
002880       05 負傷原因内容３１ＸＷ         PIC X(30)  VALUE SPACE.
002890       05 負傷原因内容３２ＸＷ         PIC X(30)  VALUE SPACE.
002900       05 負傷原因内容３３ＸＷ         PIC X(30)  VALUE SPACE.
002910       05 負傷原因内容３４ＸＷ         PIC X(30)  VALUE SPACE.
002910       05 負傷原因内容３５ＸＷ         PIC X(30)  VALUE SPACE.
002910       05 負傷原因内容３６ＸＷ         PIC X(30)  VALUE SPACE.
002910       05 負傷原因内容３７ＸＷ         PIC X(30)  VALUE SPACE.
002910       05 負傷原因内容３８ＸＷ         PIC X(30)  VALUE SPACE.
002910       05 負傷原因内容３９ＸＷ         PIC X(30)  VALUE SPACE.
002910       05 負傷原因内容３１０ＸＷ       PIC X(30)  VALUE SPACE.
002910       05 負傷原因内容３１１ＸＷ       PIC X(16)   VALUE SPACE.
002870    03 負傷原因内容分解４ＸＷ.
002880       05 負傷原因内容４１ＸＷ         PIC X(26)  VALUE SPACE.
002890       05 負傷原因内容４２ＸＷ         PIC X(26)  VALUE SPACE.
002900       05 負傷原因内容４３ＸＷ         PIC X(26)  VALUE SPACE.
002910       05 負傷原因内容４４ＸＷ         PIC X(26)  VALUE SPACE.
002910       05 負傷原因内容４５ＸＷ         PIC X(26)  VALUE SPACE.
002910       05 負傷原因内容４６ＸＷ         PIC X(26)  VALUE SPACE.
002910       05 負傷原因内容４７ＸＷ         PIC X(26)  VALUE SPACE.
002910       05 負傷原因内容４８ＸＷ         PIC X(26)  VALUE SPACE.
002910       05 負傷原因内容４９ＸＷ         PIC X(26)  VALUE SPACE.
002910       05 負傷原因内容４１０ＸＷ       PIC X(26)  VALUE SPACE.
002910       05 負傷原因内容４１１ＸＷ       PIC X(26)  VALUE SPACE.
002910       05 負傷原因内容４１２ＸＷ       PIC X(26)  VALUE SPACE.
002910       05 負傷原因内容４１３ＸＷ       PIC X(4)   VALUE SPACE.
002920*
002930** 負傷原因印刷区分用
002940 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
002950*
002960* 社保用
002970 01 接尾語区分Ｗ                       PIC 9     VALUE ZERO.
002980*
002990****************
003000* 連結項目待避 *
003010****************
003020*    ************
003030*    * 印刷キー *
003040*    ************
003050 01 対象データＷＲ.
003060    03 施術和暦年月ＷＲ.
003070       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
003080       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
003090       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
003100    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
003110    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
003120    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
003130    03 被保険者カナＷＲ                 PIC X(20) VALUE SPACE.
003140    03 患者コードＷＲ.
003150       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
003160       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
003170    03 印刷モードＦＷＲ                 PIC 9(1)  VALUE ZERO.
003180    03 提出年月日ＷＲ.
003190       05 提出年ＷＲ                    PIC 9(2)  VALUE ZERO.
003200       05 提出月ＷＲ                    PIC 9(2)  VALUE ZERO.
003210       05 提出日ＷＲ                    PIC 9(2)  VALUE ZERO.
003220    03 段数ＷＲ                         PIC 9(1)  VALUE ZERO.
003230************
003240* 料金情報 *
003250************
003260*
003270 01 印刷負担率Ｗ                        PIC 9(1)  VALUE ZERO.
003280**************
003290* 受診者情報 *
003300**************
003310 01 受診者情報Ｗ.
003320    03 患者コードＷ.
003330       05 患者番号Ｗ                   PIC 9(6)  VALUE ZERO.
003340       05 枝番Ｗ                       PIC X(1)  VALUE SPACE.
003350    03 市町村番号Ｗ.
003360       05 印刷市町村番号Ｗ             PIC X(8)  VALUE SPACE.
003370       05 FILLER                       PIC X(2).
003380    03 受益者番号Ｗ.
003390       05 印刷受益者番号Ｗ             PIC X(7)  VALUE SPACE.
003400       05 FILLER                       PIC X(13).
003410    03 費用負担者番号Ｗ.
003420       05 印刷費用負担者番号Ｗ         PIC X(8)  VALUE SPACE.
003430       05 FILLER                       PIC X(2).
      */10桁印字/0412  15桁印刷/1410
003440    03 受益者番号助成Ｗ.
003450       05 印刷受益者番号助成Ｗ         PIC X(7)  VALUE SPACE.
003450       05 印刷受益者番号助成２Ｗ         PIC X(8)  VALUE SPACE.
003460       05 FILLER                       PIC X(10).
003470    03 記号Ｗ                          PIC X(24)  VALUE SPACE.
003480    03 番号Ｗ.
003490       05 印刷番号Ｗ                   PIC X(30)  VALUE SPACE.
003500*       05 FILLER                       PIC X(18).
003510    03 被保険者情報Ｗ.
003520       05 被保険者カナＷ               PIC X(50)  VALUE SPACE.
003530       05 被保険者氏名Ｗ               PIC X(50)  VALUE SPACE.
003540*
003550*       05 被保険者和暦チェックＷ.
003560*          07 被保険者明治Ｗ            PIC N(1)  VALUE SPACE.
003570*          07 被保険者大正Ｗ            PIC N(1)  VALUE SPACE.
003580*          07 被保険者昭和Ｗ            PIC N(1)  VALUE SPACE.
003590       05 被保険者性別チェックＷ.
003600          07 被保険者男チェックＷ      PIC N(1)  VALUE SPACE.
003610          07 被保険者女チェックＷ      PIC N(1)  VALUE SPACE.
003620       05 被保険者性別Ｗ               PIC 9     VALUE ZERO.
003630       05 被保険者和暦チェックＷ.
003640          07 被保険者明治チェックＷ    PIC N(1)  VALUE SPACE.
003650          07 被保険者大正チェックＷ    PIC N(1)  VALUE SPACE.
003660          07 被保険者昭和チェックＷ    PIC N(1)  VALUE SPACE.
003670          07 被保険者平成チェックＷ    PIC N(1)  VALUE SPACE.
003670          07 被保険者令和チェックＷ    PIC N(1)  VALUE SPACE.
003680       05 被保険者生年月日Ｗ.
003690          07 被保険者和暦Ｗ            PIC 9     VALUE ZERO.
003700          07 被保険者年Ｗ              PIC 9(2)  VALUE ZERO.
003710          07 被保険者月Ｗ              PIC 9(2)  VALUE ZERO.
003720          07 被保険者日Ｗ              PIC 9(2)  VALUE ZERO.
003730       05 被保険者年齢Ｗ               PIC 9(3)  VALUE ZERO.
003740*
003750       05 被保険者郵便番号Ｗ.
003760          07 被保険者郵便番号１Ｗ      PIC X(3)  VALUE SPACE.
003770          07 被保険者郵便番号２Ｗ      PIC X(4)  VALUE SPACE.
003780       05 被保険者住所Ｗ.
003790          07 被保険者住所１Ｗ          PIC X(40) VALUE SPACE.
003800          07 被保険者住所２Ｗ          PIC X(40) VALUE SPACE.
003810       05 被保険者電話番号Ｗ           PIC X(15) VALUE SPACE.
003820       05 分割電話番号Ｗ.
003830          07 分割電話番号１Ｗ          PIC X(7)  VALUE SPACE.
003840          07 分割電話番号２Ｗ          PIC X(8)  VALUE SPACE.
003850          07 分割電話番号３Ｗ          PIC X(8)  VALUE SPACE.
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
004030          07 資格令和チェックＷ        PIC N(1)  VALUE SPACE.
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
004170          07 令和チェックＷ            PIC N(1)  VALUE SPACE.
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
004410       05 自衛チェックＷ               PIC N(1) VALUE SPACE.
004420       05 公費チェックＷ               PIC N(1) VALUE SPACE.
004430       05 自費チェックＷ               PIC N(1) VALUE SPACE.
004440       05 患者郵便番号Ｗ.
004450          07 患者郵便番号１Ｗ          PIC X(3)  VALUE SPACE.
004460          07 患者郵便番号２Ｗ          PIC X(4)  VALUE SPACE.
004470       05 患者住所Ｗ.
004480          07 患者住所１Ｗ              PIC X(40) VALUE SPACE.
004490          07 患者住所２Ｗ              PIC X(40) VALUE SPACE.
004500       05 患者電話番号Ｗ               PIC X(15) VALUE SPACE.
004520 01 負傷原因情報Ｗ.
004530    03 負傷原因コードＷ  OCCURS   9.
004540       09 負傷原因患者番号Ｗ           PIC 9(6) VALUE ZERO.
004550       09 負傷原因連番Ｗ               PIC 9(4) VALUE ZERO.
004560****************
004570* 詳細用ワーク *
004580****************
004590* 連印－負傷印刷行用ワーク
004600 01 印刷行ＳＷ                           PIC 9(1) VALUE ZERO.
004610*
004620 01 負傷原因情報ＳＷ.
004630    03 負傷原因コードＳＷ  OCCURS   9.
004640       09 負傷患者番号ＳＷ               PIC 9(6) VALUE ZERO.
004650       09 負傷連番ＳＷ                   PIC 9(4) VALUE ZERO.
004660*
004670 01 負傷情報ＳＷ.
004680    03 負傷データＳＷ  OCCURS   9.
004690       05 会部位コードＳＷ               PIC X(4)  VALUE SPACE.
004700       05 部位ＣＮＴＳＷ                 PIC 9(1)  VALUE ZERO.
004710       05 負傷種別ＳＷ                   PIC 9(2)  VALUE ZERO.
004720       05 部位ＳＷ                       PIC 9(2)  VALUE ZERO.
004730       05 左右区分ＳＷ                   PIC 9(1)  VALUE ZERO.
004740       05 負傷位置番号ＳＷ               PIC 9(2)  VALUE ZERO.
004750       05 負傷名ＳＷ                     PIC N(18) VALUE SPACE.
004760       05 負傷年月日ＳＷ.
004770          07 負傷和暦ＳＷ                PIC 9(1)  VALUE ZERO.
004780          07 負傷年ＳＷ                  PIC 9(2)  VALUE ZERO.
004790          07 負傷月ＳＷ                  PIC 9(2)  VALUE ZERO.
004800          07 負傷日ＳＷ                  PIC 9(2)  VALUE ZERO.
004810          07 負傷年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
004820       05 施術開始年月日ＳＷ.
004830          07 施術開始和暦ＳＷ            PIC 9(1)  VALUE ZERO.
004840          07 施術開始年ＳＷ              PIC 9(2)  VALUE ZERO.
004850          07 施術開始月ＳＷ              PIC 9(2)  VALUE ZERO.
004860          07 施術開始日ＳＷ              PIC 9(2)  VALUE ZERO.
004870          07 施開年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
004880       05 施術終了年月日ＳＷ.
004890          07 施術終了和暦ＳＷ            PIC 9(1)  VALUE ZERO.
004900          07 施術終了年ＳＷ              PIC 9(2)  VALUE ZERO.
004910          07 施術終了月ＳＷ              PIC 9(2)  VALUE ZERO.
004920          07 施術終了日ＳＷ              PIC 9(2)  VALUE ZERO.
004930          07 施終年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
004940       05 開始年月日取得フラグＳ         PIC X(3)  VALUE SPACE.
004950       05 転帰ＳＷ                       PIC N(4)  VALUE SPACE.
004960       05 転帰チェックＳＷ.
004970          07 治癒チェックＳＷ            PIC N(1)  VALUE SPACE.
004980          07 中止チェックＳＷ            PIC N(1)  VALUE SPACE.
004990          07 転医チェックＳＷ            PIC N(1)  VALUE SPACE.
005000       05 施術累計回数ＳＷ               PIC 9(4)  VALUE ZERO.
005010*/負傷時間を追加
005020       05 負傷時間ＳＷ.
005030          07 負傷時ＳＷ                  PIC 9(2) VALUE ZERO.
005040          07 負傷分ＳＷ                  PIC 9(2) VALUE ZERO.
      */負傷原因を部位毎の欄に印刷するように変更0302
             05 負傷原因ＳＷ                   PIC X(302) VALUE SPACE.
005050**************
005060* 事業所情報 *
005070**************
005080 01 事業所情報Ｗ.
005090    03 事業所名称Ｗ.
005100       05 印刷事業所名称Ｗ             PIC X(60)  VALUE SPACE.
005110    03 事業所郵便番号Ｗ.
005120       05 事業所郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
005130       05 事業所郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
005140    03 事業所住所Ｗ.
005150       05 印刷事業所住所１Ｗ           PIC X(50)  VALUE SPACE.
005150       05 印刷事業所住所２Ｗ           PIC X(50)  VALUE SPACE.
005170**************
005180* 請求先情報 *
005190**************
005200 01 請求先情報Ｗ.
005210    03 保険者番号Ｗ.
005220       05 印刷保険者番号Ｗ             PIC X(8)  VALUE SPACE.
005230       05 FILLER                       PIC X(2).
005240    03 請求先名称Ｗ.
005250       05 印刷請求先名称Ｗ             PIC X(40) VALUE SPACE.
005260    03 支部名Ｗ.
005270       05 印刷支部名Ｗ                 PIC X(40) VALUE SPACE.
005280    03 請求先名支部名Ｗ.
005290       05 印刷請求先名支部名１Ｗ       PIC X(40) VALUE SPACE.
005300       05 印刷請求先名支部名２Ｗ       PIC X(40) VALUE SPACE.
005310    03 保険者呼名Ｗ.
005320*       05 印刷保険者呼名Ｗ             PIC N(7)  VALUE SPACE.
005330       05 印刷保険者呼名Ｗ             PIC X(14)  VALUE SPACE.
005340    03 請求先郵便番号Ｗ.
005350       05 請求先郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
005360       05 請求先郵便番号区切Ｗ         PIC X(1)  VALUE SPACE.
005370       05 請求先郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
005380    03 請求先住所１Ｗ.
005390       05 印刷請求先住所１Ｗ           PIC X(40) VALUE SPACE.
005400    03 請求先住所２Ｗ.
005410       05 印刷請求先住所２Ｗ           PIC X(40) VALUE SPACE.
005420*       05 FILLER                       PIC X(5).
005430    03 保険者住所Ｗ.
005440       05 保険者住所１Ｗ               PIC X(40) VALUE SPACE.
005450       05 保険者住所２Ｗ               PIC X(40) VALUE SPACE.
005460****************
005470* 負傷データＦ *
005480****************
005490 01 負傷情報Ｗ.
005500    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
005510    03 負傷データ情報Ｗ  OCCURS   9.
005520       05 会部位コードＷ               PIC X(4)  VALUE SPACE.
005530       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
005540       05 負傷種別Ｗ                   PIC 9(2)  VALUE ZERO.
005550       05 部位Ｗ                       PIC 9(2)  VALUE ZERO.
005560       05 左右区分Ｗ                   PIC 9(1)  VALUE ZERO.
005570       05 負傷位置番号Ｗ               PIC 9(2)  VALUE ZERO.
005580       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
005590       05 負傷年月日Ｗ.
005600          07 負傷和暦Ｗ                PIC 9(1)  VALUE ZERO.
005610          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
005620          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
005630          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
005640          07 負傷年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005650       05 施術開始年月日Ｗ.
005660          07 施術開始和暦Ｗ            PIC 9(1)  VALUE ZERO.
005670          07 施術開始年Ｗ              PIC 9(2)  VALUE ZERO.
005680          07 施術開始月Ｗ              PIC 9(2)  VALUE ZERO.
005690          07 施術開始日Ｗ              PIC 9(2)  VALUE ZERO.
005700          07 施開年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005710       05 施術終了年月日Ｗ.
005720          07 施術終了和暦Ｗ            PIC 9(1)  VALUE ZERO.
005730          07 施術終了年Ｗ              PIC 9(2)  VALUE ZERO.
005740          07 施術終了月Ｗ              PIC 9(2)  VALUE ZERO.
005750          07 施術終了日Ｗ              PIC 9(2)  VALUE ZERO.
005760          07 施終年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005770       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
005780       05 転帰Ｗ                       PIC N(4)  VALUE SPACE.
005790       05 転帰チェックＷ.
005800          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
005810          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
005820          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
005830*/負傷時間を追加
005840       05 負傷時間Ｗ.
005850          07 負傷時Ｗ                  PIC 9(2) VALUE ZERO.
005860          07 負傷分Ｗ                  PIC 9(2) VALUE ZERO.
      */負傷原因を部位毎の欄に印刷するように変更0302
             05 負傷原因Ｗ                   PIC X(302) VALUE SPACE.
005870*/
005880 01 負傷時間ＸＷ.
005890   03 負傷時ＸＷ                       PIC X(2) VALUE SPACE.
005900   03 負傷分ＸＷ                       PIC X(2) VALUE SPACE.
005910*
005920******************
005930* 負担率チェック *
005940******************
005950 01 負担率チェックＷ.
005960    03 ０割チェックＷ                  PIC N(1)  VALUE SPACE.
005970    03 １割チェックＷ                  PIC N(1)  VALUE SPACE.
005980    03 ２割チェックＷ                  PIC N(1)  VALUE SPACE.
005990    03 ３割チェックＷ                  PIC N(1)  VALUE SPACE.
006000**********
006010* 合計行 *
006020**********
006030 01 合計行Ｗ.
006040    03 請求終了日Ｗ                    PIC 9(2) VALUE ZERO.
006050    03 対象西暦Ｗ                      PIC 9(4) VALUE ZERO.
006060    03 商Ｗ                            PIC 9(3) VALUE ZERO.
006070    03 余Ｗ                            PIC 9(3) VALUE ZERO.
006080*/年月毎持つ保険者データのワーク
006090 01 保負担率区分Ｗ                     PIC 9    VALUE ZERO.
006100 01 保本人負担率Ｗ                     PIC 9(3) VALUE ZERO.
006110 01 保家族負担率Ｗ                     PIC 9(3) VALUE ZERO.
006120 01 保本人負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
006130 01 保家族負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
006140*
006150 01 比較和暦年月Ｗ.
006160    03 比較和暦Ｗ                      PIC 9    VALUE ZERO.
006170    03 比較年Ｗ                        PIC 9(2) VALUE ZERO.
006180    03 比較月Ｗ                        PIC 9(2) VALUE ZERO.
006190*
006200*/老人保険改正の対応0101
006210 01 老人負担計算区分Ｗ                 PIC 9    VALUE ZERO.
006220 01 負担率用種別Ｗ                     PIC 9(2) VALUE ZERO.
006230 01 助成負担区分Ｗ                     PIC 9(2) VALUE ZERO.
006240*
006241*
006242** 助成負担率用(14/10～)
006243 01 助成負担率ＷＷ.
006244    03 助成負担率Ｗ                    PIC 9(3) VALUE ZERO.
006245    03 助成負担率Ｗ１                  PIC 9    VALUE ZERO.
006246    03 助成負担率文字Ｗ１              PIC X    VALUE SPACE.
006247    03 助成負担率表示Ｗ                PIC X(6) VALUE SPACE.
006248*
006250******************************************************************
006260 01 印刷制御.
006270     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
006280     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
006290     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
006300     03 拡張制御Ｐ.
006310         05 端末制御Ｐ.
006320             07 移動方向Ｐ             PIC X(1)  VALUE SPACE.
006330             07 移動行数Ｐ             PIC 9(3)  VALUE ZERO.
006340         05 詳細制御Ｐ                 PIC X(2)  VALUE SPACE.
006350     03 通知情報Ｐ                     PIC X(2)  VALUE SPACE.
006360     03 ユニット名Ｐ                   PIC X(8)  VALUE SPACE.
006370*
006380 01 計算機西暦年Ｗ                     PIC 9(2)  VALUE ZERO.
006390* 日付ＷＯＲＫ
006400 01 和暦終了年Ｗ                       PIC 9(4)  VALUE ZERO.
006410 01 計算機西暦.
006420    03 計算機西暦年                    PIC 9(4)  VALUE ZERO.
006430    03 計算機西暦月日                  PIC 9(4)  VALUE ZERO.
006440 01 計算機西暦Ｒ REDEFINES 計算機西暦.
006450    03 計算機世紀                      PIC 9(2).
006460    03 計算機日付                      PIC 9(6).
006470    03 計算機日付Ｒ REDEFINES 計算機日付.
006480       05 計算機年月                   PIC 9(4).
006490       05 計算機年月Ｒ REDEFINES 計算機年月.
006500         07 計算機年                   PIC 9(2).
006510         07 計算機月                   PIC 9(2).
006520       05 計算機日                     PIC 9(2).
006530*
002400** バーコード区分用
002410 01 バーコード使用区分Ｗ               PIC 9 VALUE ZERO.
002420*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
006540******************************************************************
006550*                          連結項目                              *
006560******************************************************************
006570*
006580**********************
006590* メッセージ表示キー *
006600**********************
006610 01 連メ－キー IS EXTERNAL.
006620    03  連メ－メッセージ                 PIC N(20).
006630*
006640****************
006650* 連結項目待避 *
006660****************
006670*    ************
006680*    * 料金情報 *
006690*    ************
006700*    月毎の料金
006710***********************
006720 01 連施－料金１ IS EXTERNAL.
006730   03 連施－本人負担率                 PIC 9(3).
006740************
006750* 印刷キー *
006760************
006770 01 連印－対象データ IS EXTERNAL.
006780    03 連印－施術和暦年月.
006790       05 連印－施術和暦                  PIC 9(1).
006800       05 連印－施術年                    PIC 9(2).
006810       05 連印－施術月                    PIC 9(2).
006820    03 連印－保険種別                     PIC 9(2).
006830    03 連印－保険者番号                   PIC X(10).
006840    03 連印－本人家族区分                 PIC 9(1).
006850    03 連印－被保険者カナ                 PIC X(20).
006860    03 連印－患者コード.
006870       05 連印－患者番号                  PIC 9(6).
006880       05 連印－枝番                      PIC X(1).
006890    03 連印－印刷モードＦ                 PIC 9(1).
006900*/詳細
006910    03 連印－保険証印刷区分               PIC 9(1).
006920    03 連印－負傷詳細 OCCURS 7.
006930       05 連印－負傷印刷行                PIC 9(1).
006940       05 連印－部位印刷区分              PIC 9(1).
006950       05 連印－転帰印刷区分              PIC 9(1).
006960       05 連印－原因印刷区分              PIC 9(1).
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
007308*
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
007309******************************************************************
007310*                      PROCEDURE  DIVISION                       *
007320******************************************************************
007330 PROCEDURE               DIVISION.
007340************
007350*           *
007360* 初期処理   *
007370*           *
007380************
002570     PERFORM プリンタファイル作成.
007390     PERFORM 初期化.
007400************
007410*           *
007420* 主処理     *
007430*           *
007440************
007450* 印刷
007460     PERFORM 連結項目待避.
007470     PERFORM 印刷セット
007480     PERFORM 印刷処理
007490************
007500*           *
007510* 終了処理   *
007520*           *
007530************
007540     PERFORM 終了処理.
007550     EXIT PROGRAM.
007560*
007570*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YJB6622"              TO Ｈ連ＰＲＴＦ－帳票プログラム名.
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
007580*================================================================*
007590 初期化 SECTION.
007600*
007610     PERFORM ファイルオープン.
007620*    /* 現在日付取得 */
007630     ACCEPT 計算機日付 FROM DATE.
007640*    /* 1980～2079年の間で設定 */
007650     IF 計算機年 > 80
007660         MOVE 19 TO 計算機世紀
007670     ELSE
007680         MOVE 20 TO 計算機世紀
007690     END-IF.
007700     PERFORM カレント元号取得.
007710     PERFORM 和暦終了年取得.
007720     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
007730*================================================================*
007740 カレント元号取得 SECTION.
007750*
007760     MOVE ZEROS TO 制－制御区分.
007770     READ 制御情報マスタ
007780     NOT INVALID KEY
007790         MOVE 制－カレント元号               TO カレント元号Ｗ
007800         MOVE 制－カルテ負傷原因印刷区分     TO 負傷原因印刷区分Ｗ
007700         MOVE 制－バーコード２３２Ｃ使用区分 TO バーコード使用区分Ｗ
007810     END-READ.
007820*
007830*================================================================*
007840 和暦終了年取得 SECTION.
007850*
007860     MOVE カレント元号Ｗ TO 元－元号区分.
007870     READ 元号マスタ
007880     INVALID KEY
007890         DISPLAY NC"指定和暦が登録されていません" UPON CONS
007900         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
007910                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007920         ACCEPT  キー入力 FROM CONS
007930         PERFORM 終了処理
007940         EXIT PROGRAM
007950     NOT INVALID KEY
007960         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
007970         MOVE 前和暦Ｗ TO 元－元号区分
007980         READ 元号マスタ
007990         INVALID KEY
008000             DISPLAY NC"指定和暦が登録されていません" UPON CONS
008010             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008020                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
008030             ACCEPT  キー入力 FROM CONS
008040             PERFORM 終了処理
008050             EXIT PROGRAM
008060         NOT INVALID KEY
008070             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
008080         END-READ
008090     END-READ.
008100*
008110*================================================================*
008120 連結項目待避 SECTION.
008130*
008140     INITIALIZE 対象データＷＲ.
008150     MOVE 連印－施術和暦      TO 施術和暦ＷＲ.
008160     MOVE 連印－施術年        TO 施術年ＷＲ.
008170     MOVE 連印－施術月        TO 施術月ＷＲ.
008180     MOVE 連印－保険種別      TO 保険種別ＷＲ.
008190     MOVE 連印－保険者番号    TO 保険者番号ＷＲ.
008200     MOVE 連印－本人家族区分  TO 本人家族区分ＷＲ.
008210     MOVE 連印－被保険者カナ  TO 被保険者カナＷＲ.
008220     MOVE 連印－患者番号      TO 患者番号ＷＲ.
008230     MOVE 連印－枝番          TO 枝番ＷＲ.
008240     MOVE 連印－印刷モードＦ  TO 印刷モードＦＷＲ.
008250*================================================================*
008260 ファイルオープン SECTION.
008270*
008280     OPEN INPUT   保険者マスタ
008290         MOVE NC"保険者" TO ファイル名.
008300         PERFORM オープンチェック.
008310     OPEN INPUT   元号マスタ
008320         MOVE NC"元号" TO ファイル名.
008330         PERFORM オープンチェック.
008340     OPEN INPUT   名称マスタ
008350         MOVE NC"名称" TO ファイル名.
008360         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
008400     OPEN INPUT   制御情報マスタ
008410         MOVE NC"制御情報" TO ファイル名.
008420         PERFORM オープンチェック.
008460     OPEN INPUT   請求先マスタ
008470         MOVE NC"請先" TO ファイル名.
008480         PERFORM オープンチェック.
008520     OPEN INPUT   受診者情報Ｆ.
008530         MOVE NC"受情" TO ファイル名.
008540         PERFORM オープンチェック.
008550     OPEN INPUT   施術記録Ｆ.
008560         MOVE NC"施記Ｆ" TO ファイル名.
008570         PERFORM オープンチェック.
008580     OPEN INPUT   負傷データＦ.
008590         MOVE NC"負傷" TO ファイル名.
008600         PERFORM オープンチェック.
008640     OPEN INPUT   負傷原因Ｆ.
008650         MOVE NC"負傷原因" TO ファイル名.
008660         PERFORM オープンチェック.
008670     OPEN INPUT 市町村マスタ.
008680         MOVE NC"市町村" TO ファイル名.
008690         PERFORM オープンチェック.
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
008740     OPEN I-O   印刷ファイル
008750         MOVE NC"印刷" TO ファイル名.
008760         PERFORM エラー処理Ｐ.
008770*================================================================*
008780 オープンチェック SECTION.
008790*
008800     IF 状態キー  NOT =  "00"
008810         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
008820         DISPLAY NC"状態キー：" 状態キー         UPON CONS
008830         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008840                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
008850         ACCEPT  キー入力 FROM CONS
008860         PERFORM ファイル閉鎖
008870         EXIT PROGRAM.
008880*================================================================*
008890 ファイル閉鎖 SECTION.
008900*
008910     CLOSE 印刷ファイル   保険者マスタ 元号マスタ
008920           制御情報マスタ 名称マスタ   請求先マスタ
008930           レセプトＦ     受診者情報Ｆ 施術記録Ｆ
008940           負傷データＦ   負傷原因Ｆ   生保情報Ｆ
008950           市町村マスタ   事業所マスタ 自賠責情報Ｆ
                 保険会社マスタ.
008960*================================================================*
008970 終了処理 SECTION.
008980*
008990     PERFORM ファイル閉鎖.
009000*================================================================*
009010 印刷セット SECTION.
009020*
009030     EVALUATE 印刷モードＦＷＲ
009040     WHEN 0
009050         PERFORM 印刷セット１
009060     WHEN 1
009070         PERFORM 印刷セット２
009080     WHEN 2
009090         PERFORM 印刷セット３
009120*/詳細モード用
009130     WHEN 4
009140         PERFORM 印刷セット５
009150     END-EVALUATE.
009160*
009170*================================================================*
009180 印刷セット１ SECTION.
009190*
009200     INITIALIZE YJB6622P.
009210     INITIALIZE 受診者情報Ｗ.
009220     INITIALIZE 事業所情報Ｗ.
009230     INITIALIZE 請求先情報Ｗ.
009240     INITIALIZE 負傷情報Ｗ.
009250     INITIALIZE 負担率チェックＷ.
009260     PERFORM 負担率取得.
009280     PERFORM 受診者情報取得.
009270     PERFORM 請求先情報取得.
009290     PERFORM 負傷データ取得.
009300*
009310     IF 負傷原因印刷区分Ｗ  NOT = 1 
009320        PERFORM 負傷原因取得
009330     END-IF.
      *
      ********     PERFORM テスト.
009340*
009350     PERFORM 印刷上部セット.
009360     PERFORM 印刷下部セット.
009370*================================================================*
009380 印刷セット２ SECTION.
009390*
009400     INITIALIZE YJB6622P.
009410     INITIALIZE 受診者情報Ｗ.
009420     INITIALIZE 事業所情報Ｗ.
009430     INITIALIZE 請求先情報Ｗ.
009440     INITIALIZE 負傷情報Ｗ.
009450     INITIALIZE 負担率チェックＷ.
009460     PERFORM 負担率取得.
009480     PERFORM 受診者情報取得.
009470     PERFORM 請求先情報取得.
009490     PERFORM 印刷上部セット.
009500*================================================================*
009510 印刷セット３ SECTION.
009520*
009530     INITIALIZE YJB6622P.
009540     INITIALIZE 受診者情報Ｗ.
009550     INITIALIZE 事業所情報Ｗ.
009560     INITIALIZE 請求先情報Ｗ.
009570     INITIALIZE 負傷情報Ｗ.
009480     PERFORM 受診者情報取得.
009580     PERFORM 負傷データ取得.
009590*
009600     IF 負傷原因印刷区分Ｗ  NOT = 1 
009610        PERFORM 負傷原因取得
009620     END-IF.
009630*
009640     PERFORM 印刷下部セット.
009790*================================================================*
009800 印刷セット５ SECTION.
009810*
009820*/詳細モード用
009830     INITIALIZE YJB6622P.
009840     INITIALIZE 受診者情報Ｗ.
009850     INITIALIZE 事業所情報Ｗ.
009860     INITIALIZE 請求先情報Ｗ.
009870     INITIALIZE 負傷情報Ｗ.
009880     INITIALIZE 負傷原因情報Ｗ.
009900     PERFORM 負担率取得.
009920     PERFORM 受診者情報取得.
009910     PERFORM 請求先情報取得.
009930     PERFORM 負傷データ取得.
009940*
009950     IF 負傷原因印刷区分Ｗ  NOT = 1
009960*        PERFORM 詳細モード負傷原因処理
009970        PERFORM 負傷原因取得
009980     END-IF.
009990*
010000     IF 連印－保険証印刷区分 = 1
010010         PERFORM 印刷上部セット
010020     END-IF.
010030*
010040     PERFORM 詳細モード負傷処理.
      */負傷原因を部位毎の欄に印刷するように変更0302
           PERFORM 詳細モード負傷原因処理.
010050     PERFORM 印刷下部セット.
010060*
010070*================================================================*
010080 印刷上部セット SECTION.
010090*
010050*/バーコードは使用時のみ印刷する/050311
010060*     MOVE 患者コードＷ      TO 患者コード.
010070     IF バーコード使用区分Ｗ NOT = ZERO
010080        MOVE 患者コードＷ     TO 患者コード
010090        MOVE SPACE            TO EDIT-MODE OF 患者コード
010100     ELSE
010110         MOVE "X" TO EDIT-MODE OF 患者コード
010120     END-IF.
010130*
010100     MOVE 患者番号Ｗ                 TO 患者番号.
010110     MOVE 枝番Ｗ                     TO 枝番.
010120**************************
010130* 被保険者証情報セット   *
010140**************************
010150     IF 記号Ｗ(1:2) = "＊" 
010160        MOVE  SPACE    TO  記号
010170     ELSE
010180        MOVE 記号Ｗ    TO  記号
010190     END-IF.
010200     IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
010210        ( 印刷番号Ｗ(1:2) = "＊" )
010220        MOVE  SPACE      TO  番号
010230     ELSE
010240        MOVE 印刷番号Ｗ  TO  番号
010250     END-IF.
010260************************
010270* 被保険者情報セット   *
010280************************
010290     MOVE 被保険者氏名Ｗ             TO 被保険者氏名.
010300*     MOVE 被保険者カナＷ             TO 被保険者カナ.
010310*     MOVE 資格取得元号Ｗ             TO 資格取得元号.
010320     MOVE 資格取得年Ｗ               TO 資格取得年.
010330     MOVE 資格取得月Ｗ               TO 資格取得月.
010340     MOVE 資格取得日Ｗ               TO 資格取得日.
010350     MOVE 資格昭和チェックＷ         TO 資格昭和チェック.
010360     MOVE 資格平成チェックＷ         TO 資格平成チェック.
010360     MOVE 資格令和チェックＷ         TO 資格令和チェック.
010370*/有効期限
010380     MOVE 有効期限年Ｗ               TO 有効年.
010390     MOVE 有効期限月Ｗ               TO 有効月.
010400     MOVE 有効期限日Ｗ               TO 有効日.
010410*     IF ( 有効期限年Ｗ = ZERO ) AND
010420*        ( 有効期限月Ｗ = ZERO ) AND
010430*        ( 有効期限日Ｗ = ZERO )
010440*         MOVE SPACE          TO 有効元号
010450*         MOVE SPACE          TO 有効年月日表示
010460*         MOVE SPACE          TO 有効年表示
010470*         MOVE SPACE          TO 有効月表示
010480*         MOVE SPACE          TO 有効日表示
010490*     ELSE
010500*         MOVE 有効期限元号Ｗ TO 有効元号
010510*         MOVE NC"有効年月日" TO 有効年月日表示
010520*         MOVE NC"年"         TO 有効年表示
010530*         MOVE NC"月"         TO 有効月表示
010540*         MOVE NC"日"         TO 有効日表示
010550*     END-IF.
010560     MOVE 被保険者明治チェックＷ     TO 被保険者明治チェック.
010570     MOVE 被保険者大正チェックＷ     TO 被保険者大正チェック.
010580     MOVE 被保険者昭和チェックＷ     TO 被保険者昭和チェック.
010590     MOVE 被保険者平成チェックＷ     TO 被保険者平成チェック.
010590     MOVE 被保険者令和チェックＷ     TO 被保険者令和チェック.
010600     MOVE 被保険者年Ｗ               TO 被保険者年.
010610     MOVE 被保険者月Ｗ               TO 被保険者月.
010620     MOVE 被保険者日Ｗ               TO 被保険者日.
010630*     MOVE 被保険者年齢Ｗ             TO 被保険者年齢.
010640********************
010650* 患者情報セット   *
010660********************
010670     MOVE ０割チェックＷ             TO ０割チェック.
010680     MOVE １割チェックＷ             TO １割チェック.
010690     MOVE ２割チェックＷ             TO ２割チェック.
010700     MOVE ３割チェックＷ             TO ３割チェック.
010701*
010702* 14/10～ (助成の負担率）
010703     MOVE 助成負担率表示Ｗ           TO 助成負担率.
010704*
010710     IF  印刷市町村番号Ｗ(1:2) = "99"
010720         MOVE SPACE                  TO 市町村番号
010730     ELSE
010740         MOVE 印刷市町村番号Ｗ       TO 市町村番号
010750     END-IF.
010760     IF (印刷受益者番号Ｗ(1:1) = "*") OR
010770        (印刷受益者番号Ｗ(1:2) = "＊") 
010780         MOVE SPACE                  TO 受益者番号
010790     ELSE
010800         MOVE 印刷受益者番号Ｗ       TO 受益者番号
010810     END-IF
010820*
010830     IF  印刷費用負担者番号Ｗ(1:2) = "99"
010840         MOVE SPACE                  TO 公費負担者番号
010850     ELSE
010860         MOVE 印刷費用負担者番号Ｗ   TO 公費負担者番号
010870     END-IF.
010880*
010890     IF (印刷受益者番号助成Ｗ(1:1) = "*") OR
010900        (印刷受益者番号助成Ｗ(1:2) = "＊")
010910         MOVE SPACE                  TO 受益者番号助成
010920     ELSE
               IF 印刷受益者番号助成２Ｗ = SPACE
                   MOVE 印刷受益者番号助成Ｗ   TO 受益者番号助成
               ELSE
                   MOVE 受益者番号助成Ｗ       TO 受益者番号助成１
               END-IF
010940     END-IF.
010950     MOVE 患者カナＷ                 TO 患者カナ.
010960     MOVE 患者氏名Ｗ                 TO 患者氏名.
010970     MOVE 被保険者男チェックＷ       TO 被保険者男チェック.
010980     MOVE 被保険者女チェックＷ       TO 被保険者女チェック.
010990     MOVE 被保険者郵便番号１Ｗ       TO 被保険者郵便番号１.
011000     MOVE 被保険者郵便番号２Ｗ       TO 被保険者郵便番号２.
011010     MOVE "-"                        TO 被保険者郵便区切.
011020     MOVE 被保険者住所１Ｗ           TO 被保険者住所１.
011030     MOVE 被保険者住所２Ｗ           TO 被保険者住所２.
011040*     MOVE 被保険者電話番号Ｗ         TO 被保険者電話番号.
011050     MOVE 分割電話番号１Ｗ           TO 電話番号１.
011060     MOVE 分割電話番号２Ｗ           TO 電話番号２.
011070     MOVE 分割電話番号３Ｗ           TO 電話番号３.
011080     MOVE 患者男チェックＷ           TO 患者男チェック.
011090     MOVE 患者女チェックＷ           TO 患者女チェック.
011100     MOVE 明治チェックＷ             TO 明治チェック.
011110     MOVE 大正チェックＷ             TO 大正チェック.
011120     MOVE 昭和チェックＷ             TO 昭和チェック.
011130     MOVE 平成チェックＷ             TO 平成チェック.
011130     MOVE 令和チェックＷ             TO 令和チェック.
011140     MOVE 患者年Ｗ                   TO 患者年.
011150     MOVE 患者月Ｗ                   TO 患者月.
011160     MOVE 患者日Ｗ                   TO 患者日.
011170     MOVE 患者年齢Ｗ                 TO 患者年齢.
011180     MOVE 印刷続柄Ｗ                 TO 続柄.
011190*     MOVE 患者郵便番号１Ｗ           TO 患者郵便番号１.
011200*     MOVE 患者郵便番号２Ｗ           TO 患者郵便番号２.
011210*     MOVE "-"                        TO 患者郵便区切.
011220*     MOVE 患者住所Ｗ                 TO 患者住所.
011230*     MOVE 患者電話番号Ｗ             TO 患者電話番号.
011240*     MOVE 患者住所１Ｗ               TO 患者住所１.
011250*     MOVE 患者住所２Ｗ               TO 患者住所２.
      *     IF 保険種別ＷＲ = 85
011190*        MOVE 患者郵便番号１Ｗ           TO 被保険者郵便番号１
011200*        MOVE 患者郵便番号２Ｗ           TO 被保険者郵便番号２
011210*        MOVE "-"                        TO 被保険者郵便区切
011240*        MOVE 患者住所１Ｗ               TO 被保険者住所１
011250*        MOVE 患者住所２Ｗ               TO 被保険者住所２
      *     END-IF.
011260********************
011270* 事業所情報セット *
011280********************
011290     MOVE 事業所名称Ｗ               TO 事業所名称.
011300     MOVE 事業所郵便番号１Ｗ         TO 事業所郵便番号１.
011310     MOVE 事業所郵便番号２Ｗ         TO 事業所郵便番号２.
011320     MOVE 印刷事業所住所１Ｗ         TO 事業所住所.
011320     MOVE 印刷事業所住所２Ｗ         TO 事業所住所２.
011330************************************
011340* 都道府県・健康保険組合情報セット *
011350************************************
011360*労災、自費の時は保険者情報は印字しない
011370     IF 保険種別ＷＲ = 70 OR 90
011380         MOVE SPACE                  TO 保険者番号
011390         MOVE SPACE                  TO 請求先名支部名１
011400         MOVE SPACE                  TO 請求先名支部名２
011410         MOVE SPACE                  TO 保険者郵便番号
011420         MOVE SPACE                  TO 保険者住所１
011430         MOVE SPACE                  TO 保険者住所２
011440     ELSE
011450         MOVE 印刷保険者番号Ｗ       TO 保険者番号
011460         MOVE 印刷請求先名支部名１Ｗ TO 請求先名支部名１
011470         MOVE 印刷請求先名支部名２Ｗ TO 請求先名支部名２
011480         MOVE 請求先郵便番号Ｗ       TO 保険者郵便番号
011490*         MOVE 請求先住所１Ｗ         TO 保険者住所１
011500*         MOVE 請求先住所２Ｗ         TO 保険者住所２
011510         MOVE 保険者住所１Ｗ         TO 保険者住所１
011520         MOVE 保険者住所２Ｗ         TO 保険者住所２
011530     END-IF.
011540**************************
011550* 保険種別チェックセット *
011560**************************
011570     MOVE 社保チェックＷ             TO 社保チェック.
011580     MOVE 組合チェックＷ             TO 組合チェック.
011590     MOVE 日雇チェックＷ             TO 日雇チェック.
011600     MOVE 船員チェックＷ             TO 船員チェック.
011610     MOVE 共済チェックＷ             TO 共済チェック.
011620     MOVE 国保チェックＷ             TO 国保チェック.
011630     MOVE 退職チェックＷ             TO 退職チェック.
011640     MOVE 老人チェックＷ             TO 老人チェック.
011650*     MOVE 乳幼児チェックＷ           TO 乳幼児チェック.
011660*     MOVE 自賠チェックＷ             TO 自賠チェック.
011670*     MOVE 生保チェックＷ             TO 生保チェック.
011680*     MOVE 福チェックＷ               TO 福チェック.
011690*     MOVE 身障チェックＷ             TO 身障チェック.
011700*     MOVE 母子チェックＷ             TO 母子チェック.
011710*     MOVE 生保チェックＷ             TO 生保チェック.
011720*     MOVE 原爆チェックＷ             TO 原爆チェック.
011730     MOVE 自衛チェックＷ             TO 自衛チェック.
011740     MOVE 公費チェックＷ             TO 公費チェック.
011750     MOVE 自費チェックＷ             TO 自費チェック.
011760*================================================================*
011770 印刷下部セット SECTION.
011780*
011790********************
011800* 負傷データセット *
011810********************
011820* １部位 *
011830**********
011840     MOVE 会部位コードＷ(1)   TO 負傷コード１.
011850     MOVE 負傷名Ｗ(1)         TO 負傷名１.
011860     MOVE 負傷年Ｗ(1)         TO 負傷年１.
011870     MOVE 負傷月Ｗ(1)         TO 負傷月１.
011880     MOVE 負傷日Ｗ(1)         TO 負傷日１.
011890     MOVE 施術開始年Ｗ(1)     TO 施術開始年１.
011900     MOVE 施術開始月Ｗ(1)     TO 施術開始月１.
011910     MOVE 施術開始日Ｗ(1)     TO 施術開始日１.
011920     MOVE 施術終了年Ｗ(1)     TO 施術終了年１.
011930     MOVE 施術終了月Ｗ(1)     TO 施術終了月１.
011940     MOVE 施術終了日Ｗ(1)     TO 施術終了日１.
011950*     MOVE 負傷年月日区切Ｗ(1) TO 区切１１ 区切１２.
011960*     MOVE 施開年月日区切Ｗ(1) TO 区切１３ 区切１４.
011970*     MOVE 施終年月日区切Ｗ(1) TO 区切１５ 区切１６.
011980*     MOVE 転帰Ｗ(1)           TO 転帰１.
011990     MOVE 治癒チェックＷ(1)   TO 治癒チェック１.
012000     MOVE 中止チェックＷ(1)   TO 中止チェック１.
012010     MOVE 転医チェックＷ(1)   TO 転医チェック１.
012020*/負傷時間を追加
012030*/数字項目では未入力時にゼロが表示されるため文字項目に変換
012040     IF (負傷時Ｗ(1) = ZERO) AND (負傷分Ｗ(1) = ZERO)
012050         MOVE SPACE TO 負傷時 負傷分
012060     ELSE
012070         MOVE 負傷時間Ｗ(1) TO 負傷時間ＸＷ
012080*       */時の１桁目が０の場合は空白にする
012090         IF 負傷時ＸＷ(1:1) = "0"
012100             MOVE SPACE TO 負傷時ＸＷ(1:1)
012110         END-IF
012120         MOVE 負傷時ＸＷ    TO 負傷時
012130         MOVE 負傷分ＸＷ    TO 負傷分
012140     END-IF.
012150*     ******************
012160*     * 日数は一時保留 *
012170*     ******************
012180*     **********************
012190*     * 施術回数は一時保留 *
012200*     **********************
012210*     **********************
012220*     * 転帰名称は一時保留 *
012230*     **********************
012240**********
012250* ２部位 *
012260**********
012270     MOVE 会部位コードＷ(2)   TO 負傷コード２.
012280     MOVE 負傷名Ｗ(2)         TO 負傷名２.
012290     MOVE 負傷年Ｗ(2)         TO 負傷年２.
012300     MOVE 負傷月Ｗ(2)         TO 負傷月２.
012310     MOVE 負傷日Ｗ(2)         TO 負傷日２.
012320     MOVE 施術開始年Ｗ(2)     TO 施術開始年２.
012330     MOVE 施術開始月Ｗ(2)     TO 施術開始月２.
012340     MOVE 施術開始日Ｗ(2)     TO 施術開始日２.
012350     MOVE 施術終了年Ｗ(2)     TO 施術終了年２.
012360     MOVE 施術終了月Ｗ(2)     TO 施術終了月２.
012370     MOVE 施術終了日Ｗ(2)     TO 施術終了日２.
012380*     MOVE 負傷年月日区切Ｗ(2) TO 区切２１ 区切２２.
012390*     MOVE 施開年月日区切Ｗ(2) TO 区切２３ 区切２４.
012400*     MOVE 施終年月日区切Ｗ(2) TO 区切２５ 区切２６.
012410*     MOVE 転帰Ｗ(2)           TO 転帰２.
012420     MOVE 治癒チェックＷ(2)   TO 治癒チェック２.
012430     MOVE 中止チェックＷ(2)   TO 中止チェック２.
012440     MOVE 転医チェックＷ(2)   TO 転医チェック２.
012450*     ******************
012460*     * 日数は一時保留 *
012470*     ******************
012480*     **********************
012490*     * 施術回数は一時保留 *
012500*     **********************
012510*     **********************
012520*     * 転帰名称は一時保留 *
012530*     **********************
012540**********
012550* ３部位 *
012560**********
012570     MOVE 会部位コードＷ(3)   TO 負傷コード３.
012580     MOVE 負傷名Ｗ(3)         TO 負傷名３.
012590     MOVE 負傷年Ｗ(3)         TO 負傷年３.
012600     MOVE 負傷月Ｗ(3)         TO 負傷月３.
012610     MOVE 負傷日Ｗ(3)         TO 負傷日３.
012620     MOVE 施術開始年Ｗ(3)     TO 施術開始年３.
012630     MOVE 施術開始月Ｗ(3)     TO 施術開始月３.
012640     MOVE 施術開始日Ｗ(3)     TO 施術開始日３.
012650     MOVE 施術終了年Ｗ(3)     TO 施術終了年３.
012660     MOVE 施術終了月Ｗ(3)     TO 施術終了月３.
012670     MOVE 施術終了日Ｗ(3)     TO 施術終了日３.
012680*     MOVE 負傷年月日区切Ｗ(3) TO 区切３１ 区切３２.
012690*     MOVE 施開年月日区切Ｗ(3) TO 区切３３ 区切３４.
012700*     MOVE 施終年月日区切Ｗ(3) TO 区切３５ 区切３６.
012710*     MOVE 転帰Ｗ(3)           TO 転帰３.
012720     MOVE 治癒チェックＷ(3)   TO 治癒チェック３.
012730     MOVE 中止チェックＷ(3)   TO 中止チェック３.
012740     MOVE 転医チェックＷ(3)   TO 転医チェック３.
012750*     ******************
012760*     * 日数は一時保留 *
012770*     ******************
012780*     **********************
012790*     * 施術回数は一時保留 *
012800*     **********************
012810*     **********************
012820*     * 転帰名称は一時保留 *
012830*     **********************
012840**********
012850* ４部位 *
012860**********
012870     MOVE 会部位コードＷ(4)   TO 負傷コード４.
012880     MOVE 負傷名Ｗ(4)         TO 負傷名４.
012890     MOVE 負傷年Ｗ(4)         TO 負傷年４.
012900     MOVE 負傷月Ｗ(4)         TO 負傷月４.
012910     MOVE 負傷日Ｗ(4)         TO 負傷日４.
012920     MOVE 施術開始年Ｗ(4)     TO 施術開始年４.
012930     MOVE 施術開始月Ｗ(4)     TO 施術開始月４.
012940     MOVE 施術開始日Ｗ(4)     TO 施術開始日４.
012950     MOVE 施術終了年Ｗ(4)     TO 施術終了年４.
012960     MOVE 施術終了月Ｗ(4)     TO 施術終了月４.
012970     MOVE 施術終了日Ｗ(4)     TO 施術終了日４.
012980*     MOVE 負傷年月日区切Ｗ(4) TO 区切４１ 区切４２.
012990*     MOVE 施開年月日区切Ｗ(4) TO 区切４３ 区切４４.
013000*     MOVE 施終年月日区切Ｗ(4) TO 区切４５ 区切４６.
013010*     MOVE 転帰Ｗ(4)           TO 転帰４.
013020     MOVE 治癒チェックＷ(4)   TO 治癒チェック４.
013030     MOVE 中止チェックＷ(4)   TO 中止チェック４.
013040     MOVE 転医チェックＷ(4)   TO 転医チェック４.
013050*     ******************
013060*     * 日数は一時保留 *
013070*     ******************
013080*     **********************
013090*     * 施術回数は一時保留 *
013100*     **********************
013110*     **********************
013120*     * 転帰名称は一時保留 *
013130*     **********************
013140**********
013150* ５部位 *
013160**********
013170     MOVE 会部位コードＷ(5)   TO 負傷コード５.
013180     MOVE 負傷名Ｗ(5)         TO 負傷名５.
013190     MOVE 負傷年Ｗ(5)         TO 負傷年５.
013200     MOVE 負傷月Ｗ(5)         TO 負傷月５.
013210     MOVE 負傷日Ｗ(5)         TO 負傷日５.
013220     MOVE 施術開始年Ｗ(5)     TO 施術開始年５.
013230     MOVE 施術開始月Ｗ(5)     TO 施術開始月５.
013240     MOVE 施術開始日Ｗ(5)     TO 施術開始日５.
013250     MOVE 施術終了年Ｗ(5)     TO 施術終了年５.
013260     MOVE 施術終了月Ｗ(5)     TO 施術終了月５.
013270     MOVE 施術終了日Ｗ(5)     TO 施術終了日５.
013280*     MOVE 負傷年月日区切Ｗ(5) TO 区切５１ 区切５２.
013290*     MOVE 施開年月日区切Ｗ(5) TO 区切５３ 区切５４.
013300*     MOVE 施終年月日区切Ｗ(5) TO 区切５５ 区切５６.
013310*     MOVE 転帰Ｗ(5)           TO 転帰５.
013320     MOVE 治癒チェックＷ(5)   TO 治癒チェック５.
013330     MOVE 中止チェックＷ(5)   TO 中止チェック５.
013340     MOVE 転医チェックＷ(5)   TO 転医チェック５.
013350*     ******************
013360*     * 日数は一時保留 *
013370*     ******************
013380*     **********************
013390*     * 施術回数は一時保留 *
013400*     **********************
013410*     **********************
013420*     * 転帰名称は一時保留 *
013430*     **********************
013440************
013450* 負傷原因 *
013460************
      */負傷原因を部位毎の欄に印刷するように変更0302以下コメントに
013470*     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
013480*     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
013490*     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
013500*     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
013510*     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
013520*     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
013530*     MOVE 負傷原因Ｗ(7)       TO 負傷原因７.
013540*     MOVE 負傷原因Ｗ(8)       TO 負傷原因８.
013550*     MOVE 負傷原因Ｗ(9)       TO 負傷原因９.
013560*     MOVE 負傷原因Ｗ(10)      TO 負傷原因１０.
013470*     MOVE 負傷原因２Ｗ(1)     TO 負傷原因２１.
013480*     MOVE 負傷原因２Ｗ(2)     TO 負傷原因２２.
013490*     MOVE 負傷原因２Ｗ(3)     TO 負傷原因２３.
013500*     MOVE 負傷原因２Ｗ(4)     TO 負傷原因２４.
013510*     MOVE 負傷原因２Ｗ(5)     TO 負傷原因２５.
013520*     MOVE 負傷原因２Ｗ(6)     TO 負傷原因２６.
013530*     MOVE 負傷原因２Ｗ(7)     TO 負傷原因２７.
013540*     MOVE 負傷原因２Ｗ(8)     TO 負傷原因２８.
013470*     MOVE 負傷原因３Ｗ(1)     TO 負傷原因３１.
013480*     MOVE 負傷原因３Ｗ(2)     TO 負傷原因３２.
013490*     MOVE 負傷原因３Ｗ(3)     TO 負傷原因３３.
013500*     MOVE 負傷原因３Ｗ(4)     TO 負傷原因３４.
013510*     MOVE 負傷原因３Ｗ(5)     TO 負傷原因３５.
013520*     MOVE 負傷原因３Ｗ(6)     TO 負傷原因３６.
013530*     MOVE 負傷原因３Ｗ(7)     TO 負傷原因３７.
013540*     MOVE 負傷原因３Ｗ(8)     TO 負傷原因３８.
013470*     MOVE 負傷原因４Ｗ(1)     TO 負傷原因４１.
013480*     MOVE 負傷原因４Ｗ(2)     TO 負傷原因４２.
013490*     MOVE 負傷原因４Ｗ(3)     TO 負傷原因４３.
013500*     MOVE 負傷原因４Ｗ(4)     TO 負傷原因４４.
013510*     MOVE 負傷原因４Ｗ(5)     TO 負傷原因４５.
013520*     MOVE 負傷原因４Ｗ(6)     TO 負傷原因４６.
013530*     MOVE 負傷原因４Ｗ(7)     TO 負傷原因４７.
013540*     MOVE 負傷原因４Ｗ(8)     TO 負傷原因４８.
013570**
      *
      */負傷原因を部位毎の欄に印刷するように変更0302
      */負傷原因の転記
           MOVE 負傷原因Ｗ(1)( 1:44) TO 負傷原因１.
           MOVE 負傷原因Ｗ(1)(45:44) TO 負傷原因２.
           MOVE 負傷原因Ｗ(1)(89:44)  TO 負傷原因３.
           MOVE 負傷原因Ｗ(1)(133:44) TO 負傷原因４.
           MOVE 負傷原因Ｗ(1)(177:44) TO 負傷原因５.
           MOVE 負傷原因Ｗ(1)(221:44) TO 負傷原因６.
           MOVE 負傷原因Ｗ(1)(265:38) TO 負傷原因７.
      *
           MOVE 負傷原因Ｗ(2)( 1:44) TO 負傷原因２１.
           MOVE 負傷原因Ｗ(2)(45:44) TO 負傷原因２２.
           MOVE 負傷原因Ｗ(2)(89:44) TO 負傷原因２３.
           MOVE 負傷原因Ｗ(2)(133:44) TO 負傷原因２４.
           MOVE 負傷原因Ｗ(2)(177:44) TO 負傷原因２５.
           MOVE 負傷原因Ｗ(2)(221:44) TO 負傷原因２６.
           MOVE 負傷原因Ｗ(2)(265:38) TO 負傷原因２７.
      *
           MOVE 負傷原因Ｗ(3)( 1:30) TO 負傷原因３１.
           MOVE 負傷原因Ｗ(3)(31:30) TO 負傷原因３２.
           MOVE 負傷原因Ｗ(3)(61:30) TO 負傷原因３３.
           MOVE 負傷原因Ｗ(3)(91:30) TO 負傷原因３４.
           MOVE 負傷原因Ｗ(3)(121:30) TO 負傷原因３５.
           MOVE 負傷原因Ｗ(3)(151:30) TO 負傷原因３６.
           MOVE 負傷原因Ｗ(3)(181:30) TO 負傷原因３７.
           MOVE 負傷原因Ｗ(3)(211:30) TO 負傷原因３８.
           MOVE 負傷原因Ｗ(3)(241:30) TO 負傷原因３９.
           MOVE 負傷原因Ｗ(3)(271:30) TO 負傷原因３０.
      *
           MOVE 負傷原因Ｗ(4)( 1:26) TO 負傷原因４１.
           MOVE 負傷原因Ｗ(4)(27:26) TO 負傷原因４２.
           MOVE 負傷原因Ｗ(4)(53:26) TO 負傷原因４３.
           MOVE 負傷原因Ｗ(4)(79:26) TO 負傷原因４４.
           MOVE 負傷原因Ｗ(4)(105:26) TO 負傷原因４５.
           MOVE 負傷原因Ｗ(4)(131:26) TO 負傷原因４６.
           MOVE 負傷原因Ｗ(4)(157:26) TO 負傷原因４７.
           MOVE 負傷原因Ｗ(4)(183:26) TO 負傷原因４８.
           MOVE 負傷原因Ｗ(4)(209:26) TO 負傷原因４９.
           MOVE 負傷原因Ｗ(4)(235:26) TO 負傷原因５０.
      *
           MOVE 負傷原因Ｗ(5)( 1:26) TO 負傷原因５１.
           MOVE 負傷原因Ｗ(5)(27:26) TO 負傷原因５２.
           MOVE 負傷原因Ｗ(5)(53:26) TO 負傷原因５３.
           MOVE 負傷原因Ｗ(5)(79:26) TO 負傷原因５４.
           MOVE 負傷原因Ｗ(5)(105:26) TO 負傷原因５５.
           MOVE 負傷原因Ｗ(5)(131:26) TO 負傷原因５６.
           MOVE 負傷原因Ｗ(5)(157:26) TO 負傷原因５７.
           MOVE 負傷原因Ｗ(5)(183:26) TO 負傷原因５８.
           MOVE 負傷原因Ｗ(5)(209:26) TO 負傷原因５９.
           MOVE 負傷原因Ｗ(5)(235:26) TO 負傷原因６０.
      *
013580*================================================================*
013590 負担率取得 SECTION.
013600*
013610     MOVE ZERO TO 本人負担率Ｗ.
013620     MOVE ZERO TO 家族負担率Ｗ.
013630*
013640     MOVE 施術和暦ＷＲ   TO 受－施術和暦.
013650     MOVE 施術年ＷＲ     TO 受－施術年.
013660     MOVE 施術月ＷＲ     TO 受－施術月.
013670     MOVE 患者番号ＷＲ   TO 受－患者番号.
013680     MOVE 枝番ＷＲ       TO 受－枝番.
013690     READ 受診者情報Ｆ
013700     INVALID KEY
013710         MOVE  NC"　　施術月の受診者情報がありません" TO 連メ－メッセージ
013720         CALL   "MSG001"
013730         CANCEL "MSG001"
013740         PERFORM ファイル閉鎖
013750         MOVE ZERO TO PROGRAM-STATUS
013760         EXIT PROGRAM
013770     NOT INVALID KEY
013771* 14/10～　サブルーチン処理
013772         IF 受－施術和暦年月 >= 41410
013773            PERFORM 負担率取得１４１０
               END-IF
014940     END-READ.
014950*
014960     EVALUATE 負担率Ｗ
014970     WHEN 0
014980         MOVE NC"○" TO ０割チェックＷ
014990     WHEN 10
015000         MOVE NC"○" TO １割チェックＷ
015010     WHEN 20
015020         MOVE NC"○" TO ２割チェックＷ
015030     WHEN 30
015040         MOVE NC"○" TO ３割チェックＷ
015050     END-EVALUATE.
015051*
015052*================================================================*
015053 負担率取得１４１０ SECTION.
015054*
015055* 受診者Ｆ READ中
015056* 平成14/10～
015057     MOVE ZERO  TO 負担率Ｗ.
015058     INITIALIZE 助成負担率ＷＷ.
015059*
015060     MOVE SPACE TO 連率－負担率取得キー.
015061     INITIALIZE 連率－負担率取得キー.
015062     MOVE 受－施術和暦年月 TO 連率－施術和暦年月.
015063     MOVE 受－患者コード   TO 連率－患者コード.
015064*
015065     CALL   "HUTANRIT".
015066     CANCEL "HUTANRIT".
015067*
015068* / 本体 /
015069     MOVE 連率－実際本体負担率 TO 負担率Ｗ.
015070*
015071*
015072* / 助成 /
015073     IF 受－助成種別 NOT = ZERO
015074        MOVE 連率－助成負担率  TO 助成負担率Ｗ
015075*
015076        COMPUTE 助成負担率Ｗ１ = 助成負担率Ｗ / 10
015077        MOVE 助成負担率Ｗ１ TO  助成負担率文字Ｗ１
015078*
015079        STRING "("                 DELIMITED BY SIZE
015080               助成負担率文字Ｗ１  DELIMITED BY SIZE
015081               "割)"               DELIMITED BY SIZE
015082               INTO 助成負担率表示Ｗ
015083        END-STRING
015084*
015085     END-IF.
015086*
015240*================================================================*
015250 受診者情報取得 SECTION.
015260*
015270**************************************************
015280* 連結データから受診者情報Ｆより以下の情報を取得 *
015290* ● 患者番号.... 患者番号Ｗ                     *
015300* ● 枝番........ 枝番Ｗ                         *
015310* ● 記号 ....... 記号Ｗに格納                   *
015320* ● 番号 ....... 番号Ｗに格納                   *
015330* ● 被保険者カナ.被保険者カナＷに格納           *
015340* ● 被保険者氏名.被保険者氏名Ｗに格納           *
015350* ● 郵便番号１ ..郵便番号１Ｗに格納             *
015360* ● 郵便番号２ ..郵便番号２Ｗに格納             *
015370* ● 住所１ ......住所１Ｗに格納                 *
015380* ● 住所２ ......住所２Ｗに格納                 *
015390* ● 電話番号.....電話番号Ｗに格納               *
015400* ● 患者カナ ....患者カナＷに格納               *
015410* ● 患者氏名 ....患者氏名Ｗに格納               *
015420* ● 続柄 ........名称マスタより続柄Ｗに取得     *
015430* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
015440* ● 患者年 ......患者年Ｗに格納                 *
015450* ● 患者月 ......患者月Ｗに格納                 *
015460* ● 患者日 ......患者日Ｗに格納                 *
015470**************************************************
015480     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
015490     MOVE 施術年ＷＲ         TO 受－施術年.
015500     MOVE 施術月ＷＲ         TO 受－施術月.
015510     MOVE 患者コードＷＲ     TO 受－患者コード.
015520     READ 受診者情報Ｆ
015530     INVALID KEY
015540         CONTINUE
015550*            /* ありえない */
015560     NOT INVALID KEY
015570         MOVE 受－患者番号     TO 患者番号Ｗ
015580         MOVE 受－枝番         TO 枝番Ｗ
015590*         MOVE 受－記号         TO 記号Ｗ
015600*         MOVE 受－番号         TO 番号Ｗ
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
015680         MOVE 受－被保険者カナ TO 被保険者カナＷ
015690         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
015700*
015710         EVALUATE 受－被保険者性別
015720         WHEN 1
015730             MOVE NC"○"  TO 被保険者男チェックＷ
015740         WHEN 2
015750             MOVE NC"○"  TO 被保険者女チェックＷ
015760         END-EVALUATE
015770         EVALUATE 受－患者性別
015780         WHEN 1
015790             MOVE NC"○"  TO 患者男チェックＷ
015800         WHEN 2
015810             MOVE NC"○"  TO 患者女チェックＷ
015820         END-EVALUATE
015830*
015840         EVALUATE 受－被保険者和暦
015850         WHEN 1
015860             MOVE NC"○"  TO 被保険者明治チェックＷ
015870         WHEN 2
015880             MOVE NC"○"  TO 被保険者大正チェックＷ
015890         WHEN 3
015900             MOVE NC"○"  TO 被保険者昭和チェックＷ
015910         WHEN 4
015920             MOVE NC"○"  TO 被保険者平成チェックＷ
015910         WHEN 5
015920             MOVE NC"○"  TO 被保険者令和チェックＷ
015930         END-EVALUATE
015940         MOVE 受－被保険者年   TO 被保険者年Ｗ
015950         MOVE 受－被保険者月   TO 被保険者月Ｗ
015960         MOVE 受－被保険者日   TO 被保険者日Ｗ
015970
015980*被保険者年齢
015990         MOVE 連印－施術和暦 TO 元－元号区分
016000         READ 元号マスタ
016010         NOT INVALID KEY
016020             COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
016030         END-READ
016040         IF ( 受－被保険者和暦 NOT = ZERO ) AND
016050            ( 受－被保険者年   NOT = ZERO ) AND
016060            ( 受－被保険者月   NOT = ZERO ) AND
016070            ( 受－被保険者日   NOT = ZERO )
016080
016090             MOVE ZERO TO 被保険者年齢Ｗ
016100*
016110             MOVE 受－被保険者和暦 TO 元－元号区分
016120             READ 元号マスタ
016130             NOT INVALID KEY
016140                 COMPUTE 被保険者西暦年Ｗ = 元－開始西暦年 + ( 受－被保険者年 - 1 )
016150             END-READ
016160*
016170             COMPUTE 被保険者年齢Ｗ   = 施術西暦年Ｗ - 被保険者西暦年Ｗ
016180*
016190             IF 連印－施術月 < 受－被保険者月
016200                 COMPUTE 被保険者年齢Ｗ = 被保険者年齢Ｗ - 1
016210             END-IF
016220*
016230*             MOVE 被保険者年齢Ｗ     TO 被保険者年齢
016240         END-IF
016250*
016260*/郵便番号 ゼロの時はスペース
016270         IF 受－郵便番号１ = "000"
016280             MOVE SPACE          TO 被保険者郵便番号１Ｗ
016290             MOVE SPACE          TO 被保険者郵便番号２Ｗ
016300         ELSE
016310             MOVE 受－郵便番号１ TO 被保険者郵便番号１Ｗ
016320             MOVE 受－郵便番号２ TO 被保険者郵便番号２Ｗ
016330         END-IF
016340*         MOVE 受－電話番号     TO 被保険者電話番号Ｗ
016350*/電話番号を分割
016360         UNSTRING 受－電話番号 DELIMITED BY "-"
016370             INTO 分割電話番号１Ｗ
016380                  分割電話番号２Ｗ
016390                  分割電話番号３Ｗ
016400         END-UNSTRING
016410*電話番号右詰処理
016420         IF 分割電話番号３Ｗ = SPACE
016430             MOVE 分割電話番号２Ｗ TO 分割電話番号３Ｗ
016440             MOVE 分割電話番号１Ｗ TO 分割電話番号２Ｗ
016450             MOVE SPACE            TO 分割電話番号１Ｗ
016460         END-IF
016470         IF 分割電話番号３Ｗ = SPACE
016480             MOVE SPACE            TO 分割電話番号Ｗ
016490             MOVE 受－電話番号     TO 分割電話番号Ｗ
016500         END-IF
016510*
016520*         STRING 受－住所１  DELIMITED BY SPACE
016530*                受－住所２  DELIMITED BY SPACE
016540*           INTO 被保険者住所Ｗ
016550*         END-STRING
016560         MOVE 受－住所１ TO 被保険者住所１Ｗ
016570         MOVE 受－住所２ TO 被保険者住所２Ｗ
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
                   WHEN 5
                       MOVE NC"○"  TO 被保険者令和チェックＷ
018140             END-EVALUATE
018150             MOVE 受－患者年   TO 被保険者年Ｗ
018160             MOVE 受－患者月   TO 被保険者月Ｗ
018170             MOVE 受－患者日   TO 被保険者日Ｗ
019210             MOVE 受－患者郵便番号１ TO 被保険者郵便番号１Ｗ
019220             MOVE 受－患者郵便番号２ TO 被保険者郵便番号２Ｗ
016560             MOVE 受－患者住所１ TO 被保険者住所１Ｗ
016570             MOVE 受－患者住所２ TO 被保険者住所２Ｗ
016350*/電話番号を分割
016360             UNSTRING 受－患者電話番号 DELIMITED BY "-"
016370                 INTO 分割電話番号１Ｗ
016380                      分割電話番号２Ｗ
016390                      分割電話番号３Ｗ
016400             END-UNSTRING
016410*電話番号右詰処理
016420             IF 分割電話番号３Ｗ = SPACE
016430                 MOVE 分割電話番号２Ｗ TO 分割電話番号３Ｗ
016440                 MOVE 分割電話番号１Ｗ TO 分割電話番号２Ｗ
016450                 MOVE SPACE            TO 分割電話番号１Ｗ
016460             END-IF
016470             IF 分割電話番号３Ｗ = SPACE
016480                 MOVE SPACE            TO 分割電話番号Ｗ
016490                 MOVE 受－電話番号     TO 分割電話番号Ｗ
016500             END-IF
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
016580*
016590*/郵便番号 ゼロの時はスペース
016600         IF 受－患者郵便番号１ = "000"
016610             MOVE SPACE              TO 患者郵便番号１Ｗ
016620             MOVE SPACE              TO 患者郵便番号２Ｗ
016630         ELSE
016640             MOVE 受－患者郵便番号１ TO 患者郵便番号１Ｗ
016650             MOVE 受－患者郵便番号２ TO 患者郵便番号２Ｗ
016660         END-IF
016670*         MOVE 受－患者住所１     TO 患者住所１Ｗ
016680*         MOVE 受－患者住所２     TO 患者住所２Ｗ
016690         STRING 受－患者住所１  DELIMITED BY SPACE
016700                受－患者住所２  DELIMITED BY SPACE
016710           INTO 患者住所Ｗ
016720         END-STRING
016730         MOVE 受－患者電話番号   TO 患者電話番号Ｗ
016740*
016750*電話番号分割
016760*         UNSTRING 電話番号Ｗ  DELIMITED BY "-"
016770*             INTO 分割電話番号１Ｗ
016780*                  分割電話番号２Ｗ
016790*                  分割電話番号３Ｗ
016800*         END-UNSTRING
016810*電話番号右詰処理
016820*         IF 分割電話番号３Ｗ = SPACE
016830*             MOVE 分割電話番号２Ｗ TO 分割電話番号３Ｗ
016840*             MOVE 分割電話番号１Ｗ TO 分割電話番号２Ｗ
016850*             MOVE SPACE            TO 分割電話番号１Ｗ
016860*         END-IF
016870*
016880         MOVE 受－患者カナ     TO 患者カナＷ
016890         MOVE 受－患者氏名     TO 患者氏名Ｗ
016900*
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
               IF 受－保険種別 NOT = 80
                  READ 事業所マスタ
                  INVALID KEY
                     MOVE SPACE TO 事－レコード
                     INITIALIZE    事－レコード
                  END-READ
                  MOVE 事－事業所名称   TO 事業所情報Ｗ
015540*            STRING 事－事業所住所１  DELIMITED BY SPACE
015550*                   事－事業所住所２  DELIMITED BY SPACE
015560*              INTO 事業所住所Ｗ
015570*            END-STRING
                  MOVE 事－事業所住所１   TO 印刷事業所住所１Ｗ
                  MOVE 事－事業所住所２   TO 印刷事業所住所２Ｗ
               END-IF
016960* 続柄
016970         IF 受－本人家族区分 = 1
016980             MOVE NC"本人"     TO 続柄Ｗ
016990         ELSE
017000             MOVE 05          TO 名－区分コード
017010             MOVE 受－続柄    TO 名－名称コード
017020             READ 名称マスタ
017030             INVALID KEY
017040                 MOVE SPACE   TO 続柄Ｗ
017050             NOT INVALID KEY
017060                 MOVE 名－略称  TO 続柄Ｗ
017070             END-READ
017080         END-IF
017090* 患者和暦年月日
017100         EVALUATE 受－患者和暦
017110         WHEN 1
017120             MOVE NC"○"  TO 明治チェックＷ
017130         WHEN 2
017140             MOVE NC"○"  TO 大正チェックＷ
017150         WHEN 3
017160             MOVE NC"○"  TO 昭和チェックＷ
017170         WHEN 4
017180             MOVE NC"○"  TO 平成チェックＷ
017170         WHEN 5
017180             MOVE NC"○"  TO 令和チェックＷ
017190         END-EVALUATE
017200         MOVE 受－患者年  TO 患者年Ｗ
017210         MOVE 受－患者月  TO 患者月Ｗ
017220         MOVE 受－患者日  TO 患者日Ｗ
017230*患者年齢
017240         MOVE 連印－施術和暦 TO 元－元号区分
017250         READ 元号マスタ
017260         NOT INVALID KEY
017270              COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
017280         END-READ
017290         IF ( 受－患者和暦 NOT = ZERO ) AND
017300            ( 受－患者年   NOT = ZERO ) AND
017310            ( 受－患者月   NOT = ZERO ) AND
017320            ( 受－患者日   NOT = ZERO )
017330*
017340             MOVE ZERO TO 患者年齢Ｗ
017350*
017360             MOVE 受－患者和暦 TO 元－元号区分
017370             READ 元号マスタ
017380             NOT INVALID KEY
017390                 COMPUTE 患者西暦年Ｗ = 元－開始西暦年 + ( 受－患者年 - 1 )
017400             END-READ
017410*
017420             COMPUTE 患者年齢Ｗ   = 施術西暦年Ｗ - 患者西暦年Ｗ
017430*
017440             IF 連印－施術月 < 受－患者月
017450                 COMPUTE 患者年齢Ｗ = 患者年齢Ｗ - 1
017460             END-IF
017470*
017480*         MOVE 患者年齢Ｗ     TO 患者年齢
017490         END-IF
017500* 患者性別
017510*         EVALUATE 受－患者性別
017520*         WHEN 1
017530*             MOVE NC"（男）" TO 患者性別Ｗ
017540*         WHEN 2
017550*             MOVE NC"（女）" TO 患者性別Ｗ
017560*         END-EVALUATE
017570*/有効期限
017580         MOVE 受－有効和暦   TO 有効期限和暦Ｗ
017590         MOVE 受－有効年     TO 有効期限年Ｗ
017600         MOVE 受－有効月     TO 有効期限月Ｗ
017610         MOVE 受－有効日     TO 有効期限日Ｗ
017620         MOVE 有効期限和暦Ｗ TO 元－元号区分
017630         READ 元号マスタ
017640         INVALID KEY
017650             MOVE SPACE        TO 有効期限元号Ｗ
017660         NOT INVALID KEY
017670             MOVE 元－元号名称 TO 有効期限元号Ｗ
017680         END-READ
017690*/資格取得年月日
017700         MOVE 受－資格和暦 TO 資格取得和暦Ｗ
017710         MOVE 受－資格年   TO 資格取得年Ｗ
017720         MOVE 受－資格月   TO 資格取得月Ｗ
017730         MOVE 受－資格日   TO 資格取得日Ｗ
017740         IF (資格取得和暦Ｗ NOT = ZERO) AND
017750            (資格取得年Ｗ   NOT = ZERO) AND
017760            (資格取得月Ｗ   NOT = ZERO) AND
017770            (資格取得日Ｗ   NOT = ZERO)
017780             EVALUATE 資格取得和暦Ｗ
017790             WHEN 3
017800                 MOVE NC"○"  TO 資格昭和チェックＷ
017810             WHEN 4
017820                 MOVE NC"○"  TO 資格平成チェックＷ
017810             WHEN 5
017820                 MOVE NC"○"  TO 資格令和チェックＷ
017830             END-EVALUATE
017840         END-IF
017850**市町村国保､退職の場合は有効期限を出力
017860**業種別国保(国保組合)､共済､日雇､船員､社保､組合､自衛官は有効期限日を出力する
017870*         IF (( 受－保険種別 = 08 ) OR
017880*            (( 受－保険種別 = 01 ) AND
017890*            (  受－保険者番号(3:1) NOT = "3" )))
017900*             MOVE 受－有効和暦 TO 資格取得和暦Ｗ
017910*             MOVE 受－有効年   TO 資格取得年Ｗ
017920*             MOVE 受－有効月   TO 資格取得月Ｗ
017930*             MOVE 受－有効日   TO 資格取得日Ｗ
017940*         ELSE
017950*             MOVE 受－資格和暦 TO 資格取得和暦Ｗ
017960*             MOVE 受－資格年   TO 資格取得年Ｗ
017970*             MOVE 受－資格月   TO 資格取得月Ｗ
017980*             MOVE 受－資格日   TO 資格取得日Ｗ
017990*         END-IF
018000*         IF (資格取得和暦Ｗ NOT = ZERO) AND
018010*            (資格取得年Ｗ   NOT = ZERO) AND
018020*            (資格取得月Ｗ   NOT = ZERO) AND
018030*            (資格取得日Ｗ   NOT = ZERO)
018040*             EVALUATE 資格取得和暦Ｗ
018050*             WHEN 3
018060*                 MOVE NC"○"  TO 資格昭和チェックＷ
018070*             WHEN 4
018080*                 MOVE NC"○"  TO 資格平成チェックＷ
018070*             WHEN 5
018080*                 MOVE NC"○"  TO 資格令和チェックＷ
018090*             END-EVALUATE
018100*         END-IF
018110* 市町村番号 受給者番号
018120         IF 受－助成種別 NOT = ZERO
018130             MOVE 受－費用負担者番号助成 TO 費用負担者番号Ｗ
018140             MOVE 受－受益者番号助成     TO 受益者番号助成Ｗ
018150         END-IF
018290         IF ( 受－公費種別 = 05 ) AND ( 受－施術和暦年月 < 42004 )
018170             MOVE 受－費用負担者番号     TO 市町村番号Ｗ
018180             MOVE 受－受益者番号老人     TO 受益者番号Ｗ
018190         END-IF
018200*/助成種別
018210         IF 受－助成種別 NOT = ZERO
018220             MOVE NC"○"   TO 公費チェックＷ
018230         END-IF
018240*         EVALUATE 受－助成種別
018250*         WHEN 50
018260*             MOVE NC"○"  TO 生保チェックＷ
018270*         WHEN 51
018280**/頭4桁が "4113"東京 "4108"茨城 "4132"島根 の時は、「福」。それ以外は「老」
018290*             IF  受－費用負担者番号助成(1:4) = "4113" OR
018300*                 受－費用負担者番号助成(1:4) = "4108" OR
018310*                 受－費用負担者番号助成(1:4) = "4132"
018320*                 MOVE NC"○"    TO 副チェックＷ
018330*             ELSE
018340*                 MOVE NC"○"    TO 老人チェックＷ
018350*             END-IF
018360*         WHEN 52
018370*             MOVE NC"○"  TO 母子チェックＷ
018380*         WHEN 53
018390*             MOVE NC"○"  TO 身障チェックＷ
018400*         WHEN 54
018410*             MOVE NC"○"  TO 原爆チェックＷ
018420*         WHEN 55
018430*             MOVE NC"○"  TO 乳幼児チェックＷ
018440*         END-EVALUATE
018450*
018460         IF 受－公費種別 = 05
018470             MOVE NC"○"  TO 老人チェックＷ
018480         ELSE
018490             EVALUATE 受－保険種別
018500             WHEN 01
018510                 MOVE NC"○"  TO 国保チェックＷ
018520             WHEN 02
018530                 MOVE NC"○"  TO 社保チェックＷ
018540             WHEN 03
018550                 MOVE NC"○"  TO 組合チェックＷ
018560             WHEN 04
018570                 MOVE NC"○"  TO 共済チェックＷ
018580             WHEN 06
018590                 MOVE NC"○"  TO 日雇チェックＷ
018600             WHEN 07
018610                 MOVE NC"○"  TO 船員チェックＷ
018620             WHEN 08
018630                 MOVE NC"○"  TO 退職チェックＷ
018640             WHEN 09
018650                 MOVE NC"○"  TO 自衛チェックＷ
018660             WHEN 90
018670                 MOVE NC"○"  TO 自費チェックＷ
018680*             WHEN 70
018690*                 MOVE NC"○"  TO 労災チェックＷ
018700*             WHEN 80
018710*                 MOVE NC"○"  TO 自賠チェックＷ
018720             END-EVALUATE
018730         END-IF
018740     END-READ.
019150*================================================================*
019160 テスト SECTION.
019170*
018790      MOVE NC"○"  TO 被保険者男チェックＷ.
018800      MOVE NC"○"  TO 被保険者女チェックＷ.
018810      MOVE NC"○"  TO 患者男チェックＷ.
018820      MOVE NC"○"  TO 患者女チェックＷ.
018830      MOVE NC"○"  TO 明治チェックＷ.
018840      MOVE NC"○"  TO 大正チェックＷ.
018850      MOVE NC"○"  TO 昭和チェックＷ.
018860      MOVE NC"○"  TO 平成チェックＷ.
018860      MOVE NC"○"  TO 令和チェックＷ.
018870      MOVE NC"○"  TO 社保チェックＷ    .
018880      MOVE NC"○"  TO 組合チェックＷ    .
018890      MOVE NC"○"  TO 共済チェックＷ    .
018900      MOVE NC"○"  TO 国保チェックＷ    .
018910      MOVE NC"○"  TO 日雇チェックＷ    .
018920      MOVE NC"○"  TO 船員チェックＷ    .
018930      MOVE NC"○"  TO 老人チェックＷ    .
018940      MOVE NC"○"  TO 福チェックＷ    .
018950      MOVE NC"○"  TO 身障チェックＷ    .
018960      MOVE NC"○"  TO 退職チェックＷ    .
018970      MOVE NC"○"  TO 母子チェックＷ    .
018980      MOVE NC"○"  TO 乳幼児チェックＷ  .
018990      MOVE NC"○"  TO 国保組合チェックＷ.
019000      MOVE NC"○"  TO 労災チェックＷ    .
019010      MOVE NC"○"  TO 生保チェックＷ    .
019020      MOVE NC"○"  TO 自賠チェックＷ    .
019030      MOVE NC"○"  TO 原爆チェックＷ    .
019040      MOVE NC"○" TO ０割チェックＷ.
019050      MOVE NC"○" TO １割チェックＷ.
019060      MOVE NC"○" TO ２割チェックＷ.
019070      MOVE NC"○" TO ３割チェックＷ.
019080      MOVE NC"○"  TO 資格昭和チェックＷ.
019090      MOVE NC"○"  TO 資格平成チェックＷ.
019090      MOVE NC"○"  TO 資格令和チェックＷ.
019100      MOVE NC"○"  TO 被保険者明治チェックＷ.
019110      MOVE NC"○"  TO 被保険者大正チェックＷ.
019120      MOVE NC"○"  TO 被保険者昭和チェックＷ.
019130      MOVE NC"○"  TO 被保険者平成チェックＷ.
019130      MOVE NC"○"  TO 被保険者令和チェックＷ.
            PERFORM VARYING カウンタ FROM 1 BY 1
                      UNTIL カウンタ > 5
               MOVE ALL "静" TO 負傷原因Ｗ(カウンタ)
               MOVE ALL NC"静" TO 負傷名Ｗ(カウンタ)
               MOVE NC"○"   TO 治癒チェックＷ(カウンタ) 中止チェックＷ(カウンタ) 転医チェックＷ(カウンタ)
            END-PERFORM.
            MOVE 99       TO 負傷時Ｗ(1) 負傷分Ｗ(1) 資格取得年Ｗ 資格取得月Ｗ 資格取得日Ｗ.
            MOVE ALL "9"  TO 受益者番号助成Ｗ.
019140*-------------------------------------
019150*================================================================*
019160 請求先情報取得 SECTION.
019170*
019180****************************************************
019190* 連結データから保険者マスタより請求先を取得する。 *
019200* ※保－請求先情報区分=1の場合請求先マスタを使用   *
019210* ● 保険者番号...保険者番号Ｗに格納               *
019220* ● 名称........ 請求先名称Ｗに格納               *
019230* ● 郵便番号１.. 請求先郵便番号１Ｗに格納         *
019240* ● 郵便番号２.. 請求先郵便番号２Ｗに格納         *
019250* ● 住所１.......請求先住所１Ｗに格納             *
019260* ● 住所２.......請求先住所２Ｗに格納             *
019270****************************************************
019280     MOVE SPACE          TO 保険者番号Ｗ.
019290     MOVE SPACE          TO 請求先名称Ｗ.
019300     MOVE SPACE          TO 支部名Ｗ.
019310     MOVE SPACE          TO 請求先郵便番号Ｗ.
019320     MOVE SPACE          TO 請求先郵便番号１Ｗ.
019330     MOVE SPACE          TO 請求先郵便番号２Ｗ.
019340     MOVE SPACE          TO 請求先住所１Ｗ.
019350     MOVE SPACE          TO 請求先住所２Ｗ.
019360     MOVE SPACE          TO 保険者住所１Ｗ.
019370     MOVE SPACE          TO 保険者住所２Ｗ.
019380     MOVE SPACE          TO 印刷保険者呼名Ｗ.
019390     MOVE SPACE          TO 請求先名支部名Ｗ
019400     MOVE ZERO           TO 接尾語区分Ｗ.
019410*
019420     MOVE 保険種別ＷＲ   TO 保－保険種別.
019430     MOVE 保険者番号ＷＲ TO 保－保険者番号.
019440     READ 保険者マスタ
019450     INVALID KEY
               IF ( 受－施術和暦年月 >= 42004 ) AND ( 保険種別ＷＲ = 05 )
016990             MOVE 保険種別ＷＲ   TO 市－公費種別
017000             MOVE 保険者番号ＷＲ TO 市－市町村番号
017010             READ 市町村マスタ
017040             NOT INVALID KEY
017090                 MOVE 市－市町村番号  TO 保険者番号Ｗ
017110                 MOVE 市－市町村名称  TO 請求先名称Ｗ
019560                 MOVE 市－支部部署名  TO 支部名Ｗ
017120                 MOVE 市－郵便番号１  TO 請求先郵便番号１Ｗ
017130                 MOVE 市－郵便番号２  TO 請求先郵便番号２Ｗ
019530                 IF 請求先郵便番号Ｗ NOT = SPACE
019540                     MOVE "-"         TO 請求先郵便番号区切Ｗ
019550                 END-IF
019580                 STRING 市－住所１ DELIMITED BY SPACE
019590                        市－住所２ DELIMITED BY SPACE
019600                   INTO 保険者住所Ｗ
019610                 END-STRING
                   END-READ
               END-IF
019470     NOT INVALID KEY
019480         MOVE 保－保険者番号  TO 保険者番号Ｗ
019490         MOVE 保－保険者名称  TO 請求先名称Ｗ
019500         MOVE 保－支部部署名  TO 支部名Ｗ
019510         MOVE 保－郵便番号１  TO 請求先郵便番号１Ｗ
019520         MOVE 保－郵便番号２  TO 請求先郵便番号２Ｗ
019530         IF 請求先郵便番号Ｗ NOT = SPACE
019540             MOVE "-"         TO 請求先郵便番号区切Ｗ
019550         END-IF
019560*         MOVE 保－住所１      TO 請求先住所１Ｗ
019570*         MOVE 保－住所２      TO 請求先住所２Ｗ
019580         STRING 保－住所１ DELIMITED BY SPACE
019590                保－住所２ DELIMITED BY SPACE
019600           INTO 保険者住所Ｗ
019610         END-STRING
019620         MOVE 保－接尾語区分  TO 接尾語区分Ｗ
019630     END-READ.
           IF 保険種別ＷＲ = 85
              MOVE 施術和暦年月ＷＲ TO 生保－施術和暦年月
              MOVE 患者コードＷＲ   TO 生保－患者コード
              READ 生保情報Ｆ
              NOT INVALID KEY
019480           MOVE 生保－負担者番号    TO 保険者番号Ｗ
019490           MOVE 生保－生保市町村名  TO 請求先名称Ｗ
019500           MOVE SPACE               TO 支部名Ｗ
019510           MOVE 生保－生保送付先郵便番号１    TO 請求先郵便番号１Ｗ
019520           MOVE 生保－生保送付先郵便番号２    TO 請求先郵便番号２Ｗ
019530           IF 請求先郵便番号Ｗ NOT = SPACE
019540               MOVE "-"         TO 請求先郵便番号区切Ｗ
019550           END-IF
019580           STRING 生保－生保送付先住所１ DELIMITED BY SPACE
019590                  生保－生保送付先住所２ DELIMITED BY SPACE
019600             INTO 保険者住所Ｗ
019610           END-STRING
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
019530               IF 請求先郵便番号Ｗ NOT = SPACE
019540                   MOVE "-"         TO 請求先郵便番号区切Ｗ
019550               END-IF
019580               STRING 保険会－住所１ DELIMITED BY SPACE
019590                      保険会－住所２ DELIMITED BY SPACE
019600                 INTO 保険者住所Ｗ
019610               END-STRING
                  ELSE
022450               MOVE 自賠－保険会社送付先郵便番号１ TO 請求先郵便番号１Ｗ
022460               MOVE 自賠－保険会社送付先郵便番号２ TO 請求先郵便番号２Ｗ
019530               IF 請求先郵便番号Ｗ NOT = SPACE
019540                   MOVE "-"         TO 請求先郵便番号区切Ｗ
019550               END-IF
019580               STRING 自賠－保険会社送付先住所１ DELIMITED BY SPACE
019590                      自賠－保険会社送付先住所２ DELIMITED BY SPACE
019600                 INTO 保険者住所Ｗ
019610               END-STRING
                  END-IF
022080        END-READ
022170     END-IF.
022180*
019640     EVALUATE 保険種別ＷＲ
019650     WHEN 2
019660     WHEN 6
019670         IF 接尾語区分Ｗ = 1
019680            MOVE SPACE              TO 印刷保険者呼名Ｗ
019690         ELSE
019700            MOVE "社会保険事務所"   TO 印刷保険者呼名Ｗ
019710         END-IF
019720     WHEN 3
019730         MOVE "健康保険組合"   TO 印刷保険者呼名Ｗ
019740     WHEN OTHER
019750         MOVE SPACE TO 印刷保険者呼名Ｗ
019760     END-EVALUATE.
019770     IF 印刷保険者呼名Ｗ NOT = SPACE
019780         STRING 請求先名称Ｗ DELIMITED BY SPACE
019790                保険者呼名Ｗ DELIMITED BY SPACE
019800           INTO 請求先名称Ｗ
019810         END-STRING
019820     END-IF.
019830*組合と共済は支部名も印字する
019840     EVALUATE 保険種別ＷＲ
019850     WHEN 03
019860     WHEN 04
019870         STRING 請求先名称Ｗ DELIMITED BY SPACE
019880                "  "         DELIMITED BY SIZE
019890                支部名Ｗ     DELIMITED BY SPACE
019900           INTO 請求先名支部名Ｗ
019910         END-STRING
019920     WHEN OTHER
019930         MOVE 請求先名称Ｗ TO 請求先名支部名Ｗ
019940     END-EVALUATE.
019950*================================================================*
019960 負傷データ取得 SECTION.
019970*
019980**************************************************
019990* 連結データから負傷データＦより以下の情報を取得 *
020000* ● 負傷名...部位＋負傷種別にて加工して格納     *
020010* ● 負傷年.......負傷年Ｗ                       *
020020* ● 負傷月.......負傷月Ｗ                       *
020030* ● 負傷日.......負傷日Ｗ                       *
020040* ● 施術終了年...終了年Ｗ                       *
020050* ● 施術終了月...終了月Ｗ                       *
020060* ● 施術終了日...終了日Ｗ                       *
020070**************************************************
020080     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
020090     MOVE 施術年ＷＲ         TO 負－施術年.
020100     MOVE 施術月ＷＲ         TO 負－施術月.
020110     MOVE 患者コードＷＲ     TO 負－患者コード.
020120     READ 負傷データＦ
020130     INVALID KEY
020140         CONTINUE
020150*            /* ありえない */
020160     NOT INVALID KEY
020170         MOVE ZERO                         TO 部位数Ｗ
020180         MOVE 負－部位数                   TO 部位数Ｗ
020190         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
020200                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
020210*********************************************
020220* 注）全柔...負傷種別＋部位にて加工して格納 *
020230*********************************************
020240* 負傷種別
020250             MOVE SPACE                     TO 負傷名称Ｗ
020260             MOVE 03                        TO 名－区分コード
020270             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
020280             READ 名称マスタ
020290             INVALID KEY
020300                 MOVE SPACE    TO 負傷名称Ｗ
020310             NOT INVALID KEY
020320*                 MOVE 名－略称 TO 負傷名称Ｗ
020330                 MOVE 名－正式名称 TO 負傷名称Ｗ
020340             END-READ
020350* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
020370             PERFORM 名称埋込処理
020430             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
020440             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
020450             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
020460*             IF 負－負傷年(部位ＣＮＴ) NOT = ZERO 
020470*                MOVE "."                   TO 負傷年月日区切Ｗ(部位ＣＮＴ)
020480*             END-IF
020490*
020500             MOVE 負－開始年(部位ＣＮＴ)   TO 施術開始年Ｗ(部位ＣＮＴ)
020510             MOVE 負－開始月(部位ＣＮＴ)   TO 施術開始月Ｗ(部位ＣＮＴ)
020520             MOVE 負－開始日(部位ＣＮＴ)   TO 施術開始日Ｗ(部位ＣＮＴ)
020530*             IF 負－開始年(部位ＣＮＴ) NOT = ZERO 
020540*                MOVE "."                   TO 施開年月日区切Ｗ(部位ＣＮＴ)
020550*             END-IF
020560*
020570             MOVE 負－終了年(部位ＣＮＴ)   TO 施術終了年Ｗ(部位ＣＮＴ)
020580             MOVE 負－終了月(部位ＣＮＴ)   TO 施術終了月Ｗ(部位ＣＮＴ)
020590             MOVE 負－終了日(部位ＣＮＴ)   TO 施術終了日Ｗ(部位ＣＮＴ)
020600*             IF 負－終了年(部位ＣＮＴ) NOT = ZERO 
020610*                MOVE "."                   TO 施終年月日区切Ｗ(部位ＣＮＴ)
020620*             END-IF
020630*転帰
020640             EVALUATE 負－転帰区分(部位ＣＮＴ)
020650             WHEN 1
020660             WHEN 2
020670                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
020680             WHEN 3
020690                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
020700             WHEN 4
020710                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
020720             END-EVALUATE
020730*/          */負傷時間を追加
020740             MOVE 負－負傷時(部位ＣＮＴ) TO 負傷時Ｗ(部位ＣＮＴ)
020750             MOVE 負－負傷分(部位ＣＮＴ) TO 負傷分Ｗ(部位ＣＮＴ)
020760*             IF 負－転帰区分(部位ＣＮＴ) NOT = 9
020770*                 MOVE 04                        TO 名－区分コード
020780*                 MOVE 負－転帰区分(部位ＣＮＴ)  TO 名－名称コード
020790*                 READ 名称マスタ
020800*                 INVALID KEY
020810*                     MOVE SPACE    TO 転帰Ｗ(部位ＣＮＴ)
020820*                 NOT INVALID KEY
020830*                     MOVE 名－略称 TO 転帰Ｗ(部位ＣＮＴ)
020840*                 END-READ
020850*             END-IF
020860*hilo---------------------------------
020870*         MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
020880*         MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
020890*         MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
020900*-------------------------------------
020910             PERFORM 負傷データ退避
020920         END-PERFORM
020930     END-READ.
020940*================================================================*
020950 施術記録取得 SECTION.
020960*
020970************************************************************
020980* 連結データから負傷データＦより以下の情報を取得           *
020990* ● 施術開始年月日...該当する部位に対して当月最初の施術日 *
021000************************************************************
021010     MOVE 施術和暦ＷＲ  TO 施記－施術和暦.
021020     MOVE 施術年ＷＲ    TO 施記－施術年.
021030     MOVE 施術月ＷＲ    TO 施記－施術月.
021040     MOVE ZERO            TO 施記－施術日.
021050     MOVE ZERO            TO 施記－患者番号.
021060     MOVE SPACE           TO 施記－枝番.
021070     START 施術記録Ｆ   KEY IS >= 施記－施術和暦年月日
021080                                  施記－患者コード.
021090     IF 状態キー = "00"
021100         MOVE SPACE  TO 終了フラグ２
021110         PERFORM 施術記録Ｆ読込
021120         PERFORM UNTIL ( 終了フラグ２       = "YES" ) OR
021130                       ( 施記－施術和暦 NOT = 施術和暦ＷＲ ) OR
021140                       ( 施記－施術年   NOT = 施術年ＷＲ   ) OR
021150                       ( 施記－施術月   NOT = 施術月ＷＲ   )
021160*            **************
021170*            * 開始年月日 *
021180*            **************
021190             PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
021200                     UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
021210            EVALUATE TRUE ALSO TRUE ALSO TRUE ALSO TRUE ALSO TRUE
021220            WHEN 施記－負傷種別(部位ＣＮＴ) = 負傷種別Ｗ(部位ＣＮＴ) ALSO
021230                 施記－部位(部位ＣＮＴ)     = 部位Ｗ(部位ＣＮＴ)     ALSO
021240                 施記－左右区分(部位ＣＮＴ) = 左右区分Ｗ(部位ＣＮＴ) ALSO
021250                 施記－負傷位置番号(部位ＣＮＴ)
021260                                       = 負傷位置番号Ｗ(部位ＣＮＴ)  ALSO
021270                 開始年月日取得フラグ(部位ＣＮＴ) = SPACE
021280                   MOVE 施記－施術年     TO 施術開始年Ｗ(部位ＣＮＴ)
021290                   MOVE 施記－施術月     TO 施術開始月Ｗ(部位ＣＮＴ)
021300                   MOVE 施記－施術日     TO 施術開始日Ｗ(部位ＣＮＴ)
021310                   MOVE "YES"       TO 開始年月日取得フラグ(部位ＣＮＴ)
021320*
021330            WHEN OTHER
021340                CONTINUE
021350            END-EVALUATE
021360             END-PERFORM
021370             PERFORM 施術記録Ｆ読込
021380         END-PERFORM
021390     END-IF.
021400*================================================================*
021410 施術記録Ｆ読込 SECTION.
021420*
021430     READ 施術記録Ｆ NEXT
021440     AT END
021450         MOVE "YES" TO 終了フラグ２
021460     END-READ.
021470*
022650*================================================================*
022660 Ｑ詳細モード負傷原因処理 SECTION.
022670*
022680*******************************************************************
022690* 連印－負傷印刷行により、負傷原因データの並び替えを行う。
022700* ( 負傷原因情報Ｗを初期化し、退避済みの負傷原因情報ＳＷから
022710*         並び替えを行い、負傷原因情報Ｗを再設定する。)
022720*******************************************************************
022730*
022740     INITIALIZE 負傷原因情報Ｗ.
022750*
022760     PERFORM VARYING 詳細ＣＮＴ FROM 1 BY 1
022770                                UNTIL ( 詳細ＣＮＴ > 5)
022780*
022790        IF ( 連印－負傷印刷行(詳細ＣＮＴ) NOT = ZERO) AND
022800           ( 連印－原因印刷区分(詳細ＣＮＴ)   = 1)
022810*
022820           MOVE 連印－負傷印刷行(詳細ＣＮＴ) TO 印刷行ＳＷ
022830*
022840           MOVE 負傷患者番号ＳＷ(詳細ＣＮＴ) TO 負傷原因患者番号Ｗ(印刷行ＳＷ)
022850           MOVE 負傷連番ＳＷ(詳細ＣＮＴ) TO 負傷原因連番Ｗ(印刷行ＳＷ)
022860        END-IF
022870*
022880     END-PERFORM.
022890*
022900*================================================================*
022910 詳細モード負傷処理 SECTION.
022920*
022930*******************************************************************
022940* 連印－負傷印刷行により、負傷データの並び替えを行う。
022950* ( 負傷情報Ｗを初期化し、退避済みの負傷情報ＳＷから
022960*         並び替えを行い、負傷情報Ｗを再設定する。)
022970*******************************************************************
022980*
022990     INITIALIZE 負傷情報Ｗ.
023000*
023010     PERFORM VARYING 詳細ＣＮＴ FROM 1 BY 1
023020                                UNTIL ( 詳細ＣＮＴ > 5)
023030*
023040        IF 連印－負傷印刷行(詳細ＣＮＴ) NOT = ZERO
023050           MOVE 連印－負傷印刷行(詳細ＣＮＴ) TO 印刷行ＳＷ
023060*
023070           IF 連印－部位印刷区分(詳細ＣＮＴ) = 1
023080*
023090              MOVE 負傷名ＳＷ(詳細ＣＮＴ) TO 負傷名Ｗ(印刷行ＳＷ)
023100              MOVE 負傷年ＳＷ(詳細ＣＮＴ) TO 負傷年Ｗ(印刷行ＳＷ)
023110              MOVE 負傷月ＳＷ(詳細ＣＮＴ) TO 負傷月Ｗ(印刷行ＳＷ)
023120              MOVE 負傷日ＳＷ(詳細ＣＮＴ) TO 負傷日Ｗ(印刷行ＳＷ)
023130              MOVE 施術開始年ＳＷ(詳細ＣＮＴ) TO 施術開始年Ｗ(印刷行ＳＷ)
023140              MOVE 施術開始月ＳＷ(詳細ＣＮＴ) TO 施術開始月Ｗ(印刷行ＳＷ)
023150              MOVE 施術開始日ＳＷ(詳細ＣＮＴ) TO 施術開始日Ｗ(印刷行ＳＷ)
023160*/負傷時間を追加
023170             MOVE 負傷時ＳＷ(詳細ＣＮＴ) TO 負傷時Ｗ(印刷行ＳＷ)
023180             MOVE 負傷分ＳＷ(詳細ＣＮＴ) TO 負傷分Ｗ(印刷行ＳＷ)
023190           END-IF
023200*
023210           IF 連印－転帰印刷区分(詳細ＣＮＴ) = 1
023220              MOVE 施術終了年ＳＷ(詳細ＣＮＴ) TO 施術終了年Ｗ(印刷行ＳＷ)
023230              MOVE 施術終了月ＳＷ(詳細ＣＮＴ) TO 施術終了月Ｗ(印刷行ＳＷ)
023240              MOVE 施術終了日ＳＷ(詳細ＣＮＴ) TO 施術終了日Ｗ(印刷行ＳＷ)
023250              MOVE 治癒チェックＳＷ(詳細ＣＮＴ) TO 治癒チェックＷ(印刷行ＳＷ)
023260              MOVE 中止チェックＳＷ(詳細ＣＮＴ) TO 中止チェックＷ(印刷行ＳＷ)
023270              MOVE 転医チェックＳＷ(詳細ＣＮＴ) TO 転医チェックＷ(印刷行ＳＷ)
023280           END-IF
023290        END-IF
023300*
023310     END-PERFORM.
023320*
023330*================================================================*
023340 負傷データ退避 SECTION.
023350*
023360*********************************************************************
023370* ① 負傷原因取得のため、負傷データＦから負傷原因Ｗに退避する。
023380* ② 詳細モード用  連印－負傷行印刷により、印刷位置を並び替えるため、
023390*                  負傷情報Ｗ  から負傷情報ＳＷに、
023400*                  負傷原因Ｗ  から負傷原因ＳＷに退避する。
023410*********************************************************************
023420*
023430     MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷原因患者番号Ｗ(部位ＣＮＴ).
023440     MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷原因連番Ｗ(部位ＣＮＴ).
023450*--------------------------------------------------------------------*
023460     MOVE 負傷名Ｗ(部位ＣＮＴ) TO 負傷名ＳＷ(部位ＣＮＴ).
023470*
023480     MOVE 負傷年Ｗ(部位ＣＮＴ) TO 負傷年ＳＷ(部位ＣＮＴ).
023490     MOVE 負傷月Ｗ(部位ＣＮＴ) TO 負傷月ＳＷ(部位ＣＮＴ).
023500     MOVE 負傷日Ｗ(部位ＣＮＴ) TO 負傷日ＳＷ(部位ＣＮＴ).
023510*
023520     MOVE 施術開始年Ｗ(部位ＣＮＴ) TO 施術開始年ＳＷ(部位ＣＮＴ).
023530     MOVE 施術開始月Ｗ(部位ＣＮＴ) TO 施術開始月ＳＷ(部位ＣＮＴ).
023540     MOVE 施術開始日Ｗ(部位ＣＮＴ) TO 施術開始日ＳＷ(部位ＣＮＴ).
023550*
023560     MOVE 施術終了年Ｗ(部位ＣＮＴ) TO 施術終了年ＳＷ(部位ＣＮＴ).
023570     MOVE 施術終了月Ｗ(部位ＣＮＴ) TO 施術終了月ＳＷ(部位ＣＮＴ).
023580     MOVE 施術終了日Ｗ(部位ＣＮＴ) TO 施術終了日ＳＷ(部位ＣＮＴ).
023590*
023600*/負傷時間を追加
023610     MOVE 負傷時Ｗ(部位ＣＮＴ)     TO 負傷時ＳＷ(部位ＣＮＴ).
023620     MOVE 負傷分Ｗ(部位ＣＮＴ)     TO 負傷分ＳＷ(部位ＣＮＴ).
023630*
023640     MOVE 治癒チェックＷ(部位ＣＮＴ) TO 治癒チェックＳＷ(部位ＣＮＴ).
023650     MOVE 中止チェックＷ(部位ＣＮＴ) TO 中止チェックＳＷ(部位ＣＮＴ).
023660     MOVE 転医チェックＷ(部位ＣＮＴ) TO 転医チェックＳＷ(部位ＣＮＴ).
023670*     MOVE 施術累計回数Ｗ(部位ＣＮＴ) TO 施術累計回数ＳＷ(部位ＣＮＴ).
023680*
023690     MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号ＳＷ(部位ＣＮＴ).
023700     MOVE 負傷原因連番Ｗ(部位ＣＮＴ)     TO 負傷連番ＳＷ(部位ＣＮＴ).
023710*
023720*================================================================*
023730 印刷処理 SECTION.
023740*
023750     EVALUATE 印刷モードＦＷＲ
023760     WHEN 0
023770         PERFORM 印刷処理２
023780         PERFORM 印刷処理３
023790     WHEN 1
023800         PERFORM 印刷処理２
023810     WHEN 2
023820         PERFORM 印刷処理３
023850     WHEN 4
023860         IF 連印－保険証印刷区分 = 1
023870             PERFORM 印刷処理２
023880         END-IF
023890         PERFORM 印刷処理３
023900     END-EVALUATE.
024000*================================================================*
024010 印刷処理２ SECTION.
024020*
024030     MOVE "YJB6622P"  TO  定義体名Ｐ.
024040     MOVE "GRP001"   TO  項目群名Ｐ.
024050     WRITE YJB6622P.
024060*     WRITE 印刷レコード.
024070     PERFORM エラー処理Ｐ.
024080*================================================================*
024090 印刷処理３ SECTION.
024100*
024110     MOVE "YJB6622P"  TO  定義体名Ｐ.
024120     MOVE "GRP002"   TO  項目群名Ｐ.
024130     WRITE YJB6622P.
024140*     WRITE 印刷レコード.
024150     PERFORM エラー処理Ｐ.
024230*================================================================*
024240 エラー処理Ｐ SECTION.
024250*
024260     IF 通知情報Ｐ NOT = "00"
024270         DISPLAY NC"帳票エラー"              UPON CONS
024280         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
024290         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
024300         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
024310         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
024320                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
024330         ACCEPT  キー入力 FROM CONS
024340         PERFORM ファイル閉鎖
024350         EXIT PROGRAM
024360     END-IF.
024370*================================================================*
024380 名称埋込処理 SECTION.
024390*
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
011180*
024500     END-READ.
024510*
026430*================================================================*
020540 負傷原因取得 SECTION.
020550*
      */負傷原因を部位毎の欄に印刷するように変更0302
020660     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
020670             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
020680                OR ( 部位ＣＮＴ > 5 )
020990** 健保は 区分 01 労災は 02 自賠責は 03
021000         EVALUATE 保険種別ＷＲ
021010         WHEN 80
021020             MOVE 03 TO 負原－区分コード
021030         WHEN 70
021040             MOVE 02 TO 負原－区分コード
021050         WHEN OTHER
021060             MOVE 01 TO 負原－区分コード
021070         END-EVALUATE
               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負原－患者番号
               MOVE 負－負傷連番    (部位ＣＮＴ) TO 負原－負傷原因連番
021100         READ 負傷原因Ｆ
021110         NOT INVALID KEY
021120             INITIALIZE 負傷原因ＷＴ
021130             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
021140             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
021150             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
021160             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
021170             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
                   IF 印刷モードＦＷＲ NOT = 4
                       MOVE 部位ＣＮＴ TO 部位番号Ｗ
                   ELSE
                       MOVE 連印－負傷印刷行(部位ＣＮＴ) TO 部位番号Ｗ
                   END-IF
      *
021210             EVALUATE 部位番号Ｗ
021220             WHEN 1
021230                MOVE "①"  TO 負傷原因ナンバーＮＷ
021240             WHEN 2
021250                MOVE "②"  TO 負傷原因ナンバーＮＷ
021260             WHEN 3
021270                MOVE "③"  TO 負傷原因ナンバーＮＷ
021280             WHEN 4
021290                MOVE "④"  TO 負傷原因ナンバーＮＷ
021300             WHEN 5
021310                MOVE "⑤"  TO 負傷原因ナンバーＮＷ
021320             WHEN OTHER
021330                CONTINUE
021340             END-EVALUATE
021360*
025582             IF 負原－負傷原因入力区分 = 1
025583                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
025584                        負傷原因１ＷＴ  DELIMITED BY SIZE
025585                        負傷原因２ＷＴ  DELIMITED BY SIZE
025586                        負傷原因３ＷＴ  DELIMITED BY SIZE
025587                        負傷原因４ＷＴ  DELIMITED BY SIZE
025588                        負傷原因５ＷＴ  DELIMITED BY SIZE
025589                        INTO 負傷原因内容合成Ｗ(部位ＣＮＴ)
025590                 END-STRING
025591             ELSE
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
025600             END-IF
021430             MOVE 負傷原因内容合成Ｗ(部位ＣＮＴ) TO 負傷原因Ｗ(部位ＣＮＴ)
021450         END-READ
               PERFORM 負傷原因退避
021460     END-PERFORM.
021470*
021480**     PERFORM 負傷原因セット.
021490*
021500*================================================================*
       負傷原因退避 SECTION.
      *
      */負傷原因を部位毎の欄に印刷するように変更0302
           MOVE 負傷原因Ｗ(部位ＣＮＴ) TO 負傷原因ＳＷ(部位ＣＮＴ).
021500*================================================================*
       詳細モード負傷原因処理 SECTION.
      *
      */負傷原因を部位毎の欄に印刷するように変更0302
           PERFORM VARYING 負傷原因ＣＮＴ FROM 1 BY 1 
                     UNTIL 負傷原因ＣＮＴ > 5
               IF (連印－負傷印刷行  (負傷原因ＣＮＴ) NOT = ZERO) AND
                  (連印－原因印刷区分(負傷原因ＣＮＴ)     = 1)
                   MOVE 負傷原因ＳＷ(負傷原因ＣＮＴ)  TO 負傷原因Ｗ(連印－負傷印刷行(負傷原因ＣＮＴ))
               END-IF
           END-PERFORM.
021500*================================================================*
026440******************************************************************
026450 END PROGRAM YJB6622.
026460******************************************************************
