000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCH662.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*         中央用
000100*　　　　　カルテ印刷（柔＋ｳｨﾝﾄﾞｳｽﾞ版）再生紙
000110*         MED = YAW662G YCH662P
      * 2014/10/15 受給者番号15桁対応
      * 2016/03/09 所見、同意、備考の印刷対応
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           1998-10-21
000140 DATE-COMPILED.          1998-10-21
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
001280     SELECT  負担率マスタ    ASSIGN      TO        HUTANRIL
001290                             ORGANIZATION             IS  INDEXED
001300                             ACCESS MODE              IS  DYNAMIC
001310                             RECORD KEY               IS  負率－保険種別
001320                                                          負率－開始和暦年月
001330                             FILE STATUS              IS  状態キー
001340                             LOCK        MODE         IS  AUTOMATIC.
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
001720     SELECT カルテファイル   ASSIGN      TO        KARUTEL
001721                             ORGANIZATION             IS  INDEXED
001722                             ACCESS                   IS  DYNAMIC
001723                             RECORD      KEY          IS  カ－会コード
001724                                                          カ－区分
001725                                                          カ－施術和暦年月
001726                                                          カ－患者コード
001727                             ALTERNATE RECORD KEY     IS  カ－会コード
001728                                                          カ－区分
001729                                                          カ－患者コード
001730                                                          カ－施術和暦年月
001731                             ALTERNATE RECORD KEY     IS  カ－区分
001732                                                          カ－施術和暦年月
001733                                                          カ－患者コード
001734                                                          カ－会コード
001735                             ALTERNATE RECORD KEY     IS  カ－区分
001736                                                          カ－患者コード
001737                                                          カ－施術和暦年月
001738                                                          カ－会コード
001739                             FILE        STATUS       IS  状態キー
001740                             LOCK        MODE         IS  AUTOMATIC.
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
002050*                           ［ＲＬ＝  ２５６］
002060 FD  負担率マスタ        BLOCK   CONTAINS   1   RECORDS.
002070     COPY HUTANRI         OF  XFDLIB  JOINING   負率 AS  PREFIX.
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
002300*                           ［ＲＬ＝  ３８４０］
002301 FD  カルテファイル      BLOCK   CONTAINS   1   RECORDS GLOBAL.
002302     COPY KARUTE          OF  XFDLIB  JOINING   カ   AS  PREFIX.
002303     COPY KARUTA          OF  XFDLIB  JOINING   カ表 AS  PREFIX.
002304     COPY KARUTB          OF  XFDLIB  JOINING   カ裏 AS  PREFIX.
002305     COPY KARUTC          OF  XFDLIB  JOINING   カ表コメ AS  PREFIX.
002190*
002200 FD  印刷ファイル.
002210     COPY YCH662P        OF  XMDLIB.
002220*----------------------------------------------------------------*
002230******************************************************************
002240*                WORKING-STORAGE SECTION                         *
002250******************************************************************
002260 WORKING-STORAGE         SECTION.
002270 01 キー入力                           PIC X     VALUE SPACE.
002280 01 状態キー                           PIC X(2)  VALUE SPACE.
002290 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002300 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002130 01 詳細ＣＮＴ                         PIC 9     VALUE ZERO.
002310 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002320 01 文字ＣＮＴ                         PIC 9(2)  VALUE ZERO.
002330 01 初検フラグ                         PIC X(3)  VALUE SPACE.
002340 01 ファイル名                         PIC N(2)  VALUE SPACE.
002350 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
002360 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002400 01 用紙Ｗ                             PIC 9     VALUE ZERO.
002370 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002380 01 部位名称Ｗ                         PIC N(10) VALUE SPACE.
002390 01 負傷番号Ｗ                         PIC 9     VALUE ZERO.
002400 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
002410    03 負傷番号Ｗ１                    PIC X.
002420*
002430 01 全角負傷番号Ｗ                     PIC N     VALUE SPACE.
002440 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
002450    03 全角負傷番号Ｗ１                PIC X(2).
002460*
002470 01 負担率Ｗ                           PIC 9(3) VALUE ZERO.
002480 01 甲乙区分Ｗ                         PIC 9    VALUE ZERO.
002490 01 本人負担率Ｗ                       PIC 9(3) VALUE ZERO.
002500 01 家族負担率Ｗ                       PIC 9(3) VALUE ZERO.
002510*
002520 01 施術和暦年月日ＣＷ.
002530   03 施術和暦年月ＣＷ.
002540     05 施術和暦ＣＷ                   PIC 9    VALUE ZERO.
002550     05 施術年月ＣＷ.
002560        07 施術年ＣＷ                  PIC 9(2) VALUE ZERO.
002570        07 施術月ＣＷ                  PIC 9(2) VALUE ZERO.
002580   03 施術日ＣＷ                       PIC 9(2) VALUE ZERO.
002590 01 施術西暦年Ｗ                       PIC 9(4) VALUE ZERO.
002600 01 患者西暦年Ｗ                       PIC 9(4) VALUE ZERO.
002610 01 被保険者西暦年Ｗ                   PIC 9(4) VALUE ZERO.
002620*
002630** 負傷原因用
002640*
002650 01 カウンタ                           PIC 9(2)  VALUE ZERO.
002660 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
002670*
002680 01 負傷原因ＷＴ.
002690    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
002700    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
002710    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
002720    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
002730    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
002740    03 負傷原因ナンバーＷＴ.
002750       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
002760    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
002770 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
002780 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
002790 01 負傷原因ＴＢＬ.
002800    03 負傷原因コードＴＢＬ            OCCURS 9.
002810       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
002820       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
002830       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
002840 01 負傷原因内容Ｗ.
002850    03 負傷原因内容合成Ｗ              PIC X(316) OCCURS 9 VALUE SPACE.
002910    03 負傷原因内容分解ＸＷ.
002920       05 負傷原因内容１ＸＷ           PIC X(50)  VALUE SPACE.
002930       05 負傷原因内容２ＸＷ           PIC X(50)  VALUE SPACE.
002940       05 負傷原因内容３ＸＷ           PIC X(50)  VALUE SPACE.
002950       05 負傷原因内容４ＸＷ           PIC X(50)  VALUE SPACE.
002960       05 負傷原因内容５ＸＷ           PIC X(50)  VALUE SPACE.
002970       05 負傷原因内容６ＸＷ           PIC X(50)  VALUE SPACE.
002980       05 負傷原因内容７ＸＷ           PIC X(16)  VALUE SPACE.
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
004350*/15桁印字/1410
003440    03 受益者番号助成Ｗ.
003450       05 印刷受益者番号助成Ｗ         PIC X(15)  VALUE SPACE.
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
003790          07 印刷被保険者住所Ｗ        PIC X(100) VALUE SPACE.
003800       05 被保険者電話番号Ｗ           PIC X(15) VALUE SPACE.
003810       05 分割電話番号Ｗ.
003820          07 分割電話番号１Ｗ          PIC X(5)  VALUE SPACE.
003830          07 分割電話番号２Ｗ          PIC X(5)  VALUE SPACE.
003840          07 分割電話番号３Ｗ          PIC X(5)  VALUE SPACE.
003850*          07 市外局番Ｗ                PIC X(5)  VALUE SPACE.
003860*          07 分割局番Ｗ                PIC X(5)  VALUE SPACE.
003870*          07 分割番号Ｗ                PIC X(5)  VALUE SPACE.
003880    03 資格取得年月日Ｗ.
003890       05 資格取得元号Ｗ               PIC N(2)  VALUE SPACE.
003900       05 資格取得和暦Ｗ               PIC 9(1)  VALUE ZERO.
003910       05 資格取得年Ｗ                 PIC 9(2)  VALUE ZERO.
003920       05 資格取得月Ｗ                 PIC 9(2)  VALUE ZERO.
003930       05 資格取得日Ｗ                 PIC 9(2)  VALUE ZERO.
003940    03 有効期限年月日Ｗ.
003950       05 有効期限元号Ｗ               PIC N(2)  VALUE SPACE.
003960       05 有効期限和暦Ｗ               PIC 9(1)  VALUE ZERO.
003970       05 有効期限年Ｗ                 PIC 9(2)  VALUE ZERO.
003980       05 有効期限月Ｗ                 PIC 9(2)  VALUE ZERO.
003990       05 有効期限日Ｗ                 PIC 9(2)  VALUE ZERO.
004000       05 資格和暦チェックＷ.
004010          07 資格昭和チェックＷ        PIC N(1)  VALUE SPACE.
004020          07 資格平成チェックＷ        PIC N(1)  VALUE SPACE.
004020          07 資格令和チェックＷ        PIC N(1)  VALUE SPACE.
004030    03 患者情報Ｗ.
004040       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
004050       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
004060       05 患者性別チェックＷ.
004070          07 患者男チェックＷ          PIC N(1)  VALUE SPACE.
004080          07 患者女チェックＷ          PIC N(1)  VALUE SPACE.
004090       05 続柄Ｗ.
004100          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
004110          07 FILLER                    PIC X(4).
004120       05 和暦チェックＷ.
004130          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
004140          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
004150          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
004160          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
004160          07 令和チェックＷ            PIC N(1)  VALUE SPACE.
004170       05 患者年Ｗ                     PIC 9(2) VALUE ZERO.
004180       05 患者月Ｗ                     PIC 9(2) VALUE ZERO.
004190       05 患者日Ｗ                     PIC 9(2) VALUE ZERO.
004200       05 患者年齢Ｗ                   PIC 9(3) VALUE ZERO.
004210       05 患者性別Ｗ                   PIC N(4) VALUE SPACE.
004220    03 保険種別チェックＷ.
004230       05 社保チェックＷ               PIC N(1) VALUE SPACE.
004240       05 組合チェックＷ               PIC N(1) VALUE SPACE.
004250       05 共済チェックＷ               PIC N(1) VALUE SPACE.
004260       05 国保チェックＷ               PIC N(1) VALUE SPACE.
004270       05 日雇チェックＷ               PIC N(1) VALUE SPACE.
004280       05 船員チェックＷ               PIC N(1) VALUE SPACE.
004290       05 老人チェックＷ               PIC N(1) VALUE SPACE.
004300       05 福チェックＷ                 PIC N(1) VALUE SPACE.
004310       05 身障チェックＷ               PIC N(1) VALUE SPACE.
004320       05 退職チェックＷ               PIC N(1) VALUE SPACE.
004330       05 母子チェックＷ               PIC N(1) VALUE SPACE.
004340       05 乳幼児チェックＷ             PIC N(1) VALUE SPACE.
004350       05 国保組合チェックＷ           PIC N(1) VALUE SPACE.
004360       05 労災チェックＷ               PIC N(1) VALUE SPACE.
004370       05 自賠チェックＷ               PIC N(1) VALUE SPACE.
004380       05 生保チェックＷ               PIC N(1) VALUE SPACE.
004390       05 原爆チェックＷ               PIC N(1) VALUE SPACE.
004400       05 患者郵便番号Ｗ.
004410          07 患者郵便番号１Ｗ          PIC X(3)  VALUE SPACE.
004420          07 患者郵便番号２Ｗ          PIC X(4)  VALUE SPACE.
004430       05 患者住所Ｗ.
004440          07 患者住所１Ｗ              PIC X(50) VALUE SPACE.
004450          07 患者住所２Ｗ              PIC X(50) VALUE SPACE.
004460       05 患者電話番号Ｗ               PIC X(15) VALUE SPACE.
004470    03 負傷原因Ｗ                      PIC X(50) OCCURS 45 VALUE SPACE.
004480**************
004490* 事業所情報 *
004500**************
004510 01 事業所情報Ｗ.
004520    03 事業所名称Ｗ.
004530       05 印刷事業所名称Ｗ             PIC X(60)  VALUE SPACE.
004540    03 事業所郵便番号Ｗ.
004550       05 事業所郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
004560       05 事業所郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
004570    03 事業所住所Ｗ.
004580       05 印刷事業所住所Ｗ             PIC X(50)  VALUE SPACE.
004580       05 印刷事業所住所２Ｗ           PIC X(50)  VALUE SPACE.
004590       05 FILLER                       PIC X(10).
004600**************
004610* 請求先情報 *
004620**************
004630 01 請求先情報Ｗ.
004640    03 保険者番号Ｗ.
004650       05 印刷保険者番号Ｗ             PIC X(8)  VALUE SPACE.
004660       05 FILLER                       PIC X(2).
004670    03 請求先名称Ｗ.
004680       05 印刷請求先名称Ｗ             PIC X(40) VALUE SPACE.
004690    03 支部名Ｗ.
004700       05 印刷支部名Ｗ                 PIC X(40) VALUE SPACE.
004710    03 請求先名支部名Ｗ.
004720       05 印刷請求先名支部名Ｗ         PIC X(54) VALUE SPACE.
004730       05 FILLER                       PIC X(26).
004740    03 保険者呼名Ｗ.
004750*       05 印刷保険者呼名Ｗ             PIC N(7)  VALUE SPACE.
004760       05 印刷保険者呼名Ｗ             PIC X(14)  VALUE SPACE.
004770    03 請求先郵便番号Ｗ.
004780       05 請求先郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
004790       05 請求先郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
004800    03 請求先住所１Ｗ.
004810       05 印刷請求先住所１Ｗ           PIC X(40) VALUE SPACE.
004820    03 請求先住所２Ｗ.
004830       05 印刷請求先住所２Ｗ           PIC X(35) VALUE SPACE.
004840       05 FILLER                       PIC X(5).
004850****************
004860* 負傷データＦ *
004870****************
004880 01 負傷情報Ｗ.
004890    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
004900    03 負傷データ情報Ｗ  OCCURS   9.
004910       05 会部位コードＷ               PIC X(4)  VALUE SPACE.
004920       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
004930       05 負傷種別Ｗ                   PIC 9(2)  VALUE ZERO.
004940       05 部位Ｗ                       PIC 9(2)  VALUE ZERO.
004950       05 左右区分Ｗ                   PIC 9(1)  VALUE ZERO.
004960       05 負傷位置番号Ｗ               PIC 9(2)  VALUE ZERO.
004970       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
004980       05 負傷年月日Ｗ.
004990          07 負傷和暦Ｗ                PIC 9(1)  VALUE ZERO.
005000          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
005010          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
005020          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
005030          07 負傷年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005040       05 施術開始年月日Ｗ.
005050          07 施術開始和暦Ｗ            PIC 9(1)  VALUE ZERO.
005060          07 施術開始年Ｗ              PIC 9(2)  VALUE ZERO.
005070          07 施術開始月Ｗ              PIC 9(2)  VALUE ZERO.
005080          07 施術開始日Ｗ              PIC 9(2)  VALUE ZERO.
005090          07 施開年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005100       05 施術終了年月日Ｗ.
005110          07 施術終了和暦Ｗ            PIC 9(1)  VALUE ZERO.
005120          07 施術終了年Ｗ              PIC 9(2)  VALUE ZERO.
005130          07 施術終了月Ｗ              PIC 9(2)  VALUE ZERO.
005140          07 施術終了日Ｗ              PIC 9(2)  VALUE ZERO.
005150          07 施終年月日区切Ｗ          PIC X(1)  VALUE SPACE.
005160       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
005170       05 転帰Ｗ                       PIC N(4)  VALUE SPACE.
005180       05 転帰チェックＷ.
005190          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
005200          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
005210          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
005220*
005230******************
005240* 負担率チェック *
005250******************
005260 01 負担率チェックＷ.
005270    03 ０割チェックＷ                  PIC N(1)  VALUE SPACE.
005280    03 １割チェックＷ                  PIC N(1)  VALUE SPACE.
005290    03 ２割チェックＷ                  PIC N(1)  VALUE SPACE.
005300    03 ３割チェックＷ                  PIC N(1)  VALUE SPACE.
005310**********
005320* 合計行 *
005330**********
005340 01 合計行Ｗ.
005350    03 請求終了日Ｗ                    PIC 9(2) VALUE ZERO.
005360    03 対象西暦Ｗ                      PIC 9(4) VALUE ZERO.
005370    03 商Ｗ                            PIC 9(3) VALUE ZERO.
005380    03 余Ｗ                            PIC 9(3) VALUE ZERO.
005390*/年月毎持つ保険者データのワーク
005400 01 保負担率区分Ｗ                     PIC 9    VALUE ZERO.
005410 01 保本人負担率Ｗ                     PIC 9(3) VALUE ZERO.
005420 01 保家族負担率Ｗ                     PIC 9(3) VALUE ZERO.
005430 01 保本人負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
005440 01 保家族負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
005450*
005460 01 比較和暦年月Ｗ.
005470    03 比較和暦Ｗ                      PIC 9    VALUE ZERO.
005480    03 比較年Ｗ                        PIC 9(2) VALUE ZERO.
005490    03 比較月Ｗ                        PIC 9(2) VALUE ZERO.
005500*
005510*/老人保険改正の対応0101
005520 01 老人負担計算区分Ｗ                 PIC 9    VALUE ZERO.
005530 01 負担率用種別Ｗ                     PIC 9(2) VALUE ZERO.
005540 01 助成負担区分Ｗ                     PIC 9(2) VALUE ZERO.
005550*
005560** 助成負担率用(14/10～)
005570 01 助成負担率ＷＷ.
005580    03 助成負担率Ｗ                    PIC 9(3) VALUE ZERO.
005590    03 助成負担率Ｗ１                  PIC 9    VALUE ZERO.
005600    03 助成負担率文字Ｗ１              PIC X    VALUE SPACE.
005610    03 助成負担率表示Ｗ                PIC X(6) VALUE SPACE.
      *
      */所見、同意、備考の印刷対応/*160309
       01 カルテＣＭ.
          03 所見Ｗ.
             05 所見ＷＰ                     PIC X(50)  VALUE SPACE OCCURS 10.
          03 同意医師名Ｗ                    PIC X(50)  VALUE SPACE.
          03 同意和暦年月日Ｗ.
             05 同意元号Ｗ                   PIC N(2)   VALUE SPACE.
             05 同意年Ｗ                     PIC 9(2)   VALUE ZERO.
             05 同意年ＴＷ                   PIC X(2)   VALUE "年".
             05 同意月Ｗ                     PIC 9(2)   VALUE ZERO.
             05 同意月ＴＷ                   PIC X(2)   VALUE "月".
             05 同意日Ｗ                     PIC 9(2)   VALUE ZERO.
             05 同意日ＴＷ                   PIC X(2)   VALUE "日".
          03 備考Ｗ.
             05 備考ＷＰ                     PIC X(100) VALUE SPACE OCCURS 5.
005620*
005630*/詳細印刷の対応/0403
005640****************
005650* 詳細用ワーク *
005660****************
005670* 連印－負傷印刷行用ワーク
005680 01 印刷行ＳＷ                           PIC 9(1) VALUE ZERO.
005690*
      */負傷原因の詳細印刷/1010
       01 負傷原因Ｗテーブル２.
          03 負傷原因Ｗ２                    PIC X(50) OCCURS 45 VALUE SPACE.
005700 01 負傷原因情報Ｗ.
005710    03 負傷原因コードＷ  OCCURS   9.
005720       09 負傷原因患者番号Ｗ           PIC 9(6) VALUE ZERO.
005730       09 負傷原因連番Ｗ               PIC 9(4) VALUE ZERO.
005740*
005750 01 負傷原因情報ＳＷ.
005760    03 負傷原因コードＳＷ  OCCURS   9.
005770       09 負傷患者番号ＳＷ               PIC 9(6) VALUE ZERO.
005780       09 負傷連番ＳＷ                   PIC 9(4) VALUE ZERO.
005790*
005800 01 負傷情報ＳＷ.
005810    03 負傷データ情報ＳＷ  OCCURS   9.
005820       05 会部位コードＳＷ               PIC X(4)  VALUE SPACE.
005830       05 部位ＣＮＴＳＷ                 PIC 9(1)  VALUE ZERO.
005840       05 負傷種別ＳＷ                   PIC 9(2)  VALUE ZERO.
005850       05 部位ＳＷ                       PIC 9(2)  VALUE ZERO.
005860       05 左右区分ＳＷ                   PIC 9(1)  VALUE ZERO.
005870       05 負傷位置番号ＳＷ               PIC 9(2)  VALUE ZERO.
005880       05 負傷名ＳＷ                     PIC N(18) VALUE SPACE.
005890       05 負傷年月日ＳＷ.
005900          07 負傷和暦ＳＷ                PIC 9(1)  VALUE ZERO.
005910          07 負傷年ＳＷ                  PIC 9(2)  VALUE ZERO.
005920          07 負傷月ＳＷ                  PIC 9(2)  VALUE ZERO.
005930          07 負傷日ＳＷ                  PIC 9(2)  VALUE ZERO.
005940          07 負傷年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
005950       05 施術開始年月日ＳＷ.
005960          07 施術開始和暦ＳＷ            PIC 9(1)  VALUE ZERO.
005970          07 施術開始年ＳＷ              PIC 9(2)  VALUE ZERO.
005980          07 施術開始月ＳＷ              PIC 9(2)  VALUE ZERO.
005990          07 施術開始日ＳＷ              PIC 9(2)  VALUE ZERO.
006000          07 施開年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
006010       05 施術終了年月日ＳＷ.
006020          07 施術終了和暦ＳＷ            PIC 9(1)  VALUE ZERO.
006030          07 施術終了年ＳＷ              PIC 9(2)  VALUE ZERO.
006040          07 施術終了月ＳＷ              PIC 9(2)  VALUE ZERO.
006050          07 施術終了日ＳＷ              PIC 9(2)  VALUE ZERO.
006060          07 施終年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
006070       05 開始年月日取得フラグＳ         PIC X(3)  VALUE SPACE.
006080       05 転帰ＳＷ                       PIC N(4)  VALUE SPACE.
006090       05 転帰チェックＳＷ.
006100          07 治癒チェックＳＷ            PIC N(1)  VALUE SPACE.
006110          07 中止チェックＳＷ            PIC N(1)  VALUE SPACE.
006120          07 転医チェックＳＷ            PIC N(1)  VALUE SPACE.
006130       05 施術日数ＳＷ                   PIC 9(4)  VALUE ZERO.
006140*/負傷原因を部位毎の欄に印刷するように変更0302
006150       05 負傷原因ＳＷ.
006160          07 負傷原因１ＳＷ              PIC X(30) VALUE SPACE.
006170          07 負傷原因２ＳＷ              PIC X(30) VALUE SPACE.
006180          07 負傷原因３ＳＷ              PIC X(30) VALUE SPACE.
006190          07 負傷原因４ＳＷ              PIC X(30) VALUE SPACE.
006200          07 負傷原因５ＳＷ              PIC X(30) VALUE SPACE.
006210          07 負傷原因６ＳＷ              PIC X(30) VALUE SPACE.
006220          07 負傷原因７ＳＷ              PIC X(30) VALUE SPACE.
006230          07 負傷原因８ＳＷ              PIC X(30) VALUE SPACE.
006240          07 負傷原因９ＳＷ              PIC X(30) VALUE SPACE.
006250          07 負傷原因１０ＳＷ            PIC X(30) VALUE SPACE.
006260***********************************************************************
006270 01 印刷制御.
006280     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
006290     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
006300     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
006310     03 拡張制御Ｐ.
006320         05 端末制御Ｐ.
006330             07 移動方向Ｐ             PIC X(1)  VALUE SPACE.
006340             07 移動行数Ｐ             PIC 9(3)  VALUE ZERO.
006350         05 詳細制御Ｐ                 PIC X(2)  VALUE SPACE.
006360     03 通知情報Ｐ                     PIC X(2)  VALUE SPACE.
006370     03 ユニット名Ｐ                   PIC X(8)  VALUE SPACE.
006380*
006390 01 計算機西暦年Ｗ                     PIC 9(2)  VALUE ZERO.
006400* 日付ＷＯＲＫ
006410 01 和暦終了年Ｗ                       PIC 9(4)  VALUE ZERO.
006420 01 計算機西暦.
006430    03 計算機西暦年                    PIC 9(4)  VALUE ZERO.
006440    03 計算機西暦月日                  PIC 9(4)  VALUE ZERO.
006450 01 計算機西暦Ｒ REDEFINES 計算機西暦.
006460    03 計算機世紀                      PIC 9(2).
006470    03 計算機日付                      PIC 9(6).
006480    03 計算機日付Ｒ REDEFINES 計算機日付.
006490       05 計算機年月                   PIC 9(4).
006500       05 計算機年月Ｒ REDEFINES 計算機年月.
006510         07 計算機年                   PIC 9(2).
006520         07 計算機月                   PIC 9(2).
006530       05 計算機日                     PIC 9(2).
006540*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
006550******************************************************************
006560*                          連結項目                              *
006570******************************************************************
006580*
006590**********************
006600* メッセージ表示キー *
006610**********************
006620 01 連メ－キー IS EXTERNAL.
006630    03  連メ－メッセージ                 PIC N(20).
006640*
006650****************
006660* 連結項目待避 *
006670****************
006750************
006760* 印刷キー *
006770************
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
      */
       01 連印－対象データ追加 IS EXTERNAL.
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
      */
       01 連印－対象データ追加４ IS EXTERNAL.
     */所見、同意医師、備考 /160309
          03 連印－所見印刷区分                 PIC 9(1).
          03 連印－同意印刷区分                 PIC 9(1).
          03 連印－備考印刷区分                 PIC 9(1).
007660*
       01 連入－表示フラグ６６０ IS EXTERNAL.
          03 連入－プレビュー区分               PIC 9(1).
007460*
007470* 負担率取得用14/10～
007480 01 連率－負担率取得キー IS EXTERNAL.
007490    03 連率－施術和暦年月.
007500       05 連率－施術和暦               PIC 9.
007510       05 連率－施術年月.
007520          07 連率－施術年              PIC 9(2).
007530          07 連率－施術月              PIC 9(2).
007540    03 連率－患者コード.
007550       05 連率－患者番号               PIC 9(6).
007560       05 連率－枝番                   PIC X.
007570    03 連率－実際負担率                PIC 9(3).
007580    03 連率－実際本体負担率            PIC 9(3).
007590    03 連率－健保負担率                PIC 9(3).
007600    03 連率－２７老負担率              PIC 9(3).
007610    03 連率－助成負担率                PIC 9(3).
007620    03 連率－特別用負担率              PIC 9(3).
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
007630*
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
007640******************************************************************
007650*                      PROCEDURE  DIVISION                       *
007660******************************************************************
007670 PROCEDURE               DIVISION.
007680************
007690*           *
007700* 初期処理   *
007710*           *
007720************
002570     PERFORM プリンタファイル作成.
007730     PERFORM 初期化.
007740************
007750*           *
007760* 主処理     *
007770*           *
007780************
007790* 印刷
007800     PERFORM 連結項目待避.
007810     PERFORM 印刷セット
007820     PERFORM 印刷処理
007830************
007840*           *
007850* 終了処理   *
007860*           *
007870************
007880     PERFORM 終了処理.
007890     EXIT PROGRAM.
007900*
007910*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YCH662"              TO Ｈ連ＰＲＴＦ－帳票プログラム名.
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
007920*================================================================*
007930 初期化 SECTION.
007940*
007950     PERFORM ファイルオープン.
007960*    /* 現在日付取得 */
007970     ACCEPT 計算機日付 FROM DATE.
007980*    /* 1980～2079年の間で設定 */
007990     IF 計算機年 > 80
008000         MOVE 19 TO 計算機世紀
008010     ELSE
008020         MOVE 20 TO 計算機世紀
008030     END-IF.
008040     PERFORM カレント元号取得.
008050     PERFORM 和暦終了年取得.
008060     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
008070*================================================================*
008080 カレント元号取得 SECTION.
008090*
008100     MOVE ZEROS TO 制－制御区分.
008110     READ 制御情報マスタ
008120     NOT INVALID KEY
008130         MOVE 制－カレント元号 TO カレント元号Ｗ
008140         MOVE 制－カルテ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
008150     END-READ.
008290*
008300     MOVE 01 TO 制－制御区分.
008310     READ 制御情報マスタ
008320     NOT INVALID KEY
               MOVE 制０１－用紙２   TO 用紙Ｗ
008360     END-READ.
008160*
008170*================================================================*
008180 和暦終了年取得 SECTION.
008190*
008200     MOVE カレント元号Ｗ TO 元－元号区分.
008210     READ 元号マスタ
008220     INVALID KEY
008230         DISPLAY NC"指定和暦が登録されていません" UPON CONS
008240         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008250                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
008260         ACCEPT  キー入力 FROM CONS
008270         PERFORM 終了処理
008280         EXIT PROGRAM
008290     NOT INVALID KEY
008300         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
008310         MOVE 前和暦Ｗ TO 元－元号区分
008320         READ 元号マスタ
008330         INVALID KEY
008340             DISPLAY NC"指定和暦が登録されていません" UPON CONS
008350             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008360                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
008370             ACCEPT  キー入力 FROM CONS
008380             PERFORM 終了処理
008390             EXIT PROGRAM
008400         NOT INVALID KEY
008410             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
008420         END-READ
008430     END-READ.
008440*
008450*================================================================*
008460 連結項目待避 SECTION.
008470*
008480     INITIALIZE 対象データＷＲ.
008490     MOVE 連印－施術和暦      TO 施術和暦ＷＲ.
008500     MOVE 連印－施術年        TO 施術年ＷＲ.
008510     MOVE 連印－施術月        TO 施術月ＷＲ.
008520     MOVE 連印－保険種別      TO 保険種別ＷＲ.
008530     MOVE 連印－保険者番号    TO 保険者番号ＷＲ.
008540     MOVE 連印－本人家族区分  TO 本人家族区分ＷＲ.
008550     MOVE 連印－被保険者カナ  TO 被保険者カナＷＲ.
008560     MOVE 連印－患者番号      TO 患者番号ＷＲ.
008570     MOVE 連印－枝番          TO 枝番ＷＲ.
008580     MOVE 連印－印刷モードＦ  TO 印刷モードＦＷＲ.
008590     MOVE 連印－提出年        TO 提出年ＷＲ.
008600     MOVE 連印－提出月        TO 提出月ＷＲ.
008610     MOVE 連印－提出日        TO 提出日ＷＲ.
008620     MOVE 連印－段数          TO 段数ＷＲ.
008630*================================================================*
008640 ファイルオープン SECTION.
008650*
008660     OPEN INPUT   保険者マスタ
008670         MOVE NC"保険者" TO ファイル名.
008680         PERFORM オープンチェック.
008690     OPEN INPUT   元号マスタ
008700         MOVE NC"元号" TO ファイル名.
008710         PERFORM オープンチェック.
008720     OPEN INPUT   名称マスタ
008730         MOVE NC"名称" TO ファイル名.
008740         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
008780     OPEN INPUT   制御情報マスタ
008790         MOVE NC"制御情報" TO ファイル名.
008800         PERFORM オープンチェック.
008840     OPEN INPUT   請求先マスタ
008850         MOVE NC"請先" TO ファイル名.
008860         PERFORM オープンチェック.
008900     OPEN INPUT   受診者情報Ｆ.
008910         MOVE NC"受情" TO ファイル名.
008920         PERFORM オープンチェック.
008930     OPEN INPUT   施術記録Ｆ.
008940         MOVE NC"施記Ｆ" TO ファイル名.
008950         PERFORM オープンチェック.
008960     OPEN INPUT   負傷データＦ.
008970         MOVE NC"負傷" TO ファイル名.
008980         PERFORM オープンチェック.
008990     OPEN INPUT   負担率マスタ.
009000         MOVE NC"負率" TO ファイル名.
009010         PERFORM オープンチェック.
009020     OPEN INPUT   負傷原因Ｆ.
009030         MOVE NC"負傷原因" TO ファイル名.
009040         PERFORM オープンチェック.
009050     OPEN INPUT 市町村マスタ.
009060         MOVE NC"市町村" TO ファイル名.
009070         PERFORM オープンチェック.
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
009320     OPEN INPUT カルテファイル.
009321         MOVE NC"カルテファイル" TO ファイル名.
009322         PERFORM オープンチェック.
009120     OPEN I-O   印刷ファイル
009130         MOVE NC"印刷" TO ファイル名.
009140         PERFORM エラー処理Ｐ.
009150*================================================================*
009160 オープンチェック SECTION.
009170*
009180     IF 状態キー  NOT =  "00"
009190         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
009200         DISPLAY NC"状態キー：" 状態キー         UPON CONS
009210         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
009220                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
009230         ACCEPT  キー入力 FROM CONS
009240         PERFORM ファイル閉鎖
009250         EXIT PROGRAM.
009260*================================================================*
009270 ファイル閉鎖 SECTION.
009280*
009290     CLOSE 印刷ファイル   保険者マスタ 元号マスタ
009300           制御情報マスタ 請求先マスタ 名称マスタ
009310           受診者情報Ｆ   レセプトＦ   施術記録Ｆ
009320           負傷データＦ   負担率マスタ 負傷原因Ｆ
009330           市町村マスタ   自賠責情報Ｆ 保険会社マスタ
                 カルテファイル.
009340*================================================================*
009350 終了処理 SECTION.
009360*
009370     PERFORM ファイル閉鎖.
009380*================================================================*
009390 印刷セット SECTION.
009400*
009410     EVALUATE 印刷モードＦＷＲ
009420     WHEN 0
009430         PERFORM 印刷セット１
009440     WHEN 1
009450         PERFORM 印刷セット２
009460     WHEN 2
009470         PERFORM 印刷セット３
009480     WHEN 3
009490         PERFORM 印刷セット４
009500*/詳細モード用/0403
009510     WHEN 4
009520         PERFORM 印刷セット５
009530     END-EVALUATE.
009540*
009550*================================================================*
009560 印刷セット１ SECTION.
009570*
009580     INITIALIZE YCH662P.
009590     INITIALIZE 受診者情報Ｗ.
009600     INITIALIZE 事業所情報Ｗ.
009610     INITIALIZE 請求先情報Ｗ.
009620     INITIALIZE 負傷情報Ｗ.
009630     INITIALIZE 負担率チェックＷ.
009640     PERFORM 負担率取得.
009650     PERFORM 請求先情報取得.
009660     PERFORM 受診者情報取得.
009670     PERFORM 負傷データ取得.
009861     PERFORM カルテコメント取得.
009680*
009690     IF 負傷原因印刷区分Ｗ  NOT = 1 
009700        PERFORM 負傷原因取得
009710     END-IF.
009720*
009730     PERFORM 印刷上部セット.
009740     PERFORM 印刷下部セット.
009750*================================================================*
009760 印刷セット２ SECTION.
009770*
009780     INITIALIZE YCH662P.
009790     INITIALIZE 受診者情報Ｗ.
009800     INITIALIZE 事業所情報Ｗ.
009810     INITIALIZE 請求先情報Ｗ.
009820     INITIALIZE 負傷情報Ｗ.
009830     INITIALIZE 負担率チェックＷ.
009840     PERFORM 負担率取得.
009850     PERFORM 請求先情報取得.
009860     PERFORM 受診者情報取得.
009870     PERFORM 印刷上部セット.
009880*================================================================*
009890 印刷セット３ SECTION.
009900*
009910     INITIALIZE YCH662P.
009920     INITIALIZE 受診者情報Ｗ.
009930     INITIALIZE 事業所情報Ｗ.
009940     INITIALIZE 請求先情報Ｗ.
009950     INITIALIZE 負傷情報Ｗ.
009660     PERFORM 受診者情報取得.
009960     PERFORM 負傷データ取得.
009861     PERFORM カルテコメント取得.
009970*
009980     IF 負傷原因印刷区分Ｗ  NOT = 1 
009990        PERFORM 負傷原因取得
010000     END-IF.
010010*
010020     PERFORM 印刷下部セット.
010030*================================================================*
010040 印刷セット４ SECTION.
010050*
010060     INITIALIZE YCH662P.
010070     INITIALIZE 受診者情報Ｗ.
010080     INITIALIZE 事業所情報Ｗ.
010090     INITIALIZE 請求先情報Ｗ.
010100     INITIALIZE 負傷情報Ｗ.
010110     INITIALIZE 合計行Ｗ.
010090     PERFORM 受診者情報取得.
010150     PERFORM 請求期間取得.
010160     PERFORM 合計金額行セット.
025820*================================================================*
025830 印刷セット５ SECTION.
025840*
025850*/詳細モード用
025860     INITIALIZE YCH662P.
025870*     MOVE "X" TO EDIT-MODE OF 患者コード.
025880     INITIALIZE 受診者情報Ｗ.
025890     INITIALIZE 事業所情報Ｗ.
025900     INITIALIZE 請求先情報Ｗ.
025910     INITIALIZE 負傷情報Ｗ.
025920     INITIALIZE 負担率チェックＷ.
025930     PERFORM 負担率取得.
025940     PERFORM 請求先情報取得.
025950     PERFORM 受診者情報取得.
025960     PERFORM 負傷データ取得.
009861     PERFORM カルテコメント取得.
025970*
025980*/再生紙は１つの枠になっている為負傷原因は印刷出来ない。/0403
025980*/負傷原因を指定行から印刷。/1010
025990     IF 負傷原因印刷区分Ｗ  NOT = 1
026000        PERFORM 詳細モード負傷原因処理
026010        PERFORM 負傷原因取得
026020     END-IF.
           PERFORM 原因行セット.
026030*
026040     IF 連印－保険証印刷区分 = 1
026050         PERFORM 印刷上部セット
026060     END-IF.
026070     PERFORM 印刷下部セット詳細.
026080*/再生紙の金額行はカルテ裏。/0403
026090*     PERFORM 印刷金額セット詳細.
026040     IF 連印－所見印刷区分 = 1
              PERFORM VARYING カウンタ FROM 1 BY 1
                        UNTIL カウンタ > 10
                 MOVE 所見ＷＰ(カウンタ)   TO 所見(カウンタ)
              END-PERFORM
026060     END-IF.
026040     IF 連印－同意印刷区分 = 1
              MOVE 同意医師名Ｗ        TO 同意医師名
              IF (同意年Ｗ NOT = ZERO) OR (同意月Ｗ NOT = ZERO) OR (同意日Ｗ NOT = ZERO)
                 MOVE 同意和暦年月日Ｗ    TO 同意年月日
              END-IF
026060     END-IF.
026040     IF 連印－備考印刷区分 = 1
              PERFORM VARYING カウンタ FROM 1 BY 1
                        UNTIL カウンタ > 5
                 MOVE 備考ＷＰ(カウンタ)   TO 備考(カウンタ)
              END-PERFORM
026060     END-IF.
010170*================================================================*
010180 印刷上部セット SECTION.
010190*
010200     MOVE 患者番号Ｗ                 TO 患者番号.
010210     MOVE 枝番Ｗ                     TO 枝番.
010220**************************
010230* 被保険者証情報セット   *
010240**************************
010250     IF 記号Ｗ(1:2) = "＊" 
010260        MOVE  SPACE    TO  記号
010270     ELSE
010280        MOVE 記号Ｗ    TO  記号
010290     END-IF.
010300     IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
010310        ( 印刷番号Ｗ(1:2) = "＊" )
010320        MOVE  SPACE      TO  番号
010330     ELSE
010340        MOVE 印刷番号Ｗ  TO  番号
010350     END-IF.
010360************************
010370* 被保険者情報セット   *
010380************************
010390     MOVE 被保険者氏名Ｗ             TO 被保険者氏名.
010400     MOVE 被保険者カナＷ             TO 被保険者カナ.
010420     MOVE 資格取得年Ｗ               TO 資格取得年.
010430     MOVE 資格取得月Ｗ               TO 資格取得月.
010440     MOVE 資格取得日Ｗ               TO 資格取得日.
           IF 用紙Ｗ = ZERO
010450        MOVE 資格昭和チェックＷ      TO 資格昭和チェック
010460        MOVE 資格平成チェックＷ      TO 資格平成チェック
              IF (資格令和チェックＷ NOT = SPACE)
                 MOVE NC"令和"             TO 資格令和ＣＭ
                 MOVE 資格令和チェックＷ   TO 資格令和チェック
              END-IF
           ELSE
010510        IF ( 資格取得年Ｗ = ZERO ) AND
010520           ( 資格取得月Ｗ = ZERO ) AND
010530           ( 資格取得日Ｗ = ZERO )
                  MOVE SPACE               TO 資格元号
              ELSE
010410            MOVE 資格取得元号Ｗ      TO 資格元号
              END-IF
           END-IF.
010470*/有効期限
010480     MOVE 有効期限年Ｗ               TO 有効年.
010490     MOVE 有効期限月Ｗ               TO 有効月.
010500     MOVE 有効期限日Ｗ               TO 有効日.
010510     IF ( 有効期限年Ｗ = ZERO ) AND
010520        ( 有効期限月Ｗ = ZERO ) AND
010530        ( 有効期限日Ｗ = ZERO )
010540         MOVE SPACE          TO 有効元号
010550         MOVE SPACE          TO 有効年月日表示
010560         MOVE SPACE          TO 有効年表示
010570         MOVE SPACE          TO 有効月表示
010580         MOVE SPACE          TO 有効日表示
010590     ELSE
010600         MOVE 有効期限元号Ｗ TO 有効元号
010610         MOVE NC"有効年月日" TO 有効年月日表示
010620         MOVE NC"年"         TO 有効年表示
010630         MOVE NC"月"         TO 有効月表示
010640         MOVE NC"日"         TO 有効日表示
010650     END-IF.
010660     MOVE 被保険者明治チェックＷ     TO 被保険者明治チェック.
010670     MOVE 被保険者大正チェックＷ     TO 被保険者大正チェック.
010680     MOVE 被保険者昭和チェックＷ     TO 被保険者昭和チェック.
010690     MOVE 被保険者平成チェックＷ     TO 被保険者平成チェック.
           IF 被保険者令和チェックＷ NOT = SPACE
              IF 用紙Ｗ = ZERO
                 MOVE NC"令和"             TO 被保険者令和ＣＭ
              END-IF
              MOVE 被保険者令和チェックＷ  TO 被保険者令和チェック
           END-IF.
010700     MOVE 被保険者年Ｗ               TO 被保険者年.
010710     MOVE 被保険者月Ｗ               TO 被保険者月.
010720     MOVE 被保険者日Ｗ               TO 被保険者日.
010730     MOVE 被保険者年齢Ｗ             TO 被保険者年齢.
010740********************
010750* 患者情報セット   *
010760********************
010770     MOVE ０割チェックＷ             TO ０割チェック.
010780     MOVE １割チェックＷ             TO １割チェック.
010790     MOVE ２割チェックＷ             TO ２割チェック.
010800     MOVE ３割チェックＷ             TO ３割チェック.
010810*
010820* 14/10～ (助成の負担率）
010830     MOVE 助成負担率表示Ｗ           TO 助成負担率.
010840*
010850     IF  印刷市町村番号Ｗ(1:2) = "99"
010860         MOVE SPACE                  TO 市町村番号
010870     ELSE
010880         MOVE 印刷市町村番号Ｗ       TO 市町村番号
010890     END-IF.
010900     IF (印刷受益者番号Ｗ(1:1) = "*") OR
010910        (印刷受益者番号Ｗ(1:2) = "＊") 
010920         MOVE SPACE                  TO 受益者番号
010930     ELSE
010940         MOVE 印刷受益者番号Ｗ       TO 受益者番号
010950     END-IF
010960*
010970     IF  印刷費用負担者番号Ｗ(1:2) = "99"
010980         MOVE SPACE                  TO 公費負担者番号
010990     ELSE
011000         MOVE 印刷費用負担者番号Ｗ   TO 公費負担者番号
011010     END-IF.
011020*
011030     IF (印刷受益者番号助成Ｗ(1:1) = "*") OR
011040        (印刷受益者番号助成Ｗ(1:2) = "＊")
011050         MOVE SPACE                  TO 受益者番号助成
011060     ELSE
      */1410
               IF 印刷受益者番号助成Ｗ(11:1) = SPACE
011450            MOVE 印刷受益者番号助成Ｗ TO 受益者番号助成
               ELSE
011450            MOVE 印刷受益者番号助成Ｗ TO 受益者番号助成２
               END-IF
011080     END-IF.
011090     MOVE 患者カナＷ                 TO 患者カナ.
011100     MOVE 患者氏名Ｗ                 TO 患者氏名.
011110     MOVE 被保険者男チェックＷ       TO 被保険者男チェック.
011120     MOVE 被保険者女チェックＷ       TO 被保険者女チェック.
011130     MOVE 被保険者郵便番号１Ｗ       TO 被保険者郵便番号１.
011140     MOVE 被保険者郵便番号２Ｗ       TO 被保険者郵便番号２.
011150     MOVE "-"                        TO 被保険者郵便区切.
011160     MOVE 被保険者住所Ｗ             TO 被保険者住所.
011170     MOVE 被保険者電話番号Ｗ         TO 被保険者電話番号.
011180     MOVE 患者男チェックＷ           TO 患者男チェック.
011190     MOVE 患者女チェックＷ           TO 患者女チェック.
011200     MOVE 明治チェックＷ             TO 明治チェック.
011210     MOVE 大正チェックＷ             TO 大正チェック.
011220     MOVE 昭和チェックＷ             TO 昭和チェック.
011230     MOVE 平成チェックＷ             TO 平成チェック.
           IF 令和チェックＷ NOT = SPACE
              IF 用紙Ｗ = ZERO
                 MOVE NC"令和"             TO 令和ＣＭ
              END-IF
              MOVE 令和チェックＷ          TO 令和チェック
           END-IF.
011240     MOVE 患者年Ｗ                   TO 患者年.
011250     MOVE 患者月Ｗ                   TO 患者月.
011260     MOVE 患者日Ｗ                   TO 患者日.
011270     MOVE 患者年齢Ｗ                 TO 患者年齢.
011280     MOVE 印刷続柄Ｗ                 TO 続柄.
011290     MOVE 患者郵便番号１Ｗ           TO 患者郵便番号１.
011300     MOVE 患者郵便番号２Ｗ           TO 患者郵便番号２.
011310     MOVE "-"                        TO 患者郵便区切.
011320     MOVE 患者住所Ｗ                 TO 患者住所.
011330     MOVE 患者電話番号Ｗ             TO 患者電話番号.
011340*     MOVE 患者住所１Ｗ               TO 患者住所１.
011350*     MOVE 患者住所２Ｗ               TO 患者住所２.
011360********************
011370* 事業所情報セット *
011380********************
011390     MOVE 事業所名称Ｗ               TO 事業所名称.
011400     MOVE 事業所郵便番号１Ｗ         TO 事業所郵便番号１.
011410     MOVE 事業所郵便番号２Ｗ         TO 事業所郵便番号２.
011420     MOVE 印刷事業所住所Ｗ           TO 事業所住所.
011420     MOVE 印刷事業所住所２Ｗ         TO 事業所住所２.
011430************************************
011440* 都道府県・健康保険組合情報セット *
011450************************************
011460*労災、自費の時は保険者情報は印字しない
011470     IF 保険種別ＷＲ = 70 OR 90
011480         MOVE SPACE                  TO 保険者番号
011490         MOVE SPACE                  TO 請求先名支部名
011500     ELSE
011510         MOVE 印刷保険者番号Ｗ       TO 保険者番号
011520         MOVE 印刷請求先名支部名Ｗ   TO 請求先名支部名
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
011650     MOVE 乳幼児チェックＷ           TO 乳幼児チェック.
011660     MOVE 自賠チェックＷ             TO 自賠チェック.
011670     MOVE 生保チェックＷ             TO 生保チェック.
011680     MOVE 福チェックＷ               TO 福チェック.
011690     MOVE 身障チェックＷ             TO 身障チェック.
011700     MOVE 母子チェックＷ             TO 母子チェック.
011710     MOVE 生保チェックＷ             TO 生保チェック.
011720     MOVE 原爆チェックＷ             TO 原爆チェック.
011730*================================================================*
011740 印刷下部セット SECTION.
011750*
011760********************
011770* 負傷データセット *
011780********************
011790* １部位 *
011800**********
011810     MOVE 会部位コードＷ(1)   TO 負傷コード１.
011820     MOVE 負傷名Ｗ(1)         TO 負傷名１.
011830     MOVE 負傷年Ｗ(1)         TO 負傷年１.
011840     MOVE 負傷月Ｗ(1)         TO 負傷月１.
011850     MOVE 負傷日Ｗ(1)         TO 負傷日１.
011860     MOVE 施術開始年Ｗ(1)     TO 施術開始年１.
011870     MOVE 施術開始月Ｗ(1)     TO 施術開始月１.
011880     MOVE 施術開始日Ｗ(1)     TO 施術開始日１.
011890     MOVE 施術終了年Ｗ(1)     TO 施術終了年１.
011900     MOVE 施術終了月Ｗ(1)     TO 施術終了月１.
011910     MOVE 施術終了日Ｗ(1)     TO 施術終了日１.
011920*     MOVE 負傷年月日区切Ｗ(1) TO 区切１１ 区切１２.
011930*     MOVE 施開年月日区切Ｗ(1) TO 区切１３ 区切１４.
011940*     MOVE 施終年月日区切Ｗ(1) TO 区切１５ 区切１６.
011950*     MOVE 転帰Ｗ(1)           TO 転帰１.
011960     MOVE 治癒チェックＷ(1)   TO 治癒チェック１.
011970     MOVE 中止チェックＷ(1)   TO 中止チェック１.
011980     MOVE 転医チェックＷ(1)   TO 転医チェック１.
011990*     ******************
012000*     * 日数は一時保留 *
012010*     ******************
012020*     **********************
012030*     * 施術回数は一時保留 *
012040*     **********************
012050*     **********************
012060*     * 転帰名称は一時保留 *
012070*     **********************
012080**********
012090* ２部位 *
012100**********
012110     MOVE 会部位コードＷ(2)   TO 負傷コード２.
012120     MOVE 負傷名Ｗ(2)         TO 負傷名２.
012130     MOVE 負傷年Ｗ(2)         TO 負傷年２.
012140     MOVE 負傷月Ｗ(2)         TO 負傷月２.
012150     MOVE 負傷日Ｗ(2)         TO 負傷日２.
012160     MOVE 施術開始年Ｗ(2)     TO 施術開始年２.
012170     MOVE 施術開始月Ｗ(2)     TO 施術開始月２.
012180     MOVE 施術開始日Ｗ(2)     TO 施術開始日２.
012190     MOVE 施術終了年Ｗ(2)     TO 施術終了年２.
012200     MOVE 施術終了月Ｗ(2)     TO 施術終了月２.
012210     MOVE 施術終了日Ｗ(2)     TO 施術終了日２.
012220*     MOVE 負傷年月日区切Ｗ(2) TO 区切２１ 区切２２.
012230*     MOVE 施開年月日区切Ｗ(2) TO 区切２３ 区切２４.
012240*     MOVE 施終年月日区切Ｗ(2) TO 区切２５ 区切２６.
012250*     MOVE 転帰Ｗ(2)           TO 転帰２.
012260     MOVE 治癒チェックＷ(2)   TO 治癒チェック２.
012270     MOVE 中止チェックＷ(2)   TO 中止チェック２.
012280     MOVE 転医チェックＷ(2)   TO 転医チェック２.
012290*     ******************
012300*     * 日数は一時保留 *
012310*     ******************
012320*     **********************
012330*     * 施術回数は一時保留 *
012340*     **********************
012350*     **********************
012360*     * 転帰名称は一時保留 *
012370*     **********************
012380**********
012390* ３部位 *
012400**********
012410     MOVE 会部位コードＷ(3)   TO 負傷コード３.
012420     MOVE 負傷名Ｗ(3)         TO 負傷名３.
012430     MOVE 負傷年Ｗ(3)         TO 負傷年３.
012440     MOVE 負傷月Ｗ(3)         TO 負傷月３.
012450     MOVE 負傷日Ｗ(3)         TO 負傷日３.
012460     MOVE 施術開始年Ｗ(3)     TO 施術開始年３.
012470     MOVE 施術開始月Ｗ(3)     TO 施術開始月３.
012480     MOVE 施術開始日Ｗ(3)     TO 施術開始日３.
012490     MOVE 施術終了年Ｗ(3)     TO 施術終了年３.
012500     MOVE 施術終了月Ｗ(3)     TO 施術終了月３.
012510     MOVE 施術終了日Ｗ(3)     TO 施術終了日３.
012520*     MOVE 負傷年月日区切Ｗ(3) TO 区切３１ 区切３２.
012530*     MOVE 施開年月日区切Ｗ(3) TO 区切３３ 区切３４.
012540*     MOVE 施終年月日区切Ｗ(3) TO 区切３５ 区切３６.
012550*     MOVE 転帰Ｗ(3)           TO 転帰３.
012560     MOVE 治癒チェックＷ(3)   TO 治癒チェック３.
012570     MOVE 中止チェックＷ(3)   TO 中止チェック３.
012580     MOVE 転医チェックＷ(3)   TO 転医チェック３.
012590*     ******************
012600*     * 日数は一時保留 *
012610*     ******************
012620*     **********************
012630*     * 施術回数は一時保留 *
012640*     **********************
012650*     **********************
012660*     * 転帰名称は一時保留 *
012670*     **********************
012680**********
012690* ４部位 *
012700**********
012710     MOVE 会部位コードＷ(4)   TO 負傷コード４.
012720     MOVE 負傷名Ｗ(4)         TO 負傷名４.
012730     MOVE 負傷年Ｗ(4)         TO 負傷年４.
012740     MOVE 負傷月Ｗ(4)         TO 負傷月４.
012750     MOVE 負傷日Ｗ(4)         TO 負傷日４.
012760     MOVE 施術開始年Ｗ(4)     TO 施術開始年４.
012770     MOVE 施術開始月Ｗ(4)     TO 施術開始月４.
012780     MOVE 施術開始日Ｗ(4)     TO 施術開始日４.
012790     MOVE 施術終了年Ｗ(4)     TO 施術終了年４.
012800     MOVE 施術終了月Ｗ(4)     TO 施術終了月４.
012810     MOVE 施術終了日Ｗ(4)     TO 施術終了日４.
012820*     MOVE 負傷年月日区切Ｗ(4) TO 区切４１ 区切４２.
012830*     MOVE 施開年月日区切Ｗ(4) TO 区切４３ 区切４４.
012840*     MOVE 施終年月日区切Ｗ(4) TO 区切４５ 区切４６.
012850*     MOVE 転帰Ｗ(4)           TO 転帰４.
012860     MOVE 治癒チェックＷ(4)   TO 治癒チェック４.
012870     MOVE 中止チェックＷ(4)   TO 中止チェック４.
012880     MOVE 転医チェックＷ(4)   TO 転医チェック４.
012890*     ******************
012900*     * 日数は一時保留 *
012910*     ******************
012920*     **********************
012930*     * 施術回数は一時保留 *
012940*     **********************
012950*     **********************
012960*     * 転帰名称は一時保留 *
012970*     **********************
012980**********
012990* ５部位 *
013000**********
013010     MOVE 会部位コードＷ(5)   TO 負傷コード５.
013020     MOVE 負傷名Ｗ(5)         TO 負傷名５.
013030     MOVE 負傷年Ｗ(5)         TO 負傷年５.
013040     MOVE 負傷月Ｗ(5)         TO 負傷月５.
013050     MOVE 負傷日Ｗ(5)         TO 負傷日５.
013060     MOVE 施術開始年Ｗ(5)     TO 施術開始年５.
013070     MOVE 施術開始月Ｗ(5)     TO 施術開始月５.
013080     MOVE 施術開始日Ｗ(5)     TO 施術開始日５.
013090     MOVE 施術終了年Ｗ(5)     TO 施術終了年５.
013100     MOVE 施術終了月Ｗ(5)     TO 施術終了月５.
013110     MOVE 施術終了日Ｗ(5)     TO 施術終了日５.
013120*     MOVE 負傷年月日区切Ｗ(5) TO 区切５１ 区切５２.
013130*     MOVE 施開年月日区切Ｗ(5) TO 区切５３ 区切５４.
013140*     MOVE 施終年月日区切Ｗ(5) TO 区切５５ 区切５６.
013150*     MOVE 転帰Ｗ(5)           TO 転帰５.
013160     MOVE 治癒チェックＷ(5)   TO 治癒チェック５.
013170     MOVE 中止チェックＷ(5)   TO 中止チェック５.
013180     MOVE 転医チェックＷ(5)   TO 転医チェック５.
013190*     ******************
013200*     * 日数は一時保留 *
013210*     ******************
013220*     **********************
013230*     * 施術回数は一時保留 *
013240*     **********************
013250*     **********************
013260*     * 転帰名称は一時保留 *
013270*     **********************
013280************
013290* 負傷原因 *
013300************
013310     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
013320     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
013330     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
013340     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
013350     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
013360     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
013370     MOVE 負傷原因Ｗ(7)       TO 負傷原因７.
013380     MOVE 負傷原因Ｗ(8)       TO 負傷原因８.
013390     MOVE 負傷原因Ｗ(9)       TO 負傷原因９.
013400     MOVE 負傷原因Ｗ(10)      TO 負傷原因１０.
013410     MOVE 負傷原因Ｗ(11)      TO 負傷原因１１.
013420     MOVE 負傷原因Ｗ(12)      TO 負傷原因１２.
013430     MOVE 負傷原因Ｗ(13)      TO 負傷原因１３.
013440     MOVE 負傷原因Ｗ(14)      TO 負傷原因１４.
013450     MOVE 負傷原因Ｗ(15)      TO 負傷原因１５.
013280**************
013290* 初検の所見 *
013300**************
           PERFORM VARYING カウンタ FROM 1 BY 1
                     UNTIL カウンタ > 10
              MOVE 所見ＷＰ(カウンタ)   TO 所見(カウンタ)
           END-PERFORM.
013280********
013290* 同意 *
013300********
           MOVE 同意医師名Ｗ        TO 同意医師名.
           IF (同意年Ｗ NOT = ZERO) OR (同意月Ｗ NOT = ZERO) OR (同意日Ｗ NOT = ZERO)
              MOVE 同意和暦年月日Ｗ TO 同意年月日
           END-IF.
013280********
013290* 備考 *
013300********
           PERFORM VARYING カウンタ FROM 1 BY 1
                     UNTIL カウンタ > 5
              MOVE 備考ＷＰ(カウンタ)   TO 備考(カウンタ)
           END-PERFORM.
013460*
013470*================================================================*
013480 負担率取得 SECTION.
013490*
013500     MOVE ZERO TO 本人負担率Ｗ.
013510     MOVE ZERO TO 家族負担率Ｗ.
013520*
013530     MOVE 施術和暦ＷＲ   TO 受－施術和暦.
013540     MOVE 施術年ＷＲ     TO 受－施術年.
013550     MOVE 施術月ＷＲ     TO 受－施術月.
013560     MOVE 患者番号ＷＲ   TO 受－患者番号.
013570     MOVE 枝番ＷＲ       TO 受－枝番.
013580     READ 受診者情報Ｆ
013590     INVALID KEY
013600         MOVE  NC"施術月の受診者情報がありません" TO 連メ－メッセージ
013610         CALL   "MSG001"
013620         CANCEL "MSG001"
013630         PERFORM ファイル閉鎖
013640         MOVE ZERO TO PROGRAM-STATUS
013650         EXIT PROGRAM
013660     NOT INVALID KEY
013670* 14/10～　サブルーチン処理
013680      IF 受－施術和暦年月 >= 41410
013690         PERFORM 負担率取得１４１０
014870       END-IF
014880     END-READ.
014890*
014900**
014910     EVALUATE 負担率Ｗ
014920     WHEN 0
014930         MOVE NC"○" TO ０割チェックＷ
014940     WHEN 10
014950         MOVE NC"○" TO １割チェックＷ
014960     WHEN 20
014970         MOVE NC"○" TO ２割チェックＷ
014980     WHEN 30
014990         MOVE NC"○" TO ３割チェックＷ
015000     END-EVALUATE.
015010*
015020*================================================================*
015030 負担率取得１４１０ SECTION.
015040*
015050* 受診者Ｆ READ中
015060* 平成14/10～
015070     MOVE ZERO  TO 負担率Ｗ.
015080     INITIALIZE 助成負担率ＷＷ.
015090*
015100     MOVE SPACE TO 連率－負担率取得キー.
015110     INITIALIZE 連率－負担率取得キー.
015120     MOVE 受－施術和暦年月 TO 連率－施術和暦年月.
015130     MOVE 受－患者コード   TO 連率－患者コード.
015140*
015150     CALL   "HUTANRIT".
015160     CANCEL "HUTANRIT".
015170*
015180* / 本体 /
015190     MOVE 連率－実際本体負担率 TO 負担率Ｗ.
015200*
015210*
015220* / 助成 /
015230     IF 受－助成種別 NOT = ZERO
015240        MOVE 連率－助成負担率  TO 助成負担率Ｗ
015250*
015260        COMPUTE 助成負担率Ｗ１ = 助成負担率Ｗ / 10
015270        MOVE 助成負担率Ｗ１ TO  助成負担率文字Ｗ１
015280*
015290        STRING "("                 DELIMITED BY SIZE
015300               助成負担率文字Ｗ１  DELIMITED BY SIZE
015310               "割)"               DELIMITED BY SIZE
015320               INTO 助成負担率表示Ｗ
015330        END-STRING
015340*
015350     END-IF.
015360*
015370*================================================================*
015560*================================================================*
015570 受診者情報取得 SECTION.
015580*
015590**************************************************
015600* 連結データから受診者情報Ｆより以下の情報を取得 *
015610* ● 患者番号.... 患者番号Ｗ                     *
015620* ● 枝番........ 枝番Ｗ                         *
015630* ● 記号 ....... 記号Ｗに格納                   *
015640* ● 番号 ....... 番号Ｗに格納                   *
015650* ● 被保険者カナ.被保険者カナＷに格納           *
015660* ● 被保険者氏名.被保険者氏名Ｗに格納           *
015670* ● 郵便番号１ ..郵便番号１Ｗに格納             *
015680* ● 郵便番号２ ..郵便番号２Ｗに格納             *
015690* ● 住所１ ......住所１Ｗに格納                 *
015700* ● 住所２ ......住所２Ｗに格納                 *
015710* ● 電話番号.....電話番号Ｗに格納               *
015720* ● 患者カナ ....患者カナＷに格納               *
015730* ● 患者氏名 ....患者氏名Ｗに格納               *
015740* ● 続柄 ........名称マスタより続柄Ｗに取得     *
015750* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
015760* ● 患者年 ......患者年Ｗに格納                 *
015770* ● 患者月 ......患者月Ｗに格納                 *
015780* ● 患者日 ......患者日Ｗに格納                 *
015790**************************************************
015800     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
015810     MOVE 施術年ＷＲ         TO 受－施術年.
015820     MOVE 施術月ＷＲ         TO 受－施術月.
015830     MOVE 患者コードＷＲ     TO 受－患者コード.
015840     READ 受診者情報Ｆ
015850     INVALID KEY
015860         CONTINUE
015870*            /* ありえない */
015880     NOT INVALID KEY
015890         MOVE 受－患者番号     TO 患者番号Ｗ
015900         MOVE 受－枝番         TO 枝番Ｗ
015910*         MOVE 受－記号         TO 記号Ｗ
015920*         MOVE 受－番号         TO 番号Ｗ
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
015930*         IF ( 受－記号 = NC"＊") 
015940*             MOVE  SPACE            TO 記号Ｗ
015950*         END-IF
015960*         MOVE 受－番号         TO 番号Ｗ
015970*         IF ( 番号Ｗ = "*") OR ( 番号Ｗ = "＊") 
015980*            MOVE  SPACE            TO 番号Ｗ
015990*         END-IF
016000         MOVE 受－被保険者カナ TO 被保険者カナＷ
016010         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
016020*
016030         EVALUATE 受－被保険者性別
016040         WHEN 1
016050             MOVE NC"○"  TO 被保険者男チェックＷ
016060         WHEN 2
016070             MOVE NC"○"  TO 被保険者女チェックＷ
016080         END-EVALUATE
016090         EVALUATE 受－患者性別
016100         WHEN 1
016110             MOVE NC"○"  TO 患者男チェックＷ
016120         WHEN 2
016130             MOVE NC"○"  TO 患者女チェックＷ
016140         END-EVALUATE
016150*
016160         EVALUATE 受－被保険者和暦
016170         WHEN 1
016180             MOVE NC"○"  TO 被保険者明治チェックＷ
016190         WHEN 2
016200             MOVE NC"○"  TO 被保険者大正チェックＷ
016210         WHEN 3
016220             MOVE NC"○"  TO 被保険者昭和チェックＷ
016230         WHEN 4
016240             MOVE NC"○"  TO 被保険者平成チェックＷ
016230         WHEN 5
016240             MOVE NC"○"  TO 被保険者令和チェックＷ
016250         END-EVALUATE
016260         MOVE 受－被保険者年   TO 被保険者年Ｗ
016270         MOVE 受－被保険者月   TO 被保険者月Ｗ
016280         MOVE 受－被保険者日   TO 被保険者日Ｗ
016290
016300*被保険者年齢
016310     MOVE 連印－施術和暦 TO 元－元号区分
016320     READ 元号マスタ
016330     NOT INVALID KEY
016340         COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
016350     END-READ
016360     IF ( 受－被保険者和暦 NOT = ZERO ) AND
016370        ( 受－被保険者年   NOT = ZERO ) AND
016380        ( 受－被保険者月   NOT = ZERO ) AND
016390        ( 受－被保険者日   NOT = ZERO )
016400
016410         MOVE ZERO TO 被保険者年齢Ｗ
016420*
016430         MOVE 受－被保険者和暦 TO 元－元号区分
016440         READ 元号マスタ
016450         NOT INVALID KEY
016460             COMPUTE 被保険者西暦年Ｗ = 元－開始西暦年 + ( 受－被保険者年 - 1 )
016470         END-READ
016480*
016490         COMPUTE 被保険者年齢Ｗ   = 施術西暦年Ｗ - 被保険者西暦年Ｗ
016500*
016510         IF 連印－施術月 < 受－被保険者月
016520             COMPUTE 被保険者年齢Ｗ = 被保険者年齢Ｗ - 1
016530         END-IF
016540*
016550         MOVE 被保険者年齢Ｗ     TO 被保険者年齢
016560     END-IF
016570*
016580*/郵便番号 ゼロの時はスペース
016590         IF 受－郵便番号１ = "000"
016600             MOVE SPACE          TO 被保険者郵便番号１Ｗ
016610             MOVE SPACE          TO 被保険者郵便番号２Ｗ
016620         ELSE
016630             MOVE 受－郵便番号１ TO 被保険者郵便番号１Ｗ
016640             MOVE 受－郵便番号２ TO 被保険者郵便番号２Ｗ
016650         END-IF
016660         MOVE 受－電話番号     TO 被保険者電話番号Ｗ
016670         STRING 受－住所１  DELIMITED BY SPACE
016680                受－住所２  DELIMITED BY SPACE
016690           INTO 被保険者住所Ｗ
016700         END-STRING
016710*
016720*/郵便番号 ゼロの時はスペース
016730         IF 受－患者郵便番号１ = "000"
016740             MOVE SPACE              TO 患者郵便番号１Ｗ
016750             MOVE SPACE              TO 患者郵便番号２Ｗ
016760         ELSE
016770             MOVE 受－患者郵便番号１ TO 患者郵便番号１Ｗ
016780             MOVE 受－患者郵便番号２ TO 患者郵便番号２Ｗ
016790         END-IF
016800*         MOVE 受－患者住所１     TO 患者住所１Ｗ
016810*         MOVE 受－患者住所２     TO 患者住所２Ｗ
016820         STRING 受－患者住所１  DELIMITED BY SPACE
016830                受－患者住所２  DELIMITED BY SPACE
016840           INTO 患者住所Ｗ
016850         END-STRING
016860         MOVE 受－患者電話番号   TO 患者電話番号Ｗ
016870*
016880*電話番号分割
016890*         UNSTRING 電話番号Ｗ  DELIMITED BY "-"
016900*             INTO 分割電話番号１Ｗ
016910*                  分割電話番号２Ｗ
016920*                  分割電話番号３Ｗ
016930*         END-UNSTRING
016940*電話番号右詰処理
016950*         IF 分割電話番号３Ｗ = SPACE
016960*             MOVE 分割電話番号２Ｗ TO 分割電話番号３Ｗ
016970*             MOVE 分割電話番号１Ｗ TO 分割電話番号２Ｗ
016980*             MOVE SPACE            TO 分割電話番号１Ｗ
016990*         END-IF
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
017000*
017010         MOVE 受－患者カナ     TO 患者カナＷ
017020         MOVE 受－患者氏名     TO 患者氏名Ｗ
017030*
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
017090* 続柄
017100         IF 受－本人家族区分 = 1
017110             MOVE NC"本人"     TO 続柄Ｗ
017120         ELSE
017130             MOVE 05          TO 名－区分コード
017140             MOVE 受－続柄    TO 名－名称コード
017150             READ 名称マスタ
017160             INVALID KEY
017170                 MOVE SPACE   TO 続柄Ｗ
017180             NOT INVALID KEY
017190                 MOVE 名－略称  TO 続柄Ｗ
017200             END-READ
017210         END-IF
017220* 患者和暦年月日
017230         EVALUATE 受－患者和暦
017240         WHEN 1
017250             MOVE NC"○"  TO 明治チェックＷ
017260         WHEN 2
017270             MOVE NC"○"  TO 大正チェックＷ
017280         WHEN 3
017290             MOVE NC"○"  TO 昭和チェックＷ
017300         WHEN 4
017310             MOVE NC"○"  TO 平成チェックＷ
017300         WHEN 5
017310             MOVE NC"○"  TO 令和チェックＷ
017320         END-EVALUATE
017330         MOVE 受－患者年  TO 患者年Ｗ
017340         MOVE 受－患者月  TO 患者月Ｗ
017350         MOVE 受－患者日  TO 患者日Ｗ
017360*患者年齢
017370     MOVE 連印－施術和暦 TO 元－元号区分
017380     READ 元号マスタ
017390     NOT INVALID KEY
017400         COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
017410     END-READ
017420     IF ( 受－患者和暦 NOT = ZERO ) AND
017430        ( 受－患者年   NOT = ZERO ) AND
017440        ( 受－患者月   NOT = ZERO ) AND
017450        ( 受－患者日   NOT = ZERO )
017460*
017470         MOVE ZERO TO 患者年齢Ｗ
017480*
017490         MOVE 受－患者和暦 TO 元－元号区分
017500         READ 元号マスタ
017510         NOT INVALID KEY
017520             COMPUTE 患者西暦年Ｗ = 元－開始西暦年 + ( 受－患者年 - 1 )
017530         END-READ
017540*
017550         COMPUTE 患者年齢Ｗ   = 施術西暦年Ｗ - 患者西暦年Ｗ
017560*
017570         IF 連印－施術月 < 受－患者月
017580             COMPUTE 患者年齢Ｗ = 患者年齢Ｗ - 1
017590         END-IF
017600*
017610         MOVE 患者年齢Ｗ     TO 患者年齢
017620     END-IF
017630* 患者性別
017640*         EVALUATE 受－患者性別
017650*         WHEN 1
017660*             MOVE NC"（男）" TO 患者性別Ｗ
017670*         WHEN 2
017680*             MOVE NC"（女）" TO 患者性別Ｗ
017690*         END-EVALUATE
017700*/有効期限
017710         MOVE 受－有効和暦   TO 有効期限和暦Ｗ
017720         MOVE 受－有効年     TO 有効期限年Ｗ
017730         MOVE 受－有効月     TO 有効期限月Ｗ
017740         MOVE 受－有効日     TO 有効期限日Ｗ
017750         MOVE 有効期限和暦Ｗ TO 元－元号区分
017760         READ 元号マスタ
017770         INVALID KEY
017780             MOVE SPACE        TO 有効期限元号Ｗ
017790         NOT INVALID KEY
017800             MOVE 元－元号名称 TO 有効期限元号Ｗ
017810         END-READ
017820*/資格取得年月日
017830         MOVE 受－資格和暦 TO 資格取得和暦Ｗ
017840         MOVE 受－資格年   TO 資格取得年Ｗ
017850         MOVE 受－資格月   TO 資格取得月Ｗ
017860         MOVE 受－資格日   TO 資格取得日Ｗ
017750         MOVE 資格取得和暦Ｗ TO 元－元号区分
017760         READ 元号マスタ
017770         INVALID KEY
017780             MOVE SPACE        TO 資格取得元号Ｗ
017790         NOT INVALID KEY
017800             MOVE 元－元号名称 TO 資格取得元号Ｗ
017810         END-READ
017870         IF (資格取得和暦Ｗ NOT = ZERO) AND
017880            (資格取得年Ｗ   NOT = ZERO) AND
017890            (資格取得月Ｗ   NOT = ZERO) AND
017900            (資格取得日Ｗ   NOT = ZERO)
017910             EVALUATE 資格取得和暦Ｗ
017920             WHEN 3
017930                 MOVE NC"○"  TO 資格昭和チェックＷ
017940             WHEN 4
017950                 MOVE NC"○"  TO 資格平成チェックＷ
017940             WHEN 5
017950                 MOVE NC"○"  TO 資格令和チェックＷ
017960             END-EVALUATE
017970         END-IF
017980**市町村国保､退職の場合は有効期限を出力
017990**業種別国保(国保組合)､共済､日雇､船員､社保､組合､自衛官は有効期限日を出力する
018000*         IF (( 受－保険種別 = 08 ) OR
018010*            (( 受－保険種別 = 01 ) AND
018020*            (  受－保険者番号(3:1) NOT = "3" )))
018030*             MOVE 受－有効和暦 TO 資格取得和暦Ｗ
018040*             MOVE 受－有効年   TO 資格取得年Ｗ
018050*             MOVE 受－有効月   TO 資格取得月Ｗ
018060*             MOVE 受－有効日   TO 資格取得日Ｗ
018070*         ELSE
018080*             MOVE 受－資格和暦 TO 資格取得和暦Ｗ
018090*             MOVE 受－資格年   TO 資格取得年Ｗ
018100*             MOVE 受－資格月   TO 資格取得月Ｗ
018110*             MOVE 受－資格日   TO 資格取得日Ｗ
018120*         END-IF
018130*         IF (資格取得和暦Ｗ NOT = ZERO) AND
018140*            (資格取得年Ｗ   NOT = ZERO) AND
018150*            (資格取得月Ｗ   NOT = ZERO) AND
018160*            (資格取得日Ｗ   NOT = ZERO)
018170*             EVALUATE 資格取得和暦Ｗ
018180*             WHEN 3
018190*                 MOVE NC"○"  TO 資格昭和チェックＷ
018200*             WHEN 4
018210*                 MOVE NC"○"  TO 資格平成チェックＷ
018200*             WHEN 5
018210*                 MOVE NC"○"  TO 資格令和チェックＷ
018220*             END-EVALUATE
018230*         END-IF
018240* 市町村番号 受給者番号
018250         IF 受－助成種別 NOT = ZERO
018260             MOVE 受－費用負担者番号助成 TO 費用負担者番号Ｗ
018270             MOVE 受－受益者番号助成     TO 受益者番号助成Ｗ
018280         END-IF
018290         IF ( 受－公費種別 = 05 ) AND ( 受－施術和暦年月 < 42004 )
018300             MOVE 受－費用負担者番号     TO 市町村番号Ｗ
018310             MOVE 受－受益者番号老人     TO 受益者番号Ｗ
018320         END-IF
018330*保険種別
018340         EVALUATE 受－助成種別
018350         WHEN 50
018360             MOVE NC"○"  TO 生保チェックＷ
018370         WHEN 51
018380*/頭4桁が "4113"東京 "4108"茨城 "4132"島根 の時は、「福」。それ以外は「老」
018390             IF  受－費用負担者番号助成(1:4) = "4113" OR
018400                 受－費用負担者番号助成(1:4) = "4108" OR
018410                 受－費用負担者番号助成(1:4) = "4132"
018420                 MOVE NC"○"    TO 福チェックＷ
018430             ELSE
018440                 MOVE NC"○"    TO 老人チェックＷ
018450             END-IF
018460         WHEN 52
018470             MOVE NC"○"  TO 母子チェックＷ
018480         WHEN 53
018490             MOVE NC"○"  TO 身障チェックＷ
018500         WHEN 54
018510             MOVE NC"○"  TO 原爆チェックＷ
018520         WHEN 55
018530             MOVE NC"○"  TO 乳幼児チェックＷ
018540         END-EVALUATE
018550
018560         IF 受－公費種別 = 05
018570             MOVE NC"○"  TO 老人チェックＷ
018580         ELSE
018590             EVALUATE 受－保険種別
018600             WHEN 01
018610                 MOVE NC"○"  TO 国保チェックＷ
018620             WHEN 02
018630                 MOVE NC"○"  TO 社保チェックＷ
018640             WHEN 03
018650                 MOVE NC"○"  TO 組合チェックＷ
018660             WHEN 04
018670                 MOVE NC"○"  TO 共済チェックＷ
018680             WHEN 06
018690                 MOVE NC"○"  TO 日雇チェックＷ
018700             WHEN 07
018710                 MOVE NC"○"  TO 船員チェックＷ
018720             WHEN 08
018730                 MOVE NC"○"  TO 退職チェックＷ
018740             WHEN 09
018750                 MOVE NC"○"  TO 共済チェックＷ
018760*             WHEN 70
018770*                 MOVE NC"○"  TO 労災チェックＷ
018780             WHEN 80
018790                 MOVE NC"○"  TO 自賠チェックＷ
018800             END-EVALUATE
018810         END-IF
018820     END-READ.
018830*HILO テスト－－－－－－－－－－－
018840*      MOVE NC"○"  TO 被保険者明治Ｗ.
018850*      MOVE NC"○"  TO 被保険者大正Ｗ.
018860*      MOVE NC"○"  TO 被保険者昭和Ｗ.
018870*      MOVE NC"○"  TO 被保険者男チェックＷ.
018880*      MOVE NC"○"  TO 被保険者女チェックＷ.
018890*      MOVE NC"○"  TO 患者男チェックＷ.
018900*      MOVE NC"○"  TO 患者女チェックＷ.
018910*      MOVE NC"○"  TO 明治チェックＷ.
018920*      MOVE NC"○"  TO 大正チェックＷ.
018930*      MOVE NC"○"  TO 昭和チェックＷ.
018940*      MOVE NC"○"  TO 平成チェックＷ.
018940*      MOVE NC"○"  TO 令和チェックＷ.
018950*      MOVE NC"○"  TO 社保チェックＷ    .
018960*      MOVE NC"○"  TO 組合チェックＷ    .
018970*      MOVE NC"○"  TO 共済チェックＷ    .
018980*      MOVE NC"○"  TO 国保チェックＷ    .
018990*      MOVE NC"○"  TO 日雇チェックＷ    .
019000*      MOVE NC"○"  TO 船員チェックＷ    .
019010*      MOVE NC"○"  TO 老人チェックＷ    .
019020*      MOVE NC"○"  TO 福チェックＷ    .
019030*      MOVE NC"○"  TO 身障チェックＷ    .
019040*      MOVE NC"○"  TO 退職チェックＷ    .
019050*      MOVE NC"○"  TO 母子チェックＷ    .
019060*      MOVE NC"○"  TO 乳幼児チェックＷ  .
019070*      MOVE NC"○"  TO 国保組合チェックＷ.
019080*      MOVE NC"○"  TO 労災チェックＷ    .
019090*      MOVE NC"○"  TO 生保チェックＷ    .
019100*      MOVE NC"○"  TO 自賠チェックＷ    .
019110*      MOVE NC"○"  TO 原爆チェックＷ    .
019120*      MOVE NC"○" TO ０割チェックＷ.
019130*      MOVE NC"○" TO １割チェックＷ.
019140*      MOVE NC"○" TO ２割チェックＷ.
019150*      MOVE NC"○" TO ３割チェックＷ.
019160*      MOVE NC"○"  TO 資格昭和チェックＷ.
019170*      MOVE NC"○"  TO 資格平成チェックＷ.
019170*      MOVE NC"○"  TO 資格令和チェックＷ.
019180*      MOVE NC"○"  TO 被保険者明治チェックＷ.
019190*      MOVE NC"○"  TO 被保険者大正チェックＷ.
019200*      MOVE NC"○"  TO 被保険者昭和チェックＷ.
019210*      MOVE NC"○"  TO 被保険者平成チェックＷ.
019210*      MOVE NC"○"  TO 被保険者令和チェックＷ.
019220*-------------------------------------
019230*================================================================*
019240 請求先情報取得 SECTION.
019250*
019260****************************************************
019270* 連結データから保険者マスタより請求先を取得する。 *
019280* ※保－請求先情報区分=1の場合請求先マスタを使用   *
019290* ● 保険者番号...保険者番号Ｗに格納               *
019300* ● 名称........ 請求先名称Ｗに格納               *
019310* ● 郵便番号１.. 請求先郵便番号１Ｗに格納         *
019320* ● 郵便番号２.. 請求先郵便番号２Ｗに格納         *
019330* ● 住所１.......請求先住所１Ｗに格納             *
019340* ● 住所２.......請求先住所２Ｗに格納             *
019350****************************************************
019360     MOVE SPACE          TO 保険者番号Ｗ.
019370     MOVE SPACE          TO 請求先名称Ｗ.
019380     MOVE SPACE          TO 支部名Ｗ.
019390     MOVE SPACE          TO 請求先郵便番号１Ｗ.
019400     MOVE SPACE          TO 請求先郵便番号２Ｗ.
019410     MOVE SPACE          TO 請求先住所１Ｗ.
019420     MOVE SPACE          TO 請求先住所２Ｗ.
019430     MOVE SPACE          TO 印刷保険者呼名Ｗ.
019440     MOVE SPACE          TO 請求先名支部名Ｗ
019450*
019460     MOVE 保険種別ＷＲ   TO 保－保険種別.
019470     MOVE 保険者番号ＷＲ TO 保－保険者番号.
019480     READ 保険者マスタ
019490     INVALID KEY
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
019530     NOT INVALID KEY
019540         MOVE 保－保険者番号  TO 保険者番号Ｗ
019550         MOVE 保－保険者名称  TO 請求先名称Ｗ
019560         MOVE 保－支部部署名  TO 支部名Ｗ
019570         MOVE 保－郵便番号１  TO 請求先郵便番号１Ｗ
019580         MOVE 保－郵便番号２  TO 請求先郵便番号２Ｗ
019590         MOVE 保－住所１      TO 請求先住所１Ｗ
019600         MOVE 保－住所２      TO 請求先住所２Ｗ
019610         MOVE 保－接尾語区分  TO 接尾語区分Ｗ
019620     END-READ.
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
019630     EVALUATE 保険種別ＷＲ
019640     WHEN 2
019650     WHEN 6
019660         IF 接尾語区分Ｗ = 1
019670            MOVE SPACE              TO 印刷保険者呼名Ｗ
019680         ELSE
019690            MOVE "社会保険事務所"   TO 印刷保険者呼名Ｗ
019700         END-IF
019710     WHEN 3
019720         MOVE "健康保険組合"   TO 印刷保険者呼名Ｗ
019730     WHEN OTHER
019740         MOVE SPACE TO 印刷保険者呼名Ｗ
019750     END-EVALUATE.
019760     IF 印刷保険者呼名Ｗ NOT = SPACE
019770         STRING 請求先名称Ｗ DELIMITED BY SPACE
019780                保険者呼名Ｗ DELIMITED BY SPACE
019790           INTO 請求先名称Ｗ
019800         END-STRING
019810     END-IF.
019820*組合と共済は支部名も印字する
019830     EVALUATE 保険種別ＷＲ
019840     WHEN 03
019850     WHEN 04
019860         STRING 請求先名称Ｗ DELIMITED BY SPACE
019870                "  "         DELIMITED BY SIZE
019880                支部名Ｗ     DELIMITED BY SPACE
019890           INTO 請求先名支部名Ｗ
019900         END-STRING
019910     WHEN OTHER
019920         MOVE 請求先名称Ｗ TO 請求先名支部名Ｗ
019930     END-EVALUATE.
019940*================================================================*
019950 負傷データ取得 SECTION.
019960*
019970**************************************************
019980* 連結データから負傷データＦより以下の情報を取得 *
019990* ● 負傷名...部位＋負傷種別にて加工して格納     *
020000* ● 負傷年.......負傷年Ｗ                       *
020010* ● 負傷月.......負傷月Ｗ                       *
020020* ● 負傷日.......負傷日Ｗ                       *
020030* ● 施術終了年...終了年Ｗ                       *
020040* ● 施術終了月...終了月Ｗ                       *
020050* ● 施術終了日...終了日Ｗ                       *
020060**************************************************
020070     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
020080     MOVE 施術年ＷＲ         TO 負－施術年.
020090     MOVE 施術月ＷＲ         TO 負－施術月.
020100     MOVE 患者コードＷＲ     TO 負－患者コード.
020110     READ 負傷データＦ
020120     INVALID KEY
020130         CONTINUE
020140*            /* ありえない */
020150     NOT INVALID KEY
020160         MOVE ZERO                         TO 部位数Ｗ
020170         MOVE 負－部位数                   TO 部位数Ｗ
020180         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
020190                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
020200*********************************************
020210* 注）全柔...負傷種別＋部位にて加工して格納 *
020220*********************************************
020230* 負傷種別
020240             MOVE SPACE                     TO 負傷名称Ｗ
020250             MOVE 03                        TO 名－区分コード
020260             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
020270             READ 名称マスタ
020280             INVALID KEY
020290                 MOVE SPACE    TO 負傷名称Ｗ
020300             NOT INVALID KEY
020310*                 MOVE 名－略称 TO 負傷名称Ｗ
020320                 MOVE 名－正式名称 TO 負傷名称Ｗ
020330             END-READ
020340* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
020360             PERFORM 名称埋込処理
020420             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
020430             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
020440             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
020450*             IF 負－負傷年(部位ＣＮＴ) NOT = ZERO 
020460*                MOVE "."                   TO 負傷年月日区切Ｗ(部位ＣＮＴ)
020470*             END-IF
020480*
020490             MOVE 負－開始年(部位ＣＮＴ)   TO 施術開始年Ｗ(部位ＣＮＴ)
020500             MOVE 負－開始月(部位ＣＮＴ)   TO 施術開始月Ｗ(部位ＣＮＴ)
020510             MOVE 負－開始日(部位ＣＮＴ)   TO 施術開始日Ｗ(部位ＣＮＴ)
020520*             IF 負－開始年(部位ＣＮＴ) NOT = ZERO 
020530*                MOVE "."                   TO 施開年月日区切Ｗ(部位ＣＮＴ)
020540*             END-IF
020550*
020560             MOVE 負－終了年(部位ＣＮＴ)   TO 施術終了年Ｗ(部位ＣＮＴ)
020570             MOVE 負－終了月(部位ＣＮＴ)   TO 施術終了月Ｗ(部位ＣＮＴ)
020580             MOVE 負－終了日(部位ＣＮＴ)   TO 施術終了日Ｗ(部位ＣＮＴ)
020590*             IF 負－終了年(部位ＣＮＴ) NOT = ZERO 
020600*                MOVE "."                   TO 施終年月日区切Ｗ(部位ＣＮＴ)
020610*             END-IF
020620*転帰
020630             EVALUATE 負－転帰区分(部位ＣＮＴ)
020640             WHEN 1
020650             WHEN 2
020660                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
020670             WHEN 3
020680                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
020690             WHEN 4
020700                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
020710             END-EVALUATE
020720*             IF 負－転帰区分(部位ＣＮＴ) NOT = 9
020730*                 MOVE 04                        TO 名－区分コード
020740*                 MOVE 負－転帰区分(部位ＣＮＴ)  TO 名－名称コード
020750*                 READ 名称マスタ
020760*                 INVALID KEY
020770*                     MOVE SPACE    TO 転帰Ｗ(部位ＣＮＴ)
020780*                 NOT INVALID KEY
020790*                     MOVE 名－略称 TO 転帰Ｗ(部位ＣＮＴ)
020800*                 END-READ
020810*             END-IF
020820*hilo---------------------------------
020830*         MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
020840*         MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
020850*         MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
020860*-------------------------------------
020870*/詳細印刷の対応/0403
020880             PERFORM 負傷データ退避
020890         END-PERFORM
020900     END-READ.
020910*================================================================*
020920 施術記録取得 SECTION.
020930*
020940************************************************************
020950* 連結データから負傷データＦより以下の情報を取得           *
020960* ● 施術開始年月日...該当する部位に対して当月最初の施術日 *
020970************************************************************
020980     MOVE 施術和暦ＷＲ  TO 施記－施術和暦.
020990     MOVE 施術年ＷＲ    TO 施記－施術年.
021000     MOVE 施術月ＷＲ    TO 施記－施術月.
021010     MOVE ZERO            TO 施記－施術日.
021020     MOVE ZERO            TO 施記－患者番号.
021030     MOVE SPACE           TO 施記－枝番.
021040     START 施術記録Ｆ   KEY IS >= 施記－施術和暦年月日
021050                                  施記－患者コード.
021060     IF 状態キー = "00"
021070         MOVE SPACE  TO 終了フラグ２
021080         PERFORM 施術記録Ｆ読込
021090         PERFORM UNTIL ( 終了フラグ２       = "YES" ) OR
021100                       ( 施記－施術和暦 NOT = 施術和暦ＷＲ ) OR
021110                       ( 施記－施術年   NOT = 施術年ＷＲ   ) OR
021120                       ( 施記－施術月   NOT = 施術月ＷＲ   )
021130*            **************
021140*            * 開始年月日 *
021150*            **************
021160             PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
021170                     UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
021180            EVALUATE TRUE ALSO TRUE ALSO TRUE ALSO TRUE ALSO TRUE
021190            WHEN 施記－負傷種別(部位ＣＮＴ) = 負傷種別Ｗ(部位ＣＮＴ) ALSO
021200                 施記－部位(部位ＣＮＴ)     = 部位Ｗ(部位ＣＮＴ)     ALSO
021210                 施記－左右区分(部位ＣＮＴ) = 左右区分Ｗ(部位ＣＮＴ) ALSO
021220                 施記－負傷位置番号(部位ＣＮＴ)
021230                                       = 負傷位置番号Ｗ(部位ＣＮＴ)  ALSO
021240                 開始年月日取得フラグ(部位ＣＮＴ) = SPACE
021250                   MOVE 施記－施術年     TO 施術開始年Ｗ(部位ＣＮＴ)
021260                   MOVE 施記－施術月     TO 施術開始月Ｗ(部位ＣＮＴ)
021270                   MOVE 施記－施術日     TO 施術開始日Ｗ(部位ＣＮＴ)
021280                   MOVE "YES"       TO 開始年月日取得フラグ(部位ＣＮＴ)
021290*
021300            WHEN OTHER
021310                CONTINUE
021320            END-EVALUATE
021330             END-PERFORM
021340             PERFORM 施術記録Ｆ読込
021350         END-PERFORM
021360     END-IF.
021370*================================================================*
021380 施術記録Ｆ読込 SECTION.
021390*
021400     READ 施術記録Ｆ NEXT
021410     AT END
021420         MOVE "YES" TO 終了フラグ２
021430     END-READ.
021440*
024400*================================================================*
024401 カルテコメント取得 SECTION.
024402*
024403************************************************************
024404* カルテファイルより以下のカルテコメントを取得             *
024405* ● 同意医師情報　氏名・住所・医療機関・電話番号          *
024406* ● 同意年月日                                            *
024407* ● フリーコメント                                        *
024408************************************************************
024409     MOVE 10            TO カ表－会コード.
024410     MOVE ZERO          TO カ表－区分.
024412     MOVE 施術和暦ＷＲ  TO カ表－施術和暦.
024413     MOVE 施術年ＷＲ    TO カ表－施術年.
024414     MOVE 施術月ＷＲ    TO カ表－施術月.
024416     MOVE 患者番号ＷＲ  TO カ表－患者番号.
024417     MOVE 枝番ＷＲ      TO カ表－枝番.
024432     READ カルテファイル
024433     INVALID KEY
024443         MOVE SPACE                          TO 同意医師名Ｗ
024453         MOVE SPACE                          TO 同意元号Ｗ
024457         MOVE ZERO                           TO 同意年Ｗ
024458         MOVE ZERO                           TO 同意月Ｗ
024459         MOVE ZERO                           TO 同意日Ｗ
024460         MOVE SPACE                          TO 所見Ｗ
024460         MOVE SPACE                          TO 備考Ｗ
024461     NOT INVALID KEY 
024464         MOVE カ表コメ－同意医師名           TO 同意医師名Ｗ
024471         MOVE カ表コメ－同意和暦             TO 元－元号区分
024475         READ 元号マスタ
024476         INVALID KEY
024477             MOVE SPACE                      TO 同意元号Ｗ
024478         NOT INVALID KEY
024479             MOVE 元－元号名称               TO 同意元号Ｗ
024480         END-READ
024481         MOVE カ表コメ－同意年               TO 同意年Ｗ
024482         MOVE カ表コメ－同意月               TO 同意月Ｗ
024483         MOVE カ表コメ－同意日               TO 同意日Ｗ
024484         MOVE カ表コメ－フリーコメント       TO 備考Ｗ
024484         MOVE カ表コメ－初検の所見           TO 所見Ｗ
024485     END-READ.
021450*================================================================*
021460 負傷原因取得 SECTION.
021470*
021480********************************************************************
021490*  負傷原因コードが同じものは、1行にまとめて印字する。
021500*  例: ①② 家で転んだ.
021510*     負傷原因コードが同じものをまとめ、テーブルにセット
021520*     (ただし、部位を飛んで同じものは、2行になる)
021530********************************************************************
021540     MOVE  ZERO   TO  カウンタ カウンタ２.
021550     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
021560             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
021570*
021580****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
021590***        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
024920       IF ( 負傷原因連番Ｗ(部位ＣＮＴ)      NOT = ZERO )
021600*
021610           IF カウンタ = ZERO
021620               MOVE 1   TO  カウンタ カウンタ２
021630*               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
021640*               MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
024960               MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ) 負傷患者番号ＣＷ
024970               MOVE 負傷原因連番Ｗ(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
021650               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
021660           ELSE
021670*              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
021680*                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
025000           IF ( 負傷原因患者番号Ｗ(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
025010              ( 負傷原因連番Ｗ(部位ＣＮＴ)      = 負傷連番ＣＷ     )
021690                 COMPUTE カウンタ２ = カウンタ２  +  1
021700                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
021710              ELSE
021720                 COMPUTE カウンタ = カウンタ  +  1
021730                 MOVE 1   TO  カウンタ２
021740*                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
021750*                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
025070                 MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
025080                 MOVE 負傷原因連番Ｗ(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
021760                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
021770              END-IF
021780           END-IF
021790        END-IF
021800     END-PERFORM.
021810**************************************************************************
021820*  負傷原因マスタより文章取得
021830**************************************************************************
021840     MOVE  ZERO   TO  カウンタ カウンタ２.
021850     PERFORM VARYING カウンタ FROM 1 BY 1
021860             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
021870** 健保は 区分 01
021880*         MOVE 01                        TO 負原－区分コード
      */健保：０１、労災：０２、自賠責：０３
               EVALUATE 受－保険種別
               WHEN 70
                   MOVE 2 TO 負原－区分コード
               WHEN 80
                   MOVE 3 TO 負原－区分コード
               WHEN OTHER
                   MOVE 1 TO 負原－区分コード
               END-EVALUATE
021890         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
021900         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
021910         READ 負傷原因Ｆ
021920         NOT INVALID KEY
021930             INITIALIZE 負傷原因ＷＴ
021940             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
021950             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
021960             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
021970             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
021980             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
021990             PERFORM VARYING カウンタ２ FROM 1 BY 1
022000                     UNTIL ( カウンタ２ > 9 )  OR 
022010                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
022020                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
022030                WHEN 1
022040                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
022050                WHEN 2
022060                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
022070                WHEN 3
022080                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
022090                WHEN 4
022100                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
022110                WHEN 5
022120                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
022130                WHEN OTHER
022140                   CONTINUE
022150                END-EVALUATE
022160             END-PERFORM
022170*
022252             IF 負原－負傷原因入力区分 = 1
022253                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
022254                        負傷原因１ＷＴ  DELIMITED BY SIZE
022255                        負傷原因２ＷＴ  DELIMITED BY SIZE
022256                        負傷原因３ＷＴ  DELIMITED BY SIZE
022257                        負傷原因４ＷＴ  DELIMITED BY SIZE
022258                        負傷原因５ＷＴ  DELIMITED BY SIZE
022259                        INTO 負傷原因内容合成Ｗ(カウンタ)
022260                 END-STRING
022261             ELSE
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
022270             END-IF
022271*
022272         END-READ
022273     END-PERFORM.
022280*
022290     PERFORM 負傷原因セット.
022300*
022310*================================================================*
022320 負傷原因セット SECTION.
022330*
022340**************************************************************************
022350*  文章が1行を超える時は、複数行に分解する。
022360**************************************************************************
022370     MOVE  ZERO   TO  カウンタ カウンタ２.
022380     PERFORM VARYING カウンタ FROM 1 BY 1
022390             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
022400*
022410          INITIALIZE 負傷原因内容分解ＸＷ
022420          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
022430          IF  負傷原因内容１ＸＷ  NOT = SPACE
022440              COMPUTE カウンタ２ = カウンタ２  +  1
022450              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
022460          END-IF
022470          IF  負傷原因内容２ＸＷ  NOT = SPACE
022480              COMPUTE カウンタ２ = カウンタ２  +  1
022490              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
022500          END-IF
022510          IF  負傷原因内容３ＸＷ  NOT = SPACE
022520              COMPUTE カウンタ２ = カウンタ２  +  1
022530              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
022540          END-IF
022550          IF  負傷原因内容４ＸＷ  NOT = SPACE
022560              COMPUTE カウンタ２ = カウンタ２  +  1
022570              MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
022580          END-IF
022590          IF  負傷原因内容５ＸＷ  NOT = SPACE
022600              COMPUTE カウンタ２ = カウンタ２  +  1
022610              MOVE 負傷原因内容５ＸＷ  TO 負傷原因Ｗ(カウンタ２)
022620          END-IF
022630          IF  負傷原因内容６ＸＷ  NOT = SPACE
022600              COMPUTE カウンタ２ = カウンタ２  +  1
022610              MOVE 負傷原因内容６ＸＷ  TO 負傷原因Ｗ(カウンタ２)
022620          END-IF
022630          IF  負傷原因内容７ＸＷ  NOT = SPACE
022600              COMPUTE カウンタ２ = カウンタ２  +  1
022610              MOVE 負傷原因内容７ＸＷ  TO 負傷原因Ｗ(カウンタ２)
022620          END-IF
022630*
022640     END-PERFORM.
022650*
022660*================================================================*
022670 印刷処理 SECTION.
022680*
022690     EVALUATE 印刷モードＦＷＲ
022700     WHEN 0
022710         PERFORM 印刷処理２
022720         PERFORM 印刷処理３
022730     WHEN 1
022740         PERFORM 印刷処理２
022750     WHEN 2
022760         PERFORM 印刷処理３
022770     WHEN 3
022780         PERFORM 印刷処理４
022790*/詳細印刷の対応/0403
022800     WHEN 4
022810         IF 連印－保険証印刷区分 = 1
022820             PERFORM 印刷処理２
022830         END-IF
022840         PERFORM 印刷処理３
022850*         PERFORM 印刷処理４
022860     END-EVALUATE.
022870*================================================================*
022880 印刷処理１ SECTION.
022890*
022900     MOVE "YCH662P" TO  定義体名Ｐ.
022910*     MOVE "SCREEN"  TO  項目群名Ｐ.
022920     MOVE "GRP001"  TO  項目群名Ｐ.
022930     WRITE YCH662P.
022940*     WRITE 印刷レコード.
022950     PERFORM エラー処理Ｐ.
022960*================================================================*
022970 印刷処理２ SECTION.
022980*
022990     MOVE "YCH662P"  TO  定義体名Ｐ.
023000     MOVE "GRP001"   TO  項目群名Ｐ.
023010     WRITE YCH662P.
023020*     WRITE 印刷レコード.
023030     PERFORM エラー処理Ｐ.
023040*================================================================*
023050 印刷処理３ SECTION.
023060*
023070     MOVE "YCH662P"  TO  定義体名Ｐ.
023080     MOVE "GRP002"   TO  項目群名Ｐ.
023090     WRITE YCH662P.
023100*     WRITE 印刷レコード.
023110     PERFORM エラー処理Ｐ.
023120*================================================================*
023130 印刷処理４ SECTION.
023140*
023150     MOVE "YCH662P"  TO  定義体名Ｐ.
023160     MOVE "GRP003"   TO  項目群名Ｐ.
023170     WRITE YCH662P.
023180     PERFORM エラー処理Ｐ.
023190*================================================================*
023200 エラー処理Ｐ SECTION.
023210*
023220     IF 通知情報Ｐ NOT = "00"
023230         DISPLAY NC"帳票エラー"              UPON CONS
023240         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
023250         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
023260         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
023270         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
023280                                             UPON CONS
023290         ACCEPT  キー入力 FROM CONS
023300         PERFORM ファイル閉鎖
023310         EXIT PROGRAM
023320     END-IF.
023330*================================================================*
023340 名称埋込処理 SECTION.
023350*
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
023460     END-READ.
023470*
023760*================================================================*
023770 合計金額行セット SECTION.
023780*
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
024320*================================================================*
024330  請求期間取得 SECTION.
024340*
024350     EVALUATE 施術月ＷＲ
024360     WHEN 4
024370     WHEN 6
024380     WHEN 9
024390     WHEN 11
024400         MOVE 30 TO 請求終了日Ｗ
024410     WHEN 2
024420         COMPUTE 対象西暦Ｗ = 和暦終了年Ｗ + 施術年ＷＲ
024430         DIVIDE 4 INTO 対象西暦Ｗ GIVING    商Ｗ
024440                                  REMAINDER 余Ｗ
024450         END-DIVIDE
024460         IF 余Ｗ = ZERO
024470             MOVE 29 TO 請求終了日Ｗ
024480         ELSE
024490             MOVE 28 TO 請求終了日Ｗ
024500         END-IF
024510     WHEN 1
024520     WHEN 3
024530     WHEN 5
024540     WHEN 7
024550     WHEN 8
024560     WHEN 10
024570     WHEN 12
024580         MOVE 31 TO 請求終了日Ｗ
024590     WHEN OTHER
024600         MOVE  NC"未知のエラー発生" TO 連メ－メッセージ
024610         CALL   "MSG001"
024620         CANCEL "MSG001"
024630     END-EVALUATE.
026100*================================================================*
026110 印刷下部セット詳細 SECTION.
026120*
026130********************
026140* 負傷データセット *
026150********************
026160* １部位 *
026170**********
026180     IF 連印－負傷印刷行(1) NOT = ZERO
026190         IF 連印－部位印刷区分(1) = 1
026200             MOVE 会部位コードＷ(1)   TO 会部位コードＳＷ(連印－負傷印刷行(1))
026210             MOVE 負傷名Ｗ(1)         TO 負傷名ＳＷ      (連印－負傷印刷行(1))
026220             MOVE 負傷年Ｗ(1)         TO 負傷年ＳＷ      (連印－負傷印刷行(1))
026230             MOVE 負傷月Ｗ(1)         TO 負傷月ＳＷ      (連印－負傷印刷行(1))
026240             MOVE 負傷日Ｗ(1)         TO 負傷日ＳＷ      (連印－負傷印刷行(1))
026250             MOVE 施術開始年Ｗ(1)     TO 施術開始年ＳＷ  (連印－負傷印刷行(1))
026260             MOVE 施術開始月Ｗ(1)     TO 施術開始月ＳＷ  (連印－負傷印刷行(1))
026270             MOVE 施術開始日Ｗ(1)     TO 施術開始日ＳＷ  (連印－負傷印刷行(1))
026280         END-IF
026290         IF 連印－転帰印刷区分(1) = 1
026300             MOVE 施術終了年Ｗ(1)     TO 施術終了年ＳＷ  (連印－負傷印刷行(1))
026310             MOVE 施術終了月Ｗ(1)     TO 施術終了月ＳＷ  (連印－負傷印刷行(1))
026320             MOVE 施術終了日Ｗ(1)     TO 施術終了日ＳＷ  (連印－負傷印刷行(1))
026330             MOVE 治癒チェックＷ(1)   TO 治癒チェックＳＷ(連印－負傷印刷行(1))
026340             MOVE 中止チェックＷ(1)   TO 中止チェックＳＷ(連印－負傷印刷行(1))
026350             MOVE 転医チェックＷ(1)   TO 転医チェックＳＷ(連印－負傷印刷行(1))
026360         END-IF
026370     END-IF
026380**********
026390* ２部位 *
026400**********
026410     IF 連印－負傷印刷行(2) NOT = ZERO
026420         IF 連印－部位印刷区分(2) = 1
026430             MOVE 会部位コードＷ(2)   TO 会部位コードＳＷ(連印－負傷印刷行(2))
026440             MOVE 負傷名Ｗ(2)         TO 負傷名ＳＷ      (連印－負傷印刷行(2))
026450             MOVE 負傷年Ｗ(2)         TO 負傷年ＳＷ      (連印－負傷印刷行(2))
026460             MOVE 負傷月Ｗ(2)         TO 負傷月ＳＷ      (連印－負傷印刷行(2))
026470             MOVE 負傷日Ｗ(2)         TO 負傷日ＳＷ      (連印－負傷印刷行(2))
026480             MOVE 施術開始年Ｗ(2)     TO 施術開始年ＳＷ  (連印－負傷印刷行(2))
026490             MOVE 施術開始月Ｗ(2)     TO 施術開始月ＳＷ  (連印－負傷印刷行(2))
026500             MOVE 施術開始日Ｗ(2)     TO 施術開始日ＳＷ  (連印－負傷印刷行(2))
026510         END-IF
026520         IF 連印－転帰印刷区分(2) = 1
026530             MOVE 施術終了年Ｗ(2)     TO 施術終了年ＳＷ  (連印－負傷印刷行(2))
026540             MOVE 施術終了月Ｗ(2)     TO 施術終了月ＳＷ  (連印－負傷印刷行(2))
026550             MOVE 施術終了日Ｗ(2)     TO 施術終了日ＳＷ  (連印－負傷印刷行(2))
026560             MOVE 治癒チェックＷ(2)   TO 治癒チェックＳＷ(連印－負傷印刷行(2))
026570             MOVE 中止チェックＷ(2)   TO 中止チェックＳＷ(連印－負傷印刷行(2))
026580             MOVE 転医チェックＷ(2)   TO 転医チェックＳＷ(連印－負傷印刷行(2))
026590         END-IF
026600     END-IF
026610**********
026620* ３部位 *
026630**********
026640     IF 連印－負傷印刷行(3) NOT = ZERO
026650         IF 連印－部位印刷区分(3) = 1
026660             MOVE 会部位コードＷ(3)   TO 会部位コードＳＷ(連印－負傷印刷行(3))
026670             MOVE 負傷名Ｗ(3)         TO 負傷名ＳＷ      (連印－負傷印刷行(3))
026680             MOVE 負傷年Ｗ(3)         TO 負傷年ＳＷ      (連印－負傷印刷行(3))
026690             MOVE 負傷月Ｗ(3)         TO 負傷月ＳＷ      (連印－負傷印刷行(3))
026700             MOVE 負傷日Ｗ(3)         TO 負傷日ＳＷ      (連印－負傷印刷行(3))
026710             MOVE 施術開始年Ｗ(3)     TO 施術開始年ＳＷ  (連印－負傷印刷行(3))
026720             MOVE 施術開始月Ｗ(3)     TO 施術開始月ＳＷ  (連印－負傷印刷行(3))
026730             MOVE 施術開始日Ｗ(3)     TO 施術開始日ＳＷ  (連印－負傷印刷行(3))
026740         END-IF
026750         IF 連印－転帰印刷区分(3) = 1
026760             MOVE 施術終了年Ｗ(3)     TO 施術終了年ＳＷ  (連印－負傷印刷行(3))
026770             MOVE 施術終了月Ｗ(3)     TO 施術終了月ＳＷ  (連印－負傷印刷行(3))
026780             MOVE 施術終了日Ｗ(3)     TO 施術終了日ＳＷ  (連印－負傷印刷行(3))
026790             MOVE 治癒チェックＷ(3)   TO 治癒チェックＳＷ(連印－負傷印刷行(3))
026800             MOVE 中止チェックＷ(3)   TO 中止チェックＳＷ(連印－負傷印刷行(3))
026810             MOVE 転医チェックＷ(3)   TO 転医チェックＳＷ(連印－負傷印刷行(3))
026820         END-IF
026830     END-IF
026840**********
026850* ４部位 *
026860**********
026870     IF 連印－負傷印刷行(4) NOT = ZERO
026880         IF 連印－部位印刷区分(4) = 1
026890             MOVE 会部位コードＷ(4)   TO 会部位コードＳＷ(連印－負傷印刷行(4))
026900             MOVE 負傷名Ｗ(4)         TO 負傷名ＳＷ      (連印－負傷印刷行(4))
026910             MOVE 負傷年Ｗ(4)         TO 負傷年ＳＷ      (連印－負傷印刷行(4))
026920             MOVE 負傷月Ｗ(4)         TO 負傷月ＳＷ      (連印－負傷印刷行(4))
026930             MOVE 負傷日Ｗ(4)         TO 負傷日ＳＷ      (連印－負傷印刷行(4))
026940             MOVE 施術開始年Ｗ(4)     TO 施術開始年ＳＷ  (連印－負傷印刷行(4))
026950             MOVE 施術開始月Ｗ(4)     TO 施術開始月ＳＷ  (連印－負傷印刷行(4))
026960             MOVE 施術開始日Ｗ(4)     TO 施術開始日ＳＷ  (連印－負傷印刷行(4))
026970         END-IF
026980         IF 連印－転帰印刷区分(4) = 1
026990             MOVE 施術終了年Ｗ(4)     TO 施術終了年ＳＷ  (連印－負傷印刷行(4))
027000             MOVE 施術終了月Ｗ(4)     TO 施術終了月ＳＷ  (連印－負傷印刷行(4))
027010             MOVE 施術終了日Ｗ(4)     TO 施術終了日ＳＷ  (連印－負傷印刷行(4))
027020             MOVE 治癒チェックＷ(4)   TO 治癒チェックＳＷ(連印－負傷印刷行(4))
027030             MOVE 中止チェックＷ(4)   TO 中止チェックＳＷ(連印－負傷印刷行(4))
027040             MOVE 転医チェックＷ(4)   TO 転医チェックＳＷ(連印－負傷印刷行(4))
027050         END-IF
027060     END-IF
027070**********
027080* ５部位 *
027090**********
027100     IF 連印－負傷印刷行(5) NOT = ZERO
027110         IF 連印－部位印刷区分(5) = 1
027120             MOVE 会部位コードＷ(5)   TO 会部位コードＳＷ(連印－負傷印刷行(5))
027130             MOVE 負傷名Ｗ(5)         TO 負傷名ＳＷ      (連印－負傷印刷行(5))
027140             MOVE 負傷年Ｗ(5)         TO 負傷年ＳＷ      (連印－負傷印刷行(5))
027150             MOVE 負傷月Ｗ(5)         TO 負傷月ＳＷ      (連印－負傷印刷行(5))
027160             MOVE 負傷日Ｗ(5)         TO 負傷日ＳＷ      (連印－負傷印刷行(5))
027170             MOVE 施術開始年Ｗ(5)     TO 施術開始年ＳＷ  (連印－負傷印刷行(5))
027180             MOVE 施術開始月Ｗ(5)     TO 施術開始月ＳＷ  (連印－負傷印刷行(5))
027190             MOVE 施術開始日Ｗ(5)     TO 施術開始日ＳＷ  (連印－負傷印刷行(5))
027200         END-IF
027210         IF 連印－転帰印刷区分(5) = 1
027220             MOVE 施術終了年Ｗ(5)     TO 施術終了年ＳＷ  (連印－負傷印刷行(5))
027230             MOVE 施術終了月Ｗ(5)     TO 施術終了月ＳＷ  (連印－負傷印刷行(5))
027240             MOVE 施術終了日Ｗ(5)     TO 施術終了日ＳＷ  (連印－負傷印刷行(5))
027250             MOVE 治癒チェックＷ(5)   TO 治癒チェックＳＷ(連印－負傷印刷行(5))
027260             MOVE 中止チェックＷ(5)   TO 中止チェックＳＷ(連印－負傷印刷行(5))
027270             MOVE 転医チェックＷ(5)   TO 転医チェックＳＷ(連印－負傷印刷行(5))
027280         END-IF
027290     END-IF
027300*
027310**********
027320* １部位 *
027330**********
027340     MOVE 会部位コードＳＷ(1)   TO 負傷コード１.
027350     MOVE 負傷名ＳＷ(1)         TO 負傷名１.
027360     MOVE 負傷年ＳＷ(1)         TO 負傷年１.
027370     MOVE 負傷月ＳＷ(1)         TO 負傷月１.
027380     MOVE 負傷日ＳＷ(1)         TO 負傷日１.
027390     MOVE 施術開始年ＳＷ(1)     TO 施術開始年１.
027400     MOVE 施術開始月ＳＷ(1)     TO 施術開始月１.
027410     MOVE 施術開始日ＳＷ(1)     TO 施術開始日１.
027420     MOVE 施術終了年ＳＷ(1)     TO 施術終了年１.
027430     MOVE 施術終了月ＳＷ(1)     TO 施術終了月１.
027440     MOVE 施術終了日ＳＷ(1)     TO 施術終了日１.
027450     MOVE 治癒チェックＳＷ(1)   TO 治癒チェック１.
027460     MOVE 中止チェックＳＷ(1)   TO 中止チェック１.
027470     MOVE 転医チェックＳＷ(1)   TO 転医チェック１.
027480**********
027490* ２部位 *
027500**********
027510     MOVE 会部位コードＳＷ(2)   TO 負傷コード２.
027520     MOVE 負傷名ＳＷ(2)         TO 負傷名２.
027530     MOVE 負傷年ＳＷ(2)         TO 負傷年２.
027540     MOVE 負傷月ＳＷ(2)         TO 負傷月２.
027550     MOVE 負傷日ＳＷ(2)         TO 負傷日２.
027560     MOVE 施術開始年ＳＷ(2)     TO 施術開始年２.
027570     MOVE 施術開始月ＳＷ(2)     TO 施術開始月２.
027580     MOVE 施術開始日ＳＷ(2)     TO 施術開始日２.
027590     MOVE 施術終了年ＳＷ(2)     TO 施術終了年２.
027600     MOVE 施術終了月ＳＷ(2)     TO 施術終了月２.
027610     MOVE 施術終了日ＳＷ(2)     TO 施術終了日２.
027620     MOVE 治癒チェックＳＷ(2)   TO 治癒チェック２.
027630     MOVE 中止チェックＳＷ(2)   TO 中止チェック２.
027640     MOVE 転医チェックＳＷ(2)   TO 転医チェック２.
027650**********
027660* ３部位 *
027670**********
027680     MOVE 会部位コードＳＷ(3)   TO 負傷コード３.
027690     MOVE 負傷名ＳＷ(3)         TO 負傷名３.
027700     MOVE 負傷年ＳＷ(3)         TO 負傷年３.
027710     MOVE 負傷月ＳＷ(3)         TO 負傷月３.
027720     MOVE 負傷日ＳＷ(3)         TO 負傷日３.
027730     MOVE 施術開始年ＳＷ(3)     TO 施術開始年３.
027740     MOVE 施術開始月ＳＷ(3)     TO 施術開始月３.
027750     MOVE 施術開始日ＳＷ(3)     TO 施術開始日３.
027760     MOVE 施術終了年ＳＷ(3)     TO 施術終了年３.
027770     MOVE 施術終了月ＳＷ(3)     TO 施術終了月３.
027780     MOVE 施術終了日ＳＷ(3)     TO 施術終了日３.
027790     MOVE 治癒チェックＳＷ(3)   TO 治癒チェック３.
027800     MOVE 中止チェックＳＷ(3)   TO 中止チェック３.
027810     MOVE 転医チェックＳＷ(3)   TO 転医チェック３.
027820**********
027830* ４部位 *
027840**********
027850     MOVE 会部位コードＳＷ(4)   TO 負傷コード４.
027860     MOVE 負傷名ＳＷ(4)         TO 負傷名４.
027870     MOVE 負傷年ＳＷ(4)         TO 負傷年４.
027880     MOVE 負傷月ＳＷ(4)         TO 負傷月４.
027890     MOVE 負傷日ＳＷ(4)         TO 負傷日４.
027900     MOVE 施術開始年ＳＷ(4)     TO 施術開始年４.
027910     MOVE 施術開始月ＳＷ(4)     TO 施術開始月４.
027920     MOVE 施術開始日ＳＷ(4)     TO 施術開始日４.
027930     MOVE 施術終了年ＳＷ(4)     TO 施術終了年４.
027940     MOVE 施術終了月ＳＷ(4)     TO 施術終了月４.
027950     MOVE 施術終了日ＳＷ(4)     TO 施術終了日４.
027960     MOVE 治癒チェックＳＷ(4)   TO 治癒チェック４.
027970     MOVE 中止チェックＳＷ(4)   TO 中止チェック４.
027980     MOVE 転医チェックＳＷ(4)   TO 転医チェック４.
027990**********
028000* ５部位 *
028010**********
028020     MOVE 会部位コードＳＷ(5)   TO 負傷コード５.
028030     MOVE 負傷名ＳＷ(5)         TO 負傷名５.
028040     MOVE 負傷年ＳＷ(5)         TO 負傷年５.
028050     MOVE 負傷月ＳＷ(5)         TO 負傷月５.
028060     MOVE 負傷日ＳＷ(5)         TO 負傷日５.
028070     MOVE 施術開始年ＳＷ(5)     TO 施術開始年５.
028080     MOVE 施術開始月ＳＷ(5)     TO 施術開始月５.
028090     MOVE 施術開始日ＳＷ(5)     TO 施術開始日５.
028100     MOVE 施術終了年ＳＷ(5)     TO 施術終了年５.
028110     MOVE 施術終了月ＳＷ(5)     TO 施術終了月５.
028120     MOVE 施術終了日ＳＷ(5)     TO 施術終了日５.
028130     MOVE 治癒チェックＳＷ(5)   TO 治癒チェック５.
028140     MOVE 中止チェックＳＷ(5)   TO 中止チェック５.
028150     MOVE 転医チェックＳＷ(5)   TO 転医チェック５.
013280************
013290* 負傷原因 *
013300************
013310     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
013320     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
013330     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
013340     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
013350     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
013360     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
013370     MOVE 負傷原因Ｗ(7)       TO 負傷原因７.
013380     MOVE 負傷原因Ｗ(8)       TO 負傷原因８.
013390     MOVE 負傷原因Ｗ(9)       TO 負傷原因９.
013400     MOVE 負傷原因Ｗ(10)      TO 負傷原因１０.
013410     MOVE 負傷原因Ｗ(11)      TO 負傷原因１１.
013420     MOVE 負傷原因Ｗ(12)      TO 負傷原因１２.
013430     MOVE 負傷原因Ｗ(13)      TO 負傷原因１３.
013440     MOVE 負傷原因Ｗ(14)      TO 負傷原因１４.
013450     MOVE 負傷原因Ｗ(15)      TO 負傷原因１５.
025910*================================================================*
025920 詳細モード負傷原因処理 SECTION.
025930*
025940*******************************************************************
025950* 連印－負傷印刷行により、負傷原因データの並び替えを行う。
025960* ( 負傷原因情報Ｗを初期化し、退避済みの負傷原因情報ＳＷから
025970*         並び替えを行い、負傷原因情報Ｗを再設定する。)
025980*******************************************************************
025990*
026000     INITIALIZE 負傷原因情報Ｗ.
026010*
026020     PERFORM VARYING 詳細ＣＮＴ FROM 1 BY 1
026030                                UNTIL ( 詳細ＣＮＴ > 5)
026040*
026050        IF ( 連印－負傷印刷行(詳細ＣＮＴ) NOT = ZERO) AND
026060           ( 連印－原因印刷区分(詳細ＣＮＴ)   = 1)
026070*
026080           MOVE 連印－負傷印刷行(詳細ＣＮＴ) TO 印刷行ＳＷ
026090*
026100           MOVE 負傷患者番号ＳＷ(詳細ＣＮＴ) TO 負傷原因患者番号Ｗ(印刷行ＳＷ)
026110           MOVE 負傷連番ＳＷ(詳細ＣＮＴ) TO 負傷原因連番Ｗ(印刷行ＳＷ)
026120        END-IF
026130*
026140     END-PERFORM.
026150*
028160*================================================================*
028170 負傷データ退避 SECTION.
028180*
028190*********************************************************************
028200* ① 負傷原因取得のため、負傷データＦから負傷原因Ｗに退避する。
028210* ② 詳細モード用  連印－負傷行印刷により、印刷位置を並び替えるため、
028220*                  負傷情報Ｗ  から負傷情報ＳＷに、
028230*                  負傷原因Ｗ  から負傷原因ＳＷに退避する。
028240*********************************************************************
028250*
028260     MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷原因患者番号Ｗ(部位ＣＮＴ).
028270     MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷原因連番Ｗ(部位ＣＮＴ).
028280*
028290     MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号ＳＷ(部位ＣＮＴ).
028300     MOVE 負傷原因連番Ｗ(部位ＣＮＴ)     TO 負傷連番ＳＷ(部位ＣＮＴ).
028310*
024750*================================================================*
       原因行セット SECTION.
      */1010
      * 指定された開始行から印刷するようにセットし直す。
      * 15行.
      *
           MOVE SPACE                  TO 負傷原因Ｗテーブル２.
           MOVE ZERO                   TO カウンタ.
           IF 連印－原因開始行 = ZERO
               MOVE 1                  TO カウンタ２
           ELSE
               MOVE 連印－原因開始行   TO カウンタ２
           END-IF.
           PERFORM VARYING カウンタ FROM 1 BY 1
                   UNTIL ( カウンタ > 15 ) OR (カウンタ２ > 15)
               MOVE 負傷原因Ｗ(カウンタ) TO 負傷原因Ｗ２(カウンタ２)
               COMPUTE カウンタ２ = カウンタ２ + 1
           END-PERFORM.
      *
           PERFORM VARYING カウンタ FROM 1 BY 1
                     UNTIL カウンタ > 15
               MOVE 負傷原因Ｗ２(カウンタ) TO 負傷原因Ｗ(カウンタ)
           END-PERFORM.
028320*================================================================*
028330******************************************************************
028340 END PROGRAM YCH662.
028350******************************************************************
