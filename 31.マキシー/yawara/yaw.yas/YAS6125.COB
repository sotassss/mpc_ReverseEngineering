000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAS6125.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*    あさひ  健保共通 レセプト印刷（柔+ｳｨﾝﾄﾞｳｽﾞ版）
000100*  (助成を除く全ての保険種別用)
000110*         MED = YAW610 YAS6125P
000120* 2000年9月から
      */金属副子・運動後療の変更・追加/1805
000130*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2012-07-26
000130 DATE-COMPILED.          2012-07-26
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
000630     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000640                             ORGANIZATION             IS  INDEXED
000650                             ACCESS MODE              IS  DYNAMIC
000660                             RECORD KEY               IS 施情－施術所番号
000670                             FILE STATUS              IS  状態キー
000680                             LOCK        MODE         IS  AUTOMATIC.
000690     SELECT  請求先マスタ    ASSIGN      TO        SEIKYUSL
000700                             ORGANIZATION           IS  INDEXED
000710                             ACCESS MODE            IS  DYNAMIC
000720                             RECORD KEY             IS 請先－保険種別
000730                                                       請先－保険者番号
000740                             FILE STATUS            IS  状態キー
000750                             LOCK    MODE           IS  AUTOMATIC.
000760     SELECT  経過マスタ      ASSIGN      TO        KEIKAL
000770                             ORGANIZATION             IS  INDEXED
000780                             ACCESS MODE              IS  DYNAMIC
000790                             RECORD KEY               IS  経－区分コード
000800                                                          経－経過コード
000810                             FILE STATUS              IS  状態キー
000820                             LOCK        MODE         IS  AUTOMATIC.
000830     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
000840                             ORGANIZATION             IS  INDEXED
000850                             ACCESS MODE              IS  DYNAMIC
000860                             RECORD KEY               IS  負原－区分コード
000870                                                          負原－負傷原因コード
000880                             FILE STATUS              IS  状態キー
000890                             LOCK        MODE         IS  AUTOMATIC.
000900     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000910                             ORGANIZATION             IS  INDEXED
000920                             ACCESS MODE              IS  DYNAMIC
000930                             RECORD KEY               IS 受－施術和暦年月
000940                                                          受－患者コード
000950                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000960                                                          受－患者カナ
000970                                                          受－患者コード
000980                             ALTERNATE RECORD KEY     IS  受－患者コード
000990                                                         受－施術和暦年月
001000                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
001010                                                          受－保険種別
001020                                                          受－保険者番号
001030                                                          受－患者コード
001040                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
001050                                                          受－公費種別
001060                                                     受－費用負担者番号
001070                                                          受－患者コード
001080                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
001090                                                          受－助成種別
001100                                                  受－費用負担者番号助成
001110                                                          受－患者コード
001120                             ALTERNATE RECORD KEY  IS 受－請求和暦年月
001130                                                      受－施術和暦年月
001140                                                      受－患者コード
001150                             FILE STATUS              IS  状態キー
001160                             LOCK        MODE         IS  AUTOMATIC.
001170     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
001180                             ORGANIZATION             IS  INDEXED
001190                             ACCESS MODE              IS  DYNAMIC
001200                             RECORD KEY           IS 施記－施術和暦年月日
001210                                                     施記－患者コード
001220                             ALTERNATE RECORD KEY IS 施記－患者コード
001230                                                     施記－施術和暦年月日
001240                             FILE STATUS              IS  状態キー
001250                             LOCK        MODE         IS  AUTOMATIC.
001260     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
001270                             ORGANIZATION             IS  INDEXED
001280                             ACCESS MODE              IS  DYNAMIC
001290                             RECORD KEY               IS 負－施術和暦年月
001300                                                         負－患者コード
001310                             ALTERNATE RECORD KEY     IS 負－患者コード
001320                                                         負－施術和暦年月
001330                             FILE STATUS              IS  状態キー
001340                             LOCK        MODE         IS  AUTOMATIC.
001350     SELECT  料金マスタ      ASSIGN      TO        RYOUKINL
001360                             ORGANIZATION             IS  INDEXED
001370                             ACCESS MODE              IS  DYNAMIC
001380                             RECORD KEY               IS  料－区分コード
001390                                                          料－部位コード
001400                                                          料－開始和暦年月.
001410     SELECT  会情報マスタ    ASSIGN      TO        KAIJOHOL
001420                             ORGANIZATION             IS  INDEXED
001430                             ACCESS MODE              IS  DYNAMIC
000130                             RECORD KEY               IS  会情－柔整鍼灸区分
000131                                                          会情－協会コード
000132                                                          会情－保険種別
000133                                                          会情－変更和暦年月
000134                             ALTERNATE RECORD KEY     IS  会情－柔整鍼灸区分
000135                                                          会情－接骨師会カナ
000136                                                          会情－協会コード
000137                                                          会情－保険種別
000138                                                          会情－変更和暦年月
000151                             FILE STATUS              IS  状態キー
001520                             LOCK        MODE         IS  AUTOMATIC.
001530     SELECT  ＩＤ管理マスタ    ASSIGN      TO        IDKANRL
001540                             ORGANIZATION             IS  INDEXED
001550                             ACCESS MODE              IS  DYNAMIC
001560                             RECORD KEY               IS  ＩＤ管－ＩＤ区分
001570                                                          ＩＤ管－施術所番号
001580                                                          ＩＤ管－保険種別
001590                                                          ＩＤ管－保険者番号
001600                             ALTERNATE RECORD KEY     IS  ＩＤ管－施術ＩＤ番号
001610                                                          ＩＤ管－ＩＤ区分
001620                                                          ＩＤ管－施術所番号
001630                                                          ＩＤ管－保険種別
001640                                                          ＩＤ管－保険者番号
001650                             FILE STATUS              IS  状態キー
001660                             LOCK        MODE         IS  AUTOMATIC.
001500     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
001510                             ORGANIZATION             IS  INDEXED
001520                             ACCESS MODE              IS  DYNAMIC
001530                             RECORD KEY               IS  市－公費種別
001540                                                          市－市町村番号
001550                             ALTERNATE RECORD KEY     IS  市－公費種別
001560                                                          市－市町村名称
001570                                                          市－市町村番号
001580                             FILE STATUS              IS  状態キー
001590                             LOCK        MODE         IS  AUTOMATIC.
           SELECT  計算マスタ      ASSIGN      TO        KEISANL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  計－制御区分
                                                                計－開始和暦年月
                                   FILE STATUS              IS  状態キー
                                   LOCK        MODE         IS  AUTOMATIC.
001810* 並び順印字用
001820     SELECT  作業ファイル２  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001830                             ORGANIZATION             IS  INDEXED
001840                             ACCESS                   IS  DYNAMIC
001850                             RECORD      KEY          IS  作２－施術和暦年月
001860                                                          作２－患者コード
001870                                                          作２－保険種別
001880                             FILE        STATUS       IS  状態キー
001890                             LOCK        MODE         IS  AUTOMATIC.
001900     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF002
001910                             SYMBOLIC    DESTINATION  IS "PRT"
001920                             FORMAT                   IS  定義体名Ｐ
001930                             GROUP                    IS  項目群名Ｐ
001940                             PROCESSING  MODE         IS  処理種別Ｐ
001950                             UNIT        CONTROL      IS  拡張制御Ｐ
001960                             FILE        STATUS       IS  通知情報Ｐ.
001970******************************************************************
001980*                      DATA DIVISION                             *
001990******************************************************************
002000 DATA                    DIVISION.
002010 FILE                    SECTION.
002020*                           ［ＲＬ＝  ３２０］
002030 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
002040     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
002050*                           ［ＲＬ＝  １２８］
002060 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
002070     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
002080*                           ［ＲＬ＝  １２８］
002090 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
002100     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
002140*                           ［ＲＬ＝  ２５６］
002150 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
002160     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
002170*                           ［ＲＬ＝  １２８］
002180 FD  施術所情報マスタ          BLOCK   CONTAINS   1   RECORDS.
002190     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
002200*                           ［ＲＬ＝  １２８］
002210 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
002220     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
002230*                           ［ＲＬ＝  １２８］
002240 FD  経過マスタ          BLOCK   CONTAINS   1   RECORDS.
002250     COPY KEIKA          OF  XFDLIB  JOINING   経   AS  PREFIX.
002260*                           ［ＲＬ＝  ３２０］
002270 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
002280     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
002290*                           ［ＲＬ＝  ２５６］
002300 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
002310     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
002320*                           ［ＲＬ＝  １２８］
002330 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002340     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
002350*                           ［ＲＬ＝  １２８］
002360 FD  負傷原因Ｆ         BLOCK   CONTAINS   1   RECORDS.
002370     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
002380*
002390 FD  料金マスタ         BLOCK   CONTAINS   1   RECORDS.
002400     COPY RYOUKIN         OF  XFDLIB  JOINING   料   AS  PREFIX.
002410     COPY RYOUKNA         OF  XFDLIB  JOINING   料Ａ AS  PREFIX.
002420     COPY RYOUKNB         OF  XFDLIB  JOINING   料Ｂ AS  PREFIX.
002430     COPY RYOUKNC         OF  XFDLIB  JOINING   料Ｃ AS  PREFIX.
002440     COPY RYOUKND         OF  XFDLIB  JOINING   料Ｄ AS  PREFIX.
002450     COPY RYOUKNE         OF  XFDLIB  JOINING   料Ｅ AS  PREFIX.
002460     COPY RYOUKNF         OF  XFDLIB  JOINING   料Ｆ AS  PREFIX.
002470*                           ［ＲＬ＝  ６４０］
002480 FD  会情報マスタ        BLOCK   CONTAINS   1   RECORDS.
002490     COPY KAIJOHO         OF  XFDLIB  JOINING   会情   AS  PREFIX.
002500*                           ［ＲＬ＝  １２８］
002510 FD  ＩＤ管理マスタ          BLOCK   CONTAINS   1   RECORDS.
002520     COPY IDKANR    OF  XFDLIB  JOINING   ＩＤ管   AS  PREFIX.
002470*                           ［ＲＬ＝  ２５６］
002480 FD  市町村マスタ          BLOCK   CONTAINS   1   RECORDS.
002490     COPY SITYOSN        OF  XFDLIB  JOINING   市   AS  PREFIX.
      *                           ［ＲＬ＝  ２５６］
       FD  計算マスタ          BLOCK   CONTAINS   1   RECORDS.
           COPY KEISAN     OF  XFDLIB  JOINING   計   AS  PREFIX.
           COPY KEISANA    OF  XFDLIB  JOINING   計Ａ AS  PREFIX.
002560**
002570 FD  作業ファイル２ RECORD  CONTAINS 32 CHARACTERS.
002580 01  作２－レコード.
002590     03  作２－レコードキー.
002600         05  作２－施術和暦年月.
002610             07  作２－施術和暦            PIC 9.
002620             07  作２－施術年              PIC 9(2).
002630             07  作２－施術月              PIC 9(2).
002640         05  作２－患者コード.
002650             07 作２－患者番号             PIC 9(6).
002660             07 作２－枝番                 PIC X(1).
002670         05  作２－保険種別                PIC 9(2).
002680     03  作２－レコードデータ.
002690         05  作２－順番                    PIC 9(4).
002700         05  FILLER                        PIC X(14).
002710*
002720 FD  印刷ファイル.
002730     COPY YAS6125P        OF  XMDLIB.
002740*----------------------------------------------------------------*
002750******************************************************************
002760*                WORKING-STORAGE SECTION                         *
002770******************************************************************
002780 WORKING-STORAGE         SECTION.
002790 01 キー入力                           PIC X     VALUE SPACE.
002800 01 状態キー                           PIC X(2)  VALUE SPACE.
002810 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002820 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002830 01 初検フラグ                         PIC X(3)  VALUE SPACE.
002840 01 継続フラグ                         PIC X(3)  VALUE SPACE.
002850 01 ファイル名                         PIC N(6)  VALUE SPACE.
002860 01 レセプトＰＧＷ                     PIC X(8)  VALUE SPACE.
002870 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
002880 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
002890 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002900 01 患者番号Ｗ                         PIC 9(6)  VALUE ZERO.
002910 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002920 01 部位名称Ｗ                         PIC N(12) VALUE SPACE.
002930 01 部位長Ｗ                           PIC 9(2) VALUE 1.
002940 01 脱出フラグ                         PIC X(3)  VALUE SPACE.
002950 01 空白Ｗ                             PIC X(2)  VALUE SPACE.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002960*
002970** 数字→日本語変換
002980 01 数字Ｗ                             PIC 9(2).
002990 01 数字Ｒ REDEFINES 数字Ｗ.
003000    03 数字Ｗ１                        PIC X(1).
003010    03 数字Ｗ２                        PIC X(1).
003020*
003030 01 負傷番号Ｗ                         PIC 9.
003040 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
003050    03 負傷番号Ｗ１                    PIC X.
003060*
003070 01 全角負傷番号Ｗ                     PIC N.
003080 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
003090    03 全角負傷番号Ｗ１                PIC X(2).
003100*************
003110* 共済番号用
003120 01 共済連番号集団Ｗ.
003130    03 共済連番号名Ｗ                  PIC X(2)  VALUE SPACE.
003140    03 共済連番号名ＮＷ REDEFINES  共済連番号名Ｗ  PIC N(1).
003150    03 共済連番号Ｗ                    PIC X(6)  VALUE SPACE.
003160    03 共済連番号単位Ｗ                PIC X(2)  VALUE SPACE.
003170    03 共済連番号単位ＮＷ REDEFINES  共済連番号単位Ｗ  PIC N.
003180* 自衛官番号用
003190 01 自衛官番号集団Ｗ.
003200    03 自衛官番号名Ｗ                  PIC X(2)  VALUE SPACE.
003210    03 自衛官番号名ＮＷ REDEFINES  自衛官番号名Ｗ  PIC N(1).
003220    03 自衛官番号Ｗ                    PIC X(6)  VALUE SPACE.
003230    03 自衛官番号単位Ｗ                PIC X(2)  VALUE SPACE.
003240    03 自衛官番号単位ＮＷ REDEFINES  自衛官番号単位Ｗ  PIC N.
003250*******
003260*
003270 01 カウンタ                           PIC 9(2)  VALUE ZERO.
003280 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
003290 01 保険名称Ｗ                         PIC N(12) VALUE SPACE.
003300*
003310* 退避用
003320 01 終了年月日ＷＴ.
003330    03 終了年ＷＴ                      PIC 9(2)  VALUE ZERO.
003340    03 終了月ＷＴ                      PIC 9(2)  VALUE ZERO.
003350    03 終了日ＷＴ                      PIC 9(2)  VALUE ZERO.
003360* 初検日退避用
003370 01 初検年月日ＷＴ.
003380    03 初検和暦ＷＴ                    PIC 9     VALUE ZERO.
003390    03 初検年ＷＴ                      PIC 9(2)  VALUE ZERO.
003400    03 初検月ＷＴ                      PIC 9(2)  VALUE ZERO.
003410    03 初検日ＷＴ                      PIC 9(2)  VALUE ZERO.
003420* 負傷原因用
003430 01 負傷原因ＷＴ.
003440    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
003450    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
003460    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
003470    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
003480    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
003490    03 負傷原因ナンバーＷＴ.
003500       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
003510    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
003520 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
003530 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
003540 01 負傷原因ＴＢＬ.
003550    03 負傷原因コードＴＢＬ            OCCURS 9.
003560       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
003570       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
003580       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
003590 01 負傷原因内容Ｗ.
003600    03 負傷原因内容合成Ｗ              PIC X(318) OCCURS 9 VALUE SPACE.
003620    03 負傷原因内容分解ＸＷ.
003630       05 負傷原因内容１ＸＷ           PIC X(80)  VALUE SPACE.
003640       05 負傷原因内容２ＸＷ           PIC X(80)  VALUE SPACE.
003640       05 負傷原因内容３ＸＷ           PIC X(80)  VALUE SPACE.
003650       05 負傷原因内容４ＸＷ           PIC X(78)  VALUE SPACE.
       01 負傷原因１文Ｗ.
          03 負傷原因１文ＷＲ                OCCURS 7.
             05 負傷原因１文ＷＰ             PIC X(80) VALUE SPACE.
003650*
003660* 初検加算時刻用
003670 01 初検加算ＷＴ.
003680    03 初検加算カウント                PIC 9    VALUE ZERO.
003690    03 番号カウンタ                    PIC 9    VALUE ZERO.
003700    03 初検加算集団ＷＴ  OCCURS 3.
003710       05 初検加算区分ＷＴ             PIC 9    VALUE ZERO.
003720       05 初検加算時ＷＴ               PIC 9(2) VALUE ZERO.
003730       05 初検加算分ＷＴ               PIC 9(2) VALUE ZERO.
003740    03 初検加算集団ＮＷ  OCCURS 3.
003750       05 加算区切Ｗ                   PIC N(1) VALUE SPACE.
003760       05 加算内容Ｗ                   PIC N(3) VALUE SPACE.
003770       05 初検加算時ＮＷ１             PIC N(1) VALUE SPACE.
003780       05 初検加算時ＮＷ２             PIC N(1) VALUE SPACE.
003790       05 時固定Ｗ                     PIC N(1) VALUE SPACE.
003800       05 初検加算分ＮＷ１             PIC N(1) VALUE SPACE.
003810       05 初検加算分ＮＷ２             PIC N(1) VALUE SPACE.
003820       05 分固定Ｗ                     PIC N(1) VALUE SPACE.
003830    03 初検加算時刻１Ｗ                PIC N(10) VALUE SPACE.
003840    03 初検加算時刻２Ｗ                PIC N(10) VALUE SPACE.
003850    03 初検加算時刻３Ｗ                PIC N(10) VALUE SPACE.
003070    03 初検加算区切Ｗ                  PIC X     VALUE SPACE.
003080    03 初検加算時Ｗ                    PIC 9(2)  VALUE ZERO.
003090    03 初検加算分Ｗ                    PIC 9(2)  VALUE ZERO.
003860*
003870** 前月初検のみ用
003880 01 初日再検フラグ                     PIC X(3)  VALUE SPACE.
003890 01 前月フラグ                         PIC X(3)  VALUE SPACE.
003900*
003910 01 計算年月日Ｗ.
003920    03 計算和暦Ｗ                      PIC 9(1)  VALUE ZERO.
003930    03 計算年Ｗ                        PIC S9(2)  VALUE ZERO.
003940    03 計算月Ｗ                        PIC S9(2)  VALUE ZERO.
003950    03 計算日Ｗ                        PIC S9(2)  VALUE ZERO.
003960 01 開始年月日２Ｗ.
003970    03 開始和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003980    03 開始年２Ｗ                      PIC 9(2)  VALUE ZERO.
003990    03 開始月２Ｗ                      PIC 9(2)  VALUE ZERO.
004000    03 開始日２Ｗ                      PIC 9(2)  VALUE ZERO.
004010    03 開始西暦年Ｗ                    PIC S9(4) VALUE ZERO.
004020 01 終了年月日２Ｗ.
004030    03 終了和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
004040    03 終了年２Ｗ                      PIC 9(2)  VALUE ZERO.
004050    03 終了月２Ｗ                      PIC 9(2)  VALUE ZERO.
004060    03 終了日２Ｗ                      PIC 9(2)  VALUE ZERO.
004070    03 終了西暦年Ｗ                    PIC S9(4) VALUE ZERO.
004080***
004090** 負傷原因・長期理由印刷区分用
004100 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
004110 01 長期理由印刷区分Ｗ                 PIC 9 VALUE ZERO.
004120*
004130** レセ下段の日付区分用 (0:最終通院日、1:月末日、9:印字なし)
004140 01 レセプト日付区分Ｗ                 PIC 9 VALUE ZERO.
004150 01 レセプト患者日付区分Ｗ             PIC 9 VALUE ZERO.
004160*
004170** 月末日用
004180 01 施術西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
004190 01 商Ｗ                               PIC 9(3)  VALUE ZERO.
004200 01 余Ｗ                               PIC 9(3)  VALUE ZERO.
004210*
004220** 枝番判定用
004230 01 開始診療日手動区分Ｗ               PIC 9     VALUE ZERO.
004240*
004250** 請求先名称用
004260 01 請求先名称ＴＢＬ.
004270    03 請求先名称ＷＴ                  PIC X(2)  OCCURS 20 VALUE SPACE.
004280 01 請求先名称ＷＴ１                   PIC X(2)  VALUE SPACE.
004290 01 支部部署名Ｗ                       PIC X(40) VALUE SPACE.
004300*
004310* 社保用
004320 01 接尾語区分Ｗ                       PIC 9     VALUE ZERO.
004330*
004340* 保険者番号
004350 01 保険者番号比較Ｗ                   PIC X(6)   VALUE SPACE.
004360*
004370** 負担割合用
004380 01 負担割合数字Ｗ                     PIC 9     VALUE ZERO.
004390 01 負担割合Ｗ                         PIC 9(2)  VALUE ZERO.
004400 01 割合固定Ｗ                         PIC N     VALUE SPACE.
004410*
004420* 帳票固定印字用
004430 01 被保険者氏名固定Ｗ                 PIC N(10)  VALUE SPACE.
004440 01 生年月日固定Ｗ                     PIC N(4)   VALUE SPACE.
004450 01 被保険者住所固定Ｗ                 PIC N(10)  VALUE SPACE.
004460 01 委任固定Ｗ                         PIC N(4)   VALUE SPACE.
004470 01 県共済固定Ｗ                       PIC N(15)  VALUE SPACE.
004480*
004490* 会長委任文用
004500* 01 会長委任文１Ｗ                     PIC N(18)  VALUE SPACE.
004510* 01 会長委任文２Ｗ.
004520*    03 会長委任文２１Ｗ                PIC N(4)   VALUE SPACE.
004530*    03 会長名Ｗ                        PIC N(5)   VALUE SPACE.
004540*    03 会長委任文２２Ｗ                PIC N(9)   VALUE SPACE.
       01 会長委任文Ｗ.
      */文章の変更↓↓↓/20190307
      *    03 会長委任文１Ｗ                  PIC X(45)  VALUE SPACE.
      *    03 会長委任文２Ｗ                  PIC X(45)  VALUE SPACE.
      *    03 会長委任文３Ｗ                  PIC X(45)  VALUE SPACE.
          03 会長委任文１Ｗ                  PIC X(50)  VALUE SPACE.
          03 会長委任文２Ｗ                  PIC X(50)  VALUE SPACE.
          03 会長委任文３Ｗ                  PIC X(50)  VALUE SPACE.
      */文章の変更↑↑↑/20190307
004550*
004560*
004570** レセ摘要用( N(38)固定） /
004580 01 負傷の経過Ｗ.
004590*    03 負傷の経過行Ｗ                  PIC X(76) OCCURS 2 VALUE SPACE.
004590    03 負傷の経過行Ｗ                  PIC X(64) OCCURS 2 VALUE SPACE.
004600 01 負傷の経過ＮＷ REDEFINES 負傷の経過Ｗ.
004610*    03 負傷の経過行ＮＷ                PIC N(38) OCCURS 2.
004610    03 負傷の経過行ＮＷ                PIC N(32) OCCURS 2.
004620*
004621* 負傷原因印刷区分
004622 01 レセ負傷原因印刷区分Ｗ             PIC 9    VALUE ZERO.
002580 01 レセ長期理由印刷区分Ｗ             PIC 9    VALUE ZERO.
      *
      */金属副子・運動後療の変更・追加/1805
       01 金属副子ＣＭ                       PIC X(200) VALUE SPACE.
       01 運動後療ＣＭ                       PIC X(68)  VALUE SPACE.
004623*
004770******************************
004780* ５部位  摘要欄印字  編集用 *
004790******************************
004800 01 部位５Ｗ.
004840   03 逓減開始月日５Ｗ.
004850      05 逓減開始月５Ｗ                PIC ZZ.
            05 月ＣＭ                        PIC X(2).
004870      05 逓減開始日５Ｗ                PIC ZZ.
            05 日ＣＭ                        PIC X(2).
         03 括弧１Ｗ                         PIC X(1).
004890   03 後療５Ｗ.
            05 括弧２Ｗ                      PIC X(1).
004900      05 後療単価５Ｗ                  PIC ZZZZ.
            05 乗算記号１Ｗ                  PIC X(1).
004920      05 後療回数５Ｗ                  PIC ZZ.
            05 イコール１Ｗ                  PIC X(1).
004940      05 後療料５Ｗ                    PIC ZZ,ZZZ.
         03 括弧３Ｗ                         PIC X(1).
         03 加算記号１Ｗ                     PIC X(1).
         03 括弧４Ｗ                         PIC X(1).
004960   03 冷罨法５Ｗ.
            05 冷罨法単価５Ｗ                PIC Z(2).
            05 乗算記号２Ｗ                  PIC X(1).
004970      05 冷罨法回数５Ｗ                PIC ZZ.
            05 イコール２Ｗ                  PIC X(1).
004990      05 冷罨法料５Ｗ                  PIC ZZZZ.
         03 括弧５Ｗ                         PIC X(1).
         03 加算記号２Ｗ                     PIC X(1).
         03 括弧６Ｗ                         PIC X(1).
005010   03 温罨法５Ｗ.
            05 温罨法単価５Ｗ                PIC Z(2).
            05 乗算記号３Ｗ                  PIC X(1).
005020      05 温罨法回数５Ｗ                PIC ZZ.
            05 イコール３Ｗ                  PIC X(1).
005040      05 温罨法料５Ｗ                  PIC ZZZZ.
         03 括弧７Ｗ                         PIC X(1).
         03 加算記号３Ｗ                     PIC X(1).
         03 括弧８Ｗ                         PIC X(1).
005060   03 電療５Ｗ.
            05 電療単価５Ｗ                  PIC Z(2).
            05 乗算記号４Ｗ                  PIC X(1).
005070      05 電療回数５Ｗ                  PIC ZZ.
            05 イコール４Ｗ                  PIC X(1).
005090      05 電療料５Ｗ                    PIC ZZZZ.
            05 括弧９Ｗ                      PIC X(1).
         03 括弧１０Ｗ                       PIC X(1).
         03 乗算記号５Ｗ                     PIC X(1).
005130   03 多部位率５Ｗ                     PIC X(3).
         03 乗算記号６Ｗ                     PIC X(1).
005170   03 長期逓減率５Ｗ                   PIC 9.9.
         03 イコール５Ｗ                     PIC X(1).
005190   03 長期込小計５Ｗ                   PIC ZZ,ZZZ.
005210*
004630****************
004640* 連結項目待避 *
004650****************
004660*    ************
004670*    * 印刷キー *
004680*    ************
004690 01 対象データＷＲ.
004700    03 施術和暦年月ＷＲ.
004710       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
004720       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
004730       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
004740    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004750    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
004760    03 公費種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004770    03 費用負担者番号ＷＲ               PIC X(10) VALUE SPACE.
004780    03 助成種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004790    03 費用負担者番号助成ＷＲ           PIC X(10) VALUE SPACE.
004800    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
004810    03 患者カナＷＲ                     PIC X(20) VALUE SPACE.
004820    03 患者コードＷＲ.
004830       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
004840       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
004850*
004860*    ****************
004870*    * 基本料金情報 *
004880*    ****************
004890 01 基本料金Ｗ.
004900   03 冷罨法単価Ｗ                      PIC 9(4)  VALUE ZERO.
004910   03 温罨法単価Ｗ                      PIC 9(4)  VALUE ZERO.
004920   03 電療単価Ｗ                        PIC 9(4)  VALUE ZERO.
004930*    ************
004940*    * 料金情報 *
004950*    ************
004960*    月毎の料金
004970***********************
004980 01 料金１ＷＲ.
004990   03 初検ＷＲ.
005000      05 負担割合ＷＲ               PIC 9(3)    VALUE ZERO.
005010      05 初検料ＷＲ                 PIC 9(5)    VALUE ZERO.
005020      05 初検加算料ＷＲ             PIC 9(5)    VALUE ZERO.
         03 初検時相談料ＷＲ              PIC 9(4)    VALUE ZERO.
005030   03 再検料ＷＲ                    PIC 9(5)    VALUE ZERO.
005040   03 往療ＷＲ.
005050      05 往療距離ＷＲ               PIC 9(2)V9  VALUE ZERO.
005060      05 往療回数ＷＲ               PIC 9(2)    VALUE ZERO.
005070      05 往療料ＷＲ                 PIC 9(5)    VALUE ZERO.
005080      05 往療加算料ＷＲ             PIC 9(5)    VALUE ZERO.
005090   03 金属副子加算料ＷＲ            PIC 9(5)    VALUE ZERO.
005100   03 施術情報提供料ＷＲ            PIC 9(5)    VALUE ZERO.
005110   03 合計ＷＲ                      PIC 9(6)    VALUE ZERO.
005120   03 一部負担金ＷＲ                PIC 9(6)    VALUE ZERO.
005130   03 請求金額ＷＲ                  PIC 9(6)    VALUE ZERO.
005140   03 給付割合ＷＲ                  PIC 9(1)    VALUE ZERO.
005150   03 受給者負担額ＷＲ              PIC 9(6)    VALUE ZERO.
005160   03 助成請求金額ＷＲ              PIC 9(6)    VALUE ZERO.
005170*
005180* 負傷部位毎の料金
005190***********************
005200 01 料金２ＷＲ.
005210   03 初回処置ＷＲ    OCCURS   9.
005220      05 初回処置料ＷＲ             PIC 9(5)    VALUE ZERO.
005230*
005240* 逓減毎の料金
005250***********************
005260 01 料金３ＷＲ.
005270**********
005280* １部位 *
005290**********
005300   03 部位１ＷＲ.
005310      05 後療１ＷＲ.
005320         07 後療単価１ＷＲ              PIC 9(4)    VALUE ZERO.
005330         07 後療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
005340         07 後療料１ＷＲ                PIC 9(5)    VALUE ZERO.
005350      05 冷罨法１ＷＲ.
005360         07 冷罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
005370         07 冷罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
005380      05 温罨法１ＷＲ.
005390         07 温罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
005400         07 温罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
005410      05 電療１ＷＲ.
005420         07 電療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
005430         07 電療料１ＷＲ                PIC 9(4)    VALUE ZERO.
005440      05 小計１ＷＲ                     PIC 9(6)    VALUE ZERO.
005450      05 長期逓減率１ＷＲ               PIC 9(3)    VALUE ZERO.
005460      05 長期込小計１ＷＲ               PIC 9(6)    VALUE ZERO.
005470**********
005480* ２部位 *
005490**********
005500   03 部位２ＷＲ.
005510      05 後療２ＷＲ.
005520         07 後療単価２ＷＲ              PIC 9(4)    VALUE ZERO.
005530         07 後療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
005540         07 後療料２ＷＲ                PIC 9(5)    VALUE ZERO.
005550      05 冷罨法２ＷＲ.
005560         07 冷罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
005570         07 冷罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
005580      05 温罨法２ＷＲ.
005590         07 温罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
005600         07 温罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
005610      05 電療２ＷＲ.
005620         07 電療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
005630         07 電療料２ＷＲ                PIC 9(4)    VALUE ZERO.
005640      05 小計２ＷＲ                     PIC 9(6)    VALUE ZERO.
005650      05 長期逓減率２ＷＲ               PIC 9(3)    VALUE ZERO.
005660      05 長期込小計２ＷＲ               PIC 9(6)    VALUE ZERO.
005670******************
005680* ３部位／８割 *
005690******************
005700   03 部位３８ＷＲ.
005710      05 後療３８ＷＲ.
005720         07 後療単価３８ＷＲ              PIC 9(4)  VALUE ZERO.
005730         07 後療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
005740         07 後療料３８ＷＲ                PIC 9(5)  VALUE ZERO.
005750      05 冷罨法３８ＷＲ.
005760         07 冷罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
005770         07 冷罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
005780      05 温罨法３８ＷＲ.
005790         07 温罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
005800         07 温罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
005810      05 電療３８ＷＲ.
005820         07 電療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
005830         07 電療料３８ＷＲ                PIC 9(4)  VALUE ZERO.
005840      05 小計３８ＷＲ                     PIC 9(6)  VALUE ZERO.
005850      05 多部位込小計３８ＷＲ             PIC 9(6)  VALUE ZERO.
005860      05 長期逓減率３８ＷＲ               PIC 9(3)  VALUE ZERO.
005870      05 長期込小計３８ＷＲ               PIC 9(6)  VALUE ZERO.
005880******************
005890* ３部位／１０割 *
005900******************
005910   03 部位３０ＷＲ.
005920      05 逓減開始月日３０ＷＲ.
005930         07 逓減開始月３０ＷＲ            PIC 9(2)  VALUE ZERO.
005940         07 逓減開始日３０ＷＲ            PIC 9(2)  VALUE ZERO.
005950      05 後療３０ＷＲ.
005960         07 後療単価３０ＷＲ              PIC 9(4)  VALUE ZERO.
005970         07 後療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
005980         07 後療料３０ＷＲ                PIC 9(5)  VALUE ZERO.
005990      05 冷罨法３０ＷＲ.
006000         07 冷罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
006010         07 冷罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
006020      05 温罨法３０ＷＲ.
006030         07 温罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
006040         07 温罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
006050      05 電療３０ＷＲ.
006060         07 電療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
006070         07 電療料３０ＷＲ                PIC 9(4)  VALUE ZERO.
006080      05 小計３０ＷＲ                     PIC 9(6)  VALUE ZERO.
006090      05 長期逓減率３０ＷＲ               PIC 9(3)  VALUE ZERO.
006100      05 長期込小計３０ＷＲ               PIC 9(6)  VALUE ZERO.
006110****************
006120* ４部位／５割 *
006130****************
006140   03 部位４５ＷＲ.
006150      05 後療４５ＷＲ.
006160         07 後療単価４５ＷＲ              PIC 9(4)  VALUE ZERO.
006170         07 後療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
006180         07 後療料４５ＷＲ                PIC 9(5)  VALUE ZERO.
006190      05 冷罨法４５ＷＲ.
006200         07 冷罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
006210         07 冷罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
006220      05 温罨法４５ＷＲ.
006230         07 温罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
006240         07 温罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
006250      05 電療４５ＷＲ.
006260         07 電療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
006270         07 電療料４５ＷＲ                PIC 9(4)  VALUE ZERO.
006280      05 小計４５ＷＲ                     PIC 9(6)  VALUE ZERO.
006290      05 多部位込小計４５ＷＲ             PIC 9(6)  VALUE ZERO.
006300      05 長期逓減率４５ＷＲ               PIC 9(3)  VALUE ZERO.
006310      05 長期込小計４５ＷＲ               PIC 9(6)  VALUE ZERO.
006320****************
006330* ４部位／８割 *
006340****************
006350   03 部位４８ＷＲ.
006360      05 逓減開始月日４８ＷＲ.
006370         07 逓減開始月４８ＷＲ            PIC 9(2)  VALUE ZERO.
006380         07 逓減開始日４８ＷＲ            PIC 9(2)  VALUE ZERO.
006390      05 後療４８ＷＲ.
006400         07 後療単価４８ＷＲ              PIC 9(4)  VALUE ZERO.
006410         07 後療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
006420         07 後療料４８ＷＲ                PIC 9(5)  VALUE ZERO.
006430      05 冷罨法４８ＷＲ.
006440         07 冷罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
006450         07 冷罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
006460      05 温罨法４８ＷＲ.
006470         07 温罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
006480         07 温罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
006490      05 電療４８ＷＲ.
006500         07 電療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
006510         07 電療料４８ＷＲ                PIC 9(4)  VALUE ZERO.
006520      05 小計４８ＷＲ                     PIC 9(6)  VALUE ZERO.
006530      05 多部位込小計４８ＷＲ             PIC 9(6)  VALUE ZERO.
006540      05 長期逓減率４８ＷＲ               PIC 9(3)  VALUE ZERO.
006550      05 長期込小計４８ＷＲ               PIC 9(6)  VALUE ZERO.
006560******************
006570* ４部位／１０割 *
006580******************
006590   03 部位４０ＷＲ.
006600      05 逓減開始月日４０ＷＲ.
006610         07 逓減開始月４０ＷＲ            PIC 9(2)  VALUE ZERO.
006620         07 逓減開始日４０ＷＲ            PIC 9(2)  VALUE ZERO.
006630      05 後療４０ＷＲ.
006640         07 後療単価４０ＷＲ              PIC 9(4)  VALUE ZERO.
006650         07 後療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
006660         07 後療料４０ＷＲ                PIC 9(5)  VALUE ZERO.
006670      05 冷罨法４０ＷＲ.
006680         07 冷罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
006690         07 冷罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
006700      05 温罨法４０ＷＲ.
006710         07 温罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
006720         07 温罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
006730      05 電療４０ＷＲ.
006740         07 電療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
006750         07 電療料４０ＷＲ                PIC 9(4)  VALUE ZERO.
006760      05 小計４０ＷＲ                     PIC 9(6)  VALUE ZERO.
006770      05 長期逓減率４０ＷＲ               PIC 9(3)  VALUE ZERO.
006780      05 長期込小計４０ＷＲ               PIC 9(6)  VALUE ZERO.
006790********************
006800* ５部位／２．５割 *
006810********************
006820   03 部位５２ＷＲ.
006830      05 後療５２ＷＲ.
006840         07 後療単価５２ＷＲ              PIC 9(4)  VALUE ZERO.
006850         07 後療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
006860         07 後療料５２ＷＲ                PIC 9(5)  VALUE ZERO.
006870      05 冷罨法５２ＷＲ.
006880         07 冷罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
006890         07 冷罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
006900      05 温罨法５２ＷＲ.
006910         07 温罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
006920         07 温罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
006930      05 電療５２ＷＲ.
006940         07 電療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
006950         07 電療料５２ＷＲ                PIC 9(4)  VALUE ZERO.
006960      05 小計５２ＷＲ                     PIC 9(6)  VALUE ZERO.
006970      05 多部位込小計５２ＷＲ             PIC 9(6)  VALUE ZERO.
006980      05 長期逓減率５２ＷＲ               PIC 9(3)  VALUE ZERO.
006990      05 長期込小計５２ＷＲ               PIC 9(6)  VALUE ZERO.
007000****************
007010* ５部位／５割 *
007020****************
007030   03 部位５５ＷＲ.
007040      05 逓減開始月日５５ＷＲ.
007050         07 逓減開始月５５ＷＲ            PIC 9(2)  VALUE ZERO.
007060         07 逓減開始日５５ＷＲ            PIC 9(2)  VALUE ZERO.
007070      05 後療５５ＷＲ.
007080         07 後療単価５５ＷＲ              PIC 9(4)  VALUE ZERO.
007090         07 後療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
007100         07 後療料５５ＷＲ                PIC 9(5)  VALUE ZERO.
007110      05 冷罨法５５ＷＲ.
007120         07 冷罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
007130         07 冷罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
007140      05 温罨法５５ＷＲ.
007150         07 温罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
007160         07 温罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
007170      05 電療５５ＷＲ.
007180         07 電療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
007190         07 電療料５５ＷＲ                PIC 9(4)  VALUE ZERO.
007200      05 小計５５ＷＲ                     PIC 9(6)  VALUE ZERO.
007210      05 多部位込小計５５ＷＲ             PIC 9(6)  VALUE ZERO.
007220      05 長期逓減率５５ＷＲ               PIC 9(3)  VALUE ZERO.
007230      05 長期込小計５５ＷＲ               PIC 9(6)  VALUE ZERO.
007240****************
007250* ５部位／８割 *
007260****************
007270   03 部位５８ＷＲ.
007280      05 逓減開始月日５８ＷＲ.
007290         07 逓減開始月５８ＷＲ            PIC 9(2)  VALUE ZERO.
007300         07 逓減開始日５８ＷＲ            PIC 9(2)  VALUE ZERO.
007310      05 後療５８ＷＲ.
007320         07 後療単価５８ＷＲ              PIC 9(4)  VALUE ZERO.
007330         07 後療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
007340         07 後療料５８ＷＲ                PIC 9(5)  VALUE ZERO.
007350      05 冷罨法５８ＷＲ.
007360         07 冷罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
007370         07 冷罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
007380      05 温罨法５８ＷＲ.
007390         07 温罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
007400         07 温罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
007410      05 電療５８ＷＲ.
007420         07 電療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
007430         07 電療料５８ＷＲ                PIC 9(4)  VALUE ZERO.
007440      05 小計５８ＷＲ                     PIC 9(6)  VALUE ZERO.
007450      05 多部位込小計５８ＷＲ             PIC 9(6)  VALUE ZERO.
007460      05 長期逓減率５８ＷＲ               PIC 9(3)  VALUE ZERO.
007470      05 長期込小計５８ＷＲ               PIC 9(6)  VALUE ZERO.
007480******************
007490* ５部位／１０割 *
007500******************
007510   03 部位５０ＷＲ.
007520      05 逓減開始月日５０ＷＲ.
007530         07 逓減開始月５０ＷＲ            PIC 9(2)  VALUE ZERO.
007540         07 逓減開始日５０ＷＲ            PIC 9(2)  VALUE ZERO.
007550      05 後療５０ＷＲ.
007560         07 後療単価５０ＷＲ              PIC 9(4)  VALUE ZERO.
007570         07 後療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
007580         07 後療料５０ＷＲ                PIC 9(5)  VALUE ZERO.
007590      05 冷罨法５０ＷＲ.
007600         07 冷罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
007610         07 冷罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
007620      05 温罨法５０ＷＲ.
007630         07 温罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
007640         07 温罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
007650      05 電療５０ＷＲ.
007660         07 電療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
007670         07 電療料５０ＷＲ                PIC 9(4)  VALUE ZERO.
007680      05 小計５０ＷＲ                     PIC 9(6)  VALUE ZERO.
007690      05 長期逓減率５０ＷＲ               PIC 9(3)  VALUE ZERO.
007700      05 長期込小計５０ＷＲ               PIC 9(6)  VALUE ZERO.
007710*
007720**************
007730* 施術所情報 *
007740**************
007750 01 施術所情報Ｗ.
007760    03 柔整師番号Ｗ                    PIC X(22)  VALUE SPACE.
007760*    03 柔整師番号Ｗ.
007760*       05 柔整師番号１Ｗ               PIC X(9)  VALUE SPACE.
007760*       05 柔整師番号２Ｗ               PIC X(1)  VALUE SPACE.
007760*       05 柔整師番号３Ｗ               PIC X(1)  VALUE SPACE.
007760*       05 柔整師番号４Ｗ               PIC X(1)  VALUE SPACE.
007760*       05 柔整師番号５Ｗ               PIC X(1)  VALUE SPACE.
007770    03 共済番号Ｗ                      PIC X(24)  VALUE SPACE.
007780    03 印刷接骨師会会員番号Ｗ.
007790       05 接骨師会名Ｗ                 PIC X(8)   VALUE SPACE.
007800       05 接骨師会会員番号Ｗ           PIC X(10)  VALUE SPACE.
007810    03 代表者カナＷ                    PIC X(50)  VALUE SPACE.
007820    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
007830    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
          03 都道府県ＪＩＳＷ                PIC X(2)   VALUE SPACE.
007840    03 施術所住所Ｗ.
007850       05 施術所住所１Ｗ               PIC X(50)  VALUE SPACE.
007860       05 施術所住所２Ｗ               PIC X(50)  VALUE SPACE.
007870*    03 施術所住所Ｗ.
007880*       05 施術所住所１Ｗ               PIC X(28)  VALUE SPACE.
007890*       05 施術所住所２Ｗ               PIC X(28)  VALUE SPACE.
007900*       05 施術所住所３Ｗ               PIC X(28)  VALUE SPACE.
007910*
007920    03 施術所郵便番号Ｗ.
007930       05 施術所郵便番号１Ｗ           PIC X(3)   VALUE SPACE.
007940       05 施術所郵便番号２Ｗ           PIC X(4)   VALUE SPACE.
007950    03 施術所電話番号Ｗ                PIC X(15)  VALUE SPACE.
007960    03 定額制受理番号Ｗ                PIC X(15)  VALUE SPACE.
007970    03 受理年月日Ｗ.
007980       05 受理年Ｗ                     PIC 9(2)   VALUE ZERO.
007990       05 受理月Ｗ                     PIC 9(2)   VALUE ZERO.
008000       05 受理日Ｗ                     PIC 9(2)   VALUE ZERO.
008010    03 最終通院年月日Ｗ.
008020       05 最終通院年Ｗ                 PIC 9(2)   VALUE ZERO.
008030       05 最終通院月Ｗ                 PIC 9(2)   VALUE ZERO.
008040       05 最終通院日Ｗ                 PIC 9(2)   VALUE ZERO.
008050    03 柔整師年月日Ｗ.
008060       05 柔整師年Ｗ                   PIC 9(2)   VALUE ZERO.
008070       05 柔整師月Ｗ                   PIC 9(2)   VALUE ZERO.
008080       05 柔整師日Ｗ                   PIC 9(2)   VALUE ZERO.
008090    03 患者委任年月日Ｗ.
008100       05 患者委任年Ｗ                 PIC 9(2)   VALUE ZERO.
008110       05 患者委任月Ｗ                 PIC 9(2)   VALUE ZERO.
008120       05 患者委任日Ｗ                 PIC 9(2)   VALUE ZERO.
008130    03 取引先情報Ｗ.
008140       05 取引先銀行名Ｗ               PIC X(40)  VALUE SPACE.
008150       05 取引先銀行支店名Ｗ           PIC X(40)  VALUE SPACE.
008160       05 預金種別Ｗ                   PIC 9(1)   VALUE ZERO.
008170       05 口座番号Ｗ                   PIC X(10)  VALUE SPACE.
008180       05 口座名義人Ｗ                 PIC X(40)  VALUE SPACE.
008190       05 口座名義人カナＷ             PIC X(40)  VALUE SPACE.
008200       05 銀行名支店名Ｗ               PIC X(60)  VALUE SPACE.
008210       05 預金種別コメントＷ           PIC N(4)   VALUE SPACE.
          03 支払機関.
             05 金融機関名Ｗ.
                07 金融機関名１Ｗ            PIC X(8) VALUE SPACE.
                07 金融機関名２Ｗ            PIC X(8) VALUE SPACE.
                07 金融機関名３Ｗ            PIC X(8) VALUE SPACE.
                07 金融機関名４Ｗ            PIC X(8) VALUE SPACE.
             05 支店名Ｗ.
                07 支店名１Ｗ                PIC X(8) VALUE SPACE.
                07 支店名２Ｗ                PIC X(8) VALUE SPACE.
                07 支店名３Ｗ                PIC X(8) VALUE SPACE.
                07 支店名４Ｗ                PIC X(8) VALUE SPACE.
             05 振込チェックＷ               PIC N(1)  VALUE SPACE.
             05 普通チェックＷ               PIC N(1)  VALUE SPACE.
             05 当座チェックＷ               PIC N(1)  VALUE SPACE.
             05 銀行チェックＷ               PIC N(1)  VALUE SPACE.
             05 金庫チェックＷ               PIC N(1)  VALUE SPACE.
             05 農協チェックＷ               PIC N(1)  VALUE SPACE.
             05 本店チェックＷ               PIC N(1)  VALUE SPACE.
             05 支店チェックＷ               PIC N(1)  VALUE SPACE.
             05 本支所チェックＷ             PIC N(1)  VALUE SPACE.
008220    03 県施術ＩＤＷ                    PIC X(15)  VALUE SPACE.
008230    03 市町村施術ＩＤＷ                PIC X(15)  VALUE SPACE.
008240**************
008250* 受診者情報 *
008260**************
008270 01 受診者情報Ｗ.
008280*    03 施術和暦Ｗ                      PIC N(2)  VALUE SPACE.
      */元号修正/20190408
          03 施術和暦Ｗ                      PIC 9(1)   VALUE ZERO.
008290    03 施術年月Ｗ.
008300       05 施術年Ｗ                     PIC 9(2)   VALUE ZERO.
008310       05 施術月Ｗ                     PIC 9(2)   VALUE ZERO.
008320*    03 記号Ｗ                          PIC N(12)  VALUE SPACE.
007570    03 記号Ｗ.
007580       05 印刷記号Ｗ                   PIC N(12)  VALUE SPACE.
008330*
008340    03 番号Ｗ.
008350       05 印刷番号Ｗ                   PIC X(15)  VALUE SPACE.
008360       05 FILLER                       PIC X(15)  VALUE SPACE.
008370*    03 番号Ｗ.
008380*       05 印刷番号１Ｗ                 PIC X(10)  VALUE SPACE.
008390*       05 印刷番号２Ｗ                 PIC X(10)  VALUE SPACE.
008400*       05 FILLER                       PIC X(10)  VALUE SPACE.
008410*
          03 記号番号Ｗ.
             05 記号番号ＸＷ                 PIC X(40) VALUE SPACE.
008420    03 保険者番号Ｗ.
008430       05 印刷保険者番号Ｗ             PIC X(8)   VALUE SPACE.
008440       05 FILLER                       PIC X(2)   VALUE SPACE.
008450*
008460    03 市町村番号Ｗ.
008470       05 印刷市町村番号Ｗ             PIC X(8)   VALUE SPACE.
008480       05 FILLER                       PIC X(2)   VALUE SPACE.
001250*    03 受益者番号助成Ｗ                PIC X(20)  VALUE SPACE.
          03 受給者番号Ｗ.
             05 印刷受給者番号Ｗ             PIC X(7)  VALUE SPACE.
             05 印刷受給者番号２Ｗ           PIC X(8)  VALUE SPACE.
008490*
008500    03 請求先名称Ｗ.
008510       05 印刷請求先名称１Ｗ           PIC X(48)  VALUE SPACE.
008520       05 印刷請求先名称２Ｗ           PIC X(48)  VALUE SPACE.
008530*
008540    03 保険種別Ｗ                      PIC 9(2)   VALUE ZERO.
008550    03 被保険者情報Ｗ.
008560       05 被保険者カナＷ               PIC X(50)  VALUE SPACE.
008570       05 被保険者氏名Ｗ               PIC X(50)  VALUE SPACE.
008580       05 被保険者性別Ｗ               PIC N(1)   VALUE SPACE.
008590       05 被保険者元号Ｗ               PIC N(2)   VALUE SPACE.
008600       05 被保険者年Ｗ                 PIC 9(2)   VALUE ZERO.
008610       05 被保険者月Ｗ                 PIC 9(2)   VALUE ZERO.
008620       05 被保険者日Ｗ                 PIC 9(2)   VALUE ZERO.
008630       05 郵便番号Ｗ.
008640          07 郵便番号１Ｗ              PIC X(3)   VALUE SPACE.
008650          07 郵便番号２Ｗ              PIC X(4)   VALUE SPACE.
008660       05 被保険者住所１Ｗ             PIC X(50)  VALUE SPACE.
008670       05 被保険者住所２Ｗ             PIC X(50)  VALUE SPACE.
008990       05 電話番号Ｗ                   PIC X(35)  VALUE SPACE.
008680    03 患者情報Ｗ.
008690       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
008700       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
008710       05 患者性別Ｗ                   PIC X(4)   VALUE SPACE.
008720       05 性別チェックＷ.
008730          07 男チェックＷ              PIC N(1)   VALUE SPACE.
008740          07 女チェックＷ              PIC N(1)   VALUE SPACE.
008750       05 和暦チェックＷ.
008760          07 明治チェックＷ            PIC N(1)   VALUE SPACE.
008770          07 大正チェックＷ            PIC N(1)   VALUE SPACE.
008780          07 昭和チェックＷ            PIC N(1)   VALUE SPACE.
008790          07 平成チェックＷ            PIC N(1)   VALUE SPACE.
008800          07 元号Ｗ                    PIC N(2)   VALUE SPACE.
      */元号修正/↓↓↓20190408
008210          07 令和チェックＷ            PIC N(1)  VALUE SPACE.
                07 令和ＣＭＷ                PIC X(4)  VALUE SPACE.
009110*          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
      */元号修正/↑↑↑20190408
008810       05 患者年Ｗ                     PIC 9(2)   VALUE ZERO.
008820       05 患者月Ｗ                     PIC 9(2)   VALUE ZERO.
008830       05 患者日Ｗ                     PIC 9(2)   VALUE ZERO.
008840       05 続柄Ｗ.
008850          07 印刷続柄Ｗ                PIC N(4)   VALUE SPACE.
008860          07 FILLER                    PIC X(4)   VALUE SPACE.
008870*
008880*       05 負傷原因Ｗ                   PIC N(40) OCCURS 27 VALUE SPACE.
      */半角対応/110421
             05 負傷原因Ｗ OCCURS 29.
                07 負傷原因ＸＷ              PIC X(80)  VALUE SPACE.
008890*
008900       05 簡易続柄Ｗ                   PIC N(2)   VALUE SPACE.
008901       05 保険種別名称Ｗ１             PIC N      VALUE SPACE.
008902       05 保険種別名称Ｗ               PIC N(4)   VALUE SPACE.
008910       05 保険種別チェックＷ.
                07 国保チェックＷ            PIC N(1)   VALUE SPACE.
                07 協会チェックＷ            PIC N(1)   VALUE SPACE.
                07 組合チェックＷ            PIC N(1)   VALUE SPACE.
                07 共済チェックＷ            PIC N(1)   VALUE SPACE.
                07 後期チェックＷ            PIC N(1)   VALUE SPACE.
                07 退職チェックＷ            PIC N(1)   VALUE SPACE.
                07 自衛チェックＷ            PIC N(1)   VALUE SPACE.
             05 本人チェックＷ               PIC N(1)   VALUE SPACE.
             05 家族チェックＷ               PIC N(1)   VALUE SPACE.
             05 単独チェックＷ               PIC N(1)   VALUE SPACE.
             05 ２併チェックＷ               PIC N(1)   VALUE SPACE.
             05 高一チェックＷ               PIC N(1)   VALUE SPACE.
             05 高７チェックＷ               PIC N(1)   VALUE SPACE.
             05 ６歳チェックＷ               PIC N(1)   VALUE SPACE.
             05 ７割チェックＷ               PIC N(1)   VALUE SPACE.
             05 ８割チェックＷ               PIC N(1)   VALUE SPACE.
             05 ９割チェックＷ               PIC N(1)   VALUE SPACE.
             05 １０割チェックＷ             PIC N(1)   VALUE SPACE.
008990*
009000*    03 助成印Ｗ                        PIC N(1)  VALUE SPACE.
009000    03 助成印Ｗ.
009000      05 助成印ＸＷ                    PIC X(2)  VALUE SPACE.
009001    03 特別コメントＷ                  PIC X(16) VALUE SPACE.
009010*
009020****************
009030* 負傷データＦ *
009040****************
009050 01 負傷情報Ｗ.
009060    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
009070    03 部位情報Ｗ  OCCURS   9.
009080       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
009090       05 部位コードＷ.
009100          07 負傷種別Ｗ                PIC 9(2)  VALUE ZERO.
009110          07 部位Ｗ                    PIC 9(2)  VALUE ZERO.
009120          07 左右区分Ｗ                PIC 9(1)  VALUE ZERO.
009130          07 負傷位置番号Ｗ            PIC 9(2)  VALUE ZERO.
009140       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
009150       05 負傷年月日Ｗ.
009160          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
009170          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
009180          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
009190       05 初検年月日Ｗ.
009200          07 初検年Ｗ                  PIC 9(2)  VALUE ZERO.
009210          07 初検月Ｗ                  PIC 9(2)  VALUE ZERO.
009220          07 初検日Ｗ                  PIC 9(2)  VALUE ZERO.
009230       05 開始年月日Ｗ.
009240          07 開始年Ｗ                  PIC 9(2)  VALUE ZERO.
009250          07 開始月Ｗ                  PIC 9(2)  VALUE ZERO.
009260          07 開始日Ｗ                  PIC 9(2)  VALUE ZERO.
009270       05 終了年月日Ｗ.
009280          07 終了年Ｗ                  PIC 9(2)  VALUE ZERO.
009290          07 終了月Ｗ                  PIC 9(2)  VALUE ZERO.
009300          07 終了日Ｗ                  PIC 9(2)  VALUE ZERO.
009310       05 実日数Ｗ                     PIC 9(2)  VALUE ZERO.
009320       05 転帰区分Ｗ                   PIC 9(1)  VALUE ZERO.
009330       05 転帰区分チェックＷ.
009340          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
009350          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
009360          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
009370       05 転帰Ｗ                       PIC N(2)  VALUE SPACE.
009380       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
009390       05 部位区切Ｗ                   PIC X(1)  VALUE SPACE.
009400       05 経過略称Ｗ.
009410          07 印刷経過略称Ｗ            PIC N(5)  VALUE SPACE.
009420          07 FILLER                    PIC X(2)  VALUE SPACE.
009430    03 経過部位Ｗ                      PIC N(1)  VALUE SPACE.
009030    03 経過ＣＭ                        PIC N(4)  VALUE SPACE.
009440    03 新規チェックＷ                  PIC N(1)  VALUE SPACE.
009450    03 継続チェックＷ                  PIC N(1)  VALUE SPACE.
009460    03 請求区分Ｗ                      PIC N(2)  VALUE SPACE.
009470*
009480************
009490* 料金情報 *
009500************
009510 01 料金情報Ｗ.
009520    03 初検加算Ｗ.
009530       05 時間外チェックＷ                PIC N(1) VALUE SPACE.
009540       05 休日チェックＷ                  PIC N(1) VALUE SPACE.
009550       05 深夜チェックＷ                  PIC N(1) VALUE SPACE.
009560       05 時間外Ｗ                        PIC N(3) VALUE SPACE.
009570       05 休日Ｗ                          PIC N(2) VALUE SPACE.
009580       05 深夜Ｗ                          PIC N(2) VALUE SPACE.
009590       05 初検加算内容Ｗ                  PIC N(10) VALUE SPACE.
009600    03 往療加算Ｗ.
009610       05 夜間チェックＷ                  PIC N(1) VALUE SPACE.
009620*       05 往療深夜チェックＷ              PIC N(1) VALUE SPACE.
009630       05 難路チェックＷ                  PIC N(1) VALUE SPACE.
009640       05 暴風雨雪チェックＷ              PIC N(1) VALUE SPACE.
009650    03 金属副子チェックＷ.
009660       05 大チェックＷ                    PIC N(1) VALUE SPACE.
009670       05 中チェックＷ                    PIC N(1) VALUE SPACE.
009680       05 小チェックＷ                    PIC N(1) VALUE SPACE.
009690       05 金属大Ｗ                        PIC N(1) VALUE SPACE.
009700       05 金属中Ｗ                        PIC N(1) VALUE SPACE.
009710       05 金属小Ｗ                        PIC N(1) VALUE SPACE.
009720    03 小計Ｗ                             PIC 9(7) VALUE ZERO.
009730    03 初回処置料合計Ｗ                   PIC 9(6) VALUE ZERO.
009740    03 初回処置料チェックＷ.
009750       05 整復料チェックＷ                PIC N(1) VALUE SPACE.
009760       05 固定料チェックＷ                PIC N(1) VALUE SPACE.
009770       05 施療料チェックＷ                PIC N(1) VALUE SPACE.
      */金属副子・運動後療の変更・追加/1805
          03 金属回数Ｗ                         PIC 9(2)  VALUE ZERO.
          03 運動料Ｗ                           PIC 9(4)  VALUE ZERO.
004300*
004310** 助成レセまとめ用
004320 01 助成レセまとめフラグ               PIC X(3)  VALUE SPACE.
004330 01 助成種別略称Ｗ                     PIC N(4)  VALUE SPACE.
004340 01 助成種別略称Ｗ２                   PIC N(4)  VALUE SPACE.
009780************
009790* 備考情報 *
009800************
009810 01 備考情報Ｗ.
009820    03 適用１Ｗ                           PIC N(38) VALUE SPACE.
009830    03 適用２Ｗ                           PIC N(38) VALUE SPACE.
009840*    03 適用３Ｗ                        PIC N(38) VALUE SPACE.
009850*    03 適用４Ｗ                        PIC N(38) VALUE SPACE.
009860*    03 経過コメントＷ                     PIC N(60) VALUE SPACE.
009870*
009880* 欄外項目 *
009890    03 レセプト管理年Ｗ.
009900       05 レセ管理世紀Ｗ                  PIC 9(2)  VALUE ZERO.
009910       05 レセ管理西暦Ｗ                  PIC 9(2)  VALUE ZERO.
009920    03 総括表順番Ｗ                       PIC 9(4)  VALUE ZERO.
009870*
002060** 制御マスタ用
002140 01 レセプト新旧区分.
002150    03 一般レセＷ                      PIC 9 VALUE ZERO.
009930***
      * 負傷原因チェック用
       01 施術和暦年月ＳＷ.
         03 施術和暦ＳＷ                     PIC 9    VALUE ZERO.
         03 施術年月ＳＷ.
            05 施術年ＳＷ                    PIC 9(2) VALUE ZERO.
            05 施術月ＳＷ                    PIC 9(2) VALUE ZERO.
      *
       01 負傷原因入力フラグ                 PIC X(3) VALUE SPACE.
       01 負傷原因未入力フラグ               PIC X(3) VALUE SPACE.
       01 負傷原因対象フラグ                 PIC X(3) VALUE SPACE.
      */入力済みでも負傷原因印刷しないになっている場合はエラー/100902
       01 原因対象非印刷Ｆ                   PIC X(3) VALUE SPACE.
       01 通院フラグ                         PIC X(3) VALUE SPACE.
       01 同時日ＣＷ.
           03 同時日Ｗ                       PIC 9 OCCURS 31.
       01 カウンタ最初Ｗ                     PIC 9(2) VALUE ZERO.
       01 カウンタ最後Ｗ                     PIC 9(2) VALUE ZERO.
      *
       01 施術和暦年月日ＣＷ.
         03 施術和暦年月ＣＷ.
           05 施術和暦ＣＷ                   PIC 9    VALUE ZERO.
           05 施術年月ＣＷ.
              07 施術年ＣＷ                  PIC 9(2) VALUE ZERO.
              07 施術月ＣＷ                  PIC 9(2) VALUE ZERO.
         03 施術日ＣＷ                       PIC 9(2) VALUE ZERO.
      */エラー表示の修正
       01 エラー表示Ｆ                       PIC 9(1) VALUE ZERO.
      *
       01 多部位逓減率３Ｗ                   PIC 9(3) VALUE ZERO.
      *
009930***
009940 01 印刷制御.
009950     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
009960     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
009970     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
009980     03 拡張制御Ｐ.
009990         05 端末制御Ｐ.
010000             07 移動方向Ｐ             PIC X(1) VALUE SPACE.
010010             07 移動行数Ｐ             PIC 9(3) VALUE ZERO.
010020         05 詳細制御Ｐ                 PIC X(2) VALUE SPACE.
010030     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
010040     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
010050*
010060 01 計算機西暦年Ｗ                     PIC 9(2) VALUE ZERO.
010070* 日付ＷＯＲＫ
010080 01 和暦終了年Ｗ                       PIC 9(4) VALUE ZERO.
010090 01 計算機西暦.
010100    03 計算機西暦年                    PIC 9(4) VALUE ZERO.
010110    03 計算機西暦月日                  PIC 9(4) VALUE ZERO.
010120 01 計算機西暦Ｒ REDEFINES 計算機西暦.
010130    03 計算機世紀                      PIC 9(2).
010140    03 計算機日付                      PIC 9(6).
010150    03 計算機日付Ｒ REDEFINES 計算機日付.
010160       05 計算機年月                   PIC 9(4).
010170       05 計算機年月Ｒ REDEFINES 計算機年月.
010180         07 計算機年                   PIC 9(2).
010190         07 計算機月                   PIC 9(2).
010200       05 計算機日                     PIC 9(2).
010210*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
010220******************************************************************
010230*                          連結項目                              *
010240******************************************************************
010250*
010260**  画面入力データ
010270 01 連入－入力データ委任印刷 IS EXTERNAL.
010280    03 連入－委任印刷                     PIC 9.
       01 連入－入力データ電話印刷 IS EXTERNAL.
          03 連入－電話印刷                     PIC 9.
009190*
       01 連入－プレビュー IS EXTERNAL.
          03 連入－プレビュー区分          PIC 9.
010300*
010310************
010320* 印刷キー *
010330************
010340*
010350 01 連レ印－対象データ IS EXTERNAL.
010360    03 連レ印－施術年月日.
010370       05 連レ印－施術和暦                  PIC 9(1).
010380       05 連レ印－施術年                    PIC 9(2).
010390       05 連レ印－施術月                    PIC 9(2).
010400    03 連レ印－患者コード.
010410       05 連レ印－患者番号                  PIC 9(6).
010420       05 連レ印－枝番                      PIC X(1).
010430    03 連レ印－保険種別                     PIC 9(2).
010440    03 連レ印－保険者番号                   PIC X(10).
010450    03 連レ印－公費種別                     PIC 9(2).
010460    03 連レ印－費用負担者番号               PIC X(10).
010470    03 連レ印－助成種別                     PIC 9(2).
010480    03 連レ印－費用負担者番号助成           PIC X(10).
010490    03 連レ印－患者カナ                     PIC X(20).
010500    03 連レ印－本人家族区分                 PIC 9(1).
013930*
013940 01 連レ－キー IS EXTERNAL.
013950    03 連レ－保険種別                  PIC 9(2).
013960************************
013970** ３カ月長期判定
013980************************
013990 01 連期間－キー IS EXTERNAL.
014000    03 連期間－施術年月.
014010       05 連期間－施術和暦               PIC 9.
014020       05 連期間－施術年                 PIC 9(2).
014030       05 連期間－施術月                 PIC 9(2).
014040    03  連期間－患者コード.
014050       05 連期間－患者番号               PIC 9(6).
014060       05 連期間－枝番                   PIC X.
014070    03 連期間－対象フラグ                PIC X(3).
014080    03 連期間－期間月Ｗ.
014090       05 連期間－期間Ｗ                 PIC 9(2) OCCURS 9.
014100*
014110************************
014120* 長期理由文セット     *
014130************************
014140 01 連長文－キー IS EXTERNAL.
014150    03 連長文－施術年月.
014160       05 連長文－施術和暦               PIC 9.
014170       05 連長文－施術年                 PIC 9(2).
014180       05 連長文－施術月                 PIC 9(2).
014190    03  連長文－患者コード.
014200       05 連長文－患者番号               PIC 9(6).
014210       05 連長文－枝番                   PIC X.
014220    03 連長文－文桁数                    PIC 9(2).
014230    03 連長文－理由文                    PIC N(63) OCCURS 15.
014240*
014230************************
014240* 摘要文セット     *
014250************************
014260 01 連摘文－キー IS EXTERNAL.
014270    03 連摘文－施術年月.
014280       05 連摘文－施術和暦               PIC 9.
014290       05 連摘文－施術年                 PIC 9(2).
014300       05 連摘文－施術月                 PIC 9(2).
014310    03  連摘文－患者コード.
014320       05 連摘文－患者番号               PIC 9(6).
014330       05 連摘文－枝番                   PIC X.
014340    03 連摘文－文桁数                    PIC 9(2).
014350    03 連摘文－摘要文                    PIC X(126) OCCURS 30.
014340    03 連摘文－長期区分                  PIC 9(1).
014370*
014761*
014762************************
014763* レセ負傷原因印刷判定
014764************************
014765 01 連レセ負原印－キー IS EXTERNAL.
014766    03 連レセ負原印－施術年月.
014767       05 連レセ負原印－施術和暦               PIC 9.
014768       05 連レセ負原印－施術年                 PIC 9(2).
014769       05 連レセ負原印－施術月                 PIC 9(2).
014770    03  連レセ負原印－患者コード.
014771       05 連レセ負原印－患者番号               PIC 9(6).
014772       05 連レセ負原印－枝番                   PIC X.
014773    03 連レセ負原印－対象フラグ                PIC X(3).
014774*
014380************************
014390* 助成レセまとめ
014400************************
014410 01 連レセまとめ－キー IS EXTERNAL.
014420    03 連レセまとめ－施術和暦年月.
014430       05 連レセまとめ－施術和暦               PIC 9.
014440       05 連レセまとめ－施術年月.
014450          07 連レセまとめ－施術年              PIC 9(2).
014460          07 連レセまとめ－施術月              PIC 9(2).
014470    03 連レセまとめ－患者コード.
014480       05 連レセまとめ－患者番号               PIC 9(6).
014490       05 連レセまとめ－枝番                   PIC X(1).
014500**-------------------------------------------------------**
014510*   1:助成レセプトなしの本体まとめの判定
014520*   2:横浜・川崎用の社保助成レセかの判定
014530    03 連レセまとめ－判定区分                  PIC 9.
014540**-------------------------------------------------------**
014550*  / OUT /　 0:対象外、1:対象
014560    03 連レセまとめ－判定結果                  PIC 9.
014570**
      */負担率取得ＰＧの対応1410
       01 連率－負担率取得キー IS EXTERNAL.
          03 連率－施術和暦年月.
             05 連率－施術和暦               PIC 9.
             05 連率－施術年月.
                07 連率－施術年              PIC 9(2).
                07 連率－施術月              PIC 9(2).
          03 連率－患者コード.
             05 連率－患者番号               PIC 9(6).
             05 連率－枝番                   PIC X.
          03 連率－実際負担率                PIC 9(3).
          03 連率－実際本体負担率            PIC 9(3).
          03 連率－健保負担率                PIC 9(3).
          03 連率－２７老負担率              PIC 9(3).
          03 連率－助成負担率                PIC 9(3).
          03 連率－特別用負担率              PIC 9(3).
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
013822*************
013823* 助成名称
013824*************
013825 01 連助成名称－キー IS EXTERNAL.
013826    03 連助成名称－助成種別             PIC 9(2).
013827    03 連助成名称－費用負担者番号助成   PIC X(10).
013828*   / OUT /
013829    03 連助成名称－名称集団.
013830       05 連助成名称－１文字            PIC N.
013831       05 連助成名称－略称              PIC N(4).
013832       05 連助成名称－正式名称          PIC N(10).
013833*
001684************************
001685* 長期頻回フラグセット *
001686************************
       01 連長頻－キー IS EXTERNAL.
          05 連長頻－長期フラグ            PIC X(3).
          05 連長頻－頻回フラグ            PIC X(3).
      * 
      */金属副子・運動後療の変更・追加/1805
       01 連金運－キー IS EXTERNAL.
          03 連金運－施術和暦年月.
             05 連金運－施術和暦                  PIC 9(1).
             05 連金運－施術年月.
                07 連金運－施術年                 PIC 9(2).
                07 連金運－施術月                 PIC 9(2).
          03 連金運－患者コード.
             05 連金運－患者番号                  PIC 9(6).
             05 連金運－枝番                      PIC X(1).
          03 連金運－保険種別                     PIC 9(2).
          03 連金運－会コード                     PIC 9(2).
          03 連金運－用紙種別                     PIC 9(1).
          03 連金運－金属副子.
             05 連金運－金属副子ＣＭ              PIC X(200).
             05 連金運－金属副子部位              OCCURS 5.
                07 連金運－金属副子和暦年月日     OCCURS 3.
                   09 連金運－金属副子和暦年月.
                      11 連金運－金属副子和暦     PIC 9(1).
                      11 連金運－金属副子年月.
                         13 連金運－金属副子年    PIC 9(2).
                         13 連金運－金属副子月    PIC 9(2).
                   09 連金運－金属副子日          PIC 9(2).
          03 連金運－運動後療.
             05 連金運－運動後療ＣＭ              PIC X(100).
             05 連金運－運動日                    PIC 9(2)    OCCURS 5.
001699*
009182* 確認メッセージ用 (２行)
009183 01 連メ７－キー IS EXTERNAL.
009184    03  連メ７－メッセージ１             PIC X(40).
009185    03  連メ７－メッセージ２             PIC X(40).
      *
014250******************************************************************
014260*                      PROCEDURE  DIVISION                       *
014270******************************************************************
014280 PROCEDURE               DIVISION.
014290************
014300*           *
014310* 初期処理   *
014320*           *
014330************
002570     PERFORM プリンタファイル作成.
014340     PERFORM 初期化.
014350************
014360*           *
014370* 主処理     *
014380*           *
014390************
014400* 印刷
014410     PERFORM 連結項目待避.
014420     PERFORM 印刷セット.
014430     PERFORM 印刷処理.
014440************
014450*           *
014460* 終了処理   *
014470*           *
014480************
014490     PERFORM 受診者印刷区分更新.
014500     PERFORM 終了処理.
014510     MOVE ZERO  TO PROGRAM-STATUS.
014520     EXIT PROGRAM.
014530*
014540*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
           MOVE "RECE"                TO Ｈ連特殊ＰＲＴＦ－用紙種類.
002970*   使用するプリンタファイル名セット
002971     MOVE "PRTF002"             TO Ｈ連ＰＲＴＦ－ファイル名.
002972*
002973*   使用する帳票プログラム名セット
002974     MOVE "YAS6125"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分  TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
014550*================================================================*
014560 初期化 SECTION.
014570*================================================================*
014580*
014590     PERFORM ファイルオープン.
014600*    /* 現在日付取得 */
014610     ACCEPT 計算機日付 FROM DATE.
014620*    /* 1980～2079年の間で設定 */
014630     IF 計算機年 > 80
014640         MOVE 19 TO 計算機世紀
014650     ELSE
014660         MOVE 20 TO 計算機世紀
014670     END-IF.
014680     PERFORM カレント元号取得.
014690     PERFORM 和暦終了年取得.
014700     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
014710*================================================================*
014720 ファイルオープン SECTION.
014730*
014740     OPEN INPUT   保険者マスタ
014750         MOVE NC"保険者" TO ファイル名.
014760         PERFORM オープンチェック.
014770     OPEN INPUT   元号マスタ
014780         MOVE NC"元号" TO ファイル名.
014790         PERFORM オープンチェック.
014800     OPEN INPUT   名称マスタ
014810         MOVE NC"名称" TO ファイル名.
014820         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
014860     OPEN INPUT   制御情報マスタ
014870         MOVE NC"制御情報" TO ファイル名.
014880         PERFORM オープンチェック.
014890     OPEN INPUT   施術所情報マスタ
014900         MOVE NC"施情" TO ファイル名.
014910         PERFORM オープンチェック.
014920     OPEN INPUT   請求先マスタ
014930         MOVE NC"請先" TO ファイル名.
014940         PERFORM オープンチェック.
014950     OPEN INPUT   経過マスタ
014960         MOVE NC"経過" TO ファイル名.
014970         PERFORM オープンチェック.
014980     OPEN INPUT   施術記録Ｆ.
014990         MOVE NC"施記Ｆ" TO ファイル名.
015000         PERFORM オープンチェック.
015010     OPEN INPUT   負傷データＦ.
015020         MOVE NC"負傷" TO ファイル名.
015030         PERFORM オープンチェック.
015040     OPEN INPUT   負傷原因Ｆ.
015050         MOVE NC"負傷原因" TO ファイル名.
015060         PERFORM オープンチェック.
015070     OPEN INPUT  ＩＤ管理マスタ.
015080         MOVE NC"ＩＤ" TO ファイル名.
015090         PERFORM オープンチェック.
015100     OPEN I-O   受診者情報Ｆ.
015110         MOVE NC"受情" TO ファイル名.
015120         PERFORM オープンチェック.
015130     OPEN INPUT 料金マスタ.
015140         MOVE NC"料金" TO ファイル名.
015150         PERFORM オープンチェック.
015160     OPEN INPUT   会情報マスタ.
015170         MOVE NC"会情" TO ファイル名.
015180         PERFORM オープンチェック.
016150     OPEN INPUT 市町村マスタ.
016160         MOVE NC"市町村" TO ファイル名.
016170         PERFORM オープンチェック.
           OPEN INPUT 計算マスタ.
               MOVE NC"計算" TO ファイル名.
               PERFORM オープンチェック.
015220     OPEN INPUT   作業ファイル２.
015230         MOVE NC"作２" TO ファイル名.
015240         PERFORM オープンチェック.
015250     OPEN I-O   印刷ファイル
015260         PERFORM エラー処理Ｐ.
015270*================================================================*
015280 オープンチェック SECTION.
015290*
015300     IF 状態キー  NOT =  "00"
015310         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
015320         DISPLAY NC"状態キー：" 状態キー         UPON CONS
015330         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
015340                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015350         ACCEPT  キー入力 FROM CONS
015360         PERFORM ファイル閉鎖
015370         EXIT PROGRAM.
015380*================================================================*
015390 カレント元号取得 SECTION.
015400*
015410     MOVE ZEROS TO 制－制御区分.
015420     READ 制御情報マスタ
015430     NOT INVALID KEY
015440         MOVE 制－カレント元号         TO カレント元号Ｗ
015450         MOVE 制－レセ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
015460         MOVE 制－レセ長期理由印刷区分 TO 長期理由印刷区分Ｗ
015470         MOVE 制－レセプト日付区分     TO レセプト日付区分Ｗ
015480         MOVE 制－レセプト患者日付区分 TO レセプト患者日付区分Ｗ
017300         MOVE 制－国保レセ             TO 一般レセＷ
015490     END-READ.
015500*
015510*================================================================*
015520 和暦終了年取得 SECTION.
015530*
015540*     DISPLAY NC"カレント元号Ｗ"  カレント元号Ｗ UPON MSGBOX.
015550     MOVE カレント元号Ｗ TO 元－元号区分.
015560     READ 元号マスタ
015570     INVALID KEY
015580         DISPLAY NC"指定和暦が登録されていません" UPON CONS
015590         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
015600                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015610         ACCEPT  キー入力 FROM CONS
015620         PERFORM 終了処理
015630         EXIT PROGRAM
015640     NOT INVALID KEY
015650         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
015660         MOVE 前和暦Ｗ TO 元－元号区分
015670         READ 元号マスタ
015680         INVALID KEY
015690             DISPLAY NC"指定和暦が登録されていません" UPON CONS
015700             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
015710                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015720             ACCEPT  キー入力 FROM CONS
015730             PERFORM 終了処理
015740             EXIT PROGRAM
015750         NOT INVALID KEY
015760             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
015770         END-READ
015780     END-READ.
015790*
015800*================================================================*
015810 連結項目待避 SECTION.
015820*
015830     MOVE 連レ印－施術和暦           TO 施術和暦ＷＲ.
015840     MOVE 連レ印－施術年             TO 施術年ＷＲ.
015850     MOVE 連レ印－施術月             TO 施術月ＷＲ.
015860     MOVE 連レ印－保険種別           TO 保険種別ＷＲ.
015870     MOVE 連レ印－保険者番号         TO 保険者番号ＷＲ.
015880     MOVE 連レ印－公費種別           TO 公費種別ＷＲ.
015890     MOVE 連レ印－費用負担者番号     TO 費用負担者番号ＷＲ.
015900     MOVE 連レ印－助成種別           TO 助成種別ＷＲ.
015910     MOVE 連レ印－費用負担者番号助成 TO 費用負担者番号助成ＷＲ.
015920     MOVE 連レ印－本人家族区分       TO 本人家族区分ＷＲ.
015930     MOVE 連レ印－患者カナ           TO 患者カナＷＲ.
015940     MOVE 連レ印－患者番号           TO 患者番号ＷＲ.
015950     MOVE 連レ印－枝番               TO 枝番ＷＲ.
015960*================================================================*
015970 印刷セット SECTION.
015980*
015990     PERFORM 項目初期化.
           PERFORM 基本情報取得.
016000     PERFORM 施術所情報取得.
           IF 公費種別ＷＲ = 05
016010         PERFORM 請求先情報取得後高
           ELSE
016010         PERFORM 請求先情報取得
           END-IF
016020     PERFORM 受診者情報取得.
016030     PERFORM 負傷データ取得.
016040     PERFORM 料金情報取得.
016050     PERFORM 施術記録取得.
016060*******     PERFORM 長期判定取得.
016070*******     PERFORM 初検日以前のデータ判定.
016080     PERFORM 初検加算時刻取得.
016090     PERFORM 助成印取得.
016100     PERFORM 基本料取得.
016110     PERFORM 施術西暦年取得.
016120     PERFORM レセプト並び順取得.
016130     PERFORM 委任年月日取得.
           PERFORM 施術日取得.
016140*
016791*-----------------------------------------------*
016800     IF ( 負傷原因印刷区分Ｗ  NOT = 1 ) AND ( レセ負傷原因印刷区分Ｗ NOT = 1 )
016813        IF ( 負傷原因印刷区分Ｗ = 3 OR 4 )
016815           PERFORM 負傷原因印刷対象判定処理
016817        ELSE
016820           PERFORM 負傷原因取得
016821        END-IF
016830     END-IF.
016831*-----------------------------------------------*
016180*
015940     IF ( 長期理由印刷区分Ｗ NOT = 1 )
               MOVE 長期理由印刷区分Ｗ TO 連摘文－長期区分
016000     END-IF.
016250*
016260     PERFORM 負担割合取得.
016270**
016280********************
016290* 受診者情報セット *
016300********************
016310*
016320*     MOVE 施術和暦Ｗ          TO 施術和暦１ 施術和暦２.
016330     MOVE 施術年Ｗ            TO 施術年１.
016340     MOVE 施術月Ｗ            TO 施術月１.
016350*
           IF ( 印刷記号Ｗ(1:1) = NC"＊" )
              MOVE  SPACE          TO  記号Ｗ
           END-IF.
           IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
              ( 印刷番号Ｗ(1:2) = "＊" )
              MOVE  SPACE          TO  番号Ｗ
           END-IF.
      *
           INSPECT 記号Ｗ  REPLACING ALL "　" BY "  ".
           EVALUATE TRUE
           WHEN (記号Ｗ NOT = SPACE) AND (番号Ｗ NOT = SPACE)
               MOVE SPACE TO 終了フラグ２
               PERFORM VARYING カウンタ FROM 24 BY -1
                 UNTIL (カウンタ <= ZERO) OR (終了フラグ２ NOT = SPACE)
                   IF 記号Ｗ(カウンタ:1) NOT = SPACE
                       MOVE 記号Ｗ TO 記号番号Ｗ
                       MOVE "・"   TO 記号番号Ｗ(カウンタ + 1:2)
                       MOVE 番号Ｗ TO 記号番号Ｗ(カウンタ + 3:40 - カウンタ - 2)
                       MOVE "YES"  TO 終了フラグ２
                   END-IF
               END-PERFORM
               MOVE 記号番号Ｗ TO 記号番号
           WHEN 記号Ｗ NOT = SPACE
               MOVE 記号Ｗ TO 記号番号
           WHEN 番号Ｗ NOT = SPACE
               MOVE 番号Ｗ TO 記号番号
           END-EVALUATE.
           MOVE 国保チェックＷ   TO 国保チェック.
           MOVE 協会チェックＷ   TO 協会チェック.
           MOVE 組合チェックＷ   TO 組合チェック.
           MOVE 共済チェックＷ   TO 共済チェック.
           MOVE 後期チェックＷ   TO 後期チェック.
           MOVE 退職チェックＷ   TO 退職チェック.
           MOVE 自衛チェックＷ   TO 自衛チェック.
           IF 自衛チェックＷ NOT = SPACE
               MOVE "7.自"       TO 自衛チェックＭＣ
           END-IF.
           MOVE 本人チェックＷ   TO 本人チェック.
           MOVE 家族チェックＷ   TO 家族チェック.
           MOVE 単独チェックＷ   TO 単独チェック.
           MOVE ２併チェックＷ   TO ２併チェック.
           MOVE 高一チェックＷ   TO 高一チェック.
           MOVE 高７チェックＷ   TO 高７チェック.
           MOVE ６歳チェックＷ   TO ６歳チェック.
           MOVE ７割チェックＷ   TO ７割チェック.
           MOVE ８割チェックＷ   TO ８割チェック.
           MOVE ９割チェックＷ   TO ９割チェック.
           MOVE １０割チェックＷ TO １０割チェック.
      */元号修正/↓↓↓20190408
037370     IF 施術和暦Ｗ > 4
              MOVE 施術和暦Ｗ         TO 元－元号区分
037380        READ 元号マスタ
037390        NOT INVALID KEY
037400            MOVE 元－元号名称   TO 施術和暦
037410        END-READ
              MOVE "===="             TO 施術和暦訂正
           END-IF.
      */元号修正/↑↑↑20190408
016570*
016580*     MOVE 委任固定Ｗ          TO 委任固定.
016590     MOVE 印刷保険者番号Ｗ    TO 保険者番号.
016600*     MOVE 請求先名称Ｗ        TO 保険者名称.
016610     MOVE 印刷請求先名称１Ｗ  TO 保険者名称１.
016620     MOVE 印刷請求先名称２Ｗ  TO 保険者名称２.
016630*     MOVE 保険種別名称Ｗ      TO 保険種別.
016640*     MOVE 被保険者氏名固定Ｗ  TO 氏名固定.
016650*     MOVE 被保険者カナＷ      TO 被保険者カナ.
016660     MOVE 被保険者氏名Ｗ      TO 被保険者氏名.
016670*     MOVE 被保険者性別Ｗ      TO 被保険者性別.
016671***
016672*     IF ( 被保険者年Ｗ NOT = ZERO ) AND ( 被保険者月Ｗ NOT = ZERO ) AND ( 被保険者日Ｗ NOT = ZERO ) 
016680*        MOVE 生年月日固定Ｗ      TO 生年月日固定
016690*        MOVE 被保険者元号Ｗ      TO 被保険者元号
016700*        MOVE 被保険者年Ｗ        TO 被保険者年
016710*        MOVE NC"年"              TO 被保険者年固定
016720*        MOVE 被保険者月Ｗ        TO 被保険者月
016730*        MOVE NC"月"              TO 被保険者月固定
016740*        MOVE 被保険者日Ｗ        TO 被保険者日
016750*        MOVE NC"日"              TO 被保険者日固定
016751*     END-IF.
016760***
      */ 郵便番号・電話番号追加 /42505
           IF (施術和暦年月ＷＲ >= 42505) AND (連入－電話印刷 = 1)
              IF (受－柔整郵便電話番号印刷 = 0 OR 2) AND
                 ((郵便番号１Ｗ NOT = SPACE) OR (郵便番号２Ｗ NOT = SPACE))
017280           MOVE "〒"          TO 郵便
017260           MOVE 郵便番号１Ｗ  TO 郵便番号１
017270           MOVE 郵便番号２Ｗ  TO 郵便番号２
017280           MOVE "-"           TO 郵便番号区切
              END-IF
              IF 受－柔整郵便電話番号印刷 = 0 OR 3
017260           MOVE 電話番号Ｗ    TO 電話番号
              END-IF
           END-IF.
016800     MOVE 被保険者住所１Ｗ    TO 住所１.
016810     MOVE 被保険者住所２Ｗ    TO 住所２.
016820     MOVE 患者カナＷ          TO 患者カナ.
016830     MOVE 患者氏名Ｗ          TO 患者氏名.
016840*     MOVE 患者性別Ｗ          TO 患者性別.
016850     MOVE 男チェックＷ        TO 男チェック.
016860     MOVE 女チェックＷ        TO 女チェック.
016870     MOVE 明治チェックＷ      TO 明治チェック.
016880     MOVE 大正チェックＷ      TO 大正チェック.
016890     MOVE 昭和チェックＷ      TO 昭和チェック.
016900     MOVE 平成チェックＷ      TO 平成チェック.
016910*     MOVE 元号Ｗ              TO 元号.
      */元号修正↓↓↓/20190408
           MOVE 令和ＣＭＷ         TO 令和ＣＭ.
023070     MOVE 令和チェックＷ     TO 令和チェック.
017390*     MOVE 元号Ｗ              TO 患者和暦.
      */元号修正↑↑↑/20190408
016920     MOVE 患者年Ｗ            TO 患者年.
016930*     MOVE NC"年"              TO 患者年固定.
016940     MOVE 患者月Ｗ            TO 患者月.
016950*     MOVE NC"月"              TO 患者月固定.
016960     MOVE 患者日Ｗ            TO 患者日.
016970*     MOVE NC"日"              TO 患者日固定.
      */大阪府内の助成は本体に負担者番号、受給者番号を記載する
           IF 市町村番号Ｗ(3:2) = "27"
               IF 市町村番号Ｗ(1:2) NOT = "99"
                   MOVE 市町村番号Ｗ TO 公費負担者番号
               END-IF
      */受給者番号が８文字以上の場合枠を無視して印刷する/110425
               IF 印刷受給者番号２Ｗ = SPACE
                   MOVE 印刷受給者番号Ｗ TO 受給者番号
               ELSE
                   MOVE 受給者番号Ｗ     TO 受給者番号２
               END-IF
           END-IF.
016980*     MOVE 印刷続柄Ｗ          TO 続柄.
016990* 
017000*     MOVE NC"（業務災害・通勤災害又は第三者行為以外の原因による）" 
017010*                              TO 負傷原因０.
017020     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
017030     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
017040     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
017050     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
017060     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
017060     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
017070*
017080     MOVE 助成印Ｗ            TO 助成印１ 助成印２.
017090     IF 助成印Ｗ NOT = NC"　"
017100        MOVE NC"○"           TO 助成印固定１ 助成印固定２
017110     END-IF.
017120*
017260********************
017270* 負傷データセット *
017280********************
017290* １部位 *
017300**********
017310     MOVE 負傷名Ｗ(1)       TO 負傷名１.
017320     MOVE 負傷年Ｗ(1)       TO 負傷年１.
017330     MOVE 負傷月Ｗ(1)       TO 負傷月１.
017340     MOVE 負傷日Ｗ(1)       TO 負傷日１.
017350     MOVE 初検年Ｗ(1)       TO 初検年１.
017360     MOVE 初検月Ｗ(1)       TO 初検月１.
017370     MOVE 初検日Ｗ(1)       TO 初検日１.
017380     MOVE 開始年Ｗ(1)       TO 開始年１.
017390     MOVE 開始月Ｗ(1)       TO 開始月１.
017400     MOVE 開始日Ｗ(1)       TO 開始日１.
017410     MOVE 終了年Ｗ(1)       TO 終了年１.
017420     MOVE 終了月Ｗ(1)       TO 終了月１.
017430     MOVE 終了日Ｗ(1)       TO 終了日１.
017440     MOVE 実日数Ｗ(1)       TO 実日数１.
017450     MOVE 治癒チェックＷ(1) TO 治癒チェック１.
017460     MOVE 中止チェックＷ(1) TO 中止チェック１.
017470     MOVE 転医チェックＷ(1) TO 転医チェック１.
017480*
017490*     MOVE 転帰Ｗ(1)         TO 転帰１.
017500*     IF 負傷年Ｗ(1) NOT = ZERO
017510*        MOVE "."            TO 区切１１ 区切１２
017520*     END-IF.
017530*     IF 初検年Ｗ(1) NOT = ZERO
017540*        MOVE "."            TO 区切１３ 区切１４
017550*     END-IF.
017560*     IF 開始年Ｗ(1) NOT = ZERO
017570*        MOVE "."            TO 区切１５ 区切１６
017580*     END-IF.
017590*     IF 終了年Ｗ(1) NOT = ZERO
017600*        MOVE "."            TO 区切１７ 区切１８
017610*     END-IF.
017620**********
017630* ２部位 *
017640**********
017650     MOVE 負傷名Ｗ(2)       TO 負傷名２.
017660     MOVE 負傷年Ｗ(2)       TO 負傷年２.
017670     MOVE 負傷月Ｗ(2)       TO 負傷月２.
017680     MOVE 負傷日Ｗ(2)       TO 負傷日２.
017690     MOVE 初検年Ｗ(2)       TO 初検年２.
017700     MOVE 初検月Ｗ(2)       TO 初検月２.
017710     MOVE 初検日Ｗ(2)       TO 初検日２.
017720     MOVE 開始年Ｗ(2)       TO 開始年２.
017730     MOVE 開始月Ｗ(2)       TO 開始月２.
017740     MOVE 開始日Ｗ(2)       TO 開始日２.
017750     MOVE 終了年Ｗ(2)       TO 終了年２.
017760     MOVE 終了月Ｗ(2)       TO 終了月２.
017770     MOVE 終了日Ｗ(2)       TO 終了日２.
017780     MOVE 実日数Ｗ(2)       TO 実日数２.
017790     MOVE 治癒チェックＷ(2) TO 治癒チェック２.
017800     MOVE 中止チェックＷ(2) TO 中止チェック２.
017810     MOVE 転医チェックＷ(2) TO 転医チェック２.
017820*     MOVE 転帰Ｗ(2)         TO 転帰２.
017830*     IF 負傷年Ｗ(2) NOT = ZERO
017840*        MOVE "."            TO 区切２１ 区切２２
017850*     END-IF.
017860*     IF 初検年Ｗ(2) NOT = ZERO
017870*        MOVE "."            TO 区切２３ 区切２４
017880*     END-IF.
017890*     IF 開始年Ｗ(2) NOT = ZERO
017900*        MOVE "."            TO 区切２５ 区切２６
017910*     END-IF.
017920*     IF 終了年Ｗ(2) NOT = ZERO
017930*        MOVE "."            TO 区切２７ 区切２８
017940*     END-IF.
017950**********
017960* ３部位 *
017970**********
017980     MOVE 負傷名Ｗ(3)       TO 負傷名３.
017990     MOVE 負傷年Ｗ(3)       TO 負傷年３.
018000     MOVE 負傷月Ｗ(3)       TO 負傷月３.
018010     MOVE 負傷日Ｗ(3)       TO 負傷日３.
018020     MOVE 初検年Ｗ(3)       TO 初検年３.
018030     MOVE 初検月Ｗ(3)       TO 初検月３.
018040     MOVE 初検日Ｗ(3)       TO 初検日３.
018050     MOVE 開始年Ｗ(3)       TO 開始年３.
018060     MOVE 開始月Ｗ(3)       TO 開始月３.
018070     MOVE 開始日Ｗ(3)       TO 開始日３.
018080     MOVE 終了年Ｗ(3)       TO 終了年３.
018090     MOVE 終了月Ｗ(3)       TO 終了月３.
018100     MOVE 終了日Ｗ(3)       TO 終了日３.
018110     MOVE 実日数Ｗ(3)       TO 実日数３.
018120     MOVE 治癒チェックＷ(3) TO 治癒チェック３.
018130     MOVE 中止チェックＷ(3) TO 中止チェック３.
018140     MOVE 転医チェックＷ(3) TO 転医チェック３.
018150*     MOVE 転帰Ｗ(3)         TO 転帰３.
018160*     IF 負傷年Ｗ(3) NOT = ZERO
018170*        MOVE "."            TO 区切３１ 区切３２
018180*     END-IF.
018190*     IF 初検年Ｗ(3) NOT = ZERO
018200*        MOVE "."            TO 区切３３ 区切３４
018210*     END-IF.
018220*     IF 開始年Ｗ(3) NOT = ZERO
018230*        MOVE "."            TO 区切３５ 区切３６
018240*     END-IF.
018250*     IF 終了年Ｗ(3) NOT = ZERO
018260*        MOVE "."            TO 区切３７ 区切３８
018270*     END-IF.
018280**********
018290* ４部位 *
018300**********
018310     MOVE 負傷名Ｗ(4)       TO 負傷名４.
018320     MOVE 負傷年Ｗ(4)       TO 負傷年４.
018330     MOVE 負傷月Ｗ(4)       TO 負傷月４.
018340     MOVE 負傷日Ｗ(4)       TO 負傷日４.
018350     MOVE 初検年Ｗ(4)       TO 初検年４.
018360     MOVE 初検月Ｗ(4)       TO 初検月４.
018370     MOVE 初検日Ｗ(4)       TO 初検日４.
018380     MOVE 開始年Ｗ(4)       TO 開始年４.
018390     MOVE 開始月Ｗ(4)       TO 開始月４.
018400     MOVE 開始日Ｗ(4)       TO 開始日４.
018410     MOVE 終了年Ｗ(4)       TO 終了年４.
018420     MOVE 終了月Ｗ(4)       TO 終了月４.
018430     MOVE 終了日Ｗ(4)       TO 終了日４.
018440     MOVE 実日数Ｗ(4)       TO 実日数４.
018450     MOVE 治癒チェックＷ(4) TO 治癒チェック４.
018460     MOVE 中止チェックＷ(4) TO 中止チェック４.
018470     MOVE 転医チェックＷ(4) TO 転医チェック４.
018480*     MOVE 転帰Ｗ(4)         TO 転帰４.
018490*     IF 負傷年Ｗ(4) NOT = ZERO
018500*        MOVE "."            TO 区切４１ 区切４２
018510*     END-IF.
018520*     IF 初検年Ｗ(4) NOT = ZERO
018530*        MOVE "."            TO 区切４３ 区切４４
018540*     END-IF.
018550*     IF 開始年Ｗ(4) NOT = ZERO
018560*        MOVE "."            TO 区切４５ 区切４６
018570*     END-IF.
018580*     IF 終了年Ｗ(4) NOT = ZERO
018590*        MOVE "."            TO 区切４７ 区切４８
018600*     END-IF.
018610**********
018620* ５部位 *
018630**********
018640     MOVE 負傷名Ｗ(5)       TO 負傷名５.
018650     MOVE 負傷年Ｗ(5)       TO 負傷年５.
018660     MOVE 負傷月Ｗ(5)       TO 負傷月５.
018670     MOVE 負傷日Ｗ(5)       TO 負傷日５.
018680     MOVE 初検年Ｗ(5)       TO 初検年５.
018690     MOVE 初検月Ｗ(5)       TO 初検月５.
018700     MOVE 初検日Ｗ(5)       TO 初検日５.
018710     MOVE 開始年Ｗ(5)       TO 開始年５.
018720     MOVE 開始月Ｗ(5)       TO 開始月５.
018730     MOVE 開始日Ｗ(5)       TO 開始日５.
018740     MOVE 終了年Ｗ(5)       TO 終了年５.
018750     MOVE 終了月Ｗ(5)       TO 終了月５.
018760     MOVE 終了日Ｗ(5)       TO 終了日５.
018770     MOVE 実日数Ｗ(5)       TO 実日数５.
018780     MOVE 治癒チェックＷ(5) TO 治癒チェック５.
018790     MOVE 中止チェックＷ(5) TO 中止チェック５.
018800     MOVE 転医チェックＷ(5) TO 転医チェック５.
018810*     MOVE 転帰Ｗ(5)         TO 転帰５.
018820*     IF 負傷年Ｗ(5) NOT = ZERO
018830*        MOVE "."            TO 区切５１ 区切５２
018840*     END-IF.
018850*     IF 初検年Ｗ(5) NOT = ZERO
018860*        MOVE "."            TO 区切５３ 区切５４
018870*     END-IF.
018880*     IF 開始年Ｗ(5) NOT = ZERO
018890*        MOVE "."            TO 区切５５ 区切５６
018900*     END-IF.
018910*     IF 終了年Ｗ(5) NOT = ZERO
018920*        MOVE "."            TO 区切５７ 区切５８
018930*     END-IF.
018940**************
018950* 経過セット *
018960**************
018970     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018980***             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
018990             UNTIL ( 部位ＣＮＴ > 5 )
019000**         MOVE 部位ＣＮＴＷ(部位ＣＮＴ)   TO 経過部位ＣＮＴ(部位ＣＮＴ)
019010**         MOVE 部位区切Ｗ(部位ＣＮＴ)     TO 部位区切(部位ＣＮＴ)
019020         MOVE 印刷経過略称Ｗ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
019030     END-PERFORM.
019040*****************************************
019050*     新規・継続チェックについて        *
019060*   ●新規...初検有り ●継続...初検なし *
019070*****************************************
019080     MOVE 新規チェックＷ    TO 新規チェック.
019090     MOVE 継続チェックＷ    TO 継続チェック.
019100*     MOVE 請求区分Ｗ        TO  請求区分.
019110********************
019120* 料金データセット *
019130********************
019140*    ****************************************************************
019150*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
019160*    ****************************************************************
019170     MOVE 初検料ＷＲ                   TO  初検料.
019180     MOVE 時間外チェックＷ             TO  時間外チェック.
019190     MOVE 休日チェックＷ               TO  休日チェック.
019200     MOVE 深夜チェックＷ               TO  深夜チェック.
019210*     MOVE 初検加算内容Ｗ               TO  初検加算内容.
019220     MOVE 初検加算料ＷＲ               TO  初検加算料.
      *     IF 施術和暦年月ＷＲ >= 42006
      *         MOVE NC"支援料"               TO  初検時相談料ＣＭ
      *         MOVE NC"円"                   TO  初検時相談料円ＣＭ
      *     END-IF.
           MOVE 初検時相談料ＷＲ             TO  初検時相談料.
           IF (時間外チェックＷ NOT = SPACE) OR (深夜チェックＷ NOT = SPACE) OR
              (休日チェックＷ NOT = SPACE)
              MOVE 初検加算時Ｗ                 TO  初検加算時
              MOVE 初検加算区切Ｗ               TO  初検加算区切
              MOVE 初検加算分Ｗ                 TO  初検加算分
           END-IF.
019230     MOVE 再検料ＷＲ                   TO  再検料.
019240     MOVE 往療距離ＷＲ                 TO  往療距離.
019250     MOVE 往療回数ＷＲ                 TO  往療回数.
019260     MOVE 往療料ＷＲ                   TO  往療料.
019270     MOVE 夜間チェックＷ               TO  夜間チェック.
019280     MOVE 難路チェックＷ               TO  難路チェック.
019290*     MOVE 往療深夜チェックＷ           TO  往療深夜チェック.
019300     MOVE 暴風雨雪チェックＷ           TO  暴風雨雪チェック.
019310     MOVE 往療加算料ＷＲ               TO  往療加算料.
      */金属副子・運動後療の変更・追加/1805
           IF ( 施術和暦年月ＷＲ < 43006 )
018050        MOVE 大チェックＷ              TO  大チェック
018060        MOVE 中チェックＷ              TO  中チェック
018070        MOVE 小チェックＷ              TO  小チェック
           ELSE
              MOVE ALL NC"＝"                TO  金属訂正
           END-IF.
      *     IF ( 施術和暦年月ＷＲ >= 43006 ) AND ( 金属副子加算料ＷＲ NOT = ZERO )
      *        MOVE 金属回数Ｗ                TO  金属回数
      *        MOVE NC"回"                    TO  金属回
      *     END-IF.
019350*     MOVE 金属大Ｗ                     TO  金属大.
019360*     MOVE 金属中Ｗ                     TO  金属中.
019370*     MOVE 金属小Ｗ                     TO  金属小.
019380     MOVE 金属副子加算料ＷＲ           TO  金属副子加算料.
019390     MOVE 施術情報提供料ＷＲ           TO  施術情報提供料.
019400     MOVE 小計Ｗ                       TO  小計.
019410********************
019420* 初回処置料セット *
019430********************
019440     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
019450***             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
019460             UNTIL ( 部位ＣＮＴ > 5 )
019470         MOVE 初回処置料ＷＲ(部位ＣＮＴ) TO 初回処置料(部位ＣＮＴ)
019480     END-PERFORM.
019490     MOVE 初回処置料合計Ｗ         TO 初回処置料合計.
019500*
019510     MOVE 施療料チェックＷ            TO 施療料チェック.
019520     MOVE 整復料チェックＷ            TO 整復料チェック.
019530     MOVE 固定料チェックＷ            TO 固定料チェック.
019540******************
019550* 基本料金セット *
019560******************
019570*     MOVE 冷罨法単価Ｗ                TO  冷罨法単価.
019580*     MOVE 温罨法単価Ｗ                TO  温罨法単価.
019590*     MOVE 電療単価Ｗ                  TO  電療単価.
019600********************
019610* 逓減毎料金セット *
019620********************
019630*    **********
019640*    * １部位 *
019650*    **********
019660     MOVE 後療単価１ＷＲ             TO 後療単価１.
019670     MOVE 後療回数１ＷＲ             TO 後療回数１.
019680     MOVE 後療料１ＷＲ               TO 後療料１.
019690     MOVE 冷罨法回数１ＷＲ           TO 冷罨法回数１.
019700     MOVE 冷罨法料１ＷＲ             TO 冷罨法料１.
019710     MOVE 温罨法回数１ＷＲ           TO 温罨法回数１.
019720     MOVE 温罨法料１ＷＲ             TO 温罨法料１.
019730     MOVE 電療回数１ＷＲ             TO 電療回数１.
019740     MOVE 電療料１ＷＲ               TO 電療料１.
019750     MOVE 小計１ＷＲ                 TO 小計１.
019760     IF 長期逓減率１ＷＲ NOT = ZERO
019770         COMPUTE 長期逓減率１ = 長期逓減率１ＷＲ / 100
019780     END-IF.
019790     MOVE 長期込小計１ＷＲ           TO 長期込小計１.
019800*    **********
019810*    * ２部位 *
019820*    **********
019830     MOVE 後療単価２ＷＲ             TO 後療単価２.
019840     MOVE 後療回数２ＷＲ             TO 後療回数２.
019850     MOVE 後療料２ＷＲ               TO 後療料２.
019860     MOVE 冷罨法回数２ＷＲ           TO 冷罨法回数２.
019870     MOVE 冷罨法料２ＷＲ             TO 冷罨法料２.
019880     MOVE 温罨法回数２ＷＲ           TO 温罨法回数２.
019890     MOVE 温罨法料２ＷＲ             TO 温罨法料２.
019900     MOVE 電療回数２ＷＲ             TO 電療回数２.
019910     MOVE 電療料２ＷＲ               TO 電療料２.
019920     MOVE 小計２ＷＲ                 TO 小計２.
019930     IF 長期逓減率２ＷＲ NOT = ZERO
019940         COMPUTE 長期逓減率２ = 長期逓減率２ＷＲ / 100
019950     END-IF.
019960     MOVE 長期込小計２ＷＲ           TO 長期込小計２.
019970*    ****************
019980*    * ３部位／８割 *
019990*    ****************
020000     MOVE 後療単価３８ＷＲ             TO 後療単価３８.
020010     MOVE 後療回数３８ＷＲ             TO 後療回数３８.
020020     MOVE 後療料３８ＷＲ               TO 後療料３８.
020030     MOVE 冷罨法回数３８ＷＲ           TO 冷罨法回数３８.
020040     MOVE 冷罨法料３８ＷＲ             TO 冷罨法料３８.
020050     MOVE 温罨法回数３８ＷＲ           TO 温罨法回数３８.
020060     MOVE 温罨法料３８ＷＲ             TO 温罨法料３８.
020070     MOVE 電療回数３８ＷＲ             TO 電療回数３８.
020080     MOVE 電療料３８ＷＲ               TO 電療料３８.
020090     MOVE 小計３８ＷＲ                 TO 小計３８.
020100     MOVE 多部位込小計３８ＷＲ         TO 多部位込小計３８.
020110     IF 長期逓減率３８ＷＲ NOT = ZERO
020120         COMPUTE 長期逓減率３８ = 長期逓減率３８ＷＲ / 100
020130     END-IF.
020140     MOVE 長期込小計３８ＷＲ           TO 長期込小計３８.
      */新用紙は訂正をしない/131008
           IF 一般レセＷ < 2
      */ 逓減率 0.7→0.6 /42505
               IF (施術和暦年月ＷＲ >= 42505)
                  MOVE "60"                  TO 逓減３８
                  MOVE "0.6"                 TO 多部位３８
                  MOVE "==="                 TO 逓減訂正３８ 多部位訂正３８
               END-IF
           END-IF.
020150*    ****************
020160*    * ３部位／10割 *
020170*    ****************
020180     MOVE 逓減開始月３０ＷＲ           TO 逓減開始月３０.
020190     MOVE 逓減開始日３０ＷＲ           TO 逓減開始日３０.
020200     MOVE 後療単価３０ＷＲ             TO 後療単価３０.
020210     MOVE 後療回数３０ＷＲ             TO 後療回数３０.
020220     MOVE 後療料３０ＷＲ               TO 後療料３０.
020230     MOVE 冷罨法回数３０ＷＲ           TO 冷罨法回数３０.
020240     MOVE 冷罨法料３０ＷＲ             TO 冷罨法料３０.
020250     MOVE 温罨法回数３０ＷＲ           TO 温罨法回数３０.
020260     MOVE 温罨法料３０ＷＲ             TO 温罨法料３０.
020270     MOVE 電療回数３０ＷＲ             TO 電療回数３０.
020280     MOVE 電療料３０ＷＲ               TO 電療料３０.
020290     MOVE 小計３０ＷＲ                 TO 小計３０.
020300     IF 長期逓減率３０ＷＲ NOT = ZERO
020310         COMPUTE 長期逓減率３０ = 長期逓減率３０ＷＲ / 100
020320     END-IF.
020330     MOVE 長期込小計３０ＷＲ           TO 長期込小計３０.
      */コメント/131010
020340**    ****************
020350**    * ４部位／５割 *
020360**    ****************
020370*     MOVE 後療単価４５ＷＲ             TO 後療単価４５.
020380*     MOVE 後療回数４５ＷＲ             TO 後療回数４５.
020390*     MOVE 後療料４５ＷＲ               TO 後療料４５.
020400*     MOVE 冷罨法回数４５ＷＲ           TO 冷罨法回数４５.
020410*     MOVE 冷罨法料４５ＷＲ             TO 冷罨法料４５.
020420*     MOVE 温罨法回数４５ＷＲ           TO 温罨法回数４５.
020430*     MOVE 温罨法料４５ＷＲ             TO 温罨法料４５.
020440*     MOVE 電療回数４５ＷＲ             TO 電療回数４５.
020450*     MOVE 電療料４５ＷＲ               TO 電療料４５.
020460*     MOVE 小計４５ＷＲ                 TO 小計４５.
020470*     MOVE 多部位込小計４５ＷＲ         TO 多部位込小計４５.
020480*     IF 長期逓減率４５ＷＲ NOT = ZERO
020490*         COMPUTE 長期逓減率４５ = 長期逓減率４５ＷＲ / 100
020500*     END-IF.
020510*     MOVE 長期込小計４５ＷＲ           TO 長期込小計４５.
020520*    ****************
020530*    * ４部位／８割 *
020540*    ****************
020550     MOVE 逓減開始月４８ＷＲ           TO 逓減開始月４８.
020560     MOVE 逓減開始日４８ＷＲ           TO 逓減開始日４８.
020570     MOVE 後療単価４８ＷＲ             TO 後療単価４８.
020580     MOVE 後療回数４８ＷＲ             TO 後療回数４８.
020590     MOVE 後療料４８ＷＲ               TO 後療料４８.
020600     MOVE 冷罨法回数４８ＷＲ           TO 冷罨法回数４８.
020610     MOVE 冷罨法料４８ＷＲ             TO 冷罨法料４８.
020620     MOVE 温罨法回数４８ＷＲ           TO 温罨法回数４８.
020630     MOVE 温罨法料４８ＷＲ             TO 温罨法料４８.
020640     MOVE 電療回数４８ＷＲ             TO 電療回数４８.
020650     MOVE 電療料４８ＷＲ               TO 電療料４８.
020660     MOVE 小計４８ＷＲ                 TO 小計４８.
020670     MOVE 多部位込小計４８ＷＲ         TO 多部位込小計４８.
020680     IF 長期逓減率４８ＷＲ NOT = ZERO
020690         COMPUTE 長期逓減率４８ = 長期逓減率４８ＷＲ / 100
020700     END-IF.
020710     MOVE 長期込小計４８ＷＲ           TO 長期込小計４８.
      */新用紙は訂正をしない/131008
           IF 一般レセＷ < 2
      */ 逓減率 0.7→0.6 /42505
               IF (施術和暦年月ＷＲ >= 42505)
                  MOVE "60"                  TO 逓減４８
                  MOVE "0.6"                 TO 多部位４８
                  MOVE "==="                 TO 逓減訂正４８ 多部位訂正４８
               END-IF
           END-IF.
020720*    ****************
020730*    * ４部位／10割 *
020740*    ****************
020750     MOVE 逓減開始月４０ＷＲ           TO 逓減開始月４０.
020760     MOVE 逓減開始日４０ＷＲ           TO 逓減開始日４０.
020770     MOVE 後療単価４０ＷＲ             TO 後療単価４０.
020780     MOVE 後療回数４０ＷＲ             TO 後療回数４０.
020790     MOVE 後療料４０ＷＲ               TO 後療料４０.
020800     MOVE 冷罨法回数４０ＷＲ           TO 冷罨法回数４０.
020810     MOVE 冷罨法料４０ＷＲ             TO 冷罨法料４０.
020820     MOVE 温罨法回数４０ＷＲ           TO 温罨法回数４０.
020830     MOVE 温罨法料４０ＷＲ             TO 温罨法料４０.
020840     MOVE 電療回数４０ＷＲ             TO 電療回数４０.
020850     MOVE 電療料４０ＷＲ               TO 電療料４０.
020860     MOVE 小計４０ＷＲ                 TO 小計４０.
020870     IF 長期逓減率４０ＷＲ NOT = ZERO
020880         COMPUTE 長期逓減率４０ = 長期逓減率４０ＷＲ / 100
020890     END-IF.
020900     MOVE 長期込小計４０ＷＲ           TO 長期込小計４０.
020910*
020920*↓***********************************************************************
020930* ５部位／2.5割の印字は必要ない。
020940*------------------------------------------------------------------------*
020950*    *****************
020960*    * ５部位／2.5割 *
020970*    *****************
020980*     MOVE 後療単価５２ＷＲ             TO 後療単価５２.
020990*     MOVE 後療回数５２ＷＲ             TO 後療回数５２.
021000*     MOVE 後療料５２ＷＲ               TO 後療料５２.
021010*     MOVE 冷罨法回数５２ＷＲ           TO 冷罨法回数５２.
021020*     MOVE 冷罨法料５２ＷＲ             TO 冷罨法料５２.
021030*     MOVE 温罨法回数５２ＷＲ           TO 温罨法回数５２.
021040*     MOVE 温罨法料５２ＷＲ             TO 温罨法料５２.
021050*     MOVE 電療回数５２ＷＲ             TO 電療回数５２.
021060*     MOVE 電療料５２ＷＲ               TO 電療料５２.
021070*     MOVE 小計５２ＷＲ                 TO 小計５２.
021080*     MOVE 多部位込小計５２ＷＲ         TO 多部位込小計５２.
021090*     IF 長期逓減率５２ＷＲ NOT = ZERO
021100*         COMPUTE 長期逓減率５２ = 長期逓減率５２ＷＲ / 100
021110*     END-IF.
021120*     MOVE 長期込小計５２ＷＲ           TO 長期込小計５２.
021130*↑***********************************************************************
      */コメント/131010
021140**    ****************
021150**    * ５部位／５割 *
021160**    ****************
021170*     IF 小計５５ＷＲ NOT = ZERO
021180*        MOVE "5)33 "                      TO 逓減固定５５
021190*        MOVE "0.33"                       TO 多部位率５５
021200*        MOVE 逓減開始月５５ＷＲ           TO 逓減開始月５５
021210*        MOVE 逓減開始日５５ＷＲ           TO 逓減開始日５５
021220*        MOVE 後療単価５５ＷＲ             TO 後療単価５５
021230*        MOVE 後療回数５５ＷＲ             TO 後療回数５５
021240*        MOVE 後療料５５ＷＲ               TO 後療料５５
021250*        MOVE 冷罨法回数５５ＷＲ           TO 冷罨法回数５５
021260*        MOVE 冷罨法料５５ＷＲ             TO 冷罨法料５５
021270*        MOVE 温罨法回数５５ＷＲ           TO 温罨法回数５５
021280*        MOVE 温罨法料５５ＷＲ             TO 温罨法料５５
021290*        MOVE 電療回数５５ＷＲ             TO 電療回数５５
021300*        MOVE 電療料５５ＷＲ               TO 電療料５５
021310*        MOVE 小計５５ＷＲ                 TO 小計５５
021320*        MOVE 多部位込小計５５ＷＲ         TO 多部位込小計５５
021330*        IF 長期逓減率５５ＷＲ NOT = ZERO
021340*            COMPUTE 長期逓減率５５ = 長期逓減率５５ＷＲ / 100
021350*        END-IF
021360*        MOVE 長期込小計５５ＷＲ           TO 長期込小計５５
021370*     END-IF.
021380*    ****************
021390*    * ５部位／８割 *
021400*    ****************
021220     MOVE SPACE TO 部位５Ｗ.
021230     IF 小計５８ＷＲ NOT = ZERO
      */日付
021560        MOVE 逓減開始月５８ＷＲ           TO 逓減開始月５Ｗ
              MOVE "月"                         TO 月ＣＭ
021570        MOVE 逓減開始日５８ＷＲ           TO 逓減開始日５Ｗ
              MOVE "日"                         TO 日ＣＭ
              MOVE "("                          TO 括弧１Ｗ
      */後療料
              IF 後療料５８ＷＲ NOT = ZERO
                  MOVE "("                      TO 括弧２Ｗ
021580            MOVE 後療単価５８ＷＲ         TO 後療単価５Ｗ
                  MOVE "x"                      TO 乗算記号１Ｗ
021590            MOVE 後療回数５８ＷＲ         TO 後療回数５Ｗ
                  MOVE "="                      TO イコール１Ｗ
021600            MOVE 後療料５８ＷＲ           TO 後療料５Ｗ
                  MOVE ")"                      TO 括弧３Ｗ
              END-IF
      */冷罨法
              IF 冷罨法料５８ＷＲ NOT = ZERO
                  MOVE "+"                      TO 加算記号１Ｗ
                  MOVE "("                      TO 括弧４Ｗ
                  COMPUTE 冷罨法単価５Ｗ        =  冷罨法料５８ＷＲ / 冷罨法回数５８ＷＲ
                  MOVE "x"                      TO 乗算記号２Ｗ
021610            MOVE 冷罨法回数５８ＷＲ       TO 冷罨法回数５Ｗ
                  MOVE "="                      TO イコール２Ｗ
021620            MOVE 冷罨法料５８ＷＲ         TO 冷罨法料５Ｗ
                  MOVE ")"                      TO 括弧５Ｗ
              END-IF
      */温罨法
              IF 温罨法料５８ＷＲ NOT = ZERO
                  MOVE "+"                      TO 加算記号２Ｗ
                  MOVE "("                      TO 括弧６Ｗ
                  COMPUTE 温罨法単価５Ｗ        =  温罨法料５８ＷＲ / 温罨法回数５８ＷＲ
                  MOVE "x"                      TO 乗算記号３Ｗ
021630            MOVE 温罨法回数５８ＷＲ       TO 温罨法回数５Ｗ
                  MOVE "="                      TO イコール３Ｗ
021640            MOVE 温罨法料５８ＷＲ         TO 温罨法料５Ｗ
                  MOVE ")"                      TO 括弧７Ｗ
              END-IF
      */電療料
              IF 電療料５８ＷＲ NOT = ZERO
                  MOVE "+"                      TO 加算記号３Ｗ
                  MOVE "("                      TO 括弧８Ｗ
                  COMPUTE 電療単価５Ｗ          =  電療料５８ＷＲ / 電療回数５８ＷＲ
                  MOVE "x"                      TO 乗算記号４Ｗ
021650            MOVE 電療回数５８ＷＲ         TO 電療回数５Ｗ
                  MOVE "="                      TO イコール４Ｗ
021660            MOVE 電療料５８ＷＲ           TO 電療料５Ｗ
                  MOVE ")"                      TO 括弧９Ｗ
              END-IF
      *
              MOVE ")"                          TO 括弧１０Ｗ
      */多部位
              MOVE "x"                          TO 乗算記号５Ｗ
      */ 逓減率 0.7→0.6 /42505
              IF (施術和暦年月ＷＲ >= 42505)
021290           MOVE "0.6 "                    TO 多部位率５Ｗ
              ELSE
021290           MOVE "0.7 "                    TO 多部位率５Ｗ
              END-IF
      */長期
021680        IF 長期逓減率５８ＷＲ NOT = ZERO
                 MOVE "x"                       TO 乗算記号６Ｗ
021690           COMPUTE 長期逓減率５Ｗ = 長期逓減率５８ＷＲ / 100
021700        END-IF
      */合計
              MOVE "="                          TO イコール５Ｗ
021710        MOVE 長期込小計５８ＷＲ           TO 長期込小計５Ｗ
021720        MOVE 部位５Ｗ                     TO 部位５８
021490     END-IF.
021620*    ****************
021630*    * ５部位／10割 *
021640*    ****************
021530     MOVE SPACE TO 部位５Ｗ.
021540     IF 小計５０ＷＲ NOT = ZERO
      */日付
021560        MOVE 逓減開始月５０ＷＲ           TO 逓減開始月５Ｗ
              MOVE "月"                         TO 月ＣＭ
021570        MOVE 逓減開始日５０ＷＲ           TO 逓減開始日５Ｗ
              MOVE "日"                         TO 日ＣＭ
              MOVE "("                          TO 括弧１Ｗ
      */後療料
              IF 後療料５０ＷＲ NOT = ZERO
                  MOVE "("                      TO 括弧２Ｗ
021580            MOVE 後療単価５０ＷＲ         TO 後療単価５Ｗ
                  MOVE "x"                      TO 乗算記号１Ｗ
021590            MOVE 後療回数５０ＷＲ         TO 後療回数５Ｗ
                  MOVE "="                      TO イコール１Ｗ
021600            MOVE 後療料５０ＷＲ           TO 後療料５Ｗ
                  MOVE ")"                      TO 括弧３Ｗ
              END-IF
      */冷罨法
              IF 冷罨法料５０ＷＲ NOT = ZERO
                  MOVE "+"                      TO 加算記号１Ｗ
                  MOVE "("                      TO 括弧４Ｗ
                  COMPUTE 冷罨法単価５Ｗ        =  冷罨法料５０ＷＲ / 冷罨法回数５０ＷＲ
                  MOVE "x"                      TO 乗算記号２Ｗ
021610            MOVE 冷罨法回数５０ＷＲ       TO 冷罨法回数５Ｗ
                  MOVE "="                      TO イコール２Ｗ
021620            MOVE 冷罨法料５０ＷＲ         TO 冷罨法料５Ｗ
                  MOVE ")"                      TO 括弧５Ｗ
              END-IF
      */温罨法
              IF 温罨法料５０ＷＲ NOT = ZERO
                  MOVE "+"                      TO 加算記号２Ｗ
                  MOVE "("                      TO 括弧６Ｗ
                  COMPUTE 温罨法単価５Ｗ        =  温罨法料５０ＷＲ / 温罨法回数５０ＷＲ
                  MOVE "x"                      TO 乗算記号３Ｗ
021630            MOVE 温罨法回数５０ＷＲ       TO 温罨法回数５Ｗ
                  MOVE "="                      TO イコール３Ｗ
021640            MOVE 温罨法料５０ＷＲ         TO 温罨法料５Ｗ
                  MOVE ")"                      TO 括弧７Ｗ
              END-IF
      */電療料
              IF 電療料５０ＷＲ NOT = ZERO
                  MOVE "+"                      TO 加算記号３Ｗ
                  MOVE "("                      TO 括弧８Ｗ
                  COMPUTE 電療単価５Ｗ          =  電療料５０ＷＲ / 電療回数５０ＷＲ
                  MOVE "x"                      TO 乗算記号４Ｗ
021650            MOVE 電療回数５０ＷＲ         TO 電療回数５Ｗ
                  MOVE "="                      TO イコール４Ｗ
021660            MOVE 電療料５０ＷＲ           TO 電療料５Ｗ
                  MOVE ")"                      TO 括弧９Ｗ
              END-IF
      *
              MOVE ")"                          TO 括弧１０Ｗ
      */多部位
      *        乗算記号５Ｗ 多部位率５Ｗ
      */長期
021680        IF 長期逓減率５０ＷＲ NOT = ZERO
                 MOVE "x"                       TO 乗算記号６Ｗ
021690           COMPUTE 長期逓減率５Ｗ = 長期逓減率５０ＷＲ / 100
021700        END-IF
      */合計
              MOVE "="                          TO イコール５Ｗ
021710        MOVE 長期込小計５０ＷＲ           TO 長期込小計５Ｗ
021720        MOVE 部位５Ｗ                     TO 部位５０
021730     END-IF.
021850*
021860     MOVE 適用１Ｗ                        TO 適用１.
021870     MOVE 適用２Ｗ                        TO 適用２.
      *
      */金属副子・運動後療の変更・追加/1805
           IF ( 施術和暦年月ＷＲ >= 43006 )
              INITIALIZE 連金運－キー
019550        MOVE 施術和暦ＷＲ TO 連金運－施術和暦
019560        MOVE 施術年ＷＲ   TO 連金運－施術年
019570        MOVE 施術月ＷＲ   TO 連金運－施術月
019580        MOVE 患者番号ＷＲ TO 連金運－患者番号
019590        MOVE 枝番ＷＲ     TO 連金運－枝番
              MOVE 保険種別ＷＲ TO 連金運－保険種別
              MOVE 24           TO 連金運－会コード
              MOVE ZERO         TO 連金運－用紙種別
              CALL "KINUNRYO"
              CANCEL "KINUNRYO"
              MOVE 連金運－金属副子ＣＭ           TO 金属副子ＣＭ
              MOVE 連金運－運動後療ＣＭ           TO 運動後療ＣＭ
              IF ( 金属副子加算料ＷＲ NOT = ZERO )
                 MOVE 金属副子ＣＭ                TO 金属副子
              END-IF
              IF ( 運動料Ｗ NOT = ZERO )
                 MOVE 運動後療ＣＭ                TO 運動後療
              END-IF
           END-IF.
      *
021880     MOVE レセ－合計                      TO 合計.
           IF 連レ－保険種別 < 50
               MOVE レセ－一部負担金            TO 一部負担金
               MOVE レセ－請求金額              TO 請求金額
           ELSE
               MOVE レセ－受給者負担額          TO 一部負担金
               MOVE レセ－助成請求金額          TO 請求金額
           END-IF
021910*
021810*------------------------------------------------------------------------------------*
021820* 特別（助成レセなしで、本体レセにまとめる時、金額は助成込み・適用２に助成種別印字）
021830     IF 助成レセまとめフラグ = "YES"
021850         MOVE レセ－合計                  TO 合計
021860         MOVE レセ－受給者負担額          TO 一部負担金
021870*     / 引き算する/
021880         COMPUTE 請求金額 = レセ－合計 - レセ－受給者負担額
      */湯河原町の母子は負担額助成を記載する/130418
      */座間市の障害は負担額助成を記載する/
      */横浜川崎の乳児は負担額助成を記載する/170217
               IF ((受－助成種別 = 52) AND (受－費用負担者番号助成 = "85140630")) OR
                  ((受－助成種別 = 53) AND (受－費用負担者番号助成 = "80140171")) OR
                  ((受－助成種別 = 55) AND (受－費用負担者番号助成(1:5) = "81144" OR "81145"))
019830             MOVE レセ－一部負担金 TO 一部負担金
019840             MOVE レセ－請求金額   TO 請求金額
               END-IF
021890*
021900*/深＿夜の空白にストリングしてしまうためNOT SPACEの時は最後に転記する。
021910*/初険加算が３回の時は余白無く転記される。
021920         IF 助成種別略称Ｗ NOT = SPACE
021930            IF 適用２Ｗ NOT = SPACE
021940                MOVE SPACE TO 助成種別略称Ｗ２
021950                STRING NC"※"             DELIMITED BY SIZE
021960                       助成種別略称Ｗ     DELIMITED BY SPACE
021970                       INTO 助成種別略称Ｗ２
021980                END-STRING
021990*                MOVE 助成種別略称Ｗ２ TO 適用２(35:4)
021990                MOVE 助成種別略称Ｗ２ TO 適用２(28:4)
022000            ELSE
022010                STRING 適用２Ｗ           DELIMITED BY SPACE
022020                       NC"※"             DELIMITED BY SIZE
022030                       助成種別略称Ｗ     DELIMITED BY SPACE
022040                       INTO 適用２
022050                END-STRING
022060            END-IF
022070         END-IF
022080*
022090     END-IF.
      */コメント/131010
021920**------------------------------------------------------------------------------------*
021930** 平成14年6月から4部位目・5部位目の逓減率が45→33に変更。
021940**
021950*     IF ( 小計５５ＷＲ NOT = ZERO ) AND
021960*        ( 施術和暦年月ＷＲ < 41406 )
021970*        MOVE "5)45 "  TO 逓減固定５５
021980*        MOVE "0.45"   TO 多部位率５５
021990*     END-IF.
022000**------------------------------------------------------------------------------------*
022010**********************
022020* 施術所データセット *
022030**********************
022040     MOVE 柔整師番号Ｗ           TO 柔整師番号.
022050*     MOVE 定額制受理番号Ｗ       TO 定額制受理番号.
022060     MOVE 施術所郵便番号１Ｗ     TO 施術所郵便番号１.
022070     MOVE 施術所郵便番号２Ｗ     TO 施術所郵便番号２.
022080*
022090     MOVE 施術所住所１Ｗ         TO 施術所住所１.
022100     MOVE 施術所住所２Ｗ         TO 施術所住所２.
022110*     MOVE 施術所住所３Ｗ         TO 施術所住所３.
022120*
022130     MOVE 印刷接骨師会会員番号Ｗ TO 接骨師会会員番号１
022140                                    接骨師会会員番号２.
022150     MOVE 代表者カナＷ           TO 代表者カナ.
022160     MOVE 代表者名Ｗ             TO 代表者名.
022170     MOVE 施術所電話番号Ｗ       TO 施術所電話番号.
022180     MOVE 接骨院名Ｗ             TO 接骨院名.
           MOVE 都道府県ＪＩＳＷ       TO 都道府県番号.
022190*
022200*     MOVE 銀行名支店名Ｗ         TO 銀行名支店名.
022210*     MOVE 預金種別コメントＷ     TO 預金種別.
022220     MOVE 口座番号Ｗ             TO 口座番号.
022230     MOVE 口座名義人カナＷ       TO 口座名義人カナ.
022240     MOVE 口座名義人Ｗ           TO 口座名義人.
022250     MOVE 会長委任文１Ｗ         TO 会長委任文１.
022260     MOVE 会長委任文２Ｗ         TO 会長委任文２.
022260     MOVE 会長委任文３Ｗ         TO 会長委任文３.
           MOVE 金融機関名１Ｗ   TO 金融機関名１.
           MOVE 金融機関名２Ｗ   TO 金融機関名２.
           MOVE 金融機関名３Ｗ   TO 金融機関名３.
           MOVE 金融機関名４Ｗ   TO 金融機関名４.
           MOVE 支店名１Ｗ       TO 支店名１.
           MOVE 支店名２Ｗ       TO 支店名２.
           MOVE 支店名３Ｗ       TO 支店名３.
           MOVE 支店名４Ｗ       TO 支店名４.
           MOVE 振込チェックＷ   TO 振込チェック.
           MOVE 普通チェックＷ   TO 普通チェック.
           MOVE 当座チェックＷ   TO 当座チェック.
           MOVE 銀行チェックＷ   TO 銀行チェック.
           MOVE 金庫チェックＷ   TO 金庫チェック.
           MOVE 農協チェックＷ   TO 農協チェック.
           MOVE 本店チェックＷ   TO 本店チェック.
           MOVE 支店チェックＷ   TO 支店チェック.
           MOVE 本支所チェックＷ TO 本支所チェック.
022270*
022271*     MOVE NC"再委任は承諾します。" TO 委任分補足.
022272*
022280****
022290*
022300* / 柔整師・患者委任日 /
      */元号修正/↓↓↓20190408
037370     IF 施術和暦Ｗ > 4
               MOVE 施術和暦Ｗ         TO 元－元号区分
037380         READ 元号マスタ
037390         NOT INVALID KEY
037400             MOVE 元－元号名称   TO 受理和暦
037410         END-READ
               MOVE "===="             TO 受理和暦訂正
           END-IF.
      */元号修正/↑↑↑20190408
022310     MOVE 柔整師年Ｗ             TO 受理年.
022320     MOVE 柔整師月Ｗ             TO 受理月.
022330     MOVE 柔整師日Ｗ             TO 受理日.
022340* ( 委任年月日 印刷するか )
022350     IF 連入－委任印刷  = ZERO
      */元号修正/↓↓↓20190408
037370         IF 施術和暦Ｗ > 4
                   MOVE 施術和暦Ｗ         TO 元－元号区分
037380             READ 元号マスタ
037390             NOT INVALID KEY
037400                 MOVE 元－元号名称   TO 委任和暦
037410             END-READ
                   MOVE "===="             TO 委任和暦訂正
               END-IF
      */元号修正/↑↑↑20190408
022360        MOVE 患者委任年Ｗ       TO 委任年
022370        MOVE 患者委任月Ｗ       TO 委任月
022380        MOVE 患者委任日Ｗ       TO 委任日
022390     END-IF.
022400*
022410* 施術ID
022420*
022430*     MOVE 県共済固定Ｗ TO 県共済固定.
022440     EVALUATE 保険種別ＷＲ
022450*     WHEN 1
022460*     WHEN 8
022470*         MOVE 県施術ＩＤＷ      TO 県施術ＩＤ
022480     WHEN 4
022490     WHEN 9
022500         MOVE 共済番号Ｗ        TO 共済番号
               MOVE 県共済固定Ｗ      TO 県共済固定
022510     END-EVALUATE.
022470     MOVE 県施術ＩＤＷ TO 県施術ＩＤ.
022500*     MOVE 共済番号Ｗ   TO 共済番号.
022520*
022530********************
022540* 欄外データセット *
022550********************
022560     MOVE 患者氏名Ｗ          TO 受診者氏名.
022570     STRING レセ管理西暦Ｗ       DELIMITED BY SPACE
022580            "-"                  DELIMITED BY SIZE
022590            施術月Ｗ             DELIMITED BY SPACE
022600            "-"                  DELIMITED BY SIZE
022610            接骨師会会員番号Ｗ   DELIMITED BY SPACE
022620            "-"                  DELIMITED BY SIZE
022630            患者コードＷＲ       DELIMITED BY SPACE
022640            INTO レセプト管理番号
022650     END-STRING.
022660     MOVE 総括表順番Ｗ        TO 総括表順番.
022670*
      */新用紙２以降は逓減率が空欄の為印刷する/131008
           IF 一般レセＷ >= 2
               PERFORM 計算マスタ取得
               MOVE 多部位逓減率３Ｗ TO 多部位逓減率３１ 多部位逓減率４１
               COMPUTE 多部位逓減率３２ = 多部位逓減率３Ｗ / 100
               COMPUTE 多部位逓減率４２ = 多部位逓減率３Ｗ / 100
           END-IF.
      */会住所の変更/131008
           IF 一般レセＷ < 2
               MOVE ALL "="                 TO 住所取消線
               MOVE "6-14 ラ・テラビル１Ｆ" TO 会新住所
           END-IF.
      *
      *     IF 一般レセＷ NOT = 1
      *         MOVE ALL NC"＝"      TO 取消線１ 取消線２
      *         MOVE "〒510-0075 三重県四日市市安島１丁目4-16 ＫＡＮＥＮＩビル７Ｆ　TEL  059-359-0333  FAX  059-359-0335"
      *                              TO 会住所
      *     END-IF.
022670*
022680******************
022690* 負担割合セット *
022700******************
022710*     MOVE 負担割合Ｗ             TO 負担割合.
022720*     MOVE 割合固定Ｗ             TO 割合固定.
022730*
022731*
022732* 特別コメント
022733*     MOVE 特別コメントＷ         TO 特別コメント.
022734*
022735*15/06廃止**     MOVE  NC"負担割合"          TO 給付割合訂正.
022736*
022737*
022740*****     PERFORM テスト印字処理.
022750*
      **/平成22年6月より、逓減率訂正印字/100617
      *     IF 施術和暦年月ＷＲ >= 42206
      *         MOVE "===" TO 訂正線３ 訂正線３２ 訂正線４ 訂正線４２
      *         MOVE "70"  TO 訂正逓減率３   訂正逓減率４
      *         MOVE "0.7" TO 訂正逓減率３２ 訂正逓減率４２
      *     END-IF.
022760*
022770*-------------------------------------------------------------------------*
022780*--- ※ レセ摘要再セットは、この印刷セットSECTION の最後にやること！ -----*
022790     PERFORM レセ摘要再セット.
022800*-------------------------------------------------------------------------*
022810*
022820*================================================================*
022830 項目初期化 SECTION.
022840*
022850     INITIALIZE 施術所情報Ｗ.
022860     INITIALIZE 受診者情報Ｗ.
022870     INITIALIZE 負傷情報Ｗ.
022880     INITIALIZE 備考情報Ｗ.
022890     INITIALIZE 料金１ＷＲ.
022900     INITIALIZE 料金２ＷＲ.
022910     INITIALIZE 料金３ＷＲ.
022930     INITIALIZE YAS6125P.
022920     MOVE SPACE TO YAS6125P.
021920*================================================================*
021930 基本情報取得 SECTION.
021920*================================================================*
023130*
           EVALUATE 公費種別ＷＲ
           WHEN 05
               MOVE 2          TO レセ－レセ種別
           WHEN OTHER
               MOVE 1          TO レセ－レセ種別
           END-EVALUATE.
019550     MOVE 施術和暦ＷＲ   TO レセ－施術和暦.
019560     MOVE 施術年ＷＲ     TO レセ－施術年.
019570     MOVE 施術月ＷＲ     TO レセ－施術月.
019580     MOVE 患者番号ＷＲ   TO レセ－患者番号.
019590     MOVE 枝番ＷＲ       TO レセ－枝番.
019600     READ レセプトＦ
019630     INVALID KEY
              MOVE SPACE     TO レセ－レコード
              INITIALIZE        レセ－レコード
           END-READ.
      *
028780     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
028790     MOVE 施術年ＷＲ         TO 受－施術年.
028800     MOVE 施術月ＷＲ         TO 受－施術月.
028810     MOVE 患者コードＷＲ     TO 受－患者コード.
028820     READ 受診者情報Ｆ
019630     INVALID KEY
              MOVE SPACE     TO 受－レコード
              INITIALIZE        受－レコード
           END-READ.
      *
027790     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
027800     MOVE 施術年ＷＲ         TO 負－施術年.
027810     MOVE 施術月ＷＲ         TO 負－施術月.
027820     MOVE 患者コードＷＲ     TO 負－患者コード.
027830     READ 負傷データＦ
019630     INVALID KEY
              MOVE SPACE     TO 負－レコード
              INITIALIZE        負－レコード
027870     NOT INVALID KEY
027900         MOVE 負－部位数                   TO 部位数Ｗ
           END-READ.
021940*
022940*================================================================*
022950 施術所情報取得 SECTION.
022960*
022970**************************************************
022980* 本院データを使用し、以下の情報を取得           *
022990* ● 柔整師番号.. 柔整師番号Ｗに格納             *
023000* ● 会員番号 ... 接骨師会会員番号Ｗに格納       *
023010* ● 代表者名 ... 代表者名Ｗに格納               *
023020* ● 住所1,2   ...施術所住所1,2Ｗに格納          *
023030* ● 電話番号 ... 施術所電話番号Ｗに格納         *
023040**************************************************
023050     MOVE ZERO  TO 施情－施術所番号.
023060     READ 施術所情報マスタ
023070     INVALID KEY
023080         CONTINUE
023090     NOT INVALID KEY
023130         MOVE 施情－新柔整師番号   TO 柔整師番号Ｗ
023150*
023160         MOVE   "あさひ・"             TO 接骨師会名Ｗ
023170         MOVE   施情－接骨師会会員番号 TO 接骨師会会員番号Ｗ
023180*
023190*** 共済・自衛官の時のみ、柔整師番号の編集をする。
023200         EVALUATE 保険種別ＷＲ
023210         WHEN 04
023220             PERFORM 共済番号セット
023230         WHEN 09
023240             PERFORM 自衛官番号セット
023250         END-EVALUATE
023260***
023270*
023280         MOVE 施情－郵便番号１        TO 施術所郵便番号１Ｗ
023290         MOVE 施情－郵便番号２        TO 施術所郵便番号２Ｗ
023300         MOVE 施情－代表者カナ        TO 代表者カナＷ
023310         MOVE 施情－代表者名          TO 代表者名Ｗ
023320         MOVE 施情－接骨院名          TO 接骨院名Ｗ
               MOVE 施情－都道府県ＪＩＳ    TO 都道府県ＪＩＳＷ
023330*
023340         MOVE 施情－住所１            TO 施術所住所１Ｗ
023350         MOVE 施情－住所２            TO 施術所住所２Ｗ
023360*         STRING 施情－住所１     DELIMITED BY SPACE
023370*                施情－住所２     DELIMITED BY SPACE
023380*                INTO 施術所住所Ｗ
023390*         END-STRING
023400*
023410         MOVE 施情－電話番号          TO 施術所電話番号Ｗ
023420** 振込先情報
023430*         MOVE 施情－取引先銀行名      TO 取引先銀行名Ｗ
023440*         MOVE 施情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
023450*         MOVE 施情－預金種別          TO 預金種別Ｗ
023460*         MOVE 施情－口座番号          TO 口座番号Ｗ
023470*         MOVE 施情－口座名義人        TO 口座名義人Ｗ
023480*         MOVE 施情－口座名義人カナ    TO 口座名義人カナＷ
023490*
023500** 振込先情報  / 会情報マスタより振込先情報を取得 /
023520         MOVE ZERO  TO  会情－柔整鍼灸区分
023510         MOVE 24    TO  会情－協会コード
023520         MOVE ZERO  TO  会情－保険種別
023530         MOVE ZERO  TO  会情－変更和暦年月
023540         READ 会情報マスタ
023550         NOT INVALID KEY
023560             MOVE 会情－取引先銀行名      TO 取引先銀行名Ｗ
023570             MOVE 会情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
023580             MOVE 会情－預金種別          TO 預金種別Ｗ
023590             MOVE 会情－口座番号          TO 口座番号Ｗ
023600             MOVE 会情－口座名義人        TO 口座名義人Ｗ
023610             MOVE 会情－口座名義人カナ    TO 口座名義人カナＷ
      *会情報マスタ使用/20190403
      *                   MOVE "ｱｻﾋｾｯｺﾂｼｶｲ ｶｲﾁｮｳ ｲｹﾀﾞ ﾏｻﾐﾂ" TO 口座名義人カナＷ
023620*
023630             STRING 取引先銀行名Ｗ     DELIMITED BY SPACE
023640                    " "                DELIMITED BY SIZE
023650                    取引先銀行支店名Ｗ DELIMITED BY SPACE
023660                    INTO 銀行名支店名Ｗ
023670             END-STRING
023680             EVALUATE 預金種別Ｗ
023690             WHEN 1
023700                 MOVE NC"（普通）" TO 預金種別コメントＷ
023710             WHEN 2
023720                 MOVE NC"（当座）" TO 預金種別コメントＷ
023730             WHEN OTHER
023740                 MOVE SPACE        TO 預金種別コメントＷ
023750             END-EVALUATE
023760*
023770*             MOVE 会情－接骨師会会長名    TO 会長名Ｗ
023780         END-READ

      */現状は振込のみ対応
               MOVE NC"○" TO 振込チェックＷ
      *
               EVALUATE 預金種別Ｗ
               WHEN 1
                   MOVE NC"○" TO 普通チェックＷ
               WHEN 2
                   MOVE NC"○" TO 当座チェックＷ
               END-EVALUATE
      *
               MOVE ZERO  TO カウンタ
               MOVE 取引先銀行名Ｗ TO 金融機関名Ｗ
               INSPECT 取引先銀行名Ｗ TALLYING カウンタ FOR ALL "銀行"
               IF ( カウンタ >= 1 )
                   MOVE NC"○" TO 銀行チェックＷ
                   MOVE SPACE  TO 金融機関名Ｗ
                   UNSTRING 取引先銀行名Ｗ DELIMITED BY "銀行"
                       INTO 金融機関名Ｗ
                   END-UNSTRING
               END-IF
               MOVE ZERO TO カウンタ
               INSPECT 取引先銀行名Ｗ TALLYING カウンタ FOR ALL "金庫"
               IF ( カウンタ >= 1 )
                   MOVE NC"○" TO 金庫チェックＷ
                   MOVE SPACE  TO 金融機関名Ｗ
                   UNSTRING 取引先銀行名Ｗ DELIMITED BY "金庫"
                       INTO 金融機関名Ｗ
                   END-UNSTRING
               END-IF
               MOVE ZERO TO カウンタ
               INSPECT 取引先銀行名Ｗ TALLYING カウンタ FOR ALL "農協"
               IF ( カウンタ >= 1 )
                   MOVE NC"○" TO 農協チェックＷ
                   MOVE SPACE  TO 金融機関名Ｗ
                   UNSTRING 取引先銀行名Ｗ DELIMITED BY "農協"
                       INTO 金融機関名Ｗ
                   END-UNSTRING
               END-IF
      *
               MOVE 取引先銀行支店名Ｗ TO 支店名Ｗ
               MOVE ZERO TO カウンタ
               INSPECT 取引先銀行支店名Ｗ TALLYING カウンタ FOR ALL "本店"
               IF ( カウンタ >= 1 )
                   MOVE NC"○" TO 本店チェックＷ
                   MOVE SPACE  TO 支店名Ｗ
                   UNSTRING 取引先銀行支店名Ｗ DELIMITED BY "本店"
                       INTO 支店名Ｗ
                   END-UNSTRING
               END-IF
               MOVE ZERO TO カウンタ
               INSPECT 取引先銀行支店名Ｗ TALLYING カウンタ FOR ALL "支店"
               IF ( カウンタ >= 1 )
                   MOVE NC"○" TO 支店チェックＷ
                   MOVE SPACE  TO 支店名Ｗ
                   UNSTRING 取引先銀行支店名Ｗ DELIMITED BY "支店"
                       INTO 支店名Ｗ
                   END-UNSTRING
               END-IF
               MOVE ZERO TO カウンタ
               INSPECT 取引先銀行支店名Ｗ TALLYING カウンタ FOR ALL "本所"
               IF ( カウンタ >= 1 )
                   MOVE NC"○" TO 本支所チェックＷ
                   MOVE SPACE  TO 支店名Ｗ
                   UNSTRING 取引先銀行支店名Ｗ DELIMITED BY "本所"
                       INTO 支店名Ｗ
                   END-UNSTRING
               END-IF
               MOVE ZERO TO カウンタ
               INSPECT 取引先銀行支店名Ｗ TALLYING カウンタ FOR ALL "支所"
               IF ( カウンタ >= 1 )
                   MOVE NC"○" TO 本支所チェックＷ
                   MOVE SPACE  TO 支店名Ｗ
                   UNSTRING 取引先銀行支店名Ｗ DELIMITED BY "支所"
                       INTO 支店名Ｗ
                   END-UNSTRING
               END-IF
023790*
023800     END-READ.
023810*
023820* 固定印字
023830*     MOVE NC"療養費支給金額の受領をあさひ接骨師会" TO 会長委任文１Ｗ.
023840*     MOVE NC"　会長　"           TO 会長委任文２１Ｗ.
023850*     MOVE NC"　殿に委任します。" TO 会長委任文２２Ｗ.
      */委任文の変更↓↓↓/20190307
      *     MOVE "療養費支給金額の受領をあさひ接骨師会"      TO 会長委任文１Ｗ
      *     MOVE "会長 池田勝光 殿(三重県四日市市安島１丁目" TO 会長委任文２Ｗ
      **/会住所の変更/131008
      **     MOVE "4-16ＫＡＮＥＮＩビル７Ｆ)に委任します。"   TO 会長委任文３Ｗ
      *     MOVE "6-14 ラ・テラビル１Ｆ)に委任します。"   TO 会長委任文３Ｗ
           MOVE "療養費の受領を株式会社ホープ接骨師会  代表取締役" TO 会長委任文１Ｗ
           MOVE "池田  勝光  殿（三重県四日市市安島 1 丁目 6-14  " TO 会長委任文２Ｗ
           MOVE "ラ・テラビル 1F）に委任します。                 " TO 会長委任文３Ｗ
      */委任文の変更↑↑↑/20190307
023860*
023870*********************************************
023880** ＩＤ管理マスタより　県施術ＩＤを取得する。
023890*   (国保組合も、対象)
023900*********************************************
023910     EVALUATE 保険種別ＷＲ 
023920* 国保
023930     WHEN 01
023940         MOVE 保険者番号ＷＲ         TO 保険者番号比較Ｗ
023950* 退職
023960     WHEN 08
026860* 後期高齢
026870     WHEN 05
023970         MOVE 保険者番号ＷＲ(3:6)    TO 保険者番号比較Ｗ
023980     END-EVALUATE.
023990**   / 県施術ID /
024000     IF 保険種別ＷＲ = 01 OR 08 OR 05
024010         MOVE 01                     TO ＩＤ管－ＩＤ区分
024020         MOVE ZERO                   TO ＩＤ管－施術所番号
024030         MOVE 保険者番号比較Ｗ(1:2)  TO ＩＤ管－保険種別
024040         MOVE SPACE                  TO ＩＤ管－保険者番号
024050         READ ＩＤ管理マスタ
024060         NOT INVALID KEY
024070              MOVE NC"県番号　施術機関番号"  TO 県共済固定Ｗ
024080              STRING 保険者番号比較Ｗ(1:2) DELIMITED BY SPACE
024090                     "   "                 DELIMITED BY SIZE
024100                     ＩＤ管－施術ＩＤ番号  DELIMITED BY SPACE
024110                     INTO 県施術ＩＤＷ
024120             END-STRING
024130         END-READ
024140     END-IF.
024150**
024160*
024170*================================================================*
024180 共済番号セット SECTION.
024190*
024200**************************************************************
024210* 保険者番号により、共済の番号を印字するか、柔整師番号か判定
024220**************************************************************
024230** 1.共済組合連盟
024240     MOVE SPACE  TO  脱出フラグ.
024250     IF 施情－共済連番号 NOT = ZERO
024260** 条件(保険者番号)
024270        IF ( 保険者番号ＷＲ(1:2) = "31" )  OR
024280           ( 保険者番号ＷＲ = "34130021" )
024290*
024300*           MOVE  NC"共済組合連盟第"   TO 共済連番号名ＮＷ 
024310           MOVE  NC"共済組合連盟"     TO 県共済固定Ｗ 
024320           MOVE  NC"第"               TO 共済連番号名ＮＷ 
024330           MOVE  NC"号"               TO 共済連番号単位ＮＷ 
024340           MOVE  施情－共済連番号     TO 共済連番号Ｗ
024350           IF    (共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
024360                 MOVE SPACE TO  共済連番号Ｗ(1:1)
024370           ELSE
024380                 MOVE "YES" TO  脱出フラグ
024390           END-IF
024400           IF    (共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
024410                 MOVE SPACE TO  共済連番号Ｗ(2:1)
024420           ELSE
024430                 MOVE "YES" TO  脱出フラグ
024440           END-IF
024450           IF    (共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
024460                 MOVE SPACE TO  共済連番号Ｗ(3:1)
024470           ELSE
024480                 MOVE "YES" TO  脱出フラグ
024490           END-IF
024500           IF    (共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
024510                 MOVE SPACE TO  共済連番号Ｗ(4:1)
024520           ELSE
024530                 MOVE "YES" TO  脱出フラグ
024540           END-IF
024550           IF    (共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
024560                 MOVE SPACE TO  共済連番号Ｗ(5:1)
024570           ELSE
024580                 MOVE "YES" TO  脱出フラグ
024590           END-IF
024600           IF    (共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
024610                 MOVE SPACE TO  共済連番号Ｗ(6:1)
024620           ELSE
024630                 MOVE "YES" TO  脱出フラグ
024640           END-IF
024650           MOVE  共済連番号集団Ｗ     TO 共済番号Ｗ
024660        END-IF
024670     END-IF.
024680*
024690** 2. 地共済協議会
024700     MOVE SPACE  TO  脱出フラグ.
024720** 条件(保険者番号)
024730     IF ( 保険者番号ＷＲ(1:2) = "32" OR "33" OR "34" )  AND
024740        ( 保険者番号ＷＲ NOT = "34130021" )
024710        IF 施情－地共済連番号 NOT = ZERO
024750*
024760*           MOVE  NC"地共済協議会"     TO 共済連番号名ＮＷ 
024770           MOVE  NC"地方公務員共済組合協議会" TO 県共済固定Ｗ
024780           MOVE  NC"第"               TO 共済連番号名ＮＷ 
024790           MOVE  NC"号"               TO 共済連番号単位ＮＷ 
024800           MOVE  施情－地共済連番号   TO 共済連番号Ｗ
024810           IF    (共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
024820                 MOVE SPACE TO  共済連番号Ｗ(1:1)
024830           ELSE
024840                 MOVE "YES" TO  脱出フラグ
024850           END-IF
024860           IF    (共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
024870                 MOVE SPACE TO  共済連番号Ｗ(2:1)
024880           ELSE
024890                 MOVE "YES" TO  脱出フラグ
024900           END-IF
024910           IF    (共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
024920                 MOVE SPACE TO  共済連番号Ｗ(3:1)
024930           ELSE
024940                 MOVE "YES" TO  脱出フラグ
024950           END-IF
024960           IF    (共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
024970                 MOVE SPACE TO  共済連番号Ｗ(4:1)
024980           ELSE
024990                 MOVE "YES" TO  脱出フラグ
025000           END-IF
025010           IF    (共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
025020                 MOVE SPACE TO  共済連番号Ｗ(5:1)
025030           ELSE
025040                 MOVE "YES" TO  脱出フラグ
025050           END-IF
025060           IF    (共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
025070                 MOVE SPACE TO  共済連番号Ｗ(6:1)
025080           ELSE
025090                 MOVE "YES" TO  脱出フラグ
025100           END-IF
025110           MOVE  共済連番号集団Ｗ     TO 共済番号Ｗ
025120        ELSE
024250           IF 施情－共済連番号 NOT = ZERO
024310               MOVE  NC"共済組合連盟"     TO 県共済固定Ｗ 
024320               MOVE  NC"第"               TO 共済連番号名ＮＷ 
024330               MOVE  NC"号"               TO 共済連番号単位ＮＷ 
024340               MOVE  施情－共済連番号     TO 共済連番号Ｗ
024350               IF (共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
024360                   MOVE SPACE TO  共済連番号Ｗ(1:1)
024370               ELSE
024380                   MOVE "YES" TO  脱出フラグ
024390               END-IF
024400               IF (共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
024410                   MOVE SPACE TO  共済連番号Ｗ(2:1)
024420               ELSE
024430                   MOVE "YES" TO  脱出フラグ
024440               END-IF
024450               IF (共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
024460                   MOVE SPACE TO  共済連番号Ｗ(3:1)
024470               ELSE
024480                   MOVE "YES" TO  脱出フラグ
024490               END-IF
024500               IF (共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
024510                   MOVE SPACE TO  共済連番号Ｗ(4:1)
024520               ELSE
024530                   MOVE "YES" TO  脱出フラグ
024540               END-IF
024550               IF (共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
024560                   MOVE SPACE TO  共済連番号Ｗ(5:1)
024570               ELSE
024580                   MOVE "YES" TO  脱出フラグ
024590               END-IF
024600               IF (共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
024610                   MOVE SPACE TO  共済連番号Ｗ(6:1)
024620               ELSE
024630                   MOVE "YES" TO  脱出フラグ
024640               END-IF
024650               MOVE  共済連番号集団Ｗ     TO 共済番号Ｗ
024670           END-IF
              END-IF
025130     END-IF.
025140*
025150*================================================================*
025160 自衛官番号セット SECTION.
025170*
025180     MOVE SPACE  TO  脱出フラグ.
025190     IF 施情－自衛官番号 NOT = ZERO
025200*           MOVE  NC"防衛庁第"    TO 自衛官番号名ＮＷ 
025201*           MOVE  NC"防衛庁"       TO 県共済固定Ｗ
025203           IF 施情－防衛省区分 = 1
025204              MOVE  NC"防衛省"      TO 県共済固定Ｗ
025205           ELSE
025206              MOVE  NC"防衛庁"      TO 県共済固定Ｗ
025207           END-IF
025220           MOVE  NC"第"           TO 自衛官番号名ＮＷ 
025230           MOVE  NC"号"           TO 自衛官番号単位ＮＷ 
025240           MOVE  施情－自衛官番号 TO 自衛官番号Ｗ
025250           IF    (自衛官番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
025260                 MOVE SPACE TO  自衛官番号Ｗ(1:1)
025270           ELSE
025280                 MOVE "YES" TO  脱出フラグ
025290           END-IF
025300           IF    (自衛官番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
025310                 MOVE SPACE TO  自衛官番号Ｗ(2:1)
025320           ELSE
025330                 MOVE "YES" TO  脱出フラグ
025340           END-IF
025350           IF    (自衛官番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
025360                 MOVE SPACE TO  自衛官番号Ｗ(3:1)
025370           ELSE
025380                 MOVE "YES" TO  脱出フラグ
025390           END-IF
025400           IF    (自衛官番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
025410                 MOVE SPACE TO  自衛官番号Ｗ(4:1)
025420           ELSE
025430                 MOVE "YES" TO  脱出フラグ
025440           END-IF
025450           IF    (自衛官番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
025460                 MOVE SPACE TO  自衛官番号Ｗ(5:1)
025470           ELSE
025480                 MOVE "YES" TO  脱出フラグ
025490           END-IF
025500           IF    (自衛官番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
025510                 MOVE SPACE TO  自衛官番号Ｗ(6:1)
025520           ELSE
025530                 MOVE "YES" TO  脱出フラグ
025540           END-IF
025550           MOVE  自衛官番号集団Ｗ     TO 共済番号Ｗ
025560     END-IF.
025570*
025580*================================================================*
025590 請求先情報取得 SECTION.
025600*
025610****************************************************
025620* 連結データから保険者マスタより請求先を取得する。 *
025630* ※保－請求先情報区分=1の場合請求先マスタを使用   *
025640* ● 請求先...... 請求先名称Ｗに格納               *
025650****************************************************
025660*----------------------------------------------------------------------------------*
025670* 請求マスタ読む。
025680* 国保退職老人助成の時、○○○"長"をつける。請求マスタにあるときは、支部部署名
025690*                       をつける（支部部署名には、（須磨区）と区名を入力しておく。
025700* 2001/10/26 修正：兵庫28のみ支部部署名をつける
025710*----------------------------------------------------------------------------------*
025720*
025730     MOVE 保険種別ＷＲ   TO 保－保険種別.
025740     MOVE 保険者番号ＷＲ TO 保－保険者番号.
025750     READ 保険者マスタ
025760     INVALID KEY
025770         MOVE SPACE              TO 請求先名称ＴＢＬ
025780         MOVE SPACE              TO 支部部署名Ｗ
025790         MOVE ZERO               TO 接尾語区分Ｗ
025800     NOT INVALID KEY
025810         IF 保－請求先情報区分 = 1
025820             MOVE 保険種別ＷＲ   TO 請先－保険種別
025830             MOVE 保険者番号ＷＲ TO 請先－保険者番号
025840             READ 請求先マスタ
025850             INVALID KEY
025860                 MOVE SPACE        TO 請求先名称ＴＢＬ
025870                 MOVE SPACE        TO 支部部署名Ｗ
025880             NOT INVALID KEY
025890                 MOVE 請先－保険者名称  TO 請求先名称ＴＢＬ
025900                 MOVE 請先－支部部署名  TO 支部部署名Ｗ
025910             END-READ
025920*        / 兵庫以外　支部部署名クリアー /
025930             IF (( 保険種別ＷＲ = 01 ) AND ( 保険者番号ＷＲ(1:2) = "28" )) OR
025940                (( 保険種別ＷＲ = 08 ) AND ( 保険者番号ＷＲ(3:2) = "28" )) 
025950                 CONTINUE
025960             ELSE
025970                 MOVE SPACE  TO 支部部署名Ｗ
025980             END-IF
025990*
026000         ELSE
026010             MOVE 保－保険者名称      TO 請求先名称ＴＢＬ
026020             MOVE 保－接尾語区分      TO 接尾語区分Ｗ
026030             EVALUATE 保険種別ＷＲ
026040             WHEN 1
026050             WHEN 8
026060                 MOVE SPACE           TO 支部部署名Ｗ
026070             WHEN OTHER
026080                 MOVE 保－支部部署名  TO 支部部署名Ｗ
026090             END-EVALUATE
026100         END-IF
026110     END-READ.
026120*
026130     IF 請求先名称ＴＢＬ NOT = SPACE
026140        EVALUATE 保険種別ＷＲ
026150** 社保・日雇
026160        WHEN 2
026170        WHEN 6
026180          IF 接尾語区分Ｗ = 1
026190*             MOVE 請求先名称ＴＢＬ    TO 請求先名称Ｗ
026210             STRING 請求先名称ＴＢＬ  DELIMITED BY SPACE
026230                    "殿"              DELIMITED BY SIZE
026240                    INTO 請求先名称Ｗ
026250             END-STRING
026200          ELSE
026210             STRING 請求先名称ＴＢＬ  DELIMITED BY SPACE
026220                    "社会保険事務所"  DELIMITED BY SIZE
026230                    "殿"              DELIMITED BY SIZE
026240                    INTO 請求先名称Ｗ
026250             END-STRING
026260          END-IF
026270** 組合・共済は支部名まで印字
026280        WHEN 3
026290            STRING 請求先名称ＴＢＬ  DELIMITED BY SPACE
026300                   "健康保険組合"    DELIMITED BY SIZE
026310                   支部部署名Ｗ      DELIMITED BY SPACE
026320                   "殿"              DELIMITED BY SIZE
026330                   INTO 請求先名称Ｗ
026340            END-STRING
026350        WHEN 4
026360            STRING 請求先名称ＴＢＬ  DELIMITED BY SPACE
026370                   "共済組合"        DELIMITED BY SIZE
026380                   支部部署名Ｗ      DELIMITED BY SPACE
026390                   "殿"              DELIMITED BY SIZE
026400                   INTO 請求先名称Ｗ
026410            END-STRING
026420** 国保・退職
026430        WHEN 1
026440        WHEN 8
026450            PERFORM 国保退職請求先セット
026460        WHEN OTHER
026470            STRING 請求先名称ＴＢＬ  DELIMITED BY SPACE
026480                   "殿"              DELIMITED BY SIZE
026490                   INTO 請求先名称Ｗ
026500            END-STRING
026510        END-EVALUATE
026520     END-IF.
026530*
026540*================================================================*
026550 国保退職請求先セット SECTION.
026560*
026570     PERFORM VARYING カウンタ FROM 1 BY 1
026580             UNTIL ( カウンタ > 20 )  OR
026590                   ( 請求先名称ＷＴ(カウンタ) = SPACE )
026600        MOVE 請求先名称ＷＴ(カウンタ) TO 請求先名称ＷＴ１
026610     END-PERFORM.
026620     IF 請求先名称ＷＴ１ = "市" OR "町" OR "村" OR "区"
026630        STRING 請求先名称ＴＢＬ  DELIMITED BY SPACE
026640               "長"              DELIMITED BY SIZE
026650               支部部署名Ｗ      DELIMITED BY SPACE
026660               "殿"              DELIMITED BY SIZE
026670               INTO 請求先名称Ｗ
026680        END-STRING
026690     ELSE
026700        STRING 請求先名称ＴＢＬ  DELIMITED BY SPACE
026710               "　"              DELIMITED BY SIZE
026720               支部部署名Ｗ      DELIMITED BY SPACE
026730               "殿"              DELIMITED BY SIZE
026740               INTO 請求先名称Ｗ
026750        END-STRING
026760     END-IF.
026770*================================================================*
026780 受診者情報取得 SECTION.
026790*
026800**************************************************
026810* 連結データから受診者情報Ｆより以下の情報を取得 *
026820* ● 施術年 ..... 施術年Ｗに格納                 *
026830* ● 施術月 ..... 施術月Ｗに格納                 *
026840* ● 患者番号.... 患者番号Ｗに格納※ＦＤ連番用   *
026850* ● 記号 ....... 記号Ｗに格納                   *
026860* ● 番号 ....... 番号Ｗに格納                   *
026870* ● 保険者番号 . 保険者番号Ｗに格納             *
026880* ● 保険種別 ... 保険種別Ｗに格納               *
026890* ● 被保険者カナ.被保険者カナＷに格納           *
026900* ● 被保険者氏名.被保険者氏名Ｗに格納           *
026910* ● 住所１ ......被保険者住所１Ｗに格納         *
026920* ● 住所２ ......被保険者住所２Ｗに格納         *
026930* ● 患者カナ ....患者カナＷに格納               *
026940* ● 患者氏名 ....患者氏名Ｗに格納               *
026950* ● 患者性別 ....区分によりチェックに"○"を格納 *
026960* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
026970* ● 患者年 ......患者年Ｗに格納                 *
026980* ● 患者月 ......患者月Ｗに格納                 *
026990* ● 患者日 ......患者日Ｗに格納                 *
027000* ● 続柄 ........名称マスタより続柄Ｗに取得     *
027010**************************************************
           IF 受－レコード NOT = SPACE
022660         EVALUATE 受－保険種別
022670         WHEN 01
022690            MOVE NC"○"        TO 国保チェックＷ
022680         WHEN 08
022690            MOVE NC"○"        TO 退職チェックＷ
022700         WHEN 02
022710         WHEN 06
022750         WHEN 07
022720            MOVE NC"○"        TO 協会チェックＷ
022730         WHEN 03
022740            MOVE NC"○"        TO 組合チェックＷ
               WHEN 04
                  MOVE NC"○"        TO 共済チェックＷ
      */自衛のチェックを分ける/140213
               WHEN 09
                  MOVE NC"○"        TO 自衛チェックＷ
               WHEN 05
                  MOVE NC"○"        TO 後期チェックＷ
022770         END-EVALUATE
               IF 受－助成種別 = ZERO
                   MOVE NC"○" TO 単独チェックＷ
               ELSE
                   MOVE NC"○" TO ２併チェックＷ
               END-IF
      */本家区分はどれか１つに○をする。
               IF 受－保険種別 = 05
                   EVALUATE 受－特別区分
                   WHEN 1
                       MOVE NC"○" TO 高一チェックＷ
                   WHEN 3
                       MOVE NC"○" TO 高７チェックＷ
                   END-EVALUATE
               ELSE
028984             EVALUATE 受－特別区分
                   WHEN 1
                   WHEN 2
                       MOVE NC"○" TO 高一チェックＷ
                   WHEN 3
                       MOVE NC"○" TO 高７チェックＷ
028991             WHEN 6
                       MOVE NC"○" TO ６歳チェックＷ
                   WHEN OTHER
                       IF 受－本人家族区分 = 1
                           MOVE NC"○" TO 本人チェックＷ
                       ELSE
                           MOVE NC"○" TO 家族チェックＷ
                       END-IF
028999             END-EVALUATE
               END-IF
               EVALUATE レセ－負担割合
               WHEN ZERO
                   MOVE NC"○" TO １０割チェックＷ
               WHEN 1
                   MOVE NC"○" TO ９割チェックＷ
      */神奈川県の場合、前期高齢者１割は、給付割合を８割にする。(国が１割負担するため、患者１割、保険者８割、国１割となる)
                   IF (受－保険種別     = 01 AND 受－保険者番号(1:2) = "14") OR
                      (受－保険種別 NOT = 01 AND 受－保険者番号(3:2) = "14")
                       IF (受－保険種別 NOT = 05 ) AND (受－特別区分 = 1)
                           MOVE SPACE  TO ９割チェックＷ
                           MOVE NC"○" TO ８割チェックＷ
                       END-IF
                   END-IF
               WHEN 2
                   MOVE NC"○" TO ８割チェックＷ
               WHEN 3
                   MOVE NC"○" TO ７割チェックＷ
               END-EVALUATE
      *
027110*         EVALUATE 受－施術和暦
027120*         WHEN 1
027130*             MOVE NC"明治"  TO 施術和暦Ｗ
027140*         WHEN 2
027150*             MOVE NC"大正"  TO 施術和暦Ｗ
027160*         WHEN 3
027170*             MOVE NC"昭和"  TO 施術和暦Ｗ
027180*         WHEN 4
027190*             MOVE NC"平成"  TO 施術和暦Ｗ
027200*         END-EVALUATE
      */元号修正/20190408
               MOVE 受－施術和暦     TO 施術和暦Ｗ
027210         MOVE 受－施術年       TO 施術年Ｗ
027220         MOVE 受－施術月       TO 施術月Ｗ
027230         MOVE 受－患者番号     TO 患者番号Ｗ
027240*         MOVE 受－記号         TO 記号Ｗ
027250*         MOVE 受－番号         TO 番号Ｗ
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
027260         MOVE 受－保険者番号   TO 保険者番号Ｗ
027270         MOVE 受－保険種別     TO 保険種別Ｗ
027280** 全国土木の枝番削除
027290         IF ( 受－保険種別 = 01 ) AND ( 受－保険者番号(1:6) = "133033" )
027300            MOVE 受－保険者番号(1:6)  TO 保険者番号Ｗ
027310         END-IF
027320**
027330         MOVE 受－被保険者カナ TO 被保険者カナＷ
027340         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
027350         MOVE 受－郵便番号１   TO 郵便番号１Ｗ
027360         MOVE 受－郵便番号２   TO 郵便番号２Ｗ
027370         MOVE 受－住所１       TO 被保険者住所１Ｗ
027380         MOVE 受－住所２       TO 被保険者住所２Ｗ
      */ 電話番号追加 /42505
               IF 受－電話番号 NOT = SPACE
                  STRING "電話:"        DELIMITED BY SIZE
                         受－電話番号   DELIMITED BY SPACE
                    INTO 電話番号Ｗ
                  END-STRING
               ELSE
                  IF 受－患者電話番号 NOT = SPACE
                     STRING "電話:"            DELIMITED BY SIZE
                            受－患者電話番号   DELIMITED BY SPACE
                       INTO 電話番号Ｗ
                     END-STRING
                  END-IF
               END-IF
027390         MOVE 受－患者カナ     TO 患者カナＷ
027400         MOVE 受－患者氏名     TO 患者氏名Ｗ
027410         MOVE 受－費用負担者番号助成 TO 市町村番号Ｗ
               MOVE 受－受益者番号助成     TO 受給者番号Ｗ
027420*
027430         EVALUATE 受－被保険者性別
027440         WHEN 1
027450             MOVE NC"男"  TO 被保険者性別Ｗ
027460         WHEN 2
027470             MOVE NC"女"  TO 被保険者性別Ｗ
027480         END-EVALUATE
027490*
027500         EVALUATE 受－患者性別
027510         WHEN 1
027520             MOVE "(男)"  TO 患者性別Ｗ
027530         WHEN 2
027540             MOVE "(女)"  TO 患者性別Ｗ
027550         END-EVALUATE
027560         EVALUATE 受－患者性別
027570         WHEN 1
027580             MOVE NC"○"  TO 男チェックＷ
027590         WHEN 2
027600             MOVE NC"○"  TO 女チェックＷ
027610         END-EVALUATE
027620*
027630         EVALUATE 受－被保険者和暦
027640         WHEN 1
027650             MOVE NC"明治"  TO 被保険者元号Ｗ
027660         WHEN 2
027670             MOVE NC"大正"  TO 被保険者元号Ｗ
027680         WHEN 3
027690             MOVE NC"昭和"  TO 被保険者元号Ｗ
027700         WHEN 4
027710             MOVE NC"平成"  TO 被保険者元号Ｗ
027720         END-EVALUATE
027730*
027740         MOVE 受－被保険者年 TO 被保険者年Ｗ
027750         MOVE 受－被保険者月 TO 被保険者月Ｗ
027760         MOVE 受－被保険者日 TO 被保険者日Ｗ
027770*
027780         EVALUATE 受－患者和暦
027790         WHEN 1
027800             MOVE NC"○"  TO 明治チェックＷ
027810         WHEN 2
027820             MOVE NC"○"  TO 大正チェックＷ
027830         WHEN 3
027840             MOVE NC"○"  TO 昭和チェックＷ
027850         WHEN 4
027860             MOVE NC"○"  TO 平成チェックＷ
      */元号修正/20190408
023060         WHEN 5
                   MOVE "5令"   TO 令和ＣＭＷ
023070             MOVE NC"○"  TO 令和チェックＷ
027870         END-EVALUATE
027880         EVALUATE 受－患者和暦
027890         WHEN 1
027900             MOVE NC"明治"  TO 元号Ｗ
027910         WHEN 2
027920             MOVE NC"大正"  TO 元号Ｗ
027930         WHEN 3
027940             MOVE NC"昭和"  TO 元号Ｗ
027950         WHEN 4
027960             MOVE NC"平成"  TO 元号Ｗ
027970         END-EVALUATE
027980*
      */元号修正/↓↓↓20190408
029310         IF 受－患者和暦 > 4
037370             MOVE 受－患者和暦     TO 元－元号区分
037380             READ 元号マスタ
037390             NOT INVALID KEY
037400                 MOVE 元－元号名称 TO 元号Ｗ
037410             END-READ
029330         END-IF
      */元号修正/↑↑↑20190408
027990         MOVE 受－患者年  TO 患者年Ｗ
028000         MOVE 受－患者月  TO 患者月Ｗ
028010         MOVE 受－患者日  TO 患者日Ｗ
028020* 続柄
028030         EVALUATE 保険種別ＷＲ 
028040* 退職
028050*         WHEN  08
028060*             IF (本人家族区分ＷＲ = 1 ) AND (受－世帯主続柄 = 1)
028070*                 MOVE NC"世帯主"  TO 続柄Ｗ
028080*             ELSE
028090*                 MOVE 05          TO 名－区分コード
028100*                 MOVE 受－続柄    TO 名－名称コード
028110*                 READ 名称マスタ
028120*                 INVALID KEY
028130*                     MOVE SPACE    TO 続柄Ｗ
028140*                 NOT INVALID KEY
028150*                     MOVE 名－略称 TO 続柄Ｗ
028160*                 END-READ
028170*             END-IF
028180* 国保
028190*         WHEN 01
028200*             IF 本人家族区分ＷＲ = 1
028210*                 MOVE NC"世帯主"    TO 続柄Ｗ
028220*             ELSE
028230*                 MOVE 05          TO 名－区分コード
028240*                 MOVE 受－続柄    TO 名－名称コード
028250*                 READ 名称マスタ
028260*                 INVALID KEY
028270*                     MOVE SPACE    TO 続柄Ｗ
028280*                 NOT INVALID KEY
028290*                     MOVE 名－略称 TO 続柄Ｗ
028300*                 END-READ
028310*             END-IF
028320* 自衛官は無条件に"本人"
028330         WHEN 09
028340              MOVE NC"本人"    TO 続柄Ｗ
028350* 国保・退職
028360         WHEN 08
028370         WHEN 01
028380             IF 本人家族区分ＷＲ = 1
028390                 MOVE NC"本人"    TO 続柄Ｗ
028400             ELSE
028410                 MOVE NC"―"      TO 続柄Ｗ
028420             END-IF
028430         WHEN OTHER
028440             IF 本人家族区分ＷＲ = 1
028450                 MOVE NC"本人"    TO 続柄Ｗ
028460             ELSE
028470                 MOVE 05          TO 名－区分コード
028480                 MOVE 受－続柄    TO 名－名称コード
028490                 READ 名称マスタ
028500                 INVALID KEY
028510                     MOVE SPACE    TO 続柄Ｗ
028520                 NOT INVALID KEY
028530                     MOVE 名－略称 TO 続柄Ｗ
028540                 END-READ
028550             END-IF
028560         END-EVALUATE
028570*
028580** 保険種別チェックを設定
028590* 14/10廃止
028591*         EVALUATE 保険種別ＷＲ
028600*         WHEN  01
028610*             MOVE NC"国保"   TO 保険種別名称Ｗ
028620*         WHEN  02
028630*         WHEN  06
028640*         WHEN  07
028650*             MOVE NC"政管"   TO 保険種別名称Ｗ
028660*         WHEN  03
028670*             MOVE NC"組合"   TO 保険種別名称Ｗ
028680*         WHEN  04
028690*             MOVE NC"共済"   TO 保険種別名称Ｗ
028700*         WHEN  08
028710*             MOVE NC"退職"   TO 保険種別名称Ｗ
028720*         WHEN  09
028730*             MOVE NC"自衛官" TO 保険種別名称Ｗ
028740*         END-EVALUATE
028741* 14/10～
028742         EVALUATE 保険種別ＷＲ
028743         WHEN  01
028744             MOVE NC"国"   TO 保険種別名称Ｗ１
028745         WHEN  02
028746         WHEN  06
028747         WHEN  07
028748             MOVE NC"政"   TO 保険種別名称Ｗ１
028749         WHEN  03
028750             MOVE NC"組"   TO 保険種別名称Ｗ１
028751         WHEN  04
028752             MOVE NC"共"   TO 保険種別名称Ｗ１
028753         WHEN  08
028754             MOVE NC"退"   TO 保険種別名称Ｗ１
028755         WHEN  09
028756             MOVE NC"自"   TO 保険種別名称Ｗ１
028757         END-EVALUATE
028758         IF 本人家族区分ＷＲ = 1
028759             MOVE NC"本人"  TO 簡易続柄Ｗ
028760         ELSE
028761             MOVE NC"家族"  TO 簡易続柄Ｗ
028762         END-IF
028763*
028764         STRING 保険種別名称Ｗ１  DELIMITED BY SIZE
028765                NC"・"            DELIMITED BY SIZE
028766                簡易続柄Ｗ        DELIMITED BY SIZE
028768                INTO 保険種別名称Ｗ
028769         END-STRING
028770*
028771* 固定項目
028772         EVALUATE 保険種別ＷＲ 
028780* 国保・退職
028790         WHEN  01
028800         WHEN  08
028810             MOVE NC"世帯主の氏名" TO 被保険者氏名固定Ｗ
028820             MOVE NC"世帯主の住所" TO 被保険者住所固定Ｗ
028830             MOVE NC"世帯主"       TO 委任固定Ｗ
028840* 組合・共済
028850         WHEN  03
028860         WHEN  04
028870             MOVE NC"組合員の氏名" TO 被保険者氏名固定Ｗ
028880             MOVE NC"組合員の住所" TO 被保険者住所固定Ｗ
028890             MOVE NC"組合員"       TO 委任固定Ｗ
028900* 社保・自衛
028910         WHEN  OTHER
028920             MOVE NC"被保険者の氏名" TO 被保険者氏名固定Ｗ
028930             MOVE NC"被保険者の住所" TO 被保険者住所固定Ｗ
028940             MOVE NC"被保険者"       TO 委任固定Ｗ
028950         END-EVALUATE
028960*
028970*         MOVE NC"生年月日" TO 生年月日固定Ｗ
028980*
028981** 14/10～　特別区分コメント印字
028982*         IF 受－施術和暦年月 >= 41410
028983*             IF 受－公費種別 = ZERO
028984*                EVALUATE 受－特別区分
028985*                WHEN 1
028986*                   MOVE "70歳以上"  TO 特別コメントＷ
028987*                WHEN 2
028988*                   MOVE "70歳以上"  TO 特別コメントＷ
028989*                WHEN 3
028990*                   MOVE "70歳以上"  TO 特別コメントＷ
028991*                WHEN 6
028992*                   IF 受－施術和暦年月 < 42004
028993*                      MOVE "3歳未満"    TO 特別コメントＷ
028994*                   ELSE
028995*                      MOVE "未就学児"   TO 特別コメントＷ
028996*                   END-IF
028999*                END-EVALUATE
029000*             END-IF
029001*         END-IF
030010**
      */レセまとめに対応/101108
030020         IF 受－助成種別 NOT = ZERO
030030            PERFORM 助成レセまとめ判定
030040         ELSE
030050            MOVE SPACE TO 助成レセまとめフラグ
030060         END-IF
029002*
029003     END-IF.
029004*================================================================*
029010 負傷データ取得 SECTION.
029020*
029030**************************************************
029040* 連結データから負傷データＦより以下の情報を取得 *
029050* ● 負傷名...部位＋負傷種別にて加工して格納     *
029060* ● 負傷年.......負傷年Ｗ                       *
029070* ● 負傷月.......負傷月Ｗ                       *
029080* ● 負傷日.......負傷日Ｗ                       *
029090* ● 開始年.......初検年Ｗ                       *
029100* ● 開始月.......初検月Ｗ                       *
029110* ● 開始日.......初検日Ｗ                       *
029120* ● 終了年.......終了年Ｗ                       *
029130* ● 終了月.......終了月Ｗ                       *
029140* ● 終了日.......終了日Ｗ                       *
029150* ● 実日数.......実日数Ｗ                       *
029160* ● 転帰区分 ....区分によりチェックに"○"を格納 *
029170* ● 金属副子 ....区分によりチェックに"○"を格納 *
029180* ● 経過コード...経過マスタより取得             *
029190**************************************************
           IF 負－レコード NOT = SPACE
029290         MOVE 負－部位数                   TO 部位数Ｗ
029300         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
029310                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
029320             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別Ｗ(部位ＣＮＴ)
029330             MOVE 負－部位(部位ＣＮＴ)     TO 部位Ｗ(部位ＣＮＴ)
029340             MOVE 負－左右区分(部位ＣＮＴ) TO 左右区分Ｗ(部位ＣＮＴ)
029350             MOVE 負－負傷位置番号(部位ＣＮＴ)
029360                                           TO 負傷位置番号Ｗ(部位ＣＮＴ)
029370********************************************************
029380* 注）全柔...部位名1+負傷種別＋部位名2にて加工して格納 *
029390********************************************************
029400* 負傷種別
029410             MOVE SPACE                     TO 負傷名称Ｗ
029420             MOVE 03                        TO 名－区分コード
029430             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
029440             READ 名称マスタ
029450             INVALID KEY
029460                 MOVE SPACE        TO 負傷名称Ｗ
029470             NOT INVALID KEY
029480                 MOVE 名－正式名称 TO 負傷名称Ｗ
029490             END-READ
029500* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
032680*
032690             PERFORM 部位名称埋込処理
029690*
029700             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
029710             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
029720             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
029730             MOVE 負－開始年(部位ＣＮＴ)   TO 初検年Ｗ(部位ＣＮＴ)
029740             MOVE 負－開始月(部位ＣＮＴ)   TO 初検月Ｗ(部位ＣＮＴ)
029750             MOVE 負－開始日(部位ＣＮＴ)   TO 初検日Ｗ(部位ＣＮＴ)
029760             IF 負－転帰区分(部位ＣＮＴ) = 9
029770                 MOVE 99                   TO 終了年Ｗ(部位ＣＮＴ)
029780                 MOVE 99                   TO 終了月Ｗ(部位ＣＮＴ)
029790                 MOVE 99                   TO 終了日Ｗ(部位ＣＮＴ)
029800             ELSE
029810                 MOVE 負－終了年(部位ＣＮＴ)   TO 終了年Ｗ(部位ＣＮＴ)
029820                 MOVE 負－終了月(部位ＣＮＴ)   TO 終了月Ｗ(部位ＣＮＴ)
029830                 MOVE 負－終了日(部位ＣＮＴ)   TO 終了日Ｗ(部位ＣＮＴ)
029840             END-IF
029850* 経過略称取得
029860             MOVE 01                         TO 経－区分コード
029870             MOVE 負－経過コード(部位ＣＮＴ) TO 経－経過コード
029880             READ 経過マスタ
029890             INVALID KEY
029900                 MOVE ZERO            TO 部位ＣＮＴＷ(部位ＣＮＴ)
029910                 MOVE SPACE           TO 部位区切Ｗ(部位ＣＮＴ)
029920                 MOVE SPACE           TO 経過略称Ｗ(部位ＣＮＴ)
029930             NOT INVALID KEY
029940                 EVALUATE 部位ＣＮＴ
029950                 WHEN 1
029960                     MOVE NC"①" TO 経過部位Ｗ
029970                 WHEN 2
029980                     MOVE NC"②" TO 経過部位Ｗ
029990                 WHEN 3
030000                     MOVE NC"③" TO 経過部位Ｗ
030010                 WHEN 4
030020                     MOVE NC"④" TO 経過部位Ｗ
030030                 WHEN 5
030040                     MOVE NC"⑤" TO 経過部位Ｗ
030050                 END-EVALUATE
030060                 STRING  経過部位Ｗ     DELIMITED BY SPACE
030070                         経－経過略称   DELIMITED BY SPACE
030080                        INTO 印刷経過略称Ｗ(部位ＣＮＴ)
030090                 END-STRING
030100             END-READ
030110*
030120             MOVE 負－転帰区分(部位ＣＮＴ) TO 転帰区分Ｗ(部位ＣＮＴ)
030130             EVALUATE 負－転帰区分(部位ＣＮＴ)
030140             WHEN 1
030150             WHEN 2
030160                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
030170             WHEN 3
030180                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
030190             WHEN 4
030200                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
030210             END-EVALUATE
030220*
030230*             EVALUATE 負－転帰区分(部位ＣＮＴ)
030240*             WHEN 1
030250*             WHEN 2
030260*                 MOVE NC"治癒"               TO 転帰Ｗ(部位ＣＮＴ)
030270*             WHEN 3
030280*                 MOVE NC"中止"               TO 転帰Ｗ(部位ＣＮＴ)
030290*             WHEN 4
030300*                 MOVE NC"転医"               TO 転帰Ｗ(部位ＣＮＴ)
030310*             WHEN OTHER
030320*                 MOVE NC"継続"               TO 転帰Ｗ(部位ＣＮＴ)
030330*             END-EVALUATE
030340*
                    MOVE レセ－部位実日数(部位ＣＮＴ) TO 実日数Ｗ(部位ＣＮＴ)
030350         END-PERFORM
030360* 新規/継続 チェック
033380         EVALUATE レセ－レセ請求区分
               WHEN 1
033390             MOVE NC"○"                   TO 新規チェックＷ
               WHEN 2
033410             MOVE NC"○"                   TO 継続チェックＷ
033400         WHEN 3
033390             MOVE NC"○"                   TO 新規チェックＷ
033410             MOVE NC"○"                   TO 継続チェックＷ
               WHEN OTHER
033410             MOVE NC"○"                   TO 継続チェックＷ
033420         END-EVALUATE
030480* 枝番判定用
030490         MOVE 負－開始診療日手動区分 TO  開始診療日手動区分Ｗ
030500*
030501* 負傷原因印刷区分
030502         MOVE 負－レセ負傷原因印刷区分 TO レセ負傷原因印刷区分Ｗ
027880         MOVE 負－レセ長期理由印刷区分 TO レセ長期理由印刷区分Ｗ
      *
      */３部位同時請求印 /42505
               PERFORM ３部位同時請求判定
               IF 負傷原因対象フラグ = "YES"
                   MOVE NC"・" TO 負原印
               END-IF
030503*
030510     END-IF.
040083*
      */長野の国保・退職・後高の時で経過が入力されてない時は、経過を入れる/160610
           IF ((保険種別ＷＲ = 01) AND (保険者番号ＷＲ(1:2) = "20" )) OR
              ((保険種別ＷＲ = 08) AND (保険者番号ＷＲ(3:2) = "20" )) OR
              ((公費種別ＷＲ = 05) AND (費用負担者番号ＷＲ(3:2) = "20" ))
016020        PERFORM 経過取得
           END-IF.
018770     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018790             UNTIL ( 部位ＣＮＴ > 5 )
018820         MOVE 印刷経過略称Ｗ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
018830     END-PERFORM.
030730*================================================================*
030910 部位名称埋込処理 SECTION.
030920*
006490     STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
009980            負傷名称Ｗ                    DELIMITED BY SPACE
006500            レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
006520       INTO 負傷名Ｗ(部位ＣＮＴ)
006570     END-STRING.
031050*
030690*================================================================*
030700 料金情報取得 SECTION.
030710*
030720********************
030730* 料金データセット *
030740********************
030750*    ****************************************************************
030760*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
030770*    ****************************************************************
030780     MOVE レセ－初検料                 TO 初検料ＷＲ.
030790     IF レセ－時間外 = 1
030800         MOVE NC"○"                   TO 時間外チェックＷ
030810     END-IF.
030820     IF レセ－休日 = 1
030830         MOVE NC"○"                   TO 休日チェックＷ
030840     END-IF.
030850     IF レセ－深夜 = 1
030860         MOVE NC"○"                   TO 深夜チェックＷ
030870     END-IF.
030880*
030890*     IF レセ－時間外 = 1
030900*         MOVE NC"時間外"               TO 時間外Ｗ
030910*     END-IF.
030920*     IF レセ－休日 = 1
030930*         MOVE NC"休日"                 TO 休日Ｗ
030940*     END-IF.
030950*     IF レセ－深夜 = 1
030960*         MOVE NC"深夜"                 TO 深夜Ｗ
030970*     END-IF.
030980*
030990*     STRING 時間外Ｗ     DELIMITED BY SPACE
031000*            NC"　"       DELIMITED BY SIZE
031010*            休日Ｗ       DELIMITED BY SPACE
031020*            NC"　"       DELIMITED BY SIZE
031030*            深夜Ｗ       DELIMITED BY SPACE
031040*            INTO 初検加算内容Ｗ
031050*     END-STRING.
031060*
031070     MOVE レセ－初検加算料             TO  初検加算料ＷＲ.
           MOVE レセ－初検時相談料           TO  初検時相談料ＷＲ.
031080     MOVE レセ－再検料                 TO  再検料ＷＲ.
031090     MOVE レセ－往療距離               TO  往療距離ＷＲ.
031100     MOVE レセ－往療回数               TO  往療回数ＷＲ.
031110     MOVE レセ－往療料                 TO  往療料ＷＲ.
031120     MOVE レセ－往療加算料             TO  往療加算料ＷＲ.
031130*
031140     IF レセ－夜間 = 1
031150         MOVE NC"○"                   TO 夜間チェックＷ
031160     END-IF.
031170*     IF レセ－時間外 = 1
031180*         MOVE NC"○"                   TO 往療深夜チェックＷ
031190*     END-IF.
031200     IF レセ－暴風雨雪 = 1
031210         MOVE NC"○"                   TO 暴風雨雪チェックＷ
031220     END-IF.
031230     IF レセ－難路 = 1
031240         MOVE NC"○"                   TO 難路チェックＷ
031250     END-IF.
031260*
031270     MOVE レセ－金属副子加算料         TO  金属副子加算料ＷＲ.
031280*
      */金属副子・運動後療の変更・追加/1805
021850     IF ( レセ－大 >= 1 )
021860         MOVE NC"○"                   TO 大チェックＷ
021870     END-IF.
021880     IF ( レセ－中 >= 1 )
021890         MOVE NC"○"                   TO 中チェックＷ
021900     END-IF.
021910     IF ( レセ－小 >= 1 )
021920         MOVE NC"○"                   TO 小チェックＷ
021930     END-IF.
           IF ( 施術和暦年月ＷＲ >= 43006 )
              MOVE レセ－金属副子回数        TO 金属回数Ｗ
           END-IF.
           MOVE レセ－運動後療料              TO 運動料Ｗ.
031380*
031390*     IF レセ－大 = 1
031400*         MOVE NC"大"                   TO 金属大Ｗ
031410*     END-IF.
031420*     IF レセ－中 = 1
031430*         MOVE NC"中"                   TO 金属中Ｗ
031440*     END-IF.
031450*     IF レセ－小 = 1
031460*         MOVE NC"小"                   TO 金属小Ｗ
031470*     END-IF.
031480*
031490     MOVE レセ－施術情報提供料         TO 施術情報提供料ＷＲ.
031500* 小計
031510     MOVE レセ－小計                   TO 小計Ｗ.
031520********************
031530* 初回処置料セット *
031540********************
031550     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
031560             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
031570         MOVE レセ－初回処置料(部位ＣＮＴ) TO 初回処置料ＷＲ(部位ＣＮＴ)
031580         IF レセ－初回処置料(部位ＣＮＴ) NOT = ZERO
031590            EVALUATE 負－負傷種別(部位ＣＮＴ)
031600* 捻挫・打撲・挫傷
031610            WHEN 1
031620            WHEN 2
031630            WHEN 3
031640                MOVE NC"○"       TO 施療料チェックＷ
031650* 脱臼・骨折・骨折拘縮
031660            WHEN 4
031670            WHEN 5
031680            WHEN 7
031690                MOVE NC"○"       TO 整復料チェックＷ
031700* 不全骨折・不全骨折拘縮
031710            WHEN 6
031720            WHEN 8
031730                MOVE NC"○"       TO 固定料チェックＷ
031740            END-EVALUATE
031750         END-IF
031760     END-PERFORM.
031770     MOVE レセ－初回処置料合計    TO 初回処置料合計Ｗ.
031780********************
031790* 逓減毎料金セット *
031800********************
031810*    **********
031820*    * １部位 *
031830*    **********
031840     MOVE レセ－後療単価１             TO 後療単価１ＷＲ.
031850     MOVE レセ－後療回数１             TO 後療回数１ＷＲ.
031860     MOVE レセ－後療料１               TO 後療料１ＷＲ.
031870     MOVE レセ－冷罨法回数１           TO 冷罨法回数１ＷＲ.
031880     MOVE レセ－冷罨法料１             TO 冷罨法料１ＷＲ.
031890     MOVE レセ－温罨法回数１           TO 温罨法回数１ＷＲ.
031900     MOVE レセ－温罨法料１             TO 温罨法料１ＷＲ.
031910     MOVE レセ－電療回数１             TO 電療回数１ＷＲ.
031920     MOVE レセ－電療料１               TO 電療料１ＷＲ.
031930     MOVE レセ－小計１                 TO 小計１ＷＲ.
031940     MOVE レセ－長期逓減率１           TO 長期逓減率１ＷＲ.
031950     MOVE レセ－長期込小計１           TO 長期込小計１ＷＲ.
031960*    **********
031970*    * ２部位 *
031980*    **********
031990     MOVE レセ－後療単価２             TO 後療単価２ＷＲ.
032000     MOVE レセ－後療回数２             TO 後療回数２ＷＲ.
032010     MOVE レセ－後療料２               TO 後療料２ＷＲ.
032020     MOVE レセ－冷罨法回数２           TO 冷罨法回数２ＷＲ.
032030     MOVE レセ－冷罨法料２             TO 冷罨法料２ＷＲ.
032040     MOVE レセ－温罨法回数２           TO 温罨法回数２ＷＲ.
032050     MOVE レセ－温罨法料２             TO 温罨法料２ＷＲ.
032060     MOVE レセ－電療回数２             TO 電療回数２ＷＲ.
032070     MOVE レセ－電療料２               TO 電療料２ＷＲ.
032080     MOVE レセ－小計２                 TO 小計２ＷＲ.
032090     MOVE レセ－長期逓減率２           TO 長期逓減率２ＷＲ.
032100     MOVE レセ－長期込小計２           TO 長期込小計２ＷＲ.
032110*    ****************
032120*    * ３部位／８割 *
032130*    ****************
032140     MOVE レセ－後療単価３８             TO 後療単価３８ＷＲ.
032150     MOVE レセ－後療回数３８             TO 後療回数３８ＷＲ.
032160     MOVE レセ－後療料３８               TO 後療料３８ＷＲ.
032170     MOVE レセ－冷罨法回数３８           TO 冷罨法回数３８ＷＲ.
032180     MOVE レセ－冷罨法料３８             TO 冷罨法料３８ＷＲ.
032190     MOVE レセ－温罨法回数３８           TO 温罨法回数３８ＷＲ.
032200     MOVE レセ－温罨法料３８             TO 温罨法料３８ＷＲ.
032210     MOVE レセ－電療回数３８             TO 電療回数３８ＷＲ.
032220     MOVE レセ－電療料３８               TO 電療料３８ＷＲ.
032230     MOVE レセ－小計３８                 TO 小計３８ＷＲ.
032240     MOVE レセ－多部位込小計３８         TO 多部位込小計３８ＷＲ.
032250     MOVE レセ－長期逓減率３８           TO 長期逓減率３８ＷＲ.
032260     MOVE レセ－長期込小計３８           TO 長期込小計３８ＷＲ.
032270*    ****************
032280*    * ３部位／10割 *
032290*    ****************
032300     MOVE レセ－逓減開始月３０           TO 逓減開始月３０ＷＲ.
032310     MOVE レセ－逓減開始日３０           TO 逓減開始日３０ＷＲ.
032320     MOVE レセ－後療単価３０             TO 後療単価３０ＷＲ.
032330     MOVE レセ－後療回数３０             TO 後療回数３０ＷＲ.
032340     MOVE レセ－後療料３０               TO 後療料３０ＷＲ.
032350     MOVE レセ－冷罨法回数３０           TO 冷罨法回数３０ＷＲ.
032360     MOVE レセ－冷罨法料３０             TO 冷罨法料３０ＷＲ.
032370     MOVE レセ－温罨法回数３０           TO 温罨法回数３０ＷＲ.
032380     MOVE レセ－温罨法料３０             TO 温罨法料３０ＷＲ.
032390     MOVE レセ－電療回数３０             TO 電療回数３０ＷＲ.
032400     MOVE レセ－電療料３０               TO 電療料３０ＷＲ.
032410     MOVE レセ－小計３０                 TO 小計３０ＷＲ.
032420     MOVE レセ－長期逓減率３０           TO 長期逓減率３０ＷＲ.
032430     MOVE レセ－長期込小計３０           TO 長期込小計３０ＷＲ.
032440**    ****************
032450**    * ４部位／５割 *
032460**    ****************
032470*     MOVE レセ－後療単価４５             TO 後療単価４５ＷＲ.
032480*     MOVE レセ－後療回数４５             TO 後療回数４５ＷＲ.
032490*     MOVE レセ－後療料４５               TO 後療料４５ＷＲ.
032500*     MOVE レセ－冷罨法回数４５           TO 冷罨法回数４５ＷＲ.
032510*     MOVE レセ－冷罨法料４５             TO 冷罨法料４５ＷＲ.
032520*     MOVE レセ－温罨法回数４５           TO 温罨法回数４５ＷＲ.
032530*     MOVE レセ－温罨法料４５             TO 温罨法料４５ＷＲ.
032540*     MOVE レセ－電療回数４５             TO 電療回数４５ＷＲ.
032550*     MOVE レセ－電療料４５               TO 電療料４５ＷＲ.
032560*     MOVE レセ－小計４５                 TO 小計４５ＷＲ.
032570*     MOVE レセ－多部位込小計４５         TO 多部位込小計４５ＷＲ.
032580*     MOVE レセ－長期逓減率４５           TO 長期逓減率４５ＷＲ.
032590*     MOVE レセ－長期込小計４５           TO 長期込小計４５ＷＲ.
032600*    ****************
032610*    * ４部位／８割 *
032620*    ****************
032630     MOVE レセ－逓減開始月４８           TO 逓減開始月４８ＷＲ.
032640     MOVE レセ－逓減開始日４８           TO 逓減開始日４８ＷＲ.
032650     MOVE レセ－後療単価４８             TO 後療単価４８ＷＲ.
032660     MOVE レセ－後療回数４８             TO 後療回数４８ＷＲ.
032670     MOVE レセ－後療料４８               TO 後療料４８ＷＲ.
032680     MOVE レセ－冷罨法回数４８           TO 冷罨法回数４８ＷＲ.
032690     MOVE レセ－冷罨法料４８             TO 冷罨法料４８ＷＲ.
032700     MOVE レセ－温罨法回数４８           TO 温罨法回数４８ＷＲ.
032710     MOVE レセ－温罨法料４８             TO 温罨法料４８ＷＲ.
032720     MOVE レセ－電療回数４８             TO 電療回数４８ＷＲ.
032730     MOVE レセ－電療料４８               TO 電療料４８ＷＲ.
032740     MOVE レセ－小計４８                 TO 小計４８ＷＲ.
032750     MOVE レセ－多部位込小計４８         TO 多部位込小計４８ＷＲ.
032760     MOVE レセ－長期逓減率４８           TO 長期逓減率４８ＷＲ.
032770     MOVE レセ－長期込小計４８           TO 長期込小計４８ＷＲ.
032780*    ****************
032790*    * ４部位／10割 *
032800*    ****************
032810     MOVE レセ－逓減開始月４０           TO 逓減開始月４０ＷＲ.
032820     MOVE レセ－逓減開始日４０           TO 逓減開始日４０ＷＲ.
032830     MOVE レセ－後療単価４０             TO 後療単価４０ＷＲ.
032840     MOVE レセ－後療回数４０             TO 後療回数４０ＷＲ.
032850     MOVE レセ－後療料４０               TO 後療料４０ＷＲ.
032860     MOVE レセ－冷罨法回数４０           TO 冷罨法回数４０ＷＲ.
032870     MOVE レセ－冷罨法料４０             TO 冷罨法料４０ＷＲ.
032880     MOVE レセ－温罨法回数４０           TO 温罨法回数４０ＷＲ.
032890     MOVE レセ－温罨法料４０             TO 温罨法料４０ＷＲ.
032900     MOVE レセ－電療回数４０             TO 電療回数４０ＷＲ.
032910     MOVE レセ－電療料４０               TO 電療料４０ＷＲ.
032920     MOVE レセ－小計４０                 TO 小計４０ＷＲ.
032930     MOVE レセ－長期逓減率４０           TO 長期逓減率４０ＷＲ.
032940     MOVE レセ－長期込小計４０           TO 長期込小計４０ＷＲ.
032950*    *****************
032960*    * ５部位／2.5割 *
032970*    *****************
032980     MOVE レセ－後療単価５２             TO 後療単価５２ＷＲ.
032990     MOVE レセ－後療回数５２             TO 後療回数５２ＷＲ.
033000     MOVE レセ－後療料５２               TO 後療料５２ＷＲ.
033010     MOVE レセ－冷罨法回数５２           TO 冷罨法回数５２ＷＲ.
033020     MOVE レセ－冷罨法料５２             TO 冷罨法料５２ＷＲ.
033030     MOVE レセ－温罨法回数５２           TO 温罨法回数５２ＷＲ.
033040     MOVE レセ－温罨法料５２             TO 温罨法料５２ＷＲ.
033050     MOVE レセ－電療回数５２             TO 電療回数５２ＷＲ.
033060     MOVE レセ－電療料５２               TO 電療料５２ＷＲ.
033070     MOVE レセ－小計５２                 TO 小計５２ＷＲ.
033080     MOVE レセ－多部位込小計５２         TO 多部位込小計５２ＷＲ.
033090     MOVE レセ－長期逓減率５２           TO 長期逓減率５２ＷＲ.
033100     MOVE レセ－長期込小計５２           TO 長期込小計５２ＷＲ.
033110**    ****************
033120**    * ５部位／５割 *
033130**    ****************
033140*     MOVE レセ－逓減開始月５５           TO 逓減開始月５５ＷＲ.
033150*     MOVE レセ－逓減開始日５５           TO 逓減開始日５５ＷＲ.
033160*     MOVE レセ－後療単価５５             TO 後療単価５５ＷＲ.
033170*     MOVE レセ－後療回数５５             TO 後療回数５５ＷＲ.
033180*     MOVE レセ－後療料５５               TO 後療料５５ＷＲ.
033190*     MOVE レセ－冷罨法回数５５           TO 冷罨法回数５５ＷＲ.
033200*     MOVE レセ－冷罨法料５５             TO 冷罨法料５５ＷＲ.
033210*     MOVE レセ－温罨法回数５５           TO 温罨法回数５５ＷＲ.
033220*     MOVE レセ－温罨法料５５             TO 温罨法料５５ＷＲ.
033230*     MOVE レセ－電療回数５５             TO 電療回数５５ＷＲ.
033240*     MOVE レセ－電療料５５               TO 電療料５５ＷＲ.
033250*     MOVE レセ－小計５５                 TO 小計５５ＷＲ.
033260*     MOVE レセ－多部位込小計５５         TO 多部位込小計５５ＷＲ.
033270*     MOVE レセ－長期逓減率５５           TO 長期逓減率５５ＷＲ.
033280*     MOVE レセ－長期込小計５５           TO 長期込小計５５ＷＲ.
033290*    ****************
033300*    * ５部位／８割 *
033310*    ****************
029950     MOVE レセ－逓減開始月５８           TO 逓減開始月５８ＷＲ.
029960     MOVE レセ－逓減開始日５８           TO 逓減開始日５８ＷＲ.
029970     MOVE レセ－後療単価５８             TO 後療単価５８ＷＲ.
029980     MOVE レセ－後療回数５８             TO 後療回数５８ＷＲ.
029990     MOVE レセ－後療料５８               TO 後療料５８ＷＲ.
030000     MOVE レセ－冷罨法回数５８           TO 冷罨法回数５８ＷＲ.
030010     MOVE レセ－冷罨法料５８             TO 冷罨法料５８ＷＲ.
030020     MOVE レセ－温罨法回数５８           TO 温罨法回数５８ＷＲ.
030030     MOVE レセ－温罨法料５８             TO 温罨法料５８ＷＲ.
030040     MOVE レセ－電療回数５８             TO 電療回数５８ＷＲ.
030050     MOVE レセ－電療料５８               TO 電療料５８ＷＲ.
030060     MOVE レセ－小計５８                 TO 小計５８ＷＲ.
030070     MOVE レセ－多部位込小計５８         TO 多部位込小計５８ＷＲ.
030080     MOVE レセ－長期逓減率５８           TO 長期逓減率５８ＷＲ.
030090     MOVE レセ－長期込小計５８           TO 長期込小計５８ＷＲ.
033470*    ****************
033480*    * ５部位／10割 *
033490*    ****************
033500     MOVE レセ－逓減開始月５０           TO 逓減開始月５０ＷＲ.
033510     MOVE レセ－逓減開始日５０           TO 逓減開始日５０ＷＲ.
033520     MOVE レセ－後療単価５０             TO 後療単価５０ＷＲ.
033530     MOVE レセ－後療回数５０             TO 後療回数５０ＷＲ.
033540     MOVE レセ－後療料５０               TO 後療料５０ＷＲ.
033550     MOVE レセ－冷罨法回数５０           TO 冷罨法回数５０ＷＲ.
033560     MOVE レセ－冷罨法料５０             TO 冷罨法料５０ＷＲ.
033570     MOVE レセ－温罨法回数５０           TO 温罨法回数５０ＷＲ.
033580     MOVE レセ－温罨法料５０             TO 温罨法料５０ＷＲ.
033590     MOVE レセ－電療回数５０             TO 電療回数５０ＷＲ.
033600     MOVE レセ－電療料５０               TO 電療料５０ＷＲ.
033610     MOVE レセ－小計５０                 TO 小計５０ＷＲ.
033620     MOVE レセ－長期逓減率５０           TO 長期逓減率５０ＷＲ.
033630     MOVE レセ－長期込小計５０           TO 長期込小計５０ＷＲ.
033640*
033650*================================================================*
033660 施術記録取得 SECTION.
033670*
033680************************************************************
033690* 作１データから負傷データＦより以下の情報を取得           *
033700* ● 初検加算 .....区分によりチェックに"○"を格納...複数可 *
033710* ● 往療加算 .....区分によりチェックに"○"を格納...複数可 *
033720************************************************************
033730     MOVE  SPACE  TO  初日再検フラグ.
033740     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 部位数Ｗ
033750         IF ( 施術年Ｗ = 初検年Ｗ(部位ＣＮＴ) ) AND
033760            ( 施術月Ｗ = 初検月Ｗ(部位ＣＮＴ) )
033770             MOVE 患者番号ＷＲ          TO 施記－患者番号
033780             MOVE 枝番ＷＲ              TO 施記－枝番
033790             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
033800             MOVE 初検年Ｗ(部位ＣＮＴ)  TO 開始年Ｗ(部位ＣＮＴ) 施記－施術年
033810             MOVE 初検月Ｗ(部位ＣＮＴ)  TO 開始月Ｗ(部位ＣＮＴ) 施記－施術月
033820             MOVE 初検日Ｗ(部位ＣＮＴ)  TO 開始日Ｗ(部位ＣＮＴ) 施記－施術日
033830         ELSE
033840             MOVE 患者番号ＷＲ          TO 施記－患者番号
033850             MOVE 枝番ＷＲ              TO 施記－枝番
033860             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
033870             MOVE 施術年ＷＲ            TO 施記－施術年
033880             MOVE 施術月ＷＲ            TO 施記－施術月
033890             MOVE ZERO                  TO 施記－施術日
033900         END-IF
033910         START 施術記録Ｆ   KEY IS >= 施記－患者コード
033920                                      施記－施術和暦年月日
033930         END-START
033940         IF 状態キー = "00"
033960             MOVE ZERO  TO 終了年ＷＴ
033970             MOVE ZERO  TO 終了月ＷＴ
033980             MOVE ZERO  TO 終了日ＷＴ
033990             MOVE SPACE TO 終了フラグ２
034000             PERFORM 施術記録Ｆ読込
034010             IF  ( 終了フラグ２      = SPACE   ) AND
034020                 ( 施記－患者コード  = 患者コードＷＲ ) AND
034030                 ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
034040                 ( 施記－施術年      = 施術年ＷＲ     ) AND
034050                 ( 施記－施術月      = 施術月ＷＲ     ) 
034060*
034070*        *****************************************************************
034080*        * 開始年月日 ( その部位が当月初検でないか、
034090*                       当月初検でも枝番がある時は、最初の施術日を開始日)*
034100*        *****************************************************************
034110                 IF ( 施術年Ｗ NOT = 初検年Ｗ(部位ＣＮＴ) ) OR
034120                    ( 施術月Ｗ NOT = 初検月Ｗ(部位ＣＮＴ) ) OR
034130                    ( 開始診療日手動区分Ｗ = 1 )
034140                     MOVE 施記－施術年   TO 開始年Ｗ(部位ＣＮＴ)
034150                     MOVE 施記－施術月   TO 開始月Ｗ(部位ＣＮＴ)
034160                     MOVE 施記－施術日   TO 開始日Ｗ(部位ＣＮＴ)
034170                 END-IF
034180             END-IF
034190             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
034200                           ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
034210                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
034220                           ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
034230                           ( 施記－施術月     NOT = 施術月ＷＲ       ) OR
034240                           ( 施記－施術日         > 終了日Ｗ(部位ＣＮＴ))
034250*               **********
034260*               * 実日数 *
034270*               **********
034290                MOVE 施記－施術年               TO 終了年ＷＴ
034300                MOVE 施記－施術月               TO 終了月ＷＴ
034310                MOVE 施記－施術日               TO 終了日ＷＴ
034320*
034330                PERFORM 施術記録Ｆ読込
034340            END-PERFORM
034350        END-IF
034360*       **************************
034370*       * 継続：終了年月日セット *
034380*       **************************
034390        IF 転帰区分Ｗ(部位ＣＮＴ) = 9
034400            MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
034410            MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
034420            MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
034430        END-IF
034440        IF 終了年月日Ｗ(部位ＣＮＴ) > 受理年月日Ｗ
034450            MOVE 終了年Ｗ(部位ＣＮＴ) TO 受理年Ｗ
034460            MOVE 終了月Ｗ(部位ＣＮＴ) TO 受理月Ｗ
034470            MOVE 終了日Ｗ(部位ＣＮＴ) TO 受理日Ｗ
034480        END-IF
034490     END-PERFORM.
034500*
034510** ----- 前月初検のみかを判定 -----------*
034520*
034530*     MOVE 患者番号ＷＲ          TO 施記－患者番号.
034540*     MOVE 枝番ＷＲ              TO 施記－枝番.
034550*     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
034560*     MOVE 施術年ＷＲ            TO 施記－施術年.
034570*     MOVE 施術月ＷＲ            TO 施記－施術月.
034580*     MOVE ZERO                  TO 施記－施術日.
034590*     START 施術記録Ｆ   KEY IS >= 施記－患者コード
034600*                                  施記－施術和暦年月日
034610*     END-START.
034620*     IF 状態キー = "00"
034630*             MOVE SPACE TO 終了フラグ２
034640*             PERFORM 施術記録Ｆ読込
034650*             IF  ( 終了フラグ２      = SPACE   ) AND
034660*                 ( 施記－患者コード  = 患者コードＷＲ ) AND
034670*                 ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
034680*                 ( 施記－施術年      = 施術年ＷＲ     ) AND
034690*                 ( 施記－施術月      = 施術月ＷＲ     ) 
034700** 当月施術開始日が再検かどうか判定
034710*                 IF   施記－再検料請求 = 1
034720*                      MOVE "YES"  TO  初日再検フラグ
034730*                 END-IF
034740**
034750*             END-IF
034760*     END-IF.
034770*     IF 初日再検フラグ = "YES"
034780*        PERFORM 前月初検のみ判定
034790*     END-IF.
034800*
034810*================================================================*
034820 前月初検のみ判定 SECTION.
034830*
034840*** 前月の通院日が初検か判定 
034850     MOVE  SPACE            TO 前月フラグ.
034860     MOVE 受－患者コード    TO 施記－患者コード.
034870     MOVE 受－施術和暦      TO 施記－施術和暦.
034880     MOVE 受－施術年        TO 施記－施術年.
034890     MOVE 受－施術月        TO 施記－施術月.
034900     MOVE 1                 TO 施記－施術日.
034910     START 施術記録Ｆ   KEY IS <  施記－患者コード
034920                                  施記－施術和暦年月日
034930                                  REVERSED
034940     END-START.
034950     IF 状態キー = "00"
034960         MOVE SPACE  TO 終了フラグ２
034970         PERFORM 施術記録Ｆ読込
034980         IF ( 終了フラグ２      = SPACE  ) AND
034990            ( 施記－患者コード  = 受－患者コード ) AND
035000            ( 施記－診療区分    = 2 ) 
035010*
035020            PERFORM 前月判定
035030**** 適用１を使用
035040            IF 前月フラグ = "YES"
035050               MOVE NC"※前月初検のみ"    TO  適用１Ｗ
035060            END-IF
035070**
035080         END-IF
035090     END-IF.
035100*
035110*================================================================*
035120 前月判定  SECTION.
035130* 
035140*** 読み込んだ施術記録の年月が、前月かどうか判定 (年月の差が 1 か?)
035150      MOVE  SPACE  TO  前月フラグ.
035160      INITIALIZE  計算年月日Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
035170**
035180      MOVE 受－施術和暦    TO 終了和暦２Ｗ.
035190      MOVE 受－施術年      TO 終了年２Ｗ.
035200      MOVE 受－施術月      TO 終了月２Ｗ.
035210      MOVE 施記－施術和暦  TO 開始和暦２Ｗ.
035220      MOVE 施記－施術年    TO 開始年２Ｗ.
035230      MOVE 施記－施術月    TO 開始月２Ｗ.
035240*
035250      EVALUATE TRUE
035260       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ = 終了年２Ｗ)
035270            PERFORM  前月比較月
035280       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ NOT = 終了年２Ｗ)
035290            PERFORM  前月比較年
035300       WHEN  開始和暦２Ｗ NOT = 終了和暦２Ｗ 
035310            PERFORM  前月比較元号
035320      END-EVALUATE.
035330*
035340      IF 計算月Ｗ = 1
035350         MOVE  "YES"  TO  前月フラグ
035360      END-IF.
035370*
035380*================================================================*
035390 長期判定取得 SECTION.
035400*
035410* ３カ月以上の長期判定は "CHOUKI" を呼ぶ. 
035420     MOVE  SPACE TO  連期間－キー.
035430     INITIALIZE      連期間－キー.
035440     MOVE 施術和暦ＷＲ  TO  連期間－施術和暦.
035450     MOVE 施術年ＷＲ    TO  連期間－施術年.
035460     MOVE 施術月ＷＲ    TO  連期間－施術月.
035470     MOVE 患者番号ＷＲ  TO  連期間－患者番号.
035480     MOVE 枝番ＷＲ      TO  連期間－枝番.
035490*
035500     CALL   "CHOUKI".
035510     CANCEL "CHOUKI".
035520*
035530**** 適用１を使用 (「前月初検のみ」がある時は、くっつける)
035540     IF 連期間－対象フラグ  = "YES"
035550        IF 適用１Ｗ  = SPACE
035560           MOVE NC"※長期施術継続理由裏面に記載"  TO 適用１Ｗ
035570        ELSE
035580           STRING 適用１Ｗ           DELIMITED BY SPACE
035590                  NC"，"             DELIMITED BY SIZE
035600                  NC"※長期施術継続理由裏面に記載"   DELIMITED BY SIZE
035610                  INTO 適用１Ｗ
035620           END-STRING
035630        END-IF
035640     END-IF.
035650*
035660*================================================================*
035670 初検日以前のデータ判定 SECTION.
035680*
035690*********************************************************************************
035700*  最初の初検日以前の当月中に施術記録レコードがあった時(治癒、中止)は、請求区分の
035710*  継続にもチェックする。(新規と継続の両方)
035720*********************************************************************************
035730** 最初の初検日を取得
035740     MOVE SPACE                 TO 初検フラグ.
035750     MOVE 患者番号ＷＲ          TO 施記－患者番号.
035760     MOVE 枝番ＷＲ              TO 施記－枝番.
035770     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
035780     MOVE 施術年ＷＲ            TO 施記－施術年.
035790     MOVE 施術月ＷＲ            TO 施記－施術月.
035800     MOVE ZERO                  TO 施記－施術日.
035810     START 施術記録Ｆ   KEY IS >= 施記－患者コード
035820                                  施記－施術和暦年月日
035830     END-START.
035840     IF 状態キー = "00"
035850         MOVE ZERO  TO 初検和暦ＷＴ
035860         MOVE ZERO  TO 初検年ＷＴ
035870         MOVE ZERO  TO 初検月ＷＴ
035880         MOVE ZERO  TO 初検日ＷＴ
035890         MOVE SPACE TO 終了フラグ２
035900         PERFORM 施術記録Ｆ読込
035910         PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
035920                       ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
035930                       ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
035940                       ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
035950                       ( 施記－施術月     NOT = 施術月ＷＲ      ) OR
035960                       ( 初検フラグ           = "YES"           ) 
035970               IF  施記－診療区分 = 2
035980                   MOVE 施記－施術和暦           TO 初検和暦ＷＴ
035990                   MOVE 施記－施術年             TO 初検年ＷＴ
036000                   MOVE 施記－施術月             TO 初検月ＷＴ
036010                   MOVE 施記－施術日             TO 初検日ＷＴ
036020                   MOVE "YES"                    TO 初検フラグ
036030               END-IF
036040               PERFORM 施術記録Ｆ読込
036050         END-PERFORM
036060     END-IF.
036070*
036080* 初検日以前のデータ判定
036090     IF 初検フラグ = "YES"
036100        MOVE 患者番号ＷＲ          TO 施記－患者番号
036110        MOVE 枝番ＷＲ              TO 施記－枝番
036120        MOVE 初検和暦ＷＴ          TO 施記－施術和暦
036130        MOVE 初検年ＷＴ            TO 施記－施術年
036140        MOVE 初検月ＷＴ            TO 施記－施術月
036150        MOVE 初検日ＷＴ            TO 施記－施術日
036160        START 施術記録Ｆ   KEY IS <  施記－患者コード
036170                                     施記－施術和暦年月日
036180                                     REVERSED
036190        END-START
036200        IF 状態キー = "00"
036210           MOVE SPACE  TO 終了フラグ２
036220           PERFORM 施術記録Ｆ読込
036230           IF ( 終了フラグ２    = SPACE        ) AND
036240              ( 施記－患者番号  = 患者番号ＷＲ ) AND
036250              ( 施記－枝番      = 枝番ＷＲ     ) AND
036260              ( 施記－施術和暦  = 初検和暦ＷＴ ) AND
036270              ( 施記－施術年    = 初検年ＷＴ   ) AND
036280              ( 施記－施術月    = 初検月ＷＴ   )
036290*  初検日以前の当月中に施術記録レコードがあった時
036300                IF 継続チェックＷ = SPACE
036310                   MOVE NC"○"    TO 継続チェックＷ
036320                END-IF
036330           END-IF
036340         END-IF
036350     END-IF.
036360*
036370*================================================================*
036380 初検加算時刻取得 SECTION.
036390*****************************************************************
036400** 初検加算が時間外と深夜の時、適用に「受付時間」を印字する。
036410**   時刻の印字は月3回まで可能
036420*****************************************************************
036430     IF ( レセ－時間外 = 1 ) OR ( レセ－深夜 = 1 ) OR ( レセ－休日 = 1 )
036440*
036450         MOVE 患者番号ＷＲ          TO 施記－患者番号
036460         MOVE 枝番ＷＲ              TO 施記－枝番
036470         MOVE 施術和暦ＷＲ          TO 施記－施術和暦
036480         MOVE 施術年ＷＲ            TO 施記－施術年
036490         MOVE 施術月ＷＲ            TO 施記－施術月
036500         MOVE ZERO                  TO 施記－施術日
036510         START 施術記録Ｆ   KEY IS >= 施記－患者コード
036520                                      施記－施術和暦年月日
036530         END-START
036540         IF 状態キー = "00"
036550             MOVE ZERO  TO 初検加算カウント
036560             MOVE SPACE TO 終了フラグ２
036570             PERFORM 施術記録Ｆ読込
036580             PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
036590                           ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
036600                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
036610                           ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
036620                           ( 施記－施術月     NOT = 施術月ＷＲ      ) 
036630                   IF  ( 施記－初検加算 = 1 OR 2 OR 3 ) AND ( 施記－診療区分 = 2 )
036640                       COMPUTE 初検加算カウント = 初検加算カウント  + 1
036650                       IF  初検加算カウント <= 3
036660                           MOVE 施記－初検加算 TO 初検加算区分ＷＴ(初検加算カウント)
036670                           MOVE 施記－受付時   TO 初検加算時ＷＴ(初検加算カウント)
036680                           MOVE 施記－受付分   TO 初検加算分ＷＴ(初検加算カウント)
036690                       END-IF
036700                   END-IF
036710                   PERFORM 施術記録Ｆ読込
036720             END-PERFORM
036730** 初検加算の時刻を適用にセット
033380            IF ( 初検加算時ＷＴ(1) NOT = ZERO ) OR ( 初検加算分ＷＴ(1) NOT = ZERO ) 
                     MOVE 初検加算時ＷＴ(1) TO 初検加算時Ｗ
                     MOVE ":"               TO 初検加算区切Ｗ
                     MOVE 初検加算分ＷＴ(1) TO 初検加算分Ｗ
                  END-IF
033380            IF ( 初検加算時ＷＴ(2) NOT = ZERO ) OR ( 初検加算分ＷＴ(2) NOT = ZERO ) 
031910               PERFORM 初検加算適用セット
                  END-IF
036750         END-IF
036760*
036770     END-IF.
036780*
036790*================================================================*
036800 初検加算適用セット SECTION.
036810*
036820     PERFORM VARYING 番号カウンタ FROM 1 BY 1
036830              UNTIL  番号カウンタ > 3
036840         IF ( 初検加算時ＷＴ(番号カウンタ)  = ZERO )  AND 
036850            ( 初検加算分ＷＴ(番号カウンタ)  = ZERO ) 
036860             CONTINUE
036870         ELSE
036880* 固定項目
036890             EVALUATE 初検加算区分ＷＴ(番号カウンタ) 
036900             WHEN 1
036910                MOVE NC"時間外"   TO 加算内容Ｗ(番号カウンタ)
036900             WHEN 2
036910                MOVE NC"休　日"   TO 加算内容Ｗ(番号カウンタ)
036920             WHEN 3
036930                MOVE NC"深　夜"   TO 加算内容Ｗ(番号カウンタ)
036940             END-EVALUATE
036950*
036960             MOVE NC"："          TO 加算区切Ｗ(番号カウンタ)
036970             MOVE NC"時"          TO 時固定Ｗ(番号カウンタ)
036980             MOVE NC"分"          TO 分固定Ｗ(番号カウンタ)
036990*
037000**** 数字→日本語変換
037010* 時間
037020             MOVE 初検加算時ＷＴ(番号カウンタ)  TO  数字Ｗ
037030             IF 数字Ｗ >= 10
037040                 MOVE 数字Ｗ１    TO 負傷番号Ｗ１
037050                 PERFORM 日本語変換
037060                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ１(番号カウンタ)
037070                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
037080                 PERFORM 日本語変換
037090                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
037100             ELSE
037110                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
037120                 PERFORM 日本語変換
037130                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
037140             END-IF
037150* 分
037160             MOVE 初検加算分ＷＴ(番号カウンタ)  TO  数字Ｗ
037170             MOVE 数字Ｗ１    TO 負傷番号Ｗ１
037180             PERFORM 日本語変換
037190             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ１(番号カウンタ)
037200             MOVE 数字Ｗ２    TO 負傷番号Ｗ１
037210             PERFORM 日本語変換
037220             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ２(番号カウンタ)
037230** 
037240        END-IF
037250     END-PERFORM.
037260*
037270     MOVE  初検加算集団ＮＷ(1)   TO 初検加算時刻１Ｗ. 
037280     MOVE  初検加算集団ＮＷ(2)   TO 初検加算時刻２Ｗ. 
037290     MOVE  初検加算集団ＮＷ(3)   TO 初検加算時刻３Ｗ. 
037300*
037310**** 適用１か２を使用（長期理由記載で適用１を使っている時は、適用２）
037320     IF ( 初検加算時ＷＴ(2)  = ZERO ) AND ( 初検加算分ＷＴ(2)  = ZERO ) 
037330         CONTINUE
037340     ELSE
037350         IF 適用１Ｗ  = SPACE
037360               STRING NC"初検加算"       DELIMITED BY SIZE
037370                      初検加算時刻１Ｗ   DELIMITED BY SIZE
037380                      初検加算時刻２Ｗ   DELIMITED BY SIZE
037390                      初検加算時刻３Ｗ   DELIMITED BY SIZE
037400                      INTO 適用１Ｗ
037410               END-STRING
037420         ELSE
037430               STRING NC"初検加算"       DELIMITED BY SIZE
037440                      初検加算時刻１Ｗ   DELIMITED BY SIZE
037450                      初検加算時刻２Ｗ   DELIMITED BY SIZE
037460                      初検加算時刻３Ｗ   DELIMITED BY SIZE
037470                      INTO 適用２Ｗ
037480               END-STRING
037490         END-IF
037500     END-IF.
037510*
037520*================================================================*
037530 日本語変換 SECTION.
037540*
037550     MOVE NC"０"     TO 全角負傷番号Ｗ.
037560     CALL "htoz" WITH C LINKAGE
037570                        USING 負傷番号Ｗ１ 全角負傷番号Ｗ１.
037580*
037590*================================================================*
037600 助成印取得 SECTION.
037610*
036080     MOVE SPACE TO  連助成名称－キー.
036090     INITIALIZE     連助成名称－キー.
036100     MOVE 助成種別ＷＲ           TO 連助成名称－助成種別.
036110     MOVE 費用負担者番号助成ＷＲ TO 連助成名称－費用負担者番号助成.
036120*
036130     CALL   "JOSEIMEI".
036140     CANCEL "JOSEIMEI".
036150*
036160     MOVE 連助成名称－１文字 TO 助成印Ｗ.
037590**================================================================*
037600* 助成印取得 SECTION.
037610**
037620******************************************
037630**  助成がある時、助成種別を印字する。
037640******************************************
037650**
037660*     EVALUATE 助成種別ＷＲ 
037670**** 生保 (生保はその他扱いで、該当なし)
037680*     WHEN  50
037690*         CONTINUE
037700**** 41老人
037710*     WHEN  51
037720************ 頭4桁が "4113"東京 "4108"茨城 "4132"島根 の時は、「福」。それ以外は「老」
037730*        IF  ( 印刷市町村番号Ｗ(1:4) = "4113" )  OR
037740*            ( 印刷市町村番号Ｗ(1:4) = "4108" )  OR
037750*            ( 印刷市町村番号Ｗ(1:4) = "4132" )  
037760*            MOVE NC"福"    TO 助成印Ｗ
037770*        ELSE
037780*            MOVE NC"老"    TO 助成印Ｗ
037790*        END-IF
037800**** 母子
037810*     WHEN  52
037820*            MOVE NC"母"    TO 助成印Ｗ
037830****            MOVE NC"親"    TO 助成印Ｗ
037840**** 身障
037850*     WHEN  53
037860*            MOVE NC"障"    TO 助成印Ｗ
037870**** 被爆
037880*     WHEN  54
037890*            MOVE NC"爆"    TO 助成印Ｗ
037900**** 乳幼児 
037910*     WHEN  55
037920*            MOVE NC"乳"    TO 助成印Ｗ
037930**** その他
037940*     WHEN  60
037950*            CONTINUE
037960*     WHEN  OTHER
037970*            CONTINUE
037980*     END-EVALUATE.
037990**
038000*================================================================*
038010 基本料取得 SECTION.
038020*
038030     MOVE 01                TO 料Ａ－区分コード.
038040     MOVE ZERO              TO 料Ａ－負傷種別.
038050     MOVE ZERO              TO 料Ａ－部位.
038060     MOVE ZERO              TO 料Ａ－左右区分.
038070     MOVE ZERO              TO 料Ａ－負傷位置番号.
038080     MOVE 施術和暦ＷＲ      TO 料Ａ－開始和暦.
038090     MOVE 施術年ＷＲ        TO 料Ａ－開始年.
038100     MOVE 施術月ＷＲ        TO 料Ａ－開始月.
038110     START 料金マスタ KEY IS <= 料－区分コード
038120                                料－部位コード
038130                                料－開始和暦年月
038140                                REVERSED
038150     END-START.
038160     READ 料金マスタ NEXT
038170     NOT AT END
038180         MOVE 料Ａ－冷罨法料          TO 冷罨法単価Ｗ
038190         MOVE 料Ａ－温罨法料          TO 温罨法単価Ｗ
038200         MOVE 料Ａ－電療料            TO 電療単価Ｗ
038210     END-READ.
038220*
038230*================================================================*
038240 施術西暦年取得 SECTION.
038250*
038260     MOVE 施術和暦ＷＲ TO 元－元号区分.
038270     READ 元号マスタ
038280     NOT INVALID KEY
038290         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
038300     END-READ.
038310     IF 施術西暦年Ｗ NOT = ZERO
038320        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
038330     END-IF.
038340     MOVE 施術西暦年Ｗ  TO レセプト管理年Ｗ.
038350*
038360*================================================================*
038370 委任年月日取得 SECTION.
038380*
038390** ---// ここの受理年には、最終通院日が入っている為、退避する //----
038400     MOVE 受理年Ｗ   TO 最終通院年Ｗ.
038410     MOVE 受理月Ｗ   TO 最終通院月Ｗ.
038420     MOVE 受理日Ｗ   TO 最終通院日Ｗ.
038430***
038440* (柔整師側)
038450     EVALUATE レセプト日付区分Ｗ 
038460*    /  最終通院日 /
038470     WHEN ZERO
038480         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
038490         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
038500         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
038510*    /  月末日 /
038520     WHEN 1 
038530         PERFORM 月末日取得
038540         MOVE 受理年Ｗ     TO 柔整師年Ｗ
038550         MOVE 受理月Ｗ     TO 柔整師月Ｗ
038560         MOVE 受理日Ｗ     TO 柔整師日Ｗ
038570*    /  印字なし /
038580     WHEN 9
038590         MOVE ZERO         TO 柔整師年Ｗ
038600         MOVE ZERO         TO 柔整師月Ｗ
038610         MOVE ZERO         TO 柔整師日Ｗ
038620*    /  その他は、最終通院日 /
038630     WHEN OTHER
038640         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
038650         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
038660         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
038670     END-EVALUATE.
038680**
038690* (患者側)
038700     EVALUATE レセプト患者日付区分Ｗ 
038710*    /  最終通院日 /
038720     WHEN ZERO
038730         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
038740         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
038750         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
038760*    /  月末日 /
038770     WHEN 1 
038780         PERFORM 月末日取得
038790         MOVE 受理年Ｗ     TO 患者委任年Ｗ
038800         MOVE 受理月Ｗ     TO 患者委任月Ｗ
038810         MOVE 受理日Ｗ     TO 患者委任日Ｗ
038820*    /  印字なし /
038830     WHEN 9
038840         MOVE ZERO         TO 患者委任年Ｗ
038850         MOVE ZERO         TO 患者委任月Ｗ
038860         MOVE ZERO         TO 患者委任日Ｗ
038870*    /  その他は、最終通院日 /
038880     WHEN OTHER
038890         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
038900         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
038910         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
038920     END-EVALUATE.
038930*
038940*================================================================*
038950 月末日取得 SECTION.
038960*
038970     MOVE 施術年ＷＲ   TO 受理年Ｗ.
038980     MOVE 施術月ＷＲ   TO 受理月Ｗ.
038990     MOVE 施術和暦ＷＲ TO 元－元号区分.
039000     READ 元号マスタ
039010     NOT INVALID KEY
039020         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
039030     END-READ.
039040     IF 施術西暦年Ｗ NOT = ZERO
039050        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
039060     END-IF.
039070*
039080     EVALUATE 施術月ＷＲ
039090     WHEN 4
039100     WHEN 6
039110     WHEN 9
039120     WHEN 11
039130         MOVE 30 TO 受理日Ｗ
039140     WHEN 2
039150         DIVIDE 4 INTO 施術西暦年Ｗ GIVING    商Ｗ
039160                                    REMAINDER 余Ｗ
039170         END-DIVIDE
039180         IF 余Ｗ = ZERO
039190             MOVE 29 TO 受理日Ｗ
039200         ELSE
039210             MOVE 28 TO 受理日Ｗ
039220         END-IF
039230     WHEN 1
039240     WHEN 3
039250     WHEN 5
039260     WHEN 7
039270     WHEN 8
039280     WHEN 10
039290     WHEN 12
039300         MOVE 31 TO 受理日Ｗ
039310     WHEN OTHER
039320          CONTINUE
039330     END-EVALUATE.
039340*
039350*================================================================*
039360 負傷原因取得 SECTION.
039370*
039380********************************************************************
039390*  負傷原因コードが同じものは、1行にまとめて印字する。
039400*  例: ①② 家で転んだ.
039410*     負傷原因コードが同じものをまとめ、テーブルにセット
039420*     (ただし、部位を飛んで同じものは、2行になる)
039430********************************************************************
039440     MOVE  ZERO   TO  カウンタ カウンタ２.
039450     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
039460             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
039470*
039480****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
039490        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
039500*
039510           IF カウンタ = ZERO
039520               MOVE 1   TO  カウンタ カウンタ２
039530               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
039540               MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
039550               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
039560           ELSE
039570              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
039580                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
039590                 COMPUTE カウンタ２ = カウンタ２  +  1
039600                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
039610              ELSE
039620                 COMPUTE カウンタ = カウンタ  +  1
039630                 MOVE 1   TO  カウンタ２
039640                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
039650                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
039660                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
039670              END-IF
039680           END-IF
039690        END-IF
039700     END-PERFORM.
039710**************************************************************************
039720*  負傷原因マスタより文章取得
039730**************************************************************************
039740     MOVE  ZERO   TO  カウンタ カウンタ２.
039750     PERFORM VARYING カウンタ FROM 1 BY 1
039760             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
039770** 健保は 区分 01
039780         MOVE 01                        TO 負原－区分コード
039790         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
039800         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
039810         READ 負傷原因Ｆ
039820         NOT INVALID KEY
039830             INITIALIZE 負傷原因ＷＴ
039840             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
039850             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
039860             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
039870             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
039880             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
039890             PERFORM VARYING カウンタ２ FROM 1 BY 1
039900                     UNTIL ( カウンタ２ > 9 )  OR 
039910                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
039920                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
039930                WHEN 1
039940                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
039950                WHEN 2
039960                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
039970                WHEN 3
039980                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
039990                WHEN 4
040000                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
040010                WHEN 5
040020                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
039990                WHEN 6
040000                   MOVE "⑥"  TO  負傷原因ナンバーＷ１(カウンタ２)
040010                WHEN 7
040020                   MOVE "⑦"  TO  負傷原因ナンバーＷ１(カウンタ２)
040030                WHEN OTHER
040040                   CONTINUE
040050                END-EVALUATE
040060             END-PERFORM
040070*
040152             IF 負原－負傷原因入力区分 = 1
040153                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
040154                        負傷原因１ＷＴ  DELIMITED BY SIZE
040155                        負傷原因２ＷＴ  DELIMITED BY SIZE
040156                        負傷原因３ＷＴ  DELIMITED BY SIZE
040157                        負傷原因４ＷＴ  DELIMITED BY SIZE
040158                        負傷原因５ＷＴ  DELIMITED BY SIZE
040159                        INTO 負傷原因内容合成Ｗ(カウンタ)
040160                 END-STRING
040161             ELSE
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
040170             END-IF
040171*
040172         END-READ
040173     END-PERFORM.
040180*
040190*     PERFORM 負傷原因セット.
035680     PERFORM 全負傷原因合体セット.
040200*
040210*================================================================*
040220 負傷原因セット SECTION.
040230*
040240**************************************************************************
040250*  文章が1行を超える時は、複数行に分解する。
040260**************************************************************************
040270     MOVE  ZERO   TO  カウンタ カウンタ２.
040280     PERFORM VARYING カウンタ FROM 1 BY 1
040290             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
040300*
040520        INITIALIZE 負傷原因内容分解ＸＷ
040530        MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
040540        IF ( 負傷原因内容１ＸＷ  NOT = SPACE )
040550           COMPUTE カウンタ２ = カウンタ２  +  1
040560           MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
040570        END-IF
040580        IF ( 負傷原因内容２ＸＷ  NOT = SPACE )
040590           COMPUTE カウンタ２ = カウンタ２  +  1
040600           MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
040610        END-IF
034690        IF  負傷原因内容３ＸＷ  NOT = SPACE
034700            COMPUTE カウンタ２ = カウンタ２  +  1
034710            MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
034720        END-IF
034690        IF  負傷原因内容４ＸＷ  NOT = SPACE
034700            COMPUTE カウンタ２ = カウンタ２  +  1
034710            MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
034720        END-IF
040450*
040460     END-PERFORM.
035700*================================================================*
035710 全負傷原因合体セット SECTION.
035720*
035730**************************************************************************
035740*  文章が1行を超える時は、複数行に分解する。
035750**************************************************************************
           MOVE 負傷原因内容合成Ｗ(1) TO 文字１Ｗ.
007270     PERFORM VARYING カウンタ FROM 2 BY 1
007280             UNTIL ( カウンタ > 9 )  OR  ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
               MOVE 負傷原因内容合成Ｗ(カウンタ) TO 文字２Ｗ
006966         CALL プログラム名Ｗ WITH C LINKAGE
006967                             USING BY REFERENCE 文字１Ｗ
006968                                   BY REFERENCE 文字２Ｗ
           END-PERFORM.
035760     MOVE  文字１Ｗ   TO  負傷原因１文Ｗ.
035760     MOVE  ZERO   TO  カウンタ.
035770     PERFORM VARYING カウンタ FROM 1 BY 1
035780             UNTIL ( カウンタ > 7 )
035790*
035910        MOVE 負傷原因１文ＷＰ(カウンタ)  TO 負傷原因Ｗ(カウンタ)
035980*
035990     END-PERFORM.
036000*
040470*================================================================*
040480 長期理由文取得 SECTION.
040490*
040500* 長期理由文取得は "CHOUBUN" を呼ぶ. 
040510     MOVE  SPACE TO  連長文－キー.
040520     INITIALIZE      連長文－キー.
040530     MOVE 施術和暦ＷＲ  TO  連長文－施術和暦.
040540     MOVE 施術年ＷＲ    TO  連長文－施術年.
040550     MOVE 施術月ＷＲ    TO  連長文－施術月.
040560     MOVE 患者番号ＷＲ  TO  連長文－患者番号.
040570     MOVE 枝番ＷＲ      TO  連長文－枝番.
040580** 日接用は56桁
040590     MOVE 56            TO  連長文－文桁数.
040600*
040610     CALL   "CHOUBUN".
040620     CANCEL "CHOUBUN".
040630*
040640*
040650*================================================================*
040660 負担割合取得 SECTION.
040670*================================================================*
040680* ※ 本人負担割合ではなく、給付率
040700*     COMPUTE 負担割合数字Ｗ = 連計－負担率 / 10.
040710*     COMPUTE 負担割合Ｗ     = 10 - 負担割合数字Ｗ.
040720*     MOVE NC"割"           TO  割合固定Ｗ.
040721*
040722* 14/10～　本人負担割合に変更
040723* 15/06 給付割合に変更
      * 19/07 負担割合に変更/0707
040724     MOVE レセ－負担割合 TO 負担割合Ｗ.
040726     MOVE NC"割"        TO  割合固定Ｗ.
040730*
040740*================================================================*
040750 施術記録Ｆ読込 SECTION.
040760*
040770     READ 施術記録Ｆ NEXT
040780     AT END
040790         MOVE "YES" TO 終了フラグ２
040800     END-READ.
040810*================================================================*
040820 前月比較月  SECTION.
040830*
040840     IF  終了月２Ｗ >  開始月２Ｗ
040850         COMPUTE 計算月Ｗ = 終了月２Ｗ - 開始月２Ｗ
040860     ELSE
040870        MOVE ZERO TO 計算月Ｗ
040880     END-IF.
040890*
040900*================================================================*
040910 前月比較年  SECTION.
040920*
040930     IF  終了年２Ｗ >  開始年２Ｗ
040940         COMPUTE 計算年Ｗ = 終了年２Ｗ - 開始年２Ｗ
040950         COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
040960     ELSE
040970        MOVE ZERO TO 計算月Ｗ
040980     END-IF.
040990*
041000*================================================================*
041010 前月比較元号  SECTION.
041020*
041030     MOVE 開始和暦２Ｗ TO 元－元号区分.
041040     READ 元号マスタ
041050     NOT INVALID KEY
041060         MOVE 元－開始西暦年 TO 開始西暦年Ｗ
041070     END-READ.
041080     MOVE 終了和暦２Ｗ TO 元－元号区分.
041090     READ 元号マスタ
041100     NOT INVALID KEY
041110         MOVE 元－開始西暦年 TO 終了西暦年Ｗ
041120     END-READ.
041130**
041140     IF (開始西暦年Ｗ NOT = ZERO) AND (終了西暦年Ｗ NOT = ZERO)
041150        COMPUTE 開始西暦年Ｗ = 開始西暦年Ｗ + 開始年２Ｗ - 1
041160        COMPUTE 終了西暦年Ｗ = 終了西暦年Ｗ + 終了年２Ｗ - 1
041170*
041180        IF 終了西暦年Ｗ =  開始西暦年Ｗ
041190           PERFORM  前月比較月
041200        ELSE
041210           IF  終了西暦年Ｗ >  開始西暦年Ｗ
041220               COMPUTE 計算年Ｗ = 終了西暦年Ｗ - 開始西暦年Ｗ
041230               COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
041240           ELSE
041250               MOVE ZERO TO 計算月Ｗ
041260           END-IF
041270        END-IF
041280     ELSE
041290        MOVE ZERO TO 計算月Ｗ
041300     END-IF.
041310*
041320*================================================================*
041330 レセプト並び順取得 SECTION.
041340*
041350     MOVE 施術和暦ＷＲ       TO 作２－施術和暦.
041360     MOVE 施術年ＷＲ         TO 作２－施術年.
041370     MOVE 施術月ＷＲ         TO 作２－施術月.
041380     MOVE 患者コードＷＲ     TO 作２－患者コード.
041390     MOVE 保険種別ＷＲ       TO 作２－保険種別.
           IF 連レ－保険種別 < 50
041390         MOVE 保険種別ＷＲ   TO 作２－保険種別
           ELSE
032090         MOVE 助成種別ＷＲ   TO 作２－保険種別
           END-IF.

041400     READ 作業ファイル２
041410     NOT INVALID KEY
041420          MOVE 作２－順番    TO 総括表順番Ｗ
041430     END-READ.
041440*
041450*================================================================*
041460 印刷処理 SECTION.
041470*
041480     EVALUATE 保険種別ＷＲ
041490     WHEN 01
041500     WHEN 04
041510     WHEN 08
041520     WHEN 09
           WHEN 05
041530        MOVE "YAS6125P"  TO  定義体名Ｐ
041540        MOVE "GRP002"   TO  項目群名Ｐ
041550        WRITE YAS6125P
041560****        WRITE 印刷レコード
041570        PERFORM エラー処理Ｐ
041580     WHEN OTHER
041590        MOVE "YAS6125P"  TO  定義体名Ｐ
041600        MOVE "GRP001"   TO  項目群名Ｐ
041610        WRITE YAS6125P
041620****        WRITE 印刷レコード
041630        PERFORM エラー処理Ｐ
041640     END-EVALUATE.
041650*
041660     MOVE "YAS6125P"  TO  定義体名Ｐ.
041670     MOVE "SCREEN"   TO  項目群名Ｐ.
041680     WRITE YAS6125P.
041690****     WRITE 印刷レコード.
041700     PERFORM エラー処理Ｐ.
041710*================================================================*
041720 エラー処理Ｐ SECTION.
041730*
041740     IF 通知情報Ｐ NOT = "00"
041750         DISPLAY NC"帳票エラー"              UPON CONS
041760         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
041770         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
041780         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
041790         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
041800                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
041810         ACCEPT  キー入力 FROM CONS
041820         PERFORM ファイル閉鎖
041830         MOVE 99  TO PROGRAM-STATUS
041840         EXIT PROGRAM
041850     END-IF.
041860*================================================================*
041870 受診者印刷区分更新 SECTION.
041880*
041890** //  受診者情報Ｆの印刷区分に１をセットし、更新する。//  
041900*
041910     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
041920     MOVE 施術年ＷＲ         TO 受－施術年.
041930     MOVE 施術月ＷＲ         TO 受－施術月.
041940     MOVE 患者コードＷＲ     TO 受－患者コード.
041950     READ 受診者情報Ｆ
041960     NOT INVALID KEY
041970         MOVE  1  TO  受－レセ印刷区分
041980         REWRITE  受－レコード
041990         END-REWRITE
042000         IF 状態キー NOT = "00"
042010            MOVE NC"受診者" TO ファイル名
042020            PERFORM エラー表示
042030         END-IF
042040     END-READ.
042050*
040580*================================================================*
040590 摘要文取得 SECTION.
040600*
040610* 摘要文取得は "TEKIYBUN" を呼ぶ. 
040620     MOVE  SPACE TO  連摘文－キー.
040630     INITIALIZE      連摘文－キー.
           INITIALIZE      連長頻－キー.
040640     MOVE 施術和暦ＷＲ  TO  連摘文－施術和暦.
040650     MOVE 施術年ＷＲ    TO  連摘文－施術年.
040660     MOVE 施術月ＷＲ    TO  連摘文－施術月.
040670     MOVE 患者番号ＷＲ  TO  連摘文－患者番号.
040680     MOVE 枝番ＷＲ      TO  連摘文－枝番.
040700*     MOVE 63            TO  連摘文－文桁数.
039370     MOVE 56            TO  連摘文－文桁数.
015000     IF (レセ長期理由印刷区分Ｗ NOT = 1 )
               MOVE 長期理由印刷区分Ｗ TO 連摘文－長期区分
           ELSE
               MOVE 1                  TO 連摘文－長期区分
015050     END-IF.
040710*
040720     CALL   "TEKIYBUN".
040730     CANCEL "TEKIYBUN".
040740*/42505
           IF 連長頻－頻回フラグ = "YES"
              MOVE NC"："       TO 頻回印
           END-IF.
           IF 連長頻－長期フラグ = "YES"
              MOVE NC"∴"       TO 長期印
           END-IF.
      *
042060*================================================================*
042070 レセ摘要再セット SECTION.
043230*---------------------------------------------------------------*
043240* 摘要ファイルがあれば長期理由の前に再セットする。
043250* （無ければ何もしない、つまり長期理由はそのまま）
043260*---------------------------------------------------------------*
           PERFORM 摘要文取得.
           MOVE 連摘文－摘要文(1)    TO 長期理由文１.
           MOVE 連摘文－摘要文(2)    TO 長期理由文２.
           MOVE 連摘文－摘要文(3)    TO 長期理由文３.
           MOVE 連摘文－摘要文(4)    TO 長期理由文４.
           MOVE 連摘文－摘要文(5)    TO 長期理由文５.
           MOVE 連摘文－摘要文(6)    TO 長期理由文６.
           MOVE 連摘文－摘要文(7)    TO 長期理由文７.
           MOVE 連摘文－摘要文(8)    TO 長期理由文８.
042250*
044960*================================================================*
044961 負傷原因印刷対象判定処理 SECTION.
044963*------------------------------------------------------------------------------------*
044964* 制御マスタの「負傷原因印刷区分」が 3 （３部位以上印刷）の時、３部位以上か判定して、
044965* その時のみ、負傷原因を印刷する。
044966*------------------------------------------------------------------------------------*
044967*
044979     MOVE  SPACE TO  連レセ負原印－キー.
044980     INITIALIZE      連レセ負原印－キー.
044981     MOVE 施術和暦ＷＲ  TO  連レセ負原印－施術和暦.
044982     MOVE 施術年ＷＲ    TO  連レセ負原印－施術年.
044983     MOVE 施術月ＷＲ    TO  連レセ負原印－施術月.
044984     MOVE 患者番号ＷＲ  TO  連レセ負原印－患者番号.
044985     MOVE 枝番ＷＲ      TO  連レセ負原印－枝番.
044986     CALL   "RECEHUGE".
044987     CANCEL "RECEHUGE".
044989*
044990     IF 連レセ負原印－対象フラグ = "YES"
044991        PERFORM 負傷原因取得
044992     END-IF.
044993*
042260*================================================================*
042270*================================================================*
042280 エラー表示 SECTION.
042290*
042300     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
042310     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
042320     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
042330     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
042340     ACCEPT  キー入力 FROM CONS
042350     PERFORM ファイル閉鎖.
042360     EXIT PROGRAM.
042370*================================================================*
042380 終了処理 SECTION.
042390*
042400     PERFORM ファイル閉鎖.
042410*================================================================*
042420 ファイル閉鎖 SECTION.
042430*
042440     CLOSE 印刷ファイル.
042450     CLOSE 保険者マスタ     元号マスタ          名称マスタ
042460           レセプトＦ       制御情報マスタ      施術所情報マスタ
042470           経過マスタ       受診者情報Ｆ        会情報マスタ
042480           施術記録Ｆ       負傷データＦ        負傷原因Ｆ
042490           料金マスタ       請求先マスタ        ＩＤ管理マスタ
042500           作業ファイル２   市町村マスタ        計算マスタ.
042510*
042520*================================================================*
042530*================================================================*
042540 テスト印字処理 SECTION.
042550     MOVE ALL "X"    TO 接骨師会会員番号１ 接骨師会会員番号２
042560                        県施術ＩＤ 共済番号.
042570     MOVE ALL NC"Ｎ" TO 県共済固定.
042580     MOVE ALL NC"Ｎ" TO 助成印１ 助成印２.
042590     MOVE NC"○"     TO 助成印固定１ 助成印固定２.
042600*     MOVE ALL NC"Ｎ" TO 施術和暦１ 施術和暦２.
042610     MOVE 99         TO 施術年１  施術月１ .
042620*     MOVE ALL NC"Ｎ" TO 氏名固定 生年月日固定 住所固定
      * 委任固定.
042630*     MOVE ALL "X"    TO 被保険者カナ.
042640     MOVE ALL "Ｎ" TO 被保険者氏名.
042650*     MOVE ALL NC"Ｎ" TO 被保険者元号 被保険者年固定 被保険者月固定 被保険者日固定.
042660*     MOVE 99         TO 被保険者年 被保険者月 被保険者日.
042670     MOVE ALL "X"    TO 住所１ 住所２.
042680*     MOVE ALL NC"Ｎ" TO 保険種別 割合固定.
042690*     MOVE 9          TO 負担割合.
042700     MOVE ALL "X"    TO 保険者番号.
042710*     MOVE ALL NC"Ｎ" TO 記号.
042720*     MOVE ALL "X"    TO 番号.
042730     MOVE ALL "X"    TO 患者カナ.
042740     MOVE ALL "Ｎ" TO 患者氏名.
042750*     MOVE "(男)"     TO 患者性別.
042760*     MOVE ALL NC"Ｎ" TO 元号 患者年固定 患者月固定 患者日固定.
042770     MOVE 99         TO 患者年 患者月 患者日.
042780     MOVE ALL "M"    TO 負傷原因１ 負傷原因２ 負傷原因３ 負傷原因４
                              負傷原因５ 負傷原因６.
042800     MOVE ALL NC"Ｎ" TO 負傷名１.
042810     MOVE 99 TO 負傷年１ 負傷月１ 負傷日１ 初検年１ 初検月１ 初検日１
042820                開始年１ 開始月１ 開始日１ 終了年１ 終了月１ 終了日１
042830                実日数１.
042840     MOVE NC"○" TO 治癒チェック１ 中止チェック１ 転医チェック１.
042850     MOVE ALL NC"Ｎ" TO 負傷名２.
042860     MOVE 99 TO 負傷年２ 負傷月２ 負傷日２ 初検年２ 初検月２ 初検日２
042870                開始年２ 開始月２ 開始日２ 終了年２ 終了月２ 終了日２
042880                実日数２.
042890     MOVE NC"○" TO 治癒チェック２ 中止チェック２ 転医チェック２.
042900     MOVE ALL NC"Ｎ" TO 負傷名３.
042910     MOVE 99 TO 負傷年３ 負傷月３ 負傷日３ 初検年３ 初検月３ 初検日３
042920                開始年３ 開始月３ 開始日３ 終了年３ 終了月３ 終了日３
042930                実日数３.
042940     MOVE NC"○" TO 治癒チェック３ 中止チェック３ 転医チェック３.
042950     MOVE ALL NC"Ｎ" TO 負傷名４.
042960     MOVE 99 TO 負傷年４ 負傷月４ 負傷日４ 初検年４ 初検月４ 初検日４
042970                開始年４ 開始月４ 開始日４ 終了年４ 終了月４ 終了日４
042980                実日数４.
042990     MOVE NC"○" TO 治癒チェック４ 中止チェック４ 転医チェック４.
043000     MOVE ALL NC"Ｎ" TO 負傷名５.
043010     MOVE 99 TO 負傷年５ 負傷月５ 負傷日５ 初検年５ 初検月５ 初検日５
043020                開始年５ 開始月５ 開始日５ 終了年５ 終了月５ 終了日５
043030                実日数５.
043040     MOVE NC"○" TO 治癒チェック５ 中止チェック５ 転医チェック５.
043050     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
043060             UNTIL ( 部位ＣＮＴ > 5 )
043070         MOVE ALL NC"Ｎ" TO 経過略称(部位ＣＮＴ)
043080     END-PERFORM.
           MOVE NC"○" TO 男チェック 女チェック 
           明治チェック 大正チェック 昭和チェック 平成チェック 
           １０割チェック ９割チェック ８割チェック ７割チェック 
           協会チェック 組合チェック 共済チェック 国保チェック 退職チェック 後期チェック 
           単独チェック ２併チェック 
           本人チェック ６歳チェック 家族チェック 高７チェック 高一チェック
           振込チェック 普通チェック 当座チェック 銀行チェック 金庫チェック
           農協チェック 本店チェック 支店チェック 本支所チェック

           MOVE ALL NC"□" TO 適用１ 適用２
           MOVE ALL "X" TO 公費負担者番号 受給者番号 記号番号 
      *     柔整師番号
      *
043090     MOVE NC"○" TO 新規チェック 継続チェック.
043100     MOVE 99999 TO  初検料.
043110     MOVE 99999 TO  再検料.
043120     MOVE 99.9 TO  往療距離.
043130     MOVE 99 TO  往療回数.
043140     MOVE 99999 TO  往療料.
043150     MOVE NC"○" TO  大チェック 中チェック 小チェック.
043160     MOVE 99999 TO  金属副子加算料.
043170     MOVE 999999 TO  小計.
043180     MOVE NC"○" TO  時間外チェック 休日チェック 深夜チェック.
043190     MOVE 99999 TO  初検加算料.
043200     MOVE NC"○" TO  夜間チェック 難路チェック 暴風雨雪チェック.
043210     MOVE 99999 TO  往療加算料.
043220     MOVE 99999 TO  施術情報提供料.
043230     MOVE NC"○" TO 整復料チェック 固定料チェック 施療料チェック.
043240     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
043250             UNTIL ( 部位ＣＮＴ > 5 )
043260         MOVE 99999 TO 初回処置料(部位ＣＮＴ)
043270     END-PERFORM.
043280     MOVE 999999 TO 初回処置料合計.
043290     MOVE 99    TO 後療回数１ 冷罨法回数１ 温罨法回数１ 電療回数１.
043300     MOVE 9999  TO 後療単価１ 冷罨法料１   温罨法料１   電療料１.
043310     MOVE 99999 TO 後療料１   小計１       長期込小計１.
043320     MOVE 9.9 TO 長期逓減率１.
043330     MOVE 99 TO 後療回数２ 冷罨法回数２ 温罨法回数２ 電療回数２.
043340     MOVE 9999  TO 後療単価２ 冷罨法料２   温罨法料２   電療料２.
043350     MOVE 99999 TO 後療料２   小計２       長期込小計２.
043360     MOVE 9.9 TO 長期逓減率２.
043370     MOVE 99 TO 後療回数３８ 冷罨法回数３８ 温罨法回数３８ 電療回数３８.
043380     MOVE 9999  TO 後療単価３８ 冷罨法料３８   温罨法料３８   電療料３８.
043390     MOVE 99999 TO 後療料３８ 小計３８ 長期込小計３８ 多部位込小計３８.
043400     MOVE 9.9 TO 長期逓減率３８.
043410     MOVE 99 TO 逓減開始月３０ 逓減開始日３０.
043420     MOVE 99 TO 後療回数３０ 冷罨法回数３０ 温罨法回数３０ 電療回数３０.
043430     MOVE 9999  TO 後療単価３０ 冷罨法料３０   温罨法料３０   電療料３０.
043440     MOVE 99999 TO 後療料３０ 小計３０ 長期込小計３０.
043450     MOVE 9.9 TO 長期逓減率３０.
043460*     MOVE 99 TO 後療回数４５ 冷罨法回数４５ 温罨法回数４５ 電療回数４５.
043470*     MOVE 9999  TO 後療単価４５ 冷罨法料４５   温罨法料４５   電療料４５.
043480*     MOVE 99999 TO 後療料４５ 小計４５ 長期込小計４５ 多部位込小計４５.
043490*     MOVE 9.9 TO 長期逓減率４５.
043500     MOVE 99 TO 逓減開始月４８ 逓減開始日４８.
043510     MOVE 99 TO 後療回数４８ 冷罨法回数４８ 温罨法回数４８ 電療回数４８.
043520     MOVE 9999  TO 後療単価４８ 冷罨法料４８   温罨法料４８   電療料４８.
043530     MOVE 99999 TO 後療料４８ 小計４８ 長期込小計４８ 多部位込小計４８.
043540     MOVE 9.9 TO 長期逓減率４８.
043550     MOVE 99 TO 逓減開始月４０ 逓減開始日４０.
043560     MOVE 99 TO 後療回数４０ 冷罨法回数４０ 温罨法回数４０ 電療回数４０.
043570     MOVE 9999  TO 後療単価４０ 冷罨法料４０   温罨法料４０   電療料４０.
043580     MOVE 99999 TO 後療料４０ 小計４０ 長期込小計４０.
043590     MOVE 9.9 TO 長期逓減率４０.
043860     MOVE ALL "X" TO 部位５０ 部位５８.
043600*
043610*     MOVE "5)45 " TO 逓減固定５５.
043620*     MOVE "0.45"  TO 多部位率５５.
043630*     MOVE 11 TO 逓減開始月５５ 逓減開始日５５.
043640*     MOVE 22 TO 後療回数５５ 冷罨法回数５５ 温罨法回数５５ 電療回数５５.
043650*     MOVE 3335  TO 後療単価５５ 冷罨法料５５   温罨法料５５   電療料５５.
043660*     MOVE 99995 TO 後療料５５ 小計５５ 長期込小計５５ 多部位込小計５５.
043670*     MOVE 8.8 TO 長期逓減率５５.
043680*     MOVE "5)80 " TO 逓減固定５８.
043690*     MOVE "0.8 "  TO 多部位率５８.
043700*     MOVE 11 TO 逓減開始月５８ 逓減開始日５８.
043710*     MOVE 22 TO 後療回数５８ 冷罨法回数５８ 温罨法回数５８ 電療回数５８.
043720*     MOVE 3338  TO 後療単価５８ 冷罨法料５８   温罨法料５８   電療料５８.
043730*     MOVE 99998 TO 後療料５８ 小計５８ 長期込小計５８ 多部位込小計５８.
043740*     MOVE 8.8 TO 長期逓減率５８.
043750*     MOVE "5)100" TO 逓減固定５０.
043760*     MOVE 11 TO 逓減開始月５０ 逓減開始日５０.
043770*     MOVE 22 TO 後療回数５０ 冷罨法回数５０ 温罨法回数５０ 電療回数５０.
043780*     MOVE 3330  TO 後療単価５０ 冷罨法料５０   温罨法料５０   電療料５０.
043790*     MOVE 99990 TO 後療料５０ 小計５０ 長期込小計５０.
043800*     MOVE 8.8 TO 長期逓減率５０.
043810     MOVE ALL NC"Ｎ" TO 適用１ 適用２.
043820     MOVE ALL "Ｎ" TO 長期理由文１ 長期理由文２ 長期理由文３
043830                      長期理由文４ 長期理由文５ 長期理由文６.
043840     MOVE 999999 TO 合計 一部負担金 請求金額.
043850*     MOVE ALL "X"    TO 保険者名称１ 保険者名称２.
043860*     MOVE ALL "X" TO 柔整師番号.
043870     MOVE 99 TO 受理年 受理月 受理日.
043880     MOVE 99 TO 委任年 委任月 委任日.
043890     MOVE 999  TO 施術所郵便番号１.
043900     MOVE 9999 TO 施術所郵便番号２.
043910     MOVE ALL "X" TO 施術所住所１ 施術所住所２.
043920     MOVE ALL "Ｎ" TO 接骨院名.
043930     MOVE ALL "X" TO 代表者カナ.
043940     MOVE ALL "Ｎ" TO 代表者名.
043950     MOVE ALL "X" TO 施術所電話番号.
043960*     MOVE ALL "Ｎ" TO 口座名義人.
043970*     MOVE ALL "X" TO 銀行名支店名 口座番号 口座名義人カナ.
043980*     MOVE NC"（ＮＮ）" TO 預金種別.
043990*
044000*================================================================*
042180*================================================================*
042190 助成レセまとめ判定 SECTION.
042200*---------------------------------------------------------------------------*
042210* 本体まとめ区分＝１
042220* の時は、フラグYES (金額を助成込みで印字）
042230*（例：横浜市の障害は、本体保険（国保系）のレセプト１枚で請求、助成レセはなし）
042240*---------------------------------------------------------------------------*
042250*
042260     MOVE SPACE TO 助成レセまとめフラグ.
009201     IF レセ－本体まとめ区分 = 1 
009202        MOVE "YES" TO 助成レセまとめフラグ
009203     END-IF.
042650*
042660*----------------------------------------------------------------------*
042670** / 神奈川県固有：摘要に負担者番号と受給者番号 /
042680     IF ( 助成レセまとめフラグ = "YES" ) AND
042690        ( 受－費用負担者番号助成(3:2) = "14" )
042700        IF 受－費用負担者番号助成(1:2) NOT = "99"
042710*            MOVE ALL NC"￣"    TO 横線１ 横線２ 横線３
042720*            MOVE ALL NC"｜"    TO 縦線１ 縦線２
042730*            MOVE NC"｜"                    TO 縦線３ 縦線４
042740*            MOVE NC"公費負担者番号"        TO 神奈川固定１
042750*            MOVE NC"受給者番号"            TO 神奈川固定２
042760*            MOVE NC"／"                    TO 神奈川固定３
042770*            MOVE 受－費用負担者番号助成    TO 神奈川負担者番号 
042780*            MOVE 受－受益者番号助成        TO 神奈川受給者番号
                  MOVE 市町村番号Ｗ     TO 公費負担者番号
      *            MOVE 受益者番号助成Ｗ TO 受給者番号
      */受給者番号が８文字以上の場合枠を無視して印刷する/110425
                  MOVE 受－受益者番号助成   TO 受給者番号Ｗ
                  IF 印刷受給者番号２Ｗ = SPACE
016830                MOVE 印刷受給者番号Ｗ TO 受給者番号
                  ELSE
                      MOVE 受給者番号Ｗ     TO 受給者番号２
                  END-IF
042790        END-IF
042800     END-IF.
042810**/和歌山県障害乳幼児ひとり親/100518
042820     IF ( 助成レセまとめフラグ = "YES" ) AND
042830        ( 受－費用負担者番号助成(3:2) = "30" )
042840        IF 受－費用負担者番号助成(1:2) NOT = "99"
042850*            MOVE ALL NC"￣"    TO 横線１ 横線２ 横線３
042860*            MOVE ALL NC"｜"    TO 縦線１ 縦線２
042870*            MOVE NC"｜"                    TO 縦線３ 縦線４
042880*            MOVE NC"公費負担者番号"        TO 神奈川固定１
042890*            MOVE NC"受給者番号"            TO 神奈川固定２
042900*            MOVE NC"／"                    TO 神奈川固定３
042910*            MOVE 受－費用負担者番号助成    TO 神奈川負担者番号 
042920*            MOVE 受－受益者番号助成        TO 神奈川受給者番号
                  MOVE 市町村番号Ｗ     TO 公費負担者番号
      *            MOVE 受益者番号助成Ｗ TO 受給者番号
      */受給者番号が８文字以上の場合枠を無視して印刷する/110425
                  MOVE 受－受益者番号助成   TO 受給者番号Ｗ
                  IF 印刷受給者番号２Ｗ = SPACE
016830                MOVE 印刷受給者番号Ｗ TO 受給者番号
                  ELSE
                      MOVE 受給者番号Ｗ     TO 受給者番号２
                  END-IF
042790        END-IF
042930     END-IF.
042940*
043210*================================================================*
       施術日取得 SECTION.
      *
      *     MOVE SPACE TO 施術日Ｗ.
028350     MOVE 患者番号ＷＲ          TO 施記－患者番号
028360     MOVE 枝番ＷＲ              TO 施記－枝番
028370     MOVE 施術和暦ＷＲ          TO 施記－施術和暦
028380     MOVE 施術年ＷＲ            TO 施記－施術年
028390     MOVE 施術月ＷＲ            TO 施記－施術月
028400     MOVE ZERO                  TO 施記－施術日
028420     START 施術記録Ｆ   KEY IS >= 施記－患者コード
028430                                  施記－施術和暦年月日
028440     END-START
028450     IF 状態キー = "00"
030910         MOVE SPACE TO 終了フラグ２
030920         PERFORM 施術記録Ｆ読込
030930         PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
030940                       ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
030950                       ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
030960                       ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
030970                       ( 施記－施術月     NOT = 施術月ＷＲ      )
                   MOVE NC"○" TO 施術日チェック(施記－施術日)
                   PERFORM 施術記録Ｆ読込
               END-PERFORM
           END-IF.
           PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 31
               MOVE カウンタ TO 施術日(カウンタ)
           END-PERFORM.
037520*================================================================*
023610 請求先情報取得後高 SECTION.
023620*
023630****************************************************
023640* 連結データから市町村マスタより請求先を取得する。 *
023650* ● 請求先...... 請求先名称Ｗに格納               *
023660*                                                  *
023670* 2001/10/26 修正：兵庫28のみ支部部署名をつける    *
023680*                                                  *
023690****************************************************
023700     MOVE 公費種別ＷＲ        TO 市－公費種別.
023710     MOVE 費用負担者番号ＷＲ  TO 市－市町村番号.
023720     READ 市町村マスタ
023730     INVALID KEY
023740         MOVE SPACE              TO 請求先名称ＴＢＬ
023750         MOVE SPACE              TO 支部部署名Ｗ
023760     NOT INVALID KEY
023770         IF 市－請求先区分 = 1
023780             MOVE 公費種別ＷＲ       TO 請先－保険種別
023790             MOVE 費用負担者番号ＷＲ TO 請先－保険者番号
023800             READ 請求先マスタ
023810             INVALID KEY
023820                 MOVE SPACE        TO 請求先名称ＴＢＬ
023830                 MOVE SPACE        TO 支部部署名Ｗ
023840             NOT INVALID KEY
023850                 MOVE 請先－保険者名称  TO 請求先名称ＴＢＬ
023860                 MOVE 請先－支部部署名  TO 支部部署名Ｗ
023870*
023880                 IF 費用負担者番号ＷＲ(3:2) NOT = "28"
023890                    MOVE SPACE TO 支部部署名Ｗ
023900                 END-IF
023910*
023920             END-READ
023930         ELSE
023940             MOVE 市－市町村名称  TO 請求先名称ＴＢＬ
023950             MOVE SPACE           TO 支部部署名Ｗ
023960         END-IF
023970     END-READ.
023980*
023990     IF 請求先名称ＴＢＬ NOT = SPACE
024000        PERFORM VARYING カウンタ FROM 1 BY 1
024010                UNTIL ( カウンタ > 20 )  OR
024020                      ( 請求先名称ＷＴ(カウンタ) = SPACE )
024030           MOVE 請求先名称ＷＴ(カウンタ) TO 請求先名称ＷＴ１
024040        END-PERFORM
024050        IF 請求先名称ＷＴ１ = "市" OR "町" OR "村" OR "区"
024060           STRING 請求先名称ＴＢＬ  DELIMITED BY SPACE
024070                  "長"              DELIMITED BY SIZE
024080                  支部部署名Ｗ      DELIMITED BY SPACE
024090                  "殿"              DELIMITED BY SIZE
024100                  INTO 請求先名称Ｗ
024110           END-STRING
024120        ELSE
024130           STRING 請求先名称ＴＢＬ  DELIMITED BY SPACE
024140                  "　"              DELIMITED BY SIZE
024150                  支部部署名Ｗ      DELIMITED BY SPACE
024160                  "殿"              DELIMITED BY SIZE
024170                  INTO 請求先名称Ｗ
024180           END-STRING
024190        END-IF
024200     END-IF.
024210*
024220*================================================================*
       ３部位同時請求判定 SECTION.
      *
      *------------------------------------------------------------------------------------------*
      * ②３部位以上負傷原因all入力済みかチェック　平成22年6月施術より
      *------------------------------------------------------------------------------------------*
           MOVE SPACE  TO 負傷原因対象フラグ.
           MOVE SPACE  TO 負傷原因未入力フラグ.
      *
      *     PERFORM データチェック.
      *     IF 実行キーＷ = "YES"
               MOVE 施術和暦ＷＲ   TO 施術和暦ＳＷ
               MOVE 施術年ＷＲ     TO 施術年ＳＷ
               MOVE 施術月ＷＲ     TO 施術月ＳＷ
               IF ( 負－部位数 >= 3 )
      *
      *---------------------------------------------------------------------------*
      * --- 対象：100分の？？が発生→（同時に３部位以上あるか）のチェック-----------*
      *
      *--------------------------------------------------------* 
      *
      *           / ZEROクリアー /
                  MOVE ZERO TO 同時日ＣＷ
                  MOVE ZERO TO カウンタ最初Ｗ
                  MOVE ZERO TO カウンタ最後Ｗ
      *
                  PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
                           UNTIL  部位ＣＮＴ > 負－部位数 
      *              / -- 当月開始 --- /
                     IF ( 施術和暦ＷＲ  = 負－開始和暦(部位ＣＮＴ) ) AND
                        ( 施術年ＷＲ    = 負－開始年(部位ＣＮＴ) )   AND
                        ( 施術月ＷＲ    = 負－開始月(部位ＣＮＴ) )
      *                 / 継続 /
                        IF 負－転帰区分(部位ＣＮＴ) = 9
                           MOVE 負－開始日(部位ＣＮＴ)  TO カウンタ最初Ｗ
                           MOVE 31                      TO カウンタ最後Ｗ
                        ELSE
      *                 / 終了 /
                           MOVE 負－開始日(部位ＣＮＴ)  TO カウンタ最初Ｗ
                           MOVE 負－終了日(部位ＣＮＴ)  TO カウンタ最後Ｗ
                        END-IF
      *              / -- 当月開始以外 --- /
                     ELSE
      *                  / 継続 /
                        IF 負－転帰区分(部位ＣＮＴ) = 9
                           MOVE 1                       TO カウンタ最初Ｗ
                           MOVE 31                      TO カウンタ最後Ｗ
                        ELSE
      *                  / 終了 /
                           MOVE 1                       TO カウンタ最初Ｗ
                           MOVE 負－終了日(部位ＣＮＴ)  TO カウンタ最後Ｗ
                        END-IF
                     END-IF
      *             / 同時数加算 /
                     PERFORM VARYING カウンタ FROM  カウンタ最初Ｗ BY 1
                                     UNTIL カウンタ  >  カウンタ最後Ｗ 
                          COMPUTE 同時日Ｗ(カウンタ) = 同時日Ｗ(カウンタ) + 1
                     END-PERFORM
      *
                  END-PERFORM
      **
      **          / 同時テーブルチェック /
                  PERFORM VARYING カウンタ FROM 1 BY 1
                                     UNTIL ( カウンタ > 31 )  OR ( 負傷原因対象フラグ = "YES" )
                      IF 同時日Ｗ(カウンタ) >= 3
                          MOVE "YES" TO 負傷原因対象フラグ
                      END-IF
                  END-PERFORM
               END-IF.
      *     END-IF.
      *
      *     IF 負傷原因対象フラグ = "YES"
      *         MOVE "YES" TO 対象フラグ
      *     END-IF.
024220*================================================================*
       計算マスタ取得 SECTION.
      *
      * 制御区分：０１　健康保険
           MOVE 01             TO 計－制御区分.
           MOVE 連レ印－施術和暦 TO 計－開始和暦 施術和暦ＣＷ.
           MOVE 連レ印－施術年   TO 計－開始年   施術年ＣＷ.
           MOVE 連レ印－施術月   TO 計－開始月   施術月ＣＷ.
      *
           START 計算マスタ KEY IS <= 計－制御区分 計－開始和暦年月 REVERSED
           END-START.
      *
           IF 状態キー = "00"
               READ 計算マスタ NEXT
               AT END
                   PERFORM コンソールエラー確認
                   DISPLAY "施術年月に対応した料金がみつかりません"
                   " 受診者№=" 連レ印－患者コード " 施術年月=" 連レ印－施術年 連レ印－施術月 UPON CONS
                   PERFORM 終了処理
                   MOVE ZERO TO PROGRAM-STATUS
                   EXIT PROGRAM
               NOT AT END
                   IF ( 施術和暦年月ＣＷ >= 計Ａ－開始和暦年月 ) AND
                      ( 施術和暦年月ＣＷ <= 計Ａ－終了和暦年月 )
                       MOVE 計Ａ－多部位逓減率(3) TO 多部位逓減率３Ｗ
                   ELSE
                       PERFORM コンソールエラー確認
                       DISPLAY "施術年月に対応した料金がみつかりません"
                       " 受診者№=" 連レ印－患者コード " 施術年月=" 連レ印－施術年 連レ印－施術月 UPON CONS
                       PERFORM 終了処理
                       MOVE ZERO TO PROGRAM-STATUS
                       EXIT PROGRAM
                   END-IF
               END-READ
           END-IF.
040820*================================================================*
040830 経過取得 SECTION.
040840*
036040     MOVE  SPACE TO  連期間－キー.
036050     INITIALIZE      連期間－キー.
036060     MOVE 施術和暦ＷＲ  TO  連期間－施術和暦.
036070     MOVE 施術年ＷＲ    TO  連期間－施術年.
036080     MOVE 施術月ＷＲ    TO  連期間－施術月.
036090     MOVE 患者番号ＷＲ  TO  連期間－患者番号.
036100     MOVE 枝番ＷＲ      TO  連期間－枝番.
036110*
036120     CALL   "CHOUKI".
036130     CANCEL "CHOUKI".
036140*
      */捻挫・打撲・挫傷は転帰にかかわらず３ヶ月以上は「やや良好」、
      */それ以外はすべて「良好」にする　　　　　　　　　　　　　　　　/151217
031620     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
031630             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
               IF  (負－経過コード(部位ＣＮＴ) = ZERO)
028850             EVALUATE 部位ＣＮＴ
028860             WHEN 1
028870                 MOVE NC"①" TO 経過部位Ｗ
028880             WHEN 2
028890                 MOVE NC"②" TO 経過部位Ｗ
028900             WHEN 3
028910                 MOVE NC"③" TO 経過部位Ｗ
028920             WHEN 4
028930                 MOVE NC"④" TO 経過部位Ｗ
028940             WHEN 5
028950                 MOVE NC"⑤" TO 経過部位Ｗ
028960             END-EVALUATE
                   IF 負－負傷種別(部位ＣＮＴ) = 01 OR 02 OR 03
040850*              IF ( 負－転帰区分(部位ＣＮＴ) NOT = 1 AND 2)
040900                 IF ( 連期間－期間Ｗ(部位ＣＮＴ)  >= 3 )
040910                     MOVE NC"やや良好" TO  経過ＣＭ
040920                 ELSE
040930                     MOVE NC"良好"     TO  経過ＣＭ
040940                 END-IF
                   ELSE
040930                 MOVE NC"良好"     TO  経過ＣＭ
040950*              END-IF
                   END-IF
                   MOVE SPACE      TO  印刷経過略称Ｗ(部位ＣＮＴ)
028970             STRING  経過部位Ｗ     DELIMITED BY SPACE
028980                     経過ＣＭ       DELIMITED BY SPACE
028990                INTO 印刷経過略称Ｗ(部位ＣＮＴ)
029000             END-STRING
               END-IF
           END-PERFORM.
040960*
005659*================================================================*
       コンソールエラー確認 SECTION.
      *
           IF エラー表示Ｆ = ZERO
014083         MOVE  "エラーが発生したのでコンソール画面の内容" TO 連メ７－メッセージ１
014084         MOVE  "を確認し、修正してください。"             TO 連メ７－メッセージ２
014085         CALL   "MSG007"
014086         CANCEL "MSG007"
           END-IF.
           MOVE 1 TO エラー表示Ｆ.
      *================================================================*
044010******************************************************************
044020 END PROGRAM YAS6125.
044030******************************************************************
