000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCH6125.
000060 AUTHOR.                 池田 幸子
000070*
000080*----------------------------------------------------------------*
000090*         中央 レセプト印刷（柔+ｳｨﾝﾄﾞｳｽﾞ版）
000100*         MED = YAW610 YCH6125P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2011-09-08
000130 DATE-COMPILED.          2011-09-08
      */実日数はレセ－部位実日数を転記する/160816
      */金属副子・運動後療の変更・追加/1805
      */長野県のこどもの助成で受給者負担金を記入/2106
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
000620     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000630                             ORGANIZATION             IS  INDEXED
000640                             ACCESS MODE              IS  DYNAMIC
000650                             RECORD KEY               IS  施情－施術所番号
000660                             FILE STATUS              IS  状態キー
000670                             LOCK        MODE         IS  AUTOMATIC.
000680     SELECT  請求先マスタ    ASSIGN      TO        SEIKYUSL
000690                             ORGANIZATION             IS  INDEXED
000700                             ACCESS MODE              IS  DYNAMIC
000710                             RECORD KEY               IS  請先－保険種別
000720                                                          請先－保険者番号
000730                             FILE STATUS              IS  状態キー
000740                             LOCK    MODE             IS  AUTOMATIC.
000750     SELECT  経過マスタ      ASSIGN      TO        KEIKAL
000760                             ORGANIZATION             IS  INDEXED
000770                             ACCESS MODE              IS  DYNAMIC
000780                             RECORD KEY               IS  経－区分コード
000790                                                          経－経過コード
000800                             FILE STATUS              IS  状態キー
000810                             LOCK        MODE         IS  AUTOMATIC.
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
001210                             RECORD KEY               IS  負－施術和暦年月
001220                                                          負－患者コード
001230                             ALTERNATE RECORD KEY     IS  負－患者コード
001240                                                          負－施術和暦年月
001250                             FILE STATUS              IS  状態キー
001260                             LOCK        MODE         IS  AUTOMATIC.
001270     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
001280                             ORGANIZATION             IS  INDEXED
001290                             ACCESS MODE              IS  DYNAMIC
001300                             RECORD KEY               IS  負原－区分コード
001310                                                          負原－負傷原因コード
001320                             FILE STATUS              IS  状態キー
001330                             LOCK        MODE         IS  AUTOMATIC.
001340     SELECT  会情報マスタ    ASSIGN      TO        KAIJOHOL
001350                             ORGANIZATION             IS  INDEXED
001360                             ACCESS MODE              IS  DYNAMIC
001370                             RECORD KEY               IS  会情－柔整鍼灸区分
                                                                会情－協会コード
001380                                                          会情－保険種別
001390                                                          会情－変更和暦年月
001400                             ALTERNATE RECORD KEY     IS  会情－柔整鍼灸区分
                                                                会情－接骨師会カナ
001410                                                          会情－協会コード
001420                                                          会情－保険種別
001430                                                          会情－変更和暦年月
001440                             FILE STATUS              IS  状態キー
001450                             LOCK        MODE         IS  AUTOMATIC.
001460     SELECT  ＩＤ管理マスタ    ASSIGN      TO        IDKANRL
001470                             ORGANIZATION             IS  INDEXED
001480                             ACCESS MODE              IS  DYNAMIC
001490                             RECORD KEY               IS  ＩＤ管－ＩＤ区分
001500                                                          ＩＤ管－施術所番号
001510                                                          ＩＤ管－保険種別
001520                                                          ＩＤ管－保険者番号
001530                             ALTERNATE RECORD KEY     IS  ＩＤ管－施術ＩＤ番号
001540                                                          ＩＤ管－ＩＤ区分
001550                                                          ＩＤ管－施術所番号
001560                                                          ＩＤ管－保険種別
001570                                                          ＩＤ管－保険者番号
001580                             FILE STATUS              IS  状態キー
001590                             LOCK        MODE         IS  AUTOMATIC.
001600     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
001610                             ORGANIZATION             IS  INDEXED
001620                             ACCESS MODE              IS  DYNAMIC
001630                             RECORD KEY               IS  市－公費種別
001640                                                          市－市町村番号
001650                             ALTERNATE RECORD KEY     IS  市－公費種別
001660                                                          市－市町村名称
001670                                                          市－市町村番号
001680                             FILE STATUS              IS  状態キー
001690                             LOCK        MODE         IS  AUTOMATIC.
001700     SELECT  メモファイル    ASSIGN      TO        MEMOL
001710                             ORGANIZATION             IS  INDEXED
001720                             ACCESS MODE              IS  DYNAMIC
001730                             RECORD KEY               IS  メモ－制御区分
001740                                                          メモ－患者コード
001750                                                          メモ－施術和暦年月日
001760                             ALTERNATE RECORD KEY     IS  メモ－制御区分
001770                                                          メモ－施術和暦年月日
001780                                                          メモ－患者コード
001790                             ALTERNATE RECORD KEY     IS  メモ－患者コード
001800                                                          メモ－施術和暦年月日
001810                                                          メモ－制御区分
001820                             FILE STATUS              IS  状態キー
001830                             LOCK        MODE         IS  AUTOMATIC.
001840* 並び順印字用
001850     SELECT  作業ファイル４  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001860                             ORGANIZATION             IS  INDEXED
001870                             ACCESS                   IS  DYNAMIC
001880                             RECORD      KEY          IS  作４－施術和暦年月
001890                                                          作４－患者コード
001900                                                          作４－保険種別
001910                             FILE        STATUS       IS  状態キー
001920                             LOCK        MODE         IS  AUTOMATIC.
001930*
001940     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF002
001950                             SYMBOLIC    DESTINATION  IS "PRT"
001960                             FORMAT                   IS  定義体名Ｐ
001970                             GROUP                    IS  項目群名Ｐ
001980                             PROCESSING  MODE         IS  処理種別Ｐ
001990                             UNIT        CONTROL      IS  拡張制御Ｐ
002000                             FILE        STATUS       IS  通知情報Ｐ.
002010******************************************************************
002020*                      DATA DIVISION                             *
002030******************************************************************
002040 DATA                    DIVISION.
002050 FILE                    SECTION.
002060*                           ［ＲＬ＝  ３２０］
002070 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
002080     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
002090*                           ［ＲＬ＝  １２８］
002100 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
002110     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
002120*                           ［ＲＬ＝  １２８］
002130 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
002140     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
002180*                           ［ＲＬ＝  ２５６］
002190 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
002200     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
002210*                           ［ＲＬ＝  １２８］
002220 FD  施術所情報マスタ          BLOCK   CONTAINS   1   RECORDS.
002230     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
002240*                           ［ＲＬ＝  １２８］
002250 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
002260     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
002270*                           ［ＲＬ＝  １２８］
002280 FD  経過マスタ          BLOCK   CONTAINS   1   RECORDS.
002290     COPY KEIKA          OF  XFDLIB  JOINING   経   AS  PREFIX.
002300*                           ［ＲＬ＝  ３２０］
002310 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
002320     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
002330*                           ［ＲＬ＝  ２５６］
002340 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
002350     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
002360*                           ［ＲＬ＝  １２８］
002370 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002380     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
002390*                           ［ＲＬ＝  １２８］
002400 FD  負傷原因Ｆ          BLOCK   CONTAINS   1   RECORDS.
002410     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
002420*                           ［ＲＬ＝  ６４０］
002430 FD  会情報マスタ        BLOCK   CONTAINS   1   RECORDS.
002440     COPY KAIJOHO         OF  XFDLIB  JOINING   会情   AS  PREFIX.
002450*                           ［ＲＬ＝  １２８］
002460 FD  ＩＤ管理マスタ          BLOCK   CONTAINS   1   RECORDS.
002470     COPY IDKANR    OF  XFDLIB  JOINING   ＩＤ管   AS  PREFIX.
002480*                           ［ＲＬ＝  ２５６］
002490 FD  市町村マスタ          BLOCK   CONTAINS   1   RECORDS.
002500     COPY SITYOSN        OF  XFDLIB  JOINING   市   AS  PREFIX.
002510*                           ［ＲＬ＝  ８３２］
002520 FD  メモファイル        BLOCK CONTAINS 1     RECORDS.
002530     COPY MEMO           OF    XFDLIB JOINING メモ AS PREFIX.
002540**
002550**
002560 FD  作業ファイル４ RECORD  CONTAINS 32 CHARACTERS.
002570 01  作４－レコード.
002580     03  作４－レコードキー.
002590         05  作４－施術和暦年月.
002600             07  作４－施術和暦            PIC 9.
002610             07  作４－施術年              PIC 9(2).
002620             07  作４－施術月              PIC 9(2).
002630         05  作４－患者コード.
002640             07 作４－患者番号             PIC 9(6).
002650             07 作４－枝番                 PIC X(1).
002660         05  作４－保険種別                PIC 9(2).
002670     03  作４－レコードデータ.
002680         05  作４－順番                    PIC 9(4).
002690         05  FILLER                        PIC X(14).
002700*
002710*
002720 FD  印刷ファイル.
002730     COPY YCH6125P       OF  XMDLIB.
002740*----------------------------------------------------------------*
002750******************************************************************
002760*                WORKING-STORAGE SECTION                         *
002770******************************************************************
002780 WORKING-STORAGE         SECTION.
002790 01 キー入力                           PIC X     VALUE SPACE.
002800 01 状態キー                           PIC X(2)  VALUE SPACE.
002810 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002820 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002820 01 終了フラグ３                       PIC X(3)  VALUE SPACE.
002830 01 初検フラグ                         PIC X(3)  VALUE SPACE.
002840 01 ファイル名                         PIC N(6)  VALUE SPACE.
002850 01 レセプトＰＧＷ                     PIC X(8)  VALUE SPACE.
002860 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
002870 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
002880 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002890 01 患者番号Ｗ                         PIC 9(6)  VALUE ZERO.
002900 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002910 01 部位名称Ｗ                         PIC N(12) VALUE SPACE.
002920 01 部位長Ｗ                           PIC 9(2) VALUE 1.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002870 01 用紙種別Ｗ                         PIC 9(1)  VALUE ZERO.
002930**
002940 01 遅延フラグ                         PIC X(3) VALUE SPACE.
002950 01 遅延回数Ｗ                         PIC 9(4) VALUE ZERO.
002960 01 遅延ＣＮＴ                         PIC 9(5) VALUE ZERO.
002970 01 最大登録数Ｗ                       PIC 9 VALUE ZERO.
002980 01 負傷連続登録Ｗ                     PIC 9 VALUE ZERO.
002990**
003000** 数字→日本語変換
003010 01 数字Ｗ                             PIC 9(2).
003020 01 数字Ｒ REDEFINES 数字Ｗ.
003030    03 数字Ｗ１                        PIC X(1).
003040    03 数字Ｗ２                        PIC X(1).
003050*
003060 01 負傷番号Ｗ                         PIC 9.
003070 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
003080    03 負傷番号Ｗ１                    PIC X.
003090*
003100 01 全角負傷番号Ｗ                     PIC N.
003110 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
003120    03 全角負傷番号Ｗ１                PIC X(2).
003130*
003140 01 カウンタ                           PIC 9(2)  VALUE ZERO.
003150 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
003160*
003170* 退避用
003180 01 終了年月日ＷＴ.
003190    03 終了年ＷＴ                      PIC 9(2)  VALUE ZERO.
003200    03 終了月ＷＴ                      PIC 9(2)  VALUE ZERO.
003210    03 終了日ＷＴ                      PIC 9(2)  VALUE ZERO.
003220* 初検日退避用
003230 01 初検年月日ＷＴ.
003240    03 初検和暦ＷＴ                    PIC 9     VALUE ZERO.
003250    03 初検年ＷＴ                      PIC 9(2)  VALUE ZERO.
003260    03 初検月ＷＴ                      PIC 9(2)  VALUE ZERO.
003270    03 初検日ＷＴ                      PIC 9(2)  VALUE ZERO.
003280* 初検加算時刻用
003290 01 初検加算ＷＴ.
003300    03 初検加算カウント                PIC 9    VALUE ZERO.
003310    03 番号カウンタ                    PIC 9    VALUE ZERO.
003320    03 初検加算集団ＷＴ  OCCURS 3.
003330       05 初検加算区分ＷＴ             PIC 9    VALUE ZERO.
003340       05 初検加算時ＷＴ               PIC 9(2) VALUE ZERO.
003350       05 初検加算分ＷＴ               PIC 9(2) VALUE ZERO.
003360    03 初検加算集団ＮＷ  OCCURS 3.
003370       05 加算区切Ｗ                   PIC N(1) VALUE SPACE.
003380       05 加算内容Ｗ                   PIC N(3) VALUE SPACE.
003390       05 初検加算時ＮＷ１             PIC N(1) VALUE SPACE.
003400       05 初検加算時ＮＷ２             PIC N(1) VALUE SPACE.
003410       05 時固定Ｗ                     PIC N(1) VALUE SPACE.
003420       05 初検加算分ＮＷ１             PIC N(1) VALUE SPACE.
003430       05 初検加算分ＮＷ２             PIC N(1) VALUE SPACE.
003440       05 分固定Ｗ                     PIC N(1) VALUE SPACE.
003450    03 初検加算時刻１Ｗ                PIC N(10) VALUE SPACE.
003460    03 初検加算時刻２Ｗ                PIC N(10) VALUE SPACE.
003470    03 初検加算時刻３Ｗ                PIC N(10) VALUE SPACE.
003070    03 初検加算区切Ｗ                  PIC X     VALUE SPACE.
003080    03 初検加算時Ｗ                    PIC 9(2)  VALUE ZERO.
003090    03 初検加算分Ｗ                    PIC 9(2)  VALUE ZERO.
003480* 負傷原因用
003490 01 負傷原因ＷＴ.
003500    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
003510    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
003520    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
003530    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
003540    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
003550    03 負傷原因ナンバーＷＴ.
003560       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
003570    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
003580 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
003590 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
003600 01 負傷原因ＴＢＬ.
003610    03 負傷原因コードＴＢＬ            OCCURS 9.
003620       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
003630       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
003640       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
003650 01 負傷原因内容Ｗ.
003660    03 負傷原因内容合成Ｗ              PIC X(318) OCCURS 9 VALUE SPACE.
003670    03 負傷原因内容分解ＸＷ.
003680       05 負傷原因内容１ＸＷ           PIC X(80)  VALUE SPACE.
003690       05 負傷原因内容２ＸＷ           PIC X(80)  VALUE SPACE.
003690       05 負傷原因内容３ＸＷ           PIC X(80)  VALUE SPACE.
003700       05 負傷原因内容４ＸＷ           PIC X(78)  VALUE SPACE.
003710*
003720* 保険者番号
003730 01 保険者番号比較Ｗ                   PIC X(6)   VALUE SPACE.
003740* 東京振込先 会長名用(５文字）
003750 01 会長名Ｗ                           PIC N(5)   VALUE SPACE.
003760*
003770** 前月初検のみ用
003780 01 初日再検フラグ                     PIC X(3)  VALUE SPACE.
003790 01 前月フラグ                         PIC X(3)  VALUE SPACE.
003800*
003810 01 計算年月日Ｗ.
003820    03 計算和暦Ｗ                      PIC 9(1)  VALUE ZERO.
003830    03 計算年Ｗ                        PIC S9(2)  VALUE ZERO.
003840    03 計算月Ｗ                        PIC S9(2)  VALUE ZERO.
003850    03 計算日Ｗ                        PIC S9(2)  VALUE ZERO.
003860 01 開始年月日２Ｗ.
003870    03 開始和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003880    03 開始年２Ｗ                      PIC 9(2)  VALUE ZERO.
003890    03 開始月２Ｗ                      PIC 9(2)  VALUE ZERO.
003900    03 開始日２Ｗ                      PIC 9(2)  VALUE ZERO.
003910    03 開始西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003920 01 終了年月日２Ｗ.
003930    03 終了和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003940    03 終了年２Ｗ                      PIC 9(2)  VALUE ZERO.
003950    03 終了月２Ｗ                      PIC 9(2)  VALUE ZERO.
003960    03 終了日２Ｗ                      PIC 9(2)  VALUE ZERO.
003970    03 終了西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003980***
003990** 負傷原因・長期理由印刷区分用
004000 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
004010 01 長期理由印刷区分Ｗ                 PIC 9 VALUE ZERO.
004020*
004030** レセ下段の日付区分用 (0:最終通院日、1:月末日、9:印字なし)
004040 01 レセプト日付区分Ｗ                 PIC 9 VALUE ZERO.
004050 01 レセプト患者日付区分Ｗ             PIC 9 VALUE ZERO.
004060*
004070** 月末日用
004080 01 施術西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
004090 01 商Ｗ                               PIC 9(3)  VALUE ZERO.
004100 01 余Ｗ                               PIC 9(3)  VALUE ZERO.
004110*
004120** 枝番判定用
004130 01 開始診療日手動区分Ｗ               PIC 9    VALUE ZERO.
004140*
004150*
004160** 助成レセまとめ用
004170 01 助成レセまとめフラグ               PIC X(3)  VALUE SPACE.
004180 01 助成種別略称Ｗ                     PIC N(4)  VALUE SPACE.
004190 01 助成種別略称Ｗ２                   PIC N(4)  VALUE SPACE.
004200*
004210** レセ摘要用( N(38)固定） /
004220 01 負傷の経過Ｗ.
004230    03 負傷の経過行Ｗ                  PIC X(76) OCCURS 2 VALUE SPACE.
004240 01 負傷の経過ＮＷ REDEFINES 負傷の経過Ｗ.
004250    03 負傷の経過行ＮＷ                PIC N(38) OCCURS 2.
004260*
004270* 負傷原因印刷区分
004280 01 レセ負傷原因印刷区分Ｗ             PIC 9    VALUE ZERO.
004290*
003460 01 長期理由印刷区分Ｆ                 PIC 9 VALUE ZERO.
004291*
004292* 福岡の経過固定印字用に使用
004293 01 全柔ＦＰＤ区分Ｗ                   PIC 9     VALUE ZERO.
004294 01 経過部位数字Ｗ                     PIC N(1)  VALUE SPACE.
      *
004490* 長野県用
004500 01 受給者番号編集Ｗ.
004510    03 受給者番号編集Ｗ１              PIC X(3)  VALUE SPACE.
004520    03 受給者区切１                    PIC X     VALUE SPACE.
004530    03 受給者番号編集Ｗ２              PIC X(2)  VALUE SPACE.
004540    03 受給者区切２                    PIC X     VALUE SPACE.
004550    03 受給者番号編集Ｗ３              PIC X(10) VALUE SPACE.
004560*
      */金属副子・運動後療の変更・追加/1805
       01 金属副子ＣＭ１                     PIC X(130) VALUE SPACE.
       01 金属副子ＣＭ２                     PIC X(170) VALUE SPACE.
       01 運動後療ＣＭ                       PIC X(68)  VALUE SPACE.
004295*
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
004296*
004300****************
004310* 連結項目待避 *
004320****************
004330*    ************
004340*    * 印刷キー *
004350*    ************
004360 01 対象データＷＲ.
004370    03 施術和暦年月ＷＲ.
004380       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
004390       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
004400       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
004410    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004420    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
004430    03 公費種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004440    03 費用負担者番号ＷＲ               PIC X(10) VALUE SPACE.
004450    03 助成種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004460    03 費用負担者番号助成ＷＲ           PIC X(10) VALUE SPACE.
004470    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
004480    03 患者カナＷＲ                     PIC X(50) VALUE SPACE.
004490    03 患者コードＷＲ.
004500       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
004510       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
004520*    ************
004530*    * 料金情報 *
004540*    ************
004550*    月毎の料金
004560***********************
004570 01 料金１ＷＲ.
004580   03 初検ＷＲ.
004590      05 負担割合ＷＲ               PIC 9(3)    VALUE ZERO.
004600      05 初検料ＷＲ                 PIC 9(5)    VALUE ZERO.
004610      05 初検加算料ＷＲ             PIC 9(5)    VALUE ZERO.
         03 初検時相談料ＷＲ              PIC 9(4)    VALUE ZERO.
004620   03 再検料ＷＲ                    PIC 9(5)    VALUE ZERO.
004630   03 往療ＷＲ.
004640      05 往療距離ＷＲ               PIC 9(2)V9  VALUE ZERO.
004650      05 往療回数ＷＲ               PIC 9(2)    VALUE ZERO.
004660      05 往療料ＷＲ                 PIC 9(6)    VALUE ZERO.
004670      05 往療加算料ＷＲ             PIC 9(5)    VALUE ZERO.
004680   03 金属副子加算料ＷＲ            PIC 9(5)    VALUE ZERO.
004690   03 施術情報提供料ＷＲ            PIC 9(5)    VALUE ZERO.
004700   03 合計ＷＲ                      PIC 9(6)    VALUE ZERO.
004710   03 一部負担金ＷＲ                PIC 9(6)    VALUE ZERO.
004720   03 請求金額ＷＲ                  PIC 9(6)    VALUE ZERO.
004730   03 給付割合ＷＲ                  PIC 9(1)    VALUE ZERO.
004740   03 受給者負担額ＷＲ              PIC 9(6)    VALUE ZERO.
004750   03 助成請求金額ＷＲ              PIC 9(6)    VALUE ZERO.
004760*
004770* 負傷部位毎の料金
004780***********************
004790 01 料金２ＷＲ.
004800   03 初回処置ＷＲ    OCCURS   9.
004810      05 初回処置料ＷＲ             PIC 9(5)    VALUE ZERO.
004820*
004830* 逓減毎の料金
004840***********************
004850 01 料金３ＷＲ.
004860**********
004870* １部位 *
004880**********
004890   03 部位１ＷＲ.
004900      05 後療１ＷＲ.
004910         07 後療単価１ＷＲ              PIC 9(4)    VALUE ZERO.
004920         07 後療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
004930         07 後療料１ＷＲ                PIC 9(5)    VALUE ZERO.
004940      05 冷罨法１ＷＲ.
004950         07 冷罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
004960         07 冷罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
004970      05 温罨法１ＷＲ.
004980         07 温罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
004990         07 温罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
005000      05 電療１ＷＲ.
005010         07 電療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
005020         07 電療料１ＷＲ                PIC 9(4)    VALUE ZERO.
005030      05 小計１ＷＲ                     PIC 9(6)    VALUE ZERO.
005040      05 長期逓減率１ＷＲ               PIC 9(3)    VALUE ZERO.
005050      05 長期込小計１ＷＲ               PIC 9(6)    VALUE ZERO.
005060**********
005070* ２部位 *
005080**********
005090   03 部位２ＷＲ.
005100      05 後療２ＷＲ.
005110         07 後療単価２ＷＲ              PIC 9(4)    VALUE ZERO.
005120         07 後療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
005130         07 後療料２ＷＲ                PIC 9(5)    VALUE ZERO.
005140      05 冷罨法２ＷＲ.
005150         07 冷罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
005160         07 冷罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
005170      05 温罨法２ＷＲ.
005180         07 温罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
005190         07 温罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
005200      05 電療２ＷＲ.
005210         07 電療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
005220         07 電療料２ＷＲ                PIC 9(4)    VALUE ZERO.
005230      05 小計２ＷＲ                     PIC 9(6)    VALUE ZERO.
005240      05 長期逓減率２ＷＲ               PIC 9(3)    VALUE ZERO.
005250      05 長期込小計２ＷＲ               PIC 9(6)    VALUE ZERO.
005260******************
005270* ３部位／８割 *
005280******************
005290   03 部位３８ＷＲ.
005300      05 後療３８ＷＲ.
005310         07 後療単価３８ＷＲ              PIC 9(4)  VALUE ZERO.
005320         07 後療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
005330         07 後療料３８ＷＲ                PIC 9(5)  VALUE ZERO.
005340      05 冷罨法３８ＷＲ.
005350         07 冷罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
005360         07 冷罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
005370      05 温罨法３８ＷＲ.
005380         07 温罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
005390         07 温罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
005400      05 電療３８ＷＲ.
005410         07 電療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
005420         07 電療料３８ＷＲ                PIC 9(4)  VALUE ZERO.
005430      05 小計３８ＷＲ                     PIC 9(6)  VALUE ZERO.
005440      05 多部位込小計３８ＷＲ             PIC 9(6)  VALUE ZERO.
005450      05 長期逓減率３８ＷＲ               PIC 9(3)  VALUE ZERO.
005460      05 長期込小計３８ＷＲ               PIC 9(6)  VALUE ZERO.
005470******************
005480* ３部位／１０割 *
005490******************
005500   03 部位３０ＷＲ.
005510      05 逓減開始月日３０ＷＲ.
005520         07 逓減開始月３０ＷＲ            PIC 9(2)  VALUE ZERO.
005530         07 逓減開始日３０ＷＲ            PIC 9(2)  VALUE ZERO.
005540      05 後療３０ＷＲ.
005550         07 後療単価３０ＷＲ              PIC 9(4)  VALUE ZERO.
005560         07 後療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
005570         07 後療料３０ＷＲ                PIC 9(5)  VALUE ZERO.
005580      05 冷罨法３０ＷＲ.
005590         07 冷罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
005600         07 冷罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
005610      05 温罨法３０ＷＲ.
005620         07 温罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
005630         07 温罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
005640      05 電療３０ＷＲ.
005650         07 電療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
005660         07 電療料３０ＷＲ                PIC 9(4)  VALUE ZERO.
005670      05 小計３０ＷＲ                     PIC 9(6)  VALUE ZERO.
005680      05 長期逓減率３０ＷＲ               PIC 9(3)  VALUE ZERO.
005690      05 長期込小計３０ＷＲ               PIC 9(6)  VALUE ZERO.
005700****************
005710* ４部位／５割 *
005720****************
005730   03 部位４５ＷＲ.
005740      05 後療４５ＷＲ.
005750         07 後療単価４５ＷＲ              PIC 9(4)  VALUE ZERO.
005760         07 後療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005770         07 後療料４５ＷＲ                PIC 9(5)  VALUE ZERO.
005780      05 冷罨法４５ＷＲ.
005790         07 冷罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005800         07 冷罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
005810      05 温罨法４５ＷＲ.
005820         07 温罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005830         07 温罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
005840      05 電療４５ＷＲ.
005850         07 電療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005860         07 電療料４５ＷＲ                PIC 9(4)  VALUE ZERO.
005870      05 小計４５ＷＲ                     PIC 9(6)  VALUE ZERO.
005880      05 多部位込小計４５ＷＲ             PIC 9(6)  VALUE ZERO.
005890      05 長期逓減率４５ＷＲ               PIC 9(3)  VALUE ZERO.
005900      05 長期込小計４５ＷＲ               PIC 9(6)  VALUE ZERO.
005910****************
005920* ４部位／８割 *
005930****************
005940   03 部位４８ＷＲ.
005950      05 逓減開始月日４８ＷＲ.
005960         07 逓減開始月４８ＷＲ            PIC 9(2)  VALUE ZERO.
005970         07 逓減開始日４８ＷＲ            PIC 9(2)  VALUE ZERO.
005980      05 後療４８ＷＲ.
005990         07 後療単価４８ＷＲ              PIC 9(4)  VALUE ZERO.
006000         07 後療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
006010         07 後療料４８ＷＲ                PIC 9(5)  VALUE ZERO.
006020      05 冷罨法４８ＷＲ.
006030         07 冷罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
006040         07 冷罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
006050      05 温罨法４８ＷＲ.
006060         07 温罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
006070         07 温罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
006080      05 電療４８ＷＲ.
006090         07 電療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
006100         07 電療料４８ＷＲ                PIC 9(4)  VALUE ZERO.
006110      05 小計４８ＷＲ                     PIC 9(6)  VALUE ZERO.
006120      05 多部位込小計４８ＷＲ             PIC 9(6)  VALUE ZERO.
006130      05 長期逓減率４８ＷＲ               PIC 9(3)  VALUE ZERO.
006140      05 長期込小計４８ＷＲ               PIC 9(6)  VALUE ZERO.
006150******************
006160* ４部位／１０割 *
006170******************
006180   03 部位４０ＷＲ.
006190      05 逓減開始月日４０ＷＲ.
006200         07 逓減開始月４０ＷＲ            PIC 9(2)  VALUE ZERO.
006210         07 逓減開始日４０ＷＲ            PIC 9(2)  VALUE ZERO.
006220      05 後療４０ＷＲ.
006230         07 後療単価４０ＷＲ              PIC 9(4)  VALUE ZERO.
006240         07 後療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
006250         07 後療料４０ＷＲ                PIC 9(5)  VALUE ZERO.
006260      05 冷罨法４０ＷＲ.
006270         07 冷罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
006280         07 冷罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
006290      05 温罨法４０ＷＲ.
006300         07 温罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
006310         07 温罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
006320      05 電療４０ＷＲ.
006330         07 電療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
006340         07 電療料４０ＷＲ                PIC 9(4)  VALUE ZERO.
006350      05 小計４０ＷＲ                     PIC 9(6)  VALUE ZERO.
006360      05 長期逓減率４０ＷＲ               PIC 9(3)  VALUE ZERO.
006370      05 長期込小計４０ＷＲ               PIC 9(6)  VALUE ZERO.
006380********************
006390* ５部位／２．５割 *
006400********************
006410   03 部位５２ＷＲ.
006420      05 後療５２ＷＲ.
006430         07 後療単価５２ＷＲ              PIC 9(4)  VALUE ZERO.
006440         07 後療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
006450         07 後療料５２ＷＲ                PIC 9(5)  VALUE ZERO.
006460      05 冷罨法５２ＷＲ.
006470         07 冷罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
006480         07 冷罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
006490      05 温罨法５２ＷＲ.
006500         07 温罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
006510         07 温罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
006520      05 電療５２ＷＲ.
006530         07 電療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
006540         07 電療料５２ＷＲ                PIC 9(4)  VALUE ZERO.
006550      05 小計５２ＷＲ                     PIC 9(6)  VALUE ZERO.
006560      05 多部位込小計５２ＷＲ             PIC 9(6)  VALUE ZERO.
006570      05 長期逓減率５２ＷＲ               PIC 9(3)  VALUE ZERO.
006580      05 長期込小計５２ＷＲ               PIC 9(6)  VALUE ZERO.
006590****************
006600* ５部位／５割 *
006610****************
006620   03 部位５５ＷＲ.
006630      05 逓減開始月日５５ＷＲ.
006640         07 逓減開始月５５ＷＲ            PIC 9(2)  VALUE ZERO.
006650         07 逓減開始日５５ＷＲ            PIC 9(2)  VALUE ZERO.
006660      05 後療５５ＷＲ.
006670         07 後療単価５５ＷＲ              PIC 9(4)  VALUE ZERO.
006680         07 後療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
006690         07 後療料５５ＷＲ                PIC 9(5)  VALUE ZERO.
006700      05 冷罨法５５ＷＲ.
006710         07 冷罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
006720         07 冷罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
006730      05 温罨法５５ＷＲ.
006740         07 温罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
006750         07 温罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
006760      05 電療５５ＷＲ.
006770         07 電療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
006780         07 電療料５５ＷＲ                PIC 9(4)  VALUE ZERO.
006790      05 小計５５ＷＲ                     PIC 9(6)  VALUE ZERO.
006800      05 多部位込小計５５ＷＲ             PIC 9(6)  VALUE ZERO.
006810      05 長期逓減率５５ＷＲ               PIC 9(3)  VALUE ZERO.
006820      05 長期込小計５５ＷＲ               PIC 9(6)  VALUE ZERO.
006830****************
006840* ５部位／８割 *
006850****************
006860   03 部位５８ＷＲ.
006870      05 逓減開始月日５８ＷＲ.
006880         07 逓減開始月５８ＷＲ            PIC 9(2)  VALUE ZERO.
006890         07 逓減開始日５８ＷＲ            PIC 9(2)  VALUE ZERO.
006900      05 後療５８ＷＲ.
006910         07 後療単価５８ＷＲ              PIC 9(4)  VALUE ZERO.
006920         07 後療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
006930         07 後療料５８ＷＲ                PIC 9(5)  VALUE ZERO.
006940      05 冷罨法５８ＷＲ.
006950         07 冷罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
006960         07 冷罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
006970      05 温罨法５８ＷＲ.
006980         07 温罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
006990         07 温罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
007000      05 電療５８ＷＲ.
007010         07 電療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
007020         07 電療料５８ＷＲ                PIC 9(4)  VALUE ZERO.
007030      05 小計５８ＷＲ                     PIC 9(6)  VALUE ZERO.
007040      05 多部位込小計５８ＷＲ             PIC 9(6)  VALUE ZERO.
007050      05 長期逓減率５８ＷＲ               PIC 9(3)  VALUE ZERO.
007060      05 長期込小計５８ＷＲ               PIC 9(6)  VALUE ZERO.
007070******************
007080* ５部位／１０割 *
007090******************
007100   03 部位５０ＷＲ.
007110      05 逓減開始月日５０ＷＲ.
007120         07 逓減開始月５０ＷＲ            PIC 9(2)  VALUE ZERO.
007130         07 逓減開始日５０ＷＲ            PIC 9(2)  VALUE ZERO.
007140      05 後療５０ＷＲ.
007150         07 後療単価５０ＷＲ              PIC 9(4)  VALUE ZERO.
007160         07 後療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
007170         07 後療料５０ＷＲ                PIC 9(5)  VALUE ZERO.
007180      05 冷罨法５０ＷＲ.
007190         07 冷罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
007200         07 冷罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
007210      05 温罨法５０ＷＲ.
007220         07 温罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
007230         07 温罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
007240      05 電療５０ＷＲ.
007250         07 電療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
007260         07 電療料５０ＷＲ                PIC 9(4)  VALUE ZERO.
007270      05 小計５０ＷＲ                     PIC 9(6)  VALUE ZERO.
007280      05 長期逓減率５０ＷＲ               PIC 9(3)  VALUE ZERO.
007290      05 長期込小計５０ＷＲ               PIC 9(6)  VALUE ZERO.
007300*
007310**************
007320* 施術所情報 *
007330**************
007340 01 施術所情報Ｗ.
007350    03 柔整師番号Ｗ                    PIC X(16)  VALUE SPACE.
007360    03 接骨師会会員番号Ｗ              PIC X(16)  VALUE SPACE.
007370    03 代表者カナＷ                    PIC X(50)  VALUE SPACE.
007380    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
007390    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
          03 都道府県ＪＩＳＷ                PIC X(2)   VALUE SPACE.
007400    03 施術所住所Ｗ.
007410       05 施術所住所１Ｗ               PIC X(50)  VALUE SPACE.
007420       05 施術所住所２Ｗ               PIC X(50)  VALUE SPACE.
007430    03 施術所郵便番号Ｗ.
007440       05 施術所郵便番号１Ｗ           PIC X(3)   VALUE SPACE.
007450       05 施術所郵便番号２Ｗ           PIC X(4)   VALUE SPACE.
007460    03 施術所電話番号Ｗ                PIC X(20)  VALUE SPACE.
007470    03 定額制受理番号Ｗ                PIC X(15)  VALUE SPACE.
007480    03 受理年月日Ｗ.
007490       05 受理年Ｗ                     PIC 9(2)   VALUE ZERO.
007500       05 受理月Ｗ                     PIC 9(2)   VALUE ZERO.
007510       05 受理日Ｗ                     PIC 9(2)   VALUE ZERO.
007520    03 最終通院年月日Ｗ.
007530       05 最終通院年Ｗ                 PIC 9(2)   VALUE ZERO.
007540       05 最終通院月Ｗ                 PIC 9(2)   VALUE ZERO.
007550       05 最終通院日Ｗ                 PIC 9(2)   VALUE ZERO.
007560    03 柔整師年月日Ｗ.
007570       05 柔整師年Ｗ                   PIC 9(2)   VALUE ZERO.
007580       05 柔整師月Ｗ                   PIC 9(2)   VALUE ZERO.
007590       05 柔整師日Ｗ                   PIC 9(2)   VALUE ZERO.
007600    03 患者委任年月日Ｗ.
007610       05 患者委任年Ｗ                 PIC 9(2)   VALUE ZERO.
007620       05 患者委任月Ｗ                 PIC 9(2)   VALUE ZERO.
007630       05 患者委任日Ｗ                 PIC 9(2)   VALUE ZERO.
007640    03 取引先情報Ｗ.
007650        05 取引先銀行名Ｗ              PIC X(40)  VALUE SPACE.
007660        05 取引先銀行支店名Ｗ          PIC X(40)  VALUE SPACE.
007670        05 預金種別Ｗ                  PIC 9(1)   VALUE ZERO.
007680        05 口座番号Ｗ                  PIC X(10)  VALUE SPACE.
007690        05 口座名義人Ｗ                PIC X(40)  VALUE SPACE.
007700        05 口座名義人カナＷ            PIC X(40)  VALUE SPACE.
007710        05 口座名義人とカナＷ          PIC X(60)  VALUE SPACE.
007720        05 銀行名支店名Ｗ              PIC X(60)  VALUE SPACE.
007730        05 預金種別コメントＷ          PIC N(3)   VALUE SPACE.
007740        05 預金種別コメントＸＷ        PIC X(4)   VALUE SPACE.
          03 支払機関.
             05 金融機関名Ｗ.
                07 金融機関名１Ｗ            PIC X(12)  VALUE SPACE.
                07 金融機関名２Ｗ            PIC X(12)  VALUE SPACE.
      *          07 金融機関名３Ｗ            PIC X(8)  VALUE SPACE.
      *          07 金融機関名４Ｗ            PIC X(8)  VALUE SPACE.
      *          07 金融機関名５Ｗ            PIC X(8)  VALUE SPACE.
             05 支店名Ｗ.
                07 支店名１Ｗ                PIC X(12) VALUE SPACE.
                07 支店名２Ｗ                PIC X(12) VALUE SPACE.
      *          07 支店名３Ｗ                PIC X(12) VALUE SPACE.
      *          07 支店名４Ｗ                PIC X(12) VALUE SPACE.
             05 振込チェックＷ               PIC N(1)  VALUE SPACE.
             05 普通チェックＷ               PIC N(1)  VALUE SPACE.
             05 当座チェックＷ               PIC N(1)  VALUE SPACE.
             05 銀行チェックＷ               PIC N(1)  VALUE SPACE.
             05 金庫チェックＷ               PIC N(1)  VALUE SPACE.
             05 農協チェックＷ               PIC N(1)  VALUE SPACE.
             05 本店チェックＷ               PIC N(1)  VALUE SPACE.
             05 支店チェックＷ               PIC N(1)  VALUE SPACE.
             05 本支所チェックＷ             PIC N(1)  VALUE SPACE.
007750*    03 コメントＷ.
007760*        05 コメント１Ｗ                PIC X(36)  VALUE SPACE.
007770*        05 コメント２Ｗ                PIC X(36)  VALUE SPACE.
007780*        05 コメント３Ｗ                PIC X(36)  VALUE SPACE.
007790*        05 コメント４Ｗ                PIC N(18)  VALUE SPACE.
007750    03 コメントＷ.
007760        05 コメント１Ｗ                PIC X(36)  VALUE SPACE.
007770        05 コメント２Ｗ                PIC X(36)  VALUE SPACE.
007780        05 コメント３Ｗ                PIC X(36)  VALUE SPACE.
007790        05 コメント４Ｗ                PIC X(36)  VALUE SPACE.
007800    03 県施術ＩＤＷ                    PIC X(15)  VALUE SPACE.
007810    03 市町村施術ＩＤＷ                PIC X(15)  VALUE SPACE.
007820**************
007830* 受診者情報 *
007840**************
007850 01 受診者情報Ｗ.
      */元号修正/20190408
          03 施術和暦Ｗ                      PIC 9(1)   VALUE ZERO.
007860    03 施術年月Ｗ.
007870       05 施術年Ｗ                     PIC 9(2)   VALUE ZERO.
007880       05 施術月Ｗ                     PIC 9(2)   VALUE ZERO.
007890*    03 記号Ｗ                          PIC N(12)  VALUE SPACE.
007570    03 記号Ｗ.
007580       05 印刷記号Ｗ                   PIC N(12)  VALUE SPACE.
          03 記号番号Ｗ.
             05 記号番号ＸＷ                 PIC X(40) VALUE SPACE.
007900    03 番号Ｗ.
007910       05 印刷番号Ｗ                   PIC X(15)  VALUE SPACE.
007920       05 FILLER                       PIC X(15)  VALUE SPACE.
007930    03 保険者番号Ｗ.
007940       05 印刷保険者番号Ｗ             PIC X(8)   VALUE SPACE.
007950       05 FILLER                       PIC X(2)   VALUE SPACE.
007960    03 市町村番号Ｗ.
007970       05 印刷市町村番号Ｗ             PIC X(8)   VALUE SPACE.
007980       05 FILLER                       PIC X(2)   VALUE SPACE.
          03 受給者番号Ｗ                    PIC X(15) VALUE SPACE.
007990    03 請求先名称Ｗ.
008000       05 印刷請求先名称Ｗ             PIC X(38)  VALUE SPACE.
008010       05 FILLER                       PIC X(38)  VALUE SPACE.
008020    03 保険種別Ｗ                      PIC 9(2)   VALUE ZERO.
008030    03 被保険者情報Ｗ.
008040       05 被保険者カナＷ               PIC X(50)  VALUE SPACE.
008050       05 被保険者氏名Ｗ               PIC X(50)  VALUE SPACE.
008060       05 郵便番号Ｗ.
008070          07 郵便番号１Ｗ              PIC X(3)   VALUE SPACE.
008080          07 郵便番号２Ｗ              PIC X(4)   VALUE SPACE.
008090       05 被保険者住所１Ｗ             PIC X(50)  VALUE SPACE.
008100       05 被保険者住所２Ｗ             PIC X(50)  VALUE SPACE.
008990       05 電話番号Ｗ                   PIC X(35)  VALUE SPACE.
008110    03 患者情報Ｗ.
008120       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
008130       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
008140       05 性別チェックＷ.
008150          07 男チェックＷ              PIC N(1)  VALUE SPACE.
008160          07 女チェックＷ              PIC N(1)  VALUE SPACE.
008170       05 患者性別Ｗ.
008180          07 性別Ｗ                    PIC N(1)  VALUE SPACE.
008190       05 和暦チェックＷ.
008200          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
008210          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
008220          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
008230          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
008240          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
      */元号修正/↓↓↓20190408
008210          07 令和チェックＷ            PIC N(1)  VALUE SPACE.
                07 令和ＣＭＷ                PIC X(4)  VALUE SPACE.
009110*          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
      */元号修正/↑↑↑20190408
008250       05 患者年Ｗ                     PIC 9(2)  VALUE ZERO.
008260       05 患者月Ｗ                     PIC 9(2)  VALUE ZERO.
008270       05 患者日Ｗ                     PIC 9(2)  VALUE ZERO.
008280       05 続柄Ｗ.
008290          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
008300          07 FILLER                    PIC X(4)  VALUE SPACE.
008310       05 負傷原因１Ｗ                 PIC N(37) VALUE SPACE.
008320       05 負傷原因２Ｗ                 PIC N(37) VALUE SPACE.
008330       05 負傷原因３Ｗ                 PIC N(37) VALUE SPACE.
008340       05 負傷原因４Ｗ                 PIC N(37) VALUE SPACE.
008350       05 負傷原因５Ｗ                 PIC N(37) VALUE SPACE.
008360*
008370*       05 負傷原因Ｗ                   PIC N(40) OCCURS 29 VALUE SPACE.
      */半角対応/110421
             05 負傷原因Ｗ OCCURS 29.
                07 負傷原因ＸＷ              PIC X(80)  VALUE SPACE.
008380*
008390*       05 国保区分チェックＷ.
008400*          07 国保チェックＷ            PIC N(1)  VALUE SPACE.
008410*          07 国保組合チェックＷ        PIC N(1)  VALUE SPACE.
008420    03 助成印Ｗ                        PIC N(1)  VALUE SPACE.
008430    03 特別コメントＷ                  PIC X(16) VALUE SPACE.
007390    03 保険種別チェックＷ.
007400       05 社保チェックＷ               PIC N(1)  VALUE SPACE.
007410       05 船員チェックＷ               PIC N(1)  VALUE SPACE.
007420       05 組合チェックＷ               PIC N(1)  VALUE SPACE.
007430       05 国保チェックＷ               PIC N(1)  VALUE SPACE.
             05 共済チェックＷ               PIC N(1)  VALUE SPACE.
             05 自チェックＷ                 PIC N(1)  VALUE SPACE.
             05 退職チェックＷ               PIC N(1)  VALUE SPACE.
             05 後期チェックＷ               PIC N(1)  VALUE SPACE.
          03 本人チェックＷ                  PIC N(1)  VALUE SPACE.
          03 家族チェックＷ                  PIC N(1)  VALUE SPACE.
          03 単独チェックＷ                  PIC N(1)  VALUE SPACE.
          03 ２併チェックＷ                  PIC N(1)  VALUE SPACE.
          03 高一チェックＷ                  PIC N(1)  VALUE SPACE.
          03 高７チェックＷ                  PIC N(1)  VALUE SPACE.
          03 ６歳チェックＷ                  PIC N(1)  VALUE SPACE.
          03 給付割合チェックＷ.
             05 ７割チェックＷ               PIC N(1)  VALUE SPACE.
             05 ８割チェックＷ               PIC N(1)  VALUE SPACE.
             05 ９割チェックＷ               PIC N(1)  VALUE SPACE.
             05 １０割チェックＷ             PIC N(1)  VALUE SPACE.
008440*
008450****************
008460* 負傷データＦ *
008470****************
008480 01 負傷情報Ｗ.
008490    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
008500    03 部位情報Ｗ  OCCURS   9.
008510       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
008520       05 部位コードＷ.
008530          07 負傷種別Ｗ                PIC 9(2)  VALUE ZERO.
008540          07 部位Ｗ                    PIC 9(2)  VALUE ZERO.
008550          07 左右区分Ｗ                PIC 9(1)  VALUE ZERO.
008560          07 負傷位置番号Ｗ            PIC 9(2)  VALUE ZERO.
008570       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
008580       05 負傷年月日Ｗ.
008590          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
008600          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
008610          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
008620       05 初検年月日Ｗ.
008630          07 初検年Ｗ                  PIC 9(2)  VALUE ZERO.
008640          07 初検月Ｗ                  PIC 9(2)  VALUE ZERO.
008650          07 初検日Ｗ                  PIC 9(2)  VALUE ZERO.
008660       05 開始年月日Ｗ.
008670          07 開始年Ｗ                  PIC 9(2)  VALUE ZERO.
008680          07 開始月Ｗ                  PIC 9(2)  VALUE ZERO.
008690          07 開始日Ｗ                  PIC 9(2)  VALUE ZERO.
008700       05 終了年月日Ｗ.
008710          07 終了年Ｗ                  PIC 9(2)  VALUE ZERO.
008720          07 終了月Ｗ                  PIC 9(2)  VALUE ZERO.
008730          07 終了日Ｗ                  PIC 9(2)  VALUE ZERO.
008740       05 実日数Ｗ                     PIC 9(2)  VALUE ZERO.
008750       05 転帰区分Ｗ                   PIC 9(1)  VALUE ZERO.
008760       05 転帰区分チェックＷ.
008770          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
008780          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
008790          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
008800       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
008810       05 部位区切Ｗ                   PIC X(1)  VALUE SPACE.
008820       05 経過略称Ｗ.
008830          07 印刷経過略称Ｗ            PIC N(5)  VALUE SPACE.
008840          07 FILLER                    PIC X(2)  VALUE SPACE.
008850    03 経過部位Ｗ                      PIC N(1)  VALUE SPACE.
009030    03 経過ＣＭ                        PIC N(4)  VALUE SPACE.
008860    03 新規チェックＷ                  PIC N(1)  VALUE SPACE.
008870    03 継続チェックＷ                  PIC N(1)  VALUE SPACE.
          03 施術日Ｗ.
             05 施術日チェックＷ   OCCURS 31 PIC N(1)  VALUE SPACE.
008880****************
008890* ＯＣＲコード *
008900****************
008910* 01 ＯＣＲコードＷ.
008920*    03 ＯＣＲ会員番号Ｗ                PIC X(6)    VALUE ZERO.
008930*    03 ＯＣＲ施術年月Ｗ                PIC 9(4)    VALUE ZERO.
008940*    03 ＯＣＲ保険種別Ｗ                PIC 9(2)    VALUE ZERO.
008950*    03 ＯＣＲ各社ＩＤＷ                PIC 9(2)    VALUE ZERO.
008960*    03 ＯＣＲＦＤ連番Ｗ.
008970*       05 ＦＤ連番患者番号Ｗ           PIC 9(6)    VALUE ZERO.
008980*       05 ＦＤ連番健保ＩＤＷ           PIC 9(1)    VALUE ZERO.
008990*    03 ＯＣＲ請求金額Ｗ                PIC 9(6)    VALUE ZERO.
009000*    03 ＯＣＲ負担割合Ｗ                PIC 9(1)    VALUE ZERO.
009010*    03 ＯＣＲ接骨師会会員番号Ｗ        PIC 9(10)   VALUE ZERO.
009020*
009030************
009040* 料金情報 *
009050************
009060 01 料金情報Ｗ.
009070    03 初検加算Ｗ.
009080       05 時間外チェックＷ                PIC N(1) VALUE SPACE.
009090       05 休日チェックＷ                  PIC N(1) VALUE SPACE.
009100       05 深夜チェックＷ                  PIC N(1) VALUE SPACE.
009110    03 往療加算Ｗ.
009120       05 夜間チェックＷ                  PIC N(1) VALUE SPACE.
009130       05 暴風雨雪チェックＷ              PIC N(1) VALUE SPACE.
009140    03 金属副子チェックＷ.
009150       05 大チェックＷ                    PIC N(1) VALUE SPACE.
009160       05 中チェックＷ                    PIC N(1) VALUE SPACE.
009170       05 小チェックＷ                    PIC N(1) VALUE SPACE.
009180    03 小計Ｗ                             PIC 9(7) VALUE ZERO.
009190    03 初回処置料合計Ｗ                   PIC 9(6) VALUE ZERO.
009200    03 初回処置料チェックＷ.
009210       05 整復料チェックＷ                PIC N(1) VALUE SPACE.
009220       05 固定料チェックＷ                PIC N(1) VALUE SPACE.
009230       05 施療料チェックＷ                PIC N(1) VALUE SPACE.
      */金属副子・運動後療の変更・追加/1805
          03 金属回数Ｗ                         PIC 9(2)  VALUE ZERO.
          03 運動料Ｗ                           PIC 9(4)  VALUE ZERO.
009240************
009250* 備考情報 *
009260************
009270 01 備考情報Ｗ.
009280    03 適用１Ｗ                        PIC N(38) VALUE SPACE.
009290    03 適用２Ｗ                        PIC N(38) VALUE SPACE.
009300*    03 適用３Ｗ                        PIC N(38) VALUE SPACE.
009310*    03 適用４Ｗ                        PIC N(38) VALUE SPACE.
009320    03 経過コメントＷ                  PIC N(60) VALUE SPACE.
      *
       01 助成施術日Ｗ                       PIC X(150) VALUE SPACE.
       01 助成施術日ＷＲ.
          03 助成施術日ＷＰ                  PIC X(4)  VALUE SPACE OCCURS 31.
       01 日ＷＰ                             PIC Z9    VALUE ZERO.
       01 施術日文字数Ｗ                     PIC 9(3)  VALUE ZERO.
009330*
009340*****************
009350* レセプト並び順 *
009360*****************
009370 01 順番固定Ｗ                         PIC N(1) VALUE SPACE.
009380 01 順番Ｗ                             PIC 9(4) VALUE ZERO.
009390*
       01 摘要施術日Ｗ                       PIC X(100) VALUE SPACE.
       01 施術日Ｗ.
          03 施術日２Ｗ                      PIC X(1)  VALUE SPACE.
          03 施術日１Ｗ                      PIC X(1)  VALUE SPACE.
      *
002790** 負担割合用
002800 01 負担割合Ｗ                         PIC 9(2)  VALUE ZERO.
002810 01 給付割合Ｗ                         PIC 9(2)  VALUE ZERO.
009400*******************************************************************
009410 01 印刷制御.
009420     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
009430     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
009440     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
009450     03 拡張制御Ｐ.
009460         05 端末制御Ｐ.
009470             07 移動方向Ｐ             PIC X(1) VALUE SPACE.
009480             07 移動行数Ｐ             PIC 9(3) VALUE ZERO.
009490         05 詳細制御Ｐ                 PIC X(2) VALUE SPACE.
009500     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
009510     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
009520*
009530 01 計算機西暦年Ｗ                     PIC 9(2) VALUE ZERO.
009540* 日付ＷＯＲＫ
009550 01 和暦終了年Ｗ                       PIC 9(4) VALUE ZERO.
009560 01 計算機西暦.
009570    03 計算機西暦年                    PIC 9(4) VALUE ZERO.
009580    03 計算機西暦月日                  PIC 9(4) VALUE ZERO.
009590 01 計算機西暦Ｒ REDEFINES 計算機西暦.
009600    03 計算機世紀                      PIC 9(2).
009610    03 計算機日付                      PIC 9(6).
009620    03 計算機日付Ｒ REDEFINES 計算機日付.
009630       05 計算機年月                   PIC 9(4).
009640       05 計算機年月Ｒ REDEFINES 計算機年月.
009650         07 計算機年                   PIC 9(2).
009660         07 計算機月                   PIC 9(2).
009670       05 計算機日                     PIC 9(2).
009680*
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
009690******************************************************************
009700*                          連結項目                              *
009710******************************************************************
009720**  画面入力データ
009730 01 連入－入力データ委任印刷 IS EXTERNAL.
009740    03 連入－委任印刷                     PIC 9.
014620*
       01 連入－入力データ電話印刷 IS EXTERNAL.
          03 連入－電話印刷                     PIC 9.
009190*
       01 連入－プレビュー IS EXTERNAL.
          03 連入－プレビュー区分          PIC 9.
009750*
009760** ３カ月長期判定
009770 01 連期間－キー IS EXTERNAL.
009780    03 連期間－施術年月.
009790       05 連期間－施術和暦               PIC 9.
009800       05 連期間－施術年                 PIC 9(2).
009810       05 連期間－施術月                 PIC 9(2).
009820    03  連期間－患者コード.
009830       05 連期間－患者番号               PIC 9(6).
009840       05 連期間－枝番                   PIC X.
009850    03 連期間－対象フラグ                PIC X(3).
009860    03 連期間－期間月Ｗ.
009870       05 連期間－期間Ｗ                 PIC 9(2) OCCURS 9.
009880************
009890* 印刷キー *
009900************
009910*
009920*
009930 01 連レ印－対象データ IS EXTERNAL.
009940    03 連レ印－施術年月日.
009950       05 連レ印－施術和暦                  PIC 9(1).
009960       05 連レ印－施術年                    PIC 9(2).
009970       05 連レ印－施術月                    PIC 9(2).
009980    03 連レ印－患者コード.
009990       05 連レ印－患者番号                  PIC 9(6).
010000       05 連レ印－枝番                      PIC X(1).
010010    03 連レ印－保険種別                     PIC 9(2).
010020    03 連レ印－保険者番号                   PIC X(10).
010030    03 連レ印－公費種別                     PIC 9(2).
010040    03 連レ印－費用負担者番号               PIC X(10).
010050    03 連レ印－助成種別                     PIC 9(2).
010060    03 連レ印－費用負担者番号助成           PIC X(10).
010070    03 連レ印－患者カナ                     PIC X(20).
010080    03 連レ印－本人家族区分                 PIC 9(1).
013510*
013520 01 連レ－キー IS EXTERNAL.
013530    03 連レ－保険種別                  PIC 9(2).
013540*
013550************************
013560* 長期理由文セット     *
013570************************
013580 01 連長文－キー IS EXTERNAL.
013590    03 連長文－施術年月.
013600       05 連長文－施術和暦               PIC 9.
013610       05 連長文－施術年                 PIC 9(2).
013620       05 連長文－施術月                 PIC 9(2).
013630    03  連長文－患者コード.
013640       05 連長文－患者番号               PIC 9(6).
013650       05 連長文－枝番                   PIC X.
013660    03 連長文－文桁数                    PIC 9(2).
013670    03 連長文－理由文                    PIC N(63) OCCURS 15.
013140*
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
013680*
013681************************
013682* 助成レセまとめ
013683************************
013684 01 連レセまとめ－キー IS EXTERNAL.
013685    03 連レセまとめ－施術和暦年月.
013686       05 連レセまとめ－施術和暦               PIC 9.
013687       05 連レセまとめ－施術年月.
013688          07 連レセまとめ－施術年              PIC 9(2).
013689          07 連レセまとめ－施術月              PIC 9(2).
013690    03 連レセまとめ－患者コード.
013691       05 連レセまとめ－患者番号               PIC 9(6).
013692       05 連レセまとめ－枝番                   PIC X(1).
013693**-------------------------------------------------------**
013694*   1:助成レセプトなしの本体まとめの判定
013695*   2:横浜・川崎用の社保助成レセかの判定
013696    03 連レセまとめ－判定区分                  PIC 9.
013697**-------------------------------------------------------**
013698*  / OUT /　 0:対象外、1:対象
013699    03 連レセまとめ－判定結果                  PIC 9.
013700**
013701*
013702*************
013703* 助成名称
013704*************
013705 01 連助成名称－キー IS EXTERNAL.
013706    03 連助成名称－助成種別             PIC 9(2).
013707    03 連助成名称－費用負担者番号助成   PIC X(10).
013708*   / OUT /
013709    03 連助成名称－名称集団.
013710       05 連助成名称－１文字            PIC N.
013711       05 連助成名称－略称              PIC N(4).
013712       05 連助成名称－正式名称          PIC N(10).
013713*
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
013714*
      * 年齢判定用
       01 連年齢２－キー IS EXTERNAL.
          03 連年齢２－施術和暦年月.
             05 連年齢２－施術和暦      PIC 9.
             05 連年齢２－施術年月.
               07 連年齢２－施術年      PIC 9(2).
               07 連年齢２－施術月      PIC 9(2).
          03 連年齢２－和暦年月日.
             05 連年齢２－和暦          PIC 9.
             05 連年齢２－年            PIC 9(2).
             05 連年齢２－月            PIC 9(2).
             05 連年齢２－日            PIC 9(2).
          03 連年齢２－保険年齢         PIC 9(3).
          03 連年齢２－乳幼児判定       PIC 9.
          03 連年齢２－老人判定         PIC 9.
006510*
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
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
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
      *
       01 連金運２－キー IS EXTERNAL.
           03 連金運２－金属副子ＣＭ.
               05 連金運２－金属副子ＣＭ１        PIC X(130).
               05 連金運２－金属副子ＣＭ２        PIC X(170).
      *
013715******************************************************************
013716*                      PROCEDURE  DIVISION                       *
013717******************************************************************
013720 PROCEDURE               DIVISION.
013730************
013740*           *
013750* 初期処理   *
013760*           *
013770************
002570     PERFORM プリンタファイル作成.
013780     PERFORM 初期化.
013790     PERFORM 制御情報取得.
013800************
013810*           *
013820* 主処理     *
013830*           *
013840************
013850* 印刷
013860     PERFORM 連結項目待避.
013870     PERFORM 印刷セット.
013880     PERFORM 印刷処理.
013890************
013900*           *
013910* 終了処理   *
013920*           *
013930************
013940     PERFORM 受診者印刷区分更新.
013950     PERFORM 終了処理.
013960*     PERFORM 遅延処理.
013970     MOVE ZERO  TO PROGRAM-STATUS.
013980     EXIT PROGRAM.
013990*
014000*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YCH6125"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分  TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
014010*================================================================*
014020 初期化 SECTION.
014030*
014040     PERFORM ファイルオープン.
014050*    /* 現在日付取得 */
014060     ACCEPT 計算機日付 FROM DATE.
014070*    /* 1980～2079年の間で設定 */
014080     IF ( 計算機年 > 80 )
014090         MOVE 19 TO 計算機世紀
014100     ELSE
014110         MOVE 20 TO 計算機世紀
014120     END-IF.
014130     PERFORM カレント元号取得.
014140     PERFORM 和暦終了年取得.
014150     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 和暦終了年Ｗ.
014160*================================================================*
014170 カレント元号取得 SECTION.
014180*
014190     MOVE ZEROS TO 制－制御区分.
014200     READ 制御情報マスタ
014210     NOT INVALID KEY
014220         MOVE 制－カレント元号         TO カレント元号Ｗ
014230         MOVE 制－レセ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
014240         MOVE 制－レセ長期理由印刷区分 TO 長期理由印刷区分Ｗ
014250         MOVE 制－レセプト日付区分     TO レセプト日付区分Ｗ
014260         MOVE 制－レセプト患者日付区分 TO レセプト患者日付区分Ｗ
014261         MOVE 制－全柔ＦＰＤ区分       TO 全柔ＦＰＤ区分Ｗ
015320         MOVE 制－国保レセ             TO 用紙種別Ｗ
014270     END-READ.
014280*
014290*================================================================*
014300 和暦終了年取得 SECTION.
014310*
014320*     DISPLAY NC"カレント元号Ｗ"  カレント元号Ｗ UPON MSGBOX.
014330     MOVE カレント元号Ｗ TO 元－元号区分.
014340     READ 元号マスタ
014350     INVALID KEY
014360         DISPLAY NC"指定和暦が登録されていません" UPON CONS
014370         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
014380                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
014390         ACCEPT  キー入力 FROM CONS
014400         PERFORM 終了処理
014410         EXIT PROGRAM
014420     NOT INVALID KEY
014430         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
014440         MOVE 前和暦Ｗ TO 元－元号区分
014450         READ 元号マスタ
014460         INVALID KEY
014470             DISPLAY NC"指定和暦が登録されていません" UPON CONS
014480             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
014490                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
014500             ACCEPT  キー入力 FROM CONS
014510             PERFORM 終了処理
014520             EXIT PROGRAM
014530         NOT INVALID KEY
014540             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
014550         END-READ
014560     END-READ.
014570*
014580*================================================================*
014590 ファイルオープン SECTION.
014600*
014610     OPEN INPUT   保険者マスタ
014620         MOVE NC"保険者" TO ファイル名.
014630         PERFORM オープンチェック.
014640     OPEN INPUT   元号マスタ
014650         MOVE NC"元号" TO ファイル名.
014660         PERFORM オープンチェック.
014670     OPEN INPUT   名称マスタ
014680         MOVE NC"名称" TO ファイル名.
014690         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
014730     OPEN INPUT   制御情報マスタ
014740         MOVE NC"制御情報" TO ファイル名.
014750         PERFORM オープンチェック.
014760     OPEN INPUT   施術所情報マスタ
014770         MOVE NC"施情" TO ファイル名.
014780         PERFORM オープンチェック.
014790     OPEN INPUT   請求先マスタ
014800         MOVE NC"請先" TO ファイル名.
014810         PERFORM オープンチェック.
014820     OPEN INPUT   経過マスタ
014830         MOVE NC"経過" TO ファイル名.
014840         PERFORM オープンチェック.
014850     OPEN INPUT   施術記録Ｆ.
014860         MOVE NC"施記Ｆ" TO ファイル名.
014870         PERFORM オープンチェック.
014880     OPEN INPUT   負傷データＦ.
014890         MOVE NC"負傷" TO ファイル名.
014900         PERFORM オープンチェック.
014910     OPEN INPUT   負傷原因Ｆ.
014920         MOVE NC"負傷原因" TO ファイル名.
014930         PERFORM オープンチェック.
014940     OPEN INPUT   会情報マスタ.
014950         MOVE NC"会情報マスタ" TO ファイル名.
014960         PERFORM オープンチェック.
014970     OPEN INPUT   ＩＤ管理マスタ
014980         MOVE NC"ＩＤ" TO ファイル名.
014990         PERFORM オープンチェック.
015000     OPEN INPUT 市町村マスタ.
015010         MOVE NC"市町村" TO ファイル名.
015020         PERFORM オープンチェック.
015030     OPEN INPUT メモファイル.
015040         MOVE NC"メモ" TO ファイル名.
015050         PERFORM オープンチェック.
015060     OPEN INPUT  作業ファイル４.
015170         IF ( 状態キー  NOT =  "00" )
015060            OPEN OUTPUT  作業ファイル４
                  CLOSE 作業ファイル４
015060            OPEN INPUT  作業ファイル４
               END-IF.
015090     OPEN I-O   受診者情報Ｆ.
015100         MOVE NC"受情" TO ファイル名.
015110         PERFORM オープンチェック.
015120     OPEN I-O   印刷ファイル
015130         PERFORM エラー処理Ｐ.
015140*================================================================*
015150 オープンチェック SECTION.
015160*
015170     IF ( 状態キー  NOT =  "00" )
015180         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
015190         DISPLAY NC"状態キー：" 状態キー         UPON CONS
015200         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
015210                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015220         ACCEPT  キー入力 FROM CONS
015230         PERFORM ファイル閉鎖
015240         EXIT PROGRAM.
015250*================================================================*
015260 制御情報取得 SECTION.
015270*
015280     MOVE ZERO TO 制－制御区分
015290     READ 制御情報マスタ
015300     NOT INVALID KEY
015310         MOVE 制－最大登録部位数 TO 最大登録数Ｗ
015320         MOVE 制－負傷連続登録   TO 負傷連続登録Ｗ
015330         MOVE 制－遅延回数       TO 遅延回数Ｗ
015340     END-READ.
015350*
015360*================================================================*
015370 遅延処理 SECTION.
015380*
015390     PERFORM VARYING 遅延ＣＮＴ FROM 1 BY 1
015400                                UNTIL 遅延ＣＮＴ > 遅延回数Ｗ
015410         MOVE SPACE TO 遅延フラグ
015420     END-PERFORM.
015430*
015440*================================================================*
015450 連結項目待避 SECTION.
015460*
015470     MOVE 連レ印－施術和暦           TO 施術和暦ＷＲ.
015480     MOVE 連レ印－施術年             TO 施術年ＷＲ.
015490     MOVE 連レ印－施術月             TO 施術月ＷＲ.
015500     MOVE 連レ印－保険種別           TO 保険種別ＷＲ.
015510     MOVE 連レ印－保険者番号         TO 保険者番号ＷＲ.
015520     MOVE 連レ印－公費種別           TO 公費種別ＷＲ.
015530     MOVE 連レ印－費用負担者番号     TO 費用負担者番号ＷＲ.
015540     MOVE 連レ印－助成種別           TO 助成種別ＷＲ.
015550     MOVE 連レ印－費用負担者番号助成 TO 費用負担者番号助成ＷＲ.
015560     MOVE 連レ印－本人家族区分       TO 本人家族区分ＷＲ.
015570     MOVE 連レ印－患者カナ           TO 患者カナＷＲ.
015580     MOVE 連レ印－患者番号           TO 患者番号ＷＲ.
015590     MOVE 連レ印－枝番               TO 枝番ＷＲ.
015600*================================================================*
015610 印刷セット SECTION.
015620*
015630     PERFORM 項目初期化.
014800     PERFORM 負傷読込.
015680     PERFORM 料金情報取得.
015640     PERFORM 施術所情報取得.
015650     PERFORM 請求先情報取得.
015660     PERFORM 受診者情報取得.
015670     PERFORM 負傷データ取得.
015690     PERFORM 施術記録取得.
015700     PERFORM レセプト並び順取得.
015710**     PERFORM ＯＣＲ情報取得.
015720**     PERFORM 長期判定取得.
015730**     PERFORM 初検日以前のデータ判定.
015740     PERFORM 初検加算時刻取得.
015750     PERFORM 助成印取得.
015760     PERFORM 委任年月日取得.
           PERFORM 施術日取得.
015770*
016791*-----------------------------------------------*
016800     IF ( 負傷原因印刷区分Ｗ  NOT = 1 ) AND ( レセ負傷原因印刷区分Ｗ NOT = 1 )
016813        IF ( 負傷原因印刷区分Ｗ = 3 OR 4 )
016815           PERFORM 負傷原因印刷対象判定処理
016817        ELSE
016820           PERFORM 負傷原因取得
016821        END-IF
016830     END-IF.
      */取り消し/160816
      **/千葉県重度心身障害医療費助成事業
      *     IF (受－助成種別 = "53" ) AND (受－費用負担者番号助成(1:4) = "8112" )
      *         MOVE NC"重心"         TO 重心
      *         MOVE NC"○"           TO 重心丸
      *     END-IF.
015900*
015910**********************
015920* ＯＣＲコードセット *
015930**********************
015940*
015950*     MOVE ＯＣＲ接骨師会会員番号Ｗ     TO ＯＣＲ会員番号.
015960*     MOVE ＯＣＲ施術年月Ｗ     TO ＯＣＲ施術年月.
015970*     MOVE ＯＣＲ保険種別Ｗ     TO ＯＣＲ保険種別.
015980*     MOVE ＯＣＲ各社ＩＤＷ     TO ＯＣＲ各社ＩＤ.
015990*     MOVE ＯＣＲＦＤ連番Ｗ     TO ＯＣＲＦＤ連番.
016000*     MOVE ＯＣＲ請求金額Ｗ     TO ＯＣＲ請求金額.
016010*     MOVE ＯＣＲ負担割合Ｗ     TO ＯＣＲ負担割合.
016020*
016030********************
016040* 受診者情報セット *
016050********************
016060     MOVE 施術年Ｗ            TO 施術年.
016070     MOVE 施術月Ｗ            TO 施術月.
016080*
016090*     IF ( 記号Ｗ(1:1) = NC"＊" )
016100*        MOVE  SPACE    TO  記号
016110*     ELSE
016120*        MOVE 記号Ｗ    TO  記号
016130*     END-IF.
016140*     IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
016150*        ( 印刷番号Ｗ(1:2) = "＊" )
016160*        MOVE  SPACE      TO  番号
016170*     ELSE
016180*        MOVE 印刷番号Ｗ  TO  番号
016190*     END-IF.
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
016200*
016210     MOVE 印刷保険者番号Ｗ    TO 保険者番号.
016220*     MOVE 請求先名称Ｗ        TO 保険者名称.
016230*     MOVE 被保険者カナＷ      TO 被保険者カナ.
016240     MOVE 被保険者氏名Ｗ      TO 被保険者氏名.
      */ 郵便番号・電話番号追加 /42505
           IF (施術和暦年月ＷＲ >= 42505) AND (連入－電話印刷 = 1)
              IF (受－柔整郵便電話番号印刷 = 0 OR 2) AND
                 ((郵便番号１Ｗ NOT = SPACE) OR (郵便番号２Ｗ NOT = SPACE))
017280           MOVE "〒"          TO 郵便
017260           MOVE 郵便番号１Ｗ  TO 郵便番号１
017270           MOVE 郵便番号２Ｗ  TO 郵便番号２
017280           MOVE "-"           TO 郵便区切
              END-IF
              IF 受－柔整郵便電話番号印刷 = 0 OR 3
017260           MOVE 電話番号Ｗ    TO 電話番号
              END-IF
           END-IF.
016280     MOVE 被保険者住所１Ｗ    TO 住所１.
016290     MOVE 被保険者住所２Ｗ    TO 住所２.
016300*     MOVE 患者カナＷ          TO 患者カナ.
016310     MOVE 患者氏名Ｗ          TO 患者氏名.
016320     MOVE 男チェックＷ        TO 男チェック.
016330     MOVE 女チェックＷ        TO 女チェック.
016340*     MOVE 性別Ｗ               TO 性別.
016350*     MOVE 明治チェックＷ      TO 明治チェック.
016360*     MOVE 大正チェックＷ      TO 大正チェック.
016370     MOVE 昭和チェックＷ      TO 昭和チェック.
016380     MOVE 平成チェックＷ      TO 平成チェック.
      */元号修正↓↓↓/20190408
           MOVE 令和ＣＭＷ         TO 令和ＣＭ.
023070     MOVE 令和チェックＷ     TO 令和チェック.
017390*     MOVE 元号Ｗ              TO 患者和暦.
      */元号修正↑↑↑/20190408
016390*     MOVE 元号Ｗ              TO 元号.
016400     MOVE 患者年Ｗ            TO 患者年.
016410     MOVE 患者月Ｗ            TO 患者月.
016420     MOVE 患者日Ｗ            TO 患者日.
016430*     MOVE 印刷続柄Ｗ          TO 続柄.
016440     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
016450     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
016460     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
016470     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
016480     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
016480     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
016490*
016500     MOVE 助成印Ｗ            TO 助成印.
016510*
016520*     MOVE 国保チェックＷ      TO 国保チェック.
016530*     MOVE 国保組合チェックＷ  TO 国保組合チェック.
016540*
015190     MOVE 社保チェックＷ     TO 社保チェック.
015210     MOVE 組合チェックＷ     TO 組合チェック.
015220     MOVE 国保チェックＷ     TO 国保チェック.
           MOVE 共済チェックＷ     TO 共済チェック.
           MOVE 自チェックＷ       TO 自チェック.
           MOVE 退職チェックＷ     TO 退職チェック.
           MOVE 後期チェックＷ     TO 後期チェック.
015230     MOVE ７割チェックＷ     TO ７割チェック.
015240     MOVE ８割チェックＷ     TO ８割チェック.
015250     MOVE ９割チェックＷ     TO ９割チェック.
015260     MOVE １０割チェックＷ   TO １０割チェック.
      *
           MOVE 本人チェックＷ     TO 本人チェック.
           MOVE 家族チェックＷ     TO 家族チェック.
           MOVE 単独チェックＷ     TO 単独チェック.
           MOVE ２併チェックＷ     TO ２併チェック.
           MOVE 高一チェックＷ     TO 高一チェック.
           MOVE 高７チェックＷ     TO 高７チェック.
           MOVE ６歳チェックＷ     TO ６歳チェック.
      */元号修正/↓↓↓20190408
037370     IF (施術和暦Ｗ > 4) OR (用紙種別Ｗ > 1)
              MOVE 施術和暦Ｗ         TO 元－元号区分
037380        READ 元号マスタ
037390        NOT INVALID KEY
037400            MOVE 元－元号名称   TO 施術和暦
037410        END-READ
      *        MOVE "===="             TO 施術和暦訂正
           END-IF.
      */元号修正/↑↑↑20190408
      */大阪府内の国保退職後期高齢＋助成の場合は負担者受給者番号を記載する
           IF (受－保険種別 = 01      ) AND (受－保険者番号(1:2) = "27") OR
              (受－保険種別 = 05 OR 08) AND (受－保険者番号(3:2) = "27")
               IF ( 市町村番号Ｗ(1:2) NOT = "99" )
                   MOVE 市町村番号Ｗ TO 公費負担者番号
               END-IF
               IF ( 受給者番号Ｗ(1:1) = "*"  ) OR
                  ( 受給者番号Ｗ(1:2) = "＊" )
                   MOVE SPACE        TO 受給者番号
               ELSE
                   MOVE 受給者番号Ｗ TO 受給者番号
               END-IF
           END-IF.
      */宮城県の国保＋乳幼児の場合は負担者受給者番号を記載する/160609
           IF (受－保険種別 = 01) AND
              (受－助成種別 = 55) AND (市町村番号Ｗ(3:2) = "04")
               IF ( 市町村番号Ｗ(1:2) NOT = "99" )
                   MOVE 市町村番号Ｗ         TO 公費負担者番号
               END-IF
               IF ( 受給者番号Ｗ(1:1) = "*"  ) OR
                  ( 受給者番号Ｗ(1:2) = "＊" )
                  MOVE  SPACE                TO 受給者番号
               ELSE
                  MOVE 受給者番号Ｗ     TO 受給者番号
               END-IF
           END-IF.
016550********************
016560* 長期理由文セット *
016570********************
016580*     MOVE 連長文－理由文(1)    TO 長期理由文１.
016590*     MOVE 連長文－理由文(2)    TO 長期理由文２.
016600*     MOVE 連長文－理由文(3)    TO 長期理由文３.
016610*     MOVE 連長文－理由文(4)    TO 長期理由文４.
016620*     MOVE 連長文－理由文(5)    TO 長期理由文５.
016630*     MOVE 連長文－理由文(6)    TO 長期理由文６.
016640*     MOVE 連長文－理由文(7)    TO 長期理由文７.
016650*     IF ( 連長文－理由文(1)  NOT = SPACE )
016660*        MOVE NC"（長期理由）"  TO 長期理由固定
016670*     END-IF.
016680*
016690********************
016700* 負傷データセット *
016710********************
016720* １部位 *
016730**********
016740     MOVE 負傷名Ｗ(1)       TO 負傷名１.
016750     MOVE 負傷年Ｗ(1)       TO 負傷年１.
016760     MOVE 負傷月Ｗ(1)       TO 負傷月１.
016770     MOVE 負傷日Ｗ(1)       TO 負傷日１.
016780     MOVE 初検年Ｗ(1)       TO 初検年１.
016790     MOVE 初検月Ｗ(1)       TO 初検月１.
016800     MOVE 初検日Ｗ(1)       TO 初検日１.
016810     MOVE 開始年Ｗ(1)       TO 開始年１.
016820     MOVE 開始月Ｗ(1)       TO 開始月１.
016830     MOVE 開始日Ｗ(1)       TO 開始日１.
016840     MOVE 終了年Ｗ(1)       TO 終了年１.
016850     MOVE 終了月Ｗ(1)       TO 終了月１.
016860     MOVE 終了日Ｗ(1)       TO 終了日１.
016870     MOVE 実日数Ｗ(1)       TO 実日数１.
016880     MOVE 治癒チェックＷ(1) TO 治癒チェック１.
016890     MOVE 中止チェックＷ(1) TO 中止チェック１.
016900     MOVE 転医チェックＷ(1) TO 転医チェック１.
016910**********
016920* ２部位 *
016930**********
016940     MOVE 負傷名Ｗ(2)       TO 負傷名２.
016950     MOVE 負傷年Ｗ(2)       TO 負傷年２.
016960     MOVE 負傷月Ｗ(2)       TO 負傷月２.
016970     MOVE 負傷日Ｗ(2)       TO 負傷日２.
016980     MOVE 初検年Ｗ(2)       TO 初検年２.
016990     MOVE 初検月Ｗ(2)       TO 初検月２.
017000     MOVE 初検日Ｗ(2)       TO 初検日２.
017010     MOVE 開始年Ｗ(2)       TO 開始年２.
017020     MOVE 開始月Ｗ(2)       TO 開始月２.
017030     MOVE 開始日Ｗ(2)       TO 開始日２.
017040     MOVE 終了年Ｗ(2)       TO 終了年２.
017050     MOVE 終了月Ｗ(2)       TO 終了月２.
017060     MOVE 終了日Ｗ(2)       TO 終了日２.
017070     MOVE 実日数Ｗ(2)       TO 実日数２.
017080     MOVE 治癒チェックＷ(2) TO 治癒チェック２.
017090     MOVE 中止チェックＷ(2) TO 中止チェック２.
017100     MOVE 転医チェックＷ(2) TO 転医チェック２.
017110**********
017120* ３部位 *
017130**********
017140     MOVE 負傷名Ｗ(3)       TO 負傷名３.
017150     MOVE 負傷年Ｗ(3)       TO 負傷年３.
017160     MOVE 負傷月Ｗ(3)       TO 負傷月３.
017170     MOVE 負傷日Ｗ(3)       TO 負傷日３.
017180     MOVE 初検年Ｗ(3)       TO 初検年３.
017190     MOVE 初検月Ｗ(3)       TO 初検月３.
017200     MOVE 初検日Ｗ(3)       TO 初検日３.
017210     MOVE 開始年Ｗ(3)       TO 開始年３.
017220     MOVE 開始月Ｗ(3)       TO 開始月３.
017230     MOVE 開始日Ｗ(3)       TO 開始日３.
017240     MOVE 終了年Ｗ(3)       TO 終了年３.
017250     MOVE 終了月Ｗ(3)       TO 終了月３.
017260     MOVE 終了日Ｗ(3)       TO 終了日３.
017270     MOVE 実日数Ｗ(3)       TO 実日数３.
017280     MOVE 治癒チェックＷ(3) TO 治癒チェック３.
017290     MOVE 中止チェックＷ(3) TO 中止チェック３.
017300     MOVE 転医チェックＷ(3) TO 転医チェック３.
017310**********
017320* ４部位 *
017330**********
017340     MOVE 負傷名Ｗ(4)       TO 負傷名４.
017350     MOVE 負傷年Ｗ(4)       TO 負傷年４.
017360     MOVE 負傷月Ｗ(4)       TO 負傷月４.
017370     MOVE 負傷日Ｗ(4)       TO 負傷日４.
017380     MOVE 初検年Ｗ(4)       TO 初検年４.
017390     MOVE 初検月Ｗ(4)       TO 初検月４.
017400     MOVE 初検日Ｗ(4)       TO 初検日４.
017410     MOVE 開始年Ｗ(4)       TO 開始年４.
017420     MOVE 開始月Ｗ(4)       TO 開始月４.
017430     MOVE 開始日Ｗ(4)       TO 開始日４.
017440     MOVE 終了年Ｗ(4)       TO 終了年４.
017450     MOVE 終了月Ｗ(4)       TO 終了月４.
017460     MOVE 終了日Ｗ(4)       TO 終了日４.
017470     MOVE 実日数Ｗ(4)       TO 実日数４.
017480     MOVE 治癒チェックＷ(4) TO 治癒チェック４.
017490     MOVE 中止チェックＷ(4) TO 中止チェック４.
017500     MOVE 転医チェックＷ(4) TO 転医チェック４.
017510**********
017520* ５部位 *
017530**********
017540     MOVE 負傷名Ｗ(5)       TO 負傷名５.
017550     MOVE 負傷年Ｗ(5)       TO 負傷年５.
017560     MOVE 負傷月Ｗ(5)       TO 負傷月５.
017570     MOVE 負傷日Ｗ(5)       TO 負傷日５.
017580     MOVE 初検年Ｗ(5)       TO 初検年５.
017590     MOVE 初検月Ｗ(5)       TO 初検月５.
017600     MOVE 初検日Ｗ(5)       TO 初検日５.
017610     MOVE 開始年Ｗ(5)       TO 開始年５.
017620     MOVE 開始月Ｗ(5)       TO 開始月５.
017630     MOVE 開始日Ｗ(5)       TO 開始日５.
017640     MOVE 終了年Ｗ(5)       TO 終了年５.
017650     MOVE 終了月Ｗ(5)       TO 終了月５.
017660     MOVE 終了日Ｗ(5)       TO 終了日５.
017670     MOVE 実日数Ｗ(5)       TO 実日数５.
017680     MOVE 治癒チェックＷ(5) TO 治癒チェック５.
017690     MOVE 中止チェックＷ(5) TO 中止チェック５.
017700     MOVE 転医チェックＷ(5) TO 転医チェック５.
017710**************
017720* 経過セット *
017730**************
017740     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
016870             UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR
016880                   ( 部位ＣＮＴ > 5 )
017770*         MOVE 部位ＣＮＴＷ(部位ＣＮＴ)   TO 経過部位ＣＮＴ(部位ＣＮＴ)
017780*         MOVE 部位区切Ｗ(部位ＣＮＴ)     TO 部位区切(部位ＣＮＴ)
017790         MOVE 印刷経過略称Ｗ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
017800     END-PERFORM.
017810*****************************************
017820*     新規・継続チェックについて        *
017830*   ●新規...初検有り ●継続...初検なし *
017840*****************************************
017850     MOVE 新規チェックＷ    TO 新規チェック.
017860     MOVE 継続チェックＷ    TO 継続チェック.
017870********************
017880* 料金データセット *
017890********************
017900*    ****************************************************************
017910*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
017920*    ****************************************************************
017930     MOVE 初検料ＷＲ                   TO  初検料.
017940     MOVE 時間外チェックＷ             TO  時間外チェック.
017950     MOVE 休日チェックＷ               TO  休日チェック.
017960     MOVE 深夜チェックＷ               TO  深夜チェック.
017970     MOVE 初検加算料ＷＲ               TO  初検加算料.
      *
           IF ((時間外チェックＷ NOT = SPACE) OR (深夜チェックＷ NOT = SPACE) OR
              (休日チェックＷ NOT = SPACE)) AND
              ((初検加算時Ｗ NOT = ZERO) OR (初検加算分Ｗ NOT = ZERO))
              MOVE 初検加算時Ｗ                 TO  初検加算時
              MOVE 初検加算区切Ｗ               TO  初検加算区切
              MOVE 初検加算分Ｗ                 TO  初検加算分
           END-IF.
      *
           MOVE 初検時相談料ＷＲ             TO  初検時相談料.
017980     MOVE 再検料ＷＲ                   TO  再検料.
017990     MOVE 往療距離ＷＲ                 TO  往療距離.
018000     MOVE 往療回数ＷＲ                 TO  往療回数.
018010     MOVE 往療料ＷＲ                   TO  往療料.
018020     MOVE 夜間チェックＷ               TO  夜間チェック.
018030     MOVE 暴風雨雪チェックＷ           TO  暴風雨雪チェック.
018040     MOVE 往療加算料ＷＲ               TO  往療加算料.
      */金属副子・運動後療の変更・追加/1805
           IF 用紙種別Ｗ < 2
              IF ( 施術和暦年月ＷＲ < 43006 )
018050           MOVE 大チェックＷ              TO  大チェック
018060           MOVE 中チェックＷ              TO  中チェック
018070           MOVE 小チェックＷ              TO  小チェック
              ELSE
                 MOVE ALL NC"＝"                TO  金属訂正
              END-IF
           END-IF.
           IF ( 施術和暦年月ＷＲ >= 43006 ) AND ( 金属副子加算料ＷＲ NOT = ZERO )
              MOVE 金属回数Ｗ                TO  金属回数
              MOVE NC"回"                    TO  金属回
           END-IF.
018080     MOVE 金属副子加算料ＷＲ           TO  金属副子加算料.
018090     MOVE 施術情報提供料ＷＲ           TO  施術情報提供料.
018100     MOVE 小計Ｗ                       TO 小計.
018110********************
018120* 初回処置料セット *
018130********************
018140     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
016870             UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR
016880                   ( 部位ＣＮＴ > 5 )
018170         MOVE 初回処置料ＷＲ(部位ＣＮＴ) TO 初回処置料(部位ＣＮＴ)
018180     END-PERFORM.
018190     MOVE 初回処置料合計Ｗ         TO 初回処置料合計
018200*
018210     MOVE 施療料チェックＷ            TO 施療料チェック.
018220     MOVE 整復料チェックＷ            TO 整復料チェック.
018230     MOVE 固定料チェックＷ            TO 固定料チェック.
018240********************
018250* 逓減毎料金セット *
018260********************
018270*    **********
018280*    * １部位 *
018290*    **********
018300     MOVE 後療単価１ＷＲ             TO 後療単価１.
018310     MOVE 後療回数１ＷＲ             TO 後療回数１.
018320     MOVE 後療料１ＷＲ               TO 後療料１.
018330     MOVE 冷罨法回数１ＷＲ           TO 冷罨法回数１.
018340     MOVE 冷罨法料１ＷＲ             TO 冷罨法料１.
018350     MOVE 温罨法回数１ＷＲ           TO 温罨法回数１.
018360     MOVE 温罨法料１ＷＲ             TO 温罨法料１.
018370     MOVE 電療回数１ＷＲ             TO 電療回数１.
018380     MOVE 電療料１ＷＲ               TO 電療料１.
018390     MOVE 小計１ＷＲ                 TO 小計１.
018400     IF ( 長期逓減率１ＷＲ NOT = ZERO )
018410         COMPUTE 長期逓減率１ = 長期逓減率１ＷＲ / 100
018420     END-IF.
018430     MOVE 長期込小計１ＷＲ           TO 長期込小計１.
018440*    **********
018450*    * ２部位 *
018460*    **********
018470     MOVE 後療単価２ＷＲ             TO 後療単価２.
018480     MOVE 後療回数２ＷＲ             TO 後療回数２.
018490     MOVE 後療料２ＷＲ               TO 後療料２.
018500     MOVE 冷罨法回数２ＷＲ           TO 冷罨法回数２.
018510     MOVE 冷罨法料２ＷＲ             TO 冷罨法料２.
018520     MOVE 温罨法回数２ＷＲ           TO 温罨法回数２.
018530     MOVE 温罨法料２ＷＲ             TO 温罨法料２.
018540     MOVE 電療回数２ＷＲ             TO 電療回数２.
018550     MOVE 電療料２ＷＲ               TO 電療料２.
018560     MOVE 小計２ＷＲ                 TO 小計２.
018570     IF ( 長期逓減率２ＷＲ NOT = ZERO )
018580         COMPUTE 長期逓減率２ = 長期逓減率２ＷＲ / 100
018590     END-IF.
018600     MOVE 長期込小計２ＷＲ           TO 長期込小計２.
018610*    ****************
018620*    * ３部位／８割 *
018630*    ****************
018640     MOVE 後療単価３８ＷＲ             TO 後療単価３８.
018650     MOVE 後療回数３８ＷＲ             TO 後療回数３８.
018660     MOVE 後療料３８ＷＲ               TO 後療料３８.
018670     MOVE 冷罨法回数３８ＷＲ           TO 冷罨法回数３８.
018680     MOVE 冷罨法料３８ＷＲ             TO 冷罨法料３８.
018690     MOVE 温罨法回数３８ＷＲ           TO 温罨法回数３８.
018700     MOVE 温罨法料３８ＷＲ             TO 温罨法料３８.
018710     MOVE 電療回数３８ＷＲ             TO 電療回数３８.
018720     MOVE 電療料３８ＷＲ               TO 電療料３８.
018730     MOVE 小計３８ＷＲ                 TO 小計３８.
018740     MOVE 多部位込小計３８ＷＲ         TO 多部位込小計３８.
018750     IF ( 長期逓減率３８ＷＲ NOT = ZERO )
018760         COMPUTE 長期逓減率３８ = 長期逓減率３８ＷＲ / 100
018770     END-IF.
018780     MOVE 長期込小計３８ＷＲ           TO 長期込小計３８.
      */ 逓減率 0.7→0.6 /42505
           IF (施術和暦年月ＷＲ >= 42505)
              MOVE "60"                      TO 逓減３８
              MOVE "0.6"                     TO 多部位３８
           END-IF.
018790*    ****************
018800*    * ３部位／10割 *
018810*    ****************
018820     MOVE 逓減開始月３０ＷＲ           TO 逓減開始月３０.
018830     MOVE 逓減開始日３０ＷＲ           TO 逓減開始日３０.
018840     MOVE 後療単価３０ＷＲ             TO 後療単価３０.
018850     MOVE 後療回数３０ＷＲ             TO 後療回数３０.
018860     MOVE 後療料３０ＷＲ               TO 後療料３０.
018870     MOVE 冷罨法回数３０ＷＲ           TO 冷罨法回数３０.
018880     MOVE 冷罨法料３０ＷＲ             TO 冷罨法料３０.
018890     MOVE 温罨法回数３０ＷＲ           TO 温罨法回数３０.
018900     MOVE 温罨法料３０ＷＲ             TO 温罨法料３０.
018910     MOVE 電療回数３０ＷＲ             TO 電療回数３０.
018920     MOVE 電療料３０ＷＲ               TO 電療料３０.
018930     MOVE 小計３０ＷＲ                 TO 小計３０.
018940     IF ( 長期逓減率３０ＷＲ NOT = ZERO )
018950         COMPUTE 長期逓減率３０ = 長期逓減率３０ＷＲ / 100
018960     END-IF.
018970     MOVE 長期込小計３０ＷＲ           TO 長期込小計３０.

018980*    ****************
018990*    * ４部位／５割 *
019000*    ****************
019010*     MOVE 後療単価４５ＷＲ             TO 後療単価４５.
019020*     MOVE 後療回数４５ＷＲ             TO 後療回数４５.
019030*     MOVE 後療料４５ＷＲ               TO 後療料４５.
019040*     MOVE 冷罨法回数４５ＷＲ           TO 冷罨法回数４５.
019050*     MOVE 冷罨法料４５ＷＲ             TO 冷罨法料４５.
019060*     MOVE 温罨法回数４５ＷＲ           TO 温罨法回数４５.
019070*     MOVE 温罨法料４５ＷＲ             TO 温罨法料４５.
019080*     MOVE 電療回数４５ＷＲ             TO 電療回数４５.
019090*     MOVE 電療料４５ＷＲ               TO 電療料４５.
019100*     MOVE 小計４５ＷＲ                 TO 小計４５.
019110*     MOVE 多部位込小計４５ＷＲ         TO 多部位込小計４５.
019120*     IF ( 長期逓減率４５ＷＲ NOT = ZERO )
019130*         COMPUTE 長期逓減率４５ = 長期逓減率４５ＷＲ / 100
019140*     END-IF.
019150*     MOVE 長期込小計４５ＷＲ           TO 長期込小計４５.
019160*    ****************
019170*    * ４部位／８割 *
019180*    ****************
019190     MOVE 逓減開始月４８ＷＲ           TO 逓減開始月４８.
019200     MOVE 逓減開始日４８ＷＲ           TO 逓減開始日４８.
019210     MOVE 後療単価４８ＷＲ             TO 後療単価４８.
019220     MOVE 後療回数４８ＷＲ             TO 後療回数４８.
019230     MOVE 後療料４８ＷＲ               TO 後療料４８.
019240     MOVE 冷罨法回数４８ＷＲ           TO 冷罨法回数４８.
019250     MOVE 冷罨法料４８ＷＲ             TO 冷罨法料４８.
019260     MOVE 温罨法回数４８ＷＲ           TO 温罨法回数４８.
019270     MOVE 温罨法料４８ＷＲ             TO 温罨法料４８.
019280     MOVE 電療回数４８ＷＲ             TO 電療回数４８.
019290     MOVE 電療料４８ＷＲ               TO 電療料４８.
019300     MOVE 小計４８ＷＲ                 TO 小計４８.
019310     MOVE 多部位込小計４８ＷＲ         TO 多部位込小計４８.
019320     IF ( 長期逓減率４８ＷＲ NOT = ZERO )
019330         COMPUTE 長期逓減率４８ = 長期逓減率４８ＷＲ / 100
019340     END-IF.
019350     MOVE 長期込小計４８ＷＲ           TO 長期込小計４８.
      */ 逓減率 0.7→0.6 /42505
           IF (施術和暦年月ＷＲ >= 42505)
              MOVE "60"                      TO 逓減４８
              MOVE "0.6"                     TO 多部位４８
           END-IF.
019360*    ****************
019370*    * ４部位／10割 *
019380*    ****************
019390     MOVE 逓減開始月４０ＷＲ           TO 逓減開始月４０.
019400     MOVE 逓減開始日４０ＷＲ           TO 逓減開始日４０.
019410     MOVE 後療単価４０ＷＲ             TO 後療単価４０.
019420     MOVE 後療回数４０ＷＲ             TO 後療回数４０.
019430     MOVE 後療料４０ＷＲ               TO 後療料４０.
019440     MOVE 冷罨法回数４０ＷＲ           TO 冷罨法回数４０.
019450     MOVE 冷罨法料４０ＷＲ             TO 冷罨法料４０.
019460     MOVE 温罨法回数４０ＷＲ           TO 温罨法回数４０.
019470     MOVE 温罨法料４０ＷＲ             TO 温罨法料４０.
019480     MOVE 電療回数４０ＷＲ             TO 電療回数４０.
019490     MOVE 電療料４０ＷＲ               TO 電療料４０.
019500     MOVE 小計４０ＷＲ                 TO 小計４０.
019510     IF ( 長期逓減率４０ＷＲ NOT = ZERO )
019520         COMPUTE 長期逓減率４０ = 長期逓減率４０ＷＲ / 100
019530     END-IF.
019540     MOVE 長期込小計４０ＷＲ           TO 長期込小計４０.
019550*
019560*↓***********************************************************************
019570* ５部位／2.5割の印字は必要ない。
019580*------------------------------------------------------------------------*
019590*    *****************
019600*    * ５部位／2.5割 *
019610*    *****************
019620*     MOVE 後療単価５２ＷＲ             TO 後療単価５２.
019630*     MOVE 後療回数５２ＷＲ             TO 後療回数５２.
019640*     MOVE 後療料５２ＷＲ               TO 後療料５２.
019650*     MOVE 冷罨法回数５２ＷＲ           TO 冷罨法回数５２.
019660*     MOVE 冷罨法料５２ＷＲ             TO 冷罨法料５２.
019670*     MOVE 温罨法回数５２ＷＲ           TO 温罨法回数５２.
019680*     MOVE 温罨法料５２ＷＲ             TO 温罨法料５２.
019690*     MOVE 電療回数５２ＷＲ             TO 電療回数５２.
019700*     MOVE 電療料５２ＷＲ               TO 電療料５２.
019710*     MOVE 小計５２ＷＲ                 TO 小計５２.
019720*     MOVE 多部位込小計５２ＷＲ         TO 多部位込小計５２.
019730*     IF ( 長期逓減率５２ＷＲ NOT = ZERO )
019740*         COMPUTE 長期逓減率５２ = 長期逓減率５２ＷＲ / 100
019750*     END-IF.
019760*     MOVE 長期込小計５２ＷＲ           TO 長期込小計５２.
019770*↑***********************************************************************
019780*
019790*    ****************
019800*    * ５部位／５割 *
019810*    ****************
019820*     MOVE 逓減開始月５５ＷＲ           TO 逓減開始月５５.
019830*     MOVE 逓減開始日５５ＷＲ           TO 逓減開始日５５.
019840*     MOVE 後療単価５５ＷＲ             TO 後療単価５５.
019850*     MOVE 後療回数５５ＷＲ             TO 後療回数５５.
019860*     MOVE 後療料５５ＷＲ               TO 後療料５５.
019870*     MOVE 冷罨法回数５５ＷＲ           TO 冷罨法回数５５.
019880*     MOVE 冷罨法料５５ＷＲ             TO 冷罨法料５５.
019890*     MOVE 温罨法回数５５ＷＲ           TO 温罨法回数５５.
019900*     MOVE 温罨法料５５ＷＲ             TO 温罨法料５５.
019910*     MOVE 電療回数５５ＷＲ             TO 電療回数５５.
019920*     MOVE 電療料５５ＷＲ               TO 電療料５５.
019930*     MOVE 小計５５ＷＲ                 TO 小計５５.
019940*     MOVE 多部位込小計５５ＷＲ         TO 多部位込小計５５.
019950*     IF ( 長期逓減率５５ＷＲ NOT = ZERO )
019960*         COMPUTE 長期逓減率５５ = 長期逓減率５５ＷＲ / 100
019970*     END-IF.
019980*     MOVE 長期込小計５５ＷＲ           TO 長期込小計５５.
019990*    ****************
020000*    * ５部位／８割 *
020010*    ****************
020020*     MOVE 逓減開始月５８ＷＲ           TO 逓減開始月５８.
020030*     MOVE 逓減開始日５８ＷＲ           TO 逓減開始日５８.
020040*     MOVE 後療単価５８ＷＲ             TO 後療単価５８.
020050*     MOVE 後療回数５８ＷＲ             TO 後療回数５８.
020060*     MOVE 後療料５８ＷＲ               TO 後療料５８.
020070*     MOVE 冷罨法回数５８ＷＲ           TO 冷罨法回数５８.
020080*     MOVE 冷罨法料５８ＷＲ             TO 冷罨法料５８.
020090*     MOVE 温罨法回数５８ＷＲ           TO 温罨法回数５８.
020100*     MOVE 温罨法料５８ＷＲ             TO 温罨法料５８.
020110*     MOVE 電療回数５８ＷＲ             TO 電療回数５８.
020120*     MOVE 電療料５８ＷＲ               TO 電療料５８.
020130*     MOVE 小計５８ＷＲ                 TO 小計５８.
020140*     MOVE 多部位込小計５８ＷＲ         TO 多部位込小計５８.
020150*     IF ( 長期逓減率５８ＷＲ NOT = ZERO )
020160*         COMPUTE 長期逓減率５８ = 長期逓減率５８ＷＲ / 100
020170*     END-IF.
020180*     MOVE 長期込小計５８ＷＲ           TO 長期込小計５８.
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
020190*    ****************
020200*    * ５部位／10割 *
020210*    ****************
020220*     MOVE 逓減開始月５０ＷＲ           TO 逓減開始月５０.
020230*     MOVE 逓減開始日５０ＷＲ           TO 逓減開始日５０.
020240*     MOVE 後療単価５０ＷＲ             TO 後療単価５０.
020250*     MOVE 後療回数５０ＷＲ             TO 後療回数５０.
020260*     MOVE 後療料５０ＷＲ               TO 後療料５０.
020270*     MOVE 冷罨法回数５０ＷＲ           TO 冷罨法回数５０.
020280*     MOVE 冷罨法料５０ＷＲ             TO 冷罨法料５０.
020290*     MOVE 温罨法回数５０ＷＲ           TO 温罨法回数５０.
020300*     MOVE 温罨法料５０ＷＲ             TO 温罨法料５０.
020310*     MOVE 電療回数５０ＷＲ             TO 電療回数５０.
020320*     MOVE 電療料５０ＷＲ               TO 電療料５０.
020330*     MOVE 小計５０ＷＲ                 TO 小計５０.
020340*     MOVE 長期逓減率５０ＷＲ           TO 長期逓減率５０.
020350*     IF ( 長期逓減率５０ＷＲ NOT = ZERO )
020360*         COMPUTE 長期逓減率５０ = 長期逓減率５０ＷＲ / 100
020370*     END-IF.
020380*     MOVE 長期込小計５０ＷＲ           TO 長期込小計５０.
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
020381*
020390****
020400     MOVE 適用１Ｗ                       TO 適用１.
020410     MOVE 適用２Ｗ                       TO 適用２.
020420*     MOVE 適用３Ｗ                       TO 適用３.
020430*     MOVE 適用４Ｗ                       TO 適用４.
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
              MOVE 13           TO 連金運－会コード
              MOVE ZERO         TO 連金運－用紙種別
              CALL "KINUNRYO"
              CANCEL "KINUNRYO"
              MOVE 連金運２－金属副子ＣＭ１       TO 金属副子ＣＭ１
              MOVE 連金運２－金属副子ＣＭ２       TO 金属副子ＣＭ２
              MOVE 連金運－運動後療ＣＭ           TO 運動後療ＣＭ
              IF ( 金属副子加算料ＷＲ NOT = ZERO )
                 MOVE 金属副子ＣＭ１              TO 金属副子１
                 MOVE 金属副子ＣＭ２              TO 金属副子２
              END-IF
              IF ( 運動料Ｗ NOT = ZERO )
                 MOVE 運動後療ＣＭ                TO 運動後療
              END-IF
           END-IF.
      *
020440     MOVE レセ－合計                     TO 合計.
      */中央は助成まとめの金額を記載する/120209
020450*     MOVE レセ－印刷レセ負担額           TO 一部負担金.
020460*     MOVE レセ－印刷レセ請求額           TO 請求金額.
           MOVE レセ－一部負担金               TO 一部負担金.
           MOVE レセ－請求金額                 TO 請求金額.
020470*
      */取り消し/160816
      **/千葉県の重度心身障害医療費助成/150703
      *     IF ((助成種別ＷＲ = 53) AND (費用負担者番号助成ＷＲ(1:4) =  "8112"))
021370*         MOVE レセ－受給者負担額        TO 受給者負担額
021380*         MOVE レセ－助成請求金額        TO 助成請求額
      *         MOVE "自己負担金額："          TO 一部負担金ＣＭ
      *         MOVE "公費負担金額："          TO 請求金額ＣＭ
      *         MOVE "円" TO 一部負担金円ＣＭ 請求金額円ＣＭ
      *     END-IF.
020480*------------------------------------------------------------------------------------*
020490* 特別（助成レセなしで、本体レセにまとめる時、金額は助成込み・適用２に助成種別印字）
020500*     IF ( 助成レセまとめフラグ = "YES" )
020510*         PERFORM 助成料金計算
020520*         MOVE レセ－合計          TO 合計
020530*         MOVE レセ－受給者負担額  TO 一部負担金
020540*     / 引き算する/
020550*         COMPUTE 請求金額 = レセ－合計 - レセ－印刷レセ受給者負担額
020560*
020570*/深＿夜の空白にストリングしてしまうためNOT SPACEの時は最後に転記する。
020571*         IF 助成種別略称Ｗ NOT = SPACE
020580*            IF ( 適用２Ｗ NOT = SPACE )
020590*                MOVE SPACE TO 助成種別略称Ｗ２
020600*                STRING NC"※"             DELIMITED BY SIZE
020610*                       助成種別略称Ｗ     DELIMITED BY SPACE
020620*                       INTO 助成種別略称Ｗ２
020630*                END-STRING
020640*                MOVE 助成種別略称Ｗ２ TO 適用２(35:4)
020650*            ELSE
020660*                STRING 適用２Ｗ           DELIMITED BY SPACE
020670*                       NC"※"             DELIMITED BY SIZE
020680*                       助成種別略称Ｗ     DELIMITED BY SPACE
020690*                       INTO 適用２
020700*                END-STRING
020710*            END-IF
020711*         END-IF
020720*     END-IF.
      */長野県の国保助成で助成種別が"60"の時、受給者負担額を出す/2106
           IF (保険種別ＷＲ = 01 OR 08) AND (受－費用負担者番号助成(3:2) = "20") AND
              (助成種別ＷＲ = 60)
021370         MOVE レセ－印刷レセ受給者負担額       TO 助成請求額
               MOVE "受給者負担金"                   TO 請求金額ＣＭ
               MOVE "円" TO 請求金額円ＣＭ
               IF ( 受給者番号Ｗ(1:1) = "*"  ) OR
                  ( 受給者番号Ｗ(1:2) = "＊" )
                   MOVE  SPACE           TO 受給者番号
               ELSE
                   MOVE 受給者番号Ｗ     TO 受給者番号
               END-IF
               MOVE SPACE                TO 受給者番号１
               MOVE SPACE                TO 受給者番号３
           END-IF.
      */愛媛県国保連の国保系＋助成（障・母・子）本体まとめで受給者負担額を出す/220601
           IF (((受－保険種別     = 01) AND (受－保険者番号(1:2) = "38")) OR
               ((受－保険種別     = 05 OR 08) AND (受－保険者番号(3:2) = "38"))) AND
              (受－助成種別 NOT = ZERO)
              IF 受－助成種別 = 52 OR 53 OR 55
021370           MOVE レセ－印刷レセ負担額           TO 一部負担金
021370           MOVE レセ－印刷レセ請求額           TO 請求金額
021370           MOVE レセ－印刷レセ助成請求金額     TO 受給者負担額２
037781           MOVE 受－費用負担者番号助成         TO 公費負担者番号 
                 IF ( 受給者番号Ｗ(1:1) = "*"  ) OR
                    ( 受給者番号Ｗ(1:2) = "＊" )
                    MOVE  SPACE               TO 受給者番号
                 ELSE
                    MOVE 受給者番号Ｗ      TO 受給者番号
                 END-IF
                 MOVE SPACE                TO 受給者番号１
                 MOVE SPACE                TO 受給者番号３
                 PERFORM 助成施術日取得
                 MOVE 助成施術日Ｗ       TO 助成施術日
              END-IF
           END-IF.
020968*
020969**********************
020970* 施術所データセット *
020971**********************
           MOVE 都道府県ＪＩＳＷ       TO 都道府県番号.
020972     MOVE 柔整師番号Ｗ           TO 柔整師番号.
020973*     MOVE 定額制受理番号Ｗ       TO 定額制受理番号.
020974     MOVE 施術所郵便番号１Ｗ     TO 施術所郵便番号１.
020975     MOVE 施術所郵便番号２Ｗ     TO 施術所郵便番号２.
      **/施術所住所、長い場合は小さい文字で２行に印刷する/0905
      *     IF 施術所住所Ｗ(45:1) = SPACE
020976*         MOVE 施術所住所Ｗ       TO 施術所住所１
      *     ELSE
      *         MOVE 施術所住所１Ｗ     TO 施術所住所２
      *         MOVE 施術所住所２Ｗ     TO 施術所住所３
      *     END-IF.
020977     MOVE 施術所住所１Ｗ         TO 施術所住所１.
020978     MOVE 施術所住所２Ｗ         TO 施術所住所２.
020979*     MOVE 接骨師会会員番号Ｗ     TO 接骨師会会員番号.
020980     MOVE 代表者カナＷ           TO 代表者カナ.
020981     MOVE 代表者名Ｗ             TO 代表者名.
020982     MOVE 接骨師会会員番号Ｗ     TO 接骨師会会員番号.
020983     MOVE 施術所電話番号Ｗ       TO 施術所電話番号.
020984*
020985     MOVE 接骨院名Ｗ             TO 接骨院名.
020986*
020987*     MOVE 銀行名支店名Ｗ         TO 銀行名支店名.
020988*     MOVE 預金種別コメントＷ     TO 預金種別.
020989     MOVE 口座番号Ｗ             TO 口座番号.
020990     MOVE 口座名義人カナＷ       TO 口座名義人カナ.
020991*     MOVE 口座名義人とカナＷ     TO 口座名義人.
           MOVE 口座名義人Ｗ           TO 口座名義人.
020992     MOVE コメント１Ｗ           TO コメント１.
020993     MOVE コメント２Ｗ           TO コメント２.
021000     MOVE コメント３Ｗ           TO コメント３.
021000     MOVE コメント４Ｗ           TO コメント４.
           MOVE 金融機関名１Ｗ         TO 金融機関名１.
           MOVE 金融機関名２Ｗ         TO 金融機関名２.
      *     MOVE 金融機関名３Ｗ         TO 金融機関名３.
      *     MOVE 金融機関名４Ｗ         TO 金融機関名４.
           MOVE 支店名１Ｗ             TO 支店名１.
           MOVE 支店名２Ｗ             TO 支店名２.
      *     MOVE 支店名３Ｗ             TO 支店名３.
      *     MOVE 支店名４Ｗ             TO 支店名４.
           MOVE 振込チェックＷ         TO 振込チェック.
           MOVE 普通チェックＷ         TO 普通チェック.
           MOVE 当座チェックＷ         TO 当座チェック.
           MOVE 銀行チェックＷ         TO 銀行チェック.
           MOVE 金庫チェックＷ         TO 金庫チェック.
           MOVE 農協チェックＷ         TO 農協チェック.
           MOVE 本店チェックＷ         TO 本店チェック.
           MOVE 支店チェックＷ         TO 支店チェック.
           MOVE 本支所チェックＷ       TO 本支所チェック.
021010*
021020* / 柔整師・患者委任日 /
      */元号修正/↓↓↓20190408
037370     IF (施術和暦Ｗ > 4) OR (用紙種別Ｗ > 1)
               MOVE 施術和暦Ｗ         TO 元－元号区分
037380         READ 元号マスタ
037390         NOT INVALID KEY
037400             MOVE 元－元号名称   TO 受理和暦
037410         END-READ
      *         MOVE "===="             TO 受理和暦訂正
           END-IF.
      */元号修正/↑↑↑20190408
021030     MOVE 柔整師年Ｗ             TO 受理年.
021040     MOVE 柔整師月Ｗ             TO 受理月.
021050     MOVE 柔整師日Ｗ             TO 受理日.
021060* ( 委任年月日 印刷するか )
021070     IF ( 連入－委任印刷  = ZERO )
      */元号修正/↓↓↓20190408
037370     IF (施術和暦Ｗ > 4) OR (用紙種別Ｗ > 1)
                   MOVE 施術和暦Ｗ         TO 元－元号区分
037380             READ 元号マスタ
037390             NOT INVALID KEY
037400                 MOVE 元－元号名称   TO 委任和暦
037410             END-READ
      *             MOVE "===="             TO 委任和暦訂正
               END-IF
      */元号修正/↑↑↑20190408
021080         MOVE 患者委任年Ｗ       TO 委任年
021090         MOVE 患者委任月Ｗ       TO 委任月
021100         MOVE 患者委任日Ｗ       TO 委任日
021110     END-IF.
021120*
021130* 施術ID
021140     MOVE 県施術ＩＤＷ           TO 県施術ＩＤ.
021150*
021160************************
021170* レセプト並び順セット *
021180************************
021190     MOVE 順番固定Ｗ          TO 順番固定.
021200     MOVE 順番Ｗ              TO 順番.
021210     MOVE 患者番号ＷＲ        TO 患者番号.
021220     MOVE 枝番ＷＲ            TO 枝番.
021230*
021240*
021250* 特別コメント
021260*     MOVE 特別コメントＷ      TO 特別コメント.
021270*
021280*     PERFORM テスト印字処理.
021310*-------------------------------------------------------------------------*
021320*--- ※ レセ摘要再セットは、この印刷セットSECTION の最後にやること！ -----*
021330     PERFORM レセ摘要再セット.
021340*-------------------------------------------------------------------------*
021341*
021342*-------------------------------------------------------------------------*
021343*--- ※ 地域特有処理は、この印刷セットSECTION の最後にやること！   　-----*
021344     PERFORM 地域特有処理.
021345*-------------------------------------------------------------------------*
021346*
021350*
021360*================================================================*
021370 項目初期化 SECTION.
021380*
021390     INITIALIZE 施術所情報Ｗ.
021400     INITIALIZE 受診者情報Ｗ.
021410     INITIALIZE 負傷情報Ｗ.
021420     INITIALIZE 備考情報Ｗ.
021430*     INITIALIZE ＯＣＲコードＷ.
021440     INITIALIZE 料金１ＷＲ.
021450     INITIALIZE 料金２ＷＲ.
021460     INITIALIZE 料金３ＷＲ.
021480     INITIALIZE YCH6125P.
021470     MOVE SPACE TO YCH6125P.
021490*================================================================*
021500 料金情報取得 SECTION.
021510*
           MOVE 1            TO レセ－レセ種別.
019550     MOVE 施術和暦ＷＲ TO レセ－施術和暦.
019560     MOVE 施術年ＷＲ   TO レセ－施術年.
019570     MOVE 施術月ＷＲ   TO レセ－施術月.
019580     MOVE 患者番号ＷＲ TO レセ－患者番号.
019590     MOVE 枝番ＷＲ     TO レセ－枝番.
019600     READ レセプトＦ
019630     INVALID KEY
              MOVE SPACE     TO レセ－レコード
              INITIALIZE        レセ－レコード
           END-READ.
021520********************
021530* 料金データセット *
021540********************
021550*    ****************************************************************
021560*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
021570*    ****************************************************************
021580     MOVE レセ－初検料                 TO 初検料ＷＲ.
021590     IF ( レセ－時間外 = 1 )
021600         MOVE NC"○"                   TO 時間外チェックＷ
021610     END-IF.
021620     IF ( レセ－休日 = 1 )
021630         MOVE NC"○"                   TO 休日チェックＷ
021640     END-IF.
021650     IF ( レセ－深夜 = 1 )
021660         MOVE NC"○"                   TO 深夜チェックＷ
021670     END-IF.
021680*
021690     MOVE レセ－初検加算料             TO  初検加算料ＷＲ.
           MOVE レセ－初検時相談料           TO  初検時相談料ＷＲ.
021700     MOVE レセ－再検料                 TO  再検料ＷＲ.
021710     MOVE レセ－往療距離               TO  往療距離ＷＲ.
021720     MOVE レセ－往療回数               TO  往療回数ＷＲ.
021730     MOVE レセ－往療料                 TO  往療料ＷＲ.
021740     MOVE レセ－往療加算料             TO  往療加算料ＷＲ.
021750*
021760     IF ( レセ－夜間 = 1 )
021770         MOVE NC"○"                   TO 夜間チェックＷ
021780     END-IF.
021790     IF ( レセ－暴風雨雪 = 1 )
021800         MOVE NC"○"                   TO 暴風雨雪チェックＷ
021810     END-IF.
021820*
021830     MOVE レセ－金属副子加算料         TO  金属副子加算料ＷＲ.
021840*
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
      *
021940*
021950     MOVE レセ－施術情報提供料          TO  施術情報提供料ＷＲ.
021960* 小計
021970     MOVE レセ－小計                    TO 小計Ｗ.
021980********************
021990* 初回処置料セット *
022000********************
022010     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
022020             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
022030         MOVE レセ－初回処置料(部位ＣＮＴ) TO 初回処置料ＷＲ(部位ＣＮＴ)
022040         IF ( レセ－初回処置料(部位ＣＮＴ) NOT = ZERO )
022050            EVALUATE 負－負傷種別(部位ＣＮＴ)
022060* 捻挫・打撲・挫傷
022070            WHEN 1
022080            WHEN 2
022090            WHEN 3
022100                MOVE NC"○"       TO 施療料チェックＷ
022110* 脱臼・骨折・骨折拘縮
022120            WHEN 4
022130            WHEN 5
022140            WHEN 7
022150                MOVE NC"○"       TO 整復料チェックＷ
022160* 不全骨折・不全骨折拘縮
022170            WHEN 6
022180            WHEN 8
022190                MOVE NC"○"       TO 固定料チェックＷ
022200            END-EVALUATE
022210         END-IF
022220     END-PERFORM.
022230*
022240     MOVE レセ－初回処置料合計         TO 初回処置料合計Ｗ.
022250********************
022260* 逓減毎料金セット *
022270********************
022280*    **********
022290*    * １部位 *
022300*    **********
022310     MOVE レセ－後療単価１             TO 後療単価１ＷＲ.
022320     MOVE レセ－後療回数１             TO 後療回数１ＷＲ.
022330     MOVE レセ－後療料１               TO 後療料１ＷＲ.
022340     MOVE レセ－冷罨法回数１           TO 冷罨法回数１ＷＲ.
022350     MOVE レセ－冷罨法料１             TO 冷罨法料１ＷＲ.
022360     MOVE レセ－温罨法回数１           TO 温罨法回数１ＷＲ.
022370     MOVE レセ－温罨法料１             TO 温罨法料１ＷＲ.
022380     MOVE レセ－電療回数１             TO 電療回数１ＷＲ.
022390     MOVE レセ－電療料１               TO 電療料１ＷＲ.
022400     MOVE レセ－小計１                 TO 小計１ＷＲ.
022410     MOVE レセ－長期逓減率１           TO 長期逓減率１ＷＲ.
022420     MOVE レセ－長期込小計１           TO 長期込小計１ＷＲ.
022430*    **********
022440*    * ２部位 *
022450*    **********
022460     MOVE レセ－後療単価２             TO 後療単価２ＷＲ.
022470     MOVE レセ－後療回数２             TO 後療回数２ＷＲ.
022480     MOVE レセ－後療料２               TO 後療料２ＷＲ.
022490     MOVE レセ－冷罨法回数２           TO 冷罨法回数２ＷＲ.
022500     MOVE レセ－冷罨法料２             TO 冷罨法料２ＷＲ.
022510     MOVE レセ－温罨法回数２           TO 温罨法回数２ＷＲ.
022520     MOVE レセ－温罨法料２             TO 温罨法料２ＷＲ.
022530     MOVE レセ－電療回数２             TO 電療回数２ＷＲ.
022540     MOVE レセ－電療料２               TO 電療料２ＷＲ.
022550     MOVE レセ－小計２                 TO 小計２ＷＲ.
022560     MOVE レセ－長期逓減率２           TO 長期逓減率２ＷＲ.
022570     MOVE レセ－長期込小計２           TO 長期込小計２ＷＲ.
022580*    ****************
022590*    * ３部位／８割 *
022600*    ****************
022610     MOVE レセ－後療単価３８             TO 後療単価３８ＷＲ.
022620     MOVE レセ－後療回数３８             TO 後療回数３８ＷＲ.
022630     MOVE レセ－後療料３８               TO 後療料３８ＷＲ.
022640     MOVE レセ－冷罨法回数３８           TO 冷罨法回数３８ＷＲ.
022650     MOVE レセ－冷罨法料３８             TO 冷罨法料３８ＷＲ.
022660     MOVE レセ－温罨法回数３８           TO 温罨法回数３８ＷＲ.
022670     MOVE レセ－温罨法料３８             TO 温罨法料３８ＷＲ.
022680     MOVE レセ－電療回数３８             TO 電療回数３８ＷＲ.
022690     MOVE レセ－電療料３８               TO 電療料３８ＷＲ.
022700     MOVE レセ－小計３８                 TO 小計３８ＷＲ.
022710     MOVE レセ－多部位込小計３８         TO 多部位込小計３８ＷＲ.
022720     MOVE レセ－長期逓減率３８           TO 長期逓減率３８ＷＲ.
022730     MOVE レセ－長期込小計３８           TO 長期込小計３８ＷＲ.
022740*    ****************
022750*    * ３部位／10割 *
022760*    ****************
022770     MOVE レセ－逓減開始月３０           TO 逓減開始月３０ＷＲ.
022780     MOVE レセ－逓減開始日３０           TO 逓減開始日３０ＷＲ.
022790     MOVE レセ－後療単価３０             TO 後療単価３０ＷＲ.
022800     MOVE レセ－後療回数３０             TO 後療回数３０ＷＲ.
022810     MOVE レセ－後療料３０               TO 後療料３０ＷＲ.
022820     MOVE レセ－冷罨法回数３０           TO 冷罨法回数３０ＷＲ.
022830     MOVE レセ－冷罨法料３０             TO 冷罨法料３０ＷＲ.
022840     MOVE レセ－温罨法回数３０           TO 温罨法回数３０ＷＲ.
022850     MOVE レセ－温罨法料３０             TO 温罨法料３０ＷＲ.
022860     MOVE レセ－電療回数３０             TO 電療回数３０ＷＲ.
022870     MOVE レセ－電療料３０               TO 電療料３０ＷＲ.
022880     MOVE レセ－小計３０                 TO 小計３０ＷＲ.
022890     MOVE レセ－長期逓減率３０           TO 長期逓減率３０ＷＲ.
022900     MOVE レセ－長期込小計３０           TO 長期込小計３０ＷＲ.
022910*    ****************
022920*    * ４部位／５割 *
022930*    ****************
022940     MOVE レセ－後療単価４５             TO 後療単価４５ＷＲ.
022950     MOVE レセ－後療回数４５             TO 後療回数４５ＷＲ.
022960     MOVE レセ－後療料４５               TO 後療料４５ＷＲ.
022970     MOVE レセ－冷罨法回数４５           TO 冷罨法回数４５ＷＲ.
022980     MOVE レセ－冷罨法料４５             TO 冷罨法料４５ＷＲ.
022990     MOVE レセ－温罨法回数４５           TO 温罨法回数４５ＷＲ.
023000     MOVE レセ－温罨法料４５             TO 温罨法料４５ＷＲ.
023010     MOVE レセ－電療回数４５             TO 電療回数４５ＷＲ.
023020     MOVE レセ－電療料４５               TO 電療料４５ＷＲ.
023030     MOVE レセ－小計４５                 TO 小計４５ＷＲ.
023040     MOVE レセ－多部位込小計４５         TO 多部位込小計４５ＷＲ.
023050     MOVE レセ－長期逓減率４５           TO 長期逓減率４５ＷＲ.
023060     MOVE レセ－長期込小計４５           TO 長期込小計４５ＷＲ.
023070*    ****************
023080*    * ４部位／８割 *
023090*    ****************
023100     MOVE レセ－逓減開始月４８           TO 逓減開始月４８ＷＲ.
023110     MOVE レセ－逓減開始日４８           TO 逓減開始日４８ＷＲ.
023120     MOVE レセ－後療単価４８             TO 後療単価４８ＷＲ.
023130     MOVE レセ－後療回数４８             TO 後療回数４８ＷＲ.
023140     MOVE レセ－後療料４８               TO 後療料４８ＷＲ.
023150     MOVE レセ－冷罨法回数４８           TO 冷罨法回数４８ＷＲ.
023160     MOVE レセ－冷罨法料４８             TO 冷罨法料４８ＷＲ.
023170     MOVE レセ－温罨法回数４８           TO 温罨法回数４８ＷＲ.
023180     MOVE レセ－温罨法料４８             TO 温罨法料４８ＷＲ.
023190     MOVE レセ－電療回数４８             TO 電療回数４８ＷＲ.
023200     MOVE レセ－電療料４８               TO 電療料４８ＷＲ.
023210     MOVE レセ－小計４８                 TO 小計４８ＷＲ.
023220     MOVE レセ－多部位込小計４８         TO 多部位込小計４８ＷＲ.
023230     MOVE レセ－長期逓減率４８           TO 長期逓減率４８ＷＲ.
023240     MOVE レセ－長期込小計４８           TO 長期込小計４８ＷＲ.
023250*    ****************
023260*    * ４部位／10割 *
023270*    ****************
023280     MOVE レセ－逓減開始月４０           TO 逓減開始月４０ＷＲ.
023290     MOVE レセ－逓減開始日４０           TO 逓減開始日４０ＷＲ.
023300     MOVE レセ－後療単価４０             TO 後療単価４０ＷＲ.
023310     MOVE レセ－後療回数４０             TO 後療回数４０ＷＲ.
023320     MOVE レセ－後療料４０               TO 後療料４０ＷＲ.
023330     MOVE レセ－冷罨法回数４０           TO 冷罨法回数４０ＷＲ.
023340     MOVE レセ－冷罨法料４０             TO 冷罨法料４０ＷＲ.
023350     MOVE レセ－温罨法回数４０           TO 温罨法回数４０ＷＲ.
023360     MOVE レセ－温罨法料４０             TO 温罨法料４０ＷＲ.
023370     MOVE レセ－電療回数４０             TO 電療回数４０ＷＲ.
023380     MOVE レセ－電療料４０               TO 電療料４０ＷＲ.
023390     MOVE レセ－小計４０                 TO 小計４０ＷＲ.
023400     MOVE レセ－長期逓減率４０           TO 長期逓減率４０ＷＲ.
023410     MOVE レセ－長期込小計４０           TO 長期込小計４０ＷＲ.
023420*    *****************
023430*    * ５部位／2.5割 *
023440*    *****************
023450     MOVE レセ－後療単価５２             TO 後療単価５２ＷＲ.
023460     MOVE レセ－後療回数５２             TO 後療回数５２ＷＲ.
023470     MOVE レセ－後療料５２               TO 後療料５２ＷＲ.
023480     MOVE レセ－冷罨法回数５２           TO 冷罨法回数５２ＷＲ.
023490     MOVE レセ－冷罨法料５２             TO 冷罨法料５２ＷＲ.
023500     MOVE レセ－温罨法回数５２           TO 温罨法回数５２ＷＲ.
023510     MOVE レセ－温罨法料５２             TO 温罨法料５２ＷＲ.
023520     MOVE レセ－電療回数５２             TO 電療回数５２ＷＲ.
023530     MOVE レセ－電療料５２               TO 電療料５２ＷＲ.
023540     MOVE レセ－小計５２                 TO 小計５２ＷＲ.
023550     MOVE レセ－多部位込小計５２         TO 多部位込小計５２ＷＲ.
023560     MOVE レセ－長期逓減率５２           TO 長期逓減率５２ＷＲ.
023570     MOVE レセ－長期込小計５２           TO 長期込小計５２ＷＲ.
023580*    ****************
023590*    * ５部位／５割 *
023600*    ****************
023610     MOVE レセ－逓減開始月５５           TO 逓減開始月５５ＷＲ.
023620     MOVE レセ－逓減開始日５５           TO 逓減開始日５５ＷＲ.
023630     MOVE レセ－後療単価５５             TO 後療単価５５ＷＲ.
023640     MOVE レセ－後療回数５５             TO 後療回数５５ＷＲ.
023650     MOVE レセ－後療料５５               TO 後療料５５ＷＲ.
023660     MOVE レセ－冷罨法回数５５           TO 冷罨法回数５５ＷＲ.
023670     MOVE レセ－冷罨法料５５             TO 冷罨法料５５ＷＲ.
023680     MOVE レセ－温罨法回数５５           TO 温罨法回数５５ＷＲ.
023690     MOVE レセ－温罨法料５５             TO 温罨法料５５ＷＲ.
023700     MOVE レセ－電療回数５５             TO 電療回数５５ＷＲ.
023710     MOVE レセ－電療料５５               TO 電療料５５ＷＲ.
023720     MOVE レセ－小計５５                 TO 小計５５ＷＲ.
023730     MOVE レセ－多部位込小計５５         TO 多部位込小計５５ＷＲ.
023740     MOVE レセ－長期逓減率５５           TO 長期逓減率５５ＷＲ.
023750     MOVE レセ－長期込小計５５           TO 長期込小計５５ＷＲ.
023760*    ****************
023770*    * ５部位／８割 *
023780*    ****************
023790     MOVE レセ－逓減開始月５８           TO 逓減開始月５８ＷＲ.
023800     MOVE レセ－逓減開始日５８           TO 逓減開始日５８ＷＲ.
023810     MOVE レセ－後療単価５８             TO 後療単価５８ＷＲ.
023820     MOVE レセ－後療回数５８             TO 後療回数５８ＷＲ.
023830     MOVE レセ－後療料５８               TO 後療料５８ＷＲ.
023840     MOVE レセ－冷罨法回数５８           TO 冷罨法回数５８ＷＲ.
023850     MOVE レセ－冷罨法料５８             TO 冷罨法料５８ＷＲ.
023860     MOVE レセ－温罨法回数５８           TO 温罨法回数５８ＷＲ.
023870     MOVE レセ－温罨法料５８             TO 温罨法料５８ＷＲ.
023880     MOVE レセ－電療回数５８             TO 電療回数５８ＷＲ.
023890     MOVE レセ－電療料５８               TO 電療料５８ＷＲ.
023900     MOVE レセ－小計５８                 TO 小計５８ＷＲ.
023910     MOVE レセ－多部位込小計５８         TO 多部位込小計５８ＷＲ.
023920     MOVE レセ－長期逓減率５８           TO 長期逓減率５８ＷＲ.
023930     MOVE レセ－長期込小計５８           TO 長期込小計５８ＷＲ.
023940*    ****************
023950*    * ５部位／10割 *
023960*    ****************
023970     MOVE レセ－逓減開始月５０           TO 逓減開始月５０ＷＲ.
023980     MOVE レセ－逓減開始日５０           TO 逓減開始日５０ＷＲ.
023990     MOVE レセ－後療単価５０             TO 後療単価５０ＷＲ.
024000     MOVE レセ－後療回数５０             TO 後療回数５０ＷＲ.
024010     MOVE レセ－後療料５０               TO 後療料５０ＷＲ.
024020     MOVE レセ－冷罨法回数５０           TO 冷罨法回数５０ＷＲ.
024030     MOVE レセ－冷罨法料５０             TO 冷罨法料５０ＷＲ.
024040     MOVE レセ－温罨法回数５０           TO 温罨法回数５０ＷＲ.
024050     MOVE レセ－温罨法料５０             TO 温罨法料５０ＷＲ.
024060     MOVE レセ－電療回数５０             TO 電療回数５０ＷＲ.
024070     MOVE レセ－電療料５０               TO 電療料５０ＷＲ.
024080     MOVE レセ－小計５０                 TO 小計５０ＷＲ.
024090     MOVE レセ－長期逓減率５０           TO 長期逓減率５０ＷＲ.
024100     MOVE レセ－長期込小計５０           TO 長期込小計５０ＷＲ.
024110*
024120*================================================================*
024130 施術所情報取得 SECTION.
024140*
024150**************************************************
024160* 本院データを使用し、以下の情報を取得           *
024170* ● 柔整師番号.. 柔整師番号Ｗに格納             *
024180* ● 会員番号 ... 接骨師会会員番号Ｗに格納       *
024190* ● 代表者名 ... 代表者名Ｗに格納               *
024200* ● 住所1,2   ...施術所住所1,2Ｗに格納          *
024210* ● 電話番号 ... 施術所電話番号Ｗに格納         *
024220**************************************************
024230     MOVE ZERO  TO 施情－施術所番号.
024240     READ 施術所情報マスタ
024250     INVALID KEY
024260         CONTINUE
024270     NOT INVALID KEY
024280*
               MOVE 施情－都道府県ＪＩＳ     TO 都道府県ＪＩＳＷ
024320         MOVE 施情－新柔整師番号       TO 柔整師番号Ｗ
024340*
024350*         MOVE 施情－接骨師会会員番号  TO ＯＣＲ接骨師会会員番号Ｗ
024360*
024370         STRING "中央-"                DELIMITED BY SIZE
024380                施情－接骨師会会員番号 DELIMITED BY SPACE
024390           INTO 接骨師会会員番号Ｗ
024400         END-STRING
024410*
024420         MOVE 施情－郵便番号１        TO 施術所郵便番号１Ｗ
024430         MOVE 施情－郵便番号２        TO 施術所郵便番号２Ｗ
024440         MOVE 施情－代表者カナ        TO 代表者カナＷ
024450         MOVE 施情－代表者名          TO 代表者名Ｗ
024460         MOVE 施情－接骨院名          TO 接骨院名Ｗ
024470*
024480         MOVE 施情－住所１            TO 施術所住所１Ｗ
024490         MOVE 施情－住所２            TO 施術所住所２Ｗ
024500*         STRING 施情－住所１  DELIMITED BY SPACE
024510*                施情－住所２  DELIMITED BY SPACE
024520*           INTO 施術所住所Ｗ
024530*         END-STRING
024540*
024550         MOVE 施情－電話番号          TO 施術所電話番号Ｗ
024560**
024570** 振込先情報
024580         EVALUATE 保険種別ＷＲ 
024590* 国保
024600         WHEN 01
024610            MOVE 保険者番号ＷＲ       TO 保険者番号比較Ｗ
024620* 退職
024630         WHEN 08
024640            MOVE 保険者番号ＷＲ(3:6)  TO 保険者番号比較Ｗ
024650         END-EVALUATE
024660**
024670** 保険者番号 13XXXX の時、会情報マスタより
024680         IF ( 保険者番号比較Ｗ(1:2) = "13" )
024700             MOVE ZERO             TO  会情－柔整鍼灸区分
024690             MOVE 13               TO  会情－協会コード
024700             MOVE ZERO             TO  会情－保険種別
024710             MOVE 施術和暦年月ＷＲ TO  会情－変更和暦年月
024720             START 会情報マスタ KEY IS <  会情－柔整鍼灸区分
034410                                          会情－協会コード
                                                会情－保険種別
                                                会情－変更和暦年月
034420                                          REVERSED
034430             END-START
034440             IF ( 状態キー = "00" )
034450                 MOVE SPACE  TO 終了フラグ３
034460                 READ 会情報マスタ NEXT
                       END-READ
024740                 MOVE 会情－取引先銀行名      TO 取引先銀行名Ｗ
024750                 MOVE 会情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
024760                 MOVE 会情－預金種別          TO 預金種別Ｗ
024770                 MOVE 会情－口座番号          TO 口座番号Ｗ
024780*                 MOVE 会情－口座名義人        TO 口座名義人とカナＷ
024780                 MOVE 会情－口座名義人        TO 口座名義人Ｗ
024790                 MOVE 会情－口座名義人カナ    TO 口座名義人カナＷ
024800*                 EVALUATE 預金種別Ｗ
024810*                 WHEN 1
024820*                     MOVE "(普)"  TO 預金種別コメントＸＷ
024830*                 WHEN 2
024840*                     MOVE "(当)"  TO 預金種別コメントＸＷ
024850*                 WHEN OTHER
024860*                     MOVE SPACE   TO 預金種別コメントＸＷ
024870*                 END-EVALUATE
024880*
024890*                 STRING 取引先銀行名Ｗ       DELIMITED BY SPACE
024900*                        " "                  DELIMITED BY SIZE
024910*                        取引先銀行支店名Ｗ   DELIMITED BY SPACE
024920*                        "  "                 DELIMITED BY SIZE
024930*                        預金種別コメントＸＷ DELIMITED BY SPACE
024940*                        " "                  DELIMITED BY SIZE
024950*                        会情－口座番号       DELIMITED BY SPACE
024960*                        INTO 銀行名支店名Ｗ
024970*                 END-STRING
024980             END-IF
024990* 固定印字
025000*             MOVE NC"また、私が取得した上記金額"   TO  コメント１Ｗ
025010*             MOVE NC"の受領を中央接骨師会会長　"   TO  コメント２Ｗ
025020*             MOVE 会情－接骨師会会長名   TO 会長名Ｗ
025030*             STRING 会長名Ｗ             DELIMITED BY SIZE
025040*                    NC"に委任します。"   DELIMITED BY SIZE
025050*                    INTO コメント４Ｗ
025060*             END-STRING
025070*
025080*             MOVE "また、私が取得した上記金額の受領を、" TO  コメント１Ｗ
025090*             MOVE "下記の(協)中央接骨師会理事長に委任"   TO  コメント２Ｗ
025100*             MOVE "します。"                             TO  コメント３Ｗ
025080             MOVE "また、私が取得した上記金額の受領を、" TO  コメント１Ｗ
025090             MOVE "(協)中央接骨師会理事長 根岸 進"       TO  コメント２Ｗ
025100             MOVE "(東京都板橋区成増2-9-5)に委任"        TO  コメント３Ｗ
025100             MOVE "します。"                             TO  コメント４Ｗ
025110*
025120** 保険者番号 13XXXX 以外
025130         ELSE
025140             MOVE 施情－取引先銀行名      TO 取引先銀行名Ｗ
025150             MOVE 施情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
025160             MOVE 施情－預金種別          TO 預金種別Ｗ
025170             MOVE 施情－口座番号          TO 口座番号Ｗ
025180             MOVE 施情－口座名義人        TO 口座名義人Ｗ
                   MOVE 施情－口座名義人カナ    TO 口座名義人カナＷ
025190             STRING 取引先銀行名Ｗ     DELIMITED BY SPACE
025200                    " "                DELIMITED BY SIZE
025210                    取引先銀行支店名Ｗ DELIMITED BY SPACE
025220                    INTO 銀行名支店名Ｗ
025230             END-STRING
025240*             STRING 口座名義人Ｗ           DELIMITED BY SPACE
025250*                    "  "                   DELIMITED BY SIZE
025260*                    施情－口座名義人カナ   DELIMITED BY SIZE
025270*                    INTO 口座名義人とカナＷ
025280*             END-STRING
025290             EVALUATE 預金種別Ｗ
025300             WHEN 1
025310                 MOVE NC"（普）" TO 預金種別コメントＷ
025320             WHEN 2
025330                 MOVE NC"（当）" TO 預金種別コメントＷ
025340             WHEN OTHER
025350                 MOVE SPACE      TO 預金種別コメントＷ
025360             END-EVALUATE
025370         END-IF
025380*
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
009745         IF 取引先銀行名Ｗ NOT = SPACE
009746            PERFORM VARYING カウンタ FROM 40 BY -1
009747                      UNTIL (取引先銀行名Ｗ(カウンタ:1) NOT = SPACE) OR
009748                            (カウンタ <= ZERO)
009749                CONTINUE
009750            END-PERFORM
009751            IF カウンタ > 4
009752               IF 取引先銀行名Ｗ(カウンタ - 3 : 4)  = "銀行"
009753                  MOVE  取引先銀行名Ｗ(1:カウンタ - 4)   TO 金融機関名Ｗ
009754                  MOVE NC"○" TO 銀行チェックＷ
009755               ELSE
009756                  IF 取引先銀行名Ｗ(カウンタ - 3 : 4)  = "金庫"
009757                     MOVE  取引先銀行名Ｗ(1:カウンタ - 4)   TO 金融機関名Ｗ
009758                     MOVE NC"○" TO 金庫チェックＷ
009759                  ELSE
009760                     IF 取引先銀行名Ｗ(カウンタ - 3 : 4)  = "農協"
009761                        MOVE  取引先銀行名Ｗ(1:カウンタ - 4)   TO 金融機関名Ｗ
009762                        MOVE NC"○" TO 農協チェックＷ
009763                     ELSE
009764                        MOVE  取引先銀行名Ｗ  TO 金融機関名Ｗ
009765                     END-IF
009766                  END-IF
009767               END-IF
009768            ELSE
009769               MOVE  取引先銀行名Ｗ  TO 金融機関名Ｗ
009770            END-IF
009771         END-IF
009779*
009780         IF 取引先銀行支店名Ｗ NOT = SPACE
009781            PERFORM VARYING カウンタ FROM 40 BY -1
009782                      UNTIL (取引先銀行支店名Ｗ(カウンタ:1) NOT = SPACE) OR
009783                            (カウンタ <= ZERO)
009784                CONTINUE
009785            END-PERFORM
009786            IF カウンタ >= 4
009787               IF 取引先銀行支店名Ｗ(カウンタ - 3 : 4)  = "本店"
009788                  MOVE  取引先銀行支店名Ｗ(1:カウンタ - 4)   TO 支店名Ｗ
009789                  MOVE NC"○" TO 本店チェックＷ
009790               ELSE
009791                  IF 取引先銀行支店名Ｗ(カウンタ - 3 : 4)  = "支店"
009792                     MOVE  取引先銀行支店名Ｗ(1:カウンタ - 4)   TO 支店名Ｗ
009793                     MOVE NC"○" TO 支店チェックＷ
009794                  ELSE
009791                     IF 取引先銀行支店名Ｗ(カウンタ - 3 : 4)  = "支所"
009792                        MOVE  取引先銀行支店名Ｗ(1:カウンタ - 4)   TO 支店名Ｗ
009793                        MOVE NC"○" TO 本支所チェックＷ
009794                     ELSE
009791                         IF 取引先銀行支店名Ｗ(カウンタ - 3 : 4)  = "本所"
009792                            MOVE  取引先銀行支店名Ｗ(1:カウンタ - 4)   TO 支店名Ｗ
009793                            MOVE NC"○" TO 本支所チェックＷ
009794                         ELSE
009800                             MOVE  取引先銀行支店名Ｗ  TO 支店名Ｗ
009801                         END-IF
009804                     END-IF
009805                  END-IF
009806               END-IF
009807            ELSE
009808               MOVE  取引先銀行支店名Ｗ  TO 支店名Ｗ
009809            END-IF
009810         END-IF
      *
025390*********************************************
025400** ＩＤ管理マスタより　県施術ＩＤを取得する。
025410*   (国保組合は、対象外　→　対象！2005/09 )
025420*********************************************
025430**   / 県施術ID /
025440***         IF ( 保険者番号比較Ｗ(3:1)  NOT = "3" )
025450            MOVE 01                     TO ＩＤ管－ＩＤ区分
025460            MOVE ZERO                   TO ＩＤ管－施術所番号
025470            MOVE 保険者番号比較Ｗ(1:2)  TO ＩＤ管－保険種別
025480            MOVE SPACE                  TO ＩＤ管－保険者番号
025490            READ ＩＤ管理マスタ
025500            NOT INVALID KEY
025510                MOVE ＩＤ管－施術ＩＤ番号   TO 県施術ＩＤＷ
025520            END-READ
025530***         END-IF
025550*
025560     END-READ.
025570*
025580*================================================================*
025590 受診者情報取得 SECTION.
025600*
025610**************************************************
025620* 連結データから受診者情報Ｆより以下の情報を取得 *
025630* ● 施術年 ..... 施術年Ｗに格納                 *
025640* ● 施術月 ..... 施術月Ｗに格納                 *
025650* ● 患者番号.... 患者番号Ｗに格納※ＦＤ連番用   *
025660* ● 記号 ....... 記号Ｗに格納                   *
025670* ● 番号 ....... 番号Ｗに格納                   *
025680* ● 保険者番号 . 保険者番号Ｗに格納             *
025690* ● 保険種別 ... 保険種別Ｗに格納               *
025700* ● 被保険者カナ.被保険者カナＷに格納           *
025710* ● 被保険者氏名.被保険者氏名Ｗに格納           *
025720* ● 住所１ ......被保険者住所１Ｗに格納         *
025730* ● 住所２ ......被保険者住所２Ｗに格納         *
025740* ● 患者カナ ....患者カナＷに格納               *
025750* ● 患者氏名 ....患者氏名Ｗに格納               *
025760* ● 患者性別 ....区分によりチェックに"○"を格納 *
025770* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
025780* ● 患者年 ......患者年Ｗに格納                 *
025790* ● 患者月 ......患者月Ｗに格納                 *
025800* ● 患者日 ......患者日Ｗに格納                 *
025810* ● 続柄 ........名称マスタより続柄Ｗに取得     *
025820**************************************************
025830     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
025840     MOVE 施術年ＷＲ         TO 受－施術年.
025850     MOVE 施術月ＷＲ         TO 受－施術月.
025860     MOVE 患者コードＷＲ     TO 受－患者コード.
025870     READ 受診者情報Ｆ
025880     INVALID KEY
025890         CONTINUE
025900*            /* ありえない */
025910     NOT INVALID KEY
022660         EVALUATE 受－保険種別
022670         WHEN 01
022690            MOVE NC"○"        TO 国保チェックＷ
022700         WHEN 02
022710         WHEN 06
022750         WHEN 07
022720            MOVE NC"○"        TO 社保チェックＷ
022730         WHEN 03
022740            MOVE NC"○"        TO 組合チェックＷ
022750*         WHEN 07
022760*            MOVE NC"○"        TO 船員チェックＷ
               WHEN 04
                  MOVE NC"○"        TO 共済チェックＷ
               WHEN 09
                  MOVE NC"○"        TO 自チェックＷ
               WHEN 08
                  MOVE NC"○"        TO 退職チェックＷ
               WHEN 05
                  MOVE NC"○"        TO 後期チェックＷ
022770         END-EVALUATE
      *
               IF 受－助成種別 = ZERO
                   MOVE NC"○" TO 単独チェックＷ
               ELSE
                   MOVE NC"○" TO ２併チェックＷ
               END-IF
      */愛媛県国保連の国保系＋助成で本体レセは単独に○をする/20201029
               IF (((受－保険種別     = 01) AND (受－保険者番号(1:2) = "38")) OR
                   ((受－保険種別     = 05 OR 08) AND (受－保険者番号(3:2) = "38"))) AND
                  (受－助成種別 NOT = ZERO)
      */愛媛県国保連の国保系＋助成（障・母・子）は本体まとめで２併に○をする/220601
                  IF 受－助成種別 = 52 OR 53 OR 55
                     MOVE SPACE  TO 単独チェックＷ
                     MOVE NC"○" TO ２併チェックＷ
                  ELSE
                     MOVE NC"○" TO 単独チェックＷ
                     MOVE SPACE  TO ２併チェックＷ
                  END-IF
               END-IF
      */本家区分はどれか１つに○をする。
               IF 受－保険種別 = 05
                   EVALUATE 受－特別区分
                   WHEN 1
                   WHEN 2
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
      */全ての保険種別で給付割合に○を付ける/130719
      *         IF 受－保険種別 = 01 OR 08
                   EVALUATE レセ－給付割合
                   WHEN 10
                       MOVE NC"○" TO １０割チェックＷ
                   WHEN 9
                       MOVE NC"○" TO ９割チェックＷ
      */中央の前期高齢１割は８割給付に○/110721
                       IF (受－保険種別 NOT = 05 ) AND (受－特別区分 = 1)
                           MOVE SPACE  TO ９割チェックＷ
                           MOVE NC"○" TO ８割チェックＷ
                       END-IF
                   WHEN 8
                       MOVE NC"○" TO ８割チェックＷ
                   WHEN 7
                       MOVE NC"○" TO ７割チェックＷ
                   END-EVALUATE
      *         END-IF
      */元号修正/20190408
               MOVE 受－施術和暦     TO 施術和暦Ｗ
025920         MOVE 受－施術年       TO 施術年Ｗ
025930         MOVE 受－施術月       TO 施術月Ｗ
025940         MOVE 受－患者番号     TO 患者番号Ｗ
025950*         MOVE 受－記号         TO 記号Ｗ
025960*         MOVE 受－番号         TO 番号Ｗ
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
025970         MOVE 受－保険者番号   TO 保険者番号Ｗ
025980         MOVE 受－保険種別     TO 保険種別Ｗ
025990** 全国土木の枝番削除
026000         IF ( 受－保険種別 = 01 ) AND ( 受－保険者番号(1:6) = "133033" )
026010            MOVE 受－保険者番号(1:6)  TO 保険者番号Ｗ
026020         END-IF
026030**
026040         MOVE 受－被保険者カナ TO 被保険者カナＷ
026050         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
026060         MOVE 受－郵便番号１   TO 郵便番号１Ｗ
026070         MOVE 受－郵便番号２   TO 郵便番号２Ｗ
026080         MOVE 受－住所１       TO 被保険者住所１Ｗ
026090         MOVE 受－住所２       TO 被保険者住所２Ｗ
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
026100         MOVE 受－患者カナ     TO 患者カナＷ
026110         MOVE 受－患者氏名     TO 患者氏名Ｗ
026120         MOVE 受－費用負担者番号助成 TO 市町村番号Ｗ
               MOVE 受－受益者番号助成     TO 受給者番号Ｗ
026130*
026140         EVALUATE 受－患者性別
026150         WHEN 1
026160             MOVE NC"男"  TO 性別Ｗ
026170             MOVE NC"○"  TO 男チェックＷ
026180         WHEN 2
026190             MOVE NC"女"  TO 性別Ｗ
026200             MOVE NC"○"  TO 女チェックＷ
026210         END-EVALUATE
026220         EVALUATE 受－患者和暦
026230         WHEN 1
026240             MOVE NC"明治"  TO 元号Ｗ
026250             MOVE NC"○"    TO 明治チェックＷ
026260         WHEN 2
026270             MOVE NC"大正"  TO 元号Ｗ
026280             MOVE NC"○"    TO 大正チェックＷ
026290         WHEN 3
026300             MOVE NC"昭和"  TO 元号Ｗ
026310             MOVE NC"○"    TO 昭和チェックＷ
026320         WHEN 4
026330             MOVE NC"平成"  TO 元号Ｗ
026340             MOVE NC"○"    TO 平成チェックＷ
      */元号修正/20190408
023060         WHEN 5
                   MOVE "5令"   TO 令和ＣＭＷ
023070             MOVE NC"○"  TO 令和チェックＷ
026350         END-EVALUATE
026360*
      */元号修正/↓↓↓20190408
029310         IF 受－患者和暦 > 4
037370             MOVE 受－患者和暦     TO 元－元号区分
037380             READ 元号マスタ
037390             NOT INVALID KEY
037400                 MOVE 元－元号名称 TO 元号Ｗ
037410             END-READ
029330         END-IF
      */元号修正/↑↑↑20190408
026370         MOVE 受－患者年  TO 患者年Ｗ
026380         MOVE 受－患者月  TO 患者月Ｗ
026390         MOVE 受－患者日  TO 患者日Ｗ
026400*
026410* 続柄設定
026420         IF ( 本人家族区分ＷＲ = 1 )
026430            MOVE NC"本人"    TO 続柄Ｗ
026440         ELSE
026450            MOVE 05          TO 名－区分コード
026460            MOVE 受－続柄    TO 名－名称コード
026470            READ 名称マスタ
026480            INVALID KEY
026490                MOVE SPACE    TO 続柄Ｗ
026500            NOT INVALID KEY
026510                MOVE 名－略称 TO 続柄Ｗ
026520            END-READ
026530         END-IF
026540**
026550         IF ( 受－助成種別 NOT = ZERO )
026560            PERFORM 助成レセまとめ判定
026570         ELSE
026580            MOVE SPACE TO 助成レセまとめフラグ
026590         END-IF
      */被爆＋19～の場合本体に公費負担者番号、受給者番号を記載する。
               IF (受－助成種別 = 54) AND (受－費用負担者番号助成(1:2) = "19")
                   MOVE 受－費用負担者番号助成 TO 公費負担者番号 
                   MOVE 受－受益者番号助成     TO 受給者番号
               END-IF
031450* 15/7～　長野　助成
031460         IF 受－施術和暦年月 >= 41507
031470            IF ( 受－助成種別 = 52 OR 53 OR 55 OR 60 ) AND
031480               ( 受－府県助成 = 20 ) AND
031490               ( 受－資格証明区分 NOT = 1 ) AND
031500               ( レセ－一部負担金 NOT = ZERO )
031510*
031520*               MOVE 受－受益者番号助成(1:3)   TO 受給者番号編集Ｗ１
031530*               MOVE 受－受益者番号助成(4:2)   TO 受給者番号編集Ｗ２
031540*               MOVE 受－受益者番号助成(6:10)  TO 受給者番号編集Ｗ３
031550*               MOVE "-"  TO 受給者区切１ 受給者区切２
016580               IF ( 受－費用負担者番号助成(1:2) NOT = "99" )
042910                  MOVE 受－費用負担者番号助成    TO 公費負担者番号 
016620               END-IF
                     IF 受－受益者番号助成(9:7) NOT = SPACE
                        MOVE 受－受益者番号助成(1:8)  TO 受給者番号１
                        MOVE 受－受益者番号助成(9:7)  TO 受給者番号３
                     ELSE
                        MOVE 受－受益者番号助成(1:7)  TO 受給者番号
                     END-IF
031560            END-IF
031570         END-IF
026600**
026610** 14/10～　特別区分コメント印字
026620*         IF ( 受－施術和暦年月 >= 41410 )
026630*             IF ( 受－公費種別 = ZERO )
026640*                EVALUATE 受－特別区分
026650*                WHEN 1
026660*                   MOVE "70才以上 1割"  TO 特別コメントＷ
026670*                WHEN 2
026680*                   MOVE "70才以上 2割"  TO 特別コメントＷ
026681*                WHEN 3
026682*                   MOVE "70才以上 3割"  TO 特別コメントＷ
026690*                WHEN 6
026701*                   IF 受－施術和暦年月 < 42004
026703*                      MOVE "3才未満"        TO 特別コメントＷ
026704*                   ELSE
026705*                      MOVE "義務教育就学前" TO 特別コメントＷ
026706*                   END-IF
026710*                END-EVALUATE
      **/７０歳未満の前期高齢者は特別コメントをクリアする/100929
      *                IF 受－特別区分 = 1 OR 2 OR 3
      *                     MOVE SPACE TO 連年齢２－キー
      *                     INITIALIZE    連年齢２－キー
      *                     MOVE 受－施術和暦 TO 連年齢２－施術和暦
      *                     MOVE 受－施術年   TO 連年齢２－施術年  
      *                     MOVE 受－施術月   TO 連年齢２－施術月  
      *                     MOVE 受－患者和暦 TO 連年齢２－和暦
      *                     MOVE 受－患者年   TO 連年齢２－年
      *                     MOVE 受－患者月   TO 連年齢２－月
      *                     MOVE 受－患者日   TO 連年齢２－日
      *                     CALL   "NENREI2"
      *                     CANCEL "NENREI2"
      *                     IF 連年齢２－老人判定 = ZERO
      *                         MOVE SPACE TO 特別コメントＷ
      *                     END-IF
      *                END-IF
026720*             END-IF
026730*         END-IF
026740*
026750     END-READ.
026760*================================================================*
026770 請求先情報取得 SECTION.
026780*
026790****************************************************
026800* 連結データから保険者マスタより請求先を取得する。 *
026810* ※保－請求先情報区分=1の場合請求先マスタを使用   *
026820* ● 請求先...... 請求先名称Ｗに格納               *
026830****************************************************
026840     MOVE 保険種別ＷＲ   TO 保－保険種別.
026850     MOVE 保険者番号ＷＲ TO 保－保険者番号.
026860     READ 保険者マスタ
026870     INVALID KEY
026880         MOVE SPACE           TO 請求先名称Ｗ
026890     NOT INVALID KEY
026900         MOVE 保－保険者名称  TO 請求先名称Ｗ
026910     END-READ.
026920*
026930** 国保で保険者番号 XX3XXX の時
026940*     IF ( 保険種別ＷＲ = 01 ) AND
026950*        ( 保険者番号ＷＲ(3:1) = "3" )
026960*        MOVE NC"○" TO 国保組合チェックＷ
026970** 以外
026980*     ELSE
026990*        MOVE NC"○" TO 国保チェックＷ
027000*     END-IF.
027010* 
027590*================================================================*
027600 負傷読込 SECTION.
027610*
027790     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
027800     MOVE 施術年ＷＲ         TO 負－施術年.
027810     MOVE 施術月ＷＲ         TO 負－施術月.
027820     MOVE 患者コードＷＲ     TO 負－患者コード.
027830     READ 負傷データＦ
027870     NOT INVALID KEY
027900         MOVE 負－部位数                   TO 部位数Ｗ
           END-READ.
027020*================================================================*
027030 負傷データ取得 SECTION.
027040*
027050**************************************************
027060* 連結データから負傷データＦより以下の情報を取得 *
027070* ● 負傷名...部位＋負傷種別にて加工して格納     *
027080* ● 負傷年.......負傷年Ｗ                       *
027090* ● 負傷月.......負傷月Ｗ                       *
027100* ● 負傷日.......負傷日Ｗ                       *
027110* ● 開始年.......初検年Ｗ                       *
027120* ● 開始月.......初検月Ｗ                       *
027130* ● 開始日.......初検日Ｗ                       *
027140* ● 終了年.......終了年Ｗ                       *
027150* ● 終了月.......終了月Ｗ                       *
027160* ● 終了日.......終了日Ｗ                       *
027170* ● 実日数.......実日数Ｗ                       *
027180* ● 転帰区分 ....区分によりチェックに"○"を格納 *
027190* ● 金属副子 ....区分によりチェックに"○"を格納 *
027200* ● 経過コード...経過マスタより取得             *
027210**************************************************
027220*     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
027230*     MOVE 施術年ＷＲ         TO 負－施術年.
027240*     MOVE 施術月ＷＲ         TO 負－施術月.
027250*     MOVE 患者コードＷＲ     TO 負－患者コード.
027260*     READ 負傷データＦ
027270*     INVALID KEY
027280*         CONTINUE
027290**            /* ありえない */
027300*     NOT INVALID KEY
027310*         MOVE 負－部位数                   TO 部位数Ｗ
027320         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
027330                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR
                             ( 部位ＣＮＴ > 5 )
027340             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別Ｗ(部位ＣＮＴ)
027350             MOVE 負－部位(部位ＣＮＴ)     TO 部位Ｗ(部位ＣＮＴ)
027360             MOVE 負－左右区分(部位ＣＮＴ) TO 左右区分Ｗ(部位ＣＮＴ)
027370             MOVE 負－負傷位置番号(部位ＣＮＴ)
027380                                           TO 負傷位置番号Ｗ(部位ＣＮＴ)
027390*********************************************
027400* 注）全柔...負傷種別＋部位にて加工して格納 *
027410*********************************************
027420* 負傷種別
027430             MOVE SPACE                     TO 負傷名称Ｗ
027440             MOVE 03                        TO 名－区分コード
027450             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
027460             READ 名称マスタ
027470             INVALID KEY
027480                 MOVE SPACE        TO 負傷名称Ｗ
027490             NOT INVALID KEY
027500                 MOVE 名－正式名称 TO 負傷名称Ｗ
027510             END-READ
027520* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
028160*
028170             PERFORM 部位名称埋込処理
027590*
027720             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
027730             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
027740             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
027750             MOVE 負－開始年(部位ＣＮＴ)   TO 初検年Ｗ(部位ＣＮＴ)
027760             MOVE 負－開始月(部位ＣＮＴ)   TO 初検月Ｗ(部位ＣＮＴ)
027770             MOVE 負－開始日(部位ＣＮＴ)   TO 初検日Ｗ(部位ＣＮＴ)
027780             IF ( 負－転帰区分(部位ＣＮＴ) = 9 )
027790                 MOVE 99                   TO 終了年Ｗ(部位ＣＮＴ)
027800                 MOVE 99                   TO 終了月Ｗ(部位ＣＮＴ)
027810                 MOVE 99                   TO 終了日Ｗ(部位ＣＮＴ)
027820             ELSE
027830                 MOVE 負－終了年(部位ＣＮＴ)   TO 終了年Ｗ(部位ＣＮＴ)
027840                 MOVE 負－終了月(部位ＣＮＴ)   TO 終了月Ｗ(部位ＣＮＴ)
027850                 MOVE 負－終了日(部位ＣＮＴ)   TO 終了日Ｗ(部位ＣＮＴ)
027860             END-IF
027870* 経過略称取得
027880             MOVE 01                         TO 経－区分コード
027890             MOVE 負－経過コード(部位ＣＮＴ) TO 経－経過コード
027900             READ 経過マスタ
027910             INVALID KEY
027920                 MOVE ZERO            TO 部位ＣＮＴＷ(部位ＣＮＴ)
027930                 MOVE SPACE           TO 部位区切Ｗ(部位ＣＮＴ)
027940                 MOVE SPACE           TO 経過略称Ｗ(部位ＣＮＴ)
027950             NOT INVALID KEY
027960*
027970                 EVALUATE 部位ＣＮＴ
027980                 WHEN 1
027990                     MOVE NC"①" TO 経過部位Ｗ
028000                 WHEN 2
028010                     MOVE NC"②" TO 経過部位Ｗ
028020                 WHEN 3
028030                     MOVE NC"③" TO 経過部位Ｗ
028040                 WHEN 4
028050                     MOVE NC"④" TO 経過部位Ｗ
028060                 WHEN 5
028070                     MOVE NC"⑤" TO 経過部位Ｗ
028080                 END-EVALUATE
028090                 STRING  経過部位Ｗ     DELIMITED BY SPACE
028100                         経－経過略称   DELIMITED BY SPACE
028110                        INTO 印刷経過略称Ｗ(部位ＣＮＴ)
028120                 END-STRING
028130*
028140             END-READ
028150*
028160             MOVE 負－転帰区分(部位ＣＮＴ) TO 転帰区分Ｗ(部位ＣＮＴ)
028170             EVALUATE 負－転帰区分(部位ＣＮＴ)
028180             WHEN 1
028190             WHEN 2
028200                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
028210             WHEN 3
028220                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
028230             WHEN 4
028240                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
028250             END-EVALUATE
028260*
      */実日数はレセ－部位実日数を転記する/160816
031230             MOVE レセ－部位実日数(部位ＣＮＴ) TO 実日数Ｗ(部位ＣＮＴ)
028270         END-PERFORM.
028280* 新規/継続 チェック
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
033420         END-EVALUATE.
028340*
028350* 枝番判定用
028360         MOVE 負－開始診療日手動区分   TO 開始診療日手動区分Ｗ.
028370* 負傷原因印刷区分
028380         MOVE 負－レセ負傷原因印刷区分 TO レセ負傷原因印刷区分Ｗ.
028370* 長期理由印刷区分
027880         MOVE 負－レセ長期理由印刷区分     TO 長期理由印刷区分Ｆ.
028390*
028400*     END-READ.
028410*================================================================*
028420*================================================================*
028430 施術記録取得 SECTION.
028440*
028450************************************************************
028460* 作１データから負傷データＦより以下の情報を取得           *
028470* ● 初検加算 .....区分によりチェックに"○"を格納...複数可 *
028480* ● 往療加算 .....区分によりチェックに"○"を格納...複数可 *
028490************************************************************
028500     MOVE  SPACE  TO  初日再検フラグ.
028510     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 部位数Ｗ
028520         IF ( 施術年Ｗ = 初検年Ｗ(部位ＣＮＴ) ) AND
028530            ( 施術月Ｗ = 初検月Ｗ(部位ＣＮＴ) )
028540             MOVE 患者番号ＷＲ          TO 施記－患者番号
028550             MOVE 枝番ＷＲ              TO 施記－枝番
028560             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
028570             MOVE 初検年Ｗ(部位ＣＮＴ)  TO 開始年Ｗ(部位ＣＮＴ) 施記－施術年
028580             MOVE 初検月Ｗ(部位ＣＮＴ)  TO 開始月Ｗ(部位ＣＮＴ) 施記－施術月
028590             MOVE 初検日Ｗ(部位ＣＮＴ)  TO 開始日Ｗ(部位ＣＮＴ) 施記－施術日
028600         ELSE
028610             MOVE 患者番号ＷＲ          TO 施記－患者番号
028620             MOVE 枝番ＷＲ              TO 施記－枝番
028630             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
028640             MOVE 施術年ＷＲ            TO 施記－施術年
028650             MOVE 施術月ＷＲ            TO 施記－施術月
028660             MOVE ZERO                  TO 施記－施術日
028670         END-IF
028680         START 施術記録Ｆ   KEY IS >= 施記－患者コード
028690                                      施記－施術和暦年月日
028700         END-START
028710         IF ( 状態キー = "00" )
      */実日数はレセ－部位実日数を転記する/160816
028720*             MOVE ZERO  TO 実日数Ｗ(部位ＣＮＴ)
028730             MOVE ZERO  TO 終了年ＷＴ
028740             MOVE ZERO  TO 終了月ＷＴ
028750             MOVE ZERO  TO 終了日ＷＴ
028760             MOVE SPACE TO 終了フラグ２
028770             PERFORM 施術記録Ｆ読込
028780             IF ( 終了フラグ２      = SPACE   ) AND
028790                ( 施記－患者コード  = 患者コードＷＲ ) AND
028800                ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
028810                ( 施記－施術年      = 施術年ＷＲ     ) AND
028820                ( 施記－施術月      = 施術月ＷＲ     ) 
028830*
028840*        *****************************************************************
028850*        * 開始年月日 ( その部位が当月初検でないか、
028860*                       当月初検でも枝番がある時は、最初の施術日を開始日)*
028870*        *****************************************************************
028880                 IF ( 施術年Ｗ NOT = 初検年Ｗ(部位ＣＮＴ) ) OR
028890                    ( 施術月Ｗ NOT = 初検月Ｗ(部位ＣＮＴ) ) OR
028900                    ( 開始診療日手動区分Ｗ = 1 )
028910                     MOVE 施記－施術年   TO 開始年Ｗ(部位ＣＮＴ)
028920                     MOVE 施記－施術月   TO 開始月Ｗ(部位ＣＮＴ)
028930                     MOVE 施記－施術日   TO 開始日Ｗ(部位ＣＮＴ)
028940                 END-IF
028950             END-IF
028960             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
028970                           ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
028980                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
028990                           ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
029000                           ( 施記－施術月     NOT = 施術月ＷＲ       ) OR
029010                           ( 施記－施術日         > 終了日Ｗ(部位ＣＮＴ))
029020*               **********
029030*               * 実日数 *
029040*               **********
      */実日数はレセ－部位実日数を転記する/160816
029050*                COMPUTE 実日数Ｗ(部位ＣＮＴ) = 実日数Ｗ(部位ＣＮＴ) + 1
029060                MOVE 施記－施術年               TO 終了年ＷＴ
029070                MOVE 施記－施術月               TO 終了月ＷＴ
029080                MOVE 施記－施術日               TO 終了日ＷＴ
029090*
029100                PERFORM 施術記録Ｆ読込
029110            END-PERFORM
029120        END-IF
029130*       **************************
029140*       * 継続：終了年月日セット *
029150*       **************************
029160        IF ( 転帰区分Ｗ(部位ＣＮＴ) = 9 )
029170            MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
029180            MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
029190            MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
029200        END-IF
029210        IF ( 終了年月日Ｗ(部位ＣＮＴ) > 受理年月日Ｗ )
029220            MOVE 終了年Ｗ(部位ＣＮＴ) TO 受理年Ｗ
029230            MOVE 終了月Ｗ(部位ＣＮＴ) TO 受理月Ｗ
029240            MOVE 終了日Ｗ(部位ＣＮＴ) TO 受理日Ｗ
029250        END-IF
029260     END-PERFORM.
029270*
029280** ----- 前月初検のみかを判定 -----------*
029290*
029300*     MOVE 患者番号ＷＲ          TO 施記－患者番号.
029310*     MOVE 枝番ＷＲ              TO 施記－枝番.
029320*     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
029330*     MOVE 施術年ＷＲ            TO 施記－施術年.
029340*     MOVE 施術月ＷＲ            TO 施記－施術月.
029350*     MOVE ZERO                  TO 施記－施術日.
029360*     START 施術記録Ｆ   KEY IS >= 施記－患者コード
029370*                                  施記－施術和暦年月日
029380*     END-START.
029390*     IF ( 状態キー = "00" )
029400*             MOVE SPACE TO 終了フラグ２
029410*             PERFORM 施術記録Ｆ読込
029420*             IF ( 終了フラグ２      = SPACE   ) AND
029430*                ( 施記－患者コード  = 患者コードＷＲ ) AND
029440*                ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
029450*                ( 施記－施術年      = 施術年ＷＲ     ) AND
029460*                ( 施記－施術月      = 施術月ＷＲ     ) 
029470** 当月施術開始日が再検かどうか判定
029480*                 IF ( 施記－再検料請求 = 1 )
029490*                      MOVE "YES"  TO  初日再検フラグ
029500*                 END-IF
029510**
029520*             END-IF
029530*     END-IF.
029540*     IF ( 初日再検フラグ = "YES" )
029550*        PERFORM 前月初検のみ判定
029560*     END-IF.
029570*
029580*================================================================*
029590*================================================================*
029600 レセプト並び順取得 SECTION.
029610*================================================================*
029620     MOVE 施術和暦ＷＲ       TO 作４－施術和暦.
029630     MOVE 施術年ＷＲ         TO 作４－施術年.
029640     MOVE 施術月ＷＲ         TO 作４－施術月.
029650     MOVE 患者コードＷＲ     TO 作４－患者コード.
029660     MOVE 保険種別ＷＲ       TO 作４－保険種別.
029670     READ 作業ファイル４
029680     NOT INVALID KEY
029690          MOVE NC"№"        TO 順番固定Ｗ
029700          MOVE 作４－順番    TO 順番Ｗ
029710     END-READ.
029720*
029730*================================================================*
029740*================================================================*
029750* ＯＣＲ情報取得 SECTION.
029760*
029770****************************************************************
029780* ●     接骨師会会員番号 .....施術所情報マスタより既に取得    *
029790* ●     施術年月         .....受診者情報Ｆより既に取得        *
029800* ● 　　保険種別         .....全柔用国保コード42をセット    　*
029810*                              新規マスタにより対応の可能性有り*
029820* ●     指定各社ＩＤ     .....自社ＩＤ"21"をセット            *
029830* ●     ＦＤ連番         .....資料参照                        *
029840* ● 　　請求金額         .....レセ－請求金額よりセット        *
029850* ●　　 負担割合         .....負担割合 = レセ－負担割合 / 10　*
029860****************************************************************
029870* 接骨師会会員番号
029880*     MOVE ＯＣＲ接骨師会会員番号Ｗ TO ＯＣＲ会員番号Ｗ.
029890* 施術年月
029900*     MOVE 施術年月Ｗ         TO ＯＣＲ施術年月Ｗ.
029910* 保険種別 = 全柔コード = 42
029920*     MOVE 42                 TO ＯＣＲ保険種別Ｗ.
029930* 指定各社ＩＤ = 自社ＩＤ = 21
029940*     MOVE "21"               TO ＯＣＲ各社ＩＤＷ.
029950* ＦＤ連番 = ＦＤ連番患者番号Ｗ + ＦＤ連番健保ＩＤＷ
029960*     MOVE 患者番号Ｗ         TO ＦＤ連番患者番号Ｗ.
029970*     MOVE 1                  TO ＦＤ連番健保ＩＤＷ.
029980* 請求金額
029990*     MOVE レセ－請求金額     TO ＯＣＲ請求金額Ｗ.
030000* 負担割合
030010*     MOVE レセ－負担割合     TO 負担割合ＷＲ.
030020*     COMPUTE ＯＣＲ負担割合Ｗ = 負担割合ＷＲ / 10.
030030*================================================================*
030040 施術記録Ｆ読込 SECTION.
030050*
030060     READ 施術記録Ｆ NEXT
030070     AT END
030080         MOVE "YES" TO 終了フラグ２
030090     END-READ.
030100*================================================================*
030110 印刷処理 SECTION.
030120*
030130     MOVE "YCH6125P" TO  定義体名Ｐ.
030140     MOVE "SCREEN"   TO  項目群名Ｐ.
030150     WRITE YCH6125P.
030160***     WRITE 印刷レコード.
030170     PERFORM エラー処理Ｐ.
030180*================================================================*
030190 エラー処理Ｐ SECTION.
030200*
030210     IF ( 通知情報Ｐ NOT = "00" )
030220         DISPLAY NC"帳票エラー"              UPON CONS
030230         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
030240         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
030250         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
030260         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
030270                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
030280         ACCEPT  キー入力 FROM CONS
030290         PERFORM ファイル閉鎖
030300         MOVE 99 TO PROGRAM-STATUS
030310         EXIT PROGRAM
030320     END-IF.
030330*================================================================*
030340 部位名称埋込処理 SECTION.
030350*
006490     STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
009980            負傷名称Ｗ                    DELIMITED BY SPACE
006500            レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
006520       INTO 負傷名Ｗ(部位ＣＮＴ)
006570     END-STRING.
030550*
030560*================================================================*
030570 初検日以前のデータ判定 SECTION.
030580*
030590*********************************************************************************
030600*  最初の初検日以前の当月中に施術記録レコードがあった時(治癒、中止)は、請求区分の
030610*  継続にもチェックする。(新規と継続の両方)
030620*********************************************************************************
030630** 最初の初検日を取得
030640     MOVE SPACE                 TO 初検フラグ.
030650     MOVE 患者番号ＷＲ          TO 施記－患者番号.
030660     MOVE 枝番ＷＲ              TO 施記－枝番.
030670     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
030680     MOVE 施術年ＷＲ            TO 施記－施術年.
030690     MOVE 施術月ＷＲ            TO 施記－施術月.
030700     MOVE ZERO                  TO 施記－施術日.
030710     START 施術記録Ｆ   KEY IS >= 施記－患者コード
030720                                  施記－施術和暦年月日
030730     END-START.
030740     IF ( 状態キー = "00" )
030750         MOVE ZERO  TO 初検和暦ＷＴ
030760         MOVE ZERO  TO 初検年ＷＴ
030770         MOVE ZERO  TO 初検月ＷＴ
030780         MOVE ZERO  TO 初検日ＷＴ
030790         MOVE SPACE TO 終了フラグ２
030800         PERFORM 施術記録Ｆ読込
030810         PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
030820                       ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
030830                       ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
030840                       ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
030850                       ( 施記－施術月     NOT = 施術月ＷＲ      ) OR
030860                       ( 初検フラグ           = "YES"           ) 
030870               IF ( 施記－診療区分 = 2 )
030880                   MOVE 施記－施術和暦           TO 初検和暦ＷＴ
030890                   MOVE 施記－施術年             TO 初検年ＷＴ
030900                   MOVE 施記－施術月             TO 初検月ＷＴ
030910                   MOVE 施記－施術日             TO 初検日ＷＴ
030920                   MOVE "YES"                    TO 初検フラグ
030930               END-IF
030940               PERFORM 施術記録Ｆ読込
030950         END-PERFORM
030960     END-IF.
030970*
030980* 初検日以前のデータ判定
030990     IF ( 初検フラグ = "YES" )
031000        MOVE 患者番号ＷＲ          TO 施記－患者番号
031010        MOVE 枝番ＷＲ              TO 施記－枝番
031020        MOVE 初検和暦ＷＴ          TO 施記－施術和暦
031030        MOVE 初検年ＷＴ            TO 施記－施術年
031040        MOVE 初検月ＷＴ            TO 施記－施術月
031050        MOVE 初検日ＷＴ            TO 施記－施術日
031060        START 施術記録Ｆ   KEY IS <  施記－患者コード
031070                                     施記－施術和暦年月日
031080                                     REVERSED
031090        END-START
031100        IF ( 状態キー = "00" )
031110           MOVE SPACE  TO 終了フラグ２
031120           PERFORM 施術記録Ｆ読込
031130           IF ( 終了フラグ２    = SPACE        ) AND
031140              ( 施記－患者番号  = 患者番号ＷＲ ) AND
031150              ( 施記－枝番      = 枝番ＷＲ     ) AND
031160              ( 施記－施術和暦  = 初検和暦ＷＴ ) AND
031170              ( 施記－施術年    = 初検年ＷＴ   ) AND
031180              ( 施記－施術月    = 初検月ＷＴ   )
031190*  初検日以前の当月中に施術記録レコードがあった時
031200                IF ( 継続チェックＷ = SPACE )
031210                   MOVE NC"○"    TO 継続チェックＷ
031220                END-IF
031230           END-IF
031240         END-IF
031250     END-IF.
031260*
031270*================================================================*
031280 長期判定取得 SECTION.
031290*
031300* ３カ月以上の長期判定は "CHOUKI" を呼ぶ. 
031310     MOVE  SPACE TO  連期間－キー.
031320     INITIALIZE      連期間－キー.
031330     MOVE 施術和暦ＷＲ  TO  連期間－施術和暦.
031340     MOVE 施術年ＷＲ    TO  連期間－施術年.
031350     MOVE 施術月ＷＲ    TO  連期間－施術月.
031360     MOVE 患者番号ＷＲ  TO  連期間－患者番号.
031370     MOVE 枝番ＷＲ      TO  連期間－枝番.
031380*
031390     CALL   "CHOUKI".
031400     CANCEL "CHOUKI".
031410*
031420**** 適用１を使用 (「前月初検のみ」がある時は、くっつける)
031430     IF ( 連期間－対象フラグ  = "YES" )
031440        IF ( 適用１Ｗ  = SPACE )
031450           MOVE NC"※長期施術継続理由裏面に記載"  TO 適用１Ｗ
031460        ELSE
031470           STRING 適用１Ｗ           DELIMITED BY SPACE
031480                  NC"，"             DELIMITED BY SIZE
031490                  NC"※長期施術継続理由裏面に記載"   DELIMITED BY SIZE
031500                  INTO 適用１Ｗ
031510           END-STRING
031520        END-IF
031530     END-IF.
031540*
031550*================================================================*
031560 初検加算時刻取得 SECTION.
031570*****************************************************************
031580** 初検加算が時間外と深夜の時、適用に「受付時間」を印字する。
031590**   時刻の印字は月3回まで可能
031600*****************************************************************
031610     IF ( レセ－時間外 = 1 ) OR ( レセ－深夜 = 1 ) OR ( レセ－休日 = 1 )
031620*
031630         MOVE 患者番号ＷＲ          TO 施記－患者番号
031640         MOVE 枝番ＷＲ              TO 施記－枝番
031650         MOVE 施術和暦ＷＲ          TO 施記－施術和暦
031660         MOVE 施術年ＷＲ            TO 施記－施術年
031670         MOVE 施術月ＷＲ            TO 施記－施術月
031680         MOVE ZERO                  TO 施記－施術日
031690         START 施術記録Ｆ   KEY IS >= 施記－患者コード
031700                                      施記－施術和暦年月日
031710         END-START
031720         IF ( 状態キー = "00" )
031730             MOVE ZERO  TO 初検加算カウント
031740             MOVE SPACE TO 終了フラグ２
032630             PERFORM 施術記録Ｆ読込
031750             PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
031760                           ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
031770                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
031780                           ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
031790                           ( 施記－施術月     NOT = 施術月ＷＲ      ) 
031800               IF ( 施記－初検加算 = 1 OR 2 OR 3 ) AND ( 施記－診療区分 = 2 )
031810                  COMPUTE 初検加算カウント = 初検加算カウント  + 1
031820                  IF ( 初検加算カウント <= 3 )
031830                     MOVE 施記－初検加算 TO 初検加算区分ＷＴ(初検加算カウント)
031840                     MOVE 施記－受付時   TO 初検加算時ＷＴ(初検加算カウント)
031850                     MOVE 施記－受付分   TO 初検加算分ＷＴ(初検加算カウント)
031860                  END-IF
031870               END-IF
031880               PERFORM 施術記録Ｆ読込
031890            END-PERFORM
031900** 初検加算の時刻を適用にセット
033380            IF ( 初検加算時ＷＴ(1) NOT = ZERO ) OR ( 初検加算分ＷＴ(1) NOT = ZERO ) 
                      MOVE 初検加算時ＷＴ(1) TO 初検加算時Ｗ
                      MOVE ":"               TO 初検加算区切Ｗ
                      MOVE 初検加算分ＷＴ(1) TO 初検加算分Ｗ
                  END-IF
033380            IF ( 初検加算時ＷＴ(2) NOT = ZERO ) OR ( 初検加算分ＷＴ(2) NOT = ZERO ) 
031910                PERFORM 初検加算適用セット
                  END-IF
031920         END-IF
031930*
031940     END-IF.
031950*
031960*================================================================*
031970 初検加算適用セット SECTION.
031980*
031990     PERFORM VARYING 番号カウンタ FROM 1 BY 1
032000              UNTIL  番号カウンタ > 3
032010         IF ( 初検加算時ＷＴ(番号カウンタ)  = ZERO )  AND 
032020            ( 初検加算分ＷＴ(番号カウンタ)  = ZERO ) 
032030             CONTINUE
032040         ELSE
032050* 固定項目
032060             EVALUATE 初検加算区分ＷＴ(番号カウンタ) 
032070             WHEN 1
032080                MOVE NC"時間外"   TO 加算内容Ｗ(番号カウンタ)
033320             WHEN 2
033330                MOVE NC"休　日"   TO 加算内容Ｗ(番号カウンタ)
032090             WHEN 3
032100                MOVE NC"深　夜"   TO 加算内容Ｗ(番号カウンタ)
032110             END-EVALUATE
032120*
032130             MOVE NC"："          TO 加算区切Ｗ(番号カウンタ)
032140             MOVE NC"時"          TO 時固定Ｗ(番号カウンタ)
032150             MOVE NC"分"          TO 分固定Ｗ(番号カウンタ)
032160*
032170**** 数字→日本語変換
032180* 時間
032190             MOVE 初検加算時ＷＴ(番号カウンタ)  TO  数字Ｗ
032200             IF ( 数字Ｗ >= 10 )
032210                 MOVE 数字Ｗ１    TO 負傷番号Ｗ１
032220                 PERFORM 日本語変換
032230                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ１(番号カウンタ)
032240                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
032250                 PERFORM 日本語変換
032260                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
032270             ELSE
032280                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
032290                 PERFORM 日本語変換
032300                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
032310             END-IF
032320* 分
032330             MOVE 初検加算分ＷＴ(番号カウンタ)  TO  数字Ｗ
032340             MOVE 数字Ｗ１    TO 負傷番号Ｗ１
032350             PERFORM 日本語変換
032360             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ１(番号カウンタ)
032370             MOVE 数字Ｗ２    TO 負傷番号Ｗ１
032380             PERFORM 日本語変換
032390             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ２(番号カウンタ)
032400** 
032410        END-IF
032420     END-PERFORM.
032430*
032440     MOVE  初検加算集団ＮＷ(1)   TO 初検加算時刻１Ｗ. 
032450     MOVE  初検加算集団ＮＷ(2)   TO 初検加算時刻２Ｗ. 
032460     MOVE  初検加算集団ＮＷ(3)   TO 初検加算時刻３Ｗ. 
032470*
032480**** 適用１か２を使用（長期理由記載で適用１を使っている時は、適用２）
032490     IF ( 初検加算時ＷＴ(2)  = ZERO ) AND ( 初検加算分ＷＴ(2)  = ZERO ) 
032500         CONTINUE
032510     ELSE
032520         IF ( 適用１Ｗ  = SPACE )
032530               STRING NC"初検加算"       DELIMITED BY SIZE
032540                      初検加算時刻１Ｗ   DELIMITED BY SIZE
032550                      初検加算時刻２Ｗ   DELIMITED BY SIZE
032560                      初検加算時刻３Ｗ   DELIMITED BY SIZE
032570                      INTO 適用１Ｗ
032580               END-STRING
032590         ELSE
032600               STRING NC"初検加算"       DELIMITED BY SIZE
032610                      初検加算時刻１Ｗ   DELIMITED BY SIZE
032620                      初検加算時刻２Ｗ   DELIMITED BY SIZE
032630                      初検加算時刻３Ｗ   DELIMITED BY SIZE
032640                      INTO 適用２Ｗ
032650               END-STRING
032660         END-IF
032670     END-IF.
032680*
032690*================================================================*
032700 日本語変換 SECTION.
032710*
032720     MOVE NC"０"     TO 全角負傷番号Ｗ.
032730     CALL "htoz" WITH C LINKAGE
032740                        USING 負傷番号Ｗ１ 全角負傷番号Ｗ１.
032750*
032760*================================================================*
032770*================================================================*
032780 負傷原因取得 SECTION.
032790*
032800********************************************************************
032810*  負傷原因コードが同じものは、1行にまとめて印字する。
032820*  例: ①② 家で転んだ.
032830*     負傷原因コードが同じものをまとめ、テーブルにセット
032840*     (ただし、部位を飛んで同じものは、2行になる)
032850********************************************************************
032860     MOVE  ZERO   TO  カウンタ カウンタ２.
032870     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
032880             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
032890*
032900****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
032910        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
032920*
032930           IF ( カウンタ = ZERO )
032940               MOVE 1   TO  カウンタ カウンタ２
032950               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
032960               MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
032970               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
032980           ELSE
032990              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
033000                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
033010                 COMPUTE カウンタ２ = カウンタ２  +  1
033020                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
033030              ELSE
033040                 COMPUTE カウンタ = カウンタ  +  1
033050                 MOVE 1   TO  カウンタ２
033060                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
033070                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
033080                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
033090              END-IF
033100           END-IF
033110        END-IF
033120     END-PERFORM.
033130**************************************************************************
033140*  負傷原因マスタより文章取得
033150**************************************************************************
033160     MOVE  ZERO   TO  カウンタ カウンタ２.
033170     PERFORM VARYING カウンタ FROM 1 BY 1
033180             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
033190** 健保は 区分 01
033200         MOVE 01                        TO 負原－区分コード
033210         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
033220         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
033230         READ 負傷原因Ｆ
033240         NOT INVALID KEY
033250             INITIALIZE 負傷原因ＷＴ
033260             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
033270             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
033280             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
033290             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
033300             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
033310             PERFORM VARYING カウンタ２ FROM 1 BY 1
033320                     UNTIL ( カウンタ２ > 9 )  OR 
033330                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
033340                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
033350                WHEN 1
033360                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
033370                WHEN 2
033380                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
033390                WHEN 3
033400                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
033410                WHEN 4
033420                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
033430                WHEN 5
033440                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
033410                WHEN 6
033420                   MOVE "⑥"  TO  負傷原因ナンバーＷ１(カウンタ２)
033430                WHEN 7
033440                   MOVE "⑦"  TO  負傷原因ナンバーＷ１(カウンタ２)
033450                WHEN OTHER
033460                   CONTINUE
033470                END-EVALUATE
033480             END-PERFORM
033490*
033572             IF 負原－負傷原因入力区分 = 1
033573                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
033574                        負傷原因１ＷＴ  DELIMITED BY SIZE
033575                        負傷原因２ＷＴ  DELIMITED BY SIZE
033576                        負傷原因３ＷＴ  DELIMITED BY SIZE
033577                        負傷原因４ＷＴ  DELIMITED BY SIZE
033578                        負傷原因５ＷＴ  DELIMITED BY SIZE
033579                        INTO 負傷原因内容合成Ｗ(カウンタ)
033580                 END-STRING
033581             ELSE
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
033590             END-IF
033591*
033592         END-READ
033593     END-PERFORM.
033600*
033610     PERFORM 負傷原因セット.
033620*
033630*================================================================*
033640 負傷原因セット SECTION.
033650*
033660**************************************************************************
033670*  文章が1行を超える時は、複数行に分解する。
033680**************************************************************************
033690     MOVE  ZERO   TO  カウンタ カウンタ２.
033700     PERFORM VARYING カウンタ FROM 1 BY 1
033710             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
033720*
033730          INITIALIZE 負傷原因内容分解ＸＷ
033740          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
033750          IF ( 負傷原因内容１ＸＷ  NOT = SPACE )
033760              COMPUTE カウンタ２ = カウンタ２  +  1
033770              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
033780          END-IF
033790          IF ( 負傷原因内容２ＸＷ  NOT = SPACE )
033800              COMPUTE カウンタ２ = カウンタ２  +  1
033810              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
033820          END-IF
033830          IF ( 負傷原因内容３ＸＷ  NOT = SPACE )
033840              COMPUTE カウンタ２ = カウンタ２  +  1
033850              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
033860          END-IF
033830          IF ( 負傷原因内容４ＸＷ  NOT = SPACE )
033840              COMPUTE カウンタ２ = カウンタ２  +  1
033850              MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
033860          END-IF
033870*
033880     END-PERFORM.
033890*================================================================*
033900 助成印取得 SECTION.
033910*
033911* 2006/04 変更
033912* 助成印は "JOSEIMEI" を呼ぶ. 
033913     MOVE SPACE TO  連助成名称－キー.
033914     INITIALIZE     連助成名称－キー.
033915     MOVE 助成種別ＷＲ           TO 連助成名称－助成種別.
033916     MOVE 費用負担者番号助成ＷＲ TO 連助成名称－費用負担者番号助成.
033917*
033918     CALL   "JOSEIMEI".
033919     CANCEL "JOSEIMEI".
033920*
033921     MOVE 連助成名称－１文字 TO 助成印Ｗ.
033922*
033923***
034290*
034300*================================================================*
034310 前月初検のみ判定 SECTION.
034320*
034330*** 前月の通院日が初検か判定 
034340     MOVE  SPACE            TO 前月フラグ.
034350     MOVE 受－患者コード    TO 施記－患者コード.
034360     MOVE 受－施術和暦      TO 施記－施術和暦.
034370     MOVE 受－施術年        TO 施記－施術年.
034380     MOVE 受－施術月        TO 施記－施術月.
034390     MOVE 1                 TO 施記－施術日.
034400     START 施術記録Ｆ   KEY IS <  施記－患者コード
034410                                  施記－施術和暦年月日
034420                                  REVERSED
034430     END-START.
034440     IF ( 状態キー = "00" )
034450         MOVE SPACE  TO 終了フラグ２
034460         PERFORM 施術記録Ｆ読込
034470         IF ( 終了フラグ２      = SPACE  ) AND
034480            ( 施記－患者コード  = 受－患者コード ) AND
034490            ( 施記－診療区分    = 2 ) 
034500*
034510            PERFORM 前月判定
034520**** 適用１を使用
034530            IF ( 前月フラグ = "YES" )
034540               MOVE NC"※前月初検のみ"    TO  適用１Ｗ
034550            END-IF
034560**
034570         END-IF
034580     END-IF.
034590*
034600*================================================================*
034610 前月判定  SECTION.
034620* 
034630*** 読み込んだ施術記録の年月が、前月かどうか判定 (年月の差が 1 か?)
034640      MOVE  SPACE  TO  前月フラグ.
034650      INITIALIZE  計算年月日Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
034660**
034670      MOVE 受－施術和暦    TO 終了和暦２Ｗ.
034680      MOVE 受－施術年      TO 終了年２Ｗ.
034690      MOVE 受－施術月      TO 終了月２Ｗ.
034700      MOVE 施記－施術和暦  TO 開始和暦２Ｗ.
034710      MOVE 施記－施術年    TO 開始年２Ｗ.
034720      MOVE 施記－施術月    TO 開始月２Ｗ.
034730*
034740      EVALUATE TRUE
034750       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ = 終了年２Ｗ)
034760            PERFORM  前月比較月
034770       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ NOT = 終了年２Ｗ)
034780            PERFORM  前月比較年
034790       WHEN  開始和暦２Ｗ NOT = 終了和暦２Ｗ 
034800            PERFORM  前月比較元号
034810      END-EVALUATE.
034820*
034830      IF ( 計算月Ｗ = 1 )
034840         MOVE  "YES"  TO  前月フラグ
034850      END-IF.
034860*
034870*================================================================*
034880 前月比較月  SECTION.
034890*
034900     IF ( 終了月２Ｗ >  開始月２Ｗ )
034910         COMPUTE 計算月Ｗ = 終了月２Ｗ - 開始月２Ｗ
034920     ELSE
034930        MOVE ZERO TO 計算月Ｗ
034940     END-IF.
034950*
034960*================================================================*
034970 前月比較年  SECTION.
034980*
034990     IF ( 終了年２Ｗ >  開始年２Ｗ )
035000         COMPUTE 計算年Ｗ = 終了年２Ｗ - 開始年２Ｗ
035010         COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
035020     ELSE
035030        MOVE ZERO TO 計算月Ｗ
035040     END-IF.
035050*
035060*================================================================*
035070 前月比較元号  SECTION.
035080*
035090     MOVE 開始和暦２Ｗ TO 元－元号区分.
035100     READ 元号マスタ
035110     NOT INVALID KEY
035120         MOVE 元－開始西暦年 TO 開始西暦年Ｗ
035130     END-READ.
035140     MOVE 終了和暦２Ｗ TO 元－元号区分.
035150     READ 元号マスタ
035160     NOT INVALID KEY
035170         MOVE 元－開始西暦年 TO 終了西暦年Ｗ
035180     END-READ.
035190**
035200     IF ( 開始西暦年Ｗ NOT = ZERO ) AND ( 終了西暦年Ｗ NOT = ZERO )
035210        COMPUTE 開始西暦年Ｗ = 開始西暦年Ｗ + 開始年２Ｗ - 1
035220        COMPUTE 終了西暦年Ｗ = 終了西暦年Ｗ + 終了年２Ｗ - 1
035230*
035240        IF ( 終了西暦年Ｗ =  開始西暦年Ｗ )
035250           PERFORM  前月比較月
035260        ELSE
035270           IF ( 終了西暦年Ｗ >  開始西暦年Ｗ )
035280               COMPUTE 計算年Ｗ = 終了西暦年Ｗ - 開始西暦年Ｗ
035290               COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
035300           ELSE
035310               MOVE ZERO TO 計算月Ｗ
035320           END-IF
035330        END-IF
035340     ELSE
035350        MOVE ZERO TO 計算月Ｗ
035360     END-IF.
035540*================================================================*
035550 受診者印刷区分更新 SECTION.
035560*
035570** //  受診者情報Ｆの印刷区分に１をセットし、更新する。//  
035580*
035590     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
035600     MOVE 施術年ＷＲ         TO 受－施術年.
035610     MOVE 施術月ＷＲ         TO 受－施術月.
035620     MOVE 患者コードＷＲ     TO 受－患者コード.
035630     READ 受診者情報Ｆ
035640     NOT INVALID KEY
035650         MOVE  1  TO  受－レセ印刷区分
035660         REWRITE  受－レコード
035670         END-REWRITE
035680         IF ( 状態キー NOT = "00" )
035690            MOVE NC"受診者" TO ファイル名
035700            PERFORM エラー表示
035710         END-IF
035720     END-READ.
035730*
035740*================================================================*
035750 月末日取得 SECTION.
035760*
035770     MOVE 施術年ＷＲ   TO 受理年Ｗ.
035780     MOVE 施術月ＷＲ   TO 受理月Ｗ.
035790     MOVE 施術和暦ＷＲ TO 元－元号区分.
035800     READ 元号マスタ
035810     NOT INVALID KEY
035820         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
035830     END-READ.
035840     IF ( 施術西暦年Ｗ NOT = ZERO )
035850        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
035860     END-IF.
035870*
035880     EVALUATE 施術月ＷＲ
035890     WHEN 4
035900     WHEN 6
035910     WHEN 9
035920     WHEN 11
035930         MOVE 30 TO 受理日Ｗ
035940     WHEN 2
035950         DIVIDE 4 INTO 施術西暦年Ｗ GIVING    商Ｗ
035960                                    REMAINDER 余Ｗ
035970         END-DIVIDE
035980         IF ( 余Ｗ = ZERO )
035990             MOVE 29 TO 受理日Ｗ
036000         ELSE
036010             MOVE 28 TO 受理日Ｗ
036020         END-IF
036030     WHEN 1
036040     WHEN 3
036050     WHEN 5
036060     WHEN 7
036070     WHEN 8
036080     WHEN 10
036090     WHEN 12
036100         MOVE 31 TO 受理日Ｗ
036110     WHEN OTHER
036120          CONTINUE
036130     END-EVALUATE.
036140*
036150*================================================================*
036160 委任年月日取得 SECTION.
036170*
036180** ---// ここの受理年には、最終通院日が入っている為、退避する //----
036190     MOVE 受理年Ｗ   TO 最終通院年Ｗ.
036200     MOVE 受理月Ｗ   TO 最終通院月Ｗ.
036210     MOVE 受理日Ｗ   TO 最終通院日Ｗ.
036220***
036230* (柔整師側)
036240     EVALUATE レセプト日付区分Ｗ 
036250*    /  最終通院日 /
036260     WHEN ZERO
036270         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
036280         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
036290         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
036300*    /  月末日 /
036310     WHEN 1 
036320         PERFORM 月末日取得
036330         MOVE 受理年Ｗ     TO 柔整師年Ｗ
036340         MOVE 受理月Ｗ     TO 柔整師月Ｗ
036350         MOVE 受理日Ｗ     TO 柔整師日Ｗ
036360*    /  印字なし /
036370     WHEN 9
036380         MOVE ZERO         TO 柔整師年Ｗ
036390         MOVE ZERO         TO 柔整師月Ｗ
036400         MOVE ZERO         TO 柔整師日Ｗ
036410*    /  その他は、最終通院日 /
036420     WHEN OTHER
036430         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
036440         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
036450         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
036460     END-EVALUATE.
036470**
036480* (患者側)
036490     EVALUATE レセプト患者日付区分Ｗ 
036500*    /  最終通院日 /
036510     WHEN ZERO
036520         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
036530         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
036540         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
036550*    /  月末日 /
036560     WHEN 1 
036570         PERFORM 月末日取得
036580         MOVE 受理年Ｗ     TO 患者委任年Ｗ
036590         MOVE 受理月Ｗ     TO 患者委任月Ｗ
036600         MOVE 受理日Ｗ     TO 患者委任日Ｗ
036610*    /  印字なし /
036620     WHEN 9
036630         MOVE ZERO         TO 患者委任年Ｗ
036640         MOVE ZERO         TO 患者委任月Ｗ
036650         MOVE ZERO         TO 患者委任日Ｗ
036660*    /  その他は、最終通院日 /
036670     WHEN OTHER
036680         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
036690         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
036700         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
036710     END-EVALUATE.
036720*
036730*================================================================*
036731*================================================================*
036732 助成レセまとめ判定 SECTION.
036733*---------------------------------------------------------------------------*
036734* 市町村マスタを読み、レセまとめ区分＝１でかつ、本体保険が国保・退職
036735* の時は、フラグYES (金額を助成込みで印字）
036736*（例：横浜市の障害は、本体保険（国保系）のレセプト１枚で請求、助成レセはなし）
036737*---------------------------------------------------------------------------*
036738*
036739     MOVE SPACE TO 助成レセまとめフラグ.
036740*     MOVE SPACE TO 助成種別略称Ｗ.
036741*
036742*     MOVE 受－助成種別           TO 市－公費種別.
036743*     MOVE 受－費用負担者番号助成 TO 市－市町村番号.
036744*     READ 市町村マスタ
036745*     NOT INVALID KEY
036746*         IF 市－レセまとめ区分 = 1 
036747*            IF (( 受－保険種別 = 01 ) AND ( 受－保険者番号(3:1) NOT = "3" )) OR
036748*               ( 受－保険種別 = 08 ) 
036749*                MOVE "YES" TO 助成レセまとめフラグ
036750*
036751*                MOVE 02            TO 名－区分コード
036752*                MOVE 受－助成種別  TO 名－名称コード
036753*                READ 名称マスタ
036754*                NOT INVALID KEY
036755*                    MOVE 名－略称  TO 助成種別略称Ｗ
036756*                END-READ
036757*            END-IF
036758*         END-IF
036759*     END-READ.
036760*
036761** / CALL JRECEOFF /
036762*     IF 助成レセまとめフラグ = SPACE
036763*        INITIALIZE 連レセまとめ－キー
036764*        MOVE 施術和暦ＷＲ TO 連レセまとめ－施術和暦
036765*        MOVE 施術年ＷＲ   TO 連レセまとめ－施術年
036766*        MOVE 施術月ＷＲ   TO 連レセまとめ－施術月
036767*        MOVE 患者番号ＷＲ TO 連レセまとめ－患者番号
036768*        MOVE 枝番ＷＲ     TO 連レセまとめ－枝番
036769*       1:助成レセプトなしの本体まとめの判定
036770*        MOVE 1            TO 連レセまとめ－判定区分
036771*        CALL   "JRECEOFF"
036772*        CANCEL "JRECEOFF"
036773*
036774*        IF 連レセまとめ－判定結果 = 1
           IF ( レセ－本体まとめ区分 = 1 )
036775           MOVE "YES" TO 助成レセまとめフラグ
036776*        END-IF
036777     END-IF.
036778*
036779*----------------------------------------------------------------------*
036780** / 神奈川県固有：摘要に負担者番号と受給者番号 /
036781     IF ( 助成レセまとめフラグ = "YES" ) AND
036782        ( 受－費用負担者番号助成(3:2) = "14" )
036783        IF 受－費用負担者番号助成(1:2) NOT = "99"
036784*            MOVE ALL NC"￣"    TO 横線１ 横線２ 横線３
036785*            MOVE ALL NC"｜"    TO 縦線１ 縦線２
036786*            MOVE NC"｜"                    TO 縦線３ 縦線４
036787*            MOVE NC"公費負担者番号"        TO 神奈川固定１
036788*            MOVE NC"受給者番号"            TO 神奈川固定２
036789*            MOVE NC"／"                    TO 神奈川固定３
036790            MOVE 受－費用負担者番号助成    TO 公費負担者番号 
036791            MOVE 受－受益者番号助成        TO 受給者番号
036792        END-IF
036793     END-IF.
      */和歌山県障害乳幼児ひとり親/100518
037769     IF ( 助成レセまとめフラグ = "YES" ) AND
037770        ( 受－費用負担者番号助成(3:2) = "30" )
037772        IF 受－費用負担者番号助成(1:2) NOT = "99"
037773*            MOVE ALL NC"￣"    TO 横線１ 横線２ 横線３
037774*            MOVE ALL NC"｜"    TO 縦線１ 縦線２
037775*            MOVE NC"｜"                    TO 縦線３ 縦線４
037776*            MOVE NC"公費負担者番号"        TO 神奈川固定１
037777*            MOVE NC"受給者番号"            TO 神奈川固定２
037778*            MOVE NC"／"                    TO 神奈川固定３
037781            MOVE 受－費用負担者番号助成    TO 公費負担者番号 
037782            MOVE 受－受益者番号助成        TO 受給者番号
036792        END-IF
           END-IF.
      */滋賀県/110806
037769     IF ( 助成レセまとめフラグ = "YES" ) AND
037770        ( 受－費用負担者番号助成(3:2) = "25" )
037772        IF 受－費用負担者番号助成(1:2) NOT = "99"
037781            MOVE 受－費用負担者番号助成    TO 公費負担者番号 
037782            MOVE 受－受益者番号助成        TO 受給者番号
036792        END-IF
           END-IF.
036794*
037095*================================================================*
037096*================================================================*
037097 助成料金計算 SECTION.
           MOVE 3            TO レセ－レセ種別
019550     MOVE 受－施術和暦 TO レセ－施術和暦.
019560     MOVE 受－施術年   TO レセ－施術年.
019570     MOVE 受－施術月   TO レセ－施術月.
019580     MOVE 受－患者番号 TO レセ－患者番号.
019590     MOVE 受－枝番     TO レセ－枝番.
019600     READ レセプトＦ
019630     INVALID KEY
              MOVE SPACE     TO レセ－レコード
              INITIALIZE        レセ－レコード
           END-READ.
037290*
037213*================================================================*
037214 レセ摘要再セット SECTION.
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
037320*
040580*================================================================*
040590 摘要文取得 SECTION.
040600*
040610* 摘要文取得は "TEKIYBUN" を呼ぶ. 
040620     MOVE  SPACE TO  連摘文－キー.
040630     INITIALIZE      連摘文－キー.
040640     MOVE 施術和暦ＷＲ  TO  連摘文－施術和暦.
040650     MOVE 施術年ＷＲ    TO  連摘文－施術年.
040660     MOVE 施術月ＷＲ    TO  連摘文－施術月.
040670     MOVE 患者番号ＷＲ  TO  連摘文－患者番号.
040680     MOVE 枝番ＷＲ      TO  連摘文－枝番.
040700*     MOVE 63            TO  連摘文－文桁数.
039370     MOVE 56            TO  連摘文－文桁数.
015000     IF (長期理由印刷区分Ｆ NOT = 1 )
               MOVE 長期理由印刷区分Ｗ TO 連摘文－長期区分
           ELSE
               MOVE 1                  TO 連摘文－長期区分
015050     END-IF.
040710*
040720     CALL   "TEKIYBUN".
040730     CANCEL "TEKIYBUN".
040740*
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
037500*================================================================*
037501*================================================================*
037502 地域特有処理 SECTION.
037503*
037504*--------------------------------------------------------*
037505*  福岡県：経過欄の固定印字 (全柔ＦＰＤ区分Ｗ 1 使用)
037506*  長期以外の部位は、「順調」
037507*  長期の部位は、「緩慢」
037508*--------------------------------------------------------*
037509*
037510     IF 全柔ＦＰＤ区分Ｗ = 1
037511*      まず「順調」セット
037512        PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
016870                UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR
016880                      ( 部位ＣＮＴ > 5 )
037514*
037515                 EVALUATE 部位ＣＮＴ
037516                 WHEN 1
037517                     MOVE NC"①" TO 経過部位数字Ｗ
037518                 WHEN 2
037519                     MOVE NC"②" TO 経過部位数字Ｗ
037520                 WHEN 3
037521                     MOVE NC"③" TO 経過部位数字Ｗ
037522                 WHEN 4
037523                     MOVE NC"④" TO 経過部位数字Ｗ
037524                 WHEN 5
037525                     MOVE NC"⑤" TO 経過部位数字Ｗ
037526                 END-EVALUATE
037527                 MOVE SPACE TO 経過略称(部位ＣＮＴ)
037528                 STRING  経過部位数字Ｗ   DELIMITED BY SPACE
037529                         NC"順調"         DELIMITED BY SPACE
037530                        INTO 経過略称(部位ＣＮＴ)
037531                 END-STRING
037532        END-PERFORM
037533*
037534*      次に、３カ月以上の長期判定
037535        MOVE  SPACE TO  連期間－キー
037536        INITIALIZE      連期間－キー
037537        MOVE 施術和暦ＷＲ  TO  連期間－施術和暦
037538        MOVE 施術年ＷＲ    TO  連期間－施術年
037539        MOVE 施術月ＷＲ    TO  連期間－施術月
037540        MOVE 患者番号ＷＲ  TO  連期間－患者番号
037541        MOVE 枝番ＷＲ      TO  連期間－枝番
037542        CALL   "CHOUKI"
037543        CANCEL "CHOUKI"
037544*
037545        IF 連期間－対象フラグ  = "YES"
037546           PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
016870                   UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR
016880                         ( 部位ＣＮＴ > 5 )
037548*
037549               IF 連期間－期間Ｗ(部位ＣＮＴ)  >  ZERO
037550
037551                   EVALUATE 部位ＣＮＴ
037552                   WHEN 1
037553                       MOVE NC"①" TO 経過部位数字Ｗ
037554                   WHEN 2
037555                       MOVE NC"②" TO 経過部位数字Ｗ
037556                   WHEN 3
037557                       MOVE NC"③" TO 経過部位数字Ｗ
037558                   WHEN 4
037559                       MOVE NC"④" TO 経過部位数字Ｗ
037560                   WHEN 5
037561                       MOVE NC"⑤" TO 経過部位数字Ｗ
037562                   END-EVALUATE
037563                   MOVE SPACE TO 経過略称(部位ＣＮＴ)
037564                   STRING  経過部位数字Ｗ   DELIMITED BY SPACE
037565                           NC"緩慢"         DELIMITED BY SPACE
037566                          INTO 経過略称(部位ＣＮＴ)
037567                   END-STRING
037568               END-IF
037569           END-PERFORM
037570        END-IF
037571*
037572     END-IF.
037573*
      */長野の国保・退職・後高の時で経過が入力されてない時は、経過を入れる/160610
           IF ((保険種別ＷＲ = 01) AND (保険者番号ＷＲ(1:2) = "20")) OR
              ((保険種別ＷＲ = 08) AND (保険者番号ＷＲ(3:2) = "20")) OR
              ((公費種別ＷＲ = 05) AND (費用負担者番号ＷＲ(3:2) = "20"))
016020        PERFORM 経過取得
           END-IF.
018770     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018790             UNTIL ( 部位ＣＮＴ > 5 )
018820         MOVE 印刷経過略称Ｗ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
018830     END-PERFORM.
037574*
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
040910                    MOVE NC"やや良好" TO  経過ＣＭ
040920                 ELSE
040930                    MOVE NC"良好"     TO  経過ＣＭ
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
037575*================================================================*
037576*================================================================*
037577*================================================================*
037578 エラー表示 SECTION.
037579*
037580     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
037581     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
037582     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
037583     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
037590                                                   UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
037600     ACCEPT  キー入力 FROM CONS
037610     PERFORM ファイル閉鎖.
037620     EXIT PROGRAM.
037630*================================================================*
037640*================================================================*
037650 ファイル閉鎖 SECTION.
037660*
037670     CLOSE 印刷ファイル.
037680     CLOSE 保険者マスタ     元号マスタ          名称マスタ
037690           レセプトＦ       制御情報マスタ      施術所情報マスタ
037700           請求先マスタ     経過マスタ          受診者情報Ｆ
037710           施術記録Ｆ       負傷データＦ        負傷原因Ｆ
037720           会情報マスタ     ＩＤ管理マスタ      市町村マスタ
037730           作業ファイル４   メモファイル.
037740*================================================================*
037750 終了処理 SECTION.
037760*
037770     PERFORM ファイル閉鎖.
037780*================================================================*
037790*================================================================*
037800 テスト印字処理 SECTION.
037810*
           MOVE ALL "9" TO
           施術月 施術年 都道府県番号 開始年１ 開始月１ 開始日１ 終了年１ 終了月１ 終了日１
           負傷年１ 負傷月１ 負傷日１ 初検年１ 初検月１ 初検日１ 実日数１ 開始年２ 開始月２
           開始日２ 終了年２ 終了月２ 終了日２ 負傷年２ 負傷月２ 負傷日２ 初検年２ 初検月２
           初検日２ 実日数２ 開始年３ 開始月３ 開始日３ 終了年３ 終了月３ 終了日３ 負傷年３
           負傷月３ 負傷日３ 初検年３ 初検月３ 初検日３ 実日数３ 開始年４ 開始月４ 開始日４ 
           終了年４ 終了月４ 終了日４ 負傷年４ 負傷月４ 負傷日４ 初検年４ 初検月４ 初検日４ 
           実日数４ 開始年５ 開始月５ 開始日５ 終了年５ 終了月５ 終了日５ 負傷年５ 負傷月５ 
           負傷日５ 初検年５ 初検月５ 初検日５ 実日数５ 初検料 初検時相談料 往療距離 再検料 
           金属副子加算料 往療回数 往療料 小計 初検加算料 施術情報提供料 往療加算料 初検加算時 
           初検加算分 初回処置料(1) 初回処置料(2) 初回処置料(3) 初回処置料(4) 初回処置料(5) 
           初回処置料合計 後療単価１ 
      *     冷罨法単価 温罨法単価 電療単価 
           後療回数１ 後療料１ 冷罨法回数１ 冷罨法料１ 温罨法回数１ 温罨法料１ 電療回数１ 
           電療料１ 小計１ 長期逓減率１ 長期込小計１ 後療単価２ 後療回数２ 後療料２ 冷罨法回数２ 
           冷罨法料２ 温罨法回数２ 温罨法料２ 電療回数２ 電療料２ 小計２ 長期逓減率２ 長期込小計２
           後療単価３８ 後療回数３８ 後療料３８ 冷罨法回数３８ 冷罨法料３８ 温罨法回数３８ 
           温罨法料３８ 電療回数３８ 電療料３８ 小計３８ 多部位込小計３８ 長期逓減率３８ 
           長期込小計３８ 逓減開始月３０ 逓減開始日３０ 後療単価３０ 後療回数３０ 後療料３０ 
           冷罨法回数３０ 冷罨法料３０ 温罨法回数３０ 温罨法料３０ 電療回数３０ 電療料３０ 
           小計３０ 長期逓減率３０ 長期込小計３０ 逓減開始月４８ 逓減開始日４８ 後療単価４８ 
           後療回数４８ 後療料４８ 冷罨法回数４８ 冷罨法料４８ 温罨法回数４８ 温罨法料４８ 
           電療回数４８ 電療料４８ 小計４８ 多部位込小計４８ 長期逓減率４８ 長期込小計４８ 
           逓減開始月４０ 逓減開始日４０ 後療単価４０ 後療回数４０ 後療料４０ 冷罨法回数４０ 
           冷罨法料４０ 温罨法回数４０ 温罨法料４０ 電療回数４０ 電療料４０ 小計４０ 
           長期逓減率４０ 長期込小計４０ 合計 一部負担金 負担割合 請求金額 受理年 受理月 受理日 
           委任年 委任月 委任日
           .
      *
           MOVE ALL "X" TO 
           公費負担者番号 受給者番号 県施術ＩＤ 記号番号 住所１ 住所２
      *      郵便番号１ 郵便番号２ 郵便番号区切 
           口座名義人カナ 口座名義人 柔整師番号 口座番号 保険者名称 
           会長委任コメント１ 会長委任コメント２ 会長委任コメント３ 施術所郵便番号１  
           施術所郵便番号２ 施術所住所１ 施術所住所２ 施術所電話番号 代表者カナ 接骨師会会員番号
           .
      *
           MOVE ALL NC"Ｎ" TO
           負傷名１ 負傷名２ 負傷名３ 負傷名４ 負傷名５ 経過略称(1) 
           経過略称(2) 経過略称(3) 経過略称(4) 経過略称(5) 適用１ 適用２ 部位５適用 
           .
      *
           MOVE ALL "Ｎ" TO
           被保険者氏名 患者氏名 代表者名 接骨院名
           長期理由文１ 長期理由文２ 長期理由文３ 長期理由文４ 長期理由文５ 長期理由文６
           .
      *
           MOVE NC"○" TO
           普通チェック 振込チェック 当座チェック 本店チェック 支店チェック 本支所チェック 
           銀行チェック 金庫チェック 農協チェック 施術日チェック１ 施術日チェック２ 
           施術日チェック３ 施術日チェック４ 施術日チェック５ 施術日チェック６ 施術日チェック７ 
           施術日チェック８ 施術日チェック９ 施術日チェック１０ 施術日チェック１１ 施術日チェック１２ 
           施術日チェック１３ 施術日チェック１４ 施術日チェック１５ 施術日チェック１６ 
           施術日チェック１７ 施術日チェック１８ 施術日チェック１９ 施術日チェック２０ 
           施術日チェック２１ 施術日チェック２２ 施術日チェック２３ 施術日チェック２４ 
           施術日チェック２５ 施術日チェック２６ 施術日チェック２７ 施術日チェック２８ 
           施術日チェック２９ 施術日チェック３０ 施術日チェック３１ 深夜チェック 時間外チェック 
           休日チェック 固定料チェック 整復料チェック 施療料チェック 夜間チェック 暴風雨雪チェック 
           難路チェック 大チェック 中チェック 小チェック 治癒チェック１ 中止チェック１ 転医チェック１ 
           治癒チェック２ 中止チェック２ 転医チェック２ 治癒チェック３ 中止チェック３ 転医チェック３ 
           治癒チェック４ 中止チェック４ 転医チェック４ 治癒チェック５ 中止チェック５ 転医チェック５ 
           新規チェック 継続チェック 男チェック 明治チェック 大正チェック 女チェック 昭和チェック 
           平成チェック 単独チェック 本人チェック 高一チェック 共済チェック 自チェック 社保チェック 
           組合チェック １０割チェック ９割チェック ２併チェック ６歳チェック ８割チェック ７割チェック 
           後期チェック 退職チェック 国保チェック 家族チェック 高７チェック
           .
039120*
039130*================================================================*
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
                   MOVE NC"○" TO 施術日チェックＷ(施記－施術日)
                   PERFORM 施術記録Ｆ読込
               END-PERFORM
           END-IF.
           MOVE 施術日チェックＷ(1)  TO 施術日チェック１.
           MOVE 施術日チェックＷ(2)  TO 施術日チェック２.
           MOVE 施術日チェックＷ(3)  TO 施術日チェック３.
           MOVE 施術日チェックＷ(4)  TO 施術日チェック４.
           MOVE 施術日チェックＷ(5)  TO 施術日チェック５.
           MOVE 施術日チェックＷ(6)  TO 施術日チェック６.
           MOVE 施術日チェックＷ(7)  TO 施術日チェック７.
           MOVE 施術日チェックＷ(8)  TO 施術日チェック８.
           MOVE 施術日チェックＷ(9)  TO 施術日チェック９.
           MOVE 施術日チェックＷ(10) TO 施術日チェック１０.
           MOVE 施術日チェックＷ(11) TO 施術日チェック１１.
           MOVE 施術日チェックＷ(12) TO 施術日チェック１２.
           MOVE 施術日チェックＷ(13) TO 施術日チェック１３.
           MOVE 施術日チェックＷ(14) TO 施術日チェック１４.
           MOVE 施術日チェックＷ(15) TO 施術日チェック１５.
           MOVE 施術日チェックＷ(16) TO 施術日チェック１６.
           MOVE 施術日チェックＷ(17) TO 施術日チェック１７.
           MOVE 施術日チェックＷ(18) TO 施術日チェック１８.
           MOVE 施術日チェックＷ(19) TO 施術日チェック１９.
           MOVE 施術日チェックＷ(20) TO 施術日チェック２０.
           MOVE 施術日チェックＷ(21) TO 施術日チェック２１.
           MOVE 施術日チェックＷ(22) TO 施術日チェック２２.
           MOVE 施術日チェックＷ(23) TO 施術日チェック２３.
           MOVE 施術日チェックＷ(24) TO 施術日チェック２４.
           MOVE 施術日チェックＷ(25) TO 施術日チェック２５.
           MOVE 施術日チェックＷ(26) TO 施術日チェック２６.
           MOVE 施術日チェックＷ(27) TO 施術日チェック２７.
           MOVE 施術日チェックＷ(28) TO 施術日チェック２８.
           MOVE 施術日チェックＷ(29) TO 施術日チェック２９.
           MOVE 施術日チェックＷ(30) TO 施術日チェック３０.
           MOVE 施術日チェックＷ(31) TO 施術日チェック３１.
042680**================================================================*
       助成施術日取得 SECTION.
      *
           MOVE 3              TO レセ－レセ種別.
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
028350     MOVE 患者番号ＷＲ          TO 施記－患者番号.
028360     MOVE 枝番ＷＲ              TO 施記－枝番.
028370     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
028380     MOVE 施術年ＷＲ            TO 施記－施術年.
028390     MOVE 施術月ＷＲ            TO 施記－施術月.
      *------------------------------------------------------------------------*
           IF ( レセ－助成月途中対象 = 1 )
               MOVE 受－助成月途中開始日  TO 施記－施術日
           ELSE
               MOVE ZERO                  TO 施記－施術日
           END-IF.
      *------------------------------------------------------------------------*
028420     START 施術記録Ｆ   KEY IS >= 施記－患者コード
028430                                  施記－施術和暦年月日
028440     END-START.
028450     IF 状態キー = "00"
030910         MOVE SPACE TO 終了フラグ２
               MOVE ZERO  TO カウンタ
030920         PERFORM 施術記録Ｆ読込
030930         PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
030940                       ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
030950                       ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
030960                       ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
030970                       ( 施記－施術月     NOT = 施術月ＷＲ      )
                   COMPUTE カウンタ = カウンタ + 1
                   MOVE 施記－施術日 TO 日ＷＰ
                   STRING 日ＷＰ                DELIMITED BY SIZE
                          "・"                  DELIMITED BY SIZE
                     INTO 助成施術日ＷＰ(カウンタ)
                   END-STRING
                   PERFORM 施術記録Ｆ読込
               END-PERFORM
           END-IF.
           MOVE カウンタ TO 日ＷＰ
           COMPUTE 施術日文字数Ｗ = カウンタ * 4 - 2
           STRING "［施術日］"                     DELIMITED BY SPACE
                  助成施術日ＷＲ(1:施術日文字数Ｗ) DELIMITED BY SIZE
                  "［実日数］"                     DELIMITED BY SIZE
                  日ＷＰ                           DELIMITED BY SIZE
                  "日"                             DELIMITED BY SIZE
             INTO 助成施術日Ｗ
           END-STRING.
      *================================================================*
039140******************************************************************
039150 END PROGRAM YCH6125.
039160******************************************************************
