000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCH6475.
000060 AUTHOR.                 池田 幸子
000070*
000080*----------------------------------------------------------------*
000090*         中央 後高レセプト印刷（柔+ｳｨﾝﾄﾞｳｽﾞ版）
000100*         MED = YAW610 YCH6125P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2011-09-13
000130 DATE-COMPILED.          2011-09-13
      */実日数はレセ－部位実日数を転記する/160816
      */金属副子・運動後療の変更・追加/1805
      */明細書発行加算を適用に追加/2022
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
000370     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
000380                             ORGANIZATION             IS  INDEXED
000390                             ACCESS MODE              IS  DYNAMIC
000400                             RECORD KEY               IS  市－公費種別
000410                                                          市－市町村番号
000420                             ALTERNATE RECORD KEY     IS  市－公費種別
000430                                                          市－市町村名称
000440                                                          市－市町村番号
000450                             FILE STATUS              IS  状態キー
000460                             LOCK        MODE         IS  AUTOMATIC.
000470     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000480                             ORGANIZATION             IS  INDEXED
000490                             ACCESS MODE              IS  DYNAMIC
000500                             RECORD KEY               IS  元－元号区分
000510                             FILE STATUS              IS  状態キー
000520                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS MODE              IS  DYNAMIC
000560                             RECORD KEY               IS  名－区分コード
000570                                                          名－名称コード
000580                             FILE STATUS              IS  状態キー
000590                             LOCK        MODE         IS  AUTOMATIC.
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
000660     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000670                             ORGANIZATION             IS  INDEXED
000680                             ACCESS MODE              IS  DYNAMIC
000690                             RECORD KEY               IS  制－制御区分
000700                             FILE STATUS              IS  状態キー
000710                             LOCK        MODE         IS  AUTOMATIC.
000720     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000730                             ORGANIZATION             IS  INDEXED
000740                             ACCESS MODE              IS  DYNAMIC
000750                             RECORD KEY               IS  施情－施術所番号
000760                             FILE STATUS              IS  状態キー
000770                             LOCK        MODE         IS  AUTOMATIC.
000780     SELECT  請求先マスタ    ASSIGN      TO        SEIKYUSL
000790                             ORGANIZATION             IS  INDEXED
000800                             ACCESS MODE              IS  DYNAMIC
000810                             RECORD KEY               IS  請先－保険種別
000820                                                          請先－保険者番号
000830                             FILE STATUS              IS  状態キー
000840                             LOCK    MODE             IS  AUTOMATIC.
000850     SELECT  経過マスタ      ASSIGN      TO        KEIKAL
000860                             ORGANIZATION             IS  INDEXED
000870                             ACCESS MODE              IS  DYNAMIC
000880                             RECORD KEY               IS  経－区分コード
000890                                                          経－経過コード
000900                             FILE STATUS              IS  状態キー
000910                             LOCK        MODE         IS  AUTOMATIC.
000920     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000930                             ORGANIZATION             IS  INDEXED
000940                             ACCESS MODE              IS  DYNAMIC
000950                             RECORD KEY               IS  受－施術和暦年月
000960                                                          受－患者コード
000970                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000980                                                          受－患者カナ
000990                                                          受－患者コード
001000                             ALTERNATE RECORD KEY     IS  受－患者コード
001010                                                          受－施術和暦年月
001020                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001030                                                          受－保険種別
001040                                                          受－保険者番号
001050                                                          受－患者コード
001060                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001070                                                          受－公費種別
001080                                                          受－費用負担者番号
001090                                                          受－患者コード
001100                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001110                                                          受－助成種別
001120                                                          受－費用負担者番号助成
001130                                                          受－患者コード
001140                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
001150                                                          受－施術和暦年月
001160                                                          受－患者コード
001170                             FILE STATUS              IS  状態キー
001180                             LOCK        MODE         IS  AUTOMATIC.
001190     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
001200                             ORGANIZATION             IS  INDEXED
001210                             ACCESS MODE              IS  DYNAMIC
001220                             RECORD KEY               IS  施記－施術和暦年月日
001230                                                          施記－患者コード
001240                             ALTERNATE RECORD KEY     IS  施記－患者コード
001250                                                          施記－施術和暦年月日
001260                             FILE STATUS              IS  状態キー
001270                             LOCK        MODE         IS  AUTOMATIC.
001280     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
001290                             ORGANIZATION             IS  INDEXED
001300                             ACCESS MODE              IS  DYNAMIC
001310                             RECORD KEY               IS  負－施術和暦年月
001320                                                          負－患者コード
001330                             ALTERNATE RECORD KEY     IS  負－患者コード
001340                                                          負－施術和暦年月
001350                             FILE STATUS              IS  状態キー
001360                             LOCK        MODE         IS  AUTOMATIC.
001370     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
001380                             ORGANIZATION             IS  INDEXED
001390                             ACCESS MODE              IS  DYNAMIC
001400                             RECORD KEY               IS  負原－区分コード
001410                                                          負原－負傷原因コード
001420                             FILE STATUS              IS  状態キー
001430                             LOCK        MODE         IS  AUTOMATIC.
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
001560     SELECT  ＩＤ管理マスタ    ASSIGN      TO        IDKANRL
001570                             ORGANIZATION             IS  INDEXED
001580                             ACCESS MODE              IS  DYNAMIC
001590                             RECORD KEY               IS  ＩＤ管－ＩＤ区分
001600                                                          ＩＤ管－施術所番号
001610                                                          ＩＤ管－保険種別
001620                                                          ＩＤ管－保険者番号
001630                             ALTERNATE RECORD KEY     IS  ＩＤ管－施術ＩＤ番号
001640                                                          ＩＤ管－ＩＤ区分
001650                                                          ＩＤ管－施術所番号
001660                                                          ＩＤ管－保険種別
001670                                                          ＩＤ管－保険者番号
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
002090*                           ［ＲＬ＝  ２５６］
002100 FD  市町村マスタ          BLOCK   CONTAINS   1   RECORDS.
002110     COPY SITYOSN        OF  XFDLIB  JOINING   市   AS  PREFIX.
002120*                           ［ＲＬ＝  １２８］
002130 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
002140     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
002150*                           ［ＲＬ＝  １２８］
002160 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
002170     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
002210*                           ［ＲＬ＝  ２５６］
002220 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
002230     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
002240*                           ［ＲＬ＝  １２８］
002250 FD  施術所情報マスタ          BLOCK   CONTAINS   1   RECORDS.
002260     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
002270*                           ［ＲＬ＝  １２８］
002280 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
002290     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
002300*                           ［ＲＬ＝  １２８］
002310 FD  経過マスタ          BLOCK   CONTAINS   1   RECORDS.
002320     COPY KEIKA          OF  XFDLIB  JOINING   経   AS  PREFIX.
002330*                           ［ＲＬ＝  ３２０］
002340 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
002350     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
002360*                           ［ＲＬ＝  ２５６］
002370 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
002380     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
002390*                           ［ＲＬ＝  １２８］
002400 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002410     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
002420*                           ［ＲＬ＝  １２８］
002430 FD  負傷原因Ｆ         BLOCK   CONTAINS   1   RECORDS.
002440     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
002450*                           ［ＲＬ＝  ６４０］
002460 FD  会情報マスタ        BLOCK   CONTAINS   1   RECORDS.
002470     COPY KAIJOHO         OF  XFDLIB  JOINING   会情   AS  PREFIX.
002480*                           ［ＲＬ＝  １２８］
002490 FD  ＩＤ管理マスタ          BLOCK   CONTAINS   1   RECORDS.
002500     COPY IDKANR    OF  XFDLIB  JOINING   ＩＤ管   AS  PREFIX.
002510*                           ［ＲＬ＝  ８３２］
002520 FD  メモファイル        BLOCK CONTAINS 1     RECORDS.
002530     COPY MEMO           OF    XFDLIB JOINING メモ AS PREFIX.
002540**
002550 FD  作業ファイル４ RECORD  CONTAINS 32 CHARACTERS.
002560 01  作４－レコード.
002570     03  作４－レコードキー.
002580         05  作４－施術和暦年月.
002590             07  作４－施術和暦            PIC 9.
002600             07  作４－施術年              PIC 9(2).
002610             07  作４－施術月              PIC 9(2).
002620         05  作４－患者コード.
002630             07 作４－患者番号             PIC 9(6).
002640             07 作４－枝番                 PIC X(1).
002650         05  作４－保険種別                PIC 9(2).
002660     03  作４－レコードデータ.
002670         05  作４－順番                    PIC 9(4).
002680         05  FILLER                        PIC X(14).
002690*
002700 FD  印刷ファイル.
002710     COPY YCH6125P       OF  XMDLIB.
002720*----------------------------------------------------------------*
002730******************************************************************
002740*                WORKING-STORAGE SECTION                         *
002750******************************************************************
002760 WORKING-STORAGE         SECTION.
002770 01 キー入力                           PIC X     VALUE SPACE.
002780 01 状態キー                           PIC X(2)  VALUE SPACE.
002790 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002800 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002820 01 終了フラグ３                       PIC X(3)  VALUE SPACE.
002810 01 初検フラグ                         PIC X(3)  VALUE SPACE.
002820 01 ファイル名                         PIC N(6)  VALUE SPACE.
002830 01 レセプトＰＧＷ                     PIC X(8)  VALUE SPACE.
002840 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
002850 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
002860 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002870 01 患者番号Ｗ                         PIC 9(6)  VALUE ZERO.
002880 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002890 01 部位名称Ｗ                         PIC N(12) VALUE SPACE.
002900 01 部位長Ｗ                           PIC 9(2) VALUE 1.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002870 01 用紙種別Ｗ                         PIC 9(1)  VALUE ZERO.
002910** 数字→日本語変換
002920 01 数字Ｗ                             PIC 9(2).
002930 01 数字Ｒ REDEFINES 数字Ｗ.
002940    03 数字Ｗ１                        PIC X(1).
002950    03 数字Ｗ２                        PIC X(1).
002960*
002970 01 負傷番号Ｗ                         PIC 9.
002980 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
002990    03 負傷番号Ｗ１                    PIC X.
003000*
003010 01 全角負傷番号Ｗ                     PIC N.
003020 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
003030    03 全角負傷番号Ｗ１                PIC X(2).
003040*
003050 01 カウンタ                           PIC 9(2)  VALUE ZERO.
003060 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
003070*
003080* 退避用
003090 01 終了年月日ＷＴ.
003100    03 終了年ＷＴ                      PIC 9(2)  VALUE ZERO.
003110    03 終了月ＷＴ                      PIC 9(2)  VALUE ZERO.
003120    03 終了日ＷＴ                      PIC 9(2)  VALUE ZERO.
003130* 初検日退避用
003140 01 初検年月日ＷＴ.
003150    03 初検和暦ＷＴ                    PIC 9     VALUE ZERO.
003160    03 初検年ＷＴ                      PIC 9(2)  VALUE ZERO.
003170    03 初検月ＷＴ                      PIC 9(2)  VALUE ZERO.
003180    03 初検日ＷＴ                      PIC 9(2)  VALUE ZERO.
003190* 初検加算時刻用
003200 01 初検加算ＷＴ.
003210    03 初検加算カウント                PIC 9    VALUE ZERO.
003220    03 番号カウンタ                    PIC 9    VALUE ZERO.
003230    03 初検加算集団ＷＴ  OCCURS 3.
003240       05 初検加算区分ＷＴ             PIC 9    VALUE ZERO.
003250       05 初検加算時ＷＴ               PIC 9(2) VALUE ZERO.
003260       05 初検加算分ＷＴ               PIC 9(2) VALUE ZERO.
003270    03 初検加算集団ＮＷ  OCCURS 3.
003280       05 加算区切Ｗ                   PIC N(1) VALUE SPACE.
003290       05 加算内容Ｗ                   PIC N(3) VALUE SPACE.
003300       05 初検加算時ＮＷ１             PIC N(1) VALUE SPACE.
003310       05 初検加算時ＮＷ２             PIC N(1) VALUE SPACE.
003320       05 時固定Ｗ                     PIC N(1) VALUE SPACE.
003330       05 初検加算分ＮＷ１             PIC N(1) VALUE SPACE.
003340       05 初検加算分ＮＷ２             PIC N(1) VALUE SPACE.
003350       05 分固定Ｗ                     PIC N(1) VALUE SPACE.
003360    03 初検加算時刻１Ｗ                PIC N(10) VALUE SPACE.
003370    03 初検加算時刻２Ｗ                PIC N(10) VALUE SPACE.
003380    03 初検加算時刻３Ｗ                PIC N(10) VALUE SPACE.
003070    03 初検加算区切Ｗ                  PIC X     VALUE SPACE.
003080    03 初検加算時Ｗ                    PIC 9(2)  VALUE ZERO.
003090    03 初検加算分Ｗ                    PIC 9(2)  VALUE ZERO.
003390* 負傷原因用
003400 01 負傷原因ＷＴ.
003410    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
003420    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
003430    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
003440    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
003450    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
003460    03 負傷原因ナンバーＷＴ.
003470       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
003480    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
003490 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
003500 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
003510 01 負傷原因ＴＢＬ.
003520    03 負傷原因コードＴＢＬ            OCCURS 9.
003530       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
003540       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
003550       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
003560 01 負傷原因内容Ｗ.
003570    03 負傷原因内容合成Ｗ              PIC X(318) OCCURS 9 VALUE SPACE.
003580    03 負傷原因内容分解ＸＷ.
003590       05 負傷原因内容１ＸＷ           PIC X(80)  VALUE SPACE.
003600       05 負傷原因内容２ＸＷ           PIC X(80)  VALUE SPACE.
003610       05 負傷原因内容３ＸＷ           PIC X(80)  VALUE SPACE.
003700       05 負傷原因内容４ＸＷ           PIC X(78)  VALUE SPACE.
003620*
003630* 東京振込先 会長名用(５文字）
003640 01 会長名Ｗ                           PIC N(5)   VALUE SPACE.
003650*
003660** 前月初検のみ用
003670 01 初日再検フラグ                     PIC X(3)  VALUE SPACE.
003680 01 前月フラグ                         PIC X(3)  VALUE SPACE.
003690*
003700 01 計算年月日Ｗ.
003710    03 計算和暦Ｗ                      PIC 9(1)  VALUE ZERO.
003720    03 計算年Ｗ                        PIC S9(2)  VALUE ZERO.
003730    03 計算月Ｗ                        PIC S9(2)  VALUE ZERO.
003740    03 計算日Ｗ                        PIC S9(2)  VALUE ZERO.
003750 01 開始年月日２Ｗ.
003760    03 開始和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003770    03 開始年２Ｗ                      PIC 9(2)  VALUE ZERO.
003780    03 開始月２Ｗ                      PIC 9(2)  VALUE ZERO.
003790    03 開始日２Ｗ                      PIC 9(2)  VALUE ZERO.
003800    03 開始西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003810 01 終了年月日２Ｗ.
003820    03 終了和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003830    03 終了年２Ｗ                      PIC 9(2)  VALUE ZERO.
003840    03 終了月２Ｗ                      PIC 9(2)  VALUE ZERO.
003850    03 終了日２Ｗ                      PIC 9(2)  VALUE ZERO.
003860    03 終了西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003870***
003880** 負傷原因・長期理由印刷区分用
003890 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
003900 01 長期理由印刷区分Ｗ                 PIC 9 VALUE ZERO.
003910*
003920** レセ下段の日付区分用 (0:最終通院日、1:月末日、9:印字なし)
003930 01 レセプト日付区分Ｗ                 PIC 9 VALUE ZERO.
003940 01 レセプト患者日付区分Ｗ             PIC 9 VALUE ZERO.
003950*
003960** 月末日用
003970 01 施術西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
003980 01 商Ｗ                               PIC 9(3)  VALUE ZERO.
003990 01 余Ｗ                               PIC 9(3)  VALUE ZERO.
004000*
004010** 枝番判定用
004020 01 開始診療日手動区分Ｗ               PIC 9    VALUE ZERO.
004030*
004040*
004050** 助成レセまとめ用
004060 01 助成レセまとめフラグ               PIC X(3)  VALUE SPACE.
004070 01 助成種別略称Ｗ                     PIC N(4)  VALUE SPACE.
004080 01 助成種別略称Ｗ２                   PIC N(4)  VALUE SPACE.
004090*
004100*
004110** レセ摘要用( N(38)固定） /
004120 01 負傷の経過Ｗ.
004130    03 負傷の経過行Ｗ                  PIC X(76) OCCURS 2 VALUE SPACE.
004140 01 負傷の経過ＮＷ REDEFINES 負傷の経過Ｗ.
004150    03 負傷の経過行ＮＷ                PIC N(38) OCCURS 2.
004160*
003460 01 長期理由印刷区分Ｆ                 PIC 9 VALUE ZERO.
004170*
004180* 負傷原因印刷区分
004190 01 レセ負傷原因印刷区分Ｗ             PIC 9    VALUE ZERO.
      *
004490* 長野県用
004500 01 受給者番号編集Ｗ.
004510    03 受給者番号編集Ｗ１              PIC X(3)  VALUE SPACE.
004520    03 受給者区切１                    PIC X     VALUE SPACE.
004530    03 受給者番号編集Ｗ２              PIC X(2)  VALUE SPACE.
004540    03 受給者区切２                    PIC X     VALUE SPACE.
004550    03 受給者番号編集Ｗ３              PIC X(10) VALUE SPACE.
      *
      */金属副子・運動後療の変更・追加/1805
       01 金属副子ＣＭ１                     PIC X(130) VALUE SPACE.
       01 金属副子ＣＭ２                     PIC X(170) VALUE SPACE.
       01 運動後療ＣＭ                       PIC X(68)  VALUE SPACE.
004200*
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
      *
004210****************
004220* 連結項目待避 *
004230****************
004240*    ************
004250*    * 印刷キー *
004260*    ************
004270 01 対象データＷＲ.
004280    03 施術和暦年月ＷＲ.
004290       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
004300       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
004310       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
004320    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004330    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
004340    03 公費種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004350    03 費用負担者番号ＷＲ               PIC X(10) VALUE SPACE.
004360    03 助成種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004370    03 費用負担者番号助成ＷＲ           PIC X(10) VALUE SPACE.
004380    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
004390    03 患者カナＷＲ                     PIC X(20) VALUE SPACE.
004400    03 患者コードＷＲ.
004410       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
004420       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
004430*    ************
004440*    * 料金情報 *
004450*    ************
004460*    月毎の料金
004470***********************
004480 01 料金１ＷＲ.
004490   03 初検ＷＲ.
004500      05 負担割合ＷＲ               PIC 9(3)    VALUE ZERO.
004510      05 初検料ＷＲ                 PIC 9(5)    VALUE ZERO.
004520      05 初検加算料ＷＲ             PIC 9(5)    VALUE ZERO.
         03 初検時相談料ＷＲ              PIC 9(4)    VALUE ZERO.
004530   03 再検料ＷＲ                    PIC 9(5)    VALUE ZERO.
004540   03 往療ＷＲ.
004550      05 往療距離ＷＲ               PIC 9(2)V9  VALUE ZERO.
004560      05 往療回数ＷＲ               PIC 9(2)    VALUE ZERO.
004570      05 往療料ＷＲ                 PIC 9(6)    VALUE ZERO.
004580      05 往療加算料ＷＲ             PIC 9(5)    VALUE ZERO.
004590   03 金属副子加算料ＷＲ            PIC 9(5)    VALUE ZERO.
004600   03 施術情報提供料ＷＲ            PIC 9(5)    VALUE ZERO.
004610   03 合計ＷＲ                      PIC 9(6)    VALUE ZERO.
004620   03 一部負担金ＷＲ                PIC 9(6)    VALUE ZERO.
004630   03 請求金額ＷＲ                  PIC 9(6)    VALUE ZERO.
004640   03 給付割合ＷＲ                  PIC 9(1)    VALUE ZERO.
004650   03 受給者負担額ＷＲ              PIC 9(6)    VALUE ZERO.
004660   03 助成請求金額ＷＲ              PIC 9(6)    VALUE ZERO.
004670*
004680* 負傷部位毎の料金
004690***********************
004700 01 料金２ＷＲ.
004710   03 初回処置ＷＲ    OCCURS   9.
004720      05 初回処置料ＷＲ             PIC 9(5)    VALUE ZERO.
004730*
004740* 逓減毎の料金
004750***********************
004760 01 料金３ＷＲ.
004770**********
004780* １部位 *
004790**********
004800   03 部位１ＷＲ.
004810      05 後療１ＷＲ.
004820         07 後療単価１ＷＲ              PIC 9(4)    VALUE ZERO.
004830         07 後療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
004840         07 後療料１ＷＲ                PIC 9(5)    VALUE ZERO.
004850      05 冷罨法１ＷＲ.
004860         07 冷罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
004870         07 冷罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
004880      05 温罨法１ＷＲ.
004890         07 温罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
004900         07 温罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
004910      05 電療１ＷＲ.
004920         07 電療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
004930         07 電療料１ＷＲ                PIC 9(4)    VALUE ZERO.
004940      05 小計１ＷＲ                     PIC 9(6)    VALUE ZERO.
004950      05 長期逓減率１ＷＲ               PIC 9(3)    VALUE ZERO.
004960      05 長期込小計１ＷＲ               PIC 9(6)    VALUE ZERO.
004970**********
004980* ２部位 *
004990**********
005000   03 部位２ＷＲ.
005010      05 後療２ＷＲ.
005020         07 後療単価２ＷＲ              PIC 9(4)    VALUE ZERO.
005030         07 後療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
005040         07 後療料２ＷＲ                PIC 9(5)    VALUE ZERO.
005050      05 冷罨法２ＷＲ.
005060         07 冷罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
005070         07 冷罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
005080      05 温罨法２ＷＲ.
005090         07 温罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
005100         07 温罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
005110      05 電療２ＷＲ.
005120         07 電療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
005130         07 電療料２ＷＲ                PIC 9(4)    VALUE ZERO.
005140      05 小計２ＷＲ                     PIC 9(6)    VALUE ZERO.
005150      05 長期逓減率２ＷＲ               PIC 9(3)    VALUE ZERO.
005160      05 長期込小計２ＷＲ               PIC 9(6)    VALUE ZERO.
005170******************
005180* ３部位／８割 *
005190******************
005200   03 部位３８ＷＲ.
005210      05 後療３８ＷＲ.
005220         07 後療単価３８ＷＲ              PIC 9(4)  VALUE ZERO.
005230         07 後療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
005240         07 後療料３８ＷＲ                PIC 9(5)  VALUE ZERO.
005250      05 冷罨法３８ＷＲ.
005260         07 冷罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
005270         07 冷罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
005280      05 温罨法３８ＷＲ.
005290         07 温罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
005300         07 温罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
005310      05 電療３８ＷＲ.
005320         07 電療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
005330         07 電療料３８ＷＲ                PIC 9(4)  VALUE ZERO.
005340      05 小計３８ＷＲ                     PIC 9(6)  VALUE ZERO.
005350      05 多部位込小計３８ＷＲ             PIC 9(6)  VALUE ZERO.
005360      05 長期逓減率３８ＷＲ               PIC 9(3)  VALUE ZERO.
005370      05 長期込小計３８ＷＲ               PIC 9(6)  VALUE ZERO.
005380******************
005390* ３部位／１０割 *
005400******************
005410   03 部位３０ＷＲ.
005420      05 逓減開始月日３０ＷＲ.
005430         07 逓減開始月３０ＷＲ            PIC 9(2)  VALUE ZERO.
005440         07 逓減開始日３０ＷＲ            PIC 9(2)  VALUE ZERO.
005450      05 後療３０ＷＲ.
005460         07 後療単価３０ＷＲ              PIC 9(4)  VALUE ZERO.
005470         07 後療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
005480         07 後療料３０ＷＲ                PIC 9(5)  VALUE ZERO.
005490      05 冷罨法３０ＷＲ.
005500         07 冷罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
005510         07 冷罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
005520      05 温罨法３０ＷＲ.
005530         07 温罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
005540         07 温罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
005550      05 電療３０ＷＲ.
005560         07 電療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
005570         07 電療料３０ＷＲ                PIC 9(4)  VALUE ZERO.
005580      05 小計３０ＷＲ                     PIC 9(6)  VALUE ZERO.
005590      05 長期逓減率３０ＷＲ               PIC 9(3)  VALUE ZERO.
005600      05 長期込小計３０ＷＲ               PIC 9(6)  VALUE ZERO.
005610****************
005620* ４部位／５割 *
005630****************
005640   03 部位４５ＷＲ.
005650      05 後療４５ＷＲ.
005660         07 後療単価４５ＷＲ              PIC 9(4)  VALUE ZERO.
005670         07 後療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005680         07 後療料４５ＷＲ                PIC 9(5)  VALUE ZERO.
005690      05 冷罨法４５ＷＲ.
005700         07 冷罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005710         07 冷罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
005720      05 温罨法４５ＷＲ.
005730         07 温罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005740         07 温罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
005750      05 電療４５ＷＲ.
005760         07 電療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005770         07 電療料４５ＷＲ                PIC 9(4)  VALUE ZERO.
005780      05 小計４５ＷＲ                     PIC 9(6)  VALUE ZERO.
005790      05 多部位込小計４５ＷＲ             PIC 9(6)  VALUE ZERO.
005800      05 長期逓減率４５ＷＲ               PIC 9(3)  VALUE ZERO.
005810      05 長期込小計４５ＷＲ               PIC 9(6)  VALUE ZERO.
005820****************
005830* ４部位／８割 *
005840****************
005850   03 部位４８ＷＲ.
005860      05 逓減開始月日４８ＷＲ.
005870         07 逓減開始月４８ＷＲ            PIC 9(2)  VALUE ZERO.
005880         07 逓減開始日４８ＷＲ            PIC 9(2)  VALUE ZERO.
005890      05 後療４８ＷＲ.
005900         07 後療単価４８ＷＲ              PIC 9(4)  VALUE ZERO.
005910         07 後療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
005920         07 後療料４８ＷＲ                PIC 9(5)  VALUE ZERO.
005930      05 冷罨法４８ＷＲ.
005940         07 冷罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
005950         07 冷罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
005960      05 温罨法４８ＷＲ.
005970         07 温罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
005980         07 温罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
005990      05 電療４８ＷＲ.
006000         07 電療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
006010         07 電療料４８ＷＲ                PIC 9(4)  VALUE ZERO.
006020      05 小計４８ＷＲ                     PIC 9(6)  VALUE ZERO.
006030      05 多部位込小計４８ＷＲ             PIC 9(6)  VALUE ZERO.
006040      05 長期逓減率４８ＷＲ               PIC 9(3)  VALUE ZERO.
006050      05 長期込小計４８ＷＲ               PIC 9(6)  VALUE ZERO.
006060******************
006070* ４部位／１０割 *
006080******************
006090   03 部位４０ＷＲ.
006100      05 逓減開始月日４０ＷＲ.
006110         07 逓減開始月４０ＷＲ            PIC 9(2)  VALUE ZERO.
006120         07 逓減開始日４０ＷＲ            PIC 9(2)  VALUE ZERO.
006130      05 後療４０ＷＲ.
006140         07 後療単価４０ＷＲ              PIC 9(4)  VALUE ZERO.
006150         07 後療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
006160         07 後療料４０ＷＲ                PIC 9(5)  VALUE ZERO.
006170      05 冷罨法４０ＷＲ.
006180         07 冷罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
006190         07 冷罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
006200      05 温罨法４０ＷＲ.
006210         07 温罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
006220         07 温罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
006230      05 電療４０ＷＲ.
006240         07 電療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
006250         07 電療料４０ＷＲ                PIC 9(4)  VALUE ZERO.
006260      05 小計４０ＷＲ                     PIC 9(6)  VALUE ZERO.
006270      05 長期逓減率４０ＷＲ               PIC 9(3)  VALUE ZERO.
006280      05 長期込小計４０ＷＲ               PIC 9(6)  VALUE ZERO.
006290********************
006300* ５部位／２．５割 *
006310********************
006320   03 部位５２ＷＲ.
006330      05 後療５２ＷＲ.
006340         07 後療単価５２ＷＲ              PIC 9(4)  VALUE ZERO.
006350         07 後療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
006360         07 後療料５２ＷＲ                PIC 9(5)  VALUE ZERO.
006370      05 冷罨法５２ＷＲ.
006380         07 冷罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
006390         07 冷罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
006400      05 温罨法５２ＷＲ.
006410         07 温罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
006420         07 温罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
006430      05 電療５２ＷＲ.
006440         07 電療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
006450         07 電療料５２ＷＲ                PIC 9(4)  VALUE ZERO.
006460      05 小計５２ＷＲ                     PIC 9(6)  VALUE ZERO.
006470      05 多部位込小計５２ＷＲ             PIC 9(6)  VALUE ZERO.
006480      05 長期逓減率５２ＷＲ               PIC 9(3)  VALUE ZERO.
006490      05 長期込小計５２ＷＲ               PIC 9(6)  VALUE ZERO.
006500****************
006510* ５部位／５割 *
006520****************
006530   03 部位５５ＷＲ.
006540      05 逓減開始月日５５ＷＲ.
006550         07 逓減開始月５５ＷＲ            PIC 9(2)  VALUE ZERO.
006560         07 逓減開始日５５ＷＲ            PIC 9(2)  VALUE ZERO.
006570      05 後療５５ＷＲ.
006580         07 後療単価５５ＷＲ              PIC 9(4)  VALUE ZERO.
006590         07 後療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
006600         07 後療料５５ＷＲ                PIC 9(5)  VALUE ZERO.
006610      05 冷罨法５５ＷＲ.
006620         07 冷罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
006630         07 冷罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
006640      05 温罨法５５ＷＲ.
006650         07 温罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
006660         07 温罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
006670      05 電療５５ＷＲ.
006680         07 電療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
006690         07 電療料５５ＷＲ                PIC 9(4)  VALUE ZERO.
006700      05 小計５５ＷＲ                     PIC 9(6)  VALUE ZERO.
006710      05 多部位込小計５５ＷＲ             PIC 9(6)  VALUE ZERO.
006720      05 長期逓減率５５ＷＲ               PIC 9(3)  VALUE ZERO.
006730      05 長期込小計５５ＷＲ               PIC 9(6)  VALUE ZERO.
006740****************
006750* ５部位／８割 *
006760****************
006770   03 部位５８ＷＲ.
006780      05 逓減開始月日５８ＷＲ.
006790         07 逓減開始月５８ＷＲ            PIC 9(2)  VALUE ZERO.
006800         07 逓減開始日５８ＷＲ            PIC 9(2)  VALUE ZERO.
006810      05 後療５８ＷＲ.
006820         07 後療単価５８ＷＲ              PIC 9(4)  VALUE ZERO.
006830         07 後療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
006840         07 後療料５８ＷＲ                PIC 9(5)  VALUE ZERO.
006850      05 冷罨法５８ＷＲ.
006860         07 冷罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
006870         07 冷罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
006880      05 温罨法５８ＷＲ.
006890         07 温罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
006900         07 温罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
006910      05 電療５８ＷＲ.
006920         07 電療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
006930         07 電療料５８ＷＲ                PIC 9(4)  VALUE ZERO.
006940      05 小計５８ＷＲ                     PIC 9(6)  VALUE ZERO.
006950      05 多部位込小計５８ＷＲ             PIC 9(6)  VALUE ZERO.
006960      05 長期逓減率５８ＷＲ               PIC 9(3)  VALUE ZERO.
006970      05 長期込小計５８ＷＲ               PIC 9(6)  VALUE ZERO.
006980******************
006990* ５部位／１０割 *
007000******************
007010   03 部位５０ＷＲ.
007020      05 逓減開始月日５０ＷＲ.
007030         07 逓減開始月５０ＷＲ            PIC 9(2)  VALUE ZERO.
007040         07 逓減開始日５０ＷＲ            PIC 9(2)  VALUE ZERO.
007050      05 後療５０ＷＲ.
007060         07 後療単価５０ＷＲ              PIC 9(4)  VALUE ZERO.
007070         07 後療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
007080         07 後療料５０ＷＲ                PIC 9(5)  VALUE ZERO.
007090      05 冷罨法５０ＷＲ.
007100         07 冷罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
007110         07 冷罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
007120      05 温罨法５０ＷＲ.
007130         07 温罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
007140         07 温罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
007150      05 電療５０ＷＲ.
007160         07 電療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
007170         07 電療料５０ＷＲ                PIC 9(4)  VALUE ZERO.
007180      05 小計５０ＷＲ                     PIC 9(6)  VALUE ZERO.
007190      05 長期逓減率５０ＷＲ               PIC 9(3)  VALUE ZERO.
007200      05 長期込小計５０ＷＲ               PIC 9(6)  VALUE ZERO.
008000*******************
008010*  明細書発行加算 */202206
008020*******************
008030   03 明細書発行加算料ＷＲ                PIC ZZZ   VALUE ZERO.
008030   03 明細書発行加算日ＷＲ                PIC ZZ    VALUE ZERO.
007210*
007220**************
007230* 施術所情報 *
007240**************
007250 01 施術所情報Ｗ.
007260    03 柔整師番号Ｗ                    PIC X(16)  VALUE SPACE.
007270    03 接骨師会会員番号Ｗ              PIC X(16)  VALUE SPACE.
007280    03 代表者カナＷ                    PIC X(50)  VALUE SPACE.
007290    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
007300    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
          03 都道府県ＪＩＳＷ                PIC X(2)   VALUE SPACE.
007310    03 施術所住所Ｗ.
007320       05 施術所住所１Ｗ               PIC X(50)  VALUE SPACE.
007330       05 施術所住所２Ｗ               PIC X(50)  VALUE SPACE.
007340    03 施術所郵便番号Ｗ.
007350       05 施術所郵便番号１Ｗ           PIC X(3)   VALUE SPACE.
007360       05 施術所郵便番号２Ｗ           PIC X(4)   VALUE SPACE.
007370    03 施術所電話番号Ｗ                PIC X(20)  VALUE SPACE.
007380    03 定額制受理番号Ｗ                PIC X(15)  VALUE SPACE.
007390    03 受理年月日Ｗ.
007400       05 受理年Ｗ                     PIC 9(2)   VALUE ZERO.
007410       05 受理月Ｗ                     PIC 9(2)   VALUE ZERO.
007420       05 受理日Ｗ                     PIC 9(2)   VALUE ZERO.
007430    03 最終通院年月日Ｗ.
007440       05 最終通院年Ｗ                 PIC 9(2)   VALUE ZERO.
007450       05 最終通院月Ｗ                 PIC 9(2)   VALUE ZERO.
007460       05 最終通院日Ｗ                 PIC 9(2)   VALUE ZERO.
007470    03 柔整師年月日Ｗ.
007480       05 柔整師年Ｗ                   PIC 9(2)   VALUE ZERO.
007490       05 柔整師月Ｗ                   PIC 9(2)   VALUE ZERO.
007500       05 柔整師日Ｗ                   PIC 9(2)   VALUE ZERO.
007510    03 患者委任年月日Ｗ.
007520       05 患者委任年Ｗ                 PIC 9(2)   VALUE ZERO.
007530       05 患者委任月Ｗ                 PIC 9(2)   VALUE ZERO.
007540       05 患者委任日Ｗ                 PIC 9(2)   VALUE ZERO.
007550    03 取引先情報Ｗ.
007560        05 取引先銀行名Ｗ              PIC X(40)  VALUE SPACE.
007570        05 取引先銀行支店名Ｗ          PIC X(40)  VALUE SPACE.
007580        05 預金種別Ｗ                  PIC 9(1)   VALUE ZERO.
007590        05 口座番号Ｗ                  PIC X(10)  VALUE SPACE.
007600        05 口座名義人Ｗ                PIC X(40)  VALUE SPACE.
007610        05 口座名義人カナＷ            PIC X(40)  VALUE SPACE.
007620        05 口座名義人とカナＷ          PIC X(60)  VALUE SPACE.
007630        05 銀行名支店名Ｗ              PIC X(60)  VALUE SPACE.
007640        05 預金種別コメントＷ          PIC N(3)   VALUE SPACE.
007650        05 預金種別コメントＸＷ        PIC X(4)   VALUE SPACE.
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
007660*    03 コメントＷ.
007670*        05 コメント１Ｗ                PIC X(40)  VALUE SPACE.
007680*        05 コメント２Ｗ                PIC X(40)  VALUE SPACE.
007690*        05 コメント３Ｗ                PIC X(40)  VALUE SPACE.
007700*        05 コメント４Ｗ                PIC X(40)  VALUE SPACE.
007710*        05 コメント５Ｗ                PIC N(20)  VALUE SPACE.
007750    03 コメントＷ.
007760        05 コメント１Ｗ                PIC X(36)  VALUE SPACE.
007770        05 コメント２Ｗ                PIC X(36)  VALUE SPACE.
007780        05 コメント３Ｗ                PIC X(36)  VALUE SPACE.
007790        05 コメント４Ｗ                PIC X(36)  VALUE SPACE.
007720    03 県施術ＩＤＷ                    PIC X(15)  VALUE SPACE.
007730    03 市町村施術ＩＤＷ                PIC X(15)  VALUE SPACE.
007740**************
007750* 受診者情報 *
007760**************
007770 01 受診者情報Ｗ.
      */元号修正/20190408
          03 施術和暦Ｗ                      PIC 9(1)   VALUE ZERO.
007780    03 施術年月Ｗ.
007790       05 施術年Ｗ                     PIC 9(2)   VALUE ZERO.
007800       05 施術月Ｗ                     PIC 9(2)   VALUE ZERO.
007810*    03 記号Ｗ                          PIC N(12)  VALUE SPACE.
007570    03 記号Ｗ.
007580       05 印刷記号Ｗ                   PIC N(12)  VALUE SPACE.
          03 記号番号Ｗ.
             05 記号番号ＸＷ                 PIC X(40) VALUE SPACE.
007820    03 番号Ｗ.
007830       05 印刷番号Ｗ                   PIC X(15)  VALUE SPACE.
007840       05 FILLER                       PIC X(15)  VALUE SPACE.
007850    03 保険者番号Ｗ.
007860       05 印刷保険者番号Ｗ             PIC X(8)   VALUE SPACE.
007870       05 FILLER                       PIC X(2)   VALUE SPACE.
007880    03 市町村番号Ｗ.
007890       05 FILLER                       PIC X(2)   VALUE SPACE.
007900       05 市町村番号下６桁Ｗ           PIC X(6)   VALUE SPACE.
007910       05 FILLER                       PIC X(2)   VALUE SPACE.
007920    03 受給者番号Ｗ.
007930       05 印刷受給者番号Ｗ             PIC X(7)   VALUE SPACE.
007940       05 FILLER                       PIC X(13).
007950    03 助成市町村番号Ｗ                PIC X(8)   VALUE SPACE.
          03 助成受給者番号Ｗ                PIC X(15) VALUE SPACE.
007960    03 請求先名称Ｗ.
007970       05 印刷請求先名称Ｗ             PIC X(38)  VALUE SPACE.
007980       05 FILLER                       PIC X(38)  VALUE SPACE.
007990    03 保険種別Ｗ                      PIC 9(2)   VALUE ZERO.
008000    03 被保険者情報Ｗ.
008010       05 被保険者カナＷ               PIC X(50)  VALUE SPACE.
008020       05 被保険者氏名Ｗ               PIC X(50)  VALUE SPACE.
008030       05 郵便番号Ｗ.
008040          07 郵便番号１Ｗ              PIC X(3)   VALUE SPACE.
008050          07 郵便番号２Ｗ              PIC X(4)   VALUE SPACE.
008060       05 被保険者住所Ｗ               PIC X(80)  VALUE SPACE.
008070       05 被保険者住所１Ｗ             PIC X(50)  VALUE SPACE.
008080       05 被保険者住所２Ｗ             PIC X(50)  VALUE SPACE.
008990       05 電話番号Ｗ                   PIC X(35)  VALUE SPACE.
008090    03 患者情報Ｗ.
008100       05 患者住所Ｗ                   PIC X(80)  VALUE SPACE.
008110       05 患者住所１Ｗ                 PIC X(50)  VALUE SPACE.
008120       05 患者住所２Ｗ                 PIC X(50)  VALUE SPACE.
008130       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
008140       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
008150       05 性別チェックＷ.
008160          07 男チェックＷ              PIC N(1)  VALUE SPACE.
008170          07 女チェックＷ              PIC N(1)  VALUE SPACE.
008180       05 患者性別Ｗ.
008190          07 性別Ｗ                    PIC N(1)  VALUE SPACE.
008200       05 和暦チェックＷ.
008210          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
008220          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
008230          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
008240          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
008250          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
      */元号修正/↓↓↓20190408
008210          07 令和チェックＷ            PIC N(1)  VALUE SPACE.
                07 令和ＣＭＷ                PIC X(4)  VALUE SPACE.
009110*          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
      */元号修正/↑↑↑20190408
008260       05 患者年Ｗ                     PIC 9(2)  VALUE ZERO.
008270       05 患者月Ｗ                     PIC 9(2)  VALUE ZERO.
008280       05 患者日Ｗ                     PIC 9(2)  VALUE ZERO.
008290       05 続柄Ｗ.
008300          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
008310          07 FILLER                    PIC X(4)  VALUE SPACE.
008320       05 負傷原因１Ｗ                 PIC N(37) VALUE SPACE.
008330       05 負傷原因２Ｗ                 PIC N(37) VALUE SPACE.
008340       05 負傷原因３Ｗ                 PIC N(37) VALUE SPACE.
008350       05 負傷原因４Ｗ                 PIC N(37) VALUE SPACE.
008360       05 負傷原因５Ｗ                 PIC N(37) VALUE SPACE.
008370*
008370*       05 負傷原因Ｗ                   PIC N(40) OCCURS 29 VALUE SPACE.
      */半角対応/110421
             05 負傷原因Ｗ OCCURS 29.
                07 負傷原因ＸＷ              PIC X(80)  VALUE SPACE.
008390*
008400*       05 国保区分チェックＷ.
008410*          07 国保チェックＷ            PIC N(1)  VALUE SPACE.
008420*          07 国保組合チェックＷ        PIC N(1)  VALUE SPACE.
008430    03 助成印Ｗ                        PIC N(1)  VALUE SPACE.
008440    03 特別コメントＷ                  PIC X(16) VALUE SPACE.
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
008450*
008460****************
008470* 負傷データＦ *
008480****************
008490 01 負傷情報Ｗ.
008500    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
008510    03 部位情報Ｗ  OCCURS   9.
008520       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
008530       05 部位コードＷ.
008540          07 負傷種別Ｗ                PIC 9(2)  VALUE ZERO.
008550          07 部位Ｗ                    PIC 9(2)  VALUE ZERO.
008560          07 左右区分Ｗ                PIC 9(1)  VALUE ZERO.
008570          07 負傷位置番号Ｗ            PIC 9(2)  VALUE ZERO.
008580       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
008590       05 負傷年月日Ｗ.
008600          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
008610          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
008620          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
008630       05 初検年月日Ｗ.
008640          07 初検年Ｗ                  PIC 9(2)  VALUE ZERO.
008650          07 初検月Ｗ                  PIC 9(2)  VALUE ZERO.
008660          07 初検日Ｗ                  PIC 9(2)  VALUE ZERO.
008670       05 開始年月日Ｗ.
008680          07 開始年Ｗ                  PIC 9(2)  VALUE ZERO.
008690          07 開始月Ｗ                  PIC 9(2)  VALUE ZERO.
008700          07 開始日Ｗ                  PIC 9(2)  VALUE ZERO.
008710       05 終了年月日Ｗ.
008720          07 終了年Ｗ                  PIC 9(2)  VALUE ZERO.
008730          07 終了月Ｗ                  PIC 9(2)  VALUE ZERO.
008740          07 終了日Ｗ                  PIC 9(2)  VALUE ZERO.
008750       05 実日数Ｗ                     PIC 9(2)  VALUE ZERO.
008760       05 転帰区分Ｗ                   PIC 9(1)  VALUE ZERO.
008770       05 転帰区分チェックＷ.
008780          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
008790          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
008800          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
008810       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
008820       05 部位区切Ｗ                   PIC X(1)  VALUE SPACE.
008830       05 経過略称Ｗ.
008840          07 印刷経過略称Ｗ            PIC N(5)  VALUE SPACE.
008850          07 FILLER                    PIC X(2)  VALUE SPACE.
008860    03 経過部位Ｗ                      PIC N(1)  VALUE SPACE.
009030    03 経過ＣＭ                        PIC N(4)  VALUE SPACE.
008870    03 新規チェックＷ                  PIC N(1)  VALUE SPACE.
008880    03 継続チェックＷ                  PIC N(1)  VALUE SPACE.
          03 施術日Ｗ.
             05 施術日チェックＷ   OCCURS 31 PIC N(1)  VALUE SPACE.
008890****************
008900* ＯＣＲコード *
008910****************
008920* 01 ＯＣＲコードＷ.
008930*    03 ＯＣＲ会員番号Ｗ                PIC X(6)    VALUE ZERO.
008940*    03 ＯＣＲ施術年月Ｗ                PIC 9(4)    VALUE ZERO.
008950*    03 ＯＣＲ保険種別Ｗ                PIC 9(2)    VALUE ZERO.
008960*    03 ＯＣＲ各社ＩＤＷ                PIC 9(2)    VALUE ZERO.
008970*    03 ＯＣＲＦＤ連番Ｗ.
008980*       05 ＦＤ連番患者番号Ｗ           PIC 9(6)    VALUE ZERO.
008990*       05 ＦＤ連番健保ＩＤＷ           PIC 9(1)    VALUE ZERO.
009000*    03 ＯＣＲ請求金額Ｗ                PIC 9(6)    VALUE ZERO.
009010*    03 ＯＣＲ負担割合Ｗ                PIC 9(1)    VALUE ZERO.
009020*    03 ＯＣＲ接骨師会会員番号Ｗ        PIC 9(10)   VALUE ZERO.
009030*
009040************
009050* 料金情報 *
009060************
009070 01 料金情報Ｗ.
009080    03 初検加算Ｗ.
009090       05 時間外チェックＷ                PIC N(1) VALUE SPACE.
009100       05 休日チェックＷ                  PIC N(1) VALUE SPACE.
009110       05 深夜チェックＷ                  PIC N(1) VALUE SPACE.
009120    03 往療加算Ｗ.
009130       05 夜間チェックＷ                  PIC N(1) VALUE SPACE.
009140       05 暴風雨雪チェックＷ              PIC N(1) VALUE SPACE.
009150    03 金属副子チェックＷ.
009160       05 大チェックＷ                    PIC N(1) VALUE SPACE.
009170       05 中チェックＷ                    PIC N(1) VALUE SPACE.
009180       05 小チェックＷ                    PIC N(1) VALUE SPACE.
009190    03 小計Ｗ                             PIC 9(7) VALUE ZERO.
009200    03 初回処置料合計Ｗ                   PIC 9(6) VALUE ZERO.
009210    03 初回処置料チェックＷ.
009220       05 整復料チェックＷ                PIC N(1) VALUE SPACE.
009230       05 固定料チェックＷ                PIC N(1) VALUE SPACE.
009240       05 施療料チェックＷ                PIC N(1) VALUE SPACE.
      */金属副子・運動後療の変更・追加/1805
          03 金属回数Ｗ                         PIC 9(2)  VALUE ZERO.
          03 運動料Ｗ                           PIC 9(4)  VALUE ZERO.
009250************
009260* 備考情報 *
009270************
009280 01 備考情報Ｗ.
009290    03 適用１Ｗ                        PIC N(38) VALUE SPACE.
009300    03 適用２Ｗ                        PIC N(38) VALUE SPACE.
009310    03 適用３Ｗ                        PIC X(40) VALUE SPACE.
009320*    03 適用４Ｗ                        PIC N(38) VALUE SPACE.
009330    03 経過コメントＷ                  PIC N(60) VALUE SPACE.
      *
       01 助成施術日Ｗ                       PIC X(150) VALUE SPACE.
       01 助成施術日ＷＲ.
          03 助成施術日ＷＰ                  PIC X(4)  VALUE SPACE OCCURS 31.
       01 日ＷＰ                             PIC Z9    VALUE ZERO.
       01 施術日文字数Ｗ                     PIC 9(3)  VALUE ZERO.
009340*
009350*****************
009360* レセプト並び順 *
009370*****************
009380 01 順番固定Ｗ                         PIC N(1) VALUE SPACE.
009390 01 順番Ｗ                             PIC 9(4) VALUE ZERO.
009400*
       01 摘要施術日Ｗ                       PIC X(100) VALUE SPACE.
       01 施術日Ｗ.
          03 施術日２Ｗ                      PIC X(1)  VALUE SPACE.
          03 施術日１Ｗ                      PIC X(1)  VALUE SPACE.
002790** 負担割合用
002800 01 負担割合Ｗ                         PIC 9(2)  VALUE ZERO.
002810 01 給付割合Ｗ                         PIC 9(2)  VALUE ZERO.
009410*******************************************************************
009420 01 印刷制御.
009430     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
009440     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
009450     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
009460     03 拡張制御Ｐ.
009470         05 端末制御Ｐ.
009480             07 移動方向Ｐ             PIC X(1) VALUE SPACE.
009490             07 移動行数Ｐ             PIC 9(3) VALUE ZERO.
009500         05 詳細制御Ｐ                 PIC X(2) VALUE SPACE.
009510     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
009520     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
009530*
009540 01 計算機西暦年Ｗ                     PIC 9(2) VALUE ZERO.
009550* 日付ＷＯＲＫ
009560 01 和暦終了年Ｗ                       PIC 9(4) VALUE ZERO.
009570 01 計算機西暦.
009580    03 計算機西暦年                    PIC 9(4) VALUE ZERO.
009590    03 計算機西暦月日                  PIC 9(4) VALUE ZERO.
009600 01 計算機西暦Ｒ REDEFINES 計算機西暦.
009610    03 計算機世紀                      PIC 9(2).
009620    03 計算機日付                      PIC 9(6).
009630    03 計算機日付Ｒ REDEFINES 計算機日付.
009640       05 計算機年月                   PIC 9(4).
009650       05 計算機年月Ｒ REDEFINES 計算機年月.
009660         07 計算機年                   PIC 9(2).
009670         07 計算機月                   PIC 9(2).
009680       05 計算機日                     PIC 9(2).
009690*
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
009700******************************************************************
009710*                          連結項目                              *
009720******************************************************************
009730**  画面入力データ
009740 01 連入－入力データ委任印刷 IS EXTERNAL.
009750    03 連入－委任印刷                     PIC 9.
014620*
       01 連入－入力データ電話印刷 IS EXTERNAL.
          03 連入－電話印刷                     PIC 9.
009190*
       01 連入－プレビュー IS EXTERNAL.
          03 連入－プレビュー区分          PIC 9.
009760*
009770** ３カ月長期判定
009780 01 連期間－キー IS EXTERNAL.
009790    03 連期間－施術年月.
009800       05 連期間－施術和暦               PIC 9.
009810       05 連期間－施術年                 PIC 9(2).
009820       05 連期間－施術月                 PIC 9(2).
009830    03  連期間－患者コード.
009840       05 連期間－患者番号               PIC 9(6).
009850       05 連期間－枝番                   PIC X.
009860    03 連期間－対象フラグ                PIC X(3).
009870    03 連期間－期間月Ｗ.
009880       05 連期間－期間Ｗ                 PIC 9(2) OCCURS 9.
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
009890************
009900* 印刷キー *
009910************
009940 01 連レ印－対象データ IS EXTERNAL.
009950    03 連レ印－施術年月日.
009960       05 連レ印－施術和暦                  PIC 9(1).
009970       05 連レ印－施術年                    PIC 9(2).
009980       05 連レ印－施術月                    PIC 9(2).
009990    03 連レ印－患者コード.
010000       05 連レ印－患者番号                  PIC 9(6).
010010       05 連レ印－枝番                      PIC X(1).
010020    03 連レ印－保険種別                     PIC 9(2).
010030    03 連レ印－保険者番号                   PIC X(10).
010040    03 連レ印－公費種別                     PIC 9(2).
010050    03 連レ印－費用負担者番号               PIC X(10).
010060    03 連レ印－助成種別                     PIC 9(2).
010070    03 連レ印－費用負担者番号助成           PIC X(10).
010080    03 連レ印－患者カナ                     PIC X(20).
010090    03 連レ印－本人家族区分                 PIC 9(1).
013520*
013530 01 連レ－キー IS EXTERNAL.
013540    03 連レ－保険種別                  PIC 9(2).
013550*
013560************************
013570* 長期理由文セット     *
013580************************
013590 01 連長文－キー IS EXTERNAL.
013600    03 連長文－施術年月.
013610       05 連長文－施術和暦               PIC 9.
013620       05 連長文－施術年                 PIC 9(2).
013630       05 連長文－施術月                 PIC 9(2).
013640    03  連長文－患者コード.
013650       05 連長文－患者番号               PIC 9(6).
013660       05 連長文－枝番                   PIC X.
013670    03 連長文－文桁数                    PIC 9(2).
013680    03 連長文－理由文                    PIC N(63) OCCURS 15.
013690*
013691************************
013692* 助成レセまとめ
013693************************
013694 01 連レセまとめ－キー IS EXTERNAL.
013695    03 連レセまとめ－施術和暦年月.
013696       05 連レセまとめ－施術和暦               PIC 9.
013697       05 連レセまとめ－施術年月.
013698          07 連レセまとめ－施術年              PIC 9(2).
013699          07 連レセまとめ－施術月              PIC 9(2).
013700    03 連レセまとめ－患者コード.
013701       05 連レセまとめ－患者番号               PIC 9(6).
013702       05 連レセまとめ－枝番                   PIC X(1).
013703**-------------------------------------------------------**
013704*   1:助成レセプトなしの本体まとめの判定
013705*   2:横浜・川崎用の社保助成レセかの判定
013706    03 連レセまとめ－判定区分                  PIC 9.
013707**-------------------------------------------------------**
013708*  / OUT /　 0:対象外、1:対象
013709    03 連レセまとめ－判定結果                  PIC 9.
013710**
013711*
013712*************
013713* 助成名称
013714*************
013715 01 連助成名称－キー IS EXTERNAL.
013716    03 連助成名称－助成種別             PIC 9(2).
013717    03 連助成名称－費用負担者番号助成   PIC X(10).
013718*   / OUT /
013719    03 連助成名称－名称集団.
013720       05 連助成名称－１文字            PIC N.
013721       05 連助成名称－略称              PIC N(4).
013722       05 連助成名称－正式名称          PIC N(10).
013723*
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
013725******************************************************************
013726*                      PROCEDURE  DIVISION                       *
013727******************************************************************
013730 PROCEDURE               DIVISION.
013740************
013750*           *
013760* 初期処理   *
013770*           *
013780************
002570     PERFORM プリンタファイル作成.
013790     PERFORM 初期化.
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
013960     MOVE ZERO  TO PROGRAM-STATUS.
013970     EXIT PROGRAM.
013980*
013990*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YCH6475"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分  TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
014000*================================================================*
014010 初期化 SECTION.
014020*
014030     PERFORM ファイルオープン.
014040*    /* 現在日付取得 */
014050     ACCEPT 計算機日付 FROM DATE.
014060*    /* 1980～2079年の間で設定 */
014070     IF ( 計算機年 > 80 )
014080         MOVE 19 TO 計算機世紀
014090     ELSE
014100         MOVE 20 TO 計算機世紀
014110     END-IF.
014120     PERFORM カレント元号取得.
014130     PERFORM 和暦終了年取得.
014140     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 和暦終了年Ｗ.
014150*================================================================*
014160 カレント元号取得 SECTION.
014170*
014180     MOVE ZEROS TO 制－制御区分.
014190     READ 制御情報マスタ
014200     NOT INVALID KEY
014210         MOVE 制－カレント元号         TO カレント元号Ｗ
014220         MOVE 制－レセ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
014230         MOVE 制－レセ長期理由印刷区分 TO 長期理由印刷区分Ｗ
014240         MOVE 制－レセプト日付区分     TO レセプト日付区分Ｗ
014250         MOVE 制－レセプト患者日付区分 TO レセプト患者日付区分Ｗ
015320         MOVE 制－老人レセ             TO 用紙種別Ｗ
014260     END-READ.
014270*
014280*================================================================*
014290 和暦終了年取得 SECTION.
014300*
014310*     DISPLAY NC"カレント元号Ｗ"  カレント元号Ｗ UPON MSGBOX.
014320     MOVE カレント元号Ｗ TO 元－元号区分.
014330     READ 元号マスタ
014340     INVALID KEY
014350         DISPLAY NC"指定和暦が登録されていません" UPON CONS
014360         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
014370                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
014380         ACCEPT  キー入力 FROM CONS
014390         PERFORM 終了処理
014400         EXIT PROGRAM
014410     NOT INVALID KEY
014420         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
014430         MOVE 前和暦Ｗ TO 元－元号区分
014440         READ 元号マスタ
014450         INVALID KEY
014460             DISPLAY NC"指定和暦が登録されていません" UPON CONS
014470             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
014480                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
014490             ACCEPT  キー入力 FROM CONS
014500             PERFORM 終了処理
014510             EXIT PROGRAM
014520         NOT INVALID KEY
014530             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
014540         END-READ
014550     END-READ.
014560*
014570*================================================================*
014580 ファイルオープン SECTION.
014590*
014600     OPEN INPUT   保険者マスタ
014610         MOVE NC"保険者" TO ファイル名.
014620         PERFORM オープンチェック.
014630     OPEN INPUT   市町村マスタ
014640         MOVE NC"市町村" TO ファイル名.
014650         PERFORM オープンチェック.
014660     OPEN INPUT   元号マスタ
014670         MOVE NC"元号" TO ファイル名.
014680         PERFORM オープンチェック.
014690     OPEN INPUT   名称マスタ
014700         MOVE NC"名称" TO ファイル名.
014710         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
014750     OPEN INPUT   制御情報マスタ
014760         MOVE NC"制御情報" TO ファイル名.
014770         PERFORM オープンチェック.
014780     OPEN INPUT   施術所情報マスタ
014790         MOVE NC"施情" TO ファイル名.
014800         PERFORM オープンチェック.
014810     OPEN INPUT   請求先マスタ
014820         MOVE NC"請先" TO ファイル名.
014830         PERFORM オープンチェック.
014840     OPEN INPUT   経過マスタ
014850         MOVE NC"経過" TO ファイル名.
014860         PERFORM オープンチェック.
014870     OPEN INPUT   施術記録Ｆ.
014880         MOVE NC"施記Ｆ" TO ファイル名.
014890         PERFORM オープンチェック.
014900     OPEN INPUT   負傷データＦ.
014910         MOVE NC"負傷" TO ファイル名.
014920         PERFORM オープンチェック.
014930     OPEN INPUT   負傷原因Ｆ.
014940         MOVE NC"負傷原因" TO ファイル名.
014950         PERFORM オープンチェック.
014960     OPEN INPUT   会情報マスタ.
014970         MOVE NC"会情報マスタ" TO ファイル名.
014980         PERFORM オープンチェック.
014990     OPEN INPUT   ＩＤ管理マスタ
015000         MOVE NC"ＩＤ" TO ファイル名.
015010         PERFORM オープンチェック.
015020     OPEN INPUT メモファイル.
015030         MOVE NC"メモ" TO ファイル名.
015040         PERFORM オープンチェック.
015050     OPEN INPUT   作業ファイル４.
015170         IF ( 状態キー  NOT =  "00" )
015060            OPEN OUTPUT  作業ファイル４
                  CLOSE 作業ファイル４
015060            OPEN INPUT  作業ファイル４
               END-IF.
015080     OPEN I-O   受診者情報Ｆ.
015090         MOVE NC"受情" TO ファイル名.
015100         PERFORM オープンチェック.
015110     OPEN I-O   印刷ファイル
015120         PERFORM エラー処理Ｐ.
015130*================================================================*
015140 オープンチェック SECTION.
015150*
015160     IF ( 状態キー  NOT =  "00" )
015170         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
015180         DISPLAY NC"状態キー：" 状態キー         UPON CONS
015190         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
015200                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015210         ACCEPT  キー入力 FROM CONS
015220         PERFORM ファイル閉鎖
015230         EXIT PROGRAM.
015240*================================================================*
015250 連結項目待避 SECTION.
015260*
015270     MOVE 連レ印－施術和暦           TO 施術和暦ＷＲ.
015280     MOVE 連レ印－施術年             TO 施術年ＷＲ.
015290     MOVE 連レ印－施術月             TO 施術月ＷＲ.
015300     MOVE 連レ印－保険種別           TO 保険種別ＷＲ.
015310     MOVE 連レ印－保険者番号         TO 保険者番号ＷＲ.
015320     MOVE 連レ印－公費種別           TO 公費種別ＷＲ.
015330     MOVE 連レ印－費用負担者番号     TO 費用負担者番号ＷＲ.
015340     MOVE 連レ印－助成種別           TO 助成種別ＷＲ.
015350     MOVE 連レ印－費用負担者番号助成 TO 費用負担者番号助成ＷＲ.
015360     MOVE 連レ印－本人家族区分       TO 本人家族区分ＷＲ.
015370     MOVE 連レ印－患者カナ           TO 患者カナＷＲ.
015380     MOVE 連レ印－患者番号           TO 患者番号ＷＲ.
015390     MOVE 連レ印－枝番               TO 枝番ＷＲ.
015400*================================================================*
015410 印刷セット SECTION.
015420*
015430     PERFORM 項目初期化.
014800     PERFORM 負傷読込.
015480     PERFORM 料金情報取得.
015440     PERFORM 施術所情報取得.
015450     PERFORM 請求先情報取得.
015460     PERFORM 受診者情報取得.
015470     PERFORM 負傷データ取得.
015490     PERFORM 施術記録取得.
015500     PERFORM レセプト並び順取得.
015510**     PERFORM ＯＣＲ情報取得.
015520**     PERFORM 長期判定取得.
015530**     PERFORM 初検日以前のデータ判定.
015540     PERFORM 初検加算時刻取得.
015550     PERFORM 助成印取得.
015560     PERFORM 委任年月日取得.
           PERFORM 施術日取得.
015570*
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
015700*
015710**********************
015720* ＯＣＲコードセット *
015730**********************
015740*
015750*     MOVE ＯＣＲ接骨師会会員番号Ｗ     TO ＯＣＲ会員番号.
015760*     MOVE ＯＣＲ施術年月Ｗ     TO ＯＣＲ施術年月.
015770*     MOVE ＯＣＲ保険種別Ｗ     TO ＯＣＲ保険種別.
015780*     MOVE ＯＣＲ各社ＩＤＷ     TO ＯＣＲ各社ＩＤ.
015790*     MOVE ＯＣＲＦＤ連番Ｗ     TO ＯＣＲＦＤ連番.
015800*     MOVE ＯＣＲ請求金額Ｗ     TO ＯＣＲ請求金額.
015810*     MOVE ＯＣＲ負担割合Ｗ     TO ＯＣＲ負担割合.
015820*
015830********************
015840* 受診者情報セット *
015850********************
015860     MOVE 施術年Ｗ            TO 施術年.
015870     MOVE 施術月Ｗ            TO 施術月.
015880*
015890*
015900*     IF ( 記号Ｗ(1:1) = NC"＊" )
015910*        MOVE  SPACE    TO  記号
015920*     ELSE
015930*        MOVE 記号Ｗ    TO  記号
015940*     END-IF.
015950*     IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
015960*        ( 印刷番号Ｗ(1:2) = "＊" )
015970*        MOVE  SPACE      TO  被保険者番号
015980*     ELSE
015990*        MOVE 印刷番号Ｗ  TO  被保険者番号
016000*     END-IF.
016010*
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
016020**
016030     MOVE 印刷保険者番号Ｗ    TO 保険者番号.
016060**
016070*     MOVE 請求先名称Ｗ        TO 請求先名称.
016080*     MOVE 請求先名称Ｗ        TO 保険者名称.
016090*     MOVE 被保険者カナＷ      TO 被保険者カナ.
016100     MOVE 被保険者氏名Ｗ      TO 被保険者氏名.
016110*     MOVE 郵便番号１Ｗ        TO 郵便番号１.
016120*     MOVE 郵便番号２Ｗ        TO 郵便番号２.
016130*     MOVE "-"                 TO 郵便区切.
016140     MOVE 被保険者住所１Ｗ    TO 住所１.
016150     MOVE 被保険者住所２Ｗ    TO 住所２.
016160**     MOVE 被保険者住所Ｗ      TO 住所.
016170*     MOVE 患者住所Ｗ          TO 住所.
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
016170*     MOVE 患者住所１Ｗ          TO 住所１.
016170*     MOVE 患者住所２Ｗ          TO 住所２.
016180*     MOVE 患者カナＷ          TO 患者カナ.
016190     MOVE 患者氏名Ｗ          TO 患者氏名.
016200     MOVE 男チェックＷ        TO 男チェック.
016210     MOVE 女チェックＷ        TO 女チェック.
016220*     MOVE 性別Ｗ               TO 性別.
016230     MOVE 明治チェックＷ      TO 明治チェック.
016240     MOVE 大正チェックＷ      TO 大正チェック.
016250     MOVE 昭和チェックＷ      TO 昭和チェック.
016260*     MOVE 平成チェックＷ      TO 平成チェック.
016270*     MOVE 元号Ｗ              TO 元号.
      */元号修正↓↓↓/20190408
           MOVE 令和ＣＭＷ         TO 令和ＣＭ.
023070     MOVE 令和チェックＷ     TO 令和チェック.
017390*     MOVE 元号Ｗ              TO 患者和暦.
      */元号修正↑↑↑/20190408
016280     MOVE 患者年Ｗ            TO 患者年.
016290     MOVE 患者月Ｗ            TO 患者月.
016300     MOVE 患者日Ｗ            TO 患者日.
016310*     MOVE 印刷続柄Ｗ          TO 続柄.
016320     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
016330     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
016340     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
016350     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
016360     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
016480     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
016370*
016380     MOVE 助成印Ｗ            TO 助成印.
016390*
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
               IF ( 助成市町村番号Ｗ(1:2) NOT = "99" )
                   MOVE 助成市町村番号Ｗ TO 公費負担者番号
               END-IF
               IF ( 助成受給者番号Ｗ(1:1) = "*"  ) OR
                  ( 助成受給者番号Ｗ(1:2) = "＊" )
                   MOVE SPACE            TO 受給者番号
               ELSE
                   MOVE 助成受給者番号Ｗ TO 受給者番号
               END-IF
           END-IF.
016400********************
016410* 長期理由文セット *
016420********************
016430*     MOVE 連長文－理由文(1)    TO 長期理由文１.
016440*     MOVE 連長文－理由文(2)    TO 長期理由文２.
016450*     MOVE 連長文－理由文(3)    TO 長期理由文３.
016460*     MOVE 連長文－理由文(4)    TO 長期理由文４.
016470*     MOVE 連長文－理由文(5)    TO 長期理由文５.
016480*     MOVE 連長文－理由文(6)    TO 長期理由文６.
016490*     MOVE 連長文－理由文(7)    TO 長期理由文７.
015780*     IF ( 連長文－理由文(1)  NOT = SPACE )
015790*        MOVE NC"（長期理由）"  TO 長期理由固定
015800*     END-IF.
016530********************
016540* 負傷データセット *
016550********************
016560* １部位 *
016570**********
016580     MOVE 負傷名Ｗ(1)       TO 負傷名１.
016590     MOVE 負傷年Ｗ(1)       TO 負傷年１.
016600     MOVE 負傷月Ｗ(1)       TO 負傷月１.
016610     MOVE 負傷日Ｗ(1)       TO 負傷日１.
016620     MOVE 初検年Ｗ(1)       TO 初検年１.
016630     MOVE 初検月Ｗ(1)       TO 初検月１.
016640     MOVE 初検日Ｗ(1)       TO 初検日１.
016650     MOVE 開始年Ｗ(1)       TO 開始年１.
016660     MOVE 開始月Ｗ(1)       TO 開始月１.
016670     MOVE 開始日Ｗ(1)       TO 開始日１.
016680     MOVE 終了年Ｗ(1)       TO 終了年１.
016690     MOVE 終了月Ｗ(1)       TO 終了月１.
016700     MOVE 終了日Ｗ(1)       TO 終了日１.
016710     MOVE 実日数Ｗ(1)       TO 実日数１.
016720     MOVE 治癒チェックＷ(1) TO 治癒チェック１.
016730     MOVE 中止チェックＷ(1) TO 中止チェック１.
016740     MOVE 転医チェックＷ(1) TO 転医チェック１.
016750**********
016760* ２部位 *
016770**********
016780     MOVE 負傷名Ｗ(2)       TO 負傷名２.
016790     MOVE 負傷年Ｗ(2)       TO 負傷年２.
016800     MOVE 負傷月Ｗ(2)       TO 負傷月２.
016810     MOVE 負傷日Ｗ(2)       TO 負傷日２.
016820     MOVE 初検年Ｗ(2)       TO 初検年２.
016830     MOVE 初検月Ｗ(2)       TO 初検月２.
016840     MOVE 初検日Ｗ(2)       TO 初検日２.
016850     MOVE 開始年Ｗ(2)       TO 開始年２.
016860     MOVE 開始月Ｗ(2)       TO 開始月２.
016870     MOVE 開始日Ｗ(2)       TO 開始日２.
016880     MOVE 終了年Ｗ(2)       TO 終了年２.
016890     MOVE 終了月Ｗ(2)       TO 終了月２.
016900     MOVE 終了日Ｗ(2)       TO 終了日２.
016910     MOVE 実日数Ｗ(2)       TO 実日数２.
016920     MOVE 治癒チェックＷ(2) TO 治癒チェック２.
016930     MOVE 中止チェックＷ(2) TO 中止チェック２.
016940     MOVE 転医チェックＷ(2) TO 転医チェック２.
016950**********
016960* ３部位 *
016970**********
016980     MOVE 負傷名Ｗ(3)       TO 負傷名３.
016990     MOVE 負傷年Ｗ(3)       TO 負傷年３.
017000     MOVE 負傷月Ｗ(3)       TO 負傷月３.
017010     MOVE 負傷日Ｗ(3)       TO 負傷日３.
017020     MOVE 初検年Ｗ(3)       TO 初検年３.
017030     MOVE 初検月Ｗ(3)       TO 初検月３.
017040     MOVE 初検日Ｗ(3)       TO 初検日３.
017050     MOVE 開始年Ｗ(3)       TO 開始年３.
017060     MOVE 開始月Ｗ(3)       TO 開始月３.
017070     MOVE 開始日Ｗ(3)       TO 開始日３.
017080     MOVE 終了年Ｗ(3)       TO 終了年３.
017090     MOVE 終了月Ｗ(3)       TO 終了月３.
017100     MOVE 終了日Ｗ(3)       TO 終了日３.
017110     MOVE 実日数Ｗ(3)       TO 実日数３.
017120     MOVE 治癒チェックＷ(3) TO 治癒チェック３.
017130     MOVE 中止チェックＷ(3) TO 中止チェック３.
017140     MOVE 転医チェックＷ(3) TO 転医チェック３.
017150**********
017160* ４部位 *
017170**********
017180     MOVE 負傷名Ｗ(4)       TO 負傷名４.
017190     MOVE 負傷年Ｗ(4)       TO 負傷年４.
017200     MOVE 負傷月Ｗ(4)       TO 負傷月４.
017210     MOVE 負傷日Ｗ(4)       TO 負傷日４.
017220     MOVE 初検年Ｗ(4)       TO 初検年４.
017230     MOVE 初検月Ｗ(4)       TO 初検月４.
017240     MOVE 初検日Ｗ(4)       TO 初検日４.
017250     MOVE 開始年Ｗ(4)       TO 開始年４.
017260     MOVE 開始月Ｗ(4)       TO 開始月４.
017270     MOVE 開始日Ｗ(4)       TO 開始日４.
017280     MOVE 終了年Ｗ(4)       TO 終了年４.
017290     MOVE 終了月Ｗ(4)       TO 終了月４.
017300     MOVE 終了日Ｗ(4)       TO 終了日４.
017310     MOVE 実日数Ｗ(4)       TO 実日数４.
017320     MOVE 治癒チェックＷ(4) TO 治癒チェック４.
017330     MOVE 中止チェックＷ(4) TO 中止チェック４.
017340     MOVE 転医チェックＷ(4) TO 転医チェック４.
017350**********
017360* ５部位 *
017370**********
017380     MOVE 負傷名Ｗ(5)       TO 負傷名５.
017390     MOVE 負傷年Ｗ(5)       TO 負傷年５.
017400     MOVE 負傷月Ｗ(5)       TO 負傷月５.
017410     MOVE 負傷日Ｗ(5)       TO 負傷日５.
017420     MOVE 初検年Ｗ(5)       TO 初検年５.
017430     MOVE 初検月Ｗ(5)       TO 初検月５.
017440     MOVE 初検日Ｗ(5)       TO 初検日５.
017450     MOVE 開始年Ｗ(5)       TO 開始年５.
017460     MOVE 開始月Ｗ(5)       TO 開始月５.
017470     MOVE 開始日Ｗ(5)       TO 開始日５.
017480     MOVE 終了年Ｗ(5)       TO 終了年５.
017490     MOVE 終了月Ｗ(5)       TO 終了月５.
017500     MOVE 終了日Ｗ(5)       TO 終了日５.
017510     MOVE 実日数Ｗ(5)       TO 実日数５.
017520     MOVE 治癒チェックＷ(5) TO 治癒チェック５.
017530     MOVE 中止チェックＷ(5) TO 中止チェック５.
017540     MOVE 転医チェックＷ(5) TO 転医チェック５.
017550**************
017560* 経過セット *
017570**************
017580     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
017590***             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
017600             UNTIL ( 部位ＣＮＴ > 5 )
017610*         MOVE 部位ＣＮＴＷ(部位ＣＮＴ)   TO 経過部位ＣＮＴ(部位ＣＮＴ)
017620*         MOVE 部位区切Ｗ(部位ＣＮＴ)     TO 部位区切(部位ＣＮＴ)
017630         MOVE 印刷経過略称Ｗ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
017640     END-PERFORM.
017650*****************************************
017660*     新規・継続チェックについて        *
017670*   ●新規...初検有り ●継続...初検なし *
017680*****************************************
017690     MOVE 新規チェックＷ    TO 新規チェック.
017700     MOVE 継続チェックＷ    TO 継続チェック.
017710********************
017720* 料金データセット *
017730********************
017740*    ****************************************************************
017750*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
017760*    ****************************************************************
017770     MOVE 初検料ＷＲ                   TO  初検料.
017780     MOVE 時間外チェックＷ             TO  時間外チェック.
017790     MOVE 休日チェックＷ               TO  休日チェック.
017800     MOVE 深夜チェックＷ               TO  深夜チェック.
017810     MOVE 初検加算料ＷＲ               TO  初検加算料.
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
017820     MOVE 再検料ＷＲ                   TO  再検料.
017830     MOVE 往療距離ＷＲ                 TO  往療距離.
017840     MOVE 往療回数ＷＲ                 TO  往療回数.
017850     MOVE 往療料ＷＲ                   TO  往療料.
017860     MOVE 夜間チェックＷ               TO  夜間チェック.
017870     MOVE 暴風雨雪チェックＷ           TO  暴風雨雪チェック.
017880     MOVE 往療加算料ＷＲ               TO  往療加算料.
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
017920     MOVE 金属副子加算料ＷＲ           TO  金属副子加算料.
017930     MOVE 施術情報提供料ＷＲ           TO  施術情報提供料.
017940     MOVE 小計Ｗ                       TO 小計.
017950********************
017960* 初回処置料セット *
017970********************
017980     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
017990***             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
018000             UNTIL ( 部位ＣＮＴ > 5 )
018010         MOVE 初回処置料ＷＲ(部位ＣＮＴ) TO 初回処置料(部位ＣＮＴ)
018020     END-PERFORM.
018030     MOVE 初回処置料合計Ｗ         TO 初回処置料合計
018040*
018050     MOVE 施療料チェックＷ            TO 施療料チェック.
018060     MOVE 整復料チェックＷ            TO 整復料チェック.
018070     MOVE 固定料チェックＷ            TO 固定料チェック.
018080********************
018090* 逓減毎料金セット *
018100********************
018110*    **********
018120*    * １部位 *
018130*    **********
018140     MOVE 後療単価１ＷＲ             TO 後療単価１.
018150     MOVE 後療回数１ＷＲ             TO 後療回数１.
018160     MOVE 後療料１ＷＲ               TO 後療料１.
018170     MOVE 冷罨法回数１ＷＲ           TO 冷罨法回数１.
018180     MOVE 冷罨法料１ＷＲ             TO 冷罨法料１.
018190     MOVE 温罨法回数１ＷＲ           TO 温罨法回数１.
018200     MOVE 温罨法料１ＷＲ             TO 温罨法料１.
018210     MOVE 電療回数１ＷＲ             TO 電療回数１.
018220     MOVE 電療料１ＷＲ               TO 電療料１.
018230     MOVE 小計１ＷＲ                 TO 小計１.
018240     IF ( 長期逓減率１ＷＲ NOT = ZERO )
018250         COMPUTE 長期逓減率１ = 長期逓減率１ＷＲ / 100
018260     END-IF.
018270     MOVE 長期込小計１ＷＲ           TO 長期込小計１.
018280*    **********
018290*    * ２部位 *
018300*    **********
018310     MOVE 後療単価２ＷＲ             TO 後療単価２.
018320     MOVE 後療回数２ＷＲ             TO 後療回数２.
018330     MOVE 後療料２ＷＲ               TO 後療料２.
018340     MOVE 冷罨法回数２ＷＲ           TO 冷罨法回数２.
018350     MOVE 冷罨法料２ＷＲ             TO 冷罨法料２.
018360     MOVE 温罨法回数２ＷＲ           TO 温罨法回数２.
018370     MOVE 温罨法料２ＷＲ             TO 温罨法料２.
018380     MOVE 電療回数２ＷＲ             TO 電療回数２.
018390     MOVE 電療料２ＷＲ               TO 電療料２.
018400     MOVE 小計２ＷＲ                 TO 小計２.
018410     IF ( 長期逓減率２ＷＲ NOT = ZERO )
018420         COMPUTE 長期逓減率２ = 長期逓減率２ＷＲ / 100
018430     END-IF.
018440     MOVE 長期込小計２ＷＲ           TO 長期込小計２.
018450*    ****************
018460*    * ３部位／８割 *
018470*    ****************
018480     MOVE 後療単価３８ＷＲ             TO 後療単価３８.
018490     MOVE 後療回数３８ＷＲ             TO 後療回数３８.
018500     MOVE 後療料３８ＷＲ               TO 後療料３８.
018510     MOVE 冷罨法回数３８ＷＲ           TO 冷罨法回数３８.
018520     MOVE 冷罨法料３８ＷＲ             TO 冷罨法料３８.
018530     MOVE 温罨法回数３８ＷＲ           TO 温罨法回数３８.
018540     MOVE 温罨法料３８ＷＲ             TO 温罨法料３８.
018550     MOVE 電療回数３８ＷＲ             TO 電療回数３８.
018560     MOVE 電療料３８ＷＲ               TO 電療料３８.
018570     MOVE 小計３８ＷＲ                 TO 小計３８.
018580     MOVE 多部位込小計３８ＷＲ         TO 多部位込小計３８.
018590     IF ( 長期逓減率３８ＷＲ NOT = ZERO )
018600         COMPUTE 長期逓減率３８ = 長期逓減率３８ＷＲ / 100
018610     END-IF.
018620     MOVE 長期込小計３８ＷＲ           TO 長期込小計３８.
      */ 逓減率 0.7→0.6 /42505
           IF (施術和暦年月ＷＲ >= 42505)
              MOVE "60"                      TO 逓減３８
              MOVE "0.6"                     TO 多部位３８
           END-IF.
018630*    ****************
018640*    * ３部位／10割 *
018650*    ****************
018660     MOVE 逓減開始月３０ＷＲ           TO 逓減開始月３０.
018670     MOVE 逓減開始日３０ＷＲ           TO 逓減開始日３０.
018680     MOVE 後療単価３０ＷＲ             TO 後療単価３０.
018690     MOVE 後療回数３０ＷＲ             TO 後療回数３０.
018700     MOVE 後療料３０ＷＲ               TO 後療料３０.
018710     MOVE 冷罨法回数３０ＷＲ           TO 冷罨法回数３０.
018720     MOVE 冷罨法料３０ＷＲ             TO 冷罨法料３０.
018730     MOVE 温罨法回数３０ＷＲ           TO 温罨法回数３０.
018740     MOVE 温罨法料３０ＷＲ             TO 温罨法料３０.
018750     MOVE 電療回数３０ＷＲ             TO 電療回数３０.
018760     MOVE 電療料３０ＷＲ               TO 電療料３０.
018770     MOVE 小計３０ＷＲ                 TO 小計３０.
018780     IF ( 長期逓減率３０ＷＲ NOT = ZERO )
018790         COMPUTE 長期逓減率３０ = 長期逓減率３０ＷＲ / 100
018800     END-IF.
018810     MOVE 長期込小計３０ＷＲ           TO 長期込小計３０.
018820*    ****************
018830*    * ４部位／５割 *
018840*    ****************
018850*     MOVE 後療単価４５ＷＲ             TO 後療単価４５.
018860*     MOVE 後療回数４５ＷＲ             TO 後療回数４５.
018870*     MOVE 後療料４５ＷＲ               TO 後療料４５.
018880*     MOVE 冷罨法回数４５ＷＲ           TO 冷罨法回数４５.
018890*     MOVE 冷罨法料４５ＷＲ             TO 冷罨法料４５.
018900*     MOVE 温罨法回数４５ＷＲ           TO 温罨法回数４５.
018910*     MOVE 温罨法料４５ＷＲ             TO 温罨法料４５.
018920*     MOVE 電療回数４５ＷＲ             TO 電療回数４５.
018930*     MOVE 電療料４５ＷＲ               TO 電療料４５.
018940*     MOVE 小計４５ＷＲ                 TO 小計４５.
018950*     MOVE 多部位込小計４５ＷＲ         TO 多部位込小計４５.
018960*     IF ( 長期逓減率４５ＷＲ NOT = ZERO )
018970*         COMPUTE 長期逓減率４５ = 長期逓減率４５ＷＲ / 100
018980*     END-IF.
018990*     MOVE 長期込小計４５ＷＲ           TO 長期込小計４５.
019000*    ****************
019010*    * ４部位／８割 *
019020*    ****************
019030     MOVE 逓減開始月４８ＷＲ           TO 逓減開始月４８.
019040     MOVE 逓減開始日４８ＷＲ           TO 逓減開始日４８.
019050     MOVE 後療単価４８ＷＲ             TO 後療単価４８.
019060     MOVE 後療回数４８ＷＲ             TO 後療回数４８.
019070     MOVE 後療料４８ＷＲ               TO 後療料４８.
019080     MOVE 冷罨法回数４８ＷＲ           TO 冷罨法回数４８.
019090     MOVE 冷罨法料４８ＷＲ             TO 冷罨法料４８.
019100     MOVE 温罨法回数４８ＷＲ           TO 温罨法回数４８.
019110     MOVE 温罨法料４８ＷＲ             TO 温罨法料４８.
019120     MOVE 電療回数４８ＷＲ             TO 電療回数４８.
019130     MOVE 電療料４８ＷＲ               TO 電療料４８.
019140     MOVE 小計４８ＷＲ                 TO 小計４８.
019150     MOVE 多部位込小計４８ＷＲ         TO 多部位込小計４８.
019160     IF ( 長期逓減率４８ＷＲ NOT = ZERO )
019170         COMPUTE 長期逓減率４８ = 長期逓減率４８ＷＲ / 100
019180     END-IF.
019190     MOVE 長期込小計４８ＷＲ           TO 長期込小計４８.
      */ 逓減率 0.7→0.6 /42505
           IF (施術和暦年月ＷＲ >= 42505)
              MOVE "60"                      TO 逓減４８
              MOVE "0.6"                     TO 多部位４８
           END-IF.
019200*    ****************
019210*    * ４部位／10割 *
019220*    ****************
019230     MOVE 逓減開始月４０ＷＲ           TO 逓減開始月４０.
019240     MOVE 逓減開始日４０ＷＲ           TO 逓減開始日４０.
019250     MOVE 後療単価４０ＷＲ             TO 後療単価４０.
019260     MOVE 後療回数４０ＷＲ             TO 後療回数４０.
019270     MOVE 後療料４０ＷＲ               TO 後療料４０.
019280     MOVE 冷罨法回数４０ＷＲ           TO 冷罨法回数４０.
019290     MOVE 冷罨法料４０ＷＲ             TO 冷罨法料４０.
019300     MOVE 温罨法回数４０ＷＲ           TO 温罨法回数４０.
019310     MOVE 温罨法料４０ＷＲ             TO 温罨法料４０.
019320     MOVE 電療回数４０ＷＲ             TO 電療回数４０.
019330     MOVE 電療料４０ＷＲ               TO 電療料４０.
019340     MOVE 小計４０ＷＲ                 TO 小計４０.
019350     IF ( 長期逓減率４０ＷＲ NOT = ZERO )
019360         COMPUTE 長期逓減率４０ = 長期逓減率４０ＷＲ / 100
019370     END-IF.
019380     MOVE 長期込小計４０ＷＲ           TO 長期込小計４０.
019390*
019400*↓***********************************************************************
019410* ５部位／2.5割の印字は必要ない。
019420*------------------------------------------------------------------------*
019430*    *****************
019440*    * ５部位／2.5割 *
019450*    *****************
019460*     MOVE 後療単価５２ＷＲ             TO 後療単価５２.
019470*     MOVE 後療回数５２ＷＲ             TO 後療回数５２.
019480*     MOVE 後療料５２ＷＲ               TO 後療料５２.
019490*     MOVE 冷罨法回数５２ＷＲ           TO 冷罨法回数５２.
019500*     MOVE 冷罨法料５２ＷＲ             TO 冷罨法料５２.
019510*     MOVE 温罨法回数５２ＷＲ           TO 温罨法回数５２.
019520*     MOVE 温罨法料５２ＷＲ             TO 温罨法料５２.
019530*     MOVE 電療回数５２ＷＲ             TO 電療回数５２.
019540*     MOVE 電療料５２ＷＲ               TO 電療料５２.
019550*     MOVE 小計５２ＷＲ                 TO 小計５２.
019560*     MOVE 多部位込小計５２ＷＲ         TO 多部位込小計５２.
019570*     IF ( 長期逓減率５２ＷＲ NOT = ZERO )
019580*         COMPUTE 長期逓減率５２ = 長期逓減率５２ＷＲ / 100
019590*     END-IF.
019600*     MOVE 長期込小計５２ＷＲ           TO 長期込小計５２.
019610*↑***********************************************************************
019620*
019630*    ****************
019640*    * ５部位／５割 *
019650*    ****************
019660*     MOVE 逓減開始月５５ＷＲ           TO 逓減開始月５５.
019670*     MOVE 逓減開始日５５ＷＲ           TO 逓減開始日５５.
019680*     MOVE 後療単価５５ＷＲ             TO 後療単価５５.
019690*     MOVE 後療回数５５ＷＲ             TO 後療回数５５.
019700*     MOVE 後療料５５ＷＲ               TO 後療料５５.
019710*     MOVE 冷罨法回数５５ＷＲ           TO 冷罨法回数５５.
019720*     MOVE 冷罨法料５５ＷＲ             TO 冷罨法料５５.
019730*     MOVE 温罨法回数５５ＷＲ           TO 温罨法回数５５.
019740*     MOVE 温罨法料５５ＷＲ             TO 温罨法料５５.
019750*     MOVE 電療回数５５ＷＲ             TO 電療回数５５.
019760*     MOVE 電療料５５ＷＲ               TO 電療料５５.
019770*     MOVE 小計５５ＷＲ                 TO 小計５５.
019780*     MOVE 多部位込小計５５ＷＲ         TO 多部位込小計５５.
019790*     IF ( 長期逓減率５５ＷＲ NOT = ZERO )
019800*         COMPUTE 長期逓減率５５ = 長期逓減率５５ＷＲ / 100
019810*     END-IF.
019820*     MOVE 長期込小計５５ＷＲ           TO 長期込小計５５.
019830*    ****************
019840*    * ５部位／８割 *
019850*    ****************
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
020030*    ****************
020040*    * ５部位／10割 *
020050*    ****************
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
020230*
020240     MOVE 適用１Ｗ                       TO 適用１.
020250     MOVE 適用２Ｗ                       TO 適用２.
020260     MOVE 適用３Ｗ                       TO 適用３.
020270*     MOVE 適用４Ｗ                       TO 適用４.
      *
      */金属副子・運動後療の変更・追加/1805
           IF ( 施術和暦年月ＷＲ >= 43006 )
              INITIALIZE 連金運－キー
019550        MOVE 施術和暦ＷＲ TO 連金運－施術和暦
019560        MOVE 施術年ＷＲ   TO 連金運－施術年
019570        MOVE 施術月ＷＲ   TO 連金運－施術月
019580        MOVE 患者番号ＷＲ TO 連金運－患者番号
019590        MOVE 枝番ＷＲ     TO 連金運－枝番
              MOVE 公費種別ＷＲ TO 連金運－保険種別
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
020310*
      */取り消し/160816
      **/千葉県の重度心身障害医療費助成/150703
      *     IF ((助成種別ＷＲ = 53) AND (費用負担者番号助成ＷＲ(1:4) =  "8112"))
021370*         MOVE レセ－受給者負担額        TO 受給者負担額
021380*         MOVE レセ－助成請求金額        TO 助成請求額
      *         MOVE "自己負担金額："          TO 一部負担金ＣＭ
      *         MOVE "公費負担金額："          TO 請求金額ＣＭ
      *         MOVE "円" TO 一部負担金円ＣＭ 請求金額円ＣＭ
      *     END-IF.
020320*------------------------------------------------------------------------------------*
020330* 特別（助成レセなしで、本体レセにまとめる時、金額は助成込み・適用２に助成種別印字）
020340*     IF ( 助成レセまとめフラグ = "YES" )
020350*         PERFORM 助成料金計算
020360*         MOVE 連計－費用額老人         TO 合計
020370*         MOVE 連計－負担額助成         TO 一部負担金
020380*     / 引き算する/
020390*         COMPUTE 請求金額 = 連計－費用額老人 - 連計－負担額助成
020400*
020410*/深＿夜の空白にストリングしてしまうためNOT SPACEの時は最後に転記する。
020411*         IF 助成種別略称Ｗ NOT = SPACE
020420*            IF ( 適用２Ｗ NOT = SPACE )
020430*                MOVE SPACE TO 助成種別略称Ｗ２
020440*                STRING NC"※"             DELIMITED BY SIZE
020450*                       助成種別略称Ｗ     DELIMITED BY SPACE
020460*                       INTO 助成種別略称Ｗ２
020470*                END-STRING
020480*                MOVE 助成種別略称Ｗ２ TO 適用２(35:4)
020490*            ELSE
020500*                STRING 適用２Ｗ           DELIMITED BY SPACE
020510*                       NC"※"             DELIMITED BY SIZE
020520*                       助成種別略称Ｗ     DELIMITED BY SPACE
020530*                       INTO 適用２
020540*                END-STRING
020550*            END-IF
020551*         END-IF
020560*     END-IF.
020570*------------------------------------------------------------------------------------*
020571*
      */新用紙対応済み/080407
020572**------------------------------------------------------------------------*
020573** 平成18年6月から温罨法75円に変更のため、訂正印字する
020574**
020575*     IF ( 施術和暦年月ＷＲ >= 41806 )
020576**
020577*        IF ( 金属副子加算料ＷＲ NOT = ZERO )
020578*           MOVE NC"副子等加算"   TO 副子訂正
020579*        END-IF
020580**
020581*        IF ( 温罨法料１ＷＲ   NOT = ZERO )  OR 
020582*           ( 温罨法料２ＷＲ   NOT = ZERO )  OR 
020583*           ( 温罨法料３８ＷＲ NOT = ZERO )  OR 
020584*           ( 温罨法料３０ＷＲ NOT = ZERO )  OR 
020585*           ( 温罨法料４５ＷＲ NOT = ZERO )  OR 
020586*           ( 温罨法料４８ＷＲ NOT = ZERO )  OR 
020587*           ( 温罨法料４０ＷＲ NOT = ZERO )  OR 
020588*           ( 温罨法料５５ＷＲ NOT = ZERO )  OR 
020589*           ( 温罨法料５８ＷＲ NOT = ZERO )  OR 
020590*           ( 温罨法料５０ＷＲ NOT = ZERO ) 
020591*           MOVE "75円"   TO 温庵訂正金額
020592*        END-IF
020593*     END-IF.
020594*
020595*------------------------------------------------------------------------*
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
                    MOVE 助成受給者番号Ｗ     TO 受給者番号
                 END-IF
                 MOVE SPACE                TO 受給者番号１
                 MOVE SPACE                TO 受給者番号３
                 PERFORM 助成施術日取得
                 MOVE 助成施術日Ｗ       TO 助成施術日
              END-IF
           END-IF.
020596*
020597*
020598**********************
020600* 施術所データセット *
020610**********************
           MOVE 都道府県ＪＩＳＷ       TO 都道府県番号.
020620     MOVE 柔整師番号Ｗ           TO 柔整師番号.
020630*     MOVE 定額制受理番号Ｗ       TO 定額制受理番号.
020640     MOVE 施術所郵便番号１Ｗ     TO 施術所郵便番号１.
020650     MOVE 施術所郵便番号２Ｗ     TO 施術所郵便番号２.
020670     MOVE 施術所住所１Ｗ         TO 施術所住所１.
020680     MOVE 施術所住所２Ｗ         TO 施術所住所２.
020690     MOVE 接骨師会会員番号Ｗ     TO 接骨師会会員番号.
020700     MOVE 代表者カナＷ           TO 代表者カナ.
020710     MOVE 代表者名Ｗ             TO 代表者名.
020720     MOVE 接骨師会会員番号Ｗ     TO 接骨師会会員番号.
020730     MOVE 施術所電話番号Ｗ       TO 施術所電話番号.
020740*
020750     MOVE 接骨院名Ｗ             TO 接骨院名.
020760*
020770*     MOVE 銀行名支店名Ｗ         TO 銀行名支店名.
020780*     MOVE 預金種別コメントＷ     TO 預金種別.
020790     MOVE 口座番号Ｗ             TO 口座番号.
020800     MOVE 口座名義人カナＷ       TO 口座名義人カナ.
020810*     MOVE 口座名義人とカナＷ     TO 口座名義人.
020810     MOVE 口座名義人Ｗ           TO 口座名義人.
020820     MOVE コメント１Ｗ           TO コメント１.
020830     MOVE コメント２Ｗ           TO コメント２.
020840     MOVE コメント３Ｗ           TO コメント３.
020850     MOVE コメント４Ｗ           TO コメント４.
020860*     MOVE コメント５Ｗ           TO コメント５.
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
020870*
020880* / 柔整師・患者委任日 /
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
020890     MOVE 柔整師年Ｗ             TO 受理年.
020900     MOVE 柔整師月Ｗ             TO 受理月.
020910     MOVE 柔整師日Ｗ             TO 受理日.
020920* ( 委任年月日 印刷するか )
020930     IF ( 連入－委任印刷  = ZERO )
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
020940         MOVE 患者委任年Ｗ       TO 委任年
020950         MOVE 患者委任月Ｗ       TO 委任月
020960         MOVE 患者委任日Ｗ       TO 委任日
020970     END-IF.
020980*
020990* 施術ID
021000     MOVE 県施術ＩＤＷ           TO 県施術ＩＤ.
021010*
021020************************
021030* レセプト並び順セット *
021040************************
021050     MOVE 順番固定Ｗ          TO 順番固定.
021060     MOVE 順番Ｗ              TO 順番.
021070     MOVE 患者番号ＷＲ        TO 患者番号.
021080     MOVE 枝番ＷＲ            TO 枝番.
021090*
021100*
021110* 負担割合コメント
021120*     MOVE 特別コメントＷ      TO 特別コメント.
021130*
021140**********     PERFORM テスト印字処理.
021170*-------------------------------------------------------------------------*
021180*--- ※ レセ摘要再セットは、この印刷セットSECTION の最後にやること！ -----*
021190     PERFORM レセ摘要再セット.
021200*-------------------------------------------------------------------------*
037573*
      */長野の国保・退職・後高の時で経過が入力されてない時は、経過を入れる/160610
           IF ((公費種別ＷＲ = 05) AND (費用負担者番号ＷＲ(3:2) = "20"))
016020        PERFORM 経過取得
           END-IF.
018770     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018790             UNTIL ( 部位ＣＮＴ > 5 )
018820         MOVE 印刷経過略称Ｗ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
018830     END-PERFORM.
021210*
021220*================================================================*
021230 項目初期化 SECTION.
021240*
021250     INITIALIZE 施術所情報Ｗ.
021260     INITIALIZE 受診者情報Ｗ.
021270     INITIALIZE 負傷情報Ｗ.
021280     INITIALIZE 備考情報Ｗ.
021290*     INITIALIZE ＯＣＲコードＷ.
021300     INITIALIZE 料金１ＷＲ.
021310     INITIALIZE 料金２ＷＲ.
021320     INITIALIZE 料金３ＷＲ.
021340     INITIALIZE YCH6125P.
021330     MOVE SPACE TO YCH6125P.
021350*================================================================*
021360 料金情報取得 SECTION.
021370*
           MOVE 2            TO レセ－レセ種別.
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
021380********************
021390* 料金データセット *
021400********************
021410*    ****************************************************************
021420*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
021430*    ****************************************************************
021440     MOVE レセ－初検料                 TO 初検料ＷＲ.
021450     IF ( レセ－時間外 = 1 )
021460         MOVE NC"○"                   TO 時間外チェックＷ
021470     END-IF.
021480     IF ( レセ－休日 = 1 )
021490         MOVE NC"○"                   TO 休日チェックＷ
021500     END-IF.
021510     IF ( レセ－深夜 = 1 )
021520         MOVE NC"○"                   TO 深夜チェックＷ
021530     END-IF.
021540*
021550     MOVE レセ－初検加算料             TO  初検加算料ＷＲ.
           MOVE レセ－初検時相談料           TO  初検時相談料ＷＲ.
021560     MOVE レセ－再検料                 TO  再検料ＷＲ.
021570     MOVE レセ－往療距離               TO  往療距離ＷＲ.
021580     MOVE レセ－往療回数               TO  往療回数ＷＲ.
021590     MOVE レセ－往療料                 TO  往療料ＷＲ.
021600     MOVE レセ－往療加算料             TO  往療加算料ＷＲ.
021610*
021620     IF ( レセ－夜間 = 1 )
021630         MOVE NC"○"                   TO 夜間チェックＷ
021640     END-IF.
021650     IF ( レセ－暴風雨雪 = 1 )
021660         MOVE NC"○"                   TO 暴風雨雪チェックＷ
021670     END-IF.
021680*
021690     MOVE レセ－金属副子加算料         TO  金属副子加算料ＷＲ.
021700*
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
021800*
021810     MOVE レセ－施術情報提供料         TO  施術情報提供料ＷＲ.
021820* 小計
021830     MOVE レセ－小計                   TO 小計Ｗ.
021840********************
021850* 初回処置料セット *
021860********************
021870     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
021880             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
021890         MOVE レセ－初回処置料(部位ＣＮＴ) TO 初回処置料ＷＲ(部位ＣＮＴ)
021900         IF ( レセ－初回処置料(部位ＣＮＴ) NOT = ZERO )
021910            EVALUATE 負－負傷種別(部位ＣＮＴ)
021920* 捻挫・打撲・挫傷
021930            WHEN 1
021940            WHEN 2
021950            WHEN 3
021960                MOVE NC"○"       TO 施療料チェックＷ
021970* 脱臼・骨折・骨折拘縮
021980            WHEN 4
021990            WHEN 5
022000            WHEN 7
022010                MOVE NC"○"       TO 整復料チェックＷ
022020* 不全骨折・不全骨折拘縮
022030            WHEN 6
022040            WHEN 8
022050                MOVE NC"○"       TO 固定料チェックＷ
022060            END-EVALUATE
022070         END-IF
022080     END-PERFORM.
022090     MOVE レセ－初回処置料合計         TO 初回処置料合計Ｗ.
022100********************
022110* 逓減毎料金セット *
022120********************
022130*    **********
022140*    * １部位 *
022150*    **********
022160     MOVE レセ－後療単価１             TO 後療単価１ＷＲ.
022170     MOVE レセ－後療回数１             TO 後療回数１ＷＲ.
022180     MOVE レセ－後療料１               TO 後療料１ＷＲ.
022190     MOVE レセ－冷罨法回数１           TO 冷罨法回数１ＷＲ.
022200     MOVE レセ－冷罨法料１             TO 冷罨法料１ＷＲ.
022210     MOVE レセ－温罨法回数１           TO 温罨法回数１ＷＲ.
022220     MOVE レセ－温罨法料１             TO 温罨法料１ＷＲ.
022230     MOVE レセ－電療回数１             TO 電療回数１ＷＲ.
022240     MOVE レセ－電療料１               TO 電療料１ＷＲ.
022250     MOVE レセ－小計１                 TO 小計１ＷＲ.
022260     MOVE レセ－長期逓減率１           TO 長期逓減率１ＷＲ.
022270     MOVE レセ－長期込小計１           TO 長期込小計１ＷＲ.
022280*    **********
022290*    * ２部位 *
022300*    **********
022310     MOVE レセ－後療単価２             TO 後療単価２ＷＲ.
022320     MOVE レセ－後療回数２             TO 後療回数２ＷＲ.
022330     MOVE レセ－後療料２               TO 後療料２ＷＲ.
022340     MOVE レセ－冷罨法回数２           TO 冷罨法回数２ＷＲ.
022350     MOVE レセ－冷罨法料２             TO 冷罨法料２ＷＲ.
022360     MOVE レセ－温罨法回数２           TO 温罨法回数２ＷＲ.
022370     MOVE レセ－温罨法料２             TO 温罨法料２ＷＲ.
022380     MOVE レセ－電療回数２             TO 電療回数２ＷＲ.
022390     MOVE レセ－電療料２               TO 電療料２ＷＲ.
022400     MOVE レセ－小計２                 TO 小計２ＷＲ.
022410     MOVE レセ－長期逓減率２           TO 長期逓減率２ＷＲ.
022420     MOVE レセ－長期込小計２           TO 長期込小計２ＷＲ.
022430*    ****************
022440*    * ３部位／８割 *
022450*    ****************
022460     MOVE レセ－後療単価３８             TO 後療単価３８ＷＲ.
022470     MOVE レセ－後療回数３８             TO 後療回数３８ＷＲ.
022480     MOVE レセ－後療料３８               TO 後療料３８ＷＲ.
022490     MOVE レセ－冷罨法回数３８           TO 冷罨法回数３８ＷＲ.
022500     MOVE レセ－冷罨法料３８             TO 冷罨法料３８ＷＲ.
022510     MOVE レセ－温罨法回数３８           TO 温罨法回数３８ＷＲ.
022520     MOVE レセ－温罨法料３８             TO 温罨法料３８ＷＲ.
022530     MOVE レセ－電療回数３８             TO 電療回数３８ＷＲ.
022540     MOVE レセ－電療料３８               TO 電療料３８ＷＲ.
022550     MOVE レセ－小計３８                 TO 小計３８ＷＲ.
022560     MOVE レセ－多部位込小計３８         TO 多部位込小計３８ＷＲ.
022570     MOVE レセ－長期逓減率３８           TO 長期逓減率３８ＷＲ.
022580     MOVE レセ－長期込小計３８           TO 長期込小計３８ＷＲ.
022590*    ****************
022600*    * ３部位／10割 *
022610*    ****************
022620     MOVE レセ－逓減開始月３０           TO 逓減開始月３０ＷＲ.
022630     MOVE レセ－逓減開始日３０           TO 逓減開始日３０ＷＲ.
022640     MOVE レセ－後療単価３０             TO 後療単価３０ＷＲ.
022650     MOVE レセ－後療回数３０             TO 後療回数３０ＷＲ.
022660     MOVE レセ－後療料３０               TO 後療料３０ＷＲ.
022670     MOVE レセ－冷罨法回数３０           TO 冷罨法回数３０ＷＲ.
022680     MOVE レセ－冷罨法料３０             TO 冷罨法料３０ＷＲ.
022690     MOVE レセ－温罨法回数３０           TO 温罨法回数３０ＷＲ.
022700     MOVE レセ－温罨法料３０             TO 温罨法料３０ＷＲ.
022710     MOVE レセ－電療回数３０             TO 電療回数３０ＷＲ.
022720     MOVE レセ－電療料３０               TO 電療料３０ＷＲ.
022730     MOVE レセ－小計３０                 TO 小計３０ＷＲ.
022740     MOVE レセ－長期逓減率３０           TO 長期逓減率３０ＷＲ.
022750     MOVE レセ－長期込小計３０           TO 長期込小計３０ＷＲ.
022760*    ****************
022770*    * ４部位／５割 *
022780*    ****************
022790     MOVE レセ－後療単価４５             TO 後療単価４５ＷＲ.
022800     MOVE レセ－後療回数４５             TO 後療回数４５ＷＲ.
022810     MOVE レセ－後療料４５               TO 後療料４５ＷＲ.
022820     MOVE レセ－冷罨法回数４５           TO 冷罨法回数４５ＷＲ.
022830     MOVE レセ－冷罨法料４５             TO 冷罨法料４５ＷＲ.
022840     MOVE レセ－温罨法回数４５           TO 温罨法回数４５ＷＲ.
022850     MOVE レセ－温罨法料４５             TO 温罨法料４５ＷＲ.
022860     MOVE レセ－電療回数４５             TO 電療回数４５ＷＲ.
022870     MOVE レセ－電療料４５               TO 電療料４５ＷＲ.
022880     MOVE レセ－小計４５                 TO 小計４５ＷＲ.
022890     MOVE レセ－多部位込小計４５         TO 多部位込小計４５ＷＲ.
022900     MOVE レセ－長期逓減率４５           TO 長期逓減率４５ＷＲ.
022910     MOVE レセ－長期込小計４５           TO 長期込小計４５ＷＲ.
022920*    ****************
022930*    * ４部位／８割 *
022940*    ****************
022950     MOVE レセ－逓減開始月４８           TO 逓減開始月４８ＷＲ.
022960     MOVE レセ－逓減開始日４８           TO 逓減開始日４８ＷＲ.
022970     MOVE レセ－後療単価４８             TO 後療単価４８ＷＲ.
022980     MOVE レセ－後療回数４８             TO 後療回数４８ＷＲ.
022990     MOVE レセ－後療料４８               TO 後療料４８ＷＲ.
023000     MOVE レセ－冷罨法回数４８           TO 冷罨法回数４８ＷＲ.
023010     MOVE レセ－冷罨法料４８             TO 冷罨法料４８ＷＲ.
023020     MOVE レセ－温罨法回数４８           TO 温罨法回数４８ＷＲ.
023030     MOVE レセ－温罨法料４８             TO 温罨法料４８ＷＲ.
023040     MOVE レセ－電療回数４８             TO 電療回数４８ＷＲ.
023050     MOVE レセ－電療料４８               TO 電療料４８ＷＲ.
023060     MOVE レセ－小計４８                 TO 小計４８ＷＲ.
023070     MOVE レセ－多部位込小計４８         TO 多部位込小計４８ＷＲ.
023080     MOVE レセ－長期逓減率４８           TO 長期逓減率４８ＷＲ.
023090     MOVE レセ－長期込小計４８           TO 長期込小計４８ＷＲ.
023100*    ****************
023110*    * ４部位／10割 *
023120*    ****************
023130     MOVE レセ－逓減開始月４０           TO 逓減開始月４０ＷＲ.
023140     MOVE レセ－逓減開始日４０           TO 逓減開始日４０ＷＲ.
023150     MOVE レセ－後療単価４０             TO 後療単価４０ＷＲ.
023160     MOVE レセ－後療回数４０             TO 後療回数４０ＷＲ.
023170     MOVE レセ－後療料４０               TO 後療料４０ＷＲ.
023180     MOVE レセ－冷罨法回数４０           TO 冷罨法回数４０ＷＲ.
023190     MOVE レセ－冷罨法料４０             TO 冷罨法料４０ＷＲ.
023200     MOVE レセ－温罨法回数４０           TO 温罨法回数４０ＷＲ.
023210     MOVE レセ－温罨法料４０             TO 温罨法料４０ＷＲ.
023220     MOVE レセ－電療回数４０             TO 電療回数４０ＷＲ.
023230     MOVE レセ－電療料４０               TO 電療料４０ＷＲ.
023240     MOVE レセ－小計４０                 TO 小計４０ＷＲ.
023250     MOVE レセ－長期逓減率４０           TO 長期逓減率４０ＷＲ.
023260     MOVE レセ－長期込小計４０           TO 長期込小計４０ＷＲ.
023270*    *****************
023280*    * ５部位／2.5割 *
023290*    *****************
023300     MOVE レセ－後療単価５２             TO 後療単価５２ＷＲ.
023310     MOVE レセ－後療回数５２             TO 後療回数５２ＷＲ.
023320     MOVE レセ－後療料５２               TO 後療料５２ＷＲ.
023330     MOVE レセ－冷罨法回数５２           TO 冷罨法回数５２ＷＲ.
023340     MOVE レセ－冷罨法料５２             TO 冷罨法料５２ＷＲ.
023350     MOVE レセ－温罨法回数５２           TO 温罨法回数５２ＷＲ.
023360     MOVE レセ－温罨法料５２             TO 温罨法料５２ＷＲ.
023370     MOVE レセ－電療回数５２             TO 電療回数５２ＷＲ.
023380     MOVE レセ－電療料５２               TO 電療料５２ＷＲ.
023390     MOVE レセ－小計５２                 TO 小計５２ＷＲ.
023400     MOVE レセ－多部位込小計５２         TO 多部位込小計５２ＷＲ.
023410     MOVE レセ－長期逓減率５２           TO 長期逓減率５２ＷＲ.
023420     MOVE レセ－長期込小計５２           TO 長期込小計５２ＷＲ.
023430*    ****************
023440*    * ５部位／５割 *
023450*    ****************
023460     MOVE レセ－逓減開始月５５           TO 逓減開始月５５ＷＲ.
023470     MOVE レセ－逓減開始日５５           TO 逓減開始日５５ＷＲ.
023480     MOVE レセ－後療単価５５             TO 後療単価５５ＷＲ.
023490     MOVE レセ－後療回数５５             TO 後療回数５５ＷＲ.
023500     MOVE レセ－後療料５５               TO 後療料５５ＷＲ.
023510     MOVE レセ－冷罨法回数５５           TO 冷罨法回数５５ＷＲ.
023520     MOVE レセ－冷罨法料５５             TO 冷罨法料５５ＷＲ.
023530     MOVE レセ－温罨法回数５５           TO 温罨法回数５５ＷＲ.
023540     MOVE レセ－温罨法料５５             TO 温罨法料５５ＷＲ.
023550     MOVE レセ－電療回数５５             TO 電療回数５５ＷＲ.
023560     MOVE レセ－電療料５５               TO 電療料５５ＷＲ.
023570     MOVE レセ－小計５５                 TO 小計５５ＷＲ.
023580     MOVE レセ－多部位込小計５５         TO 多部位込小計５５ＷＲ.
023590     MOVE レセ－長期逓減率５５           TO 長期逓減率５５ＷＲ.
023600     MOVE レセ－長期込小計５５           TO 長期込小計５５ＷＲ.
023610*    ****************
023620*    * ５部位／８割 *
023630*    ****************
023640     MOVE レセ－逓減開始月５８           TO 逓減開始月５８ＷＲ.
023650     MOVE レセ－逓減開始日５８           TO 逓減開始日５８ＷＲ.
023660     MOVE レセ－後療単価５８             TO 後療単価５８ＷＲ.
023670     MOVE レセ－後療回数５８             TO 後療回数５８ＷＲ.
023680     MOVE レセ－後療料５８               TO 後療料５８ＷＲ.
023690     MOVE レセ－冷罨法回数５８           TO 冷罨法回数５８ＷＲ.
023700     MOVE レセ－冷罨法料５８             TO 冷罨法料５８ＷＲ.
023710     MOVE レセ－温罨法回数５８           TO 温罨法回数５８ＷＲ.
023720     MOVE レセ－温罨法料５８             TO 温罨法料５８ＷＲ.
023730     MOVE レセ－電療回数５８             TO 電療回数５８ＷＲ.
023740     MOVE レセ－電療料５８               TO 電療料５８ＷＲ.
023750     MOVE レセ－小計５８                 TO 小計５８ＷＲ.
023760     MOVE レセ－多部位込小計５８         TO 多部位込小計５８ＷＲ.
023770     MOVE レセ－長期逓減率５８           TO 長期逓減率５８ＷＲ.
023780     MOVE レセ－長期込小計５８           TO 長期込小計５８ＷＲ.
023790*    ****************
023800*    * ５部位／10割 *
023810*    ****************
023820     MOVE レセ－逓減開始月５０           TO 逓減開始月５０ＷＲ.
023830     MOVE レセ－逓減開始日５０           TO 逓減開始日５０ＷＲ.
023840     MOVE レセ－後療単価５０             TO 後療単価５０ＷＲ.
023850     MOVE レセ－後療回数５０             TO 後療回数５０ＷＲ.
023860     MOVE レセ－後療料５０               TO 後療料５０ＷＲ.
023870     MOVE レセ－冷罨法回数５０           TO 冷罨法回数５０ＷＲ.
023880     MOVE レセ－冷罨法料５０             TO 冷罨法料５０ＷＲ.
023890     MOVE レセ－温罨法回数５０           TO 温罨法回数５０ＷＲ.
023900     MOVE レセ－温罨法料５０             TO 温罨法料５０ＷＲ.
023910     MOVE レセ－電療回数５０             TO 電療回数５０ＷＲ.
023920     MOVE レセ－電療料５０               TO 電療料５０ＷＲ.
023930     MOVE レセ－小計５０                 TO 小計５０ＷＲ.
023940     MOVE レセ－長期逓減率５０           TO 長期逓減率５０ＷＲ.
023950     MOVE レセ－長期込小計５０           TO 長期込小計５０ＷＲ.
      */2022
           MOVE レセ－明細書発行加算料         TO 明細書発行加算料ＷＲ.
           MOVE レセ－明細書発行加算日         TO 明細書発行加算日ＷＲ.
           IF レセ－明細書発行加算料 NOT = ZERO
               STRING "明細書発行体制加算"     DELIMITED BY SIZE
                      明細書発行加算料ＷＲ     DELIMITED BY SIZE
                      "円 加算日"              DELIMITED BY SIZE
                      明細書発行加算日ＷＲ     DELIMITED BY SIZE
                      "日"                     DELIMITED BY SIZE
                 INTO 適用３Ｗ
               END-STRING
           END-IF.
023960*
023970*================================================================*
023980 施術所情報取得 SECTION.
023990*
024000**************************************************
024010* 本院データを使用し、以下の情報を取得           *
024020* ● 柔整師番号.. 柔整師番号Ｗに格納             *
024030* ● 会員番号 ... 接骨師会会員番号Ｗに格納       *
024040* ● 代表者名 ... 代表者名Ｗに格納               *
024050* ● 住所1,2   ...施術所住所1,2Ｗに格納          *
024060* ● 電話番号 ... 施術所電話番号Ｗに格納         *
024070**************************************************
024080     MOVE ZERO  TO 施情－施術所番号.
024090     READ 施術所情報マスタ
024100     INVALID KEY
024110         CONTINUE
024120     NOT INVALID KEY
024130*
               MOVE 施情－都道府県ＪＩＳ     TO 都道府県ＪＩＳＷ
024170         MOVE 施情－新柔整師番号       TO 柔整師番号Ｗ
024190*
024200*         MOVE 施情－接骨師会会員番号  TO ＯＣＲ接骨師会会員番号Ｗ
024210*
024220         STRING "中央-"                DELIMITED BY SIZE
024230                施情－接骨師会会員番号 DELIMITED BY SPACE
024240           INTO 接骨師会会員番号Ｗ
024250         END-STRING
024260*
024270         MOVE 施情－郵便番号１        TO 施術所郵便番号１Ｗ
024280         MOVE 施情－郵便番号２        TO 施術所郵便番号２Ｗ
024290         MOVE 施情－代表者カナ        TO 代表者カナＷ
024300         MOVE 施情－代表者名          TO 代表者名Ｗ
024310*
024320         MOVE 施情－接骨院名          TO 接骨院名Ｗ
024330*
024340*         STRING 施情－住所１  DELIMITED BY SPACE
024350*                施情－住所２  DELIMITED BY SPACE
024360*           INTO 施術所住所Ｗ
024370*         END-STRING
024380         MOVE 施情－住所１            TO 施術所住所１Ｗ
024390         MOVE 施情－住所２            TO 施術所住所２Ｗ
024410         MOVE 施情－電話番号          TO 施術所電話番号Ｗ
024420**
024430** 振込先情報
024440** 老人の市町村番号 2713XXXX(東京) の時、会情報マスタより
024450         IF ( 費用負担者番号ＷＲ(3:2) = "13" )
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
024510                 MOVE 会情－取引先銀行名      TO 取引先銀行名Ｗ
024520                 MOVE 会情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
024530                 MOVE 会情－預金種別          TO 預金種別Ｗ
024540                 MOVE 会情－口座番号          TO 口座番号Ｗ
024550*                 MOVE 会情－口座名義人        TO 口座名義人とカナＷ
024550                 MOVE 会情－口座名義人        TO 口座名義人Ｗ
024560                 MOVE 会情－口座名義人カナ    TO 口座名義人カナＷ
024570                 EVALUATE 預金種別Ｗ
024580                 WHEN 1
024590                     MOVE "(普)"  TO 預金種別コメントＸＷ
024600                 WHEN 2
024610                     MOVE "(当)"  TO 預金種別コメントＸＷ
024620                 WHEN OTHER
024630                     MOVE SPACE   TO 預金種別コメントＸＷ
024640                 END-EVALUATE
024650*
024660                 STRING 取引先銀行名Ｗ       DELIMITED BY SPACE
024670                        " "                  DELIMITED BY SIZE
024680                        取引先銀行支店名Ｗ   DELIMITED BY SPACE
024690                        "  "                 DELIMITED BY SIZE
024700                        預金種別コメントＸＷ DELIMITED BY SPACE
024710                        " "                  DELIMITED BY SIZE
024720                        会情－口座番号       DELIMITED BY SPACE
024730                        INTO 銀行名支店名Ｗ
024740                 END-STRING
024750             END-IF
024760* 固定印字
024917*             MOVE "また、私が取得した上記金額の受領を、" TO  コメント１Ｗ
024918*             MOVE "下記の(協)中央接骨師会理事長に委任"   TO  コメント２Ｗ
024919*             MOVE "します。"                             TO  コメント３Ｗ
025080             MOVE "また、私が取得した上記金額の受領を、" TO  コメント１Ｗ
025090             MOVE "(協)中央接骨師会理事長 根岸 進"       TO  コメント２Ｗ
025100             MOVE "(東京都板橋区成増2-9-5)に委任"        TO  コメント３Ｗ
025100             MOVE "します。"                             TO  コメント４Ｗ
024930** 以外
024940         ELSE
024950             MOVE 施情－取引先銀行名      TO 取引先銀行名Ｗ
024960             MOVE 施情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
024970             MOVE 施情－預金種別          TO 預金種別Ｗ
024980             MOVE 施情－口座番号          TO 口座番号Ｗ
024990             MOVE 施情－口座名義人        TO 口座名義人Ｗ
                   MOVE 施情－口座名義人カナ    TO 口座名義人カナＷ
025000             STRING 取引先銀行名Ｗ     DELIMITED BY SPACE
025010                    " "                DELIMITED BY SIZE
025020                    取引先銀行支店名Ｗ DELIMITED BY SPACE
025030                    INTO 銀行名支店名Ｗ
025040             END-STRING
025050*             STRING 口座名義人Ｗ           DELIMITED BY SPACE
025060*                    "  "                   DELIMITED BY SIZE
025070*                    施情－口座名義人カナ   DELIMITED BY SIZE
025080*                    INTO 口座名義人とカナＷ
025090*             END-STRING
025100             EVALUATE 預金種別Ｗ
025110             WHEN 1
025120                 MOVE NC"（普）" TO 預金種別コメントＷ
025130             WHEN 2
025140                 MOVE NC"（当）" TO 預金種別コメントＷ
025150             WHEN OTHER
025160                 MOVE SPACE      TO 預金種別コメントＷ
025170             END-EVALUATE
025180         END-IF
025190*
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
025200     END-READ.
025210*
025220*********************************************
025230** ＩＤ管理マスタより　県施術ＩＤを取得する。
025240*********************************************
025250** 県施術ID
025260     MOVE 01                   TO ＩＤ管－ＩＤ区分
025270     MOVE ZERO                 TO ＩＤ管－施術所番号
025280     MOVE 費用負担者番号ＷＲ(3:2)  TO ＩＤ管－保険種別
025290     MOVE SPACE                TO ＩＤ管－保険者番号
025300     READ ＩＤ管理マスタ
025310     NOT INVALID KEY
025320          MOVE ＩＤ管－施術ＩＤ番号   TO 県施術ＩＤＷ
025330     END-READ.
025340*
025350*================================================================*
025360 受診者情報取得 SECTION.
025370*
025380**************************************************
025390* 連結データから受診者情報Ｆより以下の情報を取得 *
025400* ● 施術年 ..... 施術年Ｗに格納                 *
025410* ● 施術月 ..... 施術月Ｗに格納                 *
025420* ● 患者番号.... 患者番号Ｗに格納※ＦＤ連番用   *
025430* ● 記号 ....... 記号Ｗに格納                   *
025440* ● 番号 ....... 番号Ｗに格納                   *
025450* ● 保険者番号 . 保険者番号Ｗに格納             *
025460* ● 保険種別 ... 保険種別Ｗに格納               *
025470* ● 被保険者カナ.被保険者カナＷに格納           *
025480* ● 被保険者氏名.被保険者氏名Ｗに格納           *
025490* ● 住所１ ......被保険者住所１Ｗに格納         *
025500* ● 住所２ ......被保険者住所２Ｗに格納         *
025510* ● 患者カナ ....患者カナＷに格納               *
025520* ● 患者氏名 ....患者氏名Ｗに格納               *
025530* ● 患者性別 ....区分によりチェックに"○"を格納 *
025540* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
025550* ● 患者年 ......患者年Ｗに格納                 *
025560* ● 患者月 ......患者月Ｗに格納                 *
025570* ● 患者日 ......患者日Ｗに格納                 *
025580* ● 続柄 ........名称マスタより続柄Ｗに取得     *
025590**************************************************
025600     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
025610     MOVE 施術年ＷＲ         TO 受－施術年.
025620     MOVE 施術月ＷＲ         TO 受－施術月.
025630     MOVE 患者コードＷＲ     TO 受－患者コード.
025640     READ 受診者情報Ｆ
025650     INVALID KEY
025660         CONTINUE
025670*            /* ありえない */
025680     NOT INVALID KEY
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
      */後期高齢給付割合○付け/110721
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
025690         MOVE 受－施術年       TO 施術年Ｗ
025700         MOVE 受－施術月       TO 施術月Ｗ
025710         MOVE 受－患者番号     TO 患者番号Ｗ
025720*         MOVE 受－記号         TO 記号Ｗ
025730*         MOVE 受－番号         TO 番号Ｗ
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
025740         MOVE 受－保険者番号   TO 保険者番号Ｗ
025750         MOVE 受－保険種別     TO 保険種別Ｗ
025760** 全国土木の枝番削除
025770         IF ( 受－保険種別 = 01 ) AND ( 受－保険者番号(1:6) = "133033" )
025780            MOVE 受－保険者番号(1:6)  TO 保険者番号Ｗ
025790         END-IF
025800**
025810         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
025820         MOVE 受－郵便番号１   TO 郵便番号１Ｗ
025830         MOVE 受－郵便番号２   TO 郵便番号２Ｗ
025840         MOVE 受－住所１       TO 被保険者住所１Ｗ
025850         MOVE 受－住所２       TO 被保険者住所２Ｗ
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
025820         MOVE 受－患者郵便番号１     TO 郵便番号１Ｗ
025830         MOVE 受－患者郵便番号２     TO 郵便番号２Ｗ
               MOVE 受－患者住所１         TO 患者住所１Ｗ
               MOVE 受－患者住所２         TO 患者住所２Ｗ
025940         MOVE 受－患者カナ           TO 患者カナＷ
025950         MOVE 受－患者氏名           TO 患者氏名Ｗ
025960         MOVE 受－費用負担者番号助成 TO 助成市町村番号Ｗ
037782         MOVE 受－受益者番号助成     TO 助成受給者番号Ｗ
025970         MOVE 受－費用負担者番号     TO 市町村番号Ｗ
025980         MOVE 受－受益者番号老人     TO 受給者番号Ｗ
025990*
026000         EVALUATE 受－患者性別
026010         WHEN 1
026020             MOVE NC"男"  TO 性別Ｗ
026030             MOVE NC"○"  TO 男チェックＷ
026040         WHEN 2
026050             MOVE NC"女"  TO 性別Ｗ
026060             MOVE NC"○"  TO 女チェックＷ
026070         END-EVALUATE
026080         EVALUATE 受－患者和暦
026090         WHEN 1
026100             MOVE NC"明治"  TO 元号Ｗ
026110             MOVE NC"○"    TO 明治チェックＷ
026120         WHEN 2
026130             MOVE NC"大正"  TO 元号Ｗ
026140             MOVE NC"○"    TO 大正チェックＷ
026150         WHEN 3
026160             MOVE NC"昭和"  TO 元号Ｗ
026170             MOVE NC"○"    TO 昭和チェックＷ
026180         WHEN 4
026190             MOVE NC"平成"  TO 元号Ｗ
026200             MOVE NC"○"    TO 平成チェックＷ
      */元号修正/20190408
023060         WHEN 5
                   MOVE "5令"   TO 令和ＣＭＷ
023070             MOVE NC"○"  TO 令和チェックＷ
026210         END-EVALUATE
026220*
      */元号修正/↓↓↓20190408
029310         IF 受－患者和暦 > 4
037370             MOVE 受－患者和暦     TO 元－元号区分
037380             READ 元号マスタ
037390             NOT INVALID KEY
037400                 MOVE 元－元号名称 TO 元号Ｗ
037410             END-READ
029330         END-IF
      */元号修正/↑↑↑20190408
026230         MOVE 受－患者年  TO 患者年Ｗ
026240         MOVE 受－患者月  TO 患者月Ｗ
026250         MOVE 受－患者日  TO 患者日Ｗ
026260* 続柄なし
026270*         IF ( 本人家族区分ＷＲ = 1 )
026280*             MOVE SPACE       TO 続柄Ｗ
026290*         ELSE
026300*             MOVE 05          TO 名－区分コード
026310*             MOVE 受－続柄    TO 名－名称コード
026320*             READ 名称マスタ
026330*             INVALID KEY
026340*                 MOVE SPACE    TO 続柄Ｗ
026350*             NOT INVALID KEY
026360*                 MOVE 名－略称 TO 続柄Ｗ
026370*             END-READ
026380*         END-IF
026390*
026400**
026410         IF ( 受－助成種別 NOT = ZERO )
026420            PERFORM 助成レセまとめ判定
026430         ELSE
026440            MOVE SPACE TO 助成レセまとめフラグ
026450         END-IF
               IF (受－助成種別 = 54) AND (受－費用負担者番号助成(1:2) = "19")
                   MOVE 受－費用負担者番号助成 TO 公費負担者番号 
                   MOVE 受－受益者番号助成     TO 受給者番号
               END-IF
031450* 15/7～　長野　助成
031460         IF 受－施術和暦年月 >= 41507
031470            IF ( 受－助成種別 = 52 OR 53 OR 55 ) AND
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
026460**
026470* 14/10～　特別区分コメント印字
026541         EVALUATE 受－特別区分
026542         WHEN 2
026543             MOVE "２割"   TO 特別コメントＷ
026544         WHEN 3
026545             MOVE "３割"   TO 特別コメントＷ
026546         WHEN OTHER
026547             MOVE "１割"   TO 特別コメントＷ
026548         END-EVALUATE
026570*
026580     END-READ.
026590*
026600*================================================================*
026610 請求先情報取得 SECTION.
026620*
026630****************************************************
026640* 連結データから保険者マスタより請求先を取得する。 *
026650* ※保－請求先情報区分=1の場合請求先マスタを使用   *
026660* ● 請求先...... 請求先名称Ｗに格納               *
026670****************************************************
026680     MOVE 保険種別ＷＲ       TO 市－公費種別.
026690     MOVE 保険者番号ＷＲ     TO 市－市町村番号.
026700     READ 市町村マスタ
026710     INVALID KEY
026720         MOVE SPACE          TO 請求先名称Ｗ
026730     NOT INVALID KEY
026840         MOVE 市－市町村名称  TO 請求先名称Ｗ
026860     END-READ.
026870* 
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
026880*================================================================*
026890 負傷データ取得 SECTION.
026900*
026910**************************************************
026920* 連結データから負傷データＦより以下の情報を取得 *
026930* ● 負傷名...部位＋負傷種別にて加工して格納     *
026940* ● 負傷年.......負傷年Ｗ                       *
026950* ● 負傷月.......負傷月Ｗ                       *
026960* ● 負傷日.......負傷日Ｗ                       *
026970* ● 開始年.......初検年Ｗ                       *
026980* ● 開始月.......初検月Ｗ                       *
026990* ● 開始日.......初検日Ｗ                       *
027000* ● 終了年.......終了年Ｗ                       *
027010* ● 終了月.......終了月Ｗ                       *
027020* ● 終了日.......終了日Ｗ                       *
027030* ● 実日数.......実日数Ｗ                       *
027040* ● 転帰区分 ....区分によりチェックに"○"を格納 *
027050* ● 金属副子 ....区分によりチェックに"○"を格納 *
027060* ● 経過コード...経過マスタより取得             *
027070**************************************************
027080*     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
027090*     MOVE 施術年ＷＲ         TO 負－施術年.
027100*     MOVE 施術月ＷＲ         TO 負－施術月.
027110*     MOVE 患者コードＷＲ     TO 負－患者コード.
027120*     READ 負傷データＦ
027130*     INVALID KEY
027140*         CONTINUE
027150**            /* ありえない */
027160*     NOT INVALID KEY
027170*         MOVE 負－部位数                   TO 部位数Ｗ
027180         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
027190                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
027200             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別Ｗ(部位ＣＮＴ)
027210             MOVE 負－部位(部位ＣＮＴ)     TO 部位Ｗ(部位ＣＮＴ)
027220             MOVE 負－左右区分(部位ＣＮＴ) TO 左右区分Ｗ(部位ＣＮＴ)
027230             MOVE 負－負傷位置番号(部位ＣＮＴ)
027240                                           TO 負傷位置番号Ｗ(部位ＣＮＴ)
027250*********************************************
027260* 注）全柔...負傷種別＋部位にて加工して格納 *
027270*********************************************
027280* 負傷種別
027290             MOVE SPACE                     TO 負傷名称Ｗ
027300             MOVE 03                        TO 名－区分コード
027310             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
027320             READ 名称マスタ
027330             INVALID KEY
027340                 MOVE SPACE        TO 負傷名称Ｗ
027350             NOT INVALID KEY
027360                 MOVE 名－正式名称 TO 負傷名称Ｗ
027370             END-READ
027380* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
028160*
028170             PERFORM 部位名称埋込処理
027570*
027580             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
027590             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
027600             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
027610             MOVE 負－開始年(部位ＣＮＴ)   TO 初検年Ｗ(部位ＣＮＴ)
027620             MOVE 負－開始月(部位ＣＮＴ)   TO 初検月Ｗ(部位ＣＮＴ)
027630             MOVE 負－開始日(部位ＣＮＴ)   TO 初検日Ｗ(部位ＣＮＴ)
027640             IF ( 負－転帰区分(部位ＣＮＴ) = 9 )
027650                 MOVE 99                   TO 終了年Ｗ(部位ＣＮＴ)
027660                 MOVE 99                   TO 終了月Ｗ(部位ＣＮＴ)
027670                 MOVE 99                   TO 終了日Ｗ(部位ＣＮＴ)
027680             ELSE
027690                 MOVE 負－終了年(部位ＣＮＴ)   TO 終了年Ｗ(部位ＣＮＴ)
027700                 MOVE 負－終了月(部位ＣＮＴ)   TO 終了月Ｗ(部位ＣＮＴ)
027710                 MOVE 負－終了日(部位ＣＮＴ)   TO 終了日Ｗ(部位ＣＮＴ)
027720             END-IF
027730* 経過略称取得
027740             MOVE 01                         TO 経－区分コード
027750             MOVE 負－経過コード(部位ＣＮＴ) TO 経－経過コード
027760             READ 経過マスタ
027770             INVALID KEY
027780                 MOVE ZERO            TO 部位ＣＮＴＷ(部位ＣＮＴ)
027790                 MOVE SPACE           TO 部位区切Ｗ(部位ＣＮＴ)
027800                 MOVE SPACE           TO 経過略称Ｗ(部位ＣＮＴ)
027810             NOT INVALID KEY
027820*
027830                 EVALUATE 部位ＣＮＴ
027840                 WHEN 1
027850                     MOVE NC"①" TO 経過部位Ｗ
027860                 WHEN 2
027870                     MOVE NC"②" TO 経過部位Ｗ
027880                 WHEN 3
027890                     MOVE NC"③" TO 経過部位Ｗ
027900                 WHEN 4
027910                     MOVE NC"④" TO 経過部位Ｗ
027920                 WHEN 5
027930                     MOVE NC"⑤" TO 経過部位Ｗ
027940                 END-EVALUATE
027950                 STRING  経過部位Ｗ     DELIMITED BY SPACE
027960                         経－経過略称   DELIMITED BY SPACE
027970                        INTO 印刷経過略称Ｗ(部位ＣＮＴ)
027980                 END-STRING
027990*
028000             END-READ
028010*
028020             MOVE 負－転帰区分(部位ＣＮＴ) TO 転帰区分Ｗ(部位ＣＮＴ)
028030             EVALUATE 負－転帰区分(部位ＣＮＴ)
028040             WHEN 1
028050             WHEN 2
028060                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
028070             WHEN 3
028080                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
028090             WHEN 4
028100                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
028110             END-EVALUATE
028120*
      */実日数はレセ－部位実日数を転記する/160816
031230             MOVE レセ－部位実日数(部位ＣＮＴ) TO 実日数Ｗ(部位ＣＮＴ)
028130         END-PERFORM.
028140* 新規/継続 チェック
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
028200* 枝番判定用
028210         MOVE 負－開始診療日手動区分 TO  開始診療日手動区分Ｗ.
028220*
028230* 負傷原因印刷区分
028240         MOVE 負－レセ負傷原因印刷区分 TO レセ負傷原因印刷区分Ｗ.
028370* 長期理由印刷区分
027880         MOVE 負－レセ長期理由印刷区分     TO 長期理由印刷区分Ｆ.
028250*
028260*     END-READ.
028270*================================================================*
028280*================================================================*
028290 施術記録取得 SECTION.
028300*
028310************************************************************
028320* 作１データから負傷データＦより以下の情報を取得           *
028330* ● 初検加算 .....区分によりチェックに"○"を格納...複数可 *
028340* ● 往療加算 .....区分によりチェックに"○"を格納...複数可 *
028350************************************************************
028360     MOVE  SPACE  TO  初日再検フラグ.
028370     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 部位数Ｗ
028380         IF ( 施術年Ｗ = 初検年Ｗ(部位ＣＮＴ) ) AND
028390            ( 施術月Ｗ = 初検月Ｗ(部位ＣＮＴ) )
028400             MOVE 患者番号ＷＲ          TO 施記－患者番号
028410             MOVE 枝番ＷＲ              TO 施記－枝番
028420             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
028430             MOVE 初検年Ｗ(部位ＣＮＴ)  TO 開始年Ｗ(部位ＣＮＴ) 施記－施術年
028440             MOVE 初検月Ｗ(部位ＣＮＴ)  TO 開始月Ｗ(部位ＣＮＴ) 施記－施術月
028450             MOVE 初検日Ｗ(部位ＣＮＴ)  TO 開始日Ｗ(部位ＣＮＴ) 施記－施術日
028460         ELSE
028470             MOVE 患者番号ＷＲ          TO 施記－患者番号
028480             MOVE 枝番ＷＲ              TO 施記－枝番
028490             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
028500             MOVE 施術年ＷＲ            TO 施記－施術年
028510             MOVE 施術月ＷＲ            TO 施記－施術月
028520             MOVE ZERO                  TO 施記－施術日
028530         END-IF
028540         START 施術記録Ｆ   KEY IS >= 施記－患者コード
028550                                      施記－施術和暦年月日
028560         END-START
028570         IF ( 状態キー = "00" )
      */実日数はレセ－部位実日数を転記する/160816
028580*             MOVE ZERO  TO 実日数Ｗ(部位ＣＮＴ)
028590             MOVE ZERO  TO 終了年ＷＴ
028600             MOVE ZERO  TO 終了月ＷＴ
028610             MOVE ZERO  TO 終了日ＷＴ
028620             MOVE SPACE TO 終了フラグ２
028630             PERFORM 施術記録Ｆ読込
028640             IF ( 終了フラグ２      = SPACE   ) AND
028650                ( 施記－患者コード  = 患者コードＷＲ ) AND
028660                ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
028670                ( 施記－施術年      = 施術年ＷＲ     ) AND
028680                ( 施記－施術月      = 施術月ＷＲ     ) 
028690*
028700*        *****************************************************************
028710*        * 開始年月日 ( その部位が当月初検でないか、
028720*                       当月初検でも枝番がある時は、最初の施術日を開始日)*
028730*        *****************************************************************
028740                 IF ( 施術年Ｗ NOT = 初検年Ｗ(部位ＣＮＴ) ) OR
028750                    ( 施術月Ｗ NOT = 初検月Ｗ(部位ＣＮＴ) ) OR
028760                    ( 開始診療日手動区分Ｗ = 1 )
028770                     MOVE 施記－施術年   TO 開始年Ｗ(部位ＣＮＴ)
028780                     MOVE 施記－施術月   TO 開始月Ｗ(部位ＣＮＴ)
028790                     MOVE 施記－施術日   TO 開始日Ｗ(部位ＣＮＴ)
028800                 END-IF
028810             END-IF
028820             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
028830                           ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
028840                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
028850                           ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
028860                           ( 施記－施術月     NOT = 施術月ＷＲ       ) OR
028870                           ( 施記－施術日         > 終了日Ｗ(部位ＣＮＴ))
028880*               **********
028890*               * 実日数 *
028900*               **********
      */実日数はレセ－部位実日数を転記する/160816
028910*                COMPUTE 実日数Ｗ(部位ＣＮＴ) = 実日数Ｗ(部位ＣＮＴ) + 1
028920                MOVE 施記－施術年               TO 終了年ＷＴ
028930                MOVE 施記－施術月               TO 終了月ＷＴ
028940                MOVE 施記－施術日               TO 終了日ＷＴ
028950*
028960                PERFORM 施術記録Ｆ読込
028970            END-PERFORM
028980        END-IF
028990*       **************************
029000*       * 継続：終了年月日セット *
029010*       **************************
029020        IF ( 転帰区分Ｗ(部位ＣＮＴ) = 9 )
029030            MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
029040            MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
029050            MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
029060        END-IF
029070        IF ( 終了年月日Ｗ(部位ＣＮＴ) > 受理年月日Ｗ )
029080            MOVE 終了年Ｗ(部位ＣＮＴ) TO 受理年Ｗ
029090            MOVE 終了月Ｗ(部位ＣＮＴ) TO 受理月Ｗ
029100            MOVE 終了日Ｗ(部位ＣＮＴ) TO 受理日Ｗ
029110        END-IF
029120     END-PERFORM.
029130*
029140** ----- 前月初検のみかを判定 -----------*
029150*
029160*     MOVE 患者番号ＷＲ          TO 施記－患者番号.
029170*     MOVE 枝番ＷＲ              TO 施記－枝番.
029180*     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
029190*     MOVE 施術年ＷＲ            TO 施記－施術年.
029200*     MOVE 施術月ＷＲ            TO 施記－施術月.
029210*     MOVE ZERO                  TO 施記－施術日.
029220*     START 施術記録Ｆ   KEY IS >= 施記－患者コード
029230*                                  施記－施術和暦年月日
029240*     END-START.
029250*     IF ( 状態キー = "00" )
029260*             MOVE SPACE TO 終了フラグ２
029270*             PERFORM 施術記録Ｆ読込
029280*             IF ( 終了フラグ２      = SPACE   ) AND
029290*                ( 施記－患者コード  = 患者コードＷＲ ) AND
029300*                ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
029310*                ( 施記－施術年      = 施術年ＷＲ     ) AND
029320*                ( 施記－施術月      = 施術月ＷＲ     ) 
029330** 当月施術開始日が再検かどうか判定
029340*                 IF ( 施記－再検料請求 = 1 )
029350*                      MOVE "YES"  TO  初日再検フラグ
029360*                 END-IF
029370**
029380*             END-IF
029390*     END-IF.
029400*     IF ( 初日再検フラグ = "YES" )
029410*        PERFORM 前月初検のみ判定
029420*     END-IF.
029430*
029440*================================================================*
029450*================================================================*
029460 レセプト並び順取得 SECTION.
029470*================================================================*
029480     MOVE 施術和暦ＷＲ       TO 作４－施術和暦.
029490     MOVE 施術年ＷＲ         TO 作４－施術年.
029500     MOVE 施術月ＷＲ         TO 作４－施術月.
029510     MOVE 患者コードＷＲ     TO 作４－患者コード.
029520     MOVE 公費種別ＷＲ       TO 作４－保険種別.
029530     READ 作業ファイル４
029540     NOT INVALID KEY
029550          MOVE NC"№"        TO 順番固定Ｗ
029560          MOVE 作４－順番    TO 順番Ｗ
029570     END-READ.
029580*
029590*================================================================*
029600*================================================================*
029610* ＯＣＲ情報取得 SECTION.
029620*
029630****************************************************************
029640* ●     接骨師会会員番号 .....施術所情報マスタより既に取得    *
029650* ●     施術年月         .....受診者情報Ｆより既に取得        *
029660* ● 　　保険種別         .....全柔用国保コード42をセット    　*
029670*                              新規マスタにより対応の可能性有り*
029680* ●     指定各社ＩＤ     .....自社ＩＤ"21"をセット            *
029690* ●     ＦＤ連番         .....資料参照                        *
029700* ● 　　請求金額         .....レセ－請求金額よりセット        *
029710* ●　　 負担割合         .....負担割合 = レセ－負担割合 / 10　*
029720****************************************************************
029730* 接骨師会会員番号
029740*     MOVE ＯＣＲ接骨師会会員番号Ｗ TO ＯＣＲ会員番号Ｗ.
029750* 施術年月
029760*     MOVE 施術年月Ｗ         TO ＯＣＲ施術年月Ｗ.
029770* 保険種別 = 全柔コード = 42
029780*     MOVE 42                 TO ＯＣＲ保険種別Ｗ.
029790* 指定各社ＩＤ = 自社ＩＤ = 21
029800*     MOVE "21"               TO ＯＣＲ各社ＩＤＷ.
029810* ＦＤ連番 = ＦＤ連番患者番号Ｗ + ＦＤ連番健保ＩＤＷ
029820*     MOVE 患者番号Ｗ         TO ＦＤ連番患者番号Ｗ.
029830*     MOVE 1                  TO ＦＤ連番健保ＩＤＷ.
029840* 請求金額
029850*     MOVE レセ－請求額       TO ＯＣＲ請求金額Ｗ.
029860* 負担割合
029870*     MOVE レセ－負担率     TO 負担割合ＷＲ.
029880*     COMPUTE ＯＣＲ負担割合Ｗ = 負担割合ＷＲ / 10.
029890*================================================================*
029900 施術記録Ｆ読込 SECTION.
029910*
029920     READ 施術記録Ｆ NEXT
029930     AT END
029940         MOVE "YES" TO 終了フラグ２
029950     END-READ.
029960*================================================================*
029970 印刷処理 SECTION.
029980*
029990     MOVE "YCH6125P" TO  定義体名Ｐ.
030000     MOVE "SCREEN"   TO  項目群名Ｐ.
030010     WRITE YCH6125P.
030020***     WRITE 印刷レコード.
030030     PERFORM エラー処理Ｐ.
030040*================================================================*
030050 エラー処理Ｐ SECTION.
030060*
030070     IF ( 通知情報Ｐ NOT = "00" )
030080         DISPLAY NC"帳票エラー"              UPON CONS
030090         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
030100         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
030110         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
030120         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
030130                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
030140         ACCEPT  キー入力 FROM CONS
030150         PERFORM ファイル閉鎖
030160         MOVE 99 TO PROGRAM-STATUS
030170         EXIT PROGRAM
030180     END-IF.
030190*================================================================*
030200 部位名称埋込処理 SECTION.
030210*
006490     STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
009980            負傷名称Ｗ                    DELIMITED BY SPACE
006500            レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
006520       INTO 負傷名Ｗ(部位ＣＮＴ)
006570     END-STRING.
030410*
030420*================================================================*
030430 初検日以前のデータ判定 SECTION.
030440*
030450*********************************************************************************
030460*  最初の初検日以前の当月中に施術記録レコードがあった時(治癒、中止)は、請求区分の
030470*  継続にもチェックする。(新規と継続の両方)
030480*********************************************************************************
030490** 最初の初検日を取得
030500     MOVE SPACE                 TO 初検フラグ.
030510     MOVE 患者番号ＷＲ          TO 施記－患者番号.
030520     MOVE 枝番ＷＲ              TO 施記－枝番.
030530     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
030540     MOVE 施術年ＷＲ            TO 施記－施術年.
030550     MOVE 施術月ＷＲ            TO 施記－施術月.
030560     MOVE ZERO                  TO 施記－施術日.
030570     START 施術記録Ｆ   KEY IS >= 施記－患者コード
030580                                  施記－施術和暦年月日
030590     END-START.
030600     IF ( 状態キー = "00" )
030610         MOVE ZERO  TO 初検和暦ＷＴ
030620         MOVE ZERO  TO 初検年ＷＴ
030630         MOVE ZERO  TO 初検月ＷＴ
030640         MOVE ZERO  TO 初検日ＷＴ
030650         MOVE SPACE TO 終了フラグ２
030660         PERFORM 施術記録Ｆ読込
030670         PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
030680                       ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
030690                       ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
030700                       ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
030710                       ( 施記－施術月     NOT = 施術月ＷＲ      ) OR
030720                       ( 初検フラグ           = "YES"           ) 
030730               IF ( 施記－診療区分 = 2 )
030740                   MOVE 施記－施術和暦           TO 初検和暦ＷＴ
030750                   MOVE 施記－施術年             TO 初検年ＷＴ
030760                   MOVE 施記－施術月             TO 初検月ＷＴ
030770                   MOVE 施記－施術日             TO 初検日ＷＴ
030780                   MOVE "YES"                    TO 初検フラグ
030790               END-IF
030800               PERFORM 施術記録Ｆ読込
030810         END-PERFORM
030820     END-IF.
030830*
030840* 初検日以前のデータ判定
030850     IF ( 初検フラグ = "YES" )
030860        MOVE 患者番号ＷＲ          TO 施記－患者番号
030870        MOVE 枝番ＷＲ              TO 施記－枝番
030880        MOVE 初検和暦ＷＴ          TO 施記－施術和暦
030890        MOVE 初検年ＷＴ            TO 施記－施術年
030900        MOVE 初検月ＷＴ            TO 施記－施術月
030910        MOVE 初検日ＷＴ            TO 施記－施術日
030920        START 施術記録Ｆ   KEY IS <  施記－患者コード
030930                                     施記－施術和暦年月日
030940                                     REVERSED
030950        END-START
030960        IF ( 状態キー = "00" )
030970           MOVE SPACE  TO 終了フラグ２
030980           PERFORM 施術記録Ｆ読込
030990           IF ( 終了フラグ２    = SPACE        ) AND
031000              ( 施記－患者番号  = 患者番号ＷＲ ) AND
031010              ( 施記－枝番      = 枝番ＷＲ     ) AND
031020              ( 施記－施術和暦  = 初検和暦ＷＴ ) AND
031030              ( 施記－施術年    = 初検年ＷＴ   ) AND
031040              ( 施記－施術月    = 初検月ＷＴ   )
031050*  初検日以前の当月中に施術記録レコードがあった時
031060                IF ( 継続チェックＷ = SPACE )
031070                   MOVE NC"○"    TO 継続チェックＷ
031080                END-IF
031090           END-IF
031100         END-IF
031110     END-IF.
031120*
031130*================================================================*
031140 長期判定取得 SECTION.
031150*
031160* ３カ月以上の長期判定は "CHOUKI" を呼ぶ. 
031170     MOVE  SPACE TO  連期間－キー.
031180     INITIALIZE      連期間－キー.
031190     MOVE 施術和暦ＷＲ  TO  連期間－施術和暦.
031200     MOVE 施術年ＷＲ    TO  連期間－施術年.
031210     MOVE 施術月ＷＲ    TO  連期間－施術月.
031220     MOVE 患者番号ＷＲ  TO  連期間－患者番号.
031230     MOVE 枝番ＷＲ      TO  連期間－枝番.
031240*
031250     CALL   "CHOUKI".
031260     CANCEL "CHOUKI".
031270*
031280**** 適用１を使用 (「前月初検のみ」がある時は、くっつける)
031290     IF ( 連期間－対象フラグ  = "YES" )
031300        IF ( 適用１Ｗ  = SPACE )
031310           MOVE NC"※長期施術継続理由裏面に記載"  TO 適用１Ｗ
031320        ELSE
031330           STRING 適用１Ｗ           DELIMITED BY SPACE
031340                  NC"，"             DELIMITED BY SIZE
031350                  NC"※長期施術継続理由裏面に記載"   DELIMITED BY SIZE
031360                  INTO 適用１Ｗ
031370           END-STRING
031380        END-IF
031390     END-IF.
031400*
031410*================================================================*
031420 初検加算時刻取得 SECTION.
031430*****************************************************************
031440** 初検加算が時間外と深夜の時、適用に「受付時間」を印字する。
031450**   時刻の印字は月3回まで可能
031460*****************************************************************
031470     IF ( レセ－時間外 = 1 ) OR ( レセ－深夜 = 1 ) OR ( レセ－休日 = 1 )
031480*
031490         MOVE 患者番号ＷＲ          TO 施記－患者番号
031500         MOVE 枝番ＷＲ              TO 施記－枝番
031510         MOVE 施術和暦ＷＲ          TO 施記－施術和暦
031520         MOVE 施術年ＷＲ            TO 施記－施術年
031530         MOVE 施術月ＷＲ            TO 施記－施術月
031540         MOVE ZERO                  TO 施記－施術日
031550         START 施術記録Ｆ   KEY IS >= 施記－患者コード
031560                                      施記－施術和暦年月日
031570         END-START
031580         IF ( 状態キー = "00" )
031590             MOVE ZERO  TO 初検加算カウント
031600             MOVE SPACE TO 終了フラグ２
031610             PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
031620                           ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
031630                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
031640                           ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
031650                           ( 施記－施術月     NOT = 施術月ＷＲ      ) 
031660               IF ( 施記－初検加算 = 1 OR 2 OR 3 ) AND ( 施記－診療区分 = 2 )
031670                  COMPUTE 初検加算カウント = 初検加算カウント  + 1
031680                  IF ( 初検加算カウント <= 3 )
031690                     MOVE 施記－初検加算 TO 初検加算区分ＷＴ(初検加算カウント)
031700                     MOVE 施記－受付時   TO 初検加算時ＷＴ(初検加算カウント)
031710                     MOVE 施記－受付分   TO 初検加算分ＷＴ(初検加算カウント)
031720                  END-IF
031730               END-IF
031740               PERFORM 施術記録Ｆ読込
031750            END-PERFORM
031760** 初検加算の時刻を適用にセット
033380            IF ( 初検加算時ＷＴ(1) NOT = ZERO ) OR ( 初検加算分ＷＴ(1) NOT = ZERO ) 
                      MOVE 初検加算時ＷＴ(1) TO 初検加算時Ｗ
                      MOVE ":"               TO 初検加算区切Ｗ
                      MOVE 初検加算分ＷＴ(1) TO 初検加算分Ｗ
                  END-IF
033380            IF ( 初検加算時ＷＴ(2) NOT = ZERO ) OR ( 初検加算分ＷＴ(2) NOT = ZERO ) 
031910                PERFORM 初検加算適用セット
                  END-IF
031780         END-IF
031790*
031800     END-IF.
031810*
031820*================================================================*
031830 初検加算適用セット SECTION.
031840*
031850     PERFORM VARYING 番号カウンタ FROM 1 BY 1
031860              UNTIL  番号カウンタ > 3
031870         IF ( 初検加算時ＷＴ(番号カウンタ)  = ZERO )  AND 
031880            ( 初検加算分ＷＴ(番号カウンタ)  = ZERO ) 
031890             CONTINUE
031900         ELSE
031910* 固定項目
031920             EVALUATE 初検加算区分ＷＴ(番号カウンタ) 
031930             WHEN 1
031940                MOVE NC"時間外"   TO 加算内容Ｗ(番号カウンタ)
033320             WHEN 2
033330                MOVE NC"休　日"   TO 加算内容Ｗ(番号カウンタ)
031950             WHEN 3
031960                MOVE NC"深　夜"   TO 加算内容Ｗ(番号カウンタ)
031970             END-EVALUATE
031980*
031990             MOVE NC"："          TO 加算区切Ｗ(番号カウンタ)
032000             MOVE NC"時"          TO 時固定Ｗ(番号カウンタ)
032010             MOVE NC"分"          TO 分固定Ｗ(番号カウンタ)
032020*
032030**** 数字→日本語変換
032040* 時間
032050             MOVE 初検加算時ＷＴ(番号カウンタ)  TO  数字Ｗ
032060             IF ( 数字Ｗ >= 10 )
032070                 MOVE 数字Ｗ１    TO 負傷番号Ｗ１
032080                 PERFORM 日本語変換
032090                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ１(番号カウンタ)
032100                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
032110                 PERFORM 日本語変換
032120                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
032130             ELSE
032140                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
032150                 PERFORM 日本語変換
032160                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
032170             END-IF
032180* 分
032190             MOVE 初検加算分ＷＴ(番号カウンタ)  TO  数字Ｗ
032200             MOVE 数字Ｗ１    TO 負傷番号Ｗ１
032210             PERFORM 日本語変換
032220             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ１(番号カウンタ)
032230             MOVE 数字Ｗ２    TO 負傷番号Ｗ１
032240             PERFORM 日本語変換
032250             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ２(番号カウンタ)
032260** 
032270        END-IF
032280     END-PERFORM.
032290*
032300     MOVE  初検加算集団ＮＷ(1)   TO 初検加算時刻１Ｗ. 
032310     MOVE  初検加算集団ＮＷ(2)   TO 初検加算時刻２Ｗ. 
032320     MOVE  初検加算集団ＮＷ(3)   TO 初検加算時刻３Ｗ. 
032330*
032340**** 適用１か２を使用（長期理由記載で適用１を使っている時は、適用２）
032350     IF ( 初検加算時ＷＴ(2)  = ZERO ) AND ( 初検加算分ＷＴ(2)  = ZERO ) 
032360         CONTINUE
032370     ELSE
032380         IF ( 適用１Ｗ  = SPACE )
032390               STRING NC"初検加算"       DELIMITED BY SIZE
032400                      初検加算時刻１Ｗ   DELIMITED BY SIZE
032410                      初検加算時刻２Ｗ   DELIMITED BY SIZE
032420                      初検加算時刻３Ｗ   DELIMITED BY SIZE
032430                      INTO 適用１Ｗ
032440               END-STRING
032450         ELSE
032460               STRING NC"初検加算"       DELIMITED BY SIZE
032470                      初検加算時刻１Ｗ   DELIMITED BY SIZE
032480                      初検加算時刻２Ｗ   DELIMITED BY SIZE
032490                      初検加算時刻３Ｗ   DELIMITED BY SIZE
032500                      INTO 適用２Ｗ
032510               END-STRING
032520         END-IF
032530     END-IF.
032540*
032550*================================================================*
032560 日本語変換 SECTION.
032570*
032580     MOVE NC"０"     TO 全角負傷番号Ｗ.
032590     CALL "htoz" WITH C LINKAGE
032600                        USING 負傷番号Ｗ１ 全角負傷番号Ｗ１.
032610*
032620*================================================================*
032630*================================================================*
032640 負傷原因取得 SECTION.
032650*
032660********************************************************************
032670*  負傷原因コードが同じものは、1行にまとめて印字する。
032680*  例: ①② 家で転んだ.
032690*     負傷原因コードが同じものをまとめ、テーブルにセット
032700*     (ただし、部位を飛んで同じものは、2行になる)
032710********************************************************************
032720     MOVE  ZERO   TO  カウンタ カウンタ２.
032730     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
032740             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
032750*
032760****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
032770        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
032780*
032790           IF ( カウンタ = ZERO )
032800               MOVE 1   TO  カウンタ カウンタ２
032810               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
032820               MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
032830               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
032840           ELSE
032850              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
032860                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
032870                 COMPUTE カウンタ２ = カウンタ２  +  1
032880                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
032890              ELSE
032900                 COMPUTE カウンタ = カウンタ  +  1
032910                 MOVE 1   TO  カウンタ２
032920                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
032930                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
032940                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
032950              END-IF
032960           END-IF
032970        END-IF
032980     END-PERFORM.
032990**************************************************************************
033000*  負傷原因マスタより文章取得
033010**************************************************************************
033020     MOVE  ZERO   TO  カウンタ カウンタ２.
033030     PERFORM VARYING カウンタ FROM 1 BY 1
033040             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
033050** 健保は 区分 01
033060         MOVE 01                        TO 負原－区分コード
033070         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
033080         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
033090         READ 負傷原因Ｆ
033100         NOT INVALID KEY
033110             INITIALIZE 負傷原因ＷＴ
033120             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
033130             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
033140             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
033150             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
033160             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
033170             PERFORM VARYING カウンタ２ FROM 1 BY 1
033180                     UNTIL ( カウンタ２ > 9 )  OR 
033190                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
033200                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
033210                WHEN 1
033220                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
033230                WHEN 2
033240                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
033250                WHEN 3
033260                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
033270                WHEN 4
033280                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
033290                WHEN 5
033300                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
033270                WHEN 6
033280                   MOVE "⑥"  TO  負傷原因ナンバーＷ１(カウンタ２)
033290                WHEN 7
033300                   MOVE "⑦"  TO  負傷原因ナンバーＷ１(カウンタ２)
033310                WHEN OTHER
033320                   CONTINUE
033330                END-EVALUATE
033340             END-PERFORM
033350*
033432             IF 負原－負傷原因入力区分 = 1
033433                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
033434                        負傷原因１ＷＴ  DELIMITED BY SIZE
033435                        負傷原因２ＷＴ  DELIMITED BY SIZE
033436                        負傷原因３ＷＴ  DELIMITED BY SIZE
033437                        負傷原因４ＷＴ  DELIMITED BY SIZE
033438                        負傷原因５ＷＴ  DELIMITED BY SIZE
033439                        INTO 負傷原因内容合成Ｗ(カウンタ)
033440                 END-STRING
033441             ELSE
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
033450             END-IF
033451*
033452         END-READ
033453     END-PERFORM.
033460*
033470     PERFORM 負傷原因セット.
033480*
033490*================================================================*
033500 負傷原因セット SECTION.
033510*
033520**************************************************************************
033530*  文章が1行を超える時は、複数行に分解する。
033540**************************************************************************
033550     MOVE  ZERO   TO  カウンタ カウンタ２.
033560     PERFORM VARYING カウンタ FROM 1 BY 1
033570             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
033580*
033590          INITIALIZE 負傷原因内容分解ＸＷ
033600          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
033610          IF ( 負傷原因内容１ＸＷ  NOT = SPACE )
033620              COMPUTE カウンタ２ = カウンタ２  +  1
033630              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
033640          END-IF
033650          IF ( 負傷原因内容２ＸＷ  NOT = SPACE )
033660              COMPUTE カウンタ２ = カウンタ２  +  1
033670              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
033680          END-IF
033690          IF ( 負傷原因内容３ＸＷ  NOT = SPACE )
033700              COMPUTE カウンタ２ = カウンタ２  +  1
033710              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
033720          END-IF
033830          IF ( 負傷原因内容４ＸＷ  NOT = SPACE )
033840              COMPUTE カウンタ２ = カウンタ２  +  1
033850              MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
033860          END-IF
033730*
033740     END-PERFORM.
033750*================================================================*
033760 助成印取得 SECTION.
033770*
033780* 2006/04 変更
033790* 助成印は "JOSEIMEI" を呼ぶ. 
033800     MOVE SPACE TO  連助成名称－キー.
033810     INITIALIZE     連助成名称－キー.
033820     MOVE 助成種別ＷＲ           TO 連助成名称－助成種別.
033830     MOVE 費用負担者番号助成ＷＲ TO 連助成名称－費用負担者番号助成.
033840*
033850     CALL   "JOSEIMEI".
033860     CANCEL "JOSEIMEI".
033870*
033880     MOVE 連助成名称－１文字 TO 助成印Ｗ.
033890*
033900***
034070*
034080*================================================================*
034090 前月初検のみ判定 SECTION.
034100*
034110*** 前月の通院日が初検か判定 
034120     MOVE  SPACE            TO 前月フラグ.
034130     MOVE 受－患者コード    TO 施記－患者コード.
034140     MOVE 受－施術和暦      TO 施記－施術和暦.
034150     MOVE 受－施術年        TO 施記－施術年.
034160     MOVE 受－施術月        TO 施記－施術月.
034170     MOVE 1                 TO 施記－施術日.
034180     START 施術記録Ｆ   KEY IS <  施記－患者コード
034190                                  施記－施術和暦年月日
034200                                  REVERSED
034210     END-START.
034220     IF ( 状態キー = "00" )
034230         MOVE SPACE  TO 終了フラグ２
034240         PERFORM 施術記録Ｆ読込
034250         IF ( 終了フラグ２      = SPACE  ) AND
034260            ( 施記－患者コード  = 受－患者コード ) AND
034270            ( 施記－診療区分    = 2 ) 
034280*
034290            PERFORM 前月判定
034300**** 適用１を使用
034310            IF ( 前月フラグ = "YES" )
034320               MOVE NC"※前月初検のみ"    TO  適用１Ｗ
034330            END-IF
034340**
034350         END-IF
034360     END-IF.
034370*
034380*================================================================*
034390 前月判定  SECTION.
034400* 
034410*** 読み込んだ施術記録の年月が、前月かどうか判定 (年月の差が 1 か?)
034420      MOVE  SPACE  TO  前月フラグ.
034430      INITIALIZE  計算年月日Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
034440**
034450      MOVE 受－施術和暦    TO 終了和暦２Ｗ.
034460      MOVE 受－施術年      TO 終了年２Ｗ.
034470      MOVE 受－施術月      TO 終了月２Ｗ.
034480      MOVE 施記－施術和暦  TO 開始和暦２Ｗ.
034490      MOVE 施記－施術年    TO 開始年２Ｗ.
034500      MOVE 施記－施術月    TO 開始月２Ｗ.
034510*
034520      EVALUATE TRUE
034530       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ = 終了年２Ｗ)
034540            PERFORM  前月比較月
034550       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ NOT = 終了年２Ｗ)
034560            PERFORM  前月比較年
034570       WHEN  開始和暦２Ｗ NOT = 終了和暦２Ｗ 
034580            PERFORM  前月比較元号
034590      END-EVALUATE.
034600*
034610      IF ( 計算月Ｗ = 1 )
034620         MOVE  "YES"  TO  前月フラグ
034630      END-IF.
034640*
034650*================================================================*
034660 前月比較月  SECTION.
034670*
034680     IF ( 終了月２Ｗ >  開始月２Ｗ )
034690         COMPUTE 計算月Ｗ = 終了月２Ｗ - 開始月２Ｗ
034700     ELSE
034710        MOVE ZERO TO 計算月Ｗ
034720     END-IF.
034730*
034740*================================================================*
034750 前月比較年  SECTION.
034760*
034770     IF ( 終了年２Ｗ >  開始年２Ｗ )
034780         COMPUTE 計算年Ｗ = 終了年２Ｗ - 開始年２Ｗ
034790         COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
034800     ELSE
034810        MOVE ZERO TO 計算月Ｗ
034820     END-IF.
034830*
034840*================================================================*
034850 前月比較元号  SECTION.
034860*
034870     MOVE 開始和暦２Ｗ TO 元－元号区分.
034880     READ 元号マスタ
034890     NOT INVALID KEY
034900         MOVE 元－開始西暦年 TO 開始西暦年Ｗ
034910     END-READ.
034920     MOVE 終了和暦２Ｗ TO 元－元号区分.
034930     READ 元号マスタ
034940     NOT INVALID KEY
034950         MOVE 元－開始西暦年 TO 終了西暦年Ｗ
034960     END-READ.
034970**
034980     IF ( 開始西暦年Ｗ NOT = ZERO ) AND ( 終了西暦年Ｗ NOT = ZERO )
034990        COMPUTE 開始西暦年Ｗ = 開始西暦年Ｗ + 開始年２Ｗ - 1
035000        COMPUTE 終了西暦年Ｗ = 終了西暦年Ｗ + 終了年２Ｗ - 1
035010*
035020        IF ( 終了西暦年Ｗ =  開始西暦年Ｗ )
035030           PERFORM  前月比較月
035040        ELSE
035050           IF ( 終了西暦年Ｗ >  開始西暦年Ｗ )
035060               COMPUTE 計算年Ｗ = 終了西暦年Ｗ - 開始西暦年Ｗ
035070               COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
035080           ELSE
035090               MOVE ZERO TO 計算月Ｗ
035100           END-IF
035110        END-IF
035120     ELSE
035130        MOVE ZERO TO 計算月Ｗ
035140     END-IF.
035150*
035330*================================================================*
035340 受診者印刷区分更新 SECTION.
035350*
035360** //  受診者情報Ｆの印刷区分に１をセットし、更新する。//  
035370*
035380     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
035390     MOVE 施術年ＷＲ         TO 受－施術年.
035400     MOVE 施術月ＷＲ         TO 受－施術月.
035410     MOVE 患者コードＷＲ     TO 受－患者コード.
035420     READ 受診者情報Ｆ
035430     NOT INVALID KEY
035440         MOVE  1  TO  受－レセ印刷区分
035450         REWRITE  受－レコード
035460         END-REWRITE
035470         IF ( 状態キー NOT = "00" )
035480            MOVE NC"受診者" TO ファイル名
035490            PERFORM エラー表示
035500         END-IF
035510     END-READ.
035520*
035530*================================================================*
035540 月末日取得 SECTION.
035550*
035560     MOVE 施術年ＷＲ   TO 受理年Ｗ.
035570     MOVE 施術月ＷＲ   TO 受理月Ｗ.
035580     MOVE 施術和暦ＷＲ TO 元－元号区分.
035590     READ 元号マスタ
035600     NOT INVALID KEY
035610         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
035620     END-READ.
035630     IF ( 施術西暦年Ｗ NOT = ZERO )
035640        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
035650     END-IF.
035660*
035670     EVALUATE 施術月ＷＲ
035680     WHEN 4
035690     WHEN 6
035700     WHEN 9
035710     WHEN 11
035720         MOVE 30 TO 受理日Ｗ
035730     WHEN 2
035740         DIVIDE 4 INTO 施術西暦年Ｗ GIVING    商Ｗ
035750                                    REMAINDER 余Ｗ
035760         END-DIVIDE
035770         IF ( 余Ｗ = ZERO )
035780             MOVE 29 TO 受理日Ｗ
035790         ELSE
035800             MOVE 28 TO 受理日Ｗ
035810         END-IF
035820     WHEN 1
035830     WHEN 3
035840     WHEN 5
035850     WHEN 7
035860     WHEN 8
035870     WHEN 10
035880     WHEN 12
035890         MOVE 31 TO 受理日Ｗ
035900     WHEN OTHER
035910          CONTINUE
035920     END-EVALUATE.
035930*
035940*================================================================*
035950 委任年月日取得 SECTION.
035960*
035970** ---// ここの受理年には、最終通院日が入っている為、退避する //----
035980     MOVE 受理年Ｗ   TO 最終通院年Ｗ.
035990     MOVE 受理月Ｗ   TO 最終通院月Ｗ.
036000     MOVE 受理日Ｗ   TO 最終通院日Ｗ.
036010***
036020* (柔整師側)
036030     EVALUATE レセプト日付区分Ｗ 
036040*    /  最終通院日 /
036050     WHEN ZERO
036060         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
036070         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
036080         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
036090*    /  月末日 /
036100     WHEN 1 
036110         PERFORM 月末日取得
036120         MOVE 受理年Ｗ     TO 柔整師年Ｗ
036130         MOVE 受理月Ｗ     TO 柔整師月Ｗ
036140         MOVE 受理日Ｗ     TO 柔整師日Ｗ
036150*    /  印字なし /
036160     WHEN 9
036170         MOVE ZERO         TO 柔整師年Ｗ
036180         MOVE ZERO         TO 柔整師月Ｗ
036190         MOVE ZERO         TO 柔整師日Ｗ
036200*    /  その他は、最終通院日 /
036210     WHEN OTHER
036220         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
036230         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
036240         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
036250     END-EVALUATE.
036260**
036270* (患者側)
036280     EVALUATE レセプト患者日付区分Ｗ 
036290*    /  最終通院日 /
036300     WHEN ZERO
036310         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
036320         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
036330         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
036340*    /  月末日 /
036350     WHEN 1 
036360         PERFORM 月末日取得
036370         MOVE 受理年Ｗ     TO 患者委任年Ｗ
036380         MOVE 受理月Ｗ     TO 患者委任月Ｗ
036390         MOVE 受理日Ｗ     TO 患者委任日Ｗ
036400*    /  印字なし /
036410     WHEN 9
036420         MOVE ZERO         TO 患者委任年Ｗ
036430         MOVE ZERO         TO 患者委任月Ｗ
036440         MOVE ZERO         TO 患者委任日Ｗ
036450*    /  その他は、最終通院日 /
036460     WHEN OTHER
036470         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
036480         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
036490         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
036500     END-EVALUATE.
036510*
036520*================================================================*
036831*================================================================*
036832 助成レセまとめ判定 SECTION.
036833*---------------------------------------------------------------------------*
036834* 市町村マスタを読み、レセまとめ区分＝１でかつ、本体保険が国保・退職
036835* の時は、フラグYES (金額を助成込みで印字）
036836*（例：横浜市の障害は、本体保険（国保系）のレセプト１枚で請求、助成レセはなし）
036837*---------------------------------------------------------------------------*
036838*
036839     MOVE SPACE TO 助成レセまとめフラグ.
036840*     MOVE SPACE TO 助成種別略称Ｗ.
036841*
036842*     MOVE 受－助成種別           TO 市－公費種別.
036843*     MOVE 受－費用負担者番号助成 TO 市－市町村番号.
036844*     READ 市町村マスタ
036845*     NOT INVALID KEY
036846*         IF 市－レセまとめ区分 = 1 
036847*            IF (( 受－保険種別 = 01 ) AND ( 受－保険者番号(3:1) NOT = "3" )) OR
036848*               ( 受－保険種別 = 08 ) 
      *                OR ( 受－保険種別 = 05 )
036849*                MOVE "YES" TO 助成レセまとめフラグ
036850*
036851*                MOVE 02            TO 名－区分コード
036852*                MOVE 受－助成種別  TO 名－名称コード
036853*                READ 名称マスタ
036854*                NOT INVALID KEY
036855*                    MOVE 名－略称  TO 助成種別略称Ｗ
036856*                END-READ
036857*            END-IF
036858*         END-IF
036859*     END-READ.
036860*
036861** / CALL JRECEOFF /
036862*     IF 助成レセまとめフラグ = SPACE
036863*        INITIALIZE 連レセまとめ－キー
036864*        MOVE 施術和暦ＷＲ TO 連レセまとめ－施術和暦
036865*        MOVE 施術年ＷＲ   TO 連レセまとめ－施術年
036866*        MOVE 施術月ＷＲ   TO 連レセまとめ－施術月
036867*        MOVE 患者番号ＷＲ TO 連レセまとめ－患者番号
036868*        MOVE 枝番ＷＲ     TO 連レセまとめ－枝番
036869*       1:助成レセプトなしの本体まとめの判定
036870*        MOVE 1            TO 連レセまとめ－判定区分
036871*        CALL   "JRECEOFF"
036872*        CANCEL "JRECEOFF"
036873*
036874*        IF 連レセまとめ－判定結果 = 1
           IF ( レセ－本体まとめ区分 = 1 )
036875           MOVE "YES" TO 助成レセまとめフラグ
036877     END-IF.
036878*
036879*----------------------------------------------------------------------*
036880** / 神奈川県固有：摘要に負担者番号と受給者番号 /
036881     IF ( 助成レセまとめフラグ = "YES" ) AND
036882        ( 受－費用負担者番号助成(3:2) = "14" )
036883        IF 受－費用負担者番号助成(1:2) NOT = "99"
036884*            MOVE ALL NC"￣"    TO 横線１ 横線２ 横線３
036885*            MOVE ALL NC"｜"    TO 縦線１ 縦線２
036886*            MOVE NC"｜"                    TO 縦線３ 縦線４
036887*            MOVE NC"公費負担者番号"        TO 固定１
036888*            MOVE NC"受給者番号"            TO 固定２
036889*            MOVE NC"／"                    TO 神奈川固定３
036890            MOVE 受－費用負担者番号助成    TO 公費負担者番号 
036891            MOVE 受－受益者番号助成        TO 受給者番号
036892        END-IF
036893     END-IF.
      */和歌山県障害乳幼児ひとり親/100518
037769     IF ( 助成レセまとめフラグ = "YES" ) AND
037770        ( 受－費用負担者番号助成(3:2) = "30" )
037772        IF 受－費用負担者番号助成(1:2) NOT = "99"
036887*            MOVE NC"公費負担者番号"        TO 固定１
036888*            MOVE NC"受給者番号"            TO 固定２
036890            MOVE 受－費用負担者番号助成    TO 公費負担者番号 
036891            MOVE 受－受益者番号助成        TO 受給者番号
           END-IF.
      */滋賀県/110806
037769     IF ( 助成レセまとめフラグ = "YES" ) AND
037770        ( 受－費用負担者番号助成(3:2) = "25" )
037772        IF 受－費用負担者番号助成(1:2) NOT = "99"
037781            MOVE 受－費用負担者番号助成    TO 公費負担者番号 
037782            MOVE 受－受益者番号助成        TO 受給者番号
036792        END-IF
           END-IF.
036894*
037100*================================================================*
037110 レセ摘要再セット SECTION.
037120*---------------------------------------------------------------*
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
037290*
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
037320*================================================================*
037330 エラー表示 SECTION.
037340*
037350     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
037360     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
037370     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
037380     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
037390                                                   UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
037400     ACCEPT  キー入力 FROM CONS
037410     PERFORM ファイル閉鎖.
037420     EXIT PROGRAM.
037430*================================================================*
037440*================================================================*
037450 ファイル閉鎖 SECTION.
037460*
037470     CLOSE 印刷ファイル.
037480     CLOSE 保険者マスタ     元号マスタ          名称マスタ
037490           レセプトＦ       制御情報マスタ      施術所情報マスタ
037500           請求先マスタ     経過マスタ          受診者情報Ｆ
037510           施術記録Ｆ       負傷データＦ        負傷原因Ｆ
037520           市町村マスタ     メモファイル
037530           会情報マスタ     ＩＤ管理マスタ      作業ファイル４.
037540*================================================================*
037550 終了処理 SECTION.
037560*
037570     PERFORM ファイル閉鎖.
037580*================================================================*
037590*================================================================*
037600 テスト印字処理 SECTION.
037610*
           CONTINUE.
038950*
038960*================================================================*
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
038970******************************************************************
038980 END PROGRAM YCH6475.
038990******************************************************************
