000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCH6225.
000060 AUTHOR.                 池田 幸子
000070*
000080*----------------------------------------------------------------*
000090*         中央 共済レセプト印刷（柔+ｳｨﾝﾄﾞｳｽﾞ版）
000100*         MED = YAW610 YCH6125P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2011-09-12
000130 DATE-COMPILED.          2011-09-12
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
001270*
001280     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
001290                             ORGANIZATION             IS  INDEXED
001300                             ACCESS MODE              IS  DYNAMIC
001310                             RECORD KEY               IS  負原－区分コード
001320                                                          負原－負傷原因コード
001330                             FILE STATUS              IS  状態キー
001340                             LOCK        MODE         IS  AUTOMATIC.
001350     SELECT  メモファイル    ASSIGN      TO        MEMOL
001360                             ORGANIZATION             IS  INDEXED
001370                             ACCESS MODE              IS  DYNAMIC
001380                             RECORD KEY               IS  メモ－制御区分
001390                                                          メモ－患者コード
001400                                                          メモ－施術和暦年月日
001410                             ALTERNATE RECORD KEY     IS  メモ－制御区分
001420                                                          メモ－施術和暦年月日
001430                                                          メモ－患者コード
001440                             ALTERNATE RECORD KEY     IS  メモ－患者コード
001450                                                          メモ－施術和暦年月日
001460                                                          メモ－制御区分
001470                             FILE STATUS              IS  状態キー
001480                             LOCK        MODE         IS  AUTOMATIC.
001490* 並び順印字用
001500     SELECT  作業ファイル４  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001510                             ORGANIZATION             IS  INDEXED
001520                             ACCESS                   IS  DYNAMIC
001530                             RECORD      KEY          IS  作４－施術和暦年月
001540                                                          作４－患者コード
001550                                                          作４－保険種別
001560                             FILE        STATUS       IS  状態キー
001570                             LOCK        MODE         IS  AUTOMATIC.
001580     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF002
001590                             SYMBOLIC    DESTINATION  IS "PRT"
001600                             FORMAT                   IS  定義体名Ｐ
001610                             GROUP                    IS  項目群名Ｐ
001620                             PROCESSING  MODE         IS  処理種別Ｐ
001630                             UNIT        CONTROL      IS  拡張制御Ｐ
001640                             FILE        STATUS       IS  通知情報Ｐ.
001650******************************************************************
001660*                      DATA DIVISION                             *
001670******************************************************************
001680 DATA                    DIVISION.
001690 FILE                    SECTION.
001700*                           ［ＲＬ＝  ３２０］
001710 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001720     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
001730*                           ［ＲＬ＝  １２８］
001740 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001750     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001760*                           ［ＲＬ＝  １２８］
001770 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
001780     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
001820*                           ［ＲＬ＝  ２５６］
001830 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001840     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
001850*                           ［ＲＬ＝  １２８］
001860 FD  施術所情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001870     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
001880*                           ［ＲＬ＝  １２８］
001890 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
001900     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
001910*                           ［ＲＬ＝  １２８］
001920 FD  経過マスタ          BLOCK   CONTAINS   1   RECORDS.
001930     COPY KEIKA          OF  XFDLIB  JOINING   経   AS  PREFIX.
001940*                           ［ＲＬ＝  ３２０］
001950 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
001960     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
001970*                           ［ＲＬ＝  ２５６］
001980 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
001990     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
002000*                           ［ＲＬ＝  １２８］
002010 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002020     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
002030*                           ［ＲＬ＝  １２８］
002040 FD  負傷原因Ｆ         BLOCK   CONTAINS   1   RECORDS.
002050     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
002060*                           ［ＲＬ＝  ８３２］
002070 FD  メモファイル        BLOCK CONTAINS 1     RECORDS.
002080     COPY MEMO           OF    XFDLIB JOINING メモ AS PREFIX.
002090**
002100 FD  作業ファイル４ RECORD  CONTAINS 32 CHARACTERS.
002110 01  作４－レコード.
002120     03  作４－レコードキー.
002130         05  作４－施術和暦年月.
002140             07  作４－施術和暦            PIC 9.
002150             07  作４－施術年              PIC 9(2).
002160             07  作４－施術月              PIC 9(2).
002170         05  作４－患者コード.
002180             07 作４－患者番号             PIC 9(6).
002190             07 作４－枝番                 PIC X(1).
002200         05  作４－保険種別                PIC 9(2).
002210     03  作４－レコードデータ.
002220         05  作４－順番                    PIC 9(4).
002230         05  FILLER                        PIC X(14).
002240*
002250 FD  印刷ファイル.
002260     COPY YCH6125P       OF  XMDLIB.
002270*----------------------------------------------------------------*
002280******************************************************************
002290*                WORKING-STORAGE SECTION                         *
002300******************************************************************
002310 WORKING-STORAGE         SECTION.
002320 01 キー入力                           PIC X     VALUE SPACE.
002330 01 状態キー                           PIC X(2)  VALUE SPACE.
002340 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002350 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002360 01 初検フラグ                         PIC X(3)  VALUE SPACE.
002370 01 ファイル名                         PIC N(6)  VALUE SPACE.
002380 01 レセプトＰＧＷ                     PIC X(8)  VALUE SPACE.
002390 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
002400 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
002410 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002420 01 患者番号Ｗ                         PIC 9(6)  VALUE ZERO.
002430 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002440 01 部位名称Ｗ                         PIC N(12) VALUE SPACE.
002450 01 部位長Ｗ                           PIC 9(2) VALUE 1.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002870 01 用紙種別Ｗ                         PIC 9(1)  VALUE ZERO.
002460** 数字→日本語変換
002470 01 数字Ｗ                             PIC 9(2).
002480 01 数字Ｒ REDEFINES 数字Ｗ.
002490    03 数字Ｗ１                        PIC X(1).
002500    03 数字Ｗ２                        PIC X(1).
002510 01 負傷番号Ｗ                         PIC 9.
002520 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
002530    03 負傷番号Ｗ１                    PIC X.
002540*
002550 01 全角負傷番号Ｗ                     PIC N     VALUE SPACE.
002560 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
002570    03 全角負傷番号Ｗ１                PIC X(2).
002580*
002590 01 カウンタ                           PIC 9(2)  VALUE ZERO.
002600 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
002610*
002620* 退避用
002630 01 終了年月日ＷＴ.
002640    03 終了年ＷＴ                      PIC 9(2)  VALUE ZERO.
002650    03 終了月ＷＴ                      PIC 9(2)  VALUE ZERO.
002660    03 終了日ＷＴ                      PIC 9(2)  VALUE ZERO.
002670* 初検日退避用
002680 01 初検年月日ＷＴ.
002690    03 初検和暦ＷＴ                    PIC 9     VALUE ZERO.
002700    03 初検年ＷＴ                      PIC 9(2)  VALUE ZERO.
002710    03 初検月ＷＴ                      PIC 9(2)  VALUE ZERO.
002720    03 初検日ＷＴ                      PIC 9(2)  VALUE ZERO.
002730*
002740* 初検加算時刻用
002750 01 初検加算ＷＴ.
002760    03 初検加算カウント                PIC 9    VALUE ZERO.
002770    03 番号カウンタ                    PIC 9    VALUE ZERO.
002780    03 初検加算集団ＷＴ  OCCURS 3.
002790       05 初検加算区分ＷＴ             PIC 9    VALUE ZERO.
002800       05 初検加算時ＷＴ               PIC 9(2) VALUE ZERO.
002810       05 初検加算分ＷＴ               PIC 9(2) VALUE ZERO.
002820    03 初検加算集団ＮＷ  OCCURS 3.
002830       05 加算区切Ｗ                   PIC N(1) VALUE SPACE.
002840       05 加算内容Ｗ                   PIC N(3) VALUE SPACE.
002850       05 初検加算時ＮＷ１             PIC N(1) VALUE SPACE.
002860       05 初検加算時ＮＷ２             PIC N(1) VALUE SPACE.
002870       05 時固定Ｗ                     PIC N(1) VALUE SPACE.
002880       05 初検加算分ＮＷ１             PIC N(1) VALUE SPACE.
002890       05 初検加算分ＮＷ２             PIC N(1) VALUE SPACE.
002900       05 分固定Ｗ                     PIC N(1) VALUE SPACE.
002910    03 初検加算時刻１Ｗ                PIC N(10) VALUE SPACE.
002920    03 初検加算時刻２Ｗ                PIC N(10) VALUE SPACE.
002930    03 初検加算時刻３Ｗ                PIC N(10) VALUE SPACE.
003070    03 初検加算区切Ｗ                  PIC X     VALUE SPACE.
003080    03 初検加算時Ｗ                    PIC 9(2)  VALUE ZERO.
003090    03 初検加算分Ｗ                    PIC 9(2)  VALUE ZERO.
002940* 負傷原因用
002950 01 負傷原因ＷＴ.
002960    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
002970    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
002980    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
002990    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
003000    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
003010    03 負傷原因ナンバーＷＴ.
003020       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
003030    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
003040 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
003050 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
003060 01 負傷原因ＴＢＬ.
003070    03 負傷原因コードＴＢＬ            OCCURS 9.
003080       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
003090       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
003100       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
003110 01 負傷原因内容Ｗ.
003120    03 負傷原因内容合成Ｗ              PIC X(318) OCCURS 9 VALUE SPACE.
003130    03 負傷原因内容分解ＸＷ.
003140       05 負傷原因内容１ＸＷ           PIC X(80)  VALUE SPACE.
003150       05 負傷原因内容２ＸＷ           PIC X(80)  VALUE SPACE.
003160       05 負傷原因内容３ＸＷ           PIC X(80)  VALUE SPACE.
003700       05 負傷原因内容４ＸＷ           PIC X(78)  VALUE SPACE.
003170*
003180*************
003190* 共済番号用
003200 01 共済連番号集団Ｗ.
003210    03 共済連番号名Ｗ                  PIC X(14)  VALUE SPACE.
003220    03 共済連番号名ＮＷ REDEFINES  共済連番号名Ｗ  PIC N(7).
003230    03 共済連番号Ｗ                    PIC X(6)  VALUE SPACE.
003240    03 共済連番号単位Ｗ                PIC X(2)  VALUE SPACE.
003250    03 共済連番号単位ＮＷ REDEFINES  共済連番号単位Ｗ  PIC N.
003260* 自衛官番号用
003270 01 自衛官番号集団Ｗ.
003280    03 自衛官番号名Ｗ                  PIC X(8)  VALUE SPACE.
003290    03 自衛官番号名ＮＷ REDEFINES  自衛官番号名Ｗ  PIC N(4).
003300    03 自衛官番号Ｗ                    PIC X(6)  VALUE SPACE.
003310    03 自衛官番号単位Ｗ                PIC X(2)  VALUE SPACE.
003320    03 自衛官番号単位ＮＷ REDEFINES  自衛官番号単位Ｗ  PIC N.
003330 01 脱出フラグ                         PIC X(3)  VALUE SPACE.
003340****************
003350** 前月初検のみ用
003360 01 初日再検フラグ                     PIC X(3)  VALUE SPACE.
003370 01 前月フラグ                         PIC X(3)  VALUE SPACE.
003380*
003390 01 計算年月日Ｗ.
003400    03 計算和暦Ｗ                      PIC 9(1)  VALUE ZERO.
003410    03 計算年Ｗ                        PIC S9(2)  VALUE ZERO.
003420    03 計算月Ｗ                        PIC S9(2)  VALUE ZERO.
003430    03 計算日Ｗ                        PIC S9(2)  VALUE ZERO.
003440 01 開始年月日２Ｗ.
003450    03 開始和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003460    03 開始年２Ｗ                      PIC 9(2)  VALUE ZERO.
003470    03 開始月２Ｗ                      PIC 9(2)  VALUE ZERO.
003480    03 開始日２Ｗ                      PIC 9(2)  VALUE ZERO.
003490    03 開始西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003500 01 終了年月日２Ｗ.
003510    03 終了和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003520    03 終了年２Ｗ                      PIC 9(2)  VALUE ZERO.
003530    03 終了月２Ｗ                      PIC 9(2)  VALUE ZERO.
003540    03 終了日２Ｗ                      PIC 9(2)  VALUE ZERO.
003550    03 終了西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003560***
003570** 負傷原因・長期理由印刷区分用
003580 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
003590 01 長期理由印刷区分Ｗ                 PIC 9 VALUE ZERO.
003600*
003610** レセ下段の日付区分用 (0:最終通院日、1:月末日、9:印字なし)
003620 01 レセプト日付区分Ｗ                 PIC 9 VALUE ZERO.
003630 01 レセプト患者日付区分Ｗ             PIC 9 VALUE ZERO.
003640*
003650*
003660** 月末日用
003670 01 施術西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
003680 01 商Ｗ                               PIC 9(3)  VALUE ZERO.
003690 01 余Ｗ                               PIC 9(3)  VALUE ZERO.
003700*
003710** 枝番判定用
003720 01 開始診療日手動区分Ｗ               PIC 9    VALUE ZERO.
003730*
003740*
003750** レセ摘要用( N(38)固定） /
003760 01 負傷の経過Ｗ.
003770    03 負傷の経過行Ｗ                  PIC X(76) OCCURS 2 VALUE SPACE.
003780 01 負傷の経過ＮＷ REDEFINES 負傷の経過Ｗ.
003790    03 負傷の経過行ＮＷ                PIC N(38) OCCURS 2.
003800*
003810*
003820* 負傷原因印刷区分
003830 01 レセ負傷原因印刷区分Ｗ             PIC 9    VALUE ZERO.
003840*
003460 01 長期理由印刷区分Ｆ                 PIC 9 VALUE ZERO.
003841*
003842* 福岡の経過固定印字用に使用
003843 01 全柔ＦＰＤ区分Ｗ                   PIC 9     VALUE ZERO.
003844 01 経過部位数字Ｗ                     PIC N(1)  VALUE SPACE.
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
003845*
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
003846*
003850****************
003860* 連結項目待避 *
003870****************
003880*    ************
003890*    * 印刷キー *
003900*    ************
003910 01 対象データＷＲ.
003920    03 施術和暦年月ＷＲ.
003930       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
003940       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
003950       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
003960    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
003970    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
003980    03 公費種別ＷＲ                     PIC 9(2)  VALUE ZERO.
003990    03 費用負担者番号ＷＲ               PIC X(10) VALUE SPACE.
004000    03 助成種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004010    03 費用負担者番号助成ＷＲ           PIC X(10) VALUE SPACE.
004020    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
004030    03 患者カナＷＲ                     PIC X(20) VALUE SPACE.
004040    03 患者コードＷＲ.
004050       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
004060       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
004070*    ************
004080*    * 料金情報 *
004090*    ************
004100*    月毎の料金
004110***********************
004120 01 料金１ＷＲ.
004130   03 初検ＷＲ.
004140      05 負担割合ＷＲ               PIC 9(3)    VALUE ZERO.
004150      05 初検料ＷＲ                 PIC 9(5)    VALUE ZERO.
004160      05 初検加算料ＷＲ             PIC 9(5)    VALUE ZERO.
         03 初検時相談料ＷＲ              PIC 9(4)    VALUE ZERO.
004170   03 再検料ＷＲ                    PIC 9(5)    VALUE ZERO.
004180   03 往療ＷＲ.
004190      05 往療距離ＷＲ               PIC 9(2)V9  VALUE ZERO.
004200      05 往療回数ＷＲ               PIC 9(2)    VALUE ZERO.
004210      05 往療料ＷＲ                 PIC 9(6)    VALUE ZERO.
004220      05 往療加算料ＷＲ             PIC 9(5)    VALUE ZERO.
004230   03 金属副子加算料ＷＲ            PIC 9(5)    VALUE ZERO.
004240   03 施術情報提供料ＷＲ            PIC 9(5)    VALUE ZERO.
004250   03 合計ＷＲ                      PIC 9(6)    VALUE ZERO.
004260   03 一部負担金ＷＲ                PIC 9(6)    VALUE ZERO.
004270   03 請求金額ＷＲ                  PIC 9(6)    VALUE ZERO.
004280   03 給付割合ＷＲ                  PIC 9(1)    VALUE ZERO.
004290   03 受給者負担額ＷＲ              PIC 9(6)    VALUE ZERO.
004300   03 助成請求金額ＷＲ              PIC 9(6)    VALUE ZERO.
004310*
004320* 負傷部位毎の料金
004330***********************
004340 01 料金２ＷＲ.
004350   03 初回処置ＷＲ    OCCURS   9.
004360      05 初回処置料ＷＲ             PIC 9(5)    VALUE ZERO.
004370*
004380* 逓減毎の料金
004390***********************
004400 01 料金３ＷＲ.
004410**********
004420* １部位 *
004430**********
004440   03 部位１ＷＲ.
004450      05 後療１ＷＲ.
004460         07 後療単価１ＷＲ              PIC 9(4)    VALUE ZERO.
004470         07 後療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
004480         07 後療料１ＷＲ                PIC 9(5)    VALUE ZERO.
004490      05 冷罨法１ＷＲ.
004500         07 冷罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
004510         07 冷罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
004520      05 温罨法１ＷＲ.
004530         07 温罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
004540         07 温罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
004550      05 電療１ＷＲ.
004560         07 電療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
004570         07 電療料１ＷＲ                PIC 9(4)    VALUE ZERO.
004580      05 小計１ＷＲ                     PIC 9(6)    VALUE ZERO.
004590      05 長期逓減率１ＷＲ               PIC 9(3)    VALUE ZERO.
004600      05 長期込小計１ＷＲ               PIC 9(6)    VALUE ZERO.
004610**********
004620* ２部位 *
004630**********
004640   03 部位２ＷＲ.
004650      05 後療２ＷＲ.
004660         07 後療単価２ＷＲ              PIC 9(4)    VALUE ZERO.
004670         07 後療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
004680         07 後療料２ＷＲ                PIC 9(5)    VALUE ZERO.
004690      05 冷罨法２ＷＲ.
004700         07 冷罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
004710         07 冷罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
004720      05 温罨法２ＷＲ.
004730         07 温罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
004740         07 温罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
004750      05 電療２ＷＲ.
004760         07 電療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
004770         07 電療料２ＷＲ                PIC 9(4)    VALUE ZERO.
004780      05 小計２ＷＲ                     PIC 9(6)    VALUE ZERO.
004790      05 長期逓減率２ＷＲ               PIC 9(3)    VALUE ZERO.
004800      05 長期込小計２ＷＲ               PIC 9(6)    VALUE ZERO.
004810******************
004820* ３部位／８割 *
004830******************
004840   03 部位３８ＷＲ.
004850      05 後療３８ＷＲ.
004860         07 後療単価３８ＷＲ              PIC 9(4)  VALUE ZERO.
004870         07 後療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
004880         07 後療料３８ＷＲ                PIC 9(5)  VALUE ZERO.
004890      05 冷罨法３８ＷＲ.
004900         07 冷罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
004910         07 冷罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
004920      05 温罨法３８ＷＲ.
004930         07 温罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
004940         07 温罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
004950      05 電療３８ＷＲ.
004960         07 電療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
004970         07 電療料３８ＷＲ                PIC 9(4)  VALUE ZERO.
004980      05 小計３８ＷＲ                     PIC 9(6)  VALUE ZERO.
004990      05 多部位込小計３８ＷＲ             PIC 9(6)  VALUE ZERO.
005000      05 長期逓減率３８ＷＲ               PIC 9(3)  VALUE ZERO.
005010      05 長期込小計３８ＷＲ               PIC 9(6)  VALUE ZERO.
005020******************
005030* ３部位／１０割 *
005040******************
005050   03 部位３０ＷＲ.
005060      05 逓減開始月日３０ＷＲ.
005070         07 逓減開始月３０ＷＲ            PIC 9(2)  VALUE ZERO.
005080         07 逓減開始日３０ＷＲ            PIC 9(2)  VALUE ZERO.
005090      05 後療３０ＷＲ.
005100         07 後療単価３０ＷＲ              PIC 9(4)  VALUE ZERO.
005110         07 後療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
005120         07 後療料３０ＷＲ                PIC 9(5)  VALUE ZERO.
005130      05 冷罨法３０ＷＲ.
005140         07 冷罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
005150         07 冷罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
005160      05 温罨法３０ＷＲ.
005170         07 温罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
005180         07 温罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
005190      05 電療３０ＷＲ.
005200         07 電療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
005210         07 電療料３０ＷＲ                PIC 9(4)  VALUE ZERO.
005220      05 小計３０ＷＲ                     PIC 9(6)  VALUE ZERO.
005230      05 長期逓減率３０ＷＲ               PIC 9(3)  VALUE ZERO.
005240      05 長期込小計３０ＷＲ               PIC 9(6)  VALUE ZERO.
005250****************
005260* ４部位／５割 *
005270****************
005280   03 部位４５ＷＲ.
005290      05 後療４５ＷＲ.
005300         07 後療単価４５ＷＲ              PIC 9(4)  VALUE ZERO.
005310         07 後療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005320         07 後療料４５ＷＲ                PIC 9(5)  VALUE ZERO.
005330      05 冷罨法４５ＷＲ.
005340         07 冷罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005350         07 冷罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
005360      05 温罨法４５ＷＲ.
005370         07 温罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005380         07 温罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
005390      05 電療４５ＷＲ.
005400         07 電療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005410         07 電療料４５ＷＲ                PIC 9(4)  VALUE ZERO.
005420      05 小計４５ＷＲ                     PIC 9(6)  VALUE ZERO.
005430      05 多部位込小計４５ＷＲ             PIC 9(6)  VALUE ZERO.
005440      05 長期逓減率４５ＷＲ               PIC 9(3)  VALUE ZERO.
005450      05 長期込小計４５ＷＲ               PIC 9(6)  VALUE ZERO.
005460****************
005470* ４部位／８割 *
005480****************
005490   03 部位４８ＷＲ.
005500      05 逓減開始月日４８ＷＲ.
005510         07 逓減開始月４８ＷＲ            PIC 9(2)  VALUE ZERO.
005520         07 逓減開始日４８ＷＲ            PIC 9(2)  VALUE ZERO.
005530      05 後療４８ＷＲ.
005540         07 後療単価４８ＷＲ              PIC 9(4)  VALUE ZERO.
005550         07 後療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
005560         07 後療料４８ＷＲ                PIC 9(5)  VALUE ZERO.
005570      05 冷罨法４８ＷＲ.
005580         07 冷罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
005590         07 冷罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
005600      05 温罨法４８ＷＲ.
005610         07 温罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
005620         07 温罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
005630      05 電療４８ＷＲ.
005640         07 電療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
005650         07 電療料４８ＷＲ                PIC 9(4)  VALUE ZERO.
005660      05 小計４８ＷＲ                     PIC 9(6)  VALUE ZERO.
005670      05 多部位込小計４８ＷＲ             PIC 9(6)  VALUE ZERO.
005680      05 長期逓減率４８ＷＲ               PIC 9(3)  VALUE ZERO.
005690      05 長期込小計４８ＷＲ               PIC 9(6)  VALUE ZERO.
005700******************
005710* ４部位／１０割 *
005720******************
005730   03 部位４０ＷＲ.
005740      05 逓減開始月日４０ＷＲ.
005750         07 逓減開始月４０ＷＲ            PIC 9(2)  VALUE ZERO.
005760         07 逓減開始日４０ＷＲ            PIC 9(2)  VALUE ZERO.
005770      05 後療４０ＷＲ.
005780         07 後療単価４０ＷＲ              PIC 9(4)  VALUE ZERO.
005790         07 後療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
005800         07 後療料４０ＷＲ                PIC 9(5)  VALUE ZERO.
005810      05 冷罨法４０ＷＲ.
005820         07 冷罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
005830         07 冷罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
005840      05 温罨法４０ＷＲ.
005850         07 温罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
005860         07 温罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
005870      05 電療４０ＷＲ.
005880         07 電療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
005890         07 電療料４０ＷＲ                PIC 9(4)  VALUE ZERO.
005900      05 小計４０ＷＲ                     PIC 9(6)  VALUE ZERO.
005910      05 長期逓減率４０ＷＲ               PIC 9(3)  VALUE ZERO.
005920      05 長期込小計４０ＷＲ               PIC 9(6)  VALUE ZERO.
005930********************
005940* ５部位／２．５割 *
005950********************
005960   03 部位５２ＷＲ.
005970      05 後療５２ＷＲ.
005980         07 後療単価５２ＷＲ              PIC 9(4)  VALUE ZERO.
005990         07 後療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
006000         07 後療料５２ＷＲ                PIC 9(5)  VALUE ZERO.
006010      05 冷罨法５２ＷＲ.
006020         07 冷罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
006030         07 冷罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
006040      05 温罨法５２ＷＲ.
006050         07 温罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
006060         07 温罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
006070      05 電療５２ＷＲ.
006080         07 電療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
006090         07 電療料５２ＷＲ                PIC 9(4)  VALUE ZERO.
006100      05 小計５２ＷＲ                     PIC 9(6)  VALUE ZERO.
006110      05 多部位込小計５２ＷＲ             PIC 9(6)  VALUE ZERO.
006120      05 長期逓減率５２ＷＲ               PIC 9(3)  VALUE ZERO.
006130      05 長期込小計５２ＷＲ               PIC 9(6)  VALUE ZERO.
006140****************
006150* ５部位／５割 *
006160****************
006170   03 部位５５ＷＲ.
006180      05 逓減開始月日５５ＷＲ.
006190         07 逓減開始月５５ＷＲ            PIC 9(2)  VALUE ZERO.
006200         07 逓減開始日５５ＷＲ            PIC 9(2)  VALUE ZERO.
006210      05 後療５５ＷＲ.
006220         07 後療単価５５ＷＲ              PIC 9(4)  VALUE ZERO.
006230         07 後療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
006240         07 後療料５５ＷＲ                PIC 9(5)  VALUE ZERO.
006250      05 冷罨法５５ＷＲ.
006260         07 冷罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
006270         07 冷罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
006280      05 温罨法５５ＷＲ.
006290         07 温罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
006300         07 温罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
006310      05 電療５５ＷＲ.
006320         07 電療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
006330         07 電療料５５ＷＲ                PIC 9(4)  VALUE ZERO.
006340      05 小計５５ＷＲ                     PIC 9(6)  VALUE ZERO.
006350      05 多部位込小計５５ＷＲ             PIC 9(6)  VALUE ZERO.
006360      05 長期逓減率５５ＷＲ               PIC 9(3)  VALUE ZERO.
006370      05 長期込小計５５ＷＲ               PIC 9(6)  VALUE ZERO.
006380****************
006390* ５部位／８割 *
006400****************
006410   03 部位５８ＷＲ.
006420      05 逓減開始月日５８ＷＲ.
006430         07 逓減開始月５８ＷＲ            PIC 9(2)  VALUE ZERO.
006440         07 逓減開始日５８ＷＲ            PIC 9(2)  VALUE ZERO.
006450      05 後療５８ＷＲ.
006460         07 後療単価５８ＷＲ              PIC 9(4)  VALUE ZERO.
006470         07 後療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
006480         07 後療料５８ＷＲ                PIC 9(5)  VALUE ZERO.
006490      05 冷罨法５８ＷＲ.
006500         07 冷罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
006510         07 冷罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
006520      05 温罨法５８ＷＲ.
006530         07 温罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
006540         07 温罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
006550      05 電療５８ＷＲ.
006560         07 電療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
006570         07 電療料５８ＷＲ                PIC 9(4)  VALUE ZERO.
006580      05 小計５８ＷＲ                     PIC 9(6)  VALUE ZERO.
006590      05 多部位込小計５８ＷＲ             PIC 9(6)  VALUE ZERO.
006600      05 長期逓減率５８ＷＲ               PIC 9(3)  VALUE ZERO.
006610      05 長期込小計５８ＷＲ               PIC 9(6)  VALUE ZERO.
006620******************
006630* ５部位／１０割 *
006640******************
006650   03 部位５０ＷＲ.
006660      05 逓減開始月日５０ＷＲ.
006670         07 逓減開始月５０ＷＲ            PIC 9(2)  VALUE ZERO.
006680         07 逓減開始日５０ＷＲ            PIC 9(2)  VALUE ZERO.
006690      05 後療５０ＷＲ.
006700         07 後療単価５０ＷＲ              PIC 9(4)  VALUE ZERO.
006710         07 後療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
006720         07 後療料５０ＷＲ                PIC 9(5)  VALUE ZERO.
006730      05 冷罨法５０ＷＲ.
006740         07 冷罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
006750         07 冷罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
006760      05 温罨法５０ＷＲ.
006770         07 温罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
006780         07 温罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
006790      05 電療５０ＷＲ.
006800         07 電療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
006810         07 電療料５０ＷＲ                PIC 9(4)  VALUE ZERO.
006820      05 小計５０ＷＲ                     PIC 9(6)  VALUE ZERO.
006830      05 長期逓減率５０ＷＲ               PIC 9(3)  VALUE ZERO.
006840      05 長期込小計５０ＷＲ               PIC 9(6)  VALUE ZERO.
006850*
006860**************
006870* 施術所情報 *
006880**************
006890 01 施術所情報Ｗ.
006900    03 柔整師番号Ｗ                    PIC X(16)  VALUE SPACE.
006910    03 接骨師会会員番号Ｗ              PIC X(16)  VALUE SPACE.
006920    03 代表者カナＷ                    PIC X(50)  VALUE SPACE.
006930    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
006940    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
          03 都道府県ＪＩＳＷ                PIC X(2)   VALUE SPACE.
006950    03 施術所住所Ｗ.
006960       05 施術所住所１Ｗ               PIC X(50)  VALUE SPACE.
006970       05 施術所住所２Ｗ               PIC X(50)  VALUE SPACE.
006980    03 施術所郵便番号Ｗ.
006990       05 施術所郵便番号１Ｗ           PIC X(3)   VALUE SPACE.
007000       05 施術所郵便番号２Ｗ           PIC X(4)   VALUE SPACE.
007010    03 施術所電話番号Ｗ                PIC X(20)  VALUE SPACE.
007020    03 定額制受理番号Ｗ                PIC X(15)  VALUE SPACE.
007030    03 受理年月日Ｗ.
007040       05 受理年Ｗ                     PIC 9(2)   VALUE ZERO.
007050       05 受理月Ｗ                     PIC 9(2)   VALUE ZERO.
007060       05 受理日Ｗ                     PIC 9(2)   VALUE ZERO.
007070    03 最終通院年月日Ｗ.
007080       05 最終通院年Ｗ                 PIC 9(2)   VALUE ZERO.
007090       05 最終通院月Ｗ                 PIC 9(2)   VALUE ZERO.
007100       05 最終通院日Ｗ                 PIC 9(2)   VALUE ZERO.
007110    03 柔整師年月日Ｗ.
007120       05 柔整師年Ｗ                   PIC 9(2)   VALUE ZERO.
007130       05 柔整師月Ｗ                   PIC 9(2)   VALUE ZERO.
007140       05 柔整師日Ｗ                   PIC 9(2)   VALUE ZERO.
007150    03 患者委任年月日Ｗ.
007160       05 患者委任年Ｗ                 PIC 9(2)   VALUE ZERO.
007170       05 患者委任月Ｗ                 PIC 9(2)   VALUE ZERO.
007180       05 患者委任日Ｗ                 PIC 9(2)   VALUE ZERO.
007190    03 取引先情報Ｗ.
007200        05 取引先銀行名Ｗ              PIC X(40)  VALUE SPACE.
007210        05 取引先銀行支店名Ｗ          PIC X(40)  VALUE SPACE.
007220        05 預金種別Ｗ                  PIC 9(1)   VALUE ZERO.
007230        05 口座番号Ｗ                  PIC X(10)  VALUE SPACE.
007240        05 口座名義人Ｗ                PIC X(40)  VALUE SPACE.
007250        05 口座名義人カナＷ            PIC X(40)  VALUE SPACE.
007260        05 口座名義人とカナＷ          PIC X(60)  VALUE SPACE.
007270        05 銀行名支店名Ｗ              PIC X(60)  VALUE SPACE.
007280        05 預金種別コメントＷ          PIC N(3)   VALUE SPACE.
007290    03 コメントＷ.
007300        05 コメント１Ｗ                PIC N(15)  VALUE SPACE.
007310        05 コメント２Ｗ                PIC N(15)  VALUE SPACE.
007320        05 コメント３Ｗ                PIC N(15)  VALUE SPACE.
007330    03 共済番号Ｗ                      PIC X(28)  VALUE SPACE.
007340**************
007350* 受診者情報 *
007360**************
007370 01 受診者情報Ｗ.
      */元号修正/20190408
          03 施術和暦Ｗ                      PIC 9(1)   VALUE ZERO.
007380    03 施術年月Ｗ.
007390       05 施術年Ｗ                     PIC 9(2)   VALUE ZERO.
007400       05 施術月Ｗ                     PIC 9(2)   VALUE ZERO.
007410*    03 記号Ｗ                          PIC N(12)  VALUE SPACE.
007570    03 記号Ｗ.
007580       05 印刷記号Ｗ                   PIC N(12)  VALUE SPACE.
          03 記号番号Ｗ.
             05 記号番号ＸＷ                 PIC X(40) VALUE SPACE.
007420    03 番号Ｗ.
007430       05 印刷番号Ｗ                   PIC X(15)  VALUE SPACE.
007440       05 FILLER                       PIC X(15)  VALUE SPACE.
007450    03 保険者番号Ｗ.
007460       05 印刷保険者番号Ｗ             PIC X(8)   VALUE SPACE.
007470       05 FILLER                       PIC X(2)   VALUE SPACE.
007480    03 市町村番号Ｗ.
007490       05 印刷市町村番号Ｗ             PIC X(8)   VALUE SPACE.
007500       05 FILLER                       PIC X(2)   VALUE SPACE.
007510*
007520    03 請求先名称Ｗ.
007530       05 印刷請求先名称１Ｗ           PIC X(38)  VALUE SPACE.
007540       05 印刷請求先名称２Ｗ           PIC X(38)  VALUE SPACE.
007550*
007560    03 保険種別Ｗ                      PIC 9(2)   VALUE ZERO.
007570    03 被保険者情報Ｗ.
007580       05 被保険者カナＷ               PIC X(50)  VALUE SPACE.
007590       05 被保険者氏名Ｗ               PIC X(50)  VALUE SPACE.
007600       05 郵便番号Ｗ.
007610          07 郵便番号１Ｗ              PIC X(3)   VALUE SPACE.
007620          07 郵便番号２Ｗ              PIC X(4)   VALUE SPACE.
007630       05 被保険者住所１Ｗ             PIC X(50)  VALUE SPACE.
007640       05 被保険者住所２Ｗ             PIC X(50)  VALUE SPACE.
008990       05 電話番号Ｗ                   PIC X(35)  VALUE SPACE.
007650    03 患者情報Ｗ.
007660       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
007670       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
007680       05 患者住所１Ｗ                 PIC X(40)  VALUE SPACE.
007690       05 患者住所２Ｗ                 PIC X(40)  VALUE SPACE.
007700       05 性別チェックＷ.
007710          07 男チェックＷ              PIC N(1)  VALUE SPACE.
007720          07 女チェックＷ              PIC N(1)  VALUE SPACE.
007730       05 患者性別Ｗ.
007740          07 性別Ｗ                    PIC N(1)  VALUE SPACE.
007750       05 和暦チェックＷ.
007760          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
007770          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
007780          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
007790          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
007800          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
      */元号修正/↓↓↓20190408
008210          07 令和チェックＷ            PIC N(1)  VALUE SPACE.
                07 令和ＣＭＷ                PIC X(4)  VALUE SPACE.
009110*          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
      */元号修正/↑↑↑20190408
007810       05 患者年Ｗ                     PIC 9(2)  VALUE ZERO.
007820       05 患者月Ｗ                     PIC 9(2)  VALUE ZERO.
007830       05 患者日Ｗ                     PIC 9(2)  VALUE ZERO.
007840       05 続柄Ｗ.
007850          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
007860          07 FILLER                    PIC X(4)  VALUE SPACE.
007870*
007880       05 負傷原因１Ｗ                 PIC N(37) VALUE SPACE.
007890       05 負傷原因２Ｗ                 PIC N(37) VALUE SPACE.
007900       05 負傷原因３Ｗ                 PIC N(37) VALUE SPACE.
007910       05 負傷原因４Ｗ                 PIC N(37) VALUE SPACE.
007920       05 負傷原因５Ｗ                 PIC N(37) VALUE SPACE.
007930*
008370*       05 負傷原因Ｗ                   PIC N(40) OCCURS 29 VALUE SPACE.
      */半角対応/110421
             05 負傷原因Ｗ OCCURS 29.
                07 負傷原因ＸＷ              PIC X(80)  VALUE SPACE.
007950    03 助成印Ｗ                        PIC N(1)  VALUE SPACE.
007960    03 特別コメントＷ                  PIC X(16) VALUE SPACE.
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
007970*
007980****************
007990* 負傷データＦ *
008000****************
008010 01 負傷情報Ｗ.
008020    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
008030    03 部位情報Ｗ  OCCURS   9.
008040       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
008050       05 部位コードＷ.
008060          07 負傷種別Ｗ                PIC 9(2)  VALUE ZERO.
008070          07 部位Ｗ                    PIC 9(2)  VALUE ZERO.
008080          07 左右区分Ｗ                PIC 9(1)  VALUE ZERO.
008090          07 負傷位置番号Ｗ            PIC 9(2)  VALUE ZERO.
008100       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
008110       05 負傷年月日Ｗ.
008120          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
008130          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
008140          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
008150       05 初検年月日Ｗ.
008160          07 初検年Ｗ                  PIC 9(2)  VALUE ZERO.
008170          07 初検月Ｗ                  PIC 9(2)  VALUE ZERO.
008180          07 初検日Ｗ                  PIC 9(2)  VALUE ZERO.
008190       05 開始年月日Ｗ.
008200          07 開始年Ｗ                  PIC 9(2)  VALUE ZERO.
008210          07 開始月Ｗ                  PIC 9(2)  VALUE ZERO.
008220          07 開始日Ｗ                  PIC 9(2)  VALUE ZERO.
008230       05 終了年月日Ｗ.
008240          07 終了年Ｗ                  PIC 9(2)  VALUE ZERO.
008250          07 終了月Ｗ                  PIC 9(2)  VALUE ZERO.
008260          07 終了日Ｗ                  PIC 9(2)  VALUE ZERO.
008270       05 実日数Ｗ                     PIC 9(2)  VALUE ZERO.
008280       05 転帰区分Ｗ                   PIC 9(1)  VALUE ZERO.
008290       05 転帰区分チェックＷ.
008300          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
008310          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
008320          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
008330       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
008340       05 部位区切Ｗ                   PIC X(1)  VALUE SPACE.
008350       05 経過略称Ｗ.
008360          07 印刷経過略称Ｗ            PIC N(5)  VALUE SPACE.
008370          07 FILLER                    PIC X(2)  VALUE SPACE.
008380    03 経過部位Ｗ                      PIC N(1)  VALUE SPACE.
008390    03 新規チェックＷ                  PIC N(1)  VALUE SPACE.
008400    03 継続チェックＷ                  PIC N(1)  VALUE SPACE.
          03 施術日Ｗ.
             05 施術日チェックＷ   OCCURS 31 PIC N(1)  VALUE SPACE.
008410****************
008420* ＯＣＲコード *
008430****************
008440* 01 ＯＣＲコードＷ.
008450*    03 ＯＣＲ会員番号Ｗ                PIC X(6)    VALUE ZERO.
008460*    03 ＯＣＲ施術年月Ｗ                PIC 9(4)    VALUE ZERO.
008470*    03 ＯＣＲ保険種別Ｗ                PIC 9(2)    VALUE ZERO.
008480*    03 ＯＣＲ各社ＩＤＷ                PIC 9(2)    VALUE ZERO.
008490*    03 ＯＣＲＦＤ連番Ｗ.
008500*       05 ＦＤ連番患者番号Ｗ           PIC 9(6)    VALUE ZERO.
008510*       05 ＦＤ連番健保ＩＤＷ           PIC 9(1)    VALUE ZERO.
008520*    03 ＯＣＲ請求金額Ｗ                PIC 9(6)    VALUE ZERO.
008530*    03 ＯＣＲ負担割合Ｗ                PIC 9(1)    VALUE ZERO.
008540*    03 ＯＣＲ接骨師会会員番号Ｗ        PIC 9(10)   VALUE ZERO.
008550*
008560************
008570* 料金情報 *
008580************
008590 01 料金情報Ｗ.
008600    03 初検加算Ｗ.
008610       05 時間外チェックＷ                PIC N(1) VALUE SPACE.
008620       05 休日チェックＷ                  PIC N(1) VALUE SPACE.
008630       05 深夜チェックＷ                  PIC N(1) VALUE SPACE.
008640    03 往療加算Ｗ.
008650       05 夜間チェックＷ                  PIC N(1) VALUE SPACE.
008660       05 暴風雨雪チェックＷ              PIC N(1) VALUE SPACE.
008670    03 金属副子チェックＷ.
008680       05 大チェックＷ                    PIC N(1) VALUE SPACE.
008690       05 中チェックＷ                    PIC N(1) VALUE SPACE.
008700       05 小チェックＷ                    PIC N(1) VALUE SPACE.
008710    03 小計Ｗ                             PIC 9(7) VALUE ZERO.
008720    03 初回処置料合計Ｗ                   PIC 9(6) VALUE ZERO.
008730    03 初回処置料チェックＷ.
008740       05 整復料チェックＷ                PIC N(1) VALUE SPACE.
008750       05 固定料チェックＷ                PIC N(1) VALUE SPACE.
008760       05 施療料チェックＷ                PIC N(1) VALUE SPACE.
      */金属副子・運動後療の変更・追加/1805
          03 金属回数Ｗ                         PIC 9(2)  VALUE ZERO.
          03 運動料Ｗ                           PIC 9(4)  VALUE ZERO.
008770************
008780* 備考情報 *
008790************
008800 01 備考情報Ｗ.
008810    03 適用１Ｗ                        PIC N(38) VALUE SPACE.
008820    03 適用２Ｗ                        PIC N(38) VALUE SPACE.
008830*    03 適用３Ｗ                        PIC N(38) VALUE SPACE.
008840*    03 適用４Ｗ                        PIC N(38) VALUE SPACE.
008850    03 経過コメントＷ                  PIC N(60) VALUE SPACE.
008860*
008870*****************
008880* レセプト並び順 *
008890*****************
008900 01 順番固定Ｗ                         PIC N(1) VALUE SPACE.
008910 01 順番Ｗ                             PIC 9(4) VALUE ZERO.
008920*
       01 摘要施術日Ｗ                       PIC X(100) VALUE SPACE.
       01 施術日Ｗ.
          03 施術日２Ｗ                      PIC X(1)  VALUE SPACE.
          03 施術日１Ｗ                      PIC X(1)  VALUE SPACE.
002790** 負担割合用
002800 01 負担割合Ｗ                         PIC 9(2)  VALUE ZERO.
002810 01 給付割合Ｗ                         PIC 9(2)  VALUE ZERO.
008930********************
008940 01 印刷制御.
008950     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
008960     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
008970     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
008980     03 拡張制御Ｐ.
008990         05 端末制御Ｐ.
009000             07 移動方向Ｐ             PIC X(1) VALUE SPACE.
009010             07 移動行数Ｐ             PIC 9(3) VALUE ZERO.
009020         05 詳細制御Ｐ                 PIC X(2) VALUE SPACE.
009030     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
009040     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
009050*
009060 01 計算機西暦年Ｗ                     PIC 9(2) VALUE ZERO.
009070* 日付ＷＯＲＫ
009080 01 和暦終了年Ｗ                       PIC 9(4) VALUE ZERO.
009090 01 計算機西暦.
009100    03 計算機西暦年                    PIC 9(4) VALUE ZERO.
009110    03 計算機西暦月日                  PIC 9(4) VALUE ZERO.
009120 01 計算機西暦Ｒ REDEFINES 計算機西暦.
009130    03 計算機世紀                      PIC 9(2).
009140    03 計算機日付                      PIC 9(6).
009150    03 計算機日付Ｒ REDEFINES 計算機日付.
009160       05 計算機年月                   PIC 9(4).
009170       05 計算機年月Ｒ REDEFINES 計算機年月.
009180         07 計算機年                   PIC 9(2).
009190         07 計算機月                   PIC 9(2).
009200       05 計算機日                     PIC 9(2).
009210*
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
009220******************************************************************
009230*                          連結項目                              *
009240******************************************************************
009250**  画面入力データ
009260 01 連入－入力データ委任印刷 IS EXTERNAL.
009270    03 連入－委任印刷                     PIC 9.
014620*
       01 連入－入力データ電話印刷 IS EXTERNAL.
          03 連入－電話印刷                     PIC 9.
009190*
       01 連入－プレビュー IS EXTERNAL.
          03 連入－プレビュー区分          PIC 9.
009280*
009290** ３カ月長期判定
009300 01 連期間－キー IS EXTERNAL.
009310    03 連期間－施術年月.
009320       05 連期間－施術和暦               PIC 9.
009330 05 連期間－施術年                 PIC 9(2).
009340 05 連期間－施術月                 PIC 9(2).
009350    03  連期間－患者コード.
009360       05 連期間－患者番号               PIC 9(6).
009370       05 連期間－枝番                   PIC X.
009380    03 連期間－対象フラグ                PIC X(3).
009390    03 連期間－期間月Ｗ.
009400       05 連期間－期間Ｗ                 PIC 9(2) OCCURS 9.
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
009410************
009420* 印刷キー *
009430************
009460 01 連レ印－対象データ IS EXTERNAL.
009470    03 連レ印－施術年月日.
009480       05 連レ印－施術和暦                  PIC 9(1).
009490       05 連レ印－施術年                    PIC 9(2).
009500       05 連レ印－施術月                    PIC 9(2).
009510    03 連レ印－患者コード.
009520       05 連レ印－患者番号                  PIC 9(6).
009530       05 連レ印－枝番                      PIC X(1).
009540    03 連レ印－保険種別                     PIC 9(2).
009550    03 連レ印－保険者番号                   PIC X(10).
009560    03 連レ印－公費種別                     PIC 9(2).
009570    03 連レ印－費用負担者番号               PIC X(10).
009580    03 連レ印－助成種別                     PIC 9(2).
009590    03 連レ印－費用負担者番号助成           PIC X(10).
009600    03 連レ印－患者カナ                     PIC X(20).
009610    03 連レ印－本人家族区分                 PIC 9(1).
009620*
013050 01 連レ－キー IS EXTERNAL.
013060    03 連レ－保険種別                  PIC 9(2).
      *
       01 連レ－長野キー IS EXTERNAL GLOBAL.
          03 連レ－長野助成                  PIC 9(2).
013070*
013080************************
013090* 長期理由文セット     *
013100************************
013110 01 連長文－キー IS EXTERNAL.
013120    03 連長文－施術年月.
013130       05 連長文－施術和暦               PIC 9.
013140       05 連長文－施術年                 PIC 9(2).
013150       05 連長文－施術月                 PIC 9(2).
013160    03  連長文－患者コード.
013170       05 連長文－患者番号               PIC 9(6).
013180       05 連長文－枝番                   PIC X.
013190    03 連長文－文桁数                    PIC 9(2).
013200    03 連長文－理由文                    PIC N(63) OCCURS 15.
013210*
013211*
013212*************
013213* 助成名称
013214*************
013215 01 連助成名称－キー IS EXTERNAL.
013216    03 連助成名称－助成種別             PIC 9(2).
013217    03 連助成名称－費用負担者番号助成   PIC X(10).
013218*   / OUT /
013219    03 連助成名称－名称集団.
013220       05 連助成名称－１文字            PIC N.
013221       05 連助成名称－略称              PIC N(4).
013222       05 連助成名称－正式名称          PIC N(10).
013223*
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
013224*
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
013225******************************************************************
013230*                      PROCEDURE  DIVISION                       *
013240******************************************************************
013250 PROCEDURE               DIVISION.
013260************
013270*           *
013280* 初期処理   *
013290*           *
013300************
002570     PERFORM プリンタファイル作成.
013310     PERFORM 初期化.
013320************
013330*           *
013340* 主処理     *
013350*           *
013360************
013370* 印刷
013380     PERFORM 連結項目待避.
013390     PERFORM 印刷セット.
013400     PERFORM 印刷処理.
013410************
013420*           *
013430* 終了処理   *
013440*           *
013450************
013460     PERFORM 受診者印刷区分更新.
013470     PERFORM 終了処理.
013480     MOVE ZERO  TO PROGRAM-STATUS.
013490     EXIT PROGRAM.
013500*
013510*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YCH6225"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分  TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
013520*================================================================*
013530 初期化 SECTION.
013540*
013550     PERFORM ファイルオープン.
013560*    /* 現在日付取得 */
013570     ACCEPT 計算機日付 FROM DATE.
013580*    /* 1980～2079年の間で設定 */
013590     IF ( 計算機年 > 80 )
013600         MOVE 19 TO 計算機世紀
013610     ELSE
013620         MOVE 20 TO 計算機世紀
013630     END-IF.
013640     PERFORM カレント元号取得.
013650     PERFORM 和暦終了年取得.
013660     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 和暦終了年Ｗ.
013670*================================================================*
013680 カレント元号取得 SECTION.
013690*
013700     MOVE ZEROS TO 制－制御区分.
013710     READ 制御情報マスタ
013720     NOT INVALID KEY
013730         MOVE 制－カレント元号         TO カレント元号Ｗ
013740         MOVE 制－レセ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
013750         MOVE 制－レセ長期理由印刷区分 TO 長期理由印刷区分Ｗ
013760         MOVE 制－レセプト日付区分     TO レセプト日付区分Ｗ
013770         MOVE 制－レセプト患者日付区分 TO レセプト患者日付区分Ｗ
013771         MOVE 制－全柔ＦＰＤ区分       TO 全柔ＦＰＤ区分Ｗ
015320         MOVE 制－共済本人レセ         TO 用紙種別Ｗ
013780     END-READ.
013790*
013800*================================================================*
013810 和暦終了年取得 SECTION.
013820*
013830*     DISPLAY NC"カレント元号Ｗ"  カレント元号Ｗ UPON MSGBOX.
013840     MOVE カレント元号Ｗ TO 元－元号区分.
013850     READ 元号マスタ
013860     INVALID KEY
013870         DISPLAY NC"指定和暦が登録されていません" UPON CONS
013880         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
013890                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
013900         ACCEPT  キー入力 FROM CONS
013910         PERFORM 終了処理
013920         EXIT PROGRAM
013930     NOT INVALID KEY
013940         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
013950         MOVE 前和暦Ｗ TO 元－元号区分
013960         READ 元号マスタ
013970         INVALID KEY
013980             DISPLAY NC"指定和暦が登録されていません" UPON CONS
013990             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
014000                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
014010             ACCEPT  キー入力 FROM CONS
014020             PERFORM 終了処理
014030             EXIT PROGRAM
014040         NOT INVALID KEY
014050             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
014060         END-READ
014070     END-READ.
014080*
014090*================================================================*
014100 ファイルオープン SECTION.
014110*
014120     OPEN INPUT   保険者マスタ
014130         MOVE NC"保険者" TO ファイル名.
014140         PERFORM オープンチェック.
014150     OPEN INPUT   元号マスタ
014160         MOVE NC"元号" TO ファイル名.
014170         PERFORM オープンチェック.
014180     OPEN INPUT   名称マスタ
014190         MOVE NC"名称" TO ファイル名.
014200         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
014240     OPEN INPUT   制御情報マスタ
014250         MOVE NC"制御情報" TO ファイル名.
014260         PERFORM オープンチェック.
014270     OPEN INPUT   施術所情報マスタ
014280         MOVE NC"施情" TO ファイル名.
014290         PERFORM オープンチェック.
014300     OPEN INPUT   請求先マスタ
014310         MOVE NC"請先" TO ファイル名.
014320         PERFORM オープンチェック.
014330     OPEN INPUT   経過マスタ
014340         MOVE NC"経過" TO ファイル名.
014350         PERFORM オープンチェック.
014360     OPEN INPUT   施術記録Ｆ.
014370         MOVE NC"施記Ｆ" TO ファイル名.
014380         PERFORM オープンチェック.
014390     OPEN INPUT   負傷データＦ.
014400         MOVE NC"負傷" TO ファイル名.
014410         PERFORM オープンチェック.
014420     OPEN INPUT   負傷原因Ｆ.
014430         MOVE NC"負傷原因" TO ファイル名.
014440         PERFORM オープンチェック.
014450     OPEN INPUT メモファイル.
014460         MOVE NC"メモ" TO ファイル名.
014470         PERFORM オープンチェック.
014480     OPEN INPUT   作業ファイル４.
015170         IF ( 状態キー  NOT =  "00" )
015060            OPEN OUTPUT  作業ファイル４
                  CLOSE 作業ファイル４
015060            OPEN INPUT  作業ファイル４
               END-IF.
014510     OPEN I-O   受診者情報Ｆ.
014520         MOVE NC"受情" TO ファイル名.
014530         PERFORM オープンチェック.
014540     OPEN I-O   印刷ファイル
014550         PERFORM エラー処理Ｐ.
014560*================================================================*
014570 オープンチェック SECTION.
014580*
014590     IF ( 状態キー  NOT =  "00" )
014600         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
014610         DISPLAY NC"状態キー：" 状態キー         UPON CONS
014620         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
014630                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
014640         ACCEPT  キー入力 FROM CONS
014650         PERFORM ファイル閉鎖
014660         EXIT PROGRAM.
014670*================================================================*
014680 連結項目待避 SECTION.
014690*
014700     MOVE 連レ印－施術和暦           TO 施術和暦ＷＲ.
014710     MOVE 連レ印－施術年             TO 施術年ＷＲ.
014720     MOVE 連レ印－施術月             TO 施術月ＷＲ.
014730     MOVE 連レ印－保険種別           TO 保険種別ＷＲ.
014740     MOVE 連レ印－保険者番号         TO 保険者番号ＷＲ.
014750     MOVE 連レ印－公費種別           TO 公費種別ＷＲ.
014760     MOVE 連レ印－費用負担者番号     TO 費用負担者番号ＷＲ.
014770     MOVE 連レ印－助成種別           TO 助成種別ＷＲ.
014780     MOVE 連レ印－費用負担者番号助成 TO 費用負担者番号助成ＷＲ.
014790     MOVE 連レ印－本人家族区分       TO 本人家族区分ＷＲ.
014800     MOVE 連レ印－患者カナ           TO 患者カナＷＲ.
014810     MOVE 連レ印－患者番号           TO 患者番号ＷＲ.
014820     MOVE 連レ印－枝番               TO 枝番ＷＲ.
014830*================================================================*
014840 印刷セット SECTION.
014850*
014860     PERFORM 項目初期化.
014800     PERFORM 負傷読込.
014910     PERFORM 料金情報取得.
014870     PERFORM 施術所情報取得.
014880     PERFORM 請求先情報取得.
014890     PERFORM 受診者情報取得.
014900     PERFORM 負傷データ取得.
014920     PERFORM 施術記録取得.
014930     PERFORM レセプト並び順取得.
014940**     PERFORM ＯＣＲ情報取得.
014950**     PERFORM 長期判定取得.
014960**     PERFORM 初検日以前のデータ判定.
014970     PERFORM 初検加算時刻取得.
014980     PERFORM 助成印取得.
014990     PERFORM 委任年月日取得.
           PERFORM 施術日取得.
015000*
016791*-----------------------------------------------*
016800     IF ( 負傷原因印刷区分Ｗ  NOT = 1 ) AND ( レセ負傷原因印刷区分Ｗ NOT = 1 )
016813        IF ( 負傷原因印刷区分Ｗ = 3 OR 4 )
016815           PERFORM 負傷原因印刷対象判定処理
016817        ELSE
016820           PERFORM 負傷原因取得
016821        END-IF
016830     END-IF.
015130*
015140**********************
015150* ＯＣＲコードセット *
015160**********************
015170*
015180*     MOVE ＯＣＲ接骨師会会員番号Ｗ     TO ＯＣＲ会員番号.
015190*     MOVE ＯＣＲ施術年月Ｗ     TO ＯＣＲ施術年月.
015200*     MOVE ＯＣＲ保険種別Ｗ     TO ＯＣＲ保険種別.
015210*     MOVE ＯＣＲ各社ＩＤＷ     TO ＯＣＲ各社ＩＤ.
015220*     MOVE ＯＣＲＦＤ連番Ｗ     TO ＯＣＲＦＤ連番.
015230*     MOVE ＯＣＲ請求金額Ｗ     TO ＯＣＲ請求金額.
015240*     MOVE ＯＣＲ負担割合Ｗ     TO ＯＣＲ負担割合.
015250*
015260********************
015270* 受診者情報セット *
015280********************
015290     MOVE 施術年Ｗ            TO 施術年.
015300     MOVE 施術月Ｗ            TO 施術月.
015310*
015320*
015330*     IF ( 記号Ｗ(1:1) = NC"＊" )
015340*        MOVE  SPACE    TO  記号
015350*     ELSE
015360*        MOVE 記号Ｗ    TO  記号
015370*     END-IF.
015380*     IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
015390*        ( 印刷番号Ｗ(1:2) = "＊" )
015400*        MOVE  SPACE      TO  番号
015410*     ELSE
015420*        MOVE 印刷番号Ｗ  TO  番号
015430*     END-IF.
015440*
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
015450*
015460     MOVE 印刷保険者番号Ｗ    TO 保険者番号.
015470*     MOVE 印刷請求先名称１Ｗ  TO 保険者名称.
015480*     MOVE 印刷請求先名称２Ｗ  TO 保険者名称２.
015190     MOVE 社保チェックＷ     TO 社保チェック.
015210     MOVE 組合チェックＷ     TO 組合チェック.
015220     MOVE 国保チェックＷ     TO 国保チェック.
           MOVE 共済チェックＷ     TO 共済チェック.
           MOVE 自チェックＷ       TO 自チェック.
           IF 自チェックＷ NOT = SPACE
               MOVE NC"自"         TO 自マーク
           END-IF.
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
015490*
015500*     MOVE 被保険者カナＷ      TO 被保険者カナ.
015510     MOVE 被保険者氏名Ｗ      TO 被保険者氏名.
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
015550     MOVE 被保険者住所１Ｗ    TO 住所１.
015560     MOVE 被保険者住所２Ｗ    TO 住所２.
015570*     MOVE 患者カナＷ          TO 患者カナ.
015580     MOVE 患者氏名Ｗ          TO 患者氏名.
015590*     MOVE 患者住所１Ｗ        TO 患者住所１.
015600*     MOVE 患者住所２Ｗ        TO 患者住所２.
015610     MOVE 男チェックＷ        TO 男チェック.
015620     MOVE 女チェックＷ        TO 女チェック.
015630*     MOVE 性別Ｗ              TO 性別.
015640*     MOVE 明治チェックＷ      TO 明治チェック.
015650*     MOVE 大正チェックＷ      TO 大正チェック.
015660     MOVE 昭和チェックＷ      TO 昭和チェック.
015670     MOVE 平成チェックＷ      TO 平成チェック.
015680*     MOVE 元号Ｗ              TO 元号.
      */元号修正↓↓↓/20190408
           MOVE 令和ＣＭＷ         TO 令和ＣＭ.
023070     MOVE 令和チェックＷ     TO 令和チェック.
017390*     MOVE 元号Ｗ              TO 患者和暦.
      */元号修正↑↑↑/20190408
015690     MOVE 患者年Ｗ            TO 患者年.
015700     MOVE 患者月Ｗ            TO 患者月.
015710     MOVE 患者日Ｗ            TO 患者日.
015720*     MOVE 印刷続柄Ｗ          TO 続柄.
015730     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
015740     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
015750     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
015760     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
015770     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
016480     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
015780*
015790     MOVE 助成印Ｗ            TO 助成印.
015800********************
015810* 長期理由文セット *
015820********************
015830*     MOVE 連長文－理由文(1)    TO 長期理由文１.
015840*     MOVE 連長文－理由文(2)    TO 長期理由文２.
015850*     MOVE 連長文－理由文(3)    TO 長期理由文３.
015860*     MOVE 連長文－理由文(4)    TO 長期理由文４.
015870*     MOVE 連長文－理由文(5)    TO 長期理由文５.
015880*     MOVE 連長文－理由文(6)    TO 長期理由文６.
015890*     MOVE 連長文－理由文(7)    TO 長期理由文７.
015780*     IF ( 連長文－理由文(1)  NOT = SPACE )
015790*        MOVE NC"（長期理由）"  TO 長期理由固定
015800*     END-IF.
015930********************
015940* 負傷データセット *
015950********************
015960* １部位 *
015970**********
015980     MOVE 負傷名Ｗ(1)       TO 負傷名１.
015990     MOVE 負傷年Ｗ(1)       TO 負傷年１.
016000     MOVE 負傷月Ｗ(1)       TO 負傷月１.
016010     MOVE 負傷日Ｗ(1)       TO 負傷日１.
016020     MOVE 初検年Ｗ(1)       TO 初検年１.
016030     MOVE 初検月Ｗ(1)       TO 初検月１.
016040     MOVE 初検日Ｗ(1)       TO 初検日１.
016050     MOVE 開始年Ｗ(1)       TO 開始年１.
016060     MOVE 開始月Ｗ(1)       TO 開始月１.
016070     MOVE 開始日Ｗ(1)       TO 開始日１.
016080     MOVE 終了年Ｗ(1)       TO 終了年１.
016090     MOVE 終了月Ｗ(1)       TO 終了月１.
016100     MOVE 終了日Ｗ(1)       TO 終了日１.
016110     MOVE 実日数Ｗ(1)       TO 実日数１.
016120     MOVE 治癒チェックＷ(1) TO 治癒チェック１.
016130     MOVE 中止チェックＷ(1) TO 中止チェック１.
016140     MOVE 転医チェックＷ(1) TO 転医チェック１.
016150**********
016160* ２部位 *
016170**********
016180     MOVE 負傷名Ｗ(2)       TO 負傷名２.
016190     MOVE 負傷年Ｗ(2)       TO 負傷年２.
016200     MOVE 負傷月Ｗ(2)       TO 負傷月２.
016210     MOVE 負傷日Ｗ(2)       TO 負傷日２.
016220     MOVE 初検年Ｗ(2)       TO 初検年２.
016230     MOVE 初検月Ｗ(2)       TO 初検月２.
016240     MOVE 初検日Ｗ(2)       TO 初検日２.
016250     MOVE 開始年Ｗ(2)       TO 開始年２.
016260     MOVE 開始月Ｗ(2)       TO 開始月２.
016270     MOVE 開始日Ｗ(2)       TO 開始日２.
016280     MOVE 終了年Ｗ(2)       TO 終了年２.
016290     MOVE 終了月Ｗ(2)       TO 終了月２.
016300     MOVE 終了日Ｗ(2)       TO 終了日２.
016310     MOVE 実日数Ｗ(2)       TO 実日数２.
016320     MOVE 治癒チェックＷ(2) TO 治癒チェック２.
016330     MOVE 中止チェックＷ(2) TO 中止チェック２.
016340     MOVE 転医チェックＷ(2) TO 転医チェック２.
016350**********
016360* ３部位 *
016370**********
016380     MOVE 負傷名Ｗ(3)       TO 負傷名３.
016390     MOVE 負傷年Ｗ(3)       TO 負傷年３.
016400     MOVE 負傷月Ｗ(3)       TO 負傷月３.
016410     MOVE 負傷日Ｗ(3)       TO 負傷日３.
016420     MOVE 初検年Ｗ(3)       TO 初検年３.
016430     MOVE 初検月Ｗ(3)       TO 初検月３.
016440     MOVE 初検日Ｗ(3)       TO 初検日３.
016450     MOVE 開始年Ｗ(3)       TO 開始年３.
016460     MOVE 開始月Ｗ(3)       TO 開始月３.
016470     MOVE 開始日Ｗ(3)       TO 開始日３.
016480     MOVE 終了年Ｗ(3)       TO 終了年３.
016490     MOVE 終了月Ｗ(3)       TO 終了月３.
016500     MOVE 終了日Ｗ(3)       TO 終了日３.
016510     MOVE 実日数Ｗ(3)       TO 実日数３.
016520     MOVE 治癒チェックＷ(3) TO 治癒チェック３.
016530     MOVE 中止チェックＷ(3) TO 中止チェック３.
016540     MOVE 転医チェックＷ(3) TO 転医チェック３.
016550**********
016560* ４部位 *
016570**********
016580     MOVE 負傷名Ｗ(4)       TO 負傷名４.
016590     MOVE 負傷年Ｗ(4)       TO 負傷年４.
016600     MOVE 負傷月Ｗ(4)       TO 負傷月４.
016610     MOVE 負傷日Ｗ(4)       TO 負傷日４.
016620     MOVE 初検年Ｗ(4)       TO 初検年４.
016630     MOVE 初検月Ｗ(4)       TO 初検月４.
016640     MOVE 初検日Ｗ(4)       TO 初検日４.
016650     MOVE 開始年Ｗ(4)       TO 開始年４.
016660     MOVE 開始月Ｗ(4)       TO 開始月４.
016670     MOVE 開始日Ｗ(4)       TO 開始日４.
016680     MOVE 終了年Ｗ(4)       TO 終了年４.
016690     MOVE 終了月Ｗ(4)       TO 終了月４.
016700     MOVE 終了日Ｗ(4)       TO 終了日４.
016710     MOVE 実日数Ｗ(4)       TO 実日数４.
016720     MOVE 治癒チェックＷ(4) TO 治癒チェック４.
016730     MOVE 中止チェックＷ(4) TO 中止チェック４.
016740     MOVE 転医チェックＷ(4) TO 転医チェック４.
016750**********
016760* ５部位 *
016770**********
016780     MOVE 負傷名Ｗ(5)       TO 負傷名５.
016790     MOVE 負傷年Ｗ(5)       TO 負傷年５.
016800     MOVE 負傷月Ｗ(5)       TO 負傷月５.
016810     MOVE 負傷日Ｗ(5)       TO 負傷日５.
016820     MOVE 初検年Ｗ(5)       TO 初検年５.
016830     MOVE 初検月Ｗ(5)       TO 初検月５.
016840     MOVE 初検日Ｗ(5)       TO 初検日５.
016850     MOVE 開始年Ｗ(5)       TO 開始年５.
016860     MOVE 開始月Ｗ(5)       TO 開始月５.
016870     MOVE 開始日Ｗ(5)       TO 開始日５.
016880     MOVE 終了年Ｗ(5)       TO 終了年５.
016890     MOVE 終了月Ｗ(5)       TO 終了月５.
016900     MOVE 終了日Ｗ(5)       TO 終了日５.
016910     MOVE 実日数Ｗ(5)       TO 実日数５.
016920     MOVE 治癒チェックＷ(5) TO 治癒チェック５.
016930     MOVE 中止チェックＷ(5) TO 中止チェック５.
016940     MOVE 転医チェックＷ(5) TO 転医チェック５.
016950**************
016960* 経過セット *
016970**************
016980     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
016870             UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR
016880                   ( 部位ＣＮＴ > 5 )
017010*         MOVE 部位ＣＮＴＷ(部位ＣＮＴ)   TO 経過部位ＣＮＴ(部位ＣＮＴ)
017020*         MOVE 部位区切Ｗ(部位ＣＮＴ)     TO 部位区切(部位ＣＮＴ)
017030         MOVE 印刷経過略称Ｗ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
017040     END-PERFORM.
017050*****************************************
017060*     新規・継続チェックについて        *
017070*   ●新規...初検有り ●継続...初検なし *
017080*****************************************
017090     MOVE 新規チェックＷ    TO 新規チェック.
017100     MOVE 継続チェックＷ    TO 継続チェック.
017110********************
017120* 料金データセット *
017130********************
017140*    ****************************************************************
017150*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
017160*    ****************************************************************
017170     MOVE 初検料ＷＲ                   TO  初検料.
017180     MOVE 時間外チェックＷ             TO  時間外チェック.
017190     MOVE 休日チェックＷ               TO  休日チェック.
017200     MOVE 深夜チェックＷ               TO  深夜チェック.
017210     MOVE 初検加算料ＷＲ               TO  初検加算料.
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
017220     MOVE 再検料ＷＲ                   TO  再検料.
017230     MOVE 往療距離ＷＲ                 TO  往療距離.
017240     MOVE 往療回数ＷＲ                 TO  往療回数.
017250     MOVE 往療料ＷＲ                   TO  往療料.
017260     MOVE 夜間チェックＷ               TO  夜間チェック.
017270     MOVE 暴風雨雪チェックＷ           TO  暴風雨雪チェック.
017280     MOVE 往療加算料ＷＲ               TO  往療加算料.
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
017320     MOVE 金属副子加算料ＷＲ           TO  金属副子加算料.
017330     MOVE 施術情報提供料ＷＲ           TO  施術情報提供料.
017340     MOVE 小計Ｗ                       TO 小計.
017350********************
017360* 初回処置料セット *
017370********************
017380     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
016870             UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR
016880                   ( 部位ＣＮＴ > 5 )
017410         MOVE 初回処置料ＷＲ(部位ＣＮＴ) TO 初回処置料(部位ＣＮＴ)
017420     END-PERFORM.
017430     MOVE 初回処置料合計Ｗ            TO 初回処置料合計
017440*
017450     MOVE 施療料チェックＷ            TO 施療料チェック.
017460     MOVE 整復料チェックＷ            TO 整復料チェック.
017470     MOVE 固定料チェックＷ            TO 固定料チェック.
017480********************
017490* 逓減毎料金セット *
017500********************
017510*    **********
017520*    * １部位 *
017530*    **********
017540     MOVE 後療単価１ＷＲ             TO 後療単価１.
017550     MOVE 後療回数１ＷＲ             TO 後療回数１.
017560     MOVE 後療料１ＷＲ               TO 後療料１.
017570     MOVE 冷罨法回数１ＷＲ           TO 冷罨法回数１.
017580     MOVE 冷罨法料１ＷＲ             TO 冷罨法料１.
017590     MOVE 温罨法回数１ＷＲ           TO 温罨法回数１.
017600     MOVE 温罨法料１ＷＲ             TO 温罨法料１.
017610     MOVE 電療回数１ＷＲ             TO 電療回数１.
017620     MOVE 電療料１ＷＲ               TO 電療料１.
017630     MOVE 小計１ＷＲ                 TO 小計１.
017640     IF ( 長期逓減率１ＷＲ NOT = ZERO )
017650         COMPUTE 長期逓減率１ = 長期逓減率１ＷＲ / 100
017660     END-IF.
017670     MOVE 長期込小計１ＷＲ           TO 長期込小計１.
017680*    **********
017690*    * ２部位 *
017700*    **********
017710     MOVE 後療単価２ＷＲ             TO 後療単価２.
017720     MOVE 後療回数２ＷＲ             TO 後療回数２.
017730     MOVE 後療料２ＷＲ               TO 後療料２.
017740     MOVE 冷罨法回数２ＷＲ           TO 冷罨法回数２.
017750     MOVE 冷罨法料２ＷＲ             TO 冷罨法料２.
017760     MOVE 温罨法回数２ＷＲ           TO 温罨法回数２.
017770     MOVE 温罨法料２ＷＲ             TO 温罨法料２.
017780     MOVE 電療回数２ＷＲ             TO 電療回数２.
017790     MOVE 電療料２ＷＲ               TO 電療料２.
017800     MOVE 小計２ＷＲ                 TO 小計２.
017810     IF ( 長期逓減率２ＷＲ NOT = ZERO )
017820         COMPUTE 長期逓減率２ = 長期逓減率２ＷＲ / 100
017830     END-IF.
017840     MOVE 長期込小計２ＷＲ           TO 長期込小計２.
017850*    ****************
017860*    * ３部位／８割 *
017870*    ****************
017880     MOVE 後療単価３８ＷＲ             TO 後療単価３８.
017890     MOVE 後療回数３８ＷＲ             TO 後療回数３８.
017900     MOVE 後療料３８ＷＲ               TO 後療料３８.
017910     MOVE 冷罨法回数３８ＷＲ           TO 冷罨法回数３８.
017920     MOVE 冷罨法料３８ＷＲ             TO 冷罨法料３８.
017930     MOVE 温罨法回数３８ＷＲ           TO 温罨法回数３８.
017940     MOVE 温罨法料３８ＷＲ             TO 温罨法料３８.
017950     MOVE 電療回数３８ＷＲ             TO 電療回数３８.
017960     MOVE 電療料３８ＷＲ               TO 電療料３８.
017970     MOVE 小計３８ＷＲ                 TO 小計３８.
017980     MOVE 多部位込小計３８ＷＲ         TO 多部位込小計３８.
017990     IF ( 長期逓減率３８ＷＲ NOT = ZERO )
018000         COMPUTE 長期逓減率３８ = 長期逓減率３８ＷＲ / 100
018010     END-IF.
018020     MOVE 長期込小計３８ＷＲ           TO 長期込小計３８.
      */ 逓減率 0.7→0.6 /42505
           IF (施術和暦年月ＷＲ >= 42505)
              MOVE "60"                      TO 逓減３８
              MOVE "0.6"                     TO 多部位３８
           END-IF.
018030*    ****************
018040*    * ３部位／10割 *
018050*    ****************
018060     MOVE 逓減開始月３０ＷＲ           TO 逓減開始月３０.
018070     MOVE 逓減開始日３０ＷＲ           TO 逓減開始日３０.
018080     MOVE 後療単価３０ＷＲ             TO 後療単価３０.
018090     MOVE 後療回数３０ＷＲ             TO 後療回数３０.
018100     MOVE 後療料３０ＷＲ               TO 後療料３０.
018110     MOVE 冷罨法回数３０ＷＲ           TO 冷罨法回数３０.
018120     MOVE 冷罨法料３０ＷＲ             TO 冷罨法料３０.
018130     MOVE 温罨法回数３０ＷＲ           TO 温罨法回数３０.
018140     MOVE 温罨法料３０ＷＲ             TO 温罨法料３０.
018150     MOVE 電療回数３０ＷＲ             TO 電療回数３０.
018160     MOVE 電療料３０ＷＲ               TO 電療料３０.
018170     MOVE 小計３０ＷＲ                 TO 小計３０.
018180     IF ( 長期逓減率３０ＷＲ NOT = ZERO )
018190         COMPUTE 長期逓減率３０ = 長期逓減率３０ＷＲ / 100
018200     END-IF.
018210     MOVE 長期込小計３０ＷＲ           TO 長期込小計３０.
018220*    ****************
018230*    * ４部位／５割 *
018240*    ****************
018250*     MOVE 後療単価４５ＷＲ             TO 後療単価４５.
018260*     MOVE 後療回数４５ＷＲ             TO 後療回数４５.
018270*     MOVE 後療料４５ＷＲ               TO 後療料４５.
018280*     MOVE 冷罨法回数４５ＷＲ           TO 冷罨法回数４５.
018290*     MOVE 冷罨法料４５ＷＲ             TO 冷罨法料４５.
018300*     MOVE 温罨法回数４５ＷＲ           TO 温罨法回数４５.
018310*     MOVE 温罨法料４５ＷＲ             TO 温罨法料４５.
018320*     MOVE 電療回数４５ＷＲ             TO 電療回数４５.
018330*     MOVE 電療料４５ＷＲ               TO 電療料４５.
018340*     MOVE 小計４５ＷＲ                 TO 小計４５.
018350*     MOVE 多部位込小計４５ＷＲ         TO 多部位込小計４５.
018360*     IF ( 長期逓減率４５ＷＲ NOT = ZERO )
018370*         COMPUTE 長期逓減率４５ = 長期逓減率４５ＷＲ / 100
018380*     END-IF.
018390*     MOVE 長期込小計４５ＷＲ           TO 長期込小計４５.
018400*    ****************
018410*    * ４部位／８割 *
018420*    ****************
018430     MOVE 逓減開始月４８ＷＲ           TO 逓減開始月４８.
018440     MOVE 逓減開始日４８ＷＲ           TO 逓減開始日４８.
018450     MOVE 後療単価４８ＷＲ             TO 後療単価４８.
018460     MOVE 後療回数４８ＷＲ             TO 後療回数４８.
018470     MOVE 後療料４８ＷＲ               TO 後療料４８.
018480     MOVE 冷罨法回数４８ＷＲ           TO 冷罨法回数４８.
018490     MOVE 冷罨法料４８ＷＲ             TO 冷罨法料４８.
018500     MOVE 温罨法回数４８ＷＲ           TO 温罨法回数４８.
018510     MOVE 温罨法料４８ＷＲ             TO 温罨法料４８.
018520     MOVE 電療回数４８ＷＲ             TO 電療回数４８.
018530     MOVE 電療料４８ＷＲ               TO 電療料４８.
018540     MOVE 小計４８ＷＲ                 TO 小計４８.
018550     MOVE 多部位込小計４８ＷＲ         TO 多部位込小計４８.
018560     IF ( 長期逓減率４８ＷＲ NOT = ZERO )
018570         COMPUTE 長期逓減率４８ = 長期逓減率４８ＷＲ / 100
018580     END-IF.
018590     MOVE 長期込小計４８ＷＲ           TO 長期込小計４８.
      */ 逓減率 0.7→0.6 /42505
           IF (施術和暦年月ＷＲ >= 42505)
              MOVE "60"                      TO 逓減４８
              MOVE "0.6"                     TO 多部位４８
           END-IF.
018600*    ****************
018610*    * ４部位／10割 *
018620*    ****************
018630     MOVE 逓減開始月４０ＷＲ           TO 逓減開始月４０.
018640     MOVE 逓減開始日４０ＷＲ           TO 逓減開始日４０.
018650     MOVE 後療単価４０ＷＲ             TO 後療単価４０.
018660     MOVE 後療回数４０ＷＲ             TO 後療回数４０.
018670     MOVE 後療料４０ＷＲ               TO 後療料４０.
018680     MOVE 冷罨法回数４０ＷＲ           TO 冷罨法回数４０.
018690     MOVE 冷罨法料４０ＷＲ             TO 冷罨法料４０.
018700     MOVE 温罨法回数４０ＷＲ           TO 温罨法回数４０.
018710     MOVE 温罨法料４０ＷＲ             TO 温罨法料４０.
018720     MOVE 電療回数４０ＷＲ             TO 電療回数４０.
018730     MOVE 電療料４０ＷＲ               TO 電療料４０.
018740     MOVE 小計４０ＷＲ                 TO 小計４０.
018750     IF ( 長期逓減率４０ＷＲ NOT = ZERO )
018760         COMPUTE 長期逓減率４０ = 長期逓減率４０ＷＲ / 100
018770     END-IF.
018780     MOVE 長期込小計４０ＷＲ           TO 長期込小計４０.
018790*
018800*↓***********************************************************************
018810* ５部位／2.5割の印字は必要ない。
018820*------------------------------------------------------------------------*
018830*    *****************
018840*    * ５部位／2.5割 *
018850*    *****************
018860*     MOVE 後療単価５２ＷＲ             TO 後療単価５２.
018870*     MOVE 後療回数５２ＷＲ             TO 後療回数５２.
018880*     MOVE 後療料５２ＷＲ               TO 後療料５２.
018890*     MOVE 冷罨法回数５２ＷＲ           TO 冷罨法回数５２.
018900*     MOVE 冷罨法料５２ＷＲ             TO 冷罨法料５２.
018910*     MOVE 温罨法回数５２ＷＲ           TO 温罨法回数５２.
018920*     MOVE 温罨法料５２ＷＲ             TO 温罨法料５２.
018930*     MOVE 電療回数５２ＷＲ             TO 電療回数５２.
018940*     MOVE 電療料５２ＷＲ               TO 電療料５２.
018950*     MOVE 小計５２ＷＲ                 TO 小計５２.
018960*     MOVE 多部位込小計５２ＷＲ         TO 多部位込小計５２.
018970*     IF ( 長期逓減率５２ＷＲ NOT = ZERO )
018980*         COMPUTE 長期逓減率５２ = 長期逓減率５２ＷＲ / 100
018990*     END-IF.
019000*     MOVE 長期込小計５２ＷＲ           TO 長期込小計５２.
019010*↑***********************************************************************
019020*
019030*    ****************
019040*    * ５部位／５割 *
019050*    ****************
019060*     MOVE 逓減開始月５５ＷＲ           TO 逓減開始月５５.
019070*     MOVE 逓減開始日５５ＷＲ           TO 逓減開始日５５.
019080*     MOVE 後療単価５５ＷＲ             TO 後療単価５５.
019090*     MOVE 後療回数５５ＷＲ             TO 後療回数５５.
019100*     MOVE 後療料５５ＷＲ               TO 後療料５５.
019110*     MOVE 冷罨法回数５５ＷＲ           TO 冷罨法回数５５.
019120*     MOVE 冷罨法料５５ＷＲ             TO 冷罨法料５５.
019130*     MOVE 温罨法回数５５ＷＲ           TO 温罨法回数５５.
019140*     MOVE 温罨法料５５ＷＲ             TO 温罨法料５５.
019150*     MOVE 電療回数５５ＷＲ             TO 電療回数５５.
019160*     MOVE 電療料５５ＷＲ               TO 電療料５５.
019170*     MOVE 小計５５ＷＲ                 TO 小計５５.
019180*     MOVE 多部位込小計５５ＷＲ         TO 多部位込小計５５.
019190*     IF ( 長期逓減率５５ＷＲ NOT = ZERO )
019200*         COMPUTE 長期逓減率５５ = 長期逓減率５５ＷＲ / 100
019210*     END-IF.
019220*     MOVE 長期込小計５５ＷＲ           TO 長期込小計５５.
019230*    ****************
019240*    * ５部位／８割 *
019250*    ****************
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
019430*    ****************
019440*    * ５部位／10割 *
019450*    ****************
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
019630*
019640     MOVE 適用１Ｗ                       TO 適用１.
019650     MOVE 適用２Ｗ                       TO 適用２.
019660*     MOVE 適用３Ｗ                       TO 適用３.
019670*     MOVE 適用４Ｗ                       TO 適用４.
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
020450     MOVE レセ－一部負担金               TO 一部負担金.
020460     MOVE レセ－請求金額                 TO 請求金額.
      *
      */長野県の社保助成で助成種別が"60"で県固有から印刷された場合、受給者負担額と（写）を出す/2106
           IF (保険種別ＷＲ NOT = 01 AND 08) AND (受－費用負担者番号助成(3:2) =  "20") AND
              (連レ－長野助成 = 60)
021370         MOVE レセ－印刷レセ受給者負担額       TO 助成請求額
               MOVE "受給者負担金"                   TO 請求金額ＣＭ
               MOVE "円" TO 請求金額円ＣＭ
022720         MOVE NC"写"                           TO 写記号
022730         MOVE NC"○"                           TO 写マル
               IF ( 受－受益者番号助成(1:1) = "*"  ) OR
                  ( 受－受益者番号助成(1:2) = "＊" )
                   MOVE  SPACE           TO 受給者番号
               ELSE
                   MOVE 受－受益者番号助成     TO 受給者番号
               END-IF
               MOVE SPACE                TO 受給者番号１
               MOVE SPACE                TO 受給者番号３
           END-IF.
019710*
019712*------------------------------------------------------------------------*
019737**********************
019738* 施術所データセット *
019740**********************
           MOVE 都道府県ＪＩＳＷ       TO 都道府県番号.
019750     MOVE 柔整師番号Ｗ           TO 柔整師番号.
019760*     MOVE 定額制受理番号Ｗ       TO 定額制受理番号.
019770     MOVE 施術所郵便番号１Ｗ     TO 施術所郵便番号１.
019780     MOVE 施術所郵便番号２Ｗ     TO 施術所郵便番号２.
019800     MOVE 施術所住所１Ｗ         TO 施術所住所１.
019810     MOVE 施術所住所２Ｗ         TO 施術所住所２.
019820     MOVE 接骨師会会員番号Ｗ     TO 接骨師会会員番号.
019830     MOVE 代表者カナＷ           TO 代表者カナ.
019840     MOVE 代表者名Ｗ             TO 代表者名.
019850     MOVE 接骨師会会員番号Ｗ     TO 接骨師会会員番号.
019860     MOVE 施術所電話番号Ｗ       TO 施術所電話番号.
019870*
019880     MOVE 接骨院名Ｗ             TO 接骨院名.
019890*
019900* / 柔整師・患者委任日 /
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
019910     MOVE 柔整師年Ｗ             TO 受理年.
019920     MOVE 柔整師月Ｗ             TO 受理月.
019930     MOVE 柔整師日Ｗ             TO 受理日.
019940* ( 委任年月日 印刷するか )
019950     IF ( 連入－委任印刷  = ZERO )
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
019960         MOVE 患者委任年Ｗ       TO 委任年
019970         MOVE 患者委任月Ｗ       TO 委任月
019980         MOVE 患者委任日Ｗ       TO 委任日
019990     END-IF.
020000*
020010* 共済番号(施術機関コード欄に記載する)
020020     MOVE  共済番号Ｗ            TO 共済番号.
020030*
020040*     MOVE 銀行名支店名Ｗ         TO 銀行名支店名.
020050*     MOVE 預金種別コメントＷ     TO 預金種別.
020060*     MOVE 口座番号Ｗ             TO 口座番号.
020070*     MOVE 口座名義人カナＷ       TO 口座名義人カナ.
020080*     MOVE 口座名義人とカナＷ     TO 口座名義人.
020090*     MOVE コメント１Ｗ           TO コメント１.
020100*     MOVE コメント２Ｗ           TO コメント２.
020110*     MOVE コメント３Ｗ           TO コメント３.
020120*
020130*
020140************************
020150* レセプト並び順セット *
020160************************
020170     MOVE 順番固定Ｗ          TO 順番固定.
020180     MOVE 順番Ｗ              TO 順番.
020190     MOVE 患者番号ＷＲ        TO 患者番号.
020200     MOVE 枝番ＷＲ            TO 枝番.
020210*
020220*
020230* 特別コメント
020240*     MOVE 特別コメントＷ      TO 特別コメント.
020250*
020260**********     PERFORM テスト印字処理.
020290*-------------------------------------------------------------------------*
020300*--- ※ レセ摘要再セットは、この印刷セットSECTION の最後にやること！ -----*
020310     PERFORM レセ摘要再セット.
020320*-------------------------------------------------------------------------*
020321*
020322*-------------------------------------------------------------------------*
020323*--- ※ 地域特有処理は、この印刷セットSECTION の最後にやること！   　-----*
020324     PERFORM 地域特有処理.
020325*-------------------------------------------------------------------------*
020326*
020330*
020340*================================================================*
020350 項目初期化 SECTION.
020360*
020370     INITIALIZE 施術所情報Ｗ.
020380     INITIALIZE 受診者情報Ｗ.
020390     INITIALIZE 負傷情報Ｗ.
020400     INITIALIZE 備考情報Ｗ.
020410*     INITIALIZE ＯＣＲコードＷ.
020420     INITIALIZE 料金１ＷＲ.
020430     INITIALIZE 料金２ＷＲ.
020440     INITIALIZE 料金３ＷＲ.
020460     INITIALIZE YCH6125P.
020450     MOVE SPACE TO YCH6125P.
020470*================================================================*
020480 料金情報取得 SECTION.
020490*
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
020500********************
020510* 料金データセット *
020520********************
020530*    ****************************************************************
020540*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
020550*    ****************************************************************
020560     MOVE レセ－初検料                 TO 初検料ＷＲ.
020570     IF ( レセ－時間外 = 1 )
020580         MOVE NC"○"                   TO 時間外チェックＷ
020590     END-IF.
020600     IF ( レセ－休日 = 1 )
020610         MOVE NC"○"                   TO 休日チェックＷ
020620     END-IF.
020630     IF ( レセ－深夜 = 1 )
020640         MOVE NC"○"                   TO 深夜チェックＷ
020650     END-IF.
020660*
020670     MOVE レセ－初検加算料             TO  初検加算料ＷＲ.
           MOVE レセ－初検時相談料           TO  初検時相談料ＷＲ.
020680     MOVE レセ－再検料                 TO  再検料ＷＲ.
020690     MOVE レセ－往療距離               TO  往療距離ＷＲ.
020700     MOVE レセ－往療回数               TO  往療回数ＷＲ.
020710     MOVE レセ－往療料                 TO  往療料ＷＲ.
020720     MOVE レセ－往療加算料             TO  往療加算料ＷＲ.
020730*
020740     IF ( レセ－夜間 = 1 )
020750         MOVE NC"○"                   TO 夜間チェックＷ
020760     END-IF.
020770     IF ( レセ－暴風雨雪 = 1 )
020780         MOVE NC"○"                   TO 暴風雨雪チェックＷ
020790     END-IF.
020800*
020810     MOVE レセ－金属副子加算料         TO  金属副子加算料ＷＲ.
020820*
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
020920*
020930     MOVE レセ－施術情報提供料         TO  施術情報提供料ＷＲ.
020940* 小計
020950     MOVE レセ－小計                   TO 小計Ｗ.
020960********************
020970* 初回処置料セット *
020980********************
020990     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
021000             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
021010         MOVE レセ－初回処置料(部位ＣＮＴ) TO 初回処置料ＷＲ(部位ＣＮＴ)
021020         IF ( レセ－初回処置料(部位ＣＮＴ) NOT = ZERO )
021030            EVALUATE 負－負傷種別(部位ＣＮＴ)
021040* 捻挫・打撲・挫傷
021050            WHEN 1
021060            WHEN 2
021070            WHEN 3
021080                MOVE NC"○"       TO 施療料チェックＷ
021090* 脱臼・骨折・骨折拘縮
021100            WHEN 4
021110            WHEN 5
021120            WHEN 7
021130                MOVE NC"○"       TO 整復料チェックＷ
021140* 不全骨折・不全骨折拘縮
021150            WHEN 6
021160            WHEN 8
021170                MOVE NC"○"       TO 固定料チェックＷ
021180            END-EVALUATE
021190         END-IF
021200     END-PERFORM.
021210     MOVE レセ－初回処置料合計         TO 初回処置料合計Ｗ.
021220********************
021230* 逓減毎料金セット *
021240********************
021250*    **********
021260*    * １部位 *
021270*    **********
021280     MOVE レセ－後療単価１             TO 後療単価１ＷＲ.
021290     MOVE レセ－後療回数１             TO 後療回数１ＷＲ.
021300     MOVE レセ－後療料１               TO 後療料１ＷＲ.
021310     MOVE レセ－冷罨法回数１           TO 冷罨法回数１ＷＲ.
021320     MOVE レセ－冷罨法料１             TO 冷罨法料１ＷＲ.
021330     MOVE レセ－温罨法回数１           TO 温罨法回数１ＷＲ.
021340     MOVE レセ－温罨法料１             TO 温罨法料１ＷＲ.
021350     MOVE レセ－電療回数１             TO 電療回数１ＷＲ.
021360     MOVE レセ－電療料１               TO 電療料１ＷＲ.
021370     MOVE レセ－小計１                 TO 小計１ＷＲ.
021380     MOVE レセ－長期逓減率１           TO 長期逓減率１ＷＲ.
021390     MOVE レセ－長期込小計１           TO 長期込小計１ＷＲ.
021400*    **********
021410*    * ２部位 *
021420*    **********
021430     MOVE レセ－後療単価２             TO 後療単価２ＷＲ.
021440     MOVE レセ－後療回数２             TO 後療回数２ＷＲ.
021450     MOVE レセ－後療料２               TO 後療料２ＷＲ.
021460     MOVE レセ－冷罨法回数２           TO 冷罨法回数２ＷＲ.
021470     MOVE レセ－冷罨法料２             TO 冷罨法料２ＷＲ.
021480     MOVE レセ－温罨法回数２           TO 温罨法回数２ＷＲ.
021490     MOVE レセ－温罨法料２             TO 温罨法料２ＷＲ.
021500     MOVE レセ－電療回数２             TO 電療回数２ＷＲ.
021510     MOVE レセ－電療料２               TO 電療料２ＷＲ.
021520     MOVE レセ－小計２                 TO 小計２ＷＲ.
021530     MOVE レセ－長期逓減率２           TO 長期逓減率２ＷＲ.
021540     MOVE レセ－長期込小計２           TO 長期込小計２ＷＲ.
021550*    ****************
021560*    * ３部位／８割 *
021570*    ****************
021580     MOVE レセ－後療単価３８             TO 後療単価３８ＷＲ.
021590     MOVE レセ－後療回数３８             TO 後療回数３８ＷＲ.
021600     MOVE レセ－後療料３８               TO 後療料３８ＷＲ.
021610     MOVE レセ－冷罨法回数３８           TO 冷罨法回数３８ＷＲ.
021620     MOVE レセ－冷罨法料３８             TO 冷罨法料３８ＷＲ.
021630     MOVE レセ－温罨法回数３８           TO 温罨法回数３８ＷＲ.
021640     MOVE レセ－温罨法料３８             TO 温罨法料３８ＷＲ.
021650     MOVE レセ－電療回数３８             TO 電療回数３８ＷＲ.
021660     MOVE レセ－電療料３８               TO 電療料３８ＷＲ.
021670     MOVE レセ－小計３８                 TO 小計３８ＷＲ.
021680     MOVE レセ－多部位込小計３８         TO 多部位込小計３８ＷＲ.
021690     MOVE レセ－長期逓減率３８           TO 長期逓減率３８ＷＲ.
021700     MOVE レセ－長期込小計３８           TO 長期込小計３８ＷＲ.
021710*    ****************
021720*    * ３部位／10割 *
021730*    ****************
021740     MOVE レセ－逓減開始月３０           TO 逓減開始月３０ＷＲ.
021750     MOVE レセ－逓減開始日３０           TO 逓減開始日３０ＷＲ.
021760     MOVE レセ－後療単価３０             TO 後療単価３０ＷＲ.
021770     MOVE レセ－後療回数３０             TO 後療回数３０ＷＲ.
021780     MOVE レセ－後療料３０               TO 後療料３０ＷＲ.
021790     MOVE レセ－冷罨法回数３０           TO 冷罨法回数３０ＷＲ.
021800     MOVE レセ－冷罨法料３０             TO 冷罨法料３０ＷＲ.
021810     MOVE レセ－温罨法回数３０           TO 温罨法回数３０ＷＲ.
021820     MOVE レセ－温罨法料３０             TO 温罨法料３０ＷＲ.
021830     MOVE レセ－電療回数３０             TO 電療回数３０ＷＲ.
021840     MOVE レセ－電療料３０               TO 電療料３０ＷＲ.
021850     MOVE レセ－小計３０                 TO 小計３０ＷＲ.
021860     MOVE レセ－長期逓減率３０           TO 長期逓減率３０ＷＲ.
021870     MOVE レセ－長期込小計３０           TO 長期込小計３０ＷＲ.
021880*    ****************
021890*    * ４部位／５割 *
021900*    ****************
021910     MOVE レセ－後療単価４５             TO 後療単価４５ＷＲ.
021920     MOVE レセ－後療回数４５             TO 後療回数４５ＷＲ.
021930     MOVE レセ－後療料４５               TO 後療料４５ＷＲ.
021940     MOVE レセ－冷罨法回数４５           TO 冷罨法回数４５ＷＲ.
021950     MOVE レセ－冷罨法料４５             TO 冷罨法料４５ＷＲ.
021960     MOVE レセ－温罨法回数４５           TO 温罨法回数４５ＷＲ.
021970     MOVE レセ－温罨法料４５             TO 温罨法料４５ＷＲ.
021980     MOVE レセ－電療回数４５             TO 電療回数４５ＷＲ.
021990     MOVE レセ－電療料４５               TO 電療料４５ＷＲ.
022000     MOVE レセ－小計４５                 TO 小計４５ＷＲ.
022010     MOVE レセ－多部位込小計４５         TO 多部位込小計４５ＷＲ.
022020     MOVE レセ－長期逓減率４５           TO 長期逓減率４５ＷＲ.
022030     MOVE レセ－長期込小計４５           TO 長期込小計４５ＷＲ.
022040*    ****************
022050*    * ４部位／８割 *
022060*    ****************
022070     MOVE レセ－逓減開始月４８           TO 逓減開始月４８ＷＲ.
022080     MOVE レセ－逓減開始日４８           TO 逓減開始日４８ＷＲ.
022090     MOVE レセ－後療単価４８             TO 後療単価４８ＷＲ.
022100     MOVE レセ－後療回数４８             TO 後療回数４８ＷＲ.
022110     MOVE レセ－後療料４８               TO 後療料４８ＷＲ.
022120     MOVE レセ－冷罨法回数４８           TO 冷罨法回数４８ＷＲ.
022130     MOVE レセ－冷罨法料４８             TO 冷罨法料４８ＷＲ.
022140     MOVE レセ－温罨法回数４８           TO 温罨法回数４８ＷＲ.
022150     MOVE レセ－温罨法料４８             TO 温罨法料４８ＷＲ.
022160     MOVE レセ－電療回数４８             TO 電療回数４８ＷＲ.
022170     MOVE レセ－電療料４８               TO 電療料４８ＷＲ.
022180     MOVE レセ－小計４８                 TO 小計４８ＷＲ.
022190     MOVE レセ－多部位込小計４８         TO 多部位込小計４８ＷＲ.
022200     MOVE レセ－長期逓減率４８           TO 長期逓減率４８ＷＲ.
022210     MOVE レセ－長期込小計４８           TO 長期込小計４８ＷＲ.
022220*    ****************
022230*    * ４部位／10割 *
022240*    ****************
022250     MOVE レセ－逓減開始月４０           TO 逓減開始月４０ＷＲ.
022260     MOVE レセ－逓減開始日４０           TO 逓減開始日４０ＷＲ.
022270     MOVE レセ－後療単価４０             TO 後療単価４０ＷＲ.
022280     MOVE レセ－後療回数４０             TO 後療回数４０ＷＲ.
022290     MOVE レセ－後療料４０               TO 後療料４０ＷＲ.
022300     MOVE レセ－冷罨法回数４０           TO 冷罨法回数４０ＷＲ.
022310     MOVE レセ－冷罨法料４０             TO 冷罨法料４０ＷＲ.
022320     MOVE レセ－温罨法回数４０           TO 温罨法回数４０ＷＲ.
022330     MOVE レセ－温罨法料４０             TO 温罨法料４０ＷＲ.
022340     MOVE レセ－電療回数４０             TO 電療回数４０ＷＲ.
022350     MOVE レセ－電療料４０               TO 電療料４０ＷＲ.
022360     MOVE レセ－小計４０                 TO 小計４０ＷＲ.
022370     MOVE レセ－長期逓減率４０           TO 長期逓減率４０ＷＲ.
022380     MOVE レセ－長期込小計４０           TO 長期込小計４０ＷＲ.
022390*    *****************
022400*    * ５部位／2.5割 *
022410*    *****************
022420     MOVE レセ－後療単価５２             TO 後療単価５２ＷＲ.
022430     MOVE レセ－後療回数５２             TO 後療回数５２ＷＲ.
022440     MOVE レセ－後療料５２               TO 後療料５２ＷＲ.
022450     MOVE レセ－冷罨法回数５２           TO 冷罨法回数５２ＷＲ.
022460     MOVE レセ－冷罨法料５２             TO 冷罨法料５２ＷＲ.
022470     MOVE レセ－温罨法回数５２           TO 温罨法回数５２ＷＲ.
022480     MOVE レセ－温罨法料５２             TO 温罨法料５２ＷＲ.
022490     MOVE レセ－電療回数５２             TO 電療回数５２ＷＲ.
022500     MOVE レセ－電療料５２               TO 電療料５２ＷＲ.
022510     MOVE レセ－小計５２                 TO 小計５２ＷＲ.
022520     MOVE レセ－多部位込小計５２         TO 多部位込小計５２ＷＲ.
022530     MOVE レセ－長期逓減率５２           TO 長期逓減率５２ＷＲ.
022540     MOVE レセ－長期込小計５２           TO 長期込小計５２ＷＲ.
022550*    ****************
022560*    * ５部位／５割 *
022570*    ****************
022580     MOVE レセ－逓減開始月５５           TO 逓減開始月５５ＷＲ.
022590     MOVE レセ－逓減開始日５５           TO 逓減開始日５５ＷＲ.
022600     MOVE レセ－後療単価５５             TO 後療単価５５ＷＲ.
022610     MOVE レセ－後療回数５５             TO 後療回数５５ＷＲ.
022620     MOVE レセ－後療料５５               TO 後療料５５ＷＲ.
022630     MOVE レセ－冷罨法回数５５           TO 冷罨法回数５５ＷＲ.
022640     MOVE レセ－冷罨法料５５             TO 冷罨法料５５ＷＲ.
022650     MOVE レセ－温罨法回数５５           TO 温罨法回数５５ＷＲ.
022660     MOVE レセ－温罨法料５５             TO 温罨法料５５ＷＲ.
022670     MOVE レセ－電療回数５５             TO 電療回数５５ＷＲ.
022680     MOVE レセ－電療料５５               TO 電療料５５ＷＲ.
022690     MOVE レセ－小計５５                 TO 小計５５ＷＲ.
022700     MOVE レセ－多部位込小計５５         TO 多部位込小計５５ＷＲ.
022710     MOVE レセ－長期逓減率５５           TO 長期逓減率５５ＷＲ.
022720     MOVE レセ－長期込小計５５           TO 長期込小計５５ＷＲ.
022730*    ****************
022740*    * ５部位／８割 *
022750*    ****************
022760     MOVE レセ－逓減開始月５８           TO 逓減開始月５８ＷＲ.
022770     MOVE レセ－逓減開始日５８           TO 逓減開始日５８ＷＲ.
022780     MOVE レセ－後療単価５８             TO 後療単価５８ＷＲ.
022790     MOVE レセ－後療回数５８             TO 後療回数５８ＷＲ.
022800     MOVE レセ－後療料５８               TO 後療料５８ＷＲ.
022810     MOVE レセ－冷罨法回数５８           TO 冷罨法回数５８ＷＲ.
022820     MOVE レセ－冷罨法料５８             TO 冷罨法料５８ＷＲ.
022830     MOVE レセ－温罨法回数５８           TO 温罨法回数５８ＷＲ.
022840     MOVE レセ－温罨法料５８             TO 温罨法料５８ＷＲ.
022850     MOVE レセ－電療回数５８             TO 電療回数５８ＷＲ.
022860     MOVE レセ－電療料５８               TO 電療料５８ＷＲ.
022870     MOVE レセ－小計５８                 TO 小計５８ＷＲ.
022880     MOVE レセ－多部位込小計５８         TO 多部位込小計５８ＷＲ.
022890     MOVE レセ－長期逓減率５８           TO 長期逓減率５８ＷＲ.
022900     MOVE レセ－長期込小計５８           TO 長期込小計５８ＷＲ.
022910*    ****************
022920*    * ５部位／10割 *
022930*    ****************
022940     MOVE レセ－逓減開始月５０           TO 逓減開始月５０ＷＲ.
022950     MOVE レセ－逓減開始日５０           TO 逓減開始日５０ＷＲ.
022960     MOVE レセ－後療単価５０             TO 後療単価５０ＷＲ.
022970     MOVE レセ－後療回数５０             TO 後療回数５０ＷＲ.
022980     MOVE レセ－後療料５０               TO 後療料５０ＷＲ.
022990     MOVE レセ－冷罨法回数５０           TO 冷罨法回数５０ＷＲ.
023000     MOVE レセ－冷罨法料５０             TO 冷罨法料５０ＷＲ.
023010     MOVE レセ－温罨法回数５０           TO 温罨法回数５０ＷＲ.
023020     MOVE レセ－温罨法料５０             TO 温罨法料５０ＷＲ.
023030     MOVE レセ－電療回数５０             TO 電療回数５０ＷＲ.
023040     MOVE レセ－電療料５０               TO 電療料５０ＷＲ.
023050     MOVE レセ－小計５０                 TO 小計５０ＷＲ.
023060     MOVE レセ－長期逓減率５０           TO 長期逓減率５０ＷＲ.
023070     MOVE レセ－長期込小計５０           TO 長期込小計５０ＷＲ.
023080*
023090*================================================================*
023100 施術所情報取得 SECTION.
023110*
023120**************************************************
023130* 本院データを使用し、以下の情報を取得           *
023140* ● 柔整師番号.. 柔整師番号Ｗに格納             *
023150* ● 会員番号 ... 接骨師会会員番号Ｗに格納       *
023160* ● 代表者名 ... 代表者名Ｗに格納               *
023170* ● 住所1,2   ...施術所住所1,2Ｗに格納          *
023180* ● 電話番号 ... 施術所電話番号Ｗに格納         *
023190**************************************************
023200     MOVE ZERO  TO 施情－施術所番号.
023210     READ 施術所情報マスタ
023220     INVALID KEY
023230         CONTINUE
023240     NOT INVALID KEY
023250*
               MOVE 施情－都道府県ＪＩＳ     TO 都道府県ＪＩＳＷ
023290         MOVE 施情－新柔整師番号       TO 柔整師番号Ｗ
023310*
023320*** 共済・自衛官の時のみ、共済番号の編集をする。
023330         EVALUATE 保険種別ＷＲ
023340         WHEN 04
023350             PERFORM 共済番号セット
023360         WHEN 09
023370             PERFORM 自衛官番号セット
023380         END-EVALUATE
023390*******
023400*         MOVE 施情－接骨師会会員番号  TO ＯＣＲ接骨師会会員番号Ｗ
023410*
023420         STRING "中央-"                DELIMITED BY SIZE
023430                施情－接骨師会会員番号 DELIMITED BY SPACE
023440           INTO 接骨師会会員番号Ｗ
023450         END-STRING
023460*
023470         MOVE 施情－郵便番号１        TO 施術所郵便番号１Ｗ
023480         MOVE 施情－郵便番号２        TO 施術所郵便番号２Ｗ
023490         MOVE 施情－代表者カナ        TO 代表者カナＷ
023500         MOVE 施情－代表者名          TO 代表者名Ｗ
023510*
023520         MOVE 施情－接骨院名          TO 接骨院名Ｗ
023530*
023540*         STRING 施情－住所１  DELIMITED BY SPACE
023550*                施情－住所２  DELIMITED BY SPACE
023560*           INTO 施術所住所Ｗ
023570*         END-STRING
023580         MOVE 施情－住所１            TO 施術所住所１Ｗ
023590         MOVE 施情－住所２            TO 施術所住所２Ｗ
023600*
023610         MOVE 施情－電話番号          TO 施術所電話番号Ｗ
023620*
023630     END-READ.
023640*================================================================*
023650 共済番号セット SECTION.
023660*
023670**************************************************************
023680* 保険者番号により、共済の番号を印字するか判定
023690* 中央特有 追加 99/10
023700**************************************************************
023710** 1.共済組合連盟
023720     MOVE SPACE  TO  脱出フラグ.
023730     IF ( 施情－共済連番号 NOT = ZERO )
023740** 条件(保険者番号)
023750        IF ( 保険者番号ＷＲ(1:2) = "31" )  OR
023760           ( 保険者番号ＷＲ = "34130021" )
023770*
023780           MOVE  NC"共済組合連盟第"   TO 共済連番号名ＮＷ 
023790           MOVE  NC"号"               TO 共済連番号単位ＮＷ 
023800           MOVE  施情－共済連番号     TO 共済連番号Ｗ
023810           IF ( 共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
023820                 MOVE SPACE TO  共済連番号Ｗ(1:1)
023830           ELSE
023840                 MOVE "YES" TO  脱出フラグ
023850           END-IF
023860           IF ( 共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
023870                 MOVE SPACE TO  共済連番号Ｗ(2:1)
023880           ELSE
023890                 MOVE "YES" TO  脱出フラグ
023900           END-IF
023910           IF ( 共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
023920                 MOVE SPACE TO  共済連番号Ｗ(3:1)
023930           ELSE
023940                 MOVE "YES" TO  脱出フラグ
023950           END-IF
023960           IF ( 共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
023970                 MOVE SPACE TO  共済連番号Ｗ(4:1)
023980           ELSE
023990                 MOVE "YES" TO  脱出フラグ
024000           END-IF
024010           IF ( 共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
024020                 MOVE SPACE TO  共済連番号Ｗ(5:1)
024030           ELSE
024040                 MOVE "YES" TO  脱出フラグ
024050           END-IF
024060           IF ( 共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
024070                 MOVE SPACE TO  共済連番号Ｗ(6:1)
024080           ELSE
024090                 MOVE "YES" TO  脱出フラグ
024100           END-IF
024110           MOVE  共済連番号集団Ｗ     TO 共済番号Ｗ
024120        END-IF
024130     END-IF.
024140*
024150** 2. 地共済協議会
024160     MOVE SPACE  TO  脱出フラグ.
024170     IF ( 施情－地共済連番号 NOT = ZERO )
024180** 条件(保険者番号)
024190        IF ( 保険者番号ＷＲ(1:2) = "32" OR "33" OR "34" )  AND
024200           ( 保険者番号ＷＲ NOT = "34130021" )
024210*
024220           MOVE  NC"地共済協議会第"   TO 共済連番号名ＮＷ 
024230           MOVE  NC"号"               TO 共済連番号単位ＮＷ 
024240           MOVE  施情－地共済連番号   TO 共済連番号Ｗ
024250           IF ( 共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
024260                 MOVE SPACE TO  共済連番号Ｗ(1:1)
024270           ELSE
024280                 MOVE "YES" TO  脱出フラグ
024290           END-IF
024300           IF ( 共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
024310                 MOVE SPACE TO  共済連番号Ｗ(2:1)
024320           ELSE
024330                 MOVE "YES" TO  脱出フラグ
024340           END-IF
024350           IF ( 共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
024360                 MOVE SPACE TO  共済連番号Ｗ(3:1)
024370           ELSE
024380                 MOVE "YES" TO  脱出フラグ
024390           END-IF
024400           IF ( 共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
024410                 MOVE SPACE TO  共済連番号Ｗ(4:1)
024420           ELSE
024430                 MOVE "YES" TO  脱出フラグ
024440           END-IF
024450           IF ( 共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
024460                 MOVE SPACE TO  共済連番号Ｗ(5:1)
024470           ELSE
024480                 MOVE "YES" TO  脱出フラグ
024490           END-IF
024500           IF ( 共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
024510                 MOVE SPACE TO  共済連番号Ｗ(6:1)
024520           ELSE
024530                 MOVE "YES" TO  脱出フラグ
024540           END-IF
024550           MOVE  共済連番号集団Ｗ     TO 共済番号Ｗ
024560        END-IF
024570**
024580**------/  地共済連番号が未入力で、共済連番号が入力されている時は、無条件に共済連番号
024590**         をセットする。(中央特有)   /
024600     ELSE
024610        IF ( 施情－共済連番号 NOT = ZERO ) AND ( 共済番号Ｗ = SPACE )
024620*
024630           MOVE  NC"共済組合連盟第"   TO 共済連番号名ＮＷ 
024640           MOVE  NC"号"               TO 共済連番号単位ＮＷ 
024650           MOVE  施情－共済連番号     TO 共済連番号Ｗ
024660           IF ( 共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
024670                 MOVE SPACE TO  共済連番号Ｗ(1:1)
024680           ELSE
024690                 MOVE "YES" TO  脱出フラグ
024700           END-IF
024710           IF ( 共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
024720                 MOVE SPACE TO  共済連番号Ｗ(2:1)
024730           ELSE
024740                 MOVE "YES" TO  脱出フラグ
024750           END-IF
024760           IF ( 共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
024770                 MOVE SPACE TO  共済連番号Ｗ(3:1)
024780           ELSE
024790                 MOVE "YES" TO  脱出フラグ
024800           END-IF
024810           IF ( 共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
024820                 MOVE SPACE TO  共済連番号Ｗ(4:1)
024830           ELSE
024840                 MOVE "YES" TO  脱出フラグ
024850           END-IF
024860           IF ( 共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
024870                 MOVE SPACE TO  共済連番号Ｗ(5:1)
024880           ELSE
024890                 MOVE "YES" TO  脱出フラグ
024900           END-IF
024910           IF ( 共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
024920                 MOVE SPACE TO  共済連番号Ｗ(6:1)
024930           ELSE
024940                 MOVE "YES" TO  脱出フラグ
024950           END-IF
024960           MOVE  共済連番号集団Ｗ     TO 共済番号Ｗ
024970        END-IF
024980*
024990     END-IF.
025000*
025010*================================================================*
025020 自衛官番号セット SECTION.
025030*
025040     MOVE SPACE  TO  脱出フラグ.
025050     IF ( 施情－自衛官番号 NOT = ZERO )
025051           IF 施情－防衛省区分 = 1
025052              MOVE  NC"防衛省第"      TO 自衛官番号名ＮＷ 
025053           ELSE
025054              MOVE  NC"防衛庁第"      TO 自衛官番号名ＮＷ 
025055           END-IF
025060*           MOVE  NC"防衛庁第"         TO 自衛官番号名ＮＷ 
025070           MOVE  NC"号"               TO 自衛官番号単位ＮＷ 
025080           MOVE  施情－自衛官番号     TO 自衛官番号Ｗ
025090           IF ( 自衛官番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
025100                 MOVE SPACE TO  自衛官番号Ｗ(1:1)
025110           ELSE
025120                 MOVE "YES" TO  脱出フラグ
025130           END-IF
025140           IF ( 自衛官番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
025150                 MOVE SPACE TO  自衛官番号Ｗ(2:1)
025160           ELSE
025170                 MOVE "YES" TO  脱出フラグ
025180           END-IF
025190           IF ( 自衛官番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
025200                 MOVE SPACE TO  自衛官番号Ｗ(3:1)
025210           ELSE
025220                 MOVE "YES" TO  脱出フラグ
025230           END-IF
025240           IF ( 自衛官番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
025250                 MOVE SPACE TO  自衛官番号Ｗ(4:1)
025260           ELSE
025270                 MOVE "YES" TO  脱出フラグ
025280           END-IF
025290           IF ( 自衛官番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
025300                 MOVE SPACE TO  自衛官番号Ｗ(5:1)
025310           ELSE
025320                 MOVE "YES" TO  脱出フラグ
025330           END-IF
025340           IF ( 自衛官番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
025350                 MOVE SPACE TO  自衛官番号Ｗ(6:1)
025360           ELSE
025370                 MOVE "YES" TO  脱出フラグ
025380           END-IF
025390           MOVE  自衛官番号集団Ｗ     TO 共済番号Ｗ
025400     END-IF.
025410*
025420*================================================================*
025430 受診者情報取得 SECTION.
025440*
025450**************************************************
025460* 連結データから受診者情報Ｆより以下の情報を取得 *
025470* ● 施術年 ..... 施術年Ｗに格納                 *
025480* ● 施術月 ..... 施術月Ｗに格納                 *
025490* ● 患者番号.... 患者番号Ｗに格納※ＦＤ連番用   *
025500* ● 記号 ....... 記号Ｗに格納                   *
025510* ● 番号 ....... 番号Ｗに格納                   *
025520* ● 保険者番号 . 保険者番号Ｗに格納             *
025530* ● 保険種別 ... 保険種別Ｗに格納               *
025540* ● 被保険者カナ.被保険者カナＷに格納           *
025550* ● 被保険者氏名.被保険者氏名Ｗに格納           *
025560* ● 住所１ ......被保険者住所１Ｗに格納         *
025570* ● 住所２ ......被保険者住所２Ｗに格納         *
025580* ● 患者カナ ....患者カナＷに格納               *
025590* ● 患者氏名 ....患者氏名Ｗに格納               *
025600* ● 患者性別 ....区分によりチェックに"○"を格納 *
025610* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
025620* ● 患者年 ......患者年Ｗに格納                 *
025630* ● 患者月 ......患者月Ｗに格納                 *
025640* ● 患者日 ......患者日Ｗに格納                 *
025650* ● 続柄 ........名称マスタより続柄Ｗに取得     *
025660**************************************************
025670     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
025680     MOVE 施術年ＷＲ         TO 受－施術年.
025690     MOVE 施術月ＷＲ         TO 受－施術月.
025700     MOVE 患者コードＷＲ     TO 受－患者コード.
025710     READ 受診者情報Ｆ
025720     INVALID KEY
025730         CONTINUE
025740*            /* ありえない */
025750     NOT INVALID KEY
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
      */本家区分はどれか１つに○をする。
               IF 受－助成種別 = ZERO
                   MOVE NC"○" TO 単独チェックＷ
               ELSE
                   MOVE NC"○" TO ２併チェックＷ
               END-IF
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
025760         MOVE 受－施術年       TO 施術年Ｗ
025770         MOVE 受－施術月       TO 施術月Ｗ
025780         MOVE 受－患者番号     TO 患者番号Ｗ
025790*         MOVE 受－記号         TO 記号Ｗ
025800*         MOVE 受－番号         TO 番号Ｗ
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
025810         MOVE 受－保険者番号   TO 保険者番号Ｗ
025820         MOVE 受－保険種別     TO 保険種別Ｗ
025830         MOVE 受－被保険者カナ TO 被保険者カナＷ
025840         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
025850         MOVE 受－郵便番号１   TO 郵便番号１Ｗ
025860         MOVE 受－郵便番号２   TO 郵便番号２Ｗ
025870         MOVE 受－住所１       TO 被保険者住所１Ｗ
025880         MOVE 受－住所２       TO 被保険者住所２Ｗ
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
025890         MOVE 受－患者カナ     TO 患者カナＷ
025900         MOVE 受－患者氏名     TO 患者氏名Ｗ
025910         IF ( 受－本人家族区分 = 2 )
025920             MOVE 受－患者住所１ TO 患者住所１Ｗ
025930             MOVE 受－患者住所２ TO 患者住所２Ｗ
025940         ELSE
025950             MOVE SPACE          TO 患者住所１Ｗ
025960             MOVE SPACE          TO 患者住所２Ｗ
025970         END-IF
025980         MOVE 受－費用負担者番号助成 TO 市町村番号Ｗ
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
      */被爆＋19～の場合本体に公費負担者番号、受給者番号を記載する。
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
026260*
026270*         IF ( 本人家族区分ＷＲ = 1 )
026280*             MOVE NC"本人"    TO 続柄Ｗ
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
026400** 14/10～　特別区分コメント印字
026410*         IF ( 受－施術和暦年月 >= 41410 )
026420*             IF ( 受－公費種別 = ZERO )
026510*                EVALUATE 受－特別区分
026511*                WHEN 1
026512*                   MOVE "70才以上 1割"  TO 特別コメントＷ
026513*                WHEN 2
026514*                   MOVE "70才以上 2割"  TO 特別コメントＷ
026515*                WHEN 3
026516*                   MOVE "70才以上 3割"  TO 特別コメントＷ
026517*                WHEN 6
026518*                   IF 受－施術和暦年月 < 42004
026519*                      MOVE "3才未満"        TO 特別コメントＷ
026520*                   ELSE
026521*                      MOVE "義務教育就学前" TO 特別コメントＷ
026522*                   END-IF
026523*                END-EVALUATE
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
026524*             END-IF
026525*         END-IF
026530*
026540     END-READ.
026550*================================================================*
026560 請求先情報取得 SECTION.
026570*
026580****************************************************
026590* 連結データから保険者マスタより請求先を取得する。 *
026600* ※保－請求先情報区分=1の場合請求先マスタを使用   *
026610* ● 請求先...... 請求先名称Ｗに格納               *
026620****************************************************
026630     MOVE 保険種別ＷＲ   TO 保－保険種別.
026640     MOVE 保険者番号ＷＲ TO 保－保険者番号.
026650     READ 保険者マスタ
026660     INVALID KEY
026670         MOVE SPACE             TO 請求先名称Ｗ
026680     NOT INVALID KEY
026690         STRING 保－保険者名称  DELIMITED BY SPACE
026700                "  "            DELIMITED BY SIZE
026710                保－支部部署名  DELIMITED BY SPACE
026720                INTO 請求先名称Ｗ
026730         END-STRING
026740     END-READ.
026750*
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
026760*================================================================*
026770 負傷データ取得 SECTION.
026780*
026790**************************************************
026800* 連結データから負傷データＦより以下の情報を取得 *
026810* ● 負傷名...部位＋負傷種別にて加工して格納     *
026820* ● 負傷年.......負傷年Ｗ                       *
026830* ● 負傷月.......負傷月Ｗ                       *
026840* ● 負傷日.......負傷日Ｗ                       *
026850* ● 開始年.......初検年Ｗ                       *
026860* ● 開始月.......初検月Ｗ                       *
026870* ● 開始日.......初検日Ｗ                       *
026880* ● 終了年.......終了年Ｗ                       *
026890* ● 終了月.......終了月Ｗ                       *
026900* ● 終了日.......終了日Ｗ                       *
026910* ● 実日数.......実日数Ｗ                       *
026920* ● 転帰区分 ....区分によりチェックに"○"を格納 *
026930* ● 金属副子 ....区分によりチェックに"○"を格納 *
026940* ● 経過コード...経過マスタより取得             *
026950**************************************************
026960*     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
026970*     MOVE 施術年ＷＲ         TO 負－施術年.
026980*     MOVE 施術月ＷＲ         TO 負－施術月.
026990*     MOVE 患者コードＷＲ     TO 負－患者コード.
027000*     READ 負傷データＦ
027010*     INVALID KEY
027020*         CONTINUE
027030**            /* ありえない */
027040*     NOT INVALID KEY
027050*         MOVE 負－部位数                   TO 部位数Ｗ
027060         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
027070                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
027080             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別Ｗ(部位ＣＮＴ)
027090             MOVE 負－部位(部位ＣＮＴ)     TO 部位Ｗ(部位ＣＮＴ)
027100             MOVE 負－左右区分(部位ＣＮＴ) TO 左右区分Ｗ(部位ＣＮＴ)
027110             MOVE 負－負傷位置番号(部位ＣＮＴ)
027120                                           TO 負傷位置番号Ｗ(部位ＣＮＴ)
027130*********************************************
027140* 注）全柔...負傷種別＋部位にて加工して格納 *
027150*********************************************
027160* 負傷種別
027170             MOVE SPACE                     TO 負傷名称Ｗ
027180             MOVE 03                        TO 名－区分コード
027190             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
027200             READ 名称マスタ
027210             INVALID KEY
027220                 MOVE SPACE        TO 負傷名称Ｗ
027230             NOT INVALID KEY
027240                 MOVE 名－正式名称 TO 負傷名称Ｗ
027250             END-READ
027260* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
028160*
028170             PERFORM 部位名称埋込処理
027450*
027460             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
027470             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
027480             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
027490             MOVE 負－開始年(部位ＣＮＴ)   TO 初検年Ｗ(部位ＣＮＴ)
027500             MOVE 負－開始月(部位ＣＮＴ)   TO 初検月Ｗ(部位ＣＮＴ)
027510             MOVE 負－開始日(部位ＣＮＴ)   TO 初検日Ｗ(部位ＣＮＴ)
027520             IF ( 負－転帰区分(部位ＣＮＴ) = 9 )
027530                 MOVE 99                   TO 終了年Ｗ(部位ＣＮＴ)
027540                 MOVE 99                   TO 終了月Ｗ(部位ＣＮＴ)
027550                 MOVE 99                   TO 終了日Ｗ(部位ＣＮＴ)
027560             ELSE
027570                 MOVE 負－終了年(部位ＣＮＴ)   TO 終了年Ｗ(部位ＣＮＴ)
027580                 MOVE 負－終了月(部位ＣＮＴ)   TO 終了月Ｗ(部位ＣＮＴ)
027590                 MOVE 負－終了日(部位ＣＮＴ)   TO 終了日Ｗ(部位ＣＮＴ)
027600             END-IF
027610* 経過略称取得
027620             MOVE 01                         TO 経－区分コード
027630             MOVE 負－経過コード(部位ＣＮＴ) TO 経－経過コード
027640             READ 経過マスタ
027650             INVALID KEY
027660                 MOVE ZERO            TO 部位ＣＮＴＷ(部位ＣＮＴ)
027670                 MOVE SPACE           TO 部位区切Ｗ(部位ＣＮＴ)
027680                 MOVE SPACE           TO 経過略称Ｗ(部位ＣＮＴ)
027690             NOT INVALID KEY
027700*
027710                 EVALUATE 部位ＣＮＴ
027720                 WHEN 1
027730                     MOVE NC"①" TO 経過部位Ｗ
027740                 WHEN 2
027750                     MOVE NC"②" TO 経過部位Ｗ
027760                 WHEN 3
027770                     MOVE NC"③" TO 経過部位Ｗ
027780                 WHEN 4
027790                     MOVE NC"④" TO 経過部位Ｗ
027800                 WHEN 5
027810                     MOVE NC"⑤" TO 経過部位Ｗ
027820                 END-EVALUATE
027830                 STRING  経過部位Ｗ     DELIMITED BY SPACE
027840                         経－経過略称   DELIMITED BY SPACE
027850                        INTO 印刷経過略称Ｗ(部位ＣＮＴ)
027860                 END-STRING
027870*
027880             END-READ
027890*
027900             MOVE 負－転帰区分(部位ＣＮＴ) TO 転帰区分Ｗ(部位ＣＮＴ)
027910             EVALUATE 負－転帰区分(部位ＣＮＴ)
027920             WHEN 1
027930             WHEN 2
027940                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
027950             WHEN 3
027960                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
027970             WHEN 4
027980                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
027990             END-EVALUATE
028260*
      */実日数はレセ－部位実日数を転記する/160816
031230             MOVE レセ－部位実日数(部位ＣＮＴ) TO 実日数Ｗ(部位ＣＮＴ)
028000         END-PERFORM.
028010* 新規/継続 チェック
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
028070* 枝番判定用
028080         MOVE 負－開始診療日手動区分 TO  開始診療日手動区分Ｗ.
028090*
028100* 負傷原因印刷区分
028110         MOVE 負－レセ負傷原因印刷区分 TO レセ負傷原因印刷区分Ｗ.
028370* 長期理由印刷区分
027880         MOVE 負－レセ長期理由印刷区分 TO 長期理由印刷区分Ｆ.
028120*
028130*     END-READ.
028140*================================================================*
028150*================================================================*
028160 施術記録取得 SECTION.
028170*
028180************************************************************
028190* 作１データから負傷データＦより以下の情報を取得           *
028200* ● 初検加算 .....区分によりチェックに"○"を格納...複数可 *
028210* ● 往療加算 .....区分によりチェックに"○"を格納...複数可 *
028220************************************************************
028230     MOVE  SPACE  TO  初日再検フラグ.
028240     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 部位数Ｗ
028250         IF ( 施術年Ｗ = 初検年Ｗ(部位ＣＮＴ) ) AND
028260            ( 施術月Ｗ = 初検月Ｗ(部位ＣＮＴ) )
028270             MOVE 患者番号ＷＲ          TO 施記－患者番号
028280             MOVE 枝番ＷＲ              TO 施記－枝番
028290             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
028300             MOVE 初検年Ｗ(部位ＣＮＴ)  TO 開始年Ｗ(部位ＣＮＴ) 施記－施術年
028310             MOVE 初検月Ｗ(部位ＣＮＴ)  TO 開始月Ｗ(部位ＣＮＴ) 施記－施術月
028320             MOVE 初検日Ｗ(部位ＣＮＴ)  TO 開始日Ｗ(部位ＣＮＴ) 施記－施術日
028330         ELSE
028340             MOVE 患者番号ＷＲ          TO 施記－患者番号
028350             MOVE 枝番ＷＲ              TO 施記－枝番
028360             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
028370             MOVE 施術年ＷＲ            TO 施記－施術年
028380             MOVE 施術月ＷＲ            TO 施記－施術月
028390             MOVE ZERO                  TO 施記－施術日
028400         END-IF
028410         START 施術記録Ｆ   KEY IS >= 施記－患者コード
028420                                      施記－施術和暦年月日
028430         END-START
028440         IF ( 状態キー = "00" )
      */実日数はレセ－部位実日数を転記する/160816
028450*             MOVE ZERO  TO 実日数Ｗ(部位ＣＮＴ)
028460             MOVE ZERO  TO 終了年ＷＴ
028470             MOVE ZERO  TO 終了月ＷＴ
028480             MOVE ZERO  TO 終了日ＷＴ
028490             MOVE SPACE TO 終了フラグ２
028500             PERFORM 施術記録Ｆ読込
028510             IF ( 終了フラグ２      = SPACE   ) AND
028520                ( 施記－患者コード  = 患者コードＷＲ ) AND
028530                ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
028540                ( 施記－施術年      = 施術年ＷＲ     ) AND
028550                ( 施記－施術月      = 施術月ＷＲ     ) 
028560*
028570*        *****************************************************************
028580*        * 開始年月日 ( その部位が当月初検でないか、
028590*                       当月初検でも枝番がある時は、最初の施術日を開始日)*
028600*        *****************************************************************
028610                 IF ( 施術年Ｗ NOT = 初検年Ｗ(部位ＣＮＴ) ) OR
028620                    ( 施術月Ｗ NOT = 初検月Ｗ(部位ＣＮＴ) ) OR
028630                    ( 開始診療日手動区分Ｗ = 1 )
028640                     MOVE 施記－施術年   TO 開始年Ｗ(部位ＣＮＴ)
028650                     MOVE 施記－施術月   TO 開始月Ｗ(部位ＣＮＴ)
028660                     MOVE 施記－施術日   TO 開始日Ｗ(部位ＣＮＴ)
028670                 END-IF
028680             END-IF
028690             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
028700                           ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
028710                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
028720                           ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
028730                           ( 施記－施術月     NOT = 施術月ＷＲ       ) OR
028740                           ( 施記－施術日         > 終了日Ｗ(部位ＣＮＴ))
028750*               **********
028760*               * 実日数 *
028770*               **********
      */実日数はレセ－部位実日数を転記する/160816
028780*                COMPUTE 実日数Ｗ(部位ＣＮＴ) = 実日数Ｗ(部位ＣＮＴ) + 1
028790                MOVE 施記－施術年               TO 終了年ＷＴ
028800                MOVE 施記－施術月               TO 終了月ＷＴ
028810                MOVE 施記－施術日               TO 終了日ＷＴ
028820*
028830                PERFORM 施術記録Ｆ読込
028840            END-PERFORM
028850        END-IF
028860*       **************************
028870*       * 継続：終了年月日セット *
028880*       **************************
028890        IF ( 転帰区分Ｗ(部位ＣＮＴ) = 9 )
028900            MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
028910            MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
028920            MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
028930        END-IF
028940        IF ( 終了年月日Ｗ(部位ＣＮＴ) > 受理年月日Ｗ )
028950            MOVE 終了年Ｗ(部位ＣＮＴ) TO 受理年Ｗ
028960            MOVE 終了月Ｗ(部位ＣＮＴ) TO 受理月Ｗ
028970            MOVE 終了日Ｗ(部位ＣＮＴ) TO 受理日Ｗ
028980        END-IF
028990     END-PERFORM.
029000*
029010** ----- 前月初検のみかを判定 -----------*
029020*
029030*     MOVE 患者番号ＷＲ          TO 施記－患者番号.
029040*     MOVE 枝番ＷＲ              TO 施記－枝番.
029050*     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
029060*     MOVE 施術年ＷＲ            TO 施記－施術年.
029070*     MOVE 施術月ＷＲ            TO 施記－施術月.
029080*     MOVE ZERO                  TO 施記－施術日.
029090*     START 施術記録Ｆ   KEY IS >= 施記－患者コード
029100*                                  施記－施術和暦年月日
029110*     END-START.
029120*     IF ( 状態キー = "00" )
029130*             MOVE SPACE TO 終了フラグ２
029140*             PERFORM 施術記録Ｆ読込
029150*             IF ( 終了フラグ２      = SPACE   ) AND
029160*                ( 施記－患者コード  = 患者コードＷＲ ) AND
029170*                ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
029180*                ( 施記－施術年      = 施術年ＷＲ     ) AND
029190*                ( 施記－施術月      = 施術月ＷＲ     ) 
029200** 当月施術開始日が再検かどうか判定
029210*                 IF ( 施記－再検料請求 = 1 )
029220*                      MOVE "YES"  TO  初日再検フラグ
029230*                 END-IF
029240**
029250*             END-IF
029260*     END-IF.
029270*     IF ( 初日再検フラグ = "YES" )
029280*        PERFORM 前月初検のみ判定
029290*     END-IF.
029300*
029310*================================================================*
029320*================================================================*
029330 レセプト並び順取得 SECTION.
029340*================================================================*
029350     MOVE 施術和暦ＷＲ       TO 作４－施術和暦.
029360     MOVE 施術年ＷＲ         TO 作４－施術年.
029370     MOVE 施術月ＷＲ         TO 作４－施術月.
029380     MOVE 患者コードＷＲ     TO 作４－患者コード.
029390     MOVE 保険種別ＷＲ       TO 作４－保険種別.
029400     READ 作業ファイル４
029410     NOT INVALID KEY
029420          MOVE NC"№"        TO 順番固定Ｗ
029430          MOVE 作４－順番    TO 順番Ｗ
029440     END-READ.
029450*
029460*================================================================*
029470*================================================================*
029480* ＯＣＲ情報取得 SECTION.
029490*
029500****************************************************************
029510* ●     接骨師会会員番号 .....施術所情報マスタより既に取得    *
029520* ●     施術年月         .....受診者情報Ｆより既に取得        *
029530* ● 　　保険種別         .....全柔用国保コード42をセット    　*
029540*                              新規マスタにより対応の可能性有り*
029550* ●     指定各社ＩＤ     .....自社ＩＤ"21"をセット            *
029560* ●     ＦＤ連番         .....資料参照                        *
029570* ● 　　請求金額         .....レセ－請求金額よりセット        *
029580* ●　　 負担割合         .....負担割合 = レセ－負担割合 / 10　*
029590****************************************************************
029600* 接骨師会会員番号
029610*     MOVE ＯＣＲ接骨師会会員番号Ｗ TO ＯＣＲ会員番号Ｗ.
029620* 施術年月
029630*     MOVE 施術年月Ｗ         TO ＯＣＲ施術年月Ｗ.
029640* 保険種別 = 全柔コード = 42
029650*     MOVE 42                 TO ＯＣＲ保険種別Ｗ.
029660* 指定各社ＩＤ = 自社ＩＤ = 21
029670*     MOVE "21"               TO ＯＣＲ各社ＩＤＷ.
029680* ＦＤ連番 = ＦＤ連番患者番号Ｗ + ＦＤ連番健保ＩＤＷ
029690*     MOVE 患者番号Ｗ         TO ＦＤ連番患者番号Ｗ.
029700*     MOVE 1                  TO ＦＤ連番健保ＩＤＷ.
029710* 請求金額
029720*     MOVE レセ－請求額       TO ＯＣＲ請求金額Ｗ.
029730* 負担割合
029740*     MOVE レセ－負担率     TO 負担割合ＷＲ.
029750*     COMPUTE ＯＣＲ負担割合Ｗ = 負担割合ＷＲ / 10.
029760*================================================================*
029770 施術記録Ｆ読込 SECTION.
029780*
029790     READ 施術記録Ｆ NEXT
029800     AT END
029810         MOVE "YES" TO 終了フラグ２
029820     END-READ.
029830*================================================================*
029840 印刷処理 SECTION.
029850*
029860     MOVE "YCH6125P" TO  定義体名Ｐ.
029870     MOVE "SCREEN"   TO  項目群名Ｐ.
029880     WRITE YCH6125P.
029890***     WRITE 印刷レコード.
029900     PERFORM エラー処理Ｐ.
029910*================================================================*
029920 エラー処理Ｐ SECTION.
029930*
029940     IF ( 通知情報Ｐ NOT = "00" )
029950         DISPLAY NC"帳票エラー"              UPON CONS
029960         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
029970         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
029980         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
029990         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
030000                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
030010         ACCEPT  キー入力 FROM CONS
030020         PERFORM ファイル閉鎖
030030         MOVE 99 TO PROGRAM-STATUS
030040         EXIT PROGRAM
030050     END-IF.
030060*================================================================*
030070 部位名称埋込処理 SECTION.
030080*
006490     STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
009980            負傷名称Ｗ                    DELIMITED BY SPACE
006500            レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
006520       INTO 負傷名Ｗ(部位ＣＮＴ)
006570     END-STRING.
030280*
030290*================================================================*
030300 初検日以前のデータ判定 SECTION.
030310*
030320*********************************************************************************
030330*  最初の初検日以前の当月中に施術記録レコードがあった時(治癒、中止)は、請求区分の
030340*  継続にもチェックする。(新規と継続の両方)
030350*********************************************************************************
030360** 最初の初検日を取得
030370     MOVE SPACE                 TO 初検フラグ.
030380     MOVE 患者番号ＷＲ          TO 施記－患者番号.
030390     MOVE 枝番ＷＲ              TO 施記－枝番.
030400     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
030410     MOVE 施術年ＷＲ            TO 施記－施術年.
030420     MOVE 施術月ＷＲ            TO 施記－施術月.
030430     MOVE ZERO                  TO 施記－施術日.
030440     START 施術記録Ｆ   KEY IS >= 施記－患者コード
030450                                  施記－施術和暦年月日
030460     END-START.
030470     IF ( 状態キー = "00" )
030480         MOVE ZERO  TO 初検和暦ＷＴ
030490         MOVE ZERO  TO 初検年ＷＴ
030500         MOVE ZERO  TO 初検月ＷＴ
030510         MOVE ZERO  TO 初検日ＷＴ
030520         MOVE SPACE TO 終了フラグ２
030530         PERFORM 施術記録Ｆ読込
030540         PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
030550                       ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
030560                       ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
030570                       ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
030580                       ( 施記－施術月     NOT = 施術月ＷＲ      ) OR
030590                       ( 初検フラグ           = "YES"           ) 
030600           IF ( 施記－診療区分 = 2 )
030610               MOVE 施記－施術和暦           TO 初検和暦ＷＴ
030620               MOVE 施記－施術年             TO 初検年ＷＴ
030630               MOVE 施記－施術月             TO 初検月ＷＴ
030640               MOVE 施記－施術日             TO 初検日ＷＴ
030650               MOVE "YES"                    TO 初検フラグ
030660           END-IF
030670           PERFORM 施術記録Ｆ読込
030680         END-PERFORM
030690     END-IF.
030700*
030710* 初検日以前のデータ判定
030720     IF ( 初検フラグ = "YES" )
030730        MOVE 患者番号ＷＲ          TO 施記－患者番号
030740        MOVE 枝番ＷＲ              TO 施記－枝番
030750        MOVE 初検和暦ＷＴ          TO 施記－施術和暦
030760        MOVE 初検年ＷＴ            TO 施記－施術年
030770        MOVE 初検月ＷＴ            TO 施記－施術月
030780        MOVE 初検日ＷＴ            TO 施記－施術日
030790        START 施術記録Ｆ   KEY IS <  施記－患者コード
030800                                     施記－施術和暦年月日
030810                                     REVERSED
030820        END-START
030830        IF ( 状態キー = "00" )
030840           MOVE SPACE  TO 終了フラグ２
030850           PERFORM 施術記録Ｆ読込
030860           IF ( 終了フラグ２    = SPACE        ) AND
030870              ( 施記－患者番号  = 患者番号ＷＲ ) AND
030880              ( 施記－枝番      = 枝番ＷＲ     ) AND
030890              ( 施記－施術和暦  = 初検和暦ＷＴ ) AND
030900              ( 施記－施術年    = 初検年ＷＴ   ) AND
030910              ( 施記－施術月    = 初検月ＷＴ   )
030920*  初検日以前の当月中に施術記録レコードがあった時
030930                IF ( 継続チェックＷ = SPACE )
030940                   MOVE NC"○"    TO 継続チェックＷ
030950                END-IF
030960           END-IF
030970         END-IF
030980     END-IF.
030990*
031000*================================================================*
031010 長期判定取得 SECTION.
031020*
031030* ３カ月以上の長期判定は "CHOUKI" を呼ぶ. 
031040     MOVE  SPACE TO  連期間－キー.
031050     INITIALIZE      連期間－キー.
031060     MOVE 施術和暦ＷＲ  TO  連期間－施術和暦.
031070     MOVE 施術年ＷＲ    TO  連期間－施術年.
031080     MOVE 施術月ＷＲ    TO  連期間－施術月.
031090     MOVE 患者番号ＷＲ  TO  連期間－患者番号.
031100     MOVE 枝番ＷＲ      TO  連期間－枝番.
031110*
031120     CALL   "CHOUKI".
031130     CANCEL "CHOUKI".
031140*
031150**** 適用１を使用 (「前月初検のみ」がある時は、くっつける)
031160     IF ( 連期間－対象フラグ  = "YES" )
031170        IF ( 適用１Ｗ  = SPACE )
031180           MOVE NC"※長期施術継続理由裏面に記載"  TO 適用１Ｗ
031190        ELSE
031200           STRING 適用１Ｗ           DELIMITED BY SPACE
031210                  NC"，"             DELIMITED BY SIZE
031220                  NC"※長期施術継続理由裏面に記載"   DELIMITED BY SIZE
031230                  INTO 適用１Ｗ
031240           END-STRING
031250        END-IF
031260     END-IF.
031270*
031280*================================================================*
031290 初検加算時刻取得 SECTION.
031300*****************************************************************
031310** 初検加算が時間外と深夜の時、適用に「受付時間」を印字する。
031320**   時刻の印字は月3回まで可能
031330*****************************************************************
031340     IF ( レセ－時間外 = 1 ) OR ( レセ－深夜 = 1 ) OR ( レセ－休日 = 1 )
031350*
031360         MOVE 患者番号ＷＲ          TO 施記－患者番号
031370         MOVE 枝番ＷＲ              TO 施記－枝番
031380         MOVE 施術和暦ＷＲ          TO 施記－施術和暦
031390         MOVE 施術年ＷＲ            TO 施記－施術年
031400         MOVE 施術月ＷＲ            TO 施記－施術月
031410         MOVE ZERO                  TO 施記－施術日
031420         START 施術記録Ｆ   KEY IS >= 施記－患者コード
031430                                      施記－施術和暦年月日
031440         END-START
031450         IF ( 状態キー = "00" )
031460             MOVE ZERO  TO 初検加算カウント
031470             MOVE SPACE TO 終了フラグ２
031480             PERFORM 施術記録Ｆ読込
031490             PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
031500                           ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
031510                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
031520                           ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
031530                           ( 施記－施術月     NOT = 施術月ＷＲ      ) 
031540               IF ( 施記－初検加算 = 1 OR 2 OR 3 ) AND ( 施記－診療区分 = 2 )
031550                   COMPUTE 初検加算カウント = 初検加算カウント  + 1
031560                   IF ( 初検加算カウント <= 3 )
031570                       MOVE 施記－初検加算 TO 初検加算区分ＷＴ(初検加算カウント)
031580                       MOVE 施記－受付時   TO 初検加算時ＷＴ(初検加算カウント)
031590                       MOVE 施記－受付分   TO 初検加算分ＷＴ(初検加算カウント)
031600                   END-IF
031610               END-IF
031620               PERFORM 施術記録Ｆ読込
031630             END-PERFORM
031640** 初検加算の時刻を適用にセット
033380            IF ( 初検加算時ＷＴ(1) NOT = ZERO ) OR ( 初検加算分ＷＴ(1) NOT = ZERO ) 
                      MOVE 初検加算時ＷＴ(1) TO 初検加算時Ｗ
                      MOVE ":"               TO 初検加算区切Ｗ
                      MOVE 初検加算分ＷＴ(1) TO 初検加算分Ｗ
                  END-IF
033380            IF ( 初検加算時ＷＴ(2) NOT = ZERO ) OR ( 初検加算分ＷＴ(2) NOT = ZERO ) 
031910                PERFORM 初検加算適用セット
                  END-IF
031660         END-IF
031670*
031680     END-IF.
031690*
031700*================================================================*
031710 初検加算適用セット SECTION.
031720*
031730     PERFORM VARYING 番号カウンタ FROM 1 BY 1
031740              UNTIL  番号カウンタ > 3
031750         IF ( 初検加算時ＷＴ(番号カウンタ)  = ZERO )  AND 
031760            ( 初検加算分ＷＴ(番号カウンタ)  = ZERO ) 
031770             CONTINUE
031780         ELSE
031790* 固定項目
031800             EVALUATE 初検加算区分ＷＴ(番号カウンタ) 
031810             WHEN 1
031820                MOVE NC"時間外"   TO 加算内容Ｗ(番号カウンタ)
033320             WHEN 2
033330                MOVE NC"休　日"   TO 加算内容Ｗ(番号カウンタ)
031830             WHEN 3
031840                MOVE NC"深　夜"   TO 加算内容Ｗ(番号カウンタ)
031850             END-EVALUATE
031860*
031870             MOVE NC"："          TO 加算区切Ｗ(番号カウンタ)
031880             MOVE NC"時"          TO 時固定Ｗ(番号カウンタ)
031890             MOVE NC"分"          TO 分固定Ｗ(番号カウンタ)
031900*
031910**** 数字→日本語変換
031920* 時間
031930             MOVE 初検加算時ＷＴ(番号カウンタ)  TO  数字Ｗ
031940             IF ( 数字Ｗ >= 10 )
031950                MOVE 数字Ｗ１    TO 負傷番号Ｗ１
031960                PERFORM 日本語変換
031970                MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ１(番号カウンタ)
031980                MOVE 数字Ｗ２    TO 負傷番号Ｗ１
031990                PERFORM 日本語変換
032000                MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
032010             ELSE
032020                MOVE 数字Ｗ２    TO 負傷番号Ｗ１
032030                PERFORM 日本語変換
032040                MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
032050             END-IF
032060* 分
032070             MOVE 初検加算分ＷＴ(番号カウンタ)  TO  数字Ｗ
032080             MOVE 数字Ｗ１    TO 負傷番号Ｗ１
032090             PERFORM 日本語変換
032100             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ１(番号カウンタ)
032110             MOVE 数字Ｗ２    TO 負傷番号Ｗ１
032120             PERFORM 日本語変換
032130             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ２(番号カウンタ)
032140** 
032150        END-IF
032160     END-PERFORM.
032170*
032180     MOVE  初検加算集団ＮＷ(1)   TO 初検加算時刻１Ｗ. 
032190     MOVE  初検加算集団ＮＷ(2)   TO 初検加算時刻２Ｗ. 
032200     MOVE  初検加算集団ＮＷ(3)   TO 初検加算時刻３Ｗ. 
032210*
032220**** 適用１か２を使用（長期理由記載で適用１を使っている時は、適用２）
032230     IF ( 初検加算時ＷＴ(2)  = ZERO ) AND ( 初検加算分ＷＴ(2)  = ZERO ) 
032240     CONTINUE
032250     ELSE
032260         IF ( 適用１Ｗ  = SPACE )
032270               STRING NC"初検加算"       DELIMITED BY SIZE
032280                      初検加算時刻１Ｗ   DELIMITED BY SIZE
032290                      初検加算時刻２Ｗ   DELIMITED BY SIZE
032300                      初検加算時刻３Ｗ   DELIMITED BY SIZE
032310                INTO 適用１Ｗ
032320         END-STRING
032330         ELSE
032340               STRING NC"初検加算"       DELIMITED BY SIZE
032350                      初検加算時刻１Ｗ   DELIMITED BY SIZE
032360                      初検加算時刻２Ｗ   DELIMITED BY SIZE
032370                      初検加算時刻３Ｗ   DELIMITED BY SIZE
032380                      INTO 適用２Ｗ
032390         END-STRING
032400         END-IF
032410     END-IF.
032420*
032430*================================================================*
032440 日本語変換 SECTION.
032450*
032460     MOVE NC"０"     TO 全角負傷番号Ｗ.
032470     CALL "htoz" WITH C LINKAGE
032480                        USING 負傷番号Ｗ１ 全角負傷番号Ｗ１.
032490*
032500*================================================================*
032510*================================================================*
032520 負傷原因取得 SECTION.
032530*
032540********************************************************************
032550*  負傷原因コードが同じものは、1行にまとめて印字する。
032560*  例: ①② 家で転んだ.
032570*     負傷原因コードが同じものをまとめ、テーブルにセット
032580*     (ただし、部位を飛んで同じものは、2行になる)
032590********************************************************************
032600     MOVE  ZERO   TO  カウンタ カウンタ２.
032610     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
032620             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
032630*
032640****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
032650        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
032660*
032670           IF ( カウンタ = ZERO )
032680               MOVE 1   TO  カウンタ カウンタ２
032690               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
032700               MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
032710               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
032720           ELSE
032730              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
032740                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
032750                 COMPUTE カウンタ２ = カウンタ２  +  1
032760                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
032770              ELSE
032780                 COMPUTE カウンタ = カウンタ  +  1
032790                 MOVE 1   TO  カウンタ２
032800                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
032810                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
032820                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
032830              END-IF
032840           END-IF
032850        END-IF
032860     END-PERFORM.
032870**************************************************************************
032880*  負傷原因マスタより文章取得
032890**************************************************************************
032900     MOVE  ZERO   TO  カウンタ カウンタ２.
032910     PERFORM VARYING カウンタ FROM 1 BY 1
032920             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
032930** 健保は 区分 01
032940         MOVE 01                        TO 負原－区分コード
032950         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
032960         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
032970         READ 負傷原因Ｆ
032980         NOT INVALID KEY
032990             INITIALIZE 負傷原因ＷＴ
033000             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
033010             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
033020             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
033030             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
033040             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
033050             PERFORM VARYING カウンタ２ FROM 1 BY 1
033060                     UNTIL ( カウンタ２ > 9 )  OR 
033070                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
033080                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
033090                WHEN 1
033100                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
033110                WHEN 2
033120                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
033130                WHEN 3
033140                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
033150                WHEN 4
033160                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
033170                WHEN 5
033180                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
033150                WHEN 6
033160                   MOVE "⑥"  TO  負傷原因ナンバーＷ１(カウンタ２)
033170                WHEN 7
033180                   MOVE "⑦"  TO  負傷原因ナンバーＷ１(カウンタ２)
033190                WHEN OTHER
033200                   CONTINUE
033210                END-EVALUATE
033220             END-PERFORM
033230*
033312             IF 負原－負傷原因入力区分 = 1
033313                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
033314                        負傷原因１ＷＴ  DELIMITED BY SIZE
033315                        負傷原因２ＷＴ  DELIMITED BY SIZE
033316                        負傷原因３ＷＴ  DELIMITED BY SIZE
033317                        負傷原因４ＷＴ  DELIMITED BY SIZE
033318                        負傷原因５ＷＴ  DELIMITED BY SIZE
033319                        INTO 負傷原因内容合成Ｗ(カウンタ)
033320                 END-STRING
033321             ELSE
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
033331*
033332         END-READ
033333     END-PERFORM.
033340*
033350     PERFORM 負傷原因セット.
033360*
033370*================================================================*
033380 負傷原因セット SECTION.
033390*
033400**************************************************************************
033410*  文章が1行を超える時は、複数行に分解する。
033420**************************************************************************
033430     MOVE  ZERO   TO  カウンタ カウンタ２.
033440     PERFORM VARYING カウンタ FROM 1 BY 1
033450             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
033460*
033470          INITIALIZE 負傷原因内容分解ＸＷ
033480          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
033490          IF ( 負傷原因内容１ＸＷ  NOT = SPACE )
033500              COMPUTE カウンタ２ = カウンタ２  +  1
033510              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
033520          END-IF
033530          IF ( 負傷原因内容２ＸＷ  NOT = SPACE )
033540              COMPUTE カウンタ２ = カウンタ２  +  1
033550              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
033560          END-IF
033570          IF ( 負傷原因内容３ＸＷ  NOT = SPACE )
033580              COMPUTE カウンタ２ = カウンタ２  +  1
033590              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
033600          END-IF
033830          IF ( 負傷原因内容４ＸＷ  NOT = SPACE )
033840              COMPUTE カウンタ２ = カウンタ２  +  1
033850              MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
033860          END-IF
033610*
033620     END-PERFORM.
033630*================================================================*
033640 助成印取得 SECTION.
033650*
033660* 2006/04 変更
033670* 助成印は "JOSEIMEI" を呼ぶ. 
033680     MOVE SPACE TO  連助成名称－キー.
033690     INITIALIZE     連助成名称－キー.
033700     MOVE 助成種別ＷＲ           TO 連助成名称－助成種別.
033710     MOVE 費用負担者番号助成ＷＲ TO 連助成名称－費用負担者番号助成.
033720*
033730     CALL   "JOSEIMEI".
033740     CANCEL "JOSEIMEI".
033750*
033760     MOVE 連助成名称－１文字 TO 助成印Ｗ.
033770*
033780***
034030*
034040*================================================================*
034050 前月初検のみ判定 SECTION.
034060*
034070*** 前月の通院日が初検か判定 
034080     MOVE  SPACE            TO 前月フラグ.
034090     MOVE 受－患者コード    TO 施記－患者コード.
034100     MOVE 受－施術和暦      TO 施記－施術和暦.
034110     MOVE 受－施術年        TO 施記－施術年.
034120     MOVE 受－施術月        TO 施記－施術月.
034130     MOVE 1                 TO 施記－施術日.
034140     START 施術記録Ｆ   KEY IS <  施記－患者コード
034150                                  施記－施術和暦年月日
034160                                  REVERSED
034170     END-START.
034180     IF ( 状態キー = "00" )
034190         MOVE SPACE  TO 終了フラグ２
034200         PERFORM 施術記録Ｆ読込
034210         IF ( 終了フラグ２      = SPACE  ) AND
034220            ( 施記－患者コード  = 受－患者コード ) AND
034230            ( 施記－診療区分    = 2 ) 
034240*
034250            PERFORM 前月判定
034260**** 適用１を使用
034270            IF ( 前月フラグ = "YES" )
034280               MOVE NC"※前月初検のみ"    TO  適用１Ｗ
034290            END-IF
034300**
034310         END-IF
034320     END-IF.
034330*
034340*================================================================*
034350 前月判定  SECTION.
034360* 
034370*** 読み込んだ施術記録の年月が、前月かどうか判定 (年月の差が 1 か?)
034380      MOVE  SPACE  TO  前月フラグ.
034390      INITIALIZE  計算年月日Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
034400**
034410      MOVE 受－施術和暦    TO 終了和暦２Ｗ.
034420      MOVE 受－施術年      TO 終了年２Ｗ.
034430      MOVE 受－施術月      TO 終了月２Ｗ.
034440      MOVE 施記－施術和暦  TO 開始和暦２Ｗ.
034450      MOVE 施記－施術年    TO 開始年２Ｗ.
034460      MOVE 施記－施術月    TO 開始月２Ｗ.
034470*
034480      EVALUATE TRUE
034490       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ = 終了年２Ｗ)
034500            PERFORM  前月比較月
034510       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ NOT = 終了年２Ｗ)
034520            PERFORM  前月比較年
034530       WHEN  開始和暦２Ｗ NOT = 終了和暦２Ｗ 
034540            PERFORM  前月比較元号
034550      END-EVALUATE.
034560*
034570      IF ( 計算月Ｗ = 1 )
034580         MOVE  "YES"  TO  前月フラグ
034590      END-IF.
034600*
034610*================================================================*
034620 前月比較月  SECTION.
034630*
034640     IF ( 終了月２Ｗ >  開始月２Ｗ )
034650         COMPUTE 計算月Ｗ = 終了月２Ｗ - 開始月２Ｗ
034660     ELSE
034670        MOVE ZERO TO 計算月Ｗ
034680     END-IF.
034690*
034700*================================================================*
034710 前月比較年  SECTION.
034720*
034730     IF ( 終了年２Ｗ >  開始年２Ｗ )
034740         COMPUTE 計算年Ｗ = 終了年２Ｗ - 開始年２Ｗ
034750         COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
034760     ELSE
034770        MOVE ZERO TO 計算月Ｗ
034780     END-IF.
034790*
034800*================================================================*
034810 前月比較元号  SECTION.
034820*
034830     MOVE 開始和暦２Ｗ TO 元－元号区分.
034840     READ 元号マスタ
034850     NOT INVALID KEY
034860         MOVE 元－開始西暦年 TO 開始西暦年Ｗ
034870     END-READ.
034880     MOVE 終了和暦２Ｗ TO 元－元号区分.
034890     READ 元号マスタ
034900     NOT INVALID KEY
034910         MOVE 元－開始西暦年 TO 終了西暦年Ｗ
034920     END-READ.
034930**
034940     IF ( 開始西暦年Ｗ NOT = ZERO ) AND ( 終了西暦年Ｗ NOT = ZERO )
034950        COMPUTE 開始西暦年Ｗ = 開始西暦年Ｗ + 開始年２Ｗ - 1
034960        COMPUTE 終了西暦年Ｗ = 終了西暦年Ｗ + 終了年２Ｗ - 1
034970*
034980        IF ( 終了西暦年Ｗ =  開始西暦年Ｗ )
034990           PERFORM  前月比較月
035000        ELSE
035010           IF ( 終了西暦年Ｗ >  開始西暦年Ｗ )
035020               COMPUTE 計算年Ｗ = 終了西暦年Ｗ - 開始西暦年Ｗ
035030               COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
035040           ELSE
035050               MOVE ZERO TO 計算月Ｗ
035060           END-IF
035070        END-IF
035080     ELSE
035090        MOVE ZERO TO 計算月Ｗ
035100     END-IF.
035110*
035290*================================================================*
035300 受診者印刷区分更新 SECTION.
035310*
035320** //  受診者情報Ｆの印刷区分に１をセットし、更新する。//  
035330*
035340     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
035350     MOVE 施術年ＷＲ         TO 受－施術年.
035360     MOVE 施術月ＷＲ         TO 受－施術月.
035370     MOVE 患者コードＷＲ     TO 受－患者コード.
035380     READ 受診者情報Ｆ
035390     NOT INVALID KEY
035400         MOVE  1  TO  受－レセ印刷区分
035410         REWRITE  受－レコード
035420         END-REWRITE
035430         IF ( 状態キー NOT = "00" )
035440            MOVE NC"受診者" TO ファイル名
035450            PERFORM エラー表示
035460         END-IF
035470     END-READ.
035480*
035490*================================================================*
035500 月末日取得 SECTION.
035510*
035520     MOVE 施術年ＷＲ   TO 受理年Ｗ.
035530     MOVE 施術月ＷＲ   TO 受理月Ｗ.
035540     MOVE 施術和暦ＷＲ TO 元－元号区分.
035550     READ 元号マスタ
035560     NOT INVALID KEY
035570         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
035580     END-READ.
035590     IF ( 施術西暦年Ｗ NOT = ZERO )
035600        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
035610     END-IF.
035620*
035630     EVALUATE 施術月ＷＲ
035640     WHEN 4
035650     WHEN 6
035660     WHEN 9
035670     WHEN 11
035680         MOVE 30 TO 受理日Ｗ
035690     WHEN 2
035700         DIVIDE 4 INTO 施術西暦年Ｗ GIVING    商Ｗ
035710                                    REMAINDER 余Ｗ
035720         END-DIVIDE
035730         IF ( 余Ｗ = ZERO )
035740             MOVE 29 TO 受理日Ｗ
035750         ELSE
035760             MOVE 28 TO 受理日Ｗ
035770         END-IF
035780     WHEN 1
035790     WHEN 3
035800     WHEN 5
035810     WHEN 7
035820     WHEN 8
035830     WHEN 10
035840     WHEN 12
035850         MOVE 31 TO 受理日Ｗ
035860     WHEN OTHER
035870          CONTINUE
035880     END-EVALUATE.
035890*
035900*================================================================*
035910 委任年月日取得 SECTION.
035920*
035930** ---// ここの受理年には、最終通院日が入っている為、退避する //----
035940     MOVE 受理年Ｗ   TO 最終通院年Ｗ.
035950     MOVE 受理月Ｗ   TO 最終通院月Ｗ.
035960     MOVE 受理日Ｗ   TO 最終通院日Ｗ.
035970***
035980* (柔整師側)
035990     EVALUATE レセプト日付区分Ｗ 
036000*    /  最終通院日 /
036010     WHEN ZERO
036020         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
036030         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
036040         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
036050*    /  月末日 /
036060     WHEN 1 
036070         PERFORM 月末日取得
036080         MOVE 受理年Ｗ     TO 柔整師年Ｗ
036090         MOVE 受理月Ｗ     TO 柔整師月Ｗ
036100         MOVE 受理日Ｗ     TO 柔整師日Ｗ
036110*    /  印字なし /
036120     WHEN 9
036130         MOVE ZERO         TO 柔整師年Ｗ
036140         MOVE ZERO         TO 柔整師月Ｗ
036150         MOVE ZERO         TO 柔整師日Ｗ
036160*    /  その他は、最終通院日 /
036170     WHEN OTHER
036180         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
036190         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
036200         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
036210     END-EVALUATE.
036220**
036230* (患者側)
036240     EVALUATE レセプト患者日付区分Ｗ 
036250*    /  最終通院日 /
036260     WHEN ZERO
036270         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
036280         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
036290         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
036300*    /  月末日 /
036310     WHEN 1 
036320         PERFORM 月末日取得
036330         MOVE 受理年Ｗ     TO 患者委任年Ｗ
036340         MOVE 受理月Ｗ     TO 患者委任月Ｗ
036350         MOVE 受理日Ｗ     TO 患者委任日Ｗ
036360*    /  印字なし /
036370     WHEN 9
036380         MOVE ZERO         TO 患者委任年Ｗ
036390         MOVE ZERO         TO 患者委任月Ｗ
036400         MOVE ZERO         TO 患者委任日Ｗ
036410*    /  その他は、最終通院日 /
036420     WHEN OTHER
036430         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
036440         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
036450         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
036460     END-EVALUATE.
036470*
036480*================================================================*
036490 レセ摘要再セット SECTION.
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
036670*
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
036680*================================================================*
036681*================================================================*
036682 地域特有処理 SECTION.
036683*
036684*--------------------------------------------------------*
036685*  福岡県：経過欄の固定印字 (全柔ＦＰＤ区分Ｗ 1 使用)
036686*  長期以外の部位は、「順調」
036687*  長期の部位は、「緩慢」
036688*--------------------------------------------------------*
036689*
036690     IF 全柔ＦＰＤ区分Ｗ = 1
036691*      まず「順調」セット
036692        PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
016870                UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR
016880                      ( 部位ＣＮＴ > 5 )
036694*
036695                 EVALUATE 部位ＣＮＴ
036696                 WHEN 1
036697                     MOVE NC"①" TO 経過部位数字Ｗ
036698                 WHEN 2
036699                     MOVE NC"②" TO 経過部位数字Ｗ
036700                 WHEN 3
036701                     MOVE NC"③" TO 経過部位数字Ｗ
036702                 WHEN 4
036703                     MOVE NC"④" TO 経過部位数字Ｗ
036704                 WHEN 5
036705                     MOVE NC"⑤" TO 経過部位数字Ｗ
036706                 END-EVALUATE
036707                 MOVE SPACE TO 経過略称(部位ＣＮＴ)
036708                 STRING  経過部位数字Ｗ   DELIMITED BY SPACE
036709                         NC"順調"         DELIMITED BY SPACE
036710                        INTO 経過略称(部位ＣＮＴ)
036711                 END-STRING
036712        END-PERFORM
036713*
036714*      次に、３カ月以上の長期判定
036715        MOVE  SPACE TO  連期間－キー
036716        INITIALIZE      連期間－キー
036717        MOVE 施術和暦ＷＲ  TO  連期間－施術和暦
036718        MOVE 施術年ＷＲ    TO  連期間－施術年
036719        MOVE 施術月ＷＲ    TO  連期間－施術月
036720        MOVE 患者番号ＷＲ  TO  連期間－患者番号
036721        MOVE 枝番ＷＲ      TO  連期間－枝番
036722        CALL   "CHOUKI"
036723        CANCEL "CHOUKI"
036724*
036725        IF 連期間－対象フラグ  = "YES"
036726           PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
016870                   UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR
016880                         ( 部位ＣＮＴ > 5 )
036728*
036729               IF 連期間－期間Ｗ(部位ＣＮＴ)  >  ZERO
036730
036731                   EVALUATE 部位ＣＮＴ
036732                   WHEN 1
036733                       MOVE NC"①" TO 経過部位数字Ｗ
036734                   WHEN 2
036735                       MOVE NC"②" TO 経過部位数字Ｗ
036736                   WHEN 3
036737                       MOVE NC"③" TO 経過部位数字Ｗ
036738                   WHEN 4
036739                       MOVE NC"④" TO 経過部位数字Ｗ
036740                   WHEN 5
036741                       MOVE NC"⑤" TO 経過部位数字Ｗ
036742                   END-EVALUATE
036743                   MOVE SPACE TO 経過略称(部位ＣＮＴ)
036744                   STRING  経過部位数字Ｗ   DELIMITED BY SPACE
036745                           NC"緩慢"         DELIMITED BY SPACE
036746                          INTO 経過略称(部位ＣＮＴ)
036747                   END-STRING
036748               END-IF
036749           END-PERFORM
036750        END-IF
036751*
036752     END-IF.
036753*
036754*
036755*================================================================*
036756*================================================================*
036757*================================================================*
036758 エラー表示 SECTION.
036759*
036760     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
036761     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
036762     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
036763     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
036770     ACCEPT  キー入力 FROM CONS
036780     PERFORM ファイル閉鎖.
036790     EXIT PROGRAM.
036800*================================================================*
036810*================================================================*
036820 ファイル閉鎖 SECTION.
036830*
036840     CLOSE 印刷ファイル.
036850     CLOSE 保険者マスタ     元号マスタ          名称マスタ
036860           レセプトＦ       制御情報マスタ      施術所情報マスタ
036870           請求先マスタ     経過マスタ          受診者情報Ｆ
036880           施術記録Ｆ       負傷データＦ        負傷原因Ｆ
036890           作業ファイル４   メモファイル.
036900*================================================================*
036910 終了処理 SECTION.
036920*
036930     PERFORM ファイル閉鎖.
036940*================================================================*
036950*================================================================*
036960 テスト印字処理 SECTION.
036970*
           CONTINUE.
038230*
036390*================================================================*
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
      *================================================================*
038250******************************************************************
038260 END PROGRAM YCH6225.
038270******************************************************************
