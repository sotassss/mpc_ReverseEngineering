000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCH6165.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*         中央 社保レセプト印刷（柔+ｳｨﾝﾄﾞｳｽﾞ版）
000100*         MED = YAW610 YCH6125P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2011-03-08
000130 DATE-COMPILED.          2011-03-08
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
002820 01 終了フラグ３                       PIC X(3)  VALUE SPACE.
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
003020       05 負傷原因ナンバーＷ１         PIC X(2) OCCURS 9 VALUE SPACE.
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
003180*
003190** 前月初検のみ用
003200 01 初日再検フラグ                     PIC X(3)  VALUE SPACE.
003210 01 前月フラグ                         PIC X(3)  VALUE SPACE.
003220*
003230 01 計算年月日Ｗ.
003240    03 計算和暦Ｗ                      PIC 9(1)  VALUE ZERO.
003250    03 計算年Ｗ                        PIC S9(2)  VALUE ZERO.
003260    03 計算月Ｗ                        PIC S9(2)  VALUE ZERO.
003270    03 計算日Ｗ                        PIC S9(2)  VALUE ZERO.
003280 01 開始年月日２Ｗ.
003290    03 開始和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003300    03 開始年２Ｗ                      PIC 9(2)  VALUE ZERO.
003310    03 開始月２Ｗ                      PIC 9(2)  VALUE ZERO.
003320    03 開始日２Ｗ                      PIC 9(2)  VALUE ZERO.
003330    03 開始西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003340 01 終了年月日２Ｗ.
003350    03 終了和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003360    03 終了年２Ｗ                      PIC 9(2)  VALUE ZERO.
003370    03 終了月２Ｗ                      PIC 9(2)  VALUE ZERO.
003380    03 終了日２Ｗ                      PIC 9(2)  VALUE ZERO.
003390    03 終了西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003400***
003410** 負傷原因・長期理由印刷区分用
003420 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
003430 01 長期理由印刷区分Ｗ                 PIC 9 VALUE ZERO.
003440*
003450** レセ下段の日付区分用 (0:最終通院日、1:月末日、9:印字なし)
003460 01 レセプト日付区分Ｗ                 PIC 9 VALUE ZERO.
003470 01 レセプト患者日付区分Ｗ             PIC 9 VALUE ZERO.
003480*
003490*
003500** 月末日用
003510 01 施術西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
003520 01 商Ｗ                               PIC 9(3)  VALUE ZERO.
003530 01 余Ｗ                               PIC 9(3)  VALUE ZERO.
003540*
003550** 枝番判定用
003560 01 開始診療日手動区分Ｗ               PIC 9    VALUE ZERO.
003570*
003580*
003590** レセ摘要用( N(38)固定） /
003600 01 負傷の経過Ｗ.
003610    03 負傷の経過行Ｗ                  PIC X(76) OCCURS 2 VALUE SPACE.
003620 01 負傷の経過ＮＷ REDEFINES 負傷の経過Ｗ.
003630    03 負傷の経過行ＮＷ                PIC N(38) OCCURS 2.
003640*
003650* 負傷原因印刷区分
003660 01 レセ負傷原因印刷区分Ｗ             PIC 9    VALUE ZERO.
003670*
003460 01 長期理由印刷区分Ｆ                 PIC 9 VALUE ZERO.
003671*
003672* 福岡の経過固定印字用に使用
003673 01 全柔ＦＰＤ区分Ｗ                   PIC 9     VALUE ZERO.
003674 01 経過部位数字Ｗ                     PIC N(1)  VALUE SPACE.
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
003675*
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
003676*
003680*
003690****************
003700* 連結項目待避 *
003710****************
003720*    ************
003730*    * 印刷キー *
003740*    ************
003750 01 対象データＷＲ.
003760    03 施術和暦年月ＷＲ.
003770       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
003780       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
003790       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
003800    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
003810    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
003820    03 公費種別ＷＲ                     PIC 9(2)  VALUE ZERO.
003830    03 費用負担者番号ＷＲ               PIC X(10) VALUE SPACE.
003840    03 助成種別ＷＲ                     PIC 9(2)  VALUE ZERO.
003850    03 費用負担者番号助成ＷＲ           PIC X(10) VALUE SPACE.
003860    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
003870    03 患者カナＷＲ                     PIC X(20) VALUE SPACE.
003880    03 患者コードＷＲ.
003890       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
003900       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
003910*    ************
003920*    * 料金情報 *
003930*    ************
003940*    月毎の料金
003950***********************
003960 01 料金１ＷＲ.
003970   03 初検ＷＲ.
003980      05 負担割合ＷＲ               PIC 9(3)    VALUE ZERO.
003990      05 初検料ＷＲ                 PIC 9(5)    VALUE ZERO.
004000      05 初検加算料ＷＲ             PIC 9(5)    VALUE ZERO.
         03 初検時相談料ＷＲ              PIC 9(4)    VALUE ZERO.
004010   03 再検料ＷＲ                    PIC 9(5)    VALUE ZERO.
004020   03 往療ＷＲ.
004030      05 往療距離ＷＲ               PIC 9(2)V9  VALUE ZERO.
004040      05 往療回数ＷＲ               PIC 9(2)    VALUE ZERO.
004050      05 往療料ＷＲ                 PIC 9(6)    VALUE ZERO.
004060      05 往療加算料ＷＲ             PIC 9(5)    VALUE ZERO.
004070   03 金属副子加算料ＷＲ            PIC 9(5)    VALUE ZERO.
004080   03 施術情報提供料ＷＲ            PIC 9(5)    VALUE ZERO.
004090   03 合計ＷＲ                      PIC 9(6)    VALUE ZERO.
004100   03 一部負担金ＷＲ                PIC 9(6)    VALUE ZERO.
004110   03 請求金額ＷＲ                  PIC 9(6)    VALUE ZERO.
004120   03 給付割合ＷＲ                  PIC 9(1)    VALUE ZERO.
004130   03 受給者負担額ＷＲ              PIC 9(6)    VALUE ZERO.
004140   03 助成請求金額ＷＲ              PIC 9(6)    VALUE ZERO.
004150*
004160* 負傷部位毎の料金
004170***********************
004180 01 料金２ＷＲ.
004190   03 初回処置ＷＲ    OCCURS   9.
004200      05 初回処置料ＷＲ             PIC 9(5)    VALUE ZERO.
004210*
004220* 逓減毎の料金
004230***********************
004240 01 料金３ＷＲ.
004250**********
004260* １部位 *
004270**********
004280   03 部位１ＷＲ.
004290      05 後療１ＷＲ.
004300         07 後療単価１ＷＲ              PIC 9(4)    VALUE ZERO.
004310         07 後療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
004320         07 後療料１ＷＲ                PIC 9(5)    VALUE ZERO.
004330      05 冷罨法１ＷＲ.
004340         07 冷罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
004350         07 冷罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
004360      05 温罨法１ＷＲ.
004370         07 温罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
004380         07 温罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
004390      05 電療１ＷＲ.
004400         07 電療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
004410         07 電療料１ＷＲ                PIC 9(4)    VALUE ZERO.
004420      05 小計１ＷＲ                     PIC 9(6)    VALUE ZERO.
004430      05 長期逓減率１ＷＲ               PIC 9(3)    VALUE ZERO.
004440      05 長期込小計１ＷＲ               PIC 9(6)    VALUE ZERO.
004450**********
004460* ２部位 *
004470**********
004480   03 部位２ＷＲ.
004490      05 後療２ＷＲ.
004500         07 後療単価２ＷＲ              PIC 9(4)    VALUE ZERO.
004510         07 後療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
004520         07 後療料２ＷＲ                PIC 9(5)    VALUE ZERO.
004530      05 冷罨法２ＷＲ.
004540         07 冷罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
004550         07 冷罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
004560      05 温罨法２ＷＲ.
004570         07 温罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
004580         07 温罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
004590      05 電療２ＷＲ.
004600         07 電療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
004610         07 電療料２ＷＲ                PIC 9(4)    VALUE ZERO.
004620      05 小計２ＷＲ                     PIC 9(6)    VALUE ZERO.
004630      05 長期逓減率２ＷＲ               PIC 9(3)    VALUE ZERO.
004640      05 長期込小計２ＷＲ               PIC 9(6)    VALUE ZERO.
004650******************
004660* ３部位／８割 *
004670******************
004680   03 部位３８ＷＲ.
004690      05 後療３８ＷＲ.
004700         07 後療単価３８ＷＲ              PIC 9(4)  VALUE ZERO.
004710         07 後療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
004720         07 後療料３８ＷＲ                PIC 9(5)  VALUE ZERO.
004730      05 冷罨法３８ＷＲ.
004740         07 冷罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
004750         07 冷罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
004760      05 温罨法３８ＷＲ.
004770         07 温罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
004780         07 温罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
004790      05 電療３８ＷＲ.
004800         07 電療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
004810         07 電療料３８ＷＲ                PIC 9(4)  VALUE ZERO.
004820      05 小計３８ＷＲ                     PIC 9(6)  VALUE ZERO.
004830      05 多部位込小計３８ＷＲ             PIC 9(6)  VALUE ZERO.
004840      05 長期逓減率３８ＷＲ               PIC 9(3)  VALUE ZERO.
004850      05 長期込小計３８ＷＲ               PIC 9(6)  VALUE ZERO.
004860******************
004870* ３部位／１０割 *
004880******************
004890   03 部位３０ＷＲ.
004900      05 逓減開始月日３０ＷＲ.
004910         07 逓減開始月３０ＷＲ            PIC 9(2)  VALUE ZERO.
004920         07 逓減開始日３０ＷＲ            PIC 9(2)  VALUE ZERO.
004930      05 後療３０ＷＲ.
004940         07 後療単価３０ＷＲ              PIC 9(4)  VALUE ZERO.
004950         07 後療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
004960         07 後療料３０ＷＲ                PIC 9(5)  VALUE ZERO.
004970      05 冷罨法３０ＷＲ.
004980         07 冷罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
004990         07 冷罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
005000      05 温罨法３０ＷＲ.
005010         07 温罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
005020         07 温罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
005030      05 電療３０ＷＲ.
005040         07 電療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
005050         07 電療料３０ＷＲ                PIC 9(4)  VALUE ZERO.
005060      05 小計３０ＷＲ                     PIC 9(6)  VALUE ZERO.
005070      05 長期逓減率３０ＷＲ               PIC 9(3)  VALUE ZERO.
005080      05 長期込小計３０ＷＲ               PIC 9(6)  VALUE ZERO.
005090****************
005100* ４部位／５割 *
005110****************
005120   03 部位４５ＷＲ.
005130      05 後療４５ＷＲ.
005140         07 後療単価４５ＷＲ              PIC 9(4)  VALUE ZERO.
005150         07 後療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005160         07 後療料４５ＷＲ                PIC 9(5)  VALUE ZERO.
005170      05 冷罨法４５ＷＲ.
005180         07 冷罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005190         07 冷罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
005200      05 温罨法４５ＷＲ.
005210         07 温罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005220         07 温罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
005230      05 電療４５ＷＲ.
005240         07 電療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005250         07 電療料４５ＷＲ                PIC 9(4)  VALUE ZERO.
005260      05 小計４５ＷＲ                     PIC 9(6)  VALUE ZERO.
005270      05 多部位込小計４５ＷＲ             PIC 9(6)  VALUE ZERO.
005280      05 長期逓減率４５ＷＲ               PIC 9(3)  VALUE ZERO.
005290      05 長期込小計４５ＷＲ               PIC 9(6)  VALUE ZERO.
005300****************
005310* ４部位／８割 *
005320****************
005330   03 部位４８ＷＲ.
005340      05 逓減開始月日４８ＷＲ.
005350         07 逓減開始月４８ＷＲ            PIC 9(2)  VALUE ZERO.
005360         07 逓減開始日４８ＷＲ            PIC 9(2)  VALUE ZERO.
005370      05 後療４８ＷＲ.
005380         07 後療単価４８ＷＲ              PIC 9(4)  VALUE ZERO.
005390         07 後療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
005400         07 後療料４８ＷＲ                PIC 9(5)  VALUE ZERO.
005410      05 冷罨法４８ＷＲ.
005420         07 冷罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
005430         07 冷罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
005440      05 温罨法４８ＷＲ.
005450         07 温罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
005460         07 温罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
005470      05 電療４８ＷＲ.
005480         07 電療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
005490         07 電療料４８ＷＲ                PIC 9(4)  VALUE ZERO.
005500      05 小計４８ＷＲ                     PIC 9(6)  VALUE ZERO.
005510      05 多部位込小計４８ＷＲ             PIC 9(6)  VALUE ZERO.
005520      05 長期逓減率４８ＷＲ               PIC 9(3)  VALUE ZERO.
005530      05 長期込小計４８ＷＲ               PIC 9(6)  VALUE ZERO.
005540******************
005550* ４部位／１０割 *
005560******************
005570   03 部位４０ＷＲ.
005580      05 逓減開始月日４０ＷＲ.
005590         07 逓減開始月４０ＷＲ            PIC 9(2)  VALUE ZERO.
005600         07 逓減開始日４０ＷＲ            PIC 9(2)  VALUE ZERO.
005610      05 後療４０ＷＲ.
005620         07 後療単価４０ＷＲ              PIC 9(4)  VALUE ZERO.
005630         07 後療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
005640         07 後療料４０ＷＲ                PIC 9(5)  VALUE ZERO.
005650      05 冷罨法４０ＷＲ.
005660         07 冷罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
005670         07 冷罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
005680      05 温罨法４０ＷＲ.
005690         07 温罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
005700         07 温罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
005710      05 電療４０ＷＲ.
005720         07 電療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
005730         07 電療料４０ＷＲ                PIC 9(4)  VALUE ZERO.
005740      05 小計４０ＷＲ                     PIC 9(6)  VALUE ZERO.
005750      05 長期逓減率４０ＷＲ               PIC 9(3)  VALUE ZERO.
005760      05 長期込小計４０ＷＲ               PIC 9(6)  VALUE ZERO.
005770********************
005780* ５部位／２．５割 *
005790********************
005800   03 部位５２ＷＲ.
005810      05 後療５２ＷＲ.
005820         07 後療単価５２ＷＲ              PIC 9(4)  VALUE ZERO.
005830         07 後療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
005840         07 後療料５２ＷＲ                PIC 9(5)  VALUE ZERO.
005850      05 冷罨法５２ＷＲ.
005860         07 冷罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
005870         07 冷罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
005880      05 温罨法５２ＷＲ.
005890         07 温罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
005900         07 温罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
005910      05 電療５２ＷＲ.
005920         07 電療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
005930         07 電療料５２ＷＲ                PIC 9(4)  VALUE ZERO.
005940      05 小計５２ＷＲ                     PIC 9(6)  VALUE ZERO.
005950      05 多部位込小計５２ＷＲ             PIC 9(6)  VALUE ZERO.
005960      05 長期逓減率５２ＷＲ               PIC 9(3)  VALUE ZERO.
005970      05 長期込小計５２ＷＲ               PIC 9(6)  VALUE ZERO.
005980****************
005990* ５部位／５割 *
006000****************
006010   03 部位５５ＷＲ.
006020      05 逓減開始月日５５ＷＲ.
006030         07 逓減開始月５５ＷＲ            PIC 9(2)  VALUE ZERO.
006040         07 逓減開始日５５ＷＲ            PIC 9(2)  VALUE ZERO.
006050      05 後療５５ＷＲ.
006060         07 後療単価５５ＷＲ              PIC 9(4)  VALUE ZERO.
006070         07 後療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
006080         07 後療料５５ＷＲ                PIC 9(5)  VALUE ZERO.
006090      05 冷罨法５５ＷＲ.
006100         07 冷罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
006110         07 冷罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
006120      05 温罨法５５ＷＲ.
006130         07 温罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
006140         07 温罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
006150      05 電療５５ＷＲ.
006160         07 電療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
006170         07 電療料５５ＷＲ                PIC 9(4)  VALUE ZERO.
006180      05 小計５５ＷＲ                     PIC 9(6)  VALUE ZERO.
006190      05 多部位込小計５５ＷＲ             PIC 9(6)  VALUE ZERO.
006200      05 長期逓減率５５ＷＲ               PIC 9(3)  VALUE ZERO.
006210      05 長期込小計５５ＷＲ               PIC 9(6)  VALUE ZERO.
006220****************
006230* ５部位／８割 *
006240****************
006250   03 部位５８ＷＲ.
006260      05 逓減開始月日５８ＷＲ.
006270         07 逓減開始月５８ＷＲ            PIC 9(2)  VALUE ZERO.
006280         07 逓減開始日５８ＷＲ            PIC 9(2)  VALUE ZERO.
006290      05 後療５８ＷＲ.
006300         07 後療単価５８ＷＲ              PIC 9(4)  VALUE ZERO.
006310         07 後療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
006320         07 後療料５８ＷＲ                PIC 9(5)  VALUE ZERO.
006330      05 冷罨法５８ＷＲ.
006340         07 冷罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
006350         07 冷罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
006360      05 温罨法５８ＷＲ.
006370         07 温罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
006380         07 温罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
006390      05 電療５８ＷＲ.
006400         07 電療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
006410         07 電療料５８ＷＲ                PIC 9(4)  VALUE ZERO.
006420      05 小計５８ＷＲ                     PIC 9(6)  VALUE ZERO.
006430      05 多部位込小計５８ＷＲ             PIC 9(6)  VALUE ZERO.
006440      05 長期逓減率５８ＷＲ               PIC 9(3)  VALUE ZERO.
006450      05 長期込小計５８ＷＲ               PIC 9(6)  VALUE ZERO.
006460******************
006470* ５部位／１０割 *
006480******************
006490   03 部位５０ＷＲ.
006500      05 逓減開始月日５０ＷＲ.
006510         07 逓減開始月５０ＷＲ            PIC 9(2)  VALUE ZERO.
006520         07 逓減開始日５０ＷＲ            PIC 9(2)  VALUE ZERO.
006530      05 後療５０ＷＲ.
006540         07 後療単価５０ＷＲ              PIC 9(4)  VALUE ZERO.
006550         07 後療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
006560         07 後療料５０ＷＲ                PIC 9(5)  VALUE ZERO.
006570      05 冷罨法５０ＷＲ.
006580         07 冷罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
006590         07 冷罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
006600      05 温罨法５０ＷＲ.
006610         07 温罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
006620         07 温罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
006630      05 電療５０ＷＲ.
006640         07 電療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
006650         07 電療料５０ＷＲ                PIC 9(4)  VALUE ZERO.
006660      05 小計５０ＷＲ                     PIC 9(6)  VALUE ZERO.
006670      05 長期逓減率５０ＷＲ               PIC 9(3)  VALUE ZERO.
006680      05 長期込小計５０ＷＲ               PIC 9(6)  VALUE ZERO.
006690*
006700**************
006710* 施術所情報 *
006720**************
006730 01 施術所情報Ｗ.
006740    03 柔整師番号Ｗ                    PIC X(16)  VALUE SPACE.
006750    03 接骨師会会員番号Ｗ              PIC X(16)  VALUE SPACE.
006760    03 代表者カナＷ                    PIC X(50)  VALUE SPACE.
006770    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
006780    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
          03 都道府県ＪＩＳＷ                PIC X(2)   VALUE SPACE.
006790    03 施術所住所Ｗ.
006800       05 施術所住所１Ｗ               PIC X(50)  VALUE SPACE.
006810       05 施術所住所２Ｗ               PIC X(50)  VALUE SPACE.
006820    03 施術所郵便番号Ｗ.
006830       05 施術所郵便番号１Ｗ           PIC X(3)   VALUE SPACE.
006840       05 施術所郵便番号２Ｗ           PIC X(4)   VALUE SPACE.
006850    03 施術所電話番号Ｗ                PIC X(20)  VALUE SPACE.
006860    03 定額制受理番号Ｗ                PIC X(15)  VALUE SPACE.
006870    03 受理年月日Ｗ.
006880       05 受理年Ｗ                     PIC 9(2)   VALUE ZERO.
006890       05 受理月Ｗ                     PIC 9(2)   VALUE ZERO.
006900       05 受理日Ｗ                     PIC 9(2)   VALUE ZERO.
006910    03 最終通院年月日Ｗ.
006920       05 最終通院年Ｗ                 PIC 9(2)   VALUE ZERO.
006930       05 最終通院月Ｗ                 PIC 9(2)   VALUE ZERO.
006940       05 最終通院日Ｗ                 PIC 9(2)   VALUE ZERO.
006950    03 柔整師年月日Ｗ.
006960       05 柔整師年Ｗ                   PIC 9(2)   VALUE ZERO.
006970       05 柔整師月Ｗ                   PIC 9(2)   VALUE ZERO.
006980       05 柔整師日Ｗ                   PIC 9(2)   VALUE ZERO.
006990    03 患者委任年月日Ｗ.
007000       05 患者委任年Ｗ                 PIC 9(2)   VALUE ZERO.
007010       05 患者委任月Ｗ                 PIC 9(2)   VALUE ZERO.
007020       05 患者委任日Ｗ                 PIC 9(2)   VALUE ZERO.
007030    03 取引先情報Ｗ.
007040        05 取引先銀行名Ｗ              PIC X(40)  VALUE SPACE.
007050        05 取引先銀行支店名Ｗ          PIC X(40)  VALUE SPACE.
007060        05 預金種別Ｗ                  PIC 9(1)   VALUE ZERO.
007070        05 口座番号Ｗ                  PIC X(10)  VALUE SPACE.
007080        05 口座名義人Ｗ                PIC X(40)  VALUE SPACE.
007090        05 口座名義人カナＷ            PIC X(40)  VALUE SPACE.
007100        05 口座名義人とカナＷ          PIC X(60)  VALUE SPACE.
007110        05 銀行名支店名Ｗ              PIC X(60)  VALUE SPACE.
007120        05 預金種別コメントＷ          PIC N(3)   VALUE SPACE.
007130    03 コメントＷ.
007140        05 コメント１Ｗ                PIC X(30)  VALUE SPACE.
007150        05 コメント２Ｗ                PIC X(30)  VALUE SPACE.
007160        05 コメント３Ｗ                PIC X(30)  VALUE SPACE.
007170**************
007180* 受診者情報 *
007190**************
007200 01 受診者情報Ｗ.
      */元号修正/20190408
          03 施術和暦Ｗ                      PIC 9(1)   VALUE ZERO.
007210    03 施術年月Ｗ.
007220       05 施術年Ｗ                     PIC 9(2)   VALUE ZERO.
007230       05 施術月Ｗ                     PIC 9(2)   VALUE ZERO.
007240*    03 記号Ｗ                          PIC N(12)  VALUE SPACE.
007570    03 記号Ｗ.
007580       05 印刷記号Ｗ                   PIC N(12)  VALUE SPACE.
          03 記号番号Ｗ.
             05 記号番号ＸＷ                 PIC X(40) VALUE SPACE.
007250    03 番号Ｗ.
007260       05 印刷番号Ｗ                   PIC X(15)  VALUE SPACE.
007270       05 FILLER                       PIC X(15)  VALUE SPACE.
007280*    03 種別チェックＷ.
007290*       05 政チェックＷ                 PIC N(1)   VALUE SPACE.
007300*       05 日チェックＷ                 PIC N(1)   VALUE SPACE.
007310*       05 船チェックＷ                 PIC N(1)   VALUE SPACE.
007320*       05 組チェックＷ                 PIC N(1)   VALUE SPACE.
007330    03 保険者番号Ｗ.
007340       05 印刷保険者番号Ｗ             PIC X(8)   VALUE SPACE.
007350       05 FILLER                       PIC X(2)   VALUE SPACE.
007360    03 市町村番号Ｗ.
007370       05 印刷市町村番号Ｗ             PIC X(8)   VALUE SPACE.
007380       05 FILLER                       PIC X(2)   VALUE SPACE.
007390*
007400    03 請求先名称Ｗ.
007410       05 印刷請求先名称１Ｗ           PIC X(38)  VALUE SPACE.
007420       05 印刷請求先名称２Ｗ           PIC X(38)  VALUE SPACE.
          03 支部名Ｗ.
             05 支部名１Ｗ                   PIC X(10)  VALUE SPACE.
             05 支部名２Ｗ                   PIC X(10)  VALUE SPACE.
007430*
007440    03 保険種別Ｗ                      PIC 9(2)   VALUE ZERO.
007450    03 被保険者情報Ｗ.
007460       05 被保険者カナＷ               PIC X(50)  VALUE SPACE.
007470       05 被保険者氏名Ｗ               PIC X(50)  VALUE SPACE.
007480       05 郵便番号Ｗ.
007490          07 郵便番号１Ｗ              PIC X(3)   VALUE SPACE.
007500          07 郵便番号２Ｗ              PIC X(4)   VALUE SPACE.
007510       05 被保険者住所１Ｗ             PIC X(50)  VALUE SPACE.
007520       05 被保険者住所２Ｗ             PIC X(50)  VALUE SPACE.
008990       05 電話番号Ｗ                   PIC X(35)  VALUE SPACE.
007530    03 患者情報Ｗ.
007540       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
007550       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
007560       05 性別チェックＷ.
007570          07 男チェックＷ              PIC N(1)  VALUE SPACE.
007580          07 女チェックＷ              PIC N(1)  VALUE SPACE.
007590       05 患者性別Ｗ.
007600          07 性別Ｗ                    PIC N(1)  VALUE SPACE.
007610       05 和暦チェックＷ.
007620          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
007630          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
007640          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
007650          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
007660          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
      */元号修正/↓↓↓20190408
008210          07 令和チェックＷ            PIC N(1)  VALUE SPACE.
                07 令和ＣＭＷ                PIC X(4)  VALUE SPACE.
009110*          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
      */元号修正/↑↑↑20190408
007670       05 患者年Ｗ                     PIC 9(2)  VALUE ZERO.
007680       05 患者月Ｗ                     PIC 9(2)  VALUE ZERO.
007690       05 患者日Ｗ                     PIC 9(2)  VALUE ZERO.
007700       05 続柄Ｗ.
007710          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
007720          07 FILLER                    PIC X(4)  VALUE SPACE.
007730*
007740       05 負傷原因１Ｗ                 PIC N(37) VALUE SPACE.
007750       05 負傷原因２Ｗ                 PIC N(37) VALUE SPACE.
007760       05 負傷原因３Ｗ                 PIC N(37) VALUE SPACE.
007770       05 負傷原因４Ｗ                 PIC N(37) VALUE SPACE.
007780       05 負傷原因５Ｗ                 PIC N(37) VALUE SPACE.
007790*
008370*       05 負傷原因Ｗ                   PIC N(40) OCCURS 29 VALUE SPACE.
      */半角対応/110421
             05 負傷原因Ｗ OCCURS 29.
                07 負傷原因ＸＷ              PIC X(80)  VALUE SPACE.
007810    03 助成印Ｗ                        PIC N(1)  VALUE SPACE.
007820    03 特別コメントＷ                  PIC X(16) VALUE SPACE.
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
007830*
007840****************
007850* 負傷データＦ *
007860****************
007870 01 負傷情報Ｗ.
007880    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
007890    03 部位情報Ｗ  OCCURS   9.
007900       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
007910       05 部位コードＷ.
007920          07 負傷種別Ｗ                PIC 9(2)  VALUE ZERO.
007930          07 部位Ｗ                    PIC 9(2)  VALUE ZERO.
007940          07 左右区分Ｗ                PIC 9(1)  VALUE ZERO.
007950          07 負傷位置番号Ｗ            PIC 9(2)  VALUE ZERO.
007960       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
007970       05 負傷年月日Ｗ.
007980          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
007990          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
008000          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
008010       05 初検年月日Ｗ.
008020          07 初検年Ｗ                  PIC 9(2)  VALUE ZERO.
008030          07 初検月Ｗ                  PIC 9(2)  VALUE ZERO.
008040          07 初検日Ｗ                  PIC 9(2)  VALUE ZERO.
008050       05 開始年月日Ｗ.
008060          07 開始年Ｗ                  PIC 9(2)  VALUE ZERO.
008070          07 開始月Ｗ                  PIC 9(2)  VALUE ZERO.
008080          07 開始日Ｗ                  PIC 9(2)  VALUE ZERO.
008090       05 終了年月日Ｗ.
008100          07 終了年Ｗ                  PIC 9(2)  VALUE ZERO.
008110          07 終了月Ｗ                  PIC 9(2)  VALUE ZERO.
008120          07 終了日Ｗ                  PIC 9(2)  VALUE ZERO.
008130       05 実日数Ｗ                     PIC 9(2)  VALUE ZERO.
008140       05 転帰区分Ｗ                   PIC 9(1)  VALUE ZERO.
008150       05 転帰区分チェックＷ.
008160          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
008170          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
008180          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
008190       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
008200       05 部位区切Ｗ                   PIC X(1)  VALUE SPACE.
008210       05 経過略称Ｗ.
008220          07 印刷経過略称Ｗ            PIC N(6)  VALUE SPACE.
008240    03 経過部位Ｗ                      PIC N(1)  VALUE SPACE.
008250    03 新規チェックＷ                  PIC N(1)  VALUE SPACE.
008260    03 継続チェックＷ                  PIC N(1)  VALUE SPACE.
          03 施術日Ｗ.
             05 施術日チェックＷ   OCCURS 31 PIC N(1)  VALUE SPACE.
008270****************
008280* ＯＣＲコード *
008290****************
008300* 01 ＯＣＲコードＷ.
008310*    03 ＯＣＲ会員番号Ｗ                PIC X(6)    VALUE ZERO.
008320*    03 ＯＣＲ施術年月Ｗ                PIC 9(4)    VALUE ZERO.
008330*    03 ＯＣＲ保険種別Ｗ                PIC 9(2)    VALUE ZERO.
008340*    03 ＯＣＲ各社ＩＤＷ                PIC 9(2)    VALUE ZERO.
008350*    03 ＯＣＲＦＤ連番Ｗ.
008360*       05 ＦＤ連番患者番号Ｗ           PIC 9(6)    VALUE ZERO.
008370*       05 ＦＤ連番健保ＩＤＷ           PIC 9(1)    VALUE ZERO.
008380*    03 ＯＣＲ請求金額Ｗ                PIC 9(6)    VALUE ZERO.
008390*    03 ＯＣＲ負担割合Ｗ                PIC 9(1)    VALUE ZERO.
008400*    03 ＯＣＲ接骨師会会員番号Ｗ        PIC 9(10)   VALUE ZERO.
008410*
008420************
008430* 料金情報 *
008440************
008450 01 料金情報Ｗ.
008460    03 初検加算Ｗ.
008470       05 時間外チェックＷ                PIC N(1) VALUE SPACE.
008480       05 休日チェックＷ                  PIC N(1) VALUE SPACE.
008490       05 深夜チェックＷ                  PIC N(1) VALUE SPACE.
008500    03 往療加算Ｗ.
008510       05 夜間チェックＷ                  PIC N(1) VALUE SPACE.
008520       05 暴風雨雪チェックＷ              PIC N(1) VALUE SPACE.
008530    03 金属副子チェックＷ.
008540       05 大チェックＷ                    PIC N(1) VALUE SPACE.
008550       05 中チェックＷ                    PIC N(1) VALUE SPACE.
008560       05 小チェックＷ                    PIC N(1) VALUE SPACE.
008570    03 小計Ｗ                             PIC 9(7) VALUE ZERO.
008580    03 初回処置料合計Ｗ                   PIC 9(6) VALUE ZERO.
008590    03 初回処置料チェックＷ.
008600       05 整復料チェックＷ                PIC N(1) VALUE SPACE.
008610       05 固定料チェックＷ                PIC N(1) VALUE SPACE.
008620       05 施療料チェックＷ                PIC N(1) VALUE SPACE.
      */金属副子・運動後療の変更・追加/1805
          03 金属回数Ｗ                         PIC 9(2)  VALUE ZERO.
          03 運動料Ｗ                           PIC 9(4)  VALUE ZERO.
008630************
008640* 備考情報 *
008650************
008660 01 備考情報Ｗ.
008670    03 適用１Ｗ                        PIC N(38) VALUE SPACE.
008680    03 適用２Ｗ                        PIC N(38) VALUE SPACE.
008690*    03 適用３Ｗ                        PIC N(38) VALUE SPACE.
008700*    03 適用４Ｗ                        PIC N(38) VALUE SPACE.
008710    03 経過コメントＷ                  PIC N(60) VALUE SPACE.
008720*
008730*****************
008740* レセプト並び順 *
008750*****************
008760 01 順番固定Ｗ                         PIC N(1) VALUE SPACE.
008770 01 順番Ｗ                             PIC 9(4) VALUE ZERO.
008780*
       01 摘要施術日Ｗ                       PIC X(100) VALUE SPACE.
       01 施術日Ｗ.
          03 施術日２Ｗ                      PIC X(1)  VALUE SPACE.
          03 施術日１Ｗ                      PIC X(1)  VALUE SPACE.
002790** 負担割合用
002800 01 負担割合Ｗ                         PIC 9(2)  VALUE ZERO.
002810 01 給付割合Ｗ                         PIC 9(2)  VALUE ZERO.
008790********************
008800 01 印刷制御.
008810     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
008820     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
008830     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
008840     03 拡張制御Ｐ.
008850         05 端末制御Ｐ.
008860             07 移動方向Ｐ             PIC X(1) VALUE SPACE.
008870             07 移動行数Ｐ             PIC 9(3) VALUE ZERO.
008880         05 詳細制御Ｐ                 PIC X(2) VALUE SPACE.
008890     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
008900     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
008910*
008920 01 計算機西暦年Ｗ                     PIC 9(2) VALUE ZERO.
008930* 日付ＷＯＲＫ
008940 01 和暦終了年Ｗ                       PIC 9(4) VALUE ZERO.
008950 01 計算機西暦.
008960    03 計算機西暦年                    PIC 9(4) VALUE ZERO.
008970    03 計算機西暦月日                  PIC 9(4) VALUE ZERO.
008980 01 計算機西暦Ｒ REDEFINES 計算機西暦.
008990    03 計算機世紀                      PIC 9(2).
009000    03 計算機日付                      PIC 9(6).
009010    03 計算機日付Ｒ REDEFINES 計算機日付.
009020       05 計算機年月                   PIC 9(4).
009030       05 計算機年月Ｒ REDEFINES 計算機年月.
009040         07 計算機年                   PIC 9(2).
009050         07 計算機月                   PIC 9(2).
009060       05 計算機日                     PIC 9(2).
009070*
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
009080******************************************************************
009090*                          連結項目                              *
009100******************************************************************
009110**  画面入力データ
009120 01 連入－入力データ委任印刷 IS EXTERNAL.
009130    03 連入－委任印刷                     PIC 9.
014620*
       01 連入－入力データ電話印刷 IS EXTERNAL.
          03 連入－電話印刷                     PIC 9.
009190*
       01 連入－プレビュー IS EXTERNAL.
          03 連入－プレビュー区分          PIC 9.
009140*
009150** ３カ月長期判定
009160 01 連期間－キー IS EXTERNAL.
009170    03 連期間－施術年月.
009180       05 連期間－施術和暦               PIC 9.
009190 05 連期間－施術年                 PIC 9(2).
009200 05 連期間－施術月                 PIC 9(2).
009210    03  連期間－患者コード.
009220       05 連期間－患者番号               PIC 9(6).
009230       05 連期間－枝番                   PIC X.
009240    03 連期間－対象フラグ                PIC X(3).
009250    03 連期間－期間月Ｗ.
009260       05 連期間－期間Ｗ                 PIC 9(2) OCCURS 9.
009270************
009280* 印刷キー *
009290************
009300*
009310*
009320 01 連レ印－対象データ IS EXTERNAL.
009330    03 連レ印－施術年月日.
009340       05 連レ印－施術和暦                  PIC 9(1).
009350       05 連レ印－施術年                    PIC 9(2).
009360       05 連レ印－施術月                    PIC 9(2).
009370    03 連レ印－患者コード.
009380       05 連レ印－患者番号                  PIC 9(6).
009390       05 連レ印－枝番                      PIC X(1).
009400    03 連レ印－保険種別                     PIC 9(2).
009410    03 連レ印－保険者番号                   PIC X(10).
009420    03 連レ印－公費種別                     PIC 9(2).
009430    03 連レ印－費用負担者番号               PIC X(10).
009440    03 連レ印－助成種別                     PIC 9(2).
009450    03 連レ印－費用負担者番号助成           PIC X(10).
009460    03 連レ印－患者カナ                     PIC X(20).
009470    03 連レ印－本人家族区分                 PIC 9(1).
009480*
012910 01 連レ－キー IS EXTERNAL.
012920    03 連レ－保険種別                  PIC 9(2).
      *
       01 連レ－長野キー IS EXTERNAL GLOBAL.
          03 連レ－長野助成                  PIC 9(2).
012930*
012940************************
012950* 長期理由文セット     *
012960************************
012970 01 連長文－キー IS EXTERNAL.
012980    03 連長文－施術年月.
012990       05 連長文－施術和暦               PIC 9.
013000       05 連長文－施術年                 PIC 9(2).
013010       05 連長文－施術月                 PIC 9(2).
013020    03  連長文－患者コード.
013030       05 連長文－患者番号               PIC 9(6).
013040       05 連長文－枝番                   PIC X.
013050    03 連長文－文桁数                    PIC 9(2).
013060    03 連長文－理由文                    PIC N(63) OCCURS 15.
013070*
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
013071*
013072*************
013073* 助成名称
013074*************
013075 01 連助成名称－キー IS EXTERNAL.
013076    03 連助成名称－助成種別             PIC 9(2).
013077    03 連助成名称－費用負担者番号助成   PIC X(10).
013078*   / OUT /
013079    03 連助成名称－名称集団.
013080       05 連助成名称－１文字            PIC N.
013081       05 連助成名称－略称              PIC N(4).
013082       05 連助成名称－正式名称          PIC N(10).
013083*
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
013084*
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
013085******************************************************************
013090*                      PROCEDURE  DIVISION                       *
013100******************************************************************
013110 PROCEDURE               DIVISION.
013120************
013130*           *
013140* 初期処理   *
013150*           *
013160************
002570     PERFORM プリンタファイル作成.
013170     PERFORM 初期化.
013180************
013190*           *
013200* 主処理     *
013210*           *
013220************
013230* 印刷
013240     PERFORM 連結項目待避.
013250     PERFORM 印刷セット.
013260     PERFORM 印刷処理.
013270************
013280*           *
013290* 終了処理   *
013300*           *
013310************
013320     PERFORM 受診者印刷区分更新.
013330     PERFORM 終了処理.
013340     MOVE ZERO  TO PROGRAM-STATUS.
013350     EXIT PROGRAM.
013360*
013370*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YCH6165"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分  TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
013380*================================================================*
013390 初期化 SECTION.
013400*
013410     PERFORM ファイルオープン.
013420*    /* 現在日付取得 */
013430     ACCEPT 計算機日付 FROM DATE.
013440*    /* 1980～2079年の間で設定 */
013450     IF ( 計算機年 > 80 )
013460         MOVE 19 TO 計算機世紀
013470     ELSE
013480         MOVE 20 TO 計算機世紀
013490     END-IF.
013500     PERFORM カレント元号取得.
013510     PERFORM 和暦終了年取得.
013520     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 和暦終了年Ｗ.
013530*================================================================*
013540 カレント元号取得 SECTION.
013550*
013560     MOVE ZEROS TO 制－制御区分.
013570     READ 制御情報マスタ
013580     NOT INVALID KEY
013590         MOVE 制－カレント元号         TO カレント元号Ｗ
013600         MOVE 制－レセ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
013610         MOVE 制－レセ長期理由印刷区分 TO 長期理由印刷区分Ｗ
013620         MOVE 制－レセプト日付区分     TO レセプト日付区分Ｗ
013630         MOVE 制－レセプト患者日付区分 TO レセプト患者日付区分Ｗ
013631         MOVE 制－全柔ＦＰＤ区分       TO 全柔ＦＰＤ区分Ｗ
015320         MOVE 制－社保本人レセ         TO 用紙種別Ｗ
013640     END-READ.
013650*
013660*================================================================*
013670 和暦終了年取得 SECTION.
013680*
013690*     DISPLAY NC"カレント元号Ｗ"  カレント元号Ｗ UPON MSGBOX.
013700     MOVE カレント元号Ｗ TO 元－元号区分.
013710     READ 元号マスタ
013720     INVALID KEY
013730         DISPLAY NC"指定和暦が登録されていません" UPON CONS
013740         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
013750                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
013760         ACCEPT  キー入力 FROM CONS
013770         PERFORM 終了処理
013780         EXIT PROGRAM
013790     NOT INVALID KEY
013800         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
013810         MOVE 前和暦Ｗ TO 元－元号区分
013820         READ 元号マスタ
013830         INVALID KEY
013840             DISPLAY NC"指定和暦が登録されていません" UPON CONS
013850             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
013860                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
013870             ACCEPT  キー入力 FROM CONS
013880             PERFORM 終了処理
013890             EXIT PROGRAM
013900         NOT INVALID KEY
013910             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
013920         END-READ
013930     END-READ.
013940*
013950*================================================================*
013960 ファイルオープン SECTION.
013970*
013980     OPEN INPUT   保険者マスタ
013990         MOVE NC"保険者" TO ファイル名.
014000         PERFORM オープンチェック.
014010     OPEN INPUT   元号マスタ
014020         MOVE NC"元号" TO ファイル名.
014030         PERFORM オープンチェック.
014040     OPEN INPUT   名称マスタ
014050         MOVE NC"名称" TO ファイル名.
014060         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
014100     OPEN INPUT   制御情報マスタ
014110         MOVE NC"制御情報" TO ファイル名.
014120         PERFORM オープンチェック.
014130     OPEN INPUT   施術所情報マスタ
014140         MOVE NC"施情" TO ファイル名.
014150         PERFORM オープンチェック.
014160     OPEN INPUT   請求先マスタ
014170         MOVE NC"請先" TO ファイル名.
014180         PERFORM オープンチェック.
014190     OPEN INPUT   経過マスタ
014200         MOVE NC"経過" TO ファイル名.
014210         PERFORM オープンチェック.
014220     OPEN INPUT   施術記録Ｆ.
014230         MOVE NC"施記Ｆ" TO ファイル名.
014240         PERFORM オープンチェック.
014250     OPEN INPUT   負傷データＦ.
014260         MOVE NC"負傷" TO ファイル名.
014270         PERFORM オープンチェック.
014280     OPEN INPUT   負傷原因Ｆ.
014290         MOVE NC"負傷原因" TO ファイル名.
014300         PERFORM オープンチェック.
014310     OPEN INPUT メモファイル.
014320         MOVE NC"メモ" TO ファイル名.
014330         PERFORM オープンチェック.
014340     OPEN INPUT   作業ファイル４.
015170         IF ( 状態キー  NOT =  "00" )
015060            OPEN OUTPUT  作業ファイル４
                  CLOSE 作業ファイル４
015060            OPEN INPUT  作業ファイル４
               END-IF.
014370     OPEN I-O   受診者情報Ｆ.
014380         MOVE NC"受情" TO ファイル名.
014390         PERFORM オープンチェック.
014400     OPEN I-O   印刷ファイル
014410         PERFORM エラー処理Ｐ.
014420*================================================================*
014430 オープンチェック SECTION.
014440*
014450     IF ( 状態キー  NOT =  "00" )
014460         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
014470         DISPLAY NC"状態キー：" 状態キー         UPON CONS
014480         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
014490                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
014500         ACCEPT  キー入力 FROM CONS
014510         PERFORM ファイル閉鎖
014520         EXIT PROGRAM.
014530*================================================================*
014540 連結項目待避 SECTION.
014550*
014560     MOVE 連レ印－施術和暦           TO 施術和暦ＷＲ.
014570     MOVE 連レ印－施術年             TO 施術年ＷＲ.
014580     MOVE 連レ印－施術月             TO 施術月ＷＲ.
014590     MOVE 連レ印－保険種別           TO 保険種別ＷＲ.
014600     MOVE 連レ印－保険者番号         TO 保険者番号ＷＲ.
014610     MOVE 連レ印－公費種別           TO 公費種別ＷＲ.
014620     MOVE 連レ印－費用負担者番号     TO 費用負担者番号ＷＲ.
014630     MOVE 連レ印－助成種別           TO 助成種別ＷＲ.
014640     MOVE 連レ印－費用負担者番号助成 TO 費用負担者番号助成ＷＲ.
014650     MOVE 連レ印－本人家族区分       TO 本人家族区分ＷＲ.
014660     MOVE 連レ印－患者カナ           TO 患者カナＷＲ.
014670     MOVE 連レ印－患者番号           TO 患者番号ＷＲ.
014680     MOVE 連レ印－枝番               TO 枝番ＷＲ.
014690*================================================================*
014700 印刷セット SECTION.
014710*
014720     PERFORM 項目初期化.
014800     PERFORM 負傷読込.
014770     PERFORM 料金情報取得.
014730     PERFORM 施術所情報取得.
014740     PERFORM 請求先情報取得.
014750     PERFORM 受診者情報取得.
014760     PERFORM 負傷データ取得.
014780     PERFORM 施術記録取得.
014790     PERFORM レセプト並び順取得.
014800**     PERFORM ＯＣＲ情報取得.
014810**     PERFORM 長期判定取得.
014820**     PERFORM 初検日以前のデータ判定.
014830     PERFORM 初検加算時刻取得.
014840     PERFORM 助成印取得.
014850     PERFORM 委任年月日取得.
           PERFORM 施術日取得.
014860*
016791*-----------------------------------------------*
016800     IF ( 負傷原因印刷区分Ｗ  NOT = 1 ) AND ( レセ負傷原因印刷区分Ｗ NOT = 1 )
016813        IF ( 負傷原因印刷区分Ｗ = 3 OR 4 )
016815           PERFORM 負傷原因印刷対象判定処理
016817        ELSE
016820           PERFORM 負傷原因取得
016821        END-IF
016830     END-IF.
014990*
015000**********************
015010* ＯＣＲコードセット *
015020**********************
015030*
015040*     MOVE ＯＣＲ接骨師会会員番号Ｗ     TO ＯＣＲ会員番号.
015050*     MOVE ＯＣＲ施術年月Ｗ     TO ＯＣＲ施術年月.
015060*     MOVE ＯＣＲ保険種別Ｗ     TO ＯＣＲ保険種別.
015070*     MOVE ＯＣＲ各社ＩＤＷ     TO ＯＣＲ各社ＩＤ.
015080*     MOVE ＯＣＲＦＤ連番Ｗ     TO ＯＣＲＦＤ連番.
015090*     MOVE ＯＣＲ請求金額Ｗ     TO ＯＣＲ請求金額.
015100*     MOVE ＯＣＲ負担割合Ｗ     TO ＯＣＲ負担割合.
015110*
015120********************
015130* 受診者情報セット *
015140********************
015150     MOVE 施術年Ｗ            TO 施術年.
015160     MOVE 施術月Ｗ            TO 施術月.
015170*
015180*     IF ( 記号Ｗ(1:1) = NC"＊" )
015190*        MOVE  SPACE    TO  記号
015200*     ELSE
015210*        MOVE 記号Ｗ    TO  記号
015220*     END-IF.
015230*     IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
015240*        ( 印刷番号Ｗ(1:2) = "＊" )
015250*        MOVE  SPACE      TO  番号
015260*     ELSE
015270*        MOVE 印刷番号Ｗ  TO  番号
015280*     END-IF.
015290*
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
015300*
015310     MOVE 印刷保険者番号Ｗ    TO 保険者番号.
015320*     MOVE 請求先名称Ｗ        TO 請求先名称.
015330*
015340*     MOVE 印刷請求先名称１Ｗ  TO 保険者名称.
015350*     MOVE 印刷請求先名称２Ｗ  TO 保険者名称２.
015360*
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
015400*     MOVE 被保険者カナＷ      TO 被保険者カナ.
015410     MOVE 被保険者氏名Ｗ      TO 被保険者氏名.
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
015450     MOVE 被保険者住所１Ｗ    TO 住所１.
015460     MOVE 被保険者住所２Ｗ    TO 住所２.
015470*     MOVE 患者カナＷ          TO 患者カナ.
015480     MOVE 患者氏名Ｗ          TO 患者氏名.
015490     MOVE 男チェックＷ        TO 男チェック.
015500     MOVE 女チェックＷ        TO 女チェック.
015510*     MOVE 性別Ｗ              TO 性別.
015520*     MOVE 明治チェックＷ      TO 明治チェック.
015530*     MOVE 大正チェックＷ      TO 大正チェック.
015540     MOVE 昭和チェックＷ      TO 昭和チェック.
015550     MOVE 平成チェックＷ      TO 平成チェック.
015560*     MOVE 元号Ｗ              TO 元号.
      */元号修正↓↓↓/20190408
           MOVE 令和ＣＭＷ         TO 令和ＣＭ.
023070     MOVE 令和チェックＷ     TO 令和チェック.
017390*     MOVE 元号Ｗ              TO 患者和暦.
      */元号修正↑↑↑/20190408
015570     MOVE 患者年Ｗ            TO 患者年.
015580     MOVE 患者月Ｗ            TO 患者月.
015590     MOVE 患者日Ｗ            TO 患者日.
015600*     MOVE 印刷続柄Ｗ          TO 続柄.
015610     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
015620     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
015630     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
015640     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
015650     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
016480     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
015660*
015670     MOVE 助成印Ｗ            TO 助成印.
015680********************
015690* 長期理由文セット *
015700********************
015710*     MOVE 連長文－理由文(1)    TO 長期理由文１.
015720*     MOVE 連長文－理由文(2)    TO 長期理由文２.
015730*     MOVE 連長文－理由文(3)    TO 長期理由文３.
015740*     MOVE 連長文－理由文(4)    TO 長期理由文４.
015750*     MOVE 連長文－理由文(5)    TO 長期理由文５.
015760*     MOVE 連長文－理由文(6)    TO 長期理由文６.
015770*     MOVE 連長文－理由文(7)    TO 長期理由文７.
015780*     IF ( 連長文－理由文(1)  NOT = SPACE )
015790*        MOVE NC"（長期理由）"  TO 長期理由固定
015800*     END-IF.
015810********************
015820* 負傷データセット *
015830********************
015840* １部位 *
015850**********
015860     MOVE 負傷名Ｗ(1)       TO 負傷名１.
015870     MOVE 負傷年Ｗ(1)       TO 負傷年１.
015880     MOVE 負傷月Ｗ(1)       TO 負傷月１.
015890     MOVE 負傷日Ｗ(1)       TO 負傷日１.
015900     MOVE 初検年Ｗ(1)       TO 初検年１.
015910     MOVE 初検月Ｗ(1)       TO 初検月１.
015920     MOVE 初検日Ｗ(1)       TO 初検日１.
015930     MOVE 開始年Ｗ(1)       TO 開始年１.
015940     MOVE 開始月Ｗ(1)       TO 開始月１.
015950     MOVE 開始日Ｗ(1)       TO 開始日１.
015960     MOVE 終了年Ｗ(1)       TO 終了年１.
015970     MOVE 終了月Ｗ(1)       TO 終了月１.
015980     MOVE 終了日Ｗ(1)       TO 終了日１.
015990     MOVE 実日数Ｗ(1)       TO 実日数１.
016000     MOVE 治癒チェックＷ(1) TO 治癒チェック１.
016010     MOVE 中止チェックＷ(1) TO 中止チェック１.
016020     MOVE 転医チェックＷ(1) TO 転医チェック１.
016030**********
016040* ２部位 *
016050**********
016060     MOVE 負傷名Ｗ(2)       TO 負傷名２.
016070     MOVE 負傷年Ｗ(2)       TO 負傷年２.
016080     MOVE 負傷月Ｗ(2)       TO 負傷月２.
016090     MOVE 負傷日Ｗ(2)       TO 負傷日２.
016100     MOVE 初検年Ｗ(2)       TO 初検年２.
016110     MOVE 初検月Ｗ(2)       TO 初検月２.
016120     MOVE 初検日Ｗ(2)       TO 初検日２.
016130     MOVE 開始年Ｗ(2)       TO 開始年２.
016140     MOVE 開始月Ｗ(2)       TO 開始月２.
016150     MOVE 開始日Ｗ(2)       TO 開始日２.
016160     MOVE 終了年Ｗ(2)       TO 終了年２.
016170     MOVE 終了月Ｗ(2)       TO 終了月２.
016180     MOVE 終了日Ｗ(2)       TO 終了日２.
016190     MOVE 実日数Ｗ(2)       TO 実日数２.
016200     MOVE 治癒チェックＷ(2) TO 治癒チェック２.
016210     MOVE 中止チェックＷ(2) TO 中止チェック２.
016220     MOVE 転医チェックＷ(2) TO 転医チェック２.
016230**********
016240* ３部位 *
016250**********
016260     MOVE 負傷名Ｗ(3)       TO 負傷名３.
016270     MOVE 負傷年Ｗ(3)       TO 負傷年３.
016280     MOVE 負傷月Ｗ(3)       TO 負傷月３.
016290     MOVE 負傷日Ｗ(3)       TO 負傷日３.
016300     MOVE 初検年Ｗ(3)       TO 初検年３.
016310     MOVE 初検月Ｗ(3)       TO 初検月３.
016320     MOVE 初検日Ｗ(3)       TO 初検日３.
016330     MOVE 開始年Ｗ(3)       TO 開始年３.
016340     MOVE 開始月Ｗ(3)       TO 開始月３.
016350     MOVE 開始日Ｗ(3)       TO 開始日３.
016360     MOVE 終了年Ｗ(3)       TO 終了年３.
016370     MOVE 終了月Ｗ(3)       TO 終了月３.
016380     MOVE 終了日Ｗ(3)       TO 終了日３.
016390     MOVE 実日数Ｗ(3)       TO 実日数３.
016400     MOVE 治癒チェックＷ(3) TO 治癒チェック３.
016410     MOVE 中止チェックＷ(3) TO 中止チェック３.
016420     MOVE 転医チェックＷ(3) TO 転医チェック３.
016430**********
016440* ４部位 *
016450**********
016460     MOVE 負傷名Ｗ(4)       TO 負傷名４.
016470     MOVE 負傷年Ｗ(4)       TO 負傷年４.
016480     MOVE 負傷月Ｗ(4)       TO 負傷月４.
016490     MOVE 負傷日Ｗ(4)       TO 負傷日４.
016500     MOVE 初検年Ｗ(4)       TO 初検年４.
016510     MOVE 初検月Ｗ(4)       TO 初検月４.
016520     MOVE 初検日Ｗ(4)       TO 初検日４.
016530     MOVE 開始年Ｗ(4)       TO 開始年４.
016540     MOVE 開始月Ｗ(4)       TO 開始月４.
016550     MOVE 開始日Ｗ(4)       TO 開始日４.
016560     MOVE 終了年Ｗ(4)       TO 終了年４.
016570     MOVE 終了月Ｗ(4)       TO 終了月４.
016580     MOVE 終了日Ｗ(4)       TO 終了日４.
016590     MOVE 実日数Ｗ(4)       TO 実日数４.
016600     MOVE 治癒チェックＷ(4) TO 治癒チェック４.
016610     MOVE 中止チェックＷ(4) TO 中止チェック４.
016620     MOVE 転医チェックＷ(4) TO 転医チェック４.
016630**********
016640* ５部位 *
016650**********
016660     MOVE 負傷名Ｗ(5)       TO 負傷名５.
016670     MOVE 負傷年Ｗ(5)       TO 負傷年５.
016680     MOVE 負傷月Ｗ(5)       TO 負傷月５.
016690     MOVE 負傷日Ｗ(5)       TO 負傷日５.
016700     MOVE 初検年Ｗ(5)       TO 初検年５.
016710     MOVE 初検月Ｗ(5)       TO 初検月５.
016720     MOVE 初検日Ｗ(5)       TO 初検日５.
016730     MOVE 開始年Ｗ(5)       TO 開始年５.
016740     MOVE 開始月Ｗ(5)       TO 開始月５.
016750     MOVE 開始日Ｗ(5)       TO 開始日５.
016760     MOVE 終了年Ｗ(5)       TO 終了年５.
016770     MOVE 終了月Ｗ(5)       TO 終了月５.
016780     MOVE 終了日Ｗ(5)       TO 終了日５.
016790     MOVE 実日数Ｗ(5)       TO 実日数５.
016800     MOVE 治癒チェックＷ(5) TO 治癒チェック５.
016810     MOVE 中止チェックＷ(5) TO 中止チェック５.
016820     MOVE 転医チェックＷ(5) TO 転医チェック５.
016830**************
016840* 経過セット *
016850**************
016860     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
016870             UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR
016880                   ( 部位ＣＮＴ > 5 )
016890*         MOVE 部位ＣＮＴＷ(部位ＣＮＴ)   TO 経過部位ＣＮＴ(部位ＣＮＴ)
016900*         MOVE 部位区切Ｗ(部位ＣＮＴ)     TO 部位区切(部位ＣＮＴ)
016910         MOVE 印刷経過略称Ｗ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
016920     END-PERFORM.
016930*****************************************
016940*     新規・継続チェックについて        *
016950*   ●新規...初検有り ●継続...初検なし *
016960*****************************************
016970     MOVE 新規チェックＷ    TO 新規チェック.
016980     MOVE 継続チェックＷ    TO 継続チェック.
016990********************
017000* 料金データセット *
017010********************
017020*    ****************************************************************
017030*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
017040*    ****************************************************************
017050     MOVE 初検料ＷＲ                   TO  初検料.
017060     MOVE 時間外チェックＷ             TO  時間外チェック.
017070     MOVE 休日チェックＷ               TO  休日チェック.
017080     MOVE 深夜チェックＷ               TO  深夜チェック.
017090     MOVE 初検加算料ＷＲ               TO  初検加算料.
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
017100     MOVE 再検料ＷＲ                   TO  再検料.
017110     MOVE 往療距離ＷＲ                 TO  往療距離.
017120     MOVE 往療回数ＷＲ                 TO  往療回数.
017130     MOVE 往療料ＷＲ                   TO  往療料.
017140     MOVE 夜間チェックＷ               TO  夜間チェック.
017150     MOVE 暴風雨雪チェックＷ           TO  暴風雨雪チェック.
017160     MOVE 往療加算料ＷＲ               TO  往療加算料.
      */金属副子・運動後療の変更・追加/1805
           IF 用紙種別Ｗ < 2
              IF ( 施術和暦年月ＷＲ < 43006 )
017170           MOVE 大チェックＷ              TO  大チェック
017180           MOVE 中チェックＷ              TO  中チェック
017190           MOVE 小チェックＷ              TO  小チェック
              ELSE
                 MOVE ALL NC"＝"                TO  金属訂正
              END-IF
           END-IF.
           IF ( 施術和暦年月ＷＲ >= 43006 ) AND ( 金属副子加算料ＷＲ NOT = ZERO )
              MOVE 金属回数Ｗ                TO  金属回数
              MOVE NC"回"                    TO  金属回
           END-IF.
017200     MOVE 金属副子加算料ＷＲ           TO  金属副子加算料.
017210     MOVE 施術情報提供料ＷＲ           TO  施術情報提供料.
017220     MOVE 小計Ｗ                       TO 小計.
017230********************
017240* 初回処置料セット *
017250********************
017260     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
016870             UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR
016880                   ( 部位ＣＮＴ > 5 )
017290         MOVE 初回処置料ＷＲ(部位ＣＮＴ) TO 初回処置料(部位ＣＮＴ)
017300     END-PERFORM.
017310     MOVE 初回処置料合計Ｗ            TO 初回処置料合計
017320*
017330     MOVE 施療料チェックＷ            TO 施療料チェック.
017340     MOVE 整復料チェックＷ            TO 整復料チェック.
017350     MOVE 固定料チェックＷ            TO 固定料チェック.
017360********************
017370* 逓減毎料金セット *
017380********************
017390*    **********
017400*    * １部位 *
017410*    **********
017420     MOVE 後療単価１ＷＲ             TO 後療単価１.
017430     MOVE 後療回数１ＷＲ             TO 後療回数１.
017440     MOVE 後療料１ＷＲ               TO 後療料１.
017450     MOVE 冷罨法回数１ＷＲ           TO 冷罨法回数１.
017460     MOVE 冷罨法料１ＷＲ             TO 冷罨法料１.
017470     MOVE 温罨法回数１ＷＲ           TO 温罨法回数１.
017480     MOVE 温罨法料１ＷＲ             TO 温罨法料１.
017490     MOVE 電療回数１ＷＲ             TO 電療回数１.
017500     MOVE 電療料１ＷＲ               TO 電療料１.
017510     MOVE 小計１ＷＲ                 TO 小計１.
017520     IF ( 長期逓減率１ＷＲ NOT = ZERO )
017530        COMPUTE 長期逓減率１ = 長期逓減率１ＷＲ / 100
017540     END-IF.
017550     MOVE 長期込小計１ＷＲ           TO 長期込小計１.
017560*    **********
017570*    * ２部位 *
017580*    **********
017590     MOVE 後療単価２ＷＲ             TO 後療単価２.
017600     MOVE 後療回数２ＷＲ             TO 後療回数２.
017610     MOVE 後療料２ＷＲ               TO 後療料２.
017620     MOVE 冷罨法回数２ＷＲ           TO 冷罨法回数２.
017630     MOVE 冷罨法料２ＷＲ             TO 冷罨法料２.
017640     MOVE 温罨法回数２ＷＲ           TO 温罨法回数２.
017650     MOVE 温罨法料２ＷＲ             TO 温罨法料２.
017660     MOVE 電療回数２ＷＲ             TO 電療回数２.
017670     MOVE 電療料２ＷＲ               TO 電療料２.
017680     MOVE 小計２ＷＲ                 TO 小計２.
017690     IF ( 長期逓減率２ＷＲ NOT = ZERO )
017700        COMPUTE 長期逓減率２ = 長期逓減率２ＷＲ / 100
017710     END-IF.
017720     MOVE 長期込小計２ＷＲ           TO 長期込小計２.
017730*    ****************
017740*    * ３部位／８割 *
017750*    ****************
017760     MOVE 後療単価３８ＷＲ             TO 後療単価３８.
017770     MOVE 後療回数３８ＷＲ             TO 後療回数３８.
017780     MOVE 後療料３８ＷＲ               TO 後療料３８.
017790     MOVE 冷罨法回数３８ＷＲ           TO 冷罨法回数３８.
017800     MOVE 冷罨法料３８ＷＲ             TO 冷罨法料３８.
017810     MOVE 温罨法回数３８ＷＲ           TO 温罨法回数３８.
017820     MOVE 温罨法料３８ＷＲ             TO 温罨法料３８.
017830     MOVE 電療回数３８ＷＲ             TO 電療回数３８.
017840     MOVE 電療料３８ＷＲ               TO 電療料３８.
017850     MOVE 小計３８ＷＲ                 TO 小計３８.
017860     MOVE 多部位込小計３８ＷＲ         TO 多部位込小計３８.
017870     IF ( 長期逓減率３８ＷＲ NOT = ZERO )
017880        COMPUTE 長期逓減率３８ = 長期逓減率３８ＷＲ / 100
017890     END-IF.
017900     MOVE 長期込小計３８ＷＲ           TO 長期込小計３８.
      */ 逓減率 0.7→0.6 /42505
           IF (施術和暦年月ＷＲ >= 42505)
              MOVE "60"                      TO 逓減３８
              MOVE "0.6"                     TO 多部位３８
           END-IF.
017910*    ****************
017920*    * ３部位／10割 *
017930*    ****************
017940     MOVE 逓減開始月３０ＷＲ           TO 逓減開始月３０.
017950     MOVE 逓減開始日３０ＷＲ           TO 逓減開始日３０.
017960     MOVE 後療単価３０ＷＲ             TO 後療単価３０.
017970     MOVE 後療回数３０ＷＲ             TO 後療回数３０.
017980     MOVE 後療料３０ＷＲ               TO 後療料３０.
017990     MOVE 冷罨法回数３０ＷＲ           TO 冷罨法回数３０.
018000     MOVE 冷罨法料３０ＷＲ             TO 冷罨法料３０.
018010     MOVE 温罨法回数３０ＷＲ           TO 温罨法回数３０.
018020     MOVE 温罨法料３０ＷＲ             TO 温罨法料３０.
018030     MOVE 電療回数３０ＷＲ             TO 電療回数３０.
018040     MOVE 電療料３０ＷＲ               TO 電療料３０.
018050     MOVE 小計３０ＷＲ                 TO 小計３０.
018060     IF ( 長期逓減率３０ＷＲ NOT = ZERO )
018070        COMPUTE 長期逓減率３０ = 長期逓減率３０ＷＲ / 100
018080     END-IF.
018090     MOVE 長期込小計３０ＷＲ           TO 長期込小計３０.
018100*    ****************
018110*    * ４部位／５割 *
018120*    ****************
018130*     MOVE 後療単価４５ＷＲ             TO 後療単価４５.
018140*     MOVE 後療回数４５ＷＲ             TO 後療回数４５.
018150*     MOVE 後療料４５ＷＲ               TO 後療料４５.
018160*     MOVE 冷罨法回数４５ＷＲ           TO 冷罨法回数４５.
018170*     MOVE 冷罨法料４５ＷＲ             TO 冷罨法料４５.
018180*     MOVE 温罨法回数４５ＷＲ           TO 温罨法回数４５.
018190*     MOVE 温罨法料４５ＷＲ             TO 温罨法料４５.
018200*     MOVE 電療回数４５ＷＲ             TO 電療回数４５.
018210*     MOVE 電療料４５ＷＲ               TO 電療料４５.
018220*     MOVE 小計４５ＷＲ                 TO 小計４５.
018230*     MOVE 多部位込小計４５ＷＲ         TO 多部位込小計４５.
018240*     IF ( 長期逓減率４５ＷＲ NOT = ZERO )
018250*        COMPUTE 長期逓減率４５ = 長期逓減率４５ＷＲ / 100
018260*     END-IF.
018270*     MOVE 長期込小計４５ＷＲ           TO 長期込小計４５.
018280*    ****************
018290*    * ４部位／８割 *
018300*    ****************
018310     MOVE 逓減開始月４８ＷＲ           TO 逓減開始月４８.
018320     MOVE 逓減開始日４８ＷＲ           TO 逓減開始日４８.
018330     MOVE 後療単価４８ＷＲ             TO 後療単価４８.
018340     MOVE 後療回数４８ＷＲ             TO 後療回数４８.
018350     MOVE 後療料４８ＷＲ               TO 後療料４８.
018360     MOVE 冷罨法回数４８ＷＲ           TO 冷罨法回数４８.
018370     MOVE 冷罨法料４８ＷＲ             TO 冷罨法料４８.
018380     MOVE 温罨法回数４８ＷＲ           TO 温罨法回数４８.
018390     MOVE 温罨法料４８ＷＲ             TO 温罨法料４８.
018400     MOVE 電療回数４８ＷＲ             TO 電療回数４８.
018410     MOVE 電療料４８ＷＲ               TO 電療料４８.
018420     MOVE 小計４８ＷＲ                 TO 小計４８.
018430     MOVE 多部位込小計４８ＷＲ         TO 多部位込小計４８.
018440     IF ( 長期逓減率４８ＷＲ NOT = ZERO )
018450        COMPUTE 長期逓減率４８ = 長期逓減率４８ＷＲ / 100
018460     END-IF.
018470     MOVE 長期込小計４８ＷＲ           TO 長期込小計４８.
      */ 逓減率 0.7→0.6 /42505
           IF (施術和暦年月ＷＲ >= 42505)
              MOVE "60"                      TO 逓減４８
              MOVE "0.6"                     TO 多部位４８
           END-IF.
018480*    ****************
018490*    * ４部位／10割 *
018500*    ****************
018510     MOVE 逓減開始月４０ＷＲ           TO 逓減開始月４０.
018520     MOVE 逓減開始日４０ＷＲ           TO 逓減開始日４０.
018530     MOVE 後療単価４０ＷＲ             TO 後療単価４０.
018540     MOVE 後療回数４０ＷＲ             TO 後療回数４０.
018550     MOVE 後療料４０ＷＲ               TO 後療料４０.
018560     MOVE 冷罨法回数４０ＷＲ           TO 冷罨法回数４０.
018570     MOVE 冷罨法料４０ＷＲ             TO 冷罨法料４０.
018580     MOVE 温罨法回数４０ＷＲ           TO 温罨法回数４０.
018590     MOVE 温罨法料４０ＷＲ             TO 温罨法料４０.
018600     MOVE 電療回数４０ＷＲ             TO 電療回数４０.
018610     MOVE 電療料４０ＷＲ               TO 電療料４０.
018620     MOVE 小計４０ＷＲ                 TO 小計４０.
018630     IF ( 長期逓減率４０ＷＲ NOT = ZERO )
018640        COMPUTE 長期逓減率４０ = 長期逓減率４０ＷＲ / 100
018650     END-IF.
018660     MOVE 長期込小計４０ＷＲ           TO 長期込小計４０.
018670*
018680*↓***********************************************************************
018690* ５部位／2.5割の印字は必要ない。
018700*------------------------------------------------------------------------*
018710*    *****************
018720*    * ５部位／2.5割 *
018730*    *****************
018740*     MOVE 後療単価５２ＷＲ             TO 後療単価５２.
018750*     MOVE 後療回数５２ＷＲ             TO 後療回数５２.
018760*     MOVE 後療料５２ＷＲ               TO 後療料５２.
018770*     MOVE 冷罨法回数５２ＷＲ           TO 冷罨法回数５２.
018780*     MOVE 冷罨法料５２ＷＲ             TO 冷罨法料５２.
018790*     MOVE 温罨法回数５２ＷＲ           TO 温罨法回数５２.
018800*     MOVE 温罨法料５２ＷＲ             TO 温罨法料５２.
018810*     MOVE 電療回数５２ＷＲ             TO 電療回数５２.
018820*     MOVE 電療料５２ＷＲ               TO 電療料５２.
018830*     MOVE 小計５２ＷＲ                 TO 小計５２.
018840*     MOVE 多部位込小計５２ＷＲ         TO 多部位込小計５２.
018850*     IF ( 長期逓減率５２ＷＲ NOT = ZERO )
018860*        COMPUTE 長期逓減率５２ = 長期逓減率５２ＷＲ / 100
018870*     END-IF.
018880*     MOVE 長期込小計５２ＷＲ           TO 長期込小計５２.
018890*↑***********************************************************************
018900*
018910*    ****************
018920*    * ５部位／５割 *
018930*    ****************
018940*     MOVE 逓減開始月５５ＷＲ           TO 逓減開始月５５.
018950*     MOVE 逓減開始日５５ＷＲ           TO 逓減開始日５５.
018960*     MOVE 後療単価５５ＷＲ             TO 後療単価５５.
018970*     MOVE 後療回数５５ＷＲ             TO 後療回数５５.
018980*     MOVE 後療料５５ＷＲ               TO 後療料５５.
018990*     MOVE 冷罨法回数５５ＷＲ           TO 冷罨法回数５５.
019000*     MOVE 冷罨法料５５ＷＲ             TO 冷罨法料５５.
019010*     MOVE 温罨法回数５５ＷＲ           TO 温罨法回数５５.
019020*     MOVE 温罨法料５５ＷＲ             TO 温罨法料５５.
019030*     MOVE 電療回数５５ＷＲ             TO 電療回数５５.
019040*     MOVE 電療料５５ＷＲ               TO 電療料５５.
019050*     MOVE 小計５５ＷＲ                 TO 小計５５.
019060*     MOVE 多部位込小計５５ＷＲ         TO 多部位込小計５５.
019070*     IF ( 長期逓減率５５ＷＲ NOT = ZERO )
019080*        COMPUTE 長期逓減率５５ = 長期逓減率５５ＷＲ / 100
019090*     END-IF.
019100*     MOVE 長期込小計５５ＷＲ           TO 長期込小計５５.
019110*    ****************
019120*    * ５部位／８割 *
019130*    ****************
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
019310*    ****************
019320*    * ５部位／10割 *
019330*    ****************
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
019510*
019520     MOVE 適用１Ｗ                       TO 適用１.
019530     MOVE 適用２Ｗ                       TO 適用２.
019540*     MOVE 適用３Ｗ                       TO 適用３.
019550*     MOVE 適用４Ｗ                       TO 適用４.
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
019592*------------------------------------------------------------------------*
019617**********************
019618* 施術所データセット *
019620**********************
           MOVE 都道府県ＪＩＳＷ       TO 都道府県番号.
019630     MOVE 柔整師番号Ｗ           TO 柔整師番号.
019640*     MOVE 定額制受理番号Ｗ       TO 定額制受理番号.
019650     MOVE 施術所郵便番号１Ｗ     TO 施術所郵便番号１.
019660     MOVE 施術所郵便番号２Ｗ     TO 施術所郵便番号２.
           MOVE 施術所住所１Ｗ         TO 施術所住所１
           MOVE 施術所住所２Ｗ         TO 施術所住所２
019680*     MOVE 施術所住所１Ｗ         TO 施術所住所１.
019690*     MOVE 施術所住所２Ｗ         TO 施術所住所２.
019700     MOVE 接骨師会会員番号Ｗ     TO 接骨師会会員番号.
019710     MOVE 代表者カナＷ           TO 代表者カナ.
019720     MOVE 代表者名Ｗ             TO 代表者名.
019730     MOVE 接骨師会会員番号Ｗ     TO 接骨師会会員番号.
019740     MOVE 施術所電話番号Ｗ       TO 施術所電話番号.
019750*
019760     MOVE 接骨院名Ｗ             TO 接骨院名.
019770*
019780* / 柔整師・患者委任日 /
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
019790     MOVE 柔整師年Ｗ             TO 受理年.
019800     MOVE 柔整師月Ｗ             TO 受理月.
019810     MOVE 柔整師日Ｗ             TO 受理日.
019820* ( 委任年月日 印刷するか )
019830     IF ( 連入－委任印刷  = ZERO )
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
019840         MOVE 患者委任年Ｗ       TO 委任年
019850         MOVE 患者委任月Ｗ       TO 委任月
019860         MOVE 患者委任日Ｗ       TO 委任日
019870     END-IF.
019880*
019890*
019900*     MOVE 銀行名支店名Ｗ         TO 銀行名支店名.
019910*     MOVE 預金種別コメントＷ     TO 預金種別.
019920*     MOVE 口座番号Ｗ             TO 口座番号.
019930*     MOVE 口座名義人カナＷ       TO 口座名義人カナ.
019940*     MOVE 口座名義人とカナＷ     TO 口座名義人.
019950*     MOVE コメント１Ｗ           TO コメント１.
019960*     MOVE コメント２Ｗ           TO コメント２.
019970*     MOVE コメント３Ｗ           TO コメント３.
019980*
019990************************
020000* レセプト並び順セット *
020010************************
020020     MOVE 順番固定Ｗ          TO 順番固定.
020030     MOVE 順番Ｗ              TO 順番.
020040     MOVE 患者番号ＷＲ        TO 患者番号.
020050     MOVE 枝番ＷＲ            TO 枝番.
020060*
020070*
020080* 特別コメント
020090*     MOVE 特別コメントＷ      TO 特別コメント.
020100*
020110*-------------------------------------------------------------------------*
020120*--- ※ レセ摘要再セットは、この印刷セットSECTION の最後にやること！ -----*
020130     PERFORM レセ摘要再セット.
020140*-------------------------------------------------------------------------*
020141*
020142*-------------------------------------------------------------------------*
020143*--- ※ 地域特有処理は、この印刷セットSECTION の最後にやること！   　-----*
020144     PERFORM 地域特有処理.
020145*-------------------------------------------------------------------------*
020146*
020150*
020160**********     PERFORM テスト印字処理.
020170*
020180*================================================================*
020190 項目初期化 SECTION.
020200*
020210     INITIALIZE 施術所情報Ｗ.
020220     INITIALIZE 受診者情報Ｗ.
020230     INITIALIZE 負傷情報Ｗ.
020240     INITIALIZE 備考情報Ｗ.
020250*     INITIALIZE ＯＣＲコードＷ.
020260     INITIALIZE 料金１ＷＲ.
020270     INITIALIZE 料金２ＷＲ.
020280     INITIALIZE 料金３ＷＲ.
020300     INITIALIZE YCH6125P.
020290     MOVE SPACE TO YCH6125P.
020310*================================================================*
020320 料金情報取得 SECTION.
020330*
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
020340********************
020350* 料金データセット *
020360********************
020370*    ****************************************************************
020380*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
020390*    ****************************************************************
020400     MOVE レセ－初検料                 TO 初検料ＷＲ.
020410     IF ( レセ－時間外 = 1 )
020420         MOVE NC"○"                   TO 時間外チェックＷ
020430     END-IF.
020440     IF ( レセ－休日 = 1 )
020450         MOVE NC"○"                   TO 休日チェックＷ
020460     END-IF.
020470     IF ( レセ－深夜 = 1 )
020480         MOVE NC"○"                   TO 深夜チェックＷ
020490     END-IF.
020500*
020510     MOVE レセ－初検加算料             TO  初検加算料ＷＲ.
020520     MOVE レセ－再検料                 TO  再検料ＷＲ.
           MOVE レセ－初検時相談料           TO  初検時相談料ＷＲ.
020530     MOVE レセ－往療距離               TO  往療距離ＷＲ.
020540     MOVE レセ－往療回数               TO  往療回数ＷＲ.
020550     MOVE レセ－往療料                 TO  往療料ＷＲ.
020560     MOVE レセ－往療加算料             TO  往療加算料ＷＲ.
020570*
020580     IF ( レセ－夜間 = 1 )
020590         MOVE NC"○"                   TO 夜間チェックＷ
020600     END-IF.
020610     IF ( レセ－暴風雨雪 = 1 )
020620         MOVE NC"○"                   TO 暴風雨雪チェックＷ
020630     END-IF.
020640*
020650     MOVE レセ－金属副子加算料         TO  金属副子加算料ＷＲ.
020660*
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
020770     MOVE レセ－施術情報提供料         TO  施術情報提供料ＷＲ.
020780* 小計
020790     MOVE レセ－小計                   TO 小計Ｗ.
020800********************
020810* 初回処置料セット *
020820********************
020830     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
020840             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
020850         MOVE レセ－初回処置料(部位ＣＮＴ) TO 初回処置料ＷＲ(部位ＣＮＴ)
020860         IF ( レセ－初回処置料(部位ＣＮＴ) NOT = ZERO )
020870            EVALUATE 負－負傷種別(部位ＣＮＴ)
020880* 捻挫・打撲・挫傷
020890            WHEN 1
020900            WHEN 2
020910            WHEN 3
020920                MOVE NC"○"       TO 施療料チェックＷ
020930* 脱臼・骨折・骨折拘縮
020940            WHEN 4
020950            WHEN 5
020960            WHEN 7
020970                MOVE NC"○"       TO 整復料チェックＷ
020980* 不全骨折・不全骨折拘縮
020990            WHEN 6
021000            WHEN 8
021010                MOVE NC"○"       TO 固定料チェックＷ
021020            END-EVALUATE
021030         END-IF
021040     END-PERFORM.
021050     MOVE レセ－初回処置料合計         TO 初回処置料合計Ｗ.
021060********************
021070* 逓減毎料金セット *
021080********************
021090*    **********
021100*    * １部位 *
021110*    **********
021120     MOVE レセ－後療単価１             TO 後療単価１ＷＲ.
021130     MOVE レセ－後療回数１             TO 後療回数１ＷＲ.
021140     MOVE レセ－後療料１               TO 後療料１ＷＲ.
021150     MOVE レセ－冷罨法回数１           TO 冷罨法回数１ＷＲ.
021160     MOVE レセ－冷罨法料１             TO 冷罨法料１ＷＲ.
021170     MOVE レセ－温罨法回数１           TO 温罨法回数１ＷＲ.
021180     MOVE レセ－温罨法料１             TO 温罨法料１ＷＲ.
021190     MOVE レセ－電療回数１             TO 電療回数１ＷＲ.
021200     MOVE レセ－電療料１               TO 電療料１ＷＲ.
021210     MOVE レセ－小計１                 TO 小計１ＷＲ.
021220     MOVE レセ－長期逓減率１           TO 長期逓減率１ＷＲ.
021230     MOVE レセ－長期込小計１           TO 長期込小計１ＷＲ.
021240*    **********
021250*    * ２部位 *
021260*    **********
021270     MOVE レセ－後療単価２             TO 後療単価２ＷＲ.
021280     MOVE レセ－後療回数２             TO 後療回数２ＷＲ.
021290     MOVE レセ－後療料２               TO 後療料２ＷＲ.
021300     MOVE レセ－冷罨法回数２           TO 冷罨法回数２ＷＲ.
021310     MOVE レセ－冷罨法料２             TO 冷罨法料２ＷＲ.
021320     MOVE レセ－温罨法回数２           TO 温罨法回数２ＷＲ.
021330     MOVE レセ－温罨法料２             TO 温罨法料２ＷＲ.
021340     MOVE レセ－電療回数２             TO 電療回数２ＷＲ.
021350     MOVE レセ－電療料２               TO 電療料２ＷＲ.
021360     MOVE レセ－小計２                 TO 小計２ＷＲ.
021370     MOVE レセ－長期逓減率２           TO 長期逓減率２ＷＲ.
021380     MOVE レセ－長期込小計２           TO 長期込小計２ＷＲ.
021390*    ****************
021400*    * ３部位／８割 *
021410*    ****************
021420     MOVE レセ－後療単価３８             TO 後療単価３８ＷＲ.
021430     MOVE レセ－後療回数３８             TO 後療回数３８ＷＲ.
021440     MOVE レセ－後療料３８               TO 後療料３８ＷＲ.
021450     MOVE レセ－冷罨法回数３８           TO 冷罨法回数３８ＷＲ.
021460     MOVE レセ－冷罨法料３８             TO 冷罨法料３８ＷＲ.
021470     MOVE レセ－温罨法回数３８           TO 温罨法回数３８ＷＲ.
021480     MOVE レセ－温罨法料３８             TO 温罨法料３８ＷＲ.
021490     MOVE レセ－電療回数３８             TO 電療回数３８ＷＲ.
021500     MOVE レセ－電療料３８               TO 電療料３８ＷＲ.
021510     MOVE レセ－小計３８                 TO 小計３８ＷＲ.
021520     MOVE レセ－多部位込小計３８         TO 多部位込小計３８ＷＲ.
021530     MOVE レセ－長期逓減率３８           TO 長期逓減率３８ＷＲ.
021540     MOVE レセ－長期込小計３８           TO 長期込小計３８ＷＲ.
021550*    ****************
021560*    * ３部位／10割 *
021570*    ****************
021580     MOVE レセ－逓減開始月３０           TO 逓減開始月３０ＷＲ.
021590     MOVE レセ－逓減開始日３０           TO 逓減開始日３０ＷＲ.
021600     MOVE レセ－後療単価３０             TO 後療単価３０ＷＲ.
021610     MOVE レセ－後療回数３０             TO 後療回数３０ＷＲ.
021620     MOVE レセ－後療料３０               TO 後療料３０ＷＲ.
021630     MOVE レセ－冷罨法回数３０           TO 冷罨法回数３０ＷＲ.
021640     MOVE レセ－冷罨法料３０             TO 冷罨法料３０ＷＲ.
021650     MOVE レセ－温罨法回数３０           TO 温罨法回数３０ＷＲ.
021660     MOVE レセ－温罨法料３０             TO 温罨法料３０ＷＲ.
021670     MOVE レセ－電療回数３０             TO 電療回数３０ＷＲ.
021680     MOVE レセ－電療料３０               TO 電療料３０ＷＲ.
021690     MOVE レセ－小計３０                 TO 小計３０ＷＲ.
021700     MOVE レセ－長期逓減率３０           TO 長期逓減率３０ＷＲ.
021710     MOVE レセ－長期込小計３０           TO 長期込小計３０ＷＲ.
021720*    ****************
021730*    * ４部位／５割 *
021740*    ****************
021750     MOVE レセ－後療単価４５             TO 後療単価４５ＷＲ.
021760     MOVE レセ－後療回数４５             TO 後療回数４５ＷＲ.
021770     MOVE レセ－後療料４５               TO 後療料４５ＷＲ.
021780     MOVE レセ－冷罨法回数４５           TO 冷罨法回数４５ＷＲ.
021790     MOVE レセ－冷罨法料４５             TO 冷罨法料４５ＷＲ.
021800     MOVE レセ－温罨法回数４５           TO 温罨法回数４５ＷＲ.
021810     MOVE レセ－温罨法料４５             TO 温罨法料４５ＷＲ.
021820     MOVE レセ－電療回数４５             TO 電療回数４５ＷＲ.
021830     MOVE レセ－電療料４５               TO 電療料４５ＷＲ.
021840     MOVE レセ－小計４５                 TO 小計４５ＷＲ.
021850     MOVE レセ－多部位込小計４５         TO 多部位込小計４５ＷＲ.
021860     MOVE レセ－長期逓減率４５           TO 長期逓減率４５ＷＲ.
021870     MOVE レセ－長期込小計４５           TO 長期込小計４５ＷＲ.
021880*    ****************
021890*    * ４部位／８割 *
021900*    ****************
021910     MOVE レセ－逓減開始月４８           TO 逓減開始月４８ＷＲ.
021920     MOVE レセ－逓減開始日４８           TO 逓減開始日４８ＷＲ.
021930     MOVE レセ－後療単価４８             TO 後療単価４８ＷＲ.
021940     MOVE レセ－後療回数４８             TO 後療回数４８ＷＲ.
021950     MOVE レセ－後療料４８               TO 後療料４８ＷＲ.
021960     MOVE レセ－冷罨法回数４８           TO 冷罨法回数４８ＷＲ.
021970     MOVE レセ－冷罨法料４８             TO 冷罨法料４８ＷＲ.
021980     MOVE レセ－温罨法回数４８           TO 温罨法回数４８ＷＲ.
021990     MOVE レセ－温罨法料４８             TO 温罨法料４８ＷＲ.
022000     MOVE レセ－電療回数４８             TO 電療回数４８ＷＲ.
022010     MOVE レセ－電療料４８               TO 電療料４８ＷＲ.
022020     MOVE レセ－小計４８                 TO 小計４８ＷＲ.
022030     MOVE レセ－多部位込小計４８         TO 多部位込小計４８ＷＲ.
022040     MOVE レセ－長期逓減率４８           TO 長期逓減率４８ＷＲ.
022050     MOVE レセ－長期込小計４８           TO 長期込小計４８ＷＲ.
022060*    ****************
022070*    * ４部位／10割 *
022080*    ****************
022090     MOVE レセ－逓減開始月４０           TO 逓減開始月４０ＷＲ.
022100     MOVE レセ－逓減開始日４０           TO 逓減開始日４０ＷＲ.
022110     MOVE レセ－後療単価４０             TO 後療単価４０ＷＲ.
022120     MOVE レセ－後療回数４０             TO 後療回数４０ＷＲ.
022130     MOVE レセ－後療料４０               TO 後療料４０ＷＲ.
022140     MOVE レセ－冷罨法回数４０           TO 冷罨法回数４０ＷＲ.
022150     MOVE レセ－冷罨法料４０             TO 冷罨法料４０ＷＲ.
022160     MOVE レセ－温罨法回数４０           TO 温罨法回数４０ＷＲ.
022170     MOVE レセ－温罨法料４０             TO 温罨法料４０ＷＲ.
022180     MOVE レセ－電療回数４０             TO 電療回数４０ＷＲ.
022190     MOVE レセ－電療料４０               TO 電療料４０ＷＲ.
022200     MOVE レセ－小計４０                 TO 小計４０ＷＲ.
022210     MOVE レセ－長期逓減率４０           TO 長期逓減率４０ＷＲ.
022220     MOVE レセ－長期込小計４０           TO 長期込小計４０ＷＲ.
022230*    *****************
022240*    * ５部位／2.5割 *
022250*    *****************
022260     MOVE レセ－後療単価５２             TO 後療単価５２ＷＲ.
022270     MOVE レセ－後療回数５２             TO 後療回数５２ＷＲ.
022280     MOVE レセ－後療料５２               TO 後療料５２ＷＲ.
022290     MOVE レセ－冷罨法回数５２           TO 冷罨法回数５２ＷＲ.
022300     MOVE レセ－冷罨法料５２             TO 冷罨法料５２ＷＲ.
022310     MOVE レセ－温罨法回数５２           TO 温罨法回数５２ＷＲ.
022320     MOVE レセ－温罨法料５２             TO 温罨法料５２ＷＲ.
022330     MOVE レセ－電療回数５２             TO 電療回数５２ＷＲ.
022340     MOVE レセ－電療料５２               TO 電療料５２ＷＲ.
022350     MOVE レセ－小計５２                 TO 小計５２ＷＲ.
022360     MOVE レセ－多部位込小計５２         TO 多部位込小計５２ＷＲ.
022370     MOVE レセ－長期逓減率５２           TO 長期逓減率５２ＷＲ.
022380     MOVE レセ－長期込小計５２           TO 長期込小計５２ＷＲ.
022390*    ****************
022400*    * ５部位／５割 *
022410*    ****************
022420     MOVE レセ－逓減開始月５５           TO 逓減開始月５５ＷＲ.
022430     MOVE レセ－逓減開始日５５           TO 逓減開始日５５ＷＲ.
022440     MOVE レセ－後療単価５５             TO 後療単価５５ＷＲ.
022450     MOVE レセ－後療回数５５             TO 後療回数５５ＷＲ.
022460     MOVE レセ－後療料５５               TO 後療料５５ＷＲ.
022470     MOVE レセ－冷罨法回数５５           TO 冷罨法回数５５ＷＲ.
022480     MOVE レセ－冷罨法料５５             TO 冷罨法料５５ＷＲ.
022490     MOVE レセ－温罨法回数５５           TO 温罨法回数５５ＷＲ.
022500     MOVE レセ－温罨法料５５             TO 温罨法料５５ＷＲ.
022510     MOVE レセ－電療回数５５             TO 電療回数５５ＷＲ.
022520     MOVE レセ－電療料５５               TO 電療料５５ＷＲ.
022530     MOVE レセ－小計５５                 TO 小計５５ＷＲ.
022540     MOVE レセ－多部位込小計５５         TO 多部位込小計５５ＷＲ.
022550     MOVE レセ－長期逓減率５５           TO 長期逓減率５５ＷＲ.
022560     MOVE レセ－長期込小計５５           TO 長期込小計５５ＷＲ.
022570*    ****************
022580*    * ５部位／８割 *
022590*    ****************
022600     MOVE レセ－逓減開始月５８           TO 逓減開始月５８ＷＲ.
022610     MOVE レセ－逓減開始日５８           TO 逓減開始日５８ＷＲ.
022620     MOVE レセ－後療単価５８             TO 後療単価５８ＷＲ.
022630     MOVE レセ－後療回数５８             TO 後療回数５８ＷＲ.
022640     MOVE レセ－後療料５８               TO 後療料５８ＷＲ.
022650     MOVE レセ－冷罨法回数５８           TO 冷罨法回数５８ＷＲ.
022660     MOVE レセ－冷罨法料５８             TO 冷罨法料５８ＷＲ.
022670     MOVE レセ－温罨法回数５８           TO 温罨法回数５８ＷＲ.
022680     MOVE レセ－温罨法料５８             TO 温罨法料５８ＷＲ.
022690     MOVE レセ－電療回数５８             TO 電療回数５８ＷＲ.
022700     MOVE レセ－電療料５８               TO 電療料５８ＷＲ.
022710     MOVE レセ－小計５８                 TO 小計５８ＷＲ.
022720     MOVE レセ－多部位込小計５８         TO 多部位込小計５８ＷＲ.
022730     MOVE レセ－長期逓減率５８           TO 長期逓減率５８ＷＲ.
022740     MOVE レセ－長期込小計５８           TO 長期込小計５８ＷＲ.
022750*    ****************
022760*    * ５部位／10割 *
022770*    ****************
022780     MOVE レセ－逓減開始月５０           TO 逓減開始月５０ＷＲ.
022790     MOVE レセ－逓減開始日５０           TO 逓減開始日５０ＷＲ.
022800     MOVE レセ－後療単価５０             TO 後療単価５０ＷＲ.
022810     MOVE レセ－後療回数５０             TO 後療回数５０ＷＲ.
022820     MOVE レセ－後療料５０               TO 後療料５０ＷＲ.
022830     MOVE レセ－冷罨法回数５０           TO 冷罨法回数５０ＷＲ.
022840     MOVE レセ－冷罨法料５０             TO 冷罨法料５０ＷＲ.
022850     MOVE レセ－温罨法回数５０           TO 温罨法回数５０ＷＲ.
022860     MOVE レセ－温罨法料５０             TO 温罨法料５０ＷＲ.
022870     MOVE レセ－電療回数５０             TO 電療回数５０ＷＲ.
022880     MOVE レセ－電療料５０               TO 電療料５０ＷＲ.
022890     MOVE レセ－小計５０                 TO 小計５０ＷＲ.
022900     MOVE レセ－長期逓減率５０           TO 長期逓減率５０ＷＲ.
022910     MOVE レセ－長期込小計５０           TO 長期込小計５０ＷＲ.
022920*
022930*================================================================*
022940 施術所情報取得 SECTION.
022950*
022960**************************************************
022970* 本院データを使用し、以下の情報を取得           *
022980* ● 柔整師番号.. 柔整師番号Ｗに格納             *
022990* ● 会員番号 ... 接骨師会会員番号Ｗに格納       *
023000* ● 代表者名 ... 代表者名Ｗに格納               *
023010* ● 住所1,2   ...施術所住所1,2Ｗに格納          *
023020* ● 電話番号 ... 施術所電話番号Ｗに格納         *
023030**************************************************
023040     MOVE ZERO  TO 施情－施術所番号.
023050     READ 施術所情報マスタ
023060     INVALID KEY
023070         CONTINUE
023080     NOT INVALID KEY
023090*
               MOVE 施情－都道府県ＪＩＳ     TO 都道府県ＪＩＳＷ
023130         MOVE 施情－新柔整師番号       TO 柔整師番号Ｗ
023150*
023160*         MOVE 施情－接骨師会会員番号  TO ＯＣＲ接骨師会会員番号Ｗ
023170*
023180         STRING "中央-"                DELIMITED BY SIZE
023190                施情－接骨師会会員番号 DELIMITED BY SPACE
023200           INTO 接骨師会会員番号Ｗ
023210         END-STRING
023220*
023230         MOVE 施情－郵便番号１        TO 施術所郵便番号１Ｗ
023240         MOVE 施情－郵便番号２        TO 施術所郵便番号２Ｗ
023250         MOVE 施情－代表者カナ        TO 代表者カナＷ
023260         MOVE 施情－代表者名          TO 代表者名Ｗ
023270*
023280         MOVE 施情－接骨院名          TO 接骨院名Ｗ
023290         MOVE 施情－住所１            TO 施術所住所１Ｗ
023300         MOVE 施情－住所２            TO 施術所住所２Ｗ
023310*         STRING 施情－住所１  DELIMITED BY SPACE
023320*                施情－住所２  DELIMITED BY SPACE
023330*           INTO 施術所住所Ｗ
023340*         END-STRING
023350*
023360         MOVE 施情－電話番号          TO 施術所電話番号Ｗ
023370*
023380     END-READ.
023390*================================================================*
023400 受診者情報取得 SECTION.
023410*
023420**************************************************
023430* 連結データから受診者情報Ｆより以下の情報を取得 *
023440* ● 施術年 ..... 施術年Ｗに格納                 *
023450* ● 施術月 ..... 施術月Ｗに格納                 *
023460* ● 患者番号.... 患者番号Ｗに格納※ＦＤ連番用   *
023470* ● 記号 ....... 記号Ｗに格納                   *
023480* ● 番号 ....... 番号Ｗに格納                   *
023490* ● 保険者番号 . 保険者番号Ｗに格納             *
023500* ● 保険種別 ... 保険種別Ｗに格納               *
023510* ● 被保険者カナ.被保険者カナＷに格納           *
023520* ● 被保険者氏名.被保険者氏名Ｗに格納           *
023530* ● 住所１ ......被保険者住所１Ｗに格納         *
023540* ● 住所２ ......被保険者住所２Ｗに格納         *
023550* ● 患者カナ ....患者カナＷに格納               *
023560* ● 患者氏名 ....患者氏名Ｗに格納               *
023570* ● 患者性別 ....区分によりチェックに"○"を格納 *
023580* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
023590* ● 患者年 ......患者年Ｗに格納                 *
023600* ● 患者月 ......患者月Ｗに格納                 *
023610* ● 患者日 ......患者日Ｗに格納                 *
023620* ● 続柄 ........名称マスタより続柄Ｗに取得     *
023630**************************************************
023640     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
023650     MOVE 施術年ＷＲ         TO 受－施術年.
023660     MOVE 施術月ＷＲ         TO 受－施術月.
023670     MOVE 患者コードＷＲ     TO 受－患者コード.
023680     READ 受診者情報Ｆ
023690     INVALID KEY
023700         CONTINUE
023710*            /* ありえない */
023720     NOT INVALID KEY
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
023730         MOVE 受－施術年       TO 施術年Ｗ
023740         MOVE 受－施術月       TO 施術月Ｗ
023750         MOVE 受－患者番号     TO 患者番号Ｗ
023760*         MOVE 受－記号         TO 記号Ｗ
023770*         MOVE 受－番号         TO 番号Ｗ
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
023860         MOVE 受－保険者番号   TO 保険者番号Ｗ
023870         MOVE 受－保険種別     TO 保険種別Ｗ
023880         MOVE 受－被保険者カナ TO 被保険者カナＷ
023890         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
023900         MOVE 受－郵便番号１   TO 郵便番号１Ｗ
023910         MOVE 受－郵便番号２   TO 郵便番号２Ｗ
023920         MOVE 受－住所１       TO 被保険者住所１Ｗ
023930         MOVE 受－住所２       TO 被保険者住所２Ｗ
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
023940         MOVE 受－患者カナ     TO 患者カナＷ
023950         MOVE 受－患者氏名     TO 患者氏名Ｗ
023960         MOVE 受－費用負担者番号助成 TO 市町村番号Ｗ
023970*
023980         EVALUATE 受－患者性別
023990         WHEN 1
024000             MOVE NC"男"  TO 性別Ｗ
024010             MOVE NC"○"  TO 男チェックＷ
024020         WHEN 2
024030             MOVE NC"女"  TO 性別Ｗ
024040             MOVE NC"○"  TO 女チェックＷ
024050         END-EVALUATE
024060         EVALUATE 受－患者和暦
024070         WHEN 1
024080             MOVE NC"明治"  TO 元号Ｗ
024090             MOVE NC"○"    TO 明治チェックＷ
024100         WHEN 2
024110             MOVE NC"大正"  TO 元号Ｗ
024120             MOVE NC"○"    TO 大正チェックＷ
024130         WHEN 3
024140             MOVE NC"昭和"  TO 元号Ｗ
024150             MOVE NC"○"    TO 昭和チェックＷ
024160         WHEN 4
024170             MOVE NC"平成"  TO 元号Ｗ
024180             MOVE NC"○"    TO 平成チェックＷ
      */元号修正/20190408
023060         WHEN 5
                   MOVE "5令"   TO 令和ＣＭＷ
023070             MOVE NC"○"  TO 令和チェックＷ
024190         END-EVALUATE
024200*
      */元号修正/↓↓↓20190408
029310         IF 受－患者和暦 > 4
037370             MOVE 受－患者和暦     TO 元－元号区分
037380             READ 元号マスタ
037390             NOT INVALID KEY
037400                 MOVE 元－元号名称 TO 元号Ｗ
037410             END-READ
029330         END-IF
      */元号修正/↑↑↑20190408
024210         MOVE 受－患者年  TO 患者年Ｗ
024220         MOVE 受－患者月  TO 患者月Ｗ
024230         MOVE 受－患者日  TO 患者日Ｗ
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
024240*
024250*         IF ( 本人家族区分ＷＲ = 1 )
024260*             MOVE NC"本人"    TO 続柄Ｗ
024270*         ELSE
024280*             MOVE 05          TO 名－区分コード
024290*             MOVE 受－続柄    TO 名－名称コード
024300*             READ 名称マスタ
024310*             INVALID KEY
024320*                 MOVE SPACE    TO 続柄Ｗ
024330*             NOT INVALID KEY
024340*                 MOVE 名－略称 TO 続柄Ｗ
024350*             END-READ
024360*         END-IF
024370*
024380** 14/10～　特別区分コメント印字
024390*         IF ( 受－施術和暦年月 >= 41410 )
024400*             IF ( 受－公費種別 = ZERO )
024401*                EVALUATE 受－特別区分
024402*                WHEN 1
024403*                   MOVE "70才以上 1割"  TO 特別コメントＷ
024404*                WHEN 2
024405*                   MOVE "70才以上 2割"  TO 特別コメントＷ
024406*                WHEN 3
024407*                   MOVE "70才以上 3割"  TO 特別コメントＷ
024408*                WHEN 6
024409*                   IF 受－施術和暦年月 < 42004
024410*                      MOVE "3才未満"        TO 特別コメントＷ
024411*                   ELSE
024412*                      MOVE "義務教育就学前" TO 特別コメントＷ
024413*                   END-IF
024414*                END-EVALUATE
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
024490*             END-IF
024500*         END-IF
024510*
024520     END-READ.
024530*================================================================*
024540 請求先情報取得 SECTION.
024550*
024560****************************************************
024570* 連結データから保険者マスタより請求先を取得する。 *
024580* ※保－請求先情報区分=1の場合請求先マスタを使用   *
024590* ● 請求先...... 請求先名称Ｗに格納               *
024600****************************************************
024610     MOVE 保険種別ＷＲ   TO 保－保険種別.
024620     MOVE 保険者番号ＷＲ TO 保－保険者番号.
024630     READ 保険者マスタ
024640     INVALID KEY
024650         MOVE SPACE              TO 請求先名称Ｗ
024660     NOT INVALID KEY
024670* 社保、日雇は「社会保険事務所」をつける
024680                 EVALUATE 保険種別ＷＲ 
024690                 WHEN  02
024700                 WHEN  06
                           IF 保－保険者名称(1:16) = "全国健康保険協会"
                              UNSTRING 保－保険者名称 DELIMITED BY "全国健康保険協会" OR "支部"
                                       INTO 支部名１Ｗ 支部名２Ｗ
                              END-UNSTRING
                              STRING "全国健康保険協会" DELIMITED BY SIZE
                                     "　"               DELIMITED BY SIZE
                                     支部名２Ｗ         DELIMITED BY SPACE
                                     "　"               DELIMITED BY SIZE
                                     "支部"             DELIMITED BY SIZE
                                   INTO 請求先名称Ｗ
                              END-STRING
                           ELSE
024710                        IF ( 保－接尾語区分 = 1 )
024720                           MOVE 保－保険者名称    TO 請求先名称Ｗ
024730                        ELSE
024740                           STRING 保－保険者名称    DELIMITED BY SPACE
024750                                  "社会保険事務所"  DELIMITED BY SIZE
024760                                  INTO 請求先名称Ｗ
024770                           END-STRING
024780                        END-IF
                           END-IF
024790** 組合は支部名まで印字
024800                 WHEN  03
024810                     STRING 保－保険者名称  DELIMITED BY SPACE
024820                            "  "            DELIMITED BY SIZE
024830                            保－支部部署名  DELIMITED BY SPACE
024840                           INTO 請求先名称Ｗ
024850                     END-STRING
024860                 WHEN OTHER
024870                     MOVE 保－保険者名称   TO 請求先名称Ｗ
024880                 END-EVALUATE
024890     END-READ.
024900*
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
024910*================================================================*
024920 負傷データ取得 SECTION.
024930*
024940**************************************************
024950* 連結データから負傷データＦより以下の情報を取得 *
024960* ● 負傷名...部位＋負傷種別にて加工して格納     *
024970* ● 負傷年.......負傷年Ｗ                       *
024980* ● 負傷月.......負傷月Ｗ                       *
024990* ● 負傷日.......負傷日Ｗ                       *
025000* ● 開始年.......初検年Ｗ                       *
025010* ● 開始月.......初検月Ｗ                       *
025020* ● 開始日.......初検日Ｗ                       *
025030* ● 終了年.......終了年Ｗ                       *
025040* ● 終了月.......終了月Ｗ                       *
025050* ● 終了日.......終了日Ｗ                       *
025060* ● 実日数.......実日数Ｗ                       *
025070* ● 転帰区分 ....区分によりチェックに"○"を格納 *
025080* ● 金属副子 ....区分によりチェックに"○"を格納 *
025090* ● 経過コード...経過マスタより取得             *
025100**************************************************
025110*     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
025120*     MOVE 施術年ＷＲ         TO 負－施術年.
025130*     MOVE 施術月ＷＲ         TO 負－施術月.
025140*     MOVE 患者コードＷＲ     TO 負－患者コード.
025150*     READ 負傷データＦ
025160*     INVALID KEY
025170*         CONTINUE
025180**            /* ありえない */
025190*     NOT INVALID KEY
025200*         MOVE 負－部位数                   TO 部位数Ｗ
025210         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
025220                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
025230             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別Ｗ(部位ＣＮＴ)
025240             MOVE 負－部位(部位ＣＮＴ)     TO 部位Ｗ(部位ＣＮＴ)
025250             MOVE 負－左右区分(部位ＣＮＴ) TO 左右区分Ｗ(部位ＣＮＴ)
025260             MOVE 負－負傷位置番号(部位ＣＮＴ)
025270                                           TO 負傷位置番号Ｗ(部位ＣＮＴ)
025280*********************************************
025290* 注）全柔...負傷種別＋部位にて加工して格納 *
025300*********************************************
025310* 負傷種別
025320             MOVE SPACE                     TO 負傷名称Ｗ
025330             MOVE 03                        TO 名－区分コード
025340             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
025350             READ 名称マスタ
025360             INVALID KEY
025370                 MOVE SPACE        TO 負傷名称Ｗ
025380             NOT INVALID KEY
025390                 MOVE 名－正式名称 TO 負傷名称Ｗ
025400             END-READ
025410* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
028160*
028170             PERFORM 部位名称埋込処理
025600*
025610             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
025620             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
025630             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
025640             MOVE 負－開始年(部位ＣＮＴ)   TO 初検年Ｗ(部位ＣＮＴ)
025650             MOVE 負－開始月(部位ＣＮＴ)   TO 初検月Ｗ(部位ＣＮＴ)
025660             MOVE 負－開始日(部位ＣＮＴ)   TO 初検日Ｗ(部位ＣＮＴ)
025670             IF ( 負－転帰区分(部位ＣＮＴ) = 9 )
025680                 MOVE 99                   TO 終了年Ｗ(部位ＣＮＴ)
025690                 MOVE 99                   TO 終了月Ｗ(部位ＣＮＴ)
025700                 MOVE 99                   TO 終了日Ｗ(部位ＣＮＴ)
025710             ELSE
025720                 MOVE 負－終了年(部位ＣＮＴ)   TO 終了年Ｗ(部位ＣＮＴ)
025730                 MOVE 負－終了月(部位ＣＮＴ)   TO 終了月Ｗ(部位ＣＮＴ)
025740                 MOVE 負－終了日(部位ＣＮＴ)   TO 終了日Ｗ(部位ＣＮＴ)
025750             END-IF
025760* 経過略称取得
025770             MOVE 01                         TO 経－区分コード
025780             MOVE 負－経過コード(部位ＣＮＴ) TO 経－経過コード
025790             READ 経過マスタ
025800             INVALID KEY
025810                 MOVE ZERO            TO 部位ＣＮＴＷ(部位ＣＮＴ)
025820                 MOVE SPACE           TO 部位区切Ｗ(部位ＣＮＴ)
025830                 MOVE SPACE           TO 経過略称Ｗ(部位ＣＮＴ)
025840             NOT INVALID KEY
025850*
025860                 EVALUATE 部位ＣＮＴ
025870                 WHEN 1
025880                     MOVE NC"①" TO 経過部位Ｗ
025890                 WHEN 2
025900                     MOVE NC"②" TO 経過部位Ｗ
025910                 WHEN 3
025920                     MOVE NC"③" TO 経過部位Ｗ
025930                 WHEN 4
025940                     MOVE NC"④" TO 経過部位Ｗ
025950                 WHEN 5
025960                     MOVE NC"⑤" TO 経過部位Ｗ
025970                 END-EVALUATE
025980                 STRING  経過部位Ｗ     DELIMITED BY SPACE
025990                         経－経過略称   DELIMITED BY SPACE
026000                        INTO 印刷経過略称Ｗ(部位ＣＮＴ)
026010                 END-STRING
026020*
026030             END-READ
026040*
026050             MOVE 負－転帰区分(部位ＣＮＴ) TO 転帰区分Ｗ(部位ＣＮＴ)
026060             EVALUATE 負－転帰区分(部位ＣＮＴ)
026070             WHEN 1
026080             WHEN 2
026090                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
026100             WHEN 3
026110                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
026120             WHEN 4
026130                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
026140             END-EVALUATE
028260*
      */実日数はレセ－部位実日数を転記する/160816
031230             MOVE レセ－部位実日数(部位ＣＮＴ) TO 実日数Ｗ(部位ＣＮＴ)
026150         END-PERFORM.
026160* 新規/継続 チェック
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
026220* 枝番判定用
026230         MOVE 負－開始診療日手動区分 TO  開始診療日手動区分Ｗ.
026240* 負傷原因印刷区分
026250         MOVE 負－レセ負傷原因印刷区分 TO レセ負傷原因印刷区分Ｗ.
028370* 長期理由印刷区分
027880         MOVE 負－レセ長期理由印刷区分 TO 長期理由印刷区分Ｆ.
026260*
026270*     END-READ.
026280*================================================================*
026290*================================================================*
026300 施術記録取得 SECTION.
026310*
026320************************************************************
026330* 作１データから負傷データＦより以下の情報を取得           *
026340* ● 初検加算 .....区分によりチェックに"○"を格納...複数可 *
026350* ● 往療加算 .....区分によりチェックに"○"を格納...複数可 *
026360************************************************************
026370     MOVE  SPACE  TO  初日再検フラグ.
026380     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 部位数Ｗ
026390         IF ( 施術年Ｗ = 初検年Ｗ(部位ＣＮＴ) ) AND
026400            ( 施術月Ｗ = 初検月Ｗ(部位ＣＮＴ) )
026410             MOVE 患者番号ＷＲ          TO 施記－患者番号
026420             MOVE 枝番ＷＲ              TO 施記－枝番
026430             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
026440             MOVE 初検年Ｗ(部位ＣＮＴ)  TO 開始年Ｗ(部位ＣＮＴ) 施記－施術年
026450             MOVE 初検月Ｗ(部位ＣＮＴ)  TO 開始月Ｗ(部位ＣＮＴ) 施記－施術月
026460             MOVE 初検日Ｗ(部位ＣＮＴ)  TO 開始日Ｗ(部位ＣＮＴ) 施記－施術日
026470         ELSE
026480             MOVE 患者番号ＷＲ          TO 施記－患者番号
026490             MOVE 枝番ＷＲ              TO 施記－枝番
026500             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
026510             MOVE 施術年ＷＲ            TO 施記－施術年
026520             MOVE 施術月ＷＲ            TO 施記－施術月
026530             MOVE ZERO                  TO 施記－施術日
026540         END-IF
026550         START 施術記録Ｆ   KEY IS >= 施記－患者コード
026560                                      施記－施術和暦年月日
026570         END-START
026580         IF ( 状態キー = "00" )
      */実日数はレセ－部位実日数を転記する/160816
026590*             MOVE ZERO  TO 実日数Ｗ(部位ＣＮＴ)
026600             MOVE ZERO  TO 終了年ＷＴ
026610             MOVE ZERO  TO 終了月ＷＴ
026620             MOVE ZERO  TO 終了日ＷＴ
026630             MOVE SPACE TO 終了フラグ２
026640             PERFORM 施術記録Ｆ読込
026650             IF  ( 終了フラグ２      = SPACE   ) AND
026660                 ( 施記－患者コード  = 患者コードＷＲ ) AND
026670                 ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
026680                 ( 施記－施術年      = 施術年ＷＲ     ) AND
026690                 ( 施記－施術月      = 施術月ＷＲ     ) 
026700*
026710*        *****************************************************************
026720*        * 開始年月日 ( その部位が当月初検でないか、
026730*                       当月初検でも枝番がある時は、最初の施術日を開始日)*
026740*        *****************************************************************
026750                 IF ( 施術年Ｗ NOT = 初検年Ｗ(部位ＣＮＴ) ) OR
026760                    ( 施術月Ｗ NOT = 初検月Ｗ(部位ＣＮＴ) ) OR
026770                    ( 開始診療日手動区分Ｗ = 1 )
026780                     MOVE 施記－施術年   TO 開始年Ｗ(部位ＣＮＴ)
026790                     MOVE 施記－施術月   TO 開始月Ｗ(部位ＣＮＴ)
026800                     MOVE 施記－施術日   TO 開始日Ｗ(部位ＣＮＴ)
026810                 END-IF
026820             END-IF
026830             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
026840                           ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
026850                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
026860                           ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
026870                           ( 施記－施術月     NOT = 施術月ＷＲ       ) OR
026880                           ( 施記－施術日         > 終了日Ｗ(部位ＣＮＴ))
026890*               **********
026900*               * 実日数 *
026910*               **********
      */実日数はレセ－部位実日数を転記する/160816
026920*                COMPUTE 実日数Ｗ(部位ＣＮＴ) = 実日数Ｗ(部位ＣＮＴ) + 1
026930                MOVE 施記－施術年               TO 終了年ＷＴ
026940                MOVE 施記－施術月               TO 終了月ＷＴ
026950                MOVE 施記－施術日               TO 終了日ＷＴ
026960*
026970                PERFORM 施術記録Ｆ読込
026980            END-PERFORM
026990        END-IF
027000*       **************************
027010*       * 継続：終了年月日セット *
027020*       **************************
027030        IF ( 転帰区分Ｗ(部位ＣＮＴ) = 9 )
027040            MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
027050            MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
027060            MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
027070        END-IF
027080        IF ( 終了年月日Ｗ(部位ＣＮＴ) > 受理年月日Ｗ )
027090            MOVE 終了年Ｗ(部位ＣＮＴ) TO 受理年Ｗ
027100            MOVE 終了月Ｗ(部位ＣＮＴ) TO 受理月Ｗ
027110            MOVE 終了日Ｗ(部位ＣＮＴ) TO 受理日Ｗ
027120        END-IF
027130     END-PERFORM.
027140*
027150** ----- 前月初検のみかを判定 -----------*
027160*
027170*     MOVE 患者番号ＷＲ          TO 施記－患者番号.
027180*     MOVE 枝番ＷＲ              TO 施記－枝番.
027190*     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
027200*     MOVE 施術年ＷＲ            TO 施記－施術年.
027210*     MOVE 施術月ＷＲ            TO 施記－施術月.
027220*     MOVE ZERO                  TO 施記－施術日.
027230*     START 施術記録Ｆ   KEY IS >= 施記－患者コード
027240*                                  施記－施術和暦年月日
027250*     END-START.
027260*     IF ( 状態キー = "00" )
027270*             MOVE SPACE TO 終了フラグ２
027280*             PERFORM 施術記録Ｆ読込
027290*             IF  ( 終了フラグ２      = SPACE   ) AND
027300*                 ( 施記－患者コード  = 患者コードＷＲ ) AND
027310*                 ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
027320*                 ( 施記－施術年      = 施術年ＷＲ     ) AND
027330*                 ( 施記－施術月      = 施術月ＷＲ     ) 
027340** 当月施術開始日が再検かどうか判定
027350*                 IF ( 施記－再検料請求 = 1 )
027360*                      MOVE "YES"  TO  初日再検フラグ
027370*                 END-IF
027380**
027390*             END-IF
027400*     END-IF.
027410*     IF ( 初日再検フラグ = "YES" )
027420*        PERFORM 前月初検のみ判定
027430*     END-IF.
027440*
027450*================================================================*
027460*================================================================*
027470 レセプト並び順取得 SECTION.
027480*================================================================*
027490     MOVE 施術和暦ＷＲ       TO 作４－施術和暦.
027500     MOVE 施術年ＷＲ         TO 作４－施術年.
027510     MOVE 施術月ＷＲ         TO 作４－施術月.
027520     MOVE 患者コードＷＲ     TO 作４－患者コード.
027530     MOVE 保険種別ＷＲ       TO 作４－保険種別.
027540     READ 作業ファイル４
027550     NOT INVALID KEY
027560          MOVE NC"№"        TO 順番固定Ｗ
027570          MOVE 作４－順番    TO 順番Ｗ
027580     END-READ.
027590*
027600*================================================================*
027610*================================================================*
027620* ＯＣＲ情報取得 SECTION.
027630*
027640****************************************************************
027650* ●     接骨師会会員番号 .....施術所情報マスタより既に取得    *
027660* ●     施術年月         .....受診者情報Ｆより既に取得        *
027670* ● 　　保険種別         .....全柔用国保コード42をセット    　*
027680*                              新規マスタにより対応の可能性有り*
027690* ●     指定各社ＩＤ     .....自社ＩＤ"21"をセット            *
027700* ●     ＦＤ連番         .....資料参照                        *
027710* ● 　　請求金額         .....レセ－請求金額よりセット        *
027720* ●　　 負担割合         .....負担割合 = レセ－負担割合 / 10　*
027730****************************************************************
027740* 接骨師会会員番号
027750*     MOVE ＯＣＲ接骨師会会員番号Ｗ TO ＯＣＲ会員番号Ｗ.
027760* 施術年月
027770*     MOVE 施術年月Ｗ         TO ＯＣＲ施術年月Ｗ.
027780* 保険種別 = 全柔コード = 42
027790*     MOVE 42                 TO ＯＣＲ保険種別Ｗ.
027800* 指定各社ＩＤ = 自社ＩＤ = 21
027810*     MOVE "21"               TO ＯＣＲ各社ＩＤＷ.
027820* ＦＤ連番 = ＦＤ連番患者番号Ｗ + ＦＤ連番健保ＩＤＷ
027830*     MOVE 患者番号Ｗ         TO ＦＤ連番患者番号Ｗ.
027840*     MOVE 1                  TO ＦＤ連番健保ＩＤＷ.
027850* 請求金額
027860*     MOVE レセ－請求額       TO ＯＣＲ請求金額Ｗ.
027870* 負担割合
027880*     MOVE レセ－負担率     TO 負担割合ＷＲ.
027890*     COMPUTE ＯＣＲ負担割合Ｗ = 負担割合ＷＲ / 10.
027900*================================================================*
027910 施術記録Ｆ読込 SECTION.
027920*
027930     READ 施術記録Ｆ NEXT
027940     AT END
027950         MOVE "YES" TO 終了フラグ２
027960     END-READ.
027970*================================================================*
027980 印刷処理 SECTION.
027990*
028000     MOVE "YCH6125P" TO  定義体名Ｐ.
028010     MOVE "SCREEN"   TO  項目群名Ｐ.
028020     WRITE YCH6125P.
028030***     WRITE 印刷レコード.
028040     PERFORM エラー処理Ｐ.
028050*================================================================*
028060 エラー処理Ｐ SECTION.
028070*
028080     IF ( 通知情報Ｐ NOT = "00" )
028090         DISPLAY NC"帳票エラー"              UPON CONS
028100         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
028110         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
028120         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
028130         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
028140                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
028150         ACCEPT  キー入力 FROM CONS
028160         PERFORM ファイル閉鎖
028170         MOVE 99 TO PROGRAM-STATUS
028180         EXIT PROGRAM
028190     END-IF.
028200*================================================================*
028210 部位名称埋込処理 SECTION.
028220*
006490     STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
009980            負傷名称Ｗ                    DELIMITED BY SPACE
006500            レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
006520       INTO 負傷名Ｗ(部位ＣＮＴ)
006570     END-STRING.
028420*
028430*================================================================*
028440 初検日以前のデータ判定 SECTION.
028450*
028460*********************************************************************************
028470*  最初の初検日以前の当月中に施術記録レコードがあった時(治癒、中止)は、請求区分の
028480*  継続にもチェックする。(新規と継続の両方)
028490*********************************************************************************
028500** 最初の初検日を取得
028510     MOVE SPACE                 TO 初検フラグ.
028520     MOVE 患者番号ＷＲ          TO 施記－患者番号.
028530     MOVE 枝番ＷＲ              TO 施記－枝番.
028540     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
028550     MOVE 施術年ＷＲ            TO 施記－施術年.
028560     MOVE 施術月ＷＲ            TO 施記－施術月.
028570     MOVE ZERO                  TO 施記－施術日.
028580     START 施術記録Ｆ   KEY IS >= 施記－患者コード
028590                                  施記－施術和暦年月日
028600     END-START.
028610     IF ( 状態キー = "00" )
028620         MOVE ZERO  TO 初検和暦ＷＴ
028630         MOVE ZERO  TO 初検年ＷＴ
028640         MOVE ZERO  TO 初検月ＷＴ
028650         MOVE ZERO  TO 初検日ＷＴ
028660         MOVE SPACE TO 終了フラグ２
028670         PERFORM 施術記録Ｆ読込
028680         PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
028690                       ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
028700                       ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
028710                       ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
028720                       ( 施記－施術月     NOT = 施術月ＷＲ      ) OR
028730                       ( 初検フラグ           = "YES"           ) 
028740           IF ( 施記－診療区分 = 2 )
028750               MOVE 施記－施術和暦           TO 初検和暦ＷＴ
028760               MOVE 施記－施術年             TO 初検年ＷＴ
028770               MOVE 施記－施術月             TO 初検月ＷＴ
028780               MOVE 施記－施術日             TO 初検日ＷＴ
028790               MOVE "YES"                    TO 初検フラグ
028800           END-IF
028810           PERFORM 施術記録Ｆ読込
028820         END-PERFORM
028830     END-IF.
028840*
028850* 初検日以前のデータ判定
028860     IF ( 初検フラグ = "YES" )
028870        MOVE 患者番号ＷＲ          TO 施記－患者番号
028880        MOVE 枝番ＷＲ              TO 施記－枝番
028890        MOVE 初検和暦ＷＴ          TO 施記－施術和暦
028900        MOVE 初検年ＷＴ            TO 施記－施術年
028910        MOVE 初検月ＷＴ            TO 施記－施術月
028920        MOVE 初検日ＷＴ            TO 施記－施術日
028930        START 施術記録Ｆ   KEY IS <  施記－患者コード
028940                                     施記－施術和暦年月日
028950                                     REVERSED
028960        END-START
028970        IF ( 状態キー = "00" )
028980           MOVE SPACE  TO 終了フラグ２
028990           PERFORM 施術記録Ｆ読込
029000           IF ( 終了フラグ２    = SPACE        ) AND
029010              ( 施記－患者番号  = 患者番号ＷＲ ) AND
029020              ( 施記－枝番      = 枝番ＷＲ     ) AND
029030              ( 施記－施術和暦  = 初検和暦ＷＴ ) AND
029040              ( 施記－施術年    = 初検年ＷＴ   ) AND
029050              ( 施記－施術月    = 初検月ＷＴ   )
029060*  初検日以前の当月中に施術記録レコードがあった時
029070                IF ( 継続チェックＷ = SPACE )
029080                   MOVE NC"○"    TO 継続チェックＷ
029090                END-IF
029100           END-IF
029110         END-IF
029120     END-IF.
029130*
029140*================================================================*
029150 長期判定取得 SECTION.
029160*
029170* ３カ月以上の長期判定は "CHOUKI" を呼ぶ. 
029180     MOVE  SPACE TO  連期間－キー.
029190     INITIALIZE      連期間－キー.
029200     MOVE 施術和暦ＷＲ  TO  連期間－施術和暦.
029210     MOVE 施術年ＷＲ    TO  連期間－施術年.
029220     MOVE 施術月ＷＲ    TO  連期間－施術月.
029230     MOVE 患者番号ＷＲ  TO  連期間－患者番号.
029240     MOVE 枝番ＷＲ      TO  連期間－枝番.
029250*
029260     CALL   "CHOUKI".
029270     CANCEL "CHOUKI".
029280*
029290**** 適用１を使用 (「前月初検のみ」がある時は、くっつける)
029300     IF ( 連期間－対象フラグ  = "YES" )
029310        IF ( 適用１Ｗ  = SPACE )
029320           MOVE NC"※長期施術継続理由裏面に記載"  TO 適用１Ｗ
029330        ELSE
029340           STRING 適用１Ｗ           DELIMITED BY SPACE
029350                  NC"，"             DELIMITED BY SIZE
029360                  NC"※長期施術継続理由裏面に記載"   DELIMITED BY SIZE
029370                  INTO 適用１Ｗ
029380           END-STRING
029390        END-IF
029400     END-IF.
029410*
029420*================================================================*
029430 初検加算時刻取得 SECTION.
029440*****************************************************************
029450** 初検加算が時間外と深夜の時、適用に「受付時間」を印字する。
029460**   時刻の印字は月3回まで可能
029470*****************************************************************
029480     IF ( レセ－時間外 = 1 ) OR ( レセ－深夜 = 1 ) OR ( レセ－休日 = 1 )
029490*
029500         MOVE 患者番号ＷＲ          TO 施記－患者番号
029510         MOVE 枝番ＷＲ              TO 施記－枝番
029520         MOVE 施術和暦ＷＲ          TO 施記－施術和暦
029530         MOVE 施術年ＷＲ            TO 施記－施術年
029540         MOVE 施術月ＷＲ            TO 施記－施術月
029550         MOVE ZERO                  TO 施記－施術日
029560         START 施術記録Ｆ   KEY IS >= 施記－患者コード
029570                                      施記－施術和暦年月日
029580         END-START
029590         IF ( 状態キー = "00" )
029600             MOVE ZERO  TO 初検加算カウント
029610             MOVE SPACE TO 終了フラグ２
029620             PERFORM 施術記録Ｆ読込
029630             PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
029640                           ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
029650                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
029660                           ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
029670                           ( 施記－施術月     NOT = 施術月ＷＲ      ) 
029680               IF ( 施記－初検加算 = 1 OR 2 OR 3 ) AND ( 施記－診療区分 = 2 )
029690                   COMPUTE 初検加算カウント = 初検加算カウント  + 1
029700                   IF ( 初検加算カウント <= 3 )
029710                       MOVE 施記－初検加算 TO 初検加算区分ＷＴ(初検加算カウント)
029720                       MOVE 施記－受付時   TO 初検加算時ＷＴ(初検加算カウント)
029730                       MOVE 施記－受付分   TO 初検加算分ＷＴ(初検加算カウント)
029740                   END-IF
029750               END-IF
029760               PERFORM 施術記録Ｆ読込
029770             END-PERFORM
029780** 初検加算の時刻を適用にセット
033380            IF ( 初検加算時ＷＴ(1) NOT = ZERO ) OR ( 初検加算分ＷＴ(1) NOT = ZERO ) 
                      MOVE 初検加算時ＷＴ(1) TO 初検加算時Ｗ
                      MOVE ":"               TO 初検加算区切Ｗ
                      MOVE 初検加算分ＷＴ(1) TO 初検加算分Ｗ
                  END-IF
033380            IF ( 初検加算時ＷＴ(2) NOT = ZERO ) OR ( 初検加算分ＷＴ(2) NOT = ZERO ) 
031910                PERFORM 初検加算適用セット
                  END-IF
029800         END-IF
029810*
029820     END-IF.
029830*
029840*================================================================*
029850 初検加算適用セット SECTION.
029860*
029870     PERFORM VARYING 番号カウンタ FROM 1 BY 1
029880              UNTIL  番号カウンタ > 3
029890         IF ( 初検加算時ＷＴ(番号カウンタ)  = ZERO )  AND 
029900            ( 初検加算分ＷＴ(番号カウンタ)  = ZERO ) 
029910             CONTINUE
029920         ELSE
029930* 固定項目
029940             EVALUATE 初検加算区分ＷＴ(番号カウンタ) 
029950             WHEN 1
029960                MOVE NC"時間外"   TO 加算内容Ｗ(番号カウンタ)
033320             WHEN 2
033330                MOVE NC"休　日"   TO 加算内容Ｗ(番号カウンタ)
029970             WHEN 3
029980                MOVE NC"深　夜"   TO 加算内容Ｗ(番号カウンタ)
029990             END-EVALUATE
030000*
030010             MOVE NC"："          TO 加算区切Ｗ(番号カウンタ)
030020             MOVE NC"時"          TO 時固定Ｗ(番号カウンタ)
030030             MOVE NC"分"          TO 分固定Ｗ(番号カウンタ)
030040*
030050**** 数字→日本語変換
030060* 時間
030070             MOVE 初検加算時ＷＴ(番号カウンタ)  TO  数字Ｗ
030080             IF ( 数字Ｗ >= 10 )
030090                MOVE 数字Ｗ１    TO 負傷番号Ｗ１
030100                PERFORM 日本語変換
030110                MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ１(番号カウンタ)
030120                MOVE 数字Ｗ２    TO 負傷番号Ｗ１
030130                PERFORM 日本語変換
030140                MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
030150             ELSE
030160                MOVE 数字Ｗ２    TO 負傷番号Ｗ１
030170                PERFORM 日本語変換
030180                MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
030190             END-IF
030200* 分
030210             MOVE 初検加算分ＷＴ(番号カウンタ)  TO  数字Ｗ
030220             MOVE 数字Ｗ１    TO 負傷番号Ｗ１
030230             PERFORM 日本語変換
030240             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ１(番号カウンタ)
030250             MOVE 数字Ｗ２    TO 負傷番号Ｗ１
030260             PERFORM 日本語変換
030270             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ２(番号カウンタ)
030280** 
030290        END-IF
030300     END-PERFORM.
030310*
030320     MOVE  初検加算集団ＮＷ(1)   TO 初検加算時刻１Ｗ. 
030330     MOVE  初検加算集団ＮＷ(2)   TO 初検加算時刻２Ｗ. 
030340     MOVE  初検加算集団ＮＷ(3)   TO 初検加算時刻３Ｗ. 
030350*
030360**** 適用１か２を使用（長期理由記載で適用１を使っている時は、適用２）
030370     IF ( 初検加算時ＷＴ(2)  = ZERO ) AND ( 初検加算分ＷＴ(2)  = ZERO ) 
030380     CONTINUE
030390     ELSE
030400         IF ( 適用１Ｗ  = SPACE )
030410               STRING NC"初検加算"       DELIMITED BY SIZE
030420                      初検加算時刻１Ｗ   DELIMITED BY SIZE
030430                      初検加算時刻２Ｗ   DELIMITED BY SIZE
030440                      初検加算時刻３Ｗ   DELIMITED BY SIZE
030450                INTO 適用１Ｗ
030460         END-STRING
030470         ELSE
030480               STRING NC"初検加算"       DELIMITED BY SIZE
030490                      初検加算時刻１Ｗ   DELIMITED BY SIZE
030500                      初検加算時刻２Ｗ   DELIMITED BY SIZE
030510                      初検加算時刻３Ｗ   DELIMITED BY SIZE
030520                      INTO 適用２Ｗ
030530         END-STRING
030540         END-IF
030550     END-IF.
030560*
030570*================================================================*
030580 日本語変換 SECTION.
030590*
030600     MOVE NC"０"     TO 全角負傷番号Ｗ.
030610     CALL "htoz" WITH C LINKAGE
030620                        USING 負傷番号Ｗ１ 全角負傷番号Ｗ１.
030630*
030640*================================================================*
030650*================================================================*
030660 負傷原因取得 SECTION.
030670*
030680********************************************************************
030690*  負傷原因コードが同じものは、1行にまとめて印字する。
030700*  例: ①② 家で転んだ.
030710*     負傷原因コードが同じものをまとめ、テーブルにセット
030720*     (ただし、部位を飛んで同じものは、2行になる)
030730********************************************************************
030740     MOVE  ZERO   TO  カウンタ カウンタ２.
030750     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
030760             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
030770*
030780****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
030790        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
030800*
030810           IF ( カウンタ = ZERO )
030820               MOVE 1   TO  カウンタ カウンタ２
030830               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
030840               MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
030850               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
030860           ELSE
030870              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
030880                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
030890                 COMPUTE カウンタ２ = カウンタ２  +  1
030900                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
030910              ELSE
030920                 COMPUTE カウンタ = カウンタ  +  1
030930                 MOVE 1   TO  カウンタ２
030940                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
030950                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
030960                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
030970              END-IF
030980           END-IF
030990        END-IF
031000     END-PERFORM.
031010**************************************************************************
031020*  負傷原因マスタより文章取得
031030**************************************************************************
031040     MOVE  ZERO   TO  カウンタ カウンタ２.
031050     PERFORM VARYING カウンタ FROM 1 BY 1
031060             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
031070** 健保は 区分 01
031080         MOVE 01                        TO 負原－区分コード
031090         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
031100         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
031110         READ 負傷原因Ｆ
031120         NOT INVALID KEY
031130             INITIALIZE 負傷原因ＷＴ
031140             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
031150             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
031160             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
031170             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
031180             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
031190             PERFORM VARYING カウンタ２ FROM 1 BY 1
031200                     UNTIL ( カウンタ２ > 9 )  OR 
031210                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
031220                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
031230                WHEN 1
031240                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
031250                WHEN 2
031260                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
031270                WHEN 3
031280                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
031290                WHEN 4
031300                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
031310                WHEN 5
031320                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
031290                WHEN 6
031300                   MOVE "⑥"  TO  負傷原因ナンバーＷ１(カウンタ２)
031310                WHEN 7
031320                   MOVE "⑦"  TO  負傷原因ナンバーＷ１(カウンタ２)
031330                WHEN OTHER
031340                   CONTINUE
031350                END-EVALUATE
031360             END-PERFORM
031370*
031452             IF 負原－負傷原因入力区分 = 1
031453                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
031454                        負傷原因１ＷＴ  DELIMITED BY SIZE
031455                        負傷原因２ＷＴ  DELIMITED BY SIZE
031456                        負傷原因３ＷＴ  DELIMITED BY SIZE
031457                        負傷原因４ＷＴ  DELIMITED BY SIZE
031458                        負傷原因５ＷＴ  DELIMITED BY SIZE
031459                        INTO 負傷原因内容合成Ｗ(カウンタ)
031460                 END-STRING
031461             ELSE
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
031470             END-IF
031471*
031472         END-READ
031473     END-PERFORM.
031480*
031490     PERFORM 負傷原因セット.
031500*
031510*================================================================*
031520 負傷原因セット SECTION.
031530*
031540**************************************************************************
031550*  文章が1行を超える時は、複数行に分解する。
031560**************************************************************************
031570     MOVE  ZERO   TO  カウンタ カウンタ２.
031580     PERFORM VARYING カウンタ FROM 1 BY 1
031590             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
031600*
031610          INITIALIZE 負傷原因内容分解ＸＷ
031620          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
031630          IF ( 負傷原因内容１ＸＷ  NOT = SPACE )
031640              COMPUTE カウンタ２ = カウンタ２  +  1
031650              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
031660          END-IF
031670          IF ( 負傷原因内容２ＸＷ  NOT = SPACE )
031680              COMPUTE カウンタ２ = カウンタ２  +  1
031690              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
031700          END-IF
031710          IF ( 負傷原因内容３ＸＷ  NOT = SPACE )
031720              COMPUTE カウンタ２ = カウンタ２  +  1
031730              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
031740          END-IF
033830          IF ( 負傷原因内容４ＸＷ  NOT = SPACE )
033840              COMPUTE カウンタ２ = カウンタ２  +  1
033850              MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
033860          END-IF
031750*
031760     END-PERFORM.
031770*================================================================*
031780 助成印取得 SECTION.
031790*
031800* 2006/04 変更
031810* 助成印は "JOSEIMEI" を呼ぶ. 
031820     MOVE SPACE TO  連助成名称－キー.
031830     INITIALIZE     連助成名称－キー.
031840     MOVE 助成種別ＷＲ           TO 連助成名称－助成種別.
031850     MOVE 費用負担者番号助成ＷＲ TO 連助成名称－費用負担者番号助成.
031860*
031870     CALL   "JOSEIMEI".
031880     CANCEL "JOSEIMEI".
031890*
031900     MOVE 連助成名称－１文字 TO 助成印Ｗ.
031910*
031920***
032170*
032180*================================================================*
032190 前月初検のみ判定 SECTION.
032200*
032210*** 前月の通院日が初検か判定 
032220     MOVE  SPACE            TO 前月フラグ.
032230     MOVE 受－患者コード    TO 施記－患者コード.
032240     MOVE 受－施術和暦      TO 施記－施術和暦.
032250     MOVE 受－施術年        TO 施記－施術年.
032260     MOVE 受－施術月        TO 施記－施術月.
032270     MOVE 1                 TO 施記－施術日.
032280     START 施術記録Ｆ   KEY IS <  施記－患者コード
032290                                  施記－施術和暦年月日
032300                                  REVERSED
032310     END-START.
032320     IF ( 状態キー = "00" )
032330         MOVE SPACE  TO 終了フラグ２
032340         PERFORM 施術記録Ｆ読込
032350         IF ( 終了フラグ２      = SPACE  ) AND
032360            ( 施記－患者コード  = 受－患者コード ) AND
032370            ( 施記－診療区分    = 2 ) 
032380*
032390            PERFORM 前月判定
032400**** 適用１を使用
032410            IF ( 前月フラグ = "YES" )
032420               MOVE NC"※前月初検のみ"    TO  適用１Ｗ
032430            END-IF
032440**
032450         END-IF
032460     END-IF.
032470*
032480*================================================================*
032490 前月判定  SECTION.
032500* 
032510*** 読み込んだ施術記録の年月が、前月かどうか判定 (年月の差が 1 か?)
032520      MOVE  SPACE  TO  前月フラグ.
032530      INITIALIZE  計算年月日Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
032540**
032550      MOVE 受－施術和暦    TO 終了和暦２Ｗ.
032560      MOVE 受－施術年      TO 終了年２Ｗ.
032570      MOVE 受－施術月      TO 終了月２Ｗ.
032580      MOVE 施記－施術和暦  TO 開始和暦２Ｗ.
032590      MOVE 施記－施術年    TO 開始年２Ｗ.
032600      MOVE 施記－施術月    TO 開始月２Ｗ.
032610*
032620      EVALUATE TRUE
032630       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ = 終了年２Ｗ)
032640            PERFORM  前月比較月
032650       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ NOT = 終了年２Ｗ)
032660            PERFORM  前月比較年
032670       WHEN  開始和暦２Ｗ NOT = 終了和暦２Ｗ 
032680            PERFORM  前月比較元号
032690      END-EVALUATE.
032700*
032710      IF ( 計算月Ｗ = 1 )
032720         MOVE  "YES"  TO  前月フラグ
032730      END-IF.
032740*
032750*================================================================*
032760 前月比較月  SECTION.
032770*
032780     IF ( 終了月２Ｗ >  開始月２Ｗ )
032790         COMPUTE 計算月Ｗ = 終了月２Ｗ - 開始月２Ｗ
032800     ELSE
032810        MOVE ZERO TO 計算月Ｗ
032820     END-IF.
032830*
032840*================================================================*
032850 前月比較年  SECTION.
032860*
032870     IF ( 終了年２Ｗ >  開始年２Ｗ )
032880         COMPUTE 計算年Ｗ = 終了年２Ｗ - 開始年２Ｗ
032890         COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
032900     ELSE
032910        MOVE ZERO TO 計算月Ｗ
032920     END-IF.
032930*
032940*================================================================*
032950 前月比較元号  SECTION.
032960*
032970     MOVE 開始和暦２Ｗ TO 元－元号区分.
032980     READ 元号マスタ
032990     NOT INVALID KEY
033000         MOVE 元－開始西暦年 TO 開始西暦年Ｗ
033010     END-READ.
033020     MOVE 終了和暦２Ｗ TO 元－元号区分.
033030     READ 元号マスタ
033040     NOT INVALID KEY
033050         MOVE 元－開始西暦年 TO 終了西暦年Ｗ
033060     END-READ.
033070**
033080     IF ( 開始西暦年Ｗ NOT = ZERO ) AND ( 終了西暦年Ｗ NOT = ZERO )
033090        COMPUTE 開始西暦年Ｗ = 開始西暦年Ｗ + 開始年２Ｗ - 1
033100        COMPUTE 終了西暦年Ｗ = 終了西暦年Ｗ + 終了年２Ｗ - 1
033110*
033120        IF ( 終了西暦年Ｗ =  開始西暦年Ｗ )
033130           PERFORM  前月比較月
033140        ELSE
033150           IF ( 終了西暦年Ｗ >  開始西暦年Ｗ )
033160               COMPUTE 計算年Ｗ = 終了西暦年Ｗ - 開始西暦年Ｗ
033170               COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
033180           ELSE
033190               MOVE ZERO TO 計算月Ｗ
033200           END-IF
033210        END-IF
033220     ELSE
033230        MOVE ZERO TO 計算月Ｗ
033240     END-IF.
033250*
033430*================================================================*
033440 受診者印刷区分更新 SECTION.
033450*
033460** //  受診者情報Ｆの印刷区分に１をセットし、更新する。//  
033470*
033480     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
033490     MOVE 施術年ＷＲ         TO 受－施術年.
033500     MOVE 施術月ＷＲ         TO 受－施術月.
033510     MOVE 患者コードＷＲ     TO 受－患者コード.
033520     READ 受診者情報Ｆ
033530     NOT INVALID KEY
033540         MOVE  1  TO  受－レセ印刷区分
033550         REWRITE  受－レコード
033560         END-REWRITE
033570         IF ( 状態キー NOT = "00" )
033580            MOVE NC"受診者" TO ファイル名
033590            PERFORM エラー表示
033600         END-IF
033610     END-READ.
033620*
033630*================================================================*
033640 月末日取得 SECTION.
033650*
033660     MOVE 施術年ＷＲ   TO 受理年Ｗ.
033670     MOVE 施術月ＷＲ   TO 受理月Ｗ.
033680     MOVE 施術和暦ＷＲ TO 元－元号区分.
033690     READ 元号マスタ
033700     NOT INVALID KEY
033710         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
033720     END-READ.
033730     IF ( 施術西暦年Ｗ NOT = ZERO )
033740        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
033750     END-IF.
033760*
033770     EVALUATE 施術月ＷＲ
033780     WHEN 4
033790     WHEN 6
033800     WHEN 9
033810     WHEN 11
033820         MOVE 30 TO 受理日Ｗ
033830     WHEN 2
033840         DIVIDE 4 INTO 施術西暦年Ｗ GIVING    商Ｗ
033850                                    REMAINDER 余Ｗ
033860         END-DIVIDE
033870         IF ( 余Ｗ = ZERO )
033880             MOVE 29 TO 受理日Ｗ
033890         ELSE
033900             MOVE 28 TO 受理日Ｗ
033910         END-IF
033920     WHEN 1
033930     WHEN 3
033940     WHEN 5
033950     WHEN 7
033960     WHEN 8
033970     WHEN 10
033980     WHEN 12
033990         MOVE 31 TO 受理日Ｗ
034000     WHEN OTHER
034010          CONTINUE
034020     END-EVALUATE.
034030*
034040*================================================================*
034050 委任年月日取得 SECTION.
034060*
034070** ---// ここの受理年には、最終通院日が入っている為、退避する //----
034080     MOVE 受理年Ｗ   TO 最終通院年Ｗ.
034090     MOVE 受理月Ｗ   TO 最終通院月Ｗ.
034100     MOVE 受理日Ｗ   TO 最終通院日Ｗ.
034110***
034120* (柔整師側)
034130     EVALUATE レセプト日付区分Ｗ 
034140*    /  最終通院日 /
034150     WHEN ZERO
034160         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
034170         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
034180         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
034190*    /  月末日 /
034200     WHEN 1 
034210         PERFORM 月末日取得
034220         MOVE 受理年Ｗ     TO 柔整師年Ｗ
034230         MOVE 受理月Ｗ     TO 柔整師月Ｗ
034240         MOVE 受理日Ｗ     TO 柔整師日Ｗ
034250*    /  印字なし /
034260     WHEN 9
034270         MOVE ZERO         TO 柔整師年Ｗ
034280         MOVE ZERO         TO 柔整師月Ｗ
034290         MOVE ZERO         TO 柔整師日Ｗ
034300*    /  その他は、最終通院日 /
034310     WHEN OTHER
034320         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
034330         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
034340         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
034350     END-EVALUATE.
034360**
034370* (患者側)
034380     EVALUATE レセプト患者日付区分Ｗ 
034390*    /  最終通院日 /
034400     WHEN ZERO
034410         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
034420         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
034430         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
034440*    /  月末日 /
034450     WHEN 1 
034460         PERFORM 月末日取得
034470         MOVE 受理年Ｗ     TO 患者委任年Ｗ
034480         MOVE 受理月Ｗ     TO 患者委任月Ｗ
034490         MOVE 受理日Ｗ     TO 患者委任日Ｗ
034500*    /  印字なし /
034510     WHEN 9
034520         MOVE ZERO         TO 患者委任年Ｗ
034530         MOVE ZERO         TO 患者委任月Ｗ
034540         MOVE ZERO         TO 患者委任日Ｗ
034550*    /  その他は、最終通院日 /
034560     WHEN OTHER
034570         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
034580         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
034590         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
034600     END-EVALUATE.
034610*
034620*================================================================*
034630 レセ摘要再セット SECTION.
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
034810*
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
034820*================================================================*
034821*================================================================*
034822 地域特有処理 SECTION.
034823*
034824*--------------------------------------------------------*
034825*  福岡県：経過欄の固定印字 (全柔ＦＰＤ区分Ｗ 1 使用)
034826*  長期以外の部位は、「順調」
034827*  長期の部位は、「緩慢」
034828*--------------------------------------------------------*
034829*
034830     IF 全柔ＦＰＤ区分Ｗ = 1
034831*      まず「順調」セット
034832        PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
016870                UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR
016880                      ( 部位ＣＮＴ > 5 )
034834*
034835                 EVALUATE 部位ＣＮＴ
034836                 WHEN 1
034837                     MOVE NC"①" TO 経過部位数字Ｗ
034838                 WHEN 2
034839                     MOVE NC"②" TO 経過部位数字Ｗ
034840                 WHEN 3
034841                     MOVE NC"③" TO 経過部位数字Ｗ
034842                 WHEN 4
034843                     MOVE NC"④" TO 経過部位数字Ｗ
034844                 WHEN 5
034845                     MOVE NC"⑤" TO 経過部位数字Ｗ
034846                 END-EVALUATE
034847                 MOVE SPACE TO 経過略称(部位ＣＮＴ)
034848                 STRING  経過部位数字Ｗ   DELIMITED BY SPACE
034849                         NC"順調"         DELIMITED BY SPACE
034850                        INTO 経過略称(部位ＣＮＴ)
034851                 END-STRING
034852        END-PERFORM
034853*
034854*      次に、３カ月以上の長期判定
034855        MOVE  SPACE TO  連期間－キー
034856        INITIALIZE      連期間－キー
034857        MOVE 施術和暦ＷＲ  TO  連期間－施術和暦
034858        MOVE 施術年ＷＲ    TO  連期間－施術年
034859        MOVE 施術月ＷＲ    TO  連期間－施術月
034860        MOVE 患者番号ＷＲ  TO  連期間－患者番号
034861        MOVE 枝番ＷＲ      TO  連期間－枝番
034862        CALL   "CHOUKI"
034863        CANCEL "CHOUKI"
034864*
034865        IF 連期間－対象フラグ  = "YES"
034866           PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
016870                   UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR
016880                         ( 部位ＣＮＴ > 5 )
034868*
034869               IF 連期間－期間Ｗ(部位ＣＮＴ)  >  ZERO
034870
034871                   EVALUATE 部位ＣＮＴ
034872                   WHEN 1
034873                       MOVE NC"①" TO 経過部位数字Ｗ
034874                   WHEN 2
034875                       MOVE NC"②" TO 経過部位数字Ｗ
034876                   WHEN 3
034877                       MOVE NC"③" TO 経過部位数字Ｗ
034878                   WHEN 4
034879                       MOVE NC"④" TO 経過部位数字Ｗ
034880                   WHEN 5
034881                       MOVE NC"⑤" TO 経過部位数字Ｗ
034882                   END-EVALUATE
034883                   MOVE SPACE TO 経過略称(部位ＣＮＴ)
034884                   STRING  経過部位数字Ｗ   DELIMITED BY SPACE
034885                           NC"緩慢"         DELIMITED BY SPACE
034886                          INTO 経過略称(部位ＣＮＴ)
034887                   END-STRING
034888               END-IF
034889           END-PERFORM
034890        END-IF
034891*
034892     END-IF.
034893*
034894*
034895*================================================================*
034896*================================================================*
034897 エラー表示 SECTION.
034898*
034899     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
034900     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
034901     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
034902     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
034903                                                   UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
034910     ACCEPT  キー入力 FROM CONS
034920     PERFORM ファイル閉鎖.
034930     EXIT PROGRAM.
034940*================================================================*
034950*================================================================*
034960 ファイル閉鎖 SECTION.
034970*
034980     CLOSE 印刷ファイル.
034990     CLOSE 保険者マスタ     元号マスタ          名称マスタ
035000           レセプトＦ       制御情報マスタ      施術所情報マスタ
035010           請求先マスタ     経過マスタ          受診者情報Ｆ
035020           施術記録Ｆ       負傷データＦ        負傷原因Ｆ
035030           作業ファイル４   メモファイル.
035040*================================================================*
035050 終了処理 SECTION.
035060*
035070     PERFORM ファイル閉鎖.
035080*================================================================*
035090*================================================================*
035100 テスト印字処理 SECTION.
035110*
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
           初回処置料合計 冷罨法単価 温罨法単価 電療単価 後療単価１ 
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
           公費負担者番号 受給者番号 県施術ＩＤ 記号番号 住所１ 住所２ 郵便番号１ 郵便番号２ 
           郵便区切 口座名義人カナ 口座名義人 柔整師番号 口座番号 保険者名称 
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
036380*
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
036400******************************************************************
036410 END PROGRAM YCH6165.
036420******************************************************************
