000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHN6121.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*        日骨  レセプト印刷（柔+ｳｨﾝﾄﾞｳｽﾞ版）
000100*         MED = YAW610 YHN6121P
      *
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2020-08-31
000130 DATE-COMPILED.          2020-08-31
      */金属副子・運動後療の変更・追加/1805
      */明細書発行加算を適用２に追加/2022
      */2024.10  長期頻回を適用に追加/2407
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
           SELECT  レセプトＦ      ASSIGN      TO        RECEPTL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  レセ－施術和暦年月
                                                                レセ－患者コード
                                                                レセ－レセ種別
                                   ALTERNATE RECORD KEY     IS  レセ－患者コード
                                                                レセ－施術和暦年月
                                                                レセ－レセ種別
                                   ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
                                                                レセ－施術和暦年月
                                                                レセ－患者コード
                                                                レセ－レセ種別
                                   ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
                                                                レセ－レセ種別
                                                                レセ－請求保険者番号
                                                                レセ－患者コード
                                                                レセ－施術和暦年月
                                   ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
                                                                レセ－請求保険者番号
                                                                レセ－患者コード
                                                                レセ－レセ種別
                                                                レセ－施術和暦年月
                                   FILE STATUS              IS  状態キー
                                   LOCK        MODE         IS  AUTOMATIC.
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
000680     SELECT  ＩＤ管理マスタ    ASSIGN      TO        IDKANRL
000690                             ORGANIZATION             IS  INDEXED
000700                             ACCESS MODE              IS  DYNAMIC
000710                             RECORD KEY               IS  ＩＤ管－ＩＤ区分
000720                                                          ＩＤ管－施術所番号
000730                                                          ＩＤ管－保険種別
000740                                                          ＩＤ管－保険者番号
000750                             ALTERNATE RECORD KEY     IS  ＩＤ管－施術ＩＤ番号
000760                                                          ＩＤ管－ＩＤ区分
000770                                                          ＩＤ管－施術所番号
000780                                                          ＩＤ管－保険種別
000790                                                          ＩＤ管－保険者番号
000800                             FILE STATUS              IS  状態キー
000810                             LOCK        MODE         IS  AUTOMATIC.
000820     SELECT  経過マスタ      ASSIGN      TO        KEIKAL
000830                             ORGANIZATION             IS  INDEXED
000840                             ACCESS MODE              IS  DYNAMIC
000850                             RECORD KEY               IS  経－区分コード
000860                                                          経－経過コード
000870                             FILE STATUS              IS  状態キー
000880                             LOCK        MODE         IS  AUTOMATIC.
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
001280                             RECORD KEY               IS  負－施術和暦年月
001290                                                          負－患者コード
001300                             ALTERNATE RECORD KEY     IS  負－患者コード
001310                                                          負－施術和暦年月
001320                             FILE STATUS              IS  状態キー
001330                             LOCK        MODE         IS  AUTOMATIC.
001340     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
001350                             ORGANIZATION             IS  INDEXED
001360                             ACCESS MODE              IS  DYNAMIC
001370                             RECORD KEY               IS  負原－区分コード
001380                                                          負原－負傷原因コード
001390                             FILE STATUS              IS  状態キー
001400                             LOCK        MODE         IS  AUTOMATIC.
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
001080* レセ並び順用
001081     SELECT  作業ファイル３  ASSIGN      TO  "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001100                             ORGANIZATION             IS  INDEXED
001110                             ACCESS                   IS  DYNAMIC
001120                             RECORD      KEY          IS  作３－施術和暦年月
001130                                                          作３－患者コード
001140                                                          作３－保険種別
001150                             FILE        STATUS       IS  状態キー
001160                             LOCK        MODE         IS  AUTOMATIC.
      */並び順用　レセ
000108     SELECT  作業ファイル５  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W4315L.DAT"
000109                             ORGANIZATION             IS  INDEXED
000110                             ACCESS                   IS  DYNAMIC
000111                             RECORD      KEY          IS  作５－患者コード
000912                                                          作５－施術和暦年月
000912                                                          作５－保険種別
000134                             ALTERNATE RECORD KEY     IS  作５－６号順番
                                                                作５－７号順番
                                                                作５－患者順番
000980                             FILE        STATUS       IS  状態キー
000990                             LOCK        MODE         IS  AUTOMATIC.
001830     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF002
001840                             SYMBOLIC    DESTINATION  IS "PRT"
001850                             FORMAT                   IS  定義体名Ｐ
001860                             GROUP                    IS  項目群名Ｐ
001870                             PROCESSING  MODE         IS  処理種別Ｐ
001880                             UNIT        CONTROL      IS  拡張制御Ｐ
001890                             FILE        STATUS       IS  通知情報Ｐ.
001900******************************************************************
001910*                      DATA DIVISION                             *
001920******************************************************************
001930 DATA                    DIVISION.
001940 FILE                    SECTION.
001950*                           ［ＲＬ＝  ３２０］
001960 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001970     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
001980*                           ［ＲＬ＝  １２８］
001990 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
002000     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
002010*                           ［ＲＬ＝  １２８］
002020 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
002030     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
      *
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
002070*                           ［ＲＬ＝  ２５６］
002080 FD  制御情報マスタ      BLOCK   CONTAINS   1   RECORDS.
002090     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
002100*                           ［ＲＬ＝  １２８］
002110 FD  施術所情報マスタ    BLOCK   CONTAINS   1   RECORDS.
002120     COPY SEJOHO          OF  XFDLIB  JOINING   施情   AS  PREFIX.
002130*                           ［ＲＬ＝  １２８］
002140 FD  ＩＤ管理マスタ      BLOCK   CONTAINS   1   RECORDS.
002150     COPY IDKANR          OF  XFDLIB  JOINING   ＩＤ管   AS  PREFIX.
002160*                           ［ＲＬ＝  １２８］
002170 FD  経過マスタ          BLOCK   CONTAINS   1   RECORDS.
002180     COPY KEIKA           OF  XFDLIB  JOINING   経   AS  PREFIX.
002190*                           ［ＲＬ＝  ３２０］
002200 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
002210     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
002220*                           ［ＲＬ＝  ２５６］
002230 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
002240     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
002250*                           ［ＲＬ＝  １２８］
002260 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002270     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
002280*                           ［ＲＬ＝  １２８］
002290 FD  負傷原因Ｆ          BLOCK   CONTAINS   1   RECORDS.
002300     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
002250*                           ［ＲＬ＝  ２５６］
002260 FD  市町村マスタ          BLOCK   CONTAINS   1   RECORDS.
002270     COPY SITYOSN        OF  XFDLIB  JOINING   市   AS  PREFIX.
002470*                           ［ＲＬ＝  ６４０］
002480 FD  会情報マスタ        BLOCK   CONTAINS   1   RECORDS.
002490     COPY KAIJOHO         OF  XFDLIB  JOINING   会情   AS  PREFIX.
002400**
001740 FD  作業ファイル３ RECORD  CONTAINS 32 CHARACTERS.
001750 01  作３－レコード.
001760     03  作３－レコードキー.
001770         05  作３－施術和暦年月.
001780             07  作３－施術和暦            PIC 9.
001790             07  作３－施術年              PIC 9(2).
001800             07  作３－施術月              PIC 9(2).
001810         05  作３－患者コード.
001820             07 作３－患者番号             PIC 9(6).
001830             07 作３－枝番                 PIC X(1).
001840         05  作３－保険種別                PIC 9(2).
001850     03  作３－レコードデータ.
001860         05  作３－順番                    PIC 9(4).
001870         05  FILLER                        PIC X(14).
000174*                           ［ＲＬ＝  ３２］
000175 FD  作業ファイル５ RECORD  CONTAINS 32 CHARACTERS.
000176 01  作５－レコード.
000177     03  作５－レコードキー.
001310         05  作５－患者コード.
001320             07 作５－患者番号               PIC 9(6).
001330             07 作５－枝番                   PIC X(1).
001340         05  作５－施術和暦年月.
001350             07  作５－施術和暦              PIC 9.
001360             07  作５－施術年                PIC 9(2).
001370             07  作５－施術月                PIC 9(2).
001400         05  作５－保険種別                  PIC 9(2).
000188     03  作５－レコードデータ.
001261         05  作５－６号順番                  PIC 9(3).
001261         05  作５－７号順番                  PIC 9(3).
001261         05  作５－患者順番                  PIC 9(3).
001261         05  作５－分類コード                PIC 9(1).
000201         05  FILLER                          PIC X(8).
002550*
002560 FD  印刷ファイル.
002570     COPY YHN6121P        OF  XMDLIB.
002580*----------------------------------------------------------------*
002590******************************************************************
002600*                WORKING-STORAGE SECTION                         *
002610******************************************************************
002620 WORKING-STORAGE         SECTION.
002630 01 キー入力                           PIC X     VALUE SPACE.
002640 01 状態キー                           PIC X(2)  VALUE SPACE.
002650 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002660 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
004581 01 終了フラグ４                       PIC X(3) VALUE SPACE.
002670 01 ファイル名                         PIC N(6)  VALUE SPACE.
002680 01 レセプトＰＧＷ                     PIC X(8)  VALUE SPACE.
002690 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002710 01 レセプト種類Ｗ                     PIC X(4)  VALUE SPACE.
002640 01 脱出フラグ                         PIC X(3)  VALUE SPACE.
005190 01 英数字項目２Ｗ                     PIC X(22) VALUE SPACE.
005150 01 英数字項目Ｗ.
005160   03 英数字項目ＸＷ                   PIC X(22) VALUE SPACE.
003630 01 文字ＣＮＴ                         PIC 9(2) VALUE ZERO.
       01 数字変換Ｗ.
          03 数字変換ＷＲ                    PIC 9(8) VALUE ZERO.
002700*
002740*--- 制御マスタ退避 ---*
002750 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
002760 01 最大登録数Ｗ                       PIC 9(1)  VALUE ZERO.
002770 01 負傷連続登録Ｗ                     PIC 9(1)  VALUE ZERO.
002780 01 遅延フラグ                         PIC X(3)  VALUE SPACE.
002790 01 遅延回数Ｗ                         PIC 9(4)  VALUE ZERO.
002800 01 遅延ＣＮＴ                         PIC 9(5)  VALUE ZERO.
002810*
002820** 負傷原因・長期理由印刷区分用
002830 01 負傷原因印刷区分Ｗ                 PIC 9     VALUE ZERO.
002840 01 長期理由印刷区分Ｗ                 PIC 9     VALUE ZERO.
002140*
002860** レセ下段の日付区分用 (0:最終通院日、1:月末日、9:印字なし)
002870 01 レセプト日付区分Ｗ                 PIC 9     VALUE ZERO.
002880 01 レセプト患者日付区分Ｗ             PIC 9     VALUE ZERO.
002890*
002900*--- カウンタ ---*
002910 01 カウンタ                           PIC 9(2)  VALUE ZERO.
002920 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
002930 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002940*
002950*--- 郵便番号編集用 ---*
002960 01 郵便番号編集Ｗ.
002970    03 FILLER                          PIC X(2)  VALUE "〒".
002980    03 郵便番号編集１Ｗ                PIC X(3)  VALUE SPACE.
002990    03 FILLER                          PIC X(1)  VALUE "-".
003000    03 郵便番号編集２Ｗ                PIC X(4)  VALUE SPACE.
003010*
003020*--- 負傷データ取得用 ---*
003030 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
003040 01 部位名称Ｗ                         PIC N(12) VALUE SPACE.
003050 01 部位長Ｗ                           PIC 9(2)  VALUE 1.
003060 01 経過部位Ｗ                         PIC N(1)  VALUE SPACE.
003070*
003080** 枝番判定用
003090 01 開始診療日手動区分Ｗ               PIC 9     VALUE ZERO.
003100*
003110* 負傷原因印刷区分
003120 01 レセ負傷原因印刷区分Ｗ             PIC 9     VALUE ZERO.
004440 01 レセ長期理由印刷区分Ｗ             PIC 9    VALUE ZERO.
003130*
003140*--- 経過略称編集用 ---*
003150 01 経過略称編集ＴＢＬ.
003160    03 経過略称編集Ｔ                  PIC N(10) VALUE SPACE OCCURS 5.
003170 01 経過略称編集Ｗ                     PIC N(10) VALUE SPACE.
003180*
003190*--- 施術記録取得用 ---*
003200 01 初日再検フラグ                     PIC X(3)  VALUE SPACE.
003210 01 前月フラグ                         PIC X(3)  VALUE SPACE.
003220*
003230 01 終了年月日ＷＴ.
002980    03 終了和暦ＷＴ                    PIC 9     VALUE ZERO.
003240    03 終了年ＷＴ                      PIC 9(2)  VALUE ZERO.
003250    03 終了月ＷＴ                      PIC 9(2)  VALUE ZERO.
003260    03 終了日ＷＴ                      PIC 9(2)  VALUE ZERO.
003270** 前月判定用
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
003400 01 計算年月日Ｗ.
003410    03 計算和暦Ｗ                      PIC 9(1)  VALUE ZERO.
003420    03 計算年Ｗ                        PIC S9(2) VALUE ZERO.
003430    03 計算月Ｗ                        PIC S9(2) VALUE ZERO.
003440    03 計算日Ｗ                        PIC S9(2) VALUE ZERO.
003450*
003460*--- 初検日退避用 ---*
003470 01 初検フラグ                         PIC X(3)  VALUE SPACE.
003480*
003490 01 初検年月日ＷＴ.
003500    03 初検和暦ＷＴ                    PIC 9     VALUE ZERO.
003510    03 初検年ＷＴ                      PIC 9(2)  VALUE ZERO.
003520    03 初検月ＷＴ                      PIC 9(2)  VALUE ZERO.
003530    03 初検日ＷＴ                      PIC 9(2)  VALUE ZERO.
003540*
003550*--- 初検加算時刻用 ---*
003560 01 初検加算ＷＴ.
003570    03 初検加算カウント                PIC 9     VALUE ZERO.
003580    03 番号カウンタ                    PIC 9     VALUE ZERO.
003590    03 初検加算集団ＷＴ  OCCURS 3.
003600       05 初検加算区分ＷＴ             PIC 9     VALUE ZERO.
003610       05 初検加算時ＷＴ               PIC 9(2)  VALUE ZERO.
003620       05 初検加算分ＷＴ               PIC 9(2)  VALUE ZERO.
003630    03 初検加算集団ＮＷ  OCCURS 3.
003640       05 加算区切Ｗ                   PIC N(1)  VALUE SPACE.
003650       05 加算内容Ｗ                   PIC N(3)  VALUE SPACE.
003660       05 初検加算時ＮＷ１             PIC N(1)  VALUE SPACE.
003670       05 初検加算時ＮＷ２             PIC N(1)  VALUE SPACE.
003680       05 時固定Ｗ                     PIC N(1)  VALUE SPACE.
003690       05 初検加算分ＮＷ１             PIC N(1)  VALUE SPACE.
003700       05 初検加算分ＮＷ２             PIC N(1)  VALUE SPACE.
003710       05 分固定Ｗ                     PIC N(1)  VALUE SPACE.
003720    03 初検加算時刻１Ｗ                PIC N(10) VALUE SPACE.
003730    03 初検加算時刻２Ｗ                PIC N(10) VALUE SPACE.
003740    03 初検加算時刻３Ｗ                PIC N(10) VALUE SPACE.
003070    03 初検加算区切Ｗ                  PIC X     VALUE SPACE.
003080    03 初検加算時Ｗ                    PIC 9(2)  VALUE ZERO.
003090    03 初検加算分Ｗ                    PIC 9(2)  VALUE ZERO.
003630*
003640* 共済番号用
003650 01 共済連番号集団Ｗ.
003660    03 共済連番号名Ｗ                  PIC X(14)  VALUE SPACE.
003670    03 共済連番号名ＮＷ REDEFINES  共済連番号名Ｗ  PIC N(7).
003680    03 共済連番号Ｗ                    PIC X(6)  VALUE SPACE.
003690    03 共済連番号単位Ｗ                PIC X(2)  VALUE SPACE.
003700    03 共済連番号単位ＮＷ REDEFINES  共済連番号単位Ｗ  PIC N.
003710*
003720* 自衛官番号用
003730 01 自衛官番号集団Ｗ.
003740    03 自衛官番号名Ｗ                  PIC X(8)  VALUE SPACE.
003750    03 自衛官番号名ＮＷ REDEFINES  自衛官番号名Ｗ  PIC N(4).
003760    03 自衛官番号Ｗ                    PIC X(6)  VALUE SPACE.
003770    03 自衛官番号単位Ｗ                PIC X(2)  VALUE SPACE.
003780    03 自衛官番号単位ＮＷ REDEFINES  自衛官番号単位Ｗ  PIC N.
003750*
003760** 数字→日本語変換
003770 01 数字Ｗ                             PIC 9(2).
003780 01 数字Ｒ REDEFINES 数字Ｗ.
003790    03 数字Ｗ１                        PIC X(1).
003800    03 数字Ｗ２                        PIC X(1).
003810*
003820 01 負傷番号Ｗ                         PIC 9.
003830 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
003840    03 負傷番号Ｗ１                    PIC X.
003850*
003860 01 全角負傷番号Ｗ                     PIC N.
003870 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
003880    03 全角負傷番号Ｗ１                PIC X(2).
003890*
003900*--- 負傷原因用 ---*
003910 01 負傷原因固定Ｗ                     PIC X(50)
003920     VALUE "業務災害、通勤災害又は第三者行為以外の原因による。".
003930*
003940 01 負傷原因ＷＴ.
003450    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
003460    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
003470    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
003480    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
003490    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
004000    03 負傷原因ナンバーＷＴ.
004010       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
004020    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
004030 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
004040 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
004050 01 負傷原因ＴＢＬ.
004060    03 負傷原因コードＴＢＬ            OCCURS 9.
004070       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
004080       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
004090       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
004100 01 負傷原因内容Ｗ.
004110    03 負傷原因内容合成Ｗ              PIC X(318) OCCURS 9 VALUE SPACE.
003620    03 負傷原因内容分解ＸＷ.
003630       05 負傷原因内容１ＸＷ           PIC X(80)  VALUE SPACE.
003640       05 負傷原因内容２ＸＷ           PIC X(80)  VALUE SPACE.
003640       05 負傷原因内容３ＸＷ           PIC X(80)  VALUE SPACE.
003650       05 負傷原因内容４ＸＷ           PIC X(78)  VALUE SPACE.
       01 負傷原因１文Ｗ.
          03 負傷原因１文ＷＲ                OCCURS 7.
             05 負傷原因１文ＷＰ             PIC X(100) VALUE SPACE.
004170*
004180*--- 委任年月日用 ---*
004190 01 受理年月日Ｗ.
007350    03 受理和暦Ｗ                      PIC 9     VALUE ZERO.
004200    03 受理年Ｗ                        PIC 9(2)  VALUE ZERO.
004210    03 受理月Ｗ                        PIC 9(2)  VALUE ZERO.
004220    03 受理日Ｗ                        PIC 9(2)  VALUE ZERO.
004230 01 最終通院年月日Ｗ.
007390    03 最終通院和暦Ｗ                  PIC 9     VALUE ZERO.
004240    03 最終通院年Ｗ                    PIC 9(2)  VALUE ZERO.
004250    03 最終通院月Ｗ                    PIC 9(2)  VALUE ZERO.
004260    03 最終通院日Ｗ                    PIC 9(2)  VALUE ZERO.
004270** 月末日用
004280 01 施術西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
004290 01 商Ｗ                               PIC 9(3)  VALUE ZERO.
004300 01 余Ｗ                               PIC 9(3)  VALUE ZERO.
004310*
004320*--- 取引先銀行用 ---*
004330 01 銀行名支店名Ｗ.
004340    03 銀行名支店名１Ｗ                PIC X(26) VALUE SPACE.
004350    03 銀行名支店名２Ｗ                PIC X(34) VALUE SPACE.
004360 01 預金種別コメントＷ                 PIC X(4)  VALUE SPACE.
       01 支払機関Ｗ.
          03 金融機関名Ｗ.
             05 金融機関名１Ｗ            PIC X(8)  VALUE SPACE.
             05 金融機関名２Ｗ            PIC X(8)  VALUE SPACE.
             05 金融機関名３Ｗ            PIC X(8)  VALUE SPACE.
             05 金融機関名４Ｗ            PIC X(8)  VALUE SPACE.
             05 金融機関名５Ｗ            PIC X(8)  VALUE SPACE.
          03 支店名Ｗ.
             05 支店名１Ｗ                PIC X(12) VALUE SPACE.
             05 支店名２Ｗ                PIC X(12) VALUE SPACE.
             05 支店名３Ｗ                PIC X(12) VALUE SPACE.
             05 支店名４Ｗ                PIC X(12) VALUE SPACE.
          03 振込チェックＷ               PIC N(1)  VALUE SPACE.
          03 普通チェックＷ               PIC N(1)  VALUE SPACE.
          03 当座チェックＷ               PIC N(1)  VALUE SPACE.
          03 銀行チェックＷ               PIC N(1)  VALUE SPACE.
          03 金庫チェックＷ               PIC N(1)  VALUE SPACE.
          03 農協チェックＷ               PIC N(1)  VALUE SPACE.
          03 本店チェックＷ               PIC N(1)  VALUE SPACE.
          03 支店チェックＷ               PIC N(1)  VALUE SPACE.
          03 本支所チェックＷ             PIC N(1)  VALUE SPACE.
004370*
004380*-- レセ摘要用( N(38)固定）--*
004390 01 負傷の経過Ｗ.
004400    03 負傷の経過行Ｗ                  PIC X(76) OCCURS 2 VALUE SPACE.
004410 01 負傷の経過ＮＷ REDEFINES 負傷の経過Ｗ.
004420    03 負傷の経過行ＮＷ                PIC N(38) OCCURS 2.
004430*
004440*--- 給付割合用 ---*
004450 01 負担割合Ｗ                         PIC 9(2)  VALUE ZERO.
004460 01 給付割合Ｗ                         PIC 9(2)  VALUE ZERO.
004470 01 負担率Ｗ                           PIC 9(3)  VALUE ZERO.
004450 01 割合Ｗ                             PIC X(2)  VALUE SPACE.
004480*
004490*--- 請求先名称編集用 ---*
004500 01 請求先名称ＴＢＬ.
004510    03 請求先名称Ｔ                    PIC X(1)  OCCURS 40.
004520 01 桁位置Ｗ                           PIC S9(2) VALUE ZERO.
003750*
003751** 助成レセまとめ用
003752 01 助成レセまとめフラグ               PIC X(3)  VALUE SPACE.
003753*
      */金属副子・運動後療の変更・追加/1805
       01 金属副子ＣＭ                       PIC X(200) VALUE SPACE.
       01 運動後療ＣＭ                       PIC X(68)  VALUE SPACE.
004530*
      */長期頻回の追加/2407
       01 長期頻回Ｗ.
          03 長期頻回ＣＭ                    PIC X(280) VALUE SPACE.
          03 長期頻回１ＷＴ                  PIC X(54)  VALUE SPACE.
          03 長期頻回２ＷＴ                  PIC X(54)  VALUE SPACE.
          03 長期頻回３ＷＴ                  PIC X(54)  VALUE SPACE.
          03 長期頻回４ＷＴ                  PIC X(54)  VALUE SPACE.
          03 長期頻回５ＷＴ                  PIC X(54)  VALUE SPACE.
          03 負傷名ＷＲ                      OCCURS 5.
             05 負傷名ＷＰ                   PIC X(36)  VALUE SPACE.
          03 長期頻回ＣＭ２                  PIC X(280) VALUE SPACE.
       01 月数Ｗ                             PIC Z9     VALUE ZERO.
004730*
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
004540****************
004550* 連結項目待避 *
004560****************
004570*    ************
004580*    * 印刷キー *
004590*    ************
004600 01 対象データＷＲ.
004610    03 施術和暦年月ＷＲ.
004620       05 施術和暦ＷＲ                 PIC 9(1)  VALUE ZERO.
004630       05 施術年ＷＲ                   PIC 9(2)  VALUE ZERO.
004640       05 施術月ＷＲ                   PIC 9(2)  VALUE ZERO.
004650    03 保険種別ＷＲ                    PIC 9(2)  VALUE ZERO.
004660    03 保険者番号ＷＲ                  PIC X(10) VALUE SPACE.
004670    03 公費種別ＷＲ                    PIC 9(2)  VALUE ZERO.
004680    03 費用負担者番号ＷＲ              PIC X(10) VALUE SPACE.
004690    03 助成種別ＷＲ                    PIC 9(2)  VALUE ZERO.
004700    03 費用負担者番号助成ＷＲ          PIC X(10) VALUE SPACE.
004710    03 本人家族区分ＷＲ                PIC 9(1)  VALUE ZERO.
004720    03 患者カナＷＲ                    PIC X(20) VALUE SPACE.
004730    03 患者コードＷＲ.
004740       05 患者番号ＷＲ                 PIC 9(6)  VALUE ZERO.
004750       05 枝番ＷＲ                     PIC X(1)  VALUE SPACE.
004760*    ************
004770*    * 料金情報 *
004780*    ************
004790*--- 月毎の料金 ---*
004800 01 料金１ＷＲ.
004810    03 初検ＷＲ.
004820       05 負担割合ＷＲ                 PIC 9(3)  VALUE ZERO.
004830       05 初検料ＷＲ                   PIC 9(5)  VALUE ZERO.
004840       05 初検加算料ＷＲ               PIC 9(5)  VALUE ZERO.
          03 相談料ＷＲ                      PIC 9(4)  VALUE ZERO.
004850    03 再検料ＷＲ                      PIC 9(5)  VALUE ZERO.
004860    03 往療ＷＲ.
004870       05 往療距離ＷＲ                 PIC 9(2)V9 VALUE ZERO.
004880       05 往療回数ＷＲ                 PIC 9(2)  VALUE ZERO.
004890       05 往療料ＷＲ                   PIC 9(5)  VALUE ZERO.
004900       05 往療加算料ＷＲ               PIC 9(5)  VALUE ZERO.
004910    03 金属副子加算料ＷＲ              PIC 9(5)  VALUE ZERO.
004920    03 施術情報提供料ＷＲ              PIC 9(5)  VALUE ZERO.
004930    03 合計ＷＲ                        PIC 9(6)  VALUE ZERO.
004940    03 一部負担金ＷＲ                  PIC 9(6)  VALUE ZERO.
004950    03 請求金額ＷＲ                    PIC 9(6)  VALUE ZERO.
004960    03 給付割合ＷＲ                    PIC 9(1)  VALUE ZERO.
004970    03 受給者負担額ＷＲ                PIC 9(6)  VALUE ZERO.
004980    03 助成請求金額ＷＲ                PIC 9(6)  VALUE ZERO.
004990*
005000*--- 負傷部位毎の料金 ---*
005010 01 料金２ＷＲ.
005020   03 初回処置ＷＲ    OCCURS   9.
005030      05 初回処置料ＷＲ                PIC 9(5)  VALUE ZERO.
005040*
005050*--- 逓減毎の料金 ---*
005060 01 料金３ＷＲ.
005070**********
005080* １部位 *
005090**********
005100   03 部位１ＷＲ.
005110      05 後療１ＷＲ.
005120         07 後療単価１ＷＲ             PIC 9(4)  VALUE ZERO.
005130         07 後療回数１ＷＲ             PIC 9(2)  VALUE ZERO.
005140         07 後療料１ＷＲ               PIC 9(5)  VALUE ZERO.
005150      05 冷罨法１ＷＲ.
005160         07 冷罨法回数１ＷＲ           PIC 9(2)  VALUE ZERO.
005170         07 冷罨法料１ＷＲ             PIC 9(4)  VALUE ZERO.
005180      05 温罨法１ＷＲ.
005190         07 温罨法回数１ＷＲ           PIC 9(2)  VALUE ZERO.
005200         07 温罨法料１ＷＲ             PIC 9(4)  VALUE ZERO.
005210      05 電療１ＷＲ.
005220         07 電療回数１ＷＲ             PIC 9(2)  VALUE ZERO.
005230         07 電療料１ＷＲ               PIC 9(4)  VALUE ZERO.
005240      05 小計１ＷＲ                    PIC 9(6)  VALUE ZERO.
005250      05 長期逓減率１ＷＲ              PIC 9(3)  VALUE ZERO.
005260      05 長期込小計１ＷＲ              PIC 9(6)  VALUE ZERO.
005270**********
005280* ２部位 *
005290**********
005300   03 部位２ＷＲ.
005310      05 後療２ＷＲ.
005320         07 後療単価２ＷＲ             PIC 9(4)  VALUE ZERO.
005330         07 後療回数２ＷＲ             PIC 9(2)  VALUE ZERO.
005340         07 後療料２ＷＲ               PIC 9(5)  VALUE ZERO.
005350      05 冷罨法２ＷＲ.
005360         07 冷罨法回数２ＷＲ           PIC 9(2)  VALUE ZERO.
005370         07 冷罨法料２ＷＲ             PIC 9(4)  VALUE ZERO.
005380      05 温罨法２ＷＲ.
005390         07 温罨法回数２ＷＲ           PIC 9(2)  VALUE ZERO.
005400         07 温罨法料２ＷＲ             PIC 9(4)  VALUE ZERO.
005410      05 電療２ＷＲ.
005420         07 電療回数２ＷＲ             PIC 9(2)  VALUE ZERO.
005430         07 電療料２ＷＲ               PIC 9(4)  VALUE ZERO.
005440      05 小計２ＷＲ                    PIC 9(6)  VALUE ZERO.
005450      05 長期逓減率２ＷＲ              PIC 9(3)  VALUE ZERO.
005460      05 長期込小計２ＷＲ              PIC 9(6)  VALUE ZERO.
005470******************
005480* ３部位／８割 *
005490******************
005500   03 部位３８ＷＲ.
005510      05 後療３８ＷＲ.
005520         07 後療単価３８ＷＲ           PIC 9(4)  VALUE ZERO.
005530         07 後療回数３８ＷＲ           PIC 9(2)  VALUE ZERO.
005540         07 後療料３８ＷＲ             PIC 9(5)  VALUE ZERO.
005550      05 冷罨法３８ＷＲ.
005560         07 冷罨法回数３８ＷＲ         PIC 9(2)  VALUE ZERO.
005570         07 冷罨法料３８ＷＲ           PIC 9(4)  VALUE ZERO.
005580      05 温罨法３８ＷＲ.
005590         07 温罨法回数３８ＷＲ         PIC 9(2)  VALUE ZERO.
005600         07 温罨法料３８ＷＲ           PIC 9(4)  VALUE ZERO.
005610      05 電療３８ＷＲ.
005620         07 電療回数３８ＷＲ           PIC 9(2)  VALUE ZERO.
005630         07 電療料３８ＷＲ             PIC 9(4)  VALUE ZERO.
005640      05 小計３８ＷＲ                  PIC 9(6)  VALUE ZERO.
005650      05 多部位込小計３８ＷＲ          PIC 9(6)  VALUE ZERO.
005660      05 長期逓減率３８ＷＲ            PIC 9(3)  VALUE ZERO.
005670      05 長期込小計３８ＷＲ            PIC 9(6)  VALUE ZERO.
005680******************
005690* ３部位／１０割 *
005700******************
005710   03 部位３０ＷＲ.
005720      05 逓減開始月日３０ＷＲ.
005730         07 逓減開始月３０ＷＲ         PIC 9(2)  VALUE ZERO.
005740         07 逓減開始日３０ＷＲ         PIC 9(2)  VALUE ZERO.
005750      05 後療３０ＷＲ.
005760         07 後療単価３０ＷＲ           PIC 9(4)  VALUE ZERO.
005770         07 後療回数３０ＷＲ           PIC 9(2)  VALUE ZERO.
005780         07 後療料３０ＷＲ             PIC 9(5)  VALUE ZERO.
005790      05 冷罨法３０ＷＲ.
005800         07 冷罨法回数３０ＷＲ         PIC 9(2)  VALUE ZERO.
005810         07 冷罨法料３０ＷＲ           PIC 9(4)  VALUE ZERO.
005820      05 温罨法３０ＷＲ.
005830         07 温罨法回数３０ＷＲ         PIC 9(2)  VALUE ZERO.
005840         07 温罨法料３０ＷＲ           PIC 9(4)  VALUE ZERO.
005850      05 電療３０ＷＲ.
005860         07 電療回数３０ＷＲ           PIC 9(2)  VALUE ZERO.
005870         07 電療料３０ＷＲ             PIC 9(4)  VALUE ZERO.
005880      05 小計３０ＷＲ                  PIC 9(6)  VALUE ZERO.
005890      05 長期逓減率３０ＷＲ            PIC 9(3)  VALUE ZERO.
005900      05 長期込小計３０ＷＲ            PIC 9(6)  VALUE ZERO.
005910****************
005920* ４部位／５割 *
005930****************
005940   03 部位４５ＷＲ.
005950      05 後療４５ＷＲ.
005960         07 後療単価４５ＷＲ           PIC 9(4)  VALUE ZERO.
005970         07 後療回数４５ＷＲ           PIC 9(2)  VALUE ZERO.
005980         07 後療料４５ＷＲ             PIC 9(5)  VALUE ZERO.
005990      05 冷罨法４５ＷＲ.
006000         07 冷罨法回数４５ＷＲ         PIC 9(2)  VALUE ZERO.
006010         07 冷罨法料４５ＷＲ           PIC 9(4)  VALUE ZERO.
006020      05 温罨法４５ＷＲ.
006030         07 温罨法回数４５ＷＲ         PIC 9(2)  VALUE ZERO.
006040         07 温罨法料４５ＷＲ           PIC 9(4)  VALUE ZERO.
006050      05 電療４５ＷＲ.
006060         07 電療回数４５ＷＲ           PIC 9(2)  VALUE ZERO.
006070         07 電療料４５ＷＲ             PIC 9(4)  VALUE ZERO.
006080      05 小計４５ＷＲ                  PIC 9(6)  VALUE ZERO.
006090      05 多部位込小計４５ＷＲ          PIC 9(6)  VALUE ZERO.
006100      05 長期逓減率４５ＷＲ            PIC 9(3)  VALUE ZERO.
006110      05 長期込小計４５ＷＲ            PIC 9(6)  VALUE ZERO.
006120****************
006130* ４部位／８割 *
006140****************
006150   03 部位４８ＷＲ.
006160      05 逓減開始月日４８ＷＲ.
006170         07 逓減開始月４８ＷＲ         PIC 9(2)  VALUE ZERO.
006180         07 逓減開始日４８ＷＲ         PIC 9(2)  VALUE ZERO.
006190      05 後療４８ＷＲ.
006200         07 後療単価４８ＷＲ           PIC 9(4)  VALUE ZERO.
006210         07 後療回数４８ＷＲ           PIC 9(2)  VALUE ZERO.
006220         07 後療料４８ＷＲ             PIC 9(5)  VALUE ZERO.
006230      05 冷罨法４８ＷＲ.
006240         07 冷罨法回数４８ＷＲ         PIC 9(2)  VALUE ZERO.
006250         07 冷罨法料４８ＷＲ           PIC 9(4)  VALUE ZERO.
006260      05 温罨法４８ＷＲ.
006270         07 温罨法回数４８ＷＲ         PIC 9(2)  VALUE ZERO.
006280         07 温罨法料４８ＷＲ           PIC 9(4)  VALUE ZERO.
006290      05 電療４８ＷＲ.
006300         07 電療回数４８ＷＲ           PIC 9(2)  VALUE ZERO.
006310         07 電療料４８ＷＲ             PIC 9(4)  VALUE ZERO.
006320      05 小計４８ＷＲ                  PIC 9(6)  VALUE ZERO.
006330      05 多部位込小計４８ＷＲ          PIC 9(6)  VALUE ZERO.
006340      05 長期逓減率４８ＷＲ            PIC 9(3)  VALUE ZERO.
006350      05 長期込小計４８ＷＲ            PIC 9(6)  VALUE ZERO.
006360******************
006370* ４部位／１０割 *
006380******************
006390   03 部位４０ＷＲ.
006400      05 逓減開始月日４０ＷＲ.
006410         07 逓減開始月４０ＷＲ         PIC 9(2)  VALUE ZERO.
006420         07 逓減開始日４０ＷＲ         PIC 9(2)  VALUE ZERO.
006430      05 後療４０ＷＲ.
006440         07 後療単価４０ＷＲ           PIC 9(4)  VALUE ZERO.
006450         07 後療回数４０ＷＲ           PIC 9(2)  VALUE ZERO.
006460         07 後療料４０ＷＲ             PIC 9(5)  VALUE ZERO.
006470      05 冷罨法４０ＷＲ.
006480         07 冷罨法回数４０ＷＲ         PIC 9(2)  VALUE ZERO.
006490         07 冷罨法料４０ＷＲ           PIC 9(4)  VALUE ZERO.
006500      05 温罨法４０ＷＲ.
006510         07 温罨法回数４０ＷＲ         PIC 9(2)  VALUE ZERO.
006520         07 温罨法料４０ＷＲ           PIC 9(4)  VALUE ZERO.
006530      05 電療４０ＷＲ.
006540         07 電療回数４０ＷＲ           PIC 9(2)  VALUE ZERO.
006550         07 電療料４０ＷＲ             PIC 9(4)  VALUE ZERO.
006560      05 小計４０ＷＲ                  PIC 9(6)  VALUE ZERO.
006570      05 長期逓減率４０ＷＲ            PIC 9(3)  VALUE ZERO.
006580      05 長期込小計４０ＷＲ            PIC 9(6)  VALUE ZERO.
006590********************
006600* ５部位／２．５割 *
006610********************
006620   03 部位５２ＷＲ.
006630      05 後療５２ＷＲ.
006640         07 後療単価５２ＷＲ           PIC 9(4)  VALUE ZERO.
006650         07 後療回数５２ＷＲ           PIC 9(2)  VALUE ZERO.
006660         07 後療料５２ＷＲ             PIC 9(5)  VALUE ZERO.
006670      05 冷罨法５２ＷＲ.
006680         07 冷罨法回数５２ＷＲ         PIC 9(2)  VALUE ZERO.
006690         07 冷罨法料５２ＷＲ           PIC 9(4)  VALUE ZERO.
006700      05 温罨法５２ＷＲ.
006710         07 温罨法回数５２ＷＲ         PIC 9(2)  VALUE ZERO.
006720         07 温罨法料５２ＷＲ           PIC 9(4)  VALUE ZERO.
006730      05 電療５２ＷＲ.
006740         07 電療回数５２ＷＲ           PIC 9(2)  VALUE ZERO.
006750         07 電療料５２ＷＲ             PIC 9(4)  VALUE ZERO.
006760      05 小計５２ＷＲ                  PIC 9(6)  VALUE ZERO.
006770      05 多部位込小計５２ＷＲ          PIC 9(6)  VALUE ZERO.
006780      05 長期逓減率５２ＷＲ            PIC 9(3)  VALUE ZERO.
006790      05 長期込小計５２ＷＲ            PIC 9(6)  VALUE ZERO.
006800****************
006810* ５部位／５割 *
006820****************
006830   03 部位５５ＷＲ.
006840      05 逓減開始月日５５ＷＲ.
006850         07 逓減開始月５５ＷＲ         PIC 9(2)  VALUE ZERO.
006860         07 逓減開始日５５ＷＲ         PIC 9(2)  VALUE ZERO.
006870      05 後療５５ＷＲ.
006880         07 後療単価５５ＷＲ           PIC 9(4)  VALUE ZERO.
006890         07 後療回数５５ＷＲ           PIC 9(2)  VALUE ZERO.
006900         07 後療料５５ＷＲ             PIC 9(5)  VALUE ZERO.
006910      05 冷罨法５５ＷＲ.
006920         07 冷罨法回数５５ＷＲ         PIC 9(2)  VALUE ZERO.
006930         07 冷罨法料５５ＷＲ           PIC 9(4)  VALUE ZERO.
006940      05 温罨法５５ＷＲ.
006950         07 温罨法回数５５ＷＲ         PIC 9(2)  VALUE ZERO.
006960         07 温罨法料５５ＷＲ           PIC 9(4)  VALUE ZERO.
006970      05 電療５５ＷＲ.
006980         07 電療回数５５ＷＲ           PIC 9(2)  VALUE ZERO.
006990         07 電療料５５ＷＲ             PIC 9(4)  VALUE ZERO.
007000      05 小計５５ＷＲ                  PIC 9(6)  VALUE ZERO.
007010      05 多部位込小計５５ＷＲ          PIC 9(6)  VALUE ZERO.
007020      05 長期逓減率５５ＷＲ            PIC 9(3)  VALUE ZERO.
007030      05 長期込小計５５ＷＲ            PIC 9(6)  VALUE ZERO.
007040****************
007050* ５部位／８割 *
007060****************
007070   03 部位５８ＷＲ.
007080      05 逓減開始月日５８ＷＲ.
007090         07 逓減開始月５８ＷＲ         PIC 9(2)  VALUE ZERO.
007100         07 逓減開始日５８ＷＲ         PIC 9(2)  VALUE ZERO.
007110      05 後療５８ＷＲ.
007120         07 後療単価５８ＷＲ           PIC 9(4)  VALUE ZERO.
007130         07 後療回数５８ＷＲ           PIC 9(2)  VALUE ZERO.
007140         07 後療料５８ＷＲ             PIC 9(5)  VALUE ZERO.
007150      05 冷罨法５８ＷＲ.
007160         07 冷罨法回数５８ＷＲ         PIC 9(2)  VALUE ZERO.
007170         07 冷罨法料５８ＷＲ           PIC 9(4)  VALUE ZERO.
007180      05 温罨法５８ＷＲ.
007190         07 温罨法回数５８ＷＲ         PIC 9(2)  VALUE ZERO.
007200         07 温罨法料５８ＷＲ           PIC 9(4)  VALUE ZERO.
007210      05 電療５８ＷＲ.
007220         07 電療回数５８ＷＲ           PIC 9(2)  VALUE ZERO.
007230         07 電療料５８ＷＲ             PIC 9(4)  VALUE ZERO.
007240      05 小計５８ＷＲ                  PIC 9(6)  VALUE ZERO.
007250      05 多部位込小計５８ＷＲ          PIC 9(6)  VALUE ZERO.
007260      05 長期逓減率５８ＷＲ            PIC 9(3)  VALUE ZERO.
007270      05 長期込小計５８ＷＲ            PIC 9(6)  VALUE ZERO.
007280******************
007290* ５部位／１０割 *
007300******************
007310   03 部位５０ＷＲ.
007320      05 逓減開始月日５０ＷＲ.
007330         07 逓減開始月５０ＷＲ         PIC 9(2)  VALUE ZERO.
007340         07 逓減開始日５０ＷＲ         PIC 9(2)  VALUE ZERO.
007350      05 後療５０ＷＲ.
007360         07 後療単価５０ＷＲ           PIC 9(4)  VALUE ZERO.
007370         07 後療回数５０ＷＲ           PIC 9(2)  VALUE ZERO.
007380         07 後療料５０ＷＲ             PIC 9(5)  VALUE ZERO.
007390      05 冷罨法５０ＷＲ.
007400         07 冷罨法回数５０ＷＲ         PIC 9(2)  VALUE ZERO.
007410         07 冷罨法料５０ＷＲ           PIC 9(4)  VALUE ZERO.
007420      05 温罨法５０ＷＲ.
007430         07 温罨法回数５０ＷＲ         PIC 9(2)  VALUE ZERO.
007440         07 温罨法料５０ＷＲ           PIC 9(4)  VALUE ZERO.
007450      05 電療５０ＷＲ.
007460         07 電療回数５０ＷＲ           PIC 9(2)  VALUE ZERO.
007470         07 電療料５０ＷＲ             PIC 9(4)  VALUE ZERO.
007480      05 小計５０ＷＲ                  PIC 9(6)  VALUE ZERO.
007490      05 長期逓減率５０ＷＲ            PIC 9(3)  VALUE ZERO.
007500      05 長期込小計５０ＷＲ            PIC 9(6)  VALUE ZERO.
008000*******************
008010*  明細書発行加算 */202206
008020*******************
008030   03 明細書発行加算料ＷＲ                PIC ZZZ   VALUE ZERO.
008030   03 明細書発行加算日ＷＲ                PIC ZZ    VALUE ZERO.
007510*
007520**************
007530* 施術所情報 *
007540**************
007550 01 施術所情報Ｗ.
007560    03 柔整師番号Ｗ                    PIC X(16) VALUE SPACE.
007570    03 接骨師会会員番号Ｗ              PIC X(16) VALUE SPACE.
007580    03 代表者カナＷ                    PIC X(50) VALUE SPACE.
007590    03 代表者名Ｗ                      PIC X(50) VALUE SPACE.
007600    03 接骨院名Ｗ                      PIC X(50) VALUE SPACE.
          03 都道府県ＪＩＳＷ                PIC X(2)   VALUE SPACE.
007610    03 施術所郵便番号Ｗ.
007620       05 施術所郵便番号１Ｗ           PIC X(3)  VALUE SPACE.
007630       05 施術所郵便番号２Ｗ           PIC X(4)  VALUE SPACE.
007640    03 施術所住所Ｗ.
007650       05 施術所住所１Ｗ               PIC X(50) VALUE SPACE.
007660       05 施術所住所２Ｗ               PIC X(50) VALUE SPACE.
007670    03 施術所電話番号Ｗ                PIC X(15) VALUE SPACE.
007680    03 接骨師会会長名Ｗ                PIC N(10) VALUE SPACE.
007690    03 取引先情報Ｗ.
007700        05 取引先銀行名Ｗ              PIC X(40) VALUE SPACE.
007710        05 取引先銀行支店名Ｗ          PIC X(40) VALUE SPACE.
007720        05 預金種別Ｗ                  PIC 9(1)  VALUE ZERO.
007730        05 口座番号Ｗ                  PIC X(10) VALUE SPACE.
007740        05 口座名義人Ｗ                PIC X(40) VALUE SPACE.
007750        05 口座名義人カナＷ            PIC X(40) VALUE SPACE.
007760    03 定額制受理番号Ｗ                PIC X(15) VALUE SPACE.
007770    03 柔整師年月日Ｗ.
007350       05 柔整師和暦Ｗ                 PIC 9      VALUE ZERO.
007780       05 柔整師年Ｗ                   PIC 9(2)  VALUE ZERO.
007790       05 柔整師月Ｗ                   PIC 9(2)  VALUE ZERO.
007800       05 柔整師日Ｗ                   PIC 9(2)  VALUE ZERO.
007810    03 患者委任年月日Ｗ.
007350       05 患者委任和暦Ｗ               PIC 9      VALUE ZERO.
007820       05 患者委任年Ｗ                 PIC 9(2)  VALUE ZERO.
007830       05 患者委任月Ｗ                 PIC 9(2)  VALUE ZERO.
007840       05 患者委任日Ｗ                 PIC 9(2)  VALUE ZERO.
007850    03 県施術ＩＤＷ                    PIC X(15) VALUE SPACE.
007860    03 市町村施術ＩＤＷ                PIC X(15) VALUE SPACE.
007330    03 共済番号Ｗ                      PIC X(28)  VALUE SPACE.
007860    03 会長委任文１Ｗ                  PIC X(50) VALUE SPACE.
007860    03 会長委任文２Ｗ                  PIC X(50) VALUE SPACE.
007860    03 会長委任文３Ｗ                  PIC X(50) VALUE SPACE.
007860    03 会長委任文４Ｗ                  PIC X(50) VALUE SPACE.
007850    03 県番号Ｗ                        PIC X(2) VALUE SPACE.
002600** 会員番号右詰め用
002610 01 会員番号ＷＴ.
002620    03 会員番号左詰めＷ.
002630      05 会員番号左詰めＷ１            PIC X OCCURS 8 VALUE SPACE.
002640    03 会員番号右詰めＷ.
002650      05 会員番号右詰めＷ１            PIC X OCCURS 8 VALUE SPACE.
007870**************
007880* 受診者情報 *
007890**************
007900 01 受診者情報Ｗ.
      */元号修正/20190426
          03 施術和暦Ｗ                      PIC 9(1)   VALUE ZERO.
007910    03 施術年月Ｗ.
007920       05 施術年Ｗ                     PIC 9(2)  VALUE ZERO.
007930       05 施術月Ｗ                     PIC 9(2)  VALUE ZERO.
007390    03 保険種別チェックＷ.
007400       05 社保チェックＷ               PIC N(1)  VALUE SPACE.
007410       05 船員チェックＷ               PIC N(1)  VALUE SPACE.
007420       05 組合チェックＷ               PIC N(1)  VALUE SPACE.
007430       05 国保チェックＷ               PIC N(1)  VALUE SPACE.
             05 共済チェックＷ               PIC N(1)  VALUE SPACE.
             05 自チェックＷ                 PIC N(1)  VALUE SPACE.
             05 退職チェックＷ               PIC N(1)  VALUE SPACE.
             05 後期チェックＷ               PIC N(1)  VALUE SPACE.
          03 本人チェックＷ                  PIC N(1)   VALUE SPACE.
          03 家族チェックＷ                  PIC N(1)   VALUE SPACE.
          03 単独チェックＷ                  PIC N(1)   VALUE SPACE.
          03 ２併チェックＷ                  PIC N(1)   VALUE SPACE.
          03 高一チェックＷ                  PIC N(1)   VALUE SPACE.
          03 高７チェックＷ                  PIC N(1)   VALUE SPACE.
          03 ６歳チェックＷ                  PIC N(1)   VALUE SPACE.
          03 給付割合チェックＷ.
             05 ７割チェックＷ               PIC N(1)  VALUE SPACE.
             05 ８割チェックＷ               PIC N(1)  VALUE SPACE.
             05 ９割チェックＷ               PIC N(1)  VALUE SPACE.
             05 １０割チェックＷ             PIC N(1)  VALUE SPACE.
007940*    03 記号Ｗ                          PIC N(12) VALUE SPACE.
007570    03 記号Ｗ.
007580       05 印刷記号Ｗ                   PIC N(12)  VALUE SPACE.
007950    03 番号Ｗ.
007960       05 印刷番号Ｗ                   PIC X(15) VALUE SPACE.
007970       05 FILLER                       PIC X(15) VALUE SPACE.
          03 記号番号Ｗ.
             05 記号番号ＸＷ                 PIC X(40) VALUE SPACE.
007980    03 保険者番号Ｗ                    PIC X(10) VALUE SPACE.
007990    03 保険者名称Ｗ.
008000       05 保険者名称１Ｗ               PIC X(30) VALUE SPACE.
008010       05 保険者名称２Ｗ               PIC X(30) VALUE SPACE.
008020       05 FILLER                       PIC X(20) VALUE SPACE.
007340    03 請求先名称Ｗ                    PIC X(56) VALUE SPACE.
008030*    03 請求先名称Ｗ.
008040*       05 請求先名称１Ｗ               PIC X(30) VALUE SPACE.
008050*       05 請求先名称２Ｗ               PIC X(30) VALUE SPACE.
008060*       05 FILLER                       PIC X(20) VALUE SPACE.
          03 公費負担者番号Ｗ                PIC X(8)   VALUE SPACE.
          03 受給者番号Ｗ.
             05 印刷受給者番号Ｗ             PIC X(7)  VALUE SPACE.
             05 印刷受給者番号２Ｗ           PIC X(8)  VALUE SPACE.
008070    03 請求先名称チェック.
008080       05 市チェックＷ                 PIC N(1)  VALUE SPACE.
008090       05 町チェックＷ                 PIC N(1)  VALUE SPACE.
008100       05 村チェックＷ                 PIC N(1)  VALUE SPACE.
008110       05 国組チェックＷ               PIC N(1)  VALUE SPACE.
008120    03 被保険者情報Ｗ.
008130       05 被保険者カナＷ               PIC X(50) VALUE SPACE.
008140       05 被保険者氏名Ｗ               PIC X(50) VALUE SPACE.
008150       05 郵便番号Ｗ.
008160          07 郵便番号１Ｗ              PIC X(3)  VALUE SPACE.
008170          07 郵便番号２Ｗ              PIC X(4)  VALUE SPACE.
008180       05 被保険者住所１Ｗ             PIC X(50) VALUE SPACE.
008190       05 被保険者住所２Ｗ             PIC X(50) VALUE SPACE.
008990       05 電話番号Ｗ                   PIC X(35)  VALUE SPACE.
008200    03 患者情報Ｗ.
008210       05 患者カナＷ                   PIC X(50) VALUE SPACE.
008220       05 患者氏名Ｗ                   PIC X(50) VALUE SPACE.
008230       05 患者性別Ｗ.
008240          07 性別Ｗ                    PIC N(1)  VALUE SPACE.
008250       05 性別チェックＷ.
008260          07 男チェックＷ              PIC N(1)  VALUE SPACE.
008270          07 女チェックＷ              PIC N(1)  VALUE SPACE.
008280       05 患者和暦Ｗ                   PIC 9(1)  VALUE ZERO.
008290       05 患者和暦名称Ｗ               PIC N(2)  VALUE SPACE.
008300       05 和暦チェックＷ.
008310          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
008320          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
008330          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
008340          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
      */元号修正/↓↓↓20190426
008210          07 令和チェックＷ            PIC N(1)  VALUE SPACE.
                07 令和ＣＭＷ                PIC X(4)  VALUE SPACE.
009110          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
      */元号修正/↑↑↑20190426
008350       05 患者年Ｗ                     PIC 9(2)  VALUE ZERO.
008360       05 患者月Ｗ                     PIC 9(2)  VALUE ZERO.
008370       05 患者日Ｗ                     PIC 9(2)  VALUE ZERO.
008380       05 患者郵便番号Ｗ.
008390          07 患者郵便番号１Ｗ          PIC X(3)  VALUE SPACE.
008400          07 患者郵便番号２Ｗ          PIC X(4)  VALUE SPACE.
008410       05 患者住所１Ｗ                 PIC X(50) VALUE SPACE.
008420       05 患者住所２Ｗ                 PIC X(50) VALUE SPACE.
008430       05 続柄Ｗ.
008440          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
008450          07 FILLER                    PIC X(4)  VALUE SPACE.
008430*       05 続柄チェックＷ.
008260*          07 本人チェックＷ            PIC N(1)  VALUE SPACE.
008260*          07 家族チェックＷ            PIC N(1)  VALUE SPACE.
008460*
008470*       05 負傷原因Ｗ                   PIC N(40) OCCURS 34 VALUE SPACE.
      */半角対応/110421
             05 負傷原因Ｗ OCCURS 29.
                07 負傷原因ＸＷ              PIC X(100)  VALUE SPACE.
008480*
008490    03 助成印Ｗ                        PIC N(1)  VALUE SPACE.
008500    03 特別コメントＷ                  PIC X(16) VALUE SPACE.
008490    03 保険種別Ｗ                      PIC X(4)  VALUE SPACE.
008490    03 助成種別Ｗ                      PIC X(10) VALUE SPACE.
008490    03 保険種別ＣＭＷ                  PIC X(16) VALUE SPACE.
008490    03 県ＣＭＷ.
             05 県ＣＭＷＰ                   PIC X(8) VALUE SPACE.
          03 タイトルＷ                      PIC X(20) VALUE SPACE.
          03 取消線Ｗ                        PIC X(60) VALUE SPACE.
008510*
008520****************
008530* 負傷データＦ *
008540****************
008550 01 負傷情報Ｗ.
008560    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
008570    03 部位情報Ｗ  OCCURS   9.
008580       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
008590       05 部位コードＷ.
008600          07 負傷種別Ｗ                PIC 9(2)  VALUE ZERO.
008610          07 部位Ｗ                    PIC 9(2)  VALUE ZERO.
008620          07 左右区分Ｗ                PIC 9(1)  VALUE ZERO.
008630          07 負傷位置番号Ｗ            PIC 9(2)  VALUE ZERO.
008640       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
008650       05 負傷年月日Ｗ.
008660          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
008670          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
008680          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
008690       05 初検年月日Ｗ.
008700          07 初検年Ｗ                  PIC 9(2)  VALUE ZERO.
008710          07 初検月Ｗ                  PIC 9(2)  VALUE ZERO.
008720          07 初検日Ｗ                  PIC 9(2)  VALUE ZERO.
008730       05 開始年月日Ｗ.
008740          07 開始年Ｗ                  PIC 9(2)  VALUE ZERO.
008750          07 開始月Ｗ                  PIC 9(2)  VALUE ZERO.
008760          07 開始日Ｗ                  PIC 9(2)  VALUE ZERO.
008770       05 終了年月日Ｗ.
002980          07 終了和暦Ｗ                PIC 9     VALUE ZERO.
008780          07 終了年Ｗ                  PIC 9(2)  VALUE ZERO.
008790          07 終了月Ｗ                  PIC 9(2)  VALUE ZERO.
008800          07 終了日Ｗ                  PIC 9(2)  VALUE ZERO.
008810       05 実日数Ｗ                     PIC 9(2)  VALUE ZERO.
008820       05 転帰区分Ｗ                   PIC 9(1)  VALUE ZERO.
008830       05 転帰区分チェックＷ.
008840          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
008850          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
008860          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
008870       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
008880       05 部位区切Ｗ                   PIC X(1)  VALUE SPACE.
008890       05 経過略称Ｗ.
008900          07 印刷経過略称Ｗ            PIC N(10) VALUE SPACE.
008910          07 FILLER                    PIC X(2)  VALUE SPACE.
008920    03 新規チェックＷ                  PIC N(1)  VALUE SPACE.
008930    03 継続チェックＷ                  PIC N(1)  VALUE SPACE.
          03 施術日Ｗ.
             05 施術日チェックＷ   OCCURS 31 PIC N(1)  VALUE SPACE.
008940*
008950************
008960* 料金情報 *
008970************
008980 01 料金情報Ｗ.
008990    03 初検加算Ｗ.
009000       05 時間外チェックＷ             PIC N(1)  VALUE SPACE.
009010       05 休日チェックＷ               PIC N(1)  VALUE SPACE.
009020       05 深夜チェックＷ               PIC N(1)  VALUE SPACE.
009030    03 往療加算Ｗ.
009040       05 夜間チェックＷ               PIC N(1)  VALUE SPACE.
009050       05 暴風雨雪チェックＷ           PIC N(1)  VALUE SPACE.
009060       05 難路チェックＷ               PIC N(1)  VALUE SPACE.
009070       05 往療加算回数Ｗ               PIC 9(2)  VALUE ZERO.
009080    03 金属副子チェックＷ.
009090       05 大チェックＷ                 PIC N(1)  VALUE SPACE.
009100       05 中チェックＷ                 PIC N(1)  VALUE SPACE.
009110       05 小チェックＷ                 PIC N(1)  VALUE SPACE.
009120    03 小計Ｗ                          PIC 9(7)  VALUE ZERO.
009130    03 初回処置料合計Ｗ                PIC 9(6)  VALUE ZERO.
009140    03 初回処置料チェックＷ.
009150       05 整復料チェックＷ             PIC N(1)  VALUE SPACE.
009160       05 固定料チェックＷ             PIC N(1)  VALUE SPACE.
009170       05 施療料チェックＷ             PIC N(1)  VALUE SPACE.
      */金属副子・運動後療の変更・追加/1805
          03 金属回数Ｗ                         PIC 9(2)  VALUE ZERO.
          03 運動回数Ｗ                         PIC 9(1)  VALUE ZERO.
          03 運動料Ｗ                           PIC 9(4)  VALUE ZERO.
009180*
009280************
009290* 備考情報 *
009300************
009310 01 備考情報Ｗ.
010010    03 適用１Ｗ                        PIC N(48) VALUE SPACE.
010020    03 適用２Ｗ                        PIC X(40) VALUE SPACE.
009340*    03 適用３Ｗ                        PIC N(38) VALUE SPACE.
009350*    03 適用４Ｗ                        PIC N(38) VALUE SPACE.
009360    03 経過コメントＷ                  PIC N(60) VALUE SPACE.
009370*****************
009380* レセプト並び順 *
009390*****************
009400 01 順番Ｗ                             PIC 9(4) VALUE ZERO.
009410*
       01 摘要施術日Ｗ                       PIC X(100) VALUE SPACE.
       01 施術日Ｗ.
          03 施術日２Ｗ                      PIC X(1)  VALUE SPACE.
          03 施術日１Ｗ                      PIC X(1)  VALUE SPACE.
       01 レイアウトＷ.
004750    03 請求西暦年月ＷＱ.
004720       05 請求西暦年ＷＱ               PIC 9(4) VALUE ZERO.
004770       05 請求月ＷＱ                   PIC 9(2) VALUE ZERO.
004770    03 会員番号ＷＱ                    PIC 9(8) VALUE ZERO.
004770    03 保険番号ＷＱ                    PIC X(8) VALUE ZERO.
004770    03 公費負担者番号ＷＱ              PIC X(8) VALUE ZERO.
004770    03 医療助成区分ＷＱ                PIC 9(1) VALUE ZERO.
004770    03 本人家族ＷＱ                    PIC 9(1) VALUE ZERO.
004750    03 施術西暦年月ＷＱ.
004720       05 施術西暦年ＷＱ               PIC 9(4) VALUE ZERO.
004770       05 施術月ＷＱ                   PIC 9(2) VALUE ZERO.
004770    03 費用額ＷＱ                      PIC 9(6) VALUE ZERO.
004770    03 負担額ＷＱ                      PIC 9(6) VALUE ZERO.
004770    03 請求額ＷＱ                      PIC 9(6) VALUE ZERO.
004770    03 実日数ＷＱ                      PIC 9(2) VALUE ZERO.
004770    03 部位数ＷＱ                      PIC 9(1) VALUE ZERO.
004730    03 患者コードＷＱ.
004740       05 患者番号ＷＱ                 PIC 9(6)  VALUE ZERO.
004750       05 枝番ＷＱ                     PIC X(1)  VALUE SPACE.
             05 FILLER                       PIC X(1)  VALUE SPACE.
004770 01 カンマＷＱ                         PIC X(1) VALUE ",".
008140 01 被保険者名ＷＱ                     PIC X(20) VALUE SPACE.
008140 01 受診者名ＷＱ                       PIC X(20) VALUE SPACE.
       01 ＱＲデータＷ                       PIC X(109) VALUE SPACE.
009420*-----------------------------------------------------------------------*
009430 01 印刷制御.
009440     03 定義体名Ｐ                     PIC X(8)  VALUE SPACE.
009450     03 項目群名Ｐ                     PIC X(8)  VALUE SPACE.
009460     03 処理種別Ｐ                     PIC X(2)  VALUE SPACE.
009470     03 拡張制御Ｐ.
009480         05 端末制御Ｐ.
009490             07 移動方向Ｐ             PIC X(1)  VALUE SPACE.
009500             07 移動行数Ｐ             PIC 9(3)  VALUE ZERO.
009510         05 詳細制御Ｐ                 PIC X(2)  VALUE SPACE.
009520     03 通知情報Ｐ                     PIC X(2)  VALUE SPACE.
009530     03 ユニット名Ｐ                   PIC X(8)  VALUE SPACE.
009540*-----------------------------------------------------------------------*
009700*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
009710******************************************************************
009720*                          連結項目                              *
009730******************************************************************
009740******************
009750* 画面入力データ *
009760******************
       01 連入－プレビュー IS EXTERNAL.
          03 連入－プレビュー区分          PIC 9.
       01 連入－入力データ電話印刷 IS EXTERNAL.
          03 連入－電話印刷                     PIC 9.
010440*
009770 01 連入－入力データ委任印刷 IS EXTERNAL.
009780    03 連入－委任印刷                  PIC 9.
009790*
009800************
009810* 印刷キー *
009820************
009830 01 連レ印－対象データ IS EXTERNAL.
009840    03 連レ印－施術年月日.
009850       05 連レ印－施術和暦             PIC 9(1).
009860       05 連レ印－施術年               PIC 9(2).
009870       05 連レ印－施術月               PIC 9(2).
009880    03 連レ印－患者コード.
009890       05 連レ印－患者番号             PIC 9(6).
009900       05 連レ印－枝番                 PIC X(1).
009910    03 連レ印－保険種別                PIC 9(2).
009920    03 連レ印－保険者番号              PIC X(10).
009930    03 連レ印－公費種別                PIC 9(2).
009940    03 連レ印－費用負担者番号          PIC X(10).
009950    03 連レ印－助成種別                PIC 9(2).
009960    03 連レ印－費用負担者番号助成      PIC X(10).
009970    03 連レ印－患者カナ                PIC X(20).
009980    03 連レ印－本人家族区分            PIC 9(1).
009990*
013420 01 連レ－キー IS EXTERNAL.
013430    03 連レ－保険種別                  PIC 9(2).
013440*
013450******************
013460* ３カ月長期判定 *
013470******************
013480 01 連期間－キー IS EXTERNAL.
013490    03 連期間－施術年月.
013500       05 連期間－施術和暦             PIC 9.
013510       05 連期間－施術年               PIC 9(2).
013520       05 連期間－施術月               PIC 9(2).
013530    03  連期間－患者コード.
013540       05 連期間－患者番号             PIC 9(6).
013550       05 連期間－枝番                 PIC X.
013560    03 連期間－対象フラグ              PIC X(3).
013570    03 連期間－期間月Ｗ.
013580       05 連期間－期間Ｗ               PIC 9(2) OCCURS 9.
013590*
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
013620*
013600************************
013610* 長期理由文セット     *
013620************************
013630 01 連長文－キー IS EXTERNAL.
013640    03 連長文－施術年月.
013650       05 連長文－施術和暦             PIC 9.
013660       05 連長文－施術年               PIC 9(2).
013670       05 連長文－施術月               PIC 9(2).
013680    03  連長文－患者コード.
013690       05 連長文－患者番号             PIC 9(6).
013700       05 連長文－枝番                 PIC X.
013710    03 連長文－文桁数                  PIC 9(2).
013720    03 連長文－理由文                  PIC N(63) OCCURS 15.
013730*
013740* 負担率取得用14/10～
013750 01 連率－負担率取得キー IS EXTERNAL.
013760    03 連率－施術和暦年月.
013770       05 連率－施術和暦               PIC 9.
013780       05 連率－施術年月.
013790          07 連率－施術年              PIC 9(2).
013800          07 連率－施術月              PIC 9(2).
013810    03 連率－患者コード.
013820       05 連率－患者番号               PIC 9(6).
013830       05 連率－枝番                   PIC X.
013840    03 連率－実際負担率                PIC 9(3).
013850    03 連率－実際本体負担率            PIC 9(3).
013860    03 連率－健保負担率                PIC 9(3).
013870    03 連率－２７老負担率              PIC 9(3).
013880    03 連率－助成負担率                PIC 9(3).
013890    03 連率－特別用負担率              PIC 9(3).
013900*
013163*************
013164* 助成名称
013165*************
013166 01 連助成名称－キー IS EXTERNAL.
013167    03 連助成名称－助成種別             PIC 9(2).
013168    03 連助成名称－費用負担者番号助成   PIC X(10).
013169*   / OUT /
013170    03 連助成名称－名称集団.
013171       05 連助成名称－１文字            PIC N.
013172       05 連助成名称－略称              PIC N(4).
013173       05 連助成名称－正式名称          PIC N(10).
013180**
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
013834*
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
013910******************************************************************
013920*                      PROCEDURE  DIVISION                       *
013930******************************************************************
013940 PROCEDURE               DIVISION.
013950************
013960*           *
013970* 初期処理   *
013980*           *
013990************
002570     PERFORM プリンタファイル作成.
014000     PERFORM 初期化.
014020     PERFORM 連結項目待避.
014030************
014040*           *
014050* 主処理     *
014060*           *
014070************
014080* 印刷
014090     PERFORM 印刷セット.
014100     PERFORM 印刷処理.
014110************
014120*           *
014130* 終了処理   *
014140*           *
014150************
014160     PERFORM 受診者印刷区分更新.
014170     PERFORM 終了処理.
014190     MOVE ZERO  TO PROGRAM-STATUS.
014200     EXIT PROGRAM.
014210*
014220*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YHN6121"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分  TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
014230************
014240* 初期処理  *
014250************
014260*================================================================*
014270 初期化 SECTION.
014280*================================================================*
014290     PERFORM ファイルオープン.
014380     PERFORM 制御情報取得.
014410*
014420*================================================================*
014430 制御情報取得 SECTION.
014440*
014450     MOVE ZEROS TO 制－制御区分.
014460     READ 制御情報マスタ
014470     NOT INVALID KEY
014490         MOVE 制－レセ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
014500         MOVE 制－レセ長期理由印刷区分 TO 長期理由印刷区分Ｗ
014510         MOVE 制－レセプト日付区分     TO レセプト日付区分Ｗ
014520         MOVE 制－レセプト患者日付区分 TO レセプト患者日付区分Ｗ
014530     END-READ.
014540*
014830*================================================================*
014840 ファイルオープン SECTION.
014850*
014860     OPEN INPUT   保険者マスタ
014870         MOVE NC"保険者" TO ファイル名.
014880         PERFORM オープンチェック.
014890     OPEN INPUT   元号マスタ
014900         MOVE NC"元号" TO ファイル名.
014910         PERFORM オープンチェック.
014920     OPEN INPUT   名称マスタ
014930         MOVE NC"名称" TO ファイル名.
014940         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
014980     OPEN INPUT   制御情報マスタ
014990         MOVE NC"制御情報" TO ファイル名.
015000         PERFORM オープンチェック.
015010     OPEN INPUT   施術所情報マスタ
015020         MOVE NC"施情" TO ファイル名.
015030         PERFORM オープンチェック.
015040     OPEN INPUT   ＩＤ管理マスタ
015050         MOVE NC"ＩＤ" TO ファイル名.
015060         PERFORM オープンチェック.
015070     OPEN INPUT   経過マスタ
015080         MOVE NC"経過" TO ファイル名.
015090         PERFORM オープンチェック.
015100     OPEN INPUT   施術記録Ｆ.
015110         MOVE NC"施記Ｆ" TO ファイル名.
015120         PERFORM オープンチェック.
015130     OPEN INPUT   負傷データＦ.
015140         MOVE NC"負傷" TO ファイル名.
015150         PERFORM オープンチェック.
015160     OPEN INPUT   負傷原因Ｆ.
015170         MOVE NC"負傷原因" TO ファイル名.
015180         PERFORM オープンチェック.
015250     OPEN INPUT   作業ファイル３.
015260         MOVE NC"作３" TO ファイル名.
015270         PERFORM オープンチェック.
015250     OPEN INPUT   作業ファイル５.
015260         MOVE NC"作５" TO ファイル名.
015270         PERFORM オープンチェック.
014840     OPEN INPUT 市町村マスタ.
014850         MOVE NC"市町村" TO ファイル名.
014860         PERFORM オープンチェック.
015160     OPEN INPUT   会情報マスタ.
015170         MOVE NC"会情" TO ファイル名.
015180         PERFORM オープンチェック.
015310*
015320     OPEN I-O   受診者情報Ｆ.
015330         MOVE NC"受情" TO ファイル名.
015340         PERFORM オープンチェック.
015350*
015360     OPEN I-O   印刷ファイル
015370         PERFORM エラー処理Ｐ.
015380*
015390*================================================================*
015400 オープンチェック SECTION.
015410*
015420     IF ( 状態キー  NOT =  "00" )
015430         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
015440         DISPLAY NC"状態キー：" 状態キー         UPON CONS
015450         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
015460                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015470         ACCEPT  キー入力 FROM CONS
015480         PERFORM ファイル閉鎖
015490         EXIT PROGRAM.
015500*
015620*================================================================*
015630 連結項目待避 SECTION.
015640*================================================================*
015650     MOVE 連レ印－施術和暦           TO 施術和暦ＷＲ.
015660     MOVE 連レ印－施術年             TO 施術年ＷＲ.
015670     MOVE 連レ印－施術月             TO 施術月ＷＲ.
015680     MOVE 連レ印－保険種別           TO 保険種別ＷＲ.
015690     MOVE 連レ印－保険者番号         TO 保険者番号ＷＲ.
015700     MOVE 連レ印－公費種別           TO 公費種別ＷＲ.
015710     MOVE 連レ印－費用負担者番号     TO 費用負担者番号ＷＲ.
015720     MOVE 連レ印－助成種別           TO 助成種別ＷＲ.
015730     MOVE 連レ印－費用負担者番号助成 TO 費用負担者番号助成ＷＲ.
015740     MOVE 連レ印－本人家族区分       TO 本人家族区分ＷＲ.
015750     MOVE 連レ印－患者カナ           TO 患者カナＷＲ.
015760     MOVE 連レ印－患者番号           TO 患者番号ＷＲ.
015770     MOVE 連レ印－枝番               TO 枝番ＷＲ.
015850*
015860     EVALUATE 連レ－保険種別
015870     WHEN 05
015880        MOVE "ROUJ" TO レセプト種類Ｗ
015890     WHEN 01
015900        MOVE "KOKU" TO レセプト種類Ｗ
015910     WHEN 02
015920     WHEN 06
015930     WHEN 07
015940        MOVE "SYAH" TO レセプト種類Ｗ
015950     WHEN 03
015960        MOVE "KUMI" TO レセプト種類Ｗ
015970     WHEN 04
015980        MOVE "KYOS" TO レセプト種類Ｗ
015990     WHEN 08
016000        MOVE "TAIS" TO レセプト種類Ｗ
016010     WHEN 09
016020        MOVE "JIEI" TO レセプト種類Ｗ
016030     WHEN 50 THRU 60
016040        MOVE "JYOS" TO レセプト種類Ｗ
016050     WHEN OTHER
016060        MOVE SPACE  TO レセプト種類Ｗ
016070     END-EVALUATE.
015780*
015790************
015800* 主処理    *
015810************
015820*================================================================*
015830 印刷セット SECTION.
015840*================================================================*
015850     PERFORM 項目初期化.
           PERFORM 基本情報取得.
015860     PERFORM 施術所情報取得.
015870     PERFORM 受診者情報取得.
015880     PERFORM 請求先情報取得.
015890     PERFORM 負傷データ取得.
015910     PERFORM 料金情報取得.
015920     PERFORM 施術記録取得.
015930     PERFORM レセプト並び順取得.
015940***     PERFORM 長期判定取得.
015960     PERFORM 初検加算時刻取得.
           PERFORM 県施術ＩＤ取得.
           PERFORM ＱＲデータセット.
      *
           MOVE 受－患者コード     TO 患者コード.
015970*
016791*-----------------------------------------------*
016800     IF ( 負傷原因印刷区分Ｗ  NOT = 1 ) AND ( レセ負傷原因印刷区分Ｗ NOT = 1 )
016813        IF ( 負傷原因印刷区分Ｗ = 3 OR 4 )
016815           PERFORM 負傷原因印刷対象判定処理
016817        ELSE
016820           PERFORM 負傷原因取得
016821        END-IF
016830     END-IF.
016831*-----------------------------------------------*
016020*
016030     IF ( 長期理由印刷区分Ｗ  NOT = 1 )
               MOVE 長期理由印刷区分Ｗ TO 連摘文－長期区分
016080     END-IF.
016090*
016100     PERFORM 委任年月日取得.
           PERFORM 施術日取得.
016110*
016120     PERFORM 往療加算回数取得.
016130*     PERFORM 負担率取得.
           IF 受－助成種別 NOT = ZERO
016140        PERFORM 助成印取得
              IF 助成印Ｗ NOT = SPACE
                 MOVE 助成印Ｗ         TO 助成印
                 MOVE NC"○"           TO 助成用マル
              END-IF
           END-IF.
016150*
016420******************
016430* タイトルセット *
016440******************
           IF 連レ－保険種別 > 50
              MOVE タイトルＷ TO タイトル
              MOVE 取消線Ｗ   TO 取消線
           END-IF.
016160********************
016170* 受診者情報セット *
016180********************
016230*
           MOVE 施術和暦Ｗ         TO 元－元号区分.
037380     READ 元号マスタ
037390     NOT INVALID KEY
037400         MOVE 元－元号名称   TO 施術和暦
037410     END-READ.
016240     MOVE 施術年Ｗ           TO 施術年.
016250     MOVE 施術月Ｗ           TO 施術月.
      *
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
      *
           IF 連レ－保険種別 > 50
               IF 公費負担者番号Ｗ(1:2) = "99"
                   MOVE SPACE            TO 公費負担者番号
               ELSE
                   MOVE 公費負担者番号Ｗ TO 公費負担者番号
               END-IF
      */受給者番号が８文字以上の場合枠を無視して印刷する/110425
               IF 印刷受給者番号２Ｗ = SPACE
                   MOVE 印刷受給者番号Ｗ TO 受給者番号
               ELSE
                   MOVE 受給者番号Ｗ     TO 受給者番号２
               END-IF
               MOVE "（助成）"           TO 助成ＣＭ
               EVALUATE 受－助成種別
               WHEN 51
                   MOVE "/４１老"        TO 助成種別Ｗ
               WHEN 52
                   MOVE "/ひとり親"      TO 助成種別Ｗ
               WHEN 53
                   MOVE "/身障"          TO 助成種別Ｗ
               WHEN 54
                   MOVE "/被爆"          TO 助成種別Ｗ
               WHEN 55
                   MOVE "/乳幼"          TO 助成種別Ｗ
               WHEN 60
                   MOVE "/他助成"        TO 助成種別Ｗ
               END-EVALUATE
           END-IF.
017070*
016410     MOVE 保険者番号Ｗ      TO 保険者番号 保険者番号１.
           MOVE 請求先名称Ｗ      TO 保険者名.
016420*
016430*     IF ( 保険者名称２Ｗ = SPACE )
016440*        MOVE SPACE          TO 保険者名称１ 保険者名称２
016450*        MOVE 保険者名称１Ｗ TO 保険者名称
016460*     ELSE
016470*        MOVE SPACE          TO 保険者名称
016480*        MOVE 保険者名称１Ｗ TO 保険者名称１
016490*        MOVE 保険者名称２Ｗ TO 保険者名称２
016500*     END-IF.
016510*
016520*     IF ( 請求先名称２Ｗ = SPACE )
016530*        MOVE SPACE          TO 請求先名称１ 請求先名称２
016540*        MOVE 請求先名称１Ｗ TO 請求先名称
016550*     ELSE
016560*        MOVE SPACE          TO 請求先名称
016570*        MOVE 請求先名称１Ｗ TO 請求先名称１
016580*        MOVE 請求先名称２Ｗ TO 請求先名称２
016590*     END-IF.
016640*
016650     MOVE 被保険者カナＷ    TO 被保険者カナ.
016660     MOVE 被保険者氏名Ｗ    TO 被保険者氏名.
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
016700     MOVE 被保険者住所１Ｗ  TO 住所１.
016710     MOVE 被保険者住所２Ｗ  TO 住所２.
016720*
016730***     MOVE 患者郵便番号１Ｗ  TO 郵便番号１.
016740***     MOVE 患者郵便番号２Ｗ  TO 郵便番号２.
016750***     MOVE "-"               TO 郵便区切.
016760***     MOVE 患者住所１Ｗ      TO 住所１.
016770***     MOVE 患者住所２Ｗ      TO 住所２.
016780*
016790     MOVE 患者カナＷ        TO 患者カナ.
016800     MOVE 患者氏名Ｗ        TO 患者氏名.
016810     MOVE 男チェックＷ      TO 男チェック.
016820     MOVE 女チェックＷ      TO 女チェック.
016830***     MOVE 性別Ｗ            TO 性別.
016840     MOVE 明治チェックＷ    TO 明治チェック.
016850     MOVE 大正チェックＷ    TO 大正チェック.
016860     MOVE 昭和チェックＷ    TO 昭和チェック.
016870     MOVE 平成チェックＷ    TO 平成チェック.
023070     MOVE 令和チェックＷ    TO 令和チェック.
           MOVE "1明 2大 3昭 4平 5令"   TO 元号ＣＭ.
016880***     MOVE 患者和暦名称Ｗ    TO 患者和暦.
016890     MOVE 患者年Ｗ          TO 患者年.
016900     MOVE 患者月Ｗ          TO 患者月.
016910     MOVE 患者日Ｗ          TO 患者日.
016920***     MOVE 印刷続柄Ｗ        TO 続柄.
016930***     MOVE 特別コメントＷ    TO 続柄コメント.
016940*
016960        MOVE 負傷原因固定Ｗ TO 負傷原因ＣＭ.
016980        MOVE 負傷原因Ｗ(1)  TO 負傷原因１.
016990        MOVE 負傷原因Ｗ(2)  TO 負傷原因２.
017000        MOVE 負傷原因Ｗ(3)  TO 負傷原因３.
017010        MOVE 負傷原因Ｗ(4)  TO 負傷原因４.
017010        MOVE 負傷原因Ｗ(5)  TO 負傷原因５.
017010        MOVE 負傷原因Ｗ(6)  TO 負傷原因６.
017010        MOVE 負傷原因Ｗ(7)  TO 負傷原因７.
017030*
017040********************
017050* 負傷データセット *
017060********************
017070* １部位 *
017080**********
017090     MOVE 負傷名Ｗ(1)       TO 負傷名１.
017100     MOVE 負傷年Ｗ(1)       TO 負傷年１.
017110     MOVE 負傷月Ｗ(1)       TO 負傷月１.
017120     MOVE 負傷日Ｗ(1)       TO 負傷日１.
017130     MOVE 初検年Ｗ(1)       TO 初検年１.
017140     MOVE 初検月Ｗ(1)       TO 初検月１.
017150     MOVE 初検日Ｗ(1)       TO 初検日１.
017160     MOVE 開始年Ｗ(1)       TO 開始年１.
017170     MOVE 開始月Ｗ(1)       TO 開始月１.
017180     MOVE 開始日Ｗ(1)       TO 開始日１.
017190     MOVE 終了年Ｗ(1)       TO 終了年１.
017200     MOVE 終了月Ｗ(1)       TO 終了月１.
017210     MOVE 終了日Ｗ(1)       TO 終了日１.
017220     MOVE 実日数Ｗ(1)       TO 実日数１.
017230     MOVE 治癒チェックＷ(1) TO 治癒チェック１.
017240     MOVE 中止チェックＷ(1) TO 中止チェック１.
017250     MOVE 転医チェックＷ(1) TO 転医チェック１.
017260**********
017270* ２部位 *
017280**********
017290     MOVE 負傷名Ｗ(2)       TO 負傷名２.
017300     MOVE 負傷年Ｗ(2)       TO 負傷年２.
017310     MOVE 負傷月Ｗ(2)       TO 負傷月２.
017320     MOVE 負傷日Ｗ(2)       TO 負傷日２.
017330     MOVE 初検年Ｗ(2)       TO 初検年２.
017340     MOVE 初検月Ｗ(2)       TO 初検月２.
017350     MOVE 初検日Ｗ(2)       TO 初検日２.
017360     MOVE 開始年Ｗ(2)       TO 開始年２.
017370     MOVE 開始月Ｗ(2)       TO 開始月２.
017380     MOVE 開始日Ｗ(2)       TO 開始日２.
017390     MOVE 終了年Ｗ(2)       TO 終了年２.
017400     MOVE 終了月Ｗ(2)       TO 終了月２.
017410     MOVE 終了日Ｗ(2)       TO 終了日２.
017420     MOVE 実日数Ｗ(2)       TO 実日数２.
017230     MOVE 治癒チェックＷ(2) TO 治癒チェック２.
017440     MOVE 中止チェックＷ(2) TO 中止チェック２.
017450     MOVE 転医チェックＷ(2) TO 転医チェック２.
017460**********
017470* ３部位 *
017480**********
017490     MOVE 負傷名Ｗ(3)       TO 負傷名３.
017500     MOVE 負傷年Ｗ(3)       TO 負傷年３.
017510     MOVE 負傷月Ｗ(3)       TO 負傷月３.
017520     MOVE 負傷日Ｗ(3)       TO 負傷日３.
017530     MOVE 初検年Ｗ(3)       TO 初検年３.
017540     MOVE 初検月Ｗ(3)       TO 初検月３.
017550     MOVE 初検日Ｗ(3)       TO 初検日３.
017560     MOVE 開始年Ｗ(3)       TO 開始年３.
017570     MOVE 開始月Ｗ(3)       TO 開始月３.
017580     MOVE 開始日Ｗ(3)       TO 開始日３.
017590     MOVE 終了年Ｗ(3)       TO 終了年３.
017600     MOVE 終了月Ｗ(3)       TO 終了月３.
017610     MOVE 終了日Ｗ(3)       TO 終了日３.
017620     MOVE 実日数Ｗ(3)       TO 実日数３.
017230     MOVE 治癒チェックＷ(3) TO 治癒チェック３.
017640     MOVE 中止チェックＷ(3) TO 中止チェック３.
017650     MOVE 転医チェックＷ(3) TO 転医チェック３.
017660**********
017670* ４部位 *
017680**********
017690     MOVE 負傷名Ｗ(4)       TO 負傷名４.
017700     MOVE 負傷年Ｗ(4)       TO 負傷年４.
017710     MOVE 負傷月Ｗ(4)       TO 負傷月４.
017720     MOVE 負傷日Ｗ(4)       TO 負傷日４.
017730     MOVE 初検年Ｗ(4)       TO 初検年４.
017740     MOVE 初検月Ｗ(4)       TO 初検月４.
017750     MOVE 初検日Ｗ(4)       TO 初検日４.
017760     MOVE 開始年Ｗ(4)       TO 開始年４.
017770     MOVE 開始月Ｗ(4)       TO 開始月４.
017780     MOVE 開始日Ｗ(4)       TO 開始日４.
017790     MOVE 終了年Ｗ(4)       TO 終了年４.
017800     MOVE 終了月Ｗ(4)       TO 終了月４.
017810     MOVE 終了日Ｗ(4)       TO 終了日４.
017820     MOVE 実日数Ｗ(4)       TO 実日数４.
017230     MOVE 治癒チェックＷ(4) TO 治癒チェック４.
017840     MOVE 中止チェックＷ(4) TO 中止チェック４.
017850     MOVE 転医チェックＷ(4) TO 転医チェック４.
017860**********
017870* ５部位 *
017880**********
017890     MOVE 負傷名Ｗ(5)       TO 負傷名５.
017900     MOVE 負傷年Ｗ(5)       TO 負傷年５.
017910     MOVE 負傷月Ｗ(5)       TO 負傷月５.
017920     MOVE 負傷日Ｗ(5)       TO 負傷日５.
017930     MOVE 初検年Ｗ(5)       TO 初検年５.
017940     MOVE 初検月Ｗ(5)       TO 初検月５.
017950     MOVE 初検日Ｗ(5)       TO 初検日５.
017960     MOVE 開始年Ｗ(5)       TO 開始年５.
017970     MOVE 開始月Ｗ(5)       TO 開始月５.
017980     MOVE 開始日Ｗ(5)       TO 開始日５.
017990     MOVE 終了年Ｗ(5)       TO 終了年５.
018000     MOVE 終了月Ｗ(5)       TO 終了月５.
018010     MOVE 終了日Ｗ(5)       TO 終了日５.
018020     MOVE 実日数Ｗ(5)       TO 実日数５.
017230     MOVE 治癒チェックＷ(5) TO 治癒チェック５.
018040     MOVE 中止チェックＷ(5) TO 中止チェック５.
018050     MOVE 転医チェックＷ(5) TO 転医チェック５.
018060**************
018070* 経過セット *
018080**************
018090*/ 編集後の経過略称を帳票にセットする /*
018100     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL ( 部位ＣＮＴ > 5 )
018110         MOVE 経過略称編集Ｔ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
018120     END-PERFORM.
018130*
018140*****     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018150********             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
018160*****             UNTIL ( 部位ＣＮＴ > 5 )
018170******         MOVE 部位ＣＮＴＷ(部位ＣＮＴ)   TO 経過部位ＣＮＴ(部位ＣＮＴ)
018180******         MOVE 部位区切Ｗ(部位ＣＮＴ)     TO 部位区切(部位ＣＮＴ)
018190*****         MOVE 印刷経過略称Ｗ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
018200*****     END-PERFORM.
018210*****************************************
018220*     新規・継続チェックについて        *
018230*   ●新規...初検有り ●継続...初検なし *
018240*****************************************
018250     MOVE 新規チェックＷ    TO 新規チェック.
018260     MOVE 継続チェックＷ    TO 継続チェック.
018270********************
018280* 料金データセット *
018290********************
018300*    ****************************************************************
018310*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
018320*    ****************************************************************
018330     MOVE 初検料ＷＲ                   TO  初検料.
           MOVE 相談料ＷＲ                   TO  初検時相談料.
018340     MOVE 休日チェックＷ               TO  休日チェック.
018350     MOVE 深夜チェックＷ               TO  深夜チェック.
018360     MOVE 時間外チェックＷ             TO  時間外チェック.
018370     MOVE 初検加算料ＷＲ               TO  初検加算料.
019110     IF ( 初検加算時ＷＴ(1) NOT = ZERO ) OR
019120        ( 初検加算分ＷＴ(1) NOT = ZERO )
019130        MOVE 初検加算時ＷＴ(1)         TO  初検加算時
019140        MOVE 初検加算分ＷＴ(1)         TO  初検加算分
      *        MOVE "施術時間"                TO 初検加算ＣＭ
              MOVE ":"                       TO 初検加算区切
019150     END-IF.
018380     MOVE 再検料ＷＲ                   TO  再検料.
018390*
018400     MOVE 往療距離ＷＲ                 TO  往療距離.
018410     MOVE 往療回数ＷＲ                 TO  往療回数.
018420     MOVE 往療料ＷＲ                   TO  往療料.
018430     MOVE 夜間チェックＷ               TO  夜間チェック.
018440     MOVE 難路チェックＷ               TO  難路チェック.
018450     MOVE 暴風雨雪チェックＷ           TO  暴風雨雪チェック.
018460*     MOVE 往療加算回数Ｗ               TO  往療加算回数.
018470     MOVE 往療加算料ＷＲ               TO  往療加算料.
018480*
           MOVE 金属回数Ｗ                   TO  金属回数.
018520     MOVE 金属副子加算料ＷＲ           TO  金属副子加算料.
           MOVE 運動回数Ｗ                   TO  運動回数.
           MOVE 運動料Ｗ                     TO  運動後療料.
018530     MOVE 施術情報提供料ＷＲ           TO  施術情報提供料.
018540*
018550     MOVE 小計Ｗ                       TO 小計.
018560********************
018570* 初回処置料セット *
018580********************
018590     MOVE 整復料チェックＷ            TO 整復料チェック.
018600     MOVE 固定料チェックＷ            TO 固定料チェック.
018610     MOVE 施療料チェックＷ            TO 施療料チェック.
018620*
018630     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018640             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
018650         MOVE 初回処置料ＷＲ(部位ＣＮＴ) TO 初回処置料(部位ＣＮＴ)
018660     END-PERFORM.
018670     MOVE 初回処置料合計Ｗ            TO 初回処置料合計.
018680*
018690********************
018700* 逓減毎料金セット *
018710********************
018720*    **********
018730*    * １部位 *
018740*    **********
018750     MOVE 後療単価１ＷＲ             TO 後療単価１.
018760     MOVE 後療回数１ＷＲ             TO 後療回数１.
018770     MOVE 後療料１ＷＲ               TO 後療料１.
018780     MOVE 冷罨法回数１ＷＲ           TO 冷罨法回数１.
018790     MOVE 冷罨法料１ＷＲ             TO 冷罨法料１.
018800     MOVE 温罨法回数１ＷＲ           TO 温罨法回数１.
018810     MOVE 温罨法料１ＷＲ             TO 温罨法料１.
018820     MOVE 電療回数１ＷＲ             TO 電療回数１.
018830     MOVE 電療料１ＷＲ               TO 電療料１.
018840     MOVE 小計１ＷＲ                 TO 小計１.
018850     IF ( 長期逓減率１ＷＲ NOT = ZERO )
018860         COMPUTE 長期逓減率１ = 長期逓減率１ＷＲ / 100
018870     END-IF.
018880     MOVE 長期込小計１ＷＲ           TO 長期込小計１.
018890*    **********
018900*    * ２部位 *
018910*    **********
018920     MOVE 後療単価２ＷＲ             TO 後療単価２.
018930     MOVE 後療回数２ＷＲ             TO 後療回数２.
018940     MOVE 後療料２ＷＲ               TO 後療料２.
018950     MOVE 冷罨法回数２ＷＲ           TO 冷罨法回数２.
018960     MOVE 冷罨法料２ＷＲ             TO 冷罨法料２.
018970     MOVE 温罨法回数２ＷＲ           TO 温罨法回数２.
018980     MOVE 温罨法料２ＷＲ             TO 温罨法料２.
018990     MOVE 電療回数２ＷＲ             TO 電療回数２.
019000     MOVE 電療料２ＷＲ               TO 電療料２.
019010     MOVE 小計２ＷＲ                 TO 小計２.
019020     IF ( 長期逓減率２ＷＲ NOT = ZERO )
019030         COMPUTE 長期逓減率２ = 長期逓減率２ＷＲ / 100
019040     END-IF.
019050     MOVE 長期込小計２ＷＲ           TO 長期込小計２.
019060*    ****************
019070*    * ３部位／８割 *
019080*    ****************
019090     MOVE 後療単価３８ＷＲ             TO 後療単価３８.
019100     MOVE 後療回数３８ＷＲ             TO 後療回数３８.
019110     MOVE 後療料３８ＷＲ               TO 後療料３８.
019120     MOVE 冷罨法回数３８ＷＲ           TO 冷罨法回数３８.
019130     MOVE 冷罨法料３８ＷＲ             TO 冷罨法料３８.
019140     MOVE 温罨法回数３８ＷＲ           TO 温罨法回数３８.
019150     MOVE 温罨法料３８ＷＲ             TO 温罨法料３８.
019160     MOVE 電療回数３８ＷＲ             TO 電療回数３８.
019170     MOVE 電療料３８ＷＲ               TO 電療料３８.
019180     MOVE 小計３８ＷＲ                 TO 小計３８.
019190     MOVE 多部位込小計３８ＷＲ         TO 多部位込小計３８.
019200     IF ( 長期逓減率３８ＷＲ NOT = ZERO )
019210         COMPUTE 長期逓減率３８ = 長期逓減率３８ＷＲ / 100
019220     END-IF.
019230     MOVE 長期込小計３８ＷＲ           TO 長期込小計３８.
      */ 逓減率 0.7→0.6 /42505
      *     IF (施術和暦年月ＷＲ >= 42505)
      *        MOVE "60"                      TO 逓減３８
      *        MOVE "0.6"                     TO 多部位３８
      *        MOVE "==="                     TO 逓減訂正３８ 多部位訂正３８
      *     END-IF.
019240*    ****************
019250*    * ３部位／10割 *
019260*    ****************
019270     MOVE 逓減開始月３０ＷＲ           TO 逓減開始月３０.
019280     MOVE 逓減開始日３０ＷＲ           TO 逓減開始日３０.
019320     MOVE 後療単価３０ＷＲ             TO 後療単価３０.
019330     MOVE 後療回数３０ＷＲ             TO 後療回数３０.
019340     MOVE 後療料３０ＷＲ               TO 後療料３０.
019350     MOVE 冷罨法回数３０ＷＲ           TO 冷罨法回数３０.
019360     MOVE 冷罨法料３０ＷＲ             TO 冷罨法料３０.
019370     MOVE 温罨法回数３０ＷＲ           TO 温罨法回数３０.
019380     MOVE 温罨法料３０ＷＲ             TO 温罨法料３０.
019390     MOVE 電療回数３０ＷＲ             TO 電療回数３０.
019400     MOVE 電療料３０ＷＲ               TO 電療料３０.
019410     MOVE 小計３０ＷＲ                 TO 小計３０.
019420     IF ( 長期逓減率３０ＷＲ NOT = ZERO )
019430         COMPUTE 長期逓減率３０ = 長期逓減率３０ＷＲ / 100
019440     END-IF.
019450     MOVE 長期込小計３０ＷＲ           TO 長期込小計３０.
019460**    ****************
019470**    * ４部位／５割 *
019480**    ****************
019490*     MOVE 後療単価４５ＷＲ             TO 後療単価４５.
019500*     MOVE 後療回数４５ＷＲ             TO 後療回数４５.
019510*     MOVE 後療料４５ＷＲ               TO 後療料４５.
019520*     MOVE 冷罨法回数４５ＷＲ           TO 冷罨法回数４５.
019530*     MOVE 冷罨法料４５ＷＲ             TO 冷罨法料４５.
019540*     MOVE 温罨法回数４５ＷＲ           TO 温罨法回数４５.
019550*     MOVE 温罨法料４５ＷＲ             TO 温罨法料４５.
019560*     MOVE 電療回数４５ＷＲ             TO 電療回数４５.
019570*     MOVE 電療料４５ＷＲ               TO 電療料４５.
019580*     MOVE 小計４５ＷＲ                 TO 小計４５.
019590*     MOVE 多部位込小計４５ＷＲ         TO 多部位込小計４５.
019600*     IF ( 長期逓減率４５ＷＲ NOT = ZERO )
019610*         COMPUTE 長期逓減率４５ = 長期逓減率４５ＷＲ / 100
019620*     END-IF.
019630*     MOVE 長期込小計４５ＷＲ           TO 長期込小計４５.
019640*    ****************
019650*    * ４部位／８割 *
019660*    ****************
019670     MOVE 逓減開始月４８ＷＲ           TO 逓減開始月４８.
019680     MOVE 逓減開始日４８ＷＲ           TO 逓減開始日４８.
019720     MOVE 後療単価４８ＷＲ             TO 後療単価４８.
019730     MOVE 後療回数４８ＷＲ             TO 後療回数４８.
019740     MOVE 後療料４８ＷＲ               TO 後療料４８.
019750     MOVE 冷罨法回数４８ＷＲ           TO 冷罨法回数４８.
019760     MOVE 冷罨法料４８ＷＲ             TO 冷罨法料４８.
019770     MOVE 温罨法回数４８ＷＲ           TO 温罨法回数４８.
019780     MOVE 温罨法料４８ＷＲ             TO 温罨法料４８.
019790     MOVE 電療回数４８ＷＲ             TO 電療回数４８.
019800     MOVE 電療料４８ＷＲ               TO 電療料４８.
019810     MOVE 小計４８ＷＲ                 TO 小計４８.
019820     MOVE 多部位込小計４８ＷＲ         TO 多部位込小計４８.
019830     IF ( 長期逓減率４８ＷＲ NOT = ZERO )
019840         COMPUTE 長期逓減率４８ = 長期逓減率４８ＷＲ / 100
019850     END-IF.
019860     MOVE 長期込小計４８ＷＲ           TO 長期込小計４８.
      */ 逓減率 0.7→0.6 /42505
      *     IF (施術和暦年月ＷＲ >= 42505)
      *        MOVE "60"                      TO 逓減４８
      *        MOVE "0.6"                     TO 多部位４８
      *        MOVE "==="                     TO 逓減訂正４８ 多部位訂正４８
      *     END-IF.
019870*    ****************
019880*    * ４部位／10割 *
019890*    ****************
019900     MOVE 逓減開始月４０ＷＲ           TO 逓減開始月４０.
019910     MOVE 逓減開始日４０ＷＲ           TO 逓減開始日４０.
019950     MOVE 後療単価４０ＷＲ             TO 後療単価４０.
019960     MOVE 後療回数４０ＷＲ             TO 後療回数４０.
019970     MOVE 後療料４０ＷＲ               TO 後療料４０.
019980     MOVE 冷罨法回数４０ＷＲ           TO 冷罨法回数４０.
019990     MOVE 冷罨法料４０ＷＲ             TO 冷罨法料４０.
020000     MOVE 温罨法回数４０ＷＲ           TO 温罨法回数４０.
020010     MOVE 温罨法料４０ＷＲ             TO 温罨法料４０.
020020     MOVE 電療回数４０ＷＲ             TO 電療回数４０.
020030     MOVE 電療料４０ＷＲ               TO 電療料４０.
020040     MOVE 小計４０ＷＲ                 TO 小計４０.
020050     IF ( 長期逓減率４０ＷＲ NOT = ZERO )
020060         COMPUTE 長期逓減率４０ = 長期逓減率４０ＷＲ / 100
020070     END-IF.
020080     MOVE 長期込小計４０ＷＲ           TO 長期込小計４０.
020090*
020100*↓***********************************************************************
020110* ５部位／2.5割の印字は必要ない。
020120*------------------------------------------------------------------------*
020130*    *****************
020140*    * ５部位／2.5割 *
020150*    *****************
020160*     MOVE 後療単価５２ＷＲ             TO 後療単価５２.
020170*     MOVE 後療回数５２ＷＲ             TO 後療回数５２.
020180*     MOVE 後療料５２ＷＲ               TO 後療料５２.
020190*     MOVE 冷罨法回数５２ＷＲ           TO 冷罨法回数５２.
020200*     MOVE 冷罨法料５２ＷＲ             TO 冷罨法料５２.
020210*     MOVE 温罨法回数５２ＷＲ           TO 温罨法回数５２.
020220*     MOVE 温罨法料５２ＷＲ             TO 温罨法料５２.
020230*     MOVE 電療回数５２ＷＲ             TO 電療回数５２.
020240*     MOVE 電療料５２ＷＲ               TO 電療料５２.
020250*     MOVE 小計５２ＷＲ                 TO 小計５２.
020260*     MOVE 多部位込小計５２ＷＲ         TO 多部位込小計５２.
020270*     IF ( 長期逓減率５２ＷＲ NOT = ZERO )
020280*         COMPUTE 長期逓減率５２ = 長期逓減率５２ＷＲ / 100
020290*     END-IF.
020300*     MOVE 長期込小計５２ＷＲ           TO 長期込小計５２.
020310*↑***********************************************************************
020320*
020330*    ****************
020340*    * ５部位／５割 *
020350*    ****************
020360*     MOVE 逓減開始月５５ＷＲ           TO 逓減開始月５５.
020370*     MOVE 逓減開始日５５ＷＲ           TO 逓減開始日５５.
020410*     MOVE 後療単価５５ＷＲ             TO 後療単価５５.
020420*     MOVE 後療回数５５ＷＲ             TO 後療回数５５.
020430*     MOVE 後療料５５ＷＲ               TO 後療料５５.
020440*     MOVE 冷罨法回数５５ＷＲ           TO 冷罨法回数５５.
020450*     MOVE 冷罨法料５５ＷＲ             TO 冷罨法料５５.
020460*     MOVE 温罨法回数５５ＷＲ           TO 温罨法回数５５.
020470*     MOVE 温罨法料５５ＷＲ             TO 温罨法料５５.
020480*     MOVE 電療回数５５ＷＲ             TO 電療回数５５.
020490*     MOVE 電療料５５ＷＲ               TO 電療料５５.
020500*     MOVE 小計５５ＷＲ                 TO 小計５５.
020510*     MOVE 多部位込小計５５ＷＲ         TO 多部位込小計５５.
020520*     IF ( 長期逓減率５５ＷＲ NOT = ZERO )
020530*         COMPUTE 長期逓減率５５ = 長期逓減率５５ＷＲ / 100
020540*     END-IF.
020550*     MOVE 長期込小計５５ＷＲ           TO 長期込小計５５.
020560*    ****************
020570*    * ５部位／８割 *
020580*    ****************
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
020590*     MOVE 逓減開始月５８ＷＲ           TO 逓減開始月５８.
020600*     MOVE 逓減開始日５８ＷＲ           TO 逓減開始日５８.
020640*     MOVE 後療単価５８ＷＲ             TO 後療単価５８.
020650*     MOVE 後療回数５８ＷＲ             TO 後療回数５８.
020660*     MOVE 後療料５８ＷＲ               TO 後療料５８.
020670*     MOVE 冷罨法回数５８ＷＲ           TO 冷罨法回数５８.
020680*     MOVE 冷罨法料５８ＷＲ             TO 冷罨法料５８.
020690*     MOVE 温罨法回数５８ＷＲ           TO 温罨法回数５８.
020700*     MOVE 温罨法料５８ＷＲ             TO 温罨法料５８.
020710*     MOVE 電療回数５８ＷＲ             TO 電療回数５８.
020720*     MOVE 電療料５８ＷＲ               TO 電療料５８.
020730*     MOVE 小計５８ＷＲ                 TO 小計５８.
020740*     MOVE 多部位込小計５８ＷＲ         TO 多部位込小計５８.
020750*     IF ( 長期逓減率５８ＷＲ NOT = ZERO )
020760*         COMPUTE 長期逓減率５８ = 長期逓減率５８ＷＲ / 100
020770*     END-IF.
020780*     MOVE 長期込小計５８ＷＲ           TO 長期込小計５８.
020790*    ****************
020800*    * ５部位／10割 *
020810*    ****************
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
020820*     MOVE 逓減開始月５０ＷＲ           TO 逓減開始月５０.
020830*     MOVE 逓減開始日５０ＷＲ           TO 逓減開始日５０.
020870*     MOVE 後療単価５０ＷＲ             TO 後療単価５０.
020880*     MOVE 後療回数５０ＷＲ             TO 後療回数５０.
020890*     MOVE 後療料５０ＷＲ               TO 後療料５０.
020900*     MOVE 冷罨法回数５０ＷＲ           TO 冷罨法回数５０.
020910*     MOVE 冷罨法料５０ＷＲ             TO 冷罨法料５０.
020920*     MOVE 温罨法回数５０ＷＲ           TO 温罨法回数５０.
020930*     MOVE 温罨法料５０ＷＲ             TO 温罨法料５０.
020940*     MOVE 電療回数５０ＷＲ             TO 電療回数５０.
020950*     MOVE 電療料５０ＷＲ               TO 電療料５０.
020960*     MOVE 小計５０ＷＲ                 TO 小計５０.
020970*     MOVE 長期逓減率５０ＷＲ           TO 長期逓減率５０.
020980*     IF ( 長期逓減率５０ＷＲ NOT = ZERO )
020990*         COMPUTE 長期逓減率５０ = 長期逓減率５０ＷＲ / 100
021000*     END-IF.
021010*     MOVE 長期込小計５０ＷＲ           TO 長期込小計５０.
021020*
021327*------------------------------------------------------------------------*
      *
      */金属副子・運動後療の変更・追加/1805
           IF ( 施術和暦年月ＷＲ >= 43006 )
              INITIALIZE 連金運－キー
019550        MOVE 施術和暦ＷＲ TO 連金運－施術和暦
019560        MOVE 施術年ＷＲ   TO 連金運－施術年
019570        MOVE 施術月ＷＲ   TO 連金運－施術月
019580        MOVE 患者番号ＷＲ TO 連金運－患者番号
019590        MOVE 枝番ＷＲ     TO 連金運－枝番
              MOVE 連レ－保険種別 TO 連金運－保険種別
              MOVE 44           TO 連金運－会コード
              MOVE 1            TO 連金運－用紙種別
              CALL "KINUNRYO"
              CANCEL "KINUNRYO"
              MOVE 連金運－金属副子ＣＭ           TO 金属副子ＣＭ
              IF ( 金属副子加算料ＷＲ NOT = ZERO )
                 MOVE 金属副子ＣＭ                TO 金属副子
              END-IF
              PERFORM VARYING カウンタ FROM 1 BY 1
                        UNTIL カウンタ > 3
                 MOVE 連金運－金属副子月(1 カウンタ) TO 金属月(カウンタ)
                 MOVE 連金運－金属副子日(1 カウンタ) TO 金属日(カウンタ)
                 IF 連金運－金属副子月(1 カウンタ) NOT = ZERO
                    MOVE "月"                        TO 月(カウンタ)
                 END-IF
              END-PERFORM
              PERFORM VARYING カウンタ FROM 1 BY 1
                        UNTIL カウンタ > 5
                 MOVE 連金運－運動日(カウンタ)     TO 運動日(カウンタ)
              END-PERFORM
           END-IF.
      *
021330*    ************
021340*    * 合計金額 *
021350*    ************
021360     MOVE レセ－合計             TO 合計.
021380     MOVE レセ－一部負担金       TO 一部負担金.
021440     MOVE レセ－請求金額         TO 請求金額.
           IF 連レ－保険種別 > 50
              EVALUATE TRUE
              WHEN 公費負担者番号Ｗ(3:2) = 27
021440            MOVE レセ－受給者負担額 TO 一部負担金
021440            MOVE レセ－助成請求金額 TO 請求金額
              WHEN OTHER
021370            MOVE レセ－受給者負担額       TO 受給者負担額
021380            MOVE レセ－助成請求金額       TO 助成請求額
                  MOVE "一部負担金相当額（医療助成費）" TO 一部負担金ＣＭ
                  MOVE "請求金額（医療助成費）"         TO 請求金額ＣＭ
                  MOVE "円" TO 一部負担金円ＣＭ 請求金額円ＣＭ
              END-EVALUATE
           END-IF.
021450*
021460**************
021470* 適用セット *
021480**************
021490     MOVE 適用１Ｗ               TO 適用１.
021500     MOVE 適用２Ｗ               TO 適用２.
021510***     MOVE 適用３Ｗ               TO 適用３.
021520***     MOVE 適用４Ｗ               TO 適用４.
021530*
      *
      */大阪、広島 本体レセに負担者番号、受給者番号を印字
      */助成レセに助成マーク
           IF 公費負担者番号Ｗ(3:2) = "27" OR "34"
               IF 公費負担者番号Ｗ(1:2) = "99"
                   MOVE SPACE            TO 公費負担者番号
               ELSE
                   MOVE 公費負担者番号Ｗ TO 公費負担者番号
               END-IF
      */受給者番号が８文字以上の場合枠を無視して印刷する/110425
               IF 印刷受給者番号２Ｗ = SPACE
                   MOVE 印刷受給者番号Ｗ TO 受給者番号
               ELSE
                   MOVE 受給者番号Ｗ     TO 受給者番号２
               END-IF
               IF 連レ－保険種別 > 50
                   MOVE 助成印Ｗ         TO 助成印
                   MOVE NC"○"           TO 助成用マル
               END-IF
           ELSE
      */大阪、広島以外 本体レセに助成マーク
               IF (受－助成種別 NOT = ZERO) AND
                  (連レ－保険種別   < 50  ) AND
                  (助成印Ｗ NOT = SPACE)
                   MOVE 助成印Ｗ         TO 助成印
                   MOVE NC"○"           TO 助成用マル
               END-IF
           END-IF.
      */奈良県助成(国保退職は負担者番号、受給者番号を記載、それ以外は「奈良県福祉医療」と記載)
           IF 公費負担者番号Ｗ(3:2) = "29"
               IF 受－保険種別 = 01 OR 08 OR 05
                   IF 公費負担者番号Ｗ(1:2) = "99"
                       MOVE SPACE        TO 公費負担者番号
                   ELSE
                       IF (受－助成種別 = 52  AND 公費負担者番号Ｗ(1:2) = "91") OR
                          (受－助成種別 = 53  AND 公費負担者番号Ｗ(1:2) = "81") OR
                          (受－助成種別 = 55  AND 公費負担者番号Ｗ(1:2) = "71")
                           MOVE 公費負担者番号Ｗ TO 公費負担者番号
                       END-IF
                   END-IF
      *
                   IF ( 印刷受給者番号Ｗ(1:1) = "*"  ) OR
                      ( 印刷受給者番号Ｗ(1:2) = "＊" )
                      MOVE  SPACE                TO 受給者番号
                   ELSE
      *    /受給者番号が８文字以上の場合枠を無視して印刷する/110425
                       IF 印刷受給者番号２Ｗ = SPACE
                           MOVE 印刷受給者番号Ｗ TO 受給者番号
                       ELSE
                           MOVE 受給者番号Ｗ     TO 受給者番号２
                       END-IF
                   END-IF
               ELSE
                   IF (受－助成種別 = 52  AND 公費負担者番号Ｗ(1:2) = "91") OR
                      (受－助成種別 = 53  AND 公費負担者番号Ｗ(1:2) = "81") OR
                      (受－助成種別 = 55  AND 公費負担者番号Ｗ(1:2) = "71")
033830                 STRING 適用１             DELIMITED BY SPACE
036850                        NC"，"             DELIMITED BY SIZE
036860                      NC"奈良県福祉医療"   DELIMITED BY SIZE
033870                      INTO 適用１
034720                 END-STRING
                   END-IF
               END-IF
           END-IF.
      */32140410神奈川県市町村職員＋被爆の助成の場合、本体レセにも負担者番号と受給者番号を印刷する/111213
           IF (受－保険者番号 = 32140410) AND (受－助成種別 = 54)
               IF 公費負担者番号Ｗ(1:2) = "99"
                   MOVE SPACE            TO 公費負担者番号
               ELSE
                   MOVE 公費負担者番号Ｗ TO 公費負担者番号
               END-IF
               IF 印刷受給者番号２Ｗ = SPACE
                   MOVE 印刷受給者番号Ｗ TO 受給者番号
               ELSE
                   MOVE 受給者番号Ｗ     TO 受給者番号２
               END-IF
           END-IF.
      */水俣病の場合、本体レセにも負担者番号と受給者番号を印刷する/150610
      *     IF (受－保険種別 NOT = 05) AND (受－特別区分 = 2) AND
      *        (受－助成種別 = 60) 
               EVALUATE 公費負担者番号Ｗ
               WHEN "51433019"
               WHEN "51433027"
               WHEN "51433035"
               WHEN "51433043"
               WHEN "51153013"
               WHEN "51153021"
               WHEN "51463016"
               WHEN "51463024"
                   MOVE 公費負担者番号Ｗ TO 公費負担者番号
                   IF 印刷受給者番号２Ｗ = SPACE
                       MOVE 印刷受給者番号Ｗ TO 受給者番号
                   ELSE
                       MOVE 受給者番号Ｗ     TO 受給者番号２
                   END-IF
               END-EVALUATE.
      *     END-IF.
019850*
022410*------------------------------------------------------------------------*
022420* 長期頻回の時、摘要欄に内容を記載
      *
           MOVE SPACE                     TO 長期頻回Ｗ.
      *     IF (レセ－部位継続月数(1) > 5) OR (レセ－部位継続月数(2) > 5) OR
      *        (レセ－部位継続月数(3) > 5) OR (レセ－部位継続月数(4) > 5) OR
      *        (レセ－部位継続月数(5) > 5)
      *        MOVE "長期頻回該当："       TO 長期頻回ＣＭ
      *     END-IF.
           IF (レセ－部位継続月数(1) >= 1) OR (レセ－部位継続月数(2) >= 1) OR
              (レセ－部位継続月数(3) >= 1) OR (レセ－部位継続月数(4) >= 1) OR
              (レセ－部位継続月数(5) >= 1)
              MOVE "長期頻回該当："       TO 長期頻回ＣＭ
           END-IF.
           MOVE SPACE                     TO 長期頻回ＣＭ２.
      *     IF (レセ－部位継続月数(1) > 5)
      *        MOVE "長期頻回該当："       TO 長期頻回ＣＭ２
      *     END-IF.
           IF (レセ－部位継続月数(1) > 0)
              MOVE レセ－部位継続月数(1)  TO 月数Ｗ
              MOVE 負傷名Ｗ(1)            TO 負傷名ＷＲ(1)
              STRING 長期頻回ＣＭ２       DELIMITED BY SPACE
                     "(1)"                DELIMITED BY SIZE
                     負傷名ＷＰ(1)        DELIMITED BY "　"
                     "、継続月数"         DELIMITED BY SIZE
                     月数Ｗ               DELIMITED BY SIZE
                     "月"                 DELIMITED BY SIZE
                INTO 長期頻回１ＷＴ
              END-STRING
           END-IF.
           MOVE SPACE                     TO 長期頻回ＣＭ２.
      *     IF (レセ－部位継続月数(2) > 5)
      *        MOVE "長期頻回該当："       TO 長期頻回ＣＭ２
      *     END-IF.
           IF (レセ－部位継続月数(2) > 0)
              MOVE レセ－部位継続月数(2)  TO 月数Ｗ
              MOVE 負傷名Ｗ(2)            TO 負傷名ＷＲ(2)
              STRING 長期頻回ＣＭ２       DELIMITED BY SPACE
                     "(2)"                DELIMITED BY SIZE
                     負傷名ＷＰ(2)        DELIMITED BY "　"
                     "、継続月数"         DELIMITED BY SIZE
                     月数Ｗ               DELIMITED BY SIZE
                     "月"                 DELIMITED BY SIZE
                INTO 長期頻回２ＷＴ
              END-STRING
           END-IF.
           MOVE SPACE                     TO 長期頻回ＣＭ２.
      *     IF (レセ－部位継続月数(3) > 5)
      *        MOVE "長期頻回該当："       TO 長期頻回ＣＭ２
      *     END-IF.
           IF (レセ－部位継続月数(3) > 0)
              MOVE レセ－部位継続月数(3)  TO 月数Ｗ
              MOVE 負傷名Ｗ(3)            TO 負傷名ＷＲ(3)
              STRING 長期頻回ＣＭ２       DELIMITED BY SPACE
                     "(3)"                DELIMITED BY SIZE
                     負傷名ＷＰ(3)        DELIMITED BY "　"
                     "、継続月数"         DELIMITED BY SIZE
                     月数Ｗ               DELIMITED BY SIZE
                     "月"                 DELIMITED BY SIZE
                INTO 長期頻回３ＷＴ
              END-STRING
           END-IF.
           MOVE SPACE                     TO 長期頻回ＣＭ２.
      *     IF (レセ－部位継続月数(4) > 5)
      *        MOVE "長期頻回該当："       TO 長期頻回ＣＭ２
      *     END-IF.
           IF (レセ－部位継続月数(4) > 0)
              MOVE レセ－部位継続月数(4)  TO 月数Ｗ
              MOVE 負傷名Ｗ(4)            TO 負傷名ＷＲ(4)
              STRING 長期頻回ＣＭ２       DELIMITED BY SPACE
                     "(4)"                DELIMITED BY SIZE
                     負傷名ＷＰ(4)        DELIMITED BY "　"
                     "、継続月数"         DELIMITED BY SIZE
                     月数Ｗ               DELIMITED BY SIZE
                     "月"                 DELIMITED BY SIZE
                INTO 長期頻回４ＷＴ
              END-STRING
           END-IF.
           MOVE SPACE                     TO 長期頻回ＣＭ２.
      *     IF (レセ－部位継続月数(5) > 5)
      *        MOVE "長期頻回該当："       TO 長期頻回ＣＭ２
      *     END-IF.
           IF (レセ－部位継続月数(5) > 0)
              MOVE レセ－部位継続月数(5)  TO 月数Ｗ
              MOVE 負傷名Ｗ(5)            TO 負傷名ＷＲ(5)
              STRING 長期頻回ＣＭ２       DELIMITED BY SPACE
                     "(5)"                DELIMITED BY SIZE
                     負傷名ＷＰ(5)        DELIMITED BY "　"
                     "、継続月数"         DELIMITED BY SIZE
                     月数Ｗ               DELIMITED BY SIZE
                     "月"                 DELIMITED BY SIZE
                INTO 長期頻回５ＷＴ
              END-STRING
           END-IF.
           MOVE 長期頻回ＣＭ   TO 文字１Ｗ.
           MOVE 長期頻回１ＷＴ TO 文字２Ｗ.
           CALL プログラム名Ｗ WITH C LINKAGE
                         USING BY REFERENCE 文字１Ｗ
                               BY REFERENCE 文字２Ｗ.
           MOVE 長期頻回２ＷＴ TO 文字２Ｗ.
           CALL プログラム名Ｗ WITH C LINKAGE
                         USING BY REFERENCE 文字１Ｗ
                               BY REFERENCE 文字２Ｗ.
           MOVE 長期頻回３ＷＴ TO 文字２Ｗ.
           CALL プログラム名Ｗ WITH C LINKAGE
                         USING BY REFERENCE 文字１Ｗ
                               BY REFERENCE 文字２Ｗ.
           MOVE 長期頻回４ＷＴ TO 文字２Ｗ.
           CALL プログラム名Ｗ WITH C LINKAGE
                         USING BY REFERENCE 文字１Ｗ
                               BY REFERENCE 文字２Ｗ.
           MOVE 長期頻回５ＷＴ TO 文字２Ｗ.
           CALL プログラム名Ｗ WITH C LINKAGE
                         USING BY REFERENCE 文字１Ｗ
                               BY REFERENCE 文字２Ｗ.
           MOVE 文字１Ｗ       TO 長期頻回.
      *
021680**********************
021690* 施術所データセット *
021700**********************
           MOVE 都道府県ＪＩＳＷ       TO 都道府県番号.
021710     MOVE 柔整師番号Ｗ           TO 柔整師番号.
           MOVE 共済番号Ｗ             TO 共済番号.
           PERFORM 会員番号右詰め.
           MOVE 会員番号右詰めＷ     TO 会員番号.
021730***     MOVE 定額制受理番号Ｗ       TO 定額制受理番号.
021740*
021760     MOVE 施術所郵便番号１Ｗ     TO 施術所郵便番号１.
022750     MOVE "-"                    TO 施術所郵便番号区切.
021770     MOVE 施術所郵便番号２Ｗ     TO 施術所郵便番号２.
021800     MOVE 施術所住所１Ｗ         TO 施術所住所１.
021810     MOVE 施術所住所２Ｗ         TO 施術所住所２.
021820     MOVE 接骨院名Ｗ             TO 接骨院名.
021830     MOVE 代表者カナＷ           TO 代表者カナ.
021840     MOVE 代表者名Ｗ             TO 代表者名.
021850     MOVE 施術所電話番号Ｗ       TO 施術所電話番号.
022250     MOVE 会長委任文１Ｗ         TO 会長委任コメント２.
022260     MOVE 会長委任文２Ｗ         TO 会長委任コメント３.
022260     MOVE 会長委任文３Ｗ         TO 会長委任コメント４.
022260     MOVE 会長委任文４Ｗ         TO 会長委任コメント５.
      *
021940     MOVE 口座番号Ｗ             TO 口座番号.
021950     MOVE 口座名義人カナＷ       TO 口座名義人カナ.
021960     MOVE 口座名義人Ｗ           TO 口座名義人.
           MOVE 金融機関名１Ｗ         TO 金融機関名１.
           MOVE 金融機関名２Ｗ         TO 金融機関名２.
           MOVE 金融機関名３Ｗ         TO 金融機関名３.
           MOVE 金融機関名４Ｗ         TO 金融機関名４.
           MOVE 支店名１Ｗ             TO 支店名１.
           MOVE 支店名２Ｗ             TO 支店名２.
           MOVE 支店名３Ｗ             TO 支店名３.
           MOVE 支店名４Ｗ             TO 支店名４.
           MOVE 振込チェックＷ         TO 振込チェック.
           MOVE 普通チェックＷ         TO 普通チェック.
           MOVE 当座チェックＷ         TO 当座チェック.
           MOVE 銀行チェックＷ         TO 銀行チェック.
           MOVE 金庫チェックＷ         TO 金庫チェック.
           MOVE 農協チェックＷ         TO 農協チェック.
           MOVE 本店チェックＷ         TO 本店チェック.
           MOVE 支店チェックＷ         TO 支店チェック.
           MOVE 本支所チェックＷ       TO 本支所チェック.
021970*
021980* / 柔整師・患者委任日 /
           MOVE 柔整師和暦Ｗ           TO 元－元号区分.
037380     READ 元号マスタ
037390     NOT INVALID KEY
037400         MOVE 元－元号名称       TO 受理和暦
037410     END-READ.
021990     MOVE 柔整師年Ｗ             TO 受理年.
022000     MOVE 柔整師月Ｗ             TO 受理月.
022010     MOVE 柔整師日Ｗ             TO 受理日.
022020* ( 委任年月日 印刷するか )
022030     IF ( 連入－委任印刷  = ZERO )
               MOVE 患者委任和暦Ｗ     TO 元－元号区分
037380         READ 元号マスタ
037390         NOT INVALID KEY
037400             MOVE 元－元号名称   TO 委任和暦
037410         END-READ
022040         MOVE 患者委任年Ｗ       TO 委任年
022050         MOVE 患者委任月Ｗ       TO 委任月
022060         MOVE 患者委任日Ｗ       TO 委任日
022070     END-IF.
022080*
022090* 施術ID
022100     MOVE 県施術ＩＤＷ           TO 県施術ＩＤ.
      */助成の施術所ＩＤが入力されている場合は優先する/120711
           IF 市町村施術ＩＤＷ NOT = SPACE
      */京都市の後期＋障害/120606
               IF (受－保険種別 = 05 AND 受－助成種別 = 53) AND
                  (受－費用負担者番号助成(1:5) = "39261" OR "43264")
022020             MOVE 市町村施術ＩＤＷ TO 県施術ＩＤ
               END-IF
           END-IF.
022110*
022120* 特別コメント
022130*     MOVE 特別コメントＷ         TO 特別コメント.
022140*
      */ 保険種別、県名、被保険者名を下空間に印刷 /150219
           IF 受－保険種別 = 01
               MOVE 受－保険者番号(1:2)  TO 県番号Ｗ
           ELSE
               MOVE 受－保険者番号(3:2)  TO 県番号Ｗ
           END-IF.
           IF 連レ－保険種別 > 50
               MOVE 受－費用負担者番号助成(3:2)  TO 県番号Ｗ
           END-IF.
025960     MOVE 13                     TO 名－区分コード.
025970     MOVE 県番号Ｗ               TO 名－名称コード.
025980     READ 名称マスタ
025990     INVALID KEY
026000         MOVE SPACE              TO 県ＣＭＷ
026010     NOT INVALID KEY
026020         MOVE 名－略称           TO 県ＣＭＷ
026030     END-READ.
           STRING "["                  DELIMITED BY SIZE
                  県ＣＭＷＰ           DELIMITED BY "　"
                  "]"                  DELIMITED BY SIZE
             INTO 県ＣＭ
           END-STRING.
           STRING "<"                  DELIMITED BY SIZE
                  保険種別Ｗ           DELIMITED BY SPACE
                  助成種別Ｗ           DELIMITED BY SPACE
                  ">"                  DELIMITED BY SIZE
             INTO 保険種別ＣＭ
           END-STRING.
           IF 連レ－保険種別 > 50
016660        MOVE 患者氏名Ｗ          TO 被保険者名
           ELSE
016660        MOVE 被保険者氏名Ｗ      TO 被保険者名
           END-IF.
      *
           MOVE ＱＲデータＷ                 TO ＱＲコード.
011710*      MOVE "X" TO EDIT-MODE OF ＱＲコード.
      *
           MOVE 受－患者コード   TO 作５－患者コード.
           MOVE 受－施術和暦年月 TO 作５－施術和暦年月.
           IF 連レ－保険種別 > 50
              MOVE 受－助成種別  TO 作５－保険種別
           ELSE
              MOVE 受－保険種別  TO 作５－保険種別
           END-IF.
           READ 作業ファイル５
           NOT INVALID KEY
              MOVE 作５－６号順番   TO 区分１
              MOVE 作５－７号順番   TO 区分２
              MOVE 作５－患者順番   TO 区分３
              MOVE "-"              TO 区切１ 区切２
              IF 作５－分類コード = 3 OR 4 OR 6
                 MOVE "×"          TO 無
              END-IF
              START 作業ファイル５ KEY IS >= 作５－６号順番
                                             作５－７号順番
                                             作５－患者順番
              END-START
              READ 作業ファイル５ NEXT
              NOT AT END
                 READ 作業ファイル５ NEXT
                 AT END
                    MOVE "〆"             TO 締め
                 NOT AT END
                    IF (作５－６号順番 NOT = 区分１) OR
                       (作５－７号順番 NOT = 区分２)
                       MOVE "〆"          TO 締め
                    END-IF
                 END-READ
              END-READ
           END-READ.
022150************************
022160* レセプト並び順セット *
022170************************
022180     MOVE 順番Ｗ                 TO 順番.
022190*
022200*-------------------------------------------------------------------------*
022210*--- ※ レセ摘要再セットは、この印刷セットSECTION の最後にやること！ -----*
022220     PERFORM レセ摘要再セット.
022230*-------------------------------------------------------------------------*
022240*
022250*--- TEST ---*
022260*******     PERFORM テスト印字処理.
022270*
022280*================================================================*
022290 項目初期化 SECTION.
022300*================================================================*
022310     INITIALIZE 施術所情報Ｗ.
022320     INITIALIZE 受診者情報Ｗ.
022330     INITIALIZE 負傷情報Ｗ.
022340     INITIALIZE 料金情報Ｗ.
022350*     INITIALIZE 負担率チェックＷ.
022360     INITIALIZE 備考情報Ｗ.
022370*
022380     INITIALIZE 料金１ＷＲ.
022390     INITIALIZE 料金２ＷＲ.
022400     INITIALIZE 料金３ＷＲ.
022410*
022420     MOVE SPACE TO YHN6121P.
022430*****     INITIALIZE YHN6121P.
022440*
021920*================================================================*
021930 基本情報取得 SECTION.
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
022450*================================================================*
022460 施術所情報取得 SECTION.
022470*================================================================*
022480**************************************************
022490* 本院データを使用し、以下の情報を取得           *
022500* ● 柔整師番号.. 柔整師番号Ｗに格納             *
022510* ● 会員番号 ... 接骨師会会員番号Ｗに格納       *
022520* ● 代表者名 ... 代表者名Ｗに格納               *
022530* ● 住所1,2   ...施術所住所1,2Ｗに格納          *
022540* ● 電話番号 ... 施術所電話番号Ｗに格納         *
022550**************************************************
022560     MOVE ZERO  TO 施情－施術所番号.
022570     READ 施術所情報マスタ
022580     INVALID KEY
022590         CONTINUE
022600     NOT INVALID KEY
022610*
022650         MOVE 施情－新柔整師番号  TO 柔整師番号Ｗ
023280* 共済・自衛官の時のみ、柔整師番号の編集をする。
023290         IF ( レセプト種類Ｗ = "ROUJ"  OR "JYOS" )
023300            CONTINUE
023310         ELSE
023320            EVALUATE 保険種別ＷＲ 
023330            WHEN  04
023340                PERFORM 共済番号セット
023350            WHEN  09
023360                PERFORM 自衛官番号セット
023370            END-EVALUATE
023380         END-IF
022670*
               MOVE 施情－都道府県ＪＩＳ    TO 都道府県ＪＩＳＷ
022680         MOVE 施情－接骨師会会員番号  TO 接骨師会会員番号Ｗ
022690*
022700         MOVE 施情－代表者カナ        TO 代表者カナＷ
022710         MOVE 施情－代表者名          TO 代表者名Ｗ
022720         MOVE 施情－接骨院名          TO 接骨院名Ｗ
022730*
022740         MOVE 施情－郵便番号１        TO 施術所郵便番号１Ｗ
022750         MOVE 施情－郵便番号２        TO 施術所郵便番号２Ｗ
022760         MOVE 施情－住所１            TO 施術所住所１Ｗ
022770         MOVE 施情－住所２            TO 施術所住所２Ｗ
022780         MOVE 施情－電話番号          TO 施術所電話番号Ｗ
022790*
022800         MOVE 施情－取引先銀行名      TO 取引先銀行名Ｗ
022810         MOVE 施情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
022820         MOVE 施情－預金種別          TO 預金種別Ｗ
022830         MOVE 施情－口座番号          TO 口座番号Ｗ
022840         MOVE 施情－口座名義人        TO 口座名義人Ｗ
022850         MOVE 施情－口座名義人カナ    TO 口座名義人カナＷ
023490*
023500** 振込先情報  / 会情報マスタより振込先情報を取得 /
023520         MOVE ZERO  TO  会情－柔整鍼灸区分
023510         MOVE 44    TO  会情－協会コード
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
               END-READ.
022860*
               MOVE  取引先銀行名Ｗ     TO 金融機関名Ｗ
               MOVE  取引先銀行支店名Ｗ TO 支店名Ｗ
022920*
022930         EVALUATE 預金種別Ｗ
022940         WHEN 1
022950             MOVE "(普)" TO 預金種別コメントＷ
022960         WHEN 2
022970             MOVE "(当)" TO 預金種別コメントＷ
022980         WHEN OTHER
022990             MOVE SPACE  TO 預金種別コメントＷ
023000         END-EVALUATE
023010*
023020*********************************************
023030** ＩＤ管理マスタより　県施術ＩＤを取得する。
023040*   (国保組合は、対象外)
023050*********************************************
023060**   / 県施術ID /
023070*         IF ( 保険者番号ＷＲ(3:1) NOT = "3" )
023080*            MOVE 01                  TO ＩＤ管－ＩＤ区分
023090*            MOVE ZERO                TO ＩＤ管－施術所番号
023100*            MOVE 保険者番号ＷＲ(1:2) TO ＩＤ管－保険種別
023110*            MOVE SPACE               TO ＩＤ管－保険者番号
023120*            READ ＩＤ管理マスタ
023130*            NOT INVALID KEY
023140*                MOVE ＩＤ管－施術ＩＤ番号 TO 県施術ＩＤＷ
023150*            END-READ
023160*         END-IF
023170*     END-READ.
      */現状は振込のみ対応
           MOVE NC"○" TO 振込チェックＷ.
      *
           EVALUATE 預金種別Ｗ
           WHEN 1
               MOVE NC"○" TO 普通チェックＷ
           WHEN 2
               MOVE NC"○" TO 当座チェックＷ
           END-EVALUATE.
      *
009745     IF 取引先銀行名Ｗ NOT = SPACE
009746        PERFORM VARYING カウンタ FROM 40 BY -1
009747                  UNTIL (取引先銀行名Ｗ(カウンタ:1) NOT = SPACE) OR
009748                        (カウンタ <= ZERO)
009749            CONTINUE
009750        END-PERFORM
009751        IF カウンタ > 4
009752           IF 取引先銀行名Ｗ(カウンタ - 3 : 4)  = "銀行"
009753              MOVE  取引先銀行名Ｗ(1:カウンタ - 4)   TO 金融機関名Ｗ
009754              MOVE NC"○" TO 銀行チェックＷ
009755           ELSE
009756              IF 取引先銀行名Ｗ(カウンタ - 3 : 4)  = "金庫"
009757                 MOVE  取引先銀行名Ｗ(1:カウンタ - 4)   TO 金融機関名Ｗ
009758                 MOVE NC"○" TO 金庫チェックＷ
009759              ELSE
009760                 IF 取引先銀行名Ｗ(カウンタ - 3 : 4)  = "農協"
009761                    MOVE  取引先銀行名Ｗ(1:カウンタ - 4)   TO 金融機関名Ｗ
009762                    MOVE NC"○" TO 農協チェックＷ
009763                 ELSE
009764                    MOVE  取引先銀行名Ｗ  TO 金融機関名Ｗ
009765                 END-IF
009766              END-IF
009767           END-IF
009768        ELSE
009769           MOVE  取引先銀行名Ｗ  TO 金融機関名Ｗ
009770        END-IF
009771     END-IF.
009779*
009780     IF 取引先銀行支店名Ｗ NOT = SPACE
009781        PERFORM VARYING カウンタ FROM 40 BY -1
009782                  UNTIL (取引先銀行支店名Ｗ(カウンタ:1) NOT = SPACE) OR
009783                        (カウンタ <= ZERO)
009784            CONTINUE
009785        END-PERFORM
009786        IF カウンタ >= 4
009787           IF 取引先銀行支店名Ｗ(カウンタ - 3 : 4)  = "本店"
009788              MOVE  取引先銀行支店名Ｗ(1:カウンタ - 4)   TO 支店名Ｗ
009789              MOVE NC"○" TO 本店チェックＷ
009790           ELSE
009791              IF 取引先銀行支店名Ｗ(カウンタ - 3 : 4)  = "支店"
009792                 MOVE  取引先銀行支店名Ｗ(1:カウンタ - 4)   TO 支店名Ｗ
009793                 MOVE NC"○" TO 支店チェックＷ
009794              ELSE
009791                 IF 取引先銀行支店名Ｗ(カウンタ - 3 : 4)  = "支所"
009792                    MOVE  取引先銀行支店名Ｗ(1:カウンタ - 4)   TO 支店名Ｗ
009793                    MOVE NC"○" TO 本支所チェックＷ
009794                 ELSE
009791                     IF 取引先銀行支店名Ｗ(カウンタ - 3 : 4)  = "本所"
009792                        MOVE  取引先銀行支店名Ｗ(1:カウンタ - 4)   TO 支店名Ｗ
009793                        MOVE NC"○" TO 本支所チェックＷ
009794                     ELSE
009800                         MOVE  取引先銀行支店名Ｗ  TO 支店名Ｗ
009801                     END-IF
009804                 END-IF
009805              END-IF
009806           END-IF
009807        ELSE
009808           MOVE  取引先銀行支店名Ｗ  TO 支店名Ｗ
009809        END-IF
009810     END-IF.
      *
023820* 固定印字
      */会名称変更/20210824
      *     MOVE "また、療養費の受領を(株)ワールドSPS保険協会 代"       TO 会長委任文１Ｗ.
      *     MOVE "表取締役 西村 陽介(大阪市住吉区長居東4-12-5サン"      TO 会長委任文２Ｗ.
           MOVE "また、療養費の受領を(株)ワールド保険協会 代表"       TO 会長委任文１Ｗ.
           MOVE "取締役 西村 陽介(大阪市住吉区長居東4-12-5サン"      TO 会長委任文２Ｗ.
           MOVE "シャインビル1F)に委任します。"                        TO 会長委任文３Ｗ.
      *
      *     MOVE "                    ワールドSPS保険協会"              TO 委任情報１.
      *     MOVE "                      理事長 栗山 健司 "              TO 委任情報２.
      */会名称変更/20210824
      *     MOVE "                    ワールドSPS保険協会"              TO 委任情報１.
           MOVE "                       ワールド保険協会"              TO 委任情報１.
           MOVE "    〒558-0004 大阪市住吉区長居東4-12-5"              TO 委任情報２.
           MOVE "                           サンシャインビル1F"        TO 委任情報３.
           MOVE "                           (電話)06-6586-9155"        TO 委任情報４.
023180*
023970*================================================================*
023980 共済番号セット SECTION.
023990*
024000**************************************************************
024010* 保険者番号により、共済の番号を印字するか、柔整師番号か判定
024020**************************************************************
024030** 1.共済組合連盟
024040     MOVE SPACE  TO  脱出フラグ.
024050     IF ( 施情－共済連番号 NOT = ZERO )
024060** 条件(保険者番号)
024070        IF ( 保険者番号ＷＲ(1:2) = "31" )  OR
024080           ( 保険者番号ＷＲ = "34130021" )
024090*
024100           MOVE  NC"共済組合連盟第"   TO 共済連番号名ＮＷ 
024110           MOVE  NC"号"               TO 共済連番号単位ＮＷ 
024120           MOVE  施情－共済連番号     TO 共済連番号Ｗ
024130           IF    (共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
024140                 MOVE SPACE TO  共済連番号Ｗ(1:1)
024150           ELSE
024160                 MOVE "YES" TO  脱出フラグ
024170           END-IF
024180           IF    (共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
024190                 MOVE SPACE TO  共済連番号Ｗ(2:1)
024200           ELSE
024210                 MOVE "YES" TO  脱出フラグ
024220           END-IF
024230           IF    (共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
024240                 MOVE SPACE TO  共済連番号Ｗ(3:1)
024250           ELSE
024260                 MOVE "YES" TO  脱出フラグ
024270           END-IF
024280           IF    (共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
024290                 MOVE SPACE TO  共済連番号Ｗ(4:1)
024300           ELSE
024310                 MOVE "YES" TO  脱出フラグ
024320           END-IF
024330           IF    (共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
024340                 MOVE SPACE TO  共済連番号Ｗ(5:1)
024350           ELSE
024360                 MOVE "YES" TO  脱出フラグ
024370           END-IF
024380           IF    (共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
024390                 MOVE SPACE TO  共済連番号Ｗ(6:1)
024400           ELSE
024410                 MOVE "YES" TO  脱出フラグ
024420           END-IF
027560**/共済時両方印字する/090608
027570*           MOVE  柔整師番号Ｗ         TO 柔整師番号２Ｗ
027580*           MOVE  共済連番号集団Ｗ     TO 柔整師番号Ｗ
024110            MOVE  共済連番号集団Ｗ     TO 共済番号Ｗ
024440        END-IF
024450     END-IF.
024460*
027620** 2. 地共済協議会
027630     MOVE SPACE  TO  脱出フラグ.
027640     IF 施情－地共済連番号 NOT = ZERO
027650** 条件(保険者番号)
027660        IF ( 保険者番号ＷＲ(1:2) = "32" OR "33" OR "34" )  AND
027670           ( 保険者番号ＷＲ NOT = "34130021" )
027680*
027690           MOVE  NC"地共済協議会"     TO 共済連番号名ＮＷ 
027700           MOVE  NC"号"               TO 共済連番号単位ＮＷ 
027710           MOVE  施情－地共済連番号   TO 共済連番号Ｗ
027720           IF    (共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
027730                 MOVE SPACE TO  共済連番号Ｗ(1:1)
027740           ELSE
027750                 MOVE "YES" TO  脱出フラグ
027760           END-IF
027770           IF    (共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
027780                 MOVE SPACE TO  共済連番号Ｗ(2:1)
027790           ELSE
027800                 MOVE "YES" TO  脱出フラグ
027810           END-IF
027820           IF    (共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
027830                 MOVE SPACE TO  共済連番号Ｗ(3:1)
027840           ELSE
027850                 MOVE "YES" TO  脱出フラグ
027860           END-IF
027870           IF    (共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
027880                 MOVE SPACE TO  共済連番号Ｗ(4:1)
027890           ELSE
027900                 MOVE "YES" TO  脱出フラグ
027910           END-IF
027920           IF    (共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
027930                 MOVE SPACE TO  共済連番号Ｗ(5:1)
027940           ELSE
027950                 MOVE "YES" TO  脱出フラグ
027960           END-IF
027970           IF    (共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
027980                 MOVE SPACE TO  共済連番号Ｗ(6:1)
027990           ELSE
028000                 MOVE "YES" TO  脱出フラグ
028010           END-IF
028020**/共済時両方印字する/090608
028030*           MOVE  柔整師番号Ｗ         TO 柔整師番号２Ｗ
028040*           MOVE  共済連番号集団Ｗ     TO 柔整師番号Ｗ
024110            MOVE  共済連番号集団Ｗ     TO 共済番号Ｗ
                  MOVE "地共"                TO 保険種別Ｗ
028050        END-IF
028060     END-IF.
024900*
024910*================================================================*
024920 自衛官番号セット SECTION.
024930*
028110     MOVE SPACE  TO  脱出フラグ.
028120     IF 施情－自衛官番号 NOT = ZERO
028130           IF 施情－防衛省区分 = 1
028140              MOVE  NC"防衛省第"      TO 自衛官番号名ＮＷ 
028150           ELSE
028160              MOVE  NC"防衛庁第"      TO 自衛官番号名ＮＷ 
028170           END-IF
028180           MOVE  NC"号"               TO 自衛官番号単位ＮＷ 
028190           MOVE  施情－自衛官番号     TO 自衛官番号Ｗ
028200           IF    (自衛官番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
028210                 MOVE SPACE TO  自衛官番号Ｗ(1:1)
028220           ELSE
028230                 MOVE "YES" TO  脱出フラグ
028240           END-IF
028250           IF    (自衛官番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
028260                 MOVE SPACE TO  自衛官番号Ｗ(2:1)
028270           ELSE
028280                 MOVE "YES" TO  脱出フラグ
028290           END-IF
028300           IF    (自衛官番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
028310                 MOVE SPACE TO  自衛官番号Ｗ(3:1)
028320           ELSE
028330                 MOVE "YES" TO  脱出フラグ
028340           END-IF
028350           IF    (自衛官番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
028360                 MOVE SPACE TO  自衛官番号Ｗ(4:1)
028370           ELSE
028380                 MOVE "YES" TO  脱出フラグ
028390           END-IF
028400           IF    (自衛官番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
028410                 MOVE SPACE TO  自衛官番号Ｗ(5:1)
028420           ELSE
028430                 MOVE "YES" TO  脱出フラグ
028440           END-IF
028450           IF    (自衛官番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
028460                 MOVE SPACE TO  自衛官番号Ｗ(6:1)
028470           ELSE
028480                 MOVE "YES" TO  脱出フラグ
028490           END-IF
028500*           MOVE  自衛官番号集団Ｗ     TO 柔整師番号Ｗ
028500         MOVE  自衛官番号集団Ｗ     TO 共済番号Ｗ
028510     END-IF.
025310*
023190*================================================================*
023200 受診者情報取得 SECTION.
023210*================================================================*
023220**************************************************
023230* 連結データから受診者情報Ｆより以下の情報を取得 *
023240* ● 施術年 ..... 施術年Ｗに格納                 *
023250* ● 施術月 ..... 施術月Ｗに格納                 *
023260* ● 記号 ....... 記号Ｗに格納                   *
023270* ● 番号 ....... 番号Ｗに格納                   *
023280* ● 保険者番号 . 保険者番号Ｗに格納             *
023290* ● 保険種別 ... 保険種別Ｗに格納               *
023300* ● 被保険者カナ.被保険者カナＷに格納           *
023310* ● 被保険者氏名.被保険者氏名Ｗに格納           *
023320* ● 住所１ ......被保険者住所１Ｗに格納         *
023330* ● 住所２ ......被保険者住所２Ｗに格納         *
023340* ● 患者カナ ....患者カナＷに格納               *
023350* ● 患者氏名 ....患者氏名Ｗに格納               *
023360* ● 患者性別 ....区分によりチェックに"○"を格納 *
023370* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
023380* ● 患者年 ......患者年Ｗに格納                 *
023390* ● 患者月 ......患者月Ｗに格納                 *
023400* ● 患者日 ......患者日Ｗに格納                 *
023410* ● 続柄 ........名称マスタより続柄Ｗに取得     *
023420**************************************************
           IF 受－レコード NOT = SPACE
               IF (受－助成種別 = 53 ) AND (受－費用負担者番号助成(1:5) = "39261")
                   EVALUATE レセ－負担割合
                   WHEN 0
                       MOVE "０"    TO 割合Ｗ
                   WHEN 1
                       MOVE "１"    TO 割合Ｗ
                   WHEN 2
                       MOVE "２"    TO 割合Ｗ
                   WHEN 3
                       MOVE "３"    TO 割合Ｗ
                   END-EVALUATE
                   STRING "健康管理費　　"   DELIMITED BY SIZE
                          割合Ｗ             DELIMITED BY SIZE
                          "割"               DELIMITED BY SIZE
                     INTO タイトルＷ
                   END-STRING
                   MOVE ALL "="      TO 取消線Ｗ
               END-IF
      */元号修正/20190426
               MOVE 受－施術和暦     TO 施術和暦Ｗ
023520         MOVE 受－施術年       TO 施術年Ｗ
023530         MOVE 受－施術月       TO 施術月Ｗ
023540*         MOVE 受－記号         TO 記号Ｗ
023550*         MOVE 受－番号         TO 番号Ｗ
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
               MOVE 受－費用負担者番号助成 TO 公費負担者番号Ｗ
               MOVE 受－受益者番号助成     TO 受給者番号Ｗ
023560** 全国土木の枝番削除
023570         IF ( 受－保険種別 = 01 ) AND ( 受－保険者番号(1:6) = "133033" )
023580            MOVE 受－保険者番号(1:6) TO 保険者番号Ｗ
023590         ELSE
023600            MOVE 受－保険者番号      TO 保険者番号Ｗ
023610         END-IF
022660         EVALUATE 受－保険種別
022670         WHEN 01
022690            MOVE NC"○"        TO 国保チェックＷ
                  MOVE "国保"        TO 保険種別Ｗ
022700         WHEN 02
022710         WHEN 06
022720            MOVE NC"○"        TO 社保チェックＷ
                  MOVE "協会"        TO 保険種別Ｗ
022750         WHEN 07
022720            MOVE NC"○"        TO 社保チェックＷ
                  MOVE "船員"        TO 保険種別Ｗ
022730         WHEN 03
022740            MOVE NC"○"        TO 組合チェックＷ
                  MOVE "組合"        TO 保険種別Ｗ
               WHEN 04
                  MOVE NC"○"        TO 共済チェックＷ
                  MOVE "共済"        TO 保険種別Ｗ
               WHEN 09
                  MOVE NC"○"        TO 自チェックＷ
                  MOVE "防衛"        TO 保険種別Ｗ
               WHEN 08
                  MOVE NC"○"        TO 退職チェックＷ
                  MOVE "退職"        TO 保険種別Ｗ
               WHEN 05
                  MOVE NC"○"        TO 後期チェックＷ
                  MOVE "後期"        TO 保険種別Ｗ
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
      */給付割合は全て○付けする/110408
               EVALUATE レセ－給付割合
               WHEN 10
                   MOVE NC"○" TO １０割チェックＷ
               WHEN 9
                   MOVE NC"○" TO ９割チェックＷ
      */神奈川県の場合、前期高齢者１割は、給付割合を８割にする。(国が１割負担するため、患者１割、保険者８割、国１割となる)
                   IF (受－保険種別     = 01 AND 受－保険者番号(1:2) = "14") OR
                      (受－保険種別 NOT = 01 AND 受－保険者番号(3:2) = "14")
                       IF (受－保険種別 NOT = 05 ) AND (受－特別区分 = 1)
                           MOVE SPACE  TO ９割チェックＷ
                           MOVE NC"○" TO ８割チェックＷ
                       END-IF
                   END-IF
               WHEN 8
                   MOVE NC"○" TO ８割チェックＷ
               WHEN 7
                   MOVE NC"○" TO ７割チェックＷ
               END-EVALUATE
023620**
023630         MOVE 受－被保険者カナ       TO 被保険者カナＷ
023640         MOVE 受－被保険者氏名       TO 被保険者氏名Ｗ
023650         MOVE 受－郵便番号１         TO 郵便番号１Ｗ
023660         MOVE 受－郵便番号２         TO 郵便番号２Ｗ
023670         MOVE 受－住所１             TO 被保険者住所１Ｗ
023680         MOVE 受－住所２             TO 被保険者住所２Ｗ
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
023690         MOVE 受－患者カナ           TO 患者カナＷ
023700         MOVE 受－患者氏名           TO 患者氏名Ｗ
023710         MOVE 受－患者郵便番号１     TO 患者郵便番号１Ｗ
023720         MOVE 受－患者郵便番号２     TO 患者郵便番号２Ｗ
023730         MOVE 受－患者住所１         TO 患者住所１Ｗ
023740         MOVE 受－患者住所２         TO 患者住所２Ｗ
023750*
023760         EVALUATE 受－患者性別
023770         WHEN 1
023780             MOVE NC"男"  TO 性別Ｗ
023790             MOVE NC"○"  TO 男チェックＷ
023800         WHEN 2
023810             MOVE NC"女"  TO 性別Ｗ
023820             MOVE NC"○"  TO 女チェックＷ
023830         END-EVALUATE
023840*
023850         MOVE 受－患者和暦  TO 患者和暦Ｗ
023860         EVALUATE 受－患者和暦
023870         WHEN 1
023880             MOVE NC"明治"  TO 患者和暦名称Ｗ
023890             MOVE NC"○"    TO 明治チェックＷ
023900         WHEN 2
023910             MOVE NC"大正"  TO 患者和暦名称Ｗ
023920             MOVE NC"○"    TO 大正チェックＷ
023930         WHEN 3
023940             MOVE NC"昭和"  TO 患者和暦名称Ｗ
023950             MOVE NC"○"    TO 昭和チェックＷ
023960         WHEN 4
023970             MOVE NC"平成"  TO 患者和暦名称Ｗ
023980             MOVE NC"○"    TO 平成チェックＷ
      */元号修正/20190426
023060         WHEN 5
                   MOVE "5令"   TO 令和ＣＭＷ
023070             MOVE NC"○"  TO 令和チェックＷ
023990         END-EVALUATE
024000*
      */元号修正/↓↓↓20190426
029310         IF 受－患者和暦 > 4
037370             MOVE 受－患者和暦     TO 元－元号区分
037380             READ 元号マスタ
037390             NOT INVALID KEY
037400                 MOVE 元－元号名称 TO 元号Ｗ
037410             END-READ
029330         END-IF
      */元号修正/↑↑↑20190426
024010         MOVE 受－患者年  TO 患者年Ｗ
024020         MOVE 受－患者月  TO 患者月Ｗ
024030         MOVE 受－患者日  TO 患者日Ｗ
024040*
      */レセまとめに対応/101108
030020         IF 受－助成種別 NOT = ZERO
030030            PERFORM 助成レセまとめ判定
030040         ELSE
030050            MOVE SPACE TO 助成レセまとめフラグ
030060         END-IF
029002*
024260     END-IF.
024270*
025540*================================================================*
025550 負傷データ取得 SECTION.
025560*================================================================*
025570**************************************************
025580* 連結データから負傷データＦより以下の情報を取得 *
025590* ● 負傷名...部位＋負傷種別にて加工して格納     *
025600* ● 負傷年.......負傷年Ｗ                       *
025610* ● 負傷月.......負傷月Ｗ                       *
025620* ● 負傷日.......負傷日Ｗ                       *
025630* ● 開始年.......初検年Ｗ                       *
025640* ● 開始月.......初検月Ｗ                       *
025650* ● 開始日.......初検日Ｗ                       *
025660* ● 終了年.......終了年Ｗ                       *
025670* ● 終了月.......終了月Ｗ                       *
025680* ● 終了日.......終了日Ｗ                       *
025690* ● 実日数.......実日数Ｗ                       *
025700* ● 転帰区分 ....区分によりチェックに"○"を格納 *
025710* ● 金属副子 ....区分によりチェックに"○"を格納 *
025720* ● 経過コード...経過マスタより取得             *
025730**************************************************
           IF 負－レコード NOT = SPACE
025830         MOVE 負－部位数                   TO 部位数Ｗ
025840         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
025850                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
025860             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別Ｗ(部位ＣＮＴ)
025870             MOVE 負－部位(部位ＣＮＴ)     TO 部位Ｗ(部位ＣＮＴ)
025880             MOVE 負－左右区分(部位ＣＮＴ) TO 左右区分Ｗ(部位ＣＮＴ)
025890             MOVE 負－負傷位置番号(部位ＣＮＴ)
025900                                           TO 負傷位置番号Ｗ(部位ＣＮＴ)
025910*********************************************
025920* 注）全柔...負傷種別＋部位にて加工して格納 *
025930*********************************************
025940* 負傷種別
025950             MOVE SPACE                     TO 負傷名称Ｗ
025960             MOVE 03                        TO 名－区分コード
025970             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
025980             READ 名称マスタ
025990             INVALID KEY
026000                 MOVE SPACE        TO 負傷名称Ｗ
026010             NOT INVALID KEY
026020                 MOVE 名－正式名称 TO 負傷名称Ｗ
026030             END-READ
026040* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
032680*
032690             PERFORM 部位名称埋込処理
026230*
026240             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
026250             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
026260             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
026270             MOVE 負－開始年(部位ＣＮＴ)   TO 初検年Ｗ(部位ＣＮＴ)
026280             MOVE 負－開始月(部位ＣＮＴ)   TO 初検月Ｗ(部位ＣＮＴ)
026290             MOVE 負－開始日(部位ＣＮＴ)   TO 初検日Ｗ(部位ＣＮＴ)
026300             IF ( 負－転帰区分(部位ＣＮＴ) = 9 )
032900                 MOVE 9                    TO 終了和暦Ｗ(部位ＣＮＴ)
026310                 MOVE 99                   TO 終了年Ｗ(部位ＣＮＴ)
026320                 MOVE 99                   TO 終了月Ｗ(部位ＣＮＴ)
026330                 MOVE 99                   TO 終了日Ｗ(部位ＣＮＴ)
026340             ELSE
032940                 MOVE 負－終了和暦(部位ＣＮＴ) TO 終了和暦Ｗ(部位ＣＮＴ)
026350                 MOVE 負－終了年(部位ＣＮＴ)   TO 終了年Ｗ(部位ＣＮＴ)
026360                 MOVE 負－終了月(部位ＣＮＴ)   TO 終了月Ｗ(部位ＣＮＴ)
026370                 MOVE 負－終了日(部位ＣＮＴ)   TO 終了日Ｗ(部位ＣＮＴ)
026380             END-IF
026390* 経過略称取得
026400             MOVE 01                         TO 経－区分コード
026410             MOVE 負－経過コード(部位ＣＮＴ) TO 経－経過コード
026420             READ 経過マスタ
026430             INVALID KEY
026440                 MOVE ZERO       TO 部位ＣＮＴＷ(部位ＣＮＴ)
026450                 MOVE SPACE      TO 部位区切Ｗ(部位ＣＮＴ)
026460                 MOVE SPACE      TO 経過略称Ｗ(部位ＣＮＴ)
026470             NOT INVALID KEY
026480*
026490                 EVALUATE 部位ＣＮＴ
026500                 WHEN 1
026510                     MOVE NC"①" TO 経過部位Ｗ
026520                 WHEN 2
026530                     MOVE NC"②" TO 経過部位Ｗ
026540                 WHEN 3
026550                     MOVE NC"③" TO 経過部位Ｗ
026560                 WHEN 4
026570                     MOVE NC"④" TO 経過部位Ｗ
026580                 WHEN 5
026590                     MOVE NC"⑤" TO 経過部位Ｗ
026600                 END-EVALUATE
026610                 STRING  経過部位Ｗ     DELIMITED BY SPACE
026620                         経－経過略称   DELIMITED BY SPACE
026630                        INTO 印刷経過略称Ｗ(部位ＣＮＴ)
026640                 END-STRING
026650*
026660             END-READ
026670*
026680             MOVE 負－転帰区分(部位ＣＮＴ) TO 転帰区分Ｗ(部位ＣＮＴ)
026690             EVALUATE 負－転帰区分(部位ＣＮＴ)
026700             WHEN 1
026710             WHEN 2
026720                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
026730             WHEN 3
026740                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
026750             WHEN 4
026760                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
026770             END-EVALUATE
026780*
026790         END-PERFORM
026800* 新規/継続 チェック
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
026860*
026870* 枝番判定用
026880         MOVE 負－開始診療日手動区分   TO 開始診療日手動区分Ｗ
026890* 負傷原因印刷区分
026900         MOVE 負－レセ負傷原因印刷区分 TO レセ負傷原因印刷区分Ｗ
027880         MOVE 負－レセ長期理由印刷区分 TO レセ長期理由印刷区分Ｗ
026910*
026920     END-IF.
026930*
026940*================================================================*
026950 部位名称埋込処理 SECTION.
026960*
006490     STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
009980            負傷名称Ｗ                    DELIMITED BY SPACE
006500            レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
006520       INTO 負傷名Ｗ(部位ＣＮＴ)
006570     END-STRING.
027130*
027560*================================================================*
027570 料金情報取得 SECTION.
027580*================================================================*
027590********************
027600* 料金データセット *
027610********************
027620*    ****************************************************************
027630*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
027640*    ****************************************************************
027650     MOVE レセ－初検料                 TO 初検料ＷＲ.
027660     IF ( レセ－時間外 = 1 )
027670         MOVE NC"○"                   TO 時間外チェックＷ
027680     END-IF.
027690     IF ( レセ－休日 = 1 )
027700         MOVE NC"○"                   TO 休日チェックＷ
027710     END-IF.
027720     IF ( レセ－深夜 = 1 )
027730         MOVE NC"○"                   TO 深夜チェックＷ
027740     END-IF.
           MOVE レセ－初検時相談料           TO 相談料ＷＲ.
027750*
027760     MOVE レセ－初検加算料             TO  初検加算料ＷＲ.
027770     MOVE レセ－再検料                 TO  再検料ＷＲ.
027780     MOVE レセ－往療距離               TO  往療距離ＷＲ.
027790     MOVE レセ－往療回数               TO  往療回数ＷＲ.
027800     MOVE レセ－往療料                 TO  往療料ＷＲ.
027810     MOVE レセ－往療加算料             TO  往療加算料ＷＲ.
027820*
027830     IF ( レセ－夜間 = 1 )
027840         MOVE NC"○"                   TO 夜間チェックＷ
027850     END-IF.
027860     IF ( レセ－暴風雨雪 = 1 )
027870         MOVE NC"○"                   TO 暴風雨雪チェックＷ
027880     END-IF.
027890     IF ( レセ－難路 = 1 )
027900        MOVE NC"○"                    TO 難路チェックＷ
027910     END-IF.
027920*
027930     MOVE レセ－金属副子加算料         TO  金属副子加算料ＷＲ.
           MOVE レセ－金属副子回数            TO 金属回数Ｗ.
           MOVE レセ－運動後療回数            TO 運動回数Ｗ.
           MOVE レセ－運動後療料              TO 運動料Ｗ.
028040*
028050     MOVE レセ－施術情報提供料         TO  施術情報提供料ＷＲ.
028060* 小計
022420     COMPUTE 小計Ｗ = レセ－小計 + レセ－運動後療料.
028080********************
028090* 初回処置料セット *
028100********************
028110     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
028120             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
028130         MOVE レセ－初回処置料(部位ＣＮＴ) TO 初回処置料ＷＲ(部位ＣＮＴ)
028140         IF ( レセ－初回処置料(部位ＣＮＴ) NOT = ZERO )
028150            EVALUATE 負－負傷種別(部位ＣＮＴ)
028160* 捻挫・打撲・挫傷
028170            WHEN 1
028180            WHEN 2
028190            WHEN 3
028200                MOVE NC"○"            TO 施療料チェックＷ
028210* 脱臼・骨折・骨折拘縮
028220            WHEN 4
028230            WHEN 5
028240            WHEN 7
028250                MOVE NC"○"            TO 整復料チェックＷ
028260* 不全骨折・不全骨折拘縮
028270            WHEN 6
028280            WHEN 8
028290                MOVE NC"○"            TO 固定料チェックＷ
028300            END-EVALUATE
028310         END-IF
028320     END-PERFORM.
028330*
028340     MOVE レセ－初回処置料合計         TO 初回処置料合計Ｗ.
028350********************
028360* 逓減毎料金セット *
028370********************
028380*    **********
028390*    * １部位 *
028400*    **********
028410     MOVE レセ－後療単価１             TO 後療単価１ＷＲ.
028420     MOVE レセ－後療回数１             TO 後療回数１ＷＲ.
028430     MOVE レセ－後療料１               TO 後療料１ＷＲ.
028440     MOVE レセ－冷罨法回数１           TO 冷罨法回数１ＷＲ.
028450     MOVE レセ－冷罨法料１             TO 冷罨法料１ＷＲ.
028460     MOVE レセ－温罨法回数１           TO 温罨法回数１ＷＲ.
028470     MOVE レセ－温罨法料１             TO 温罨法料１ＷＲ.
028480     MOVE レセ－電療回数１             TO 電療回数１ＷＲ.
028490     MOVE レセ－電療料１               TO 電療料１ＷＲ.
028500     MOVE レセ－小計１                 TO 小計１ＷＲ.
           IF レセ－長期頻回逓減率１ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率１   TO 長期逓減率１ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率１       TO 長期逓減率１ＷＲ
           END-IF.
028520     MOVE レセ－長期込小計１           TO 長期込小計１ＷＲ.
028530*    **********
028540*    * ２部位 *
028550*    **********
028560     MOVE レセ－後療単価２             TO 後療単価２ＷＲ.
028570     MOVE レセ－後療回数２             TO 後療回数２ＷＲ.
028580     MOVE レセ－後療料２               TO 後療料２ＷＲ.
028590     MOVE レセ－冷罨法回数２           TO 冷罨法回数２ＷＲ.
028600     MOVE レセ－冷罨法料２             TO 冷罨法料２ＷＲ.
028610     MOVE レセ－温罨法回数２           TO 温罨法回数２ＷＲ.
028620     MOVE レセ－温罨法料２             TO 温罨法料２ＷＲ.
028630     MOVE レセ－電療回数２             TO 電療回数２ＷＲ.
028640     MOVE レセ－電療料２               TO 電療料２ＷＲ.
028650     MOVE レセ－小計２                 TO 小計２ＷＲ.
           IF レセ－長期頻回逓減率２ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率２   TO 長期逓減率２ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率２       TO 長期逓減率２ＷＲ
           END-IF.
028670     MOVE レセ－長期込小計２           TO 長期込小計２ＷＲ.
028680*    ****************
028690*    * ３部位／８割 *
028700*    ****************
028710     MOVE レセ－後療単価３８           TO 後療単価３８ＷＲ.
028720     MOVE レセ－後療回数３８           TO 後療回数３８ＷＲ.
028730     MOVE レセ－後療料３８             TO 後療料３８ＷＲ.
028740     MOVE レセ－冷罨法回数３８         TO 冷罨法回数３８ＷＲ.
028750     MOVE レセ－冷罨法料３８           TO 冷罨法料３８ＷＲ.
028760     MOVE レセ－温罨法回数３８         TO 温罨法回数３８ＷＲ.
028770     MOVE レセ－温罨法料３８           TO 温罨法料３８ＷＲ.
028780     MOVE レセ－電療回数３８           TO 電療回数３８ＷＲ.
028790     MOVE レセ－電療料３８             TO 電療料３８ＷＲ.
028800     MOVE レセ－小計３８               TO 小計３８ＷＲ.
028810     MOVE レセ－多部位込小計３８       TO 多部位込小計３８ＷＲ.
           IF レセ－長期頻回逓減率３８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率３８   TO 長期逓減率３８ＷＲ
           ELSE
024160         MOVE レセ－長期逓減率３８       TO 長期逓減率３８ＷＲ
           END-IF.
028830     MOVE レセ－長期込小計３８         TO 長期込小計３８ＷＲ.
028840*    ****************
028850*    * ３部位／10割 *
028860*    ****************
028870     MOVE レセ－逓減開始月３０         TO 逓減開始月３０ＷＲ.
028880     MOVE レセ－逓減開始日３０         TO 逓減開始日３０ＷＲ.
028890     MOVE レセ－後療単価３０           TO 後療単価３０ＷＲ.
028900     MOVE レセ－後療回数３０           TO 後療回数３０ＷＲ.
028910     MOVE レセ－後療料３０             TO 後療料３０ＷＲ.
028920     MOVE レセ－冷罨法回数３０         TO 冷罨法回数３０ＷＲ.
028930     MOVE レセ－冷罨法料３０           TO 冷罨法料３０ＷＲ.
028940     MOVE レセ－温罨法回数３０         TO 温罨法回数３０ＷＲ.
028950     MOVE レセ－温罨法料３０           TO 温罨法料３０ＷＲ.
028960     MOVE レセ－電療回数３０           TO 電療回数３０ＷＲ.
028970     MOVE レセ－電療料３０             TO 電療料３０ＷＲ.
028980     MOVE レセ－小計３０               TO 小計３０ＷＲ.
           IF レセ－長期頻回逓減率３０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率３０   TO 長期逓減率３０ＷＲ
           ELSE
024330         MOVE レセ－長期逓減率３０       TO 長期逓減率３０ＷＲ
           END-IF.
029000     MOVE レセ－長期込小計３０         TO 長期込小計３０ＷＲ.
029010*    ****************
029020*    * ４部位／５割 *
029030*    ****************
029040     MOVE レセ－後療単価４５           TO 後療単価４５ＷＲ.
029050     MOVE レセ－後療回数４５           TO 後療回数４５ＷＲ.
029060     MOVE レセ－後療料４５             TO 後療料４５ＷＲ.
029070     MOVE レセ－冷罨法回数４５         TO 冷罨法回数４５ＷＲ.
029080     MOVE レセ－冷罨法料４５           TO 冷罨法料４５ＷＲ.
029090     MOVE レセ－温罨法回数４５         TO 温罨法回数４５ＷＲ.
029100     MOVE レセ－温罨法料４５           TO 温罨法料４５ＷＲ.
029110     MOVE レセ－電療回数４５           TO 電療回数４５ＷＲ.
029120     MOVE レセ－電療料４５             TO 電療料４５ＷＲ.
029130     MOVE レセ－小計４５               TO 小計４５ＷＲ.
029140     MOVE レセ－多部位込小計４５       TO 多部位込小計４５ＷＲ.
029150     MOVE レセ－長期逓減率４５         TO 長期逓減率４５ＷＲ.
029160     MOVE レセ－長期込小計４５         TO 長期込小計４５ＷＲ.
029170*    ****************
029180*    * ４部位／８割 *
029190*    ****************
029200     MOVE レセ－逓減開始月４８         TO 逓減開始月４８ＷＲ.
029210     MOVE レセ－逓減開始日４８         TO 逓減開始日４８ＷＲ.
029220     MOVE レセ－後療単価４８           TO 後療単価４８ＷＲ.
029230     MOVE レセ－後療回数４８           TO 後療回数４８ＷＲ.
029240     MOVE レセ－後療料４８             TO 後療料４８ＷＲ.
029250     MOVE レセ－冷罨法回数４８         TO 冷罨法回数４８ＷＲ.
029260     MOVE レセ－冷罨法料４８           TO 冷罨法料４８ＷＲ.
029270     MOVE レセ－温罨法回数４８         TO 温罨法回数４８ＷＲ.
029280     MOVE レセ－温罨法料４８           TO 温罨法料４８ＷＲ.
029290     MOVE レセ－電療回数４８           TO 電療回数４８ＷＲ.
029300     MOVE レセ－電療料４８             TO 電療料４８ＷＲ.
029310     MOVE レセ－小計４８               TO 小計４８ＷＲ.
029320     MOVE レセ－多部位込小計４８       TO 多部位込小計４８ＷＲ.
           IF レセ－長期頻回逓減率４８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率４８   TO 長期逓減率４８ＷＲ
           ELSE
024670         MOVE レセ－長期逓減率４８       TO 長期逓減率４８ＷＲ
           END-IF.
029340     MOVE レセ－長期込小計４８         TO 長期込小計４８ＷＲ.
029350*    ****************
029360*    * ４部位／10割 *
029370*    ****************
029380     MOVE レセ－逓減開始月４０         TO 逓減開始月４０ＷＲ.
029390     MOVE レセ－逓減開始日４０         TO 逓減開始日４０ＷＲ.
029400     MOVE レセ－後療単価４０           TO 後療単価４０ＷＲ.
029410     MOVE レセ－後療回数４０           TO 後療回数４０ＷＲ.
029420     MOVE レセ－後療料４０             TO 後療料４０ＷＲ.
029430     MOVE レセ－冷罨法回数４０         TO 冷罨法回数４０ＷＲ.
029440     MOVE レセ－冷罨法料４０           TO 冷罨法料４０ＷＲ.
029450     MOVE レセ－温罨法回数４０         TO 温罨法回数４０ＷＲ.
029460     MOVE レセ－温罨法料４０           TO 温罨法料４０ＷＲ.
029470     MOVE レセ－電療回数４０           TO 電療回数４０ＷＲ.
029480     MOVE レセ－電療料４０             TO 電療料４０ＷＲ.
029490     MOVE レセ－小計４０               TO 小計４０ＷＲ.
           IF レセ－長期頻回逓減率４０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率４０   TO 長期逓減率４０ＷＲ
           ELSE
024840         MOVE レセ－長期逓減率４０       TO 長期逓減率４０ＷＲ
           END-IF.
029510     MOVE レセ－長期込小計４０         TO 長期込小計４０ＷＲ.
029520*    *****************
029530*    * ５部位／2.5割 *
029540*    *****************
029550*     MOVE レセ－後療単価５２           TO 後療単価５２ＷＲ.
029560*     MOVE レセ－後療回数５２           TO 後療回数５２ＷＲ.
029570*     MOVE レセ－後療料５２             TO 後療料５２ＷＲ.
029580*     MOVE レセ－冷罨法回数５２         TO 冷罨法回数５２ＷＲ.
029590*     MOVE レセ－冷罨法料５２           TO 冷罨法料５２ＷＲ.
029600*     MOVE レセ－温罨法回数５２         TO 温罨法回数５２ＷＲ.
029610*     MOVE レセ－温罨法料５２           TO 温罨法料５２ＷＲ.
029620*     MOVE レセ－電療回数５２           TO 電療回数５２ＷＲ.
029630*     MOVE レセ－電療料５２             TO 電療料５２ＷＲ.
029640*     MOVE レセ－小計５２               TO 小計５２ＷＲ.
029650*     MOVE レセ－多部位込小計５２       TO 多部位込小計５２ＷＲ.
029660*     MOVE レセ－長期逓減率５２         TO 長期逓減率５２ＷＲ.
029670*     MOVE レセ－長期込小計５２         TO 長期込小計５２ＷＲ.
029680*    ****************
029690*    * ５部位／５割 *
029700*    ****************
029710     MOVE レセ－逓減開始月５５         TO 逓減開始月５５ＷＲ.
029720     MOVE レセ－逓減開始日５５         TO 逓減開始日５５ＷＲ.
029730     MOVE レセ－後療単価５５           TO 後療単価５５ＷＲ.
029740     MOVE レセ－後療回数５５           TO 後療回数５５ＷＲ.
029750     MOVE レセ－後療料５５             TO 後療料５５ＷＲ.
029760     MOVE レセ－冷罨法回数５５         TO 冷罨法回数５５ＷＲ.
029770     MOVE レセ－冷罨法料５５           TO 冷罨法料５５ＷＲ.
029780     MOVE レセ－温罨法回数５５         TO 温罨法回数５５ＷＲ.
029790     MOVE レセ－温罨法料５５           TO 温罨法料５５ＷＲ.
029800     MOVE レセ－電療回数５５           TO 電療回数５５ＷＲ.
029810     MOVE レセ－電療料５５             TO 電療料５５ＷＲ.
029820     MOVE レセ－小計５５               TO 小計５５ＷＲ.
029830     MOVE レセ－多部位込小計５５       TO 多部位込小計５５ＷＲ.
029840     MOVE レセ－長期逓減率５５         TO 長期逓減率５５ＷＲ.
029850     MOVE レセ－長期込小計５５         TO 長期込小計５５ＷＲ.
029860*    ****************
029870*    * ５部位／８割 *
029880*    ****************
029890     MOVE レセ－逓減開始月５８         TO 逓減開始月５８ＷＲ.
029900     MOVE レセ－逓減開始日５８         TO 逓減開始日５８ＷＲ.
029910     MOVE レセ－後療単価５８           TO 後療単価５８ＷＲ.
029920     MOVE レセ－後療回数５８           TO 後療回数５８ＷＲ.
029930     MOVE レセ－後療料５８             TO 後療料５８ＷＲ.
029940     MOVE レセ－冷罨法回数５８         TO 冷罨法回数５８ＷＲ.
029950     MOVE レセ－冷罨法料５８           TO 冷罨法料５８ＷＲ.
029960     MOVE レセ－温罨法回数５８         TO 温罨法回数５８ＷＲ.
029970     MOVE レセ－温罨法料５８           TO 温罨法料５８ＷＲ.
029980     MOVE レセ－電療回数５８           TO 電療回数５８ＷＲ.
029990     MOVE レセ－電療料５８             TO 電療料５８ＷＲ.
030000     MOVE レセ－小計５８               TO 小計５８ＷＲ.
030010     MOVE レセ－多部位込小計５８       TO 多部位込小計５８ＷＲ.
           IF レセ－長期頻回逓減率５８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率５８   TO 長期逓減率５８ＷＲ
           ELSE
025360         MOVE レセ－長期逓減率５８       TO 長期逓減率５８ＷＲ
           END-IF.
030030     MOVE レセ－長期込小計５８         TO 長期込小計５８ＷＲ.
030040*    ****************
030050*    * ５部位／10割 *
030060*    ****************
030070     MOVE レセ－逓減開始月５０         TO 逓減開始月５０ＷＲ.
030080     MOVE レセ－逓減開始日５０         TO 逓減開始日５０ＷＲ.
030090     MOVE レセ－後療単価５０           TO 後療単価５０ＷＲ.
030100     MOVE レセ－後療回数５０           TO 後療回数５０ＷＲ.
030110     MOVE レセ－後療料５０             TO 後療料５０ＷＲ.
030120     MOVE レセ－冷罨法回数５０         TO 冷罨法回数５０ＷＲ.
030130     MOVE レセ－冷罨法料５０           TO 冷罨法料５０ＷＲ.
030140     MOVE レセ－温罨法回数５０         TO 温罨法回数５０ＷＲ.
030150     MOVE レセ－温罨法料５０           TO 温罨法料５０ＷＲ.
030160     MOVE レセ－電療回数５０           TO 電療回数５０ＷＲ.
030170     MOVE レセ－電療料５０             TO 電療料５０ＷＲ.
030180     MOVE レセ－小計５０               TO 小計５０ＷＲ.
           IF レセ－長期頻回逓減率５０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率５０   TO 長期逓減率５０ＷＲ
           ELSE
025530         MOVE レセ－長期逓減率５０       TO 長期逓減率５０ＷＲ
           END-IF.
030200     MOVE レセ－長期込小計５０         TO 長期込小計５０ＷＲ.
      */2022
           MOVE レセ－明細書発行加算料         TO 明細書発行加算料ＷＲ.
           MOVE レセ－明細書発行加算日         TO 明細書発行加算日ＷＲ.
           IF レセ－明細書発行加算料 NOT = ZERO
               STRING "明細書発行体制加算"     DELIMITED BY SIZE
                      明細書発行加算料ＷＲ     DELIMITED BY SIZE
                      "円 加算日"              DELIMITED BY SIZE
                      明細書発行加算日ＷＲ     DELIMITED BY SIZE
                      "日"                     DELIMITED BY SIZE
                 INTO 適用２Ｗ
               END-STRING
           END-IF.
030210*
030220*================================================================*
030230 施術記録取得 SECTION.
030240*================================================================*
030250************************************************************
030260* 作１データから負傷データＦより以下の情報を取得           *
030270* ● 初検加算 .....区分によりチェックに"○"を格納...複数可 *
030280* ● 往療加算 .....区分によりチェックに"○"を格納...複数可 *
030290************************************************************
030300     MOVE  SPACE  TO  初日再検フラグ.
030310     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
030320         IF ( 施術年Ｗ = 初検年Ｗ(部位ＣＮＴ) ) AND
030330            ( 施術月Ｗ = 初検月Ｗ(部位ＣＮＴ) )
030340             MOVE 患者番号ＷＲ          TO 施記－患者番号
030350             MOVE 枝番ＷＲ              TO 施記－枝番
030360             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
030370             MOVE 初検年Ｗ(部位ＣＮＴ)  TO 開始年Ｗ(部位ＣＮＴ) 施記－施術年
030380             MOVE 初検月Ｗ(部位ＣＮＴ)  TO 開始月Ｗ(部位ＣＮＴ) 施記－施術月
030390             MOVE 初検日Ｗ(部位ＣＮＴ)  TO 開始日Ｗ(部位ＣＮＴ) 施記－施術日
030400         ELSE
030410             MOVE 患者番号ＷＲ          TO 施記－患者番号
030420             MOVE 枝番ＷＲ              TO 施記－枝番
030430             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
030440             MOVE 施術年ＷＲ            TO 施記－施術年
030450             MOVE 施術月ＷＲ            TO 施記－施術月
030460             MOVE ZERO                  TO 施記－施術日
030470         END-IF
030480         START 施術記録Ｆ   KEY IS >= 施記－患者コード
030490                                      施記－施術和暦年月日
030500         END-START
030510         IF ( 状態キー = "00" )
030520             MOVE ZERO  TO 実日数Ｗ(部位ＣＮＴ)
030830             MOVE ZERO  TO 終了和暦ＷＴ
030530             MOVE ZERO  TO 終了年ＷＴ
030540             MOVE ZERO  TO 終了月ＷＴ
030550             MOVE ZERO  TO 終了日ＷＴ
030560             MOVE SPACE TO 終了フラグ２
030570             PERFORM 施術記録Ｆ読込
030580             IF ( 終了フラグ２      = SPACE   ) AND
030590                ( 施記－患者コード  = 患者コードＷＲ ) AND
030600                ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
030610                ( 施記－施術年      = 施術年ＷＲ     ) AND
030620                ( 施記－施術月      = 施術月ＷＲ     ) 
030630*
030640*        *****************************************************************
030650*        * 開始年月日 ( その部位が当月初検でないか、
030660*                       当月初検でも枝番がある時は、最初の施術日を開始日)*
030670*        *****************************************************************
030680                 IF ( 施術年Ｗ NOT = 初検年Ｗ(部位ＣＮＴ) ) OR
030690                    ( 施術月Ｗ NOT = 初検月Ｗ(部位ＣＮＴ) ) OR
030700                    ( 開始診療日手動区分Ｗ = 1 )
030710                     MOVE 施記－施術年   TO 開始年Ｗ(部位ＣＮＴ)
030720                     MOVE 施記－施術月   TO 開始月Ｗ(部位ＣＮＴ)
030730                     MOVE 施記－施術日   TO 開始日Ｗ(部位ＣＮＴ)
030740                 END-IF
030750             END-IF
030760             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
030770                           ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
030780                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
030790                           ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
030800                           ( 施記－施術月     NOT = 施術月ＷＲ       ) OR
030810                           ( 施記－施術日         > 終了日Ｗ(部位ＣＮＴ))
030820*               **********
030830*               * 実日数 *
030840*               **********
030850                COMPUTE 実日数Ｗ(部位ＣＮＴ) = 実日数Ｗ(部位ＣＮＴ) + 1
031240                MOVE 施記－施術和暦             TO 終了和暦ＷＴ
030860                MOVE 施記－施術年               TO 終了年ＷＴ
030870                MOVE 施記－施術月               TO 終了月ＷＴ
030880                MOVE 施記－施術日               TO 終了日ＷＴ
030890*
030900                PERFORM 施術記録Ｆ読込
030910            END-PERFORM
030920        END-IF
030930*       **************************
030940*       * 継続：終了年月日セット *
030950*       **************************
030960        IF ( 転帰区分Ｗ(部位ＣＮＴ) = 9 )
032090            MOVE 終了和暦ＷＴ  TO 終了和暦Ｗ(部位ＣＮＴ)
030970            MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
030980            MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
030990            MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
031000        END-IF
031010        IF ( 終了年月日Ｗ(部位ＣＮＴ) > 受理年月日Ｗ )
032140            MOVE 終了和暦Ｗ(部位ＣＮＴ) TO 受理和暦Ｗ
031020            MOVE 終了年Ｗ(部位ＣＮＴ) TO 受理年Ｗ
031030            MOVE 終了月Ｗ(部位ＣＮＴ) TO 受理月Ｗ
031040            MOVE 終了日Ｗ(部位ＣＮＴ) TO 受理日Ｗ
031050        END-IF
031060     END-PERFORM.
031070*
031080** ----- 前月初検のみかを判定 -----------*
031090*
031100*     MOVE 患者番号ＷＲ          TO 施記－患者番号.
031110*     MOVE 枝番ＷＲ              TO 施記－枝番.
031120*     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
031130*     MOVE 施術年ＷＲ            TO 施記－施術年.
031140*     MOVE 施術月ＷＲ            TO 施記－施術月.
031150*     MOVE ZERO                  TO 施記－施術日.
031160*     START 施術記録Ｆ   KEY IS >= 施記－患者コード
031170*                                  施記－施術和暦年月日
031180*     END-START.
031190*     IF ( 状態キー = "00" )
031200*             MOVE SPACE TO 終了フラグ２
031210*             PERFORM 施術記録Ｆ読込
031220*             IF ( 終了フラグ２      = SPACE   ) AND
031230*                ( 施記－患者コード  = 患者コードＷＲ ) AND
031240*                ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
031250*                ( 施記－施術年      = 施術年ＷＲ     ) AND
031260*                ( 施記－施術月      = 施術月ＷＲ     ) 
031270** 当月施術開始日が再検かどうか判定
031280*                 IF ( 施記－再検料請求 = 1 )
031290*                      MOVE "YES"  TO  初日再検フラグ
031300*                 END-IF
031310**
031320*             END-IF
031330*     END-IF.
031340*     IF ( 初日再検フラグ = "YES" )
031350*        PERFORM 前月初検のみ判定
031360*     END-IF.
031370*
031380*================================================================*
031390 前月初検のみ判定 SECTION.
031400*
031410*** 前月の通院日が初検か判定 
031420     MOVE  SPACE            TO 前月フラグ.
031430     MOVE 受－患者コード    TO 施記－患者コード.
031440     MOVE 受－施術和暦      TO 施記－施術和暦.
031450     MOVE 受－施術年        TO 施記－施術年.
031460     MOVE 受－施術月        TO 施記－施術月.
031470     MOVE 1                 TO 施記－施術日.
031480     START 施術記録Ｆ   KEY IS <  施記－患者コード
031490                                  施記－施術和暦年月日
031500                                  REVERSED
031510     END-START.
031520     IF ( 状態キー = "00" )
031530         MOVE SPACE  TO 終了フラグ２
031540         PERFORM 施術記録Ｆ読込
031550         IF ( 終了フラグ２      = SPACE  ) AND
031560            ( 施記－患者コード  = 受－患者コード ) AND
031570            ( 施記－診療区分    = 2 ) 
031580*
031590            PERFORM 前月判定
031600**** 適用１を使用
031610            IF ( 前月フラグ = "YES" )
031620               MOVE NC"※前月初検のみ"    TO  適用１Ｗ
031630            END-IF
031640**
031650         END-IF
031660     END-IF.
031670*
031680*================================================================*
031690 前月判定  SECTION.
031700* 
031710*** 読み込んだ施術記録の年月が、前月かどうか判定 (年月の差が 1 か?)
031720      MOVE  SPACE  TO  前月フラグ.
031730      INITIALIZE  計算年月日Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
031740**
031750      MOVE 受－施術和暦    TO 終了和暦２Ｗ.
031760      MOVE 受－施術年      TO 終了年２Ｗ.
031770      MOVE 受－施術月      TO 終了月２Ｗ.
031780      MOVE 施記－施術和暦  TO 開始和暦２Ｗ.
031790      MOVE 施記－施術年    TO 開始年２Ｗ.
031800      MOVE 施記－施術月    TO 開始月２Ｗ.
031810*
031820      EVALUATE TRUE
031830       WHEN ( 開始和暦２Ｗ = 終了和暦２Ｗ ) AND ( 開始年２Ｗ = 終了年２Ｗ )
031840            PERFORM  前月比較月
031850       WHEN ( 開始和暦２Ｗ = 終了和暦２Ｗ ) AND ( 開始年２Ｗ NOT = 終了年２Ｗ )
031860            PERFORM  前月比較年
031870       WHEN ( 開始和暦２Ｗ NOT = 終了和暦２Ｗ )
031880            PERFORM  前月比較元号
031890      END-EVALUATE.
031900*
031910      IF ( 計算月Ｗ = 1 )
031920         MOVE  "YES"  TO  前月フラグ
031930      END-IF.
031940*
031950*================================================================*
031960 前月比較元号  SECTION.
031970*
031980     MOVE 開始和暦２Ｗ TO 元－元号区分.
031990     READ 元号マスタ
032000     NOT INVALID KEY
032010         MOVE 元－開始西暦年 TO 開始西暦年Ｗ
032020     END-READ.
032030     MOVE 終了和暦２Ｗ TO 元－元号区分.
032040     READ 元号マスタ
032050     NOT INVALID KEY
032060         MOVE 元－開始西暦年 TO 終了西暦年Ｗ
032070     END-READ.
032080**
032090     IF ( 開始西暦年Ｗ NOT = ZERO ) AND ( 終了西暦年Ｗ NOT = ZERO )
032100        COMPUTE 開始西暦年Ｗ = 開始西暦年Ｗ + 開始年２Ｗ - 1
032110        COMPUTE 終了西暦年Ｗ = 終了西暦年Ｗ + 終了年２Ｗ - 1
032120*
032130        IF ( 終了西暦年Ｗ =  開始西暦年Ｗ )
032140           PERFORM  前月比較月
032150        ELSE
032160           IF ( 終了西暦年Ｗ >  開始西暦年Ｗ )
032170               COMPUTE 計算年Ｗ = 終了西暦年Ｗ - 開始西暦年Ｗ
032180               COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
032190           ELSE
032200               MOVE ZERO TO 計算月Ｗ
032210           END-IF
032220        END-IF
032230     ELSE
032240        MOVE ZERO TO 計算月Ｗ
032250     END-IF.
032260*
032270*================================================================*
032280 前月比較年  SECTION.
032290*
032300     IF ( 終了年２Ｗ >  開始年２Ｗ )
032310         COMPUTE 計算年Ｗ = 終了年２Ｗ - 開始年２Ｗ
032320         COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
032330     ELSE
032340        MOVE ZERO TO 計算月Ｗ
032350     END-IF.
032360*
032370*================================================================*
032380 前月比較月  SECTION.
032390*
032400     IF ( 終了月２Ｗ >  開始月２Ｗ )
032410         COMPUTE 計算月Ｗ = 終了月２Ｗ - 開始月２Ｗ
032420     ELSE
032430        MOVE ZERO TO 計算月Ｗ
032440     END-IF.
032450*
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
                  MOVE 公費負担者番号Ｗ     TO 公費負担者番号
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
                  MOVE 公費負担者番号Ｗ     TO 公費負担者番号
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
032460*================================================================*
032470 レセプト並び順取得 SECTION.
032480*================================================================*
032490     MOVE 施術和暦ＷＲ       TO 作３－施術和暦.
032500     MOVE 施術年ＷＲ         TO 作３－施術年.
032510     MOVE 施術月ＷＲ         TO 作３－施術月.
032520     MOVE 患者コードＷＲ     TO 作３－患者コード.
032530     MOVE 連レ－保険種別     TO 作３－保険種別.
032540     READ 作業ファイル３
032550     NOT INVALID KEY
032560          MOVE 作３－順番    TO 順番Ｗ
032570     END-READ.
032580*
032590*================================================================*
032600 長期判定取得 SECTION.
032610*================================================================*
032620* ３カ月以上の長期判定は "CHOUKI" を呼ぶ. 
032630     MOVE  SPACE TO  連期間－キー.
032640     INITIALIZE      連期間－キー.
032650     MOVE 施術和暦ＷＲ  TO  連期間－施術和暦.
032660     MOVE 施術年ＷＲ    TO  連期間－施術年.
032670     MOVE 施術月ＷＲ    TO  連期間－施術月.
032680     MOVE 患者番号ＷＲ  TO  連期間－患者番号.
032690     MOVE 枝番ＷＲ      TO  連期間－枝番.
032700*
032710     CALL   "CHOUKI".
032720     CANCEL "CHOUKI".
032730*
032740**** 適用１を使用 (「前月初検のみ」がある時は、くっつける)
032750*****     IF ( 連期間－対象フラグ  = "YES" )
032760*****        IF ( 適用１Ｗ  = SPACE )
032770*****           MOVE NC"※長期施術継続理由裏面に記載"  TO 適用１Ｗ
032780*****        ELSE
032790*****           STRING 適用１Ｗ           DELIMITED BY SPACE
032800*****                  NC"，"             DELIMITED BY SIZE
032810*****                  NC"※長期施術継続理由裏面に記載"   DELIMITED BY SIZE
032820*****                  INTO 適用１Ｗ
032830*****           END-STRING
032840*****        END-IF
032850*****     END-IF.
032860*
033580*================================================================*
033590 初検加算時刻取得 SECTION.
033600*================================================================*
033610*****************************************************************
033620** 初検加算が時間外と深夜の時、適用に「受付時間」を印字する。
033630**   時刻の印字は月3回まで可能
033640*****************************************************************
033650     INITIALIZE 初検加算ＷＴ.
033660*
033670     IF ( レセ－時間外 = 1 ) OR ( レセ－深夜 = 1 ) OR ( レセ－休日 = 1 )
033680         MOVE 患者番号ＷＲ          TO 施記－患者番号
033690         MOVE 枝番ＷＲ              TO 施記－枝番
033700         MOVE 施術和暦ＷＲ          TO 施記－施術和暦
033710         MOVE 施術年ＷＲ            TO 施記－施術年
033720         MOVE 施術月ＷＲ            TO 施記－施術月
033730         MOVE ZERO                  TO 施記－施術日
033740         START 施術記録Ｆ   KEY IS >= 施記－患者コード
033750                                      施記－施術和暦年月日
033760         END-START
033770         IF ( 状態キー = "00" )
033780             MOVE ZERO  TO 初検加算カウント
033790             MOVE SPACE TO 終了フラグ２
033800             PERFORM 施術記録Ｆ読込
033810             PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
033820                           ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
033830                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
033840                           ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
033850                           ( 施記－施術月     NOT = 施術月ＷＲ      ) 
033860               IF ( 施記－初検加算 = 1 OR 2 OR 3 ) AND
033870                  ( 施記－診療区分 = 2 )
033880                  COMPUTE 初検加算カウント = 初検加算カウント  + 1
033890                  IF ( 初検加算カウント <= 3 )
033900                     MOVE 施記－初検加算 TO 初検加算区分ＷＴ(初検加算カウント)
033910                     MOVE 施記－受付時   TO 初検加算時ＷＴ(初検加算カウント)
033920                     MOVE 施記－受付分   TO 初検加算分ＷＴ(初検加算カウント)
033930                  END-IF
033940               END-IF
033950               PERFORM 施術記録Ｆ読込
033960            END-PERFORM
033970** 初検加算の時刻を適用にセット
033380            IF ( 初検加算時ＷＴ(1) NOT = ZERO ) OR ( 初検加算分ＷＴ(1) NOT = ZERO ) 
                      MOVE 初検加算時ＷＴ(1) TO 初検加算時Ｗ
                      MOVE ":"               TO 初検加算区切Ｗ
                      MOVE 初検加算分ＷＴ(1) TO 初検加算分Ｗ
                  END-IF
033380            IF ( 初検加算時ＷＴ(2) NOT = ZERO ) OR ( 初検加算分ＷＴ(2) NOT = ZERO ) 
031910                PERFORM 初検加算適用セット
                  END-IF
033990         END-IF
034000*
034010     END-IF.
034020*
034030*================================================================*
034040 初検加算適用セット SECTION.
034050*
034060     PERFORM VARYING 番号カウンタ FROM 1 BY 1
034070             UNTIL ( 番号カウンタ > 3 )
034080         IF ( 初検加算時ＷＴ(番号カウンタ)  = ZERO )  AND 
034090            ( 初検加算分ＷＴ(番号カウンタ)  = ZERO ) 
034100             CONTINUE
034110         ELSE
034120* 固定項目
034130             EVALUATE 初検加算区分ＷＴ(番号カウンタ) 
034140             WHEN 1
034150                MOVE NC"時間外"   TO 加算内容Ｗ(番号カウンタ)
034430             WHEN 2
034440                MOVE NC"休　日"   TO 加算内容Ｗ(番号カウンタ)
034160             WHEN 3
034170                MOVE NC"深　夜"   TO 加算内容Ｗ(番号カウンタ)
034180             END-EVALUATE
034190*
034200             MOVE NC"："          TO 加算区切Ｗ(番号カウンタ)
034210             MOVE NC"時"          TO 時固定Ｗ(番号カウンタ)
034220             MOVE NC"分"          TO 分固定Ｗ(番号カウンタ)
034230*
034240**** 数字→日本語変換
034250* 時間
034260             MOVE 初検加算時ＷＴ(番号カウンタ)  TO  数字Ｗ
034270             IF ( 数字Ｗ >= 10 )
034280                 MOVE 数字Ｗ１    TO 負傷番号Ｗ１
034290                 PERFORM 日本語変換
034300                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ１(番号カウンタ)
034310                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
034320                 PERFORM 日本語変換
034330                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
034340             ELSE
034350                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
034360                 PERFORM 日本語変換
034370                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
034380             END-IF
034390* 分
034400             MOVE 初検加算分ＷＴ(番号カウンタ)  TO  数字Ｗ
034410             MOVE 数字Ｗ１    TO 負傷番号Ｗ１
034420             PERFORM 日本語変換
034430             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ１(番号カウンタ)
034440             MOVE 数字Ｗ２    TO 負傷番号Ｗ１
034450             PERFORM 日本語変換
034460             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ２(番号カウンタ)
034470** 
034480        END-IF
034490     END-PERFORM.
034500*
034510     MOVE  初検加算集団ＮＷ(1)   TO 初検加算時刻１Ｗ. 
034520     MOVE  初検加算集団ＮＷ(2)   TO 初検加算時刻２Ｗ. 
034530     MOVE  初検加算集団ＮＷ(3)   TO 初検加算時刻３Ｗ. 
034540*
034550**** 適用１か２を使用（長期理由記載で適用１を使っている時は、適用２）
034560     IF ( 初検加算時ＷＴ(1)  = ZERO ) AND ( 初検加算分ＷＴ(1)  = ZERO ) 
034570         CONTINUE
034580     ELSE
034590         IF ( 適用１Ｗ  = SPACE )
034600               STRING NC"初検加算"       DELIMITED BY SIZE
034610                      初検加算時刻１Ｗ   DELIMITED BY SIZE
034620                      初検加算時刻２Ｗ   DELIMITED BY SIZE
034630                      初検加算時刻３Ｗ   DELIMITED BY SIZE
034640                      INTO 適用１Ｗ
034650               END-STRING
034660         ELSE
033830               STRING 適用１Ｗ           DELIMITED BY SPACE
036850                      NC"，"             DELIMITED BY SIZE
036860                      NC"初検加算"       DELIMITED BY SIZE
033840                      初検加算時刻１Ｗ   DELIMITED BY SIZE
033850                      初検加算時刻２Ｗ   DELIMITED BY SIZE
033860                      初検加算時刻３Ｗ   DELIMITED BY SIZE
033870                      INTO 適用１Ｗ
034720               END-STRING
034730         END-IF
034740     END-IF.
034750*
034760*================================================================*
034770 日本語変換 SECTION.
034780*
034790     MOVE NC"０"     TO 全角負傷番号Ｗ.
034800     CALL "htoz" WITH C LINKAGE
034810                        USING 負傷番号Ｗ１ 全角負傷番号Ｗ１.
034820*
034830*================================================================*
034840 負傷原因取得 SECTION.
034850*================================================================*
034860********************************************************************
034870*  負傷原因コードが同じものは、1行にまとめて印字する。
034880*  例: ①② 家で転んだ.
034890*     負傷原因コードが同じものをまとめ、テーブルにセット
034900*     (ただし、部位を飛んで同じものは、2行になる)
034910********************************************************************
034920     MOVE  ZERO   TO  カウンタ カウンタ２.
034930     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
034940             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
034950*
034960***        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
034970        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
034980*
034990           IF ( カウンタ = ZERO )
035000              MOVE 1   TO  カウンタ カウンタ２
035010              MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
035020              MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
035030              MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
035040           ELSE
035050              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
035060                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
035070                 COMPUTE カウンタ２ = カウンタ２  +  1
035080                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
035090              ELSE
035100                 COMPUTE カウンタ = カウンタ  +  1
035110                 MOVE 1   TO  カウンタ２
035120                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)
035130                                                      負傷患者番号ＣＷ
035140                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
035150                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
035160              END-IF
035170           END-IF
035180        END-IF
035190     END-PERFORM.
035200**************************************************************************
035210*  負傷原因マスタより文章取得
035220**************************************************************************
035230     MOVE  ZERO   TO  カウンタ カウンタ２.
035240     PERFORM VARYING カウンタ FROM 1 BY 1
035250             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
035260** 健保は 区分 01
035270         MOVE 01                        TO 負原－区分コード
035280         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
035290         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
035300         READ 負傷原因Ｆ
035310         NOT INVALID KEY
035320             INITIALIZE 負傷原因ＷＴ
035330             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
035340             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
035350             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
035360             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
035370             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
035380             PERFORM VARYING カウンタ２ FROM 1 BY 1
035390                     UNTIL ( カウンタ２ > 9 )  OR 
035400                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
035410                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
035420                WHEN 1
035430                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
035440                WHEN 2
035450                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
035460                WHEN 3
035470                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
035480                WHEN 4
035490                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
035500                WHEN 5
035510                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
035480                WHEN 6
035490                   MOVE "⑥"  TO  負傷原因ナンバーＷ１(カウンタ２)
035500                WHEN 7
035510                   MOVE "⑦"  TO  負傷原因ナンバーＷ１(カウンタ２)
035520                WHEN OTHER
035530                   CONTINUE
035540                END-EVALUATE
035550             END-PERFORM
035560*
035642             IF 負原－負傷原因入力区分 = 1
035643                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
035644                        負傷原因１ＷＴ  DELIMITED BY SIZE
035645                        負傷原因２ＷＴ  DELIMITED BY SIZE
035646                        負傷原因３ＷＴ  DELIMITED BY SIZE
035647                        負傷原因４ＷＴ  DELIMITED BY SIZE
035648                        負傷原因５ＷＴ  DELIMITED BY SIZE
035649                        INTO 負傷原因内容合成Ｗ(カウンタ)
035650                 END-STRING
035651             ELSE
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
035660             END-IF
035661*
035662         END-READ
035663     END-PERFORM.
035670*
035680*     PERFORM 負傷原因セット.
035680     PERFORM 全負傷原因合体セット.
035690*
035700*================================================================*
035710 負傷原因セット SECTION.
035720*
035730**************************************************************************
035740*  文章が1行を超える時は、複数行に分解する。
035750**************************************************************************
035760     MOVE  ZERO   TO  カウンタ カウンタ２.
035770     PERFORM VARYING カウンタ FROM 1 BY 1
035780             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
035790*
035870          INITIALIZE 負傷原因内容分解ＸＷ
035880          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
035890          IF ( 負傷原因内容１ＸＷ  NOT = SPACE )
035900              COMPUTE カウンタ２ = カウンタ２  +  1
035910              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
035920          END-IF
035930          IF ( 負傷原因内容２ＸＷ  NOT = SPACE )
035940              COMPUTE カウンタ２ = カウンタ２  +  1
035950              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
035960          END-IF
035970          IF ( 負傷原因内容３ＸＷ  NOT = SPACE )
035980              COMPUTE カウンタ２ = カウンタ２  +  1
035990              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
036000          END-IF
034690          IF  負傷原因内容４ＸＷ  NOT = SPACE
034700              COMPUTE カウンタ２ = カウンタ２  +  1
034710              MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
034720          END-IF
035980*
035990     END-PERFORM.
036000*
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
036010*================================================================*
036020 長期理由文取得 SECTION.
036030*================================================================*
036040* 長期理由文取得は "CHOUBUN" を呼ぶ. 
036050     MOVE  SPACE TO  連長文－キー.
036060     INITIALIZE      連長文－キー.
036070     MOVE 施術和暦ＷＲ  TO  連長文－施術和暦.
036080     MOVE 施術年ＷＲ    TO  連長文－施術年.
036090     MOVE 施術月ＷＲ    TO  連長文－施術月.
036100     MOVE 患者番号ＷＲ  TO  連長文－患者番号.
036110     MOVE 枝番ＷＲ      TO  連長文－枝番.
036130     MOVE 56            TO  連長文－文桁数.
036140*
036150     CALL   "CHOUBUN".
036160     CANCEL "CHOUBUN".
036170*
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
015000     IF (レセ長期理由印刷区分Ｗ NOT = 1 )
               MOVE 長期理由印刷区分Ｗ TO 連摘文－長期区分
           ELSE
               MOVE 1                  TO 連摘文－長期区分
015050     END-IF.
040710*
040720     CALL   "TEKIYBUN".
040730     CANCEL "TEKIYBUN".
040740*
036180*================================================================*
036190 委任年月日取得 SECTION.
036200*================================================================*
036210** ---// ここの受理年には、最終通院日が入っている為、退避する //----
036770     MOVE 受理和暦Ｗ TO 最終通院和暦Ｗ.
036220     MOVE 受理年Ｗ   TO 最終通院年Ｗ.
036230     MOVE 受理月Ｗ   TO 最終通院月Ｗ.
036240     MOVE 受理日Ｗ   TO 最終通院日Ｗ.
036250***
036260* (柔整師側)
036270     EVALUATE レセプト日付区分Ｗ 
036280*    /  最終通院日 /
036290     WHEN ZERO
036850         MOVE 最終通院和暦Ｗ TO 柔整師和暦Ｗ
036300         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
036310         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
036320         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
036330*    /  月末日 /
036340     WHEN 1 
036350         PERFORM 月末日取得
036910         MOVE 受理和暦Ｗ   TO 柔整師和暦Ｗ
036360         MOVE 受理年Ｗ     TO 柔整師年Ｗ
036370         MOVE 受理月Ｗ     TO 柔整師月Ｗ
036380         MOVE 受理日Ｗ     TO 柔整師日Ｗ
036390*    /  印字なし /
036400     WHEN 9
036960         MOVE ZERO         TO 柔整師和暦Ｗ
036410         MOVE ZERO         TO 柔整師年Ｗ
036420         MOVE ZERO         TO 柔整師月Ｗ
036430         MOVE ZERO         TO 柔整師日Ｗ
036440*    /  その他は、最終通院日 /
036450     WHEN OTHER
037010         MOVE 最終通院和暦Ｗ TO 柔整師和暦Ｗ
036460         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
036470         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
036480         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
036490     END-EVALUATE.
036500**
036510* (患者側)
036520     EVALUATE レセプト患者日付区分Ｗ 
036530*    /  最終通院日 /
036540     WHEN ZERO
037100         MOVE 最終通院和暦Ｗ TO 患者委任和暦Ｗ
036550         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
036560         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
036570         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
036580*    /  月末日 /
036590     WHEN 1 
036600         PERFORM 月末日取得
037160         MOVE 受理和暦Ｗ   TO 患者委任和暦Ｗ
036610         MOVE 受理年Ｗ     TO 患者委任年Ｗ
036620         MOVE 受理月Ｗ     TO 患者委任月Ｗ
036630         MOVE 受理日Ｗ     TO 患者委任日Ｗ
036640*    /  印字なし /
036650     WHEN 9
037210         MOVE ZERO         TO 患者委任和暦Ｗ
036660         MOVE ZERO         TO 患者委任年Ｗ
036670         MOVE ZERO         TO 患者委任月Ｗ
036680         MOVE ZERO         TO 患者委任日Ｗ
036690*    /  その他は、最終通院日 /
036700     WHEN OTHER
037260         MOVE 最終通院和暦Ｗ TO 患者委任和暦Ｗ
036710         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
036720         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
036730         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
036740     END-EVALUATE.
036750*
036760*================================================================*
036770 月末日取得 SECTION.
036780*
037350     MOVE 施術和暦ＷＲ TO 受理和暦Ｗ.
036790     MOVE 施術年ＷＲ   TO 受理年Ｗ.
036800     MOVE 施術月ＷＲ   TO 受理月Ｗ.
036810     MOVE 施術和暦ＷＲ TO 元－元号区分.
036820     READ 元号マスタ
036830     NOT INVALID KEY
036840         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
036850     END-READ.
036860     IF ( 施術西暦年Ｗ NOT = ZERO )
036870        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
036880     END-IF.
036890*
036900     EVALUATE 施術月ＷＲ
036910     WHEN 4
036920     WHEN 6
036930     WHEN 9
036940     WHEN 11
036950         MOVE 30 TO 受理日Ｗ
036960     WHEN 2
036970         DIVIDE 4 INTO 施術西暦年Ｗ GIVING    商Ｗ
036980                                    REMAINDER 余Ｗ
036990         END-DIVIDE
037000         IF ( 余Ｗ = ZERO )
037010             MOVE 29 TO 受理日Ｗ
037020         ELSE
037030             MOVE 28 TO 受理日Ｗ
037040         END-IF
037050     WHEN 1
037060     WHEN 3
037070     WHEN 5
037080     WHEN 7
037090     WHEN 8
037100     WHEN 10
037110     WHEN 12
037120         MOVE 31 TO 受理日Ｗ
037130     WHEN OTHER
037140          CONTINUE
037150     END-EVALUATE.
037160*
037170*================================================================*
037180 往療加算回数取得 SECTION.
037190*================================================================*
037200     MOVE 患者コードＷＲ TO 施記－患者コード.
037210     MOVE 施術和暦ＷＲ   TO 施記－施術和暦.
037220     MOVE 施術年ＷＲ     TO 施記－施術年.
037230     MOVE 施術月ＷＲ     TO 施記－施術月.
037240     MOVE ZERO           TO 施記－施術日.
037250*
037260     START 施術記録Ｆ   KEY IS >= 施記－患者コード
037270                                  施記－施術和暦年月日
037280     END-START.
037290     MOVE SPACE TO 終了フラグ２.
037300     PERFORM 施術記録Ｆ読込.
037310     PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
037320                   ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
037330                   ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
037340                   ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
037350                   ( 施記－施術月     NOT = 施術月ＷＲ      ) 
037360*
037370*       ****************
037380*       * 往療加算回数 *
037390*       ****************
037400        IF ( 施記－往療加算 NOT = ZERO )
037410            COMPUTE 往療加算回数Ｗ = 往療加算回数Ｗ + 1
037420        END-IF
037430*
037440        PERFORM 施術記録Ｆ読込
037450     END-PERFORM.
037460*
038000*================================================================*
038010 助成印取得 SECTION.
038020*================================================================*
036740* 2006/04 変更
036750* 助成印は "JOSEIMEI" を呼ぶ. 
036760     MOVE SPACE TO  連助成名称－キー.
036770     INITIALIZE     連助成名称－キー.
036780     MOVE 助成種別ＷＲ           TO 連助成名称－助成種別.
036790     MOVE 費用負担者番号助成ＷＲ TO 連助成名称－費用負担者番号助成.
036800*
036810     CALL   "JOSEIMEI".
036820     CANCEL "JOSEIMEI".
036830*
036840     MOVE 連助成名称－１文字 TO 助成印Ｗ.
038400*
039790*================================================================*
039800 レセ摘要再セット SECTION.
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
      *     MOVE 連摘文－摘要文(8)    TO 長期理由文８.
040000*
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
038610*================================================================*
038620 印刷処理 SECTION.
038630*================================================================*
038640     MOVE "YHN6121P"  TO  定義体名Ｐ.
038650     MOVE "SCREEN"   TO  項目群名Ｐ.
038660     WRITE YHN6121P.
038670     PERFORM エラー処理Ｐ.
038680     PERFORM 改頁処理.
038690*
038700*================================================================*
038710 改頁処理  SECTION.
038720*
038730     MOVE "YHN6121P"  TO  定義体名Ｐ.
038740     MOVE "CT"       TO  処理種別Ｐ.
038750     MOVE "PAGE"     TO  拡張制御Ｐ.
038760     MOVE SPACE      TO  項目群名Ｐ.
038770     WRITE YHN6121P.
038780     PERFORM エラー処理Ｐ.
038790     MOVE SPACE     TO  拡張制御Ｐ.
038800*
038810************
038820* 終了処理  *
038830************
038840*================================================================*
038850 受診者印刷区分更新 SECTION.
038860*================================================================*
038870** //  受診者情報Ｆの印刷区分に１をセットし、更新する。//  
038880*
038890     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
038900     MOVE 施術年ＷＲ         TO 受－施術年.
038910     MOVE 施術月ＷＲ         TO 受－施術月.
038920     MOVE 患者コードＷＲ     TO 受－患者コード.
038930     READ 受診者情報Ｆ
038940     NOT INVALID KEY
               IF 連レ－保険種別 > 50
036620             MOVE  1  TO  受－レセ印刷区分助成
               ELSE
036620             MOVE  1  TO  受－レセ印刷区分
               END-IF
038960         REWRITE  受－レコード
038970         END-REWRITE
038980         IF ( 状態キー NOT = "00" )
038990            MOVE NC"受診者" TO ファイル名
039000            PERFORM エラー表示
039010         END-IF
039020     END-READ.
039030*
039040*================================================================*
039050 終了処理 SECTION.
039060*================================================================*
039070     PERFORM ファイル閉鎖.
039080*
039090*================================================================*
039100 ファイル閉鎖 SECTION.
039110*
039120     CLOSE 印刷ファイル.
039130     CLOSE 保険者マスタ     元号マスタ       名称マスタ
039140           レセプトＦ       制御情報マスタ   施術所情報マスタ
039150           ＩＤ管理マスタ   経過マスタ       受診者情報Ｆ
039160           施術記録Ｆ       負傷データＦ     負傷原因Ｆ
039170           市町村マスタ     会情報マスタ     作業ファイル３     作業ファイル５.
039190*
039280*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
039290*================================================================*
039300 施術記録Ｆ読込 SECTION.
039310*================================================================*
039320     READ 施術記録Ｆ NEXT
039330     AT END
039340         MOVE "YES" TO 終了フラグ２
039350     END-READ.
039360*
039370*================================================================*
039380 エラー処理Ｐ SECTION.
039390*================================================================*
039400     IF ( 通知情報Ｐ NOT = "00" )
039410         DISPLAY NC"帳票エラー"              UPON CONS
039420         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
039430         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
039440         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
039450         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
039460                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
039470         ACCEPT  キー入力 FROM CONS
039480         PERFORM ファイル閉鎖
039490         MOVE 99 TO PROGRAM-STATUS
039500         EXIT PROGRAM
039510     END-IF.
039520*
039530*================================================================*
039540 エラー表示 SECTION.
039550*================================================================*
039560     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
039570     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
039580     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
039590     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
039600                                                   UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
039610     ACCEPT  キー入力 FROM CONS
039620     PERFORM ファイル閉鎖.
039630     EXIT PROGRAM.
039640*
039650*----------------------------------------------------------------
039660*================================================================*
039670 テスト印字処理 SECTION.
039680*
           MOVE ALL "9" TO
           都道府県番号 施術月 施術年 患者年 患者月 患者日 開始年１ 開始月１ 開始日１ 終了年１ 
           終了月１ 終了日１ 負傷年１ 負傷月１ 負傷日１ 初検年１ 初検月１ 初検日１ 実日数１ 
           開始年２ 開始月２ 開始日２ 終了年２ 終了月２ 終了日２ 負傷年２ 負傷月２ 負傷日２ 
           初検年２ 初検月２ 初検日２ 実日数２ 開始年３ 開始月３ 開始日３ 終了年３ 終了月３ 
           終了日３ 負傷年３ 負傷月３ 負傷日３ 初検年３ 初検月３ 初検日３ 実日数３ 開始年４ 
           開始月４ 開始日４ 終了年４ 終了月４ 終了日４ 負傷年４ 負傷月４ 負傷日４ 初検年４ 
           初検月４ 初検日４ 実日数４ 開始年５ 開始月５ 開始日５ 終了年５ 終了月５ 終了日５ 
           負傷年５ 負傷月５ 負傷日５ 初検年５ 初検月５ 初検日５ 実日数５ 初検料 初検時相談料 
           往療距離 再検料 金属副子加算料 往療回数 往療料 小計 初検加算料 施術情報提供料 
           往療加算料 初検加算時 初検加算分 初検加算区切 初回処置料(1) 初回処置料(2) 
           初回処置料(3) 初回処置料(4) 初回処置料(5) 初回処置料合計 後療単価１ 後療回数１ 
           後療料１ 冷罨法回数１ 冷罨法料１ 温罨法回数１ 温罨法料１ 電療回数１ 電療料１ 小計１ 
           長期逓減率１ 長期込小計１ 後療単価２ 後療回数２ 後療料２ 冷罨法回数２ 冷罨法料２ 
           温罨法回数２ 温罨法料２ 電療回数２ 電療料２ 小計２ 長期逓減率２ 長期込小計２ 
           後療単価３８ 後療回数３８ 後療料３８ 冷罨法回数３８ 冷罨法料３８ 温罨法回数３８ 
           温罨法料３８ 電療回数３８ 電療料３８ 小計３８ 多部位込小計３８ 長期逓減率３８ 
           長期込小計３８ 逓減開始月３０ 逓減開始日３０ 後療単価３０ 後療回数３０ 後療料３０ 
           冷罨法回数３０ 冷罨法料３０ 温罨法回数３０ 温罨法料３０ 電療回数３０ 電療料３０ 小計３０
           長期逓減率３０ 長期込小計３０ 逓減開始月４８ 逓減開始日４８ 後療単価４８ 後療回数４８ 
           後療料４８ 冷罨法回数４８ 冷罨法料４８ 温罨法回数４８ 温罨法料４８ 電療回数４８ 電療料４８ 
           小計４８ 多部位込小計４８ 長期逓減率４８ 長期込小計４８ 逓減開始月４０ 逓減開始日４０ 
           後療単価４０ 後療回数４０ 後療料４０ 冷罨法回数４０ 冷罨法料４０ 温罨法回数４０ 温罨法料４０ 
           電療回数４０ 電療料４０ 小計４０ 長期逓減率４０ 長期込小計４０ 合計 一部負担金 請求金額 
           受理年 受理月 受理日 委任年 委任月 委任日
           金属月(1) 金属月(2) 金属月(3) 月(1) 月(2) 月(3) 金属日(1) 金属日(2) 金属日(3)
           運動後療料 金属回数 運動回数 運動日(1) 運動日(2) 運動日(3) 運動日(4) 運動日(5)
           .
           MOVE ALL "X" TO
           県施術ＩＤ 保険者番号 記号番号 公費負担者番号 受給者番号 住所１ 住所２ 
           口座名義人 柔整師番号 口座番号 
           施術所郵便番号１ 施術所郵便番号２ 
           施術所住所１ 施術所住所２ 施術所電話番号 代表者カナ 保険者名
           負傷原因１ 負傷原因２ 負傷原因３ 負傷原因４ 負傷原因５ 負傷原因６ 部位５８ 部位５０
           長期理由文１ 長期理由文２ 長期理由文３ 長期理由文４ 長期理由文５ 適用２ 長期頻回 
           長期理由文６ 長期理由文７ 接骨院名 代表者名 被保険者氏名 患者氏名 金属副子
           .
           MOVE ALL NC"Ｎ" TO
           負傷名１ 負傷名２ 負傷名３ 負傷名４ 負傷名５ 経過略称(1) 経過略称(2) 経過略称(3) 
           経過略称(4) 経過略称(5) 適用１
           .
           MOVE NC"○" TO
           単独チェック 本人チェック 高一チェック 共済チェック 自チェック 社保チェック 
           組合チェック １０割チェック ９割チェック ２併チェック ６歳チェック ８割チェック 
           ７割チェック 後期チェック 退職チェック 国保チェック 家族チェック 高７チェック 
           男チェック 明治チェック 大正チェック 女チェック 昭和チェック 平成チェック 
           治癒チェック１ 中止チェック１ 転医チェック１ 治癒チェック２ 中止チェック２ 
           転医チェック２ 治癒チェック３ 中止チェック３ 転医チェック３ 治癒チェック４ 
           中止チェック４ 転医チェック４ 治癒チェック５ 中止チェック５ 転医チェック５ 新規チェック 
           継続チェック 深夜チェック 時間外チェック 休日チェック 固定料チェック 整復料チェック 
           施療料チェック 夜間チェック 暴風雨雪チェック 難路チェック 
           普通チェック 振込チェック 当座チェック 銀行チェック 金庫チェック 農協チェック 
           本店チェック 支店チェック 本支所チェック
           .
041110*================================================================*
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
021810 請求先情報取得 SECTION.
021820*
           IF (保険種別ＷＲ = 05) OR (保険種別ＷＲ >= 50)
030800         MOVE 保険種別ＷＲ   TO 市－公費種別
030810         MOVE 保険者番号ＷＲ TO 市－市町村番号
030820         READ 市町村マスタ
030830         INVALID KEY
030840             MOVE SPACE      TO 請求先名称Ｗ
030850         NOT INVALID KEY
021950             IF 保険種別ＷＲ = 05
022160                 STRING 市－市町村名称 DELIMITED BY SPACE
022110                        市－支部部署名 DELIMITED BY SPACE
022200                        INTO 請求先名称Ｗ
022210                 END-STRING
                   ELSE
022160                 STRING 市－市町村名称 DELIMITED BY SPACE
002220                        INTO 請求先名称Ｗ
022210                 END-STRING
                   END-IF
               END-READ
           ELSE
021880         MOVE 保険種別ＷＲ   TO 保－保険種別
021890         MOVE 保険者番号ＷＲ TO 保－保険者番号
021900         READ 保険者マスタ
021910         INVALID KEY
021920             MOVE SPACE      TO 請求先名称Ｗ
021930         NOT INVALID KEY
021940* 社保、日雇は「社会保険事務所」をつける
021950             EVALUATE 保険種別ＷＲ 
021960             WHEN 02
021970             WHEN 06
021980                  IF ( 保－接尾語区分 = 1 )
021990*                      MOVE 保－保険者名称    TO 請求先名称Ｗ
022160                      STRING 保－保険者名称 DELIMITED BY SPACE
022200                             INTO 請求先名称Ｗ
022210                      END-STRING
022000                  ELSE
022010                     STRING 保－保険者名称  DELIMITED BY SPACE
022020                           "社会保険事務所" DELIMITED BY SIZE
022030                           INTO 請求先名称Ｗ
022040                     END-STRING
022050                  END-IF
022060* 組合は支部名まで印字
022070             WHEN 03
                        IF 保－支部部署名 = SPACE
022080                      STRING 保－保険者名称     DELIMITED BY SPACE
022090                             "健康保険組合"     DELIMITED BY SIZE
022110                             保－支部部署名     DELIMITED BY SPACE
022120                             INTO 請求先名称Ｗ
022130                      END-STRING
                        ELSE
022080                      STRING 保－保険者名称     DELIMITED BY SPACE
022090                             "健康保険組合"     DELIMITED BY SIZE
022110                             保－支部部署名     DELIMITED BY SPACE
022120                             INTO 請求先名称Ｗ
022130                      END-STRING
                        END-IF
022140* 共済は支部名まで印字
022150             WHEN 04
024700*/               日本私立学校振興・共済事業団(34130021)の場合、"共済組合"を付けない。
024710                 IF ( 保－保険者番号 = "34130021" )
022160                     STRING 保－保険者名称 DELIMITED BY SPACE
022200                            INTO 請求先名称Ｗ
022210                     END-STRING
024730                 ELSE
                           IF 保－支部部署名 = SPACE
022160                         STRING 保－保険者名称     DELIMITED BY SPACE
022170                                "共済組合"         DELIMITED BY SIZE
022190                                保－支部部署名     DELIMITED BY SPACE
022200                                INTO 請求先名称Ｗ
022210                         END-STRING
                           ELSE
022160                         STRING 保－保険者名称     DELIMITED BY SPACE
022170                                "共済組合"         DELIMITED BY SIZE
022190                                保－支部部署名     DELIMITED BY SPACE
022200                                INTO 請求先名称Ｗ
022210                         END-STRING
                           END-IF
                       END-IF
022220             WHEN OTHER
022230*                 MOVE 保－保険者名称   TO 請求先名称Ｗ
022160                 STRING 保－保険者名称 DELIMITED BY SPACE
022200                        INTO 請求先名称Ｗ
022210                 END-STRING
022240             END-EVALUATE
022250         END-READ
           END-IF.
022260*
022270*================================================================*
       県施術ＩＤ取得 SECTION.
      *
026770*********************************************
026780** ＩＤ管理マスタより　県施術ＩＤを取得する。
026790*********************************************
      */施術機関コード 国保退職のみ印字する/130129
           IF 保険種別ＷＲ = 01 OR 08 OR 05
026800         EVALUATE 保険種別ＷＲ 
026810* 国保
026820             WHEN 01
026830                MOVE 保険者番号ＷＲ(1:2)  TO ＩＤ管－保険種別
026840* 退職
026850             WHEN 08
026860** 後期高齢
026870             WHEN 05
026880                MOVE 保険者番号ＷＲ(3:2)  TO ＩＤ管－保険種別
026890         END-EVALUATE
026900** / 県施術ID /
026910         MOVE 01                     TO ＩＤ管－ＩＤ区分
026920         MOVE ZERO                   TO ＩＤ管－施術所番号
026940         MOVE SPACE                  TO ＩＤ管－保険者番号
026950         READ ＩＤ管理マスタ
026960         NOT INVALID KEY
026970             MOVE ＩＤ管－施術ＩＤ番号   TO 県施術ＩＤＷ
026980         END-READ
           END-IF.
           IF 連レ－保険種別 > 50
025890        MOVE 01                   TO ＩＤ管－ＩＤ区分
025900        MOVE ZERO                 TO ＩＤ管－施術所番号
025910        MOVE 費用負担者番号助成ＷＲ(3:2)  TO ＩＤ管－保険種別
025920        MOVE SPACE                TO ＩＤ管－保険者番号
025930        READ ＩＤ管理マスタ
025940        NOT INVALID KEY
025950             MOVE ＩＤ管－施術ＩＤ番号   TO 県施術ＩＤＷ
025960        END-READ
025970*
025980** 市町村ID
025990        MOVE 02                     TO ＩＤ管－ＩＤ区分
026000        MOVE ZERO                   TO ＩＤ管－施術所番号
026010        MOVE 助成種別ＷＲ           TO ＩＤ管－保険種別
026020        MOVE 費用負担者番号助成ＷＲ TO ＩＤ管－保険者番号
      */京都市の重度障害/120711
              IF 費用負担者番号助成ＷＲ(1:5) = "39261"
026020            MOVE "264"              TO ＩＤ管－保険者番号
              END-IF
      *
026030        READ ＩＤ管理マスタ
              INVALID KEY
                 IF 費用負担者番号助成ＷＲ(1:5) = "39261"
025890              MOVE 01                   TO ＩＤ管－ＩＤ区分
025900              MOVE ZERO                 TO ＩＤ管－施術所番号
025910              MOVE 50                   TO ＩＤ管－保険種別
025920              MOVE SPACE                TO ＩＤ管－保険者番号
025930              READ ＩＤ管理マスタ
025940              NOT INVALID KEY
026050                 MOVE ＩＤ管－施術ＩＤ番号   TO 市町村施術ＩＤＷ
                    END-READ
                 END-IF
026040        NOT INVALID KEY
026050           MOVE ＩＤ管－施術ＩＤ番号   TO 市町村施術ＩＤＷ
026060        END-READ
           END-IF.
022270*================================================================*
       ＱＲデータセット SECTION.
      *
009900     MOVE ZERO   TO 請求西暦年ＷＱ.
009910     MOVE レセ－請求和暦 TO 元－元号区分.
009920     READ 元号マスタ
009930     NOT INVALID KEY
009940         COMPUTE 請求西暦年ＷＱ = 元－開始西暦年 + レセ－請求年 - 1
009950     END-READ.
           MOVE レセ－請求月           TO 請求月ＷＱ.
           PERFORM 会員番号右詰め.
           MOVE 会員番号右詰めＷ       TO 会員番号ＷＱ.
           MOVE 保険者番号ＷＲ         TO 保険番号ＷＱ.
           MOVE 公費負担者番号Ｗ       TO 公費負担者番号ＷＱ.
           IF 連レ－保険種別 > 50
               MOVE 3                  TO 医療助成区分ＷＱ
               MOVE レセ－受給者負担額 TO 負担額ＷＱ
               MOVE レセ－助成請求金額 TO 請求額ＷＱ
           ELSE
               MOVE 1                  TO 医療助成区分ＷＱ
               MOVE レセ－一部負担金   TO 負担額ＷＱ
               MOVE レセ－請求金額     TO 請求額ＷＱ
           END-IF.
           MOVE 本人家族区分ＷＲ       TO 本人家族ＷＱ.
009900     MOVE ZERO   TO 施術西暦年ＷＱ.
009910     MOVE レセ－施術和暦 TO 元－元号区分.
009920     READ 元号マスタ
009930     NOT INVALID KEY
009940         COMPUTE 施術西暦年ＷＱ = 元－開始西暦年 + レセ－施術年 - 1
009950     END-READ.
           MOVE レセ－施術月           TO 施術月ＷＱ.
           MOVE レセ－合計             TO 費用額ＷＱ.
           MOVE レセ－レセ実日数       TO 実日数ＷＱ.
           MOVE 部位数Ｗ               TO 部位数ＷＱ.
           MOVE 受－患者番号           TO 患者番号ＷＱ.
           MOVE 受－枝番               TO 枝番ＷＱ.
008870     MOVE SPACE TO 終了フラグ４.
008880     MOVE カンマＷＱ             TO 英数字項目２Ｗ.
008890     MOVE 被保険者氏名Ｗ         TO 英数字項目２Ｗ(2:20)
008900     PERFORM VARYING 文字ＣＮＴ FROM 22 BY -1
008910             UNTIL (文字ＣＮＴ  <= ZERO) OR
008920                   (終了フラグ４ = "YES")
008930         IF 英数字項目２Ｗ(文字ＣＮＴ:1) NOT = SPACE
008940            COMPUTE 文字ＣＮＴ = 文字ＣＮＴ + 1
008950            MOVE カンマＷＱ TO 英数字項目２Ｗ(文字ＣＮＴ:1)
008960            MOVE "YES" TO 終了フラグ４
008970         END-IF
008980     END-PERFORM.
           STRING レイアウトＷ                         DELIMITED BY SIZE
                  英数字項目２Ｗ(1:文字ＣＮＴ + 1)     DELIMITED BY SIZE
                  患者氏名Ｗ                           DELIMITED BY SIZE
             INTO ＱＲデータＷ
           END-STRING.
019160*================================================================*
019170 会員番号右詰め SECTION.
019180*
019190     MOVE 接骨師会会員番号Ｗ(1:8)  TO  会員番号左詰めＷ.
019200     MOVE SPACE         TO  会員番号右詰めＷ.
019210*
019220     MOVE  9  TO  カウンタ.
019230*
019240     IF  会員番号左詰めＷ１(8) NOT = SPACE
019250         COMPUTE カウンタ = カウンタ  -  1
019260         MOVE 会員番号左詰めＷ１(8)  TO  会員番号右詰めＷ１(カウンタ)
019270     END-IF.
019280     IF  会員番号左詰めＷ１(7) NOT = SPACE
019290         COMPUTE カウンタ = カウンタ  -  1
019300         MOVE 会員番号左詰めＷ１(7)  TO  会員番号右詰めＷ１(カウンタ)
019310     END-IF.
019320     IF  会員番号左詰めＷ１(6) NOT = SPACE
019330         COMPUTE カウンタ = カウンタ  -  1
019340         MOVE 会員番号左詰めＷ１(6)  TO  会員番号右詰めＷ１(カウンタ)
019350     END-IF.
019360     IF  会員番号左詰めＷ１(5) NOT = SPACE
019370         COMPUTE カウンタ = カウンタ  -  1
019380         MOVE 会員番号左詰めＷ１(5)  TO  会員番号右詰めＷ１(カウンタ)
019390     END-IF.
019400     IF  会員番号左詰めＷ１(4) NOT = SPACE
019410         COMPUTE カウンタ = カウンタ  -  1
019420         MOVE 会員番号左詰めＷ１(4)  TO  会員番号右詰めＷ１(カウンタ)
019430     END-IF.
019440     IF  会員番号左詰めＷ１(3) NOT = SPACE
019450         COMPUTE カウンタ = カウンタ  -  1
019460         MOVE 会員番号左詰めＷ１(3)  TO  会員番号右詰めＷ１(カウンタ)
019470     END-IF.
019480     IF  会員番号左詰めＷ１(2) NOT = SPACE
019490         COMPUTE カウンタ = カウンタ  -  1
019500         MOVE 会員番号左詰めＷ１(2)  TO  会員番号右詰めＷ１(カウンタ)
019510     END-IF.
019520     IF  会員番号左詰めＷ１(1) NOT = SPACE
019530         COMPUTE カウンタ = カウンタ  -  1
019540         MOVE 会員番号左詰めＷ１(1)  TO  会員番号右詰めＷ１(カウンタ)
019550     END-IF.
           INSPECT 会員番号右詰めＷ REPLACING ALL SPACE BY ZERO.
019560*
022270*================================================================*
041120******************************************************************
041130 END PROGRAM YHN6121.
041140******************************************************************
