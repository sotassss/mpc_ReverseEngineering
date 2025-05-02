000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YDT6421.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*         代替 健保共通 レセプト印刷（柔+ｳｨﾝﾄﾞｳｽﾞ版）
000100*  (助成を除く全ての保険種別用)
000110*         MED = YDT6421P
000120*
000120*2014/12/15 ５部位目の明細を印字しない
000130*----------------------------------------------------------------*
000140 DATE-WRITTEN.           2019-05-16
000150 DATE-COMPILED.          2019-05-16
      */実日数はレセ－部位実日数を転記する/160816
      */金属副子・運動後療の変更・追加/1805
      */元号改正により新用紙/1905
      */明細書発行加算を適用２に追加/2022
      */2024.10  長期頻回を適用に追加/2407
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
000330* 将来は、キー項目の保険者名称を保険者カナにする
000340                             ALTERNATE RECORD KEY     IS  保－保険種別
000350                                                          保－保険者名称
000360                                                         保－保険者番号
000370                             FILE STATUS              IS  状態キー
000380                             LOCK        MODE         IS  AUTOMATIC.
000390     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000400                             ORGANIZATION             IS  INDEXED
000410                             ACCESS MODE              IS  DYNAMIC
000420                             RECORD KEY               IS  元－元号区分
000430                             FILE STATUS              IS  状態キー
000440                             LOCK        MODE         IS  AUTOMATIC.
000450     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000460                             ORGANIZATION             IS  INDEXED
000470                             ACCESS MODE              IS  DYNAMIC
000480                             RECORD KEY               IS  名－区分コード
000490                                                          名－名称コード
000500                             FILE STATUS              IS  状態キー
000510                             LOCK        MODE         IS  AUTOMATIC.
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
000580     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000590                             ORGANIZATION             IS  INDEXED
000600                             ACCESS MODE              IS  DYNAMIC
000610                             RECORD KEY               IS  制－制御区分
000620                             FILE STATUS              IS  状態キー
000630                             LOCK        MODE         IS  AUTOMATIC.
000640     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000650                             ORGANIZATION             IS  INDEXED
000660                             ACCESS MODE              IS  DYNAMIC
000670                             RECORD KEY               IS 施情－施術所番号
000680                             FILE STATUS              IS  状態キー
000690                             LOCK        MODE         IS  AUTOMATIC.
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
000700     SELECT  請求先マスタ    ASSIGN      TO        SEIKYUSL
000710                             ORGANIZATION           IS  INDEXED
000720                             ACCESS MODE            IS  DYNAMIC
000730                             RECORD KEY             IS 請先－保険種別
000740                                                       請先－保険者番号
000750                             FILE STATUS            IS  状態キー
000760                             LOCK    MODE           IS  AUTOMATIC.
000770     SELECT  経過マスタ      ASSIGN      TO        KEIKAL
000780                             ORGANIZATION             IS  INDEXED
000790                             ACCESS MODE              IS  DYNAMIC
000800                             RECORD KEY               IS  経－区分コード
000810                                                          経－経過コード
000820                             FILE STATUS              IS  状態キー
000830                             LOCK        MODE         IS  AUTOMATIC.
000840     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
000850                             ORGANIZATION             IS  INDEXED
000860                             ACCESS MODE              IS  DYNAMIC
000870                             RECORD KEY               IS  負原－区分コード
000880                                                          負原－負傷原因コード
000890                             FILE STATUS              IS  状態キー
000900                             LOCK        MODE         IS  AUTOMATIC.
000910     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000920                             ORGANIZATION             IS  INDEXED
000930                             ACCESS MODE              IS  DYNAMIC
000940                             RECORD KEY               IS 受－施術和暦年月
000950                                                          受－患者コード
000960                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000970                                                          受－患者カナ
000980                                                          受－患者コード
000990                             ALTERNATE RECORD KEY     IS  受－患者コード
001000                                                         受－施術和暦年月
001010                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
001020                                                          受－保険種別
001030                                                          受－保険者番号
001040                                                          受－患者コード
001050                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
001060                                                          受－公費種別
001070                                                     受－費用負担者番号
001080                                                          受－患者コード
001090                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
001100                                                          受－助成種別
001110                                                  受－費用負担者番号助成
001120                                                          受－患者コード
001130                             ALTERNATE RECORD KEY  IS 受－請求和暦年月
001140                                                      受－施術和暦年月
001150                                                      受－患者コード
001160                             FILE STATUS              IS  状態キー
001170                             LOCK        MODE         IS  AUTOMATIC.
000180     SELECT  受診者情報２Ｆ  ASSIGN      TO        JUSINJ2L
000190                             ORGANIZATION             IS INDEXED
000200                             ACCESS MODE              IS DYNAMIC
000210                             RECORD KEY               IS 受２－施術和暦年月
000220                                                         受２－患者コード
000230                             ALTERNATE RECORD KEY     IS 受２－請求対象区分
000240                                                         受２－請求和暦年月
000250                                                         受２－施術和暦年月
000260                                                         受２－患者コード
000270                             ALTERNATE RECORD KEY     IS 受２－助成請求対象区分
000280                                                         受２－助成請求和暦年月
000290                                                         受２－施術和暦年月
000300                                                         受２－患者コード
000310                             FILE STATUS              IS  状態キー
000320                             LOCK        MODE         IS  AUTOMATIC.
001180     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
001190                             ORGANIZATION             IS  INDEXED
001200                             ACCESS MODE              IS  DYNAMIC
001210                             RECORD KEY           IS 施記－施術和暦年月日
001220                                                     施記－患者コード
001230                             ALTERNATE RECORD KEY IS 施記－患者コード
001240                                                     施記－施術和暦年月日
001250                             FILE STATUS              IS  状態キー
001260                             LOCK        MODE         IS  AUTOMATIC.
001270     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
001280                             ORGANIZATION             IS  INDEXED
001290                             ACCESS MODE              IS  DYNAMIC
001300                             RECORD KEY               IS 負－施術和暦年月
001310                                                         負－患者コード
001320                             ALTERNATE RECORD KEY     IS 負－患者コード
001330                                                         負－施術和暦年月
001340                             FILE STATUS              IS  状態キー
001350                             LOCK        MODE         IS  AUTOMATIC.
001360     SELECT  ＩＤ管理マスタ    ASSIGN      TO        IDKANRL
001370                             ORGANIZATION             IS  INDEXED
001380                             ACCESS MODE              IS  DYNAMIC
001390                             RECORD KEY               IS  ＩＤ管－ＩＤ区分
001400                                                          ＩＤ管－施術所番号
001410                                                          ＩＤ管－保険種別
001420                                                          ＩＤ管－保険者番号
001430                             ALTERNATE RECORD KEY     IS  ＩＤ管－施術ＩＤ番号
001440                                                          ＩＤ管－ＩＤ区分
001450                                                          ＩＤ管－施術所番号
001460                                                          ＩＤ管－保険種別
001470                                                          ＩＤ管－保険者番号
001480                             FILE STATUS              IS  状態キー
001490                             LOCK        MODE         IS  AUTOMATIC.
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
001600     SELECT  メモファイル    ASSIGN      TO        MEMOL
001610                             ORGANIZATION             IS  INDEXED
001620                             ACCESS MODE              IS  DYNAMIC
001630                             RECORD KEY               IS  メモ－制御区分
001640                                                          メモ－患者コード
001650                                                          メモ－施術和暦年月日
001660                             ALTERNATE RECORD KEY     IS  メモ－制御区分
001670                                                          メモ－施術和暦年月日
001680                                                          メモ－患者コード
001690                             ALTERNATE RECORD KEY     IS  メモ－患者コード
001700                                                          メモ－施術和暦年月日
001710                                                          メモ－制御区分
001720                             FILE STATUS              IS  状態キー
001730                             LOCK        MODE         IS  AUTOMATIC.
000340     SELECT  委任者情報マスタ    ASSIGN      TO ININSHAL
000350                             ORGANIZATION             IS  INDEXED
000360                             ACCESS MODE              IS  DYNAMIC
000370                             RECORD KEY               IS  委任－保険種別
000440                             FILE STATUS              IS  状態キー
000450                             LOCK        MODE         IS  AUTOMATIC.
001860* 並び順印字用
001870     SELECT  作業ファイル２  ASSIGN      TO     "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001880                             ORGANIZATION             IS  INDEXED
001890                             ACCESS                   IS  DYNAMIC
001900                             RECORD      KEY          IS  作２－施術和暦年月
001910                                                          作２－患者コード
001920                                                          作２－保険種別
001930                             FILE        STATUS       IS  状態キー
001940                             LOCK        MODE         IS  AUTOMATIC.
001950     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF002
001960                             SYMBOLIC    DESTINATION  IS "PRT"
001970                             FORMAT                   IS  定義体名Ｐ
001980                             GROUP                    IS  項目群名Ｐ
001990                             PROCESSING  MODE         IS  処理種別Ｐ
002000                             UNIT        CONTROL      IS  拡張制御Ｐ
002010                             FILE        STATUS       IS  通知情報Ｐ.
002020******************************************************************
002030*                      DATA DIVISION                             *
002040******************************************************************
002050 DATA                    DIVISION.
002060 FILE                    SECTION.
002070*                           ［ＲＬ＝  ３２０］
002080 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
002090     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
002100*                           ［ＲＬ＝  １２８］
002110 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
002120     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
002130*                           ［ＲＬ＝  １２８］
002140 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
002150     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
002470*                           ［ＲＬ＝  ６４０］
002480 FD  会情報マスタ        BLOCK   CONTAINS   1   RECORDS.
002490     COPY KAIJOHO         OF  XFDLIB  JOINING   会情   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
002190*                           ［ＲＬ＝  ２５６］
002200 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
002210     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
002220     COPY SEIGYO01        OF  XFDLIB  JOINING   制０１   AS  PREFIX.
002230*                           ［ＲＬ＝  １２８］
002240 FD  施術所情報マスタ          BLOCK   CONTAINS   1   RECORDS.
002250     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
002260*                           ［ＲＬ＝  １２８］
002270 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
002280     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
002290*                           ［ＲＬ＝  １２８］
002300 FD  経過マスタ          BLOCK   CONTAINS   1   RECORDS.
002310     COPY KEIKA          OF  XFDLIB  JOINING   経   AS  PREFIX.
002320*                           ［ＲＬ＝  ３２０］
002330 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
002340     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
002560*                          ［ＲＬ＝  1024］
000340 FD  受診者情報２Ｆ        BLOCK   CONTAINS   1   RECORDS.
000350     COPY JUSINJ2          OF  XFDLIB  JOINING   受２   AS  PREFIX.
002350*                           ［ＲＬ＝  ２５６］
002360 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
002370     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
002380*                           ［ＲＬ＝  １２８］
002390 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002400     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
002410*                           ［ＲＬ＝  １２８］
002420 FD  負傷原因Ｆ         BLOCK   CONTAINS   1   RECORDS.
002430     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
002440*                           ［ＲＬ＝  １２８］
002450 FD  ＩＤ管理マスタ          BLOCK   CONTAINS   1   RECORDS.
002460     COPY IDKANR    OF  XFDLIB  JOINING   ＩＤ管   AS  PREFIX.
002470*                           ［ＲＬ＝  ２５６］
002480 FD  市町村マスタ          BLOCK   CONTAINS   1   RECORDS.
002490     COPY SITYOSN        OF  XFDLIB  JOINING   市   AS  PREFIX.
002500*                           ［ＲＬ＝  ８３２］
002510 FD  メモファイル        BLOCK CONTAINS 1     RECORDS.
002520     COPY MEMO           OF    XFDLIB JOINING メモ AS PREFIX.
002560*                          ［ＲＬ＝  1024］
000820 FD  委任者情報マスタ    BLOCK   CONTAINS   1   RECORDS.
000830     COPY ININSHA         OF  XFDLIB  JOINING   委任   AS  PREFIX.
002570*
002580 FD  作業ファイル２ RECORD  CONTAINS 32 CHARACTERS.
002590 01  作２－レコード.
002600     03  作２－レコードキー.
002610         05  作２－施術和暦年月.
002620             07  作２－施術和暦            PIC 9.
002630             07  作２－施術年              PIC 9(2).
002640             07  作２－施術月              PIC 9(2).
002650         05  作２－患者コード.
002660             07 作２－患者番号             PIC 9(6).
002670             07 作２－枝番                 PIC X(1).
002680         05  作２－保険種別                PIC 9(2).
002690     03  作２－レコードデータ.
002700         05  作２－順番                    PIC 9(4).
002710         05  FILLER                        PIC X(14).
002720*
002730 FD  印刷ファイル.
002740     COPY YDT6421P       OF  XMDLIB.
002750*----------------------------------------------------------------*
002760******************************************************************
002770*                WORKING-STORAGE SECTION                         *
002780******************************************************************
002790 WORKING-STORAGE         SECTION.
002800 01 キー入力                           PIC X     VALUE SPACE.
002810 01 状態キー                           PIC X(2)  VALUE SPACE.
002820 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002830 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002840 01 初検フラグ                         PIC X(3)  VALUE SPACE.
002850 01 継続フラグ                         PIC X(3)  VALUE SPACE.
002860 01 ファイル名                         PIC N(6)  VALUE SPACE.
002870 01 レセプトＰＧＷ                     PIC X(8)  VALUE SPACE.
002880 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
002890 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
002900 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002910 01 患者番号Ｗ                         PIC 9(6)  VALUE ZERO.
002920 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002930 01 部位名称Ｗ                         PIC N(12) VALUE SPACE.
002940 01 部位長Ｗ                           PIC 9(2) VALUE 1.
002950 01 脱出フラグ                         PIC X(3)  VALUE SPACE.
002960 01 空白Ｗ                             PIC X(2)  VALUE SPACE.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002910 01 助成レセＷ                         PIC 9(1)  VALUE ZERO.
002970*
002980** 数字→日本語変換
002990 01 数字Ｗ                             PIC 9(2).
003000 01 数字Ｒ REDEFINES 数字Ｗ.
003010    03 数字Ｗ１                        PIC X(1).
003020    03 数字Ｗ２                        PIC X(1).
003030*
003040 01 負傷番号Ｗ                         PIC 9.
003050 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
003060    03 負傷番号Ｗ１                    PIC X.
003070*
003080 01 全角負傷番号Ｗ                     PIC N.
003090 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
003100    03 全角負傷番号Ｗ１                PIC X(2).
003110*************
003120* 共済番号用
003130 01 共済連番号集団Ｗ.
003140    03 共済連番号名Ｗ                  PIC X(14)  VALUE SPACE.
003150    03 共済連番号名ＮＷ REDEFINES  共済連番号名Ｗ  PIC N(7).
003160    03 共済連番号Ｗ                    PIC X(6)  VALUE SPACE.
003170    03 共済連番号単位Ｗ                PIC X(2)  VALUE SPACE.
003180    03 共済連番号単位ＮＷ REDEFINES  共済連番号単位Ｗ  PIC N.
003190* 自衛官番号用
003200 01 自衛官番号集団Ｗ.
003210    03 自衛官番号名Ｗ                  PIC X(8)  VALUE SPACE.
003220    03 自衛官番号名ＮＷ REDEFINES  自衛官番号名Ｗ  PIC N(4).
003230    03 自衛官番号Ｗ                    PIC X(6)  VALUE SPACE.
003240    03 自衛官番号単位Ｗ                PIC X(2)  VALUE SPACE.
003250    03 自衛官番号単位ＮＷ REDEFINES  自衛官番号単位Ｗ  PIC N.
003260*******
003270*
003280 01 カウンタ                           PIC 9(3)  VALUE ZERO.
003290 01 カウンタ２                         PIC 9(3)  VALUE ZERO.
003300 01 保険名称Ｗ                         PIC N(12) VALUE SPACE.
003310*
003320* 退避用
003330 01 終了年月日ＷＴ.
003340    03 終了年ＷＴ                      PIC 9(2)  VALUE ZERO.
003350    03 終了月ＷＴ                      PIC 9(2)  VALUE ZERO.
003360    03 終了日ＷＴ                      PIC 9(2)  VALUE ZERO.
003370* 初検日退避用
003380 01 初検年月日ＷＴ.
003390    03 初検和暦ＷＴ                    PIC 9     VALUE ZERO.
003400    03 初検年ＷＴ                      PIC 9(2)  VALUE ZERO.
003410    03 初検月ＷＴ                      PIC 9(2)  VALUE ZERO.
003420    03 初検日ＷＴ                      PIC 9(2)  VALUE ZERO.
003430* 負傷原因用
003440 01 負傷原因ＷＴ.
003450    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
003460    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
003470    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
003480    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
003490    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
003500    03 負傷原因ナンバーＷＴ.
003510       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
003520    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
003530 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
003540 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
003550 01 負傷原因ＴＢＬ.
003560    03 負傷原因コードＴＢＬ            OCCURS 9.
003570       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
003580       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
003590       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
003600 01 負傷原因内容Ｗ.
003610    03 負傷原因内容合成Ｗ              PIC X(318) OCCURS 9 VALUE SPACE.
003620    03 負傷原因内容分解ＸＷ.
003630       05 負傷原因内容１ＸＷ           PIC X(80)  VALUE SPACE.
003640       05 負傷原因内容２ＸＷ           PIC X(80)  VALUE SPACE.
003640       05 負傷原因内容３ＸＷ           PIC X(80)  VALUE SPACE.
003650       05 負傷原因内容４ＸＷ           PIC X(78)  VALUE SPACE.
009270*
009280 01 長期理由Ｗ.
          03 摘要文ＷＴＢＬ.
             05 摘要文ＷＴ                   PIC X(112) OCCURS 15 VALUE SPACE.
          03 長期理由ＷＴＢＬ.
             05 長期理由ＷＴ                 PIC X(112) OCCURS 15 VALUE SPACE.
009280    03 長期理由合成Ｗ                  PIC N(846) VALUE SPACE.
          03 長期理由分解Ｗ.
             05 長期理由内容Ｗ               PIC X(112) OCCURS 30 VALUE SPACE.
003660*
003670* 初検加算時刻用
003680 01 初検加算ＷＴ.
003690    03 初検加算カウント                PIC 9    VALUE ZERO.
003700    03 番号カウンタ                    PIC 9    VALUE ZERO.
003710    03 初検加算集団ＷＴ  OCCURS 3.
003720       05 初検加算区分ＷＴ             PIC 9    VALUE ZERO.
003730       05 初検加算時ＷＴ               PIC 9(2) VALUE ZERO.
003740       05 初検加算分ＷＴ               PIC 9(2) VALUE ZERO.
003750    03 初検加算集団ＮＷ  OCCURS 3.
003760       05 加算区切Ｗ                   PIC N(1) VALUE SPACE.
003770       05 加算内容Ｗ                   PIC N(3) VALUE SPACE.
003780       05 初検加算時ＮＷ１             PIC N(1) VALUE SPACE.
003790       05 初検加算時ＮＷ２             PIC N(1) VALUE SPACE.
003800       05 時固定Ｗ                     PIC N(1) VALUE SPACE.
003810       05 初検加算分ＮＷ１             PIC N(1) VALUE SPACE.
003820       05 初検加算分ＮＷ２             PIC N(1) VALUE SPACE.
003830       05 分固定Ｗ                     PIC N(1) VALUE SPACE.
003840    03 初検加算時刻１Ｗ                PIC N(10) VALUE SPACE.
003850    03 初検加算時刻２Ｗ                PIC N(10) VALUE SPACE.
003860    03 初検加算時刻３Ｗ                PIC N(10) VALUE SPACE.
003070    03 初検加算区切Ｗ                  PIC X     VALUE SPACE.
003080    03 初検加算時Ｗ                    PIC 9(2)  VALUE ZERO.
003090    03 初検加算分Ｗ                    PIC 9(2)  VALUE ZERO.
003870*
003880** 前月初検のみ用
003890 01 初日再検フラグ                     PIC X(3)  VALUE SPACE.
003900 01 前月フラグ                         PIC X(3)  VALUE SPACE.
003910*
003920 01 計算年月日Ｗ.
003930    03 計算和暦Ｗ                      PIC 9(1)  VALUE ZERO.
003940    03 計算年Ｗ                        PIC S9(2)  VALUE ZERO.
003950    03 計算月Ｗ                        PIC S9(2)  VALUE ZERO.
003960    03 計算日Ｗ                        PIC S9(2)  VALUE ZERO.
003970 01 開始年月日２Ｗ.
003980    03 開始和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003990    03 開始年２Ｗ                      PIC 9(2)  VALUE ZERO.
004000    03 開始月２Ｗ                      PIC 9(2)  VALUE ZERO.
004010    03 開始日２Ｗ                      PIC 9(2)  VALUE ZERO.
004020    03 開始西暦年Ｗ                    PIC S9(4) VALUE ZERO.
004030 01 終了年月日２Ｗ.
004040    03 終了和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
004050    03 終了年２Ｗ                      PIC 9(2)  VALUE ZERO.
004060    03 終了月２Ｗ                      PIC 9(2)  VALUE ZERO.
004070    03 終了日２Ｗ                      PIC 9(2)  VALUE ZERO.
004080    03 終了西暦年Ｗ                    PIC S9(4) VALUE ZERO.
004090***
004100** 負傷原因・長期理由印刷区分用
004110 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
004120 01 長期理由印刷区分Ｗ                 PIC 9 VALUE ZERO.
004130*
004140** レセ下段の日付区分用 (0:最終通院日、1:月末日、9:印字なし)
004150 01 レセプト日付区分Ｗ                 PIC 9 VALUE ZERO.
004160 01 レセプト患者日付区分Ｗ             PIC 9 VALUE ZERO.
004170*
004180** 月末日用
004190 01 施術西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
004200 01 商Ｗ                               PIC 9(3)  VALUE ZERO.
004210 01 余Ｗ                               PIC 9(3)  VALUE ZERO.
004220*
004230*
004240** 枝番判定用
004250 01 開始診療日手動区分Ｗ               PIC 9    VALUE ZERO.
004260*
004270* 保険者番号
004280 01 保険者番号比較Ｗ                   PIC X(6)   VALUE SPACE.
004290*
004300*
004310** 助成レセまとめ用
004320 01 助成レセまとめフラグ               PIC X(3)  VALUE SPACE.
004330 01 助成種別略称Ｗ                     PIC N(4)  VALUE SPACE.
004340 01 助成種別略称Ｗ２                   PIC N(4)  VALUE SPACE.
004350*
004360** レセ摘要用( N(38)固定） /
004370 01 負傷の経過Ｗ.
004380    03 負傷の経過行Ｗ                  PIC X(76) OCCURS 2 VALUE SPACE.
004390 01 負傷の経過ＮＷ REDEFINES 負傷の経過Ｗ.
004400    03 負傷の経過行ＮＷ                PIC N(38) OCCURS 2.
004410*
004420*
004430* 負傷原因印刷区分
004440 01 レセ負傷原因印刷区分Ｗ             PIC 9    VALUE ZERO.
004440 01 レセ長期理由印刷区分Ｗ             PIC 9    VALUE ZERO.
004450*
004460* レセプト並び順 *
004470 01 順番Ｗ                             PIC 9(4) VALUE ZERO.
004480*
004490* 長野県用
004500 01 受給者番号編集Ｗ.
004510    03 受給者番号編集Ｗ１              PIC X(3)  VALUE SPACE.
004520    03 受給者区切１                    PIC X     VALUE SPACE.
004530    03 受給者番号編集Ｗ２              PIC X(2)  VALUE SPACE.
004540    03 受給者区切２                    PIC X     VALUE SPACE.
004550    03 受給者番号編集Ｗ３              PIC X(10) VALUE SPACE.
004560*
004570** H18/08 レセプトの温罨法料の訂正をするかしないかの設定。（０：訂正する １：訂正なし）
004580 01 レセ温罨法訂正Ｗ.
004590    03 健保レセ温罨訂正Ｗ              PIC 9 VALUE ZERO.
004600    03 老人レセ温罨訂正Ｗ              PIC 9 VALUE ZERO.
004610    03 助成レセ温罨訂正Ｗ              PIC 9 VALUE ZERO.
004620*
004630***
004640 01 協会コードＷ                       PIC 9(2)  VALUE ZERO.
004650* 会委任文用（柔整総研）
004660 01 その他編集Ｗ.
004670    03 その他編集内容Ｗ                PIC N(10) VALUE SPACE.
      * 委任者情報マスタ使用区分(０：使用しない、１：使用する)
       01 委任者情報区分Ｗ                   PIC 9     VALUE ZERO.
004680***
004690*
004700* 福岡の経過固定印字用に使用
004710 01 全柔ＦＰＤ区分Ｗ                   PIC 9     VALUE ZERO.
004720 01 経過部位数字Ｗ                     PIC N(1)  VALUE SPACE.
      *
      */金属副子・運動後療の変更・追加/1805
       01 金属副子ＣＭ                       PIC X(200) VALUE SPACE.
       01 運動後療ＣＭ                       PIC X(68)  VALUE SPACE.
004730*
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
004800* 01 部位５Ｗ.
004810*   03 FILLER                           PIC X(1).
004820*   03 逓減固定５Ｗ                     PIC X(5).
004830*   03 FILLER                           PIC X(1).
004840*   03 逓減開始月日５Ｗ.
004850*      05 逓減開始月５Ｗ                PIC ZZ.
004860*      05 FILLER                        PIC X(2).
004870*      05 逓減開始日５Ｗ                PIC ZZ.
004880*   03 FILLER                           PIC X(2).
004890*   03 後療５Ｗ.
004900*      05 後療単価５Ｗ                  PIC ZZZZ.
004910*      05 FILLER                        PIC X(2).
004920*      05 後療回数５Ｗ                  PIC ZZ.
004930*      05 FILLER                        PIC X(2).
004940*      05 後療料５Ｗ                    PIC ZZ,ZZZ.
004950*   03 FILLER                           PIC X(3).
004960*   03 冷罨法５Ｗ.
004970*      05 冷罨法回数５Ｗ                PIC ZZ.
004980*      05 FILLER                        PIC X(2).
004990*      05 冷罨法料５Ｗ                  PIC ZZZZ.
005000*   03 FILLER                           PIC X(3).
005010*   03 温罨法５Ｗ.
005020*      05 温罨法回数５Ｗ                PIC ZZ.
005030*      05 FILLER                        PIC X(2).
005040*      05 温罨法料５Ｗ                  PIC ZZZZ.
005050*   03 FILLER                           PIC X(3).
005060*   03 電療５Ｗ.
005070*      05 電療回数５Ｗ                  PIC ZZ.
005080*      05 FILLER                        PIC X(2).
005090*      05 電療料５Ｗ                    PIC ZZZZ.
005100*   03 FILLER                           PIC X(4).
005110*   03 小計５Ｗ                         PIC ZZ,ZZZ.
005120*   03 FILLER                           PIC X(2).
005130*   03 多部位率５Ｗ                     PIC X(4).
005140*   03 FILLER                           PIC X(1).
005150*   03 多部位込小計５Ｗ                 PIC ZZ,ZZZ.
005160*   03 FILLER                           PIC X(3).
005170*   03 長期逓減率５Ｗ                   PIC 9.9.
005180*   03 FILLER                           PIC X(5).
005190*   03 長期込小計５Ｗ                   PIC ZZ,ZZZ.
005200*   03 FILLER                           PIC X(1).
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
005220****************
005230* 連結項目待避 *
005240****************
005250*    ************
005260*    * 印刷キー *
005270*    ************
005280 01 対象データＷＲ.
005290    03 施術和暦年月ＷＲ.
005300       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
005310       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
005320       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
005330    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
005340    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
005350    03 公費種別ＷＲ                     PIC 9(2)  VALUE ZERO.
005360    03 費用負担者番号ＷＲ               PIC X(10) VALUE SPACE.
005370    03 助成種別ＷＲ                     PIC 9(2)  VALUE ZERO.
005380    03 費用負担者番号助成ＷＲ           PIC X(10) VALUE SPACE.
005390    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
005400    03 患者カナＷＲ                     PIC X(20) VALUE SPACE.
005410    03 患者コードＷＲ.
005420       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
005430       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
005440*    ************
005450*    * 料金情報 *
005460*    ************
005470*    月毎の料金
005480***********************
005490 01 料金１ＷＲ.
005500   03 初検ＷＲ.
005510      05 負担割合ＷＲ               PIC 9(3)    VALUE ZERO.
005520      05 初検料ＷＲ                 PIC 9(5)    VALUE ZERO.
005530      05 初検加算料ＷＲ             PIC 9(5)    VALUE ZERO.
005540   03 相談料ＷＲ                    PIC 9(4)    VALUE ZERO.
005550   03 再検料ＷＲ                    PIC 9(5)    VALUE ZERO.
005560   03 往療ＷＲ.
005570      05 往療距離ＷＲ               PIC 9(2)V9  VALUE ZERO.
005580      05 往療回数ＷＲ               PIC 9(2)    VALUE ZERO.
005590      05 往療料ＷＲ                 PIC 9(6)    VALUE ZERO.
005600      05 往療加算料ＷＲ             PIC 9(5)    VALUE ZERO.
005610   03 金属副子加算料ＷＲ            PIC 9(5)    VALUE ZERO.
005620   03 施術情報提供料ＷＲ            PIC 9(5)    VALUE ZERO.
005630   03 合計ＷＲ                      PIC 9(6)    VALUE ZERO.
005640   03 一部負担金ＷＲ                PIC 9(6)    VALUE ZERO.
005650   03 請求金額ＷＲ                  PIC 9(6)    VALUE ZERO.
005660   03 給付割合ＷＲ                  PIC 9(1)    VALUE ZERO.
005670   03 受給者負担額ＷＲ              PIC 9(6)    VALUE ZERO.
005680   03 助成請求金額ＷＲ              PIC 9(6)    VALUE ZERO.
005690*
005700* 負傷部位毎の料金
005710***********************
005720 01 料金２ＷＲ.
005730   03 初回処置ＷＲ    OCCURS   9.
005740      05 初回処置料ＷＲ             PIC 9(5)    VALUE ZERO.
005750*
005760* 逓減毎の料金
005770***********************
005780 01 料金３ＷＲ.
005790**********
005800* １部位 *
005810**********
005820   03 部位１ＷＲ.
005830      05 後療１ＷＲ.
005840         07 後療単価１ＷＲ              PIC 9(4)    VALUE ZERO.
005850         07 後療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
005860         07 後療料１ＷＲ                PIC 9(5)    VALUE ZERO.
005870      05 冷罨法１ＷＲ.
005880         07 冷罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
005890         07 冷罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
005900      05 温罨法１ＷＲ.
005910         07 温罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
005920         07 温罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
005930      05 電療１ＷＲ.
005940         07 電療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
005950         07 電療料１ＷＲ                PIC 9(4)    VALUE ZERO.
005960      05 小計１ＷＲ                     PIC 9(6)    VALUE ZERO.
005970      05 長期逓減率１ＷＲ               PIC 9(3)    VALUE ZERO.
005980      05 長期込小計１ＷＲ               PIC 9(6)    VALUE ZERO.
005990**********
006000* ２部位 *
006010**********
006020   03 部位２ＷＲ.
006030      05 後療２ＷＲ.
006040         07 後療単価２ＷＲ              PIC 9(4)    VALUE ZERO.
006050         07 後療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
006060         07 後療料２ＷＲ                PIC 9(5)    VALUE ZERO.
006070      05 冷罨法２ＷＲ.
006080         07 冷罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
006090         07 冷罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
006100      05 温罨法２ＷＲ.
006110         07 温罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
006120         07 温罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
006130      05 電療２ＷＲ.
006140         07 電療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
006150         07 電療料２ＷＲ                PIC 9(4)    VALUE ZERO.
006160      05 小計２ＷＲ                     PIC 9(6)    VALUE ZERO.
006170      05 長期逓減率２ＷＲ               PIC 9(3)    VALUE ZERO.
006180      05 長期込小計２ＷＲ               PIC 9(6)    VALUE ZERO.
006190******************
006200* ３部位／８割 *
006210******************
006220   03 部位３８ＷＲ.
006230      05 後療３８ＷＲ.
006240         07 後療単価３８ＷＲ              PIC 9(4)  VALUE ZERO.
006250         07 後療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
006260         07 後療料３８ＷＲ                PIC 9(5)  VALUE ZERO.
006270      05 冷罨法３８ＷＲ.
006280         07 冷罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
006290         07 冷罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
006300      05 温罨法３８ＷＲ.
006310         07 温罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
006320         07 温罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
006330      05 電療３８ＷＲ.
006340         07 電療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
006350         07 電療料３８ＷＲ                PIC 9(4)  VALUE ZERO.
006360      05 小計３８ＷＲ                     PIC 9(6)  VALUE ZERO.
006370      05 多部位込小計３８ＷＲ             PIC 9(6)  VALUE ZERO.
006380      05 長期逓減率３８ＷＲ               PIC 9(3)  VALUE ZERO.
006390      05 長期込小計３８ＷＲ               PIC 9(6)  VALUE ZERO.
006400******************
006410* ３部位／１０割 *
006420******************
006430   03 部位３０ＷＲ.
006440      05 逓減開始月日３０ＷＲ.
006450         07 逓減開始月３０ＷＲ            PIC 9(2)  VALUE ZERO.
006460         07 逓減開始日３０ＷＲ            PIC 9(2)  VALUE ZERO.
006470      05 後療３０ＷＲ.
006480         07 後療単価３０ＷＲ              PIC 9(4)  VALUE ZERO.
006490         07 後療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
006500         07 後療料３０ＷＲ                PIC 9(5)  VALUE ZERO.
006510      05 冷罨法３０ＷＲ.
006520         07 冷罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
006530         07 冷罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
006540      05 温罨法３０ＷＲ.
006550         07 温罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
006560         07 温罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
006570      05 電療３０ＷＲ.
006580         07 電療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
006590         07 電療料３０ＷＲ                PIC 9(4)  VALUE ZERO.
006600      05 小計３０ＷＲ                     PIC 9(6)  VALUE ZERO.
006610      05 長期逓減率３０ＷＲ               PIC 9(3)  VALUE ZERO.
006620      05 長期込小計３０ＷＲ               PIC 9(6)  VALUE ZERO.
006630****************
006640* ４部位／５割 *
006650****************
006660   03 部位４５ＷＲ.
006670      05 後療４５ＷＲ.
006680         07 後療単価４５ＷＲ              PIC 9(4)  VALUE ZERO.
006690         07 後療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
006700         07 後療料４５ＷＲ                PIC 9(5)  VALUE ZERO.
006710      05 冷罨法４５ＷＲ.
006720         07 冷罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
006730         07 冷罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
006740      05 温罨法４５ＷＲ.
006750         07 温罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
006760         07 温罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
006770      05 電療４５ＷＲ.
006780         07 電療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
006790         07 電療料４５ＷＲ                PIC 9(4)  VALUE ZERO.
006800      05 小計４５ＷＲ                     PIC 9(6)  VALUE ZERO.
006810      05 多部位込小計４５ＷＲ             PIC 9(6)  VALUE ZERO.
006820      05 長期逓減率４５ＷＲ               PIC 9(3)  VALUE ZERO.
006830      05 長期込小計４５ＷＲ               PIC 9(6)  VALUE ZERO.
006840****************
006850* ４部位／８割 *
006860****************
006870   03 部位４８ＷＲ.
006880      05 逓減開始月日４８ＷＲ.
006890         07 逓減開始月４８ＷＲ            PIC 9(2)  VALUE ZERO.
006900         07 逓減開始日４８ＷＲ            PIC 9(2)  VALUE ZERO.
006910      05 後療４８ＷＲ.
006920         07 後療単価４８ＷＲ              PIC 9(4)  VALUE ZERO.
006930         07 後療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
006940         07 後療料４８ＷＲ                PIC 9(5)  VALUE ZERO.
006950      05 冷罨法４８ＷＲ.
006960         07 冷罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
006970         07 冷罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
006980      05 温罨法４８ＷＲ.
006990         07 温罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
007000         07 温罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
007010      05 電療４８ＷＲ.
007020         07 電療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
007030         07 電療料４８ＷＲ                PIC 9(4)  VALUE ZERO.
007040      05 小計４８ＷＲ                     PIC 9(6)  VALUE ZERO.
007050      05 多部位込小計４８ＷＲ             PIC 9(6)  VALUE ZERO.
007060      05 長期逓減率４８ＷＲ               PIC 9(3)  VALUE ZERO.
007070      05 長期込小計４８ＷＲ               PIC 9(6)  VALUE ZERO.
007080******************
007090* ４部位／１０割 *
007100******************
007110   03 部位４０ＷＲ.
007120      05 逓減開始月日４０ＷＲ.
007130         07 逓減開始月４０ＷＲ            PIC 9(2)  VALUE ZERO.
007140         07 逓減開始日４０ＷＲ            PIC 9(2)  VALUE ZERO.
007150      05 後療４０ＷＲ.
007160         07 後療単価４０ＷＲ              PIC 9(4)  VALUE ZERO.
007170         07 後療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
007180         07 後療料４０ＷＲ                PIC 9(5)  VALUE ZERO.
007190      05 冷罨法４０ＷＲ.
007200         07 冷罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
007210         07 冷罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
007220      05 温罨法４０ＷＲ.
007230         07 温罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
007240         07 温罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
007250      05 電療４０ＷＲ.
007260         07 電療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
007270         07 電療料４０ＷＲ                PIC 9(4)  VALUE ZERO.
007280      05 小計４０ＷＲ                     PIC 9(6)  VALUE ZERO.
007290      05 長期逓減率４０ＷＲ               PIC 9(3)  VALUE ZERO.
007300      05 長期込小計４０ＷＲ               PIC 9(6)  VALUE ZERO.
007310********************
007320* ５部位／２．５割 *
007330********************
007340   03 部位５２ＷＲ.
007350      05 後療５２ＷＲ.
007360         07 後療単価５２ＷＲ              PIC 9(4)  VALUE ZERO.
007370         07 後療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
007380         07 後療料５２ＷＲ                PIC 9(5)  VALUE ZERO.
007390      05 冷罨法５２ＷＲ.
007400         07 冷罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
007410         07 冷罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
007420      05 温罨法５２ＷＲ.
007430         07 温罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
007440         07 温罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
007450      05 電療５２ＷＲ.
007460         07 電療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
007470         07 電療料５２ＷＲ                PIC 9(4)  VALUE ZERO.
007480      05 小計５２ＷＲ                     PIC 9(6)  VALUE ZERO.
007490      05 多部位込小計５２ＷＲ             PIC 9(6)  VALUE ZERO.
007500      05 長期逓減率５２ＷＲ               PIC 9(3)  VALUE ZERO.
007510      05 長期込小計５２ＷＲ               PIC 9(6)  VALUE ZERO.
007520****************
007530* ５部位／５割 *
007540****************
007550   03 部位５５ＷＲ.
007560      05 逓減開始月日５５ＷＲ.
007570         07 逓減開始月５５ＷＲ            PIC 9(2)  VALUE ZERO.
007580         07 逓減開始日５５ＷＲ            PIC 9(2)  VALUE ZERO.
007590      05 後療５５ＷＲ.
007600         07 後療単価５５ＷＲ              PIC 9(4)  VALUE ZERO.
007610         07 後療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
007620         07 後療料５５ＷＲ                PIC 9(5)  VALUE ZERO.
007630      05 冷罨法５５ＷＲ.
007640         07 冷罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
007650         07 冷罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
007660      05 温罨法５５ＷＲ.
007670         07 温罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
007680         07 温罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
007690      05 電療５５ＷＲ.
007700         07 電療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
007710         07 電療料５５ＷＲ                PIC 9(4)  VALUE ZERO.
007720      05 小計５５ＷＲ                     PIC 9(6)  VALUE ZERO.
007730      05 多部位込小計５５ＷＲ             PIC 9(6)  VALUE ZERO.
007740      05 長期逓減率５５ＷＲ               PIC 9(3)  VALUE ZERO.
007750      05 長期込小計５５ＷＲ               PIC 9(6)  VALUE ZERO.
007760****************
007770* ５部位／８割 *
007780****************
007790   03 部位５８ＷＲ.
007800      05 逓減開始月日５８ＷＲ.
007810         07 逓減開始月５８ＷＲ            PIC 9(2)  VALUE ZERO.
007820         07 逓減開始日５８ＷＲ            PIC 9(2)  VALUE ZERO.
007830      05 後療５８ＷＲ.
007840         07 後療単価５８ＷＲ              PIC 9(4)  VALUE ZERO.
007850         07 後療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
007860         07 後療料５８ＷＲ                PIC 9(5)  VALUE ZERO.
007870      05 冷罨法５８ＷＲ.
007880         07 冷罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
007890         07 冷罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
007900      05 温罨法５８ＷＲ.
007910         07 温罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
007920         07 温罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
007930      05 電療５８ＷＲ.
007940         07 電療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
007950         07 電療料５８ＷＲ                PIC 9(4)  VALUE ZERO.
007960      05 小計５８ＷＲ                     PIC 9(6)  VALUE ZERO.
007970      05 多部位込小計５８ＷＲ             PIC 9(6)  VALUE ZERO.
007980      05 長期逓減率５８ＷＲ               PIC 9(3)  VALUE ZERO.
007990      05 長期込小計５８ＷＲ               PIC 9(6)  VALUE ZERO.
008000******************
008010* ５部位／１０割 *
008020******************
008030   03 部位５０ＷＲ.
008040      05 逓減開始月日５０ＷＲ.
008050         07 逓減開始月５０ＷＲ            PIC 9(2)  VALUE ZERO.
008060         07 逓減開始日５０ＷＲ            PIC 9(2)  VALUE ZERO.
008070      05 後療５０ＷＲ.
008080         07 後療単価５０ＷＲ              PIC 9(4)  VALUE ZERO.
008090         07 後療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
008100         07 後療料５０ＷＲ                PIC 9(5)  VALUE ZERO.
008110      05 冷罨法５０ＷＲ.
008120         07 冷罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
008130         07 冷罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
008140      05 温罨法５０ＷＲ.
008150         07 温罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
008160         07 温罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
008170      05 電療５０ＷＲ.
008180         07 電療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
008190         07 電療料５０ＷＲ                PIC 9(4)  VALUE ZERO.
008200      05 小計５０ＷＲ                     PIC 9(6)  VALUE ZERO.
008210      05 長期逓減率５０ＷＲ               PIC 9(3)  VALUE ZERO.
008220      05 長期込小計５０ＷＲ               PIC 9(6)  VALUE ZERO.
008000*******************
008010*  明細書発行加算 */202206
008020*******************
008030   03 明細書発行加算料ＷＲ                PIC ZZZ   VALUE ZERO.
008030   03 明細書発行加算日ＷＲ                PIC ZZ    VALUE ZERO.
008230*
008240**************
008250* 施術所情報 *
008260**************
008270 01 施術所情報Ｗ.
008280    03 柔整師番号Ｗ                    PIC X(22)  VALUE SPACE.
008290    03 接骨師会会員番号Ｗ              PIC X(10)  VALUE SPACE.
008300    03 代表者カナＷ                    PIC X(50)  VALUE SPACE.
008310    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
008320    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
          03 都道府県ＪＩＳＷ                PIC X(2)   VALUE SPACE.
008330    03 施術所住所Ｗ.
008340       05 施術所住所１Ｗ               PIC X(50)  VALUE SPACE.
008350       05 施術所住所２Ｗ               PIC X(50)  VALUE SPACE.
008360    03 施術所郵便番号Ｗ.
008370       05 施術所郵便番号１Ｗ           PIC X(3)   VALUE SPACE.
008380       05 施術所郵便番号２Ｗ           PIC X(4)   VALUE SPACE.
008390    03 施術所電話番号Ｗ                PIC X(15)  VALUE SPACE.
008400    03 定額制受理番号Ｗ                PIC X(15)  VALUE SPACE.
008410    03 受理年月日Ｗ.
007350       05 受理和暦Ｗ                   PIC 9      VALUE ZERO.
008420       05 受理年Ｗ                     PIC 9(2)   VALUE ZERO.
008430       05 受理月Ｗ                     PIC 9(2)   VALUE ZERO.
008440       05 受理日Ｗ                     PIC 9(2)   VALUE ZERO.
008450    03 最終通院年月日Ｗ.
007390       05 最終通院和暦Ｗ               PIC 9      VALUE ZERO.
008460       05 最終通院年Ｗ                 PIC 9(2)   VALUE ZERO.
008470       05 最終通院月Ｗ                 PIC 9(2)   VALUE ZERO.
008480       05 最終通院日Ｗ                 PIC 9(2)   VALUE ZERO.
008490    03 柔整師年月日Ｗ.
007430       05 柔整師和暦Ｗ                 PIC 9      VALUE ZERO.
008500       05 柔整師年Ｗ                   PIC 9(2)   VALUE ZERO.
008510       05 柔整師月Ｗ                   PIC 9(2)   VALUE ZERO.
008520       05 柔整師日Ｗ                   PIC 9(2)   VALUE ZERO.
008530    03 患者委任年月日Ｗ.
007470       05 患者委任和暦Ｗ               PIC 9      VALUE ZERO.
008540       05 患者委任年Ｗ                 PIC 9(2)   VALUE ZERO.
008550       05 患者委任月Ｗ                 PIC 9(2)   VALUE ZERO.
008560       05 患者委任日Ｗ                 PIC 9(2)   VALUE ZERO.
008570    03 取引先情報Ｗ.
008580        05 取引先銀行名Ｗ              PIC X(40)  VALUE SPACE.
008590        05 取引先銀行支店名Ｗ          PIC X(40)  VALUE SPACE.
008600        05 預金種別Ｗ                  PIC 9(1)   VALUE ZERO.
008610        05 口座番号Ｗ                  PIC X(10)  VALUE SPACE.
008620        05 口座名義人Ｗ.
008620           07 口座名義人１Ｗ           PIC X(40)  VALUE SPACE.
008620           07 口座名義人２Ｗ           PIC X(40)  VALUE SPACE.
008630        05 口座名義人カナＷ.
008630           07 口座名義人カナ１Ｗ       PIC X(60)  VALUE SPACE.
008630           07 口座名義人カナ２Ｗ       PIC X(50)  VALUE SPACE.
008630           07 口座名義人カナ３Ｗ       PIC X(40)  VALUE SPACE.
008640        05 銀行名支店名Ｗ              PIC X(60)  VALUE SPACE.
008650        05 預金種別コメントＷ          PIC N(2)   VALUE SPACE.
          03 支払機関.
             05 金融機関名Ｗ.
                07 金融機関名１Ｗ            PIC X(12) VALUE SPACE.
                07 金融機関名２Ｗ            PIC X(12) VALUE SPACE.
                07 金融機関名３Ｗ            PIC X(12) VALUE SPACE.
                07 金融機関名４Ｗ            PIC X(12) VALUE SPACE.
                07 金融機関名５Ｗ            PIC X(8)  VALUE SPACE.
             05 支店名Ｗ.
                07 支店名１Ｗ                PIC X(12) VALUE SPACE.
                07 支店名２Ｗ                PIC X(12) VALUE SPACE.
                07 支店名３Ｗ                PIC X(12) VALUE SPACE.
                07 支店名４Ｗ                PIC X(12) VALUE SPACE.
             05 振込チェックＷ               PIC N(1)  VALUE SPACE.
             05 普通チェックＷ               PIC N(1)  VALUE SPACE.
             05 当座チェックＷ               PIC N(1)  VALUE SPACE.
             05 銀行チェックＷ               PIC N(1)  VALUE SPACE.
             05 金庫チェックＷ               PIC N(1)  VALUE SPACE.
             05 農協チェックＷ               PIC N(1)  VALUE SPACE.
             05 本店チェックＷ               PIC N(1)  VALUE SPACE.
             05 支店チェックＷ               PIC N(1)  VALUE SPACE.
             05 本支所チェックＷ             PIC N(1)  VALUE SPACE.
008660    03 県施術ＩＤＷ                    PIC X(15)  VALUE SPACE.
008670    03 市町村施術ＩＤＷ                PIC X(15)  VALUE SPACE.
008680    03 柔整師番号２Ｗ                  PIC X(22)  VALUE SPACE.
007330    03 共済番号Ｗ                      PIC X(28)  VALUE SPACE.
008690**************
008700* 受診者情報 *
008710**************
008720 01 受診者情報Ｗ.
          03 施術和暦Ｗ                      PIC 9(1)   VALUE ZERO.
008730    03 施術年月Ｗ.
008740       05 施術年Ｗ                     PIC 9(2)   VALUE ZERO.
008750       05 施術月Ｗ                     PIC 9(2)   VALUE ZERO.
008760*    03 記号Ｗ                          PIC N(12)  VALUE SPACE.
007570    03 記号Ｗ.
007580       05 印刷記号Ｗ                   PIC N(12)  VALUE SPACE.
          03 記号番号Ｗ.
             05 記号番号ＸＷ                 PIC X(40) VALUE SPACE.
008770    03 番号Ｗ.
008780       05 印刷番号Ｗ                   PIC X(15)  VALUE SPACE.
008790       05 FILLER                       PIC X(15)  VALUE SPACE.
008800    03 保険者番号Ｗ.
008810       05 印刷保険者番号Ｗ             PIC X(8)   VALUE SPACE.
008820       05 FILLER                       PIC X(2)   VALUE SPACE.
008830    03 市町村番号Ｗ.
008840       05 印刷市町村番号Ｗ             PIC X(8)   VALUE SPACE.
008850       05 FILLER                       PIC X(2).
008860*
008870    03 請求先名称Ｗ.
008880       05 印刷請求先名称１Ｗ           PIC X(54)  VALUE SPACE.
008890       05 印刷請求先名称２Ｗ           PIC X(32)  VALUE SPACE.
008870    03 請求先名称ＷＲ                  PIC X(80)  VALUE SPACE.
008900*
008910    03 保険種別Ｗ                      PIC 9(2)   VALUE ZERO.
008870    03 保険種別名称Ｗ.
008880       05 保険種別名称ＷＰ             PIC N(3)  VALUE SPACE.
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
007750    03 給付割合チェックＷ.
007760       05 ７割チェックＷ               PIC N(1)  VALUE SPACE.
007770       05 ８割チェックＷ               PIC N(1)  VALUE SPACE.
007780       05 ９割チェックＷ               PIC N(1)  VALUE SPACE.
007790       05 １０割チェックＷ             PIC N(1)  VALUE SPACE.
008920    03 被保険者情報Ｗ.
008930       05 被保険者カナＷ               PIC X(50)  VALUE SPACE.
008940       05 被保険者氏名Ｗ               PIC X(50)  VALUE SPACE.
008950       05 郵便番号Ｗ.
008960          07 郵便番号１Ｗ              PIC X(3)   VALUE SPACE.
008970          07 郵便番号２Ｗ              PIC X(4)   VALUE SPACE.
008980       05 被保険者住所１Ｗ             PIC X(50)  VALUE SPACE.
008990       05 被保険者住所２Ｗ             PIC X(50)  VALUE SPACE.
008990       05 電話番号Ｗ                   PIC X(35)  VALUE SPACE.
           03 受給者番号Ｗ.
              05 印刷受給者番号Ｗ            PIC X(7)  VALUE SPACE.
              05 印刷受給者番号２Ｗ          PIC X(8)  VALUE SPACE.
009000    03 患者情報Ｗ.
009010       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
009020       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
008980       05 患者住所１Ｗ                 PIC X(50)  VALUE SPACE.
008990       05 患者住所２Ｗ                 PIC X(50)  VALUE SPACE.
009030       05 性別チェックＷ.
009040          07 男チェックＷ              PIC N(1)  VALUE SPACE.
009050          07 女チェックＷ              PIC N(1)  VALUE SPACE.
009060       05 和暦チェックＷ.
009070          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
009080          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
009090          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
009100          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
      */元号修正/↓↓↓20190405
008210          07 令和チェックＷ            PIC N(1)  VALUE SPACE.
                07 令和ＣＭＷ                PIC X(4)  VALUE SPACE.
009110          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
009120       05 患者年Ｗ                     PIC 9(2)  VALUE ZERO.
009130       05 患者月Ｗ                     PIC 9(2)  VALUE ZERO.
009140       05 患者日Ｗ                     PIC 9(2)  VALUE ZERO.
009150       05 続柄Ｗ.
009160          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
009170          07 FILLER                    PIC X(4)  VALUE SPACE.
009180       05 続柄チェックＷ.
009190          07 続柄本人チェックＷ        PIC N(1)  VALUE SPACE.
009200          07 続柄家族チェックＷ        PIC N(1)  VALUE SPACE.
009210*
009220       05 負傷原因１Ｗ                 PIC X(72) VALUE SPACE.
009230       05 負傷原因２Ｗ                 PIC X(72) VALUE SPACE.
009240       05 負傷原因３Ｗ                 PIC X(72) VALUE SPACE.
009250       05 負傷原因４Ｗ                 PIC X(72) VALUE SPACE.
009260       05 負傷原因５Ｗ                 PIC X(72) VALUE SPACE.
009270*
009280       05 負傷原因Ｗ                   PIC X(80) OCCURS 36 VALUE SPACE.
009290*
009300    03 助成印Ｗ                        PIC N(1)  VALUE SPACE.
009310    03 助成番号Ｗ                      PIC X(2)  VALUE SPACE.
009320    03 特別コメントＷ                  PIC X(16) VALUE SPACE.
009330    03 国保連用記号Ｗ                  PIC N(1)  VALUE SPACE.
009340    03 国保連用マルＷ                  PIC N(1)  VALUE SPACE.
009350*
009360****************
009370* 負傷データＦ *
009380****************
009390 01 負傷情報Ｗ.
009400    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
009410    03 部位情報Ｗ  OCCURS   9.
009420       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
009430       05 部位コードＷ.
009440          07 負傷種別Ｗ                PIC 9(2)  VALUE ZERO.
009450          07 部位Ｗ                    PIC 9(2)  VALUE ZERO.
009460          07 左右区分Ｗ                PIC 9(1)  VALUE ZERO.
009470          07 負傷位置番号Ｗ            PIC 9(2)  VALUE ZERO.
009480       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
009490       05 負傷年月日Ｗ.
009500          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
009510          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
009520          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
009530       05 初検年月日Ｗ.
009540          07 初検年Ｗ                  PIC 9(2)  VALUE ZERO.
009550          07 初検月Ｗ                  PIC 9(2)  VALUE ZERO.
009560          07 初検日Ｗ                  PIC 9(2)  VALUE ZERO.
009570       05 開始年月日Ｗ.
009580          07 開始年Ｗ                  PIC 9(2)  VALUE ZERO.
009590          07 開始月Ｗ                  PIC 9(2)  VALUE ZERO.
009600          07 開始日Ｗ                  PIC 9(2)  VALUE ZERO.
009610       05 終了年月日Ｗ.
009620          07 終了年Ｗ                  PIC 9(2)  VALUE ZERO.
009630          07 終了月Ｗ                  PIC 9(2)  VALUE ZERO.
009640          07 終了日Ｗ                  PIC 9(2)  VALUE ZERO.
009650       05 実日数Ｗ                     PIC 9(2)  VALUE ZERO.
009660       05 転帰区分Ｗ                   PIC 9(1)  VALUE ZERO.
009670       05 転帰区分チェックＷ.
009680          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
009690          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
009700          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
009710       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
009720       05 部位区切Ｗ                   PIC X(1)  VALUE SPACE.
009730       05 経過略称Ｗ.
009740          07 印刷経過略称Ｗ            PIC N(5)  VALUE SPACE.
009750          07 FILLER                    PIC X(2)  VALUE SPACE.
009760    03 経過部位Ｗ                      PIC N(1)  VALUE SPACE.
009770    03 新規チェックＷ                  PIC N(1)  VALUE SPACE.
009780    03 継続チェックＷ                  PIC N(1)  VALUE SPACE.
          03 施術日Ｗ.
             05 施術日チェックＷ   OCCURS 31 PIC N(1)  VALUE SPACE.
009790*
009800************
009810* 料金情報 *
009820************
009830 01 料金情報Ｗ.
009840    03 初検加算Ｗ.
009850       05 時間外チェックＷ                PIC N(1) VALUE SPACE.
009860       05 休日チェックＷ                  PIC N(1) VALUE SPACE.
009870       05 深夜チェックＷ                  PIC N(1) VALUE SPACE.
009880    03 往療加算Ｗ.
009890       05 夜間チェックＷ                  PIC N(1) VALUE SPACE.
009900       05 暴風雨雪チェックＷ              PIC N(1) VALUE SPACE.
009910    03 金属副子チェックＷ.
009920       05 大チェックＷ                    PIC N(1) VALUE SPACE.
009930       05 中チェックＷ                    PIC N(1) VALUE SPACE.
009940       05 小チェックＷ                    PIC N(1) VALUE SPACE.
009950    03 小計Ｗ                             PIC 9(7) VALUE ZERO.
009960    03 初回処置料合計Ｗ                   PIC 9(6) VALUE ZERO.
      */金属副子・運動後療の変更・追加/1805
          03 金属回数Ｗ                         PIC 9(2)  VALUE ZERO.
          03 運動回数Ｗ                         PIC 9(1)  VALUE ZERO.
          03 運動料Ｗ                           PIC 9(5)  VALUE ZERO.
009970************
009980* 備考情報 *
009990************
010000 01 備考情報Ｗ.
010010    03 適用１Ｗ                        PIC N(38) VALUE SPACE.
010020    03 適用２Ｗ                        PIC N(38) VALUE SPACE.
010030    03 適用３Ｗ                        PIC X(40) VALUE SPACE.
010040*    03 適用４Ｗ                        PIC N(38) VALUE SPACE.
010050    03 経過コメントＷ                  PIC N(60) VALUE SPACE.
010060**
003720*--- 負担給付割合用 ---*
003730 01 負担割合Ｗ                         PIC 9(2)  VALUE ZERO.
003740 01 給付割合Ｗ                         PIC 9(2)  VALUE ZERO.
010070*
       01 摘要施術日Ｗ                       PIC X(100) VALUE SPACE.
       01 施術日Ｗ.
          03 施術日２Ｗ                      PIC X(1)  VALUE SPACE.
          03 施術日１Ｗ                      PIC X(1)  VALUE SPACE.
      */委任者情報
       01 委任者情報Ｗ.
          03 接骨師会名Ｗ.
            05 接骨師会名ＮＷ                PIC X(50) VALUE SPACE.
          03 接骨師会会長名Ｗ.
            05 接骨師会会長名ＮＷ            PIC X(50) VALUE SPACE.
          03 会住所Ｗ                        PIC X(80) VALUE SPACE.
          03  委任団体名Ｗ                   PIC X(60) VALUE SPACE.
          03  委任者名Ｗ                     PIC X(60) VALUE SPACE.
          03  代理人郵便番号Ｗ               PIC X(10) VALUE SPACE.
          03  代理人住所Ｗ.
            05  代理人住所１Ｗ               PIC X(50) VALUE SPACE.
            05  代理人住所２Ｗ               PIC X(50) VALUE SPACE.
          03  委任電話番号１Ｗ               PIC X(20) VALUE SPACE.
          03  委任電話番号２Ｗ               PIC X(20) VALUE SPACE.
          03 受取代理人ＣＭＷ                PIC X(10) VALUE SPACE.
       01 委任コメントＷ                     PIC X(200) VALUE SPACE.
       01 委任コメントＷ２                   PIC X(200) VALUE SPACE.
       01 委任コメントＷ５.
          03 委任コメント１Ｗ                PIC X(84) VALUE SPACE.
          03 委任コメント２Ｗ                PIC X(84) VALUE SPACE.
          03 委任コメント３Ｗ                PIC X(40) VALUE SPACE.
          03 委任コメント４Ｗ                PIC X(40) VALUE SPACE.
          03 委任コメント５Ｗ                PIC X(34) VALUE SPACE.
          03 FILLER                          PIC X(50).
010080*-------------------------------------------------------------------*
010090 01 印刷制御.
010100     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
010110     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
010120     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
010130     03 拡張制御Ｐ.
010140         05 端末制御Ｐ.
010150             07 移動方向Ｐ             PIC X(1) VALUE SPACE.
010160             07 移動行数Ｐ             PIC 9(3) VALUE ZERO.
010170         05 詳細制御Ｐ                 PIC X(2) VALUE SPACE.
010180     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
010190     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
010200*
010210 01 計算機西暦年Ｗ                     PIC 9(2) VALUE ZERO.
010220* 日付ＷＯＲＫ
010230 01 和暦終了年Ｗ                       PIC 9(4) VALUE ZERO.
010240 01 計算機西暦.
010250    03 計算機西暦年                    PIC 9(4) VALUE ZERO.
010260    03 計算機西暦月日                  PIC 9(4) VALUE ZERO.
010270 01 計算機西暦Ｒ REDEFINES 計算機西暦.
010280    03 計算機世紀                      PIC 9(2).
010290    03 計算機日付                      PIC 9(6).
010300    03 計算機日付Ｒ REDEFINES 計算機日付.
010310       05 計算機年月                   PIC 9(4).
010320       05 計算機年月Ｒ REDEFINES 計算機年月.
010330         07 計算機年                   PIC 9(2).
010340         07 計算機月                   PIC 9(2).
010350       05 計算機日                     PIC 9(2).
010360*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
010370******************************************************************
010380*                          連結項目                              *
010390******************************************************************
010400************
010410* 印刷キー *
010420************
010430*
       01 連入－プレビュー IS EXTERNAL.
          03 連入－プレビュー区分          PIC 9.
010440*
010450 01 連レ印－対象データ IS EXTERNAL.
010460    03 連レ印－施術年月日.
010470       05 連レ印－施術和暦                  PIC 9(1).
010480       05 連レ印－施術年                    PIC 9(2).
010490       05 連レ印－施術月                    PIC 9(2).
010500    03 連レ印－患者コード.
010510       05 連レ印－患者番号                  PIC 9(6).
010520       05 連レ印－枝番                      PIC X(1).
010530    03 連レ印－保険種別                     PIC 9(2).
010540    03 連レ印－保険者番号                   PIC X(10).
010550    03 連レ印－公費種別                     PIC 9(2).
010560    03 連レ印－費用負担者番号               PIC X(10).
010570    03 連レ印－助成種別                     PIC 9(2).
010580    03 連レ印－費用負担者番号助成           PIC X(10).
010590    03 連レ印－患者カナ                     PIC X(20).
010600    03 連レ印－本人家族区分                 PIC 9(1).
013600*
001408************************
014090** ３カ月長期判定
014100************************
014110 01 連期間－キー IS EXTERNAL.
014120    03 連期間－施術年月.
014130       05 連期間－施術和暦               PIC 9.
014140       05 連期間－施術年                 PIC 9(2).
014150       05 連期間－施術月                 PIC 9(2).
014160    03  連期間－患者コード.
014170       05 連期間－患者番号               PIC 9(6).
014180       05 連期間－枝番                   PIC X.
014190    03 連期間－対象フラグ                PIC X(3).
014200    03 連期間－期間月Ｗ.
014210       05 連期間－期間Ｗ                 PIC 9(2) OCCURS 9.
014220*
014230************************
014240* 長期理由文セット     *
014250************************
014260 01 連長文－キー IS EXTERNAL.
014270    03 連長文－施術年月.
014280       05 連長文－施術和暦               PIC 9.
014290       05 連長文－施術年                 PIC 9(2).
014300       05 連長文－施術月                 PIC 9(2).
014310    03  連長文－患者コード.
014320       05 連長文－患者番号               PIC 9(6).
014330       05 連長文－枝番                   PIC X.
014340    03 連長文－文桁数                    PIC 9(2).
014350    03 連長文－理由文                    PIC N(63) OCCURS 15.
014360*
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
014580*
014590**  画面入力データ
014600 01 連入－入力データ委任印刷 IS EXTERNAL.
014610    03 連入－委任印刷                     PIC 9.
014620*
       01 連入－入力データ電話印刷 IS EXTERNAL.
          03 連入－電話印刷                     PIC 9.
014630*
014640*************
014650* 助成名称
014660*************
014670 01 連助成名称－キー IS EXTERNAL.
014680    03 連助成名称－助成種別             PIC 9(2).
014690    03 連助成名称－費用負担者番号助成   PIC X(10).
014700*   / OUT /
014710    03 連助成名称－名称集団.
014720       05 連助成名称－１文字            PIC N.
014730       05 連助成名称－略称              PIC N(4).
014740       05 連助成名称－正式名称          PIC N(10).
014070 01 連助成名称－会キー IS EXTERNAL.
014080    03 連助成名称－協会コード           PIC 9(2).
014750*
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
014775******************************************************************
014780*                      PROCEDURE  DIVISION                       *
014790******************************************************************
014800 PROCEDURE               DIVISION.
014810************
014820*           *
014830* 初期処理   *
014840*           *
014850************
002570     PERFORM プリンタファイル作成.
014860     PERFORM 初期化.
014870************
014880*           *
014890* 主処理     *
014900*           *
014910************
014920* 印刷
014930     PERFORM 連結項目待避.
014940     PERFORM 印刷セット.
014950     PERFORM 印刷処理.
014960************
014970*           *
014980* 終了処理   *
014990*           *
015000************
015010     PERFORM 受診者印刷区分更新.
015020     PERFORM 終了処理.
015030     MOVE ZERO  TO PROGRAM-STATUS.
015040     EXIT PROGRAM.
015050*
015060*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YDT6421"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分  TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
015070*================================================================*
015080 初期化 SECTION.
015090*
015100     PERFORM ファイルオープン.
015110*    /* 現在日付取得 */
015120     ACCEPT 計算機日付 FROM DATE.
015130*    /* 1980～2079年の間で設定 */
015140     IF 計算機年 > 80
015150         MOVE 19 TO 計算機世紀
015160     ELSE
015170         MOVE 20 TO 計算機世紀
015180     END-IF.
015190     PERFORM カレント元号取得.
015200     PERFORM 和暦終了年取得.
015210     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 和暦終了年Ｗ.
015220*================================================================*
015230 カレント元号取得 SECTION.
015240*
015250     MOVE ZEROS TO 制－制御区分.
015260     READ 制御情報マスタ
015270     NOT INVALID KEY
015280         MOVE 制－カレント元号         TO カレント元号Ｗ
015290         MOVE 制－レセ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
015300         MOVE 制－レセ長期理由印刷区分 TO 長期理由印刷区分Ｗ
015310         MOVE 制－レセプト日付区分     TO レセプト日付区分Ｗ
015320         MOVE 制－レセプト患者日付区分 TO レセプト患者日付区分Ｗ
015330         MOVE 制－協会コード           TO 協会コードＷ
015340         MOVE 制－全柔ＦＰＤ区分       TO 全柔ＦＰＤ区分Ｗ
015330         MOVE 制－助成レセ             TO 助成レセＷ
015350     END-READ.
015360*
015370*** 制御区分01
015380     MOVE 01 TO 制－制御区分.
015390     READ 制御情報マスタ
015400     NOT INVALID KEY
               MOVE 制０１－委任者情報区分    TO 委任者情報区分Ｗ
015440     END-READ.
015450***
015460*
015470*================================================================*
015480 和暦終了年取得 SECTION.
015490*
015500*     DISPLAY NC"カレント元号Ｗ"  カレント元号Ｗ UPON MSGBOX.
015510     MOVE カレント元号Ｗ TO 元－元号区分.
015520     READ 元号マスタ
015530     INVALID KEY
015540         DISPLAY NC"指定和暦が登録されていません" UPON CONS
015550         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
015560                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015570         ACCEPT  キー入力 FROM CONS
015580         PERFORM 終了処理
015590         EXIT PROGRAM
015600     NOT INVALID KEY
015610         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
015620         MOVE 前和暦Ｗ TO 元－元号区分
015630         READ 元号マスタ
015640         INVALID KEY
015650             DISPLAY NC"指定和暦が登録されていません" UPON CONS
015660             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
015670                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015680             ACCEPT  キー入力 FROM CONS
015690             PERFORM 終了処理
015700             EXIT PROGRAM
015710         NOT INVALID KEY
015720             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
015730         END-READ
015740     END-READ.
015750*
015760*================================================================*
015770 ファイルオープン SECTION.
015780*
015790     OPEN INPUT   保険者マスタ
015800         MOVE NC"保険者" TO ファイル名.
015810         PERFORM オープンチェック.
015820     OPEN INPUT   元号マスタ
015830         MOVE NC"元号" TO ファイル名.
015840         PERFORM オープンチェック.
015850     OPEN INPUT   名称マスタ
015860         MOVE NC"名称" TO ファイル名.
015870         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
015910     OPEN INPUT   制御情報マスタ
015920         MOVE NC"制御情報" TO ファイル名.
015930         PERFORM オープンチェック.
015940     OPEN INPUT   施術所情報マスタ
015950         MOVE NC"施情" TO ファイル名.
015960         PERFORM オープンチェック.
015160     OPEN INPUT   会情報マスタ.
015170         MOVE NC"会情" TO ファイル名.
015180         PERFORM オープンチェック.
015970     OPEN INPUT   請求先マスタ
015980         MOVE NC"請先" TO ファイル名.
015990         PERFORM オープンチェック.
016000     OPEN INPUT   経過マスタ
016010         MOVE NC"経過" TO ファイル名.
016020         PERFORM オープンチェック.
016030     OPEN INPUT   施術記録Ｆ.
016040         MOVE NC"施記Ｆ" TO ファイル名.
016050         PERFORM オープンチェック.
016060     OPEN INPUT   負傷データＦ.
016070         MOVE NC"負傷" TO ファイル名.
016080         PERFORM オープンチェック.
016090     OPEN INPUT   負傷原因Ｆ.
016100         MOVE NC"負傷原因" TO ファイル名.
016110         PERFORM オープンチェック.
016120     OPEN INPUT   ＩＤ管理マスタ
016130         MOVE NC"ＩＤ" TO ファイル名.
016140         PERFORM オープンチェック.
016150     OPEN INPUT 市町村マスタ.
016160         MOVE NC"市町村" TO ファイル名.
016170         PERFORM オープンチェック.
016180     OPEN INPUT メモファイル.
016190         MOVE NC"メモ" TO ファイル名.
016200         PERFORM オープンチェック.
005550     OPEN INPUT 委任者情報マスタ.
005560         MOVE NC"委任" TO ファイル名.
005570         PERFORM オープンチェック.
016210     OPEN INPUT 作業ファイル２.
015170         IF ( 状態キー  NOT =  "00" )
015060            OPEN OUTPUT  作業ファイル２
                  CLOSE 作業ファイル２
015060            OPEN INPUT  作業ファイル２
               END-IF.
015560     OPEN INPUT   受診者情報２Ｆ.
015570         MOVE NC"受診者情報２Ｆ" TO ファイル名.
015580         PERFORM オープンチェック.
016240     OPEN I-O   受診者情報Ｆ.
016250         MOVE NC"受情" TO ファイル名.
016260         PERFORM オープンチェック.
016270     OPEN I-O   印刷ファイル
016280         PERFORM エラー処理Ｐ.
016290*
016300*================================================================*
016310 オープンチェック SECTION.
016320*
016330     IF 状態キー  NOT =  "00"
016340         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
016350         DISPLAY NC"状態キー：" 状態キー         UPON CONS
016360         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
016370                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
016380         ACCEPT  キー入力 FROM CONS
016390         PERFORM ファイル閉鎖
016400         EXIT PROGRAM.
016410*================================================================*
016420 連結項目待避 SECTION.
016430*
016440     MOVE 連レ印－施術和暦           TO 施術和暦ＷＲ.
016450     MOVE 連レ印－施術年             TO 施術年ＷＲ.
016460     MOVE 連レ印－施術月             TO 施術月ＷＲ.
016470     MOVE 連レ印－保険種別           TO 保険種別ＷＲ.
016480     MOVE 連レ印－保険者番号         TO 保険者番号ＷＲ.
016490     MOVE 連レ印－公費種別           TO 公費種別ＷＲ.
016500     MOVE 連レ印－費用負担者番号     TO 費用負担者番号ＷＲ.
016510     MOVE 連レ印－助成種別           TO 助成種別ＷＲ.
016520     MOVE 連レ印－費用負担者番号助成 TO 費用負担者番号助成ＷＲ.
016530     MOVE 連レ印－本人家族区分       TO 本人家族区分ＷＲ.
016540     MOVE 連レ印－患者カナ           TO 患者カナＷＲ.
016550     MOVE 連レ印－患者番号           TO 患者番号ＷＲ.
016560     MOVE 連レ印－枝番               TO 枝番ＷＲ.
016570*================================================================*
016580 印刷セット SECTION.
016590*
016600     PERFORM 項目初期化.
014800     PERFORM 負傷読込.
016650     PERFORM 料金情報取得.
016610     PERFORM 施術所情報取得.
016620     PERFORM 請求先情報取得.
016630     PERFORM 受診者情報取得.
016640     PERFORM 負傷データ取得.
016660     PERFORM 施術記録取得.
016670*******     PERFORM 長期判定取得.
016680*******     PERFORM 初検日以前のデータ判定.
016690     PERFORM 初検加算時刻取得.
016700*     PERFORM 助成印取得.
016710*     PERFORM 保険名称取得.
016720     PERFORM 委任年月日取得.
           PERFORM 施術日取得.
016730     PERFORM レセプト並び順取得.
030010**
030020     IF 受－助成種別 NOT = ZERO
030030        PERFORM 助成レセまとめ判定
030040     ELSE
030050        MOVE SPACE TO 助成レセまとめフラグ
030060     END-IF.
016740*-----------------------------------------------*
016800     IF ( 負傷原因印刷区分Ｗ  NOT = 1 ) AND ( レセ負傷原因印刷区分Ｗ NOT = 1 )
016813        IF ( 負傷原因印刷区分Ｗ = 3 OR 4 )
016815           PERFORM 負傷原因印刷対象判定処理
016817        ELSE
016820           PERFORM 負傷原因取得
016821        END-IF
016830     END-IF.
016831*-----------------------------------------------*
016832*
016850     IF 長期理由印刷区分Ｗ  NOT = 1 
               MOVE 長期理由印刷区分Ｗ TO 連摘文－長期区分
016900     END-IF.
016910*
      */京都市の後期＋障害/120606
           IF (受－保険種別 = 05 AND 受－助成種別 = 53)
              IF (受－費用負担者番号助成(1:5) = "39261" OR "43264")
022020           MOVE ALL NC"＝"      TO 取消線
                 MOVE NC"健康管理費"  TO タイトル
              END-IF
           END-IF.
016910*
016940********************
016950* 受診者情報セット *
016960********************
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
      *助成印を印字する/111208
017000     MOVE 助成印Ｗ            TO 助成印.
      */京都市重度障害の場合負担率を印刷する/120711
           IF (受－保険種別 = 05 AND 受－助成種別 = 53) AND
              (受－費用負担者番号助成(1:5) = "39261" OR "43264")
               MOVE レセ－負担割合 TO 負担割合
               MOVE NC"割"         TO 負担割合固定
017220         MOVE 印刷請求先名称１Ｗ  TO 保険者名称１
017230         MOVE 印刷請求先名称２Ｗ  TO 保険者名称２
           END-IF.
017030*
           MOVE 施術和暦Ｗ         TO 元－元号区分
037380     READ 元号マスタ
037390     NOT INVALID KEY
037400         MOVE 元－元号名称   TO 施術和暦
037410     END-READ.
017040     MOVE 施術年Ｗ            TO 施術年.
017050     MOVE 施術月Ｗ            TO 施術月.
017060*
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
017200*
017210     MOVE 印刷保険者番号Ｗ    TO 保険者番号.
017240*     MOVE 被保険者カナＷ      TO 被保険者カナ.
017250*     MOVE 被保険者氏名Ｗ      TO 被保険者氏名.
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
017290*     MOVE 被保険者住所１Ｗ    TO 住所１.
017300*     MOVE 被保険者住所２Ｗ    TO 住所２.
017290     MOVE 患者住所１Ｗ        TO 住所１.
017300     MOVE 患者住所２Ｗ        TO 住所２.
017310*     MOVE 患者カナＷ          TO 患者カナ.
017320     MOVE 患者氏名Ｗ          TO 患者氏名 被保険者氏名.
017330     MOVE 男チェックＷ        TO 男チェック.
017340     MOVE 女チェックＷ        TO 女チェック.
017350     MOVE 明治チェックＷ      TO 明治チェック.
017360     MOVE 大正チェックＷ      TO 大正チェック.
017370     MOVE 昭和チェックＷ      TO 昭和チェック.
017380     MOVE 平成チェックＷ      TO 平成チェック.
017390*     MOVE 元号Ｗ              TO 元号.
      */元号修正↓↓↓/20190405
      */令和時のみ5令を印刷する/20190519
           MOVE "1明 2大"      TO 元号ＣＭ１.
           IF 令和チェックＷ NOT = SPACE
               MOVE "1明 2大 5令"  TO 元号ＣＭ１
           END-IF.
           MOVE "3昭 4平"          TO 元号ＣＭ２.
023070     MOVE 令和チェックＷ      TO 令和チェック.
017390*     MOVE 元号Ｗ              TO 患者和暦.
      */元号修正↑↑↑/20190405
017400     MOVE 患者年Ｗ            TO 患者年.
017410     MOVE 患者月Ｗ            TO 患者月.
017420     MOVE 患者日Ｗ            TO 患者日.
017430*     MOVE 印刷続柄Ｗ          TO 続柄.
017440*     MOVE 続柄本人チェックＷ  TO 続柄本人チェック.
017450*     MOVE 続柄家族チェックＷ  TO 続柄家族チェック.
      *
           IF (受２－助成被保険者氏名 = SPACE)
              CONTINUE
           ELSE
016940        MOVE 受２－助成被保険者氏名 TO 被保険者氏名
           END-IF.
017460* 
017470     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
017480     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
017490     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
017500     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
017510     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
017510     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
017510     MOVE 負傷原因Ｗ(7)       TO 負傷原因７.
017510     MOVE 負傷原因Ｗ(8)       TO 負傷原因８.
017520*
      */大阪府内の助成は本体に負担者番号、受給者番号を記載する
      *     IF 市町村番号Ｗ(3:2) = "27"
      *         IF 市町村番号Ｗ(1:2) NOT = "99"
      *             MOVE 市町村番号Ｗ TO 公費負担者番号
      *         END-IF
      */受給者番号が８文字以上の場合枠を無視して印刷する/110425
      *         IF 印刷受給者番号２Ｗ = SPACE
      *             MOVE 印刷受給者番号Ｗ TO 受給者番号
      *         ELSE
      *             MOVE 受給者番号Ｗ     TO 受給者番号２
      *         END-IF
      *     END-IF.
            MOVE 市町村番号Ｗ TO 公費負担者番号.
      */受給者番号が８文字以上の場合枠を無視して印刷する/110425
            IF 印刷受給者番号２Ｗ = SPACE
                MOVE 印刷受給者番号Ｗ TO 受給者番号
            ELSE
                MOVE 受給者番号Ｗ     TO 受給者番号２
            END-IF.
017720********************
017730* 負傷データセット *
017740********************
017750* １部位 *
017760**********
017770     MOVE 負傷名Ｗ(1)       TO 負傷名１.
017780     MOVE 負傷年Ｗ(1)       TO 負傷年１.
017790     MOVE 負傷月Ｗ(1)       TO 負傷月１.
017800     MOVE 負傷日Ｗ(1)       TO 負傷日１.
017810     MOVE 初検年Ｗ(1)       TO 初検年１.
017820     MOVE 初検月Ｗ(1)       TO 初検月１.
017830     MOVE 初検日Ｗ(1)       TO 初検日１.
017840     MOVE 開始年Ｗ(1)       TO 開始年１.
017850     MOVE 開始月Ｗ(1)       TO 開始月１.
017860     MOVE 開始日Ｗ(1)       TO 開始日１.
017870     MOVE 終了年Ｗ(1)       TO 終了年１.
017880     MOVE 終了月Ｗ(1)       TO 終了月１.
017890     MOVE 終了日Ｗ(1)       TO 終了日１.
           IF 負傷年Ｗ(1) NOT = ZERO
              MOVE "･"            TO 負傷１区切１ 負傷１区切２
           END-IF.
           IF 初検年Ｗ(1) NOT = ZERO
              MOVE "･"            TO 負傷１区切３ 負傷１区切４
           END-IF.
           IF 開始年Ｗ(1) NOT = ZERO
              MOVE "･"            TO 負傷１区切５ 負傷１区切６
           END-IF.
           IF 終了年Ｗ(1) NOT = ZERO
              MOVE "･"            TO 負傷１区切７ 負傷１区切８
           END-IF.
017900     MOVE 実日数Ｗ(1)       TO 実日数１.
017910     MOVE 治癒チェックＷ(1) TO 治癒チェック１.
017920     MOVE 中止チェックＷ(1) TO 中止チェック１.
017930     MOVE 転医チェックＷ(1) TO 転医チェック１.
017940**********
017950* ２部位 *
017960**********
017970     MOVE 負傷名Ｗ(2)       TO 負傷名２.
017980     MOVE 負傷年Ｗ(2)       TO 負傷年２.
017990     MOVE 負傷月Ｗ(2)       TO 負傷月２.
018000     MOVE 負傷日Ｗ(2)       TO 負傷日２.
018010     MOVE 初検年Ｗ(2)       TO 初検年２.
018020     MOVE 初検月Ｗ(2)       TO 初検月２.
018030     MOVE 初検日Ｗ(2)       TO 初検日２.
018040     MOVE 開始年Ｗ(2)       TO 開始年２.
018050     MOVE 開始月Ｗ(2)       TO 開始月２.
018060     MOVE 開始日Ｗ(2)       TO 開始日２.
018070     MOVE 終了年Ｗ(2)       TO 終了年２.
018080     MOVE 終了月Ｗ(2)       TO 終了月２.
018090     MOVE 終了日Ｗ(2)       TO 終了日２.
           IF 負傷年Ｗ(2) NOT = ZERO
              MOVE "･"            TO 負傷２区切１ 負傷２区切２
           END-IF.
           IF 初検年Ｗ(2) NOT = ZERO
              MOVE "･"            TO 負傷２区切３ 負傷２区切４
           END-IF.
           IF 開始年Ｗ(2) NOT = ZERO
              MOVE "･"            TO 負傷２区切５ 負傷２区切６
           END-IF.
           IF 終了年Ｗ(2) NOT = ZERO
              MOVE "･"            TO 負傷２区切７ 負傷２区切８
           END-IF.
018100     MOVE 実日数Ｗ(2)       TO 実日数２.
018110     MOVE 治癒チェックＷ(2) TO 治癒チェック２.
018120     MOVE 中止チェックＷ(2) TO 中止チェック２.
018130     MOVE 転医チェックＷ(2) TO 転医チェック２.
018140**********
018150* ３部位 *
018160**********
018170     MOVE 負傷名Ｗ(3)       TO 負傷名３.
018180     MOVE 負傷年Ｗ(3)       TO 負傷年３.
018190     MOVE 負傷月Ｗ(3)       TO 負傷月３.
018200     MOVE 負傷日Ｗ(3)       TO 負傷日３.
018210     MOVE 初検年Ｗ(3)       TO 初検年３.
018220     MOVE 初検月Ｗ(3)       TO 初検月３.
018230     MOVE 初検日Ｗ(3)       TO 初検日３.
018240     MOVE 開始年Ｗ(3)       TO 開始年３.
018250     MOVE 開始月Ｗ(3)       TO 開始月３.
018260     MOVE 開始日Ｗ(3)       TO 開始日３.
018270     MOVE 終了年Ｗ(3)       TO 終了年３.
018280     MOVE 終了月Ｗ(3)       TO 終了月３.
018290     MOVE 終了日Ｗ(3)       TO 終了日３.
           IF 負傷年Ｗ(3) NOT = ZERO
              MOVE "･"            TO 負傷３区切１ 負傷３区切２
           END-IF.
           IF 初検年Ｗ(3) NOT = ZERO
              MOVE "･"            TO 負傷３区切３ 負傷３区切４
           END-IF.
           IF 開始年Ｗ(3) NOT = ZERO
              MOVE "･"            TO 負傷３区切５ 負傷３区切６
           END-IF.
           IF 終了年Ｗ(3) NOT = ZERO
              MOVE "･"            TO 負傷３区切７ 負傷３区切８
           END-IF.
018300     MOVE 実日数Ｗ(3)       TO 実日数３.
018310     MOVE 治癒チェックＷ(3) TO 治癒チェック３.
018320     MOVE 中止チェックＷ(3) TO 中止チェック３.
018330     MOVE 転医チェックＷ(3) TO 転医チェック３.
018340**********
018350* ４部位 *
018360**********
018370     MOVE 負傷名Ｗ(4)       TO 負傷名４.
018380     MOVE 負傷年Ｗ(4)       TO 負傷年４.
018390     MOVE 負傷月Ｗ(4)       TO 負傷月４.
018400     MOVE 負傷日Ｗ(4)       TO 負傷日４.
018410     MOVE 初検年Ｗ(4)       TO 初検年４.
018420     MOVE 初検月Ｗ(4)       TO 初検月４.
018430     MOVE 初検日Ｗ(4)       TO 初検日４.
018440     MOVE 開始年Ｗ(4)       TO 開始年４.
018450     MOVE 開始月Ｗ(4)       TO 開始月４.
018460     MOVE 開始日Ｗ(4)       TO 開始日４.
018470     MOVE 終了年Ｗ(4)       TO 終了年４.
018480     MOVE 終了月Ｗ(4)       TO 終了月４.
018490     MOVE 終了日Ｗ(4)       TO 終了日４.
           IF 負傷年Ｗ(4) NOT = ZERO
              MOVE "･"            TO 負傷４区切１ 負傷４区切２
           END-IF.
           IF 初検年Ｗ(4) NOT = ZERO
              MOVE "･"            TO 負傷４区切３ 負傷４区切４
           END-IF.
           IF 開始年Ｗ(4) NOT = ZERO
              MOVE "･"            TO 負傷４区切５ 負傷４区切６
           END-IF.
           IF 終了年Ｗ(4) NOT = ZERO
              MOVE "･"            TO 負傷４区切７ 負傷４区切８
           END-IF.
018500     MOVE 実日数Ｗ(4)       TO 実日数４.
018510     MOVE 治癒チェックＷ(4) TO 治癒チェック４.
018520     MOVE 中止チェックＷ(4) TO 中止チェック４.
018530     MOVE 転医チェックＷ(4) TO 転医チェック４.
018540**********
018550* ５部位 *
018560**********
018570     MOVE 負傷名Ｗ(5)       TO 負傷名５.
018580     MOVE 負傷年Ｗ(5)       TO 負傷年５.
018590     MOVE 負傷月Ｗ(5)       TO 負傷月５.
018600     MOVE 負傷日Ｗ(5)       TO 負傷日５.
018610     MOVE 初検年Ｗ(5)       TO 初検年５.
018620     MOVE 初検月Ｗ(5)       TO 初検月５.
018630     MOVE 初検日Ｗ(5)       TO 初検日５.
018640     MOVE 開始年Ｗ(5)       TO 開始年５.
018650     MOVE 開始月Ｗ(5)       TO 開始月５.
018660     MOVE 開始日Ｗ(5)       TO 開始日５.
018670     MOVE 終了年Ｗ(5)       TO 終了年５.
018680     MOVE 終了月Ｗ(5)       TO 終了月５.
018690     MOVE 終了日Ｗ(5)       TO 終了日５.
           IF 負傷年Ｗ(5) NOT = ZERO
              MOVE "･"            TO 負傷５区切１ 負傷５区切２
           END-IF.
           IF 初検年Ｗ(5) NOT = ZERO
              MOVE "･"            TO 負傷５区切３ 負傷５区切４
           END-IF.
           IF 開始年Ｗ(5) NOT = ZERO
              MOVE "･"            TO 負傷５区切５ 負傷５区切６
           END-IF.
           IF 終了年Ｗ(5) NOT = ZERO
              MOVE "･"            TO 負傷５区切７ 負傷５区切８
           END-IF.
018700     MOVE 実日数Ｗ(5)       TO 実日数５.
018710     MOVE 治癒チェックＷ(5) TO 治癒チェック５.
018720     MOVE 中止チェックＷ(5) TO 中止チェック５.
018730     MOVE 転医チェックＷ(5) TO 転医チェック５.
018740**************
018750* 経過セット *
018760**************
018770     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018780***             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
018790             UNTIL ( 部位ＣＮＴ > 5 )
018800**         MOVE 部位ＣＮＴＷ(部位ＣＮＴ)   TO 経過部位ＣＮＴ(部位ＣＮＴ)
018810**         MOVE 部位区切Ｗ(部位ＣＮＴ)     TO 部位区切(部位ＣＮＴ)
018820         MOVE 印刷経過略称Ｗ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
018830     END-PERFORM.
018840*****************************************
018850*     新規・継続チェックについて        *
018860*   ●新規...初検有り ●継続...初検なし *
018870*****************************************
018880     MOVE 新規チェックＷ    TO 新規チェック.
018890     MOVE 継続チェックＷ    TO 継続チェック.
018900********************
018910* 料金データセット *
018920********************
018930*    ****************************************************************
018940*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
018950*    ****************************************************************
018960     MOVE 初検料ＷＲ                   TO  初検料.
018970     MOVE 相談料ＷＲ                   TO  初検時相談料.
019020     MOVE 時間外チェックＷ             TO  時間外チェック.
019030     MOVE 休日チェックＷ               TO  休日チェック.
019040     MOVE 深夜チェックＷ               TO  深夜チェック.
019050     MOVE 初検加算料ＷＲ               TO  初検加算料.
      *
           IF (時間外チェックＷ NOT = SPACE) OR (深夜チェックＷ NOT = SPACE) OR
              (休日チェックＷ NOT = SPACE)
              MOVE 初検加算時Ｗ                 TO  初検加算時
              MOVE 初検加算区切Ｗ               TO  初検加算区切
              MOVE 初検加算分Ｗ                 TO  初検加算分
           END-IF.
      *
019060     MOVE 再検料ＷＲ                   TO  再検料.
019070     MOVE 往療距離ＷＲ                 TO  往療距離.
019080     MOVE 往療回数ＷＲ                 TO  往療回数.
019090     MOVE 往療料ＷＲ                   TO  往療料.
019100     MOVE 夜間チェックＷ               TO  夜間チェック.
019110     MOVE 暴風雨雪チェックＷ           TO  暴風雨雪チェック.
019120     MOVE 往療加算料ＷＲ               TO  往療加算料.
      */金属副子・運動後療の変更・追加/1805
           MOVE 金属回数Ｗ                   TO  金属回数.
019380     MOVE 金属副子加算料ＷＲ           TO  金属副子加算料.
           MOVE 運動回数Ｗ                   TO  運動回数.
           MOVE 運動料Ｗ                     TO  運動後療料.
019160     MOVE 金属副子加算料ＷＲ           TO  金属副子加算料.
019170     MOVE 施術情報提供料ＷＲ           TO  施術情報提供料.
019180     MOVE 小計Ｗ                       TO 小計.
019190********************
019200* 初回処置料セット *
019210********************
019220     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
019230***             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
019240             UNTIL ( 部位ＣＮＴ > 5 )
019250         MOVE 初回処置料ＷＲ(部位ＣＮＴ) TO 初回処置料(部位ＣＮＴ)
019260     END-PERFORM.
019270     MOVE 初回処置料合計Ｗ         TO 初回処置料合計
019280********************
019290* 逓減毎料金セット *
019300********************
019310*    **********
019320*    * １部位 *
019330*    **********
019340     MOVE 後療単価１ＷＲ             TO 後療単価１.
019350     MOVE 後療回数１ＷＲ             TO 後療回数１.
019360     MOVE 後療料１ＷＲ               TO 後療料１.
019370     MOVE 冷罨法回数１ＷＲ           TO 冷罨法回数１.
019380     MOVE 冷罨法料１ＷＲ             TO 冷罨法料１.
019390     MOVE 温罨法回数１ＷＲ           TO 温罨法回数１.
019400     MOVE 温罨法料１ＷＲ             TO 温罨法料１.
019410     MOVE 電療回数１ＷＲ             TO 電療回数１.
019420     MOVE 電療料１ＷＲ               TO 電療料１.
019430     MOVE 小計１ＷＲ                 TO 小計１.
019440     IF 長期逓減率１ＷＲ NOT = ZERO
019450         COMPUTE 長期逓減率１ = 長期逓減率１ＷＲ / 100
019460     END-IF.
019470     MOVE 長期込小計１ＷＲ           TO 長期込小計１.
019480*    **********
019490*    * ２部位 *
019500*    **********
019510     MOVE 後療単価２ＷＲ             TO 後療単価２.
019520     MOVE 後療回数２ＷＲ             TO 後療回数２.
019530     MOVE 後療料２ＷＲ               TO 後療料２.
019540     MOVE 冷罨法回数２ＷＲ           TO 冷罨法回数２.
019550     MOVE 冷罨法料２ＷＲ             TO 冷罨法料２.
019560     MOVE 温罨法回数２ＷＲ           TO 温罨法回数２.
019570     MOVE 温罨法料２ＷＲ             TO 温罨法料２.
019580     MOVE 電療回数２ＷＲ             TO 電療回数２.
019590     MOVE 電療料２ＷＲ               TO 電療料２.
019600     MOVE 小計２ＷＲ                 TO 小計２.
019610     IF 長期逓減率２ＷＲ NOT = ZERO
019620         COMPUTE 長期逓減率２ = 長期逓減率２ＷＲ / 100
019630     END-IF.
019640     MOVE 長期込小計２ＷＲ           TO 長期込小計２.
019650*    ****************
019660*    * ３部位／８割 *
019670*    ****************
019680     MOVE 後療単価３８ＷＲ             TO 後療単価３８.
019690     MOVE 後療回数３８ＷＲ             TO 後療回数３８.
019700     MOVE 後療料３８ＷＲ               TO 後療料３８.
019710     MOVE 冷罨法回数３８ＷＲ           TO 冷罨法回数３８.
019720     MOVE 冷罨法料３８ＷＲ             TO 冷罨法料３８.
019730     MOVE 温罨法回数３８ＷＲ           TO 温罨法回数３８.
019740     MOVE 温罨法料３８ＷＲ             TO 温罨法料３８.
019750     MOVE 電療回数３８ＷＲ             TO 電療回数３８.
019760     MOVE 電療料３８ＷＲ               TO 電療料３８.
019770     MOVE 小計３８ＷＲ                 TO 小計３８.
019780     MOVE 多部位込小計３８ＷＲ         TO 多部位込小計３８.
019790     IF 長期逓減率３８ＷＲ NOT = ZERO
019800         COMPUTE 長期逓減率３８ = 長期逓減率３８ＷＲ / 100
019810     END-IF.
019820     MOVE 長期込小計３８ＷＲ           TO 長期込小計３８.
      */ 逓減率 0.7→0.6 /42505
           IF (施術和暦年月ＷＲ >= 42505) AND
              (助成レセＷ = ZERO)
              MOVE "60"                      TO 逓減３８
              MOVE "0.6"                     TO 多部位３８
              MOVE "==="                     TO 逓減訂正３８ 多部位訂正３８
           END-IF.
019830*    ****************
019840*    * ３部位／10割 *
019850*    ****************
019860     MOVE 逓減開始月３０ＷＲ           TO 逓減開始月３０.
019870     MOVE 逓減開始日３０ＷＲ           TO 逓減開始日３０.
019880     MOVE 後療単価３０ＷＲ             TO 後療単価３０.
019890     MOVE 後療回数３０ＷＲ             TO 後療回数３０.
019900     MOVE 後療料３０ＷＲ               TO 後療料３０.
019910     MOVE 冷罨法回数３０ＷＲ           TO 冷罨法回数３０.
019920     MOVE 冷罨法料３０ＷＲ             TO 冷罨法料３０.
019930     MOVE 温罨法回数３０ＷＲ           TO 温罨法回数３０.
019940     MOVE 温罨法料３０ＷＲ             TO 温罨法料３０.
019950     MOVE 電療回数３０ＷＲ             TO 電療回数３０.
019960     MOVE 電療料３０ＷＲ               TO 電療料３０.
019970     MOVE 小計３０ＷＲ                 TO 小計３０.
019980     IF 長期逓減率３０ＷＲ NOT = ZERO
019990         COMPUTE 長期逓減率３０ = 長期逓減率３０ＷＲ / 100
020000     END-IF.
020010     MOVE 長期込小計３０ＷＲ           TO 長期込小計３０.
020020*    ****************
020030*    * ４部位／５割 *
020040*    ****************
020050*     MOVE 後療単価４５ＷＲ             TO 後療単価４５.
020060*     MOVE 後療回数４５ＷＲ             TO 後療回数４５.
020070*     MOVE 後療料４５ＷＲ               TO 後療料４５.
020080*     MOVE 冷罨法回数４５ＷＲ           TO 冷罨法回数４５.
020090*     MOVE 冷罨法料４５ＷＲ             TO 冷罨法料４５.
020100*     MOVE 温罨法回数４５ＷＲ           TO 温罨法回数４５.
020110*     MOVE 温罨法料４５ＷＲ             TO 温罨法料４５.
020120*     MOVE 電療回数４５ＷＲ             TO 電療回数４５.
020130*     MOVE 電療料４５ＷＲ               TO 電療料４５.
020140*     MOVE 小計４５ＷＲ                 TO 小計４５.
020150*     MOVE 多部位込小計４５ＷＲ         TO 多部位込小計４５.
020160*     IF 長期逓減率４５ＷＲ NOT = ZERO
020170*         COMPUTE 長期逓減率４５ = 長期逓減率４５ＷＲ / 100
020180*     END-IF.
020190*     MOVE 長期込小計４５ＷＲ           TO 長期込小計４５.
020200*    ****************
020210*    * ４部位／８割 *
020220*    ****************
020230     MOVE 逓減開始月４８ＷＲ           TO 逓減開始月４８.
020240     MOVE 逓減開始日４８ＷＲ           TO 逓減開始日４８.
020250     MOVE 後療単価４８ＷＲ             TO 後療単価４８.
020260     MOVE 後療回数４８ＷＲ             TO 後療回数４８.
020270     MOVE 後療料４８ＷＲ               TO 後療料４８.
020280     MOVE 冷罨法回数４８ＷＲ           TO 冷罨法回数４８.
020290     MOVE 冷罨法料４８ＷＲ             TO 冷罨法料４８.
020300     MOVE 温罨法回数４８ＷＲ           TO 温罨法回数４８.
020310     MOVE 温罨法料４８ＷＲ             TO 温罨法料４８.
020320     MOVE 電療回数４８ＷＲ             TO 電療回数４８.
020330     MOVE 電療料４８ＷＲ               TO 電療料４８.
020340     MOVE 小計４８ＷＲ                 TO 小計４８.
020350     MOVE 多部位込小計４８ＷＲ         TO 多部位込小計４８.
020360     IF 長期逓減率４８ＷＲ NOT = ZERO
020370         COMPUTE 長期逓減率４８ = 長期逓減率４８ＷＲ / 100
020380     END-IF.
020390     MOVE 長期込小計４８ＷＲ           TO 長期込小計４８.
      */ 逓減率 0.7→0.6 /42505
           IF (施術和暦年月ＷＲ >= 42505) AND
              (助成レセＷ = ZERO)
              MOVE "60"                      TO 逓減４８
              MOVE "0.6"                     TO 多部位４８
              MOVE "==="                     TO 逓減訂正４８ 多部位訂正４８
           END-IF.
020400*    ****************
020410*    * ４部位／10割 *
020420*    ****************
020430     MOVE 逓減開始月４０ＷＲ           TO 逓減開始月４０.
020440     MOVE 逓減開始日４０ＷＲ           TO 逓減開始日４０.
020450     MOVE 後療単価４０ＷＲ             TO 後療単価４０.
020460     MOVE 後療回数４０ＷＲ             TO 後療回数４０.
020470     MOVE 後療料４０ＷＲ               TO 後療料４０.
020480     MOVE 冷罨法回数４０ＷＲ           TO 冷罨法回数４０.
020490     MOVE 冷罨法料４０ＷＲ             TO 冷罨法料４０.
020500     MOVE 温罨法回数４０ＷＲ           TO 温罨法回数４０.
020510     MOVE 温罨法料４０ＷＲ             TO 温罨法料４０.
020520     MOVE 電療回数４０ＷＲ             TO 電療回数４０.
020530     MOVE 電療料４０ＷＲ               TO 電療料４０.
020540     MOVE 小計４０ＷＲ                 TO 小計４０.
020550     IF 長期逓減率４０ＷＲ NOT = ZERO
020560         COMPUTE 長期逓減率４０ = 長期逓減率４０ＷＲ / 100
020570     END-IF.
020580     MOVE 長期込小計４０ＷＲ           TO 長期込小計４０.
020590*
020600**↓***********************************************************************
020610** ５部位／2.5割の印字は必要ない。
020620**------------------------------------------------------------------------*
020630**    *****************
020640**    * ５部位／2.5割 *
020650**    *****************
020660**     MOVE 後療単価５２ＷＲ             TO 後療単価５２.
020670**     MOVE 後療回数５２ＷＲ             TO 後療回数５２.
020680**     MOVE 後療料５２ＷＲ               TO 後療料５２.
020690**     MOVE 冷罨法回数５２ＷＲ           TO 冷罨法回数５２.
020700**     MOVE 冷罨法料５２ＷＲ             TO 冷罨法料５２.
020710**     MOVE 温罨法回数５２ＷＲ           TO 温罨法回数５２.
020720**     MOVE 温罨法料５２ＷＲ             TO 温罨法料５２.
020730**     MOVE 電療回数５２ＷＲ             TO 電療回数５２.
020740**     MOVE 電療料５２ＷＲ               TO 電療料５２.
020750**     MOVE 小計５２ＷＲ                 TO 小計５２.
020760**     MOVE 多部位込小計５２ＷＲ         TO 多部位込小計５２.
020770**     IF 長期逓減率５２ＷＲ NOT = ZERO
020780**         COMPUTE 長期逓減率５２ = 長期逓減率５２ＷＲ / 100
020790**     END-IF.
020800**     MOVE 長期込小計５２ＷＲ           TO 長期込小計５２.
020810**↑***********************************************************************
020820**
020830**    ****************
020840**    * ５部位／５割 *
020850**    ****************
020860**     MOVE SPACE TO 部位５Ｗ.
020870**     IF 小計５５ＷＲ NOT = ZERO
020880**        MOVE "5)33 "                      TO 逓減固定５Ｗ
020890**        MOVE "0.33"                       TO 多部位率５Ｗ
020900**        MOVE 逓減開始月５５ＷＲ           TO 逓減開始月５Ｗ
020910**        MOVE 逓減開始日５５ＷＲ           TO 逓減開始日５Ｗ
020920**        MOVE 後療単価５５ＷＲ             TO 後療単価５Ｗ
020930**        MOVE 後療回数５５ＷＲ             TO 後療回数５Ｗ
020940**        MOVE 後療料５５ＷＲ               TO 後療料５Ｗ
020950**        MOVE 冷罨法回数５５ＷＲ           TO 冷罨法回数５Ｗ
020960**        MOVE 冷罨法料５５ＷＲ             TO 冷罨法料５Ｗ
020970**        MOVE 温罨法回数５５ＷＲ           TO 温罨法回数５Ｗ
020980**        MOVE 温罨法料５５ＷＲ             TO 温罨法料５Ｗ
020990**        MOVE 電療回数５５ＷＲ             TO 電療回数５Ｗ
021000**        MOVE 電療料５５ＷＲ               TO 電療料５Ｗ
021010**        MOVE 小計５５ＷＲ                 TO 小計５Ｗ
021020**        MOVE 多部位込小計５５ＷＲ         TO 多部位込小計５Ｗ
021030**        IF 長期逓減率５５ＷＲ NOT = ZERO
021040**           COMPUTE 長期逓減率５Ｗ = 長期逓減率５５ＷＲ / 100
021050**        END-IF
021060**        MOVE 長期込小計５５ＷＲ           TO 長期込小計５Ｗ
021070***------------------------------------------------------------------------------------*
021080*** 平成14年6月から4部位目・5部位目の逓減率が45→33に変更。
021090*** それにより、5部位目（欄外）印字について、平成14年6月より前の場合、45を設定する。
021100***
021110**        IF ( 施術和暦年月ＷＲ < 41406 )
021120**           MOVE "5)45 "                   TO 逓減固定５Ｗ
021130**           MOVE "0.45"                    TO 多部位率５Ｗ
021140**        END-IF
021150***------------------------------------------------------------------------------------*
021160***
021170***        MOVE 部位５Ｗ                     TO 部位５５
021180**     END-IF.
021190**    ****************
021200**    * ５部位／８割 *
021210**    ****************
021220*     MOVE SPACE TO 部位５Ｗ.
021230*     IF 小計５８ＷＲ NOT = ZERO
021240**        MOVE "5)80 "                      TO 逓減固定５Ｗ
021250**        MOVE "0.8 "                       TO 多部位率５Ｗ
021260***/平成22年6月より、逓減率訂正印字/100602
021270**        IF ( 施術和暦年月ＷＲ >= 42206 )
021280**            MOVE "5)70 "                  TO 逓減固定５Ｗ
021290**            MOVE "0.7 "                   TO 多部位率５Ｗ
021300**        END-IF
021310**        MOVE 逓減開始月５８ＷＲ           TO 逓減開始月５Ｗ
021320**        MOVE 逓減開始日５８ＷＲ           TO 逓減開始日５Ｗ
021330**        MOVE 後療単価５８ＷＲ             TO 後療単価５Ｗ
021340**        MOVE 後療回数５８ＷＲ             TO 後療回数５Ｗ
021350**        MOVE 後療料５８ＷＲ               TO 後療料５Ｗ
021360**        MOVE 冷罨法回数５８ＷＲ           TO 冷罨法回数５Ｗ
021370**        MOVE 冷罨法料５８ＷＲ             TO 冷罨法料５Ｗ
021380**        MOVE 温罨法回数５８ＷＲ           TO 温罨法回数５Ｗ
021390**        MOVE 温罨法料５８ＷＲ             TO 温罨法料５Ｗ
021400**        MOVE 電療回数５８ＷＲ             TO 電療回数５Ｗ
021410**        MOVE 電療料５８ＷＲ               TO 電療料５Ｗ
021420**        MOVE 小計５８ＷＲ                 TO 小計５Ｗ
021430**        MOVE 多部位込小計５８ＷＲ         TO 多部位込小計５Ｗ
021440**        IF 長期逓減率５８ＷＲ NOT = ZERO
021450**           COMPUTE 長期逓減率５Ｗ = 長期逓減率５８ＷＲ / 100
021460**        END-IF
021470**        MOVE 長期込小計５８ＷＲ           TO 長期込小計５Ｗ
021480**        MOVE 部位５Ｗ                     TO 部位５８
      **/日付
021560*        MOVE 逓減開始月５８ＷＲ           TO 逓減開始月５Ｗ
      *        MOVE "月"                         TO 月ＣＭ
021570*        MOVE 逓減開始日５８ＷＲ           TO 逓減開始日５Ｗ
      *        MOVE "日"                         TO 日ＣＭ
      *        MOVE "("                          TO 括弧１Ｗ
      **/後療料
      *        IF 後療料５８ＷＲ NOT = ZERO
      *            MOVE "("                      TO 括弧２Ｗ
021580*            MOVE 後療単価５８ＷＲ         TO 後療単価５Ｗ
      *            MOVE "x"                      TO 乗算記号１Ｗ
021590*            MOVE 後療回数５８ＷＲ         TO 後療回数５Ｗ
      *            MOVE "="                      TO イコール１Ｗ
021600*            MOVE 後療料５８ＷＲ           TO 後療料５Ｗ
      *            MOVE ")"                      TO 括弧３Ｗ
      *        END-IF
      **/冷罨法
      *        IF 冷罨法料５８ＷＲ NOT = ZERO
      *            MOVE "+"                      TO 加算記号１Ｗ
      *            MOVE "("                      TO 括弧４Ｗ
      *            COMPUTE 冷罨法単価５Ｗ        =  冷罨法料５８ＷＲ / 冷罨法回数５８ＷＲ
      *            MOVE "x"                      TO 乗算記号２Ｗ
021610*            MOVE 冷罨法回数５８ＷＲ       TO 冷罨法回数５Ｗ
      *            MOVE "="                      TO イコール２Ｗ
021620*            MOVE 冷罨法料５８ＷＲ         TO 冷罨法料５Ｗ
      *            MOVE ")"                      TO 括弧５Ｗ
      *        END-IF
      **/温罨法
      *        IF 温罨法料５８ＷＲ NOT = ZERO
      *            MOVE "+"                      TO 加算記号２Ｗ
      *            MOVE "("                      TO 括弧６Ｗ
      *            COMPUTE 温罨法単価５Ｗ        =  温罨法料５８ＷＲ / 温罨法回数５８ＷＲ
      *            MOVE "x"                      TO 乗算記号３Ｗ
021630*            MOVE 温罨法回数５８ＷＲ       TO 温罨法回数５Ｗ
      *            MOVE "="                      TO イコール３Ｗ
021640*            MOVE 温罨法料５８ＷＲ         TO 温罨法料５Ｗ
      *            MOVE ")"                      TO 括弧７Ｗ
      *        END-IF
      **/電療料
      *        IF 電療料５８ＷＲ NOT = ZERO
      *            MOVE "+"                      TO 加算記号３Ｗ
      *            MOVE "("                      TO 括弧８Ｗ
      *            COMPUTE 電療単価５Ｗ          =  電療料５８ＷＲ / 電療回数５８ＷＲ
      *            MOVE "x"                      TO 乗算記号４Ｗ
021650*            MOVE 電療回数５８ＷＲ         TO 電療回数５Ｗ
      *            MOVE "="                      TO イコール４Ｗ
021660*            MOVE 電療料５８ＷＲ           TO 電療料５Ｗ
      *            MOVE ")"                      TO 括弧９Ｗ
      *        END-IF
      **
      *        MOVE ")"                          TO 括弧１０Ｗ
      **/多部位
      *        MOVE "x"                          TO 乗算記号５Ｗ
      **/ 逓減率 0.7→0.6 /42505
      *        IF (施術和暦年月ＷＲ >= 42505)
021290*           MOVE "0.6 "                    TO 多部位率５Ｗ
      *        ELSE
021290*           MOVE "0.7 "                    TO 多部位率５Ｗ
      *        END-IF
      **/長期
021680*        IF 長期逓減率５８ＷＲ NOT = ZERO
      *           MOVE "x"                       TO 乗算記号６Ｗ
021690*           COMPUTE 長期逓減率５Ｗ = 長期逓減率５８ＷＲ / 100
021700*        END-IF
      **/合計
      *        MOVE "="                          TO イコール５Ｗ
021710*        MOVE 長期込小計５８ＷＲ           TO 長期込小計５Ｗ
021720*        MOVE 部位５Ｗ                     TO 部位５８
021490*     END-IF.
021500**    ****************
021510**    * ５部位／10割 *
021520**    ****************
021530*     MOVE SPACE TO 部位５Ｗ.
021540*     IF 小計５０ＷＲ NOT = ZERO
021550**        MOVE "5)100"                      TO 逓減固定５Ｗ
021560**        MOVE 逓減開始月５０ＷＲ           TO 逓減開始月５Ｗ
021570**        MOVE 逓減開始日５０ＷＲ           TO 逓減開始日５Ｗ
021580**        MOVE 後療単価５０ＷＲ             TO 後療単価５Ｗ
021590**        MOVE 後療回数５０ＷＲ             TO 後療回数５Ｗ
021600**        MOVE 後療料５０ＷＲ               TO 後療料５Ｗ
021610**        MOVE 冷罨法回数５０ＷＲ           TO 冷罨法回数５Ｗ
021620**        MOVE 冷罨法料５０ＷＲ             TO 冷罨法料５Ｗ
021630**        MOVE 温罨法回数５０ＷＲ           TO 温罨法回数５Ｗ
021640**        MOVE 温罨法料５０ＷＲ             TO 温罨法料５Ｗ
021650**        MOVE 電療回数５０ＷＲ             TO 電療回数５Ｗ
021660**        MOVE 電療料５０ＷＲ               TO 電療料５Ｗ
021670**        MOVE 小計５０ＷＲ                 TO 小計５Ｗ
021680**        IF 長期逓減率５０ＷＲ NOT = ZERO
021690**           COMPUTE 長期逓減率５Ｗ = 長期逓減率５０ＷＲ / 100
021700**        END-IF
021710**        MOVE 長期込小計５０ＷＲ           TO 長期込小計５Ｗ
      **/日付
021560*        MOVE 逓減開始月５０ＷＲ           TO 逓減開始月５Ｗ
      *        MOVE "月"                         TO 月ＣＭ
021570*        MOVE 逓減開始日５０ＷＲ           TO 逓減開始日５Ｗ
      *        MOVE "日"                         TO 日ＣＭ
      *        MOVE "("                          TO 括弧１Ｗ
      **/後療料
      *        IF 後療料５０ＷＲ NOT = ZERO
      *            MOVE "("                      TO 括弧２Ｗ
021580*            MOVE 後療単価５０ＷＲ         TO 後療単価５Ｗ
      *            MOVE "x"                      TO 乗算記号１Ｗ
021590*            MOVE 後療回数５０ＷＲ         TO 後療回数５Ｗ
      *            MOVE "="                      TO イコール１Ｗ
021600*            MOVE 後療料５０ＷＲ           TO 後療料５Ｗ
      *            MOVE ")"                      TO 括弧３Ｗ
      *        END-IF
      **/冷罨法
      *        IF 冷罨法料５０ＷＲ NOT = ZERO
      *            MOVE "+"                      TO 加算記号１Ｗ
      *            MOVE "("                      TO 括弧４Ｗ
      *            COMPUTE 冷罨法単価５Ｗ        =  冷罨法料５０ＷＲ / 冷罨法回数５０ＷＲ
      *            MOVE "x"                      TO 乗算記号２Ｗ
021610*            MOVE 冷罨法回数５０ＷＲ       TO 冷罨法回数５Ｗ
      *            MOVE "="                      TO イコール２Ｗ
021620*            MOVE 冷罨法料５０ＷＲ         TO 冷罨法料５Ｗ
      *            MOVE ")"                      TO 括弧５Ｗ
      *        END-IF
      **/温罨法
      *        IF 温罨法料５０ＷＲ NOT = ZERO
      *            MOVE "+"                      TO 加算記号２Ｗ
      *            MOVE "("                      TO 括弧６Ｗ
      *            COMPUTE 温罨法単価５Ｗ        =  温罨法料５０ＷＲ / 温罨法回数５０ＷＲ
      *            MOVE "x"                      TO 乗算記号３Ｗ
021630*            MOVE 温罨法回数５０ＷＲ       TO 温罨法回数５Ｗ
      *            MOVE "="                      TO イコール３Ｗ
021640*            MOVE 温罨法料５０ＷＲ         TO 温罨法料５Ｗ
      *            MOVE ")"                      TO 括弧７Ｗ
      *        END-IF
      **/電療料
      *        IF 電療料５０ＷＲ NOT = ZERO
      *            MOVE "+"                      TO 加算記号３Ｗ
      *            MOVE "("                      TO 括弧８Ｗ
      *            COMPUTE 電療単価５Ｗ          =  電療料５０ＷＲ / 電療回数５０ＷＲ
      *            MOVE "x"                      TO 乗算記号４Ｗ
021650*            MOVE 電療回数５０ＷＲ         TO 電療回数５Ｗ
      *            MOVE "="                      TO イコール４Ｗ
021660*            MOVE 電療料５０ＷＲ           TO 電療料５Ｗ
      *            MOVE ")"                      TO 括弧９Ｗ
      *        END-IF
      **
      *        MOVE ")"                          TO 括弧１０Ｗ
      **/多部位
      **        乗算記号５Ｗ 多部位率５Ｗ
      **/長期
021680*        IF 長期逓減率５０ＷＲ NOT = ZERO
      *           MOVE "x"                       TO 乗算記号６Ｗ
021690*           COMPUTE 長期逓減率５Ｗ = 長期逓減率５０ＷＲ / 100
021700*        END-IF
      **/合計
      *        MOVE "="                          TO イコール５Ｗ
021710*        MOVE 長期込小計５０ＷＲ           TO 長期込小計５Ｗ
021720*        MOVE 部位５Ｗ                     TO 部位５０
021730*     END-IF.
021740**
021750     MOVE 適用１Ｗ                       TO 適用１.
021760     MOVE 適用２Ｗ                       TO 適用２.
021760     MOVE 適用３Ｗ                       TO 適用３.
      *
      */金属副子・運動後療の変更・追加/1805
           IF ( 施術和暦年月ＷＲ >= 43006 )
              INITIALIZE 連金運－キー
019550        MOVE 施術和暦ＷＲ TO 連金運－施術和暦
019560        MOVE 施術年ＷＲ   TO 連金運－施術年
019570        MOVE 施術月ＷＲ   TO 連金運－施術月
019580        MOVE 患者番号ＷＲ TO 連金運－患者番号
019590        MOVE 枝番ＷＲ     TO 連金運－枝番
              MOVE 助成種別ＷＲ TO 連金運－保険種別
              MOVE 39           TO 連金運－会コード
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
021770     MOVE レセ－合計                     TO 合計.
021370     MOVE レセ－受給者負担額             TO 一部負担金.
021380     MOVE レセ－助成請求金額             TO 請求金額.
           MOVE "一部負担金"                   TO 一部負担金ＣＭ.
           MOVE "保険給付金額"                 TO 請求金額ＣＭ.
021780     MOVE レセ－一部負担金               TO 受給者負担額.
021790     MOVE レセ－請求金額                 TO 助成請求額.
           MOVE "円"          TO 一部負担金円ＣＭ 請求金額円ＣＭ.
021800*
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
022100*------------------------------------------------------------------------------------*
022230**********************
022240* 施術所データセット *
022250**********************
           MOVE 都道府県ＪＩＳＷ       TO 都道府県番号.
022260*/共済時両方印字する/090608
022270*     IF 柔整師番号２Ｗ NOT = SPACE
022280*         MOVE 柔整師番号２Ｗ TO 柔整師番号２
022290*     END-IF.
022300     MOVE 柔整師番号Ｗ           TO 柔整師番号.
           MOVE 共済番号Ｗ             TO 共済番号.
022310*     MOVE 定額制受理番号Ｗ       TO 定額制受理番号.
022320     MOVE 施術所郵便番号１Ｗ     TO 施術所郵便番号１.
022330     MOVE 施術所郵便番号２Ｗ     TO 施術所郵便番号２.
022340*     MOVE 施術所住所Ｗ           TO 施術所住所１.
022350     MOVE 施術所住所１Ｗ         TO 施術所住所１.
022360     MOVE 施術所住所２Ｗ         TO 施術所住所２.
022370*     MOVE 接骨師会会員番号Ｗ     TO 接骨師会会員番号.
022380     MOVE 代表者カナＷ           TO 代表者カナ.
022390     MOVE 代表者名Ｗ             TO 代表者名.
022400     MOVE 施術所電話番号Ｗ       TO 施術所電話番号.
022410*
022420*     MOVE 銀行名支店名Ｗ         TO 銀行名支店名.
022430*     MOVE 預金種別コメントＷ     TO 預金種別.
022440     MOVE 口座番号Ｗ             TO 口座番号.
022450     MOVE 口座名義人カナ１Ｗ     TO 口座名義人カナ１.
022450     MOVE 口座名義人カナ２Ｗ     TO 口座名義人カナ２.
           MOVE 口座名義人カナ３Ｗ     TO 口座名義人カナ３.
           IF (口座名義人カナ３Ｗ = SPACE) AND (口座名義人２Ｗ = SPACE)
022460         MOVE 口座名義人Ｗ       TO 口座名義人
           END-IF.
           MOVE 金融機関名１Ｗ   TO 金融機関名１.
           MOVE 金融機関名２Ｗ   TO 金融機関名２.
           MOVE 金融機関名３Ｗ   TO 金融機関名３.
           MOVE 金融機関名４Ｗ   TO 金融機関名４.
           MOVE 支店名１Ｗ       TO 支店名１.
           MOVE 支店名２Ｗ       TO 支店名２.
           MOVE 支店名３Ｗ       TO 支店名３.
           MOVE 支店名４Ｗ       TO 支店名４.
      *     MOVE 振込チェックＷ   TO 振込チェック.
      *     MOVE 普通チェックＷ   TO 普通チェック.
      *     MOVE 当座チェックＷ   TO 当座チェック.
           MOVE 銀行チェックＷ   TO 銀行チェック.
           MOVE 金庫チェックＷ   TO 金庫チェック.
           MOVE 農協チェックＷ   TO 農協チェック.
           MOVE 本店チェックＷ   TO 本店チェック.
           MOVE 支店チェックＷ   TO 支店チェック.
           MOVE 本支所チェックＷ TO 本支所チェック.
      */委任者情報
           MOVE "また、療養費の受領を代替医療協会 事務局長 鈴木茂雄（住所は欄外に記入）に委任します。"
                                 TO 委任コメントＷ５.
           MOVE 委任コメント１Ｗ TO 会長委任コメント１.
           MOVE 委任コメント２Ｗ TO 会長委任コメント２.
           MOVE 委任コメント３Ｗ TO 会長委任コメント３.
           MOVE 委任コメント４Ｗ TO 会長委任コメント４.
           MOVE 委任コメント５Ｗ TO 会長委任コメント５.
022470*
022480     MOVE 接骨院名Ｗ             TO 接骨院名.
022490*
022500* / 柔整師・患者委任日 /
      */元号修正/↓↓↓20190405
           MOVE 施術和暦Ｗ         TO 元－元号区分
037380     READ 元号マスタ
037390      NOT INVALID KEY
037400         MOVE 元－元号名称   TO 受理和暦
037410     END-READ.
      */元号修正/↑↑↑20190405
022510     MOVE 柔整師年Ｗ             TO 受理年.
022520     MOVE 柔整師月Ｗ             TO 受理月.
022530     MOVE 柔整師日Ｗ             TO 受理日.
022540* ( 委任年月日 印刷するか )
022550     IF 連入－委任印刷  = ZERO
      */元号修正/↓↓↓20190405
              MOVE 施術和暦Ｗ         TO 元－元号区分
037380        READ 元号マスタ
037390        NOT INVALID KEY
037400            MOVE 元－元号名称   TO 委任和暦
037410        END-READ
      */元号修正/↑↑↑20190405
022560         MOVE 患者委任年Ｗ       TO 委任年
022570         MOVE 患者委任月Ｗ       TO 委任月
022580         MOVE 患者委任日Ｗ       TO 委任日
022590     END-IF.
022600*
           PERFORM フッタセット.
022610* 施術ID
022620     MOVE 県施術ＩＤＷ           TO 県施術ＩＤ.
      */助成の施術所ＩＤが入力されている場合は優先する/120711
           IF 市町村施術ＩＤＷ NOT = SPACE
      */京都市の後期＋障害/120606
               IF (受－保険種別 = 05 AND 受－助成種別 = 53) AND
                  (受－費用負担者番号助成(1:5) = "39261" OR "43264")
022020             MOVE 市町村施術ＩＤＷ TO 県施術ＩＤ
                   MOVE "99"             TO 費用負担者番号助成ＷＲ(1:2)
                   MOVE 費用負担者番号助成ＷＲ TO 公費負担者番号
                   STRING "["                    DELIMITED BY SIZE
                          費用負担者番号助成ＷＲ DELIMITED BY SIZE
                          "]"                    DELIMITED BY SIZE
                     INTO 保険者番号２
                   END-STRING
                   MOVE "京都市（重度障害老人）"  TO 保険者名称
               END-IF
           END-IF.
022630*     MOVE 市町村施術ＩＤＷ       TO 市町村施術ＩＤ.
022680*
022740*
022890*-------------------------------------------------------------------------*
022900*--- ※ レセ摘要再セットは、この印刷セットSECTION の最後にやること！ -----*
022910     PERFORM レセ摘要再セット.
022920*-------------------------------------------------------------------------*
022770*
022780* 県固有の備考
022790*     MOVE 受給者番号編集Ｗ       TO 県固有備考.
      */スペースが無いので５部位目の金額の欄で開いている行を使用。開いてなければ長期の７行目/110323
           IF 受給者番号編集Ｗ NOT = SPACE
               EVALUATE TRUE
               WHEN 部位５０ = SPACE
                   MOVE 受給者番号編集Ｗ TO 部位５０
               WHEN 部位５８ = SPACE
                   MOVE 受給者番号編集Ｗ TO 部位５８
               WHEN OTHER
                   MOVE SPACE            TO 長期理由文７
                   MOVE 受給者番号編集Ｗ TO 長期理由文７
               END-EVALUATE
           END-IF.
022860*
022870*****     PERFORM テスト印字処理.
022930*
022970*-------------------------------------------------------------------------*
022980*
022990*================================================================*
023000 項目初期化 SECTION.
023010*
023020     INITIALIZE 施術所情報Ｗ.
023030     INITIALIZE 受診者情報Ｗ.
023040     INITIALIZE 負傷情報Ｗ.
023050     INITIALIZE 備考情報Ｗ.
023060     INITIALIZE 料金１ＷＲ.
023070     INITIALIZE 料金２ＷＲ.
023080     INITIALIZE 料金３ＷＲ.
023100     INITIALIZE YDT6421P.
023090     MOVE SPACE TO YDT6421P.
023110*================================================================*
023120 料金情報取得 SECTION.
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
023140********************
023150* 料金データセット *
023160********************
023170*    ****************************************************************
023180*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
023190*    ****************************************************************
023200     MOVE レセ－初検料                 TO 初検料ＷＲ.
023210     IF レセ－時間外 = 1
023220         MOVE NC"○"                   TO 時間外チェックＷ
023230     END-IF.
023240     IF レセ－休日 = 1
023250         MOVE NC"○"                   TO 休日チェックＷ
023260     END-IF.
023270     IF レセ－深夜 = 1
023280         MOVE NC"○"                   TO 深夜チェックＷ
023290     END-IF.
023300     MOVE レセ－初検時相談料           TO 相談料ＷＲ.
023310*
023320     MOVE レセ－初検加算料             TO  初検加算料ＷＲ.
023330     MOVE レセ－再検料                 TO  再検料ＷＲ.
023340     MOVE レセ－往療距離               TO  往療距離ＷＲ.
023350     MOVE レセ－往療回数               TO  往療回数ＷＲ.
023360     MOVE レセ－往療料                 TO  往療料ＷＲ.
023370     MOVE レセ－往療加算料             TO  往療加算料ＷＲ.
023380*
023390     IF レセ－夜間 = 1
023400         MOVE NC"○"                   TO 夜間チェックＷ
023410     END-IF.
023420     IF レセ－暴風雨雪 = 1
023430         MOVE NC"○"                   TO 暴風雨雪チェックＷ
023440     END-IF.
023450*
023460     MOVE レセ－金属副子加算料         TO  金属副子加算料ＷＲ.
023470*
      */金属副子・運動後療の変更・追加/1805
           MOVE レセ－金属副子回数            TO 金属回数Ｗ.
           MOVE レセ－運動後療回数            TO 運動回数Ｗ.
           MOVE レセ－運動後療料              TO 運動料Ｗ.
023570*
023580     MOVE レセ－施術情報提供料         TO  施術情報提供料ＷＲ.
023590* 小計
022420     COMPUTE 小計Ｗ = レセ－小計 + レセ－運動後療料.
023610********************
023620* 初回処置料セット *
023630********************
023640     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
023650             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
023660         MOVE レセ－初回処置料(部位ＣＮＴ) TO 初回処置料ＷＲ(部位ＣＮＴ)
023670     END-PERFORM.
023680     MOVE レセ－初回処置料合計         TO 初回処置料合計Ｗ.
023690********************
023700* 逓減毎料金セット *
023710********************
023720*    **********
023730*    * １部位 *
023740*    **********
023750     MOVE レセ－後療単価１             TO 後療単価１ＷＲ.
023760     MOVE レセ－後療回数１             TO 後療回数１ＷＲ.
023770     MOVE レセ－後療料１               TO 後療料１ＷＲ.
023780     MOVE レセ－冷罨法回数１           TO 冷罨法回数１ＷＲ.
023790     MOVE レセ－冷罨法料１             TO 冷罨法料１ＷＲ.
023800     MOVE レセ－温罨法回数１           TO 温罨法回数１ＷＲ.
023810     MOVE レセ－温罨法料１             TO 温罨法料１ＷＲ.
023820     MOVE レセ－電療回数１             TO 電療回数１ＷＲ.
023830     MOVE レセ－電療料１               TO 電療料１ＷＲ.
023840     MOVE レセ－小計１                 TO 小計１ＷＲ.
           IF レセ－長期頻回逓減率１ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率１   TO 長期逓減率１ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率１       TO 長期逓減率１ＷＲ
           END-IF.
023860     MOVE レセ－長期込小計１           TO 長期込小計１ＷＲ.
023870*    **********
023880*    * ２部位 *
023890*    **********
023900     MOVE レセ－後療単価２             TO 後療単価２ＷＲ.
023910     MOVE レセ－後療回数２             TO 後療回数２ＷＲ.
023920     MOVE レセ－後療料２               TO 後療料２ＷＲ.
023930     MOVE レセ－冷罨法回数２           TO 冷罨法回数２ＷＲ.
023940     MOVE レセ－冷罨法料２             TO 冷罨法料２ＷＲ.
023950     MOVE レセ－温罨法回数２           TO 温罨法回数２ＷＲ.
023960     MOVE レセ－温罨法料２             TO 温罨法料２ＷＲ.
023970     MOVE レセ－電療回数２             TO 電療回数２ＷＲ.
023980     MOVE レセ－電療料２               TO 電療料２ＷＲ.
023990     MOVE レセ－小計２                 TO 小計２ＷＲ.
           IF レセ－長期頻回逓減率２ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率２   TO 長期逓減率２ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率２       TO 長期逓減率２ＷＲ
           END-IF.
024010     MOVE レセ－長期込小計２           TO 長期込小計２ＷＲ.
024020*    ****************
024030*    * ３部位／８割 *
024040*    ****************
024050     MOVE レセ－後療単価３８             TO 後療単価３８ＷＲ.
024060     MOVE レセ－後療回数３８             TO 後療回数３８ＷＲ.
024070     MOVE レセ－後療料３８               TO 後療料３８ＷＲ.
024080     MOVE レセ－冷罨法回数３８           TO 冷罨法回数３８ＷＲ.
024090     MOVE レセ－冷罨法料３８             TO 冷罨法料３８ＷＲ.
024100     MOVE レセ－温罨法回数３８           TO 温罨法回数３８ＷＲ.
024110     MOVE レセ－温罨法料３８             TO 温罨法料３８ＷＲ.
024120     MOVE レセ－電療回数３８             TO 電療回数３８ＷＲ.
024130     MOVE レセ－電療料３８               TO 電療料３８ＷＲ.
024140     MOVE レセ－小計３８                 TO 小計３８ＷＲ.
024150     MOVE レセ－多部位込小計３８         TO 多部位込小計３８ＷＲ.
           IF レセ－長期頻回逓減率３８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率３８   TO 長期逓減率３８ＷＲ
           ELSE
024160         MOVE レセ－長期逓減率３８       TO 長期逓減率３８ＷＲ
           END-IF.
024170     MOVE レセ－長期込小計３８           TO 長期込小計３８ＷＲ.
024180*    ****************
024190*    * ３部位／10割 *
024200*    ****************
024210     MOVE レセ－逓減開始月３０           TO 逓減開始月３０ＷＲ.
024220     MOVE レセ－逓減開始日３０           TO 逓減開始日３０ＷＲ.
024230     MOVE レセ－後療単価３０             TO 後療単価３０ＷＲ.
024240     MOVE レセ－後療回数３０             TO 後療回数３０ＷＲ.
024250     MOVE レセ－後療料３０               TO 後療料３０ＷＲ.
024260     MOVE レセ－冷罨法回数３０           TO 冷罨法回数３０ＷＲ.
024270     MOVE レセ－冷罨法料３０             TO 冷罨法料３０ＷＲ.
024280     MOVE レセ－温罨法回数３０           TO 温罨法回数３０ＷＲ.
024290     MOVE レセ－温罨法料３０             TO 温罨法料３０ＷＲ.
024300     MOVE レセ－電療回数３０             TO 電療回数３０ＷＲ.
024310     MOVE レセ－電療料３０               TO 電療料３０ＷＲ.
024320     MOVE レセ－小計３０                 TO 小計３０ＷＲ.
           IF レセ－長期頻回逓減率３０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率３０   TO 長期逓減率３０ＷＲ
           ELSE
024330         MOVE レセ－長期逓減率３０       TO 長期逓減率３０ＷＲ
           END-IF.
024340     MOVE レセ－長期込小計３０           TO 長期込小計３０ＷＲ.
024350*    ****************
024360*    * ４部位／５割 *
024370*    ****************
024380     MOVE レセ－後療単価４５             TO 後療単価４５ＷＲ.
024390     MOVE レセ－後療回数４５             TO 後療回数４５ＷＲ.
024400     MOVE レセ－後療料４５               TO 後療料４５ＷＲ.
024410     MOVE レセ－冷罨法回数４５           TO 冷罨法回数４５ＷＲ.
024420     MOVE レセ－冷罨法料４５             TO 冷罨法料４５ＷＲ.
024430     MOVE レセ－温罨法回数４５           TO 温罨法回数４５ＷＲ.
024440     MOVE レセ－温罨法料４５             TO 温罨法料４５ＷＲ.
024450     MOVE レセ－電療回数４５             TO 電療回数４５ＷＲ.
024460     MOVE レセ－電療料４５               TO 電療料４５ＷＲ.
024470     MOVE レセ－小計４５                 TO 小計４５ＷＲ.
024480     MOVE レセ－多部位込小計４５         TO 多部位込小計４５ＷＲ.
024490     MOVE レセ－長期逓減率４５           TO 長期逓減率４５ＷＲ.
024500     MOVE レセ－長期込小計４５           TO 長期込小計４５ＷＲ.
024510*    ****************
024520*    * ４部位／８割 *
024530*    ****************
024540     MOVE レセ－逓減開始月４８           TO 逓減開始月４８ＷＲ.
024550     MOVE レセ－逓減開始日４８           TO 逓減開始日４８ＷＲ.
024560     MOVE レセ－後療単価４８             TO 後療単価４８ＷＲ.
024570     MOVE レセ－後療回数４８             TO 後療回数４８ＷＲ.
024580     MOVE レセ－後療料４８               TO 後療料４８ＷＲ.
024590     MOVE レセ－冷罨法回数４８           TO 冷罨法回数４８ＷＲ.
024600     MOVE レセ－冷罨法料４８             TO 冷罨法料４８ＷＲ.
024610     MOVE レセ－温罨法回数４８           TO 温罨法回数４８ＷＲ.
024620     MOVE レセ－温罨法料４８             TO 温罨法料４８ＷＲ.
024630     MOVE レセ－電療回数４８             TO 電療回数４８ＷＲ.
024640     MOVE レセ－電療料４８               TO 電療料４８ＷＲ.
024650     MOVE レセ－小計４８                 TO 小計４８ＷＲ.
024660     MOVE レセ－多部位込小計４８         TO 多部位込小計４８ＷＲ.
           IF レセ－長期頻回逓減率４８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率４８   TO 長期逓減率４８ＷＲ
           ELSE
024670         MOVE レセ－長期逓減率４８       TO 長期逓減率４８ＷＲ
           END-IF.
024680     MOVE レセ－長期込小計４８           TO 長期込小計４８ＷＲ.
024690*    ****************
024700*    * ４部位／10割 *
024710*    ****************
024720     MOVE レセ－逓減開始月４０           TO 逓減開始月４０ＷＲ.
024730     MOVE レセ－逓減開始日４０           TO 逓減開始日４０ＷＲ.
024740     MOVE レセ－後療単価４０             TO 後療単価４０ＷＲ.
024750     MOVE レセ－後療回数４０             TO 後療回数４０ＷＲ.
024760     MOVE レセ－後療料４０               TO 後療料４０ＷＲ.
024770     MOVE レセ－冷罨法回数４０           TO 冷罨法回数４０ＷＲ.
024780     MOVE レセ－冷罨法料４０             TO 冷罨法料４０ＷＲ.
024790     MOVE レセ－温罨法回数４０           TO 温罨法回数４０ＷＲ.
024800     MOVE レセ－温罨法料４０             TO 温罨法料４０ＷＲ.
024810     MOVE レセ－電療回数４０             TO 電療回数４０ＷＲ.
024820     MOVE レセ－電療料４０               TO 電療料４０ＷＲ.
024830     MOVE レセ－小計４０                 TO 小計４０ＷＲ.
           IF レセ－長期頻回逓減率４０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率４０   TO 長期逓減率４０ＷＲ
           ELSE
024840         MOVE レセ－長期逓減率４０       TO 長期逓減率４０ＷＲ
           END-IF.
024850     MOVE レセ－長期込小計４０           TO 長期込小計４０ＷＲ.
024860*    *****************
024870*    * ５部位／2.5割 *
024880*    *****************
024890     MOVE レセ－後療単価５２             TO 後療単価５２ＷＲ.
024900     MOVE レセ－後療回数５２             TO 後療回数５２ＷＲ.
024910     MOVE レセ－後療料５２               TO 後療料５２ＷＲ.
024920     MOVE レセ－冷罨法回数５２           TO 冷罨法回数５２ＷＲ.
024930     MOVE レセ－冷罨法料５２             TO 冷罨法料５２ＷＲ.
024940     MOVE レセ－温罨法回数５２           TO 温罨法回数５２ＷＲ.
024950     MOVE レセ－温罨法料５２             TO 温罨法料５２ＷＲ.
024960     MOVE レセ－電療回数５２             TO 電療回数５２ＷＲ.
024970     MOVE レセ－電療料５２               TO 電療料５２ＷＲ.
024980     MOVE レセ－小計５２                 TO 小計５２ＷＲ.
024990     MOVE レセ－多部位込小計５２         TO 多部位込小計５２ＷＲ.
025000     MOVE レセ－長期逓減率５２           TO 長期逓減率５２ＷＲ.
025010     MOVE レセ－長期込小計５２           TO 長期込小計５２ＷＲ.
025020*    ****************
025030*    * ５部位／５割 *
025040*    ****************
025050     MOVE レセ－逓減開始月５５           TO 逓減開始月５５ＷＲ.
025060     MOVE レセ－逓減開始日５５           TO 逓減開始日５５ＷＲ.
025070     MOVE レセ－後療単価５５             TO 後療単価５５ＷＲ.
025080     MOVE レセ－後療回数５５             TO 後療回数５５ＷＲ.
025090     MOVE レセ－後療料５５               TO 後療料５５ＷＲ.
025100     MOVE レセ－冷罨法回数５５           TO 冷罨法回数５５ＷＲ.
025110     MOVE レセ－冷罨法料５５             TO 冷罨法料５５ＷＲ.
025120     MOVE レセ－温罨法回数５５           TO 温罨法回数５５ＷＲ.
025130     MOVE レセ－温罨法料５５             TO 温罨法料５５ＷＲ.
025140     MOVE レセ－電療回数５５             TO 電療回数５５ＷＲ.
025150     MOVE レセ－電療料５５               TO 電療料５５ＷＲ.
025160     MOVE レセ－小計５５                 TO 小計５５ＷＲ.
025170     MOVE レセ－多部位込小計５５         TO 多部位込小計５５ＷＲ.
025180     MOVE レセ－長期逓減率５５           TO 長期逓減率５５ＷＲ.
025190     MOVE レセ－長期込小計５５           TO 長期込小計５５ＷＲ.
025200*    ****************
025210*    * ５部位／８割 *
025220*    ****************
025230     MOVE レセ－逓減開始月５８           TO 逓減開始月５８ＷＲ.
025240     MOVE レセ－逓減開始日５８           TO 逓減開始日５８ＷＲ.
025250     MOVE レセ－後療単価５８             TO 後療単価５８ＷＲ.
025260     MOVE レセ－後療回数５８             TO 後療回数５８ＷＲ.
025270     MOVE レセ－後療料５８               TO 後療料５８ＷＲ.
025280     MOVE レセ－冷罨法回数５８           TO 冷罨法回数５８ＷＲ.
025290     MOVE レセ－冷罨法料５８             TO 冷罨法料５８ＷＲ.
025300     MOVE レセ－温罨法回数５８           TO 温罨法回数５８ＷＲ.
025310     MOVE レセ－温罨法料５８             TO 温罨法料５８ＷＲ.
025320     MOVE レセ－電療回数５８             TO 電療回数５８ＷＲ.
025330     MOVE レセ－電療料５８               TO 電療料５８ＷＲ.
025340     MOVE レセ－小計５８                 TO 小計５８ＷＲ.
025350     MOVE レセ－多部位込小計５８         TO 多部位込小計５８ＷＲ.
           IF レセ－長期頻回逓減率５８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率５８   TO 長期逓減率５８ＷＲ
           ELSE
025360         MOVE レセ－長期逓減率５８       TO 長期逓減率５８ＷＲ
           END-IF.
025370     MOVE レセ－長期込小計５８           TO 長期込小計５８ＷＲ.
025380*    ****************
025390*    * ５部位／10割 *
025400*    ****************
025410     MOVE レセ－逓減開始月５０           TO 逓減開始月５０ＷＲ.
025420     MOVE レセ－逓減開始日５０           TO 逓減開始日５０ＷＲ.
025430     MOVE レセ－後療単価５０             TO 後療単価５０ＷＲ.
025440     MOVE レセ－後療回数５０             TO 後療回数５０ＷＲ.
025450     MOVE レセ－後療料５０               TO 後療料５０ＷＲ.
025460     MOVE レセ－冷罨法回数５０           TO 冷罨法回数５０ＷＲ.
025470     MOVE レセ－冷罨法料５０             TO 冷罨法料５０ＷＲ.
025480     MOVE レセ－温罨法回数５０           TO 温罨法回数５０ＷＲ.
025490     MOVE レセ－温罨法料５０             TO 温罨法料５０ＷＲ.
025500     MOVE レセ－電療回数５０             TO 電療回数５０ＷＲ.
025510     MOVE レセ－電療料５０               TO 電療料５０ＷＲ.
025520     MOVE レセ－小計５０                 TO 小計５０ＷＲ.
           IF レセ－長期頻回逓減率５０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率５０   TO 長期逓減率５０ＷＲ
           ELSE
025530         MOVE レセ－長期逓減率５０       TO 長期逓減率５０ＷＲ
           END-IF.
025540     MOVE レセ－長期込小計５０           TO 長期込小計５０ＷＲ.
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
025550*
025560*================================================================*
025570 施術所情報取得 SECTION.
025580*
025590**************************************************
025600* 本院データを使用し、以下の情報を取得           *
025610* ● 柔整師番号.. 柔整師番号Ｗに格納             *
025620* ● 会員番号 ... 接骨師会会員番号Ｗに格納       *
025630* ● 代表者名 ... 代表者名Ｗに格納               *
025640* ● 住所1,2   ...施術所住所1,2Ｗに格納          *
025650* ● 電話番号 ... 施術所電話番号Ｗに格納         *
025660**************************************************
025670     MOVE ZERO  TO 施情－施術所番号.
025680     READ 施術所情報マスタ
025690     INVALID KEY
025700         CONTINUE
025710     NOT INVALID KEY
025720*
               MOVE 施情－都道府県ＪＩＳ TO 都道府県ＪＩＳＷ
025760         MOVE 施情－新柔整師番号   TO 柔整師番号Ｗ
025780*
025790*** 共済・自衛官の時のみ、柔整師番号の編集をする。
025800         EVALUATE 保険種別ＷＲ
025810         WHEN 04
025820             PERFORM 共済番号セット
025830         WHEN 09
025840             PERFORM 自衛官番号セット
025850         END-EVALUATE
025860***
025940         MOVE 施情－郵便番号１        TO 施術所郵便番号１Ｗ
025950         MOVE 施情－郵便番号２        TO 施術所郵便番号２Ｗ
025960         MOVE 施情－代表者カナ        TO 代表者カナＷ
025970         MOVE 施情－代表者名          TO 代表者名Ｗ
025980*
025990         MOVE 施情－接骨院名          TO 接骨院名Ｗ
026000*
026050         MOVE 施情－住所１            TO 施術所住所１Ｗ
026060         MOVE 施情－住所２            TO 施術所住所２Ｗ
026070*
026080         MOVE 施情－電話番号          TO 施術所電話番号Ｗ
026090** 振込先情報
026100         MOVE 施情－取引先銀行名      TO 取引先銀行名Ｗ
026110         MOVE 施情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
026120         MOVE 施情－預金種別          TO 預金種別Ｗ
026130         MOVE 施情－口座番号          TO 口座番号Ｗ
026140         MOVE 施情－口座名義人        TO 口座名義人Ｗ
026150         MOVE 施情－口座名義人カナ    TO 口座名義人カナＷ
026160         STRING 取引先銀行名Ｗ     DELIMITED BY SPACE
026170                " "                DELIMITED BY SIZE
026180                取引先銀行支店名Ｗ DELIMITED BY SPACE
026190                INTO 銀行名支店名Ｗ
026200         END-STRING
026210         EVALUATE 預金種別Ｗ
026220         WHEN 1
026230             MOVE NC"普通" TO 預金種別コメントＷ
026240         WHEN 2
026250             MOVE NC"当座" TO 預金種別コメントＷ
026260         WHEN OTHER
026270             MOVE SPACE    TO 預金種別コメントＷ
026280         END-EVALUATE
026290*
026300     END-READ.
026310*
026320*-------------------------------------------------------------------------*
026330*  組合→03, 共済・自衛官→04 は会の口座を使用
      */ 全て会の口座を使用 /140728
026350*-------------------------------------------------------------------------*
026360*     IF 保険種別ＷＲ = 03 OR 04 OR 09
              MOVE ZERO  TO 会情－柔整鍼灸区分
              MOVE 39    TO 会情－協会コード
              MOVE ZERO  TO 会情－保険種別
              MOVE ZERO  TO 会情－変更和暦年月
026480        READ 会情報マスタ
026490        NOT INVALID KEY
026500            IF ( 会情－取引先銀行名 NOT = SPACE ) AND
026510               ( 会情－口座番号     NOT = SPACE )
026520*           / 振込先情報の再セット /
026530                MOVE 会情－取引先銀行名      TO 取引先銀行名Ｗ
026540                MOVE 会情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
026550                MOVE 会情－預金種別          TO 預金種別Ｗ
026560                MOVE 会情－口座番号          TO 口座番号Ｗ
026570                MOVE 会情－口座名義人        TO 口座名義人Ｗ
026580                MOVE 会情－口座名義人カナ    TO 口座名義人カナＷ
026590                MOVE SPACE TO 銀行名支店名Ｗ
026600                STRING 取引先銀行名Ｗ     DELIMITED BY SPACE
026610                       " "                DELIMITED BY SIZE
026620                       取引先銀行支店名Ｗ DELIMITED BY SPACE
026630                       INTO 銀行名支店名Ｗ
026640                END-STRING
026650                EVALUATE 預金種別Ｗ
026660                WHEN 1
026670                    MOVE NC"普通" TO 預金種別コメントＷ
026680                WHEN 2
026690                    MOVE NC"当座" TO 預金種別コメントＷ
026700                WHEN OTHER
026710                    MOVE SPACE    TO 預金種別コメントＷ
026720                END-EVALUATE
026730            END-IF
026740        END-READ.
026750*     END-IF.
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
009809     END-IF.
      *
026760*
026770*********************************************
026780** ＩＤ管理マスタより　県施術ＩＤを取得する。
026790*********************************************
026800     EVALUATE 保険種別ＷＲ 
026810* 国保
026820         WHEN 01
026830            MOVE 保険者番号ＷＲ       TO 保険者番号比較Ｗ
026840* 退職
026850         WHEN 08
026860* 後期高齢
026870         WHEN 05
026880            MOVE 保険者番号ＷＲ(3:6)  TO 保険者番号比較Ｗ
026890     END-EVALUATE.
026900**   / 県施術ID /
026910     MOVE 01                     TO ＩＤ管－ＩＤ区分.
026920     MOVE ZERO                   TO ＩＤ管－施術所番号.
026930     MOVE 保険者番号比較Ｗ(1:2)  TO ＩＤ管－保険種別.
026940     MOVE SPACE                  TO ＩＤ管－保険者番号.
026950     READ ＩＤ管理マスタ
026960     NOT INVALID KEY
026970         MOVE ＩＤ管－施術ＩＤ番号   TO 県施術ＩＤＷ
026980     END-READ.
026990*
027000**   / 市町村施術ID /
027010     MOVE 02                     TO ＩＤ管－ＩＤ区分.
027020     MOVE ZERO                   TO ＩＤ管－施術所番号.
027030     MOVE 保険種別ＷＲ           TO ＩＤ管－保険種別.
027040     MOVE 保険者番号ＷＲ         TO ＩＤ管－保険者番号.
      */京都市の重度障害/120711
           IF 費用負担者番号助成ＷＲ(1:5) = "39261" OR "43264"
026910        MOVE 01                     TO ＩＤ管－ＩＤ区分
026920        MOVE ZERO                   TO ＩＤ管－施術所番号
026930        MOVE 50                     TO ＩＤ管－保険種別
026940        MOVE SPACE                  TO ＩＤ管－保険者番号
           END-IF.
      *
027050     READ ＩＤ管理マスタ
027060     NOT INVALID KEY
027070          MOVE ＩＤ管－施術ＩＤ番号   TO 市町村施術ＩＤＷ
027080     END-READ.
027090*
027100*================================================================*
027110 共済番号セット SECTION.
027120*
027130**************************************************************
027140* 保険者番号により、共済の番号を印字するか、柔整師番号か判定
027150**************************************************************
027160** 1.共済組合連盟
027170     MOVE SPACE  TO  脱出フラグ.
027180     IF 施情－共済連番号 NOT = ZERO
027190** 条件(保険者番号)
027200        IF ( 保険者番号ＷＲ(1:2) = "31" )  OR
027210           ( 保険者番号ＷＲ = "34130021" )
027220*
027230           MOVE  NC"共済組合連盟第"   TO 共済連番号名ＮＷ 
027240           MOVE  NC"号"               TO 共済連番号単位ＮＷ 
027250           MOVE  施情－共済連番号     TO 共済連番号Ｗ
027260           IF    (共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
027270                 MOVE SPACE TO  共済連番号Ｗ(1:1)
027280           ELSE
027290                 MOVE "YES" TO  脱出フラグ
027300           END-IF
027310           IF    (共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
027320                 MOVE SPACE TO  共済連番号Ｗ(2:1)
027330           ELSE
027340                 MOVE "YES" TO  脱出フラグ
027350           END-IF
027360           IF    (共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
027370                 MOVE SPACE TO  共済連番号Ｗ(3:1)
027380           ELSE
027390                 MOVE "YES" TO  脱出フラグ
027400           END-IF
027410           IF    (共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
027420                 MOVE SPACE TO  共済連番号Ｗ(4:1)
027430           ELSE
027440                 MOVE "YES" TO  脱出フラグ
027450           END-IF
027460           IF    (共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
027470                 MOVE SPACE TO  共済連番号Ｗ(5:1)
027480           ELSE
027490                 MOVE "YES" TO  脱出フラグ
027500           END-IF
027510           IF    (共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
027520                 MOVE SPACE TO  共済連番号Ｗ(6:1)
027530           ELSE
027540                 MOVE "YES" TO  脱出フラグ
027550           END-IF
027560**/共済時両方印字する/090608
027570*           MOVE  柔整師番号Ｗ         TO 柔整師番号２Ｗ
027580*           MOVE  共済連番号集団Ｗ     TO 柔整師番号Ｗ
024110            MOVE  共済連番号集団Ｗ     TO 共済番号Ｗ
027590        END-IF
027600     END-IF.
027610*
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
028050        END-IF
028060     END-IF.
028070*
028080*================================================================*
028090 自衛官番号セット SECTION.
028100*
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
028520*
028530*================================================================*
028540 受診者情報取得 SECTION.
028550*
028560**************************************************
028570* 連結データから受診者情報Ｆより以下の情報を取得 *
028580* ● 施術年 ..... 施術年Ｗに格納                 *
028590* ● 施術月 ..... 施術月Ｗに格納                 *
028600* ● 患者番号.... 患者番号Ｗに格納※ＦＤ連番用   *
028610* ● 記号 ....... 記号Ｗに格納                   *
028620* ● 番号 ....... 番号Ｗに格納                   *
028630* ● 保険者番号 . 保険者番号Ｗに格納             *
028640* ● 保険種別 ... 保険種別Ｗに格納               *
028650* ● 被保険者カナ.被保険者カナＷに格納           *
028660* ● 被保険者氏名.被保険者氏名Ｗに格納           *
028670* ● 住所１ ......被保険者住所１Ｗに格納         *
028680* ● 住所２ ......被保険者住所２Ｗに格納         *
028690* ● 患者カナ ....患者カナＷに格納               *
028700* ● 患者氏名 ....患者氏名Ｗに格納               *
028710* ● 患者性別 ....区分によりチェックに"○"を格納 *
028720* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
028730* ● 患者年 ......患者年Ｗに格納                 *
028740* ● 患者月 ......患者月Ｗに格納                 *
028750* ● 患者日 ......患者日Ｗに格納                 *
028760* ● 続柄 ........名称マスタより続柄Ｗに取得     *
028770**************************************************
028780     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
028790     MOVE 施術年ＷＲ         TO 受－施術年.
028800     MOVE 施術月ＷＲ         TO 受－施術月.
028810     MOVE 患者コードＷＲ     TO 受－患者コード.
028820     READ 受診者情報Ｆ
028830     INVALID KEY
028840         CONTINUE
028850*            /* ありえない */
028860     NOT INVALID KEY
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
               EVALUATE レセ－給付割合
               WHEN 10
                   MOVE NC"○" TO １０割チェックＷ
               WHEN 9
                   MOVE NC"○" TO ９割チェックＷ
      **/神奈川14、宮城04の場合、前期高齢者１割は、給付割合を８割にする。(国が１割負担するため、患者１割、保険者８割、国１割となる)
      *             IF (受－保険種別     = 01 AND 受－保険者番号(1:2) = "14" OR "04") OR
      *                (受－保険種別 NOT = 01 AND 受－保険者番号(3:2) = "14" OR "04")
      */神奈川14、宮城04、愛媛38、山口35、福島07、福井18の場合、前期高齢者１割は、給付割合を８割にする。(国が１割負担するため、患者１割、保険者８割、国１割となる)/130109山口追加/130319福島追加/130401福井追加
                   IF ((受－保険種別     = 01) AND (受－保険者番号(1:2) = "14" OR "04" OR "38" OR "35" OR "07" OR "18")) OR
                      ((受－保険種別 NOT = 01) AND (受－保険者番号(3:2) = "14" OR "04" OR "38" OR "35" OR "07" OR "18"))
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
028870         MOVE 受－施術和暦     TO 施術和暦Ｗ
028870         MOVE 受－施術年       TO 施術年Ｗ
028880         MOVE 受－施術月       TO 施術月Ｗ
028890         MOVE 受－患者番号     TO 患者番号Ｗ
028900*         MOVE 受－記号         TO 記号Ｗ
028910*         MOVE 受－番号         TO 番号Ｗ
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
028920         MOVE 受－保険者番号   TO 保険者番号Ｗ
028930         MOVE 受－保険種別     TO 保険種別Ｗ
028940** 全国土木の枝番削除
028950         IF ( 受－保険種別 = 01 ) AND ( 受－保険者番号(1:6) = "133033" )
028960            MOVE 受－保険者番号(1:6)  TO 保険者番号Ｗ
028970         END-IF
028980**
028990         MOVE 受－費用負担者番号助成 TO 市町村番号Ｗ
027240         MOVE 受－受益者番号助成     TO 受給者番号Ｗ
029000         MOVE 受－被保険者カナ TO 被保険者カナＷ
029010         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
029020*         MOVE 受－郵便番号１   TO 郵便番号１Ｗ
029030*         MOVE 受－郵便番号２   TO 郵便番号２Ｗ
029040         MOVE 受－住所１       TO 被保険者住所１Ｗ
029050         MOVE 受－住所２       TO 被保険者住所２Ｗ
029020         MOVE 受－患者郵便番号１   TO 郵便番号１Ｗ
029030         MOVE 受－患者郵便番号２   TO 郵便番号２Ｗ
029040         MOVE 受－患者住所１       TO 患者住所１Ｗ
029050         MOVE 受－患者住所２       TO 患者住所２Ｗ
      */ 電話番号追加 /42505
      *         IF 受－電話番号 NOT = SPACE
      *            STRING "電話:"        DELIMITED BY SIZE
      *                   受－電話番号   DELIMITED BY SPACE
      *              INTO 電話番号Ｗ
      *            END-STRING
      *         ELSE
                  IF 受－患者電話番号 NOT = SPACE
                     STRING "電話:"            DELIMITED BY SIZE
                            受－患者電話番号   DELIMITED BY SPACE
                       INTO 電話番号Ｗ
                     END-STRING
                  END-IF
      *         END-IF
029060         MOVE 受－患者カナ     TO 患者カナＷ
029070         MOVE 受－患者氏名     TO 患者氏名Ｗ
029080         EVALUATE 受－患者性別
029090         WHEN 1
029100             MOVE NC"○"  TO 男チェックＷ
029110         WHEN 2
029120             MOVE NC"○"  TO 女チェックＷ
029130         END-EVALUATE
029140         EVALUATE 受－患者和暦
029150         WHEN 1
029160             MOVE NC"○"  TO 明治チェックＷ
029170         WHEN 2
029180             MOVE NC"○"  TO 大正チェックＷ
029190         WHEN 3
029200             MOVE NC"○"  TO 昭和チェックＷ
029210         WHEN 4
029220             MOVE NC"○"  TO 平成チェックＷ
      */元号修正/20190405
023060         WHEN 5
                   MOVE "5令"   TO 令和ＣＭＷ
023070             MOVE NC"○"  TO 令和チェックＷ
029230         END-EVALUATE
029240         EVALUATE 受－患者和暦
029250         WHEN 1
029260             MOVE NC"明治"  TO 元号Ｗ
029270         WHEN 2
029280             MOVE NC"大正"  TO 元号Ｗ
029290         WHEN 3
029300             MOVE NC"昭和"  TO 元号Ｗ
029310         WHEN 4
029320             MOVE NC"平成"  TO 元号Ｗ
029330         END-EVALUATE
029340*
029350         MOVE 受－患者年  TO 患者年Ｗ
029360         MOVE 受－患者月  TO 患者月Ｗ
029370         MOVE 受－患者日  TO 患者日Ｗ
029380* 続柄
029390         EVALUATE 保険種別ＷＲ 
029400* 自衛官は無条件に"本人"
029410         WHEN  09
029420              MOVE NC"本人"    TO 続柄Ｗ
029430              MOVE NC"○"      TO 続柄本人チェックＷ
029440* 退職
029450         WHEN  08
029460             IF (本人家族区分ＷＲ = 1 ) AND (受－世帯主続柄 = 1)
029470                 MOVE NC"世帯主"  TO 続柄Ｗ
029480                 MOVE NC"○"      TO 続柄本人チェックＷ
029490             ELSE
029500*                / 徳島県特別 /
029510                 IF 受－保険者番号(3:2) = "36"
029520                    IF 本人家族区分ＷＲ = 1
029530                       MOVE NC"○"   TO 続柄本人チェックＷ
029540                    ELSE
029550                       MOVE NC"○"   TO 続柄家族チェックＷ
029560                    END-IF
029570                 ELSE
029580                    MOVE 05          TO 名－区分コード
029590                    MOVE 受－続柄    TO 名－名称コード
029600                    READ 名称マスタ
029610                    INVALID KEY
029620                        MOVE SPACE    TO 続柄Ｗ
029630                    NOT INVALID KEY
029640                        MOVE 名－略称 TO 続柄Ｗ
029650                    END-READ
029660                    MOVE NC"○"       TO 続柄家族チェックＷ
029670                END-IF
029680             END-IF
029690* 国保
029700         WHEN 01
029710             IF 本人家族区分ＷＲ = 1
029720                 MOVE NC"世帯主"  TO 続柄Ｗ
029730                 MOVE NC"○"      TO 続柄本人チェックＷ
029740             ELSE
029750                 MOVE 05          TO 名－区分コード
029760                 MOVE 受－続柄    TO 名－名称コード
029770                 READ 名称マスタ
029780                 INVALID KEY
029790                     MOVE SPACE    TO 続柄Ｗ
029800                 NOT INVALID KEY
029810                     MOVE 名－略称 TO 続柄Ｗ
029820                 END-READ
029830                 MOVE NC"○"       TO 続柄家族チェックＷ
029840             END-IF
029850         WHEN OTHER
029860             IF 本人家族区分ＷＲ = 1
029870                 MOVE NC"本人"    TO 続柄Ｗ
029880                 MOVE NC"○"      TO 続柄本人チェックＷ
029890             ELSE
029900                 MOVE 05          TO 名－区分コード
029910                 MOVE 受－続柄    TO 名－名称コード
029920                 READ 名称マスタ
029930                 INVALID KEY
029940                     MOVE SPACE    TO 続柄Ｗ
029950                 NOT INVALID KEY
029960                     MOVE 名－略称 TO 続柄Ｗ
029970                 END-READ
029980                 MOVE NC"○"       TO 続柄家族チェックＷ
029990             END-IF
030000         END-EVALUATE
030070**
030080* 14/10～　特別区分コメント印字
030090         IF 受－施術和暦年月 >= 41410
030100             IF 受－公費種別 = ZERO
030110                EVALUATE 受－特別区分
030120                WHEN 1
030130                   MOVE "70歳以上 1割"  TO 特別コメントＷ
030140                WHEN 2
030150                   MOVE "70歳以上 2割"  TO 特別コメントＷ
030160                WHEN 3
030170                   MOVE "70歳以上 3割"  TO 特別コメントＷ
030180                WHEN 6
030190                   IF 受－施術和暦年月 < 42004
030200                      MOVE "3歳未満"       TO 特別コメントＷ
030210                   ELSE
030220                      MOVE "義務教育就学前"  TO 特別コメントＷ
030230                   END-IF
030240                END-EVALUATE
030250             END-IF
030260         END-IF
030270**
030280*---  市町村独自仕様 -----*
030290* 14/10～　新潟　国保退職のみ表示が違う
030300         IF 受－施術和暦年月 >= 41410
030310             IF 受－公費種別 = ZERO
030320                EVALUATE 保険種別ＷＲ 
030330                WHEN 01
030340                   IF 受－保険者番号(1:2) = "15"
030350                      EVALUATE 受－特別区分
030360                      WHEN 1
030370                         MOVE "高齢者９割"    TO 特別コメントＷ
030380                      WHEN 2
030390                         MOVE "高齢者８割"    TO 特別コメントＷ
030400                      WHEN 3
030410                         MOVE "高齢者７割"    TO 特別コメントＷ
030420                      WHEN 6
030430                         IF 受－施術和暦年月 < 42004
030440                            MOVE "3歳未満 8割"   TO 特別コメントＷ
030450                         ELSE
030460                            MOVE "未就学児8割"   TO 特別コメントＷ
030470                         END-IF
030480                      WHEN OTHER
030490                         MOVE SPACE           TO 特別コメントＷ
030500                      END-EVALUATE
030510                   END-IF
030520                WHEN 08
030530                   IF 受－保険者番号(3:2) = "15"
030540                      EVALUATE 受－特別区分
030550                      WHEN 1
030560                         MOVE "高齢者９割"    TO 特別コメントＷ
030570                      WHEN 2
030580                         MOVE "高齢者８割"    TO 特別コメントＷ
030590                      WHEN 3
030600                         MOVE "高齢者７割"    TO 特別コメントＷ
030610                      WHEN 6
030620                         IF 受－施術和暦年月 < 42004
030630                            MOVE "3歳未満 8割"   TO 特別コメントＷ
030640                         ELSE
030650                            MOVE "未就学児8割"   TO 特別コメントＷ
030660                         END-IF
030670                      WHEN OTHER
030680                         MOVE SPACE           TO 特別コメントＷ
030690                      END-EVALUATE
030700                   END-IF
030710                END-EVALUATE
030720             END-IF
030730         END-IF
030740**
030750* 20/04～　後期高齢特別区分コメント印字
030760         IF 受－施術和暦年月 >= 42004
030770             IF 受－保険種別 = 05
030780                EVALUATE 受－特別区分
030790                WHEN 1
030800                   MOVE "高齢者１割"  TO 特別コメントＷ
030810                WHEN 2
030820                   MOVE "高齢者２割"  TO 特別コメントＷ
030830                WHEN 3
030840                   MOVE "高齢者３割"  TO 特別コメントＷ
030850                END-EVALUATE
030860             END-IF
030870         END-IF
031450* 15/7～　長野　助成
031460         IF 受－施術和暦年月 >= 41507
031470            IF ( 受－助成種別 = 52 OR 53 OR 55 ) AND
031480               ( 受－府県助成 = 20 ) AND
031490               ( 受－資格証明区分 NOT = 1 ) AND
031500               ( レセ－一部負担金 NOT = ZERO )
031510*
031520               MOVE 受－受益者番号助成(1:3)   TO 受給者番号編集Ｗ１
031530               MOVE 受－受益者番号助成(4:2)   TO 受給者番号編集Ｗ２
031540               MOVE 受－受益者番号助成(6:10)  TO 受給者番号編集Ｗ３
031550               MOVE "-"  TO 受給者区切１ 受給者区切２
031560            END-IF
031570         END-IF
031580*
031590     END-READ.
      *
028780     MOVE 施術和暦ＷＲ       TO 受２－施術和暦.
028790     MOVE 施術年ＷＲ         TO 受２－施術年.
028800     MOVE 施術月ＷＲ         TO 受２－施術月.
028810     MOVE 患者コードＷＲ     TO 受２－患者コード.
028820     READ 受診者情報２Ｆ
019630     INVALID KEY
              MOVE SPACE     TO 受２－レコード
              INITIALIZE        受２－レコード
           END-READ.
031600*================================================================*
031610 請求先情報取得 SECTION.
031620*
031630****************************************************
031640* 連結データから保険者マスタより請求先を取得する。 *
031650* ※保－請求先情報区分=1の場合請求先マスタを使用   *
031660* ● 請求先...... 請求先名称Ｗに格納               *
031670****************************************************
031730     MOVE 助成種別ＷＲ           TO 市－公費種別.
031740     MOVE 費用負担者番号助成ＷＲ TO 市－市町村番号.
031750     READ 市町村マスタ
031760     INVALID KEY
031770         MOVE SPACE      TO 請求先名称Ｗ
031780     NOT INVALID KEY
028520         IF 市－請求先区分 = 1
028530             MOVE 助成種別ＷＲ           TO 請先－保険種別
028540             MOVE 費用負担者番号助成ＷＲ TO 請先－保険者番号
028550             READ 請求先マスタ
028560             INVALID KEY
028570                 MOVE SPACE        TO 請求先名称Ｗ
028580             NOT INVALID KEY
028590                 MOVE 請先－保険者名称  TO 請求先名称Ｗ
028600             END-READ
028610         ELSE
028620             MOVE 市－市町村名称  TO 請求先名称Ｗ
028630         END-IF
028640     END-READ.
028650*
           MOVE 請求先名称Ｗ   TO 請求先名称ＷＲ.
           STRING 請求先名称Ｗ DELIMITED BY SPACE
                  "長　殿"     DELIMITED BY SIZE
                  INTO 請求先名称Ｗ
           END-STRING.
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
032130*================================================================*
032140 負傷データ取得 SECTION.
032150*
032160**************************************************
032170* 連結データから負傷データＦより以下の情報を取得 *
032180* ● 負傷名...部位＋負傷種別にて加工して格納     *
032190* ● 負傷年.......負傷年Ｗ                       *
032200* ● 負傷月.......負傷月Ｗ                       *
032210* ● 負傷日.......負傷日Ｗ                       *
032220* ● 開始年.......初検年Ｗ                       *
032230* ● 開始月.......初検月Ｗ                       *
032240* ● 開始日.......初検日Ｗ                       *
032250* ● 終了年.......終了年Ｗ                       *
032260* ● 終了月.......終了月Ｗ                       *
032270* ● 終了日.......終了日Ｗ                       *
032280* ● 実日数.......実日数Ｗ                       *
032290* ● 転帰区分 ....区分によりチェックに"○"を格納 *
032300* ● 金属副子 ....区分によりチェックに"○"を格納 *
032310* ● 経過コード...経過マスタより取得             *
032320**************************************************
032330*     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
032340*     MOVE 施術年ＷＲ         TO 負－施術年.
032350*     MOVE 施術月ＷＲ         TO 負－施術月.
032360*     MOVE 患者コードＷＲ     TO 負－患者コード.
032370*     READ 負傷データＦ
032380*     INVALID KEY
032390*         CONTINUE
032400**            /* ありえない */
032410*     NOT INVALID KEY
032420*         MOVE 負－部位数                   TO 部位数Ｗ
032430         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
032440                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
032450             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別Ｗ(部位ＣＮＴ)
032460             MOVE 負－部位(部位ＣＮＴ)     TO 部位Ｗ(部位ＣＮＴ)
032470             MOVE 負－左右区分(部位ＣＮＴ) TO 左右区分Ｗ(部位ＣＮＴ)
032480             MOVE 負－負傷位置番号(部位ＣＮＴ)
032490                                           TO 負傷位置番号Ｗ(部位ＣＮＴ)
032500********************************************************
032510* 注）全柔...部位名1+負傷種別＋部位名2にて加工して格納 *
032520********************************************************
032530* 負傷種別
032540             MOVE SPACE                     TO 負傷名称Ｗ
032550             MOVE 03                        TO 名－区分コード
032560             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
032570             READ 名称マスタ
032580             INVALID KEY
032590                 MOVE SPACE        TO 負傷名称Ｗ
032600             NOT INVALID KEY
032610                 MOVE 名－正式名称 TO 負傷名称Ｗ
032620             END-READ
032630* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
032680*
032690             PERFORM 部位名称埋込処理
032700*
032830             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
032840             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
032850             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
032860             MOVE 負－開始年(部位ＣＮＴ)   TO 初検年Ｗ(部位ＣＮＴ)
032870             MOVE 負－開始月(部位ＣＮＴ)   TO 初検月Ｗ(部位ＣＮＴ)
032880             MOVE 負－開始日(部位ＣＮＴ)   TO 初検日Ｗ(部位ＣＮＴ)
032890             IF 負－転帰区分(部位ＣＮＴ) = 9
032900                 MOVE 99                   TO 終了年Ｗ(部位ＣＮＴ)
032910                 MOVE 99                   TO 終了月Ｗ(部位ＣＮＴ)
032920                 MOVE 99                   TO 終了日Ｗ(部位ＣＮＴ)
032930             ELSE
032940                 MOVE 負－終了年(部位ＣＮＴ)   TO 終了年Ｗ(部位ＣＮＴ)
032950                 MOVE 負－終了月(部位ＣＮＴ)   TO 終了月Ｗ(部位ＣＮＴ)
032960                 MOVE 負－終了日(部位ＣＮＴ)   TO 終了日Ｗ(部位ＣＮＴ)
032970             END-IF
032980* 経過略称取得
032990             MOVE 01                         TO 経－区分コード
033000             MOVE 負－経過コード(部位ＣＮＴ) TO 経－経過コード
033010             READ 経過マスタ
033020             INVALID KEY
033030                 MOVE ZERO            TO 部位ＣＮＴＷ(部位ＣＮＴ)
033040                 MOVE SPACE           TO 部位区切Ｗ(部位ＣＮＴ)
033050                 MOVE SPACE           TO 経過略称Ｗ(部位ＣＮＴ)
033060             NOT INVALID KEY
033070                 EVALUATE 部位ＣＮＴ
033080                 WHEN 1
033090                     MOVE NC"①" TO 経過部位Ｗ
033100                 WHEN 2
033110                     MOVE NC"②" TO 経過部位Ｗ
033120                 WHEN 3
033130                     MOVE NC"③" TO 経過部位Ｗ
033140                 WHEN 4
033150                     MOVE NC"④" TO 経過部位Ｗ
033160                 WHEN 5
033170                     MOVE NC"⑤" TO 経過部位Ｗ
033180                 END-EVALUATE
033190                 STRING  経過部位Ｗ     DELIMITED BY SPACE
033200                         経－経過略称   DELIMITED BY SPACE
033210                        INTO 印刷経過略称Ｗ(部位ＣＮＴ)
033220                 END-STRING
033230             END-READ
033240*
033250             MOVE 負－転帰区分(部位ＣＮＴ) TO 転帰区分Ｗ(部位ＣＮＴ)
033260             EVALUATE 負－転帰区分(部位ＣＮＴ)
033270             WHEN 1
033280             WHEN 2
033290                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
033300             WHEN 3
033310                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
033320             WHEN 4
033330                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
033340             END-EVALUATE
033350*
      */実日数はレセ－部位実日数を転記する/160816
031230             MOVE レセ－部位実日数(部位ＣＮＴ) TO 実日数Ｗ(部位ＣＮＴ)
033360         END-PERFORM.
033370* 新規/継続 チェック
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
033430* 枝番判定用
033440         MOVE 負－開始診療日手動区分 TO  開始診療日手動区分Ｗ.
033450*
033460* 負傷原因印刷区分
033470         MOVE 負－レセ負傷原因印刷区分 TO レセ負傷原因印刷区分Ｗ.
027880         MOVE 負－レセ長期理由印刷区分 TO レセ長期理由印刷区分Ｗ.
033480*
033490*     END-READ.
033500*================================================================*
033510*================================================================*
033520 負傷原因取得 SECTION.
033530*
033540********************************************************************
033550*  負傷原因コードが同じものは、1行にまとめて印字する。
033560*  例: ①② 家で転んだ.
033570*     負傷原因コードが同じものをまとめ、テーブルにセット
033580*     (ただし、部位を飛んで同じものは、2行になる)
033590********************************************************************
033600     MOVE  ZERO   TO  カウンタ カウンタ２.
033610     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
033620             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
033630*
033640****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
033650        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
033660*
033670           IF カウンタ = ZERO
033680               MOVE 1   TO  カウンタ カウンタ２
033690               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
033700               MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
033710               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
033720           ELSE
033730              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
033740                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
033750                 COMPUTE カウンタ２ = カウンタ２  +  1
033760                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
033770              ELSE
033780                 COMPUTE カウンタ = カウンタ  +  1
033790                 MOVE 1   TO  カウンタ２
033800                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
033810                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
033820                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
033830              END-IF
033840           END-IF
033850        END-IF
033860     END-PERFORM.
033870**************************************************************************
033880*  負傷原因マスタより文章取得
033890**************************************************************************
033900     MOVE  ZERO   TO  カウンタ カウンタ２.
033910     PERFORM VARYING カウンタ FROM 1 BY 1
033920             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
033930** 健保は 区分 01
033940         MOVE 01                        TO 負原－区分コード
033950         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
033960         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
033970         READ 負傷原因Ｆ
033980         NOT INVALID KEY
033990             INITIALIZE 負傷原因ＷＴ
034000             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
034010             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
034020             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
034030             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
034040             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
034050             PERFORM VARYING カウンタ２ FROM 1 BY 1
034060                     UNTIL ( カウンタ２ > 9 )  OR 
034070                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
034080                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
034090                WHEN 1
034100                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
034110                WHEN 2
034120                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
034130                WHEN 3
034140                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
034150                WHEN 4
034160                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
034170                WHEN 5
034180                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
034170                WHEN 6
034180                   MOVE "⑥"  TO  負傷原因ナンバーＷ１(カウンタ２)
034170                WHEN 7
034180                   MOVE "⑦"  TO  負傷原因ナンバーＷ１(カウンタ２)
034190                WHEN OTHER
034200                   CONTINUE
034210                END-EVALUATE
034220             END-PERFORM
034230*
034240             IF 負原－負傷原因入力区分 = 1
034250                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
034260                        負傷原因１ＷＴ  DELIMITED BY SIZE
034270                        負傷原因２ＷＴ  DELIMITED BY SIZE
034280                        負傷原因３ＷＴ  DELIMITED BY SIZE
034290                        負傷原因４ＷＴ  DELIMITED BY SIZE
034300                        負傷原因５ＷＴ  DELIMITED BY SIZE
034310                        INTO 負傷原因内容合成Ｗ(カウンタ)
034320                 END-STRING
034330             ELSE
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
034420             END-IF
034430*
034440         END-READ
034450     END-PERFORM.
034460*
034470     PERFORM 負傷原因セット.
034480*
034490*================================================================*
034500 負傷原因セット SECTION.
034510*
034520**************************************************************************
034530*  文章が1行を超える時は、複数行に分解する。
034540**************************************************************************
034550     MOVE  ZERO   TO  カウンタ カウンタ２.
034560     PERFORM VARYING カウンタ FROM 1 BY 1
034570             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
034580*
034590          INITIALIZE 負傷原因内容分解ＸＷ
034600          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO  負傷原因内容分解ＸＷ
034610          IF  負傷原因内容１ＸＷ  NOT = SPACE
034620              COMPUTE カウンタ２ = カウンタ２  +  1
034630              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
034640          END-IF
034650          IF  負傷原因内容２ＸＷ  NOT = SPACE
034660              COMPUTE カウンタ２ = カウンタ２  +  1
034670              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
034680          END-IF
034690          IF  負傷原因内容３ＸＷ  NOT = SPACE
034700              COMPUTE カウンタ２ = カウンタ２  +  1
034710              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
034720          END-IF
034690          IF  負傷原因内容４ＸＷ  NOT = SPACE
034700              COMPUTE カウンタ２ = カウンタ２  +  1
034710              MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
034720          END-IF
034730*
034740     END-PERFORM.
034750*================================================================*
034760*================================================================*
034770 施術記録取得 SECTION.
034780*
034790************************************************************
034800* 作１データから負傷データＦより以下の情報を取得           *
034810* ● 初検加算 .....区分によりチェックに"○"を格納...複数可 *
034820* ● 往療加算 .....区分によりチェックに"○"を格納...複数可 *
034830************************************************************
034840     MOVE  SPACE  TO  初日再検フラグ.
034850     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 部位数Ｗ
034860         IF ( 施術年Ｗ = 初検年Ｗ(部位ＣＮＴ) ) AND
034870            ( 施術月Ｗ = 初検月Ｗ(部位ＣＮＴ) )
034880             MOVE 患者番号ＷＲ          TO 施記－患者番号
034890             MOVE 枝番ＷＲ              TO 施記－枝番
034900             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
034910             MOVE 初検年Ｗ(部位ＣＮＴ)  TO 開始年Ｗ(部位ＣＮＴ) 施記－施術年
034920             MOVE 初検月Ｗ(部位ＣＮＴ)  TO 開始月Ｗ(部位ＣＮＴ) 施記－施術月
034930             MOVE 初検日Ｗ(部位ＣＮＴ)  TO 開始日Ｗ(部位ＣＮＴ) 施記－施術日
034940         ELSE
034950             MOVE 患者番号ＷＲ          TO 施記－患者番号
034960             MOVE 枝番ＷＲ              TO 施記－枝番
034970             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
034980             MOVE 施術年ＷＲ            TO 施記－施術年
034990             MOVE 施術月ＷＲ            TO 施記－施術月
035000             MOVE ZERO                  TO 施記－施術日
035010         END-IF
035020         START 施術記録Ｆ   KEY IS >= 施記－患者コード
035030                                      施記－施術和暦年月日
035040         END-START
035050         IF 状態キー = "00"
      */実日数はレセ－部位実日数を転記する/160816
035060*             MOVE ZERO  TO 実日数Ｗ(部位ＣＮＴ)
035070             MOVE ZERO  TO 終了年ＷＴ
035080             MOVE ZERO  TO 終了月ＷＴ
035090             MOVE ZERO  TO 終了日ＷＴ
035100             MOVE SPACE TO 終了フラグ２
035110             PERFORM 施術記録Ｆ読込
035120             IF  ( 終了フラグ２      = SPACE   ) AND
035130                 ( 施記－患者コード  = 患者コードＷＲ ) AND
035140                 ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
035150                 ( 施記－施術年      = 施術年ＷＲ     ) AND
035160                 ( 施記－施術月      = 施術月ＷＲ     ) 
035170*
035180*        *****************************************************************
035190*        * 開始年月日 ( その部位が当月初検でないか、
035200*                       当月初検でも枝番がある時は、最初の施術日を開始日)*
035210*        *****************************************************************
035220                 IF ( 施術年Ｗ NOT = 初検年Ｗ(部位ＣＮＴ) ) OR
035230                    ( 施術月Ｗ NOT = 初検月Ｗ(部位ＣＮＴ) ) OR
035240                    ( 開始診療日手動区分Ｗ = 1 )
035250                     MOVE 施記－施術年   TO 開始年Ｗ(部位ＣＮＴ)
035260                     MOVE 施記－施術月   TO 開始月Ｗ(部位ＣＮＴ)
035270                     MOVE 施記－施術日   TO 開始日Ｗ(部位ＣＮＴ)
035280                 END-IF
035290             END-IF
035300             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
035310                           ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
035320                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
035330                           ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
035340                           ( 施記－施術月     NOT = 施術月ＷＲ       ) OR
035350                           ( 施記－施術日         > 終了日Ｗ(部位ＣＮＴ))
035360*               **********
035370*               * 実日数 *
035380*               **********
      */実日数はレセ－部位実日数を転記する/160816
035390*                COMPUTE 実日数Ｗ(部位ＣＮＴ) = 実日数Ｗ(部位ＣＮＴ) + 1
035400                MOVE 施記－施術年               TO 終了年ＷＴ
035410                MOVE 施記－施術月               TO 終了月ＷＴ
035420                MOVE 施記－施術日               TO 終了日ＷＴ
035430*
035440                PERFORM 施術記録Ｆ読込
035450            END-PERFORM
035460        END-IF
035470*       **************************
035480*       * 継続：終了年月日セット *
035490*       **************************
035500        IF 転帰区分Ｗ(部位ＣＮＴ) = 9
035510            MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
035520            MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
035530            MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
035540        END-IF
035550        IF 終了年月日Ｗ(部位ＣＮＴ) > 受理年月日Ｗ
035560            MOVE 終了年Ｗ(部位ＣＮＴ) TO 受理年Ｗ
035570            MOVE 終了月Ｗ(部位ＣＮＴ) TO 受理月Ｗ
035580            MOVE 終了日Ｗ(部位ＣＮＴ) TO 受理日Ｗ
035590        END-IF
035600     END-PERFORM.
035610*
035620** ----- 前月初検のみかを判定 -----------*
035630*
035640*     MOVE 患者番号ＷＲ          TO 施記－患者番号.
035650*     MOVE 枝番ＷＲ              TO 施記－枝番.
035660*     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
035670*     MOVE 施術年ＷＲ            TO 施記－施術年.
035680*     MOVE 施術月ＷＲ            TO 施記－施術月.
035690*     MOVE ZERO                  TO 施記－施術日.
035700*     START 施術記録Ｆ   KEY IS >= 施記－患者コード
035710*                                  施記－施術和暦年月日
035720*     END-START.
035730*     IF 状態キー = "00"
035740*             MOVE SPACE TO 終了フラグ２
035750*             PERFORM 施術記録Ｆ読込
035760*             IF  ( 終了フラグ２      = SPACE   ) AND
035770*                 ( 施記－患者コード  = 患者コードＷＲ ) AND
035780*                 ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
035790*                 ( 施記－施術年      = 施術年ＷＲ     ) AND
035800*                 ( 施記－施術月      = 施術月ＷＲ     ) 
035810** 当月施術開始日が再検かどうか判定
035820*                 IF   施記－再検料請求 = 1
035830*                      MOVE "YES"  TO  初日再検フラグ
035840*                 END-IF
035850**
035860*             END-IF
035870*     END-IF.
035880*     IF 初日再検フラグ = "YES"
035890*        PERFORM 前月初検のみ判定
035900*     END-IF.
035910*
035920*================================================================*
035930*================================================================*
035940 初検日以前のデータ判定 SECTION.
035950*
035960*********************************************************************************
035970*  最初の初検日以前の当月中に施術記録レコードがあった時(治癒、中止)は、請求区分の
035980*  継続にもチェックする。(新規と継続の両方)
035990*********************************************************************************
036000** 最初の初検日を取得
036010     MOVE SPACE                 TO 初検フラグ.
036020     MOVE 患者番号ＷＲ          TO 施記－患者番号.
036030     MOVE 枝番ＷＲ              TO 施記－枝番.
036040     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
036050     MOVE 施術年ＷＲ            TO 施記－施術年.
036060     MOVE 施術月ＷＲ            TO 施記－施術月.
036070     MOVE ZERO                  TO 施記－施術日.
036080     START 施術記録Ｆ   KEY IS >= 施記－患者コード
036090                                  施記－施術和暦年月日
036100     END-START.
036110     IF 状態キー = "00"
036120         MOVE ZERO  TO 初検和暦ＷＴ
036130         MOVE ZERO  TO 初検年ＷＴ
036140         MOVE ZERO  TO 初検月ＷＴ
036150         MOVE ZERO  TO 初検日ＷＴ
036160         MOVE SPACE TO 終了フラグ２
036170         PERFORM 施術記録Ｆ読込
036180         PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
036190                       ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
036200                       ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
036210                       ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
036220                       ( 施記－施術月     NOT = 施術月ＷＲ      ) OR
036230                       ( 初検フラグ           = "YES"           ) 
036240               IF  施記－診療区分 = 2
036250                   MOVE 施記－施術和暦           TO 初検和暦ＷＴ
036260                   MOVE 施記－施術年             TO 初検年ＷＴ
036270                   MOVE 施記－施術月             TO 初検月ＷＴ
036280                   MOVE 施記－施術日             TO 初検日ＷＴ
036290                   MOVE "YES"                    TO 初検フラグ
036300               END-IF
036310               PERFORM 施術記録Ｆ読込
036320         END-PERFORM
036330     END-IF.
036340*
036350* 初検日以前のデータ判定
036360     IF 初検フラグ = "YES"
036370        MOVE 患者番号ＷＲ          TO 施記－患者番号
036380        MOVE 枝番ＷＲ              TO 施記－枝番
036390        MOVE 初検和暦ＷＴ          TO 施記－施術和暦
036400        MOVE 初検年ＷＴ            TO 施記－施術年
036410        MOVE 初検月ＷＴ            TO 施記－施術月
036420        MOVE 初検日ＷＴ            TO 施記－施術日
036430        START 施術記録Ｆ   KEY IS <  施記－患者コード
036440                                     施記－施術和暦年月日
036450                                     REVERSED
036460        END-START
036470        IF 状態キー = "00"
036480           MOVE SPACE  TO 終了フラグ２
036490           PERFORM 施術記録Ｆ読込
036500           IF ( 終了フラグ２    = SPACE        ) AND
036510              ( 施記－患者番号  = 患者番号ＷＲ ) AND
036520              ( 施記－枝番      = 枝番ＷＲ     ) AND
036530              ( 施記－施術和暦  = 初検和暦ＷＴ ) AND
036540              ( 施記－施術年    = 初検年ＷＴ   ) AND
036550              ( 施記－施術月    = 初検月ＷＴ   )
036560*  初検日以前の当月中に施術記録レコードがあった時
036570                IF 継続チェックＷ = SPACE
036580                   MOVE NC"○"    TO 継続チェックＷ
036590                END-IF
036600           END-IF
036610         END-IF
036620     END-IF.
036630*
036640*================================================================*
036650 長期判定取得 SECTION.
036660*
036670* ３カ月以上の長期判定は "CHOUKI" を呼ぶ. 
036680     MOVE  SPACE TO  連期間－キー.
036690     INITIALIZE      連期間－キー.
036700     MOVE 施術和暦ＷＲ  TO  連期間－施術和暦.
036710     MOVE 施術年ＷＲ    TO  連期間－施術年.
036720     MOVE 施術月ＷＲ    TO  連期間－施術月.
036730     MOVE 患者番号ＷＲ  TO  連期間－患者番号.
036740     MOVE 枝番ＷＲ      TO  連期間－枝番.
036750*
036760     CALL   "CHOUKI".
036770     CANCEL "CHOUKI".
036780*
036790**** 適用１を使用 (「前月初検のみ」がある時は、くっつける)
036800     IF 連期間－対象フラグ  = "YES"
036810        IF 適用１Ｗ  = SPACE
036820           MOVE NC"※長期施術継続理由裏面に記載"  TO 適用１Ｗ
036830        ELSE
036840           STRING 適用１Ｗ           DELIMITED BY SPACE
036850                  NC"，"             DELIMITED BY SIZE
036860                  NC"※長期施術継続理由裏面に記載"   DELIMITED BY SIZE
036870                  INTO 適用１Ｗ
036880           END-STRING
036890        END-IF
036900     END-IF.
036910*
036920*================================================================*
036930 初検加算時刻取得 SECTION.
036940*****************************************************************
036950** 初検加算が時間外と深夜の時、「受付時間」を印字する。
036970*****************************************************************
036980     IF ( レセ－時間外 = 1 ) OR ( レセ－深夜 = 1 ) OR ( レセ－休日 = 1 )
036990*
037000         MOVE 患者番号ＷＲ          TO 施記－患者番号
037010         MOVE 枝番ＷＲ              TO 施記－枝番
037020         MOVE 施術和暦ＷＲ          TO 施記－施術和暦
037030         MOVE 施術年ＷＲ            TO 施記－施術年
037040         MOVE 施術月ＷＲ            TO 施記－施術月
037050         MOVE ZERO                  TO 施記－施術日
037060         START 施術記録Ｆ   KEY IS >= 施記－患者コード
037070                                      施記－施術和暦年月日
037080         END-START
037090         IF 状態キー = "00"
037100             MOVE ZERO  TO 初検加算カウント
037110             MOVE SPACE TO 終了フラグ２
037120             PERFORM 施術記録Ｆ読込
037130             PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
037140                           ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
037150                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
037160                           ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
037170                           ( 施記－施術月     NOT = 施術月ＷＲ      ) 
037180                   IF  ( 施記－初検加算 = 1 OR 2 OR 3 ) AND ( 施記－診療区分 = 2 )
035640                       COMPUTE 初検加算カウント = 初検加算カウント  + 1
037200                       IF  初検加算カウント <= 3
037210                           MOVE 施記－初検加算 TO 初検加算区分ＷＴ(初検加算カウント)
037220                           MOVE 施記－受付時   TO 初検加算時ＷＴ(初検加算カウント)
037230                           MOVE 施記－受付分   TO 初検加算分ＷＴ(初検加算カウント)
037240                       END-IF
037250                   END-IF
037260                   PERFORM 施術記録Ｆ読込
037270             END-PERFORM
037280** 初検加算の時刻をセット
033380             IF ( 初検加算時ＷＴ(1) NOT = ZERO ) OR ( 初検加算分ＷＴ(1) NOT = ZERO ) 
                       MOVE 初検加算時ＷＴ(1) TO 初検加算時Ｗ
                       MOVE ":"               TO 初検加算区切Ｗ
                       MOVE 初検加算分ＷＴ(1) TO 初検加算分Ｗ
                   END-IF
033380             IF ( 初検加算時ＷＴ(2) NOT = ZERO ) OR ( 初検加算分ＷＴ(2) NOT = ZERO ) 
031910                 PERFORM 初検加算適用セット
                   END-IF
037300         END-IF
037310*
037320     END-IF.
037330*
037340*================================================================*
037350 初検加算適用セット SECTION.
037360*
037370     PERFORM VARYING 番号カウンタ FROM 1 BY 1
037380              UNTIL  番号カウンタ > 3
037390         IF ( 初検加算時ＷＴ(番号カウンタ)  = ZERO )  AND 
037400            ( 初検加算分ＷＴ(番号カウンタ)  = ZERO ) 
037410             CONTINUE
037420         ELSE
037430* 固定項目
037440             EVALUATE 初検加算区分ＷＴ(番号カウンタ) 
037450             WHEN 1
037460                MOVE NC"時間外"   TO 加算内容Ｗ(番号カウンタ)
033320             WHEN 2
033330                MOVE NC"休　日"   TO 加算内容Ｗ(番号カウンタ)
037470             WHEN 3
037480                MOVE NC"深　夜"   TO 加算内容Ｗ(番号カウンタ)
037490             END-EVALUATE
037500*
037510             MOVE NC"："          TO 加算区切Ｗ(番号カウンタ)
037520             MOVE NC"時"          TO 時固定Ｗ(番号カウンタ)
037530             MOVE NC"分"          TO 分固定Ｗ(番号カウンタ)
037540*
037550**** 数字→日本語変換
037560* 時間
037570             MOVE 初検加算時ＷＴ(番号カウンタ)  TO  数字Ｗ
037580             IF 数字Ｗ >= 10
037590                 MOVE 数字Ｗ１    TO 負傷番号Ｗ１
037600                 PERFORM 日本語変換
037610                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ１(番号カウンタ)
037620                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
037630                 PERFORM 日本語変換
037640                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
037650             ELSE
037660                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
037670                 PERFORM 日本語変換
037680                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
037690             END-IF
037700* 分
037710             MOVE 初検加算分ＷＴ(番号カウンタ)  TO  数字Ｗ
037720             MOVE 数字Ｗ１    TO 負傷番号Ｗ１
037730             PERFORM 日本語変換
037740             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ１(番号カウンタ)
037750             MOVE 数字Ｗ２    TO 負傷番号Ｗ１
037760             PERFORM 日本語変換
037770             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ２(番号カウンタ)
037780** 
037790        END-IF
037800     END-PERFORM.
037810*
037820     MOVE  初検加算集団ＮＷ(1)   TO 初検加算時刻１Ｗ. 
037830     MOVE  初検加算集団ＮＷ(2)   TO 初検加算時刻２Ｗ. 
037840     MOVE  初検加算集団ＮＷ(3)   TO 初検加算時刻３Ｗ. 
037850*
037860**** 適用１か２を使用（長期理由記載で適用１を使っている時は、適用２）
037870     IF ( 初検加算時ＷＴ(2)  = ZERO ) AND ( 初検加算分ＷＴ(2)  = ZERO ) 
037880         CONTINUE
037890     ELSE
037900         IF 適用１Ｗ  = SPACE
037910               STRING NC"初検加算"       DELIMITED BY SIZE
037920                      初検加算時刻１Ｗ   DELIMITED BY SIZE
037930                      初検加算時刻２Ｗ   DELIMITED BY SIZE
037940                      初検加算時刻３Ｗ   DELIMITED BY SIZE
037950                      INTO 適用１Ｗ
037960               END-STRING
037970         ELSE
037980               STRING NC"初検加算"       DELIMITED BY SIZE
037990                      初検加算時刻１Ｗ   DELIMITED BY SIZE
038000                      初検加算時刻２Ｗ   DELIMITED BY SIZE
038010                      初検加算時刻３Ｗ   DELIMITED BY SIZE
038020                      INTO 適用２Ｗ
038030               END-STRING
038040         END-IF
038050     END-IF.
038060*
038070*================================================================*
038080 日本語変換 SECTION.
038090*
038100     MOVE NC"０"     TO 全角負傷番号Ｗ.
038110     CALL "htoz" WITH C LINKAGE
038120                        USING 負傷番号Ｗ１ 全角負傷番号Ｗ１.
038130*
038140*================================================================*
038150 施術記録Ｆ読込 SECTION.
038160*
038170     READ 施術記録Ｆ NEXT
038180     AT END
038190         MOVE "YES" TO 終了フラグ２
038200     END-READ.
038210*
038220*================================================================*
038230 助成印取得 SECTION.
038240*
038250* 2006/04 変更
038260* 助成印は "JOSEIMEI" を呼ぶ. 
038270     MOVE SPACE TO  連助成名称－キー.
038280     INITIALIZE     連助成名称－キー.
038290     MOVE 助成種別ＷＲ           TO 連助成名称－助成種別.
038300     MOVE 費用負担者番号助成ＷＲ TO 連助成名称－費用負担者番号助成.
           MOVE 39                     TO 連助成名称－協会コード.
038310*
038320     CALL   "JOSEIMEI".
038330     CANCEL "JOSEIMEI".
038340*
038350     MOVE 連助成名称－１文字 TO 助成印Ｗ.
038740*------------------------------------------------------------------------*
038750*/ 大阪("27")の国保・退職で助成がある場合、助成番号を印字する
038760*/ 大阪の後期高齢も同様に/
038770*
038780     MOVE SPACE TO 助成番号Ｗ.
038790     IF ( 助成種別ＷＲ NOT = ZERO ) AND
038800        ( 費用負担者番号助成ＷＲ(1:2) NOT = "99" ) AND
038810        ( 費用負担者番号助成ＷＲ(3:2)     = "27" )
038820        IF ( 公費種別ＷＲ NOT = 05 ) AND
038830           (( 保険種別ＷＲ = 01 AND 保険者番号ＷＲ(1:2) = "27" ) OR
038840            ( 保険種別ＷＲ = 08 AND 保険者番号ＷＲ(3:2) = "27" ))
038850           MOVE 費用負担者番号助成ＷＲ(1:2) TO 助成番号Ｗ
038860        END-IF
038870*
038880        IF (保険種別ＷＲ = 05) AND (保険者番号ＷＲ(3:2) = "27" )
038890           MOVE 費用負担者番号助成ＷＲ(1:2) TO 助成番号Ｗ
038900        END-IF
038910     END-IF.
038920*
039500*================================================================*
039510 前月初検のみ判定 SECTION.
039520*
039530*** 前月の通院日が初検か判定 
039540     MOVE  SPACE            TO 前月フラグ.
039550     MOVE 受－患者コード    TO 施記－患者コード.
039560     MOVE 受－施術和暦      TO 施記－施術和暦.
039570     MOVE 受－施術年        TO 施記－施術年.
039580     MOVE 受－施術月        TO 施記－施術月.
039590     MOVE 1                 TO 施記－施術日.
039600     START 施術記録Ｆ   KEY IS <  施記－患者コード
039610                                  施記－施術和暦年月日
039620                                  REVERSED
039630     END-START.
039640     IF 状態キー = "00"
039650         MOVE SPACE  TO 終了フラグ２
039660         PERFORM 施術記録Ｆ読込
039670         IF ( 終了フラグ２      = SPACE  ) AND
039680            ( 施記－患者コード  = 受－患者コード ) AND
039690            ( 施記－診療区分    = 2 ) 
039700*
039710            PERFORM 前月判定
039720**** 適用１を使用
039730            IF 前月フラグ = "YES"
039740               MOVE NC"※前月初検のみ"    TO  適用１Ｗ
039750            END-IF
039760**
039770         END-IF
039780     END-IF.
039790*
039800*================================================================*
039810 前月判定  SECTION.
039820* 
039830*** 読み込んだ施術記録の年月が、前月かどうか判定 (年月の差が 1 か?)
039840      MOVE  SPACE  TO  前月フラグ.
039850      INITIALIZE  計算年月日Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
039860**
039870      MOVE 受－施術和暦    TO 終了和暦２Ｗ.
039880      MOVE 受－施術年      TO 終了年２Ｗ.
039890      MOVE 受－施術月      TO 終了月２Ｗ.
039900      MOVE 施記－施術和暦  TO 開始和暦２Ｗ.
039910      MOVE 施記－施術年    TO 開始年２Ｗ.
039920      MOVE 施記－施術月    TO 開始月２Ｗ.
039930*
039940      EVALUATE TRUE
039950       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ = 終了年２Ｗ)
039960            PERFORM  前月比較月
039970       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ NOT = 終了年２Ｗ)
039980            PERFORM  前月比較年
039990       WHEN  開始和暦２Ｗ NOT = 終了和暦２Ｗ 
040000            PERFORM  前月比較元号
040010      END-EVALUATE.
040020*
040030      IF 計算月Ｗ = 1
040040         MOVE  "YES"  TO  前月フラグ
040050      END-IF.
040060*
040070*================================================================*
040080 前月比較月  SECTION.
040090*
040100     IF  終了月２Ｗ >  開始月２Ｗ
040110         COMPUTE 計算月Ｗ = 終了月２Ｗ - 開始月２Ｗ
040120     ELSE
040130        MOVE ZERO TO 計算月Ｗ
040140     END-IF.
040150*
040160*================================================================*
040170 前月比較年  SECTION.
040180*
040190     IF  終了年２Ｗ >  開始年２Ｗ
040200         COMPUTE 計算年Ｗ = 終了年２Ｗ - 開始年２Ｗ
040210         COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
040220     ELSE
040230        MOVE ZERO TO 計算月Ｗ
040240     END-IF.
040250*
040260*================================================================*
040270 前月比較元号  SECTION.
040280*
040290     MOVE 開始和暦２Ｗ TO 元－元号区分.
040300     READ 元号マスタ
040310     NOT INVALID KEY
040320         MOVE 元－開始西暦年 TO 開始西暦年Ｗ
040330     END-READ.
040340     MOVE 終了和暦２Ｗ TO 元－元号区分.
040350     READ 元号マスタ
040360     NOT INVALID KEY
040370         MOVE 元－開始西暦年 TO 終了西暦年Ｗ
040380     END-READ.
040390**
040400     IF (開始西暦年Ｗ NOT = ZERO) AND (終了西暦年Ｗ NOT = ZERO)
040410        COMPUTE 開始西暦年Ｗ = 開始西暦年Ｗ + 開始年２Ｗ - 1
040420        COMPUTE 終了西暦年Ｗ = 終了西暦年Ｗ + 終了年２Ｗ - 1
040430*
040440        IF 終了西暦年Ｗ =  開始西暦年Ｗ
040450           PERFORM  前月比較月
040460        ELSE
040470           IF  終了西暦年Ｗ >  開始西暦年Ｗ
040480               COMPUTE 計算年Ｗ = 終了西暦年Ｗ - 開始西暦年Ｗ
040490               COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
040500           ELSE
040510               MOVE ZERO TO 計算月Ｗ
040520           END-IF
040530        END-IF
040540     ELSE
040550        MOVE ZERO TO 計算月Ｗ
040560     END-IF.
040570*
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
040750*================================================================*
040760 印刷処理 SECTION.
040770*
040780     MOVE "YDT6421P" TO  定義体名Ｐ.
040790     MOVE "SCREEN"   TO  項目群名Ｐ.
040800     WRITE YDT6421P.
040810***     WRITE 印刷レコード.
040820     PERFORM エラー処理Ｐ.
040830*================================================================*
040840 エラー処理Ｐ SECTION.
040850*
040860     IF 通知情報Ｐ NOT = "00"
040870         DISPLAY NC"帳票エラー"              UPON CONS
040880         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
040890         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
040900         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
040910         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
040920                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
040930         ACCEPT  キー入力 FROM CONS
040940         PERFORM ファイル閉鎖
040950         MOVE 99  TO PROGRAM-STATUS
040960         EXIT PROGRAM
040970     END-IF.
040980*================================================================*
040990 受診者印刷区分更新 SECTION.
041000*
041010** //  受診者情報Ｆの印刷区分に１をセットし、更新する。//  
041020*
041030     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
041040     MOVE 施術年ＷＲ         TO 受－施術年.
041050     MOVE 施術月ＷＲ         TO 受－施術月.
041060     MOVE 患者コードＷＲ     TO 受－患者コード.
041070     READ 受診者情報Ｆ
041080     NOT INVALID KEY
041090         MOVE  1  TO  受－レセ印刷区分助成
041100         REWRITE  受－レコード
041110         END-REWRITE
041120         IF 状態キー NOT = "00"
041130            MOVE NC"受診者" TO ファイル名
041140            PERFORM エラー表示
041150         END-IF
041160     END-READ.
041170*
041180*================================================================*
041190 委任年月日取得 SECTION.
041200*
041210** ---// ここの受理年には、最終通院日が入っている為、退避する //----
041220     MOVE 受理年Ｗ   TO 最終通院年Ｗ.
041230     MOVE 受理月Ｗ   TO 最終通院月Ｗ.
041240     MOVE 受理日Ｗ   TO 最終通院日Ｗ.
041250***/月末日固定
041260* (柔整師側)
041270*     EVALUATE レセプト日付区分Ｗ 
041280*    /  最終通院日 /
041290*     WHEN ZERO
041300*         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
041310*         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
041320*         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
041330*    /  月末日 /
041340*     WHEN 1 
041350         PERFORM 月末日取得
041360         MOVE 受理年Ｗ     TO 柔整師年Ｗ.
041370         MOVE 受理月Ｗ     TO 柔整師月Ｗ.
041380         MOVE 受理日Ｗ     TO 柔整師日Ｗ.
041390*    /  印字なし /
041400*     WHEN 9
041410*         MOVE ZERO         TO 柔整師年Ｗ
041420*         MOVE ZERO         TO 柔整師月Ｗ
041430*         MOVE ZERO         TO 柔整師日Ｗ
041440*    /  その他は、最終通院日 /
041450*     WHEN OTHER
041460*         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
041470*         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
041480*         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
041490*     END-EVALUATE.
041500**
041510* (患者側)
041520*     EVALUATE レセプト患者日付区分Ｗ 
041530*    /  最終通院日 /
041540*     WHEN ZERO
041550*         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
041560*         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
041570*         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
041580*    /  月末日 /
041590*     WHEN 1 
041600*         PERFORM 月末日取得
041610         MOVE 受理年Ｗ     TO 患者委任年Ｗ.
041620         MOVE 受理月Ｗ     TO 患者委任月Ｗ.
041630         MOVE 受理日Ｗ     TO 患者委任日Ｗ.
041640*    /  印字なし /
041650*     WHEN 9
041660*         MOVE ZERO         TO 患者委任年Ｗ
041670*         MOVE ZERO         TO 患者委任月Ｗ
041680*         MOVE ZERO         TO 患者委任日Ｗ
041690*    /  その他は、最終通院日 /
041700*     WHEN OTHER
041710*         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
041720*         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
041730*         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
041740*     END-EVALUATE.
041750*
041760*================================================================*
041770*================================================================*
041780 月末日取得 SECTION.
041790*
041800     MOVE 施術年ＷＲ   TO 受理年Ｗ.
041810     MOVE 施術月ＷＲ   TO 受理月Ｗ.
041820     MOVE 施術和暦ＷＲ TO 元－元号区分.
041830     READ 元号マスタ
041840     NOT INVALID KEY
041850         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
041860     END-READ.
041870     IF 施術西暦年Ｗ NOT = ZERO
041880        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
041890     END-IF.
041900*
041910     EVALUATE 施術月ＷＲ
041920     WHEN 4
041930     WHEN 6
041940     WHEN 9
041950     WHEN 11
041960         MOVE 30 TO 受理日Ｗ
041970     WHEN 2
041980         DIVIDE 4 INTO 施術西暦年Ｗ GIVING    商Ｗ
041990                                    REMAINDER 余Ｗ
042000         END-DIVIDE
042010         IF 余Ｗ = ZERO
042020             MOVE 29 TO 受理日Ｗ
042030         ELSE
042040             MOVE 28 TO 受理日Ｗ
042050         END-IF
042060     WHEN 1
042070     WHEN 3
042080     WHEN 5
042090     WHEN 7
042100     WHEN 8
042110     WHEN 10
042120     WHEN 12
042130         MOVE 31 TO 受理日Ｗ
042140     WHEN OTHER
042150          CONTINUE
042160     END-EVALUATE.
042170*
042180*================================================================*
042190 助成レセまとめ判定 SECTION.
042200*---------------------------------------------------------------------------*
042210* 市町村マスタを読み、レセまとめ区分＝１でかつ、本体保険が国保・退職
042220* の時は、フラグYES (金額を助成込みで印字）
042230*（例：横浜市の障害は、本体保険（国保系）のレセプト１枚で請求、助成レセはなし）
042240*---------------------------------------------------------------------------*
042250*
042260     MOVE SPACE TO 助成レセまとめフラグ.
           IF ( レセ－本体まとめ区分 = 1 )
042620           MOVE "YES" TO 助成レセまとめフラグ
042630*        END-IF
042640     END-IF.
042650*
042660*----------------------------------------------------------------------*
042670** / 神奈川県固有：摘要に負担者番号と受給者番号 /
042680     IF ( 助成レセまとめフラグ = "YES" ) AND
042690        ( 受－費用負担者番号助成(3:2) = "14" )
042700        IF 受－費用負担者番号助成(1:2) NOT = "99"
042770            MOVE 受－費用負担者番号助成    TO 公費負担者番号
042780*            MOVE 受－受益者番号助成        TO 受給者番号
      */受給者番号が８文字以上の場合枠を無視して印刷する/110425
                  MOVE 受－受益者番号助成   TO 受給者番号Ｗ
                  IF 印刷受給者番号２Ｗ = SPACE
016830                MOVE 印刷受給者番号Ｗ TO 受給者番号
                  ELSE
                      MOVE 受給者番号Ｗ     TO 受給者番号２
                  END-IF
042790        END-IF
042800     END-IF.
042810*/和歌山県障害乳幼児ひとり親/100518
042820     IF ( 助成レセまとめフラグ = "YES" ) AND
042830        ( 受－費用負担者番号助成(3:2) = "30" )
042840        IF 受－費用負担者番号助成(1:2) NOT = "99"
042910            MOVE 受－費用負担者番号助成    TO 公費負担者番号 
042920*            MOVE 受－受益者番号助成        TO 受給者番号
      */受給者番号が８文字以上の場合枠を無視して印刷する/110425
                  MOVE 受－受益者番号助成   TO 受給者番号Ｗ
                  IF 印刷受給者番号２Ｗ = SPACE
016830                MOVE 印刷受給者番号Ｗ TO 受給者番号
                  ELSE
                      MOVE 受給者番号Ｗ     TO 受給者番号２
                  END-IF
              END-IF
042930     END-IF.
      */福島県内のレセまとめ時は給付割合を10割にする/121108
042820     IF ( 助成レセまとめフラグ        = "YES") AND
042830        ( 受－費用負担者番号助成(3:2) = "07" )
               MOVE NC"○" TO １０割チェックＷ
               MOVE SPACE  TO ９割チェックＷ ８割チェックＷ ７割チェックＷ
           END-IF.
042940*
042950*================================================================*
042960 助成料金計算 SECTION.
           EVALUATE 受－保険種別
           WHEN 05
               MOVE 2          TO レセ－レセ種別
           WHEN OTHER
               MOVE 1          TO レセ－レセ種別
           END-EVALUATE.
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
043210*================================================================*
043220 レセ摘要再セット SECTION.
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
043400*
043410*================================================================*
043420 レセプト並び順取得 SECTION.
043430*
043440     MOVE 施術和暦ＷＲ       TO 作２－施術和暦.
043450     MOVE 施術年ＷＲ         TO 作２－施術年.
043460     MOVE 施術月ＷＲ         TO 作２－施術月.
043470     MOVE 患者コードＷＲ     TO 作２－患者コード.
043480     MOVE 助成種別ＷＲ       TO 作２－保険種別.
043490     READ 作業ファイル２
043500     NOT INVALID KEY
043510          MOVE 作２－順番    TO 順番Ｗ
043520     END-READ.
043530*
043540*================================================================*
043550*================================================================*
043560 エラー表示 SECTION.
043570*
043580     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
043590     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
043600     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
043610     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
043620     ACCEPT  キー入力 FROM CONS
043630     PERFORM ファイル閉鎖.
043640     EXIT PROGRAM.
044950*
044951*================================================================*
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
045001*================================================================*
045002*================================================================*
045003 部位名称埋込処理 SECTION.
045004*
006490     STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
009980            負傷名称Ｗ                    DELIMITED BY SPACE
006500            レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
006520       INTO 負傷名Ｗ(部位ＣＮＴ)
006570     END-STRING.
045140*
045150*================================================================*
045160 ファイル閉鎖 SECTION.
045170*
045180     CLOSE 印刷ファイル     保険者マスタ     元号マスタ
045190           名称マスタ       レセプトＦ       制御情報マスタ
045200           施術所情報マスタ 請求先マスタ     経過マスタ
045210           受診者情報Ｆ     施術記録Ｆ       負傷データＦ
045220           負傷原因Ｆ      ＩＤ管理マスタ    市町村マスタ
045230           メモファイル     作業ファイル２
                 委任者情報マスタ 会情報マスタ     受診者情報２Ｆ.
045240*================================================================*
045250 終了処理 SECTION.
045260*
045270     PERFORM ファイル閉鎖.
045280*================================================================*
045290*================================================================*
045300 テスト印字処理 SECTION.
045310*
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
           受理年 受理月 受理日 委任年 委任月 委任日 金属回数 運動回数 運動後療料 助成請求額 受給者負担額
           金属月(1) 金属月(2) 金属月(3) 月(1) 月(2) 月(3) 金属日(1) 金属日(2) 金属日(3)
           運動日(1) 運動日(2) 運動日(3) 運動日(4) 運動日(5)
           .
           MOVE ALL "X" TO
           共済番号 県施術ＩＤ 保険者番号 記号番号 公費負担者番号 受給者番号 住所１ 住所２ 
           口座名義人カナ１ 口座名義人 柔整師番号 口座番号 金融機関名１ 金融機関名２ 金融機関名３ 
           金融機関名４ 支店名１ 支店名２ 支店名３ 支店名４ 施術所郵便番号１ 施術所郵便番号２ 
           施術所住所１ 施術所住所２ 施術所電話番号 代表者カナ 保険者名称 金属副子
           .
           MOVE ALL "Ｎ" TO
           被保険者氏名 患者氏名 接骨院名 代表者名
           負傷原因１ 負傷原因２ 負傷原因３ 負傷原因４ 負傷原因５ 負傷原因６ 負傷原因７ 負傷原因８
           長期理由文１  長期理由文２ 長期理由文３ 長期理由文４ 長期理由文５
           長期理由文６ 長期理由文７ 適用３ 長期頻回
           .
           MOVE ALL NC"Ｎ" TO
           負傷名１ 負傷名２ 負傷名３ 負傷名４ 負傷名５ 経過略称(1) 経過略称(2) 経過略称(3) 
           経過略称(4) 経過略称(5) 適用１ 適用２
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
046640*
      *================================================================*
       施術日取得 SECTION.
      *
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
           IF 助成レセＷ NOT = ZERO
               PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 31
                   MOVE カウンタ TO 施術日(カウンタ)
               END-PERFORM
           END-IF.
037520*================================================================*
       フッタセット SECTION.
      *
           MOVE ZERO  TO 会情－柔整鍼灸区分.
           MOVE 39    TO 会情－協会コード.
           MOVE ZERO  TO 会情－保険種別.
           MOVE ZERO  TO 会情－変更和暦年月.
026480     READ 会情報マスタ.
           MOVE "【お願い】"          TO お願い.
           MOVE "本申請書内容について問合わせ、支給不支給の通知書、又は本申請書返戻等の送付先は下記住所連絡先にお願いします。"
                                      TO 委任情報１.
           MOVE 会情－接骨師会名      TO 委任団体名.
           STRING "〒"                DELIMITED BY SIZE
                  会情－会郵便番号１  DELIMITED BY SIZE
                  "-"                 DELIMITED BY SIZE
                  会情－会郵便番号２  DELIMITED BY SIZE
             INTO 代理人郵便番号
           END-STRING.
           MOVE 会情－会住所１        TO 代理人住所１.
           MOVE "電話"                TO 電話１ＣＭ.
           MOVE 会情－会電話番号      TO 委任電話番号１.
      *
           STRING "["                    DELIMITED BY SIZE
                  費用負担者番号助成ＷＲ DELIMITED BY SIZE
                  "]"                    DELIMITED BY SIZE
             INTO 保険者番号２
           END-STRING.
           MOVE 請求先名称ＷＲ        TO 保険者名称.
           STRING "("                 DELIMITED BY SIZE
                  患者コードＷＲ      DELIMITED BY SIZE
                  ")"                 DELIMITED BY SIZE
             INTO 患者コード
           END-STRING.
      *     MOVE 被保険者氏名Ｗ        TO 被保険者氏名２.
           MOVE 患者氏名Ｗ            TO 被保険者氏名２.
           MOVE 02                    TO 名－区分コード.
           MOVE 助成種別ＷＲ          TO 名－名称コード.
           READ 名称マスタ.
           MOVE 名－略称              TO 保険種別名称ＷＰ.
           STRING 保険種別名称Ｗ      DELIMITED BY "　"
                  ":DNo.="            DELIMITED BY SIZE
             INTO 保険種別
           END-STRING.
022750* レセプト並び順セット *
022760     MOVE 順番Ｗ                 TO 順番.
037520*================================================================*
046660******************************************************************
046670 END PROGRAM YDT6421.
046680******************************************************************
