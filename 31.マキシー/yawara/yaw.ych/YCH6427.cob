000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCH6427.
000060 AUTHOR.                 山田 浩之
000070*
000080*----------------------------------------------------------------*
000090*         中央 助成レセプト印刷（柔+ｳｨﾝﾄﾞｳｽﾞ版）
000100*         MED = YAW610 YCH6427P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2024-10-05
000130 DATE-COMPILED.          2024
      */実日数はレセ－部位実日数を転記する/160816
      */金属副子・運動後療の変更・追加/1805
      */明細書発行加算を適用に追加/2022
      */2022.11より新用紙に切り替え/2022
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
001700* 並び順印字用
001710     SELECT  作業ファイル４  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001720                             ORGANIZATION             IS  INDEXED
001730                             ACCESS                   IS  DYNAMIC
001740                             RECORD      KEY          IS  作４－施術和暦年月
001750                                                          作４－患者コード
001760                                                          作４－保険種別
001770                             FILE        STATUS       IS  状態キー
001780                             LOCK        MODE         IS  AUTOMATIC.
001790*
001800     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF002
001810                             SYMBOLIC    DESTINATION  IS "PRT"
001820                             FORMAT                   IS  定義体名Ｐ
001830                             GROUP                    IS  項目群名Ｐ
001840                             PROCESSING  MODE         IS  処理種別Ｐ
001850                             UNIT        CONTROL      IS  拡張制御Ｐ
001860                             FILE        STATUS       IS  通知情報Ｐ.
001870******************************************************************
001880*                      DATA DIVISION                             *
001890******************************************************************
001900 DATA                    DIVISION.
001910 FILE                    SECTION.
001920*                           ［ＲＬ＝  ３２０］
001930 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001940     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
001950*                           ［ＲＬ＝  ２５６］
001960 FD  市町村マスタ          BLOCK   CONTAINS   1   RECORDS.
001970     COPY SITYOSN        OF  XFDLIB  JOINING   市   AS  PREFIX.
001980*                           ［ＲＬ＝  １２８］
001990 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
002000     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
002010*                           ［ＲＬ＝  １２８］
002020 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
002030     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
002070*                           ［ＲＬ＝  ２５６］
002080 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
002090     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
002100*                           ［ＲＬ＝  １２８］
002110 FD  施術所情報マスタ          BLOCK   CONTAINS   1   RECORDS.
002120     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
002130*                           ［ＲＬ＝  １２８］
002140 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
002150     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
002160*                           ［ＲＬ＝  １２８］
002170 FD  経過マスタ          BLOCK   CONTAINS   1   RECORDS.
002180     COPY KEIKA          OF  XFDLIB  JOINING   経   AS  PREFIX.
002190*                           ［ＲＬ＝  ３２０］
002200 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
002210     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
002560*                          ［ＲＬ＝  1024］
000340 FD  受診者情報２Ｆ        BLOCK   CONTAINS   1   RECORDS.
000350     COPY JUSINJ2          OF  XFDLIB  JOINING   受２   AS  PREFIX.
002220*                           ［ＲＬ＝  ２５６］
002230 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
002240     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
002250*                           ［ＲＬ＝  １２８］
002260 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002270     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
002280*                           ［ＲＬ＝  １２８］
002290 FD  負傷原因Ｆ         BLOCK   CONTAINS   1   RECORDS.
002300     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
002310*                           ［ＲＬ＝  ６４０］
002320 FD  会情報マスタ        BLOCK   CONTAINS   1   RECORDS.
002330     COPY KAIJOHO         OF  XFDLIB  JOINING   会情   AS  PREFIX.
002340*                           ［ＲＬ＝  １２８］
002350 FD  ＩＤ管理マスタ          BLOCK   CONTAINS   1   RECORDS.
002360     COPY IDKANR    OF  XFDLIB  JOINING   ＩＤ管   AS  PREFIX.
002510*                           ［ＲＬ＝  ８３２］
002520 FD  メモファイル        BLOCK CONTAINS 1     RECORDS.
002530     COPY MEMO           OF    XFDLIB JOINING メモ AS PREFIX.
002370**
002380 FD  作業ファイル４ RECORD  CONTAINS 32 CHARACTERS.
002390 01  作４－レコード.
002400     03  作４－レコードキー.
002410         05  作４－施術和暦年月.
002420             07  作４－施術和暦            PIC 9.
002430             07  作４－施術年              PIC 9(2).
002440             07  作４－施術月              PIC 9(2).
002450         05  作４－患者コード.
002460             07 作４－患者番号             PIC 9(6).
002470             07 作４－枝番                 PIC X(1).
002480         05  作４－保険種別                PIC 9(2).
002490     03  作４－レコードデータ.
002500         05  作４－順番                    PIC 9(4).
002510         05  FILLER                        PIC X(14).
002520*
002530 FD  印刷ファイル.
002540     COPY YCH6427P       OF  XMDLIB.
002550*----------------------------------------------------------------*
002560******************************************************************
002570*                WORKING-STORAGE SECTION                         *
002580******************************************************************
002590 WORKING-STORAGE         SECTION.
002600 01 キー入力                           PIC X     VALUE SPACE.
002610 01 状態キー                           PIC X(2)  VALUE SPACE.
002620 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002630 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002820 01 終了フラグ３                       PIC X(3)  VALUE SPACE.
002640 01 初検フラグ                         PIC X(3)  VALUE SPACE.
002650 01 ファイル名                         PIC N(6)  VALUE SPACE.
002660 01 レセプトＰＧＷ                     PIC X(8)  VALUE SPACE.
002670 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
002680 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
002690 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002700 01 患者番号Ｗ                         PIC 9(6)  VALUE ZERO.
002710 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002720 01 部位名称Ｗ                         PIC N(12) VALUE SPACE.
002730 01 部位長Ｗ                           PIC 9(2) VALUE 1.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002870 01 用紙種別Ｗ                         PIC 9(1)  VALUE ZERO.
002740** 数字→日本語変換
002750 01 数字Ｗ                             PIC 9(2).
002760 01 数字Ｒ REDEFINES 数字Ｗ.
002770    03 数字Ｗ１                        PIC X(1).
002780    03 数字Ｗ２                        PIC X(1).
002790*
002800 01 負傷番号Ｗ                         PIC 9.
002810 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
002820    03 負傷番号Ｗ１                    PIC X.
002830*
002840 01 全角負傷番号Ｗ                     PIC N.
002850 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
002860    03 全角負傷番号Ｗ１                PIC X(2).
002870*
002880 01 カウンタ                           PIC 9(2)  VALUE ZERO.
002890 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
002900*
002910 01 都道府県Ｗ                         PIC X(2)  VALUE SPACE.
002920*
002930* 退避用
002940 01 終了年月日ＷＴ.
002950    03 終了年ＷＴ                      PIC 9(2)  VALUE ZERO.
002960    03 終了月ＷＴ                      PIC 9(2)  VALUE ZERO.
002970    03 終了日ＷＴ                      PIC 9(2)  VALUE ZERO.
002980* 初検日退避用
002990 01 初検年月日ＷＴ.
003000    03 初検和暦ＷＴ                    PIC 9     VALUE ZERO.
003010    03 初検年ＷＴ                      PIC 9(2)  VALUE ZERO.
003020    03 初検月ＷＴ                      PIC 9(2)  VALUE ZERO.
003030    03 初検日ＷＴ                      PIC 9(2)  VALUE ZERO.
003040* 初検加算時刻用
003050 01 初検加算ＷＴ.
003060    03 初検加算カウント                PIC 9    VALUE ZERO.
003070    03 番号カウンタ                    PIC 9    VALUE ZERO.
003080    03 初検加算集団ＷＴ  OCCURS 3.
003090       05 初検加算区分ＷＴ             PIC 9    VALUE ZERO.
003100       05 初検加算時ＷＴ               PIC 9(2) VALUE ZERO.
003110       05 初検加算分ＷＴ               PIC 9(2) VALUE ZERO.
003120    03 初検加算集団ＮＷ  OCCURS 3.
003130       05 加算区切Ｗ                   PIC N(1) VALUE SPACE.
003140       05 加算内容Ｗ                   PIC N(3) VALUE SPACE.
003150       05 初検加算時ＮＷ１             PIC N(1) VALUE SPACE.
003160       05 初検加算時ＮＷ２             PIC N(1) VALUE SPACE.
003170       05 時固定Ｗ                     PIC N(1) VALUE SPACE.
003180       05 初検加算分ＮＷ１             PIC N(1) VALUE SPACE.
003190       05 初検加算分ＮＷ２             PIC N(1) VALUE SPACE.
003200       05 分固定Ｗ                     PIC N(1) VALUE SPACE.
003210    03 初検加算時刻１Ｗ                PIC N(10) VALUE SPACE.
003220    03 初検加算時刻２Ｗ                PIC N(10) VALUE SPACE.
003230    03 初検加算時刻３Ｗ                PIC N(10) VALUE SPACE.
003070    03 初検加算区切Ｗ                  PIC X     VALUE SPACE.
003080    03 初検加算時Ｗ                    PIC 9(2)  VALUE ZERO.
003090    03 初検加算分Ｗ                    PIC 9(2)  VALUE ZERO.
003240* 負傷原因用
003250 01 負傷原因ＷＴ.
003260    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
003270    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
003280    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
003290    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
003300    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
003310    03 負傷原因ナンバーＷＴ.
003320       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
003330    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
003340 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
003350 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
003360 01 負傷原因ＴＢＬ.
003370    03 負傷原因コードＴＢＬ            OCCURS 9.
003380       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
003390       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
003400       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
003410 01 負傷原因内容Ｗ.
003420    03 負傷原因内容合成Ｗ              PIC X(318) OCCURS 9 VALUE SPACE.
003430    03 負傷原因内容分解ＸＷ.
003440       05 負傷原因内容１ＸＷ           PIC X(80)  VALUE SPACE.
003450       05 負傷原因内容２ＸＷ           PIC X(80)  VALUE SPACE.
003460       05 負傷原因内容３ＸＷ           PIC X(80)  VALUE SPACE.
003700       05 負傷原因内容４ＸＷ           PIC X(78)  VALUE SPACE.
003470*
003480*
003490* 全角数字抽出用
003500 01 全角文字ＷＴ.
003510     03 混在文字全体Ｗ.
003520        05 混在文字全体１Ｗ            PIC X(2) OCCURS 8 VALUE SPACE.
003530     03 混在文字Ｗ.
003540        05 混在文字１Ｗ                PIC X(2) OCCURS 8 VALUE SPACE.
003550     03 全角数字Ｗ.
003560        05 全角数字１Ｗ                PIC N(1) OCCURS 8 VALUE SPACE.
003570     03 カウンタ３                     PIC 9(2)  VALUE ZERO.
003580* 新柔整師番号抽出用
003590 01 新柔整師番号ＷＴ.
003600     03 新柔整師番号１Ｗ               PIC X(2)  VALUE SPACE.
003610     03 新柔整師番号２Ｗ               PIC X(3)  VALUE SPACE.
003620     03 新柔整師番号３Ｗ.
003630        05 新柔整師番号３１Ｗ          PIC X OCCURS 4 VALUE SPACE.
003640     03 新柔整師番号４Ｗ               PIC X(4)  VALUE SPACE.
003650*
003660* 東京振込先 会長名用(５文字）
003670 01 会長名Ｗ                           PIC N(5)   VALUE SPACE.
003680*
003690** 前月初検のみ用
003700 01 初日再検フラグ                     PIC X(3)  VALUE SPACE.
003710 01 前月フラグ                         PIC X(3)  VALUE SPACE.
003720*
003730 01 計算年月日Ｗ.
003740    03 計算和暦Ｗ                      PIC 9(1)  VALUE ZERO.
003750    03 計算年Ｗ                        PIC S9(2)  VALUE ZERO.
003760    03 計算月Ｗ                        PIC S9(2)  VALUE ZERO.
003770    03 計算日Ｗ                        PIC S9(2)  VALUE ZERO.
003780 01 開始年月日２Ｗ.
003790    03 開始和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003800    03 開始年２Ｗ                      PIC 9(2)  VALUE ZERO.
003810    03 開始月２Ｗ                      PIC 9(2)  VALUE ZERO.
003820    03 開始日２Ｗ                      PIC 9(2)  VALUE ZERO.
003830    03 開始西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003840 01 終了年月日２Ｗ.
003850    03 終了和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003860    03 終了年２Ｗ                      PIC 9(2)  VALUE ZERO.
003870    03 終了月２Ｗ                      PIC 9(2)  VALUE ZERO.
003880    03 終了日２Ｗ                      PIC 9(2)  VALUE ZERO.
003890    03 終了西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003900***
003910** 負傷原因・長期理由印刷区分用
003920 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
003930 01 長期理由印刷区分Ｗ                 PIC 9 VALUE ZERO.
003940*
003950** レセ下段の日付区分用 (0:最終通院日、1:月末日、9:印字なし)
003960 01 レセプト日付区分Ｗ                 PIC 9 VALUE ZERO.
003970 01 レセプト患者日付区分Ｗ             PIC 9 VALUE ZERO.
003980*
003990** 月末日用
004000 01 施術西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
004010 01 商Ｗ                               PIC 9(3)  VALUE ZERO.
004020 01 余Ｗ                               PIC 9(3)  VALUE ZERO.
004030*
004040** 枝番判定用
004050 01 開始診療日手動区分Ｗ               PIC 9    VALUE ZERO.
004060*
004210** レセ摘要用( N(38)固定） /
004220 01 負傷の経過Ｗ.
004230    03 負傷の経過行Ｗ                  PIC X(76) OCCURS 2 VALUE SPACE.
004240 01 負傷の経過ＮＷ REDEFINES 負傷の経過Ｗ.
004250    03 負傷の経過行ＮＷ                PIC N(38) OCCURS 2.
004070*
004080* 負傷原因印刷区分
004090 01 レセ負傷原因印刷区分Ｗ             PIC 9    VALUE ZERO.
004100*
003460 01 長期理由印刷区分Ｆ                 PIC 9 VALUE ZERO.
004101*
004102* 福岡の経過固定印字用に使用
004103 01 全柔ＦＰＤ区分Ｗ                   PIC 9     VALUE ZERO.
004104 01 経過部位数字Ｗ                     PIC N(1)  VALUE SPACE.
      *
      */金属副子・運動後療の変更・追加/1805
       01 金属副子ＣＭ                       PIC X(200) VALUE SPACE.
       01 運動後療ＣＭ                       PIC X(68)  VALUE SPACE.
004105*
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
004106*
004110****************
004120* 連結項目待避 *
004130****************
004140*    ************
004150*    * 印刷キー *
004160*    ************
004170 01 対象データＷＲ.
004180    03 施術和暦年月ＷＲ.
004190       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
004200       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
004210       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
004220    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004230    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
004240    03 公費種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004250    03 費用負担者番号ＷＲ               PIC X(10) VALUE SPACE.
004260    03 助成種別ＷＲ                     PIC 9(2)  VALUE ZERO.
004270    03 費用負担者番号助成ＷＲ           PIC X(10) VALUE SPACE.
004280    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
004290    03 患者カナＷＲ                     PIC X(50) VALUE SPACE.
004300    03 患者コードＷＲ.
004310       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
004320       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
004330*    ************
004340*    * 料金情報 *
004350*    ************
004360*    月毎の料金
004370***********************
004380 01 料金１ＷＲ.
004390   03 初検ＷＲ.
004400      05 負担割合ＷＲ               PIC 9(3)    VALUE ZERO.
004410      05 初検料ＷＲ                 PIC 9(5)    VALUE ZERO.
004420      05 初検加算料ＷＲ             PIC 9(5)    VALUE ZERO.
         03 初検時相談料ＷＲ              PIC 9(4)    VALUE ZERO.
004430   03 再検料ＷＲ                    PIC 9(5)    VALUE ZERO.
004440   03 往療ＷＲ.
004450      05 往療距離ＷＲ               PIC 9(2)V9  VALUE ZERO.
004460      05 往療回数ＷＲ               PIC 9(2)    VALUE ZERO.
004470      05 往療料ＷＲ                 PIC 9(6)    VALUE ZERO.
004480      05 往療加算料ＷＲ             PIC 9(5)    VALUE ZERO.
004490   03 金属副子加算料ＷＲ            PIC 9(5)    VALUE ZERO.
004500   03 施術情報提供料ＷＲ            PIC 9(5)    VALUE ZERO.
004510   03 合計ＷＲ                      PIC 9(6)    VALUE ZERO.
004520   03 一部負担金ＷＲ                PIC 9(6)    VALUE ZERO.
004530   03 請求金額ＷＲ                  PIC 9(6)    VALUE ZERO.
004540   03 給付割合ＷＲ                  PIC 9(1)    VALUE ZERO.
004550   03 受給者負担額ＷＲ              PIC 9(6)    VALUE ZERO.
004560   03 助成請求金額ＷＲ              PIC 9(6)    VALUE ZERO.
004570*
004580* 負傷部位毎の料金
004590***********************
004600 01 料金２ＷＲ.
004610   03 初回処置ＷＲ    OCCURS   9.
004620      05 初回処置料ＷＲ             PIC 9(5)    VALUE ZERO.
004630*
004640* 逓減毎の料金
004650***********************
004660 01 料金３ＷＲ.
004670**********
004680* １部位 *
004690**********
004700   03 部位１ＷＲ.
004710      05 後療１ＷＲ.
004720         07 後療単価１ＷＲ              PIC 9(4)    VALUE ZERO.
004730         07 後療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
004740         07 後療料１ＷＲ                PIC 9(5)    VALUE ZERO.
004750      05 冷罨法１ＷＲ.
004760         07 冷罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
004770         07 冷罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
004780      05 温罨法１ＷＲ.
004790         07 温罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
004800         07 温罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
004810      05 電療１ＷＲ.
004820         07 電療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
004830         07 電療料１ＷＲ                PIC 9(4)    VALUE ZERO.
004840      05 小計１ＷＲ                     PIC 9(6)    VALUE ZERO.
004850      05 長期逓減率１ＷＲ               PIC 9(3)    VALUE ZERO.
004860      05 長期込小計１ＷＲ               PIC 9(6)    VALUE ZERO.
004870**********
004880* ２部位 *
004890**********
004900   03 部位２ＷＲ.
004910      05 後療２ＷＲ.
004920         07 後療単価２ＷＲ              PIC 9(4)    VALUE ZERO.
004930         07 後療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
004940         07 後療料２ＷＲ                PIC 9(5)    VALUE ZERO.
004950      05 冷罨法２ＷＲ.
004960         07 冷罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
004970         07 冷罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
004980      05 温罨法２ＷＲ.
004990         07 温罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
005000         07 温罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
005010      05 電療２ＷＲ.
005020         07 電療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
005030         07 電療料２ＷＲ                PIC 9(4)    VALUE ZERO.
005040      05 小計２ＷＲ                     PIC 9(6)    VALUE ZERO.
005050      05 長期逓減率２ＷＲ               PIC 9(3)    VALUE ZERO.
005060      05 長期込小計２ＷＲ               PIC 9(6)    VALUE ZERO.
005070******************
005080* ３部位／８割 *
005090******************
005100   03 部位３８ＷＲ.
005110      05 後療３８ＷＲ.
005120         07 後療単価３８ＷＲ              PIC 9(4)  VALUE ZERO.
005130         07 後療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
005140         07 後療料３８ＷＲ                PIC 9(5)  VALUE ZERO.
005150      05 冷罨法３８ＷＲ.
005160         07 冷罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
005170         07 冷罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
005180      05 温罨法３８ＷＲ.
005190         07 温罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
005200         07 温罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
005210      05 電療３８ＷＲ.
005220         07 電療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
005230         07 電療料３８ＷＲ                PIC 9(4)  VALUE ZERO.
005240      05 小計３８ＷＲ                     PIC 9(6)  VALUE ZERO.
005250      05 多部位込小計３８ＷＲ             PIC 9(6)  VALUE ZERO.
005260      05 長期逓減率３８ＷＲ               PIC 9(3)  VALUE ZERO.
005270      05 長期込小計３８ＷＲ               PIC 9(6)  VALUE ZERO.
005280******************
005290* ３部位／１０割 *
005300******************
005310   03 部位３０ＷＲ.
005320      05 逓減開始月日３０ＷＲ.
005330         07 逓減開始月３０ＷＲ            PIC 9(2)  VALUE ZERO.
005340         07 逓減開始日３０ＷＲ            PIC 9(2)  VALUE ZERO.
005350      05 後療３０ＷＲ.
005360         07 後療単価３０ＷＲ              PIC 9(4)  VALUE ZERO.
005370         07 後療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
005380         07 後療料３０ＷＲ                PIC 9(5)  VALUE ZERO.
005390      05 冷罨法３０ＷＲ.
005400         07 冷罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
005410         07 冷罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
005420      05 温罨法３０ＷＲ.
005430         07 温罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
005440         07 温罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
005450      05 電療３０ＷＲ.
005460         07 電療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
005470         07 電療料３０ＷＲ                PIC 9(4)  VALUE ZERO.
005480      05 小計３０ＷＲ                     PIC 9(6)  VALUE ZERO.
005490      05 長期逓減率３０ＷＲ               PIC 9(3)  VALUE ZERO.
005500      05 長期込小計３０ＷＲ               PIC 9(6)  VALUE ZERO.
005510****************
005520* ４部位／５割 *
005530****************
005540   03 部位４５ＷＲ.
005550      05 後療４５ＷＲ.
005560         07 後療単価４５ＷＲ              PIC 9(4)  VALUE ZERO.
005570         07 後療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005580         07 後療料４５ＷＲ                PIC 9(5)  VALUE ZERO.
005590      05 冷罨法４５ＷＲ.
005600         07 冷罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005610         07 冷罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
005620      05 温罨法４５ＷＲ.
005630         07 温罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005640         07 温罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
005650      05 電療４５ＷＲ.
005660         07 電療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005670         07 電療料４５ＷＲ                PIC 9(4)  VALUE ZERO.
005680      05 小計４５ＷＲ                     PIC 9(6)  VALUE ZERO.
005690      05 多部位込小計４５ＷＲ             PIC 9(6)  VALUE ZERO.
005700      05 長期逓減率４５ＷＲ               PIC 9(3)  VALUE ZERO.
005710      05 長期込小計４５ＷＲ               PIC 9(6)  VALUE ZERO.
005720****************
005730* ４部位／８割 *
005740****************
005750   03 部位４８ＷＲ.
005760      05 逓減開始月日４８ＷＲ.
005770         07 逓減開始月４８ＷＲ            PIC 9(2)  VALUE ZERO.
005780         07 逓減開始日４８ＷＲ            PIC 9(2)  VALUE ZERO.
005790      05 後療４８ＷＲ.
005800         07 後療単価４８ＷＲ              PIC 9(4)  VALUE ZERO.
005810         07 後療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
005820         07 後療料４８ＷＲ                PIC 9(5)  VALUE ZERO.
005830      05 冷罨法４８ＷＲ.
005840         07 冷罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
005850         07 冷罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
005860      05 温罨法４８ＷＲ.
005870         07 温罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
005880         07 温罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
005890      05 電療４８ＷＲ.
005900         07 電療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
005910         07 電療料４８ＷＲ                PIC 9(4)  VALUE ZERO.
005920      05 小計４８ＷＲ                     PIC 9(6)  VALUE ZERO.
005930      05 多部位込小計４８ＷＲ             PIC 9(6)  VALUE ZERO.
005940      05 長期逓減率４８ＷＲ               PIC 9(3)  VALUE ZERO.
005950      05 長期込小計４８ＷＲ               PIC 9(6)  VALUE ZERO.
005960******************
005970* ４部位／１０割 *
005980******************
005990   03 部位４０ＷＲ.
006000      05 逓減開始月日４０ＷＲ.
006010         07 逓減開始月４０ＷＲ            PIC 9(2)  VALUE ZERO.
006020         07 逓減開始日４０ＷＲ            PIC 9(2)  VALUE ZERO.
006030      05 後療４０ＷＲ.
006040         07 後療単価４０ＷＲ              PIC 9(4)  VALUE ZERO.
006050         07 後療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
006060         07 後療料４０ＷＲ                PIC 9(5)  VALUE ZERO.
006070      05 冷罨法４０ＷＲ.
006080         07 冷罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
006090         07 冷罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
006100      05 温罨法４０ＷＲ.
006110         07 温罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
006120         07 温罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
006130      05 電療４０ＷＲ.
006140         07 電療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
006150         07 電療料４０ＷＲ                PIC 9(4)  VALUE ZERO.
006160      05 小計４０ＷＲ                     PIC 9(6)  VALUE ZERO.
006170      05 長期逓減率４０ＷＲ               PIC 9(3)  VALUE ZERO.
006180      05 長期込小計４０ＷＲ               PIC 9(6)  VALUE ZERO.
006190********************
006200* ５部位／２．５割 *
006210********************
006220   03 部位５２ＷＲ.
006230      05 後療５２ＷＲ.
006240         07 後療単価５２ＷＲ              PIC 9(4)  VALUE ZERO.
006250         07 後療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
006260         07 後療料５２ＷＲ                PIC 9(5)  VALUE ZERO.
006270      05 冷罨法５２ＷＲ.
006280         07 冷罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
006290         07 冷罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
006300      05 温罨法５２ＷＲ.
006310         07 温罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
006320         07 温罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
006330      05 電療５２ＷＲ.
006340         07 電療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
006350         07 電療料５２ＷＲ                PIC 9(4)  VALUE ZERO.
006360      05 小計５２ＷＲ                     PIC 9(6)  VALUE ZERO.
006370      05 多部位込小計５２ＷＲ             PIC 9(6)  VALUE ZERO.
006380      05 長期逓減率５２ＷＲ               PIC 9(3)  VALUE ZERO.
006390      05 長期込小計５２ＷＲ               PIC 9(6)  VALUE ZERO.
006400****************
006410* ５部位／５割 *
006420****************
006430   03 部位５５ＷＲ.
006440      05 逓減開始月日５５ＷＲ.
006450         07 逓減開始月５５ＷＲ            PIC 9(2)  VALUE ZERO.
006460         07 逓減開始日５５ＷＲ            PIC 9(2)  VALUE ZERO.
006470      05 後療５５ＷＲ.
006480         07 後療単価５５ＷＲ              PIC 9(4)  VALUE ZERO.
006490         07 後療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
006500         07 後療料５５ＷＲ                PIC 9(5)  VALUE ZERO.
006510      05 冷罨法５５ＷＲ.
006520         07 冷罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
006530         07 冷罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
006540      05 温罨法５５ＷＲ.
006550         07 温罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
006560         07 温罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
006570      05 電療５５ＷＲ.
006580         07 電療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
006590         07 電療料５５ＷＲ                PIC 9(4)  VALUE ZERO.
006600      05 小計５５ＷＲ                     PIC 9(6)  VALUE ZERO.
006610      05 多部位込小計５５ＷＲ             PIC 9(6)  VALUE ZERO.
006620      05 長期逓減率５５ＷＲ               PIC 9(3)  VALUE ZERO.
006630      05 長期込小計５５ＷＲ               PIC 9(6)  VALUE ZERO.
006640****************
006650* ５部位／８割 *
006660****************
006670   03 部位５８ＷＲ.
006680      05 逓減開始月日５８ＷＲ.
006690         07 逓減開始月５８ＷＲ            PIC 9(2)  VALUE ZERO.
006700         07 逓減開始日５８ＷＲ            PIC 9(2)  VALUE ZERO.
006710      05 後療５８ＷＲ.
006720         07 後療単価５８ＷＲ              PIC 9(4)  VALUE ZERO.
006730         07 後療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
006740         07 後療料５８ＷＲ                PIC 9(5)  VALUE ZERO.
006750      05 冷罨法５８ＷＲ.
006760         07 冷罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
006770         07 冷罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
006780      05 温罨法５８ＷＲ.
006790         07 温罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
006800         07 温罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
006810      05 電療５８ＷＲ.
006820         07 電療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
006830         07 電療料５８ＷＲ                PIC 9(4)  VALUE ZERO.
006840      05 小計５８ＷＲ                     PIC 9(6)  VALUE ZERO.
006850      05 多部位込小計５８ＷＲ             PIC 9(6)  VALUE ZERO.
006860      05 長期逓減率５８ＷＲ               PIC 9(3)  VALUE ZERO.
006870      05 長期込小計５８ＷＲ               PIC 9(6)  VALUE ZERO.
006880******************
006890* ５部位／１０割 *
006900******************
006910   03 部位５０ＷＲ.
006920      05 逓減開始月日５０ＷＲ.
006930         07 逓減開始月５０ＷＲ            PIC 9(2)  VALUE ZERO.
006940         07 逓減開始日５０ＷＲ            PIC 9(2)  VALUE ZERO.
006950      05 後療５０ＷＲ.
006960         07 後療単価５０ＷＲ              PIC 9(4)  VALUE ZERO.
006970         07 後療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
006980         07 後療料５０ＷＲ                PIC 9(5)  VALUE ZERO.
006990      05 冷罨法５０ＷＲ.
007000         07 冷罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
007010         07 冷罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
007020      05 温罨法５０ＷＲ.
007030         07 温罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
007040         07 温罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
007050      05 電療５０ＷＲ.
007060         07 電療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
007070         07 電療料５０ＷＲ                PIC 9(4)  VALUE ZERO.
007080      05 小計５０ＷＲ                     PIC 9(6)  VALUE ZERO.
007090      05 長期逓減率５０ＷＲ               PIC 9(3)  VALUE ZERO.
007100      05 長期込小計５０ＷＲ               PIC 9(6)  VALUE ZERO.
008000*******************
008010*  明細書発行加算 */202206
008020*******************
008030   03 明細書発行加算料ＷＲ                PIC ZZZ   VALUE ZERO.
008030   03 明細書発行加算日ＷＲ                PIC ZZ    VALUE ZERO.
007110*
007120**************
007130* 施術所情報 *
007140**************
007150 01 施術所情報Ｗ.
007160    03 柔整師番号Ｗ                    PIC X(16)  VALUE SPACE.
007170*    03 柔整師番号１Ｗ                  PIC X(6)   VALUE SPACE.
007180*    03 柔整師番号２Ｗ                  PIC N(4)   VALUE SPACE.
007190*    03 柔整師番号３Ｗ                  PIC X(4)   VALUE SPACE.
007200    03 接骨師会会員番号Ｗ              PIC X(16)  VALUE SPACE.
007210    03 代表者カナＷ                    PIC X(50)  VALUE SPACE.
007220    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
007230    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
          03 都道府県ＪＩＳＷ                PIC X(2)   VALUE SPACE.
007240    03 施術所住所Ｗ.
007250       05 施術所住所１Ｗ               PIC X(50)  VALUE SPACE.
007260       05 施術所住所２Ｗ               PIC X(50)  VALUE SPACE.
007270    03 施術所郵便番号Ｗ.
007280       05 施術所郵便番号１Ｗ           PIC X(3)   VALUE SPACE.
007290       05 施術所郵便番号２Ｗ           PIC X(4)   VALUE SPACE.
007300    03 施術所電話番号Ｗ                PIC X(20)  VALUE SPACE.
007310    03 定額制受理番号Ｗ                PIC X(15)  VALUE SPACE.
007320    03 受理年月日Ｗ.
007330       05 受理年Ｗ                     PIC 9(2)   VALUE ZERO.
007340       05 受理月Ｗ                     PIC 9(2)   VALUE ZERO.
007350       05 受理日Ｗ                     PIC 9(2)   VALUE ZERO.
007360    03 最終通院年月日Ｗ.
007370       05 最終通院年Ｗ                 PIC 9(2)   VALUE ZERO.
007380       05 最終通院月Ｗ                 PIC 9(2)   VALUE ZERO.
007390       05 最終通院日Ｗ                 PIC 9(2)   VALUE ZERO.
007400    03 柔整師年月日Ｗ.
007410       05 柔整師年Ｗ                   PIC 9(2)   VALUE ZERO.
007420       05 柔整師月Ｗ                   PIC 9(2)   VALUE ZERO.
007430       05 柔整師日Ｗ                   PIC 9(2)   VALUE ZERO.
007440    03 患者委任年月日Ｗ.
007450       05 患者委任年Ｗ                 PIC 9(2)   VALUE ZERO.
007460       05 患者委任月Ｗ                 PIC 9(2)   VALUE ZERO.
007470       05 患者委任日Ｗ                 PIC 9(2)   VALUE ZERO.
007480    03 取引先情報Ｗ.
007490        05 取引先銀行名Ｗ              PIC X(40)  VALUE SPACE.
007500        05 取引先銀行支店名Ｗ          PIC X(40)  VALUE SPACE.
007510        05 預金種別Ｗ                  PIC 9(1)   VALUE ZERO.
007520        05 口座番号Ｗ                  PIC X(10)  VALUE SPACE.
007530        05 口座名義人Ｗ                PIC X(40)  VALUE SPACE.
007540        05 口座名義人カナＷ            PIC X(40)  VALUE SPACE.
007550* 助成レセ
007560        05 口座名義人とカナＷ.
007570           07 口座名義人とカナ１Ｗ     PIC X(38)  VALUE SPACE.
007580           07 口座名義人とカナ２Ｗ     PIC X(30)  VALUE SPACE.
007590*
007600        05 銀行名支店名Ｗ              PIC X(60)  VALUE SPACE.
007610        05 預金種別コメントＷ          PIC N(3)   VALUE SPACE.
007620        05 預金種別コメントＸＷ        PIC X(4)   VALUE SPACE.
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
007630    03 コメントＷ.
007640        05 コメント１Ｗ                PIC X(54)  VALUE SPACE.
007650        05 コメント２Ｗ                PIC X(54)  VALUE SPACE.
007660        05 コメント３Ｗ                PIC X(54)  VALUE SPACE.
007670        05 コメント４Ｗ                PIC X(54)  VALUE SPACE.
007680        05 コメント５Ｗ                PIC N(27)  VALUE SPACE.
007690    03 県施術ＩＤＷ                    PIC X(15)  VALUE SPACE.
007700    03 市町村施術ＩＤＷ                PIC X(15)  VALUE SPACE.
007710**************
007720* 受診者情報 *
007730**************
007740 01 受診者情報Ｗ.
      */元号修正/20190408
          03 施術和暦Ｗ                      PIC 9(1)   VALUE ZERO.
007750    03 施術年月Ｗ.
007760       05 施術年Ｗ                     PIC 9(2)   VALUE ZERO.
007770       05 施術月Ｗ                     PIC 9(2)   VALUE ZERO.
007780*    03 記号Ｗ                          PIC N(12)  VALUE SPACE.
007570    03 記号Ｗ.
007580       05 印刷記号Ｗ                   PIC N(12)  VALUE SPACE.
          03 記号番号Ｗ.
             05 記号番号ＸＷ                 PIC X(40) VALUE SPACE.
007790    03 番号Ｗ.
007800       05 印刷番号Ｗ                   PIC X(15)  VALUE SPACE.
007810       05 FILLER                       PIC X(15)  VALUE SPACE.
007820    03 保険者番号Ｗ.
007830       05 印刷保険者番号Ｗ             PIC X(8)   VALUE SPACE.
007840       05 FILLER                       PIC X(2)   VALUE SPACE.
007850    03 市町村番号Ｗ.
007860       05 印刷市町村番号Ｗ             PIC X(8)   VALUE SPACE.
007870       05 FILLER                       PIC X(2).
007880    03 請求先名称Ｗ.
007890       05 印刷請求先名称１Ｗ           PIC X(40)  VALUE SPACE.
007900       05 印刷請求先名称２Ｗ           PIC X(40)  VALUE SPACE.
007910    03 受給者番号Ｗ.
007920       05 印刷受給者番号Ｗ             PIC X(15)  VALUE SPACE.
007930*       05 FILLER                       PIC X(13).
007940**    03 助成市町村番号Ｗ                PIC X(8)   VALUE SPACE.
007950    03 保険種別Ｗ                      PIC 9(2)   VALUE ZERO.
007960    03 被保険者情報Ｗ.
007970       05 被保険者カナＷ               PIC X(50)  VALUE SPACE.
007980       05 被保険者氏名Ｗ               PIC X(50)  VALUE SPACE.
007990       05 郵便番号Ｗ.
008000          07 郵便番号１Ｗ              PIC X(3)   VALUE SPACE.
008010          07 郵便番号２Ｗ              PIC X(4)   VALUE SPACE.
008020       05 被保険者住所Ｗ               PIC X(80)  VALUE SPACE.
008030       05 被保険者住所１Ｗ             PIC X(50)  VALUE SPACE.
008040       05 被保険者住所２Ｗ             PIC X(50)  VALUE SPACE.
008990       05 電話番号Ｗ                   PIC X(35)  VALUE SPACE.
008050    03 患者情報Ｗ.
008060       05 患者住所Ｗ                   PIC X(80)  VALUE SPACE.
008070       05 患者住所１Ｗ                 PIC X(50)  VALUE SPACE.
008080       05 患者住所２Ｗ                 PIC X(50)  VALUE SPACE.
008090       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
008100       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
008110       05 性別チェックＷ.
008120          07 男チェックＷ              PIC N(1)  VALUE SPACE.
008130          07 女チェックＷ              PIC N(1)  VALUE SPACE.
008140       05 患者性別Ｗ.
008150          07 性別Ｗ                    PIC N(1)  VALUE SPACE.
008160       05 和暦チェックＷ.
008170          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
008180          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
008190          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
008200          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
008210          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
      */元号修正/↓↓↓20190408
008210          07 令和チェックＷ            PIC N(1)  VALUE SPACE.
                07 令和ＣＭＷ                PIC X(4)  VALUE SPACE.
009110*          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
      */元号修正/↑↑↑20190408
008220       05 患者年Ｗ                     PIC 9(2)  VALUE ZERO.
008230       05 患者月Ｗ                     PIC 9(2)  VALUE ZERO.
008240       05 患者日Ｗ                     PIC 9(2)  VALUE ZERO.
008250       05 続柄Ｗ.
008260          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
008270          07 FILLER                    PIC X(4)  VALUE SPACE.
008280       05 負傷原因１Ｗ                 PIC N(37) VALUE SPACE.
008290       05 負傷原因２Ｗ                 PIC N(37) VALUE SPACE.
008300       05 負傷原因３Ｗ                 PIC N(37) VALUE SPACE.
008310       05 負傷原因４Ｗ                 PIC N(37) VALUE SPACE.
008320       05 負傷原因５Ｗ                 PIC N(37) VALUE SPACE.
008330*
008370*       05 負傷原因Ｗ                   PIC N(40) OCCURS 29 VALUE SPACE.
      */半角対応/110421
             05 負傷原因Ｗ OCCURS 29.
                07 負傷原因ＸＷ              PIC X(80)  VALUE SPACE.
008350*
008360    03 助成印Ｗ                        PIC N(1)  VALUE SPACE.
008370*    03 種別チェックＷ.
008380*       05 政チェックＷ                 PIC N(1)  VALUE SPACE.
008390*       05 日チェックＷ                 PIC N(1)  VALUE SPACE.
008400*       05 船チェックＷ                 PIC N(1)  VALUE SPACE.
008410*       05 組チェックＷ                 PIC N(1)  VALUE SPACE.
008420*       05 共チェックＷ                 PIC N(1)  VALUE SPACE.
008430*       05 国チェックＷ                 PIC N(1)  VALUE SPACE.
008440*       05 退チェックＷ                 PIC N(1)  VALUE SPACE.
008440*       05 後高チェックＷ               PIC N(1)  VALUE SPACE.
008440*       05 後高１Ｗ                     PIC N(1)  VALUE SPACE.
008450    03 特別マークＷ                    PIC N(1)  VALUE SPACE.
008460    03 特別コメントＷ                  PIC X(16) VALUE SPACE.
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
008470*
008480****************
008490* 負傷データＦ *
008500****************
008510 01 負傷情報Ｗ.
008520    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
008530    03 部位情報Ｗ  OCCURS   9.
008540       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
008550       05 部位コードＷ.
008560          07 負傷種別Ｗ                PIC 9(2)  VALUE ZERO.
008570          07 部位Ｗ                    PIC 9(2)  VALUE ZERO.
008580          07 左右区分Ｗ                PIC 9(1)  VALUE ZERO.
008590          07 負傷位置番号Ｗ            PIC 9(2)  VALUE ZERO.
008600       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
008610       05 負傷年月日Ｗ.
008620          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
008630          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
008640          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
008650       05 初検年月日Ｗ.
008660          07 初検年Ｗ                  PIC 9(2)  VALUE ZERO.
008670          07 初検月Ｗ                  PIC 9(2)  VALUE ZERO.
008680          07 初検日Ｗ                  PIC 9(2)  VALUE ZERO.
008690       05 開始年月日Ｗ.
008700          07 開始年Ｗ                  PIC 9(2)  VALUE ZERO.
008710          07 開始月Ｗ                  PIC 9(2)  VALUE ZERO.
008720          07 開始日Ｗ                  PIC 9(2)  VALUE ZERO.
008730       05 終了年月日Ｗ.
008740          07 終了年Ｗ                  PIC 9(2)  VALUE ZERO.
008750          07 終了月Ｗ                  PIC 9(2)  VALUE ZERO.
008760          07 終了日Ｗ                  PIC 9(2)  VALUE ZERO.
008770       05 実日数Ｗ                     PIC 9(2)  VALUE ZERO.
             05 部位継続月数Ｗ               PIC 9(3)  VALUE ZERO.
008780       05 転帰区分Ｗ                   PIC 9(1)  VALUE ZERO.
008790       05 転帰区分チェックＷ.
008800          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
008810          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
008820          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
008830       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
008840       05 部位区切Ｗ                   PIC X(1)  VALUE SPACE.
008850       05 経過略称Ｗ.
008860          07 印刷経過略称Ｗ            PIC N(5)  VALUE SPACE.
008870          07 FILLER                    PIC X(2)  VALUE SPACE.
008880    03 経過部位Ｗ                      PIC N(1)  VALUE SPACE.
009030    03 経過ＣＭ                        PIC N(4)  VALUE SPACE.
008890    03 新規チェックＷ                  PIC N(1)  VALUE SPACE.
008900    03 継続チェックＷ                  PIC N(1)  VALUE SPACE.
          03 施術日Ｗ.
             05 施術日チェックＷ   OCCURS 31 PIC N(1)  VALUE SPACE.
008910****************
008920* ＯＣＲコード *
008930****************
008940* 01 ＯＣＲコードＷ.
008950*    03 ＯＣＲ会員番号Ｗ                PIC X(6)    VALUE ZERO.
008960*    03 ＯＣＲ施術年月Ｗ                PIC 9(4)    VALUE ZERO.
008970*    03 ＯＣＲ保険種別Ｗ                PIC 9(2)    VALUE ZERO.
008980*    03 ＯＣＲ各社ＩＤＷ                PIC 9(2)    VALUE ZERO.
008990*    03 ＯＣＲＦＤ連番Ｗ.
009000*       05 ＦＤ連番患者番号Ｗ           PIC 9(6)    VALUE ZERO.
009010*       05 ＦＤ連番健保ＩＤＷ           PIC 9(1)    VALUE ZERO.
009020*    03 ＯＣＲ請求金額Ｗ                PIC 9(6)    VALUE ZERO.
009030*    03 ＯＣＲ負担割合Ｗ                PIC 9(1)    VALUE ZERO.
009040*    03 ＯＣＲ接骨師会会員番号Ｗ        PIC 9(10)   VALUE ZERO.
009050*
009060********************
009070* 給付割合チェック *
009080********************
009090* 01 給付割合情報Ｗ.
009100*    03 給付７割チェックＷ                PIC N(1)  VALUE SPACE.
009110*    03 給付８割チェックＷ                PIC N(1)  VALUE SPACE.
009120*    03 給付９割チェックＷ                PIC N(1)  VALUE SPACE.
009130*    03 老人給付チェックＷ                PIC N(1)  VALUE SPACE.
009130*    03 後高給付チェックＷ                PIC N(1)  VALUE SPACE.
009130*    03 後高２Ｗ                          PIC N(1)  VALUE SPACE.
009140************
009150* 料金情報 *
009160************
009170 01 料金情報Ｗ.
009180    03 初検加算Ｗ.
009190       05 時間外チェックＷ                PIC N(1) VALUE SPACE.
009200       05 休日チェックＷ                  PIC N(1) VALUE SPACE.
009210       05 深夜チェックＷ                  PIC N(1) VALUE SPACE.
009220    03 往療加算Ｗ.
009230       05 夜間チェックＷ                  PIC N(1) VALUE SPACE.
009240       05 暴風雨雪チェックＷ              PIC N(1) VALUE SPACE.
009250    03 金属副子チェックＷ.
009260       05 大チェックＷ                    PIC N(1) VALUE SPACE.
009270       05 中チェックＷ                    PIC N(1) VALUE SPACE.
009280       05 小チェックＷ                    PIC N(1) VALUE SPACE.
009290    03 小計Ｗ                             PIC 9(7) VALUE ZERO.
009300    03 初回処置料合計Ｗ                   PIC 9(6) VALUE ZERO.
009310    03 初回処置料チェックＷ.
009320       05 整復料チェックＷ                PIC N(1) VALUE SPACE.
009330       05 固定料チェックＷ                PIC N(1) VALUE SPACE.
009340       05 施療料チェックＷ                PIC N(1) VALUE SPACE.
      */金属副子・運動後療の変更・追加/1805
          03 金属回数Ｗ                         PIC 9(2)  VALUE ZERO.
          03 運動回数Ｗ                         PIC 9(2)  VALUE ZERO.
          03 運動料Ｗ                           PIC 9(4)  VALUE ZERO.
009350************
009360* 備考情報 * 
009370************
009380 01 備考情報Ｗ.
009390    03 受給者負担額内訳Ｗ      OCCURS  10.
009400       05 印刷負担回数Ｗ               PIC N(1)  VALUE SPACE.
009410       05 当日分負担額Ｗ               PIC 9(4)  VALUE ZERO.
009420    03 適用１Ｗ                        PIC N(34) VALUE SPACE.
009430    03 適用２Ｗ                        PIC N(34) VALUE SPACE.
008830    03 適用３Ｗ                        PIC X(40) VALUE SPACE.
009440    03 経過コメントＷ                  PIC N(60) VALUE SPACE.
009450*
009460*****************
009470* レセプト並び順 *
009480*****************
009490 01 順番固定Ｗ                         PIC N(1) VALUE SPACE.
009500 01 順番Ｗ                             PIC 9(4) VALUE ZERO.
      *
       01 摘要施術日Ｗ                       PIC X(100) VALUE SPACE.
       01 施術日Ｗ.
          03 施術日２Ｗ                      PIC X(1)  VALUE SPACE.
          03 施術日１Ｗ                      PIC X(1)  VALUE SPACE.
002790** 負担割合用
002800 01 負担割合Ｗ                         PIC 9(2)  VALUE ZERO.
002810 01 給付割合Ｗ                         PIC 9(2)  VALUE ZERO.
009510*******************************************************************
009520 01 印刷制御.
009530     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
009540     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
009550     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
009560     03 拡張制御Ｐ.
009570         05 端末制御Ｐ.
009580             07 移動方向Ｐ             PIC X(1) VALUE SPACE.
009590             07 移動行数Ｐ             PIC 9(3) VALUE ZERO.
009600         05 詳細制御Ｐ                 PIC X(2) VALUE SPACE.
009610     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
009620     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
009630*
009640 01 計算機西暦年Ｗ                     PIC 9(2) VALUE ZERO.
009650* 日付ＷＯＲＫ
009660 01 和暦終了年Ｗ                       PIC 9(4) VALUE ZERO.
009670 01 計算機西暦.
009680    03 計算機西暦年                    PIC 9(4) VALUE ZERO.
009690    03 計算機西暦月日                  PIC 9(4) VALUE ZERO.
009700 01 計算機西暦Ｒ REDEFINES 計算機西暦.
009710    03 計算機世紀                      PIC 9(2).
009720    03 計算機日付                      PIC 9(6).
009730    03 計算機日付Ｒ REDEFINES 計算機日付.
009740       05 計算機年月                   PIC 9(4).
009750       05 計算機年月Ｒ REDEFINES 計算機年月.
009760         07 計算機年                   PIC 9(2).
009770         07 計算機月                   PIC 9(2).
009780       05 計算機日                     PIC 9(2).
009790*
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
009800******************************************************************
009810*                          連結項目                              *
009820******************************************************************
009830**  画面入力データ
009840 01 連入－入力データ委任印刷 IS EXTERNAL.
009850    03 連入－委任印刷                     PIC 9.
014620*
       01 連入－入力データ電話印刷 IS EXTERNAL.
          03 連入－電話印刷                     PIC 9.
009190*
       01 連入－プレビュー IS EXTERNAL.
          03 連入－プレビュー区分          PIC 9.
009860*
009870** ３カ月長期判定
009880 01 連期間－キー IS EXTERNAL.
009890    03 連期間－施術年月.
009900       05 連期間－施術和暦               PIC 9.
009910       05 連期間－施術年                 PIC 9(2).
009920       05 連期間－施術月                 PIC 9(2).
009930    03  連期間－患者コード.
009940       05 連期間－患者番号               PIC 9(6).
009950       05 連期間－枝番                   PIC X.
009960    03 連期間－対象フラグ                PIC X(3).
009970    03 連期間－期間月Ｗ.
009980       05 連期間－期間Ｗ                 PIC 9(2) OCCURS 9.
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
009990************
010000* 印刷キー *
010010************
010040 01 連レ印－対象データ IS EXTERNAL.
010050    03 連レ印－施術年月日.
010060       05 連レ印－施術和暦                  PIC 9(1).
010070       05 連レ印－施術年                    PIC 9(2).
010080       05 連レ印－施術月                    PIC 9(2).
010090    03 連レ印－患者コード.
010100       05 連レ印－患者番号                  PIC 9(6).
010110       05 連レ印－枝番                      PIC X(1).
010120    03 連レ印－保険種別                     PIC 9(2).
010130    03 連レ印－保険者番号                   PIC X(10).
010140    03 連レ印－公費種別                     PIC 9(2).
010150    03 連レ印－費用負担者番号               PIC X(10).
010160    03 連レ印－助成種別                     PIC 9(2).
010170    03 連レ印－費用負担者番号助成           PIC X(10).
010180    03 連レ印－患者カナ                     PIC X(20).
010190    03 連レ印－本人家族区分                 PIC 9(1).
010200*
013630 01 連レ－キー IS EXTERNAL.
013640    03 連レ－保険種別                  PIC 9(2).
013650*
013660************************
013670* 長期理由文セット     *
013680************************
013690 01 連長文－キー IS EXTERNAL.
013700    03 連長文－施術年月.
013710       05 連長文－施術和暦               PIC 9.
013720       05 連長文－施術年                 PIC 9(2).
013730       05 連長文－施術月                 PIC 9(2).
013740    03  連長文－患者コード.
013750       05 連長文－患者番号               PIC 9(6).
013760       05 連長文－枝番                   PIC X.
013770    03 連長文－文桁数                    PIC 9(2).
013780    03 連長文－理由文                    PIC N(63) OCCURS 15.
013790*
013792*************
013793* 助成名称
013794*************
013795 01 連助成名称－キー IS EXTERNAL.
013796    03 連助成名称－助成種別             PIC 9(2).
013797    03 連助成名称－費用負担者番号助成   PIC X(10).
013798*   / OUT /
013799    03 連助成名称－名称集団.
013800       05 連助成名称－１文字            PIC N.
013801       05 連助成名称－略称              PIC N(4).
013802       05 連助成名称－正式名称          PIC N(10).
013803*
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
013804*
013805******************************************************************
013810*                      PROCEDURE  DIVISION                       *
013820******************************************************************
013830 PROCEDURE               DIVISION.
013840************
013850*           *
013860* 初期処理   *
013870*           *
013880************
002570     PERFORM プリンタファイル作成.
013890     PERFORM 初期化.
013900************
013910*           *
013920* 主処理     *
013930*           *
013940************
013950* 印刷
013960     PERFORM 連結項目待避.
013970     PERFORM 印刷セット.
013980     PERFORM 印刷処理.
013990************
014000*           *
014010* 終了処理   *
014020*           *
014030************
014040     PERFORM 受診者印刷区分更新.
014050     PERFORM 終了処理.
014060     MOVE ZERO  TO PROGRAM-STATUS.
014070     EXIT PROGRAM.
014080*
014090*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YCH6427"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分  TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
014100*================================================================*
014110 初期化 SECTION.
014120*
014130     PERFORM ファイルオープン.
014140*    /* 現在日付取得 */
014150     ACCEPT 計算機日付 FROM DATE.
014160*    /* 1980～2079年の間で設定 */
014170     IF ( 計算機年 > 80 )
014180         MOVE 19 TO 計算機世紀
014190     ELSE
014200         MOVE 20 TO 計算機世紀
014210     END-IF.
014220     PERFORM カレント元号取得.
014230     PERFORM 和暦終了年取得.
014240     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 和暦終了年Ｗ.
014250*================================================================*
014260 カレント元号取得 SECTION.
014270*
014280     MOVE ZEROS TO 制－制御区分.
014290     READ 制御情報マスタ
014300     NOT INVALID KEY
014310         MOVE 制－カレント元号         TO カレント元号Ｗ
014320         MOVE 制－レセ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
014330         MOVE 制－レセ長期理由印刷区分 TO 長期理由印刷区分Ｗ
014340         MOVE 制－レセプト日付区分     TO レセプト日付区分Ｗ
014350         MOVE 制－レセプト患者日付区分 TO レセプト患者日付区分Ｗ
014351         MOVE 制－全柔ＦＰＤ区分       TO 全柔ＦＰＤ区分Ｗ
015320         MOVE 制－助成レセ             TO 用紙種別Ｗ
014360     END-READ.
014370*
014380*================================================================*
014390 和暦終了年取得 SECTION.
014400*
014410*     DISPLAY NC"カレント元号Ｗ"  カレント元号Ｗ UPON MSGBOX.
014420     MOVE カレント元号Ｗ TO 元－元号区分.
014430     READ 元号マスタ
014440     INVALID KEY
014450         DISPLAY NC"指定和暦が登録されていません" UPON CONS
014460         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
014470                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
014480         ACCEPT  キー入力 FROM CONS
014490         PERFORM 終了処理
014500         EXIT PROGRAM
014510     NOT INVALID KEY
014520         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
014530         MOVE 前和暦Ｗ TO 元－元号区分
014540         READ 元号マスタ
014550         INVALID KEY
014560             DISPLAY NC"指定和暦が登録されていません" UPON CONS
014570             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
014580                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
014590             ACCEPT  キー入力 FROM CONS
014600             PERFORM 終了処理
014610             EXIT PROGRAM
014620         NOT INVALID KEY
014630             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
014640         END-READ
014650     END-READ.
014660*
014670*================================================================*
014680 ファイルオープン SECTION.
014690*
014700     OPEN INPUT   保険者マスタ
014710         MOVE NC"保険者" TO ファイル名.
014720         PERFORM オープンチェック.
014730     OPEN INPUT   市町村マスタ
014740         MOVE NC"市町村" TO ファイル名.
014750         PERFORM オープンチェック.
014760     OPEN INPUT   元号マスタ
014770         MOVE NC"元号" TO ファイル名.
014780         PERFORM オープンチェック.
014790     OPEN INPUT   名称マスタ
014800         MOVE NC"名称" TO ファイル名.
014810         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
014850     OPEN INPUT   制御情報マスタ
014860         MOVE NC"制御情報" TO ファイル名.
014870         PERFORM オープンチェック.
014880     OPEN INPUT   施術所情報マスタ
014890         MOVE NC"施情" TO ファイル名.
014900         PERFORM オープンチェック.
014910     OPEN INPUT   請求先マスタ
014920         MOVE NC"請先" TO ファイル名.
014930         PERFORM オープンチェック.
014940     OPEN INPUT   経過マスタ
014950         MOVE NC"経過" TO ファイル名.
014960         PERFORM オープンチェック.
014970     OPEN INPUT   施術記録Ｆ.
014980         MOVE NC"施記Ｆ" TO ファイル名.
014990         PERFORM オープンチェック.
015000     OPEN INPUT   負傷データＦ.
015010         MOVE NC"負傷" TO ファイル名.
015020         PERFORM オープンチェック.
015030     OPEN INPUT   負傷原因Ｆ.
015040         MOVE NC"負傷原因" TO ファイル名.
015050         PERFORM オープンチェック.
015060     OPEN INPUT   会情報マスタ.
015070         MOVE NC"会情報マスタ" TO ファイル名.
015080         PERFORM オープンチェック.
015090     OPEN INPUT   ＩＤ管理マスタ
015100         MOVE NC"ＩＤ" TO ファイル名.
015110         PERFORM オープンチェック.
015030     OPEN INPUT メモファイル.
015040         MOVE NC"メモ" TO ファイル名.
015050         PERFORM オープンチェック.
015560     OPEN INPUT   受診者情報２Ｆ.
015570         MOVE NC"受診者情報２Ｆ" TO ファイル名.
015580         PERFORM オープンチェック.
015120     OPEN INPUT   作業ファイル４.
015170         IF ( 状態キー  NOT =  "00" )
015060            OPEN OUTPUT  作業ファイル４
                  CLOSE 作業ファイル４
015060            OPEN INPUT  作業ファイル４
               END-IF.
015150     OPEN I-O   受診者情報Ｆ.
015160         MOVE NC"受情" TO ファイル名.
015170         PERFORM オープンチェック.
015180     OPEN I-O   印刷ファイル
015190         PERFORM エラー処理Ｐ.
015200*================================================================*
015210 オープンチェック SECTION.
015220*
015230     IF ( 状態キー  NOT =  "00" )
015240         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
015250         DISPLAY NC"状態キー：" 状態キー         UPON CONS
015260         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
015270                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
015280         ACCEPT  キー入力 FROM CONS
015290         PERFORM ファイル閉鎖
015300         EXIT PROGRAM.
015310*================================================================*
015320 連結項目待避 SECTION.
015330*
015340     MOVE 連レ印－施術和暦           TO 施術和暦ＷＲ.
015350     MOVE 連レ印－施術年             TO 施術年ＷＲ.
015360     MOVE 連レ印－施術月             TO 施術月ＷＲ.
015370     MOVE 連レ印－保険種別           TO 保険種別ＷＲ.
015380     MOVE 連レ印－保険者番号         TO 保険者番号ＷＲ.
015390     MOVE 連レ印－公費種別           TO 公費種別ＷＲ.
015400     MOVE 連レ印－費用負担者番号     TO 費用負担者番号ＷＲ.
015410     MOVE 連レ印－助成種別           TO 助成種別ＷＲ.
015420     MOVE 連レ印－費用負担者番号助成 TO 費用負担者番号助成ＷＲ.
015430     MOVE 連レ印－本人家族区分       TO 本人家族区分ＷＲ.
015440     MOVE 連レ印－患者カナ           TO 患者カナＷＲ.
015450     MOVE 連レ印－患者番号           TO 患者番号ＷＲ.
015460     MOVE 連レ印－枝番               TO 枝番ＷＲ.
015470*================================================================*
015480 印刷セット SECTION.
015490*
015500     PERFORM 項目初期化.
014800     PERFORM 負傷読込.
015550     PERFORM 料金情報取得.
015510     PERFORM 施術所情報取得.
015520     PERFORM 請求先情報取得.
015530     PERFORM 受診者情報取得.
015540     PERFORM 負傷データ取得.
015560     PERFORM 施術記録取得.
015570     PERFORM レセプト並び順取得.
015580*     PERFORM ＯＣＲ情報取得.
015590*     PERFORM 長期判定取得.
015600*     PERFORM 初検日以前のデータ判定.
015610     PERFORM 初検加算時刻取得.
015620     PERFORM 助成印取得.
015630*     PERFORM 給付割合チェック取得.
015640     PERFORM 委任年月日取得.
           PERFORM 施術日取得.
015650*
      */千葉県ひとり親医療費助成事業/201001
           IF (受－助成種別 = "52" ) AND (受－費用負担者番号助成(1:4) = "8512" )
               MOVE "千葉県ひとり親家庭等医療費等助成事業" TO タイトル
           END-IF.
      */千葉県子ども医療費助成事業/120525
           IF (受－助成種別 = "60" ) AND (受－費用負担者番号助成(1:4) = "8312" )
               MOVE "千葉県子ども医療費助成事業" TO タイトル
           END-IF.
      */千葉県重度心身障害医療費助成事業
           IF (受－助成種別 = "53" ) AND (受－費用負担者番号助成(1:4) = "8112" )
               MOVE "千葉県重度心身障害者（児）医療費助成"         TO タイトル
               MOVE NC"重心"         TO 重心
               MOVE NC"○"           TO 重心丸
           END-IF.
016791*-----------------------------------------------*
016800     IF ( 負傷原因印刷区分Ｗ  NOT = 1 ) AND ( レセ負傷原因印刷区分Ｗ NOT = 1 )
016813        IF ( 負傷原因印刷区分Ｗ = 3 OR 4 )
016815           PERFORM 負傷原因印刷対象判定処理
016817        ELSE
016820           PERFORM 負傷原因取得
016821        END-IF
016830     END-IF.
015780*
015790**********************
015800* ＯＣＲコードセット *
015810**********************
015820*
015830*     MOVE ＯＣＲ接骨師会会員番号Ｗ     TO ＯＣＲ会員番号.
015840*     MOVE ＯＣＲ施術年月Ｗ     TO ＯＣＲ施術年月.
015850*     MOVE ＯＣＲ保険種別Ｗ     TO ＯＣＲ保険種別.
015860*     MOVE ＯＣＲ各社ＩＤＷ     TO ＯＣＲ各社ＩＤ.
015870*     MOVE ＯＣＲＦＤ連番Ｗ     TO ＯＣＲＦＤ連番.
015880*     MOVE ＯＣＲ請求金額Ｗ     TO ＯＣＲ請求金額.
015890*     MOVE ＯＣＲ負担割合Ｗ     TO ＯＣＲ負担割合.
015900*
015910**********************
015920* 給付割合チェック   *
015930**********************
015940*     MOVE 給付７割チェックＷ     TO  給付７割チェック.
015950*     MOVE 給付８割チェックＷ     TO  給付８割チェック.
015960*     MOVE 給付９割チェックＷ     TO  給付９割チェック.
015970*     MOVE 老人給付チェックＷ     TO  老人給付チェック.
015970*     MOVE 後高給付チェックＷ     TO  後高給付チェック.
      *     MOVE 後高２Ｗ               TO  後高２.
015980**********************
015990* 親保険チェック   *
016000**********************
016010*     MOVE 政チェックＷ        TO 政府チェック.
016020*     MOVE 組チェックＷ        TO 組合チェック.
016030*     MOVE 日チェックＷ        TO 日雇チェック.
016040*     MOVE 船チェックＷ        TO 船員チェック.
016050*     MOVE 共チェックＷ        TO 共済チェック.
016060*     MOVE 国チェックＷ        TO 国保チェック.
016070*     MOVE 退チェックＷ        TO 退職チェック.
016070*     MOVE 後高チェックＷ      TO 後高チェック.
      *     MOVE 後高１Ｗ            TO 後高１.
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
016080********************
016090* 受診者情報セット *
016100********************
016110     MOVE 施術年Ｗ            TO 施術年.
016120     MOVE 施術月Ｗ            TO 施術月.
016130*
016140*
016150*     IF ( 記号Ｗ(1:1) = NC"＊" )
016160*        MOVE  SPACE    TO  記号
016170*     ELSE
016180*        MOVE 記号Ｗ    TO  記号
016190*     END-IF.
016200*     IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
016210*        ( 印刷番号Ｗ(1:2) = "＊" )
016220*        MOVE  SPACE      TO  番号
016230*     ELSE
016240*        MOVE 印刷番号Ｗ  TO  番号
016250*     END-IF.
016260*
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
016270**
016322     IF 市町村番号Ｗ(1:2) = "99"
016323         MOVE SPACE        TO 公費負担者番号
016324     ELSE
016328         MOVE 市町村番号Ｗ TO 公費負担者番号
016330     END-IF.
016334**
016335*
016340     IF ( 印刷受給者番号Ｗ(1:1) = "*"  ) OR
016350        ( 印刷受給者番号Ｗ(1:2) = "＊" )
016360        MOVE  SPACE              TO 受給者番号
016370     ELSE
016380        MOVE 受給者番号Ｗ        TO 受給者番号
016390     END-IF.
016400**
016410     MOVE 印刷保険者番号Ｗ    TO 保険者番号.
016420**
016430*     MOVE 印刷請求先名称１Ｗ  TO 保険者名称.
016440*     MOVE 印刷請求先名称２Ｗ  TO 保険者名称２.
016450*
016460*     MOVE 請求先名称Ｗ        TO 保険者名称.
016470*     MOVE 被保険者カナＷ      TO 被保険者カナ.
016480     MOVE 被保険者氏名Ｗ      TO 被保険者氏名.
016490*     MOVE 郵便番号１Ｗ        TO 郵便番号１.
016500*     MOVE 郵便番号２Ｗ        TO 郵便番号２.
016510*     MOVE "-"                 TO 郵便区切.
016520     MOVE 被保険者住所１Ｗ    TO 住所１.
016530     MOVE 被保険者住所２Ｗ    TO 住所２.
      */愛知県の助成は受診者氏名住所を記載する/110519
           IF 市町村番号Ｗ(3:2) = "23"
               MOVE 患者氏名Ｗ      TO 被保険者氏名
               MOVE 患者住所１Ｗ    TO 住所１
               MOVE 患者住所２Ｗ    TO 住所２
           END-IF.
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
016540**     MOVE 被保険者住所Ｗ      TO 住所.
016550*     MOVE 患者住所Ｗ          TO 住所.
016550*     MOVE 患者住所１Ｗ          TO 住所１.
016550*     MOVE 患者住所２Ｗ          TO 住所２.
016560*     MOVE 患者カナＷ          TO 患者カナ.
016570     MOVE 患者氏名Ｗ          TO 患者氏名.
016580     MOVE 男チェックＷ        TO 男チェック.
016590     MOVE 女チェックＷ        TO 女チェック.
016600*     MOVE 性別Ｗ               TO 性別.
016610     MOVE 明治チェックＷ      TO 明治チェック.
016620     MOVE 大正チェックＷ      TO 大正チェック.
016630     MOVE 昭和チェックＷ      TO 昭和チェック.
016640     MOVE 平成チェックＷ      TO 平成チェック.
016650*     MOVE 元号Ｗ              TO 元号.
023070     MOVE 令和チェックＷ     TO 令和チェック.
017390*     MOVE 元号Ｗ              TO 患者和暦.
016660     MOVE 患者年Ｗ            TO 患者年.
016670     MOVE 患者月Ｗ            TO 患者月.
016680     MOVE 患者日Ｗ            TO 患者日.
016690*     MOVE 印刷続柄Ｗ          TO 続柄.
      *
           IF 受２－助成被保険者氏名 NOT = SPACE
016940        MOVE 受２－助成被保険者氏名 TO 被保険者氏名
           END-IF.
017170* 
016960     MOVE "・業務災害、通勤災害又は第三者行為以外の原因による。" TO 負傷原因.
016700     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
016710     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
016720     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
016730     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
016740     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
016480     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
016750*
016760     MOVE 助成印Ｗ            TO 助成印.
016770*
016780********************
016790* 長期理由文セット *
016800********************
016810*     MOVE 連長文－理由文(1)    TO 長期理由文１.
016820*     MOVE 連長文－理由文(2)    TO 長期理由文２.
016830*     MOVE 連長文－理由文(3)    TO 長期理由文３.
016840*     MOVE 連長文－理由文(4)    TO 長期理由文４.
016850*     MOVE 連長文－理由文(5)    TO 長期理由文５.
016860*     MOVE 連長文－理由文(6)    TO 長期理由文６.
016870*     MOVE 連長文－理由文(7)    TO 長期理由文７.
016880*     IF ( 連長文－理由文(1)  NOT = SPACE )
016890*          MOVE NC"（長期理由）"  TO 長期理由固定
016900*     END-IF.
016910*
016920********************
016930* 負傷データセット *
016940********************
016950* １部位 *
016960**********
016970     MOVE 負傷名Ｗ(1)       TO 負傷名１.
016980     MOVE 負傷年Ｗ(1)       TO 負傷年１.
016990     MOVE 負傷月Ｗ(1)       TO 負傷月１.
017000     MOVE 負傷日Ｗ(1)       TO 負傷日１.
017010     MOVE 初検年Ｗ(1)       TO 初検年１.
017020     MOVE 初検月Ｗ(1)       TO 初検月１.
017030     MOVE 初検日Ｗ(1)       TO 初検日１.
017040     MOVE 開始年Ｗ(1)       TO 開始年１.
017050     MOVE 開始月Ｗ(1)       TO 開始月１.
017060     MOVE 開始日Ｗ(1)       TO 開始日１.
017070     MOVE 終了年Ｗ(1)       TO 終了年１.
017080     MOVE 終了月Ｗ(1)       TO 終了月１.
017090     MOVE 終了日Ｗ(1)       TO 終了日１.
017100     MOVE 実日数Ｗ(1)       TO 実日数１.
017110     MOVE 治癒チェックＷ(1) TO 治癒チェック１.
017120     MOVE 中止チェックＷ(1) TO 中止チェック１.
017130     MOVE 転医チェックＷ(1) TO 転医チェック１.
017140**********
017150* ２部位 *
017160**********
017170     MOVE 負傷名Ｗ(2)       TO 負傷名２.
017180     MOVE 負傷年Ｗ(2)       TO 負傷年２.
017190     MOVE 負傷月Ｗ(2)       TO 負傷月２.
017200     MOVE 負傷日Ｗ(2)       TO 負傷日２.
017210     MOVE 初検年Ｗ(2)       TO 初検年２.
017220     MOVE 初検月Ｗ(2)       TO 初検月２.
017230     MOVE 初検日Ｗ(2)       TO 初検日２.
017240     MOVE 開始年Ｗ(2)       TO 開始年２.
017250     MOVE 開始月Ｗ(2)       TO 開始月２.
017260     MOVE 開始日Ｗ(2)       TO 開始日２.
017270     MOVE 終了年Ｗ(2)       TO 終了年２.
017280     MOVE 終了月Ｗ(2)       TO 終了月２.
017290     MOVE 終了日Ｗ(2)       TO 終了日２.
017300     MOVE 実日数Ｗ(2)       TO 実日数２.
017310     MOVE 治癒チェックＷ(2) TO 治癒チェック２.
017320     MOVE 中止チェックＷ(2) TO 中止チェック２.
017330     MOVE 転医チェックＷ(2) TO 転医チェック２.
017340**********
017350* ３部位 *
017360**********
017370     MOVE 負傷名Ｗ(3)       TO 負傷名３.
017380     MOVE 負傷年Ｗ(3)       TO 負傷年３.
017390     MOVE 負傷月Ｗ(3)       TO 負傷月３.
017400     MOVE 負傷日Ｗ(3)       TO 負傷日３.
017410     MOVE 初検年Ｗ(3)       TO 初検年３.
017420     MOVE 初検月Ｗ(3)       TO 初検月３.
017430     MOVE 初検日Ｗ(3)       TO 初検日３.
017440     MOVE 開始年Ｗ(3)       TO 開始年３.
017450     MOVE 開始月Ｗ(3)       TO 開始月３.
017460     MOVE 開始日Ｗ(3)       TO 開始日３.
017470     MOVE 終了年Ｗ(3)       TO 終了年３.
017480     MOVE 終了月Ｗ(3)       TO 終了月３.
017490     MOVE 終了日Ｗ(3)       TO 終了日３.
017500     MOVE 実日数Ｗ(3)       TO 実日数３.
017510     MOVE 治癒チェックＷ(3) TO 治癒チェック３.
017520     MOVE 中止チェックＷ(3) TO 中止チェック３.
017530     MOVE 転医チェックＷ(3) TO 転医チェック３.
017540**********
017550* ４部位 *
017560**********
017570     MOVE 負傷名Ｗ(4)       TO 負傷名４.
017580     MOVE 負傷年Ｗ(4)       TO 負傷年４.
017590     MOVE 負傷月Ｗ(4)       TO 負傷月４.
017600     MOVE 負傷日Ｗ(4)       TO 負傷日４.
017610     MOVE 初検年Ｗ(4)       TO 初検年４.
017620     MOVE 初検月Ｗ(4)       TO 初検月４.
017630     MOVE 初検日Ｗ(4)       TO 初検日４.
017640     MOVE 開始年Ｗ(4)       TO 開始年４.
017650     MOVE 開始月Ｗ(4)       TO 開始月４.
017660     MOVE 開始日Ｗ(4)       TO 開始日４.
017670     MOVE 終了年Ｗ(4)       TO 終了年４.
017680     MOVE 終了月Ｗ(4)       TO 終了月４.
017690     MOVE 終了日Ｗ(4)       TO 終了日４.
017700     MOVE 実日数Ｗ(4)       TO 実日数４.
017710     MOVE 治癒チェックＷ(4) TO 治癒チェック４.
017720     MOVE 中止チェックＷ(4) TO 中止チェック４.
017730     MOVE 転医チェックＷ(4) TO 転医チェック４.
017740**********
017750* ５部位 *
017760**********
017770     MOVE 負傷名Ｗ(5)       TO 負傷名５.
017780     MOVE 負傷年Ｗ(5)       TO 負傷年５.
017790     MOVE 負傷月Ｗ(5)       TO 負傷月５.
017800     MOVE 負傷日Ｗ(5)       TO 負傷日５.
017810     MOVE 初検年Ｗ(5)       TO 初検年５.
017820     MOVE 初検月Ｗ(5)       TO 初検月５.
017830     MOVE 初検日Ｗ(5)       TO 初検日５.
017840     MOVE 開始年Ｗ(5)       TO 開始年５.
017850     MOVE 開始月Ｗ(5)       TO 開始月５.
017860     MOVE 開始日Ｗ(5)       TO 開始日５.
017870     MOVE 終了年Ｗ(5)       TO 終了年５.
017880     MOVE 終了月Ｗ(5)       TO 終了月５.
017890     MOVE 終了日Ｗ(5)       TO 終了日５.
017900     MOVE 実日数Ｗ(5)       TO 実日数５.
017910     MOVE 治癒チェックＷ(5) TO 治癒チェック５.
017920     MOVE 中止チェックＷ(5) TO 中止チェック５.
017930     MOVE 転医チェックＷ(5) TO 転医チェック５.
017940**************
017950* 経過セット *
017960**************
017970     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
017980             UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR
017990                   ( 部位ＣＮＴ > 5 )
018000*         MOVE 部位ＣＮＴＷ(部位ＣＮＴ)   TO 経過部位ＣＮＴ(部位ＣＮＴ)
018010*         MOVE 部位区切Ｗ(部位ＣＮＴ)     TO 部位区切(部位ＣＮＴ)
018020         MOVE 印刷経過略称Ｗ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
018030     END-PERFORM.
018040*****************************************
018050*     新規・継続チェックについて        *
018060*   ●新規...初検有り ●継続...初検なし *
018070*****************************************
018080     MOVE 新規チェックＷ    TO 新規チェック.
018090     MOVE 継続チェックＷ    TO 継続チェック.
018100********************
018110* 料金データセット *
018120********************
018130*    ****************************************************************
018140*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
018150*    ****************************************************************
018160     MOVE 初検料ＷＲ                   TO  初検料.
018170     MOVE 時間外チェックＷ             TO  時間外チェック.
018180     MOVE 休日チェックＷ               TO  休日チェック.
018190     MOVE 深夜チェックＷ               TO  深夜チェック.
018200     MOVE 初検加算料ＷＲ               TO  初検加算料.
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
018210     MOVE 再検料ＷＲ                   TO  再検料.
018220     MOVE 往療距離ＷＲ                 TO  往療距離.
018230     MOVE 往療回数ＷＲ                 TO  往療回数.
018240     MOVE 往療料ＷＲ                   TO  往療料.
018250     MOVE 夜間チェックＷ               TO  夜間チェック.
018260     MOVE 暴風雨雪チェックＷ           TO  暴風雨雪チェック.
018270     MOVE 往療加算料ＷＲ               TO  往療加算料.
      */金属副子・運動後療の変更・追加/1805
           MOVE 金属回数Ｗ                   TO  金属回数.
019380     MOVE 金属副子加算料ＷＲ           TO  金属副子加算料.
           MOVE 運動回数Ｗ                   TO  運動回数.
           MOVE 運動料Ｗ                     TO  運動後療料.
018090     MOVE 施術情報提供料ＷＲ           TO  施術情報提供料.
018090     MOVE 明細書発行加算料ＷＲ         TO  明細書発行加算料.
018090     MOVE 明細書発行加算日ＷＲ         TO  明細書発行加算日.
018330     MOVE 小計Ｗ                       TO  小計.
018340********************
018350* 初回処置料セット *
018360********************
018370     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018380             UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR
018390                   ( 部位ＣＮＴ > 5 )
018400         MOVE 初回処置料ＷＲ(部位ＣＮＴ) TO 初回処置料(部位ＣＮＴ)
018410     END-PERFORM.
018420     MOVE 初回処置料合計Ｗ         TO 初回処置料合計
018430*
018440     MOVE 施療料チェックＷ            TO 施療料チェック.
018450     MOVE 整復料チェックＷ            TO 整復料チェック.
018460     MOVE 固定料チェックＷ            TO 固定料チェック.
018470********************
018480* 逓減毎料金セット *
018490********************
018500*    **********
018510*    * １部位 *
018520*    **********
018530     MOVE 後療単価１ＷＲ             TO 後療単価１.
018540     MOVE 後療回数１ＷＲ             TO 後療回数１.
018550     MOVE 後療料１ＷＲ               TO 後療料１.
018560     MOVE 冷罨法回数１ＷＲ           TO 冷罨法回数１.
018570     MOVE 冷罨法料１ＷＲ             TO 冷罨法料１.
018580     MOVE 温罨法回数１ＷＲ           TO 温罨法回数１.
018590     MOVE 温罨法料１ＷＲ             TO 温罨法料１.
018600     MOVE 電療回数１ＷＲ             TO 電療回数１.
018610     MOVE 電療料１ＷＲ               TO 電療料１.
018620     MOVE 小計１ＷＲ                 TO 小計１.
018630     IF ( 長期逓減率１ＷＲ NOT = ZERO )
018640         COMPUTE 長期逓減率１ = 長期逓減率１ＷＲ / 100
018650     END-IF.
018660     MOVE 長期込小計１ＷＲ           TO 長期込小計１.
018670*    **********
018680*    * ２部位 *
018690*    **********
018700     MOVE 後療単価２ＷＲ             TO 後療単価２.
018710     MOVE 後療回数２ＷＲ             TO 後療回数２.
018720     MOVE 後療料２ＷＲ               TO 後療料２.
018730     MOVE 冷罨法回数２ＷＲ           TO 冷罨法回数２.
018740     MOVE 冷罨法料２ＷＲ             TO 冷罨法料２.
018750     MOVE 温罨法回数２ＷＲ           TO 温罨法回数２.
018760     MOVE 温罨法料２ＷＲ             TO 温罨法料２.
018770     MOVE 電療回数２ＷＲ             TO 電療回数２.
018780     MOVE 電療料２ＷＲ               TO 電療料２.
018790     MOVE 小計２ＷＲ                 TO 小計２.
018800     IF ( 長期逓減率２ＷＲ NOT = ZERO )
018810         COMPUTE 長期逓減率２ = 長期逓減率２ＷＲ / 100
018820     END-IF.
018830     MOVE 長期込小計２ＷＲ           TO 長期込小計２.
018840*    ****************
018850*    * ３部位／８割 *
018860*    ****************
018870     MOVE 後療単価３８ＷＲ             TO 後療単価３８.
018880     MOVE 後療回数３８ＷＲ             TO 後療回数３８.
018890     MOVE 後療料３８ＷＲ               TO 後療料３８.
018900     MOVE 冷罨法回数３８ＷＲ           TO 冷罨法回数３８.
018910     MOVE 冷罨法料３８ＷＲ             TO 冷罨法料３８.
018920     MOVE 温罨法回数３８ＷＲ           TO 温罨法回数３８.
018930     MOVE 温罨法料３８ＷＲ             TO 温罨法料３８.
018940     MOVE 電療回数３８ＷＲ             TO 電療回数３８.
018950     MOVE 電療料３８ＷＲ               TO 電療料３８.
018960     MOVE 小計３８ＷＲ                 TO 小計３８.
018970     MOVE 多部位込小計３８ＷＲ         TO 多部位込小計３８.
018980     IF ( 長期逓減率３８ＷＲ NOT = ZERO )
018990         COMPUTE 長期逓減率３８ = 長期逓減率３８ＷＲ / 100
019000     END-IF.
019010     MOVE 長期込小計３８ＷＲ           TO 長期込小計３８.
      */ 逓減率 0.7→0.6 /42505
           IF (施術和暦年月ＷＲ >= 42505)
              MOVE "60"                      TO 逓減３８
              MOVE "0.6"                     TO 多部位３８
           END-IF.
019020*    ****************
019030*    * ３部位／10割 *
019040*    ****************
019050     MOVE 逓減開始月３０ＷＲ           TO 逓減開始月３０.
019060     MOVE 逓減開始日３０ＷＲ           TO 逓減開始日３０.
019070     MOVE 後療単価３０ＷＲ             TO 後療単価３０.
019080     MOVE 後療回数３０ＷＲ             TO 後療回数３０.
019090     MOVE 後療料３０ＷＲ               TO 後療料３０.
019100     MOVE 冷罨法回数３０ＷＲ           TO 冷罨法回数３０.
019110     MOVE 冷罨法料３０ＷＲ             TO 冷罨法料３０.
019120     MOVE 温罨法回数３０ＷＲ           TO 温罨法回数３０.
019130     MOVE 温罨法料３０ＷＲ             TO 温罨法料３０.
019140     MOVE 電療回数３０ＷＲ             TO 電療回数３０.
019150     MOVE 電療料３０ＷＲ               TO 電療料３０.
019160     MOVE 小計３０ＷＲ                 TO 小計３０.
019170     IF ( 長期逓減率３０ＷＲ NOT = ZERO )
019180         COMPUTE 長期逓減率３０ = 長期逓減率３０ＷＲ / 100
019190     END-IF.
019200     MOVE 長期込小計３０ＷＲ           TO 長期込小計３０.
019210*    ****************
019220*    * ４部位／５割 *
019230*    ****************
019240*     MOVE 後療単価４５ＷＲ             TO 後療単価４５.
019250*     MOVE 後療回数４５ＷＲ             TO 後療回数４５.
019260*     MOVE 後療料４５ＷＲ               TO 後療料４５.
019270*     MOVE 冷罨法回数４５ＷＲ           TO 冷罨法回数４５.
019280*     MOVE 冷罨法料４５ＷＲ             TO 冷罨法料４５.
019290*     MOVE 温罨法回数４５ＷＲ           TO 温罨法回数４５.
019300*     MOVE 温罨法料４５ＷＲ             TO 温罨法料４５.
019310*     MOVE 電療回数４５ＷＲ             TO 電療回数４５.
019320*     MOVE 電療料４５ＷＲ               TO 電療料４５.
019330*     MOVE 小計４５ＷＲ                 TO 小計４５.
019340*     MOVE 多部位込小計４５ＷＲ         TO 多部位込小計４５.
019350*     IF ( 長期逓減率４５ＷＲ NOT = ZERO )
019360*         COMPUTE 長期逓減率４５ = 長期逓減率４５ＷＲ / 100
019370*     END-IF.
019380*     MOVE 長期込小計４５ＷＲ           TO 長期込小計４５.
019390*    ****************
019400*    * ４部位／８割 *
019410*    ****************
019420     MOVE 逓減開始月４８ＷＲ           TO 逓減開始月４８.
019430     MOVE 逓減開始日４８ＷＲ           TO 逓減開始日４８.
019440     MOVE 後療単価４８ＷＲ             TO 後療単価４８.
019450     MOVE 後療回数４８ＷＲ             TO 後療回数４８.
019460     MOVE 後療料４８ＷＲ               TO 後療料４８.
019470     MOVE 冷罨法回数４８ＷＲ           TO 冷罨法回数４８.
019480     MOVE 冷罨法料４８ＷＲ             TO 冷罨法料４８.
019490     MOVE 温罨法回数４８ＷＲ           TO 温罨法回数４８.
019500     MOVE 温罨法料４８ＷＲ             TO 温罨法料４８.
019510     MOVE 電療回数４８ＷＲ             TO 電療回数４８.
019520     MOVE 電療料４８ＷＲ               TO 電療料４８.
019530     MOVE 小計４８ＷＲ                 TO 小計４８.
019540     MOVE 多部位込小計４８ＷＲ         TO 多部位込小計４８.
019550     IF ( 長期逓減率４８ＷＲ NOT = ZERO )
019560         COMPUTE 長期逓減率４８ = 長期逓減率４８ＷＲ / 100
019570     END-IF.
019580     MOVE 長期込小計４８ＷＲ           TO 長期込小計４８.
      */ 逓減率 0.7→0.6 /42505
           IF (施術和暦年月ＷＲ >= 42505)
              MOVE "60"                      TO 逓減４８
              MOVE "0.6"                     TO 多部位４８
           END-IF.
019590*    ****************
019600*    * ４部位／10割 *
019610*    ****************
019620     MOVE 逓減開始月４０ＷＲ           TO 逓減開始月４０.
019630     MOVE 逓減開始日４０ＷＲ           TO 逓減開始日４０.
019640     MOVE 後療単価４０ＷＲ             TO 後療単価４０.
019650     MOVE 後療回数４０ＷＲ             TO 後療回数４０.
019660     MOVE 後療料４０ＷＲ               TO 後療料４０.
019670     MOVE 冷罨法回数４０ＷＲ           TO 冷罨法回数４０.
019680     MOVE 冷罨法料４０ＷＲ             TO 冷罨法料４０.
019690     MOVE 温罨法回数４０ＷＲ           TO 温罨法回数４０.
019700     MOVE 温罨法料４０ＷＲ             TO 温罨法料４０.
019710     MOVE 電療回数４０ＷＲ             TO 電療回数４０.
019720     MOVE 電療料４０ＷＲ               TO 電療料４０.
019730     MOVE 小計４０ＷＲ                 TO 小計４０.
019740     IF ( 長期逓減率４０ＷＲ NOT = ZERO )
019750         COMPUTE 長期逓減率４０ = 長期逓減率４０ＷＲ / 100
019760     END-IF.
019770     MOVE 長期込小計４０ＷＲ           TO 長期込小計４０.
019780*
019790*↓***********************************************************************
019800* ５部位／2.5割の印字は必要ない。
019810*------------------------------------------------------------------------*
019820*    *****************
019830*    * ５部位／2.5割 *
019840*    *****************
019850*     MOVE 後療単価５２ＷＲ             TO 後療単価５２.
019860*     MOVE 後療回数５２ＷＲ             TO 後療回数５２.
019870*     MOVE 後療料５２ＷＲ               TO 後療料５２.
019880*     MOVE 冷罨法回数５２ＷＲ           TO 冷罨法回数５２.
019890*     MOVE 冷罨法料５２ＷＲ             TO 冷罨法料５２.
019900*     MOVE 温罨法回数５２ＷＲ           TO 温罨法回数５２.
019910*     MOVE 温罨法料５２ＷＲ             TO 温罨法料５２.
019920*     MOVE 電療回数５２ＷＲ             TO 電療回数５２.
019930*     MOVE 電療料５２ＷＲ               TO 電療料５２.
019940*     MOVE 小計５２ＷＲ                 TO 小計５２.
019950*     MOVE 多部位込小計５２ＷＲ         TO 多部位込小計５２.
019960*     IF ( 長期逓減率５２ＷＲ NOT = ZERO )
019970*         COMPUTE 長期逓減率５２ = 長期逓減率５２ＷＲ / 100
019980*     END-IF.
019990*     MOVE 長期込小計５２ＷＲ           TO 長期込小計５２.
020000*↑***********************************************************************
020010*
020020*    ****************
020030*    * ５部位／５割 *
020040*    ****************
020050*     MOVE 逓減開始月５５ＷＲ           TO 逓減開始月５５.
020060*     MOVE 逓減開始日５５ＷＲ           TO 逓減開始日５５.
020070*     MOVE 後療単価５５ＷＲ             TO 後療単価５５.
020080*     MOVE 後療回数５５ＷＲ             TO 後療回数５５.
020090*     MOVE 後療料５５ＷＲ               TO 後療料５５.
020100*     MOVE 冷罨法回数５５ＷＲ           TO 冷罨法回数５５.
020110*     MOVE 冷罨法料５５ＷＲ             TO 冷罨法料５５.
020120*     MOVE 温罨法回数５５ＷＲ           TO 温罨法回数５５.
020130*     MOVE 温罨法料５５ＷＲ             TO 温罨法料５５.
020140*     MOVE 電療回数５５ＷＲ             TO 電療回数５５.
020150*     MOVE 電療料５５ＷＲ               TO 電療料５５.
020160*     MOVE 小計５５ＷＲ                 TO 小計５５.
020170*     MOVE 多部位込小計５５ＷＲ         TO 多部位込小計５５.
020180*     IF ( 長期逓減率５５ＷＲ NOT = ZERO )
020190*         COMPUTE 長期逓減率５５ = 長期逓減率５５ＷＲ / 100
020200*     END-IF.
020210*     MOVE 長期込小計５５ＷＲ           TO 長期込小計５５.
020220*    ****************
020230*    * ５部位／８割 *
020240*    ****************
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
020420*    ****************
020430*    * ５部位／10割 *
020440*    ****************
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
020620*
020630* ********************
020640* * 受給者負担額内訳 *
020650* ********************
020660*     IF ( 連料－受給者負担額  NOT = ZERO )
020670*         MOVE NC"受給者負担額"   TO 負担額内訳題字
020680*     END-IF.
020690*     PERFORM VARYING 回数ＣＮＴ FROM 1 BY 1 UNTIL 回数ＣＮＴ > 4
020700*         MOVE 印刷負担回数Ｗ(回数ＣＮＴ) TO 印刷負担回数(回数ＣＮＴ)
020710*         MOVE 当日分負担額Ｗ(回数ＣＮＴ) TO 当日分負担額(回数ＣＮＴ)
020720*         IF ( 印刷負担回数Ｗ(回数ＣＮＴ) NOT = SPACE )
020730*              MOVE NC"円"                TO 当日分負担額単位(回数ＣＮＴ)
020740*         END-IF
020750*     END-PERFORM.
020760* 
020770     MOVE 適用１Ｗ                       TO 適用１.
020780     MOVE 適用２Ｗ                       TO 適用２.
019660*     MOVE 適用３Ｗ                       TO 適用３.
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
              MOVE 13           TO 連金運－会コード
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
020790******
020440     MOVE レセ－合計                     TO 合計.
020450     MOVE レセ－一部負担金               TO 一部負担金.
020460     MOVE レセ－請求金額                 TO 請求金額.
      *
           EVALUATE TRUE
      */愛知県(３つ枠のみ使用。費用、負担は本体。請求額に助成)
           WHEN 市町村番号Ｗ(3:2) = "23"
               MOVE レセ－助成請求金額 TO 請求金額
      */大阪府(３つ枠のみ使用。費用は本体。負担、請求額に助成)
      */京都も同様
           WHEN (市町村番号Ｗ(3:2)  = "27") OR
                ((市町村番号Ｗ(3:2) = "26") AND (助成種別ＷＲ NOT = 54))
               MOVE レセ－受給者負担額 TO 一部負担金
               MOVE レセ－助成請求金額 TO 請求金額
      */千葉県の子ども医療費助成/120404
      */千葉県の重度心身障害医療費助成/150703
      */千葉県のひとり親医療費助成/201001
           WHEN ((助成種別ＷＲ = 60) AND (市町村番号Ｗ(1:4) =  "8312")) OR
                ((助成種別ＷＲ = 53) AND (市町村番号Ｗ(1:4) =  "8112")) OR
                ((助成種別ＷＲ = 52) AND (市町村番号Ｗ(1:4) =  "8512"))
               MOVE "X" TO EDIT-MODE OF   一部負担金
               MOVE レセ－一部負担金   TO 一部負担金２
               MOVE レセ－受給者負担額 TO 受給者負担額３
               MOVE レセ－助成請求金額 TO 請求金額
           WHEN OTHER
020830         MOVE レセ－受給者負担額 TO 受給者負担額
020840         MOVE レセ－助成請求金額 TO 助成請求額
           END-EVALUATE.
020850*
022410*------------------------------------------------------------------------*
      */長期頻回20241007/↓↓↓
           MOVE 部位継続月数Ｗ(1) TO 継続月数１.
           MOVE 部位継続月数Ｗ(2) TO 継続月数２.
           MOVE 部位継続月数Ｗ(3) TO 継続月数３.
           MOVE 部位継続月数Ｗ(4) TO 継続月数４.
           MOVE 部位継続月数Ｗ(5) TO 継続月数５.
      *
           IF レセ－長期頻回逓減率１ NOT = ZERO
               MOVE ZERO TO 長期逓減率１
018410         COMPUTE 頻回逓減率１ = 長期逓減率１ＷＲ / 100
           END-IF
           IF レセ－長期頻回逓減率２ NOT = ZERO
               MOVE ZERO TO 長期逓減率２
018410         COMPUTE 頻回逓減率２ = 長期逓減率２ＷＲ / 100
           END-IF
           IF レセ－長期頻回逓減率３８ NOT = ZERO
               MOVE ZERO TO 長期逓減率３８
018410         COMPUTE 頻回逓減率３８ = 長期逓減率３８ＷＲ / 100
           END-IF
           IF レセ－長期頻回逓減率３０ NOT = ZERO
               MOVE ZERO TO 長期逓減率３０
018410         COMPUTE 頻回逓減率３０ = 長期逓減率３０ＷＲ / 100
           END-IF
           IF レセ－長期頻回逓減率４８ NOT = ZERO
               MOVE ZERO TO 長期逓減率４８
018410         COMPUTE 頻回逓減率４８ = 長期逓減率４８ＷＲ / 100
           END-IF
           IF レセ－長期頻回逓減率４０ NOT = ZERO
               MOVE ZERO TO 長期逓減率４０
018410         COMPUTE 頻回逓減率４０ = 長期逓減率４０ＷＲ / 100
           END-IF
      */長期頻回20241007/↑↑↑
      */長期頻回コメント/20241007↓↓↓
022420** 長期頻回の時、摘要欄に内容を記載
      **
      *     MOVE SPACE                     TO 長期頻回Ｗ.
      *     IF (レセ－部位継続月数(1) > 5) OR (レセ－部位継続月数(2) > 5) OR
      *        (レセ－部位継続月数(3) > 5) OR (レセ－部位継続月数(4) > 5) OR
      *        (レセ－部位継続月数(5) > 5)
      *        MOVE "長期頻回該当："       TO 長期頻回ＣＭ
      *     END-IF.
      *     IF (レセ－部位継続月数(1) > 5)
      *        MOVE レセ－部位継続月数(1)  TO 月数Ｗ
      *        MOVE 負傷名Ｗ(1)            TO 負傷名ＷＲ(1)
      *        STRING "(1)"                DELIMITED BY SIZE
      *               負傷名ＷＰ(1)        DELIMITED BY "　"
      *               "、継続月数"         DELIMITED BY SIZE
      *               月数Ｗ               DELIMITED BY SIZE
      *               "月"                 DELIMITED BY SIZE
      *          INTO 長期頻回１ＷＴ
      *        END-STRING
      *     END-IF.
      *     IF (レセ－部位継続月数(2) > 5)
      *        MOVE レセ－部位継続月数(2)  TO 月数Ｗ
      *        MOVE 負傷名Ｗ(2)            TO 負傷名ＷＲ(2)
      *        STRING "(2)"                DELIMITED BY SIZE
      *               負傷名ＷＰ(2)        DELIMITED BY "　"
      *               "、継続月数"         DELIMITED BY SIZE
      *               月数Ｗ               DELIMITED BY SIZE
      *               "月"                 DELIMITED BY SIZE
      *          INTO 長期頻回２ＷＴ
      *        END-STRING
      *     END-IF.
      *     IF (レセ－部位継続月数(3) > 5)
      *        MOVE レセ－部位継続月数(3)  TO 月数Ｗ
      *        MOVE 負傷名Ｗ(3)            TO 負傷名ＷＲ(3)
      *        STRING "(3)"                DELIMITED BY SIZE
      *               負傷名ＷＰ(3)        DELIMITED BY "　"
      *               "、継続月数"         DELIMITED BY SIZE
      *               月数Ｗ               DELIMITED BY SIZE
      *               "月"                 DELIMITED BY SIZE
      *          INTO 長期頻回３ＷＴ
      *        END-STRING
      *     END-IF.
      *     IF (レセ－部位継続月数(4) > 5)
      *        MOVE レセ－部位継続月数(4)  TO 月数Ｗ
      *        MOVE 負傷名Ｗ(4)            TO 負傷名ＷＲ(4)
      *        STRING "(4)"                DELIMITED BY SIZE
      *               負傷名ＷＰ(4)        DELIMITED BY "　"
      *               "、継続月数"         DELIMITED BY SIZE
      *               月数Ｗ               DELIMITED BY SIZE
      *               "月"                 DELIMITED BY SIZE
      *          INTO 長期頻回４ＷＴ
      *        END-STRING
      *     END-IF.
      *     IF (レセ－部位継続月数(5) > 5)
      *        MOVE レセ－部位継続月数(5)  TO 月数Ｗ
      *        MOVE 負傷名Ｗ(5)            TO 負傷名ＷＲ(5)
      *        STRING "(5)"                DELIMITED BY SIZE
      *               負傷名ＷＰ(5)        DELIMITED BY "　"
      *               "、継続月数"         DELIMITED BY SIZE
      *               月数Ｗ               DELIMITED BY SIZE
      *               "月"                 DELIMITED BY SIZE
      *          INTO 長期頻回５ＷＴ
      *        END-STRING
      *     END-IF.
      *     MOVE 長期頻回ＣＭ   TO 文字１Ｗ.
      *     MOVE 長期頻回１ＷＴ TO 文字２Ｗ.
      *     CALL プログラム名Ｗ WITH C LINKAGE
      *                   USING BY REFERENCE 文字１Ｗ
      *                         BY REFERENCE 文字２Ｗ.
      *     MOVE 長期頻回２ＷＴ TO 文字２Ｗ.
      *     CALL プログラム名Ｗ WITH C LINKAGE
      *                   USING BY REFERENCE 文字１Ｗ
      *                         BY REFERENCE 文字２Ｗ.
      *     MOVE 長期頻回３ＷＴ TO 文字２Ｗ.
      *     CALL プログラム名Ｗ WITH C LINKAGE
      *                   USING BY REFERENCE 文字１Ｗ
      *                         BY REFERENCE 文字２Ｗ.
      *     MOVE 長期頻回４ＷＴ TO 文字２Ｗ.
      *     CALL プログラム名Ｗ WITH C LINKAGE
      *                   USING BY REFERENCE 文字１Ｗ
      *                         BY REFERENCE 文字２Ｗ.
      *     MOVE 長期頻回５ＷＴ TO 文字２Ｗ.
      *     CALL プログラム名Ｗ WITH C LINKAGE
      *                   USING BY REFERENCE 文字１Ｗ
      *                         BY REFERENCE 文字２Ｗ.
      *     MOVE 文字１Ｗ       TO 長期頻回.
      **
      */長期頻回コメント/20241007↑↑↑
020878**********************
020880* 施術所データセット *
020890**********************
           MOVE 都道府県ＪＩＳＷ       TO 都道府県番号.
020900     MOVE 柔整師番号Ｗ           TO 柔整師番号.
020910*     MOVE 柔整師番号１Ｗ           TO 柔整師番号１.
020920*     MOVE 柔整師番号２Ｗ           TO 柔整師番号２.
020930*     MOVE 柔整師番号３Ｗ           TO 柔整師番号３.
020940*     MOVE 定額制受理番号Ｗ       TO 定額制受理番号.
020950     MOVE 施術所郵便番号１Ｗ     TO 施術所郵便番号１.
020960     MOVE 施術所郵便番号２Ｗ     TO 施術所郵便番号２.
020980     MOVE 施術所住所１Ｗ         TO 施術所住所１.
020990     MOVE 施術所住所２Ｗ         TO 施術所住所２.
021000     MOVE 接骨師会会員番号Ｗ     TO 接骨師会会員番号.
021010     MOVE 代表者カナＷ           TO 代表者カナ.
021020     MOVE 代表者名Ｗ             TO 代表者名.
021030     MOVE 接骨師会会員番号Ｗ     TO 接骨師会会員番号.
021040     MOVE 施術所電話番号Ｗ       TO 施術所電話番号.
021050*
021060     MOVE 接骨院名Ｗ             TO 接骨院名.
021070*
021080*     MOVE 銀行名支店名Ｗ         TO 銀行名支店名.
021090*     MOVE 預金種別コメントＷ     TO 預金種別.
021100     MOVE 口座番号Ｗ             TO 口座番号.
021110     MOVE 口座名義人カナＷ       TO 口座名義人カナ.
021120*     MOVE 口座名義人とカナ１Ｗ   TO 口座名義人.
021130*     MOVE 口座名義人とカナ２Ｗ   TO 口座名義人２.
           MOVE 口座名義人Ｗ           TO 口座名義人.
021140     MOVE コメント１Ｗ           TO コメント１.
021150     MOVE コメント２Ｗ           TO コメント２.
021160     MOVE コメント３Ｗ           TO コメント３.
021170     MOVE コメント４Ｗ           TO コメント４.
021180*     MOVE コメント５Ｗ           TO コメント５.
021190*
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

021200* / 柔整師・患者委任日 /
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
021210     MOVE 柔整師年Ｗ             TO 受理年.
021220     MOVE 柔整師月Ｗ             TO 受理月.
021230     MOVE 柔整師日Ｗ             TO 受理日.
021240* ( 委任年月日 印刷するか )
021250     IF ( 連入－委任印刷  = ZERO )
037370     IF (施術和暦Ｗ > 4) OR (用紙種別Ｗ > 1)
037370         IF 施術和暦Ｗ > 4
                   MOVE 施術和暦Ｗ         TO 元－元号区分
037380             READ 元号マスタ
037390             NOT INVALID KEY
037400                 MOVE 元－元号名称   TO 委任和暦
037410             END-READ
      *             MOVE "===="             TO 委任和暦訂正
               END-IF
      */元号修正/↑↑↑20190408
021260         MOVE 患者委任年Ｗ       TO 委任年
021270         MOVE 患者委任月Ｗ       TO 委任月
021280         MOVE 患者委任日Ｗ       TO 委任日
021290     END-IF.
021300*
021310* 施術ID
021320     MOVE 県施術ＩＤＷ           TO 県施術ＩＤ.
021330*
021340************************
021350* レセプト並び順セット *
021360************************
021370     MOVE 順番固定Ｗ          TO 順番固定.
021380     MOVE 順番Ｗ              TO 順番.
021390     MOVE 患者番号ＷＲ        TO 患者番号.
021400     MOVE 枝番ＷＲ            TO 枝番.
021410*
021420*
021430* 東京都　右上に「前」印字（高齢者） 14/10～
021440*     MOVE 特別マークＷ           TO 特別マーク.
021450*
021460* 愛知県　特別コメント（４１老）14/10～
021470*     MOVE 特別コメントＷ         TO 特別コメント.
021310*-------------------------------------------------------------------------*
021320*--- ※ レセ摘要再セットは、この印刷セットSECTION の最後にやること！ -----*
021330     PERFORM レセ摘要再セット.
021340*-------------------------------------------------------------------------*
021480*
021492*-------------------------------------------------------------------------*
021493*--- ※ 地域特有処理は、この印刷セットSECTION の最後にやること！   　-----*
021494     PERFORM 地域特有処理.
021495*-------------------------------------------------------------------------*
021496*
021500********     PERFORM テスト印字処理.
021510*
021520*================================================================*
021530 項目初期化 SECTION.
021540*
021550     INITIALIZE 施術所情報Ｗ.
021560     INITIALIZE 受診者情報Ｗ.
021570     INITIALIZE 負傷情報Ｗ.
021580     INITIALIZE 備考情報Ｗ.
021590*     INITIALIZE ＯＣＲコードＷ.
021600     INITIALIZE 料金１ＷＲ.
021610     INITIALIZE 料金２ＷＲ.
021620     INITIALIZE 料金３ＷＲ.
021640     INITIALIZE YCH6427P.
021630     MOVE SPACE TO YCH6427P.
021650*================================================================*
021660 料金情報取得 SECTION.
021670*
           MOVE 3            TO レセ－レセ種別.
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
021680********************
021690* 料金データセット *
021700********************
021710*    ****************************************************************
021720*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
021730*    ****************************************************************
021740     MOVE レセ－初検料                 TO 初検料ＷＲ.
021750     IF ( レセ－時間外 = 1 )
021760         MOVE NC"○"                   TO 時間外チェックＷ
021770     END-IF.
021780     IF ( レセ－休日 = 1 )
021790         MOVE NC"○"                   TO 休日チェックＷ
021800     END-IF.
021810     IF ( レセ－深夜 = 1 )
021820         MOVE NC"○"                   TO 深夜チェックＷ
021830     END-IF.
021840*
021850     MOVE レセ－初検加算料             TO  初検加算料ＷＲ.
           MOVE レセ－初検時相談料           TO  初検時相談料ＷＲ.
021860     MOVE レセ－再検料                 TO  再検料ＷＲ.
021870     MOVE レセ－往療距離               TO  往療距離ＷＲ.
021880     MOVE レセ－往療回数               TO  往療回数ＷＲ.
021890     MOVE レセ－往療料                 TO  往療料ＷＲ.
021900     MOVE レセ－往療加算料             TO  往療加算料ＷＲ.
021910*
021920     IF ( レセ－夜間 = 1 )
021930         MOVE NC"○"                   TO 夜間チェックＷ
021940     END-IF.
021950     IF ( レセ－暴風雨雪 = 1 )
021960         MOVE NC"○"                   TO 暴風雨雪チェックＷ
021970     END-IF.
021980*
021990     MOVE レセ－金属副子加算料         TO  金属副子加算料ＷＲ.
022000*
      */金属副子・運動後療の変更・追加/1805
           MOVE レセ－金属副子回数            TO 金属回数Ｗ.
           MOVE レセ－運動後療回数            TO 運動回数Ｗ.
           MOVE レセ－運動後療料              TO 運動料Ｗ.
021940*
021950     MOVE レセ－施術情報提供料          TO  施術情報提供料ＷＲ.
      */2022
           MOVE レセ－明細書発行加算料         TO 明細書発行加算料ＷＲ.
           MOVE レセ－明細書発行加算日         TO 明細書発行加算日ＷＲ.
021960* 小計
022420     COMPUTE 小計Ｗ = レセ－小計 + レセ－運動後療料 + レセ－明細書発行加算料.
022140********************
022150* 初回処置料セット *
022160********************
022170     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
022180             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
022190         MOVE レセ－初回処置料(部位ＣＮＴ) TO 初回処置料ＷＲ(部位ＣＮＴ)
022200         IF ( レセ－初回処置料(部位ＣＮＴ) NOT = ZERO )
022210            EVALUATE 負－負傷種別(部位ＣＮＴ)
022220* 捻挫・打撲・挫傷
022230            WHEN 1
022240            WHEN 2
022250            WHEN 3
022260                MOVE NC"○"       TO 施療料チェックＷ
022270* 脱臼・骨折・骨折拘縮
022280            WHEN 4
022290            WHEN 5
022300            WHEN 7
022310                MOVE NC"○"       TO 整復料チェックＷ
022320* 不全骨折・不全骨折拘縮
022330            WHEN 6
022340            WHEN 8
022350                MOVE NC"○"       TO 固定料チェックＷ
022360            END-EVALUATE
022370         END-IF
022380     END-PERFORM.
022390     MOVE レセ－初回処置料合計         TO 初回処置料合計Ｗ.
022400********************
022410* 逓減毎料金セット *
022420********************
022430*    **********
022440*    * １部位 *
022450*    **********
022460     MOVE レセ－後療単価１             TO 後療単価１ＷＲ.
022470     MOVE レセ－後療回数１             TO 後療回数１ＷＲ.
022480     MOVE レセ－後療料１               TO 後療料１ＷＲ.
022490     MOVE レセ－冷罨法回数１           TO 冷罨法回数１ＷＲ.
022500     MOVE レセ－冷罨法料１             TO 冷罨法料１ＷＲ.
022510     MOVE レセ－温罨法回数１           TO 温罨法回数１ＷＲ.
022520     MOVE レセ－温罨法料１             TO 温罨法料１ＷＲ.
022530     MOVE レセ－電療回数１             TO 電療回数１ＷＲ.
022540     MOVE レセ－電療料１               TO 電療料１ＷＲ.
022550     MOVE レセ－小計１                 TO 小計１ＷＲ.
           IF レセ－長期頻回逓減率１ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率１   TO 長期逓減率１ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率１       TO 長期逓減率１ＷＲ
           END-IF.
022570     MOVE レセ－長期込小計１           TO 長期込小計１ＷＲ.
022580*    **********
022590*    * ２部位 *
022600*    **********
022610     MOVE レセ－後療単価２             TO 後療単価２ＷＲ.
022620     MOVE レセ－後療回数２             TO 後療回数２ＷＲ.
022630     MOVE レセ－後療料２               TO 後療料２ＷＲ.
022640     MOVE レセ－冷罨法回数２           TO 冷罨法回数２ＷＲ.
022650     MOVE レセ－冷罨法料２             TO 冷罨法料２ＷＲ.
022660     MOVE レセ－温罨法回数２           TO 温罨法回数２ＷＲ.
022670     MOVE レセ－温罨法料２             TO 温罨法料２ＷＲ.
022680     MOVE レセ－電療回数２             TO 電療回数２ＷＲ.
022690     MOVE レセ－電療料２               TO 電療料２ＷＲ.
022700     MOVE レセ－小計２                 TO 小計２ＷＲ.
           IF レセ－長期頻回逓減率２ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率２   TO 長期逓減率２ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率２       TO 長期逓減率２ＷＲ
           END-IF.
022720     MOVE レセ－長期込小計２           TO 長期込小計２ＷＲ.
022730*    ****************
022740*    * ３部位／８割 *
022750*    ****************
022760     MOVE レセ－後療単価３８             TO 後療単価３８ＷＲ.
022770     MOVE レセ－後療回数３８             TO 後療回数３８ＷＲ.
022780     MOVE レセ－後療料３８               TO 後療料３８ＷＲ.
022790     MOVE レセ－冷罨法回数３８           TO 冷罨法回数３８ＷＲ.
022800     MOVE レセ－冷罨法料３８             TO 冷罨法料３８ＷＲ.
022810     MOVE レセ－温罨法回数３８           TO 温罨法回数３８ＷＲ.
022820     MOVE レセ－温罨法料３８             TO 温罨法料３８ＷＲ.
022830     MOVE レセ－電療回数３８             TO 電療回数３８ＷＲ.
022840     MOVE レセ－電療料３８               TO 電療料３８ＷＲ.
022850     MOVE レセ－小計３８                 TO 小計３８ＷＲ.
022860     MOVE レセ－多部位込小計３８         TO 多部位込小計３８ＷＲ.
           IF レセ－長期頻回逓減率３８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率３８   TO 長期逓減率３８ＷＲ
           ELSE
024160         MOVE レセ－長期逓減率３８       TO 長期逓減率３８ＷＲ
           END-IF.
022880     MOVE レセ－長期込小計３８           TO 長期込小計３８ＷＲ.
022890*    ****************
022900*    * ３部位／10割 *
022910*    ****************
022920     MOVE レセ－逓減開始月３０           TO 逓減開始月３０ＷＲ.
022930     MOVE レセ－逓減開始日３０           TO 逓減開始日３０ＷＲ.
022940     MOVE レセ－後療単価３０             TO 後療単価３０ＷＲ.
022950     MOVE レセ－後療回数３０             TO 後療回数３０ＷＲ.
022960     MOVE レセ－後療料３０               TO 後療料３０ＷＲ.
022970     MOVE レセ－冷罨法回数３０           TO 冷罨法回数３０ＷＲ.
022980     MOVE レセ－冷罨法料３０             TO 冷罨法料３０ＷＲ.
022990     MOVE レセ－温罨法回数３０           TO 温罨法回数３０ＷＲ.
023000     MOVE レセ－温罨法料３０             TO 温罨法料３０ＷＲ.
023010     MOVE レセ－電療回数３０             TO 電療回数３０ＷＲ.
023020     MOVE レセ－電療料３０               TO 電療料３０ＷＲ.
023030     MOVE レセ－小計３０                 TO 小計３０ＷＲ.
           IF レセ－長期頻回逓減率３０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率３０   TO 長期逓減率３０ＷＲ
           ELSE
024330         MOVE レセ－長期逓減率３０       TO 長期逓減率３０ＷＲ
           END-IF.
023050     MOVE レセ－長期込小計３０           TO 長期込小計３０ＷＲ.
023060*    ****************
023070*    * ４部位／５割 *
023080*    ****************
023090     MOVE レセ－後療単価４５             TO 後療単価４５ＷＲ.
023100     MOVE レセ－後療回数４５             TO 後療回数４５ＷＲ.
023110     MOVE レセ－後療料４５               TO 後療料４５ＷＲ.
023120     MOVE レセ－冷罨法回数４５           TO 冷罨法回数４５ＷＲ.
023130     MOVE レセ－冷罨法料４５             TO 冷罨法料４５ＷＲ.
023140     MOVE レセ－温罨法回数４５           TO 温罨法回数４５ＷＲ.
023150     MOVE レセ－温罨法料４５             TO 温罨法料４５ＷＲ.
023160     MOVE レセ－電療回数４５             TO 電療回数４５ＷＲ.
023170     MOVE レセ－電療料４５               TO 電療料４５ＷＲ.
023180     MOVE レセ－小計４５                 TO 小計４５ＷＲ.
023190     MOVE レセ－多部位込小計４５         TO 多部位込小計４５ＷＲ.
023200     MOVE レセ－長期逓減率４５           TO 長期逓減率４５ＷＲ.
023210     MOVE レセ－長期込小計４５           TO 長期込小計４５ＷＲ.
023220*    ****************
023230*    * ４部位／８割 *
023240*    ****************
023250     MOVE レセ－逓減開始月４８           TO 逓減開始月４８ＷＲ.
023260     MOVE レセ－逓減開始日４８           TO 逓減開始日４８ＷＲ.
023270     MOVE レセ－後療単価４８             TO 後療単価４８ＷＲ.
023280     MOVE レセ－後療回数４８             TO 後療回数４８ＷＲ.
023290     MOVE レセ－後療料４８               TO 後療料４８ＷＲ.
023300     MOVE レセ－冷罨法回数４８           TO 冷罨法回数４８ＷＲ.
023310     MOVE レセ－冷罨法料４８             TO 冷罨法料４８ＷＲ.
023320     MOVE レセ－温罨法回数４８           TO 温罨法回数４８ＷＲ.
023330     MOVE レセ－温罨法料４８             TO 温罨法料４８ＷＲ.
023340     MOVE レセ－電療回数４８             TO 電療回数４８ＷＲ.
023350     MOVE レセ－電療料４８               TO 電療料４８ＷＲ.
023360     MOVE レセ－小計４８                 TO 小計４８ＷＲ.
023370     MOVE レセ－多部位込小計４８         TO 多部位込小計４８ＷＲ.
           IF レセ－長期頻回逓減率４８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率４８   TO 長期逓減率４８ＷＲ
           ELSE
024670         MOVE レセ－長期逓減率４８       TO 長期逓減率４８ＷＲ
           END-IF.
023390     MOVE レセ－長期込小計４８           TO 長期込小計４８ＷＲ.
023400*    ****************
023410*    * ４部位／10割 *
023420*    ****************
023430     MOVE レセ－逓減開始月４０           TO 逓減開始月４０ＷＲ.
023440     MOVE レセ－逓減開始日４０           TO 逓減開始日４０ＷＲ.
023450     MOVE レセ－後療単価４０             TO 後療単価４０ＷＲ.
023460     MOVE レセ－後療回数４０             TO 後療回数４０ＷＲ.
023470     MOVE レセ－後療料４０               TO 後療料４０ＷＲ.
023480     MOVE レセ－冷罨法回数４０           TO 冷罨法回数４０ＷＲ.
023490     MOVE レセ－冷罨法料４０             TO 冷罨法料４０ＷＲ.
023500     MOVE レセ－温罨法回数４０           TO 温罨法回数４０ＷＲ.
023510     MOVE レセ－温罨法料４０             TO 温罨法料４０ＷＲ.
023520     MOVE レセ－電療回数４０             TO 電療回数４０ＷＲ.
023530     MOVE レセ－電療料４０               TO 電療料４０ＷＲ.
023540     MOVE レセ－小計４０                 TO 小計４０ＷＲ.
           IF レセ－長期頻回逓減率４０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率４０   TO 長期逓減率４０ＷＲ
           ELSE
024840         MOVE レセ－長期逓減率４０       TO 長期逓減率４０ＷＲ
           END-IF.
023560     MOVE レセ－長期込小計４０           TO 長期込小計４０ＷＲ.
023570*    *****************
023580*    * ５部位／2.5割 *
023590*    *****************
023600     MOVE レセ－後療単価５２             TO 後療単価５２ＷＲ.
023610     MOVE レセ－後療回数５２             TO 後療回数５２ＷＲ.
023620     MOVE レセ－後療料５２               TO 後療料５２ＷＲ.
023630     MOVE レセ－冷罨法回数５２           TO 冷罨法回数５２ＷＲ.
023640     MOVE レセ－冷罨法料５２             TO 冷罨法料５２ＷＲ.
023650     MOVE レセ－温罨法回数５２           TO 温罨法回数５２ＷＲ.
023660     MOVE レセ－温罨法料５２             TO 温罨法料５２ＷＲ.
023670     MOVE レセ－電療回数５２             TO 電療回数５２ＷＲ.
023680     MOVE レセ－電療料５２               TO 電療料５２ＷＲ.
023690     MOVE レセ－小計５２                 TO 小計５２ＷＲ.
023700     MOVE レセ－多部位込小計５２         TO 多部位込小計５２ＷＲ.
023710     MOVE レセ－長期逓減率５２           TO 長期逓減率５２ＷＲ.
023720     MOVE レセ－長期込小計５２           TO 長期込小計５２ＷＲ.
023730*    ****************
023740*    * ５部位／５割 *
023750*    ****************
023760     MOVE レセ－逓減開始月５５           TO 逓減開始月５５ＷＲ.
023770     MOVE レセ－逓減開始日５５           TO 逓減開始日５５ＷＲ.
023780     MOVE レセ－後療単価５５             TO 後療単価５５ＷＲ.
023790     MOVE レセ－後療回数５５             TO 後療回数５５ＷＲ.
023800     MOVE レセ－後療料５５               TO 後療料５５ＷＲ.
023810     MOVE レセ－冷罨法回数５５           TO 冷罨法回数５５ＷＲ.
023820     MOVE レセ－冷罨法料５５             TO 冷罨法料５５ＷＲ.
023830     MOVE レセ－温罨法回数５５           TO 温罨法回数５５ＷＲ.
023840     MOVE レセ－温罨法料５５             TO 温罨法料５５ＷＲ.
023850     MOVE レセ－電療回数５５             TO 電療回数５５ＷＲ.
023860     MOVE レセ－電療料５５               TO 電療料５５ＷＲ.
023870     MOVE レセ－小計５５                 TO 小計５５ＷＲ.
023880     MOVE レセ－多部位込小計５５         TO 多部位込小計５５ＷＲ.
023890     MOVE レセ－長期逓減率５５           TO 長期逓減率５５ＷＲ.
023900     MOVE レセ－長期込小計５５           TO 長期込小計５５ＷＲ.
023910*    ****************
023920*    * ５部位／８割 *
023930*    ****************
023940     MOVE レセ－逓減開始月５８           TO 逓減開始月５８ＷＲ.
023950     MOVE レセ－逓減開始日５８           TO 逓減開始日５８ＷＲ.
023960     MOVE レセ－後療単価５８             TO 後療単価５８ＷＲ.
023970     MOVE レセ－後療回数５８             TO 後療回数５８ＷＲ.
023980     MOVE レセ－後療料５８               TO 後療料５８ＷＲ.
023990     MOVE レセ－冷罨法回数５８           TO 冷罨法回数５８ＷＲ.
024000     MOVE レセ－冷罨法料５８             TO 冷罨法料５８ＷＲ.
024010     MOVE レセ－温罨法回数５８           TO 温罨法回数５８ＷＲ.
024020     MOVE レセ－温罨法料５８             TO 温罨法料５８ＷＲ.
024030     MOVE レセ－電療回数５８             TO 電療回数５８ＷＲ.
024040     MOVE レセ－電療料５８               TO 電療料５８ＷＲ.
024050     MOVE レセ－小計５８                 TO 小計５８ＷＲ.
024060     MOVE レセ－多部位込小計５８         TO 多部位込小計５８ＷＲ.
           IF レセ－長期頻回逓減率５８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率５８   TO 長期逓減率５８ＷＲ
           ELSE
025360         MOVE レセ－長期逓減率５８       TO 長期逓減率５８ＷＲ
           END-IF.
024080     MOVE レセ－長期込小計５８           TO 長期込小計５８ＷＲ.
024090*    ****************
024100*    * ５部位／10割 *
024110*    ****************
024120     MOVE レセ－逓減開始月５０           TO 逓減開始月５０ＷＲ.
024130     MOVE レセ－逓減開始日５０           TO 逓減開始日５０ＷＲ.
024140     MOVE レセ－後療単価５０             TO 後療単価５０ＷＲ.
024150     MOVE レセ－後療回数５０             TO 後療回数５０ＷＲ.
024160     MOVE レセ－後療料５０               TO 後療料５０ＷＲ.
024170     MOVE レセ－冷罨法回数５０           TO 冷罨法回数５０ＷＲ.
024180     MOVE レセ－冷罨法料５０             TO 冷罨法料５０ＷＲ.
024190     MOVE レセ－温罨法回数５０           TO 温罨法回数５０ＷＲ.
024200     MOVE レセ－温罨法料５０             TO 温罨法料５０ＷＲ.
024210     MOVE レセ－電療回数５０             TO 電療回数５０ＷＲ.
024220     MOVE レセ－電療料５０               TO 電療料５０ＷＲ.
024230     MOVE レセ－小計５０                 TO 小計５０ＷＲ.
           IF レセ－長期頻回逓減率５０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率５０   TO 長期逓減率５０ＷＲ
           ELSE
025530         MOVE レセ－長期逓減率５０       TO 長期逓減率５０ＷＲ
           END-IF.
024250     MOVE レセ－長期込小計５０           TO 長期込小計５０ＷＲ.
      */2022
           MOVE レセ－明細書発行加算料         TO 明細書発行加算料ＷＲ.
           MOVE レセ－明細書発行加算日         TO 明細書発行加算日ＷＲ.
024260*
024270****************************************
024280* 適用欄「受給者負担額の内訳」のセット *
024290****************************************
024300*     PERFORM VARYING 回数ＣＮＴ FROM 1 BY 1
024310*             UNTIL ( 回数ＣＮＴ > 10 ) OR
024320*                   ( レセ－当日分負担額(回数ＣＮＴ) = ZERO )
024330*         EVALUATE 回数ＣＮＴ
024340*         WHEN 1
024350*             MOVE NC"①"                     TO 印刷負担回数Ｗ(回数ＣＮＴ)
024360*         WHEN 2
024370*             MOVE NC"②"                     TO 印刷負担回数Ｗ(回数ＣＮＴ)
024380*         WHEN 3
024390*             MOVE NC"③"                     TO 印刷負担回数Ｗ(回数ＣＮＴ)
024400*         WHEN 4
024410*             MOVE NC"④"                     TO 印刷負担回数Ｗ(回数ＣＮＴ)
024420*         WHEN 5
024430*             MOVE NC"⑤"                     TO 印刷負担回数Ｗ(回数ＣＮＴ)
024440*         WHEN 6
024450*             MOVE NC"⑥"                     TO 印刷負担回数Ｗ(回数ＣＮＴ)
024460*         WHEN 7
024470*             MOVE NC"⑦"                     TO 印刷負担回数Ｗ(回数ＣＮＴ)
024480*         WHEN 8
024490*             MOVE NC"⑧"                     TO 印刷負担回数Ｗ(回数ＣＮＴ)
024500*         WHEN 9
024510*             MOVE NC"⑨"                     TO 印刷負担回数Ｗ(回数ＣＮＴ)
024520*         WHEN 10
024530*             MOVE NC"⑩"                     TO 印刷負担回数Ｗ(回数ＣＮＴ)
024540*         END-EVALUATE
024550*         MOVE レセ－当日分負担額(回数ＣＮＴ) TO 当日分負担額Ｗ(回数ＣＮＴ)
024560*     END-PERFORM.
024570**
024580*================================================================*
024590 施術所情報取得 SECTION.
024600*
024610**************************************************
024620* 本院データを使用し、以下の情報を取得           *
024630* ● 柔整師番号.. 柔整師番号Ｗに格納             *
024640* ● 会員番号 ... 接骨師会会員番号Ｗに格納       *
024650* ● 代表者名 ... 代表者名Ｗに格納               *
024660* ● 住所1,2   ...施術所住所1,2Ｗに格納          *
024670* ● 電話番号 ... 施術所電話番号Ｗに格納         *
024680**************************************************
024690     MOVE ZERO  TO 施情－施術所番号.
024700     READ 施術所情報マスタ
024710     INVALID KEY
024720         CONTINUE
024730     NOT INVALID KEY
024740*
               MOVE 施情－都道府県ＪＩＳ     TO 都道府県ＪＩＳＷ
024320         MOVE 施情－新柔整師番号 TO 柔整師番号Ｗ
024750*         IF ( 施術和暦年月ＷＲ < 施情－開始和暦年月 )
024760*             PERFORM 柔整師全角数字取得
024770*         ELSE
024780*             PERFORM 新柔整師全角数字取得
024790*         END-IF
024800*
024810*         MOVE 施情－接骨師会会員番号  TO ＯＣＲ接骨師会会員番号Ｗ
024820*
024830         STRING "中央-"                DELIMITED BY SIZE
024840                施情－接骨師会会員番号 DELIMITED BY SPACE
024850           INTO 接骨師会会員番号Ｗ
024860         END-STRING
024870*
024880         MOVE 施情－郵便番号１        TO 施術所郵便番号１Ｗ
024890         MOVE 施情－郵便番号２        TO 施術所郵便番号２Ｗ
024900         MOVE 施情－代表者カナ        TO 代表者カナＷ
024910         MOVE 施情－代表者名          TO 代表者名Ｗ
024920*
024930         MOVE 施情－接骨院名          TO 接骨院名Ｗ
024940         MOVE 施情－住所１            TO 施術所住所１Ｗ
024950         MOVE 施情－住所２            TO 施術所住所２Ｗ
025000*
025010         MOVE 施情－電話番号          TO 施術所電話番号Ｗ
025020         MOVE 施情－都道府県ＪＩＳ    TO 都道府県Ｗ
025030**
025040** 振込先情報
025050** 東京都の障・親・乳・子・被爆で、JIS 東京(13)の時
      ** 千葉県の障・親・乳・子で、JIS 千葉(12)の時
      ** 長崎県の乳（長崎市を除く）で、JIS 長崎(42)の時
               IF (((( 助成種別ＷＲ = 53 ) AND ( 費用負担者番号助成ＷＲ(1:4) = "8013" )) OR
                    (( 助成種別ＷＲ = 52 ) AND ( 費用負担者番号助成ＷＲ(1:4) = "8113" )) OR
                    (( 助成種別ＷＲ = 54 ) AND ( 費用負担者番号助成ＷＲ(1:4) = "1913" )) OR
                    (( 助成種別ＷＲ = 60 OR 55 ) AND ( 費用負担者番号助成ＷＲ(1:4) = "8813" )) OR
                    (( 助成種別ＷＲ = 60 ) AND ( 費用負担者番号助成ＷＲ(1:4) = "8913" ))) AND
                   ( 施情－都道府県ＪＩＳ = "13" )) OR
                  (((( 助成種別ＷＲ = 60 OR 55 ) AND (費用負担者番号助成ＷＲ(1:4) = "8312" )) OR
                    (( 助成種別ＷＲ = 53 ) AND (費用負担者番号助成ＷＲ(1:4) = "8112")) OR
                    ((助成種別ＷＲ = 52) AND (費用負担者番号助成ＷＲ(1:4) =  "8512"))) AND
                   ( 施情－都道府県ＪＩＳ = "12" )) OR
                  ((((助成種別ＷＲ = 55) AND (費用負担者番号助成ＷＲ(1:4) =  "8042")) AND
                    (費用負担者番号助成ＷＲ NOT =  "80420011")) AND
                   ( 施情－都道府県ＪＩＳ = "42" ))
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
025130                 MOVE 会情－取引先銀行名      TO 取引先銀行名Ｗ
025140                 MOVE 会情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
025150                 MOVE 会情－預金種別          TO 預金種別Ｗ
025160*                 MOVE 会情－口座番号          TO 口座番号Ｗ
                       MOVE "6639305"               TO 口座番号Ｗ
024780                 MOVE 会情－口座名義人        TO 口座名義人Ｗ
025180                 MOVE 会情－口座名義人カナ    TO 口座名義人カナＷ
025370             END-IF
025380* 固定印字
025080             MOVE "また、私が取得した上記金額の受領を、" TO  コメント１Ｗ
      */令和５年８月提出分より理事長変更/20230628
025090*             MOVE "(協)中央接骨師会理事長 根岸 進"       TO  コメント２Ｗ
025090             MOVE "(協)中央接骨師会理事長 藤井 茂"       TO  コメント２Ｗ
025100             MOVE "(東京都板橋区成増2-9-5)に委任"        TO  コメント３Ｗ
025100             MOVE "します。"                             TO  コメント４Ｗ
025520*
025522*
025530** 以外
025540         ELSE
025550             MOVE 施情－取引先銀行名      TO 取引先銀行名Ｗ
025560             MOVE 施情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
025570             MOVE 施情－預金種別          TO 預金種別Ｗ
025580             MOVE 施情－口座番号          TO 口座番号Ｗ
025590             MOVE 施情－口座名義人        TO 口座名義人Ｗ
                   MOVE 施情－口座名義人カナ    TO 口座名義人カナＷ
025780         END-IF
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
025790*
025800     END-READ.
025810*
025820*********************************************
025830** ＩＤ管理マスタより　県施術ＩＤを取得する。
025840*********************************************
025850** 県施術ID
025860     MOVE 01                   TO ＩＤ管－ＩＤ区分
025870     MOVE ZERO                 TO ＩＤ管－施術所番号
025880     MOVE 費用負担者番号助成ＷＲ(3:2)  TO ＩＤ管－保険種別
025890     MOVE SPACE                TO ＩＤ管－保険者番号
025900     READ ＩＤ管理マスタ
025910     NOT INVALID KEY
025920          MOVE ＩＤ管－施術ＩＤ番号   TO 県施術ＩＤＷ
025930     END-READ.
025940*
025950*================================================================*
025960 受診者情報取得 SECTION.
025970*
025980**************************************************
025990* 連結データから受診者情報Ｆより以下の情報を取得 *
026000* ● 施術年 ..... 施術年Ｗに格納                 *
026010* ● 施術月 ..... 施術月Ｗに格納                 *
026020* ● 患者番号.... 患者番号Ｗに格納※ＦＤ連番用   *
026030* ● 記号 ....... 記号Ｗに格納                   *
026040* ● 番号 ....... 番号Ｗに格納                   *
026050* ● 保険者番号 . 保険者番号Ｗに格納             *
026060* ● 保険種別 ... 保険種別Ｗに格納               *
026070* ● 被保険者カナ.被保険者カナＷに格納           *
026080* ● 被保険者氏名.被保険者氏名Ｗに格納           *
026090* ● 住所１ ......被保険者住所１Ｗに格納         *
026100* ● 住所２ ......被保険者住所２Ｗに格納         *
026110* ● 患者カナ ....患者カナＷに格納               *
026120* ● 患者氏名 ....患者氏名Ｗに格納               *
026130* ● 患者性別 ....区分によりチェックに"○"を格納 *
026140* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
026150* ● 患者年 ......患者年Ｗに格納                 *
026160* ● 患者月 ......患者月Ｗに格納                 *
026170* ● 患者日 ......患者日Ｗに格納                 *
026180* ● 続柄 ........名称マスタより続柄Ｗに取得     *
026190**************************************************
026460     MOVE 施術和暦ＷＲ       TO 受２－施術和暦.
026470     MOVE 施術年ＷＲ         TO 受２－施術年.
026480     MOVE 施術月ＷＲ         TO 受２－施術月.
026490     MOVE 患者コードＷＲ     TO 受２－患者コード.
026500     READ 受診者情報２Ｆ
           INVALID KEY
              MOVE SPACE           TO 受２－レコード
           END-READ.
026200     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
026210     MOVE 施術年ＷＲ         TO 受－施術年.
026220     MOVE 施術月ＷＲ         TO 受－施術月.
026230     MOVE 患者コードＷＲ     TO 受－患者コード.
026240     READ 受診者情報Ｆ
026250     INVALID KEY
026260         CONTINUE
026270*            /* ありえない */
026280     NOT INVALID KEY
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
               EVALUATE 受２－助成本人家族区分
               WHEN 1
                   MOVE NC"○" TO 本人チェックＷ
                   MOVE SPACE  TO 家族チェックＷ
               WHEN 2
                   MOVE SPACE  TO 本人チェックＷ
                   MOVE NC"○" TO 家族チェックＷ
               END-EVALUATE
      */助成レセは必ず○付け
      *         IF ( 受－保険種別 = 01 OR 08) OR
      *            ((受－助成種別 = 54) AND (受－費用負担者番号助成(1:2) = "19"))
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
026290         MOVE 受－施術年       TO 施術年Ｗ
026300         MOVE 受－施術月       TO 施術月Ｗ
026310         MOVE 受－患者番号     TO 患者番号Ｗ
026320*         MOVE 受－記号         TO 記号Ｗ
026330*         MOVE 受－番号         TO 番号Ｗ
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
026340         MOVE 受－保険者番号   TO 保険者番号Ｗ
026350         MOVE 受－保険種別     TO 保険種別Ｗ
026360** 全国土木の枝番削除
026370         IF ( 受－保険種別 = 01 ) AND ( 受－保険者番号(1:6) = "133033" )
026380            MOVE 受－保険者番号(1:6)  TO 保険者番号Ｗ
026390         END-IF
026400**
026410         MOVE 受－被保険者カナ TO 被保険者カナＷ
026420         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
026450         MOVE 受－住所１       TO 被保険者住所１Ｗ
026460         MOVE 受－住所２       TO 被保険者住所２Ｗ
026470*         STRING 受－住所１   DELIMITED BY SPACE
026480*                受－住所２   DELIMITED BY SPACE
026490*                INTO 被保険者住所Ｗ
026500*         END-STRING
026510*         STRING 受－患者住所１   DELIMITED BY SPACE
026520*                受－患者住所２   DELIMITED BY SPACE
026530*                INTO 患者住所Ｗ
026540*         END-STRING
               MOVE 受－患者住所１ TO 患者住所１Ｗ
               MOVE 受－患者住所２ TO 患者住所２Ｗ
026550         MOVE 受－患者カナ     TO 患者カナＷ
026560         MOVE 受－患者氏名     TO 患者氏名Ｗ
      */ 郵便番号・電話番号追加 /42505
               IF 市町村番号Ｗ(3:2) = "23"
026430            MOVE 受－患者郵便番号１   TO 郵便番号１Ｗ
026440            MOVE 受－患者郵便番号２   TO 郵便番号２Ｗ
                  IF 受－患者電話番号 NOT = SPACE
                     STRING "電話:"            DELIMITED BY SIZE
                            受－患者電話番号   DELIMITED BY SPACE
                       INTO 電話番号Ｗ
                     END-STRING
                  END-IF
               ELSE
026430            MOVE 受－郵便番号１   TO 郵便番号１Ｗ
026440            MOVE 受－郵便番号２   TO 郵便番号２Ｗ
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
               END-IF
026570*
026580         MOVE 受－費用負担者番号助成 TO 市町村番号Ｗ
026590         MOVE 受－受益者番号助成     TO 受給者番号Ｗ
026600*
026610         EVALUATE 受－患者性別
026620         WHEN 1
026630             MOVE NC"男"  TO 性別Ｗ
026640             MOVE NC"○"  TO 男チェックＷ
026650         WHEN 2
026660             MOVE NC"女"  TO 性別Ｗ
026670             MOVE NC"○"  TO 女チェックＷ
026680         END-EVALUATE
026690*
026700         EVALUATE 受－患者和暦
026710         WHEN 1
026720             MOVE NC"明治"  TO 元号Ｗ
026730             MOVE NC"○"    TO 明治チェックＷ
026740         WHEN 2
026750             MOVE NC"大正"  TO 元号Ｗ
026760             MOVE NC"○"    TO 大正チェックＷ
026770         WHEN 3
026780             MOVE NC"昭和"  TO 元号Ｗ
026790             MOVE NC"○"    TO 昭和チェックＷ
026800         WHEN 4
026810             MOVE NC"平成"  TO 元号Ｗ
026820             MOVE NC"○"    TO 平成チェックＷ
      */元号修正/20190408
023060         WHEN 5
                   MOVE "5令"   TO 令和ＣＭＷ
023070             MOVE NC"○"  TO 令和チェックＷ
026830         END-EVALUATE
026840*
      */元号修正/↓↓↓20190408
029310         IF 受－患者和暦 > 4
037370             MOVE 受－患者和暦     TO 元－元号区分
037380             READ 元号マスタ
037390             NOT INVALID KEY
037400                 MOVE 元－元号名称 TO 元号Ｗ
037410             END-READ
029330         END-IF
      */元号修正/↑↑↑20190408
026850         MOVE 受－患者年  TO 患者年Ｗ
026860         MOVE 受－患者月  TO 患者月Ｗ
026870         MOVE 受－患者日  TO 患者日Ｗ
026880*** 親保険種別
026890*         EVALUATE 受－保険種別
026900*         WHEN 02
026910*             MOVE NC"○"       TO 政チェックＷ
026920*         WHEN 03
026930*             MOVE NC"○"       TO 組チェックＷ
026940*         WHEN 06
026950*             MOVE NC"○"       TO 日チェックＷ
026960*         WHEN 07
026970*             MOVE NC"○"       TO 船チェックＷ
026980*         WHEN 04
026990*         WHEN 09
027000*             MOVE NC"○"       TO 共チェックＷ
027010*         WHEN 01
027020*             MOVE NC"○"       TO 国チェックＷ
027030*         WHEN 08
027040*             MOVE NC"○"       TO 退チェックＷ
027030*         WHEN 05
027040*             MOVE NC"○"       TO 後高チェックＷ
027040*             MOVE NC"後"       TO 後高１Ｗ
027050*         WHEN OTHER
027060*             CONTINUE
027070*         END-EVALUATE
027080* 続柄なし
027090*         IF ( 本人家族区分ＷＲ = 1 )
027100*             MOVE SPACE       TO 続柄Ｗ
027110*         ELSE
027120*             MOVE 05          TO 名－区分コード
027130*             MOVE 受－続柄    TO 名－名称コード
027140*             READ 名称マスタ
027150*             INVALID KEY
027160*                 MOVE SPACE    TO 続柄Ｗ
027170*             NOT INVALID KEY
027180*                 MOVE 名－略称 TO 続柄Ｗ
027190*             END-READ
027200*         END-IF
027210*
027220**
027230*---  市町村独自仕様 -----*
027240* 14/10～　東京都のみ→ 特別区分1,2,3(高齢者）の時、「前」を右上に印字
027250*                       親が老人の時、保険者番号欄には、２７番号を印字
027260         IF ( 受－施術和暦年月 >= 41410 )
027270            IF ( 受－費用負担者番号助成(3:2) = "13" ) AND
027280               ( 都道府県Ｗ = "13" )
027290               IF ( 受－公費種別 = ZERO )
027300                  IF ( 受－特別区分 = 1 OR 2 OR 3)
027310                     MOVE NC"前" TO 特別マークＷ
027320                  END-IF
027330               ELSE
027340                  MOVE 受－費用負担者番号  TO 保険者番号Ｗ
027350               END-IF
027360            END-IF
027370         END-IF
027380*
027390* 14/10～　愛知県のみ→ 41老人の負担率を右上に印字
027400*         IF ( 受－施術和暦年月 >= 41410 )
027410*            IF ( 受－費用負担者番号助成(3:2) = "23" ) AND
027420*               ( 受－助成種別 = 51 ) AND ( 都道府県Ｗ = "23" )
027473*               EVALUATE 受－助成負担金免除
027474*               WHEN 2
027476*                  MOVE "41老人 ２割"   TO 特別コメントＷ
027477*               WHEN 3
027479*                  MOVE "41老人 ３割"   TO 特別コメントＷ
027480*               WHEN OTHER
027482*                  MOVE "41老人 １割"   TO 特別コメントＷ
027483*               END-EVALUATE
027486*            END-IF
027490*         END-IF
027500*
027510*
027520     END-READ.
027530*================================================================*
027540 請求先情報取得 SECTION.
027550*
027560****************************************************
027570* 連結データから保険者マスタより請求先を取得する。 *
027580* ※保－請求先情報区分=1の場合請求先マスタを使用   *
027590* ● 請求先...... 請求先名称Ｗに格納               *
027600* ※※※  親保険の保険者名称(請求先名称)をセットする!! *
027610********************************************************
027620     MOVE 保険種別ＷＲ   TO 保－保険種別.
027630     MOVE 保険者番号ＷＲ TO 保－保険者番号.
027640     READ 保険者マスタ
027650     INVALID KEY
               IF ( 保険種別ＷＲ = 05 ) AND ( 施術和暦年月ＷＲ >= 42004 )
030800             MOVE 保険種別ＷＲ   TO 市－公費種別
030810             MOVE 保険者番号ＷＲ TO 市－市町村番号
030820             READ 市町村マスタ
030830             INVALID KEY
030840                 MOVE SPACE      TO 請求先名称Ｗ
030850             NOT INVALID KEY
031330                 MOVE 市－市町村名称    TO 請求先名称Ｗ
                   END-READ
               ELSE
030840             MOVE SPACE      TO 請求先名称Ｗ
               END-IF
027670     NOT INVALID KEY
027680* 社保、日雇は「社会保険事務所」をつける
027690                 EVALUATE 保険種別ＷＲ 
027700                 WHEN  02
027710                 WHEN  06
027720                     IF ( 保－接尾語区分 = 1 )
027730                        MOVE 保－保険者名称    TO 請求先名称Ｗ
027740                     ELSE
027750                        STRING 保－保険者名称    DELIMITED BY SPACE
027760                               "社会保険事務所"  DELIMITED BY SIZE
027770                               INTO 請求先名称Ｗ
027780                        END-STRING
027790                     END-IF
027800** 組合は支部名まで印字
027810                 WHEN  03
027820                     STRING 保－保険者名称    DELIMITED BY SPACE
027830                           "健康保険組合"     DELIMITED BY SIZE
027840                            "  "              DELIMITED BY SIZE
027850                            保－支部部署名    DELIMITED BY SPACE
027860                            INTO 請求先名称Ｗ
027870                     END-STRING
027880** 共済は支部名まで印字
027890                 WHEN  04
027900                     STRING 保－保険者名称    DELIMITED BY SPACE
027910                           "共済組合"         DELIMITED BY SIZE
027920                            "  "              DELIMITED BY SIZE
027930                            保－支部部署名    DELIMITED BY SPACE
027940                            INTO 請求先名称Ｗ
027950                     END-STRING
027960                 WHEN OTHER
027970                     MOVE 保－保険者名称    TO 請求先名称Ｗ
027980                 END-EVALUATE
027990     END-READ.
028000*
028010****************************************************
028020*     MOVE 助成種別ＷＲ           TO 市－公費種別.
028030*     MOVE 市町村番号Ｗ           TO 市－市町村番号.
028040*
028050*     READ 市町村マスタ
028060*     INVALID KEY
028070*         MOVE SPACE              TO 請求先名称Ｗ
028080*     NOT INVALID KEY
028090*         IF ( 市－請求先区分 = 1 )
028100*             MOVE 助成種別ＷＲ     TO 請先－保険種別
028110*             MOVE 市町村番号Ｗ     TO 請先－保険者番号
028120*             READ 請求先マスタ
028130*             INVALID KEY
028140*                 MOVE SPACE        TO 請求先名称Ｗ
028150*             NOT INVALID KEY
028160*                 MOVE 請先－保険者名称  TO 請求先名称Ｗ
028170*             END-READ
028180*         ELSE
028190*             MOVE 市－市町村名称  TO 請求先名称Ｗ
028200*         END-IF
028210*     END-READ.
028220*
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
028230*================================================================*
028240 負傷データ取得 SECTION.
028250*
028260**************************************************
028270* 連結データから負傷データＦより以下の情報を取得 *
028280* ● 負傷名...部位＋負傷種別にて加工して格納     *
028290* ● 負傷年.......負傷年Ｗ                       *
028300* ● 負傷月.......負傷月Ｗ                       *
028310* ● 負傷日.......負傷日Ｗ                       *
028320* ● 開始年.......初検年Ｗ                       *
028330* ● 開始月.......初検月Ｗ                       *
028340* ● 開始日.......初検日Ｗ                       *
028350* ● 終了年.......終了年Ｗ                       *
028360* ● 終了月.......終了月Ｗ                       *
028370* ● 終了日.......終了日Ｗ                       *
028380* ● 実日数.......実日数Ｗ                       *
028390* ● 転帰区分 ....区分によりチェックに"○"を格納 *
028400* ● 金属副子 ....区分によりチェックに"○"を格納 *
028410* ● 経過コード...経過マスタより取得             *
028420**************************************************
028430*     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
028440*     MOVE 施術年ＷＲ         TO 負－施術年.
028450*     MOVE 施術月ＷＲ         TO 負－施術月.
028460*     MOVE 患者コードＷＲ     TO 負－患者コード.
028470*     READ 負傷データＦ
028480*     INVALID KEY
028490*         CONTINUE
028500**            /* ありえない */
028510*     NOT INVALID KEY
028520*         MOVE 負－部位数                   TO 部位数Ｗ
028530         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
028540                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
028550             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別Ｗ(部位ＣＮＴ)
028560             MOVE 負－部位(部位ＣＮＴ)     TO 部位Ｗ(部位ＣＮＴ)
028570             MOVE 負－左右区分(部位ＣＮＴ) TO 左右区分Ｗ(部位ＣＮＴ)
028580             MOVE 負－負傷位置番号(部位ＣＮＴ)
028590                                           TO 負傷位置番号Ｗ(部位ＣＮＴ)
028600*********************************************
028610* 注）全柔...負傷種別＋部位にて加工して格納 *
028620*********************************************
028630* 負傷種別
028640             MOVE SPACE                     TO 負傷名称Ｗ
028650             MOVE 03                        TO 名－区分コード
028660             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
028670             READ 名称マスタ
028680             INVALID KEY
028690                 MOVE SPACE        TO 負傷名称Ｗ
028700             NOT INVALID KEY
028710                 MOVE 名－正式名称 TO 負傷名称Ｗ
028720             END-READ
028730* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
028160*
028170             PERFORM 部位名称埋込処理
028920*
028930             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
028940             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
028950             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
028960             MOVE 負－開始年(部位ＣＮＴ)   TO 初検年Ｗ(部位ＣＮＴ)
028970             MOVE 負－開始月(部位ＣＮＴ)   TO 初検月Ｗ(部位ＣＮＴ)
028980             MOVE 負－開始日(部位ＣＮＴ)   TO 初検日Ｗ(部位ＣＮＴ)
028990             IF ( 負－転帰区分(部位ＣＮＴ) = 9 )
029000                 MOVE 99                   TO 終了年Ｗ(部位ＣＮＴ)
029010                 MOVE 99                   TO 終了月Ｗ(部位ＣＮＴ)
029020                 MOVE 99                   TO 終了日Ｗ(部位ＣＮＴ)
029030             ELSE
029040                 MOVE 負－終了年(部位ＣＮＴ)   TO 終了年Ｗ(部位ＣＮＴ)
029050                 MOVE 負－終了月(部位ＣＮＴ)   TO 終了月Ｗ(部位ＣＮＴ)
029060                 MOVE 負－終了日(部位ＣＮＴ)   TO 終了日Ｗ(部位ＣＮＴ)
029070             END-IF
029080* 経過略称取得
029090             MOVE 01                         TO 経－区分コード
029100             MOVE 負－経過コード(部位ＣＮＴ) TO 経－経過コード
029110             READ 経過マスタ
029120             INVALID KEY
029130                 MOVE ZERO            TO 部位ＣＮＴＷ(部位ＣＮＴ)
029140                 MOVE SPACE           TO 部位区切Ｗ(部位ＣＮＴ)
029150                 MOVE SPACE           TO 経過略称Ｗ(部位ＣＮＴ)
029160             NOT INVALID KEY
029170*
029180                 EVALUATE 部位ＣＮＴ
029190                 WHEN 1
029200                     MOVE NC"①" TO 経過部位Ｗ
029210                 WHEN 2
029220                     MOVE NC"②" TO 経過部位Ｗ
029230                 WHEN 3
029240                     MOVE NC"③" TO 経過部位Ｗ
029250                 WHEN 4
029260                     MOVE NC"④" TO 経過部位Ｗ
029270                 WHEN 5
029280                     MOVE NC"⑤" TO 経過部位Ｗ
029290                 END-EVALUATE
029300                 STRING  経過部位Ｗ     DELIMITED BY SPACE
029310                         経－経過略称   DELIMITED BY SPACE
029320                        INTO 印刷経過略称Ｗ(部位ＣＮＴ)
029330                 END-STRING
029340*
029350             END-READ
029360*
029370             MOVE 負－転帰区分(部位ＣＮＴ) TO 転帰区分Ｗ(部位ＣＮＴ)
029380             EVALUATE 負－転帰区分(部位ＣＮＴ)
029390             WHEN 1
029400             WHEN 2
029410                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
029420             WHEN 3
029430                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
029440             WHEN 4
029450                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
029460             END-EVALUATE
029470*
      */実日数はレセ－部位実日数を転記する/160816
031230             MOVE レセ－部位実日数(部位ＣＮＴ) TO 実日数Ｗ(部位ＣＮＴ)
      */長期頻回20241007/
                   MOVE レセ－部位継続月数(部位ＣＮＴ) TO 部位継続月数Ｗ(部位ＣＮＴ)
029480         END-PERFORM.
029490* 新規/継続 チェック
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
029550* 枝番判定用
029560         MOVE 負－開始診療日手動区分 TO  開始診療日手動区分Ｗ.
029570*
029580* 負傷原因印刷区分
029590         MOVE 負－レセ負傷原因印刷区分 TO レセ負傷原因印刷区分Ｗ.
028370* 長期理由印刷区分
027880         MOVE 負－レセ長期理由印刷区分 TO 長期理由印刷区分Ｆ.
029600*
029610*     END-READ.
029620*================================================================*
029630*================================================================*
029640 施術記録取得 SECTION.
029650*
029660************************************************************
029670* 作１データから負傷データＦより以下の情報を取得           *
029680* ● 初検加算 .....区分によりチェックに"○"を格納...複数可 *
029690* ● 往療加算 .....区分によりチェックに"○"を格納...複数可 *
029700************************************************************
029710     MOVE  SPACE  TO  初日再検フラグ.
029720     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 部位数Ｗ
029730         IF ( 施術年Ｗ = 初検年Ｗ(部位ＣＮＴ) ) AND
029740            ( 施術月Ｗ = 初検月Ｗ(部位ＣＮＴ) )
029750             MOVE 患者番号ＷＲ          TO 施記－患者番号
029760             MOVE 枝番ＷＲ              TO 施記－枝番
029770             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
029780             MOVE 初検年Ｗ(部位ＣＮＴ)  TO 開始年Ｗ(部位ＣＮＴ) 施記－施術年
029790             MOVE 初検月Ｗ(部位ＣＮＴ)  TO 開始月Ｗ(部位ＣＮＴ) 施記－施術月
029800             MOVE 初検日Ｗ(部位ＣＮＴ)  TO 開始日Ｗ(部位ＣＮＴ) 施記－施術日
029810         ELSE
029820             MOVE 患者番号ＷＲ          TO 施記－患者番号
029830             MOVE 枝番ＷＲ              TO 施記－枝番
029840             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
029850             MOVE 施術年ＷＲ            TO 施記－施術年
029860             MOVE 施術月ＷＲ            TO 施記－施術月
029870             MOVE ZERO                  TO 施記－施術日
029880         END-IF
      *------------------------------------------------------------------------*
               IF ( 連レ－保険種別 > 50 ) AND ( レセ－助成月途中対象 = 1 )
                  IF 開始日Ｗ(部位ＣＮＴ) < 受－助成月途中開始日
                     MOVE 受－助成月途中開始日  TO  開始日Ｗ(部位ＣＮＴ) 施記－施術日
                  END-IF
               END-IF
      *------------------------------------------------------------------------*
029890         START 施術記録Ｆ   KEY IS >= 施記－患者コード
029900                                      施記－施術和暦年月日
029910         END-START
029920         IF ( 状態キー = "00" )
      */実日数はレセ－部位実日数を転記する/160816
029930*             MOVE ZERO  TO 実日数Ｗ(部位ＣＮＴ)
029940             MOVE ZERO  TO 終了年ＷＴ
029950             MOVE ZERO  TO 終了月ＷＴ
029960             MOVE ZERO  TO 終了日ＷＴ
029970             MOVE SPACE TO 終了フラグ２
029980             PERFORM 施術記録Ｆ読込
029990             IF ( 終了フラグ２      = SPACE   ) AND
030000                ( 施記－患者コード  = 患者コードＷＲ ) AND
030010                ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
030020                ( 施記－施術年      = 施術年ＷＲ     ) AND
030030                ( 施記－施術月      = 施術月ＷＲ     ) 
030040*
030050*        *****************************************************************
030060*        * 開始年月日 ( その部位が当月初検でないか、
030070*                       当月初検でも枝番がある時は、最初の施術日を開始日)*
030080*        *****************************************************************
030090                 IF ( 施術年Ｗ NOT = 初検年Ｗ(部位ＣＮＴ) ) OR
030100                    ( 施術月Ｗ NOT = 初検月Ｗ(部位ＣＮＴ) ) OR
030110                    ( 開始診療日手動区分Ｗ = 1 )
030120                     MOVE 施記－施術年   TO 開始年Ｗ(部位ＣＮＴ)
030130                     MOVE 施記－施術月   TO 開始月Ｗ(部位ＣＮＴ)
030140                     MOVE 施記－施術日   TO 開始日Ｗ(部位ＣＮＴ)
030150                 END-IF
030160             END-IF
030170             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
030180                           ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
030190                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
030200                           ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
030210                           ( 施記－施術月     NOT = 施術月ＷＲ       ) OR
030220                           ( 施記－施術日         > 終了日Ｗ(部位ＣＮＴ))
030230*               **********
030240*               * 実日数 *
030250*               **********
      */実日数はレセ－部位実日数を転記する/160816
030260*                COMPUTE 実日数Ｗ(部位ＣＮＴ) = 実日数Ｗ(部位ＣＮＴ) + 1
030270                MOVE 施記－施術年               TO 終了年ＷＴ
030280                MOVE 施記－施術月               TO 終了月ＷＴ
030290                MOVE 施記－施術日               TO 終了日ＷＴ
030300*
030310                PERFORM 施術記録Ｆ読込
030320            END-PERFORM
030330        END-IF
030340*       **************************
030350*       * 継続：終了年月日セット *
030360*       **************************
030370        IF ( 転帰区分Ｗ(部位ＣＮＴ) = 9 )
030380            MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
030390            MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
030400            MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
030410        END-IF
030420        IF ( 終了年月日Ｗ(部位ＣＮＴ) > 受理年月日Ｗ )
030430            MOVE 終了年Ｗ(部位ＣＮＴ) TO 受理年Ｗ
030440            MOVE 終了月Ｗ(部位ＣＮＴ) TO 受理月Ｗ
030450            MOVE 終了日Ｗ(部位ＣＮＴ) TO 受理日Ｗ
030460        END-IF
030470     END-PERFORM.
030480*
030490** ----- 前月初検のみかを判定 -----------*
030500*
030510*     MOVE 患者番号ＷＲ          TO 施記－患者番号.
030520*     MOVE 枝番ＷＲ              TO 施記－枝番.
030530*     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
030540*     MOVE 施術年ＷＲ            TO 施記－施術年.
030550*     MOVE 施術月ＷＲ            TO 施記－施術月.
030560*     MOVE ZERO                  TO 施記－施術日.
030570*     START 施術記録Ｆ   KEY IS >= 施記－患者コード
030580*                                  施記－施術和暦年月日
030590*     END-START.
030600*     IF ( 状態キー = "00" )
030610*             MOVE SPACE TO 終了フラグ２
030620*             PERFORM 施術記録Ｆ読込
030630*             IF ( 終了フラグ２      = SPACE   ) AND
030640*                ( 施記－患者コード  = 患者コードＷＲ ) AND
030650*                ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
030660*                ( 施記－施術年      = 施術年ＷＲ     ) AND
030670*                ( 施記－施術月      = 施術月ＷＲ     ) 
030680** 当月施術開始日が再検かどうか判定
030690*                 IF ( 施記－再検料請求 = 1 )
030700*                      MOVE "YES"  TO  初日再検フラグ
030710*                 END-IF
030720**
030730*             END-IF
030740*     END-IF.
030750*     IF ( 初日再検フラグ = "YES" )
030760*        PERFORM 前月初検のみ判定
030770*     END-IF.
030780*
030790*================================================================*
030800*================================================================*
030810 レセプト並び順取得 SECTION.
030820*================================================================*
030830     MOVE 施術和暦ＷＲ       TO 作４－施術和暦.
030840     MOVE 施術年ＷＲ         TO 作４－施術年.
030850     MOVE 施術月ＷＲ         TO 作４－施術月.
030860     MOVE 患者コードＷＲ     TO 作４－患者コード.
030870     MOVE 助成種別ＷＲ       TO 作４－保険種別.
030880     READ 作業ファイル４
030890     NOT INVALID KEY
030900          MOVE NC"№"        TO 順番固定Ｗ
030910          MOVE 作４－順番    TO 順番Ｗ
030920     END-READ.
030930*
030940*================================================================*
030950*================================================================*
030960* ＯＣＲ情報取得 SECTION.
030970*
030980****************************************************************
030990* ●     接骨師会会員番号 .....施術所情報マスタより既に取得    *
031000* ●     施術年月         .....受診者情報Ｆより既に取得        *
031010* ● 　　保険種別         .....全柔用国保コード42をセット    　*
031020*                              新規マスタにより対応の可能性有り*
031030* ●     指定各社ＩＤ     .....自社ＩＤ"21"をセット            *
031040* ●     ＦＤ連番         .....資料参照                        *
031050* ● 　　請求金額         .....レセ－請求金額よりセット        *
031060* ●　　 負担割合         .....負担割合 = レセ－負担割合 / 10　*
031070****************************************************************
031080* 接骨師会会員番号
031090*     MOVE ＯＣＲ接骨師会会員番号Ｗ TO ＯＣＲ会員番号Ｗ.
031100* 施術年月
031110*     MOVE 施術年月Ｗ         TO ＯＣＲ施術年月Ｗ.
031120* 保険種別 = 全柔コード = 42
031130*     MOVE 42                 TO ＯＣＲ保険種別Ｗ.
031140* 指定各社ＩＤ = 自社ＩＤ = 21
031150*     MOVE "21"               TO ＯＣＲ各社ＩＤＷ.
031160* ＦＤ連番 = ＦＤ連番患者番号Ｗ + ＦＤ連番健保ＩＤＷ
031170*     MOVE 患者番号Ｗ         TO ＦＤ連番患者番号Ｗ.
031180*     MOVE 1                  TO ＦＤ連番健保ＩＤＷ.
031190* 請求金額
031200*     MOVE レセ－請求額       TO ＯＣＲ請求金額Ｗ.
031210* 負担割合
031220*     MOVE レセ－負担率     TO 負担割合ＷＲ.
031230*     COMPUTE ＯＣＲ負担割合Ｗ = 負担割合ＷＲ / 10.
031240*================================================================*
031250 施術記録Ｆ読込 SECTION.
031260*
031270     READ 施術記録Ｆ NEXT
031280     AT END
031290         MOVE "YES" TO 終了フラグ２
031300     END-READ.
031310*================================================================*
031320 印刷処理 SECTION.
031330*
031340     MOVE "YCH6427P" TO  定義体名Ｐ.
031350     MOVE "SCREEN"   TO  項目群名Ｐ.
031360     WRITE YCH6427P.
031370***     WRITE 印刷レコード.
031380     PERFORM エラー処理Ｐ.
031390*================================================================*
031400 エラー処理Ｐ SECTION.
031410*
031420     IF ( 通知情報Ｐ NOT = "00" )
031430         DISPLAY NC"帳票エラー"              UPON CONS
031440         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
031450         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
031460         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
031470         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
031480                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
031490         ACCEPT  キー入力 FROM CONS
031500         PERFORM ファイル閉鎖
031510         MOVE 99 TO PROGRAM-STATUS
031520         EXIT PROGRAM
031530     END-IF.
031540*================================================================*
031550 部位名称埋込処理 SECTION.
031560*
006490     STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
009980            負傷名称Ｗ                    DELIMITED BY SPACE
006500            レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
006520       INTO 負傷名Ｗ(部位ＣＮＴ)
006570     END-STRING.
031760*
031770*================================================================*
031780 初検日以前のデータ判定 SECTION.
031790*
031800*********************************************************************************
031810*  最初の初検日以前の当月中に施術記録レコードがあった時(治癒、中止)は、請求区分の
031820*  継続にもチェックする。(新規と継続の両方)
031830*********************************************************************************
031840** 最初の初検日を取得
031850     MOVE SPACE                 TO 初検フラグ.
031860     MOVE 患者番号ＷＲ          TO 施記－患者番号.
031870     MOVE 枝番ＷＲ              TO 施記－枝番.
031880     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
031890     MOVE 施術年ＷＲ            TO 施記－施術年.
031900     MOVE 施術月ＷＲ            TO 施記－施術月.
031910     MOVE ZERO                  TO 施記－施術日.
031920     START 施術記録Ｆ   KEY IS >= 施記－患者コード
031930                                  施記－施術和暦年月日
031940     END-START.
031950     IF ( 状態キー = "00" )
031960         MOVE ZERO  TO 初検和暦ＷＴ
031970         MOVE ZERO  TO 初検年ＷＴ
031980         MOVE ZERO  TO 初検月ＷＴ
031990         MOVE ZERO  TO 初検日ＷＴ
032000         MOVE SPACE TO 終了フラグ２
032010         PERFORM 施術記録Ｆ読込
032020         PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
032030                       ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
032040                       ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
032050                       ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
032060                       ( 施記－施術月     NOT = 施術月ＷＲ      ) OR
032070                       ( 初検フラグ           = "YES"           ) 
032080               IF ( 施記－診療区分 = 2 )
032090                   MOVE 施記－施術和暦           TO 初検和暦ＷＴ
032100                   MOVE 施記－施術年             TO 初検年ＷＴ
032110                   MOVE 施記－施術月             TO 初検月ＷＴ
032120                   MOVE 施記－施術日             TO 初検日ＷＴ
032130                   MOVE "YES"                    TO 初検フラグ
032140               END-IF
032150               PERFORM 施術記録Ｆ読込
032160         END-PERFORM
032170     END-IF.
032180*
032190* 初検日以前のデータ判定
032200     IF ( 初検フラグ = "YES" )
032210        MOVE 患者番号ＷＲ          TO 施記－患者番号
032220        MOVE 枝番ＷＲ              TO 施記－枝番
032230        MOVE 初検和暦ＷＴ          TO 施記－施術和暦
032240        MOVE 初検年ＷＴ            TO 施記－施術年
032250        MOVE 初検月ＷＴ            TO 施記－施術月
032260        MOVE 初検日ＷＴ            TO 施記－施術日
032270        START 施術記録Ｆ   KEY IS <  施記－患者コード
032280                                     施記－施術和暦年月日
032290                                     REVERSED
032300        END-START
032310        IF ( 状態キー = "00" )
032320           MOVE SPACE  TO 終了フラグ２
032330           PERFORM 施術記録Ｆ読込
032340           IF ( 終了フラグ２    = SPACE        ) AND
032350              ( 施記－患者番号  = 患者番号ＷＲ ) AND
032360              ( 施記－枝番      = 枝番ＷＲ     ) AND
032370              ( 施記－施術和暦  = 初検和暦ＷＴ ) AND
032380              ( 施記－施術年    = 初検年ＷＴ   ) AND
032390              ( 施記－施術月    = 初検月ＷＴ   )
032400*  初検日以前の当月中に施術記録レコードがあった時
032410                IF ( 継続チェックＷ = SPACE )
032420                   MOVE NC"○"    TO 継続チェックＷ
032430                END-IF
032440           END-IF
032450         END-IF
032460     END-IF.
032470*
032480*================================================================*
032490 長期判定取得 SECTION.
032500*
032510* ３カ月以上の長期判定は "CHOUKI" を呼ぶ. 
032520     MOVE  SPACE TO  連期間－キー.
032530     INITIALIZE      連期間－キー.
032540     MOVE 施術和暦ＷＲ  TO  連期間－施術和暦.
032550     MOVE 施術年ＷＲ    TO  連期間－施術年.
032560     MOVE 施術月ＷＲ    TO  連期間－施術月.
032570     MOVE 患者番号ＷＲ  TO  連期間－患者番号.
032580     MOVE 枝番ＷＲ      TO  連期間－枝番.
032590*
032600     CALL   "CHOUKI".
032610     CANCEL "CHOUKI".
032620*
032630**** 適用１を使用 (「前月初検のみ」がある時は、くっつける)
032640     IF ( 連期間－対象フラグ  = "YES" )
032650        IF ( 適用１Ｗ  = SPACE )
032660           MOVE NC"※長期施術継続理由裏面に記載"  TO 適用１Ｗ
032670        ELSE
032680           STRING 適用１Ｗ           DELIMITED BY SPACE
032690                  NC"，"             DELIMITED BY SIZE
032700                  NC"※長期施術継続理由裏面に記載"   DELIMITED BY SIZE
032710                  INTO 適用１Ｗ
032720           END-STRING
032730        END-IF
032740     END-IF.
032750*
032760*================================================================*
032770 初検加算時刻取得 SECTION.
032780*****************************************************************
032790** 初検加算が時間外と深夜の時、適用に「受付時間」を印字する。
032800**   時刻の印字は月3回まで可能
032810*****************************************************************
032820     IF ( レセ－時間外 = 1 ) OR ( レセ－深夜 = 1 ) OR ( レセ－休日 = 1 )
032830*
032840         MOVE 患者番号ＷＲ          TO 施記－患者番号
032850         MOVE 枝番ＷＲ              TO 施記－枝番
032860         MOVE 施術和暦ＷＲ          TO 施記－施術和暦
032870         MOVE 施術年ＷＲ            TO 施記－施術年
032880         MOVE 施術月ＷＲ            TO 施記－施術月
032890         MOVE ZERO                  TO 施記－施術日
032900         START 施術記録Ｆ   KEY IS >= 施記－患者コード
032910                                      施記－施術和暦年月日
032920         END-START
032930         IF ( 状態キー = "00" )
032940             MOVE ZERO  TO 初検加算カウント
032950             MOVE SPACE TO 終了フラグ２
032960             PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
032970                           ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
032980                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
032990                           ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
033000                           ( 施記－施術月     NOT = 施術月ＷＲ      ) 
033010               IF ( 施記－初検加算 = 1 OR 2 OR 3 ) AND ( 施記－診療区分 = 2 )
033020                  COMPUTE 初検加算カウント = 初検加算カウント  + 1
033030                  IF ( 初検加算カウント <= 3 )
033040                     MOVE 施記－初検加算 TO 初検加算区分ＷＴ(初検加算カウント)
033050                     MOVE 施記－受付時   TO 初検加算時ＷＴ(初検加算カウント)
033060                     MOVE 施記－受付分   TO 初検加算分ＷＴ(初検加算カウント)
033070                  END-IF
033080               END-IF
033090               PERFORM 施術記録Ｆ読込
033100            END-PERFORM
033110** 初検加算の時刻を適用にセット
033380            IF ( 初検加算時ＷＴ(1) NOT = ZERO ) OR ( 初検加算分ＷＴ(1) NOT = ZERO ) 
                      MOVE 初検加算時ＷＴ(1) TO 初検加算時Ｗ
                      MOVE ":"               TO 初検加算区切Ｗ
                      MOVE 初検加算分ＷＴ(1) TO 初検加算分Ｗ
                  END-IF
033380            IF ( 初検加算時ＷＴ(2) NOT = ZERO ) OR ( 初検加算分ＷＴ(2) NOT = ZERO ) 
031910                PERFORM 初検加算適用セット
                  END-IF
033130         END-IF
033140*
033150     END-IF.
033160*
033170*================================================================*
033180 初検加算適用セット SECTION.
033190*
033200     PERFORM VARYING 番号カウンタ FROM 1 BY 1
033210              UNTIL  番号カウンタ > 3
033220         IF ( 初検加算時ＷＴ(番号カウンタ)  = ZERO )  AND 
033230            ( 初検加算分ＷＴ(番号カウンタ)  = ZERO ) 
033240             CONTINUE
033250         ELSE
033260* 固定項目
033270             EVALUATE 初検加算区分ＷＴ(番号カウンタ) 
033280             WHEN 1
033290                MOVE NC"時間外"   TO 加算内容Ｗ(番号カウンタ)
033320             WHEN 2
033330                MOVE NC"休　日"   TO 加算内容Ｗ(番号カウンタ)
033300             WHEN 3
033310                MOVE NC"深　夜"   TO 加算内容Ｗ(番号カウンタ)
033320             END-EVALUATE
033330*
033340             MOVE NC"："          TO 加算区切Ｗ(番号カウンタ)
033350             MOVE NC"時"          TO 時固定Ｗ(番号カウンタ)
033360             MOVE NC"分"          TO 分固定Ｗ(番号カウンタ)
033370*
033380**** 数字→日本語変換
033390* 時間
033400             MOVE 初検加算時ＷＴ(番号カウンタ)  TO  数字Ｗ
033410             IF ( 数字Ｗ >= 10 )
033420                 MOVE 数字Ｗ１    TO 負傷番号Ｗ１
033430                 PERFORM 日本語変換
033440                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ１(番号カウンタ)
033450                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
033460                 PERFORM 日本語変換
033470                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
033480             ELSE
033490                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
033500                 PERFORM 日本語変換
033510                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
033520             END-IF
033530* 分
033540             MOVE 初検加算分ＷＴ(番号カウンタ)  TO  数字Ｗ
033550             MOVE 数字Ｗ１    TO 負傷番号Ｗ１
033560             PERFORM 日本語変換
033570             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ１(番号カウンタ)
033580             MOVE 数字Ｗ２    TO 負傷番号Ｗ１
033590             PERFORM 日本語変換
033600             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ２(番号カウンタ)
033610** 
033620        END-IF
033630     END-PERFORM.
033640*
033650     MOVE  初検加算集団ＮＷ(1)   TO 初検加算時刻１Ｗ. 
033660     MOVE  初検加算集団ＮＷ(2)   TO 初検加算時刻２Ｗ. 
033670     MOVE  初検加算集団ＮＷ(3)   TO 初検加算時刻３Ｗ. 
033680*
033690**** 適用１か２を使用（長期理由記載で適用１を使っている時は、適用２）
033700     IF ( 初検加算時ＷＴ(2)  = ZERO ) AND ( 初検加算分ＷＴ(2)  = ZERO ) 
033710         CONTINUE
033720     ELSE
033730         IF ( 適用１Ｗ  = SPACE )
033740               STRING NC"初検加算"       DELIMITED BY SIZE
033750                      初検加算時刻１Ｗ   DELIMITED BY SIZE
033760                      初検加算時刻２Ｗ   DELIMITED BY SIZE
033770                      初検加算時刻３Ｗ   DELIMITED BY SIZE
033780                      INTO 適用１Ｗ
033790               END-STRING
033800         ELSE
033810               STRING NC"初検加算"       DELIMITED BY SIZE
033820                      初検加算時刻１Ｗ   DELIMITED BY SIZE
033830                      初検加算時刻２Ｗ   DELIMITED BY SIZE
033840                      初検加算時刻３Ｗ   DELIMITED BY SIZE
033850                      INTO 適用２Ｗ
033860               END-STRING
033870         END-IF
033880     END-IF.
033890*
033900*================================================================*
033910 日本語変換 SECTION.
033920*
033930     MOVE NC"０"     TO 全角負傷番号Ｗ.
033940     CALL "htoz" WITH C LINKAGE
033950                        USING 負傷番号Ｗ１ 全角負傷番号Ｗ１.
033960*
033970*================================================================*
033980*================================================================*
033990 負傷原因取得 SECTION.
034000*
034010********************************************************************
034020*  負傷原因コードが同じものは、1行にまとめて印字する。
034030*  例: ①② 家で転んだ.
034040*     負傷原因コードが同じものをまとめ、テーブルにセット
034050*     (ただし、部位を飛んで同じものは、2行になる)
034060********************************************************************
034070     MOVE  ZERO   TO  カウンタ カウンタ２.
034080     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
034090             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
034100*
034110****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
034120        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
034130*
034140           IF ( カウンタ = ZERO )
034150               MOVE 1   TO  カウンタ カウンタ２
034160               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
034170               MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
034180               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
034190           ELSE
034200              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
034210                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
034220                 COMPUTE カウンタ２ = カウンタ２  +  1
034230                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
034240              ELSE
034250                 COMPUTE カウンタ = カウンタ  +  1
034260                 MOVE 1   TO  カウンタ２
034270                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
034280                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
034290                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
034300              END-IF
034310           END-IF
034320        END-IF
034330     END-PERFORM.
034340**************************************************************************
034350*  負傷原因マスタより文章取得
034360**************************************************************************
034370     MOVE  ZERO   TO  カウンタ カウンタ２.
034380     PERFORM VARYING カウンタ FROM 1 BY 1
034390             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
034400** 健保は 区分 01
034410         MOVE 01                        TO 負原－区分コード
034420         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
034430         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
034440         READ 負傷原因Ｆ
034450         NOT INVALID KEY
034460             INITIALIZE 負傷原因ＷＴ
034470             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
034480             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
034490             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
034500             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
034510             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
034520             PERFORM VARYING カウンタ２ FROM 1 BY 1
034530                     UNTIL ( カウンタ２ > 9 )  OR 
034540                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
034550                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
034560                WHEN 1
034570                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
034580                WHEN 2
034590                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
034600                WHEN 3
034610                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
034620                WHEN 4
034630                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
034640                WHEN 5
034650                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
034620                WHEN 6
034630                   MOVE "⑥"  TO  負傷原因ナンバーＷ１(カウンタ２)
034640                WHEN 7
034650                   MOVE "⑦"  TO  負傷原因ナンバーＷ１(カウンタ２)
034660                WHEN OTHER
034670                   CONTINUE
034680                END-EVALUATE
034690             END-PERFORM
034700*
034782             IF 負原－負傷原因入力区分 = 1
034783                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
034784                        負傷原因１ＷＴ  DELIMITED BY SIZE
034785                        負傷原因２ＷＴ  DELIMITED BY SIZE
034786                        負傷原因３ＷＴ  DELIMITED BY SIZE
034787                        負傷原因４ＷＴ  DELIMITED BY SIZE
034788                        負傷原因５ＷＴ  DELIMITED BY SIZE
034789                        INTO 負傷原因内容合成Ｗ(カウンタ)
034790                 END-STRING
034791             ELSE
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
034800             END-IF
034801*
034802         END-READ
034803     END-PERFORM.
034810*
034820     PERFORM 負傷原因セット.
034830*
034840*================================================================*
034850 負傷原因セット SECTION.
034860*
034870**************************************************************************
034880*  文章が1行を超える時は、複数行に分解する。
034890**************************************************************************
034900     MOVE  ZERO   TO  カウンタ カウンタ２.
034910     PERFORM VARYING カウンタ FROM 1 BY 1
034920             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
034930*
034940          INITIALIZE 負傷原因内容分解ＸＷ
034950          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
034960          IF ( 負傷原因内容１ＸＷ  NOT = SPACE )
034970              COMPUTE カウンタ２ = カウンタ２  +  1
034980              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
034990          END-IF
035000          IF ( 負傷原因内容２ＸＷ  NOT = SPACE )
035010              COMPUTE カウンタ２ = カウンタ２  +  1
035020              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
035030          END-IF
035040          IF ( 負傷原因内容３ＸＷ  NOT = SPACE )
035050              COMPUTE カウンタ２ = カウンタ２  +  1
035060              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
035070          END-IF
033830          IF ( 負傷原因内容４ＸＷ  NOT = SPACE )
033840              COMPUTE カウンタ２ = カウンタ２  +  1
033850              MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
033860          END-IF
035080*
035090     END-PERFORM.
035100*================================================================*
035110 助成印取得 SECTION.
035120*
035130* 2006/04 変更
035140* 助成印は "JOSEIMEI" を呼ぶ. 
035150     MOVE SPACE TO  連助成名称－キー.
035160     INITIALIZE     連助成名称－キー.
035170     MOVE 助成種別ＷＲ           TO 連助成名称－助成種別.
035180     MOVE 費用負担者番号助成ＷＲ TO 連助成名称－費用負担者番号助成.
035190*
035200     CALL   "JOSEIMEI".
035210     CANCEL "JOSEIMEI".
035220*
035230     MOVE 連助成名称－１文字 TO 助成印Ｗ.
035240*
035250***
035500*
035510**================================================================*
035520* 給付割合チェック取得 SECTION.
035530**
035540**** ２７身障、被爆（３ペア）の時は、老人給付チェックに○
035550*     IF ( 公費種別ＷＲ NOT = ZERO )  AND
035560*        ( 助成種別ＷＲ NOT = ZERO )
      *         IF ( 受－施術和暦年月 < 42004 )
035570*             MOVE NC"○"   TO  老人給付チェックＷ 
      *         ELSE
035570*             MOVE NC"○"   TO  後高給付チェックＷ 
      *             MOVE NC"後"   TO  後高２Ｗ
      **/後期高齢＋広島、長崎の被爆は本体の負担割合にもチェックを入れる/080922
      *             
      *             IF (助成種別ＷＲ = 54) AND
      *                (費用負担者番号助成ＷＲ(3:2) = "34" OR "42")
      *                 MOVE 施術和暦ＷＲ TO 連率－施術和暦
      *                 MOVE 施術年ＷＲ   TO 連率－施術年
      *                 MOVE 施術月ＷＲ   TO 連率－施術月
      *                 MOVE 患者番号ＷＲ TO 連率－患者番号
      *                 MOVE 枝番ＷＲ     TO 連率－枝番
      *                 CALL   "HUTANRIT"
      *                 CANCEL "HUTANRIT"
035600*                 COMPUTE 負担割合ＷＲ = 連率－実際本体負担率 / 10
035610*                 COMPUTE 給付割合ＷＲ = 10 - 負担割合ＷＲ
035620*                 EVALUATE  給付割合ＷＲ
035630*                 WHEN  7
035640*                    MOVE NC"○"   TO  給付７割チェックＷ 
035650*                 WHEN  8
035660*                    MOVE NC"○"   TO  給付８割チェックＷ 
035670*                 WHEN  9
035680*                    MOVE NC"○"   TO  給付９割チェックＷ 
035690*                 WHEN  OTHER
035700*                    CONTINUE
035710*                 END-EVALUATE
      *             END-IF
      *         END-IF
035580**
035590*     ELSE
035600*         COMPUTE 負担割合ＷＲ = 連計－負担率 / 10
035610*         COMPUTE 給付割合ＷＲ = 10 - 負担割合ＷＲ
035620*         EVALUATE  給付割合ＷＲ
035630*         WHEN  7
035640*            MOVE NC"○"   TO  給付７割チェックＷ 
035650*         WHEN  8
035660*            MOVE NC"○"   TO  給付８割チェックＷ 
035670*         WHEN  9
035680*            MOVE NC"○"   TO  給付９割チェックＷ 
035690*         WHEN  OTHER
035700*            CONTINUE
035710*         END-EVALUATE
035720*     END-IF.
035730**
035740*================================================================*
035750* 柔整師全角数字取得 SECTION.
035760** 
035770** 99/12月まで
035780*     MOVE SPACE                TO 混在文字全体Ｗ.
035790*     MOVE SPACE                TO 混在文字Ｗ.
035800*     MOVE SPACE                TO 全角数字Ｗ.
035810*     MOVE 施情－柔整師番号     TO 混在文字全体Ｗ.
035820*     MOVE 1                    TO カウンタ３.
035830**
035840*     PERFORM VARYING カウンタ FROM 1 BY 1
035850*             UNTIL ( カウンタ > 8 ) 
035860*          EVALUATE 混在文字全体１Ｗ(カウンタ)
035870*          WHEN "１"
035880*              MOVE NC"１"   TO 全角数字１Ｗ(カウンタ３)
035890*              COMPUTE カウンタ３ = カウンタ３ + 1
035900*          WHEN "２"
035910*              MOVE NC"２"   TO 全角数字１Ｗ(カウンタ３)
035920*              COMPUTE カウンタ３ = カウンタ３ + 1
035930*          WHEN "３"
035940*              MOVE NC"３"   TO 全角数字１Ｗ(カウンタ３)
035950*              COMPUTE カウンタ３ = カウンタ３ + 1
035960*          WHEN "４"
035970*              MOVE NC"４"   TO 全角数字１Ｗ(カウンタ３)
035980*              COMPUTE カウンタ３ = カウンタ３ + 1
035990*          WHEN "５"
036000*              MOVE NC"５"   TO 全角数字１Ｗ(カウンタ３)
036010*              COMPUTE カウンタ３ = カウンタ３ + 1
036020*          WHEN "６"
036030*              MOVE NC"６"   TO 全角数字１Ｗ(カウンタ３)
036040*              COMPUTE カウンタ３ = カウンタ３ + 1
036050*          WHEN "７"
036060*              MOVE NC"７"   TO 全角数字１Ｗ(カウンタ３)
036070*              COMPUTE カウンタ３ = カウンタ３ + 1
036080*          WHEN "８"
036090*              MOVE NC"８"   TO 全角数字１Ｗ(カウンタ３)
036100*              COMPUTE カウンタ３ = カウンタ３ + 1
036110*          WHEN "９"
036120*              MOVE NC"９"   TO 全角数字１Ｗ(カウンタ３)
036130*              COMPUTE カウンタ３ = カウンタ３ + 1
036140*          WHEN "０"
036150*              MOVE NC"０"   TO 全角数字１Ｗ(カウンタ３)
036160*              COMPUTE カウンタ３ = カウンタ３ + 1
036170*          WHEN OTHER
036180*              MOVE 混在文字全体１Ｗ(カウンタ)  TO 混在文字１Ｗ(カウンタ)
036190*          END-EVALUATE
036200*     END-PERFORM.
036210**
036220*     MOVE 混在文字Ｗ  TO  柔整師番号１Ｗ.
036230*     MOVE 全角数字Ｗ  TO  柔整師番号２Ｗ.
036240**
036250**================================================================*
036260* 新柔整師全角数字取得 SECTION.
036270**
036280** 新柔整師番号を「契XXX XXXX -X-X 」で分解。2つめのXXXX を日本語タイプにする。
036290** 2000/01から
036300**
036310*     MOVE SPACE                TO 新柔整師番号ＷＴ.
036320*     MOVE SPACE                TO 全角数字Ｗ.
036330**
036340*     MOVE 施情－新柔整師番号   TO 新柔整師番号ＷＴ.
036350*     MOVE 1                    TO カウンタ３.
036360**
036370*     PERFORM VARYING カウンタ FROM 1 BY 1
036380*             UNTIL ( カウンタ > 4 ) 
036390*          EVALUATE 新柔整師番号３１Ｗ(カウンタ)
036400*          WHEN "1"
036410*              MOVE NC"１"   TO 全角数字１Ｗ(カウンタ３)
036420*              COMPUTE カウンタ３ = カウンタ３ + 1
036430*          WHEN "2"
036440*              MOVE NC"２"   TO 全角数字１Ｗ(カウンタ３)
036450*              COMPUTE カウンタ３ = カウンタ３ + 1
036460*          WHEN "3"
036470*              MOVE NC"３"   TO 全角数字１Ｗ(カウンタ３)
036480*              COMPUTE カウンタ３ = カウンタ３ + 1
036490*          WHEN "4"
036500*              MOVE NC"４"   TO 全角数字１Ｗ(カウンタ３)
036510*              COMPUTE カウンタ３ = カウンタ３ + 1
036520*          WHEN "5"
036530*              MOVE NC"５"   TO 全角数字１Ｗ(カウンタ３)
036540*              COMPUTE カウンタ３ = カウンタ３ + 1
036550*          WHEN "6"
036560*              MOVE NC"６"   TO 全角数字１Ｗ(カウンタ３)
036570*              COMPUTE カウンタ３ = カウンタ３ + 1
036580*          WHEN "7"
036590*              MOVE NC"７"   TO 全角数字１Ｗ(カウンタ３)
036600*              COMPUTE カウンタ３ = カウンタ３ + 1
036610*          WHEN "8"
036620*              MOVE NC"８"   TO 全角数字１Ｗ(カウンタ３)
036630*              COMPUTE カウンタ３ = カウンタ３ + 1
036640*          WHEN "9"
036650*              MOVE NC"９"   TO 全角数字１Ｗ(カウンタ３)
036660*              COMPUTE カウンタ３ = カウンタ３ + 1
036670*          WHEN "0"
036680*              MOVE NC"０"   TO 全角数字１Ｗ(カウンタ３)
036690*              COMPUTE カウンタ３ = カウンタ３ + 1
036700*          WHEN OTHER
036710*              MOVE SPACE    TO 全角数字１Ｗ(カウンタ３)
036720*              COMPUTE カウンタ３ = カウンタ３ + 1
036730*          END-EVALUATE
036740*     END-PERFORM.
036750**
036760** ( 契XXX )
036770*     STRING 新柔整師番号１Ｗ  DELIMITED BY SIZE
036780*            新柔整師番号２Ｗ  DELIMITED BY SIZE
036790*            INTO 柔整師番号１Ｗ
036800*     END-STRING.
036810** ( XXXX )
036820*     MOVE 全角数字Ｗ        TO  柔整師番号２Ｗ.
036830** ( -X-X )
036840*     MOVE 新柔整師番号４Ｗ  TO  柔整師番号３Ｗ.
036850**
036860*================================================================*
036870 前月初検のみ判定 SECTION.
036880*
036890*** 前月の通院日が初検か判定 
036900     MOVE  SPACE            TO 前月フラグ.
036910     MOVE 受－患者コード    TO 施記－患者コード.
036920     MOVE 受－施術和暦      TO 施記－施術和暦.
036930     MOVE 受－施術年        TO 施記－施術年.
036940     MOVE 受－施術月        TO 施記－施術月.
036950     MOVE 1                 TO 施記－施術日.
036960     START 施術記録Ｆ   KEY IS <  施記－患者コード
036970                                  施記－施術和暦年月日
036980                                  REVERSED
036990     END-START.
037000     IF ( 状態キー = "00" )
037010         MOVE SPACE  TO 終了フラグ２
037020         PERFORM 施術記録Ｆ読込
037030         IF ( 終了フラグ２      = SPACE  ) AND
037040            ( 施記－患者コード  = 受－患者コード ) AND
037050            ( 施記－診療区分    = 2 ) 
037060*
037070            PERFORM 前月判定
037080**** 適用１を使用
037090            IF ( 前月フラグ = "YES" )
037100               MOVE NC"※前月初検のみ"    TO  適用１Ｗ
037110            END-IF
037120**
037130         END-IF
037140     END-IF.
037150*
037160*================================================================*
037170 前月判定  SECTION.
037180* 
037190*** 読み込んだ施術記録の年月が、前月かどうか判定 (年月の差が 1 か?)
037200      MOVE  SPACE  TO  前月フラグ.
037210      INITIALIZE  計算年月日Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
037220**
037230      MOVE 受－施術和暦    TO 終了和暦２Ｗ.
037240      MOVE 受－施術年      TO 終了年２Ｗ.
037250      MOVE 受－施術月      TO 終了月２Ｗ.
037260      MOVE 施記－施術和暦  TO 開始和暦２Ｗ.
037270      MOVE 施記－施術年    TO 開始年２Ｗ.
037280      MOVE 施記－施術月    TO 開始月２Ｗ.
037290*
037300      EVALUATE TRUE
037310       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ = 終了年２Ｗ)
037320            PERFORM  前月比較月
037330       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ NOT = 終了年２Ｗ)
037340            PERFORM  前月比較年
037350       WHEN  開始和暦２Ｗ NOT = 終了和暦２Ｗ 
037360            PERFORM  前月比較元号
037370      END-EVALUATE.
037380*
037390      IF ( 計算月Ｗ = 1 )
037400         MOVE  "YES"  TO  前月フラグ
037410      END-IF.
037420*
037430*================================================================*
037440 前月比較月  SECTION.
037450*
037460     IF ( 終了月２Ｗ >  開始月２Ｗ )
037470         COMPUTE 計算月Ｗ = 終了月２Ｗ - 開始月２Ｗ
037480     ELSE
037490        MOVE ZERO TO 計算月Ｗ
037500     END-IF.
037510*
037520*================================================================*
037530 前月比較年  SECTION.
037540*
037550     IF ( 終了年２Ｗ >  開始年２Ｗ )
037560         COMPUTE 計算年Ｗ = 終了年２Ｗ - 開始年２Ｗ
037570         COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
037580     ELSE
037590        MOVE ZERO TO 計算月Ｗ
037600     END-IF.
037610*
037620*================================================================*
037630 前月比較元号  SECTION.
037640*
037650     MOVE 開始和暦２Ｗ TO 元－元号区分.
037660     READ 元号マスタ
037670     NOT INVALID KEY
037680         MOVE 元－開始西暦年 TO 開始西暦年Ｗ
037690     END-READ.
037700     MOVE 終了和暦２Ｗ TO 元－元号区分.
037710     READ 元号マスタ
037720     NOT INVALID KEY
037730         MOVE 元－開始西暦年 TO 終了西暦年Ｗ
037740     END-READ.
037750**
037760     IF ( 開始西暦年Ｗ NOT = ZERO ) AND ( 終了西暦年Ｗ NOT = ZERO )
037770        COMPUTE 開始西暦年Ｗ = 開始西暦年Ｗ + 開始年２Ｗ - 1
037780        COMPUTE 終了西暦年Ｗ = 終了西暦年Ｗ + 終了年２Ｗ - 1
037790*
037800        IF ( 終了西暦年Ｗ =  開始西暦年Ｗ )
037810           PERFORM  前月比較月
037820        ELSE
037830           IF ( 終了西暦年Ｗ >  開始西暦年Ｗ )
037840               COMPUTE 計算年Ｗ = 終了西暦年Ｗ - 開始西暦年Ｗ
037850               COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
037860           ELSE
037870               MOVE ZERO TO 計算月Ｗ
037880           END-IF
037890        END-IF
037900     ELSE
037910        MOVE ZERO TO 計算月Ｗ
037920     END-IF.
037930*
038110*================================================================*
038120 受診者印刷区分更新 SECTION.
038130*
038140** //  受診者情報Ｆの印刷区分に１をセットし、更新する。//  
038150*      ( 印刷区分助成)
038160*
038170     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
038180     MOVE 施術年ＷＲ         TO 受－施術年.
038190     MOVE 施術月ＷＲ         TO 受－施術月.
038200     MOVE 患者コードＷＲ     TO 受－患者コード.
038210     READ 受診者情報Ｆ
038220     NOT INVALID KEY
038230         MOVE  1  TO  受－レセ印刷区分助成
038240         REWRITE  受－レコード
038250         END-REWRITE
038260         IF ( 状態キー NOT = "00" )
038270            MOVE NC"受診者" TO ファイル名
038280            PERFORM エラー表示
038290         END-IF
038300     END-READ.
038310*
038320*================================================================*
038330 月末日取得 SECTION.
038340*
038350     MOVE 施術年ＷＲ   TO 受理年Ｗ.
038360     MOVE 施術月ＷＲ   TO 受理月Ｗ.
038370     MOVE 施術和暦ＷＲ TO 元－元号区分.
038380     READ 元号マスタ
038390     NOT INVALID KEY
038400         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
038410     END-READ.
038420     IF ( 施術西暦年Ｗ NOT = ZERO )
038430        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
038440     END-IF.
038450*
038460     EVALUATE 施術月ＷＲ
038470     WHEN 4
038480     WHEN 6
038490     WHEN 9
038500     WHEN 11
038510         MOVE 30 TO 受理日Ｗ
038520     WHEN 2
038530         DIVIDE 4 INTO 施術西暦年Ｗ GIVING    商Ｗ
038540                                    REMAINDER 余Ｗ
038550         END-DIVIDE
038560         IF ( 余Ｗ = ZERO )
038570             MOVE 29 TO 受理日Ｗ
038580         ELSE
038590             MOVE 28 TO 受理日Ｗ
038600         END-IF
038610     WHEN 1
038620     WHEN 3
038630     WHEN 5
038640     WHEN 7
038650     WHEN 8
038660     WHEN 10
038670     WHEN 12
038680         MOVE 31 TO 受理日Ｗ
038690     WHEN OTHER
038700          CONTINUE
038710     END-EVALUATE.
038720*
038730*================================================================*
038740 委任年月日取得 SECTION.
038750*
038760** ---// ここの受理年には、最終通院日が入っている為、退避する //----
038770     MOVE 受理年Ｗ   TO 最終通院年Ｗ.
038780     MOVE 受理月Ｗ   TO 最終通院月Ｗ.
038790     MOVE 受理日Ｗ   TO 最終通院日Ｗ.
038800***
038810* (柔整師側)
038820     EVALUATE レセプト日付区分Ｗ 
038830*    /  最終通院日 /
038840     WHEN ZERO
038850         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
038860         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
038870         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
038880*    /  月末日 /
038890     WHEN 1 
038900         PERFORM 月末日取得
038910         MOVE 受理年Ｗ     TO 柔整師年Ｗ
038920         MOVE 受理月Ｗ     TO 柔整師月Ｗ
038930         MOVE 受理日Ｗ     TO 柔整師日Ｗ
038940*    /  印字なし /
038950     WHEN 9
038960         MOVE ZERO         TO 柔整師年Ｗ
038970         MOVE ZERO         TO 柔整師月Ｗ
038980         MOVE ZERO         TO 柔整師日Ｗ
038990*    /  その他は、最終通院日 /
039000     WHEN OTHER
039010         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
039020         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
039030         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
039040     END-EVALUATE.
039050**
039060* (患者側)
039070     EVALUATE レセプト患者日付区分Ｗ 
039080*    /  最終通院日 /
039090     WHEN ZERO
039100         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
039110         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
039120         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
039130*    /  月末日 /
039140     WHEN 1 
039150         PERFORM 月末日取得
039160         MOVE 受理年Ｗ     TO 患者委任年Ｗ
039170         MOVE 受理月Ｗ     TO 患者委任月Ｗ
039180         MOVE 受理日Ｗ     TO 患者委任日Ｗ
039190*    /  印字なし /
039200     WHEN 9
039210         MOVE ZERO         TO 患者委任年Ｗ
039220         MOVE ZERO         TO 患者委任月Ｗ
039230         MOVE ZERO         TO 患者委任日Ｗ
039240*    /  その他は、最終通院日 /
039250     WHEN OTHER
039260         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
039270         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
039280         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
039290     END-EVALUATE.
039300*
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
039310*================================================================*
039311*================================================================*
039312 地域特有処理 SECTION.
039313*
039314*--------------------------------------------------------*
039315*  福岡県：経過欄の固定印字 (全柔ＦＰＤ区分Ｗ 1 使用)
039316*  長期以外の部位は、「順調」
039317*  長期の部位は、「緩慢」
039318*--------------------------------------------------------*
039319*
039320     IF 全柔ＦＰＤ区分Ｗ = 1
039321*      まず「順調」セット
039322        PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
034833                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR
                             ( 部位ＣＮＴ > 5 )
039324*
039325                 EVALUATE 部位ＣＮＴ
039326                 WHEN 1
039327                     MOVE NC"①" TO 経過部位数字Ｗ
039328                 WHEN 2
039329                     MOVE NC"②" TO 経過部位数字Ｗ
039330                 WHEN 3
039331                     MOVE NC"③" TO 経過部位数字Ｗ
039332                 WHEN 4
039333                     MOVE NC"④" TO 経過部位数字Ｗ
039334                 WHEN 5
039335                     MOVE NC"⑤" TO 経過部位数字Ｗ
039336                 END-EVALUATE
039337                 MOVE SPACE TO 経過略称(部位ＣＮＴ)
039338                 STRING  経過部位数字Ｗ   DELIMITED BY SPACE
039339                         NC"順調"         DELIMITED BY SPACE
039340                        INTO 経過略称(部位ＣＮＴ)
039341                 END-STRING
039342        END-PERFORM
039343*
039344*      次に、３カ月以上の長期判定
039345        MOVE  SPACE TO  連期間－キー
039346        INITIALIZE      連期間－キー
039347        MOVE 施術和暦ＷＲ  TO  連期間－施術和暦
039348        MOVE 施術年ＷＲ    TO  連期間－施術年
039349        MOVE 施術月ＷＲ    TO  連期間－施術月
039350        MOVE 患者番号ＷＲ  TO  連期間－患者番号
039351        MOVE 枝番ＷＲ      TO  連期間－枝番
039352        CALL   "CHOUKI"
039353        CANCEL "CHOUKI"
039354*
039355        IF 連期間－対象フラグ  = "YES"
039356           PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
034833                   UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR
                               ( 部位ＣＮＴ > 5 )
039358*
039359               IF 連期間－期間Ｗ(部位ＣＮＴ)  >  ZERO
039360
039361                   EVALUATE 部位ＣＮＴ
039362                   WHEN 1
039363                       MOVE NC"①" TO 経過部位数字Ｗ
039364                   WHEN 2
039365                       MOVE NC"②" TO 経過部位数字Ｗ
039366                   WHEN 3
039367                       MOVE NC"③" TO 経過部位数字Ｗ
039368                   WHEN 4
039369                       MOVE NC"④" TO 経過部位数字Ｗ
039370                   WHEN 5
039371                       MOVE NC"⑤" TO 経過部位数字Ｗ
039372                   END-EVALUATE
039373                   MOVE SPACE TO 経過略称(部位ＣＮＴ)
039374                   STRING  経過部位数字Ｗ   DELIMITED BY SPACE
039375                           NC"緩慢"         DELIMITED BY SPACE
039376                          INTO 経過略称(部位ＣＮＴ)
039377                   END-STRING
039378               END-IF
039379           END-PERFORM
039380        END-IF
039381*
039382     END-IF.
039383*
      */長野で経過が入力されてない時は、経過を入れる/160610
           IF (費用負担者番号助成ＷＲ(3:2) = "20")
016020        PERFORM 経過取得
           END-IF.
018770     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018790             UNTIL ( 部位ＣＮＴ > 5 )
018820         MOVE 印刷経過略称Ｗ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
018830     END-PERFORM.
039384*
039385*================================================================*
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
039386*================================================================*
039387 エラー表示 SECTION.
039388*
039389     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
039390     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
039391     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
039392     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
039393                                                   UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
039400     ACCEPT  キー入力 FROM CONS
039410     PERFORM ファイル閉鎖.
039420     EXIT PROGRAM.
039430*================================================================*
039440*================================================================*
039450 ファイル閉鎖 SECTION.
039460*
039470     CLOSE 保険者マスタ     元号マスタ          名称マスタ
039480           レセプトＦ       制御情報マスタ      施術所情報マスタ
039490           請求先マスタ     経過マスタ          受診者情報Ｆ
039500           施術記録Ｆ       負傷データＦ        印刷ファイル
039510           負傷原因Ｆ       市町村マスタ        受診者情報２Ｆ
039520           会情報マスタ     ＩＤ管理マスタ      作業ファイル４
                 メモファイル.
039530*================================================================*
039540 終了処理 SECTION.
039550*
039560     PERFORM ファイル閉鎖.
039570*================================================================*
039580*================================================================*
039590 テスト印字処理 SECTION.
039600*
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
           委任年 委任月 委任日 明細書発行加算料 明細書発行加算日
           金属月(1) 金属月(2) 金属月(3) 月(1) 月(2) 月(3) 金属日(1) 金属日(2) 金属日(3)
           運動後療料 金属回数 運動回数 運動日(1) 運動日(2) 運動日(3) 運動日(4) 運動日(5)
           .
      *
           MOVE ALL "X" TO 
           公費負担者番号 受給者番号 県施術ＩＤ 記号番号 住所１ 住所２
           口座名義人カナ 口座名義人 柔整師番号 口座番号 保険者名称 施術所郵便番号１  
           施術所郵便番号２ 施術所住所１ 施術所住所２ 施術所電話番号 代表者カナ 接骨師会会員番号
           .
      *
           MOVE ALL NC"Ｎ" TO
           負傷名１ 負傷名２ 負傷名３ 負傷名４ 負傷名５ 経過略称(1) 助成印
           経過略称(2) 経過略称(3) 経過略称(4) 経過略称(5) 適用１ 適用２ 部位５適用 
           .
      *
           MOVE ALL "Ｎ" TO
           被保険者氏名 患者氏名 代表者名 接骨院名 金属副子 長期頻回
           長期理由文１ 長期理由文２ 長期理由文３ 長期理由文４ 長期理由文５ 長期理由文６
           長期理由文７ 長期理由文８ 負傷原因１ 負傷原因２ 負傷原因３ 負傷原因４ 負傷原因５ 負傷原因６
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
           難路チェック 治癒チェック１ 中止チェック１ 転医チェック１ 令和チェック
           治癒チェック２ 中止チェック２ 転医チェック２ 治癒チェック３ 中止チェック３ 転医チェック３ 
           治癒チェック４ 中止チェック４ 転医チェック４ 治癒チェック５ 中止チェック５ 転医チェック５ 
           新規チェック 継続チェック 男チェック 明治チェック 大正チェック 女チェック 昭和チェック 
           平成チェック 単独チェック 本人チェック 高一チェック 共済チェック 自チェック 社保チェック 
           組合チェック １０割チェック ９割チェック ２併チェック ６歳チェック ８割チェック ７割チェック 
           後期チェック 退職チェック 国保チェック 家族チェック 高７チェック
           .
040870*
040880*================================================================*
       施術日取得 SECTION.
      *
      *     MOVE SPACE TO 施術日Ｗ.
028350     MOVE 患者番号ＷＲ          TO 施記－患者番号.
028360     MOVE 枝番ＷＲ              TO 施記－枝番.
028370     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
028380     MOVE 施術年ＷＲ            TO 施記－施術年.
028390     MOVE 施術月ＷＲ            TO 施記－施術月.
      *------------------------------------------------------------------------*
           IF ( 連レ－保険種別 > 50 ) AND ( レセ－助成月途中対象 = 1 )
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
           PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 31
               MOVE カウンタ TO 施術日(カウンタ)
           END-PERFORM.
      *================================================================*
037310 レセ摘要再セット SECTION.
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
037490*
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
040890******************************************************************
040900 END PROGRAM YCH6427.
040910******************************************************************
