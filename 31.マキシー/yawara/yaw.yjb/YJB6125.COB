000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YJB6125.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*    ＪＢ  レセプト印刷（柔+ｳｨﾝﾄﾞｳｽﾞ版）
000110*         MED = YJB610G YJB6125P
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2011-02-10
000140 DATE-COMPILED.          2011-02-10
      */実日数はレセ－部位実日数を転記する/160823
      */金属副子・運動後療の変更・追加/1805
      */平成30年7月～会長名変更/20180713
      */長野県のこどもの助成で受給者負担金を記入/2106
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
000320                             ALTERNATE RECORD KEY     IS  保－保険種別
000330                                                          保－保険者名称
000340                                                          保－保険者番号
000350                             FILE STATUS              IS  状態キー
000360                             LOCK        MODE         IS  AUTOMATIC.
000361     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
000362                             ORGANIZATION             IS  INDEXED
000363                             ACCESS MODE              IS  DYNAMIC
000364                             RECORD KEY               IS  市－公費種別
000365                                                          市－市町村番号
000366                             ALTERNATE RECORD KEY     IS  市－公費種別
000367                                                          市－市町村名称
000368                                                          市－市町村番号
000369                             FILE STATUS              IS  状態キー
000370                             LOCK        MODE         IS  AUTOMATIC.
000371     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
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
000650                             RECORD KEY               IS 施情－施術所番号
000660                             FILE STATUS              IS  状態キー
000670                             LOCK        MODE         IS  AUTOMATIC.
000680     SELECT  請求先マスタ    ASSIGN      TO        SEIKYUSL
000690                             ORGANIZATION           IS  INDEXED
000700                             ACCESS MODE            IS  DYNAMIC
000710                             RECORD KEY             IS 請先－保険種別
000720                                                       請先－保険者番号
000730                             FILE STATUS            IS  状態キー
000740                             LOCK    MODE           IS  AUTOMATIC.
000750     SELECT  経過マスタ      ASSIGN      TO        KEIKAL
000760                             ORGANIZATION             IS  INDEXED
000770                             ACCESS MODE              IS  DYNAMIC
000780                             RECORD KEY               IS  経－区分コード
000790                                                          経－経過コード
000800                             FILE STATUS              IS  状態キー
000810                             LOCK        MODE         IS  AUTOMATIC.
000820     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
000830                             ORGANIZATION             IS  INDEXED
000840                             ACCESS MODE              IS  DYNAMIC
000850                             RECORD KEY               IS  負原－区分コード
000860                                                          負原－負傷原因コード
000870                             FILE STATUS              IS  状態キー
000880                             LOCK        MODE         IS  AUTOMATIC.
000890     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000900                             ORGANIZATION             IS  INDEXED
000910                             ACCESS MODE              IS  DYNAMIC
000920                             RECORD KEY               IS 受－施術和暦年月
000930                                                          受－患者コード
000940                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000950                                                          受－患者カナ
000960                                                          受－患者コード
000970                             ALTERNATE RECORD KEY     IS  受－患者コード
000980                                                         受－施術和暦年月
000990                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
001000                                                          受－保険種別
001010                                                          受－保険者番号
001020                                                          受－患者コード
001030                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
001040                                                          受－公費種別
001050                                                     受－費用負担者番号
001060                                                          受－患者コード
001070                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
001080                                                          受－助成種別
001090                                                  受－費用負担者番号助成
001100                                                          受－患者コード
001110                             ALTERNATE RECORD KEY  IS 受－請求和暦年月
001120                                                      受－施術和暦年月
001130                                                      受－患者コード
001140                             FILE STATUS              IS  状態キー
001150                             LOCK        MODE         IS  AUTOMATIC.
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
001160     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
001170                             ORGANIZATION             IS  INDEXED
001180                             ACCESS MODE              IS  DYNAMIC
001190                             RECORD KEY           IS 施記－施術和暦年月日
001200                                                     施記－患者コード
001210                             ALTERNATE RECORD KEY IS 施記－患者コード
001220                                                     施記－施術和暦年月日
001230                             FILE STATUS              IS  状態キー
001240                             LOCK        MODE         IS  AUTOMATIC.
001250     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
001260                             ORGANIZATION             IS  INDEXED
001270                             ACCESS MODE              IS  DYNAMIC
001280                             RECORD KEY               IS 負－施術和暦年月
001290                                                         負－患者コード
001300                             ALTERNATE RECORD KEY     IS 負－患者コード
001310                                                         負－施術和暦年月
001320                             FILE STATUS              IS  状態キー
001330                             LOCK        MODE         IS  AUTOMATIC.
001340     SELECT  メモファイル    ASSIGN      TO        MEMOL
001350                             ORGANIZATION             IS  INDEXED
001360                             ACCESS MODE              IS  DYNAMIC
001370                             RECORD KEY               IS  メモ－制御区分
001380                                                          メモ－患者コード
001390                                                          メモ－施術和暦年月日
001400                             ALTERNATE RECORD KEY     IS  メモ－制御区分
001410                                                          メモ－施術和暦年月日
001420                                                          メモ－患者コード
001430                             ALTERNATE RECORD KEY     IS  メモ－患者コード
001440                                                          メモ－施術和暦年月日
001450                                                          メモ－制御区分
001460                             FILE STATUS              IS  状態キー
001470                             LOCK        MODE         IS  AUTOMATIC.
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
001480     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF002
001490                             SYMBOLIC    DESTINATION  IS "PRT"
001500                             FORMAT                   IS  定義体名Ｐ
001510                             GROUP                    IS  項目群名Ｐ
001520                             PROCESSING  MODE         IS  処理種別Ｐ
001530                             UNIT        CONTROL      IS  拡張制御Ｐ
001540                             FILE        STATUS       IS  通知情報Ｐ.
001550******************************************************************
001560*                      DATA DIVISION                             *
001570******************************************************************
001580 DATA                    DIVISION.
001590 FILE                    SECTION.
001600*                           ［ＲＬ＝  ３２０］
001610 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001620     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
001621*                           ［ＲＬ＝  ２５６］
001622 FD  市町村マスタ          BLOCK   CONTAINS   1   RECORDS.
001623     COPY SITYOSN        OF  XFDLIB  JOINING   市   AS  PREFIX.
001630*                           ［ＲＬ＝  １２８］
001640 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001650     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001660*                           ［ＲＬ＝  １２８］
001670 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
001680     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
001720*                           ［ＲＬ＝  ２５６］
001730 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001740     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
001750*                           ［ＲＬ＝  １２８］
001760 FD  施術所情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001770     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
001780*                           ［ＲＬ＝  １２８］
001790 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
001800     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
001810*                           ［ＲＬ＝  １２８］
001820 FD  経過マスタ          BLOCK   CONTAINS   1   RECORDS.
001830     COPY KEIKA          OF  XFDLIB  JOINING   経   AS  PREFIX.
001840*                           ［ＲＬ＝  ３２０］
001850 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
001860     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
002560*                          ［ＲＬ＝  1024］
000340 FD  受診者情報２Ｆ        BLOCK   CONTAINS   1   RECORDS.
000350     COPY JUSINJ2          OF  XFDLIB  JOINING   受２   AS  PREFIX.
001870*                           ［ＲＬ＝  ２５６］
001880 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
001890     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
001900*                           ［ＲＬ＝  １２８］
001910 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
001920     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
001930*                           ［ＲＬ＝  １２８］
001940 FD  負傷原因Ｆ         BLOCK   CONTAINS   1   RECORDS.
001950     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
001960*                           ［ＲＬ＝  ８３２］
001970 FD  メモファイル        BLOCK CONTAINS 1     RECORDS.
001980     COPY MEMO           OF    XFDLIB JOINING メモ AS PREFIX.
002440*                           ［ＲＬ＝  １２８］
002450 FD  ＩＤ管理マスタ          BLOCK   CONTAINS   1   RECORDS.
002460     COPY IDKANR    OF  XFDLIB  JOINING   ＩＤ管   AS  PREFIX.
001990*
002000 FD  印刷ファイル.
002010     COPY YJB6125P       OF  XMDLIB.
002020*----------------------------------------------------------------*
002030******************************************************************
002040*                WORKING-STORAGE SECTION                         *
002050******************************************************************
002060 WORKING-STORAGE         SECTION.
002070 01 キー入力                           PIC X     VALUE SPACE.
002080 01 状態キー                           PIC X(2)  VALUE SPACE.
002090 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002100 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002110 01 初検フラグ                         PIC X(3)  VALUE SPACE.
002120 01 継続フラグ                         PIC X(3)  VALUE SPACE.
002130 01 ファイル名                         PIC N(6)  VALUE SPACE.
002140 01 レセプトＰＧＷ                     PIC X(8)  VALUE SPACE.
002150 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
002160 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
002170 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002180 01 患者番号Ｗ                         PIC 9(6)  VALUE ZERO.
002190 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002200 01 部位名称Ｗ                         PIC N(12) VALUE SPACE.
002210 01 部位長Ｗ                           PIC 9(2) VALUE 1.
002220 01 脱出フラグ                         PIC X(3)  VALUE SPACE.
002230 01 空白Ｗ                             PIC X(2)  VALUE SPACE.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
002240*
002250** 施術所住所編集
002260 01 住所連結Ｗ.
002270    03 住所連結１Ｗ                    PIC X(50).
002280    03 住所連結２Ｗ                    PIC X(30).
002290 01 住所編集Ｗ.
002300    03 FILLER                          PIC X(10).
002310    03 住所編集１Ｗ                    PIC X(40).
002320*
002330** 数字→日本語変換
002340 01 数字Ｗ                             PIC 9(2).
002350 01 数字Ｒ REDEFINES 数字Ｗ.
002360    03 数字Ｗ１                        PIC X(1).
002370    03 数字Ｗ２                        PIC X(1).
002380*
002390 01 負傷番号Ｗ                         PIC 9.
002400 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
002410    03 負傷番号Ｗ１                    PIC X.
002420*
002430 01 全角負傷番号Ｗ                     PIC N.
002440 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
002450    03 全角負傷番号Ｗ１                PIC X(2).
002460*************
002470* 共済番号用
002480 01 共済連番号集団Ｗ.
002490    03 共済連番号名Ｗ                  PIC X(14)  VALUE SPACE.
002500    03 共済連番号名ＮＷ REDEFINES  共済連番号名Ｗ  PIC N(7).
002510    03 共済連番号Ｗ                    PIC X(6)  VALUE SPACE.
002520    03 共済連番号単位Ｗ                PIC X(2)  VALUE SPACE.
002530    03 共済連番号単位ＮＷ REDEFINES  共済連番号単位Ｗ  PIC N.
002540* 自衛官番号用
002550 01 自衛官番号集団Ｗ.
002560    03 自衛官番号名Ｗ                  PIC X(8)  VALUE SPACE.
002570    03 自衛官番号名ＮＷ REDEFINES  自衛官番号名Ｗ  PIC N(4).
002580    03 自衛官番号Ｗ                    PIC X(6)  VALUE SPACE.
002590    03 自衛官番号単位Ｗ                PIC X(2)  VALUE SPACE.
002600    03 自衛官番号単位ＮＷ REDEFINES  自衛官番号単位Ｗ  PIC N.
002610*******
002620*
002630 01 カウンタ                           PIC 9(2)  VALUE ZERO.
002640 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
002650 01 保険名称Ｗ                         PIC N(12) VALUE SPACE.
002660*
002670* 退避用
002680 01 終了年月日ＷＴ.
002690    03 終了年ＷＴ                      PIC 9(2)  VALUE ZERO.
002700    03 終了月ＷＴ                      PIC 9(2)  VALUE ZERO.
002710    03 終了日ＷＴ                      PIC 9(2)  VALUE ZERO.
002720* 初検日退避用
002730 01 初検年月日ＷＴ.
002740    03 初検和暦ＷＴ                    PIC 9     VALUE ZERO.
002750    03 初検年ＷＴ                      PIC 9(2)  VALUE ZERO.
002760    03 初検月ＷＴ                      PIC 9(2)  VALUE ZERO.
002770    03 初検日ＷＴ                      PIC 9(2)  VALUE ZERO.
002780* 負傷原因用
002790 01 負傷原因ＷＴ.
002800    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
002810    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
002820    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
002830    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
002840    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
002850    03 負傷原因ナンバーＷＴ.
002860       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
002870    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
002880 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
002890 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
002900 01 負傷原因ＴＢＬ.
002910    03 負傷原因コードＴＢＬ            OCCURS 9.
002920       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
002930       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
002940       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
002950 01 負傷原因内容Ｗ.
002960    03 負傷原因内容合成Ｗ              PIC X(318) OCCURS 9 VALUE SPACE.
003620    03 負傷原因内容分解ＸＷ.
003630       05 負傷原因内容１ＸＷ           PIC X(80)  VALUE SPACE.
003640       05 負傷原因内容２ＸＷ           PIC X(80)  VALUE SPACE.
003640       05 負傷原因内容３ＸＷ           PIC X(80)  VALUE SPACE.
003650       05 負傷原因内容４ＸＷ           PIC X(78)  VALUE SPACE.
009270*
009280 01 長期理由Ｗ.
          03 摘要文ＷＴＢＬ.
             05 摘要文ＷＴ                   PIC N(56) OCCURS 15 VALUE SPACE.
          03 長期理由ＷＴＢＬ.
             05 長期理由ＷＴ                 PIC N(56) OCCURS 15 VALUE SPACE.
009280    03 長期理由合成Ｗ                  PIC N(846) VALUE SPACE.
          03 長期理由分解Ｗ.
             05 長期理由内容Ｗ               PIC N(56) OCCURS 30 VALUE SPACE.
003010*
003020* 初検加算時刻用
003030 01 初検加算ＷＴ.
003040    03 初検加算カウント                PIC 9    VALUE ZERO.
003050    03 番号カウンタ                    PIC 9    VALUE ZERO.
003060    03 初検加算集団ＷＴ  OCCURS 3.
003070       05 初検加算区分ＷＴ             PIC 9    VALUE ZERO.
003080       05 初検加算時ＷＴ               PIC 9(2) VALUE ZERO.
003090       05 初検加算分ＷＴ               PIC 9(2) VALUE ZERO.
003100    03 初検加算集団ＮＷ  OCCURS 3.
003110       05 加算区切Ｗ                   PIC N(1) VALUE SPACE.
003120       05 加算内容Ｗ                   PIC N(3) VALUE SPACE.
003130       05 初検加算時ＮＷ１             PIC N(1) VALUE SPACE.
003140       05 初検加算時ＮＷ２             PIC N(1) VALUE SPACE.
003150       05 時固定Ｗ                     PIC N(1) VALUE SPACE.
003160       05 初検加算分ＮＷ１             PIC N(1) VALUE SPACE.
003170       05 初検加算分ＮＷ２             PIC N(1) VALUE SPACE.
003180       05 分固定Ｗ                     PIC N(1) VALUE SPACE.
003190    03 初検加算時刻１Ｗ                PIC N(10) VALUE SPACE.
003200    03 初検加算時刻２Ｗ                PIC N(10) VALUE SPACE.
003210    03 初検加算時刻３Ｗ                PIC N(10) VALUE SPACE.
003070    03 初検加算区切Ｗ                  PIC X     VALUE SPACE.
003080    03 初検加算時Ｗ                    PIC 9(2)  VALUE ZERO.
003090    03 初検加算分Ｗ                    PIC 9(2)  VALUE ZERO.
003220*
003230** 前月初検のみ用
003240 01 初日再検フラグ                     PIC X(3)  VALUE SPACE.
003250 01 前月フラグ                         PIC X(3)  VALUE SPACE.
003260*
003270 01 計算年月日Ｗ.
003280    03 計算和暦Ｗ                      PIC 9(1)  VALUE ZERO.
003290    03 計算年Ｗ                        PIC S9(2)  VALUE ZERO.
003300    03 計算月Ｗ                        PIC S9(2)  VALUE ZERO.
003310    03 計算日Ｗ                        PIC S9(2)  VALUE ZERO.
003320 01 開始年月日２Ｗ.
003330    03 開始和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003340    03 開始年２Ｗ                      PIC 9(2)  VALUE ZERO.
003350    03 開始月２Ｗ                      PIC 9(2)  VALUE ZERO.
003360    03 開始日２Ｗ                      PIC 9(2)  VALUE ZERO.
003370    03 開始西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003380 01 終了年月日２Ｗ.
003390    03 終了和暦２Ｗ                    PIC 9(1)  VALUE ZERO.
003400    03 終了年２Ｗ                      PIC 9(2)  VALUE ZERO.
003410    03 終了月２Ｗ                      PIC 9(2)  VALUE ZERO.
003420    03 終了日２Ｗ                      PIC 9(2)  VALUE ZERO.
003430    03 終了西暦年Ｗ                    PIC S9(4) VALUE ZERO.
003440*
003450** 負傷データＦ用ワーク
003460 01 負傷原因印刷区分Ｆ                 PIC 9 VALUE ZERO.
003460 01 長期理由印刷区分Ｆ                 PIC 9 VALUE ZERO.
003470***
003480** 負傷原因・長期理由印刷区分用
003490 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
003500 01 長期理由印刷区分Ｗ                 PIC 9 VALUE ZERO.
003510*
003520** レセ下段の日付区分用 (0:最終通院日、1:月末日、9:印字なし)
003530 01 レセプト日付区分Ｗ                 PIC 9 VALUE ZERO.
003540 01 レセプト患者日付区分Ｗ             PIC 9 VALUE ZERO.
003550*
003560** 月末日用
003570 01 施術西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
003580 01 商Ｗ                               PIC 9(3)  VALUE ZERO.
003590 01 余Ｗ                               PIC 9(3)  VALUE ZERO.
003600*
003610*
003620** 枝番判定用
003630 01 開始診療日手動区分Ｗ               PIC 9    VALUE ZERO.
003640*
003650** 負担割合用
003660 01 負担割合数字Ｗ                     PIC 9 VALUE ZERO.
003670 01 負担割合文字Ｗ                     PIC X VALUE SPACE.
003680*
003690*
003700** レセ摘要用( N(38)固定） /
003710 01 負傷の経過Ｗ.
003720    03 負傷の経過行Ｗ                  PIC X(76) OCCURS 2 VALUE SPACE.
003730 01 負傷の経過ＮＷ REDEFINES 負傷の経過Ｗ.
003740    03 負傷の経過行ＮＷ                PIC N(38) OCCURS 2.
003750*
003751** 助成レセまとめ用
003752 01 助成レセまとめフラグ               PIC X(3)  VALUE SPACE.
003753*
003754*
003755* 旧用紙区分
003756 01 旧用紙区分Ｗ                       PIC 9 VALUE ZERO.
002140 01 レセプト新旧区分.
002150    03 一般レセＷ                      PIC 9 VALUE ZERO.
      *
      */金属副子・運動後療の変更・追加/1805
       01 金属副子ＣＭ                       PIC X(200) VALUE SPACE.
       01 運動後療ＣＭ                       PIC X(68)  VALUE SPACE.
003757*
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
003760****************
003770* 連結項目待避 *
003780****************
003790*    ************
003800*    * 印刷キー *
003810*    ************
003820 01 対象データＷＲ.
003830    03 施術和暦年月ＷＲ.
003840       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
003850       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
003860       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
003870    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
003880    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
003890    03 公費種別ＷＲ                     PIC 9(2)  VALUE ZERO.
003900    03 費用負担者番号ＷＲ               PIC X(10) VALUE SPACE.
003910    03 助成種別ＷＲ                     PIC 9(2)  VALUE ZERO.
003920    03 費用負担者番号助成ＷＲ           PIC X(10) VALUE SPACE.
003930    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
003940    03 患者カナＷＲ                     PIC X(20) VALUE SPACE.
003950    03 患者コードＷＲ.
003960       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
003970       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
003980*    ************
003990*    * 料金情報 *
004000*    ************
004010*    月毎の料金
004020***********************
004030 01 料金１ＷＲ.
004040   03 初検ＷＲ.
004050      05 負担割合ＷＲ               PIC 9(3)    VALUE ZERO.
004060      05 初検料ＷＲ                 PIC 9(5)    VALUE ZERO.
004070      05 初検加算料ＷＲ             PIC 9(5)    VALUE ZERO.
         03 初検時相談料ＷＲ              PIC 9(4)    VALUE ZERO.
004080   03 再検料ＷＲ                    PIC 9(5)    VALUE ZERO.
004090   03 往療ＷＲ.
004100      05 往療距離ＷＲ               PIC 9(2)V9  VALUE ZERO.
004110      05 往療回数ＷＲ               PIC 9(2)    VALUE ZERO.
004120      05 往療料ＷＲ                 PIC 9(6)    VALUE ZERO.
004130      05 往療加算料ＷＲ             PIC 9(5)    VALUE ZERO.
004140   03 金属副子加算料ＷＲ            PIC 9(5)    VALUE ZERO.
004150   03 施術情報提供料ＷＲ            PIC 9(5)    VALUE ZERO.
004160   03 合計ＷＲ                      PIC 9(6)    VALUE ZERO.
004170   03 一部負担金ＷＲ                PIC 9(6)    VALUE ZERO.
004180   03 請求金額ＷＲ                  PIC 9(6)    VALUE ZERO.
004190   03 給付割合ＷＲ                  PIC 9(1)    VALUE ZERO.
004200   03 受給者負担額ＷＲ              PIC 9(6)    VALUE ZERO.
004210   03 助成請求金額ＷＲ              PIC 9(6)    VALUE ZERO.
004220*
004230* 負傷部位毎の料金
004240***********************
004250 01 料金２ＷＲ.
004260   03 初回処置ＷＲ    OCCURS   9.
004270      05 初回処置料ＷＲ             PIC 9(5)    VALUE ZERO.
004280*
004290* 逓減毎の料金
004300***********************
004310 01 料金３ＷＲ.
004320**********
004330* １部位 *
004340**********
004350   03 部位１ＷＲ.
004360      05 後療１ＷＲ.
004370         07 後療単価１ＷＲ              PIC 9(4)    VALUE ZERO.
004380         07 後療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
004390         07 後療料１ＷＲ                PIC 9(5)    VALUE ZERO.
004400      05 冷罨法１ＷＲ.
004410         07 冷罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
004420         07 冷罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
004430      05 温罨法１ＷＲ.
004440         07 温罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
004450         07 温罨法料１ＷＲ              PIC 9(4)    VALUE ZERO.
004460      05 電療１ＷＲ.
004470         07 電療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
004480         07 電療料１ＷＲ                PIC 9(4)    VALUE ZERO.
004490      05 小計１ＷＲ                     PIC 9(6)    VALUE ZERO.
004500      05 長期逓減率１ＷＲ               PIC 9(3)    VALUE ZERO.
004510      05 長期込小計１ＷＲ               PIC 9(6)    VALUE ZERO.
004520**********
004530* ２部位 *
004540**********
004550   03 部位２ＷＲ.
004560      05 後療２ＷＲ.
004570         07 後療単価２ＷＲ              PIC 9(4)    VALUE ZERO.
004580         07 後療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
004590         07 後療料２ＷＲ                PIC 9(5)    VALUE ZERO.
004600      05 冷罨法２ＷＲ.
004610         07 冷罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
004620         07 冷罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
004630      05 温罨法２ＷＲ.
004640         07 温罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
004650         07 温罨法料２ＷＲ              PIC 9(4)    VALUE ZERO.
004660      05 電療２ＷＲ.
004670         07 電療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
004680         07 電療料２ＷＲ                PIC 9(4)    VALUE ZERO.
004690      05 小計２ＷＲ                     PIC 9(6)    VALUE ZERO.
004700      05 長期逓減率２ＷＲ               PIC 9(3)    VALUE ZERO.
004710      05 長期込小計２ＷＲ               PIC 9(6)    VALUE ZERO.
004720******************
004730* ３部位／８割 *
004740******************
004750   03 部位３８ＷＲ.
004760      05 後療３８ＷＲ.
004770         07 後療単価３８ＷＲ              PIC 9(4)  VALUE ZERO.
004780         07 後療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
004790         07 後療料３８ＷＲ                PIC 9(5)  VALUE ZERO.
004800      05 冷罨法３８ＷＲ.
004810         07 冷罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
004820         07 冷罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
004830      05 温罨法３８ＷＲ.
004840         07 温罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
004850         07 温罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
004860      05 電療３８ＷＲ.
004870         07 電療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
004880         07 電療料３８ＷＲ                PIC 9(4)  VALUE ZERO.
004890      05 小計３８ＷＲ                     PIC 9(6)  VALUE ZERO.
004900      05 多部位込小計３８ＷＲ             PIC 9(6)  VALUE ZERO.
004910      05 長期逓減率３８ＷＲ               PIC 9(3)  VALUE ZERO.
004920      05 長期込小計３８ＷＲ               PIC 9(6)  VALUE ZERO.
004930******************
004940* ３部位／１０割 *
004950******************
004960   03 部位３０ＷＲ.
004970      05 逓減開始月日３０ＷＲ.
004980         07 逓減開始月３０ＷＲ            PIC 9(2)  VALUE ZERO.
004990         07 逓減開始日３０ＷＲ            PIC 9(2)  VALUE ZERO.
005000      05 後療３０ＷＲ.
005010         07 後療単価３０ＷＲ              PIC 9(4)  VALUE ZERO.
005020         07 後療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
005030         07 後療料３０ＷＲ                PIC 9(5)  VALUE ZERO.
005040      05 冷罨法３０ＷＲ.
005050         07 冷罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
005060         07 冷罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
005070      05 温罨法３０ＷＲ.
005080         07 温罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
005090         07 温罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
005100      05 電療３０ＷＲ.
005110         07 電療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
005120         07 電療料３０ＷＲ                PIC 9(4)  VALUE ZERO.
005130      05 小計３０ＷＲ                     PIC 9(6)  VALUE ZERO.
005140      05 長期逓減率３０ＷＲ               PIC 9(3)  VALUE ZERO.
005150      05 長期込小計３０ＷＲ               PIC 9(6)  VALUE ZERO.
005160****************
005170* ４部位／５割 *
005180****************
005190   03 部位４５ＷＲ.
005200      05 後療４５ＷＲ.
005210         07 後療単価４５ＷＲ              PIC 9(4)  VALUE ZERO.
005220         07 後療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005230         07 後療料４５ＷＲ                PIC 9(5)  VALUE ZERO.
005240      05 冷罨法４５ＷＲ.
005250         07 冷罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005260         07 冷罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
005270      05 温罨法４５ＷＲ.
005280         07 温罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005290         07 温罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
005300      05 電療４５ＷＲ.
005310         07 電療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005320         07 電療料４５ＷＲ                PIC 9(4)  VALUE ZERO.
005330      05 小計４５ＷＲ                     PIC 9(6)  VALUE ZERO.
005340      05 多部位込小計４５ＷＲ             PIC 9(6)  VALUE ZERO.
005350      05 長期逓減率４５ＷＲ               PIC 9(3)  VALUE ZERO.
005360      05 長期込小計４５ＷＲ               PIC 9(6)  VALUE ZERO.
005370****************
005380* ４部位／８割 *
005390****************
005400   03 部位４８ＷＲ.
005410      05 逓減開始月日４８ＷＲ.
005420         07 逓減開始月４８ＷＲ            PIC 9(2)  VALUE ZERO.
005430         07 逓減開始日４８ＷＲ            PIC 9(2)  VALUE ZERO.
005440      05 後療４８ＷＲ.
005450         07 後療単価４８ＷＲ              PIC 9(4)  VALUE ZERO.
005460         07 後療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
005470         07 後療料４８ＷＲ                PIC 9(5)  VALUE ZERO.
005480      05 冷罨法４８ＷＲ.
005490         07 冷罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
005500         07 冷罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
005510      05 温罨法４８ＷＲ.
005520         07 温罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
005530         07 温罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
005540      05 電療４８ＷＲ.
005550         07 電療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
005560         07 電療料４８ＷＲ                PIC 9(4)  VALUE ZERO.
005570      05 小計４８ＷＲ                     PIC 9(6)  VALUE ZERO.
005580      05 多部位込小計４８ＷＲ             PIC 9(6)  VALUE ZERO.
005590      05 長期逓減率４８ＷＲ               PIC 9(3)  VALUE ZERO.
005600      05 長期込小計４８ＷＲ               PIC 9(6)  VALUE ZERO.
005610******************
005620* ４部位／１０割 *
005630******************
005640   03 部位４０ＷＲ.
005650      05 逓減開始月日４０ＷＲ.
005660         07 逓減開始月４０ＷＲ            PIC 9(2)  VALUE ZERO.
005670         07 逓減開始日４０ＷＲ            PIC 9(2)  VALUE ZERO.
005680      05 後療４０ＷＲ.
005690         07 後療単価４０ＷＲ              PIC 9(4)  VALUE ZERO.
005700         07 後療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
005710         07 後療料４０ＷＲ                PIC 9(5)  VALUE ZERO.
005720      05 冷罨法４０ＷＲ.
005730         07 冷罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
005740         07 冷罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
005750      05 温罨法４０ＷＲ.
005760         07 温罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
005770         07 温罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
005780      05 電療４０ＷＲ.
005790         07 電療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
005800         07 電療料４０ＷＲ                PIC 9(4)  VALUE ZERO.
005810      05 小計４０ＷＲ                     PIC 9(6)  VALUE ZERO.
005820      05 長期逓減率４０ＷＲ               PIC 9(3)  VALUE ZERO.
005830      05 長期込小計４０ＷＲ               PIC 9(6)  VALUE ZERO.
005840********************
005850* ５部位／２．５割 *
005860********************
005870   03 部位５２ＷＲ.
005880      05 後療５２ＷＲ.
005890         07 後療単価５２ＷＲ              PIC 9(4)  VALUE ZERO.
005900         07 後療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
005910         07 後療料５２ＷＲ                PIC 9(5)  VALUE ZERO.
005920      05 冷罨法５２ＷＲ.
005930         07 冷罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
005940         07 冷罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
005950      05 温罨法５２ＷＲ.
005960         07 温罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
005970         07 温罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
005980      05 電療５２ＷＲ.
005990         07 電療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
006000         07 電療料５２ＷＲ                PIC 9(4)  VALUE ZERO.
006010      05 小計５２ＷＲ                     PIC 9(6)  VALUE ZERO.
006020      05 多部位込小計５２ＷＲ             PIC 9(6)  VALUE ZERO.
006030      05 長期逓減率５２ＷＲ               PIC 9(3)  VALUE ZERO.
006040      05 長期込小計５２ＷＲ               PIC 9(6)  VALUE ZERO.
006050****************
006060* ５部位／５割 *
006070****************
006080   03 部位５５ＷＲ.
006090      05 逓減開始月日５５ＷＲ.
006100         07 逓減開始月５５ＷＲ            PIC 9(2)  VALUE ZERO.
006110         07 逓減開始日５５ＷＲ            PIC 9(2)  VALUE ZERO.
006120      05 後療５５ＷＲ.
006130         07 後療単価５５ＷＲ              PIC 9(4)  VALUE ZERO.
006140         07 後療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
006150         07 後療料５５ＷＲ                PIC 9(5)  VALUE ZERO.
006160      05 冷罨法５５ＷＲ.
006170         07 冷罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
006180         07 冷罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
006190      05 温罨法５５ＷＲ.
006200         07 温罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
006210         07 温罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
006220      05 電療５５ＷＲ.
006230         07 電療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
006240         07 電療料５５ＷＲ                PIC 9(4)  VALUE ZERO.
006250      05 小計５５ＷＲ                     PIC 9(6)  VALUE ZERO.
006260      05 多部位込小計５５ＷＲ             PIC 9(6)  VALUE ZERO.
006270      05 長期逓減率５５ＷＲ               PIC 9(3)  VALUE ZERO.
006280      05 長期込小計５５ＷＲ               PIC 9(6)  VALUE ZERO.
006290****************
006300* ５部位／８割 *
006310****************
006320   03 部位５８ＷＲ.
006330      05 逓減開始月日５８ＷＲ.
006340         07 逓減開始月５８ＷＲ            PIC 9(2)  VALUE ZERO.
006350         07 逓減開始日５８ＷＲ            PIC 9(2)  VALUE ZERO.
006360      05 後療５８ＷＲ.
006370         07 後療単価５８ＷＲ              PIC 9(4)  VALUE ZERO.
006380         07 後療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
006390         07 後療料５８ＷＲ                PIC 9(5)  VALUE ZERO.
006400      05 冷罨法５８ＷＲ.
006410         07 冷罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
006420         07 冷罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
006430      05 温罨法５８ＷＲ.
006440         07 温罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
006450         07 温罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
006460      05 電療５８ＷＲ.
006470         07 電療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
006480         07 電療料５８ＷＲ                PIC 9(4)  VALUE ZERO.
006490      05 小計５８ＷＲ                     PIC 9(6)  VALUE ZERO.
006500      05 多部位込小計５８ＷＲ             PIC 9(6)  VALUE ZERO.
006510      05 長期逓減率５８ＷＲ               PIC 9(3)  VALUE ZERO.
006520      05 長期込小計５８ＷＲ               PIC 9(6)  VALUE ZERO.
006530******************
006540* ５部位／１０割 *
006550******************
006560   03 部位５０ＷＲ.
006570      05 逓減開始月日５０ＷＲ.
006580         07 逓減開始月５０ＷＲ            PIC 9(2)  VALUE ZERO.
006590         07 逓減開始日５０ＷＲ            PIC 9(2)  VALUE ZERO.
006600      05 後療５０ＷＲ.
006610         07 後療単価５０ＷＲ              PIC 9(4)  VALUE ZERO.
006620         07 後療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
006630         07 後療料５０ＷＲ                PIC 9(5)  VALUE ZERO.
006640      05 冷罨法５０ＷＲ.
006650         07 冷罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
006660         07 冷罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
006670      05 温罨法５０ＷＲ.
006680         07 温罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
006690         07 温罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
006700      05 電療５０ＷＲ.
006710         07 電療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
006720         07 電療料５０ＷＲ                PIC 9(4)  VALUE ZERO.
006730      05 小計５０ＷＲ                     PIC 9(6)  VALUE ZERO.
006740      05 長期逓減率５０ＷＲ               PIC 9(3)  VALUE ZERO.
006750      05 長期込小計５０ＷＲ               PIC 9(6)  VALUE ZERO.
006760*
006770**************
006780* 施術所情報 *
006790**************
006800 01 施術所情報Ｗ.
006810    03 柔整師番号Ｗ                    PIC X(22)  VALUE SPACE.
006820    03 柔整師番号２Ｗ                  PIC X(24)  VALUE SPACE.
006830    03 接骨師会会員番号Ｗ.
006840       05 会ＪＢＷ                     PIC X(6)   VALUE SPACE.
006850       05 会員番号Ｗ                   PIC X(10)  VALUE SPACE.
006860    03 代表者カナＷ                    PIC X(50)  VALUE SPACE.
006870    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
006880    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
          03 都道府県ＪＩＳＷ                PIC X(2)   VALUE SPACE.
006890    03 施術所住所Ｗ.
006900       05 施術所住所１Ｗ               PIC X(50)  VALUE SPACE.
006910       05 施術所住所２Ｗ               PIC X(50)  VALUE SPACE.
006920*    03 施術所住所Ｗ.
006930*       05 施術所住所１Ｗ               PIC X(28)  VALUE SPACE.
006940*       05 施術所住所２Ｗ               PIC X(28)  VALUE SPACE.
006950*       05 施術所住所３Ｗ               PIC X(28)  VALUE SPACE.
006960*
006970    03 施術所郵便番号Ｗ.
006980       05 施術所郵便番号１Ｗ           PIC X(3)   VALUE SPACE.
006990       05 施術所郵便番号２Ｗ           PIC X(4)   VALUE SPACE.
007000    03 施術所電話番号Ｗ                PIC X(15)  VALUE SPACE.
007010    03 定額制受理番号Ｗ                PIC X(15)  VALUE SPACE.
007020    03 受理年月日Ｗ.
007030       05 受理年Ｗ                     PIC 9(2)   VALUE ZERO.
007040       05 受理月Ｗ                     PIC 9(2)   VALUE ZERO.
007050       05 受理日Ｗ                     PIC 9(2)   VALUE ZERO.
007060    03 最終通院年月日Ｗ.
007070       05 最終通院年Ｗ                 PIC 9(2)   VALUE ZERO.
007080       05 最終通院月Ｗ                 PIC 9(2)   VALUE ZERO.
007090       05 最終通院日Ｗ                 PIC 9(2)   VALUE ZERO.
007100    03 柔整師年月日Ｗ.
007110       05 柔整師年Ｗ                   PIC 9(2)   VALUE ZERO.
007120       05 柔整師月Ｗ                   PIC 9(2)   VALUE ZERO.
007130       05 柔整師日Ｗ                   PIC 9(2)   VALUE ZERO.
007140    03 患者委任年月日Ｗ.
007150       05 患者委任年Ｗ                 PIC 9(2)   VALUE ZERO.
007160       05 患者委任月Ｗ                 PIC 9(2)   VALUE ZERO.
007170       05 患者委任日Ｗ                 PIC 9(2)   VALUE ZERO.
007180    03 取引先情報Ｗ.
007190       05 取引先銀行名Ｗ               PIC X(40)  VALUE SPACE.
007200       05 取引先銀行支店名Ｗ           PIC X(40)  VALUE SPACE.
007210       05 預金種別Ｗ                   PIC 9(1)   VALUE ZERO.
007220       05 口座番号Ｗ                   PIC X(10)  VALUE SPACE.
007230       05 口座名義人Ｗ                 PIC X(40)  VALUE SPACE.
007240       05 口座名義人カナＷ             PIC X(40)  VALUE SPACE.
007250       05 銀行名支店名Ｗ               PIC X(60)  VALUE SPACE.
007260       05 預金種別コメントＷ           PIC N(2)   VALUE SPACE.
008660    03 県施術ＩＤＷ                    PIC X(15)  VALUE SPACE.
007270**************
007280* 受診者情報 *
007290**************
007380 01 受診者情報Ｗ.
      */元号修正/20190405
          03 施術和暦Ｗ                      PIC 9(1)   VALUE ZERO.
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
007390    03 施術年月Ｗ.
007400       05 施術年Ｗ                     PIC 9(2)   VALUE ZERO.
007410       05 施術月Ｗ                     PIC 9(2)   VALUE ZERO.
007420*    03 記号Ｗ                          PIC N(12)  VALUE SPACE.
007570    03 記号Ｗ.
007580       05 印刷記号Ｗ                   PIC N(12)  VALUE SPACE.
007430*
007440    03 番号Ｗ.
007450       05 印刷番号１Ｗ                 PIC X(20)  VALUE SPACE.
007460*       05 印刷番号２Ｗ                 PIC X(10)  VALUE SPACE.
007470       05 FILLER                       PIC X(10)  VALUE SPACE.
          03 記号番号Ｗ.
             05 記号番号ＸＷ                 PIC X(40) VALUE SPACE.
007480*
007490    03 保険者番号Ｗ.
007500       05 印刷保険者番号Ｗ             PIC X(8)   VALUE SPACE.
007510       05 FILLER                       PIC X(2)   VALUE SPACE.
          03 公費負担者番号Ｗ                PIC X(8)   VALUE SPACE.
      *    03 受給者番号Ｗ                    PIC X(20)  VALUE SPACE.
          03 受給者番号Ｗ.
             05 印刷受給者番号Ｗ             PIC X(7)  VALUE SPACE.
             05 印刷受給者番号２Ｗ           PIC X(8)  VALUE SPACE.
007520*
007530    03 請求先名称Ｗ.
007540       05 印刷請求先名称１Ｗ           PIC X(40)  VALUE SPACE.
007550       05 印刷請求先名称２Ｗ           PIC X(40)  VALUE SPACE.
007560*
007570    03 保険種別Ｗ                      PIC 9(2)   VALUE ZERO.
007580    03 助成印Ｗ                        PIC N(1)   VALUE SPACE.
007590    03 被保険者情報Ｗ.
007600       05 被保険者カナＷ               PIC X(50)  VALUE SPACE.
007610       05 被保険者氏名Ｗ               PIC X(50)  VALUE SPACE.
007620       05 被保険者性別Ｗ               PIC N(1)   VALUE SPACE.
007630       05 郵便番号Ｗ.
007640          07 郵便番号１Ｗ              PIC X(3)   VALUE SPACE.
007650          07 郵便番号２Ｗ              PIC X(4)   VALUE SPACE.
007660       05 被保険者住所Ｗ.
007670          07 被保険者住所１Ｗ          PIC X(50)  VALUE SPACE.
007680          07 被保険者住所２Ｗ          PIC X(50)  VALUE SPACE.
008990       05 電話番号Ｗ                   PIC X(35)  VALUE SPACE.
007690    03 患者情報Ｗ.
009100       05 患者郵便番号Ｗ.
009110          07 患者郵便番号１Ｗ          PIC X(3)  VALUE SPACE.
009120          07 患者郵便番号２Ｗ          PIC X(4)  VALUE SPACE.
009130       05 患者住所１Ｗ                 PIC X(50) VALUE SPACE.
009140       05 患者住所２Ｗ                 PIC X(50) VALUE SPACE.
008990       05 患者電話番号Ｗ               PIC X(35)  VALUE SPACE.
007700       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
007710       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
007720       05 患者性別Ｗ                   PIC N(1)   VALUE SPACE.
007730       05 性別チェックＷ.
007740          07 男チェックＷ              PIC N(1)  VALUE SPACE.
007750          07 女チェックＷ              PIC N(1)  VALUE SPACE.
007760       05 和暦チェックＷ.
007770          07 明治チェックＷ            PIC N(1)  VALUE SPACE.
007780          07 大正チェックＷ            PIC N(1)  VALUE SPACE.
007790          07 昭和チェックＷ            PIC N(1)  VALUE SPACE.
007800          07 平成チェックＷ            PIC N(1)  VALUE SPACE.
      */元号修正/↓↓↓20190405
008210          07 令和チェックＷ            PIC N(1)  VALUE SPACE.
                07 令和ＣＭＷ                PIC X(4)  VALUE SPACE.
      */元号修正/↑↑↑20190405
007810          07 元号Ｗ                    PIC N(2)  VALUE SPACE.
007820       05 患者年Ｗ                     PIC 9(2)  VALUE ZERO.
007830       05 患者月Ｗ                     PIC 9(2)  VALUE ZERO.
007840       05 患者日Ｗ                     PIC 9(2)  VALUE ZERO.
007850       05 続柄Ｗ.
007860          07 印刷続柄Ｗ                PIC N(4)  VALUE SPACE.
007870          07 FILLER                    PIC X(4)  VALUE SPACE.
007880*
007890*       05 負傷原因Ｗ                   PIC N(40) OCCURS 29 VALUE SPACE.
      */半角対応/110421
             05 負傷原因Ｗ OCCURS 29.
                07 負傷原因ＸＷ              PIC X(80)  VALUE SPACE.
007900*
007910 01 特別区分チェックＷ.
007920    03 ３歳未満チェックＷ              PIC N(1)  VALUE SPACE.
007930    03 ７０歳チェックＷ                PIC N(1)  VALUE SPACE.
007940    03 ７０歳１チェックＷ              PIC N(1)  VALUE SPACE.
007941    03 ７０歳２チェックＷ              PIC N(1)  VALUE SPACE.
007950    03 ７０歳３チェックＷ              PIC N(1)  VALUE SPACE.
          03 義務教育就学前Ｗ                PIC N(7)  VALUE SPACE.
007960*
007970****************
007980* 負傷データＦ *
007990****************
008000 01 負傷情報Ｗ.
008010    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
008020    03 部位情報Ｗ  OCCURS   9.
008030       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
008040       05 部位コードＷ.
008050          07 負傷種別Ｗ                PIC 9(2)  VALUE ZERO.
008060          07 部位Ｗ                    PIC 9(2)  VALUE ZERO.
008070          07 左右区分Ｗ                PIC 9(1)  VALUE ZERO.
008080          07 負傷位置番号Ｗ            PIC 9(2)  VALUE ZERO.
008090       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
008100       05 負傷年月日Ｗ.
008110          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
008120          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
008130          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
008140       05 初検年月日Ｗ.
008150          07 初検年Ｗ                  PIC 9(2)  VALUE ZERO.
008160          07 初検月Ｗ                  PIC 9(2)  VALUE ZERO.
008170          07 初検日Ｗ                  PIC 9(2)  VALUE ZERO.
008180       05 開始年月日Ｗ.
008190          07 開始年Ｗ                  PIC 9(2)  VALUE ZERO.
008200          07 開始月Ｗ                  PIC 9(2)  VALUE ZERO.
008210          07 開始日Ｗ                  PIC 9(2)  VALUE ZERO.
008220       05 終了年月日Ｗ.
008230          07 終了年Ｗ                  PIC 9(2)  VALUE ZERO.
008240          07 終了月Ｗ                  PIC 9(2)  VALUE ZERO.
008250          07 終了日Ｗ                  PIC 9(2)  VALUE ZERO.
008260       05 実日数Ｗ                     PIC 9(2)  VALUE ZERO.
008270       05 転帰区分Ｗ                   PIC 9(1)  VALUE ZERO.
008280       05 転帰区分チェックＷ.
008290          07 治癒チェックＷ            PIC N(1)  VALUE SPACE.
008300          07 中止チェックＷ            PIC N(1)  VALUE SPACE.
008310          07 転医チェックＷ            PIC N(1)  VALUE SPACE.
008320       05 転帰Ｗ                       PIC N(2)  VALUE SPACE.
008330       05 開始年月日取得フラグ         PIC X(3)  VALUE SPACE.
008340       05 部位区切Ｗ                   PIC X(1)  VALUE SPACE.
008350       05 経過略称Ｗ.
008360          07 印刷経過略称Ｗ            PIC N(5)  VALUE SPACE.
008370          07 FILLER                    PIC X(2)  VALUE SPACE.
008380    03 経過部位Ｗ                      PIC N(1)  VALUE SPACE.
009030    03 経過ＣＭ                        PIC N(4)  VALUE SPACE.
008390    03 新規チェックＷ                  PIC N(1)  VALUE SPACE.
008400    03 継続チェックＷ                  PIC N(1)  VALUE SPACE.
008410    03 請求区分Ｗ                      PIC N(2)  VALUE SPACE.
008420*
008430************
008440* 料金情報 *
008450************
008460 01 料金情報Ｗ.
008470    03 初検加算Ｗ.
008480       05 時間外チェックＷ                PIC N(1) VALUE SPACE.
008490       05 休日チェックＷ                  PIC N(1) VALUE SPACE.
008500       05 深夜チェックＷ                  PIC N(1) VALUE SPACE.
008510       05 時間外Ｗ                        PIC N(3) VALUE SPACE.
008520       05 休日Ｗ                          PIC N(2) VALUE SPACE.
008530       05 深夜Ｗ                          PIC N(2) VALUE SPACE.
008540       05 初検加算内容Ｗ                  PIC N(10) VALUE SPACE.
008550    03 往療加算Ｗ.
008560       05 夜間チェックＷ                  PIC N(1) VALUE SPACE.
008570       05 難路チェックＷ                  PIC N(1) VALUE SPACE.
008580       05 暴風雨雪チェックＷ              PIC N(1) VALUE SPACE.
008590    03 金属副子チェックＷ.
008600       05 大チェックＷ                    PIC N(1) VALUE SPACE.
008610       05 中チェックＷ                    PIC N(1) VALUE SPACE.
008620       05 小チェックＷ                    PIC N(1) VALUE SPACE.
008630       05 金属大Ｗ                        PIC N(1) VALUE SPACE.
008640       05 金属中Ｗ                        PIC N(1) VALUE SPACE.
008650       05 金属小Ｗ                        PIC N(1) VALUE SPACE.
008660    03 小計Ｗ                             PIC 9(7) VALUE ZERO.
008670    03 初回処置料合計Ｗ                   PIC 9(6) VALUE ZERO.
008680    03 初回処置料チェックＷ.
008690       05 整復料チェックＷ                PIC N(1) VALUE SPACE.
008700       05 固定料チェックＷ                PIC N(1) VALUE SPACE.
008710       05 施療料チェックＷ                PIC N(1) VALUE SPACE.
      */金属副子・運動後療の変更・追加/1805
          03 金属回数Ｗ                         PIC 9(2)  VALUE ZERO.
          03 運動料Ｗ                           PIC 9(4)  VALUE ZERO.
008720************
008730* 備考情報 *
008740************
008750 01 備考情報Ｗ.
008760    03 適用１Ｗ                           PIC N(38) VALUE SPACE.
008770    03 適用２Ｗ                           PIC N(38) VALUE SPACE.
008780*    03 適用３Ｗ                        PIC N(38) VALUE SPACE.
008790*    03 適用４Ｗ                        PIC N(38) VALUE SPACE.
008800*    03 経過コメントＷ                     PIC N(60) VALUE SPACE.
008810*
       01 摘要施術日Ｗ                       PIC X(100) VALUE SPACE.
       01 施術日Ｗ.
          03 施術日２Ｗ                      PIC X(1)  VALUE SPACE.
          03 施術日１Ｗ                      PIC X(1)  VALUE SPACE.
004270* 保険者番号
004280 01 保険者番号比較Ｗ                   PIC X(6)  VALUE SPACE.
008820***
008830 01 印刷制御.
008840     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
008850     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
008860     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
008870     03 拡張制御Ｐ.
008880         05 端末制御Ｐ.
008890             07 移動方向Ｐ             PIC X(1) VALUE SPACE.
008900             07 移動行数Ｐ             PIC 9(3) VALUE ZERO.
008910         05 詳細制御Ｐ                 PIC X(2) VALUE SPACE.
008920     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
008930     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
008940*
008950 01 計算機西暦年Ｗ                     PIC 9(2) VALUE ZERO.
008960* 日付ＷＯＲＫ
008970 01 和暦終了年Ｗ                       PIC 9(4) VALUE ZERO.
008980 01 計算機西暦.
008990    03 計算機西暦年                    PIC 9(4) VALUE ZERO.
009000    03 計算機西暦月日                  PIC 9(4) VALUE ZERO.
009010 01 計算機西暦Ｒ REDEFINES 計算機西暦.
009020    03 計算機世紀                      PIC 9(2).
009030    03 計算機日付                      PIC 9(6).
009040    03 計算機日付Ｒ REDEFINES 計算機日付.
009050       05 計算機年月                   PIC 9(4).
009060       05 計算機年月Ｒ REDEFINES 計算機年月.
009070         07 計算機年                   PIC 9(2).
009080         07 計算機月                   PIC 9(2).
009090       05 計算機日                     PIC 9(2).
009100*
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
009110******************************************************************
009120*                          連結項目                              *
009130******************************************************************
009140*
009150*
009160**  画面入力データ
009170 01 連入－入力データ委任印刷 IS EXTERNAL.
009180    03 連入－委任印刷                     PIC 9.
       01 連入－入力データ電話印刷 IS EXTERNAL.
          03 連入－電話印刷                     PIC 9.
009190*
       01 連入－プレビュー IS EXTERNAL.
          03 連入－プレビュー区分          PIC 9.
009200*
009210************
009220* 印刷キー *
009230************
009240*
009250 01 連レ印－対象データ IS EXTERNAL.
009260    03 連レ印－施術年月日.
009270       05 連レ印－施術和暦                  PIC 9(1).
009280       05 連レ印－施術年                    PIC 9(2).
009290       05 連レ印－施術月                    PIC 9(2).
009300    03 連レ印－患者コード.
009310       05 連レ印－患者番号                  PIC 9(6).
009320       05 連レ印－枝番                      PIC X(1).
009330    03 連レ印－保険種別                     PIC 9(2).
009340    03 連レ印－保険者番号                   PIC X(10).
009350    03 連レ印－公費種別                     PIC 9(2).
009360    03 連レ印－費用負担者番号               PIC X(10).
009370    03 連レ印－助成種別                     PIC 9(2).
009380    03 連レ印－費用負担者番号助成           PIC X(10).
009390    03 連レ印－患者カナ                     PIC X(20).
009400    03 連レ印－本人家族区分                 PIC 9(1).
012510*
012840 01 連レ－キー IS EXTERNAL.
012850    03 連レ－保険種別                  PIC 9(2).
      *
       01 連レ－長野キー IS EXTERNAL GLOBAL.
          03 連レ－長野助成                  PIC 9(2).
012860************************
012870** ３カ月長期判定
012880************************
012890 01 連期間－キー IS EXTERNAL.
012900    03 連期間－施術年月.
012910       05 連期間－施術和暦               PIC 9.
012920       05 連期間－施術年                 PIC 9(2).
012930       05 連期間－施術月                 PIC 9(2).
012940    03  連期間－患者コード.
012950       05 連期間－患者番号               PIC 9(6).
012960       05 連期間－枝番                   PIC X.
012970    03 連期間－対象フラグ                PIC X(3).
012980    03 連期間－期間月Ｗ.
012990       05 連期間－期間Ｗ                 PIC 9(2) OCCURS 9.
013000*
013010************************
013020* 長期理由文セット     *
013030************************
013040 01 連長文－キー IS EXTERNAL.
013050    03 連長文－施術年月.
013060       05 連長文－施術和暦               PIC 9.
013070       05 連長文－施術年                 PIC 9(2).
013080       05 連長文－施術月                 PIC 9(2).
013090    03  連長文－患者コード.
013100       05 連長文－患者番号               PIC 9(6).
013110       05 連長文－枝番                   PIC X.
013120    03 連長文－文桁数                    PIC 9(2).
013130    03 連長文－理由文                    PIC N(63) OCCURS 15.
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
014370*
013141************************
013142* 助成レセまとめ
013143************************
013144 01 連レセまとめ－キー IS EXTERNAL.
013145    03 連レセまとめ－施術和暦年月.
013146       05 連レセまとめ－施術和暦               PIC 9.
013147       05 連レセまとめ－施術年月.
013148          07 連レセまとめ－施術年              PIC 9(2).
013149          07 連レセまとめ－施術月              PIC 9(2).
013150    03 連レセまとめ－患者コード.
013151       05 連レセまとめ－患者番号               PIC 9(6).
013152       05 連レセまとめ－枝番                   PIC X(1).
013153**-------------------------------------------------------**
013154*   1:助成レセプトなしの本体まとめの判定
013155*   2:横浜・川崎用の社保助成レセかの判定
013156    03 連レセまとめ－判定区分                  PIC 9.
013157**-------------------------------------------------------**
013158*  / OUT /　 0:対象外、1:対象
013159    03 連レセまとめ－判定結果                  PIC 9.
013160**
013161*
013162*
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
014070 01 連助成名称－会キー IS EXTERNAL.
014080    03 連助成名称－協会コード           PIC 9(2).
002850*
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
      */負担率取得ＰＧの対応
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
013181******************************************************************
013182*                      PROCEDURE  DIVISION                       *
013183******************************************************************
013184 PROCEDURE               DIVISION.
013190************
013200*           *
013210* 初期処理   *
013220*           *
013230************
013235*
002570     PERFORM プリンタファイル作成.
013240     PERFORM 初期化.
013250************
013260*           *
013270* 主処理     *
013280*           *
013290************
013300* 印刷
013310     PERFORM 連結項目待避.
013320     PERFORM 印刷セット.
013330     PERFORM 印刷処理.
013340************
013350*           *
013360* 終了処理   *
013370*           *
013380************
013390     PERFORM 受診者印刷区分更新.
013400     PERFORM 終了処理.
013410     MOVE ZERO  TO PROGRAM-STATUS.
013420     EXIT PROGRAM.
013430*
013440*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YJB6125"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分  TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
013450*================================================================*
013460 初期化 SECTION.
013470*
013480     PERFORM ファイルオープン.
013490*    /* 現在日付取得 */
013500     ACCEPT 計算機日付 FROM DATE.
013510*    /* 1980～2079年の間で設定 */
013520     IF 計算機年 > 80
013530         MOVE 19 TO 計算機世紀
013540     ELSE
013550         MOVE 20 TO 計算機世紀
013560     END-IF.
013570     PERFORM カレント元号取得.
013580     PERFORM 和暦終了年取得.
013590     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 和暦終了年Ｗ.
013600*================================================================*
013610 カレント元号取得 SECTION.
013620*
013630     MOVE ZEROS TO 制－制御区分.
013640     READ 制御情報マスタ
013650     NOT INVALID KEY
013660         MOVE 制－カレント元号         TO カレント元号Ｗ
013670         MOVE 制－レセ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
013680         MOVE 制－レセ長期理由印刷区分 TO 長期理由印刷区分Ｗ
013690         MOVE 制－レセプト日付区分     TO レセプト日付区分Ｗ
013700         MOVE 制－レセプト患者日付区分 TO レセプト患者日付区分Ｗ
013710     END-READ.
013720*
013730*================================================================*
013740 和暦終了年取得 SECTION.
013750*
013760*     DISPLAY NC"カレント元号Ｗ"  カレント元号Ｗ UPON MSGBOX.
013770     MOVE カレント元号Ｗ TO 元－元号区分.
013780     READ 元号マスタ
013790     INVALID KEY
013800         DISPLAY NC"指定和暦が登録されていません" UPON CONS
013810         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
013820                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
013830         ACCEPT  キー入力 FROM CONS
013840         PERFORM 終了処理
013850         EXIT PROGRAM
013860     NOT INVALID KEY
013870         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
013880         MOVE 前和暦Ｗ TO 元－元号区分
013890         READ 元号マスタ
013900         INVALID KEY
013910             DISPLAY NC"指定和暦が登録されていません" UPON CONS
013920             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
013930                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
013940             ACCEPT  キー入力 FROM CONS
013950             PERFORM 終了処理
013960             EXIT PROGRAM
013970         NOT INVALID KEY
013980             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
013990         END-READ
014000     END-READ.
014010*
014020*================================================================*
014030 ファイルオープン SECTION.
014040*
014050     OPEN INPUT   保険者マスタ
014060         MOVE NC"保険者" TO ファイル名.
014070         PERFORM オープンチェック.
014071     OPEN INPUT 市町村マスタ.
014072         MOVE NC"市町村" TO ファイル名.
014073         PERFORM オープンチェック.
014080     OPEN INPUT   元号マスタ
014090         MOVE NC"元号" TO ファイル名.
014100         PERFORM オープンチェック.
014110     OPEN INPUT   名称マスタ
014120         MOVE NC"名称" TO ファイル名.
014130         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
014170     OPEN INPUT   制御情報マスタ
014180         MOVE NC"制御情報" TO ファイル名.
014190         PERFORM オープンチェック.
014200     OPEN INPUT   施術所情報マスタ
014210         MOVE NC"施情" TO ファイル名.
014220         PERFORM オープンチェック.
014230*     OPEN INPUT   請求先マスタ
014240*         MOVE NC"請先" TO ファイル名.
014250*         PERFORM オープンチェック.
014260     OPEN INPUT   経過マスタ
014270         MOVE NC"経過" TO ファイル名.
014280         PERFORM オープンチェック.
014290     OPEN INPUT   施術記録Ｆ.
014300         MOVE NC"施記Ｆ" TO ファイル名.
014310         PERFORM オープンチェック.
014320     OPEN INPUT   負傷データＦ.
014330         MOVE NC"負傷" TO ファイル名.
014340         PERFORM オープンチェック.
014350     OPEN INPUT   負傷原因Ｆ.
014360         MOVE NC"負傷原因" TO ファイル名.
014370         PERFORM オープンチェック.
014380     OPEN INPUT メモファイル.
014390         MOVE NC"メモ" TO ファイル名.
014400         PERFORM オープンチェック.
015560     OPEN INPUT   受診者情報２Ｆ.
015570         MOVE NC"受診者情報２Ｆ" TO ファイル名.
015580         PERFORM オープンチェック.
016120     OPEN INPUT   ＩＤ管理マスタ
016130         MOVE NC"ＩＤ" TO ファイル名.
016140         PERFORM オープンチェック.
014410     OPEN I-O   受診者情報Ｆ.
014420         MOVE NC"受情" TO ファイル名.
014430         PERFORM オープンチェック.
014440     OPEN I-O   印刷ファイル
014450         PERFORM エラー処理Ｐ.
014460*================================================================*
014470 オープンチェック SECTION.
014480*
014490     IF 状態キー  NOT =  "00"
014500         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
014510         DISPLAY NC"状態キー：" 状態キー         UPON CONS
014520         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
014530                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
014540         ACCEPT  キー入力 FROM CONS
014550         PERFORM ファイル閉鎖
014560         EXIT PROGRAM.
014570*================================================================*
014580 連結項目待避 SECTION.
014590*
014600     MOVE 連レ印－施術和暦           TO 施術和暦ＷＲ.
014610     MOVE 連レ印－施術年             TO 施術年ＷＲ.
014620     MOVE 連レ印－施術月             TO 施術月ＷＲ.
014630     MOVE 連レ印－保険種別           TO 保険種別ＷＲ.
014640     MOVE 連レ印－保険者番号         TO 保険者番号ＷＲ.
014650     MOVE 連レ印－公費種別           TO 公費種別ＷＲ.
014660     MOVE 連レ印－費用負担者番号     TO 費用負担者番号ＷＲ.
014670     MOVE 連レ印－助成種別           TO 助成種別ＷＲ.
014680     MOVE 連レ印－費用負担者番号助成 TO 費用負担者番号助成ＷＲ.
014690     MOVE 連レ印－本人家族区分       TO 本人家族区分ＷＲ.
014700     MOVE 連レ印－患者カナ           TO 患者カナＷＲ.
014710     MOVE 連レ印－患者番号           TO 患者番号ＷＲ.
014720     MOVE 連レ印－枝番               TO 枝番ＷＲ.
014730*================================================================*
014740 印刷セット SECTION.
014750*
014760     PERFORM 項目初期化.
014800     PERFORM 負傷読込.
014810     PERFORM 料金情報取得.
014770     PERFORM 施術所情報取得.
014780     PERFORM 請求先情報取得.
           PERFORM レセ用紙情報取得.
014790     PERFORM 受診者情報取得.
014800     PERFORM 負傷データ取得.
014820     PERFORM 施術記録取得.
014830*******     PERFORM 長期判定取得.
014840*******     PERFORM 初検日以前のデータ判定.
014850     PERFORM 初検加算時刻取得.
014860     PERFORM 助成印取得.
014870     PERFORM 委任年月日取得.
026961**
014890* / 退職のみ負担率取得 /
014900     IF ( 保険種別ＷＲ = 08 )
014910        PERFORM 負担率取得
014920     END-IF.
014930*
014940* / 制御マスタ・負傷データＦの印刷区分を確認し取得 /
016791*-----------------------------------------------*
016800     IF ( 負傷原因印刷区分Ｗ  NOT = 1 ) AND ( 負傷原因印刷区分Ｆ NOT = 1 )
016813        IF ( 負傷原因印刷区分Ｗ = 3 OR 4 )
016815           PERFORM 負傷原因印刷対象判定処理
016817        ELSE
016820           PERFORM 負傷原因取得
016821        END-IF
016830     END-IF.
016831*-----------------------------------------------*
015080********************
015090* 受診者情報セット *
015100********************
015110*
      */2506用紙に対応/130614
           IF 一般レセＷ >= 4
               MOVE "1.男" TO 男ＣＭ
               MOVE "2.女" TO 女ＣＭ
               MOVE NC"年" TO 生年ＣＭ
               MOVE NC"月" TO 生月ＣＭ
               MOVE NC"日" TO 生日ＣＭ
               MOVE "1.明" TO 明治ＣＭ
               MOVE "2.大" TO 大正ＣＭ
               MOVE "3.昭" TO 昭和ＣＭ
               MOVE "4.平" TO 平成ＣＭ
           END-IF.
      */千葉県子ども医療費助成事業
           IF (連レ－保険種別 >=  50  ) AND
              (受－助成種別    = "60" ) AND
              (受－費用負担者番号助成(1:4) = "8312" )
               MOVE "千葉県子ども医療費助成事業" TO 助成タイトル
           END-IF
      */千葉県重度心身障害医療費助成事業 本体も重心を書く150914
      *     IF (連レ－保険種別 >=  50  ) AND
           IF (受－助成種別    = "53" ) AND
              (受－費用負担者番号助成(1:4) = "8112" )
               IF (連レ－保険種別 >=  50  )
                   MOVE "千葉県重度心身障害者（児）医療費助成" TO 助成タイトル
               END-IF
               MOVE NC"重心"         TO 重心
               MOVE NC"○"           TO 重心丸
           END-IF
      */千葉県ひとり親医療費助成事業/201001
           IF (連レ－保険種別 > 50) AND
              (受－助成種別 = "52" ) AND (受－費用負担者番号助成(1:4) = "8512" )
               MOVE "千葉県ひとり親家庭等医療費等助成事業" TO 助成タイトル
           END-IF.
      *
015190     MOVE 社保チェックＷ     TO 社保チェック.
015210     MOVE 組合チェックＷ     TO 組合チェック.
015220     MOVE 国保チェックＷ     TO 国保チェック.
           MOVE 共済チェックＷ     TO 共済チェック.
           MOVE 自チェックＷ       TO 自チェック.
           IF 自チェック NOT = SPACE
               MOVE NC"自"         TO 自ＣＭ
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
015190*
      */元号修正/↓↓↓20190405
037370     IF 施術和暦Ｗ > 4
              MOVE 施術和暦Ｗ         TO 元－元号区分
037380        READ 元号マスタ
037390        NOT INVALID KEY
037400            MOVE 元－元号名称   TO 施術和暦
037410        END-READ
              MOVE "===="             TO 施術和暦訂正
           END-IF.
      */元号修正/↑↑↑20190405
015200     MOVE 施術年Ｗ            TO 施術年.
015210     MOVE 施術月Ｗ            TO 施術月.
015220*
015230*
015240*     IF 記号Ｗ(1:1) = NC"＊" 
015250*        MOVE  SPACE    TO  記号
015260*     ELSE
015270*        MOVE 記号Ｗ    TO  記号
015280*     END-IF.
015290**
015300*     IF ( 印刷番号１Ｗ(1:1) = "*"  ) OR
015310*        ( 印刷番号１Ｗ(1:2) = "＊" )
015320*        MOVE  SPACE         TO 番号
015330*     ELSE
015340*        MOVE 印刷番号１Ｗ   TO 番号
015350*     END-IF.
015360*
           IF ( 印刷記号Ｗ(1:1) = NC"＊" )
              MOVE  SPACE          TO  記号Ｗ
           END-IF.
           IF ( 番号Ｗ(1:1) = "*"  ) OR
              ( 番号Ｗ(1:2) = "＊" )
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
015370*
015380     MOVE 印刷保険者番号Ｗ    TO 保険者番号.
015390*     MOVE 請求先名称Ｗ        TO 保険者名称.
015400     MOVE 印刷請求先名称１Ｗ  TO 保険者名称１.
015410     MOVE 印刷請求先名称２Ｗ  TO 保険者名称２.
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
           ELSE
      */長野県内の52母子、53障害、55乳幼児は本体レセの受給者番号②に印字する/110922
               IF (受－助成種別 = 52 OR 53 OR 55) AND
                  (受－費用負担者番号助成(3:2) = "20" )
      */ダミー番号で無い場合は通常印刷/20180906
                   IF 受－費用負担者番号助成(1:2) = "99"
                       IF 印刷受給者番号２Ｗ = SPACE
                           MOVE 印刷受給者番号Ｗ   TO 受給者番号３
                       ELSE
                           MOVE 受給者番号Ｗ(1:8)  TO 受給者番号４
                           MOVE 受給者番号Ｗ(9:7)  TO 受給者番号３
                       END-IF
      */ダミー番号で無い場合は通常印刷↓↓↓/20180906/②を使う/20180910
                   ELSE
                       MOVE 公費負担者番号Ｗ TO 受給者番号４
                       MOVE 印刷受給者番号Ｗ TO 受給者番号３
                   END-IF
      */ダミー番号で無い場合は通常印刷↑↑↑/20180906/②を使う/20180910
               END-IF
      */長野県内の60その他は本体レセの受給者番号①に印字する/111214
               IF (受－助成種別 = 60) AND
                  (受－費用負担者番号助成(3:2) = "20" )
                   MOVE 公費負担者番号Ｗ TO 公費負担者番号
                   MOVE 印刷受給者番号Ｗ TO 受給者番号２
               END-IF
           END-IF
015420*     MOVE 被保険者カナＷ      TO 被保険者カナ.
015430*     MOVE 被保険者性別Ｗ      TO 被保険者性別.
015440     MOVE 被保険者氏名Ｗ      TO 被保険者氏名.
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
007670*     MOVE 被保険者住所１Ｗ    TO 住所１.
007680*     MOVE 被保険者住所２Ｗ    TO 住所２.
015490*     MOVE 被保険者住所２Ｗ    TO 住所２.
           EVALUATE TRUE
           WHEN 連レ－保険種別 >= 50
      */ 郵便番号・電話番号追加 /42505
               IF (施術和暦年月ＷＲ >= 42505) AND (連入－電話印刷 = 1)
                  IF (受－柔整郵便電話番号印刷 = 0 OR 2) AND
                     ((患者郵便番号１Ｗ NOT = SPACE) OR (患者郵便番号２Ｗ NOT = SPACE))
017280               MOVE "〒"              TO 郵便
017260               MOVE 患者郵便番号１Ｗ  TO 郵便番号１
017270               MOVE 患者郵便番号２Ｗ  TO 郵便番号２
017280               MOVE "-"               TO 郵便番号区切
                  END-IF
                  IF 受－柔整郵便電話番号印刷 = 0 OR 3
017260*               MOVE 患者電話番号Ｗ    TO 電話番号
                     IF 電話番号Ｗ NOT = SPACE
017260                  MOVE 電話番号Ｗ     TO 電話番号
                     ELSE
017260                  MOVE 患者電話番号Ｗ TO 電話番号
                     END-IF
                  END-IF
               END-IF
017450         MOVE 患者住所１Ｗ     TO 住所１
017460         MOVE 患者住所２Ｗ     TO 住所２
017470     WHEN OTHER
      */ 郵便番号・電話番号追加 /42505
              IF (施術和暦年月ＷＲ >= 42505) AND (連入－電話印刷 = 1)
                  IF (受－柔整郵便電話番号印刷 = 0 OR 2) AND
                     ((郵便番号１Ｗ NOT = SPACE) OR (郵便番号２Ｗ NOT = SPACE))
017280               MOVE "〒"          TO 郵便
017260               MOVE 郵便番号１Ｗ  TO 郵便番号１
017270               MOVE 郵便番号２Ｗ  TO 郵便番号２
017280               MOVE "-"           TO 郵便番号区切
                  END-IF
                  IF 受－柔整郵便電話番号印刷 = 0 OR 3
                     IF 電話番号Ｗ NOT = SPACE
017260                  MOVE 電話番号Ｗ     TO 電話番号
                     ELSE
017260                  MOVE 患者電話番号Ｗ TO 電話番号
                     END-IF
                  END-IF
              END-IF
017540         MOVE 被保険者住所１Ｗ TO 住所１
017550         MOVE 被保険者住所２Ｗ TO 住所２
017560     END-EVALUATE.
015500     MOVE 患者カナＷ          TO 患者カナ.
015510     MOVE 患者氏名Ｗ          TO 患者氏名.
015520*     MOVE 患者性別Ｗ          TO 患者性別.
015530     MOVE 男チェックＷ        TO 男チェック.
015540     MOVE 女チェックＷ        TO 女チェック.
015550     MOVE 明治チェックＷ      TO 明治チェック.
015560     MOVE 大正チェックＷ      TO 大正チェック.
015570     MOVE 昭和チェックＷ      TO 昭和チェック.
015580     MOVE 平成チェックＷ      TO 平成チェック.
015590*     MOVE 元号Ｗ              TO 元号.
      */元号修正↓↓↓/20190405
           MOVE 令和ＣＭＷ         TO 令和ＣＭ.
023070     MOVE 令和チェックＷ     TO 令和チェック.
017390*     MOVE 元号Ｗ              TO 患者和暦.
      */元号修正↑↑↑/20190405
015600     MOVE 患者年Ｗ            TO 患者年.
015610*     MOVE "."                 TO 区切１.
015620     MOVE 患者月Ｗ            TO 患者月.
015630*     MOVE "."                 TO 区切２.
015640     MOVE 患者日Ｗ            TO 患者日.
015650*     MOVE 印刷続柄Ｗ          TO 続柄.
      *
           IF ( 連レ－保険種別 > 50 ) AND
              (受２－助成被保険者氏名 NOT = SPACE)
016940        MOVE 受２－助成被保険者氏名 TO 被保険者氏名
           END-IF.
015660*
015670     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
015680     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
015690     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
015700     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
015710     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
015710     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
015720*
015730** 特別区分のセット
015740*     MOVE ３歳未満チェックＷ  TO ３歳未満チェック.
015750*     MOVE ７０歳チェックＷ    TO ７０歳チェック.
015760*     MOVE ７０歳１チェックＷ  TO ７０歳１チェック.
015770*     MOVE ７０歳２チェックＷ  TO ７０歳２チェック.
      *     MOVE 義務教育就学前Ｗ    TO 義務教育就学前.
015807*
015808*** 助成名称のセット
015809*     MOVE 助成印Ｗ            TO 助成印.
015810*
015820* 負担率(退職のみ)
015830*     MOVE 負担割合文字Ｗ      TO 負担率.
015840
015850********************
015860* 長期理由文セット *
015870********************
015880*     MOVE 連長文－理由文(1)    TO 長期理由文１.
015890*     MOVE 連長文－理由文(2)    TO 長期理由文２.
015900*     MOVE 連長文－理由文(3)    TO 長期理由文３.
015910*     MOVE 連長文－理由文(4)    TO 長期理由文４.
015920*     MOVE 連長文－理由文(5)    TO 長期理由文５.
015930*     MOVE 連長文－理由文(6)    TO 長期理由文６.
015940*     MOVE 連長文－理由文(7)    TO 長期理由文７.
015950*     IF   連長文－理由文(1)  NOT = SPACE
015960*          MOVE NC"（長期理由）"  TO 長期理由固定
015970*     END-IF.
015980*
015990********************
016000* 負傷データセット *
016010********************
016020* １部位 *
016030**********
016040     MOVE 負傷名Ｗ(1)       TO 負傷名１.
016050     MOVE 負傷年Ｗ(1)       TO 負傷年１.
016060     MOVE 負傷月Ｗ(1)       TO 負傷月１.
016070     MOVE 負傷日Ｗ(1)       TO 負傷日１.
016080     MOVE 初検年Ｗ(1)       TO 初検年１.
016090     MOVE 初検月Ｗ(1)       TO 初検月１.
016100     MOVE 初検日Ｗ(1)       TO 初検日１.
016110     MOVE 開始年Ｗ(1)       TO 開始年１.
016120     MOVE 開始月Ｗ(1)       TO 開始月１.
016130     MOVE 開始日Ｗ(1)       TO 開始日１.
016140     MOVE 終了年Ｗ(1)       TO 終了年１.
016150     MOVE 終了月Ｗ(1)       TO 終了月１.
016160     MOVE 終了日Ｗ(1)       TO 終了日１.
016170     MOVE 実日数Ｗ(1)       TO 実日数１.
016180     MOVE 治癒チェックＷ(1) TO 治癒チェック１.
016190     MOVE 中止チェックＷ(1) TO 中止チェック１.
016200     MOVE 転医チェックＷ(1) TO 転医チェック１.
016210*
016220**********
016230* ２部位 *
016240**********
016250     MOVE 負傷名Ｗ(2)       TO 負傷名２.
016260     MOVE 負傷年Ｗ(2)       TO 負傷年２.
016270     MOVE 負傷月Ｗ(2)       TO 負傷月２.
016280     MOVE 負傷日Ｗ(2)       TO 負傷日２.
016290     MOVE 初検年Ｗ(2)       TO 初検年２.
016300     MOVE 初検月Ｗ(2)       TO 初検月２.
016310     MOVE 初検日Ｗ(2)       TO 初検日２.
016320     MOVE 開始年Ｗ(2)       TO 開始年２.
016330     MOVE 開始月Ｗ(2)       TO 開始月２.
016340     MOVE 開始日Ｗ(2)       TO 開始日２.
016350     MOVE 終了年Ｗ(2)       TO 終了年２.
016360     MOVE 終了月Ｗ(2)       TO 終了月２.
016370     MOVE 終了日Ｗ(2)       TO 終了日２.
016380     MOVE 実日数Ｗ(2)       TO 実日数２.
016390     MOVE 治癒チェックＷ(2) TO 治癒チェック２.
016400     MOVE 中止チェックＷ(2) TO 中止チェック２.
016410     MOVE 転医チェックＷ(2) TO 転医チェック２.
016420**********
016430* ３部位 *
016440**********
016450     MOVE 負傷名Ｗ(3)       TO 負傷名３.
016460     MOVE 負傷年Ｗ(3)       TO 負傷年３.
016470     MOVE 負傷月Ｗ(3)       TO 負傷月３.
016480     MOVE 負傷日Ｗ(3)       TO 負傷日３.
016490     MOVE 初検年Ｗ(3)       TO 初検年３.
016500     MOVE 初検月Ｗ(3)       TO 初検月３.
016510     MOVE 初検日Ｗ(3)       TO 初検日３.
016520     MOVE 開始年Ｗ(3)       TO 開始年３.
016530     MOVE 開始月Ｗ(3)       TO 開始月３.
016540     MOVE 開始日Ｗ(3)       TO 開始日３.
016550     MOVE 終了年Ｗ(3)       TO 終了年３.
016560     MOVE 終了月Ｗ(3)       TO 終了月３.
016570     MOVE 終了日Ｗ(3)       TO 終了日３.
016580     MOVE 実日数Ｗ(3)       TO 実日数３.
016590     MOVE 治癒チェックＷ(3) TO 治癒チェック３.
016600     MOVE 中止チェックＷ(3) TO 中止チェック３.
016610     MOVE 転医チェックＷ(3) TO 転医チェック３.
016620**********
016630* ４部位 *
016640**********
016650     MOVE 負傷名Ｗ(4)       TO 負傷名４.
016660     MOVE 負傷年Ｗ(4)       TO 負傷年４.
016670     MOVE 負傷月Ｗ(4)       TO 負傷月４.
016680     MOVE 負傷日Ｗ(4)       TO 負傷日４.
016690     MOVE 初検年Ｗ(4)       TO 初検年４.
016700     MOVE 初検月Ｗ(4)       TO 初検月４.
016710     MOVE 初検日Ｗ(4)       TO 初検日４.
016720     MOVE 開始年Ｗ(4)       TO 開始年４.
016730     MOVE 開始月Ｗ(4)       TO 開始月４.
016740     MOVE 開始日Ｗ(4)       TO 開始日４.
016750     MOVE 終了年Ｗ(4)       TO 終了年４.
016760     MOVE 終了月Ｗ(4)       TO 終了月４.
016770     MOVE 終了日Ｗ(4)       TO 終了日４.
016780     MOVE 実日数Ｗ(4)       TO 実日数４.
016790     MOVE 治癒チェックＷ(4) TO 治癒チェック４.
016800     MOVE 中止チェックＷ(4) TO 中止チェック４.
016810     MOVE 転医チェックＷ(4) TO 転医チェック４.
016820**********
016830* ５部位 *
016840**********
016850     MOVE 負傷名Ｗ(5)       TO 負傷名５.
016860     MOVE 負傷年Ｗ(5)       TO 負傷年５.
016870     MOVE 負傷月Ｗ(5)       TO 負傷月５.
016880     MOVE 負傷日Ｗ(5)       TO 負傷日５.
016890     MOVE 初検年Ｗ(5)       TO 初検年５.
016900     MOVE 初検月Ｗ(5)       TO 初検月５.
016910     MOVE 初検日Ｗ(5)       TO 初検日５.
016920     MOVE 開始年Ｗ(5)       TO 開始年５.
016930     MOVE 開始月Ｗ(5)       TO 開始月５.
016940     MOVE 開始日Ｗ(5)       TO 開始日５.
016950     MOVE 終了年Ｗ(5)       TO 終了年５.
016960     MOVE 終了月Ｗ(5)       TO 終了月５.
016970     MOVE 終了日Ｗ(5)       TO 終了日５.
016980     MOVE 実日数Ｗ(5)       TO 実日数５.
016990     MOVE 治癒チェックＷ(5) TO 治癒チェック５.
017000     MOVE 中止チェックＷ(5) TO 中止チェック５.
017010     MOVE 転医チェックＷ(5) TO 転医チェック５.
017020**************
017030* 経過セット *
017040**************
017050     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
017060***             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
017070             UNTIL ( 部位ＣＮＴ > 5 )
017080**         MOVE 部位ＣＮＴＷ(部位ＣＮＴ)   TO 経過部位ＣＮＴ(部位ＣＮＴ)
017090**         MOVE 部位区切Ｗ(部位ＣＮＴ)     TO 部位区切(部位ＣＮＴ)
017100         MOVE 印刷経過略称Ｗ(部位ＣＮＴ) TO 経過略称(部位ＣＮＴ)
017110     END-PERFORM.
017120*****************************************
017130*     新規・継続チェックについて        *
017140*   ●新規...初検有り ●継続...初検なし *
017150*****************************************
017160     MOVE 新規チェックＷ    TO 新規チェック.
017170     MOVE 継続チェックＷ    TO 継続チェック.
017180*     MOVE 請求区分Ｗ         TO  請求区分.
017190********************
017200* 料金データセット *
017210********************
017220*    ****************************************************************
017230*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
017240*    ****************************************************************
017250     MOVE 初検料ＷＲ                   TO  初検料.
017260     MOVE 時間外チェックＷ             TO  時間外チェック.
017270     MOVE 休日チェックＷ               TO  休日チェック.
017280     MOVE 深夜チェックＷ               TO  深夜チェック.
017290*     MOVE 初検加算内容Ｗ               TO  初検加算内容.
017300     MOVE 初検加算料ＷＲ               TO  初検加算料.
      *
           IF ((時間外チェックＷ NOT = SPACE) OR (深夜チェックＷ NOT = SPACE) OR
              (休日チェックＷ  NOT = SPACE)) AND
              ((初検加算時Ｗ NOT = ZERO) OR (初検加算分Ｗ NOT = ZERO))
              MOVE 初検加算時Ｗ                 TO  初検加算時
              MOVE 初検加算区切Ｗ               TO  初検加算区切
              MOVE 初検加算分Ｗ                 TO  初検加算分
           END-IF.
      *
           MOVE 初検時相談料ＷＲ             TO  初検時相談料.
017310     MOVE 再検料ＷＲ                   TO  再検料.
017320     MOVE 往療距離ＷＲ                 TO  往療距離.
017330     MOVE 往療回数ＷＲ                 TO  往療回数.
017340     MOVE 往療料ＷＲ                   TO  往療料.
017350     MOVE 夜間チェックＷ               TO  夜間チェック.
017360     MOVE 難路チェックＷ               TO  難路チェック.
017370     MOVE 暴風雨雪チェックＷ           TO  暴風雨雪チェック.
017380     MOVE 往療加算料ＷＲ               TO  往療加算料.
017412*
      */金属副子・運動後療の変更・追加/1805
           IF ( 施術和暦年月ＷＲ < 43006 )
018050        MOVE 大チェックＷ              TO  大チェック
018060        MOVE 中チェックＷ              TO  中チェック
018070        MOVE 小チェックＷ              TO  小チェック
           END-IF.
           IF ( 施術和暦年月ＷＲ >= 43006 ) AND ( 金属副子加算料ＷＲ NOT = ZERO )
              MOVE ALL NC"＝"                TO  金属訂正
      *        MOVE 金属回数Ｗ                TO  金属回数
      *        MOVE NC"回"                    TO  金属回
           END-IF.
017431*-----------------------------------------------------*
017432*
017437*     MOVE 金属大Ｗ                     TO  金属大.
017438*     MOVE 金属中Ｗ                     TO  金属中.
017440*     MOVE 金属小Ｗ                     TO  金属小.
017450     MOVE 金属副子加算料ＷＲ           TO  金属副子加算料.
017460     MOVE 施術情報提供料ＷＲ           TO  施術情報提供料.
017470     MOVE 小計Ｗ                       TO  小計.
017480********************
017490* 初回処置料セット *
017500********************
017510     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
017520***             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
017530             UNTIL ( 部位ＣＮＴ > 5 )
017540         MOVE 初回処置料ＷＲ(部位ＣＮＴ) TO 初回処置料(部位ＣＮＴ)
017550     END-PERFORM.
017560     MOVE 初回処置料合計Ｗ         TO 初回処置料合計.
017570********************
017580* 逓減毎料金セット *
017590********************
017600*    **********
017610*    * １部位 *
017620*    **********
017630     MOVE 後療単価１ＷＲ             TO 後療単価１.
017640     MOVE 後療回数１ＷＲ             TO 後療回数１.
017650     MOVE 後療料１ＷＲ               TO 後療料１.
017660     MOVE 冷罨法回数１ＷＲ           TO 冷罨法回数１.
017670     MOVE 冷罨法料１ＷＲ             TO 冷罨法料１.
017680     MOVE 温罨法回数１ＷＲ           TO 温罨法回数１.
017690     MOVE 温罨法料１ＷＲ             TO 温罨法料１.
017700     MOVE 電療回数１ＷＲ             TO 電療回数１.
017710     MOVE 電療料１ＷＲ               TO 電療料１.
017720     MOVE 小計１ＷＲ                 TO 小計１.
017730     IF 長期逓減率１ＷＲ NOT = ZERO
017740         COMPUTE 長期逓減率１ = 長期逓減率１ＷＲ / 100
017750     END-IF.
017760     MOVE 長期込小計１ＷＲ           TO 長期込小計１.
017770*    **********
017780*    * ２部位 *
017790*    **********
017800     MOVE 後療単価２ＷＲ             TO 後療単価２.
017810     MOVE 後療回数２ＷＲ             TO 後療回数２.
017820     MOVE 後療料２ＷＲ               TO 後療料２.
017830     MOVE 冷罨法回数２ＷＲ           TO 冷罨法回数２.
017840     MOVE 冷罨法料２ＷＲ             TO 冷罨法料２.
017850     MOVE 温罨法回数２ＷＲ           TO 温罨法回数２.
017860     MOVE 温罨法料２ＷＲ             TO 温罨法料２.
017870     MOVE 電療回数２ＷＲ             TO 電療回数２.
017880     MOVE 電療料２ＷＲ               TO 電療料２.
017890     MOVE 小計２ＷＲ                 TO 小計２.
017900     IF 長期逓減率２ＷＲ NOT = ZERO
017910         COMPUTE 長期逓減率２ = 長期逓減率２ＷＲ / 100
017920     END-IF.
017930     MOVE 長期込小計２ＷＲ           TO 長期込小計２.
017940*    ****************
017950*    * ３部位／８割 *
017960*    ****************
017970     MOVE 後療単価３８ＷＲ             TO 後療単価３８.
017980     MOVE 後療回数３８ＷＲ             TO 後療回数３８.
017990     MOVE 後療料３８ＷＲ               TO 後療料３８.
018000     MOVE 冷罨法回数３８ＷＲ           TO 冷罨法回数３８.
018010     MOVE 冷罨法料３８ＷＲ             TO 冷罨法料３８.
018020     MOVE 温罨法回数３８ＷＲ           TO 温罨法回数３８.
018030     MOVE 温罨法料３８ＷＲ             TO 温罨法料３８.
018040     MOVE 電療回数３８ＷＲ             TO 電療回数３８.
018050     MOVE 電療料３８ＷＲ               TO 電療料３８.
018060     MOVE 小計３８ＷＲ                 TO 小計３８.
018070     MOVE 多部位込小計３８ＷＲ         TO 多部位込小計３８.
018080     IF 長期逓減率３８ＷＲ NOT = ZERO
018090         COMPUTE 長期逓減率３８ = 長期逓減率３８ＷＲ / 100
018100     END-IF.
018110     MOVE 長期込小計３８ＷＲ           TO 長期込小計３８.
      */ 逓減率 0.7→0.6 /42505
           IF (施術和暦年月ＷＲ >= 42505)
      */2506用紙より逓減の訂正をしない/130614
               IF 一般レセＷ < 5
                   MOVE "60"                 TO 逓減３８
                   MOVE "0.6"                TO 多部位３８
                   MOVE "==="                TO 逓減訂正３８ 多部位訂正３８
               END-IF
           END-IF.
018120*    ****************
018130*    * ３部位／10割 *
018140*    ****************
018150     MOVE 逓減開始月３０ＷＲ           TO 逓減開始月３０.
018160     MOVE 逓減開始日３０ＷＲ           TO 逓減開始日３０.
018170     MOVE 後療単価３０ＷＲ             TO 後療単価３０.
018180     MOVE 後療回数３０ＷＲ             TO 後療回数３０.
018190     MOVE 後療料３０ＷＲ               TO 後療料３０.
018200     MOVE 冷罨法回数３０ＷＲ           TO 冷罨法回数３０.
018210     MOVE 冷罨法料３０ＷＲ             TO 冷罨法料３０.
018220     MOVE 温罨法回数３０ＷＲ           TO 温罨法回数３０.
018230     MOVE 温罨法料３０ＷＲ             TO 温罨法料３０.
018240     MOVE 電療回数３０ＷＲ             TO 電療回数３０.
018250     MOVE 電療料３０ＷＲ               TO 電療料３０.
018260     MOVE 小計３０ＷＲ                 TO 小計３０.
018270     IF 長期逓減率３０ＷＲ NOT = ZERO
018280         COMPUTE 長期逓減率３０ = 長期逓減率３０ＷＲ / 100
018290     END-IF.
018300     MOVE 長期込小計３０ＷＲ           TO 長期込小計３０.
018310*    ****************
018320*    * ４部位／５割 *
018330*    ****************
018340*     MOVE 後療単価４５ＷＲ             TO 後療単価４５.
018350*     MOVE 後療回数４５ＷＲ             TO 後療回数４５.
018360*     MOVE 後療料４５ＷＲ               TO 後療料４５.
018370*     MOVE 冷罨法回数４５ＷＲ           TO 冷罨法回数４５.
018380*     MOVE 冷罨法料４５ＷＲ             TO 冷罨法料４５.
018390*     MOVE 温罨法回数４５ＷＲ           TO 温罨法回数４５.
018400*     MOVE 温罨法料４５ＷＲ             TO 温罨法料４５.
018410*     MOVE 電療回数４５ＷＲ             TO 電療回数４５.
018420*     MOVE 電療料４５ＷＲ               TO 電療料４５.
018430*     MOVE 小計４５ＷＲ                 TO 小計４５.
018440*     MOVE 多部位込小計４５ＷＲ         TO 多部位込小計４５.
018450*     IF 長期逓減率４５ＷＲ NOT = ZERO
018460*         COMPUTE 長期逓減率４５ = 長期逓減率４５ＷＲ / 100
018470*     END-IF.
018480*     MOVE 長期込小計４５ＷＲ           TO 長期込小計４５.
018490*    ****************
018500*    * ４部位／８割 *
018510*    ****************
018520     MOVE 逓減開始月４８ＷＲ           TO 逓減開始月４８.
018530     MOVE 逓減開始日４８ＷＲ           TO 逓減開始日４８.
018540     MOVE 後療単価４８ＷＲ             TO 後療単価４８.
018550     MOVE 後療回数４８ＷＲ             TO 後療回数４８.
018560     MOVE 後療料４８ＷＲ               TO 後療料４８.
018570     MOVE 冷罨法回数４８ＷＲ           TO 冷罨法回数４８.
018580     MOVE 冷罨法料４８ＷＲ             TO 冷罨法料４８.
018590     MOVE 温罨法回数４８ＷＲ           TO 温罨法回数４８.
018600     MOVE 温罨法料４８ＷＲ             TO 温罨法料４８.
018610     MOVE 電療回数４８ＷＲ             TO 電療回数４８.
018620     MOVE 電療料４８ＷＲ               TO 電療料４８.
018630     MOVE 小計４８ＷＲ                 TO 小計４８.
018640     MOVE 多部位込小計４８ＷＲ         TO 多部位込小計４８.
018650     IF 長期逓減率４８ＷＲ NOT = ZERO
018660         COMPUTE 長期逓減率４８ = 長期逓減率４８ＷＲ / 100
018670     END-IF.
018680     MOVE 長期込小計４８ＷＲ           TO 長期込小計４８.
      */ 逓減率 0.7→0.6 /42505
           IF (施術和暦年月ＷＲ >= 42505)
      */2506用紙より逓減の訂正をしない/130614
               IF 一般レセＷ < 5
                   MOVE "60"                 TO 逓減４８
                   MOVE "0.6"                TO 多部位４８
                   MOVE "==="                TO 逓減訂正４８ 多部位訂正４８
               END-IF
           END-IF.
018690*    ****************
018700*    * ４部位／10割 *
018710*    ****************
018720     MOVE 逓減開始月４０ＷＲ           TO 逓減開始月４０.
018730     MOVE 逓減開始日４０ＷＲ           TO 逓減開始日４０.
018740     MOVE 後療単価４０ＷＲ             TO 後療単価４０.
018750     MOVE 後療回数４０ＷＲ             TO 後療回数４０.
018760     MOVE 後療料４０ＷＲ               TO 後療料４０.
018770     MOVE 冷罨法回数４０ＷＲ           TO 冷罨法回数４０.
018780     MOVE 冷罨法料４０ＷＲ             TO 冷罨法料４０.
018790     MOVE 温罨法回数４０ＷＲ           TO 温罨法回数４０.
018800     MOVE 温罨法料４０ＷＲ             TO 温罨法料４０.
018810     MOVE 電療回数４０ＷＲ             TO 電療回数４０.
018820     MOVE 電療料４０ＷＲ               TO 電療料４０.
018830     MOVE 小計４０ＷＲ                 TO 小計４０.
018840     IF 長期逓減率４０ＷＲ NOT = ZERO
018850         COMPUTE 長期逓減率４０ = 長期逓減率４０ＷＲ / 100
018860     END-IF.
018870     MOVE 長期込小計４０ＷＲ           TO 長期込小計４０.
018880*
018890*↓***********************************************************************
018900* 帳票定義の変更により、５部位の印字は必要ない。
018910*------------------------------------------------------------------------*
018920*    *****************
018930*    * ５部位／2.5割 *
018940*    *****************
018950*     MOVE 後療単価５２ＷＲ             TO 後療単価５２.
018960*     MOVE 後療回数５２ＷＲ             TO 後療回数５２.
018970*     MOVE 後療料５２ＷＲ               TO 後療料５２.
018980*     MOVE 冷罨法回数５２ＷＲ           TO 冷罨法回数５２.
018990*     MOVE 冷罨法料５２ＷＲ             TO 冷罨法料５２.
019000*     MOVE 温罨法回数５２ＷＲ           TO 温罨法回数５２.
019010*     MOVE 温罨法料５２ＷＲ             TO 温罨法料５２.
019020*     MOVE 電療回数５２ＷＲ             TO 電療回数５２.
019030*     MOVE 電療料５２ＷＲ               TO 電療料５２.
019040*     MOVE 小計５２ＷＲ                 TO 小計５２.
019050*     MOVE 多部位込小計５２ＷＲ         TO 多部位込小計５２.
019060*     IF 長期逓減率５２ＷＲ NOT = ZERO
019070*         COMPUTE 長期逓減率５２ = 長期逓減率５２ＷＲ / 100
019080*     END-IF.
019090*     MOVE 長期込小計５２ＷＲ           TO 長期込小計５２.
019100*    ****************
019110*    * ５部位／５割 *
019120*    ****************
019130*     MOVE 逓減開始月５５ＷＲ           TO 逓減開始月５５.
019140*     MOVE 逓減開始日５５ＷＲ           TO 逓減開始日５５.
019150*     MOVE 後療単価５５ＷＲ             TO 後療単価５５.
019160*     MOVE 後療回数５５ＷＲ             TO 後療回数５５.
019170*     MOVE 後療料５５ＷＲ               TO 後療料５５.
019180*     MOVE 冷罨法回数５５ＷＲ           TO 冷罨法回数５５.
019190*     MOVE 冷罨法料５５ＷＲ             TO 冷罨法料５５.
019200*     MOVE 温罨法回数５５ＷＲ           TO 温罨法回数５５.
019210*     MOVE 温罨法料５５ＷＲ             TO 温罨法料５５.
019220*     MOVE 電療回数５５ＷＲ             TO 電療回数５５.
019230*     MOVE 電療料５５ＷＲ               TO 電療料５５.
019240*     MOVE 小計５５ＷＲ                 TO 小計５５.
019250*     MOVE 多部位込小計５５ＷＲ         TO 多部位込小計５５.
019260*     IF 長期逓減率５５ＷＲ NOT = ZERO
019270*         COMPUTE 長期逓減率５５ = 長期逓減率５５ＷＲ / 100
019280*     END-IF.
019290*     MOVE 長期込小計５５ＷＲ           TO 長期込小計５５.
019300*    ****************
019310*    * ５部位／８割 *
019320*    ****************
019330*     MOVE 逓減開始月５８ＷＲ           TO 逓減開始月５８.
019340*     MOVE 逓減開始日５８ＷＲ           TO 逓減開始日５８.
019350*     MOVE 後療単価５８ＷＲ             TO 後療単価５８.
019360*     MOVE 後療回数５８ＷＲ             TO 後療回数５８.
019370*     MOVE 後療料５８ＷＲ               TO 後療料５８.
019380*     MOVE 冷罨法回数５８ＷＲ           TO 冷罨法回数５８.
019390*     MOVE 冷罨法料５８ＷＲ             TO 冷罨法料５８.
019400*     MOVE 温罨法回数５８ＷＲ           TO 温罨法回数５８.
019410*     MOVE 温罨法料５８ＷＲ             TO 温罨法料５８.
019420*     MOVE 電療回数５８ＷＲ             TO 電療回数５８.
019430*     MOVE 電療料５８ＷＲ               TO 電療料５８.
019440*     MOVE 小計５８ＷＲ                 TO 小計５８.
019450*     MOVE 多部位込小計５８ＷＲ         TO 多部位込小計５８.
019460*     IF 長期逓減率５８ＷＲ NOT = ZERO
019470*         COMPUTE 長期逓減率５８ = 長期逓減率５８ＷＲ / 100
019480*     END-IF.
019490*     MOVE 長期込小計５８ＷＲ           TO 長期込小計５８.
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
019500*    ****************
019510*    * ５部位／10割 *
019520*    ****************
019530*     MOVE 逓減開始月５０ＷＲ           TO 逓減開始月５０.
019540*     MOVE 逓減開始日５０ＷＲ           TO 逓減開始日５０.
019550*     MOVE 後療単価５０ＷＲ             TO 後療単価５０.
019560*     MOVE 後療回数５０ＷＲ             TO 後療回数５０.
019570*     MOVE 後療料５０ＷＲ               TO 後療料５０.
019580*     MOVE 冷罨法回数５０ＷＲ           TO 冷罨法回数５０.
019590*     MOVE 冷罨法料５０ＷＲ             TO 冷罨法料５０.
019600*     MOVE 温罨法回数５０ＷＲ           TO 温罨法回数５０.
019610*     MOVE 温罨法料５０ＷＲ             TO 温罨法料５０.
019620*     MOVE 電療回数５０ＷＲ             TO 電療回数５０.
019630*     MOVE 電療料５０ＷＲ               TO 電療料５０.
019640*     MOVE 小計５０ＷＲ                 TO 小計５０.
019650*     MOVE 長期逓減率５０ＷＲ           TO 長期逓減率５０.
019660*     IF 長期逓減率５０ＷＲ NOT = ZERO
019670*         COMPUTE 長期逓減率５０ = 長期逓減率５０ＷＲ / 100
019680*     END-IF.
019690*     MOVE 長期込小計５０ＷＲ           TO 長期込小計５０.
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
019700*↑***********************************************************************
019710*------------------------------------------------------------------------------------*
019720** 特別（５部位目の明細がある場合は、適用欄にコメントを印字する）
019730*     IF ( 小計５５ＷＲ NOT = ZERO ) OR
019740*        ( 小計５８ＷＲ NOT = ZERO ) OR
019750*        ( 小計５０ＷＲ NOT = ZERO )
019760*        MOVE  NC"※５部位目請求あり"   TO 部位５適用
019770*     END-IF.
019780*------------------------------------------------------------------------------------*
019790*
019800     MOVE 適用１Ｗ                     TO 適用１.
019810     MOVE 適用２Ｗ                     TO 適用２.
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
              MOVE 8            TO 連金運－会コード
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
019820     MOVE レセ－合計                   TO 合計.
019830     MOVE レセ－一部負担金             TO 一部負担金.
019840     MOVE レセ－請求金額               TO 請求金額.
      */横浜川崎のレセまとめは本体金額を印刷する/110418/121025
019853     IF 助成レセまとめフラグ = "YES"
      */平成25年4月より神奈川県内の全ての市町村でまとめる/130406
004050*         IF 受－費用負担者番号助成(3:3) = "144" OR "145"
004050         IF (受－費用負担者番号助成(3:3) = "144" OR "145") AND (受－施術和暦年月 <  42504) OR
                  (受－費用負担者番号助成(3:2) = "14")           AND (受－施術和暦年月 >= 42504)
019830             MOVE レセ－印刷レセ負担額 TO 一部負担金
019840             MOVE レセ－印刷レセ請求額 TO 請求金額
      */湯河原町の母子は負担額助成を記載する/130418
      */座間市の障害は負担額助成を記載する/
      */横浜川崎の乳児は負担額助成を記載する/170217
                   IF ((受－助成種別 = 52) AND (受－費用負担者番号助成 = "85140630")) OR
                      ((受－助成種別 = 53) AND (受－費用負担者番号助成 = "80140171")) OR
                      ((受－助成種別 = 55) AND
                       (((受－費用負担者番号助成 >= "81144511") AND (受－費用負担者番号助成 <= "81144685")) OR
                        (受－費用負担者番号助成 = "81145500")))
019820                 MOVE レセ－合計       TO 合計
019830                 MOVE レセ－一部負担金 TO 一部負担金
019840                 MOVE レセ－請求金額   TO 請求金額
                   END-IF
               END-IF
           END-IF.
      *
      */県固有処理
      */愛知県の助成は被保険者の欄に本人の名前を印刷
           IF 連レ－保険種別 > 50
               IF 公費負担者番号Ｗ(3:2) = "23"
                   MOVE 患者氏名Ｗ           TO 被保険者氏名
               END-IF
           END-IF.
      *
      */助成レセ時の金額欄
           IF 連レ－保険種別 > 50
               EVALUATE TRUE
      *         EVALUATE 公費負担者番号Ｗ(3:2)
               WHEN 公費負担者番号Ｗ(3:2) = 14 AND 助成種別ＷＲ NOT = 54
               WHEN 公費負担者番号Ｗ(3:2) = 23
               WHEN 公費負担者番号Ｗ(3:2) = 26
      */神奈川(被爆以外)、愛知、京都 (本体費用額、本体負担額、助成請求額)
                   MOVE 助成印Ｗ           TO 助成印
                   MOVE "( )"              TO 助成印ＣＭ
021370             MOVE レセ－受給者負担額 TO 一部負担金
021380             MOVE レセ－助成請求金額 TO 請求金額
      */千葉県の子ども医療費助成/120314
      */千葉県の重度心身障害医療費助成/150703
      */千葉県のひとり親医療費助成/201001
               WHEN ((助成種別ＷＲ = 60) AND (公費負担者番号Ｗ(1:4) =  "8312")) OR
                    ((助成種別ＷＲ = 53) AND (公費負担者番号Ｗ(1:4) =  "8112")) OR
                    ((助成種別ＷＲ = 52) AND (公費負担者番号Ｗ(1:4) =  "8512"))
                   MOVE "X" TO EDIT-MODE OF           一部負担金
                   MOVE レセ－一部負担金   TO 一部負担金２
                   MOVE レセ－受給者負担額 TO 受給者負担額２
                   MOVE レセ－助成請求金額 TO 請求金額
               WHEN OTHER
      */通常金額欄
                   MOVE 助成印Ｗ             TO 助成印
                   MOVE "( )"                TO 助成印ＣＭ
                   MOVE "一部負担金"         TO 一部負担金ＣＭ
                   MOVE "円"                 TO 円ＣＭ
                   MOVE レセ－受給者負担額   TO 受給者負担額
                   MOVE レセ－助成請求金額   TO 助成請求金額
               END-EVALUATE
           ELSE
      */千葉県の重度心身障害医療費助成の本体に助成の金額/150914
               IF ((助成種別ＷＲ = 53) AND (公費負担者番号Ｗ(1:4) =  "8112"))
                   MOVE "自己負担金額："     TO 自己負担金ＣＭ
                   MOVE "円"                 TO 円ＣＭ１
                   MOVE レセ－受給者負担額   TO 自己負担金
                   MOVE "公費負担金額："     TO 一部負担金ＣＭ
                   MOVE "円"                 TO 円ＣＭ
                   MOVE レセ－助成請求金額   TO 受給者負担額
               END-IF
           END-IF.
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
                   MOVE 助成印Ｗ         TO 助成印２
                   MOVE NC"○"           TO 助成印丸
               END-IF
           ELSE
      */大阪、広島以外 本体レセに助成マーク
               IF (受－助成種別 NOT = ZERO) AND
                  (連レ－保険種別   < 50  )
                   MOVE 助成印Ｗ         TO 助成印２
                   MOVE NC"○"           TO 助成印丸
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
      */福島 子ども医療の時備考欄に"子適用"と印字/1703
           IF (受－助成種別 = 55 OR 60) AND (公費負担者番号Ｗ(3:2) = "07") AND
              (助成レセまとめフラグ = "YES")
               MOVE "子適用"             TO 助成印ＣＭ２
           END-IF.
019850*
019851*------------------------------------------------------------------------------------*
019852* 特別（助成レセなしで、本体レセにまとめる時、金額は助成込み・適用２に助成種別印字）
019853*     IF 助成レセまとめフラグ = "YES"
      */横浜川崎のレセまとめは本体金額を印刷する/110418
004050*         IF 受－費用負担者番号助成(3:3) = "144" OR "145"
      *            CONTINUE
      *         ELSE
019854*             PERFORM 助成料金計算
019855*             MOVE レセ－合計                 TO 合計
019856*             MOVE レセ－受給者負担額 TO 一部負担金
019857*         / 引き算する/
019858*             COMPUTE 請求金額 = レセ－合計 - レセ－受給者負担額
019879*         END-IF
019879*     END-IF.
019880*------------------------------------------------------------------------------------*
019881**********************
019882* 施術所データセット *
019883**********************
019890     MOVE 柔整師番号Ｗ           TO 柔整師番号.
019900* 共済・自衛官用
019910     MOVE 柔整師番号２Ｗ         TO 柔整師番号２.
019920*     MOVE 定額制受理番号Ｗ       TO 定額制受理番号.
019930     MOVE 施術所郵便番号１Ｗ     TO 施術所郵便番号１.
019940     MOVE 施術所郵便番号２Ｗ     TO 施術所郵便番号２.
019950*
019960*     MOVE 施術所住所Ｗ           TO 施術所住所１.
019970     MOVE 施術所住所１Ｗ         TO 施術所住所１.
019980     MOVE 施術所住所２Ｗ         TO 施術所住所２.
           MOVE 都道府県ＪＩＳＷ       TO 都道府県番号.
019990*
020000     MOVE 接骨師会会員番号Ｗ     TO 接骨師会会員番号.
020010     MOVE 代表者カナＷ           TO 代表者カナ.
020020     MOVE 代表者名Ｗ             TO 代表者名.
020030     MOVE 施術所電話番号Ｗ       TO 施術所電話番号.
020040*
020050     MOVE 接骨院名Ｗ             TO 接骨院名.
020060*
020070*     MOVE 銀行名支店名Ｗ         TO 銀行名支店名.
020080*     MOVE 預金種別コメントＷ     TO 預金種別.
020090*     MOVE 口座番号Ｗ             TO 口座番号.
020100*     MOVE 口座名義人カナＷ       TO 口座名義人カナ.
020110*     MOVE 口座名義人Ｗ           TO 口座名義人.
020120*
020130****
020140*
020150* / 柔整師・患者委任日 /
      */元号修正/↓↓↓20190405
037370     IF 施術和暦Ｗ > 4
               MOVE 施術和暦Ｗ         TO 元－元号区分
037380         READ 元号マスタ
037390         NOT INVALID KEY
037400             MOVE 元－元号名称   TO 受理和暦
037410         END-READ
               MOVE "===="             TO 受理和暦訂正
           END-IF.
      */元号修正/↑↑↑20190405
020160     MOVE 柔整師年Ｗ             TO 受理年.
020170     MOVE 柔整師月Ｗ             TO 受理月.
020180     MOVE 柔整師日Ｗ             TO 受理日.
020190* ( 委任年月日 印刷するか )
020200     IF 連入－委任印刷  = ZERO
      */元号修正/↓↓↓20190405
037370         IF 施術和暦Ｗ > 4
                   MOVE 施術和暦Ｗ         TO 元－元号区分
037380             READ 元号マスタ
037390             NOT INVALID KEY
037400                 MOVE 元－元号名称   TO 委任和暦
037410             END-READ
                   MOVE "===="             TO 委任和暦訂正
               END-IF
      */元号修正/↑↑↑20190405
020210         MOVE 患者委任年Ｗ       TO 委任年
020220         MOVE 患者委任月Ｗ       TO 委任月
020230         MOVE 患者委任日Ｗ       TO 委任日
020240     END-IF.
020250*
      */平成23年1月より、摘要欄に施術日を記載する/101129
           IF 施術和暦年月ＷＲ >= 42301
               PERFORM 摘要施術日セット
           END-IF.
022610* 施術ID
022620     MOVE 県施術ＩＤＷ           TO 県施術ＩＤ.
      */平成30年7月～会長名変更/20180713/７月施術以降に変更/0831
           IF 施術和暦年月ＷＲ >= 43007
               MOVE "早津泰治"         TO 会長名
               MOVE ALL "="            TO 会長名取消線 取消線１ 取消線２
           END-IF.
020260*-------------------------------------------------------------------------*
020270*--- ※ レセ摘要再セットは、この印刷セットSECTION の最後にやること！ -----*
020280     PERFORM レセ摘要再セット.
020290*-------------------------------------------------------------------------*
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
      */長野県の国保助成で助成種別が"60"の時、受給者負担額を出す/2106
      */長野県の助成種別が"60"の時、受給者負担額を出す/2201
      *     IF (保険種別ＷＲ = 01 OR 08) AND (受－費用負担者番号助成(3:2) = "20") AND
           IF (受－費用負担者番号助成(3:2) = "20") AND
              (助成種別ＷＲ = 60)
021370         MOVE レセ－印刷レセ受給者負担額       TO 受給者負担額
               MOVE "受給者負担金"                   TO 一部負担金ＣＭ
               MOVE "円" TO 円ＣＭ
               MOVE 公費負担者番号Ｗ TO 公費負担者番号
               IF ( 印刷受給者番号Ｗ(1:1) = "*"  ) OR
                  ( 印刷受給者番号Ｗ(1:2) = "＊" )
                   MOVE  SPACE               TO 受給者番号
               ELSE
                   IF 印刷受給者番号２Ｗ = SPACE
                       MOVE 印刷受給者番号Ｗ TO 受給者番号
                   ELSE
                       MOVE 受給者番号Ｗ     TO 受給者番号２
                   END-IF
                   MOVE SPACE                TO 受給者番号４
                   MOVE SPACE                TO 受給者番号３
               END-IF
           END-IF.
      */長野県の社保助成で助成種別が"60"で県固有から印刷された場合、受給者負担額を出す/2106
           IF (保険種別ＷＲ NOT = 01 AND 08) AND (受－費用負担者番号助成(3:2) =  "20") AND
              (連レ－長野助成 = 60)
021370         MOVE レセ－印刷レセ受給者負担額       TO 受給者負担額
               MOVE "受給者負担金"                   TO 一部負担金ＣＭ
               MOVE "円" TO 円ＣＭ
022720*         MOVE NC"写"                           TO 写記号
022730*         MOVE NC"○"                           TO 写マル
               MOVE 公費負担者番号Ｗ TO 公費負担者番号
               IF ( 印刷受給者番号Ｗ(1:1) = "*"  ) OR
                  ( 印刷受給者番号Ｗ(1:2) = "＊" )
                   MOVE  SPACE               TO 受給者番号
               ELSE
                   IF 印刷受給者番号２Ｗ = SPACE
                       MOVE 印刷受給者番号Ｗ TO 受給者番号
                   ELSE
                       MOVE 受給者番号Ｗ     TO 受給者番号２
                   END-IF
                   MOVE SPACE                TO 受給者番号４
                   MOVE SPACE                TO 受給者番号３
               END-IF
           END-IF.
044950*
020300*
020310*
020320*     PERFORM テスト印字処理.
020330*
020340*================================================================*
020350 項目初期化 SECTION.
020360*
020370     INITIALIZE 施術所情報Ｗ.
020380     INITIALIZE 受診者情報Ｗ.
020390     INITIALIZE 負傷情報Ｗ.
020400     INITIALIZE 備考情報Ｗ.
020410     INITIALIZE 料金１ＷＲ.
020420     INITIALIZE 料金２ＷＲ.
020430     INITIALIZE 料金３ＷＲ.
020440     MOVE SPACE TO YJB6125P.
020450*     INITIALIZE YJB6125P.
020460*================================================================*
020470 料金情報取得 SECTION.
020480*
           IF 連レ－保険種別 > 50
              MOVE 3          TO レセ－レセ種別
           ELSE
              IF 公費種別ＷＲ = 05
                 MOVE 2          TO レセ－レセ種別
              ELSE
                 MOVE 1          TO レセ－レセ種別
              END-IF
           END-IF.
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
020490********************
020500* 料金データセット *
020510********************
020520*    ****************************************************************
020530*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
020540*    ****************************************************************
020550     MOVE レセ－初検料                 TO 初検料ＷＲ.
020560     IF レセ－時間外 = 1
020570         MOVE NC"○"                   TO 時間外チェックＷ
020580     END-IF.
020590     IF レセ－休日 = 1
020600         MOVE NC"○"                   TO 休日チェックＷ
020610     END-IF.
020620     IF レセ－深夜 = 1
020630         MOVE NC"○"                   TO 深夜チェックＷ
020640     END-IF.
020650*
020660*
020670     MOVE レセ－初検加算料             TO  初検加算料ＷＲ.
           MOVE レセ－初検時相談料           TO  初検時相談料ＷＲ.
020680     MOVE レセ－再検料                 TO  再検料ＷＲ.
020690     MOVE レセ－往療距離               TO  往療距離ＷＲ.
020700     MOVE レセ－往療回数               TO  往療回数ＷＲ.
020710     MOVE レセ－往療料                 TO  往療料ＷＲ.
020720     MOVE レセ－往療加算料             TO  往療加算料ＷＲ.
020730*
020740     IF レセ－夜間 = 1
020750         MOVE NC"○"                   TO 夜間チェックＷ
020760     END-IF.
020770     IF レセ－難路 = 1
020780         MOVE NC"○"                   TO 難路チェックＷ
020790     END-IF.
020800     IF レセ－暴風雨雪 = 1
020810         MOVE NC"○"                   TO 暴風雨雪チェックＷ
020820     END-IF.
020830*
020840     MOVE レセ－金属副子加算料         TO  金属副子加算料ＷＲ.
020850*
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
021050*
021060     MOVE レセ－施術情報提供料         TO 施術情報提供料ＷＲ.
021070* 小計
021080     MOVE レセ－小計                   TO 小計Ｗ.
021090********************
021100* 初回処置料セット *
021110********************
021120     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
021130             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
021140         MOVE レセ－初回処置料(部位ＣＮＴ) TO 初回処置料ＷＲ(部位ＣＮＴ)
021150     END-PERFORM.
021160     MOVE レセ－初回処置料合計           TO 初回処置料合計Ｗ.
021170********************
021180* 逓減毎料金セット *
021190********************
021200*    **********
021210*    * １部位 *
021220*    **********
021230     MOVE レセ－後療単価１             TO 後療単価１ＷＲ.
021240     MOVE レセ－後療回数１             TO 後療回数１ＷＲ.
021250     MOVE レセ－後療料１               TO 後療料１ＷＲ.
021260     MOVE レセ－冷罨法回数１           TO 冷罨法回数１ＷＲ.
021270     MOVE レセ－冷罨法料１             TO 冷罨法料１ＷＲ.
021280     MOVE レセ－温罨法回数１           TO 温罨法回数１ＷＲ.
021290     MOVE レセ－温罨法料１             TO 温罨法料１ＷＲ.
021300     MOVE レセ－電療回数１             TO 電療回数１ＷＲ.
021310     MOVE レセ－電療料１               TO 電療料１ＷＲ.
021320     MOVE レセ－小計１                 TO 小計１ＷＲ.
021330     MOVE レセ－長期逓減率１           TO 長期逓減率１ＷＲ.
021340     MOVE レセ－長期込小計１           TO 長期込小計１ＷＲ.
021350*    **********
021360*    * ２部位 *
021370*    **********
021380     MOVE レセ－後療単価２             TO 後療単価２ＷＲ.
021390     MOVE レセ－後療回数２             TO 後療回数２ＷＲ.
021400     MOVE レセ－後療料２               TO 後療料２ＷＲ.
021410     MOVE レセ－冷罨法回数２           TO 冷罨法回数２ＷＲ.
021420     MOVE レセ－冷罨法料２             TO 冷罨法料２ＷＲ.
021430     MOVE レセ－温罨法回数２           TO 温罨法回数２ＷＲ.
021440     MOVE レセ－温罨法料２             TO 温罨法料２ＷＲ.
021450     MOVE レセ－電療回数２             TO 電療回数２ＷＲ.
021460     MOVE レセ－電療料２               TO 電療料２ＷＲ.
021470     MOVE レセ－小計２                 TO 小計２ＷＲ.
021480     MOVE レセ－長期逓減率２           TO 長期逓減率２ＷＲ.
021490     MOVE レセ－長期込小計２           TO 長期込小計２ＷＲ.
021500*    ****************
021510*    * ３部位／８割 *
021520*    ****************
021530     MOVE レセ－後療単価３８             TO 後療単価３８ＷＲ.
021540     MOVE レセ－後療回数３８             TO 後療回数３８ＷＲ.
021550     MOVE レセ－後療料３８               TO 後療料３８ＷＲ.
021560     MOVE レセ－冷罨法回数３８           TO 冷罨法回数３８ＷＲ.
021570     MOVE レセ－冷罨法料３８             TO 冷罨法料３８ＷＲ.
021580     MOVE レセ－温罨法回数３８           TO 温罨法回数３８ＷＲ.
021590     MOVE レセ－温罨法料３８             TO 温罨法料３８ＷＲ.
021600     MOVE レセ－電療回数３８             TO 電療回数３８ＷＲ.
021610     MOVE レセ－電療料３８               TO 電療料３８ＷＲ.
021620     MOVE レセ－小計３８                 TO 小計３８ＷＲ.
021630     MOVE レセ－多部位込小計３８         TO 多部位込小計３８ＷＲ.
021640     MOVE レセ－長期逓減率３８           TO 長期逓減率３８ＷＲ.
021650     MOVE レセ－長期込小計３８           TO 長期込小計３８ＷＲ.
021660*    ****************
021670*    * ３部位／10割 *
021680*    ****************
021690     MOVE レセ－逓減開始月３０           TO 逓減開始月３０ＷＲ.
021700     MOVE レセ－逓減開始日３０           TO 逓減開始日３０ＷＲ.
021710     MOVE レセ－後療単価３０             TO 後療単価３０ＷＲ.
021720     MOVE レセ－後療回数３０             TO 後療回数３０ＷＲ.
021730     MOVE レセ－後療料３０               TO 後療料３０ＷＲ.
021740     MOVE レセ－冷罨法回数３０           TO 冷罨法回数３０ＷＲ.
021750     MOVE レセ－冷罨法料３０             TO 冷罨法料３０ＷＲ.
021760     MOVE レセ－温罨法回数３０           TO 温罨法回数３０ＷＲ.
021770     MOVE レセ－温罨法料３０             TO 温罨法料３０ＷＲ.
021780     MOVE レセ－電療回数３０             TO 電療回数３０ＷＲ.
021790     MOVE レセ－電療料３０               TO 電療料３０ＷＲ.
021800     MOVE レセ－小計３０                 TO 小計３０ＷＲ.
021810     MOVE レセ－長期逓減率３０           TO 長期逓減率３０ＷＲ.
021820     MOVE レセ－長期込小計３０           TO 長期込小計３０ＷＲ.
021830*    ****************
021840*    * ４部位／５割 *
021850*    ****************
021860     MOVE レセ－後療単価４５             TO 後療単価４５ＷＲ.
021870     MOVE レセ－後療回数４５             TO 後療回数４５ＷＲ.
021880     MOVE レセ－後療料４５               TO 後療料４５ＷＲ.
021890     MOVE レセ－冷罨法回数４５           TO 冷罨法回数４５ＷＲ.
021900     MOVE レセ－冷罨法料４５             TO 冷罨法料４５ＷＲ.
021910     MOVE レセ－温罨法回数４５           TO 温罨法回数４５ＷＲ.
021920     MOVE レセ－温罨法料４５             TO 温罨法料４５ＷＲ.
021930     MOVE レセ－電療回数４５             TO 電療回数４５ＷＲ.
021940     MOVE レセ－電療料４５               TO 電療料４５ＷＲ.
021950     MOVE レセ－小計４５                 TO 小計４５ＷＲ.
021960     MOVE レセ－多部位込小計４５         TO 多部位込小計４５ＷＲ.
021970     MOVE レセ－長期逓減率４５           TO 長期逓減率４５ＷＲ.
021980     MOVE レセ－長期込小計４５           TO 長期込小計４５ＷＲ.
021990*    ****************
022000*    * ４部位／８割 *
022010*    ****************
022020     MOVE レセ－逓減開始月４８           TO 逓減開始月４８ＷＲ.
022030     MOVE レセ－逓減開始日４８           TO 逓減開始日４８ＷＲ.
022040     MOVE レセ－後療単価４８             TO 後療単価４８ＷＲ.
022050     MOVE レセ－後療回数４８             TO 後療回数４８ＷＲ.
022060     MOVE レセ－後療料４８               TO 後療料４８ＷＲ.
022070     MOVE レセ－冷罨法回数４８           TO 冷罨法回数４８ＷＲ.
022080     MOVE レセ－冷罨法料４８             TO 冷罨法料４８ＷＲ.
022090     MOVE レセ－温罨法回数４８           TO 温罨法回数４８ＷＲ.
022100     MOVE レセ－温罨法料４８             TO 温罨法料４８ＷＲ.
022110     MOVE レセ－電療回数４８             TO 電療回数４８ＷＲ.
022120     MOVE レセ－電療料４８               TO 電療料４８ＷＲ.
022130     MOVE レセ－小計４８                 TO 小計４８ＷＲ.
022140     MOVE レセ－多部位込小計４８         TO 多部位込小計４８ＷＲ.
022150     MOVE レセ－長期逓減率４８           TO 長期逓減率４８ＷＲ.
022160     MOVE レセ－長期込小計４８           TO 長期込小計４８ＷＲ.
022170*    ****************
022180*    * ４部位／10割 *
022190*    ****************
022200     MOVE レセ－逓減開始月４０           TO 逓減開始月４０ＷＲ.
022210     MOVE レセ－逓減開始日４０           TO 逓減開始日４０ＷＲ.
022220     MOVE レセ－後療単価４０             TO 後療単価４０ＷＲ.
022230     MOVE レセ－後療回数４０             TO 後療回数４０ＷＲ.
022240     MOVE レセ－後療料４０               TO 後療料４０ＷＲ.
022250     MOVE レセ－冷罨法回数４０           TO 冷罨法回数４０ＷＲ.
022260     MOVE レセ－冷罨法料４０             TO 冷罨法料４０ＷＲ.
022270     MOVE レセ－温罨法回数４０           TO 温罨法回数４０ＷＲ.
022280     MOVE レセ－温罨法料４０             TO 温罨法料４０ＷＲ.
022290     MOVE レセ－電療回数４０             TO 電療回数４０ＷＲ.
022300     MOVE レセ－電療料４０               TO 電療料４０ＷＲ.
022310     MOVE レセ－小計４０                 TO 小計４０ＷＲ.
022320     MOVE レセ－長期逓減率４０           TO 長期逓減率４０ＷＲ.
022330     MOVE レセ－長期込小計４０           TO 長期込小計４０ＷＲ.
022340*    *****************
022350*    * ５部位／2.5割 *
022360*    *****************
022370     MOVE レセ－後療単価５２             TO 後療単価５２ＷＲ.
022380     MOVE レセ－後療回数５２             TO 後療回数５２ＷＲ.
022390     MOVE レセ－後療料５２               TO 後療料５２ＷＲ.
022400     MOVE レセ－冷罨法回数５２           TO 冷罨法回数５２ＷＲ.
022410     MOVE レセ－冷罨法料５２             TO 冷罨法料５２ＷＲ.
022420     MOVE レセ－温罨法回数５２           TO 温罨法回数５２ＷＲ.
022430     MOVE レセ－温罨法料５２             TO 温罨法料５２ＷＲ.
022440     MOVE レセ－電療回数５２             TO 電療回数５２ＷＲ.
022450     MOVE レセ－電療料５２               TO 電療料５２ＷＲ.
022460     MOVE レセ－小計５２                 TO 小計５２ＷＲ.
022470     MOVE レセ－多部位込小計５２         TO 多部位込小計５２ＷＲ.
022480     MOVE レセ－長期逓減率５２           TO 長期逓減率５２ＷＲ.
022490     MOVE レセ－長期込小計５２           TO 長期込小計５２ＷＲ.
022500*    ****************
022510*    * ５部位／５割 *
022520*    ****************
022530     MOVE レセ－逓減開始月５５           TO 逓減開始月５５ＷＲ.
022540     MOVE レセ－逓減開始日５５           TO 逓減開始日５５ＷＲ.
022550     MOVE レセ－後療単価５５             TO 後療単価５５ＷＲ.
022560     MOVE レセ－後療回数５５             TO 後療回数５５ＷＲ.
022570     MOVE レセ－後療料５５               TO 後療料５５ＷＲ.
022580     MOVE レセ－冷罨法回数５５           TO 冷罨法回数５５ＷＲ.
022590     MOVE レセ－冷罨法料５５             TO 冷罨法料５５ＷＲ.
022600     MOVE レセ－温罨法回数５５           TO 温罨法回数５５ＷＲ.
022610     MOVE レセ－温罨法料５５             TO 温罨法料５５ＷＲ.
022620     MOVE レセ－電療回数５５             TO 電療回数５５ＷＲ.
022630     MOVE レセ－電療料５５               TO 電療料５５ＷＲ.
022640     MOVE レセ－小計５５                 TO 小計５５ＷＲ.
022650     MOVE レセ－多部位込小計５５         TO 多部位込小計５５ＷＲ.
022660     MOVE レセ－長期逓減率５５           TO 長期逓減率５５ＷＲ.
022670     MOVE レセ－長期込小計５５           TO 長期込小計５５ＷＲ.
022680*    ****************
022690*    * ５部位／８割 *
022700*    ****************
022710     MOVE レセ－逓減開始月５８           TO 逓減開始月５８ＷＲ.
022720     MOVE レセ－逓減開始日５８           TO 逓減開始日５８ＷＲ.
022730     MOVE レセ－後療単価５８             TO 後療単価５８ＷＲ.
022740     MOVE レセ－後療回数５８             TO 後療回数５８ＷＲ.
022750     MOVE レセ－後療料５８               TO 後療料５８ＷＲ.
022760     MOVE レセ－冷罨法回数５８           TO 冷罨法回数５８ＷＲ.
022770     MOVE レセ－冷罨法料５８             TO 冷罨法料５８ＷＲ.
022780     MOVE レセ－温罨法回数５８           TO 温罨法回数５８ＷＲ.
022790     MOVE レセ－温罨法料５８             TO 温罨法料５８ＷＲ.
022800     MOVE レセ－電療回数５８             TO 電療回数５８ＷＲ.
022810     MOVE レセ－電療料５８               TO 電療料５８ＷＲ.
022820     MOVE レセ－小計５８                 TO 小計５８ＷＲ.
022830     MOVE レセ－多部位込小計５８         TO 多部位込小計５８ＷＲ.
022840     MOVE レセ－長期逓減率５８           TO 長期逓減率５８ＷＲ.
022850     MOVE レセ－長期込小計５８           TO 長期込小計５８ＷＲ.
022860*    ****************
022870*    * ５部位／10割 *
022880*    ****************
022890     MOVE レセ－逓減開始月５０           TO 逓減開始月５０ＷＲ.
022900     MOVE レセ－逓減開始日５０           TO 逓減開始日５０ＷＲ.
022910     MOVE レセ－後療単価５０             TO 後療単価５０ＷＲ.
022920     MOVE レセ－後療回数５０             TO 後療回数５０ＷＲ.
022930     MOVE レセ－後療料５０               TO 後療料５０ＷＲ.
022940     MOVE レセ－冷罨法回数５０           TO 冷罨法回数５０ＷＲ.
022950     MOVE レセ－冷罨法料５０             TO 冷罨法料５０ＷＲ.
022960     MOVE レセ－温罨法回数５０           TO 温罨法回数５０ＷＲ.
022970     MOVE レセ－温罨法料５０             TO 温罨法料５０ＷＲ.
022980     MOVE レセ－電療回数５０             TO 電療回数５０ＷＲ.
022990     MOVE レセ－電療料５０               TO 電療料５０ＷＲ.
023000     MOVE レセ－小計５０                 TO 小計５０ＷＲ.
023010     MOVE レセ－長期逓減率５０           TO 長期逓減率５０ＷＲ.
023020     MOVE レセ－長期込小計５０           TO 長期込小計５０ＷＲ.
023030*
023040*================================================================*
023050 施術所情報取得 SECTION.
023060*
023070**************************************************
023080* 本院データを使用し、以下の情報を取得           *
023090* ● 柔整師番号.. 柔整師番号Ｗに格納             *
023100* ● 会員番号 ... 接骨師会会員番号Ｗに格納       *
023110* ● 代表者名 ... 代表者名Ｗに格納               *
023120* ● 住所1,2   ...施術所住所1,2Ｗに格納          *
023130* ● 電話番号 ... 施術所電話番号Ｗに格納         *
023140**************************************************
023150     MOVE ZERO  TO 施情－施術所番号.
023160     READ 施術所情報マスタ
023170     INVALID KEY
023180         CONTINUE
023190     NOT INVALID KEY
023200*
023240         MOVE 施情－新柔整師番号   TO 柔整師番号Ｗ
023260*
023270         MOVE 施情－接骨師会会員番号  TO 会員番号Ｗ
023280         MOVE "ＪＢ－"                TO 会ＪＢＷ
023290*
023300*** 共済・自衛官の時のみ、柔整師番号の編集をする。
023310         EVALUATE 保険種別ＷＲ
023320         WHEN 04
023330             PERFORM 共済番号セット
023340         WHEN 09
023350             PERFORM 自衛官番号セット
023360         END-EVALUATE
023370***
023380*
023390         MOVE 施情－郵便番号１        TO 施術所郵便番号１Ｗ
023400         MOVE 施情－郵便番号２        TO 施術所郵便番号２Ｗ
023410         MOVE 施情－代表者カナ        TO 代表者カナＷ
023420         MOVE 施情－代表者名          TO 代表者名Ｗ
023430*
023440         MOVE 施情－接骨院名          TO 接骨院名Ｗ
               MOVE 施情－都道府県ＪＩＳ    TO 都道府県ＪＩＳＷ
023450*
023460         MOVE 施情－住所１            TO 施術所住所１Ｗ
023470         MOVE 施情－住所２            TO 施術所住所２Ｗ
023480*         MOVE SPACE           TO 住所連結Ｗ
023490*         STRING 施情－住所１  DELIMITED BY SPACE
023500*                施情－住所２  DELIMITED BY SPACE
023510*           INTO 住所連結Ｗ
023520*         END-STRING
023530*         IF ( 住所連結２Ｗ = SPACE )
023540*            MOVE SPACE        TO 施術所住所１Ｗ
023550*            MOVE 住所連結１Ｗ TO 施術所住所２Ｗ
023560*         ELSE
023570*            MOVE 施情－住所１ TO 施術所住所１Ｗ
023580*            MOVE SPACE        TO 住所編集Ｗ
023590*            MOVE 施情－住所２ TO 住所編集１Ｗ
023600*            MOVE 住所編集Ｗ   TO 施術所住所２Ｗ
023610*         END-IF
023620*
023630         MOVE 施情－電話番号          TO 施術所電話番号Ｗ
023640** 振込先情報
023650*         MOVE 施情－取引先銀行名      TO 取引先銀行名Ｗ
023660*         MOVE 施情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
023670*         MOVE 施情－預金種別          TO 預金種別Ｗ
023680*         MOVE 施情－口座番号          TO 口座番号Ｗ
023690*         MOVE 施情－口座名義人        TO 口座名義人Ｗ
023700*         MOVE 施情－口座名義人カナ    TO 口座名義人カナＷ
023710*         STRING 取引先銀行名Ｗ     DELIMITED BY SPACE
023720*                " "                DELIMITED BY SIZE
023730*                取引先銀行支店名Ｗ DELIMITED BY SPACE
023740*                INTO 銀行名支店名Ｗ
023750*         END-STRING
023760*         EVALUATE 預金種別Ｗ
023770*         WHEN 1
023780*             MOVE NC"普通" TO 預金種別コメントＷ
023790*         WHEN 2
023800*             MOVE NC"当座" TO 預金種別コメントＷ
023810*         WHEN OTHER
023820*             MOVE SPACE    TO 預金種別コメントＷ
023830*         END-EVALUATE
023840*
023850     END-READ.
023860*
026770*********************************************
026780** ＩＤ管理マスタより　県施術ＩＤを取得する。
026790*********************************************
026970     MOVE SPACE TO 県施術ＩＤＷ 保険者番号比較Ｗ.
           IF 連レ－保険種別 < 50
026800         EVALUATE 保険種別ＷＲ
026820             WHEN 01
026830                MOVE 保険者番号ＷＲ       TO 保険者番号比較Ｗ
026850             WHEN 08
026870             WHEN 05
026880                MOVE 保険者番号ＷＲ(3:6)  TO 保険者番号比較Ｗ
026890         END-EVALUATE
      *     ELSE
      *         MOVE 費用負担者番号助成ＷＲ(3:6) TO 保険者番号比較Ｗ
           END-IF.
      */ＪＢ神奈川県の国保退職後期のみ施術機関コードを印刷する/121102
           IF 保険者番号比較Ｗ(1:2) = "14"
026900**   / 県施術ID /
026910         MOVE 01                          TO ＩＤ管－ＩＤ区分
026920         MOVE ZERO                        TO ＩＤ管－施術所番号
026930         MOVE 保険者番号比較Ｗ(1:2)       TO ＩＤ管－保険種別
026940         MOVE SPACE                       TO ＩＤ管－保険者番号
026950         READ ＩＤ管理マスタ
026960         NOT INVALID KEY
026970             MOVE ＩＤ管－施術ＩＤ番号    TO 県施術ＩＤＷ
026980         END-READ
           END-IF.
      *
      */神奈川県の助成レセに施術機関コードを印刷する/130406
           IF (施術和暦年月ＷＲ >= 42504) AND (連レ－保険種別 > 50) AND (費用負担者番号助成ＷＲ(3:2) = "14")
026900**   / 県施術ID /
026910         MOVE 01                          TO ＩＤ管－ＩＤ区分
026920         MOVE ZERO                        TO ＩＤ管－施術所番号
026930         MOVE 費用負担者番号助成ＷＲ(3:2) TO ＩＤ管－保険種別
026940         MOVE SPACE                       TO ＩＤ管－保険者番号
026950         READ ＩＤ管理マスタ
026960         NOT INVALID KEY
026970             MOVE ＩＤ管－施術ＩＤ番号    TO 県施術ＩＤＷ
026980         END-READ
           END-IF.
023870*================================================================*
023880* 共済番号セット SECTION.
023890*
023900**************************************************************
023910* 保険者番号により、共済の番号を印字するか、柔整師番号か判定
023920**************************************************************
023930** 1.共済組合連盟
023940*     MOVE SPACE  TO  脱出フラグ.
023950*     IF 施情－共済連番号 NOT = ZERO
023960** 条件(保険者番号)
023970*        IF ( 保険者番号ＷＲ(1:2) = "31" )  OR
023980*           ( 保険者番号ＷＲ = "34130021" )
023990*
024000*           MOVE  NC"共済組合連盟第"   TO 共済連番号名ＮＷ 
024010*           MOVE  NC"号"               TO 共済連番号単位ＮＷ 
024020*           MOVE  施情－共済連番号     TO 共済連番号Ｗ
024030*           IF    (共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
024040*                 MOVE SPACE TO  共済連番号Ｗ(1:1)
024050*           ELSE
024060*                 MOVE "YES" TO  脱出フラグ
024070*           END-IF
024080*           IF    (共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
024090*                 MOVE SPACE TO  共済連番号Ｗ(2:1)
024100*           ELSE
024110*                 MOVE "YES" TO  脱出フラグ
024120*           END-IF
024130*           IF    (共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
024140*                 MOVE SPACE TO  共済連番号Ｗ(3:1)
024150*           ELSE
024160*                 MOVE "YES" TO  脱出フラグ
024170*           END-IF
024180*           IF    (共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
024190*                 MOVE SPACE TO  共済連番号Ｗ(4:1)
024200*           ELSE
024210*                 MOVE "YES" TO  脱出フラグ
024220*           END-IF
024230*           IF    (共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
024240*                 MOVE SPACE TO  共済連番号Ｗ(5:1)
024250*           ELSE
024260*                 MOVE "YES" TO  脱出フラグ
024270*           END-IF
024280*           IF    (共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
024290*                 MOVE SPACE TO  共済連番号Ｗ(6:1)
024300*           ELSE
024310*                 MOVE "YES" TO  脱出フラグ
024320*           END-IF
024330*           MOVE  共済連番号集団Ｗ     TO 柔整師番号２Ｗ
024340*        END-IF
024350*     END-IF.
024360**
024370*** 2. 地共済協議会
024380*     MOVE SPACE  TO  脱出フラグ.
024390*     IF 施情－地共済連番号 NOT = ZERO
024400*** 条件(保険者番号)
024410*        IF ( 保険者番号ＷＲ(1:2) = "32" OR "33" OR "34" )  AND
024420*           ( 保険者番号ＷＲ NOT = "34130021" )
024430**
024440*           MOVE  NC"地共済協議会"     TO 共済連番号名ＮＷ 
024450*           MOVE  NC"号"               TO 共済連番号単位ＮＷ 
024460*           MOVE  施情－地共済連番号   TO 共済連番号Ｗ
024470*           IF    (共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
024480*                 MOVE SPACE TO  共済連番号Ｗ(1:1)
024490*           ELSE
024500*                 MOVE "YES" TO  脱出フラグ
024510*           END-IF
024520*           IF    (共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
024530*                 MOVE SPACE TO  共済連番号Ｗ(2:1)
024540*           ELSE
024550*                 MOVE "YES" TO  脱出フラグ
024560*           END-IF
024570*           IF    (共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
024580*                 MOVE SPACE TO  共済連番号Ｗ(3:1)
024590*           ELSE
024600*                 MOVE "YES" TO  脱出フラグ
024610*           END-IF
024620*           IF    (共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
024630*                 MOVE SPACE TO  共済連番号Ｗ(4:1)
024640*           ELSE
024650*                 MOVE "YES" TO  脱出フラグ
024660*           END-IF
024670*           IF    (共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
024680*                 MOVE SPACE TO  共済連番号Ｗ(5:1)
024690*           ELSE
024700*                 MOVE "YES" TO  脱出フラグ
024710*           END-IF
024720*           IF    (共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
024730*                 MOVE SPACE TO  共済連番号Ｗ(6:1)
024740*           ELSE
024750*                 MOVE "YES" TO  脱出フラグ
024760*           END-IF
024770*           MOVE  共済連番号集団Ｗ     TO 柔整師番号２Ｗ
024780**
      **/特別処理をやめる/100708
024790***** -- 特別処理---------------------------------------------------------**
024800*****    静岡県開業の人は、32220113(地方職員静岡)のみ地共済番号セット
024810**           IF  施情－都道府県ＪＩＳ = "22" 
024820**               IF 保険者番号ＷＲ  NOT = "32220113" 
024830**                  MOVE SPACE TO 柔整師番号２Ｗ
024840**               END-IF
024850**           END-IF
024860*****---------------------------------------------------------------------**
024870*        END-IF
024880*     END-IF.
024890**
024900**================================================================*
024910 自衛官番号セット SECTION.
024920*
024930     MOVE SPACE  TO  脱出フラグ.
024940     IF 施情－自衛官番号 NOT = ZERO
024941           IF 施情－防衛省区分 = 1
024942              MOVE  NC"防衛省第"      TO 自衛官番号名ＮＷ 
024943           ELSE
024944              MOVE  NC"防衛庁第"      TO 自衛官番号名ＮＷ 
024945           END-IF
024950*           MOVE  NC"防衛庁第"         TO 自衛官番号名ＮＷ 
024960           MOVE  NC"号"               TO 自衛官番号単位ＮＷ 
024970           MOVE  施情－自衛官番号     TO 自衛官番号Ｗ
024980           IF    (自衛官番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
024990                 MOVE SPACE TO  自衛官番号Ｗ(1:1)
025000           ELSE
025010                 MOVE "YES" TO  脱出フラグ
025020           END-IF
025030           IF    (自衛官番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
025040                 MOVE SPACE TO  自衛官番号Ｗ(2:1)
025050           ELSE
025060                 MOVE "YES" TO  脱出フラグ
025070           END-IF
025080           IF    (自衛官番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
025090                 MOVE SPACE TO  自衛官番号Ｗ(3:1)
025100           ELSE
025110                 MOVE "YES" TO  脱出フラグ
025120           END-IF
025130           IF    (自衛官番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
025140                 MOVE SPACE TO  自衛官番号Ｗ(4:1)
025150           ELSE
025160                 MOVE "YES" TO  脱出フラグ
025170           END-IF
025180           IF    (自衛官番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
025190                 MOVE SPACE TO  自衛官番号Ｗ(5:1)
025200           ELSE
025210                 MOVE "YES" TO  脱出フラグ
025220           END-IF
025230           IF    (自衛官番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
025240                 MOVE SPACE TO  自衛官番号Ｗ(6:1)
025250           ELSE
025260                 MOVE "YES" TO  脱出フラグ
025270           END-IF
025280           MOVE  自衛官番号集団Ｗ     TO 柔整師番号２Ｗ
025290     END-IF.
025300*
025310*================================================================*
025320 受診者情報取得 SECTION.
025330*
025340**************************************************
025350* 連結データから受診者情報Ｆより以下の情報を取得 *
025360* ● 施術年 ..... 施術年Ｗに格納                 *
025370* ● 施術月 ..... 施術月Ｗに格納                 *
025380* ● 患者番号.... 患者番号Ｗに格納※ＦＤ連番用   *
025390* ● 記号 ....... 記号Ｗに格納                   *
025400* ● 番号 ....... 番号Ｗに格納                   *
025410* ● 保険者番号 . 保険者番号Ｗに格納             *
025420* ● 保険種別 ... 保険種別Ｗに格納               *
025430* ● 被保険者カナ.被保険者カナＷに格納           *
025440* ● 被保険者氏名.被保険者氏名Ｗに格納           *
025450* ● 住所１ ......被保険者住所１Ｗに格納         *
025460* ● 住所２ ......被保険者住所２Ｗに格納         *
025470* ● 患者カナ ....患者カナＷに格納               *
025480* ● 患者氏名 ....患者氏名Ｗに格納               *
025490* ● 患者性別 ....区分によりチェックに"○"を格納 *
025500* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
025510* ● 患者年 ......患者年Ｗに格納                 *
025520* ● 患者月 ......患者月Ｗに格納                 *
025530* ● 患者日 ......患者日Ｗに格納                 *
025540* ● 続柄 ........名称マスタより続柄Ｗに取得     *
025550**************************************************
026460     MOVE 施術和暦ＷＲ       TO 受２－施術和暦.
026470     MOVE 施術年ＷＲ         TO 受２－施術年.
026480     MOVE 施術月ＷＲ         TO 受２－施術月.
026490     MOVE 患者コードＷＲ     TO 受２－患者コード.
026500     READ 受診者情報２Ｆ
           INVALID KEY
              MOVE SPACE           TO 受２－レコード
           END-READ.
025560     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
025570     MOVE 施術年ＷＲ         TO 受－施術年.
025580     MOVE 施術月ＷＲ         TO 受－施術月.
025590     MOVE 患者コードＷＲ     TO 受－患者コード.
025600     READ 受診者情報Ｆ
025610     INVALID KEY
025620         CONTINUE
025630*            /* ありえない */
025640     NOT INVALID KEY
026962         IF 受－助成種別 NOT = ZERO
026963            PERFORM 助成レセまとめ判定
026964         ELSE
026965            MOVE SPACE TO 助成レセまとめフラグ
026966         END-IF
014880*
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
      *         WHEN 09
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
      */福島 子ども医療の時単独に"○"/1703
               IF (受－助成種別 = 55 OR 60) AND (受－費用負担者番号助成(3:2) = "07") AND
                  (助成レセまとめフラグ = "YES")
                   MOVE NC"○" TO 単独チェックＷ
                   MOVE SPACE  TO ２併チェックＷ
               END-IF
      **/本家区分はどれか１つに○をする。
      */本家区分は独立して○付け。
      *         IF 受－本人家族区分 = 1
      *             MOVE NC"○" TO 本人チェックＷ
      *         ELSE
      *             MOVE NC"○" TO 家族チェックＷ
      *         END-IF
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
      *                 MOVE SPACE  TO 本人チェックＷ 家族チェックＷ
                   WHEN OTHER
                       IF 受－本人家族区分 = 1
                           MOVE NC"○" TO 本人チェックＷ
                       ELSE
                           MOVE NC"○" TO 家族チェックＷ
                       END-IF
028999             END-EVALUATE
               END-IF
      *
               IF ( 連レ－保険種別 > 50 )
                  EVALUATE 受２－助成本人家族区分
                  WHEN 1
                      MOVE NC"○" TO 本人チェックＷ
                      MOVE SPACE  TO 家族チェックＷ
                  WHEN 2
                      MOVE SPACE  TO 本人チェックＷ
                      MOVE NC"○" TO 家族チェックＷ
                  END-EVALUATE
               END-IF
      *
      *         IF (受－保険種別 = 01 OR 08) OR (連レ－保険種別 > 50)
      */福島 子ども医療の時１０割に"○"/1703
               IF (受－助成種別 = 55 OR 60) AND (受－費用負担者番号助成(3:2) = "07") AND
                  (助成レセまとめフラグ = "YES")
                   MOVE NC"○" TO １０割チェックＷ
               ELSE
                   EVALUATE レセ－給付割合
                   WHEN 10
                       MOVE NC"○" TO １０割チェックＷ
                   WHEN 9
                       MOVE NC"○" TO ９割チェックＷ
      */ＪＢの前期高齢１割は８割給付に○/120525
                       IF (受－保険種別 NOT = 05 ) AND (受－特別区分 = 1) AND
                          (受－保険者番号 NOT = "01170018" AND "01220011" AND "01430016" AND
                                                "138081")
                           MOVE SPACE  TO ９割チェックＷ
                           MOVE NC"○" TO ８割チェックＷ
                       END-IF
                   WHEN 8
                       MOVE NC"○" TO ８割チェックＷ
                   WHEN 7
                       MOVE NC"○" TO ７割チェックＷ
                   END-EVALUATE
               END-IF
      *
      */元号修正/20190405
               MOVE 受－施術和暦     TO 施術和暦Ｗ
025820         MOVE 受－施術年       TO 施術年Ｗ
025830         MOVE 受－施術月       TO 施術月Ｗ
025840         MOVE 受－患者番号     TO 患者番号Ｗ
025850         MOVE 受－記号         TO 記号Ｗ
025860         MOVE 受－番号         TO 番号Ｗ
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
025870         MOVE 受－保険者番号   TO 保険者番号Ｗ
025880         MOVE 受－保険種別     TO 保険種別Ｗ
               MOVE 受－費用負担者番号助成 TO 公費負担者番号Ｗ
               MOVE 受－受益者番号助成     TO 受給者番号Ｗ
025890** 全国土木の枝番削除
025900         IF ( 受－保険種別 = 01 ) AND ( 受－保険者番号(1:6) = "133033" )
025910            MOVE 受－保険者番号(1:6)  TO 保険者番号Ｗ
025920         END-IF
025930**
025940         MOVE 受－被保険者カナ TO 被保険者カナＷ
025950         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
025960         MOVE 受－郵便番号１   TO 郵便番号１Ｗ
025970         MOVE 受－郵便番号２   TO 郵便番号２Ｗ
025980         MOVE 受－住所１       TO 被保険者住所１Ｗ
025990         MOVE 受－住所２       TO 被保険者住所２Ｗ
      */ 電話番号追加 /42505
               IF 受－電話番号 NOT = SPACE
                  STRING "電話:"        DELIMITED BY SIZE
                         受－電話番号   DELIMITED BY SPACE
                    INTO 電話番号Ｗ
                  END-STRING
               END-IF
026000         MOVE 受－患者カナ     TO 患者カナＷ
026010         MOVE 受－患者氏名     TO 患者氏名Ｗ
029550         MOVE 受－患者郵便番号１ TO 患者郵便番号１Ｗ
029560         MOVE 受－患者郵便番号２ TO 患者郵便番号２Ｗ
029570         MOVE 受－患者住所１     TO 患者住所１Ｗ
029580         MOVE 受－患者住所２     TO 患者住所２Ｗ
      */ 電話番号追加 /42505
               IF 受－患者電話番号 NOT = SPACE
                  STRING "電話:"            DELIMITED BY SIZE
                         受－患者電話番号   DELIMITED BY SPACE
                    INTO 患者電話番号Ｗ
                  END-STRING
               END-IF
026020*
026030         EVALUATE 受－被保険者性別
026040         WHEN 1
026050             MOVE NC"男"  TO 被保険者性別Ｗ
026060         WHEN 2
026070             MOVE NC"女"  TO 被保険者性別Ｗ
026080         END-EVALUATE
026090         EVALUATE 受－患者性別
026100         WHEN 1
026110             MOVE NC"男"  TO 患者性別Ｗ
026120         WHEN 2
026130             MOVE NC"女"  TO 患者性別Ｗ
026140         END-EVALUATE
026150         EVALUATE 受－患者性別
026160         WHEN 1
026170             MOVE NC"○"  TO 男チェックＷ
026180         WHEN 2
026190             MOVE NC"○"  TO 女チェックＷ
026200         END-EVALUATE
026210         EVALUATE 受－患者和暦
026220         WHEN 1
026230             MOVE NC"○"  TO 明治チェックＷ
026240         WHEN 2
026250             MOVE NC"○"  TO 大正チェックＷ
026260         WHEN 3
026270             MOVE NC"○"  TO 昭和チェックＷ
026280         WHEN 4
026290             MOVE NC"○"  TO 平成チェックＷ
      */元号修正/20190405
023060         WHEN 5
                   MOVE "5令"   TO 令和ＣＭＷ
023070             MOVE NC"○"  TO 令和チェックＷ
026300         END-EVALUATE
      */元号修正/↓↓↓20190405
029310         IF 受－患者和暦 > 4
037370             MOVE 受－患者和暦     TO 元－元号区分
037380             READ 元号マスタ
037390             NOT INVALID KEY
037400                 MOVE 元－元号名称   TO 元号Ｗ
037410             END-READ
029330         END-IF
      */元号修正/↑↑↑20190405
026310*         EVALUATE 受－患者和暦
026320*         WHEN 1
026330*             MOVE NC"明治"  TO 元号Ｗ
026340*         WHEN 2
026350*             MOVE NC"大正"  TO 元号Ｗ
026360*         WHEN 3
026370*             MOVE NC"昭和"  TO 元号Ｗ
026380*         WHEN 4
026390*             MOVE NC"平成"  TO 元号Ｗ
026400*         END-EVALUATE
026410*
026420         MOVE 受－患者年  TO 患者年Ｗ
026430         MOVE 受－患者月  TO 患者月Ｗ
026440         MOVE 受－患者日  TO 患者日Ｗ
026450* 続柄
026460         EVALUATE 保険種別ＷＲ 
026470* 自衛官は無条件に"本人"
026480         WHEN  09
026490              MOVE NC"本人"    TO 続柄Ｗ
026500* 退職
026510         WHEN  08
026511*          / 15/05変更
026512             IF 本人家族区分ＷＲ = 1
026513                 MOVE NC"本人"    TO 続柄Ｗ
026514             ELSE
026515                 MOVE NC"家族"    TO 続柄Ｗ
026516             END-IF
026520*             IF (本人家族区分ＷＲ = 1 ) AND (受－世帯主続柄 = 1)
026530*                 MOVE NC"世帯主"  TO 続柄Ｗ
026540*             ELSE
026550*                 MOVE 05          TO 名－区分コード
026560*                 MOVE 受－続柄    TO 名－名称コード
026570*                 READ 名称マスタ
026580*                 INVALID KEY
026590*                     MOVE SPACE    TO 続柄Ｗ
026600*                 NOT INVALID KEY
026610*                     MOVE 名－略称 TO 続柄Ｗ
026620*                 END-READ
026630*             END-IF
026640* 国保
026650         WHEN 01
026660             IF 本人家族区分ＷＲ = 1
026670                IF 受－保険者番号(3:1) = "3" 
026680                   MOVE NC"本人"    TO 続柄Ｗ
026690                ELSE
026700                   MOVE NC"世帯主"  TO 続柄Ｗ
026710                END-IF
026720             ELSE
026730                 MOVE 05          TO 名－区分コード
026740                 MOVE 受－続柄    TO 名－名称コード
026750                 READ 名称マスタ
026760                 INVALID KEY
026770                     MOVE SPACE    TO 続柄Ｗ
026780                 NOT INVALID KEY
026790                     MOVE 名－略称 TO 続柄Ｗ
026800                 END-READ
026810             END-IF
026820         WHEN OTHER
026830             IF 本人家族区分ＷＲ = 1
026840                 MOVE NC"本人"    TO 続柄Ｗ
026850             ELSE
026860                 MOVE 05          TO 名－区分コード
026870                 MOVE 受－続柄    TO 名－名称コード
026880                 READ 名称マスタ
026890                 INVALID KEY
026900                     MOVE SPACE    TO 続柄Ｗ
026910                 NOT INVALID KEY
026920                     MOVE 名－略称 TO 続柄Ｗ
026930                 END-READ
026940             END-IF
026950         END-EVALUATE
026960*
026967**
026970* 14/10～　特別区分コメント印字
026980         IF 受－施術和暦年月 >= 41410
026990             IF 受－公費種別 = ZERO
027000                EVALUATE 受－特別区分
027010                WHEN 1
027020                   MOVE NC"○"  TO ７０歳チェックＷ
027030                   MOVE NC"○"  TO ７０歳１チェックＷ
027040                WHEN 2
027050                   MOVE NC"○"  TO ７０歳チェックＷ
027060                   MOVE NC"○"  TO ７０歳２チェックＷ
027061                WHEN 3
027062                   MOVE NC"○"  TO ７０歳チェックＷ
027063                   MOVE NC"○"  TO ７０歳３チェックＷ
027070                WHEN 6
      */義務教育就学前/080408
                         IF 受－施術和暦年月 < 42004
027080                       MOVE NC"○"  TO ３歳未満チェックＷ
                         ELSE
                             MOVE NC"義務教育就学前" TO 義務教育就学前Ｗ
                         END-IF
027090                END-EVALUATE
027100             END-IF
027110         END-IF
      */後期高齢者
               IF (受－保険種別 = 05) AND (受－施術和暦年月 >= 42004)
                   EVALUATE 受－特別区分
027010             WHEN 1
027020                MOVE NC"○"  TO ７０歳チェックＷ
027030                MOVE NC"○"  TO ７０歳１チェックＷ
027040             WHEN 2
027050                MOVE NC"○"  TO ７０歳チェックＷ
027060                MOVE NC"○"  TO ７０歳２チェックＷ
027061             WHEN 3
027062                MOVE NC"○"  TO ７０歳チェックＷ
027063                MOVE NC"○"  TO ７０歳３チェックＷ
                   END-EVALUATE
               END-IF
027120*
027130     END-READ.
027140*================================================================*
027150 請求先情報取得 SECTION.
027160*
027170****************************************************
027180* 連結データから保険者マスタより請求先を取得する。 *
027190* ※保－請求先情報区分=1の場合請求先マスタを使用   *
027200* ● 請求先...... 請求先名称Ｗに格納               *
027210****************************************************
027220     MOVE 保険種別ＷＲ   TO 保－保険種別.
027230     MOVE 保険者番号ＷＲ TO 保－保険者番号.
027240     READ 保険者マスタ
027250     INVALID KEY
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
027270     NOT INVALID KEY
027280*
027290                 EVALUATE 保険種別ＷＲ
027300** 社保・日雇
027310                 WHEN 2
027320                 WHEN 6
027330                     IF 保－接尾語区分 = 1
027340                        MOVE 保－保険者名称    TO 請求先名称Ｗ
027350                     ELSE
027360                        STRING 保－保険者名称    DELIMITED BY SPACE
027370                               "社会保険事務所"  DELIMITED BY SIZE
027380                               INTO 請求先名称Ｗ
027390                        END-STRING
027400                     END-IF
027410** 組合・共済は支部名まで印字
027420                 WHEN 3
027430                     STRING 保－保険者名称    DELIMITED BY SPACE
027440                            "健康保険組合"    DELIMITED BY SIZE
027450                            保－支部部署名    DELIMITED BY SPACE
027460                            INTO 請求先名称Ｗ
027470                     END-STRING
027480                 WHEN 4
027490                     STRING 保－保険者名称    DELIMITED BY SPACE
027500                            "共済組合"        DELIMITED BY SIZE
027510                            保－支部部署名    DELIMITED BY SPACE
027520                            INTO 請求先名称Ｗ
027530                     END-STRING
027540                 WHEN OTHER
027550                     MOVE 保－保険者名称      TO 請求先名称Ｗ
027560                 END-EVALUATE
027570     END-READ.
027580*
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
027590*================================================================*
027600 負傷データ取得 SECTION.
027610*
027620**************************************************
027630* 連結データから負傷データＦより以下の情報を取得 *
027640* ● 負傷名...部位＋負傷種別にて加工して格納     *
027650* ● 負傷年.......負傷年Ｗ                       *
027660* ● 負傷月.......負傷月Ｗ                       *
027670* ● 負傷日.......負傷日Ｗ                       *
027680* ● 開始年.......初検年Ｗ                       *
027690* ● 開始月.......初検月Ｗ                       *
027700* ● 開始日.......初検日Ｗ                       *
027710* ● 終了年.......終了年Ｗ                       *
027720* ● 終了月.......終了月Ｗ                       *
027730* ● 終了日.......終了日Ｗ                       *
027740* ● 実日数.......実日数Ｗ                       *
027750* ● 転帰区分 ....区分によりチェックに"○"を格納 *
027760* ● 金属副子 ....区分によりチェックに"○"を格納 *
027770* ● 経過コード...経過マスタより取得             *
027780**************************************************
027790*     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
027800*     MOVE 施術年ＷＲ         TO 負－施術年.
027810*     MOVE 施術月ＷＲ         TO 負－施術月.
027820*     MOVE 患者コードＷＲ     TO 負－患者コード.
027830*     READ 負傷データＦ
027840*     INVALID KEY
027850*         CONTINUE
027860**            /* ありえない */
027870*     NOT INVALID KEY
027880         MOVE 負－レセ負傷原因印刷区分     TO 負傷原因印刷区分Ｆ.
027880         MOVE 負－レセ長期理由印刷区分     TO 長期理由印刷区分Ｆ.
027890*
027900*         MOVE 負－部位数                   TO 部位数Ｗ
027910         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
027920                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
027930             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別Ｗ(部位ＣＮＴ)
027940             MOVE 負－部位(部位ＣＮＴ)     TO 部位Ｗ(部位ＣＮＴ)
027950             MOVE 負－左右区分(部位ＣＮＴ) TO 左右区分Ｗ(部位ＣＮＴ)
027960             MOVE 負－負傷位置番号(部位ＣＮＴ)
027970                                           TO 負傷位置番号Ｗ(部位ＣＮＴ)
027980********************************************************
027990* 注）全柔...部位名1+負傷種別＋部位名2にて加工して格納 *
028000********************************************************
028010* 負傷種別
028020             MOVE SPACE                     TO 負傷名称Ｗ
028030             MOVE 03                        TO 名－区分コード
028040             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
028050             READ 名称マスタ
028060             INVALID KEY
028070                 MOVE SPACE        TO 負傷名称Ｗ
028080             NOT INVALID KEY
028090                 MOVE 名－正式名称 TO 負傷名称Ｗ
028100             END-READ
028110* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
028160*
028170             PERFORM 部位名称埋込処理
028180*
028310             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
028320             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
028330             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
028340             MOVE 負－開始年(部位ＣＮＴ)   TO 初検年Ｗ(部位ＣＮＴ)
028350             MOVE 負－開始月(部位ＣＮＴ)   TO 初検月Ｗ(部位ＣＮＴ)
028360             MOVE 負－開始日(部位ＣＮＴ)   TO 初検日Ｗ(部位ＣＮＴ)
028370             IF 負－転帰区分(部位ＣＮＴ) = 9
028380                 MOVE 99                   TO 終了年Ｗ(部位ＣＮＴ)
028390                 MOVE 99                   TO 終了月Ｗ(部位ＣＮＴ)
028400                 MOVE 99                   TO 終了日Ｗ(部位ＣＮＴ)
028410             ELSE
028420                 MOVE 負－終了年(部位ＣＮＴ)   TO 終了年Ｗ(部位ＣＮＴ)
028430                 MOVE 負－終了月(部位ＣＮＴ)   TO 終了月Ｗ(部位ＣＮＴ)
028440                 MOVE 負－終了日(部位ＣＮＴ)   TO 終了日Ｗ(部位ＣＮＴ)
028450             END-IF
028460* 経過略称取得
028470             MOVE 01                         TO 経－区分コード
028480             MOVE 負－経過コード(部位ＣＮＴ) TO 経－経過コード
028490             READ 経過マスタ
028500             INVALID KEY
028510                 MOVE ZERO            TO 部位ＣＮＴＷ(部位ＣＮＴ)
028520                 MOVE SPACE           TO 部位区切Ｗ(部位ＣＮＴ)
028530                 MOVE SPACE           TO 経過略称Ｗ(部位ＣＮＴ)
028540             NOT INVALID KEY
028550                 EVALUATE 部位ＣＮＴ
028560                 WHEN 1
028570                     MOVE NC"①" TO 経過部位Ｗ
028580                 WHEN 2
028590                     MOVE NC"②" TO 経過部位Ｗ
028600                 WHEN 3
028610                     MOVE NC"③" TO 経過部位Ｗ
028620                 WHEN 4
028630                     MOVE NC"④" TO 経過部位Ｗ
028640                 WHEN 5
028650                     MOVE NC"⑤" TO 経過部位Ｗ
028660                 END-EVALUATE
028670                 STRING  経過部位Ｗ     DELIMITED BY SPACE
028680                         経－経過略称   DELIMITED BY SPACE
028690                        INTO 印刷経過略称Ｗ(部位ＣＮＴ)
028700                 END-STRING
028710             END-READ
028720*
028730             MOVE 負－転帰区分(部位ＣＮＴ) TO 転帰区分Ｗ(部位ＣＮＴ)
028740             EVALUATE 負－転帰区分(部位ＣＮＴ)
028750             WHEN 1
028760             WHEN 2
028770                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
028780             WHEN 3
028790                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
028800             WHEN 4
028810                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
028820             END-EVALUATE
028830*
028840*             EVALUATE 負－転帰区分(部位ＣＮＴ)
028850*             WHEN 1
028860*             WHEN 2
028870*                 MOVE NC"治癒"               TO 転帰Ｗ(部位ＣＮＴ)
028880*             WHEN 3
028890*                 MOVE NC"中止"               TO 転帰Ｗ(部位ＣＮＴ)
028900*             WHEN 4
028910*                 MOVE NC"転医"               TO 転帰Ｗ(部位ＣＮＴ)
028920*             WHEN OTHER
028930*                 MOVE NC"継続"               TO 転帰Ｗ(部位ＣＮＴ)
028940*             END-EVALUATE
028950*
      */実日数はレセ－部位実日数を転記する/160823
031230             MOVE レセ－部位実日数(部位ＣＮＴ) TO 実日数Ｗ(部位ＣＮＴ)
028960         END-PERFORM.
028970* 新規/継続 チェック
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
029030* 新規/継続 チェック
029040*         IF レセ－初検料  NOT = ZERO
029050*             MOVE NC"新規"                   TO 請求区分Ｗ
029060*         ELSE
029070*             MOVE NC"継続"                   TO 請求区分Ｗ
029080*         END-IF
029090* 枝番判定用
029100         MOVE 負－開始診療日手動区分 TO  開始診療日手動区分Ｗ.
029110*
029120*     END-READ.
029130*================================================================*
029140*================================================================*
029150 負傷原因取得 SECTION.
029160*
029170********************************************************************
029180*  負傷原因コードが同じものは、1行にまとめて印字する。
029190*  例: ①② 家で転んだ.
029200*     負傷原因コードが同じものをまとめ、テーブルにセット
029210*     (ただし、部位を飛んで同じものは、2行になる)
029220********************************************************************
029230     MOVE  ZERO   TO  カウンタ カウンタ２.
029240     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
029250             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
029260*
029270****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
029280        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
029290*
029300           IF カウンタ = ZERO
029310               MOVE 1   TO  カウンタ カウンタ２
029320               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
029330               MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
029340               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
029350           ELSE
029360              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
029370                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
029380                 COMPUTE カウンタ２ = カウンタ２  +  1
029390                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
029400              ELSE
029410                 COMPUTE カウンタ = カウンタ  +  1
029420                 MOVE 1   TO  カウンタ２
029430                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
029440                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
029450                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
029460              END-IF
029470           END-IF
029480        END-IF
029490     END-PERFORM.
029500**************************************************************************
029510*  負傷原因マスタより文章取得
029520**************************************************************************
029530     MOVE  ZERO   TO  カウンタ カウンタ２.
029540     PERFORM VARYING カウンタ FROM 1 BY 1
029550             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
029560** 健保は 区分 01
029570         MOVE 01                        TO 負原－区分コード
029580         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
029590         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
029600         READ 負傷原因Ｆ
029610         NOT INVALID KEY
029620             INITIALIZE 負傷原因ＷＴ
029630             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
029640             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
029650             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
029660             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
029670             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
029680             PERFORM VARYING カウンタ２ FROM 1 BY 1
029690                     UNTIL ( カウンタ２ > 9 )  OR 
029700                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
029710                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
029720                WHEN 1
029730                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
029740                WHEN 2
029750                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
029760                WHEN 3
029770                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
029780                WHEN 4
029790                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
029800                WHEN 5
029810                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
029780                WHEN 6
029790                   MOVE "⑥"  TO  負傷原因ナンバーＷ１(カウンタ２)
029800                WHEN 7
029810                   MOVE "⑦"  TO  負傷原因ナンバーＷ１(カウンタ２)
029820                WHEN OTHER
029830                   CONTINUE
029840                END-EVALUATE
029850             END-PERFORM
029860*
029942             IF 負原－負傷原因入力区分 = 1
029943                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
029944                        負傷原因１ＷＴ  DELIMITED BY SIZE
029945                        負傷原因２ＷＴ  DELIMITED BY SIZE
029946                        負傷原因３ＷＴ  DELIMITED BY SIZE
029947                        負傷原因４ＷＴ  DELIMITED BY SIZE
029948                        負傷原因５ＷＴ  DELIMITED BY SIZE
029949                        INTO 負傷原因内容合成Ｗ(カウンタ)
029950                 END-STRING
029951             ELSE
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
029960             END-IF
029961*
029962         END-READ
029963     END-PERFORM.
029970*
029980     PERFORM 負傷原因セット.
029990*
030000*================================================================*
030010 負傷原因セット SECTION.
030020*
030030**************************************************************************
030040*  文章が1行を超える時は、複数行に分解する。
030050**************************************************************************
030060     MOVE  ZERO   TO  カウンタ カウンタ２.
030070     PERFORM VARYING カウンタ FROM 1 BY 1
030080             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
030090*
030100          INITIALIZE 負傷原因内容分解ＸＷ
030110          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
030120          IF  負傷原因内容１ＸＷ  NOT = SPACE
030130              COMPUTE カウンタ２ = カウンタ２  +  1
030140              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
030150          END-IF
030160          IF  負傷原因内容２ＸＷ  NOT = SPACE
030170              COMPUTE カウンタ２ = カウンタ２  +  1
030180              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
030190          END-IF
030200          IF  負傷原因内容３ＸＷ  NOT = SPACE
030210              COMPUTE カウンタ２ = カウンタ２  +  1
030220              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
030230          END-IF
034690          IF  負傷原因内容４ＸＷ  NOT = SPACE
034700              COMPUTE カウンタ２ = カウンタ２  +  1
034710              MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
034720          END-IF
030240*
030250     END-PERFORM.
030260*================================================================*
030270*================================================================*
030280 施術記録取得 SECTION.
030290*
030300************************************************************
030310* 作１データから負傷データＦより以下の情報を取得           *
030320* ● 初検加算 .....区分によりチェックに"○"を格納...複数可 *
030330* ● 往療加算 .....区分によりチェックに"○"を格納...複数可 *
030340************************************************************
030350     MOVE  SPACE  TO  初日再検フラグ.
030360     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 部位数Ｗ
030370         IF ( 施術年Ｗ = 初検年Ｗ(部位ＣＮＴ) ) AND
030380            ( 施術月Ｗ = 初検月Ｗ(部位ＣＮＴ) )
030390             MOVE 患者番号ＷＲ          TO 施記－患者番号
030400             MOVE 枝番ＷＲ              TO 施記－枝番
030410             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
030420             MOVE 初検年Ｗ(部位ＣＮＴ)  TO 開始年Ｗ(部位ＣＮＴ) 施記－施術年
030430             MOVE 初検月Ｗ(部位ＣＮＴ)  TO 開始月Ｗ(部位ＣＮＴ) 施記－施術月
030440             MOVE 初検日Ｗ(部位ＣＮＴ)  TO 開始日Ｗ(部位ＣＮＴ) 施記－施術日
030450         ELSE
030460             MOVE 患者番号ＷＲ          TO 施記－患者番号
030470             MOVE 枝番ＷＲ              TO 施記－枝番
030480             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
030490             MOVE 施術年ＷＲ            TO 施記－施術年
030500             MOVE 施術月ＷＲ            TO 施記－施術月
030510             MOVE ZERO                  TO 施記－施術日
030520         END-IF
      *------------------------------------------------------------------------*
               IF ( 連レ－保険種別 > 50 ) AND ( レセ－助成月途中対象 = 1 )
                  IF 開始日Ｗ(部位ＣＮＴ) < 受－助成月途中開始日
                     MOVE 受－助成月途中開始日  TO  開始日Ｗ(部位ＣＮＴ) 施記－施術日
                  END-IF
               END-IF
      *------------------------------------------------------------------------*
030530         START 施術記録Ｆ   KEY IS >= 施記－患者コード
030540                                      施記－施術和暦年月日
030550         END-START
030560         IF 状態キー = "00"
      */実日数はレセ－部位実日数を転記する/160823
030570*             MOVE ZERO  TO 実日数Ｗ(部位ＣＮＴ)
030580             MOVE ZERO  TO 終了年ＷＴ
030590             MOVE ZERO  TO 終了月ＷＴ
030600             MOVE ZERO  TO 終了日ＷＴ
030610             MOVE SPACE TO 終了フラグ２
030620             PERFORM 施術記録Ｆ読込
030630             IF  ( 終了フラグ２      = SPACE   ) AND
030640                 ( 施記－患者コード  = 患者コードＷＲ ) AND
030650                 ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
030660                 ( 施記－施術年      = 施術年ＷＲ     ) AND
030670                 ( 施記－施術月      = 施術月ＷＲ     ) 
030680*
030690*        *****************************************************************
030700*        * 開始年月日 ( その部位が当月初検でないか、
030710*                       当月初検でも枝番がある時は、最初の施術日を開始日)*
030720*        *****************************************************************
030730                 IF ( 施術年Ｗ NOT = 初検年Ｗ(部位ＣＮＴ) ) OR
030740                    ( 施術月Ｗ NOT = 初検月Ｗ(部位ＣＮＴ) ) OR
030750                    ( 開始診療日手動区分Ｗ = 1 )
030760                     MOVE 施記－施術年   TO 開始年Ｗ(部位ＣＮＴ)
030770                     MOVE 施記－施術月   TO 開始月Ｗ(部位ＣＮＴ)
030780                     MOVE 施記－施術日   TO 開始日Ｗ(部位ＣＮＴ)
030790                 END-IF
030800             END-IF
030810             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
030820                           ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
030830                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
030840                           ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
030850                           ( 施記－施術月     NOT = 施術月ＷＲ       ) OR
030860                           ( 施記－施術日         > 終了日Ｗ(部位ＣＮＴ))
030870*               **********
030880*               * 実日数 *
030890*               **********
      */実日数はレセ－部位実日数を転記する/160823
030900*                COMPUTE 実日数Ｗ(部位ＣＮＴ) = 実日数Ｗ(部位ＣＮＴ) + 1
030910                MOVE 施記－施術年               TO 終了年ＷＴ
030920                MOVE 施記－施術月               TO 終了月ＷＴ
030930                MOVE 施記－施術日               TO 終了日ＷＴ
030940*
030950                PERFORM 施術記録Ｆ読込
030960            END-PERFORM
030970        END-IF
030980*       **************************
030990*       * 継続：終了年月日セット *
031000*       **************************
031010        IF 転帰区分Ｗ(部位ＣＮＴ) = 9
031020            MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
031030            MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
031040            MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
031050        END-IF
031060        IF 終了年月日Ｗ(部位ＣＮＴ) > 受理年月日Ｗ
031070            MOVE 終了年Ｗ(部位ＣＮＴ) TO 受理年Ｗ
031080            MOVE 終了月Ｗ(部位ＣＮＴ) TO 受理月Ｗ
031090            MOVE 終了日Ｗ(部位ＣＮＴ) TO 受理日Ｗ
031100        END-IF
031110     END-PERFORM.
031120*
031130** ----- 前月初検のみかを判定 -----------*
031140*
031150*     MOVE 患者番号ＷＲ          TO 施記－患者番号.
031160*     MOVE 枝番ＷＲ              TO 施記－枝番.
031170*     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
031180*     MOVE 施術年ＷＲ            TO 施記－施術年.
031190*     MOVE 施術月ＷＲ            TO 施記－施術月.
031200*     MOVE ZERO                  TO 施記－施術日.
031210*     START 施術記録Ｆ   KEY IS >= 施記－患者コード
031220*                                  施記－施術和暦年月日
031230*     END-START.
031240*     IF 状態キー = "00"
031250*             MOVE SPACE TO 終了フラグ２
031260*             PERFORM 施術記録Ｆ読込
031270*             IF  ( 終了フラグ２      = SPACE   ) AND
031280*                 ( 施記－患者コード  = 患者コードＷＲ ) AND
031290*                 ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
031300*                 ( 施記－施術年      = 施術年ＷＲ     ) AND
031310*                 ( 施記－施術月      = 施術月ＷＲ     ) 
031320** 当月施術開始日が再検かどうか判定
031330*                 IF   施記－再検料請求 = 1
031340*                      MOVE "YES"  TO  初日再検フラグ
031350*                 END-IF
031360**
031370*             END-IF
031380*     END-IF.
031390*     IF 初日再検フラグ = "YES"
031400*        PERFORM 前月初検のみ判定
031410*     END-IF.
031420*
031430*================================================================*
031440*================================================================*
031450 初検日以前のデータ判定 SECTION.
031460*
031470*********************************************************************************
031480*  最初の初検日以前の当月中に施術記録レコードがあった時(治癒、中止)は、請求区分の
031490*  継続にもチェックする。(新規と継続の両方)
031500*********************************************************************************
031510** 最初の初検日を取得
031520     MOVE SPACE                 TO 初検フラグ.
031530     MOVE 患者番号ＷＲ          TO 施記－患者番号.
031540     MOVE 枝番ＷＲ              TO 施記－枝番.
031550     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
031560     MOVE 施術年ＷＲ            TO 施記－施術年.
031570     MOVE 施術月ＷＲ            TO 施記－施術月.
031580     MOVE ZERO                  TO 施記－施術日.
031590     START 施術記録Ｆ   KEY IS >= 施記－患者コード
031600                                  施記－施術和暦年月日
031610     END-START.
031620     IF 状態キー = "00"
031630         MOVE ZERO  TO 初検和暦ＷＴ
031640         MOVE ZERO  TO 初検年ＷＴ
031650         MOVE ZERO  TO 初検月ＷＴ
031660         MOVE ZERO  TO 初検日ＷＴ
031670         MOVE SPACE TO 終了フラグ２
031680         PERFORM 施術記録Ｆ読込
031690         PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
031700                       ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
031710                       ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
031720                       ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
031730                       ( 施記－施術月     NOT = 施術月ＷＲ      ) OR
031740                       ( 初検フラグ           = "YES"           ) 
031750               IF  施記－診療区分 = 2
031760                   MOVE 施記－施術和暦           TO 初検和暦ＷＴ
031770                   MOVE 施記－施術年             TO 初検年ＷＴ
031780                   MOVE 施記－施術月             TO 初検月ＷＴ
031790                   MOVE 施記－施術日             TO 初検日ＷＴ
031800                   MOVE "YES"                    TO 初検フラグ
031810               END-IF
031820               PERFORM 施術記録Ｆ読込
031830         END-PERFORM
031840     END-IF.
031850*
031860* 初検日以前のデータ判定
031870     IF 初検フラグ = "YES"
031880        MOVE 患者番号ＷＲ          TO 施記－患者番号
031890        MOVE 枝番ＷＲ              TO 施記－枝番
031900        MOVE 初検和暦ＷＴ          TO 施記－施術和暦
031910        MOVE 初検年ＷＴ            TO 施記－施術年
031920        MOVE 初検月ＷＴ            TO 施記－施術月
031930        MOVE 初検日ＷＴ            TO 施記－施術日
031940        START 施術記録Ｆ   KEY IS <  施記－患者コード
031950                                     施記－施術和暦年月日
031960                                     REVERSED
031970        END-START
031980        IF 状態キー = "00"
031990           MOVE SPACE  TO 終了フラグ２
032000           PERFORM 施術記録Ｆ読込
032010           IF ( 終了フラグ２    = SPACE        ) AND
032020              ( 施記－患者番号  = 患者番号ＷＲ ) AND
032030              ( 施記－枝番      = 枝番ＷＲ     ) AND
032040              ( 施記－施術和暦  = 初検和暦ＷＴ ) AND
032050              ( 施記－施術年    = 初検年ＷＴ   ) AND
032060              ( 施記－施術月    = 初検月ＷＴ   )
032070*  初検日以前の当月中に施術記録レコードがあった時
032080                IF 継続チェックＷ = SPACE
032090                   MOVE NC"○"    TO 継続チェックＷ
032100                END-IF
032110           END-IF
032120         END-IF
032130     END-IF.
032140*
032150*================================================================*
032160 長期判定取得 SECTION.
032170*
032180* ３カ月以上の長期判定は "CHOUKI" を呼ぶ. 
032190     MOVE  SPACE TO  連期間－キー.
032200     INITIALIZE      連期間－キー.
032210     MOVE 施術和暦ＷＲ  TO  連期間－施術和暦.
032220     MOVE 施術年ＷＲ    TO  連期間－施術年.
032230     MOVE 施術月ＷＲ    TO  連期間－施術月.
032240     MOVE 患者番号ＷＲ  TO  連期間－患者番号.
032250     MOVE 枝番ＷＲ      TO  連期間－枝番.
032260*
032270     CALL   "CHOUKI".
032280     CANCEL "CHOUKI".
032290*
032300**** 適用１を使用 (「前月初検のみ」がある時は、くっつける)
032310     IF 連期間－対象フラグ  = "YES"
032320        IF 適用１Ｗ  = SPACE
032330           MOVE NC"※長期施術継続理由裏面に記載"  TO 適用１Ｗ
032340        ELSE
032350           STRING 適用１Ｗ           DELIMITED BY SPACE
032360                  NC"，"             DELIMITED BY SIZE
032370                  NC"※長期施術継続理由裏面に記載"   DELIMITED BY SIZE
032380                  INTO 適用１Ｗ
032390           END-STRING
032400        END-IF
032410     END-IF.
032420*
032430*================================================================*
032440 初検加算時刻取得 SECTION.
032450*****************************************************************
032460** 初検加算が時間外と深夜の時、適用に「受付時間」を印字する。
032470**   時刻の印字は月3回まで可能
032480*****************************************************************
032490     IF ( レセ－時間外 = 1 ) OR ( レセ－深夜 = 1 ) OR ( レセ－休日 = 1 )
032500*
032510         MOVE 患者番号ＷＲ          TO 施記－患者番号
032520         MOVE 枝番ＷＲ              TO 施記－枝番
032530         MOVE 施術和暦ＷＲ          TO 施記－施術和暦
032540         MOVE 施術年ＷＲ            TO 施記－施術年
032550         MOVE 施術月ＷＲ            TO 施記－施術月
032560         MOVE ZERO                  TO 施記－施術日
032570         START 施術記録Ｆ   KEY IS >= 施記－患者コード
032580                                      施記－施術和暦年月日
032590         END-START
032600         IF 状態キー = "00"
032610             MOVE ZERO  TO 初検加算カウント
032620             MOVE SPACE TO 終了フラグ２
032630             PERFORM 施術記録Ｆ読込
032640             PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
032650                           ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
032660                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
032670                           ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
032680                           ( 施記－施術月     NOT = 施術月ＷＲ      ) 
032690                   IF  ( 施記－初検加算 = 1 OR 2 OR 3 ) AND ( 施記－診療区分 = 2 )
032700                       COMPUTE 初検加算カウント = 初検加算カウント  + 1
032710                       IF  初検加算カウント <= 3
032720                           MOVE 施記－初検加算 TO 初検加算区分ＷＴ(初検加算カウント)
032730                           MOVE 施記－受付時   TO 初検加算時ＷＴ(初検加算カウント)
032740                           MOVE 施記－受付分   TO 初検加算分ＷＴ(初検加算カウント)
032750                       END-IF
032760                   END-IF
032770                   PERFORM 施術記録Ｆ読込
032780             END-PERFORM
032790** 初検加算の時刻を適用にセット
033380             IF ( 初検加算時ＷＴ(1) NOT = ZERO ) OR ( 初検加算分ＷＴ(1) NOT = ZERO ) 
                       MOVE 初検加算時ＷＴ(1) TO 初検加算時Ｗ
                       MOVE ":"               TO 初検加算区切Ｗ
                       MOVE 初検加算分ＷＴ(1) TO 初検加算分Ｗ
                   END-IF
033380             IF ( 初検加算時ＷＴ(2) NOT = ZERO ) OR ( 初検加算分ＷＴ(2) NOT = ZERO ) 
032800                 PERFORM 初検加算適用セット
                   END-IF
032810         END-IF
032820*
032830     END-IF.
032840*
032850*================================================================*
032860 初検加算適用セット SECTION.
032870*
032880     PERFORM VARYING 番号カウンタ FROM 1 BY 1
032890              UNTIL  番号カウンタ > 3
032900         IF ( 初検加算時ＷＴ(番号カウンタ)  = ZERO )  AND 
032910            ( 初検加算分ＷＴ(番号カウンタ)  = ZERO ) 
032920             CONTINUE
032930         ELSE
032940* 固定項目
032950             EVALUATE 初検加算区分ＷＴ(番号カウンタ) 
032960             WHEN 1
032970                MOVE NC"時間外"   TO 加算内容Ｗ(番号カウンタ)
033320             WHEN 2
033330                MOVE NC"休　日"   TO 加算内容Ｗ(番号カウンタ)
032980             WHEN 3
032990                MOVE NC"深　夜"   TO 加算内容Ｗ(番号カウンタ)
033000             END-EVALUATE
033010*
033020             MOVE NC"："          TO 加算区切Ｗ(番号カウンタ)
033030             MOVE NC"時"          TO 時固定Ｗ(番号カウンタ)
033040             MOVE NC"分"          TO 分固定Ｗ(番号カウンタ)
033050*
033060**** 数字→日本語変換
033070* 時間
033080             MOVE 初検加算時ＷＴ(番号カウンタ)  TO  数字Ｗ
033090             IF 数字Ｗ >= 10
033100                 MOVE 数字Ｗ１    TO 負傷番号Ｗ１
033110                 PERFORM 日本語変換
033120                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ１(番号カウンタ)
033130                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
033140                 PERFORM 日本語変換
033150                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
033160             ELSE
033170                 MOVE 数字Ｗ２    TO 負傷番号Ｗ１
033180                 PERFORM 日本語変換
033190                 MOVE 全角負傷番号Ｗ  TO 初検加算時ＮＷ２(番号カウンタ)
033200             END-IF
033210* 分
033220             MOVE 初検加算分ＷＴ(番号カウンタ)  TO  数字Ｗ
033230             MOVE 数字Ｗ１    TO 負傷番号Ｗ１
033240             PERFORM 日本語変換
033250             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ１(番号カウンタ)
033260             MOVE 数字Ｗ２    TO 負傷番号Ｗ１
033270             PERFORM 日本語変換
033280             MOVE 全角負傷番号Ｗ  TO 初検加算分ＮＷ２(番号カウンタ)
033290** 
033300        END-IF
033310     END-PERFORM.
033320*
033330     MOVE  初検加算集団ＮＷ(1)   TO 初検加算時刻１Ｗ. 
033340     MOVE  初検加算集団ＮＷ(2)   TO 初検加算時刻２Ｗ. 
033350     MOVE  初検加算集団ＮＷ(3)   TO 初検加算時刻３Ｗ. 
033360*
033370**** 適用１か２を使用（長期理由記載で適用１を使っている時は、適用２）
033380     IF ( 初検加算時ＷＴ(2)  = ZERO ) AND ( 初検加算分ＷＴ(2)  = ZERO ) 
033390         CONTINUE
033400     ELSE
033410         IF 適用１Ｗ  = SPACE
033420               STRING NC"初検加算"       DELIMITED BY SIZE
033430                      初検加算時刻１Ｗ   DELIMITED BY SIZE
033440                      初検加算時刻２Ｗ   DELIMITED BY SIZE
033450                      初検加算時刻３Ｗ   DELIMITED BY SIZE
033460                      INTO 適用１Ｗ
033470               END-STRING
033480         ELSE
033490               STRING NC"初検加算"       DELIMITED BY SIZE
033500                      初検加算時刻１Ｗ   DELIMITED BY SIZE
033510                      初検加算時刻２Ｗ   DELIMITED BY SIZE
033520                      初検加算時刻３Ｗ   DELIMITED BY SIZE
033530                      INTO 適用２Ｗ
033540               END-STRING
033550         END-IF
033560     END-IF.
033570*
033580*================================================================*
033590 日本語変換 SECTION.
033600*
033610     MOVE NC"０"     TO 全角負傷番号Ｗ.
033620     CALL "htoz" WITH C LINKAGE
033630                        USING 負傷番号Ｗ１ 全角負傷番号Ｗ１.
033640*
033650*================================================================*
033660 施術記録Ｆ読込 SECTION.
033670*
033680     READ 施術記録Ｆ NEXT
033690     AT END
033700         MOVE "YES" TO 終了フラグ２
033710     END-READ.
033720*================================================================*
033730*================================================================*
033740 前月初検のみ判定 SECTION.
033750*
033760*** 前月の通院日が初検か判定 
033770     MOVE  SPACE            TO 前月フラグ.
033780     MOVE 受－患者コード    TO 施記－患者コード.
033790     MOVE 受－施術和暦      TO 施記－施術和暦.
033800     MOVE 受－施術年        TO 施記－施術年.
033810     MOVE 受－施術月        TO 施記－施術月.
033820     MOVE 1                 TO 施記－施術日.
033830     START 施術記録Ｆ   KEY IS <  施記－患者コード
033840                                  施記－施術和暦年月日
033850                                  REVERSED
033860     END-START.
033870     IF 状態キー = "00"
033880         MOVE SPACE  TO 終了フラグ２
033890         PERFORM 施術記録Ｆ読込
033900         IF ( 終了フラグ２      = SPACE  ) AND
033910            ( 施記－患者コード  = 受－患者コード ) AND
033920            ( 施記－診療区分    = 2 ) 
033930*
033940            PERFORM 前月判定
033950**** 適用１を使用
033960            IF 前月フラグ = "YES"
033970               MOVE NC"※前月初検のみ"    TO  適用１Ｗ
033980            END-IF
033990**
034000         END-IF
034010     END-IF.
034020*
034030*================================================================*
034040 前月判定  SECTION.
034050* 
034060*** 読み込んだ施術記録の年月が、前月かどうか判定 (年月の差が 1 か?)
034070      MOVE  SPACE  TO  前月フラグ.
034080      INITIALIZE  計算年月日Ｗ 開始年月日２Ｗ 終了年月日２Ｗ.
034090**
034100      MOVE 受－施術和暦    TO 終了和暦２Ｗ.
034110      MOVE 受－施術年      TO 終了年２Ｗ.
034120      MOVE 受－施術月      TO 終了月２Ｗ.
034130      MOVE 施記－施術和暦  TO 開始和暦２Ｗ.
034140      MOVE 施記－施術年    TO 開始年２Ｗ.
034150      MOVE 施記－施術月    TO 開始月２Ｗ.
034160*
034170      EVALUATE TRUE
034180       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ = 終了年２Ｗ)
034190            PERFORM  前月比較月
034200       WHEN (開始和暦２Ｗ = 終了和暦２Ｗ) AND (開始年２Ｗ NOT = 終了年２Ｗ)
034210            PERFORM  前月比較年
034220       WHEN  開始和暦２Ｗ NOT = 終了和暦２Ｗ 
034230            PERFORM  前月比較元号
034240      END-EVALUATE.
034250*
034260      IF 計算月Ｗ = 1
034270         MOVE  "YES"  TO  前月フラグ
034280      END-IF.
034290*
034300*================================================================*
034310 前月比較月  SECTION.
034320*
034330     IF  終了月２Ｗ >  開始月２Ｗ
034340         COMPUTE 計算月Ｗ = 終了月２Ｗ - 開始月２Ｗ
034350     ELSE
034360        MOVE ZERO TO 計算月Ｗ
034370     END-IF.
034380*
034390*================================================================*
034400 前月比較年  SECTION.
034410*
034420     IF  終了年２Ｗ >  開始年２Ｗ
034430         COMPUTE 計算年Ｗ = 終了年２Ｗ - 開始年２Ｗ
034440         COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
034450     ELSE
034460        MOVE ZERO TO 計算月Ｗ
034470     END-IF.
034480*
034490*================================================================*
034500 前月比較元号  SECTION.
034510*
034520     MOVE 開始和暦２Ｗ TO 元－元号区分.
034530     READ 元号マスタ
034540     NOT INVALID KEY
034550         MOVE 元－開始西暦年 TO 開始西暦年Ｗ
034560     END-READ.
034570     MOVE 終了和暦２Ｗ TO 元－元号区分.
034580     READ 元号マスタ
034590     NOT INVALID KEY
034600         MOVE 元－開始西暦年 TO 終了西暦年Ｗ
034610     END-READ.
034620**
034630     IF (開始西暦年Ｗ NOT = ZERO) AND (終了西暦年Ｗ NOT = ZERO)
034640        COMPUTE 開始西暦年Ｗ = 開始西暦年Ｗ + 開始年２Ｗ - 1
034650        COMPUTE 終了西暦年Ｗ = 終了西暦年Ｗ + 終了年２Ｗ - 1
034660*
034670        IF 終了西暦年Ｗ =  開始西暦年Ｗ
034680           PERFORM  前月比較月
034690        ELSE
034700           IF  終了西暦年Ｗ >  開始西暦年Ｗ
034710               COMPUTE 計算年Ｗ = 終了西暦年Ｗ - 開始西暦年Ｗ
034720               COMPUTE 計算月Ｗ = (計算年Ｗ * 12 + 終了月２Ｗ) - 開始月２Ｗ
034730           ELSE
034740               MOVE ZERO TO 計算月Ｗ
034750           END-IF
034760        END-IF
034770     ELSE
034780        MOVE ZERO TO 計算月Ｗ
034790     END-IF.
034800*
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
034980*================================================================*
034990 印刷処理 SECTION.
035000*
035010     MOVE "YJB6125P" TO  定義体名Ｐ.
035020     MOVE "SCREEN"   TO  項目群名Ｐ.
035030     WRITE YJB6125P.
035040****     WRITE 印刷レコード.
035050     PERFORM エラー処理Ｐ.
035060*================================================================*
035070 エラー処理Ｐ SECTION.
035080*
035090     IF 通知情報Ｐ NOT = "00"
035100         DISPLAY NC"帳票エラー"              UPON CONS
035110         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
035120         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
035130         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
035140         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
035150                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
035160         ACCEPT  キー入力 FROM CONS
035170         PERFORM ファイル閉鎖
035180         MOVE 99  TO PROGRAM-STATUS
035190         EXIT PROGRAM
035200     END-IF.
035210*================================================================*
035220 部位名称埋込処理 SECTION.
035230*
006490     STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
009980            負傷名称Ｗ                    DELIMITED BY SPACE
006500            レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
006520       INTO 負傷名Ｗ(部位ＣＮＴ)
006570     END-STRING.
035370*
035380*================================================================*
035390 ファイル閉鎖 SECTION.
035400*
035410     CLOSE 印刷ファイル.
035420     CLOSE 保険者マスタ     元号マスタ          名称マスタ
035430           レセプトＦ       制御情報マスタ      施術所情報マスタ
035440           経過マスタ       受診者情報Ｆ        メモファイル
035450           施術記録Ｆ       負傷データＦ        負傷原因Ｆ
                 市町村マスタ     受診者情報２Ｆ      ＩＤ管理マスタ.
035460*
035470*================================================================*
035480 終了処理 SECTION.
035490*
035500     PERFORM ファイル閉鎖.
035510*================================================================*
035520 委任年月日取得 SECTION.
035530*
035540** ---// ここの受理年には、最終通院日が入っている為、退避する //----
035550     MOVE 受理年Ｗ   TO 最終通院年Ｗ.
035560     MOVE 受理月Ｗ   TO 最終通院月Ｗ.
035570     MOVE 受理日Ｗ   TO 最終通院日Ｗ.
035580***
035590* (柔整師側)
035600     EVALUATE レセプト日付区分Ｗ 
035610*    /  最終通院日 /
035620     WHEN ZERO
035630         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
035640         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
035650         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
035660*    /  月末日 /
035670     WHEN 1 
035680         PERFORM 月末日取得
035690         MOVE 受理年Ｗ     TO 柔整師年Ｗ
035700         MOVE 受理月Ｗ     TO 柔整師月Ｗ
035710         MOVE 受理日Ｗ     TO 柔整師日Ｗ
035720*    /  印字なし /
035730     WHEN 9
035740         MOVE ZERO         TO 柔整師年Ｗ
035750         MOVE ZERO         TO 柔整師月Ｗ
035760         MOVE ZERO         TO 柔整師日Ｗ
035770*    /  その他は、最終通院日 /
035780     WHEN OTHER
035790         MOVE 最終通院年Ｗ TO 柔整師年Ｗ
035800         MOVE 最終通院月Ｗ TO 柔整師月Ｗ
035810         MOVE 最終通院日Ｗ TO 柔整師日Ｗ
035820     END-EVALUATE.
035830**
035840* (患者側)
035850     EVALUATE レセプト患者日付区分Ｗ 
035860*    /  最終通院日 /
035870     WHEN ZERO
035880         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
035890         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
035900         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
035910*    /  月末日 /
035920     WHEN 1 
035930         PERFORM 月末日取得
035940         MOVE 受理年Ｗ     TO 患者委任年Ｗ
035950         MOVE 受理月Ｗ     TO 患者委任月Ｗ
035960         MOVE 受理日Ｗ     TO 患者委任日Ｗ
035970*    /  印字なし /
035980     WHEN 9
035990         MOVE ZERO         TO 患者委任年Ｗ
036000         MOVE ZERO         TO 患者委任月Ｗ
036010         MOVE ZERO         TO 患者委任日Ｗ
036020*    /  その他は、最終通院日 /
036030     WHEN OTHER
036040         MOVE 最終通院年Ｗ TO 患者委任年Ｗ
036050         MOVE 最終通院月Ｗ TO 患者委任月Ｗ
036060         MOVE 最終通院日Ｗ TO 患者委任日Ｗ
036070     END-EVALUATE.
036080*
036090*================================================================*
036100*================================================================*
036110 月末日取得 SECTION.
036120*
036130     MOVE 施術年ＷＲ   TO 受理年Ｗ.
036140     MOVE 施術月ＷＲ   TO 受理月Ｗ.
036150     MOVE 施術和暦ＷＲ TO 元－元号区分.
036160     READ 元号マスタ
036170     NOT INVALID KEY
036180         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
036190     END-READ.
036200     IF 施術西暦年Ｗ NOT = ZERO
036210        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
036220     END-IF.
036230*
036240     EVALUATE 施術月ＷＲ
036250     WHEN 4
036260     WHEN 6
036270     WHEN 9
036280     WHEN 11
036290         MOVE 30 TO 受理日Ｗ
036300     WHEN 2
036310         DIVIDE 4 INTO 施術西暦年Ｗ GIVING    商Ｗ
036320                                    REMAINDER 余Ｗ
036330         END-DIVIDE
036340         IF 余Ｗ = ZERO
036350             MOVE 29 TO 受理日Ｗ
036360         ELSE
036370             MOVE 28 TO 受理日Ｗ
036380         END-IF
036390     WHEN 1
036400     WHEN 3
036410     WHEN 5
036420     WHEN 7
036430     WHEN 8
036440     WHEN 10
036450     WHEN 12
036460         MOVE 31 TO 受理日Ｗ
036470     WHEN OTHER
036480          CONTINUE
036490     END-EVALUATE.
036500*
036510*================================================================*
036520 受診者印刷区分更新 SECTION.
036530*
036540** //  受診者情報Ｆの印刷区分に１をセットし、更新する。//  
036550*
036560     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
036570     MOVE 施術年ＷＲ         TO 受－施術年.
036580     MOVE 施術月ＷＲ         TO 受－施術月.
036590     MOVE 患者コードＷＲ     TO 受－患者コード.
036600     READ 受診者情報Ｆ
036610     NOT INVALID KEY
               IF 連レ－保険種別 > 50
036620             MOVE  1  TO  受－レセ印刷区分助成
               ELSE
036620             MOVE  1  TO  受－レセ印刷区分
               END-IF
036630         REWRITE  受－レコード
036640         END-REWRITE
036650         IF 状態キー NOT = "00"
036660            MOVE NC"受診者" TO ファイル名
036670            PERFORM エラー表示
036680         END-IF
036690     END-READ.
036700*
036710*================================================================*
036720 助成印取得 SECTION.
036730*
036740* 2006/04 変更
036750* 助成印は "JOSEIMEI" を呼ぶ. 
036760     MOVE SPACE TO  連助成名称－キー 連助成名称－会キー.
036770     INITIALIZE     連助成名称－キー 連助成名称－会キー.
036780     MOVE 助成種別ＷＲ           TO 連助成名称－助成種別.
036790     MOVE 費用負担者番号助成ＷＲ TO 連助成名称－費用負担者番号助成.
      */被爆の印変更の為会コードを転記/20191211
           MOVE 8 TO 連助成名称－協会コード.
036800*
036810     CALL   "JOSEIMEI".
036820     CANCEL "JOSEIMEI".
036830*
036840     MOVE 連助成名称－１文字 TO 助成印Ｗ.
      *
           IF (受－助成種別 = 55 OR 60) AND (費用負担者番号助成ＷＲ(1:4) = "8111")
              MOVE NC"子"          TO 助成印Ｗ
           END-IF.
036850*
037050*================================================================*
037060 負担率取得 SECTION.
037070*
037080* ※ 退職時の本人負担割合
037090*
           MOVE 受－施術和暦 TO 連率－施術和暦.
           MOVE 受－施術年   TO 連率－施術年.
           MOVE 受－施術月   TO 連率－施術月.
           MOVE 受－患者番号 TO 連率－患者番号.
           MOVE 受－枝番     TO 連率－枝番.
           CALL   "HUTANRIT".
           CANCEL "HUTANRIT".
           COMPUTE 負担割合数字Ｗ = 連率－実際本体負担率 / 10.
037110     MOVE  負担割合数字Ｗ  TO  負担割合文字Ｗ.
037120*
037121*================================================================*
037122 助成レセまとめ判定 SECTION.
037123*---------------------------------------------------------------------------*
037124* 市町村マスタを読み、レセまとめ区分＝１でかつ、本体保険が国保・退職
037125* の時は、フラグYES (金額を助成込みで印字）
037126*（例：横浜市の障害は、本体保険（国保系）のレセプト１枚で請求、助成レセはなし）
037127*---------------------------------------------------------------------------*
037128*
037129     MOVE SPACE TO 助成レセまとめフラグ.
037131*
037132*     MOVE 受－助成種別           TO 市－公費種別.
037133*     MOVE 受－費用負担者番号助成 TO 市－市町村番号.
037134*     READ 市町村マスタ
037135*     NOT INVALID KEY
037136*         IF 市－レセまとめ区分 = 1 
037137*            IF (( 受－保険種別 = 01 ) AND ( 受－保険者番号(3:1) NOT = "3" )) OR
037138*               ( 受－保険種別 = 08 ) 
037139*                MOVE "YES" TO 助成レセまとめフラグ
037147*            END-IF
037148*         END-IF
037149*     END-READ.
037150*
037151** / CALL JRECEOFF /
037152*     IF 助成レセまとめフラグ = SPACE
037153*        INITIALIZE 連レセまとめ－キー
037154*        MOVE 施術和暦ＷＲ TO 連レセまとめ－施術和暦
037155*        MOVE 施術年ＷＲ   TO 連レセまとめ－施術年
037156*        MOVE 施術月ＷＲ   TO 連レセまとめ－施術月
037157*        MOVE 患者番号ＷＲ TO 連レセまとめ－患者番号
037158*        MOVE 枝番ＷＲ     TO 連レセまとめ－枝番
037159*       1:助成レセプトなしの本体まとめの判定
037160*        MOVE 1            TO 連レセまとめ－判定区分
037161*        CALL   "JRECEOFF"
037162*        CANCEL "JRECEOFF"
037163*
037164*        IF 連レセまとめ－判定結果 = 1
           IF ( レセ－本体まとめ区分 = 1 )
037165           MOVE "YES" TO 助成レセまとめフラグ
037166*        END-IF
037167     END-IF.
037168*
037169*----------------------------------------------------------------------*
037170** / 神奈川県固有：摘要に負担者番号と受給者番号 /
037171     IF ( 助成レセまとめフラグ = "YES" ) AND
037172        ( 受－費用負担者番号助成(3:2) = "14" )
037173        IF 受－費用負担者番号助成(1:2) NOT = "99"
037174*            MOVE ALL NC"￣"    TO 横線１ 横線２
037175*            MOVE ALL NC"｜"    TO 縦線１ 縦線２
037176*            MOVE NC"｜"        TO 縦線３
037177*            MOVE NC"公費負担者番号"        TO 神奈川固定１
037178*            MOVE NC"受給者番号"            TO 神奈川固定２
037180*            MOVE 受－費用負担者番号助成    TO 神奈川負担者番号 
037181*            MOVE 受－受益者番号助成        TO 神奈川受給者番号
037180            MOVE 受－費用負担者番号助成    TO 公費負担者番号
037181            MOVE 受－受益者番号助成        TO 受給者番号Ｗ
      */受給者番号が８文字以上の場合枠を無視して印刷する/110425
                  IF 印刷受給者番号２Ｗ = SPACE
                      MOVE 印刷受給者番号Ｗ TO 受給者番号
                  ELSE
                      MOVE 受給者番号Ｗ     TO 受給者番号２
                  END-IF
037182        END-IF
037183     END-IF.
037184*
037185*================================================================*
037186 助成料金計算 SECTION.
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
037210*
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
017240*================================================================*
017250 レセ用紙情報取得 SECTION.
017260*
017270     MOVE ZERO TO 制－制御区分.
017280     READ 制御情報マスタ
017290     NOT INVALID KEY
017300         MOVE 制－国保レセ      TO 一般レセＷ
017420     END-READ.
017430*
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
037330*================================================================*
037340*================================================================*
037350 エラー表示 SECTION.
037360*
037370     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
037380     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
037390     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
037400     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
037410                                                   UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
037420     ACCEPT  キー入力 FROM CONS
037430     PERFORM ファイル閉鎖.
037440     EXIT PROGRAM.
037450*
037460*================================================================*
037470*================================================================*
037480 テスト印字処理 SECTION.
037490*
           MOVE ALL "9" TO
           後療単価１ 後療回数１ 後療料１ 冷罨法回数１ 冷罨法料１ 温罨法回数１ 温罨法料１ 
           電療回数１ 電療料１ 小計１ 長期逓減率１ 長期込小計１ 後療単価２ 後療回数２ 後療料２ 
           冷罨法回数２ 冷罨法料２ 温罨法回数２ 温罨法料２ 電療回数２ 電療料２ 小計２ 長期逓減率２            長期込小計２ 後療単価３８ 後療回数３８ 後療料３８ 冷罨法回数３８ 冷罨法料３８ 
           温罨法回数３８ 温罨法料３８ 電療回数３８ 電療料３８ 小計３８ 多部位込小計３８ 
           長期逓減率３８ 長期込小計３８ 逓減開始月３０ 逓減開始日３０ 後療単価３０ 後療回数３０ 
           後療料３０ 冷罨法回数３０ 冷罨法料３０ 温罨法回数３０ 温罨法料３０ 電療回数３０ 
           電療料３０ 小計３０ 長期逓減率３０ 長期込小計３０ 逓減開始月４８ 逓減開始日４８ 
           後療単価４８ 後療回数４８ 後療料４８ 冷罨法回数４８ 冷罨法料４８ 温罨法回数４８ 
           温罨法料４８ 電療回数４８ 電療料４８ 小計４８ 多部位込小計４８ 長期逓減率４８ 
           長期込小計４８ 逓減開始月４０ 逓減開始日４０ 後療単価４０ 後療回数４０ 後療料４０ 
           冷罨法回数４０ 冷罨法料４０ 温罨法回数４０ 温罨法料４０ 電療回数４０ 電療料４０ 
           小計４０ 長期逓減率４０ 長期込小計４０ 合計  一部負担金 負担割合 請求金額 
           開始年１ 開始月１ 開始日１ 終了年１ 終了月１ 終了日１ 負傷年１ 負傷月１ 負傷日１ 
           初検年１ 初検月１ 初検日１ 実日数１ 開始年２ 開始月２ 開始日２ 終了年２ 終了月２ 
           終了日２ 負傷年２ 負傷月２ 負傷日２ 初検年２ 初検月２ 初検日２ 実日数２ 開始年３ 
           開始月３ 開始日３ 終了年３ 終了月３ 終了日３ 負傷年３ 負傷月３ 負傷日３ 初検年３ 
           初検月３ 初検日３ 実日数３ 開始年４ 開始月４ 開始日４ 終了年４ 終了月４ 終了日４ 
           負傷年４ 負傷月４ 負傷日４ 初検年４ 初検月４ 初検日４ 実日数４ 開始年５ 開始月５ 
           開始日５ 終了年５ 終了月５ 終了日５ 負傷年５ 負傷月５ 負傷日５ 初検年５ 初検月５ 
           初検日５ 実日数５ 患者年 患者月 患者日
           初検料 初検時相談料 往療距離 再検料 金属副子加算料 往療回数 往療料 小計  初検加算料 
           施術情報提供料 往療加算料 初検加算時 初検加算分 初検加算区切 初回処置料合計
           初回処置料(1) 初回処置料(2) 初回処置料(3) 初回処置料(4) 初回処置料(5) 
           MOVE NC"○" TO
           深夜チェック 時間外チェック 休日チェック 
           夜間チェック 暴風雨雪チェック 難路チェック 
           大チェック 中チェック 小チェック 
           固定料チェック 整復料チェック 施療料チェック
           単独チェック 本人チェック 高一チェック 共済チェック 自チェック 社保チェック 
           組合チェック １０割チェック ９割チェック ２併チェック ６歳チェック ８割チェック 
           ７割チェック 後期チェック 退職チェック 国保チェック 家族チェック 高７チェック 
           男チェック 明治チェック 大正チェック 女チェック 昭和チェック 平成チェック
           治癒チェック１ 中止チェック１ 転医チェック１ 治癒チェック２ 中止チェック２
           転医チェック２ 治癒チェック３ 中止チェック３ 転医チェック３ 治癒チェック４
           中止チェック４ 転医チェック４ 治癒チェック５ 中止チェック５ 転医チェック５
           .
           MOVE ALL "X" TO
           初検加算ＣＭ 住所１ 住所２ 公費負担者番号 受給者番号 保険者名称１ 保険者名称２ 
           県施術ＩＤ 記号番号 柔整師番号２ 接骨師会会員番号 柔整師番号 保険者番号
           負傷原因１ 負傷原因２ 負傷原因３ 負傷原因４ 負傷原因５ 負傷原因６ 
           被保険者氏名 患者氏名 長期理由文１ 長期理由文２ 長期理由文３ 
           長期理由文４ 長期理由文５ 長期理由文６ 長期理由文７ 長期理由文８
           .
           MOVE ALL NC"Ｎ" TO
           適用１ 適用２ 部位５適用 負傷名１ 負傷名２ 負傷名３ 負傷名４ 負傷名５
           .
038250*
038260*================================================================*
       摘要施術日セット SECTION.
      *
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
                   MOVE NC"○" TO 施術日チェック(施記－施術日)
                   PERFORM 施術記録Ｆ読込
               END-PERFORM
           END-IF.
           PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 31
               MOVE カウンタ TO 施術日(カウンタ)
           END-PERFORM.
037520*================================================================*
023880 共済番号セット SECTION.
023890*
023940     MOVE SPACE  TO  脱出フラグ.
           EVALUATE TRUE
           WHEN 保険者番号ＷＲ(1:2) = "31"
           WHEN 保険者番号ＷＲ = "34130021"
      */共済組合連盟
               PERFORM 共済番号転記
           WHEN 保険者番号ＷＲ(1:2) = "32" OR "33" OR "34"
               IF 施情－地共済連番号 NOT = ZERO
      */地共済協議会
                   PERFORM 地共済番号転記
               ELSE
      */共済組合連盟
                   PERFORM 共済番号転記
               END-IF
           END-EVALUATE.
024890*
024900*================================================================*
       共済番号転記 SECTION.
023990*
024000     MOVE  NC"共済組合連盟第"   TO 共済連番号名ＮＷ .
024010     MOVE  NC"号"               TO 共済連番号単位ＮＷ. 
024020     MOVE  施情－共済連番号     TO 共済連番号Ｗ.
024030     IF    (共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
024040           MOVE SPACE TO  共済連番号Ｗ(1:1)
024050     ELSE
024060           MOVE "YES" TO  脱出フラグ
024070     END-IF.
024080     IF    (共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
024090           MOVE SPACE TO  共済連番号Ｗ(2:1)
024100     ELSE
024110           MOVE "YES" TO  脱出フラグ
024120     END-IF.
024130     IF    (共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
024140           MOVE SPACE TO  共済連番号Ｗ(3:1)
024150     ELSE
024160           MOVE "YES" TO  脱出フラグ
024170     END-IF.
024180     IF    (共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
024190           MOVE SPACE TO  共済連番号Ｗ(4:1)
024200     ELSE
024210           MOVE "YES" TO  脱出フラグ
024220     END-IF.
024230     IF    (共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
024240           MOVE SPACE TO  共済連番号Ｗ(5:1)
024250     ELSE
024260           MOVE "YES" TO  脱出フラグ
024270     END-IF.
024280     IF    (共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
024290           MOVE SPACE TO  共済連番号Ｗ(6:1)
024300     ELSE
024310           MOVE "YES" TO  脱出フラグ
024320     END-IF.
024330     MOVE  共済連番号集団Ｗ     TO 柔整師番号２Ｗ.
024900*================================================================*
       地共済番号転記 SECTION.
024430*
024440     MOVE  NC"地共済協議会第"   TO 共済連番号名ＮＷ .
024450     MOVE  NC"号"               TO 共済連番号単位ＮＷ .
024460     MOVE  施情－地共済連番号   TO 共済連番号Ｗ.
024470     IF    (共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
024480           MOVE SPACE TO  共済連番号Ｗ(1:1)
024490     ELSE
024500           MOVE "YES" TO  脱出フラグ
024510     END-IF.
024520     IF    (共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
024530           MOVE SPACE TO  共済連番号Ｗ(2:1)
024540     ELSE
024550           MOVE "YES" TO  脱出フラグ
024560     END-IF.
024570     IF    (共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
024580           MOVE SPACE TO  共済連番号Ｗ(3:1)
024590     ELSE
024600           MOVE "YES" TO  脱出フラグ
024610     END-IF.
024620     IF    (共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
024630           MOVE SPACE TO  共済連番号Ｗ(4:1)
024640     ELSE
024650           MOVE "YES" TO  脱出フラグ
024660     END-IF.
024670     IF    (共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
024680           MOVE SPACE TO  共済連番号Ｗ(5:1)
024690     ELSE
024700           MOVE "YES" TO  脱出フラグ
024710     END-IF.
024720     IF    (共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
024730           MOVE SPACE TO  共済連番号Ｗ(6:1)
024740     ELSE
024750           MOVE "YES" TO  脱出フラグ
024760     END-IF.
024770     MOVE  共済連番号集団Ｗ     TO 柔整師番号２Ｗ.
040820*================================================================*
040830 経過取得 SECTION.
040840*
036040*     MOVE  SPACE TO  連期間－キー.
036050*     INITIALIZE      連期間－キー.
036060*     MOVE 施術和暦ＷＲ  TO  連期間－施術和暦.
036070*     MOVE 施術年ＷＲ    TO  連期間－施術年.
036080*     MOVE 施術月ＷＲ    TO  連期間－施術月.
036090*     MOVE 患者番号ＷＲ  TO  連期間－患者番号.
036100*     MOVE 枝番ＷＲ      TO  連期間－枝番.
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
      *             IF (負－負傷種別(部位ＣＮＴ) = 01 OR 02 OR 03)
040850*              IF ( 負－転帰区分(部位ＣＮＴ) NOT = 1 AND 2)
040900*                 IF ( 連期間－期間Ｗ(部位ＣＮＴ)  >= 3 )
040910*                    MOVE NC"やや良好" TO  経過ＣＭ
040920*                 ELSE
040930*                    MOVE NC"良好"     TO  経過ＣＭ
040940*                 END-IF
      *             ELSE
040930                  MOVE NC"良好"     TO  経過ＣＭ
040950*              END-IF
      *             END-IF
                   MOVE SPACE      TO  印刷経過略称Ｗ(部位ＣＮＴ)
028970             STRING  経過部位Ｗ     DELIMITED BY SPACE
028980                     経過ＣＭ       DELIMITED BY SPACE
028990                INTO 印刷経過略称Ｗ(部位ＣＮＴ)
029000             END-STRING
               END-IF
           END-PERFORM.
040960*
037520*================================================================*
038270******************************************************************
038280 END PROGRAM YJB6125.
038290******************************************************************
