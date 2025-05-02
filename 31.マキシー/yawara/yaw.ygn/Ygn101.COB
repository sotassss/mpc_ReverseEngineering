000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YGN101.
000060 AUTHOR.                 池田　幸子
      *
000070*
000080*----------------------------------------------------------------*
000090*      提出FPD作成 群馬 【ﾃﾞｰﾀ作成】柔＋ｳｨﾝﾄﾞｳｽﾞ版
000140*----------------------------------------------------------------*
000150 DATE-WRITTEN.           2025-01-09
000160 DATE-COMPILED.          2025-01-09
000170*----------------------------------------------------------------*
000180******************************************************************
000190*            ENVIRONMENT         DIVISION                        *
000200******************************************************************
000210 ENVIRONMENT             DIVISION.
000220 CONFIGURATION           SECTION.
000230 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000240 OBJECT-COMPUTER.        FMV-DESKPOWER.
000250 SPECIAL-NAMES.          CONSOLE  IS  CONS
000260                         SYSERR   IS  MSGBOX.
000270 INPUT-OUTPUT            SECTION.
000280 FILE-CONTROL.
000281     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000282                             ORGANIZATION             IS  INDEXED
000283                             ACCESS MODE              IS  DYNAMIC
000284                             RECORD KEY               IS  制－制御区分
000285                             FILE STATUS              IS  状態キー
000286                             LOCK        MODE         IS  AUTOMATIC.
000290     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000300                             ORGANIZATION             IS  INDEXED
000310                             ACCESS MODE              IS  DYNAMIC
000320                             RECORD KEY               IS  元－元号区分
000330                             FILE STATUS              IS  状態キー
000340                             LOCK        MODE         IS  AUTOMATIC.
000350     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000360                             ORGANIZATION             IS  INDEXED
000370                             ACCESS MODE              IS  DYNAMIC
000380                             RECORD KEY               IS  名－区分コード
000390                                                          名－名称コード
000400                             FILE STATUS              IS  状態キー
000410                             LOCK        MODE         IS  AUTOMATIC.
000420     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000430                             ORGANIZATION             IS  INDEXED
000440                             ACCESS MODE              IS  DYNAMIC
000450                             RECORD KEY               IS 施情－施術所番号
000460                             FILE STATUS              IS  状態キー
000470                             LOCK        MODE         IS  AUTOMATIC.
000480     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
000490                             ORGANIZATION             IS  INDEXED
000500                             ACCESS MODE              IS  DYNAMIC
000510                             RECORD KEY           IS 施記－施術和暦年月日
000520                                                     施記－患者コード
000530                             ALTERNATE RECORD KEY IS 施記－患者コード
000540                                                     施記－施術和暦年月日
000550                             FILE STATUS              IS  状態キー
000560                             LOCK        MODE         IS  AUTOMATIC.
000260     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY               IS  受－施術和暦年月
000300                                                          受－患者コード
000310                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000320                                                          受－患者カナ
000330                                                          受－患者コード
000340                             ALTERNATE RECORD KEY     IS  受－患者コード
000350                                                          受－施術和暦年月
000360                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000370                                                          受－保険種別
000380                                                          受－保険者番号
000390                                                          受－患者コード
000400                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000410                                                          受－公費種別
000420                                                          受－費用負担者番号
000430                                                          受－患者コード
000440                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000450                                                          受－助成種別
000460                                                          受－費用負担者番号助成
000470                                                          受－患者コード
000480                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
000490                                                          受－施術和暦年月
000500                                                          受－患者コード
000510                             FILE STATUS              IS  状態キー
000520                             LOCK        MODE         IS  AUTOMATIC.
000840     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
000850                             ORGANIZATION             IS  INDEXED
000860                             ACCESS MODE              IS  DYNAMIC
000870                             RECORD KEY               IS 負－施術和暦年月
000880                                                         負－患者コード
000890                             ALTERNATE RECORD KEY     IS 負－患者コード
000900                                                         負－施術和暦年月
000910                             FILE STATUS              IS  状態キー
000920                             LOCK        MODE         IS  AUTOMATIC.
000770     SELECT  経過マスタ      ASSIGN      TO        KEIKAL
000780                             ORGANIZATION             IS  INDEXED
000790                             ACCESS MODE              IS  DYNAMIC
000800                             RECORD KEY               IS  経－区分コード
000810                                                          経－経過コード
000820                             FILE STATUS              IS  状態キー
000830                             LOCK        MODE         IS  AUTOMATIC.
000921     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
000922                             ORGANIZATION             IS  INDEXED
000923                             ACCESS MODE              IS  DYNAMIC
000924                             RECORD KEY               IS  負原－区分コード
000925                                                          負原－負傷原因コード
000926                             FILE STATUS              IS  状態キー
000927                             LOCK        MODE         IS  AUTOMATIC.
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
000611     SELECT  会計データＦ    ASSIGN      TO        KAIKEIL
000612                             ORGANIZATION             IS  INDEXED
000613                             ACCESS MODE              IS  DYNAMIC
000089                             RECORD KEY               IS  会－施術和暦年月日
000090                                                          会－患者コード
000092                             ALTERNATE RECORD KEY     IS  会－患者コード
000093                                                          会－施術和暦年月日
000621                             FILE STATUS              IS  状態キー
000622                             LOCK        MODE         IS  AUTOMATIC.
           SELECT  計算マスタ      ASSIGN      TO        KEISANL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  計－制御区分
                                                                計－開始和暦年月
                                   FILE STATUS              IS  状態キー
                                   LOCK        MODE         IS  AUTOMATIC.
001113     SELECT  長期継続者Ｆ    ASSIGN      TO        CHOKEIL
001120                             ORGANIZATION             IS INDEXED
001130                             ACCESS MODE              IS DYNAMIC
001140                             RECORD KEY               IS 長継－施術和暦年月
001150                                                         長継－患者コード
001160                             ALTERNATE RECORD KEY     IS 長継－患者コード
001170                                                         長継－施術和暦年月
001180                             FILE STATUS              IS 状態キー
001190                             LOCK      MODE           IS AUTOMATIC.
000810     SELECT  ＩＤ管理マスタ  ASSIGN      TO        IDKANRL
000820                             ORGANIZATION             IS  INDEXED
000830                             ACCESS MODE              IS  DYNAMIC
000840                             RECORD KEY               IS  ＩＤ管－ＩＤ区分
000850                                                          ＩＤ管－施術所番号
000860                                                          ＩＤ管－保険種別
000870                                                          ＩＤ管－保険者番号
000880                             ALTERNATE RECORD KEY     IS  ＩＤ管－施術ＩＤ番号
000890                                                          ＩＤ管－ＩＤ区分
000900                                                          ＩＤ管－施術所番号
000910                                                          ＩＤ管－保険種別
000920                                                          ＩＤ管－保険者番号
000930                             FILE STATUS              IS  状態キー
000940                             LOCK        MODE         IS  AUTOMATIC.
000241     SELECT  生保情報Ｆ      ASSIGN      TO        SEIHOJL
000242                             ORGANIZATION           IS INDEXED
000243                             ACCESS MODE            IS DYNAMIC
000244                             RECORD KEY             IS 生保－施術和暦年月
000245                                                       生保－患者コード
000255                             ALTERNATE RECORD KEY   IS 生保－患者コード
000265                                                       生保－施術和暦年月
000277                             FILE STATUS            IS 状態キー
000278                             LOCK        MODE       IS AUTOMATIC.
001520     SELECT 作業ファイル１   ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1011L.DAT"
001530                             ORGANIZATION             IS  INDEXED
001540                             ACCESS MODE              IS  DYNAMIC
000905                             RECORD      KEY          IS  作１－保険者番号
                                                                作１－種別
000908                                                          作１－本人家族区分
000908                                                          作１－患者カナ
000911                                                          作１－患者コード
000912                                                          作１－施術和暦年月
000952                             FILE STATUS              IS  状態キー
001600                             LOCK        MODE         IS  AUTOMATIC.
001520     SELECT 作業ファイル２   ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1012L.DAT"
000410                             ORGANIZATION             IS  LINE SEQUENTIAL
000420                             ACCESS MODE              IS  SEQUENTIAL
000430                             FILE STATUS              IS  状態キー
000440                             LOCK      MODE           IS  AUTOMATIC.
001224******************************************************************
001230*                      DATA DIVISION                             *
001240******************************************************************
001250 DATA                    DIVISION.
001260 FILE                    SECTION.
001261*                           ［ＲＬ＝  ２５６］
001262 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001263     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
001270*                           ［ＲＬ＝  １２８］
001280 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001290     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001300*                           ［ＲＬ＝  １２８］
001310 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
001320     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
001330*
001340 FD  施術所情報マスタ    BLOCK   CONTAINS   1   RECORDS.
001350     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
001360*                           ［ＲＬ＝  ２５６］
001370 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
001380     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
001390*                           ［ＲＬ＝  ３２０］
001400 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
001410     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
001420*                           ［ＲＬ＝  １２８］
001430 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
001440     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
002290*                           ［ＲＬ＝  １２８］
002300 FD  経過マスタ          BLOCK   CONTAINS   1   RECORDS.
002310     COPY KEIKA          OF  XFDLIB  JOINING   経   AS  PREFIX.
001441*                           ［ＲＬ＝  １２８］
001442 FD  負傷原因Ｆ         BLOCK   CONTAINS   1   RECORDS.
001443     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
000981*                           ［ＲＬ＝  ５１２］
000982 FD  会計データＦ        BLOCK   CONTAINS   1   RECORDS.
000983     COPY KAIKEI          OF  XFDLIB  JOINING   会   AS  PREFIX.
      *                           ［ＲＬ＝  ２５６］
       FD  計算マスタ          BLOCK   CONTAINS   1   RECORDS.
           COPY KEISAN          OF  XFDLIB  JOINING   計   AS  PREFIX.
           COPY KEISANA         OF  XFDLIB  JOINING   計Ａ AS  PREFIX.
003410*                           ［ＲＬ＝  １２８］
003420 FD  長期継続者Ｆ        BLOCK   CONTAINS   1   RECORDS.
003430     COPY CHOKEI     OF  XFDLIB  JOINING   長継   AS  PREFIX.
002070*                           ［ＲＬ＝  １２８］
002080 FD  ＩＤ管理マスタ          BLOCK   CONTAINS   1   RECORDS.
002090     COPY IDKANR          OF  XFDLIB  JOINING   ＩＤ管   AS  PREFIX.
001080* 
000280 FD  生保情報Ｆ          BLOCK   CONTAINS   1   RECORDS.
000281     COPY SEIHOJ          OF  XFDLIB  JOINING   生保   AS  PREFIX.
002352*
001520 FD 作業ファイル１ RECORD  CONTAINS 9071 CHARACTERS.
001200 01 作１－レコード.
001210    03 作１－レコードキー.
001271       05 作１－保険者番号              PIC X(8).
             05 作１－種別                    PIC 9(2).
001280       05 作１－本人家族区分            PIC 9.
001300       05 作１－患者カナ                PIC X(50).
001310       05 作１－患者コード.
001320          07 作１－患者番号             PIC 9(6).
001330          07 作１－枝番                 PIC X(1).
001340       05 作１－施術和暦年月            PIC 9(5).
          03 作１－レコードデータ.
001340       05 作１－データ区分              PIC 9(1).
001340       05 作１－施術年月                PIC 9(6).
             05 作１－会員番号                PIC 9(9).
             05 作１－保険者番号本体          PIC X(8).
             05 作１－保険証記号              PIC X(30).
             05 作１－保険証番号              PIC X(16).
             05 作１－保険証枝番              PIC 9(2).
             05 作１－医療助成区分            PIC 9(1).
             05 作１－負担者番号１            PIC X(8).
             05 作１－受給者番号１            PIC X(16).
             05 作１－負担者番号２            PIC X(8).
             05 作１－受給者番号２            PIC X(16).
             05 作１－保険種別                PIC 9(1).
             05 作１－単併区分                PIC 9(1).
001280       05 作１－本家区分                PIC 9.
001280       05 作１－本家区分２              PIC 9.
             05 作１－給付割合                PIC 9(2).
001280       05 作１－本人家族区分２          PIC 9.
             05 作１－被保険者カナ            PIC X(30).
             05 作１－被保険者氏名            PIC X(40).
             05 作１－受診者カナ              PIC X(30).
             05 作１－受診者氏名              PIC X(40).
             05 作１－患者性別                PIC 9(1).
             05 作１－患者生年月日            PIC 9(8).
             05 作１－合計金額                PIC 9(6).
             05 作１－一部負担金              PIC 9(6).
             05 作１－請求金額                PIC 9(6).
             05 作１－公費一部負担金          PIC 9(6).
             05 作１－公費請求金額            PIC 9(6).
             05 作１－公費一部負担金２        PIC 9(6).
             05 作１－公費請求金額２          PIC 9(6).
             05 作１－実日数                  PIC 9(3).
             05 作１－部位数                  PIC 9(1).
             05 作１－再請求区分              PIC 9(1).
             05 作１－高齢者区分              PIC 9(1).
             05 作１－患者Ｎ                  PIC X(10).
000566       05 作１－負傷部位データ  OCCURS 8.
000567          07 作１－負傷区分             PIC 9.
000567          07 作１－負傷コード           PIC 9(6).
000568          07 作１－負傷名               PIC X(32).
000569          07 作１－初回処置回数         PIC 9.
000569          07 作１－初回処置料           PIC 9(5).
000570          07 作１－負傷年月日           PIC 9(8).
000571          07 作１－初検年月日           PIC 9(8).
000572          07 作１－施術開始年月日       PIC 9(8).
000573          07 作１－施術終了年月日       PIC 9(8).
000574          07 作１－部位実日数           PIC 9(2).
                07 作１－継続月数             PIC 9(2).
000575          07 作１－転帰区分             PIC 9.
             05 作１－請求区分                PIC 9(1).
             05 作１－施術日                  PIC X(31).
             05 作１－初検回数                PIC 9(1).
             05 作１－初検料                  PIC 9(5).
             05 作１－初検時間外加算回数      PIC 9(1).
             05 作１－初検休日加算回数        PIC 9(1).
             05 作１－初検深夜加算回数        PIC 9(1).
             05 作１－初検時間外加算日        PIC 9(2).
             05 作１－初検時間外加算時刻      PIC 9(4).
             05 作１－初検休日加算日          PIC 9(2).
             05 作１－初検深夜加算日          PIC 9(2).
             05 作１－初検深夜加算時刻        PIC 9(4).
             05 作１－初検加算                PIC 9(5).
             05 作１－相談支援回数            PIC 9(1).
             05 作１－相談支援料              PIC 9(5).
             05 作１－再検回数                PIC 9(1).
             05 作１－再検料                  PIC 9(5).
             05 作１－往療距離                PIC 9(3).
             05 作１－往療回数                PIC 9(2).
             05 作１－往療料                  PIC 9(5).
             05 作１－夜間加算往療回数        PIC 9(1).
             05 作１－難路加算往療回数        PIC 9(1).
             05 作１－暴風雨雪加算往療回数    PIC 9(1).
             05 作１－往療加算                PIC 9(5).
000561       05 作１－金属副子回数            PIC 9.
             05 作１－金属副子加算            PIC 9(5).
000564       05 作１－情報提供料回数          PIC 9.
000564       05 作１－情報提供料              PIC 9(5).
             05 作１－運動後療料回数          PIC 9(1).
             05 作１－運動後療料              PIC 9(5).
             05 作１－明細書発行回数          PIC 9(1).
             05 作１－明細書発行              PIC 9(5).
             05 作１－明細書発行月日          PIC 9(4).
000566       05 作１－金属加算データ  OCCURS 8.
000570          07 作１－金属副子日１         PIC 9(8).
000570          07 作１－金属副子日２         PIC 9(8).
000570          07 作１－金属副子日３         PIC 9(8).
             05 作１－運動後療日              PIC X(31).
             05 作１－医師の同意              PIC 9(1).
000566       05 作１－部位料金データ  OCCURS 14.
000570          07 作１－逓減開始月日         PIC 9(4).
000576          07 作１－後療回数             PIC 9(2).
000576          07 作１－後療料               PIC 9(5).
000577          07 作１－冷罨法回数           PIC 9.
000576          07 作１－冷罨法料             PIC 9(5).
000578          07 作１－温罨法回数           PIC 9(2).
000576          07 作１－温罨法料             PIC 9(5).
000579          07 作１－電療回数             PIC 9(2).
000576          07 作１－電療料               PIC 9(5).
000580          07 作１－多部位逓減率         PIC 9(2).
000576          07 作１－多部位逓減額         PIC 9(5).
000581          07 作１－長期逓減率           PIC 9(2).
000581          07 作１－長期頻回逓減率       PIC 9(2).
000576          07 作１－部位別料金計         PIC 9(5).
             05 作１－負傷原因                PIC X(1600).
             05 作１－経過                    PIC X(200).
             05 作１－長期理由                PIC X(1600).
             05 作１－往療理由                PIC X(200).
             05 作１－適用                    PIC X(1000).
             05 作１－患者郵便番号            PIC X(8).
             05 作１－患者住所１              PIC X(50).
             05 作１－患者住所２              PIC X(50).
             05 作１－患者住所３              PIC X(50).
             05 作１－患者電話番号            PIC X(14).
             05 作１－初検料計                PIC 9(6).
             05 作１－整復料計                PIC 9(6).
             05 作１－労働保険番号            PIC X(14).
             05 作１－労災指導管理料回数      PIC 9(2).
             05 作１－労災指導管理料          PIC 9(5).
             05 作１－労災文書回数            PIC 9(2).
             05 作１－労災文書料              PIC 9(5).
             05 作１－労災休業証明開始        PIC 9(8).
             05 作１－労災休業証明終了        PIC 9(8).
000566       05 作１－労災料金データ  OCCURS 8.
000570          07 作１－特別材料料           PIC 9(5).
000570          07 作１－包帯交換回数         PIC 9(2).
000570          07 作１－包帯交換料           PIC 9(5).
             05 作１－予備                    PIC X(2083).
002352*
001520 FD 作業ファイル２ RECORD  CONTAINS 1198 CHARACTERS.
001200 01 作２－レコード.
          03 作２－レコードデータ             PIC X(1198).
      *
002353*----------------------------------------------------------------*
002354******************************************************************
002360*                WORKING-STORAGE SECTION                         *
002370******************************************************************
002380 WORKING-STORAGE         SECTION.
002390 01 キー入力                           PIC X    VALUE SPACE.
002400 01 状態キー                           PIC X(2) VALUE SPACE.
002410 01 初検フラグ                         PIC X(3) VALUE SPACE.
002420 01 終了フラグ                         PIC X(3) VALUE SPACE.
002430 01 終了フラグ２                       PIC X(3) VALUE SPACE.
001270 01 終了フラグ４                       PIC X(3) VALUE SPACE.
002440 01 実行キーＷ                         PIC X(3) VALUE SPACE.
002450 01 施術記録有Ｗ                       PIC X(3) VALUE SPACE.
002460 01 ファイル名                         PIC N(8) VALUE SPACE.
       01 県ＩＤＷ.
          03 施術ＩＤ番号Ｗ                  PIC 9(10) VALUE ZERO.
002470*
002480 01 保険種別ＷＲ                       PIC 9(2) VALUE ZERO.
002490 01 患者コードＷＲ.
002500    03 患者番号ＷＲ                    PIC 9(6) VALUE ZERO.
002510    03 枝番ＷＲ                        PIC X    VALUE SPACE.
       01 住所Ｗ.
          03 住所１Ｗ                        PIC X(50) VALUE SPACE.
          03 住所２Ｗ                        PIC X(50) VALUE SPACE.
          03 住所３Ｗ                        PIC X(50) VALUE SPACE.
002520*
002530 01 印刷形式ＷＲ                       PIC 9    VALUE ZERO.
002550 01 レセプト種類ＷＲ                   PIC X(4) VALUE SPACE.
002560 01 本人家族区分ＷＲ                   PIC 9    VALUE ZERO.
002570 01 続柄Ｗ                             PIC N(2) VALUE SPACE.
002580 01 施術和暦年月ＷＲ.
002590    03 施術和暦ＷＲ                    PIC 9    VALUE ZERO.
002600    03 施術年ＷＲ                      PIC 9(2) VALUE ZERO.
002610    03 施術月ＷＲ                      PIC 9(2) VALUE ZERO.
002620 01 請求和暦年月ＷＲ.
002630    03 請求和暦ＷＲ                    PIC 9    VALUE ZERO.
002640    03 請求年ＷＲ                      PIC 9(2) VALUE ZERO.
002650    03 請求月ＷＲ                      PIC 9(2) VALUE ZERO.
001970 01 保険証枝番Ｗ.
          03 保険枝番Ｗ                      PIC 9(2) VALUE ZERO.
002660**
002670 01 連番Ｗ                             PIC 9(4) VALUE ZERO.
002680 01 助成フラグ                         PIC X(3) VALUE SPACE.
002690 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002700 01 負傷種別変換前Ｗ                   PIC 9(2)  VALUE ZERO.
002710 01 負傷種別変換後Ｗ                   PIC 9     VALUE ZERO.
002720 01 転帰変換前Ｗ                       PIC 9     VALUE ZERO.
002730 01 転帰変換後Ｗ                       PIC 9     VALUE ZERO.
002740**
002750 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002760 01 カウンタ                           PIC 9(2)  VALUE ZERO.
002770 01 カウンタ２                         PIC 9(3)  VALUE ZERO.
002780 01 カウンタ３                         PIC 9(2)  VALUE ZERO.
002790 01 全角空白                           PIC X(2)  VALUE X"8140".
002800 01 半角空白                           PIC X(2)  VALUE X"2020".
002810**
002820** エラーメッセージ用
002830 01 エラーメッセージＷ.
002840    03 エラー患者コードＷ              PIC X(7) VALUE SPACE.
002850    03 エラー区切りＷ                  PIC X(1) VALUE SPACE.
002860    03 エラー保険種別Ｗ                PIC X(2) VALUE SPACE.
002870    03 FILLER                          PIC X(10) VALUE SPACE.
002880** 保険者番号右詰め用
002890 01 保険者番号ＷＴ.
002900    03 保険者番号左詰めＷ.
002910      05 保険者番号左詰めＷ１          PIC X OCCURS 8 VALUE SPACE.
002920    03 保険者番号右詰めＷ.
002930      05 保険者番号右詰めＷ１          PIC X OCCURS 8 VALUE ZERO.
002940    03 保険者番号数字Ｗ                PIC 9(8)  VALUE ZERO.
002640    03 保険者番号Ｗ                    PIC X(8)  VALUE SPACE.
002960** 会員番号右詰め用
002970 01 会員番号ＷＴ.
002980    03 会員番号左詰めＷ.
002990      05 会員番号左詰めＷ１            PIC X OCCURS 7 VALUE SPACE.
003000    03 会員番号右詰めＷ.
003010      05 会員番号右詰めＷ１            PIC X OCCURS 7 VALUE ZERO.
003020    03 会員番号数字Ｗ                  PIC 9(7)  VALUE ZERO.
003030    03 会員番号Ｗ                      PIC X(7)  VALUE SPACE.
002960** ＩＤ番号右詰め用
002970 01 ＩＤ番号ＷＴ.
002980    03 ＩＤ番号左詰めＷ.
002990      05 ＩＤ番号左詰めＷ１            PIC X OCCURS 10 VALUE SPACE.
003000    03 ＩＤ番号右詰めＷ.
003010      05 ＩＤ番号右詰めＷ１            PIC X OCCURS 10 VALUE ZERO.
003020    03 ＩＤ番号数字Ｗ                  PIC 9(10)  VALUE ZERO.
003030    03 ＩＤ番号Ｗ                      PIC X(10)  VALUE SPACE.
003040** 西暦日付ワーク用
003050 01 西暦年月Ｗ.
003060    03 西暦年Ｗ                        PIC 9(4) VALUE ZERO.
003070    03 西暦月Ｗ                        PIC 9(2) VALUE ZERO.
003080** 西暦請求年月用
003090 01 西暦請求年月Ｗ.
003100    03 西暦請求年Ｗ                    PIC 9(4) VALUE ZERO.
003110    03 西暦請求月Ｗ                    PIC 9(2) VALUE ZERO.
003120** 西暦施術年月用
003130 01 西暦施術年月Ｗ.
003140    03 西暦施術年Ｗ                    PIC 9(4) VALUE ZERO.
003150    03 西暦施術月Ｗ                    PIC 9(2) VALUE ZERO.
003080** 西暦提出年月用
003090 01 西暦提出年月Ｗ.
003100    03 西暦提出年Ｗ                    PIC 9(4) VALUE ZERO.
003110    03 西暦提出月Ｗ                    PIC 9(2) VALUE ZERO.
       01 翌月西暦年月Ｗ.
          03 翌月西暦年Ｗ                    PIC 9(4)  VALUE ZERO.
          03 翌月西暦月Ｗ                    PIC 9(2)  VALUE ZERO.

003160** 記号左詰め用
003170 01 記号ＷＴ.
003180    03 記号元Ｗ.
003190      05 記号元Ｗ１                    PIC N OCCURS 12 VALUE SPACE.
003200    03 記号左詰めＷ.
003210      05 記号左詰めＷ１                PIC N OCCURS 12 VALUE SPACE.
003180    03 記号元ＸＷ.
003190      05 記号元ＸＷ１                  PIC X OCCURS 24 VALUE SPACE.
003200    03 記号左詰めＸＷ.
003210      05 記号左詰めＸＷ１              PIC X OCCURS 24 VALUE SPACE.
003220    03 記号Ｗ.
003230      05 記号ＮＷ                      PIC N(12) VALUE SPACE.
003240    03 記号ＰＷ.
003250      05 記号ＰＮＷ                    PIC X(24) VALUE SPACE.
003260** 助成負担者番号左詰め用
003270 01 助成番号ＷＴ.
003280    03 助成番号元Ｗ.
003290      05 助成番号元Ｗ１                PIC X OCCURS 10 VALUE SPACE.
003300    03 助成番号左詰めＷ.
003310      05 助成番号左詰めＷ１            PIC X OCCURS 10 VALUE SPACE.
003320    03 助成番号Ｗ                      PIC X(10) VALUE SPACE.
003330*
003340** 西暦年月日ワーク用
003350 01 計算西暦年月日Ｗ.
003360    03 計算西暦年Ｗ                    PIC 9(4) VALUE ZERO.
003370    03 計算西暦月Ｗ                    PIC 9(2) VALUE ZERO.
003380    03 計算西暦日Ｗ                    PIC 9(2) VALUE ZERO.
003390 01 計算和暦年月日Ｗ.
003400    03 計算和暦Ｗ                      PIC 9 VALUE ZERO.
003410    03 計算年Ｗ                        PIC 9(2) VALUE ZERO.
003420    03 計算月Ｗ                        PIC 9(2) VALUE ZERO.
003430    03 計算日Ｗ                        PIC 9(2) VALUE ZERO.
003440** 枝番判定用
003450 01 開始診療日手動区分Ｗ               PIC 9    VALUE ZERO.
003460*
003470* 終了日退避用
003480 01 終了年月日ＷＴ.
003490    03 終了和暦ＷＴ                    PIC 9     VALUE ZERO.
003500    03 終了年ＷＴ                      PIC 9(2)  VALUE ZERO.
003510    03 終了月ＷＴ                      PIC 9(2)  VALUE ZERO.
003520    03 終了日ＷＴ                      PIC 9(2)  VALUE ZERO.
003530* 初検日退避用
003540 01 初検年月日ＷＴ.
003550    03 初検和暦ＷＴ                    PIC 9     VALUE ZERO.
003560    03 初検年ＷＴ                      PIC 9(2)  VALUE ZERO.
003570    03 初検月ＷＴ                      PIC 9(2)  VALUE ZERO.
003580    03 初検日ＷＴ                      PIC 9(2)  VALUE ZERO.
003590*
003591* 連計の金額退避用
003592 01 連計金額Ｗ.
003593    03  費用額Ｗ                   PIC 9(6) VALUE ZERO.
003594    03  負担額Ｗ                   PIC 9(6) VALUE ZERO.
003595    03  請求額Ｗ                   PIC 9(6) VALUE ZERO.
003596    03  費用額老人Ｗ               PIC 9(6) VALUE ZERO.
003597    03  負担額老人Ｗ               PIC 9(6) VALUE ZERO.
003598    03  請求額老人Ｗ               PIC 9(6) VALUE ZERO.
003599    03  費用額助成Ｗ               PIC 9(6) VALUE ZERO.
003600    03  負担額助成Ｗ               PIC 9(5) VALUE ZERO.
003601    03  請求額助成Ｗ               PIC 9(5) VALUE ZERO.
003602    03  負担率Ｗ                   PIC 9(3) VALUE ZERO.
003603*
003604* 負傷原因用
003605 01 負傷原因ＷＴ.
003606    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
003607    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
003608    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
003609    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
003610    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
003611    03 負傷原因ナンバーＷＴ.
003612       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
003613    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
003614 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
003615 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
003616 01 負傷原因ＴＢＬ.
003617    03 負傷原因コードＴＢＬ            OCCURS 9.
003618       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
003619       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
003620       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
003621 01 負傷原因内容Ｗ.
003622    03 負傷原因内容合成Ｗ              PIC X(318) OCCURS 9 VALUE SPACE.
003623    03 負傷原因内容分解Ｗ.
005400       05 負傷原因内容１Ｗ             PIC X(60)  VALUE SPACE.
005410       05 負傷原因内容２Ｗ             PIC X(60)  VALUE SPACE.
005420       05 負傷原因内容３Ｗ             PIC X(60)  VALUE SPACE.
005400       05 負傷原因内容４Ｗ             PIC X(60)  VALUE SPACE.
005410       05 負傷原因内容５Ｗ             PIC X(60)  VALUE SPACE.
005420       05 負傷原因内容６Ｗ             PIC X(18)  VALUE SPACE.
       01 転記用負傷原因Ｗ.
          03 転記用負傷原因ＮＷ              PIC N(150) VALUE SPACE.
005370 01 転記用負傷原因内容Ｗ.
005380    03 転記用負傷原因内容合成Ｗ        OCCURS 9.
005380       05 転記用負傷原因内容合成ＸＷ   PIC X(318) VALUE SPACE.
003627*
003628** 負傷原因・長期理由印刷区分用
003629 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
003630 01 長期理由印刷区分Ｗ                 PIC 9 VALUE ZERO.
003631*
       01 経過文Ｗ.
009730    03 経過略称Ｗ                      OCCURS 5.
009740       05 印刷経過略称Ｗ               PIC X(10)  VALUE SPACE.
       01 経過Ｗ.
          03 経過ＷＲ                        PIC N(6)   VALUE SPACE.
009760 01 部位番号Ｗ                         PIC X(2)   VALUE SPACE.
001370 01 理由文Ｗ                           PIC X(320)   VALUE SPACE.
001367 01 合成理由文Ｗ.
001370    03 合成理由文ＷＴ                  PIC X(342)   OCCURS 5 VALUE SPACE.
       01 ナンバーＷＴ.
          03 ナンバーＷ                      PIC X(2)     OCCURS 5 VALUE SPACE.
       01 ナンバーＮＷ  REDEFINES ナンバーＷＴ PIC X(10).
001201 01 番号ＣＮＴ                         PIC 9(1) VALUE ZERO.
001370 01 文章Ｗ                             PIC X(400)  VALUE SPACE.
003631*
       01 適用Ｗ.
009730    03 適用文Ｗ                        OCCURS 8  PIC X(126)  VALUE SPACE.
003631*
003632** 助成レセまとめ用
003633 01 助成レセまとめフラグ               PIC X(3)  VALUE SPACE.
003634*
003635**********************************************************************************
003636*
003637 01 退避項目ＧＷ.
003638   03 レセプト種類Ｗ                   PIC X(4).
003640   03 レセプト種類ＧＷ                 PIC X(4).
003650   03 レセプト種別ＧＷ                 PIC 9(2).
003660*
003670****************
003680* 負傷データＦ *
003690****************
003700 01 負傷情報Ｗ.
003710    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
003720    03 部位情報Ｗ  OCCURS   9.
003730       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
003740       05 部位コードＷ.
003750          07 負傷種別Ｗ                PIC 9(2)  VALUE ZERO.
003760          07 部位Ｗ                    PIC 9(2)  VALUE ZERO.
003770          07 左右区分Ｗ                PIC 9(1)  VALUE ZERO.
003780          07 負傷位置番号Ｗ            PIC 9(2)  VALUE ZERO.
             05 負傷名ＷＲ.
003790          07 負傷名Ｗ                  PIC N(16) VALUE SPACE.
003800       05 負傷年月日Ｗ.
003810          07 負傷和暦Ｗ                PIC 9     VALUE ZERO.
003820          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
003830          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
003840          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
003850       05 初検年月日Ｗ.
003860          07 初検和暦Ｗ                PIC 9     VALUE ZERO.
003870          07 初検年Ｗ                  PIC 9(2)  VALUE ZERO.
003880          07 初検月Ｗ                  PIC 9(2)  VALUE ZERO.
003890          07 初検日Ｗ                  PIC 9(2)  VALUE ZERO.
003900       05 開始年月日Ｗ.
003910          07 開始和暦Ｗ                PIC 9     VALUE ZERO.
003920          07 開始年Ｗ                  PIC 9(2)  VALUE ZERO.
003930          07 開始月Ｗ                  PIC 9(2)  VALUE ZERO.
003940          07 開始日Ｗ                  PIC 9(2)  VALUE ZERO.
003950       05 終了年月日Ｗ.
003960          07 終了和暦Ｗ                PIC 9     VALUE ZERO.
003970          07 終了年Ｗ                  PIC 9(2)  VALUE ZERO.
003980          07 終了月Ｗ                  PIC 9(2)  VALUE ZERO.
003990          07 終了日Ｗ                  PIC 9(2)  VALUE ZERO.
004000       05 実日数Ｗ                     PIC 9(2)  VALUE ZERO.
004000       05 継続月数Ｗ                   PIC 9(2)  VALUE ZERO.
004010       05 初回処置回数Ｗ               PIC 9     VALUE ZERO.
004010       05 初回処置料Ｗ                 PIC 9(5)  VALUE ZERO.
004020       05 転帰区分Ｗ                   PIC 9(1)  VALUE ZERO.
004030    03 新規区分Ｗ                      PIC 9(1)  VALUE ZERO.
004040    03 継続区分Ｗ                      PIC 9(1)  VALUE ZERO.
          03 負傷原因Ｗ OCCURS 27.
004041       05 負傷原因ＷＰ                 PIC X(74) VALUE SPACE.
004050*
004060*********************************************************************
004070*    ************
004080*    * 料金情報 *
004090*    ************
004100*    月毎の料金
004110***********************
004120 01 料金１ＷＲ.
004130   03 初検ＷＲ.
004140      05 初検回数Ｗ                 PIC 9(1)    VALUE ZERO.
004140      05 初検料Ｗ                   PIC 9(5)    VALUE ZERO.
004150      05 初検時間外回数Ｗ           PIC 9(1)    VALUE ZERO.
004150      05 初検時間外日Ｗ             PIC 9(2)    VALUE ZERO.
004150      05 初検時間外時刻Ｗ.
               07 時間外時Ｗ              PIC 9(2)    VALUE ZERO.
               07 時間外分Ｗ              PIC 9(2)    VALUE ZERO.
004160      05 初検休日回数Ｗ             PIC 9(1)    VALUE ZERO.
004150      05 初検休日日Ｗ               PIC 9(2)    VALUE ZERO.
004170      05 初検深夜回数Ｗ             PIC 9(1)    VALUE ZERO.
004150      05 初検深夜日Ｗ               PIC 9(2)    VALUE ZERO.
004150      05 初検深夜時刻Ｗ.
               07 深夜時Ｗ                PIC 9(2)    VALUE ZERO.
               07 深夜分Ｗ                PIC 9(2)    VALUE ZERO.
004140      05 初検加算Ｗ                 PIC 9(5)    VALUE ZERO.
004180   03 再検回数Ｗ                    PIC 9(1)    VALUE ZERO.
004140   03 再検料Ｗ                      PIC 9(5)    VALUE ZERO.
004190   03 往療ＷＲ.
004200      05 往療回数Ｗ                 PIC 9(2)    VALUE ZERO.
004210      05 往療距離Ｗ                 PIC 9(3)V9  VALUE ZERO.
004211      05 往療距離２Ｗ               PIC 9(3)    VALUE ZERO.
004140      05 往療料Ｗ                   PIC 9(5)    VALUE ZERO.
004220      05 往療夜間Ｗ                 PIC 9(1)    VALUE ZERO.
004230      05 往療難路Ｗ                 PIC 9(2)    VALUE ZERO.
004240      05 往療暴風Ｗ                 PIC 9(2)    VALUE ZERO.
004140      05 往療加算Ｗ                 PIC 9(5)    VALUE ZERO.
      */金属副子変更/20180607
000561   05 金属副子回数Ｗ                PIC 9(1)    VALUE ZERO.
004140   03 金属副子加算Ｗ                PIC 9(5)    VALUE ZERO.
         03 金属月日Ｗ                    OCCURS 8.
            05 金属月日１Ｗ               PIC 9(7)    VALUE ZERO.
            05 金属月日２Ｗ               PIC 9(7)    VALUE ZERO.
            05 金属月日３Ｗ               PIC 9(7)    VALUE ZERO.
004280   03 情報提供料回数Ｗ              PIC 9(1)    VALUE ZERO.
004140   03 情報提供料Ｗ                  PIC 9(5)    VALUE ZERO.
004140   03 初検料計Ｗ                    PIC 9(6)    VALUE ZERO.
004140   03 整復料計Ｗ                    PIC 9(6)    VALUE ZERO.
004290   03 一部負担金ＷＲ                PIC 9(6)    VALUE ZERO.
004300   03 請求金額ＷＲ                  PIC 9(6)    VALUE ZERO.
004310   03 給付割合ＷＲ                  PIC 9(1)    VALUE ZERO.
004320   03 受給者負担額ＷＲ              PIC 9(6)    VALUE ZERO.
004330   03 助成請求金額ＷＲ              PIC 9(6)    VALUE ZERO.
      */
         03 相談支援回数Ｗ                PIC 9(1)    VALUE ZERO.
004140   03 相談支援料Ｗ                  PIC 9(5)    VALUE ZERO.
         03 施術日ＴＷ.
            05 施術日Ｗ                   PIC 9(1) OCCURS 31 VALUE ZERO.
      */運動後療追加/20180607
         03 運動後療料回数Ｗ              PIC 9(1)    VALUE ZERO.
         03 運動後療料Ｗ                  PIC 9(5)    VALUE ZERO.
         03 運動後療日ＴＷ.
            05 運動後療日Ｗ               PIC 9(1) OCCURS 31 VALUE ZERO.
      */明細書発行体制加算追加/20221012
         03 明細書発行回数Ｗ              PIC 9(1)    VALUE ZERO.
         03 明細書発行Ｗ                  PIC 9(3)    VALUE ZERO.
         03 明細書発行月日Ｗ.
            05 明細書発行月Ｗ             PIC 9(2)    VALUE ZERO.
            05 明細書発行日Ｗ             PIC 9(2)    VALUE ZERO.
004440*
004450* 逓減毎の料金
004460***********************
004470 01 料金３ＷＲ.
004480**********
004490* １部位 *
004500**********
004510   03 部位１ＷＲ.
004520      05 後療１ＷＲ.
004530         07 後療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
004530         07 後療料１ＷＲ                PIC 9(5)    VALUE ZERO.
004540      05 冷罨法１ＷＲ.
004550         07 冷罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
004530         07 冷罨法料１ＷＲ              PIC 9(5)    VALUE ZERO.
004560      05 温罨法１ＷＲ.
004570         07 温罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
004530         07 温罨法料１ＷＲ              PIC 9(5)    VALUE ZERO.
004580      05 電療１ＷＲ.
004590         07 電療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
004530         07 電療料１ＷＲ                PIC 9(5)    VALUE ZERO.
004590      05 長期率１ＷＲ                   PIC 9(2)    VALUE ZERO.
004590      05 長期頻回率１ＷＲ               PIC 9(2)    VALUE ZERO.
004590      05 部位合計１ＷＲ                 PIC 9(5)    VALUE ZERO.
004600**********
004610* ２部位 *
004620**********
004630   03 部位２ＷＲ.
004640      05 後療２ＷＲ.
004650         07 後療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
004530         07 後療料２ＷＲ                PIC 9(5)    VALUE ZERO.
004660      05 冷罨法２ＷＲ.
004670         07 冷罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
004530         07 冷罨法料２ＷＲ              PIC 9(5)    VALUE ZERO.
004680      05 温罨法２ＷＲ.
004690         07 温罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
004530         07 温罨法料２ＷＲ              PIC 9(5)    VALUE ZERO.
004700      05 電療２ＷＲ.
004710         07 電療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
004530         07 電療料２ＷＲ                PIC 9(5)    VALUE ZERO.
004590      05 長期率２ＷＲ                   PIC 9(2)    VALUE ZERO.
004590      05 長期頻回率２ＷＲ               PIC 9(2)    VALUE ZERO.
004590      05 部位合計２ＷＲ                 PIC 9(5)    VALUE ZERO.
004720******************
004730* ３部位／割 *
004740******************
004750   03 部位３ＷＲ.
004760      05 後療３ＷＲ.
004770         07 後療回数３ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 後療料３ＷＲ                PIC 9(5)    VALUE ZERO.
004780      05 冷罨法３ＷＲ.
004790         07 冷罨法回数３ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 冷罨法料３ＷＲ              PIC 9(5)    VALUE ZERO.
004800      05 温罨法３ＷＲ.
004810         07 温罨法回数３ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 温罨法料３ＷＲ              PIC 9(5)    VALUE ZERO.
004820      05 電療３ＷＲ.
004830         07 電療回数３ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 電療料３ＷＲ                PIC 9(5)    VALUE ZERO.
004580      05 多部位３ＷＲ.
004590         07 多部位率３ＷＲ              PIC 9(2)    VALUE ZERO.
004530         07 多部位料３ＷＲ              PIC 9(5)    VALUE ZERO.
004590      05 長期率３ＷＲ                   PIC 9(2)    VALUE ZERO.
004590      05 長期頻回率３ＷＲ               PIC 9(2)    VALUE ZERO.
004590      05 部位合計３ＷＲ                 PIC 9(5)    VALUE ZERO.
004840******************
004850* ３部位／１０割 *
004860******************
004870   03 部位３０ＷＲ.
001780      05 逓減開始月日３０ＷＲ.
001790         07 逓減開始月３０ＷＲ            PIC 9(2).
001800         07 逓減開始日３０ＷＲ            PIC 9(2).
004880      05 後療３０ＷＲ.
004890         07 後療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 後療料３０ＷＲ                PIC 9(5)    VALUE ZERO.
004900      05 冷罨法３０ＷＲ.
004910         07 冷罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 冷罨法料３０ＷＲ              PIC 9(5)    VALUE ZERO.
004920      05 温罨法３０ＷＲ.
004930         07 温罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 温罨法料３０ＷＲ              PIC 9(5)    VALUE ZERO.
004940      05 電療３０ＷＲ.
004950         07 電療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 電療料３０ＷＲ                PIC 9(5)    VALUE ZERO.
004590      05 長期率３０ＷＲ                   PIC 9(2)    VALUE ZERO.
004590      05 長期頻回率３０ＷＲ               PIC 9(2)    VALUE ZERO.
004590      05 部位合計３０ＷＲ                 PIC 9(5)    VALUE ZERO.
005200****************
005210* ４部位／割 *
005220****************
005230   03 部位４ＷＲ.
001780      05 逓減開始月日４ＷＲ.
001790         07 逓減開始月４ＷＲ            PIC 9(2).
001800         07 逓減開始日４ＷＲ            PIC 9(2).
005240      05 後療４ＷＲ.
005250         07 後療回数４ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 後療料４ＷＲ                PIC 9(5)    VALUE ZERO.
005260      05 冷罨法４ＷＲ.
005270         07 冷罨法回数４ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 冷罨法料４ＷＲ              PIC 9(5)    VALUE ZERO.
005280      05 温罨法４ＷＲ.
005290         07 温罨法回数４ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 温罨法料４ＷＲ              PIC 9(5)    VALUE ZERO.
005300      05 電療４ＷＲ.
005310         07 電療回数４ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 電療料４ＷＲ                PIC 9(5)    VALUE ZERO.
004580      05 多部位４ＷＲ.
004590         07 多部位率４ＷＲ              PIC 9(2)    VALUE ZERO.
004530         07 多部位料４ＷＲ              PIC 9(5)    VALUE ZERO.
004590      05 長期率４ＷＲ                   PIC 9(2)    VALUE ZERO.
004590      05 長期頻回率４ＷＲ               PIC 9(2)    VALUE ZERO.
004590      05 部位合計４ＷＲ                 PIC 9(5)    VALUE ZERO.
005320******************
005330* ４部位／１０割 *
005340******************
005350   03 部位４０ＷＲ.
001780      05 逓減開始月日４０ＷＲ.
001790         07 逓減開始月４０ＷＲ            PIC 9(2).
001800         07 逓減開始日４０ＷＲ            PIC 9(2).
005360      05 後療４０ＷＲ.
005370         07 後療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 後療料４０ＷＲ                PIC 9(5)    VALUE ZERO.
005380      05 冷罨法４０ＷＲ.
005390         07 冷罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 冷罨法料４０ＷＲ              PIC 9(5)    VALUE ZERO.
005400      05 温罨法４０ＷＲ.
005410         07 温罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 温罨法料４０ＷＲ              PIC 9(5)    VALUE ZERO.
005420      05 電療４０ＷＲ.
005430         07 電療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 電療料４０ＷＲ                PIC 9(5)    VALUE ZERO.
004590      05 長期率４０ＷＲ                   PIC 9(2)    VALUE ZERO.
004590      05 長期頻回率４０ＷＲ               PIC 9(2)    VALUE ZERO.
004590      05 部位合計４０ＷＲ                 PIC 9(5)    VALUE ZERO.
005200****************
005210* ５部位／割 *
005220****************
005230   03 部位５ＷＲ.
001780      05 逓減開始月日５ＷＲ.
001790         07 逓減開始月５ＷＲ            PIC 9(2).
001800         07 逓減開始日５ＷＲ            PIC 9(2).
005240      05 後療５ＷＲ.
005250         07 後療回数５ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 後療料５ＷＲ                PIC 9(5)    VALUE ZERO.
005260      05 冷罨法５ＷＲ.
005270         07 冷罨法回数５ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 冷罨法料５ＷＲ              PIC 9(5)    VALUE ZERO.
005280      05 温罨法５ＷＲ.
005290         07 温罨法回数５ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 温罨法料５ＷＲ              PIC 9(5)    VALUE ZERO.
005300      05 電療５ＷＲ.
005310         07 電療回数５ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 電療料５ＷＲ                PIC 9(5)    VALUE ZERO.
001390      05 小計５ＷＲ                     PIC 9(6)    VALUE ZERO.
004580      05 多部位５ＷＲ.
004590         07 多部位率５ＷＲ              PIC 9(2)    VALUE ZERO.
004530         07 多部位料５ＷＲ              PIC 9(5)    VALUE ZERO.
004590      05 長期率５ＷＲ                   PIC 9(2)    VALUE ZERO.
004590      05 長期頻回率５ＷＲ               PIC 9(2)    VALUE ZERO.
001410      05 部位合計５ＷＲ                 PIC 9(6)    VALUE ZERO.
005320******************
005330* ５部位／１０割 *
005340******************
005350   03 部位５０ＷＲ.
001780      05 逓減開始月日５０ＷＲ.
001790         07 逓減開始月５０ＷＲ            PIC 9(2).
001800         07 逓減開始日５０ＷＲ            PIC 9(2).
005360      05 後療５０ＷＲ.
005370         07 後療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 後療料５０ＷＲ                PIC 9(5)    VALUE ZERO.
005380      05 冷罨法５０ＷＲ.
005390         07 冷罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 冷罨法料５０ＷＲ              PIC 9(5)    VALUE ZERO.
005400      05 温罨法５０ＷＲ.
005410         07 温罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 温罨法料５０ＷＲ              PIC 9(5)    VALUE ZERO.
005420      05 電療５０ＷＲ.
005430         07 電療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 電療料５０ＷＲ                PIC 9(5)    VALUE ZERO.
001390      05 小計５０ＷＲ                     PIC 9(6)    VALUE ZERO.
004590      05 長期率５０ＷＲ                   PIC 9(2)    VALUE ZERO.
004590      05 長期頻回率５０ＷＲ               PIC 9(2)    VALUE ZERO.
001410      05 部位合計５０ＷＲ                 PIC 9(6)    VALUE ZERO.
005200****************
005210* ６部位／割 *
005220****************
005230   03 部位６ＷＲ.
001780      05 逓減開始月日６ＷＲ.
001790         07 逓減開始月６ＷＲ            PIC 9(2).
001800         07 逓減開始日６ＷＲ            PIC 9(2).
005240      05 後療６ＷＲ.
005250         07 後療回数６ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 後療料６ＷＲ                PIC 9(5)    VALUE ZERO.
005260      05 冷罨法６ＷＲ.
005270         07 冷罨法回数６ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 冷罨法料６ＷＲ              PIC 9(5)    VALUE ZERO.
005280      05 温罨法６ＷＲ.
005290         07 温罨法回数６ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 温罨法料６ＷＲ              PIC 9(5)    VALUE ZERO.
005300      05 電療６ＷＲ.
005310         07 電療回数６ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 電療料６ＷＲ                PIC 9(5)    VALUE ZERO.
001390      05 小計６ＷＲ                     PIC 9(6)    VALUE ZERO.
004580      05 多部位６ＷＲ.
004590         07 多部位率６ＷＲ              PIC 9(2)    VALUE ZERO.
004530         07 多部位料６ＷＲ              PIC 9(5)    VALUE ZERO.
004590      05 長期率６ＷＲ                   PIC 9(2)    VALUE ZERO.
004590      05 長期頻回率６ＷＲ               PIC 9(2)    VALUE ZERO.
001410      05 部位合計６ＷＲ                 PIC 9(6)    VALUE ZERO.
005320******************
005330* ６部位／１０割 *
005340******************
005350   03 部位６０ＷＲ.
001780      05 逓減開始月日６０ＷＲ.
001790         07 逓減開始月６０ＷＲ            PIC 9(2).
001800         07 逓減開始日６０ＷＲ            PIC 9(2).
005360      05 後療６０ＷＲ.
005370         07 後療回数６０ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 後療料６０ＷＲ                PIC 9(5)    VALUE ZERO.
005380      05 冷罨法６０ＷＲ.
005390         07 冷罨法回数６０ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 冷罨法料６０ＷＲ              PIC 9(5)    VALUE ZERO.
005400      05 温罨法６０ＷＲ.
005410         07 温罨法回数６０ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 温罨法料６０ＷＲ              PIC 9(5)    VALUE ZERO.
005420      05 電療６０ＷＲ.
005430         07 電療回数６０ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 電療料６０ＷＲ                PIC 9(5)    VALUE ZERO.
001390      05 小計６０ＷＲ                     PIC 9(6)    VALUE ZERO.
004590      05 長期率６０ＷＲ                   PIC 9(2)    VALUE ZERO.
004590      05 長期頻回率６０ＷＲ               PIC 9(2)    VALUE ZERO.
001410      05 部位合計６０ＷＲ                 PIC 9(6)    VALUE ZERO.
005200****************
005210* ７部位／割 *
005220****************
005230   03 部位７ＷＲ.
001780      05 逓減開始月日７ＷＲ.
001790         07 逓減開始月７ＷＲ            PIC 9(2).
001800         07 逓減開始日７ＷＲ            PIC 9(2).
005240      05 後療７ＷＲ.
005250         07 後療回数７ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 後療料７ＷＲ                PIC 9(5)    VALUE ZERO.
005260      05 冷罨法７ＷＲ.
005270         07 冷罨法回数７ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 冷罨法料７ＷＲ              PIC 9(5)    VALUE ZERO.
005280      05 温罨法７ＷＲ.
005290         07 温罨法回数７ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 温罨法料７ＷＲ              PIC 9(5)    VALUE ZERO.
005300      05 電療７ＷＲ.
005310         07 電療回数７ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 電療料７ＷＲ                PIC 9(5)    VALUE ZERO.
001390      05 小計７ＷＲ                     PIC 9(6)    VALUE ZERO.
004580      05 多部位７ＷＲ.
004590         07 多部位率７ＷＲ              PIC 9(2)    VALUE ZERO.
004530         07 多部位料７ＷＲ              PIC 9(5)    VALUE ZERO.
004590      05 長期率７ＷＲ                   PIC 9(2)    VALUE ZERO.
004590      05 長期頻回率７ＷＲ               PIC 9(2)    VALUE ZERO.
001410      05 部位合計７ＷＲ                 PIC 9(6)    VALUE ZERO.
005320******************
005330* ７部位／１０割 *
005340******************
005350   03 部位７０ＷＲ.
001780      05 逓減開始月日７０ＷＲ.
001790         07 逓減開始月７０ＷＲ            PIC 9(2).
001800         07 逓減開始日７０ＷＲ            PIC 9(2).
005360      05 後療７０ＷＲ.
005370         07 後療回数７０ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 後療料７０ＷＲ                PIC 9(5)    VALUE ZERO.
005380      05 冷罨法７０ＷＲ.
005390         07 冷罨法回数７０ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 冷罨法料７０ＷＲ              PIC 9(5)    VALUE ZERO.
005400      05 温罨法７０ＷＲ.
005410         07 温罨法回数７０ＷＲ            PIC 9(2)  VALUE ZERO.
004530         07 温罨法料７０ＷＲ              PIC 9(5)    VALUE ZERO.
005420      05 電療７０ＷＲ.
005430         07 電療回数７０ＷＲ              PIC 9(2)  VALUE ZERO.
004530         07 電療料７０ＷＲ                PIC 9(5)    VALUE ZERO.
001390      05 小計７０ＷＲ                     PIC 9(6)    VALUE ZERO.
004590      05 長期率７０ＷＲ                   PIC 9(2)    VALUE ZERO.
004590      05 長期頻回率７０ＷＲ               PIC 9(2)    VALUE ZERO.
001410      05 部位合計７０ＷＲ                 PIC 9(6)    VALUE ZERO.
006160*
       01 集計Ｗ.
          03 請求年月Ｗ                        PIC 9(6).
          03 会員番号２Ｗ                      PIC 9(9).
          03 協県内本件数Ｗ                    PIC 9(4).
          03 協県内本日数Ｗ                    PIC 9(5).
          03 協県内本合計額Ｗ                  PIC 9(9).
          03 協県内本請求額Ｗ                  PIC 9(9).
          03 協返戻県内本件数Ｗ                PIC 9(4).
          03 協返戻県内本日数Ｗ                PIC 9(5).
          03 協返戻県内本合計額Ｗ              PIC 9(9).
          03 協返戻県内本請求額Ｗ              PIC 9(9).
          03 協県内家件数Ｗ                    PIC 9(4).
          03 協県内家日数Ｗ                    PIC 9(5).
          03 協県内家合計額Ｗ                  PIC 9(9).
          03 協県内家請求額Ｗ                  PIC 9(9).
          03 協返戻県内家件数Ｗ                PIC 9(4).
          03 協返戻県内家日数Ｗ                PIC 9(5).
          03 協返戻県内家合計額Ｗ              PIC 9(9).
          03 協返戻県内家請求額Ｗ              PIC 9(9).
          03 協県外本件数Ｗ                    PIC 9(4).
          03 協県外本日数Ｗ                    PIC 9(5).
          03 協県外本合計額Ｗ                  PIC 9(9).
          03 協県外本請求額Ｗ                  PIC 9(9).
          03 協返戻県外本件数Ｗ                PIC 9(4).
          03 協返戻県外本日数Ｗ                PIC 9(5).
          03 協返戻県外本合計額Ｗ              PIC 9(9).
          03 協返戻県外本請求額Ｗ              PIC 9(9).
          03 協県外家件数Ｗ                    PIC 9(4).
          03 協県外家日数Ｗ                    PIC 9(5).
          03 協県外家合計額Ｗ                  PIC 9(9).
          03 協県外家請求額Ｗ                  PIC 9(9).
          03 協返戻県外家件数Ｗ                PIC 9(4).
          03 協返戻県外家日数Ｗ                PIC 9(5).
          03 協返戻県外家合計額Ｗ              PIC 9(9).
          03 協返戻県外家請求額Ｗ              PIC 9(9).
          03 組県内本件数Ｗ                    PIC 9(4).
          03 組県内本日数Ｗ                    PIC 9(5).
          03 組県内本合計額Ｗ                  PIC 9(9).
          03 組県内本請求額Ｗ                  PIC 9(9).
          03 組返戻県内本件数Ｗ                PIC 9(4).
          03 組返戻県内本日数Ｗ                PIC 9(5).
          03 組返戻県内本合計額Ｗ              PIC 9(9).
          03 組返戻県内本請求額Ｗ              PIC 9(9).
          03 組県内家件数Ｗ                    PIC 9(4).
          03 組県内家日数Ｗ                    PIC 9(5).
          03 組県内家合計額Ｗ                  PIC 9(9).
          03 組県内家請求額Ｗ                  PIC 9(9).
          03 組返戻県内家件数Ｗ                PIC 9(4).
          03 組返戻県内家日数Ｗ                PIC 9(5).
          03 組返戻県内家合計額Ｗ              PIC 9(9).
          03 組返戻県内家請求額Ｗ              PIC 9(9).
          03 組県外本件数Ｗ                    PIC 9(4).
          03 組県外本日数Ｗ                    PIC 9(5).
          03 組県外本合計額Ｗ                  PIC 9(9).
          03 組県外本請求額Ｗ                  PIC 9(9).
          03 組返戻県外本件数Ｗ                PIC 9(4).
          03 組返戻県外本日数Ｗ                PIC 9(5).
          03 組返戻県外本合計額Ｗ              PIC 9(9).
          03 組返戻県外本請求額Ｗ              PIC 9(9).
          03 組県外家件数Ｗ                    PIC 9(4).
          03 組県外家日数Ｗ                    PIC 9(5).
          03 組県外家合計額Ｗ                  PIC 9(9).
          03 組県外家請求額Ｗ                  PIC 9(9).
          03 組返戻県外家件数Ｗ                PIC 9(4).
          03 組返戻県外家日数Ｗ                PIC 9(5).
          03 組返戻県外家合計額Ｗ              PIC 9(9).
          03 組返戻県外家請求額Ｗ              PIC 9(9).
          03 共本件数Ｗ                        PIC 9(4).
          03 共本日数Ｗ                        PIC 9(5).
          03 共本合計額Ｗ                      PIC 9(9).
          03 共本請求額Ｗ                      PIC 9(9).
          03 共返戻本件数Ｗ                    PIC 9(4).
          03 共返戻本日数Ｗ                    PIC 9(5).
          03 共返戻本合計額Ｗ                  PIC 9(9).
          03 共返戻本請求額Ｗ                  PIC 9(9).
          03 共家件数Ｗ                        PIC 9(4).
          03 共家日数Ｗ                        PIC 9(5).
          03 共家合計額Ｗ                      PIC 9(9).
          03 共家請求額Ｗ                      PIC 9(9).
          03 共返戻家件数Ｗ                    PIC 9(4).
          03 共返戻家日数Ｗ                    PIC 9(5).
          03 共返戻家合計額Ｗ                  PIC 9(9).
          03 共返戻家請求額Ｗ                  PIC 9(9).
          03 国県内件数Ｗ                      PIC 9(4).
          03 国県内日数Ｗ                      PIC 9(5).
          03 国県内合計額Ｗ                    PIC 9(9).
          03 国県内請求額Ｗ                    PIC 9(9).
          03 国返戻県内件数Ｗ                  PIC 9(4).
          03 国返戻県内日数Ｗ                  PIC 9(5).
          03 国返戻県内合計額Ｗ                PIC 9(9).
          03 国返戻県内請求額Ｗ                PIC 9(9).
          03 国県外件数Ｗ                      PIC 9(4).
          03 国県外日数Ｗ                      PIC 9(5).
          03 国県外合計額Ｗ                    PIC 9(9).
          03 国県外請求額Ｗ                    PIC 9(9).
          03 国返戻県外件数Ｗ                  PIC 9(4).
          03 国返戻県外日数Ｗ                  PIC 9(5).
          03 国返戻県外合計額Ｗ                PIC 9(9).
          03 国返戻県外請求額Ｗ                PIC 9(9).
          03 後県内件数Ｗ                      PIC 9(4).
          03 後県内日数Ｗ                      PIC 9(5).
          03 後県内合計額Ｗ                    PIC 9(9).
          03 後県内請求額Ｗ                    PIC 9(9).
          03 後返戻県内件数Ｗ                  PIC 9(4).
          03 後返戻県内日数Ｗ                  PIC 9(5).
          03 後返戻県内合計額Ｗ                PIC 9(9).
          03 後返戻県内請求額Ｗ                PIC 9(9).
          03 後県外件数Ｗ                      PIC 9(4).
          03 後県外日数Ｗ                      PIC 9(5).
          03 後県外合計額Ｗ                    PIC 9(9).
          03 後県外請求額Ｗ                    PIC 9(9).
          03 後返戻県外件数Ｗ                  PIC 9(4).
          03 後返戻県外日数Ｗ                  PIC 9(5).
          03 後返戻県外合計額Ｗ                PIC 9(9).
          03 後返戻県外請求額Ｗ                PIC 9(9).
          03 助成件数Ｗ                        PIC 9(4).
          03 助成日数Ｗ                        PIC 9(5).
          03 助成合計額Ｗ                      PIC 9(9).
          03 助成請求額Ｗ                      PIC 9(9).
          03 助成返戻件数Ｗ                    PIC 9(4).
          03 助成返戻日数Ｗ                    PIC 9(5).
          03 助成返戻合計額Ｗ                  PIC 9(9).
          03 助成返戻請求額Ｗ                  PIC 9(9).
          03 生保件数Ｗ                        PIC 9(4).
          03 生保日数Ｗ                        PIC 9(5).
          03 生保合計額Ｗ                      PIC 9(9).
          03 生保請求額Ｗ                      PIC 9(9).
          03 生保返戻件数Ｗ                    PIC 9(4).
          03 生保返戻日数Ｗ                    PIC 9(5).
          03 生保返戻合計額Ｗ                  PIC 9(9).
          03 生保返戻請求額Ｗ                  PIC 9(9).
          03 労災件数Ｗ                        PIC 9(4).
          03 労災日数Ｗ                        PIC 9(5).
          03 労災合計額Ｗ                      PIC 9(9).
          03 労災請求額Ｗ                      PIC 9(9).
          03 労災返戻件数Ｗ                    PIC 9(4).
          03 労災返戻日数Ｗ                    PIC 9(5).
          03 労災返戻合計額Ｗ                  PIC 9(9).
          03 労災返戻請求額Ｗ                  PIC 9(9).
          03 予備Ｗ                            PIC X(265).
      *   
006170*****************************************************************
006180 01 計算機西暦年Ｗ                     PIC 9(2).
006190* 日付ＷＯＲＫ
006200 01 計算機西暦.
006210    03 計算機西暦年                    PIC 9(4).
006220    03 計算機西暦月日                  PIC 9(4).
006230 01 計算機西暦Ｒ REDEFINES 計算機西暦.
006240    03 計算機世紀                      PIC 9(2).
006250    03 計算機日付                      PIC 9(6).
006260    03 計算機日付Ｒ REDEFINES 計算機日付.
006270       05 計算機年月                   PIC 9(4).
006280       05 計算機年月Ｒ REDEFINES 計算機年月.
006290         07 計算機年                   PIC 9(2).
006300         07 計算機月                   PIC 9(2).
006310       05 計算機日                     PIC 9(2).
006320*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
006330******************************************************************
006340*                          連結項目                              *
006350******************************************************************
006360*
006370********************
006380* メッセージ表示キー *
006390********************
006400 01 連メ－キー IS EXTERNAL.
006410    03  連メ－メッセージ               PIC N(20).
006420*
006430 01 連メ３－キー IS EXTERNAL.
006440    03  連メ３－メッセージ             PIC N(20).
006450    03  連メ３－メッセージ１           PIC X(20).
006460*
006470****************
006480* 画面入力情報 *
006490****************
       01 連入－入力データＹＧＮ５８０ IS EXTERNAL GLOBAL.
          03 連入－施術和暦年月.
             05 連入－施術和暦               PIC 9(1).
             05 連入－施術年月.
                07 連入－施術年              PIC 9(2).
                07 連入－施術月              PIC 9(2).
       01 連入－入力データＹＧＮ５８５ IS EXTERNAL GLOBAL.
          03 連入－請求和暦年月日.
             05 連入－請求和暦               PIC 9(1).
             05 連入－請求年                 PIC 9(2).
             05 連入－請求月                 PIC 9(2).
             05 連入－請求日                 PIC 9(2).
          03 連入－保険者番号                PIC X(10).
006470****************
006480* 出力情報     *
006490****************
       01 連印－データ情報ＹＧＮ１０１ IS EXTERNAL.
          03 連印－請求年月                  PIC 9(6).
001360    03 連印－会員番号                  PIC 9(9).
001360    03 連印－機関コード                PIC 9(10).
          03 連印－登録記号番号区分          PIC 9.
          03 連印－登録記号番号              PIC X(11).
          03 連印－合計件数                  PIC 9(5).
          03 連印－合計請求額                PIC 9(9).
006560*
009851************************
009852* 長期理由文セット     *
009853************************
009854 01 連長文－キー IS EXTERNAL.
009855    03 連長文－施術年月.
009856       05 連長文－施術和暦               PIC 9.
009857       05 連長文－施術年                 PIC 9(2).
009858       05 連長文－施術月                 PIC 9(2).
009859    03  連長文－患者コード.
009860       05 連長文－患者番号               PIC 9(6).
009861       05 連長文－枝番                   PIC X.
009862    03 連長文－文桁数                    PIC 9(2).
009863    03 連長文－理由文                    PIC N(63) OCCURS 15.
009864*
009865************************
009866* 助成レセまとめ
009867************************
009868 01 連レセまとめ－キー IS EXTERNAL.
009869    03 連レセまとめ－施術和暦年月.
009870       05 連レセまとめ－施術和暦               PIC 9.
009871       05 連レセまとめ－施術年月.
009872          07 連レセまとめ－施術年              PIC 9(2).
009873          07 連レセまとめ－施術月              PIC 9(2).
009874    03 連レセまとめ－患者コード.
009875       05 連レセまとめ－患者番号               PIC 9(6).
009876       05 連レセまとめ－枝番                   PIC X(1).
009877**-------------------------------------------------------**
009878*   1:助成レセプトなしの本体まとめの判定
009879*   2:横浜・川崎用の社保助成レセかの判定
009880    03 連レセまとめ－判定区分                  PIC 9.
009881**-------------------------------------------------------**
009882*  / OUT /　 0:対象外、1:対象
009883    03 連レセまとめ－判定結果                  PIC 9.
009884**
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
009885*
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
009886******************************************************************
009887*                      PROCEDURE  DIVISION                       *
009888******************************************************************
009890 PROCEDURE               DIVISION.
009900************
009910*           *
009920* 初期処理   *
009930*           *
009940************
009950     PERFORM 初期化.
009960     PERFORM 制御情報取得.
009970************
009980*           *
009990* 主処理     *
010000*           *
010010************
010020     PERFORM 作業ファイル１作成.
010020     PERFORM 作業ファイル２作成.
010030************
010040*           *
010050* 終了処理   *
010060*           *
010070************
010080     PERFORM 終了処理.
010090     MOVE ZERO TO PROGRAM-STATUS.
010100     EXIT PROGRAM.
010110*
010120*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
010130*================================================================*
010140 初期化 SECTION.
010150*
010160     PERFORM ファイルオープン.
010170* 連結項目の待避
010180     MOVE 連入－施術和暦  TO 請求和暦ＷＲ.
010190     MOVE 連入－施術年    TO 請求年ＷＲ.
010200     MOVE 連入－施術月    TO 請求月ＷＲ.
010210*
010220     MOVE ZERO            TO 連番Ｗ.
      *
           INITIALIZE 連印－データ情報ＹＧＮ１０１.
010230*
010240* 西暦請求年月の取得
010250     MOVE ZERO          TO 西暦年月Ｗ  西暦請求年月Ｗ.
010260     MOVE 請求和暦ＷＲ  TO 元－元号区分.
010270     READ 元号マスタ
010280     NOT INVALID KEY
010290         MOVE 元－開始西暦年 TO 西暦年Ｗ
010300     END-READ.
010310*
010320     IF 西暦年Ｗ = ZERO
010330          MOVE  NC"元号マスタに開始西暦年を登録して下さい" TO 連メ－メッセージ
010340          CALL   "MSG001"
010350          CANCEL "MSG001"
010360          PERFORM ファイル閉鎖
010370          MOVE 99 TO PROGRAM-STATUS
010380          EXIT PROGRAM
010390     ELSE
010400          COMPUTE 西暦年Ｗ = 西暦年Ｗ + 請求年ＷＲ - 1
010410          MOVE 請求月ＷＲ TO 西暦月Ｗ
010420     END-IF.
010430*
010440     MOVE 西暦年月Ｗ   TO  西暦請求年月Ｗ 連印－請求年月.
010450*
      */20250120↓↓↓
           COMPUTE 連印－請求年月 = 連印－請求年月 + 1.
           IF 連印－請求年月(5:2) > 12
               MOVE 01 TO 連印－請求年月(5:2)
               COMPUTE 連印－請求年月 = 連印－請求年月 + 100
           END-IF.
      *
010460*================================================================*
010470 ファイルオープン SECTION.
010480*
010481     OPEN INPUT 制御情報マスタ.
010482         MOVE NC"制御情報" TO ファイル名.
010483         PERFORM オープンチェック.
010490     OPEN INPUT 元号マスタ.
010500         MOVE NC"元号マスタ" TO ファイル名.
010510         PERFORM オープンチェック.
010520     OPEN INPUT 名称マスタ.
010530         MOVE NC"名称マスタ" TO ファイル名.
010540         PERFORM オープンチェック.
010550     OPEN INPUT 施術所情報マスタ
010560         MOVE NC"施情" TO ファイル名.
010570         PERFORM オープンチェック.
010580     OPEN INPUT 施術記録Ｆ.
010590         MOVE NC"施術記録Ｆ" TO ファイル名.
010600         PERFORM オープンチェック.
010610     OPEN INPUT 受診者情報Ｆ.
010620         MOVE NC"受診者情報Ｆ" TO ファイル名.
010630         PERFORM オープンチェック.
010640     OPEN INPUT 負傷データＦ.
010650         MOVE NC"負傷データＦ" TO ファイル名.
010660         PERFORM オープンチェック.
016000     OPEN INPUT   経過マスタ
016010         MOVE NC"経過" TO ファイル名.
016020         PERFORM オープンチェック.
010661     OPEN INPUT 負傷原因Ｆ.
010662         MOVE NC"負傷原因" TO ファイル名.
010663         PERFORM オープンチェック.
006630     OPEN INPUT レセプトＦ.
006640         MOVE NC"レセ" TO ファイル名.
006650         PERFORM オープンチェック.
003001     OPEN INPUT 会計データＦ.
003002         MOVE NC"会計" TO ファイル名.
003003         PERFORM オープンチェック.
           OPEN INPUT 計算マスタ.
               MOVE NC"計算" TO ファイル名.
               PERFORM オープンチェック.
005350     OPEN INPUT 長期継続者Ｆ.
005360         MOVE NC"長継" TO ファイル名.
005370         PERFORM オープンチェック.
014860     OPEN INPUT   ＩＤ管理マスタ.
014870         MOVE NC"ＩＤ" TO ファイル名.
014880         PERFORM オープンチェック.
006630     OPEN INPUT 生保情報Ｆ.
006640         MOVE NC"生保" TO ファイル名.
006650         PERFORM オープンチェック.
010730*
010740*================================================================*
010750 オープンチェック SECTION.
010760*
010770     IF 状態キー  NOT =  "00"
010780         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
010790         DISPLAY NC"状態キー：" 状態キー         UPON CONS
010800         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
010810                                                 UPON CONS
004380*-----------------------------------------*
004390         CALL "actcshm"  WITH C LINKAGE
004400*-----------------------------------------*
010820         ACCEPT  キー入力 FROM CONS
010830         PERFORM ファイル閉鎖
010840         MOVE 99 TO PROGRAM-STATUS
010850         EXIT PROGRAM.
010860*================================================================*
010870 ファイル閉鎖 SECTION.
010880*
010890     CLOSE 制御情報マスタ 元号マスタ   名称マスタ  受診者情報Ｆ
010891           負傷データＦ   負傷原因Ｆ   施術記録Ｆ  施術所情報マスタ
010901           会計データＦ   レセプトＦ   経過マスタ  長期継続者Ｆ
                 計算マスタ     生保情報Ｆ   ＩＤ管理マスタ.
010910*================================================================*
010920 終了処理 SECTION.
010930*
010940     PERFORM ファイル閉鎖.
010941*================================================================*
010942 エラー表示Ｒ SECTION.
010943*
010944     DISPLAY NC"ファイル読込エラー" ファイル名     UPON CONS.
010945     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
010946     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
010947     ACCEPT  キー入力 FROM CONS.
010948     PERFORM ファイル閉鎖.
010949     MOVE 99 TO PROGRAM-STATUS.
010950     EXIT PROGRAM.
010951*================================================================*
010960 エラー表示 SECTION.
010970*
010980     DISPLAY NC"状態キー" 状態キー  UPON CONS.
010990     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
011000     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
011010     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
011020     ACCEPT  キー入力 FROM CONS.
011030     PERFORM ファイル閉鎖.
011040     MOVE 99 TO PROGRAM-STATUS.
011050     EXIT PROGRAM.
011051*================================================================*
011052 制御情報取得 SECTION.
011053*
011054     MOVE ZEROS TO 制－制御区分.
011055     READ 制御情報マスタ
011056     NOT INVALID KEY
011058         MOVE 制－レセ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
011059         MOVE 制－レセ長期理由印刷区分 TO 長期理由印刷区分Ｗ
011062     END-READ.
011063*
012420*================================================================*
009510 施術所情報取得 SECTION.
009520*
009530     MOVE ZERO  TO 施情－施術所番号.
009540     READ 施術所情報マスタ
009550     INVALID KEY
009560          MOVE  NC"施術所情報マスタに登録後、実行して下さい" TO 連メ－メッセージ
009570          CALL   "MSG001"
009580          CANCEL "MSG001"
009590          PERFORM ファイル閉鎖
009600          MOVE 99 TO PROGRAM-STATUS
009610          EXIT PROGRAM
008970     NOT INVALID KEY
005250          MOVE 施情－接骨師会会員番号(1:9)   TO 会員番号Ｗ
005260          PERFORM 会員番号右詰め
                MOVE 会員番号数字Ｗ           TO 作１－会員番号 連印－会員番号
005250          EVALUATE 施情－新柔整師番号(1:2)
                WHEN "協"
                    MOVE 1 TO 連印－登録記号番号区分
                WHEN "契"
                    MOVE 2 TO 連印－登録記号番号区分
                END-EVALUATE
005250          MOVE 施情－新柔整師番号(3:11) TO 連印－登録記号番号
009620     END-READ.
      *
026770*********************************************
026780** ＩＤ管理マスタより　県施術ＩＤを取得する。
026790*********************************************
026880     MOVE 10                     TO ＩＤ管－保険種別.
026900**   / 県施術ID /
026910     MOVE 01                     TO ＩＤ管－ＩＤ区分.
026920     MOVE ZERO                   TO ＩＤ管－施術所番号.
026940     MOVE SPACE                  TO ＩＤ管－保険者番号.
026950     READ ＩＤ管理マスタ
026960     NOT INVALID KEY
005260          PERFORM ＩＤ番号右詰め
               MOVE 施術ＩＤ番号Ｗ         TO 連印－機関コード
026980     END-READ.
009630*
011220*================================================================*
011230 会員番号右詰め SECTION.
011240*
011250     MOVE 会員番号Ｗ      TO  会員番号左詰めＷ.
011260     MOVE ZERO            TO  会員番号右詰めＷ.
011270     MOVE ZERO            TO  会員番号数字Ｗ.
011280*
011290     MOVE  8  TO  カウンタ.
011300*
011350     IF  会員番号左詰めＷ１(7) NOT = SPACE
011360         COMPUTE カウンタ = カウンタ  -  1
011370         MOVE 会員番号左詰めＷ１(7)  TO  会員番号右詰めＷ１(カウンタ)
011380     END-IF.
011350     IF  会員番号左詰めＷ１(6) NOT = SPACE
011360         COMPUTE カウンタ = カウンタ  -  1
011370         MOVE 会員番号左詰めＷ１(6)  TO  会員番号右詰めＷ１(カウンタ)
011380     END-IF.
011390     IF  会員番号左詰めＷ１(5) NOT = SPACE
011400         COMPUTE カウンタ = カウンタ  -  1
011410         MOVE 会員番号左詰めＷ１(5)  TO  会員番号右詰めＷ１(カウンタ)
011420     END-IF.
011430     IF  会員番号左詰めＷ１(4) NOT = SPACE
011440         COMPUTE カウンタ = カウンタ  -  1
011450         MOVE 会員番号左詰めＷ１(4)  TO  会員番号右詰めＷ１(カウンタ)
011460     END-IF.
011470     IF  会員番号左詰めＷ１(3) NOT = SPACE
011480         COMPUTE カウンタ = カウンタ  -  1
011490         MOVE 会員番号左詰めＷ１(3)  TO  会員番号右詰めＷ１(カウンタ)
011500     END-IF.
011510     IF  会員番号左詰めＷ１(2) NOT = SPACE
011520         COMPUTE カウンタ = カウンタ  -  1
011530         MOVE 会員番号左詰めＷ１(2)  TO  会員番号右詰めＷ１(カウンタ)
011540     END-IF.
011550     IF  会員番号左詰めＷ１(1) NOT = SPACE
011560         COMPUTE カウンタ = カウンタ  -  1
011570         MOVE 会員番号左詰めＷ１(1)  TO  会員番号右詰めＷ１(カウンタ)
011580     END-IF.
011590*
011600     MOVE 会員番号右詰めＷ TO 会員番号数字Ｗ.
011610*
011220*================================================================*
011230 ＩＤ番号右詰め SECTION.
011240*
011250     MOVE ＩＤ管－施術ＩＤ番号(3:8)  TO  ＩＤ番号左詰めＷ.
011260     MOVE ZERO                       TO  ＩＤ番号右詰めＷ.
011270     MOVE ZERO                       TO  施術ＩＤ番号Ｗ.
011280*
011290     MOVE  11  TO  カウンタ.
011300*
011350     IF  ＩＤ番号左詰めＷ１(10) NOT = SPACE
011360         COMPUTE カウンタ = カウンタ  -  1
011370         MOVE ＩＤ番号左詰めＷ１(10)  TO  ＩＤ番号右詰めＷ１(カウンタ)
011380     END-IF.
011350     IF  ＩＤ番号左詰めＷ１(9) NOT = SPACE
011360         COMPUTE カウンタ = カウンタ  -  1
011370         MOVE ＩＤ番号左詰めＷ１(9)  TO  ＩＤ番号右詰めＷ１(カウンタ)
011380     END-IF.
011350     IF  ＩＤ番号左詰めＷ１(8) NOT = SPACE
011360         COMPUTE カウンタ = カウンタ  -  1
011370         MOVE ＩＤ番号左詰めＷ１(8)  TO  ＩＤ番号右詰めＷ１(カウンタ)
011380     END-IF.
011350     IF  ＩＤ番号左詰めＷ１(7) NOT = SPACE
011360         COMPUTE カウンタ = カウンタ  -  1
011370         MOVE ＩＤ番号左詰めＷ１(7)  TO  ＩＤ番号右詰めＷ１(カウンタ)
011380     END-IF.
011350     IF  ＩＤ番号左詰めＷ１(6) NOT = SPACE
011360         COMPUTE カウンタ = カウンタ  -  1
011370         MOVE ＩＤ番号左詰めＷ１(6)  TO  ＩＤ番号右詰めＷ１(カウンタ)
011380     END-IF.
011390     IF  ＩＤ番号左詰めＷ１(5) NOT = SPACE
011400         COMPUTE カウンタ = カウンタ  -  1
011410         MOVE ＩＤ番号左詰めＷ１(5)  TO  ＩＤ番号右詰めＷ１(カウンタ)
011420     END-IF.
011430     IF  ＩＤ番号左詰めＷ１(4) NOT = SPACE
011440         COMPUTE カウンタ = カウンタ  -  1
011450         MOVE ＩＤ番号左詰めＷ１(4)  TO  ＩＤ番号右詰めＷ１(カウンタ)
011460     END-IF.
011470     IF  ＩＤ番号左詰めＷ１(3) NOT = SPACE
011480         COMPUTE カウンタ = カウンタ  -  1
011490         MOVE ＩＤ番号左詰めＷ１(3)  TO  ＩＤ番号右詰めＷ１(カウンタ)
011500     END-IF.
011510     IF  ＩＤ番号左詰めＷ１(2) NOT = SPACE
011520         COMPUTE カウンタ = カウンタ  -  1
011530         MOVE ＩＤ番号左詰めＷ１(2)  TO  ＩＤ番号右詰めＷ１(カウンタ)
011540     END-IF.
011550     IF  ＩＤ番号左詰めＷ１(1) NOT = SPACE
011560         COMPUTE カウンタ = カウンタ  -  1
011570         MOVE ＩＤ番号左詰めＷ１(1)  TO  ＩＤ番号右詰めＷ１(カウンタ)
011580     END-IF.
011590*
011600     MOVE ＩＤ番号右詰めＷ TO 施術ＩＤ番号Ｗ.
011610*
012420*================================================================*
008560 データチェック SECTION.
008570*
008580     MOVE SPACE          TO 実行キーＷ.
005300     IF ( レセ－レセ印刷対象区分 NOT = 1 )
005310        MOVE "YES"  TO 実行キーＷ
005320     END-IF.
019520* *****************************************************************
019530* * レセプトＦの請求対象区分 = 0 の場合データ作成対象としない *
019540* *****************************************************************
005360     IF 実行キーＷ  = "YES"
005370*      (再度、実行キーＷ SPACE)
005380        MOVE SPACE  TO 実行キーＷ
019640        IF レセ－請求対象区分 NOT = ZERO
004090           MOVE レセ－施術和暦  TO 受－施術和暦
004100           MOVE レセ－施術年    TO 受－施術年
004110           MOVE レセ－施術月    TO 受－施術月
004120           MOVE レセ－患者番号  TO 受－患者番号
004130           MOVE レセ－枝番      TO 受－枝番
                 READ 受診者情報Ｆ
                 NOT INVALID KEY
019880               MOVE "YES"  TO 実行キーＷ
019950           END-READ
              END-IF
019960     END-IF.
009040*
020480*================================================================*
020490 料金情報取得 SECTION.
020500*
020510***********************************************
020520* 料金データセット                            *
020530***********************************************
020540*    ****************************************************************
020550*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
020560*    ****************************************************************
020570     INITIALIZE 料金１ＷＲ.
020590     INITIALIZE 料金３ＷＲ.
020600*
023200     MOVE レセ－初検料                 TO  初検料Ｗ.
023320     MOVE レセ－初検加算料             TO  初検加算Ｗ.
023300     MOVE レセ－初検時相談料           TO  相談支援料Ｗ.
023330     MOVE レセ－再検料                 TO  再検料Ｗ.
020620     MOVE レセ－往療回数               TO  往療回数Ｗ.
020621     MOVE レセ－往療距離               TO  往療距離Ｗ.
023360     MOVE レセ－往療料                 TO  往療料Ｗ.
023360     MOVE レセ－夜間                   TO  往療夜間Ｗ.
023360     MOVE レセ－難路                   TO  往療難路Ｗ.
023360     MOVE レセ－暴風雨雪               TO  往療暴風Ｗ.
023370     MOVE レセ－往療加算料             TO  往療加算Ｗ.
020622* 単位100m
020623     COMPUTE  往療距離２Ｗ  =  往療距離Ｗ * 10.
020630*
           MOVE レセ－金属副子回数           TO  金属副子回数Ｗ.
023460     MOVE レセ－金属副子加算料         TO  金属副子加算Ｗ.
023580     MOVE レセ－施術情報提供料         TO  情報提供料Ｗ.
      */運動後療追加/20180607
           MOVE レセ－運動後療回数           TO 運動後療料回数Ｗ.
           MOVE レセ－運動後療料             TO 運動後療料Ｗ.
      */明細書発行体制加算追加/20221012
           MOVE レセ－明細書発行加算料       TO 明細書発行Ｗ.
           MOVE レセ－明細書発行加算日       TO 明細書発行日Ｗ.
           IF レセ－明細書発行加算料 NOT = ZERO
               MOVE 1                        TO 明細書発行回数Ｗ
               MOVE レセ－施術月             TO 明細書発行月Ｗ
           END-IF.
022420     COMPUTE 初検料計Ｗ = レセ－小計 + レセ－運動後療料 + レセ－明細書発行加算料.
031840     MOVE レセ－初回処置料合計         TO 整復料計Ｗ.
023610********************
023620* 初回処置料セット *
023630********************
023640     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
023650             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
023660         MOVE レセ－初回処置料(部位ＣＮＴ)   TO 初回処置料Ｗ(部位ＣＮＴ)
023660         MOVE レセ－部位実日数(部位ＣＮＴ)   TO 実日数Ｗ(部位ＣＮＴ)
               MOVE レセ－部位継続月数(部位ＣＮＴ) TO 継続月数Ｗ(部位ＣＮＴ)
023670     END-PERFORM.
020640********************
020650* 逓減毎料金セット *
020660********************
020670*    **********
020680*    * １部位 *
020690*    **********
020700     MOVE レセ－後療回数１             TO 後療回数１ＷＲ.
023770     MOVE レセ－後療料１               TO 後療料１ＷＲ.
020710     MOVE レセ－冷罨法回数１           TO 冷罨法回数１ＷＲ.
023790     MOVE レセ－冷罨法料１             TO 冷罨法料１ＷＲ.
020720     MOVE レセ－温罨法回数１           TO 温罨法回数１ＷＲ.
023810     MOVE レセ－温罨法料１             TO 温罨法料１ＷＲ.
020730     MOVE レセ－電療回数１             TO 電療回数１ＷＲ.
023830     MOVE レセ－電療料１               TO 電療料１ＷＲ.
           IF レセ－長期頻回逓減率１ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率１   TO 長期頻回率１ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率１       TO 長期率１ＷＲ
           END-IF.
020760     MOVE レセ－長期込小計１           TO 部位合計１ＷＲ.
020800*    **********
020810*    * ２部位 *
020820*    **********
020830     MOVE レセ－後療回数２             TO 後療回数２ＷＲ.
023770     MOVE レセ－後療料２               TO 後療料２ＷＲ.
020840     MOVE レセ－冷罨法回数２           TO 冷罨法回数２ＷＲ.
023790     MOVE レセ－冷罨法料２             TO 冷罨法料２ＷＲ.
020850     MOVE レセ－温罨法回数２           TO 温罨法回数２ＷＲ.
023810     MOVE レセ－温罨法料２             TO 温罨法料２ＷＲ.
020860     MOVE レセ－電療回数２             TO 電療回数２ＷＲ.
023830     MOVE レセ－電療料２               TO 電療料２ＷＲ.
           IF レセ－長期頻回逓減率２ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率２   TO 長期頻回率２ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率２       TO 長期率２ＷＲ
           END-IF.
020760     MOVE レセ－長期込小計２           TO 部位合計２ＷＲ.
020930*    ****************
020940*    * ３部位／割 *
020950*    ****************
020980     MOVE レセ－後療回数３８           TO 後療回数３ＷＲ.
023770     MOVE レセ－後療料３８             TO 後療料３ＷＲ.
020990     MOVE レセ－冷罨法回数３８         TO 冷罨法回数３ＷＲ.
023790     MOVE レセ－冷罨法料３８           TO 冷罨法料３ＷＲ.
021000     MOVE レセ－温罨法回数３８         TO 温罨法回数３ＷＲ.
023810     MOVE レセ－温罨法料３８           TO 温罨法料３ＷＲ.
021010     MOVE レセ－電療回数３８           TO 電療回数３ＷＲ.
023830     MOVE レセ－電療料３８             TO 電療料３ＷＲ.
020760     MOVE レセ－多部位込小計３８       TO 多部位料３ＷＲ.
           IF レセ－長期頻回逓減率３８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率３８ TO 長期頻回率３ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率３８     TO 長期率３ＷＲ
           END-IF.
020760     MOVE レセ－長期込小計３８         TO 部位合計３ＷＲ.
021080*    ****************
021090*    * ３部位／10割 *
021100*    ****************
021110     MOVE レセ－逓減開始月日３０       TO 逓減開始月日３０ＷＲ.
021110     MOVE レセ－後療回数３０           TO 後療回数３０ＷＲ.
023770     MOVE レセ－後療料３０             TO 後療料３０ＷＲ.
021120     MOVE レセ－冷罨法回数３０         TO 冷罨法回数３０ＷＲ.
023790     MOVE レセ－冷罨法料３０           TO 冷罨法料３０ＷＲ.
021130     MOVE レセ－温罨法回数３０         TO 温罨法回数３０ＷＲ.
023810     MOVE レセ－温罨法料３０           TO 温罨法料３０ＷＲ.
021140     MOVE レセ－電療回数３０           TO 電療回数３０ＷＲ.
023830     MOVE レセ－電療料３０             TO 電療料３０ＷＲ.
           IF レセ－長期頻回逓減率３０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率３０ TO 長期頻回率３０ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率３０       TO 長期率３０ＷＲ
           END-IF.
020760     MOVE レセ－長期込小計３０         TO 部位合計３０ＷＲ.
021400*    ****************
021410*    * ４部位／割 *
021420*    ****************
021110     MOVE レセ－逓減開始月日４８       TO 逓減開始月日４ＷＲ.
021430     MOVE レセ－後療回数４８           TO 後療回数４ＷＲ.
023770     MOVE レセ－後療料４８             TO 後療料４ＷＲ.
021440     MOVE レセ－冷罨法回数４８         TO 冷罨法回数４ＷＲ.
023790     MOVE レセ－冷罨法料４８           TO 冷罨法料４ＷＲ.
021450     MOVE レセ－温罨法回数４８         TO 温罨法回数４ＷＲ.
023810     MOVE レセ－温罨法料４８           TO 温罨法料４ＷＲ.
021460     MOVE レセ－電療回数４８           TO 電療回数４ＷＲ.
023830     MOVE レセ－電療料４８             TO 電療料４ＷＲ.
020760     MOVE レセ－多部位込小計４８       TO 多部位料４ＷＲ.
           IF レセ－長期頻回逓減率４８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率４８ TO 長期頻回率４ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率４８     TO 長期率４ＷＲ
           END-IF.
020760     MOVE レセ－長期込小計４８         TO 部位合計４ＷＲ.
021530*    ****************
021540*    * ４部位／10割 *
021550*    ****************
021110     MOVE レセ－逓減開始月日４０       TO 逓減開始月日４０ＷＲ.
021560     MOVE レセ－後療回数４０           TO 後療回数４０ＷＲ.
023770     MOVE レセ－後療料４０             TO 後療料４０ＷＲ.
021570     MOVE レセ－冷罨法回数４０         TO 冷罨法回数４０ＷＲ.
023790     MOVE レセ－冷罨法料４０           TO 冷罨法料４０ＷＲ.
021580     MOVE レセ－温罨法回数４０         TO 温罨法回数４０ＷＲ.
023810     MOVE レセ－温罨法料４０           TO 温罨法料４０ＷＲ.
021590     MOVE レセ－電療回数４０           TO 電療回数４０ＷＲ.
023830     MOVE レセ－電療料４０             TO 電療料４０ＷＲ.
           IF レセ－長期頻回逓減率４０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率４０ TO 長期頻回率４０ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率４０       TO 長期率４０ＷＲ
           END-IF.
020760     MOVE レセ－長期込小計４０         TO 部位合計４０ＷＲ.
021400*    ****************
021410*    * ５部位／割 *
021420*    ****************
021110     MOVE レセ－逓減開始月日５８       TO 逓減開始月日５ＷＲ.
021430     MOVE レセ－後療回数５８           TO 後療回数５ＷＲ.
023770     MOVE レセ－後療料５８             TO 後療料５ＷＲ.
021440     MOVE レセ－冷罨法回数５８         TO 冷罨法回数５ＷＲ.
023790     MOVE レセ－冷罨法料５８           TO 冷罨法料５ＷＲ.
021450     MOVE レセ－温罨法回数５８         TO 温罨法回数５ＷＲ.
023810     MOVE レセ－温罨法料５８           TO 温罨法料５ＷＲ.
021460     MOVE レセ－電療回数５８           TO 電療回数５ＷＲ.
023830     MOVE レセ－電療料５８             TO 電療料５ＷＲ.
020760     MOVE レセ－多部位込小計５８       TO 多部位料５ＷＲ.
           IF レセ－長期頻回逓減率５８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率５８ TO 長期頻回率５ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率５８     TO 長期率５ＷＲ
           END-IF.
020760     MOVE レセ－長期込小計５８         TO 部位合計５ＷＲ.
021530*    ****************
021540*    * ５部位／10割 *
021550*    ****************
021110     MOVE レセ－逓減開始月日５０       TO 逓減開始月日５０ＷＲ.
021560     MOVE レセ－後療回数５０           TO 後療回数５０ＷＲ.
023770     MOVE レセ－後療料５０             TO 後療料５０ＷＲ.
021570     MOVE レセ－冷罨法回数５０         TO 冷罨法回数５０ＷＲ.
023790     MOVE レセ－冷罨法料５０           TO 冷罨法料５０ＷＲ.
021580     MOVE レセ－温罨法回数５０         TO 温罨法回数５０ＷＲ.
023810     MOVE レセ－温罨法料５０           TO 温罨法料５０ＷＲ.
021590     MOVE レセ－電療回数５０           TO 電療回数５０ＷＲ.
023830     MOVE レセ－電療料５０             TO 電療料５０ＷＲ.
           IF レセ－長期頻回逓減率５０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率５０ TO 長期頻回率５０ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率５０       TO 長期率５０ＷＲ
           END-IF.
020760     MOVE レセ－長期込小計５０         TO 部位合計５０ＷＲ.
021400*    ****************
021410*    * ６部位／割 *
021420*    ****************
021110     MOVE レセ－逓減開始月日６８       TO 逓減開始月日６ＷＲ.
021430     MOVE レセ－後療回数６８           TO 後療回数６ＷＲ.
023770     MOVE レセ－後療料６８             TO 後療料６ＷＲ.
021440     MOVE レセ－冷罨法回数６８         TO 冷罨法回数６ＷＲ.
023790     MOVE レセ－冷罨法料６８           TO 冷罨法料６ＷＲ.
021450     MOVE レセ－温罨法回数６８         TO 温罨法回数６ＷＲ.
023810     MOVE レセ－温罨法料６８           TO 温罨法料６ＷＲ.
021460     MOVE レセ－電療回数６８           TO 電療回数６ＷＲ.
023830     MOVE レセ－電療料６８             TO 電療料６ＷＲ.
020760     MOVE レセ－多部位込小計６８       TO 多部位料６ＷＲ.
           IF レセ－長期頻回逓減率６８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率６８ TO 長期頻回率６ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率６８     TO 長期率６ＷＲ
           END-IF.
020760     MOVE レセ－長期込小計６８         TO 部位合計６ＷＲ.
021530*    ****************
021540*    * ６部位／10割 *
021550*    ****************
021110     MOVE レセ－逓減開始月日６０       TO 逓減開始月日６０ＷＲ.
021560     MOVE レセ－後療回数６０           TO 後療回数６０ＷＲ.
023770     MOVE レセ－後療料６０             TO 後療料６０ＷＲ.
021570     MOVE レセ－冷罨法回数６０         TO 冷罨法回数６０ＷＲ.
023790     MOVE レセ－冷罨法料６０           TO 冷罨法料６０ＷＲ.
021580     MOVE レセ－温罨法回数６０         TO 温罨法回数６０ＷＲ.
023810     MOVE レセ－温罨法料６０           TO 温罨法料６０ＷＲ.
021590     MOVE レセ－電療回数６０           TO 電療回数６０ＷＲ.
023830     MOVE レセ－電療料６０             TO 電療料６０ＷＲ.
           IF レセ－長期頻回逓減率６０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率６０ TO 長期頻回率６０ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率６０     TO 長期率６０ＷＲ
           END-IF.
020760     MOVE レセ－長期込小計６０         TO 部位合計６０ＷＲ.
021400*    ****************
021410*    * ７部位／割 *
021420*    ****************
021110     MOVE レセ－逓減開始月日７８       TO 逓減開始月日７ＷＲ.
021430     MOVE レセ－後療回数７８           TO 後療回数７ＷＲ.
023770     MOVE レセ－後療料７８             TO 後療料７ＷＲ.
021440     MOVE レセ－冷罨法回数７８         TO 冷罨法回数７ＷＲ.
023790     MOVE レセ－冷罨法料７８           TO 冷罨法料７ＷＲ.
021450     MOVE レセ－温罨法回数７８         TO 温罨法回数７ＷＲ.
023810     MOVE レセ－温罨法料７８           TO 温罨法料７ＷＲ.
021460     MOVE レセ－電療回数７８           TO 電療回数７ＷＲ.
023830     MOVE レセ－電療料７８             TO 電療料７ＷＲ.
020760     MOVE レセ－多部位込小計７８       TO 多部位料７ＷＲ.
           IF レセ－長期頻回逓減率７８ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率７８ TO 長期頻回率７ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率７８     TO 長期率７ＷＲ
           END-IF.
020760     MOVE レセ－長期込小計７８         TO 部位合計７ＷＲ.
021530*    ****************
021540*    * ７部位／10割 *
021550*    ****************
021110     MOVE レセ－逓減開始月日７０       TO 逓減開始月日７０ＷＲ.
021560     MOVE レセ－後療回数７０           TO 後療回数７０ＷＲ.
023770     MOVE レセ－後療料７０             TO 後療料７０ＷＲ.
021570     MOVE レセ－冷罨法回数７０         TO 冷罨法回数７０ＷＲ.
023790     MOVE レセ－冷罨法料７０           TO 冷罨法料７０ＷＲ.
021580     MOVE レセ－温罨法回数７０         TO 温罨法回数７０ＷＲ.
023810     MOVE レセ－温罨法料７０           TO 温罨法料７０ＷＲ.
021590     MOVE レセ－電療回数７０           TO 電療回数７０ＷＲ.
023830     MOVE レセ－電療料７０             TO 電療料７０ＷＲ.
           IF レセ－長期頻回逓減率７０ NOT = ZERO
023850         MOVE レセ－長期頻回逓減率７０ TO 長期頻回率７０ＷＲ
           ELSE
024000         MOVE レセ－長期逓減率７０     TO 長期率７０ＷＲ
           END-IF.
020760     MOVE レセ－長期込小計７０         TO 部位合計７０ＷＲ.
           PERFORM 計算情報取得.
022320*
037520*================================================================*
       計算情報取得 SECTION.
      *
      * 制御区分：０１  健康保険
           MOVE 01             TO 計－制御区分.
           MOVE 施術和暦ＷＲ   TO 計－開始和暦.
           MOVE 施術年ＷＲ     TO 計－開始年.
           MOVE 施術月ＷＲ     TO 計－開始月.
      *
           START 計算マスタ KEY IS <= 計－制御区分 計－開始和暦年月
                                                           REVERSED
           END-START.
      *
           IF 状態キー = "00"
               READ 計算マスタ NEXT
               NOT AT END
                   IF ( 施術和暦年月ＷＲ >= 計Ａ－開始和暦年月 ) AND
                      ( 施術和暦年月ＷＲ <= 計Ａ－終了和暦年月 )
                       MOVE 計Ａ－多部位逓減率(3) TO 多部位率３ＷＲ
                       MOVE 計Ａ－多部位逓減率(4) TO 多部位率４ＷＲ
                   END-IF
               END-READ
           END-IF.
012900*================================================================*
022340 負傷データ取得 SECTION.
022350*
022360     INITIALIZE 負傷情報Ｗ.
022361*
022370     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
022380     MOVE 施術年ＷＲ         TO 負－施術年.
022390     MOVE 施術月ＷＲ         TO 負－施術月.
022400     MOVE 患者コードＷＲ     TO 負－患者コード.
022410     READ 負傷データＦ
022420     INVALID KEY
022430         CONTINUE
022440     NOT INVALID KEY
022450         MOVE 負－部位数                   TO 部位数Ｗ
022460         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
022470                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
022480             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別Ｗ(部位ＣＮＴ)
022490             MOVE 負－部位(部位ＣＮＴ)     TO 部位Ｗ(部位ＣＮＴ)
022500             MOVE 負－左右区分(部位ＣＮＴ) TO 左右区分Ｗ(部位ＣＮＴ)
022510             MOVE 負－負傷位置番号(部位ＣＮＴ)
022520                                           TO 負傷位置番号Ｗ(部位ＣＮＴ)
022530* 負傷種別
022540             MOVE SPACE                     TO 負傷名称Ｗ
022550             MOVE 03                        TO 名－区分コード
022560             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
022570             READ 名称マスタ
022580             INVALID KEY
022590                 MOVE SPACE        TO 負傷名称Ｗ
022600             NOT INVALID KEY
022610                 MOVE 名－正式名称 TO 負傷名称Ｗ
022620             END-READ
022630* 部位
022720             STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
022730                    負傷名称Ｗ                    DELIMITED BY SPACE
022740                    レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
022750                    INTO 負傷名Ｗ(部位ＣＮＴ)
022760             END-STRING
                   IF 負－負傷種別(部位ＣＮＴ) = 9
                      MOVE レセ－部位名称１(部位ＣＮＴ) TO 負傷名Ｗ(部位ＣＮＴ)
                   END-IF
022780*
022790             MOVE 負－負傷和暦(部位ＣＮＴ)   TO 負傷和暦Ｗ(部位ＣＮＴ)
022800             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
022810             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
022820             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
022830             MOVE 負－開始和暦(部位ＣＮＴ)   TO 初検和暦Ｗ(部位ＣＮＴ)
022840             MOVE 負－開始年(部位ＣＮＴ)   TO 初検年Ｗ(部位ＣＮＴ)
022850             MOVE 負－開始月(部位ＣＮＴ)   TO 初検月Ｗ(部位ＣＮＴ)
022860             MOVE 負－開始日(部位ＣＮＴ)   TO 初検日Ｗ(部位ＣＮＴ)
022870             IF 負－転帰区分(部位ＣＮＴ) = 9
022880                 MOVE 99                   TO 終了年Ｗ(部位ＣＮＴ)
022890                 MOVE 99                   TO 終了月Ｗ(部位ＣＮＴ)
022900                 MOVE 99                   TO 終了日Ｗ(部位ＣＮＴ)
022910             ELSE
022920                 MOVE 負－終了和暦(部位ＣＮＴ)   TO 終了和暦Ｗ(部位ＣＮＴ)
022930                 MOVE 負－終了年(部位ＣＮＴ)   TO 終了年Ｗ(部位ＣＮＴ)
022940                 MOVE 負－終了月(部位ＣＮＴ)   TO 終了月Ｗ(部位ＣＮＴ)
022950                 MOVE 負－終了日(部位ＣＮＴ)   TO 終了日Ｗ(部位ＣＮＴ)
022960             END-IF
022970*
022980             MOVE 負－転帰区分(部位ＣＮＴ) TO 転帰区分Ｗ(部位ＣＮＴ)
022990*
023000         END-PERFORM
023080* 枝番判定用
023090         MOVE 負－開始診療日手動区分 TO  開始診療日手動区分Ｗ
023100*
023110     END-READ.
023120*
023840*================================================================*
023850 施術記録取得 SECTION.
023860*
023870     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 部位数Ｗ
023880         IF ( 施術年ＷＲ = 初検年Ｗ(部位ＣＮＴ) ) AND
023890            ( 施術月ＷＲ = 初検月Ｗ(部位ＣＮＴ) )
023900             MOVE 患者番号ＷＲ          TO 施記－患者番号
023910             MOVE 枝番ＷＲ              TO 施記－枝番
023920             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
023930             MOVE 初検和暦Ｗ(部位ＣＮＴ)  TO 開始和暦Ｗ(部位ＣＮＴ)
023940             MOVE 初検年Ｗ(部位ＣＮＴ)  TO 開始年Ｗ(部位ＣＮＴ) 施記－施術年
023950             MOVE 初検月Ｗ(部位ＣＮＴ)  TO 開始月Ｗ(部位ＣＮＴ) 施記－施術月
023960             MOVE 初検日Ｗ(部位ＣＮＴ)  TO 開始日Ｗ(部位ＣＮＴ) 施記－施術日
023970         ELSE
023980             MOVE 患者番号ＷＲ          TO 施記－患者番号
023990             MOVE 枝番ＷＲ              TO 施記－枝番
024000             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
024010             MOVE 施術年ＷＲ            TO 施記－施術年
024020             MOVE 施術月ＷＲ            TO 施記－施術月
024030             MOVE ZERO                  TO 施記－施術日
024040         END-IF
024050         START 施術記録Ｆ   KEY IS >= 施記－患者コード
024060                                      施記－施術和暦年月日
024070         END-START
024080         IF 状態キー = "00"
024100             MOVE ZERO  TO 初回処置回数Ｗ(部位ＣＮＴ)
024110             MOVE ZERO  TO 終了和暦ＷＴ
024120             MOVE ZERO  TO 終了年ＷＴ
024130             MOVE ZERO  TO 終了月ＷＴ
024140             MOVE ZERO  TO 終了日ＷＴ
024150             MOVE SPACE TO 終了フラグ２
024160             PERFORM 施術記録Ｆ読込
024170             IF  ( 終了フラグ２      = SPACE   ) AND
024180                 ( 施記－患者コード  = 患者コードＷＲ ) AND
024190                 ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
024200                 ( 施記－施術年      = 施術年ＷＲ     ) AND
024210                 ( 施記－施術月      = 施術月ＷＲ     ) 
024220*
024340*        ****************
024350*        * 情報提供回数 *
024360*        ****************
024370             IF 施記－情報提供区分(部位ＣＮＴ) = 1
024380                 COMPUTE 情報提供料回数Ｗ = 情報提供料回数Ｗ + 1
024390             END-IF
024400*        *****************************************************************
024410*        * 開始年月日 ( その部位が当月初検でないか、
024420*                       当月初検でも枝番がある時は、最初の施術日を開始日)*
024430*        *****************************************************************
024440                 IF ( 施術年ＷＲ NOT = 初検年Ｗ(部位ＣＮＴ) ) OR
024450                    ( 施術月ＷＲ NOT = 初検月Ｗ(部位ＣＮＴ) ) OR
024460                    ( 開始診療日手動区分Ｗ = 1 )
024470                     MOVE 施記－施術和暦 TO 開始和暦Ｗ(部位ＣＮＴ)
024480                     MOVE 施記－施術年   TO 開始年Ｗ(部位ＣＮＴ)
024490                     MOVE 施記－施術月   TO 開始月Ｗ(部位ＣＮＴ)
024500                     MOVE 施記－施術日   TO 開始日Ｗ(部位ＣＮＴ)
024510                 END-IF
024520             END-IF
024530             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
024540                           ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
024550                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
024560                           ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
024570                           ( 施記－施術月     NOT = 施術月ＷＲ       ) OR
024580                           ( 施記－施術日         > 終了日Ｗ(部位ＣＮＴ))
024590*               **********
024600*               * 実日数 *
024610*               **********
024630                MOVE 施記－施術和暦             TO 終了和暦ＷＴ
024640                MOVE 施記－施術年               TO 終了年ＷＴ
024650                MOVE 施記－施術月               TO 終了月ＷＴ
024660                MOVE 施記－施術日               TO 終了日ＷＴ
024670*            /　初回処置のカウント　/
024680                IF 施記－整復施療区分(部位ＣＮＴ) = 1
024690                    COMPUTE 初回処置回数Ｗ(部位ＣＮＴ) = 初回処置回数Ｗ(部位ＣＮＴ) + 1
024700                END-IF
024710*
024720                PERFORM 施術記録Ｆ読込
024730            END-PERFORM
024740        END-IF
024750*       **************************
024760*       * 継続：終了年月日セット *
024770*       **************************
024780        IF 転帰区分Ｗ(部位ＣＮＴ) = 9
024790            MOVE 終了和暦ＷＴ  TO 終了和暦Ｗ(部位ＣＮＴ)
024800            MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
024810            MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
024820            MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
024830        END-IF
024840     END-PERFORM.
024841***
024850     MOVE 患者番号ＷＲ          TO 施記－患者番号.
024860     MOVE 枝番ＷＲ              TO 施記－枝番.
024870     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
024880     MOVE 施術年ＷＲ            TO 施記－施術年.
024890     MOVE 施術月ＷＲ            TO 施記－施術月.
024900     MOVE ZERO                  TO 施記－施術日.
024910     START 施術記録Ｆ   KEY IS >= 施記－患者コード
024920                                  施記－施術和暦年月日
024930     END-START.
024940     IF 状態キー = "00"
024150         MOVE SPACE TO 終了フラグ２
024950         PERFORM 施術記録Ｆ読込
024960         PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
024970                       ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
024980                       ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
024990                       ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
025000                       ( 施記－施術月     NOT = 施術月ＷＲ       )
025010*        ************
025020*        * 初検回数 *
025030*        ************
025040             IF 施記－初検料請求区分 = 1
025050                 COMPUTE 初検回数Ｗ = 初検回数Ｗ + 1
025060             END-IF
025070*        ************
025080*        * 初検加算 *
025090*        ************
025100             EVALUATE 施記－初検加算
025110             WHEN 1
025120                 COMPUTE 初検時間外回数Ｗ = 初検時間外回数Ｗ + 1
                       IF 初検時間外日Ｗ = ZERO
                           MOVE 施記－施術日   TO 初検時間外日Ｗ
                       END-IF
                       IF 初検時間外時刻Ｗ = ZERO
                           MOVE 施記－受付時   TO 時間外時Ｗ
                           MOVE 施記－受付分   TO 時間外分Ｗ
                       END-IF
025130             WHEN 2
025140                 COMPUTE 初検休日回数Ｗ   = 初検休日回数Ｗ + 1
                       IF 初検休日日Ｗ = ZERO
                           MOVE 施記－施術日   TO 初検休日日Ｗ
                       END-IF
025150             WHEN 3
025160                 COMPUTE 初検深夜回数Ｗ   = 初検深夜回数Ｗ + 1
                       IF 初検深夜日Ｗ = ZERO
                           MOVE 施記－施術日   TO 初検深夜日Ｗ
                       END-IF
                       IF 初検深夜時刻Ｗ = ZERO
                           MOVE 施記－受付時   TO 深夜時Ｗ
                           MOVE 施記－受付分   TO 深夜分Ｗ
                       END-IF
025170             END-EVALUATE
025180*        ************
025190*        * 再検回数 *
025200*        ************
025210             IF 施記－再検料請求 = 1
025220                 COMPUTE 再検回数Ｗ = 再検回数Ｗ + 1
025230             END-IF
025240*        ****************
025250*        * 初検時相談料 *
025260*        ****************
                   IF (施記－診療区分 = 2 ) AND (施記－初検時相談料区分 NOT = 1)
                       COMPUTE 相談支援回数Ｗ = 相談支援回数Ｗ + 1
                   END-IF
025240*        **********
025250*        * 施術日 *
025260*        **********
                   MOVE 1 TO 施術日Ｗ(施記－施術日)
      *
025350             PERFORM 施術記録Ｆ読込
025240*        **************
025250*        * 運動後療日 *
025260*        **************
                   IF 施記－運動療法区分 NOT = ZERO
                       MOVE 1 TO 運動後療日Ｗ(施記－施術日)
                   END-IF
025360         END-PERFORM
025370     END-IF.
025380*

027550*================================================================*
026930 負傷種別変換 SECTION.
026940*
026950     MOVE ZERO  TO 負傷種別変換後Ｗ.
026960*
026970     EVALUATE 負傷種別変換前Ｗ
026980     WHEN  ZERO
026990        MOVE ZERO TO 負傷種別変換後Ｗ
027000* 捻挫
027010     WHEN  01
027020        MOVE  4   TO 負傷種別変換後Ｗ
027030* 打撲
027040     WHEN  02
027050        MOVE  5   TO 負傷種別変換後Ｗ
027060* 挫傷
027070     WHEN  03
027080        MOVE  6   TO 負傷種別変換後Ｗ
027090* 脱臼
027100     WHEN  04
027110        MOVE  3   TO 負傷種別変換後Ｗ
027120* 骨折
027130     WHEN  05
027140        MOVE  1   TO 負傷種別変換後Ｗ
027150* 不全骨折
027160     WHEN  06
027170        MOVE  2   TO 負傷種別変換後Ｗ
027180* 骨折・不全骨折拘縮
027190     WHEN  07
027200     WHEN  08
027210        MOVE  7   TO 負傷種別変換後Ｗ
027220* 負傷名なし（無病）
027230     WHEN  09
027240        MOVE  9   TO 負傷種別変換後Ｗ
027250     WHEN OTHER
027260        CONTINUE
027270     END-EVALUATE.
027280*
027290*================================================================*
027300 転帰区分変換 SECTION.
027310*
027320     MOVE ZERO  TO 転帰変換後Ｗ.
027330*
027340     EVALUATE 転帰変換前Ｗ
027350     WHEN  ZERO
027360        MOVE ZERO TO 転帰変換後Ｗ
027370* 治癒
027380     WHEN  1
027390     WHEN  2
027400        MOVE  1   TO 転帰変換後Ｗ
027410* 中止
027420     WHEN  3
027430        MOVE  2   TO 転帰変換後Ｗ
027440* 転医
027450     WHEN  4
027460        MOVE  3   TO 転帰変換後Ｗ
027470* 継続・自然治癒
027480     WHEN  5
027490     WHEN  9
027500        MOVE  ZERO TO 転帰変換後Ｗ
027510     WHEN OTHER
027520        CONTINUE
027530     END-EVALUATE.
027540*
025390*================================================================*
012910 レセプトＦ読込 SECTION.
012920*
012930     READ レセプトＦ NEXT
012940     AT END
012950         MOVE "YES" TO 終了フラグ
012960     END-READ.
012970*
012980*================================================================*
012990 施術記録Ｆ読込 SECTION.
013000*
013010     READ 施術記録Ｆ NEXT
013020     AT END
013030         MOVE "YES"  TO 終了フラグ２
013040     END-READ.
025390*================================================================*
025820 西暦施術年月取得 SECTION.
025830* 
025840     MOVE ZERO          TO 西暦年月Ｗ  西暦施術年月Ｗ.
025850     MOVE 受－施術和暦  TO 元－元号区分.
025860     READ 元号マスタ
025870     NOT INVALID KEY
025880         MOVE 元－開始西暦年 TO 西暦年Ｗ
025890     END-READ.
025900**
025910     IF 西暦年Ｗ = ZERO
025920          MOVE  NC"元号マスタに開始西暦年を登録して下さい" TO 連メ－メッセージ
025930          CALL   "MSG001"
025940          CANCEL "MSG001"
025950          PERFORM ファイル閉鎖
025960          MOVE 99 TO PROGRAM-STATUS
025970          EXIT PROGRAM
025980     ELSE
025990          COMPUTE 西暦年Ｗ = 西暦年Ｗ + 受－施術年 - 1
026000          MOVE 受－施術月 TO 西暦月Ｗ
026010     END-IF.
026020*
026030     MOVE 西暦年月Ｗ   TO  西暦施術年月Ｗ.
026040*
026050*================================================================*
026060 西暦年月日取得 SECTION.
026070*
026080     MOVE ZERO  TO 計算西暦年月日Ｗ.
026090*
026100     IF 計算和暦Ｗ  NOT = ZERO
026110         MOVE 計算和暦Ｗ    TO 元－元号区分
026120         READ 元号マスタ
026130         NOT INVALID KEY
026140             MOVE 元－開始西暦年 TO 計算西暦年Ｗ
026150         END-READ
026160**
026170         IF 計算西暦年Ｗ = ZERO
026180              MOVE  NC"元号マスタに開始西暦年を登録して下さい３" TO 連メ－メッセージ
026190              CALL   "MSG001"
026200              CANCEL "MSG001"
026210              PERFORM ファイル閉鎖
026220              MOVE 99 TO PROGRAM-STATUS
026230              EXIT PROGRAM
026240         ELSE
026250              COMPUTE 計算西暦年Ｗ = 計算西暦年Ｗ + 計算年Ｗ - 1
026260              MOVE 計算月Ｗ TO 計算西暦月Ｗ
026270              MOVE 計算日Ｗ TO 計算西暦日Ｗ
026280         END-IF
026290     END-IF.
026300*
010440*================================================================*
010450 保険者番号右詰め SECTION.
010460*
010470     MOVE 保険者番号Ｗ    TO  保険者番号左詰めＷ.
010480     MOVE ZERO            TO  保険者番号右詰めＷ.
010490     MOVE ZERO            TO  保険者番号数字Ｗ.
010500*
010510     MOVE  9  TO  カウンタ.
010520*
010530     IF  保険者番号左詰めＷ１(8) NOT = SPACE
010540         COMPUTE カウンタ = カウンタ  -  1
010550         MOVE 保険者番号左詰めＷ１(8)  TO  保険者番号右詰めＷ１(カウンタ)
010560     END-IF.
010570     IF  保険者番号左詰めＷ１(7) NOT = SPACE
010580         COMPUTE カウンタ = カウンタ  -  1
010590         MOVE 保険者番号左詰めＷ１(7)  TO  保険者番号右詰めＷ１(カウンタ)
010600     END-IF.
010610     IF  保険者番号左詰めＷ１(6) NOT = SPACE
010620         COMPUTE カウンタ = カウンタ  -  1
010630         MOVE 保険者番号左詰めＷ１(6)  TO  保険者番号右詰めＷ１(カウンタ)
010640     END-IF.
010650     IF  保険者番号左詰めＷ１(5) NOT = SPACE
010660         COMPUTE カウンタ = カウンタ  -  1
010670         MOVE 保険者番号左詰めＷ１(5)  TO  保険者番号右詰めＷ１(カウンタ)
010680     END-IF.
010690     IF  保険者番号左詰めＷ１(4) NOT = SPACE
010700         COMPUTE カウンタ = カウンタ  -  1
010710         MOVE 保険者番号左詰めＷ１(4)  TO  保険者番号右詰めＷ１(カウンタ)
010720     END-IF.
010730     IF  保険者番号左詰めＷ１(3) NOT = SPACE
010740         COMPUTE カウンタ = カウンタ  -  1
010750         MOVE 保険者番号左詰めＷ１(3)  TO  保険者番号右詰めＷ１(カウンタ)
010760     END-IF.
010770     IF  保険者番号左詰めＷ１(2) NOT = SPACE
010780         COMPUTE カウンタ = カウンタ  -  1
010790         MOVE 保険者番号左詰めＷ１(2)  TO  保険者番号右詰めＷ１(カウンタ)
010800     END-IF.
010810     IF  保険者番号左詰めＷ１(1) NOT = SPACE
010820         COMPUTE カウンタ = カウンタ  -  1
010830         MOVE 保険者番号左詰めＷ１(1)  TO  保険者番号右詰めＷ１(カウンタ)
010840     END-IF.
010850*
010860     MOVE 保険者番号右詰めＷ TO 保険者番号数字Ｗ.
010870*
012420*================================================================*
011330 作業ファイル１作成 SECTION.
011340*
011344     OPEN OUTPUT 作業ファイル１.
011345         MOVE NC"作１" TO ファイル名.
011346         PERFORM オープンチェック.
           CLOSE 作業ファイル１.
026580     OPEN I-O 作業ファイル１.
026590         MOVE NC"作１" TO ファイル名.
026600         PERFORM オープンチェック.
026710*
           MOVE SPACE         TO 集計Ｗ.
      */20250120
           INITIALIZE 集計Ｗ.
005420     MOVE 請求和暦ＷＲ  TO レセ－請求和暦.
005430     MOVE 請求年ＷＲ    TO レセ－請求年.
005440     MOVE 請求月ＷＲ    TO レセ－請求月.
005450     MOVE 1             TO レセ－施術和暦.
005460     MOVE ZERO          TO レセ－施術年.
005470     MOVE ZERO          TO レセ－施術月.
005480     MOVE ZERO          TO レセ－患者番号.
005490     MOVE LOW-VALUE     TO レセ－枝番.
005500     MOVE ZERO          TO レセ－レセ種別.
005510     START レセプトＦ   KEY IS >= レセ－請求和暦年月
005520                                  レセ－施術和暦年月
005530                                  レセ－患者コード
005540                                  レセ－レセ種別
026830     END-START.
026840     IF 状態キー = "00"
026850         MOVE SPACE  TO 終了フラグ
005580         PERFORM レセプトＦ読込
005590         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
005600                       ( レセ－請求和暦 NOT = 請求和暦ＷＲ ) OR
005610                       ( レセ－請求年   NOT = 請求年ＷＲ   ) OR
005620                       ( レセ－請求月   NOT = 請求月ＷＲ   )
                   IF レセ－レセ種別 = 1 OR 2 OR 3 OR 7
026920                 PERFORM データチェック
026930                 IF 実行キーＷ = "YES"
004212*↓* 特別処理（助成レセまとめ）
004213                     IF 受－助成種別 NOT = ZERO
004214                         PERFORM 助成レセまとめ判定
004215                     ELSE
004216                         MOVE SPACE TO 助成レセまとめフラグ
004217                     END-IF
                           EVALUATE レセ－レセ種別
004220*
004539*            ********
004540*            * 助成 *
004550*            ********
004560                     WHEN 3
004350* 健保＋医療助成か？
004360                         IF (助成レセまとめフラグ = SPACE)
004362                             PERFORM 作１レコードセット
                                   PERFORM 集計処理
004380                             PERFORM 作１ファイル書込
004381                         END-IF
004230*            ********
004240*            * 健保 *
004250*            ********
                           WHEN OTHER
004330                         PERFORM 作１レコードセット
                               PERFORM 集計処理
004340                         PERFORM 作１ファイル書込
                           END-EVALUATE
                       END-IF
005200             END-IF
027850             PERFORM レセプトＦ読込
027860         END-PERFORM
027870     END-IF.
027880     CLOSE 作業ファイル１.
012420*================================================================*
       作１レコードセット SECTION.
      *
005933     MOVE SPACE TO 作１－レコード.
005934     INITIALIZE 作１－レコード.
           MOVE レセ－施術和暦年月 TO 施術和暦年月ＷＲ.
           MOVE レセ－患者コード   TO 患者コードＷＲ.
009961     PERFORM 施術所情報取得.
015500     PERFORM 負傷データ取得.
015510     PERFORM 料金情報取得.
015520     PERFORM 施術記録取得.
           MOVE 受－施術和暦年月 TO 計算和暦年月日Ｗ 作１－施術和暦年月.
015440     PERFORM 西暦年月日取得.
           MOVE 2                TO 作１－データ区分.
015450     MOVE 計算西暦年月日Ｗ TO 作１－施術年月.
           MOVE 受－患者コード   TO 作１－患者コード.
           MOVE 受－保険者番号   TO 作１－保険者番号 作１－保険者番号本体.
           IF レセ－レセ種別 = 7
               MOVE 受－施術和暦年月 TO 生保－施術和暦年月
               MOVE 受－患者コード   TO 生保－患者コード 
               READ 生保情報Ｆ
028490         INVALID KEY
028500             MOVE SPACE              TO 作１－保険者番号 作１－保険者番号本体
               NOT INVALID KEY
                   MOVE 生保－負担者番号   TO 作１－保険者番号 作１－保険者番号本体
028640         END-READ
           END-IF.
      *-----------------------------------------------------------------*
           MOVE SPACE TO 連暗号複合－暗号情報
      *
      *        / 連暗号複合－入力情報セット /
           MOVE 受－記号       TO 連暗号複合－記号.
           MOVE 受－番号       TO 連暗号複合－番号.
           MOVE 受－暗号化項目 TO 連暗号複合－暗号化項目.
      *
           CALL   複合プログラム名Ｗ.
           CANCEL 複合プログラム名Ｗ.
      *
           MOVE 連暗号複合－複合した記号 TO 作１－保険証記号.
           MOVE 連暗号複合－複合した番号 TO 作１－保険証番号.
      *
           MOVE ZERO TO 保険枝番Ｗ.
           IF 受－保険証枝番 NOT = SPACE
              MOVE 受－保険証枝番 TO 保険証枝番Ｗ
           END-IF.
           MOVE 保険枝番Ｗ     TO 作１－保険証枝番.
      *
           MOVE SPACE                      TO 作１－負担者番号１.
           MOVE SPACE                      TO 作１－受給者番号１.
           MOVE SPACE                      TO 作１－負担者番号２.
           MOVE SPACE                      TO 作１－受給者番号２.
           MOVE レセ－レセ種別             TO 作１－種別.
000510     IF レセ－レセ種別 = 3
               MOVE 3                      TO 作１－医療助成区分
               MOVE 受－費用負担者番号助成 TO 作１－負担者番号１
               MOVE 受－受益者番号助成     TO 作１－受給者番号１
013174         MOVE レセ－受給者負担額     TO 作１－公費一部負担金
013175         MOVE レセ－助成請求金額     TO 作１－公費請求金額
           ELSE
               MOVE 1                      TO 作１－医療助成区分
           END-IF
           MOVE 1                          TO 作１－単併区分.
           IF (受－助成種別 NOT = ZERO) AND
              (レセ－本体枝番まとめ区分 = 0)
              MOVE 2                       TO 作１－単併区分
           END-IF.
           EVALUATE 受－保険種別
           WHEN 1
               MOVE 4                  TO 作１－保険種別
           WHEN 2
           WHEN 6
           WHEN 7
               MOVE 1                  TO 作１－保険種別
           WHEN 3
               MOVE 2                  TO 作１－保険種別
           WHEN 4
           WHEN 9
               MOVE 3                  TO 作１－保険種別
           WHEN 5
               MOVE 6                  TO 作１－保険種別
           WHEN 8
               MOVE 5                  TO 作１－保険種別
           WHEN OTHER
               MOVE 9                  TO 作１－保険種別
           END-EVALUATE.
           IF 受－特別区分 = 3
               MOVE 0 TO 作１－本家区分
               IF 受－本人家族区分 = 1
                   MOVE 2 TO 作１－本家区分２
               ELSE
                   MOVE 6 TO 作１－本家区分２
               END-IF
           ELSE
               EVALUATE 受－特別区分
               WHEN 1
               WHEN 2
                   MOVE 8 TO 作１－本家区分
               WHEN 6
                   MOVE 4 TO 作１－本家区分
               END-EVALUATE
               IF 受－特別区分 = ZERO
                   IF 受－本人家族区分 = 1
                       MOVE 2 TO 作１－本家区分
                   ELSE
                       MOVE 6 TO 作１－本家区分
                   END-IF
               ELSE
                   IF 受－本人家族区分 = 1
                       MOVE 2 TO 作１－本家区分２
                   ELSE
                       MOVE 6 TO 作１－本家区分２
                   END-IF
               END-IF
           END-IF.
014125     MOVE レセ－給付割合         TO 作１－給付割合.
           MOVE 受－本人家族区分       TO 作１－本人家族区分 作１－本人家族区分２.
           MOVE 受－被保険者カナ       TO 作１－被保険者カナ.
           MOVE 受－被保険者氏名       TO 作１－被保険者氏名.
           MOVE 受－患者カナ           TO 作１－患者カナ 作１－受診者カナ.
           MOVE 受－患者氏名           TO 作１－受診者氏名.
           MOVE 受－患者性別           TO 作１－患者性別
015410* 生年月日
015420     MOVE ZERO                   TO 計算和暦年月日Ｗ.
015430     MOVE 受－患者生年月日       TO 計算和暦年月日Ｗ.
015440     PERFORM 西暦年月日取得.
015450     MOVE 計算西暦年月日Ｗ       TO 作１－患者生年月日.
013173     MOVE レセ－合計             TO 作１－合計金額
013174     MOVE レセ－一部負担金       TO 作１－一部負担金
013175     MOVE レセ－請求金額         TO 作１－請求金額
013175     MOVE レセ－レセ実日数       TO 作１－実日数
018270     MOVE 部位数Ｗ               TO 作１－部位数.
           IF レセ－請求区分 = 2
018270         MOVE 1                  TO 作１－再請求区分
           ELSE
018270         MOVE ZERO               TO 作１－再請求区分
           END-IF.
      *
           MOVE 受－特別区分           TO 作１－高齢者区分.
           IF 受－保険種別 = 05
               MOVE ZERO               TO 作１－高齢者区分
           END-IF.
           MOVE 受－患者コード         TO 作１－患者Ｎ.
      *
           PERFORM VARYING カウンタ FROM 1 BY 1
                     UNTIL カウンタ > 8
015560         MOVE 負傷種別Ｗ(カウンタ)   TO 負傷種別変換前Ｗ
015570         PERFORM 負傷種別変換
015580         MOVE 負傷種別変換後Ｗ       TO 作１－負傷区分(カウンタ)
015580         MOVE ZERO                   TO 作１－負傷コード(カウンタ)
015590         MOVE 負傷名ＷＲ(カウンタ)   TO 作１－負傷名(カウンタ)
015600*
               MOVE 初回処置回数Ｗ(カウンタ) TO 作１－初回処置回数(カウンタ)
               MOVE 初回処置料Ｗ(カウンタ)   TO 作１－初回処置料(カウンタ)
015610*
015620         MOVE ZERO                   TO 計算和暦年月日Ｗ
015630         MOVE 負傷年月日Ｗ(カウンタ) TO 計算和暦年月日Ｗ
015640         PERFORM 西暦年月日取得
015650         MOVE 計算西暦年月日Ｗ       TO 作１－負傷年月日(カウンタ)
015660*
015670         MOVE ZERO                   TO 計算和暦年月日Ｗ
015680         MOVE 初検年月日Ｗ(カウンタ) TO 計算和暦年月日Ｗ
015690         PERFORM 西暦年月日取得
015700         MOVE 計算西暦年月日Ｗ       TO 作１－初検年月日(カウンタ)
015710*
015720         MOVE ZERO                   TO 計算和暦年月日Ｗ
015730         MOVE 開始年月日Ｗ(カウンタ) TO 計算和暦年月日Ｗ
015740         PERFORM 西暦年月日取得
015750         MOVE 計算西暦年月日Ｗ       TO 作１－施術開始年月日(カウンタ)
015760*
015770         MOVE ZERO                   TO 計算和暦年月日Ｗ
015780         MOVE 終了年月日Ｗ(カウンタ) TO 計算和暦年月日Ｗ
015790         PERFORM 西暦年月日取得
015800         MOVE 計算西暦年月日Ｗ       TO 作１－施術終了年月日(カウンタ)
015810*
015820         MOVE 実日数Ｗ(カウンタ)     TO 作１－部位実日数(カウンタ)
015820         MOVE 継続月数Ｗ(カウンタ)   TO 作１－継続月数(カウンタ)
015830*
015840         MOVE 転帰区分Ｗ(カウンタ)   TO 転帰変換前Ｗ
015850         PERFORM 転帰区分変換
015860         MOVE 転帰変換後Ｗ           TO 作１－転帰区分(カウンタ)
           END-PERFORM.
      *
018330     MOVE レセ－レセ請求区分     TO 作１－請求区分.
           MOVE 施術日ＴＷ             TO 作１－施術日.
018360     MOVE 初検回数Ｗ             TO 作１－初検回数.
018360     MOVE 初検料Ｗ               TO 作１－初検料.
018370     MOVE 初検時間外回数Ｗ       TO 作１－初検時間外加算回数.
018380     MOVE 初検休日回数Ｗ         TO 作１－初検休日加算回数.
018390     MOVE 初検深夜回数Ｗ         TO 作１－初検深夜加算回数.
018370     MOVE 初検時間外日Ｗ         TO 作１－初検時間外加算日.
018370     MOVE 初検時間外時刻Ｗ       TO 作１－初検時間外加算時刻.
018380     MOVE 初検休日日Ｗ           TO 作１－初検休日加算日.
018390     MOVE 初検深夜日Ｗ           TO 作１－初検深夜加算日.
018390     MOVE 初検深夜時刻Ｗ         TO 作１－初検深夜加算時刻.
018360     MOVE 初検加算Ｗ             TO 作１－初検加算.
018370     MOVE 相談支援回数Ｗ         TO 作１－相談支援回数.
018360     MOVE 相談支援料Ｗ           TO 作１－相談支援料  .
018400     MOVE 再検回数Ｗ             TO 作１－再検回数.
018400     MOVE 再検料Ｗ               TO 作１－再検料.
018410     MOVE 往療距離２Ｗ           TO 作１－往療距離.
018420     MOVE 往療回数Ｗ             TO 作１－往療回数.
018420     MOVE 往療料Ｗ               TO 作１－往療料.
018430*
018440     MOVE 往療夜間Ｗ             TO 作１－夜間加算往療回数.
018460     MOVE 往療難路Ｗ             TO 作１－難路加算往療回数.
018450     MOVE 往療暴風Ｗ             TO 作１－暴風雨雪加算往療回数.
018420     MOVE 往療加算Ｗ             TO 作１－往療加算.
018470*
           MOVE 金属副子回数Ｗ         TO 作１－金属副子回数.
018420     MOVE 金属副子加算Ｗ         TO 作１－金属副子加算.
018510*
018520     MOVE 情報提供料回数Ｗ       TO 作１－情報提供料回数.
018520     MOVE 情報提供料Ｗ           TO 作１－情報提供料.
      *
           MOVE 運動後療料回数Ｗ       TO 作１－運動後療料回数.
           MOVE 運動後療料Ｗ           TO 作１－運動後療料.
           MOVE 明細書発行回数Ｗ       TO 作１－明細書発行回数.
           MOVE 明細書発行Ｗ           TO 作１－明細書発行.
           MOVE 明細書発行月日Ｗ       TO 作１－明細書発行月日.
      *
           MOVE 初検料計Ｗ             TO 作１－初検料計.
           MOVE 整復料計Ｗ             TO 作１－整復料計.
      *
           IF ( 施術和暦年月ＷＲ >= 43006 )
              INITIALIZE 連金運－キー
019550        MOVE 施術和暦ＷＲ   TO 連金運－施術和暦
019560        MOVE 施術年ＷＲ     TO 連金運－施術年
019570        MOVE 施術月ＷＲ     TO 連金運－施術月
019580        MOVE 患者番号ＷＲ   TO 連金運－患者番号
019590        MOVE 枝番ＷＲ       TO 連金運－枝番
              MOVE 保険種別ＷＲ   TO 連金運－保険種別
              MOVE 53             TO 連金運－会コード
              MOVE 1              TO 連金運－用紙種別
              CALL "KINUNRYO"
              CANCEL "KINUNRYO"
              PERFORM VARYING カウンタ FROM 1 BY 1
                        UNTIL カウンタ > 5
                 MOVE 連金運－金属副子和暦年月日(カウンタ 1) TO 金属月日１Ｗ(カウンタ)
                 MOVE 連金運－金属副子和暦年月日(カウンタ 2) TO 金属月日２Ｗ(カウンタ)
                 MOVE 連金運－金属副子和暦年月日(カウンタ 3) TO 金属月日３Ｗ(カウンタ)
              END-PERFORM
           END-IF.
      *
           PERFORM VARYING カウンタ FROM 1 BY 1
                     UNTIL カウンタ > 8
015620         MOVE ZERO                   TO 計算和暦年月日Ｗ
015630         MOVE 金属月日１Ｗ(カウンタ) TO 計算和暦年月日Ｗ
015640         PERFORM 西暦年月日取得
015650         MOVE 計算西暦年月日Ｗ       TO 作１－金属副子日１(カウンタ)
015620         MOVE ZERO                   TO 計算和暦年月日Ｗ
015630         MOVE 金属月日２Ｗ(カウンタ) TO 計算和暦年月日Ｗ
015640         PERFORM 西暦年月日取得
015650         MOVE 計算西暦年月日Ｗ       TO 作１－金属副子日２(カウンタ)
015620         MOVE ZERO                   TO 計算和暦年月日Ｗ
015630         MOVE 金属月日３Ｗ(カウンタ) TO 計算和暦年月日Ｗ
015640         PERFORM 西暦年月日取得
015650         MOVE 計算西暦年月日Ｗ       TO 作１－金属副子日３(カウンタ)
           END-PERFORM.
018470*
018420     MOVE 運動後療日ＴＷ         TO 作１－運動後療日.
018470*
018420     MOVE ZERO                   TO 作１－医師の同意.
      *
015960     MOVE ZERO                   TO 作１－逓減開始月日(1).
015880     MOVE 後療回数１ＷＲ         TO 作１－後療回数(1).
015880     MOVE 後療料１ＷＲ           TO 作１－後療料(1).
015900     MOVE 冷罨法回数１ＷＲ       TO 作１－冷罨法回数(1).
015900     MOVE 冷罨法料１ＷＲ         TO 作１－冷罨法料(1).
015920     MOVE 温罨法回数１ＷＲ       TO 作１－温罨法回数(1).
015920     MOVE 温罨法料１ＷＲ         TO 作１－温罨法料(1).
015940     MOVE 電療回数１ＷＲ         TO 作１－電療回数(1).
015940     MOVE 電療料１ＷＲ           TO 作１－電療料(1).
015960     MOVE ZERO                   TO 作１－多部位逓減率(1).
015960     MOVE ZERO                   TO 作１－多部位逓減額(1).
015980     MOVE 長期率１ＷＲ           TO 作１－長期逓減率(1).
015980     MOVE 長期頻回率１ＷＲ       TO 作１－長期頻回逓減率(1).
      */20250120↓↓↓
           MOVE 部位合計１ＷＲ         TO 作１－部位別料金計(1)
015990*
015960     MOVE ZERO                   TO 作１－逓減開始月日(2).
016330     MOVE 後療回数２ＷＲ         TO 作１－後療回数(2).
015880     MOVE 後療料２ＷＲ           TO 作１－後療料(2).
016350     MOVE 冷罨法回数２ＷＲ       TO 作１－冷罨法回数(2).
015900     MOVE 冷罨法料２ＷＲ         TO 作１－冷罨法料(2).
016370     MOVE 温罨法回数２ＷＲ       TO 作１－温罨法回数(2).
015920     MOVE 温罨法料２ＷＲ         TO 作１－温罨法料(2).
016390     MOVE 電療回数２ＷＲ         TO 作１－電療回数(2).
015940     MOVE 電療料２ＷＲ           TO 作１－電療料(2).
015960     MOVE ZERO                   TO 作１－多部位逓減率(2).
015960     MOVE ZERO                   TO 作１－多部位逓減額(2).
015980     MOVE 長期率２ＷＲ           TO 作１－長期逓減率(2).
015980     MOVE 長期頻回率２ＷＲ       TO 作１－長期頻回逓減率(2).
      */20250120↓↓↓
020760     MOVE 部位合計２ＷＲ         TO 作１－部位別料金計(2).
016440*
015960     MOVE ZERO                   TO 作１－逓減開始月日(3).
016780     MOVE 後療回数３ＷＲ         TO 作１－後療回数(3).
015880     MOVE 後療料３ＷＲ           TO 作１－後療料(3).
016800     MOVE 冷罨法回数３ＷＲ       TO 作１－冷罨法回数(3).
015900     MOVE 冷罨法料３ＷＲ         TO 作１－冷罨法料(3).
016820     MOVE 温罨法回数３ＷＲ       TO 作１－温罨法回数(3).
015920     MOVE 温罨法料３ＷＲ         TO 作１－温罨法料(3).
016840     MOVE 電療回数３ＷＲ         TO 作１－電療回数(3).
015940     MOVE 電療料３ＷＲ           TO 作１－電療料(3).
016860     MOVE 多部位率３ＷＲ         TO 作１－多部位逓減率(3).
016860     MOVE 多部位料３ＷＲ         TO 作１－多部位逓減額(3).
015980     MOVE 長期率３ＷＲ           TO 作１－長期逓減率(3).
015980     MOVE 長期頻回率３ＷＲ       TO 作１－長期頻回逓減率(3).
      */20250120↓↓↓
020760     MOVE 部位合計３ＷＲ         TO 作１－部位別料金計(3).
016440*
015960     MOVE 逓減開始月日３０ＷＲ   TO 作１－逓減開始月日(4).
016780     MOVE 後療回数３０ＷＲ       TO 作１－後療回数(4).
015880     MOVE 後療料３０ＷＲ         TO 作１－後療料(4).
016800     MOVE 冷罨法回数３０ＷＲ     TO 作１－冷罨法回数(4).
015900     MOVE 冷罨法料３０ＷＲ       TO 作１－冷罨法料(4).
016820     MOVE 温罨法回数３０ＷＲ     TO 作１－温罨法回数(4).
015920     MOVE 温罨法料３０ＷＲ       TO 作１－温罨法料(4).
016840     MOVE 電療回数３０ＷＲ       TO 作１－電療回数(4).
015940     MOVE 電療料３０ＷＲ         TO 作１－電療料(4).
016860     MOVE ZERO                   TO 作１－多部位逓減率(4).
016860     MOVE ZERO                   TO 作１－多部位逓減額(4).
015980     MOVE 長期率３０ＷＲ         TO 作１－長期逓減率(4).
015980     MOVE 長期頻回率３０ＷＲ     TO 作１－長期頻回逓減率(4).
      */20250120↓↓↓
020760     MOVE 部位合計３０ＷＲ       TO 作１－部位別料金計(4).
016890*
015960     MOVE 逓減開始月日４ＷＲ     TO 作１－逓減開始月日(5).
017230     MOVE 後療回数４ＷＲ         TO 作１－後療回数(5).
015880     MOVE 後療料４ＷＲ           TO 作１－後療料(5).
017250     MOVE 冷罨法回数４ＷＲ       TO 作１－冷罨法回数(5).
015900     MOVE 冷罨法料４ＷＲ         TO 作１－冷罨法料(5).
017270     MOVE 温罨法回数４ＷＲ       TO 作１－温罨法回数(5).
015920     MOVE 温罨法料４ＷＲ         TO 作１－温罨法料(5).
017290     MOVE 電療回数４ＷＲ         TO 作１－電療回数(5).
015940     MOVE 電療料４ＷＲ           TO 作１－電療料(5).
016860     MOVE 多部位率４ＷＲ         TO 作１－多部位逓減率(5).
016860     MOVE 多部位料４ＷＲ         TO 作１－多部位逓減額(5).
015980     MOVE 長期率４ＷＲ           TO 作１－長期逓減率(5).
015980     MOVE 長期頻回率４ＷＲ       TO 作１－長期頻回逓減率(5).
      */20250120↓↓↓
020760     MOVE 部位合計４ＷＲ         TO 作１－部位別料金計(5).
016890*
015960     MOVE 逓減開始月日４０ＷＲ   TO 作１－逓減開始月日(6).
017230     MOVE 後療回数４０ＷＲ       TO 作１－後療回数(6).
015880     MOVE 後療料４０ＷＲ         TO 作１－後療料(6).
017250     MOVE 冷罨法回数４０ＷＲ     TO 作１－冷罨法回数(6).
015900     MOVE 冷罨法料４０ＷＲ       TO 作１－冷罨法料(6).
017270     MOVE 温罨法回数４０ＷＲ     TO 作１－温罨法回数(6).
015920     MOVE 温罨法料４０ＷＲ       TO 作１－温罨法料(6).
017290     MOVE 電療回数４０ＷＲ       TO 作１－電療回数(6).
015940     MOVE 電療料４０ＷＲ         TO 作１－電療料(6).
016860     MOVE ZERO                   TO 作１－多部位逓減率(6).
016860     MOVE ZERO                   TO 作１－多部位逓減額(6).
015980     MOVE 長期率４０ＷＲ         TO 作１－長期逓減率(6).
015980     MOVE 長期頻回率４０ＷＲ     TO 作１－長期頻回逓減率(6).
      */20250120↓↓↓
020760     MOVE 部位合計４０ＷＲ       TO 作１－部位別料金計(6).
016890*
015960     MOVE 逓減開始月日５ＷＲ     TO 作１－逓減開始月日(7).
017230     MOVE 後療回数５ＷＲ         TO 作１－後療回数(7).
015880     MOVE 後療料５ＷＲ           TO 作１－後療料(7).
017250     MOVE 冷罨法回数５ＷＲ       TO 作１－冷罨法回数(7).
015900     MOVE 冷罨法料５ＷＲ         TO 作１－冷罨法料(7).
017270     MOVE 温罨法回数５ＷＲ       TO 作１－温罨法回数(7).
015920     MOVE 温罨法料５ＷＲ         TO 作１－温罨法料(7).
017290     MOVE 電療回数５ＷＲ         TO 作１－電療回数(7).
015940     MOVE 電療料５ＷＲ           TO 作１－電療料(7).
016860     MOVE 多部位率５ＷＲ         TO 作１－多部位逓減率(7).
016860     MOVE 多部位料５ＷＲ         TO 作１－多部位逓減額(7).
015980     MOVE 長期率５ＷＲ           TO 作１－長期逓減率(7).
015980     MOVE 長期頻回率５ＷＲ       TO 作１－長期頻回逓減率(7).
      */20250120↓↓↓
020760     MOVE 部位合計５ＷＲ         TO 作１－部位別料金計(7).
016890*
015960     MOVE 逓減開始月日５０ＷＲ   TO 作１－逓減開始月日(8).
017230     MOVE 後療回数５０ＷＲ       TO 作１－後療回数(8).
015880     MOVE 後療料５０ＷＲ         TO 作１－後療料(8).
017250     MOVE 冷罨法回数５０ＷＲ     TO 作１－冷罨法回数(8).
015900     MOVE 冷罨法料５０ＷＲ       TO 作１－冷罨法料(8).
017270     MOVE 温罨法回数５０ＷＲ     TO 作１－温罨法回数(8).
015920     MOVE 温罨法料５０ＷＲ       TO 作１－温罨法料(8).
017290     MOVE 電療回数５０ＷＲ       TO 作１－電療回数(8).
015940     MOVE 電療料５０ＷＲ         TO 作１－電療料(8).
016860     MOVE ZERO                   TO 作１－多部位逓減率(8).
016860     MOVE ZERO                   TO 作１－多部位逓減額(8).
015980     MOVE 長期率５０ＷＲ         TO 作１－長期逓減率(8).
015980     MOVE 長期頻回率５０ＷＲ     TO 作１－長期頻回逓減率(8).
      */20250120↓↓↓
020760     MOVE 部位合計５０ＷＲ       TO 作１－部位別料金計(8).
016890*
015960     MOVE 逓減開始月日６ＷＲ     TO 作１－逓減開始月日(9).
017230     MOVE 後療回数６ＷＲ         TO 作１－後療回数(9).
015880     MOVE 後療料６ＷＲ           TO 作１－後療料(9).
017250     MOVE 冷罨法回数６ＷＲ       TO 作１－冷罨法回数(9).
015900     MOVE 冷罨法料６ＷＲ         TO 作１－冷罨法料(9).
017270     MOVE 温罨法回数６ＷＲ       TO 作１－温罨法回数(9).
015920     MOVE 温罨法料６ＷＲ         TO 作１－温罨法料(9).
017290     MOVE 電療回数６ＷＲ         TO 作１－電療回数(9).
015940     MOVE 電療料６ＷＲ           TO 作１－電療料(9).
016860     MOVE 多部位率６ＷＲ         TO 作１－多部位逓減率(9).
016860     MOVE 多部位料６ＷＲ         TO 作１－多部位逓減額(9).
015980     MOVE 長期率６ＷＲ           TO 作１－長期逓減率(9).
015980     MOVE 長期頻回率６ＷＲ       TO 作１－長期頻回逓減率(9).
      */20250120↓↓↓
020760     MOVE 部位合計６ＷＲ         TO 作１－部位別料金計(9).
016890*
015960     MOVE 逓減開始月日６０ＷＲ   TO 作１－逓減開始月日(10).
017230     MOVE 後療回数６０ＷＲ       TO 作１－後療回数(10).
015880     MOVE 後療料６０ＷＲ         TO 作１－後療料(10).
017250     MOVE 冷罨法回数６０ＷＲ     TO 作１－冷罨法回数(10).
015900     MOVE 冷罨法料６０ＷＲ       TO 作１－冷罨法料(10).
017270     MOVE 温罨法回数６０ＷＲ     TO 作１－温罨法回数(10).
015920     MOVE 温罨法料６０ＷＲ       TO 作１－温罨法料(10).
017290     MOVE 電療回数６０ＷＲ       TO 作１－電療回数(10).
015940     MOVE 電療料６０ＷＲ         TO 作１－電療料(10).
016860     MOVE ZERO                   TO 作１－多部位逓減率(10).
016860     MOVE ZERO                   TO 作１－多部位逓減額(10).
015980     MOVE 長期率６０ＷＲ         TO 作１－長期逓減率(10).
015980     MOVE 長期頻回率６０ＷＲ     TO 作１－長期頻回逓減率(10).
      */20250120↓↓↓
020760     MOVE 部位合計６０ＷＲ       TO 作１－部位別料金計(10).
016890*
015960     MOVE 逓減開始月日７ＷＲ     TO 作１－逓減開始月日(11).
017230     MOVE 後療回数７ＷＲ         TO 作１－後療回数(11).
015880     MOVE 後療料７ＷＲ           TO 作１－後療料(11).
017250     MOVE 冷罨法回数７ＷＲ       TO 作１－冷罨法回数(11).
015900     MOVE 冷罨法料７ＷＲ         TO 作１－冷罨法料(11).
017270     MOVE 温罨法回数７ＷＲ       TO 作１－温罨法回数(11).
015920     MOVE 温罨法料７ＷＲ         TO 作１－温罨法料(11).
017290     MOVE 電療回数７ＷＲ         TO 作１－電療回数(11).
015940     MOVE 電療料７ＷＲ           TO 作１－電療料(11).
016860     MOVE 多部位率７ＷＲ         TO 作１－多部位逓減率(11).
016860     MOVE 多部位料７ＷＲ         TO 作１－多部位逓減額(11).
015980     MOVE 長期率７ＷＲ           TO 作１－長期逓減率(11).
015980     MOVE 長期頻回率７ＷＲ       TO 作１－長期頻回逓減率(11).
      */20250120↓↓↓
020760     MOVE 部位合計７ＷＲ         TO 作１－部位別料金計(11).
016890*
015960     MOVE 逓減開始月日７０ＷＲ   TO 作１－逓減開始月日(12).
017230     MOVE 後療回数７０ＷＲ       TO 作１－後療回数(12).
015880     MOVE 後療料７０ＷＲ         TO 作１－後療料(12).
017250     MOVE 冷罨法回数７０ＷＲ     TO 作１－冷罨法回数(12).
015900     MOVE 冷罨法料７０ＷＲ       TO 作１－冷罨法料(12).
017270     MOVE 温罨法回数７０ＷＲ     TO 作１－温罨法回数(12).
015920     MOVE 温罨法料７０ＷＲ       TO 作１－温罨法料(12).
017290     MOVE 電療回数７０ＷＲ       TO 作１－電療回数(12).
015940     MOVE 電療料７０ＷＲ         TO 作１－電療料(12).
016860     MOVE ZERO                   TO 作１－多部位逓減率(12).
016860     MOVE ZERO                   TO 作１－多部位逓減額(12).
015980     MOVE 長期率７０ＷＲ         TO 作１－長期逓減率(12).
015980     MOVE 長期頻回率７０ＷＲ     TO 作１－長期頻回逓減率(12).
      */20250120↓↓↓
020760     MOVE 部位合計７０ＷＲ       TO 作１－部位別料金計(12).
017670*
           MOVE SPACE                  TO 作１－部位料金データ(13).
           MOVE SPACE                  TO 作１－部位料金データ(14).
      */20250120↓↓↓
           MOVE ZERO TO 作１－部位料金データ(13) 作１－部位料金データ(14).
      *
HILO***       DISPLAY "------------------------" PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 14 
HILO***       DISPLAY "102-1 " 受－患者コード " (" カウンタ ") " 作１－部位別料金計(カウンタ) END-PERFORM.
      */負傷原因
018380*      / 負傷原因用WORKクリアー /
018390     INITIALIZE 負傷原因ＷＴ.
018400     INITIALIZE 負傷患者番号ＣＷ.
018410     INITIALIZE 負傷連番ＣＷ.
018420     INITIALIZE 負傷原因ＴＢＬ.
018430     INITIALIZE 負傷原因内容Ｗ.
           PERFORM 負傷原因取得.
      */経過
018390     INITIALIZE 経過文Ｗ.
           PERFORM 経過取得.
           MOVE 経過文Ｗ               TO 作１－経過.
      */長期理由
018390     INITIALIZE 文章Ｗ.
           INITIALIZE 合成理由文Ｗ.
           INITIALIZE 理由文Ｗ.
           PERFORM 長期継続者Ｆ読込.
           MOVE 文章Ｗ                 TO 作１－長期理由.
      */往療理由
           MOVE SPACE                  TO 作１－往療理由.
      */適用
           PERFORM 摘要文取得.
           PERFORM VARYING カウンタ FROM 1 BY 1
                     UNTIL カウンタ > 8
               MOVE 連摘文－摘要文(カウンタ) TO 適用文Ｗ(カウンタ)
           END-PERFORM.
           MOVE 適用Ｗ                 TO 作１－適用
      *
           STRING 受－患者郵便番号１   DELIMITED BY SPACE
                  "-"                  DELIMITED BY SPACE
                  受－患者郵便番号２   DELIMITED BY SPACE
             INTO 作１－患者郵便番号
           END-STRING.
      *
           STRING 受－患者住所１       DELIMITED BY SPACE
                  受－患者住所２       DELIMITED BY SPACE
             INTO 住所Ｗ
           END-STRING.
           MOVE 住所１Ｗ               TO 作１－患者住所１.
           MOVE 住所２Ｗ               TO 作１－患者住所２.
           MOVE 住所３Ｗ               TO 作１－患者住所３.
      *
           MOVE 受－患者電話番号       TO 作１－患者電話番号.
      *
           IF レセ－レセ種別 NOT = 4
              MOVE SPACE               TO 作１－労働保険番号
              MOVE ZERO                TO 作１－労災指導管理料回数
              MOVE ZERO                TO 作１－労災指導管理料
              MOVE ZERO                TO 作１－労災文書回数
              MOVE ZERO                TO 作１－労災文書料
              MOVE ZERO                TO 作１－労災休業証明開始
              MOVE ZERO                TO 作１－労災休業証明終了
              PERFORM VARYING カウンタ FROM 1 BY 1
                        UNTIL カウンタ > 8
                 MOVE ZERO             TO 作１－特別材料料(カウンタ)
                 MOVE ZERO             TO 作１－包帯交換回数(カウンタ)
                 MOVE ZERO             TO 作１－包帯交換料(カウンタ)
              END-PERFORM
           END-IF.
      *
           MOVE SPACE                  TO 作１－予備.
015870*
012420*================================================================*
019320 集計処理 SECTION.
019330*
           COMPUTE 連印－合計件数 = 連印－合計件数 + 1.
      */20250120↓↓↓
      *     COMPUTE 連印－合計請求額 = 連印－合計請求額 + 作１－請求金額.
           IF 作１－医療助成区分 = 1
               COMPUTE 連印－合計請求額 = 連印－合計請求額 + 作１－請求金額
           ELSE
               COMPUTE 連印－合計請求額 = 連印－合計請求額 + 作１－公費請求金額
           END-IF.
      */20250120↑↑↑
      *
           EVALUATE レセ－レセ種別
           WHEN 1
              EVALUATE 受－保険種別
              WHEN 1
                 IF 受－保険者番号(1:2) = "10"
                    IF レセ－請求区分 = 2
                       COMPUTE 国返戻県内件数Ｗ   = 国返戻県内件数Ｗ   + 1
                       COMPUTE 国返戻県内日数Ｗ   = 国返戻県内日数Ｗ   + レセ－レセ実日数
                       COMPUTE 国返戻県内合計額Ｗ = 国返戻県内合計額Ｗ + レセ－合計
                       COMPUTE 国返戻県内請求額Ｗ = 国返戻県内請求額Ｗ + レセ－請求金額
                    ELSE
                       COMPUTE 国県内件数Ｗ   = 国県内件数Ｗ   + 1
                       COMPUTE 国県内日数Ｗ   = 国県内日数Ｗ   + レセ－レセ実日数
                       COMPUTE 国県内合計額Ｗ = 国県内合計額Ｗ + レセ－合計
                       COMPUTE 国県内請求額Ｗ = 国県内請求額Ｗ + レセ－請求金額
                    END-IF
                 ELSE
                    IF レセ－請求区分 = 2
                       COMPUTE 国返戻県外件数Ｗ   = 国返戻県外件数Ｗ   + 1
                       COMPUTE 国返戻県外日数Ｗ   = 国返戻県外日数Ｗ   + レセ－レセ実日数
                       COMPUTE 国返戻県外合計額Ｗ = 国返戻県外合計額Ｗ + レセ－合計
                       COMPUTE 国返戻県外請求額Ｗ = 国返戻県外請求額Ｗ + レセ－請求金額
                    ELSE
                       COMPUTE 国県外件数Ｗ   = 国県外件数Ｗ   + 1
                       COMPUTE 国県外日数Ｗ   = 国県外日数Ｗ   + レセ－レセ実日数
                       COMPUTE 国県外合計額Ｗ = 国県外合計額Ｗ + レセ－合計
                       COMPUTE 国県外請求額Ｗ = 国県外請求額Ｗ + レセ－請求金額
                    END-IF
                 END-IF
              WHEN 2
              WHEN 6
              WHEN 7
                 IF 受－保険者番号(3:2) = "10"
                    IF 受－本人家族区分 = 1
                       IF レセ－請求区分 = 2
                          COMPUTE 協返戻県内本件数Ｗ   = 協返戻県内本件数Ｗ   + 1
                          COMPUTE 協返戻県内本日数Ｗ   = 協返戻県内本日数Ｗ   + レセ－レセ実日数
                          COMPUTE 協返戻県内本合計額Ｗ = 協返戻県内本合計額Ｗ + レセ－合計
                          COMPUTE 協返戻県内本請求額Ｗ = 協返戻県内本請求額Ｗ + レセ－請求金額
                       ELSE
                          COMPUTE 協県内本件数Ｗ   = 協県内本件数Ｗ   + 1
                          COMPUTE 協県内本日数Ｗ   = 協県内本日数Ｗ   + レセ－レセ実日数
                          COMPUTE 協県内本合計額Ｗ = 協県内本合計額Ｗ + レセ－合計
                          COMPUTE 協県内本請求額Ｗ = 協県内本請求額Ｗ + レセ－請求金額
                       END-IF
                    ELSE
                       IF レセ－請求区分 = 2
                          COMPUTE 協返戻県内家件数Ｗ   = 協返戻県内家件数Ｗ   + 1
                          COMPUTE 協返戻県内家日数Ｗ   = 協返戻県内家日数Ｗ   + レセ－レセ実日数
                          COMPUTE 協返戻県内家合計額Ｗ = 協返戻県内家合計額Ｗ + レセ－合計
                          COMPUTE 協返戻県内家請求額Ｗ = 協返戻県内家請求額Ｗ + レセ－請求金額
                       ELSE
                          COMPUTE 協県内家件数Ｗ   = 協県内家件数Ｗ   + 1
                          COMPUTE 協県内家日数Ｗ   = 協県内家日数Ｗ   + レセ－レセ実日数
                          COMPUTE 協県内家合計額Ｗ = 協県内家合計額Ｗ + レセ－合計
                          COMPUTE 協県内家請求額Ｗ = 協県内家請求額Ｗ + レセ－請求金額
                       END-IF
                    END-IF
                 ELSE
                    IF 受－本人家族区分 = 1
                       IF レセ－請求区分 = 2
                          COMPUTE 協返戻県外本件数Ｗ   = 協返戻県外本件数Ｗ   + 1
                          COMPUTE 協返戻県外本日数Ｗ   = 協返戻県外本日数Ｗ   + レセ－レセ実日数
                          COMPUTE 協返戻県外本合計額Ｗ = 協返戻県外本合計額Ｗ + レセ－合計
                          COMPUTE 協返戻県外本請求額Ｗ = 協返戻県外本請求額Ｗ + レセ－請求金額
                       ELSE
                          COMPUTE 協県外本件数Ｗ   = 協県外本件数Ｗ   + 1
                          COMPUTE 協県外本日数Ｗ   = 協県外本日数Ｗ   + レセ－レセ実日数
                          COMPUTE 協県外本合計額Ｗ = 協県外本合計額Ｗ + レセ－合計
                          COMPUTE 協県外本請求額Ｗ = 協県外本請求額Ｗ + レセ－請求金額
                       END-IF
                    ELSE
                       IF レセ－請求区分 = 2
                          COMPUTE 協返戻県外家件数Ｗ   = 協返戻県外家件数Ｗ   + 1
                          COMPUTE 協返戻県外家日数Ｗ   = 協返戻県外家日数Ｗ   + レセ－レセ実日数
                          COMPUTE 協返戻県外家合計額Ｗ = 協返戻県外家合計額Ｗ + レセ－合計
                          COMPUTE 協返戻県外家請求額Ｗ = 協返戻県外家請求額Ｗ + レセ－請求金額
                       ELSE
                          COMPUTE 協県外家件数Ｗ   = 協県外家件数Ｗ   + 1
                          COMPUTE 協県外家日数Ｗ   = 協県外家日数Ｗ   + レセ－レセ実日数
                          COMPUTE 協県外家合計額Ｗ = 協県外家合計額Ｗ + レセ－合計
                          COMPUTE 協県外家請求額Ｗ = 協県外家請求額Ｗ + レセ－請求金額
                       END-IF
                    END-IF
                  END-IF
              WHEN 3
                 IF 受－保険者番号(3:2) = "10"
                    IF 受－本人家族区分 = 1
                       IF レセ－請求区分 = 2
                          COMPUTE 組返戻県内本件数Ｗ   = 組返戻県内本件数Ｗ   + 1
                          COMPUTE 組返戻県内本日数Ｗ   = 組返戻県内本日数Ｗ   + レセ－レセ実日数
                          COMPUTE 組返戻県内本合計額Ｗ = 組返戻県内本合計額Ｗ + レセ－合計
                          COMPUTE 組返戻県内本請求額Ｗ = 組返戻県内本請求額Ｗ + レセ－請求金額
                       ELSE
                          COMPUTE 組県内本件数Ｗ   = 組県内本件数Ｗ   + 1
                          COMPUTE 組県内本日数Ｗ   = 組県内本日数Ｗ   + レセ－レセ実日数
                          COMPUTE 組県内本合計額Ｗ = 組県内本合計額Ｗ + レセ－合計
                          COMPUTE 組県内本請求額Ｗ = 組県内本請求額Ｗ + レセ－請求金額
                       END-IF
                    ELSE
                       IF レセ－請求区分 = 2
                          COMPUTE 組返戻県内家件数Ｗ   = 組返戻県内家件数Ｗ   + 1
                          COMPUTE 組返戻県内家日数Ｗ   = 組返戻県内家日数Ｗ   + レセ－レセ実日数
                          COMPUTE 組返戻県内家合計額Ｗ = 組返戻県内家合計額Ｗ + レセ－合計
                          COMPUTE 組返戻県内家請求額Ｗ = 組返戻県内家請求額Ｗ + レセ－請求金額
                       ELSE
                          COMPUTE 組県内家件数Ｗ   = 組県内家件数Ｗ   + 1
                          COMPUTE 組県内家日数Ｗ   = 組県内家日数Ｗ   + レセ－レセ実日数
                          COMPUTE 組県内家合計額Ｗ = 組県内家合計額Ｗ + レセ－合計
                          COMPUTE 組県内家請求額Ｗ = 組県内家請求額Ｗ + レセ－請求金額
                       END-IF
                    END-IF
                 ELSE
                    IF 受－本人家族区分 = 1
                       IF レセ－請求区分 = 2
                          COMPUTE 組返戻県外本件数Ｗ   = 組返戻県外本件数Ｗ   + 1
                          COMPUTE 組返戻県外本日数Ｗ   = 組返戻県外本日数Ｗ   + レセ－レセ実日数
                          COMPUTE 組返戻県外本合計額Ｗ = 組返戻県外本合計額Ｗ + レセ－合計
                          COMPUTE 組返戻県外本請求額Ｗ = 組返戻県外本請求額Ｗ + レセ－請求金額
                       ELSE
                          COMPUTE 組県外本件数Ｗ   = 組県外本件数Ｗ   + 1
                          COMPUTE 組県外本日数Ｗ   = 組県外本日数Ｗ   + レセ－レセ実日数
                          COMPUTE 組県外本合計額Ｗ = 組県外本合計額Ｗ + レセ－合計
                          COMPUTE 組県外本請求額Ｗ = 組県外本請求額Ｗ + レセ－請求金額
                       END-IF
                    ELSE
                       IF レセ－請求区分 = 2
                          COMPUTE 組返戻県外家件数Ｗ   = 組返戻県外家件数Ｗ   + 1
                          COMPUTE 組返戻県外家日数Ｗ   = 組返戻県外家日数Ｗ   + レセ－レセ実日数
                          COMPUTE 組返戻県外家合計額Ｗ = 組返戻県外家合計額Ｗ + レセ－合計
                          COMPUTE 組返戻県外家請求額Ｗ = 組返戻県外家請求額Ｗ + レセ－請求金額
                       ELSE
                          COMPUTE 組県外家件数Ｗ   = 組県外家件数Ｗ   + 1
                          COMPUTE 組県外家日数Ｗ   = 組県外家日数Ｗ   + レセ－レセ実日数
                          COMPUTE 組県外家合計額Ｗ = 組県外家合計額Ｗ + レセ－合計
                          COMPUTE 組県外家請求額Ｗ = 組県外家請求額Ｗ + レセ－請求金額
                       END-IF
                    END-IF
                  END-IF
               WHEN 4
               WHEN 9
                  IF 受－本人家族区分 = 1
                     IF レセ－請求区分 = 2
                       COMPUTE 共返戻本件数Ｗ   = 共返戻本件数Ｗ   + 1
                       COMPUTE 共返戻本日数Ｗ   = 共返戻本日数Ｗ   + レセ－レセ実日数
                       COMPUTE 共返戻本合計額Ｗ = 共返戻本合計額Ｗ + レセ－合計
                       COMPUTE 共返戻本請求額Ｗ = 共返戻本請求額Ｗ + レセ－請求金額
                    ELSE
                       COMPUTE 共本件数Ｗ   = 共本件数Ｗ   + 1
                       COMPUTE 共本日数Ｗ   = 共本日数Ｗ   + レセ－レセ実日数
                       COMPUTE 共本合計額Ｗ = 共本合計額Ｗ + レセ－合計
                       COMPUTE 共本請求額Ｗ = 共本請求額Ｗ + レセ－請求金額
                    END-IF
                 ELSE
                    IF レセ－請求区分 = 2
                       COMPUTE 共返戻家件数Ｗ   = 共返戻家件数Ｗ   + 1
                       COMPUTE 共返戻家日数Ｗ   = 共返戻家日数Ｗ   + レセ－レセ実日数
                       COMPUTE 共返戻家合計額Ｗ = 共返戻家合計額Ｗ + レセ－合計
                       COMPUTE 共返戻家請求額Ｗ = 共返戻家請求額Ｗ + レセ－請求金額
                    ELSE
                       COMPUTE 共家件数Ｗ   = 共家件数Ｗ   + 1
                       COMPUTE 共家日数Ｗ   = 共家日数Ｗ   + レセ－レセ実日数
                       COMPUTE 共家合計額Ｗ = 共家合計額Ｗ + レセ－合計
                       COMPUTE 共家請求額Ｗ = 共家請求額Ｗ + レセ－請求金額
                    END-IF
                 END-IF
               WHEN 8
                 IF 受－保険者番号(3:2) = "10"
                    IF レセ－請求区分 = 2
                       COMPUTE 国返戻県内件数Ｗ   = 国返戻県内件数Ｗ   + 1
                       COMPUTE 国返戻県内日数Ｗ   = 国返戻県内日数Ｗ   + レセ－レセ実日数
                       COMPUTE 国返戻県内合計額Ｗ = 国返戻県内合計額Ｗ + レセ－合計
                       COMPUTE 国返戻県内請求額Ｗ = 国返戻県内請求額Ｗ + レセ－請求金額
                    ELSE
                       COMPUTE 国県内件数Ｗ   = 国県内件数Ｗ   + 1
                       COMPUTE 国県内日数Ｗ   = 国県内日数Ｗ   + レセ－レセ実日数
                       COMPUTE 国県内合計額Ｗ = 国県内合計額Ｗ + レセ－合計
                       COMPUTE 国県内請求額Ｗ = 国県内請求額Ｗ + レセ－請求金額
                    END-IF
                 ELSE
                    IF レセ－請求区分 = 2
                       COMPUTE 国返戻県外件数Ｗ   = 国返戻県外件数Ｗ   + 1
                       COMPUTE 国返戻県外日数Ｗ   = 国返戻県外日数Ｗ   + レセ－レセ実日数
                       COMPUTE 国返戻県外合計額Ｗ = 国返戻県外合計額Ｗ + レセ－合計
                       COMPUTE 国返戻県外請求額Ｗ = 国返戻県外請求額Ｗ + レセ－請求金額
                    ELSE
                       COMPUTE 国県外件数Ｗ   = 国県外件数Ｗ   + 1
                       COMPUTE 国県外日数Ｗ   = 国県外日数Ｗ   + レセ－レセ実日数
                       COMPUTE 国県外合計額Ｗ = 国県外合計額Ｗ + レセ－合計
                       COMPUTE 国県外請求額Ｗ = 国県外請求額Ｗ + レセ－請求金額
                    END-IF
                 END-IF
              END-EVALUATE
           WHEN 2
              IF 受－費用負担者番号(3:2) = "10"
                 IF レセ－請求区分 = 2
                    COMPUTE 後返戻県内件数Ｗ   = 後返戻県内件数Ｗ   + 1
                    COMPUTE 後返戻県内日数Ｗ   = 後返戻県内日数Ｗ   + レセ－レセ実日数
                    COMPUTE 後返戻県内合計額Ｗ = 後返戻県内合計額Ｗ + レセ－合計
                    COMPUTE 後返戻県内請求額Ｗ = 後返戻県内請求額Ｗ + レセ－請求金額
                 ELSE
                    COMPUTE 後県内件数Ｗ   = 後県内件数Ｗ   + 1
                    COMPUTE 後県内日数Ｗ   = 後県内日数Ｗ   + レセ－レセ実日数
                    COMPUTE 後県内合計額Ｗ = 後県内合計額Ｗ + レセ－合計
                    COMPUTE 後県内請求額Ｗ = 後県内請求額Ｗ + レセ－請求金額
                 END-IF
              ELSE
                 IF レセ－請求区分 = 2
                    COMPUTE 後返戻県外件数Ｗ   = 後返戻県外件数Ｗ   + 1
                    COMPUTE 後返戻県外日数Ｗ   = 後返戻県外日数Ｗ   + レセ－レセ実日数
                    COMPUTE 後返戻県外合計額Ｗ = 後返戻県外合計額Ｗ + レセ－合計
                    COMPUTE 後返戻県外請求額Ｗ = 後返戻県外請求額Ｗ + レセ－請求金額
                 ELSE
                    COMPUTE 後県外件数Ｗ   = 後県外件数Ｗ   + 1
                    COMPUTE 後県外日数Ｗ   = 後県外日数Ｗ   + レセ－レセ実日数
                    COMPUTE 後県外合計額Ｗ = 後県外合計額Ｗ + レセ－合計
                    COMPUTE 後県外請求額Ｗ = 後県外請求額Ｗ + レセ－請求金額
                 END-IF
              END-IF
           WHEN 3
              IF レセ－請求区分 = 2
                 COMPUTE 助成返戻件数Ｗ   = 助成返戻件数Ｗ   + 1
                 COMPUTE 助成返戻日数Ｗ   = 助成返戻日数Ｗ   + レセ－レセ実日数
                 COMPUTE 助成返戻合計額Ｗ = 助成返戻合計額Ｗ + レセ－合計
                 COMPUTE 助成返戻請求額Ｗ = 助成返戻請求額Ｗ + レセ－助成請求金額
              ELSE
                 COMPUTE 助成件数Ｗ   = 助成件数Ｗ   + 1
                 COMPUTE 助成日数Ｗ   = 助成日数Ｗ   + レセ－レセ実日数
                 COMPUTE 助成合計額Ｗ = 助成合計額Ｗ + レセ－合計
                 COMPUTE 助成請求額Ｗ = 助成請求額Ｗ + レセ－助成請求金額
              END-IF
           WHEN 4
              IF レセ－請求区分 = 2
                 COMPUTE 労災返戻件数Ｗ   = 労災返戻件数Ｗ   + 1
                 COMPUTE 労災返戻日数Ｗ   = 労災返戻日数Ｗ   + レセ－レセ実日数
                 COMPUTE 労災返戻合計額Ｗ = 労災返戻合計額Ｗ + レセ－合計
                 COMPUTE 労災返戻請求額Ｗ = 労災返戻請求額Ｗ + レセ－請求金額
              ELSE
                 COMPUTE 労災件数Ｗ   = 労災件数Ｗ   + 1
                 COMPUTE 労災日数Ｗ   = 労災日数Ｗ   + レセ－レセ実日数
                 COMPUTE 労災合計額Ｗ = 労災合計額Ｗ + レセ－合計
                 COMPUTE 労災請求額Ｗ = 労災請求額Ｗ + レセ－請求金額
              END-IF
           WHEN 7
              IF レセ－請求区分 = 2
                 COMPUTE 生保返戻件数Ｗ   = 生保返戻件数Ｗ   + 1
                 COMPUTE 生保返戻日数Ｗ   = 生保返戻日数Ｗ   + レセ－レセ実日数
                 COMPUTE 生保返戻合計額Ｗ = 生保返戻合計額Ｗ + レセ－合計
                 COMPUTE 生保返戻請求額Ｗ = 生保返戻請求額Ｗ + レセ－請求金額
              ELSE
                 COMPUTE 生保件数Ｗ   = 生保件数Ｗ   + 1
                 COMPUTE 生保日数Ｗ   = 生保日数Ｗ   + レセ－レセ実日数
                 COMPUTE 生保合計額Ｗ = 生保合計額Ｗ + レセ－合計
                 COMPUTE 生保請求額Ｗ = 生保請求額Ｗ + レセ－請求金額
              END-IF
           END-EVALUATE.
015870*
012420*================================================================*
019320 作１ファイル書込 SECTION.
019330*
019340     WRITE 作１－レコード
019350     INVALID KEY
019360         MOVE NC"作１"  TO ファイル名
019370         PERFORM エラー表示
019380     END-WRITE.
036110*================================================================*
036120 負傷原因取得 SECTION.
036130*================================================================*
036140********************************************************************
036150*  負傷原因コードが同じものは、1行にまとめて印字する。
036160*  例: ①② 家で転んだ.
036170*     負傷原因コードが同じものをまとめ、テーブルにセット
036180*     (ただし、部位を飛んで同じものは、2行になる)
036190********************************************************************
036200     MOVE  ZERO   TO  カウンタ カウンタ２.
036210     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
036220             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
036230*
036240****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
036250        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
036260*
036270           IF ( カウンタ = ZERO )
036280              MOVE 1   TO  カウンタ カウンタ２
036290              MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
036300              MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
036310              MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
036320           ELSE
036330              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
036340                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
036350                 COMPUTE カウンタ２ = カウンタ２  +  1
036360                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
036370              ELSE
036380                 COMPUTE カウンタ = カウンタ  +  1
036390                 MOVE 1   TO  カウンタ２
036400                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
036410                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
036420                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
036430              END-IF
036440           END-IF
036450        END-IF
036460     END-PERFORM.
036470**************************************************************************
036480*  負傷原因マスタより文章取得
036490**************************************************************************
036500     MOVE  ZERO   TO  カウンタ カウンタ２.
036510     PERFORM VARYING カウンタ FROM 1 BY 1
036520             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
036530** 健保は 区分 01
036540         MOVE 01                        TO 負原－区分コード
036550         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
036560         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
036570         READ 負傷原因Ｆ
036580         NOT INVALID KEY
036590             INITIALIZE 負傷原因ＷＴ
036600             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
036610             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
036620             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
036630             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
036640             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
036650             PERFORM VARYING カウンタ２ FROM 1 BY 1
036660                     UNTIL ( カウンタ２ > 9 )  OR 
036670                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
036680                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
036690                WHEN 1
036700                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
036710                WHEN 2
036720                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
036730                WHEN 3
036740                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
036750                WHEN 4
036760                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
036770                WHEN 5
036780                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
036750                WHEN 6
036760                   MOVE "⑥"  TO  負傷原因ナンバーＷ１(カウンタ２)
036770                WHEN 7
036780                   MOVE "⑦"  TO  負傷原因ナンバーＷ１(カウンタ２)
036790                WHEN OTHER
036800                   CONTINUE
036810                END-EVALUATE
036820             END-PERFORM
036830*
036840             IF 負原－負傷原因入力区分 = 1
036850                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
036860                        負傷原因１ＷＴ  DELIMITED BY SIZE
036870                        負傷原因２ＷＴ  DELIMITED BY SIZE
036880                        負傷原因３ＷＴ  DELIMITED BY SIZE
036890                        負傷原因４ＷＴ  DELIMITED BY SIZE
036900                        負傷原因５ＷＴ  DELIMITED BY SIZE
036910                        INTO 負傷原因内容合成Ｗ(カウンタ)
036920                 END-STRING
036930             ELSE
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
037020             END-IF
037030*
037040         END-READ
037050     END-PERFORM.
027610*
027620     PERFORM 負傷原因セット.
018460*
           MOVE 負傷原因内容合成Ｗ(1) TO 転記用負傷原因内容合成Ｗ(1)
           MOVE 負傷原因内容合成Ｗ(2) TO 転記用負傷原因内容合成Ｗ(2)
           MOVE 負傷原因内容合成Ｗ(3) TO 転記用負傷原因内容合成Ｗ(3)
           MOVE 負傷原因内容合成Ｗ(4) TO 転記用負傷原因内容合成Ｗ(4)
           MOVE 負傷原因内容合成Ｗ(5) TO 転記用負傷原因内容合成Ｗ(5)
           MOVE 負傷原因内容合成Ｗ(6) TO 転記用負傷原因内容合成Ｗ(6)
           MOVE 負傷原因内容合成Ｗ(7) TO 転記用負傷原因内容合成Ｗ(7)
           MOVE 負傷原因内容合成Ｗ(8) TO 転記用負傷原因内容合成Ｗ(8)
           MOVE 負傷原因内容合成Ｗ(9) TO 転記用負傷原因内容合成Ｗ(9)
           INSPECT 転記用負傷原因内容Ｗ REPLACING ALL "　" BY "  "
           STRING 転記用負傷原因内容合成Ｗ(1) DELIMITED BY "  "
                  "　"                        DELIMITED BY SIZE
                  転記用負傷原因内容合成Ｗ(2) DELIMITED BY "  "
                  "　"                        DELIMITED BY SIZE
                  転記用負傷原因内容合成Ｗ(3) DELIMITED BY "  "
                  "　"                        DELIMITED BY SIZE
                  転記用負傷原因内容合成Ｗ(4) DELIMITED BY "  "
                  "　"                        DELIMITED BY SIZE
                  転記用負傷原因内容合成Ｗ(5) DELIMITED BY "  "
                  "　"                        DELIMITED BY SIZE
                  転記用負傷原因内容合成Ｗ(6) DELIMITED BY "  "
                  "　"                        DELIMITED BY SIZE
                  転記用負傷原因内容合成Ｗ(7) DELIMITED BY "  "
                  "　"                        DELIMITED BY SIZE
                  転記用負傷原因内容合成Ｗ(8) DELIMITED BY "  "
                  "　"                        DELIMITED BY SIZE
                  転記用負傷原因内容合成Ｗ(9) DELIMITED BY "  "
                  INTO 作１－負傷原因
           END-STRING.
037060*
027640*================================================================*
027650 負傷原因セット SECTION.
027660*
027670**************************************************************************
027680*  文章が1行を超える時は、複数行に分解する。
027690**************************************************************************
027700     MOVE  ZERO   TO  カウンタ カウンタ２.
027710     PERFORM VARYING カウンタ FROM 1 BY 1
027720             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
027730*
027740          INITIALIZE 負傷原因内容分解Ｗ
027750          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解Ｗ
027760          IF  負傷原因内容１Ｗ  NOT = SPACE
027770              COMPUTE カウンタ２ = カウンタ２  +  1
027780              MOVE 負傷原因内容１Ｗ  TO 負傷原因Ｗ(カウンタ２)
027790          END-IF
027800          IF  負傷原因内容２Ｗ  NOT = SPACE
027810              COMPUTE カウンタ２ = カウンタ２  +  1
027820              MOVE 負傷原因内容２Ｗ  TO 負傷原因Ｗ(カウンタ２)
027830          END-IF
027840          IF  負傷原因内容３Ｗ  NOT = SPACE
027850              COMPUTE カウンタ２ = カウンタ２  +  1
027860              MOVE 負傷原因内容３Ｗ  TO 負傷原因Ｗ(カウンタ２)
027870          END-IF
027760          IF  負傷原因内容４Ｗ  NOT = SPACE
027770              COMPUTE カウンタ２ = カウンタ２  +  1
027780              MOVE 負傷原因内容４Ｗ  TO 負傷原因Ｗ(カウンタ２)
027790          END-IF
027800          IF  負傷原因内容５Ｗ  NOT = SPACE
027810              COMPUTE カウンタ２ = カウンタ２  +  1
027820              MOVE 負傷原因内容５Ｗ  TO 負傷原因Ｗ(カウンタ２)
027830          END-IF
027840          IF  負傷原因内容６Ｗ  NOT = SPACE
027850              COMPUTE カウンタ２ = カウンタ２  +  1
027860              MOVE 負傷原因内容６Ｗ  TO 負傷原因Ｗ(カウンタ２)
027870          END-IF
027880*
027890     END-PERFORM.
027900*
036110*================================================================*
036120 経過取得 SECTION.
      *
036510     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
036520             UNTIL ( 部位ＣＮＴ > 5 )
032990         MOVE 01                         TO 経－区分コード
033000         MOVE 負－経過コード(部位ＣＮＴ) TO 経－経過コード
033010         READ 経過マスタ
033020         INVALID KEY
033030             MOVE ZERO            TO 部位ＣＮＴＷ(部位ＣＮＴ)
033050             MOVE SPACE           TO 経過略称Ｗ(部位ＣＮＴ)
033060         NOT INVALID KEY
033070             EVALUATE 部位ＣＮＴ
033080             WHEN 1
033090                 MOVE "①" TO 部位番号Ｗ
033100             WHEN 2
033110                 MOVE "②" TO 部位番号Ｗ
033120             WHEN 3
033130                 MOVE "③" TO 部位番号Ｗ
033140             WHEN 4
033150                 MOVE "④" TO 部位番号Ｗ
033160             WHEN 5
033170                 MOVE "⑤" TO 部位番号Ｗ
033180             END-EVALUATE
                   MOVE 経－経過略称      TO 経過ＷＲ
033190             STRING  部位番号Ｗ     DELIMITED BY SPACE
033200                     経過Ｗ         DELIMITED BY SPACE
033210                    INTO 印刷経過略称Ｗ(部位ＣＮＴ)
033220             END-STRING
033230         END-READ
           END-PERFORM.
033240*
012420*================================================================*
       長期継続者Ｆ読込 SECTION.
      *
           MOVE レセ－施術和暦年月 TO 長継－施術和暦年月.
000170     MOVE レセ－患者コード   TO 長継－患者コード.
           READ 長期継続者Ｆ
           INVALID KEY
               MOVE SPACE TO 長継－レコード
033060     NOT INVALID KEY
               MOVE 1           TO カウンタ２
               MOVE SPACE       TO 合成理由文Ｗ
036510         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
036520                   UNTIL ( 部位ＣＮＴ > 5 )
004560            IF  長継－理由文(部位ＣＮＴ) NOT = SPACE 
004561               PERFORM 番号転記
004595* 理由文
                     IF ナンバーＷＴ NOT = SPACE
004598                  MOVE 長継－理由文(部位ＣＮＴ)  TO  理由文Ｗ
                        STRING ナンバーＮＷ     DELIMITED BY SPACE
                               理由文Ｗ         DELIMITED BY SPACE
                          INTO 合成理由文ＷＴ(カウンタ２)
                        END-STRING
                     END-IF
                     COMPUTE カウンタ２ = カウンタ２ + 1
                  END-IF
004637         END-PERFORM
           END-READ.
004543     PERFORM VARYING カウンタ２ FROM 1 BY 1
004544             UNTIL   カウンタ２  > 5
              IF 合成理由文ＷＴ(カウンタ２) NOT = SPACE
                 STRING 文章Ｗ                     DELIMITED BY SPACE
                        合成理由文ＷＴ(カウンタ２) DELIMITED BY SPACE
                   INTO 文章Ｗ
                 END-STRING
              END-IF
           END-PERFORM.
005590*================================================================*
005600 番号転記 SECTION.
005601*
           MOVE SPACE        TO ナンバーＷＴ.
           MOVE 部位ＣＮＴ   TO カウンタ.
           MOVE 1            TO 番号ＣＮＴ.
           IF ( 長継－同一理由部位番号(部位ＣＮＴ) = ZERO )
                 EVALUATE 部位ＣＮＴ
005606           WHEN  1
005607               MOVE "①"  TO ナンバーＷ(番号ＣＮＴ)
005608           WHEN  2
005609               MOVE "②"  TO ナンバーＷ(番号ＣＮＴ)
005610           WHEN  3
005611               MOVE "③"  TO ナンバーＷ(番号ＣＮＴ)
005612           WHEN  4
005613               MOVE "④"  TO ナンバーＷ(番号ＣＮＴ)
005614           WHEN  5
005615               MOVE "⑤"  TO ナンバーＷ(番号ＣＮＴ)
005616           WHEN  OTHER
005617               CONTINUE
005618           END-EVALUATE
                 COMPUTE 番号ＣＮＴ = 番号ＣＮＴ + 1
005605           PERFORM VARYING カウンタ FROM 部位ＣＮＴ BY 1
                         UNTIL カウンタ > 5
                    IF ( 長継－同一理由部位番号(カウンタ) = 部位ＣＮＴ )
                       EVALUATE カウンタ
005606                 WHEN  1
005607                     MOVE "①"  TO ナンバーＷ(番号ＣＮＴ)
005608                 WHEN  2
005609                     MOVE "②"  TO ナンバーＷ(番号ＣＮＴ)
005610                 WHEN  3
005611                     MOVE "③"  TO ナンバーＷ(番号ＣＮＴ)
005612                 WHEN  4
005613                     MOVE "④"  TO ナンバーＷ(番号ＣＮＴ)
005614                 WHEN  5
005615                     MOVE "⑤"  TO ナンバーＷ(番号ＣＮＴ)
005616                 WHEN  OTHER
005617                     CONTINUE
005618                 END-EVALUATE
                       COMPUTE 番号ＣＮＴ = 番号ＣＮＴ + 1
                    END-IF
                 END-PERFORM
             END-IF.
005619*
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
039370     MOVE 63            TO  連摘文－文桁数.
           MOVE 1             TO  連摘文－長期区分.
040710*
040720     CALL   "TEKIYBUN".
040730     CANCEL "TEKIYBUN".
040740*
008264*================================================================*
008265 助成レセまとめ判定 SECTION.
008266*---------------------------------------------------------------------------*
014700* 本体まとめ区分＝１
008268* の時は、フラグYES (金額を助成込みで印字）
008269*（例：横浜市の障害は、本体保険（国保系）のレセプト１枚で請求、助成レセはなし）
008270*---------------------------------------------------------------------------*
008271*
008272     MOVE SPACE TO 助成レセまとめフラグ.
008273*
014770     IF レセ－本体まとめ区分 = 1 
014780        MOVE "YES" TO 助成レセまとめフラグ
014790     END-IF.
008313*
012420*================================================================*
011330 作業ファイル２作成 SECTION.
011340*
011344     OPEN OUTPUT 作業ファイル２.
011345         MOVE NC"作２" TO ファイル名.
011346         PERFORM オープンチェック.
      *
           MOVE 連印－請求年月  TO 請求年月Ｗ.
           MOVE 会員番号数字Ｗ  TO 会員番号２Ｗ.
           MOVE 集計Ｗ          TO 作２－レコード.
           PERFORM 作２ファイル書込.
      *
           CLOSE 作業ファイル２.
012420*================================================================*
019320 作２ファイル書込 SECTION.
019330*
019340     WRITE 作２－レコード
019350     INVALID KEY
019360         MOVE NC"作２"  TO ファイル名
019370         PERFORM エラー表示
019380     END-WRITE.
027744******************************************************************
027745 END PROGRAM YGN101.
027746******************************************************************
