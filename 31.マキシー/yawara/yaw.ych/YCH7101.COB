000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCH7101.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*         カルテ裏(複数月)【ﾃﾞｰﾀ作成】柔ｳｨﾝﾄﾞｳｽﾞ版
000100*         MED = YCH7100 YCH7102P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2018-02-22
000130 DATE-COMPILED.          2018-02-22
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
000370     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000380                             ORGANIZATION             IS  INDEXED
000390                             ACCESS MODE              IS  DYNAMIC
000400                             RECORD KEY               IS  元－元号区分
000410                             FILE STATUS              IS  状態キー
000420                             LOCK        MODE         IS  AUTOMATIC.
000260     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY           IS 施記－施術和暦年月日
000300                                                     施記－患者コード
000310                             ALTERNATE RECORD KEY IS 施記－患者コード
000320                                                     施記－施術和暦年月日
000330                             FILE STATUS              IS  状態キー
000340                             LOCK        MODE         IS  AUTOMATIC.
000350     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000360                             ORGANIZATION             IS  INDEXED
000370                             ACCESS MODE              IS  DYNAMIC
000380                             RECORD KEY               IS  受－施術和暦年月
000390                                                          受－患者コード
000400                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000410                                                          受－患者カナ
000420                                                          受－患者コード
000430                             ALTERNATE RECORD KEY     IS  受－患者コード
000440                                                          受－施術和暦年月
000450                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000460                                                          受－保険種別
000470                                                          受－保険者番号
000480                                                          受－患者コード
000490                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000500                                                          受－公費種別
000510                                                          受－費用負担者番号
000520                                                          受－患者コード
000530                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000540                                                          受－助成種別
000550                                                          受－費用負担者番号助成
000560                                                          受－患者コード
000570                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
000580                                                          受－施術和暦年月
000590                                                          受－患者コード
000600                             FILE STATUS              IS  状態キー
000610                             LOCK        MODE         IS  AUTOMATIC.
000620     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
000630                             ORGANIZATION             IS  INDEXED
000640                             ACCESS MODE              IS  DYNAMIC
000650                             RECORD KEY               IS 負－施術和暦年月
000660                                                         負－患者コード
000670                             ALTERNATE RECORD KEY     IS 負－患者コード
000680                                                         負－施術和暦年月
000690                             FILE STATUS              IS  状態キー
000700                             LOCK        MODE         IS  AUTOMATIC.
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
000690     SELECT  会計データＦ    ASSIGN      TO        KAIKEIL
000700                             ORGANIZATION             IS  INDEXED
000710                             ACCESS MODE              IS  DYNAMIC
000089                             RECORD KEY               IS  会－施術和暦年月日
000090                                                          会－患者コード
000092                             ALTERNATE RECORD KEY     IS  会－患者コード
000093                                                          会－施術和暦年月日
000790                             FILE STATUS              IS  状態キー
000800                             LOCK        MODE         IS  AUTOMATIC.
           SELECT  計算マスタ      ASSIGN      TO        KEISANL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  計－制御区分
                                                                計－開始和暦年月
                                   FILE STATUS              IS  状態キー
                                   LOCK        MODE         IS  AUTOMATIC.
000770     SELECT  メモファイル    ASSIGN      TO        MEMOL
000780                             ORGANIZATION             IS  INDEXED
000790                             ACCESS MODE              IS  DYNAMIC
000800                             RECORD KEY               IS  メモ－制御区分
000810                                                          メモ－患者コード
000820                                                          メモ－施術和暦年月日
000830                             ALTERNATE RECORD KEY     IS  メモ－制御区分
000840                                                          メモ－施術和暦年月日
000850                                                          メモ－患者コード
000860                             ALTERNATE RECORD KEY     IS  メモ－患者コード
000870                                                          メモ－施術和暦年月日
000880                                                          メモ－制御区分
000890                             FILE STATUS              IS  状態キー
000900                             LOCK        MODE         IS  AUTOMATIC.
000770     SELECT  作業ファイル１    ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W7101L.DAT"
000780                             ORGANIZATION             IS  INDEXED
000790                             ACCESS                   IS  DYNAMIC
000560                             RECORD      KEY          IS  作１－施術和暦年月日
000610                                                          作１－患者コード
000910                             ALTERNATE RECORD KEY     IS  作１－施術和暦年月日
                                                                作１－患者カナ
                                                                作１－患者コード
000910                             ALTERNATE RECORD KEY     IS  作１－患者コード
                                                                作１－施術和暦年月日
000910                             ALTERNATE RECORD KEY     IS  作１－患者カナ
                                                                作１－患者コード
                                                                作１－施術和暦年月日
000810                             FILE        STATUS       IS  状態キー
000820                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  作業ファイル２  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W7132L.DAT"
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS                   IS  DYNAMIC
000560                             RECORD      KEY          IS  作２－施術和暦年月
000610                                                          作２－患者コード
000910                             ALTERNATE RECORD KEY     IS  作２－患者コード
                                                                作２－施術和暦年月
000620                             FILE        STATUS       IS  状態キー
000630                             LOCK        MODE         IS  AUTOMATIC.
      */枝番有りチェック用
000970     SELECT  作業ファイル３  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W7133L.DAT"
000980                             ORGANIZATION             IS  INDEXED
000990                             ACCESS                   IS  DYNAMIC
001000                             RECORD      KEY    IS        作３－患者番号
                                                                作３－施術和暦年月
001010                             FILE        STATUS       IS  状態キー
001020                             LOCK        MODE         IS  AUTOMATIC.
000830******************************************************************
000840*                      DATA DIVISION                             *
000850******************************************************************
000860 DATA                    DIVISION.
000870 FILE                    SECTION.
001500*                           ［ＲＬ＝  １２８］
001510 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001520     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
000880*                           ［ＲＬ＝  ２５６］
000890 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
000900     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
000910*                           ［ＲＬ＝  ３２０］
000920 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
000930     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
000940*                           ［ＲＬ＝  ６４０］
000950 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
000960     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
      *                           ［ＲＬ＝  ２５６］
       FD  計算マスタ          BLOCK   CONTAINS   1   RECORDS.
           COPY KEISAN          OF  XFDLIB  JOINING   計   AS  PREFIX.
           COPY KEISANA         OF  XFDLIB  JOINING   計Ａ AS  PREFIX.
001060*                           ［ＲＬ＝  ５１２］
001070 FD  会計データＦ        BLOCK   CONTAINS   1   RECORDS.
001080     COPY KAIKEI     OF  XFDLIB  JOINING   会   AS  PREFIX.
001200*                           ［ＲＬ＝  ８３２］
001210 FD  メモファイル        BLOCK CONTAINS 1     RECORDS.
001220     COPY MEMO           OF    XFDLIB JOINING メモ AS PREFIX.
001000*                           ［ＲＬ＝  ３２０］
001010 FD  作業ファイル１ RECORD  CONTAINS 320 CHARACTERS.
001020 01 作１－レコード.
001030    03 作１－レコードキー.
001040       05  作１－施術和暦年月日.
001050          07 作１－施術和暦               PIC 9.
001060          07 作１－施術年月.
001070             09 作１－施術年              PIC 9(2).
001080             09 作１－施術月              PIC 9(2).
001090          07 作１－施術日                 PIC 9(2).
001110       05 作１－患者コード.
001120          07 作１－患者番号               PIC 9(6).
001130          07 作１－枝番                   PIC X.
001100    03 作１－レコードデータ.
             05 作１－患者カナ                  PIC X(50).
001140* ＊＊＊　日毎＋部位毎データ　＊＊＊
001150         05  作１－日チェック              PIC 9.
001160         05  作１－部位データ              OCCURS 4.
001170             07  作１－部位計              PIC 9(5).
001180         05  作１－電罨料.
001190             07  作１－罨法金額            PIC 9(4).
001200         05  作１－一部負担金              PIC 9(5).
001200         05  作１－コメント                PIC X(48).
             05 作１－初検                      PIC 9.
003280       05 作１－初診料.
003290          07 作１－初検料                 PIC 9(4).
003300          07 作１－初検加算料             PIC 9(4).
003310          07 作１－休日                   PIC 9.
003320          07 作１－深夜                   PIC 9.
003330          07 作１－時間外                 PIC 9.
003130          07 作１－診療時                 PIC 9(2).
003140          07 作１－診療分                 PIC 9(2).
             05 作１－相談料                    PIC 9(4).
003340       05 作１－再検料                    PIC 9(4).
003450       05 作１－往療料.
003410          07 作１－夜間                   PIC 9.
003420          07 作１－難路                   PIC 9.
003420          07 作１－暴風                   PIC 9.
003470          07 作１－回数                   PIC 9(2).
003440          07 作１－距離                   PIC 9(3)V9.
003480          07 作１－往療料金額             PIC 9(6).
003430          07 作１－往療加算料             PIC 9(5).
003350       05 作１－部位ごと金額              OCCURS 4.
003490          07 作１－金属                   PIC 9.
003360          07 作１－初回処置料             PIC 9(6).
003270          07 作１－冷罨療法.
003280             09 作１－冷罨料              PIC 9(6).
003520          07 作１－金額                   PIC 9(6).
003530       05 作１－情報提供料                PIC 9(6).
001220       05  FILLER                         PIC X(52).
001220*
001340*                         ［ＲＬ＝  １２８］
001350 FD  作業ファイル２ RECORD  CONTAINS 128 CHARACTERS.
001360 01 作２－レコード.
001370    03 作２－レコードキー.
001535       05 作２－施術和暦年月.
001536          07 作２－施術和暦                PIC 9.
001537          07 作２－施術年月.
001538             09 作２－施術年               PIC 9(2).
001539             09 作２－施術月               PIC 9(2).
001460       05 作２－患者コード.
001470          07 作２－患者番号                PIC 9(6).
001480          07 作２－枝番                    PIC X(1).
001490    03 作２－レコードデータ.
             05 作２－月合計.
001551          07 作２－日数１                  PIC 9(2).
001551          07 作２－日数２                  PIC 9(2).
001551          07 作２－日数３                  PIC 9(2).
001551          07 作２－日数４                  PIC 9(2).
001551          07 作２－電罨日数                PIC 9(2).
             05 作２－金属運動ＣＭ               PIC X(60).
001820       05 作２－保険種別                   PIC 9(2).
001150       05 作２－本人家族区分               PIC 9(1).
001500       05 FILLER                           PIC X(43).
      *
001580*                           ［ＲＬ＝  １６］
001590 FD  作業ファイル３ RECORD  CONTAINS 16 CHARACTERS.
001600 01 作３－レコード.
001610    03 作３－レコードキー.
001700       05 作３－患者番号                  PIC 9(6).
001620       05 作３－施術和暦年月.
001630          07 作３－施術和暦               PIC 9.
001640          07 作３－施術年月.
001650             09 作３－施術年              PIC 9(2).
001660             09 作３－施術月              PIC 9(2).
001680    03 作３－レコードデータ.
001720       05 作３－枝番Ｆ                    PIC 9.
             05 作３－保険変更Ｆ                PIC 9.
001520       05 FILLE                           PIC X(3).
      *
001230*----------------------------------------------------------------*
001240******************************************************************
001250*                WORKING-STORAGE SECTION                         *
001260******************************************************************
001270 WORKING-STORAGE         SECTION.
001280 01 キー入力                         PIC X     VALUE SPACE.
001290 01 状態キー                         PIC X(2)  VALUE SPACE.
001300 01 実行キーＷ                       PIC X(3)  VALUE SPACE.
001310 01 終了フラグ                       PIC X(3)  VALUE SPACE.
001320 01 終了フラグ２                     PIC X(3) VALUE SPACE.
001320 01 終了フラグ３                     PIC X(3) VALUE SPACE.
001330 01 施術記録有Ｗ                     PIC X(3) VALUE SPACE.
001340 01 対象フラグ                       PIC X(3) VALUE SPACE.
002350 01 印刷フラグ                       PIC X(3) VALUE SPACE.
001020* **************
001030* * 項目待避用 
001040* **************
001050 01 待避項目ＷＲ.
001060    03 施術和暦年月ＷＲ.
001070       05 施術和暦ＷＲ                 PIC 9(1) VALUE ZERO.
001080       05 施術年ＷＲ                   PIC 9(2) VALUE ZERO.
001090       05 施術月ＷＲ                   PIC 9(2) VALUE ZERO.
001090    03 施術日ＷＲ                      PIC 9(2) VALUE ZERO.
001090    03 開始日ＷＰ                      PIC 9(2) VALUE ZERO.
001090    03 終了日ＷＰ                      PIC 9(2) VALUE ZERO.
001060    03 開始和暦年月日ＷＲ.
001060       05 開始和暦年月ＷＲ.
001070          07 開始和暦ＷＲ              PIC 9(1) VALUE ZERO.
001080          07 開始年ＷＲ                PIC 9(2) VALUE ZERO.
001090          07 開始月ＷＲ                PIC 9(2) VALUE ZERO.
001090       05 開始日ＷＲ                   PIC 9(2) VALUE ZERO.
001060    03 終了和暦年月日ＷＲ.
001060       05 終了和暦年月ＷＲ.
001070          07 終了和暦ＷＲ              PIC 9(1) VALUE ZERO.
001080          07 終了年ＷＲ                PIC 9(2) VALUE ZERO.
001090          07 終了月ＷＲ                PIC 9(2) VALUE ZERO.
001090       05 終了日ＷＲ                   PIC 9(2) VALUE ZERO.
001100    03 種別区分ＷＲ                    PIC X(4) VALUE SPACE.
001110    03 保険種別ＷＲ                    PIC 9(2) VALUE ZERO.
001120    03 患者コードＷＲ.
001130       05 患者番号ＷＲ                 PIC 9(6) VALUE ZERO.
001140       05 枝番ＷＲ                     PIC X(1) VALUE SPACE.
001150    03 本人家族区分ＷＲ                PIC 9(1) VALUE ZERO.
      *
          03 印刷条件ＷＲ                    PIC 9(2) VALUE ZERO.
001490 01 施術日数Ｗ                       PIC 9(2)  VALUE ZERO.
001500 01 ファイル名                       PIC N(10) VALUE SPACE.
001510 01 部位ＣＮＴ                       PIC 9(2)  VALUE ZERO.
001520 01 カウンタ                         PIC 9(1)  VALUE ZERO.
001530*
001540 01 退避項目ＧＷ.
001550   03 レセプト種類Ｗ                 PIC X(4).
001560   03 レセプト種類ＧＷ               PIC X(4).
001570   03 レセプト種別ＧＷ               PIC 9(2).
001580*
001590*
001600 01 明細.
001660    03 初検ＷＲ                      PIC 9     VALUE ZERO.
001610    03 初検料ＷＲ                    PIC 9(4)  VALUE ZERO.
          03 相談料ＷＲ                    PIC 9(4)  VALUE ZERO.
001620    03 初検加算料ＷＲ                PIC 9(4)  VALUE ZERO.
001630    03 休日ＷＲ                      PIC 9     VALUE ZERO.
001640    03 深夜ＷＲ                      PIC 9     VALUE ZERO.
001650    03 時間外ＷＲ                    PIC 9     VALUE ZERO.
001660    03 診療時ＷＲ                    PIC 9(2)  VALUE ZERO.
001670    03 診療分ＷＲ                    PIC 9(2)  VALUE ZERO.
001680    03 再検料ＷＲ                    PIC 9(4)  VALUE ZERO.
001690    03 往療夜間ＷＲ                  PIC 9     VALUE ZERO.
001700    03 往療難路ＷＲ                  PIC 9     VALUE ZERO.
001710    03 往療暴風ＷＲ                  PIC 9     VALUE ZERO.
001720    03 往療回数ＷＲ                  PIC 9(2)  VALUE ZERO.
001730    03 往療距離ＷＲ                  PIC 9(3)V9 VALUE ZERO.
001740    03 往療料ＷＲ                    PIC 9(6)  VALUE ZERO.
001750    03 往療加算料ＷＲ                PIC 9(5)  VALUE ZERO.
001760    03 電罨料ＷＲ                    PIC 9(4)  VALUE ZERO.
001770    03 一部負担金ＷＲ                PIC 9(5)  VALUE ZERO.
001780    03 施術情報提供料ＷＲ            PIC 9(6)  VALUE ZERO.
001790    03 部位Ｗ                        OCCURS 4.
001800       05 初回処置料ＷＲ             PIC 9(5)  VALUE ZERO.
001810       05 後療料ＷＲ                 PIC 9(4)  VALUE ZERO.
001820       05 冷罨料ＷＲ                 PIC 9(4)  VALUE ZERO.
001830       05 温罨料ＷＲ                 PIC 9(4)  VALUE ZERO.
001840       05 電療料ＷＲ                 PIC 9(4)  VALUE ZERO.
001850       05 部位計ＷＲ                 PIC 9(5)  VALUE ZERO.
001860       05 金属ＷＲ                   PIC 9     VALUE ZERO.
001960    03 金属加算ＷＲ                  PIC 9(5)  VALUE ZERO.
001880    03 冷罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
001890    03 冷罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
001900    03 冷罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
001910    03 冷罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
001920    03 冷罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
001930    03 温罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
001940    03 温罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
001950    03 温罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
001960    03 温罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
001970    03 温罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
001980    03 電療料３０ＷＲ                PIC 9(4)  VALUE ZERO.
001990    03 電療料３８ＷＲ                PIC 9(4)  VALUE ZERO.
002000    03 電療料４０ＷＲ                PIC 9(4)  VALUE ZERO.
002010    03 電療料４５ＷＲ                PIC 9(4)  VALUE ZERO.
002020    03 電療料４８ＷＲ                PIC 9(4)  VALUE ZERO.
002030    03 後療料３０ＷＲ                PIC 9(4)  VALUE ZERO.
002040    03 後療料３８ＷＲ                PIC 9(4)  VALUE ZERO.
002050    03 後療料４０ＷＲ                PIC 9(4)  VALUE ZERO.
002060    03 後療料４５ＷＲ                PIC 9(4)  VALUE ZERO.
002070    03 後療料４８ＷＲ                PIC 9(4)  VALUE ZERO.
002080    03 冷罨料計ＷＲ                  PIC 9(4)  VALUE ZERO.
002090    03 温罨料計ＷＲ                  PIC 9(4)  VALUE ZERO.
002100    03 電療料計ＷＲ                  PIC 9(4)  VALUE ZERO.
          03 日数計Ｗ.
001870       05 日数ＷＲ                   PIC 9(2)  OCCURS 5 VALUE ZERO.
001590*
001600 01 金属運動Ｗ.
004680    03 金属副子加算料ＷＲ            PIC ZZ,ZZ9 VALUE ZERO.
          03 金属副子単価Ｗ                PIC Z,ZZ9  VALUE ZERO.
          03 金属副子回数Ｗ                PIC Z9     VALUE ZERO.
          03 運動後療回数Ｗ                PIC Z9     VALUE ZERO.
          03 運動後療単価Ｗ                PIC ZZ9    VALUE ZERO.
          03 運動後療料Ｗ                  PIC Z,ZZ9  VALUE ZERO.
          03 金属運動ＣＭ                  PIC X(60)  VALUE SPACE.
002110*
002130 01 公費種別Ｗ                       PIC 9(2)  VALUE ZERO.
002140 01 助成種別Ｗ                       PIC 9(2)  VALUE ZERO.
002150 01 負担額Ｗ２                       PIC 9(5)  VALUE ZERO.
002160 01 費用額Ｗ２                       PIC 9(5)  VALUE ZERO.
002170 01 施術累計回数Ｗ                   PIC 9(3)  VALUE ZERO.
002180*
002190 01 負傷和暦年月日Ｗ.
002200     03 負傷和暦年月Ｗ.
002210         05 負傷和暦Ｗ               PIC 9.
002220         05 負傷年月Ｗ.
002230            07 負傷年Ｗ              PIC 9(2).
002240            07 負傷月Ｗ              PIC 9(2).
002250     03 負傷日Ｗ                     PIC 9(2).
002260*
002270 01 開始和暦年月日Ｗ.
002280     03 開始和暦年月Ｗ.
002290         05 開始和暦Ｗ               PIC 9.
002300         05 開始年月Ｗ.
002310            07 開始年Ｗ              PIC 9(2).
002320            07 開始月Ｗ              PIC 9(2).
002330     03 開始日Ｗ                     PIC 9(2).
002340*
002350 01 終了和暦年月日Ｗ.
002360     03 終了和暦年月Ｗ.
002370         05 終了和暦Ｗ               PIC 9.
002380         05 終了年月Ｗ.
002390            07 終了年Ｗ              PIC 9(2).
002400            07 終了月Ｗ              PIC 9(2).
002410     03 終了日Ｗ                     PIC 9(2).
002420*
002170 01 患者コードＷ.
002180    03 患者番号Ｗ                    PIC 9(6)  VALUE ZERO.
002190    03 枝番Ｗ                        PIC X     VALUE SPACE.
003180 01 保険種別Ｗ                       PIC 9(2)  VALUE ZERO.
001150 01 本人家族区分Ｗ                   PIC 9(1) VALUE ZERO.
       01 施術和暦年月Ｗ.
          03 施術和暦Ｗ                    PIC 9.
          03 施術年月Ｗ.
             05 施術年Ｗ                   PIC 9(2).
             05 施術月Ｗ                   PIC 9(2).
002430*
002440 01 計算和暦年月Ｗ.
002450     03 計算和暦Ｗ                     PIC 9.
002460     03 計算年月Ｗ.
002470        05 計算年Ｗ                    PIC 9(2).
002480        05 計算月Ｗ                    PIC 9(2).
002490     03 計算日Ｗ                       PIC 9(2).
002500*
002510 01 実日数Ｗ                           PIC 9(2) VALUE ZERO.
002520 01 部位数Ｗ                           PIC 9 VALUE ZERO.
002530 01 最大登録数Ｗ                       PIC 9 VALUE ZERO.
002540 01 部位長Ｗ                           PIC 9(2) VALUE 1.
002618 01 施術西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
002550 01 商Ｗ                               PIC 9(4) VALUE ZERO.
002560 01 余Ｗ                               PIC 9(4) VALUE ZERO.
002570 01 負傷名Ｗ                           PIC N(6) VALUE SPACE.
002580*
002590 01 絶対値Ｗ                           PIC 9(6).
002600 01 絶対値月Ｗ                         PIC 9(3).
002610 01 絶対値月Ｗ２                       PIC 9(3).
002620 01 負傷経過日Ｗ                       PIC 9(3).
002630 01 西暦年Ｗ                           PIC 9(4).
002640 01 西暦年差Ｗ                         PIC 9(2).
002650 01 西暦年差２Ｗ                       PIC 9(2).
002660 01 閏年回数Ｗ                         PIC 9(2).
002670 01 通常年回数Ｗ                       PIC 9(2).
002680 01 通常年絶対値Ｗ                     PIC 9(6).
002690 01 閏年絶対値Ｗ                       PIC 9(6).
002700 01 年絶対値Ｗ                         PIC 9(6).
002710 01 負傷絶対値Ｗ                       PIC 9(6).
002720 01 施術絶対値Ｗ                       PIC 9(6).
       01 加算年Ｗ                           PIC 9(4).
002730*
      */冷罨法の合計を多部位逓減後の合計に変更(長期逓減はありえない。端数は四捨五入）
      *
       01 多部位ＣＮＴ                       PIC 9(2) VALUE ZERO.
      *
       01 施術和暦年月日ＣＷ.
         03 施術和暦年月ＣＷ.
           05 施術和暦ＣＷ                   PIC 9.
           05 施術年月ＣＷ.
              07 施術年ＣＷ                  PIC 9(2).
              07 施術月ＣＷ                  PIC 9(2).
         03 施術日ＣＷ                       PIC 9(2).
      *
       01 逓減率ＳＫＷ.
         03 逓減率ＫＷ  OCCURS 9.
           05 多部位逓減率ＫＷ               PIC 9(3) VALUE ZERO.
      *
      */エラー表示の修正
       01 エラー表示Ｆ                       PIC 9(1)  VALUE ZERO.
      *
002740******************************************************************
002750*                          連結項目                              *
002760******************************************************************
002770*
002780****************
002790* 画面入力情報 *
002800****************
       01 連入－入力データＹＣＨ７１０１ IS EXTERNAL GLOBAL.
          03 連入－開始和暦年月日.
             05 連入－開始和暦                  PIC 9(1).
             05 連入－開始年                    PIC 9(2).
             05 連入－開始月                    PIC 9(2).
             05 連入－開始日                    PIC 9(2).
          03 連入－終了和暦年月日.
             05 連入－終了和暦                  PIC 9(1).
             05 連入－終了年                    PIC 9(2).
             05 連入－終了月                    PIC 9(2).
             05 連入－終了日                    PIC 9(2).
          03 連入－保険種別                     PIC 9(2).
          03 連入－本人家族区分                 PIC 9(1).
          03 連入－患者コード.
             05 連入－患者番号                  PIC 9(6).
             05 連入－枝番                      PIC X(1).
          03 連入－印刷条件                     PIC 9(2).
      */データが無い場合は画面表示しない。0:データなし、1:データあり/050518
002810 01 連入－入力データＹＣＨ７１０２ IS EXTERNAL.
002820    03 連入－対象フラグ                   PIC 9(1).
003310*
006610 01 連メ－キー IS EXTERNAL.
006620    03  連メ－メッセージ                 PIC N(20).
      *
009182* 確認メッセージ用 (２行)
009183 01 連メ７－キー IS EXTERNAL.
009184    03  連メ７－メッセージ１             PIC X(40).
009185    03  連メ７－メッセージ２             PIC X(40).
      *
006700******************************************************************
006710*                      PROCEDURE  DIVISION                       *
006720******************************************************************
006730 PROCEDURE               DIVISION.
006740************
006750*           *
006760* 初期処理   *
006770*           *
006780************
006790     PERFORM ファイルオープン.
006800     PERFORM 連結項目待避.
           MOVE ZERO TO 連入－対象フラグ.
006810************
006820*           *
006830* 主処理     *
006840*           *
006850************
006900     PERFORM 作業ファイル作成.
006920************
006930*           *
006940* 終了処理   *
006950*           *
006960************
006970     PERFORM 終了処理.
      */対象がない場合は印刷しないように/050610
           IF (連入－対象フラグ NOT = ZERO)
               MOVE ZERO TO PROGRAM-STATUS
           ELSE
               MOVE 99   TO PROGRAM-STATUS
           END-IF.
006990     EXIT PROGRAM.
007000*
007010*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
007020*================================================================*
007030 連結項目待避 SECTION.
007040*
001700     INITIALIZE 待避項目ＷＲ.
001720     MOVE 連入－保険種別      TO 保険種別ＷＲ.
001730     MOVE 連入－患者番号      TO 患者番号ＷＲ.
001740     MOVE 連入－枝番          TO 枝番ＷＲ.
001750     MOVE 連入－開始和暦      TO 開始和暦ＷＲ.
001760     MOVE 連入－開始年        TO 開始年ＷＲ.
001770     MOVE 連入－開始月        TO 開始月ＷＲ.
001770     MOVE 連入－開始日        TO 開始日ＷＲ.
001750     MOVE 連入－終了和暦      TO 終了和暦ＷＲ.
001760     MOVE 連入－終了年        TO 終了年ＷＲ.
001770     MOVE 連入－終了月        TO 終了月ＷＲ.
001770     MOVE 連入－終了日        TO 終了日ＷＲ.
001780     MOVE 連入－本人家族区分  TO 本人家族区分ＷＲ.
      *
           MOVE 連入－印刷条件      TO 印刷条件ＷＲ.
007160*================================================================*
007170 ファイルオープン SECTION.
007180*
012140     OPEN INPUT   元号マスタ
012150         MOVE NC"元号" TO ファイル名.
012160         PERFORM オープンチェック.
007190     OPEN INPUT 施術記録Ｆ.
007200         MOVE NC"施記" TO ファイル名.
007210         PERFORM オープンチェック.
007220     OPEN INPUT 受診者情報Ｆ.
007230         MOVE NC"受診" TO ファイル名.
007240         PERFORM オープンチェック.
007250     OPEN INPUT 負傷データＦ.
007260         MOVE NC"負傷" TO ファイル名.
007270         PERFORM オープンチェック.
007870     OPEN INPUT レセプトＦ.
007880         MOVE NC"レセ"         TO ファイル名.
007890         PERFORM オープンチェック.
003060     OPEN INPUT 会計データＦ.
003070         MOVE NC"会計" TO ファイル名.
003080         PERFORM オープンチェック.
007280     OPEN INPUT 計算マスタ.
007290         MOVE NC"計算" TO ファイル名.
007300         PERFORM オープンチェック.
008630     OPEN INPUT メモファイル.
008640         MOVE NC"メモ"         TO ファイル名.
008650         PERFORM オープンチェック.
      */0410
008010     OPEN OUTPUT 作業ファイル１.
008020         MOVE NC"作業" TO ファイル名.
008030         PERFORM オープンチェック.
002210     OPEN OUTPUT 作業ファイル２
002220          MOVE NC"作２" TO ファイル名
002230          PERFORM オープンチェック
002210     OPEN OUTPUT 作業ファイル３.
002220          MOVE NC"作３" TO ファイル名.
002230          PERFORM オープンチェック.
           CLOSE 作業ファイル３.
002210     OPEN I-O 作業ファイル３.
002220          MOVE NC"作３" TO ファイル名.
002230          PERFORM オープンチェック.
007310*================================================================*
007320 オープンチェック SECTION.
007330*
007340     IF 状態キー  NOT =  "00"
007350         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
007360         DISPLAY NC"状態キー：" 状態キー         UPON CONS
007370         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
003131*-----------------------------------------*
003132         CALL "actcshm"  WITH C LINKAGE
003133*-----------------------------------------*
007390         ACCEPT  キー入力 FROM CONS
007400         PERFORM ファイル閉鎖
007410         MOVE 99 TO PROGRAM-STATUS
007420         EXIT PROGRAM.
007430*================================================================*
007440 エラー表示 SECTION.
007450*
007460     DISPLAY NC"状態キー" 状態キー  UPON CONS.
007470     DISPLAY NC"５ファイル書込エラー：" ファイル名 UPON CONS.
007480     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
007490     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
003131*-----------------------------------------*
003132     CALL "actcshm"  WITH C LINKAGE.
003133*-----------------------------------------*
007500     ACCEPT  キー入力 FROM CONS.
007510     PERFORM ファイル閉鎖.
007520     MOVE 99 TO PROGRAM-STATUS.
007530     EXIT PROGRAM.
007980*================================================================*
007990 作業ファイル作成 SECTION.
008000*
008310     MOVE 開始和暦ＷＲ  TO 受－施術和暦.
008320     MOVE 開始年ＷＲ    TO 受－施術年.
008330     MOVE 開始月ＷＲ    TO 受－施術月.
           MOVE SPACE             TO 受－患者カナ.
           MOVE SPACE             TO 受－患者コード.
008100     START 受診者情報Ｆ   KEY IS >= 受－施術和暦年月
008110                                    受－患者コード
008120     END-START.
008130     IF 状態キー = "00"
003840         MOVE SPACE  TO 終了フラグ
008400         PERFORM 受診者情報Ｆ読込
003860         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
003870                       ( 受－施術和暦年月 > 終了和暦年月ＷＲ )
      */印刷対象条件の判定
001110*             IF ((保険種別ＷＲ     NOT = ZERO)              AND
      *                 (保険種別ＷＲ     NOT = 受－保険種別))     OR
HILO               IF  ((患者番号ＷＲ     NOT = ZERO)              AND
HILO                   (患者番号ＷＲ   NOT = 受－患者番号))   OR
001150                ((本人家族区分ＷＲ NOT = ZERO)              AND
                       (本人家族区分ＷＲ NOT = 受－本人家族区分))
      */対象外
                       CONTINUE
                   ELSE
                       MOVE "YES" TO 対象フラグ
                       MOVE 受－患者コード   TO 患者コードＷ
                       MOVE 受－保険種別     TO 保険種別Ｗ
                       MOVE 受－本人家族区分 TO 本人家族区分Ｗ
                       IF 印刷条件ＷＲ NOT = ZERO
                           PERFORM 印刷条件判定
                       END-IF
                       IF 対象フラグ = "YES"
008150                     PERFORM データチェック
008160                     IF 実行キーＷ = "YES"
                               MOVE 1 TO 連入－対象フラグ
                               MOVE 受－施術和暦年月 TO 施術和暦年月ＷＲ
                               IF 受－施術和暦年月 = 開始和暦年月ＷＲ
                                   MOVE 開始日ＷＲ TO 開始日Ｗ
                               ELSE
                                   MOVE 1          TO 開始日Ｗ
                               END-IF
                               IF 受－施術和暦年月 = 終了和暦年月ＷＲ
                                   MOVE 終了日ＷＲ TO 終了日Ｗ
                               ELSE
                                   PERFORM 月末日取得
                                   MOVE 終了日ＷＰ   TO 終了日Ｗ
                               END-IF
                               PERFORM 対象チェック
008890                         IF 印刷フラグ = "YES"
                                  MOVE ZERO TO 日数計Ｗ
007930                            PERFORM 負傷データ取得
001110                            IF ((患者番号ＷＲ     NOT = ZERO) AND
                                      (患者コードＷＲ   NOT = 受－患者コード))
                                     CONTINUE
                                  ELSE
003910                               PERFORM 作１レコードセット
                                  END-IF
003910                            PERFORM 作２レコードセット
008910                         END-IF
                           END-IF
                       END-IF
008480             END-IF
003930             PERFORM 受診者情報Ｆ読込
               END-PERFORM
003910         PERFORM 作３レコードセット
008240     ELSE
               MOVE ZERO TO 連入－対象フラグ
008270     END-IF.
008280*
008290*     CLOSE 作業ファイル１.
004030*================================================================*
004040 対象チェック SECTION.
004050*
009020     MOVE SPACE TO 印刷フラグ.
009150     PERFORM VARYING 施術日ＷＲ FROM 開始日Ｗ BY 1
009160             UNTIL ( 施術日ＷＲ > 終了日Ｗ )
009180         MOVE 受－患者番号  TO 施記－患者番号
009190         MOVE 受－枝番      TO 施記－枝番
009200         MOVE 受－施術和暦  TO 施記－施術和暦
009210         MOVE 受－施術年    TO 施記－施術年
009220         MOVE 受－施術月    TO 施記－施術月
009230         MOVE 施術日ＷＲ    TO 施記－施術日
009240         READ 施術記録Ｆ
009250         NOT INVALID KEY
009310             MOVE "YES" TO 印刷フラグ
009380         END-READ
009390     END-PERFORM.
004030*================================================================*
004040 作１レコードセット SECTION.
004050*
008170     PERFORM VARYING 施術日ＷＲ FROM 1 BY 1 UNTIL 施術日ＷＲ > 終了日Ｗ
      */高速化の為、通院日の無い日は計算しないように変更
               MOVE ZERO TO 一部負担金ＷＲ 電罨料ＷＲ
               PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 4
                   MOVE ZERO TO 部位計ＷＲ(カウンタ)
               END-PERFORM
      *
               MOVE 受－患者番号  TO 施記－患者番号
               MOVE 受－枝番      TO 施記－枝番
               MOVE 受－施術和暦  TO 施記－施術和暦
               MOVE 受－施術年    TO 施記－施術年
               MOVE 受－施術月    TO 施記－施術月
               MOVE 施術日ＷＲ    TO 施記－施術日
               READ 施術記録Ｆ
               NOT INVALID KEY
008180             PERFORM レセプト呼出し２
008190             PERFORM 項目ごと計算
               END-READ
008200         PERFORM 作業ファイル１セット
008210         PERFORM 作１レコード書込
008220     END-PERFORM.
008300*================================================================*
008310 作業ファイル１セット SECTION.
008320*
008330     INITIALIZE 作１－レコード.
      */通院していない日は計算ＰＧを呼ばない為、値は不定。0304
008050     MOVE 施術和暦ＷＲ       TO 作１－施術和暦.
008060     MOVE 施術年ＷＲ         TO 作１－施術年.
008070     MOVE 施術月ＷＲ         TO 作１－施術月.
      *
008370     MOVE 施術日ＷＲ         TO 作１－施術日.
008380     MOVE 受－患者コード     TO 作１－患者コード.
008390     MOVE 一部負担金ＷＲ     TO 作１－一部負担金.
008400     MOVE 電罨料ＷＲ         TO 作１－電罨料.
008410     IF 電罨料ＷＲ NOT = ZERO
008420         COMPUTE 日数ＷＲ(5) = 日数ＷＲ(5) + 1
008430     END-IF.
008440     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 4
008450         MOVE 部位計ＷＲ(カウンタ)     TO 作１－部位計(カウンタ)
008460         IF 部位計ＷＲ(カウンタ) NOT = ZERO
008470             MOVE 1          TO 作１－日チェック
008480             COMPUTE 日数ＷＲ(カウンタ) = 日数ＷＲ(カウンタ) + 1
008640             MOVE 初検ＷＲ   TO 作１－初検
008490         END-IF
008500     END-PERFORM.
010920     MOVE 1                  TO メモ－制御区分.
010930     MOVE 受－患者コード     TO メモ－患者コード.
010940     MOVE 受－施術和暦       TO メモ－施術和暦.
010950     MOVE 受－施術年         TO メモ－施術年.
010960     MOVE 受－施術月         TO メモ－施術月.
010970     MOVE 施術日ＷＲ         TO メモ－施術日.
010980     READ メモファイル
010990     NOT INVALID KEY
011000         MOVE メモ－施術コメント TO 作１－コメント
011010     END-READ.
007950     MOVE 初検料ＷＲ             TO  作１－初検料
           MOVE 相談料ＷＲ             TO  作１－相談料
007960     MOVE 再検料ＷＲ             TO  作１－再検料
008050     MOVE 初検加算料ＷＲ         TO  作１－初検加算料.
008060     MOVE 休日ＷＲ               TO  作１－休日.
008070     MOVE 深夜ＷＲ               TO  作１－深夜.
008080     MOVE 時間外ＷＲ             TO  作１－時間外.
007800     MOVE 診療時ＷＲ             TO  作１－診療時
007810     MOVE 診療分ＷＲ             TO  作１－診療分
008100     MOVE 往療回数ＷＲ           TO  作１－回数.
008110     MOVE 往療距離ＷＲ           TO  作１－距離.
008120     MOVE 往療料ＷＲ             TO  作１－往療料金額.
008130     MOVE 往療加算料ＷＲ         TO  作１－往療加算料.
008140     MOVE 往療夜間ＷＲ           TO  作１－夜間.
008150     MOVE 往療暴風ＷＲ           TO  作１－暴風.
008150     MOVE 往療難路ＷＲ           TO  作１－難路.
008210     MOVE 施術情報提供料ＷＲ     TO  作１－情報提供料.
008160*
007970     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
007980               UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR (部位ＣＮＴ > 4)
007930        MOVE 金属ＷＲ(部位ＣＮＴ)       TO 作１－金属(部位ＣＮＴ)
007990        MOVE 初回処置料ＷＲ(部位ＣＮＴ) TO 作１－初回処置料(部位ＣＮＴ)
007950        MOVE 冷罨料ＷＲ(部位ＣＮＴ)     TO 作１－冷罨料(部位ＣＮＴ)
008000     END-PERFORM.
008040*
007760*================================================================*
007770 作２レコードセット SECTION.
007780*
008270     INITIALIZE 作２－レコード.
008580     MOVE 施術和暦ＷＲ     TO 作２－施術和暦.
008590     MOVE 施術年ＷＲ       TO 作２－施術年.
008600     MOVE 施術月ＷＲ       TO 作２－施術月.
004130     MOVE 受－患者コード   TO 作２－患者コード.
012060     MOVE 日数ＷＲ(1)      TO 作２－日数１.
012060     MOVE 日数ＷＲ(2)      TO 作２－日数２.
012060     MOVE 日数ＷＲ(3)      TO 作２－日数３.
012060     MOVE 日数ＷＲ(4)      TO 作２－日数４.
012060     MOVE 日数ＷＲ(5)      TO 作２－電罨日数.
           PERFORM 金属運動情報セット.
           MOVE 金属運動ＣＭ     TO 作２－金属運動ＣＭ.
           MOVE 保険種別Ｗ       TO 作２－保険種別.
           MOVE 本人家族区分Ｗ   TO 作２－本人家族区分.
           PERFORM 作２レコード書込.
008220*
004030*================================================================*
004040 金属運動情報セット SECTION.
004050*
           IF 受－助成種別 NOT = ZERO
              MOVE  3   TO レセ－レセ種別
           ELSE
              IF 受－公費種別 NOT = ZERO
                 MOVE  2   TO レセ－レセ種別
              ELSE
                 IF 受－保険種別 = 85
                    MOVE  7   TO レセ－レセ種別
                 ELSE
                    MOVE  1   TO レセ－レセ種別
                 END-IF
              END-IF
           END-IF.
005200     MOVE 受－施術和暦  TO レセ－施術和暦.
005210     MOVE 受－施術年    TO レセ－施術年.  
005220     MOVE 受－施術月    TO レセ－施術月.  
005230     MOVE 受－患者番号  TO レセ－患者番号.
005240     MOVE 受－枝番      TO レセ－枝番.    
           READ レセプトＦ
           INVALID KEY
               MOVE SPACE     TO レセ－レコード
           END-READ.
011200     MOVE レセ－金属副子加算料 TO 金属副子加算料ＷＲ.
           MOVE レセ－金属副子回数   TO 金属副子回数Ｗ.
           MOVE レセ－運動後療回数   TO 運動後療回数Ｗ.
           MOVE レセ－運動後療単価   TO 運動後療単価Ｗ.
           MOVE レセ－運動後療料     TO 運動後療料Ｗ.
           IF レセ－金属副子回数 NOT = ZERO
               COMPUTE 金属副子単価Ｗ = レセ－金属副子加算料 / レセ－金属副子回数
           END-IF.
           IF レセ－金属副子加算料 NOT = ZERO
              STRING "金属副子 "        DELIMITED BY SIZE
                     金属副子単価Ｗ     DELIMITED BY SIZE
                     "×"               DELIMITED BY SIZE
                     金属副子回数Ｗ     DELIMITED BY SIZE
                     "＝"               DELIMITED BY SIZE
                     金属副子加算料ＷＲ DELIMITED BY SIZE
                     "  　"             DELIMITED BY SIZE
                INTO 金属運動ＣＭ
              END-STRING
              IF レセ－運動後療料 NOT = ZERO
                 STRING 金属運動ＣＭ    DELIMITED BY "　"
                        "運動後療　"    DELIMITED BY SIZE
                        運動後療単価Ｗ  DELIMITED BY SIZE
                        "×"            DELIMITED BY SIZE
                        運動後療回数Ｗ  DELIMITED BY SIZE
                        "＝"            DELIMITED BY SIZE
                        運動後療料Ｗ    DELIMITED BY SIZE
                   INTO 金属運動ＣＭ
                 END-STRING
              END-IF
           ELSE
              IF レセ－運動後療料 NOT = ZERO
                 STRING "運動後療　"    DELIMITED BY SIZE
                        運動後療単価Ｗ  DELIMITED BY SIZE
                        "×"            DELIMITED BY SIZE
                        運動後療回数Ｗ  DELIMITED BY SIZE
                        "＝"            DELIMITED BY SIZE
                        運動後療料Ｗ    DELIMITED BY SIZE
                   INTO 金属運動ＣＭ
                 END-STRING
              END-IF
           END-IF.
008220*
004030*================================================================*
004040 作３レコードセット SECTION.
004050*
008290     CLOSE 作業ファイル２.
002210     OPEN INPUT 作業ファイル２
002220          MOVE NC"作２" TO ファイル名
002230          PERFORM オープンチェック.
      *
           MOVE ZERO       TO 作２－施術和暦年月.
           MOVE ZERO       TO 作２－患者番号.
           MOVE SPACE      TO 作２－枝番.
008100     START 作業ファイル２   KEY IS >= 作２－患者コード
008110                                      作２－施術和暦年月
008120     END-START.
008130     IF 状態キー = "00"
003840         MOVE SPACE  TO 終了フラグ２
008400         PERFORM 作業ファイル２読込
003860         PERFORM UNTIL ( 終了フラグ２ = "YES" )
                  MOVE 作２－患者コード     TO 患者コードＷ
                  MOVE 作２－保険種別       TO 保険種別Ｗ
                  MOVE 作２－本人家族区分   TO 本人家族区分Ｗ
                  MOVE 作２－施術和暦年月   TO 施術和暦年月Ｗ
003860            PERFORM UNTIL ( 終了フラグ２ = "YES" ) OR
                                ( 作２－患者番号 NOT = 患者番号Ｗ )
008270               INITIALIZE 作３－レコード
                     IF (作２－保険種別 NOT = 70 AND 80 AND 85 AND 90)
004130                  MOVE 作２－患者番号       TO 作３－患者番号
                        MOVE 作２－施術和暦年月   TO 作３－施術和暦年月
009240                  READ 作業ファイル３
                        INVALID KEY
                           IF ((作２－枝番 >= "A") AND (枝番Ｗ <= "Z"))
                              MOVE 1             TO 作３－枝番Ｆ
                           END-IF
001110                     IF (保険種別Ｗ     NOT = 作２－保険種別)     OR
001150                        (本人家族区分Ｗ NOT = 作２－本人家族区分)
                              MOVE 1             TO 作３－保険変更Ｆ
                           END-IF
                           WRITE 作３－レコード
                           END-WRITE
                        NOT INVALID KEY
                           IF ((作２－枝番 >= "A") AND (枝番Ｗ <= "Z"))
                              MOVE 1             TO 作３－枝番Ｆ
                           END-IF
001110                     IF (保険種別Ｗ     NOT = 作２－保険種別)     OR
001150                        (本人家族区分Ｗ NOT = 作２－本人家族区分)
                              MOVE 1             TO 作３－保険変更Ｆ
                           END-IF
                           REWRITE 作３－レコード
                           END-REWRITE
                        END-READ
                     END-IF
                     IF 作２－施術和暦年月 NOT = 施術和暦年月Ｗ
                        MOVE 作２－保険種別       TO 保険種別Ｗ
                        MOVE 作２－本人家族区分   TO 本人家族区分Ｗ
                     END-IF
                     MOVE 作２－施術和暦年月      TO 施術和暦年月Ｗ
008400               PERFORM 作業ファイル２読込
                  END-PERFORM
               END-PERFORM
           END-IF.
002180*
008510*================================================================*
008520 データチェック SECTION.
008530*
008540     MOVE SPACE  TO 実行キーＷ.
008550* *****************************************************************
008560* * 負傷部位有無チェック：部位数 = 0 の場合データ作成対象としない *
008570* *****************************************************************
008580     MOVE 受－施術和暦   TO 負－施術和暦.
008590     MOVE 受－施術年     TO 負－施術年.
008600     MOVE 受－施術月     TO 負－施術月.
008610     MOVE 受－患者番号   TO 負－患者番号.
008620     MOVE 受－枝番       TO 負－枝番.
008630     READ 負傷データＦ
008640     INVALID KEY
008650         MOVE SPACE  TO 実行キーＷ
008660     NOT INVALID KEY
008670         IF 負－部位数 NOT = ZERO
008680*        *************************************************************
008690*        * 施術記録チェック：通院数 = 0 の場合データ作成対象としない *
008700*        *************************************************************
008710             MOVE 負－患者番号  TO 施記－患者番号
008720             MOVE 負－枝番      TO 施記－枝番
008730             MOVE 負－施術和暦  TO 施記－施術和暦
008740             MOVE 負－施術年    TO 施記－施術年
008750             MOVE 負－施術月    TO 施記－施術月
008760             MOVE ZERO          TO 施記－施術日
008770             START 施術記録Ｆ   KEY IS >= 施記－患者コード
008780                                          施記－施術和暦年月日
008790             END-START
008800             IF 状態キー = "00"
008810                 MOVE SPACE TO 終了フラグ２
008820                 MOVE SPACE TO 施術記録有Ｗ
008830                 PERFORM 施術記録Ｆ読込
008840                 PERFORM UNTIL ( 終了フラグ２         = "YES"          ) OR
008850                               ( 施記－患者コード NOT = 負－患者コード ) OR
008860                               ( 施記－施術和暦   NOT = 負－施術和暦   ) OR
008870                               ( 施記－施術年     NOT = 負－施術年     ) OR
008880                               ( 施記－施術月     NOT = 負－施術月     ) OR
008890                               ( 施術記録有Ｗ         = "YES"          )
008900                     MOVE "YES"  TO 施術記録有Ｗ
008910                     MOVE "YES"  TO 実行キーＷ
008920                 END-PERFORM
008930             ELSE
008940                 MOVE SPACE  TO 実行キーＷ
008950             END-IF
008960         ELSE
009000             MOVE SPACE  TO 実行キーＷ
009010         END-IF
009020     END-READ.
009030*
009380*================================================================*
009390 レセプト呼出し２ SECTION.
009400*
009180     MOVE 受－患者番号  TO 会－患者番号.
009190     MOVE 受－枝番      TO 会－枝番.
009200     MOVE 受－施術和暦  TO 会－施術和暦.
009210     MOVE 受－施術年    TO 会－施術年.
009220     MOVE 受－施術月    TO 会－施術月.
009230     MOVE 施術日ＷＲ    TO 会－施術日.
009240     READ 会計データＦ
           INVALID KEY
               MOVE SPACE     TO 会－レコード
           END-READ.
009710*
009750     MOVE 会－一部負担金     TO 一部負担金ＷＲ.
009770*
010640*================================================================*
010650 負傷データ取得 SECTION.
010660*
010670**************************************************
010680* 連結データから負傷データＦより以下の情報を取得 *
010690* ● 金属副子 ....区分によりチェックに"○"を格納 *
010700**************************************************
010710     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
010720     MOVE 施術年ＷＲ         TO 負－施術年.
010730     MOVE 施術月ＷＲ         TO 負－施術月.
010740     MOVE 患者コードＷ       TO 負－患者コード.
010750     READ 負傷データＦ
010760     INVALID KEY
010770         CONTINUE
010780*            /* ありえない */
010790     NOT INVALID KEY
010800         MOVE 負－部位数                   TO 部位数Ｗ
010920*
010940     END-READ.
010950*
011430*================================================================*
011440 項目ごと計算 SECTION.
011450***********************************************
011460* 料金データセット                            *
011470***********************************************
011180     MOVE 会－初検料                 TO  初検料ＷＲ.
           MOVE 会－初検時相談料           TO  相談料ＷＲ.
011190*
011200     MOVE 会－初検加算料             TO  初検加算料ＷＲ.
011210     MOVE 会－休日                   TO  休日ＷＲ.
011220     MOVE 会－深夜                   TO  深夜ＷＲ.
011230     MOVE 会－時間外                 TO  時間外ＷＲ.
011240     MOVE 会－再検料                 TO  再検料ＷＲ.
011270     MOVE 会－往療回数               TO  往療回数ＷＲ.
011280     MOVE 会－往療距離               TO  往療距離ＷＲ.
011290     MOVE 会－往療料                 TO  往療料ＷＲ.
011300     MOVE 会－往療加算料             TO  往療加算料ＷＲ.
011310     MOVE 会－夜間                   TO  往療夜間ＷＲ.
011310     MOVE 会－難路                   TO  往療夜間ＷＲ.
011320     MOVE 会－暴風雨雪               TO  往療暴風ＷＲ.
011340*
011350     MOVE 会－金属副子加算料         TO  金属加算ＷＲ.
011390*
011400     MOVE 会－施術情報提供料         TO  施術情報提供料ＷＲ.
      *
           MOVE 会－患者コード             TO 施記－患者コード.
           MOVE 会－施術和暦年月日         TO 施記－施術和暦年月日.
           READ 施術記録Ｆ
           NOT INVALID KEY
               IF 施記－診療区分 = 2
                   MOVE 1                  TO 初検ＷＲ
               ELSE
                   MOVE 0                  TO 初検ＷＲ
               END-IF
               MOVE 施記－受付時           TO 診療時ＷＲ
               MOVE 施記－受付分           TO 診療分ＷＲ
011480         PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 4
                   MOVE 施記－金属副子区分(カウンタ) TO 金属ＷＲ(カウンタ)
011500         END-PERFORM
           END-READ.
011410********************
011420* 初回処置料セット *
011430********************
011480     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 4
011490         MOVE 会－初回処置料(カウンタ) TO 初回処置料ＷＲ(カウンタ)
011500     END-PERFORM.
011510     MOVE 会－後療料１     TO 後療料ＷＲ(1).
011520     MOVE 会－後療料２     TO 後療料ＷＲ(2).
011530     MOVE 会－後療料３８   TO 後療料３８ＷＲ.
011540     MOVE 会－後療料３０   TO 後療料３０ＷＲ.
011550     COMPUTE 後療料ＷＲ(3)   = 後療料３８ＷＲ   + 後療料３０ＷＲ.
011560     MOVE 会－後療料４５   TO 後療料４５ＷＲ.
011570     MOVE 会－後療料４８   TO 後療料４８ＷＲ.
011580     MOVE 会－後療料４０   TO 後療料４０ＷＲ.
011590     COMPUTE 後療料ＷＲ(4)   = 後療料４５ＷＲ   + 後療料４８ＷＲ   + 後療料４０ＷＲ.
011600     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 4
011610         COMPUTE 部位計ＷＲ(カウンタ) = 後療料ＷＲ(カウンタ) + 初回処置料ＷＲ(カウンタ)
011620     END-PERFORM.
011630********************
011640* 逓減毎料金セット *
011650********************
011660     MOVE 会－冷罨法料１             TO 冷罨料ＷＲ(1).
011670     MOVE 会－冷罨法料２             TO 冷罨料ＷＲ(2).
011680     MOVE 会－冷罨法料３８           TO 冷罨法料３８ＷＲ.
011690     MOVE 会－冷罨法料３０           TO 冷罨法料３０ＷＲ.
011700     COMPUTE 冷罨料ＷＲ(3)   = 冷罨法料３８ＷＲ  + 冷罨法料３０ＷＲ.
011710     MOVE 会－冷罨法料４５           TO 冷罨法料４５ＷＲ.
011720     MOVE 会－冷罨法料４８           TO 冷罨法料４８ＷＲ.
011730     MOVE 会－冷罨法料４０           TO 冷罨法料４０ＷＲ.
011740     COMPUTE 冷罨料ＷＲ(4)   = 冷罨法料４５ＷＲ  + 冷罨法料４８ＷＲ  + 冷罨法料４０ＷＲ.
011750     COMPUTE 冷罨料計ＷＲ = 冷罨料ＷＲ(1) + 冷罨料ＷＲ(2) + 冷罨料ＷＲ(3) + 冷罨料ＷＲ(4).
      *
      *     PERFORM 計算情報取得.
      */冷罨法の合計を多部位逓減後の合計に変更(長期逓減はありえない。端数は四捨五入）
      *     COMPUTE 冷罨法料３８ＷＲ ROUNDED = 冷罨法料３８ＷＲ * 多部位逓減率ＫＷ(3) / 100
011370*     COMPUTE 冷罨料ＷＲ(3)   = 冷罨法料３８ＷＲ  + 冷罨法料３０ＷＲ.
      */冷罨法の合計を多部位逓減後の合計に変更(長期逓減はありえない。端数は四捨五入）
      *     COMPUTE 冷罨法料４５ＷＲ ROUNDED = 冷罨法料４５ＷＲ * 多部位逓減率ＫＷ(4) / 100
      *     COMPUTE 冷罨法料４８ＷＲ ROUNDED = 冷罨法料４８ＷＲ * 多部位逓減率ＫＷ(3) / 100
011410*     COMPUTE 冷罨料ＷＲ(4)   = 冷罨法料４５ＷＲ  + 冷罨法料４８ＷＲ  + 冷罨法料４０ＷＲ.
011760*
011770     MOVE 会－温罨法料１             TO 温罨料ＷＲ(1).
011780     MOVE 会－温罨法料２             TO 温罨料ＷＲ(2).
011790     MOVE 会－温罨法料３８           TO 温罨法料３８ＷＲ.
011800     MOVE 会－温罨法料３０           TO 温罨法料３０ＷＲ.
011810     COMPUTE 温罨料ＷＲ(3)   = 温罨法料３８ＷＲ  + 温罨法料３０ＷＲ.
011820     MOVE 会－温罨法料４５           TO 温罨法料４５ＷＲ.
011830     MOVE 会－温罨法料４８           TO 温罨法料４８ＷＲ.
011840     MOVE 会－温罨法料４０           TO 温罨法料４０ＷＲ.
011850     COMPUTE 温罨料ＷＲ(4)   = 温罨法料４５ＷＲ  + 温罨法料４８ＷＲ  + 温罨法料４０ＷＲ.
011860     COMPUTE 温罨料計ＷＲ = 温罨料ＷＲ(1) + 温罨料ＷＲ(2) + 温罨料ＷＲ(3) + 温罨料ＷＲ(4).
011870*
011880     MOVE 会－電療料１             TO 電療料ＷＲ(1).
011890     MOVE 会－電療料２             TO 電療料ＷＲ(2).
011900     MOVE 会－電療料３８           TO 電療料３８ＷＲ.
011910     MOVE 会－電療料３０           TO 電療料３０ＷＲ.
011920     COMPUTE 電療料ＷＲ(3)  = 電療料３８ＷＲ  + 電療料３０ＷＲ.
011930     MOVE 会－電療料４５           TO 電療料４５ＷＲ.
011940     MOVE 会－電療料４８           TO 電療料４８ＷＲ.
011950     MOVE 会－電療料４０           TO 電療料４０ＷＲ.
011960     COMPUTE 電療料ＷＲ(4)  = 電療料４５ＷＲ  + 電療料４８ＷＲ  + 電療料４０ＷＲ.
011970     COMPUTE 電療料計ＷＲ = 電療料ＷＲ(1) + 電療料ＷＲ(2) + 電療料ＷＲ(3) + 電療料ＷＲ(4).
011980*
011990     COMPUTE 電罨料ＷＲ = 冷罨料計ＷＲ + 温罨料計ＷＲ + 電療料計ＷＲ.
012000*
002820*================================================================*
       印刷条件判定 SECTION.
      *
           MOVE SPACE TO 対象フラグ
      */労災自賠責のデータは対象外
           IF 受－保険種別 NOT = 70 AND 80
               EVALUATE 印刷条件ＷＲ
               WHEN 01
                   PERFORM 初検判定
               WHEN 02
                   PERFORM 後療のみ判定
               WHEN 03
                   PERFORM 終了判定
               WHEN 04
                   PERFORM 初検判定
                   IF 対象フラグ = "YES"
                       PERFORM 終了判定
                   END-IF
               WHEN OTHER
                   CONTINUE
               END-EVALUATE
           END-IF.
040770*================================================================*
       初検判定 SECTION.
      *
014120     MOVE 受－患者番号  TO 施記－患者番号
014130     MOVE 受－枝番      TO 施記－枝番
014140     MOVE 受－施術和暦  TO 施記－施術和暦
014150     MOVE 受－施術年    TO 施記－施術年
014160     MOVE 受－施術月    TO 施記－施術月
014170     MOVE ZERO          TO 施記－施術日
014180     START 施術記録Ｆ   KEY IS >= 施記－患者コード
014190                                  施記－施術和暦年月日
014200     END-START
014210     IF 状態キー = "00"
014120         MOVE SPACE TO 終了フラグ２
014140         PERFORM 施術記録Ｆ読込
014150         PERFORM UNTIL (終了フラグ２         = "YES"         ) OR
014160                       (施記－患者コード NOT = 受－患者コード) OR
014170                       (施記－施術和暦   NOT = 受－施術和暦  ) OR
014180                       (施記－施術年     NOT = 受－施術年    ) OR
014190                       (施記－施術月     NOT = 受－施術月    )
      *
                   IF 施記－診療区分 = 2
                       MOVE "YES" TO 対象フラグ
                       MOVE "YES" TO 終了フラグ２
                   END-IF
040620             PERFORM 施術記録Ｆ読込
               END-PERFORM
           END-IF.
040770*================================================================*
       後療のみ判定 SECTION.
      *
014120     MOVE 受－患者番号  TO 施記－患者番号
014130     MOVE 受－枝番      TO 施記－枝番
014140     MOVE 受－施術和暦  TO 施記－施術和暦
014150     MOVE 受－施術年    TO 施記－施術年
014160     MOVE 受－施術月    TO 施記－施術月
014170     MOVE ZERO          TO 施記－施術日
014180     START 施術記録Ｆ   KEY IS >= 施記－患者コード
014190                                  施記－施術和暦年月日
014200     END-START
014210     IF 状態キー = "00"
014120         MOVE SPACE TO 終了フラグ２
014140         PERFORM 施術記録Ｆ読込
014150         PERFORM UNTIL (終了フラグ２         = "YES"         ) OR
014160                       (施記－患者コード NOT = 受－患者コード) OR
014170                       (施記－施術和暦   NOT = 受－施術和暦  ) OR
014180                       (施記－施術年     NOT = 受－施術年    ) OR
014190                       (施記－施術月     NOT = 受－施術月    )
      *
                   IF 施記－診療区分 = 1
                       MOVE "YES" TO 対象フラグ
                   ELSE
                       MOVE SPACE TO 対象フラグ
                       MOVE "YES" TO 終了フラグ２
                   END-IF
040620             PERFORM 施術記録Ｆ読込
               END-PERFORM
           END-IF.
040770*================================================================*
       終了判定 SECTION.
      *
           MOVE "YES" TO 対象フラグ.
      *
014190     MOVE 受－施術和暦   TO 負－施術和暦.
014200     MOVE 受－施術年     TO 負－施術年.
014210     MOVE 受－施術月     TO 負－施術月.
014120     MOVE 受－患者番号   TO 負－患者番号.
014130     MOVE 受－枝番       TO 負－枝番.
014140     READ 負傷データＦ
014150     INVALID KEY
014160         MOVE SPACE  TO 対象フラグ
014170     NOT INVALID KEY
               IF 負－部位数 = ZERO
                   MOVE SPACE TO 対象フラグ
               ELSE
020600             PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
020610                      UNTIL (部位ＣＮＴ > 負－部位数)
                       IF 負－転帰区分(部位ＣＮＴ) = 9 OR 5
                           MOVE SPACE TO 対象フラグ
                       END-IF
                   END-PERFORM
               END-IF
           END-READ.
027377*================================================================*
027378 月末日取得 SECTION.
027379*
027382     MOVE 施術和暦ＷＲ TO 元－元号区分.
027383     READ 元号マスタ
027384     NOT INVALID KEY
027385         MOVE 元－開始西暦年 TO 施術西暦年Ｗ
027386     END-READ.
027387     IF 施術西暦年Ｗ NOT = ZERO
027388        COMPUTE 施術西暦年Ｗ = 施術西暦年Ｗ + 施術年ＷＲ - 1
027389     END-IF.
027390*
027391     EVALUATE 施術月ＷＲ
027392     WHEN 4
027393     WHEN 6
027394     WHEN 9
027395     WHEN 11
027396         MOVE 30 TO 終了日ＷＰ
027397     WHEN 2
027398         DIVIDE 4 INTO 施術西暦年Ｗ GIVING    商Ｗ
027399                                    REMAINDER 余Ｗ
027400         END-DIVIDE
027401         IF 余Ｗ = ZERO
027402             MOVE 29 TO 終了日ＷＰ
027403         ELSE
027404             MOVE 28 TO 終了日ＷＰ
027405         END-IF
027406     WHEN 1
027407     WHEN 3
027408     WHEN 5
027409     WHEN 7
027410     WHEN 8
027411     WHEN 10
027412     WHEN 12
027413         MOVE 31 TO 終了日ＷＰ
027414     WHEN OTHER
027415          CONTINUE
027416     END-EVALUATE.
027417*
012080*================================================================*
012090 施術記録Ｆ読込 SECTION.
012100*
012110     READ 施術記録Ｆ NEXT
012120     AT END
012130         MOVE "YES" TO 終了フラグ２
012140     END-READ.
012150*================================================================*
012160 受診者情報Ｆ読込 SECTION.
012170*
012180     READ 受診者情報Ｆ NEXT
012190     AT END
012200         MOVE "YES" TO 終了フラグ
012210     END-READ.
012220*================================================================*
012230 作１レコード書込 SECTION.
012240*
012250     WRITE 作１－レコード
012260     INVALID KEY
012270         MOVE NC"作業"  TO ファイル名
012280         PERFORM エラー表示
012290     END-WRITE.
004140*================================================================*
004150 作２レコード書込 SECTION.
004160*
004170     WRITE 作２－レコード
004180     INVALID KEY
004190         MOVE NC"作２"  TO ファイル名
004200         PERFORM エラー表示
004210     END-WRITE.
012150*================================================================*
012160 作業ファイル２読込 SECTION.
012170*
012180     READ 作業ファイル２ NEXT
012190     AT END
012200         MOVE "YES" TO 終了フラグ２
012210     END-READ.
012300*================================================================*
012310 ファイル閉鎖 SECTION.
012320*
012330     CLOSE 元号マスタ 施術記録Ｆ 受診者情報Ｆ 負傷データＦ
                 レセプトＦ 会計データＦ 計算マスタ    メモファイル
      */0410
                 作業ファイル１ 作業ファイル２ 作業ファイル３.
012340*================================================================*
012350 終了処理 SECTION.
012360*
012370     PERFORM ファイル閉鎖.
007980*================================================================*
       計算情報取得 SECTION.
      *
      * 制御区分：０１　健康保険
           MOVE 01             TO 計－制御区分.
           MOVE 施術和暦ＷＲ   TO 計－開始和暦 施術和暦ＣＷ.
           MOVE 施術年ＷＲ     TO 計－開始年   施術年ＣＷ.
           MOVE 施術月ＷＲ     TO 計－開始月   施術月ＣＷ.
      *
           START 計算マスタ KEY IS <= 計－制御区分 計－開始和暦年月
                                                           REVERSED
           END-START.
      *
           IF 状態キー = "00"
               READ 計算マスタ NEXT
               AT END
      */エラー表示の修正
                   PERFORM コンソールエラー確認
                   DISPLAY "施術年月に対応した料金がみつかりません"
                           " 受診者№=" 患者コードＷ
                           " 施術年月=" 施術年ＷＲ 施術月ＷＲ   UPON CONS
      *             DISPLAY NC"施術年月に対応した料金がみつかりません" UPON MSGBOX
                   PERFORM 終了処理
                   MOVE ZERO TO PROGRAM-STATUS
                   EXIT PROGRAM
               NOT AT END
                   IF ( 施術和暦年月ＣＷ >= 計Ａ－開始和暦年月 ) AND
                      ( 施術和暦年月ＣＷ <= 計Ａ－終了和暦年月 )
                       PERFORM VARYING 多部位ＣＮＴ FROM 1 BY 1
                               UNTIL 多部位ＣＮＴ > 計Ａ－後療可能算定数
                           MOVE 計Ａ－多部位逓減率(多部位ＣＮＴ) TO 多部位逓減率ＫＷ(多部位ＣＮＴ)
                       END-PERFORM
                   ELSE
      */エラー表示の修正
                       PERFORM コンソールエラー確認
                       DISPLAY "施術年月に対応した料金がみつかりません"
                               " 受診者№=" 患者コードＷＲ
                               " 施術年月=" 施術年ＷＲ 施術月ＷＲ   UPON CONS
      *                 DISPLAY NC"施術年月に対応した料金がみつかりません" UPON MSGBOX
                       PERFORM 終了処理
                       MOVE ZERO TO PROGRAM-STATUS
                       EXIT PROGRAM
                   END-IF
               END-READ
           END-IF.
      *
      *================================================================*
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
013080******************************************************************
013090 END PROGRAM YCH7101.
013100******************************************************************
