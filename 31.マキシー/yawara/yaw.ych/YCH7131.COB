000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCH7131.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*         カルテ裏(複数月)【ﾃﾞｰﾀ作成】［受診者・年月別］柔ｳｨﾝﾄﾞｳｽﾞ版
000100*         MED = YCH7130 YCH7132P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2018-02-02
000130 DATE-COMPILED.          2018-02-02
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
000400     SELECT  メモファイル    ASSIGN      TO        MEMOL
000410                             ORGANIZATION             IS  INDEXED
000420                             ACCESS MODE              IS  DYNAMIC
000430                             RECORD KEY               IS  メモ－制御区分
                                                                メモ－患者コード
                                                                メモ－施術和暦年月日
000360                             ALTERNATE RECORD KEY     IS  メモ－制御区分
                                                                メモ－施術和暦年月日
                                                                メモ－患者コード
000360                             ALTERNATE RECORD KEY     IS  メモ－患者コード
                                                                メモ－施術和暦年月日
                                                                メモ－制御区分
000440                             FILE STATUS              IS  状態キー
000450                             LOCK        MODE         IS  AUTOMATIC.
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
000530     SELECT  作業ファイル１  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W7131L.DAT"
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS                   IS  DYNAMIC
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
000620                             FILE        STATUS       IS  状態キー
000630                             LOCK        MODE         IS  AUTOMATIC.
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
000600*                           ［ＲＬ＝  ８３２］
000610 FD  メモファイル        BLOCK CONTAINS 1     RECORDS.
000620     COPY MEMO           OF    XFDLIB JOINING メモ AS PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
001060*                           ［ＲＬ＝  ５１２］
001070 FD  会計データＦ        BLOCK   CONTAINS   1   RECORDS.
001080     COPY KAIKEI     OF  XFDLIB  JOINING   会   AS  PREFIX.
001340*                         ［ＲＬ＝  ３８４］
001350 FD  作業ファイル１ RECORD  CONTAINS 384 CHARACTERS.
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
001150       05  作１－日チェック               PIC 9.
001160       05  作１－部位データ               OCCURS 5.
001170           07  作１－部位計               PIC 9(5).
001180       05  作１－電罨料.
001190           07  作１－罨法金額             PIC 9(4).
001200           07  作１－電療金額             PIC 9(4).
001210       05  作１－一部負担金               PIC 9(5).
001210       05  作１－金属料                   PIC 9(4).
001210       05  作１－運動料                   PIC 9(3).
001551       05  作１－コメント                 PIC X(100).
             05 作１－初検                      PIC 9.
003280       05 作１－初診料.
003290          07 作１－初検料                 PIC 9(4).
003300          07 作１－初検加算料             PIC 9(4).
003310          07 作１－休日                   PIC 9.
003320          07 作１－深夜                   PIC 9.
003330          07 作１－時間外                 PIC 9.
             05 作１－相談料                    PIC 9(4).
003340       05 作１－再検料                    PIC 9(4).
003350       05 作１－初回処置                  OCCURS 5.
003360          07 作１－初回処置料             PIC 9(5).
003370       05 作１－施療料チェック            PIC N.
003380       05 作１－整復料チェック            PIC N.
003390       05 作１－固定料チェック            PIC N.
003400       05 作１－往療加算.
003410          07 作１－夜間                   PIC 9.
003420          07 作１－暴風                   PIC 9.
003430          07 作１－往療加算料             PIC 9(5).
003440          07 作１－距離                   PIC 9(3)V9.
003450       05 作１－往療料                    OCCURS 3.
003460          07 作１－月                     PIC 9(2).
003470          07 作１－回数                   PIC 9(2).
003480          07 作１－往療料金額             PIC 9(6).
003490       05 作１－金属大                    PIC 9.
003500       05 作１－金属中                    PIC 9.
003510       05 作１－金属小                    PIC 9.
003520       05 作１－金額                      PIC 9(6).
003530       05 作１－情報提供料                PIC 9(6).
001220       05  FILLER                         PIC X(43).
001230*
001340*                         ［ＲＬ＝  ６４］
001350 FD  作業ファイル２ RECORD  CONTAINS 64 CHARACTERS.
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
001551          07 作２－実日数                  PIC 9(2).
001551          07 作２－費用額                  PIC 9(7).
001551          07 作２－負担額                  PIC 9(6).
001551          07 作２－請求額                  PIC 9(7).
001500       05 FILLER                           PIC X(30).
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
001240*----------------------------------------------------------------*
001250******************************************************************
001260*                WORKING-STORAGE SECTION                         *
001270******************************************************************
001280 WORKING-STORAGE         SECTION.
001290 01 キー入力                         PIC X     VALUE SPACE.
001300 01 状態キー                         PIC X(2)  VALUE SPACE.
001310 01 実行キーＷ                       PIC X(3)  VALUE SPACE.
001320 01 終了フラグ                       PIC X(3)  VALUE SPACE.
001330 01 終了フラグ２                     PIC X(3) VALUE SPACE.
001330 01 終了フラグ３                     PIC X(3) VALUE SPACE.
       01 施術記録有Ｗ                     PIC X(3) VALUE SPACE.
001340 01 対象フラグ                       PIC X(3) VALUE SPACE.
001410 01 初検和暦年月ＷＲ.
001420    03 初検和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
001430    03 初検年月ＷＲ.
001440       05 初検年ＷＲ                 PIC 9(2)  VALUE ZERO.
001450       05 初検月ＷＲ                 PIC 9(2)  VALUE ZERO.
001500 01 日ごとモードＷＲ                 PIC 9(1)  VALUE ZERO.
001510 01 氏名モードＷＲ                   PIC 9(1)  VALUE ZERO.
001490 01 コメントモードＷＲ               PIC 9(1)  VALUE ZERO.
001520 01 施術日数Ｗ                       PIC 9(2)  VALUE ZERO.
001530 01 ファイル名                       PIC N(10) VALUE SPACE.
001540 01 部位ＣＮＴ                       PIC 9(2)  VALUE ZERO.
001550 01 カウンタ                         PIC 9(1)  VALUE ZERO.
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
001560*
001570 01 退避項目ＧＷ.
001580   03 レセプト種類Ｗ                 PIC X(4).
001590   03 レセプト種類ＧＷ               PIC X(4).
001600   03 レセプト種別ＧＷ               PIC 9(2).
001610*
001620*
001630 01 明細.
001660    03 初検ＷＲ                      PIC 9     VALUE ZERO.
001640    03 初検料ＷＲ                    PIC 9(4)  VALUE ZERO.
          03 相談料ＷＲ                    PIC 9(4)  VALUE ZERO.
001650    03 初検加算料ＷＲ                PIC 9(4)  VALUE ZERO.
001660    03 休日ＷＲ                      PIC 9     VALUE ZERO.
001670    03 深夜ＷＲ                      PIC 9     VALUE ZERO.
001680    03 時間外ＷＲ                    PIC 9     VALUE ZERO.
001690    03 再検料ＷＲ                    PIC 9(4)  VALUE ZERO.
001700    03 往療夜間ＷＲ                  PIC 9     VALUE ZERO.
001710    03 往療暴風ＷＲ                  PIC 9     VALUE ZERO.
001720    03 往療加算料ＷＲ                PIC 9(5)  VALUE ZERO.
001730    03 往療距離ＷＲ                  PIC 9(3)V9 VALUE ZERO.
001740    03 往療料Ｗ                      OCCURS 3.
001750       05 往療月ＷＲ                 PIC 9(2)  VALUE ZERO.
001760       05 往療回数ＷＲ               PIC 9(2)  VALUE ZERO.
001770       05 往療料ＷＲ                 PIC 9(6)  VALUE ZERO.
001780    03 罨法料ＷＲ                    PIC 9(4)  VALUE ZERO.
001790    03 電療料金ＷＲ                  PIC 9(4)  VALUE ZERO.
001800    03 一部負担金ＷＲ                PIC 9(5)  VALUE ZERO.
001960    03 金属料ＷＲ                    PIC 9(4)  VALUE ZERO.
001960    03 運動料ＷＲ                    PIC 9(3)  VALUE ZERO.
001810    03 施術情報提供料ＷＲ            PIC 9(6)  VALUE ZERO.
001820    03 初回処置料チェックＷ.
001830       05 整復料チェックＷ                PIC N(1) VALUE SPACE.
001840       05 固定料チェックＷ                PIC N(1) VALUE SPACE.
001850       05 施療料チェックＷ                PIC N(1) VALUE SPACE.
001860    03 部位Ｗ                        OCCURS 5.
001870       05 初回処置料ＷＲ             PIC 9(5)  VALUE ZERO.
001880       05 後療料ＷＲ                 PIC 9(4)  VALUE ZERO.
001890       05 冷罨料ＷＲ                 PIC 9(4)  VALUE ZERO.
001900       05 温罨料ＷＲ                 PIC 9(4)  VALUE ZERO.
001910       05 電療料ＷＲ                 PIC 9(4)  VALUE ZERO.
001920       05 部位計ＷＲ                 PIC 9(5)  VALUE ZERO.
001930    03 金属大ＷＲ                    PIC 9     VALUE ZERO.
001940    03 金属中ＷＲ                    PIC 9     VALUE ZERO.
001950    03 金属小ＷＲ                    PIC 9     VALUE ZERO.
001960    03 金属加算ＷＲ                  PIC 9(5)  VALUE ZERO.
001970    03 日数ＷＲ                      PIC 9(2)  OCCURS 6 VALUE ZERO.
001980    03 冷罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
001990    03 冷罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
002000    03 冷罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
002010    03 冷罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
002020    03 冷罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
002030    03 冷罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
002040    03 冷罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
002050    03 冷罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
002060    03 冷罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
002070    03 温罨法料３０ＷＲ              PIC 9(4)  VALUE ZERO.
002080    03 温罨法料３８ＷＲ              PIC 9(4)  VALUE ZERO.
002090    03 温罨法料４０ＷＲ              PIC 9(4)  VALUE ZERO.
002100    03 温罨法料４５ＷＲ              PIC 9(4)  VALUE ZERO.
002110    03 温罨法料４８ＷＲ              PIC 9(4)  VALUE ZERO.
002120    03 温罨法料５２ＷＲ              PIC 9(4)  VALUE ZERO.
002130    03 温罨法料５５ＷＲ              PIC 9(4)  VALUE ZERO.
002140    03 温罨法料５８ＷＲ              PIC 9(4)  VALUE ZERO.
002150    03 温罨法料５０ＷＲ              PIC 9(4)  VALUE ZERO.
002160    03 電療料３０ＷＲ                PIC 9(4)  VALUE ZERO.
002170    03 電療料３８ＷＲ                PIC 9(4)  VALUE ZERO.
002180    03 電療料４０ＷＲ                PIC 9(4)  VALUE ZERO.
002190    03 電療料４５ＷＲ                PIC 9(4)  VALUE ZERO.
002200    03 電療料４８ＷＲ                PIC 9(4)  VALUE ZERO.
002210    03 電療料５２ＷＲ                PIC 9(4)  VALUE ZERO.
002220    03 電療料５５ＷＲ                PIC 9(4)  VALUE ZERO.
002230    03 電療料５８ＷＲ                PIC 9(4)  VALUE ZERO.
002240    03 電療料５０ＷＲ                PIC 9(4)  VALUE ZERO.
002250    03 後療料３０ＷＲ                PIC 9(4)  VALUE ZERO.
002260    03 後療料３８ＷＲ                PIC 9(4)  VALUE ZERO.
002270    03 後療料４０ＷＲ                PIC 9(4)  VALUE ZERO.
002280    03 後療料４５ＷＲ                PIC 9(4)  VALUE ZERO.
002290    03 後療料４８ＷＲ                PIC 9(4)  VALUE ZERO.
002300    03 後療料５２ＷＲ                PIC 9(5)  VALUE ZERO.
002310    03 後療料５５ＷＲ                PIC 9(5)  VALUE ZERO.
002320    03 後療料５８ＷＲ                PIC 9(5)  VALUE ZERO.
002330    03 後療料５０ＷＲ                PIC 9(5)  VALUE ZERO.
002340    03 冷罨料計ＷＲ                  PIC 9(4)  VALUE ZERO.
002350    03 温罨料計ＷＲ                  PIC 9(4)  VALUE ZERO.
002360    03 電療料計ＷＲ                  PIC 9(4)  VALUE ZERO.
002360    03 実日数ＷＲ                    PIC 9(2)  VALUE ZERO.
002360    03 費用額ＷＲ                    PIC 9(6)  VALUE ZERO.
002360    03 負担額ＷＲ                    PIC 9(6)  VALUE ZERO.
002360    03 請求額ＷＲ                    PIC 9(6)  VALUE ZERO.
002370*
002390 01 公費種別Ｗ                       PIC 9(2)  VALUE ZERO.
002400 01 助成種別Ｗ                       PIC 9(2)  VALUE ZERO.
002410 01 負担額Ｗ２                       PIC 9(5)  VALUE ZERO.
002420 01 費用額Ｗ２                       PIC 9(5)  VALUE ZERO.
002430 01 施術累計回数Ｗ                   PIC 9(3)  VALUE ZERO.
002440*
002450 01 負傷和暦年月日Ｗ.
002460     03 負傷和暦年月Ｗ.
002470         05 負傷和暦Ｗ               PIC 9.
002480         05 負傷年月Ｗ.
002490            07 負傷年Ｗ              PIC 9(2).
002500            07 負傷月Ｗ              PIC 9(2).
002510     03 負傷日Ｗ                     PIC 9(2).
002520*
002530 01 開始和暦年月日Ｗ.
002540     03 開始和暦年月Ｗ.
002550         05 開始和暦Ｗ               PIC 9.
002560         05 開始年月Ｗ.
002570            07 開始年Ｗ              PIC 9(2).
002580            07 開始月Ｗ              PIC 9(2).
002590     03 開始日Ｗ                     PIC 9(2).
002600*
002610 01 終了和暦年月日Ｗ.
002620     03 終了和暦年月Ｗ.
002630         05 終了和暦Ｗ               PIC 9.
002640         05 終了年月Ｗ.
002650            07 終了年Ｗ              PIC 9(2).
002660            07 終了月Ｗ              PIC 9(2).
002670     03 終了日Ｗ                     PIC 9(2).
002680*
002170 01 患者コードＷ.
002180    03 患者番号Ｗ                    PIC 9(6)  VALUE ZERO.
002190    03 枝番Ｗ                        PIC X     VALUE SPACE.
003180 01 保険種別Ｗ                       PIC 9(2)  VALUE ZERO.
001150 01 本人家族区分Ｗ                   PIC 9(1) VALUE ZERO.
002690*
002700 01 計算和暦年月Ｗ.
002710     03 計算和暦Ｗ                     PIC 9.
002720     03 計算年月Ｗ.
002730        05 計算年Ｗ                    PIC 9(2).
002740        05 計算月Ｗ                    PIC 9(2).
002750     03 計算日Ｗ                       PIC 9(2).
002760*
002770 01 実日数Ｗ                           PIC 9(2) VALUE ZERO.
002780 01 部位数Ｗ                           PIC 9 VALUE ZERO.
002790 01 最大登録数Ｗ                       PIC 9 VALUE ZERO.
002800 01 部位長Ｗ                           PIC 9(2) VALUE 1.
002618 01 施術西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
002810 01 商Ｗ                               PIC 9(4) VALUE ZERO.
002820 01 余Ｗ                               PIC 9(4) VALUE ZERO.
002830 01 負傷名Ｗ                           PIC N(6) VALUE SPACE.
002840*
002850 01 絶対値Ｗ                           PIC 9(6).
002860 01 絶対値月Ｗ                         PIC 9(3).
002870 01 絶対値月Ｗ２                       PIC 9(3).
002880 01 負傷経過日Ｗ                       PIC 9(3).
002890 01 西暦年Ｗ                           PIC 9(4).
002900 01 西暦年差Ｗ                         PIC 9(2).
002910 01 西暦年差２Ｗ                       PIC 9(2).
002920 01 閏年回数Ｗ                         PIC 9(2).
002930 01 通常年回数Ｗ                       PIC 9(2).
002940 01 通常年絶対値Ｗ                     PIC 9(6).
002950 01 閏年絶対値Ｗ                       PIC 9(6).
002960 01 年絶対値Ｗ                         PIC 9(6).
002970 01 負傷絶対値Ｗ                       PIC 9(6).
002980 01 施術絶対値Ｗ                       PIC 9(6).
       01 加算年Ｗ                           PIC 9(4).
002990*
003000******************************************************************
003010*                          連結項目                              *
003020******************************************************************
003030*
003040****************
003050* 画面入力情報 *
003060****************
       01 連入－入力データＹＣＨ７１３０ IS EXTERNAL GLOBAL.
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
002810 01 連入－入力データＹＣＨ７１３４ IS EXTERNAL.
002820    03 連入－対象フラグ                   PIC 9(1).
006850*
006860 01 連メ－キー IS EXTERNAL.
006870    03  連メ－メッセージ                 PIC N(20).
006940*
006950******************************************************************
006960*                      PROCEDURE  DIVISION                       *
006970******************************************************************
006980 PROCEDURE               DIVISION.
006990************
007000*           *
007010* 初期処理   *
007020*           *
007030************
007040     PERFORM ファイルオープン.
007050     PERFORM 連結項目待避.
           MOVE ZERO TO 連入－対象フラグ.
007060************
007070*           *
007080* 主処理     *
007090*           *
007100************
           PERFORM 作業ファイル作成.
007170************
007180*           *
007190* 終了処理   *
007200*           *
007210************
007220     PERFORM 終了処理.
      */対象がない場合は印刷しないように/050610
           IF (連入－対象フラグ NOT = ZERO) 
               MOVE ZERO TO PROGRAM-STATUS
           ELSE
               MOVE 99   TO PROGRAM-STATUS
           END-IF.
007240     EXIT PROGRAM.
007250*
007260*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
007270*================================================================*
007280 連結項目待避 SECTION.
007290*
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
007380*================================================================*
007390 ファイルオープン SECTION.
007400*
012140     OPEN INPUT   元号マスタ
012150         MOVE NC"元号" TO ファイル名.
012160         PERFORM オープンチェック.
007410     OPEN INPUT 施術記録Ｆ.
007420         MOVE NC"施記" TO ファイル名.
007430         PERFORM オープンチェック.
007440     OPEN INPUT 受診者情報Ｆ.
007450         MOVE NC"受診" TO ファイル名.
007460         PERFORM オープンチェック.
007470     OPEN INPUT 負傷データＦ.
007480         MOVE NC"負傷" TO ファイル名.
007490         PERFORM オープンチェック.
002780     OPEN INPUT メモファイル.
002790         MOVE NC"メモ"         TO ファイル名.
002800         PERFORM オープンチェック.
007870     OPEN INPUT レセプトＦ.
007880         MOVE NC"レセ"         TO ファイル名.
007890         PERFORM オープンチェック.
003060     OPEN INPUT 会計データＦ.
003070         MOVE NC"会計" TO ファイル名.
003080         PERFORM オープンチェック.
007530*================================================================*
007540 オープンチェック SECTION.
007550*
007560     IF 状態キー  NOT =  "00"
007570         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
007580         DISPLAY NC"状態キー：" 状態キー         UPON CONS
007590         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
003131*-----------------------------------------*
003132         CALL "actcshm"  WITH C LINKAGE
003133*-----------------------------------------*
007610         ACCEPT  キー入力 FROM CONS
007620         PERFORM ファイル閉鎖
007630         MOVE 99 TO PROGRAM-STATUS
007640         EXIT PROGRAM.
007650*================================================================*
007660 エラー表示 SECTION.
007670*
007680     DISPLAY NC"状態キー" 状態キー  UPON CONS.
007690     DISPLAY NC"５ファイル書込エラー：" ファイル名 UPON CONS.
007700     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
007710     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
003131*-----------------------------------------*
003132     CALL "actcshm"  WITH C LINKAGE.
003133*-----------------------------------------*
007720     ACCEPT  キー入力 FROM CONS.
007730     PERFORM ファイル閉鎖.
007740     MOVE 99 TO PROGRAM-STATUS.
007750     EXIT PROGRAM.
008240*================================================================*
008250 作業ファイル作成 SECTION.
008260*
           MOVE ZERO TO 連入－対象フラグ.
      *
008270     OPEN OUTPUT 作業ファイル１.
002220          MOVE NC"作１" TO ファイル名
002230          PERFORM オープンチェック
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
008300*
008310     MOVE 開始和暦ＷＲ  TO 受－施術和暦.
008320     MOVE 開始年ＷＲ    TO 受－施術年.
008330     MOVE 開始月ＷＲ    TO 受－施術月.
           MOVE SPACE             TO 受－患者カナ.
           MOVE SPACE             TO 受－患者コード.
003790     START 受診者情報Ｆ   KEY IS >= 受－施術和暦年月
                                          受－患者カナ
                                          受－患者コード
008380     END-START.
008390     IF 状態キー = "00"
003840         MOVE SPACE  TO 終了フラグ
008400         PERFORM 受診者情報Ｆ読込
003860         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
003870                       ( 受－施術和暦年月 > 終了和暦年月ＷＲ )
      */印刷対象条件の判定
001110             IF ((保険種別ＷＲ     NOT = ZERO)              AND
                       (保険種別ＷＲ     NOT = 受－保険種別))     OR
HILO                  ((患者番号ＷＲ     NOT = ZERO)              AND
HILO                   (患者コードＷＲ   NOT = 受－患者コード))   OR
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
008410                     PERFORM データチェック
008420                     IF 実行キーＷ = "YES"
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
007930                         PERFORM 負傷データ取得
003910                         PERFORM 作１レコードセット
007920                         PERFORM レセプト呼出し１
                               PERFORM 料金情報取得
003910                         PERFORM 作２レコードセット
                               PERFORM 作２レコード書込
                           END-IF
                       END-IF
008480             END-IF
003930             PERFORM 受診者情報Ｆ読込
003910             PERFORM 作３レコードセット
               END-PERFORM
           ELSE
               MOVE ZERO TO 連入－対象フラグ
008510     END-IF.
008520*
008530     CLOSE 作業ファイル１ 作業ファイル２ 作業ファイル３.
004030*================================================================*
004040 作１レコードセット SECTION.
004050*
           PERFORM VARYING 施術日ＷＲ FROM 開始日Ｗ BY 1
                   UNTIL ( 施術日ＷＲ > 終了日Ｗ )
008270         INITIALIZE 作１－レコード
004130         MOVE 受－患者コード   TO 作１－患者コード
               MOVE 受－患者カナ     TO 作１－患者カナ
009180         MOVE 受－患者番号  TO 会－患者番号
009190         MOVE 受－枝番      TO 会－枝番
009200         MOVE 受－施術和暦  TO 会－施術和暦
009210         MOVE 受－施術年    TO 会－施術年
009220         MOVE 受－施術月    TO 会－施術月
009230         MOVE 施術日ＷＲ    TO 会－施術日
009240         READ 会計データＦ
               NOT INVALID KEY
                   PERFORM 項目ごと計算
                   PERFORM 作業ファイル１セット
                   PERFORM 作１レコード書込
               END-READ
           END-PERFORM.
008540*================================================================*
008550 作業ファイル１セット SECTION.
008560*
008580     MOVE 施術和暦ＷＲ       TO 作１－施術和暦 メモ－施術和暦.
008590     MOVE 施術年ＷＲ         TO 作１－施術年 メモ－施術年.
008600     MOVE 施術月ＷＲ         TO 作１－施術月 メモ－施術月.
008610     MOVE 施術日ＷＲ         TO 作１－施術日 メモ－施術日.
008620     MOVE 受－患者コード     TO 作１－患者コード メモ－患者コード.
008640     MOVE 初検ＷＲ           TO 作１－初検.
008630     MOVE 一部負担金ＷＲ     TO 作１－一部負担金.
008640     MOVE 金属料ＷＲ         TO 作１－金属料.
008640     MOVE 運動料ＷＲ         TO 作１－運動料.
008640     MOVE 罨法料ＷＲ         TO 作１－罨法金額.
008650     MOVE 電療料金ＷＲ       TO 作１－電療金額.
008660     IF 罨法料ＷＲ NOT = ZERO
008670         COMPUTE 日数ＷＲ(6) = 日数ＷＲ(6) + 1
008680     END-IF.
008690     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 5
008700         MOVE 部位計ＷＲ(カウンタ)     TO 作１－部位計(カウンタ)
008710         IF 部位計ＷＲ(カウンタ) NOT = ZERO
008720             MOVE 1      TO 作１－日チェック
008730             COMPUTE 日数ＷＲ(カウンタ) = 日数ＷＲ(カウンタ) + 1
008740         END-IF
008750     END-PERFORM.
           MOVE 1                  TO メモ－制御区分.
           READ メモファイル
           NOT INVALID KEY
               MOVE メモ－施術コメント TO 作１－コメント
           END-READ.
007950     MOVE 初検料ＷＲ             TO  作１－初検料
           MOVE 相談料ＷＲ             TO  作１－相談料
007960     MOVE 再検料ＷＲ             TO  作１－再検料
007970     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
007980               UNTIL ( 部位ＣＮＴ > 部位数Ｗ ) OR (部位ＣＮＴ > 5)
007990        MOVE 初回処置料ＷＲ(部位ＣＮＴ) TO 作１－初回処置料(部位ＣＮＴ)
008000     END-PERFORM.
008010     MOVE 施療料チェックＷ       TO  作１－施療料チェック.
008020     MOVE 整復料チェックＷ       TO  作１－整復料チェック.
008030     MOVE 固定料チェックＷ       TO  作１－固定料チェック.
008040*
008050     MOVE 初検加算料ＷＲ         TO  作１－初検加算料.
008060     MOVE 休日ＷＲ               TO  作１－休日.
008070     MOVE 深夜ＷＲ               TO  作１－深夜.
008080     MOVE 時間外ＷＲ             TO  作１－時間外.
008090     MOVE 往療月ＷＲ(1)          TO  作１－月(1).
008100     MOVE 往療回数ＷＲ(1)        TO  作１－回数(1).
008110     MOVE 往療距離ＷＲ           TO  作１－距離.
008120     MOVE 往療料ＷＲ(1)          TO  作１－往療料金額(1).
008130     MOVE 往療加算料ＷＲ         TO  作１－往療加算料.
008140     MOVE 往療夜間ＷＲ           TO  作１－夜間.
008150     MOVE 往療暴風ＷＲ           TO  作１－暴風.
008160*
008170     MOVE 金属大ＷＲ             TO  作１－金属大.
008180     MOVE 金属中ＷＲ             TO  作１－金属中.
008190     MOVE 金属小ＷＲ             TO  作１－金属小.
008200     MOVE 金属加算ＷＲ           TO  作１－金額.
008210     MOVE 施術情報提供料ＷＲ     TO  作１－情報提供料.
007760*================================================================*
007770 作２レコードセット SECTION.
007780*
008270     INITIALIZE 作２－レコード.
008580     MOVE 施術和暦ＷＲ     TO 作２－施術和暦.
008590     MOVE 施術年ＷＲ       TO 作２－施術年.
008600     MOVE 施術月ＷＲ       TO 作２－施術月.
004130     MOVE 受－患者コード   TO 作２－患者コード.
008200     MOVE 実日数ＷＲ       TO 作２－実日数.
008200     MOVE 費用額ＷＲ       TO 作２－費用額.
008200     MOVE 負担額ＷＲ       TO 作２－負担額.
008200     MOVE 請求額ＷＲ       TO 作２－請求額.
008220*
004030*================================================================*
004040 作３レコードセット SECTION.
004050*
008270     INITIALIZE 作３－レコード.
           IF (受－患者番号 = 患者番号Ｗ) AND
              (受－保険種別 NOT = 70 AND 80 AND 85 AND 90)
004130        MOVE 受－患者番号       TO 作３－患者番号
              MOVE 受－施術和暦年月   TO 作３－施術和暦年月
009240        READ 作業ファイル３
              INVALID KEY
                  IF ((受－枝番 >= "A") AND (枝番Ｗ <= "Z"))
                     MOVE 1             TO 作３－枝番Ｆ
                  END-IF
001110            IF (保険種別Ｗ     NOT = 受－保険種別)     OR
001150               (本人家族区分Ｗ NOT = 受－本人家族区分)
                     MOVE 1             TO 作３－保険変更Ｆ
                  END-IF
                  WRITE 作３－レコード
                  END-WRITE
              NOT INVALID KEY
                  IF ((受－枝番 >= "A") AND (枝番Ｗ <= "Z"))
                     MOVE 1             TO 作３－枝番Ｆ
                  END-IF
001110            IF (保険種別Ｗ     NOT = 受－保険種別)     OR
001150               (本人家族区分Ｗ NOT = 受－本人家族区分)
                     MOVE 1             TO 作３－保険変更Ｆ
                  END-IF
                  REWRITE 作３－レコード
                  END-REWRITE
              END-READ
           END-IF.
002180*
004140*================================================================*
004150 作１レコード書込 SECTION.
004160*
004170     WRITE 作１－レコード
004180     INVALID KEY
004190         MOVE NC"作１"  TO ファイル名
004200         PERFORM エラー表示
004210     END-WRITE.
004140*================================================================*
004150 作２レコード書込 SECTION.
004160*
004170     WRITE 作２－レコード
004180     INVALID KEY
004190         MOVE NC"作２"  TO ファイル名
004200         PERFORM エラー表示
004210     END-WRITE.
008760*================================================================*
008770 データチェック SECTION.
008780*
008790     MOVE SPACE  TO 実行キーＷ.
008800* *****************************************************************
008810* * 負傷部位有無チェック：部位数 = 0 の場合データ作成対象としない *
008820* *****************************************************************
008830     MOVE 受－施術和暦   TO 負－施術和暦.
008840     MOVE 受－施術年     TO 負－施術年.
008850     MOVE 受－施術月     TO 負－施術月.
008860     MOVE 受－患者番号   TO 負－患者番号.
008870     MOVE 受－枝番       TO 負－枝番.
008880     READ 負傷データＦ
008890     INVALID KEY
008900         MOVE SPACE  TO 実行キーＷ
008910     NOT INVALID KEY
008920         IF 負－部位数 NOT = ZERO
008930*        *************************************************************
008940*        * 施術記録チェック：通院数 = 0 の場合データ作成対象としない *
008950*        *************************************************************
008960             MOVE 負－患者番号  TO 施記－患者番号
008970             MOVE 負－枝番      TO 施記－枝番
008980             MOVE 負－施術和暦  TO 施記－施術和暦
008990             MOVE 負－施術年    TO 施記－施術年
009000             MOVE 負－施術月    TO 施記－施術月
009010             MOVE ZERO          TO 施記－施術日
009020             START 施術記録Ｆ   KEY IS >= 施記－患者コード
009030                                          施記－施術和暦年月日
009040             END-START
009050             IF 状態キー = "00"
009060                 MOVE SPACE TO 終了フラグ２
009070                 MOVE SPACE TO 施術記録有Ｗ
009080                 PERFORM 施術記録Ｆ読込
009090                 PERFORM UNTIL ( 終了フラグ２         = "YES"          ) OR
009100                               ( 施記－患者コード NOT = 負－患者コード ) OR
009110                               ( 施記－施術和暦   NOT = 負－施術和暦   ) OR
009120                               ( 施記－施術年     NOT = 負－施術年     ) OR
009130                               ( 施記－施術月     NOT = 負－施術月     ) OR
009140                               ( 施術記録有Ｗ         = "YES"          )
009150                     MOVE "YES"  TO 施術記録有Ｗ
009160                     MOVE "YES"  TO 実行キーＷ
009170                 END-PERFORM
009180             ELSE
009190                 MOVE SPACE  TO 実行キーＷ
009200             END-IF
009210         ELSE
009250             MOVE SPACE  TO 実行キーＷ
009260         END-IF
009270     END-READ.
009280*
009290*================================================================*
009300 レセプト呼出し１ SECTION.
009310*
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
009620*
010890*================================================================*
010900 負傷データ取得 SECTION.
010910*
010920**************************************************
010930* 連結データから負傷データＦより以下の情報を取得 *
010940* ● 金属副子 ....区分によりチェックに"○"を格納 *
010950**************************************************
010960     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
010970     MOVE 施術年ＷＲ         TO 負－施術年.
010980     MOVE 施術月ＷＲ         TO 負－施術月.
010990     MOVE 受－患者コード     TO 負－患者コード.
011000     READ 負傷データＦ
011010     INVALID KEY
011020         CONTINUE
011030*            /* ありえない */
011040     NOT INVALID KEY
011050         MOVE 負－部位数                   TO 部位数Ｗ
011060         MOVE 負－初検和暦年月             TO 初検和暦年月ＷＲ
011070     END-READ.
011080*
011090*================================================================*
011100 料金情報取得 SECTION.
011110*
011120***********************************************
011130* 料金データセット                            *
011140***********************************************
      *
           MOVE レセ－レセ実日数             TO 実日数ＷＲ.
           MOVE レセ－合計                   TO 費用額ＷＲ.
           MOVE レセ－一部負担金             TO 負担額ＷＲ.
           MOVE レセ－請求金額               TO 請求額ＷＲ.
011830*
011840*================================================================*
011850 項目ごと計算 SECTION.
011860***********************************************
011870* 料金データセット                            *
011880***********************************************
011180     MOVE 会－初検料                 TO  初検料ＷＲ.
           MOVE 会－初検時相談料           TO  相談料ＷＲ.
011190*
011200     MOVE 会－初検加算料             TO  初検加算料ＷＲ.
011210     MOVE 会－休日                   TO  休日ＷＲ.
011220     MOVE 会－深夜                   TO  深夜ＷＲ.
011230     MOVE 会－時間外                 TO  時間外ＷＲ.
011240     MOVE 会－再検料                 TO  再検料ＷＲ.
011250     IF 開始和暦年月ＷＲ = 初検和暦年月ＷＲ
011260         MOVE 連入－開始月             TO  往療月ＷＲ(1)
011270         MOVE 会－往療回数           TO  往療回数ＷＲ(1)
011280         MOVE 会－往療距離           TO  往療距離ＷＲ
011290         MOVE 会－往療料             TO  往療料ＷＲ(1)
011300         MOVE 会－往療加算料         TO  往療加算料ＷＲ
011310         MOVE 会－夜間               TO  往療夜間ＷＲ
011320         MOVE 会－暴風雨雪           TO  往療暴風ＷＲ
011330     END-IF.
011340*
011350     MOVE 会－金属副子加算料         TO  金属加算ＷＲ.
011360     MOVE 会－大                     TO  金属大ＷＲ.
011370     MOVE 会－中                     TO  金属中ＷＲ.
011380     MOVE 会－小                     TO  金属小ＷＲ.
011390*
011400     MOVE 会－施術情報提供料         TO  施術情報提供料ＷＲ.
011410********************
011420* 初回処置料セット *
011430********************
011440     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
011450             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
011460         MOVE 会－初回処置料(部位ＣＮＴ) TO 初回処置料ＷＲ(部位ＣＮＴ)
011470         IF 会－初回処置料(部位ＣＮＴ) NOT = ZERO
011480            EVALUATE 負－負傷種別(部位ＣＮＴ)
011490* 捻挫・打撲・挫傷
011500            WHEN 1
011510            WHEN 2
011520            WHEN 3
011530                MOVE NC"○"       TO 施療料チェックＷ
011540* 脱臼・骨折・骨折拘縮
011550            WHEN 4
011560            WHEN 5
011570            WHEN 7
011580                MOVE NC"○"       TO 整復料チェックＷ
011590* 不全骨折・不全骨折拘縮
011600            WHEN 6
011610            WHEN 8
011620                MOVE NC"○"       TO 固定料チェックＷ
011630            END-EVALUATE
011640         END-IF
011650     END-PERFORM.
      *
011920     MOVE 会－後療料１     TO 後療料ＷＲ(1).
011930     MOVE 会－後療料２     TO 後療料ＷＲ(2).
011940     MOVE 会－後療料３８   TO 後療料３８ＷＲ.
011950     MOVE 会－後療料３０   TO 後療料３０ＷＲ.
011960     COMPUTE 後療料ＷＲ(3) = 後療料３８ＷＲ   + 後療料３０ＷＲ.
011970     MOVE 会－後療料４５   TO 後療料４５ＷＲ.
011980     MOVE 会－後療料４８   TO 後療料４８ＷＲ.
011990     MOVE 会－後療料４０   TO 後療料４０ＷＲ.
012000     COMPUTE 後療料ＷＲ(4) = 後療料４５ＷＲ   + 後療料４８ＷＲ   + 後療料４０ＷＲ.
012010     MOVE 会－後療料５２   TO 後療料５２ＷＲ.
012020     MOVE 会－後療料５５   TO 後療料５５ＷＲ.
012030     MOVE 会－後療料５８   TO 後療料５８ＷＲ.
012040     MOVE 会－後療料５０   TO 後療料５０ＷＲ.
012050     COMPUTE 後療料ＷＲ(5) = 後療料５２ＷＲ + 後療料５５ＷＲ + 後療料５８ＷＲ + 後療料５０ＷＲ.
012060     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 5
012070         COMPUTE 部位計ＷＲ(カウンタ) = 後療料ＷＲ(カウンタ) + 初回処置料ＷＲ(カウンタ)
012080     END-PERFORM.
012090********************
012100* 逓減毎料金セット *
012110********************
012120     MOVE 会－冷罨法料１             TO 冷罨料ＷＲ(1).
012130     MOVE 会－冷罨法料２             TO 冷罨料ＷＲ(2).
012140     MOVE 会－冷罨法料３８           TO 冷罨法料３８ＷＲ.
012150     MOVE 会－冷罨法料３０           TO 冷罨法料３０ＷＲ.
012160     COMPUTE 冷罨料ＷＲ(3) = 冷罨法料３８ＷＲ  + 冷罨法料３０ＷＲ.
012170     MOVE 会－冷罨法料４５           TO 冷罨法料４５ＷＲ.
012180     MOVE 会－冷罨法料４８           TO 冷罨法料４８ＷＲ.
012190     MOVE 会－冷罨法料４０           TO 冷罨法料４０ＷＲ.
012200     COMPUTE 冷罨料ＷＲ(4) = 冷罨法料４５ＷＲ  + 冷罨法料４８ＷＲ  + 冷罨法料４０ＷＲ.
012210     MOVE 会－冷罨法料５２           TO 冷罨法料５２ＷＲ.
012220     MOVE 会－冷罨法料５５           TO 冷罨法料５５ＷＲ.
012230     MOVE 会－冷罨法料５８           TO 冷罨法料５８ＷＲ.
012240     MOVE 会－冷罨法料５０           TO 冷罨法料５０ＷＲ.
012250     COMPUTE 冷罨料ＷＲ(5) = 冷罨法料５２ＷＲ + 冷罨法料５５ＷＲ + 冷罨法料５８ＷＲ + 冷罨法料５０ＷＲ.
012260     COMPUTE 冷罨料計ＷＲ = 
012270             冷罨料ＷＲ(1) + 冷罨料ＷＲ(2) + 冷罨料ＷＲ(3) + 冷罨料ＷＲ(4) + 冷罨料ＷＲ(5).
012280*
012290     MOVE 会－温罨法料１             TO 温罨料ＷＲ(1).
012300     MOVE 会－温罨法料２             TO 温罨料ＷＲ(2).
012310     MOVE 会－温罨法料３８           TO 温罨法料３８ＷＲ.
012320     MOVE 会－温罨法料３０           TO 温罨法料３０ＷＲ.
012330     COMPUTE 温罨料ＷＲ(3)   = 温罨法料３８ＷＲ  + 温罨法料３０ＷＲ.
012340     MOVE 会－温罨法料４５           TO 温罨法料４５ＷＲ.
012350     MOVE 会－温罨法料４８           TO 温罨法料４８ＷＲ.
012360     MOVE 会－温罨法料４０           TO 温罨法料４０ＷＲ.
012370     COMPUTE 温罨料ＷＲ(4)   = 温罨法料４５ＷＲ  + 温罨法料４８ＷＲ  + 温罨法料４０ＷＲ.
012380     MOVE 会－温罨法料５２           TO 温罨法料５２ＷＲ.
012390     MOVE 会－温罨法料５５           TO 温罨法料５５ＷＲ.
012400     MOVE 会－温罨法料５８           TO 温罨法料５８ＷＲ.
012410     MOVE 会－温罨法料５０           TO 温罨法料５０ＷＲ.
012420     COMPUTE 温罨料ＷＲ(5) = 温罨法料５２ＷＲ + 温罨法料５５ＷＲ + 温罨法料５８ＷＲ + 温罨法料５０ＷＲ.
012430     COMPUTE 温罨料計ＷＲ =
012440     温罨料ＷＲ(1) + 温罨料ＷＲ(2) + 温罨料ＷＲ(3) + 温罨料ＷＲ(4) + 温罨料ＷＲ(5).
012450*
012460     MOVE 会－電療料１             TO 電療料ＷＲ(1).
012470     MOVE 会－電療料２             TO 電療料ＷＲ(2).
012480     MOVE 会－電療料３８           TO 電療料３８ＷＲ.
012490     MOVE 会－電療料３０           TO 電療料３０ＷＲ.
012500     COMPUTE 電療料ＷＲ(3)  = 電療料３８ＷＲ  + 電療料３０ＷＲ.
012510     MOVE 会－電療料４５           TO 電療料４５ＷＲ.
012520     MOVE 会－電療料４８           TO 電療料４８ＷＲ.
012530     MOVE 会－電療料４０           TO 電療料４０ＷＲ.
012540     COMPUTE 電療料ＷＲ(4)  = 電療料４５ＷＲ  + 電療料４８ＷＲ  + 電療料４０ＷＲ.
012550     MOVE 会－電療料５２           TO 電療料５２ＷＲ.
012560     MOVE 会－電療料５５           TO 電療料５５ＷＲ.
012570     MOVE 会－電療料５８           TO 電療料５８ＷＲ.
012580     MOVE 会－電療料５０           TO 電療料５０ＷＲ.
012590     COMPUTE 電療料ＷＲ(5) = 電療料５２ＷＲ + 電療料５５ＷＲ + 電療料５８ＷＲ + 電療料５０ＷＲ.
012600     COMPUTE 電療料計ＷＲ =
012610             電療料ＷＲ(1) + 電療料ＷＲ(2) + 電療料ＷＲ(3) + 電療料ＷＲ(4) + 電療料ＷＲ(5).
012620*
012630     COMPUTE 罨法料ＷＲ = 冷罨料計ＷＲ + 温罨料計ＷＲ.
012640     MOVE 電療料計ＷＲ   TO 電療料金ＷＲ.
009710*
009750     MOVE 会－一部負担金     TO 一部負担金ＷＲ.
012640     MOVE 会－金属副子加算料 TO 金属料ＷＲ.
012640     MOVE 会－運動後療料     TO 運動料ＷＲ.
      *
           MOVE 会－患者コード     TO 施記－患者コード.
           MOVE 会－施術和暦年月日 TO 施記－施術和暦年月日.
           READ 施術記録Ｆ
           NOT INVALID KEY
               IF 施記－診療区分 = 2
                   MOVE 1          TO 初検ＷＲ
               ELSE
                   MOVE 0          TO 初検ＷＲ
               END-IF
           END-READ.
012650*
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
014120         MOVE SPACE TO 終了フラグ３
014140         PERFORM 施術記録Ｆ読込
014150         PERFORM UNTIL (終了フラグ３         = "YES"         ) OR
014160                       (施記－患者コード NOT = 受－患者コード) OR
014170                       (施記－施術和暦   NOT = 受－施術和暦  ) OR
014180                       (施記－施術年     NOT = 受－施術年    ) OR
014190                       (施記－施術月     NOT = 受－施術月    )
      *
                   IF 施記－診療区分 = 2
                       MOVE "YES" TO 対象フラグ
                       MOVE "YES" TO 終了フラグ３
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
014120         MOVE SPACE TO 終了フラグ３
014140         PERFORM 施術記録Ｆ読込
014150         PERFORM UNTIL (終了フラグ３         = "YES"         ) OR
014160                       (施記－患者コード NOT = 受－患者コード) OR
014170                       (施記－施術和暦   NOT = 受－施術和暦  ) OR
014180                       (施記－施術年     NOT = 受－施術年    ) OR
014190                       (施記－施術月     NOT = 受－施術月    )
      *
                   IF 施記－診療区分 = 1
                       MOVE "YES" TO 対象フラグ
                   ELSE
                       MOVE SPACE TO 対象フラグ
                       MOVE "YES" TO 終了フラグ３
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
012720*================================================================*
012730 施術記録Ｆ読込 SECTION.
012740*
012750     READ 施術記録Ｆ NEXT
012760     AT END
012770         MOVE "YES" TO 終了フラグ２
012780     END-READ.
012790*================================================================*
012800 受診者情報Ｆ読込 SECTION.
012810*
012820     READ 受診者情報Ｆ NEXT
012830     AT END
012840         MOVE "YES" TO 終了フラグ
012850     END-READ.
012940*================================================================*
012950 ファイル閉鎖 SECTION.
012960*
012970     CLOSE 元号マスタ   施術記録Ｆ 受診者情報Ｆ 負傷データＦ
                 メモファイル レセプトＦ 会計データＦ.
012980*================================================================*
012990 終了処理 SECTION.
013000*
013010     PERFORM ファイル閉鎖.
013720******************************************************************
013730 END PROGRAM YCH7131.
013740******************************************************************
