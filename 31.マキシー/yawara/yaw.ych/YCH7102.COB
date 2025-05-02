000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCH7102.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*         中央 カルテ裏印刷(複数月)（柔ｳｨﾝﾄﾞｳｽﾞ版）【通常用紙】
000100*         MED = YCH7101 YCH712P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2018-02-26
000130 DATE-COMPILED.          2018-02-26
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
000320     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000330                             ORGANIZATION             IS  INDEXED
000340                             ACCESS MODE              IS  DYNAMIC
000350                             RECORD KEY               IS  制－制御区分
000360                             FILE STATUS              IS  状態キー
000370                             LOCK        MODE         IS  AUTOMATIC.
000380     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000390                             ORGANIZATION             IS  INDEXED
000400                             ACCESS MODE              IS  DYNAMIC
000410                             RECORD KEY               IS 受－施術和暦年月
000420                                                          受－患者コード
000430                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000440                                                          受－患者カナ
000450                                                          受－患者コード
000460                             ALTERNATE RECORD KEY     IS  受－患者コード
000470                                                         受－施術和暦年月
000480                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000490                                                          受－保険種別
000500                                                          受－保険者番号
000510                                                          受－患者コード
000520                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000530                                                          受－公費種別
000540                                                     受－費用負担者番号
000550                                                          受－患者コード
000560                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000570                                                          受－助成種別
000580                                                  受－費用負担者番号助成
000590                                                          受－患者コード
000600                             ALTERNATE RECORD KEY  IS 受－請求和暦年月
000610                                                      受－施術和暦年月
000620                                                      受－患者コード
000630                             FILE STATUS              IS  状態キー
000640                             LOCK        MODE         IS  AUTOMATIC.
000293     SELECT  長期継続者Ｆ    ASSIGN      TO        CHOKEIL
000294                             ORGANIZATION             IS INDEXED
000295                             ACCESS MODE              IS DYNAMIC
000296                             RECORD KEY               IS 長継－施術和暦年月
000297                                                         長継－患者コード
000298                             ALTERNATE RECORD KEY     IS 長継－患者コード
000299                                                         長継－施術和暦年月
000300                             FILE STATUS              IS 状態キー
000301                             LOCK      MODE           IS AUTOMATIC.
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
000710*
000720     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF002
000730                             SYMBOLIC    DESTINATION  IS "PRT"
000740                             FORMAT                   IS  定義体名Ｐ
000750                             GROUP                    IS  項目群名Ｐ
000760                             PROCESSING  MODE         IS  処理種別Ｐ
000770                             UNIT        CONTROL      IS  拡張制御Ｐ
000780                             FILE        STATUS       IS  通知情報Ｐ.
000790******************************************************************
000800*                      DATA DIVISION                             *
000810******************************************************************
000820 DATA                    DIVISION.
000830 FILE                    SECTION.
000870*                           ［ＲＬ＝  ２５６］
000880 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
000890     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
000900*                           ［ＲＬ＝  ３２０］
000910 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
000920     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
000686*                           ［ＲＬ＝  １２８］
000687 FD  長期継続者Ｆ          BLOCK   CONTAINS   1   RECORDS.
000688     COPY CHOKEI          OF  XFDLIB  JOINING   長継   AS  PREFIX.
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
001150*
001160 FD  印刷ファイル.
001170     COPY YCH712P        OF  XMDLIB.
001180*----------------------------------------------------------------*
001190******************************************************************
001200*                WORKING-STORAGE SECTION                         *
001210******************************************************************
001220 WORKING-STORAGE         SECTION.
001230 01 キー入力                           PIC X     VALUE SPACE.
001240 01 状態キー                           PIC X(2)  VALUE SPACE.
001250 01 終了フラグ                         PIC X(3)  VALUE SPACE.
001260 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
001270 01 初検フラグ                         PIC 9(1)  VALUE ZERO.
001280 01 ファイル名                         PIC N(6)  VALUE SPACE.
001290 01 レセプトＰＧＷ                     PIC X(8)  VALUE SPACE.
001300 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
001310 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
001320 01 日位置ＣＮＴ                       PIC 9(2)  VALUE ZERO.
001330 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
001340 01 患者番号Ｗ                         PIC 9(6)  VALUE ZERO.
       01 文字ＣＮＴ                         PIC 9(2)  VALUE ZERO.
001210 01 メッセージＷ                       PIC X(240) VALUE SPACE.
       01 改行Ｗ                             PIC X(2)  VALUE X"0D0A".
001350**
001360 01 遅延フラグ                         PIC X(3) VALUE SPACE.
001370 01 遅延回数Ｗ                         PIC 9(4) VALUE ZERO.
001380 01 遅延ＣＮＴ                         PIC 9(5) VALUE ZERO.
001390**
001210 01 オープンフラグ                     PIC X(3)   VALUE SPACE.
001400*
      *
      */長期理由文用ワーク↓↓↓0401
       01 長期理由文２Ｗ.
          03 長期理由文Ｗ PIC N(35) OCCURS 16 VALUE SPACE.
001201 01 番号カウンタ                       PIC 9(2) VALUE ZERO.
001329****
001330 01 長期負傷種別Ｗ                     PIC 9(2) VALUE ZERO.
001331 01 長期部位Ｗ                         PIC 9(2) VALUE ZERO.
001332 01 理由コードＷ                       PIC 9(3) VALUE ZERO.
001340 01 症状コードＷ                       PIC 9(2) VALUE ZERO.
001350 01 症状名Ｗ                           PIC N(4) VALUE SPACE.
001361 01 合成理由文Ｗ.
001369    03 長期番号Ｗ                      PIC X(2)   VALUE SPACE.
001360    03 理由文Ｗ                        PIC X(320) VALUE SPACE.
001362* 01 カウンタ                           PIC 9(2)  VALUE ZERO.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
001365***
001366*** 症状用
001367 01 症状Ｗ.
001368    03 症状内容Ｗ                      PIC N(29) VALUE SPACE.
001401** 適用４０文字レセプト用
001402 01 理由４０桁Ｗ.
001403    03 理由４０桁１Ｗ                  PIC N(40) VALUE SPACE.
001404    03 理由４０桁２Ｗ                  PIC N(40) VALUE SPACE.
001405    03 理由４０桁３Ｗ                  PIC N(40) VALUE SPACE.
001405    03 理由４０桁４Ｗ                  PIC N(40) VALUE SPACE.
001405    03 理由４０桁５Ｗ                  PIC N(40) VALUE SPACE.
001401** 適用３５文字用
001402 01 理由３５桁Ｗ.
001403    03 理由３５桁１Ｗ                  PIC N(35) VALUE SPACE.
001404    03 理由３５桁２Ｗ                  PIC N(35) VALUE SPACE.
001405    03 理由３５桁３Ｗ                  PIC N(35) VALUE SPACE.
001405    03 理由３５桁４Ｗ                  PIC N(35) VALUE SPACE.
001405    03 理由３５桁５Ｗ                  PIC N(35) VALUE SPACE.
001405    03 理由３５桁６Ｗ                  PIC N(35) VALUE SPACE.
      *
002290 01 カウンタ                           PIC 9(2)  VALUE ZERO.
      */長期理由文用ワーク↑↑↑0401
001410****************
001420* 連結項目待避 *
001430****************
001440*    ************
001450*    * 印刷キー *
001460*    ************
001470 01 対象データＷＲ.
01870    03 開始和暦年月ＷＲ.
001880       05 開始和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
001890       05 開始年ＷＲ                    PIC 9(2)  VALUE ZERO.
001900       05 開始月ＷＲ                    PIC 9(2)  VALUE ZERO.
001910    03 開始日ＷＲ                       PIC 9(2)  VALUE ZERO.
001870    03 終了和暦年月日ＷＲ.
001870       05 終了和暦年月ＷＲ.
001880          07 終了和暦ＷＲ               PIC 9(1)  VALUE ZERO.
001890          07 終了年ＷＲ                 PIC 9(2)  VALUE ZERO.
001900          07 終了月ＷＲ                 PIC 9(2)  VALUE ZERO.
001920       05 終了日ＷＲ                    PIC 9(2)  VALUE ZERO.
001540    03 患者カナＷＲ                     PIC X(50) VALUE SPACE.
001550    03 患者コードＷＲ.
001560       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
001570       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
001580    03 印刷モードＦＷＲ                 PIC 9(1)  VALUE ZERO.
001590    03 日ごとモードＷＲ                 PIC 9(1)  VALUE ZERO.
001600    03 氏名モードＷＲ                   PIC 9(1)  VALUE ZERO.
001610    03 年月モードＷＲ                   PIC 9(1)  VALUE ZERO.
001620    03 合計モードＷＲ                   PIC 9(1)  VALUE ZERO.
001630    03 初検明細モードＷＲ               PIC 9(1)  VALUE ZERO.
001480    03 コメントモードＷＲ               PIC 9(1)  VALUE ZERO.
001640    03 印字位置ＣＮＴ                   PIC 9     VALUE ZERO.
      */長期理由印刷0401
          03 長期モードＷＲ                   PIC 9(1)  VALUE ZERO.
001650**************
001660* 受診者情報 *
001670**************
001680 01 受診者情報Ｗ.
002230    03 施術和暦年月ＷＲ.
001070       05 施術和暦ＷＲ                 PIC 9(1) VALUE ZERO.
002240       05 施術年ＷＲ                   PIC 9(2)   VALUE ZERO.
002250       05 施術月ＷＲ                   PIC 9(2)   VALUE ZERO.
001720    03 保険者番号Ｗ.
001730       05 印刷保険者番号Ｗ             PIC X(6)   VALUE SPACE.
001740       05 FILLER                       PIC X(4)   VALUE SPACE.
001750    03 退職保険者番号Ｗ.
001760       05 FILLER                       PIC X(2)   VALUE SPACE.
001770       05 退職印刷保険者番号Ｗ         PIC X(6)   VALUE SPACE.
001780       05 FILLER                       PIC X(2)   VALUE SPACE.
001790    03 市町村番号Ｗ.
001800       05 印刷市町村番号Ｗ             PIC X(8)   VALUE SPACE.
001810       05 FILLER                       PIC X(2)   VALUE SPACE.
001820    03 保険種別Ｗ                      PIC 9(2)   VALUE ZERO.
001830    03 患者情報Ｗ.
001840       05 患者カナＷ                   PIC X(50)  VALUE SPACE.
001850       05 患者氏名Ｗ                   PIC X(50)  VALUE SPACE.
001850       05 患者氏名ＷＰ.
                07 患者氏名２Ｗ              PIC N(1)  VALUE SPACE OCCURS 10.
001860************
001870* 料金情報 *
001880************
001890 01 料金情報Ｗ.
001900    03 初検加算Ｗ.
001910       05 時間外チェックＷ                PIC N(1) VALUE SPACE.
001920       05 休日チェックＷ                  PIC N(1) VALUE SPACE.
001930       05 深夜チェックＷ                  PIC N(1) VALUE SPACE.
001940    03 往療加算Ｗ.
001950       05 夜間チェックＷ                  PIC N(1) VALUE SPACE.
001960       05 難路チェックＷ                  PIC N(1) VALUE SPACE.
001970       05 暴風チェックＷ                  PIC N(1) VALUE SPACE.
001980    03 往療回数ＷＲ                       PIC 9(2)  VALUE ZERO.
001990    03 往療距離ＷＲ                       PIC 9(3)V9 VALUE ZERO.
002000    03 往療料ＷＲ                         PIC 9(6)  VALUE ZERO.
002010    03 往療加算料ＷＲ                     PIC 9(5)  VALUE ZERO.
002020    03 金属副子チェックＷ                 OCCURS 4.
002030       05 大チェックＷ                    PIC N(1) VALUE SPACE.
002040       05 中チェックＷ                    PIC N(1) VALUE SPACE.
002050       05 小チェックＷ                    PIC N(1) VALUE SPACE.
002060    03 初検料ＷＲ                         PIC 9(4) VALUE ZERO.
          03 相談料ＷＲ                         PIC 9(4) VALUE ZERO.
002070    03 再検料ＷＲ                         PIC 9(4) VALUE ZERO.
002080    03 初検加算料ＷＲ                     PIC 9(4) VALUE ZERO.
002090    03 初回処置料ＷＲ                     PIC 9(5) OCCURS 4 VALUE ZERO.
002100    03 冷罨料ＷＲ                         PIC 9(5) OCCURS 4 VALUE ZERO.
002090    03 初回処置料Ｗ                       PIC 9(5) OCCURS 4 VALUE ZERO.
002100    03 冷罨料Ｗ                           PIC 9(5) OCCURS 4 VALUE ZERO.
002110    03 初回処置料チェックＷ.
002120       05 整復料チェックＷ                PIC N(1) VALUE SPACE.
002130       05 固定料チェックＷ                PIC N(1) VALUE SPACE.
002140       05 施療料チェックＷ                PIC N(1) VALUE SPACE.
002150    03 情報提供料ＷＲ                     PIC 9(6) VALUE ZERO.
002160 01 明細.
002170    03 電罨料Ｗ                           PIC 9(4) VALUE ZERO.
002180    03 一部負担金Ｗ                       PIC 9(5) VALUE ZERO.
002190    03 部位Ｗ                             OCCURS 4.
002200       05 部位計Ｗ                        PIC 9(4) VALUE ZERO.
002210       05 日数Ｗ                          PIC 9(2) VALUE ZERO.
002520    03 コメントＷ.
002530       05 コメント１Ｗ                    PIC X(24) VALUE SPACE.
002540       05 コメント２Ｗ                    PIC X(24) VALUE SPACE.
002220*******************************************************************
002230 01 印刷制御.
002240     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
002250     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
002260     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
002270     03 拡張制御Ｐ.
002280         05 端末制御Ｐ.
002290             07 移動方向Ｐ             PIC X(1) VALUE SPACE.
002300             07 移動行数Ｐ             PIC 9(3) VALUE ZERO.
002310         05 詳細制御Ｐ                 PIC X(2) VALUE SPACE.
002320     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
002330     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
002340*
002350 01 計算機西暦年Ｗ                     PIC 9(2) VALUE ZERO.
002360* 日付ＷＯＲＫ
002370 01 和暦終了年Ｗ                       PIC 9(4) VALUE ZERO.
002380 01 計算機西暦.
002390    03 計算機西暦年                    PIC 9(4) VALUE ZERO.
002400    03 計算機西暦月日                  PIC 9(4) VALUE ZERO.
002410 01 計算機西暦Ｒ REDEFINES 計算機西暦.
002420    03 計算機世紀                      PIC 9(2).
002430    03 計算機日付                      PIC 9(6).
002440    03 計算機日付Ｒ REDEFINES 計算機日付.
002450       05 計算機年月                   PIC 9(4).
002460       05 計算機年月Ｒ REDEFINES 計算機年月.
002470         07 計算機年                   PIC 9(2).
002480         07 計算機月                   PIC 9(2).
002490       05 計算機日                     PIC 9(2).
002280*
001831* POWER COBOL用
001832 01 dll-name  PIC X(260)  VALUE SPACE.
001833 01 form-name PIC X(14)   VALUE SPACE.
002500*
002510******************************************************************
002520*                          連結項目                              *
002530******************************************************************
002540************
002550* 印刷キー *
002560************
       01 連印－対象データＹＣＨ７１０１ IS EXTERNAL GLOBAL.
          03 連印－施術和暦年月.
             05 連印－施術和暦                  PIC 9(1).
             05 連印－施術年                    PIC 9(2).
             05 連印－施術月                    PIC 9(2).
          03 連印－患者コード.
             05 連印－患者番号                  PIC 9(6).
             05 連印－枝番                      PIC X(1).
          03 連印－氏名モード                   PIC 9(1).
          03 連印－明細モード                   PIC 9(1).
          03 連印－年月モード                   PIC 9(1).
          03 連印－長期モード                   PIC 9(1).
          03 連印－コメントモード               PIC 9(1).
          03 連印－印刷モードＦ                 PIC 9(1).
          03 連印－初検明細モード               PIC 9(1).
          03 連印－合計モード                   PIC 9(1).
          03 連印－印刷位置                     PIC 9(1).
          03 連印－読み順                       PIC X(4).
          03 連印－段数                         PIC 9(2).
          03 連印－処理キー                     PIC X(4).
002820****************
002830* 画面入力情報 *
002840****************
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
      *
       01 連入－表示フラグＹＣＨ７１０１ IS EXTERNAL.
          03 連入－プレビュー区分               PIC 9(1).
003020*
003490**********************
003500* メッセージ表示キー *
003510**********************
003520 01 連メ－キー IS EXTERNAL.
003530    03  連メ－メッセージ                 PIC N(20).
003540*
       01 連メッセージＰ情報００５１ IS EXTERNAL.
          03 連メＰ－メッセージ番号       PIC 9(2).
          03 連メＰ－メッセージ.
             05 連メＰ－メッセージ内容    PIC X(40) OCCURS 6.
          03 連メＰ－メッセージ１         PIC X(20).
          03 連メＰ－メッセージ２         PIC X(12).
          03 連メＰ－返り値               PIC X.
003540*
       01 連ＰＣメッセージ情報 IS EXTERNAL.
          03 連ＰＣメ－メッセージ内容      PIC X(240).
003554*
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
003100*
003110******************************************************************
003120*                      PROCEDURE  DIVISION                       *
003130******************************************************************
003140 PROCEDURE               DIVISION.
003150************
003160*           *
003170* 初期処理   *
003180*           *
003190************
002570     PERFORM プリンタファイル作成.
003200     PERFORM 初期化.
003210     PERFORM 制御情報取得.
003220************
003230*           *
003240* 主処理     *
003250*           *
003260************
003270* 印刷
003280     PERFORM 連結項目待避.
003290     PERFORM 印刷セット１
003300     PERFORM 印刷処理
003310************
003320*           *
003330* 終了処理   *
003340*           *
003350************
003360     PERFORM 終了処理.
003370     PERFORM 遅延処理.
003380     EXIT PROGRAM.
003390*
003400*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
           MOVE "KARUTE"              TO Ｈ連特殊ＰＲＴＦ－用紙種類.
002970*   使用するプリンタファイル名セット
002231     MOVE "PRTF002"             TO Ｈ連ＰＲＴＦ－ファイル名.
002972*
002973*   使用する帳票プログラム名セット
002974     MOVE "YCH712"              TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分 TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
003410*================================================================*
003420 初期化 SECTION.
003430*
003440     PERFORM ファイルオープン.
003450*    /* 現在日付取得 */
003460     ACCEPT 計算機日付 FROM DATE.
003470*    /* 1980～2079年の間で設定 */
003480     IF 計算機年 > 80
003490         MOVE 19 TO 計算機世紀
003500     ELSE
003510         MOVE 20 TO 計算機世紀
003520     END-IF.
003530     PERFORM カレント元号取得.
003550     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
003560*================================================================*
003570 カレント元号取得 SECTION.
003580*
003590     MOVE ZEROS TO 制－制御区分.
003600     READ 制御情報マスタ
003610     NOT INVALID KEY
003620         MOVE 制－カレント元号 TO カレント元号Ｗ
003630     END-READ.
003640*
003940*================================================================*
003950 ファイルオープン SECTION.
003960*
004000     OPEN INPUT   制御情報マスタ
004010         MOVE NC"制御情報" TO ファイル名.
004020         PERFORM オープンチェック.
004030     OPEN INPUT   受診者情報Ｆ.
004040         MOVE NC"受情" TO ファイル名.
004050         PERFORM オープンチェック.
004060     OPEN INPUT 作業ファイル１
004070         MOVE NC"作業１" TO ファイル名.
004080         PERFORM オープンチェック.
004060     OPEN INPUT 作業ファイル２
004070         MOVE NC"作業２" TO ファイル名.
004080         PERFORM オープンチェック.
004060     OPEN INPUT 作業ファイル３
004070         MOVE NC"作業３" TO ファイル名.
004080         PERFORM オープンチェック.
004090*     OPEN I-O   印刷ファイル
004100*         PERFORM エラー処理Ｐ.
002650     OPEN INPUT   長期継続者Ｆ.
002651         MOVE NC"長期継続者Ｆ" TO ファイル名.
002652         PERFORM オープンチェック.
004110*================================================================*
004120 オープンチェック SECTION.
004130*
004140     IF 状態キー  NOT =  "00"
004150         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
004160         DISPLAY NC"状態キー：" 状態キー         UPON CONS
004170         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
004180                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
004190         ACCEPT  キー入力 FROM CONS
004200         PERFORM ファイル閉鎖
004210         EXIT PROGRAM.
004220*================================================================*
004230 制御情報取得 SECTION.
004240*
004250     MOVE ZERO TO 制－制御区分
004260     READ 制御情報マスタ
004270     NOT INVALID KEY
004280         MOVE 制－遅延回数       TO 遅延回数Ｗ
004290     END-READ.
004300*
004310*================================================================*
004320 遅延処理 SECTION.
004330*
004340     PERFORM VARYING 遅延ＣＮＴ FROM 1 BY 1
004350                                UNTIL 遅延ＣＮＴ > 遅延回数Ｗ
004360         MOVE SPACE TO 遅延フラグ
004370     END-PERFORM.
004380*
004390*================================================================*
004400 連結項目待避 SECTION.
004410*
004960     MOVE 連入－開始和暦           TO 開始和暦ＷＲ 施術和暦ＷＲ.
004970     MOVE 連入－開始年             TO 開始年ＷＲ 施術年ＷＲ.
004980     MOVE 連入－開始月             TO 開始月ＷＲ 施術月ＷＲ.
004990     MOVE 連入－開始日             TO 開始日ＷＲ.
004960     MOVE 連入－終了和暦           TO 終了和暦ＷＲ.
004970     MOVE 連入－終了年             TO 終了年ＷＲ.
004980     MOVE 連入－終了月             TO 終了月ＷＲ.
005000     MOVE 連入－終了日             TO 終了日ＷＲ.
005010     MOVE 連入－患者番号           TO 患者番号ＷＲ.
005020     MOVE 連入－枝番               TO 枝番ＷＲ.
005150     MOVE 連印－氏名モード         TO 氏名モードＷＲ.
005150     MOVE 連印－年月モード         TO 年月モードＷＲ.
005040     MOVE 連印－明細モード         TO 日ごとモードＷＲ.
005090     MOVE 連印－コメントモード     TO コメントモードＷＲ.
005050     MOVE 連印－初検明細モード     TO 初検明細モードＷＲ.
005060     MOVE 連印－合計モード         TO 合計モードＷＲ.
           MOVE 連印－長期モード         TO 長期モードＷＲ.
004550     MOVE 連印－印刷位置           TO 印字位置ＣＮＴ.
004490     MOVE 連印－印刷モードＦ       TO 印刷モードＦＷＲ.
004560*================================================================*
004570 印刷セット１ SECTION.
004580*
004590     PERFORM 項目初期化.
004600     PERFORM 受診者情報取得.
004800     IF (合計モードＷＲ = 1) OR (日ごとモードＷＲ = 1) OR
              (コメントモードＷＲ = 1) OR (初検明細モードＷＲ = 1)
005400         PERFORM 印刷データセット
           ELSE
004640         IF 氏名モードＷＲ = 1
                   PERFORM ヘッダセット
004660         END-IF
004670         IF 年月モードＷＲ = 1
004680             EVALUATE 印字位置ＣＮＴ
004690             WHEN 1
004700                 MOVE 施術年ＷＲ      TO 施術年左
004710                 MOVE 施術月ＷＲ      TO 施術月左
004720             WHEN 2
004730                 MOVE 施術年ＷＲ      TO 施術年中
004740                 MOVE 施術月ＷＲ      TO 施術月中
004750             WHEN 3
004760                 MOVE 施術年ＷＲ      TO 施術年右
004770                 MOVE 施術月ＷＲ      TO 施術月右
004780             END-EVALUATE
004790         END-IF
      */長期理由を印刷0401
               IF 長期モードＷＲ = 1
                   PERFORM 長期理由文取得
004810             EVALUATE 印字位置ＣＮＴ
004820             WHEN 1
004830                 PERFORM 長期セット左
004840             WHEN 2
004850                 PERFORM 長期セット中
004860             WHEN 3
004870                 PERFORM 長期セット右
004880             END-EVALUATE
               END-IF
004890     END-IF.
005530*================================================================*
005540 ヘッダセット SECTION.
005550*
004650     MOVE 患者氏名Ｗ        TO 患者氏名 患者氏名ＷＰ.
           MOVE 患者番号ＷＲ      TO 患者番号.
           MOVE 枝番ＷＲ          TO 枝番.
           PERFORM VARYING 文字ＣＮＴ FROM 1 BY 1 
                     UNTIL 文字ＣＮＴ > 10
              MOVE 患者氏名２Ｗ(文字ＣＮＴ) TO 患者氏名２(文字ＣＮＴ)
           END-PERFORM.
005530*================================================================*
005540 印刷データセット SECTION.
005550*
           MOVE 1                  TO 初検フラグ.
           MOVE SPACE              TO 終了フラグ.
005560     MOVE 開始和暦ＷＲ       TO 作１－施術和暦.
005570     MOVE 開始年ＷＲ         TO 作１－施術年.
005580     MOVE 開始月ＷＲ         TO 作１－施術月.
005590     MOVE 開始日ＷＲ         TO 作１－施術日.
005590     MOVE 患者コードＷＲ     TO 作１－患者コード.
005600     START 作業ファイル１ KEY IS >= 作１－患者コード
                                          作１－施術和暦年月日
005610     END-START.
005620     IF 状態キー = "00"
005630         PERFORM 作業ファイル１読込
               MOVE 患者番号ＷＲ     TO 作３－患者番号
               MOVE 開始和暦年月ＷＲ TO 作３－施術和暦年月
008420         READ 作業ファイル３
               NOT INVALID KEY
                  IF (枝番ＷＲ = SPACE) AND (作３－枝番Ｆ = 1)
                     MOVE SPACE  TO  連メッセージＰ情報００５１
                     MOVE 10     TO  連メＰ－メッセージ番号
                     MOVE "枝番があります。"               TO 連メＰ－メッセージ内容(1)
                     MOVE "印刷してよろしければ［はい］を" TO 連メＰ－メッセージ内容(2)
                     MOVE "押してください。"               TO 連メＰ－メッセージ内容(3)
                     MOVE "PMSG0051.DLL"     TO dll-name
                     MOVE "PMSG0051"         TO form-name
                     CALL "POWEROPENSHEET" USING dll-name form-name
                     EVALUATE  連メＰ－返り値
                     WHEN "Y"
                         CONTINUE
                     WHEN OTHER
                         PERFORM 終了処理
005933                   PERFORM 遅延処理
005934                   EXIT PROGRAM
                     END-EVALUATE
                  END-IF
               END-READ
005220         IF 氏名モードＷＲ = 1
                   PERFORM ヘッダセット
005360         END-IF
004670         IF 年月モードＷＲ = 1
004680             EVALUATE 印字位置ＣＮＴ
004690             WHEN 1
004700                 MOVE 施術年ＷＲ      TO 施術年左
004710                 MOVE 施術月ＷＲ      TO 施術月左
004720             WHEN 2
004730                 MOVE 施術年ＷＲ      TO 施術年中
004740                 MOVE 施術月ＷＲ      TO 施術月中
004750             WHEN 3
004760                 MOVE 施術年ＷＲ      TO 施術年右
004770                 MOVE 施術月ＷＲ      TO 施術月右
004780             END-EVALUATE
               END-IF
005640         PERFORM UNTIL (作１－施術和暦年月日 > 終了和暦年月日ＷＲ) OR
                             (終了フラグ = "YES") OR
                             (作１－患者コード NOT = 患者コードＷＲ)
005640            PERFORM UNTIL (作１－施術和暦     NOT = 施術和暦ＷＲ) OR
                                (作１－施術年       NOT = 施術年ＷＲ) OR
                                (作１－施術月       NOT = 施術月ＷＲ) OR
                                (終了フラグ = "YES") OR
                                (作１－患者コード NOT = 患者コードＷＲ)
      */初検があっても改頁しない
      *               IF ( 作１－初検 = 1 ) AND (初検フラグ = ZERO)
005830*                  MOVE SPACE  TO  連メッセージＰ情報００５１
      *                  MOVE 7      TO  連メＰ－メッセージ番号
005840*                  MOVE "初検があります。"             TO 連メＰ－メッセージ内容(1)
005840*                  MOVE "新しいカルテに変更しますか？" TO 連メＰ－メッセージ内容(2)
004793*                  MOVE "PMSG0051.DLL" TO dll-name
004794*                  MOVE "PMSG0051"     TO form-name
004795*                  CALL "POWEROPENSHEET" USING dll-name form-name
005870*                  EVALUATE  連メＰ－返り値
005871*                  WHEN "Y"
      *                      IF 合計モードＷＲ = 1
      *                          PERFORM 月合計セット
005450*                      END-IF
004800*                      IF 初検明細モードＷＲ = 1
005400*                         PERFORM 印刷セット２
004890*                      END-IF
      *                      IF 長期モードＷＲ = 1
      *                         PERFORM 長期理由文取得
004810*                         EVALUATE 印字位置ＣＮＴ
004820*                         WHEN 1
004830*                             PERFORM 長期セット左
004840*                         WHEN 2
004850*                             PERFORM 長期セット中
004860*                         WHEN 3
004870*                             PERFORM 長期セット右
004880*                         END-EVALUATE
      *                      END-IF
005810*                      PERFORM 印刷処理
005820*                      PERFORM 改頁処理
005872*                      MOVE 1             TO 印字位置ＣＮＴ
      *                      MOVE 1             TO 初検フラグ
005220*                      IF 氏名モードＷＲ = 1
      *                          PERFORM ヘッダセット
005360*                      END-IF
004670*                      IF 年月モードＷＲ = 1
004680*                          EVALUATE 印字位置ＣＮＴ
004690*                          WHEN 1
004700*                              MOVE 施術年ＷＲ      TO 施術年左
004710*                              MOVE 施術月ＷＲ      TO 施術月左
004720*                          WHEN 2
004730*                              MOVE 施術年ＷＲ      TO 施術年中
004740*                              MOVE 施術月ＷＲ      TO 施術月中
004750*                          WHEN 3
004760*                              MOVE 施術年ＷＲ      TO 施術年右
004770*                              MOVE 施術月ＷＲ      TO 施術月右
004780*                          END-EVALUATE
      *                      END-IF
005871*                  WHEN "N"
      *                      CONTINUE
005981*                  WHEN OTHER
      **/行退避のタイミングの変更と追加/060201
      *                      IF 合計モードＷＲ = 1
      *                          PERFORM 月合計セット
005450*                      END-IF
004800*                      IF 初検明細モードＷＲ = 1
005400*                          PERFORM 印刷セット２
004890*                      END-IF
      *                      IF 長期モードＷＲ = 1
      *                         PERFORM 長期理由文取得
004810*                         EVALUATE 印字位置ＣＮＴ
004820*                         WHEN 1
004830*                             PERFORM 長期セット左
004840*                         WHEN 2
004850*                             PERFORM 長期セット中
004860*                         WHEN 3
004870*                             PERFORM 長期セット右
004880*                         END-EVALUATE
      *                      END-IF
005810*                      PERFORM 印刷処理
005820*                      PERFORM 改頁処理
005982*                      PERFORM 終了処理
005983*                      PERFORM 遅延処理
005984*                      EXIT PROGRAM
005985*                  END-EVALUATE
      *               END-IF
005340               IF (日ごとモードＷＲ = 1) OR (コメントモードＷＲ = 1)
004810                  EVALUATE 印字位置ＣＮＴ
004820                  WHEN 1
004830                      PERFORM 左側印刷
004840                  WHEN 2
004850                      PERFORM 中側印刷
004860                  WHEN 3
004870                      PERFORM 右側印刷
004880                  END-EVALUATE
005450               END-IF
005340*               IF (初検明細モードＷＲ = 1) AND (作１－初検 = 1)
005340               IF (初検明細モードＷＲ = 1) AND (作１－日チェック = 1)
                        PERFORM 初検明細セット
005450               END-IF
                     MOVE 0             TO 初検フラグ
007950               PERFORM 作業ファイル１読込
                  END-PERFORM
                  IF (作１－患者コード = 患者コードＷＲ) AND
                     ((作１－施術和暦 NOT = 施術和暦ＷＲ) OR
                      (作１－施術年   NOT = 施術年ＷＲ) OR
                      (作１－施術月   NOT = 施術月ＷＲ))
                     IF 合計モードＷＲ = 1
                         PERFORM 月合計セット
                     END-IF
                     IF 長期モードＷＲ = 1
                        PERFORM 長期理由文取得
004810                  EVALUATE 印字位置ＣＮＴ
004820                  WHEN 1
004830                      PERFORM 長期セット左
004840                  WHEN 2
004850                      PERFORM 長期セット中
004860                  WHEN 3
004870                      PERFORM 長期セット右
004880                  END-EVALUATE
                     END-IF
      */月途中変更があった場合終了
                     MOVE 患者番号ＷＲ       TO 作３－患者番号
                     MOVE 作１－施術和暦     TO 作３－施術和暦
                     MOVE 作１－施術年       TO 作３－施術年
                     MOVE 作１－施術月       TO 作３－施術月
008420               READ 作業ファイル３
                     NOT INVALID KEY
                        IF (作１－枝番 = SPACE) AND
                           ((作３－枝番Ｆ = 1) OR (作３－保険変更Ｆ = 1))
                           STRING "保険証内容が変更されています。"   DELIMITED BY SIZE
                                  改行Ｗ                             DELIMITED BY SIZE
                                  "確認してください。"               DELIMITED BY SIZE
                                  改行Ｗ                             DELIMITED BY SIZE
                                  "変更の前月まで印刷して中断します。"  DELIMITED BY SIZE
                             INTO メッセージＷ
                           END-STRING
005830                     MOVE SPACE  TO  連ＰＣメッセージ情報
                           MOVE メッセージＷ     TO 連ＰＣメ－メッセージ内容
004793                     MOVE "Pmsg001.DLL"    TO dll-name
004794                     MOVE "Pmsg001"        TO form-name
004795                     CALL "POWEROPENSHEET" USING dll-name form-name
005830                     MOVE SPACE            TO  メッセージＷ
                           MOVE "YES"            TO 終了フラグ
004800                     IF 初検明細モードＷＲ = 1
005400                         PERFORM 印刷セット２
004890                     END-IF
005810                     PERFORM 印刷処理
005820                     PERFORM 改頁処理
005932                     PERFORM 終了処理
005933                     PERFORM 遅延処理
005934                     EXIT PROGRAM
                        END-IF
                     END-READ
                     MOVE 作１－施術和暦      TO 施術和暦ＷＲ
                     MOVE 作１－施術年        TO 施術年ＷＲ
                     MOVE 作１－施術月        TO 施術月ＷＲ
                     COMPUTE 印字位置ＣＮＴ = 印字位置ＣＮＴ + 1
                     IF 印字位置ＣＮＴ > 3
004800                  IF 初検明細モードＷＲ = 1
005400                      PERFORM 印刷セット２
004890                  END-IF
005810                  PERFORM 印刷処理
005820                  PERFORM 改頁処理
005872                  MOVE 1             TO 印字位置ＣＮＴ
005220                  IF 氏名モードＷＲ = 1
                            PERFORM ヘッダセット
005360                  END-IF
                     END-IF
004670               IF 年月モードＷＲ = 1
004680                   EVALUATE 印字位置ＣＮＴ
004690                   WHEN 1
004700                       MOVE 施術年ＷＲ      TO 施術年左
004710                       MOVE 施術月ＷＲ      TO 施術月左
004720                   WHEN 2
004730                       MOVE 施術年ＷＲ      TO 施術年中
004740                       MOVE 施術月ＷＲ      TO 施術月中
004750                   WHEN 3
004760                       MOVE 施術年ＷＲ      TO 施術年右
004770                       MOVE 施術月ＷＲ      TO 施術月右
004780                   END-EVALUATE
                     END-IF
                  ELSE
                     IF 合計モードＷＲ = 1
                         PERFORM 月合計セット
                     END-IF
                     IF 長期モードＷＲ = 1
                        PERFORM 長期理由文取得
004810                  EVALUATE 印字位置ＣＮＴ
004820                  WHEN 1
004830                      PERFORM 長期セット左
004840                  WHEN 2
004850                      PERFORM 長期セット中
004860                  WHEN 3
004870                      PERFORM 長期セット右
004880                  END-EVALUATE
                     END-IF
004800               IF 初検明細モードＷＲ = 1
005400                   PERFORM 印刷セット２
004890               END-IF
                  END-IF
007960         END-PERFORM
007970     END-IF.
           IF 枝番ＷＲ NOT = SPACE
003970         MOVE  NC"印刷の続きがないか確認して下さい。" TO 連メ－メッセージ
013150         CALL   "MSG001"
013160         CANCEL "MSG001"
           END-IF.
004900*================================================================*
004910 印刷セット２ SECTION.
004920*
004930********************
004940* 料金データセット *
004950********************
004970*    ****************************************************************
004980*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
004990*    ****************************************************************
005000     MOVE 初検料ＷＲ                   TO  初検料.
           MOVE 相談料ＷＲ                   TO  相談料.
           IF 相談料ＷＲ NOT = ZERO
               MOVE "初検時相談支援料"       TO  相談料Ｔ
               MOVE "円"                     TO  相談料円
           END-IF.
005010     MOVE 時間外チェックＷ             TO  時間外チェック.
005020     MOVE 休日チェックＷ               TO  休日チェック.
005030     MOVE 深夜チェックＷ               TO  深夜チェック.
005040*     MOVE 午前チェックＷ               TO  午前チェック.
005050*     MOVE 午後チェックＷ               TO  午後チェック.
005060*     MOVE 時Ｗ                         TO  時.
005070*     MOVE 分Ｗ                         TO  分.
           IF 休日チェックＷ NOT = SPACE
005090         MOVE 初検加算料ＷＲ           TO  休日加算
           ELSE
005110         MOVE 初検加算料ＷＲ           TO  時間外加算
005130     END-IF.
           MOVE 再検料ＷＲ                   TO  再検料.
005140     MOVE 往療距離ＷＲ                 TO  往療距離.
005150     MOVE 往療回数ＷＲ                 TO  往療回数.
005160     MOVE 往療料ＷＲ                   TO  往療料.
005170     MOVE 夜間チェックＷ               TO  夜間チェック.
005180     MOVE 難路チェックＷ               TO  難路チェック.
005190     MOVE 暴風チェックＷ               TO  暴風チェック.
005200     MOVE 往療加算料ＷＲ               TO  加算料合計.
005210     MOVE 情報提供料ＷＲ               TO  情報提供料.
005220     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 4
005230         MOVE 冷罨料ＷＲ(部位ＣＮＴ)     TO  冷罨料(部位ＣＮＴ)
005240         MOVE 初回処置料ＷＲ(部位ＣＮＴ) TO  処置料(部位ＣＮＴ)
005250*         MOVE 大チェックＷ(部位ＣＮＴ) TO  大チェック(部位ＣＮＴ)
005260*         MOVE 中チェックＷ(部位ＣＮＴ) TO  中チェック(部位ＣＮＴ)
005270*         MOVE 小チェックＷ(部位ＣＮＴ) TO  小チェック(部位ＣＮＴ)
005280     END-PERFORM.
005290*
005300*================================================================*
005310 項目初期化 SECTION.
005320*
005340     INITIALIZE 料金情報Ｗ.
005350     INITIALIZE 明細.
005360     MOVE SPACE TO YCH712P.
005370     INITIALIZE YCH712P.
005380*================================================================*
005390 左側印刷 SECTION.
005590     IF 作１－日チェック = 1
              IF 日ごとモードＷＲ = 1
005600           MOVE 作１－部位計(1)    TO 左１(作１－施術日)
005610           MOVE 作１－部位計(2)    TO 左２(作１－施術日)
005620           MOVE 作１－部位計(3)    TO 左３(作１－施術日)
005630           MOVE 作１－部位計(4)    TO 左４(作１－施術日)
005640           MOVE 作１－罨法金額     TO 左電罨(作１－施術日)
005650           IF 印刷モードＦＷＲ = ZERO
005670              MOVE 作１－一部負担金   TO 左一部負担金(作１－施術日)
                 END-IF
005680        END-IF
              IF (コメントモードＷＲ = 1) AND (印刷モードＦＷＲ NOT = ZERO)
                 MOVE 作１－コメント  TO コメントＷ
                 MOVE コメント１Ｗ    TO 左コメント１(作１－施術日)
                 MOVE コメント２Ｗ    TO 左コメント２(作１－施術日)
005720        END-IF
005730     END-IF.
005740*================================================================*
005750 中側印刷 SECTION.
005590     IF 作１－日チェック = 1
              IF 日ごとモードＷＲ = 1
005600           MOVE 作１－部位計(1)    TO 中１(作１－施術日)
005610           MOVE 作１－部位計(2)    TO 中２(作１－施術日)
005620           MOVE 作１－部位計(3)    TO 中３(作１－施術日)
005630           MOVE 作１－部位計(4)    TO 中４(作１－施術日)
005640           MOVE 作１－罨法金額     TO 中電罨(作１－施術日)
005650           IF 印刷モードＦＷＲ = ZERO
005670              MOVE 作１－一部負担金   TO 中一部負担金(作１－施術日)
                 END-IF
005680        END-IF
              IF (コメントモードＷＲ = 1) AND (印刷モードＦＷＲ NOT = ZERO)
                 MOVE 作１－コメント  TO コメントＷ
                 MOVE コメント１Ｗ    TO 中コメント１(作１－施術日)
                 MOVE コメント２Ｗ    TO 中コメント２(作１－施術日)
005720        END-IF
005730     END-IF.
006100*================================================================*
006110 右側印刷 SECTION.
005590     IF 作１－日チェック = 1
              IF 日ごとモードＷＲ = 1
005600           MOVE 作１－部位計(1)    TO 右１(作１－施術日)
005610           MOVE 作１－部位計(2)    TO 右２(作１－施術日)
005620           MOVE 作１－部位計(3)    TO 右３(作１－施術日)
005630           MOVE 作１－部位計(4)    TO 右４(作１－施術日)
005640           MOVE 作１－罨法金額     TO 右電罨(作１－施術日)
005650           IF 印刷モードＦＷＲ = ZERO
005670              MOVE 作１－一部負担金   TO 右一部負担金(作１－施術日)
                 END-IF
005680        END-IF
              IF (コメントモードＷＲ = 1) AND (印刷モードＦＷＲ NOT = ZERO)
                 MOVE 作１－コメント  TO コメントＷ
                 MOVE コメント１Ｗ    TO 右コメント１(作１－施術日)
                 MOVE コメント２Ｗ    TO 右コメント２(作１－施術日)
005720        END-IF
005730     END-IF.
007980*================================================================*
007990 月合計セット SECTION.
008000*
           MOVE 施術和暦年月ＷＲ   TO 作２－施術和暦年月.
           MOVE 患者コードＷＲ     TO 作２－患者コード.
           READ 作業ファイル２
           NOT INVALID KEY
               EVALUATE 印字位置ＣＮＴ
               WHEN 1
                   MOVE 作２－日数１    TO 左日数１
                   MOVE 作２－日数２    TO 左日数２
                   MOVE 作２－日数３    TO 左日数３
                   MOVE 作２－日数４    TO 左日数４
                   MOVE 作２－電罨日数  TO 左電罨日数
                   MOVE 作２－金属運動ＣＭ  TO 左金属運動ＣＭ
               WHEN 2
                   MOVE 作２－日数１    TO 中日数１
                   MOVE 作２－日数２    TO 中日数２
                   MOVE 作２－日数３    TO 中日数３
                   MOVE 作２－日数４    TO 中日数４
                   MOVE 作２－電罨日数  TO 中電罨日数
                   MOVE 作２－金属運動ＣＭ  TO 中金属運動ＣＭ
               WHEN 3
                   MOVE 作２－日数１    TO 右日数１
                   MOVE 作２－日数２    TO 右日数２
                   MOVE 作２－日数３    TO 右日数３
                   MOVE 作２－日数４    TO 右日数４
                   MOVE 作２－電罨日数  TO 右電罨日数
                   MOVE 作２－金属運動ＣＭ  TO 右金属運動ＣＭ
               END-EVALUATE
           END-READ.
006460*================================================================*
006470 初検明細セット SECTION.
006480*
006490     COMPUTE 初検料ＷＲ = 初検料ＷＲ + 作１－初検料.
006490     COMPUTE 再検料ＷＲ = 再検料ＷＲ + 作１－再検料.
006490     COMPUTE 相談料ＷＲ = 相談料ＷＲ + 作１－相談料.
006490     COMPUTE 初検加算料ＷＲ = 初検加算料ＷＲ + 作１－初検加算料.
006510     IF 作１－休日 = 1
006520         MOVE NC"○"       TO 休日チェックＷ
006530     END-IF.
006540     IF 作１－深夜 = 1
006550         MOVE NC"○"       TO 深夜チェックＷ
006560     END-IF.
006570     IF 作１－時間外 = 1
006580         MOVE NC"○"       TO 時間外チェックＷ
006590     END-IF.
006610     IF 作１－夜間 = 1
006620         MOVE NC"○"       TO 夜間チェックＷ
006630     END-IF.
006640     IF 作１－難路 = 1
006650         MOVE NC"○"       TO 難路チェックＷ
006660     END-IF.
006670     IF 作１－暴風 = 1
006680         MOVE NC"○"       TO 暴風チェックＷ
006690     END-IF.
006490     COMPUTE 往療回数ＷＲ = 往療回数ＷＲ + 作１－回数.
006710     MOVE 作１－距離       TO 往療距離ＷＲ.
006490     COMPUTE 往療料ＷＲ = 往療料ＷＲ + 作１－往療料金額.
006490     COMPUTE 往療加算料ＷＲ = 往療加算料ＷＲ + 作１－往療加算料.
006490     COMPUTE 情報提供料ＷＲ = 情報提供料ＷＲ + 作１－情報提供料.
006750     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 4
006760*         EVALUATE 作１－金属(部位ＣＮＴ)
006770*         WHEN 1
006780*             MOVE NC"○"   TO 大チェックＷ(部位ＣＮＴ)
006790*         WHEN 2
006800*             MOVE NC"○"   TO 中チェックＷ(部位ＣＮＴ)
006810*         WHEN 3
006820*             MOVE NC"○"   TO 小チェックＷ(部位ＣＮＴ)
006830*         END-EVALUATE
006490         COMPUTE 初回処置料ＷＲ(部位ＣＮＴ) = 初回処置料ＷＲ(部位ＣＮＴ) + 作１－初回処置料(部位ＣＮＴ)
006490         COMPUTE 冷罨料ＷＲ(部位ＣＮＴ) = 冷罨料ＷＲ(部位ＣＮＴ) + 作１－冷罨料(部位ＣＮＴ)
006490*         COMPUTE 金額ＷＲ(部位ＣＮＴ) = 金額ＷＲ(部位ＣＮＴ) + 作１－金額(部位ＣＮＴ)
006870     END-PERFORM.
006880*================================================================*
006890 受診者情報取得 SECTION.
006900*
006910**************************************************
006920* 連結データから受診者情報Ｆより以下の情報を取得 *
006930* ● 施術年 ..... 施術年Ｗに格納                 *
006940* ● 施術月 ..... 施術月Ｗに格納                 *
006950* ● 患者番号.... 患者番号Ｗに格納※ＦＤ連番用   *
006960* ● 患者氏名 ....患者氏名Ｗに格納               *
006970**************************************************
006980     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
006990     MOVE 施術年ＷＲ         TO 受－施術年.
007000     MOVE 施術月ＷＲ         TO 受－施術月.
007010     MOVE 患者コードＷＲ     TO 受－患者コード.
007020     READ 受診者情報Ｆ
007030     INVALID KEY
007040         CONTINUE
007050*            /* ありえない */
007060     NOT INVALID KEY
007090         MOVE 受－患者番号     TO 患者番号Ｗ
007100         MOVE 受－患者氏名     TO 患者氏名Ｗ
007110     END-READ.
007120*================================================================*
007130 作業ファイル１読込 SECTION.
007140*
007150     READ 作業ファイル１ NEXT
007160     AT END
007170         MOVE "YES" TO 終了フラグ
007180     END-READ.
007190*
007200*================================================================*
007210 印刷処理 SECTION.
007220*
004310     IF ( オープンフラグ NOT = "YES" )
004320        MOVE "YES" TO オープンフラグ
004330        OPEN I-O  印刷ファイル
004340        PERFORM エラー処理Ｐ
004350     END-IF.
013440*
007230     MOVE "YCH712P"  TO  定義体名Ｐ.
007240     MOVE "GRP001"   TO  項目群名Ｐ.
006440     MOVE SPACE      TO  処理種別Ｐ.
006450     MOVE SPACE      TO  拡張制御Ｐ.
007250     WRITE YCH712P.
007260     PERFORM エラー処理Ｐ.
008560*================================================================*
008570 改頁処理  SECTION.
008580
006600     MOVE "YCH712P" TO  定義体名Ｐ.
006610     MOVE "CT"      TO  処理種別Ｐ.
006620     MOVE "PAGE"    TO  拡張制御Ｐ.
006630     MOVE SPACE     TO  項目群名Ｐ.
006640     WRITE YCH712P.
006650     PERFORM エラー処理Ｐ.
006660     MOVE SPACE     TO  拡張制御Ｐ.
005190     PERFORM 項目初期化.
008660*
008670     CLOSE  印刷ファイル.
004310     IF ( オープンフラグ = "YES" )
004320        MOVE SPACE TO オープンフラグ
004350     END-IF.
008700*
007270*================================================================*
007280 エラー処理Ｐ SECTION.
007290*
007300     IF 通知情報Ｐ NOT = "00"
007310         DISPLAY NC"帳票エラー"              UPON CONS
007320         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
007330         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
007340         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
007350         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
007360                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007370         ACCEPT  キー入力 FROM CONS
007380         PERFORM ファイル閉鎖
007390         MOVE 99 TO PROGRAM-STATUS
007400         EXIT PROGRAM
007410     END-IF.
007420*================================================================*
007430 ファイル閉鎖 SECTION.
007440*
002990     IF ( オープンフラグ = "YES" )
002991         CLOSE 印刷ファイル
003041     END-IF.
007460     CLOSE 制御情報マスタ  受診者情報Ｆ  長期継続者Ｆ.
008670     CLOSE  作業ファイル１ 作業ファイル２ 作業ファイル３.
007470*================================================================*
007480 終了処理 SECTION.
007490*
007500     PERFORM ファイル閉鎖.
007510*================================================================*
035270 長期理由文取得 SECTION.
003034     MOVE  ZERO   TO  カウンタ.
           MOVE  SPACE  TO  長期理由文２Ｗ.
035280*
004502     MOVE 施術和暦ＷＲ   TO 長継－施術和暦
004503     MOVE 施術年ＷＲ     TO 長継－施術年.
004504     MOVE 施術月ＷＲ     TO 長継－施術月.
004505     MOVE 患者番号ＷＲ   TO 長継－患者番号.
004506     MOVE 枝番ＷＲ       TO 長継－枝番.
004507*
004508     READ 長期継続者Ｆ 
004509     INVALID KEY
                CONTINUE
004522     NOT INVALID KEY
004523         IF  (長継－理由文(1)  = ZERO )  AND
004524             (長継－理由文(2)  = ZERO )  AND
004525             (長継－理由文(3)  = ZERO )  AND
004526             (長継－理由文(4)  = ZERO )  AND
004527             (長継－理由文(5)  = ZERO )
                   CONTINUE
004535         ELSE
                   PERFORM 理由文セット
004537         END-IF
004538     END-READ.
035430*================================================================*
004541 理由文セット SECTION.
004542*
004543     PERFORM VARYING 番号カウンタ FROM 1 BY 1
004544             UNTIL   番号カウンタ  > 5 
004559*
004560         IF  長継－理由文(番号カウンタ) NOT = SPACE 
004561             PERFORM 番号転記
004595* 理由文
004598           MOVE 長継－理由文(番号カウンタ)  TO  理由文Ｗ
                 PERFORM 理由文分解３５桁
004631         END-IF
004632**
004637     END-PERFORM.
004647*
005590*================================================================*
005600 番号転記 SECTION.
005601*
005605      EVALUATE 番号カウンタ
005606      WHEN  1
005607          MOVE "①"  TO 長期番号Ｗ
005608      WHEN  2
005609          MOVE "②"  TO 長期番号Ｗ
005610      WHEN  3
005611          MOVE "③"  TO 長期番号Ｗ
005612      WHEN  4
005613          MOVE "④"  TO 長期番号Ｗ
005614      WHEN  5
005615          MOVE "⑤"  TO 長期番号Ｗ
005616      WHEN  OTHER
005617          CONTINUE
005618      END-EVALUATE.
005619*
005898*================================================================*
005706 理由文分解３５桁 SECTION.
005707**********
005708*** ３５桁
005709**********
005716* 合成した理由文を分割する。
005717     INITIALIZE 理由３５桁Ｗ.
005718     MOVE  合成理由文Ｗ  TO  理由３５桁Ｗ.
005945*半角スペースを全角にかえる(189ﾊﾞｲﾄより長いｴﾘｱに半角空白が入っているので)
005946     INSPECT 理由３５桁Ｗ REPLACING ALL 半角空白 BY 全角空白.
005719*
005720     IF  理由３５桁１Ｗ  NOT = SPACE
005721         COMPUTE カウンタ = カウンタ  +  1
005722         MOVE 理由３５桁１Ｗ  TO 長期理由文Ｗ(カウンタ)
005724     END-IF.
005725     IF  理由３５桁２Ｗ  NOT = SPACE
005726         COMPUTE カウンタ = カウンタ  +  1
005727         MOVE 理由３５桁２Ｗ  TO 長期理由文Ｗ(カウンタ)
005729     END-IF.
005730     IF  理由３５桁３Ｗ  NOT = SPACE
005731         COMPUTE カウンタ = カウンタ  +  1
005732         MOVE 理由３５桁３Ｗ  TO 長期理由文Ｗ(カウンタ)
005733     END-IF.
           IF  理由３５桁４Ｗ  NOT = SPACE
               COMPUTE カウンタ = カウンタ  +  1
               MOVE 理由３５桁４Ｗ  TO 長期理由文Ｗ(カウンタ)
           END-IF.
           IF  理由３５桁５Ｗ  NOT = SPACE
               COMPUTE カウンタ = カウンタ  +  1
               MOVE 理由３５桁５Ｗ  TO 長期理由文Ｗ(カウンタ)
           END-IF.
           IF  理由３５桁６Ｗ  NOT = SPACE
               COMPUTE カウンタ = カウンタ  +  1
               MOVE 理由３５桁６Ｗ  TO 長期理由文Ｗ(カウンタ)
           END-IF.
005734*
005898*================================================================*
004830 長期セット左 SECTION.
      *
      ************
      * 長期理由 *
      ************
      */長期理由を印刷0311
           PERFORM VARYING カウンタ FROM 1 BY 1
                   UNTIL   カウンタ > 7
               MOVE 長期理由文Ｗ(カウンタ) TO 長期理由文(カウンタ)
           END-PERFORM.
      *
007510*================================================================*
004850 長期セット中 SECTION.
      *
      ************
      * 長期理由 *
      ************
      */長期理由を印刷0311
           PERFORM VARYING カウンタ FROM 1 BY 1
                   UNTIL   カウンタ > 7
               MOVE 長期理由文Ｗ(カウンタ) TO 長期理由文２(カウンタ)
           END-PERFORM.
      *
007510*================================================================*
004870 長期セット右 SECTION.
      *
      ************
      * 長期理由 *
      ************
      */長期理由を印刷0311
           PERFORM VARYING カウンタ FROM 1 BY 1
                   UNTIL   カウンタ > 7
               MOVE 長期理由文Ｗ(カウンタ) TO 長期理由文３(カウンタ)
           END-PERFORM.
      *
007510*================================================================*
007520******************************************************************
007530 END PROGRAM YCH7102.
007540******************************************************************
