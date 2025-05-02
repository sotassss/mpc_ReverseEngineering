000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YJB7223.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*         施術明細（柔+ｳｨﾝﾄﾞｳｽﾞ版）
000100*         MED = YJB720 YJB7223P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2022-08-18
000130 DATE-COMPILED.          2022-08-18
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
000260     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY               IS  元－元号区分
000300                             FILE STATUS              IS  状態キー
000310                             LOCK        MODE         IS  AUTOMATIC.
000320     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000330                             ORGANIZATION             IS  INDEXED
000340                             ACCESS MODE              IS  DYNAMIC
000350                             RECORD KEY               IS  制－制御区分
000360                             FILE STATUS              IS  状態キー
000370                             LOCK        MODE         IS  AUTOMATIC.
000410     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000420                             ORGANIZATION             IS  INDEXED
000430                             ACCESS MODE              IS  DYNAMIC
000440                             RECORD KEY               IS  名－区分コード
000450                                                          名－名称コード
000460                             FILE STATUS              IS  状態キー
000470                             LOCK        MODE         IS  AUTOMATIC.
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
000380     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000390                             ORGANIZATION             IS  INDEXED
000400                             ACCESS MODE              IS  DYNAMIC
000410                             RECORD KEY               IS 受－施術和暦年月
000420                                                         受－患者コード
000430                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000440                                                         受－患者カナ
000450                                                         受－患者コード
000460                             ALTERNATE RECORD KEY     IS 受－患者コード
000470                                                         受－施術和暦年月
000480                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000490                                                         受－保険種別
000500                                                         受－保険者番号
000510                                                         受－患者コード
000520                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000530                                                         受－公費種別
000540                                                         受－費用負担者番号
000550                                                         受－患者コード
000560                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000570                                                         受－助成種別
000580                                                         受－費用負担者番号助成
000590                                                         受－患者コード
000600                             ALTERNATE RECORD KEY     IS 受－請求和暦年月
000610                                                         受－施術和暦年月
000620                                                         受－患者コード
000630                             FILE STATUS              IS  状態キー
000640                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  作業ファイル１  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W7211L.DAT"
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
000530     SELECT  作業ファイル２  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W7212L.DAT"
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS                   IS  DYNAMIC
000560                             RECORD      KEY          IS  作２－施術和暦年月
000610                                                          作２－患者コード
000620                             FILE        STATUS       IS  状態キー
000630                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  作業ファイル３  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W7213L.DAT"
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS                   IS  DYNAMIC
000560                             RECORD      KEY          IS  作３－施術和暦年月
000610                                                          作３－患者コード
000620                             FILE        STATUS       IS  状態キー
000630                             LOCK        MODE         IS  AUTOMATIC.
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
000840*                           ［ＲＬ＝  １２８］
000850 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
000860     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
000870*                           ［ＲＬ＝  ２５６］
000880 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
000890     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
000840*                           ［ＲＬ＝  １２８］
000850 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
000860     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
000600*                           ［ＲＬ＝  ８３２］
000610 FD  メモファイル        BLOCK CONTAINS 1     RECORDS.
000620     COPY MEMO           OF    XFDLIB JOINING メモ AS PREFIX.
000900*                           ［ＲＬ＝  ３２０］
000910 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
000920     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
001310*****************
001320* 作業ファイル１ *
001330*****************
001340*                         ［ＲＬ＝  ３２０］
001350 FD  作業ファイル１ RECORD  CONTAINS 320 CHARACTERS.
001360 01 作１－レコード.
001370    03 作１－レコードキー.
001535       05 作１－施術和暦年月日.
001536          07 作１－施術和暦               PIC 9.
001537          07 作１－施術年月.
001538             09 作１－施術年              PIC 9(2).
001539             09 作１－施術月              PIC 9(2).
001540          07 作１－施術日                 PIC 9(2).
001460       05 作１－患者コード.
001470          07 作１－患者番号                PIC 9(6).
001480          07 作１－枝番                    PIC X(1).
001490    03 作１－レコードデータ.
             05 作１－患者カナ                   PIC X(50).
             05 作１－患者氏名                   PIC X(50).
             05 作１－料金.
                07 作１－初検                    OCCURS 4.
                   09 作１－初検等               PIC N(1).
001547          07 作１－部位データ              OCCURS 4.
                   09 作１－整復                 PIC N(1).
                   09 作１－後療                 PIC N(1).
                   09 作１－罨法                 PIC N(1).
                   09 作１－電療                 PIC N(1).
                07 作１－金属                    PIC N(1).
                07 作１－運動                    PIC N(1).
001550          07 作１－初検金額                PIC 9(5).
001550          07 作１－整復金額                PIC 9(5).
001550          07 作１－後療金額                PIC 9(5).
001550          07 作１－罨法金額                PIC 9(5).
001550          07 作１－電療金額                PIC 9(5).
001551          07 作１－明細書加算              PIC 9(4).
001551          07 作１－一部負担金              PIC 9(5).
001551          07 作１－コメント                PIC X(100).
001500       05 FILLER                           PIC X(28).
000920*
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
             05 作２－初検計                     PIC 9(5).
001550       05 作２－整復計                     PIC 9(5).
001550       05 作２－後療計                     PIC 9(5).
001550       05 作２－罨法計                     PIC 9(5).
001550       05 作２－電療計                     PIC 9(5).
001551       05 作２－明細計                     PIC 9(4).
001551       05 作２－負担計                     PIC 9(5).
001500       05 FILLER                           PIC X(18).
001340*                         ［ＲＬ＝  ３２］
001350 FD  作業ファイル３ RECORD  CONTAINS 32 CHARACTERS.
001360 01 作３－レコード.
001370    03 作３－レコードキー.
001535       05 作３－施術和暦年月.
001536          07 作３－施術和暦                PIC 9.
001537          07 作３－施術年月.
001538             09 作３－施術年               PIC 9(2).
001539             09 作３－施術月               PIC 9(2).
001460       05 作３－患者コード.
001470          07 作３－患者番号                PIC 9(6).
001480          07 作３－枝番                    PIC X(1).
001490    03 作３－レコードデータ.
001420       05 作３－費用額                     PIC 9(6).
001420       05 作３－請求額                     PIC 9(6).
001420       05 作３－負担金                     PIC 9(5).
001500       05 FILLER                           PIC X(3).
001150*
001160 FD  印刷ファイル.
001170     COPY YJB7223P        OF  XMDLIB.
001180*----------------------------------------------------------------*
001190******************************************************************
001200*                WORKING-STORAGE SECTION                         *
001210******************************************************************
001220 WORKING-STORAGE         SECTION.
001230 01 キー入力                           PIC X     VALUE SPACE.
001240 01 状態キー                           PIC X(2)  VALUE SPACE.
001250 01 終了フラグ                         PIC X(3)  VALUE SPACE.
001260 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
001270 01 初検フラグ                         PIC X(3)  VALUE SPACE.
001280 01 ファイル名                         PIC N(6)  VALUE SPACE.
001290 01 レセプトＰＧＷ                     PIC X(8)  VALUE SPACE.
001300 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
001310 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
001330 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
001330 01 カウンタ                           PIC 9(2)  VALUE ZERO.
001330 01 カウンタ１                         PIC 9(2)  VALUE ZERO.
001340 01 患者番号Ｗ                         PIC 9(6)  VALUE ZERO.
001240 01 行カウンタ                         PIC 9(2) VALUE 0.
001240 01 頁カウンタ                         PIC 9(2) VALUE 0.
001260 01 最大行数                           PIC 9(2) VALUE 0.
001270 01 ヘッダ行数                         PIC 9(2) VALUE 0.
001280 01 移動行数Ｗ                         PIC 9(2) VALUE 0.
001580 01 オープンフラグ                     PIC X(3) VALUE SPACE.
001300 01 用紙種別Ｗ                         PIC 9     VALUE ZERO.
001350**
001360 01 遅延フラグ                         PIC X(3) VALUE SPACE.
001370 01 遅延回数Ｗ                         PIC 9(4) VALUE ZERO.
001380 01 遅延ＣＮＴ                         PIC 9(5) VALUE ZERO.
001390**
001360 01 合計Ｗ.
001380    03 初検料計Ｗ                      PIC 9(5) VALUE ZERO.
001390    03 整復料計Ｗ                      PIC 9(5) VALUE ZERO.
001400    03 後療料計Ｗ                      PIC 9(5) VALUE ZERO.
001400    03 罨法料計Ｗ                      PIC 9(5) VALUE ZERO.
001400    03 電療料計Ｗ                      PIC 9(5) VALUE ZERO.
001400    03 明細料計Ｗ                      PIC 9(5) VALUE ZERO.
001400    03 負担金計Ｗ                      PIC 9(5) VALUE ZERO.
001400*
001360 01 料金Ｗ.
001380    03 初検料Ｗ                        PIC 9(5) VALUE ZERO.
001390    03 整復料Ｗ                        PIC 9(5) VALUE ZERO.
001400    03 後療料Ｗ                        PIC 9(5) VALUE ZERO.
001400    03 罨法料Ｗ                        PIC 9(5) VALUE ZERO.
001400    03 電療料Ｗ                        PIC 9(5) VALUE ZERO.
001400    03 明細料Ｗ                        PIC 9(5) VALUE ZERO.
001400    03 負担金Ｗ                        PIC 9(5) VALUE ZERO.
      */改ページ不具合対応011221
      */次のレコードを読んだ後に前のレコードのヘッダを印刷する為のワーク。
      */１ページちょうどで改ページして２ページ目が合計行のみの印刷の場合に必要
       01 ヘッダ項目退避用ワーク.
         03 患者コードＨＷ.
           05 患者番号ＨＷ                   PIC 9(6)  VALUE ZERO.
           05 枝番ＨＷ                       PIC X(1)  VALUE SPACE.
         03 患者氏名ＨＷ                     PIC X(50) VALUE SPACE.
         03 保険種別ＨＷ                     PIC 9(2)  VALUE ZERO.
         03 公費種別ＨＷ                     PIC 9(2)  VALUE ZERO.
         03 助成種別ＨＷ                     PIC 9(2)  VALUE ZERO.
         03 本人家族区分ＨＷ                 PIC 9(1)  VALUE ZERO.
         03 負担割合ＨＷ                     PIC 9(2)  VALUE ZERO.
001410****************
001420* 連結項目待避 *
001430****************
001440*    ************
001450*    * 印刷キー *
001460*    ************
001470 01 対象データＷＲ.
001480    03 施術和暦年月ＷＲ.
001490       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
001500       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
001510       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
001520    03 開始日ＷＲ                       PIC 9(2)  VALUE ZERO.
001530    03 終了日ＷＲ                       PIC 9(2)  VALUE ZERO.
001540    03 患者カナＷＲ                     PIC X(20) VALUE SPACE.
001550    03 患者コードＷ.
001560       05 患者番号Ｗ                    PIC 9(6)  VALUE ZERO.
001570       05 枝番Ｗ                        PIC X(1)  VALUE SPACE.
001580    03 印刷モードＦＷＲ                 PIC 9(1)  VALUE ZERO.
001630    03 印字位置ＣＮＴ                   PIC 9(2)  VALUE ZERO.
001640**************
001650* 受診者情報 *
001660**************
001670 01 受診者情報Ｗ.
001680    03 施術年月Ｗ.
001690       05 施術和暦Ｗ                   PIC 9(1)   VALUE ZERO.
001690       05 施術年Ｗ                     PIC 9(2)   VALUE ZERO.
001700       05 施術月Ｗ                     PIC 9(2)   VALUE ZERO.
001710    03 保険者番号Ｗ.
001720       05 印刷保険者番号Ｗ             PIC X(6)   VALUE SPACE.
001730       05 FILLER                       PIC X(4)   VALUE SPACE.
001740    03 退職保険者番号Ｗ.
001750       05 FILLER                       PIC X(2)   VALUE SPACE.
001760       05 退職印刷保険者番号Ｗ         PIC X(6)   VALUE SPACE.
001770       05 FILLER                       PIC X(2)   VALUE SPACE.
001780    03 市町村番号Ｗ.
001790       05 印刷市町村番号Ｗ             PIC X(8)   VALUE SPACE.
001800       05 FILLER                       PIC X(2)   VALUE SPACE.
001810    03 保険種別Ｗ                      PIC 9(2)   VALUE ZERO.
001810    03 公費種別Ｗ                      PIC 9(2)   VALUE ZERO.
001810    03 助成種別Ｗ                      PIC 9(2)   VALUE ZERO.
001820    03 患者情報Ｗ.
001830       05 患者カナＷ                   PIC X(20)  VALUE SPACE.
001840       05 患者氏名Ｗ                   PIC N(10)  VALUE SPACE.
       01 患者氏名ＷＰ.
          03 患者名ＷＰ                      OCCURS 10. 
             05 患者名Ｗ                        PIC N(1)   VALUE SPACE.
       01 コメントＷ.
          03 コメント１Ｗ                       PIC X(50) VALUE SPACE.
          03 コメント２Ｗ                       PIC X(50) VALUE SPACE.
001831* POWER COBOL用
001832 01 dll-name  PIC X(260)  VALUE SPACE.
001833 01 form-name PIC X(14)   VALUE SPACE.
002210*******************************************************************
002220 01 印刷制御.
002230     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
002240     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
002250     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
002260     03 拡張制御Ｐ.
002270         05 端末制御Ｐ.
002280             07 移動方向Ｐ             PIC X(1) VALUE SPACE.
002290             07 移動行数Ｐ             PIC 9(3) VALUE ZERO.
002300         05 詳細制御Ｐ                 PIC X(2) VALUE SPACE.
002310     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
002320     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
002330*
002340 01 計算機西暦年Ｗ                     PIC 9(2) VALUE ZERO.
002350* 日付ＷＯＲＫ
002360 01 和暦終了年Ｗ                       PIC 9(4) VALUE ZERO.
002370 01 計算機西暦.
002380    03 計算機西暦年                    PIC 9(4) VALUE ZERO.
002390    03 計算機西暦月日                  PIC 9(4) VALUE ZERO.
002400 01 計算機西暦Ｒ REDEFINES 計算機西暦.
002410    03 計算機世紀                      PIC 9(2).
002420    03 計算機日付                      PIC 9(6).
002430    03 計算機日付Ｒ REDEFINES 計算機日付.
002440       05 計算機年月                   PIC 9(4).
002450       05 計算機年月Ｒ REDEFINES 計算機年月.
002460         07 計算機年                   PIC 9(2).
002470         07 計算機月                   PIC 9(2).
002480       05 計算機日                     PIC 9(2).
002490*
002500******************************************************************
002510*                          連結項目                              *
002520******************************************************************
002510**********************
002520* メッセージ表示キー *
002530**********************
002540 01 連メ－キー IS EXTERNAL.
002550    03  連メ－メッセージ                 PIC N(20).
003540*
003541 01 連メ５－キー IS EXTERNAL.
003542    03  連メ５－メッセージ               PIC N(20).
003543    03  連メ５－初期値                   PIC X.
003544    03  連メ５－返り値                   PIC X.
002560*
002870************
002880* 印刷キー *
002890************
002900 01 連印－対象データＹＪＢ７２０ IS EXTERNAL.
003120    03 連印－施術和暦年月日.
003130       05 連印－施術和暦                  PIC 9(1).
003140       05 連印－施術年                    PIC 9(2).
003150       05 連印－施術月                    PIC 9(2).
003150       05 連印－施術日                    PIC 9(2).
003190    03 連印－患者コード.
003200       05 連印－患者番号                  PIC 9(6).
003210       05 連印－枝番                      PIC X(1).
003020    03 連印－氏名モード                   PIC 9(1).
003020    03 連印－年月モード                   PIC 9(1).
003020    03 連印－合計モード                   PIC 9(1).
003020    03 連印－明細モード                   PIC 9(1).
003020    03 連印－コメントモード               PIC 9(1).
003020    03 連印－用紙種別                     PIC 9(1).
          03 連印－読み順                       PIC X(4).
003070    03 連印－段数                         PIC 9(2).
003070    03 連印－処理キー                     PIC X(4).
002810****************
002820* 画面入力情報 *
002830****************
003110 01 連入－入力データＹＪＢ７２０ IS EXTERNAL.
003120    03 連入－開始和暦年月日.
003130       05 連入－開始和暦                  PIC 9(1).
003140       05 連入－開始年                    PIC 9(2).
003150       05 連入－開始月                    PIC 9(2).
003150       05 連入－開始日                    PIC 9(2).
003120    03 連入－終了和暦年月日.
003130       05 連入－終了和暦                  PIC 9(1).
003140       05 連入－終了年                    PIC 9(2).
003150       05 連入－終了月                    PIC 9(2).
003150       05 連入－終了日                    PIC 9(2).
003170    03 連入－保険種別                     PIC 9(2).
003180    03 連入－本人家族区分                 PIC 9(1).
003190    03 連入－患者コード.
003200       05 連入－患者番号                  PIC 9(6).
003210       05 連入－枝番                      PIC X(1).
          03 連入－印刷条件                     PIC 9(2).
003120    03 連入－レセ合計.
             05 連入－レセプト                OCCURS 5.
      */金額行の印字に年を入力出来るように変更(和暦は開始和暦を流用)
                07 連入－合計和暦年月.
                   09 連入－合計和暦            PIC 9.
                   09 連入－合計年              PIC 9(2).
                   09 連入－合計月              PIC 9(2).
003120          07 連入－請求年月日.
003140             09 連入－請求年              PIC 9(2).
003150             09 連入－請求月              PIC 9(2).
003150             09 連入－請求日              PIC 9(2).
003120          07 連入－領収年月日.
003140             09 連入－領収年              PIC 9(2).
003150             09 連入－領収月              PIC 9(2).
003150             09 連入－領収日              PIC 9(2).
003020*
       01 連入－表示フラグＹＪＢ７２０ IS EXTERNAL GLOBAL.
          03 連入－プレビュー区分               PIC 9(1).
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
007308*
       01 連メッセージＰ情報００５１ IS EXTERNAL.
          03 連メＰ－メッセージ番号                PIC 9(2).
          03 連メＰ－メッセージ.
             05 連メＰ－メッセージ内容             PIC X(40) OCCURS 6.
          03 連メＰ－メッセージ１                  PIC X(20).
          03 連メＰ－メッセージ２                  PIC X(12).
          03 連メＰ－返り値                        PIC X.
003090******************************************************************
003100*                      PROCEDURE  DIVISION                       *
003110******************************************************************
003120 PROCEDURE               DIVISION.
003130************
003140*           *
003150* 初期処理   *
003160*           *
003170************
002570     PERFORM プリンタファイル作成.
003180     PERFORM 初期化.
003190     PERFORM 制御情報取得.
003200************
003210*           *
003220* 主処理     *
003230*           *
003240************
003250* 印刷
           IF ( 連印－明細モード = 1 ) OR ( 連印－コメントモード = 1 )
003290         PERFORM 印刷セット１
           ELSE
               IF 連印－合計モード = 1
                   PERFORM 印刷セット２
               ELSE
                   IF ( 連印－氏名モード = 1 ) OR ( 連印－年月モード = 1 )
002990                 PERFORM 作業ファイル１読込
004650                 PERFORM ヘッダセット
                       IF 連入－レセ合計 NOT = ZERO
                           PERFORM レセ合計セット
                           PERFORM レセ合計印刷
                       END-IF
                   ELSE
                       IF 連入－レセ合計 NOT = ZERO
                           PERFORM レセ合計セット
                           PERFORM レセ合計印刷
                       END-IF
                   END-IF
               END-IF
           END-IF.
003350************
003360*           *
003370* 終了処理   *
003380*           *
003390************
003400     PERFORM 終了処理.
003410     PERFORM 遅延処理.
003420     EXIT PROGRAM.
003430*
003440*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YJB7223"             TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分 TO Ｈ連ＰＲＴＦ－プレビュー区分.
003000*     MOVE 1 TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
003450*================================================================*
003460 初期化 SECTION.
003470*
003480     PERFORM ファイルオープン.
003490*    /* 現在日付取得 */
003500     ACCEPT 計算機日付 FROM DATE.
003510*    /* 1980～2079年の間で設定 */
003520     IF 計算機年 > 80
003530         MOVE 19 TO 計算機世紀
003540     ELSE
003550         MOVE 20 TO 計算機世紀
003560     END-IF.
003570     PERFORM カレント元号取得.
003580     PERFORM 和暦終了年取得.
003590     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
003600*================================================================*
003610 カレント元号取得 SECTION.
003620*
003630     MOVE ZEROS TO 制－制御区分.
003640     READ 制御情報マスタ
003650     NOT INVALID KEY
003660         MOVE 制－カレント元号 TO カレント元号Ｗ
003670     END-READ.
003680*
003690*================================================================*
003700 和暦終了年取得 SECTION.
003710*
003720*     DISPLAY NC"カレント元号Ｗ"  カレント元号Ｗ UPON MSGBOX.
003730     MOVE カレント元号Ｗ TO 元－元号区分.
003740     READ 元号マスタ
003750     INVALID KEY
003760         DISPLAY NC"指定和暦が登録されていません" UPON CONS
003770         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
003780                                                  UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
003790         ACCEPT  キー入力 FROM CONS
003800         PERFORM 終了処理
003810         EXIT PROGRAM
003820     NOT INVALID KEY
003830         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
003840         MOVE 前和暦Ｗ TO 元－元号区分
003850         READ 元号マスタ
003860         INVALID KEY
003870             DISPLAY NC"指定和暦が登録されていません" UPON CONS
003880             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
003890                                                      UPON CONS
000080*-----------------------------------------*
000090             CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
003900             ACCEPT  キー入力 FROM CONS
003910             PERFORM 終了処理
003920             EXIT PROGRAM
003930         NOT INVALID KEY
003940             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
003950         END-READ
003960     END-READ.
003970*
003980*================================================================*
003990 ファイルオープン SECTION.
004000*
004010     OPEN INPUT   元号マスタ
004020         MOVE NC"元号" TO ファイル名.
004030         PERFORM オープンチェック.
004040     OPEN INPUT   制御情報マスタ
004050         MOVE NC"制御情報" TO ファイル名.
004060         PERFORM オープンチェック.
004800     OPEN INPUT 名称マスタ.
004810         MOVE NC"名称" TO ファイル名.
004820         PERFORM オープンチェック.
002780     OPEN INPUT メモファイル.
002790         MOVE NC"メモ" TO ファイル名.
002800         PERFORM オープンチェック.
004070     OPEN INPUT   受診者情報Ｆ.
004080         MOVE NC"受情" TO ファイル名.
004090         PERFORM オープンチェック.
004100     OPEN INPUT 作業ファイル１
004110         MOVE NC"作業１" TO ファイル名.
004120         PERFORM オープンチェック.
004100     OPEN INPUT 作業ファイル２
004110         MOVE NC"作業２" TO ファイル名.
004120         PERFORM オープンチェック.
004100     OPEN INPUT 作業ファイル３
004110         MOVE NC"作業３" TO ファイル名.
004120         PERFORM オープンチェック.
004130*     OPEN I-O   印刷ファイル
004140*         PERFORM エラー処理Ｐ.
004150*================================================================*
004160 オープンチェック SECTION.
004170*
004180     IF 状態キー  NOT =  "00"
004190         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
004200         DISPLAY NC"状態キー：" 状態キー         UPON CONS
004210         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
004220                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
004230         ACCEPT  キー入力 FROM CONS
004240         PERFORM ファイル閉鎖
004250         EXIT PROGRAM.
004260*================================================================*
004270 制御情報取得 SECTION.
004280*
004290     MOVE ZERO TO 制－制御区分
004300     READ 制御情報マスタ
004310     NOT INVALID KEY
004320         MOVE 制－遅延回数       TO 遅延回数Ｗ
004320         MOVE 制－カルテ用紙種別 TO 用紙種別Ｗ
004330     END-READ.
004340*
004350*================================================================*
004360 遅延処理 SECTION.
004370*
004380     PERFORM VARYING 遅延ＣＮＴ FROM 1 BY 1
004390                                UNTIL 遅延ＣＮＴ > 遅延回数Ｗ
004400         MOVE SPACE TO 遅延フラグ
004410     END-PERFORM.
004420*
004590*================================================================*
004600 印刷セット１ SECTION.
004610*
002940     MOVE SPACE TO 終了フラグ.
002950     MOVE ZERO  TO 行カウンタ.
           MOVE 1     TO 頁カウンタ.
002980     PERFORM 合計値初期化.
           MOVE 連印－患者コード  TO 作１－患者コード.
           MOVE ZERO              TO 作１－施術和暦.
           MOVE ZERO              TO 作１－施術年.
           MOVE ZERO              TO 作１－施術月.
           START 作業ファイル１ KEY IS > 作１－患者コード
                                         作１－施術和暦年月日
           END-START.
013500     IF 状態キー  =  "00"
013510         MOVE SPACE  TO 終了フラグ
013520         PERFORM 作業ファイル１読込
003000         MOVE 作１－患者コード   TO 患者コードＷ
               MOVE 作１－施術和暦     TO 施術和暦Ｗ
               MOVE 作１－施術年       TO 施術年Ｗ
               MOVE 作１－施術月       TO 施術月Ｗ
      */改ページ不具合対応011221
      */次のレコードを読んでから前のヘッダを印刷する必要がある場合の為に退避しておく。(改ページ後の合計行)
               MOVE 作１－患者氏名      TO 患者氏名ＨＷ    
      *
               IF 連印－段数 = ZERO
                   IF 連印－用紙種別 = 1
                       MOVE 2              TO 行カウンタ
                   ELSE
                       MOVE 1              TO 行カウンタ
                   END-IF
               ELSE
                   IF 連印－用紙種別 = 1
                       COMPUTE 行カウンタ = 連印－段数 + 1
                   ELSE
                       MOVE 連印－段数     TO 行カウンタ
                   END-IF
               END-IF
003070         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
003090                       ( 作１－患者コード NOT = 患者コードＷ )
                   INITIALIZE YJB7223P
                   MOVE SPACE TO YJB7223P
004640             IF ( 連印－氏名モード = 1 ) OR ( 連印－年月モード = 1 )
004650                 PERFORM ヘッダセット
004660             END-IF
004380             PERFORM VARYING カウンタ FROM 行カウンタ BY 1
003080                     UNTIL ( カウンタ > 31 ) OR
003090                           ( 作１－患者コード NOT = 患者コードＷ ) OR
                                 ( 終了フラグ = "YES" )
004380                 IF ( 作１－施術年 NOT = 施術年Ｗ ) OR
                          ( 作１－施術月 NOT = 施術月Ｗ )
004640*                     IF 連印－コメントモード = 1
      */総評の印刷はコメントの印刷があり、かつ、合計の印刷もある時のみに変更。020108
                           IF (連印－コメントモード = 1) AND
                              (連印－合計モード     = 1)
      */                      */総評コメントの転記
003130                         PERFORM 総評コメント印刷処理
                           END-IF
                           IF 連印－合計モード = 1
      */                      */合計行情報の転記
003130                         PERFORM 合計印刷処理
                               IF カウンタ > 31
      */                          */明細の印刷
                                   PERFORM 明細印刷
      */                          */改頁処理
003650                             PERFORM 改頁処理
      *
005830                             MOVE SPACE  TO  連メ５－キー
005840                             MOVE NC"施術明細専用紙をセットしてください" TO 連メ５－メッセージ
005850*                             CALL   "MSG005"
005860*                             CANCEL "MSG005"
      */メッセージボックスの変更/
                                   MOVE 1                  TO 連メＰ－メッセージ番号
                                   MOVE 連メ５－メッセージ TO 連メＰ－メッセージ
                                   MOVE "PMSG0051.DLL"     TO dll-name
                                   MOVE "PMSG0051"         TO form-name
                                   CALL "POWEROPENSHEET" USING dll-name form-name
                                   MOVE  連メＰ－返り値    TO 連メ５－返り値
005870                             EVALUATE  連メ５－返り値
005871                             WHEN "Y"
                                       INITIALIZE YJB7223P
                                       MOVE SPACE TO YJB7223P
004640                                 IF ( 連印－氏名モード = 1 ) OR ( 連印－年月モード = 1 )
      */                              */ヘッダをセットして印刷する。
004650                                     PERFORM ヘッダセット
004660                                 END-IF
                                       MOVE 2              TO 連印－用紙種別
                                       MOVE 1              TO カウンタ
005931                             WHEN OTHER
                                       MOVE 1              TO  印字位置ＣＮＴ
                                       PERFORM 印刷行数退避
005932                                 PERFORM 終了処理
005933                                 PERFORM 遅延処理
005934                                 EXIT PROGRAM
005935                             END-EVALUATE
                               END-IF
                           END-IF
      */                  */合計ワークの初期化。
                           PERFORM 合計値初期化
                       END-IF
004640                 IF 連印－明細モード = 1
      */                  */明細行の転記
                           PERFORM 明細印刷処理
                       END-IF
004640                 IF 連印－コメントモード = 1
      */                  */コメント行の転記
                           PERFORM コメント印刷処理
                       END-IF
003590                 MOVE 作１－患者コード   TO 患者コードＷ
003590                 MOVE 作１－施術和暦     TO 施術和暦Ｗ
003590                 MOVE 作１－施術年       TO 施術年Ｗ
003590                 MOVE 作１－施術月       TO 施術月Ｗ
      */改ページ不具合対応011221
      */次のレコードを読んでから前のヘッダを印刷する必要がある場合の為に退避しておく。(改ページ後の合計行)
                       MOVE 作１－患者氏名      TO 患者氏名ＨＷ    
      *
003600                 PERFORM 作業ファイル１読込
003610             END-PERFORM
                   IF カウンタ > 31
      */              */明細の印刷
                       PERFORM 明細印刷
                       IF ( 連入－レセ合計 NOT = ZERO ) AND ( 頁カウンタ = 1 )
      */                  */合計行のセット
                           PERFORM レセ合計セット
      */                  */合計行の印刷
                           PERFORM レセ合計印刷
                       END-IF
      */              */改頁処理
003650                 PERFORM 改頁処理
                       MOVE 2              TO 連印－用紙種別
                       COMPUTE 頁カウンタ = 頁カウンタ + 1
                       MOVE 1              TO 行カウンタ
                       MOVE 1  TO 印字位置ＣＮＴ
      */１人の患者で改頁の必要がある場合次の用紙をセットさせる旨のメッセージを表示する。020108
      *                 IF 終了フラグ = "YES"
                       IF ( 終了フラグ = "YES") OR               
                          ( 作１－患者コード NOT = 患者コードＷ )
                           IF 連印－合計モード = 1
005830                         MOVE SPACE  TO  連メ５－キー
005840                         MOVE NC"施術明細専用紙をセットしてください" TO 連メ５－メッセージ
005850*                         CALL   "MSG005"
005860*                         CANCEL "MSG005"
      */メッセージボックスの変更/
                               MOVE 1                  TO 連メＰ－メッセージ番号
                               MOVE 連メ５－メッセージ TO 連メＰ－メッセージ
                               MOVE "PMSG0051.DLL"     TO dll-name
                               MOVE "PMSG0051"         TO form-name
                               CALL "POWEROPENSHEET" USING dll-name form-name
                               MOVE  連メＰ－返り値    TO 連メ５－返り値
005870                         IF 連メ５－返り値 NOT = "Y"
                                   MOVE 1              TO  印字位置ＣＮＴ
                                   PERFORM 印刷行数退避
005932                             PERFORM 終了処理
005933                             PERFORM 遅延処理
005934                             EXIT PROGRAM
                               END-IF
                               MOVE 1              TO カウンタ
                               INITIALIZE YJB7223P
                               MOVE SPACE TO YJB7223P
                               IF ( 連印－氏名モード = 1 ) OR ( 連印－年月モード = 1 )
                                       PERFORM ヘッダセット２
                               END-IF
      */総評の印刷はコメントの印刷があり、かつ、合計の印刷もある時のみに変更。020108
                               IF (連印－コメントモード = 1) AND
                                  (連印－合計モード     = 1)
      */                  */総評コメントの転記
                                   PERFORM 総評コメント印刷処理
                               END-IF
                               IF 連印－合計モード = 1
      */                  */合計行情報の転記
                                   PERFORM 合計印刷処理
                               END-IF
      */              */合計ワークの初期化。
                               PERFORM 合計値初期化
      */              */明細の印刷
                               PERFORM 明細印刷
                               IF ( 連入－レセ合計 NOT = ZERO ) AND ( 頁カウンタ = 1 )
      */                  */合計行のセット
                                   PERFORM レセ合計セット
      */                  */合計行の印刷
                                   PERFORM レセ合計印刷
                               END-IF
                           END-IF
                       ELSE
      *                     IF (連印－合計モード = 1) 
005830                         MOVE SPACE  TO  連メ５－キー
005840                         MOVE NC"施術明細専用紙をセットしてください" TO 連メ５－メッセージ
005850*                         CALL   "MSG005"
005860*                         CANCEL "MSG005"
      */メッセージボックスの変更/
                               MOVE 1                  TO 連メＰ－メッセージ番号
                               MOVE 連メ５－メッセージ TO 連メＰ－メッセージ
                               MOVE "PMSG0051.DLL"     TO dll-name
                               MOVE "PMSG0051"         TO form-name
                               CALL "POWEROPENSHEET" USING dll-name form-name
                               MOVE  連メＰ－返り値    TO 連メ５－返り値
005870                         IF 連メ５－返り値 NOT = "Y"
                                   MOVE 1              TO  印字位置ＣＮＴ
                                   PERFORM 印刷行数退避
005932                             PERFORM 終了処理
005933                             PERFORM 遅延処理
005934                             EXIT PROGRAM
                               END-IF
                               MOVE 2              TO 連印－用紙種別
      *                     END-IF
                       END-IF
                   ELSE
      *                 IF 連印－コメントモード = 1
      */総評の印刷はコメントの印刷があり、かつ、合計の印刷もある時のみに変更。020108
                       IF (連印－コメントモード = 1) AND
                          (連印－合計モード     = 1)
      */                  */総評コメントの転記
                           PERFORM 総評コメント印刷処理
                       END-IF
                       IF 連印－合計モード = 1
      */                  */合計行情報の転記
                           PERFORM 合計印刷処理
                       END-IF
      */              */合計ワークの初期化。
                       PERFORM 合計値初期化
      */              */明細の印刷
                       PERFORM 明細印刷
                       IF ( 連入－レセ合計 NOT = ZERO ) AND ( 頁カウンタ = 1 )
      */                  */合計行のセット
                           PERFORM レセ合計セット
      */                  */合計行の印刷
                           PERFORM レセ合計印刷
                       END-IF
      */              */改頁処理
                       PERFORM 改頁処理
                       IF カウンタ > 31
                           MOVE 1  TO 印字位置ＣＮＴ
                           MOVE 2  TO 連印－用紙種別
                       ELSE
                           IF 連印－用紙種別 = 1
                               COMPUTE 印字位置ＣＮＴ = カウンタ - 1
                           ELSE
                               MOVE カウンタ   TO 印字位置ＣＮＴ
                           END-IF
                       END-IF
                   END-IF
003620         END-PERFORM
               PERFORM 印刷行数退避
           END-IF.
004590*================================================================*
004600 印刷セット２ SECTION.
004610*
002940     MOVE SPACE TO 終了フラグ.
002950     MOVE ZERO  TO 行カウンタ.
002950     MOVE 1     TO 頁カウンタ.
002980     PERFORM 合計値初期化.
           MOVE 連印－患者コード  TO 作１－患者コード.
           MOVE ZERO              TO 作１－施術和暦.
           MOVE ZERO              TO 作１－施術年.
           MOVE ZERO              TO 作１－施術月.
           START 作業ファイル１ KEY IS > 作１－患者コード
                                         作１－施術和暦年月日
           END-START.
013500     IF 状態キー  =  "00"
013510         MOVE SPACE  TO 終了フラグ
013520         PERFORM 作業ファイル１読込
003000         MOVE 作１－患者コード   TO 患者コードＷ
               MOVE 作１－施術和暦     TO 施術和暦Ｗ
               MOVE 作１－施術年       TO 施術年Ｗ
               MOVE 作１－施術月       TO 施術月Ｗ
               MOVE 連印－段数         TO カウンタ
004640         IF ( 連印－氏名モード = 1 ) OR ( 連印－年月モード = 1 )
004650             PERFORM ヘッダセット
004660         END-IF
               PERFORM UNTIL ( 作１－患者コード NOT = 患者コードＷ ) OR
                             ( 終了フラグ = "YES" )
                   IF カウンタ > 31
                       PERFORM 明細印刷
                       IF ( 連入－レセ合計 NOT = ZERO ) AND ( 頁カウンタ = 1 )
                           PERFORM レセ合計セット
                           PERFORM レセ合計印刷
                       END-IF
003650                 PERFORM 改頁処理
                       COMPUTE 頁カウンタ = 頁カウンタ + 1
                       MOVE 2              TO 連印－用紙種別
      *
                       INITIALIZE YJB7223P
                       MOVE SPACE TO YJB7223P
004640                 IF ( 連印－氏名モード = 1 ) OR ( 連印－年月モード = 1 )
004650                     PERFORM ヘッダセット
004660                 END-IF
                       MOVE 1  TO カウンタ
                   END-IF
004380             IF ( 作１－施術和暦 NOT = 施術和暦Ｗ ) OR
                      ( 作１－施術年 NOT = 施術年Ｗ ) OR
                      ( 作１－施術月 NOT = 施術月Ｗ )
003130                 PERFORM 合計印刷処理
                       PERFORM 合計値初期化
                   END-IF
003590             MOVE 作１－患者コード   TO 患者コードＷ
003590             MOVE 作１－施術和暦     TO 施術和暦Ｗ
003590             MOVE 作１－施術年       TO 施術年Ｗ
003590             MOVE 作１－施術月       TO 施術月Ｗ
003600             PERFORM 作業ファイル１読込
003610         END-PERFORM
               PERFORM 合計印刷処理
               PERFORM 明細印刷
               IF ( 連入－レセ合計 NOT = ZERO ) AND ( 頁カウンタ = 1 )
                   PERFORM レセ合計セット
                   PERFORM レセ合計印刷
               END-IF
               PERFORM 改頁処理
               PERFORM 合計値初期化
               IF カウンタ > 31
                   MOVE 1          TO  印字位置ＣＮＴ
               ELSE
                   IF 連印－用紙種別 = 1
                       COMPUTE 印字位置ＣＮＴ = カウンタ - 1
                   ELSE
                       MOVE カウンタ   TO  印字位置ＣＮＴ
                   END-IF
               END-IF
004490         PERFORM 印刷行数退避
           END-IF.
004900*================================================================*
004910 ヘッダセット SECTION.
004920*
           IF 連印－氏名モード = 1
               MOVE 作１－患者氏名      TO 患者氏名
           END-IF.
           IF 連印－年月モード = 1
               IF 用紙種別Ｗ = ZERO OR 1
                   MOVE 作１－施術年        TO 施術年
                   MOVE 作１－施術月        TO 施術月
               ELSE
                   MOVE 作１－施術年        TO 施術年２
                   MOVE 作１－施術月        TO 施術月２
               END-IF
           END-IF.
           PERFORM ヘッダ印刷.
004900*================================================================*
004910 明細印刷処理 SECTION.
004920*
005510     MOVE 作１－施術月       TO 月(カウンタ).
005520     MOVE 作１－施術日       TO 日(カウンタ).
004940********************
004950* 料金データセット *
004960********************
005010     MOVE 作１－初検金額     TO  初検料等(カウンタ).
           MOVE 作１－初検等(1)    TO  初検等１(カウンタ).
           MOVE 作１－初検等(2)    TO  初検等２(カウンタ).
           MOVE 作１－初検等(3)    TO  初検等３(カウンタ).
           MOVE 作１－初検等(4)    TO  初検等４(カウンタ).
005010     MOVE 作１－整復金額     TO  整復料(カウンタ).
           MOVE 作１－整復(1)      TO  整復等１(カウンタ).
           MOVE 作１－整復(2)      TO  整復等２(カウンタ).
           MOVE 作１－整復(3)      TO  整復等３(カウンタ).
           MOVE 作１－整復(4)      TO  整復等４(カウンタ).
           MOVE 作１－金属         TO  金属(カウンタ).
           MOVE 作１－運動         TO  運動(カウンタ).
005010     MOVE 作１－後療金額     TO  後療料(カウンタ).
005240     MOVE 作１－後療(1)      TO  後療１(カウンタ).
005240     MOVE 作１－後療(2)      TO  後療２(カウンタ).
005240     MOVE 作１－後療(3)      TO  後療３(カウンタ).
005240     MOVE 作１－後療(4)      TO  後療４(カウンタ).
005010     MOVE 作１－罨法金額     TO  罨法料(カウンタ).
005240     MOVE 作１－罨法(1)      TO  罨法１(カウンタ).
005240     MOVE 作１－罨法(2)      TO  罨法２(カウンタ).
005240     MOVE 作１－罨法(3)      TO  罨法３(カウンタ).
005240     MOVE 作１－罨法(4)      TO  罨法４(カウンタ).
005010     MOVE 作１－電療金額     TO  電療料(カウンタ).
005240     MOVE 作１－電療(1)      TO  電療１(カウンタ).
005240     MOVE 作１－電療(2)      TO  電療２(カウンタ).
005240     MOVE 作１－電療(3)      TO  電療３(カウンタ).
005240     MOVE 作１－電療(4)      TO  電療４(カウンタ).
005010     MOVE 作１－明細書加算   TO  明細書発行加算(カウンタ).
005010     MOVE 作１－一部負担金   TO  一部負担金(カウンタ).
005530*================================================================*
005540 コメント印刷処理 SECTION.
005550*
           MOVE 作１－コメント           TO  コメントＷ.
           MOVE コメント１Ｗ             TO  コメント１(カウンタ).
           MOVE コメント２Ｗ             TO  コメント２(カウンタ).
005300*
005730*================================================================*
005740 合計値初期化 SECTION.
005750*
005760     MOVE ZERO  TO 合計Ｗ.
004900*================================================================*
004910 合計印刷処理 SECTION.
004920*
           MOVE 施術和暦Ｗ   TO 作２－施術和暦.
           MOVE 施術年Ｗ     TO 作２－施術年.
           MOVE 施術月Ｗ     TO 作２－施術月.
           MOVE 患者コードＷ TO 作２－患者コード.
           READ 作業ファイル２
           NOT INVALID KEY
               MOVE NC"合計"     TO 合計(カウンタ)
               MOVE 作２－初検計 TO 初検料等(カウンタ)
               MOVE 作２－整復計 TO 整復料(カウンタ)
               MOVE 作２－後療計 TO 後療料(カウンタ)
               MOVE 作２－罨法計 TO 罨法料(カウンタ)
               MOVE 作２－電療計 TO 電療料(カウンタ)
               MOVE 作２－明細計 TO 明細書発行加算(カウンタ)
005010         MOVE 作２－負担計 TO 一部負担金(カウンタ)
           END-READ.
           COMPUTE カウンタ = カウンタ + 1.
004900*================================================================*
004910 レセ合計セット SECTION.
004920*
           PERFORM VARYING カウンタ１ FROM 1 BY 1
                   UNTIL カウンタ１ > 5
      */金額行の印字に年を入力出来るように変更(和暦は開始和暦を流用)
      *         MOVE 施術和暦Ｗ                TO 作３－施術和暦
      *         MOVE 施術年Ｗ                  TO 作３－施術年
               MOVE 連入－合計和暦(カウンタ１) TO 作３－施術和暦
               MOVE 連入－合計年  (カウンタ１) TO 作３－施術年
               MOVE 連入－合計月  (カウンタ１) TO 作３－施術月
               MOVE 連入－患者コード           TO 作３－患者コード
               READ 作業ファイル３
               NOT INVALID KEY
                   MOVE 作３－施術月        TO 合計月(カウンタ１)
                   MOVE 作３－費用額        TO 合計金額(カウンタ１)
                   MOVE 作３－負担金        TO 負担合計(カウンタ１)
                   MOVE 作３－請求額        TO 請求合計(カウンタ１)
      */当月に請求が無い場合は印字しないように変更
                   MOVE 連入－請求年(カウンタ１)  TO 請求年(カウンタ１)
                   MOVE 連入－請求月(カウンタ１)  TO 請求月(カウンタ１)
                   MOVE 連入－請求日(カウンタ１)  TO 請求日(カウンタ１)
                   MOVE 連入－領収年(カウンタ１)  TO 領収年(カウンタ１)
                   MOVE 連入－領収月(カウンタ１)  TO 領収月(カウンタ１)
                   MOVE 連入－領収日(カウンタ１)  TO 領収日(カウンタ１)
               END-READ
           END-PERFORM.
004900*================================================================*
004910 総評コメント印刷処理 SECTION.
004920*
008280     MOVE 施術和暦Ｗ TO メモ－施術和暦.
008290     MOVE 施術年Ｗ   TO メモ－施術年.
008300     MOVE 施術月Ｗ   TO メモ－施術月.
008310     MOVE 99         TO メモ－施術日.
           MOVE 1          TO メモ－制御区分.
004130     MOVE 患者コードＷ  TO メモ－患者コード.
           READ メモファイル
           NOT INVALID KEY
               MOVE メモ－施術コメント TO コメントＷ
               MOVE コメント１Ｗ   TO  コメント１(カウンタ)
               MOVE コメント２Ｗ   TO  コメント２(カウンタ)
               IF 連印－合計モード NOT = 1
                   COMPUTE カウンタ = カウンタ + 1
               END-IF
           END-READ.
007070*================================================================*
007080 作業ファイル１読込 SECTION.
007090*
007100     READ 作業ファイル１ NEXT
007110     AT END
007120         MOVE "YES" TO 終了フラグ
007130     END-READ.
007140*
007150*================================================================*
007160 ヘッダ印刷 SECTION.
007170*
004310     IF ( オープンフラグ NOT = "YES" )
004320        MOVE "YES" TO オープンフラグ
004330        OPEN I-O  印刷ファイル
004340        PERFORM エラー処理Ｐ
004350     END-IF.
013440*
007180     MOVE "YJB7223P"  TO  定義体名Ｐ.
007190     MOVE "HEDDA"   TO  項目群名Ｐ.
003770     MOVE SPACE      TO  処理種別Ｐ.
007200     WRITE YJB7223P.
007210     PERFORM エラー処理Ｐ.
007150*================================================================*
007160 明細印刷 SECTION.
007170*
004310     IF ( オープンフラグ NOT = "YES" )
004320        MOVE "YES" TO オープンフラグ
004330        OPEN I-O  印刷ファイル
004340        PERFORM エラー処理Ｐ
004350     END-IF.
013440*
007180     MOVE "YJB7223P"  TO  定義体名Ｐ.
007190     MOVE "MEISAI"   TO  項目群名Ｐ.
003770     MOVE SPACE      TO  処理種別Ｐ.
007200     WRITE YJB7223P.
007210     PERFORM エラー処理Ｐ.
007150*================================================================*
007160 レセ合計印刷 SECTION.
007170*
004310     IF ( オープンフラグ NOT = "YES" )
004320        MOVE "YES" TO オープンフラグ
004330        OPEN I-O  印刷ファイル
004340        PERFORM エラー処理Ｐ
004350     END-IF.
013440*
007180     MOVE "YJB7223P"  TO  定義体名Ｐ.
007190     MOVE "GOKEI"   TO  項目群名Ｐ.
003770     MOVE SPACE     TO  処理種別Ｐ.
007200     WRITE YJB7223P.
007210     PERFORM エラー処理Ｐ.
007170*
003730*================================================================*
003740 改頁処理  SECTION.
003750*
003760     MOVE "YJB7223P" TO  定義体名Ｐ.
003770     MOVE "CT"      TO  処理種別Ｐ.
003780     MOVE "PAGE"    TO  拡張制御Ｐ.
003790     MOVE SPACE     TO  項目群名Ｐ.
003800     WRITE YJB7223P.
003810     PERFORM エラー処理Ｐ.
003820     MOVE SPACE     TO  拡張制御Ｐ.
003821*
003822     CLOSE  印刷ファイル.
004320     MOVE SPACE TO オープンフラグ.
003823*     OPEN I-O   印刷ファイル.
003824*     PERFORM エラー処理Ｐ.
003825*
007610*================================================================*
007620 エラー表示 SECTION.
007630*
007640     DISPLAY ファイル名   NC"ファイル書込エラー"   UPON CONS.
007650     DISPLAY NC"状態キー：" 状態キー               UPON CONS.
007660     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
007670     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
007680     ACCEPT  キー入力 FROM CONS.
007690     PERFORM ファイル閉鎖.
007700     EXIT PROGRAM.
007290*================================================================*
007300 エラー処理Ｐ SECTION.
007310*
007320     IF 通知情報Ｐ NOT = "00"
007330         DISPLAY NC"帳票エラー"              UPON CONS
007340         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
007350         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
007360         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
007370         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
007380                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
007390         ACCEPT  キー入力 FROM CONS
007400         PERFORM ファイル閉鎖
007410         MOVE 99 TO PROGRAM-STATUS
007420         EXIT PROGRAM
007430     END-IF.
007440*================================================================*
007450 ファイル閉鎖 SECTION.
007460*
002990     IF ( オープンフラグ = "YES" )
002991         CLOSE 印刷ファイル
003041     END-IF.
007480     CLOSE 元号マスタ   制御情報マスタ 名称マスタ
                 メモファイル 受診者情報Ｆ   作業ファイル１
                 作業ファイル２ 作業ファイル３.
007490*================================================================*
007500 終了処理 SECTION.
007510*
007520     PERFORM ファイル閉鎖.
006850*================================================================*
006860 印刷行数退避 SECTION.
006870     CLOSE 受診者情報Ｆ.
006880     PERFORM 遅延処理.
006890     OPEN I-O 受診者情報Ｆ.
006900         MOVE NC"受診" TO ファイル名.
006910         PERFORM オープンチェック.
006920     PERFORM 遅延処理.
006930*
006940     MOVE ZERO            TO 受－施術和暦.
006950     MOVE ZERO            TO 受－施術年.
006960     MOVE ZERO            TO 受－施術月.
006970     MOVE 患者コードＷ    TO 受－患者コード.
006990*
007000     READ 受診者情報Ｆ
007010     NOT INVALID KEY
               IF 連印－用紙種別 = 1
007020             MOVE 印字位置ＣＮＴ TO 受－カルテ裏印刷行数
               ELSE
007020             COMPUTE 受－カルテ裏印刷行数 = 印字位置ＣＮＴ + 30
               END-IF
007030         REWRITE 受－レコード
007040         INVALID KEY
007050             MOVE NC"受Ｆ" TO ファイル名
007060             PERFORM エラー表示
007070         END-REWRITE
007080     END-READ.
007090     PERFORM 遅延処理.
007100     CLOSE 受診者情報Ｆ.
007110     PERFORM 遅延処理.
007120     OPEN INPUT 受診者情報Ｆ.
007130         MOVE NC"受診" TO ファイル名.
007140         PERFORM オープンチェック.
007530*================================================================*
004910 ヘッダセット２ SECTION.
004920*
           IF 連印－氏名モード = 1
               MOVE 患者氏名ＨＷ TO 患者氏名
           END-IF.
           IF 連印－年月モード = 1
               MOVE 作１－施術年 TO 施術年
               MOVE 作１－施術月 TO 施術月
           END-IF.
           PERFORM ヘッダ印刷.
004900*================================================================*
007540******************************************************************
007550 END PROGRAM YJB7223.
007560******************************************************************
