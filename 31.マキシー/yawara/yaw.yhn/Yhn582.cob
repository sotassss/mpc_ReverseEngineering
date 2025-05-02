000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHN582.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*       提出用並び替え一覧 【印刷】
000100*       MED = YHN580 YHN582P 
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2014-09-05
000130 DATE-COMPILED.          2014-09-05
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
000251     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000252                             ORGANIZATION             IS  INDEXED
000253                             ACCESS MODE              IS  DYNAMIC
000254                             RECORD KEY               IS 受－施術和暦年月
000255                                                          受－患者コード
000256                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000257                                                          受－患者カナ
000258                                                          受－患者コード
000259                             ALTERNATE RECORD KEY     IS  受－患者コード
000260                                                         受－施術和暦年月
000261                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000262                                                          受－保険種別
000263                                                          受－保険者番号
000264                                                          受－患者コード
000265                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000266                                                          受－公費種別
000267                                                     受－費用負担者番号
000268                                                          受－患者コード
000269                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000270                                                          受－助成種別
000271                                                  受－費用負担者番号助成
000272                                                          受－患者コード
000273                             ALTERNATE RECORD KEY  IS 受－請求和暦年月
000274                                                      受－施術和暦年月
000275                                                      受－患者コード
000276                             FILE STATUS              IS  状態キー
000277                             LOCK        MODE         IS  AUTOMATIC.
000278     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000279                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY               IS  元－元号区分
000300                             FILE STATUS              IS  状態キー
000310                             LOCK        MODE         IS  AUTOMATIC.
000320     SELECT  保険者マスタ    ASSIGN      TO        HOKENSL
000330                             ORGANIZATION             IS  INDEXED
000340                             ACCESS MODE              IS  DYNAMIC
000350                             RECORD KEY               IS  保－保険種別
000360                                                          保－保険者番号
000370                             ALTERNATE RECORD KEY     IS  保－保険種別
000380                                                          保－保険者名称
000390                                                          保－保険者番号
000400                             FILE STATUS              IS  状態キー
000410                             LOCK        MODE         IS  AUTOMATIC.
000420     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
000430                             ORGANIZATION             IS  INDEXED
000440                             ACCESS MODE              IS  DYNAMIC
000450                             RECORD KEY               IS  市－公費種別
000460                                                          市－市町村番号
000470                             ALTERNATE RECORD KEY     IS  市－公費種別
000480                                                          市－市町村名称
000490                                                          市－市町村番号
000500                             FILE STATUS              IS  状態キー
000510                             LOCK        MODE         IS  AUTOMATIC.
000650     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000660                             ORGANIZATION             IS  INDEXED
000670                             ACCESS MODE              IS  DYNAMIC
000680                             RECORD KEY               IS  施情－施術所番号
000690                             FILE STATUS              IS  状態キー
000700                             LOCK        MODE         IS  AUTOMATIC.
000650     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000660                             ORGANIZATION             IS  INDEXED
000670                             ACCESS MODE              IS  DYNAMIC
000680                             RECORD KEY               IS  名－区分コード
000690                                                          名－名称コード
000700                             FILE STATUS              IS  状態キー
000710                             LOCK        MODE         IS  AUTOMATIC.
000541* レセ並び順用
000931     SELECT  作業ファイル２  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5802L.DAT"
000543                             ORGANIZATION             IS  INDEXED
000544                             ACCESS                   IS  DYNAMIC
000545                             RECORD      KEY          IS  作２－施術和暦年月
000546                                                          作２－患者コード
000547                                                          作２－保険種別
000548                             ALTERNATE RECORD KEY     IS  作２－順番
000551                             FILE        STATUS       IS  状態キー
000552                             LOCK        MODE         IS  AUTOMATIC.
000553*
000554     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF001
000560                             SYMBOLIC    DESTINATION  IS "PRT"
000570                             FORMAT                   IS  定義体名Ｐ
000580                             GROUP                    IS  項目群名Ｐ
000590                             PROCESSING  MODE         IS  処理種別Ｐ
000600                             UNIT        CONTROL      IS  拡張制御Ｐ
000610                             FILE        STATUS       IS  通知情報Ｐ.
000620******************************************************************
000630*                      DATA DIVISION                             *
000640******************************************************************
000650 DATA                    DIVISION.
000660 FILE                    SECTION.
000670*                           ［ＲＬ＝  １２８］
000680 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
000690     COPY GENGOU         OF  XFDLIB  JOINING    元   AS  PREFIX.
000700*                           ［ＲＬ＝  ３２０］
000710 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
000720     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
000721*                           ［ＲＬ＝  ２５６］
000722 FD  市町村マスタ          BLOCK   CONTAINS   1   RECORDS.
000723     COPY SITYOSN        OF  XFDLIB  JOINING   市   AS  PREFIX.
000724*
000744 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
000754     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
002140*                           ［ＲＬ＝  １２８］
002150 FD  施術所情報マスタ    BLOCK   CONTAINS   1   RECORDS.
002160     COPY SEJOHO          OF  XFDLIB  JOINING   施情   AS  PREFIX.
001070*                           ［ＲＬ＝  １２８］
001080 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
001090     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
000971*
001581 FD  作業ファイル２ RECORD  CONTAINS 64 CHARACTERS.
001582 01  作２－レコード.
001583     03  作２－レコードキー.
001584         05  作２－施術和暦年月.
001585             07  作２－施術和暦            PIC 9.
001586             07  作２－施術年              PIC 9(2).
001587             07  作２－施術月              PIC 9(2).
001588         05  作２－患者コード.
001589             07 作２－患者番号             PIC 9(6).
001590             07 作２－枝番                 PIC X(1).
001591         05  作２－保険種別                PIC 9(2).
001592     03  作２－レコードデータ.
001593         05  作２－返戻区分                PIC 9.
001594         05  作２－分類コード              PIC 9(1).
001261         05  作２－県コード                PIC X(2).
001400         05  作２－保険順                  PIC 9(2).
001595         05  作２－順番                    PIC 9(4).
001596         05  作２－本人家族区分            PIC 9.
001597         05  作２－保険者番号              PIC X(8).
001591         05  作２－負担割合                PIC 9(2).
               05  作２－費用額                  PIC 9(6).
               05  作２－負担額                  PIC 9(6).
               05  作２－請求額                  PIC 9(6).
001598         05  FILLER                        PIC X(11).
000994*
000995 FD  印刷ファイル.
001000     COPY YHN582P        OF  XMDLIB.
001001*
001010******************************************************************
001020*                WORKING-STORAGE SECTION                         *
001030******************************************************************
001040 WORKING-STORAGE         SECTION.
001050 01 行カウンタ                         PIC 9(2) VALUE ZERO.
001060 01 頁カウンタ                         PIC 9(4) VALUE ZERO.
001070 01 最大行数                           PIC 9(2) VALUE ZERO.
001080 01 ヘッダ行数                         PIC 9(2) VALUE ZERO.
001090 01 キー入力                           PIC X    VALUE SPACE.
001100 01 状態キー                           PIC X(2) VALUE SPACE.
001110 01 終了フラグ                         PIC X(3) VALUE SPACE.
001120 01 終了フラグ２                       PIC X(3) VALUE SPACE.
001130 01 終了フラグ３                       PIC X(3) VALUE SPACE.
001140 01 確認入力Ｗ                         PIC X(1) VALUE SPACE.
001150 01 ファイル名Ｗ                       PIC N(2) VALUE SPACE.
001160 01 カレント元号Ｗ                     PIC 9(1) VALUE ZERO.
001210 01 オープンフラグ                     PIC X(3) VALUE SPACE.
001170 01 実行キーＷ                         PIC X(4) VALUE SPACE.
001180 01 前和暦Ｗ                           PIC 9 VALUE ZERO.
001190 01 返戻Ｆ                             PIC 9 VALUE ZERO.
001200 01 処理移動キー                       PIC X(4) VALUE SPACE.
001210 01 本人家族Ｗ                         PIC 9(1) VALUE ZERO.
001220 01 返戻Ｗ                             PIC 9(1) VALUE ZERO.
001220 01 分類コードＷＲ                     PIC 9(1) VALUE ZERO.
001220 01 県コードＷＲ                       PIC X(2) VALUE SPACE.
001220 01 保険順ＷＲ                         PIC 9(2) VALUE ZERO.
001220 01 府県Ｗ                             PIC X(2) VALUE SPACE.
001220 01 県名Ｗ.
          03 県名ＷＰ                        PIC X(8) VALUE SPACE.
001220 01 保険種別Ｗ                         PIC 9(2) VALUE ZERO.
001597 01 保険者番号ＷＲ                     PIC X(8) VALUE SPACE.
001200 01 種別Ｗ                             PIC X(30) VALUE SPACE.
001230 01 番号Ｗ.
001231    03 分類コードＷ                    PIC 9 VALUE ZERO.
001232    03 FILLER                          PIC X VALUE "-".
001233    03 順番Ｗ                          PIC 9(3) VALUE ZERO.
       01 システム日付Ｗ.
          03 西暦Ｗ                          PIC X(2) VALUE SPACE.
          03 年月日Ｗ                        PIC X(6) VALUE SPACE.
       01 時間ＷＰ.
          03 時分秒Ｗ                        PIC X(6) VALUE SPACE.
          03 秒以下Ｗ                        PIC X(2) VALUE SPACE.
001200 01 印刷日付Ｗ                         PIC X(20) VALUE SPACE.
001240*
001250 01 請求先名称Ｗ.
001260    03 印刷請求先名称Ｗ                PIC X(40) VALUE SPACE.
001270    03 FILLER                          PIC X(30) VALUE SPACE.
001280 01 施術和暦年月ＷＲ.
001290    03 施術和暦ＷＲ                    PIC 9    VALUE ZERO.
001300    03 施術年月ＷＲ.
001310       05 施術年ＷＲ                   PIC 9(2) VALUE ZERO.
001320       05 施術月ＷＲ                   PIC 9(2) VALUE ZERO.
001330 01 請求和暦年月日Ｗ.
001340    03 請求和暦Ｗ                      PIC 9    VALUE ZERO.
001350    03 請求年月Ｗ.
001360       05 請求年Ｗ                     PIC 9(2) VALUE ZERO.
001370       05 請求月Ｗ                     PIC 9(2) VALUE ZERO.
001380    03 請求日Ｗ                        PIC 9(2) VALUE ZERO.
001390 01 元号名称Ｗ                         PIC N(2) VALUE SPACE.
007080**************
007090* 施術所情報 *
007100**************
007110 01 施術所情報Ｗ.
007120    03 柔整師番号Ｗ                    PIC X(22)  VALUE SPACE.
007130    03 接骨師会会員番号Ｗ              PIC X(10)  VALUE SPACE.
007140    03 代表者カナＷ                    PIC X(50)  VALUE SPACE.
007150    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
007160    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
007170    03 施術所住所Ｗ.
007180       05 施術所住所１Ｗ               PIC X(50)  VALUE SPACE.
007190       05 施術所住所２Ｗ               PIC X(50)  VALUE SPACE.
007200    03 施術所郵便番号Ｗ.
007210       05 施術所郵便番号１Ｗ           PIC X(3)   VALUE SPACE.
007220       05 施術所郵便番号２Ｗ           PIC X(4)   VALUE SPACE.
007230    03 施術所電話番号Ｗ                PIC X(15)  VALUE SPACE.
001800** 合計用
       01 合計Ｗ.
          03 合計件数Ｗ                      PIC 9(4) VALUE ZERO.
          03 合計費用額Ｗ                    PIC 9(10) VALUE ZERO.
          03 合計負担額Ｗ                    PIC 9(10) VALUE ZERO.
          03 合計請求額Ｗ                    PIC 9(10) VALUE ZERO.
          03 合計助成件数Ｗ                  PIC 9(4) VALUE ZERO.
          03 合計受給者負担金Ｗ              PIC 9(10) VALUE ZERO.
       01 種別合計Ｗ.
          03 種別合計件数Ｗ                  PIC 9(4) VALUE ZERO.
          03 種別合計費用額Ｗ                PIC 9(10) VALUE ZERO.
          03 種別合計負担額Ｗ                PIC 9(10) VALUE ZERO.
          03 種別合計請求額Ｗ                PIC 9(10) VALUE ZERO.
          03 種別合計助成件数Ｗ              PIC 9(4) VALUE ZERO.
          03 種別合計受給者負担金Ｗ          PIC 9(10) VALUE ZERO.
      *
001400* エラーメッセージ用
001410 01 エラーメッセージＷ.
001420    03 エラー患者コードＷ              PIC X(7)  VALUE SPACE.
001430    03 エラー区切りＷ                  PIC X(1)  VALUE SPACE.
001440    03 エラー保険種別Ｗ                PIC X(2)  VALUE SPACE.
001450    03 FILLER                          PIC X(10) VALUE SPACE.
001650*
001651***********************************************************************
001660 01 印刷制御.
001670     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
001680     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
001690     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
001700     03 拡張制御Ｐ.
001710         05 端末制御Ｐ.
001720             07 移動方向Ｐ             PIC X(1).
001730             07 移動行数Ｐ             PIC 9(3).
001740         05 詳細制御Ｐ                 PIC X(2).
001750     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
001760     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
001770 01 計算機西暦年Ｗ                     PIC 9(2).
001780* 日付ＷＯＲＫ
001790 01 和暦終了年Ｗ                       PIC 9(4).
001800 01 計算機和暦年Ｗ                     PIC 9(2).
001810 01 計算機西暦.
001820    03 計算機西暦年                    PIC 9(4).
001830    03 計算機西暦月日                  PIC 9(4).
001840 01 計算機西暦Ｒ REDEFINES 計算機西暦.
001850    03 計算機世紀                      PIC 9(2).
001860    03 計算機日付                      PIC 9(6).
001870    03 計算機日付Ｒ REDEFINES 計算機日付.
001880       05 計算機年月                   PIC 9(4).
001890       05 計算機年月Ｒ REDEFINES 計算機年月.
001900         07 計算機年                   PIC 9(2).
001910         07 計算機月                   PIC 9(2).
001920       05 計算機日                     PIC 9(2).
001930*
001940******************************************************************
001950*                          連結項目                              *
001960******************************************************************
001970*
001980**********************
001990* メッセージ表示キー *
002000**********************
002010*
002020 01 連メ－キー IS EXTERNAL.
002030    03  連メ－メッセージ                 PIC N(20).
002040*
002050 01 連メ３－キー IS EXTERNAL.
002060    03  連メ３－メッセージ             PIC N(20).
002070    03  連メ３－メッセージ１           PIC X(20).
002140*
002150****************
002160* 画面入力情報 *
002170****************
002180 01 連入－画面情報ＹＨＮ５８０ IS EXTERNAL.
002270    03 連入－請求和暦年月.
002280       05 連入－請求和暦               PIC 9.
002290       05 連入－請求年月.
002300         07 連入－請求年               PIC 9(2).
002310         07 連入－請求月               PIC 9(2).
          03 連入－プレビュー区分             PIC 9.
002320*
002260 01 連印－印刷情報ＹＨＮ５８１ IS EXTERNAL.
           03 連印－当月件数                 PIC 9(4).
           03 連印－当月合計額               PIC 9(8).
           03 連印－当月負担金額             PIC 9(8).
           03 連印－当月請求金額             PIC 9(8).
           03 連印－返戻件数                 PIC 9(4).
           03 連印－返戻合計額               PIC 9(8).
           03 連印－返戻負担金額             PIC 9(8).
           03 連印－返戻請求金額             PIC 9(8).
           03 連印－総件数                   PIC 9(4).
           03 連印－総合計額                 PIC 9(8).
           03 連印－総負担金額               PIC 9(8).
           03 連印－総請求金額               PIC 9(8).
002231*
000540************************************
000550* プリンタファイル作成用           *
000560************************************
000570 01 Ｈ連ＰＲＴＦ－作成データ IS EXTERNAL.
000580     03 Ｈ連ＰＲＴＦ－ファイル名           PIC X(8).
000590     03 Ｈ連ＰＲＴＦ－プレビュー区分       PIC 9.
000600     03 Ｈ連ＰＲＴＦ－帳票プログラム名     PIC X(8).
000610     03 Ｈ連ＰＲＴＦ－オーバレイ名         PIC X(8).
002240*
002240******************************************************************
002250*                      PROCEDURE  DIVISION                       *
002260******************************************************************
002270 PROCEDURE               DIVISION.
002280************
002290*           *
002300* 初期処理   *
002310*           *
002320************
002560     MOVE SPACE TO オープンフラグ.
002570     PERFORM プリンタファイル作成.
002330     PERFORM 初期化.
002340************
002350*           *
002360* 主処理     *
002370*           *
002380************
002390     PERFORM 印刷処理.
002400************
002410*           *
002420* 終了処理   *
002430*           *
002440************
002450     PERFORM 終了処理.
002460     EXIT PROGRAM.
002470*
002480*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
002860*================================================================*
002870 プリンタファイル作成 SECTION.
002880*================================================================*
002890*   / 初期化 /
002900     MOVE SPACE TO Ｈ連ＰＲＴＦ－作成データ.
002910     INITIALIZE Ｈ連ＰＲＴＦ－作成データ.
002920*
002930*
002940*--↓↓ 変更箇所 ↓↓--------------------------------------*
002970*   使用するプリンタファイル名セット
      *   印字調整なし
002971     MOVE "PRTF001"             TO Ｈ連ＰＲＴＦ－ファイル名.
002972*
002973*   使用する帳票プログラム名セット
002974     MOVE "YHN582"              TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分 TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
002490*================================================================*
002500 初期化 SECTION.
002510*
002520     OPEN INPUT 元号マスタ.
002530         MOVE NC"元号" TO ファイル名Ｗ.
002540          PERFORM オープンチェック.
002550     OPEN INPUT 保険者マスタ.
002560         MOVE NC"保険者マスタ" TO ファイル名Ｗ.
002570         PERFORM オープンチェック.
002571     OPEN INPUT 市町村マスタ
002572         MOVE NC"市町村" TO ファイル名Ｗ.
002573         PERFORM オープンチェック.
002574     OPEN INPUT 受診者情報Ｆ.
002575         MOVE NC"受診者" TO ファイル名Ｗ.
002576         PERFORM オープンチェック.
014470     OPEN INPUT   施術所情報マスタ.
014480         MOVE NC"施情" TO ファイル名Ｗ.
014490         PERFORM オープンチェック.
003540     OPEN INPUT 名称マスタ.
003550         MOVE NC"名称マスタ"   TO ファイル名Ｗ.
003560         PERFORM オープンチェック.
002577*
002580     OPEN INPUT 作業ファイル２.
002590         MOVE NC"作２" TO ファイル名Ｗ.
002600         PERFORM オープンチェック.
002640*
002650*================================================================*
002660 オープンチェック SECTION.
002670*
002680     IF 状態キー  NOT =  "00"
002690         DISPLAY ファイル名Ｗ NC"Ｆオープンエラー" UPON CONS
002700         DISPLAY NC"状態キー：" 状態キー           UPON CONS
002710         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
002720                                                   UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
002730         ACCEPT  キー入力 FROM CONS
002740         PERFORM ファイル閉鎖
002750         EXIT PROGRAM.
002760*================================================================*
002770 ファイル閉鎖 SECTION.
002780*
003570     IF ( オープンフラグ = "YES" )
003580         CLOSE 印刷ファイル
003630     END-IF.
003640*
002790     CLOSE  受診者情報Ｆ 元号マスタ  保険者マスタ   施術所情報マスタ
                  作業ファイル２  市町村マスタ 名称マスタ.
002800*================================================================*
002810 終了処理 SECTION.
002820*
002830     PERFORM ファイル閉鎖.
002840*================================================================*
002850 印刷処理 SECTION.
002860*
002870     MOVE 40    TO 最大行数.
002880*
002890     MOVE ZERO  TO 合計Ｗ.
002890     MOVE ZERO  TO 種別合計Ｗ.
002890     MOVE ZERO  TO 行カウンタ.
002900     MOVE ZERO  TO 頁カウンタ.
002910     MOVE SPACE TO 終了フラグ.
002930*
           ACCEPT 年月日Ｗ FROM DATE.
      *    /* 1980～2079年の間で設定 */
           IF 年月日Ｗ(1:2) > 80
               MOVE 19 TO 西暦Ｗ
           ELSE
               MOVE 20 TO 西暦Ｗ
           END-IF.
           ACCEPT 時間ＷＰ FROM TIME.
      *
           STRING システム日付Ｗ(1:4)    DELIMITED BY SIZE
                  "/"                    DELIMITED BY SIZE
                  システム日付Ｗ(5:2)    DELIMITED BY SIZE
                  "/"                    DELIMITED BY SIZE
                  システム日付Ｗ(7:2)    DELIMITED BY SIZE
                  " "                    DELIMITED BY SIZE
                  時間ＷＰ(1:2)          DELIMITED BY SIZE
                  ":"                    DELIMITED BY SIZE
                  時間ＷＰ(3:2)          DELIMITED BY SIZE
                  ":"                    DELIMITED BY SIZE
                  時間ＷＰ(5:2)          DELIMITED BY SIZE
             INTO 印刷日付Ｗ
           END-STRING.
      *
002970     MOVE ZERO  TO 作２－分類コード.
002960     MOVE ZERO  TO 作２－県コード.
002970     MOVE ZERO  TO 作２－保険者番号.
002980     MOVE ZERO  TO 作２－順番.
003020*
003030     START 作業ファイル２ KEY IS > 作２－順番
003110     END-START.
003120     IF 状態キー  =  "00"
003130         PERFORM 作業ファイル２読込
003140         MOVE 作２－分類コード  TO 分類コードＷＲ
003140         MOVE 作２－保険者番号  TO 保険者番号ＷＲ
003200         MOVE 作２－県コード    TO 県コードＷＲ
003200         MOVE 作２－保険順      TO 保険順ＷＲ
003160         COMPUTE 頁カウンタ = 頁カウンタ + 1
003170         PERFORM ヘッダレコードセット
003180         PERFORM ヘッダ印刷処理
               PERFORM 種別レコードセット
               PERFORM 種別印刷処理
003190         PERFORM UNTIL 終了フラグ = "YES"
003200             IF ( 行カウンタ >= 最大行数   )
003210                 PERFORM 改頁処理
003220                 COMPUTE 頁カウンタ = 頁カウンタ + 1
003230                 PERFORM ヘッダレコードセット
003240                 PERFORM ヘッダ印刷処理
003251                 MOVE ZERO    TO 行カウンタ
003260             ELSE
                       IF ((作２－県コード = SPACE) AND (作２－保険者番号 NOT = 保険者番号ＷＲ)) OR
                          ((作２－県コード NOT = SPACE) AND (作２－県コード NOT = 県コードＷＲ )) OR
                          ((分類コードＷＲ = 5) AND (作２－保険順     NOT = 保険順ＷＲ))
                           PERFORM 合計レコードセット
                           PERFORM 合計印刷処理
                           MOVE ZERO TO 合計Ｗ
003140                     MOVE 作２－保険者番号  TO 保険者番号ＷＲ
003200                     MOVE 作２－県コード    TO 県コードＷＲ
003200                     MOVE 作２－保険順      TO 保険順ＷＲ
003200                     IF ( 行カウンタ >= 最大行数   )
003210                        PERFORM 改頁処理
003220                        COMPUTE 頁カウンタ = 頁カウンタ + 1
003230                        PERFORM ヘッダレコードセット
003240                        PERFORM ヘッダ印刷処理
003251                        MOVE ZERO    TO 行カウンタ
                           END-IF
                       END-IF
                       IF ( 作２－分類コード NOT = 分類コードＷＲ )
                           PERFORM 種別合計レコードセット
                           PERFORM 合計印刷処理
                           MOVE ZERO TO 種別合計Ｗ
003200                     IF ( 行カウンタ >= 最大行数   )
003210                        PERFORM 改頁処理
003220                        COMPUTE 頁カウンタ = 頁カウンタ + 1
003230                        PERFORM ヘッダレコードセット
003240                        PERFORM ヘッダ印刷処理
003251                        MOVE ZERO    TO 行カウンタ
                           END-IF
                           PERFORM 種別レコードセット
                           PERFORM 種別印刷処理
003140                     MOVE 作２－分類コード  TO 分類コードＷＲ
003200                     IF ( 行カウンタ >= 最大行数   )
003210                        PERFORM 改頁処理
003220                        COMPUTE 頁カウンタ = 頁カウンタ + 1
003230                        PERFORM ヘッダレコードセット
003240                        PERFORM ヘッダ印刷処理
003251                        MOVE ZERO    TO 行カウンタ
                           END-IF
                       END-IF
003320                 PERFORM 明細レコードセット
003330                 PERFORM 明細印刷処理
003330                 PERFORM 集計処理
003360                 PERFORM 作業ファイル２読込
003370             END-IF
003380         END-PERFORM
               PERFORM 合計レコードセット
               PERFORM 合計印刷処理
003200         IF ( 行カウンタ >= 最大行数   )
003210            PERFORM 改頁処理
003220            COMPUTE 頁カウンタ = 頁カウンタ + 1
003230            PERFORM ヘッダレコードセット
003240            PERFORM ヘッダ印刷処理
003251            MOVE ZERO    TO 行カウンタ
               END-IF
               PERFORM 種別合計レコードセット
               PERFORM 合計印刷処理
003200         IF ( 行カウンタ >= 最大行数   )
003210            PERFORM 改頁処理
003220            COMPUTE 頁カウンタ = 頁カウンタ + 1
003230            PERFORM ヘッダレコードセット
003240            PERFORM ヘッダ印刷処理
003251            MOVE ZERO    TO 行カウンタ
               END-IF
               PERFORM 総合計レコードセット
               PERFORM 総合計印刷処理
003400     END-IF.
003401*
003410*================================================================*
003420 作業ファイル２読込 SECTION.
003430*
003440     READ 作業ファイル２ NEXT
003450     AT END
003460         MOVE "YES" TO 終了フラグ
003470     END-READ.
003471*
003480*================================================================*
003490 ヘッダレコードセット SECTION.
003500*
003510     MOVE SPACE TO YHN582P.
003520     INITIALIZE YHN582P.
003530     MOVE 頁カウンタ        TO 頁.
           MOVE 印刷日付Ｗ        TO 印刷日付.
003546*
      */元号修正/↓↓↓20190514
           MOVE 連入－請求和暦     TO 元－元号区分
037380     READ 元号マスタ
037390     NOT INVALID KEY
037400         MOVE 元－元号名称   TO 請求和暦
037410     END-READ.
      */元号修正/↑↑↑20190514
003620     MOVE 連入－請求年      TO 請求年.
003630     MOVE 連入－請求月      TO 請求月.
           PERFORM 施術所情報取得.
           MOVE 接骨師会会員番号Ｗ TO 会員番号.
           MOVE 接骨院名Ｗ        TO 施術所名.
           MOVE 代表者名Ｗ        TO 代表者名.
003631*
003640*================================================================*
003650 明細レコードセット SECTION.
003660*
003675     MOVE SPACE TO YHN582P.
003680     INITIALIZE YHN582P.
003690*
003700     MOVE 作２－順番         TO 番号.
003710*
           MOVE 10              TO 名－区分コード.
           MOVE 作２－保険種別  TO 名－名称コード.
           READ 名称マスタ
           NOT INVALID KEY
               MOVE 名－略称       TO 種別
           END-READ.
           EVALUATE 作２－保険種別
           WHEN 1
               IF 作２－保険者番号(3:1) = 3
                  MOVE NC"国組"    TO 種別
               END-IF
           WHEN 8
               MOVE NC"退国"       TO 種別
           WHEN 5
               MOVE NC"後期"       TO 種別
           END-EVALUATE.
003711*
003772     IF 作２－本人家族区分 = 1
003773         MOVE NC"本"     TO 本人家族
003774     ELSE
003775         MOVE NC"家"     TO 本人家族
003776     END-IF.
003781*
003782     MOVE 作２－保険者番号     TO 保険者番号.
003790     PERFORM 請求先情報取得.
003800     MOVE 請求先名称Ｗ         TO 保険者名称.
003810     MOVE 作２－患者番号       TO 患者番号.
003810     MOVE 作２－枝番           TO 枝番.
003811*
003813     MOVE 作２－施術和暦       TO 受－施術和暦.
003814     MOVE 作２－施術年         TO 受－施術年.
003815     MOVE 作２－施術月         TO 受－施術月.
003816     MOVE 作２－患者コード     TO 受－患者コード.
003817     READ 受診者情報Ｆ
003821     NOT INVALID KEY
003845         MOVE 受－患者氏名     TO 患者氏名
003856     END-READ.
003711*
003772     EVALUATE 作２－返戻区分
           WHEN 1
003773         MOVE NC"当"           TO 返戻
003774     WHEN 2
003775         MOVE NC"返"           TO 返戻
003776     END-EVALUATE.
003893*
003894     IF 連入－請求和暦年月 NOT = 作２－施術和暦年月
003895         MOVE 作２－施術年     TO 施術年
003896         MOVE 作２－施術月     TO 施術月
003897         MOVE "/"              TO 区切
003898     END-IF.
003690*
           MOVE 作２－負担割合       TO 割合.
           MOVE NC"割"               TO 割.
003700     MOVE 作２－費用額         TO 費用額.
003700     MOVE 作２－負担額         TO 一部負担金.
003700     MOVE 作２－請求額         TO 請求額.
           IF 作２－保険種別 > 50
              MOVE "*"               TO 費用額符号
              MOVE "*"               TO 一部負担金符号
           END-IF.
      *
           IF 作２－返戻区分 = 2
              MOVE 1                 TO 返戻Ｆ
           END-IF.
003899*
003640*================================================================*
003650 種別レコードセット SECTION.
003857*
003675     MOVE SPACE TO YHN582P.
003680     INITIALIZE YHN582P.
003690*
003858     EVALUATE 作２－分類コード
003862     WHEN 01
003863         MOVE "全国健康保険協会"            TO 保険種別 種別Ｗ
003862     WHEN 02
003863         MOVE "船員保険"                    TO 保険種別 種別Ｗ
           WHEN 03
003865         MOVE "健康保険組合"                TO 保険種別 種別Ｗ
003866     WHEN 04
003867         MOVE "共済組合、自衛官"            TO 保険種別 種別Ｗ
           WHEN 05
003861         MOVE "国保連合会"                  TO 保険種別 種別Ｗ
003868     WHEN 06
003875         MOVE "医療助成"                    TO 保険種別 種別Ｗ
003890     END-EVALUATE.
003899*
003640*================================================================*
003650 合計レコードセット SECTION.
003857*
003675     MOVE SPACE TO YHN582P.
003680     INITIALIZE YHN582P.
003857*
           IF (県コードＷＲ = SPACE) OR (分類コードＷＲ = 1 OR 2)
              STRING "　　【合計】　"        DELIMITED BY SIZE
                     請求先名称Ｗ            DELIMITED BY SPACE
                INTO 合計保険種別
              END-STRING
           ELSE
              EVALUATE 県コードＷＲ
              WHEN 01
                  MOVE SPACE         TO 府県Ｗ
              WHEN 13
                  MOVE "都"          TO 府県Ｗ
              WHEN 26
              WHEN 27
                  MOVE "府"          TO 府県Ｗ
              WHEN OTHER
                  MOVE "県"          TO 府県Ｗ
              END-EVALUATE
              MOVE 13                TO 名－区分コード
              MOVE 県コードＷＲ      TO 名－名称コード
              READ 名称マスタ
              NOT INVALID KEY
                 MOVE 名－略称       TO 県名Ｗ
              END-READ
              STRING "　　【合計】　"         DELIMITED BY SIZE
                     県名ＷＰ                 DELIMITED BY "　"
                     府県Ｗ                   DELIMITED BY SPACE
                     "国民健康保険団体連合会" DELIMITED BY SIZE
                INTO 合計保険種別
              END-STRING
           END-IF.
      *
           IF 返戻Ｆ = 1
              MOVE "※返戻分は集計されません" TO 返戻コメント
              MOVE ZERO              TO 返戻Ｆ
           END-IF.
      *
           MOVE 合計件数Ｗ           TO 合計件数.
           MOVE 合計費用額Ｗ         TO 合計費用額.
           MOVE 合計負担額Ｗ         TO 合計一部負担金.
           MOVE 合計請求額Ｗ         TO 合計請求額.
           IF 合計助成件数Ｗ NOT = ZERO
              MOVE 合計助成件数Ｗ     TO 合計助成件数
              MOVE 合計受給者負担金Ｗ TO 合計受給者負担金
              MOVE "件"               TO 助成件
              MOVE "*"             TO 受給者負担金符号
              IF 合計受給者負担金Ｗ = ZERO
                 MOVE "0"             TO 受給者負担金ゼロ
              END-IF
           END-IF.
003899*
003640*================================================================*
003650 種別合計レコードセット SECTION.
003857*
003675     MOVE SPACE TO YHN582P.
003680     INITIALIZE YHN582P.
003857*
           STRING "【種別合計】　"        DELIMITED BY SIZE
                  種別Ｗ                  DELIMITED BY SPACE
             INTO 合計保険種別
           END-STRING.
      *
           MOVE 種別合計件数Ｗ           TO 合計件数.
           MOVE 種別合計費用額Ｗ         TO 合計費用額.
           MOVE 種別合計負担額Ｗ         TO 合計一部負担金.
           MOVE 種別合計請求額Ｗ         TO 合計請求額.
           IF 種別合計助成件数Ｗ NOT = ZERO
              MOVE 種別合計助成件数Ｗ     TO 合計助成件数
              MOVE 種別合計受給者負担金Ｗ TO 合計受給者負担金
              MOVE "件"                   TO 助成件
              MOVE "*"             TO 受給者負担金符号
              IF 種別合計受給者負担金Ｗ = ZERO
                 MOVE "0"             TO 受給者負担金ゼロ
              END-IF
           END-IF.
003899*
003640*================================================================*
003650 総合計レコードセット SECTION.
003857*
003675     MOVE SPACE TO YHN582P.
003680     INITIALIZE YHN582P.
003857*
           MOVE 連印－当月件数         TO 当月合計件数.
           MOVE 連印－当月合計額       TO 当月合計費用額.
           MOVE 連印－当月負担金額     TO 当月合計一部負担金.
           MOVE 連印－当月請求金額     TO 当月合計請求額.
003857*
           IF 連印－返戻件数 NOT = ZERO
               MOVE "返戻分："         TO 返戻分
               MOVE "件"               TO 返戻件
               MOVE 連印－返戻件数     TO 返戻件数
               MOVE 連印－返戻合計額   TO 返戻費用額
               MOVE 連印－返戻負担金額 TO 返戻一部負担金
               MOVE 連印－返戻請求金額 TO 返戻請求額
           END-IF.
      *
           MOVE 連印－総件数           TO 総合計件数.
           MOVE 連印－総合計額         TO 総合計費用額.
           MOVE 連印－総負担金額       TO 総合計一部負担金.
           MOVE 連印－総請求金額       TO 総合計請求額.
003899*
003640*================================================================*
003650 集計処理 SECTION.
003857*
           IF 作２－返戻区分 NOT = 2
              IF 作２－保険種別 < 10
                 COMPUTE 合計件数Ｗ   = 合計件数Ｗ + 1
                 COMPUTE 合計費用額Ｗ = 合計費用額Ｗ + 作２－費用額
                 COMPUTE 合計負担額Ｗ = 合計負担額Ｗ + 作２－負担額
                 COMPUTE 合計請求額Ｗ = 合計請求額Ｗ + 作２－請求額
      *
                 COMPUTE 種別合計件数Ｗ   = 種別合計件数Ｗ + 1
                 COMPUTE 種別合計費用額Ｗ = 種別合計費用額Ｗ + 作２－費用額
                 COMPUTE 種別合計負担額Ｗ = 種別合計負担額Ｗ + 作２－負担額
                 COMPUTE 種別合計請求額Ｗ = 種別合計請求額Ｗ + 作２－請求額
              ELSE
                 COMPUTE 合計助成件数Ｗ     = 合計助成件数Ｗ + 1
                 COMPUTE 合計受給者負担金Ｗ = 合計受給者負担金Ｗ + 作２－負担額
                 COMPUTE 合計請求額Ｗ = 合計請求額Ｗ + 作２－請求額
      *
                 COMPUTE 種別合計助成件数Ｗ     = 種別合計助成件数Ｗ + 1
                 COMPUTE 種別合計受給者負担金Ｗ = 種別合計受給者負担金Ｗ + 作２－負担額
                 COMPUTE 種別合計請求額Ｗ = 種別合計請求額Ｗ + 作２－請求額
              END-IF
           END-IF.
      *
022390*================================================================*
022400 施術所情報取得 SECTION.
022410*================================================================*
022420**************************************************
022430* 本院データを使用し、以下の情報を取得           *
022440* ● 柔整師番号.. 柔整師番号Ｗに格納             *
022450* ● 会員番号 ... 接骨師会会員番号Ｗに格納       *
022460* ● 代表者名 ... 代表者名Ｗに格納               *
022470* ● 住所1,2   ...施術所住所1,2Ｗに格納          *
022480* ● 電話番号 ... 施術所電話番号Ｗに格納         *
022490**************************************************
022500     MOVE ZERO  TO 施情－施術所番号.
022510     READ 施術所情報マスタ
022520     INVALID KEY
022530         CONTINUE
022540     NOT INVALID KEY
022550*
022590         MOVE 施情－新柔整師番号      TO 柔整師番号Ｗ
022610*
022620         MOVE 施情－接骨師会会員番号  TO 接骨師会会員番号Ｗ
022630*
022640         MOVE 施情－郵便番号１        TO 施術所郵便番号１Ｗ
022650         MOVE 施情－郵便番号２        TO 施術所郵便番号２Ｗ
022660         MOVE 施情－代表者カナ        TO 代表者カナＷ
022670         MOVE 施情－代表者名          TO 代表者名Ｗ
022680         MOVE 施情－接骨院名          TO 接骨院名Ｗ
022690*
022700         MOVE 施情－住所１            TO 施術所住所１Ｗ
022710         MOVE 施情－住所２            TO 施術所住所２Ｗ
022720*
022730         MOVE 施情－電話番号          TO 施術所電話番号Ｗ
022750**
023010     END-READ.
003900*================================================================*
003901 請求先情報取得 SECTION.
003910*
003920     IF ( 作２－保険種別 NOT = 05 ) AND ( 作２－保険種別 < 10 )
003930*    / 健保 /
003960        MOVE 作２－保険種別      TO 保－保険種別
003970        MOVE 作２－保険者番号    TO 保－保険者番号
003980        MOVE SPACE               TO 請求先名称Ｗ
003990        READ 保険者マスタ
004000        INVALID KEY
004010            MOVE SPACE           TO 請求先名称Ｗ
004020        NOT INVALID KEY
004030            MOVE 保－保険者名称  TO 請求先名称Ｗ
004040        END-READ
004041     ELSE
004042*    / 老人・助成 /
004045        MOVE 作２－保険種別      TO 市－公費種別
004046        MOVE 作２－保険者番号    TO 市－市町村番号
004048        READ 市町村マスタ
004049        INVALID KEY
004050            MOVE SPACE           TO 請求先名称Ｗ
004051        NOT INVALID KEY
                  IF 作２－保険種別 = 05
004062                MOVE 市－支部名  TO 請求先名称Ｗ
                  ELSE
004062                MOVE 市－市町村名称  TO 請求先名称Ｗ
                  END-IF
004064        END-READ
004065     END-IF.
004066*
004068*================================================================*
004069 ヘッダ印刷処理 SECTION.
004070*
006050     IF ( オープンフラグ NOT = "YES" )
006060        MOVE "YES" TO オープンフラグ
006070        OPEN I-O  印刷ファイル
006080        PERFORM エラー処理Ｐ
006090     END-IF.
004080     MOVE "YHN582P"  TO  定義体名Ｐ.
004090     MOVE SPACE      TO  処理種別Ｐ.
004100     MOVE "HEAD"     TO  項目群名Ｐ.
004110     WRITE YHN582P.
004120     PERFORM エラー処理Ｐ.
004130*================================================================*
004140 種別印刷処理 SECTION.
004150*
004160     MOVE "YHN582P"  TO  定義体名Ｐ.
004090     MOVE SPACE      TO  処理種別Ｐ.
004170     MOVE "GRP001"   TO  項目群名Ｐ.
004180     WRITE YHN582P.
004190     PERFORM エラー処理Ｐ.
004200     COMPUTE 行カウンタ = 行カウンタ + 2.
004130*================================================================*
004140 明細印刷処理 SECTION.
004150*
004160     MOVE "YHN582P"  TO  定義体名Ｐ.
004090     MOVE SPACE      TO  処理種別Ｐ.
004170     MOVE "GRP002"   TO  項目群名Ｐ.
004180     WRITE YHN582P.
004190     PERFORM エラー処理Ｐ.
004200     COMPUTE 行カウンタ = 行カウンタ + 1.
004210*================================================================*
004220 合計印刷処理 SECTION.
004230*
004240     MOVE "YHN582P"  TO  定義体名Ｐ.
004090     MOVE SPACE      TO  処理種別Ｐ.
004250     MOVE "FOOT1"    TO  項目群名Ｐ.
004260     WRITE YHN582P.
004270     PERFORM エラー処理Ｐ.
004280     COMPUTE 行カウンタ = 行カウンタ + 2.
004210*================================================================*
004220 総合計印刷処理 SECTION.
004230*
004240     MOVE "YHN582P"  TO  定義体名Ｐ.
004090     MOVE SPACE      TO  処理種別Ｐ.
004250     MOVE "FOOT2"    TO  項目群名Ｐ.
004260     WRITE YHN582P.
004270     PERFORM エラー処理Ｐ.
004280     COMPUTE 行カウンタ = 行カウンタ + 2.
004290*================================================================*
004300 改頁処理  SECTION.
004310*
004320     MOVE SPACE TO YHN582P.
004330     INITIALIZE YHN582P.
004340     MOVE "YHN582P" TO  定義体名Ｐ.
004350     MOVE "CT"      TO  処理種別Ｐ.
004360     MOVE "PAGE"    TO  拡張制御Ｐ.
004370     MOVE SPACE     TO  項目群名Ｐ.
004380     WRITE YHN582P.
004390     PERFORM エラー処理Ｐ.
004400     MOVE SPACE     TO  拡張制御Ｐ.
004410*================================================================*
004420 エラー表示 SECTION.
004430*
004440     DISPLAY NC"ファイル書込エラー：" ファイル名Ｗ UPON CONS.
004450     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
004460     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
004470     ACCEPT  キー入力 FROM CONS.
004480*================================================================*
004490 エラー処理Ｐ SECTION.
004500*
004510     IF 通知情報Ｐ NOT = "00"
004520         DISPLAY NC"帳票エラー"              UPON CONS
004530         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
004540         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
004550         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
004560         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
004570                                             UPON CONS
004380*-----------------------------------------*
004390         CALL "actcshm"  WITH C LINKAGE
004400*-----------------------------------------*
004580         ACCEPT  キー入力 FROM CONS
004590         PERFORM ファイル閉鎖
004600         MOVE 99 TO PROGRAM-STATUS
004610         EXIT PROGRAM
004620     END-IF.
004630*================================================================*
004640 エラー表示Ｒ SECTION.
004650*
004660     DISPLAY NC"ファイル読込エラー" ファイル名Ｗ UPON CONS.
004670     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
004680     ACCEPT  キー入力 FROM CONS.
004690     PERFORM ファイル閉鎖.
004700     EXIT PROGRAM.
004710*================================================================*
004720 エラー表示その他 SECTION.
004730*
004740     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
004750     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
004760                                                   UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
004770     ACCEPT  キー入力 FROM CONS.
004780     PERFORM ファイル閉鎖.
004790     EXIT PROGRAM.
004791*================================================================*
004800*================================================================*
004810******************************************************************
004820 END PROGRAM YHN582.
004830******************************************************************
