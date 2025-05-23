000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             HMZKOFU.
000060 AUTHOR.                 岡田 憲和
000070*
000080*----------------------------------------------------------------*
000090*   鍼灸マ   【前回施術報告書交付情報取得】
000100*----------------------------------------------------------------*
000110 DATE-WRITTEN.           2018-08-22
000120 DATE-COMPILED.          2018-08-22
000130*----------------------------------------------------------------*
000143******************************************************************
000150*            ENVIRONMENT         DIVISION                        *
000160******************************************************************
000170 ENVIRONMENT             DIVISION.
000180 CONFIGURATION           SECTION.
000190 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000200 OBJECT-COMPUTER.        FMV-DESKPOWER.
000210 SPECIAL-NAMES.          CONSOLE  IS  CONS
000220                         SYSERR   IS  MSGBOX.
000230 INPUT-OUTPUT            SECTION.
000240 FILE-CONTROL.
000480     SELECT  ＨレセプトＦ    ASSIGN      TO        HRECEL
000490                             ORGANIZATION             IS  INDEXED
000500                             ACCESS MODE              IS  DYNAMIC
000510                             RECORD KEY               IS  Ｈレセ−施術区分
000520                                                          Ｈレセ−施術和暦年月
000530                                                          Ｈレセ−患者コード
000540                                                          Ｈレセ−レセ種別
000550                             ALTERNATE RECORD KEY     IS  Ｈレセ−施術区分
000560                                                          Ｈレセ−患者コード
000570                                                          Ｈレセ−施術和暦年月
000580                                                          Ｈレセ−レセ種別
000590                             ALTERNATE RECORD KEY     IS  Ｈレセ−施術和暦年月
000600                                                          Ｈレセ−患者コード
000610                                                          Ｈレセ−レセ種別
000620                                                          Ｈレセ−施術区分
000630                             ALTERNATE RECORD KEY     IS  Ｈレセ−請求対象区分
000640                                                          Ｈレセ−請求和暦年月
000650                                                          Ｈレセ−施術区分
000660                                                          Ｈレセ−施術和暦年月
000670                                                          Ｈレセ−患者コード
000680                                                          Ｈレセ−レセ種別
000690                             FILE STATUS              IS  状態キー
000700                             LOCK        MODE         IS  AUTOMATIC.
000710*
000711     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000712                             ORGANIZATION             IS  INDEXED
000713                             ACCESS MODE              IS  DYNAMIC
000714                             RECORD KEY               IS  元−元号区分
000715                             FILE STATUS              IS  状態キー
000716                             LOCK        MODE         IS  AUTOMATIC.
000717*
           SELECT  Ｈ制御情報マスタ  ASSIGN      TO      HSEIGYOL
                                     ORGANIZATION             IS  INDEXED
                                     ACCESS MODE              IS  DYNAMIC
                                     RECORD KEY               IS  Ｈ制−制御区分
                                     FILE STATUS              IS  状態キー
                                     LOCK        MODE         IS  AUTOMATIC.
      *
           SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  受−施術和暦年月
                                                                受−患者コード
                                   ALTERNATE RECORD KEY     IS  受−施術和暦年月
                                                                受−患者カナ
                                                                受−患者コード
                                   ALTERNATE RECORD KEY     IS  受−患者コード
                                                                受−施術和暦年月
                                   ALTERNATE RECORD KEY     IS  受−施術和暦年月
                                                                受−保険種別
                                                                受−保険者番号
                                                                受−患者コード
                                   ALTERNATE RECORD KEY     IS  受−施術和暦年月
                                                                受−公費種別
                                                                受−費用負担者番号
                                                                受−患者コード
                                   ALTERNATE RECORD KEY     IS  受−施術和暦年月
                                                                受−助成種別
                                                                受−費用負担者番号助成
                                                                受−患者コード
                                   ALTERNATE RECORD KEY     IS  受−請求和暦年月
                                                                受−施術和暦年月
                                                                受−患者コード
                                   FILE STATUS              IS  状態キー
                                   LOCK        MODE         IS  AUTOMATIC.
      *
000310     SELECT  Ｈ負傷データＦ  ASSIGN      TO        HHUSYOUL
000320                             ORGANIZATION             IS  INDEXED
000330                             ACCESS MODE              IS  DYNAMIC
000340                             RECORD KEY               IS  Ｈ負−主キー
000350                             ALTERNATE RECORD KEY     IS  Ｈ負−施術区分
000360                                                          Ｈ負−患者コード
000370                                                          Ｈ負−主キー
000380                             FILE STATUS              IS  状態キー
000390                             LOCK        MODE         IS  AUTOMATIC.
000720******************************************************************
000730*                      DATA DIVISION                             *
000740******************************************************************
000750 DATA                    DIVISION.
000760 FILE                    SECTION.
000860*
000870 FD  ＨレセプトＦ        BLOCK   CONTAINS   1   RECORDS.
000880     COPY H_RECE     OF  XFDLIB  JOINING   Ｈレセ  AS  PREFIX.
000890*
000891 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS GLOBAL.
000892     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
000893*
       FD  Ｈ制御情報マスタ    BLOCK   CONTAINS   1   RECORDS.
           COPY H_SEIGYO   OF  XFDLIB  JOINING   Ｈ制   AS  PREFIX.
      *                           ［ＲＬ＝  ３２０］
       FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
           COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
000800*                           ［ＲＬ＝  ２５６］
000810 FD  Ｈ負傷データＦ      BLOCK   CONTAINS   1   RECORDS.
000820     COPY H_HUSYOU   OF  XFDLIB  JOINING   Ｈ負 AS  PREFIX.
003180*
000900******************************************************************
000910*                WORKING-STORAGE SECTION                         *
000920******************************************************************
000930 WORKING-STORAGE         SECTION.
000940 01 キー入力                           PIC X    VALUE SPACE.
000950 01 状態キー                           PIC X(2) VALUE SPACE.
000960 01 終了フラグ                         PIC X(3) VALUE SPACE.
000970 01 ファイル名                         PIC N(10) VALUE SPACE.
001000*
001001 01 プログラム名Ｗ PIC X(8) VALUE SPACE.
      *
       01 施術報告書交付料手動算定Ｗ         PIC 9(1) VALUE ZERO.
001002*
001003*/ 連結項目退避 /*
001010 01 施術区分ＷＲ                       PIC 9(1).
001014 01 施術和暦年月ＷＲ.
001022    03 施術和暦ＷＲ                    PIC 9.
001032    03 施術年月ＷＲ.
001042       05 施術年ＷＲ                   PIC 9(2).
001052       05 施術月ＷＲ                   PIC 9(2).
001062 01 患者コードＷＲ.
001072    03 患者番号ＷＲ                    PIC 9(6).
001082    03 枝番ＷＲ                        PIC X.
001092 01 レセ種別ＷＲ                       PIC 9(2).
001112*
001481 01 負傷主キーＷ                       PIC 9(8).
001482*
001502 01 前回期間終了和暦年月日.
001504    05 前回期間終了和暦年月.
001505       07 前回期間終了和暦             PIC 9 VALUE ZERO.
001506       07 前回期間終了年               PIC 9(2) VALUE ZERO.
001508       07 前回期間終了月               PIC 9(2) VALUE ZERO.
001509    05 前回期間終了日                  PIC 9(2) VALUE ZERO.
001516*
001517 01 元号区分Ｗ                         PIC 9(1) VALUE ZERO.
001518 01 西暦年月Ｗ.
001519    03 西暦年Ｗ                        PIC 9(4) VALUE ZERO.
001520    03 西暦月Ｗ                        PIC 9(2) VALUE ZERO.
001521 01 和暦年月Ｗ.
001522    03 和暦和暦Ｗ                      PIC 9(1) VALUE ZERO.
001523    03 和暦年Ｗ                        PIC 9(2) VALUE ZERO.
001524    03 和暦月Ｗ                        PIC 9(2) VALUE ZERO.
001527*
       01 施術内容区分Ｗ                     PIC 9(2).
       01 初療和暦年月日Ｗ.
          05 初療和暦年月Ｗ.
             07 初療和暦Ｗ                   PIC 9 VALUE ZERO.
             07 初療年Ｗ                     PIC 9(2) VALUE ZERO.
             07 初療月Ｗ                     PIC 9(2) VALUE ZERO.
          05 初療日Ｗ                        PIC 9(2) VALUE ZERO.
001528************************************
001529* 今回前回同意期間算出(処理前保存) *
001530************************************
001551 01 保存連再同２−キー IS EXTERNAL.
001552*/ in
001553*   入力はＨレセプトＦの主キー
001554    03 保存連再同２−施術区分                PIC 9.
001555    03 保存連再同２−施術和暦年月.
001556       05 保存連再同２−施術和暦             PIC 9.
001557       05 保存連再同２−施術年月.
001558         07 保存連再同２−施術年             PIC 9(2).
001559         07 保存連再同２−施術月             PIC 9(2).
001560    03 保存連再同２−患者コード.
001561       05 保存連再同２−患者番号             PIC 9(6).
001562       05 保存連再同２−枝番                 PIC X.
001563    03 保存連再同２−レセ種別                PIC 9(2).
001564    03 保存連再同２−負傷主キー              PIC 9(8).
001565*
001566*/ out
001567    03 保存連再同２−今回同意開始和暦年月日.
001568       05 保存連再同２−今回同意開始和暦年月.
001569          07 保存連再同２−今回同意開始和暦  PIC 9.
001570          07 保存連再同２−今回同意開始年    PIC 9(2).
001571          07 保存連再同２−今回同意開始月    PIC 9(2).
001572       05 保存連再同２−今回同意開始日       PIC 9(2).
001573    03 保存連再同２−今回同意終了和暦年月日.
001574       05 保存連再同２−今回同意終了和暦年月.
001575          07 保存連再同２−今回同意終了和暦  PIC 9.
001576          07 保存連再同２−今回同意終了年    PIC 9(2).
001577          07 保存連再同２−今回同意終了月    PIC 9(2).
001578       05 保存連再同２−今回同意終了日       PIC 9(2).
001579    03 保存連再同２−前回同意開始和暦年月日.
001580       05 保存連再同２−前回同意開始和暦年月.
001581          07 保存連再同２−前回同意開始和暦  PIC 9.
001582          07 保存連再同２−前回同意開始年    PIC 9(2).
001583          07 保存連再同２−前回同意開始月    PIC 9(2).
001584       05 保存連再同２−前回同意開始日       PIC 9(2).
001585    03 保存連再同２−前回同意終了和暦年月日.
001586       05 保存連再同２−前回同意終了和暦年月.
001587          07 保存連再同２−前回同意終了和暦  PIC 9.
001588          07 保存連再同２−前回同意終了年    PIC 9(2).
001589          07 保存連再同２−前回同意終了月    PIC 9(2).
001590       05 保存連再同２−前回同意終了日       PIC 9(2).
001591*/  ( 1:変形徒手あり )
001592    03 保存連再同２−今回変形徒手フラグ      PIC 9(1).
001593    03 保存連再同２−前回変形徒手フラグ      PIC 9(1).
001594*
001487********************************
001488* 再同意予定日算出(処理前保存) *
001489********************************
001490 01 保存連再同−キー.
001491*/ in
001492    03 保存連再同−再同意和暦年月日      PIC 9(7).
001493    03 保存連再同−負傷主キー            PIC 9(8).
001494*
001495    03 保存連再同−施術区分              PIC 9(1).
001496    03 保存連再同−施術内容区分          PIC 9(2).
001497*/ out
001498    03 保存連再同−再同意予定和暦年月日.
001499       05 保存連再同−再同意予定和暦年月.
001500          07 保存連再同−再同意予定和暦  PIC 9.
001501          07 保存連再同−再同意予定年    PIC 9(2).
001502          07 保存連再同−再同意予定月    PIC 9(2).
001503       05 保存連再同−再同意予定日       PIC 9(2).
001504    03 保存連再同−初療和暦年月日.
001505       05 保存連再同−初療和暦年月.
001506          07 保存連再同−初療和暦        PIC 9.
001507          07 保存連再同−初療年          PIC 9(2).
001508          07 保存連再同−初療月          PIC 9(2).
001509       05 保存連再同−初療日             PIC 9(2).
001510*/  ( 1:変形徒手あり )
001511    03 保存連再同−変形徒手フラグ        PIC 9(1).
001512*
001595******************************************************************
001596*                          連結項目                              *
001597******************************************************************
001598******************************
001599* 前回施術報告書交付情報取得 *
001600******************************
001601 01 連前交−キー IS EXTERNAL.
001602*/ in
001603*   入力はＨレセプトＦの主キー
001604    03 連前交−施術区分                PIC 9.
001605    03 連前交−施術和暦年月.
001606       05 連前交−施術和暦             PIC 9.
001607       05 連前交−施術年月.
001608         07 連前交−施術年             PIC 9(2).
001609         07 連前交−施術月             PIC 9(2).
001610    03 連前交−患者コード.
001611       05 連前交−患者番号             PIC 9(6).
001612       05 連前交−枝番                 PIC X.
001613    03 連前交−レセ種別                PIC 9(2).
001614*
001615*/ out
001746    03 連前交−前回支給和暦年月.
001748       05 連前交−前回支給和暦         PIC 9.
001749       05 連前交−前回支給年           PIC 9(2).
001750       05 連前交−前回支給月           PIC 9(2).
001752    03 連前交−支給可能和暦年月.
001754       05 連前交−支給可能和暦         PIC 9.
001755       05 連前交−支給可能年           PIC 9(2).
001756       05 連前交−支給可能月           PIC 9(2).
001758*/  ( 1:変形徒手あり )
001760    03 連前交−変形徒手フラグ          PIC 9(1).
001770*
       01 連前交−キー−支給判定 IS EXTERNAL.
      *   交付料支給可能条件の判定理由
          03 連前交−支給判定                PIC X(50).
002139************************
002140* 今回前回同意期間算出 *
002141************************
002142 01 連再同２−キー IS EXTERNAL.
002143*/ in
002144*   入力はＨレセプトＦの主キー
002145    03 連再同２−施術区分                PIC 9.
002146    03 連再同２−施術和暦年月.
002147       05 連再同２−施術和暦             PIC 9.
002148       05 連再同２−施術年月.
002149         07 連再同２−施術年             PIC 9(2).
002150         07 連再同２−施術月             PIC 9(2).
002151    03 連再同２−患者コード.
002152       05 連再同２−患者番号             PIC 9(6).
002153       05 連再同２−枝番                 PIC X.
002154    03 連再同２−レセ種別                PIC 9(2).
002155    03 連再同２−負傷主キー              PIC 9(8).
002156*
002157*/ out
002158    03 連再同２−今回同意開始和暦年月日.
002159       05 連再同２−今回同意開始和暦年月.
002160          07 連再同２−今回同意開始和暦  PIC 9.
002161          07 連再同２−今回同意開始年    PIC 9(2).
002162          07 連再同２−今回同意開始月    PIC 9(2).
002163       05 連再同２−今回同意開始日       PIC 9(2).
002164    03 連再同２−今回同意終了和暦年月日.
002165       05 連再同２−今回同意終了和暦年月.
002166          07 連再同２−今回同意終了和暦  PIC 9.
002167          07 連再同２−今回同意終了年    PIC 9(2).
002168          07 連再同２−今回同意終了月    PIC 9(2).
002169       05 連再同２−今回同意終了日       PIC 9(2).
002170    03 連再同２−前回同意開始和暦年月日.
002171       05 連再同２−前回同意開始和暦年月.
002172          07 連再同２−前回同意開始和暦  PIC 9.
002173          07 連再同２−前回同意開始年    PIC 9(2).
002174          07 連再同２−前回同意開始月    PIC 9(2).
002175       05 連再同２−前回同意開始日       PIC 9(2).
002176    03 連再同２−前回同意終了和暦年月日.
002177       05 連再同２−前回同意終了和暦年月.
002178          07 連再同２−前回同意終了和暦  PIC 9.
002179          07 連再同２−前回同意終了年    PIC 9(2).
002180          07 連再同２−前回同意終了月    PIC 9(2).
002181       05 連再同２−前回同意終了日       PIC 9(2).
002182*/  ( 1:変形徒手あり )
002183    03 連再同２−今回変形徒手フラグ      PIC 9(1).
002184    03 連再同２−前回変形徒手フラグ      PIC 9(1).
002185*
002113********************
002114* 再同意予定日算出 *
002115********************
002116 01 連再同−キー IS EXTERNAL.
002117*/ in
002118    03 連再同−再同意和暦年月日      PIC 9(7).
002119    03 連再同−負傷主キー            PIC 9(8).
002120*
002121    03 連再同−施術区分              PIC 9(1).
002122    03 連再同−施術内容区分          PIC 9(2).
002123*/ out
002124    03 連再同−再同意予定和暦年月日.
002125       05 連再同−再同意予定和暦年月.
002126          07 連再同−再同意予定和暦  PIC 9.
002127          07 連再同−再同意予定年    PIC 9(2).
002128          07 連再同−再同意予定月    PIC 9(2).
002129       05 連再同−再同意予定日       PIC 9(2).
002130    03 連再同−初療和暦年月日.
002131       05 連再同−初療和暦年月.
002132          07 連再同−初療和暦        PIC 9.
002133          07 連再同−初療年          PIC 9(2).
002134          07 連再同−初療月          PIC 9(2).
002135       05 連再同−初療日             PIC 9(2).
002136*/  ( 1:変形徒手あり )
002137    03 連再同−変形徒手フラグ        PIC 9(1).
002138*
002186******************************************************************
002187*                      PROCEDURE  DIVISION                       *
002188******************************************************************
002189 PROCEDURE               DIVISION.
002190************
002191*           *
002192* 初期処理   *
002193*           *
002194************
002200     PERFORM ファイルオープン.
002210     PERFORM 連結項目待避.
007740*------------------------------------------------------*
007750*   / Ｈ制御情報より /
007760     MOVE ZERO TO Ｈ制−制御区分.
007770     READ Ｈ制御情報マスタ
007780     NOT INVALID KEY
007790         MOVE Ｈ制−施術報告書交付料手動算定 TO 施術報告書交付料手動算定Ｗ
007800     END-READ.
007810*------------------------------------------------------*
002223************
002230*           *
002240* 主処理     *
002250*           *
002260************
002302*
002303*    返却値クリア
002304     INITIALIZE 連前交−前回支給和暦年月.
002305     INITIALIZE 連前交−支給可能和暦年月.
           INITIALIZE 連前交−支給判定
002313
002314*    当月の同意期間を取得
002315     PERFORM 当月データ取得.
002316*
002317*    前回交付年月を取得
002326     PERFORM 前回交付年月取得.
002766
002767*    変形徒手矯正術フラグセット
002768     MOVE 連再同２−今回変形徒手フラグ       TO 連前交−変形徒手フラグ.
002769
002770*    変形徒手以外は６ヶ月間隔で交付
002771     IF 連前交−変形徒手フラグ = ZERO
002772*        前回支給があるか？
002773         IF 連前交−前回支給和暦年月 = ZERO
002782*            前回支給が無いので初回の同意期間終了年月をセット
      *            (今まで取っていない場合、初回同意からから複数回再同意があっても取れる様にする)
                   PERFORM 初回同意終了日取得
                   IF 連再同−再同意予定和暦年月 >= "43010"
                        MOVE 連再同−再同意予定和暦年月      TO 連前交−支給可能和暦年月
                   ELSE
                        MOVE "43010"                         TO 連前交−支給可能和暦年月
                   END-IF
                   MOVE "初回同意期間"                       TO 連前交−支給判定
002785         ELSE
002786*            前回交付＋5ヶ月(6ヶ月)を返却
002787             MOVE 連前交−前回支給和暦 TO 元号区分Ｗ
002788             PERFORM 西暦変換処理
002789             ADD  連前交−前回支給年   TO 西暦年Ｗ 
002790             MOVE 連前交−前回支給月   TO 西暦月Ｗ
002791             IF 連再同２−今回同意開始日 <= 15
002792                 ADD  5                TO 西暦月Ｗ
002793             ELSE
002794                 ADD  6                TO 西暦月Ｗ
002795             END-IF
002796             IF 西暦月Ｗ > 12
002797                 COMPUTE 西暦年Ｗ = 西暦年Ｗ + 1
002798                 COMPUTE 西暦月Ｗ = 西暦月Ｗ - 12
002799             END-IF
002800             PERFORM 元号変換処理
002803             MOVE 和暦年月Ｗ           TO 連前交−支給可能和暦年月
                   STRING "前回支給("                     DELIMITED BY SIZE
                          連前交−前回支給年              DELIMITED BY SIZE
                          "/"                             DELIMITED BY SIZE
                          連前交−前回支給月              DELIMITED BY SIZE
                          ")"                             DELIMITED BY SIZE
                      INTO 連前交−支給判定
                   END-STRING
002805         END-IF
002807     ELSE
002808*       変形徒手は毎月交付なのでクリアする
002809        MOVE ZERO                      TO 連前交−支給可能和暦年月
              MOVE "変形徒手、毎月請求可能"  TO 連前交−支給判定
002810     END-IF
      *
      *    同一患者同一月内で枝番違いの交付料請求があるかチェック
           PERFORM 当月枝番違い交付チェック.

      *    施術報告書交付料を手動算定する場合、必ず査定できるように支給可能和暦年月をクリアする
           IF 施術報告書交付料手動算定Ｗ = 1
              MOVE ZERO                      TO 連前交−支給可能和暦年月
           END-IF.
002812*
002813************
002814*           *
002815* 終了処理   *
002816*           *
002817************
002831     PERFORM 終了処理.
002840
002841*    HMSAIDOの値を復元(他処理で直接共通変数を読んでいてもおかしくならない様にする為)
002842     MOVE 保存連再同−キー             TO 連再同−キー.
002842     MOVE 保存連再同２−キー           TO 連再同２−キー.
002843
002844     MOVE ZERO TO PROGRAM-STATUS.
002851     EXIT PROGRAM.
002860*
002870*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
002880*================================================================*
002890 ファイルオープン SECTION.
002900*================================================================*
003000     OPEN INPUT ＨレセプトＦ.
003010         MOVE NC"ＨレセプトＦ" TO ファイル名.
003020         PERFORM オープンチェック.
003030     OPEN INPUT 元号マスタ.
003031         MOVE NC"元号" TO ファイル名.
003032         PERFORM オープンチェック.
           OPEN INPUT Ｈ制御情報マスタ.
               MOVE NC"Ｈ制御" TO ファイル名.
               PERFORM オープンチェック.
           OPEN INPUT 受診者情報Ｆ.
               MOVE NC"受診者" TO ファイル名.
               PERFORM オープンチェック.
           OPEN INPUT Ｈ負傷データＦ.
               MOVE NC"Ｈ負傷" TO ファイル名.
               PERFORM オープンチェック.
003045*
003046*================================================================*
003050 オープンチェック SECTION.
003060*
003070     IF ( 状態キー  NOT =  "00" )
003080         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
003090         DISPLAY NC"状態キー：" 状態キー         UPON CONS
003100         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
003110                                                 UPON CONS
003120*-----------------------------------------*
003130         CALL "actcshm"  WITH C LINKAGE
003140*-----------------------------------------*
003150         ACCEPT  キー入力 FROM CONS
003160         PERFORM ファイル閉鎖
003170         MOVE 99 TO PROGRAM-STATUS
003180         EXIT PROGRAM.
003190*
003200*================================================================*
003210 連結項目待避 SECTION.
003220*================================================================*
003264*    HMSAIDOの値を保存
003265     MOVE 連再同−キー               TO 保存連再同−キー. 
003265     MOVE 連再同２−キー             TO 保存連再同２−キー. 
003266*
003267*    HMZKOFUの値を保存
003268     MOVE 連前交−施術区分       TO 施術区分ＷＲ.
003269     MOVE 連前交−施術和暦年月   TO 施術和暦年月ＷＲ.
003280     MOVE 連前交−患者コード     TO 患者コードＷＲ.
003290     MOVE 連前交−レセ種別       TO レセ種別ＷＲ.
003292*
003300*================================================================*
003310 当月データ取得 SECTION.
003320*================================================================*
003330     INITIALIZE Ｈレセ−レコード.
003332     MOVE 施術区分ＷＲ             TO Ｈレセ−施術区分.
003333     MOVE 施術和暦年月ＷＲ         TO Ｈレセ−施術和暦年月.
003334     MOVE 患者コードＷＲ           TO Ｈレセ−患者コード.
003335     MOVE レセ種別ＷＲ             TO Ｈレセ−レセ種別.
003340*
003350     READ ＨレセプトＦ
003360     INVALID KEY
003371          MOVE SPACE TO Ｈレセ−レコード
003380          INITIALIZE Ｈレセ−レコード
003390     END-READ.
003428*
003429     MOVE Ｈレセ−負傷主キー     TO 負傷主キーＷ.
           MOVE Ｈレセ−施術内容区分   TO 施術内容区分Ｗ.
      
           INITIALIZE Ｈ負−レコード.
           MOVE 負傷主キーＷ           TO Ｈ負−主キー.
           READ Ｈ負傷データＦ
           INVALID KEY
               INITIALIZE Ｈ負−レコード
           END-READ.
           MOVE Ｈ負−初療和暦年月日   TO 初療和暦年月日Ｗ.
003430*
003431*    当月の同意有効期限を取得
003473     INITIALIZE 連再同２−キー.
003474     MOVE Ｈレセ−施術区分       TO 連再同２−施術区分.
003475     MOVE Ｈレセ−施術和暦年月   TO 連再同２−施術和暦年月.
003476     MOVE Ｈレセ−患者コード     TO 連再同２−患者コード.
003477     MOVE Ｈレセ−レセ種別       TO 連再同２−レセ種別.
003478     MOVE Ｈレセ−負傷主キー     TO 連再同２−負傷主キー.
003479*
003480     MOVE "HMSAIDO2" TO プログラム名Ｗ.
003481     CALL プログラム名Ｗ.
003482     CANCEL プログラム名Ｗ.
003483*
003505*================================================================*
003506 前回交付年月取得 SECTION.
003507*================================================================*
003508     INITIALIZE Ｈレセ−レコード.
003509     MOVE 施術区分ＷＲ                 TO Ｈレセ−施術区分.
003510     MOVE 施術和暦年月ＷＲ             TO Ｈレセ−施術和暦年月.
003511**     MOVE 患者コードＷＲ               TO Ｈレセ−患者コード.
      *    前月以前を検索する様に枝番をクリアする。同月内で枝番あり・なし両方とも算定可能にあってしまう
           MOVE 患者番号ＷＲ                 TO Ｈレセ−患者番号.
           MOVE SPACE                        TO Ｈレセ−枝番.
003512     MOVE ZERO                         TO Ｈレセ−レセ種別.
      */生保時のみ枝番指定/20240314
           IF レセ種別ＷＲ = 07 MOVE 枝番ＷＲ TO Ｈレセ−枝番 END-IF.
003513*
003514     START ＨレセプトＦ     KEY IS <  Ｈレセ−施術区分
003515                                      Ｈレセ−患者コード
003516                                      Ｈレセ−施術和暦年月
003517                                      Ｈレセ−レセ種別
003518                                      REVERSED
003519     END-START.
003520     IF 状態キー = "00"
003521        MOVE SPACE TO 終了フラグ
003522        PERFORM ＨレセプトＦ読込
003523*       繰り返し（同一患者番号６けたで同一負傷主キー　枝番考慮）
003524        PERFORM UNTIL ( Ｈレセ−施術区分        NOT = 施術区分ＷＲ           ) OR
003525                      ( Ｈレセ−患者番号        NOT = 患者番号ＷＲ           ) OR
003526                      ( Ｈレセ−負傷主キー      NOT = 負傷主キーＷ           ) OR
003527                      ( 終了フラグ              NOT = SPACE                  )
003528*           交付料を請求した月か？
003529            IF Ｈレセ−施術報告書交付料 NOT = ZERO
003530*               前回交付年月をセット
003531                MOVE Ｈレセ−施術和暦年月     TO 連前交−前回支給和暦年月
003532                MOVE "YES"                    TO 終了フラグ
003555            END-IF
003556            PERFORM ＨレセプトＦ読込
003557        END-PERFORM.
003558*
003559*================================================================*
003560 ＨレセプトＦ読込 SECTION.
003561*
003562     READ ＨレセプトＦ NEXT
003563     AT END
003564        MOVE "YES" TO 終了フラグ
003565        INITIALIZE Ｈレセ−レコード
003566     NOT AT END
003567        PERFORM UNTIL (Ｈレセ−レセ種別 NOT = 3) OR (終了フラグ NOT = SPACE)
003568           READ ＨレセプトＦ NEXT
003569           AT END
003570              MOVE "YES" TO 終了フラグ
003571              INITIALIZE Ｈレセ−レコード
003572           END-READ
003573        END-PERFORM
003574     END-READ.
      *    
      *    受診者情報Ｆを読み込む
           IF 終了フラグ NOT = SPACE
              INITIALIZE 受−レコード
              MOVE Ｈレセ−施術和暦     TO 受−施術和暦
              MOVE Ｈレセ−施術年       TO 受−施術年
              MOVE Ｈレセ−施術月       TO 受−施術月
              MOVE Ｈレセ−患者番号     TO 受−患者番号
              MOVE Ｈレセ−枝番         TO 受−枝番
              READ 受診者情報Ｆ
              INVALID KEY
                 INITIALIZE 受−レコード
              END-READ
           END-IF.
003575*
003505*================================================================*
003506 当月枝番違い交付チェック SECTION.
003507*================================================================*
003508     INITIALIZE Ｈレセ−レコード.
003509     MOVE 施術区分ＷＲ                 TO Ｈレセ−施術区分.
003510     MOVE 施術和暦年月ＷＲ             TO Ｈレセ−施術和暦年月.
           MOVE 患者番号ＷＲ                 TO Ｈレセ−患者番号.
           MOVE SPACE                        TO Ｈレセ−枝番.
003512     MOVE ZERO                         TO Ｈレセ−レセ種別.
003513*
003514     START ＨレセプトＦ     KEY IS >= Ｈレセ−施術区分
003515                                      Ｈレセ−施術和暦年月
003516                                      Ｈレセ−患者コード
003517                                      Ｈレセ−レセ種別
003519     END-START.
003520     IF 状態キー = "00"
003521        MOVE SPACE TO 終了フラグ
003522        PERFORM ＨレセプトＦ読込
003523*       繰り返し（同一患者番号６けたで同一負傷主キー　枝番考慮）
003524        PERFORM UNTIL ( Ｈレセ−施術区分        NOT = 施術区分ＷＲ           ) OR
003525                      ( Ｈレセ−施術和暦年月    NOT = 施術和暦年月ＷＲ       ) OR
003527                      ( 終了フラグ              NOT = SPACE                  )
003528*           同じ患者で枝番違いの交付料請求があるか？
003529            IF (Ｈレセ−施術報告書交付料 NOT = ZERO)         AND 
                     (Ｈレセ−施術区分             = 施術区分ＷＲ) AND
                     ( Ｈレセ−施術和暦年月        = 施術和暦年月ＷＲ) AND
                     (Ｈレセ−患者番号             = 患者番号ＷＲ) AND
                     (Ｈレセ−枝番             NOT = 枝番ＷＲ)     AND
                     (Ｈレセ−負傷主キー           = 負傷主キーＷ)
003530*               請求できない様に前回交付年月をセット
002809                MOVE 99999                    TO 連前交−支給可能和暦年月
                      MOVE "枝番違い請求あり"       TO 連前交−支給判定
003532                MOVE "YES"                    TO 終了フラグ
003555            END-IF
003556            PERFORM ＨレセプトＦ読込
003557        END-PERFORM
           END-IF.
003558*
003300*================================================================*
003310 初回同意終了日取得 SECTION.
003320*================================================================*
003410     INITIALIZE 連再同−キー.
003411     MOVE 初療和暦年月日Ｗ         TO 連再同−再同意和暦年月日.
003413     MOVE 負傷主キーＷ             TO 連再同−負傷主キー.
003414     MOVE 施術区分ＷＲ             TO 連再同−施術区分.
003415     MOVE 施術内容区分Ｗ           TO 連再同−施術内容区分.
003417*
003418     MOVE "HMSAIDO" TO プログラム名Ｗ.
003419     CALL プログラム名Ｗ.
003420     CANCEL プログラム名Ｗ.
003483*
003576*================================================================*
003577 西暦変換処理 SECTION.
003578*
003579     INITIALIZE 元−レコード.
003580     MOVE 元号区分Ｗ                           TO 元−元号区分.
003581     READ 元号マスタ
003582     INVALID KEY
003583         MOVE ZERO                             TO 西暦年Ｗ
003584     NOT INVALID KEY
003585         COMPUTE 西暦年Ｗ = 元−開始西暦年 - 1
003586     END-READ.
003587*
003588*================================================================*
003589 元号変換処理 SECTION.
003590*
003591     INITIALIZE 元−レコード.
003592     MOVE 9                        TO 元−元号区分.
003593     START 元号マスタ       KEY IS <= 元−元号区分
003594                                      REVERSED
003595     END-START.
003596     IF 状態キー = "00"
003597        MOVE SPACE TO 終了フラグ
003598        PERFORM 元号マスタ読込
003599*       繰り返し
003600        PERFORM UNTIL ( 終了フラグ NOT = SPACE)
003601*           元号を判定
003602            IF 元−開始西暦年月 <= 西暦年月Ｗ
003603*               元号と和暦をセット
003604                MOVE 元−元号区分             TO 和暦和暦Ｗ
003605                COMPUTE 和暦年Ｗ = 西暦年Ｗ - 元−開始西暦年 + 1
003606                COMPUTE 和暦月Ｗ = 西暦月Ｗ
003607                MOVE "YES"                    TO 終了フラグ
003614            END-IF
003615            PERFORM 元号マスタ読込
003616        END-PERFORM
           END-IF.
003617*
003629*================================================================*
003630 元号マスタ読込 SECTION.
003631*
003632     READ 元号マスタ NEXT
003633     AT END
003634        MOVE "YES" TO 終了フラグ
003635        INITIALIZE 元−レコード
003643     END-READ.
003645*
003646*================================================================*
005050*================================================================*
005060 終了処理 SECTION.
005070*================================================================*
005080     PERFORM ファイル閉鎖.
005090*
005100*================================================================*
005110 ファイル閉鎖 SECTION.
005120*
005130     CLOSE ＨレセプトＦ.
005131     CLOSE 元号マスタ.
           CLOSE Ｈ制御情報マスタ.
           CLOSE 受診者情報Ｆ.
           CLOSE Ｈ負傷データＦ.
005140*
005150*================================================================*
005160******************************************************************
005170 END PROGRAM HMZKOFU.
005180******************************************************************
005190
