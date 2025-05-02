      ******************************************************************
      *            IDENTIFICATION      DIVISION                        *
      ******************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             MSG004.
       AUTHOR.                 山田  浩之　池田　幸子
      *
      *----------------------------------------------------------------*
      *         メッセージ出力４　（柔）
      *         MED = MSG001G
      *         画面定義体をPowerCOBOLに変更 2009.5
      *----------------------------------------------------------------*
       DATE-WRITTEN.           1997-12-21
       DATE-COMPILED.          1997-12-21
      *----------------------------------------------------------------*
      ******************************************************************
      *            ENVIRONMENT         DIVISION                        *
      ******************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
       OBJECT-COMPUTER.        FMV-DESKPOWER.
       SPECIAL-NAMES.          CONSOLE  IS  CONS
                               SYSERR   IS  MSGBOX.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *     SELECT  表示ファイル    ASSIGN      TO     GS-DSPFMSG4
      *                             SYMBOLIC    DESTINATION  IS "DSP"
      *                             FORMAT                   IS  定義体名
      *                             GROUP                    IS  項目群名
      *                             PROCESSING  MODE         IS  処理種別
      *                             SELECTED    FUNCTION     IS  アテンション
      *                             UNIT        CONTROL      IS  拡張制御
      *                             FILE        STATUS       IS  通知情報.
      *     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
      *                             ORGANIZATION             IS  INDEXED
      *                             ACCESS MODE              IS  DYNAMIC
      *                             RECORD KEY               IS  制－制御区分
      *                             FILE STATUS              IS  状態キー
      *                             LOCK        MODE         IS  AUTOMATIC.
      ******************************************************************
      *                      DATA DIVISION                             *
      ******************************************************************
       DATA                    DIVISION.
       FILE                    SECTION.
      * FD  表示ファイル.
      *     COPY MSG001G        OF  XMDLIB.
      *                           ［ＲＬ＝  ２５６］
      * FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
      *     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
      *
      *----------------------------------------------------------------*
      ******************************************************************
      *                WORKING-STORAGE SECTION                         *
      ******************************************************************
       WORKING-STORAGE         SECTION.
       01 キー入力                           PIC X    VALUE SPACE.
       01 状態キー                           PIC X(2) VALUE SPACE.
       01 終了フラグ                         PIC X(3) VALUE SPACE.
       01 終了フラグ２                       PIC X(3) VALUE SPACE.
       01 終了フラグ３                       PIC X(3) VALUE SPACE.
       01 終了フラグ４                       PIC X(3) VALUE SPACE.
       01 遅延フラグ                         PIC X(3) VALUE SPACE.
       01 書込フラグ                         PIC X(4) VALUE SPACE.
       01 請求フラグ                         PIC X(4) VALUE SPACE.
       01 書込許可フラグ                     PIC X(3) VALUE SPACE.
       01 年月フラグ                         PIC X(3) VALUE SPACE.
       01 新規フラグ                         PIC X(3) VALUE SPACE.
       01 追加フラグ                         PIC X(3) VALUE SPACE.
       01 修正フラグ                         PIC X(3) VALUE SPACE.
       01 移動フラグ                         PIC X(3) VALUE SPACE.
       01 削除フラグ                         PIC X(3) VALUE SPACE.
       01 行桁フラグ                         PIC X(3) VALUE SPACE.
       01 保守フラグ                         PIC X(3) VALUE SPACE.
       01 存在フラグ                         PIC X(3) VALUE SPACE.
       01 読込フラグ                         PIC X(3) VALUE SPACE.
       01 取得フラグ                         PIC X(3) VALUE SPACE.
       01 同期フラグ                         PIC X(3) VALUE SPACE.
       01 終了行フラグ                       PIC X(3) VALUE SPACE.
       01 重複フラグ                         PIC X(3) VALUE SPACE.
       01 初検生成フラグ                     PIC X(3) VALUE SPACE.
       01 中断フラグ                         PIC S9(9) COMP-5.
       01 保険区分Ｗ                         PIC 9(2) VALUE ZERO.
       01 保険者番号Ｗ                       PIC X(10) VALUE SPACE.
       01 市町村番号老人Ｗ                   PIC X(10) VALUE SPACE.
       01 市町村番号助成Ｗ                   PIC X(10) VALUE SPACE.
       01 保険者番号Ｗ２                     PIC X(10) VALUE SPACE.
       01 ファイル名                         PIC N(2) VALUE SPACE.
       01 エラー内容Ｗ                       PIC N(18) VALUE SPACE.
       01 エラー内容２Ｗ                     PIC N(18) VALUE SPACE.
       01 画面名称Ｗ                         PIC N(2) VALUE SPACE.
       01 遅延回数Ｗ                         PIC 9(4) VALUE ZERO.
       01 遅延ＣＮＴ                         PIC 9(5) VALUE ZERO.
       01 行ＣＮＴ                           PIC 9(2) VALUE ZERO.
       01 負傷ＣＮＴ                         PIC 9(2) VALUE ZERO.
       01 部位重複ＣＮＴ                     PIC 9(2) VALUE ZERO.
       01 部位カウンタ                       PIC 9(2) VALUE ZERO.
       01 日毎部位数Ｗ                       PIC 9 VALUE ZERO.
       01 部位名称Ｗ                         PIC N(12) VALUE SPACE.
       01 部位長Ｗ                           PIC 9(2) VALUE 1.
       01 実日数Ｗ                           PIC 9(2) VALUE ZERO.
      *
       01 部位コードＷ.
         03 負傷種別Ｗ                       PIC 9(2).
         03 部位Ｗ                           PIC 9(2).
         03 左右区分Ｗ                       PIC 9.
         03 負傷位置番号Ｗ                   PIC 9(2).
      *
      * 部位関連変数
       01 最大登録数Ｗ                       PIC 9 VALUE ZERO.
       01 部位数Ｗ                           PIC 9 VALUE ZERO.
       01 追加部位数Ｗ                       PIC 9(2) VALUE ZERO.
       01 部位位置Ｗ                         PIC 9(2) VALUE ZERO.
       01 移動元Ｗ                           PIC 9(2) VALUE ZERO.
       01 移動先Ｗ                           PIC 9(2) VALUE ZERO.
       01 移動先Ｗ２                         PIC 9(2) VALUE ZERO.
       01 部位ＣＮＴ                         PIC 9(2) VALUE ZERO.
       01 部位ＣＮＴ２                       PIC 9(2) VALUE ZERO.
       01 部位ＣＮＴＷ                       PIC 9(2) VALUE ZERO.
      *
       01 請求金額Ｗ                         PIC 9(6).
       01 差引請求金額Ｗ                     PIC 9(6).
       01 行カウンタ                         PIC 9(2) VALUE ZERO.
       01 選択行位置Ｗ                       PIC 9(2) VALUE ZERO.
       01 請求移動キー                       PIC X(4) VALUE SPACE.
       01 処理移動キー                       PIC X(4) VALUE SPACE.
       01 患者カナＷ                         PIC X(20).
       01 会員名Ｗ                           PIC N(12).
       01 接骨院名Ｗ                         PIC N(10).
       01 使用モードＷ                       PIC 9.
       01 退避保険区分Ｗ                     PIC 9(2)  VALUE ZERO.
       01 退避保険者番号Ｗ                   PIC X(10) VALUE SPACE.
       01 前和暦Ｗ                           PIC 9 VALUE ZERO.
       01 カレント和暦Ｗ                     PIC 9 VALUE ZERO.
       01 保険種別Ｗ                         PIC 9(2) VALUE ZERO.
       01 公費種別Ｗ                         PIC 9(2) VALUE ZERO.
       01 助成種別Ｗ                         PIC 9(2) VALUE ZERO.
       01 患者コードＷ.
          03 患者番号Ｗ                      PIC 9(6).
          03 枝番Ｗ                          PIC X.
      *
       01 患者コードＣＷ.
          03 患者番号ＣＷ                    PIC 9(6).
          03 枝番ＣＷ                        PIC X.
      *
       01 負傷連続登録Ｗ                     PIC 9 VALUE ZERO.
      *
       01 負傷時Ｗ                           PIC X(2).
       01 負傷時Ｒ REDEFINES 負傷時Ｗ.
          03 負傷時ＷＺ                      PIC 9(2).
      *
       01 負傷分Ｗ                           PIC X(2).
       01 負傷分Ｒ REDEFINES 負傷分Ｗ.
          03 負傷分ＷＺ                      PIC 9(2).
      *
       01 元号名称Ｗ.
          03 元号名称Ｗ１                    PIC N(2).
          03 FILLER                          PIC N(4).
      *
       01 負傷番号Ｗ                         PIC 9.
       01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
          03 負傷番号Ｗ１                    PIC X.
      *
       01 全角負傷番号Ｗ                     PIC N.
       01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
          03 全角負傷番号Ｗ１                PIC X(2).
      *
       01 件数Ｗ                             PIC 9(6).
       01 頁数Ｗ                             PIC 9(4).
       01 総頁数Ｗ                           PIC 9(4).
       01 剰余Ｗ                             PIC 9(2).
      *
       01 ＦＤ追加フラグ                     PIC X(3) VALUE SPACE.
      *
       01 画面退避ワーク.
         03 画面退避ＧＷ OCCURS 9.
           05 負傷時期ＧＷ                       PIC 9(2).
           05 負傷場所ＧＷ                       PIC 9(2).
           05 負傷状況ＧＷ                       PIC 9(2).
           05 負傷部位ＧＷ                       PIC 9(2).
           05 負傷動作ＧＷ                       PIC 9(2).
           05 経過コードＧＷ                     PIC 9(4).
      *
       01 部位数ＴＷ                             PIC 9.
       01 負傷部位ＴＷ.
           03 負傷部位ＴＷ       OCCURS 9.
              05  負傷種別ＴＷ                   PIC 9(2).
              05  部位ＴＷ                       PIC 9(2).
              05  左右区分ＴＷ                   PIC 9.
              05  負傷位置番号ＴＷ               PIC 9(2).
              05  負傷和暦ＴＷ                   PIC 9.
              05  負傷年ＴＷ                     PIC 9(2).
              05  負傷月ＴＷ                     PIC 9(2).
              05  負傷日ＴＷ                     PIC 9(2).
              05  負傷時ＴＷ                     PIC 9(2).
              05  負傷分ＴＷ                     PIC 9(2).
              05  開始和暦ＴＷ                   PIC 9.
              05  開始年ＴＷ                     PIC 9(2).
              05  開始月ＴＷ                     PIC 9(2).
              05  開始日ＴＷ                     PIC 9(2).
              05  負傷時期ＴＷ                   PIC 9(2).    
              05  負傷場所ＴＷ                   PIC 9(2).    
              05  負傷状況ＴＷ                   PIC 9(2).    
              05  負傷部位ＴＷ                   PIC 9(2).    
              05  負傷動作ＴＷ                   PIC 9(2).    
              05  経過コードＴＷ                 PIC 9(4).    
              05  終了和暦ＴＷ                   PIC 9.
              05  終了年ＴＷ                     PIC 9(2).
              05  終了月ＴＷ                     PIC 9(2).
              05  終了日ＴＷ                     PIC 9(2).
              05  転帰区分ＴＷ                   PIC 9.
              05  算定区分ＴＷ                   PIC 9.
              05  電療請求区分ＴＷ               PIC 9.
      *
      * 元情報比較用
       01 負傷部位ＴＷ２.
           03 負傷部位ＴＷ２       OCCURS 9.
              05  負傷種別ＴＷ２                 PIC 9(2).
              05  部位ＴＷ２                     PIC 9(2).
              05  左右区分ＴＷ２                 PIC 9.
              05  負傷位置番号ＴＷ２             PIC 9(2).
              05  負傷和暦ＴＷ２                 PIC 9.
              05  負傷年ＴＷ２                   PIC 9(2).
              05  負傷月ＴＷ２                   PIC 9(2).
              05  負傷日ＴＷ２                   PIC 9(2).
              05  負傷時ＴＷ２                   PIC 9(2).
              05  負傷分ＴＷ２                   PIC 9(2).
              05  開始和暦ＴＷ２                 PIC 9.
              05  開始年ＴＷ２                   PIC 9(2).
              05  開始月ＴＷ２                   PIC 9(2).
              05  開始日ＴＷ２                   PIC 9(2).
              05  負傷時期ＴＷ２                 PIC 9(2).    
              05  負傷場所ＴＷ２                 PIC 9(2).    
              05  負傷状況ＴＷ２                 PIC 9(2).    
              05  負傷部位ＴＷ２                 PIC 9(2).    
              05  負傷動作ＴＷ２                 PIC 9(2).    
              05  経過コードＴＷ２               PIC 9(4).    
              05  終了和暦ＴＷ２                 PIC 9.
              05  終了年ＴＷ２                   PIC 9(2).
              05  終了月ＴＷ２                   PIC 9(2).
              05  終了日ＴＷ２                   PIC 9(2).
              05  転帰区分ＴＷ２                 PIC 9.
              05  算定区分ＴＷ２                 PIC 9.
              05  電療請求区分ＴＷ２             PIC 9.
      *
      * 移動退避用
       01 施術記録部位ＭＷ.
           03 施記部位ＭＷ       OCCURS 9.
             07 部位コードＭＷ.
               09 負傷種別ＭＷ                   PIC 9(2).
               09 部位ＭＷ                       PIC 9(2).
               09 左右区分ＭＷ                   PIC 9.
               09 負傷位置番号ＭＷ               PIC 9(2).
             07 整復施療区分ＭＷ                 PIC 9.
             07 罨法区分ＭＷ                     PIC 9.
             07 電療区分ＭＷ                     PIC 9.
             07 医師拘縮区分ＭＷ                 PIC 9.
             07 金属副子区分ＭＷ                 PIC 9.
             07 指導管理区分ＭＷ                 PIC 9.
             07 特別材料区分ＭＷ                 PIC 9.
             07 包帯交換区分ＭＷ                 PIC 9.
             07 情報提供区分ＭＷ                 PIC 9.
             07 転帰区分ＭＷ                     PIC 9.
             07 経過コードＭＷ                   PIC 9(4).
      *
      *
      * 費用負担者番号（市町村番号）ＷＯＲＫ
       01 費用負担者番号Ｗ.
           03  法別番号Ｗ                    PIC X(2).
           03  助保険Ｗ.
                05  府県Ｗ                   PIC X(2).
                05  助番Ｗ                   PIC X     OCCURS 3.
                05  助ＣＤＷ                 PIC X.
           03  FILLER                        PIC X(2).
      *
      *
       01 会員名ＳＷ                         PIC N(20).
       01 会員名ＳＷＲ   REDEFINES 会員名ＳＷ.
           03 会員名Ｗ１                     PIC N(11).
           03 会員名Ｗ２                     PIC N(9).
       01 保険者名称Ｗ.
           03 保険者名称Ｗ１                 PIC X(24).
           03 保険者名称Ｗ２                 PIC X(16).
      *
       01 一覧年月Ｗ.
           03 一覧年Ｗ                       PIC 9(2) VALUE 0.
           03 一覧月Ｗ                       PIC 9(2) VALUE 0.
      *
       01 加算年月Ｗ.
           03 加算年Ｗ                       PIC 9(2).
           03 加算月Ｗ                       PIC 9(2).
      *
       01 画面先頭キーＷ.
           03 年月ＴＷ.
              05 和暦ＴＷ                    PIC 9.
              05 年ＴＷ                      PIC 9(2).
              05 月ＴＷ                      PIC 9(2).
           03 会員番号ＴＷ                   PIC X(7).
      *
       01 開始和暦年月日Ｗ.
           03 開始和暦Ｗ                     PIC 9.
           03 開始年月Ｗ.
              05 開始年Ｗ                    PIC 9(2).
              05 開始月Ｗ                    PIC 9(2).
           03 開始日Ｗ                       PIC 9(2).
      *
       01 計算和暦年月Ｗ.
           03 計算和暦Ｗ                     PIC 9.
           03 計算年月Ｗ.
              05 計算年Ｗ                    PIC 9(2).
              05 計算月Ｗ                    PIC 9(2).
           03 計算日Ｗ                       PIC 9(2).
      *
       01 絶対値Ｗ                           PIC 9(6).
       01 絶対値月Ｗ                         PIC 9(3).
       01 絶対値月Ｗ２                       PIC 9(3).
       01 負傷経過日Ｗ                       PIC 9(3).
       01 西暦年Ｗ                           PIC 9(4).
       01 西暦年差Ｗ                         PIC 9(2).
       01 西暦年差２Ｗ                       PIC 9(2).
       01 閏年回数Ｗ                         PIC 9(2).
       01 通常年回数Ｗ                       PIC 9(2).
       01 通常年絶対値Ｗ                     PIC 9(6).
       01 閏年絶対値Ｗ                       PIC 9(6).
       01 年絶対値Ｗ                         PIC 9(6).
       01 負傷絶対値Ｗ                       PIC 9(6).
       01 開始絶対値Ｗ                       PIC 9(6).
      *
       01 冷罨日数骨折Ｗ                     PIC 9(2).
       01 冷罨日数脱臼Ｗ                     PIC 9(2).
       01 冷罨日数打撲Ｗ                     PIC 9(2).
       01 温罨待機骨折Ｗ                     PIC 9(2).
       01 温罨待機脱臼Ｗ                     PIC 9(2).
      *
       01 施術和暦年月Ｗ.
           03 施術和暦Ｗ                     PIC 9.
           03 施術年月Ｗ.
              05 施術年Ｗ                    PIC 9(2).
              05 施術月Ｗ                    PIC 9(2).
      *
       01 施術和暦年月日ＣＷ.
           03 施術和暦年月ＣＷ.
               05 施術和暦ＣＷ               PIC 9.
               05 施術年月ＣＷ.
                  07 施術年ＣＷ              PIC 9(2).
                  07 施術月ＣＷ              PIC 9(2).
           03 施術日ＣＷ                     PIC 9(2).
      *
       01 終了和暦年月日ＣＷ.
         03 終了和暦ＣＷ                     PIC 9.
         03 終了年月ＣＷ.
           05 終了年ＣＷ                     PIC 9(2).
           05 終了月ＣＷ                     PIC 9(2).
         03 終了日ＣＷ                       PIC 9(2).
      *
       01 画面キーＷ.
           03 画面キーＷＧ    OCCURS 20.
              05 和暦年月ＷＧ.
                 07 和暦ＷＧ                 PIC 9.
                 07 年月ＷＧ.
                    09 年ＷＧ                PIC 9(2).
                    09 月ＷＧ                PIC 9(2).
              05 会員番号ＷＧ                PIC X(5).
              05 患者コードＷＧ              PIC X(7).
      *
       01 画面制御.
           03 定義体名                       PIC X(8) VALUE SPACE.
           03 項目群名                       PIC X(8) VALUE SPACE.
           03 項目群名Ｒ  REDEFINES  項目群名.
               05 項目群Ｒ名                 PIC X(4).
               05 項目群番号                 PIC 9(2).
               05 FILLER                     PIC X(2).
           03 処理種別                       PIC X(2) VALUE SPACE.
           03 アテンション                   PIC X(4) VALUE SPACE.
           03 機能キー                       PIC X(4) VALUE SPACE.
           03 拡張制御.
               05 端末制御                   PIC X(4) VALUE SPACE.
               05 詳細制御                   PIC X(2) VALUE SPACE.
           03 通知情報                       PIC X(2) VALUE SPACE.
           03 ユニット名.
               05 FILLER                     PIC X(2)  VALUE SPACE.
               05 端末番号                   PIC 9(3)  VALUE ZERO.
               05 FILLER                     PIC X(4)  VALUE SPACE.
           03 メッセージＷ                   PIC N(20) VALUE SPACE.
      * 日付ＷＯＲＫ
       01 和暦終了年Ｗ                       PIC 9(4) VALUE ZERO.
       01 計算機和暦年Ｗ                     PIC 9(2) VALUE ZERO.
      *
       01 計算機西暦.
          03 計算機西暦年                    PIC 9(4).
          03 計算機西暦月日                  PIC 9(4).
       01 計算機西暦Ｒ REDEFINES 計算機西暦.
          03 計算機世紀                      PIC 9(2).
          03 計算機日付                      PIC 9(6).
          03 計算機日付Ｒ REDEFINES 計算機日付.
             05 計算機年月                   PIC 9(4).
             05 計算機年月Ｒ REDEFINES 計算機年月.
               07 計算機年                   PIC 9(2).
               07 計算機月                   PIC 9(2).
             05 計算機日                     PIC 9(2).
      *
       01 請求年月ＨＷ.
           03 請求和暦ＨＷ                   PIC 9.
           03 請求年ＨＷ                     PIC 9(2).
           03 請求月ＨＷ                     PIC 9(2).
001830*
001831* POWER COBOL用
001832 01 dll-name  PIC X(260)  VALUE SPACE.
001833 01 form-name PIC X(14)   VALUE SPACE.
      *
      *****************************************************************
      *                          連結項目                             *
      *****************************************************************
      *
       01 連メ４－キー IS EXTERNAL.
          03  連メ４－メッセージ             PIC N(20).
      *
       01 連メッセージＰ情報 IS EXTERNAL.
          03 連メＰ－メッセージ番号                PIC 9(2).
          03 連メＰ－メッセージ.
             05 連メＰ－メッセージ内容             PIC X(40) OCCURS 6.
          03 連メＰ－メッセージ１                  PIC X(20).
          03 連メＰ－メッセージ２                  PIC X(12).
      *
      ******************************************************************
      *                      PROCEDURE  DIVISION                       *
      ******************************************************************
       PROCEDURE               DIVISION.
      ************
      *           *
      * 初期処理   *
      *           *
      ************
      *     PERFORM 初期化.
      *     PERFORM 制御情報取得.
      ************
      *           *
      * 主処理     *
      *           *
      ************
      * 履歴
      *     MOVE 連メ４－メッセージ TO メッセージ.
      *
      *     PERFORM 画面表示.
      *     PERFORM 終了入力.
      *
           MOVE 4                  TO 連メＰ－メッセージ番号.
           MOVE 連メ４－メッセージ TO 連メＰ－メッセージ.
004792*
004793      MOVE "PMSG001.DLL" TO dll-name.
004794      MOVE "PMSG001"     TO form-name.
004795      CALL "POWEROPENSHEET" USING dll-name form-name.
      *
      ************
      *           *
      * 終了処理   *
      *           *
      ************
      *     PERFORM 終了処理.
           EXIT PROGRAM.
      *
      *<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
      *================================================================*
      * 初期化 SECTION.
      *
      *     OPEN INPUT 制御情報マスタ.
      *         MOVE NC"制御" TO ファイル名.
      *         PERFORM オープンチェック.
      *     PERFORM 画面オープン.
      *     PERFORM 画面初期化.
      *
      *================================================================*
      * オープンチェック SECTION.
      *
      *     IF 状態キー  NOT =  "00"
      *         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
      *         DISPLAY NC"状態キー：" 状態キー         UPON CONS
      *         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
      *                                                 UPON CONS
      *         ACCEPT  キー入力 FROM CONS
      *         PERFORM ファイル閉鎖
      *         EXIT PROGRAM.
      **================================================================*
      * 画面オープン SECTION.
      **
      *     OPEN I-O 表示ファイル.
      *     IF 通知情報  NOT =  "00"
      *         DISPLAY NC"表示ファイルオープンエラー" UPON CONS
      *         DISPLAY NC"通知情報：" 通知情報        UPON CONS
      *         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
      *                                                UPON CONS
      *         ACCEPT  キー入力 FROM CONS
      *         PERFORM ファイル閉鎖
      *         EXIT PROGRAM.
      **================================================================*
      * 画面初期化 SECTION.
      **
      *     MOVE SPACE TO 画面レコード.
      *     INITIALIZE    画面レコード.
      **
      *     MOVE "MSG001G" TO 定義体名.
      *     MOVE SPACE    TO 処理種別.
      *     MOVE "SCREEN" TO 項目群名.
      *     PERFORM 画面出力.
      **================================================================*
      * 画面表示 SECTION.
      **
      *     MOVE "MSG001G" TO 定義体名.
      *     MOVE SPACE     TO 処理種別.
      *     MOVE "SCREEN"  TO 項目群名.
      *     PERFORM 画面出力.
      **================================================================*
      * 終了入力 SECTION.
      **
      *     MOVE "MSG001G" TO 定義体名.
      *     MOVE SPACE     TO 処理種別.
      *     MOVE "GRP001"  TO 項目群名.
      *     PERFORM 画面入力.
      **================================================================*
      * ファイル閉鎖 SECTION.
      **
      *     CLOSE 表示ファイル.
      **
      **================================================================*
      * 終了処理 SECTION.
      **
      *     PERFORM ファイル閉鎖.
      **================================================================*
      * 画面入力 SECTION.
      **
      *     READ 表示ファイル
      *     IF 通知情報 NOT = "00"
      *         DISPLAY NC"画面入力エラー"      UPON CONS
      *         DISPLAY NC"処理種別：" 処理種別 UPON CONS
      *         DISPLAY NC"項目群名：" 項目群名 UPON CONS
      *         DISPLAY NC"通知情報：" 通知情報 UPON CONS
      *         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"   
      *                                         UPON CONS
      *         ACCEPT  キー入力 FROM CONS
      *         PERFORM ファイル閉鎖
      *         EXIT PROGRAM.
      **================================================================*
      * 画面出力 SECTION.
      **
      *     WRITE 画面レコード
      **
      *     IF ( 通知情報 NOT = "00" )
      *         DISPLAY NC"画面入力エラー"      UPON CONS
      *         DISPLAY NC"処理種別：" 処理種別 UPON CONS
      *         DISPLAY NC"項目群名：" 項目群名 UPON CONS
      *         DISPLAY NC"通知情報：" 通知情報 UPON CONS
      *         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"   
      *                                         UPON CONS
      *         ACCEPT  キー入力 FROM CONS
      *         PERFORM ファイル閉鎖
      *         EXIT PROGRAM.
      **================================================================*
      * 制御情報取得 SECTION.
      *
      *     MOVE ZERO TO 制－制御区分
      *     READ 制御情報マスタ
      *     NOT INVALID KEY
      *         MOVE 制－最大登録部位数 TO 最大登録数Ｗ
      *         MOVE 制－負傷連続登録   TO 負傷連続登録Ｗ
      *         MOVE 制－遅延回数       TO 遅延回数Ｗ
      *     END-READ.
      **
      **================================================================*
      * 遅延処理 SECTION.
      **
      *     PERFORM VARYING 遅延ＣＮＴ FROM 1 BY 1
      *                                UNTIL 遅延ＣＮＴ > 遅延回数Ｗ
      *         MOVE SPACE TO 遅延フラグ
      *     END-PERFORM.
      *
      *================================================================*
      ******************************************************************
       END PROGRAM MSG004.
      ******************************************************************
