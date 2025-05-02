000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAI591.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*   【柔+ｳｨﾝﾄﾞｳｽﾞ版　愛知用 国民健康保険療養費申請集計表】
000100*   【データ作成】
000110*   高齢者１割２割、３歳未満、の対応
000120*         MED = YAI580 YAI592P 
000130*----------------------------------------------------------------*
000140 DATE-WRITTEN.           2013-06-28
000150 DATE-COMPILED.          2013-06-28
000160*----------------------------------------------------------------*
000170******************************************************************
000180*            ENVIRONMENT         DIVISION                        *
000190******************************************************************
000200 ENVIRONMENT             DIVISION.
000210 CONFIGURATION           SECTION.
000220 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000230 OBJECT-COMPUTER.        FMV-DESKPOWER.
000240 SPECIAL-NAMES.          CONSOLE  IS  CONS
000250                         SYSERR   IS  MSGBOX.
000260 INPUT-OUTPUT            SECTION.
000270 FILE-CONTROL.
000280     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000290                             ORGANIZATION             IS  INDEXED
000300                             ACCESS MODE              IS  DYNAMIC
000310                             RECORD KEY               IS  受－施術和暦年月
000320                                                          受－患者コード
000330                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000340                                                          受－患者カナ
000350                                                          受－患者コード
000360                             ALTERNATE RECORD KEY     IS  受－患者コード
000370                                                          受－施術和暦年月
000380                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000390                                                          受－保険種別
000400                                                          受－保険者番号
000410                                                          受－患者コード
000420                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000430                                                          受－公費種別
000440                                                          受－費用負担者番号
000450                                                          受－患者コード
000460                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000470                                                          受－助成種別
000480                                                          受－費用負担者番号助成
000490                                                          受－患者コード
000500                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
000510                                                          受－施術和暦年月
000520                                                          受－患者コード
000530                             FILE STATUS              IS  状態キー
000540                             LOCK        MODE         IS  AUTOMATIC.
           SELECT  レセプトＦ      ASSIGN      TO        RECEPTL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  レセ－施術和暦年月
                                                                レセ－患者コード
                                                                レセ－レセ種別
                                   ALTERNATE RECORD KEY     IS  レセ－患者コード
                                                                レセ－施術和暦年月
                                                                レセ－レセ種別
                                   ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
                                                                レセ－施術和暦年月
                                                                レセ－患者コード
                                                                レセ－レセ種別
                                   ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
                                                                レセ－レセ種別
                                                                レセ－請求保険者番号
                                                                レセ－患者コード
                                                                レセ－施術和暦年月
                                   ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
                                                                レセ－請求保険者番号
                                                                レセ－患者コード
                                                                レセ－レセ種別
                                                                レセ－施術和暦年月
                                   FILE STATUS              IS  状態キー
                                   LOCK        MODE         IS  AUTOMATIC.
000910     SELECT  負担率マスタ    ASSIGN      TO      HUTANRIL
000920                             ORGANIZATION             IS  INDEXED
000930                             ACCESS MODE              IS  DYNAMIC
000940                             RECORD KEY               IS 負率－保険種別
000950                                                         負率－開始和暦年月
000960                             FILE STATUS              IS  状態キー
000970                             LOCK        MODE         IS  AUTOMATIC.
000098     SELECT  保険者特別負担マスタ   ASSIGN      TO        HOKENTKL
000099                             ORGANIZATION             IS  INDEXED
000100                             ACCESS MODE              IS  DYNAMIC
000101                             RECORD KEY               IS  保特－保険種別
000102                                                          保特－保険者番号
000103                                                          保特－開始和暦年月
000105                             FILE STATUS              IS  状態キー
000106                             LOCK        MODE         IS  AUTOMATIC.
000980     SELECT  保険者マスタ    ASSIGN      TO        HOKENSL
000990                             ORGANIZATION             IS  INDEXED
001000                             ACCESS MODE              IS  DYNAMIC
001010                             RECORD KEY               IS  保－保険種別
001020                                                          保－保険者番号
001030                             ALTERNATE RECORD KEY     IS  保－保険種別
001040                                                          保－保険者名称
001050                                                          保－保険者番号
001060                             FILE STATUS              IS  状態キー
001070                             LOCK        MODE         IS  AUTOMATIC.
001080     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
001090                             ORGANIZATION             IS  INDEXED
001100                             ACCESS MODE              IS  DYNAMIC
001110                             RECORD KEY               IS  市－公費種別
001120                                                          市－市町村番号
001130                             ALTERNATE RECORD KEY     IS  市－公費種別
001140                                                          市－市町村名称
001150                                                          市－市町村番号
001160                             FILE STATUS              IS  状態キー
001170                             LOCK        MODE         IS  AUTOMATIC.
001360     SELECT  作業ファイル１  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5911L.DAT"
001370                             ORGANIZATION             IS  INDEXED
001380                             ACCESS                   IS  DYNAMIC
001390                             RECORD      KEY          IS  作１－保険者番号
001450                                                          作１－印刷区分
001490                             FILE        STATUS       IS  状態キー
001500                             LOCK        MODE         IS  AUTOMATIC.
001510******************************************************************
001520*                      DATA DIVISION                             *
001530******************************************************************
001540 DATA                    DIVISION.
001550 FILE                    SECTION.
001560*                           ［ＲＬ＝  ７６８］
001570 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
001580     COPY JUSINJ         OF  XFDLIB  JOINING    受   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
001740*                           ［ＲＬ＝  １２８］
001750 FD  負担率マスタ        BLOCK   CONTAINS   1   RECORDS.
001760     COPY HUTANRI         OF  XFDLIB  JOINING   負率 AS  PREFIX.
000107*
000108 FD  保険者特別負担マスタ  BLOCK   CONTAINS   1   RECORDS.
000109     COPY HOKENTK         OF  XFDLIB  JOINING   保特 AS PREFIX.
001770*                           ［ＲＬ＝  ３２０］
001780 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001790     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
001800*                           ［ＲＬ＝  ２５６］
001810 FD  市町村マスタ          BLOCK   CONTAINS   1   RECORDS.
001820     COPY SITYOSN        OF  XFDLIB  JOINING   市   AS  PREFIX.
001860*
001870 FD  作業ファイル１ RECORD  CONTAINS 64 CHARACTERS.
001880 01  作１－レコード.
001890   03  作１－レコードキー.
001920     05  作１－保険者番号.
001930       07 作１－法別番号               PIC X(2).
001940       07 作１－保番.
001950         09 作１－都道府県番号         PIC X(2).
001960         09 作１－保番号               PIC X(3).
001970         09 作１－検証番号             PIC X.
001980         09 FILLER                     PIC X(2).
001910     05  作１－印刷区分                PIC 9(1).
002050   03  作１－レコードデータ.
002060     05  作１－件数                    PIC 9(3).
002080     05  作１－費用額                  PIC 9(7).
002090     05  作１－請求額                  PIC 9(7).
002100     05  作１－負担額                  PIC 9(7).
002110     05  FILLER                        PIC X(29).
002120*
002130******************************************************************
002140*                WORKING-STORAGE SECTION                         *
002150******************************************************************
002160 WORKING-STORAGE         SECTION.
002170 01 キー入力                           PIC X(1) VALUE SPACE.
002180 01 状態キー                           PIC X(2) VALUE SPACE.
002190 01 終了フラグ１                       PIC X(3) VALUE SPACE.
002200 01 終了フラグ３                       PIC X(3) VALUE SPACE.
002090 01 エラーフラグ                       PIC X(3) VALUE SPACE.
002210 01 複委任フラグ                       PIC 9(1) VALUE ZERO.
002220 01 確認入力Ｗ                         PIC X(1) VALUE SPACE.
002230 01 ファイル名Ｗ                       PIC N(2) VALUE SPACE.
002240 01 ファイル名                         PIC N(2) VALUE SPACE.
002250 01 カレント元号Ｗ                     PIC 9(1) VALUE ZERO.
002260 01 実行キーＷ                         PIC X(4) VALUE SPACE.
002270 01 前和暦Ｗ                           PIC 9(1) VALUE ZERO.
002280 01 行桁フラグ                         PIC X(3) VALUE SPACE.
002290 01 処理移動キー                       PIC X(4) VALUE SPACE.
002300*
002310 01 県コードＪＩＳＷ                   PIC X(2) VALUE SPACE.
002320 01 県コード政管Ｗ                     PIC X(2) VALUE SPACE.
002330 01 施術記録有Ｗ                       PIC X(3) VALUE SPACE.
002340 01 請求和暦年月Ｗ.
002350     03 請求和暦Ｗ                     PIC 9.
002360     03 請求年月Ｗ.
002370        05 請求年Ｗ                    PIC 9(2).
002380        05 請求月Ｗ                    PIC 9(2).
002390 01 請求日Ｗ                           PIC 9(2).
002400 01 印刷形式Ｗ                         PIC 9(1)  VALUE ZERO.
002410 01 元号名称Ｗ                         PIC N(2)  VALUE SPACE.
002420*
002430**************
002440* 保険者情報 *
002450**************
002460*
002470 01 共済区分Ｗ                         PIC 9(1)  VALUE ZERO.
002960*
002970 01 費用額ＰＧＷ                     PIC X(8) VALUE SPACE.
002980 01 負担額ＰＧＷ                     PIC X(8) VALUE SPACE.
002990 01 負担額ＰＧ老人Ｗ                 PIC X(8) VALUE SPACE.
003000 01 負担額ＰＧ助成Ｗ                 PIC X(8) VALUE SPACE.
003010 01 請求和暦年月ＷＲ.
003020    03 請求和暦ＷＲ                  PIC 9     VALUE ZERO.
003030    03 請求年ＷＲ                    PIC 9(2)  VALUE ZERO.
003040    03 請求月ＷＲ                    PIC 9(2)  VALUE ZERO.
003050*
003060 01 保険者番号Ｗ.
003070    03 法別番号Ｗ                    PIC X(2) VALUE SPACE.
003080    03 保番Ｗ.
003090       05 都道府県番号Ｗ             PIC X(2) VALUE SPACE.
003100       05 保険番号Ｗ                 PIC X(3) VALUE SPACE.
003110       05 検証番号Ｗ                 PIC X    VALUE SPACE.
003120       05 FILLER                     PIC X(2).
003130 01 保険種別Ｗ                       PIC 9(2) VALUE ZERO.
003140 01 公費種別Ｗ                       PIC 9(2) VALUE ZERO.
003150 01 助成種別Ｗ                       PIC 9(2) VALUE ZERO.
003160 01 本人家族区分Ｗ                   PIC 9(1) VALUE ZERO.
003170 01 法別番号Ｗ                       PIC X(2) VALUE SPACE.
003180 01 老人負担金免除Ｗ                 PIC 9(1) VALUE ZERO.
003190 01 給付割合Ｗ                       PIC 9(2) VALUE ZERO.
003200*
003210 01 本人負担率Ｗ                     PIC 9(3) VALUE ZERO.
003220 01 家族負担率Ｗ                     PIC 9(3) VALUE ZERO.
003230 01 甲乙区分Ｗ                       PIC 9(1) VALUE ZERO.
003240*
003250 01 対象件数Ｗ                       PIC 9(4) VALUE ZERO.
003260*
003270 01 施記キーＷＲ.
003280    03 患者番号ＷＲ                  PIC 9(6) VALUE ZERO.
003290    03 枝番ＷＲ                      PIC X(1) VALUE SPACE.
003300    03 請求和暦ＷＲ２                PIC 9(1) VALUE ZERO.
003310    03 請求年ＷＲ２                  PIC 9(2) VALUE ZERO.
003320    03 請求月ＷＲ２                  PIC 9(2) VALUE ZERO.
003330*
003340 01 終了フラグ２                     PIC X(3) VALUE SPACE.
003350*
003360 01 状態フラグ                       PIC 9(1) VALUE ZERO.
003370*
003380 01 請求和暦年月日ＣＷ.
003390   03 請求和暦年月ＣＷ.
003400     05 請求和暦ＣＷ                 PIC 9    VALUE ZERO.
003410     05 請求年月ＣＷ.
003420        07 請求年ＣＷ                PIC 9(2) VALUE ZERO.
003430        07 請求月ＣＷ                PIC 9(2) VALUE ZERO.
003440   03 請求日ＣＷ                     PIC 9(2) VALUE ZERO.
003450*
003460*/年月毎持つ保険者データのワーク
003470 01 保負担率区分Ｗ                     PIC 9    VALUE ZERO.
003480 01 保本人負担率Ｗ                     PIC 9(3) VALUE ZERO.
003490 01 保家族負担率Ｗ                     PIC 9(3) VALUE ZERO.
003500 01 保本人負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
003510 01 保家族負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
003570*
003580******************************************************************
003590*                          連結項目                              *
003600******************************************************************
003610*
003620****************
003630* 画面入力情報 *
003640****************
       01 連入－画面情報ＹＡＩ５８０ IS EXTERNAL.
          03 連入－請求和暦年月.
             05 連入－請求和暦               PIC 9(1).
             05 連入－請求年月.
                07 連入－請求年              PIC 9(2).
                07 連入－請求月              PIC 9(2).
          03 連入－作成和暦年月日.
             05 連入－作成和暦年月.
                07 連入－作成和暦            PIC 9(1).
                07 連入－作成年              PIC 9(2).
                07 連入－作成月              PIC 9(2).
             05 連入－作成日                 PIC 9(2).
       01 連入－入力データＹＡＩ５８０   IS EXTERNAL.
          03 連入－保険種別                  PIC 9(2).
          03 連入－保険者番号                PIC X(10).
004160*
004170******************************************************************
004180*                      PROCEDURE  DIVISION                       *
004190******************************************************************
004200 PROCEDURE               DIVISION.
004220************
004230*           *
004240* 初期処理   *
004250*           *
004260************
004270     PERFORM 初期化.
004280************
004290*           *
004300* 主処理     *
004310*           *
004320************
004330     PERFORM 作業ファイル作成.
004340************
004350*           *
004360* 終了処理   *
004370*           *
004380************
004390     PERFORM 終了処理.
004400     MOVE ZERO TO PROGRAM-STATUS.
004410     EXIT PROGRAM.
004420*
004430*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
004440*================================================================*
004450 初期化 SECTION.
004460*
004470     OPEN INPUT 受診者情報Ｆ.
004480             MOVE NC"受診" TO ファイル名Ｗ.
004490             PERFORM オープンチェック.
003250     OPEN INPUT レセプトＦ.
003260         MOVE NC"レセプトＦ" TO ファイル名Ｗ.
003270         PERFORM オープンチェック.
004650     OPEN INPUT 負担率マスタ.
004660         MOVE NC"負担率マスタ" TO ファイル名Ｗ.
004670         PERFORM オープンチェック.
004870     OPEN INPUT 保険者特別負担マスタ.
004880         MOVE NC"保険者特別負担マスタ" TO ファイル名.
004890         PERFORM オープンチェック.
004680     OPEN INPUT 保険者マスタ.
004690         MOVE NC"保険者マスタ" TO ファイル名Ｗ.
004700         PERFORM オープンチェック.
004710     OPEN INPUT 市町村マスタ.
004720         MOVE NC"市町村マスタ" TO ファイル名Ｗ.
004730         PERFORM オープンチェック.
004740*
004920* 連結項目の待避
004930     MOVE 連入－請求和暦 TO 請求和暦ＷＲ.
004940     MOVE 連入－請求年   TO 請求年ＷＲ.
004950     MOVE 連入－請求月   TO 請求月ＷＲ.
004960*================================================================*
004970 オープンチェック SECTION.
004980*
004990     IF 状態キー  NOT =  "00"
005000         DISPLAY ファイル名Ｗ NC"Ｆオープンエラー" UPON CONS
005010         DISPLAY NC"状態キー：" 状態キー           UPON CONS
005020         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
005030                                                   UPON CONS
003131*-----------------------------------------*
003132         CALL "actcshm"  WITH C LINKAGE
003133*-----------------------------------------*
005040         ACCEPT  キー入力 FROM CONS
005050         PERFORM ファイル閉鎖
005060         EXIT PROGRAM.
005590*================================================================*
005600 ファイル閉鎖 SECTION.
005610*
005620     CLOSE  受診者情報Ｆ 負担率マスタ 保険者マスタ
005640            市町村マスタ レセプトＦ   保険者特別負担マスタ.
005650*================================================================*
005660 終了処理 SECTION.
005670*
005680     PERFORM ファイル閉鎖.
005690*================================================================*
005700* データ作成 SECTION.
005710 作業ファイル作成 SECTION.
005720*
005750     OPEN OUTPUT 作業ファイル１.
005760         MOVE NC"作１" TO ファイル名.
005770         PERFORM オープンチェック.
005780     CLOSE 作業ファイル１.
005790     OPEN I-O 作業ファイル１.
005800         MOVE NC"作１" TO ファイル名.
005810         PERFORM オープンチェック.
005850*  /*国保
007470     MOVE 請求和暦ＷＲ  TO レセ－請求和暦.
007480     MOVE 請求年ＷＲ    TO レセ－請求年.
007490     MOVE 請求月ＷＲ    TO レセ－請求月.
013730     MOVE 1             TO レセ－レセ種別.
007500     MOVE ZERO          TO レセ－請求保険者番号.
007500     MOVE ZERO          TO レセ－施術和暦.
007510     MOVE ZERO          TO レセ－施術年.
007520     MOVE ZERO          TO レセ－施術月.
007530     MOVE ZERO          TO レセ－患者番号.
007540     MOVE SPACE         TO レセ－枝番.
007550     START レセプトＦ   KEY IS >= レセ－請求和暦年月
000250                                  レセ－レセ種別
007560                                  レセ－請求保険者番号
007570                                  レセ－患者コード
                                        レセ－施術和暦年月
006010     END-START.
006020     IF 状態キー = "00"
006030         MOVE SPACE  TO 終了フラグ３
006040         PERFORM レセプトＦ読込
006080         PERFORM UNTIL ( 終了フラグ３ = "YES" ) OR
007630                       ( レセ－請求和暦 NOT = 請求和暦ＷＲ ) OR
007640                       ( レセ－請求年   NOT = 請求年ＷＲ   ) OR
007650                       ( レセ－請求月   NOT = 請求月ＷＲ   ) OR
                             ( レセ－レセ種別 NOT = 1 )
005700             PERFORM データチェック
005710             IF 実行キーＷ = "YES"
006110*
006130                 IF (( 受－保険種別 = 01 ) AND ( 受－法別番号 = "23" )) OR
                          (( 受－保険種別 = 08 ) AND ( 受－都道府県番号 = "23" )) OR
                          (( 受－保険種別 = 01 ) AND ( 受－保険者番号 = "133264" OR "133298" )) 
006240                    PERFORM 作業ファイル作成１
006260                 END-IF
006270             END-IF
006280             PERFORM レセプトＦ読込
006290         END-PERFORM
006310     END-IF.
007270     CLOSE 作業ファイル１.
007910*================================================================*
007920 レセプトＦ読込 SECTION.
007930*
007940     READ レセプトＦ NEXT
007950     AT END
007960         MOVE "YES" TO 終了フラグ３
007970     END-READ.
008220*================================================================*
008230 エラー表示 SECTION.
008240*
008250     DISPLAY NC"ファイル書込エラー：" ファイル名Ｗ UPON CONS.
008260     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
008270     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
003331*-----------------------------------------*
003332     CALL "actcshm"  WITH C LINKAGE.
003333*-----------------------------------------*
008280     ACCEPT  キー入力 FROM CONS.
008290*================================================================*
008300 エラー表示Ｒ SECTION.
008310*
008320     DISPLAY NC"ファイル読込エラー" ファイル名Ｗ UPON CONS.
008330     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
003331*-----------------------------------------*
003332     CALL "actcshm"  WITH C LINKAGE.
003333*-----------------------------------------*
008340     ACCEPT  キー入力 FROM CONS.
008350     PERFORM ファイル閉鎖.
008360     EXIT PROGRAM.
008370*================================================================*
008380 エラー表示その他 SECTION.
008390*
008400     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
008410     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
003331*-----------------------------------------*
003332     CALL "actcshm"  WITH C LINKAGE.
003333*-----------------------------------------*
008420     ACCEPT  キー入力 FROM CONS.
008430     PERFORM ファイル閉鎖.
008440     EXIT PROGRAM.
008450*================================================================*
008460 データチェック SECTION.
008470*
008480     MOVE SPACE          TO 実行キーＷ.
007260* *****************************************************************
007270* * レセプトＦの請求対象区分 = 0 の場合データ作成対象としない *
007280* *****************************************************************
007290     IF ( レセ－請求対象区分 NOT = ZERO ) AND
007300        ( レセ－償還払い区分 NOT = 1 )
007310        IF (レセ－レセ種別 = 3) AND ( レセ－会総括表印刷対象区分 = 1 )
007320            CONTINUE
007330        ELSE
007340           MOVE レセ－施術和暦  TO 受－施術和暦
007350           MOVE レセ－施術年    TO 受－施術年
007360           MOVE レセ－施術月    TO 受－施術月
007370           MOVE レセ－患者番号  TO 受－患者番号
007380           MOVE レセ－枝番      TO 受－枝番
007390           READ 受診者情報Ｆ
007400           NOT INVALID KEY
007410**      健保のみ
007420              IF 受－保険分類 = 1
007430                 MOVE "YES"  TO 実行キーＷ
007440              END-IF
007450           END-READ
007460        END-IF
007470     END-IF.
008940*
009020*================================================================*
009030 作業ファイル作成１ SECTION.
009040*
009050     MOVE SPACE TO 作１－レコード.
009060     INITIALIZE    作１－レコード.
009130*
009400     EVALUATE 受－保険種別
009410     WHEN 1
009420         MOVE 受－保険者番号       TO 作１－保険者番号
009791         EVALUATE 受－特別区分
009792         WHEN 1
009794         WHEN 2
009793             MOVE 1                TO 作１－印刷区分
009795         WHEN 3
009796             MOVE 2                TO 作１－印刷区分
009797         WHEN 6
009798             MOVE 5                TO 作１－印刷区分
009799         WHEN OTHER
009660             PERFORM 負担率取得
                   IF 給付割合Ｗ = 7
009801                MOVE 4             TO 作１－印刷区分
                   ELSE
009801                MOVE 3             TO 作１－印刷区分
                   END-IF
009803         END-EVALUATE
009446     WHEN 8
009450         MOVE 受－保番             TO 作１－保険者番号
009470         IF 受－特別区分 = 6
                  MOVE 8                 TO 作１－印刷区分
               ELSE
009470            IF 受－本人家族区分 = 1
                     MOVE 6              TO 作１－印刷区分
009490            ELSE
009500               MOVE 7              TO 作１－印刷区分
                  END-IF
009510         END-IF
009520     END-EVALUATE.
           IF 作１－保険者番号(1:3) = "234"
              MOVE "23001"           TO 作１－保険者番号
           END-IF.
009820     READ 作業ファイル１
009830     INVALID KEY
009840         MOVE 1                TO 作１－件数 
009910         MOVE レセ－合計       TO 作１－費用額
      */中央建設・全国建設工業は件数と費用額のみ/1309
               IF 作１－保険者番号 = "133264" OR "133298"
009920            MOVE ZERO             TO 作１－請求額
009880            MOVE ZERO             TO 作１－負担額
               ELSE
009920            MOVE レセ－請求金額   TO 作１－請求額
009880            MOVE レセ－一部負担金 TO 作１－負担額
               END-IF
009940         WRITE 作１－レコード
009950         INVALID KEY
009960             DISPLAY NC"状態キー" 状態キー
009970             MOVE NC"作１"  TO ファイル名Ｗ
009980             PERFORM エラー表示
009990         END-WRITE
010000     NOT INVALID KEY
010010         COMPUTE 作１－件数   = 作１－件数   + 1
010080         COMPUTE 作１－費用額 = 作１－費用額 + レセ－合計
      */中央建設・全国建設工業は件数と費用額のみ/1309
               IF 作１－保険者番号 = "133264" OR "133298"
                  CONTINUE
               ELSE
010090            COMPUTE 作１－請求額 = 作１－請求額 + レセ－請求金額
010050            COMPUTE 作１－負担額 = 作１－負担額 + レセ－一部負担金
               END-IF
010110         REWRITE 作１－レコード
010120         INVALID KEY
010130             DISPLAY NC"状態キー" 状態キー
010140             MOVE NC"作１"  TO ファイル名Ｗ
010150             PERFORM エラー表示
010160         END-REWRITE
010170     END-READ.
010180*================================================================*
010190 負担率取得 SECTION.
010200*
010210     MOVE ZERO TO 本人負担率Ｗ.
010220     MOVE ZERO TO 家族負担率Ｗ.
010230*
010240     MOVE 受－保険種別   TO 保－保険種別.
010250     MOVE 受－保険者番号 TO 保－保険者番号.
010260     READ 保険者マスタ
010270     INVALID KEY
010280         CONTINUE
010290     NOT INVALID KEY
               MOVE "YES" TO エラーフラグ
010460         MOVE 保－保険種別   TO 保特－保険種別
010470         MOVE 保－保険者番号 TO 保特－保険者番号
010460         MOVE 99999          TO 保特－開始和暦年月
007550         START 保険者特別負担マスタ   KEY IS < 保特－保険種別
                                                     保特－保険者番号
                                                     保特－開始和暦年月
                                                     REVERSED
               END-START
006220         IF 状態キー = "00"
010480            READ 保険者特別負担マスタ NEXT
010510            NOT AT END
                     IF (保－保険種別   = 保特－保険種別) AND
                        (保－保険者番号 = 保特－保険者番号)
010520                  IF ( 保特－負担率区分 NOT = ZERO ) OR
010530                     ( 受－甲乙区分   NOT = ZERO )
010540                     IF 受－甲乙区分 NOT = 2
010550                         MOVE 保特－本人負担率   TO 本人負担率Ｗ
010560                         MOVE 保特－家族負担率   TO 家族負担率Ｗ
010570                     ELSE
010580                         MOVE 保特－本人負担率乙 TO 本人負担率Ｗ
010590                         MOVE 保特－家族負担率乙 TO 家族負担率Ｗ
010600                     END-IF
                           MOVE SPACE TO エラーフラグ
                        END-IF
                     END-IF
                  END-READ
               END-IF
               IF エラーフラグ NOT = SPACE
010780             MOVE 保－保険種別     TO 負率－保険種別
010790             MOVE 連入－請求和暦   TO 負率－開始和暦 請求和暦ＣＷ
010800             MOVE 連入－請求年     TO 負率－開始年   請求年ＣＷ
010810             MOVE 連入－請求月     TO 負率－開始月   請求月ＣＷ
010820             START 負担率マスタ KEY IS <= 負率－保険種別
010830                                          負率－開始和暦年月
010840                                          REVERSED
010850             END-START
010860             IF 状態キー = "00" 
010870                 READ 負担率マスタ NEXT
010880                 AT END
010890                     CONTINUE
010900                 NOT AT END
010910                     IF ( 請求和暦年月ＣＷ >= 負率－開始和暦年月 ) AND
010920                        ( 請求和暦年月ＣＷ <= 負率－終了和暦年月 )
010930                         MOVE 負率－本人負担率 TO 本人負担率Ｗ
010940                         MOVE 負率－家族負担率 TO 家族負担率Ｗ
010950                     END-IF
010960                 END-READ
010970             END-IF
010980         END-IF
010990     END-READ.
011000     IF 受－本人家族区分 = 1
011010         EVALUATE 本人負担率Ｗ
011020         WHEN 30
011030             MOVE  7   TO 給付割合Ｗ
011040         WHEN 20
011050             MOVE  8   TO 給付割合Ｗ
011060         WHEN 10
011070             MOVE  9   TO 給付割合Ｗ
011080         WHEN 0
011090             MOVE  10  TO 給付割合Ｗ
011100         END-EVALUATE
011110     ELSE
011120         EVALUATE 家族負担率Ｗ
011130         WHEN 30
011140             MOVE  7   TO 給付割合Ｗ
011150         WHEN 20
011160             MOVE  8   TO 給付割合Ｗ
011170         WHEN 10
011180             MOVE  9   TO 給付割合Ｗ
011190         WHEN 0
011200             MOVE  10  TO 給付割合Ｗ
011210         END-EVALUATE
011220     END-IF.
011230*
011520*================================================================*
011530******************************************************************
011540 END PROGRAM YAI591.
011550******************************************************************
