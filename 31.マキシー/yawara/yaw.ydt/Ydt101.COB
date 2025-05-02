000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YDT101.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090* 代替医療協会 会提出フロッピー作成【FPD書込】
000100* 請求年月Ver.
000110*      MED = YDT580 
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2013-05-30
000140 DATE-COMPILED.          2013-05-30
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
000270     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000280                             ORGANIZATION             IS  INDEXED
000290                             ACCESS MODE              IS  DYNAMIC
000300                             RECORD KEY               IS  受－施術和暦年月
000310                                                          受－患者コード
000320                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000330                                                          受－患者カナ
000340                                                          受－患者コード
000350                             ALTERNATE RECORD KEY     IS  受－患者コード
000360                                                          受－施術和暦年月
000370                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000380                                                          受－保険種別
000390                                                          受－保険者番号
000400                                                          受－患者コード
000410                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000420                                                          受－公費種別
000430                                                          受－費用負担者番号
000440                                                          受－患者コード
000450                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000460                                                          受－助成種別
000470                                                          受－費用負担者番号助成
000480                                                          受－患者コード
000490                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
000500                                                          受－施術和暦年月
000510                                                          受－患者コード
000520                             FILE STATUS              IS  状態キー
000530                             LOCK        MODE         IS  AUTOMATIC.
000241     SELECT  生保情報Ｆ      ASSIGN      TO        SEIHOJL
000242                             ORGANIZATION             IS INDEXED
000243                             ACCESS MODE              IS DYNAMIC
000244                             RECORD KEY               IS 生保－施術和暦年月
000245                                                         生保－患者コード
000255                             ALTERNATE RECORD KEY     IS 生保－患者コード
000265                                                         生保－施術和暦年月
000277                             FILE STATUS              IS 状態キー
000278                             LOCK        MODE         IS AUTOMATIC.
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
000540     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
000550                             ORGANIZATION             IS  INDEXED
000560                             ACCESS MODE              IS  DYNAMIC
000570                             RECORD KEY               IS  負－施術和暦年月
000580                                                          負－患者コード
000590                             ALTERNATE RECORD KEY     IS  負－患者コード
000600                                                          負－施術和暦年月
000610                             FILE STATUS              IS  状態キー
000620                             LOCK        MODE         IS  AUTOMATIC.
000630     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
000640                             ORGANIZATION             IS  INDEXED
000650                             ACCESS MODE              IS  DYNAMIC
000660                             RECORD KEY               IS  施記－施術和暦年月日
000670                                                          施記－患者コード
000680                             ALTERNATE RECORD KEY     IS  施記－患者コード
000690                                                          施記－施術和暦年月日
000700                             FILE STATUS              IS  状態キー
000710                             LOCK        MODE         IS  AUTOMATIC.
000720     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000730                             ORGANIZATION             IS  INDEXED
000740                             ACCESS MODE              IS  DYNAMIC
000750                             RECORD KEY               IS 施情－施術所番号
000760                             FILE STATUS              IS  状態キー
000770                             LOCK        MODE         IS  AUTOMATIC.
000780     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000790                             ORGANIZATION             IS  INDEXED
000800                             ACCESS MODE              IS  DYNAMIC
000810                             RECORD KEY               IS  元－元号区分
000820                             FILE STATUS              IS  状態キー
000830                             LOCK        MODE         IS  AUTOMATIC.
000940     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000950                             ORGANIZATION             IS  INDEXED
000960                             ACCESS MODE              IS  DYNAMIC
000970                             RECORD KEY               IS  制－制御区分
000980                             FILE STATUS              IS  状態キー
000990                             LOCK        MODE         IS  AUTOMATIC.
000098     SELECT  保険者特別負担マスタ   ASSIGN      TO        HOKENTKL
000099                             ORGANIZATION             IS  INDEXED
000100                             ACCESS MODE              IS  DYNAMIC
000101                             RECORD KEY               IS  保特－保険種別
000102                                                          保特－保険者番号
000103                                                          保特－開始和暦年月
000105                             FILE STATUS              IS  状態キー
000106                             LOCK        MODE         IS  AUTOMATIC.
001290*
001520     SELECT 作業ファイル１ ASSIGN    TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1012L.DAT"
001530                             ORGANIZATION             IS  INDEXED
001540                             ACCESS MODE              IS  DYNAMIC
001550                             RECORD KEY               IS  作１－並び区分
001560                                                          作１－保険者番号
001570                                                          作１－施術和暦年月
001580                                                          作１－負担区分
001560                                                          作１－本人家族区分
001570                                                          作１－患者カナ
001580                                                          作１－患者コード
001590                             FILE STATUS              IS  状態キー
001600                             LOCK        MODE         IS  AUTOMATIC.
001601*
001602     SELECT 会提出ファイル   ASSIGN      TO     FD-NAME
001603                             ORGANIZATION             IS  LINE SEQUENTIAL
001604                             ACCESS MODE              IS  SEQUENTIAL
001605                             FILE STATUS              IS  状態キー
001606                             LOCK      MODE           IS  AUTOMATIC.
001610******************************************************************
001620*                      DATA DIVISION                             *
001630******************************************************************
001640 DATA                    DIVISION.
001650 FILE                    SECTION.
001660*                           ［ＲＬ＝  ３２０］
001670 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
001680     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
001080* 
000280 FD  生保情報Ｆ          BLOCK   CONTAINS   1   RECORDS.
000281     COPY SEIHOJ          OF  XFDLIB  JOINING   生保   AS  PREFIX.
      *
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
001690*                           ［ＲＬ＝  １２８］
001700 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
001710     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
001720*                           ［ＲＬ＝  ２５６］
001730 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
001740     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
001750*
001760 FD  施術所情報マスタ    BLOCK   CONTAINS   1   RECORDS.
001770     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
001780*                           ［ＲＬ＝  １２８］
001790 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001800     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001840*                           ［ＲＬ＝  ２５６］
001850 FD  制御情報マスタ      BLOCK   CONTAINS   1   RECORDS.
001860     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
000107*
000108 FD  保険者特別負担マスタ  BLOCK   CONTAINS   1   RECORDS.
000109     COPY HOKENTK         OF  XFDLIB  JOINING   保特 AS PREFIX.
004330*                           ［ＲＬ＝  ２５６］
004340 FD  作業ファイル１ BLOCK  CONTAINS 256  RECORDS.
004350 01 作１－レコード.
004360    03 作１－レコードキー.
004370       05 作１－並び区分                       PIC 9(2).
004380       05 作１－保険者番号                     PIC X(8).
004420       05 作１－施術和暦年月.
004430          07 作１－施術和暦                    PIC 9.
004440          07 作１－施術年月.
004450             09 作１－施術年                   PIC 9(2).
004460             09 作１－施術月                   PIC 9(2).
004370       05 作１－負担区分                       PIC 9(1).
004370       05 作１－本人家族区分                   PIC 9(1).
004380       05 作１－患者カナ                       PIC X(50).
004390       05 作１－患者コード.
004400          07 作１－患者番号                    PIC Z(6).
004410          07 作１－枝番                        PIC X.
001700    03 作１－レコードデータ.
001860       05 作１－患者氏名                       PIC X(20).
001840       05 作１－被保険者氏名                   PIC X(20).
001810       05 作１－受給者番号                     PIC X(7).
001960       05 作１－部位数                         PIC Z9.
001960       05 作１－日数                           PIC Z9.
001920       05 作１－合計金額                       PIC ZZZZZZZZ9.
001940       05 作１－一部負担金                     PIC ZZZZZZZZ9.
001950       05 作１－請求金額                       PIC ZZZZZZZZ9.
004380       05 作１－親保険者番号                   PIC X(8).
001870       05 作１－患者姓カナ                     PIC X(10).
001870       05 作１－患者名カナ                     PIC X(10).
001780       05 作１－記号                           PIC X(22).
001790       05 作１－番号                           PIC X(22).
001790       05 作１－兵庫限度                       PIC X(1).
004470       05 FILLER                               PIC X(31).
004471*
001330 FD  会提出ファイル RECORD  CONTAINS 236 CHARACTERS.
004479 01 会提－レコード.
004480   03 会提－レコードデータ.
004481     05 会提－連番                          PIC Z(4).
           05 会提－区切１                        PIC X(1).
004503     05 会提－患者番号                      PIC X(7).
           05 会提－区切２                        PIC X(1).
004490     05 会提－患者氏名                      PIC X(20).
           05 会提－区切３                        PIC X(1).
004492     05 会提－被保険者氏名                  PIC X(20).
           05 会提－区切４                        PIC X(1).
004485     05 会提－本人家族                      PIC 9(2).
           05 会提－区切５                        PIC X(1).
004486     05 会提－受給者番号                    PIC X(7).
           05 会提－区切６                        PIC X(1).
004486     05 会提－保険者番号                    PIC X(8).
           05 会提－区切７                        PIC X(1).
004482     05 会提－部位数                        PIC Z9.
           05 会提－区切８                        PIC X(1).
004483     05 会提－日数                          PIC X(2).
           05 会提－区切９                        PIC X(1).
004495     05 会提－合計金額                      PIC X(9).
           05 会提－区切１０                      PIC X(1).
004497     05 会提－一部負担金                    PIC X(9).
           05 会提－区切１１                      PIC X(1).
004499     05 会提－請求金額                      PIC X(9).
           05 会提－区切１２                      PIC X(1).
004494     05 会提－施術年月                      PIC X(4).
           05 会提－区切１３                      PIC X(1).
004502     05 会提－未使用                        PIC X.
           05 会提－区切１４                      PIC X(1).
004484     05 会提－柔整師番号                    PIC X(16).
           05 会提－区切１５                      PIC X(1).
004491     05 会提－柔整師名                      PIC X(20).
           05 会提－区切１６                      PIC X(1).
004486     05 会提－親保険者番号                  PIC X(8).
           05 会提－区切１７                      PIC X(1).
004489     05 会提－患者姓カナ                    PIC X(10).
           05 会提－区切１８                      PIC X(1).
004489     05 会提－患者名カナ                    PIC X(10).
           05 会提－区切１９                      PIC X(1).
004487     05 会提－保険証記号                    PIC X(22).
           05 会提－区切２０                      PIC X(1).
004488     05 会提－保険証番号                    PIC X(22).
           05 会提－区切２１                      PIC X(1).
004502     05 会提－負担区分                      PIC 9(1).
           05 会提－区切２２                      PIC X(1).
004502     05 会提－兵庫限度                      PIC X.
004508*
004509*----------------------------------------------------------------*
004510******************************************************************
004511*                WORKING-STORAGE SECTION                         *
004520******************************************************************
004530 WORKING-STORAGE         SECTION.
004540 01 キー入力                           PIC X    VALUE SPACE.
004550 01 状態キー                           PIC X(2) VALUE SPACE.
004560 01 終了フラグ                         PIC X(3) VALUE SPACE.
004570 01 終了フラグ２                       PIC X(3) VALUE SPACE.
004580 01 終了フラグ３                       PIC X(3) VALUE SPACE.
004581 01 終了フラグ４                       PIC X(3) VALUE SPACE.
004590 01 実行キーＷ                         PIC X(3)  VALUE SPACE.
004600 01 施術記録有Ｗ                       PIC X(3) VALUE SPACE.
004610 01 ファイル名                         PIC N(8) VALUE SPACE.
004620 01 カウンタ                           PIC 9(1) VALUE ZERO.
004621 01 カウンタ２                         PIC 9(1) VALUE ZERO.
004630 01 文字カウンタ                       PIC 9(4) VALUE ZERO.
004631 01 文字カウンタ２                     PIC 9(4) VALUE ZERO.
004632 01 文字カウンタ３                     PIC 9(4) VALUE ZERO.
004633 01 文字カウンタ４                     PIC 9(4) VALUE ZERO.
004634 01 文字カウンタ５                     PIC 9(4) VALUE ZERO.
004635 01 文字カウンタ６                     PIC 9(4) VALUE ZERO.
004640 01 連番Ｗ                             PIC 9(4) VALUE ZERO.
004650 01 預金種別Ｗ                         PIC X(1) VALUE SPACE.
004730 01 請求和暦年月ＷＲ.
004740    03 請求和暦ＷＲ                    PIC 9(1) VALUE ZERO.
004750    03 請求年月ＷＲ.
004760       05 請求年ＷＲ                   PIC 9(2) VALUE ZERO.
004770       05 請求月ＷＲ                   PIC 9(2) VALUE ZERO.
004780*
004990 01 実日数Ｗ                           PIC 9(2) VALUE ZERO.
005000 01 部位ＣＮＴ                         PIC 9(2) VALUE ZERO.
005010 01 部位ＣＮＴ２                       PIC 9(2) VALUE ZERO.
005090*
005100 01 遅延フラグ                         PIC X(3) VALUE SPACE.
005110 01 遅延ＣＮＴ                         PIC 9(5) VALUE ZERO.
005120 01 遅延カウンタ                       PIC 9(4) VALUE ZERO.
005130 01 遅延回数Ｗ                         PIC 9(4) VALUE ZERO.
005140*
005150 01 英数字項目Ｗ.
005160   03 英数字項目ＸＷ                   PIC X(70) VALUE SPACE.
005170 01 日本語項目Ｗ.
005180   03 日本語項目ＮＷ                   PIC N(35) VALUE SPACE.
005190 01 英数字項目２Ｗ                     PIC X(70) VALUE SPACE.
005200*
005382 01 保険者番号Ｗ                       PIC X(10) VALUE SPACE.
005382 01 患者姓カナＷ                       PIC X(10) VALUE SPACE.
005382 01 患者名カナＷ                       PIC X(10) VALUE SPACE.
      *
005388** 請求額右詰め用
005389 01 請求額ＷＴ.
005390    03 請求額左詰めＷ.
005391      05 請求額左詰めＷ１              PIC X OCCURS 6 VALUE SPACE.
005392    03 請求額右詰めＷ.
005393      05 請求額右詰めＷ１              PIC X OCCURS 6 VALUE ZERO.
005394    03 請求額数字Ｗ                    PIC 9(6)  VALUE ZERO.
005395 01 請求額Ｗ                           PIC 9(9)  VALUE ZERO.
005396
005397 01 負担額Ｗ                           PIC 9(9)  VALUE ZERO.
005397 01 費用額Ｗ                           PIC 9(9)  VALUE ZERO.
005580 01 負担率Ｗ                           PIC 9(3) VALUE ZERO.
005710*
002251 01 定期修正ＦＤＷ                     PIC X VALUE SPACE.
005723 01 FD-NAME                            PIC X(30) VALUE SPACE.
005724*
005725 01 保険種別Ｗ                         PIC X(2)  VALUE SPACE.
004780*
007570 01 記号Ｗ.
007580    03 印刷記号Ｗ                      PIC N(12)  VALUE SPACE.
007660*
007670 01 番号Ｗ.
007680    03 印刷番号Ｗ                      PIC X(30)  VALUE SPACE.
      *
       01 改行Ｗ                             PIC X(2)  VALUE X"0D0A" GLOBAL.
014774*
       01 複合プログラム名Ｗ                 PIC X(8) VALUE "MOJI2".
005730******************************************************************
005740*                          連結項目                              *
005750******************************************************************
005760*
005770********************
005780* メッセージ表示キー *
005790********************
005800 01 連メ－キー IS EXTERNAL.
005810    03  連メ－メッセージ               PIC N(20).
005820*
005830 01 連メ３－キー IS EXTERNAL.
005840    03  連メ３－メッセージ             PIC N(20).
005850    03  連メ３－メッセージ１           PIC X(20).
005860*
004091 01 連メ７－キー IS EXTERNAL.
004092    03  連メ７－メッセージ１           PIC X(40).
004102    03  連メ７－メッセージ２           PIC X(40).
005860*
005870****************
005880* 画面入力情報 *
005890****************
       01 連入－画面情報ＹＤＴ１００ IS EXTERNAL.
          03 連入－請求和暦年月.
             05 連入－請求和暦                 PIC 9.
             05 連入－請求年月.
                07 連入－請求年                PIC 9(2).
                07 連入－請求月                PIC 9(2).
          03 連入－ドライブ                    PIC X(1).
      *
      *0:通常 1:固定パス
       01 連入－固定フラグ３２１１ IS EXTERNAL.
          03 連入－固定フラグ                PIC 9(1).
006830*
007470* 負担率取得用14/10～
007480 01 連率－負担率取得キー IS EXTERNAL.
007490    03 連率－施術和暦年月.
007500       05 連率－施術和暦               PIC 9.
007510       05 連率－施術年月.
007520          07 連率－施術年              PIC 9(2).
007530          07 連率－施術月              PIC 9(2).
007540    03 連率－患者コード.
007550       05 連率－患者番号               PIC 9(6).
007560       05 連率－枝番                   PIC X.
007570    03 連率－実際負担率                PIC 9(3).
007580    03 連率－実際本体負担率            PIC 9(3).
007590    03 連率－健保負担率                PIC 9(3).
007600    03 連率－２７老負担率              PIC 9(3).
007610    03 連率－助成負担率                PIC 9(3).
007620    03 連率－特別用負担率              PIC 9(3).
      *
      * 暗号複合用
       01 連暗号複合－暗号情報 IS EXTERNAL.
          03 連暗号複合－入力情報.
             05 連暗号複合－記号               PIC X(24).
             05 連暗号複合－番号               PIC X(30).
             05 連暗号複合－暗号化項目.
                07 連暗号複合－暗号患者番号    PIC X(6).
                07 連暗号複合－暗号判定記号    PIC X.
                07 連暗号複合－暗号判定番号    PIC X.
                07 連暗号複合－暗号記号        PIC X(24).
                07 連暗号複合－暗号番号        PIC X(30).
          03 連暗号複合－出力情報.
             05 連暗号複合－複合した記号       PIC X(24).
             05 連暗号複合－複合した番号       PIC X(30).
006280*
006290******************************************************************
006300*                      PROCEDURE  DIVISION                       *
006310******************************************************************
006320 PROCEDURE               DIVISION.
006330************
006340*           *
006350* 初期処理   *
006360*           *
006370************
006380     PERFORM 初期化.
006390************
006400*           *
006410* 主処理     *
006420*           *
006430************
006440     PERFORM 制御情報取得.
006450     PERFORM 連結項目退避.
006470     PERFORM 作業ファイル作成.
006480     PERFORM クライアントファイル作成.
006500************
006510*           *
006520* 終了処理   *
006530*           *
006540************
006550     PERFORM 終了処理.
006560     MOVE ZERO TO PROGRAM-STATUS.
006570     EXIT PROGRAM.
006580*
006590*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
006600*================================================================*
006610 初期化 SECTION.
006620*
006630     PERFORM ファイルオープン.
006640*
006650*================================================================*
006660 ファイルオープン SECTION.
006670*
006680     OPEN INPUT 受診者情報Ｆ
006690         MOVE NC"受" TO ファイル名.
006700         PERFORM オープンチェック.
006630     OPEN INPUT 生保情報Ｆ.
006640         MOVE NC"生保" TO ファイル名.
006650         PERFORM オープンチェック.
004570     OPEN INPUT レセプトＦ.
004580         MOVE NC"レセ" TO ファイル名.
004590         PERFORM オープンチェック.
006710     OPEN INPUT 負傷データＦ.
006720             MOVE NC"負傷" TO ファイル名.
006730             PERFORM オープンチェック.
006740     OPEN INPUT 施術記録Ｆ.
006750             MOVE NC"施記" TO ファイル名.
006760             PERFORM オープンチェック.
006770     OPEN INPUT 施術所情報マスタ
006780         MOVE NC"施情" TO ファイル名.
006790         PERFORM オープンチェック.
006800     OPEN INPUT 元号マスタ.
006810             MOVE NC"元号" TO ファイル名.
006820             PERFORM オープンチェック.
006860     OPEN INPUT 制御情報マスタ.
006870             MOVE NC"制御" TO ファイル名.
006880             PERFORM オープンチェック.
004870     OPEN INPUT 保険者特別負担マスタ.
004880         MOVE NC"保険者特別負担マスタ" TO ファイル名.
004890         PERFORM オープンチェック.
006980*
007091
007094* フロッピー挿入確認
           IF 連入－固定フラグ = 1
002890        MOVE "C:\makishisys\DaiCHO.txt" TO FD-NAME
           ELSE
007096        STRING 連入－ドライブ     DELIMITED BY SIZE
007097               ":\DaiCHO.txt"     DELIMITED BY SIZE
007098              INTO FD-NAME
007099        END-STRING
           END-IF.
007100
007101     OPEN OUTPUT 会提出ファイル.
003770     IF 状態キー  NOT =  "00"
002690         MOVE  "指定されたドライブがないか、" TO 連メ７－メッセージ１
               MOVE  "書き込むことが出来ません。" TO 連メ７－メッセージ２
002700         CALL   "MSG007"
002710         CANCEL "MSG007"
007240         PERFORM ファイル閉鎖
003820         MOVE 99 TO PROGRAM-STATUS
003830         EXIT PROGRAM
007108     ELSE
007109         PERFORM オープンチェック
007110         CLOSE 会提出ファイル
007111         CALL "delfile"  WITH C LINKAGE
007112                         USING BY REFERENCE FD-NAME
007113     END-IF.
007160*================================================================*
007170 オープンチェック SECTION.
007180*
007190     IF 状態キー  NOT =  "00"
007200         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
007210         DISPLAY NC"状態キー：" 状態キー         UPON CONS
007220         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
004380*-----------------------------------------*
004390         CALL "actcshm"  WITH C LINKAGE
004400*-----------------------------------------*
007230         ACCEPT  キー入力 FROM CONS
007240         PERFORM ファイル閉鎖
007250         MOVE 99 TO PROGRAM-STATUS
007260         EXIT PROGRAM.
007270*================================================================*
007280 ファイル閉鎖 SECTION.
007290*
007300     CLOSE 受診者情報Ｆ        施術所情報マスタ       負傷データＦ
007310           元号マスタ          制御情報マスタ         レセプトＦ
                 生保情報Ｆ          保険者特別負担マスタ.
007340*================================================================*
007350 終了処理 SECTION.
007360*
007370     PERFORM ファイル閉鎖.
007380*================================================================*
007390 エラー表示 SECTION.
007400*
007410     DISPLAY NC"状態キー" 状態キー  UPON CONS.
007420     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
007430     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
007440     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
004910*-----------------------------------------*
004920     CALL "actcshm"  WITH C LINKAGE.
004930*-----------------------------------------*
007450     ACCEPT  キー入力 FROM CONS.
007460     PERFORM ファイル閉鎖.
007470     MOVE 99 TO PROGRAM-STATUS.
007480     EXIT PROGRAM.
009500*================================================================*
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
009620     
009621     NOT INVALID KEY
009622          MOVE 施情－新柔整師番号   TO 会提－柔整師番号
009623          MOVE 施情－代表者名       TO 会提－柔整師名
009624     END-READ.
009630*
009810*================================================================*
009820 連結項目退避 SECTION.
009830*
009840     MOVE 連入－請求和暦 TO 請求和暦ＷＲ.
009850     MOVE 連入－請求年   TO 請求年ＷＲ.
009860     MOVE 連入－請求月   TO 請求月ＷＲ.
010650*================================================================*
010740 レセプトＦ読込 SECTION.
010750*
010760     READ レセプトＦ NEXT
010770     AT END
010780         MOVE "YES" TO 終了フラグ
010790     END-READ.
010800*================================================================*
010810 健保レコードセット SECTION.
010820*
010830*/料金
010843     MOVE レセ－合計          TO 作１－合計金額.
010850     MOVE レセ－一部負担金    TO 作１－一部負担金.
010860     MOVE レセ－請求金額      TO 作１－請求金額.
011125*================================================================*
011130 助成レコードセット SECTION.
011140*
011150*/料金
011170     MOVE レセ－一部負担金    TO 作１－合計金額.
011180     MOVE レセ－受給者負担額  TO 作１－一部負担金.
011190     MOVE レセ－助成請求金額  TO 作１－請求金額.
      */兵庫県の助成
           IF 受－費用負担者番号助成(3:2) = "28"
              IF 受－助成負担金免除 = 1
                 MOVE "1"       TO 作１－兵庫限度
              ELSE
010460           MOVE 受－助成種別           TO 保特－保険種別
010470           MOVE 受－費用負担者番号助成 TO 保特－保険者番号
010460           MOVE 99999                  TO 保特－開始和暦年月
007550           START 保険者特別負担マスタ   KEY IS < 保特－保険種別
                                                       保特－保険者番号
                                                       保特－開始和暦年月
                                                       REVERSED
                 END-START
006220           IF 状態キー = "00"
010480              READ 保険者特別負担マスタ NEXT
010510              NOT AT END
                       IF (受－助成種別           = 保特－保険種別) AND
                          (受－費用負担者番号助成 = 保特－保険者番号)
010520                    EVALUATE 保特－負担区分
                          WHEN 14
                              IF 受－助成負担金免除 = 0
                                 MOVE 保特－負担一定金額(3:1) TO 作１－兵庫限度
                              ELSE
                                 MOVE 保特－事務手数料(2:1)   TO 作１－兵庫限度
                              END-IF
                          END-EVALUATE
                       END-IF
                    END-READ
                 END-IF
              END-IF
           END-IF.
           IF 作１－兵庫限度 = SPACE
013690        PERFORM 負担率取得１４１０
019540        EVALUATE 負担率Ｗ
              WHEN 10
019550            MOVE "A"       TO 作１－兵庫限度
              WHEN 20
019550            MOVE "B"       TO 作１－兵庫限度
              END-EVALUATE
           END-IF.
011405*
015020*================================================================*
015030 負担率取得１４１０ SECTION.
015040*
015050* 受診者Ｆ READ中
015060* 平成14/10～
015070     MOVE ZERO  TO 負担率Ｗ.
015090*
015100     MOVE SPACE TO 連率－負担率取得キー.
015110     INITIALIZE 連率－負担率取得キー.
015120     MOVE 受－施術和暦年月 TO 連率－施術和暦年月.
015130     MOVE 受－患者コード   TO 連率－患者コード.
015140*
015150     CALL   "HUTANRIT".
015160     CANCEL "HUTANRIT".
015170*
015220* / 助成 /
015240     MOVE 連率－助成負担率  TO 負担率Ｗ.
015360*
011406*================================================================*
011410 共通レコードセット SECTION.
011420*
026990     MOVE 受－患者番号               TO 作１－患者番号.
026990     MOVE 受－枝番                   TO 作１－枝番.
027000     MOVE 受－施術和暦年月           TO 作１－施術和暦年月.
027764     MOVE 受－受益者番号助成         TO 作１－受給者番号.
011520*/続柄０：本人、０以外の自然数：家族
011590     MOVE 受－本人家族区分           TO 作１－本人家族区分.
011585*/名前
011590     MOVE 受－患者カナ               TO 作１－患者カナ.
           UNSTRING 受－患者カナ DELIMITED BY SPACE
               INTO 患者姓カナＷ 患者名カナＷ
           END-UNSTRING.
           MOVE 患者姓カナＷ               TO 作１－患者姓カナ.
           MOVE 患者名カナＷ               TO 作１－患者名カナ.
011600     MOVE 受－患者氏名               TO 作１－患者氏名.
011620     MOVE 受－被保険者氏名           TO 作１－被保険者氏名.
           IF 受－保険種別 = 70 OR 80 OR 90 OR 85
011600        MOVE 受－患者氏名            TO 作１－被保険者氏名
           END-IF.
           IF 受－公費種別 = ZERO
               EVALUATE 受－特別区分
               WHEN 1
                   MOVE 5                  TO 作１－負担区分
               WHEN 2
                   MOVE 6                  TO 作１－負担区分
               WHEN 3
                   MOVE 7                  TO 作１－負担区分
               WHEN 6
                   MOVE 4                  TO 作１－負担区分
               WHEN OTHER
                   MOVE 3                  TO 作１－負担区分
               END-EVALUATE
           ELSE
               EVALUATE 受－特別区分
               WHEN 1
                   MOVE 1                  TO 作１－負担区分
               WHEN 2
                   MOVE 8                  TO 作１－負担区分
               WHEN 3
                   MOVE 2                  TO 作１－負担区分
               END-EVALUATE
           END-IF.
011870*/保険証記号番号
           MOVE SPACE TO 連暗号複合－暗号情報.
      *
      *    / 連暗号複合－入力情報セット /
           MOVE 受－記号       TO 連暗号複合－記号.
           MOVE 受－番号       TO 連暗号複合－番号.
           MOVE 受－暗号化項目 TO 連暗号複合－暗号化項目.
      *     
           CALL   複合プログラム名Ｗ.
           CANCEL 複合プログラム名Ｗ.
      *
           MOVE 連暗号複合－複合した記号 TO 記号Ｗ.
           MOVE 連暗号複合－複合した番号 TO 番号Ｗ.
011880     IF (印刷記号Ｗ(1:1) NOT = NC"＊")
011890         MOVE 記号Ｗ               TO 作１－記号
011900     END-IF.
011910     IF (番号Ｗ(1:1) NOT = "*") AND (番号Ｗ(1:2) NOT = "＊")
011920         MOVE 番号Ｗ               TO 作１－番号
011930     END-IF.
      *
           MOVE レセ－レセ実日数         TO 作１－日数.
015850*================================================================*
015860 負傷データＦセット SECTION.
015870******************************************************************
015880* 最大同時負傷数を求める。
015890******************************************************************
015900     MOVE 受－施術和暦   TO 負－施術和暦.
015910     MOVE 受－施術年     TO 負－施術年.
015920     MOVE 受－施術月     TO 負－施術月.
015930     MOVE 受－患者番号   TO 負－患者番号.
015940     MOVE 受－枝番       TO 負－枝番.
015950     READ 負傷データＦ
015960     INVALID KEY
015970         CONTINUE
015980     NOT INVALID KEY
015990         MOVE 負－部位数 TO 作１－部位数
016140     END-READ.
016150*================================================================*
016160 データチェック SECTION.
016170*
016180     MOVE SPACE  TO 実行キーＷ.
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
019880              MOVE "YES"  TO 実行キーＷ
                 END-READ
019900        ELSE
019910           MOVE SPACE  TO 実行キーＷ
              END-IF
019950     END-IF.
      */ 助成請求額が０の時は対象外 /140910
           IF (レセ－レセ種別 = 3) AND
              (レセ－助成請求金額 = ZERO)
019910        MOVE SPACE  TO 実行キーＷ
027840     END-IF.
016640*
022430*================================================================*
022440 遅延処理 SECTION.
022450*
022460     PERFORM VARYING 遅延カウンタ FROM 1 BY 1
022470             UNTIL 遅延カウンタ > 遅延回数Ｗ
022480         MOVE "YES" TO 遅延フラグ
022490     END-PERFORM.
022500*
022510*================================================================*
022520 制御情報取得 SECTION.
022530*
022540     MOVE ZERO TO 制－制御区分
022550     READ 制御情報マスタ
022560     NOT INVALID KEY
022570         MOVE 制－遅延回数 TO 遅延回数Ｗ
022580     END-READ.
022841*
026550*================================================================*
026560 作業ファイル作成 SECTION.
026570*
026580     OPEN OUTPUT 作業ファイル１.
026590         MOVE NC"作１" TO ファイル名.
026600         PERFORM オープンチェック.
026610*
026670     PERFORM 遅延処理.
026710*
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
026860         PERFORM レセプトＦ読込
026861
026874         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
005600                       ( レセ－請求和暦 NOT = 請求和暦ＷＲ ) OR
005610                       ( レセ－請求年   NOT = 請求年ＷＲ   ) OR
005620                       ( レセ－請求月   NOT = 請求月ＷＲ   )
026910*
026920            PERFORM データチェック
      */            労災・自賠・生保は含めない
                  IF レセ－レセ種別 = 4 OR 5 OR 6 OR 7
                     MOVE SPACE TO 実行キーＷ
                  END-IF
026930            IF 実行キーＷ = "YES"
026940                MOVE SPACE TO 作１－レコード
026950                INITIALIZE    作１－レコード
                      EVALUATE TRUE
004273                WHEN レセ－レセ種別 = 1
027080                    EVALUATE 受－保険種別
027170*               /国保
027090                    WHEN 01
                              IF 受－保険者番号(3:1) = 3
027120                            MOVE  40  TO 作１－並び区分
                              ELSE
027120                            MOVE  41  TO 作１－並び区分
                              END-IF
027170*               /社保
027180                    WHEN 02
027190                        MOVE  10      TO 作１－並び区分
027200*               /組合
027210                    WHEN 03
027220                        MOVE  20      TO 作１－並び区分
027230*               /共済
027240                    WHEN 04
027250                        MOVE  30      TO 作１－並び区分
027260*               /日雇
027270                    WHEN 06
027280                        MOVE  10      TO 作１－並び区分
027290*               /船員
027300                    WHEN 07
027310                        MOVE  10      TO 作１－並び区分
027320*               /退職
027330                    WHEN 08
027340                        MOVE  50      TO 作１－並び区分
027350*               /自衛官
027360                    WHEN 09
027370                        MOVE  31      TO 作１－並び区分
027410*               /労災
027420*                    WHEN 70
027430*                        MOVE  90      TO 作１－並び区分
027410*               /自賠
027420*                    WHEN 80
027430*                        MOVE  91      TO 作１－並び区分
027410*               /自費
027420*                    WHEN 90
027430*                        MOVE  92      TO 作１－並び区分
027410*               /生保
027420*                    WHEN 85
027430*                        MOVE  93      TO 作１－並び区分
027440                    END-EVALUATE
027450                    MOVE 受－保険者番号   TO 作１－保険者番号
      *                    IF レセ－レセ種別 = 7
      *                        MOVE 受－施術和暦年月 TO 生保－施術和暦年月
      *                        MOVE 受－患者コード   TO 生保－患者コード
      *                        READ 生保情報Ｆ
      *                        NOT INVALID KEY
003950*                           MOVE 生保－負担者番号   TO 作１－保険者番号
      *                        END-READ
      *                    END-IF
028390                    PERFORM 健保レコードセット
      *
004525                WHEN レセ－レセ種別 = 2
026961* /後高
026970                    MOVE 70                  TO 作１－並び区分
026980                    MOVE 受－費用負担者番号  TO 作１－保険者番号
028420                    PERFORM 健保レコードセット
027510*
004525                WHEN レセ－レセ種別 = 3
027540                    EVALUATE 受－助成種別
027580*               /４１老人
027590                    WHEN 51
027600                        MOVE 81 TO 作１－並び区分
027610*               /母子
027620                    WHEN 52
027630                        MOVE 60 TO 作１－並び区分
027640*               /身障
027650                    WHEN 53
027660                        MOVE 61 TO 作１－並び区分
027670*               /被爆：
027680                    WHEN 54
027690                        MOVE 82 TO 作１－並び区分
027700*               /乳幼児
027710                    WHEN 55
027720                        MOVE 80 TO 作１－並び区分
027730*               /その他
027740                    WHEN 60
027750                        MOVE 83 TO 作１－並び区分
027760                    END-EVALUATE
027761*
027450                    MOVE 受－保険者番号         TO 作１－親保険者番号
027764                    MOVE 受－費用負担者番号助成 TO 作１－保険者番号
                          IF (受－助成種別 = "53") AND
                             (受－費用負担者番号助成(1:4) = "3926")
027764                        MOVE "99"    TO 作１－保険者番号(1:2)
                          END-IF
028460                    PERFORM 助成レコードセット
027810                END-EVALUATE
027820*
028310                PERFORM 負傷データＦセット
028350                PERFORM 共通レコードセット
027767                PERFORM 作業ファイル書込
027840            END-IF
027850            PERFORM レセプトＦ読込
027860         END-PERFORM
027861     ELSE
027862         MOVE  NC"　　データが０件です。確認して下さい" TO 連メ－メッセージ
027863         CALL   "MSG001"
027864         CANCEL "MSG001"
027865         PERFORM ファイル閉鎖
027866         MOVE 99 TO PROGRAM-STATUS
027867         EXIT PROGRAM
027870     END-IF.
027872
027880     CLOSE 作業ファイル１.
027890*================================================================*
027900 作業ファイル書込 SECTION.
027910*
027920     WRITE 作１－レコード
027930     IF 状態キー  NOT =  "00"
027940         MOVE NC"作１"  TO ファイル名
027950         PERFORM エラー表示
027960     END-IF.
027970*================================================================*
027980 クライアントファイル作成 SECTION.
027990*
028000     OPEN INPUT 作業ファイル１.
028010         MOVE NC"作１" TO ファイル名.
028020         PERFORM オープンチェック.
004260     OPEN OUTPUT 会提出ファイル.
004270         MOVE NC"会提出ファイル" TO ファイル名.
004280         PERFORM オープンチェック.
028070*
028080*/並び順を、保険種別、提出保険者番号、患者コード、施術年月順にする。
028090     MOVE ZERO  TO 作１－並び区分
028100     MOVE SPACE TO 作１－保険者番号
028120     MOVE ZERO  TO 作１－施術和暦年月
028120     MOVE ZERO  TO 作１－負担区分
028120     MOVE ZERO  TO 作１－本人家族区分
028110     MOVE SPACE TO 作１－患者カナ
028110     MOVE SPACE TO 作１－患者コード
028130     START 作業ファイル１ KEY IS >= 作１－並び区分    
028140                                    作１－保険者番号  
028160                                    作１－施術和暦年月
028150                                    作１－負担区分    
                                          作１－本人家族区分
                                          作１－患者カナ    
                                          作１－患者コード  
028170     END-START.
028180     IF 状態キー = "00"
028190         MOVE SPACE  TO 終了フラグ
028200         PERFORM 作業ファイル１読込
002460         IF  終了フラグ = "YES"
002470             MOVE  NC"　データが０件です。確認して下さい。" TO 連メ－メッセージ
002480             CALL   "MSG001"
002490             CANCEL "MSG001"
002500             PERFORM ファイル閉鎖
002510             MOVE 99 TO PROGRAM-STATUS
002520             EXIT PROGRAM
002530         END-IF
028214         PERFORM UNTIL ( 終了フラグ = "YES" )
028220*
028320            PERFORM 施術所情報取得
028320            PERFORM 提出レコードセット
                  PERFORM 会提出ファイル書込
028340*
028530            PERFORM 作業ファイル１読込
028550         END-PERFORM
           ELSE
002470         MOVE  NC"　データが０件です。確認して下さい。" TO 連メ－メッセージ
002480         CALL   "MSG001"
002490         CANCEL "MSG001"
002500         PERFORM ファイル閉鎖
002510         MOVE 99 TO PROGRAM-STATUS
002520         EXIT PROGRAM
028571     END-IF.
028573*
028580     CLOSE 作業ファイル１ 会提出ファイル.
028590*================================================================*
028600 提出レコードセット SECTION.
028610*
           ADD 1 TO 連番Ｗ.
           MOVE 連番Ｗ             TO 会提－連番.
           MOVE 作１－患者コード   TO 会提－患者番号.
           MOVE 作１－患者氏名     TO 会提－患者氏名.
           MOVE 作１－被保険者氏名 TO 会提－被保険者氏名.
           MOVE 作１－本人家族区分 TO 会提－本人家族.
           MOVE 作１－受給者番号   TO 会提－受給者番号.
           MOVE 作１－保険者番号   TO 会提－保険者番号.
           MOVE 作１－部位数       TO 会提－部位数.
           MOVE 作１－日数         TO 会提－日数.
           MOVE 作１－合計金額     TO 会提－合計金額.
           MOVE 作１－一部負担金   TO 会提－一部負担金.
           MOVE 作１－請求金額     TO 会提－請求金額.
           MOVE 作１－施術年月     TO 会提－施術年月.
           MOVE 作１－親保険者番号 TO 会提－親保険者番号.
           MOVE 作１－患者姓カナ   TO 会提－患者姓カナ.
           MOVE 作１－患者名カナ   TO 会提－患者名カナ.
           MOVE 作１－記号         TO 会提－保険証記号.
           MOVE 作１－番号         TO 会提－保険証番号.
           MOVE 作１－負担区分     TO 会提－負担区分.
           MOVE 作１－兵庫限度     TO 会提－兵庫限度.
      *
           MOVE "," TO 会提－区切１ 会提－区切２ 会提－区切３ 会提－区切４ 会提－区切５
                       会提－区切６ 会提－区切７ 会提－区切８ 会提－区切９ 会提－区切１０
                       会提－区切１１ 会提－区切１２ 会提－区切１３ 会提－区切１４
                       会提－区切１５ 会提－区切１６ 会提－区切１７ 会提－区切１８
                       会提－区切１９ 会提－区切２０ 会提－区切２１ 会提－区切２２.
      *
028590*================================================================*
028600 作業ファイル１読込 SECTION.
028610*
028620     READ 作業ファイル１ NEXT
028630     AT END
028640         MOVE "YES" TO 終了フラグ
028650     END-READ.
028695*
028696*================================================================*
028697 会提出ファイル書込 SECTION.
028698*
028699     WRITE 会提－レコード
028700     END-WRITE.
028701     
028702     IF 状態キー  NOT =  "00"
028703         MOVE NC"会提出ファイル" TO ファイル名
028704         PERFORM エラー表示
028705     END-IF.
028706*================================================================*
028707******************************************************************
028708 END PROGRAM YDT101.
028709******************************************************************
