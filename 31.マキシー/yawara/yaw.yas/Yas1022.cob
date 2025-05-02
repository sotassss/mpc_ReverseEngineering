000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAS1022.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090* ＩＦ請求サポートセンター 会提出データ作成【FPD書込】
000100* 請求年月Ver.
000110*      MED = YAS580 
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2021-09-09
000140 DATE-COMPILED.          2021-09-09
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
000293     SELECT  長期継続者Ｆ    ASSIGN      TO        CHOKEIL
000294                             ORGANIZATION             IS INDEXED
000295                             ACCESS MODE              IS DYNAMIC
000296                             RECORD KEY               IS 長継－施術和暦年月
000297                                                         長継－患者コード
000298                             ALTERNATE RECORD KEY     IS 長継－患者コード
000299                                                         長継－施術和暦年月
000300                             FILE STATUS              IS 状態キー
000301                             LOCK      MODE           IS AUTOMATIC.
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
000650     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000660                             ORGANIZATION             IS  INDEXED
000670                             ACCESS MODE              IS  DYNAMIC
000680                             RECORD KEY               IS  名－区分コード
000690                                                          名－名称コード
000700                             FILE STATUS              IS  状態キー
000710                             LOCK        MODE         IS  AUTOMATIC.
001290*
000931     SELECT  作業ファイル２  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W1022L.DAT"
000932                             ORGANIZATION             IS  INDEXED
000940                             ACCESS                   IS  DYNAMIC
000950                             RECORD      KEY          IS  作２－施術和暦年月
000960                                                          作２－患者コード
000970                                                          作２－保険種別
000971                             ALTERNATE RECORD KEY     IS  作２－順番
000980                             FILE        STATUS       IS  状態キー
000990                             LOCK        MODE         IS  AUTOMATIC.
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
      *
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
001690*                           ［ＲＬ＝  １２８］
001700 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
001710     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
001750*
001760 FD  施術所情報マスタ    BLOCK   CONTAINS   1   RECORDS.
001770     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
001780*                           ［ＲＬ＝  １２８］
001790 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001800     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001840*                           ［ＲＬ＝  ２５６］
001850 FD  制御情報マスタ      BLOCK   CONTAINS   1   RECORDS.
001860     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
000686*                           ［ＲＬ＝  １２８］
000687 FD  長期継続者Ｆ          BLOCK   CONTAINS   1   RECORDS.
000688     COPY CHOKEI          OF  XFDLIB  JOINING   長継   AS  PREFIX.
000700*                           ［ＲＬ＝  ３２０］
000710 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
000720     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
000721*                           ［ＲＬ＝  ２５６］
000722 FD  市町村マスタ          BLOCK   CONTAINS   1   RECORDS.
000723     COPY SITYOSN        OF  XFDLIB  JOINING   市   AS  PREFIX.
001070*                           ［ＲＬ＝  １２８］
001080 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
001090     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
004330*                           ［ＲＬ＝  32］
001581 FD  作業ファイル２ RECORD  CONTAINS 80 CHARACTERS.
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
001594         05  作２－分類コード              PIC 9(2).
001261         05  作２－県コード                PIC X(2).
001595         05  作２－順番                    PIC 9(4).
001595         05  作２－保険種別順番            PIC 9(1).
001595         05  作２－ソート番号              PIC 9(4).
001595         05  作２－データ表示順            PIC 9(4).
001596         05  作２－本人家族区分            PIC 9.
001597         05  作２－保険者番号              PIC X(8).
001591         05  作２－負担割合                PIC 9(2).
               05  作２－費用額                  PIC 9(6).
               05  作２－負担額                  PIC 9(6).
               05  作２－請求額                  PIC 9(6).
001591         05  作２－分類保険種別            PIC 9(2).
001597         05  作２－分類保険者番号          PIC X(8).
001260         05  作２－併区分                  PIC 9.
001595         05  作２－表示フラグ              PIC 9(1).
001598         05  FILLER                        PIC X(7).
004471*
004477 FD  会提出ファイル RECORD IS VARYING IN SIZE
004478               FROM 1 TO 780 DEPENDING ON 文字カウンタ.
004479 01 会提－レコード.
004480   03 会提－レコードデータ.
            05 会提－データ PIC X OCCURS 1 TO 780 TIMES DEPENDING ON 文字カウンタ.
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
004620 01 カウンタ                           PIC 9(2) VALUE ZERO.
004621 01 カウンタ２                         PIC 9(2) VALUE ZERO.
004630 01 文字カウンタ                       PIC 9(4) VALUE ZERO.
003630 01 文字ＣＮＴ                         PIC 9(2) VALUE ZERO.
004630 01 桁位置Ｗ                           PIC 9(4) VALUE ZERO.
       01 外字フラグ                         PIC 9(1) VALUE ZERO.
       01 表示フラグＷ                       PIC 9(1) VALUE ZERO.
004640 01 通番Ｗ.
          03 通番ＷＰ                        PIC 9(5) VALUE ZERO.
001220 01 府県Ｗ                             PIC X(2) VALUE SPACE.
001220 01 県名Ｗ.
          03 県名ＷＰ                        PIC X(8) VALUE SPACE.
004660 01 柔整師コードＷ.
004670    03 協Ｗ                            PIC N(1) VALUE SPACE.
004680    03 柔整師コード数字Ｗ.
004690       05 柔整師コード数字１Ｗ         PIC X(4) VALUE SPACE.
004700       05 会員番号コードＷ             PIC X(3) VALUE SPACE.
       01 会員番号Ｗ                         PIC X(8) VALUE SPACE.
       01 システム日付Ｗ.
          03 西暦Ｗ                          PIC X(2) VALUE SPACE.
          03 年月日Ｗ                        PIC X(6) VALUE SPACE.
          03 時間Ｗ                          PIC X(6) VALUE SPACE.
       01 時間ＷＰ.
          03 時分秒Ｗ                        PIC X(6) VALUE SPACE.
          03 秒以下Ｗ                        PIC X(2) VALUE SPACE.
004710*
004720 01 請求西暦年Ｗ                       PIC 9(4) VALUE ZERO.
004730 01 請求和暦年月ＷＲ.
004740    03 請求和暦ＷＲ                    PIC 9(1) VALUE ZERO.
004750    03 請求年月ＷＲ.
004760       05 請求年ＷＲ                   PIC 9(2) VALUE ZERO.
004770       05 請求月ＷＲ                   PIC 9(2) VALUE ZERO.
004780*
004790 01 西暦年Ｗ                           PIC 9(4) VALUE ZERO.
004800* 01 和暦Ｗ                             PIC 9(1) VALUE ZERO.
004810* 01 年Ｗ                               PIC 9(2) VALUE ZERO.
004820 01 和暦年月日Ｗ.
004830   03 和暦年月Ｗ.
004840      05 和暦Ｗ                        PIC 9(1) VALUE ZERO.
004850      05 年Ｗ                          PIC 9(2) VALUE ZERO.
004860      05 月Ｗ                          PIC 9(2) VALUE ZERO.
004870   03 日Ｗ                             PIC 9(2) VALUE ZERO.
004880 01 日付１０桁Ｗ                       PIC X(10) VALUE SPACE.
004890 01 開始和暦年月日Ｗ.
004900   03 開始和暦Ｗ                       PIC 9(1) VALUE ZERO.
004910   03 開始年Ｗ                         PIC 9(2) VALUE ZERO.
004920   03 開始月Ｗ                         PIC 9(2) VALUE ZERO.
004930   03 開始日Ｗ                         PIC 9(2) VALUE ZERO.
004940 01 終了和暦年月日Ｗ.
004950   03 終了和暦Ｗ                       PIC 9(1) VALUE ZERO.
004960   03 終了年Ｗ                         PIC 9(2) VALUE ZERO.
004970   03 終了月Ｗ                         PIC 9(2) VALUE ZERO.
004980   03 終了日Ｗ                         PIC 9(2) VALUE ZERO.
004990 01 実日数Ｗ                           PIC 9(2) VALUE ZERO.
005070 01 負担割合Ｗ                         PIC 9(3) VALUE ZERO.
005000 01 部位ＣＮＴ                         PIC 9(2) VALUE ZERO.
005010 01 部位ＣＮＴ２                       PIC 9(2) VALUE ZERO.
005020 01 同時負傷数Ｗ                       PIC 9(2) VALUE ZERO.
005030 01 最大同時負傷数Ｗ                   PIC 9(2) VALUE ZERO.
005040 01 継続部位数Ｗ                       PIC 9    VALUE ZERO.
005050 01 最大継続部位数Ｗ                   PIC 9    VALUE ZERO.
005060 01 負傷終了和暦年月日Ｗ               PIC 9(7) VALUE ZERO.
005070 01 負傷種別Ｗ                         PIC 9(2) VALUE ZERO.
005080 01 部位タイプＷ                       PIC 9    VALUE ZERO.
003520 01 区切文字Ｒ                         PIC X(1) VALUE X"22".
003520 01 区切文字ＲＷ                       PIC X(2) VALUE X"2222".
       01 区切文字Ｒ２.
          03 区切文字１Ｐ                    PIC X(1) VALUE X"22".
          03 区切文字２Ｐ                    PIC X(1) VALUE ",".
          03 区切文字３Ｐ                    PIC X(1) VALUE X"22".
       01 区切文字Ｒ３.
          03 区切文字１Ｐ                    PIC X(1) VALUE X"22".
          03 区切文字２Ｐ                    PIC X(1) VALUE ",".
005090*
005100 01 遅延フラグ                         PIC X(3) VALUE SPACE.
005110 01 遅延ＣＮＴ                         PIC 9(5) VALUE ZERO.
005120 01 遅延カウンタ                       PIC 9(4) VALUE ZERO.
005130 01 遅延回数Ｗ                         PIC 9(4) VALUE ZERO.
005140*
005440** 負傷原因・長期理由印刷区分用
005450 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
005460 01 長期理由印刷区分Ｗ                 PIC 9 VALUE ZERO.
005470*
005150 01 英数字項目Ｗ.
005160   03 英数字項目ＸＷ                   PIC X(70) VALUE SPACE.
005170 01 日本語項目Ｗ.
005180   03 日本語項目ＮＷ                   PIC N(35) VALUE SPACE.
005190 01 英数字項目２Ｗ                     PIC X(70) VALUE SPACE.
005200*
005388** 請求額右詰め用
005389 01 請求額ＷＴ.
005390    03 請求額左詰めＷ.
005391      05 請求額左詰めＷ１          PIC X OCCURS 6 VALUE SPACE.
005392    03 請求額右詰めＷ.
005393      05 請求額右詰めＷ１          PIC X OCCURS 6 VALUE ZERO.
005394    03 請求額数字Ｗ                PIC 9(6)  VALUE ZERO.
005395    03 請求額Ｗ                    PIC X(6)  VALUE SPACE.
005630*
005640 01 施術西暦年月日Ｗ.
          03 施術西暦年月Ｗ.
005660       05 施術西暦年Ｗ                 PIC 9(4)  VALUE ZERO.
005670       05 施術月Ｗ                     PIC 9(2)  VALUE ZERO.
005680    03 施術日Ｗ                        PIC 9(2)  VALUE ZERO.
001240*
001220 01 保険種別Ｗ                         PIC 9(2) VALUE ZERO.
001597 01 保険者番号Ｗ                       PIC X(8) VALUE SPACE.
001250 01 請求先名称Ｗ.
001260    03 印刷請求先名称Ｗ                PIC X(70) VALUE SPACE.
001270    03 FILLER                          PIC X(10) VALUE SPACE.
005690*
002251 01 定期修正ＦＤＷ                     PIC X VALUE SPACE.
005723 01 FD-NAME                            PIC X(80) VALUE SPACE.
005724*
005725 01 氏名Ｗ                             PIC X(30)  VALUE SPACE.
005725 01 住所Ｗ                             PIC X(60)  VALUE SPACE.
004780*
007570 01 記号Ｗ.
007580    03 印刷記号Ｗ                      PIC N(10)  VALUE SPACE.
007660*
007670 01 番号Ｗ.
007680    03 印刷番号Ｗ                      PIC X(20)  VALUE SPACE.
005690*
005100 01 負傷原因フラグ                     PIC 9(1) VALUE ZERO.
005100 01 長期理由フラグ                     PIC 9(1) VALUE ZERO.
005100 01 経過フラグ                         PIC 9(1) VALUE ZERO.
      *
002251 01 レコードデータＷ                   PIC X(780) VALUE SPACE.
014774*
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      */ZIP圧縮
       01  パラ１Ｗ PIC X(2001) VALUE SPACE.
       01  プログラム名Ｗ PIC X(8)  VALUE "zip".
005723 01 圧縮ファイル名Ｗ                   PIC X(80) VALUE SPACE.
005723 01 元ファイル名Ｗ                     PIC X(80) VALUE SPACE.
000491* C 連携用
000492 01 フォルダ名Ｗ        PIC X(81).
       01 レセまとめ対象Ｆ                   PIC 9 VALUE ZERO.
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
       01 連入－画面情報ＹＡＳ１００ IS EXTERNAL.
          03 連入－ドライブ                  PIC X(1).
      *
      *0:通常 1:固定パス
       01 連入－固定フラグ３２１１ IS EXTERNAL.
          03 連入－固定フラグ                  PIC 9(1).
006830*
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
           PERFORM 施術所情報取得.
006480     PERFORM クライアントファイル作成.
006500************
006510*           *
006520* 終了処理   *
006530*           *
006540************
006550     PERFORM 終了処理.
      *
           PERFORM 圧縮ファイル作成.
      *
006560     MOVE ZERO TO PROGRAM-STATUS.
006570     EXIT PROGRAM.
006580*
006590*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
006600*================================================================*
006610 初期化 SECTION.
006620*
006630     PERFORM ファイルオープン.
006640*
000501     MOVE SPACE TO フォルダ名Ｗ.
000502     MOVE "C:\makishisys\mcb_rese\01_jyu\" TO フォルダ名Ｗ.
000503     CALL "delfile2" WITH C LINKAGE USING BY REFERENCE フォルダ名Ｗ.
           CANCEL "delfile2".
006650*================================================================*
006660 ファイルオープン SECTION.
006670*
006680     OPEN INPUT 受診者情報Ｆ
006690         MOVE NC"受" TO ファイル名.
006700         PERFORM オープンチェック.
004570     OPEN INPUT レセプトＦ.
004580         MOVE NC"レセ" TO ファイル名.
004590         PERFORM オープンチェック.
006710     OPEN INPUT 負傷データＦ.
006720             MOVE NC"負傷" TO ファイル名.
006730             PERFORM オープンチェック.
006770     OPEN INPUT 施術所情報マスタ
006780         MOVE NC"施情" TO ファイル名.
006790         PERFORM オープンチェック.
006800     OPEN INPUT 元号マスタ.
006810             MOVE NC"元号" TO ファイル名.
006820             PERFORM オープンチェック.
006860     OPEN INPUT 制御情報マスタ.
006870             MOVE NC"制御" TO ファイル名.
006880             PERFORM オープンチェック.
002647     OPEN INPUT   長期継続者Ｆ.
002648         MOVE NC"長期継続者Ｆ" TO ファイル名.
002649         PERFORM オープンチェック.
002550     OPEN INPUT 保険者マスタ.
002560         MOVE NC"保険者マスタ" TO ファイル名.
002570         PERFORM オープンチェック.
002571     OPEN INPUT 市町村マスタ
002572         MOVE NC"市町村" TO ファイル名.
002573         PERFORM オープンチェック.
003540     OPEN INPUT 名称マスタ.
003550         MOVE NC"名称マスタ"   TO ファイル名.
003560         PERFORM オープンチェック.
006980*
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
007330           長期継続者Ｆ        保険者マスタ           市町村マスタ
                 名称マスタ          会提出ファイル.
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
009620*     
009621     NOT INVALID KEY
009622          MOVE 施情－接骨師会会員番号     TO 会員番号Ｗ
009623
009624     END-READ.
      *
           ACCEPT 年月日Ｗ FROM DATE.
      *    /* 1980～2079年の間で設定 */
           IF 年月日Ｗ(1:2) > 80
               MOVE 19 TO 西暦Ｗ
           ELSE
               MOVE 20 TO 西暦Ｗ
           END-IF.
           ACCEPT 時間ＷＰ FROM TIME.
           MOVE 時分秒Ｗ   TO 時間Ｗ.
009630*
007094* フロッピー挿入確認
           IF 連入－固定フラグ = 1
002890        STRING "C:\makishisys\mcb_rese\01_jyu\"      DELIMITED BY SIZE
                     会員番号Ｗ            DELIMITED BY SPACE
                     "recedata_"           DELIMITED BY SIZE
                     システム日付Ｗ        DELIMITED BY SIZE
                     ".csv"                DELIMITED BY SIZE
                INTO FD-NAME
           ELSE
007096        STRING 連入－ドライブ        DELIMITED BY SIZE
007097               ":\"                  DELIMITED BY SIZE
                     会員番号Ｗ            DELIMITED BY SPACE
                     "recedata_"           DELIMITED BY SIZE
                     システム日付Ｗ        DELIMITED BY SIZE
                     ".csv"                DELIMITED BY SIZE
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
003820         MOVE 99 TO PROGRAM-STATUS
003830         EXIT PROGRAM
007108     ELSE
007109         PERFORM オープンチェック
007110         CLOSE 会提出ファイル
007111         CALL "delfile"  WITH C LINKAGE
007112                         USING BY REFERENCE FD-NAME
007113     END-IF.
027970*================================================================*
027980 クライアントファイル作成 SECTION.
027990*
028000     OPEN INPUT 作業ファイル２.
028010         MOVE NC"作２" TO ファイル名.
028020         PERFORM オープンチェック.
004260     OPEN OUTPUT 会提出ファイル.
004270         MOVE NC"会提出ファイル" TO ファイル名.
004280         PERFORM オープンチェック.
028030*
002970     MOVE ZERO  TO 作２－分類コード.
002960     MOVE ZERO  TO 作２－県コード.
002970     MOVE ZERO  TO 作２－保険者番号.
002980     MOVE ZERO  TO 作２－順番.
003020*
003030     START 作業ファイル２ KEY IS > 作２－順番
003110     END-START.
003120     IF 状態キー  =  "00"
028190         MOVE SPACE  TO 終了フラグ
028200         PERFORM 作業ファイル２読込
002460         IF  終了フラグ = "YES"
002470             MOVE  NC"　データが０件です。確認して下さい。" TO 連メ－メッセージ
002480             CALL   "MSG001"
002490             CANCEL "MSG001"
002500             PERFORM ファイル閉鎖
002510             MOVE 99 TO PROGRAM-STATUS
002520             EXIT PROGRAM
               ELSE
028052             MOVE 780  TO 文字カウンタ
005090             PERFORM タイトルセット
028400             PERFORM 会提出ファイル書込
002530         END-IF
028214         PERFORM UNTIL ( 終了フラグ = "YES" )
028220*
028230            MOVE 作２－施術和暦年月 TO 受－施術和暦年月
028240            MOVE 作２－患者コード   TO 受－患者コード
028250            READ 受診者情報Ｆ
028260            INVALID KEY
028270                CONTINUE
028280            NOT INVALID KEY
028297                MOVE "YES"      TO 実行キーＷ
028299*                
028305                IF 実行キーＷ = "YES"
028052                    MOVE 780   TO 文字カウンタ
011430                    MOVE SPACE TO 会提－レコード
028320                    PERFORM レセプトＦセット
028340*
028350                    PERFORM レコードセット
                          MOVE レコードデータＷ TO 会提－レコード
028052*                    MOVE 桁位置Ｗ         TO 文字カウンタ
                          COMPUTE 文字カウンタ = 桁位置Ｗ - 1
028470                    PERFORM 会提出ファイル書込
028510*
028520                END-IF
028530                PERFORM 作業ファイル２読込
028540             END-READ
028550         END-PERFORM
028570     ELSE
002470         MOVE  NC"　データが０件です。確認して下さい。" TO 連メ－メッセージ
002480         CALL   "MSG001"
002490         CANCEL "MSG001"
002500         PERFORM ファイル閉鎖
002510         MOVE 99 TO PROGRAM-STATUS
002520         EXIT PROGRAM
028571     END-IF.
028573*
028580     CLOSE 作業ファイル２.
004093*
005820*================================================================*
005830 タイトルセット SECTION.
005840*
004340     MOVE SPACE  TO 会提－レコード.
005842*
005850     STRING 区切文字Ｒ                   DELIMITED BY SIZE
                  "会員ＩＤ"                   DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "通番"                       DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "主保険フラグ"               DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "移行データフラグ"           DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "予備フラグ１"               DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "施術年月"                   DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "返戻再請求、月送り"         DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "保険者番号"                 DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "保険証記号"                 DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "保険証番号"                 DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "老人保健市町村番号"         DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "老人保健受給者番号"         DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "医療助成公費負担者番号"     DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "医療助成受給者番号"         DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "本人・家族区分"             DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "高齢、幼年助成区分"         DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "被保険者名（カナ）"         DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "被保険者名（漢字）"         DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "被保険者名外字使用フラグ"   DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "被保険者住所"               DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "受療者名（カナ）"           DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "受療者名（漢字）"           DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "受療者名外字使用フラグ"     DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "費用額"                     DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "一部負担金"                 DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "負担割合"                   DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "請求金額"                   DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "受給者負担額"               DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "助成請求金額"               DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "通院日数"                   DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "受療者名　生年月日"         DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "受療者名　性別"             DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "部位数"                     DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "負傷の原因有無フラグ"       DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "長期理由有無フラグ"         DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "経過"                       DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "基準施術年月"               DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "保険種別グループ"           DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "取りまとめ用ソート番号"     DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "取りまとめ用の文字列"       DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "データ表示順"               DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "併用表記"                   DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "表示フラグ"                 DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "表示用公費負担者番号"       DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "表示用公費負担者名称"       DELIMITED BY SIZE
                  区切文字Ｒ２                 DELIMITED BY SIZE
                  "受診者No"                   DELIMITED BY SIZE
                  区切文字Ｒ                   DELIMITED BY SIZE
             INTO 会提－レコード
           END-STRING.
006150*
009870*================================================================*
009880 西暦年取得 SECTION.
009890*
009900     MOVE ZERO   TO 西暦年Ｗ.
009910     MOVE 和暦Ｗ TO 元－元号区分.
009920     READ 元号マスタ
009930     NOT INVALID KEY
009940         COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + 年Ｗ - 1
009950     END-READ.
010650*================================================================*
010740 レセプトＦ読込 SECTION.
010750*
010760     READ レセプトＦ NEXT
010770     AT END
010780         MOVE "YES" TO 終了フラグ
010790     END-READ.
011406*================================================================*
011410 レコードセット SECTION.
011420*
           MOVE SPACE               TO レコードデータＷ.
           MOVE 1                   TO 桁位置Ｗ.
011450*/接骨院ＩＤ
005420     MOVE 会員番号Ｗ          TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ    DELIMITED BY SPACE
                  ","               DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
011450*/通番
           COMPUTE 通番ＷＰ = 通番ＷＰ + 1.
005420     MOVE 通番Ｗ          TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
011450*/保険フラグ
009622     EVALUATE レセ－レセ種別
           WHEN 3
               STRING 区切文字Ｒ    DELIMITED BY SIZE
                      "2"           DELIMITED BY SIZE
                      区切文字Ｒ    DELIMITED BY SIZE
                 INTO 英数字項目２Ｗ
               END-STRING
           WHEN OTHER
               STRING 区切文字Ｒ    DELIMITED BY SIZE
                      "0"           DELIMITED BY SIZE
                      区切文字Ｒ    DELIMITED BY SIZE
                 INTO 英数字項目２Ｗ
               END-STRING
           END-EVALUATE.
005440     STRING 英数字項目２Ｗ(1:3)          DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
011450*/移行フラグ、予備フラグ
           STRING 区切文字Ｒ    DELIMITED BY SIZE
                  "0"           DELIMITED BY SIZE
                  区切文字Ｒ    DELIMITED BY SIZE
             INTO 英数字項目２Ｗ
           END-STRING.
005440     STRING 英数字項目２Ｗ(1:3)          DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
           STRING 区切文字Ｒ    DELIMITED BY SIZE
                  "0"           DELIMITED BY SIZE
                  区切文字Ｒ    DELIMITED BY SIZE
             INTO 英数字項目２Ｗ
           END-STRING.
005440     STRING 英数字項目２Ｗ(1:3)          DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
011450*/施術年月/西暦
           MOVE 受－施術和暦年月    TO 和暦年月Ｗ.
           PERFORM 西暦年取得.
           MOVE 月Ｗ   TO 施術月Ｗ.
005420     MOVE 施術西暦年月Ｗ      TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
011450*/返戻、月遅れ
           EVALUATE レセ－請求区分
           WHEN 1
               STRING 区切文字Ｒ    DELIMITED BY SIZE
                      "2"           DELIMITED BY SIZE
                      区切文字Ｒ    DELIMITED BY SIZE
                 INTO 英数字項目２Ｗ
               END-STRING
           WHEN 2
               STRING 区切文字Ｒ    DELIMITED BY SIZE
                      "1"           DELIMITED BY SIZE
                      区切文字Ｒ    DELIMITED BY SIZE
                 INTO 英数字項目２Ｗ
               END-STRING
           WHEN OTHER
               STRING 区切文字Ｒ    DELIMITED BY SIZE
                      "0"           DELIMITED BY SIZE
                      区切文字Ｒ    DELIMITED BY SIZE
                 INTO 英数字項目２Ｗ
               END-STRING
           END-EVALUATE.
005440     STRING 英数字項目２Ｗ(1:3)          DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
011450*/保険者番号
011690*/後高
011700     IF 受－公費種別 NOT = ZERO
005420         MOVE 受－費用負担者番号  TO 英数字項目Ｗ
           ELSE
005420         MOVE 受－保険者番号      TO 英数字項目Ｗ
           END-IF.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
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
005420         MOVE 記号Ｗ               TO 英数字項目Ｗ
005430         PERFORM 変換処理英数字
005440         STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
           ELSE
               MOVE 区切文字ＲＷ         TO 英数字項目２Ｗ
005440         STRING 英数字項目２Ｗ(1:2)          DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
011900     END-IF.
011910     IF (番号Ｗ(1:1) NOT = "*") AND (番号Ｗ(1:2) NOT = "＊")
005420         MOVE 番号Ｗ               TO 英数字項目Ｗ
005430         PERFORM 変換処理英数字
005440         STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                      ","                              DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
           ELSE
               MOVE 区切文字ＲＷ         TO 英数字項目２Ｗ
005440         STRING 英数字項目２Ｗ(1:2)          DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
011930     END-IF.
011750*/老人
           MOVE 区切文字ＲＷ             TO 英数字項目２Ｗ.
005440     STRING 英数字項目２Ｗ(1:2)          DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
005440     STRING 英数字項目２Ｗ(1:2)          DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
011750*/助成
011760     IF レセ－レセ種別 = 3
005420         MOVE 受－費用負担者番号助成 TO 英数字項目Ｗ
005430         PERFORM 変換処理英数字
005440         STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                      ","                              DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
005420         MOVE 受－受益者番号助成     TO 英数字項目Ｗ
005430         PERFORM 変換処理英数字
005440         STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                      ","                              DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
           ELSE
               MOVE 区切文字ＲＷ             TO 英数字項目２Ｗ
005440         STRING 英数字項目２Ｗ(1:2)          DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
005440         STRING 英数字項目２Ｗ(1:2)          DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
011860     END-IF.
011520*/続柄
011580     IF 受－本人家族区分 = 1
               STRING 区切文字Ｒ    DELIMITED BY SIZE
                      "0"           DELIMITED BY SIZE
                      区切文字Ｒ    DELIMITED BY SIZE
                 INTO 英数字項目２Ｗ
               END-STRING
011582     ELSE
               STRING 区切文字Ｒ    DELIMITED BY SIZE
                      "1"           DELIMITED BY SIZE
                      区切文字Ｒ    DELIMITED BY SIZE
                 INTO 英数字項目２Ｗ
               END-STRING
011584     END-IF.
005440     STRING 英数字項目２Ｗ(1:3)          DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
011520*/特別区分
011580     IF 受－保険種別 NOT = 5
               EVALUATE 受－特別区分
               WHEN 1
               WHEN 2
               WHEN 3
                   STRING 区切文字Ｒ    DELIMITED BY SIZE
                          "2"           DELIMITED BY SIZE
                          区切文字Ｒ    DELIMITED BY SIZE
                     INTO 英数字項目２Ｗ
                   END-STRING
               WHEN 6
                   STRING 区切文字Ｒ    DELIMITED BY SIZE
                          "1"           DELIMITED BY SIZE
                          区切文字Ｒ    DELIMITED BY SIZE
                     INTO 英数字項目２Ｗ
                   END-STRING
               WHEN OTHER
                   STRING 区切文字Ｒ    DELIMITED BY SIZE
                          "0"           DELIMITED BY SIZE
                          区切文字Ｒ    DELIMITED BY SIZE
                     INTO 英数字項目２Ｗ
                   END-STRING
               END-EVALUATE
           ELSE
               STRING 区切文字Ｒ    DELIMITED BY SIZE
                      "0"           DELIMITED BY SIZE
                      区切文字Ｒ    DELIMITED BY SIZE
                 INTO 英数字項目２Ｗ
               END-STRING
           END-IF.
005440     STRING 英数字項目２Ｗ(1:3)          DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
011585*/名前
005420     MOVE 受－被保険者カナ           TO 氏名Ｗ.
           MOVE 氏名Ｗ                     TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
005420     MOVE 受－被保険者氏名           TO 氏名Ｗ.
           MOVE 氏名Ｗ                     TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
011600     MOVE 受－被保険者氏名           TO 氏名Ｗ.
           PERFORM 外字判定.
005420     MOVE 外字フラグ                 TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
      *
           MOVE SPACE   TO 住所Ｗ.
005420     STRING 受－住所１                       DELIMITED BY SPACE
                  受－住所２                       DELIMITED BY SPACE
             INTO 住所Ｗ
           END-STRING.
           INSPECT 住所Ｗ REPLACING ALL "," BY ".".
           INSPECT 住所Ｗ REPLACING ALL "，" BY "、".
           MOVE 住所Ｗ                     TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
005420     MOVE 受－患者カナ               TO 氏名Ｗ.
           MOVE 氏名Ｗ                     TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
005420     MOVE 受－患者氏名               TO 氏名Ｗ.
           MOVE 氏名Ｗ                     TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
011600     MOVE 受－患者氏名               TO 氏名Ｗ.
           PERFORM 外字判定.
005420     MOVE 外字フラグ                 TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
010842*
           MOVE レセ－合計                 TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
           MOVE レセ－一部負担金     TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
           COMPUTE 負担割合Ｗ = レセ－給付割合 * 10.
005420     MOVE 負担割合Ｗ                 TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
           MOVE レセ－請求金額     TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
011760     IF レセ－レセ種別 = 3
               MOVE レセ－受給者負担額     TO 英数字項目Ｗ
005430         PERFORM 変換処理英数字
005440         STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                      ","                              DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
               MOVE レセ－助成請求金額     TO 英数字項目Ｗ
005430         PERFORM 変換処理英数字
005440         STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                      ","                              DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
           ELSE
               STRING 区切文字Ｒ    DELIMITED BY SIZE
                      "0"           DELIMITED BY SIZE
                      区切文字Ｒ    DELIMITED BY SIZE
                 INTO 英数字項目２Ｗ
               END-STRING
005440         STRING 英数字項目２Ｗ(1:3)          DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
005440         STRING 英数字項目２Ｗ(1:3)          DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
           END-IF.
012150*/通院数
005420     MOVE レセ－レセ実日数           TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
012150*/生年月日/西暦
012180     MOVE 受－患者生年月日           TO 和暦年月日Ｗ.
           PERFORM 西暦年取得.
           MOVE 月Ｗ                       TO 施術月Ｗ.
           MOVE 日Ｗ                       TO 施術日Ｗ.
005420     MOVE 施術西暦年月日Ｗ           TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
012150*/性別
012170     IF 受－患者性別 = 1
               STRING 区切文字Ｒ    DELIMITED BY SIZE
                      "0"           DELIMITED BY SIZE
                      区切文字Ｒ    DELIMITED BY SIZE
                 INTO 英数字項目２Ｗ
               END-STRING
           ELSE
               STRING 区切文字Ｒ    DELIMITED BY SIZE
                      "1"           DELIMITED BY SIZE
                      区切文字Ｒ    DELIMITED BY SIZE
                 INTO 英数字項目２Ｗ
               END-STRING
           END-IF.
005440     STRING 英数字項目２Ｗ(1:3)          DELIMITED BY SIZE
                  ","                          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
      */負傷セット
028310     PERFORM 負傷データＦセット
011450*/請求年月/西暦
           MOVE レセ－請求和暦年月         TO 和暦年月Ｗ.
           PERFORM 西暦年取得.
           MOVE 月Ｗ   TO 施術月Ｗ.
005420     MOVE 施術西暦年月Ｗ             TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
012150*/保険種別グループ
005420     MOVE 作２－保険種別順番         TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
012150*/取りまとめ用ソート番号
005420     MOVE 作２－ソート番号           TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
012150*/取りまとめ用の文字列
003782     MOVE 作２－分類保険種別   TO 保険種別Ｗ.
003782     MOVE 作２－分類保険者番号 TO 保険者番号Ｗ.
           IF (作２－県コード = SPACE) OR (作２－分類コード = 10 OR 20)
003790         PERFORM 請求先情報取得
           ELSE
              EVALUATE 作２－県コード
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
              MOVE 作２－県コード    TO 名－名称コード
              READ 名称マスタ
              NOT INVALID KEY
                 MOVE 名－略称       TO 県名Ｗ
              END-READ
              STRING 県名ＷＰ                 DELIMITED BY "　"
                     府県Ｗ                   DELIMITED BY SPACE
                     "国民健康保険団体連合会" DELIMITED BY SIZE
                INTO 請求先名称Ｗ
              END-STRING
           END-IF.
005420     MOVE 請求先名称Ｗ               TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
012150*/データ表示順
005420     MOVE 作２－データ表示順         TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
012150*/併用表記
005420     MOVE 作２－併区分               TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
012150*/表示フラグ
005420     MOVE 作２－表示フラグ           TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
011750*/助成
011760     IF レセ－レセ種別 = 3
005420         MOVE 受－費用負担者番号助成 TO 英数字項目Ｗ
005430         PERFORM 変換処理英数字
005440         STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                      ","                              DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
003782         MOVE 作２－保険種別   TO 保険種別Ｗ
003782         MOVE 作２－保険者番号 TO 保険者番号Ｗ
003790         PERFORM 請求先情報取得
005420         MOVE 請求先名称Ｗ         TO 英数字項目Ｗ
005430         PERFORM 変換処理英数字
005440         STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                      ","                              DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
           ELSE
               MOVE 区切文字ＲＷ             TO 英数字項目２Ｗ
005440         STRING 英数字項目２Ｗ(1:2)          DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
005440         STRING 英数字項目２Ｗ(1:2)          DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
011860     END-IF.
011450*/移行フラグ、予備フラグ
           STRING 区切文字Ｒ         DELIMITED BY SIZE
                  作２－患者コード   DELIMITED BY SIZE
                  区切文字Ｒ         DELIMITED BY SIZE
             INTO 英数字項目２Ｗ
           END-STRING.
005440     STRING 英数字項目２Ｗ(1:9)          DELIMITED BY SIZE
             INTO レコードデータＷ
             WITH POINTER 桁位置Ｗ
           END-STRING.
      *
003900*================================================================*
003901 請求先情報取得 SECTION.
003910*
003920     IF ( 保険種別Ｗ NOT = 05 ) AND ( 保険種別Ｗ < 10 )
003930*    / 健保 /
003960        MOVE 保険種別Ｗ          TO 保－保険種別
003970        MOVE 保険者番号Ｗ        TO 保－保険者番号
003980        MOVE SPACE               TO 請求先名称Ｗ
003990        READ 保険者マスタ
004000        INVALID KEY
004010            MOVE SPACE           TO 請求先名称Ｗ
004020        NOT INVALID KEY
004030            MOVE 保－保険者名称  TO 請求先名称Ｗ
004040        END-READ
004041     ELSE
004042*    / 老人・助成 /
004045        MOVE 保険種別Ｗ          TO 市－公費種別
004046        MOVE 保険者番号Ｗ        TO 市－市町村番号
004048        READ 市町村マスタ
004049        INVALID KEY
004050            MOVE SPACE           TO 請求先名称Ｗ
004051        NOT INVALID KEY
                  IF 保険種別Ｗ = 05
004062                MOVE 市－支部名  TO 請求先名称Ｗ
                  ELSE
004062                MOVE 市－市町村名称  TO 請求先名称Ｗ
                  END-IF
004064        END-READ
004065     END-IF.
004066*
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
005420         MOVE 負－部位数            TO 英数字項目Ｗ
005430         PERFORM 変換処理英数字
005440         STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                      ","                              DELIMITED BY SIZE
                 INTO レコードデータＷ
                 WITH POINTER 桁位置Ｗ
               END-STRING
               PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
                         UNTIL ( 部位ＣＮＴ > 負－部位数 )
                  IF (負傷原因印刷区分Ｗ  NOT = 1) AND
                     (負－負傷原因コード(部位ＣＮＴ) NOT = ZERO)
                      MOVE 1              TO 負傷原因フラグ
                  END-IF
                  IF 負－経過コード(部位ＣＮＴ) NOT = ZERO
                      MOVE 1              TO 経過フラグ
                  END-IF
               END-PERFORM
016140     END-READ.
018470     IF 長期理由印刷区分Ｗ  NOT = 1 
028310         PERFORM 長期継続者Ｆセット
           END-IF.
005420     MOVE 負傷原因フラグ     TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
              INTO レコードデータＷ
              WITH POINTER 桁位置Ｗ
           END-STRING.
005420     MOVE 長期理由フラグ     TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
              INTO レコードデータＷ
              WITH POINTER 桁位置Ｗ
           END-STRING.
005420     MOVE 経過フラグ     TO 英数字項目Ｗ.
005430     PERFORM 変換処理英数字.
005440     STRING 英数字項目２Ｗ(1:文字ＣＮＴ + 1) DELIMITED BY SIZE
                  ","                              DELIMITED BY SIZE
              INTO レコードデータＷ
              WITH POINTER 桁位置Ｗ
           END-STRING.
      *
015850*================================================================*
015860 長期継続者Ｆセット SECTION.
      *
004502     MOVE 受－施術和暦   TO 長継－施術和暦
004503     MOVE 受－施術年     TO 長継－施術年.
004504     MOVE 受－施術月     TO 長継－施術月.
004505     MOVE 受－患者番号   TO 長継－患者番号.
004506     MOVE 受－枝番       TO 長継－枝番.
004507*
004508     READ 長期継続者Ｆ
004522     NOT INVALID KEY
004523         IF  (長継－理由文(1)  = SPACE )  AND
004524             (長継－理由文(2)  = SPACE )  AND
004525             (長継－理由文(3)  = SPACE )  AND
004526             (長継－理由文(4)  = SPACE )  AND
004527             (長継－理由文(5)  = SPACE )
                   MOVE 0              TO 経過フラグ
004535         ELSE
                   MOVE 1              TO 経過フラグ
004537         END-IF
004538     END-READ.
004539*
015350*================================================================*
015360 レセプトＦセット SECTION.
      *
005520     MOVE 作２－施術和暦年月 TO レセ－施術和暦年月.
005530     MOVE 作２－患者コード   TO レセ－患者コード.
005540     EVALUATE 作２－保険種別
           WHEN 5
               MOVE 2      TO レセ－レセ種別
           WHEN 51
           WHEN 52
           WHEN 53
           WHEN 54
           WHEN 55
           WHEN 60
               MOVE 3      TO レセ－レセ種別
           WHEN OTHER
               MOVE 1      TO レセ－レセ種別
           END-EVALUATE.
           READ レセプトＦ
           INVALID KEY
               MOVE SPACE  TO レセ－レコード
           END-READ.
022510*================================================================*
022520 制御情報取得 SECTION.
022530*
022540     MOVE ZERO TO 制－制御区分
022550     READ 制御情報マスタ
022560     NOT INVALID KEY
008810         MOVE 制－レセ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
008820         MOVE 制－レセ長期理由印刷区分 TO 長期理由印刷区分Ｗ
022580     END-READ.
022841*
022870*================================================================*
023649 外字判定 SECTION.
023650*
            MOVE ZERO TO 外字フラグ.
      */文字数カウント
            PERFORM VARYING カウンタ２ FROM 50 BY -1
              UNTIL (カウンタ２ <= ZERO) OR (氏名Ｗ(カウンタ２:1) NOT = SPACE)
                CONTINUE
            END-PERFORM.
            MOVE 1 TO カウンタ.
            PERFORM UNTIL ( カウンタ > カウンタ２ ) OR ( 外字フラグ = 1 )
               IF (( 氏名Ｗ(カウンタ:1) >= X"20" ) AND ( 氏名Ｗ(カウンタ:1) <= X"80" )) OR
                  (( 氏名Ｗ(カウンタ:1) >= X"A0" ) AND ( 氏名Ｗ(カウンタ:1) <= X"DF" ))
                  COMPUTE カウンタ = カウンタ + 1
               ELSE
                  IF (( 氏名Ｗ(カウンタ:2) >= X"F040" ) AND ( 氏名Ｗ(カウンタ:2) <= X"F9FC" ))
                      MOVE 1 TO 外字フラグ
                  END-IF
                  COMPUTE カウンタ = カウンタ + 2
                END-IF
            END-PERFORM.
023960*
008830*================================================================*
008840 変換処理英数字 SECTION.
008850*
008860*/英数字項目を"でくくる処理。
008870     MOVE SPACE TO 終了フラグ４.
008880     MOVE 区切文字Ｒ   TO 英数字項目２Ｗ.
008890     MOVE 英数字項目Ｗ TO 英数字項目２Ｗ(2:69)
008900     PERFORM VARYING 文字ＣＮＴ FROM 70 BY -1
008910             UNTIL (文字ＣＮＴ  <= ZERO) OR
008920                   (終了フラグ４ = "YES")
008930         IF 英数字項目２Ｗ(文字ＣＮＴ:1) NOT = SPACE
008940            COMPUTE 文字ＣＮＴ = 文字ＣＮＴ + 1
008950            MOVE 区切文字Ｒ TO 英数字項目２Ｗ(文字ＣＮＴ:1)
008960            MOVE "YES" TO 終了フラグ４
008970         END-IF
008980     END-PERFORM.
028590*================================================================*
028600 作業ファイル２読込 SECTION.
028610*
028620     READ 作業ファイル２ NEXT
028630     AT END
028640         MOVE "YES" TO 終了フラグ
028650     END-READ.
028695*
028696*================================================================*
028697 会提出ファイル書込 SECTION.
028698*
028699     WRITE 会提－レコード
028700     END-WRITE.
028702     IF 状態キー  NOT =  "00"
028703         MOVE NC"会提" TO ファイル名
028704         PERFORM エラー表示
028705     END-IF.
028706*================================================================*
       圧縮ファイル作成 SECTION.
      *
           IF 連入－固定フラグ = 1
002890        STRING "C:\makishisys\mcb_rese\01_jyu\"      DELIMITED BY SIZE
                     会員番号Ｗ            DELIMITED BY SPACE
                     "recedata_"           DELIMITED BY SIZE
                     システム日付Ｗ        DELIMITED BY SIZE
                     ".ZIP"                DELIMITED BY SIZE
                INTO 圧縮ファイル名Ｗ
           ELSE
007096        STRING 連入－ドライブ        DELIMITED BY SIZE
007097               ":\"                  DELIMITED BY SIZE
                     会員番号Ｗ            DELIMITED BY SPACE
                     "recedata_"           DELIMITED BY SIZE
                     システム日付Ｗ        DELIMITED BY SIZE
                     ".ZIP"                DELIMITED BY SIZE
007098              INTO 圧縮ファイル名Ｗ
007099        END-STRING
           END-IF.
           IF 連入－固定フラグ = 1
002890        STRING "C:\makishisys\JTAB\" DELIMITED BY SIZE
                     会員番号Ｗ            DELIMITED BY SPACE
                     "recedata_"           DELIMITED BY SIZE
                     システム日付Ｗ        DELIMITED BY SIZE
                     ".csv"                DELIMITED BY SIZE
                INTO 元ファイル名Ｗ
           ELSE
007096        STRING 連入－ドライブ        DELIMITED BY SIZE
007097               ":\"                  DELIMITED BY SIZE
                     会員番号Ｗ            DELIMITED BY SPACE
                     "recedata_"           DELIMITED BY SIZE
                     システム日付Ｗ        DELIMITED BY SIZE
                     ".csv"                DELIMITED BY SIZE
007098              INTO 元ファイル名Ｗ
007099        END-STRING
           END-IF.
      *
           MOVE SPACE TO パラ１Ｗ.
      *      
      ** 圧縮コマンド:"a -tzip"
           STRING "a -tzip"  DELIMITED BY SIZE
      *　　　  /圧縮ファイル名
                  " "              DELIMITED BY SIZE
                  圧縮ファイル名Ｗ DELIMITED BY SPACE
      */
      *　　　  /元のファイル名
                  " "              DELIMITED BY SIZE
                  元ファイル名Ｗ   DELIMITED BY SPACE
      *
      *        /処理ダイアログ出さない時指定
                         " -hide"  DELIMITED BY SIZE
      *
      *        /パスワードは、 -pに続けで指定
                        " -pjksn"  DELIMITED BY SIZE
                INTO パラ１Ｗ
            END-STRING.
            MOVE "zip" TO プログラム名Ｗ.
            CALL プログラム名Ｗ WITH C LINKAGE
                      USING BY REFERENCE パラ１Ｗ.
      *
            IF PROGRAM-STATUS NOT = ZERO
002470         MOVE  NC"　　　　　　圧縮失敗！！" TO 連メ－メッセージ
002480         CALL   "MSG001"
002490         CANCEL "MSG001"
            END-IF.     
      *
            MOVE "delfile" TO プログラム名Ｗ.
            CALL プログラム名Ｗ WITH C LINKAGE
                    USING BY REFERENCE 元ファイル名Ｗ.
028706*================================================================*
028707******************************************************************
028708 END PROGRAM YAS1022.
028709******************************************************************
