000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAS1011.
000060 AUTHOR.                 山田 浩之
000070*
000080*----------------------------------------------------------------*
      *    【会提出用フロッピー作成(SSB)】 あさひ接骨師会用
000100*    【書込み】
000100* 請求年月Ver.
000110*      MED = YNK580 
000120*----------------------------------------------------------------*
       DATE-WRITTEN.           2012-12-27
       DATE-COMPILED.          2012-12-27
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
000590     SELECT  レセプトＦ      ASSIGN      TO        RECEPTL
000600                             ORGANIZATION             IS  INDEXED
000610                             ACCESS MODE              IS  DYNAMIC
000620                             RECORD KEY               IS  レセ－施術和暦年月
000630                                                          レセ－患者コード
000640                                                          レセ－レセ種別
000650                             ALTERNATE RECORD KEY     IS  レセ－患者コード
000660                                                          レセ－施術和暦年月
000670                                                          レセ－レセ種別
000680                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
000690                                                          レセ－施術和暦年月
000700                                                          レセ－患者コード
000710                                                          レセ－レセ種別
000720                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
000730                                                          レセ－レセ種別
000740                                                          レセ－請求保険者番号
000750                                                          レセ－患者コード
000760                                                          レセ－施術和暦年月
000770                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
000780                                                          レセ－請求保険者番号
000790                                                          レセ－患者コード
000800                                                          レセ－レセ種別
000810                                                          レセ－施術和暦年月
000820                             FILE STATUS              IS  状態キー
000830                             LOCK        MODE         IS  AUTOMATIC.
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
000430     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000440                             ORGANIZATION             IS  INDEXED
000450                             ACCESS MODE              IS  DYNAMIC
000460                             RECORD KEY               IS  名－区分コード
000470                                                          名－名称コード
000480                             FILE STATUS              IS  状態キー
000490                             LOCK        MODE         IS  AUTOMATIC.
001090     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
001100                             ORGANIZATION             IS  INDEXED
001110                             ACCESS MODE              IS  DYNAMIC
001120                             RECORD KEY               IS  負原－区分コード
001130                                                          負原－負傷原因コード
001140                             FILE STATUS              IS  状態キー
001150                             LOCK        MODE         IS  AUTOMATIC.
000293     SELECT  長期継続者Ｆ    ASSIGN      TO        CHOKEIL
000294                             ORGANIZATION             IS INDEXED
000295                             ACCESS MODE              IS DYNAMIC
000296                             RECORD KEY               IS 長継－施術和暦年月
000297                                                         長継－患者コード
000298                             ALTERNATE RECORD KEY     IS 長継－患者コード
000299                                                         長継－施術和暦年月
000300                             FILE STATUS              IS 状態キー
000301                             LOCK      MODE           IS AUTOMATIC.
000241     SELECT  生保情報Ｆ      ASSIGN      TO        SEIHOJL
000242                             ORGANIZATION           IS INDEXED
000243                             ACCESS MODE            IS DYNAMIC
000244                             RECORD KEY             IS 生保－施術和暦年月
000245                                                       生保－患者コード
000255                             ALTERNATE RECORD KEY   IS 生保－患者コード
000265                                                       生保－施術和暦年月
000277                             FILE STATUS            IS 状態キー
000278                             LOCK        MODE       IS AUTOMATIC.
000241     SELECT  労災情報Ｆ      ASSIGN      TO        ROUSAIJL
000242                             ORGANIZATION             IS INDEXED
000243                             ACCESS MODE              IS DYNAMIC
000244                             RECORD KEY               IS 労災－施術和暦年月
000245                                                         労災－患者コード
000255                             ALTERNATE RECORD KEY     IS 労災－患者コード
000265                                                         労災－施術和暦年月
000277                             FILE STATUS              IS 状態キー
000278                             LOCK        MODE         IS AUTOMATIC.
001290*
001520     SELECT 作業ファイル１ ASSIGN    TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1011L.DAT"
001530                             ORGANIZATION             IS  INDEXED
001540                             ACCESS MODE              IS  DYNAMIC
000861                             RECORD      KEY          IS  作１－請求和暦年月
000890                                                          作１－保険者番号
000900                                                          作１－施術和暦年月
000910                                                          作１－被保険者カナ
000920                                                          作１－患者コード
000921                                                          作１－助成区分
000680                              ALTERNATE RECORD KEY    IS  作１－集計順
000900                                                          作１－本人家族区分
000910                                                          作１－患者コード
000900                                                          作１－施術和暦年月
000921                                                          作１－助成区分
000952                              FILE STATUS             IS  状態キー
001600                             LOCK        MODE         IS  AUTOMATIC.
001610******************************************************************
001620*                      DATA DIVISION                             *
001630******************************************************************
001640 DATA                    DIVISION.
001650 FILE                    SECTION.
001660*                           ［ＲＬ＝  ３２０］
001670 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
001680     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
001490*                          ［ＲＬ＝  １５３６］
001500 FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
001510     COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
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
001980*                           ［ＲＬ＝  １２８］
001990 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
002000     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
001980*                           ［ＲＬ＝  １２８］
001990 FD  負傷原因Ｆ         BLOCK   CONTAINS   1   RECORDS.
002000     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
000686*                           ［ＲＬ＝  １２８］
000687 FD  長期継続者Ｆ          BLOCK   CONTAINS   1   RECORDS.
000688     COPY CHOKEI          OF  XFDLIB  JOINING   長継   AS  PREFIX.
001080* 
000280 FD  生保情報Ｆ          BLOCK   CONTAINS   1   RECORDS.
000281     COPY SEIHOJ          OF  XFDLIB  JOINING   生保   AS  PREFIX.
001510*
000280 FD  労災情報Ｆ          BLOCK   CONTAINS   1   RECORDS.
000281     COPY ROUSAIJ         OF  XFDLIB  JOINING   労災   AS  PREFIX.
004330*                           ［ＲＬ＝  1742］
004340 FD  作業ファイル１ RECORD  CONTAINS 1742  CHARACTERS.
004350 01 作１－レコード.
004360    03 作１－レコードキー.
002150       05 作１－請求和暦年月.
003480          07 作１－請求和暦                  PIC 9(1).
002330          07 作１－請求年                    PIC 9(2).
002350          07 作１－請求月                    PIC 9(2).
004380       05 作１－保険者番号                   PIC X(8).
004420       05 作１－施術和暦年月.
004430          07 作１－施術和暦                  PIC 9(1).
004450          07 作１－施術年                    PIC 9(2).
004460          07 作１－施術月                    PIC 9(2).
003570       05 作１－被保険者カナ                 PIC X(25).
004390       05 作１－患者コード.
004400          07 作１－患者番号                  PIC 9(6).
004410          07 作１－枝番                      PIC X.
003610       05 作１－助成区分                     PIC 9(1).
003620    03 作１－レコードデータ.
             05 作１－集計順                       PIC 9(1).
002320       05 作１－請求年月                     PIC 9(6).
002310       05 作１－会員番号                     PIC 9(7).
002470       05 作１－記号.
                07 作１－保険証記号                PIC X(24).
002480       05 作１－番号                         PIC X(16).
002510       05 作１－助成負担者番号               PIC X(8).
002520       05 作１－助成受給者番号               PIC X(16).
002210       05 作１－本人家族区分                 PIC 9(1).
002530       05 作１－被保険者氏名.
                07 作１－被保険者名                PIC X(30).
002560       05 作１－患者カナ                     PIC X(25).
002550       05 作１－患者氏名.
                07 作１－患者名                    PIC X(30).
002680       05 作１－合計金額                     PIC 9(7).
002700       05 作１－一部負担金                   PIC 9(6).
002710       05 作１－請求金額                     PIC 9(6).
002690       05 作１－給付割合                     PIC 9(2).
003020       05 作１－実日数                       PIC 9(3).
002390       05 作１－施術年月                     PIC 9(6).
002210       05 作１－再請求区分                   PIC 9(1).
002610       05 作１－患者生年月日                 PIC 9(8).
002600       05 作１－性別                         PIC 9(1).
002720       05 作１－部位数                       PIC 9(1).
002210       05 作１－レセコン区分                 PIC 9(2).
002210       05 作１－高齢者区分                   PIC 9(1).
      */運動後療追加↓↓↓/20180607
             05 作１－運動後療料回数               PIC 9(1).
             05 作１－運動後療料                   PIC 9(5).
      */運動後療追加↑↑↑/20180607
002730       05 作１－住所                         PIC X(60).
002730       05 作１－負傷原因                     PIC X(200).
002730       05 作１－経過                         PIC X(50).
002740       05 作１－長期理由                     PIC X(400).
002210       05 作１－新規区分                     PIC 9(1).
002210       05 作１－継続区分                     PIC 9(1).
002210       05 作１－初検回数                     PIC 9(1).
002210       05 作１－初検料                       PIC 9(5).
002210       05 作１－初検時間外回数               PIC 9(1).
002210       05 作１－初検休日回数                 PIC 9(1).
002210       05 作１－初検深夜回数                 PIC 9(1).
002210       05 作１－初検加算                     PIC 9(5).
002210       05 作１－再検回数                     PIC 9(1).
002210       05 作１－再検料                       PIC 9(5).
002210       05 作１－往療距離                     PIC 9(3).
002210       05 作１－往療回数                     PIC 9(2).
002210       05 作１－往療料                       PIC 9(5).
002210       05 作１－夜間回数                     PIC 9(1).
002210       05 作１－難路回数                     PIC 9(1).
002210       05 作１－暴風雨回数                   PIC 9(1).
002210       05 作１－往療加算                     PIC 9(5).
      */金属副子変更↓↓↓/20180611
000561       05 作１－金属副子回数            PIC 9.
000561       05 作１－ダミー                  PIC X(2).
002210*       05 作１－金属大回数                   PIC 9(1).
002210*       05 作１－金属中回数                   PIC 9(1).
002210*       05 作１－金属小回数                   PIC 9(1).
      */金属副子変更↑↑↑/20180611
002210       05 作１－金属副子加算                 PIC 9(5).
002210       05 作１－情報提供回数                 PIC 9(1).
002210       05 作１－情報提供料                   PIC 9(5).
002210       05 作１－相談支援回数                 PIC 9(1).
002210       05 作１－相談支援料                   PIC 9(5).
002210       05 作１－日.
002210          07 作１－施術日                    PIC X(1) OCCURS 31.
002760       05 作１－負傷部位データ  OCCURS 5.
002770          07 作１－負傷区分                  PIC 9(1).
002770          07 作１－負傷名                    PIC X(32).
002770          07 作１－整復回数                  PIC 9(1).
002210          07 作１－整復料                    PIC 9(5).
002780          07 作１－負傷年月日                PIC 9(8).
002840          07 作１－初検年月日                PIC 9(8).
002900          07 作１－開始年月日                PIC 9(8).
002960          07 作１－終了年月日                PIC 9(8).
003020          07 作１－部位実日数                PIC 9(2).
003030          07 作１－転帰区分                  PIC 9(1).
002770*          07 作１－後療回数                  PIC 9(2).
002770*          07 作１－冷罨法回数                PIC 9(1).
002770*          07 作１－温罨法回数                PIC 9(2).
002770*          07 作１－電療回数                  PIC 9(2).
003030          07 作１－多部位区分                PIC 9(1).
003030          07 作１－長期区分                  PIC 9(1).
             05 作１－登録記号番号                 PIC X(11).
             05 作１－保険種別                     PIC 9(1).
             05 作１－単併区分                     PIC 9(1).
             05 作１－本家区分                     PIC 9(1).
             05 作１－公費一部負担金               PIC 9(6).
             05 作１－公費請求額                   PIC 9(6).
002760       05 作１－部位別料金位データ  OCCURS 6.
                07 作１－逓減開始月日              PIC 9(4).
                07 作１－後療回数                  PIC 9(2).
                07 作１－後療料                    PIC 9(5).
                07 作１－冷罨法回数                PIC 9(1).
                07 作１－冷罨法料                  PIC 9(5).
                07 作１－温罨法回数                PIC 9(2).
                07 作１－温罨法料                  PIC 9(5).
                07 作１－電療回数                  PIC 9(2).
                07 作１－電療料                    PIC 9(5).
                07 作１－多部位逓減率              PIC 9(2).
                07 作１－多部位逓減額              PIC 9(5).
                07 作１－長期逓減率                PIC 9(2).
                07 作１－部位逓減率別料金計        PIC 9(5).
004470*       05 FILLER                             PIC X(14).
004471*
004490*----------------------------------------------------------------*
004500******************************************************************
004510*                WORKING-STORAGE SECTION                         *
004520******************************************************************
004530 WORKING-STORAGE         SECTION.
004540 01 キー入力                           PIC X    VALUE SPACE.
004550 01 状態キー                           PIC X(2) VALUE SPACE.
004560 01 終了フラグ                         PIC X(3) VALUE SPACE.
004570 01 終了フラグ２                       PIC X(3) VALUE SPACE.
004580 01 終了フラグ３                       PIC X(3) VALUE SPACE.
004590 01 実行キーＷ                         PIC X(3)  VALUE SPACE.
004600 01 施術記録有Ｗ                       PIC X(3) VALUE SPACE.
004610 01 ファイル名                         PIC N(8) VALUE SPACE.
004620 01 カウンタ                           PIC 9(2) VALUE ZERO.
004620 01 カウンタ２                         PIC 9(2) VALUE ZERO.
004630 01 文字カウンタ                       PIC 9(4) VALUE ZERO.
004631 01 文字カウンタ２                     PIC 9(4) VALUE ZERO.
004632 01 文字カウンタ３                     PIC 9(4) VALUE ZERO.
004633 01 文字カウンタ４                     PIC 9(4) VALUE ZERO.
004634 01 文字カウンタ５                     PIC 9(4) VALUE ZERO.
004635 01 文字カウンタ６                     PIC 9(4) VALUE ZERO.
001363 01 全角空白                           PIC X(2)  VALUE X"8140".
001364 01 半角空白                           PIC X(2)  VALUE X"2020".
004640 01 連番Ｗ                             PIC 9(4) VALUE ZERO.
004650 01 預金種別Ｗ                         PIC X(1) VALUE SPACE.
004660 01 柔整師コードＷ.
004670    03 協Ｗ                            PIC N(1) VALUE SPACE.
004680    03 柔整師コード数字Ｗ.
004690       05 柔整師コード数字１Ｗ         PIC X(2) VALUE SPACE.
004700       05 会員番号Ｗ                   PIC 9(9) VALUE ZERO.
004270 01 改行Ｗ                             PIC X(2)  VALUE X"0D0A".
       01 接骨師会会員番号Ｗ.
          03 接骨師会会員番号数字Ｗ          PIC 9(7) VALUE ZERO.
004710*
004720 01 請求西暦年Ｗ                       PIC 9(4) VALUE ZERO.
004730 01 請求和暦年月ＷＲ.
004740    03 請求和暦ＷＲ                    PIC 9(1) VALUE ZERO.
004750    03 請求年月ＷＲ.
004760       05 請求年ＷＲ                   PIC 9(2) VALUE ZERO.
004770       05 請求月ＷＲ                   PIC 9(2) VALUE ZERO.
004780*
007570    03 記号Ｗ.
007580       05 印刷記号Ｗ                   PIC N(12)  VALUE SPACE.
007660*
007670    03 番号Ｗ.
007680       05 印刷番号Ｗ                   PIC X(30)  VALUE SPACE.
007660*
002259** 西暦日付ワーク用
002260 01 西暦年月Ｗ.
002261    03 西暦年Ｗ                        PIC 9(4) VALUE ZERO.
002262    03 西暦月Ｗ                        PIC 9(2) VALUE ZERO.
002264** 西暦請求年月用
002265 01 西暦請求年月Ｗ.
002266    03 西暦請求年Ｗ                    PIC 9(4) VALUE ZERO.
002267    03 西暦請求月Ｗ                    PIC 9(2) VALUE ZERO.
002268** 西暦施術年月用
002269 01 西暦施術年月Ｗ.
002270    03 西暦施術年Ｗ                    PIC 9(4) VALUE ZERO.
002271    03 西暦施術月Ｗ                    PIC 9(2) VALUE ZERO.
002272** 西暦患者生年月日用
002273 01 西暦患者年月日Ｗ.
002274    03 西暦患者年Ｗ                    PIC 9(4) VALUE ZERO.
002275    03 西暦患者月Ｗ                    PIC 9(2) VALUE ZERO.
002276    03 西暦患者日Ｗ                    PIC 9(2) VALUE ZERO.
004820 01 和暦年月日Ｗ.
004830   03 和暦年月Ｗ.
004840      05 和暦Ｗ                        PIC 9(1) VALUE ZERO.
004850      05 年Ｗ                          PIC 9(2) VALUE ZERO.
004860      05 月Ｗ                          PIC 9(2) VALUE ZERO.
004870   03 日Ｗ                             PIC 9(2) VALUE ZERO.
004880 01 日付８桁Ｗ                         PIC X(8) VALUE SPACE.
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
005000 01 部位ＣＮＴ                         PIC 9(2) VALUE ZERO.
005010 01 部位ＣＮＴ２                       PIC 9(2) VALUE ZERO.
005020 01 同時負傷数Ｗ                       PIC 9(2) VALUE ZERO.
005030 01 最大同時負傷数Ｗ                   PIC 9(2) VALUE ZERO.
005040 01 継続部位数Ｗ                       PIC 9    VALUE ZERO.
005050 01 最大継続部位数Ｗ                   PIC 9    VALUE ZERO.
005060 01 負傷終了和暦年月日Ｗ               PIC 9(7) VALUE ZERO.
008280 01 負傷種別ＷＴ                       PIC 9(2)  VALUE ZERO.
002720 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
005080 01 部位タイプＷ                       PIC 9    VALUE ZERO.
005090*
005100 01 遅延フラグ                         PIC X(3) VALUE SPACE.
005110 01 遅延ＣＮＴ                         PIC 9(5) VALUE ZERO.
005120 01 遅延カウンタ                       PIC 9(4) VALUE ZERO.
005130 01 遅延回数Ｗ                         PIC 9(4) VALUE ZERO.
005490*
005500 01 退避項目ＧＷ.
005510   03 レセプト種類Ｗ                   PIC X(4).
005520   03 レセプト種類ＧＷ                 PIC X(4).
005530   03 レセプト種別ＧＷ                 PIC 9(2).
005140*
005150 01 英数字項目Ｗ.
005160   03 英数字項目ＸＷ                   PIC X(70) VALUE SPACE.
005170 01 日本語項目Ｗ.
005180   03 日本語項目ＮＷ                   PIC N(35) VALUE SPACE.
005190 01 英数字項目２Ｗ                     PIC X(70) VALUE SPACE.
005200*
       01 明細.
          03 新規Ｗ                          PIC 9(1) VALUE ZERO.
          03 継続Ｗ                          PIC 9(1) VALUE ZERO.
          03 初検料Ｗ                        PIC 9(5) VALUE ZERO.
          03 初検加算Ｗ                      PIC 9(5) VALUE ZERO.
          03 初検回数Ｗ                      PIC 9(1) VALUE ZERO.
          03 初検時間外回数Ｗ                PIC 9(1) VALUE ZERO.
          03 初検休日回数Ｗ                  PIC 9(1) VALUE ZERO.
          03 初検深夜回数Ｗ                  PIC 9(1) VALUE ZERO.
          03 再検料Ｗ                        PIC 9(5) VALUE ZERO.
          03 再検回数Ｗ                      PIC 9(1) VALUE ZERO.
          03 往療料Ｗ                        PIC 9(5) VALUE ZERO.
          03 往療加算Ｗ                      PIC 9(5) VALUE ZERO.
          03 往療距離Ｗ                      PIC 9(3) VALUE ZERO.
          03 往療回数Ｗ                      PIC 9(2) VALUE ZERO.
          03 夜間回数Ｗ                      PIC 9(1) VALUE ZERO.
          03 難路回数Ｗ                      PIC 9(2) VALUE ZERO.
          03 暴風雨回数Ｗ                    PIC 9(2) VALUE ZERO.
          03 金属副子加算Ｗ                  PIC 9(5) VALUE ZERO.
      */金属副子変更/20180611
000561    03 金属副子回数Ｗ                  PIC 9(2) VALUE ZERO.
      *    03 金属大回数Ｗ                    PIC 9(1) VALUE ZERO.
      *    03 金属中回数Ｗ                    PIC 9(1) VALUE ZERO.
      *    03 金属小回数Ｗ                    PIC 9(1) VALUE ZERO.
          03 情報提供料Ｗ                    PIC 9(5) VALUE ZERO.
          03 情報回数Ｗ                      PIC 9(1) VALUE ZERO.
          03 相談支援料Ｗ                    PIC 9(5) VALUE ZERO.
          03 相談支援回数Ｗ                  PIC 9(1) VALUE ZERO.
          03 施術日Ｗ                        PIC X(1) VALUE SPACE OCCURS 31.
      */運動後療追加/20180607
          03 運動後療料回数Ｗ                PIC 9(1)    VALUE ZERO.
          03 運動後療料Ｗ                    PIC 9(5)    VALUE ZERO.
          03 負傷Ｗ  OCCURS 9.
             05 負傷区分Ｗ                   PIC 9(1) VALUE ZERO.
             05 負傷名Ｗ.
                07 負傷名ＷＴ                PIC X(32) VALUE SPACE.
             05 負傷名ＷＰ                   PIC N(16) VALUE SPACE.
             05 整復回数Ｗ                   PIC 9(1) VALUE ZERO.
             05 整復料Ｗ                     PIC 9(5) VALUE ZERO.
             05 負傷年月日Ｗ                 PIC 9(8) VALUE ZERO.
             05 初検年月日Ｗ                 PIC 9(8) VALUE ZERO.
             05 開始年月日Ｗ                 PIC 9(8) VALUE ZERO.
             05 終了年月日Ｗ                 PIC 9(8) VALUE ZERO.
             05 部位実日数Ｗ                 PIC 9(2) VALUE ZERO.
             05 転帰区分Ｗ                   PIC 9(1) VALUE ZERO.
             05 後療回数Ｗ                   PIC 9(2) VALUE ZERO.
             05 冷罨法回数Ｗ                 PIC 9(1) VALUE ZERO.
             05 温罨法回数Ｗ                 PIC 9(2) VALUE ZERO.
             05 電療回数Ｗ                   PIC 9(2) VALUE ZERO.
             05 多部位区分Ｗ                 PIC 9(1) VALUE ZERO.
             05 長期区分Ｗ                   PIC 9(1) VALUE ZERO.
      *
005390****************
005400* 負傷データＦ *
005410****************
005420 01 負傷情報Ｗ.
005430    03 部位情報Ｗ  OCCURS   9.
008260       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
008270       05 部位コードＷ.
008280          07 負傷種別Ｗ                PIC 9(2)  VALUE ZERO.
008290          07 部位Ｗ                    PIC 9(2)  VALUE ZERO.
008300          07 左右区分Ｗ                PIC 9(1)  VALUE ZERO.
008310          07 負傷位置番号Ｗ            PIC 9(2)  VALUE ZERO.
005440       05 部位負傷年月日Ｗ.
005450          07 部位負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
005460          07 部位負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
005470          07 部位負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
005480       05 部位初検年月日Ｗ.
005490          07 部位初検年Ｗ                  PIC 9(2)  VALUE ZERO.
005500          07 部位初検月Ｗ                  PIC 9(2)  VALUE ZERO.
005510          07 部位初検日Ｗ                  PIC 9(2)  VALUE ZERO.
005520       05 部位開始和暦年月日Ｗ.
005530          07 部位開始和暦Ｗ                PIC 9(1)  VALUE ZERO.
005540          07 部位開始年Ｗ                  PIC 9(2)  VALUE ZERO.
005550          07 部位開始月Ｗ                  PIC 9(2)  VALUE ZERO.
005560          07 部位開始日Ｗ                  PIC 9(2)  VALUE ZERO.
005570       05 部位終了和暦年月日Ｗ.
005580          07 部位終了和暦Ｗ                PIC 9(1)  VALUE ZERO.
005590          07 部位終了年Ｗ                  PIC 9(2)  VALUE ZERO.
005600          07 部位終了月Ｗ                  PIC 9(2)  VALUE ZERO.
005610          07 部位終了日Ｗ                  PIC 9(2)  VALUE ZERO.
005630* 退避用
005640 01 終了和暦年月日ＷＴ.
005650    03 終了和暦ＷＴ                    PIC 9(1)  VALUE ZERO.
005660    03 終了年ＷＴ                      PIC 9(2)  VALUE ZERO.
005670    03 終了月ＷＴ                      PIC 9(2)  VALUE ZERO.
005680    03 終了日ＷＴ                      PIC 9(2)  VALUE ZERO.
005690*
005700 01 初日再検フラグ                     PIC X(3)  VALUE SPACE.
005710*
005720 01 請求先名称Ｗ                       PIC X(60) VALUE SPACE.
005722
005723 01 FD-NAME                            PIC X(30) VALUE SPACE.
005724*
005725 01 保険種別Ｗ                         PIC X(2)  VALUE SPACE.
005190*
005200* 負傷原因用
005210 01 負傷原因ＷＴ.
005220    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
005230    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
005240    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
005250    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
005260    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
005270    03 負傷原因ナンバーＷＴ.
005280       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
005290    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
005300 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
005310 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
005320 01 負傷原因ＴＢＬ.
005330    03 負傷原因コードＴＢＬ            OCCURS 9.
005340       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
005350       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
005360       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
005370 01 負傷原因内容Ｗ.
005380    03 負傷原因内容合成Ｗ              PIC X(318) OCCURS 9 VALUE SPACE.
005390    03 負傷原因内容分解Ｗ.
005400       05 負傷原因内容１Ｗ             PIC X(60)  VALUE SPACE.
005410       05 負傷原因内容２Ｗ             PIC X(60)  VALUE SPACE.
005420       05 負傷原因内容３Ｗ             PIC X(60)  VALUE SPACE.
005400       05 負傷原因内容４Ｗ             PIC X(60)  VALUE SPACE.
005410       05 負傷原因内容５Ｗ             PIC X(60)  VALUE SPACE.
005420       05 負傷原因内容６Ｗ             PIC X(18)  VALUE SPACE.
005900    03 負傷原因Ｗ                      PIC X(60) OCCURS 54 VALUE SPACE.
       01 長期理由文ＷＴ.
          03 長期理由ＷＴ                    PIC N(56) OCCURS 2 VALUE SPACE.
       01 長期理由文Ｗ.
          03 長期理由Ｗ                      PIC X(200) VALUE SPACE.
005430*
005440** 負傷原因・長期理由印刷区分用
005450 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
005460 01 長期理由印刷区分Ｗ                 PIC 9 VALUE ZERO.
004290** エラーメッセージ用
004300 01 エラーメッセージＷ.
004310    03 エラー患者コードＷ              PIC X(7) VALUE SPACE.
004320    03 エラー区切りＷ                  PIC X(1) VALUE SPACE.
004330    03 エラー保険種別Ｗ                PIC X(2) VALUE SPACE.
004340    03 FILLER                          PIC X(10) VALUE SPACE.
       01 翌月西暦年月Ｗ.
          03 翌月西暦年Ｗ                    PIC 9(4)  VALUE ZERO.
          03 翌月西暦月Ｗ                    PIC 9(2)  VALUE ZERO.
       01 転記用負傷原因Ｗ.
          03 転記用負傷原因ＮＷ              PIC N(150) VALUE SPACE.
005370 01 転記用負傷原因内容Ｗ.
005380    03 転記用負傷原因内容合成Ｗ        OCCURS 9.
005380       05 転記用負傷原因内容合成ＸＷ   PIC X(318) VALUE SPACE.
      *
       01 転記用長期理由文Ｗ.
          03 転記用理由文ＸＷ OCCURS 5.
             05 転記用理由文Ｗ                PIC X(322) VALUE SPACE.
       01 番号ＷＰ                            PIC X(2)   VALUE SPACE.
003950** 負傷原因印刷区分
003960 01 レセ負傷原因印刷区分Ｗ             PIC 9    VALUE ZERO.
002580 01 レセ長期理由印刷区分Ｗ             PIC 9    VALUE ZERO.
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
014774*
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
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
005870****************
005880* 画面入力情報 *
005890****************
      * 01 連入－画面情報ＹＮＫ５８０ IS EXTERNAL.
      *    03 連入－施術和暦年月.
      *       05 連入－施術和暦               PIC 9.
      *       05 連入－施術年月.
      *         07 連入－施術年               PIC 9(2).
      *         07 連入－施術月               PIC 9(2).
      *    03 連入－送付和暦年月日.
      *       05 連入－送付和暦               PIC 9(1).
      *       05 連入－送付年月日.
      *          07 連入－送付年              PIC 9(2).
      *          07 連入－送付月              PIC 9(2).
      *          07 連入－送付日              PIC 9(2).
       01 連入－画面情報ＹＡＳ５８０ IS EXTERNAL.
          03 連入－請求和暦年月.
             05 連入－請求和暦               PIC 9.
             05 連入－請求年月.
               07 連入－請求年               PIC 9(2).
               07 連入－請求月               PIC 9(2).
014762************************
014763* レセ負傷原因印刷判定
014764************************
014765 01 連レセ負原印－キー IS EXTERNAL.
014766    03 連レセ負原印－施術年月.
014767       05 連レセ負原印－施術和暦               PIC 9.
014768       05 連レセ負原印－施術年                 PIC 9(2).
014769       05 連レセ負原印－施術月                 PIC 9(2).
014770    03  連レセ負原印－患者コード.
014771       05 連レセ負原印－患者番号               PIC 9(6).
014772       05 連レセ負原印－枝番                   PIC X.
014773    03 連レセ負原印－対象フラグ                PIC X(3).
013680*
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
006510*
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
           PERFORM 翌月セット.
006470     PERFORM 作業ファイル作成.
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
006780             MOVE NC"施情" TO ファイル名.
006790             PERFORM オープンチェック.
006800     OPEN INPUT 元号マスタ.
006810             MOVE NC"元号" TO ファイル名.
006820             PERFORM オープンチェック.
006860     OPEN INPUT 制御情報マスタ.
006870             MOVE NC"制御" TO ファイル名.
006880             PERFORM オープンチェック.
014420     OPEN INPUT 名称マスタ
014430             MOVE NC"名称" TO ファイル名.
014440             PERFORM オープンチェック.
008190     OPEN INPUT 負傷原因Ｆ.
008200             MOVE NC"負傷原因" TO ファイル名.
008210             PERFORM オープンチェック.
002650     OPEN INPUT   長期継続者Ｆ.
002651         MOVE NC"長期継続者Ｆ" TO ファイル名.
002652         PERFORM オープンチェック.
006630     OPEN INPUT 生保情報Ｆ.
006640         MOVE NC"生保" TO ファイル名.
006650         PERFORM オープンチェック.
006630     OPEN INPUT 労災情報Ｆ.
006640         MOVE NC"労災" TO ファイル名.
006650         PERFORM オープンチェック.
006980*
007160*================================================================*
007170 オープンチェック SECTION.
007180*
007190     IF 状態キー  NOT =  "00"
007200         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
007210         DISPLAY NC"状態キー：" 状態キー         UPON CONS
007220         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
004680*-----------------------------------------*
004690         CALL "actcshm"  WITH C LINKAGE
004700*-----------------------------------------*
007230         ACCEPT  キー入力 FROM CONS
007240         PERFORM ファイル閉鎖
007250         MOVE 99 TO PROGRAM-STATUS
007260         EXIT PROGRAM.
007270*================================================================*
007280 ファイル閉鎖 SECTION.
007290*
007300     CLOSE 受診者情報Ｆ        負傷データＦ     施術記録Ｆ
007310           施術所情報マスタ    元号マスタ       レセプトＦ
007320           制御情報マスタ      名称マスタ       負傷原因Ｆ
                 長期継続者Ｆ        生保情報Ｆ       労災情報Ｆ.
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
009810*================================================================*
007390 エラー表示Ｒ SECTION.
007400*
007410     DISPLAY NC"状態キー" 状態キー  UPON CONS.
007420     DISPLAY NC"ファイル読込エラー：" ファイル名   UPON CONS.
007430     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
007440     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
004910*-----------------------------------------*
004920     CALL "actcshm"  WITH C LINKAGE.
004930*-----------------------------------------*
007450     ACCEPT  キー入力 FROM CONS.
007460     PERFORM ファイル閉鎖.
007470     MOVE 99 TO PROGRAM-STATUS.
007480     EXIT PROGRAM.
009810*================================================================*
009820 連結項目退避 SECTION.
009830*
009840     MOVE 連入－請求和暦 TO 請求和暦ＷＲ.
009850     MOVE 連入－請求年   TO 請求年ＷＲ.
009860     MOVE 連入－請求月   TO 請求月ＷＲ.
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
008810         MOVE 制－レセ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
008820         MOVE 制－レセ長期理由印刷区分 TO 長期理由印刷区分Ｗ
022580     END-READ.
022841*
009870*================================================================*
009880 西暦年取得 SECTION.
009890*
009900     MOVE ZERO   TO 西暦年Ｗ.
009910     MOVE 和暦Ｗ TO 元－元号区分.
009920     READ 元号マスタ
009930     NOT INVALID KEY
009940         COMPUTE 西暦年Ｗ = 元－開始西暦年 + 年Ｗ - 1
009950     END-READ.
008102*================================================================*
008103 西暦施術年月取得 SECTION.
008104* 
008105     MOVE ZERO          TO 西暦年月Ｗ  西暦施術年月Ｗ.
008106     MOVE 受－施術和暦  TO 元－元号区分.
008107     READ 元号マスタ
008108     NOT INVALID KEY
008109         MOVE 元－開始西暦年 TO 西暦年Ｗ
008110     END-READ.
008111**
008112     IF 西暦年Ｗ = ZERO
008113          MOVE  NC"元号マスタに開始西暦年を登録して下さい" TO 連メ－メッセージ
008114          CALL   "MSG001"
008115          CANCEL "MSG001"
008116          PERFORM ファイル閉鎖
008117          MOVE 99 TO PROGRAM-STATUS
008118          EXIT PROGRAM
008119     ELSE
008120          COMPUTE 西暦年Ｗ = 西暦年Ｗ + 受－施術年 - 1
008121          MOVE 受－施術月 TO 西暦月Ｗ
008122     END-IF.
008123*
008124     MOVE 西暦年月Ｗ   TO  西暦施術年月Ｗ.
008125*
008126*================================================================*
008127 西暦請求年月取得 SECTION.
008128* 
008129     MOVE ZERO          TO 西暦年月Ｗ  西暦請求年月Ｗ.
008130     MOVE レセ－請求和暦  TO 元－元号区分.
008131     READ 元号マスタ
008132     NOT INVALID KEY
008133         MOVE 元－開始西暦年 TO 西暦年Ｗ
008134     END-READ.
008135**
008136     IF 西暦年Ｗ = ZERO
008137          MOVE  NC"元号マスタに開始西暦年を登録して下さい" TO 連メ－メッセージ
008138          CALL   "MSG001"
008139          CANCEL "MSG001"
008140          PERFORM ファイル閉鎖
008141          MOVE 99 TO PROGRAM-STATUS
008142          EXIT PROGRAM
008143     ELSE
008144          COMPUTE 西暦年Ｗ = 西暦年Ｗ + レセ－請求年 - 1
008145          MOVE レセ－請求月 TO 西暦月Ｗ
008146     END-IF.
008147*
008148     MOVE 西暦年月Ｗ   TO  西暦請求年月Ｗ.
008149*
008150*================================================================*
008151 西暦患者年月日取得 SECTION.
008152* 
008153* 14/10追加
008154     MOVE ZERO          TO 西暦患者年月日Ｗ.
008155     MOVE 受－患者和暦  TO 元－元号区分.
008156     READ 元号マスタ
008157     NOT INVALID KEY
008158          COMPUTE 西暦患者年Ｗ = 元－開始西暦年 + 受－患者年 - 1
008159          MOVE 受－患者月 TO 西暦患者月Ｗ
008160          MOVE 受－患者日 TO 西暦患者日Ｗ
008161     END-READ.
008162*
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
008970     NOT INVALID KEY
009060              MOVE 施情－新柔整師番号  TO 柔整師コードＷ
009620     END-READ.
009630*
010730*================================================================*
010740 レセプトＦ読込 SECTION.
010750*
010760     READ レセプトＦ NEXT
010770     AT END
010780         MOVE "YES" TO 終了フラグ
010790     END-READ.
026550*================================================================*
026560 作業ファイル作成 SECTION.
026570*
026580     OPEN OUTPUT 作業ファイル１.
026590         MOVE NC"作１" TO ファイル名.
026600         PERFORM オープンチェック.
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
005580         PERFORM レセプトＦ読込
005590         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
005600                       ( レセ－請求和暦 NOT = 請求和暦ＷＲ ) OR
005610                       ( レセ－請求年   NOT = 請求年ＷＲ   ) OR
005620                       ( レセ－請求月   NOT = 請求月ＷＲ   )
026910*
026920            PERFORM データチェック
004140** 労災・自賠責・自由は対象外/労災・生保を対象外にする/130812
004150            IF  受－保険種別 = 80 OR 90 OR 70 OR 85
004160                MOVE SPACE  TO 実行キーＷ
004170            END-IF
026930            IF 実行キーＷ = "YES"
005933                MOVE SPACE TO 明細
005933                MOVE SPACE TO 作１－レコード
005934                INITIALIZE 作１－レコード
028310                PERFORM 負傷データＦセット
028320                PERFORM 施術記録Ｆセット
006441                PERFORM 料金情報取得
      */080206
                      PERFORM 後療罨法電料回数セット
                      PERFORM 共通レコードセット 
                      EVALUATE TRUE
004230*            ********
004240*            * 健保 *
004250*            ********
004273                WHEN レセ－レセ種別 = 1
004280*                **********************
004290*                * 作業ファイル作成 *
004300*                **********************
004330                    PERFORM 作１レコードセット健保
004522*            ********
004523*            * 労災 *
004524*            ********
004525                WHEN レセ－レセ種別 = 4
004526                    PERFORM 作１レコードセット労災
004529*            ********
004530*            * 生保 *
004531*            ********
004525                WHEN レセ－レセ種別 = 7
004534                    PERFORM 作１レコードセット生保
004539*            ********
004540*            * 老人 *
004550*            ********
004525                WHEN レセ－レセ種別 = 2
004601                    PERFORM 作１レコードセット老人
004539*            ********
004540*            * 助成 *
004550*            ********
004525                WHEN レセ－レセ種別 = 3
004640                    PERFORM 作１レコードセット助成
                      END-EVALUATE
005930*         / 助成の請求額０は対象外にする /170208
005930*         / 大阪の助成の請求額０は対象にする /170405
005930*         / 静岡の助成の請求額０は対象にする /170508
006880                IF (レセ－レセ種別 = 3) AND (レセ－助成請求金額 = ZERO) AND
                             (受－費用負担者番号助成(3:2) NOT = "22" AND "27")
                          CONTINUE
                      ELSE
004602                    PERFORM 作１ファイル書込
                      END-IF
005200            END-IF
027850            PERFORM レセプトＦ読込
027860         END-PERFORM
027870     END-IF.
027880     CLOSE 作業ファイル１.
027890*================================================================*
027900 作１ファイル書込 SECTION.
027910*
027920     WRITE 作１－レコード
027930     IF 状態キー  NOT =  "00"
027940         MOVE NC"作１"  TO ファイル名
027950         PERFORM エラー表示
027960     END-IF.
005928*================================================================*
005929 作１レコードセット健保 SECTION.
005930*
           MOVE 1                   TO 作１－助成区分.
005943     MOVE 受－本人家族区分    TO 作１－本人家族区分.
010840     MOVE レセ－合計          TO 作１－合計金額.
010850     MOVE レセ－一部負担金    TO 作１－一部負担金.
010860     MOVE レセ－請求金額      TO 作１－請求金額.
005928*================================================================*
005929 作１レコードセット労災 SECTION.
005930*
           MOVE 1                   TO 作１－助成区分.
005943     MOVE 受－本人家族区分    TO 作１－本人家族区分.
010840     MOVE レセ－合計          TO 作１－合計金額.
010850     MOVE レセ－一部負担金    TO 作１－一部負担金.
010860     MOVE レセ－請求金額      TO 作１－請求金額.
011610     MOVE 受－患者カナ        TO 作１－被保険者カナ.
011620     MOVE 受－患者氏名        TO 作１－被保険者氏名.
           STRING 受－患者住所１    DELIMITED BY SPACE
022890            受－患者住所２    DELIMITED BY SPACE
              INTO 作１－住所
           END-STRING.
           MOVE 受－施術和暦年月 TO 労災－施術和暦年月
           MOVE 受－患者コード   TO 労災－患者コード
           READ 労災情報Ｆ
           NOT INVALID KEY
012470         MOVE 労災－労働保険番号  TO 作１－保険者番号
           END-READ.
005928*================================================================*
005929 作１レコードセット生保 SECTION.
005930*
           MOVE 3                   TO 作１－助成区分.
005943     MOVE 受－本人家族区分    TO 作１－本人家族区分.
010840     MOVE レセ－合計          TO 作１－合計金額.
010850     MOVE レセ－一部負担金    TO 作１－一部負担金.
010860     MOVE レセ－請求金額      TO 作１－請求金額.
011610     MOVE 受－患者カナ        TO 作１－被保険者カナ.
011620     MOVE 受－患者氏名        TO 作１－被保険者氏名.
           STRING 受－患者住所１    DELIMITED BY SPACE
022890            受－患者住所２    DELIMITED BY SPACE
              INTO 作１－住所
           END-STRING.
           MOVE 受－施術和暦年月 TO 生保－施術和暦年月.
           MOVE 受－患者コード   TO 生保－患者コード.
           READ 生保情報Ｆ
           NOT INVALID KEY
012470         MOVE 生保－負担者番号  TO 作１－保険者番号
           END-READ.
011000*================================================================*
011010 作１レコードセット老人 SECTION.
011020*
      */後期高齢者/080414
           IF 受－施術和暦年月 < 42004
               MOVE 2               TO 作１－助成区分
           ELSE
               MOVE 1               TO 作１－助成区分
           END-IF
005943     MOVE 1                   TO 作１－本人家族区分.
011030*/料金
011040     MOVE レセ－合計          TO 作１－合計金額.
011050     MOVE レセ－一部負担金    TO 作１－一部負担金.
011060     MOVE レセ－請求金額      TO 作１－請求金額.
011120*================================================================*
011130 作１レコードセット助成 SECTION.
011140*
           MOVE 3                   TO 作１－助成区分.
005943     MOVE 受－本人家族区分    TO 作１－本人家族区分.
011150*/料金
011160*/費用額は健保のもの
011170     MOVE レセ－合計          TO 作１－合計金額.
011180     MOVE レセ－一部負担金    TO 作１－一部負担金.
010860     MOVE レセ－請求金額      TO 作１－請求金額.
           MOVE レセ－受給者負担額  TO 作１－公費一部負担金.
           MOVE レセ－助成請求金額  TO 作１－公費請求額.
011400*================================================================*
011410 共通レコードセット SECTION.
011420*
      */請求年月、施術年月
006364     MOVE レセ－請求和暦       TO 作１－請求和暦.
006365     MOVE レセ－請求年         TO 作１－請求年.
006366     MOVE レセ－請求月         TO 作１－請求月.
006367     MOVE レセ－施術和暦       TO 作１－施術和暦.
006368     MOVE レセ－施術年         TO 作１－施術年.
006369     MOVE レセ－施術月         TO 作１－施術月.
011450*/接骨院ＩＤ
011460     PERFORM 施術所情報取得.
           MOVE 施情－接骨師会会員番号     TO 接骨師会会員番号Ｗ.
      */会員番号６桁を右詰め前ゼロの７桁で転記/130218
           MOVE 接骨師会会員番号数字Ｗ(1:6) TO 作１－会員番号(2:6).
011480*/患者コード
011490     MOVE 受－患者番号               TO 作１－患者番号.
011500     MOVE 受－枝番                   TO 作１－枝番.
011510*     PERFORM 枝番変換.
011580*/名前
011590     MOVE 受－患者カナ               TO 作１－患者カナ.
011600     MOVE 受－患者氏名               TO 作１－患者氏名.
011610     MOVE 受－被保険者カナ           TO 作１－被保険者カナ.
011620     MOVE 受－被保険者氏名           TO 作１－被保険者氏名.
           STRING 受－住所１         DELIMITED BY SPACE
022890            受－住所２         DELIMITED BY SPACE
              INTO 作１－住所
           END-STRING.
           EVALUATE 受－保険種別
           WHEN 02
           WHEN 06
           WHEN 07
               MOVE 1                      TO 作１－集計順
           WHEN 03
               MOVE 2                      TO 作１－集計順
           WHEN 04
           WHEN 09
               MOVE 3                      TO 作１－集計順
           WHEN OTHER
               MOVE 4                      TO 作１－集計順
           END-EVALUATE.
011640     MOVE 受－保険者番号         TO 作１－保険者番号.
011750*/助成
011760     IF (受－助成種別           NOT = ZERO ) AND
011770        (受－費用負担者番号助成 NOT = SPACE)
011780         IF 受－費用負担者番号助成(1:2) NOT = "99"
011790             MOVE 受－費用負担者番号助成 TO 作１－助成負担者番号
011800         END-IF
011820         IF (受－受益者番号助成(1:1) NOT = "*" ) AND 
011830            (受－受益者番号助成(1:2) NOT = "＊")
011840             MOVE 受－受益者番号助成 TO 作１－助成受給者番号
011850         END-IF
011860     END-IF.
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
011870*/保険証記号番号
011880     IF (印刷記号Ｗ(1:1) NOT = NC"＊")
011890         MOVE 記号Ｗ               TO 作１－記号
011900     END-IF.
011910     IF (番号Ｗ(1:1) NOT = "*") AND (番号Ｗ(1:2) NOT = "＊")
011920         MOVE 番号Ｗ               TO 作１－番号
011930     END-IF.
      */給付割合
           MOVE レセ－給付割合           TO 作１－給付割合
      */神奈川県の場合、前期高齢者１割は、給付割合を８割にする。(国が１割負担するため、患者１割、保険者８割、国１割となる)
           IF (受－保険種別 NOT = 05 ) AND (受－特別区分 = 1)
               MOVE 8 TO 作１－給付割合
018720     END-IF.
012390*/実日数
006312     MOVE レセ－レセ実日数           TO 作１－実日数.
011940*/患者生年月日
011950     MOVE 受－患者生年月日           TO 和暦年月日Ｗ.
011960     PERFORM 日付変換８桁.
011970     MOVE 日付８桁Ｗ                 TO 作１－患者生年月日.
011980*/性別１：男、２：女
011990     IF 受－患者性別 = 1
012000         MOVE 1                      TO 作１－性別
012010     ELSE
012020         MOVE 2                      TO 作１－性別
012030     END-IF.
      */請求年月の翌月をセットする/080208
012150**/請求年月
006265*     PERFORM 西暦請求年月取得.
006266*     MOVE 西暦請求年月Ｗ     TO 作１－請求年月.
            MOVE 翌月西暦年月Ｗ TO 作１－請求年月.
012250*/施術年月
006267     PERFORM 西暦施術年月取得.
006268     MOVE 西暦施術年月Ｗ     TO 作１－施術年月.
      */請求区分
           IF レセ－請求区分 = 2
006212         MOVE 1            TO 作１－再請求区分
           ELSE
006212         MOVE ZERO         TO 作１－再請求区分
           END-IF.
012430*/部位数
012440     MOVE 負－部位数       TO 作１－部位数.
      */レセコン業者区分：12 マキシーに固定
006212     MOVE 12               TO 作１－レセコン区分.
      */後期高齢車は転記しない/080414
           IF 受－保険種別 NOT = 05
      */高齢者区分
               EVALUATE 受－特別区分
               WHEN 1
                   MOVE 1              TO 作１－高齢者区分
               WHEN 2
                   MOVE 2              TO 作１－高齢者区分
               WHEN 3
                   MOVE 3              TO 作１－高齢者区分
               WHEN 6
                   MOVE 6              TO 作１－高齢者区分
               WHEN OTHER
                   MOVE ZERO           TO 作１－高齢者区分
               END-EVALUATE
           END-IF.
014000*/負傷原因、長期理由
016791*-----------------------------------------------*
018370*     IF 負傷原因印刷区分Ｗ  NOT = 1 
016800     IF ( 負傷原因印刷区分Ｗ  NOT = 1 ) AND ( レセ負傷原因印刷区分Ｗ NOT = 1 )
016813         IF ( 負傷原因印刷区分Ｗ = 3 OR 4 )
016815             PERFORM 負傷原因印刷対象判定処理
016817         ELSE
018380*          / 負傷原因用WORKクリアー /
018390             INITIALIZE 負傷原因ＷＴ
018400             INITIALIZE 負傷患者番号ＣＷ
018410             INITIALIZE 負傷連番ＣＷ
018420             INITIALIZE 負傷原因ＴＢＬ
018430             INITIALIZE 負傷原因内容Ｗ
018440             PERFORM 負傷原因取得
016821         END-IF
018450     END-IF.
016831*-----------------------------------------------*
018460*
           MOVE 負傷原因内容合成Ｗ(1) TO 転記用負傷原因内容合成Ｗ(1)
           MOVE 負傷原因内容合成Ｗ(2) TO 転記用負傷原因内容合成Ｗ(2)
           MOVE 負傷原因内容合成Ｗ(3) TO 転記用負傷原因内容合成Ｗ(3)
           MOVE 負傷原因内容合成Ｗ(4) TO 転記用負傷原因内容合成Ｗ(4)
           MOVE 負傷原因内容合成Ｗ(5) TO 転記用負傷原因内容合成Ｗ(5)
           MOVE 負傷原因内容合成Ｗ(6) TO 転記用負傷原因内容合成Ｗ(6)
           MOVE 負傷原因内容合成Ｗ(7) TO 転記用負傷原因内容合成Ｗ(7)
           MOVE 負傷原因内容合成Ｗ(8) TO 転記用負傷原因内容合成Ｗ(8)
           MOVE 負傷原因内容合成Ｗ(9) TO 転記用負傷原因内容合成Ｗ(9)
           INSPECT 転記用負傷原因内容Ｗ REPLACING ALL "　" BY "  "
           STRING 転記用負傷原因内容合成Ｗ(1) DELIMITED BY "  "
                  "　"                        DELIMITED BY SIZE
                  転記用負傷原因内容合成Ｗ(2) DELIMITED BY "  "
                  "　"                        DELIMITED BY SIZE
                  転記用負傷原因内容合成Ｗ(3) DELIMITED BY "  "
                  "　"                        DELIMITED BY SIZE
                  転記用負傷原因内容合成Ｗ(4) DELIMITED BY "  "
                  "　"                        DELIMITED BY SIZE
                  転記用負傷原因内容合成Ｗ(5) DELIMITED BY "  "
                  "　"                        DELIMITED BY SIZE
                  転記用負傷原因内容合成Ｗ(6) DELIMITED BY "  "
                  "　"                        DELIMITED BY SIZE
                  転記用負傷原因内容合成Ｗ(7) DELIMITED BY "  "
                  "　"                        DELIMITED BY SIZE
                  転記用負傷原因内容合成Ｗ(8) DELIMITED BY "  "
                  "　"                        DELIMITED BY SIZE
                  転記用負傷原因内容合成Ｗ(9) DELIMITED BY "  "
                  INTO 作１－負傷原因
           END-STRING
      */長期理由
           MOVE SPACE          TO 転記用長期理由文Ｗ.
004502     MOVE 受－施術和暦   TO 長継－施術和暦
004503     MOVE 受－施術年     TO 長継－施術年.
004504     MOVE 受－施術月     TO 長継－施術月.
004505     MOVE 受－患者番号   TO 長継－患者番号.
004506     MOVE 受－枝番       TO 長継－枝番.
004507*
004508     READ 長期継続者Ｆ 
004522     NOT INVALID KEY
           PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 5 
               IF 長継－理由文(部位ＣＮＴ) NOT = SPACE
                   EVALUATE 部位ＣＮＴ
                   WHEN  1
                       MOVE "①"  TO 番号ＷＰ
                   WHEN  2
                       MOVE "②"  TO 番号ＷＰ
                   WHEN  3
                       MOVE "③"  TO 番号ＷＰ
                   WHEN  4
                       MOVE "④"  TO 番号ＷＰ
                   WHEN  5
                       MOVE "⑤"  TO 番号ＷＰ
                   END-EVALUATE
      *
                   STRING 番号ＷＰ                 DELIMITED BY SIZE
                          長継－理由文(部位ＣＮＴ) DELIMITED BY SPACE
                     INTO 転記用理由文Ｗ(部位ＣＮＴ)
                   END-STRING
               END-IF
           END-PERFORM.
           INSPECT 転記用長期理由文Ｗ REPLACING ALL "　" BY "  "
           STRING 転記用理由文ＸＷ(1) DELIMITED BY "  "
                  "　"                DELIMITED BY SIZE
                  転記用理由文ＸＷ(2) DELIMITED BY "  "
                  "　"                DELIMITED BY SIZE
                  転記用理由文ＸＷ(3) DELIMITED BY "  "
                  "　"                DELIMITED BY SIZE
                  転記用理由文ＸＷ(4) DELIMITED BY "  "
                  "　"                DELIMITED BY SIZE
                  転記用理由文ＸＷ(5) DELIMITED BY "  "
             INTO 作１－長期理由
           END-STRING.
      */新規、継続
           MOVE 新規Ｗ              TO 作１－新規区分.
           MOVE 継続Ｗ              TO 作１－継続区分.
      */初検
           MOVE 初検料Ｗ            TO 作１－初検料.
           MOVE 初検加算Ｗ          TO 作１－初検加算.
           MOVE 初検回数Ｗ          TO 作１－初検回数.
           MOVE 初検時間外回数Ｗ    TO 作１－初検時間外回数.
           MOVE 初検休日回数Ｗ      TO 作１－初検休日回数.
           MOVE 初検深夜回数Ｗ      TO 作１－初検深夜回数.
      */再検
           MOVE 再検料Ｗ            TO 作１－再検料.
           MOVE 再検回数Ｗ          TO 作１－再検回数.
      */往療
           MOVE 往療料Ｗ            TO 作１－往療料.
           MOVE 往療加算Ｗ          TO 作１－往療加算.
           MOVE 往療距離Ｗ          TO 作１－往療距離.
           MOVE 往療回数Ｗ          TO 作１－往療回数.
           IF 夜間回数Ｗ = 1
              MOVE 夜間回数Ｗ       TO 作１－夜間回数
           ELSE
              MOVE ZERO             TO 作１－夜間回数
           END-IF.
           IF 難路回数Ｗ = 1
              MOVE 難路回数Ｗ       TO 作１－難路回数
           ELSE
              MOVE ZERO             TO 作１－難路回数
           END-IF.
           IF 暴風雨回数Ｗ = 1
              MOVE 暴風雨回数Ｗ     TO 作１－暴風雨回数
           ELSE
              MOVE ZERO             TO 作１－暴風雨回数
           END-IF.
      */金属副子
           MOVE 金属副子加算Ｗ      TO 作１－金属副子加算.
      */金属副子変更↓↓↓/20180611
           MOVE 金属副子回数Ｗ      TO 作１－金属副子回数.
      *     IF 金属大回数Ｗ = 1
      *        MOVE 金属大回数Ｗ     TO 作１－金属大回数
      *     ELSE
      *        MOVE ZERO             TO 作１－金属大回数
      *     END-IF.
      *     IF 金属中回数Ｗ = 1
      *        MOVE 金属中回数Ｗ     TO 作１－金属中回数
      *     ELSE
      *        MOVE ZERO             TO 作１－金属中回数
      *     END-IF.
      *     IF 金属小回数Ｗ = 1
      *        MOVE 金属小回数Ｗ     TO 作１－金属小回数
      *     ELSE
      *        MOVE ZERO             TO 作１－金属小回数
      *     END-IF.
      */金属副子変更↑↑↑/20180611
      */運動後療追加/20180611
           MOVE 運動後療料回数Ｗ    TO 作１－運動後療料回数.
           MOVE 運動後療料Ｗ        TO 作１－運動後療料    .
      *
      */情報提供
           MOVE 情報提供料Ｗ        TO 作１－情報提供料.
           MOVE 情報回数Ｗ          TO 作１－情報提供回数.
      */初検時相談支援
           MOVE 相談支援料Ｗ        TO 作１－相談支援料.
           MOVE 相談支援回数Ｗ      TO 作１－相談支援回数.
      */施術日
           PERFORM VARYING カウンタ FROM 1 BY 1
                   UNTIL   カウンタ > 31
               IF 施術日Ｗ(カウンタ) = SPACE
                  MOVE ZERO               TO 作１－施術日(カウンタ)
               ELSE
                  MOVE 施術日Ｗ(カウンタ) TO 作１－施術日(カウンタ)
               END-IF
           END-PERFORM
      */負傷（部位ごと）
           PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
                   UNTIL   部位ＣＮＴ > 5
               MOVE 負傷区分Ｗ(部位ＣＮＴ)    TO 作１－負傷区分(部位ＣＮＴ)
               MOVE 負傷名Ｗ(部位ＣＮＴ)      TO 作１－負傷名(部位ＣＮＴ)
               MOVE 整復回数Ｗ(部位ＣＮＴ)    TO 作１－整復回数(部位ＣＮＴ)
               MOVE 整復料Ｗ(部位ＣＮＴ)      TO 作１－整復料(部位ＣＮＴ)
               MOVE 負傷年月日Ｗ(部位ＣＮＴ)  TO 作１－負傷年月日(部位ＣＮＴ)
               MOVE 初検年月日Ｗ(部位ＣＮＴ)  TO 作１－初検年月日(部位ＣＮＴ)
               MOVE 開始年月日Ｗ(部位ＣＮＴ)  TO 作１－開始年月日(部位ＣＮＴ)
               MOVE 終了年月日Ｗ(部位ＣＮＴ)  TO 作１－終了年月日(部位ＣＮＴ)
               MOVE 部位実日数Ｗ(部位ＣＮＴ)  TO 作１－部位実日数(部位ＣＮＴ)
               MOVE 転帰区分Ｗ(部位ＣＮＴ)    TO 作１－転帰区分(部位ＣＮＴ)
               MOVE 後療回数Ｗ(部位ＣＮＴ)    TO 作１－後療回数(部位ＣＮＴ)
               MOVE 冷罨法回数Ｗ(部位ＣＮＴ)  TO 作１－冷罨法回数(部位ＣＮＴ)
               MOVE 温罨法回数Ｗ(部位ＣＮＴ)  TO 作１－温罨法回数(部位ＣＮＴ)
               MOVE 電療回数Ｗ(部位ＣＮＴ)    TO 作１－電療回数(部位ＣＮＴ)
               MOVE 多部位区分Ｗ(部位ＣＮＴ)  TO 作１－多部位区分(部位ＣＮＴ)
               MOVE 長期区分Ｗ(部位ＣＮＴ)    TO 作１－長期区分(部位ＣＮＴ)
           END-PERFORM.
      */柔整師番号
           MOVE 柔整師コード数字Ｗ TO 作１－登録記号番号.
      */保険種別
           EVALUATE 受－保険種別
           WHEN 01
               MOVE 4 TO 作１－保険種別
           WHEN 02
           WHEN 06
           WHEN 07
               MOVE 1 TO 作１－保険種別
           WHEN 03
               MOVE 2 TO 作１－保険種別
           WHEN 04
               MOVE 3 TO 作１－保険種別
           WHEN 05
               MOVE 6 TO 作１－保険種別
           WHEN 08
               MOVE 5 TO 作１－保険種別
           WHEN 09
               MOVE 9 TO 作１－保険種別
           WHEN OTHER
               MOVE 9 TO 作１－保険種別
           END-EVALUATE.
      */単併区分
           IF 受－助成種別 = ZERO
               MOVE 1 TO 作１－単併区分
           ELSE
               MOVE 2 TO 作１－単併区分
           END-IF
      */本家区分
           IF 受－保険種別 = 05
               EVALUATE 受－特別区分
               WHEN 1
                   MOVE 8     TO 作１－本家区分
               WHEN 3
                   MOVE ZERO  TO 作１－本家区分
               END-EVALUATE
           ELSE
028984         EVALUATE 受－特別区分
               WHEN 1
               WHEN 2
                   MOVE 8     TO 作１－本家区分
               WHEN 3
                   MOVE ZERO  TO 作１－本家区分
028991         WHEN 6
                   MOVE 4     TO 作１－本家区分
               WHEN OTHER
                   IF 受－本人家族区分 = 1
                       MOVE 2 TO 作１－本家区分
                   ELSE
                       MOVE 6 TO 作１－本家区分
                   END-IF
028999         END-EVALUATE
           END-IF.
      */部位別料金データ
002973**********
002974* １部位 *
002975**********
           IF レセ－長期込小計１ NOT = ZERO
               MOVE ZERO                    TO 作１－逓減開始月日      (1)
               MOVE レセ－後療回数１        TO 作１－後療回数          (1)
               MOVE レセ－後療料１          TO 作１－後療料            (1)
               MOVE レセ－冷罨法回数１      TO 作１－冷罨法回数        (1)
               MOVE レセ－冷罨法料１        TO 作１－冷罨法料          (1)
               MOVE レセ－温罨法回数１      TO 作１－温罨法回数        (1)
               MOVE レセ－温罨法料１        TO 作１－温罨法料          (1)
               MOVE レセ－電療回数１        TO 作１－電療回数          (1)
               MOVE レセ－電療料１          TO 作１－電療料            (1)
               MOVE ZERO                    TO 作１－多部位逓減率      (1)
               COMPUTE 作１－多部位逓減額(1) = レセ－後療料１ +
               レセ－冷罨法料１ + レセ－温罨法料１ + レセ－電療料１
               MOVE レセ－長期逓減率１      TO 作１－長期逓減率        (1)
               MOVE レセ－長期込小計１      TO 作１－部位逓減率別料金計(1)
           END-IF.
002993**********
002994* ２部位 *
002995**********
           IF レセ－長期込小計２ NOT = ZERO
               MOVE ZERO                    TO 作１－逓減開始月日      (2)
               MOVE レセ－後療回数２        TO 作１－後療回数          (2)
               MOVE レセ－後療料２          TO 作１－後療料            (2)
               MOVE レセ－冷罨法回数２      TO 作１－冷罨法回数        (2)
               MOVE レセ－冷罨法料２        TO 作１－冷罨法料          (2)
               MOVE レセ－温罨法回数２      TO 作１－温罨法回数        (2)
               MOVE レセ－温罨法料２        TO 作１－温罨法料          (2)
               MOVE レセ－電療回数２        TO 作１－電療回数          (2)
               MOVE レセ－電療料２          TO 作１－電療料            (2)
               MOVE ZERO                    TO 作１－多部位逓減率      (2)
               COMPUTE 作１－多部位逓減額(2) = レセ－後療料２ +
               レセ－冷罨法料２ + レセ－温罨法料２ + レセ－電療料２
               MOVE レセ－長期逓減率２      TO 作１－長期逓減率        (2)
               MOVE レセ－長期込小計２      TO 作１－部位逓減率別料金計(2)
           END-IF.
003013******************
003014* ３部位／７割 *
003015******************
           IF レセ－長期込小計３８ NOT = ZERO
               MOVE 部位開始和暦年月日Ｗ(3)(4:4) TO 作１－逓減開始月日 (3)
               MOVE レセ－後療回数３８      TO 作１－後療回数          (3)
               MOVE レセ－後療料３８        TO 作１－後療料            (3)
               MOVE レセ－冷罨法回数３８    TO 作１－冷罨法回数        (3)
               MOVE レセ－冷罨法料３８      TO 作１－冷罨法料          (3)
               MOVE レセ－温罨法回数３８    TO 作１－温罨法回数        (3)
               MOVE レセ－温罨法料３８      TO 作１－温罨法料          (3)
               MOVE レセ－電療回数３８      TO 作１－電療回数          (3)
               MOVE レセ－電療料３８        TO 作１－電療料            (3)
               MOVE 70                      TO 作１－多部位逓減率      (3)
               MOVE レセ－多部位込小計３８  TO 作１－多部位逓減額      (3)
               MOVE レセ－長期逓減率３８    TO 作１－長期逓減率        (3)
               MOVE レセ－長期込小計３８    TO 作１－部位逓減率別料金計(3)
           END-IF.
003034******************
003035* ３部位／１０割 *
003036******************
           IF レセ－長期込小計３０ NOT = ZERO
               MOVE レセ－逓減開始月日３０  TO 作１－逓減開始月日      (4)
               MOVE レセ－後療回数３０      TO 作１－後療回数          (4)
               MOVE レセ－後療料３０        TO 作１－後療料            (4)
               MOVE レセ－冷罨法回数３０    TO 作１－冷罨法回数        (4)
               MOVE レセ－冷罨法料３０      TO 作１－冷罨法料          (4)
               MOVE レセ－温罨法回数３０    TO 作１－温罨法回数        (4)
               MOVE レセ－温罨法料３０      TO 作１－温罨法料          (4)
               MOVE レセ－電療回数３０      TO 作１－電療回数          (4)
               MOVE レセ－電療料３０        TO 作１－電療料            (4)
               MOVE ZERO                    TO 作１－多部位逓減率      (4)
               MOVE レセ－小計３０          TO 作１－多部位逓減額      (4)
               MOVE レセ－長期逓減率３０    TO 作１－長期逓減率        (4)
               MOVE レセ－長期込小計３０    TO 作１－部位逓減率別料金計(4)
           END-IF.
003078****************
003079* ４部位／７割 *
003080****************
           IF レセ－長期込小計４８ NOT = ZERO
               MOVE レセ－逓減開始月日４８  TO 作１－逓減開始月日      (5)
               MOVE レセ－後療回数４８      TO 作１－後療回数          (5)
               MOVE レセ－後療料４８        TO 作１－後療料            (5)
               MOVE レセ－冷罨法回数４８    TO 作１－冷罨法回数        (5)
               MOVE レセ－冷罨法料４８      TO 作１－冷罨法料          (5)
               MOVE レセ－温罨法回数４８    TO 作１－温罨法回数        (5)
               MOVE レセ－温罨法料４８      TO 作１－温罨法料          (5)
               MOVE レセ－電療回数４８      TO 作１－電療回数          (5)
               MOVE レセ－電療料４８        TO 作１－電療料            (5)
               MOVE 70                      TO 作１－多部位逓減率      (5)
               MOVE レセ－多部位込小計４８  TO 作１－多部位逓減額      (5)
               MOVE レセ－長期逓減率４８    TO 作１－長期逓減率        (5)
               MOVE レセ－長期込小計４８    TO 作１－部位逓減率別料金計(5)
           END-IF.
003102******************
003103* ４部位／１０割 *
003104******************
           IF レセ－長期込小計４０ NOT = ZERO
               MOVE レセ－逓減開始月日４０  TO 作１－逓減開始月日      (6)
               MOVE レセ－後療回数４０      TO 作１－後療回数          (6)
               MOVE レセ－後療料４０        TO 作１－後療料            (6)
               MOVE レセ－冷罨法回数４０    TO 作１－冷罨法回数        (6)
               MOVE レセ－冷罨法料４０      TO 作１－冷罨法料          (6)
               MOVE レセ－温罨法回数４０    TO 作１－温罨法回数        (6)
               MOVE レセ－温罨法料４０      TO 作１－温罨法料          (6)
               MOVE レセ－電療回数４０      TO 作１－電療回数          (6)
               MOVE レセ－電療料４０        TO 作１－電療料            (6)
               MOVE ZERO                    TO 作１－多部位逓減率      (6)
               MOVE レセ－小計４０          TO 作１－多部位逓減額      (6)
               MOVE レセ－長期逓減率４０    TO 作１－長期逓減率        (6)
               MOVE レセ－長期込小計４０    TO 作１－部位逓減率別料金計(6)
           END-IF.
015010*================================================================*
015020 部位タイプ取得 SECTION.
015030*
012490*/負傷種別０：負傷なし、１：骨折、２：不全骨折、
      */        ３：脱臼、４：捻挫、５：打撲、６：挫傷、７拘縮後療、９：無傷
012500*/柔では１：捻挫、２：打撲、３：挫傷、    ４：脱臼、
012510*/      ５：骨折、６：不骨、７：骨折拘縮、８：不全拘縮、
015040     EVALUATE 負傷種別ＷＴ
015050     WHEN 01
015060         MOVE 4    TO 部位タイプＷ
015070     WHEN 02
015080         MOVE 5    TO 部位タイプＷ
015090     WHEN 03
015100         MOVE 6    TO 部位タイプＷ
015110     WHEN 04
015120         MOVE 3    TO 部位タイプＷ
015130     WHEN 05
015150         MOVE 1    TO 部位タイプＷ
015160     WHEN 06
015180         MOVE 2    TO 部位タイプＷ
015140     WHEN 07
015170     WHEN 08
015180         MOVE 7    TO 部位タイプＷ
015170     WHEN 09
015180         MOVE 9    TO 部位タイプＷ
015190     WHEN OTHER
015200         MOVE ZERO TO 部位タイプＷ
015210     END-EVALUATE.
015220*================================================================*
015230 日付変換８桁 SECTION.
015240******************************************************************
015250* 和暦年月日Ｗを"yyyymmdd"形式に変換するセ作１ション
015260******************************************************************
015270     PERFORM 西暦年取得.
015280     STRING 西暦年Ｗ DELIMITED BY SIZE
015300            月Ｗ     DELIMITED BY SIZE
015320            日Ｗ     DELIMITED BY SIZE
015330       INTO 日付８桁Ｗ
015340     END-STRING.
015350*================================================================*
015360 施術記録Ｆセット SECTION.
      *
015400     MOVE 受－患者番号  TO 施記－患者番号
015410     MOVE 受－枝番      TO 施記－枝番
015420     MOVE 受－施術和暦  TO 施記－施術和暦
015430     MOVE 受－施術年    TO 施記－施術年
015440     MOVE 受－施術月    TO 施記－施術月
015450     MOVE 1             TO 施記－施術日
015460     START 施術記録Ｆ   KEY IS >= 施記－患者コード
015470                                  施記－施術和暦年月日
015480     END-START
015490     IF 状態キー = "00"
015500         MOVE SPACE TO 終了フラグ２
015540         PERFORM 施術記録Ｆ読込
015550         PERFORM UNTIL ( 終了フラグ２     NOT = SPACE          ) OR
015560                       ( 施記－患者コード NOT = 受－患者コード ) OR
015570                       ( 施記－施術和暦   NOT = 受－施術和暦   ) OR
015580                       ( 施記－施術年     NOT = 受－施術年     ) OR
015590                       ( 施記－施術月     NOT = 受－施術月     )
      */初検の判定は診療区分で。１：後療、２：初検/080207
      *             IF 施記－初検料請求区分 = 1
                   IF 施記－診療区分 = 2
                       COMPUTE 初検回数Ｗ = 初検回数Ｗ + 1
                   END-IF
                   EVALUATE 施記－初検加算
                   WHEN 1
                       COMPUTE 初検時間外回数Ｗ = 初検時間外回数Ｗ + 1
                   WHEN 2
                       COMPUTE 初検休日回数Ｗ = 初検休日回数Ｗ + 1
                   WHEN 3
                       COMPUTE 初検深夜回数Ｗ = 初検深夜回数Ｗ + 1
                   END-EVALUATE
      * 初検時相談料/100830
      *             IF 施記－初検時相談料区分 = 1
      *                 COMPUTE 相談支援回数Ｗ = 相談支援回数Ｗ + 1
      *             END-IF
                   IF (施記－診療区分 = 2 ) AND (施記－初検時相談料区分 NOT = 1)
                       COMPUTE 相談支援回数Ｗ = 相談支援回数Ｗ + 1
                   END-IF
                   IF 施記－再検料請求 = 1
                       COMPUTE 再検回数Ｗ = 再検回数Ｗ + 1
                   END-IF
                   EVALUATE 施記－往療加算
                   WHEN 1
                        MOVE 1            TO 夜間回数Ｗ
      *                 COMPUTE 夜間回数Ｗ = 夜間回数Ｗ + 1
                   WHEN 2
                        MOVE 1            TO 難路回数Ｗ
      *                 COMPUTE 難路回数Ｗ = 難路回数Ｗ + 1
                   WHEN 3
                        MOVE 1            TO 暴風雨回数Ｗ
      *                 COMPUTE 暴風雨回数Ｗ = 暴風雨回数Ｗ + 1
                   END-EVALUATE
016000             PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
016010                     UNTIL   部位ＣＮＴ > 負－部位数
      */金属副子変更↓↓↓/20180611
      *                 EVALUATE 施記－金属副子区分(部位ＣＮＴ)
      *                 WHEN 1
      *                     MOVE 1            TO 金属大回数Ｗ
      **                     COMPUTE 金属大回数Ｗ = 金属大回数Ｗ + 1
      *                 WHEN 2
      *                     MOVE 1            TO 金属中回数Ｗ
      **                     COMPUTE 金属中回数Ｗ = 金属中回数Ｗ + 1
      *                 WHEN 3
      *                     MOVE 1            TO 金属小回数Ｗ
      **                     COMPUTE 金属小回数Ｗ = 金属小回数Ｗ + 1
      *                 END-EVALUATE
      */金属副子変更↑↑↑/20180611
                       IF 施記－情報提供区分(部位ＣＮＴ) = 1
                           COMPUTE 情報回数Ｗ = 情報回数Ｗ + 1
                       END-IF
                       IF 施記－整復施療区分(部位ＣＮＴ) = 1
                           COMPUTE 整復回数Ｗ(部位ＣＮＴ) = 整復回数Ｗ(部位ＣＮＴ) + 1
                       END-IF
                   END-PERFORM
                   MOVE 1      TO 施術日Ｗ(施記－施術日)
015820             PERFORM 施術記録Ｆ読込
015830         END-PERFORM
015840     END-IF.
015850*================================================================*
015860 負傷データＦセット SECTION.
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
016000         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
016010                 UNTIL   部位ＣＮＴ > 負－部位数
012520             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別ＷＴ
012530             PERFORM 部位タイプ取得
012540             MOVE 部位タイプＷ    TO 負傷区分Ｗ(部位ＣＮＴ)
027210             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別Ｗ(部位ＣＮＴ)
027220             MOVE 負－部位(部位ＣＮＴ)     TO 部位Ｗ(部位ＣＮＴ)
027230             MOVE 負－左右区分(部位ＣＮＴ) TO 左右区分Ｗ(部位ＣＮＴ)
027240             MOVE 負－負傷位置番号(部位ＣＮＴ)
027250                                           TO 負傷位置番号Ｗ(部位ＣＮＴ)
027260*********************************************
027270* 注）全柔...負傷種別＋部位にて加工して格納 *
027280*********************************************
027290* 負傷種別
027300             MOVE SPACE                     TO 負傷名称Ｗ
027310             MOVE 03                        TO 名－区分コード
027320             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
027330             READ 名称マスタ
027340             INVALID KEY
027350                 MOVE SPACE        TO 負傷名称Ｗ
027360             NOT INVALID KEY
027370                 MOVE 名－正式名称 TO 負傷名称Ｗ
027380             END-READ
027390* 部位
006490             STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
009980                    負傷名称Ｗ                    DELIMITED BY SPACE
006500                    レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
006520               INTO 負傷名ＷＰ(部位ＣＮＴ)
006570             END-STRING
                   MOVE 負傷名ＷＰ(部位ＣＮＴ) TO 負傷名Ｗ(部位ＣＮＴ)
012720             MOVE 負－負傷和暦年月日(部位ＣＮＴ) TO 和暦年月日Ｗ
012730             PERFORM 日付変換８桁
012740             MOVE 日付８桁Ｗ          TO 負傷年月日Ｗ(部位ＣＮＴ)
012720             MOVE 負－開始和暦年月日(部位ＣＮＴ) TO 和暦年月日Ｗ
012730             PERFORM 日付変換８桁
012740             MOVE 日付８桁Ｗ          TO 初検年月日Ｗ(部位ＣＮＴ)
      *
028040             EVALUATE 負－転帰区分(部位ＣＮＴ)
028050             WHEN 1
028060             WHEN 2
028060             WHEN 5
028070                 MOVE 1               TO 転帰区分Ｗ(部位ＣＮＴ)
028080             WHEN 3
028070                 MOVE 2               TO 転帰区分Ｗ(部位ＣＮＴ)
028100             WHEN 4
028070                 MOVE 3               TO 転帰区分Ｗ(部位ＣＮＴ)
028100             WHEN OTHER
028070                 MOVE ZERO            TO 転帰区分Ｗ(部位ＣＮＴ)
028120             END-EVALUATE
013140             PERFORM 部位開始終了日取得
012720             MOVE 部位開始和暦年月日Ｗ(部位ＣＮＴ) TO 和暦年月日Ｗ
012730             PERFORM 日付変換８桁
012740             MOVE 日付８桁Ｗ          TO 開始年月日Ｗ(部位ＣＮＴ)
012720             MOVE 部位終了和暦年月日Ｗ(部位ＣＮＴ) TO 和暦年月日Ｗ
012730             PERFORM 日付変換８桁
012740             MOVE 日付８桁Ｗ          TO 終了年月日Ｗ(部位ＣＮＴ)
                   MOVE レセ－部位実日数(部位ＣＮＴ) TO 部位実日数Ｗ(部位ＣＮＴ)
016130         END-PERFORM
029420* 負傷原因印刷区分
029430         MOVE 負－レセ負傷原因印刷区分 TO レセ負傷原因印刷区分Ｗ
027880         MOVE 負－レセ長期理由印刷区分 TO レセ長期理由印刷区分Ｗ
016140     END-READ.
016150*================================================================*
016160 データチェック SECTION.
016170*
016180     MOVE SPACE  TO 実行キーＷ.
019520* *****************************************************************
019530* * レセプトＦの請求対象区分 = 0 の場合データ作成対象としない *
019540* *****************************************************************
019640     IF レセ－請求対象区分 NOT = ZERO
004090         MOVE レセ－施術和暦  TO 受－施術和暦
004100         MOVE レセ－施術年    TO 受－施術年
004110         MOVE レセ－施術月    TO 受－施術月
004120         MOVE レセ－患者番号  TO 受－患者番号
004130         MOVE レセ－枝番      TO 受－枝番
               READ 受診者情報Ｆ
               NOT INVALID KEY
019880            MOVE "YES"  TO 実行キーＷ
               END-READ
019900     ELSE
019910         MOVE SPACE  TO 実行キーＷ
019950     END-IF.
016640*
016650*================================================================*
016660 施術記録Ｆ読込 SECTION.
016670*
016680     READ 施術記録Ｆ NEXT
016690     AT END
016700         MOVE "YES" TO 終了フラグ２
016710     END-READ.
025540*================================================================*
025550 部位開始終了日取得 SECTION.
025560*
025570*     INITIALIZE 負傷情報Ｗ.
025580     MOVE  SPACE  TO  初日再検フラグ.
025590     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 負－部位数
025600*
025610         IF 負－転帰区分(カウンタ) = 9
025620             MOVE 99                       TO 部位終了年Ｗ(カウンタ)
025630             MOVE 99                       TO 部位終了月Ｗ(カウンタ)
025640             MOVE 99                       TO 部位終了日Ｗ(カウンタ)
025650         ELSE
025660             MOVE 負－終了和暦(カウンタ) TO 部位終了和暦Ｗ(カウンタ)
025670             MOVE 負－終了年  (カウンタ) TO 部位終了年Ｗ  (カウンタ)
025680             MOVE 負－終了月  (カウンタ) TO 部位終了月Ｗ  (カウンタ)
025690             MOVE 負－終了日  (カウンタ) TO 部位終了日Ｗ  (カウンタ)
025700         END-IF
025710*
025720         IF ( 受－施術和暦年月 = 負－開始和暦年月(カウンタ))
025730             MOVE 受－患者番号             TO 施記－患者番号
025740             MOVE 受－枝番                 TO 施記－枝番
025750             MOVE 負－開始和暦(カウンタ) TO 施記－施術和暦 部位開始和暦Ｗ(カウンタ)
025760             MOVE 負－開始年  (カウンタ) TO 施記－施術年   部位開始年Ｗ  (カウンタ)
025770             MOVE 負－開始月  (カウンタ) TO 施記－施術月   部位開始月Ｗ  (カウンタ)
025780             MOVE 負－開始日  (カウンタ) TO 施記－施術日   部位開始日Ｗ  (カウンタ)
025790         ELSE
025800             MOVE 受－患者番号             TO 施記－患者番号
025810             MOVE 受－枝番                 TO 施記－枝番
025820             MOVE 受－施術和暦             TO 施記－施術和暦
025830             MOVE 受－施術年               TO 施記－施術年
025840             MOVE 受－施術月               TO 施記－施術月
025850             MOVE ZERO                     TO 施記－施術日
025860         END-IF
025870         START 施術記録Ｆ   KEY IS >= 施記－患者コード
025880                                      施記－施術和暦年月日
025890         END-START
025900         IF 状態キー = "00"
025920             MOVE ZERO  TO 終了和暦ＷＴ
025930             MOVE ZERO  TO 終了年ＷＴ
025940             MOVE ZERO  TO 終了月ＷＴ
025950             MOVE ZERO  TO 終了日ＷＴ
025960             MOVE SPACE TO 終了フラグ２
025970             PERFORM 施術記録Ｆ読込
025980             IF  ( 終了フラグ２      = SPACE   ) AND
025990                 ( 施記－患者コード  = 受－患者コード ) AND
026000                 ( 施記－施術和暦    = 受－施術和暦   ) AND
026010                 ( 施記－施術年      = 受－施術年     ) AND
026020                 ( 施記－施術月      = 受－施術月     ) 
026030** 当月施術開始日が再検かどうか判定
026040                 IF   施記－再検料請求 = 1
026050                      MOVE "YES"  TO  初日再検フラグ
026060                 END-IF
026070**
026080             END-IF
026090*        **************
026100*        * 開始年月日 *
026110*        **************
026120             IF ( 受－施術和暦年月 NOT = 負－開始和暦年月(カウンタ))
026130                 MOVE 施記－施術和暦 TO 部位開始和暦Ｗ(カウンタ)
026140                 MOVE 施記－施術年   TO 部位開始年Ｗ  (カウンタ)
026150                 MOVE 施記－施術月   TO 部位開始月Ｗ  (カウンタ)
026160                 MOVE 施記－施術日   TO 部位開始日Ｗ  (カウンタ)
026170             END-IF
026180             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
026190                           ( 施記－患者コード NOT = 受－患者コード   ) OR
026200                           ( 施記－施術和暦   NOT = 受－施術和暦     ) OR
026210                           ( 施記－施術年     NOT = 受－施術年       ) OR
026220                           ( 施記－施術月     NOT = 受－施術月       ) OR
026230                           ( 施記－施術日         > 部位終了日Ｗ(カウンタ))
026240*               **********
026250*               * 実日数 *
026260*               **********
026280                MOVE 施記－施術和暦             TO 終了和暦ＷＴ
026290                MOVE 施記－施術年               TO 終了年ＷＴ
026300                MOVE 施記－施術月               TO 終了月ＷＴ
026310                MOVE 施記－施術日               TO 終了日ＷＴ
026320                PERFORM 施術記録Ｆ読込
026330            END-PERFORM
026340        END-IF
026350*       **************************
026360*       * 継続：終了年月日セット *
026370*       **************************
026380        IF 負－転帰区分(カウンタ) = 9
026390            MOVE 終了和暦ＷＴ  TO 部位終了和暦Ｗ(カウンタ)
026400            MOVE 終了年ＷＴ    TO 部位終了年Ｗ  (カウンタ)
026410            MOVE 終了月ＷＴ    TO 部位終了月Ｗ  (カウンタ)
026420            MOVE 終了日ＷＴ    TO 部位終了日Ｗ  (カウンタ)
026430        END-IF
026490     END-PERFORM.
026540*
008340*================================================================*
008341 料金情報取得 SECTION.
008342*
008343********************
008344* 料金データセット *
008345************************************************************
008346* ● 後療回数......当月の初来院日の施記－施術年をセット    *
008347* ● 冷罨法回数....当月の初来院日の施記－施術月をセット    *
008348* ● 御罨法回数....当月の初来院日の施記－施術日をセット    *
008349* ● 電療回数......終了年Ｗ ※転帰 NOT = 9の場合のみ       *
008350* ● 算定区分......２回の場合は 2 を入れる　　　　　　　　 *
008351* ● 初検加算料....初検加算料Ｗにセット                    *
008352* ● 往療回数......往療回数Ｗにセット                      *
008353* ● 往療料........往療料Ｗにセット     　                 *
008354* ● 往療加算料 ...往療加算料Ｗにセット                    *
008355* ● 金属副子加算料..金属副子加算料Ｗにセット              *
008356* ● 費用額 .......費用額Ｗにセット                        *
008357* ● 一部負担金 ...一部負担金Ｗにセット                    *
008358* ● 請求額........請求額Ｗにセット                        *
008359* ● 受給者負担額..受給者負担額Ｗにセット                  *
008360* ● 助成請求金額..助成請求金額Ｗにセット                  *
008361************************************************************
025620     MOVE レセ－初検料        TO 初検料Ｗ.
025730     MOVE レセ－初検加算料    TO 初検加算Ｗ.
025740     MOVE レセ－再検料        TO 再検料Ｗ.
028150* 新規/継続 チェック
033380     EVALUATE レセ－レセ請求区分
           WHEN 1
033390         MOVE 1               TO 新規Ｗ
033410         MOVE ZERO            TO 継続Ｗ
           WHEN 2
033390         MOVE ZERO            TO 新規Ｗ
033410         MOVE 1               TO 継続Ｗ
033400     WHEN 3
033390         MOVE 1               TO 新規Ｗ
033410         MOVE 1               TO 継続Ｗ
033420     END-EVALUATE.
      *
025770     MOVE レセ－往療料         TO  往療料Ｗ.
021580     COMPUTE 往療距離Ｗ = レセ－往療距離 * 10.
021590     MOVE レセ－往療回数       TO  往療回数Ｗ.
025780     MOVE レセ－往療加算料     TO  往療加算Ｗ.
025870     MOVE レセ－金属副子加算料 TO  金属副子加算Ｗ.
025990     MOVE レセ－施術情報提供料 TO  情報提供料Ｗ.
025770     MOVE レセ－初検時相談料   TO  相談支援料Ｗ.
026050     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
026060             UNTIL ( 部位ＣＮＴ > 5 )
026070         MOVE レセ－初回処置料(部位ＣＮＴ) TO 整復料Ｗ(部位ＣＮＴ)
           END-PERFORM.
      *
022190*     MOVE レセ－後療回数１    TO 後療回数Ｗ(1).
022210*     MOVE レセ－冷罨法回数１  TO 冷罨法回数Ｗ(1).
022230*     MOVE レセ－温罨法回数１  TO 温罨法回数Ｗ(1).
022250*     MOVE レセ－電療回数１    TO 電療回数Ｗ(1).
           
022290     IF レセ－長期逓減率１ NOT = ZERO
               MOVE 1               TO 長期区分Ｗ(1)
           END-IF.
022190*     MOVE レセ－後療回数２    TO 後療回数Ｗ(2).
022210*     MOVE レセ－冷罨法回数２  TO 冷罨法回数Ｗ(2).
022230*     MOVE レセ－温罨法回数２  TO 温罨法回数Ｗ(2).
022250*     MOVE レセ－電療回数２    TO 電療回数Ｗ(2).
022290     IF レセ－長期逓減率２ NOT = ZERO
               MOVE 1               TO 長期区分Ｗ(2)
           END-IF.
      *     COMPUTE 後療回数Ｗ(3) = レセ－後療回数３８ + レセ－後療回数３０.
      *     COMPUTE 冷罨法回数Ｗ(3) = レセ－冷罨法回数３８ + レセ－冷罨法回数３０.
      *     COMPUTE 温罨法回数Ｗ(3) = レセ－温罨法回数３８ + レセ－温罨法回数３０.
      *     COMPUTE 電療回数Ｗ(3) = レセ－電療回数３８ + レセ－電療回数３０.
           IF レセ－後療料３８ NOT = ZERO
               MOVE 1               TO 多部位区分Ｗ(3)
           END-IF.
022290     IF ( レセ－長期逓減率３８ NOT = ZERO ) OR
              ( レセ－長期逓減率３０ NOT = ZERO )
               MOVE 1               TO 長期区分Ｗ(3)
           END-IF.
      *     COMPUTE 後療回数Ｗ(4) = レセ－後療回数４５ + レセ－後療回数４８ +
      *                             レセ－後療回数４０.
      *     COMPUTE 冷罨法回数Ｗ(4) = レセ－冷罨法回数４５ + レセ－冷罨法回数４８ +
      *                               レセ－冷罨法回数４０.
      *     COMPUTE 温罨法回数Ｗ(4) = レセ－温罨法回数４５ + レセ－温罨法回数４８ +
      *                               レセ－温罨法回数４０.
      *     COMPUTE 電療回数Ｗ(4) = レセ－電療回数４５ + レセ－電療回数４８ +
      *                             レセ－電療回数４０.
           IF (レセ－後療料４８ NOT = ZERO) OR
              (レセ－後療料４５ NOT = ZERO)
               MOVE 1               TO 多部位区分Ｗ(4)
           END-IF.
022290     IF ( レセ－長期逓減率４８ NOT = ZERO ) OR
              ( レセ－長期逓減率４５ NOT = ZERO ) OR
              ( レセ－長期逓減率４０ NOT = ZERO )
               MOVE 1               TO 長期区分Ｗ(4)
           END-IF.
      *     COMPUTE 後療回数Ｗ(5) = レセ－後療回数５２ + レセ－後療回数５５ +
      *                             レセ－後療回数５８ + レセ－後療回数５０.
      *     COMPUTE 冷罨法回数Ｗ(5) = レセ－冷罨法回数５２ + レセ－冷罨法回数５５ +
      *                               レセ－冷罨法回数５８ + レセ－冷罨法回数５０.
      *     COMPUTE 温罨法回数Ｗ(5) = レセ－温罨法回数５２ + レセ－温罨法回数５５ +
      *                               レセ－温罨法回数５８ + レセ－温罨法回数５０.
      *     COMPUTE 電療回数Ｗ(5) = レセ－電療回数５２ + レセ－電療回数５５ +
      *                             レセ－電療回数５８ + レセ－電療回数５０.
           IF (レセ－後療料５８ NOT = ZERO) OR
              (レセ－後療料５５ NOT = ZERO)
               MOVE 1               TO 多部位区分Ｗ(5)
           END-IF.
022290     IF ( レセ－長期逓減率５８ NOT = ZERO ) OR
              ( レセ－長期逓減率５５ NOT = ZERO ) OR
              ( レセ－長期逓減率５２ NOT = ZERO ) OR
              ( レセ－長期逓減率５０ NOT = ZERO )
               MOVE 1               TO 長期区分Ｗ(5)
           END-IF.
008375*
      */金属副子変更/20180611
           COMPUTE 金属副子回数Ｗ = レセ－大 + レセ－中 + レセ－小.
           IF 金属副子回数Ｗ > 9
               MOVE 9 TO 金属副子回数Ｗ
           END-IF.
      */運動後療追加/20180611
           MOVE レセ－運動後療回数           TO 運動後療料回数Ｗ.
           MOVE レセ－運動後療料             TO 運動後療料Ｗ.
028590*================================================================*
028600 作業ファイル１読込 SECTION.
028610*
028620     READ 作業ファイル１ NEXT
028630     AT END
028640         MOVE "YES" TO 終了フラグ
028650     END-READ.
026660*================================================================*
026670 負傷原因取得 SECTION.
026680*
026690********************************************************************
026700*  負傷原因コードが同じものは、1行にまとめて印字する。
026710*  例: ①② 家で転んだ.
026720*     負傷原因コードが同じものをまとめ、テーブルにセット
026730*     (ただし、部位を飛んで同じものは、2行になる)
026740********************************************************************
026750     MOVE  ZERO   TO  カウンタ カウンタ２.
026760     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
026770             UNTIL ( 部位ＣＮＴ > 負－部位数 )
026780*
026800        IF ( 負－負傷連番(部位ＣＮＴ)      NOT = ZERO )
026810*
026820           IF カウンタ = ZERO
026830               MOVE 1   TO  カウンタ カウンタ２
026840               MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
026850               MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
026860               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
026870           ELSE
026880              IF ( 負－負傷患者番号(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
026890                 ( 負－負傷連番(部位ＣＮＴ)      = 負傷連番ＣＷ     )
026900                 COMPUTE カウンタ２ = カウンタ２  +  1
026910                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
026920              ELSE
026930                 COMPUTE カウンタ = カウンタ  +  1
026940                 MOVE 1   TO  カウンタ２
026950                 MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
026960                 MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
026970                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
026980              END-IF
026990           END-IF
027000        END-IF
027010     END-PERFORM.
027020**************************************************************************
027030*  負傷原因マスタより文章取得
027040**************************************************************************
027050     MOVE  ZERO   TO  カウンタ カウンタ２.
027060     PERFORM VARYING カウンタ FROM 1 BY 1
027070             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
027080** 健保は 区分 01
027090         MOVE 01                        TO 負原－区分コード
027100         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
027110         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
027120         READ 負傷原因Ｆ
027130         NOT INVALID KEY
027140             INITIALIZE 負傷原因ＷＴ
027150             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
027160             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
027170             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
027180             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
027190             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
027200             PERFORM VARYING カウンタ２ FROM 1 BY 1
027210                     UNTIL ( カウンタ２ > 9 )  OR 
027220                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
027230                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
027240                WHEN 1
027250                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
027260                WHEN 2
027270                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
027280                WHEN 3
027290                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
027300                WHEN 4
027310                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
027320                WHEN 5
027330                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
027320                WHEN 6
027330                   MOVE "⑥"  TO  負傷原因ナンバーＷ１(カウンタ２)
027320                WHEN 7
027330                   MOVE "⑦"  TO  負傷原因ナンバーＷ１(カウンタ２)
027340                WHEN OTHER
027350                   CONTINUE
027360                END-EVALUATE
027370             END-PERFORM
027380*
027390             IF 負原－負傷原因入力区分 = 1
027400                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
027410                        負傷原因１ＷＴ  DELIMITED BY SIZE
027420                        負傷原因２ＷＴ  DELIMITED BY SIZE
027430                        負傷原因３ＷＴ  DELIMITED BY SIZE
027440                        負傷原因４ＷＴ  DELIMITED BY SIZE
027450                        負傷原因５ＷＴ  DELIMITED BY SIZE
027460                        INTO 負傷原因内容合成Ｗ(カウンタ)
027470                 END-STRING
027480             ELSE
005946                 INSPECT 負傷原因ＷＴ REPLACING ALL 全角空白 BY 半角空白
                       MOVE SPACE TO 文字１Ｗ 文字２Ｗ
                       MOVE 負傷原因ナンバーＮＷ TO 文字１Ｗ
                       MOVE 負傷原因１ＷＴ       TO 文字２Ｗ
                       CALL プログラム名Ｗ WITH C LINKAGE
                            USING BY REFERENCE 文字１Ｗ
                                  BY REFERENCE 文字２Ｗ
                       MOVE 負傷原因２ＷＴ       TO 文字２Ｗ
                       CALL プログラム名Ｗ WITH C LINKAGE
                            USING BY REFERENCE 文字１Ｗ
                                  BY REFERENCE 文字２Ｗ
                       MOVE 負傷原因３ＷＴ       TO 文字２Ｗ
                       CALL プログラム名Ｗ WITH C LINKAGE
                            USING BY REFERENCE 文字１Ｗ
                                  BY REFERENCE 文字２Ｗ
                       MOVE 負傷原因４ＷＴ       TO 文字２Ｗ
                       CALL プログラム名Ｗ WITH C LINKAGE
                            USING BY REFERENCE 文字１Ｗ
                                  BY REFERENCE 文字２Ｗ
                       MOVE 負傷原因５ＷＴ       TO 文字２Ｗ
                       CALL プログラム名Ｗ WITH C LINKAGE
                            USING BY REFERENCE 文字１Ｗ
                                  BY REFERENCE 文字２Ｗ
                        MOVE 文字１Ｗ TO 負傷原因内容合成Ｗ(カウンタ)
027570             END-IF
027580*
027590         END-READ
027600     END-PERFORM.
027610*
027620     PERFORM 負傷原因セット.
027630*
027640*================================================================*
027650 負傷原因セット SECTION.
027660*
027670**************************************************************************
027680*  文章が1行を超える時は、複数行に分解する。
027690**************************************************************************
027700     MOVE  ZERO   TO  カウンタ カウンタ２.
027710     PERFORM VARYING カウンタ FROM 1 BY 1
027720             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
027730*
027740          INITIALIZE 負傷原因内容分解Ｗ
027750          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解Ｗ
027760          IF  負傷原因内容１Ｗ  NOT = SPACE
027770              COMPUTE カウンタ２ = カウンタ２  +  1
027780              MOVE 負傷原因内容１Ｗ  TO 負傷原因Ｗ(カウンタ２)
027790          END-IF
027800          IF  負傷原因内容２Ｗ  NOT = SPACE
027810              COMPUTE カウンタ２ = カウンタ２  +  1
027820              MOVE 負傷原因内容２Ｗ  TO 負傷原因Ｗ(カウンタ２)
027830          END-IF
027840          IF  負傷原因内容３Ｗ  NOT = SPACE
027850              COMPUTE カウンタ２ = カウンタ２  +  1
027860              MOVE 負傷原因内容３Ｗ  TO 負傷原因Ｗ(カウンタ２)
027870          END-IF
027760          IF  負傷原因内容４Ｗ  NOT = SPACE
027770              COMPUTE カウンタ２ = カウンタ２  +  1
027780              MOVE 負傷原因内容４Ｗ  TO 負傷原因Ｗ(カウンタ２)
027790          END-IF
027800          IF  負傷原因内容５Ｗ  NOT = SPACE
027810              COMPUTE カウンタ２ = カウンタ２  +  1
027820              MOVE 負傷原因内容５Ｗ  TO 負傷原因Ｗ(カウンタ２)
027830          END-IF
027840          IF  負傷原因内容６Ｗ  NOT = SPACE
027850              COMPUTE カウンタ２ = カウンタ２  +  1
027860              MOVE 負傷原因内容６Ｗ  TO 負傷原因Ｗ(カウンタ２)
027870          END-IF
027880*
027890     END-PERFORM.
027900*
014400*================================================================*
014410 枝番変換 SECTION.
014420*
014430     EVALUATE 受－枝番
014440     WHEN SPACE
014450         MOVE 1  TO 作１－枝番
014460     WHEN "A"
014470         MOVE 2  TO 作１－枝番
014480     WHEN "B"
014490         MOVE 3  TO 作１－枝番
014500     WHEN "C"
014510         MOVE 4  TO 作１－枝番
014520     WHEN "D"
014530         MOVE 5  TO 作１－枝番
014540     WHEN "E"
014550         MOVE 6  TO 作１－枝番
014560     WHEN "F"
014570         MOVE 7  TO 作１－枝番
014580     WHEN "G"
014590         MOVE 8  TO 作１－枝番
014600     WHEN "H"
014610         MOVE 9  TO 作１－枝番
014620*     WHEN "I"
014630*         MOVE 10 TO 作１－枝番
014640*     WHEN "J"
014650*         MOVE 11 TO 作１－枝番
014660*     WHEN "K"
014670*         MOVE 12 TO 作１－枝番
014680*     WHEN "L"
014690*         MOVE 13 TO 作１－枝番
014700*     WHEN "M"
014710*         MOVE 14 TO 作１－枝番
014720*     WHEN "N"
014730*         MOVE 15 TO 作１－枝番
014740*     WHEN "O"
014750*         MOVE 16 TO 作１－枝番
014760*     WHEN "P"
014770*         MOVE 17 TO 作１－枝番
014780*     WHEN "Q"
014790*         MOVE 18 TO 作１－枝番
014800*     WHEN "R"
014810*         MOVE 19 TO 作１－枝番
014820*     WHEN "S"
014830*         MOVE 20 TO 作１－枝番
014840*     WHEN "T"
014850*         MOVE 21 TO 作１－枝番
014860*     WHEN "U"
014870*         MOVE 22 TO 作１－枝番
014880*     WHEN "V"
014890*         MOVE 23 TO 作１－枝番
014900*     WHEN "W"
014910*         MOVE 24 TO 作１－枝番
014920*     WHEN "X"
014930*         MOVE 25 TO 作１－枝番
014940*     WHEN "Y"
014950*         MOVE 26 TO 作１－枝番
014960*     WHEN "Z"
014970*         MOVE 27 TO 作１－枝番
014980     WHEN OTHER
014990         MOVE 1  TO 作１－枝番
015000     END-EVALUATE.
028660*================================================================*
       後療罨法電料回数セット SECTION.
      *
016220     MOVE 受－施術和暦   TO 負－施術和暦.
016230     MOVE 受－施術年     TO 負－施術年.
016240     MOVE 受－施術月     TO 負－施術月.
016250     MOVE 受－患者番号   TO 負－患者番号.
016260     MOVE 受－枝番       TO 負－枝番.
016270     READ 負傷データＦ
016300     NOT INVALID KEY
               MOVE 受－患者番号  TO 施記－患者番号
               MOVE 受－枝番      TO 施記－枝番
               MOVE 受－施術和暦  TO 施記－施術和暦
               MOVE 受－施術年    TO 施記－施術年
               MOVE 受－施術月    TO 施記－施術月
               MOVE ZERO          TO 施記－施術日
               START 施術記録Ｆ   KEY IS >= 施記－患者コード
                                            施記－施術和暦年月日
               END-START
               IF 状態キー = "00"
                   MOVE SPACE TO 終了フラグ２
                   PERFORM 施術記録Ｆ読込
                   PERFORM UNTIL (受－患者コード   NOT = 施記－患者コード  ) OR
                                 (受－施術和暦年月 NOT = 施記－施術和暦年月) OR
                                 (終了フラグ２ = "YES")
                       PERFORM VARYING カウンタ FROM 1 BY 1 
                         UNTIL (カウンタ > 5 ) OR (カウンタ > 負－部位数)
                           IF 施記－後療料請求区分(カウンタ) = 1
                               COMPUTE 後療回数Ｗ  (カウンタ) = 後療回数Ｗ  (カウンタ) + 1
                           END-IF
                           IF 施記－罨法区分(カウンタ) = 1
                               COMPUTE 冷罨法回数Ｗ(カウンタ) = 冷罨法回数Ｗ(カウンタ) + 1
                           END-IF
                           IF 施記－罨法区分(カウンタ) = 2
                               COMPUTE 温罨法回数Ｗ(カウンタ) = 温罨法回数Ｗ(カウンタ) + 1
                           END-IF
                           IF 施記－電療区分(カウンタ) = 1
                               COMPUTE 電療回数Ｗ  (カウンタ) = 電療回数Ｗ  (カウンタ) + 1
                           END-IF
                       END-PERFORM
                       PERFORM 施術記録Ｆ読込
                   END-PERFORM
               END-IF
           END-READ.
028660*================================================================*
       翌月セット SECTION.
      *
      *請求和暦年月の翌月を西暦でセットする
           MOVE 連入－請求和暦 TO 元－元号区分.
008156     READ 元号マスタ
           INVALID KEY
027940         MOVE NC"元号"  TO ファイル名
027950         PERFORM エラー表示Ｒ
008157     NOT INVALID KEY
               COMPUTE 翌月西暦年Ｗ = 元－開始西暦年 + 連入－請求年 - 1
               IF 連入－請求月 = 12
                   COMPUTE 翌月西暦年Ｗ = 翌月西暦年Ｗ + 1
                   MOVE 1 TO 翌月西暦月Ｗ
               ELSE
                   COMPUTE 翌月西暦月Ｗ = 連入－請求月 + 1
               END-IF
008161     END-READ.
028660*================================================================*
044961 負傷原因印刷対象判定処理 SECTION.
044963*------------------------------------------------------------------------------------*
044964* 制御マスタの「負傷原因印刷区分」が 3 （３部位以上印刷）の時、３部位以上か判定して、
044965* その時のみ、負傷原因を印刷する。
044966*------------------------------------------------------------------------------------*
044967*
044979     MOVE  SPACE TO  連レセ負原印－キー.
044980     INITIALIZE      連レセ負原印－キー.
044981     MOVE 受－施術和暦  TO  連レセ負原印－施術和暦.
044982     MOVE 受－施術年    TO  連レセ負原印－施術年.
044983     MOVE 受－施術月    TO  連レセ負原印－施術月.
044984     MOVE 受－患者番号  TO  連レセ負原印－患者番号.
044985     MOVE 受－枝番      TO  連レセ負原印－枝番.
044986     CALL   "RECEHUGE".
044987     CANCEL "RECEHUGE".
044989*
044990     IF 連レセ負原印－対象フラグ = "YES"
018380*      / 負傷原因用WORKクリアー /
018390         INITIALIZE 負傷原因ＷＴ
018400         INITIALIZE 負傷患者番号ＣＷ
018410         INITIALIZE 負傷連番ＣＷ
018420         INITIALIZE 負傷原因ＴＢＬ
018430         INITIALIZE 負傷原因内容Ｗ
044991         PERFORM 負傷原因取得
044992     END-IF.
044993*
042830*================================================================*
028670******************************************************************
028680 END PROGRAM YAS1011.
028690******************************************************************
