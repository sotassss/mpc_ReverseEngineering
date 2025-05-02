000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YIW1011.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*      アイワ 会提出フロッピー作成【FPD書込】
000100* 請求年月Ver.
000110*      MED = YIW580
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2022-11-11
000140 DATE-COMPILED.          2022-11-11
000150*----------------------------------------------------------------*
      */統合医療よりコピー
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
000330     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000340                             ORGANIZATION             IS  INDEXED
000350                             ACCESS MODE              IS  DYNAMIC
000360                             RECORD KEY               IS  名－区分コード
000370                                                          名－名称コード
000380                             FILE STATUS              IS  状態キー
000390                             LOCK        MODE         IS  AUTOMATIC.
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
000540     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
000550                             ORGANIZATION             IS  INDEXED
000560                             ACCESS MODE              IS  DYNAMIC
000570                             RECORD KEY               IS  負－施術和暦年月
000580                                                          負－患者コード
000590                             ALTERNATE RECORD KEY     IS  負－患者コード
000600                                                          負－施術和暦年月
000610                             FILE STATUS              IS  状態キー
000620                             LOCK        MODE         IS  AUTOMATIC.
001270     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
001280                             ORGANIZATION             IS  INDEXED
001290                             ACCESS MODE              IS  DYNAMIC
001300                             RECORD KEY               IS  負原－区分コード
001310                                                          負原－負傷原因コード
001320                             FILE STATUS              IS  状態キー
001330                             LOCK        MODE         IS  AUTOMATIC.
000293     SELECT  長期継続者Ｆ    ASSIGN      TO        CHOKEIL
000294                             ORGANIZATION             IS INDEXED
000295                             ACCESS MODE              IS DYNAMIC
000296                             RECORD KEY               IS 長継－施術和暦年月
000297                                                         長継－患者コード
000298                             ALTERNATE RECORD KEY     IS 長継－患者コード
000299                                                         長継－施術和暦年月
000300                             FILE STATUS              IS 状態キー
000301                             LOCK      MODE           IS AUTOMATIC.
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
000840     SELECT  保険者マスタ    ASSIGN      TO        HOKENSL
000850                             ORGANIZATION             IS  INDEXED
000860                             ACCESS MODE              IS  DYNAMIC
000870                             RECORD KEY               IS  保－保険種別
000880                                                          保－保険者番号
000890                             ALTERNATE RECORD KEY     IS  保－保険種別
000900                                                          保－保険者名称
000910                                                          保－保険者番号
000920                             FILE STATUS              IS  状態キー
000930                             LOCK        MODE         IS  AUTOMATIC.
000940     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000950                             ORGANIZATION             IS  INDEXED
000960                             ACCESS MODE              IS  DYNAMIC
000970                             RECORD KEY               IS  制－制御区分
000980                             FILE STATUS              IS  状態キー
000990                             LOCK        MODE         IS  AUTOMATIC.
001000     SELECT  負担率マスタ    ASSIGN      TO        HUTANRIL
001010                             ORGANIZATION             IS  INDEXED
001020                             ACCESS MODE              IS  DYNAMIC
001030                             RECORD KEY               IS  負率－保険種別
001040                                                          負率－開始和暦年月
001050                             FILE STATUS              IS  状態キー
001060                             LOCK        MODE         IS  AUTOMATIC.
000098     SELECT  保険者特別負担マスタ   ASSIGN      TO        HOKENTKL
000099                             ORGANIZATION             IS  INDEXED
000100                             ACCESS MODE              IS  DYNAMIC
000101                             RECORD KEY               IS  保特－保険種別
000102                                                          保特－保険者番号
000103                                                          保特－開始和暦年月
000105                             FILE STATUS              IS  状態キー
000106                             LOCK        MODE         IS  AUTOMATIC.
001070     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
001080                             ORGANIZATION             IS  INDEXED
001090                             ACCESS MODE              IS  DYNAMIC
001100                             RECORD KEY               IS  市－公費種別
001110                                                          市－市町村番号
001120                             ALTERNATE RECORD KEY     IS  市－公費種別
001130                                                          市－市町村名称
001140                                                          市－市町村番号
001150                             FILE STATUS              IS  状態キー
001160                             LOCK        MODE         IS  AUTOMATIC.
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
001170     SELECT  会情報マスタ    ASSIGN      TO        KAIJOHOL
001180                             ORGANIZATION             IS  INDEXED
001190                             ACCESS MODE              IS  DYNAMIC
000130                             RECORD KEY               IS  会情－柔整鍼灸区分
000131                                                          会情－協会コード
000132                                                          会情－保険種別
000133                                                          会情－変更和暦年月
000134                             ALTERNATE RECORD KEY     IS  会情－柔整鍼灸区分
000135                                                          会情－接骨師会カナ
000136                                                          会情－協会コード
000137                                                          会情－保険種別
000138                                                          会情－変更和暦年月
001270                             FILE STATUS              IS  状態キー
001280                             LOCK        MODE         IS  AUTOMATIC.
001290*
           SELECT  計算マスタ      ASSIGN      TO        KEISANL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  計－制御区分
                                                                計－開始和暦年月
                                   FILE STATUS              IS  状態キー
                                   LOCK        MODE         IS  AUTOMATIC.
           SELECT  摘要ファイル    ASSIGN      TO        TEKIYOL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  摘要－制御区分
                                                                摘要－患者コード
                                                                摘要－施術和暦年月
                                   ALTERNATE RECORD KEY     IS  摘要－制御区分
                                                                摘要－施術和暦年月
                                                                摘要－患者コード
                                   FILE STATUS              IS  状態キー
                                   LOCK        MODE         IS  AUTOMATIC.
001340     SELECT  ＩＤ管理マスタ    ASSIGN      TO        IDKANRL
001350                             ORGANIZATION             IS  INDEXED
001360                             ACCESS MODE              IS  DYNAMIC
001370                             RECORD KEY               IS  ＩＤ管－ＩＤ区分
001380                                                          ＩＤ管－施術所番号
001390                                                          ＩＤ管－保険種別
001400                                                          ＩＤ管－保険者番号
001410                             ALTERNATE RECORD KEY     IS  ＩＤ管－施術ＩＤ番号
001420                                                          ＩＤ管－ＩＤ区分
001430                                                          ＩＤ管－施術所番号
001440                                                          ＩＤ管－保険種別
001450                                                          ＩＤ管－保険者番号
001460                             FILE STATUS              IS  状態キー
001470                             LOCK        MODE         IS  AUTOMATIC.
001739*  振込口座
001740     SELECT 振込口座Ｆ       ASSIGN      TO     "C:\MAKISHISYS\YAWOBJ\IWKOUZA.DAT"
001741                             ORGANIZATION             IS  LINE SEQUENTIAL
001742                             ACCESS MODE              IS  SEQUENTIAL
001743                             FILE STATUS              IS  状態キー
001744                             LOCK        MODE         IS  AUTOMATIC.
001720* 並び順印字用
001730     SELECT  作業ファイル４  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5912L.DAT"
001740                             ORGANIZATION             IS  INDEXED
001750                             ACCESS                   IS  DYNAMIC
001760                             RECORD      KEY          IS  作４－施術和暦年月
001770                                                          作４－患者コード
001780                                                          作４－保険種別
001790                             FILE        STATUS       IS  状態キー
001800                             LOCK        MODE         IS  AUTOMATIC.
001206     SELECT 作業ファイル１ ASSIGN    TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1011L.DAT"
001208                             ORGANIZATION             IS  INDEXED
001210                             ACCESS MODE              IS  DYNAMIC
001220                             RECORD      KEY          IS  作１－レセプト番号
000520                                                          作１－施術和暦年月
000570                                                          作１－患者コード
000510                                                          作１－レセ種別
001330                             FILE STATUS              IS  状態キー
001340                             LOCK        MODE         IS  AUTOMATIC.
001206     SELECT 作業ファイル２ ASSIGN    TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1012L.DAT"
001208                             ORGANIZATION             IS  INDEXED
001210                             ACCESS MODE              IS  DYNAMIC
001220                             RECORD      KEY          IS  作２－保険種別１
000520                                                          作２－保険者番号
001330                             FILE STATUS              IS  状態キー
001340                             LOCK        MODE         IS  AUTOMATIC.
001602*     SELECT 会提出ファイル   ASSIGN      TO     FD-NAME
001602     SELECT 会提出ファイル   ASSIGN      TO  "C:\MAKISHISYS\YAWOBJ\TEMP\REZEPT1.json"
001603                             ORGANIZATION             IS  LINE SEQUENTIAL
001604                             ACCESS MODE              IS  SEQUENTIAL
001605                             FILE STATUS              IS  状態キー
001606                             LOCK      MODE           IS  AUTOMATIC.
001602     SELECT 会提出ファイル０   ASSIGN      TO  "C:\MAKISHISYS\YAWOBJ\TEMP\REZEPT3.json"
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
000780*                           ［ＲＬ＝  １２８］
000790 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
000800     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
001690*                           ［ＲＬ＝  １２８］
001700 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
001710     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
000107*
000108 FD  保険者特別負担マスタ  BLOCK   CONTAINS   1   RECORDS.
000109     COPY HOKENTK         OF  XFDLIB  JOINING   保特 AS PREFIX.
002390*                           ［ＲＬ＝  １２８］
002400 FD  負傷原因Ｆ          BLOCK   CONTAINS   1   RECORDS.
002410     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
000686*                           ［ＲＬ＝  １２８］
000687 FD  長期継続者Ｆ          BLOCK   CONTAINS   1   RECORDS.
000688     COPY CHOKEI          OF  XFDLIB  JOINING   長継   AS  PREFIX.
001720*                           ［ＲＬ＝  ２５６］
001730 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
001740     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
001750*
001760 FD  施術所情報マスタ    BLOCK   CONTAINS   1   RECORDS.
001770     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
001780*                           ［ＲＬ＝  １２８］
001790 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001800     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001810*                           ［ＲＬ＝  ３２０］
001820 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001830     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
001840*                           ［ＲＬ＝  ２５６］
001850 FD  制御情報マスタ      BLOCK   CONTAINS   1   RECORDS.
001860     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
001410     COPY SEIGYO01        OF  XFDLIB  JOINING   制０１ AS  PREFIX.
001870*                           ［ＲＬ＝  ２５６］
001880 FD  負担率マスタ        BLOCK   CONTAINS   1   RECORDS.
001890     COPY HUTANRI    OF  XFDLIB  JOINING   負率 AS  PREFIX.
001900*                           ［ＲＬ＝  ２５６］
001910 FD  市町村マスタ        BLOCK   CONTAINS   1   RECORDS.
001920     COPY SITYOSN         OF  XFDLIB  JOINING   市   AS  PREFIX.
001080* 
000280 FD  生保情報Ｆ          BLOCK   CONTAINS   1   RECORDS.
000281     COPY SEIHOJ          OF  XFDLIB  JOINING   生保   AS  PREFIX.
001510*
000280 FD  労災情報Ｆ          BLOCK   CONTAINS   1   RECORDS.
000281     COPY ROUSAIJ         OF  XFDLIB  JOINING   労災   AS  PREFIX.
001930*                           ［ＲＬ＝  ６４０］
001940 FD  会情報マスタ        BLOCK   CONTAINS   1   RECORDS.
001950     COPY KAIJOHO         OF  XFDLIB  JOINING   会情   AS  PREFIX.
      *                           ［ＲＬ＝  ２５６］
       FD  計算マスタ          BLOCK   CONTAINS   1   RECORDS.
           COPY KEISAN          OF  XFDLIB  JOINING   計   AS  PREFIX.
           COPY KEISANA         OF  XFDLIB  JOINING   計Ａ AS  PREFIX.
      *
       FD  摘要ファイル        BLOCK CONTAINS 1     RECORDS GLOBAL.
           COPY TEKIYO          OF    XFDLIB JOINING 摘要 AS PREFIX.
002300*                           ［ＲＬ＝  １２８］
002310 FD  ＩＤ管理マスタ          BLOCK   CONTAINS   1   RECORDS.
002320     COPY IDKANR    OF  XFDLIB  JOINING   ＩＤ管   AS  PREFIX.
002390**
002294 FD  振込口座Ｆ      BLOCK   CONTAINS   1   RECORDS.
002295 01  口座－レコード.
002296     03  口座－レコードデータ               PIC X(128).
002400**
002410 FD  作業ファイル４ RECORD  CONTAINS 32 CHARACTERS.
002420 01  作４－レコード.
002430     03  作４－レコードキー.
002440         05  作４－施術和暦年月.
002450             07  作４－施術和暦            PIC 9.
002460             07  作４－施術年              PIC 9(2).
002470             07  作４－施術月              PIC 9(2).
002480         05  作４－患者コード.
002490             07 作４－患者番号             PIC 9(6).
002500             07 作４－枝番                 PIC X(1).
002510         05  作４－保険種別                PIC 9(2).
002520     03  作４－レコードデータ.
002530         05  作４－順番                    PIC 9(4).
002540         05  FILLER                        PIC X(14).
002550*
001520* FD  作業ファイル１ RECORD  CONTAINS 7257 CHARACTERS.
001520* FD  作業ファイル１ RECORD  CONTAINS 7261 CHARACTERS.
001520 FD  作業ファイル１ RECORD  CONTAINS 7321 CHARACTERS.
001530 01 作１－レコード.
001540     03 作１－レコードキー.
               05 作１－レセプト番号           PIC 9(4).
000520         05 作１－施術和暦年月.
000530           07 作１－施術和暦             PIC 9.
000540           07 作１－施術年月.
000550             09 作１－施術年             PIC 9(2).
000560             09 作１－施術月             PIC 9(2).
000570         05 作１－患者コード.
000580           07 作１－患者番号             PIC 9(6).
000590           07 作１－枝番                 PIC X.
000510         05 作１－レセ種別               PIC 9(2).
           03 作１－レコードデータ.
               05 作１－施術者番号             PIC X(11).
               05 作１－施術西暦年月           PIC X(6).
               05 作１－提出先区分             PIC X(1).
               05 作１－保険者番号             PIC X(8).
               05 作１－保険証記号             PIC X(16).
               05 作１－保険証番号             PIC X(16).
               05 作１－公費負担者番号１       PIC X(8).
               05 作１－公費受給者番号１       PIC X(16).
               05 作１－公費負担者番号２       PIC X(8).
               05 作１－公費受給者番号２       PIC X(16).
               05 作１－保険種別大区分         PIC X(1).
               05 作１－単併区分               PIC X(1).
               05 作１－本家区分               PIC X(1).
               05 作１－給付割合               PIC 9(2).
               05 作１－続柄                   PIC X(1).
               05 作１－被保険者カナ           PIC X(25).
               05 作１－被保険者名             PIC X(30).
               05 作１－受療者名カナ           PIC X(25).
               05 作１－受療者名               PIC X(30).
               05 作１－受療者性別             PIC X(1).
               05 作１－受療者生年月日         PIC X(8).
               05 作１－被保険者郵便番号       PIC X(7).
      */全角半角問わず最大60文字20230413
      *         05 作１－被保険者住所           PIC X(60).
               05 作１－被保険者住所           PIC X(120).
               05 作１－合計金額               PIC 9(6).
               05 作１－一部負担金             PIC 9(6).
               05 作１－請求金額               PIC 9(6).
               05 作１－公費一部負担金相当額   PIC 9(6).
               05 作１－公費請求金額           PIC 9(6).
               05 作１－総実日数               PIC 9(2).
               05 作１－部位数                 PIC 9(1).
               05 作１－再請求区分             PIC X(1).
               05 作１－負担区分               PIC X(1).
               05 作１－負担割合               PIC 9(1).
               05 作１－患者番号２             PIC 9(6).
               05 作１－患者番号枝番           PIC 9(1).
               05 作１－負傷データ OCCURS 6.
                   07 作１－負傷ナンバー       PIC 9(1).
                   07 作１－負傷区分           PIC X(1).
                   07 作１－負傷コード         PIC X(8).
                   07 作１－負傷名             PIC X(32).
                   07 作１－負傷年月日         PIC X(8).
                   07 作１－初検年月日         PIC X(8).
                   07 作１－施術開始日         PIC X(8).
                   07 作１－施術終了日         PIC X(8).
                   07 作１－実日数             PIC 9(2).
                   07 作１－部位施術日.
                      09 作１－施術日          PIC 9(2) OCCURS 31.
                   07 作１－転帰区分           PIC X(1).
                   07 作１－整固施区分         PIC X(1).
                   07 作１－整固施回数         PIC 9(1).
                   07 作１－整固施療料         PIC 9(5).
                   07 作１－負傷原因           PIC X(200).
                   07 作１－長期理由           PIC X(200).
                   07 作１－長期頻回理由       PIC X(200).
                   07 作１－経過               PIC X(200).
                   07 作１－審査区分           PIC X(1).
               05 作１－請求区分               PIC X(1).
               05 作１－初検回数               PIC 9(1).
               05 作１－初検料                 PIC 9(5).
               05 作１－初検休日加算回数       PIC 9(1).
               05 作１－初検深夜加算回数       PIC 9(1).
               05 作１－初検時間外加算回数     PIC 9(1).
               05 作１－初検加算               PIC 9(5).
               05 作１－初検時相談支援料回数   PIC 9(1).
               05 作１－初検時相談支援料       PIC 9(5).
               05 作１－再検回数               PIC 9(1).
               05 作１－再検料                 PIC 9(5).
               05 作１－往療距離               PIC 9(3).
               05 作１－往療回数               PIC 9(2).
               05 作１－往療料                 PIC 9(5).
               05 作１－往療理由               PIC X(200).
               05 作１－夜間加算の往療回数     PIC 9(2).
               05 作１－難路加算の往療回数     PIC 9(2).
               05 作１－暴風雨雪加算の往療回数 PIC 9(2).
               05 作１－往療加算               PIC 9(5).
               05 作１－金属副子大回数         PIC 9(2).
               05 作１－金属副子中回数         PIC 9(1).
               05 作１－金属副子小回数         PIC 9(1).
               05 作１－金属副子加算           PIC 9(5).
               05 作１－金属副子加算日         PIC 9(2) OCCURS 3.
               05 作１－施術情報提供料の回数   PIC 9(1).
               05 作１－施術情報提供料         PIC 9(5).
               05 作１－運動後療実施回数       PIC 9(1).
               05 作１－運動後療料             PIC 9(5).
               05 作１－運動後療実施日         PIC 9(2) OCCURS 5.
               05 作１－部位別料金データ OCCURS 10.
                   07 作１－行番号             PIC 9(1).
                   07 作１－逓減開始月日       PIC X(4).
                   07 作１－後療回数           PIC 9(2).
                   07 作１－後療料             PIC 9(5).
                   07 作１－冷罨法回数         PIC 9(1).
                   07 作１－冷罨法料           PIC 9(5).
                   07 作１－温罨法回数         PIC 9(2).
                   07 作１－温罨法料           PIC 9(5).
                   07 作１－電療回数           PIC 9(2).
                   07 作１－電療料             PIC 9(5).
                   07 作１－多部位逓減率       PIC 9(2).
                   07 作１－多部位逓減額       PIC 9(5).
                   07 作１－長期逓減率         PIC 9(2).
                   07 作１－部位逓減率別料金計 PIC 9(5).
               05 作１－摘要                   PIC X(400).
               05 作１－同意年月日             PIC X(8).
               05 作１－同意医院               PIC X(40).
               05 作１－同意医師名             PIC X(20).
               05 作１－保険種別詳細           PIC X(2).
      */項目追加↓↓↓/20221028
               05 作１－明細書発行加算料       PIC 9(2).
               05 作１－明細書発行加算日       PIC 9(2).
      */項目追加↑↑↑/20221028
001520 FD  作業ファイル２ RECORD  CONTAINS 241 CHARACTERS.
001530 01 作２－レコード.
001540     03 作２－レコードキー.
               05 作２－保険種別１         PIC 9(2).
               05 作２－保険者番号         PIC X(8).
           03 作２－レコードデータ.
               05 作２－保険者名           PIC X(40).
               05 作２－保険種別           PIC X(2).
               05 作２－県コード           PIC X(2).
               05 作２－郵便番号           PIC X(7).
               05 作２－住所１             PIC X(80).
               05 作２－住所２             PIC X(80).
               05 作２－電話番号           PIC X(13).
               05 作２－口座番号           PIC X(7).
004472* FD  会提出ファイル RECORD  CONTAINS 200 CHARACTERS.
004472* FD  会提出ファイル RECORD IS VARYING IN SIZE FROM 1 TO 200 DEPENDING ON 文字ＣＮＴ.
004472 FD  会提出ファイル RECORD IS VARYING IN SIZE FROM 1 TO 500 DEPENDING ON 文字ＣＮＴ.
004473 01  会提－レコード.
004474*     03  会提－レコードデータ        PIC X(200).
004474     03  会提－レコードデータ        PIC X(500).
004472 FD  会提出ファイル０ RECORD IS VARYING IN SIZE FROM 1 TO 200 DEPENDING ON 文字ＣＮＴ.
004473 01  会提０－レコード.
004474     03  会提０－レコードデータ        PIC X(200).
004480*
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
004580 01 終了フラグ４                       PIC X(3) VALUE SPACE.
002090 01 エラーフラグ                       PIC X(3) VALUE SPACE.
004590 01 実行キーＷ                         PIC X(3)  VALUE SPACE.
004600 01 施術記録有Ｗ                       PIC X(3) VALUE SPACE.
004610 01 ファイル名                         PIC N(8) VALUE SPACE.
004620 01 カウンタ                           PIC 9(2) VALUE ZERO.
004630 01 文字カウンタ                       PIC 9(4) VALUE ZERO.
004631 01 文字カウンタ２                     PIC 9(4) VALUE ZERO.
004632 01 文字カウンタ３                     PIC 9(4) VALUE ZERO.
004633 01 文字カウンタ４                     PIC 9(4) VALUE ZERO.
004634 01 文字カウンタ５                     PIC 9(4) VALUE ZERO.
004635 01 文字カウンタ６                     PIC 9(4) VALUE ZERO.
004640 01 連番Ｗ                             PIC 9(4) VALUE ZERO.
004650 01 預金種別Ｗ                         PIC X(1) VALUE SPACE.
004660 01 柔整師コードＷ.
004670    03 協Ｗ                            PIC N(1) VALUE SPACE.
004680    03 柔整師コード数字Ｗ.
004690       05 柔整師コード数字１Ｗ         PIC X(4) VALUE SPACE.
004700       05 会員番号コードＷ             PIC X(3) VALUE SPACE.
004710*
004720 01 請求西暦年Ｗ                       PIC 9(4) VALUE ZERO.
004730 01 請求和暦年月ＷＲ.
004740    03 請求和暦ＷＲ                    PIC 9(1) VALUE ZERO.
004750    03 請求年月ＷＲ.
004760       05 請求年ＷＲ                   PIC 9(2) VALUE ZERO.
004770       05 請求月ＷＲ                   PIC 9(2) VALUE ZERO.
004780*
004790* 01 西暦年Ｗ                           PIC 9(4) VALUE ZERO.
004800* 01 和暦Ｗ                             PIC 9(1) VALUE ZERO.
004810* 01 年Ｗ                               PIC 9(2) VALUE ZERO.
004820 01 和暦年月日Ｗ.
004830   03 和暦年月Ｗ.
004840      05 和暦Ｗ                        PIC 9(1) VALUE ZERO.
004850      05 年Ｗ                          PIC 9(2) VALUE ZERO.
004860      05 月Ｗ                          PIC 9(2) VALUE ZERO.
004870   03 日Ｗ                             PIC 9(2) VALUE ZERO.
004880 01 日付８桁Ｗ                         PIC 9(8) VALUE ZERO.
       01 開始Ｗ.
004890   03 開始和暦年月日Ｗ                 OCCURS 7.
004900     05 開始和暦Ｗ                     PIC 9(1) VALUE ZERO.
004910     05 開始年Ｗ                       PIC 9(2) VALUE ZERO.
004920     05 開始月Ｗ                       PIC 9(2) VALUE ZERO.
004930     05 開始日Ｗ                       PIC 9(2) VALUE ZERO.
       01 終了Ｗ.
004940   03 終了和暦年月日Ｗ                 OCCURS 7.
004950     05 終了和暦Ｗ                     PIC 9(1) VALUE ZERO.
004960     05 終了年Ｗ                       PIC 9(2) VALUE ZERO.
004970     05 終了月Ｗ                       PIC 9(2) VALUE ZERO.
004980     05 終了日Ｗ                       PIC 9(2) VALUE ZERO.
004980 01 終了日ＷＲ                         PIC 9(2) VALUE ZERO.
004990 01 実日数Ｗ                           PIC 9(2) VALUE ZERO.
005000 01 部位ＣＮＴ                         PIC 9(2) VALUE ZERO.
005010 01 部位ＣＮＴ２                       PIC 9(2) VALUE ZERO.
005000 01 日カウンタ                         PIC 9(2) VALUE ZERO.
005020 01 同時負傷数Ｗ                       PIC 9(2) VALUE ZERO.
005040 01 継続部位数Ｗ                       PIC 9    VALUE ZERO.
005050 01 最大継続部位数Ｗ                   PIC 9    VALUE ZERO.
005060 01 負傷終了和暦年月日Ｗ               PIC 9(7) VALUE ZERO.
005070 01 負傷種別Ｗ                         PIC 9(2) VALUE ZERO.
005070 01 レセプト番号Ｗ                     PIC 9(4) VALUE ZERO.
005080 01 部位タイプＷ                       PIC 9    VALUE ZERO.
005100 01 負傷名称Ｗ                         PIC N(10) VALUE SPACE.
       01 記号番号Ｗ.
007570    03 記号Ｗ.
007580       05 印刷記号Ｗ                   PIC N(12)  VALUE SPACE.
007670    03 番号Ｗ.
007680       05 印刷番号Ｗ                   PIC X(30)  VALUE SPACE.
005090*
005100 01 遅延フラグ                         PIC X(3) VALUE SPACE.
005110 01 遅延ＣＮＴ                         PIC 9(5) VALUE ZERO.
005120 01 遅延カウンタ                       PIC 9(6) VALUE ZERO.
005130 01 遅延回数Ｗ                         PIC 9(4) VALUE ZERO.
005140*
005150 01 英数字項目Ｗ.
005160   03 英数字項目ＸＷ                   PIC X(70) VALUE SPACE.
005170 01 日本語項目Ｗ.
005180   03 日本語項目ＮＷ                   PIC N(35) VALUE SPACE.
005190 01 英数字項目２Ｗ                     PIC X(70) VALUE SPACE.
005200*
005210*次月取得用ワーク
005220 01 次和暦開始年月Ｗ                   PIC 9(6) VALUE ZERO.
005230 01 次月和暦年月Ｗ.
005240    03 次月和暦Ｗ                      PIC 9    VALUE ZERO.
005250    03 次月年Ｗ                        PIC 9(2) VALUE ZERO.
005260    03 次月月Ｗ                        PIC 9(2) VALUE ZERO.
005270 01 次月西暦年月Ｗ.
005280    03 次月西暦年Ｗ                    PIC 9(4) VALUE ZERO.
005290    03 次月西暦月Ｗ                    PIC 9(2) VALUE ZERO.
005300 01 前月和暦年月取得用Ｗ.
005310    03 当月和暦Ｗ                      PIC 9(1) VALUE ZERO.
005320    03 当月年Ｗ                        PIC 9(2) VALUE ZERO.
005330    03 当月月Ｗ                        PIC 9(2) VALUE ZERO.
005340*    03 前月和暦Ｗ                      PIC 9(1) VALUE ZERO.
005350*    03 前月年Ｗ                        PIC 9(2) VALUE ZERO.
005360*    03 前月月Ｗ                        PIC 9(2) VALUE ZERO.
005370*--- 会長委任用 ---*
005380 01 会長委任フラグ                     PIC X(3)  VALUE SPACE.
005381 01 保険者データＷ                     PIC X(340)  VALUE SPACE.
005382 01 保険者番号Ｗ                       PIC X(10) VALUE SPACE.
005383 01 保険者名称Ｗ                       PIC X(60) VALUE SPACE.
005384 01 郵送先名称Ｗ                       PIC X(60) VALUE SPACE.
005385 01 印字用名称Ｗ                       PIC X(60) VALUE SPACE.
005386 01 住所１Ｗ                           PIC X(40) VALUE SPACE.
005387 01 住所２Ｗ                           PIC X(40) VALUE SPACE.
      *
       01 回数カウンタＷ.
          03 初検回数Ｗ                      PIC 9(2)  VALUE ZERO.
          03 時間外回数Ｗ                    PIC 9(2)  VALUE ZERO.
          03 休日回数Ｗ                      PIC 9(2)  VALUE ZERO.
          03 深夜回数Ｗ                      PIC 9(2)  VALUE ZERO.
          03 再検回数Ｗ                      PIC 9(2)  VALUE ZERO.
          03 往療回数Ｗ                      PIC 9(2)  VALUE ZERO.
          03 夜間回数Ｗ                      PIC 9(2)  VALUE ZERO.
          03 難路回数Ｗ                      PIC 9(2)  VALUE ZERO.
          03 暴風雨雪回数Ｗ                  PIC 9(2)  VALUE ZERO.
          03 大回数Ｗ                        PIC 9(2)  VALUE ZERO.
          03 中回数Ｗ                        PIC 9(2)  VALUE ZERO.
          03 小回数Ｗ                        PIC 9(2)  VALUE ZERO.
          03 情報提供回数Ｗ                  PIC 9(2)  VALUE ZERO.
          03 相談支援回数Ｗ                  PIC 9(1)  VALUE ZERO.
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
002251 01 ドライブＷＲ                       PIC X VALUE SPACE.
005723 01 FD-NAME                            PIC X(30) VALUE SPACE.
005724*
005725 01 保険種別Ｗ                         PIC X(2)  VALUE SPACE.
014774*
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
003040** 西暦日付ワーク用
003050 01 西暦年月Ｗ.
003060    03 西暦年Ｗ                        PIC 9(4) VALUE ZERO.
003070    03 西暦月Ｗ                        PIC 9(2) VALUE ZERO.
003080** 西暦請求年月用
003090 01 西暦請求年月Ｗ.
003100    03 西暦請求年Ｗ                    PIC 9(4) VALUE ZERO.
003110    03 西暦請求月Ｗ                    PIC 9(2) VALUE ZERO.
004982* 暗号複合用
004983*
000140 01 和暦計算年月日Ｗ.
000150   03 和暦計算年月Ｗ.
000160     05 和暦計算和暦Ｗ                 PIC 9    VALUE ZERO.
000170     05 和暦計算年月Ｗ.
000180       07 和暦計算年Ｗ                 PIC 9(2) VALUE ZERO.
000190       07 和暦計算月Ｗ                 PIC 9(2) VALUE ZERO.
000200   03 和暦計算日Ｗ                     PIC 9(2) VALUE ZERO.
000140 01 西暦計算年月日Ｗ.
000170   03 西暦計算年月Ｗ.
000180       05 西暦計算年Ｗ                 PIC 9(4) VALUE ZERO.
000190       05 西暦計算月Ｗ                 PIC 9(2) VALUE ZERO.
000200   03 西暦計算日Ｗ                     PIC 9(2) VALUE ZERO.
003670****************
003680* 負傷データＦ *
003690****************
003700 01 負傷情報Ｗ.
003720    03 部位情報Ｗ  OCCURS   9.
             05 負傷名ＷＲ.
003790          07 負傷名Ｗ                  PIC N(16) VALUE SPACE.
004010       05 初回処置回数Ｗ               PIC 9     VALUE ZERO.
      *
005000 01 部位毎情報Ｗ.
005000    03 部位日ＣＮＴ OCCURS 9           PIC 9(2) VALUE ZERO.
005000    03 金属ＣＮＴ                      PIC 9(2) VALUE ZERO.
004590    03 多部位率３ＷＲ                  PIC 9(2) VALUE ZERO.
      * レセ摘要用
001362 01 分解カウンタ                       PIC 9(3)  VALUE ZERO.
       01 文字位置Ｗ                         PIC 9(4)  VALUE ZERO.
001363 01 改行                               PIC X(2)  VALUE X"0D0A".
001367 01 摘要Ｗ.
001370    03 摘要文Ｗ                        PIC X(1800)  VALUE SPACE.
001367 01 摘要文分解Ｗ.
          03 分解文Ｗ                        OCCURS 20.
001370       05 摘要分解文Ｗ                 PIC X(1800)  VALUE SPACE.
001370 01 文章Ｗ                             PIC X(1800)  VALUE SPACE.
       01 適文４５桁Ｗ.
001403    03 適文４５桁１Ｗ                  PIC X(90) VALUE SPACE.
001403    03 適文４５桁２Ｗ                  PIC X(90) VALUE SPACE.
001403    03 適文４５桁３Ｗ                  PIC X(90) VALUE SPACE.
001403    03 適文４５桁４Ｗ                  PIC X(90) VALUE SPACE.
001403    03 適文４５桁５Ｗ                  PIC X(90) VALUE SPACE.
005260 01 振込口座Ｗ.
005261    03 請求保険者番号Ｗ                PIC X(10)  VALUE SPACE.
005262    03 請求保険者名Ｗ                  PIC X(100) VALUE SPACE.
005263    03 請求口座番号Ｗ                  PIC X(10)  VALUE SPACE.
005263    03 金融機関コードＷ                PIC X(8)  VALUE SPACE.
007370**************
007380* 施術所情報 *
007390**************
007400 01 施術所情報Ｗ.
007410    03 柔整師番号Ｗ                    PIC X(16)  VALUE SPACE.
007420    03 接骨師会会員番号Ｗ              PIC X(16)  VALUE SPACE.
007430    03 代表者カナＷ                    PIC X(50)  VALUE SPACE.
007440    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
007450    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
          03 都道府県ＪＩＳＷ                PIC X(2)   VALUE SPACE.
007460    03 施術所住所Ｗ.
007470       05 施術所住所１Ｗ               PIC X(50)  VALUE SPACE.
007480       05 施術所住所２Ｗ               PIC X(50)  VALUE SPACE.
007490    03 施術所郵便番号Ｗ.
007500       05 施術所郵便番号１Ｗ           PIC X(3)   VALUE SPACE.
007510       05 施術所郵便番号２Ｗ           PIC X(4)   VALUE SPACE.
007520    03 施術所電話番号Ｗ                PIC X(15)  VALUE SPACE.
007530    03 定額制受理番号Ｗ                PIC X(15)  VALUE SPACE.
007540    03 受理年月日Ｗ.
007350       05 受理和暦Ｗ                   PIC 9     VALUE ZERO.
007550       05 受理年Ｗ                     PIC 9(2)   VALUE ZERO.
007560       05 受理月Ｗ                     PIC 9(2)   VALUE ZERO.
007570       05 受理日Ｗ                     PIC 9(2)   VALUE ZERO.
007580    03 最終通院年月日Ｗ.
007390       05 最終通院和暦Ｗ               PIC 9     VALUE ZERO.
007590       05 最終通院年Ｗ                 PIC 9(2)   VALUE ZERO.
007600       05 最終通院月Ｗ                 PIC 9(2)   VALUE ZERO.
007610       05 最終通院日Ｗ                 PIC 9(2)   VALUE ZERO.
007620    03 柔整師年月日Ｗ.
007350       05 柔整師和暦Ｗ                 PIC 9      VALUE ZERO.
007630       05 柔整師年Ｗ                   PIC 9(2)   VALUE ZERO.
007640       05 柔整師月Ｗ                   PIC 9(2)   VALUE ZERO.
007650       05 柔整師日Ｗ                   PIC 9(2)   VALUE ZERO.
007660    03 患者委任年月日Ｗ.
007350       05 患者委任和暦Ｗ               PIC 9      VALUE ZERO.
007670       05 患者委任年Ｗ                 PIC 9(2)   VALUE ZERO.
007680       05 患者委任月Ｗ                 PIC 9(2)   VALUE ZERO.
007690       05 患者委任日Ｗ                 PIC 9(2)   VALUE ZERO.
007700    03 取引先情報Ｗ.
007710        05 取引先銀行名Ｗ.
007720           07 取引先銀行名１Ｗ         PIC X(10)  VALUE SPACE.
007730           07 取引先銀行名２Ｗ         PIC X(10)  VALUE SPACE.
007740           07 FILLER                   PIC X(20)  VALUE SPACE.
007750        05 取引先銀行支店名Ｗ.
007760           07 取引先銀行支店名１Ｗ     PIC X(10)  VALUE SPACE.
007770           07 取引先銀行支店名２Ｗ     PIC X(10)  VALUE SPACE.
007780           07 FILLER                   PIC X(20)  VALUE SPACE.
007790        05 預金種別Ｗ                  PIC 9(1)   VALUE ZERO.
007800        05 口座番号Ｗ                  PIC X(10)  VALUE SPACE.
007810        05 口座名義人Ｗ                PIC X(40)  VALUE SPACE.
007820        05 口座名義人カナＷ            PIC X(40)  VALUE SPACE.
007830        05 預金種別コメントＷ          PIC N(3)   VALUE SPACE.
007840        05 預金種別コメントＸＷ        PIC X(4)   VALUE SPACE.
007850*
007860    03 県施術ＩＤＷ                    PIC X(15)  VALUE SPACE.
007870    03 市町村施術ＩＤＷ                PIC X(15)  VALUE SPACE.
007880    03 共済番号Ｗ                      PIC X(28)  VALUE SPACE.
007880    03 地共済番号Ｗ                    PIC X(28)  VALUE SPACE.
      *
008070 01 公費種別Ｗ                         PIC 9(2)   VALUE ZERO.
008010 01 市町村番号Ｗ                       PIC X(10)  VALUE SPACE.
004510 01 助成種別ＷＲ                       PIC 9(2)  VALUE ZERO.
       01 文字ＣＮＴ                         PIC 9(4)  VALUE ZERO.
003520 01 区切文字Ｒ                         PIC X(2) VALUE X"0022".
003530 01 区切文字Ｗ REDEFINES 区切文字Ｒ.
003540   03 区切文字Ｗ１                     PIC X(1).
003550   03 区切文字Ｗ２                     PIC X(1).
       01 施術開始年月日Ｗ                   PIC 9(7) VALUE ZERO.
       01 文字数判定Ｗ                       PIC X(100) VALUE SPACE.
       01 アットマークＷ                     PIC X(1) VALUE X"40".
      */ZIP圧縮
       01  パラ１Ｗ PIC X(2001) VALUE SPACE.
       01  プログラム名Ｗ PIC X(8)  VALUE "zip".
005723 01 圧縮ファイル名Ｗ                   PIC X(60) VALUE SPACE.
005723 01 元ファイル名Ｗ                     PIC X(60) VALUE SPACE.
      * C 連携用
       01 フルパス名Ｗ     PIC X(151) VALUE SPACE.
       01 空白詰Ｗ                           PIC X(200) VALUE SPACE.
      */作業ファイル１ワーク
001530 01 作１レコードＷ.
001540     03 作１レコードキーＷ.
               05 作１レセプト番号Ｗ           PIC Z(4).
000520         05 作１施術和暦年月Ｗ.
000530           07 作１施術和暦Ｗ             PIC 9.
000540           07 作１施術年月Ｗ.
000550             09 作１施術年Ｗ             PIC 9(2).
000560             09 作１施術月Ｗ             PIC 9(2).
000570         05 作１患者コードＷ.
000580           07 作１患者番号Ｗ             PIC 9(6).
000590           07 作１枝番Ｗ                 PIC X.
000510         05 作１レセ種別Ｗ               PIC 9(2).
           03 作１レコードデータＷ.
               05 作１施術者番号Ｗ             PIC X(11).
               05 作１施術西暦年月Ｗ           PIC X(6).
               05 作１提出先区分Ｗ             PIC X(1).
               05 作１保険者番号Ｗ             PIC X(8).
               05 作１保険証記号Ｗ             PIC X(16).
               05 作１保険証番号Ｗ             PIC X(16).
               05 作１公費負担者番号１Ｗ       PIC X(8).
               05 作１公費受給者番号１Ｗ       PIC X(16).
               05 作１公費負担者番号２Ｗ       PIC X(8).
               05 作１公費受給者番号２Ｗ       PIC X(16).
               05 作１保険種別大区分Ｗ         PIC X(1).
               05 作１単併区分Ｗ               PIC X(1).
               05 作１本家区分Ｗ               PIC X(1).
               05 作１給付割合Ｗ               PIC 9(2).
               05 作１続柄Ｗ                   PIC X(1).
               05 作１被保険者カナＷ           PIC X(25).
               05 作１被保険者名Ｗ             PIC X(30).
               05 作１受療者名カナＷ           PIC X(25).
               05 作１受療者名Ｗ               PIC X(30).
               05 作１受療者性別Ｗ             PIC X(1).
               05 作１受療者生年月日Ｗ         PIC X(8).
               05 作１被保険者郵便番号Ｗ       PIC X(7).
               05 作１被保険者住所Ｗ           PIC X(60).
               05 作１合計金額Ｗ               PIC 9(6).
               05 作１一部負担金Ｗ             PIC 9(6).
               05 作１請求金額Ｗ               PIC 9(6).
               05 作１公費一部負担金相当額Ｗ   PIC 9(6).
               05 作１公費請求金額Ｗ           PIC 9(6).
               05 作１総実日数Ｗ               PIC 9(2).
               05 作１部位数Ｗ                 PIC 9(1).
               05 作１再請求区分Ｗ             PIC X(1).
               05 作１負担区分Ｗ               PIC X(1).
               05 作１負担割合Ｗ               PIC 9(1).
               05 作１患者番号２Ｗ             PIC 9(6).
               05 作１患者番号枝番Ｗ           PIC 9(1).
               05 作１負傷データＷ OCCURS 6.
                   07 作１負傷ナンバーＷ       PIC 9(1).
                   07 作１負傷区分Ｗ           PIC X(1).
                   07 作１負傷コードＷ         PIC X(8).
                   07 作１負傷名Ｗ             PIC X(32).
                   07 作１負傷年月日Ｗ         PIC X(8).
                   07 作１初検年月日Ｗ         PIC X(8).
                   07 作１施術開始日Ｗ         PIC X(8).
                   07 作１施術終了日Ｗ         PIC X(8).
                   07 作１実日数Ｗ             PIC X(2).
                   07 作１部位施術日Ｗ.
                      09 作１施術日Ｗ          PIC 9(2) OCCURS 31.
                   07 作１転帰区分Ｗ           PIC X(1).
                   07 作１整固施区分Ｗ         PIC X(1).
                   07 作１整固施回数Ｗ         PIC 9(1).
                   07 作１整固施療料Ｗ         PIC 9(5).
                   07 作１負傷原因Ｗ           PIC X(200).
                   07 作１長期理由Ｗ           PIC X(200).
                   07 作１長期頻回理由Ｗ       PIC X(200).
                   07 作１経過Ｗ               PIC X(200).
                   07 作１審査区分Ｗ           PIC X(1).
               05 作１請求区分Ｗ               PIC X(1).
               05 作１初検回数Ｗ               PIC 9(1).
               05 作１初検料Ｗ                 PIC 9(5).
               05 作１初検休日加算回数Ｗ       PIC 9(1).
               05 作１初検深夜加算回数Ｗ       PIC 9(1).
               05 作１初検時間外加算回数Ｗ     PIC 9(1).
               05 作１初検加算Ｗ               PIC 9(5).
               05 作１初検時相談支援料回数Ｗ   PIC 9(1).
               05 作１初検時相談支援料Ｗ       PIC 9(5).
               05 作１再検回数Ｗ               PIC 9(1).
               05 作１再検料Ｗ                 PIC 9(5).
               05 作１往療距離Ｗ               PIC 9(3).
               05 作１往療回数Ｗ               PIC 9(2).
               05 作１往療料Ｗ                 PIC 9(5).
               05 作１往療理由Ｗ               PIC X(200).
               05 作１夜間加算の往療回数Ｗ     PIC 9(2).
               05 作１難路加算の往療回数Ｗ     PIC 9(2).
               05 作１暴風雨雪加算の往療回数Ｗ PIC 9(2).
               05 作１往療加算Ｗ               PIC 9(5).
               05 作１金属副子大回数Ｗ         PIC 9(2).
               05 作１金属副子中回数Ｗ         PIC 9(1).
               05 作１金属副子小回数Ｗ         PIC 9(1).
               05 作１金属副子加算Ｗ           PIC 9(5).
               05 作１金属副子加算日Ｗ         PIC 9(2) OCCURS 3.
               05 作１施術情報提供料の回数Ｗ   PIC 9(1).
               05 作１施術情報提供料Ｗ         PIC 9(5).
               05 作１運動後療実施回数Ｗ       PIC 9(1).
               05 作１運動後療料Ｗ             PIC 9(5).
               05 作１運動後療実施日Ｗ         PIC 9(2) OCCURS 5.
               05 作１部位別料金データＷ OCCURS 10.
                   07 作１行番号Ｗ             PIC 9(1).
                   07 作１逓減開始月日Ｗ       PIC X(4).
                   07 作１後療回数Ｗ           PIC 9(2).
                   07 作１後療料Ｗ             PIC 9(5).
                   07 作１冷罨法回数Ｗ         PIC 9(1).
                   07 作１冷罨法料Ｗ           PIC 9(5).
                   07 作１温罨法回数Ｗ         PIC 9(2).
                   07 作１温罨法料Ｗ           PIC 9(5).
                   07 作１電療回数Ｗ           PIC 9(2).
                   07 作１電療料Ｗ             PIC 9(5).
                   07 作１多部位逓減率Ｗ       PIC 9(2).
                   07 作１多部位逓減額Ｗ       PIC 9(5).
                   07 作１長期逓減率Ｗ         PIC 9(2).
                   07 作１部位逓減率別料金計Ｗ PIC 9(5).
               05 作１摘要Ｗ                   PIC X(400).
               05 作１同意年月日Ｗ             PIC X(8).
               05 作１同意医院Ｗ               PIC X(40).
               05 作１同意医師名Ｗ             PIC X(20).
               05 作１保険種別詳細Ｗ           PIC X(2).
               05 作１明細書発行加算料Ｗ       PIC 9(2).
               05 作１明細書発行加算日Ｗ       PIC 9(2).
      *
       01 数字左詰ＸＷ.
           03 数字左詰Ｗ                       PIC 9(6).
       01 数字左詰２Ｗ                         PIC X(6).
      *
       01 並べ順Ｆ                             PIC 9(1) VALUE ZERO.
       01 行番号存在Ｆ                         PIC 9(1) VALUE ZERO.
       01 明細発行加算区分Ｗ                   PIC 9(1) VALUE ZERO.
       01 明細書発行回数Ｗ                     PIC 9(1) VALUE ZERO.
004010*文字数カウント用/20230413
004020 01 漢字チェックＴＢＬ.
004030    03 漢字チェックＷ                  PIC X(1)  OCCURS 120.
004040    03 商Ｗ                            PIC 9(3) VALUE ZERO.
004050    03 余Ｗ                            PIC 9(3) VALUE ZERO.
004060*
004230 01 カウンタ１                         PIC 9(3)  VALUE ZERO.
004240 01 カウンタ２                         PIC 9(3)  VALUE ZERO.
004250 01 カウンタ３                         PIC 9(3)  VALUE ZERO.
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
005860*
004091 01 連メ７－キー IS EXTERNAL.
004092    03  連メ７－メッセージ１           PIC X(40).
004102    03  連メ７－メッセージ２           PIC X(40).
005870****************
005880* 画面入力情報 *
005890****************
       01 連入－画面情報ＹＩＷ５８０ IS EXTERNAL.
          03 連入－請求和暦年月.
             05 連入－請求和暦               PIC 9.
             05 連入－請求年                 PIC 9(2).
             05 連入－請求月                 PIC 9(2).
          03 連入－プレビュー区分            PIC 9(1).
      *
       01 連入－画面情報ＹＩＷ１００ IS EXTERNAL.
          03 連入－ドライブ                  PIC X(1).
      *
      *0:通常 1:固定パス
       01 連入－固定フラグ３２１１ IS EXTERNAL.
          03 連入－固定フラグ                PIC 9(1).
006280*
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
006290******************************************************************
006300*                      PROCEDURE  DIVISION                       *
006310******************************************************************
006320 PROCEDURE               DIVISION.
006330************
006340*           *
006350* 初期処理   *
006360*           *
006370************
           PERFORM 並び順設定.
006380     PERFORM 初期化.
006390************
006400*           *
006410* 主処理     *
006420*           *
006430************
006440     PERFORM 制御情報取得.
006450     PERFORM 連結項目退避.
010020     PERFORM 作業ファイル１作成.
           PERFORM 会提出ファイル作成.
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
006650**================================================================*
006660 ファイルオープン SECTION.
006670*
            MOVE "C:\MAKISHISYS\YAWOBJ\TEMP\REZEPT.json" TO 元ファイル名Ｗ
            MOVE "delfile" TO プログラム名Ｗ.
            CALL プログラム名Ｗ WITH C LINKAGE
                    USING BY REFERENCE 元ファイル名Ｗ.
            MOVE "C:\MAKISHISYS\YAWOBJ\TEMP\REZEPT1.json" TO 元ファイル名Ｗ
            MOVE "delfile" TO プログラム名Ｗ.
            CALL プログラム名Ｗ WITH C LINKAGE
                    USING BY REFERENCE 元ファイル名Ｗ.
            MOVE "C:\MAKISHISYS\YAWOBJ\TEMP\REZEPT3.json" TO 元ファイル名Ｗ
            MOVE "delfile" TO プログラム名Ｗ.
            CALL プログラム名Ｗ WITH C LINKAGE
                    USING BY REFERENCE 元ファイル名Ｗ.
006680     OPEN INPUT 受診者情報Ｆ
006690         MOVE NC"受" TO ファイル名.
006700         PERFORM オープンチェック.
006630     OPEN INPUT レセプトＦ.
006640         MOVE NC"レセ" TO ファイル名.
006650         PERFORM オープンチェック.
006770     OPEN INPUT 施術所情報マスタ
006780         MOVE NC"施情" TO ファイル名.
006790         PERFORM オープンチェック.
006800     OPEN INPUT 元号マスタ.
006810             MOVE NC"元号" TO ファイル名.
006820             PERFORM オープンチェック.
006710     OPEN INPUT 負傷データＦ.
006720             MOVE NC"負傷" TO ファイル名.
006730             PERFORM オープンチェック.
006740     OPEN INPUT 施術記録Ｆ.
006750             MOVE NC"施記" TO ファイル名.
006760             PERFORM オープンチェック.
004140     OPEN INPUT  名称マスタ
004150         MOVE NC"名称" TO ファイル名.
004160         PERFORM オープンチェック.
014910     OPEN INPUT   負傷原因Ｆ.
014920         MOVE NC"負傷原因" TO ファイル名.
014930         PERFORM オープンチェック.
002650     OPEN INPUT   長期継続者Ｆ.
002651         MOVE NC"長期継続者Ｆ" TO ファイル名.
002652         PERFORM オープンチェック.
           OPEN INPUT 計算マスタ.
               MOVE NC"計算" TO ファイル名.
               PERFORM オープンチェック.
014970     OPEN INPUT 摘要ファイル.
014980         MOVE NC"摘要" TO ファイル名.
014990         PERFORM オープンチェック.
006830     OPEN INPUT 保険者マスタ.
006840             MOVE NC"保険者マスタ" TO ファイル名.
006850             PERFORM オープンチェック.
006920     OPEN INPUT 市町村マスタ.
006930             MOVE NC"市町" TO ファイル名.
006940             PERFORM オープンチェック.
015080     OPEN INPUT   ＩＤ管理マスタ
015090         MOVE NC"ＩＤ" TO ファイル名.
015100         PERFORM オープンチェック.
015170     OPEN INPUT  作業ファイル４.
015170         IF ( 状態キー  NOT =  "00" )
015060            OPEN OUTPUT  作業ファイル４
                  CLOSE 作業ファイル４
015060            OPEN INPUT  作業ファイル４
               END-IF.
006860     OPEN INPUT 制御情報マスタ.
006870             MOVE NC"制御" TO ファイル名.
006880             PERFORM オープンチェック.
007091*
007094* フロッピー挿入確認
007095* ダミーファイルをオープンし、オープンできたらファイルを削除
           IF 連入－固定フラグ = 1
002890        MOVE "C:\makishisys\REZEPT1.json" TO FD-NAME
           ELSE
003324        MOVE 連入－ドライブ TO ドライブＷＲ
007096        STRING ドライブＷＲ       DELIMITED BY SIZE
007097               ":\REZEPT1.json"    DELIMITED BY SIZE
007098               INTO FD-NAME
007099        END-STRING
           END-IF.
007100
007101     OPEN OUTPUT 会提出ファイル.
007102     IF 状態キー  =  "30"
007103         MOVE  NC"ドライブがありません。" TO 連メ－メッセージ
007104         CALL   "MSG001"
007105         CANCEL "MSG001"
007106         MOVE 99 TO PROGRAM-STATUS
007107         EXIT PROGRAM
007108     ELSE
007070         MOVE NC"会提出" TO ファイル名
007109         PERFORM オープンチェック
007110         CLOSE 会提出ファイル
007111         CALL "delfile"  WITH C LINKAGE
007112                         USING BY REFERENCE FD-NAME
007113     END-IF.
007160**================================================================*
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
007270**================================================================*
007280 ファイル閉鎖 SECTION.
007290*
           CLOSE 受診者情報Ｆ 施術所情報マスタ レセプトＦ 元号マスタ 負傷データＦ 
           施術記録Ｆ 負傷原因Ｆ 名称マスタ 長期継続者Ｆ 計算マスタ 摘要ファイル 
           保険者マスタ 市町村マスタ ＩＤ管理マスタ 制御情報マスタ 作業ファイル４.
007340**================================================================*
007350 終了処理 SECTION.
007360*
007370     PERFORM ファイル閉鎖.
007380**================================================================*
007390 エラー表示 SECTION.
007400*
007410     DISPLAY NC"状態キー" 状態キー  UPON CONS.
007420     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
007430     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
007440     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
007450     ACCEPT  キー入力 FROM CONS.
007460     PERFORM ファイル閉鎖.
007470     MOVE 99 TO PROGRAM-STATUS.
007480     EXIT PROGRAM.
009500**================================================================*
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
009620     END-READ.
009630*
009810**================================================================*
009820 連結項目退避 SECTION.
009830*
009840     MOVE 連入－請求和暦 TO 請求和暦ＷＲ.
009850     MOVE 連入－請求年   TO 請求年ＷＲ.
009860     MOVE 連入－請求月   TO 請求月ＷＲ.
009860     MOVE 連入－ドライブ TO ドライブＷＲ.
030740**================================================================*
030750 開始日取得 SECTION.
030760*
030830     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 負－部位数
030840         IF ( 受－施術年 = 負－開始年(部位ＣＮＴ) ) AND
030850            ( 受－施術月 = 負－開始月(部位ＣＮＴ) )
030860             MOVE 受－患者番号              TO 施記－患者番号
030870             MOVE 受－枝番                  TO 施記－枝番
030890             MOVE 負－開始和暦(部位ＣＮＴ)  TO 開始和暦Ｗ(部位ＣＮＴ) 施記－施術和暦
030890             MOVE 負－開始年(部位ＣＮＴ)  TO 開始年Ｗ(部位ＣＮＴ) 施記－施術年
030900             MOVE 負－開始月(部位ＣＮＴ)  TO 開始月Ｗ(部位ＣＮＴ) 施記－施術月
030910             MOVE 負－開始日(部位ＣＮＴ)  TO 開始日Ｗ(部位ＣＮＴ) 施記－施術日
030920         ELSE
030930             MOVE 受－患者番号          TO 施記－患者番号
030940             MOVE 受－枝番              TO 施記－枝番
030950             MOVE 受－施術和暦          TO 施記－施術和暦
030960             MOVE 受－施術年            TO 施記－施術年
030970             MOVE 受－施術月            TO 施記－施術月
030980             MOVE ZERO                  TO 施記－施術日
030990         END-IF
031000         START 施術記録Ｆ   KEY IS >= 施記－患者コード
031010                                      施記－施術和暦年月日
031020         END-START
031030         IF ( 状態キー = "00" )
031490             MOVE ZERO  TO 終了和暦ＷＴ
031050             MOVE ZERO  TO 終了年ＷＴ
031060             MOVE ZERO  TO 終了月ＷＴ
031070             MOVE ZERO  TO 終了日ＷＴ

      */終了日に通院が設定されて無い場合に転記されない為先に転記する。
                   IF 負－転帰区分(部位ＣＮＴ) NOT = ZERO
031490                 MOVE 負－終了和暦(部位ＣＮＴ) TO 終了和暦ＷＴ
031050                 MOVE 負－終了年  (部位ＣＮＴ) TO 終了年ＷＴ
031060                 MOVE 負－終了月  (部位ＣＮＴ) TO 終了月ＷＴ
031070                 MOVE 負－終了日  (部位ＣＮＴ) TO 終了日ＷＴ
                   END-IF


031080             MOVE SPACE TO 終了フラグ２
031090             PERFORM 施術記録Ｆ読込
031100             IF ( 終了フラグ２      = SPACE   ) AND
031110                ( 施記－患者コード  = 受－患者コード ) AND
031120                ( 施記－施術和暦    = 受－施術和暦   ) AND
031130                ( 施記－施術年      = 受－施術年     ) AND
031140                ( 施記－施術月      = 受－施術月     ) 
031150*
031160*        *****************************************************************
031170*        * 開始年月日 ( その部位が当月初検でないか、
031180*                       当月初検でも枝番がある時は、最初の施術日を開始日)*
031190*        *****************************************************************
031200                 IF ( 受－施術年 NOT = 負－開始年(部位ＣＮＴ) ) OR
031210                    ( 受－施術月 NOT = 負－開始月(部位ＣＮＴ) )
      */20240322
031220                 OR ( 負－開始診療日手動区分 = 1 )
031230                     MOVE 施記－施術和暦 TO 開始和暦Ｗ(部位ＣＮＴ)
031230                     MOVE 施記－施術年   TO 開始年Ｗ(部位ＣＮＴ)
031240                     MOVE 施記－施術月   TO 開始月Ｗ(部位ＣＮＴ)
031250                     MOVE 施記－施術日   TO 開始日Ｗ(部位ＣＮＴ)
031260                 END-IF
031270             END-IF
                   IF 負－終了日(部位ＣＮＴ) = ZERO
                       MOVE 99                     TO 終了日ＷＲ
                   ELSE
                       MOVE 負－終了日(部位ＣＮＴ) TO 終了日ＷＲ
                   END-IF
031280             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
031290                           ( 施記－患者コード NOT = 受－患者コード   ) OR
031300                           ( 施記－施術和暦   NOT = 受－施術和暦     ) OR
031310                           ( 施記－施術年     NOT = 受－施術年       ) OR
031320                           ( 施記－施術月     NOT = 受－施術月       ) OR
031330                           ( 施記－施術日         > 終了日ＷＲ)
031380                 MOVE 施記－施術和暦             TO 終了和暦ＷＴ
031380                 MOVE 施記－施術年               TO 終了年ＷＴ
031390                 MOVE 施記－施術月               TO 終了月ＷＴ
031400                 MOVE 施記－施術日               TO 終了日ＷＴ
031410*
031420                 PERFORM 施術記録Ｆ読込
031430             END-PERFORM
031440         END-IF
031450*       **************************
031460*       * 継続：終了年月日セット *
031470*       **************************
031490         MOVE 終了和暦ＷＴ  TO 終了和暦Ｗ(部位ＣＮＴ)
031490         MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
031500         MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
031510         MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
031580     END-PERFORM.
016650**================================================================*
016660 施術記録Ｆ読込 SECTION.
016670*
016680     READ 施術記録Ｆ NEXT
016690     AT END
016700         MOVE "YES" TO 終了フラグ２
016710     END-READ.
022430**================================================================*
022440 遅延処理 SECTION.
022450*
022460     PERFORM VARYING 遅延カウンタ FROM 1 BY 1
022470             UNTIL 遅延カウンタ > 遅延回数Ｗ
022480         MOVE "YES" TO 遅延フラグ
022490     END-PERFORM.
022500*
022510**================================================================*
022520 制御情報取得 SECTION.
022530*
022540     MOVE ZERO TO 制－制御区分
022550     READ 制御情報マスタ
022560     NOT INVALID KEY
022570         MOVE 制－遅延回数 TO 遅延回数Ｗ
022580     END-READ.
022841*
           MOVE 01   TO 制０１－制御区分.
           READ 制御情報マスタ
           NOT INVALID KEY
               MOVE 制０１－明細発行加算区分 TO 明細発行加算区分Ｗ
           END-READ.
      *
012420*================================================================*
011330 作業ファイル１作成 SECTION.
011340*
011344     OPEN OUTPUT 作業ファイル１.
011345         MOVE NC"作１" TO ファイル名.
011346         PERFORM オープンチェック.
011344     OPEN OUTPUT 作業ファイル２.
011345         MOVE NC"作２" TO ファイル名.
011346         PERFORM オープンチェック.
           CLOSE 作業ファイル１.
           CLOSE 作業ファイル２.
026580     OPEN I-O 作業ファイル１.
026590         MOVE NC"作１" TO ファイル名.
026600         PERFORM オープンチェック.
026580     OPEN I-O 作業ファイル２.
026590         MOVE NC"作２" TO ファイル名.
026600         PERFORM オープンチェック.
026710*
           MOVE ZERO TO 施術開始年月日Ｗ.
      *
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
                   IF レセ－レセ種別 = 1 OR 2 OR 3 OR 7
026920                 PERFORM データチェック
026930                 IF 実行キーＷ = "YES"
005930*/ 助成で請求額０円は対象外にする /170411 なし
006880*                    IF (レセ－レセ種別 = 3) AND (レセ－助成請求金額 = ZERO)
      *                       CONTINUE
      *                    ELSE
                             PERFORM 作１レコードセット
                             PERFORM 作１ファイル書込
      *                    END-IF
                       END-IF
005200             END-IF
027850             PERFORM レセプトＦ読込
027860         END-PERFORM
027870     END-IF.
027880     CLOSE 作業ファイル１ 作業ファイル２.
012420*================================================================*
012910 レセプトＦ読込 SECTION.
012920*
012930     READ レセプトＦ NEXT
012940     AT END
012950         MOVE "YES" TO 終了フラグ
012960     END-READ.
012970*
012980*================================================================*
       作１レコードセット SECTION.
      *
           INITIALIZE 作１－レコード.
           PERFORM レセプト並び順取得.
007520     PERFORM 施術所情報取得.
022340     PERFORM 負傷データ取得.
           PERFORM 長期継続者Ｆ読込.
      */開始日終了日取得
           PERFORM 開始日取得.
           PERFORM 施術記録判定.
           PERFORM 計算情報取得.
      *
032020     MOVE 作４－順番                     TO 作１－レセプト番号.
000520     MOVE 受－施術和暦年月               TO 作１－施術和暦年月.
000570     MOVE 受－患者コード                 TO 作１－患者コード.
000510     MOVE レセ－レセ種別                 TO 作１－レセ種別.
           MOVE 施情－新柔整師番号(3:11)       TO 作１－施術者番号.
010240* 西暦請求年月の取得
010250     MOVE ZERO          TO 西暦年月Ｗ  西暦請求年月Ｗ.
010260     MOVE 請求和暦ＷＲ  TO 元－元号区分.
010270     READ 元号マスタ
           INVALID KEY
010330          MOVE  NC"元号マスタの読み込みに失敗しました。" TO 連メ－メッセージ
010340          CALL   "MSG001"
010350          CANCEL "MSG001"
                DISPLAY 受－患者コード " " 受－施術和暦年月 " 元号区分:" 元－元号区分
010360          PERFORM ファイル閉鎖
010370          MOVE 99 TO PROGRAM-STATUS
010380          EXIT PROGRAM
010280     NOT INVALID KEY
010290         MOVE 元－開始西暦年 TO 西暦年Ｗ
010300     END-READ.
010310*
010320     IF 西暦年Ｗ = ZERO
010330          MOVE  NC"元号マスタに開始西暦年を登録して下さい" TO 連メ－メッセージ
010340          CALL   "MSG001"
010350          CANCEL "MSG001"
010360          PERFORM ファイル閉鎖
010370          MOVE 99 TO PROGRAM-STATUS
010380          EXIT PROGRAM
010390     ELSE
010400          COMPUTE 西暦年Ｗ = 西暦年Ｗ + 請求年ＷＲ - 1
010410          MOVE 請求月ＷＲ TO 西暦月Ｗ
010420     END-IF.
010430*
010440*     MOVE 西暦年月Ｗ   TO  西暦請求年月Ｗ.
010450*
      */施術年月を転記する↓↓↓/20190817
010440*     MOVE 西暦年月Ｗ    TO  作１－施術西暦年月.
           MOVE 受－施術和暦年月 TO 和暦計算年月日Ｗ
           PERFORM 西暦年月日取得
           MOVE 西暦計算年月Ｗ   TO  作１－施術西暦年月.
      */施術年月を転記する↑↑↑/20190817
           IF レセ－レセ種別 = 3
               MOVE 3                         TO 作１－提出先区分
           ELSE
               MOVE 1                         TO 作１－提出先区分
           END-IF.
           MOVE 受－保険者番号                TO 作１－保険者番号.
013031*-----------------------------------------------------------------*
013032     MOVE SPACE TO 連暗号複合－暗号情報.
013033*
013034*    / 連暗号複合－入力情報セット /
013035     MOVE 受－記号       TO 連暗号複合－記号.
013036     MOVE 受－番号       TO 連暗号複合－番号.
013037     MOVE 受－暗号化項目 TO 連暗号複合－暗号化項目.
013038*
013039     CALL   複合プログラム名Ｗ.
013040     CANCEL 複合プログラム名Ｗ.
013041*
013042*-----------------------------------------------------------------*
013296*/保険証記号番号
013297     IF 連暗号複合－複合した記号(1:2) NOT = "＊"
013300         MOVE 連暗号複合－複合した記号               TO 作１－保険証記号
013310     END-IF.
013320     IF (連暗号複合－複合した番号(1:1) NOT = "*") AND (連暗号複合－複合した番号(1:2) NOT = "＊")
013330         MOVE 連暗号複合－複合した番号               TO 作１－保険証番号
013340     END-IF.
001110     MOVE 受－費用負担者番号助成        TO 作１－公費負担者番号１
001180     MOVE 受－受益者番号助成            TO 作１－公費受給者番号１
      *          作１－公費負担者番号２       
      *          作１－公費受給者番号２       
           EVALUATE 受－保険種別
           WHEN 01
               MOVE "4"                       TO 作１－保険種別大区分
           WHEN 02
           WHEN 06
           WHEN 07
               MOVE "1"                       TO 作１－保険種別大区分
           WHEN 03
               MOVE "2"                       TO 作１－保険種別大区分
           WHEN 04
           WHEN 09
               MOVE "3"                       TO 作１－保険種別大区分
           WHEN 05
               MOVE "6"                       TO 作１－保険種別大区分
           WHEN 08
               MOVE "5"                       TO 作１－保険種別大区分
           WHEN OTHER
               MOVE "9"                       TO 作１－保険種別大区分
           END-EVALUATE.
           IF 受－助成種別 = ZERO
               MOVE "1"                       TO 作１－単併区分
           ELSE
               MOVE "2"                       TO 作１－単併区分
           END-IF
      */本家区分はどれか１つに○をする。
           IF 受－保険種別 = 05
               EVALUATE 受－特別区分
               WHEN 1
               WHEN 2
      *             MOVE NC"○" TO 高一チェックＷ
                   MOVE "8"      TO 作１－本家区分 
               WHEN 3
      *             MOVE NC"○" TO 高７チェックＷ
                   MOVE "0"      TO 作１－本家区分 
               END-EVALUATE
           ELSE
028984         EVALUATE 受－特別区分
               WHEN 1
               WHEN 2
      *             MOVE NC"○" TO 高一チェックＷ
                   MOVE "8"      TO 作１－本家区分 
               WHEN 3
      *             MOVE NC"○" TO 高７チェックＷ
                   MOVE "0"      TO 作１－本家区分 
028991         WHEN 6
      *             MOVE NC"○" TO ６歳チェックＷ
                   MOVE "4"      TO 作１－本家区分 
               WHEN OTHER
                   IF 受－本人家族区分 = 1
      *                 MOVE NC"○" TO 本人チェックＷ
                       MOVE "2"      TO 作１－本家区分 
                   ELSE
      *                 MOVE NC"○" TO 家族チェックＷ
                       MOVE "6"      TO 作１－本家区分 
                   END-IF
028999         END-EVALUATE
           END-IF.
           MOVE レセ－給付割合 TO 作１－給付割合.
           IF 受－本人家族区分 = 1
               MOVE "1" TO 作１－続柄
           ELSE
               MOVE "2" TO 作１－続柄
           END-IF.
001680     MOVE 受－被保険者カナ              TO 作１－被保険者カナ.
001670     MOVE 受－被保険者氏名              TO 作１－被保険者名  .
000870     MOVE 受－患者カナ                  TO 作１－受療者名カナ.
000860     MOVE 受－患者氏名                  TO 作１－受療者名    .
000880     IF 受－患者性別 = 1
               MOVE "1"                       TO 作１－受療者性別
           ELSE
               MOVE "2"                       TO 作１－受療者性別
           END-IF.
010240* 西暦年月の取得
           MOVE 受－患者生年月日              TO 和暦計算年月日Ｗ.
           PERFORM 西暦年月日取得.
           MOVE 西暦計算年月日Ｗ              TO 作１－受療者生年月日.
001750     MOVE 受－郵便番号                  TO 作１－被保険者郵便番号.
022720     STRING 受－住所１ DELIMITED BY SPACE
022730            受－住所２ DELIMITED BY SPACE
022750            INTO 作１－被保険者住所
022760     END-STRING.
013173     MOVE レセ－合計             TO 作１－合計金額. 
013174     MOVE レセ－一部負担金       TO 作１－一部負担金.
013175     MOVE レセ－請求金額         TO 作１－請求金額  .
013174     MOVE レセ－受給者負担額     TO 作１－公費一部負担金相当額.
013175     MOVE レセ－助成請求金額     TO 作１－公費請求金額.
000730     MOVE レセ－レセ実日数       TO 作１－総実日数.
000310     MOVE 負－部位数             TO 作１－部位数.
000550     IF レセ－請求区分 = ZERO
              MOVE "0"                 TO 作１－再請求区分
           ELSE
              MOVE "1"                 TO 作１－再請求区分
           END-IF.
           IF 受－保険種別 = 05
               MOVE "0" TO 作１－負担区分
           ELSE
               EVALUATE 受－特別区分
               WHEN 1
               WHEN 2
               WHEN 3
                   MOVE "1" TO 作１－負担区分
               WHEN 6
                   MOVE "6" TO 作１－負担区分
               END-EVALUATE
           END-IF.
000790     MOVE レセ－負担割合        TO 作１－負担割合.
000480     MOVE レセ－患者番号        TO 作１－患者番号２.
           MOVE "0"                   TO 作１－患者番号枝番.
           PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL
              (部位ＣＮＴ > 負－部位数) OR (部位ＣＮＴ > 6)
               MOVE 部位ＣＮＴ TO 作１－負傷ナンバー(部位ＣＮＴ)
000340         EVALUATE 負－負傷種別(部位ＣＮＴ)
               WHEN 01
                   MOVE "5" TO 作１－負傷区分(部位ＣＮＴ)
               WHEN 02
                   MOVE "4" TO 作１－負傷区分(部位ＣＮＴ)
               WHEN 03
                   MOVE "6" TO 作１－負傷区分(部位ＣＮＴ)
               WHEN 04
                   MOVE "3" TO 作１－負傷区分(部位ＣＮＴ)
               WHEN 05
                   MOVE "1" TO 作１－負傷区分(部位ＣＮＴ)
               WHEN 06
                   MOVE "2" TO 作１－負傷区分(部位ＣＮＴ)
               WHEN 07
               WHEN 08
                   MOVE "7" TO 作１－負傷区分(部位ＣＮＴ)
               WHEN 09
                   MOVE "0" TO 作１－負傷区分(部位ＣＮＴ)
               END-EVALUATE
               MOVE SPACE TO 作１－負傷コード(部位ＣＮＴ)
022530* 負傷種別
022540         MOVE SPACE                     TO 負傷名称Ｗ
022550         MOVE 03                        TO 名－区分コード
022560         MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
022570         READ 名称マスタ
022580         INVALID KEY
022590             MOVE SPACE        TO 負傷名称Ｗ
022600         NOT INVALID KEY
022610             MOVE 名－正式名称 TO 負傷名称Ｗ
022620         END-READ
022630* 部位
022720         STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
022730                負傷名称Ｗ                    DELIMITED BY SPACE
022740                レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
022750                INTO 負傷名Ｗ(部位ＣＮＴ)
022760         END-STRING
022780*
022750         MOVE 負傷名ＷＲ(部位ＣＮＴ) TO 作１－負傷名(部位ＣＮＴ)
               INSPECT 作１－負傷名(部位ＣＮＴ)   REPLACING ALL "　" BY "  "
HILO  *         DISPLAY 受－レコードキー " 作１－負傷名(" 部位ＣＮＴ ") " 作１－負傷名(部位ＣＮＴ)
      */負傷日
               MOVE 負－負傷和暦年月日(部位ＣＮＴ)   TO 和暦計算年月日Ｗ
               PERFORM 西暦年月日取得
               MOVE 西暦計算年月日Ｗ                 TO 作１－負傷年月日(部位ＣＮＴ)
      */柔の開始日
               MOVE 負－開始和暦年月日(部位ＣＮＴ)   TO 和暦計算年月日Ｗ
               PERFORM 西暦年月日取得
               MOVE 西暦計算年月日Ｗ                 TO 作１－初検年月日(部位ＣＮＴ)
      */当月の開始日
               MOVE 開始和暦年月日Ｗ(部位ＣＮＴ)     TO 和暦計算年月日Ｗ
               PERFORM 西暦年月日取得
               MOVE 西暦計算年月日Ｗ                 TO 作１－施術開始日(部位ＣＮＴ)
      */終了日または最終通院日
                  MOVE 終了和暦年月日Ｗ(部位ＣＮＴ)  TO 和暦計算年月日Ｗ
                  PERFORM 西暦年月日取得
                  MOVE 西暦計算年月日Ｗ              TO 作１－施術終了日(部位ＣＮＴ)
HILO  *         DISPLAY "(" 部位ＣＮＴ ") 負傷" 作１－負傷年月日(部位ＣＮＴ) " 初検" 作１－初検年月日(部位ＣＮＴ) 
HILO  *                                 " 開始" 作１－施術開始日(部位ＣＮＴ) " 終了" 作１－施術終了日(部位ＣＮＴ)
000690          MOVE レセ－部位実日数(部位ＣＮＴ)    TO 作１－実日数(部位ＣＮＴ)
               EVALUATE 負－転帰区分(部位ＣＮＴ)
               WHEN 1
               WHEN 2
               WHEN 5
                   MOVE "1" TO 作１－転帰区分(部位ＣＮＴ)
               WHEN 3
                   MOVE "2" TO 作１－転帰区分(部位ＣＮＴ)
               WHEN 4
                   MOVE "3" TO 作１－転帰区分(部位ＣＮＴ)
               WHEN 9
                   MOVE "0" TO 作１－転帰区分(部位ＣＮＴ)
               END-EVALUATE
      */整復料（骨折・脱臼）固定料（不全骨折）施療料（打撲、捻挫・挫傷）
               IF 負－算定区分(部位ＣＮＴ) = ZERO
                   EVALUATE 負－負傷種別(部位ＣＮＴ)
                   WHEN 01
                   WHEN 02
                   WHEN 03
                       MOVE "3" TO 作１－整固施区分(部位ＣＮＴ)
                   WHEN 04
                   WHEN 05
                       MOVE "1" TO 作１－整固施区分(部位ＣＮＴ)
                   WHEN 06
                       MOVE "2" TO 作１－整固施区分(部位ＣＮＴ)
                   WHEN 07
                   WHEN 08
                   WHEN 09
                       MOVE "0" TO 作１－整固施区分(部位ＣＮＴ)
                   END-EVALUATE
               ELSE
                   MOVE "0" TO 作１－整固施区分(部位ＣＮＴ)
               END-IF
               MOVE 初回処置回数Ｗ(部位ＣＮＴ) TO 作１－整固施回数(部位ＣＮＴ)
HILO  *         DISPLAY "作１－整固施回数(" 部位ＣＮＴ ")" 作１－整固施回数(部位ＣＮＴ)
               MOVE レセ－初回処置料(部位ＣＮＴ) TO 作１－整固施療料(部位ＣＮＴ)
      */負傷原因
               MOVE 01                             TO 負原－区分コード
               MOVE 負－負傷原因コード(部位ＣＮＴ) TO 負原－負傷原因コード
               READ 負傷原因Ｆ
               NOT INVALID KEY
                   MOVE SPACE TO 空白詰Ｗ
                   STRING 負原－負傷原因ＣＭ(1) DELIMITED BY SIZE
                          負原－負傷原因ＣＭ(2) DELIMITED BY SIZE
                          負原－負傷原因ＣＭ(3) DELIMITED BY SIZE
                          負原－負傷原因ＣＭ(4) DELIMITED BY SIZE
                          負原－負傷原因ＣＭ(5) DELIMITED BY SIZE
                     INTO 空白詰Ｗ
                   END-STRING
                   PERFORM 空白詰処理
                   MOVE 空白詰Ｗ TO 作１－負傷原因(部位ＣＮＴ)
               END-READ
      */長期理由
               MOVE 長継－理由文(部位ＣＮＴ)       TO  作１－長期理由(部位ＣＮＴ)
               MOVE "0" TO 作１－審査区分(部位ＣＮＴ)
           END-PERFORM.
000720     EVALUATE レセ－レセ請求区分
           WHEN 1
               MOVE "1" TO 作１－請求区分
           WHEN 2
               MOVE "2" TO 作１－請求区分
           WHEN 3
               MOVE "3" TO 作１－請求区分
           END-EVALUATE.
           MOVE 初検回数Ｗ                   TO 作１－初検回数.
           MOVE レセ－初検料                 TO 作１－初検料.
           MOVE 休日回数Ｗ                   TO 作１－初検休日加算回数  
           MOVE 深夜回数Ｗ                   TO 作１－初検深夜加算回数  
           MOVE 時間外回数Ｗ                 TO 作１－初検時間外加算回数
           MOVE レセ－初検加算料             TO 作１－初検加算
           MOVE 相談支援回数Ｗ               TO 作１－初検時相談支援料回数
           MOVE レセ－初検時相談料           TO 作１－初検時相談支援料
           MOVE 再検回数Ｗ                   TO 作１－再検回数.
           MOVE レセ－再検料                 TO 作１－再検料
           COMPUTE 作１－往療距離 = レセ－往療距離 * 10
           MOVE 往療回数Ｗ                   TO 作１－往療回数.
           MOVE レセ－往療料                 TO 作１－往療料
           MOVE 夜間回数Ｗ                   TO 作１－夜間加算の往療回数    
           MOVE 難路回数Ｗ                   TO 作１－難路加算の往療回数    
           MOVE 暴風雨雪回数Ｗ               TO 作１－暴風雨雪加算の往療回数
           MOVE レセ－往療加算料             TO 作１－往療加算
           MOVE 大回数Ｗ                     TO 作１－金属副子大回数
           MOVE 中回数Ｗ                     TO 作１－金属副子中回数
           MOVE 小回数Ｗ                     TO 作１－金属副子小回数
           MOVE レセ－金属副子加算料         TO 作１－金属副子加算
           MOVE 情報提供回数Ｗ               TO 作１－施術情報提供料の回数
           MOVE レセ－施術情報提供料         TO 作１－施術情報提供料      
           MOVE レセ－運動後療回数           TO 作１－運動後療実施回数    
           MOVE レセ－運動後療料             TO 作１－運動後療料          
           PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 5
               MOVE レセ－運動算定日(部位ＣＮＴ) TO 作１－運動後療実施日(部位ＣＮＴ)
           END-PERFORM
      *
           IF レセ－長期込小計１ NOT = ZERO
           OR レセ－部位名称１(1) NOT = SPACE
               MOVE 1                       TO 作１－行番号            (1)
               MOVE SPACE                   TO 作１－逓減開始月日      (1)
               MOVE レセ－後療回数１        TO 作１－後療回数          (1)
               MOVE レセ－後療料１          TO 作１－後療料            (1)
               MOVE レセ－冷罨法回数１      TO 作１－冷罨法回数        (1)
               MOVE レセ－冷罨法料１        TO 作１－冷罨法料          (1)
               MOVE レセ－温罨法回数１      TO 作１－温罨法回数        (1)
               MOVE レセ－温罨法料１        TO 作１－温罨法料          (1)
               MOVE レセ－電療回数１        TO 作１－電療回数          (1)
               MOVE レセ－電療料１          TO 作１－電療料            (1)
               MOVE ZERO                    TO 作１－多部位逓減率      (1)
               COMPUTE 作１－多部位逓減額(1) = レセ－後療料１   + レセ－冷罨法料１
                                             + レセ－温罨法料１ + レセ－電療料１
               MOVE レセ－長期逓減率１      TO 作１－長期逓減率        (1)
               MOVE レセ－長期込小計１      TO 作１－部位逓減率別料金計(1)
           END-IF.
      *
           IF レセ－長期込小計２ NOT = ZERO
           OR レセ－部位名称１(2) NOT = SPACE
               MOVE 2                       TO 作１－行番号            (2)
               MOVE SPACE                   TO 作１－逓減開始月日      (2)
               MOVE レセ－後療回数２        TO 作１－後療回数          (2)
               MOVE レセ－後療料２          TO 作１－後療料            (2)
               MOVE レセ－冷罨法回数２      TO 作１－冷罨法回数        (2)
               MOVE レセ－冷罨法料２        TO 作１－冷罨法料          (2)
               MOVE レセ－温罨法回数２      TO 作１－温罨法回数        (2)
               MOVE レセ－温罨法料２        TO 作１－温罨法料          (2)
               MOVE レセ－電療回数２        TO 作１－電療回数          (2)
               MOVE レセ－電療料２          TO 作１－電療料            (2)
               MOVE ZERO                    TO 作１－多部位逓減率      (2)
               COMPUTE 作１－多部位逓減額(2) = レセ－後療料２   + レセ－冷罨法料２
                                             + レセ－温罨法料２ + レセ－電療料２
               MOVE レセ－長期逓減率２      TO 作１－長期逓減率        (2)
               MOVE レセ－長期込小計２      TO 作１－部位逓減率別料金計(2)
           END-IF.
      *
           IF レセ－長期込小計３８ NOT = ZERO
               MOVE 3                       TO 作１－行番号            (3)
               MOVE SPACE                   TO 作１－逓減開始月日      (3)
               MOVE レセ－後療回数３８      TO 作１－後療回数          (3)
               MOVE レセ－後療料３８        TO 作１－後療料            (3)
               MOVE レセ－冷罨法回数３８    TO 作１－冷罨法回数        (3)
               MOVE レセ－冷罨法料３８      TO 作１－冷罨法料          (3)
               MOVE レセ－温罨法回数３８    TO 作１－温罨法回数        (3)
               MOVE レセ－温罨法料３８      TO 作１－温罨法料          (3)
               MOVE レセ－電療回数３８      TO 作１－電療回数          (3)
               MOVE レセ－電療料３８        TO 作１－電療料            (3)
               MOVE 多部位率３ＷＲ          TO 作１－多部位逓減率      (3)
               COMPUTE 作１－多部位逓減額(3) = レセ－後療料３８   + レセ－冷罨法料３８
                                             + レセ－温罨法料３８ + レセ－電療料３８
               MOVE レセ－長期逓減率３８    TO 作１－長期逓減率        (3)
               MOVE レセ－長期込小計３８    TO 作１－部位逓減率別料金計(3)
           END-IF.
      *
           IF レセ－長期込小計３０ NOT = ZERO
               MOVE 4                       TO 作１－行番号            (4)
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
               COMPUTE 作１－多部位逓減額(4) = レセ－後療料３０   + レセ－冷罨法料３０
                                             + レセ－温罨法料３０ + レセ－電療料３０
               MOVE レセ－長期逓減率３０    TO 作１－長期逓減率        (4)
               MOVE レセ－長期込小計３０    TO 作１－部位逓減率別料金計(4)
           END-IF.
      *
           IF レセ－長期込小計４８ NOT = ZERO
               MOVE 5                       TO 作１－行番号            (5)
               MOVE レセ－逓減開始月日４８  TO 作１－逓減開始月日      (5)
               MOVE レセ－後療回数４８      TO 作１－後療回数          (5)
               MOVE レセ－後療料４８        TO 作１－後療料            (5)
               MOVE レセ－冷罨法回数４８    TO 作１－冷罨法回数        (5)
               MOVE レセ－冷罨法料４８      TO 作１－冷罨法料          (5)
               MOVE レセ－温罨法回数４８    TO 作１－温罨法回数        (5)
               MOVE レセ－温罨法料４８      TO 作１－温罨法料          (5)
               MOVE レセ－電療回数４８      TO 作１－電療回数          (5)
               MOVE レセ－電療料４８        TO 作１－電療料            (5)
               MOVE 多部位率３ＷＲ          TO 作１－多部位逓減率      (5)
               COMPUTE 作１－多部位逓減額(5) = レセ－後療料４８   + レセ－冷罨法料４８
                                             + レセ－温罨法料４８ + レセ－電療料４８
               MOVE レセ－長期逓減率４８    TO 作１－長期逓減率        (5)
               MOVE レセ－長期込小計４８    TO 作１－部位逓減率別料金計(5)
           END-IF.
      *
           IF レセ－長期込小計４０ NOT = ZERO
               MOVE 6                       TO 作１－行番号            (6)
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
               COMPUTE 作１－多部位逓減額(6) = レセ－後療料４０   + レセ－冷罨法料４０
                                             + レセ－温罨法料４０ + レセ－電療料４０
               MOVE レセ－長期逓減率４０    TO 作１－長期逓減率        (6)
               MOVE レセ－長期込小計４０    TO 作１－部位逓減率別料金計(6)
           END-IF.
      *
           IF レセ－長期込小計５８ NOT = ZERO
               MOVE 7                       TO 作１－行番号            (7)
               MOVE レセ－逓減開始月日５８  TO 作１－逓減開始月日      (7)
               MOVE レセ－後療回数５８      TO 作１－後療回数          (7)
               MOVE レセ－後療料５８        TO 作１－後療料            (7)
               MOVE レセ－冷罨法回数５８    TO 作１－冷罨法回数        (7)
               MOVE レセ－冷罨法料５８      TO 作１－冷罨法料          (7)
               MOVE レセ－温罨法回数５８    TO 作１－温罨法回数        (7)
               MOVE レセ－温罨法料５８      TO 作１－温罨法料          (7)
               MOVE レセ－電療回数５８      TO 作１－電療回数          (7)
               MOVE レセ－電療料５８        TO 作１－電療料            (7)
               MOVE 多部位率３ＷＲ          TO 作１－多部位逓減率      (7)
               COMPUTE 作１－多部位逓減額(7) = レセ－後療料５８   + レセ－冷罨法料５８
                                             + レセ－温罨法料５８ + レセ－電療料５８
               MOVE レセ－長期逓減率５８    TO 作１－長期逓減率        (7)
               MOVE レセ－長期込小計５８    TO 作１－部位逓減率別料金計(7)
           END-IF.
      *
           IF レセ－長期込小計５０ NOT = ZERO
               MOVE 8                       TO 作１－行番号            (8)
               MOVE レセ－逓減開始月日５０  TO 作１－逓減開始月日      (8)
               MOVE レセ－後療回数５０      TO 作１－後療回数          (8)
               MOVE レセ－後療料５０        TO 作１－後療料            (8)
               MOVE レセ－冷罨法回数５０    TO 作１－冷罨法回数        (8)
               MOVE レセ－冷罨法料５０      TO 作１－冷罨法料          (8)
               MOVE レセ－温罨法回数５０    TO 作１－温罨法回数        (8)
               MOVE レセ－温罨法料５０      TO 作１－温罨法料          (8)
               MOVE レセ－電療回数５０      TO 作１－電療回数          (8)
               MOVE レセ－電療料５０        TO 作１－電療料            (8)
               MOVE ZERO                    TO 作１－多部位逓減率      (8)
               COMPUTE 作１－多部位逓減額(8) = レセ－後療料５０   + レセ－冷罨法料５０
                                             + レセ－温罨法料５０ + レセ－電療料５０
               MOVE レセ－長期逓減率５０    TO 作１－長期逓減率        (8)
               MOVE レセ－長期込小計５０    TO 作１－部位逓減率別料金計(8)
           END-IF.
      *
           IF レセ－長期込小計６８ NOT = ZERO
               MOVE 9                       TO 作１－行番号            (9)
               MOVE レセ－逓減開始月日６８  TO 作１－逓減開始月日      (9)
               MOVE レセ－後療回数６８      TO 作１－後療回数          (9)
               MOVE レセ－後療料６８        TO 作１－後療料            (9)
               MOVE レセ－冷罨法回数６８    TO 作１－冷罨法回数        (9)
               MOVE レセ－冷罨法料６８      TO 作１－冷罨法料          (9)
               MOVE レセ－温罨法回数６８    TO 作１－温罨法回数        (9)
               MOVE レセ－温罨法料６８      TO 作１－温罨法料          (9)
               MOVE レセ－電療回数６８      TO 作１－電療回数          (9)
               MOVE レセ－電療料６８        TO 作１－電療料            (9)
               MOVE 多部位率３ＷＲ          TO 作１－多部位逓減率      (9)
               COMPUTE 作１－多部位逓減額(9) = レセ－後療料６８   + レセ－冷罨法料６８
                                             + レセ－温罨法料６８ + レセ－電療料６８
               MOVE レセ－長期逓減率６８    TO 作１－長期逓減率        (9)
               MOVE レセ－長期込小計６８    TO 作１－部位逓減率別料金計(9)
           END-IF.
      *
           IF レセ－長期込小計６０ NOT = ZERO
      *         MOVE 10                      TO 作１－行番号            (10)
               MOVE レセ－逓減開始月日６０  TO 作１－逓減開始月日      (10)
               MOVE レセ－後療回数６０      TO 作１－後療回数          (10)
               MOVE レセ－後療料６０        TO 作１－後療料            (10)
               MOVE レセ－冷罨法回数６０    TO 作１－冷罨法回数        (10)
               MOVE レセ－冷罨法料６０      TO 作１－冷罨法料          (10)
               MOVE レセ－温罨法回数６０    TO 作１－温罨法回数        (10)
               MOVE レセ－温罨法料６０      TO 作１－温罨法料          (10)
               MOVE レセ－電療回数６０      TO 作１－電療回数          (10)
               MOVE レセ－電療料６０        TO 作１－電療料            (10)
               MOVE ZERO                    TO 作１－多部位逓減率      (10)
               COMPUTE 作１－多部位逓減額(10) = レセ－後療料６０   + レセ－冷罨法料６０
                                              + レセ－温罨法料６０ + レセ－電療料６０
               MOVE レセ－長期逓減率６０    TO 作１－長期逓減率        (10)
               MOVE レセ－長期込小計６０    TO 作１－部位逓減率別料金計(10)
           END-IF.
      *
004549     MOVE SPACE TO 摘要文Ｗ. 
           MOVE 1              TO 摘要－制御区分.
           MOVE 受－施術和暦   TO 摘要－施術和暦.
           MOVE 受－施術年     TO 摘要－施術年.
           MOVE 受－施術月     TO 摘要－施術月.
           MOVE 受－患者番号   TO 摘要－患者番号.
           MOVE 受－枝番       TO 摘要－枝番.
           READ 摘要ファイル
           NOT INVALID KEY
               IF 摘要－摘要 NOT = SPACE
004598*             MOVE 摘要－摘要  TO  摘要文Ｗ
      *             PERFORM 摘要結合処理
004598             MOVE 摘要－摘要  TO  作１－摘要
               END-IF
           END-READ.
           IF レセ－レセ種別 NOT = 3
               EVALUATE 受－保険種別
               WHEN 01
                   IF 受－保険者番号(3:1) = "3"
                       MOVE "01" TO 作１－保険種別詳細
                   ELSE
                       MOVE "00" TO 作１－保険種別詳細
                   END-IF
               WHEN 02
                   MOVE "03" TO 作１－保険種別詳細
               WHEN 03
                   MOVE "06" TO 作１－保険種別詳細
               WHEN 04
                   MOVE "07" TO 作１－保険種別詳細
               WHEN 05
                   MOVE "21" TO 作１－保険種別詳細
               WHEN 06
                   MOVE "04" TO 作１－保険種別詳細
               WHEN 07
                   MOVE "05" TO 作１－保険種別詳細
               WHEN 08
                   MOVE "02" TO 作１－保険種別詳細
               WHEN 09
                   MOVE "08" TO 作１－保険種別詳細
               END-EVALUATE
           ELSE
               EVALUATE 受－助成種別
               WHEN 50
                   MOVE "10" TO 作１－保険種別詳細
               WHEN 51
                   MOVE "12" TO 作１－保険種別詳細
               WHEN 52
                   MOVE "14" TO 作１－保険種別詳細
               WHEN 53
                   MOVE "13" TO 作１－保険種別詳細
               WHEN 54
                   MOVE "17" TO 作１－保険種別詳細
               WHEN 55
                   MOVE "15" TO 作１－保険種別詳細
               WHEN 60
                   MOVE "17" TO 作１－保険種別詳細
               END-EVALUATE
           END-IF.
      */項目追加↓↓↓/20221028
           MOVE レセ－明細書発行加算料 TO 作１－明細書発行加算料.
           MOVE レセ－明細書発行加算日 TO 作１－明細書発行加算日.
      */項目追加↑↑↑/20221028
027870     MOVE 受－保険者番号         TO 保険者番号Ｗ
027880     MOVE 受－保険種別           TO 保険種別Ｗ
027880     MOVE 受－公費種別           TO 公費種別Ｗ
004510     MOVE 受－助成種別           TO 助成種別ＷＲ
008010     MOVE 受－費用負担者番号助成 TO 市町村番号Ｗ
           EVALUATE レセ－レセ種別
           WHEN 2
               MOVE 受－公費種別           TO 作２－保険種別１
               MOVE 受－費用負担者番号     TO 作２－保険者番号
017220         READ 作業ファイル２
017230         INVALID KEY
017870             MOVE 受－公費種別       TO 市－公費種別  
017880             MOVE 受－費用負担者番号 TO 市－市町村番号
017890             READ 市町村マスタ
017900             INVALID KEY
017910                 CONTINUE
017920             NOT INVALID KEY
017910                 CONTINUE
                       MOVE 市－市町村名称      TO 作２－保険者名  
                       MOVE "21"                TO 作２－保険種別
                       MOVE 市－市町村番号(3:2) TO 作２－県コード
                       MOVE 市－郵便番号        TO 作２－郵便番号
                       MOVE 市－住所１          TO 作２－住所１  
                       MOVE 市－住所２          TO 作２－住所２  
                       MOVE 市－電話番号        TO 作２－電話番号
023691                 PERFORM 振込口座セット
                       MOVE 口座番号Ｗ          TO 作２－口座番号
019340                 WRITE 作２－レコード
019350                 INVALID KEY
019360                     MOVE NC"作２"  TO ファイル名
019370                     PERFORM エラー表示
019380                 END-WRITE
                   END-READ
               END-READ
           WHEN 3
               MOVE 受－助成種別           TO 作２－保険種別１
               MOVE 受－費用負担者番号助成 TO 作２－保険者番号
017220         READ 作業ファイル２
017230         INVALID KEY
017870             MOVE 受－助成種別           TO 市－公費種別  
017880             MOVE 受－費用負担者番号助成 TO 市－市町村番号
017890             READ 市町村マスタ
017900             INVALID KEY
017910                 CONTINUE
017920             NOT INVALID KEY
017910                 CONTINUE
                       MOVE 市－市町村名称 TO 作２－保険者名  
                       EVALUATE 受－助成種別
                       WHEN 50
                           MOVE "10" TO 作２－保険種別
                       WHEN 51
                           MOVE "12" TO 作２－保険種別
                       WHEN 52
                           MOVE "14" TO 作２－保険種別
                       WHEN 53
                           MOVE "13" TO 作２－保険種別
                       WHEN 54
                           MOVE "17" TO 作２－保険種別
                       WHEN 55
                           MOVE "15" TO 作２－保険種別
                       WHEN 60
                           MOVE "17" TO 作２－保険種別
                       END-EVALUATE
                       MOVE 市－市町村番号(3:2) TO 作２－県コード
                       MOVE 市－郵便番号        TO 作２－郵便番号
                       MOVE 市－住所１          TO 作２－住所１  
                       MOVE 市－住所２          TO 作２－住所２  
                       MOVE 市－電話番号        TO 作２－電話番号
023691                 PERFORM 振込口座セット助成
                       MOVE 口座番号Ｗ              TO 作２－口座番号
019340                 WRITE 作２－レコード
019350                 INVALID KEY
019360                     MOVE NC"作２"  TO ファイル名
019370                     PERFORM エラー表示
019380                 END-WRITE
                   END-READ
               END-READ
           WHEN OTHER
               MOVE 受－保険種別           TO 作２－保険種別１
               MOVE 受－保険者番号         TO 作２－保険者番号
017220         READ 作業ファイル２
017230         INVALID KEY
017200             MOVE 受－保険種別           TO 保－保険種別
017210             MOVE 受－保険者番号         TO 保－保険者番号
017220             READ 保険者マスタ
017900             INVALID KEY
017910                 CONTINUE
017230             NOT INVALID KEY
                       MOVE 保－保険者名称     TO 作２－保険者名
                       EVALUATE 保－保険種別
                       WHEN 01
                           IF 保－保険者番号(3:1) = "3"
                               MOVE "01" TO 作２－保険種別
                           ELSE
                               MOVE "00" TO 作２－保険種別
                           END-IF
                       WHEN 02
                           MOVE "03" TO 作２－保険種別
                       WHEN 03
                           MOVE "06" TO 作２－保険種別
                       WHEN 04
                           MOVE "07" TO 作２－保険種別
                       WHEN 05
                           MOVE "21" TO 作２－保険種別
                       WHEN 06
                           MOVE "04" TO 作２－保険種別
                       WHEN 07
                           MOVE "05" TO 作２－保険種別
                       WHEN 08
                           MOVE "02" TO 作２－保険種別
                       WHEN 09
                           MOVE "08" TO 作２－保険種別
                       END-EVALUATE
                       IF 保－保険種別 = 01
                           MOVE 保－保険者番号(1:2) TO 作２－県コード
                       ELSE
                           MOVE 保－保険者番号(3:2) TO 作２－県コード
                       END-IF
                       MOVE 保－郵便番号            TO 作２－郵便番号
                       MOVE 保－住所１              TO 作２－住所１
                       MOVE 保－住所２              TO 作２－住所２
                       MOVE 保－電話番号            TO 作２－電話番号
023691                 PERFORM 振込口座セット
                       MOVE 口座番号Ｗ              TO 作２－口座番号
019340                 WRITE 作２－レコード
019350                 INVALID KEY
019360                     MOVE NC"作２"  TO ファイル名
019370                     PERFORM エラー表示
019380                 END-WRITE
                   END-READ
               END-READ
           END-EVALUATE.
012900*================================================================*
       施術記録判定 SECTION.
      *
           INITIALIZE 部位毎情報Ｗ 回数カウンタＷ.
      *
030930     MOVE 受－患者番号          TO 施記－患者番号
030940     MOVE 受－枝番              TO 施記－枝番
030950     MOVE 受－施術和暦          TO 施記－施術和暦
030960     MOVE 受－施術年            TO 施記－施術年
030970     MOVE 受－施術月            TO 施記－施術月
030980     MOVE ZERO                  TO 施記－施術日
031000     START 施術記録Ｆ   KEY IS >= 施記－患者コード
031010                                  施記－施術和暦年月日
031020     END-START
031030     IF 状態キー = "00"
               MOVE SPACE TO 終了フラグ２
031090         PERFORM 施術記録Ｆ読込
031280         PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
031290                       ( 施記－患者コード NOT = 受－患者コード   ) OR
031300                       ( 施記－施術和暦   NOT = 受－施術和暦     ) OR
031310                       ( 施記－施術年     NOT = 受－施術年       ) OR
031320                       ( 施記－施術月     NOT = 受－施術月       )
      */日毎項目
015610             IF 施記－診療区分 = 2
015620                 COMPUTE 初検回数Ｗ = 初検回数Ｗ + 1
015630             END-IF
                   EVALUATE 施記－初検加算
                   WHEN 1
015620                 COMPUTE 時間外回数Ｗ = 時間外回数Ｗ + 1
                   WHEN 2
015620                 COMPUTE 休日回数Ｗ   = 休日回数Ｗ + 1
                   WHEN 3
015620                 COMPUTE 深夜回数Ｗ   = 深夜回数Ｗ + 1
                   END-EVALUATE
                   IF (施記－診療区分 = 2 ) AND (施記－初検時相談料区分 NOT = 1)
                       COMPUTE 相談支援回数Ｗ = 相談支援回数Ｗ + 1
                   END-IF
015610             IF 施記－再検料請求 = 1
015620                 COMPUTE 再検回数Ｗ = 再検回数Ｗ + 1
015630             END-IF
015610             IF 施記－往療距離 NOT = ZERO
015620                 COMPUTE 往療回数Ｗ = 往療回数Ｗ + 1
015630             END-IF
                   EVALUATE 施記－往療加算
                   WHEN 1
015620                 COMPUTE 夜間回数Ｗ       = 夜間回数Ｗ + 1
                   WHEN 2
015620                 COMPUTE 難路回数Ｗ       = 難路回数Ｗ + 1
                   WHEN 3
015620                 COMPUTE 暴風雨雪回数Ｗ   = 暴風雨雪回数Ｗ + 1
                   END-EVALUATE
      */日毎部位毎項目
                   PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL
                      (部位ＣＮＴ > 負－部位数) OR (部位ＣＮＴ > 6)
      */部位毎の施術日
                      IF (施記－施術和暦年月日 >= 負－開始和暦年月日(部位ＣＮＴ)) AND
                        ((施記－施術和暦年月日 <= 負－終了和暦年月日(部位ＣＮＴ)) OR 
                         (負－終了和暦年月日(部位ＣＮＴ) = ZERO))
                          COMPUTE 部位日ＣＮＴ(部位ＣＮＴ) = 部位日ＣＮＴ(部位ＣＮＴ) + 1
                          MOVE 施記－施術日 TO 作１－施術日((部位ＣＮＴ)(部位日ＣＮＴ(部位ＣＮＴ)))
                      END-IF
024670*            /　初回処置のカウント　/
024680                IF 施記－整復施療区分(部位ＣＮＴ) = 1
024690                    COMPUTE 初回処置回数Ｗ(部位ＣＮＴ) = 初回処置回数Ｗ(部位ＣＮＴ) + 1
024700                END-IF
031410*
                       EVALUATE 施記－金属副子区分(部位ＣＮＴ)
                       WHEN 1
015620                     COMPUTE 大回数Ｗ   = 大回数Ｗ + 1
                       WHEN 2
015620                     COMPUTE 中回数Ｗ   = 中回数Ｗ + 1
                       WHEN 3
015620                     COMPUTE 小回数Ｗ   = 小回数Ｗ + 1
                       END-EVALUATE
                       IF 施記－金属副子区分(部位ＣＮＴ) NOT = ZERO
                          COMPUTE 金属ＣＮＴ = 金属ＣＮＴ + 1
                          IF 金属ＣＮＴ <= 3
                              MOVE 施記－施術日 TO 作１－金属副子加算日(金属ＣＮＴ)
                          END-IF
                       END-IF
015610                 IF 施記－情報提供区分(部位ＣＮＴ) = 1
015620                     COMPUTE 情報提供回数Ｗ = 情報提供回数Ｗ + 1
015630                 END-IF
      *               */施術開始年月日
                       IF (施術開始年月日Ｗ > 施記－施術和暦年月日) OR
                          (施術開始年月日Ｗ = ZERO)
                           MOVE 施記－施術和暦年月日 TO 施術開始年月日Ｗ
                       END-IF
031430             END-PERFORM
031420             PERFORM 施術記録Ｆ読込
031430         END-PERFORM
           END-IF.
012900*================================================================*
022340 負傷データ取得 SECTION.
022350*
022360     INITIALIZE 負傷情報Ｗ.
022361*
022370     MOVE レセ－施術和暦   TO 負－施術和暦.
022380     MOVE レセ－施術年     TO 負－施術年.
022390     MOVE レセ－施術月     TO 負－施術月.
022400     MOVE レセ－患者コード TO 負－患者コード.
022410     READ 負傷データＦ
022420     INVALID KEY
022430         MOVE SPACE TO 負－レコード
022440     NOT INVALID KEY
022430         CONTINUE
           END-READ.
012980*================================================================*
       西暦年月日取得 SECTION.
      *
010250     MOVE ZERO               TO 西暦年月Ｗ  西暦請求年月Ｗ.
010260     MOVE 和暦計算和暦Ｗ     TO 元－元号区分.
010270     READ 元号マスタ
           INVALID KEY
010330          MOVE  NC"元号マスタの読み込みに失敗しました。" TO 連メ－メッセージ
010340          CALL   "MSG001"
010350          CANCEL "MSG001"
                DISPLAY 受－患者コード " " 受－施術和暦年月 " 元号区分:" 元－元号区分
010360          PERFORM ファイル閉鎖
010370          MOVE 99 TO PROGRAM-STATUS
010380          EXIT PROGRAM
010280     NOT INVALID KEY
010290         MOVE 元－開始西暦年 TO 西暦年Ｗ
010300     END-READ.
010310*
010320     IF 西暦年Ｗ = ZERO
010330          MOVE  NC"元号マスタに開始西暦年を登録して下さい" TO 連メ－メッセージ
010340          CALL   "MSG001"
010350          CANCEL "MSG001"
010360          PERFORM ファイル閉鎖
010370          MOVE 99 TO PROGRAM-STATUS
010380          EXIT PROGRAM
010390     ELSE
010400          COMPUTE 西暦年Ｗ = 西暦年Ｗ + 和暦計算年Ｗ - 1
010410          MOVE 和暦計算月Ｗ  TO 西暦月Ｗ
010420     END-IF.
010430*
010440     MOVE 西暦年月Ｗ         TO 西暦計算年月Ｗ.
           MOVE 和暦計算日Ｗ       TO 西暦計算日Ｗ.
010450*
012980*================================================================*
019320 作１ファイル書込 SECTION.
019330*
019340     WRITE 作１－レコード
019350     INVALID KEY
019360         MOVE NC"作１"  TO ファイル名
019370         PERFORM エラー表示
019380     END-WRITE.
036110*================================================================*
008560 データチェック SECTION.
008570*
008580     MOVE SPACE          TO 実行キーＷ.
019520* *****************************************************************
019530* * レセプトＦの請求対象区分 = 0 の場合データ作成対象としない *
019540* *****************************************************************
019640     IF ( レセ－請求対象区分 NOT = ZERO ) AND
005778        ( レセ－償還払い区分 NOT = 1 )
              IF(レセ－レセ種別 = 3) AND ( レセ－会総括表印刷対象区分 = 1 )
                 CONTINUE
              ELSE
004090           MOVE レセ－施術和暦  TO 受－施術和暦
004100           MOVE レセ－施術年    TO 受－施術年
004110           MOVE レセ－施術月    TO 受－施術月
004120           MOVE レセ－患者番号  TO 受－患者番号
004130           MOVE レセ－枝番      TO 受－枝番
                 READ 受診者情報Ｆ
                 NOT INVALID KEY
019880               MOVE "YES"  TO 実行キーＷ
019950           END-READ
              END-IF
019960     END-IF.
009040*
020480*================================================================*
031920 レセプト並び順取得 SECTION.
031930*
031940     MOVE レセ－施術和暦   TO 作４－施術和暦.
031950     MOVE レセ－施術年     TO 作４－施術年.
031960     MOVE レセ－施術月     TO 作４－施術月.
031970     MOVE レセ－患者コード TO 作４－患者コード.
           IF レセ－レセ種別 = 3
031980         MOVE 受－助成種別 TO 作４－保険種別
           ELSE
031980         MOVE 受－保険種別 TO 作４－保険種別
           END-IF.
031990     READ 作業ファイル４
032000     INVALID KEY
032020         MOVE SPACE TO 作４－レコード
032000     NOT INVALID KEY
032020         CONTINUE
032030     END-READ.
032040*
032050*================================================================*
       長期継続者Ｆ読込 SECTION.
      *
           MOVE レセ－施術和暦年月 TO 長継－施術和暦年月.
000170     MOVE レセ－患者コード   TO 長継－患者コード.
           READ 長期継続者Ｆ
           INVALID KEY
               MOVE SPACE TO 長継－レコード
           END-READ.
012420*================================================================*
       計算情報取得 SECTION.
      *
      * 制御区分：０１  健康保険
           MOVE 01             TO 計－制御区分.
           MOVE 受－施術和暦   TO 計－開始和暦.
           MOVE 受－施術年     TO 計－開始年.
           MOVE 受－施術月     TO 計－開始月.
      *
           START 計算マスタ KEY IS <= 計－制御区分 計－開始和暦年月
                                                           REVERSED
           END-START.
      *
           IF 状態キー = "00"
               READ 計算マスタ NEXT
               NOT AT END
                   IF ( 受－施術和暦年月 >= 計Ａ－開始和暦年月 ) AND
                      ( 受－施術和暦年月 <= 計Ａ－終了和暦年月 )
                       MOVE 計Ａ－多部位逓減率(3) TO 多部位率３ＷＲ
                   END-IF
               END-READ
           END-IF.
012900*================================================================*
023691 振込口座セット SECTION.
023692*
023693*****************************************
023694*  保険者別に振込口座を設定する
023695*****************************************
023696*
023702     MOVE SPACE    TO 終了フラグ３.
023702     MOVE SPACE    TO 口座番号Ｗ.
023703
023716     OPEN INPUT 振込口座Ｆ.
023717             MOVE NC"振込" TO ファイル名.
023718             PERFORM オープンチェック.
      *
023722     PERFORM 振込口座Ｆ読込.
023723     PERFORM UNTIL 終了フラグ３ NOT = SPACE
023724*        請求口座情報分解
023725         UNSTRING 口座－レコードデータ  DELIMITED BY ","
023726             INTO 請求保険者番号Ｗ 請求保険者名Ｗ 請求口座番号Ｗ 金融機関コードＷ
023728         END-UNSTRING
      *        金融機関コード(2004-135)の時は口座番号(1029444)固定
023731*        請求する保険者番号とマッチするか（先頭の保険者番号0は未登録時用なので無条件セット）
               IF 請求保険者番号Ｗ = 保険者番号Ｗ
                   IF 金融機関コードＷ = "2004-135"
023746                 MOVE "1029444"        TO 口座番号Ｗ
                   ELSE
023746                 MOVE 請求口座番号Ｗ   TO 口座番号Ｗ
023747             END-IF
                   MOVE "YES"                TO 終了フラグ３
               ELSE
                   MOVE "3620000"        TO 口座番号Ｗ
023747         END-IF
023748         PERFORM 振込口座Ｆ読込
023749     END-PERFORM.
023719*
023752     CLOSE 振込口座Ｆ.
023719*
023703*/保険者番号が一致しなかった時、＠までの前方一致チェック/
023719*
           IF 口座番号Ｗ = "3620000"
023702         MOVE SPACE    TO 終了フラグ３
023716         OPEN INPUT 振込口座Ｆ
023717             MOVE NC"振込" TO ファイル名
023718             PERFORM オープンチェック
023719*
023722         PERFORM 振込口座Ｆ読込
023723         PERFORM UNTIL 終了フラグ３ NOT = SPACE
023724*        請求口座情報分解
023725             UNSTRING 口座－レコードデータ  DELIMITED BY ","
023726                INTO 請求保険者番号Ｗ 請求保険者名Ｗ 請求口座番号Ｗ 金融機関コードＷ
023728             END-UNSTRING
      *
                   PERFORM VARYING カウンタ FROM 1 BY 1
                           UNTIL (請求保険者番号Ｗ(カウンタ:1) = "@") OR
                                 (カウンタ > 10)
                       CONTINUE
                   END-PERFORM
                   IF (請求保険者番号Ｗ(1:1) NOT = "@") AND
                      (請求保険者番号Ｗ(カウンタ:1) = "@") AND
                      (請求保険者番号Ｗ(1:カウンタ - 1) = 保険者番号Ｗ(1:カウンタ - 1))
023746                 MOVE 請求口座番号Ｗ   TO 口座番号Ｗ
                       MOVE "YES"            TO 終了フラグ３
                   ELSE
                       MOVE "3620000"        TO 口座番号Ｗ
                   END-IF
023748             PERFORM 振込口座Ｆ読込
               END-PERFORM
023719*
023752         CLOSE 振込口座Ｆ
           END-IF.
023719*
023703*/保険者番号が一致しなかった時、＠終了からの後方一致チェック/
023719*
           IF 口座番号Ｗ = "3620000"
023702         MOVE SPACE    TO 終了フラグ３
023716         OPEN INPUT 振込口座Ｆ
023717             MOVE NC"振込" TO ファイル名
023718             PERFORM オープンチェック
023719*
023722         PERFORM 振込口座Ｆ読込
023723         PERFORM UNTIL 終了フラグ３ NOT = SPACE
023724*        請求口座情報分解
023725             UNSTRING 口座－レコードデータ  DELIMITED BY ","
023726                INTO 請求保険者番号Ｗ 請求保険者名Ｗ 請求口座番号Ｗ 金融機関コードＷ
023728             END-UNSTRING
      *
                   PERFORM VARYING カウンタ FROM 1 BY 1
                           UNTIL (請求保険者番号Ｗ(カウンタ:1) NOT = "@") OR
                                 (カウンタ > 10)
                       CONTINUE
                   END-PERFORM
                   IF (請求保険者番号Ｗ(1:1) = "@") AND
                      (請求保険者番号Ｗ(カウンタ:10 - カウンタ) = 保険者番号Ｗ(カウンタ:10 - カウンタ))
023746                 MOVE 請求口座番号Ｗ   TO 口座番号Ｗ
                       MOVE "YES"            TO 終了フラグ３
                   ELSE
                       MOVE "3620000"        TO 口座番号Ｗ
                   END-IF
023748             PERFORM 振込口座Ｆ読込
               END-PERFORM
023719*
023752         CLOSE 振込口座Ｆ
           END-IF.
023754*
023755*================================================================*
023756 振込口座Ｆ読込 SECTION.
023757*
023761     READ 振込口座Ｆ
023762     AT END
023763         MOVE "YES"  TO 終了フラグ３
023764     END-READ.
023767*
037520*================================================================*
023691 振込口座セット助成 SECTION.
023692*
023693*****************************************
023694*  保険者別に振込口座を設定する
023695*****************************************
023696*
023702     MOVE SPACE    TO 終了フラグ３.
023702     MOVE SPACE    TO 口座番号Ｗ.
023703
023716     OPEN INPUT 振込口座Ｆ.
023717             MOVE NC"振込" TO ファイル名.
023718             PERFORM オープンチェック.
023719
023722     PERFORM 振込口座Ｆ読込.
023723     PERFORM UNTIL 終了フラグ３ NOT = SPACE
023724*        請求口座情報分解
023725         UNSTRING 口座－レコードデータ  DELIMITED BY ","
023726             INTO 請求保険者番号Ｗ 請求保険者名Ｗ 請求口座番号Ｗ 金融機関コードＷ
023728         END-UNSTRING
023731*        請求する保険者番号とマッチするか（先頭の保険者番号0は未登録時用なので無条件セット）
023735         IF 請求保険者番号Ｗ = 市町村番号Ｗ
023746             MOVE 請求口座番号Ｗ   TO 口座番号Ｗ
                   MOVE "YES"                TO 終了フラグ３
               ELSE
                   MOVE "3620000"        TO 口座番号Ｗ
023747         END-IF
023748         PERFORM 振込口座Ｆ読込
023749     END-PERFORM.
023751*
023752     CLOSE 振込口座Ｆ.
023719*
023703*/保険者番号が一致しなかった時、＠までの前方一致チェック/
023719*
           IF 口座番号Ｗ = "3620000"
023702         MOVE SPACE    TO 終了フラグ３
023716         OPEN INPUT 振込口座Ｆ
023717             MOVE NC"振込" TO ファイル名
023718             PERFORM オープンチェック
023719*
023722         PERFORM 振込口座Ｆ読込
023723         PERFORM UNTIL 終了フラグ３ NOT = SPACE
023724*        請求口座情報分解
023725             UNSTRING 口座－レコードデータ  DELIMITED BY ","
023726                INTO 請求保険者番号Ｗ 請求保険者名Ｗ 請求口座番号Ｗ 金融機関コードＷ
023728             END-UNSTRING
      *
                   PERFORM VARYING カウンタ FROM 1 BY 1
                           UNTIL (請求保険者番号Ｗ(カウンタ:1) = "@") OR
                                 (カウンタ > 10)
                       CONTINUE
                   END-PERFORM
                   IF (請求保険者番号Ｗ(1:1) NOT = "@") AND
                      (請求保険者番号Ｗ(カウンタ:1) = "@") AND
                      (請求保険者番号Ｗ(1:カウンタ - 1) = 市町村番号Ｗ(1:カウンタ - 1))
023746                 MOVE 請求口座番号Ｗ   TO 口座番号Ｗ
                       MOVE "YES"            TO 終了フラグ３
                   ELSE
                       MOVE "3620000"        TO 口座番号Ｗ
                   END-IF
023748             PERFORM 振込口座Ｆ読込
               END-PERFORM
023719*
023752         CLOSE 振込口座Ｆ
           END-IF.
023754*
023703*/保険者番号が一致しなかった時、＠終了後の後方一致チェック/
023719*
           IF 口座番号Ｗ = "3620000"
023702         MOVE SPACE    TO 終了フラグ３
023716         OPEN INPUT 振込口座Ｆ
023717             MOVE NC"振込" TO ファイル名
023718             PERFORM オープンチェック
023719*
023722         PERFORM 振込口座Ｆ読込
023723         PERFORM UNTIL 終了フラグ３ NOT = SPACE
023724*        請求口座情報分解
023725             UNSTRING 口座－レコードデータ  DELIMITED BY ","
023726                INTO 請求保険者番号Ｗ 請求保険者名Ｗ 請求口座番号Ｗ 金融機関コードＷ
023728             END-UNSTRING
      *
                   PERFORM VARYING カウンタ FROM 1 BY 1
                           UNTIL (請求保険者番号Ｗ(カウンタ:1) NOT = "@") OR
                                 (カウンタ > 10)
                       CONTINUE
                   END-PERFORM
                   IF (請求保険者番号Ｗ(1:1) = "@") AND
                      (請求保険者番号Ｗ(カウンタ:10 - カウンタ) = 市町村番号Ｗ(カウンタ:10 - カウンタ))
023746                 MOVE 請求口座番号Ｗ   TO 口座番号Ｗ
                       MOVE "YES"            TO 終了フラグ３
                   ELSE
                       MOVE "3620000"        TO 口座番号Ｗ
                   END-IF
023748             PERFORM 振込口座Ｆ読込
               END-PERFORM
023719*
023752         CLOSE 振込口座Ｆ
           END-IF.
023719*
023755*================================================================*
       会提出ファイル作成 SECTION.
      *
007101     OPEN OUTPUT 会提出ファイル.
002680     IF 状態キー  NOT =  "00"
007103         MOVE  NC"指定されたドライブがないか書き込めません" TO 連メ－メッセージ
007104         CALL   "MSG001"
007105         CANCEL "MSG001"
002720         MOVE 99 TO PROGRAM-STATUS
002730         EXIT PROGRAM
002770     END-IF.
      *
           MOVE "{" TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
           MOVE "  |header|: {"               TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
      */データ種別
           MOVE "    |data_type|: |1|,"       TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
      */請求年月
           MOVE 請求和暦年月ＷＲ  TO 和暦計算年月日Ｗ
           PERFORM 西暦年月日取得
           STRING "    |seikyu_ym|: |" DELIMITED BY SIZE
                  西暦計算年月Ｗ       DELIMITED BY SIZE
                  "|,"                 DELIMITED BY SIZE
             INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */レセコン業者
            MOVE "    |vendor|: |MKS|,"       TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
      */バージョン
           MOVE "    |version|: |5.3|"       TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
           MOVE "  },"                       TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
      */施術者情報
           MOVE "  |sejutsusha_arr|: ["      TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
           MOVE "    {"                      TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
      */会員番号
           STRING "      |kaiin_bango|: |" DELIMITED BY SIZE
                   施情－接骨師会会員番号  DELIMITED BY SPACE
                  "|,"                     DELIMITED BY SIZE
             INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */施術者番号
           STRING "      |sejutsusha_bango|: |" DELIMITED BY SIZE
                  施情－新柔整師番号(3:11)      DELIMITED BY SIZE
                  "|,"                          DELIMITED BY SIZE
             INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */施術開始年月日
           MOVE 施術開始年月日Ｗ  TO 和暦計算年月日Ｗ
           PERFORM 西暦年月日取得
           STRING "      |sejutsu_kaishi_ymd|: |" DELIMITED BY SIZE
                  西暦計算年月日Ｗ                DELIMITED BY SIZE
                  "|,"                            DELIMITED BY SIZE
             INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */施術所名
           MOVE 施情－接骨院名(1:40) TO 文字数判定Ｗ
           PERFORM 文字数判定.
           STRING "      |sejutsusho_mei|: |" DELIMITED BY SIZE
                   文字数判定Ｗ(1:文字ＣＮＴ)  DELIMITED BY SIZE
                   "|,"                        DELIMITED BY SIZE
             INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */施術所名カナ
           MOVE 施情－接骨院名カナ(1:40) TO 文字数判定Ｗ
           PERFORM 文字数判定.
           STRING "      |sejutsusho_kana|: |" DELIMITED BY SIZE
                   文字数判定Ｗ(1:文字ＣＮＴ)   DELIMITED BY SIZE
                   "|,"                         DELIMITED BY SIZE
             INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */施術者名
           MOVE 施情－代表者名(1:30) TO 文字数判定Ｗ
           PERFORM 文字数判定.
           STRING "      |sejutsusha_mei|: |" DELIMITED BY SIZE
                   文字数判定Ｗ(1:文字ＣＮＴ)  DELIMITED BY SIZE
                   "|,"                        DELIMITED BY SIZE
             INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */施術者名カナ
           MOVE 施情－代表者カナ(1:30) TO 文字数判定Ｗ
           PERFORM 文字数判定.
           STRING "      |sejutsusha_kana|: |" DELIMITED BY SIZE
                   文字数判定Ｗ(1:文字ＣＮＴ)  DELIMITED BY SIZE
                   "|,"                          DELIMITED BY SIZE
             INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */施術所_郵便番号
           STRING "      |yubin|: |" DELIMITED BY SIZE
                   施情－郵便番号     DELIMITED BY SIZE
                   "|,"               DELIMITED BY SIZE
             INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */施術所_住所１
      */－が・になる対策‐に変換/20230413
           INSPECT 施情－住所１   REPLACING ALL "－" BY "‐"
           MOVE 施情－住所１(1:40) TO 文字数判定Ｗ
           PERFORM 文字数判定.
           STRING "      |jusho1|: |"        DELIMITED BY SIZE
                   文字数判定Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                   "|,"                       DELIMITED BY SIZE
             INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */施術所_住所２
           MOVE 施情－住所２(1:40) TO 文字数判定Ｗ
           PERFORM 文字数判定.
           STRING "      |jusho2|: |"        DELIMITED BY SIZE
                   文字数判定Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                   "|,"                       DELIMITED BY SIZE
             INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */施術所_TEL
            STRING "      |tel|: |"     DELIMITED BY SIZE
                   施情－電話番号(1:13) DELIMITED BY SPACE
                   "|,"                 DELIMITED BY SIZE
             INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */施術所_FAX
           MOVE "      |fax|: ||,"       TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
      */施術所_EMAIL
           MOVE "      |email|: ||,"       TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
      */振込先_銀行名
            MOVE 施情－取引先銀行名(1:30) TO 文字数判定Ｗ
            PERFORM 文字数判定.
            STRING "      |ginko_mei|: |"     DELIMITED BY SIZE
                   文字数判定Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                   "|,"                       DELIMITED BY SIZE
               INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */振込先_支店名
           MOVE 施情－取引先銀行支店名(1:30) TO 文字数判定Ｗ
           PERFORM 文字数判定.
           STRING "      |shiten_mei|: |"     DELIMITED BY SIZE
                  文字数判定Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                  "|,"                       DELIMITED BY SIZE
               INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */振込先_銀行名カナ
           MOVE "      |ginko_kana|: ||,"    TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
      */振込先_支店名カナ
           MOVE "      |shiten_kana|: ||,"   TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
      */振込先_口座種別
           STRING "      |koza_shu|: |" DELIMITED BY SIZE
                  施情－預金種別        DELIMITED BY SIZE
                  "|,"                  DELIMITED BY SIZE
               INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */振込先_口座番号
           STRING "      |koza_bango|: |" DELIMITED BY SIZE
                  施情－口座番号(1:7)     DELIMITED BY SIZE
                  "|,"                    DELIMITED BY SIZE
               INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */振込先_名義
           MOVE 施情－口座名義人(1:60) TO 文字数判定Ｗ
           PERFORM 文字数判定.
           STRING "      |koza_meigi|: |"    DELIMITED BY SIZE
                  文字数判定Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                  "|,"                       DELIMITED BY SIZE
               INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */振込先_名義カナ
           MOVE 施情－口座名義人カナ(1:60) TO 文字数判定Ｗ
           PERFORM 文字数判定.
           STRING "      |koza_meigi_kana|: |" DELIMITED BY SIZE
                  文字数判定Ｗ(1:文字ＣＮＴ)   DELIMITED BY SIZE
                  "|,"                         DELIMITED BY SIZE
               INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */共済番号
           STRING "      |kyosai_bango|: |" DELIMITED BY SIZE
                  施情－共済連番号          DELIMITED BY SIZE
                  "|,"                      DELIMITED BY SIZE
               INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */地共済番号
           STRING "      |chikyosai_bango|: |" DELIMITED BY SIZE
                  施情－地共済連番号           DELIMITED BY SIZE
                  "|,"                         DELIMITED BY SIZE
               INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */防衛省番号
           STRING "      |boeisho_bango|: |" DELIMITED BY SIZE
                  施情－自衛官番号           DELIMITED BY SIZE
                  "|,"                       DELIMITED BY SIZE
               INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */施術機関番号1
025580**   / 県施術ID /
025600     MOVE 01                     TO ＩＤ管－ＩＤ区分.
025610     MOVE ZERO                   TO ＩＤ管－施術所番号.
025620     MOVE 施情－都道府県ＪＩＳ   TO ＩＤ管－保険種別.
025630     MOVE SPACE                  TO ＩＤ管－保険者番号.
025640     READ ＩＤ管理マスタ
025650     NOT INVALID KEY
025660         MOVE ＩＤ管－施術ＩＤ番号   TO 県施術ＩＤＷ
025670     END-READ.
           STRING "      |sejutsu_kikan_bango1|: |" DELIMITED BY SIZE
                  県施術ＩＤＷ                      DELIMITED BY SPACE
                  "|,"                              DELIMITED BY SIZE
               INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */施術機関番号2
           MOVE "      |sejutsu_kikan_bango2|: ||,"    TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
      */項目追加↓↓↓/20221028
      */労災指定・指名番号
           MOVE "      |rosai_shitei_shimei_bango|: ||,"    TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
      */明細書発行届出有無
           STRING "      |meisaisho_hakko_umu|: |" DELIMITED BY SIZE
                  明細発行加算区分Ｗ               DELIMITED BY SPACE
                  "|"                             DELIMITED BY SIZE
               INTO 会提－レコードデータ
           END-STRING
           PERFORM 会提出Ｆ書込.
      */項目追加↑↑↑/20221028
           MOVE "    }"   TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
           MOVE "  ],"   TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
      */レセプト情報***************************
           MOVE "  |rezept_arr|: ["   TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
026580     OPEN INPUT 作業ファイル１.
026590         MOVE NC"作１" TO ファイル名.
026600         PERFORM オープンチェック.
           MOVE SPACE TO 終了フラグ３.
005580     PERFORM 作業ファイル１読込
005590     PERFORM UNTIL 終了フラグ３ = "YES"
               MOVE 作１－レコード TO 作１レコードＷ
      */レセプト番号
               MOVE "      {"   TO 会提－レコードデータ
               PERFORM 会提出Ｆ書込
               MOVE 作１－レセプト番号 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |rezept_no|: " DELIMITED BY SIZE
      *                作１－レセプト番号    DELIMITED BY SIZE
                      数字左詰２Ｗ          DELIMITED BY SPACE
                      ","                   DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */施術者番号
               STRING "      |sejutsusha_bango|: |" DELIMITED BY SIZE
                      作１－施術者番号              DELIMITED BY SIZE
                      "|,"                          DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */施術西暦年月
               STRING "      |sejutsu_ym|: |" DELIMITED BY SIZE
                      作１－施術西暦年月      DELIMITED BY SIZE
                      "|,"                    DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */提出先区分
               STRING "      |teishutsusaki_kbn|: |" DELIMITED BY SIZE
                      作１－提出先区分               DELIMITED BY SIZE
                      "|,"                           DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */保険者番号
               STRING "      |hokensha_bango|: |" DELIMITED BY SIZE
                      作１－保険者番号            DELIMITED BY SIZE
                      "|,"                        DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */保険証記号
               STRING "      |hokensho_kigo|: |" DELIMITED BY SIZE
                      作１－保険証記号           DELIMITED BY SPACE
                      "|,"                       DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */保険証番号
               STRING "      |hokensho_bango|: |" DELIMITED BY SIZE
                      作１－保険証番号            DELIMITED BY SPACE
                      "|,"                        DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */公費負担者番号１
               STRING "      |kohi_futansha_bango1|: |" DELIMITED BY SIZE
                      作１－公費負担者番号１            DELIMITED BY SPACE
                      "|,"                              DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */公費受給者番号１
               STRING "      |kohi_jukyusha_bango1|: |" DELIMITED BY SIZE
                      作１－公費受給者番号１            DELIMITED BY SPACE
                      "|,"                              DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */公費負担者番号２
               STRING "      |kohi_futansha_bango2|: |" DELIMITED BY SIZE
                      作１－公費負担者番号２            DELIMITED BY SPACE
                      "|,"                              DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */公費受給者番号２
               STRING "      |kohi_jukyusha_bango2|: |" DELIMITED BY SIZE
                      作１－公費受給者番号２            DELIMITED BY SPACE
                      "|,"                              DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */保険種別大区分
               STRING "      |hoken_shubetsu_kbn|: |" DELIMITED BY SIZE
                      作１－保険種別大区分            DELIMITED BY SIZE
                      "|,"                            DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */単併区分
               STRING "      |tanhei_kbn|: |" DELIMITED BY SIZE
                      作１－単併区分          DELIMITED BY SIZE
                      "|,"                    DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */本家区分
               STRING "      |honka_kbn|: |" DELIMITED BY SIZE
                      作１－本家区分         DELIMITED BY SIZE
                      "|,"                   DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */給付割合
               MOVE 作１－給付割合 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |kyufu_wari|: " DELIMITED BY SIZE
      *                作１－給付割合         DELIMITED BY SIZE
                      数字左詰２Ｗ           DELIMITED BY SPACE
                      ","                    DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */続柄
               STRING "      |zokugara|: |" DELIMITED BY SIZE
                      作１－続柄            DELIMITED BY SIZE
                      "|,"                  DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */被保険者カナ
               MOVE 作１－被保険者カナ TO 文字数判定Ｗ
               PERFORM 文字数判定
               STRING "      |hihokensha_kana|: |" DELIMITED BY SIZE
                      文字数判定Ｗ(1:文字ＣＮＴ)   DELIMITED BY SIZE
                      "|,"                         DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */被保険者名
               MOVE 作１－被保険者名 TO 文字数判定Ｗ
               PERFORM 文字数判定
               STRING "      |hihokensha_mei|: |" DELIMITED BY SIZE
                      文字数判定Ｗ(1:文字ＣＮＴ)  DELIMITED BY SIZE
                      "|,"                        DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */受療者名カナ
               MOVE 作１－受療者名カナ TO 文字数判定Ｗ
               PERFORM 文字数判定
               STRING "      |juryosha_kana|: |" DELIMITED BY SIZE
                      文字数判定Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                      "|,"                       DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */受療者名
               MOVE 作１－受療者名 TO 文字数判定Ｗ
               PERFORM 文字数判定
               STRING "      |juryosha_mei|: |"  DELIMITED BY SIZE
                      文字数判定Ｗ(1:文字ＣＮＴ) DELIMITED BY SIZE
                      "|,"                       DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */受療者性別
               STRING "      |juryosha_seibetsu|: |" DELIMITED BY SIZE
                      作１－受療者性別               DELIMITED BY SIZE
                      "|,"                           DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */受療者生年月日
               STRING "      |juryosha_seinengappi|: |" DELIMITED BY SIZE
                      作１－受療者生年月日              DELIMITED BY SIZE
                      "|,"                              DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */被保険者郵便番号
               STRING "      |hihokensha_yubin|: |" DELIMITED BY SIZE
                   作１－被保険者郵便番号           DELIMITED BY SIZE
                      "|,"                          DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */被保険者住所
      */－が・になる対策‐に変換/20230413
               INSPECT 作１－被保険者住所   REPLACING ALL "－" BY "‐"
      */全角、半角によらず最大60文字/20230413
               MOVE 作１－被保険者住所 TO 漢字チェックＴＢＬ
               PERFORM 全角半角チェック
               MOVE 漢字チェックＴＢＬ(1:カウンタ３) TO 作１－被保険者住所
      *
               MOVE 作１－被保険者住所 TO 文字数判定Ｗ
               PERFORM 文字数判定
               STRING "      |hihokensha_jusho|: |" DELIMITED BY SIZE
                      文字数判定Ｗ(1:文字ＣＮＴ)    DELIMITED BY SIZE
                      "|,"                          DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */合計金額
               MOVE 作１－合計金額 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |hiyo_gaku|: " DELIMITED BY SIZE
      *                作１－合計金額        DELIMITED BY SIZE
                      数字左詰２Ｗ          DELIMITED BY SPACE
                      ","                   DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */一部負担金
               MOVE 作１－一部負担金 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |ichibu_futankin|: " DELIMITED BY SIZE
      *                作１－一部負担金            DELIMITED BY SIZE
                      数字左詰２Ｗ                DELIMITED BY SPACE
                      ","                         DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */請求金額
               MOVE 作１－請求金額 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |seikyu_gaku|: " DELIMITED BY SIZE
      *                作１－請求金額          DELIMITED BY SIZE
                      数字左詰２Ｗ            DELIMITED BY SPACE
                      ","                     DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */公費一部負担金相当額
               MOVE 作１－公費一部負担金相当額 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |kohi_ichibu_futankin|: " DELIMITED BY SIZE
      *                作１－公費一部負担金相当額       DELIMITED BY SIZE
                      数字左詰２Ｗ                     DELIMITED BY SPACE
                      ","                              DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */公費請求金額
               MOVE 作１－公費請求金額 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |kohi_seikyu_gaku|: " DELIMITED BY SIZE
      *               作１－公費請求金額            DELIMITED BY SIZE
                      数字左詰２Ｗ                 DELIMITED BY SPACE
                      ","                          DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */総実日数
               MOVE 作１－総実日数 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |sojitsu_nissu|: " DELIMITED BY SIZE
      *                作１－総実日数            DELIMITED BY SIZE
                      数字左詰２Ｗ              DELIMITED BY SPACE
                      ","                       DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */部位数
               MOVE 作１－部位数 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |buisu|: " DELIMITED BY SIZE
      *                作１－部位数      DELIMITED BY SIZE
                      数字左詰２Ｗ      DELIMITED BY SPACE
                      ","               DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */再請求区分
               STRING "      |saiseikyu_kbn|: |" DELIMITED BY SIZE
                      作１－再請求区分           DELIMITED BY SIZE
                      "|,"                       DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */負担区分
               STRING "      |futan_kbn|: |" DELIMITED BY SIZE
                      作１－負担区分         DELIMITED BY SIZE
                      "|,"                   DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */負担割合
               STRING "      |futan_wari|: " DELIMITED BY SIZE
                      作１－負担割合         DELIMITED BY SIZE
                      ","                    DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */患者番号２
               MOVE 作１－患者番号２ TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |kanja_no|: " DELIMITED BY SIZE
      *                作１－患者番号２     DELIMITED BY SIZE
                      数字左詰２Ｗ           DELIMITED BY SPACE
                      ","                  DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */患者番号枝番
               STRING "      |kanja_subno|: " DELIMITED BY SIZE
                      作１－患者番号枝番      DELIMITED BY SIZE
                      ","                     DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */負傷データ
               MOVE "      |fusho_arr|: ["   TO 会提－レコードデータ
               PERFORM 会提出Ｆ書込
               PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL
      *                (部位ＣＮＴ > 負－部位数) OR (部位ＣＮＴ > 6) OR (作１－負傷ナンバー(部位ＣＮＴ) = ZERO)
                       (部位ＣＮＴ > 6) OR (作１－負傷ナンバー(部位ＣＮＴ) = ZERO)
                   IF 部位ＣＮＴ > 1
                       MOVE "        },"   TO 会提－レコードデータ
                       PERFORM 会提出Ｆ書込
                   END-IF
                   MOVE "        {"   TO 会提－レコードデータ
                   PERFORM 会提出Ｆ書込
      */負傷ナンバー
                   STRING "          |fusho_no|: "       DELIMITED BY SIZE
                          作１－負傷ナンバー(部位ＣＮＴ) DELIMITED BY SIZE
                          ","                            DELIMITED BY SIZE
                       INTO 会提－レコードデータ
                   END-STRING
                   PERFORM 会提出Ｆ書込
      */負傷区分
                   STRING "          |fusho_kbn|: |" DELIMITED BY SIZE
                   作１－負傷区分    (部位ＣＮＴ)    DELIMITED BY SIZE
                          "|,"                       DELIMITED BY SIZE
                       INTO 会提－レコードデータ
                   END-STRING
                   PERFORM 会提出Ｆ書込
      */負傷コード          
                   MOVE "          |fusho_cd|: ||,"   TO 会提－レコードデータ
                   PERFORM 会提出Ｆ書込
      */負傷名              
                   STRING "          |fusho_mei|: |"     DELIMITED BY SIZE
                          作１－負傷名      (部位ＣＮＴ) DELIMITED BY SPACE
                          "|,"                           DELIMITED BY SIZE
                       INTO 会提－レコードデータ
                   END-STRING
                   PERFORM 会提出Ｆ書込
      */負傷年月日          
                   STRING "          |fusho_ymd|: |"     DELIMITED BY SIZE
                          作１－負傷年月日  (部位ＣＮＴ) DELIMITED BY SIZE
                          "|,"                           DELIMITED BY SIZE
                       INTO 会提－レコードデータ
                   END-STRING
                   PERFORM 会提出Ｆ書込
      */初検年月日          
                   STRING "          |shoken_ymd|: |"    DELIMITED BY SIZE
                          作１－初検年月日  (部位ＣＮＴ) DELIMITED BY SIZE
                          "|,"                           DELIMITED BY SIZE
                       INTO 会提－レコードデータ
                   END-STRING
                   PERFORM 会提出Ｆ書込
      */施術開始日          
                   STRING "          |sejutsu_kaishi_ymd|: |" DELIMITED BY SIZE
                          作１－施術開始日  (部位ＣＮＴ)      DELIMITED BY SIZE
                          "|,"                                DELIMITED BY SIZE
                       INTO 会提－レコードデータ
                   END-STRING
                   PERFORM 会提出Ｆ書込
      */施術終了日          
                   STRING "          |sejutsu_shuryo_ymd|: |" DELIMITED BY SIZE
                          作１－施術終了日  (部位ＣＮＴ)      DELIMITED BY SIZE
                          "|,"                                DELIMITED BY SIZE
                       INTO 会提－レコードデータ
                   END-STRING
                   PERFORM 会提出Ｆ書込
      */実日数              
                   MOVE 作１－実日数(部位ＣＮＴ) TO 数字左詰Ｗ
                   PERFORM 数字左詰処理
                   STRING "          |jitsu_nissu|: "    DELIMITED BY SIZE
      *                    作１－実日数      (部位ＣＮＴ) DELIMITED BY SIZE
                          数字左詰２Ｗ           DELIMITED BY SPACE
                          ","                            DELIMITED BY SIZE
                       INTO 会提－レコードデータ
                   END-STRING
                   PERFORM 会提出Ｆ書込
      */施術日
                   MOVE "          |sejutsubi_arr|: ["   TO 会提－レコードデータ
                   PERFORM 会提出Ｆ書込
                   PERFORM VARYING 日カウンタ FROM 1 BY 1 UNTIL
                           (日カウンタ > 31) OR (作１－施術日(部位ＣＮＴ 日カウンタ) = ZERO)
                       MOVE 作１－施術日(部位ＣＮＴ 日カウンタ) TO 数字左詰Ｗ
                       PERFORM 数字左詰処理
                       IF 作１－施術日(部位ＣＮＴ 日カウンタ + 1) NOT = ZERO
                           STRING "            "                       DELIMITED BY SIZE
      *                            作１－施術日 (部位ＣＮＴ 日カウンタ) DELIMITED BY SIZE
                                   数字左詰２Ｗ                        DELIMITED BY SPACE
                                  ","                                  DELIMITED BY SIZE
                               INTO 会提－レコードデータ
                           END-STRING
                       ELSE
                           STRING "            "                       DELIMITED BY SIZE
      *                            作１－施術日 (部位ＣＮＴ 日カウンタ) DELIMITED BY SIZE
                                   数字左詰２Ｗ                        DELIMITED BY SPACE
                               INTO 会提－レコードデータ
                           END-STRING
                       END-IF
                       PERFORM 会提出Ｆ書込
                   END-PERFORM
                   MOVE "          ],"   TO 会提－レコードデータ
                   PERFORM 会提出Ｆ書込
      */転帰区分            
                   STRING "          |tenki_kbn|: |"     DELIMITED BY SIZE
                          作１－転帰区分    (部位ＣＮＴ) DELIMITED BY SIZE
                          "|,"                           DELIMITED BY SIZE
                       INTO 会提－レコードデータ
                   END-STRING
                   PERFORM 会提出Ｆ書込
      */整固施区分          
                   STRING "          |seikose_kbn|: |"   DELIMITED BY SIZE
                          作１－整固施区分  (部位ＣＮＴ) DELIMITED BY SIZE
                          "|,"                           DELIMITED BY SIZE
                       INTO 会提－レコードデータ
                   END-STRING
                   PERFORM 会提出Ｆ書込
      */整固施回数          
                   STRING "          |seikose_kaisu|: "  DELIMITED BY SIZE
                          作１－整固施回数  (部位ＣＮＴ) DELIMITED BY SIZE
                          ","                            DELIMITED BY SIZE
                       INTO 会提－レコードデータ
                   END-STRING
                   PERFORM 会提出Ｆ書込
      */整固施療料          
                   MOVE 作１－整固施療料(部位ＣＮＴ) TO 数字左詰Ｗ
                   PERFORM 数字左詰処理
                   STRING "          |seikose_ryo|: "    DELIMITED BY SIZE
      *                    作１－整固施療料  (部位ＣＮＴ) DELIMITED BY SIZE
                          数字左詰２Ｗ           DELIMITED BY SPACE
                          ","                            DELIMITED BY SIZE
                       INTO 会提－レコードデータ
                   END-STRING
                   PERFORM 会提出Ｆ書込
      */負傷原因            
                   STRING "          |fusho_genin|: |"   DELIMITED BY SIZE
                          作１－負傷原因    (部位ＣＮＴ) DELIMITED BY SPACE
                          "|,"                           DELIMITED BY SIZE
                       INTO 会提－レコードデータ
                   END-STRING
                   PERFORM 会提出Ｆ書込
      */長期理由            
                   STRING "          |choki_riyu|: |"    DELIMITED BY SIZE
                          作１－長期理由    (部位ＣＮＴ) DELIMITED BY SPACE
                          "|,"                           DELIMITED BY SIZE
                       INTO 会提－レコードデータ
                   END-STRING
                   PERFORM 会提出Ｆ書込
      */長期頻回理由
                   MOVE "          |choki_hinkai_riyu|: ||,"   TO 会提－レコードデータ
                   PERFORM 会提出Ｆ書込
      */経過                
                   MOVE "          |keika|: ||,"   TO 会提－レコードデータ
                   PERFORM 会提出Ｆ書込
      */審査区分            
                   MOVE "          |shinsa_kbn|: |0|"   TO 会提－レコードデータ
                   PERFORM 会提出Ｆ書込
               END-PERFORM
               MOVE "        }"   TO 会提－レコードデータ
               PERFORM 会提出Ｆ書込
               MOVE "      ],"   TO 会提－レコードデータ
               PERFORM 会提出Ｆ書込
      */請求区分
               STRING "      |seikyu_kbn|: |" DELIMITED BY SIZE
                     作１－請求区分           DELIMITED BY SIZE
                      "|,"                    DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */初検回数               
               STRING "      |shoken_kaisu|: " DELIMITED BY SIZE
                     作１－初検回数            DELIMITED BY SIZE
                      ","                      DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */初検料                 
               MOVE 作１－初検料 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |shoken_ryo|: " DELIMITED BY SIZE
      *               作１－初検料            DELIMITED BY SIZE
                      数字左詰２Ｗ           DELIMITED BY SPACE
                      ","                    DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */初検休日加算回数       
               STRING "      |shoken_kyujitsu_kasan_kaisu|: " DELIMITED BY SIZE
                     作１－初検休日加算回数                   DELIMITED BY SIZE
                      ","                                     DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */初検深夜加算回数       
               STRING "      |shoken_shinya_kasan_kaisu|: " DELIMITED BY SIZE
                     作１－初検深夜加算回数                 DELIMITED BY SIZE
                      ","                                   DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */初検時間外加算回数     
               STRING "      |shoken_jikangai_kasan_kaisu|: " DELIMITED BY SIZE
                     作１－初検時間外加算回数                 DELIMITED BY SIZE
                      ","                                     DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */初検加算               
               MOVE 作１－初検加算 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |shoken_kasan|: " DELIMITED BY SIZE
      *               作１－初検加算            DELIMITED BY SIZE
                      数字左詰２Ｗ           DELIMITED BY SPACE
                      ","                      DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */初検時相談支援料回数   
               MOVE 作１－初検時相談支援料 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |sodan_shien_kaisu|: " DELIMITED BY SIZE
      *               作１－初検時相談支援料回数     DELIMITED BY SIZE
                      数字左詰２Ｗ           DELIMITED BY SPACE
                      ","                           DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */初検時相談支援料       
               MOVE 作１－初検時相談支援料 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |sodan_shien_ryo|: " DELIMITED BY SIZE
      *                作１－初検時相談支援料       DELIMITED BY SIZE
                      数字左詰２Ｗ           DELIMITED BY SPACE
                      ","                         DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */再検回数               
               STRING "      |saiken_kaisu|: " DELIMITED BY SIZE
                     作１－再検回数            DELIMITED BY SIZE
                      ","                      DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */再検料                 
               MOVE 作１－再検料 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |saiken_ryo|: " DELIMITED BY SIZE
      *               作１－再検料            DELIMITED BY SIZE
                      数字左詰２Ｗ           DELIMITED BY SPACE
                      ","                    DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */往療距離               
               MOVE 作１－往療距離 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |oryo_kyori|: " DELIMITED BY SIZE
      *               作１－往療距離          DELIMITED BY SIZE
                      数字左詰２Ｗ           DELIMITED BY SPACE
                      ","                    DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */往療回数               
               MOVE 作１－往療回数 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |oryo_kaisu|: " DELIMITED BY SIZE
      *               作１－往療回数          DELIMITED BY SIZE
                      数字左詰２Ｗ           DELIMITED BY SPACE
                      ","                    DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */往療料                 
               MOVE 作１－往療料 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |oryo_ryo|: " DELIMITED BY SIZE
      *               作１－往療料          DELIMITED BY SIZE
                      数字左詰２Ｗ           DELIMITED BY SPACE
                      ","                  DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */往療理由               
               STRING "      |oryo_riyu|: |" DELIMITED BY SIZE
                     作１－往療理由          DELIMITED BY SPACE
                      "|,"                   DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */夜間加算の往療回数     
               MOVE 作１－夜間加算の往療回数 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |yakan_kasan_oryo_kaisu|: " DELIMITED BY SIZE
      *               作１－夜間加算の往療回数            DELIMITED BY SIZE
                      数字左詰２Ｗ           DELIMITED BY SPACE
                      ","                                DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */難路加算の往療回数     
               MOVE 作１－難路加算の往療回数 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |nanro_kasan_oryo_kaisu|: " DELIMITED BY SIZE
      *               作１－難路加算の往療回数            DELIMITED BY SIZE
                      数字左詰２Ｗ           DELIMITED BY SPACE
                      ","                                DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */暴風雨雪加算の往療回数 
               MOVE 作１－暴風雨雪加算の往療回数 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |bofu_kasan_oryo_kaisu|: " DELIMITED BY SIZE
      *               作１－暴風雨雪加算の往療回数       DELIMITED BY SIZE
                      数字左詰２Ｗ           DELIMITED BY SPACE
                      ","                               DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */往療加算               
               MOVE 作１－往療加算 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |oryo_kasan|: " DELIMITED BY SIZE
      *               作１－往療加算          DELIMITED BY SIZE
                      数字左詰２Ｗ           DELIMITED BY SPACE
                      ","                    DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */金属副子大回数         
               MOVE 作１－金属副子大回数 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |kinzoku_fukushi_dai_kaisu|: " DELIMITED BY SIZE
      *               作１－金属副子大回数                   DELIMITED BY SIZE
                      数字左詰２Ｗ           DELIMITED BY SPACE
                      ","                                   DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */金属副子中回数         
               STRING "      |kinzoku_fukushi_chu_kaisu|: " DELIMITED BY SIZE
                     作１－金属副子中回数                   DELIMITED BY SIZE
                      ","                                   DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */金属副子小回数         
               STRING "      |kinzoku_fukushi_sho_kaisu|: " DELIMITED BY SIZE
                     作１－金属副子小回数                   DELIMITED BY SIZE
                      ","                                   DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */金属副子加算           
               MOVE 作１－金属副子加算 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |kinzoku_fukushi_kasan|: " DELIMITED BY SIZE
      *               作１－金属副子加算                 DELIMITED BY SIZE
                      数字左詰２Ｗ           DELIMITED BY SPACE
                      ","                               DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */金属副子加算日
               MOVE 作１－金属副子加算日(日カウンタ) TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               MOVE "      |kinzoku_fukushi_kasanbi_arr|: ["   TO 会提－レコードデータ
               PERFORM 会提出Ｆ書込
               PERFORM VARYING 日カウンタ FROM 1 BY 1 UNTIL 日カウンタ > 3
                   IF 作１－金属副子加算日(日カウンタ) NOT = ZERO
                       IF (作１－金属副子加算日(日カウンタ + 1) NOT = ZERO) AND (日カウンタ < 3)
                           STRING "       "                        DELIMITED BY SIZE
      *                            作１－金属副子加算日(日カウンタ) DELIMITED BY SIZE
                                  数字左詰２Ｗ           DELIMITED BY SPACE
                                  ","                              DELIMITED BY SIZE
                               INTO 会提－レコードデータ
                           END-STRING
                       ELSE
                           STRING "       "                        DELIMITED BY SIZE
      *                            作１－金属副子加算日(日カウンタ) DELIMITED BY SIZE
                                  数字左詰２Ｗ           DELIMITED BY SPACE
                               INTO 会提－レコードデータ
                           END-STRING
                       END-IF
                       PERFORM 会提出Ｆ書込
                   END-IF
               END-PERFORM
               MOVE "      ],"   TO 会提－レコードデータ
               PERFORM 会提出Ｆ書込
     */施術情報提供料の回数 
               STRING "      |joho_teikyo_ryo_kaisu|: " DELIMITED BY SIZE
                     作１－施術情報提供料の回数         DELIMITED BY SIZE
                      ","                               DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
     */施術情報提供料       
               MOVE 作１－施術情報提供料 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |joho_teikyo_ryo|: " DELIMITED BY SIZE
      *               作１－施術情報提供料         DELIMITED BY SIZE
                      数字左詰２Ｗ           DELIMITED BY SPACE
                      ","                         DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
     */運動後療実施回数     
               STRING "      |undo_koryo_kaisu|: " DELIMITED BY SIZE
                     作１－運動後療実施回数        DELIMITED BY SIZE
                      ","                          DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
     */運動後療料           
               MOVE 作１－運動後療料 TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               STRING "      |undo_koryo_ryo|: " DELIMITED BY SIZE
      *               作１－運動後療料            DELIMITED BY SIZE
                      数字左詰２Ｗ           DELIMITED BY SPACE
                      ","                        DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */運動後療実施日
               MOVE 作１－運動後療実施日(日カウンタ) TO 数字左詰Ｗ
               PERFORM 数字左詰処理
               MOVE "      |undo_koryobi_arr|: ["   TO 会提－レコードデータ
               PERFORM 会提出Ｆ書込
               PERFORM VARYING 日カウンタ FROM 1 BY 1 UNTIL 日カウンタ > 5
                   IF 作１－運動後療実施日(日カウンタ) NOT = ZERO
                       IF (作１－運動後療実施日(日カウンタ + 1) NOT = ZERO) AND (日カウンタ < 5)
                           STRING "       "                        DELIMITED BY SIZE
      *                            作１－運動後療実施日(日カウンタ) DELIMITED BY SIZE
                                  数字左詰２Ｗ           DELIMITED BY SPACE
                                  ","                              DELIMITED BY SIZE
                               INTO 会提－レコードデータ
                           END-STRING
                       ELSE
                           STRING "       "                        DELIMITED BY SIZE
      *                            作１－運動後療実施日(日カウンタ) DELIMITED BY SIZE
                                  数字左詰２Ｗ           DELIMITED BY SPACE
                               INTO 会提－レコードデータ
                           END-STRING
                       END-IF
                       PERFORM 会提出Ｆ書込
                   END-IF
               END-PERFORM
               MOVE "      ],"   TO 会提－レコードデータ
               PERFORM 会提出Ｆ書込
      */項目追加↓↓↓/20221028
      */明細書発行体制加算の回数
               MOVE ZERO TO 明細書発行回数Ｗ
               IF 作１－明細書発行加算料 NOT = ZERO
                   MOVE 1 TO 明細書発行回数Ｗ
               END-IF
               STRING "      |meisaisho_hakko_kaisu|: " DELIMITED BY SIZE
                      明細書発行回数Ｗ                  DELIMITED BY SIZE
                      ","                               DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */明細書発行体制加算
               STRING "      |meisaisho_hakko_ryo|: " DELIMITED BY SIZE
                      作１－明細書発行加算料          DELIMITED BY SIZE
                      ","                             DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */明細書発行日
               IF 作１－明細書発行加算料 NOT = ZERO
                   STRING "      |meisaisho_hakko_md|: |" DELIMITED BY SIZE
                          作１－施術月                    DELIMITED BY SIZE
                          作１－明細書発行加算日          DELIMITED BY SIZE
                          "|,"                            DELIMITED BY SIZE
                       INTO 会提－レコードデータ
                   END-STRING
               ELSE
                   MOVE "      |meisaisho_hakko_md|: ||,"    TO 会提－レコードデータ
               END-IF
               PERFORM 会提出Ｆ書込
      */項目追加↑↑↑/20221028
      */部位別料金データ
               MOVE "      |bui_ryokin_arr|: ["   TO 会提－レコードデータ
               PERFORM 会提出Ｆ書込
               MOVE ZERO TO 行番号存在Ｆ
               PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL
      *                (部位ＣＮＴ > 負－部位数) OR (部位ＣＮＴ > 10)
                      (部位ＣＮＴ > 10)
                   IF 作１－行番号(部位ＣＮＴ) NOT = ZERO
                       MOVE 1 TO 行番号存在Ｆ
                       IF 部位ＣＮＴ > 1
                           MOVE "        },"   TO 会提－レコードデータ
                           PERFORM 会提出Ｆ書込
                       END-IF
                       MOVE "        {"   TO 会提－レコードデータ
                       PERFORM 会提出Ｆ書込
      */行番号
                       STRING "          |line_no|: "    DELIMITED BY SIZE
                                作１－行番号(部位ＣＮＴ) DELIMITED BY SIZE
                              ","                        DELIMITED BY SIZE
                           INTO 会提－レコードデータ
                       END-STRING
                       PERFORM 会提出Ｆ書込
      */逓減開始月日        
                       STRING "          |teigen_kaishi_md|: |"     DELIMITED BY SIZE
                              作１－逓減開始月日      (部位ＣＮＴ) DELIMITED BY SPACE
                              "|,"                                 DELIMITED BY SIZE
                           INTO 会提－レコードデータ
                       END-STRING
                       PERFORM 会提出Ｆ書込
      */後療回数            
                       MOVE 作１－後療回数(部位ＣＮＴ) TO 数字左詰Ｗ
                       PERFORM 数字左詰処理
                       STRING "          |koryo_kaisu|: "          DELIMITED BY SIZE
      *                        作１－後療回数          (部位ＣＮＴ) DELIMITED BY SIZE
                              数字左詰２Ｗ           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO 会提－レコードデータ
                       END-STRING
                       PERFORM 会提出Ｆ書込
      */後療料              
                       MOVE 作１－後療料(部位ＣＮＴ) TO 数字左詰Ｗ
                       PERFORM 数字左詰処理
                       STRING "          |koryo_ryo|: "            DELIMITED BY SIZE
      *                        作１－後療料            (部位ＣＮＴ) DELIMITED BY SIZE
                              数字左詰２Ｗ           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO 会提－レコードデータ
                       END-STRING
                       PERFORM 会提出Ｆ書込
      */冷罨法回数          
                       STRING "          |reianpo_kaisu|: "        DELIMITED BY SIZE
                              作１－冷罨法回数        (部位ＣＮＴ) DELIMITED BY SIZE
                              ","                                  DELIMITED BY SIZE
                           INTO 会提－レコードデータ
                       END-STRING
                       PERFORM 会提出Ｆ書込
      */冷罨法料            
                       MOVE 作１－冷罨法料(部位ＣＮＴ) TO 数字左詰Ｗ
                       PERFORM 数字左詰処理
                       STRING "          |reianpo_ryo|: "          DELIMITED BY SIZE
      *                        作１－冷罨法料          (部位ＣＮＴ) DELIMITED BY SIZE
                              数字左詰２Ｗ           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO 会提－レコードデータ
                       END-STRING
                       PERFORM 会提出Ｆ書込
      */温罨法回数          
                       MOVE 作１－温罨法回数(部位ＣＮＴ) TO 数字左詰Ｗ
                       PERFORM 数字左詰処理
                       STRING "          |onanpo_kaisu|: "         DELIMITED BY SIZE
      *                        作１－温罨法回数        (部位ＣＮＴ) DELIMITED BY SIZE
                              数字左詰２Ｗ           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO 会提－レコードデータ
                       END-STRING
                       PERFORM 会提出Ｆ書込
      */温罨法料            
                       MOVE 作１－温罨法料(部位ＣＮＴ) TO 数字左詰Ｗ
                       PERFORM 数字左詰処理
                       STRING "          |onanpo_ryo|: "           DELIMITED BY SIZE
      *                        作１－温罨法料          (部位ＣＮＴ) DELIMITED BY SIZE
                              数字左詰２Ｗ           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO 会提－レコードデータ
                       END-STRING
                       PERFORM 会提出Ｆ書込
      */電療回数            
                       MOVE 作１－電療回数(部位ＣＮＴ) TO 数字左詰Ｗ
                       PERFORM 数字左詰処理
                       STRING "          |denryo_kaisu|: "         DELIMITED BY SIZE
      *                        作１－電療回数          (部位ＣＮＴ) DELIMITED BY SIZE
                              数字左詰２Ｗ           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO 会提－レコードデータ
                       END-STRING
                       PERFORM 会提出Ｆ書込
      */電療料              
                       MOVE 作１－電療料(部位ＣＮＴ) TO 数字左詰Ｗ
                       PERFORM 数字左詰処理
                       STRING "          |denryo_ryo|: "           DELIMITED BY SIZE
      *                        作１－電療料            (部位ＣＮＴ) DELIMITED BY SIZE
                              数字左詰２Ｗ           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO 会提－レコードデータ
                       END-STRING
                       PERFORM 会提出Ｆ書込
      */多部位逓減率        
                       MOVE 作１－多部位逓減率(部位ＣＮＴ) TO 数字左詰Ｗ
                       PERFORM 数字左詰処理
                       STRING "          |tabui_teigen_ritsu|: "   DELIMITED BY SIZE
      *                        作１－多部位逓減率      (部位ＣＮＴ) DELIMITED BY SIZE
                              数字左詰２Ｗ           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO 会提－レコードデータ
                       END-STRING
                       PERFORM 会提出Ｆ書込
      */多部位逓減額        
                       MOVE 作１－多部位逓減額(部位ＣＮＴ) TO 数字左詰Ｗ
                       PERFORM 数字左詰処理
                       STRING "          |tabui_teigen_gaku|: "    DELIMITED BY SIZE
      *                        作１－多部位逓減額      (部位ＣＮＴ) DELIMITED BY SIZE
                              数字左詰２Ｗ           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO 会提－レコードデータ
                       END-STRING
                       PERFORM 会提出Ｆ書込
      */長期逓減率          
                       MOVE 作１－長期逓減率(部位ＣＮＴ) TO 数字左詰Ｗ
                       PERFORM 数字左詰処理
                       STRING "          |choki_teigen_ritsu|: "   DELIMITED BY SIZE
      *                        作１－長期逓減率        (部位ＣＮＴ) DELIMITED BY SIZE
                              数字左詰２Ｗ           DELIMITED BY SPACE
                              ","                                  DELIMITED BY SIZE
                           INTO 会提－レコードデータ
                       END-STRING
                       PERFORM 会提出Ｆ書込
      */部位逓減率別料金計  
                       MOVE 作１－部位逓減率別料金計(部位ＣＮＴ) TO 数字左詰Ｗ
                       PERFORM 数字左詰処理
                       STRING "          |bui_teigen_ryokin_kei|: " DELIMITED BY SIZE
      *                        作１－部位逓減率別料金計(部位ＣＮＴ)  DELIMITED BY SIZE
                              数字左詰２Ｗ           DELIMITED BY SPACE
      *                        ","                    DELIMITED BY SIZE
                           INTO 会提－レコードデータ
                       END-STRING
                       PERFORM 会提出Ｆ書込
                   END-IF
               END-PERFORM
               IF 行番号存在Ｆ NOT = ZERO
                   MOVE "        }"   TO 会提－レコードデータ
                   PERFORM 会提出Ｆ書込
               END-IF
               MOVE "      ],"   TO 会提－レコードデータ
               PERFORM 会提出Ｆ書込
      */摘要
               STRING "      |tekiyo|: |" DELIMITED BY SIZE
                      作１－摘要          DELIMITED BY SPACE
                      "|,"                DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */同意年月日              作１－同意年月日     
               MOVE "      |doi_ymd|: ||,"   TO 会提－レコードデータ
               PERFORM 会提出Ｆ書込
      */同意医院                作１－同意医院       
               MOVE "      |doi_byoin|: ||,"   TO 会提－レコードデータ
               PERFORM 会提出Ｆ書込
      */同意医師名              作１－同意医師名     
               MOVE "      |doi_ishi|: ||,"   TO 会提－レコードデータ
               PERFORM 会提出Ｆ書込
      */保険種別詳細
               STRING "      |hoken_shubetsu|: |" DELIMITED BY SIZE
                  作１－保険種別詳細              DELIMITED BY SIZE
                      "|"                         DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
005580         PERFORM 作業ファイル１読込
               IF 終了フラグ３ = SPACE
                   MOVE "    },"   TO 会提－レコードデータ
               ELSE
                   MOVE "    }"    TO 会提－レコードデータ
               END-IF
               PERFORM 会提出Ｆ書込
           END-PERFORM.
           MOVE "  ],"    TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
      */保険者情報
           MOVE "  |hokensha_arr|: ["   TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
026580     OPEN INPUT 作業ファイル２.
026590         MOVE NC"作２" TO ファイル名.
026600         PERFORM オープンチェック.
           MOVE SPACE TO 終了フラグ３.
005580     PERFORM 作業ファイル２読込
005590     PERFORM UNTIL 終了フラグ３ = "YES"
               MOVE "    {"   TO 会提－レコードデータ
               PERFORM 会提出Ｆ書込
      */保険者番号
               STRING "      |hokensha_bango|: |" DELIMITED BY SIZE
                      作２－保険者番号            DELIMITED BY SIZE
                      "|,"                        DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */保険者名
               STRING "      |hokensha_mei|: |" DELIMITED BY SIZE
                      作２－保険者名            DELIMITED BY SPACE
                      "|,"                      DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */保険種別
               STRING "      |hoken_shubetsu|: |" DELIMITED BY SIZE
                      作２－保険種別              DELIMITED BY SIZE
                      "|,"                        DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */県コード
               STRING "      |ken_cd|: |" DELIMITED BY SIZE
                      作２－県コード      DELIMITED BY SIZE
                      "|,"                DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */郵便番号
               STRING "      |yubin|: |" DELIMITED BY SIZE
                      作２－郵便番号     DELIMITED BY SIZE
                      "|,"               DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */住所１  
      */－が・になる対策‐に変換/20230413
               INSPECT 作２－住所１   REPLACING ALL "－" BY "‐"
               STRING "      |jusho1|: |" DELIMITED BY SIZE
                      作２－住所１        DELIMITED BY SPACE
                      "|,"                DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */住所２  
               STRING "      |jusho2|: |" DELIMITED BY SIZE
                      作２－住所２        DELIMITED BY SPACE
                      "|,"                DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */電話番号
               STRING "      |tel|: |" DELIMITED BY SIZE
                      作２－電話番号   DELIMITED BY SIZE
                      "|,"             DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
      */口座番号
               STRING "      |koza_bango|: |" DELIMITED BY SIZE
                      作２－口座番号          DELIMITED BY SIZE
                      "|"                     DELIMITED BY SIZE
                   INTO 会提－レコードデータ
               END-STRING
               PERFORM 会提出Ｆ書込
005580         PERFORM 作業ファイル２読込
               IF 終了フラグ３ = SPACE
                   MOVE "    },"   TO 会提－レコードデータ
               ELSE
                   MOVE "    }"    TO 会提－レコードデータ
               END-IF
               PERFORM 会提出Ｆ書込
           END-PERFORM.
           MOVE "  ]"   TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
           MOVE "}"   TO 会提－レコードデータ.
           PERFORM 会提出Ｆ書込.
           CLOSE 会提出ファイル 作業ファイル１ 作業ファイル２.
           PERFORM 文字変換処理.
023755*================================================================*
005580 作業ファイル１読込 SECTION.
012920*
012930     READ 作業ファイル１ NEXT
012940     AT END
012950         MOVE "YES" TO 終了フラグ３
012960     END-READ.
012970*
023755*================================================================*
       会提出Ｆ書込 SECTION.
      *
           MOVE SPACE TO 終了フラグ２.
      *     PERFORM VARYING 文字ＣＮＴ FROM 200 BY -1
           PERFORM VARYING 文字ＣＮＴ FROM 500 BY -1
                   UNTIL   (文字ＣＮＴ      <= ZERO ) OR
                           (終了フラグ２ NOT = SPACE)
               IF 会提－レコードデータ(文字ＣＮＴ:1) NOT = SPACE
                   COMPUTE 文字ＣＮＴ = 文字ＣＮＴ + 1
                   MOVE "YES" TO 終了フラグ２
               END-IF
           END-PERFORM.
      *
           INSPECT 会提－レコードデータ   REPLACING ALL "|" BY 区切文字Ｗ２.
           INSPECT 会提－レコードデータ   REPLACING ALL 改行 BY "\r".
           WRITE 会提－レコード
           IF 状態キー  NOT =  "00"
               MOVE NC"会提"  TO ファイル名
               PERFORM エラー表示
           END-IF.
           INITIALIZE    会提－レコードデータ.
           MOVE SPACE TO 会提－レコードデータ.
014200*================================================================*
       文字数判定 SECTION.
      *
           MOVE SPACE TO 終了フラグ２.
           PERFORM VARYING 文字ＣＮＴ FROM 100 BY -1
                   UNTIL   (文字ＣＮＴ      <= ZERO ) OR
                           (終了フラグ２ NOT = SPACE)
               IF 文字数判定Ｗ(文字ＣＮＴ:1) NOT = SPACE
                   COMPUTE 文字ＣＮＴ = 文字ＣＮＴ + 1
                   MOVE "YES" TO 終了フラグ２
               END-IF
           END-PERFORM.
014200*================================================================*
005580 作業ファイル２読込 SECTION.
012920*
012930     READ 作業ファイル２ NEXT
012940     AT END
012950         MOVE "YES" TO 終了フラグ３
012960     END-READ.
012970*
023755*================================================================*
       圧縮ファイル作成 SECTION.
      *
           IF 連入－固定フラグ = 1
002890        MOVE "C:\makishisys\REZEPT.zip" TO 圧縮ファイル名Ｗ
           ELSE
003324        MOVE 連入－ドライブ TO ドライブＷＲ
007096        STRING ドライブＷＲ       DELIMITED BY SIZE
007097               ":\REZEPT.zip"    DELIMITED BY SIZE
007098               INTO 圧縮ファイル名Ｗ
007099        END-STRING
           END-IF.
002890     MOVE "C:\MAKISHISYS\YAWOBJ\TEMP\REZEPT.json" TO 元ファイル名Ｗ
      *
            MOVE SPACE TO パラ１Ｗ.
      *      
      * 圧縮コマンド:"a -tzip"
            STRING "a -tzip"  DELIMITED BY SIZE
      *　　　　　  /圧縮ファイル名
                   " "              DELIMITED BY SIZE
                   圧縮ファイル名Ｗ DELIMITED BY SPACE
      /
      *　　　　　  /元のファイル名
                   " "              DELIMITED BY SIZE
                   元ファイル名Ｗ   DELIMITED BY SPACE
      *
      *            /処理ダイアログ出さない時指定
                   " -hide"  DELIMITED BY SIZE
      *
      *            /パスワードは、 -pに続けで指定
                   " -pIwa#Re0pt"  DELIMITED BY SIZE
                   INTO パラ１Ｗ
            END-STRING.
            MOVE "zip" TO プログラム名Ｗ.
            CALL プログラム名Ｗ WITH C LINKAGE
                      USING BY REFERENCE パラ１Ｗ.
      *
            IF PROGRAM-STATUS NOT = ZERO
      *         INVOKE POW-SELF "DisplayMessage" USING "圧縮失敗！！" "メッセージ"
002470         MOVE  NC"　　　　　　圧縮失敗！！" TO 連メ－メッセージ
002480         CALL   "MSG001"
002490         CANCEL "MSG001"
            END-IF.     
      *
      *      MOVE "delfile" TO プログラム名Ｗ.
      *      CALL プログラム名Ｗ WITH C LINKAGE
      *              USING BY REFERENCE 元ファイル名Ｗ.
028706*================================================================*
       文字変換処理 SECTION.
      *
      */SJISからUTF8に変換
001987     MOVE "C:\makishisys\YAWOBJ\YIW1011.bat"  TO フルパス名Ｗ.
001988*
001989     CALL "gofile2"  WITH C LINKAGE
001990                     USING BY REFERENCE フルパス名Ｗ.
001991*
001992      IF PROGRAM-STATUS NOT = ZERO 
001993          DISPLAY "ファイルを開けませんでした。" UPON CONS
001996      END-IF.
      *
HILO  *     ACCEPT  キー入力 FROM CONS.
      */ファイルが存在するまで繰り返す
           MOVE ZERO TO 遅延カウンタ
           OPEN INPUT 会提出ファイル０
           PERFORM UNTIL 状態キー NOT = "35" OR 遅延カウンタ > 999990
               COMPUTE 遅延カウンタ = 遅延カウンタ + 1
               OPEN INPUT 会提出ファイル０
           END-PERFORM
           CLOSE 会提出ファイル０
           MOVE 遅延カウンタ TO 遅延回数Ｗ.
026650     PERFORM 遅延処理.
028706*================================================================*
       空白詰処理 SECTION.
      *
HILO  *     DISPLAY "空白１" 空白詰Ｗ
           PERFORM VARYING 文字ＣＮＴ FROM 1 BY 1
                   UNTIL   文字ＣＮＴ > 200
               IF 空白詰Ｗ(文字ＣＮＴ:1) = SPACE
                   MOVE 空白詰Ｗ(文字ＣＮＴ + 1:200 - 文字ＣＮＴ) TO 空白詰Ｗ(文字ＣＮＴ:200 - 文字ＣＮＴ + 1)
               END-IF
           END-PERFORM.
HILO  *     DISPLAY "空白２" 空白詰Ｗ.
028706*================================================================*
       数字左詰処理 SECTION.
      *
           MOVE SPACE TO 終了フラグ４.
           PERFORM VARYING 文字ＣＮＴ FROM 1 BY 1 UNTIL 文字ＣＮＴ > 5 OR 終了フラグ４ NOT = SPACE
               IF 数字左詰Ｗ(文字ＣＮＴ:1) = "0"
                   MOVE SPACE TO 数字左詰Ｗ(文字ＣＮＴ:1)
               ELSE
                   MOVE "YES" TO 終了フラグ４
               END-IF
           END-PERFORM.
           MOVE SPACE TO 終了フラグ４ 数字左詰２Ｗ.
           PERFORM VARYING 文字ＣＮＴ FROM 1 BY 1 UNTIL 文字ＣＮＴ > 6 OR 終了フラグ４ NOT = SPACE
               IF 数字左詰Ｗ(文字ＣＮＴ:1) NOT = SPACE
                   MOVE 数字左詰Ｗ(文字ＣＮＴ:7 - 文字ＣＮＴ) TO 数字左詰２Ｗ
                   MOVE "YES" TO 終了フラグ４
               END-IF
           END-PERFORM.
028706*================================================================*
       並び順設定 SECTION.
      *
      *      MOVE 入力－プレビュー区分 TO 連入－プレビュー区分
            MOVE ZERO                 TO 連入－プレビュー区分
            CALL   "YIW581".
            CANCEL "YIW581".
028706*================================================================*
       全角半角チェック SECTION.
      *
028110     MOVE ZERO TO カウンタ１ カウンタ２.
028120     PERFORM VARYING カウンタ１ FROM 1 BY 1
028130             UNTIL   (カウンタ１ > 120) OR (カウンタ２ >= 60)
028140         IF ((漢字チェックＷ(カウンタ１) >= X"81") AND (漢字チェックＷ(カウンタ１) <= X"9F" )) OR
028150            ((漢字チェックＷ(カウンタ１) >= X"E0") AND (漢字チェックＷ(カウンタ１) <= X"FC" ))
                   MOVE カウンタ１ TO カウンタ３
028160             COMPUTE カウンタ１ = カウンタ１ + 1
028180             COMPUTE カウンタ２ = カウンタ２ + 1
028170         ELSE
                   MOVE カウンタ１ TO カウンタ３
028180             COMPUTE カウンタ２ = カウンタ２ + 1
028190         END-IF
028200     END-PERFORM.
028706*================================================================*
028670*******************************************************************
028680* END PROGRAM YIW1011.
028690*******************************************************************
