000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YIW101.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*      アイワ 会提出フロッピー作成【FPD書込】
000100* 請求年月Ver.
000110*      MED = YIW580
000120*----------------------------------------------------------------*
000130 DATE-WRITTEN.           2015-09-09
000140 DATE-COMPILED.          2015-09-09
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
001291* 2006.12.21 作成するファイルの保存先をTEMPに変更
001300     SELECT ユーザーファイル   ASSIGN    TO "C:\MAKISHISYS\YAWOBJ\TEMP\USER.TXT"
001310                             ORGANIZATION             IS  LINE SEQUENTIAL
001320                             ACCESS MODE              IS  SEQUENTIAL
001330                             FILE STATUS              IS  状態キー
001340                             LOCK        MODE         IS  AUTOMATIC.
001350     SELECT 患者情報ファイル ASSIGN  TO "C:\MAKISHISYS\YAWOBJ\TEMP\SAKURA.DAT"
001360*                             ORGANIZATION             IS  LINE SEQUENTIAL
001360                             ORGANIZATION             IS  SEQUENTIAL
001370                             ACCESS MODE              IS  SEQUENTIAL
001380                             FILE STATUS              IS  状態キー
001390                             LOCK        MODE         IS  AUTOMATIC.
001400     SELECT 保険者ファイル   ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\HOKEN.CSV"
001410                             ORGANIZATION             IS  LINE SEQUENTIAL
001420                             ACCESS MODE              IS  SEQUENTIAL
001430                             FILE STATUS              IS  状態キー
001440                             LOCK        MODE         IS  AUTOMATIC.
001450     SELECT 作業保険者ファイル ASSIGN    TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1011L.DAT"
001460                             ORGANIZATION             IS  INDEXED
001470                             ACCESS MODE              IS  DYNAMIC
001480                             RECORD KEY               IS  作保険－保険種別キー
001490                                                          作保険－保険者番号キー
001491                             ALTERNATE RECORD KEY     IS  作保険－保険種別
001492                                                          作保険－保険者番号キー
001500                             FILE STATUS              IS  状態キー
001510                             LOCK        MODE         IS  AUTOMATIC.
001130     SELECT  作業ファイル１  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W5811L.DAT"
001140                             ORGANIZATION             IS  INDEXED
001150                             ACCESS                   IS  DYNAMIC
001160                             RECORD      KEY          IS  作１－印刷順序
001180                                                          作１－保番
001170                                                          作１－種別
001190                                                          作１－保険者番号
001170                                                          作１－本人家族区分
001200                                                          作１－患者コード
001210                                                          作１－施術和暦年月
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
001960 FD  ユーザーファイル RECORD IS VARYING IN SIZE
001970               FROM 1 TO 50 DEPENDING ON 文字カウンタ.
001980* FD  ユーザーファイル RECORD  CONTAINS 718 CHARACTERS.
001990 01  ユ－レコード.
002000     03  ユ－レコードデータ                PIC X(50).
002010*
002020* FD  患者情報ファイル RECORD IS VARYING IN SIZE
002030*               FROM 1 TO 1272 DEPENDING ON 文字カウンタ.
       FD  患者情報ファイル RECORD  CONTAINS 956 CHARACTERS.
002040 01 患－レコード.
002050   03 患－レコードデータ.
002060     05 患－会員番号                      PIC X(4).
002080     05 患－定額制受理区分                PIC 9(1).
002500     05 患－請求年月                      PIC 9(6).
002080     05 患－レセプト番号                  PIC 9(4).
002520     05 患－施術年月                      PIC 9(6).
002080     05 患－レセ種別                      PIC 9(1).
002080     05 患－保険者番号                    PIC X(8).
002340     05 患－保険証記号                    PIC X(20).
002360     05 患－保険証番号                    PIC X(16).
002260     05 患－老健市町村番号                PIC X(8).
002380     05 患－老健受給者番号                PIC X(8).
002300     05 患－助成負担者番号                PIC X(8).
002400     05 患－助成受給者番号                PIC X(16).
002200     05 患－被保険者氏名漢字              PIC X(20).
002180     05 患－被保険者氏名カナ              PIC X(16).
002160     05 患－患者氏名漢字                  PIC X(20).
002140     05 患－患者氏名カナ                  PIC X(16).
002120     05 患－本人家族区分                  PIC 9(1).
002440     05 患－性別                          PIC 9(1).
002420     05 患－生年月日                      PIC X(8).
002080     05 患－被保険者住所                  PIC X(28).
002640     05 患－費用額                        PIC 9(6).
002080     05 患－給付割合                      PIC 9(2).
002660     05 患－一部負担金                    PIC 9(6).
002680     05 患－請求金額                      PIC 9(6).
002740     05 患－部位数                        PIC 9(1).
003280     05 患－負傷原因                      PIC X(20).
003400     05 患－長期理由                      PIC X(32).
002100     05 患－新規                          PIC 9(1).
002100     05 患－継続                          PIC 9(1).
002100     05 患－初検回数                      PIC 9(1).
002100     05 患－初検時間外回数                PIC 9(1).
002100     05 患－初検休日回数                  PIC 9(1).
002100     05 患－初検深夜回数                  PIC 9(1).
002100     05 患－再検回数                      PIC 9(1).
002220     05 患－往療距離                      PIC 9(3).
002220     05 患－往療回数                      PIC 9(2).
002100     05 患－往療夜間回数                  PIC 9(1).
002100     05 患－往療難路回数                  PIC 9(2).
002100     05 患－往療暴風雨雪回数              PIC 9(2).
002240     05 患－金属副子大回数                PIC 9(1).
002240     05 患－金属副子中回数                PIC 9(1).
002240     05 患－金属副子小回数                PIC 9(1).
002280     05 患－情報提供回数                  PIC 9(1).
002320     05 患－負傷区分１                    PIC 9(1).
002460     05 患－負傷名１                      PIC N(16).
002960     05 患－初回処置回数１                PIC 9(1).
002980     05 患－負傷年月日１                  PIC 9(8).
003000     05 患－初検年月日１                  PIC 9(8).
002540     05 患－施術開始日１                  PIC 9(8).
002560     05 患－施術終了日１                  PIC 9(8).
002580     05 患－実日数１                      PIC 9(2).
003080     05 患－転帰区分１                    PIC 9(1).
003100     05 患－後療回数１                    PIC 9(2).
003120     05 患－冷罨法回数１                  PIC 9(1).
003140     05 患－温罨法回数１                  PIC 9(2).
003180     05 患－電療回数１                    PIC 9(2).
003200     05 患－多部位逓減区分１              PIC 9(1).
003220     05 患－長期逓減区分１                PIC 9(1).
002320     05 患－負傷区分２                    PIC 9(1).
002460     05 患－負傷名２                      PIC N(16).
002960     05 患－初回処置回数２                PIC 9(1).
002980     05 患－負傷年月日２                  PIC 9(8).
003000     05 患－初検年月日２                  PIC 9(8).
002540     05 患－施術開始日２                  PIC 9(8).
002560     05 患－施術終了日２                  PIC 9(8).
002580     05 患－実日数２                      PIC 9(2).
003080     05 患－転帰区分２                    PIC 9(1).
003100     05 患－後療回数２                    PIC 9(2).
003120     05 患－冷罨法回数２                  PIC 9(1).
003140     05 患－温罨法回数２                  PIC 9(2).
003180     05 患－電療回数２                    PIC 9(2).
003200     05 患－多部位逓減区分２              PIC 9(1).
003220     05 患－長期逓減区分２                PIC 9(1).
002320     05 患－負傷区分３                    PIC 9(1).
002460     05 患－負傷名３                      PIC N(16).
002960     05 患－初回処置回数３                PIC 9(1).
002980     05 患－負傷年月日３                  PIC 9(8).
003000     05 患－初検年月日３                  PIC 9(8).
002540     05 患－施術開始日３                  PIC 9(8).
002560     05 患－施術終了日３                  PIC 9(8).
002580     05 患－実日数３                      PIC 9(2).
003080     05 患－転帰区分３                    PIC 9(1).
003100     05 患－後療回数３                    PIC 9(2).
003120     05 患－冷罨法回数３                  PIC 9(1).
003140     05 患－温罨法回数３                  PIC 9(2).
003180     05 患－電療回数３                    PIC 9(2).
003200     05 患－多部位逓減区分３              PIC 9(1).
003220     05 患－長期逓減区分３                PIC 9(1).
002320     05 患－負傷区分４                    PIC 9(1).
002460     05 患－負傷名４                      PIC N(16).
002960     05 患－初回処置回数４                PIC 9(1).
002980     05 患－負傷年月日４                  PIC 9(8).
003000     05 患－初検年月日４                  PIC 9(8).
002540     05 患－施術開始日４                  PIC 9(8).
002560     05 患－施術終了日４                  PIC 9(8).
002580     05 患－実日数４                      PIC 9(2).
003080     05 患－転帰区分４                    PIC 9(1).
003100     05 患－後療回数４                    PIC 9(2).
003120     05 患－冷罨法回数４                  PIC 9(1).
003140     05 患－温罨法回数４                  PIC 9(2).
003180     05 患－電療回数４                    PIC 9(2).
003200     05 患－多部位逓減区分４              PIC 9(1).
003220     05 患－長期逓減区分４                PIC 9(1).
002320     05 患－負傷区分５                    PIC 9(1).
002460     05 患－負傷名５                      PIC N(16).
002960     05 患－初回処置回数５                PIC 9(1).
002980     05 患－負傷年月日５                  PIC 9(8).
003000     05 患－初検年月日５                  PIC 9(8).
002540     05 患－施術開始日５                  PIC 9(8).
002560     05 患－施術終了日５                  PIC 9(8).
002580     05 患－実日数５                      PIC 9(2).
003080     05 患－転帰区分５                    PIC 9(1).
003100     05 患－後療回数５                    PIC 9(2).
003120     05 患－冷罨法回数５                  PIC 9(1).
003140     05 患－温罨法回数５                  PIC 9(2).
003180     05 患－電療回数５                    PIC 9(2).
003200     05 患－多部位逓減区分５              PIC 9(1).
003220     05 患－長期逓減区分５                PIC 9(1).
002320     05 患－負傷区分６                    PIC 9(1).
002460     05 患－負傷名６                      PIC N(16).
002960     05 患－初回処置回数６                PIC 9(1).
002980     05 患－負傷年月日６                  PIC 9(8).
003000     05 患－初検年月日６                  PIC 9(8).
002540     05 患－施術開始日６                  PIC 9(8).
002560     05 患－施術終了日６                  PIC 9(8).
002580     05 患－実日数６                      PIC 9(2).
003080     05 患－転帰区分６                    PIC 9(1).
003100     05 患－後療回数６                    PIC 9(2).
003120     05 患－冷罨法回数６                  PIC 9(1).
003140     05 患－温罨法回数６                  PIC 9(2).
003180     05 患－電療回数６                    PIC 9(2).
003200     05 患－多部位逓減区分６              PIC 9(1).
003220     05 患－長期逓減区分６                PIC 9(1).
002320     05 患－負傷区分７                    PIC 9(1).
002460     05 患－負傷名７                      PIC N(16).
002960     05 患－初回処置回数７                PIC 9(1).
002980     05 患－負傷年月日７                  PIC 9(8).
003000     05 患－初検年月日７                  PIC 9(8).
002540     05 患－施術開始日７                  PIC 9(8).
002560     05 患－施術終了日７                  PIC 9(8).
002580     05 患－実日数７                      PIC 9(2).
003080     05 患－転帰区分７                    PIC 9(1).
003100     05 患－後療回数７                    PIC 9(2).
003120     05 患－冷罨法回数７                  PIC 9(1).
003140     05 患－温罨法回数７                  PIC 9(2).
003180     05 患－電療回数７                    PIC 9(2).
003200     05 患－多部位逓減区分７              PIC 9(1).
003220     05 患－長期逓減区分７                PIC 9(1).
002320     05 患－負傷区分８                    PIC 9(1).
002460     05 患－負傷名８                      PIC N(16).
002960     05 患－初回処置回数８                PIC 9(1).
002980     05 患－負傷年月日８                  PIC 9(8).
003000     05 患－初検年月日８                  PIC 9(8).
002540     05 患－施術開始日８                  PIC 9(8).
002560     05 患－施術終了日８                  PIC 9(8).
002580     05 患－実日数８                      PIC 9(2).
003080     05 患－転帰区分８                    PIC 9(1).
003100     05 患－後療回数８                    PIC 9(2).
003120     05 患－冷罨法回数８                  PIC 9(1).
003140     05 患－温罨法回数８                  PIC 9(2).
003180     05 患－電療回数８                    PIC 9(2).
003200     05 患－多部位逓減区分８              PIC 9(1).
003220     05 患－長期逓減区分８                PIC 9(1).
002080     05 患－前期負担割合                  PIC 9(2).
002080     05 患－提出                          PIC 9(1).
002080     05 患－患者番号                      PIC 9(6).
002080     05 患－枝番                          PIC X(2).
002080     05 患－会番号                        PIC X(10).
002080     05 患－保険種別                      PIC X(1).
003600*     05 ヘッダ用スペース                  PIC X(316).
003603*
003604 FD  保険者ファイル RECORD IS VARYING IN SIZE
003605               FROM 1 TO 334 DEPENDING ON 文字カウンタ.
003606 01  保険－レコード.
003607   03 保険－レコードデータ.
003608     05 保険－接骨院ＩＤ                    PIC X(10).
003609     05 保険－区切文字１                    PIC X.
003610     05 保険－保険種別                      PIC 9(2).
003611     05 保険－区切文字２                    PIC X.
003612     05 保険－保険者番号                    PIC X(10).
003613     05 保険－区切文字３                    PIC X.
003614     05 保険－保険者識別ＩＤ                PIC 9.
003615     05 保険－区切文字４                    PIC X.
003616     05 保険－保険者名称                    PIC X(60).
003617     05 保険－区切文字５                    PIC X.
003618     05 保険－郵送先名称                    PIC X(60).
003619     05 保険－区切文字６                    PIC X.
003620     05 保険－印字用名称                    PIC X(60).
003621     05 保険－区切文字７                    PIC X.
003622     05 保険－県コード                      PIC X(2).
003623     05 保険－区切文字８                    PIC X.
003624     05 保険－郵便番号                      PIC X(7).
003625     05 保険－区切文字９                    PIC X.
003626     05 保険－住所１                        PIC X(40).
003627     05 保険－区切文字１０                  PIC X.
003628     05 保険－住所２                        PIC X(40).
003629     05 保険－区切文字１１                  PIC X.
003630     05 保険－ＴＥＬ                        PIC X(15).
003631     05 保険－区切文字１２                  PIC X.
003632     05 保険－本人負担区分                  PIC 9(1).
003633     05 保険－区切文字１３                  PIC X.
003634     05 保険－本人負担率                    PIC 9(1).
003635     05 保険－区切文字１４                  PIC X.
003636     05 保険－家族負担区分                  PIC 9(1).
003637     05 保険－区切文字１５                  PIC X.
003638     05 保険－家族負担率                    PIC 9(1).
003639     05 保険－区切文字１６                  PIC X.
003640     05 保険－入金予定日                    PIC X(1).
003641     05 保険－区切文字１７                  PIC X.
003642     05 保険－入金期間                      PIC X(1).
003643     05 保険－区切文字１８                  PIC X.
003644     05 保険－共済区分                      PIC 9(1).
003645     05 保険－区切文字１９                  PIC X.
003646     05 保険－更新日付                      PIC X(1).
003647
004060*                           ［ＲＬ＝  256］
004070 FD  作業保険者ファイル BLOCK   CONTAINS   328   RECORDS.
004080 01 作保険－レコード.
004090   03 作保険－レコードキー.
004100     05 作保険－保険種別キー                PIC 9(2).
004110     05 作保険－保険者番号キー              PIC X(10).
004120   03 作保険－レコードデータ.
004130     05 作保険－接骨院ＩＤ                  PIC X(10).
004140     05 作保険－保険種別                    PIC 9(2).
004141     05 作保険－保険種別２                  PIC X(2).
004150     05 作保険－保険者番号                  PIC X(10).
004160     05 作保険－保険者識別ＩＤ              PIC 9.
004170     05 作保険－保険者名称                  PIC X(60).
004180     05 作保険－郵送先名称                  PIC X(60).
004190     05 作保険－印字用名称                  PIC X(60).
004200     05 作保険－県コード                    PIC X(2).
004210     05 作保険－郵便番号                    PIC X(7).
004220     05 作保険－住所１                      PIC X(40).
004230     05 作保険－住所２                      PIC X(40).
004240     05 作保険－ＴＥＬ                      PIC X(15).
004250     05 作保険－本人負担区分                PIC 9(1).
004260     05 作保険－本人負担率                  PIC 9(1).
004270     05 作保険－家族負担区分                PIC 9(1).
004280     05 作保険－家族負担率                  PIC 9(1).
004290     05 作保険－入金予定日                  PIC X(1).
004300     05 作保険－入金期間                    PIC X(1).
004310     05 作保険－共済区分                    PIC 9(1).
004320     05 作保険－更新日付                    PIC X(10).
001510*
001520 FD  作業ファイル１ RECORD  CONTAINS 176 CHARACTERS.
001530 01  作１－レコード.
001540     03  作１－レコードキー.
001590         05  作１－印刷順序.
                   07  作１－順番                PIC 9(2).
001570             07  作１－県                  PIC 9(2).
001580             07  作１－保種                PIC 9(1).
001720         05  作１－保番                    PIC X(6).
001600         05  作１－種別                    PIC X(2).
001710         05  作１－保険者番号              PIC X(10).
001700         05  作１－本人家族区分            PIC 9(1).
001620         05  作１－患者コード.
001630             07  作１－患者番号            PIC 9(6).
001640             07  作１－枝番                PIC X(1).
001650         05  作１－施術和暦年月.
001660             07  作１－施術和暦            PIC 9(1).
001670             07  作１－施術年              PIC 9(2).
001680             07  作１－施術月              PIC 9(2).
001690     03  作２－レコードデータ.
001600         05  作１－保険種別                PIC 9(2).
001600         05  作１－助成種別                PIC 9(2).
001740         05  作１－患者氏名                PIC X(50).
001750         05  作１－被保険者氏名            PIC X(50).
001880         05  作１－実日数                  PIC 9(2).
001790         05  作１－費用額                  PIC 9(7).
001800         05  作１－負担額                  PIC 9(7).
001810         05  作１－請求額                  PIC 9(7).
002360         05  作１－前期区分                PIC 9(1).
002370         05  FILLER                        PIC X(12).
004471*
004472 FD  会提出ファイル RECORD  CONTAINS 8 CHARACTERS.
004473 01  会提－レコード.
004474     03  会提－レコードデータ.
004475         05  会提－会員番号              PIC 9(8).
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
002090 01 エラーフラグ                       PIC X(3) VALUE SPACE.
004590 01 実行キーＷ                         PIC X(3)  VALUE SPACE.
004600 01 施術記録有Ｗ                       PIC X(3) VALUE SPACE.
004610 01 ファイル名                         PIC N(8) VALUE SPACE.
004620 01 カウンタ                           PIC 9(1) VALUE ZERO.
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
004790 01 西暦年Ｗ                           PIC 9(4) VALUE ZERO.
004800* 01 和暦Ｗ                             PIC 9(1) VALUE ZERO.
004810* 01 年Ｗ                               PIC 9(2) VALUE ZERO.
004820 01 和暦年月日Ｗ.
004830   03 和暦年月Ｗ.
004840      05 和暦Ｗ                        PIC 9(1) VALUE ZERO.
004850      05 年Ｗ                          PIC 9(2) VALUE ZERO.
004860      05 月Ｗ                          PIC 9(2) VALUE ZERO.
004870   03 日Ｗ                             PIC 9(2) VALUE ZERO.
004880 01 日付８桁Ｗ                         PIC 9(8) VALUE ZERO.
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
005120 01 遅延カウンタ                       PIC 9(4) VALUE ZERO.
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
       01 連入－画面情報ＹＩＷ５８０ IS EXTERNAL.
          03 連入－請求和暦年月.
             05 連入－請求和暦               PIC 9.
             05 連入－請求年                 PIC 9(2).
             05 連入－請求月                 PIC 9(2).
          03 連入－プレビュー区分            PIC 9(1).
      *
       01 連入－画面情報ＹＩＷ１００ IS EXTERNAL.
          03 連入－ドライブ                  PIC X(1).
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
006380     PERFORM 初期化.
006390************
006400*           *
006410* 主処理     *
006420*           *
006430************
006440     PERFORM 制御情報取得.
006450     PERFORM 連結項目退避.
006460     PERFORM ユーザーファイル作成.
006480     PERFORM 患者情報ファイル作成.
006490     PERFORM 保険者ファイル作成.
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
004140     OPEN INPUT  名称マスタ
004150         MOVE NC"名称" TO ファイル名.
004160         PERFORM オープンチェック.
006630     OPEN INPUT レセプトＦ.
006640         MOVE NC"レセ" TO ファイル名.
006650         PERFORM オープンチェック.
006710     OPEN INPUT 負傷データＦ.
006720             MOVE NC"負傷" TO ファイル名.
006730             PERFORM オープンチェック.
014910     OPEN INPUT   負傷原因Ｆ.
014920         MOVE NC"負傷原因" TO ファイル名.
014930         PERFORM オープンチェック.
002650     OPEN INPUT   長期継続者Ｆ.
002651         MOVE NC"長期継続者Ｆ" TO ファイル名.
002652         PERFORM オープンチェック.
006740     OPEN INPUT 施術記録Ｆ.
006750             MOVE NC"施記" TO ファイル名.
006760             PERFORM オープンチェック.
006770     OPEN INPUT 施術所情報マスタ
006780         MOVE NC"施情" TO ファイル名.
006790         PERFORM オープンチェック.
006800     OPEN INPUT 元号マスタ.
006810             MOVE NC"元号" TO ファイル名.
006820             PERFORM オープンチェック.
006830     OPEN INPUT 保険者マスタ.
006840             MOVE NC"保険者マスタ" TO ファイル名.
006850             PERFORM オープンチェック.
006860     OPEN INPUT 制御情報マスタ.
006870             MOVE NC"制御" TO ファイル名.
006880             PERFORM オープンチェック.
006890     OPEN INPUT 負担率マスタ.
006900             MOVE NC"負率" TO ファイル名.
006910             PERFORM オープンチェック.
004870     OPEN INPUT 保険者特別負担マスタ.
004880         MOVE NC"保険者特別負担マスタ" TO ファイル名.
004890         PERFORM オープンチェック.
006920     OPEN INPUT 市町村マスタ.
006930             MOVE NC"市町" TO ファイル名.
006940             PERFORM オープンチェック.
006630     OPEN INPUT 生保情報Ｆ.
006640         MOVE NC"生保" TO ファイル名.
006650         PERFORM オープンチェック.
006630     OPEN INPUT 労災情報Ｆ.
006640         MOVE NC"労災" TO ファイル名.
006650         PERFORM オープンチェック.
006950     OPEN INPUT   会情報マスタ.
006960         MOVE NC"会情報マスタ" TO ファイル名.
006970         PERFORM オープンチェック.
006980*
006990     OPEN OUTPUT ユーザーファイル.
007000     IF 状態キー  =  "30"
007010         MOVE  NC"　　　ドライブがありません。" TO 連メ－メッセージ
007020         CALL   "MSG001"
007030         CANCEL "MSG001"
007040         MOVE 99 TO PROGRAM-STATUS
007050         EXIT PROGRAM
007060     ELSE
007070         MOVE NC"ユーザー" TO ファイル名
007080         PERFORM オープンチェック
007090     END-IF.
007091
007094* フロッピー挿入確認
007095* ダミーファイルをオープンし、オープンできたらファイルを削除
003324     MOVE 連入－ドライブ TO ドライブＷＲ.
007096     STRING ドライブＷＲ       DELIMITED BY SIZE
007097            ":\REZDATA.LZH"    DELIMITED BY SIZE
007098           INTO FD-NAME
007099     END-STRING.
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
007130     OPEN OUTPUT 保険者ファイル.
007140         MOVE NC"保険者" TO ファイル名.
007150         PERFORM オープンチェック.
007117     OPEN OUTPUT 患者情報ファイル.
007118         MOVE NC"患者情報" TO ファイル名.
007120         PERFORM オープンチェック.
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
007300     CLOSE 受診者情報Ｆ      施術所情報マスタ  負傷データＦ    名称マスタ
007310           元号マスタ        制御情報マスタ    保険者マスタ    保険者特別負担マスタ
007320           負担率マスタ      市町村マスタ      会情報マスタ    生保情報Ｆ
007330           ユーザーファイル  患者情報ファイル  保険者ファイル  労災情報Ｆ
                 レセプトＦ        負傷原因Ｆ        長期継続者Ｆ.
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
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
007450     ACCEPT  キー入力 FROM CONS.
007460     PERFORM ファイル閉鎖.
007470     MOVE 99 TO PROGRAM-STATUS.
007480     EXIT PROGRAM.
007490*================================================================*
007500 ユーザーファイル作成 SECTION.
007510*
007520     PERFORM 施術所情報取得.
007530     PERFORM 会情報取得.
007540*/ヘッダ
007550     INITIALIZE ユ－レコード.
007560     MOVE "[DATA]"           TO ユ－レコードデータ.
007570     PERFORM ユーザーファイル書込.
007580*/会種別：０固定
007590     INITIALIZE ユ－レコード.
007600     MOVE "会種別=0"         TO ユ－レコードデータ.
007610     PERFORM ユーザーファイル書込.
007620*/県コード：１３固定
007630     INITIALIZE ユ－レコード.
007640*     MOVE "県コード=13"       TO ユ－レコードデータ.
            STRING  "県コード="           DELIMITED BY SIZE
                     施情－都道府県ＪＩＳ DELIMITED BY SIZE
007700       INTO ユ－レコードデータ
007710     END-STRING.
007650     PERFORM ユーザーファイル書込.
007660*/会長名カナ
007670     INITIALIZE ユ－レコード.
007680     STRING "会長氏名（カナ）="    DELIMITED BY SIZE
007690            会情－接骨師会会長カナ DELIMITED BY SIZE
007700       INTO ユ－レコードデータ
007710     END-STRING.
007720     PERFORM ユーザーファイル書込.
007730*/会長名漢字
007740     INITIALIZE ユ－レコード.
007750     MOVE 会情－接骨師会会長名 TO 英数字項目Ｗ.
007760     INSPECT 英数字項目ＸＷ REPLACING ALL "　" BY "  ".
007770     STRING "会長氏名（漢字）=" DELIMITED BY SIZE
007780            英数字項目ＸＷ      DELIMITED BY SIZE
007790       INTO ユ－レコードデータ
007800     END-STRING.
007810     PERFORM ユーザーファイル書込.
007820*/接骨院名カナ
007830     INITIALIZE ユ－レコード.
007840     STRING "接骨院（カナ）="  DELIMITED BY SIZE
007850            施情－接骨院名カナ DELIMITED BY SIZE
007860       INTO ユ－レコードデータ
007870     END-STRING.
007880     PERFORM ユーザーファイル書込.
007890*/接骨院名
007900     INITIALIZE ユ－レコード.
007910     MOVE 施情－接骨院名 TO 英数字項目Ｗ.
007920     INSPECT 英数字項目ＸＷ REPLACING ALL "　" BY "  ".
007930     STRING "接骨院（漢字）=" DELIMITED BY SIZE
007940            英数字項目ＸＷ    DELIMITED BY SIZE
007950       INTO ユ－レコードデータ
007960     END-STRING.
007970     PERFORM ユーザーファイル書込.
007980*/代表者カナ
007990     INITIALIZE ユ－レコード.
008000     STRING "柔整師氏名（カナ）=" DELIMITED BY SIZE
008010            施情－代表者カナ      DELIMITED BY SIZE
008020       INTO ユ－レコードデータ
008030     END-STRING.
008040     PERFORM ユーザーファイル書込.
008050*/代表者名
008060     INITIALIZE ユ－レコード.
008070     MOVE 施情－代表者名 TO 英数字項目Ｗ.
008080     INSPECT 英数字項目ＸＷ REPLACING ALL "　" BY "  ".
008090     STRING "柔整師氏名（漢字）=" DELIMITED BY SIZE
008100            英数字項目ＸＷ        DELIMITED BY SIZE
008110       INTO ユ－レコードデータ
008120     END-STRING.
008130     PERFORM ユーザーファイル書込.
008140*/郵便番号
008150     INITIALIZE ユ－レコード.
008160     STRING "郵便番号="    DELIMITED BY SIZE
008170            施情－郵便番号 DELIMITED BY SIZE
008180       INTO ユ－レコードデータ
008190     END-STRING.
008200     PERFORM ユーザーファイル書込.
008210*/住所１
008220     INITIALIZE ユ－レコード.
008230     MOVE 施情－住所１ TO 英数字項目Ｗ.
008240     INSPECT 英数字項目ＸＷ REPLACING ALL "　" BY "  ".
008250     STRING "住所1="       DELIMITED BY SIZE
008260            英数字項目ＸＷ DELIMITED BY SIZE
008270       INTO ユ－レコードデータ
008280     END-STRING.
008290     PERFORM ユーザーファイル書込.
008300*/住所２
008310     INITIALIZE ユ－レコード.
008320     MOVE 施情－住所２ TO 英数字項目Ｗ.
008330     INSPECT 英数字項目ＸＷ REPLACING ALL "　" BY "  ".
008340     STRING "住所2="       DELIMITED BY SIZE
008350            英数字項目ＸＷ DELIMITED BY SIZE
008360       INTO ユ－レコードデータ
008370     END-STRING.
008380     PERFORM ユーザーファイル書込.
008390*/電話番号
008400     INITIALIZE ユ－レコード.
008410     STRING "TEL="         DELIMITED BY SIZE
008420            施情－電話番号 DELIMITED BY SIZE
008430       INTO ユ－レコードデータ
008440     END-STRING.
008450     PERFORM ユーザーファイル書込.
008460*/銀行名
008470     INITIALIZE ユ－レコード.
008480     STRING "銀行名="          DELIMITED BY SIZE
008490            施情－取引先銀行名 DELIMITED BY SIZE
008500       INTO ユ－レコードデータ
008510     END-STRING.
008520     PERFORM ユーザーファイル書込.
008530*/支店名
008540     INITIALIZE ユ－レコード.
008550     STRING "支店名="              DELIMITED BY SIZE
008560            施情－取引先銀行支店名 DELIMITED BY SIZE
008570       INTO ユ－レコードデータ
008580     END-STRING.
008590     PERFORM ユーザーファイル書込.
008600*/口座番号
008610     INITIALIZE ユ－レコード.
008620     STRING "口座番号="    DELIMITED BY SIZE
008630            施情－口座番号 DELIMITED BY SIZE
008640       INTO ユ－レコードデータ
008650     END-STRING.
008660     PERFORM ユーザーファイル書込.
008670*/預金種別、０：普通、１：当座・柔では１：普通、２：当座
008680     INITIALIZE ユ－レコード.
008690     EVALUATE 施情－預金種別
008700     WHEN 1
008710         MOVE ZERO  TO 預金種別Ｗ
008720     WHEN 2
008730         MOVE 1     TO 預金種別Ｗ
008740     WHEN OTHER
008750         MOVE SPACE TO 預金種別Ｗ
008760     END-EVALUATE
008770     STRING "預金種別=" DELIMITED BY SIZE
008780            預金種別Ｗ  DELIMITED BY SIZE
008790       INTO ユ－レコードデータ
008800     END-STRING.
008810     PERFORM ユーザーファイル書込.
008820*/口座名義人カナ
008830     INITIALIZE ユ－レコード.
008840     STRING "口座名義（カナ）"   DELIMITED BY SIZE
008850            施情－口座名義人カナ DELIMITED BY SIZE
008860       INTO ユ－レコードデータ
008870     END-STRING.
008880     PERFORM ユーザーファイル書込.
008890*/口座名義人名
008900     INITIALIZE ユ－レコード.
008910     MOVE 施情－口座名義人 TO 英数字項目Ｗ.
008920     INSPECT 英数字項目ＸＷ REPLACING ALL "　" BY "  ".
008930     STRING "口座名義（漢字）=" DELIMITED BY SIZE
008940            英数字項目ＸＷ      DELIMITED BY SIZE
008950       INTO ユ－レコードデータ
008960     END-STRING.
008970     PERFORM ユーザーファイル書込.
008980*/新柔整師番号
008990     INITIALIZE ユ－レコード.
009000     STRING "柔整師コード（漢字）=" DELIMITED BY SIZE
009010            施情－新柔整師番号 DELIMITED BY SIZE
009020       INTO ユ－レコードデータ
009030     END-STRING.
009040     PERFORM ユーザーファイル書込.
009050*/柔整師コード
009060     INITIALIZE ユ－レコード.
009070     MOVE 施情－新柔整師番号 TO 柔整師コードＷ
009080     STRING "柔整師コード（数字）=" DELIMITED BY SIZE
009090            柔整師コード数字Ｗ      DELIMITED BY SIZE
009100       INTO ユ－レコードデータ
009110     END-STRING.
009120     PERFORM ユーザーファイル書込.
009130*/会員番号
009140     INITIALIZE ユ－レコード.
009150     STRING "会員番号="            DELIMITED BY SIZE
009160*            会員番号コードＷ       DELIMITED BY SIZE
009160            施情－接骨師会会員番号 DELIMITED BY SIZE
009170       INTO ユ－レコードデータ
009180     END-STRING.
009190     PERFORM ユーザーファイル書込.
009200*/施術年月
009210     INITIALIZE ユ－レコード.
009220     MOVE 請求和暦ＷＲ TO 和暦Ｗ.
009230     MOVE 請求年ＷＲ   TO 年Ｗ.
009240     PERFORM 西暦年取得.
009250     STRING "請求年月=" DELIMITED BY SIZE
009260             西暦年Ｗ   DELIMITED BY SIZE
009270            "/"         DELIMITED BY SIZE
009280            請求月ＷＲ  DELIMITED BY SIZE
009290       INTO ユ－レコードデータ
009300     END-STRING.
009310     PERFORM ユーザーファイル書込.
026650     PERFORM 遅延処理.
009320*================================================================*
009330 ユーザーファイル書込 SECTION.
009340*
009350     MOVE SPACE TO 終了フラグ.
009360     PERFORM VARYING 文字カウンタ FROM 50 BY -1
009370             UNTIL   (文字カウンタ  <= ZERO ) OR
009380                     (終了フラグ NOT = SPACE)
009390         IF ユ－レコードデータ(文字カウンタ:1) NOT = SPACE
009400             COMPUTE 文字カウンタ = 文字カウンタ + 1
009410             MOVE "YES" TO 終了フラグ
009420         END-IF
009430     END-PERFORM.
009440*
009450     WRITE ユ－レコード
009460     IF 状態キー  NOT =  "00"
009470         MOVE NC"ユーザー"  TO ファイル名
009480         PERFORM エラー表示
009490     END-IF.
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
009620     END-READ.
009630*
009640*================================================================*
009650 会情報取得 SECTION.
009660*
009680     MOVE ZERO           TO 会情－柔整鍼灸区分.
009670     MOVE 制－協会コード TO 会情－協会コード.
009680     MOVE ZERO           TO 会情－保険種別.
009690     MOVE ZERO           TO 会情－変更和暦年月.
009700*
009720     READ 会情報マスタ
009730     INVALID KEY
009740         MOVE  NC"該当する接骨師会情報がありません" TO 連メ－メッセージ
009750         CALL   "MSG001"
009760         CANCEL "MSG001"
009770         PERFORM ファイル閉鎖
009780         MOVE 99 TO PROGRAM-STATUS
009790         EXIT PROGRAM
009800     END-READ.
009810*================================================================*
009820 連結項目退避 SECTION.
009830*
009840     MOVE 連入－請求和暦 TO 請求和暦ＷＲ.
009850     MOVE 連入－請求年   TO 請求年ＷＲ.
009860     MOVE 連入－請求月   TO 請求月ＷＲ.
009860     MOVE 連入－ドライブ TO ドライブＷＲ.
009870*================================================================*
009880 西暦年取得 SECTION.
009890*
009900     MOVE ZERO   TO 西暦年Ｗ.
009910     MOVE 和暦Ｗ TO 元－元号区分.
009920     READ 元号マスタ
009930     NOT INVALID KEY
009940         COMPUTE 西暦年Ｗ = 元－開始西暦年 + 年Ｗ - 1
009950     END-READ.
010650*================================================================*
010660 患者情報ファイル書込 SECTION.
010670*
010680     WRITE 患－レコード
010690     IF 状態キー  NOT =  "00"
010700         MOVE NC"患者情報"  TO ファイル名
010710         PERFORM エラー表示
010720     END-IF.
026650     PERFORM 遅延処理.
010730*================================================================*
010740 受診者情報Ｆ読込 SECTION.
010750*
010760     READ 受診者情報Ｆ NEXT
010770     AT END
010780         MOVE "YES" TO 終了フラグ
010790     END-READ.
010800*================================================================*
010810 健保レコードセット SECTION.
010820*
010830*/料金
010840     MOVE レセ－合計          TO 患－費用額.
010850     MOVE レセ－一部負担金    TO 患－一部負担金.
010860     MOVE レセ－請求金額      TO 患－請求金額.
011000*================================================================*
011130 助成レコードセット SECTION.
011140*
011150*/料金
011160*/費用額は健保のもの
011170     MOVE レセ－合計               TO 患－費用額.
011180     MOVE レセ－受給者負担額       TO 患－一部負担金.
011190     MOVE レセ－助成請求金額       TO 患－請求金額.
011200*** 生保単独 (請求額は 請求額+請求額助成)
011210     IF ( 受－保険種別    = ZERO ) AND
011220        ( 受－保険者番号  = SPACE )
010860         MOVE レセ－請求金額       TO 患－請求金額
011240     END-IF.
011400*================================================================*
011410 共通レコードセット SECTION.
011420*
011430     MOVE SPACE TO 患－レコード.
011440     INITIALIZE    患－レコード.
011450*/会員番号
011460     PERFORM 施術所情報取得.
           MOVE 施情－接骨師会会員番号(1:4) TO 患－会員番号.
012410*/定額制受理区分．０固定
012420     MOVE ZERO                       TO 患－定額制受理区分.
012150*/請求年月
012200     MOVE 請求和暦ＷＲ               TO 和暦Ｗ.
012210     MOVE 請求年ＷＲ                 TO 年Ｗ.
012220     PERFORM 西暦年取得.
012240     COMPUTE 患－請求年月 = (西暦年Ｗ * 100) + 請求月ＷＲ.
      */レセプト番号
           MOVE レセプト番号Ｗ             TO 患－レセプト番号.
           COMPUTE レセプト番号Ｗ = レセプト番号Ｗ + 1.
012250*/施術年月．2003→103(マイナス1900)
012260     MOVE 受－施術和暦               TO 和暦Ｗ.
012270     MOVE 受－施術年                 TO 年Ｗ.
012280     PERFORM 西暦年取得.
012300     COMPUTE 患－施術年月 = (西暦年Ｗ * 100) + 受－施術月.
      */保険種別 １：一般・後高 ２：老人 ３：助成
           IF レセ－レセ種別 = 2
              MOVE 1                       TO 患－レセ種別
           ELSE
              MOVE レセ－レセ種別          TO 患－レセ種別
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
011880     IF 記号Ｗ(1:2) NOT = "＊"
011890         MOVE 記号Ｗ                 TO 患－保険証記号
011900     END-IF.
011910     IF (番号Ｗ(1:1) NOT = "*") AND (番号Ｗ(1:2) NOT = "＊")
011920         MOVE 番号Ｗ                 TO 患－保険証番号
011930     END-IF.
      */保険者番号
           MOVE 受－保険者番号             TO 患－保険者番号.
           IF 受－保険種別 = 70 
              MOVE 受－施術和暦年月        TO 労災－施術和暦年月
              MOVE 受－患者コード          TO 労災－患者コード
              READ 労災情報Ｆ
              NOT INVALID KEY
                 MOVE 労災－労働保険番号   TO 患－保険者番号
              END-READ
           END-IF.
           IF 受－保険種別 = 85 
              MOVE 受－施術和暦年月        TO 生保－施術和暦年月
              MOVE 受－患者コード          TO 生保－患者コード
              READ 生保情報Ｆ
              NOT INVALID KEY
                 MOVE 生保－負担者番号     TO 患－保険者番号
                 MOVE 生保－生保記号番号   TO 患－保険証記号
              END-READ
           END-IF.
011690*/老人
011700     IF (受－公費種別 NOT = ZERO) AND (受－施術和暦年月 < 42004)
011710         MOVE 受－費用負担者番号     TO 患－老健市町村番号
011730         MOVE 受－受益者番号老人     TO 患－老健受給者番号
           ELSE
               MOVE SPACE                  TO 患－老健市町村番号
                                              患－老健受給者番号
011740     END-IF.
011750*/助成
011760     IF (受－助成種別           NOT = ZERO ) AND
011770        (受－費用負担者番号助成 NOT = SPACE)
011790         MOVE 受－費用負担者番号助成 TO 患－助成負担者番号
011820         IF (受－受益者番号助成(1:1) NOT = "*" ) AND 
011830            (受－受益者番号助成(1:2) NOT = "＊")
011840             MOVE 受－受益者番号助成 TO 患－助成受給者番号
011850         END-IF
           ELSE
               MOVE SPACE                  TO 患－助成負担者番号
                                              患－助成受給者番号
011860     END-IF.
011580*/名前
011620     MOVE 受－被保険者氏名           TO 患－被保険者氏名漢字.
011610     MOVE 受－被保険者カナ           TO 患－被保険者氏名カナ.
011600     MOVE 受－患者氏名               TO 患－患者氏名漢字.
011590     MOVE 受－患者カナ               TO 患－患者氏名カナ.
010960     MOVE 受－本人家族区分           TO 患－本人家族区分.
011980*/性別１：男、２：女
012000     MOVE 受－患者性別               TO 患－性別.
011940*/患者生年月日
011950     MOVE 受－患者生年月日           TO 和暦年月日Ｗ.
011960     PERFORM 日付変換８桁.
011970     MOVE 日付８桁Ｗ                 TO 患－生年月日.
011940*/被保険者住所
011620     MOVE 受－住所１                 TO 患－被保険者住所.
           IF 受－保険種別 > 60
011620        MOVE 受－患者氏名            TO 患－被保険者氏名漢字
011610        MOVE 受－患者カナ            TO 患－被保険者氏名カナ
011620        MOVE 受－患者住所１          TO 患－被保険者住所
           END-IF.
011940*/給付割合
011620     MOVE レセ－給付割合             TO 患－給付割合.
012430*/部位数
012440     MOVE 負－部位数                 TO 患－部位数.
012430*/負傷原因
032870     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
032880             UNTIL ( 部位ＣＮＴ > 負－部位数 ) OR
                         ( 負－負傷連番(部位ＣＮＴ)  NOT = ZERO )
               CONTINUE
           END-PERFORM.
033200     MOVE 01                           TO 負原－区分コード.
033210     MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負原－患者番号.
033220     MOVE 負－負傷連番(部位ＣＮＴ)     TO 負原－負傷原因連番.
033230     READ 負傷原因Ｆ
033240     NOT INVALID KEY
012440        MOVE 負原－負傷原因ＣＭ(1)     TO 患－負傷原因
           END-READ.
012430*/長期理由
004502     MOVE 受－施術和暦                 TO 長継－施術和暦
004503     MOVE 受－施術年                   TO 長継－施術年.
004504     MOVE 受－施術月                   TO 長継－施術月.
004505     MOVE 受－患者番号                 TO 長継－患者番号.
004506     MOVE 受－枝番                     TO 長継－枝番.
004507*
004508     READ 長期継続者Ｆ 
004522     NOT INVALID KEY
004523         MOVE 長継－理由文(1)          TO 患－長期理由
004538     END-READ.
028280* 新規/継続
033380     EVALUATE レセ－レセ請求区分
           WHEN 1
033390         MOVE 1                        TO 患－新規
033410         MOVE ZERO                     TO 患－継続
           WHEN 2
033390         MOVE ZERO                     TO 患－新規
033410         MOVE 1                        TO 患－継続
033400     WHEN 3
033390         MOVE 1                        TO 患－新規
033410         MOVE 1                        TO 患－継続
033420     END-EVALUATE.
      */初検、往療、その他
           PERFORM 施術情報設定.
           MOVE 初検回数Ｗ                   TO 患－初検回数.
           MOVE 時間外回数Ｗ                 TO 患－初検時間外回数.
           MOVE 休日回数Ｗ                   TO 患－初検休日回数.
           MOVE 深夜回数Ｗ                   TO 患－初検深夜回数.
           MOVE 再検回数Ｗ                   TO 患－再検回数.
           COMPUTE 患－往療距離 = レセ－往療距離 * 10.
           MOVE 往療回数Ｗ                   TO 患－往療回数.
           MOVE 夜間回数Ｗ                   TO 患－往療夜間回数.
           MOVE 難路回数Ｗ                   TO 患－往療難路回数.
           MOVE 暴風雨雪回数Ｗ               TO 患－往療暴風雨雪回数.
           MOVE 大回数Ｗ                     TO 患－金属副子大回数.
           MOVE 中回数Ｗ                     TO 患－金属副子中回数.
           MOVE 小回数Ｗ                     TO 患－金属副子小回数.
           MOVE 情報提供回数Ｗ               TO 患－情報提供回数.
012490*/負傷種別１：骨折、２：不全骨折、３：脱臼、４：打撲、５：捻挫、６：挫傷　７：拘縮後療あり
012500*/柔では１：捻挫、２：打撲、３：挫傷、    ４：脱臼、
012510*/      ５：骨折、６：不骨、７：骨折拘縮、８：不全拘縮、
012520     MOVE 負－負傷種別(1)              TO 負傷種別Ｗ.
012530     PERFORM 部位タイプ取得.
012540     MOVE 部位タイプＷ                 TO 患－負傷区分１.
      */負傷名
027440     MOVE 03                           TO 名－区分コード.
027450     MOVE 負－負傷種別(1)              TO 名－名称コード.
027460     READ 名称マスタ
027470     INVALID KEY
027480         MOVE SPACE                    TO 負傷名称Ｗ
027490     NOT INVALID KEY
027500         MOVE 名－正式名称             TO 負傷名称Ｗ
027510     END-READ.
006490     STRING レセ－部位名称１(1)  DELIMITED BY SPACE
009980            負傷名称Ｗ           DELIMITED BY SPACE
006500            レセ－部位名称２(1)  DELIMITED BY SPACE
006520       INTO 患－負傷名１
006570     END-STRING.
      */初回処置回数
           IF レセ－初回処置料(1) NOT = ZERO
              MOVE 1                         TO 患－初回処置回数１
           END-IF.
012310*/負傷年月日
012320     MOVE 負－負傷和暦年月日(1)        TO 和暦年月日Ｗ.
012330     PERFORM 日付変換８桁.
012340     MOVE 日付８桁Ｗ                   TO 患－負傷年月日１.
012310*/施術開始日：最初の通院日
012720     MOVE 負－開始和暦年月日(1)        TO 和暦年月日Ｗ.
012330     PERFORM 日付変換８桁.
012340     MOVE 日付８桁Ｗ                   TO 患－初検年月日１
                                                患－施術開始日１.
012350*/施術終了日：最終通院日？
012720     MOVE 負－終了和暦年月日(1)        TO 和暦年月日Ｗ.
012370     PERFORM 日付変換８桁.
012380     MOVE 日付８桁Ｗ                   TO 患－施術終了日１.
012390*/実日数
012400     MOVE レセ－部位実日数(1)          TO 患－実日数１.
012390*/転帰区分
           EVALUATE 負－転帰区分(1)
           WHEN 3
012400         MOVE 2                        TO 患－転帰区分１
           WHEN 4
012400         MOVE 3                        TO 患－転帰区分１
           WHEN 9
012400         MOVE ZERO                     TO 患－転帰区分１
           WHEN OTHER
012400         MOVE 1                        TO 患－転帰区分１
           END-EVALUATE.
012390*/後療回数
012400     MOVE レセ－後療回数１             TO 患－後療回数１.
012390*/冷罨法回数
012400     MOVE レセ－冷罨法回数１           TO 患－冷罨法回数１.
012390*/温罨法回数
012400     MOVE レセ－温罨法回数１           TO 患－温罨法回数１.
012390*/電療回数
012400     MOVE レセ－電療回数１             TO 患－電療回数１.
012390*/多部位逓減区分
012400     MOVE ZERO                         TO 患－多部位逓減区分１.
012390*/長期逓減区分
           IF レセ－長期逓減率１ NOT = ZERO
012400         MOVE 1                        TO 患－長期逓減区分１
           END-IF.
012490*/負傷種別１：骨折、２：不全骨折、３：脱臼、４：打撲、５：捻挫、６：挫傷　７：拘縮後療あり
012500*/柔では１：捻挫、２：打撲、３：挫傷、    ４：脱臼、
012510*/      ５：骨折、６：不骨、７：骨折拘縮、８：不全拘縮、
012520     MOVE 負－負傷種別(2)              TO 負傷種別Ｗ.
012530     PERFORM 部位タイプ取得.
012540     MOVE 部位タイプＷ                 TO 患－負傷区分２.
      */負傷名
027440     MOVE 03                           TO 名－区分コード.
027450     MOVE 負－負傷種別(2)              TO 名－名称コード.
027460     READ 名称マスタ
027470     INVALID KEY
027480         MOVE SPACE                    TO 負傷名称Ｗ
027490     NOT INVALID KEY
027500         MOVE 名－正式名称             TO 負傷名称Ｗ
027510     END-READ.
006490     STRING レセ－部位名称１(2)  DELIMITED BY SPACE
009980            負傷名称Ｗ           DELIMITED BY SPACE
006500            レセ－部位名称２(2)  DELIMITED BY SPACE
006520       INTO 患－負傷名２
006570     END-STRING.
      */初回処置回数
           IF レセ－初回処置料(2) NOT = ZERO
              MOVE 1                         TO 患－初回処置回数２
           END-IF.
012310*/負傷年月日
012320     MOVE 負－負傷和暦年月日(2)        TO 和暦年月日Ｗ.
012330     PERFORM 日付変換８桁.
012340     MOVE 日付８桁Ｗ                   TO 患－負傷年月日２.
012700*/初検日
012310*/施術開始日：最初の通院日
012720     MOVE 負－開始和暦年月日(2)        TO 和暦年月日Ｗ.
012330     PERFORM 日付変換８桁.
012340     MOVE 日付８桁Ｗ                   TO 患－初検年月日２
                                                患－施術開始日２.
012350*/施術終了日：最終通院日？
012720     MOVE 負－終了和暦年月日(2)        TO 和暦年月日Ｗ.
012370     PERFORM 日付変換８桁.
012380     MOVE 日付８桁Ｗ                   TO 患－施術終了日２.
012390*/実日数
012400     MOVE レセ－部位実日数(2)          TO 患－実日数２.
012390*/転帰区分
           EVALUATE 負－転帰区分(2)
           WHEN 3
012400         MOVE 2                        TO 患－転帰区分２
           WHEN 4
012400         MOVE 3                        TO 患－転帰区分２
           WHEN 9
012400         MOVE ZERO                     TO 患－転帰区分２
           WHEN OTHER
012400         MOVE 1                        TO 患－転帰区分２
           END-EVALUATE.
012390*/後療回数
012400     MOVE レセ－後療回数２             TO 患－後療回数２.
012390*/冷罨法回数
012400     MOVE レセ－冷罨法回数２           TO 患－冷罨法回数２.
012390*/温罨法回数
012400     MOVE レセ－温罨法回数２           TO 患－温罨法回数２.
012390*/電療回数
012400     MOVE レセ－電療回数２             TO 患－電療回数２.
012390*/多部位逓減区分
012400     MOVE ZERO                         TO 患－多部位逓減区分２.
012390*/長期逓減区分
           IF レセ－長期逓減率２ NOT = ZERO
012400         MOVE 1                        TO 患－長期逓減区分２
           END-IF.
012490*/負傷種別１：骨折、２：不全骨折、３：脱臼、４：打撲、５：捻挫、６：挫傷　７：拘縮後療あり
012500*/柔では１：捻挫、２：打撲、３：挫傷、    ４：脱臼、
012510*/      ５：骨折、６：不骨、７：骨折拘縮、８：不全拘縮、
012520     MOVE 負－負傷種別(3)              TO 負傷種別Ｗ.
012530     PERFORM 部位タイプ取得.
012540     MOVE 部位タイプＷ                 TO 患－負傷区分３.
      */負傷名
027440     MOVE 03                           TO 名－区分コード.
027450     MOVE 負－負傷種別(3)              TO 名－名称コード.
027460     READ 名称マスタ
027470     INVALID KEY
027480         MOVE SPACE                    TO 負傷名称Ｗ
027490     NOT INVALID KEY
027500         MOVE 名－正式名称             TO 負傷名称Ｗ
027510     END-READ.
006490     STRING レセ－部位名称１(3)  DELIMITED BY SPACE
009980            負傷名称Ｗ           DELIMITED BY SPACE
006500            レセ－部位名称２(3)  DELIMITED BY SPACE
006520       INTO 患－負傷名３
006570     END-STRING.
      */初回処置回数
           IF レセ－初回処置料(3) NOT = ZERO
              MOVE 1                         TO 患－初回処置回数３
           END-IF.
012310*/負傷年月日
012320     MOVE 負－負傷和暦年月日(3)        TO 和暦年月日Ｗ.
012330     PERFORM 日付変換８桁.
012340     MOVE 日付８桁Ｗ                   TO 患－負傷年月日３.
012700*/初検日
012310*/施術開始日：最初の通院日
012720     MOVE 負－開始和暦年月日(3)        TO 和暦年月日Ｗ.
012330     PERFORM 日付変換８桁.
012340     MOVE 日付８桁Ｗ                   TO 患－初検年月日３
                                                患－施術開始日３.
012350*/施術終了日：最終通院日？
012720     MOVE 負－終了和暦年月日(3)        TO 和暦年月日Ｗ.
012370     PERFORM 日付変換８桁.
012380     MOVE 日付８桁Ｗ                   TO 患－施術終了日３.
012390*/実日数
012400     MOVE レセ－部位実日数(3)          TO 患－実日数３.
012390*/転帰区分
           EVALUATE 負－転帰区分(3)
           WHEN 3
012400         MOVE 2                        TO 患－転帰区分３
           WHEN 4
012400         MOVE 3                        TO 患－転帰区分３
           WHEN 9
012400         MOVE ZERO                     TO 患－転帰区分３
           WHEN OTHER
012400         MOVE 1                        TO 患－転帰区分３
           END-EVALUATE.
012390*/後療回数
012400     COMPUTE 患－後療回数３ = レセ－後療回数３０ + レセ－後療回数３８.
012390*/冷罨法回数
012400     COMPUTE 患－冷罨法回数３ = レセ－冷罨法回数３０ + レセ－冷罨法回数３８.
012390*/温罨法回数
012400     COMPUTE 患－温罨法回数３ = レセ－温罨法回数３０ + レセ－温罨法回数３８.
012390*/電療回数
012400     COMPUTE 患－電療回数３ = レセ－電療回数３０ + レセ－電療回数３８.
012390*/多部位逓減区分
           IF レセ－小計３８ NOT = ZERO
012400        MOVE 1                         TO 患－多部位逓減区分３
           END-IF.
012390*/長期逓減区分
           IF (レセ－長期逓減率３８ NOT = ZERO) OR (レセ－長期逓減率３０ NOT = ZERO)
012400         MOVE 1                        TO 患－長期逓減区分３
           END-IF.
012490*/負傷種別１：骨折、２：不全骨折、３：脱臼、４：打撲、５：捻挫、６：挫傷　７：拘縮後療あり
012500*/柔では１：捻挫、２：打撲、３：挫傷、    ４：脱臼、
012510*/      ５：骨折、６：不骨、７：骨折拘縮、８：不全拘縮、
012520     MOVE 負－負傷種別(4)              TO 負傷種別Ｗ.
012530     PERFORM 部位タイプ取得.
012540     MOVE 部位タイプＷ                 TO 患－負傷区分４.
      */負傷名
027440     MOVE 03                           TO 名－区分コード.
027450     MOVE 負－負傷種別(4)              TO 名－名称コード.
027460     READ 名称マスタ
027470     INVALID KEY
027480         MOVE SPACE                    TO 負傷名称Ｗ
027490     NOT INVALID KEY
027500         MOVE 名－正式名称             TO 負傷名称Ｗ
027510     END-READ.
006490     STRING レセ－部位名称１(4)  DELIMITED BY SPACE
009980            負傷名称Ｗ           DELIMITED BY SPACE
006500            レセ－部位名称２(4)  DELIMITED BY SPACE
006520       INTO 患－負傷名４
006570     END-STRING.
      */初回処置回数
           IF レセ－初回処置料(4) NOT = ZERO
              MOVE 1                         TO 患－初回処置回数４
           END-IF.
012310*/負傷年月日
012320     MOVE 負－負傷和暦年月日(4)        TO 和暦年月日Ｗ.
012330     PERFORM 日付変換８桁.
012340     MOVE 日付８桁Ｗ                   TO 患－負傷年月日４.
012700*/初検日
012310*/施術開始日：最初の通院日
012720     MOVE 負－開始和暦年月日(4)        TO 和暦年月日Ｗ.
012330     PERFORM 日付変換８桁.
012340     MOVE 日付８桁Ｗ                   TO 患－初検年月日４
                                                患－施術開始日４.
012350*/施術終了日：最終通院日？
012720     MOVE 負－終了和暦年月日(4)        TO 和暦年月日Ｗ.
012370     PERFORM 日付変換８桁.
012380     MOVE 日付８桁Ｗ                   TO 患－施術終了日４.
012390*/実日数
012400     MOVE レセ－部位実日数(4)          TO 患－実日数４.
012390*/転帰区分
           EVALUATE 負－転帰区分(4)
           WHEN 3
012400         MOVE 2                        TO 患－転帰区分４
           WHEN 4
012400         MOVE 3                        TO 患－転帰区分４
           WHEN 9
012400         MOVE ZERO                     TO 患－転帰区分４
           WHEN OTHER
012400         MOVE 1                        TO 患－転帰区分４
           END-EVALUATE.
012390*/後療回数
012400     COMPUTE 患－後療回数４ = レセ－後療回数４０ + レセ－後療回数４８ +
                                    レセ－後療回数４５.
012390*/冷罨法回数
012400     COMPUTE 患－冷罨法回数４ = レセ－冷罨法回数４０ + レセ－冷罨法回数４８ +
                                      レセ－冷罨法回数４５.
012390*/温罨法回数
012400     COMPUTE 患－温罨法回数４ = レセ－温罨法回数４０ + レセ－温罨法回数４８ +
                                      レセ－温罨法回数４５.
012390*/電療回数
012400     COMPUTE 患－電療回数４ = レセ－電療回数４０ + レセ－電療回数４８ +
                                    レセ－電療回数４５.
012390*/多部位逓減区分
           IF (レセ－小計４８ NOT = ZERO) OR (レセ－小計４５ NOT = ZERO)
012400        MOVE 1                         TO 患－多部位逓減区分４
           END-IF.
012390*/長期逓減区分
           IF (レセ－長期逓減率４８ NOT = ZERO) OR
              (レセ－長期逓減率４５ NOT = ZERO) OR
              (レセ－長期逓減率４０ NOT = ZERO)
012400         MOVE 1                        TO 患－長期逓減区分４
           END-IF.
012490*/負傷種別１：骨折、２：不全骨折、３：脱臼、４：打撲、５：捻挫、６：挫傷　７：拘縮後療あり
012500*/柔では１：捻挫、２：打撲、３：挫傷、    ４：脱臼、
012510*/      ５：骨折、６：不骨、７：骨折拘縮、８：不全拘縮、
012520     MOVE 負－負傷種別(5)              TO 負傷種別Ｗ.
012530     PERFORM 部位タイプ取得.
012540     MOVE 部位タイプＷ                 TO 患－負傷区分５.
      */負傷名
027440     MOVE 03                           TO 名－区分コード.
027450     MOVE 負－負傷種別(5)              TO 名－名称コード.
027460     READ 名称マスタ
027470     INVALID KEY
027480         MOVE SPACE                    TO 負傷名称Ｗ
027490     NOT INVALID KEY
027500         MOVE 名－正式名称             TO 負傷名称Ｗ
027510     END-READ.
006490     STRING レセ－部位名称１(5)  DELIMITED BY SPACE
009980            負傷名称Ｗ           DELIMITED BY SPACE
006500            レセ－部位名称２(5)  DELIMITED BY SPACE
006520       INTO 患－負傷名５
006570     END-STRING.
      */初回処置回数
           IF レセ－初回処置料(5) NOT = ZERO
              MOVE 1                         TO 患－初回処置回数５
           END-IF.
012310*/負傷年月日
012320     MOVE 負－負傷和暦年月日(5)        TO 和暦年月日Ｗ.
012330     PERFORM 日付変換８桁.
012340     MOVE 日付８桁Ｗ                   TO 患－負傷年月日５.
012700*/初検日
012310*/施術開始日：最初の通院日
012720     MOVE 負－開始和暦年月日(5)        TO 和暦年月日Ｗ.
012330     PERFORM 日付変換８桁.
012340     MOVE 日付８桁Ｗ                   TO 患－初検年月日５
                                                患－施術開始日５.
012350*/施術終了日：最終通院日？
012720     MOVE 負－終了和暦年月日(5)        TO 和暦年月日Ｗ.
012370     PERFORM 日付変換８桁.
012380     MOVE 日付８桁Ｗ                   TO 患－施術終了日５.
012390*/実日数
012400     MOVE レセ－部位実日数(5)          TO 患－実日数５.
012390*/転帰区分
           EVALUATE 負－転帰区分(5)
           WHEN 3
012400         MOVE 2                        TO 患－転帰区分５
           WHEN 4
012400         MOVE 3                        TO 患－転帰区分５
           WHEN 9
012400         MOVE ZERO                     TO 患－転帰区分５
           WHEN OTHER
012400         MOVE 1                        TO 患－転帰区分５
           END-EVALUATE.
012390*/後療回数
012400     COMPUTE 患－後療回数５ = レセ－後療回数５０ + レセ－後療回数５８ +
                                    レセ－後療回数５５ + レセ－後療回数５２.
012390*/冷罨法回数
012400     COMPUTE 患－冷罨法回数５ = レセ－冷罨法回数５０ + レセ－冷罨法回数５８ +
                                      レセ－冷罨法回数５５ + レセ－後療回数５２.
012390*/温罨法回数
012400     COMPUTE 患－温罨法回数５ = レセ－温罨法回数５０ + レセ－温罨法回数５８ +
                                      レセ－温罨法回数５５ + レセ－後療回数５２.
012390*/電療回数
012400     COMPUTE 患－電療回数５ = レセ－電療回数５０ + レセ－電療回数５８ +
                                    レセ－電療回数５５ + レセ－後療回数５２.
012390*/多部位逓減区分
           IF (レセ－小計５２ NOT = ZERO) OR (レセ－小計５５ NOT = ZERO) OR (レセ－小計５８ NOT = ZERO)
012400        MOVE 1                         TO 患－多部位逓減区分５
           END-IF.
012390*/長期逓減区分
           IF (レセ－長期逓減率５２ NOT = ZERO) OR (レセ－長期逓減率５５ NOT = ZERO) OR
              (レセ－長期逓減率５８ NOT = ZERO) OR (レセ－長期逓減率５０ NOT = ZERO)
012400         MOVE 1                        TO 患－長期逓減区分５
           END-IF.
012490*/負傷種別１：骨折、２：不全骨折、３：脱臼、４：打撲、５：捻挫、６：挫傷　７：拘縮後療あり
012500*/柔では１：捻挫、２：打撲、３：挫傷、    ４：脱臼、
012510*/      ５：骨折、６：不骨、７：骨折拘縮、８：不全拘縮、
012520     MOVE 負－負傷種別(6)              TO 負傷種別Ｗ.
012530     PERFORM 部位タイプ取得.
012540     MOVE 部位タイプＷ                 TO 患－負傷区分６.
      */負傷名
027440     MOVE 03                           TO 名－区分コード.
027450     MOVE 負－負傷種別(6)              TO 名－名称コード.
027460     READ 名称マスタ
027470     INVALID KEY
027480         MOVE SPACE                    TO 負傷名称Ｗ
027490     NOT INVALID KEY
027500         MOVE 名－正式名称             TO 負傷名称Ｗ
027510     END-READ.
006490     STRING レセ－部位名称１(6)  DELIMITED BY SPACE
009980            負傷名称Ｗ           DELIMITED BY SPACE
006500            レセ－部位名称２(6)  DELIMITED BY SPACE
006520       INTO 患－負傷名６
006570     END-STRING.
      */初回処置回数
           IF レセ－初回処置料(6) NOT = ZERO
              MOVE 1                         TO 患－初回処置回数６
           END-IF.
012310*/負傷年月日
012320     MOVE 負－負傷和暦年月日(6)        TO 和暦年月日Ｗ.
012330     PERFORM 日付変換８桁.
012340     MOVE 日付８桁Ｗ                   TO 患－負傷年月日６.
      */初検日
012310*/施術開始日：最初の通院日
012720     MOVE 負－開始和暦年月日(6)        TO 和暦年月日Ｗ.
012330     PERFORM 日付変換８桁.
012340     MOVE 日付８桁Ｗ                   TO 患－初検年月日６
                                                患－施術開始日６.
012350*/施術終了日：最終通院日？
012720     MOVE 負－終了和暦年月日(6)        TO 和暦年月日Ｗ.
012370     PERFORM 日付変換８桁.
012380     MOVE 日付８桁Ｗ                   TO 患－施術終了日６.
012390*/実日数
012400     MOVE レセ－部位実日数(6)          TO 患－実日数６.
012390*/転帰区分
           EVALUATE 負－転帰区分(6)
           WHEN 3
012400         MOVE 2                        TO 患－転帰区分６
           WHEN 4
012400         MOVE 3                        TO 患－転帰区分６
           WHEN 9
012400         MOVE ZERO                     TO 患－転帰区分６
           WHEN OTHER
012400         MOVE 1                        TO 患－転帰区分６
           END-EVALUATE.
012390*/後療回数
012400*     MOVE レセ－後療回数６             TO 患－後療回数６.
012390*/冷罨法回数
012400*     MOVE レセ－冷罨法回数６           TO 患－冷罨法回数６.
012390*/温罨法回数
012400*     MOVE レセ－温罨法回数６           TO 患－温罨法回数６.
012390*/電療回数
012400*     MOVE レセ－電療回数６             TO 患－電療回数６.
012390*/多部位逓減区分
012400*     MOVE ZERO                         TO 患－多部位逓減区分６.
012390*/長期逓減区分
      *     IF 負－長期逓減率６ NOT = ZERO
012400*         MOVE 1                        TO 患－長期逓減区分６
      *     END-IF.
012490*/負傷種別１：骨折、２：不全骨折、３：脱臼、４：打撲、５：捻挫、６：挫傷　７：拘縮後療あり
012500*/柔では１：捻挫、２：打撲、３：挫傷、    ４：脱臼、
012510*/      ５：骨折、６：不骨、７：骨折拘縮、８：不全拘縮、
012520     MOVE 負－負傷種別(7)              TO 負傷種別Ｗ.
012530     PERFORM 部位タイプ取得.
012540     MOVE 部位タイプＷ                 TO 患－負傷区分７.
      */負傷名
027440     MOVE 03                           TO 名－区分コード.
027450     MOVE 負－負傷種別(7)              TO 名－名称コード.
027460     READ 名称マスタ
027470     INVALID KEY
027480         MOVE SPACE                    TO 負傷名称Ｗ
027490     NOT INVALID KEY
027500         MOVE 名－正式名称             TO 負傷名称Ｗ
027510     END-READ.
006490     STRING レセ－部位名称１(7)  DELIMITED BY SPACE
009980            負傷名称Ｗ           DELIMITED BY SPACE
006500            レセ－部位名称２(7)  DELIMITED BY SPACE
006520       INTO 患－負傷名７
006570     END-STRING.
      */初回処置回数
           IF レセ－初回処置料(7) NOT = ZERO
              MOVE 1                         TO 患－初回処置回数７
           END-IF.
012310*/負傷年月日
012320     MOVE 負－負傷和暦年月日(7)        TO 和暦年月日Ｗ.
012330     PERFORM 日付変換８桁.
012340     MOVE 日付８桁Ｗ                   TO 患－負傷年月日７.
012700*/初検日
012310*/施術開始日：最初の通院日
012720     MOVE 負－開始和暦年月日(7)        TO 和暦年月日Ｗ.
012330     PERFORM 日付変換８桁.
012340     MOVE 日付８桁Ｗ                   TO 患－初検年月日７
                                                患－施術開始日７.
012350*/施術終了日：最終通院日？
012720     MOVE 負－終了和暦年月日(7)        TO 和暦年月日Ｗ.
012370     PERFORM 日付変換８桁.
012380     MOVE 日付８桁Ｗ                   TO 患－施術終了日７.
012390*/実日数
012400     MOVE レセ－部位実日数(7)          TO 患－実日数７.
012390*/転帰区分
           EVALUATE 負－転帰区分(7)
           WHEN 3
012400         MOVE 2                        TO 患－転帰区分７
           WHEN 4
012400         MOVE 3                        TO 患－転帰区分７
           WHEN 9
012400         MOVE ZERO                     TO 患－転帰区分７
           WHEN OTHER
012400         MOVE 1                        TO 患－転帰区分７
           END-EVALUATE.
012390*/後療回数
012400*     MOVE レセ－後療回数７             TO 患－後療回数７.
012390*/冷罨法回数
012400*     MOVE レセ－冷罨法回数７           TO 患－冷罨法回数７.
012390*/温罨法回数
012400*     MOVE レセ－温罨法回数７           TO 患－温罨法回数７.
012390*/電療回数
012400*     MOVE レセ－電療回数７             TO 患－電療回数７.
012390*/多部位逓減区分
012400*     MOVE ZERO                         TO 患－多部位逓減区分７.
012390*/長期逓減区分
      *     IF 負－長期逓減率７ NOT = ZERO
012400*         MOVE 1                        TO 患－長期逓減区分７
      *     END-IF.
      */前期高齢負担割合
           IF 受－公費種別 NOT = 5
              IF (受－特別区分 NOT = ZERO) AND (受－特別区分 < 7)
                 MOVE レセ－負担割合          TO 患－前期負担割合
              ELSE
                 MOVE ZERO                    TO 患－前期負担割合
              END-IF
           ELSE
              MOVE ZERO                       TO 患－前期負担割合
           END-IF.
011480*/提出
011490     EVALUATE レセ－請求区分
           WHEN 1
               MOVE 2                         TO 患－提出
           WHEN 2
               MOVE 1                         TO 患－提出
           WHEN OTHER
               MOVE ZERO                      TO 患－提出
           END-EVALUATE.
011480*/患者コード
011490     MOVE 受－患者番号                  TO 患－患者番号.
011500*     MOVE 受－枝番                   TO 患－枝番.
011510     PERFORM 枝番変換.
      */会番号
           MOVE 施情－接骨師会会員番号(1:6)   TO 患－会番号.
      */保険種別
011510     PERFORM 保険種別変換.
014400*================================================================*
014410 枝番変換 SECTION.
014420*
014430     EVALUATE 受－枝番
014440     WHEN SPACE
014450         MOVE 1  TO 患－枝番
014460     WHEN "A"
014470         MOVE 2  TO 患－枝番
014480     WHEN "B"
014490         MOVE 3  TO 患－枝番
014500     WHEN "C"
014510         MOVE 4  TO 患－枝番
014520     WHEN "D"
014530         MOVE 5  TO 患－枝番
014540     WHEN "E"
014550         MOVE 6  TO 患－枝番
014560     WHEN "F"
014570         MOVE 7  TO 患－枝番
014580     WHEN "G"
014590         MOVE 8  TO 患－枝番
014600     WHEN "H"
014610         MOVE 9  TO 患－枝番
014620*     WHEN "I"
014630*         MOVE 10 TO 患－枝番
014640*     WHEN "J"
014650*         MOVE 11 TO 患－枝番
014660*     WHEN "K"
014670*         MOVE 12 TO 患－枝番
014680*     WHEN "L"
014690*         MOVE 13 TO 患－枝番
014700*     WHEN "M"
014710*         MOVE 14 TO 患－枝番
014720*     WHEN "N"
014730*         MOVE 15 TO 患－枝番
014740*     WHEN "O"
014750*         MOVE 16 TO 患－枝番
014760*     WHEN "P"
014770*         MOVE 17 TO 患－枝番
014780*     WHEN "Q"
014790*         MOVE 18 TO 患－枝番
014800*     WHEN "R"
014810*         MOVE 19 TO 患－枝番
014820*     WHEN "S"
014830*         MOVE 20 TO 患－枝番
014840*     WHEN "T"
014850*         MOVE 21 TO 患－枝番
014860*     WHEN "U"
014870*         MOVE 22 TO 患－枝番
014880*     WHEN "V"
014890*         MOVE 23 TO 患－枝番
014900*     WHEN "W"
014910*         MOVE 24 TO 患－枝番
014920*     WHEN "X"
014930*         MOVE 25 TO 患－枝番
014940*     WHEN "Y"
014950*         MOVE 26 TO 患－枝番
014960*     WHEN "Z"
014970*         MOVE 27 TO 患－枝番
014980     WHEN OTHER
014990         MOVE 1  TO 患－枝番
015000     END-EVALUATE.
015010*================================================================*
015020 部位タイプ取得 SECTION.
015030*
015040     EVALUATE 負傷種別Ｗ
015050     WHEN 01
015060         MOVE 5    TO 部位タイプＷ
015070     WHEN 02
015080         MOVE 4    TO 部位タイプＷ
015090     WHEN 03
015100         MOVE 6    TO 部位タイプＷ
015110     WHEN 04
015120         MOVE 3    TO 部位タイプＷ
015130     WHEN 05
015150         MOVE 1    TO 部位タイプＷ
015160     WHEN 06
015180         MOVE 2    TO 部位タイプＷ
015140     WHEN 07
015140     WHEN 08
015200         MOVE 7    TO 部位タイプＷ
015160     WHEN 09
015180         MOVE ZERO TO 部位タイプＷ
015210     END-EVALUATE.
014400*================================================================*
014410 保険種別変換 SECTION.
014420*
014430     EVALUATE 作１－保険種別
014440     WHEN 1
               IF 受－保険者番号(1:3) = "3"
014450            MOVE "1"    TO 患－保険種別
               ELSE
014450            MOVE "0"    TO 患－保険種別
               END-IF
014440     WHEN 2
014450         MOVE "3"    TO 患－保険種別
014440     WHEN 3
014450         MOVE "6"    TO 患－保険種別
014440     WHEN 4
014450         MOVE "7"    TO 患－保険種別
014440     WHEN 5
               IF 受－保険者番号(1:2) = "27"
014450            MOVE "B" TO 患－保険種別
               ELSE
014450            MOVE "L" TO 患－保険種別
               END-IF
014440     WHEN 6
014450         MOVE "4"    TO 患－保険種別
014440     WHEN 7
014450         MOVE "5"    TO 患－保険種別
014440     WHEN 8
014450         MOVE "2"    TO 患－保険種別
014440     WHEN 9
014450         MOVE "8"    TO 患－保険種別
014440     WHEN 51
014450         MOVE "C"    TO 患－保険種別
014440     WHEN 52
014450         MOVE "E"    TO 患－保険種別
014440     WHEN 53
014450         MOVE "D"    TO 患－保険種別
014440     WHEN 54
014450         MOVE "H"    TO 患－保険種別
014440     WHEN 55
014450         MOVE "F"    TO 患－保険種別
014440     WHEN 60
014450         MOVE "H"    TO 患－保険種別
014440     WHEN 70
014450         MOVE "9"    TO 患－保険種別
014440     WHEN 80
014450         MOVE "I"    TO 患－保険種別
014440     WHEN 85
014440     WHEN 50
014450         MOVE "A"    TO 患－保険種別
           END-EVALUATE.
015220*================================================================*
015230 日付変換８桁 SECTION.
015240******************************************************************
015250* 和暦年月日Ｗを"yyyymmdd"形式に変換するセクション
015260******************************************************************
015270     PERFORM 西暦年取得.
015280     STRING 西暦年Ｗ DELIMITED BY SIZE
015300            月Ｗ     DELIMITED BY SIZE
015320            日Ｗ     DELIMITED BY SIZE
015330       INTO 日付８桁Ｗ
015340     END-STRING.
015350*================================================================*
015360 施術情報設定 SECTION.
015370******************************************************************
015380* 初検等の回数をカウントする
015390******************************************************************
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
015510         MOVE ZERO  TO 回数カウンタＷ
015540         PERFORM 施術記録Ｆ読込
015550         PERFORM UNTIL ( 終了フラグ２     NOT = SPACE          ) OR
015560                       ( 施記－患者コード NOT = 受－患者コード ) OR
015570                       ( 施記－施術和暦   NOT = 受－施術和暦   ) OR
015580                       ( 施記－施術年     NOT = 受－施術年     ) OR
015590                       ( 施記－施術月     NOT = 受－施術月     )
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
015670             PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
015680                     UNTIL   部位ＣＮＴ > 負－部位数
                       EVALUATE 施記－金属副子区分(部位ＣＮＴ)
                       WHEN 1
015620                     COMPUTE 大回数Ｗ   = 大回数Ｗ + 1
                       WHEN 2
015620                     COMPUTE 中回数Ｗ   = 中回数Ｗ + 1
                       WHEN 3
015620                     COMPUTE 小回数Ｗ   = 小回数Ｗ + 1
                       END-EVALUATE
015610                 IF 施記－情報提供区分(部位ＣＮＴ) = 1
015620                     COMPUTE 情報提供回数Ｗ = 情報提供回数Ｗ + 1
015630                 END-IF
015780             END-PERFORM
015820             PERFORM 施術記録Ｆ読込
015830         END-PERFORM
015840     END-IF.
015850*================================================================*
015860 負傷データＦセット SECTION.
      *
015940     MOVE SPACE          TO 負－レコード.
015900     MOVE 受－施術和暦   TO 負－施術和暦.
015910     MOVE 受－施術年     TO 負－施術年.
015920     MOVE 受－施術月     TO 負－施術月.
015930     MOVE 受－患者番号   TO 負－患者番号.
015940     MOVE 受－枝番       TO 負－枝番.
015950     READ 負傷データＦ
015960     INVALID KEY
015940         MOVE SPACE      TO 負－レコード
016140     END-READ.
015850*================================================================*
015860 レセプトＦセット SECTION.
      *
015940     MOVE SPACE          TO レセ－レコード.
015900     MOVE 受－施術和暦   TO レセ－施術和暦.
015910     MOVE 受－施術年     TO レセ－施術年.
015920     MOVE 受－施術月     TO レセ－施術月.
015930     MOVE 受－患者番号   TO レセ－患者番号.
015940     MOVE 受－枝番       TO レセ－枝番.
           EVALUATE TRUE
           WHEN (作１－保険種別 < 10) AND (作１－保険種別 NOT = 5)
015940         MOVE 1          TO レセ－レセ種別
           WHEN (作１－保険種別 = 5)
015940         MOVE 2          TO レセ－レセ種別
           WHEN (作１－保険種別 > 50) AND (作１－保険種別 <= 60)
015940         MOVE 3          TO レセ－レセ種別
           WHEN (作１－保険種別 = 70)
015940         MOVE 4          TO レセ－レセ種別
           WHEN (作１－保険種別 = 80)
015940         MOVE 5          TO レセ－レセ種別
           WHEN (作１－保険種別 = 50) OR (作１－保険種別 = 85)
015940         MOVE 7          TO レセ－レセ種別
           END-EVALUATE.
015950     READ レセプトＦ
015960     INVALID KEY
015940         MOVE SPACE      TO レセ－レコード
016140     END-READ.
016650*================================================================*
016660 施術記録Ｆ読込 SECTION.
016670*
016680     READ 施術記録Ｆ NEXT
016690     AT END
016700         MOVE "YES" TO 終了フラグ２
016710     END-READ.
017110*================================================================*
017120 作業保険者ファイル作成 SECTION.
017130*
017140     INITIALIZE    作保険－レコード.
017150     MOVE SPACE TO 作保険－レコード.
017160*/健保の保険者(２７老人除く)
017170     IF (受－保険種別   NOT = ZERO ) AND
017190        (受－公費種別       = ZERO)
017191** 自由は対象外
017192         IF  受－保険種別 NOT = 90
017195
017200             MOVE 受－保険種別   TO 保－保険種別
017210             MOVE 受－保険者番号 TO 保－保険者番号
017220             READ 保険者マスタ
017230             INVALID KEY
017240                 INITIALIZE    作保険－レコード
017250                 MOVE SPACE TO 作保険－レコード
017260                 MOVE 受－保険種別   TO 作保険－保険種別キー
017270                 MOVE 受－保険者番号 TO 作保険－保険者番号キー
017280                 READ 作業保険者ファイル
017290                 INVALID KEY
017300                     PERFORM 作業保険者レコードセット健保
017310                     WRITE   作保険－レコード
017320                     INVALID KEY
017330                         MOVE NC"作保険" TO ファイル名
017340                         PERFORM エラー表示
017350                     END-WRITE
017360                 END-READ
017380             NOT INVALID KEY
017390                 INITIALIZE    作保険－レコード
017400                 MOVE SPACE TO 作保険－レコード
017410                 MOVE 保－保険種別   TO 作保険－保険種別キー
017420                 MOVE 保－保険者番号 TO 作保険－保険者番号キー
017430                 READ 作業保険者ファイル
017440                 INVALID KEY
017450                     PERFORM 作業保険者レコードセット健保
017460                     WRITE   作保険－レコード
017470                     INVALID KEY
017480                          MOVE NC"作保険" TO ファイル名
017490                          PERFORM エラー表示
017500                     END-WRITE
017510                 END-READ
017520             END-READ
017521         END-IF
017530     ELSE
017540         INITIALIZE    作保険－レコード
017550         MOVE SPACE TO 作保険－レコード
017590     END-IF.
017600*老人
017610     IF (受－公費種別           NOT = ZERO ) AND
017620        (受－費用負担者番号     NOT = SPACE)
017630         MOVE 受－公費種別       TO 市－公費種別  
017640         MOVE 受－費用負担者番号 TO 市－市町村番号
017650         READ 市町村マスタ
017660         INVALID KEY
017670             CONTINUE
017680         NOT INVALID KEY
017690             INITIALIZE    作保険－レコード
017700             MOVE SPACE TO 作保険－レコード
017710             MOVE 市－公費種別   TO 作保険－保険種別キー
017720             MOVE 市－市町村番号 TO 作保険－保険者番号キー
017730             READ 作業保険者ファイル
017740             INVALID KEY
017750                 PERFORM 作業保険者レコードセット老人
017760                 WRITE   作保険－レコード
017770                 INVALID KEY
017780                      MOVE NC"作保険" TO ファイル名
017790                      PERFORM エラー表示
017800                 END-WRITE
017810             END-READ
017820         END-READ
017830     END-IF.
017840*助成
017850     IF (受－助成種別           NOT = ZERO ) AND
017860        (受－費用負担者番号助成 NOT = SPACE)
017870          MOVE 受－助成種別           TO 市－公費種別  
017880          MOVE 受－費用負担者番号助成 TO 市－市町村番号
017890          READ 市町村マスタ
017900          INVALID KEY
017910              CONTINUE
017920          NOT INVALID KEY
017930              INITIALIZE    作保険－レコード
017940              MOVE SPACE TO 作保険－レコード
017950              MOVE 市－公費種別   TO 作保険－保険種別キー
017960              MOVE 市－市町村番号 TO 作保険－保険者番号キー
017970              READ 作業保険者ファイル
017980              INVALID KEY
017990                  PERFORM 作業保険者レコードセット助成
018000                  WRITE   作保険－レコード
018010                  INVALID KEY
018020                      MOVE NC"作保険" TO ファイル名
018030                      PERFORM エラー表示
018040                  END-WRITE
018050              END-READ
018060          END-READ
018070     END-IF.
018080*================================================================*
018090 作業保険者レコードセット健保 SECTION.
018100*
           MOVE 施情－接骨師会会員番号 TO 作保険－接骨院ＩＤ.
018120*/保険種別
018130     EVALUATE 保－保険種別
018140     WHEN 01
018150         IF 保－保険者番号(3:1) = "3"
018160*/業種別国保：１
018170             MOVE 1               TO 作保険－保険種別 作保険－保険種別２
018180         ELSE
018190*/市町村国保：０
018200             MOVE ZERO            TO 作保険－保険種別 作保険－保険種別２(1:1)
018210         END-IF
018220         MOVE 保－保険者番号(1:2) TO 作保険－県コード
018230*/社保：３
018240     WHEN 02
018250         MOVE  3 TO 作保険－保険種別 作保険－保険種別２
018260         PERFORM 社保県コード判定
018270*/組合：６
018280     WHEN 03
018290         MOVE  6                  TO 作保険－保険種別 作保険－保険種別２
018300         MOVE 保－保険者番号(3:2) TO 作保険－県コード
018310*/共済：７
018320     WHEN 04
018330         MOVE  7                  TO 作保険－保険種別 作保険－保険種別２
018340         MOVE 保－保険者番号(3:2) TO 作保険－県コード
018350*/日雇：４
018360     WHEN 06
018370         MOVE  4                  TO 作保険－保険種別 作保険－保険種別２
018380         MOVE 保－保険者番号(3:2) TO 作保険－県コード
018390*/船員：５
018400     WHEN 07
018410         MOVE  5                  TO 作保険－保険種別 作保険－保険種別２
018420         MOVE 保－保険者番号(3:2) TO 作保険－県コード
018430*/退職：２
018440     WHEN 08
018450         MOVE  2                  TO 作保険－保険種別 作保険－保険種別２
018460         MOVE 保－保険者番号(3:2) TO 作保険－県コード
018470*/自衛官：８
018480     WHEN 09
018490         MOVE  8                  TO 作保険－保険種別 作保険－保険種別２
018500         MOVE 保－保険者番号(3:2) TO 作保険－県コード
018550*/生活保護：10
018560     WHEN 85
018570         MOVE 10                  TO 作保険－保険種別 作保険－保険種別２
018580         MOVE SPACE               TO 作保険－県コード
018510*/自賠責：18
018520     WHEN 80
018530         MOVE 18                  TO 作保険－保険種別 作保険－保険種別２
018540         MOVE SPACE               TO 作保険－県コード
018550*/労災：９
018560     WHEN 70
018570         MOVE  9                  TO 作保険－保険種別 作保険－保険種別２
018580         MOVE SPACE               TO 作保険－県コード
018590     END-EVALUATE.
018600*
018610     MOVE 保－保険者番号     TO 作保険－保険者番号.
018620     MOVE ZERO               TO 作保険－保険者識別ＩＤ.
018660     MOVE SPACE              TO 請求先名称Ｗ.
018670     EVALUATE 保－保険種別
018680** 社保・日雇
018690     WHEN 2
018700     WHEN 6
018720         MOVE 保－保険者名称      TO 請求先名称Ｗ
018790** 組合・共済は支部名まで印字
018800     WHEN 3
018930         IF 保－支部部署名 = SPACE
018931             MOVE 保－保険者名称      TO 請求先名称Ｗ
018936         ELSE
018937             STRING 保－保険者名称    DELIMITED BY SPACE
018939                    "　"              DELIMITED BY SIZE
018940                    保－支部部署名    DELIMITED BY SPACE
018941                    INTO 請求先名称Ｗ
018942             END-STRING
018943         END-IF
018944     WHEN 4
019060         IF 保－支部部署名 = SPACE
019061             MOVE 保－保険者名称      TO 請求先名称Ｗ
019066         ELSE
019067             STRING 保－保険者名称    DELIMITED BY SPACE
019069                    "　"              DELIMITED BY SIZE
019070                    保－支部部署名    DELIMITED BY SPACE
019071                    INTO 請求先名称Ｗ
019072             END-STRING
019073         END-IF
           WHEN 70 
              MOVE 受－施術和暦年月 TO 労災－施術和暦年月
              MOVE 受－患者コード   TO 労災－患者コード
              READ 労災情報Ｆ
              NOT INVALID KEY
                 MOVE 労災－労災事業所名称  TO 請求先名称Ｗ
                 MOVE 労災－労働保険番号    TO 保－保険者番号 作保険－保険者番号 作保険－保険者番号キー
019140           MOVE 労災－労災事業所郵便番号  TO 保－郵便番号
019150           MOVE 労災－労災事業所住所１    TO 保－住所１
019160           MOVE 労災－労災事業所住所２    TO 保－住所２
019170           MOVE 労災－労災事業所電話番号  TO 保－電話番号
              END-READ
           WHEN 85 
              MOVE 受－施術和暦年月 TO 生保－施術和暦年月
              MOVE 受－患者コード   TO 生保－患者コード
              READ 生保情報Ｆ
              NOT INVALID KEY
                 MOVE 生保－生保市町村名        TO 請求先名称Ｗ
                 MOVE 生保－負担者番号          TO 保－保険者番号 作保険－保険者番号 作保険－保険者番号キー
019140           MOVE 生保－生保送付先郵便番号  TO 保－郵便番号
019150           MOVE 生保－生保送付先住所１    TO 保－住所１
019160           MOVE 生保－生保送付先住所２    TO 保－住所２
019170           MOVE 生保－生保電話番号        TO 保－電話番号
              END-READ
019074     WHEN OTHER
019075         MOVE 保－保険者名称      TO 請求先名称Ｗ
019080     END-EVALUATE.
019090     MOVE 請求先名称Ｗ            TO 作保険－保険者名称
019100                                     作保険－郵送先名称
019110                                     作保険－印字用名称.
019120*
019130*     MOVE 保－都道府県ＪＩＳ TO 作保険－県コード.
019140     MOVE 保－郵便番号       TO 作保険－郵便番号.
019150     MOVE 保－住所１         TO 作保険－住所１.
019160     MOVE 保－住所２         TO 作保険－住所２.
019170     MOVE 保－電話番号       TO 作保険－ＴＥＬ.
019180*
019190     MOVE ZERO               TO 作保険－本人負担区分.
019200     MOVE ZERO               TO 作保険－家族負担区分.
019210     MOVE SPACE              TO 作保険－入金予定日.
019220     MOVE SPACE              TO 作保険－入金期間.
019230     MOVE ZERO               TO 作保険－共済区分.
019240*
           MOVE "YES" TO エラーフラグ.
010460     MOVE 保－保険種別   TO 保特－保険種別.
010470     MOVE 保－保険者番号 TO 保特－保険者番号.
010460     MOVE 99999          TO 保特－開始和暦年月.
007550     START 保険者特別負担マスタ   KEY IS < 保特－保険種別
                                                 保特－保険者番号
                                                 保特－開始和暦年月
                                                 REVERSED
           END-START.
006220     IF 状態キー = "00"
010480        READ 保険者特別負担マスタ NEXT
010510        NOT AT END
                 IF (保－保険種別   = 保特－保険種別) AND
                    (保－保険者番号 = 保特－保険者番号)
010520              IF ( 保特－負担率区分 NOT = ZERO )
019540                 COMPUTE 作保険－本人負担率 = 保特－本人負担率 / 10
019550                 COMPUTE 作保険－家族負担率 = 保特－家族負担率 / 10
                       MOVE SPACE TO エラーフラグ
                    END-IF
                 END-IF
              END-READ
           END-IF.
           IF エラーフラグ NOT = SPACE
019390        MOVE 保－保険種別 TO 負率－保険種別
019400        MOVE 請求和暦ＷＲ TO 負率－開始和暦
019410        MOVE 請求年ＷＲ   TO 負率－開始年
019420        MOVE 請求月ＷＲ   TO 負率－開始月
019430        START 負担率マスタ KEY IS <= 負率－保険種別
019440                                     負率－開始和暦年月
019450                                     REVERSED
019460        END-START
019470        IF 状態キー = "00" 
019480            READ 負担率マスタ NEXT
019490            AT END
019500                CONTINUE
019510            NOT AT END
019520                IF ( 請求和暦年月ＷＲ >= 負率－開始和暦年月 ) AND
019530                   ( 請求和暦年月ＷＲ <= 負率－終了和暦年月 )
019540                    COMPUTE 作保険－本人負担率 = 負率－本人負担率 / 10
019550                    COMPUTE 作保険－家族負担率 = 負率－家族負担率 / 10
019560                END-IF
019570            END-READ
019580        END-IF
010830     END-IF.
022190     MOVE "2001/01/01"        TO 作保険－更新日付.
019600*
019830*================================================================*
019840 作業保険者レコードセット老人 SECTION.
019850*
019860     MOVE 市－公費種別        TO 作保険－保険種別キー  .
019870     MOVE 市－市町村番号      TO 作保険－保険者番号キー.
           MOVE 施情－接骨師会会員番号 TO 作保険－接骨院ＩＤ.
019890     IF 市－公費種別 = 05
019900         MOVE 21              TO 作保険－保険種別 作保険－保険種別２
019910     END-IF.
019920     MOVE 市－市町村番号      TO 作保険－保険者番号.
019930     MOVE 市－市町村番号(3:2) TO 作保険－県コード.
019940     MOVE ZERO                TO 作保険－保険者識別ＩＤ
019950     MOVE 市－市町村名称      TO 作保険－保険者名称
019960                                 作保険－郵送先名称
019970                                 作保険－印字用名称.
019990     MOVE 市－郵便番号        TO 作保険－郵便番号.
020000     MOVE 市－住所１          TO 作保険－住所１.
020010     MOVE 市－住所２          TO 作保険－住所２.
020020     MOVE 市－電話番号        TO 作保険－ＴＥＬ.
020030     MOVE 1                   TO 作保険－本人負担区分.
020040*
           MOVE "YES" TO エラーフラグ.
010460     MOVE 市－公費種別   TO 保特－保険種別.
010470     MOVE 市－保険者番号 TO 保特－保険者番号.
010460     MOVE 99999          TO 保特－開始和暦年月.
007550     START 保険者特別負担マスタ   KEY IS < 保特－保険種別
                                                 保特－保険者番号
                                                 保特－開始和暦年月
                                                 REVERSED
           END-START.
006220     IF 状態キー = "00"
010480        READ 保険者特別負担マスタ NEXT
010510        NOT AT END
                 IF (市－公費種別   = 保特－保険種別) AND
                    (市－保険者番号 = 保特－保険者番号)
010520              IF ( 保特－負担率区分 NOT = ZERO ) OR
010530                 ( 受－甲乙区分   NOT = ZERO )
010540                 IF 受－甲乙区分 NOT = 2
010550                     MOVE 保特－本人負担率   TO 作保険－本人負担率
010560                     MOVE 保特－家族負担率   TO 作保険－家族負担率
010570                 ELSE
010580                     MOVE 保特－本人負担率乙 TO 作保険－本人負担率
010590                     MOVE 保特－家族負担率乙 TO 作保険－家族負担率
010600                 END-IF
                       MOVE SPACE TO エラーフラグ
                    END-IF
                 END-IF
              END-READ
           END-IF.
           IF エラーフラグ NOT = SPACE
019390        MOVE 市－公費種別 TO 負率－保険種別
019400        MOVE 請求和暦ＷＲ TO 負率－開始和暦
019410        MOVE 請求年ＷＲ   TO 負率－開始年
019420        MOVE 請求月ＷＲ   TO 負率－開始月
019430        START 負担率マスタ KEY IS <= 負率－保険種別
019440                                     負率－開始和暦年月
019450                                     REVERSED
019460        END-START
019470        IF 状態キー = "00" 
019480            READ 負担率マスタ NEXT
019490            AT END
019500                CONTINUE
019510            NOT AT END
019520                IF ( 請求和暦年月ＷＲ >= 負率－開始和暦年月 ) AND
019530                   ( 請求和暦年月ＷＲ <= 負率－終了和暦年月 )
019540                    COMPUTE 作保険－本人負担率 = 負率－本人負担率 / 10
019550                    COMPUTE 作保険－家族負担率 = 負率－家族負担率 / 10
019560                END-IF
019570            END-READ
019580        END-IF
010830     END-IF.
020350*
020360     MOVE ZERO                TO 作保険－家族負担区分.
020370     MOVE SPACE               TO 作保険－入金予定日.
020380     MOVE SPACE               TO 作保険－入金期間.
020390     MOVE ZERO                TO 作保険－共済区分.
022190     MOVE "2001/01/01"        TO 作保険－更新日付.
020400*
020630*================================================================*
020640 作業保険者レコードセット助成 SECTION.
020650*
020660     MOVE 市－公費種別        TO 作保険－保険種別キー  .
020670     MOVE 市－市町村番号      TO 作保険－保険者番号キー.
020680*     MOVE 会員番号コードＷ    TO 作保険－接骨院ＩＤ.
           MOVE 施情－接骨師会会員番号 TO 作保険－接骨院ＩＤ.
020681     MOVE ZERO                TO 作保険－本人負担区分.
020690     EVALUATE 市－公費種別
020700*/生活保護：10
020710     WHEN 50
020720         MOVE 10              TO 作保険－保険種別 作保険－保険種別２
020730*/４１老人：12
020740     WHEN 51
020750         MOVE 12              TO 作保険－保険種別 作保険－保険種別２
020751         MOVE 2               TO 作保険－本人負担区分
020760*/母子：16
020770     WHEN 52
020780         MOVE 16              TO 作保険－保険種別 作保険－保険種別２
020790*/身障：13
020800     WHEN 53
020810         MOVE 13              TO 作保険－保険種別 作保険－保険種別２
020820*/被爆：なし、その他：17
020830     WHEN 54
020840         MOVE 17              TO 作保険－保険種別 作保険－保険種別２
020850*/乳幼児：15
020860     WHEN 55
020870         MOVE 15              TO 作保険－保険種別 作保険－保険種別２
020880*/その他：17
020890     WHEN 60
020900         MOVE 17              TO 作保険－保険種別 作保険－保険種別２
020910     END-EVALUATE.
020920     MOVE 市－市町村番号      TO 作保険－保険者番号.
020930     MOVE 市－市町村番号(3:2) TO 作保険－県コード.
020940     MOVE ZERO                TO 作保険－保険者識別ＩＤ
020950     MOVE 市－市町村名称      TO 作保険－保険者名称
020960                                 作保険－郵送先名称
020970                                 作保険－印字用名称.
020980*     MOVE 市－都道府県ＪＩＳ TO 作保険－県コード.
020990     MOVE 市－郵便番号        TO 作保険－郵便番号.
021000     MOVE 市－住所１          TO 作保険－住所１.
021010     MOVE 市－住所２          TO 作保険－住所２.
021020     MOVE 市－電話番号        TO 作保険－ＴＥＬ.
021040*
           MOVE "YES" TO エラーフラグ.
010460     MOVE 市－公費種別   TO 保特－保険種別.
010470     MOVE 市－保険者番号 TO 保特－保険者番号.
010460     MOVE 99999          TO 保特－開始和暦年月.
007550     START 保険者特別負担マスタ   KEY IS < 保特－保険種別
                                                 保特－保険者番号
                                                 保特－開始和暦年月
                                                 REVERSED
           END-START.
006220     IF 状態キー = "00"
010480        READ 保険者特別負担マスタ NEXT
010510        NOT AT END
                 IF (市－公費種別   = 保特－保険種別) AND
                    (市－保険者番号 = 保特－保険者番号)
010520              IF ( 保特－負担率区分 NOT = ZERO ) OR
010530                 ( 受－甲乙区分   NOT = ZERO )
010540                 IF 受－甲乙区分 NOT = 2
010550                     MOVE 保特－本人負担率   TO 作保険－本人負担率
010560                     MOVE 保特－家族負担率   TO 作保険－家族負担率
010570                 ELSE
010580                     MOVE 保特－本人負担率乙 TO 作保険－本人負担率
010590                     MOVE 保特－家族負担率乙 TO 作保険－家族負担率
010600                 END-IF
                       MOVE SPACE TO エラーフラグ
                    END-IF
                 END-IF
              END-READ
           END-IF.
           IF エラーフラグ NOT = SPACE
019390        MOVE 市－公費種別 TO 負率－保険種別
019400        MOVE 請求和暦ＷＲ TO 負率－開始和暦
019410        MOVE 請求年ＷＲ   TO 負率－開始年
019420        MOVE 請求月ＷＲ   TO 負率－開始月
019430        START 負担率マスタ KEY IS <= 負率－保険種別
019440                                     負率－開始和暦年月
019450                                     REVERSED
019460        END-START
019470        IF 状態キー = "00" 
019480            READ 負担率マスタ NEXT
019490            AT END
019500                CONTINUE
019510            NOT AT END
019520                IF ( 請求和暦年月ＷＲ >= 負率－開始和暦年月 ) AND
019530                   ( 請求和暦年月ＷＲ <= 負率－終了和暦年月 )
019540                    COMPUTE 作保険－本人負担率 = 負率－本人負担率 / 10
019550                    COMPUTE 作保険－家族負担率 = 負率－家族負担率 / 10
019560                END-IF
019570            END-READ
019580        END-IF
010830     END-IF.
022150*
022160     MOVE ZERO                TO 作保険－家族負担区分.
022170     MOVE SPACE               TO 作保険－入金予定日.
022180     MOVE SPACE               TO 作保険－入金期間.
022190     MOVE ZERO                TO 作保険－共済区分.
022190     MOVE "2001/01/01"        TO 作保険－更新日付.
022200*
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
022842*================================================================*
022843 保険者ファイル作成 SECTION.
022844* レコード可変長バージョン
022845     OPEN INPUT 作業保険者ファイル.
022846         MOVE NC"作保険" TO ファイル名.
022847         PERFORM オープンチェック.
022848*
022849     MOVE ZERO TO 作保険－保険種別.
022850     MOVE ZERO TO 作保険－保険者番号キー.
022851     START 作業保険者ファイル  KEY IS >= 作保険－保険種別
022852                                         作保険－保険者番号キー
022853     END-START
022854     IF 状態キー = "00"
022855         MOVE 196 TO 文字カウンタ
022856         PERFORM 保険－ヘッダ書込
022857*         MOVE 327 TO 文字カウンタ
022857         MOVE 334 TO 文字カウンタ
022858         MOVE SPACE TO 終了フラグ
022859         PERFORM 作業保険者ファイル読込
022860         PERFORM UNTIL 終了フラグ NOT = SPACE
022861* 自賠責、自費は除外
022862*             IF 作保険－保険種別 NOT = 18
022863                 PERFORM 保険者レコードセット
022864                 PERFORM 保険者ファイル書込
022865*             END-IF
022866             PERFORM 作業保険者ファイル読込
022867         END-PERFORM
022868     END-IF.
022869     CLOSE 作業保険者ファイル.
022870*================================================================*
022871 作業保険者ファイル読込 SECTION.
022872*
022880     READ 作業保険者ファイル NEXT
022890     AT END
022900         MOVE "YES" TO 終了フラグ
022910     END-READ.
022920*================================================================*
022930 保険者ファイル書込 SECTION.
022940*
023050     WRITE 保険－レコード
023060     IF 状態キー  NOT =  "00"
023070         MOVE NC"保険"  TO ファイル名
023080         PERFORM エラー表示
023090     END-IF.
026650     PERFORM 遅延処理.
023471*
023472*================================================================*
023473 保険者レコードセット SECTION.
023474*
023475     INITIALIZE    保険－レコード.
023476     MOVE SPACE TO 保険－レコード 保険者データＷ.
023477
023479* 保険種別
023480     IF 作保険－保険種別(1:1) = ZERO
023481         MOVE 作保険－保険種別(2:1) TO 保険種別Ｗ(1:1)
023482     ELSE
023483         MOVE 作保険－保険種別      TO 保険種別Ｗ
023484     END-IF.
023486
023487* 保険者名称データ長取得
023488     MOVE SPACE TO 終了フラグ２.
023489     PERFORM VARYING 文字カウンタ２ FROM 60 BY -1
023490             UNTIL   (文字カウンタ２    <= ZERO ) OR
023491                     (終了フラグ２ NOT = SPACE)
023492         IF 作保険－保険者名称(文字カウンタ２:1) NOT = SPACE
023493             COMPUTE 文字カウンタ２ = 文字カウンタ２ + 1
023494             MOVE "YES" TO 終了フラグ２
023495         END-IF
023496     END-PERFORM.
023497     IF 文字カウンタ２ = ZERO
023498         MOVE " " TO 保険者名称Ｗ
023499         MOVE 1   TO 文字カウンタ２
023500     ELSE
023501         MOVE 作保険－保険者名称(1:文字カウンタ２) TO 保険者名称Ｗ
023502     END-IF.
023503
023504* 郵送先名称データ長取得
023505     MOVE SPACE TO 終了フラグ２.
023506     PERFORM VARYING 文字カウンタ３ FROM 60 BY -1
023507             UNTIL   (文字カウンタ３    <= ZERO ) OR
023508                     (終了フラグ２ NOT = SPACE)
023509         IF 作保険－郵送先名称(文字カウンタ３:1) NOT = SPACE
023510             COMPUTE 文字カウンタ３ = 文字カウンタ３ + 1
023511             MOVE "YES" TO 終了フラグ２
023512         END-IF
023513     END-PERFORM.
023514     IF 文字カウンタ３ = ZERO
023515         MOVE " " TO 郵送先名称Ｗ
023516         MOVE 1   TO 文字カウンタ３
023517     ELSE
023518         MOVE 作保険－郵送先名称(1:文字カウンタ３) TO 郵送先名称Ｗ
023519     END-IF.
023520
023521* 印字用名称データ長取得
023522     MOVE SPACE TO 終了フラグ２.
023523     PERFORM VARYING 文字カウンタ４ FROM 60 BY -1
023524             UNTIL   (文字カウンタ４    <= ZERO ) OR
023525                     (終了フラグ２ NOT = SPACE)
023526         IF 作保険－印字用名称(文字カウンタ４:1) NOT = SPACE
023527             COMPUTE 文字カウンタ４ = 文字カウンタ４ + 1
023528             MOVE "YES" TO 終了フラグ２
023529         END-IF
023530     END-PERFORM.
023531     IF 文字カウンタ４ = ZERO
023532         MOVE " " TO 印字用名称Ｗ
023533         MOVE 1   TO 文字カウンタ４
023534     ELSE
023535         MOVE 作保険－印字用名称(1:文字カウンタ４) TO 印字用名称Ｗ
023536     END-IF.
023537
023538* 住所１データ長取得
023539     MOVE SPACE TO 終了フラグ２.
023540     PERFORM VARYING 文字カウンタ５ FROM 40 BY -1
023541             UNTIL   (文字カウンタ５    <= ZERO ) OR
023542                     (終了フラグ２ NOT = SPACE)
023543         IF 作保険－住所１(文字カウンタ５:1) NOT = SPACE
023544             COMPUTE 文字カウンタ５ = 文字カウンタ５ + 1
023545             MOVE "YES" TO 終了フラグ２
023546         END-IF
023547     END-PERFORM.
023548     IF 文字カウンタ５ = ZERO
023549         MOVE " " TO 住所１Ｗ
023550         MOVE 1   TO 文字カウンタ５
023551     ELSE
023552         MOVE 作保険－住所１(1:文字カウンタ５) TO 住所１Ｗ
023553     END-IF.
023554
023555* 住所２データ長取得
023556     MOVE SPACE TO 終了フラグ２.
023557     PERFORM VARYING 文字カウンタ６ FROM 40 BY -1
023558             UNTIL   (文字カウンタ６    <= ZERO ) OR
023559                     (終了フラグ２ NOT = SPACE)
023560         IF 作保険－住所２(文字カウンタ６:1) NOT = SPACE
023561             COMPUTE 文字カウンタ６ = 文字カウンタ６ + 1
023562             MOVE "YES" TO 終了フラグ２
023563         END-IF
023564     END-PERFORM.
023565     IF 文字カウンタ６ = ZERO
023566         MOVE " " TO 住所２Ｗ
023567         MOVE 1   TO 文字カウンタ６
023568     ELSE
023569         MOVE 作保険－住所２(1:文字カウンタ６) TO 住所２Ｗ
023570     END-IF.
023571     
023572* 保険者番号取得
023573     IF (作保険－保険者番号(1:2) NOT = "99") AND
023574        (作保険－保険種別キー    NOT =  70 ) AND
023575        (作保険－保険種別キー    NOT =  80 )
023576         MOVE 作保険－保険者番号        TO 保険者番号Ｗ
023577     ELSE
023578         MOVE " " TO 保険者番号Ｗ
023579     END-IF.
023580
023581     STRING 作保険－接骨院ＩＤ      DELIMITED BY SPACE
023582            ","                     DELIMITED BY SIZE
023583*	          作保険－保険種別        DELIMITED BY SPACE
023584	          保険種別Ｗ              DELIMITED BY SPACE
023585            ","                     DELIMITED BY SIZE
023586            保険者番号Ｗ            DELIMITED BY SPACE
023587            ","                     DELIMITED BY SIZE
023588            作保険－保険者識別ＩＤ  DELIMITED BY SPACE
023589            ","                     DELIMITED BY SIZE
023590*            作保険－保険者名称(1:文字カウンタ２) 
023591            保険者名称Ｗ(1:文字カウンタ２) 
023592            ","                     DELIMITED BY SIZE
023593            郵送先名称Ｗ(1:文字カウンタ３)
023594            ","                     DELIMITED BY SIZE
023595            印字用名称Ｗ(1:文字カウンタ４)
023596            ","                     DELIMITED BY SIZE
023597            作保険－県コード        DELIMITED BY SPACE
023598            ","                     DELIMITED BY SIZE
023599            作保険－郵便番号        DELIMITED BY SPACE
023600            ","                     DELIMITED BY SIZE
023601            住所１Ｗ(1:文字カウンタ５)
023602            ","                     DELIMITED BY SIZE
023603            住所２Ｗ(1:文字カウンタ６)
023604            ","                     DELIMITED BY SIZE
023605            作保険－ＴＥＬ          DELIMITED BY SPACE
023606            ","                     DELIMITED BY SIZE
023607            作保険－本人負担区分    DELIMITED BY SPACE
023608            ","                     DELIMITED BY SIZE
023609            作保険－本人負担率      DELIMITED BY SPACE
023610            ","                     DELIMITED BY SIZE
023611            作保険－家族負担区分    DELIMITED BY SPACE
023612            ","                     DELIMITED BY SIZE
023613            作保険－家族負担率      DELIMITED BY SPACE
023614            ","                     DELIMITED BY SIZE
023615            作保険－入金予定日      DELIMITED BY SIZE
023616            ","                     DELIMITED BY SIZE
023617            作保険－入金期間        DELIMITED BY SIZE
023618            ","                     DELIMITED BY SIZE
023619            作保険－共済区分        DELIMITED BY SPACE
023620            ","                     DELIMITED BY SIZE
023621            作保険－更新日付        DELIMITED BY SIZE
023622            
023623            INTO 保険者データＷ
023624     END-STRING.
023625
023626* １レコードデータ長取得
023627     MOVE SPACE TO 終了フラグ２.
023628     PERFORM VARYING 文字カウンタ FROM 334 BY -1
023629             UNTIL   (文字カウンタ    <= ZERO ) OR
023630                     (終了フラグ２ NOT = SPACE)
023631         IF 保険者データＷ(文字カウンタ:1) NOT = SPACE
023632             COMPUTE 文字カウンタ = 文字カウンタ + 1
023633             MOVE "YES" TO 終了フラグ２
023634         END-IF
023635     END-PERFORM.
023636
023637     MOVE 保険者データＷ(1:文字カウンタ) TO 保険－レコードデータ.
023638
023639*================================================================*
023640 保険－ヘッダ書込 SECTION.
023641*
023642     INITIALIZE    保険－レコード.
023643     MOVE SPACE TO 保険－レコード.
023644     MOVE "整骨院ID,保険種別,保険者番号,保険者識別ＩＤ,保険者名称,郵送先名称,"    TO 保険－レコード(1:66)
023645     MOVE "印字用名称,県コード,郵便番号,住所1,住所2,TEL,本人負担区分,本人負担率," TO 保険－レコード(67:69)
023646     MOVE "家族負担区分,家族負担率,入金予定日,入金期間,共済区分,更新日付"         TO 保険－レコード(136:61)
023647     PERFORM 保険者ファイル書込.
023960*================================================================*
023970 社保県コード判定 SECTION.
023980*
023990     EVALUATE 保－保険者番号(1:2)
024000     WHEN 01
024010         MOVE 01 TO 作保険－県コード
024020     WHEN 02
024030         MOVE 02 TO 作保険－県コード
024040     WHEN 03
024050         MOVE 03 TO 作保険－県コード
024060     WHEN 04
024070         MOVE 04 TO 作保険－県コード
024080     WHEN 05
024090         MOVE 05 TO 作保険－県コード
024100     WHEN 06
024110         MOVE 06 TO 作保険－県コード
024120     WHEN 07
024130         MOVE 07 TO 作保険－県コード
024140     WHEN 08
024150         MOVE 08 TO 作保険－県コード
024160     WHEN 09
024170         MOVE 09 TO 作保険－県コード
024180     WHEN 10
024190         MOVE 10 TO 作保険－県コード
024200     WHEN 11
024210         MOVE 11 TO 作保険－県コード
024220     WHEN 12
024230         MOVE 12 TO 作保険－県コード
024240*
024250     WHEN 21
024260         MOVE 13 TO 作保険－県コード
024270*
024280     WHEN 31
024290         MOVE 14 TO 作保険－県コード
024300     WHEN 32
024310         MOVE 15 TO 作保険－県コード
024320     WHEN 33
024330         MOVE 16 TO 作保険－県コード
024340     WHEN 34
024350         MOVE 17 TO 作保険－県コード
024360     WHEN 35
024370         MOVE 18 TO 作保険－県コード
024380     WHEN 36
024390         MOVE 19 TO 作保険－県コード
024400     WHEN 37
024410         MOVE 20 TO 作保険－県コード
024420     WHEN 38
024430         MOVE 21 TO 作保険－県コード
024440     WHEN 39
024450         MOVE 22 TO 作保険－県コード
024460*
024470     WHEN 41
024480         MOVE 23 TO 作保険－県コード
024490     WHEN 42
024500         MOVE 24 TO 作保険－県コード
024510*
024520     WHEN 51
024530         MOVE 25 TO 作保険－県コード
024540     WHEN 52
024550         MOVE 26 TO 作保険－県コード
024560     WHEN 53
024570         MOVE 27 TO 作保険－県コード
024580     WHEN 54
024590         MOVE 28 TO 作保険－県コード
024600     WHEN 55
024610         MOVE 29 TO 作保険－県コード
024620     WHEN 56
024630         MOVE 30 TO 作保険－県コード
024640     WHEN 57
024650         MOVE 31 TO 作保険－県コード
024660     WHEN 58
024670         MOVE 32 TO 作保険－県コード
024680     WHEN 59
024690         MOVE 33 TO 作保険－県コード
024700     WHEN 60
024710         MOVE 34 TO 作保険－県コード
024720     WHEN 61
024730         MOVE 35 TO 作保険－県コード
024740*
024750     WHEN 71
024760         MOVE 36 TO 作保険－県コード
024770     WHEN 72
024780         MOVE 37 TO 作保険－県コード
024790     WHEN 73
024800         MOVE 38 TO 作保険－県コード
024810     WHEN 74
024820         MOVE 39 TO 作保険－県コード
024830     WHEN 75
024840         MOVE 40 TO 作保険－県コード
024850     WHEN 76
024860         MOVE 41 TO 作保険－県コード
024870     WHEN 77
024880         MOVE 42 TO 作保険－県コード
024890     WHEN 78
024900         MOVE 43 TO 作保険－県コード
024910     WHEN 79
024920         MOVE 44 TO 作保険－県コード
024930     WHEN 80
024940         MOVE 45 TO 作保険－県コード
024950     WHEN 81
024960         MOVE 46 TO 作保険－県コード
024970     WHEN 82
024980         MOVE 47 TO 作保険－県コード
024990     END-EVALUATE.
027970*================================================================*
027980 患者情報ファイル作成 SECTION.
027990*
028000     OPEN INPUT 作業ファイル１.
028010         MOVE NC"作１" TO ファイル名.
028020         PERFORM オープンチェック.
026610*
026620     OPEN OUTPUT 作業保険者ファイル.
026630         MOVE NC"作保険" TO ファイル名.
026640         PERFORM オープンチェック.
026650     PERFORM 遅延処理.
026660     CLOSE 作業保険者ファイル.
026670     PERFORM 遅延処理.
026680     OPEN I-O 作業保険者ファイル.
026690         MOVE NC"作保険" TO ファイル名.
026700         PERFORM オープンチェック.
028070*
010090     MOVE 956   TO 文字カウンタ.
           MOVE 1     TO レセプト番号Ｗ.
028080*/並び順を、保険種別、提出保険者番号、患者コード、施術年月順にする。
028090     MOVE ZERO  TO 作１－印刷順序
028100     MOVE SPACE TO 作１－保番
028100     MOVE SPACE TO 作１－種別
028100     MOVE SPACE TO 作１－保険者番号
028090     MOVE ZERO  TO 作１－本人家族区分
028110     MOVE SPACE TO 作１－患者コード
028120     MOVE ZERO  TO 作１－施術和暦年月
028130     START 作業ファイル１ KEY IS >= 作１－印刷順序
028140                                    作１－保番
028140                                    作１－種別
028140                                    作１－保険者番号
028140                                    作１－本人家族区分
028150                                    作１－患者コード
028160                                    作１－施術和暦年月
028170     END-START.
028180     IF 状態キー = "00"
028190         MOVE SPACE  TO 終了フラグ
028200         PERFORM 作業ファイル１読込
028210         PERFORM UNTIL ( 終了フラグ = "YES" )
028220*
028230            MOVE 作１－施術和暦年月 TO 受－施術和暦年月
028240            MOVE 作１－患者コード   TO 受－患者コード
028250            READ 受診者情報Ｆ
028260            INVALID KEY
028270                CONTINUE
028280            NOT INVALID KEY
028310                PERFORM 負傷データＦセット
028310                PERFORM レセプトＦセット
028340*
028350                PERFORM 共通レコードセット
                      IF レセ－レセ種別 = 3
028460                    PERFORM 助成レコードセット
028360                ELSE
028390                    PERFORM 健保レコードセット
                      END-IF
028400                PERFORM 患者情報ファイル書込
026670                PERFORM 遅延処理
027824                PERFORM 作業保険者ファイル作成
028510*
028540             END-READ
028530             PERFORM 作業ファイル１読込
028550         END-PERFORM
028560     END-IF.
028570*
028580     CLOSE 作業ファイル１ 作業保険者ファイル.
028590*================================================================*
028600 作業ファイル１読込 SECTION.
028610*
028620     READ 作業ファイル１ NEXT
028630     AT END
028640         MOVE "YES" TO 終了フラグ
028650     END-READ.
028660*================================================================*
028670******************************************************************
028680 END PROGRAM YIW101.
028690******************************************************************
