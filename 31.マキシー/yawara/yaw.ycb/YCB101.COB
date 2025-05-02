000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YCB101.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*      中部柔整師協会 会提出フロッピー作成【ﾃﾞｰﾀ作成】
000100*
000110* ※ 請求年月Verのみ. 
000120*
000130*      MED = YCB580 
000140*----------------------------------------------------------------*
000150 DATE-WRITTEN.           2012-11-15
000160 DATE-COMPILED.          2012-11-15
000170*----------------------------------------------------------------*
000180******************************************************************
000190*            ENVIRONMENT         DIVISION                        *
000200******************************************************************
000210 ENVIRONMENT             DIVISION.
000220 CONFIGURATION           SECTION.
000230 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000240 OBJECT-COMPUTER.        FMV-DESKPOWER.
000250 SPECIAL-NAMES.          CONSOLE  IS  CONS
000260                         SYSERR   IS  MSGBOX.
000270 INPUT-OUTPUT            SECTION.
000280 FILE-CONTROL.
000290     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000300                             ORGANIZATION             IS  INDEXED
000310                             ACCESS MODE              IS  DYNAMIC
000320                             RECORD KEY               IS  制－制御区分
000330                             FILE STATUS              IS  状態キー
000340                             LOCK        MODE         IS  AUTOMATIC.
000350     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000360                             ORGANIZATION             IS  INDEXED
000370                             ACCESS MODE              IS  DYNAMIC
000380                             RECORD KEY               IS  元－元号区分
000390                             FILE STATUS              IS  状態キー
000400                             LOCK        MODE         IS  AUTOMATIC.
000410     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000420                             ORGANIZATION             IS  INDEXED
000430                             ACCESS MODE              IS  DYNAMIC
000440                             RECORD KEY               IS  名－区分コード
000450                                                          名－名称コード
000460                             FILE STATUS              IS  状態キー
000470                             LOCK        MODE         IS  AUTOMATIC.
000480     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000490                             ORGANIZATION             IS  INDEXED
000500                             ACCESS MODE              IS  DYNAMIC
000510                             RECORD KEY               IS 施情－施術所番号
000520                             FILE STATUS              IS  状態キー
000530                             LOCK        MODE         IS  AUTOMATIC.
000540     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
000550                             ORGANIZATION             IS  INDEXED
000560                             ACCESS MODE              IS  DYNAMIC
000570                             RECORD KEY           IS 施記－施術和暦年月日
000580                                                     施記－患者コード
000590                             ALTERNATE RECORD KEY IS 施記－患者コード
000600                                                     施記－施術和暦年月日
000610                             FILE STATUS              IS  状態キー
000620                             LOCK        MODE         IS  AUTOMATIC.
000630     SELECT  保険者マスタ    ASSIGN      TO        HOKENSL
000640                             ORGANIZATION             IS  INDEXED
000650                             ACCESS MODE              IS  DYNAMIC
000660                             RECORD KEY               IS  保－保険種別
000670                                                          保－保険者番号
000680                             ALTERNATE RECORD KEY     IS  保－保険種別
000690                                                          保－保険者名称
000700                                                          保－保険者番号
000710                             FILE STATUS              IS  状態キー
000720                             LOCK        MODE         IS  AUTOMATIC.
000730     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000740                             ORGANIZATION             IS  INDEXED
000750                             ACCESS MODE              IS  DYNAMIC
000760                             RECORD KEY               IS  受－施術和暦年月
000770                                                          受－患者コード
000780                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000790                                                          受－患者カナ
000800                                                          受－患者コード
000810                             ALTERNATE RECORD KEY     IS  受－患者コード
000820                                                          受－施術和暦年月
000830                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000840                                                          受－保険種別
000850                                                          受－保険者番号
000860                                                          受－患者コード
000870                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000880                                                          受－公費種別
000890                                                          受－費用負担者番号
000900                                                          受－患者コード
000910                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000920                                                          受－助成種別
000930                                                          受－費用負担者番号助成
000940                                                          受－患者コード
000950                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
000960                                                          受－施術和暦年月
000970                                                          受－患者コード
000980                             FILE STATUS              IS  状態キー
000990                             LOCK        MODE         IS  AUTOMATIC.
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
001000     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
001010                             ORGANIZATION             IS  INDEXED
001020                             ACCESS MODE              IS  DYNAMIC
001030                             RECORD KEY               IS 負－施術和暦年月
001040                                                         負－患者コード
001050                             ALTERNATE RECORD KEY     IS 負－患者コード
001060                                                         負－施術和暦年月
001070                             FILE STATUS              IS  状態キー
001080                             LOCK        MODE         IS  AUTOMATIC.
001090     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
001100                             ORGANIZATION             IS  INDEXED
001110                             ACCESS MODE              IS  DYNAMIC
001120                             RECORD KEY               IS  負原－区分コード
001130                                                          負原－負傷原因コード
001140                             FILE STATUS              IS  状態キー
001150                             LOCK        MODE         IS  AUTOMATIC.
001160     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
001170                             ORGANIZATION             IS  INDEXED
001180                             ACCESS MODE              IS  DYNAMIC
001190                             RECORD KEY               IS  市－公費種別
001200                                                          市－市町村番号
001210                             ALTERNATE RECORD KEY     IS  市－公費種別
001220                                                          市－市町村名称
001230                                                          市－市町村番号
001240                             FILE STATUS              IS  状態キー
001250                             LOCK        MODE         IS  AUTOMATIC.
001260     SELECT  請求先マスタ    ASSIGN      TO        SEIKYUSL
001270                             ORGANIZATION           IS  INDEXED
001280                             ACCESS MODE            IS  DYNAMIC
001290                             RECORD KEY             IS 請先－保険種別
001300                                                       請先－保険者番号
001310                             FILE STATUS            IS  状態キー
001320                             LOCK    MODE           IS  AUTOMATIC.
001390     SELECT  作業ファイル１  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W1011L.DAT"
001400                             ORGANIZATION             IS  SEQUENTIAL
001410                             ACCESS                   IS  SEQUENTIAL
001420                             FILE        STATUS       IS  状態キー
001430                             LOCK        MODE         IS  AUTOMATIC.
001440     SELECT  作業ファイル２  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W1012L.DAT"
001450                             ORGANIZATION             IS  SEQUENTIAL
001460                             ACCESS                   IS  SEQUENTIAL
001470                             FILE        STATUS       IS  状態キー
001480                             LOCK        MODE         IS  AUTOMATIC.
001540*
001550*  保険者番号順ﾌｧｲﾙ
001560     SELECT  作業ファイル４  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W1014L.DAT"
001570                             ORGANIZATION             IS  INDEXED
001580                             ACCESS                   IS  DYNAMIC
001590                             RECORD      KEY          IS  作４－請求和暦年月
001600                                                          作４－保険種別
001610                                                          作４－保険者番号
001620                                                          作４－施術和暦年月
001630                                                          作４－被保険者カナ
001640                                                          作４－患者コード
001650                                                          作４－助成区分
001660                             FILE        STATUS       IS  状態キー
001670                             LOCK        MODE         IS  AUTOMATIC.
001680*
001690******************************************************************
001700*                      DATA DIVISION                             *
001710******************************************************************
001720 DATA                    DIVISION.
001730 FILE                    SECTION.
001740*                           ［ＲＬ＝  ２５６］
001750 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001760     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
001770*                           ［ＲＬ＝  １２８］
001780 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001790     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001800*                           ［ＲＬ＝  １２８］
001810 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
001820     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
001830*
001840 FD  施術所情報マスタ    BLOCK   CONTAINS   1   RECORDS.
001850     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
001860*                           ［ＲＬ＝  ２５６］
001870 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
001880     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
001890*                           ［ＲＬ＝  ３２０］
001900 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001910     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
001920*                           ［ＲＬ＝  ３２０］
001930 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
001940     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
001950*                           ［ＲＬ＝  １２８］
001960 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
001970     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
001980*                           ［ＲＬ＝  １２８］
001990 FD  負傷原因Ｆ         BLOCK   CONTAINS   1   RECORDS.
002000     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
002010*                           ［ＲＬ＝  ２５６］
002020 FD  市町村マスタ          BLOCK   CONTAINS   1   RECORDS.
002030     COPY SITYOSN        OF  XFDLIB  JOINING   市   AS  PREFIX.
002040*                           ［ＲＬ＝  １２８］
002050 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
002060     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
002100**
002110 FD  作業ファイル１ RECORD  CONTAINS 768 CHARACTERS.
002120 01  作１－レコード.
002130*   / ヘッダ部は使用しない /
002140     03  作１－レコードヘッダ.
002150         05  作１－請求和暦年月キー.
002160             07  作１－請求和暦キー        PIC 9.
002170             07  作１－請求年キー          PIC 9(2).
002180             07  作１－請求月キー          PIC 9(2).
002190         05  作１－保険区分キー            PIC 9.
002200         05  作１－保険者番号キー          PIC 9(8).
002210         05  作１－本人家族区分キー        PIC 9.
002220         05  作１－被保険者カナキー        PIC X(20).
002230         05  作１－患者コードキー.
002240             07 作１－患者番号キー         PIC 9(6).
002250             07 作１－枝番                 PIC X(1).
002260         05  作１－施術和暦年月キー.
002270             07  作１－施術和暦キー        PIC 9.
002280             07  作１－施術年キー          PIC 9(2).
002290             07  作１－施術月キー          PIC 9(2).
002300     03  作１－レコードデータ.
002310         05  作１－会員番号                PIC 9(4).
002320         05  作１－請求年月.
002330             07  作１－請求年              PIC X(4).
002340             07  作１－請求区切１          PIC X(1).
002350             07  作１－請求月              PIC X(2).
002360             07  作１－請求区切２          PIC X(1).
002370             07  作１－請求日              PIC X(2).
002380         05  作１－レセプト番号            PIC 9(4).
002390         05  作１－施術年月.
002400             07  作１－施術年              PIC X(4).
002410             07  作１－施術区切１          PIC X(1).
002420             07  作１－施術月              PIC X(2).
002430             07  作１－施術区切２          PIC X(1).
002440             07  作１－施術日              PIC X(2).
002450         05  作１－保険種別                PIC N(4).
002460         05  作１－保険者番号              PIC X(8).
002470         05  作１－記号                    PIC N(10).
002480         05  作１－番号                    PIC X(16).
002490         05  作１－老人市町村番号          PIC X(8).
002500         05  作１－老人受給者番号          PIC X(16).
002510         05  作１－助成負担者番号          PIC X(8).
002520         05  作１－助成受給者番号          PIC X(16).
002530         05  作１－被保険者氏名            PIC N(10).
002540         05  作１－被保険者カナ            PIC X(16).
002550         05  作１－患者氏名                PIC N(10).
002560         05  作１－患者カナ                PIC X(16).
002570*/仕様変更、仕様違い？半角英数に変更0304
002580*         05  作１－続柄                    PIC N(5).
002590         05  作１－続柄                    PIC X(1).
002600         05  作１－性別                    PIC X(1).
002610         05  作１－患者生年月日.
002620             07  作１－患者生年            PIC X(4).
002630             07  作１－患者生区切１        PIC X(1).
002640             07  作１－患者生月            PIC X(2).
002650             07  作１－患者生区切２        PIC X(1).
002660             07  作１－患者生日            PIC X(2).
002670         05  作１－患者住所                PIC N(14).
002680         05  作１－合計金額                PIC 9(6).
002690         05  作１－負担割合                PIC 9(2).
002700         05  作１－一部負担金              PIC 9(6).
002710         05  作１－請求金額                PIC 9(6).
002720         05  作１－部位数                  PIC 9.
002730         05  作１－負傷原因                PIC N(10).
002740         05  作１－長期理由                PIC N(16).
002750*
002760         05  作１－負傷部位データ  OCCURS 4.
002770             07  作１－負傷名              PIC N(16).
002780             07  作１－負傷年月日.
002790                 09  作１－負傷年          PIC X(4).
002800                 09  作１－負傷区切１      PIC X(1).
002810                 09  作１－負傷月          PIC X(2).
002820                 09  作１－負傷区切２      PIC X(1).
002830                 09  作１－負傷日          PIC X(2).
002840             07  作１－初検年月日.
002850                 09  作１－初検年          PIC X(4).
002860                 09  作１－初検区切１      PIC X(1).
002870                 09  作１－初検月          PIC X(2).
002880                 09  作１－初検区切２      PIC X(1).
002890                 09  作１－初検日          PIC X(2).
002900             07  作１－施術開始年月日.
002910                 09  作１－施術開始年      PIC X(4).
002920                 09  作１－施術開始区切１  PIC X(1).
002930                 09  作１－施術開始月      PIC X(2).
002940                 09  作１－施術開始区切２  PIC X(1).
002950                 09  作１－施術開始日      PIC X(2).
002960             07  作１－施術終了年月日.
002970                 09  作１－施術終了年      PIC X(4).
002980                 09  作１－施術終了区切１  PIC X(1).
002990                 09  作１－施術終了月      PIC X(2).
003000                 09  作１－施術終了区切２  PIC X(1).
003010                 09  作１－施術終了日      PIC X(2).
003020             07  作１－実日数              PIC 9(2).
003030             07  作１－転帰区分            PIC 9.
003040         05  作１－患者番号                PIC 9(6).
003050* 2007/10/24 追加仕様対応
003060         05  作１－年齢区分                PIC X.
003070         05  作１－助成種別                PIC X(3).
003080         05  作１－市区町村名              PIC N(20).
003090         05  作１－市区町村使用区分        PIC X.
003100         05  FILLER                        PIC X(30).
003110**
003120 FD  作業ファイル２ RECORD  CONTAINS 256 CHARACTERS.
003130 01  作２－レコード.
003140*   / ヘッダ部は使用しない /
003150     03  作２－レコードヘッダ.
003160         05  作２－保険区分キー            PIC 9.
003170         05  作２－保険者番号キー          PIC 9(8).
003180     03  作２－レコードデータ.
003190         05  作２－保険者番号              PIC X(8).
003200         05  作２－保険者名称              PIC N(30).
003210         05  作２－保険者宛名              PIC N(30).
003220         05  作２－郵便番号                PIC X(8).
003230         05  作２－保険者住所              PIC N(30).
003240         05  作２－保険種別                PIC N(4).
003250         05  作２－電話番号                PIC X(12).
003260         05  FILLER                        PIC X(31).
003420***
003430* 保険者番号順ファイル
003440 FD  作業ファイル４ RECORD  CONTAINS 64 CHARACTERS.
003450 01  作４－レコード.
003460     03  作４－レコードキー.
003470         05  作４－請求和暦年月.
003480             07  作４－請求和暦            PIC 9.
003490             07  作４－請求年              PIC 9(2).
003500             07  作４－請求月              PIC 9(2).
003510         05  作４－保険種別                PIC 9(2).
003520         05  作４－保険者番号              PIC 9(8).
003530         05  作４－施術和暦年月.
003540             07  作４－施術和暦            PIC 9.
003550             07  作４－施術年              PIC 9(2).
003560             07  作４－施術月              PIC 9(2).
003570         05  作４－被保険者カナ            PIC X(20).
003580         05  作４－患者コード.
003590             07 作４－患者番号             PIC 9(6).
003600             07 作４－枝番                 PIC X(1).
003610         05  作４－助成区分                PIC 9.
003620     03  作４－レコードデータ.
003630         05  FILLER                        PIC X(16).
003640*
003650*----------------------------------------------------------------*
003660******************************************************************
003670*                WORKING-STORAGE SECTION                         *
003680******************************************************************
003690 WORKING-STORAGE         SECTION.
003700 01 キー入力                           PIC X    VALUE SPACE.
003710 01 状態キー                           PIC X(2) VALUE SPACE.
003720 01 初検フラグ                         PIC X(3) VALUE SPACE.
003730 01 終了フラグ                         PIC X(3) VALUE SPACE.
003740 01 終了フラグ２                       PIC X(3) VALUE SPACE.
003750 01 終了フラグ３                       PIC X(3) VALUE SPACE.
003760 01 実行キーＷ                         PIC X(3)  VALUE SPACE.
003770 01 施術記録有Ｗ                       PIC X(3) VALUE SPACE.
003780 01 ファイル名                         PIC N(8) VALUE SPACE.
003790*
003800 01 保険種別ＷＲ                       PIC 9(2) VALUE ZERO.
003810 01 患者コードＷＲ.
003820    03 患者番号ＷＲ                    PIC 9(6) VALUE ZERO.
003830    03 枝番ＷＲ                        PIC X    VALUE SPACE.
003840*
003850 01 印刷形式ＷＲ                       PIC 9    VALUE ZERO.
003860 01 保険者番号ＷＲ                     PIC X(10) VALUE SPACE.
003870 01 市町村番号老人ＷＲ                 PIC X(10) VALUE SPACE.
003880 01 市町村番号助成ＷＲ                 PIC X(10) VALUE SPACE.
003890 01 保険者名称Ｗ.
003900    03 保険者名称ＷＲ                  PIC N(30) VALUE SPACE.
003910 01 市町村名称Ｗ.
003920    03 市町村名称ＷＲ                  PIC N(30) VALUE SPACE.
003930 01 レセプト種類ＷＲ                   PIC X(4) VALUE SPACE.
003940 01 本人家族区分ＷＲ                   PIC 9    VALUE ZERO.
003950 01 続柄Ｗ                             PIC N(2) VALUE SPACE.
003960 01 被保険者氏名Ｗ.
003970    03 被保険者氏名ＷＲ                PIC N(10) VALUE SPACE.
003960 01 患者氏名Ｗ.
003970    03 患者氏名ＷＲ                    PIC N(10) VALUE SPACE.
003960 01 患者住所Ｗ.
003970    03 患者住所１Ｗ                    PIC N(14) VALUE SPACE.
003980 01 患者年齢Ｗ                         PIC 9(3)  VALUE ZERO.
003990 01 患者西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
004000 01 施術西暦年Ｗ                       PIC 9(4)  VALUE ZERO.
004010*/最後の桁の全角半角チェック/050222
004020 01 漢字チェックＴＢＬ.
004030    03 漢字チェックＷ                  PIC X(1)  OCCURS 28.
004040    03 商Ｗ                            PIC 9(3) VALUE ZERO.
004050    03 余Ｗ                            PIC 9(3) VALUE ZERO.
004060*
004070 01 施術和暦年月ＷＲ.
004080    03 施術和暦ＷＲ                    PIC 9    VALUE ZERO.
004090    03 施術年ＷＲ                      PIC 9(2) VALUE ZERO.
004100    03 施術月ＷＲ                      PIC 9(2) VALUE ZERO.
004110 01 請求和暦年月ＷＲ.
004120    03 請求和暦ＷＲ                    PIC 9    VALUE ZERO.
004130    03 請求年ＷＲ                      PIC 9(2) VALUE ZERO.
004140    03 請求月ＷＲ                      PIC 9(2) VALUE ZERO.
004150**
004160 01 連番Ｗ                             PIC 9(4) VALUE ZERO.
004170 01 助成フラグ                         PIC X(3) VALUE SPACE.
004180 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
004190 01 転帰変換前Ｗ                       PIC 9     VALUE ZERO.
004200 01 転帰変換後Ｗ                       PIC 9     VALUE ZERO.
004210**
004220 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
004230 01 カウンタ                           PIC 9(2)  VALUE ZERO.
004240 01 カウンタ２                         PIC 9(3)  VALUE ZERO.
004250 01 カウンタ３                         PIC 9(2)  VALUE ZERO.
004260 01 全角空白                           PIC X(2)  VALUE X"8140".
004270 01 半角空白                           PIC X(2)  VALUE X"2020".
004280**
004290** エラーメッセージ用
004300 01 エラーメッセージＷ.
004310    03 エラー患者コードＷ              PIC X(7) VALUE SPACE.
004320    03 エラー区切りＷ                  PIC X(1) VALUE SPACE.
004330    03 エラー保険種別Ｗ                PIC X(2) VALUE SPACE.
004340    03 FILLER                          PIC X(10) VALUE SPACE.
004350** 保険者番号右詰め用
004360 01 保険者番号ＷＴ.
004370    03 保険者番号左詰めＷ.
004380      05 保険者番号左詰めＷ１          PIC X OCCURS 8 VALUE SPACE.
004390    03 保険者番号右詰めＷ.
004400      05 保険者番号右詰めＷ１          PIC X OCCURS 8 VALUE ZERO.
004410    03 保険者番号数字Ｗ                PIC 9(8)  VALUE ZERO.
004420    03 保険者番号Ｗ                    PIC X(8)  VALUE SPACE.
004430** 会員番号右詰め用
004440 01 会員番号ＷＴ.
004450    03 会員番号左詰めＷ.
004460      05 会員番号左詰めＷ１            PIC X OCCURS 7 VALUE SPACE.
004470    03 会員番号右詰めＷ.
004480      05 会員番号右詰めＷ１            PIC X OCCURS 7 VALUE ZERO.
004490    03 会員番号数字Ｗ                  PIC 9(7)  VALUE ZERO.
004500    03 会員番号Ｗ                      PIC X(7)  VALUE SPACE.
004510** 西暦日付ワーク用
004520 01 西暦年月Ｗ.
004530    03 西暦年Ｗ                        PIC 9(4) VALUE ZERO.
004540    03 西暦月Ｗ                        PIC 9(2) VALUE ZERO.
004550** 西暦請求年月用
004560 01 西暦請求年月Ｗ.
004570    03 西暦請求年Ｗ                    PIC 9(4) VALUE ZERO.
004580    03 西暦請求月Ｗ                    PIC 9(2) VALUE ZERO.
004590** 西暦施術年月用
004600 01 西暦施術年月Ｗ.
004610    03 西暦施術年Ｗ                    PIC 9(4) VALUE ZERO.
004620    03 西暦施術月Ｗ                    PIC 9(2) VALUE ZERO.
004630** 記号左詰め用
004640 01 記号ＷＴ.
004650    03 記号元Ｗ.
004660      05 記号元Ｗ１                    PIC N OCCURS 12 VALUE SPACE.
004670    03 記号左詰めＷ.
004680      05 記号左詰めＷ１                PIC N OCCURS 12 VALUE SPACE.
004690    03 記号Ｗ.
004700      05 記号ＮＷ                      PIC N(12) VALUE SPACE.
004710    03 記号ＰＷ.
004720      05 記号ＰＮＷ                    PIC X(24) VALUE SPACE.
004690    03 記号ＷＲ.
004700      05 記号ＮＷＲ                    PIC N(12) VALUE SPACE.
      *番号
       01 番号ＷＲ                           PIC X(30) VALUE SPACE.
004730** 助成負担者番号左詰め用
004740 01 助成番号ＷＴ.
004750    03 助成番号元Ｗ.
004760      05 助成番号元Ｗ１                PIC X OCCURS 10 VALUE SPACE.
004770    03 助成番号左詰めＷ.
004780      05 助成番号左詰めＷ１            PIC X OCCURS 10 VALUE SPACE.
004790    03 助成番号Ｗ                      PIC X(10) VALUE SPACE.
004800*
004810** 西暦年月日ワーク用
004820 01 計算西暦年月日Ｗ.
004830    03 計算西暦年Ｗ                    PIC 9(4) VALUE ZERO.
004840    03 計算西暦月Ｗ                    PIC 9(2) VALUE ZERO.
004850    03 計算西暦日Ｗ                    PIC 9(2) VALUE ZERO.
004860 01 計算和暦年月日Ｗ.
004870    03 計算和暦Ｗ                      PIC 9 VALUE ZERO.
004880    03 計算年Ｗ                        PIC 9(2) VALUE ZERO.
004890    03 計算月Ｗ                        PIC 9(2) VALUE ZERO.
004900    03 計算日Ｗ                        PIC 9(2) VALUE ZERO.
004910** 枝番判定用
004920 01 開始診療日手動区分Ｗ               PIC 9    VALUE ZERO.
004930*
004940* 終了日退避用
004950 01 終了年月日ＷＴ.
004960    03 終了和暦ＷＴ                    PIC 9     VALUE ZERO.
004970    03 終了年ＷＴ                      PIC 9(2)  VALUE ZERO.
004980    03 終了月ＷＴ                      PIC 9(2)  VALUE ZERO.
004990    03 終了日ＷＴ                      PIC 9(2)  VALUE ZERO.
005000* 初検日退避用
005010 01 初検年月日ＷＴ.
005020    03 初検和暦ＷＴ                    PIC 9     VALUE ZERO.
005030    03 初検年ＷＴ                      PIC 9(2)  VALUE ZERO.
005040    03 初検月ＷＴ                      PIC 9(2)  VALUE ZERO.
005050    03 初検日ＷＴ                      PIC 9(2)  VALUE ZERO.
005060*
005070* 連計の金額退避用
005080 01 連計金額Ｗ.
005090    03  費用額Ｗ                   PIC 9(6) VALUE ZERO.
005100    03  負担額Ｗ                   PIC 9(6) VALUE ZERO.
005110    03  請求額Ｗ                   PIC 9(6) VALUE ZERO.
005120    03  費用額老人Ｗ               PIC 9(6) VALUE ZERO.
005130    03  負担額老人Ｗ               PIC 9(6) VALUE ZERO.
005140    03  請求額老人Ｗ               PIC 9(6) VALUE ZERO.
005150    03  費用額助成Ｗ               PIC 9(6) VALUE ZERO.
005160    03  負担額助成Ｗ               PIC 9(5) VALUE ZERO.
005170    03  請求額助成Ｗ               PIC 9(5) VALUE ZERO.
005180    03  負担率Ｗ                   PIC 9(3) VALUE ZERO.
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
03620    03 負傷原因内容分解ＸＷ.
003630       05 負傷原因内容１ＸＷ           PIC N(37)  VALUE SPACE.
003640       05 負傷原因内容２ＸＷ           PIC N(37)  VALUE SPACE.
003640       05 負傷原因内容３ＸＷ           PIC N(37)  VALUE SPACE.
003650       05 負傷原因内容４ＸＷ           PIC N(37)  VALUE SPACE.
003650       05 負傷原因内容５ＸＷ           PIC N(11)  VALUE SPACE.
005430*
005440** 負傷原因・長期理由印刷区分用
005450 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
005460 01 長期理由印刷区分Ｗ                 PIC 9 VALUE ZERO.
005470*
005480**********************************************************************************
005490*
005500 01 退避項目ＧＷ.
005510   03 レセプト種類Ｗ                   PIC X(4).
005520   03 レセプト種類ＧＷ                 PIC X(4).
005530   03 レセプト種別ＧＷ                 PIC 9(2).
005540*
005550****************
005560* 負傷データＦ *
005570****************
005580 01 負傷情報Ｗ.
005590    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
005600    03 部位情報Ｗ  OCCURS   9.
005610       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
005620       05 部位コードＷ.
005630          07 負傷種別Ｗ                PIC 9(2)  VALUE ZERO.
005640          07 部位Ｗ                    PIC 9(2)  VALUE ZERO.
005650          07 左右区分Ｗ                PIC 9(1)  VALUE ZERO.
005660          07 負傷位置番号Ｗ            PIC 9(2)  VALUE ZERO.
005670       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
005680       05 負傷年月日Ｗ.
005690          07 負傷和暦Ｗ                PIC 9     VALUE ZERO.
005700          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
005710          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
005720          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
005730       05 初検年月日Ｗ.
005740          07 初検和暦Ｗ                PIC 9     VALUE ZERO.
005750          07 初検年Ｗ                  PIC 9(2)  VALUE ZERO.
005760          07 初検月Ｗ                  PIC 9(2)  VALUE ZERO.
005770          07 初検日Ｗ                  PIC 9(2)  VALUE ZERO.
005780       05 開始年月日Ｗ.
005790          07 開始和暦Ｗ                PIC 9     VALUE ZERO.
005800          07 開始年Ｗ                  PIC 9(2)  VALUE ZERO.
005810          07 開始月Ｗ                  PIC 9(2)  VALUE ZERO.
005820          07 開始日Ｗ                  PIC 9(2)  VALUE ZERO.
005830       05 終了年月日Ｗ.
005840          07 終了和暦Ｗ                PIC 9     VALUE ZERO.
005850          07 終了年Ｗ                  PIC 9(2)  VALUE ZERO.
005860          07 終了月Ｗ                  PIC 9(2)  VALUE ZERO.
005870          07 終了日Ｗ                  PIC 9(2)  VALUE ZERO.
005880       05 実日数Ｗ                     PIC 9(2)  VALUE ZERO.
005890       05 転帰区分Ｗ                   PIC 9(1)  VALUE ZERO.
005900    03 負傷原因ＷＲ.
             05 負傷原因Ｗ                   PIC N(37) OCCURS 27 VALUE SPACE.
005910*
005920*********************************************************************
005930*    ************
005940*    * 料金情報 *
005950*    ************
005960*    月毎の料金
005970***********************
005980 01 料金１ＷＲ.
005990   03 一部負担金ＷＲ                PIC 9(6)    VALUE ZERO.
006000   03 請求金額ＷＲ                  PIC 9(6)    VALUE ZERO.
006010   03 給付割合ＷＲ                  PIC 9(1)    VALUE ZERO.
006020   03 受給者負担額ＷＲ              PIC 9(6)    VALUE ZERO.
006030   03 助成請求金額ＷＲ              PIC 9(6)    VALUE ZERO.
006040*
006050*****************************************************************
006060 01 計算機西暦年Ｗ                     PIC 9(2).
006070* 日付ＷＯＲＫ
006080 01 計算機西暦.
006090    03 計算機西暦年                    PIC 9(4).
006100    03 計算機西暦月日                  PIC 9(4).
006110 01 計算機西暦Ｒ REDEFINES 計算機西暦.
006120    03 計算機世紀                      PIC 9(2).
006130    03 計算機日付                      PIC 9(6).
006140    03 計算機日付Ｒ REDEFINES 計算機日付.
006150       05 計算機年月                   PIC 9(4).
006160       05 計算機年月Ｒ REDEFINES 計算機年月.
006170         07 計算機年                   PIC 9(2).
006180         07 計算機月                   PIC 9(2).
006190       05 計算機日                     PIC 9(2).
006200*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
006210******************************************************************
006220*                          連結項目                              *
006230******************************************************************
006240*
006250********************
006260* メッセージ表示キー *
006270********************
006280 01 連メ－キー IS EXTERNAL.
006290    03  連メ－メッセージ               PIC N(20).
006300*
006310 01 連メ３－キー IS EXTERNAL.
006320    03  連メ３－メッセージ             PIC N(20).
006330    03  連メ３－メッセージ１           PIC X(20).
006340*
006350****************
006360* 画面入力情報 *
006370****************
002700 01 連入－画面情報ＹＣＢ５８０ IS EXTERNAL.
002710    03 連入－施術和暦年月.
002720       05 連入－施術和暦                PIC 9.
002730       05 連入－施術年月.
002740         07 連入－施術年                PIC 9(2).
002750         07 連入－施術月                PIC 9(2).
          03 連入－プレビュー区分            PIC 9.
007150*
007160 01 連レ－キー IS EXTERNAL.
007170    03 連レ－保険種別                  PIC 9(2).
007180*
007190************************
007200* 長期理由文セット     *
007210************************
007220 01 連長文－キー IS EXTERNAL.
007230    03 連長文－施術年月.
007240       05 連長文－施術和暦               PIC 9.
007250       05 連長文－施術年                 PIC 9(2).
007260       05 連長文－施術月                 PIC 9(2).
007270    03  連長文－患者コード.
007280       05 連長文－患者番号               PIC 9(6).
007290       05 連長文－枝番                   PIC X.
007300    03 連長文－文桁数                    PIC 9(2).
007310    03 連長文－理由文                    PIC N(63) OCCURS 15.
007321*
007322************************
007323* 年齢用               *
007325************************
007326 01 連年齢－キー IS EXTERNAL.
007327    03 連年齢－施術和暦年月.
007328       05 連年齢－施術和暦      PIC 9.
007329       05 連年齢－施術年月.
007330         07 連年齢－施術年      PIC 9(2).
007331         07 連年齢－施術月      PIC 9(2).
007332    03 連年齢－和暦年月日.
007333       05 連年齢－和暦          PIC 9.
007334       05 連年齢－年            PIC 9(2).
007335       05 連年齢－月            PIC 9(2).
007336       05 連年齢－日            PIC 9(2).
007337    03 連年齢－年齢             PIC 9(3).
007338*
      */負担率取得ＰＧの対応1410
       01 連率－負担率取得キー IS EXTERNAL.
          03 連率－施術和暦年月.
             05 連率－施術和暦               PIC 9.
             05 連率－施術年月.
                07 連率－施術年              PIC 9(2).
                07 連率－施術月              PIC 9(2).
          03 連率－患者コード.
             05 連率－患者番号               PIC 9(6).
             05 連率－枝番                   PIC X.
          03 連率－実際負担率                PIC 9(3).
          03 連率－実際本体負担率            PIC 9(3).
          03 連率－健保負担率                PIC 9(3).
          03 連率－２７老負担率              PIC 9(3).
          03 連率－助成負担率                PIC 9(3).
          03 連率－特別用負担率              PIC 9(3).
006490*
      * 暗号複合用
       01 連暗号複合－暗号情報 IS EXTERNAL.
          03 連暗号複合－入力情報.
             05 連暗号複合－記号               PIC X(24).
             05 連暗号複合－番号               PIC X(30).
             05 連暗号複合－暗号化項目.
               07 連暗号複合－暗号患者番号     PIC X(6).
               07 連暗号複合－暗号判定記号     PIC X.
               07 連暗号複合－暗号判定番号     PIC X.
               07 連暗号複合－暗号記号         PIC X(24).
               07 連暗号複合－暗号番号         PIC X(30).
          03 連暗号複合－出力情報.
             05 連暗号複合－複合した記号       PIC X(24).
             05 連暗号複合－複合した番号       PIC X(30).
      * 
007339******************************************************************
007340*                      PROCEDURE  DIVISION                       *
007350******************************************************************
007360 PROCEDURE               DIVISION.
007370************
007380*           *
007390* 初期処理   *
007400*           *
007410************
007420     PERFORM 初期化.
007430     PERFORM 制御情報取得.
007440     PERFORM 施術所情報取得.
007450************
007460*           *
007470* 主処理     *
007480*           *
007490************
007500     PERFORM 作業ファイル作成.
007510************
007520*           *
007530* 終了処理   *
007540*           *
007550************
007560     PERFORM 終了処理.
007570     MOVE ZERO TO PROGRAM-STATUS.
007580     EXIT PROGRAM.
007590*
007600*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
007610*================================================================*
007620 初期化 SECTION.
007630*
007640     PERFORM ファイルオープン.
007650* 連結項目の待避
007660     MOVE 連入－施術和暦  TO 請求和暦ＷＲ.
007670     MOVE 連入－施術年    TO 請求年ＷＲ.
007680     MOVE 連入－施術月    TO 請求月ＷＲ.
007690*
007700* 西暦請求年月の取得
007710     MOVE ZERO          TO 西暦年月Ｗ  西暦請求年月Ｗ.
007720     MOVE 請求和暦ＷＲ  TO 元－元号区分.
007730     READ 元号マスタ
007740     NOT INVALID KEY
007750         MOVE 元－開始西暦年 TO 西暦年Ｗ
007760     END-READ.
007770*
007780     IF 西暦年Ｗ = ZERO
007790          MOVE  NC"元号マスタに開始西暦年を登録して下さい" TO 連メ－メッセージ
007800          CALL   "MSG001"
007810          CANCEL "MSG001"
007820          PERFORM ファイル閉鎖
007830          MOVE 99 TO PROGRAM-STATUS
007840          EXIT PROGRAM
007850     ELSE
007860          COMPUTE 西暦年Ｗ = 西暦年Ｗ + 請求年ＷＲ - 1
007870          MOVE 請求月ＷＲ TO 西暦月Ｗ
007880     END-IF.
007890*
007900     MOVE 西暦年月Ｗ   TO  西暦請求年月Ｗ.
007910*
007920*================================================================*
007930 ファイルオープン SECTION.
007940*
007950     OPEN INPUT 制御情報マスタ.
007960         MOVE NC"制御情報" TO ファイル名.
007970         PERFORM オープンチェック.
007980     OPEN INPUT 元号マスタ.
007990         MOVE NC"元号マスタ" TO ファイル名.
008000         PERFORM オープンチェック.
008010     OPEN INPUT 名称マスタ.
008020         MOVE NC"名称マスタ" TO ファイル名.
008030         PERFORM オープンチェック.
008040     OPEN INPUT 施術所情報マスタ
008050         MOVE NC"施情" TO ファイル名.
008060         PERFORM オープンチェック.
008070     OPEN INPUT 施術記録Ｆ.
008080         MOVE NC"施術記録Ｆ" TO ファイル名.
008090         PERFORM オープンチェック.
008100     OPEN INPUT 保険者マスタ.
008110         MOVE NC"保険者マスタ" TO ファイル名.
008120         PERFORM オープンチェック.
008130     OPEN INPUT 受診者情報Ｆ.
008140         MOVE NC"受診者情報Ｆ" TO ファイル名.
008150         PERFORM オープンチェック.
007560     OPEN INPUT   レセプトＦ
007570         MOVE NC"レセ" TO ファイル名.
007580         PERFORM オープンチェック.
008160     OPEN INPUT 負傷データＦ.
008170         MOVE NC"負傷データＦ" TO ファイル名.
008180         PERFORM オープンチェック.
008190     OPEN INPUT 負傷原因Ｆ.
008200         MOVE NC"負傷原因" TO ファイル名.
008210         PERFORM オープンチェック.
008220     OPEN INPUT 市町村マスタ
008230         MOVE NC"市町村" TO ファイル名.
008240         PERFORM オープンチェック.
008250     OPEN INPUT   請求先マスタ
008260         MOVE NC"請先" TO ファイル名.
008270         PERFORM オープンチェック.
008310*
008320*================================================================*
008330 オープンチェック SECTION.
008340*
008350     IF 状態キー  NOT =  "00"
008360         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
008370         DISPLAY NC"状態キー：" 状態キー         UPON CONS
008380         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008390                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
008400         ACCEPT  キー入力 FROM CONS
008410         PERFORM ファイル閉鎖
008420         MOVE 99 TO PROGRAM-STATUS
008430         EXIT PROGRAM.
008440*================================================================*
008450 ファイル閉鎖 SECTION.
008460*
008470     CLOSE 制御情報マスタ 元号マスタ 名称マスタ 受診者情報Ｆ レセプトＦ
008480           負傷データＦ   負傷原因Ｆ 施術記録Ｆ 施術所情報マスタ
008490           市町村マスタ   保険者マスタ 請求先マスタ.
008500*================================================================*
008510 終了処理 SECTION.
008520*
008530     PERFORM ファイル閉鎖.
008540*================================================================*
008550 エラー表示Ｒ SECTION.
008560*
008570     DISPLAY NC"ファイル読込エラー" ファイル名     UPON CONS.
008580     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
008590     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
008600     ACCEPT  キー入力 FROM CONS.
008610     PERFORM ファイル閉鎖.
008620     MOVE 99 TO PROGRAM-STATUS.
008630     EXIT PROGRAM.
008640*================================================================*
008650 エラー表示 SECTION.
008660*
008670     DISPLAY NC"状態キー" 状態キー  UPON CONS.
008680     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
008690     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
008700     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
003321*-----------------------------------------*
003322     CALL "actcshm"  WITH C LINKAGE.
003323*-----------------------------------------*
008710     ACCEPT  キー入力 FROM CONS.
008720     PERFORM ファイル閉鎖.
008730     MOVE 99 TO PROGRAM-STATUS.
008740     EXIT PROGRAM.
008750*================================================================*
008760 制御情報取得 SECTION.
008770*
008780     MOVE ZEROS TO 制－制御区分.
008790     READ 制御情報マスタ
008800     NOT INVALID KEY
008810         MOVE 制－レセ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
008820         MOVE 制－レセ長期理由印刷区分 TO 長期理由印刷区分Ｗ
008830     END-READ.
008840*
008850*================================================================*
008860 施術所情報取得 SECTION.
008870*
008880     MOVE ZERO  TO 施情－施術所番号.
008890     READ 施術所情報マスタ
008900     INVALID KEY
008910          MOVE  NC"施術所情報マスタに登録後、実行して下さい" TO 連メ－メッセージ
008920          CALL   "MSG001"
008930          CANCEL "MSG001"
008940          PERFORM ファイル閉鎖
008950          MOVE 99 TO PROGRAM-STATUS
008960          EXIT PROGRAM
008970     NOT INVALID KEY
008980          IF  施情－接骨師会会員番号 = SPACE
008990              MOVE  NC"施術所マスタに会員番号を登録して下さい" TO 連メ－メッセージ
009000              CALL   "MSG001"
009010              CANCEL "MSG001"
009020              PERFORM ファイル閉鎖
009030              MOVE 99 TO PROGRAM-STATUS
009040              EXIT PROGRAM
009050          ELSE
009060              MOVE 施情－接骨師会会員番号  TO 会員番号Ｗ
009070              PERFORM 会員番号右詰め
009080          END-IF
009090     END-READ.
009100*
009110*================================================================*
009120 作業ファイル作成 SECTION.
009130*
009140     PERFORM 保険者番号順ファイル作成.
009150*
009160     OPEN OUTPUT 作業ファイル１.
009170         MOVE NC"作１" TO ファイル名.
009180         PERFORM オープンチェック.
009190     OPEN OUTPUT 作業ファイル２.
009200         MOVE NC"作２" TO ファイル名.
009210         PERFORM オープンチェック.
009250     OPEN INPUT  作業ファイル４.
009260         MOVE NC"作４" TO ファイル名.
009270         PERFORM オープンチェック.
009280*
009290     PERFORM 作業ファイル１作成.
009300*
009310     CLOSE 作業ファイル４ 作業ファイル１ 作業ファイル２.
009320*
009330*================================================================*
009340 保険者番号順ファイル作成 SECTION.
009350**********************************************************************
009360**   受診者情報Ｆから、該当請求年月のデータを抽出し、
009370**   作業ファイル４(保険者番号順)に書き出す.
009380**********************************************************************
009390*
009400     OPEN OUTPUT 作業ファイル４.
009410         MOVE NC"作４" TO ファイル名.
009420         PERFORM オープンチェック.
009430*
004760     MOVE 請求和暦ＷＲ  TO レセ－請求和暦.
004770     MOVE 請求年ＷＲ    TO レセ－請求年.
004780     MOVE 請求月ＷＲ    TO レセ－請求月.
004790     MOVE ZERO          TO レセ－施術和暦.
004800     MOVE ZERO          TO レセ－施術年.
004810     MOVE ZERO          TO レセ－施術月.
004820     MOVE ZERO          TO レセ－患者番号.
004830     MOVE LOW-VALUE     TO レセ－枝番.
013730     MOVE ZERO          TO レセ－レセ種別.
004840     START レセプトＦ   KEY IS >= レセ－請求和暦年月
004850                                  レセ－施術和暦年月
004860                                  レセ－患者コード
000250                                  レセ－レセ種別
009550     END-START.
009560     IF 状態キー = "00"
009570         MOVE SPACE  TO 終了フラグ
009580         PERFORM レセプトＦ読込
009590         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
004920                       ( レセ－請求和暦 NOT = 請求和暦ＷＲ ) OR
004930                       ( レセ－請求年   NOT = 請求年ＷＲ   ) OR
004940                       ( レセ－請求月   NOT = 請求月ＷＲ   )
002990             PERFORM データチェック
003010             IF 実行キーＷ = "YES"
009630*
009640                 MOVE SPACE TO 作４－レコード
009650                 INITIALIZE    作４－レコード
009660                 MOVE レセ－請求和暦   TO  作４－請求和暦
009670                 MOVE レセ－請求年     TO  作４－請求年
009680                 MOVE レセ－請求月     TO  作４－請求月
009690                 MOVE レセ－施術和暦   TO  作４－施術和暦
009700                 MOVE レセ－施術年     TO  作４－施術年
009710                 MOVE レセ－施術月     TO  作４－施術月
009720                 IF  (受－助成種別   = ZERO  OR 50) OR
                           (レセ－レセ種別 = 7)
009730                     IF ( 受－保険種別   NOT = ZERO ) AND
009740                        ( 受－保険者番号 NOT = SPACE )
009750** 助成なし(生保親ありは助成なし扱い)
009760                        MOVE ZERO  TO   作４－助成区分
009770                     ELSE
009780** 生保単独
009790                        MOVE 2     TO   作４－助成区分
009800                     END-IF
009810                 ELSE
009820** 助成あり
009830                     MOVE 1        TO   作４－助成区分
009840                 END-IF
009850*
009860** 15/06 その他60は本体のみ
009870                 IF  受－助成種別  = 60
      */愛知県高齢福祉給付金(マル福)の対応/080416
                           IF (受－費用負担者番号助成(1:4) NOT = "8923")
009880                         MOVE ZERO  TO   作４－助成区分
009890                     END-IF
009890                 END-IF
009900*
009910                 MOVE 受－被保険者カナ   TO 作４－被保険者カナ
009920                 MOVE 受－患者コード     TO 作４－患者コード
009930*
009940                 IF ( 受－公費種別       = ZERO  ) AND
009950                    ( 受－費用負担者番号 = SPACE )
009960                     MOVE 受－保険種別       TO 作４－保険種別
009970                     IF 作４－保険種別 = 06 OR 07
009980                         MOVE 02             TO 作４－保険種別
009990                     END-IF
010000                     MOVE 受－保険者番号     TO 作４－保険者番号
010010                 ELSE
010020* 老人は、市町村番号
010030                     MOVE 受－公費種別       TO 作４－保険種別
010040                     MOVE 受－費用負担者番号 TO 作４－保険者番号
010050                 END-IF
010060                 IF ( レセ－レセ種別  = 3 ) 
010120                     MOVE 受－助成種別       TO 作４－保険種別
010130                     MOVE 受－費用負担者番号助成 TO 作４－保険者番号
010140                 END-IF
010150*
010160                 WRITE 作４－レコード
010170                 INVALID KEY
010180                     MOVE NC"作４"  TO ファイル名
010190                 PERFORM エラー表示
010200                 END-WRITE
010210*
                   END-IF
010220             PERFORM レセプトＦ読込
010230         END-PERFORM
010240     END-IF.
010250*
010260     CLOSE 作業ファイル４.
010270*
010280*================================================================*
010290 作業ファイル１作成 SECTION.
010300*
010310     MOVE SPACE  TO 終了フラグ.
010320     MOVE SPACE  TO 保険者番号ＷＲ.
010330     MOVE SPACE  TO 市町村番号老人ＷＲ.
010340     MOVE SPACE  TO 市町村番号助成ＷＲ.
010350     PERFORM 作業ファイル４読込.
010360     PERFORM UNTIL  終了フラグ = "YES" 
010370        MOVE SPACE TO 助成フラグ
010380        PERFORM データチェック
010390*
010400** 労災・自賠責・自由は対象外
010410        IF  受－保険種別 = 70 OR 80 OR 90
010420            MOVE SPACE  TO 実行キーＷ
010430        END-IF
010440** 生保単独は対象外
010450        IF  作４－助成区分  = 2
010460            MOVE SPACE  TO 実行キーＷ
010470        END-IF
010480*
010490** 資格証明は対象外
010500        IF  ( 受－保険種別 = 01 OR 08 ) AND
010510            ( 受－公費種別 = ZERO     ) AND
010520            ( 受－資格証明区分 = 1 )
010530            MOVE SPACE  TO 実行キーＷ
010540        END-IF
010550**
010490** 返戻分は対象外
010500        IF  ( レセ－請求区分 = 2 )
010530            MOVE SPACE  TO 実行キーＷ
010540        END-IF
010550**
010560        IF  実行キーＷ = "YES"
010600*
010610*        ********
010620*        * 健保 *
010630*        ********
010640             IF ( レセ－レセ種別  = 1 )
010660*            **********************
010670*            * 作業ファイル作成 *
010680*            **********************
010710                 IF 作４－助成区分  = ZERO
010720*   / 助成なし /
010730                     MOVE SPACE TO 助成フラグ
010740                 ELSE
010750*   / 助成あり /
010760                     MOVE "YES" TO 助成フラグ
010770                 END-IF
010850                 PERFORM 作１レコードセット健保
010860                 PERFORM 作１ファイル書込
010880                 PERFORM 作２レコードセット
010900                 MOVE 受－保険者番号  TO 保険者番号ＷＲ
010920             END-IF
010930*        ********
010940*        * 老人 *
010950*        ********
010640             IF ( レセ－レセ種別  = 2 )
010980*            **********************
010990*            * 作業ファイル作成 *
011000*            **********************
011010                 IF 作４－助成区分  = ZERO
011020*   / 助成なし /
011030                      MOVE SPACE TO 助成フラグ
011040                 ELSE
011050*   / 助成あり /
011060                      MOVE "YES" TO 助成フラグ
011070                 END-IF
011140                 PERFORM 作１レコードセット老人
011150                 PERFORM 作１ファイル書込
011170                 PERFORM 作２レコードセット老人
011180             END-IF
010930*        ********
010940*        * 助成 *
010950*        ********
010640             IF ( レセ－レセ種別  = 3 )
010980*            **********************
010990*            * 作業ファイル作成 *
011000*            **********************
                       IF 受－公費種別 = ZERO
010780                     IF ( 作４－保険種別 > 50 )
010790                         PERFORM 作１レコードセット健保助成
010800                         PERFORM 作１ファイル書込
010830                         PERFORM 作２レコードセット助成
010840                     END-IF
                       ELSE
011080                     IF ( 作４－保険種別 > 50 )
011090                         PERFORM 作１レコードセット老人助成
011100                         PERFORM 作１ファイル書込
011120                         PERFORM 作２レコードセット助成
011130                     END-IF
                       END-IF
                   END-IF
               END-IF

      *            ********************
      *            * 名古屋市福祉老人 *
      *            ********************
      */名古屋市福祉分は本体分と別に集計
               PERFORM 名古屋市福祉判定
      *
011210         MOVE 受－費用負担者番号  TO 市町村番号老人ＷＲ
011220         MOVE 受－費用負担者番号助成  TO 市町村番号助成ＷＲ
011230         PERFORM 作業ファイル４読込
011240      END-PERFORM.
011250*
011430*================================================================*
011440 作業ファイル１読込 SECTION.
011450*
011460     READ 作業ファイル１ NEXT
011470     AT END
011480         MOVE "YES" TO 終了フラグ３
011490     END-READ.
011500*================================================================*
011510 作業ファイル４読込 SECTION.
011520*
011530     READ 作業ファイル４ NEXT
011540     AT END
011550         MOVE "YES" TO 終了フラグ
011560     NOT AT END
011570         MOVE 作４－施術和暦    TO レセ－施術和暦
011580         MOVE 作４－施術年      TO レセ－施術年
011590         MOVE 作４－施術月      TO レセ－施術月
011600         MOVE 作４－患者番号    TO レセ－患者番号
011610         MOVE 作４－枝番        TO レセ－枝番
               EVALUATE TRUE
               WHEN (作４－保険種別 < 10) AND (作４－保険種別 NOT = 5)
                   MOVE 1          TO レセ－レセ種別
               WHEN 作４－保険種別 = 5
                   MOVE 2          TO レセ－レセ種別
               WHEN (作４－保険種別 > 50) AND (作４－保険種別 <= 60)
                   MOVE 3          TO レセ－レセ種別
               WHEN 作４－保険種別 = 70
                   MOVE 4          TO レセ－レセ種別
               WHEN 作４－保険種別 = 80
                   MOVE 5          TO レセ－レセ種別
               WHEN 作４－保険種別 = 90
                   MOVE 6          TO レセ－レセ種別
               WHEN 作４－保険種別 = 50 OR 85
                   MOVE 7          TO レセ－レセ種別
               WHEN 作４－保険種別 = 91
                   MOVE 8          TO レセ－レセ種別
               END-EVALUATE
011620         READ レセプトＦ
011630         INVALID KEY
011640              MOVE NC"レセプト"   TO ファイル名
011650              PERFORM エラー表示Ｒ
011660         END-READ
011670     END-READ.
011680*
011690*================================================================*
011700 データチェック SECTION.
011710*
011720     MOVE SPACE  TO 実行キーＷ.
019520* *****************************************************************
019530* * レセプトＦの請求対象区分 = 0 の場合データ作成対象としない *
019540* *****************************************************************
019640     IF ( レセ－請求対象区分 NOT = ZERO ) AND
005778        ( レセ－償還払い区分 NOT = 1 )
              IF (レセ－レセ種別 = 3) AND ( レセ－会総括表印刷対象区分 = 1 )
                  CONTINUE
              ELSE
004090           MOVE レセ－施術和暦  TO 受－施術和暦
004100           MOVE レセ－施術年    TO 受－施術年
004110           MOVE レセ－施術月    TO 受－施術月
004120           MOVE レセ－患者番号  TO 受－患者番号
004130           MOVE レセ－枝番      TO 受－枝番
                 READ 受診者情報Ｆ
                 NOT INVALID KEY
019880              MOVE "YES"  TO 実行キーＷ
019950           END-READ
              END-IF
019960     END-IF.
012180*
012280*================================================================*
012290 レセプトＦ読込 SECTION.
012300*
012310     READ レセプトＦ NEXT
012320     AT END
012330         MOVE "YES" TO 終了フラグ
012340     END-READ.
012350*
012360*================================================================*
012370 施術記録Ｆ読込 SECTION.
012380*
012390     READ 施術記録Ｆ NEXT
012400     AT END
012410         MOVE "YES"  TO 終了フラグ２
012420     END-READ.
012430*================================================================*
012440 作１レコードセット健保 SECTION.
012450*
012460**********/  健保助成なしの時  /**********
012470*
012480     MOVE SPACE TO 作１－レコード.
012490     INITIALIZE 作１－レコード.
012500*
012510     MOVE レセ－合計       TO 作１－合計金額.
012520     MOVE レセ－一部負担金 TO 作１－一部負担金.
012530     MOVE レセ－請求金額   TO 作１－請求金額.
012540*
012550     MOVE 受－保険種別     TO 作１－保険区分キー.
012560     MOVE 11               TO 名－区分コード.
012570     MOVE 受－保険種別     TO 名－名称コード.
012580     READ 名称マスタ
012590     NOT INVALID KEY
012600         MOVE 名－略称     TO 作１－保険種別
012610     END-READ.
012620     IF 作１－保険種別 = NC"退職"
012630         MOVE NC"国保退職" TO 作１－保険種別
012640     END-IF.
012650     IF ((受－保険種別 = 01) AND (受－保険者番号(3:1) = 3)) OR
012660        ((受－保険種別 = 08) AND (受－保番(3:1) = 3))
012670         MOVE NC"国保組合" TO 作１－保険種別
012680     END-IF.
012690* 船員、日雇は社保には含めない 2007/10/22
012700*     IF 作１－保険種別 = NC"船員" OR NC"日雇"
012710*         MOVE NC"社保" TO 作１－保険種別
012720*     END-IF.
012730     IF 作１－保険種別 = NC"組合"
012740         MOVE NC"健保" TO 作１－保険種別
012750     END-IF.
012760     IF 受－保険種別 = 09
012770         MOVE NC"共済" TO 作１－保険種別
012780     END-IF.
012790* 保険者番号(数字)
012800     MOVE 受－保険者番号     TO 作１－保険者番号.
012810
012820* 助成種別
012830     MOVE "0"     TO 作１－助成種別.
012840
012850     MOVE "0"     TO 作１－市区町村使用区分.
012990*
013000* 連計 転記の後
013010     PERFORM 共通レコードセット.
013020*
013030*================================================================*
013040 作１レコードセット老人 SECTION.
013050*
013060**********/ 27老人の時  /**********
013070*
013080     MOVE SPACE TO 作１－レコード.
013090     INITIALIZE 作１－レコード.
013100*
           IF 受－施術和暦年月 < 42004
      */保険者番号は本体の物を転記する/071220
013110*        MOVE 受－費用負担者番号 TO 作１－保険者番号 作１－老人市町村番号 作１－助成負担者番号
              MOVE 受－費用負担者番号 TO 作１－老人市町村番号 作１－助成負担者番号
013130        MOVE 受－受益者番号老人 TO 作１－老人受給者番号 作１－助成受給者番号
013200        MOVE NC"老人"       TO 作１－保険種別
           ELSE
013200        MOVE NC"後期高齢"   TO 作１－保険種別
           END-IF.
013140*
           MOVE 受－保険者番号     TO 作１－保険者番号.
013120*
013150     MOVE レセ－合計       TO 作１－合計金額.
013160     MOVE レセ－一部負担金 TO 作１－一部負担金.
013170     MOVE レセ－請求金額   TO 作１－請求金額.
013180*
013190     MOVE 受－公費種別   TO 作１－保険区分キー.
013210
013220     MOVE "0"     TO 作１－助成種別.
013440
013450     MOVE "0"     TO 作１－市区町村使用区分.
013470*
013600* 連計 転記の後
013610     PERFORM 共通レコードセット.
013620*
013630*================================================================*
013640 作１レコードセット健保助成 SECTION.
013650*
013660**********/  健保助成ありの時  /**********
013670*
013680     MOVE SPACE TO 作１－レコード.
013690     INITIALIZE 作１－レコード.
013700*
013710* 保険者番号(数字)
013720     MOVE 受－保険者番号     TO 作１－保険者番号.
013730
013740     PERFORM 助成負担者番号取得.
013750*     MOVE 保険者番号Ｗ   TO 作１－保険者番号.
013760*
013770     IF ( 受－受益者番号助成(1:1) = "*"  ) OR
013780        ( 受－受益者番号助成(1:2) = "＊" )
013790        MOVE SPACE                TO 作１－助成受給者番号
013800     ELSE
013810        MOVE 受－受益者番号助成   TO 作１－助成受給者番号
013820     END-IF.
013830
013840*
013850     MOVE レセ－合計         TO 作１－合計金額.
013870     MOVE レセ－受給者負担額 TO 作１－一部負担金.
013880* 助成金額
013890     MOVE レセ－助成請求金額 TO 作１－請求金額.
013900*
013910     MOVE 受－助成種別   TO 作１－保険区分キー.
013920     MOVE NC"助成"       TO 作１－保険種別.
013930
013940
013950* 助成種別
013960     EVALUATE 受－助成種別
013970     WHEN "51"
013980         MOVE "1" TO 作１－助成種別
014120
014130     WHEN "52"
014140         MOVE "3" TO 作１－助成種別
014150     WHEN "53"
014160         MOVE "2" TO 作１－助成種別
014170     WHEN "54"
014180         MOVE "6" TO 作１－助成種別
014190     WHEN "55"
014200         MOVE "4" TO 作１－助成種別
014210     WHEN "60"
14280             MOVE "0" TO 作１－助成種別
014300     
014310     WHEN OTHER
014320         MOVE "0" TO 作１－助成種別
014330     END-EVALUATE.
014340
014350* 市区町村名
014360     PERFORM 市町村情報取得２.
014370
014380*
014390* 連計 転記の後
014400     PERFORM 共通レコードセット.
014410*
014420*================================================================*
014430 作１レコードセット老人助成 SECTION.
014440*
014450**********/  老人助成ありの時  /**********
014460*
014470     MOVE SPACE TO 作１－レコード.
014480     INITIALIZE 作１－レコード.
014490*
           IF 受－施術和暦年月 < 42004
014500        MOVE 受－費用負担者番号 TO 作１－保険者番号 作１－老人市町村番号
014510
014520*        MOVE 受－費用負担者番号 TO 作１－老人市町村番号
014530*
014540        MOVE 受－受益者番号老人 TO 作１－老人受給者番号
           ELSE
014500        MOVE 受－保険者番号     TO 作１－保険者番号
           END-IF.
014550*
014560
014570     PERFORM 助成負担者番号取得.
014580*     MOVE 保険者番号Ｗ   TO 作１－保険者番号.
014590*
014600     IF ( 受－受益者番号助成(1:1) = "*"  ) OR
014610        ( 受－受益者番号助成(1:2) = "＊" )
014620        MOVE SPACE                TO 作１－助成受給者番号
014630     ELSE
014640        MOVE 受－受益者番号助成   TO 作１－助成受給者番号
014650     END-IF.
014660*
014670     MOVE レセ－合計         TO 作１－合計金額.
014690     MOVE レセ－一部負担金   TO 作１－一部負担金.
014700* 助成金額
014710     MOVE レセ－助成請求金額 TO 作１－請求金額.
014720*
014730     MOVE 受－助成種別   TO 作１－保険区分キー.
014740     MOVE NC"助成"       TO 作１－保険種別.
014750
014760
014770* 助成種別
014780         EVALUATE 受－助成種別
014790         WHEN "52"
014800            MOVE "3" TO 作１－助成種別
014810         WHEN "53"
014820             MOVE "2" TO 作１－助成種別
014830         WHEN "54"
014840             MOVE "6" TO 作１－助成種別
      */愛知県高齢福祉給付金(マル福)の対応/080416
               WHEN "60"
                   EVALUATE TRUE
                   WHEN 受－費用負担者番号助成(1:4) = "8923"
                       MOVE "9"   TO 作１－助成種別
                   WHEN OTHER
                       MOVE "0"   TO 作１－助成種別
                   END-EVALUATE
014850         WHEN OTHER
014860             MOVE "0" TO 作１－助成種別
014870         END-EVALUATE
014880* 市区町村名
014890     PERFORM 市町村情報取得２.
014900*
014910* 連計 転記の後
014920     PERFORM 共通レコードセット.
014930*
014940*================================================================*
014950 共通レコードセット SECTION.
014960*
014970     MOVE レセ－請求和暦     TO 作１－請求和暦キー.
014980     MOVE レセ－請求年       TO 作１－請求年キー.
014990     MOVE レセ－請求月       TO 作１－請求月キー.
015000     MOVE 受－施術和暦       TO 作１－施術和暦キー 施術和暦ＷＲ.
015010     MOVE 受－施術年         TO 作１－施術年キー 施術年ＷＲ.
015020     MOVE 受－施術月         TO 作１－施術月キー 施術月ＷＲ.
015030     MOVE 受－患者コード     TO 作１－患者コードキー 患者コードＷＲ.
015040     MOVE 受－患者番号       TO 作１－患者番号.
015050     MOVE 会員番号数字Ｗ     TO 作１－会員番号.
015060* 年月
015070     MOVE 西暦請求年Ｗ       TO 作１－請求年.
015080     MOVE 西暦請求月Ｗ       TO 作１－請求月.
015090*/請求日に01を転記。0303
015100     MOVE 01                 TO 作１－請求日.
015110     MOVE "/"                TO 作１－請求区切１ 作１－請求区切２.
015120*
015130     PERFORM 西暦施術年月取得.
015140     MOVE 西暦施術年Ｗ       TO 作１－施術年.
015150     MOVE 西暦施術月Ｗ       TO 作１－施術月.
015160*/施術日に01を転記。0303
015170     MOVE 01                 TO 作１－施術日.
015180     MOVE "/"                TO 作１－施術区切１ 作１－施術区切２.
015190*
015200* 保険者番号キー(数字)
015210     IF ( 受－公費種別       = ZERO  ) AND
015220        ( 受－費用負担者番号 = SPACE )
015230          MOVE 受－保険者番号     TO 作１－保険者番号キー
015240     ELSE
015250* / 老人は、市町村番号をキーに /
015260          MOVE 受－費用負担者番号 TO 作１－保険者番号キー
015270     END-IF.
015280*
      *-----------------------------------------------------------------*
               MOVE SPACE TO 連暗号複合－暗号情報
      *
      *        / 連暗号複合－入力情報セット /
               MOVE 受－記号       TO 連暗号複合－記号
               MOVE 受－番号       TO 連暗号複合－番号
               MOVE 受－暗号化項目 TO 連暗号複合－暗号化項目
      *
               CALL   複合プログラム名Ｗ
               CANCEL 複合プログラム名Ｗ
      *
               MOVE 連暗号複合－複合した記号 TO 記号ＷＲ
               MOVE 連暗号複合－複合した番号 TO 番号ＷＲ
      *
      *-----------------------------------------------------------------*
015290* 記号
015300     IF 記号ＮＷＲ(1:1)  = NC"＊" 
015310        MOVE SPACE               TO 記号ＰＮＷ
015320        INSPECT 記号ＰＮＷ  REPLACING ALL 半角空白 BY 全角空白
015330        MOVE 記号ＰＷ            TO 作１－記号
015340     ELSE
015350        PERFORM 記号左詰め
015360        MOVE 記号Ｗ              TO 作１－記号
015370     END-IF.
015380* 番号
015390     IF ( 番号ＷＲ(1:1) = "*"  ) OR
015400        ( 番号ＷＲ(1:2) = "＊" )
015410        MOVE SPACE           TO 作１－番号
015420     ELSE
015430        MOVE 番号ＷＲ        TO 作１－番号
015440*/修正が不十分↓/050310
015450        MOVE 作１－番号 TO 漢字チェックＴＢＬ
015460        MOVE 16         TO カウンタ３
015470        PERFORM 全角半角チェック
015480        MOVE 漢字チェックＴＢＬ TO 作１－番号
015580     END-IF.
015590* 氏名
015600     MOVE 受－被保険者氏名   TO 被保険者氏名Ｗ.
015620        MOVE 被保険者氏名Ｗ  TO 漢字チェックＴＢＬ.
015630        MOVE 20                 TO カウンタ３.
015640        PERFORM 全角半角チェック.
015650        MOVE 漢字チェックＴＢＬ TO 作１－被保険者氏名.
015750     MOVE 受－被保険者カナ   TO 作１－被保険者カナ 作１－被保険者カナキー.
015760     MOVE 受－患者氏名       TO 患者氏名Ｗ.
015780        MOVE 患者氏名Ｗ      TO 漢字チェックＴＢＬ.
015790        MOVE 20             TO カウンタ３.
015800        PERFORM 全角半角チェック.
015810        MOVE 漢字チェックＴＢＬ TO 作１－患者氏名.
015910     MOVE 受－患者カナ       TO 作１－患者カナ.
015920* 住所(患者)
015930     MOVE SPACE              TO 患者住所Ｗ.
015940     STRING 受－患者住所１   DELIMITED BY SPACE
015950            受－患者住所２   DELIMITED BY SPACE
015960            INTO 患者住所Ｗ
015970     END-STRING.
015990        MOVE 患者住所Ｗ TO 漢字チェックＴＢＬ.
016000        MOVE 28         TO カウンタ３.
016010        PERFORM 全角半角チェック.
016020        MOVE 漢字チェックＴＢＬ TO 患者住所Ｗ.
016120     MOVE 患者住所１Ｗ     TO 作１－患者住所.
016130*
           IF 受－本人家族区分 = 1
               MOVE "1" TO 作１－続柄
           ELSE
               MOVE "2" TO 作１－続柄
           END-IF.
016290*
016370     MOVE 受－患者性別 TO 作１－性別.
016380     
016390* 生年月日
016400     MOVE ZERO               TO 計算和暦年月日Ｗ.
016410     MOVE 受－患者生年月日   TO 計算和暦年月日Ｗ.
016420     PERFORM 西暦年月日取得.
016430     MOVE 計算西暦年Ｗ   TO 作１－患者生年.
016440     MOVE 計算西暦月Ｗ   TO 作１－患者生月.
016450     MOVE 計算西暦日Ｗ   TO 作１－患者生日.
016460     MOVE "/"            TO 作１－患者生区切１ 作１－患者生区切２.
016470
016490* 年齢を計算
016577     INITIALIZE 連年齢－キー.
016578*
016579     MOVE 受－施術和暦   TO  連年齢－施術和暦.
016580     MOVE 受－施術年     TO  連年齢－施術年.
016581     MOVE 受－施術月     TO  連年齢－施術月.
016582     MOVE 受－患者和暦   TO  連年齢－和暦.
016583     MOVE 受－患者年     TO  連年齢－年.
016584     MOVE 受－患者月     TO  連年齢－月.
016585     MOVE 受－患者日     TO  連年齢－日.
016586*
016587     CALL   "NENREI".
016588     CANCEL "NENREI".
016589*
016590     MOVE 連年齢－年齢   TO 患者年齢Ｗ.
016596* 年齢区分設定
016600     MOVE "0" TO 作１－年齢区分.
016610     
016620     IF 患者年齢Ｗ < 3
016630         MOVE "1" TO 作１－年齢区分
016640     END-IF.
016650     IF 患者年齢Ｗ >= 70
016660         MOVE "2" TO 作１－年齢区分
016670     END-IF.
016680
016690*
016700****　/ レセプトデータの取得 /
016710*
016730     PERFORM 負傷データ取得.
016740     PERFORM 施術記録取得.
016750*****
016760*
016770* １部位目
016780     MOVE 負傷名Ｗ(1)            TO 作１－負傷名(1).
016790*
016800     MOVE ZERO                   TO 計算和暦年月日Ｗ.
016810     MOVE 負傷年月日Ｗ(1)        TO 計算和暦年月日Ｗ.
016820     PERFORM 西暦年月日取得.
016830     MOVE 計算西暦年Ｗ       TO 作１－負傷年(1).
016840     MOVE 計算西暦月Ｗ       TO 作１－負傷月(1).
016850     MOVE 計算西暦日Ｗ       TO 作１－負傷日(1).
016860*
016870     MOVE ZERO                   TO 計算和暦年月日Ｗ.
016880     MOVE 初検年月日Ｗ(1)        TO 計算和暦年月日Ｗ.
016890     PERFORM 西暦年月日取得.
016900     MOVE 計算西暦年Ｗ       TO 作１－初検年(1).
016910     MOVE 計算西暦月Ｗ       TO 作１－初検月(1).
016920     MOVE 計算西暦日Ｗ       TO 作１－初検日(1).
016930*
016940     MOVE ZERO                   TO 計算和暦年月日Ｗ.
016950     MOVE 開始年月日Ｗ(1)        TO 計算和暦年月日Ｗ.
016960     PERFORM 西暦年月日取得.
016970     MOVE 計算西暦年Ｗ       TO 作１－施術開始年(1).
016980     MOVE 計算西暦月Ｗ       TO 作１－施術開始月(1).
016990     MOVE 計算西暦日Ｗ       TO 作１－施術開始日(1).
017000*
017010     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017020     MOVE 終了年月日Ｗ(1)        TO 計算和暦年月日Ｗ.
017030     PERFORM 西暦年月日取得.
017040     MOVE 計算西暦年Ｗ       TO 作１－施術終了年(1).
017050     MOVE 計算西暦月Ｗ       TO 作１－施術終了月(1).
017060     MOVE 計算西暦日Ｗ       TO 作１－施術終了日(1).
017070*
017080     MOVE 実日数Ｗ(1)            TO 作１－実日数(1).
017090*
017100     MOVE 転帰区分Ｗ(1)          TO 転帰変換前Ｗ.
017110     PERFORM 転帰区分変換.
017120     MOVE 転帰変換後Ｗ           TO 作１－転帰区分(1).
017130*
017140* ２部位目
017150     MOVE 負傷名Ｗ(2)            TO 作１－負傷名(2).
017160*
017170     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017180     MOVE 負傷年月日Ｗ(2)        TO 計算和暦年月日Ｗ.
017190     PERFORM 西暦年月日取得.
017200     MOVE 計算西暦年Ｗ       TO 作１－負傷年(2).
017210     MOVE 計算西暦月Ｗ       TO 作１－負傷月(2).
017220     MOVE 計算西暦日Ｗ       TO 作１－負傷日(2).
017230*
017240     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017250     MOVE 初検年月日Ｗ(2)        TO 計算和暦年月日Ｗ.
017260     PERFORM 西暦年月日取得.
017270     MOVE 計算西暦年Ｗ       TO 作１－初検年(2).
017280     MOVE 計算西暦月Ｗ       TO 作１－初検月(2).
017290     MOVE 計算西暦日Ｗ       TO 作１－初検日(2).
017300*
017310     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017320     MOVE 開始年月日Ｗ(2)        TO 計算和暦年月日Ｗ.
017330     PERFORM 西暦年月日取得.
017340     MOVE 計算西暦年Ｗ       TO 作１－施術開始年(2).
017350     MOVE 計算西暦月Ｗ       TO 作１－施術開始月(2).
017360     MOVE 計算西暦日Ｗ       TO 作１－施術開始日(2).
017370*
017380     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017390     MOVE 終了年月日Ｗ(2)        TO 計算和暦年月日Ｗ.
017400     PERFORM 西暦年月日取得.
017410     MOVE 計算西暦年Ｗ       TO 作１－施術終了年(2).
017420     MOVE 計算西暦月Ｗ       TO 作１－施術終了月(2).
017430     MOVE 計算西暦日Ｗ       TO 作１－施術終了日(2).
017440*
017450     MOVE 実日数Ｗ(2)            TO 作１－実日数(2).
017460*
017470     MOVE 転帰区分Ｗ(2)          TO 転帰変換前Ｗ.
017480     PERFORM 転帰区分変換.
017490     MOVE 転帰変換後Ｗ           TO 作１－転帰区分(2).
017500*
017510* ３部位目
017520     MOVE 負傷名Ｗ(3)            TO 作１－負傷名(3).
017530*
017540     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017550     MOVE 負傷年月日Ｗ(3)        TO 計算和暦年月日Ｗ.
017560     PERFORM 西暦年月日取得.
017570     MOVE 計算西暦年Ｗ       TO 作１－負傷年(3).
017580     MOVE 計算西暦月Ｗ       TO 作１－負傷月(3).
017590     MOVE 計算西暦日Ｗ       TO 作１－負傷日(3).
017600*
017610     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017620     MOVE 初検年月日Ｗ(3)        TO 計算和暦年月日Ｗ.
017630     PERFORM 西暦年月日取得.
017640     MOVE 計算西暦年Ｗ       TO 作１－初検年(3).
017650     MOVE 計算西暦月Ｗ       TO 作１－初検月(3).
017660     MOVE 計算西暦日Ｗ       TO 作１－初検日(3).
017670*
017680     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017690     MOVE 開始年月日Ｗ(3)        TO 計算和暦年月日Ｗ.
017700     PERFORM 西暦年月日取得.
017710     MOVE 計算西暦年Ｗ       TO 作１－施術開始年(3).
017720     MOVE 計算西暦月Ｗ       TO 作１－施術開始月(3).
017730     MOVE 計算西暦日Ｗ       TO 作１－施術開始日(3).
017740*
017750     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017760     MOVE 終了年月日Ｗ(3)        TO 計算和暦年月日Ｗ.
017770     PERFORM 西暦年月日取得.
017780     MOVE 計算西暦年Ｗ       TO 作１－施術終了年(3).
017790     MOVE 計算西暦月Ｗ       TO 作１－施術終了月(3).
017800     MOVE 計算西暦日Ｗ       TO 作１－施術終了日(3).
017810*
017820     MOVE 実日数Ｗ(3)            TO 作１－実日数(3).
017830*
017840     MOVE 転帰区分Ｗ(3)          TO 転帰変換前Ｗ.
017850     PERFORM 転帰区分変換.
017860     MOVE 転帰変換後Ｗ           TO 作１－転帰区分(3).
017870*
017880* ４部位目
017890     MOVE 負傷名Ｗ(4)            TO 作１－負傷名(4).
017900*
017910     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017920     MOVE 負傷年月日Ｗ(4)        TO 計算和暦年月日Ｗ.
017930     PERFORM 西暦年月日取得.
017940     MOVE 計算西暦年Ｗ       TO 作１－負傷年(4).
017950     MOVE 計算西暦月Ｗ       TO 作１－負傷月(4).
017960     MOVE 計算西暦日Ｗ       TO 作１－負傷日(4).
017970*
017980     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017990     MOVE 初検年月日Ｗ(4)        TO 計算和暦年月日Ｗ.
018000     PERFORM 西暦年月日取得.
018010     MOVE 計算西暦年Ｗ       TO 作１－初検年(4).
018020     MOVE 計算西暦月Ｗ       TO 作１－初検月(4).
018030     MOVE 計算西暦日Ｗ       TO 作１－初検日(4).
018040*
018050     MOVE ZERO                   TO 計算和暦年月日Ｗ.
018060     MOVE 開始年月日Ｗ(4)        TO 計算和暦年月日Ｗ.
018070     PERFORM 西暦年月日取得.
018080     MOVE 計算西暦年Ｗ       TO 作１－施術開始年(4).
018090     MOVE 計算西暦月Ｗ       TO 作１－施術開始月(4).
018100     MOVE 計算西暦日Ｗ       TO 作１－施術開始日(4).
018110*
018120     MOVE ZERO                   TO 計算和暦年月日Ｗ.
018130     MOVE 終了年月日Ｗ(4)        TO 計算和暦年月日Ｗ.
018140     PERFORM 西暦年月日取得.
018150     MOVE 計算西暦年Ｗ       TO 作１－施術終了年(4).
018160     MOVE 計算西暦月Ｗ       TO 作１－施術終了月(4).
018170     MOVE 計算西暦日Ｗ       TO 作１－施術終了日(4).
018180*
018190     MOVE 実日数Ｗ(4)            TO 作１－実日数(4).
018200*
018210     MOVE 転帰区分Ｗ(4)          TO 転帰変換前Ｗ.
018220     PERFORM 転帰区分変換.
018230     MOVE 転帰変換後Ｗ           TO 作１－転帰区分(4).
018240*
018250     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 4
018260         MOVE "/"    TO 作１－負傷区切１(カウンタ)     作１－負傷区切２(カウンタ)
018270                        作１－初検区切１(カウンタ)     作１－初検区切２(カウンタ)
018280                        作１－施術開始区切１(カウンタ) 作１－施術開始区切２(カウンタ)
018290                        作１－施術終了区切１(カウンタ) 作１－施術終了区切２(カウンタ)
018300     END-PERFORM.
018310**************************************************************
018320*
018330     MOVE 部位数Ｗ               TO 作１－部位数.
018340**
018350** / 負傷原因・長期理由 / **
018360*
018370     IF 負傷原因印刷区分Ｗ  NOT = 1 
018380*      / 負傷原因用WORKクリアー /
018390         INITIALIZE 負傷原因ＷＴ
018400         INITIALIZE 負傷患者番号ＣＷ
018410         INITIALIZE 負傷連番ＣＷ
018420         INITIALIZE 負傷原因ＴＢＬ
018430         INITIALIZE 負傷原因内容Ｗ
018440         PERFORM 負傷原因取得
018450     END-IF.
018460*
018470     IF 長期理由印刷区分Ｗ  NOT = 1 
018480        PERFORM 長期理由文取得
018490     ELSE
018500        MOVE  SPACE TO  連長文－キー
018510        INITIALIZE      連長文－キー
018520     END-IF.
018530*
018540     MOVE 負傷原因Ｗ(1)       TO 作１－負傷原因.
018550     MOVE 連長文－理由文(1)   TO 作１－長期理由.
018560*
           MOVE 受－施術和暦 TO 連率－施術和暦
           MOVE 受－施術年   TO 連率－施術年
           MOVE 受－施術月   TO 連率－施術月
           MOVE 受－患者番号 TO 連率－患者番号
           MOVE 受－枝番     TO 連率－枝番
           CALL   "HUTANRIT"
           CANCEL "HUTANRIT"
018570     IF 作４－保険種別 > 50
      */助成分の負担割合に変更/071220
               COMPUTE 作１－負担割合 = 10 - ((連率－実際本体負担率 - 連率－助成負担率) / 10)
018590     ELSE
018600         IF 作４－保険種別 = 05
                   COMPUTE 作１－負担割合 = 連率－２７老負担率 / 10
018690         ELSE
018700             COMPUTE 作１－負担割合 = 連率－実際本体負担率 / 10
018710         END-IF
018720     END-IF.
018730*
018740*================================================================*
018750 作２レコードセット SECTION.
018760*
018770**********/  健保の時  /**********
018780*
018790     MOVE SPACE TO 作２－レコード.
018800     INITIALIZE 作２－レコード.
018810     IF 保険者番号ＷＲ NOT = 受－保険者番号
018820         MOVE 受－保険者番号 TO 作２－保険者番号 作２－保険者番号キー
018830         PERFORM 請求先情報取得
018840         MOVE 11               TO 名－区分コード
018850         MOVE 受－保険種別     TO 名－名称コード
018860         READ 名称マスタ
018870         NOT INVALID KEY
018880             MOVE 名－略称     TO 作２－保険種別
018890         END-READ
018900         IF 作２－保険種別 = NC"退職"
018910             MOVE NC"国保退職" TO 作２－保険種別
018920         END-IF
018930         IF ((受－保険種別 = 01) AND (受－保険者番号(3:1) = 3)) OR
018940            ((受－保険種別 = 08) AND (受－保番(3:1) = 3))
018950             MOVE NC"国保組合" TO 作２－保険種別
018960         END-IF
018970* 船員、日雇は社保には含めない 2007/10/22
018980*         IF 作２－保険種別 = NC"船員" OR NC"日雇"
018990*             MOVE NC"社保" TO 作２－保険種別
019000*         END-IF
019010         IF 作２－保険種別 = NC"組合"
019020             MOVE NC"健保" TO 作２－保険種別
019030         END-IF
019040         IF 受－保険種別 = 09
019050             MOVE NC"共済" TO 作２－保険種別
019060         END-IF
019070         PERFORM 作２ファイル書込
019080     END-IF.
019090*
019100*================================================================*
019110 作２レコードセット老人 SECTION.
019120* 2007/10/24 追加
019130**********/  老人の時  /**********
019140*
019150     MOVE SPACE TO 作２－レコード.
019160     INITIALIZE 作２－レコード.
019170     IF 市町村番号老人ＷＲ NOT = 受－費用負担者番号
019180         MOVE 受－費用負担者番号 TO 作２－保険者番号 作２－保険者番号キー
019190         MOVE 受－公費種別       TO 保険種別ＷＲ
               IF 受－施術和暦年月 < 42004
019200            MOVE NC"老人" TO 作２－保険種別
               ELSE
019200            MOVE NC"後期高齢" TO 作２－保険種別
               END-IF
019210         PERFORM 市町村情報取得３
019220         PERFORM 作２ファイル書込
019230     END-IF.
019240*================================================================*
019250 作２レコードセット助成 SECTION.
019260* 2007/10/24 追加
019270**********/  助成の時  /**********
019280*
019290     MOVE SPACE TO 作２－レコード.
019300     INITIALIZE 作２－レコード.
019310     IF 市町村番号助成ＷＲ NOT = 受－費用負担者番号助成
019320         MOVE 受－費用負担者番号助成 TO 作２－保険者番号 作２－保険者番号キー
019330         MOVE 受－助成種別       TO 保険種別ＷＲ
019340         PERFORM 市町村情報取得３
019350
019360         MOVE 作１－助成負担者番号 TO 作２－保険者番号
019370         MOVE NC"助成" TO 作２－保険種別
019380
019390         PERFORM 作２ファイル書込
019400     END-IF.
019410
019710*================================================================*
019720 請求先情報取得 SECTION.
019730*
019740*********************************************************
019750* 連結データから保険者マスタより請求先を取得する。      *
019760* ● 請求先...... 請求先名称Ｗに格納                    *
019770*********************************************************
019780     MOVE 受－保険種別        TO 保－保険種別.
019790     MOVE 受－保険者番号      TO 保－保険者番号.
019800     READ 保険者マスタ
019810     INVALID KEY
019820         MOVE SPACE           TO 作２－保険者名称
019830         MOVE SPACE           TO 作２－郵便番号
019840         MOVE SPACE           TO 作２－保険者住所
019850         MOVE SPACE           TO 作２－電話番号
019860     NOT INVALID KEY
019870         MOVE 保－保険者名称  TO 保険者名称Ｗ
019880         MOVE 保険者名称ＷＲ  TO 作２－保険者名称
019890*         MOVE 保－郵便番号    TO 作２－郵便番号
019900         IF 保－郵便番号 NOT = SPACE
019910             MOVE SPACE TO 作２－郵便番号
019920             STRING 保－郵便番号１ DELIMITED BY SIZE
019930                    "-"            DELIMITED BY SIZE
019940                    保－郵便番号２ DELIMITED BY SIZE
019950               INTO 作２－郵便番号
019960             END-STRING
019970         END-IF
019980         MOVE 保－住所        TO 作２－保険者住所
019990         MOVE 保－電話番号    TO 作２－電話番号
020000     END-READ.
020390*================================================================*
020400 市町村情報取得２ SECTION.
020410*
020420****************************************************
020430* 連結データから保険者マスタより請求先を取得する。 *
020440* ● 請求先...... 請求先名称Ｗに格納               *
020450****************************************************
020460     MOVE 受－助成種別             TO 市－公費種別.
020470     MOVE 受－費用負担者番号助成   TO 市－市町村番号.
020480     READ 市町村マスタ
020490     INVALID KEY
020500         MOVE SPACE          TO 作１－市区町村名
020510     NOT INVALID KEY
020520         IF 市－請求先区分 = 1
020530             MOVE 受－助成種別       TO 請先－保険種別
020540             MOVE 受－費用負担者番号助成   TO 請先－保険者番号
020550             READ 請求先マスタ
020560             INVALID KEY
020570                 MOVE SPACE     TO 市町村名称Ｗ
020580             NOT INVALID KEY
020590                 MOVE 請先－保険者名称  TO 市町村名称Ｗ
020600             END-READ
020610         ELSE
020620             MOVE 市－市町村名称  TO 市町村名称Ｗ
020630         END-IF
020640         MOVE 市町村名称ＷＲ TO 作１－市区町村名
020670      END-READ.
020680*
020690*================================================================*
020700 市町村情報取得３ SECTION.
020720****************************************************
020730* 連結データから保険者マスタより請求先を取得する。 *
020740* ● 請求先...... 請求先名称Ｗに格納               *
020750****************************************************
020760     MOVE 保険種別ＷＲ       TO 市－公費種別.
020770     MOVE 作２－保険者番号   TO 市－市町村番号.
020780     READ 市町村マスタ
020790     INVALID KEY
020800         MOVE SPACE          TO 作２－保険者名称
020810     NOT INVALID KEY
020820         IF 市－請求先区分 = 1
020830             MOVE 保険種別ＷＲ       TO 請先－保険種別
020840             MOVE 作２－保険者番号   TO 請先－保険者番号
020850             READ 請求先マスタ
020860             INVALID KEY
020870                 MOVE SPACE     TO 市町村名称Ｗ
020880             NOT INVALID KEY
020890                 MOVE 請先－保険者名称  TO 市町村名称Ｗ
020900             END-READ
020910         ELSE
020920             MOVE 市－市町村名称  TO 市町村名称Ｗ
020930         END-IF
020940         MOVE 市町村名称ＷＲ TO 作２－保険者名称
020960         IF 市－郵便番号 NOT = SPACE
020970             MOVE SPACE TO 作２－郵便番号
020980             STRING 市－郵便番号１ DELIMITED BY SIZE
020990                    "-"            DELIMITED BY SIZE
021000                    市－郵便番号２ DELIMITED BY SIZE
021010               INTO 作２－郵便番号
021020             END-STRING
021030         END-IF
021040         MOVE 市－住所       TO 作２－保険者住所
021050         MOVE 市－電話番号   TO 作２－電話番号
021060      END-READ.
021070*
021800*================================================================*
021810 作１ファイル書込 SECTION.
021820*
021830     WRITE 作１－レコード
021840     INVALID KEY
021850         MOVE NC"作１"  TO ファイル名
021860         PERFORM エラー表示
021870     END-WRITE.
021880*================================================================*
021890 作２ファイル書込 SECTION.
021900*
021910     WRITE 作２－レコード
021920     INVALID KEY
021930         MOVE NC"作２"  TO ファイル名
021940         PERFORM エラー表示
021950     END-WRITE.
022040*================================================================*
022050 会員番号右詰め SECTION.
022060*
022070     MOVE 会員番号Ｗ      TO  会員番号左詰めＷ.
022080     MOVE ZERO            TO  会員番号右詰めＷ.
022090     MOVE ZERO            TO  会員番号数字Ｗ.
022100*
022110     MOVE  8  TO  カウンタ.
022120*
022130     IF  会員番号左詰めＷ１(7) NOT = SPACE
022140         COMPUTE カウンタ = カウンタ  -  1
022150         MOVE 会員番号左詰めＷ１(7)  TO  会員番号右詰めＷ１(カウンタ)
022160     END-IF.
022170     IF  会員番号左詰めＷ１(6) NOT = SPACE
022180         COMPUTE カウンタ = カウンタ  -  1
022190         MOVE 会員番号左詰めＷ１(6)  TO  会員番号右詰めＷ１(カウンタ)
022200     END-IF.
022210     IF  会員番号左詰めＷ１(5) NOT = SPACE
022220         COMPUTE カウンタ = カウンタ  -  1
022230         MOVE 会員番号左詰めＷ１(5)  TO  会員番号右詰めＷ１(カウンタ)
022240     END-IF.
022250     IF  会員番号左詰めＷ１(4) NOT = SPACE
022260         COMPUTE カウンタ = カウンタ  -  1
022270         MOVE 会員番号左詰めＷ１(4)  TO  会員番号右詰めＷ１(カウンタ)
022280     END-IF.
022290     IF  会員番号左詰めＷ１(3) NOT = SPACE
022300         COMPUTE カウンタ = カウンタ  -  1
022310         MOVE 会員番号左詰めＷ１(3)  TO  会員番号右詰めＷ１(カウンタ)
022320     END-IF.
022330     IF  会員番号左詰めＷ１(2) NOT = SPACE
022340         COMPUTE カウンタ = カウンタ  -  1
022350         MOVE 会員番号左詰めＷ１(2)  TO  会員番号右詰めＷ１(カウンタ)
022360     END-IF.
022370     IF  会員番号左詰めＷ１(1) NOT = SPACE
022380         COMPUTE カウンタ = カウンタ  -  1
022390         MOVE 会員番号左詰めＷ１(1)  TO  会員番号右詰めＷ１(カウンタ)
022400     END-IF.
022410*
022420     MOVE 会員番号右詰めＷ TO 会員番号数字Ｗ.
022430*
022440*================================================================*
022450 記号左詰め SECTION.
022460*
022470***** 記号の無駄なSPACEを取り除いて、左詰めにする。
022480     MOVE SPACE           TO  記号ＮＷ.
022490     MOVE SPACE           TO  記号元Ｗ.
022500     MOVE SPACE           TO  記号左詰めＷ.
022510     MOVE 記号ＮＷＲ      TO  記号元Ｗ.
022520*
022530     MOVE  ZERO  TO  カウンタ２.
022540     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 12
022550          IF  記号元Ｗ１(カウンタ) NOT = SPACE
022560              COMPUTE カウンタ２ = カウンタ２  +  1
022570              MOVE 記号元Ｗ１(カウンタ)  TO  記号左詰めＷ１(カウンタ２)
022580          END-IF
022590     END-PERFORM.
022600*
022610     MOVE 記号左詰めＷ    TO 記号ＮＷ.
022620*
022630*半角スペースを全角にかえる
022640     INSPECT 記号Ｗ REPLACING ALL 半角空白 BY 全角空白.
022650*
022660*================================================================*
022670 負傷データ取得 SECTION.
022680*
022690     INITIALIZE 負傷情報Ｗ.
022700*
022710     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
022720     MOVE 施術年ＷＲ         TO 負－施術年.
022730     MOVE 施術月ＷＲ         TO 負－施術月.
022740     MOVE 患者コードＷＲ     TO 負－患者コード.
022750     READ 負傷データＦ
022760     INVALID KEY
022770         CONTINUE
022780     NOT INVALID KEY
022790         MOVE 負－部位数                   TO 部位数Ｗ
022800         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
022810                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
022820             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別Ｗ(部位ＣＮＴ)
022830             MOVE 負－部位(部位ＣＮＴ)     TO 部位Ｗ(部位ＣＮＴ)
022840             MOVE 負－左右区分(部位ＣＮＴ) TO 左右区分Ｗ(部位ＣＮＴ)
022850             MOVE 負－負傷位置番号(部位ＣＮＴ)
022860                                           TO 負傷位置番号Ｗ(部位ＣＮＴ)
022870* 負傷種別
022880             MOVE SPACE                     TO 負傷名称Ｗ
022890             MOVE 03                        TO 名－区分コード
022900             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
022910             READ 名称マスタ
022920             INVALID KEY
022930                 MOVE SPACE        TO 負傷名称Ｗ
022940             NOT INVALID KEY
022950                 MOVE 名－正式名称 TO 負傷名称Ｗ
022960             END-READ
022970* 部位
020710             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
028160*
028170             PERFORM 部位名称埋込処理
023120*
023130             MOVE 負－負傷和暦(部位ＣＮＴ)   TO 負傷和暦Ｗ(部位ＣＮＴ)
023140             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
023150             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
023160             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
023170             MOVE 負－開始和暦(部位ＣＮＴ)   TO 初検和暦Ｗ(部位ＣＮＴ)
023180             MOVE 負－開始年(部位ＣＮＴ)   TO 初検年Ｗ(部位ＣＮＴ)
023190             MOVE 負－開始月(部位ＣＮＴ)   TO 初検月Ｗ(部位ＣＮＴ)
023200             MOVE 負－開始日(部位ＣＮＴ)   TO 初検日Ｗ(部位ＣＮＴ)
023210             IF 負－転帰区分(部位ＣＮＴ) = 9
023220                 MOVE 99                   TO 終了年Ｗ(部位ＣＮＴ)
023230                 MOVE 99                   TO 終了月Ｗ(部位ＣＮＴ)
023240                 MOVE 99                   TO 終了日Ｗ(部位ＣＮＴ)
023250             ELSE
023260                 MOVE 負－終了和暦(部位ＣＮＴ)   TO 終了和暦Ｗ(部位ＣＮＴ)
023270                 MOVE 負－終了年(部位ＣＮＴ)   TO 終了年Ｗ(部位ＣＮＴ)
023280                 MOVE 負－終了月(部位ＣＮＴ)   TO 終了月Ｗ(部位ＣＮＴ)
023290                 MOVE 負－終了日(部位ＣＮＴ)   TO 終了日Ｗ(部位ＣＮＴ)
023300             END-IF
023310*
023320             MOVE 負－転帰区分(部位ＣＮＴ) TO 転帰区分Ｗ(部位ＣＮＴ)
023330*
023340         END-PERFORM
023350         PERFORM 初検日以前のデータ判定
023360* 枝番判定用
023370         MOVE 負－開始診療日手動区分 TO  開始診療日手動区分Ｗ
023380*
023390     END-READ.
023400*
030330*================================================================*
030340 部位名称埋込処理 SECTION.
030350*
006490     STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
009980            負傷名称Ｗ                    DELIMITED BY SPACE
006500            レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
006520       INTO 負傷名Ｗ(部位ＣＮＴ)
006570     END-STRING.
030550*
023410*================================================================*
023420 初検日以前のデータ判定 SECTION.
023430*
023440*********************************************************************************
023450*  最初の初検日以前の当月中に施術記録レコードがあった時(治癒、中止)は、請求区分の
023460*  継続にもチェックする。(新規と継続の両方)
023470*********************************************************************************
023480** 最初の初検日を取得
023490     MOVE SPACE                 TO 初検フラグ.
023500     MOVE 患者番号ＷＲ          TO 施記－患者番号.
023510     MOVE 枝番ＷＲ              TO 施記－枝番.
023520     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
023530     MOVE 施術年ＷＲ            TO 施記－施術年.
023540     MOVE 施術月ＷＲ            TO 施記－施術月.
023550     MOVE ZERO                  TO 施記－施術日.
023560     START 施術記録Ｆ   KEY IS >= 施記－患者コード
023570                                  施記－施術和暦年月日
023580     END-START.
023590     IF 状態キー = "00"
023600         MOVE ZERO  TO 初検和暦ＷＴ
023610         MOVE ZERO  TO 初検年ＷＴ
023620         MOVE ZERO  TO 初検月ＷＴ
023630         MOVE ZERO  TO 初検日ＷＴ
023640         MOVE SPACE TO 終了フラグ２
023650         PERFORM 施術記録Ｆ読込
023660         PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
023670                       ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
023680                       ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
023690                       ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
023700                       ( 施記－施術月     NOT = 施術月ＷＲ      ) OR
023710                       ( 初検フラグ           = "YES"           ) 
023720               IF  施記－診療区分 = 2
023730                   MOVE 施記－施術和暦           TO 初検和暦ＷＴ
023740                   MOVE 施記－施術年             TO 初検年ＷＴ
023750                   MOVE 施記－施術月             TO 初検月ＷＴ
023760                   MOVE 施記－施術日             TO 初検日ＷＴ
023770                   MOVE "YES"                    TO 初検フラグ
023780               END-IF
023790               PERFORM 施術記録Ｆ読込
023800         END-PERFORM
023810     END-IF.
023820*
023830*================================================================*
023840 施術記録取得 SECTION.
023850*
023860     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 部位数Ｗ
023870         IF ( 施術年ＷＲ = 初検年Ｗ(部位ＣＮＴ) ) AND
023880            ( 施術月ＷＲ = 初検月Ｗ(部位ＣＮＴ) )
023890             MOVE 患者番号ＷＲ          TO 施記－患者番号
023900             MOVE 枝番ＷＲ              TO 施記－枝番
023910             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
023920             MOVE 初検和暦Ｗ(部位ＣＮＴ)  TO 開始和暦Ｗ(部位ＣＮＴ)
023930             MOVE 初検年Ｗ(部位ＣＮＴ)  TO 開始年Ｗ(部位ＣＮＴ) 施記－施術年
023940             MOVE 初検月Ｗ(部位ＣＮＴ)  TO 開始月Ｗ(部位ＣＮＴ) 施記－施術月
023950             MOVE 初検日Ｗ(部位ＣＮＴ)  TO 開始日Ｗ(部位ＣＮＴ) 施記－施術日
023960         ELSE
023970             MOVE 患者番号ＷＲ          TO 施記－患者番号
023980             MOVE 枝番ＷＲ              TO 施記－枝番
023990             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
024000             MOVE 施術年ＷＲ            TO 施記－施術年
024010             MOVE 施術月ＷＲ            TO 施記－施術月
024020             MOVE ZERO                  TO 施記－施術日
024030         END-IF
024040         START 施術記録Ｆ   KEY IS >= 施記－患者コード
024050                                      施記－施術和暦年月日
024060         END-START
024070         IF 状態キー = "00"
024080             MOVE ZERO  TO 実日数Ｗ(部位ＣＮＴ)
024090             MOVE ZERO  TO 終了和暦ＷＴ
024100             MOVE ZERO  TO 終了年ＷＴ
024110             MOVE ZERO  TO 終了月ＷＴ
024120             MOVE ZERO  TO 終了日ＷＴ
024130             MOVE SPACE TO 終了フラグ２
024140             PERFORM 施術記録Ｆ読込
024150             IF  ( 終了フラグ２      = SPACE   ) AND
024160                 ( 施記－患者コード  = 患者コードＷＲ ) AND
024170                 ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
024180                 ( 施記－施術年      = 施術年ＷＲ     ) AND
024190                 ( 施記－施術月      = 施術月ＷＲ     ) 
024200*
024210*        *****************************************************************
024220*        * 開始年月日 ( その部位が当月初検でないか、
024230*                       当月初検でも枝番がある時は、最初の施術日を開始日)*
024240*        *****************************************************************
024250                 IF ( 施術年ＷＲ NOT = 初検年Ｗ(部位ＣＮＴ) ) OR
024260                    ( 施術月ＷＲ NOT = 初検月Ｗ(部位ＣＮＴ) ) OR
024270                    ( 開始診療日手動区分Ｗ = 1 )
024280                     MOVE 施記－施術和暦 TO 開始和暦Ｗ(部位ＣＮＴ)
024290                     MOVE 施記－施術年   TO 開始年Ｗ(部位ＣＮＴ)
024300                     MOVE 施記－施術月   TO 開始月Ｗ(部位ＣＮＴ)
024310                     MOVE 施記－施術日   TO 開始日Ｗ(部位ＣＮＴ)
024320                 END-IF
024330             END-IF
024340             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
024350                           ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
024360                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
024370                           ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
024380                           ( 施記－施術月     NOT = 施術月ＷＲ       ) OR
024390                           ( 施記－施術日         > 終了日Ｗ(部位ＣＮＴ))
024400*               **********
024410*               * 実日数 *
024420*               **********
024430                COMPUTE 実日数Ｗ(部位ＣＮＴ) = 実日数Ｗ(部位ＣＮＴ) + 1
024440                MOVE 施記－施術和暦             TO 終了和暦ＷＴ
024450                MOVE 施記－施術年               TO 終了年ＷＴ
024460                MOVE 施記－施術月               TO 終了月ＷＴ
024470                MOVE 施記－施術日               TO 終了日ＷＴ
024480*
024490                PERFORM 施術記録Ｆ読込
024500            END-PERFORM
024510        END-IF
024520*       **************************
024530*       * 継続：終了年月日セット *
024540*       **************************
024550        IF 転帰区分Ｗ(部位ＣＮＴ) = 9
024560            MOVE 終了和暦ＷＴ  TO 終了和暦Ｗ(部位ＣＮＴ)
024570            MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
024580            MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
024590            MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
024600        END-IF
024610     END-PERFORM.
024620*
025030*================================================================*
025040 西暦施術年月取得 SECTION.
025050* 
025060     MOVE ZERO          TO 西暦年月Ｗ  西暦施術年月Ｗ.
025070     MOVE 受－施術和暦  TO 元－元号区分.
025080     READ 元号マスタ
025090     NOT INVALID KEY
025100         MOVE 元－開始西暦年 TO 西暦年Ｗ
025110     END-READ.
025120**
025130     IF 西暦年Ｗ = ZERO
025140          MOVE  NC"元号マスタに開始西暦年を登録して下さい" TO 連メ－メッセージ
025150          CALL   "MSG001"
025160          CANCEL "MSG001"
025170          PERFORM ファイル閉鎖
025180          MOVE 99 TO PROGRAM-STATUS
025190          EXIT PROGRAM
025200     ELSE
025210          COMPUTE 西暦年Ｗ = 西暦年Ｗ + 受－施術年 - 1
025220          MOVE 受－施術月 TO 西暦月Ｗ
025230     END-IF.
025240*
025250     MOVE 西暦年月Ｗ   TO  西暦施術年月Ｗ.
025260*
025270*================================================================*
025280 西暦年月日取得 SECTION.
025290*
025300     MOVE ZERO  TO 計算西暦年月日Ｗ.
025310*
025320     IF 計算和暦Ｗ  NOT = ZERO
025330         MOVE 計算和暦Ｗ    TO 元－元号区分
025340         READ 元号マスタ
025350         NOT INVALID KEY
025360             MOVE 元－開始西暦年 TO 計算西暦年Ｗ
025370         END-READ
025380**
025390         IF 計算西暦年Ｗ = ZERO
025400              MOVE  NC"元号マスタに開始西暦年を登録して下さい" TO 連メ－メッセージ
025410              CALL   "MSG001"
025420              CANCEL "MSG001"
025430              PERFORM ファイル閉鎖
025440              MOVE 99 TO PROGRAM-STATUS
025450              EXIT PROGRAM
025460         ELSE
025470              COMPUTE 計算西暦年Ｗ = 計算西暦年Ｗ + 計算年Ｗ - 1
025480              MOVE 計算月Ｗ TO 計算西暦月Ｗ
025490              MOVE 計算日Ｗ TO 計算西暦日Ｗ
025500         END-IF
025510     END-IF.
025520*
025530*================================================================*
025540 助成負担者番号取得 SECTION.
025550*
025560*--------------------------------------------------------------------------
025570* 負担者番号が ﾀﾞﾐｰ 99XXXXXXは 26XXXXXX にする。
025580* 負担者番号が数字以外で始まる(茨-)時は、26XXXXXX にする。XXXXXXは、国保番号
025590*   XXXXXXは、市町村マスタの中の 保険者番号 を使用する。(ﾏｽﾀに入力しておく)
025600*--------------------------------------------------------------------------
025610*
025620     PERFORM 助成番号左詰め.
025630*
025640     MOVE 受－助成種別       TO 市－公費種別
025650     MOVE 助成番号Ｗ         TO 市－市町村番号 保険者番号Ｗ
025660     READ 市町村マスタ
025670     INVALID KEY
025680         MOVE SPACE          TO 保険者番号Ｗ
025690         EVALUATE 受－助成種別
025700         WHEN 51
025710             MOVE "41" TO 作１－助成負担者番号(1:2)
025720         WHEN 52
025730             MOVE "83" TO 作１－助成負担者番号(1:2)
025740         WHEN 53
025750             MOVE "82" TO 作１－助成負担者番号(1:2)
025760         WHEN 54
025770             MOVE "85" TO 作１－助成負担者番号(1:2)
025780         WHEN 55
025790             MOVE "81" TO 作１－助成負担者番号(1:2)
025800         END-EVALUATE
025810         EVALUATE 受－保険種別
025820         WHEN 01
025830             MOVE 受－保険者番号      TO 作１－助成負担者番号(3:6)
025840         WHEN 08
025850             MOVE 受－保険者番号(3:6) TO 作１－助成負担者番号(3:6)
025860         END-EVALUATE
025870         
025880* ダミー番号の場合、市区町村名使用区分に１をセットする
025890         MOVE "1"     TO 作１－市区町村使用区分
025900     NOT INVALID KEY
025910         IF 市－市町村番号(1:2) = 99
025920             EVALUATE 受－助成種別
025930             WHEN 51
025940                 MOVE 市－市町村番号 TO 作１－助成負担者番号
025950                 MOVE "41"           TO 作１－助成負担者番号(1:2)
025960             WHEN 52
025970                 MOVE 市－市町村番号 TO 作１－助成負担者番号
025980                 MOVE "83"           TO 作１－助成負担者番号(1:2)
025990             WHEN 53
026000                 MOVE 市－市町村番号 TO 作１－助成負担者番号
026010                 MOVE "82"           TO 作１－助成負担者番号(1:2)
026020             WHEN 54
026030                 MOVE 市－市町村番号 TO 作１－助成負担者番号
026040                 MOVE "85"           TO 作１－助成負担者番号(1:2)
026050             WHEN 55
026060                 MOVE 市－市町村番号 TO 作１－助成負担者番号
026070                 MOVE "81"           TO 作１－助成負担者番号(1:2)
026080             END-EVALUATE
026090             
026100* ダミー番号の場合、市区町村名使用区分に１をセットする
026110             MOVE "1"     TO 作１－市区町村使用区分
026120         ELSE
026130*             MOVE 保険者番号Ｗ   TO 作１－助成負担者番号
026140             MOVE "0"     TO 作１－市区町村使用区分
026150             MOVE 市－市町村番号 TO 作１－助成負担者番号
026160         END-IF
026170     END-READ.
026180     MOVE 作１－助成負担者番号 TO 保険者番号Ｗ.
026190*
026200*================================================================*
026210 助成番号左詰め SECTION.
026220*
026230***** 助成の負担者番号の無駄なSPACEを取り除いて、左詰めにする。
026240     MOVE SPACE           TO  助成番号Ｗ.
026250     MOVE SPACE           TO  助成番号元Ｗ.
026260     MOVE SPACE           TO  助成番号左詰めＷ.
026270     MOVE 受－費用負担者番号助成   TO  助成番号元Ｗ.
026280*
026290     MOVE  ZERO  TO  カウンタ２.
026300     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 10
026310          IF  助成番号元Ｗ１(カウンタ) NOT = SPACE
026320              COMPUTE カウンタ２ = カウンタ２  +  1
026330              MOVE 助成番号元Ｗ１(カウンタ)  TO  助成番号左詰めＷ１(カウンタ２)
026340          END-IF
026350     END-PERFORM.
026360*
026370     MOVE 助成番号左詰めＷ    TO 助成番号Ｗ.
026380*
026390*================================================================*
026400 転帰区分変換 SECTION.
026410*
026420     MOVE ZERO  TO 転帰変換後Ｗ.
026430*
026440     EVALUATE 転帰変換前Ｗ
026450     WHEN  ZERO
026460        MOVE ZERO TO 転帰変換後Ｗ
026470* 治癒
026480     WHEN  1
026490     WHEN  2
026500     WHEN  5
026510        MOVE  1   TO 転帰変換後Ｗ
026520* 中止
026530     WHEN  3
026540        MOVE  2   TO 転帰変換後Ｗ
026550* 転医
026560     WHEN  4
026570        MOVE  3   TO 転帰変換後Ｗ
026580* 継続・自然治癒
026590*     WHEN  5
026600     WHEN  9
026610        MOVE  4   TO 転帰変換後Ｗ
026620     WHEN OTHER
026630        CONTINUE
026640     END-EVALUATE.
026650*
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
026770             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
026780*
026790****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
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
033730          INITIALIZE 負傷原因内容分解ＸＷ
033740          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
033750          IF ( 負傷原因内容１ＸＷ  NOT = SPACE )
033760              COMPUTE カウンタ２ = カウンタ２  +  1
033770              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
033780          END-IF
033790          IF ( 負傷原因内容２ＸＷ  NOT = SPACE )
033800              COMPUTE カウンタ２ = カウンタ２  +  1
033810              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
033820          END-IF
033830          IF ( 負傷原因内容３ＸＷ  NOT = SPACE )
033840              COMPUTE カウンタ２ = カウンタ２  +  1
033850              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
033860          END-IF
033830          IF ( 負傷原因内容４ＸＷ  NOT = SPACE )
033840              COMPUTE カウンタ２ = カウンタ２  +  1
033850              MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
033860          END-IF
033830          IF ( 負傷原因内容５ＸＷ  NOT = SPACE )
033840              COMPUTE カウンタ２ = カウンタ２  +  1
033850              MOVE 負傷原因内容５ＸＷ  TO 負傷原因Ｗ(カウンタ２)
033860          END-IF
027880*
027890     END-PERFORM.
027900*
027910*================================================================*
027920 長期理由文取得 SECTION.
027930*
027940* 長期理由文取得は "CHOUBUN" を呼ぶ. 
027950     MOVE  SPACE TO  連長文－キー.
027960     INITIALIZE      連長文－キー.
027970     MOVE 施術和暦ＷＲ  TO  連長文－施術和暦.
027980     MOVE 施術年ＷＲ    TO  連長文－施術年.
027990     MOVE 施術月ＷＲ    TO  連長文－施術月.
028000     MOVE 患者番号ＷＲ  TO  連長文－患者番号.
028010     MOVE 枝番ＷＲ      TO  連長文－枝番.
028020** 日接用は56桁
028030     MOVE 56            TO  連長文－文桁数.
028040*
028050     CALL   "CHOUBUN".
028060     CANCEL "CHOUBUN".
028070*
028080*================================================================*
028090 全角半角チェック SECTION.
028100*
028110     MOVE ZERO TO カウンタ カウンタ２.
028120     PERFORM VARYING カウンタ FROM 1 BY 1
028130             UNTIL   カウンタ > カウンタ３
028140         IF ((漢字チェックＷ(カウンタ) >= X"81") AND (漢字チェックＷ(カウンタ) <= X"9F" )) OR
028150            ((漢字チェックＷ(カウンタ) >= X"E0") AND (漢字チェックＷ(カウンタ) <= X"FC" ))
028160             COMPUTE カウンタ = カウンタ + 1
028170         ELSE
028180             COMPUTE カウンタ２ = カウンタ２ + 1
028190         END-IF
028200     END-PERFORM.
028210     DIVIDE 2 INTO カウンタ２ GIVING    商Ｗ
028220                              REMAINDER 余Ｗ
028230     END-DIVIDE.
028240     IF 余Ｗ = 1
028250         IF ((漢字チェックＷ(カウンタ３) >= X"81") AND (漢字チェックＷ(カウンタ３) <= X"9F" )) OR
028260            ((漢字チェックＷ(カウンタ３) >= X"E0") AND (漢字チェックＷ(カウンタ３) <= X"FC" ))
028270             MOVE SPACE TO 漢字チェックＷ(カウンタ３)
028280         END-IF
028290     END-IF.
028300*================================================================*
       名古屋市福祉判定 SECTION.
      *
      */名古屋市福祉用のレコードを書込む
005210     IF ((( 受－老人負担金免除      = 1     ) AND
005220         ( 受－費用負担者番号(3:3) = "234" )) OR
005240        (( 受－助成負担金免除      = 1     ) AND
005250         ( 受－費用負担者番号(3:3) = "234" )))
           OR
              (( 受－保険種別 = 05 ) AND ( 受－保険者番号(1:5) = "39231" ) AND
               ( 受－助成負担金免除 = 1 ))
      */助成の受給№が転記されない不具合の修正/090217
011140*         PERFORM 作１レコードセット老人
011090         PERFORM 作１レコードセット老人助成
      */名古屋市福祉分のレコードセット↓↓↓
014260         MOVE "101" TO 作１－助成種別
               COMPUTE 作１－負担割合 = 10 - (連率－２７老負担率 / 10)
025890         MOVE "1"                    TO 作１－市区町村使用区分
               MOVE NC"名古屋市福祉給付金" TO 作１－市区町村名
012510         MOVE レセ－合計     TO 作１－合計金額
012520         MOVE ZERO           TO 作１－一部負担金
012530         MOVE レセ－請求金額 TO 作１－請求金額
013920         MOVE NC"助成"       TO 作１－保険種別
      */名古屋市福祉分のレコードセット↑↑↑
011150         PERFORM 作１ファイル書込
           END-IF.
           IF (( 受－公費種別 = ZERO ) AND
               ( 受－助成種別 = 51 OR 60 ) AND
               ( 受－老人負担金免除 = 1 ) AND
               ( 受－費用負担者番号助成(3:3) = "234" )) AND 
               ( 作４－保険種別 = 51)
011090         PERFORM 作１レコードセット老人助成
      */名古屋市福祉分のレコードセット↓↓↓
014260         MOVE "101" TO 作１－助成種別
               COMPUTE 作１－負担割合 = 10 - 連率－助成負担率 / 10
025890         MOVE "1"                    TO 作１－市区町村使用区分
               MOVE NC"名古屋市福祉給付金" TO 作１－市区町村名
012510         MOVE レセ－合計     TO 作１－合計金額
012520         MOVE ZERO           TO 作１－一部負担金
012530         MOVE レセ－請求金額 TO 作１－請求金額
      */名古屋市福祉分のレコードセット↑↑↑
011100         PERFORM 作１ファイル書込
           END-IF.
      *
028300*================================================================*
028310******************************************************************
028320 END PROGRAM YCB101.
028330******************************************************************
