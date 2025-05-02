000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHN436.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*  ６号用紙印刷      【柔+ｳｨﾝﾄﾞｳｽﾞ版】
000100*         MED = YHN436P
000101*  請求年月バージョン
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2015-02-24
000130 DATE-COMPILED.          2015-02-24
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
000260     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY               IS  元－元号区分
000300                             FILE STATUS              IS  状態キー
000310                             LOCK        MODE         IS  AUTOMATIC.
000320     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000330                             ORGANIZATION             IS  INDEXED
000340                             ACCESS MODE              IS  DYNAMIC
000350                             RECORD KEY               IS  制－制御区分
000360                             FILE STATUS              IS  状態キー
000370                             LOCK        MODE         IS  AUTOMATIC.
000380     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000390                             ORGANIZATION             IS  INDEXED
000400                             ACCESS MODE              IS  DYNAMIC
000410                             RECORD KEY               IS  名－区分コード
000420                                                          名－名称コード
000430                             FILE STATUS              IS  状態キー
000440                             LOCK        MODE         IS  AUTOMATIC.
000450     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000460                             ORGANIZATION             IS  INDEXED
000470                             ACCESS MODE              IS  DYNAMIC
000480                             RECORD KEY               IS 施情－施術所番号
000490                             FILE STATUS              IS  状態キー
000500                             LOCK        MODE         IS  AUTOMATIC.
000510     SELECT  保険者マスタ    ASSIGN      TO        HOKENSL
000520                             ORGANIZATION             IS  INDEXED
000530                             ACCESS MODE              IS  DYNAMIC
000540                             RECORD KEY               IS  保－保険種別
000550                                                          保－保険者番号
000560                             ALTERNATE RECORD KEY     IS  保－保険種別
000570                                                          保－保険者名称
000580                                                          保－保険者番号
000590                             FILE STATUS              IS  状態キー
000600                             LOCK        MODE         IS  AUTOMATIC.
000610     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
000620                             ORGANIZATION             IS  INDEXED
000630                             ACCESS MODE              IS  DYNAMIC
000640                             RECORD KEY               IS  市－公費種別
000650                                                          市－市町村番号
000660                             ALTERNATE RECORD KEY     IS  市－公費種別
000670                                                          市－市町村名称
000680                                                          市－市町村番号
000690                             FILE STATUS              IS  状態キー
000700                             LOCK        MODE         IS  AUTOMATIC.
000701     SELECT  請求先マスタ    ASSIGN      TO        SEIKYUSL
000702                             ORGANIZATION             IS  INDEXED
000703                             ACCESS MODE              IS  DYNAMIC
000704                             RECORD KEY               IS  請先－保険種別
000705                                                          請先－保険者番号
000706                             FILE STATUS              IS  状態キー
000707                             LOCK    MODE             IS  AUTOMATIC.
000708     SELECT  会情報マスタ    ASSIGN      TO        KAIJOHOL
000709                             ORGANIZATION             IS  INDEXED
000710                             ACCESS MODE              IS  DYNAMIC
00130                             RECORD KEY               IS  会情－柔整鍼灸区分
000131                                                          会情－協会コード
000132                                                          会情－保険種別
000133                                                          会情－変更和暦年月
000134                             ALTERNATE RECORD KEY     IS  会情－柔整鍼灸区分
000135                                                          会情－接骨師会カナ
000136                                                          会情－協会コード
000137                                                          会情－保険種別
000138                                                          会情－変更和暦年月
000718                             FILE STATUS              IS  状態キー
000719                             LOCK        MODE         IS  AUTOMATIC.
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
000033     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000034                             ORGANIZATION             IS  INDEXED
000035                             ACCESS MODE              IS  DYNAMIC
000036                             RECORD KEY               IS 受－施術和暦年月
000037                                                          受－患者コード
000038                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000039                                                          受－患者カナ
000040                                                          受－患者コード
000041                             ALTERNATE RECORD KEY     IS  受－患者コード
000042                                                         受－施術和暦年月
000043                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000044                                                          受－保険種別
000045                                                          受－保険者番号
000046                                                          受－患者コード
000047                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000048                                                          受－公費種別
000049                                                     受－費用負担者番号
000050                                                          受－患者コード
000051                             ALTERNATE RECORD KEY     IS 受－施術和暦年月
000052                                                          受－助成種別
000053                                                  受－費用負担者番号助成
000054                                                          受－患者コード
000055                             ALTERNATE RECORD KEY  IS 受－請求和暦年月
000056                                                      受－施術和暦年月
000057                                                      受－患者コード
000058                             FILE STATUS              IS  状態キー
000059                             LOCK        MODE         IS  AUTOMATIC.
000060     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
000061                             ORGANIZATION             IS  INDEXED
000062                             ACCESS MODE              IS  DYNAMIC
000063                             RECORD KEY               IS  負－施術和暦年月
000064                                                          負－患者コード
000065                             ALTERNATE RECORD KEY     IS  負－患者コード
000066                                                          負－施術和暦年月
000067                             FILE STATUS              IS  状態キー
000068                             LOCK        MODE         IS  AUTOMATIC.
000069     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
000070                             ORGANIZATION             IS  INDEXED
000071                             ACCESS MODE              IS  DYNAMIC
000072                             RECORD KEY               IS  施記－施術和暦年月日
000073                                                          施記－患者コード
000074                             ALTERNATE RECORD KEY     IS  施記－患者コード
000075                                                          施記－施術和暦年月日
000076                             FILE STATUS              IS  状態キー
000077                             LOCK        MODE         IS  AUTOMATIC.
000108     SELECT  作業ファイル２  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W4312L.DAT"
000109                             ORGANIZATION             IS  INDEXED
000110                             ACCESS                   IS  DYNAMIC
000111                             RECORD      KEY          IS  作２－請求和暦年月
000112                                                          作２－分類コード
000112                                                          作２－県コード
000112                                                          作２－保険順
000113                                                          作２－保険者番号
000119                             FILE        STATUS       IS  状態キー
000120                             LOCK        MODE         IS  AUTOMATIC.
      */並び順用　６号
000108     SELECT  作業ファイル３  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W4313L.DAT"
000109                             ORGANIZATION             IS  INDEXED
000110                             ACCESS                   IS  DYNAMIC
000111                             RECORD      KEY          IS  作３－分類コード
000112                                                          作３－県コード
000112                                                          作３－保険順
000113                                                          作３－保険者番号
000980                             FILE        STATUS       IS  状態キー
000990                             LOCK        MODE         IS  AUTOMATIC.
000810     SELECT  印刷ファイル    ASSIGN      TO         GS-PRTF001
000820                             SYMBOLIC    DESTINATION  IS "PRT"
000830                             FORMAT                   IS  定義体名Ｐ
000840                             GROUP                    IS  項目群名Ｐ
000850                             PROCESSING  MODE         IS  処理種別Ｐ
000860                             UNIT        CONTROL      IS  拡張制御Ｐ
000870                             FILE        STATUS       IS  通知情報Ｐ.
000880*
000890******************************************************************
000900*                      DATA DIVISION                             *
000910******************************************************************
000920 DATA                    DIVISION.
000930 FILE                    SECTION.
000940*                           ［ＲＬ＝  １２８］
000950 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
000960     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
000970*                           ［ＲＬ＝  ２５６］
000980 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
000990     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
000991     COPY SEIGYO01        OF  XFDLIB  JOINING   制０１   AS  PREFIX.
000991     COPY SEIGYO02        OF  XFDLIB  JOINING   制０２   AS  PREFIX.
001000*                           ［ＲＬ＝  １２８］
001010 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
001020     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
001030*                           ［ＲＬ＝  １２８］
001040 FD  施術所情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001050     COPY SEJOHO          OF  XFDLIB  JOINING   施情   AS  PREFIX.
001060*                           ［ＲＬ＝  ３２０］
001070 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001080     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
001090*                           ［ＲＬ＝  ２５６］
001100 FD  市町村マスタ          BLOCK   CONTAINS   1   RECORDS.
001110     COPY SITYOSN        OF  XFDLIB  JOINING   市   AS  PREFIX.
001111*                           ［ＲＬ＝  １２８］
001112 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
001113     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
001114*                           ［ＲＬ＝  ６４０］
001115 FD  会情報マスタ        BLOCK   CONTAINS   1   RECORDS.
001116     COPY KAIJOHO         OF  XFDLIB  JOINING   会情 AS  PREFIX.
      *
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS GLOBAL.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
000129*                           ［ＲＬ＝  ３２０］
000130 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
000131     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
000132*                           ［ＲＬ＝  ２５６］
000133 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
000134     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
000135*                           ［ＲＬ＝  １２８］
000136 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
000137     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
001120*
000174*                           ［ＲＬ＝  １２８］
000175 FD  作業ファイル２ RECORD  CONTAINS 128 CHARACTERS.
000176 01  作２－レコード.
000177     03  作２－レコードキー.
000178         05  作２－請求和暦年月.
000179             07  作２－請求和暦              PIC 9.
000180             07  作２－請求年                PIC 9(2).
000181             07  作２－請求月                PIC 9(2).
001261         05  作２－分類コード                PIC 9(1).
001261         05  作２－県コード                  PIC X(2).
001400         05  作２－保険順                    PIC 9(2).
000183         05  作２－保険者番号                PIC X(10).
000188     03  作２－レコードデータ.
001400         05  作２－保険種別                  PIC 9(2).
000189         05  作２－件数                      PIC 9(4).
000190         05  作２－費用額                    PIC 9(9).
000191         05  作２－負担額                    PIC 9(9).
000192         05  作２－請求額                    PIC 9(9).
000193         05  作２－本人件数                  PIC 9(3).
000194         05  作２－本人費用額                PIC 9(7).
000195         05  作２－本人負担額                PIC 9(7).
000196         05  作２－本人請求額                PIC 9(7).
000193         05  作２－本人実日数                PIC 9(4).
000197         05  作２－家族件数                  PIC 9(3).
000198         05  作２－家族費用額                PIC 9(7).
000199         05  作２－家族負担額                PIC 9(7).
000200         05  作２－家族請求額                PIC 9(7).
000197         05  作２－家族実日数                PIC 9(4).
000201         05  FILLER                          PIC X(19).
000173*
000174*                           ［ＲＬ＝  ３２］
000175 FD  作業ファイル３ RECORD  CONTAINS 32 CHARACTERS.
000176 01  作３－レコード.
000177     03  作３－レコードキー.
001261         05  作３－分類コード                PIC 9(1).
001261         05  作３－県コード                  PIC X(2).
001400         05  作３－保険順                    PIC 9(2).
000183         05  作３－保険者番号                PIC X(10).
000188     03  作３－レコードデータ.
001261         05  作３－６号順番                  PIC 9(3).
000201         05  FILLER                          PIC X(13).
001350* 
001360 FD  印刷ファイル.
001370     COPY YHN436P         OF  XMDLIB.
001371*
001380******************************************************************
001390*                WORKING-STORAGE SECTION                         *
001400******************************************************************
001410 WORKING-STORAGE         SECTION.
001420 01 キー入力                           PIC X    VALUE SPACE.
001430 01 状態キー                           PIC X(2) VALUE SPACE.
001440 01 終了フラグ                         PIC X(3) VALUE SPACE.
001450 01 終了フラグ２                       PIC X(3) VALUE SPACE.
001450 01 終了フラグ３                       PIC X(3) VALUE SPACE.
001460 01 確認入力Ｗ                         PIC X(1) VALUE SPACE.
001470 01 ファイル名Ｗ                       PIC N(6) VALUE SPACE.
001480 01 カレント元号Ｗ                     PIC 9(1) VALUE ZERO.
001490 01 実行キーＷ                         PIC X(4) VALUE SPACE.
001304 01 施術記録有Ｗ                       PIC X(3)  VALUE SPACE.
001500 01 地区Ｗ                             PIC X(2) VALUE SPACE.
001510 01 公費番号Ｗ                         PIC X(8) VALUE SPACE.
001560 01 前和暦Ｗ                           PIC 9    VALUE ZERO.
001570 01 行桁フラグ                         PIC X(3) VALUE SPACE.
001580 01 検索対象フラグ                     PIC X(4) VALUE SPACE.
001590 01 ファイル名                         PIC N(4) VALUE SPACE.
001600 01 処理移動キー                       PIC X(4) VALUE SPACE.
001610 01 検索確認区分Ｗ                     PIC 9    VALUE ZERO.
001620 01 検索通過キーＷ                     PIC X(3) VALUE SPACE.
001630 01 行カウンタ                         PIC 9(2) VALUE ZERO.
001640 01 合計行カウンタ                     PIC 9(3) VALUE ZERO.
001650 01 ページカウンタ                     PIC 9(3) VALUE ZERO.
001651 01 印刷フラグ                         PIC X(3)  VALUE SPACE.
001652 01 スキップフラグ                     PIC X(3)  VALUE SPACE.
001210 01 オープンフラグ                     PIC X(3)   VALUE SPACE.
001660*
001661 01 合計用Ｗ.
001662    03 本人件数カウント                PIC 9(9) VALUE ZERO.
001670    03 家族件数カウント                PIC 9(9) VALUE ZERO.
001680    03 計件数カウント                  PIC 9(9) VALUE ZERO.
001690    03 本人費用額カウント              PIC 9(9) VALUE ZERO.
001700    03 家族費用額カウント              PIC 9(9) VALUE ZERO.
001710    03 計費用額カウント                PIC 9(9) VALUE ZERO.
001720    03 本人請求額カウント              PIC 9(9) VALUE ZERO.
001730    03 家族請求額カウント              PIC 9(9) VALUE ZERO.
001740    03 計請求額カウント                PIC 9(9) VALUE ZERO.
001770 01 県Ｗ                               PIC X(2) VALUE SPACE.
001771 01 印刷順序Ｗ                         PIC 9(2) VALUE ZERO.
001772*
001773 01 保険者名称Ｗ.
001774    03 保険者名称１Ｗ                  PIC X(20) VALUE SPACE.
001775    03 保険者名称２Ｗ                  PIC X(20) VALUE SPACE.
001776    03 保険者名称３Ｗ                  PIC X(20) VALUE SPACE.
001777*
001780 01 件数ＷＫ                           PIC 9(9) VALUE ZERO.
001790 01 費用ＷＫ                           PIC 9(9) VALUE ZERO.
001800 01 請求ＷＫ                           PIC 9(9) VALUE ZERO.
001801 01 全角空白                           PIC X(2)  VALUE X"8140".
001802 01 半角空白                           PIC X(2)  VALUE X"2020".
001810*
001820 01 施術和暦年月Ｗ.
001830     03 施術和暦Ｗ                     PIC 9    VALUE ZERO.
001840     03 施術年月Ｗ.
001850        05 施術年Ｗ                    PIC 9(2) VALUE ZERO.
001860        05 施術月Ｗ                    PIC 9(2) VALUE ZERO.
001870        05 施術日Ｗ                    PIC 9(2) VALUE ZERO.
001880     03 受理年月Ｗ.
001890        05 受理年Ｗ                    PIC 9(2) VALUE ZERO.
001900        05 受理月Ｗ                    PIC 9(2) VALUE ZERO.
001910        05 受理日Ｗ                    PIC 9(2) VALUE ZERO.
001920***
001921***
001922 01 画面情報４３０Ｗ.
001923    03 請求年月Ｗ.
001924       05 請求和暦Ｗ                   PIC 9     VALUE ZERO.
001925       05 請求年Ｗ                     PIC 9(2)  VALUE ZERO.
001926       05 請求月Ｗ                     PIC 9(2)  VALUE ZERO.
001927    03 提出年月日Ｗ.
001928       05 提出和暦Ｗ                   PIC 9     VALUE ZERO.
001929       05 提出年Ｗ                     PIC 9(2)  VALUE ZERO.
001930       05 提出月Ｗ                     PIC 9(2)  VALUE ZERO.
001931       05 提出日Ｗ                     PIC 9(2)  VALUE ZERO.
001932    03 印刷種類Ｗ                      PIC 9     VALUE ZERO.
001933*
001934**************
001935* 施術所情報 *
001936**************
001937 01 施術所情報Ｗ.
001938    03 代表者カナＷ                    PIC X(50)  VALUE SPACE.
001938    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
001939    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
001940    03 柔整師番号Ｗ                    PIC X(20)  VALUE SPACE.
001941    03 施術所住所Ｗ.
001942       05 施術所住所１Ｗ               PIC X(40)  VALUE SPACE.
001943       05 施術所住所２Ｗ               PIC X(40)  VALUE SPACE.
001944    03 施術所郵便番号Ｗ.
001945       05 施術所郵便番号記号Ｗ         PIC X(2)   VALUE SPACE.
001945       05 施術所郵便番号１Ｗ           PIC X(3)   VALUE SPACE.
001946       05 施術所郵便番号区切Ｗ         PIC X(1)   VALUE SPACE.
001947       05 施術所郵便番号２Ｗ           PIC X(4)   VALUE SPACE.
001948    03 施術所電話番号Ｗ                PIC X(15)  VALUE SPACE.
001949    03 取引先情報Ｗ.
001950        05 取引先銀行名Ｗ              PIC X(40)  VALUE SPACE.
001951        05 取引先銀行支店名Ｗ          PIC X(40)  VALUE SPACE.
001952        05 預金種別Ｗ                  PIC 9(1)   VALUE ZERO.
001953        05 銀行番号Ｗ                  PIC X(4)   VALUE SPACE.
001954        05 店番号Ｗ                    PIC X(3)   VALUE SPACE.
001955        05 口座番号Ｗ                  PIC X(10)  VALUE SPACE.
001956        05 口座名義人カナＷ            PIC X(50)  VALUE SPACE.
001957        05 口座名義人Ｗ                PIC X(50)  VALUE SPACE.
001958*
001959 01 連番Ｗ                             PIC 9(3)   VALUE ZERO.
001960 01 銀行名支店名Ｗ                     PIC X(60)  VALUE SPACE.
001961 01 預金種別コメントＷ                 PIC X(4)   VALUE SPACE.
001962 01 控えＷ                             PIC N(4)   VALUE SPACE.
001963*
001964 01 保険種別Ｗ                         PIC 9(2)  VALUE ZERO.
001965 01 保険者番号Ｗ                       PIC X(10) VALUE SPACE.
001966 01 請求先名称Ｗ                       PIC X(40) VALUE SPACE.
001967 01 支部部署名Ｗ                       PIC X(40) VALUE SPACE.
001968 01 宛名Ｗ                             PIC X(24) VALUE SPACE.
001969 01 保険者宛名Ｗ.
001970     03 保険者宛名１Ｗ                 PIC X(40) VALUE SPACE.
001971     03 保険者宛名２Ｗ                 PIC X(40) VALUE SPACE.
      *
001220 01 分類コードＷＲ                     PIC 9(1) VALUE ZERO.
001220 01 県コードＷＲ                       PIC X(2) VALUE SPACE.
001220 01 保険順ＷＲ                         PIC 9(2) VALUE ZERO.
001220 01 府県Ｗ                             PIC X(2) VALUE SPACE.
001220 01 県名Ｗ.
          03 県名ＷＰ                        PIC X(8) VALUE SPACE.
001972*
001973* 社保用
001974 01 接尾語区分Ｗ                       PIC 9     VALUE ZERO.
001975*
001976 01 協会コードＷ                       PIC 9(2)  VALUE ZERO.
001977***
001978* 請求書印刷パラメタ用
001979*  ６・７号請求書（印刷区分 0:印刷 1:印刷しない、振込先 0:自分 1:会 9:印刷しない）
001980*  当社用紙初期値1:印刷しない
001981***
001982 01 請求書関連Ｗ.
001983        07 国保６号印刷区分Ｗ          PIC 9 VALUE ZERO.
001984        07 国保７号印刷区分Ｗ          PIC 9 VALUE ZERO.
001985        07 国保当社印刷区分Ｗ          PIC 9 VALUE 1.
001986        07 国保振込先区分Ｗ            PIC 9 VALUE ZERO.
001987        07 社保６号印刷区分Ｗ          PIC 9 VALUE ZERO.
001988        07 社保７号印刷区分Ｗ          PIC 9 VALUE ZERO.
001989        07 社保当社印刷区分Ｗ          PIC 9 VALUE 1.
001990        07 社保振込先区分Ｗ            PIC 9 VALUE ZERO.
001991        07 組合６号印刷区分Ｗ          PIC 9 VALUE ZERO.
001992        07 組合７号印刷区分Ｗ          PIC 9 VALUE ZERO.
001993        07 組合当社印刷区分Ｗ          PIC 9 VALUE 1.
001994        07 組合振込先区分Ｗ            PIC 9 VALUE ZERO.
001995        07 共済６号印刷区分Ｗ          PIC 9 VALUE ZERO.
001996        07 共済７号印刷区分Ｗ          PIC 9 VALUE ZERO.
001997        07 共済当社印刷区分Ｗ          PIC 9 VALUE 1.
001998        07 共済振込先区分Ｗ            PIC 9 VALUE ZERO.
001999        07 老人６号印刷区分Ｗ          PIC 9 VALUE ZERO.
002000        07 老人７号印刷区分Ｗ          PIC 9 VALUE ZERO.
002001        07 老人当社印刷区分Ｗ          PIC 9 VALUE 1.
002002        07 老人振込先区分Ｗ            PIC 9 VALUE ZERO.
002003        07 助成６号印刷区分Ｗ          PIC 9 VALUE ZERO.
002004        07 助成７号印刷区分Ｗ          PIC 9 VALUE ZERO.
002005        07 助成当社印刷区分Ｗ          PIC 9 VALUE 1.
002006        07 助成振込先区分Ｗ            PIC 9 VALUE ZERO.
002862        07 ６７並び順区分Ｗ            PIC 9 VALUE ZERO.
002877*
002862        07 北海道区分Ｗ                PIC 9 VALUE ZERO.
002862        07 青森区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 岩手区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 宮城区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 秋田区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 山形区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 福島区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 茨城区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 栃木区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 群馬区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 埼玉区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 千葉区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 東京区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 神奈川区分Ｗ                PIC 9 VALUE ZERO.
002862        07 新潟区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 富山区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 石川区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 福井区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 山梨区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 長野区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 岐阜区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 静岡区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 愛知区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 三重区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 滋賀区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 京都区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 大阪区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 兵庫区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 奈良区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 和歌山区分Ｗ                PIC 9 VALUE ZERO.
002862        07 鳥取区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 島根区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 岡山区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 広島区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 山口区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 徳島区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 香川区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 愛媛区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 高知区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 福岡区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 佐賀区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 長崎区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 熊本区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 大分区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 宮崎区分Ｗ                  PIC 9 VALUE ZERO.
002862        07 鹿児島区分Ｗ                PIC 9 VALUE ZERO.
002862        07 沖縄区分Ｗ                  PIC 9 VALUE ZERO.
002007*
002008*********************************************************************
002009 01 印刷制御.
002010     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
002011     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
002012     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
002013     03 拡張制御Ｐ.
002014         05 端末制御Ｐ.
002015             07 移動方向Ｐ             PIC X(1) VALUE SPACE.
002016             07 移動行数Ｐ             PIC 9(3) VALUE ZERO.
002020         05 詳細制御Ｐ                 PIC X(2) VALUE SPACE.
002030     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
002040     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
002050*********************************************************************
002060*
002070 01 計算機西暦年Ｗ                     PIC 9(2).
002080* 日付ＷＯＲＫ
002090 01 和暦終了年Ｗ                       PIC 9(4).
002100 01 計算機和暦年Ｗ                     PIC 9(2).
002110 01 計算機西暦.
002120    03 計算機西暦年                    PIC 9(4).
002130    03 計算機西暦月日                  PIC 9(4).
002140 01 計算機西暦Ｒ REDEFINES 計算機西暦.
002150    03 計算機世紀                      PIC 9(2).
002160    03 計算機日付                      PIC 9(6).
002170    03 計算機日付Ｒ REDEFINES 計算機日付.
002180       05 計算機年月                   PIC 9(4).
002190       05 計算機年月Ｒ REDEFINES 計算機年月.
002200         07 計算機年                   PIC 9(2).
002210         07 計算機月                   PIC 9(2).
002220       05 計算機日                     PIC 9(2).
002230*
       01 遅延フラグ                         PIC X(3) VALUE SPACE.
       01 遅延ＣＮＴ                         PIC 9(5) VALUE ZERO.
       01 遅延カウンタ                       PIC 9(4) VALUE ZERO.
       01 遅延回数Ｗ                         PIC 9(4) VALUE ZERO.
002240******************************************************************
002250*                          連結項目                              *
002260******************************************************************
002270*
002280********************
002290* メッセージ表示キー *
002300********************
002310 01 連メ－キー IS EXTERNAL.
002320    03  連メ－メッセージ                 PIC N(20).
002330*
001591 01 連入－画面情報ＹＨＮ４３０   IS EXTERNAL.
002333    03 連入－請求年月.
002334       05 連入－請求和暦               PIC 9.
002335       05 連入－請求年                 PIC 9(2).
002336       05 連入－請求月                 PIC 9(2).
002337    03 連入－提出年月日.
002338       05 連入－提出和暦               PIC 9.
002339       05 連入－提出年                 PIC 9(2).
002340       05 連入－提出月                 PIC 9(2).
002341       05 連入－提出日                 PIC 9(2).
002342    03 連入－レセプト種類              PIC X(4).
002343    03 連入－保険種別                  PIC 9(2).
002344    03 連入－印刷種類                  PIC 9.
002345    03 連入－本人家族                  PIC 9.
002346    03 連入－用紙種類                  PIC 9.
002347    03 連入－県内県外                  PIC 9.
002348    03 連入－県ＪＩＳ                  PIC X(2).
002349    03 連入－政管ＪＩＳ                PIC X(2).
002351*
002352 01 連入－画面情報ＹＨＮ４３０追加   IS EXTERNAL.
002353    03 連入－一括区分    PIC 9.
001933    03 連入－作成日印刷  PIC 9.
          03 連入－プレビュー区分            PIC 9.
          03 連入－件数                      PIC 9(5).
          03 連入－処理モード                PIC X(2).
      *
       01 連印－印刷情報ＹＨＮ４３０   IS EXTERNAL.
          03 連印－分類コード      PIC 9(1).
          03 連印－県コード        PIC X(2).
          03 連印－保険順          PIC 9(2).
          03 連印－保険者番号      PIC X(10).
002354*
000540************************************
000550* プリンタファイル作成用           *
000560************************************
000570 01 Ｈ連ＰＲＴＦ－作成データ IS EXTERNAL.
000580     03 Ｈ連ＰＲＴＦ－ファイル名           PIC X(8).
000590     03 Ｈ連ＰＲＴＦ－プレビュー区分       PIC 9.
000600     03 Ｈ連ＰＲＴＦ－帳票プログラム名     PIC X(8).
000610     03 Ｈ連ＰＲＴＦ－オーバレイ名         PIC X(8).
001772*
002355******************************************************************
002356*                      PROCEDURE  DIVISION                       *
002360******************************************************************
002370 PROCEDURE               DIVISION.
002380************
002390*           *
002400* 初期処理   *
002410*           *
002420************
002570     PERFORM プリンタファイル作成.
002430     PERFORM 初期化.
002431     PERFORM 制御情報取得２.
002440************
002450*           *
002460* 主処理     *
002470*           *
002480************
           IF 連印－分類コード = ZERO
002484        PERFORM 印刷処理
           ELSE
002484        PERFORM 印刷処理１
           END-IF.
002500************
002510*           *
002520* 終了処理   *
002530*           *
002540************
002550     PERFORM 終了処理.
002560     EXIT PROGRAM.
002570*
002580*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002231     MOVE "PRTF001"             TO Ｈ連ＰＲＴＦ－ファイル名.
002972*
002973*   使用する帳票プログラム名セット
002974     MOVE "YHN436"              TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分 TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
002590*================================================================*
002600 初期化 SECTION.
002610*
002620     OPEN INPUT 元号マスタ.
002630             MOVE NC"元号" TO ファイル名Ｗ.
002640             PERFORM オープンチェック.
002650     OPEN INPUT 制御情報マスタ.
002660             MOVE NC"制御" TO ファイル名Ｗ.
002670             PERFORM オープンチェック.
002680     OPEN INPUT 名称マスタ.
002690             MOVE NC"名称" TO ファイル名Ｗ.
002700             PERFORM オープンチェック.
002710     OPEN INPUT 施術所情報マスタ
002720             MOVE NC"施情" TO ファイル名Ｗ.
002730             PERFORM オープンチェック.
002740     OPEN INPUT 保険者マスタ
002750             MOVE NC"保険者" TO ファイル名.
002760             PERFORM オープンチェック.
002770     OPEN INPUT 市町村マスタ
002780             MOVE NC"市町村" TO ファイル名Ｗ.
002790             PERFORM オープンチェック.
002791     OPEN INPUT 請求先マスタ
002792             MOVE NC"請求先" TO ファイル名Ｗ.
002793             PERFORM オープンチェック.
002794     OPEN INPUT   会情報マスタ
002795             MOVE NC"会情報" TO ファイル名Ｗ.
002796             PERFORM オープンチェック.
002800     OPEN INPUT 作業ファイル２.
002810             MOVE NC"作２" TO ファイル名Ｗ.
002820             PERFORM オープンチェック.
002800     OPEN INPUT 作業ファイル３.
002810             MOVE NC"作３" TO ファイル名Ｗ.
002820             PERFORM オープンチェック.
003250     OPEN INPUT レセプトＦ.
003260         MOVE NC"レセプトＦ" TO ファイル名.
003270         PERFORM オープンチェック.
002590     OPEN INPUT 受診者情報Ｆ.
002600         MOVE NC"受診者情報Ｆ" TO ファイル名Ｗ.
002610         PERFORM オープンチェック.
002611     OPEN INPUT 負傷データＦ.
002612         MOVE NC"負傷データＦ" TO ファイル名.
002613         PERFORM オープンチェック.
002614     OPEN INPUT 施術記録Ｆ.
002615         MOVE NC"施術記録Ｆ" TO ファイル名.
002616         PERFORM オープンチェック.
002830*
002840*
002850*    /* 現在日付取得 */
002860     ACCEPT 計算機日付 FROM DATE.
002870*    /* 1980～2079年の間で設定 */
002880     IF 計算機年 > 80
002890         MOVE 19 TO 計算機世紀
002900     ELSE
002910         MOVE 20 TO 計算機世紀
002920     END-IF.
002930*
002940     PERFORM カレント元号取得.
002950     PERFORM 和暦終了年取得.
002960     COMPUTE 計算機和暦年Ｗ = 計算機西暦年 - 和暦終了年Ｗ.
002970     MOVE 計算機和暦年Ｗ TO 施術年Ｗ.
002980     MOVE 計算機月       TO 施術月Ｗ.
002990     MOVE 計算機日       TO 施術日Ｗ.
003000*
003001     PERFORM 連結項目退避.
003002     PERFORM 施術所情報取得.
003003*
003010*================================================================*
003020 オープンチェック SECTION.
003030*
003040     IF 状態キー  NOT =  "00"
003050         DISPLAY ファイル名Ｗ NC"Ｆオープンエラー" UPON CONS
003060         DISPLAY NC"状態キー：" 状態キー           UPON CONS
003070         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
003080                                                   UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
003090         ACCEPT  キー入力 FROM CONS
003100         PERFORM ファイル閉鎖
003110         EXIT PROGRAM.
003120*================================================================*
003130 カレント元号取得 SECTION.
003140*
003150     MOVE ZEROS TO 制－制御区分.
003160     READ 制御情報マスタ
003170     NOT INVALID KEY
003180         MOVE 制－カレント元号 TO カレント元号Ｗ
003181         MOVE 制－協会コード   TO 協会コードＷ
               MOVE 制－遅延回数     TO 遅延回数Ｗ
003190     END-READ.
003200*
003210*================================================================*
003220 和暦終了年取得 SECTION.
003230*
003240*     DISPLAY NC"カレント元号Ｗ"  カレント元号Ｗ UPON MSGBOX.
003250     MOVE カレント元号Ｗ TO 元－元号区分.
003260     READ 元号マスタ
003270     INVALID KEY
003280         DISPLAY NC"指定和暦が登録されていません" UPON CONS
003290         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
003300                                                  UPON CONS
003310         ACCEPT  キー入力 FROM CONS
003320         PERFORM 終了処理
003330         EXIT PROGRAM
003340     NOT INVALID KEY
003350         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
003360         MOVE 前和暦Ｗ TO 元－元号区分
003370         READ 元号マスタ
003380         INVALID KEY
003390             DISPLAY NC"指定和暦が登録されていません" UPON CONS
003400             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
003410                                                      UPON CONS
003420             ACCEPT  キー入力 FROM CONS
003430             PERFORM 終了処理
003440             EXIT PROGRAM
003450         NOT INVALID KEY
003460             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
003470         END-READ
003480     END-READ.
003490*
003491*================================================================*
003492 連結項目退避 SECTION.
003493*
003494     MOVE 連入－請求和暦  TO 請求和暦Ｗ.
003495     MOVE 連入－請求年    TO 請求年Ｗ.
003496     MOVE 連入－請求月    TO 請求月Ｗ.
003497     MOVE 連入－提出和暦  TO 提出和暦Ｗ.
003498     MOVE 連入－提出年    TO 提出年Ｗ.
003499     MOVE 連入－提出月    TO 提出月Ｗ.
003500     MOVE 連入－提出日    TO 提出日Ｗ.
003501     MOVE 連入－印刷種類  TO 印刷種類Ｗ.
003502*
003503*================================================================*
003504 施術所情報取得 SECTION.
003505*
003506     MOVE ZERO  TO 施情－施術所番号.
003507     READ 施術所情報マスタ
003508     INVALID KEY
003509         CONTINUE
003510     NOT INVALID KEY
003511*
003513         MOVE "〒"                   TO 施術所郵便番号記号Ｗ
003512         MOVE 施情－郵便番号１       TO 施術所郵便番号１Ｗ
003513         MOVE "-"                    TO 施術所郵便番号区切Ｗ
003514         MOVE 施情－郵便番号２       TO 施術所郵便番号２Ｗ
003515         MOVE 施情－代表者カナ       TO 代表者カナＷ
003515         MOVE 施情－代表者名         TO 代表者名Ｗ
003516         MOVE 施情－接骨院名         TO 接骨院名Ｗ
003516*         MOVE 施情－住所１           TO 施術所住所１Ｗ
003516*         MOVE 施情－住所２           TO 施術所住所２Ｗ
003517         STRING 施情－住所１  DELIMITED BY SPACE
003518                施情－住所２  DELIMITED BY SPACE
003519           INTO 施術所住所Ｗ
003520         END-STRING
003521         MOVE 施情－電話番号         TO 施術所電話番号Ｗ
003522         MOVE 施情－新柔整師番号     TO 柔整師番号Ｗ
003523*
003524         MOVE 施情－取引先銀行名     TO 取引先銀行名Ｗ
003525         MOVE 施情－取引先銀行支店名 TO 取引先銀行支店名Ｗ
003526         MOVE 施情－預金種別         TO 預金種別Ｗ
003527         MOVE 施情－銀行番号         TO 銀行番号Ｗ
003528         MOVE 施情－店番号           TO 店番号Ｗ
003529         MOVE 施情－口座番号         TO 口座番号Ｗ
003530         MOVE 施情－口座名義人カナ   TO 口座名義人カナＷ
003531         MOVE 施情－口座名義人       TO 口座名義人Ｗ
023500** 振込先情報  / 会情報マスタより振込先情報を取得 /
023520         MOVE ZERO  TO  会情－柔整鍼灸区分
023510         MOVE 44    TO  会情－協会コード
023520         MOVE ZERO  TO  会情－保険種別
023530         MOVE ZERO  TO  会情－変更和暦年月
023540         READ 会情報マスタ
023550         NOT INVALID KEY
023560             MOVE 会情－取引先銀行名      TO 取引先銀行名Ｗ
023570             MOVE 会情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
023580             MOVE 会情－預金種別          TO 預金種別Ｗ
023590             MOVE 会情－口座番号          TO 口座番号Ｗ
023600             MOVE 会情－口座名義人        TO 口座名義人Ｗ
023610             MOVE 会情－口座名義人カナ    TO 口座名義人カナＷ
               END-READ
003537         EVALUATE 預金種別Ｗ
003538         WHEN 1
003539             MOVE "普通"   TO 預金種別コメントＷ
003540         WHEN 2
003541             MOVE "当座"   TO 預金種別コメントＷ
003542         WHEN OTHER
003543             MOVE SPACE    TO 預金種別コメントＷ
003544         END-EVALUATE
003532         STRING 取引先銀行名Ｗ     DELIMITED BY SPACE
003533                " "                DELIMITED BY SIZE
003534                取引先銀行支店名Ｗ DELIMITED BY SPACE
003533                " "                DELIMITED BY SIZE
003534                預金種別コメントＷ DELIMITED BY SPACE
003533                " "                DELIMITED BY SIZE
003534                口座番号Ｗ         DELIMITED BY SPACE
003535                INTO 銀行名支店名Ｗ
003536         END-STRING
003545*
003546     END-READ.
003547*================================================================*
003548 ファイル閉鎖 SECTION.
004610*
002990     IF ( オープンフラグ = "YES" )
002991         CLOSE 印刷ファイル
003041     END-IF.
003549*
003550     CLOSE 元号マスタ   制御情報マスタ   作業ファイル２
003551           名称マスタ   施術所情報マスタ 市町村マスタ 
003552           保険者マスタ 請求先マスタ     会情報マスタ
                 受診者情報Ｆ 負傷データＦ     施術記録Ｆ  レセプトＦ
                 作業ファイル３.
003553*================================================================*
003560 終了処理 SECTION.
003570*
003580     PERFORM ファイル閉鎖.
003590*================================================================*
003600*================================================================*
003610 エラー表示 SECTION.
003620*
003630     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
003640     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
003650     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
003660     ACCEPT  キー入力 FROM CONS.
003670*================================================================*
003680 エラー表示Ｒ SECTION.
003690*
003700     DISPLAY NC"ファイル読込エラー" ファイル名     UPON CONS.
003710     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
003720     ACCEPT  キー入力 FROM CONS.
003730*================================================================*
003740 作業ファイル２読込 SECTION.
003750*
003760     READ 作業ファイル２ NEXT
003770     AT END
003780         MOVE "YES" TO 終了フラグ
003790     END-READ.
003800*================================================================*
003810 印刷処理 SECTION.
003820*
           MOVE 1         TO ページカウンタ.
      * / 並び順変更
003853     MOVE ZERO      TO  作２－請求和暦.
003854     MOVE ZERO      TO  作２－請求年.
003855     MOVE ZERO      TO  作２－請求月.
003857     MOVE ZERO      TO  作２－分類コード.
003856     MOVE LOW-VALUE TO  作２－県コード.
003858     MOVE ZERO      TO  作２－保険順.
003859     MOVE LOW-VALUE TO  作２－保険者番号.
003860     START 作業ファイル２   KEY IS >=  作２－請求和暦年月
003861                                       作２－分類コード
003862                                       作２－県コード
003863                                       作２－保険順
003864                                       作２－保険者番号
003865     END-START.
003866     IF 状態キー = "00"
003867         MOVE SPACE TO 終了フラグ
003868         PERFORM 作業ファイル２読込
003869         IF  終了フラグ = "YES"
003870             MOVE  NC"　該当データなしです。確認して下さい。" TO 連メ－メッセージ
003871             CALL   "MSG001"
003872             CANCEL "MSG001"
003873             PERFORM ファイル閉鎖
003874             MOVE 99 TO PROGRAM-STATUS
003875             EXIT PROGRAM
003876         END-IF
003877*
003878         PERFORM UNTIL 終了フラグ = "YES"
003879           MOVE SPACE TO スキップフラグ
003880           PERFORM 印刷対象チェック
003881           IF 印刷フラグ = "YES"
003882*
003883             MOVE SPACE TO YHN436P
003884             INITIALIZE    YHN436P
003885             MOVE 作２－分類コード  TO 分類コードＷＲ
003886             MOVE 作２－県コード    TO 県コードＷＲ
003886             MOVE 作２－保険順      TO 保険順ＷＲ
003887             PERFORM  表セット１
003888* 
003889             PERFORM VARYING 行カウンタ FROM 1 BY 1
003890                     UNTIL ( 行カウンタ > 8       ) OR
003891                           ( 分類コードＷＲ   NOT = 作２－分類コード ) OR
003892                           ( 県コードＷＲ     NOT = 作２－県コード ) OR
                                 ((分類コードＷＲ = 5) AND (作２－保険順 NOT = 保険順ＷＲ)) OR
003893                           ( 終了フラグ = "YES" )
003894                  PERFORM 印刷対象チェック
003895                  IF 印刷フラグ = "YES"
003897                     PERFORM 表セット２
                           PERFORM フッタセット
      */↓↓改頁後、レコードがあり、印刷対象がない場合、合計が印刷されない/0510
003898*                  ELSE
003899*                     COMPUTE 行カウンタ =  行カウンタ - 1
003900                  END-IF
003901                  PERFORM 作業ファイル２読込
                        PERFORM 印刷対象チェック
                        IF 印刷フラグ = SPACE
                           COMPUTE 行カウンタ =  行カウンタ - 1
                        END-IF
      */↑↑改頁後、レコードがあり、印刷対象がない場合、合計が印刷されない/0510
003902                  MOVE "YES" TO スキップフラグ
003903             END-PERFORM
003904*
003905             IF ( 終了フラグ =  "YES" ) OR 
003906                ( 分類コードＷＲ NOT = 作２－分類コード ) OR
003907                ( 県コードＷＲ   NOT = 作２－県コード ) OR
                      ((作２－分類コード = 5) AND (作２－保険順 NOT = 保険順ＷＲ))
003908                 MOVE 本人件数カウント   TO     本人件数合計
003909                 MOVE 家族件数カウント   TO     家族件数合計
003910                 MOVE 計件数カウント     TO     計件数合計
003911                 MOVE 本人費用額カウント TO     本人費用額合計
003912                 MOVE 家族費用額カウント TO     家族費用額合計
003913                 MOVE 計費用額カウント   TO     計費用額合計
003914                 MOVE 本人請求額カウント TO     本人請求額合計
003915                 MOVE 家族請求額カウント TO     家族請求額合計
003916                 MOVE 計請求額カウント   TO     計請求額合計
003917                 IF 本人請求額カウント NOT = ZERO
003918                    MOVE "(" TO 本人左括弧合計
003919                    MOVE ")" TO 本人右括弧合計
003920                 ELSE
003921                    MOVE SPACE TO 本人左括弧合計
003922                    MOVE SPACE TO 本人右括弧合計
003923                 END-IF
003924                 IF 家族請求額カウント NOT = ZERO
003925                    MOVE "(" TO 家族左括弧合計
003926                    MOVE ")" TO 家族右括弧合計
003927                 ELSE
003928                    MOVE SPACE TO 家族左括弧合計
003929                    MOVE SPACE TO 家族右括弧合計
003930                 END-IF
003931                 IF 計請求額カウント NOT = ZERO
003932                    MOVE "(" TO 計左括弧合計
003933                    MOVE ")" TO 計右括弧合計
003934                 ELSE
003935                    MOVE SPACE TO 計左括弧合計
003936                    MOVE SPACE TO 計右括弧合計
003937                 END-IF
003938*
003939                 IF ページカウンタ > 1
                           MOVE ページカウンタ TO 頁
                       END-IF
                       MOVE 1                  TO ページカウンタ
                       INITIALIZE 合計用Ｗ
003940             ELSE
003941                 MOVE ZERO     TO     本人件数合計
003942                 MOVE ZERO     TO     家族件数合計
003943                 MOVE ZERO     TO     計件数合計
003944                 MOVE ZERO     TO     本人費用額合計
003945                 MOVE ZERO     TO     家族費用額合計
003946                 MOVE ZERO     TO     計費用額合計
003947                 MOVE ZERO     TO     本人請求額合計
003948                 MOVE ZERO     TO     家族請求額合計
003949                 MOVE ZERO     TO     計請求額合計
                       MOVE ページカウンタ TO 頁
                       COMPUTE ページカウンタ = ページカウンタ + 1
003950             END-IF
003953*
003954             PERFORM 印字処理
003955             PERFORM 改頁処理
003956*
003960           END-IF
003961*
003963           IF スキップフラグ NOT = "YES"
003964              PERFORM 作業ファイル２読込
003965           END-IF
003966         END-PERFORM
003967*
003968     ELSE
003969         MOVE  NC"　該当データなしです。確認して下さい。" TO 連メ－メッセージ
003970         CALL   "MSG001"
003971         CANCEL "MSG001"
003972         PERFORM ファイル閉鎖
003973         MOVE 99 TO PROGRAM-STATUS
003974         EXIT PROGRAM
003975     END-IF.
004621*
003800*================================================================*
003810 印刷処理１ SECTION.
003820*
           MOVE 1         TO ページカウンタ.
      * / 並び順変更
003853     MOVE 連入－請求和暦    TO  作２－請求和暦.
003854     MOVE 連入－請求年      TO  作２－請求年.
003855     MOVE 連入－請求月      TO  作２－請求月.
003857     MOVE 連印－分類コード  TO  作２－分類コード.
003856     MOVE 連印－県コード    TO  作２－県コード.
003858     MOVE 連印－保険順      TO  作２－保険順.
003859     MOVE SPACE             TO  作２－保険者番号.
003860     START 作業ファイル２   KEY IS >=  作２－請求和暦年月
003861                                       作２－分類コード
003862                                       作２－県コード
003863                                       作２－保険順
003864                                       作２－保険者番号
003865     END-START.
003866     IF 状態キー = "00"
003867         MOVE SPACE TO 終了フラグ
003868         PERFORM 作業ファイル２読込
003869         IF  終了フラグ = "YES"
003870             MOVE  NC"　該当データなしです。確認して下さい。" TO 連メ－メッセージ
003871             CALL   "MSG001"
003872             CANCEL "MSG001"
003873             PERFORM ファイル閉鎖
003874             MOVE 99 TO PROGRAM-STATUS
003875             EXIT PROGRAM
003876         END-IF
003877*
003878         PERFORM UNTIL (終了フラグ = "YES") OR
                             (作２－分類コード NOT = 連印－分類コード) OR
                             (作２－保険順     NOT = 連印－保険順) OR
                             (作２－県コード   NOT = 連印－県コード)
003879           MOVE SPACE TO スキップフラグ
003880           PERFORM 印刷対象チェック
003881           IF 印刷フラグ = "YES"
003882*
003883             MOVE SPACE TO YHN436P
003884             INITIALIZE    YHN436P
003887             PERFORM  表セット１
003888* 
003889             PERFORM VARYING 行カウンタ FROM 1 BY 1
003890                     UNTIL ( 行カウンタ > 8       ) OR
003891                           ( 連印－分類コード   NOT = 作２－分類コード ) OR
003892                           ( 連印－県コード     NOT = 作２－県コード ) OR
003892                           ( 連印－保険順       NOT = 作２－保険順 ) OR
003893                           ( 終了フラグ = "YES" )
003894                  PERFORM 印刷対象チェック
003895                  IF 印刷フラグ = "YES"
003897                     PERFORM 表セット２
                           PERFORM フッタセット
      */↓↓改頁後、レコードがあり、印刷対象がない場合、合計が印刷されない/0510
003898*                  ELSE
003899*                     COMPUTE 行カウンタ =  行カウンタ - 1
003900                  END-IF
003901                  PERFORM 作業ファイル２読込
                        PERFORM 印刷対象チェック
                        IF 印刷フラグ = SPACE
                           COMPUTE 行カウンタ =  行カウンタ - 1
                        END-IF
      */↑↑改頁後、レコードがあり、印刷対象がない場合、合計が印刷されない/0510
003902                  MOVE "YES" TO スキップフラグ
003903             END-PERFORM
003904*
003905             IF ( 終了フラグ =  "YES" ) OR 
003906                ( 連印－分類コード NOT = 作２－分類コード ) OR
003892                ( 連印－保険順     NOT = 作２－保険順 ) OR
003907                ( 連印－県コード   NOT = 作２－県コード )
003908                 MOVE 本人件数カウント   TO     本人件数合計
003909                 MOVE 家族件数カウント   TO     家族件数合計
003910                 MOVE 計件数カウント     TO     計件数合計
003911                 MOVE 本人費用額カウント TO     本人費用額合計
003912                 MOVE 家族費用額カウント TO     家族費用額合計
003913                 MOVE 計費用額カウント   TO     計費用額合計
003914                 MOVE 本人請求額カウント TO     本人請求額合計
003915                 MOVE 家族請求額カウント TO     家族請求額合計
003916                 MOVE 計請求額カウント   TO     計請求額合計
003917                 IF 本人請求額カウント NOT = ZERO
003918                    MOVE "(" TO 本人左括弧合計
003919                    MOVE ")" TO 本人右括弧合計
003920                 ELSE
003921                    MOVE SPACE TO 本人左括弧合計
003922                    MOVE SPACE TO 本人右括弧合計
003923                 END-IF
003924                 IF 家族請求額カウント NOT = ZERO
003925                    MOVE "(" TO 家族左括弧合計
003926                    MOVE ")" TO 家族右括弧合計
003927                 ELSE
003928                    MOVE SPACE TO 家族左括弧合計
003929                    MOVE SPACE TO 家族右括弧合計
003930                 END-IF
003931                 IF 計請求額カウント NOT = ZERO
003932                    MOVE "(" TO 計左括弧合計
003933                    MOVE ")" TO 計右括弧合計
003934                 ELSE
003935                    MOVE SPACE TO 計左括弧合計
003936                    MOVE SPACE TO 計右括弧合計
003937                 END-IF
003938*
003939                 IF ページカウンタ > 1
                           MOVE ページカウンタ TO 頁
                       END-IF
                       MOVE 1                  TO ページカウンタ
003939                 INITIALIZE 合計用Ｗ
003940             ELSE
003941                 MOVE ZERO     TO     本人件数合計
003942                 MOVE ZERO     TO     家族件数合計
003943                 MOVE ZERO     TO     計件数合計
003944                 MOVE ZERO     TO     本人費用額合計
003945                 MOVE ZERO     TO     家族費用額合計
003946                 MOVE ZERO     TO     計費用額合計
003947                 MOVE ZERO     TO     本人請求額合計
003948                 MOVE ZERO     TO     家族請求額合計
003949                 MOVE ZERO     TO     計請求額合計
                       MOVE ページカウンタ TO 頁
                       COMPUTE ページカウンタ = ページカウンタ + 1
003950             END-IF
003953*
003954             PERFORM 印字処理
003955             PERFORM 改頁処理
003956*
003960           END-IF
003961*
003963           IF スキップフラグ NOT = "YES"
003964              PERFORM 作業ファイル２読込
003965           END-IF
003966         END-PERFORM
003967*
003968     ELSE
003969         MOVE  NC"　該当データなしです。確認して下さい。" TO 連メ－メッセージ
003970         CALL   "MSG001"
003971         CANCEL "MSG001"
003972         PERFORM ファイル閉鎖
003973         MOVE 99 TO PROGRAM-STATUS
003974         EXIT PROGRAM
003975     END-IF.
004621*
004630*================================================================*
004640 表セット１ SECTION.
004650*
004944* 当月の和暦を取得
004945     MOVE 請求和暦Ｗ         TO 元－元号区分.
004946     READ 元号マスタ
004947     INVALID KEY
004948         MOVE SPACE          TO 請求和暦名称
004949     NOT INVALID KEY
004950         MOVE 元－元号名称   TO 請求和暦名称
004951     END-READ.
004952*
004953     MOVE 請求年Ｗ           TO 請求年.
004954     MOVE 請求月Ｗ           TO 請求月.
004955*
007100* 作成日を取得
           IF 連入－作成日印刷 = 1
007110        MOVE 提出和暦Ｗ         TO 元－元号区分
007120        READ 元号マスタ
007130        INVALID KEY
007140           MOVE SPACE          TO 作成和暦
007150        NOT INVALID KEY
007160           MOVE 元－元号名称   TO 作成和暦
007170        END-READ
007180        MOVE 提出年Ｗ          TO 作成年
007190        MOVE 提出月Ｗ          TO 作成月
007200        MOVE 提出日Ｗ          TO 作成日
      */コメントを提出日に変更/101012
      *        MOVE NC"作成日"        TO 作成ＣＭ
              MOVE NC"提出日"        TO 作成ＣＭ
              MOVE NC"年"            TO 作成年ＣＭ
              MOVE NC"月"            TO 作成月ＣＭ
              MOVE NC"日"            TO 作成日ＣＭ
           END-IF.
004967*
004968     MOVE 施術所住所Ｗ       TO 住所.
004968*     MOVE 施術所住所１Ｗ     TO 住所１.
004968*     MOVE 施術所住所２Ｗ     TO 住所２.
004969*     MOVE 施術所郵便番号Ｗ   TO 郵便番号.
004970*     MOVE 代表者カナＷ       TO 代表者カナ.
004970     MOVE 代表者名Ｗ         TO 代表者名.
004971     MOVE 接骨院名Ｗ         TO 接骨院名.
004972     MOVE 柔整師番号Ｗ       TO 柔整師番号.
004973     MOVE 施術所電話番号Ｗ   TO 電話番号.
004974*     MOVE 銀行名支店名Ｗ     TO 銀行名支店名.
004975*     MOVE 預金種別コメントＷ TO 預金種別.
004976*     MOVE 口座番号Ｗ         TO 口座番号.
004977*     MOVE 口座名義人カナＷ   TO 口座名義人カナ.
004978*     MOVE 口座名義人Ｗ       TO 口座名義人.
004979*
004980*================================================================*
004981 表セット２ SECTION.
004982*
004983     MOVE 作２－本人件数     TO 本人件数(行カウンタ).
004990     MOVE 作２－本人費用額   TO 本人費用額(行カウンタ).
005000     MOVE 作２－本人請求額   TO 本人請求額(行カウンタ).
005010     IF 作２－本人請求額 NOT = ZERO
005020        MOVE "(" TO 本人左括弧(行カウンタ)
005030        MOVE ")" TO 本人右括弧(行カウンタ)
005040     ELSE
005050        MOVE SPACE TO 本人左括弧(行カウンタ)
005060        MOVE SPACE TO 本人右括弧(行カウンタ)
005070     END-IF.
005080*
005090     MOVE 作２－家族件数     TO 家族件数(行カウンタ).
005100     MOVE 作２－家族費用額   TO 家族費用額(行カウンタ).
005110     MOVE 作２－家族請求額   TO 家族請求額(行カウンタ).
005120     IF 作２－家族請求額 NOT = ZERO
005130        MOVE "(" TO 家族左括弧(行カウンタ)
005140        MOVE ")" TO 家族右括弧(行カウンタ)
005150     ELSE
005160        MOVE SPACE TO 家族左括弧(行カウンタ)
005170        MOVE SPACE TO 家族右括弧(行カウンタ)
005180     END-IF.
005190*
005230     MOVE 作２－件数    TO  計件数(行カウンタ).
005240     MOVE 作２－費用額  TO  計費用額(行カウンタ).
005250     MOVE 作２－請求額  TO  計請求額(行カウンタ).
005260     IF 作２－請求額 NOT = ZERO
005270        MOVE "(" TO 計左括弧(行カウンタ)
005280        MOVE ")" TO 計右括弧(行カウンタ)
005290     ELSE
005300        MOVE SPACE TO 計左括弧(行カウンタ)
005310        MOVE SPACE TO 計右括弧(行カウンタ)
005320     END-IF.
005330*
005340     ADD 作２－本人件数   TO  本人件数カウント.
005350     ADD 作２－家族件数   TO  家族件数カウント.
005360     ADD 作２－件数       TO  計件数カウント.
005370     ADD 作２－本人費用額 TO  本人費用額カウント.
005380     ADD 作２－家族費用額 TO  家族費用額カウント.
005390     ADD 作２－費用額     TO  計費用額カウント.
005400     ADD 作２－本人請求額 TO  本人請求額カウント.
005410     ADD 作２－家族請求額 TO  家族請求額カウント.
005420     ADD 作２－請求額     TO  計請求額カウント.
005430**
005440* 保険者
005441     MOVE 作２－保険種別     TO 保険種別Ｗ.
005442     MOVE 作２－保険者番号   TO 保険者番号Ｗ.
005443     EVALUATE 保険種別Ｗ
005444     WHEN 1 THRU 4
005445     WHEN 6 THRU 9
005446         PERFORM 保険者情報取得
005447     WHEN 5
005448     WHEN 50 THRU 60
005449         PERFORM 市町村情報取得
005450     END-EVALUATE.
005451     PERFORM 結合宛名取得.
005452*
005453     MOVE 保険者宛名Ｗ  TO 保険者名称Ｗ.
005970*
006000*
006010     COMPUTE 合計行カウンタ = 行カウンタ * 3.
006020     MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 1).
006030     IF 作２－保険種別 = "50" OR "51" OR "52" OR "53" OR
006040                         "54" OR "55" OR "60" OR "05" OR "08"
006050        IF 保険者名称２Ｗ NOT = SPACE AND 保険者名称３Ｗ = SPACE
006060           MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 2)
006070           MOVE 保険者名称２Ｗ TO 保険者名(合計行カウンタ - 1)
006080        END-IF
006090     ELSE
006100        IF 保険者名称２Ｗ NOT = SPACE AND 保険者名称３Ｗ = SPACE
006110           MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 1)
006120           MOVE 保険者名称２Ｗ TO 保険者名(合計行カウンタ)
006130        END-IF
006140     END-IF.
006150     IF 作２－保険種別 = "50" OR "51" OR "52" OR "53" OR
006160                         "54" OR "55" OR "60" OR "05" OR "08"
006170        IF 保険者名称２Ｗ NOT = SPACE AND 保険者名称３Ｗ NOT = SPACE
006180           MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 2)
006190           MOVE 保険者名称２Ｗ TO 保険者名(合計行カウンタ - 1)
006200        END-IF
006210     ELSE
006220        IF 保険者名称２Ｗ NOT = SPACE AND 保険者名称３Ｗ NOT = SPACE
006230           MOVE 保険者名称１Ｗ TO 保険者名(合計行カウンタ - 2)
006240           MOVE 保険者名称２Ｗ TO 保険者名(合計行カウンタ - 1)
006250           MOVE 保険者名称３Ｗ TO 保険者名(合計行カウンタ)
006260        END-IF
006270     END-IF.
006280*     IF 作２－保険種別 = "50" OR "51" OR "52" OR "53" OR
006290*                         "54" OR "55" OR "60" 
006300*        MOVE "（助成）" TO 保険者名(合計行カウンタ)
006310*     END-IF.
006320*     IF 作２－保険種別 = "05"
006330*        MOVE "（老人）" TO 保険者名(合計行カウンタ)
006340*     END-IF.
006350*     IF 作２－保険種別 = "08"
006360*        MOVE "（退職）" TO 保険者名(合計行カウンタ)
006370*     END-IF.
006380*
006390*================================================================*
006400 フッタセット SECTION.
006410*
      */ 保険種別、県名、被保険者名を下空間に印刷 /150219
           IF 作２－保険種別 = 01
               MOVE 作２－保険者番号(1:2)  TO 名－名称コード
           ELSE
               MOVE 作２－保険者番号(3:2)  TO 名－名称コード
           END-IF.
025960     MOVE 13                     TO 名－区分コード.
025980     READ 名称マスタ
025990     INVALID KEY
026000         MOVE SPACE              TO 県名Ｗ
026010     NOT INVALID KEY
026020         MOVE 名－略称           TO 県名Ｗ
026030     END-READ.
           STRING "["                  DELIMITED BY SIZE
                  県名ＷＰ             DELIMITED BY "　"
                  "]"                  DELIMITED BY SIZE
             INTO 県ＣＭ
           END-STRING.
           EVALUATE 作２－保険種別
           WHEN 1
           WHEN 8
               MOVE "国保連合会（国保）"    TO 保険者名称
           WHEN 5
               MOVE "国保連合会（後期）"    TO 保険者名称
           WHEN 2
           WHEN 6
               MOVE "全国健康保険協会支部"    TO 保険者名称
           WHEN 7
               MOVE "船員保険"      TO 保険者名称
           WHEN 3
               MOVE "健保組合"      TO 保険者名称
           WHEN 4
           WHEN 9
               MOVE "共済組合"      TO 保険者名称
           WHEN 51
           WHEN 52
           WHEN 53
           WHEN 54
           WHEN 55
           WHEN 60
               MOVE "国保連合会（助成）"    TO 保険者名称
           END-EVALUATE.
      *
           MOVE 作２－分類コード    TO 作３－分類コード.
           MOVE 作２－県コード      TO 作３－県コード.
           MOVE 作２－保険順        TO 作３－保険順.
           IF 作２－分類コード = 3 OR 4
              MOVE 作２－保険者番号 TO 作３－保険者番号
           ELSE
              MOVE SPACE            TO 作３－保険者番号
           END-IF.
           READ 作業ファイル３
           NOT INVALID KEY
              MOVE 作３－６号順番   TO 区分１
              MOVE "*"              TO 区分２ 区分３
              MOVE "-"              TO 区切１ 区切２
           END-READ.
      *
006390*================================================================*
006400 エラー処理Ｐ SECTION.
006410*
006420     IF 通知情報Ｐ NOT = "00"
006430         DISPLAY NC"帳票エラー"              UPON CONS
006440         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
006450         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
006460         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
006470         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
006480                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
006490         ACCEPT  キー入力 FROM CONS
006500         PERFORM ファイル閉鎖
006510         MOVE 99  TO PROGRAM-STATUS
006520         EXIT PROGRAM
006530     END-IF.
006540*================================================================*
006550 印字処理  SECTION.
006560*
004310     IF ( オープンフラグ NOT = "YES" )
004320        MOVE "YES" TO オープンフラグ
004330        OPEN I-O  印刷ファイル
004340        PERFORM エラー処理Ｐ
004350     END-IF.
013440*
006570     MOVE "YHN436P" TO  定義体名Ｐ.
006580     MOVE SPACE     TO  処理種別Ｐ.
006590     MOVE "SCREEN"  TO  項目群名Ｐ.
006610     WRITE YHN436P.
006620     PERFORM エラー処理Ｐ.
006650*================================================================*
006660 改頁処理  SECTION.
006670
006680     MOVE "YHN436P" TO  定義体名Ｐ.
006690     MOVE "CT"      TO  処理種別Ｐ.
006700     MOVE "PAGE"    TO  拡張制御Ｐ.
006710     MOVE SPACE     TO  項目群名Ｐ.
006730     WRITE YHN436P.
006740     PERFORM エラー処理Ｐ.
006750     MOVE SPACE     TO  拡張制御Ｐ.
006760*
006770*     CLOSE  印刷ファイル.
006780*     OPEN I-O   印刷ファイル.
006790*     PERFORM エラー処理Ｐ.
006800*
006810*================================================================*
006811*================================================================*
006812 保険者情報取得 SECTION.
006813*
006814     MOVE  SPACE         TO 請求先名称Ｗ.
006815     MOVE  SPACE         TO 支部部署名Ｗ.
006816     MOVE  ZERO          TO 接尾語区分Ｗ.
006817*
006818     MOVE 保険種別Ｗ     TO 保－保険種別.
006819     MOVE 保険者番号Ｗ   TO 保－保険者番号.
006820     READ 保険者マスタ
006821     INVALID KEY
006822         MOVE SPACE      TO 請求先名称Ｗ
006823         MOVE SPACE      TO 支部部署名Ｗ
006824     NOT INVALID KEY
006825         IF 保－請求先情報区分 = 1
006826             MOVE 保－保険種別   TO 請先－保険種別
006827             MOVE 保－保険者番号 TO 請先－保険者番号
006828             READ 請求先マスタ
006829             INVALID KEY
006830                 MOVE SPACE             TO 請求先名称Ｗ
006831                 MOVE SPACE             TO 支部部署名Ｗ
006832             NOT INVALID KEY
006833                 MOVE 請先－保険者名称  TO 請求先名称Ｗ
006834                 MOVE 請先－支部部署名  TO 支部部署名Ｗ
006835             END-READ
006836         ELSE
006837             MOVE 保－保険者名称        TO 請求先名称Ｗ
006838             MOVE 保－支部部署名        TO 支部部署名Ｗ
006839             MOVE 保－接尾語区分        TO 接尾語区分Ｗ
006840         END-IF
006841     END-READ.
006842*================================================================*
006843 市町村情報取得 SECTION.
006844*
006845     MOVE  SPACE         TO 請求先名称Ｗ.
006846     MOVE  SPACE         TO 支部部署名Ｗ.
006847*
006848     MOVE 保険種別Ｗ               TO 市－公費種別.
006849     MOVE 保険者番号Ｗ             TO 市－市町村番号.
006850     READ 市町村マスタ
006851     INVALID KEY
006852         MOVE SPACE                TO 請求先名称Ｗ
006853         MOVE SPACE                TO 支部部署名Ｗ
006854     NOT INVALID KEY
006855         IF 市－請求先区分 = 1
006856             MOVE 保険種別Ｗ       TO 請先－保険種別
006857             MOVE 保険者番号Ｗ     TO 請先－保険者番号
006858             READ 請求先マスタ
006859             INVALID KEY
006860                 MOVE SPACE        TO 請求先名称Ｗ
006861                 MOVE SPACE        TO 支部部署名Ｗ
006862             NOT INVALID KEY
006863                 MOVE 請先－保険者名称   TO 請求先名称Ｗ
006864                 MOVE 請先－支部部署名   TO 支部部署名Ｗ
006865             END-READ
006866          ELSE
006867             MOVE 市－市町村名称   TO 請求先名称Ｗ
006868             MOVE 市－支部部署名   TO 支部部署名Ｗ
006869          END-IF
006870      END-READ.
006871*================================================================*
006872 結合宛名取得 SECTION.
006873*
006874     MOVE SPACE TO 保険者宛名Ｗ.
006875     IF 請求先名称Ｗ NOT = SPACE
006876         EVALUATE 保険種別Ｗ
006877         WHEN 2
006878             IF 接尾語区分Ｗ = 1
006879                MOVE SPACE            TO 宛名Ｗ
006880             ELSE
006881                MOVE "社会保険事務所" TO 宛名Ｗ
006882             END-IF
006883         WHEN 6
006884             IF 接尾語区分Ｗ = 1
006885                MOVE "（日雇）"               TO 宛名Ｗ
006887             ELSE
006888                MOVE "社会保険事務所（日雇）" TO 宛名Ｗ
006889             END-IF
006890         WHEN 7
006891             MOVE "（船員）"       TO 宛名Ｗ
006892         WHEN 3
006893             MOVE "健康保険組合"   TO 宛名Ｗ
006894         WHEN 4
006895             MOVE "共済組合"       TO 宛名Ｗ
006896         WHEN OTHER
006897             MOVE SPACE            TO 宛名Ｗ
006898         END-EVALUATE
006899*
006900         IF 支部部署名Ｗ = SPACE
006901             STRING  請求先名称Ｗ  DELIMITED BY SPACE
006902                     宛名Ｗ        DELIMITED BY SPACE
006903                    INTO 保険者宛名Ｗ
006904             END-STRING
006905         ELSE
006906             STRING  請求先名称Ｗ  DELIMITED BY SPACE
006907                     宛名Ｗ        DELIMITED BY SPACE
006908                     支部部署名Ｗ  DELIMITED BY SPACE
006909                    INTO 保険者宛名Ｗ
006910             END-STRING
006911         END-IF
006912     END-IF.
006913*
006914*================================================================*
006915*================================================================*
006916 制御情報取得２ SECTION.
006917* 印刷区分、振込先の情報を取得
006918* 制御区分01
006919*     MOVE 01 TO 制－制御区分.
006920*     READ 制御情報マスタ
006921*     NOT INVALID KEY
006922*        IF 制０１－請求書更新フラグ = 1
006923*           MOVE 制０１－国保６号印刷区分  TO 国保６号印刷区分Ｗ 
006924*           MOVE 制０１－国保７号印刷区分  TO 国保７号印刷区分Ｗ 
006925*           MOVE 制０１－国保当社印刷区分  TO 国保当社印刷区分Ｗ 
006926*           MOVE 制０１－国保振込先区分    TO 国保振込先区分Ｗ     
006927*           MOVE 制０１－社保６号印刷区分  TO 社保６号印刷区分Ｗ 
006928*           MOVE 制０１－社保７号印刷区分  TO 社保７号印刷区分Ｗ 
006929*           MOVE 制０１－社保当社印刷区分  TO 社保当社印刷区分Ｗ 
006930*           MOVE 制０１－社保振込先区分    TO 社保振込先区分Ｗ     
006931*           MOVE 制０１－組合６号印刷区分  TO 組合６号印刷区分Ｗ 
006932*           MOVE 制０１－組合７号印刷区分  TO 組合７号印刷区分Ｗ 
006933*           MOVE 制０１－組合当社印刷区分  TO 組合当社印刷区分Ｗ 
006934*           MOVE 制０１－組合振込先区分    TO 組合振込先区分Ｗ     
006935*           MOVE 制０１－共済６号印刷区分  TO 共済６号印刷区分Ｗ 
006936*           MOVE 制０１－共済７号印刷区分  TO 共済７号印刷区分Ｗ 
006937*           MOVE 制０１－共済当社印刷区分  TO 共済当社印刷区分Ｗ 
006938*           MOVE 制０１－共済振込先区分    TO 共済振込先区分Ｗ     
006939*           MOVE 制０１－老人６号印刷区分  TO 老人６号印刷区分Ｗ 
006940*           MOVE 制０１－老人７号印刷区分  TO 老人７号印刷区分Ｗ 
006941*           MOVE 制０１－老人当社印刷区分  TO 老人当社印刷区分Ｗ 
006942*           MOVE 制０１－老人振込先区分    TO 老人振込先区分Ｗ     
006943*           MOVE 制０１－助成６号印刷区分  TO 助成６号印刷区分Ｗ 
006944*           MOVE 制０１－助成７号印刷区分  TO 助成７号印刷区分Ｗ 
006945*           MOVE 制０１－助成当社印刷区分  TO 助成当社印刷区分Ｗ 
006946*           MOVE 制０１－助成振込先区分    TO 助成振込先区分Ｗ     
009271*           MOVE 制０１－６７並び順区分    TO ６７並び順区分Ｗ     
006947*        END-IF
006948*     END-READ.
      *
009438* 制御区分02
009439     MOVE 02 TO 制－制御区分.
009440     READ 制御情報マスタ
009441     NOT INVALID KEY
009443        MOVE 制０２－北海道総括表区分  TO 北海道区分Ｗ 
009443        MOVE 制０２－青森総括表区分    TO 青森区分Ｗ 
009443        MOVE 制０２－岩手総括表区分    TO 岩手区分Ｗ 
009443        MOVE 制０２－宮城総括表区分    TO 宮城区分Ｗ 
009443        MOVE 制０２－秋田総括表区分    TO 秋田区分Ｗ 
009443        MOVE 制０２－山形総括表区分    TO 山形区分Ｗ 
009443        MOVE 制０２－福島総括表区分    TO 福島区分Ｗ 
009443        MOVE 制０２－茨城総括表区分    TO 茨城区分Ｗ 
009443        MOVE 制０２－栃木総括表区分    TO 栃木区分Ｗ 
009443        MOVE 制０２－群馬総括表区分    TO 群馬区分Ｗ 
009443        MOVE 制０２－埼玉総括表区分    TO 埼玉区分Ｗ 
009443        MOVE 制０２－千葉総括表区分    TO 千葉区分Ｗ 
009443        MOVE 制０２－東京総括表区分    TO 東京区分Ｗ 
009443        MOVE 制０２－神奈川総括表区分  TO 神奈川区分Ｗ 
009443        MOVE 制０２－新潟総括表区分    TO 新潟区分Ｗ 
009443        MOVE 制０２－富山総括表区分    TO 富山区分Ｗ 
009443        MOVE 制０２－石川総括表区分    TO 石川区分Ｗ 
009443        MOVE 制０２－福井総括表区分    TO 福井区分Ｗ 
009443        MOVE 制０２－山梨総括表区分    TO 山梨区分Ｗ 
009443        MOVE 制０２－長野総括表区分    TO 長野区分Ｗ 
009443        MOVE 制０２－岐阜総括表区分    TO 岐阜区分Ｗ 
009443        MOVE 制０２－静岡総括表区分    TO 静岡区分Ｗ 
009443        MOVE 制０２－愛知総括表区分    TO 愛知区分Ｗ 
009443        MOVE 制０２－三重総括表区分    TO 三重区分Ｗ 
009443        MOVE 制０２－滋賀総括表区分    TO 滋賀区分Ｗ 
009443        MOVE 制０２－京都総括表区分    TO 京都区分Ｗ 
009443        MOVE 制０２－大阪総括表区分    TO 大阪区分Ｗ 
009443        MOVE 制０２－兵庫総括表区分    TO 兵庫区分Ｗ 
009443        MOVE 制０２－奈良総括表区分    TO 奈良区分Ｗ 
009443        MOVE 制０２－和歌山総括表区分  TO 和歌山区分Ｗ 
009443        MOVE 制０２－鳥取総括表区分    TO 鳥取区分Ｗ 
009443        MOVE 制０２－島根総括表区分    TO 島根区分Ｗ 
009443        MOVE 制０２－岡山総括表区分    TO 岡山区分Ｗ 
009443        MOVE 制０２－広島総括表区分    TO 広島区分Ｗ 
009443        MOVE 制０２－山口総括表区分    TO 山口区分Ｗ 
009443        MOVE 制０２－徳島総括表区分    TO 徳島区分Ｗ 
009443        MOVE 制０２－香川総括表区分    TO 香川区分Ｗ 
009443        MOVE 制０２－愛媛総括表区分    TO 愛媛区分Ｗ 
009443        MOVE 制０２－高知総括表区分    TO 高知区分Ｗ 
009443        MOVE 制０２－福岡総括表区分    TO 福岡区分Ｗ 
009443        MOVE 制０２－佐賀総括表区分    TO 佐賀区分Ｗ 
009443        MOVE 制０２－長崎総括表区分    TO 長崎区分Ｗ 
009443        MOVE 制０２－熊本総括表区分    TO 熊本区分Ｗ 
009443        MOVE 制０２－大分総括表区分    TO 大分区分Ｗ 
009443        MOVE 制０２－宮崎総括表区分    TO 宮崎区分Ｗ 
009443        MOVE 制０２－鹿児島総括表区分  TO 鹿児島区分Ｗ 
009443        MOVE 制０２－沖縄総括表区分    TO 沖縄区分Ｗ 
009468     END-READ.
006949*
006950*================================================================*
006951*================================================================*
006952 印刷対象チェック  SECTION.
006953*
006954*  印刷区分による振り分け → ６・７号請求書（印刷区分 0:印刷 1:印刷しない）
006955* （一括印刷のみ）
006956*
006957     MOVE SPACE TO 印刷フラグ.
006958*
006959*     IF 連入－一括区分 NOT = 1
006960*        MOVE "YES" TO 印刷フラグ
006961*     ELSE
006963        EVALUATE 作２－保険種別
006964        WHEN 01
006965        WHEN 08
006966*           IF 国保６号印刷区分Ｗ NOT = 1
006967              MOVE "YES" TO 印刷フラグ
006968*           END-IF
006969        WHEN 02
006970        WHEN 06
006971        WHEN 07
006972*           IF 社保６号印刷区分Ｗ NOT = 1
006973              MOVE "YES" TO 印刷フラグ
006974*           END-IF
006975*        WHEN 03
006976*           IF 組合６号印刷区分Ｗ NOT = 1
006977*              MOVE "YES" TO 印刷フラグ
006978*           END-IF
006979*        WHEN 04
006980*        WHEN 09
006981*           IF 共済６号印刷区分Ｗ NOT = 1
006982*              MOVE "YES" TO 印刷フラグ
006983*           END-IF
006984        WHEN 05
006985*           IF 老人６号印刷区分Ｗ NOT = 1
006986              MOVE "YES" TO 印刷フラグ
006987*           END-IF
006988        WHEN 50 THRU 60
006989*           IF 助成６号印刷区分Ｗ NOT = 1
                 IF 作２－分類コード NOT = 6
006990              MOVE "YES" TO 印刷フラグ
006991           END-IF
006992*        WHEN OTHER
006993*           MOVE "YES" TO 印刷フラグ
006994        END-EVALUATE
      *
              IF 印刷フラグ = "YES"
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 01)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 01))
                    IF (北海道区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 02)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 02))
                    IF (青森区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 03)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 03))
                    IF (岩手区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 04)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 04))
                    IF (宮城区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 05)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 05))
                    IF (秋田区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 06)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 06))
                    IF (山形区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 07)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 07))
                    IF (福島区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 08)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 08))
                    IF (茨城区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 09)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 09))
                    IF (栃木区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 10)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 10))
                    IF (群馬区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 11)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 11))
                    IF (埼玉区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
      */千葉は県固有選択になっていても６７号を出す*/150409
      *           IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 12)) OR
      *              ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 12))
      *              IF (千葉区分Ｗ = ZERO)
009576*                 MOVE "YES" TO 印刷フラグ
      *              ELSE
009576*                 MOVE SPACE TO 印刷フラグ
009577*              END-IF
009577*           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 13)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 13))
                    IF (東京区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 14)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 14))
                    IF (神奈川区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 15)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 15))
                    IF (新潟区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 16)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 16))
                    IF (富山区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 17)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 17))
                    IF (石川区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 18)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 18))
                    IF (福井区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 19)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 19))
                    IF (山梨区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 20)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 20)) OR
                    (((作２－保険種別 >= 50) AND (作２－保険種別 <= 60)) AND (作２－保険者番号(3:2) = 20))
                    IF (長野区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 21)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 21))
                    IF (岐阜区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 22)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 22))
                    IF (静岡区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 23)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 23))
                    IF (愛知区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 24)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 24))
                    IF (三重区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 25)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 25))
                    IF (滋賀区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 26)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 26)) OR
                    (((作２－保険種別 >= 50) AND (作２－保険種別 <= 60)) AND (作２－保険者番号(3:2) = 26))
                    IF (京都区分Ｗ = ZERO)
009576                  MOVE "YES" TO 印刷フラグ
                    ELSE
                       IF ((作２－保険種別 = 53) AND (作２－保険者番号(1:4) = 3926))
009576                    MOVE "YES" TO 印刷フラグ
                       ELSE
009576                    MOVE SPACE TO 印刷フラグ
                       END-IF
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(3:2) = 27)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 27)) OR
                    (((作２－保険種別 >= 50) AND (作２－保険種別 <= 60)) AND (作２－保険者番号(3:2) = 27))
                    IF (大阪区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 28)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 28))
                    IF (兵庫区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 29)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 29))
                    IF (奈良区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 30)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 30))
                    IF (和歌山区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 31)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 31))
                    IF (鳥取区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 32)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 32))
                    IF (島根区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 33)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 33))
                    IF (岡山区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 34)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 34))
                    IF (広島区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 35)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 35))
                    IF (山口区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 36)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 36))
                    IF (徳島区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 37)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 37))
                    IF (香川区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 38)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 38))
                    IF (愛媛区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 39)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 39))
                    IF (高知区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 40)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 40))
                    IF (福岡区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 41)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 41))
                    IF (佐賀区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 42)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 42))
                    IF (長崎区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 43)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 43))
                    IF (熊本区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 44)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 44))
                    IF (大分区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 45)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 45))
                    IF (宮崎区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 46)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 46))
                    IF (鹿児島区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
                 IF ((作２－保険種別 = 01) AND (作２－保険者番号(1:2) = 47)) OR
                    ((作２－保険種別 = 08 OR 05) AND (作２－保険者番号(3:2) = 47))
                    IF (沖縄区分Ｗ = ZERO)
009576                 MOVE "YES" TO 印刷フラグ
                    ELSE
009576                 MOVE SPACE TO 印刷フラグ
009577              END-IF
009577           END-IF
009577*        END-IF
009581     END-IF.
      */神奈川県石油業06141261 を６号の対象外にする/120427
           IF 作２－保険者番号 = "06141261"
006957         MOVE SPACE TO 印刷フラグ
           END-IF.
006996*
007011*================================================================*
       遅延処理 SECTION.
      *
           PERFORM VARYING 遅延カウンタ FROM 1 BY 1
                   UNTIL 遅延カウンタ > 遅延回数Ｗ
               MOVE "YES" TO 遅延フラグ
           END-PERFORM.
      *
005110*================================================================*
007012******************************************************************
007013 END PROGRAM YHN436.
007014******************************************************************
