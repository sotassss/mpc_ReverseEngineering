000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHN437.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*      ７号用紙   請求書【印刷】柔+ｳｨﾝﾄﾞｳｽﾞ版
000100*  請求年月バージョン
000101*         MED = YHN437P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2015-03-02
000130 DATE-COMPILED.          2015-03-02
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
000380     SELECT  保険者マスタ    ASSIGN      TO        HOKENSL
000390                             ORGANIZATION             IS  INDEXED
000400                             ACCESS MODE              IS  DYNAMIC
000410                             RECORD KEY               IS  保－保険種別
000420                                                          保－保険者番号
000430                             ALTERNATE RECORD KEY     IS  保－保険種別
000440                                                          保－保険者名称
000450                                                          保－保険者番号
000460                             FILE STATUS              IS  状態キー
000470                             LOCK        MODE         IS  AUTOMATIC.
000480     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
000490                             ORGANIZATION             IS  INDEXED
000500                             ACCESS MODE              IS  DYNAMIC
000510                             RECORD KEY               IS  市－公費種別
000520                                                          市－市町村番号
000530                             ALTERNATE RECORD KEY     IS  市－公費種別
000540                                                          市－市町村名称
000550                                                          市－市町村番号
000560                             FILE STATUS              IS  状態キー
000570                             LOCK        MODE         IS  AUTOMATIC.
000650     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000660                             ORGANIZATION             IS  INDEXED
000670                             ACCESS MODE              IS  DYNAMIC
000680                             RECORD KEY               IS 施情－施術所番号
000690                             FILE STATUS              IS  状態キー
000700                             LOCK        MODE         IS  AUTOMATIC.
000710     SELECT  請求先マスタ    ASSIGN      TO        SEIKYUSL
000720                             ORGANIZATION             IS  INDEXED
000730                             ACCESS MODE              IS  DYNAMIC
000740                             RECORD KEY               IS  請先－保険種別
000750                                                          請先－保険者番号
000760                             FILE STATUS              IS  状態キー
000770                             LOCK    MODE             IS  AUTOMATIC.
000127     SELECT  会情報マスタ    ASSIGN      TO        KAIJOHOL
000128                             ORGANIZATION             IS  INDEXED
000129                             ACCESS MODE              IS  DYNAMIC
000130                             RECORD KEY               IS  会情－柔整鍼灸区分
000131                                                          会情－協会コード
000132                                                          会情－保険種別
000133                                                          会情－変更和暦年月
000134                             ALTERNATE RECORD KEY     IS  会情－柔整鍼灸区分
000135                                                          会情－接骨師会カナ
000136                                                          会情－協会コード
000137                                                          会情－保険種別
000138                                                          会情－変更和暦年月
000139                             FILE STATUS              IS  状態キー
000140                             LOCK        MODE         IS  AUTOMATIC.
000783*
000793     SELECT  作業ファイル２  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W4312L.DAT"
000794                             ORGANIZATION             IS  INDEXED
000795                             ACCESS                   IS  DYNAMIC
000111                             RECORD      KEY          IS  作２－請求和暦年月
000112                                                          作２－分類コード
000112                                                          作２－県コード
000112                                                          作２－保険順
000113                                                          作２－保険者番号
000804                             FILE        STATUS       IS  状態キー
000805                             LOCK        MODE         IS  AUTOMATIC.
      */並び順用　７号
000108     SELECT  作業ファイル４  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W4314L.DAT"
000109                             ORGANIZATION             IS  INDEXED
000110                             ACCESS                   IS  DYNAMIC
000111                             RECORD      KEY          IS  作４－分類コード
000112                                                          作４－県コード
000112                                                          作４－保険順
000113                                                          作４－保険者番号
000980                             FILE        STATUS       IS  状態キー
000990                             LOCK        MODE         IS  AUTOMATIC.
000806*
000816     SELECT  保険者拡張マスタ ASSIGN     TO        HOKENEXL
000826                             ORGANIZATION             IS  INDEXED
000836                             ACCESS MODE              IS  DYNAMIC
000846                             RECORD KEY               IS  保拡－保険種別
000856                                                          保拡－保険者番号
000866                             FILE STATUS              IS  状態キー
000876                             LOCK        MODE         IS  AUTOMATIC.
000980     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF001
000990                             SYMBOLIC    DESTINATION  IS "PRT"
001000                             FORMAT                   IS  定義体名Ｐ
001010                             GROUP                    IS  項目群名Ｐ
001020                             PROCESSING  MODE         IS  処理種別Ｐ
001030                             UNIT        CONTROL      IS  拡張制御Ｐ
001040                             FILE        STATUS       IS  通知情報Ｐ.
001050******************************************************************
001060*                      DATA DIVISION                             *
001070******************************************************************
001080 DATA                    DIVISION.
001090 FILE                    SECTION.
001100*                           ［ＲＬ＝  １２８］
001110 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001120     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001130*                           ［ＲＬ＝  ２５６］
001140 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001150     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
001151     COPY SEIGYO01        OF  XFDLIB  JOINING   制０１   AS  PREFIX.
001151     COPY SEIGYO02        OF  XFDLIB  JOINING   制０２   AS  PREFIX.
001000*                           ［ＲＬ＝  １２８］
001010 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
001020     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
001160*                           ［ＲＬ＝  ３２０］
001170 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001180     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
001190*                           ［ＲＬ＝  ２５６］
001200 FD  市町村マスタ        BLOCK   CONTAINS   1   RECORDS.
001210     COPY SITYOSN         OF  XFDLIB  JOINING   市   AS  PREFIX.
001250*                           ［ＲＬ＝  １２８］
001260 FD  施術所情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001270     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
001280*                           ［ＲＬ＝  １２８］
001290 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
001300     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
001302*                          ［ＲＬ＝  ６４０］
001303 FD  会情報マスタ        BLOCK   CONTAINS   1   RECORDS.
001304     COPY KAIJOHO         OF  XFDLIB  JOINING   会情   AS  PREFIX.
001305*                           ［ＲＬ＝  ８００］
001306 FD  保険者拡張マスタ        BLOCK   CONTAINS   1   RECORDS.
001307     COPY HOKENEX        OF  XFDLIB  JOINING   保拡   AS  PREFIX.
001320*                           ［ＲＬ＝  １２８］
001330 FD  作業ファイル２ RECORD  CONTAINS 128 CHARACTERS.
001340 01  作２－レコード.
001350     03  作２－レコードキー.
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
000175 FD  作業ファイル４ RECORD  CONTAINS 32 CHARACTERS.
000176 01  作４－レコード.
000177     03  作４－レコードキー.
001261         05  作４－分類コード                PIC 9(1).
001261         05  作４－県コード                  PIC X(2).
001400         05  作４－保険順                    PIC 9(2).
000183         05  作４－保険者番号                PIC X(10).
000188     03  作４－レコードデータ.
001261         05  作４－６号順番                  PIC 9(3).
001261         05  作４－７号順番                  PIC 9(3).
000201         05  FILLER                          PIC X(11).
001700*
001710 FD  印刷ファイル.
001720     COPY YHN437P        OF  XMDLIB.
001721*
001730*----------------------------------------------------------------*
001740******************************************************************
001750*                WORKING-STORAGE SECTION                         *
001760******************************************************************
001770 WORKING-STORAGE         SECTION.
001780 01 キー入力                           PIC X     VALUE SPACE.
001790 01 状態キー                           PIC X(2)  VALUE SPACE.
001800 01 終了フラグ                         PIC X(3)  VALUE SPACE.
001810 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
001820 01 書込フラグ                         PIC X(4)  VALUE SPACE.
001830 01 作業フラグ                         PIC X(3)  VALUE SPACE.
001840 01 作業移動キー                       PIC X(4)  VALUE SPACE.
001850 01 終了行フラグ                       PIC X(3)  VALUE SPACE.
001860 01 ファイル名                         PIC N(2)  VALUE SPACE.
001870 01 備考Ｗ                             PIC X(20) VALUE SPACE.
001880 01 前和暦Ｗ                           PIC 9(1)  VALUE ZERO.
001890 01 保険種別Ｗ                         PIC 9(2)  VALUE ZERO.
001900 01 保険者番号Ｗ                       PIC X(10) VALUE SPACE.
001910 01 保険者番号１Ｗ                     PIC X(10) VALUE SPACE.
001920 01 保険者番号２Ｗ                     PIC X(10) VALUE SPACE.
001921 01 印刷フラグ                         PIC X(3)  VALUE SPACE.
001210 01 オープンフラグ                     PIC X(3)   VALUE SPACE.
001930*
001940 01 行カウンタ                         PIC 9(2)  VALUE ZERO.
001950 01 頁カウンタ                         PIC 9(4)  VALUE ZERO.
001960 01 最大行数                           PIC 9(2)  VALUE ZERO.
001970 01 ヘッダ行数                         PIC 9(2)  VALUE ZERO.
001980 01 移動行数Ｗ                         PIC 9(2)  VALUE ZERO.
001990 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
002000 01 保険名称Ｗ                         PIC N(2) VALUE SPACE.
002010*
002030 01 施術和暦年月Ｗ.
002040     03 施術和暦Ｗ                     PIC 9(1)  VALUE ZERO.
002050     03 施術年月Ｗ.
002060        05 施術年Ｗ                    PIC 9(2)  VALUE ZERO.
002070        05 施術月Ｗ                    PIC 9(2)  VALUE ZERO.
002220**
002290**************
002300* 施術所情報 *
002310**************
002320 01 施術所情報Ｗ.
002330    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
002340    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
002341    03 柔整師番号Ｗ                    PIC X(20)  VALUE SPACE.
002350    03 施術所住所Ｗ.
002360       05 施術所住所１Ｗ               PIC X(50)  VALUE SPACE.
002370       05 施術所住所２Ｗ               PIC X(50)  VALUE SPACE.
002380    03 施術所郵便番号Ｗ.
002390       05 施術所郵便番号１Ｗ           PIC X(3)   VALUE SPACE.
002400       05 施術所郵便番号区切Ｗ         PIC X(1)   VALUE SPACE.
002410       05 施術所郵便番号２Ｗ           PIC X(4)   VALUE SPACE.
002420    03 施術所電話番号Ｗ                PIC X(15)  VALUE SPACE.
002430    03 取引先情報Ｗ.
002440        05 取引先銀行名Ｗ              PIC X(40)  VALUE SPACE.
002450        05 取引先銀行支店名Ｗ          PIC X(40)  VALUE SPACE.
002460        05 預金種別Ｗ                  PIC 9(1)   VALUE ZERO.
002470        05 銀行番号Ｗ                  PIC X(4)   VALUE SPACE.
002480        05 店番号Ｗ                    PIC X(3)   VALUE SPACE.
002490        05 口座番号Ｗ                  PIC X(10)  VALUE SPACE.
002500        05 口座名義人カナＷ.
002500           07 口座名義人カナ１Ｗ       PIC X(60)  VALUE SPACE.
002500           07 口座名義人カナ２Ｗ       PIC X(60)  VALUE SPACE.
002501        05 口座名義人Ｗ.
002501           07 口座名義人１Ｗ           PIC X(60)  VALUE SPACE.
002501           07 口座名義人２Ｗ           PIC X(60)  VALUE SPACE.
002510*
002520 01 連番Ｗ                             PIC 9(3)   VALUE ZERO.
002530 01 銀行名支店名Ｗ                     PIC X(82)  VALUE SPACE.
002540 01 預金種別コメントＷ                 PIC N(2)   VALUE SPACE.
002551**
002552* 社保用
002553 01 接尾語区分Ｗ                       PIC 9     VALUE ZERO.
002554*
002560********************
002570* 保険者別合計金額 *
002580********************
002590 01 保険者別合計金額.
002600    03 金額Ｗ                          PIC N(2)  VALUE SPACE.
002610    03 請求メッセージＷ                PIC N(15) VALUE SPACE.
002620    03 円Ｗ                            PIC N(1)  VALUE SPACE.
002630    03 件数Ｗ                          PIC 9(3)  VALUE ZERO.
002640    03 費用額Ｗ                        PIC 9(8)  VALUE ZERO.
002650    03 請求額Ｗ                        PIC 9(7)  VALUE ZERO.
002660    03 請求先名称Ｗ                    PIC X(40) VALUE SPACE.
002670    03 支部部署名Ｗ                    PIC X(40) VALUE SPACE.
002680    03 宛名Ｗ                          PIC X(24) VALUE SPACE.
002690    03 保険者宛名Ｗ.
002700       05 保険者宛名１Ｗ               PIC X(40) VALUE SPACE.
002710       05 保険者宛名２Ｗ               PIC X(40) VALUE SPACE.
002711**
002720 01 画面情報４３０Ｗ.
002730    03 請求年月Ｗ.
002740       05 請求和暦Ｗ                   PIC 9     VALUE ZERO.
002750       05 請求年Ｗ                     PIC 9(2)  VALUE ZERO.
002760       05 請求月Ｗ                     PIC 9(2)  VALUE ZERO.
002770    03 提出年月日Ｗ.
002780       05 提出和暦Ｗ                   PIC 9     VALUE ZERO.
002790       05 提出年Ｗ                     PIC 9(2)  VALUE ZERO.
002800       05 提出月Ｗ                     PIC 9(2)  VALUE ZERO.
002810       05 提出日Ｗ                     PIC 9(2)  VALUE ZERO.
002820    03 印刷種類Ｗ                      PIC 9     VALUE ZERO.
002821***
002822* エラーメッセージ用
002823 01 エラーメッセージＷ.
002824    03 エラー保険種別Ｗ                PIC X(2) VALUE SPACE.
002825    03 エラー区切りＷ                  PIC X(1) VALUE SPACE.
002826    03 エラー保険者番号Ｗ              PIC X(10) VALUE SPACE.
002827    03 FILLER                          PIC X(7) VALUE SPACE.
002828*
002829***
002830 01 協会コードＷ                       PIC 9(2)  VALUE ZERO.
002831*
002841*
002842* その他会用
002844 01 その他編集Ｗ.
002845    03 その他編集内容Ｗ                PIC X(42) VALUE SPACE.
      *
001220 01 分類コードＷＲ                     PIC 9(2) VALUE ZERO.
001220 01 県コードＷＲ                       PIC X(2) VALUE SPACE.
001220 01 府県Ｗ                             PIC X(2) VALUE SPACE.
001220 01 県名Ｗ.
          03 県名ＷＰ                        PIC X(8) VALUE SPACE.
001220 01 保険種別名Ｗ.
          03 保険種別名ＷＰ                  PIC X(8) VALUE SPACE.
002846*
002847***
002852 01 請求書関連Ｗ.
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
002877*
003280 01 カウンタ                           PIC 9(3)  VALUE ZERO.
003290 01 カウンタ２                         PIC 9(3)  VALUE ZERO.
004200 01 商Ｗ                               PIC 9(3)  VALUE ZERO.
004210 01 余Ｗ                               PIC 9(3)  VALUE ZERO.
002878***********************************************************************
002879 01 印刷制御.
002880     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
002881     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
002882     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
002883     03 拡張制御Ｐ.
002890         05 端末制御Ｐ.
002900             07 移動方向Ｐ             PIC X(1).
002910             07 移動行数Ｐ             PIC 9(3).
002920         05 詳細制御Ｐ                 PIC X(2).
002930     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
002940     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
002950*
002960 01 計算機西暦年Ｗ                     PIC 9(2).
002970* 日付ＷＯＲＫ
002980 01 和暦終了年Ｗ                       PIC 9(4).
002990 01 計算機西暦.
003000    03 計算機西暦年                    PIC 9(4).
003010    03 計算機西暦月日                  PIC 9(4).
003020 01 計算機西暦Ｒ REDEFINES 計算機西暦.
003030    03 計算機世紀                      PIC 9(2).
003040    03 計算機日付                      PIC 9(6).
003050    03 計算機日付Ｒ REDEFINES 計算機日付.
003060       05 計算機年月                   PIC 9(4).
003070       05 計算機年月Ｒ REDEFINES 計算機年月.
003080         07 計算機年                   PIC 9(2).
003090         07 計算機月                   PIC 9(2).
003100       05 計算機日                     PIC 9(2).
003110*
003120******************************************************************
003130*                          連結項目                              *
003140******************************************************************
003150*
003160********************
003170* メッセージ表示キー *
003180********************
003184 01 連メ－キー IS EXTERNAL.
003185    03  連メ－メッセージ               PIC N(20).
003186*
003190 01 連メ３－キー IS EXTERNAL.
003200    03  連メ３－メッセージ             PIC N(20).
003210    03  連メ３－メッセージ１           PIC X(20).
003220*
003310*
003311 01 連入－画面情報ＹＨＮ４３０   IS EXTERNAL.
003312    03 連入－請求年月.
003313       05 連入－請求和暦               PIC 9.
003314       05 連入－請求年                 PIC 9(2).
003315       05 連入－請求月                 PIC 9(2).
003316    03 連入－提出年月日.
003317       05 連入－提出和暦               PIC 9.
003318       05 連入－提出年                 PIC 9(2).
003319       05 連入－提出月                 PIC 9(2).
003320       05 連入－提出日                 PIC 9(2).
003321    03 連入－レセプト種類              PIC X(4).
003322    03 連入－保険種別                  PIC 9(2).
003323    03 連入－印刷種類                  PIC 9.
003324    03 連入－本人家族                  PIC 9.
003325    03 連入－用紙種類                  PIC 9.
003326    03 連入－県内県外                  PIC 9.
003327    03 連入－県ＪＩＳ                  PIC X(2).
003328    03 連入－政管ＪＩＳ                PIC X(2).
003338*
003348 01 連入－画面情報ＹＨＮ４３０追加   IS EXTERNAL.
003358    03 連入－一括区分                  PIC 9.
001933    03 連入－作成日印刷                PIC 9.
          03 連入－プレビュー区分            PIC 9.
          03 連入－件数                      PIC 9(5).
          03 連入－処理モード                PIC X(2).
      *
       01 連印－印刷情報ＹＨＮ４３０   IS EXTERNAL.
          03 連印－分類コード      PIC 9(1).
          03 連印－県コード        PIC X(2).
          03 連印－保険順          PIC 9(2).
          03 連印－保険者番号      PIC X(10).
003020*
000540************************************
000550* プリンタファイル作成用           *
000560************************************
000570 01 Ｈ連ＰＲＴＦ－作成データ IS EXTERNAL.
000580     03 Ｈ連ＰＲＴＦ－ファイル名           PIC X(8).
000590     03 Ｈ連ＰＲＴＦ－プレビュー区分       PIC 9.
000600     03 Ｈ連ＰＲＴＦ－帳票プログラム名     PIC X(8).
000610     03 Ｈ連ＰＲＴＦ－オーバレイ名         PIC X(8).
003470*
003480******************************************************************
003490*                      PROCEDURE  DIVISION                       *
003500******************************************************************
003510 PROCEDURE               DIVISION.
003520************
003530*           *
003540* 初期処理   *
003550*           *
003560************
           IF 連印－分類コード = 4
005715         CALL   "YHN438"
005716         CANCEL "YHN438"
003820         EXIT PROGRAM
           END-IF.
002570     PERFORM プリンタファイル作成.
003581     PERFORM 初期化.
003582     PERFORM 制御情報取得２.
003980     PERFORM 施術所情報取得.
003583************
003590*           *
003600* 主処理     *
003610*           *
003620************
           IF 連印－分類コード = ZERO
002484        PERFORM 印刷処理
           ELSE
002484        PERFORM 印刷処理１
           END-IF.
003760************
003770*           *
003780* 終了処理   *
003790*           *
003800************
003810     PERFORM 終了処理.
003820     EXIT PROGRAM.
003830*
003840*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
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
002974     MOVE "YHN437"              TO Ｈ連ＰＲＴＦ－帳票プログラム名.
002975*
002976*--↑↑-----------------------------------------------------*
002980*
002990*   / プレビュー区分セット /
003000     MOVE 連入－プレビュー区分 TO Ｈ連ＰＲＴＦ－プレビュー区分.
003010*
003020     CALL   "CRTPRTF".
003030     CANCEL "CRTPRTF".
003040*
003850*================================================================*
003860 初期化 SECTION.
003870*
003880     PERFORM ファイルオープン.
003890*    /* 現在日付取得 */
003900     ACCEPT 計算機日付 FROM DATE.
003910*    /* 1980～2079年の間で設定 */
003920     IF 計算機年 > 80
003930         MOVE 19 TO 計算機世紀
003940     ELSE
003950         MOVE 20 TO 計算機世紀
003960     END-IF.
003961*
003970     PERFORM 連結項目退避.
003990     PERFORM カレント元号取得.
004020*================================================================*
004030 カレント元号取得 SECTION.
004040*
004050     MOVE ZEROS TO 制－制御区分.
004060     READ 制御情報マスタ
004070     NOT INVALID KEY
004080*         MOVE 制－カレント元号 TO カレント元号Ｗ
004081         MOVE 制－協会コード   TO 協会コードＷ
004090     END-READ.
004100*
004400*================================================================*
004410 連結項目退避 SECTION.
004420*
004430     MOVE 連入－請求和暦  TO 請求和暦Ｗ.
004440     MOVE 連入－請求年    TO 請求年Ｗ.
004450     MOVE 連入－請求月    TO 請求月Ｗ.
004460     MOVE 連入－提出和暦  TO 提出和暦Ｗ.
004470     MOVE 連入－提出年    TO 提出年Ｗ.
004480     MOVE 連入－提出月    TO 提出月Ｗ.
004490     MOVE 連入－提出日    TO 提出日Ｗ.
004500     MOVE 連入－印刷種類  TO 印刷種類Ｗ.
004510*
004520*================================================================*
004530 施術所情報取得 SECTION.
004540*
004550     MOVE ZERO  TO 施情－施術所番号.
004560     READ 施術所情報マスタ
004570     INVALID KEY
004580         CONTINUE
004590     NOT INVALID KEY
004600*
004610         MOVE 施情－郵便番号１       TO 施術所郵便番号１Ｗ
004620         MOVE "-"                    TO 施術所郵便番号区切Ｗ
004630         MOVE 施情－郵便番号２       TO 施術所郵便番号２Ｗ
004640         MOVE 施情－代表者名         TO 代表者名Ｗ
004650         MOVE 施情－接骨院名         TO 接骨院名Ｗ
004660         STRING 施情－住所１  DELIMITED BY SPACE
004670                施情－住所２  DELIMITED BY SPACE
004680           INTO 施術所住所Ｗ
004690         END-STRING
004700         MOVE 施情－電話番号         TO 施術所電話番号Ｗ
004701         MOVE 施情－新柔整師番号     TO 柔整師番号Ｗ
004710*
004720         MOVE 施情－取引先銀行名     TO 取引先銀行名Ｗ
004730         MOVE 施情－取引先銀行支店名 TO 取引先銀行支店名Ｗ
004740         MOVE 施情－預金種別         TO 預金種別Ｗ
004750         MOVE 施情－銀行番号         TO 銀行番号Ｗ
004760         MOVE 施情－店番号           TO 店番号Ｗ
004770         MOVE 施情－口座番号         TO 口座番号Ｗ
004780         MOVE 施情－口座名義人カナ   TO 口座名義人カナＷ
004781         MOVE 施情－口座名義人       TO 口座名義人Ｗ
004790         STRING 取引先銀行名Ｗ     DELIMITED BY SPACE
004800                " "                DELIMITED BY SIZE
004810                取引先銀行支店名Ｗ DELIMITED BY SPACE
004820                INTO 銀行名支店名Ｗ
004830         END-STRING
004840         EVALUATE 預金種別Ｗ
004850         WHEN 1
004860             MOVE NC"普通" TO 預金種別コメントＷ
004870         WHEN 2
004880             MOVE NC"当座" TO 預金種別コメントＷ
004890         WHEN OTHER
004900             MOVE SPACE    TO 預金種別コメントＷ
004910         END-EVALUATE
004920*
004930     END-READ.
004940*================================================================*
004950 ファイルオープン SECTION.
004960*
004970     OPEN INPUT   元号マスタ
004980         MOVE NC"元号" TO ファイル名.
004990         PERFORM オープンチェック.
002650     OPEN INPUT 制御情報マスタ.
002660         MOVE NC"制御" TO ファイル名.
002670         PERFORM オープンチェック.
002680     OPEN INPUT 名称マスタ.
002690             MOVE NC"名称" TO ファイル名.
002700             PERFORM オープンチェック.
005030     OPEN INPUT   保険者マスタ
005040         MOVE NC"保険" TO ファイル名.
005050         PERFORM オープンチェック.
005060     OPEN INPUT   市町村マスタ
005070         MOVE NC"市町" TO ファイル名.
005080         PERFORM オープンチェック.
005120     OPEN INPUT   施術所情報マスタ
005130         MOVE NC"施情" TO ファイル名.
005140         PERFORM オープンチェック.
005150     OPEN INPUT   請求先マスタ
005160         MOVE NC"請先" TO ファイル名.
005170         PERFORM オープンチェック.
005171     OPEN INPUT   会情報マスタ
005172         MOVE NC"会情報" TO ファイル名.
005173         PERFORM オープンチェック.
005174     OPEN INPUT 保険者拡張マスタ.
005175         MOVE NC"保拡" TO ファイル名.
005176         PERFORM オープンチェック.
005180*     OPEN I-O   印刷ファイル
005190*         PERFORM エラー処理Ｐ.
005200*================================================================*
005210 オープンチェック SECTION.
005220*
005230     IF 状態キー  NOT =  "00"
005240         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
005250         DISPLAY NC"状態キー：" 状態キー         UPON CONS
005260         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
005270                                                 UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
005280         ACCEPT  キー入力 FROM CONS
005290         PERFORM ファイル閉鎖
005300         EXIT PROGRAM.
005310*================================================================*
005320 ファイル閉鎖 SECTION.
005330*
002990     IF ( オープンフラグ = "YES" )
002991         CLOSE 印刷ファイル
003041     END-IF.
005340     CLOSE 元号マスタ     制御情報マスタ   保険者マスタ
005350           市町村マスタ   施術所情報マスタ 請求先マスタ  
005360           保険者拡張マスタ 会情報マスタ   名称マスタ.
005370*================================================================*
005380 終了処理 SECTION.
005390*
005400     PERFORM ファイル閉鎖.
005410*================================================================*
005420 エラー表示 SECTION.
005430*
005440     DISPLAY NC"状態キー" 状態キー  UPON CONS.
005450     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
005460     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
005470     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
005480     ACCEPT  キー入力 FROM CONS.
005490     PERFORM ファイル閉鎖.
005500     EXIT PROGRAM.
005510*================================================================*
005520 印刷処理 SECTION.
005530*
005570     OPEN INPUT  作業ファイル２.
005580         MOVE NC"作２" TO ファイル名.
005590         PERFORM オープンチェック.
005530*
           MOVE SPACE     TO  作業フラグ.
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
005626     IF 状態キー = "00"
005627         MOVE SPACE TO 終了フラグ
005628         PERFORM 作業ファイル２読込
005629         IF  終了フラグ = "YES"
005630             MOVE  NC"　該当データなしです。確認して下さい。" TO 連メ－メッセージ
005631             CALL   "MSG001"
005632             CANCEL "MSG001"
005633             PERFORM ファイル閉鎖
005634             MOVE 99 TO PROGRAM-STATUS
005635             EXIT PROGRAM
005636         END-IF
005673*
005699         PERFORM UNTIL  終了フラグ = "YES"
                  IF (作２－分類コード = 4)
                      IF (作業フラグ = SPACE)
002990                   IF ( オープンフラグ = "YES" )
002991                      CLOSE 印刷ファイル
004320                      MOVE SPACE TO オープンフラグ
003041                   END-IF
005715                   CALL   "YHN438"
005716                   CANCEL "YHN438"
                         MOVE "YES" TO 作業フラグ
                      END-IF
003820            ELSE
005700                PERFORM 印刷対象チェック
005701                IF 印刷フラグ = "YES"
005703                   MOVE SPACE TO YHN437P
005704****                INITIALIZE YHN437P
005705                   PERFORM ヘッダセット
005706                   PERFORM 明細セット
                         PERFORM フッタセット
005707                   PERFORM 印字処理
005708                   PERFORM 改頁処理
                      END-IF
005709            END-IF
005710            PERFORM 作業ファイル２読込
005711         END-PERFORM
005712*
005713     ELSE
005714         MOVE  NC"　該当データなしです。確認して下さい。" TO 連メ－メッセージ
005715         CALL   "MSG001"
005716         CANCEL "MSG001"
005717         PERFORM ファイル閉鎖
005718         MOVE 99 TO PROGRAM-STATUS
005719         EXIT PROGRAM
005720     END-IF.
006570*
006580     CLOSE 作業ファイル２.
006581*
005510*================================================================*
005520 印刷処理１ SECTION.
005530*
005570     OPEN INPUT  作業ファイル２.
005580         MOVE NC"作２" TO ファイル名.
005590         PERFORM オープンチェック.
005530*
      * / 並び順変更
003853     MOVE 連入－請求和暦    TO  作２－請求和暦.
003854     MOVE 連入－請求年      TO  作２－請求年.
003855     MOVE 連入－請求月      TO  作２－請求月.
003857     MOVE 連印－分類コード  TO  作２－分類コード.
003856     MOVE 連印－県コード    TO  作２－県コード.
003858     MOVE 連印－保険順      TO  作２－保険順.
003859     MOVE LOW-VALUE         TO  作２－保険者番号.
003860     START 作業ファイル２   KEY IS >=  作２－請求和暦年月
003861                                       作２－分類コード
003862                                       作２－県コード
003863                                       作２－保険順
003864                                       作２－保険者番号
003865     END-START.
005626     IF 状態キー = "00"
005627         MOVE SPACE TO 終了フラグ
005628         PERFORM 作業ファイル２読込
005629         IF  終了フラグ = "YES"
005630             MOVE  NC"　該当データなしです。確認して下さい。" TO 連メ－メッセージ
005631             CALL   "MSG001"
005632             CANCEL "MSG001"
005633             PERFORM ファイル閉鎖
005634             MOVE 99 TO PROGRAM-STATUS
005635             EXIT PROGRAM
005636         END-IF
005673*
003878         PERFORM UNTIL (終了フラグ = "YES") OR
                             (作２－分類コード NOT = 連印－分類コード) OR
                             (作２－保険順     NOT = 連印－保険順) OR
                             (作２－県コード   NOT = 連印－県コード)
005700                PERFORM 印刷対象チェック
005701                IF 印刷フラグ = "YES"
005703                   MOVE SPACE TO YHN437P
005704****                INITIALIZE YHN437P
005705                   PERFORM ヘッダセット
005706                   PERFORM 明細セット
                         PERFORM フッタセット
005707                   PERFORM 印字処理
005708                   PERFORM 改頁処理
005709                END-IF
005710                PERFORM 作業ファイル２読込
005711         END-PERFORM
005712*
005713     ELSE
005714         MOVE  NC"　該当データなしです。確認して下さい。" TO 連メ－メッセージ
005715         CALL   "MSG001"
005716         CANCEL "MSG001"
005717         PERFORM ファイル閉鎖
005718         MOVE 99 TO PROGRAM-STATUS
005719         EXIT PROGRAM
005720     END-IF.
006570*
006580     CLOSE 作業ファイル２.
006581*
006660*================================================================*
006670 作業ファイル２読込 SECTION.
006680*
006690     READ 作業ファイル２ NEXT
006700     AT END
006710         MOVE "YES" TO 終了フラグ
006720     END-READ.
006721*================================================================*
006722 印字処理  SECTION.
006723*
004310     IF ( オープンフラグ NOT = "YES" )
004320        MOVE "YES" TO オープンフラグ
004330        OPEN I-O  印刷ファイル
004340        PERFORM エラー処理Ｐ
004350     END-IF.
013440*
006724     MOVE "YHN437P" TO  定義体名Ｐ.
006725     MOVE SPACE     TO  処理種別Ｐ.
006726     MOVE "SCREEN"  TO  項目群名Ｐ.
006727     WRITE YHN437P.
006728     PERFORM エラー処理Ｐ.
006755*================================================================*
006756 改頁処理  SECTION.
006757*
006760     MOVE "YHN437P" TO  定義体名Ｐ.
006770     MOVE "CT"      TO  処理種別Ｐ.
006780     MOVE "PAGE"    TO  拡張制御Ｐ.
006790     MOVE SPACE     TO  項目群名Ｐ.
006800     WRITE YHN437P.
006810     PERFORM エラー処理Ｐ.
006820     MOVE SPACE     TO  拡張制御Ｐ.
006830*
006840*     CLOSE  印刷ファイル.
006850*     OPEN I-O   印刷ファイル.
006860*     PERFORM エラー処理Ｐ.
006870*
006930*================================================================*
006940 ヘッダセット SECTION.
006950*
006982* 当月の和暦を取得
006983     MOVE 請求和暦Ｗ         TO 元－元号区分.
006984     READ 元号マスタ
006985     INVALID KEY
006986         MOVE SPACE          TO 請求和暦名称
006987     NOT INVALID KEY
006988         MOVE 元－元号名称   TO 請求和暦名称
006989     END-READ.
006990*
006991     MOVE 請求年Ｗ           TO 請求年.
006992     MOVE 請求月Ｗ           TO 請求月.
007001*
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
007210*
007211     MOVE 施術所住所Ｗ       TO 住所.
007220*     MOVE 施術所郵便番号Ｗ   TO 郵便番号.
007230     MOVE 代表者名Ｗ         TO 代表者名.
007240     MOVE 接骨院名Ｗ         TO 接骨院名.
007241     MOVE 柔整師番号Ｗ       TO 柔整師番号.
007250     MOVE 施術所電話番号Ｗ   TO 電話番号.
007251*
007253     PERFORM 銀行セット.
007254*
007260     MOVE 銀行名支店名Ｗ     TO 銀行名支店名.
007270     MOVE 預金種別コメントＷ TO 預金種別.
007280     MOVE 口座番号Ｗ         TO 口座番号.
      */委任者情報対応
007281*     MOVE 口座名義人カナＷ   TO 口座名義人カナ.
007282*     MOVE 口座名義人Ｗ       TO 口座名義人.
007281     MOVE 口座名義人カナ１Ｗ   TO 口座名義人カナ.
007281     MOVE 口座名義人カナ２Ｗ   TO 口座名義人カナ２.
007282     MOVE 口座名義人１Ｗ       TO 口座名義人.
007282     MOVE 口座名義人２Ｗ       TO 口座名義人２.
007283*
007284     MOVE NC"振込先："       TO 振込先表題.
007285*
007292* / 保険者/
007300     MOVE 作２－保険種別     TO 保険種別Ｗ.
007310     MOVE 作２－保険者番号   TO 保険者番号Ｗ.
007320     EVALUATE 保険種別Ｗ
007330     WHEN 1 THRU 4
007340     WHEN 6 THRU 9
007350*     WHEN 70 
007360*     WHEN 80
007370         PERFORM 保険者情報取得
007380     WHEN 5
007390     WHEN 50 THRU 60
007400         PERFORM 市町村情報取得
007410     END-EVALUATE.
007420     PERFORM 結合宛名取得.
007421*
007430     MOVE 保険者宛名Ｗ  TO 保険者名称.
007490*
007500* / 振込番号 /
007510     MOVE 保険種別Ｗ    TO 保拡－保険種別.
007520     MOVE 保険者番号Ｗ  TO 保拡－保険者番号.
007530     READ 保険者拡張マスタ
007540     INVALID KEY
007550         MOVE SPACE           TO 振込番号
007560     NOT INVALID KEY
007570         MOVE 保拡－振込番号  TO 振込番号
007580     END-READ.
007590*
      */保険者番号を印字する/081016
           IF 作２－保険種別 <= 09
               MOVE NC"保険者番号"   TO 保険者番号表題
               MOVE 作２－保険者番号 TO 保険者番号
           END-IF.
007741*================================================================*
007742 明細セット SECTION.
007743*
007760     IF 作２－本人件数 NOT = ZERO
007761        MOVE 作２－本人件数     TO 本人件数
007762        MOVE 作２－本人費用額   TO 本人費用額
007763        MOVE 作２－本人負担額   TO 本人負担額
007770        MOVE 作２－本人請求額   TO 本人請求額
007771     END-IF.
007772     IF 作２－家族件数 NOT = ZERO
007774        MOVE 作２－家族件数     TO 家族件数
007775        MOVE 作２－家族費用額   TO 家族費用額
007776        MOVE 作２－家族負担額   TO 家族負担額
007777        MOVE 作２－家族請求額   TO 家族請求額
007778     END-IF.
007780*
006390*================================================================*
006400 フッタセット SECTION.
006410*
005570     OPEN INPUT  作業ファイル４.
005580         MOVE NC"作４" TO ファイル名.
005590         PERFORM オープンチェック.
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
           EVALUATE 作２－分類コード
           WHEN 5
               EVALUATE 作２－保険順
               WHEN 10
                   MOVE "国保連合会－国保"    TO 保険者名称ＣＭ
               WHEN 20
                   MOVE "国保連合会－後期"    TO 保険者名称ＣＭ
               WHEN 30
                   MOVE 作２－保険種別        TO 名－名称コード
025960             MOVE 12                    TO 名－区分コード
025980             READ 名称マスタ
026010             NOT INVALID KEY
026020                 MOVE 名－略称          TO 保険種別名Ｗ
026030             END-READ
                   IF 作２－保険種別 = 52
                       MOVE "ひとり親"        TO 保険種別名ＷＰ
                   END-IF
                   STRING "(国保連合会－"  DELIMITED BY SIZE
                          保険種別名ＷＰ   DELIMITED BY SPACE
                         ")"              DELIMITED BY SIZE
                     INTO 保険者名称ＣＭ
                   END-STRING
               END-EVALUATE
           WHEN 1
               MOVE "全国健康保険協会支部－協会健保"    TO 保険者名称ＣＭ
           WHEN 2
               MOVE "船員保険"      TO 保険者名称ＣＭ
           WHEN 3
               MOVE "健保組合"      TO 保険者名称ＣＭ
           WHEN 4
               STRING "共済組合("      DELIMITED BY SIZE
                      作２－保険者番号 DELIMITED BY SPACE
                      ")"              DELIMITED BY SIZE
                 INTO 保険者名称ＣＭ
               END-STRING
           WHEN 6
               MOVE 作２－保険種別     TO 名－名称コード
025960         MOVE 12                 TO 名－区分コード
025980         READ 名称マスタ
026010         NOT INVALID KEY
026020             MOVE 名－略称       TO 保険種別名Ｗ
026030         END-READ
               IF 作２－保険種別 = 52
                   MOVE "ひとり親"     TO 保険種別名ＷＰ
               END-IF
               STRING 保険種別名ＷＰ   DELIMITED BY SPACE
                      "("              DELIMITED BY SIZE
                      作２－保険者番号 DELIMITED BY SPACE
                      ")"              DELIMITED BY SIZE
                 INTO 保険者名称ＣＭ
               END-STRING
           END-EVALUATE.
      *
           MOVE 作２－分類コード TO 作４－分類コード.
           MOVE 作２－県コード   TO 作４－県コード.
           MOVE 作２－保険順     TO 作４－保険順.
           MOVE 作２－保険者番号 TO 作４－保険者番号.
           READ 作業ファイル４
           NOT INVALID KEY
              MOVE 作４－６号順番   TO 区分１
              MOVE 作４－７号順番   TO 区分２
              MOVE "*"              TO 区分３
              MOVE "-"              TO 区切１ 区切２
              IF 作４－分類コード = 3 OR 4 OR 6
                 MOVE "×"             TO 無
              END-IF
           END-READ.
006580     CLOSE 作業ファイル４.
      *
008290*================================================================*
008300 エラー処理Ｐ SECTION.
008310*
008320     IF 通知情報Ｐ NOT = "00"
008330         DISPLAY NC"帳票エラー"              UPON CONS
008340         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
008350         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
008360         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
008370         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008380                                             UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
008390         ACCEPT  キー入力 FROM CONS
008400         PERFORM ファイル閉鎖
008410         EXIT PROGRAM
008420     END-IF.
008430*================================================================*
008440 保険者情報取得 SECTION.
008450*
008451     MOVE  SPACE         TO 請求先名称Ｗ.
008452     MOVE  SPACE         TO 支部部署名Ｗ.
008453     MOVE  ZERO          TO 接尾語区分Ｗ.
008454*
008460     MOVE 保険種別Ｗ     TO 保－保険種別.
008470     MOVE 保険者番号Ｗ   TO 保－保険者番号.
008480     READ 保険者マスタ
008490     INVALID KEY
008500         MOVE SPACE      TO 請求先名称Ｗ
008510         MOVE SPACE      TO 支部部署名Ｗ
008520     NOT INVALID KEY
008530         IF 保－請求先情報区分 = 1
008540             MOVE 保－保険種別   TO 請先－保険種別
008550             MOVE 保－保険者番号 TO 請先－保険者番号
008560             READ 請求先マスタ
008570             INVALID KEY
008580                 MOVE SPACE             TO 請求先名称Ｗ
008590                 MOVE SPACE             TO 支部部署名Ｗ
008600             NOT INVALID KEY
008610                 MOVE 請先－保険者名称  TO 請求先名称Ｗ
008620                 MOVE 請先－支部部署名  TO 支部部署名Ｗ
008630             END-READ
008640         ELSE
008650             MOVE 保－保険者名称        TO 請求先名称Ｗ
008660             MOVE 保－支部部署名        TO 支部部署名Ｗ
008661             MOVE 保－接尾語区分        TO 接尾語区分Ｗ
008670         END-IF
008680     END-READ.
008690*================================================================*
008700 市町村情報取得 SECTION.
008710*
008711     MOVE  SPACE         TO 請求先名称Ｗ.
008712     MOVE  SPACE         TO 支部部署名Ｗ.
008713*
008720     MOVE 保険種別Ｗ               TO 市－公費種別.
008730     MOVE 保険者番号Ｗ             TO 市－市町村番号.
008740     READ 市町村マスタ
008750     INVALID KEY
008760         MOVE SPACE                TO 請求先名称Ｗ
008770         MOVE SPACE                TO 支部部署名Ｗ
008780     NOT INVALID KEY
008790         IF 市－請求先区分 = 1
008800             MOVE 保険種別Ｗ       TO 請先－保険種別
008810             MOVE 保険者番号Ｗ     TO 請先－保険者番号
008820             READ 請求先マスタ
008830             INVALID KEY
008840                 MOVE SPACE        TO 請求先名称Ｗ
008850                 MOVE SPACE        TO 支部部署名Ｗ
008860             NOT INVALID KEY
008868                 MOVE 請先－保険者名称   TO 請求先名称Ｗ
008869                 MOVE 請先－支部部署名   TO 支部部署名Ｗ
008890             END-READ
008900          ELSE
008908             MOVE 市－市町村名称   TO 請求先名称Ｗ
008909             MOVE 市－支部部署名   TO 支部部署名Ｗ
008930          END-IF
008940      END-READ.
008950*================================================================*
008960 結合宛名取得 SECTION.
008970*
008971     MOVE SPACE TO 保険者宛名Ｗ.
008972     IF 請求先名称Ｗ NOT = SPACE
008980         EVALUATE 保険種別Ｗ
008981         WHEN 2
008982             IF 接尾語区分Ｗ = 1
008983                MOVE SPACE            TO 宛名Ｗ
008984             ELSE
008985                MOVE "社会保険事務所" TO 宛名Ｗ
008986             END-IF
008987         WHEN 6
008988             IF 接尾語区分Ｗ = 1
008989*                MOVE "（日雇）"               TO 宛名Ｗ
                      CONTINUE
008991             ELSE
008992*                MOVE "社会保険事務所（日雇）" TO 宛名Ｗ
008992                MOVE "社会保険事務所" TO 宛名Ｗ
008993             END-IF
008994         WHEN 7
008995*             MOVE "（船員）"       TO 宛名Ｗ
                   CONTINUE
009020         WHEN 3
009030             MOVE "健康保険組合"   TO 宛名Ｗ
009031         WHEN 4
009032             MOVE "共済組合"       TO 宛名Ｗ
009033         WHEN 8
009034             MOVE "（退職）"       TO 宛名Ｗ
009040         WHEN OTHER
009050             MOVE SPACE            TO 宛名Ｗ
009060         END-EVALUATE
009070*
009080         IF 支部部署名Ｗ = SPACE
009090             STRING  請求先名称Ｗ  DELIMITED BY SPACE
009100                     宛名Ｗ        DELIMITED BY SPACE
009110                     "  殿"        DELIMITED BY SIZE
009120                    INTO 保険者宛名Ｗ
009130             END-STRING
009140         ELSE
009150             STRING  請求先名称Ｗ  DELIMITED BY SPACE
009160                     宛名Ｗ        DELIMITED BY SPACE
009170                     " "           DELIMITED BY SIZE
009180                     支部部署名Ｗ  DELIMITED BY SPACE
009190                     "  殿"        DELIMITED BY SIZE
009200                    INTO 保険者宛名Ｗ
009210             END-STRING
009220         END-IF
009221     END-IF.
009222*
009434*================================================================*
009435*================================================================*
009436 制御情報取得２ SECTION.
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
009469*
007591*================================================================*
007592 銀行セット  SECTION.
007593*
009473* 会の振込先印字設定の時
009474*
009475* 初期化
009476      PERFORM 銀行情報クリアー.
009477*
009479      MOVE ZERO          TO 会情－柔整鍼灸区分.
009478      MOVE 協会コードＷ  TO 会情－協会コード.
009479      MOVE ZERO          TO 会情－保険種別.
009480      MOVE ZERO          TO 会情－変更和暦年月.
009481      READ 会情報マスタ
009482      NOT INVALID KEY
009483            MOVE 会情－取引先銀行名      TO 取引先銀行名Ｗ
009484            MOVE 会情－取引先銀行支店名  TO 取引先銀行支店名Ｗ
009485            MOVE 会情－預金種別          TO 預金種別Ｗ
009486            MOVE 会情－銀行番号          TO 銀行番号Ｗ
009487            MOVE 会情－店番号            TO 店番号Ｗ
009488            MOVE 会情－口座番号          TO 口座番号Ｗ
009489            MOVE 会情－口座名義人カナ    TO 口座名義人カナＷ
009490            MOVE 会情－口座名義人        TO 口座名義人Ｗ
009491            MOVE SPACE TO 銀行名支店名Ｗ
009492            STRING 取引先銀行名Ｗ     DELIMITED BY SPACE
009493                   " "                DELIMITED BY SIZE
009494                   取引先銀行支店名Ｗ DELIMITED BY SPACE
009495                   INTO 銀行名支店名Ｗ
009496            END-STRING
009497            EVALUATE 預金種別Ｗ
009498            WHEN 1
009499                MOVE NC"普通" TO 預金種別コメントＷ
009500            WHEN 2
009501                MOVE NC"当座" TO 預金種別コメントＷ
009502            WHEN OTHER
009503                MOVE SPACE    TO 預金種別コメントＷ
009504            END-EVALUATE
009505      END-READ.
009506*
009507*================================================================*
009508 銀行情報クリアー  SECTION.
009509*
009510* 会の振込先印字なし
009511*
009512      MOVE SPACE TO 取引先銀行名Ｗ.
009513      MOVE SPACE TO 取引先銀行支店名Ｗ.
009514      MOVE ZERO  TO 預金種別Ｗ.
009515      MOVE SPACE TO 銀行番号Ｗ.
009516      MOVE SPACE TO 店番号Ｗ.
009517      MOVE SPACE TO 口座番号Ｗ.
009518      MOVE SPACE TO 口座名義人カナＷ.
009519      MOVE SPACE TO 口座名義人Ｗ.
009520      MOVE SPACE TO 銀行名支店名Ｗ.
009521      MOVE SPACE TO 預金種別コメントＷ.
009522*
009538*================================================================*
009539 印刷対象チェック  SECTION.
009540*
009541*  印刷区分による振り分け → ６・７号請求書（印刷区分 0:印刷 1:印刷しない）
009542* （一括印刷のみ）
009543*
009544*     MOVE SPACE TO 印刷フラグ.
009545*
009546*     IF 連入－一括区分 NOT = 1
009547        MOVE "YES" TO 印刷フラグ
009548*     ELSE
009549*        EVALUATE 作２－保険種別
009550*        WHEN 01
009551*        WHEN 08
009552*           IF 国保７号印刷区分Ｗ NOT = 1
009553*              MOVE "YES" TO 印刷フラグ
009554*           END-IF
009555*        WHEN 02
009556*        WHEN 06
009557*        WHEN 07
009558*           IF 社保７号印刷区分Ｗ NOT = 1
009559*              MOVE "YES" TO 印刷フラグ
009560*           END-IF
009561*        WHEN 03
009562*           IF 組合７号印刷区分Ｗ NOT = 1
009563*              MOVE "YES" TO 印刷フラグ
009564*           END-IF
009565*        WHEN 04
009566*        WHEN 09
009567*           IF 共済７号印刷区分Ｗ NOT = 1
009568*              MOVE "YES" TO 印刷フラグ
009569*           END-IF
009570*        WHEN 05
009571*           IF 老人７号印刷区分Ｗ NOT = 1
009572*              MOVE "YES" TO 印刷フラグ
009573*           END-IF
009574*        WHEN 50 THRU 60
009575*           IF 助成７号印刷区分Ｗ NOT = 1
009576*              MOVE "YES" TO 印刷フラグ
009577*           END-IF
009578*        WHEN OTHER
009579*           MOVE "YES" TO 印刷フラグ
009580*        END-EVALUATE
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
                    ((作２－保険種別 >= 50) AND (作２－保険種別 <= 60))
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
009582*
009605*================================================================*
009606******************************************************************
009607 END PROGRAM YHN437.
009608******************************************************************
