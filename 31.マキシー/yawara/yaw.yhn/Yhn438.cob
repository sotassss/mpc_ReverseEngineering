000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHN438.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*      ８号用紙（共済用）   請求書【印刷】柔+ｳｨﾝﾄﾞｳｽﾞ版
000100*  請求年月バージョン
000101*         MED = YHN438P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2015-03-04
000130 DATE-COMPILED.          2015-03-04
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
000108     SELECT  作業ファイル共  ASSIGN      TO        "C:\MAKISHISYS\YAWOBJ\TEMP\W43121L.DAT"
000109                             ORGANIZATION             IS  INDEXED
000110                             ACCESS                   IS  DYNAMIC
000111                             RECORD      KEY          IS  作共－請求和暦年月
000112                                                          作共－分類コード
000112                                                          作共－県コード
000112                                                          作共－保険順
000113                                                          作共－保険者番号
000911                                                          作共－患者コード
000912                                                          作共－施術和暦年月
000119                             FILE        STATUS       IS  状態キー
000120                             LOCK        MODE         IS  AUTOMATIC.
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
001250*                           ［ＲＬ＝  １２８］
001260 FD  施術所情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001270     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
001280*                           ［ＲＬ＝  １２８］
001290 FD  請求先マスタ          BLOCK   CONTAINS   1   RECORDS.
001300     COPY SEIKYUS         OF  XFDLIB  JOINING   請先   AS  PREFIX.
001302*                          ［ＲＬ＝  ６４０］
001303 FD  会情報マスタ        BLOCK   CONTAINS   1   RECORDS.
001304     COPY KAIJOHO         OF  XFDLIB  JOINING   会情   AS  PREFIX.
000174*                           ［ＲＬ＝  １２８］
000175 FD  作業ファイル共 RECORD  CONTAINS 128 CHARACTERS.
000176 01  作共－レコード.
000177     03  作共－レコードキー.
000178         05  作共－請求和暦年月.
000179             07  作共－請求和暦              PIC 9.
000180             07  作共－請求年                PIC 9(2).
000181             07  作共－請求月                PIC 9(2).
001261         05  作共－分類コード                PIC 9(1).
001261         05  作共－県コード                  PIC X(2).
001400         05  作共－保険順                    PIC 9(2).
000183         05  作共－保険者番号                PIC X(10).
001310         05  作共－患者コード.
001320             07 作共－患者番号               PIC 9(6).
001330             07 作共－枝番                   PIC X(1).
001340         05  作共－施術和暦年月.
001350             07  作共－施術和暦              PIC 9.
001360             07  作共－施術年                PIC 9(2).
001370             07  作共－施術月                PIC 9(2).
000188     03  作共－レコードデータ.
001400         05  作共－保険種別                  PIC 9(2).
001280         05  作共－本人家族区分              PIC 9.
000166         05  作共－患者氏名                  PIC X(50).
000192         05  作共－請求額                    PIC 9(9).
000197         05  作共－実日数                    PIC 9(4).
000201         05  FILLER                          PIC X(30).
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
001720     COPY YHN438P        OF  XMDLIB.
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
002640 01 脱出フラグ                         PIC X(3)  VALUE SPACE.
001210 01 オープンフラグ                     PIC X(3)   VALUE SPACE.
001930*
001940 01 行カウンタ                         PIC 9(2)  VALUE ZERO.
001950 01 頁Ｗ                               PIC 9(4)  VALUE ZERO.
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
002330    03 代表者カナＷ                    PIC X(50)  VALUE SPACE.
002330    03 代表者名Ｗ                      PIC X(50)  VALUE SPACE.
002340    03 接骨院名Ｗ                      PIC X(50)  VALUE SPACE.
002341    03 柔整師番号Ｗ                    PIC X(20)  VALUE SPACE.
002350    03 施術所住所Ｗ.
002360       05 施術所住所１Ｗ               PIC X(50)  VALUE SPACE.
002370       05 施術所住所２Ｗ               PIC X(50)  VALUE SPACE.
002380    03 施術所郵便番号Ｗ.
002400       05 施術所郵便番号記号Ｗ         PIC X(2)   VALUE SPACE.
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
002540 01 預金種別コメントＷ                 PIC X(6)   VALUE SPACE.
002551**
002552* 社保用
002553 01 接尾語区分Ｗ                       PIC 9     VALUE ZERO.
002554*
002560********************
002570* 保険者別合計金額 *
002580********************
002590 01 明細Ｗ.
          03 表項目Ｗ                        OCCURS 99.
002600       05 本人番号Ｗ                   PIC 9(2)  VALUE ZERO.
002610       05 被保険者氏名Ｗ               PIC X(50) VALUE SPACE.
002650       05 本人請求額Ｗ                 PIC 9(7)  VALUE ZERO.
002650       05 本人実日数Ｗ                 PIC 9(2)  VALUE ZERO.
002600       05 家族番号Ｗ                   PIC 9(2)  VALUE ZERO.
002610       05 患者氏名Ｗ                   PIC X(50) VALUE SPACE.
002650       05 家族請求額Ｗ                 PIC 9(7)  VALUE ZERO.
002650       05 家族実日数Ｗ                 PIC 9(2)  VALUE ZERO.
          03 集計Ｗ.
002650       05 本人合計件数Ｗ               PIC 9(2)  VALUE ZERO.
002650       05 本人合計請求額Ｗ             PIC 9(8)  VALUE ZERO.
002650       05 本人合計実日数Ｗ             PIC 9(3)  VALUE ZERO.
002650       05 家族合計件数Ｗ               PIC 9(2)  VALUE ZERO.
002650       05 家族合計請求額Ｗ             PIC 9(8)  VALUE ZERO.
002650       05 家族合計実日数Ｗ             PIC 9(3)  VALUE ZERO.
       01 合計Ｗ.
002650    03 合計件数Ｗ                      PIC 9(4)  VALUE ZERO.
002650    03 合計請求額Ｗ                    PIC 9(9)  VALUE ZERO.
002650    03 合計実日数Ｗ                    PIC 9(5)  VALUE ZERO.
003280 01 本人カウンタ                       PIC 9(2)  VALUE ZERO.
003280 01 家族カウンタ                       PIC 9(2)  VALUE ZERO.
002590 01 ヘッダＷ.
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
007330    03 共済番号Ｗ                      PIC X(28)  VALUE SPACE.
003630*
003640* 共済番号用
003650 01 共済連番号集団Ｗ.
003660    03 共済連番号名Ｗ                  PIC X(14)  VALUE SPACE.
003670    03 共済連番号名ＮＷ REDEFINES  共済連番号名Ｗ  PIC N(7).
003680    03 共済連番号Ｗ                    PIC X(6)  VALUE SPACE.
003690    03 共済連番号単位Ｗ                PIC X(2)  VALUE SPACE.
003700    03 共済連番号単位ＮＷ REDEFINES  共済連番号単位Ｗ  PIC N.
003710*
003720* 自衛官番号用
003730 01 自衛官番号集団Ｗ.
003740    03 自衛官番号名Ｗ                  PIC X(8)  VALUE SPACE.
003750    03 自衛官番号名ＮＷ REDEFINES  自衛官番号名Ｗ  PIC N(4).
003760    03 自衛官番号Ｗ                    PIC X(6)  VALUE SPACE.
003770    03 自衛官番号単位Ｗ                PIC X(2)  VALUE SPACE.
003780    03 自衛官番号単位ＮＷ REDEFINES  自衛官番号単位Ｗ  PIC N.
002846*
002847***
002848* 請求書印刷パラメタ用
002849*  ６・７号請求書（印刷区分 0:印刷 1:印刷しない、振込先 0:自分 1:会 9:印刷しない）
002850*  当社用紙初期値1:印刷しない
002851***
002852 01 請求書関連Ｗ.
002853        07 国保６号印刷区分Ｗ          PIC 9 VALUE ZERO.
002854        07 国保７号印刷区分Ｗ          PIC 9 VALUE ZERO.
002855        07 国保当社印刷区分Ｗ          PIC 9 VALUE 1.
002856        07 国保振込先区分Ｗ            PIC 9 VALUE ZERO.
002857        07 社保６号印刷区分Ｗ          PIC 9 VALUE ZERO.
002858        07 社保７号印刷区分Ｗ          PIC 9 VALUE ZERO.
002859        07 社保当社印刷区分Ｗ          PIC 9 VALUE 1.
002860        07 社保振込先区分Ｗ            PIC 9 VALUE ZERO.
002861        07 組合６号印刷区分Ｗ          PIC 9 VALUE ZERO.
002862        07 組合７号印刷区分Ｗ          PIC 9 VALUE ZERO.
002863        07 組合当社印刷区分Ｗ          PIC 9 VALUE 1.
002864        07 組合振込先区分Ｗ            PIC 9 VALUE ZERO.
002865        07 共済６号印刷区分Ｗ          PIC 9 VALUE ZERO.
002866        07 共済７号印刷区分Ｗ          PIC 9 VALUE ZERO.
002867        07 共済当社印刷区分Ｗ          PIC 9 VALUE 1.
002868        07 共済振込先区分Ｗ            PIC 9 VALUE ZERO.
002869        07 老人６号印刷区分Ｗ          PIC 9 VALUE ZERO.
002870        07 老人７号印刷区分Ｗ          PIC 9 VALUE ZERO.
002871        07 老人当社印刷区分Ｗ          PIC 9 VALUE 1.
002872        07 老人振込先区分Ｗ            PIC 9 VALUE ZERO.
002873        07 助成６号印刷区分Ｗ          PIC 9 VALUE ZERO.
002874        07 助成７号印刷区分Ｗ          PIC 9 VALUE ZERO.
002875        07 助成当社印刷区分Ｗ          PIC 9 VALUE 1.
002876        07 助成振込先区分Ｗ            PIC 9 VALUE ZERO.
002862        07 ６７並び順区分Ｗ            PIC 9 VALUE ZERO.
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
002570     PERFORM プリンタファイル作成.
003581     PERFORM 初期化.
003583************
003590*           *
003600* 主処理     *
003610*           *
003620************
002484     PERFORM 印刷処理.
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
002974     MOVE "YHN438"              TO Ｈ連ＰＲＴＦ－帳票プログラム名.
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
004620         MOVE "〒"                   TO 施術所郵便番号記号Ｗ
004610         MOVE 施情－郵便番号１       TO 施術所郵便番号１Ｗ
004620         MOVE "-"                    TO 施術所郵便番号区切Ｗ
004630         MOVE 施情－郵便番号２       TO 施術所郵便番号２Ｗ
004640         MOVE 施情－代表者カナ       TO 代表者カナＷ
004640         MOVE 施情－代表者名         TO 代表者名Ｗ
004650         MOVE 施情－接骨院名         TO 接骨院名Ｗ
004660         MOVE 施情－住所１           TO 施術所住所１Ｗ
004670         MOVE 施情－住所２           TO 施術所住所２Ｗ
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
004860             MOVE "普通"   TO 預金種別コメントＷ
004870         WHEN 2
004880             MOVE "当座"   TO 預金種別コメントＷ
004890         WHEN OTHER
004900             MOVE SPACE    TO 預金種別コメントＷ
004910         END-EVALUATE
023320         EVALUATE 保険種別Ｗ 
023330         WHEN  04
023340             PERFORM 共済番号セット
023350         WHEN  09
023360             PERFORM 自衛官番号セット
023370         END-EVALUATE
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
005120     OPEN INPUT   施術所情報マスタ
005130         MOVE NC"施情" TO ファイル名.
005140         PERFORM オープンチェック.
005150     OPEN INPUT   請求先マスタ
005160         MOVE NC"請先" TO ファイル名.
005170         PERFORM オープンチェック.
005171     OPEN INPUT   会情報マスタ
005172         MOVE NC"会情報" TO ファイル名.
005173         PERFORM オープンチェック.
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
005350           施術所情報マスタ 請求先マスタ  
005360           会情報マスタ   名称マスタ.
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
005570     OPEN INPUT  作業ファイル共.
005580         MOVE NC"作共" TO ファイル名.
005590         PERFORM オープンチェック.
005570     OPEN INPUT  作業ファイル４.
005580         MOVE NC"作４" TO ファイル名.
005590         PERFORM オープンチェック.
005530*
005627     MOVE SPACE TO 終了フラグ
005628     PERFORM 作業ファイル共読込
005673*
005699     PERFORM UNTIL  終了フラグ = "YES"
007300             MOVE 作共－保険種別     TO 保険種別Ｗ
007310             MOVE 作共－保険者番号   TO 保険者番号Ｗ
                   INITIALIZE 明細Ｗ
                   INITIALIZE 合計Ｗ
                   MOVE 1  TO 本人カウンタ
                   MOVE 1  TO 家族カウンタ
003889             PERFORM UNTIL ( 保険者番号Ｗ     NOT = 作共－保険者番号 ) OR
003893                           ( 終了フラグ = "YES" )
005706                PERFORM 明細セット
005710                PERFORM 作業ファイル共読込
                   END-PERFORM
                   MOVE 1     TO カウンタ
                   MOVE 1     TO 頁Ｗ
003889             PERFORM UNTIL ((被保険者氏名Ｗ(カウンタ) = SPACE) AND
                                  (患者氏名Ｗ(カウンタ) = SPACE))
005703                MOVE SPACE TO YHN438P
005705                PERFORM ヘッダセット
                      PERFORM フッタセット
003889                PERFORM VARYING 行カウンタ FROM 1 BY 1
003890                        UNTIL ( 行カウンタ > 10 ) OR
                                    ((被保険者氏名Ｗ(カウンタ) = SPACE) AND
                                     (患者氏名Ｗ(カウンタ) = SPACE))
                         MOVE 本人番号Ｗ(カウンタ)     TO 本人番号(行カウンタ)
                         MOVE 被保険者氏名Ｗ(カウンタ) TO 被保険者氏名(行カウンタ)
                         MOVE 本人請求額Ｗ(カウンタ)   TO 本人請求額(行カウンタ)
                         MOVE 本人実日数Ｗ(カウンタ)   TO 本人実日数(行カウンタ)
                         MOVE 家族番号Ｗ(カウンタ)     TO 家族番号(行カウンタ)
                         MOVE 患者氏名Ｗ(カウンタ)     TO 患者氏名(行カウンタ)
                         MOVE 家族請求額Ｗ(カウンタ)   TO 家族請求額(行カウンタ)
                         MOVE 家族実日数Ｗ(カウンタ)   TO 家族実日数(行カウンタ)
                         COMPUTE カウンタ = カウンタ + 1
                      END-PERFORM
                      MOVE 本人合計件数Ｗ    TO 本人件数合計
                      MOVE 本人合計請求額Ｗ  TO 本人請求額合計
                      MOVE 本人合計実日数Ｗ  TO 本人実日数合計
                      MOVE 家族合計件数Ｗ    TO 家族件数合計
                      MOVE 家族合計請求額Ｗ  TO 家族請求額合計
                      MOVE 家族合計実日数Ｗ  TO 家族実日数合計
                      IF ((被保険者氏名Ｗ(カウンタ) NOT = SPACE) OR
                          (患者氏名Ｗ(カウンタ) NOT = SPACE))
                          MOVE 頁Ｗ   TO 頁
                          COMPUTE 頁Ｗ = 頁Ｗ + 1
                      ELSE
                          MOVE 合計件数Ｗ    TO 合計件数
                          MOVE 合計請求額Ｗ  TO 合計請求額
                          MOVE 合計実日数Ｗ  TO 合計実日数
                      END-IF
005707                PERFORM 印字処理
005708                PERFORM 改頁処理
                      INITIALIZE 明細Ｗ
                   END-PERFORM
005711     END-PERFORM.
005712*
006580     CLOSE 作業ファイル共 作業ファイル４.
006581*
006660*================================================================*
006670 作業ファイル共読込 SECTION.
006680*
006690     READ 作業ファイル共 NEXT
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
006724     MOVE "YHN438P" TO  定義体名Ｐ.
006725     MOVE SPACE     TO  処理種別Ｐ.
006726     MOVE "SCREEN"  TO  項目群名Ｐ.
006727     WRITE YHN438P.
006728     PERFORM エラー処理Ｐ.
006755*================================================================*
006756 改頁処理  SECTION.
006757*
006760     MOVE "YHN438P" TO  定義体名Ｐ.
006770     MOVE "CT"      TO  処理種別Ｐ.
006780     MOVE "PAGE"    TO  拡張制御Ｐ.
006790     MOVE SPACE     TO  項目群名Ｐ.
006800     WRITE YHN438P.
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
003980     PERFORM 施術所情報取得.
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
007110     MOVE 提出和暦Ｗ         TO 元－元号区分.
007120     READ 元号マスタ
007130     INVALID KEY
007140        MOVE SPACE          TO 作成和暦
007150     NOT INVALID KEY
007160        MOVE 元－元号名称   TO 作成和暦
007170     END-READ.
007180     MOVE 提出年Ｗ          TO 作成年.
007190     MOVE 提出月Ｗ          TO 作成月.
007200     MOVE 提出日Ｗ          TO 作成日.
007210*
007211     MOVE 施術所住所１Ｗ     TO 住所１.
007211     MOVE 施術所住所２Ｗ     TO 住所２.
007220     MOVE 施術所郵便番号Ｗ   TO 郵便番号.
007230     MOVE 代表者カナＷ       TO 代表者カナ.
007230     MOVE 代表者名Ｗ         TO 代表者名.
007240     MOVE 接骨院名Ｗ         TO 接骨院名.
007241     MOVE 柔整師番号Ｗ       TO 柔整師番号.
           MOVE 共済番号Ｗ         TO 共済番号.
007250     MOVE 施術所電話番号Ｗ   TO 電話番号.
007251*
007253     PERFORM 銀行セット.
007254*
007260     MOVE 銀行名支店名Ｗ     TO 銀行名支店名.
007270     MOVE 預金種別コメントＷ TO 口座種別.
007280     MOVE 口座番号Ｗ         TO 口座番号.
007281     MOVE 口座名義人カナＷ   TO 名義カナ.
007282     MOVE 口座名義人Ｗ       TO 口座名義.
007285*
007292* / 保険者/
007370     PERFORM 保険者情報取得.
007420     PERFORM 結合宛名取得.
007421*
007430     MOVE 保険者宛名Ｗ  TO 保険者名.
007490*
007741*================================================================*
007742 明細セット SECTION.
007743*
007760     IF 作共－本人家族区分 = 1
007770        MOVE 本人カウンタ   TO 本人番号Ｗ(本人カウンタ)
007770        MOVE 作共－患者氏名 TO 被保険者氏名Ｗ(本人カウンタ)
007770        MOVE 作共－請求額   TO 本人請求額Ｗ(本人カウンタ)
007770        MOVE 作共－実日数   TO 本人実日数Ｗ(本人カウンタ)
              COMPUTE 本人カウンタ = 本人カウンタ + 1
              COMPUTE 本人合計件数Ｗ = 本人合計件数Ｗ + 1
              COMPUTE 本人合計請求額Ｗ = 本人合計請求額Ｗ + 作共－請求額
              COMPUTE 本人合計実日数Ｗ = 本人合計実日数Ｗ + 作共－実日数
007771     ELSE
007770        MOVE 家族カウンタ   TO 家族番号Ｗ(家族カウンタ)
007770        MOVE 作共－患者氏名 TO 患者氏名Ｗ(家族カウンタ)
007770        MOVE 作共－請求額   TO 家族請求額Ｗ(家族カウンタ)
007770        MOVE 作共－実日数   TO 家族実日数Ｗ(家族カウンタ)
              COMPUTE 家族カウンタ = 家族カウンタ + 1
              COMPUTE 家族合計件数Ｗ = 家族合計件数Ｗ + 1
              COMPUTE 家族合計請求額Ｗ = 家族合計請求額Ｗ + 作共－請求額
              COMPUTE 家族合計実日数Ｗ = 家族合計実日数Ｗ + 作共－実日数
007778     END-IF.
           COMPUTE 合計件数Ｗ = 合計件数Ｗ + 1.
           COMPUTE 合計請求額Ｗ = 合計請求額Ｗ + 作共－請求額.
           COMPUTE 合計実日数Ｗ = 合計実日数Ｗ + 作共－実日数.
007780*
006390*================================================================*
006400 フッタセット SECTION.
006410*
      */ 保険種別、県名、被保険者名を下空間に印刷 
           MOVE 保険者番号Ｗ(3:2)      TO 名－名称コード.
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
           STRING "共済組合("          DELIMITED BY SIZE
                  保険者番号Ｗ         DELIMITED BY SPACE
                  ")"                  DELIMITED BY SIZE
             INTO 保険者名称
           END-STRING.
      *
           MOVE 作共－分類コード TO 作４－分類コード.
           MOVE 作共－県コード   TO 作４－県コード.
           MOVE 作共－保険順     TO 作４－保険順.
           MOVE 保険者番号Ｗ     TO 作４－保険者番号.
           READ 作業ファイル４
           NOT INVALID KEY
              MOVE 作４－６号順番   TO 区分１
              MOVE "×"             TO 無
              MOVE 作４－７号順番   TO 区分２
              MOVE "*"              TO 区分３
              MOVE "-"              TO 区切１ 区切２
           END-READ.
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
008950*================================================================*
008960 結合宛名取得 SECTION.
008970*
008971     MOVE SPACE TO 保険者宛名Ｗ.
008972     IF 請求先名称Ｗ NOT = SPACE
008980         EVALUATE 保険種別Ｗ
009031         WHEN 4
009032             MOVE "共済組合"       TO 宛名Ｗ
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
009499                MOVE "普通"   TO 預金種別コメントＷ
009500            WHEN 2
009501                MOVE "当座"   TO 預金種別コメントＷ
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
023970*================================================================*
023980 共済番号セット SECTION.
023990*
024000**************************************************************
024010* 保険者番号により、共済の番号を印字するか、柔整師番号か判定
024020**************************************************************
024030** 1.共済組合連盟
024040     MOVE SPACE  TO  脱出フラグ.
024050     IF ( 施情－共済連番号 NOT = ZERO )
024060** 条件(保険者番号)
024070        IF ( 保険者番号Ｗ(1:2) = "31" )  OR
024080           ( 保険者番号Ｗ = "34130021" )
024090*
024100           MOVE  NC"共済組合連盟第"   TO 共済連番号名ＮＷ 
024110           MOVE  NC"号"               TO 共済連番号単位ＮＷ 
024120           MOVE  施情－共済連番号     TO 共済連番号Ｗ
024130           IF    (共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
024140                 MOVE SPACE TO  共済連番号Ｗ(1:1)
024150           ELSE
024160                 MOVE "YES" TO  脱出フラグ
024170           END-IF
024180           IF    (共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
024190                 MOVE SPACE TO  共済連番号Ｗ(2:1)
024200           ELSE
024210                 MOVE "YES" TO  脱出フラグ
024220           END-IF
024230           IF    (共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
024240                 MOVE SPACE TO  共済連番号Ｗ(3:1)
024250           ELSE
024260                 MOVE "YES" TO  脱出フラグ
024270           END-IF
024280           IF    (共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
024290                 MOVE SPACE TO  共済連番号Ｗ(4:1)
024300           ELSE
024310                 MOVE "YES" TO  脱出フラグ
024320           END-IF
024330           IF    (共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
024340                 MOVE SPACE TO  共済連番号Ｗ(5:1)
024350           ELSE
024360                 MOVE "YES" TO  脱出フラグ
024370           END-IF
024380           IF    (共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
024390                 MOVE SPACE TO  共済連番号Ｗ(6:1)
024400           ELSE
024410                 MOVE "YES" TO  脱出フラグ
024420           END-IF
024110            MOVE  共済連番号集団Ｗ     TO 共済番号Ｗ
024440        END-IF
024450     END-IF.
024460*
027620** 2. 地共済協議会
027630     MOVE SPACE  TO  脱出フラグ.
027640     IF 施情－地共済連番号 NOT = ZERO
027650** 条件(保険者番号)
027660        IF ( 保険者番号Ｗ(1:2) = "32" OR "33" OR "34" )  AND
027670           ( 保険者番号Ｗ NOT = "34130021" )
027680*
027690           MOVE  NC"地共済協議会"     TO 共済連番号名ＮＷ 
027700           MOVE  NC"号"               TO 共済連番号単位ＮＷ 
027710           MOVE  施情－地共済連番号   TO 共済連番号Ｗ
027720           IF    (共済連番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
027730                 MOVE SPACE TO  共済連番号Ｗ(1:1)
027740           ELSE
027750                 MOVE "YES" TO  脱出フラグ
027760           END-IF
027770           IF    (共済連番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
027780                 MOVE SPACE TO  共済連番号Ｗ(2:1)
027790           ELSE
027800                 MOVE "YES" TO  脱出フラグ
027810           END-IF
027820           IF    (共済連番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
027830                 MOVE SPACE TO  共済連番号Ｗ(3:1)
027840           ELSE
027850                 MOVE "YES" TO  脱出フラグ
027860           END-IF
027870           IF    (共済連番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
027880                 MOVE SPACE TO  共済連番号Ｗ(4:1)
027890           ELSE
027900                 MOVE "YES" TO  脱出フラグ
027910           END-IF
027920           IF    (共済連番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
027930                 MOVE SPACE TO  共済連番号Ｗ(5:1)
027940           ELSE
027950                 MOVE "YES" TO  脱出フラグ
027960           END-IF
027970           IF    (共済連番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
027980                 MOVE SPACE TO  共済連番号Ｗ(6:1)
027990           ELSE
028000                 MOVE "YES" TO  脱出フラグ
028010           END-IF
024110            MOVE  共済連番号集団Ｗ     TO 共済番号Ｗ
028050        END-IF
028060     END-IF.
024900*
024910*================================================================*
024920 自衛官番号セット SECTION.
024930*
028110     MOVE SPACE  TO  脱出フラグ.
028120     IF 施情－自衛官番号 NOT = ZERO
028130           IF 施情－防衛省区分 = 1
028140              MOVE  NC"防衛省第"      TO 自衛官番号名ＮＷ 
028150           ELSE
028160              MOVE  NC"防衛庁第"      TO 自衛官番号名ＮＷ 
028170           END-IF
028180           MOVE  NC"号"               TO 自衛官番号単位ＮＷ 
028190           MOVE  施情－自衛官番号     TO 自衛官番号Ｗ
028200           IF    (自衛官番号Ｗ(1:1) = "0")  AND (脱出フラグ  = SPACE )
028210                 MOVE SPACE TO  自衛官番号Ｗ(1:1)
028220           ELSE
028230                 MOVE "YES" TO  脱出フラグ
028240           END-IF
028250           IF    (自衛官番号Ｗ(2:1) = "0")  AND (脱出フラグ  = SPACE )
028260                 MOVE SPACE TO  自衛官番号Ｗ(2:1)
028270           ELSE
028280                 MOVE "YES" TO  脱出フラグ
028290           END-IF
028300           IF    (自衛官番号Ｗ(3:1) = "0")  AND (脱出フラグ  = SPACE )
028310                 MOVE SPACE TO  自衛官番号Ｗ(3:1)
028320           ELSE
028330                 MOVE "YES" TO  脱出フラグ
028340           END-IF
028350           IF    (自衛官番号Ｗ(4:1) = "0")  AND (脱出フラグ  = SPACE )
028360                 MOVE SPACE TO  自衛官番号Ｗ(4:1)
028370           ELSE
028380                 MOVE "YES" TO  脱出フラグ
028390           END-IF
028400           IF    (自衛官番号Ｗ(5:1) = "0")  AND (脱出フラグ  = SPACE )
028410                 MOVE SPACE TO  自衛官番号Ｗ(5:1)
028420           ELSE
028430                 MOVE "YES" TO  脱出フラグ
028440           END-IF
028450           IF    (自衛官番号Ｗ(6:1) = "0")  AND (脱出フラグ  = SPACE )
028460                 MOVE SPACE TO  自衛官番号Ｗ(6:1)
028470           ELSE
028480                 MOVE "YES" TO  脱出フラグ
028490           END-IF
028500         MOVE  自衛官番号集団Ｗ     TO 共済番号Ｗ
028510     END-IF.
025310*
009605*================================================================*
009606******************************************************************
009607 END PROGRAM YHN438.
009608******************************************************************
