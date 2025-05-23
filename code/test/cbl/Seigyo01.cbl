000010*****************************************************************
000020* ＳＥＩＧＹＯ01：制御情報マスタ ［ＲＬ＝２５６BYTE］           *
000030*****************************************************************
000040* SYSYTEM NAME:柔 FOR WINNDOWS                                  *
000050* LANGUAGE PG :COBOL97 FOR WINDOWS                              *
000060* PRODUCT DATE:2002.05.08                                       *
000070* AUTHOR      :佐野　誠                                         *
000080* PURPOSE     :制御区分は、０１                                 *
000090*              拡張情報（各種初期値）を管理                     *
000100*                                                               *
000110* プリフィックスを制０１−にする                                *
000120*****************************************************************
000130 01 レコード.
000140   03 レコードキー.
000150     05 制御区分                     PIC 9(2).
000160   03 レコードデータ.
000170* メモＦの添付ファイル格納フォルダ（初期値）
000180     05 添付ファイルフォルダ         PIC X(80).
000190* メモパスワード
000200     05 メモパスワード               PIC X(6).
000210* 「柔」終了時の起動プログラムパス
000220     05 終了プログラムパス           PIC X(50).
000230*
000240*  ６・７号請求書（印刷区分 0:印刷 1:印刷しない、振込先 0:自分 1:会 9:印刷しない）
000250*  更新フラグ１の時データが存在する。
000260     05 請求書関連. 
000270        07 請求書更新フラグ          PIC 9.
000280        07 国保６号印刷区分          PIC 9.
000290        07 国保７号印刷区分          PIC 9.
000300        07 国保当社印刷区分          PIC 9.
000310        07 国保振込先区分            PIC 9.
000320        07 社保６号印刷区分          PIC 9.
000330        07 社保７号印刷区分          PIC 9.
000340        07 社保当社印刷区分          PIC 9.
000350        07 社保振込先区分            PIC 9.
000360        07 組合６号印刷区分          PIC 9.
000370        07 組合７号印刷区分          PIC 9.
000380        07 組合当社印刷区分          PIC 9.
000390        07 組合振込先区分            PIC 9.
000400        07 共済６号印刷区分          PIC 9.
000410        07 共済７号印刷区分          PIC 9.
000420        07 共済当社印刷区分          PIC 9.
000430        07 共済振込先区分            PIC 9.
000440        07 老人６号印刷区分          PIC 9.
000450        07 老人７号印刷区分          PIC 9.
000460        07 老人当社印刷区分          PIC 9.
000470        07 老人振込先区分            PIC 9.
000480        07 助成６号印刷区分          PIC 9.
000490        07 助成７号印刷区分          PIC 9.
000500        07 助成当社印刷区分          PIC 9.
000510        07 助成振込先区分            PIC 9.
000520* 神奈川　国保老人請求書（ワクあり０、ワクなし１、新用紙２）
000530     05 神奈川請求書ワク区分         PIC 9.
000540* 日整静岡カルテ用↓
000550* 日整静岡カルテ通常 欄外に保険種別を印刷（しない０、する１）
000560     05 保険種別印刷                 PIC 9.
000570* 日整静岡カルテ 新用紙 裏 印刷項目 (印刷しない：０、印刷する：１）
000580     05 氏名印刷                     PIC 9.
000590     05 月日印刷                     PIC 9.
000600     05 明細印刷                     PIC 9.
000610     05 合計金額印刷                 PIC 9.
000620     05 負担額印刷                   PIC 9.
000630     05 コメント印刷                 PIC 9.
000640     05 印刷日付印刷                 PIC 9.
000650     05 月合計印刷                   PIC 9.
000660* 印刷日付の設定 (印刷日０、通院日１)
000670     05 印刷日区分                   PIC 9.
000680     05 経過所見印刷                 PIC 9.
000690* 日整静岡カルテ用↑
000700* 長期理由入力画面切替(旧画面０、新画面１)
000710     05 長期理由入力表示             PIC 9.
000720* 負傷入力画面での負傷日チェック(なし０、あり１)
000730     05 負傷日チェック区分           PIC 9.
000740* 自賠責証明書明細書料初期値(請求する０、請求しない１、変更しない２)
000750     05 証明書料初期値               PIC 9.
000760     05 明細書料初期値               PIC 9.
000770* 日整静岡カルテ用(印刷しない：０、印刷する：１）
000780     05 長期理由印刷                 PIC 9.
000790* メモ入力の検索後の処理の振り分け(上書き：０、追加：１)
000800     05 検索後処理区分               PIC 9.
000810* 受付画面の表示項目の選択(０：今まで通り、１：部位数表示を表示)
000820     05 受付表示画面区分             PIC 9.
000830* ２号誌のメモ印刷の振り分け(０：印刷しない １：印刷する)
000840     05 ２号誌メモ印刷区分           PIC 9.
000850* 総括集計表の画面(０：通常、１：色付)
000860     05 総括集計表画面               PIC 9.
000870* 初検に出来ない通院日の間隔(００：指定なし、０１：１ヶ月、０２〜９９：０２日〜９９日)
000880     05 初検不可通院間隔             PIC 9(2).
000890* レセプトの温罨法料の訂正をするかしないかの設定。（０：訂正する １：訂正なし）
000900     05 健保レセ温罨訂正             PIC 9(1).
000910     05 老人レセ温罨訂正             PIC 9(1).
000920     05 助成レセ温罨訂正             PIC 9(1).
000930* 神奈川　社保老人請求書（ワクあり０、ワクなし１）
000940     05 神奈川請求書ワク区分老健     PIC 9.
000950* 中央総括表（０：印字調整なし、１：印字調整あり）
000960     05 総括表区分                   PIC 9.
000970*指導管理料初期値(０：あり、１：なし)
000980     05 指導管理区分                 PIC 9.
000990*包帯交換料初期値(０：あり、１：なし)
001000     05 包帯交換区分                 PIC 9.
001010*個人用６号７号並び順(０：ラベル順、１：旧並び順)
001020     05 ６７並び順区分               PIC 9.
001030*初検時相談料初期値(０：あり、１：なし)
001040     05 初検時相談料区分             PIC 9.
001050*レセプトの初検時相談料タイトル(０：印刷する、１：しない)
001060     05 初検時相談料タイトル         PIC 9.
001070*カルテ裏用紙種別
001080     05 カルテ裏用紙種別             PIC 9.
001090* 日整静岡カルテ用(印刷しない：０、印刷する：１）
001100     05 相談料タイトル               PIC 9.
001110* 表示画面の最適化(しない：０、する：１)
001120     05 表示画面最適化               PIC 9.
001130* 保険者ラベル封筒印刷時の向き
001140     05 ラベル封筒挿入向き           PIC 9.
001150* 長期継続者一覧の対象(０：通常、１：大阪府用(社団大阪のみ))
001160     05 長期理由対象                 PIC 9.
001171* 暗号化 0:なし、1:あり
001172     05 特殊操作区分                 PIC 9.
      * 社団用自賠責用紙種別(０：通常、１：社団用)(社団のみ)
           05 自賠責用紙種別               PIC 9.
      * 子画面区分(0:通常 1:領収証)
           05 子画面区分                   PIC 9.
      * レシートプリンタ使用区分(０：使用しない、１：使用する)
           05 レシートプリンタ使用区分     PIC 9.
      * 日接用 レセ一覧画面の用紙の初期値
           05 一覧用紙初期値               PIC 9.
      * 委任者情報マスタ使用区分(０：使用しない、１：使用する)
           05 委任者情報区分               PIC 9.
      *印字調整５ミリバージョン
002210     05 老人レセ２左右               PIC 9.
002220     05 老人レセ２左右調整数         PIC 9.
002230     05 老人レセ２上下               PIC 9.
002240     05 老人レセ２上下調整数         PIC 9.
002250*
002260     05 助成レセ２左右               PIC 9.
002270     05 助成レセ２左右調整数         PIC 9.
002280     05 助成レセ２上下               PIC 9.
002290     05 助成レセ２上下調整数         PIC 9.
001173*/バーコード印刷の位置
           05 バーコード位置区分           PIC 9.
      */レシート領収証に代表者氏名を(０：印刷しない、１：印刷する)
           05 レシート代表者名             PIC 9.
      */バーコード確認印刷区分(０：確認しない、１：確認する)
           05 バーコード印刷確認区分       PIC 9.
000930* 神奈川　社保助成請求書（白紙(ワクごと)：０、専用の用紙：１）
000940     05 神奈川社保助成請求書用紙種別     PIC 9.
      */施術所情報マスタのロック(０：ロックする、１：ロックしない)
           05 施術所情報ロック区分         PIC 9.
001106* 頻回施術理由必要期間
001107     05 期間月                       PIC 9(2).
001108     05 回数                         PIC 9(2).
      * レセプト画面の郵便番号電話番号のボタン状態を保存する(０：設定に従う、１：印刷しない)
           05 レセ画面郵便電話区分         PIC 9(1).
001109*
000530*/ 請求統計表 0:通常（2枚組） 1:本体まとめ（1枚）
000540     05 請求統計まとめ区分           PIC 9.
001109*
000530*/ 自賠責レセ　詳細条件保存
      *    0:共済 1:保険
000540     05 タイトル選択                 PIC 9.
      *    0:腰部頚部 1:腰椎頚椎
000540     05 負傷名の表現                 PIC 9.
      *    0:最終通院日 1:月末日
000540     05 請求日区分                   PIC 9.
      *    0:なし 1:請求中 2:受領済
000540     05 請求受領区分                 PIC 9.
      *    0:印刷しない 1:印刷する
000540     05 負傷の経過                   PIC 9.
001109*
000530*/ 山形　身乳母総括表　用紙選択保存
      *    0:旧用紙 1:新用紙
000540     05 用紙種別                     PIC 9.
      *
      *特別材料初期値(０：あり、１：なし)
           05 特別材料区分                 PIC 9.
      *
      *頻回の日数の解釈(０：長期になった日より、１：長期になった月の初めから)
           05 頻回開始区分                 PIC 9.
      *
      *広島県固有の被爆申請書の用紙種別
           05 被爆用紙種別                 PIC 9.
      *
      *中央カルテの用紙の版(用紙１:通常用紙 用紙２:再生紙 用紙３:労災)
           05 用紙１                       PIC 9.
           05 用紙２                       PIC 9.
           05 用紙３                       PIC 9.
      */令和4年10月1日より明細書発行加算料を新設/20220704
           05 明細発行加算区分             PIC 9(1).
      */算定初期値/202306
           05 冷罨法区分                   PIC 9(1).
           05 温罨法区分                   PIC 9(1).
           05 電療料区分                   PIC 9(1).
      */負傷変更繰越時の初期値（０：いいえ、１：はい）/202310
           05 負傷変更繰越区分             PIC 9(1).
      */明細発行加算の変更和暦年月/20240724
000420     05 明細変更和暦年月.
000430       07 明細変更和暦               PIC 9.
000440       07 明細変更年月.
000450         09 明細変更年               PIC 9(2).
000460         09 明細変更月               PIC 9(2).
001180     05 FILLER                       PIC X(8).
