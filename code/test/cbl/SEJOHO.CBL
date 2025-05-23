000010*****************************************************************
000020* ＳＥＪＯＨＯ：施術所情報マスタ［ＲＬ＝1024］                  *
000030*****************************************************************
000040* SYSYTEM NAME:柔 FOR WINNDOWS                                  *
000060* LANGUAGE PG :NetCOBOL                                         *
000061* PRODUCT DATE:2009.04.01                                       *
000080* PURPOSE     :施術所情報を管理                                 *
000081*---------------------------------------------------------------*
000082*	SELECT文変更なし、項目変更あり
000083*---------------------------------------------------------------*
000090*****************************************************************
000100 01  レコード.
000110     03  レコードキー.
000120*****************************************************************
000140         05  施術所番号                PIC 9(2).
000150*****************************************************************
000160     03  レコードデータ.
000170         05  代表者名                  PIC X(50).
000180         05  代表者カナ                PIC X(50).
000190         05  接骨院名                  PIC X(50).
000200         05  接骨院名カナ              PIC X(50).
000210         05  郵便番号.
000220           07  郵便番号１              PIC X(3).
000230           07  郵便番号２              PIC X(4).
000240         05  住所.
000250           07  住所１                  PIC X(50).
000260           07  住所２                  PIC X(50).
000270         05  電話番号                  PIC X(30).
000280         05  取引先情報.
000290           07  取引先銀行名            PIC X(40).
000300           07  取引先銀行支店名        PIC X(40).
000310           07  預金種別                PIC 9.
000320           07  銀行番号                PIC X(4).
000330           07  店番号                  PIC X(3).
000340           07  口座番号                PIC X(10).
000350           07  口座名義人              PIC X(80).
000360           07  口座名義人カナ          PIC X(80).
000361**
000362*KAIJOHOで管理
000370*         05  接骨師会情報.
000380*           07  接骨師会名              PIC X(50).
000390*           07  接骨師会会長名          PIC X(30).
000400*           07  会郵便番号.
000410*             09  会郵便番号１          PIC X(3).
000420*             09  会郵便番号２          PIC X(4).
000430*           07  会住所.
000440*             09  会住所１              PIC X(50).
000450*           07  会電話番号              PIC X(15).
000520**
000540         05  接骨師情報.
000550           07  接骨師会会員番号        PIC X(10).
000560           07  柔整師番号              PIC X(16).
000570           07  共済連番号              PIC 9(6).
000580           07  地共済連番号            PIC 9(6).
000590           07  自衛官番号              PIC 9(6).
000600           07  国保番号                PIC X(10).
000610           07  労災番号                PIC X(16).
000611           07  新柔整師番号            PIC X(20).
000612           07  柔整師有効年月日.
000613             09  柔整師有効和暦        PIC 9.
000614             09  柔整師有効年          PIC 9(2).
000615             09  柔整師有効月          PIC 9(2).
000616             09  柔整師有効日          PIC 9(2).
000620         05  都道府県ＪＩＳ            PIC X(2).
000630         05  都道府県政管              PIC X(2).
000640         05  施術所ＩＤ                PIC X(6).
000650         05  使用者区分                PIC X.
000660         05  家族登録                  PIC X.
000670         05  定額制区分                PIC 9.
000680         05  隣接都道府県ＪＩＳ        PIC X(2).
000710         05  正式版ＩＤ                PIC X.
000720         05  請求台帳地区              PIC 9(2).
000730         05  防衛省区分                PIC 9.
000750*
000760*       / やめる /
000764*         05  会情報使用区分            PIC 9.
000765*         05  会情報.
000766*           07  会名                    PIC X(50).
000767*           07  会長名                  PIC X(40).
000768*           07  会郵便番号.
000769*             09  会郵便番号１          PIC X(3).
000770*             09  会郵便番号２          PIC X(4).
000771*           07  会住所                  PIC X(80).
000772*           07  会電話番号              PIC X(20).
000773*
000766         05  領収書代表者名            PIC X(50).
000766         05  開設者名                  PIC X(30).
000880         05  施術所緯度                PIC 9(3)V9(7).
000890         05  施術所経度                PIC 9(3)V9(7).
001109*
000530*/ 0:使用なし 1:使用あり
000540         05  複数施術者使用区分        PIC 9.
001109*
000530*/ 全柔で使用 /0201
000540         05  施術所コード              PIC X(5).
001109*
000530*/ インボイス対応 /2310
000540         05  インボイス登録番号        PIC X(20).
000540         05  インボイス事業者名        PIC X(50).
      *
000781         05  FILLER                    PIC X(135).
