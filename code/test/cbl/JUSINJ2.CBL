000010*****************************************************************
000020* 受診者情報２Ｆ　ＪＵＳＩＮＪ２．ＤＡＴ ［ＲＬ＝1024BYTE］     *
000030*****************************************************************
000040* SYSYTEM NAME:共通                                             *
000050* LANGUAGE PG :NetCOBOL                                         *
000060* PRODUCT DATE:2009.03.23                                       *
000070* PURPOSE     :月毎＋患者毎の保険情報を記録                     *
000080*              主として保険証情報・保険詳細情報（甲乙区分・請求 *
000090*              区分）等を管理する。                             *
000100*              又、患者毎に最新レコード（キー：施術年月ALL ZERO *
000110*              + 患者コード）を管理し、常に、最新の月情報での   *
000120*              月次更新が可能。                                 *
000130*****************************************************************
000140*---------------------------------------------------------------*
000150*	新規ファイル JUSINJより新規キー＋あまり使わない項目を分離
000160*---------------------------------------------------------------*
000170**
000180*     SELECT  受診者情報２Ｆ  ASSIGN      TO        JUSINJ2L
000190*                             ORGANIZATION             IS INDEXED
000200*                             ACCESS MODE              IS DYNAMIC
000210*                             RECORD KEY               IS 受２−施術和暦年月
000220*                                                         受２−患者コード
000230*                             ALTERNATE RECORD KEY     IS 受２−請求対象区分
000240*                                                         受２−請求和暦年月
000250*                                                         受２−施術和暦年月
000260*                                                         受２−患者コード
000270*                             ALTERNATE RECORD KEY     IS 受２−助成請求対象区分
000280*                                                         受２−助成請求和暦年月
000290*                                                         受２−施術和暦年月
000300*                                                         受２−患者コード
000310*                             FILE STATUS              IS  状態キー
000320*                             LOCK        MODE         IS  AUTOMATIC.
000330***
000340* FD  受診者情報２Ｆ        BLOCK   CONTAINS   1   RECORDS.
000350*     COPY JUSINJ2          OF  XFDLIB  JOINING   受２   AS  PREFIX.
000360*
000370*****************************************************************
000380 01 レコード.
000390   03 レコードキー.
000400     05 施術和暦年月.
000410       07 施術和暦                     PIC 9.
000420       07 施術年月.
000430         09 施術年                     PIC 9(2).
000440         09 施術月                     PIC 9(2).
000450     05 患者コード.
000460       07 患者番号                     PIC 9(6).
000470       07 枝番                         PIC X.
000480   03 レコードデータ.
000490**
000500*--------------------------------------------------------*
000510* 柔整でのみ使用する予定だったが、やめる→receptへ移動/
000520* 初期値代入（年月は施術年月の値）
000530     05 柔整請求年月情報.
000540*      0：請求　1：月遅れ請求 2:返戻、9：請求しない
000550        07 請求区分                    PIC 9.
000560        07 請求和暦年月.
000570           09 請求和暦                 PIC 9.
000580           09 請求年月.
000590             11 請求年                 PIC 9(2).
000600             11 請求月                 PIC 9(2).
000610*       0：なし　1：対象
000620        07 請求対象区分                PIC 9.
000630*
000640     05 柔整助成請求年月情報.
000650*      0：請求　1：月遅れ請求 2:返戻、9：請求しない
000660        07 助成請求区分                PIC 9.
000670        07 助成請求和暦年月.
000680           09 助成請求和暦             PIC 9.
000690           09 助成請求年月.
000700             11 助成請求年             PIC 9(2).
000710             11 助成請求月             PIC 9(2).
000720*       0：なし　1：対象
000730        07 助成請求対象区分            PIC 9.
000740**
000750*    0：通常、1：償還払い
000760     05 柔整本体償還払い区分               PIC 9.
000770*    0：通常、1：本体まとめ
000780     05 柔整本体まとめ区分                 PIC 9.
000790*    0：通常、1：償還払い
000800     05 柔整助成償還払い区分               PIC 9.
000810*    0：対象、1：対象外
000820     05 柔整助成レセ印刷対象区分           PIC 9.
000830*    0：対象、1：対象外
000840     05 柔整助成レセ会総括表印刷対象区分   PIC 9.
000850*    0：なし、1：まとめる
000860     05 柔整本体枝番まとめ区分             PIC 9.
000870     05 FILLER                             PIC X(20).
000880*--------------------------------------------------------*
000890**
000900     05 患者携帯電話番号               PIC X(30).
000910     05 患者メールアドレス             PIC X(60).
000920     05 患者緯度                       PIC 9(3)V9(7).
000930     05 患者経度                       PIC 9(3)V9(7).
000940     05 来院動機区分                   PIC 9(3).
000950     05 紹介者患者番号                 PIC 9(6).
000960**
000970     05 事業所名称                     PIC X(60).
000980     05 事業所郵便番号.
000990       07 事業所郵便番号１             PIC X(3).
001000       07 事業所郵便番号２             PIC X(4).
001010     05 事業所住所１                   PIC X(50).
001020     05 事業所住所２                   PIC X(50).
001030*
001040     05 備考                           PIC X(400).
001050*
001060     05 助成被保険者氏名               PIC X(50).
001070     05 助成被保険者住所１             PIC X(40).
001080     05 助成被保険者住所２             PIC X(40).
001090*
001100     05 助成本人家族区分               PIC 9.
001110     05 助成続柄                       PIC 9(2).
001120*
001130* カルテ裏の印刷行数(鍼灸用)
001140     05 はりカルテ裏印刷行数           PIC 9(2).
001150     05 マッサージカルテ裏印刷行数     PIC 9(2).
001160*
001171     05 ＤＭ不要                       PIC 9(1).
001180*
001181*    快癒 往療先住所（20180110 往療内訳書(高知後高用)）
001182     05 往療内訳書用住所.
001183        07 往療内訳書用住所１          PIC X(50).
001184        07 往療内訳書用住所２          PIC X(50).
001185*
001186     05 FILLER                         PIC X(48).
001190*
