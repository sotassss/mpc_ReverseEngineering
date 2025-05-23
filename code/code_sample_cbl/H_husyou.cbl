000010*****************************************************************
000020* Ｈ負傷データＦ　H_husyou.dat ［RL=256BYTE］                   *
000030*****************************************************************
000101* SYSYTEM NAME:鍼灸マッサージ                                   *
000102* LANGUAGE PG :NetCOBOL                                         *
000103* PRODUCT DATE:2009.03.26                                       *
000105* PURPOSE     :傷病情報を記録                                   *
000106*****************************************************************
000110*
000111*---------------------------------------------------------------*
000112* ※ SELECT文変更あり（キー変更）、項目変更あり
000113*---------------------------------------------------------------*
000115*
000116*     SELECT  Ｈ負傷データＦ  ASSIGN      TO        HHUSYOUL
000117*                             ORGANIZATION             IS  INDEXED
000118*                             ACCESS MODE              IS  DYNAMIC
000119*                             RECORD KEY               IS  Ｈ負−主キー
000120*                             ALTERNATE RECORD KEY     IS  Ｈ負−施術区分
000121*                                                          Ｈ負−患者コード
000125*                                                          Ｈ負−主キー
000126*                             FILE STATUS              IS  状態キー
000127*                             LOCK        MODE         IS  AUTOMATIC.
000128**
000129* FD  Ｈ負傷データＦ      BLOCK   CONTAINS   1   RECORDS.
000130*     COPY H_HUSYOU   OF  XFDLIB  JOINING   Ｈ負 AS  PREFIX.
000131*
000133*---------------------------------------------------------------*
000134***以下はH_RECEへ移動
000135*     05 傷病コード                     PIC 9(2).
000136*     05 傷病名                         PIC X(100).
000137*     05 症状                           PIC X(100).
000138*     05 負傷原因                       PIC X(100).
000139*     05 同意医師コード                 PIC 9(3).
000140*     05 同意和暦年月日.
000141*       07 同意和暦年月.
000142*         09 同意和暦                   PIC 9.
000143*         09 同意年月.
000144*           11 同意年                   PIC 9(2).
000145*           11 同意月                   PIC 9(2).
000146*       07 同意日                       PIC 9(2).
000147*     05 加療期間                       PIC 9(3).
000148*
000149*     05 施術内容区分                   PIC 9(2).
000150*
000151*     05 マッサージ固有.
000152*       07 マッサージ局所数             PIC 9.
000153*       07 変形徒手矯正術数             PIC 9.
000154*       07 マッサージ体幹               PIC 9.
000155*       07 マッサージ右上肢             PIC 9.
000156*       07 マッサージ左上肢             PIC 9.
000157*       07 マッサージ右下肢             PIC 9.
000158*       07 マッサージ左下肢             PIC 9.
000159*       07 変形徒手矯正術右上肢         PIC 9.
000160*       07 変形徒手矯正術左上肢         PIC 9.
000161*       07 変形徒手矯正術右下肢         PIC 9.
000162*       07 変形徒手矯正術左下肢         PIC 9.
000163*
000164*
000165*****************************************************************
000166 01 レコード.
000167   03 レコードキー.
000168     05 主キー                         PIC 9(8).
000169   03 レコードデータ.
000170*-----------------------------------------------*
000171* 1:鍼灸、2:あんま・マッサージ、3:両方（労災）
000172     05 施術区分                       PIC 9.
000173*-----------------------------------------------*
000174*   / 枝番は、SPACEのみ /
000175     05 患者コード.
000176       07 患者番号                     PIC 9(6).
000177       07 枝番                         PIC X.
000178**
000179     05 発病和暦年月日.
000180       07 発病和暦年月.
000181         09 発病和暦                   PIC 9.
000182         09 発病年月.
000183           11 発病年                   PIC 9(2).
000184           11 発病月                   PIC 9(2).
000185       07 発病日                       PIC 9(2).
000186*
000187*●追加-----------------------------------------*
000188     05 発病日自由                     PIC X(30).
000189*-----------------------------------------------*
000190*
000191     05 初診和暦年月日.
000192       07 初診和暦年月.
000193         09 初診和暦                   PIC 9.
000194         09 初診年月.
000195           11 初診年                   PIC 9(2).
000196           11 初診月                   PIC 9(2).
000197       07 初診日                       PIC 9(2).
000198     05 初療和暦年月日.
000199       07 初療和暦年月.
000200         09 初療和暦                   PIC 9.
000201         09 初療年月.
000202           11 初療年                   PIC 9(2).
000203           11 初療月                   PIC 9(2).
000204       07 初療日                       PIC 9(2).
000205*
000206*-----------------------------------------------*
000207* 1：治癒　2：全治　3：中止  4：転医　5．自然治癒  9：継続
000208     05 転帰区分                       PIC 9.
000209*-----------------------------------------------*
000210     05 終了和暦年月日.
000211       07 終了和暦年月.
000212         09 終了和暦                   PIC 9.
000213         09 終了年月.
000214           11 終了年                   PIC 9(2).
000215           11 終了月                   PIC 9(2).
000216       07 終了日                       PIC 9(2).
000217*
001211     05 FILLER                         PIC X(181).
001220*
