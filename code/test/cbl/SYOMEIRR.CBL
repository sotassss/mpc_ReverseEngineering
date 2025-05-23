000010*****************************************************************
000020* ＳＹＯＭＥＩＲＲ：証明書履歴ファイル ［ＲＬ＝3840BYTE］       *
000030*****************************************************************
000040* SYSYTEM NAME:柔 FOR WINNDOWS                                  *
000050* LANGUAGE PG :COBOL97 FOR WINDOWS                              *
000060* PRODUCT DATE:2012.02.22                                       *
000070* AUTHOR      :岡田  憲和                                       *
000080* PURPOSE     :施術証明書や通院証明書の印刷履歴を保存           *
000090*                                                               *
000100*   用紙区分　帳票名                                            *
000110*      1      施術証明書                                        *
000120*      2      通院証明書                                        *
000130*      3      ご紹介書                                          *
000140*      4      お礼状                                            *
000150*      5      施術情報提供紹介書                                *
000170*    901      施術確認書 (ひな形:施術年月・患者コード＝初期値)  *
000171*    902      施術計画書 (ひな形:施術年月・患者コード＝初期値)  *
000172*    991      フリー案内 (ひな形:施術年月・患者コード＝初期値)  *
000173*    999      処理グループ管理用 (患者番号を６桁処理連番とする) *
000174*                                                               *
000175*****************************************************************
000180*---------------------------------------------------------------*
000190*	SELECT文
000200*---------------------------------------------------------------*
000210**
000220*     SELECT  証明書履歴Ｆ    ASSIGN      TO        SYOMEIRL
000230*                             ORGANIZATION             IS  INDEXED
000240*                             ACCESS                   IS  DYNAMIC
000250*                             RECORD      KEY          IS  証履−施術区分
000260*                                                          証履−用紙区分
000270*                                                          証履−施術和暦年月
000280*                                                          証履−患者コード
000290*                                                          証履−連番
000300*                             FILE        STATUS       IS  状態キー
000310*                             LOCK        MODE         IS  AUTOMATIC.
000320***
000330*                           ［ＲＬ＝  3840］
000340* FD  証明書履歴Ｆ      BLOCK   CONTAINS   1   RECORDS GLOBAL.
000350*     COPY SYOMEIRR         OF  XFDLIB  JOINING   証履     AS  PREFIX.
000360*
000370*****************************************************************
000380 01 レコード.
000390   03 レコードキー.
000400     05 施術区分                     PIC 9(1).
000410     05 用紙区分                     PIC 9(3).
000420     05 施術和暦年月.
000430       07 施術和暦                   PIC 9.
000440       07 施術年月.
000450         09 施術年                   PIC 9(2).
000460         09 施術月                   PIC 9(2).
000470     05 患者コード.
000480       07 患者番号                   PIC 9(6).
000490       07 枝番                       PIC X.
000500     05 連番                         PIC 9(2).
000510   03 レコードデータ.
000520     05 タイトル                     PIC X(20).
000530     05 負傷全体.
000540       07 負傷部位 OCCURS  5.
000550         09 負傷名                   PIC X(36).
000560         09 症状コード               PIC 9(2) OCCURS  5.
000570*    0:印刷しない 1:印刷する
000580     05 負傷原因印刷区分             PIC 9(1).
000590     05 負傷原因全体.
000600       07 負傷原因 OCCURS  3         PIC X(72).
000610     05 摘要.
000620       07 加療数                     PIC 9(3).
000630       07 加療単位                   PIC X(22).
000640       07 休業数                     PIC 9(3).
000650       07 休業単位                   PIC X(10).
000660       07 休業文章                   PIC X(18).
000670     05 作成和暦年月日.
000680       07 作成和暦年月.
000690         09 作成和暦                 PIC 9.
000700         09 作成年月.
000710           11 作成年                 PIC 9(2).
000720           11 作成月                 PIC 9(2).
000730       07 作成日                     PIC 9(2).
000740     05 医師.
000750       07 医療機関名                 PIC X(50).
000760       07 医師名                     PIC X(50).
000770       07 医師敬称                   PIC X(10).
000850* コメント欄は各用紙ごとに再定義して使用します。
000860*    用紙区分 1-4
000870     05 コメント                    PIC X(2500).
000880*    用紙区分 5
000890     05 施術情報提供紹介書 REDEFINES コメント.
000900       07 職業                      PIC X(10).
000910       07 紹介目的                  PIC X(250).
000920       07 応急施術の内容            PIC X(250).
000930       07 症状                      PIC X(250).
000940       07 備考                      PIC X(250).
000950       07 FILLER                    PIC X(1490).
000960*
000961*    はがき裏印刷・メール送信用再定義
000964*
000965*    ひな形用
000967*      用紙区分＝用紙種類（901:施術確認書/902:施術計画書/991:フリー案内）
000968*      施術和暦年月日・患者コード＝初期値
000969*      連番＝パターン番号
000970*
000971*      用紙サイズ区分　0:はがき　1:A4
000972*
000973     05 はがき裏ひな形 REDEFINES コメント.
000974       07 ひな形説明                PIC X(100).
000975       07 ひな形文                  PIC X(2000).
000976       07 用紙サイズ区分            PIC 9(1).
000977       07 FILLER                    PIC X(399).
000978*    実績用 
000980*      施術和暦年月日・患者コード＝実績値  ※ヘッダは患者コード＝初期値
000981*      連番＝実績値
000982     05 はがき裏実績ヘッダ REDEFINES コメント.
000983       07 実績説明                  PIC X(100).
000984       07 実績出力先                PIC 9(1).
000985       07 実績用紙種類              PIC 9(3).
000986       07 実績使用例文              PIC 9(3).
000987       07 実績案内文                PIC X(2000).
000988       07 実績用紙サイズ            PIC 9(1).
000989       07 FILLER                    PIC X(392).
000990     05 はがき裏実績 REDEFINES コメント.
000991       07 実績編集文                PIC X(2000).
000992       07 処理連番                  PIC 9(6).
000993       07 FILLER                    PIC X(494).
000994*
000995*    実印刷を行ったマシン日付（F12登録では更新しない）
000996     05 印刷和暦年月日.
000997       07 印刷和暦年月.
000998         09 印刷和暦                 PIC 9.
001000         09 印刷年月.
001010           11 印刷年                 PIC 9(2).
001020           11 印刷月                 PIC 9(2).
001030       07 印刷日                     PIC 9(2).
000994*
      *   負傷年月日を追加/210208
000996     05 負傷和暦年月日.
000997       07 負傷和暦年月.
000998         09 負傷和暦                 PIC 9.
001000         09 負傷年月.
001010           11 負傷年                 PIC 9(2).
001020           11 負傷月                 PIC 9(2).
001030       07 負傷日                     PIC 9(2).
001040*
001050     05 FILLER                       PIC X(466).
