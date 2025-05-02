000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             CHGJUNO.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*         受診者No.変更・削除
000100*         MED = 
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2009-09-03
000130 DATE-COMPILED.          2009-09-03
000140*----------------------------------------------------------------*
000150* 2012/06/26 岡田 憲和
000151* 往療実績の往療場所患者コード＆先順患者コードも更新に含める（全件リード＆該当リライト）
000152* 2018/10/26 岡田 憲和 Ｈ報告書ファイルを追加
000154*----------------------------------------------------------------*
000155******************************************************************
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
000260     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY               IS  受－施術和暦年月
000300                                                          受－患者コード
000310                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000320                                                          受－患者カナ
000330                                                          受－患者コード
000340                             ALTERNATE RECORD KEY     IS  受－患者コード
000350                                                          受－施術和暦年月
000360                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000370                                                          受－保険種別
000380                                                          受－保険者番号
000390                                                          受－患者コード
000400                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000410                                                          受－公費種別
000420                                                          受－費用負担者番号
000430                                                          受－患者コード
000440                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000450                                                          受－助成種別
000460                                                          受－費用負担者番号助成
000470                                                          受－患者コード
000480                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
000490                                                          受－施術和暦年月
000500                                                          受－患者コード
000510                             FILE STATUS              IS  状態キー
000520                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS MODE              IS  DYNAMIC
000560                             RECORD KEY               IS  負原－区分コード
000570                                                          負原－負傷原因コード
000580                             FILE STATUS              IS  状態キー
000590                             LOCK        MODE         IS  AUTOMATIC.
000600     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
000610                             ORGANIZATION             IS  INDEXED
000620                             ACCESS MODE              IS  DYNAMIC
000630                             RECORD KEY               IS  負－施術和暦年月
000640                                                          負－患者コード
000650                             ALTERNATE RECORD KEY     IS  負－患者コード
000660                                                          負－施術和暦年月
000670                             FILE STATUS              IS  状態キー
000680                             LOCK        MODE         IS  AUTOMATIC.
000690     SELECT  受付データＦ    ASSIGN      TO        UKETUKEL
000700                             ORGANIZATION             IS  INDEXED
000710                             ACCESS MODE              IS  DYNAMIC
000720                             RECORD KEY               IS  受付－施術和暦年月日
000730                                                          受付－患者コード
000740                             ALTERNATE RECORD KEY     IS  受付－施術和暦年月日
000750                                                          受付－受付時間
000760                                                          受付－患者コード
000770                             ALTERNATE RECORD KEY     IS  受付－患者コード
000780                                                          受付－施術和暦年月日
000790                                                          受付－受付時間
000800                             ALTERNATE RECORD KEY     IS  受付－施術和暦年月日
000810                                                          受付－連番
000820                             FILE STATUS              IS  状態キー
000830                             LOCK        MODE         IS  AUTOMATIC.
000840     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
000850                             ORGANIZATION             IS  INDEXED
000860                             ACCESS MODE              IS  DYNAMIC
000870                             RECORD KEY               IS  施記－施術和暦年月日
000880                                                          施記－患者コード
000890                             ALTERNATE RECORD KEY     IS  施記－患者コード
000900                                                          施記－施術和暦年月日
000910                             FILE STATUS              IS  状態キー
000920                             LOCK        MODE         IS  AUTOMATIC.
000930     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000940                             ORGANIZATION             IS  INDEXED
000950                             ACCESS MODE              IS  DYNAMIC
000960                             RECORD KEY               IS  制－制御区分
000970                             FILE STATUS              IS  状態キー
000980                             LOCK        MODE         IS  AUTOMATIC.
001102     SELECT  会計データＦ    ASSIGN      TO        KAIKEIL
001103                             ORGANIZATION             IS  INDEXED
001104                             ACCESS MODE              IS  DYNAMIC
001105                             RECORD KEY               IS  会－施術和暦年月日
001106                                                          会－患者コード
001107                             ALTERNATE RECORD KEY     IS  会－患者コード
001108                                                          会－施術和暦年月日
001109                             FILE STATUS              IS  状態キー
001110                             LOCK        MODE         IS  AUTOMATIC.
001111*
001113     SELECT  長期継続者Ｆ    ASSIGN      TO        CHOKEIL
001120                             ORGANIZATION             IS INDEXED
001130                             ACCESS MODE              IS DYNAMIC
001140                             RECORD KEY               IS 長継－施術和暦年月
001150                                                         長継－患者コード
001160                             ALTERNATE RECORD KEY     IS 長継－患者コード
001170                                                         長継－施術和暦年月
001180                             FILE STATUS              IS 状態キー
001190                             LOCK      MODE           IS AUTOMATIC.
001200     SELECT バーコード管理Ｆ ASSIGN      TO        BARKANRL
001210                             ORGANIZATION             IS  INDEXED
001220                             ACCESS MODE              IS  DYNAMIC
001230                             RECORD KEY               IS  バ管－整理番号
001240                             ALTERNATE KEY            IS  バ管－施術和暦年月
001250                                                          バ管－患者コード
001260                                                          バ管－整理番号
001270                             ALTERNATE KEY            IS  バ管－患者コード
001280                                                          バ管－施術和暦年月
001290                                                          バ管－整理番号
001300                             ALTERNATE KEY            IS  バ管－患者カナ
001310                                                          バ管－施術和暦年月
001320                                                          バ管－患者コード
001330                                                          バ管－整理番号
001340                             ALTERNATE KEY            IS  バ管－施術和暦年月
001350                                                          バ管－患者カナ
001360                                                          バ管－整理番号
001370                             ALTERNATE KEY            IS  バ管－レセプト種別
001380                                                          バ管－施術和暦年月
001390                                                          バ管－患者コード
001400                             ALTERNATE KEY            IS  バ管－レセプト種別
001410                                                          バ管－患者コード
001420                                                          バ管－施術和暦年月
001430                             ALTERNATE KEY            IS  バ管－施術和暦年月
001440                                                          バ管－患者コード
001450                                                          バ管－レセプト種別
001460                             ALTERNATE KEY            IS  バ管－患者コード
001470                                                          バ管－施術和暦年月
001480                                                          バ管－レセプト種別
001490                             FILE STATUS              IS  状態キー
001500                             LOCK        MODE         IS  AUTOMATIC.
001791     SELECT  レセプトＦ      ASSIGN      TO        RECEPTL
001792                             ORGANIZATION             IS  INDEXED
001793                             ACCESS MODE              IS  DYNAMIC
001794                             RECORD KEY               IS  レセ－施術和暦年月
001795                                                          レセ－患者コード
001796                                                          レセ－レセ種別
001797                             ALTERNATE RECORD KEY     IS  レセ－患者コード
001798                                                          レセ－施術和暦年月
001799                                                          レセ－レセ種別
001800                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
001801                                                          レセ－施術和暦年月
001802                                                          レセ－患者コード
001803                                                          レセ－レセ種別
001804                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
001805                                                          レセ－レセ種別
001806                                                          レセ－請求保険者番号
001807                                                          レセ－患者コード
001808                                                          レセ－施術和暦年月
001809                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
001810                                                          レセ－請求保険者番号
001811                                                          レセ－患者コード
001812                                                          レセ－レセ種別
001813                                                          レセ－施術和暦年月
001814                             FILE STATUS              IS  状態キー
001815                             LOCK        MODE         IS  AUTOMATIC.
001816     SELECT カルテファイル   ASSIGN      TO        KARUTEL
001817                             ORGANIZATION             IS  INDEXED
001820                             ACCESS                   IS  DYNAMIC
001830                             RECORD      KEY          IS  カ－会コード
001840                                                          カ－区分
001850                                                          カ－施術和暦年月
001860                                                          カ－患者コード
001870                             ALTERNATE RECORD KEY     IS  カ－会コード
001880                                                          カ－区分
001890                                                          カ－患者コード
001900                                                          カ－施術和暦年月
001910                             ALTERNATE RECORD KEY     IS  カ－区分
001920                                                          カ－施術和暦年月
001930                                                          カ－患者コード
001940                                                          カ－会コード
001950                             ALTERNATE RECORD KEY     IS  カ－区分
001960                                                          カ－患者コード
001970                                                          カ－施術和暦年月
001980                                                          カ－会コード
001990                             FILE        STATUS       IS  状態キー
002000                             LOCK        MODE         IS  AUTOMATIC.
002010     SELECT  メモファイル    ASSIGN      TO        MEMOL
002020                             ORGANIZATION             IS  INDEXED
002030                             ACCESS MODE              IS  DYNAMIC
002040                             RECORD KEY               IS  メモ－制御区分
002050                                                          メモ－患者コード
002060                                                          メモ－施術和暦年月日
002070                             ALTERNATE RECORD KEY     IS  メモ－制御区分
002080                                                          メモ－施術和暦年月日
002090                                                          メモ－患者コード
002100                             ALTERNATE RECORD KEY     IS  メモ－患者コード
002110                                                          メモ－施術和暦年月日
002120                                                          メモ－制御区分
002130                             FILE STATUS              IS  状態キー
002140                             LOCK        MODE         IS  AUTOMATIC.
002141     SELECT  摘要ファイル    ASSIGN      TO        TEKIYOL
002142                             ORGANIZATION             IS  INDEXED
002143                             ACCESS MODE              IS  DYNAMIC
002144                             RECORD KEY               IS  摘要－制御区分
002145                                                          摘要－患者コード
002146                                                          摘要－施術和暦年月
002147                             ALTERNATE RECORD KEY     IS  摘要－制御区分
002148                                                          摘要－施術和暦年月
002149                                                          摘要－患者コード
002150                             FILE STATUS              IS  状態キー
002151                             LOCK        MODE         IS  AUTOMATIC.
002152**
002153     SELECT  ＨレセプトＦ    ASSIGN      TO        HRECEL
002160                             ORGANIZATION             IS  INDEXED
002170                             ACCESS MODE              IS  DYNAMIC
002180                             RECORD KEY               IS  Ｈレセ－施術区分
002190                                                          Ｈレセ－施術和暦年月
002200                                                          Ｈレセ－患者コード
002210                                                          Ｈレセ－レセ種別
002220                             ALTERNATE RECORD KEY     IS  Ｈレセ－施術区分
002230                                                          Ｈレセ－患者コード
002240                                                          Ｈレセ－施術和暦年月
002250                                                          Ｈレセ－レセ種別
002260                             ALTERNATE RECORD KEY     IS  Ｈレセ－施術和暦年月
002270                                                          Ｈレセ－患者コード
002280                                                          Ｈレセ－レセ種別
002290                                                          Ｈレセ－施術区分
002300                             ALTERNATE RECORD KEY     IS  Ｈレセ－請求対象区分
002310                                                          Ｈレセ－請求和暦年月
002320                                                          Ｈレセ－施術区分
002330                                                          Ｈレセ－施術和暦年月
002340                                                          Ｈレセ－患者コード
002350                                                          Ｈレセ－レセ種別
002360                             FILE STATUS              IS  状態キー
002370                             LOCK        MODE         IS  AUTOMATIC.
002380     SELECT  Ｈ日計データＦ  ASSIGN      TO        HNIKEIL
002390                             ORGANIZATION             IS  INDEXED
002400                             ACCESS MODE              IS  DYNAMIC
002410                             RECORD KEY               IS  Ｈ日－施術区分
002420                                                          Ｈ日－施術和暦年月日
002430                                                          Ｈ日－患者コード
002440                             ALTERNATE RECORD KEY     IS  Ｈ日－施術区分
002450                                                          Ｈ日－患者コード
002460                                                          Ｈ日－施術和暦年月日
002470                             ALTERNATE RECORD KEY     IS  Ｈ日－施術和暦年月日
002480                                                          Ｈ日－登録順
002490                             FILE STATUS              IS  状態キー
002500                             LOCK        MODE         IS  AUTOMATIC.
002510     SELECT  Ｈ負傷データＦ  ASSIGN      TO        HHUSYOUL
002520                             ORGANIZATION             IS  INDEXED
002530                             ACCESS MODE              IS  DYNAMIC
002540                             RECORD KEY               IS  Ｈ負－主キー
002550                             ALTERNATE RECORD KEY     IS  Ｈ負－施術区分
002560                                                          Ｈ負－患者コード
002570                                                          Ｈ負－主キー
002580                             FILE STATUS              IS  状態キー
002590                             LOCK        MODE         IS  AUTOMATIC.
002600     SELECT  受診者情報２Ｆ  ASSIGN      TO        JUSINJ2L
002610                             ORGANIZATION             IS INDEXED
002620                             ACCESS MODE              IS DYNAMIC
002630                             RECORD KEY               IS 受２－施術和暦年月
002640                                                         受２－患者コード
002650                             ALTERNATE RECORD KEY     IS 受２－請求対象区分
002660                                                         受２－請求和暦年月
002670                                                         受２－施術和暦年月
002680                                                         受２－患者コード
002690                             ALTERNATE RECORD KEY     IS 受２－助成請求対象区分
002700                                                         受２－助成請求和暦年月
002710                                                         受２－施術和暦年月
002720                                                         受２－患者コード
002730                             FILE STATUS              IS  状態キー
002740                             LOCK        MODE         IS  AUTOMATIC.
002750     SELECT  生保情報Ｆ      ASSIGN      TO        SEIHOJL
002760                             ORGANIZATION             IS INDEXED
002770                             ACCESS MODE              IS DYNAMIC
002780                             RECORD KEY               IS 生保－施術和暦年月
002790                                                         生保－患者コード
002800                             ALTERNATE RECORD KEY     IS 生保－患者コード
002810                                                         生保－施術和暦年月
002820                             FILE STATUS              IS 状態キー
002830                             LOCK        MODE         IS AUTOMATIC.
002840     SELECT  自賠責情報Ｆ    ASSIGN      TO        JIBAIJL
002850                             ORGANIZATION             IS INDEXED
002860                             ACCESS MODE              IS DYNAMIC
002870                             RECORD KEY               IS 自賠－施術和暦年月
002880                                                         自賠－患者コード
002890                             ALTERNATE RECORD KEY     IS 自賠－患者コード
002900                                                         自賠－施術和暦年月
002910                             FILE STATUS              IS 状態キー
002920                             LOCK        MODE         IS AUTOMATIC.
002930     SELECT  労災情報Ｆ      ASSIGN      TO        ROUSAIJL
002940                             ORGANIZATION             IS INDEXED
002950                             ACCESS MODE              IS DYNAMIC
002960                             RECORD KEY               IS 労災－施術和暦年月
002970                                                         労災－患者コード
002980                             ALTERNATE RECORD KEY     IS 労災－患者コード
002990                                                         労災－施術和暦年月
003000                             FILE STATUS              IS 状態キー
003010                             LOCK        MODE         IS AUTOMATIC.
003020     SELECT  会計領収Ｆ      ASSIGN      TO        RYOSYUL
003030                             ORGANIZATION             IS  INDEXED
003040                             ACCESS MODE              IS  DYNAMIC
003050                             RECORD KEY               IS  領－施術区分
003060                                                          領－会計領収区分
003070                                                          領－施術和暦年月日
003080                                                          領－患者コード
003090                             ALTERNATE RECORD KEY     IS  領－施術区分
003100                                                          領－患者コード
003110                                                          領－会計領収区分
003120                                                          領－施術和暦年月日
003130                             FILE STATUS              IS  状態キー
003140                             LOCK        MODE         IS  AUTOMATIC.
003141*
003142     SELECT  ＤＭ記録Ｆ          ASSIGN      TO        DMKIROKL
003143                                 ORGANIZATION             IS  INDEXED
003144                                 ACCESS MODE              IS  DYNAMIC
003145                                 RECORD KEY               IS  ＤＭ－発行和暦年月日
003146                                                              ＤＭ－発行枝番
003147                                                              ＤＭ－患者コード
003148                                 ALTERNATE RECORD KEY     IS  ＤＭ－患者コード
003149                                                              ＤＭ－発行和暦年月日
003150                                                              ＤＭ－発行枝番
003151                                 FILE STATUS              IS  状態キー
003152                                 LOCK        MODE         IS  AUTOMATIC.
003153*
003154     SELECT  施設受診者マスタ   ASSIGN      TO       SISETJUL
003155                                ORGANIZATION             IS  INDEXED
003156                                ACCESS MODE              IS  DYNAMIC
003157                                RECORD KEY               IS  施設受－施設コード
003158                                                             施設受－患者コード
003159                                ALTERNATE RECORD KEY     IS  施設受－施設コード
003160                                                             施設受－存在区分
003161                                                             施設受－患者コード
003162                                ALTERNATE RECORD KEY     IS  施設受－患者コード
003163                                                             施設受－施設コード
003164                                FILE STATUS              IS  状態キー
003165                                LOCK        MODE         IS  AUTOMATIC.
003166     SELECT  Ｈ往療実績Ｆ    ASSIGN      TO        HNOURYOL
003167                             ORGANIZATION             IS  INDEXED
003168                             ACCESS MODE              IS  DYNAMIC
003169                             RECORD KEY               IS  Ｈ往実－施術区分
003170                                                          Ｈ往実－施術和暦年月日
003171                                                          Ｈ往実－患者コード
003172                             ALTERNATE RECORD KEY     IS  Ｈ往実－施術区分
003173                                                          Ｈ往実－患者コード
003174                                                          Ｈ往実－施術和暦年月日
003175                             ALTERNATE RECORD KEY     IS  Ｈ往実－施術和暦年月日
003176                                                          Ｈ往実－施術者番号
003177                                                          Ｈ往実－登録順
003178                             ALTERNATE RECORD KEY     IS  Ｈ往実－施術和暦年月日
003179                                                          Ｈ往実－施術開始時間
003180                                                          Ｈ往実－施術者番号
003181                                                          Ｈ往実－登録順
003182                             FILE STATUS              IS  状態キー
003183                             LOCK        MODE         IS  AUTOMATIC.
003184     SELECT  Ｈ往療予定Ｆ    ASSIGN      TO        HNOYOTEL
003185                             ORGANIZATION             IS  INDEXED
003186                             ACCESS MODE              IS  DYNAMIC
003187                             RECORD KEY               IS  Ｈ往予－施術区分
003188                                                          Ｈ往予－施術和暦年月日
003189                                                          Ｈ往予－患者コード
003190                             ALTERNATE RECORD KEY     IS  Ｈ往予－施術和暦年月日
003191                                                          Ｈ往予－患者コード
003192                                                          Ｈ往予－施術区分
003193                             ALTERNATE RECORD KEY     IS  Ｈ往予－患者コード
003194                                                          Ｈ往予－施術和暦年月日
003195                                                          Ｈ往予－施術区分
003196                             ALTERNATE RECORD KEY     IS  Ｈ往予－施術和暦年月日
003197                                                          Ｈ往予－施術者番号
003198                                                          Ｈ往予－施術開始時間
003199                                                          Ｈ往予－登録順
003200                                                          Ｈ往予－患者コード
003201                             ALTERNATE RECORD KEY     IS  Ｈ往予－施術和暦年月日
003202                                                          Ｈ往予－施術開始時間
003203                                                          Ｈ往予－施術者番号
003204                                                          Ｈ往予－登録順
003205                                                          Ｈ往予－患者コード
003206                             FILE STATUS              IS  状態キー
003207                             LOCK        MODE         IS  AUTOMATIC.
003216     SELECT  レセプト労自Ｆ  ASSIGN      TO        RECERJL
003217                             ORGANIZATION             IS  INDEXED
003218                             ACCESS MODE              IS  DYNAMIC
003219                             RECORD KEY               IS  レセ労自－施術和暦年月
003220                                                          レセ労自－患者コード
003221                                                          レセ労自－レセ種別
003225                             FILE STATUS              IS  状態キー
003226                             LOCK        MODE         IS  AUTOMATIC.
003227*
003228     SELECT  証明書履歴Ｆ    ASSIGN      TO        SYOMEIRL
003229                             ORGANIZATION             IS  INDEXED
003230                             ACCESS                   IS  DYNAMIC
003231                             RECORD      KEY          IS  証履－施術区分
003232                                                          証履－用紙区分
003233                                                          証履－施術和暦年月
003234                                                          証履－患者コード
003235                                                          証履－連番
003236                             FILE        STATUS       IS  状態キー
003237                             LOCK        MODE         IS  AUTOMATIC.
003238*
003239*****
003240*****
003241     SELECT  他受診者情報Ｆ    ASSIGN      TO        JUSINJL
003242                             ORGANIZATION             IS  INDEXED
003243                             ACCESS MODE              IS  DYNAMIC
003244                             RECORD KEY               IS  他受－施術和暦年月
003245                                                          他受－患者コード
003246                             ALTERNATE RECORD KEY     IS  他受－施術和暦年月
003247                                                          他受－患者カナ
003248                                                          他受－患者コード
003249                             ALTERNATE RECORD KEY     IS  他受－患者コード
003250                                                          他受－施術和暦年月
003251                             ALTERNATE RECORD KEY     IS  他受－施術和暦年月
003252                                                          他受－保険種別
003253                                                          他受－保険者番号
003254                                                          他受－患者コード
003255                             ALTERNATE RECORD KEY     IS  他受－施術和暦年月
003256                                                          他受－公費種別
003257                                                          他受－費用負担者番号
003258                                                          他受－患者コード
003259                             ALTERNATE RECORD KEY     IS  他受－施術和暦年月
003260                                                          他受－助成種別
003261                                                          他受－費用負担者番号助成
003262                                                          他受－患者コード
003263                             ALTERNATE RECORD KEY     IS  他受－請求和暦年月
003264                                                          他受－施術和暦年月
003265                                                          他受－患者コード
003266                             FILE STATUS              IS  状態キー
003267                             LOCK        MODE         IS  AUTOMATIC.
003268*
003269     SELECT  Ｈ報告書Ｆ      ASSIGN      TO        HHOKOKL
003270                             ORGANIZATION             IS  INDEXED
003271                             ACCESS                   IS  DYNAMIC
003272                             RECORD      KEY          IS  Ｈ報－施術区分
003273                                                          Ｈ報－用紙区分
003274                                                          Ｈ報－施術和暦年月
003275                                                          Ｈ報－患者コード
003276                                                          Ｈ報－連番
003277                             ALTERNATE RECORD KEY     IS  Ｈ報－施術区分
003278                                                          Ｈ報－用紙区分
003279                                                          Ｈ報－患者コード
003280                                                          Ｈ報－施術和暦年月
003281                                                          Ｈ報－施術日
003282                                                          Ｈ報－連番
003283                             FILE        STATUS       IS  状態キー
003284                             LOCK        MODE         IS  AUTOMATIC.
003285*
000140     SELECT  Ｈレセプト詳細Ｆ    ASSIGN      TO       HRECEHKL
000150                             ORGANIZATION             IS  INDEXED
000160                             ACCESS MODE              IS  DYNAMIC
000170                             RECORD KEY               IS  Ｈレセ詳細－施術区分
000180                                                          Ｈレセ詳細－施術和暦年月
000190                                                          Ｈレセ詳細－患者コード
000200                                                          Ｈレセ詳細－レセ種別
000350                             FILE STATUS              IS  状態キー
000360                             LOCK        MODE         IS  AUTOMATIC.
003285*
003286******************************************************************
003287*                      DATA DIVISION                             *
003288******************************************************************
003289 DATA                    DIVISION.
003290 FILE                    SECTION.
003291*                           ［ＲＬ＝  ３２０］
003292 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
003293     COPY JUSINJ     OF  XFDLIB  JOINING   受   AS  PREFIX.
003294*                           ［ＲＬ＝  １２８］
003295 FD  負傷原因Ｆ          BLOCK   CONTAINS   1   RECORDS.
003296     COPY HUGEIN     OF  XFDLIB  JOINING   負原   AS  PREFIX.
003297*                           ［ＲＬ＝  １２８］
003298 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
003299     COPY HUSYOU     OF  XFDLIB  JOINING   負   AS  PREFIX.
003300*                           ［ＲＬ＝  １２８］
003301 FD  受付データＦ        BLOCK   CONTAINS   1   RECORDS.
003310     COPY UKETUKE    OF  XFDLIB  JOINING   受付 AS  PREFIX.
003320*                           ［ＲＬ＝  ２５６］
003330 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
003340     COPY SEKIROK    OF  XFDLIB  JOINING   施記 AS  PREFIX.
003350*                           ［ＲＬ＝  ２５６］
003360 FD  制御情報マスタ      BLOCK   CONTAINS   1   RECORDS.
003370     COPY SEIGYO     OF  XFDLIB  JOINING   制   AS  PREFIX.
003380*                           ［ＲＬ＝  １２８］
003390 FD  会計データＦ        BLOCK   CONTAINS   1   RECORDS.
003400     COPY KAIKEI     OF  XFDLIB  JOINING   会   AS  PREFIX.
003410*                           ［ＲＬ＝  １２８］
003420 FD  長期継続者Ｆ        BLOCK   CONTAINS   1   RECORDS.
003430     COPY CHOKEI     OF  XFDLIB  JOINING   長継   AS  PREFIX.
003440*                           ［ＲＬ＝  ６４］
003450 FD  バーコード管理Ｆ    BLOCK   CONTAINS   1   RECORDS.
003460     COPY BARKANR    OF  XFDLIB  JOINING   バ管  AS  PREFIX.
003470*
003480 FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
003490     COPY RECEPT     OF  XFDLIB  JOINING   レセ   AS  PREFIX.
003500*                           ［ＲＬ＝  ３８４０］
003510 FD  カルテファイル      BLOCK   CONTAINS   1   RECORDS.
003520     COPY KARUTE     OF  XFDLIB  JOINING   カ   AS  PREFIX.
003530     COPY KARUTA     OF  XFDLIB  JOINING   カ表 AS  PREFIX.
003540     COPY KARUTB     OF  XFDLIB  JOINING   カ裏 AS  PREFIX.
003550*                           ［ＲＬ＝  ８３２］
003560 FD  メモファイル        BLOCK CONTAINS 1     RECORDS.
003570     COPY MEMO       OF  XFDLIB JOINING メモ AS PREFIX.
003571*
003572 FD  摘要ファイル        BLOCK CONTAINS 1     RECORDS.
003573     COPY TEKIYO     OF    XFDLIB JOINING 摘要 AS PREFIX.
003574*
003580*                           ［ＲＬ＝  ７６８］
003590 FD  ＨレセプトＦ        BLOCK   CONTAINS   1   RECORDS.
003600     COPY H_RECE     OF  XFDLIB  JOINING   Ｈレセ  AS  PREFIX.
003610*                           ［ＲＬ＝  ５１２］
003620 FD  Ｈ日計データＦ      BLOCK   CONTAINS   1   RECORDS.
003630     COPY H_NIKEI    OF  XFDLIB  JOINING   Ｈ日   AS  PREFIX.
003640*                           ［ＲＬ＝  ６４０］
003650 FD  Ｈ負傷データＦ      BLOCK   CONTAINS   1   RECORDS.
003660     COPY H_HUSYOU   OF  XFDLIB  JOINING   Ｈ負 AS  PREFIX.
003670*
003680 FD  受診者情報２Ｆ      BLOCK   CONTAINS   1   RECORDS.
003690     COPY JUSINJ2    OF  XFDLIB  JOINING   受２   AS  PREFIX.
003700*
003710 FD  生保情報Ｆ          BLOCK   CONTAINS   1   RECORDS.
003720     COPY SEIHOJ     OF  XFDLIB  JOINING   生保   AS  PREFIX.
003730*
003740 FD  自賠責情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
003750     COPY JIBAIJ     OF  XFDLIB  JOINING   自賠   AS  PREFIX.
003760*
003770 FD  労災情報Ｆ          BLOCK   CONTAINS   1   RECORDS.
003780     COPY ROUSAIJ    OF  XFDLIB  JOINING   労災   AS  PREFIX.
003790*
003800 FD  会計領収Ｆ          BLOCK   CONTAINS   1   RECORDS.
003810     COPY RYOSYU     OF  XFDLIB  JOINING   領  AS  PREFIX.
003811*
003812 FD  ＤＭ記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
003813     COPY DMKIROK    OF  XFDLIB  JOINING   ＤＭ   AS  PREFIX.
003814*
003815 FD  施設受診者マスタ  BLOCK   CONTAINS   1   RECORDS.
003816     COPY SISETJU    OF  XFDLIB  JOINING   施設受   AS  PREFIX.
003817*
003818 FD  Ｈ往療実績Ｆ   BLOCK   CONTAINS   1   RECORDS.
003819     COPY H_NOURYO   OF  XFDLIB  JOINING   Ｈ往実   AS  PREFIX.
003820*
003821 FD  Ｈ往療予定Ｆ   BLOCK   CONTAINS   1   RECORDS.
003822     COPY H_NOYOTE   OF  XFDLIB  JOINING   Ｈ往予   AS  PREFIX.
003824*
003825 FD  レセプト労自Ｆ      BLOCK   CONTAINS   1   RECORDS.
003826     COPY RECERJ          OF  XFDLIB  JOINING   レセ労自 AS  PREFIX.
003827*
003828 FD  証明書履歴Ｆ      BLOCK   CONTAINS   1   RECORDS GLOBAL.
003829     COPY SYOMEIRR         OF  XFDLIB  JOINING   証履     AS  PREFIX.
003830*
003831***
003832*
003833 FD  他受診者情報Ｆ      BLOCK   CONTAINS   1   RECORDS.
003834     COPY JUSINJ     OF  XFDLIB  JOINING   他受   AS  PREFIX.
003835*                           ［ＲＬ＝  2048］
003836 FD  Ｈ報告書Ｆ          BLOCK   CONTAINS   1   RECORDS GLOBAL.
003837     COPY H_HOKOK         OF  XFDLIB  JOINING   Ｈ報     AS  PREFIX.
003838*
000380 FD  Ｈレセプト詳細Ｆ        BLOCK   CONTAINS   1   RECORDS.
000390     COPY H_RECEHK    OF  XFDLIB  JOINING   Ｈレセ詳細  AS  PREFIX.
000113*
003839*---------------------------------------------------------------*
003840******************************************************************
003850*                WORKING-STORAGE SECTION                         *
003860******************************************************************
003870 WORKING-STORAGE         SECTION.
003880 01 キー入力                           PIC X    VALUE SPACE.
003890 01 状態キー                           PIC X(2) VALUE SPACE.
003900 01 終了フラグ                         PIC X(3) VALUE SPACE.
003901 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
003910 01 カウンタ                           PIC 9    VALUE ZERO.
003920 01 ファイル名Ｗ                       PIC N(5) VALUE SPACE.
003930 01 遅延フラグ                         PIC X(3) VALUE SPACE.
003940 01 遅延ＣＮＴ                         PIC 9(5) VALUE ZERO.
003950 01 遅延カウンタ                       PIC 9(4) VALUE ZERO.
003960 01 遅延回数Ｗ                         PIC 9(4) VALUE ZERO.
003970*
003980 01 施術区分Ｗ                         PIC 9 VALUE ZERO.
003981 01 保険分類Ｗ                         PIC 9(2) VALUE ZERO.
003982**
003983 01 繰返カウンタ                       PIC 9(2) VALUE ZERO.
003984*摘要ファイルの制御区分のMAX区分
003985 01 摘要区分数                         PIC 9(2) VALUE 10.
003986**
003987*
003988 01 患者コードＷ.
003989    03 患者番号Ｗ                      PIC 9(6) VALUE ZERO.
003990    03 枝番Ｗ                          PIC X    VALUE SPACE.
003991 01 施術和暦年月Ｗ.
003992    03 施術和暦Ｗ                      PIC 9    VALUE ZERO.
003993    03 施術年Ｗ                        PIC 9(2) VALUE ZERO.
003994    03 施術月Ｗ                        PIC 9(2) VALUE ZERO.
003995*
003996****
003997 01 レセ存在フラグ                     PIC X(3)  VALUE SPACE.
003998**
003999 01 往療実績更新フラグ                 PIC X(3)  VALUE SPACE.
004000**
004001**
004002*
004003* バーコード管理Ｆ退避用
004010     COPY BARKANR    OF  XFDLIB  JOINING   退避バ AS  PREFIX.
004020*
004030* Ｈ負傷データＦ退避用
004040     COPY H_HUSYOU   OF  XFDLIB  JOINING   退避負 AS  PREFIX.
004050*
004060*****************************************************************
004070*                          連結項目                             *
004080*****************************************************************
004090*
004100* 削除連携キー
004110*処理区分 1:当月削除(in 施術和暦年月,患者コード)
004120*         2:全部削除(in 患者コード)
004130*         3:患者番号変更(in 患者コード,変更患者コード)
004140*         4:枝番作成(in 施術和暦年月,患者コード,変更患者コード)
004150 01 連患者削除－キー IS EXTERNAL.
004160    03 連患者削除－処理区分                PIC 9.
004170    03 連患者削除－施術和暦年月.
004180       05 連患者削除－施術和暦             PIC 9.
004190       05 連患者削除－施術年月.
004200          07 連患者削除－施術年            PIC 9(2).
004210          07 連患者削除－施術月            PIC 9(2).
004220    03 連患者削除－患者コード.
004230       05 連患者削除－患者番号             PIC 9(6).
004240       05 連患者削除－枝番                 PIC X.
004250    03 連患者削除－変更患者コード.
004260       05 連患者削除－変更患者番号         PIC 9(6).
004270       05 連患者削除－変更枝番             PIC X.
004280*
004290* 枝番連携キー
004300*
004310 01 連患者枝番－キー IS EXTERNAL.
004320    03 連患者枝番－変更日                   PIC 9(2).
004330    03 連患者枝番－保険変更区分             PIC 9.
004340*
004350 01 Ｈ連枝番作成－キー IS EXTERNAL.
004360   03 Ｈ連枝番作成－施術和暦年月.
004370      05 Ｈ連枝番作成－施術和暦              PIC 9.
004380      05 Ｈ連枝番作成－施術年月.
004390        07 Ｈ連枝番作成－施術年              PIC 9(2).
004400        07 Ｈ連枝番作成－施術月              PIC 9(2).
004410   03 Ｈ連枝番作成－患者コード.
004420     05 Ｈ連枝番作成－患者番号               PIC 9(6).
004430     05 Ｈ連枝番作成－枝番                   PIC X.
004440   03 Ｈ連枝番作成－老人判定                 PIC 9.
004450   03 Ｈ連枝番作成－元患者コード.
004451     05 Ｈ連枝番作成－元患者番号             PIC 9(6).
004452     05 Ｈ連枝番作成－元枝番                 PIC X.
004453*
004460* 年齢判定用
004470 01 連年齢２－キー IS EXTERNAL.
004480    03 連年齢２－施術和暦年月.
004490       05 連年齢２－施術和暦      PIC 9.
004500       05 連年齢２－施術年月.
004510         07 連年齢２－施術年      PIC 9(2).
004520         07 連年齢２－施術月      PIC 9(2).
004530    03 連年齢２－和暦年月日.
004540       05 連年齢２－和暦          PIC 9.
004550       05 連年齢２－年            PIC 9(2).
004560       05 連年齢２－月            PIC 9(2).
004570       05 連年齢２－日            PIC 9(2).
004580    03 連年齢２－保険年齢         PIC 9(3).
004590    03 連年齢２－乳幼児判定       PIC 9.
004600    03 連年齢２－老人判定         PIC 9.
004610*
004611* 新柔計算連結
004612 01 連Ｙ計算－キー IS EXTERNAL.
004613   03 連Ｙ計算－施術和暦年月.
004614     05 連Ｙ計算－施術和暦           PIC 9.
004615     05 連Ｙ計算－施術年月.
004616       07 連Ｙ計算－施術年           PIC 9(2).
004617       07 連Ｙ計算－施術月           PIC 9(2).
004618   03 連Ｙ計算－患者コード.
004619     05 連Ｙ計算－患者番号           PIC 9(6).
004620     05 連Ｙ計算－枝番               PIC X.
004621*
004622******************************************************************
004630*                      PROCEDURE  DIVISION                       *
004640******************************************************************
004650 PROCEDURE               DIVISION.
004660************
004670*           *
004680* 初期処理   *
004690*           *
004700************
004710     PERFORM 初期化.
004720     PERFORM 制御情報取得.
004730************
004740*           *
004750* 主処理     *
004760*           *
004770************
004780*--------------------------------------------------------*
004790*  連患者削除－処理区分
004800*    1:当月削除(in 施術和暦年月,患者コード)
004810*    2:全部削除(in 患者コード)
004820*    3:患者番号変更(in 患者コード,変更患者コード)
004830*    4:枝番作成(in 施術和暦年月,患者コード,変更患者コード)
004840*--------------------------------------------------------*
004850
004860     EVALUATE 連患者削除－処理区分
004870     WHEN 1
004880         PERFORM データ当月削除
004890         PERFORM はりきゅうデータ当月削除
004900     WHEN 2
004910         PERFORM データ全削除
004920         PERFORM はりきゅうデータ全削除
004930     WHEN 3
004940         PERFORM データ変更
004950         PERFORM はりきゅうデータ変更
004960     WHEN 4
004970         PERFORM 枝番作成
004980         PERFORM はりきゅう枝番作成
004981*        / CLOSE済み 終了する /
004982         EXIT PROGRAM
004990     WHEN OTHER
005000        DISPLAY "処理区分エラー。処理できません。"  UPON CONS
005010     END-EVALUATE.
005020************
005030*           *
005040* 終了処理   *
005050*           *
005060************
005070     PERFORM 終了処理.
005080     EXIT PROGRAM.
005090*
005100*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
005110*================================================================*
005120 初期化 SECTION.
005130*
005140     OPEN I-O 受診者情報Ｆ.
005150             MOVE NC"受診" TO ファイル名Ｗ.
005160             PERFORM オープンチェック.
005170     OPEN I-O 負傷原因Ｆ.
005180             MOVE NC"負傷原因" TO ファイル名Ｗ.
005190             PERFORM オープンチェック.
005200     OPEN I-O 負傷データＦ.
005210             MOVE NC"負傷" TO ファイル名Ｗ.
005220             PERFORM オープンチェック.
005230     OPEN I-O 受付データＦ.
005240             MOVE NC"受付" TO ファイル名Ｗ.
005250             PERFORM オープンチェック.
005260     OPEN I-O 施術記録Ｆ.
005270             MOVE NC"施記" TO ファイル名Ｗ.
005280             PERFORM オープンチェック.
005290     OPEN I-O 制御情報マスタ.
005300             MOVE NC"制御" TO ファイル名Ｗ.
005310             PERFORM オープンチェック.
005320     OPEN I-O 会計データＦ.
005330             MOVE NC"会計" TO ファイル名Ｗ.
005340             PERFORM オープンチェック.
005350     OPEN I-O 長期継続者Ｆ.
005360             MOVE NC"長継" TO ファイル名Ｗ.
005370             PERFORM オープンチェック.
005380     OPEN I-O バーコード管理Ｆ.
005390             MOVE NC"バー" TO ファイル名Ｗ.
005400             PERFORM オープンチェック.
005410     OPEN I-O レセプトＦ.
005420             MOVE NC"レセ" TO ファイル名Ｗ.
005430             PERFORM オープンチェック.
005440     OPEN I-O カルテファイル.
005450             MOVE NC"カルテ" TO ファイル名Ｗ.
005460             PERFORM オープンチェック.
005470     OPEN I-O メモファイル.
005480             MOVE NC"メモ" TO ファイル名Ｗ.
005490             PERFORM オープンチェック.
005491     OPEN I-O 摘要ファイル.
005492             MOVE NC"摘要" TO ファイル名Ｗ.
005493             PERFORM オープンチェック.
005500     OPEN I-O ＨレセプトＦ.
005510             MOVE NC"ＨレセプトＦ" TO ファイル名Ｗ.
005520             PERFORM オープンチェック.
005530     OPEN I-O Ｈ日計データＦ.
005540             MOVE NC"Ｈ日計データＦ" TO ファイル名Ｗ.
005550             PERFORM オープンチェック.
005560     OPEN I-O Ｈ負傷データＦ.
005570             MOVE NC"Ｈ負傷データＦ" TO ファイル名Ｗ.
005580             PERFORM オープンチェック.
005590     OPEN I-O 受診者情報２Ｆ.
005600             MOVE NC"受診２" TO ファイル名Ｗ.
005610             PERFORM オープンチェック.
005620     OPEN I-O 生保情報Ｆ.
005630             MOVE NC"生保" TO ファイル名Ｗ.
005640             PERFORM オープンチェック.
005650     OPEN I-O 自賠責情報Ｆ.
005660             MOVE NC"自賠" TO ファイル名Ｗ.
005670             PERFORM オープンチェック.
005680     OPEN I-O 労災情報Ｆ.
005690             MOVE NC"労災" TO ファイル名Ｗ.
005700             PERFORM オープンチェック.
005710     OPEN I-O 会計領収Ｆ.
005720             MOVE NC"会計" TO ファイル名Ｗ.
005730             PERFORM オープンチェック.
005731     OPEN I-O ＤＭ記録Ｆ.
005732             MOVE NC"ＤＭ記録" TO ファイル名Ｗ.
005733             PERFORM オープンチェック.
005734     OPEN I-O Ｈ往療実績Ｆ.
005735             MOVE NC"Ｈ往療実績Ｆ" TO ファイル名Ｗ.
005736             PERFORM オープンチェック.
005737     OPEN I-O 施設受診者マスタ.
005738             MOVE NC"施設受診者マスタ" TO ファイル名Ｗ.
005739             PERFORM オープンチェック.
005740     OPEN I-O Ｈ往療予定Ｆ.
005741             MOVE NC"Ｈ往療予定Ｆ" TO ファイル名Ｗ.
005742             PERFORM オープンチェック.
005743     OPEN I-O レセプト労自Ｆ.
005744             MOVE NC"レセプト労自Ｆ" TO ファイル名Ｗ.
005745             PERFORM オープンチェック.
005746     OPEN I-O 証明書履歴Ｆ.
005747             MOVE NC"証明書履歴Ｆ" TO ファイル名Ｗ.
005748             PERFORM オープンチェック.
005754     OPEN I-O Ｈ報告書Ｆ.
005755             MOVE NC"Ｈ報告書Ｆ" TO ファイル名Ｗ.
005756             PERFORM オープンチェック.
005500     OPEN I-O Ｈレセプト詳細Ｆ.
005900             IF 状態キー  =  "35"
                       OPEN OUTPUT Ｈレセプト詳細Ｆ
                       CLOSE Ｈレセプト詳細Ｆ
005500                 OPEN I-O Ｈレセプト詳細Ｆ
                   END-IF.
005510             MOVE NC"Ｈレセプト詳細Ｆ" TO ファイル名Ｗ.
005520             PERFORM オープンチェック.
005760*
005761*================================================================*
005762 ファイル閉鎖 SECTION.
005763*
005770     CLOSE 受診者情報Ｆ     負傷原因Ｆ     負傷データＦ   受付データＦ
005780           制御情報マスタ   施術記録Ｆ     会計データＦ   長期継続者Ｆ
005790           バーコード管理Ｆ レセプトＦ     カルテファイル メモファイル 摘要ファイル
005800           ＨレセプトＦ     Ｈ日計データＦ Ｈ負傷データＦ
005810           受診者情報２Ｆ   生保情報Ｆ     自賠責情報Ｆ   労災情報Ｆ
005820           会計領収Ｆ       ＤＭ記録Ｆ     Ｈ往療実績Ｆ   Ｈ往療予定Ｆ レセプト労自Ｆ 証明書履歴Ｆ
005821           施設受診者マスタ Ｈ報告書Ｆ     Ｈレセプト詳細Ｆ.
005830*================================================================*
005840 終了処理 SECTION.
005850*
005860     PERFORM ファイル閉鎖.
005870*================================================================*
005880 オープンチェック SECTION.
005890*
005900     IF 状態キー  NOT =  "00"
005910         DISPLAY ファイル名Ｗ NC"Ｆオープンエラー" UPON CONS
005920         DISPLAY NC"状態キー：" 状態キー           UPON CONS
005930         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
005940                                                   UPON CONS
005950*-----------------------------------------*
005960         CALL "actcshm"  WITH C LINKAGE
005970*-----------------------------------------*
005980         ACCEPT  キー入力 FROM CONS
005990         PERFORM ファイル閉鎖
006000         EXIT PROGRAM.
006010*
006020*================================================================*
006030 エラー表示Ｒ SECTION.
006040*
006050     DISPLAY ファイル名Ｗ NC"ファイル読込エラー"   UPON CONS.
006060     DISPLAY NC"状態キー：" 状態キー               UPON CONS.
006070     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
006080     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
006090*-----------------------------------------*
006100     CALL "actcshm"  WITH C LINKAGE.
006110*-----------------------------------------*
006120     ACCEPT  キー入力 FROM CONS.
006130*     PERFORM ファイル閉鎖.
006140*     EXIT PROGRAM.
006150*================================================================*
006160 エラー表示 SECTION.
006170*
006180     DISPLAY ファイル名Ｗ NC"ファイル書込エラー"   UPON CONS.
006190     DISPLAY NC"状態キー：" 状態キー               UPON CONS.
006200     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
006210     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
006220*-----------------------------------------*
006230     CALL "actcshm"  WITH C LINKAGE.
006240*-----------------------------------------*
006250     ACCEPT  キー入力 FROM CONS.
006260*     PERFORM ファイル閉鎖.
006270*     EXIT PROGRAM.
006280*================================================================*
006290 エラー表示Ｄ SECTION.
006300*
006310     DISPLAY ファイル名Ｗ NC"ファイル削除エラー"   UPON CONS.
006320     DISPLAY NC"状態キー：" 状態キー               UPON CONS.
006330     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
006340     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
006350*-----------------------------------------*
006360     CALL "actcshm"  WITH C LINKAGE.
006370*-----------------------------------------*
006380     ACCEPT  キー入力 FROM CONS.
006390*     PERFORM ファイル閉鎖.
006400*     EXIT PROGRAM.
006410*================================================================*
006420 データ当月削除 SECTION.
006430*
006440     PERFORM 受診者情報Ｆ当月削除.
006441     PERFORM 受診者情報２Ｆ当月削除.
006442     PERFORM 受診者情報ゼロ再構築.
006450*
006460     PERFORM 負傷データＦ当月削除.
006470     PERFORM 受付データＦ当月削除.
006480     PERFORM 施術記録当月削除.
006490     PERFORM 会計データＦ当月削除.
006500     PERFORM 長期継続者Ｆ当月削除.
006510     PERFORM バーコード管理Ｆ当月削除.
006511     PERFORM レセプトＦ当月削除.
006520     PERFORM レセプト労自Ｆ当月削除.
006530     PERFORM カルテファイル当月削除.
006531     PERFORM メモファイル当月削除.
006540     PERFORM 摘要ファイル当月削除.
006550*
006570     PERFORM 生保情報Ｆ当月削除.
006580     PERFORM 自賠責情報Ｆ当月削除.
006590     PERFORM 労災情報Ｆ当月削除.
006600     PERFORM 会計領収Ｆ当月削除.
006610*
006620*================================================================*
006630 はりきゅうデータ当月削除 SECTION.
006640*
006650     MOVE 1 TO 施術区分Ｗ.
006660     PERFORM ＨレセプトＦ当月削除.
006670     PERFORM Ｈ日計データＦ当月削除.
006680     PERFORM Ｈ報告書Ｆ当月削除.
006681*
006690     MOVE 2 TO 施術区分Ｗ.
006700     PERFORM ＨレセプトＦ当月削除.
006710     PERFORM Ｈ日計データＦ当月削除.
006720     PERFORM Ｈ報告書Ｆ当月削除.
006721*
006722*
006730*================================================================*
006740 データ全削除 SECTION.
006750*
006760     PERFORM 受診者情報Ｆ全削除.
006770     IF 連患者削除－枝番 = SPACE
006780         PERFORM 負傷原因Ｆ全削除
006790     END-IF.
006800     PERFORM 負傷データＦ全削除.
006810     PERFORM 受付データＦ全削除.
006820     PERFORM 施術記録全削除.
006830     PERFORM 会計データＦ全削除.
006840     PERFORM 長期継続者Ｆ全削除.
006850     PERFORM バーコード管理Ｆ全削除.
006851     PERFORM レセプトＦ全削除.
006860     PERFORM レセプト労自Ｆ全削除.
006870     PERFORM カルテファイル全削除.
006871     PERFORM メモファイル全削除.
006880     PERFORM 摘要ファイル全削除.
006890*
006900     PERFORM 受診者情報２Ｆ全削除.
006910     PERFORM 生保情報Ｆ全削除.
006920     PERFORM 自賠責情報Ｆ全削除.
006930     PERFORM 労災情報Ｆ全削除.
006931     PERFORM 会計領収Ｆ全削除.
006932     PERFORM ＤＭ記録Ｆ全削除.
006940     PERFORM 施設受診者マスタ全削除.
006941     PERFORM 証明書履歴Ｆ全削除.
006950*
006960*================================================================*
006970 はりきゅうデータ全削除 SECTION.
006980*
006990     MOVE 1 TO 施術区分Ｗ.
007000     PERFORM ＨレセプトＦ全削除.
007010     PERFORM Ｈ日計データＦ全削除.
007011     PERFORM Ｈ負傷データＦ全削除.
007012     PERFORM Ｈ往療実績Ｆ全削除.
007030     PERFORM Ｈ報告書Ｆ全削除.
007031*
007040     MOVE 2 TO 施術区分Ｗ.
007050     PERFORM ＨレセプトＦ全削除.
007060     PERFORM Ｈ日計データＦ全削除.
007070     PERFORM Ｈ負傷データＦ全削除.
007071     PERFORM Ｈ往療実績Ｆ全削除.
007073     PERFORM Ｈ報告書Ｆ全削除.
007074*
007075*    1回でOK
007076     PERFORM Ｈ往療予定Ｆ全削除.
007091*================================================================*
007100 データ変更 SECTION.
007110*
007120     PERFORM 受診者情報Ｆ変更.
007130*
007140     PERFORM 負傷データＦ変更.
007150     PERFORM 受付データＦ変更.
007160     PERFORM 施術記録変更.
007170     PERFORM 会計データＦ変更.
007180     PERFORM 長期継続者Ｆ変更.
007190     PERFORM バーコード管理Ｆ変更.
007191     PERFORM レセプトＦ変更.
007200     PERFORM レセプト労自Ｆ変更.
007210     PERFORM カルテファイル変更.
007211     PERFORM メモファイル変更.
007220     PERFORM 摘要ファイル変更.
007221     PERFORM 負傷原因コンバート.
007230*
007240     PERFORM 受診者情報２Ｆ変更.
007250     PERFORM 生保情報Ｆ変更.
007260     PERFORM 自賠責情報Ｆ変更.
007270     PERFORM 労災情報Ｆ変更.
007280     PERFORM 会計領収Ｆ変更.
007281     PERFORM ＤＭ記録Ｆ変更.
007282     PERFORM 施設受診者マスタ変更.
007283     PERFORM 証明書履歴Ｆ変更.
007290*
007300*================================================================*
007310 はりきゅうデータ変更 SECTION.
007320*
007330     MOVE 1 TO 施術区分Ｗ.
007340     PERFORM ＨレセプトＦ変更.
007350     PERFORM Ｈ日計データＦ変更.
007360     PERFORM Ｈ負傷データＦ変更.
007361     PERFORM Ｈ往療実績Ｆ変更.
007371     PERFORM Ｈ報告書Ｆ変更.
007372*
007380     MOVE 2 TO 施術区分Ｗ.
007390     PERFORM ＨレセプトＦ変更.
007400     PERFORM Ｈ日計データＦ変更.
007410     PERFORM Ｈ負傷データＦ変更.
007411     PERFORM Ｈ往療実績Ｆ変更.
007414     PERFORM Ｈ報告書Ｆ変更.
007415*
007416*    1回でOK
007417     PERFORM Ｈ往療予定Ｆ変更.
007421*    往療場所患者コード/先順患者コード更新
007422     PERFORM Ｈ往療実績Ｆ変更２.
007424*
007430*================================================================*
007440 枝番作成 SECTION.
007450*
007460*/元番号を指定枝番にコピー
007470     PERFORM 受診者情報Ｆ枝番作成.
007480     PERFORM 負傷データＦ枝番作成.
007490     PERFORM 長期継続者Ｆ枝番作成.
007500     PERFORM カルテファイル枝番作成.
007510     PERFORM 受診者情報２Ｆ枝番作成.
007520     PERFORM 生保情報Ｆ枝番作成.
007530     PERFORM 自賠責情報Ｆ枝番作成.
007540     PERFORM 労災情報Ｆ枝番作成.
007541     PERFORM 摘要ファイル枝番作成.
007550*
007560*/元番号を指定枝番にコピー。元番号の一部通院データを削除。
007570     PERFORM 受付データＦ枝番作成.
007580     PERFORM 施術記録枝番作成.
007590     PERFORM 会計データＦ枝番作成.
007600     PERFORM バーコード管理Ｆ枝番作成.
007601     PERFORM レセプトＦ枝番作成.
007610     PERFORM レセプト労自Ｆ枝番作成.
007620     PERFORM メモファイル枝番作成.
007630     PERFORM 会計領収Ｆ枝番作成.
007631     PERFORM ＤＭ記録Ｆ枝番作成.
007640*
007643*-------------------------------------------------------------------**
007644* ykeisan
007649*
007651*    / CLOSEしないと、YKEISANでうまくいかない /
007653     PERFORM 終了処理.
007654*
007655*
007656*   / レセプトＦある時のみ計算 /
007657     IF レセ存在フラグ = "YES"
007660* 元番号
007661        MOVE SPACE TO 連Ｙ計算－キー
007662        INITIALIZE 連Ｙ計算－キー
007663        MOVE 連患者削除－施術和暦   TO 連Ｙ計算－施術和暦
007664        MOVE 連患者削除－施術年     TO 連Ｙ計算－施術年
007665        MOVE 連患者削除－施術月     TO 連Ｙ計算－施術月
007666        MOVE 連患者削除－患者コード TO 連Ｙ計算－患者コード
007667        CALL   "YKEISAN"
007668        CANCEL "YKEISAN"
007669***
007670* 枝番あり
007671        MOVE 連患者削除－変更枝番   TO 連Ｙ計算－枝番
007672        CALL   "YKEISAN"
007673        CANCEL "YKEISAN"
007674     END-IF.
007677*-------------------------------------------------------------------**
007681*
007682*================================================================*
007683 はりきゅう枝番作成 SECTION.
007684*
007685     MOVE SPACE TO Ｈ連枝番作成－キー.
007690     INITIALIZE    Ｈ連枝番作成－キー.
007700*
007710     MOVE 連患者削除－施術和暦     TO Ｈ連枝番作成－施術和暦.
007720     MOVE 連患者削除－施術年       TO Ｈ連枝番作成－施術年.
007730     MOVE 連患者削除－施術月       TO Ｈ連枝番作成－施術月.
007740     MOVE 連患者削除－変更患者番号 TO Ｈ連枝番作成－患者番号.
007750     MOVE 連患者削除－変更枝番     TO Ｈ連枝番作成－枝番.
007760     MOVE 連年齢２－老人判定       TO Ｈ連枝番作成－老人判定.
007761*
007762     MOVE 連患者削除－患者番号     TO Ｈ連枝番作成－元患者番号.
007763     MOVE 連患者削除－枝番         TO Ｈ連枝番作成－元枝番.
007764*
007770     CALL   "HM012".
007780     CANCEL "HM012".
007790*
007822*================================================================*
007823 受診者情報Ｆ当月削除 SECTION.
007824* 
007830     MOVE 連患者削除－施術和暦年月 TO 受－施術和暦年月.
007840     MOVE 連患者削除－患者コード   TO 受－患者コード.
007850*
007860     READ 受診者情報Ｆ
007870     NOT INVALID KEY
007880         DELETE 受診者情報Ｆ
007890         INVALID KEY
007900             MOVE NC"受診" TO ファイル名Ｗ
007910             PERFORM エラー表示Ｄ
007920         END-DELETE
007930     END-READ.
007931*
007934*================================================================*
007935 受診者情報ゼロ再構築 SECTION.
007936* 
007937* JISINJ,JUSINJ2のZEROレコード再構築
007939*
007943     OPEN INPUT 他受診者情報Ｆ.
007944          MOVE NC"他受" TO ファイル名Ｗ.
007945          PERFORM オープンチェック.
007958*
007971     MOVE 連患者削除－患者コード TO 患者コードＷ.
007972     MOVE 連患者削除－患者コード TO 他受－患者コード.
007973     MOVE 99999                  TO 他受－施術和暦年月.
007974     START 他受診者情報Ｆ KEY IS < 他受－患者コード
007975                                   他受－施術和暦年月
007976                                   REVERSED
007977     END-START.
007978     IF 状態キー = "00"
007979                 MOVE SPACE TO 終了フラグ２
007980                 PERFORM 他受診者情報Ｆ読込
007981                 IF (他受－患者コード = 患者コードＷ) AND
007982                    (他受－施術和暦年月 NOT = ZERO)  AND ( 終了フラグ２ = SPACE )
007983                     MOVE 他受－レコード TO 受－レコード
007984                     MOVE 他受－施術和暦年月 TO 受－施術和暦年月Ｎ
007985                     MOVE 他受－施術和暦年月 TO 施術和暦年月Ｗ
007986                     MOVE ZERO  TO 受－施術和暦
007987                     MOVE ZERO  TO 受－施術年  
007988                     MOVE ZERO  TO 受－施術月  
007989                     MOVE ZERO  TO 受－請求和暦
007990                     MOVE ZERO  TO 受－請求年  
007991                     MOVE ZERO  TO 受－請求月  
007992* 初期化する
007993                     MOVE ZERO           TO 受－継続区分
007994                     MOVE ZERO           TO 受－請求区分
007995                     MOVE ZERO           TO 受－レセ印刷区分
007996                     MOVE ZERO           TO 受－レセ印刷区分助成
007997                     MOVE ZERO           TO 受－老人負担回数
007998                     MOVE ZERO           TO 受－老人負担額累計
007999                     MOVE ZERO           TO 受－老人最終負担日
008000                     MOVE ZERO           TO 受－老人負担額端数
008001                     MOVE ZERO           TO 受－老人負担無し
008002                     MOVE SPACE          TO 受－枝番作成時枝番
008003                     MOVE ZERO           TO 受－はり区分
008004                     MOVE ZERO           TO 受－あんま区分
008005                     MOVE ZERO           TO 受－柔整有区分
008006                     MOVE ZERO           TO 受－鍼灸有区分
008007                     MOVE ZERO           TO 受－マッサージ有区分
008008                     MOVE ZERO           TO 受－併用区分
008009                     INITIALIZE             受－保険変更情報
008010                     MOVE ZERO           TO 受－助成月途中開始日
008011*
008012                     REWRITE 受－レコード
008013	                   IF 状態キー NOT = "00"
008014                        MOVE NC"受診" TO ファイル名Ｗ
008015                        PERFORM エラー表示
008020                     END-IF
008021*
008022                     PERFORM 受診者情報２Ｆ再構築処理
008023*
008024                 ELSE
008025                     IF (他受－患者コード = 患者コードＷ) AND (他受－施術和暦年月 = ZERO) AND ( 終了フラグ２ = SPACE )
008026                          MOVE 患者コードＷ  TO 受－患者コード
008027                          MOVE ZERO          TO 受－施術和暦年月
008028                          DELETE 受診者情報Ｆ
008029                          INVALID KEY
008030                              MOVE NC"受診" TO ファイル名Ｗ
008031                              PERFORM エラー表示Ｄ
008032                          END-DELETE
008033*
008034                          MOVE 患者コードＷ  TO 受２－患者コード
008035                          MOVE ZERO          TO 受２－施術和暦年月
008036                          DELETE 受診者情報２Ｆ
008037                          INVALID KEY
008038                              MOVE NC"受診２" TO ファイル名Ｗ
008039                              PERFORM エラー表示Ｄ
008040                          END-DELETE
008041                     END-IF
008042                 END-IF
008061     END-IF.
008062*
008063     CLOSE 他受診者情報Ｆ.
008064*
008065*================================================================*
008066 受診者情報２Ｆ再構築処理 SECTION.
008067*
008068     MOVE 施術和暦年月Ｗ   TO 受２－施術和暦年月.
008069     MOVE 患者コードＷ     TO 受２－患者コード.
008070     READ 受診者情報２Ｆ
008071     NOT INVALID KEY
008072* 初期化する
008073                 MOVE ZERO           TO 受２－施術和暦
008074                 MOVE ZERO           TO 受２－施術年  
008075                 MOVE ZERO           TO 受２－施術月  
008076                 MOVE ZERO           TO 受２－請求和暦
008077                 MOVE ZERO           TO 受２－請求年  
008078                 MOVE ZERO           TO 受２－請求月  
008079                 MOVE ZERO           TO 受２－請求区分
008080                 MOVE ZERO           TO 受２－請求対象区分
008081                 MOVE ZERO           TO 受２－助成請求和暦
008082                 MOVE ZERO           TO 受２－助成請求年  
008083                 MOVE ZERO           TO 受２－助成請求月  
008084                 MOVE ZERO           TO 受２－助成請求区分
008085                 MOVE ZERO           TO 受２－助成請求対象区分
008086                 MOVE ZERO           TO 受２－来院動機区分
008087                 MOVE ZERO           TO 受２－紹介者患者番号
008088                 MOVE ZERO           TO 受２－柔整本体償還払い区分
008089                 MOVE ZERO           TO 受２－柔整本体まとめ区分
008090                 MOVE ZERO           TO 受２－柔整助成償還払い区分
008091                 MOVE ZERO           TO 受２－柔整助成レセ印刷対象区分
008092                 MOVE ZERO           TO 受２－柔整助成レセ会総括表印刷対象区分
008093                 MOVE ZERO           TO 受２－柔整本体枝番まとめ区分
008094*
008095         REWRITE 受２－レコード
008096         INVALID KEY
008097            MOVE NC"受診２" TO ファイル名Ｗ
008098            PERFORM エラー表示
008099         END-REWRITE
008100     END-READ.
008101*
008102*================================================================*
008103 他受診者情報Ｆ読込 SECTION.
008104*
008105     READ 他受診者情報Ｆ NEXT
008106     AT END
008107         MOVE "YES" TO 終了フラグ２
008108     END-READ.
008109*
008110*================================================================*
008111*================================================================*
008112*================================================================*
008113 受診者情報Ｆ全削除 SECTION.
008114*
008115     MOVE ZERO                   TO 受－施術和暦年月.
008116     MOVE 連患者削除－患者コード TO 受－患者コード.
008117*
008118     START 受診者情報Ｆ KEY IS >= 受－患者コード
008119                                  受－施術和暦年月
008120     END-START.
008121*
008122     IF 状態キー = "00"
008123         MOVE SPACE  TO 終了フラグ
008124         PERFORM 受診者情報Ｆ読込
008125         PERFORM UNTIL ( 連患者削除－患者コード NOT = 受－患者コード ) OR
008126                       ( 終了フラグ = "YES" )
008127             DELETE 受診者情報Ｆ
008128             INVALID KEY
008129                 MOVE NC"受診" TO ファイル名Ｗ
008130                 PERFORM エラー表示Ｄ
008131             END-DELETE
008140             PERFORM 受診者情報Ｆ読込
008150         END-PERFORM
008160     END-IF.
008170*================================================================*
008180 受診者情報Ｆ変更 SECTION.
008190*
008200     MOVE ZERO                   TO 受－施術和暦年月.
008210     MOVE 連患者削除－患者コード TO 受－患者コード.
008220*
008230     START 受診者情報Ｆ KEY IS >= 受－患者コード
008240                                  受－施術和暦年月
008250     END-START.
008260*
008270     IF 状態キー = "00"
008280         MOVE SPACE  TO 終了フラグ
008290         PERFORM 受診者情報Ｆ読込
008300         PERFORM UNTIL ( 連患者削除－患者コード NOT = 受－患者コード ) OR
008310                       ( 終了フラグ = "YES" )
008320             MOVE  連患者削除－変更患者コード TO  受－患者コード
008330             WRITE 受－レコード
008340             INVALID KEY
008350                 MOVE NC"受診" TO ファイル名Ｗ
008360                 PERFORM エラー表示
008370             END-WRITE
008380             MOVE  連患者削除－患者コード TO  受－患者コード
008390             DELETE 受診者情報Ｆ
008400             INVALID KEY
008410                 MOVE NC"受診" TO ファイル名Ｗ
008420                 PERFORM エラー表示Ｄ
008430             END-DELETE
008440             PERFORM 受診者情報Ｆ読込
008450         END-PERFORM
008460     END-IF.
008470*
008480*================================================================*
008490 受診者情報Ｆ枝番作成 SECTION.
008500*
008510*/元の受診者情報を読み、指定枝番にコピー。変更終了日セット
008520     MOVE 連患者削除－施術和暦年月 TO 受－施術和暦年月.
008530     MOVE 連患者削除－患者コード   TO 受－患者コード.
008540*
008550     READ 受診者情報Ｆ
008551     INVALID KEY
008552         MOVE NC"受診" TO ファイル名Ｗ
008553         PERFORM エラー表示Ｒ
008560     NOT INVALID KEY
008561         MOVE 受－保険分類 TO 保険分類Ｗ
008564*
008570         MOVE 連患者削除－変更枝番       TO 受－枝番
008580         MOVE 連患者枝番－変更日         TO 受－変更終了日
008590         MOVE 連患者枝番－保険変更区分   TO 受－保険変更区分
008600         WRITE 受－レコード
008610         INVALID KEY
008620             MOVE NC"受診" TO ファイル名Ｗ
008630             PERFORM エラー表示
008640         END-WRITE
008650     END-READ.
008651*
008660*/指定枝番データの００のデータ作成。（※健保のみ）
008662* 2:労災、3:自賠責、4:生保、5:自費は、月途中変更で枝番レコードを作らない。
008663     IF 保険分類Ｗ = 1
008664*
008670       MOVE ZERO     TO 受－施術和暦
008680       MOVE ZERO     TO 受－施術年
008690       MOVE ZERO     TO 受－施術月
008700       MOVE 連患者削除－変更患者コード TO 受－患者コード
008710       READ 受診者情報Ｆ
008720       INVALID KEY
008730         MOVE SPACE TO 受－レコード
008740         INITIALIZE    受－レコード
008750         MOVE 連患者削除－変更患者コード TO 受－患者コード
008760         MOVE 連患者削除－施術和暦年月   TO 受－施術和暦年月
008770         READ 受診者情報Ｆ
008780         NOT INVALID KEY
008790             MOVE ZERO  TO 受－施術和暦
008800             MOVE ZERO  TO 受－施術年  
008810             MOVE ZERO  TO 受－施術月  
008820             MOVE 連患者削除－施術和暦年月 TO 受－施術和暦年月Ｎ
008830             MOVE ZERO  TO 受－請求和暦
008840             MOVE ZERO  TO 受－請求年  
008850             MOVE ZERO  TO 受－請求月  
008852* 初期化する
008879             MOVE ZERO           TO 受－継続区分
008880             MOVE ZERO           TO 受－請求区分
008881             MOVE ZERO           TO 受－レセ印刷区分
008882             MOVE ZERO           TO 受－レセ印刷区分助成
008883             MOVE ZERO           TO 受－老人負担回数
008884             MOVE ZERO           TO 受－老人負担額累計
008885             MOVE ZERO           TO 受－老人最終負担日
008886             MOVE ZERO           TO 受－老人負担額端数
008887             MOVE ZERO           TO 受－老人負担無し
008888             MOVE SPACE          TO 受－枝番作成時枝番
008889             MOVE ZERO           TO 受－はり区分
008890             MOVE ZERO           TO 受－あんま区分
008891             MOVE ZERO           TO 受－柔整有区分
008892             MOVE ZERO           TO 受－鍼灸有区分
008893             MOVE ZERO           TO 受－マッサージ有区分
008894             MOVE ZERO           TO 受－併用区分
008895             INITIALIZE             受－保険変更情報
008896             MOVE ZERO           TO 受－助成月途中開始日
008897*
008898             WRITE 受－レコード
008899             INVALID KEY
008900                 MOVE NC"受診" TO ファイル名Ｗ
008901                 PERFORM エラー表示
008902             END-WRITE
008910         END-READ
008920       END-READ
008921     END-IF.
008922**
008930*/枝番なしの変更開始日をセット
008940     MOVE 連患者削除－施術和暦年月 TO 受－施術和暦年月.
008950     MOVE 連患者削除－患者コード   TO 受－患者コード.
008960*
008970     READ 受診者情報Ｆ
008980     NOT INVALID KEY
008990         MOVE 連患者枝番－変更日         TO 受－変更開始日
009000         MOVE 連患者枝番－保険変更区分   TO 受－保険変更区分
009010         MOVE 連患者削除－変更枝番       TO 受－枝番作成時枝番
009020         REWRITE 受－レコード
009030         INVALID KEY
009040             MOVE NC"受診" TO ファイル名Ｗ
009050             PERFORM エラー表示
009060         END-REWRITE
009070     END-READ.
009080*
009090*================================================================*
009100 受診者情報Ｆ読込 SECTION.
009110*
009120     READ 受診者情報Ｆ NEXT
009130     AT END
009140         MOVE "YES" TO 終了フラグ
009150     END-READ.
009160*
009170*================================================================*
009180 負傷原因Ｆ全削除 SECTION.
009190*
009200     MOVE 01           TO 負原－区分コード.
009210     MOVE ZERO         TO 負原－患者番号.
009220     MOVE 01           TO 負原－負傷原因連番.
009230*
009240     START 負傷原因Ｆ KEY IS >= 負原－区分コード
009250                                負原－負傷原因コード
009260     END-START.
009270     IF 状態キー = "00"
009280         MOVE SPACE TO 終了フラグ
009290         PERFORM 負傷原因Ｆ読込
009300         PERFORM UNTIL  終了フラグ NOT = SPACE 
009310             IF 連患者削除－患者番号 = 負原－患者番号
009320                 DELETE 負傷原因Ｆ
009330                 INVALID KEY
009340                     MOVE NC"負原" TO ファイル名Ｗ
009350                     PERFORM エラー表示Ｄ
009360                 END-DELETE
009370             END-IF
009380             PERFORM 負傷原因Ｆ読込
009390         END-PERFORM
009400     END-IF.
009410*================================================================*
009420 負傷原因コンバート SECTION.
009430*
009440     IF ( 連患者削除－枝番 = SPACE ) AND ( 連患者削除－変更枝番 = SPACE )
009450         MOVE 01           TO 負原－区分コード
009460         MOVE ZERO         TO 負原－患者番号
009470         MOVE 01           TO 負原－負傷原因連番
009480         MOVE SPACE        TO 終了フラグ
009490*
009500         START 負傷原因Ｆ KEY IS >= 負原－区分コード
009510                                    負原－負傷原因コード
009520         END-START
009530         IF 状態キー = "00"
009540             PERFORM 負傷原因Ｆ読込
009550             PERFORM UNTIL ( 終了フラグ = "YES" )
009560                 IF 連患者削除－患者番号 = 負原－患者番号
009570                     MOVE 連患者削除－変更患者番号  TO 負原－患者番号
009580                     WRITE 負原－レコード
009590                     INVALID KEY
009600                         MOVE NC"負原" TO ファイル名Ｗ
009610                         PERFORM エラー表示
009620                     END-WRITE
009630                     MOVE 連患者削除－患者番号  TO 負原－患者番号
009640                     DELETE 負傷原因Ｆ
009650                     INVALID KEY
009660                         MOVE NC"負原" TO ファイル名Ｗ
009670                         PERFORM エラー表示Ｄ
009680                     END-DELETE
009690                 END-IF
009700                 PERFORM 負傷原因Ｆ読込
009710             END-PERFORM
009720         END-IF
009730     END-IF.
009740*
009750*================================================================*
009760 負傷原因Ｆ読込 SECTION.
009770*
009780     READ 負傷原因Ｆ NEXT
009790     AT END
009800         MOVE "YES" TO 終了フラグ
009810     END-READ.
009820*================================================================*
009830 負傷データＦ当月削除 SECTION.
009840*
009850     MOVE 連患者削除－施術和暦年月 TO 負－施術和暦年月.
009860     MOVE 連患者削除－患者コード   TO 負－患者コード.
009870*
009880     READ 負傷データＦ
009890     NOT INVALID KEY
009900         DELETE 負傷データＦ
009910         INVALID KEY
009920             MOVE NC"負傷" TO ファイル名Ｗ
009930             PERFORM エラー表示Ｄ
009940         END-DELETE
009950     END-READ.
009960*================================================================*
009970 負傷データＦ全削除 SECTION.
009980*
009990     MOVE ZERO                   TO 負－施術和暦年月.
010000     MOVE 連患者削除－患者コード TO 負－患者コード.
010010*
010020     START 負傷データＦ KEY IS >= 負－患者コード
010030                                  負－施術和暦年月
010040     END-START.
010050     IF 状態キー = "00"
010060         MOVE SPACE TO 終了フラグ
010070         PERFORM 負傷データＦ読込
010080         PERFORM UNTIL ( 連患者削除－患者コード NOT = 負－患者コード ) OR
010090                       ( 終了フラグ = "YES" )
010100             DELETE 負傷データＦ
010110             INVALID KEY
010120                 MOVE NC"負傷" TO ファイル名Ｗ
010130                 PERFORM エラー表示Ｄ
010140             END-DELETE
010150             PERFORM 負傷データＦ読込
010160         END-PERFORM
010170     END-IF.
010180*================================================================*
010190 負傷データＦ変更 SECTION.
010200*
010210     MOVE ZERO                   TO 負－施術和暦年月.
010220     MOVE 連患者削除－患者コード TO 負－患者コード.
010230*
010240     START 負傷データＦ KEY IS >= 負－患者コード
010250                                  負－施術和暦年月
010260     END-START.
010270     IF 状態キー = "00"
010280         MOVE SPACE TO 終了フラグ
010290         PERFORM 負傷データＦ読込
010300         PERFORM UNTIL ( 連患者削除－患者コード NOT = 負－患者コード ) OR
010310                       ( 終了フラグ = "YES" )
010320             MOVE 連患者削除－変更患者コード TO 負－患者コード
010330             PERFORM VARYING カウンタ FROM 1 BY 1
010340                           UNTIL ( カウンタ > 8 )
010350                 IF 負－負傷患者番号(カウンタ) = 連患者削除－患者番号
010360                     MOVE 連患者削除－変更患者番号 TO 負－負傷患者番号(カウンタ)
010370                 END-IF
010380             END-PERFORM
010390             WRITE 負－レコード
010400             INVALID KEY
010410                 MOVE NC"負傷" TO ファイル名Ｗ
010420                 PERFORM エラー表示
010430             END-WRITE
010440             MOVE 連患者削除－患者コード TO 負－患者コード
010450             DELETE 負傷データＦ
010460             INVALID KEY
010470                 MOVE NC"負傷" TO ファイル名Ｗ
010480                 PERFORM エラー表示Ｄ
010490             END-DELETE
010500             PERFORM 負傷データＦ読込
010510         END-PERFORM
010520     END-IF.
010530*
010540*================================================================*
010550 負傷データＦ枝番作成 SECTION.
010560*
010570     MOVE 連患者削除－施術和暦年月 TO 負－施術和暦年月.
010580     MOVE 連患者削除－患者コード   TO 負－患者コード.
010590*
010600     READ 負傷データＦ
010610     INVALID KEY
010620         MOVE NC"負傷" TO ファイル名Ｗ
010630         PERFORM エラー表示Ｒ
010640     NOT INVALID KEY
010650*      /*枝番有りレコードの作成
010660         MOVE 連患者削除－変更枝番 TO 負－枝番
010670         WRITE 負－レコード
010680         INVALID KEY
010690             MOVE NC"負傷" TO ファイル名Ｗ
010700             PERFORM エラー表示
010710         END-WRITE
010720*      /*枝番作成時に元番号の負傷レコードの開始診療日手動区分を１にする。
010730         MOVE 連患者削除－枝番 TO 負－枝番
010740         MOVE 1     TO 負－開始診療日手動区分
010750         REWRITE 負－レコード
010760         INVALID KEY
010770             MOVE NC"負傷" TO ファイル名Ｗ
010780             PERFORM エラー表示
010790         END-REWRITE
010800     END-READ.
010810*
010820*================================================================*
010830 負傷データＦ読込 SECTION.
010840*
010850     READ 負傷データＦ NEXT
010860     AT END
010870         MOVE "YES" TO 終了フラグ
010880     END-READ.
010890*================================================================*
010900 受付データＦ当月削除 SECTION.
010910*
010920     MOVE 連患者削除－施術和暦年月 TO 受付－施術和暦年月.
010930     MOVE ZERO                     TO 受付－施術日.
010940     MOVE 連患者削除－患者コード   TO 受付－患者コード.
010950     MOVE ZERO                     TO 受付－受付時間.
010960*
010970     START 受付データＦ KEY IS >= 受付－患者コード
010980                                  受付－施術和暦年月日
010990                                  受付－受付時間
011000     END-START.
011010*
011020     IF 状態キー = "00"
011030         MOVE SPACE TO 終了フラグ
011040         PERFORM 受付データＦ読込
011050         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
011060                       ( 連患者削除－患者コード   NOT = 受付－患者コード ) OR
011070                       ( 連患者削除－施術和暦年月 NOT = 受付－施術和暦年月 )
011080             DELETE 受付データＦ
011090             INVALID KEY
011100                 MOVE NC"受付" TO ファイル名Ｗ
011110                 PERFORM エラー表示Ｄ
011120             END-DELETE
011130             PERFORM 受付データＦ読込
011140         END-PERFORM
011150     END-IF.
011160*================================================================*
011170 受付データＦ全削除 SECTION.
011180*
011190     MOVE ZERO                   TO  受付－施術和暦年月日.
011200     MOVE 連患者削除－患者コード TO  受付－患者コード.
011210     MOVE ZERO                   TO  受付－受付時間.
011220*
011230     START 受付データＦ KEY IS >= 受付－患者コード
011240                                  受付－施術和暦年月日
011250                                  受付－受付時間
011260     END-START.
011270*
011280     IF 状態キー = "00"
011290         MOVE SPACE TO 終了フラグ
011300         PERFORM 受付データＦ読込
011310         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
011320                       ( 連患者削除－患者コード   NOT = 受付－患者コード )
011330             DELETE 受付データＦ
011340             INVALID KEY
011350                 MOVE NC"受付" TO ファイル名Ｗ
011360                 PERFORM エラー表示Ｄ
011370             END-DELETE
011380             PERFORM 受付データＦ読込
011390         END-PERFORM
011400     END-IF.
011410*================================================================*
011420 受付データＦ変更 SECTION.
011430*
011440     MOVE ZERO                   TO  受付－施術和暦年月日.
011450     MOVE 連患者削除－患者コード TO  受付－患者コード.
011460     MOVE ZERO                   TO  受付－受付時間.
011470*
011480     START 受付データＦ KEY IS >= 受付－患者コード
011490                                  受付－施術和暦年月日
011500                                  受付－受付時間
011510     END-START.
011520*
011530     IF 状態キー = "00"
011540         MOVE SPACE TO 終了フラグ
011550         PERFORM 受付データＦ読込
011560         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
011570                       ( 連患者削除－患者コード   NOT = 受付－患者コード )
011580             MOVE 連患者削除－変更患者コード TO 受付－患者コード
011590             COMPUTE 受付－連番  = 受付－連番 + 0.1
011600             WRITE 受付－レコード
011610             INVALID KEY
011620                 MOVE NC"受付" TO ファイル名Ｗ
011630                 PERFORM エラー表示
011640             END-WRITE
011650             MOVE 連患者削除－患者コード TO 受付－患者コード
011660             DELETE 受付データＦ
011670             INVALID KEY
011680                 MOVE NC"受付" TO ファイル名Ｗ
011690                 PERFORM エラー表示Ｄ
011700             END-DELETE
011710*
011720             PERFORM 受付データＦ読込
011730         END-PERFORM
011740     END-IF.
011750*
011760*================================================================*
011770 受付データＦ枝番作成 SECTION.
011780*
011790     MOVE 連患者削除－施術和暦年月 TO  受付－施術和暦年月.
011800     MOVE 1                        TO  受付－施術日.
011810     MOVE 連患者削除－患者コード   TO  受付－患者コード.
011820     MOVE ZERO                     TO  受付－受付時間.
011830*
011840     START 受付データＦ KEY IS >= 受付－患者コード
011850                                  受付－施術和暦年月日
011860                                  受付－受付時間
011870     END-START.
011880*
011890     IF 状態キー = "00"
011900         MOVE SPACE TO 終了フラグ
011910         PERFORM 受付データＦ読込
011920*/変更日以降はコンバートしない
011930         PERFORM UNTIL ( 終了フラグ         NOT = SPACE ) OR 
011940                       ( 受付－患者コード   NOT = 連患者削除－患者コード ) OR
011950                       ( 受付－施術和暦年月 NOT = 連患者削除－施術和暦年月 ) OR
011960                       ( 受付－施術日          >= 連患者枝番－変更日    )
011970             MOVE 連患者削除－変更枝番   TO 受付－枝番
011980             COMPUTE 受付－連番 = 受付－連番 + 0.1
011990             WRITE 受付－レコード
012000             INVALID KEY
012010                 MOVE NC"受付" TO ファイル名Ｗ
012020                 PERFORM エラー表示
012030             END-WRITE
012040*
012050             MOVE 連患者削除－枝番  TO 受付－枝番
012060             DELETE 受付データＦ
012070             INVALID KEY
012080                 MOVE NC"受付" TO ファイル名Ｗ
012090                 PERFORM エラー表示Ｄ
012100             END-DELETE
012110*
012120             PERFORM 受付データＦ読込
012130         END-PERFORM
012140     END-IF.
012150*
012160*================================================================*
012170 受付データＦ読込 SECTION.
012180*
012190     READ 受付データＦ NEXT
012200     AT END
012210         MOVE "YES" TO 終了フラグ
012220     END-READ.
012230*================================================================*
012240 施術記録当月削除 SECTION.
012250*
012260     MOVE 連患者削除－施術和暦年月 TO  施記－施術和暦年月.
012270     MOVE ZERO                     TO  施記－施術日.  
012280     MOVE 連患者削除－患者コード   TO  施記－患者コード.
012290*
012300     START 施術記録Ｆ KEY IS >= 施記－患者コード
012310                                施記－施術和暦年月日
012320     END-START.
012330*
012340     IF 状態キー = "00"
012350         MOVE SPACE TO 終了フラグ
012360         PERFORM 施術記録Ｆ読込
012370         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
012380                       ( 連患者削除－患者コード   NOT = 施記－患者コード ) OR
012390                       ( 連患者削除－施術和暦年月 NOT = 施記－施術和暦年月 )
012400             DELETE 施術記録Ｆ
012410             INVALID KEY
012420                 MOVE NC"施記" TO ファイル名Ｗ
012430                 PERFORM エラー表示Ｄ
012440             END-DELETE
012450             PERFORM 施術記録Ｆ読込
012460         END-PERFORM
012470     END-IF.
012480*================================================================*
012490 施術記録全削除 SECTION.
012500*
012510     MOVE ZERO                   TO  施記－施術和暦年月日.
012520     MOVE 連患者削除－患者コード TO  施記－患者コード.
012530*
012540     START 施術記録Ｆ KEY IS >= 施記－患者コード
012550                                施記－施術和暦年月日
012560     END-START.
012570*
012580     IF 状態キー = "00"
012590         MOVE SPACE TO 終了フラグ
012600         PERFORM 施術記録Ｆ読込
012610         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
012620                       ( 連患者削除－患者コード   NOT = 施記－患者コード )
012630             DELETE 施術記録Ｆ
012640             INVALID KEY
012650                 MOVE NC"施記" TO ファイル名Ｗ
012660                 PERFORM エラー表示Ｄ
012670             END-DELETE
012680             PERFORM 施術記録Ｆ読込
012690         END-PERFORM
012700     END-IF.
012710*================================================================*
012720 施術記録変更 SECTION.
012730*
012740     MOVE ZERO                   TO  施記－施術和暦年月日.
012750     MOVE 連患者削除－患者コード TO  施記－患者コード.
012760*
012770     START 施術記録Ｆ KEY IS >= 施記－患者コード
012780                                施記－施術和暦年月日
012790     END-START.
012800*
012810     IF 状態キー = "00"
012820         MOVE SPACE TO 終了フラグ
012830         PERFORM 施術記録Ｆ読込
012840         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
012850                       ( 連患者削除－患者コード   NOT = 施記－患者コード )
012860             MOVE 連患者削除－変更患者コード TO 施記－患者コード
012870             WRITE 施記－レコード
012880             INVALID KEY
012890                 MOVE NC"施記" TO ファイル名Ｗ
012900                 PERFORM エラー表示
012910             END-WRITE
012920             MOVE 連患者削除－患者コード TO 施記－患者コード
012930             DELETE 施術記録Ｆ
012940             INVALID KEY
012950                 MOVE NC"施記" TO ファイル名Ｗ
012960                 PERFORM エラー表示Ｄ
012970             END-DELETE
012980*
012990             PERFORM 施術記録Ｆ読込
013000         END-PERFORM
013010     END-IF.
013020*
013030*================================================================*
013040 施術記録枝番作成 SECTION.
013050*
013060     MOVE 連患者削除－施術和暦年月 TO  施記－施術和暦年月.
013070     MOVE 1                        TO  施記－施術日.
013080     MOVE 連患者削除－患者コード   TO  施記－患者コード.
013090*
013100     START 施術記録Ｆ KEY IS >= 施記－患者コード
013110                                施記－施術和暦年月日
013120     END-START.
013130*
013140     IF 状態キー = "00"
013150         MOVE SPACE TO 終了フラグ
013160         PERFORM 施術記録Ｆ読込
013170*/変更日以降はコンバートしない
013180         PERFORM UNTIL ( 終了フラグ         NOT = SPACE ) OR 
013190                       ( 施記－患者コード   NOT = 連患者削除－患者コード ) OR
013200                       ( 施記－施術和暦年月 NOT = 連患者削除－施術和暦年月 ) OR
013210                       ( 施記－施術日          >= 連患者枝番－変更日    )
013220             MOVE 連患者削除－変更枝番   TO 施記－枝番
013230             WRITE 施記－レコード
013240             INVALID KEY
013250                 MOVE NC"施記" TO ファイル名Ｗ
013260                 PERFORM エラー表示
013270             END-WRITE
013280*
013290             MOVE 連患者削除－枝番  TO 施記－枝番
013300             DELETE 施術記録Ｆ
013310             INVALID KEY
013320                 MOVE NC"施記" TO ファイル名Ｗ
013330                 PERFORM エラー表示Ｄ
013340             END-DELETE
013350*
013360             PERFORM 施術記録Ｆ読込
013370*
013380         END-PERFORM
013390     END-IF.
013400*
013410*================================================================*
013420 施術記録Ｆ読込 SECTION.
013430*
013440     READ 施術記録Ｆ NEXT
013450     AT END
013460         MOVE "YES" TO 終了フラグ
013470     END-READ.
013480*================================================================*
013490 会計データＦ当月削除 SECTION.
013500*
013510     MOVE 連患者削除－施術和暦年月 TO  会－施術和暦年月.
013520     MOVE ZERO                     TO  会－施術日.
013530     MOVE 連患者削除－患者コード   TO  会－患者コード.
013540*
013550     START 会計データＦ KEY IS >= 会－患者コード
013560                                  会－施術和暦年月日
013570     END-START.
013580*
013590     IF 状態キー = "00"
013600         MOVE SPACE TO 終了フラグ
013610         PERFORM 会計データＦ読込
013620         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
013630                       ( 連患者削除－患者コード   NOT = 会－患者コード ) OR
013640                       ( 連患者削除－施術和暦年月 NOT = 会－施術和暦年月 )
013650             DELETE 会計データＦ
013660             INVALID KEY
013670                 MOVE NC"会計" TO ファイル名Ｗ
013680                 PERFORM エラー表示Ｄ
013690             END-DELETE
013700             PERFORM 会計データＦ読込
013710         END-PERFORM
013720     END-IF.
013730*
013740*================================================================*
013750 会計データＦ全削除 SECTION.
013760*
013770     MOVE ZERO                   TO  会－施術和暦年月日.
013780     MOVE 連患者削除－患者コード TO  会－患者コード.
013790*
013800     START 会計データＦ KEY IS >= 会－患者コード
013810                                  会－施術和暦年月日
013820     END-START.
013830*
013840     IF 状態キー = "00"
013850         MOVE SPACE TO 終了フラグ
013860         PERFORM 会計データＦ読込
013870         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
013880                       ( 連患者削除－患者コード   NOT = 会－患者コード )
013890             DELETE 会計データＦ
013900             INVALID KEY
013910                 MOVE NC"会計" TO ファイル名Ｗ
013920                 PERFORM エラー表示Ｄ
013930             END-DELETE
013940             PERFORM 会計データＦ読込
013950         END-PERFORM
013960     END-IF.
013970*
013980*================================================================*
013990 会計データＦ変更 SECTION.
014000*
014010     MOVE ZERO                   TO  会－施術和暦年月日.
014020     MOVE 連患者削除－患者コード TO  会－患者コード.
014030*
014040     START 会計データＦ KEY IS >= 会－患者コード
014050                                  会－施術和暦年月日
014060     END-START.
014070*
014080     IF 状態キー = "00"
014090         MOVE SPACE TO 終了フラグ
014100         PERFORM 会計データＦ読込
014110         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
014120                       ( 連患者削除－患者コード   NOT = 会－患者コード )
014130             MOVE  連患者削除－変更患者コード TO 会－患者コード 
014140             WRITE 会－レコード
014150             INVALID KEY
014160                 MOVE NC"会計" TO ファイル名Ｗ
014170                 PERFORM エラー表示
014180             END-WRITE
014190             MOVE  連患者削除－患者コード TO 会－患者コード 
014200             DELETE 会計データＦ
014210             INVALID KEY
014220                 MOVE NC"会計" TO ファイル名Ｗ
014230                 PERFORM エラー表示Ｄ
014240             END-DELETE
014250             PERFORM 会計データＦ読込
014260         END-PERFORM
014270     END-IF.
014280*
014290*================================================================*
014300 会計データＦ枝番作成 SECTION.
014310*
014320     MOVE 連患者削除－施術和暦年月 TO  会－施術和暦年月.
014330     MOVE 1                        TO  会－施術日.
014340     MOVE 連患者削除－患者コード   TO  会－患者コード.
014350*
014360     START 会計データＦ KEY IS >= 会－患者コード
014370                                  会－施術和暦年月日
014380     END-START.
014390*
014400     IF 状態キー = "00"
014410         MOVE SPACE TO 終了フラグ
014420         PERFORM 会計データＦ読込
014430*/変更日以降はコンバートしない
014440         PERFORM UNTIL ( 終了フラグ       NOT = SPACE ) OR 
014450                       ( 会－患者コード   NOT = 連患者削除－患者コード ) OR
014460                       ( 会－施術和暦年月 NOT = 連患者削除－施術和暦年月 ) OR
014470                       ( 会－施術日          >= 連患者枝番－変更日    )
014480             MOVE 連患者削除－変更枝番   TO 会－枝番
014490             WRITE 会－レコード
014500             INVALID KEY
014510                 MOVE NC"会" TO ファイル名Ｗ
014520                 PERFORM エラー表示
014530             END-WRITE
014540*
014550             MOVE 連患者削除－枝番  TO 会－枝番
014560             DELETE 会計データＦ
014570             INVALID KEY
014580                 MOVE NC"会" TO ファイル名Ｗ
014590                 PERFORM エラー表示Ｄ
014600             END-DELETE
014610*
014620             PERFORM 会計データＦ読込
014630         END-PERFORM
014640     END-IF.
014650*
014660*================================================================*
014670 会計データＦ読込 SECTION.
014680*
014690     READ 会計データＦ NEXT
014700     AT END
014710         MOVE "YES" TO 終了フラグ
014720     END-READ.
014730*================================================================*
014740 長期継続者Ｆ当月削除 SECTION.
014750*
014760     MOVE 連患者削除－施術和暦年月 TO  長継－施術和暦年月.
014770     MOVE 連患者削除－患者コード   TO  長継－患者コード.
014780*
014790     START 長期継続者Ｆ KEY IS >= 長継－患者コード
014800                                  長継－施術和暦年月
014810     END-START.
014820*
014830     IF 状態キー = "00"
014840         MOVE SPACE TO 終了フラグ
014850         PERFORM 長期継続者Ｆ読込
014860         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
014870                       ( 連患者削除－患者コード   NOT = 長継－患者コード ) OR
014880                       ( 連患者削除－施術和暦年月 NOT = 長継－施術和暦年月 )
014890             DELETE 長期継続者Ｆ
014900             INVALID KEY
014910                 MOVE NC"長継" TO ファイル名Ｗ
014920                 PERFORM エラー表示Ｄ
014930             END-DELETE
014940             PERFORM 長期継続者Ｆ読込
014950         END-PERFORM
014960     END-IF.
014970*================================================================*
014980 長期継続者Ｆ全削除 SECTION.
014990*
015000     MOVE ZERO                   TO  長継－施術和暦年月.
015010     MOVE 連患者削除－患者コード TO  長継－患者コード.
015020*
015030     START 長期継続者Ｆ KEY IS >= 長継－患者コード
015040                                  長継－施術和暦年月
015050     END-START.
015060*
015070     IF 状態キー = "00"
015080         MOVE SPACE TO 終了フラグ
015090         PERFORM 長期継続者Ｆ読込
015100         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
015110                       ( 連患者削除－患者コード   NOT = 長継－患者コード )
015120             DELETE 長期継続者Ｆ
015130             INVALID KEY
015140                 MOVE NC"長継" TO ファイル名Ｗ
015150                 PERFORM エラー表示Ｄ
015160             END-DELETE
015170             PERFORM 長期継続者Ｆ読込
015180         END-PERFORM
015190     END-IF.
015200*================================================================*
015210 長期継続者Ｆ変更 SECTION.
015220*
015230     MOVE ZERO                   TO  長継－施術和暦年月.
015240     MOVE 連患者削除－患者コード TO  長継－患者コード.
015250*
015260     START 長期継続者Ｆ KEY IS >= 長継－患者コード
015270                                  長継－施術和暦年月
015280     END-START.
015290*
015300     IF 状態キー = "00"
015310         MOVE SPACE TO 終了フラグ
015320         PERFORM 長期継続者Ｆ読込
015330         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
015340                       ( 連患者削除－患者コード   NOT = 長継－患者コード )
015350             MOVE  連患者削除－変更患者コード TO 長継－患者コード 
015360             WRITE 長継－レコード
015370             INVALID KEY
015380                 MOVE NC"長継" TO ファイル名Ｗ
015390                 PERFORM エラー表示
015400             END-WRITE
015410             MOVE  連患者削除－患者コード TO 長継－患者コード 
015420             DELETE 長期継続者Ｆ
015430             INVALID KEY
015440                 MOVE NC"長継" TO ファイル名Ｗ
015450                 PERFORM エラー表示Ｄ
015460             END-DELETE
015470             PERFORM 長期継続者Ｆ読込
015480         END-PERFORM
015490     END-IF.
015500*
015510*================================================================*
015520 長期継続者Ｆ枝番作成 SECTION.
015530*
015540     MOVE 連患者削除－施術和暦年月 TO  長継－施術和暦年月.
015550     MOVE 連患者削除－患者コード   TO  長継－患者コード.
015560*
015570     READ 長期継続者Ｆ
015580     NOT INVALID KEY
015590*      /*枝番有りレコードの作成
015600         MOVE 連患者削除－変更枝番 TO 長継－枝番
015610         WRITE 長継－レコード
015620         INVALID KEY
015630             MOVE NC"長継" TO ファイル名Ｗ
015640             PERFORM エラー表示
015650         END-WRITE
015660     END-READ.
015670*
015680*================================================================*
015690 長期継続者Ｆ読込 SECTION.
015700*
015710     READ 長期継続者Ｆ NEXT
015720     AT END
015730         MOVE "YES" TO 終了フラグ
015740     END-READ.
015750*================================================================*
015760 バーコード管理Ｆ当月削除 SECTION.
015770*
015780     MOVE 連患者削除－患者コード   TO  バ管－患者コード.
015790     MOVE 連患者削除－施術和暦年月 TO  バ管－施術和暦年月.
015800     MOVE ZERO                     TO  バ管－整理番号.
015810     START バーコード管理Ｆ KEY IS >= バ管－患者コード
015820                                      バ管－施術和暦年月
015830                                      バ管－整理番号
015840     END-START.
015850     IF 状態キー = "00"
015860         MOVE SPACE TO 終了フラグ
015870         PERFORM バーコード管理Ｆ読込
015880         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
015890                       ( 連患者削除－患者コード   NOT = バ管－患者コード ) OR
015900                       ( 連患者削除－施術和暦年月 NOT = バ管－施術和暦年月 )
015910             DELETE バーコード管理Ｆ
015920             INVALID KEY
015930                 MOVE NC"バ管" TO ファイル名Ｗ
015940                 PERFORM エラー表示Ｄ
015950             END-DELETE
015960             PERFORM バーコード管理Ｆ読込
015970         END-PERFORM
015980     END-IF.
015990*================================================================*
016000 バーコード管理Ｆ全削除 SECTION.
016010*
016020     MOVE 連患者削除－患者コード TO  バ管－患者コード.
016030     MOVE ZERO                   TO  バ管－施術和暦年月.
016040     MOVE ZERO                   TO  バ管－整理番号.
016050     START バーコード管理Ｆ KEY IS >= バ管－患者コード
016060                                      バ管－施術和暦年月
016070                                      バ管－整理番号
016080     END-START.
016090     IF 状態キー = "00"
016100         MOVE SPACE TO 終了フラグ
016110         PERFORM バーコード管理Ｆ読込
016120         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
016130                       ( 連患者削除－患者コード   NOT = バ管－患者コード )
016140             DELETE バーコード管理Ｆ
016150             INVALID KEY
016160                 MOVE NC"バ管" TO ファイル名Ｗ
016170                 PERFORM エラー表示Ｄ
016180             END-DELETE
016190             PERFORM バーコード管理Ｆ読込
016200         END-PERFORM
016210     END-IF.
016220*================================================================*
016230 バーコード管理Ｆ変更 SECTION.
016240*
016250     MOVE ZERO                   TO  バ管－施術和暦年月
016260     MOVE ZERO                   TO  バ管－整理番号.
016270     MOVE 連患者削除－患者コード TO  バ管－患者コード.
016280*
016290     START バーコード管理Ｆ KEY IS >= バ管－患者コード
016300                                      バ管－施術和暦年月
016310                                      バ管－整理番号
016320     END-START.
016330*
016340     IF 状態キー = "00"
016350         MOVE SPACE TO 終了フラグ
016360         PERFORM バーコード管理Ｆ読込
016370         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
016380                       ( 連患者削除－患者コード   NOT = バ管－患者コード )
016390             MOVE  連患者削除－変更患者コード TO バ管－患者コード
016400*        / REWITE /
016410             REWRITE バ管－レコード
016420             INVALID KEY
016430                 MOVE NC"バ管" TO ファイル名Ｗ
016440                 PERFORM エラー表示
016450             END-REWRITE
016460             PERFORM バーコード管理Ｆ読込
016470         END-PERFORM
016480     END-IF.
016490*
016500*================================================================*
016510 バーコード管理Ｆ枝番作成 SECTION.
016520*
016530     MOVE 連患者削除－患者コード   TO  バ管－患者コード.
016540     MOVE 連患者削除－施術和暦年月 TO  バ管－施術和暦年月.
016550     MOVE ZERO                     TO  バ管－整理番号.
016560     START バーコード管理Ｆ KEY IS >= バ管－患者コード
016570                                      バ管－施術和暦年月
016580                                      バ管－整理番号
016590     END-START.
016600     IF 状態キー = "00"
016610         MOVE SPACE TO 終了フラグ
016620         PERFORM バーコード管理Ｆ読込
016630         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
016640                       ( 連患者削除－患者コード   NOT = バ管－患者コード ) OR
016650                       ( 連患者削除－施術和暦年月 NOT = バ管－施術和暦年月 )
016660* 整理番号のダブリを防ぐため、先にレコード削除してから作成する。
016670*------------   / レコードの退避 / -------------------------------*
016680             INITIALIZE 退避バ－レコード
016690             MOVE バ管－レコード  TO 退避バ－レコード
016700*------------   / レコードの削除 / -------------------------------*
016710             DELETE バーコード管理Ｆ
016720             INVALID KEY
016730                 MOVE NC"バ管" TO ファイル名Ｗ
016740                 PERFORM エラー表示Ｄ
016750             END-DELETE
016760*
016770*------------   / 新レコードの作成 / -------------------------------*
016780             MOVE SPACE TO バ管－レコード
016790             INITIALIZE バ管－レコード
016800             MOVE 退避バ－レコード  TO バ管－レコード
016810             MOVE  連患者削除－変更患者コード TO バ管－患者コード
016820             WRITE バ管－レコード
016830             INVALID KEY
016840                 MOVE NC"バ管" TO ファイル名Ｗ
016850                 PERFORM エラー表示
016860             END-WRITE
016870             MOVE  連患者削除－患者コード TO バ管－患者コード
016880             PERFORM バーコード管理Ｆ読込
016890         END-PERFORM
016900     END-IF.
016910*
016920*================================================================*
016930 バーコード管理Ｆ読込 SECTION.
016940*
016950     READ バーコード管理Ｆ NEXT
016960     AT END
016970         MOVE "YES" TO 終了フラグ
016980     END-READ.
016990*================================================================*
017000 レセプトＦ当月削除 SECTION.
017010*
017020     MOVE 連患者削除－患者コード   TO レセ－患者コード.
017030     MOVE 連患者削除－施術和暦年月 TO レセ－施術和暦年月.
017040     MOVE ZERO                     TO レセ－レセ種別.
017050*
017060     START レセプトＦ KEY IS >= レセ－患者コード
017070                                レセ－施術和暦年月
017080                                レセ－レセ種別
017090     END-START.
017100*
017110     IF 状態キー = "00"
017120         MOVE SPACE TO 終了フラグ
017130         PERFORM レセプトＦ読込
017140         PERFORM UNTIL (レセ－患者コード   NOT = 連患者削除－患者コード)   OR
017150                       (レセ－施術和暦年月 NOT = 連患者削除－施術和暦年月) OR
017160                       (終了フラグ             = "YES")
017170             DELETE レセプトＦ
017180             INVALID KEY
017190                 MOVE NC"レセプト" TO ファイル名Ｗ
017200                 PERFORM エラー表示Ｄ
017210             END-DELETE
017220             PERFORM レセプトＦ読込
017230         END-PERFORM
017240     END-IF.
017250*
017260*================================================================*
017270 レセプトＦ全削除 SECTION.
017280*
017290     MOVE 連患者削除－患者コード   TO レセ－患者コード.
017300     MOVE ZERO                     TO レセ－施術和暦年月.
017310     MOVE ZERO                     TO レセ－レセ種別.
017320*
017330     START レセプトＦ KEY IS >= レセ－患者コード
017340                                レセ－施術和暦年月
017350                                レセ－レセ種別
017360     END-START.
017370*
017380     IF 状態キー = "00"
017390         MOVE SPACE TO 終了フラグ
017400         PERFORM レセプトＦ読込
017410         PERFORM UNTIL (レセ－患者コード   NOT = 連患者削除－患者コード) OR
017420                       (終了フラグ             = "YES")
017430             DELETE レセプトＦ
017440             INVALID KEY
017450                 MOVE NC"レセプト" TO ファイル名Ｗ
017460                 PERFORM エラー表示Ｄ
017470             END-DELETE
017480             PERFORM レセプトＦ読込
017490         END-PERFORM
017500     END-IF.
017510*
017520*================================================================*
017530 レセプトＦ変更 SECTION.
017540*
017550     MOVE 連患者削除－患者コード   TO レセ－患者コード.
017560     MOVE ZERO                     TO レセ－施術和暦年月.
017570     MOVE ZERO                     TO レセ－レセ種別.
017580*
017590     START レセプトＦ KEY IS >= レセ－患者コード
017600                                レセ－施術和暦年月
017610                                レセ－レセ種別
017620     END-START.
017630*
017640     IF 状態キー = "00"
017650         MOVE SPACE TO 終了フラグ
017660         PERFORM レセプトＦ読込
017670         PERFORM UNTIL (レセ－患者コード   NOT = 連患者削除－患者コード) OR
017680                       (終了フラグ             = "YES")
017690             MOVE  連患者削除－変更患者コード TO レセ－患者コード
017700             WRITE レセ－レコード
017710             INVALID KEY
017720                 MOVE NC"レセ" TO ファイル名Ｗ
017730                 PERFORM エラー表示
017740             END-WRITE
017750             MOVE  連患者削除－患者コード TO レセ－患者コード
017760             DELETE レセプトＦ
017770             INVALID KEY
017780                 MOVE NC"レセプト" TO ファイル名Ｗ
017790                 PERFORM エラー表示Ｄ
017800             END-DELETE
017810             PERFORM レセプトＦ読込
017820         END-PERFORM
017830     END-IF.
017840*
017850*================================================================*
017860 レセプトＦ枝番作成 SECTION.
017870*
017871     MOVE SPACE TO レセ存在フラグ.
017872*
017880     MOVE 連患者削除－患者コード   TO レセ－患者コード.
017890     MOVE 連患者削除－施術和暦年月 TO レセ－施術和暦年月.
017900     MOVE ZERO                     TO レセ－レセ種別.
017910*
017912     START レセプトＦ KEY IS >= レセ－患者コード
017913                                レセ－施術和暦年月
017914                                レセ－レセ種別
017915     END-START.
017916*
017917     IF 状態キー = "00"
017918         MOVE SPACE TO 終了フラグ
017919         PERFORM レセプトＦ読込
017921         PERFORM UNTIL (レセ－患者コード   NOT = 連患者削除－患者コード)   OR
017922                       (レセ－施術和暦年月 NOT = 連患者削除－施術和暦年月) OR
017923                       (終了フラグ             = "YES")
017925                MOVE  連患者削除－変更患者コード TO レセ－患者コード
017926                WRITE レセ－レコード
017927                INVALID KEY
017928                    MOVE NC"レセ" TO ファイル名Ｗ
017929                    PERFORM エラー表示
017930                END-WRITE
017940                MOVE "YES" TO レセ存在フラグ
017941                PERFORM レセプトＦ読込
017942         END-PERFORM
017979     END-IF.
018093*
018094*================================================================*
018095 レセプトＦ読込 SECTION.
018100*
018110     READ レセプトＦ NEXT
018120     AT END
018130         MOVE "YES" TO 終了フラグ
018140     END-READ.
018141*================================================================*
018142*================================================================*
018143 レセプト労自Ｆ当月削除 SECTION.
018144*
018145*
018146     MOVE 連患者削除－施術和暦年月 TO レセ労自－施術和暦年月.
018147     MOVE 連患者削除－患者コード   TO レセ労自－患者コード.
018149*   / 4:労災,5:自賠責 /
018150     MOVE 4                        TO レセ労自－レセ種別.
018156*
018157     READ レセプト労自Ｆ
018158     NOT INVALID KEY
018159         DELETE レセプト労自Ｆ
018160         INVALID KEY
018161             MOVE NC"レセプト労自Ｆ" TO ファイル名Ｗ
018162             PERFORM エラー表示Ｄ
018163         END-DELETE
018164     END-READ.
018165***
018167     MOVE 連患者削除－施術和暦年月 TO レセ労自－施術和暦年月.
018168     MOVE 連患者削除－患者コード   TO レセ労自－患者コード.
018169*   / 4:労災,5:自賠責 /
018170     MOVE 5                        TO レセ労自－レセ種別.
018171*
018172     READ レセプト労自Ｆ
018173     NOT INVALID KEY
018174         DELETE レセプト労自Ｆ
018175         INVALID KEY
018176             MOVE NC"レセプト労自Ｆ" TO ファイル名Ｗ
018177             PERFORM エラー表示Ｄ
018178         END-DELETE
018179     END-READ.
018190*
018191*================================================================*
018192 レセプト労自Ｆ全削除 SECTION.
018193*
018194     MOVE ZERO            TO レセ労自－施術和暦年月.
018195     MOVE ZERO            TO レセ労自－患者番号.
018196     MOVE SPACE           TO レセ労自－枝番.
018197     MOVE ZERO            TO レセ労自－レセ種別.
018198*
018199     START レセプト労自Ｆ KEY IS >= レセ労自－施術和暦年月
018200                                    レセ労自－患者コード
018201                                    レセ労自－レセ種別
018202     END-START.
018206*
018207     IF 状態キー = "00"
018208         MOVE SPACE            TO 終了フラグ
018209         PERFORM レセプト労自Ｆ読込
018210         PERFORM UNTIL (終了フラグ   = "YES")
018211             IF (レセ労自－患者コード    = 連患者削除－患者コード)
018212                 DELETE レセプト労自Ｆ
018213                 INVALID KEY
018214                     MOVE NC"レセプト労自" TO ファイル名Ｗ
018215                     PERFORM エラー表示Ｄ
018216                 END-DELETE
018217             END-IF
018218             PERFORM レセプト労自Ｆ読込
018219         END-PERFORM
018220     END-IF.
018247*
018248*================================================================*
018249 レセプト労自Ｆ変更 SECTION.
018250*
018251     MOVE ZERO            TO レセ労自－施術和暦年月.
018252     MOVE ZERO            TO レセ労自－患者番号.
018253     MOVE SPACE           TO レセ労自－枝番.
018254     MOVE ZERO            TO レセ労自－レセ種別.
018255*
018256     START レセプト労自Ｆ KEY IS >= レセ労自－施術和暦年月
018257                                    レセ労自－患者コード
018258                                    レセ労自－レセ種別
018259     END-START.
018260*
018261     IF 状態キー = "00"
018262         MOVE SPACE            TO 終了フラグ
018263         PERFORM レセプト労自Ｆ読込
018264         PERFORM UNTIL (終了フラグ   = "YES")
018265             IF (レセ労自－患者コード    = 連患者削除－患者コード)
018267                 MOVE  連患者削除－変更患者コード TO レセ労自－患者コード
018268                 WRITE レセ労自－レコード
018269                 INVALID KEY
018270                     MOVE NC"レセ労自" TO ファイル名Ｗ
018271                     PERFORM エラー表示
018272                 END-WRITE
018273                 MOVE  連患者削除－患者コード TO レセ労自－患者コード
018284                 DELETE レセプト労自Ｆ
018285                 INVALID KEY
018286                     MOVE NC"レセプト労自" TO ファイル名Ｗ
018287                     PERFORM エラー表示Ｄ
018288                 END-DELETE
018289             END-IF
018290             PERFORM レセプト労自Ｆ読込
018291         END-PERFORM
018292     END-IF.
018308*
018309*================================================================*
018310 レセプト労自Ｆ枝番作成 SECTION.
018311*
018312     MOVE 連患者削除－施術和暦年月 TO レセ労自－施術和暦年月.
018313     MOVE 連患者削除－患者コード   TO レセ労自－患者コード.
018314*   / 4:労災,5:自賠責 /
018315     MOVE 4                        TO レセ労自－レセ種別.
018316*
018317     READ レセプト労自Ｆ
018318     NOT INVALID KEY
018319         MOVE  連患者削除－変更枝番 TO  レセ労自－枝番
018320         WRITE レセ労自－レコード
018321         INVALID KEY
018322             MOVE NC"レセ労自" TO ファイル名Ｗ
018323             PERFORM エラー表示
018324         END-WRITE
018333     END-READ.
018334***
018348     MOVE 連患者削除－施術和暦年月 TO レセ労自－施術和暦年月.
018349     MOVE 連患者削除－患者コード   TO レセ労自－患者コード.
018350*   / 4:労災,5:自賠責 /
018351     MOVE 5                        TO レセ労自－レセ種別.
018352*
018353     READ レセプト労自Ｆ
018354     NOT INVALID KEY
018355         MOVE  連患者削除－変更枝番 TO  レセ労自－枝番
018356         WRITE レセ労自－レコード
018357         INVALID KEY
018358             MOVE NC"レセ労自" TO ファイル名Ｗ
018359             PERFORM エラー表示
018360         END-WRITE
018361     END-READ.
018394*
018395*================================================================*
018396 レセプト労自Ｆ読込 SECTION.
018397*
018398     READ レセプト労自Ｆ NEXT
018399     AT END
018400         MOVE "YES" TO 終了フラグ
018401     END-READ.
018402*================================================================*
018403*================================================================*
018404
018405*                             RECORD      KEY          IS  証履－施術区分
018406*                                                          証履－用紙区分
018407*                                                          証履－施術和暦年月
018408*                                                          証履－患者コード
018409*                                                          証履－連番
018422
018423*================================================================*
018424 証明書履歴Ｆ全削除 SECTION.
018425*
018426     MOVE ZERO            TO 証履－施術区分.
018427     MOVE ZERO            TO 証履－用紙区分.
018428     MOVE ZERO            TO 証履－施術和暦年月.
018429     MOVE ZERO            TO 証履－患者番号.
018431     MOVE SPACE           TO 証履－枝番.
018432     MOVE ZERO            TO 証履－連番.
018433*
018434     START 証明書履歴Ｆ KEY IS >= 証履－施術区分
018435                                  証履－用紙区分
018436                                  証履－施術和暦年月
018437                                  証履－患者コード
018438                                  証履－連番
018441     END-START.
018442*
018443     IF 状態キー = "00"
018444         MOVE SPACE            TO 終了フラグ
018445         PERFORM 証明書履歴Ｆ読込
018446         PERFORM UNTIL (終了フラグ   = "YES")
018447             IF (証履－用紙区分  NOT = 999) AND
                      (証履－患者コード    = 連患者削除－患者コード)
018448                 DELETE 証明書履歴Ｆ
018449                 INVALID KEY
018450                     MOVE NC"証明書履歴Ｆ" TO ファイル名Ｗ
018451                     PERFORM エラー表示Ｄ
018452                 END-DELETE
018453             END-IF
018454             PERFORM 証明書履歴Ｆ読込
018455         END-PERFORM
018456     END-IF.
018457*
018458*================================================================*
018459 証明書履歴Ｆ変更 SECTION.
018460*
018472     MOVE ZERO            TO 証履－施術区分.
018473     MOVE ZERO            TO 証履－用紙区分.
018474     MOVE ZERO            TO 証履－施術和暦年月.
018475     MOVE ZERO            TO 証履－患者番号.
018476     MOVE SPACE           TO 証履－枝番.
018477     MOVE ZERO            TO 証履－連番.
018478*
018479     START 証明書履歴Ｆ KEY IS >= 証履－施術区分
018480                                  証履－用紙区分
018481                                  証履－施術和暦年月
018482                                  証履－患者コード
018483                                  証履－連番
018484     END-START.
018485*
018486     IF 状態キー = "00"
018487         MOVE SPACE            TO 終了フラグ
018488         PERFORM 証明書履歴Ｆ読込
018489         PERFORM UNTIL (終了フラグ   = "YES")
018490             IF (証履－用紙区分  NOT = 999) AND
                      (証履－患者コード    = 連患者削除－患者コード)
018492                 MOVE  連患者削除－変更患者コード TO 証履－患者コード
018493                 WRITE 証履－レコード
018494                 INVALID KEY
018495                     MOVE NC"証明書履歴Ｆ" TO ファイル名Ｗ
018496                     PERFORM エラー表示
018497                 END-WRITE
018498                 MOVE  連患者削除－患者コード TO 証履－患者コード
018499                 DELETE 証明書履歴Ｆ
018500                 INVALID KEY
018501                     MOVE NC"証明書履歴Ｆ" TO ファイル名Ｗ
018502                     PERFORM エラー表示Ｄ
018503                 END-DELETE
018515             END-IF
018516             PERFORM 証明書履歴Ｆ読込
018517         END-PERFORM
018518     END-IF.
018524*
018525*================================================================*
018526 証明書履歴Ｆ読込 SECTION.
018527*
018528     READ 証明書履歴Ｆ NEXT
018529     AT END
018530         MOVE "YES" TO 終了フラグ
018531     END-READ.
018532*
018533*================================================================*
018534*================================================================*
018535 カルテファイル当月削除 SECTION.
018536*
018537     MOVE ZERO         TO カ－会コード.
018538     MOVE ZERO         TO カ－区分.
018539     MOVE ZERO         TO カ－施術和暦年月.
018540     MOVE ZERO         TO カ－患者番号.
018541     MOVE SPACE        TO カ－枝番.
018542*
018543     START カルテファイル KEY IS >= カ－区分
018544                                    カ－患者コード
018545                                    カ－施術和暦年月
018546                                    カ－会コード
018547     END-START.
018548*
018549     IF 状態キー = "00"
018550         MOVE SPACE TO 終了フラグ
018551         PERFORM カルテファイル読込
018552         PERFORM UNTIL (終了フラグ = "YES")
018553             IF ( カ－施術和暦年月 = 連患者削除－施術和暦年月 ) AND
018554                ( カ－患者コード   = 連患者削除－患者コード )
018555                 DELETE カルテファイル
018556                 INVALID KEY
018557                     MOVE NC"カルテ" TO ファイル名Ｗ
018558                     PERFORM エラー表示Ｄ
018559                 END-DELETE
018560             END-IF
018561             PERFORM カルテファイル読込
018562         END-PERFORM
018563     END-IF.
018564*
018565*================================================================*
018566 カルテファイル全削除 SECTION.
018567*
018568     MOVE ZERO         TO カ－会コード.
018569     MOVE ZERO         TO カ－区分.
018570     MOVE ZERO         TO カ－施術和暦年月.
018571     MOVE ZERO         TO カ－患者番号.
018572     MOVE SPACE        TO カ－枝番.
018573*
018574     START カルテファイル KEY IS >= カ－区分
018575                                    カ－患者コード
018576                                    カ－施術和暦年月
018580                                    カ－会コード
018590     END-START.
018600*
018610     IF 状態キー = "00"
018620         MOVE SPACE TO 終了フラグ
018630         PERFORM カルテファイル読込
018640         PERFORM UNTIL (終了フラグ = "YES")
018650             IF ( カ－患者コード   = 連患者削除－患者コード )
018660                 DELETE カルテファイル
018670                 INVALID KEY
018680                     MOVE NC"カルテ" TO ファイル名Ｗ
018690                     PERFORM エラー表示Ｄ
018700                 END-DELETE
018710             END-IF
018720             PERFORM カルテファイル読込
018730         END-PERFORM
018740     END-IF.
018750*
018760*================================================================*
018770 カルテファイル変更 SECTION.
018780*
018790     MOVE ZERO         TO カ－会コード.
018800     MOVE ZERO         TO カ－区分.
018810     MOVE ZERO         TO カ－施術和暦年月.
018820     MOVE ZERO         TO カ－患者番号.
018830     MOVE SPACE        TO カ－枝番.
018840*
018850     START カルテファイル KEY IS >= カ－区分
018860                                    カ－患者コード
018870                                    カ－施術和暦年月
018880                                    カ－会コード
018890     END-START.
018900*
018910     IF 状態キー = "00"
018920         MOVE SPACE TO 終了フラグ
018930         PERFORM カルテファイル読込
018940         PERFORM UNTIL (終了フラグ = "YES")
018950             IF ( カ－患者コード   = 連患者削除－患者コード )
018960                 MOVE  連患者削除－変更患者コード TO カ－患者コード
018970                 WRITE カ－レコード
018980                 INVALID KEY
018990                     MOVE NC"カルテ" TO ファイル名Ｗ
019000                     PERFORM エラー表示
019010                 END-WRITE
019020                 MOVE  連患者削除－患者コード TO カ－患者コード
019030                 DELETE カルテファイル
019040                 INVALID KEY
019050                     MOVE NC"カルテ" TO ファイル名Ｗ
019060                     PERFORM エラー表示Ｄ
019070                 END-DELETE
019080             END-IF
019090             PERFORM カルテファイル読込
019100         END-PERFORM
019110     END-IF.
019120*
019130*================================================================*
019140 カルテファイル枝番作成 SECTION.
019150*
019160     MOVE ZERO         TO カ－会コード.
019170     MOVE ZERO         TO カ－区分.
019180     MOVE ZERO         TO カ－施術和暦年月.
019190     MOVE ZERO         TO カ－患者番号.
019200     MOVE SPACE        TO カ－枝番.
019210*
019220     START カルテファイル KEY IS >= カ－区分
019230                                    カ－患者コード
019240                                    カ－施術和暦年月
019250                                    カ－会コード
019260     END-START.
019270*
019280     IF 状態キー = "00"
019290         MOVE SPACE TO 終了フラグ
019300         PERFORM カルテファイル読込
019310         PERFORM UNTIL (終了フラグ = "YES")
019320             IF ( カ－施術和暦年月 = 連患者削除－施術和暦年月 ) AND
019330                ( カ－患者コード   = 連患者削除－患者コード )
019340                 MOVE  連患者削除－変更患者コード TO カ－患者コード
019350                 WRITE カ－レコード
019360                 INVALID KEY
019370                     MOVE NC"カルテ" TO ファイル名Ｗ
019380                     PERFORM エラー表示
019390                 END-WRITE
019400                 MOVE  連患者削除－患者コード TO カ－患者コード
019410             END-IF
019420             PERFORM カルテファイル読込
019430         END-PERFORM
019440     END-IF.
019450*
019460*================================================================*
019470 カルテファイル読込 SECTION.
019480*
019490     READ カルテファイル NEXT
019500     AT END
019510         MOVE "YES" TO 終了フラグ
019520     END-READ.
019530*================================================================*
019540 メモファイル当月削除 SECTION.
019550*
019560     MOVE 連患者削除－患者コード   TO メモ－患者コード.
019570     MOVE 連患者削除－施術和暦年月 TO メモ－施術和暦年月.
019580     MOVE ZERO                     TO メモ－施術日.
019590     MOVE ZERO                     TO メモ－制御区分.
019600*
019610     START メモファイル KEY IS >= メモ－患者コード
019620                                  メモ－施術和暦年月日
019630                                  メモ－制御区分
019640     END-START.
019650*
019660     IF 状態キー = "00"
019670         MOVE SPACE            TO 終了フラグ
019680         PERFORM メモファイル読込
019690         PERFORM UNTIL (メモ－患者コード   NOT = 連患者削除－患者コード)   OR
019700                       (メモ－施術和暦年月 NOT = 連患者削除－施術和暦年月) OR
019710                       (終了フラグ             = "YES")
019720             DELETE メモファイル
019730             INVALID KEY
019740                 MOVE NC"メモ" TO ファイル名Ｗ
019750                 PERFORM エラー表示Ｄ
019760             END-DELETE
019770             PERFORM メモファイル読込
019780         END-PERFORM
019790     END-IF.
019800*
019810*================================================================*
019820 メモファイル全削除 SECTION.
019830*
019840     MOVE 連患者削除－患者コード   TO メモ－患者コード.
019850     MOVE ZERO                     TO メモ－施術和暦年月日.
019860     MOVE ZERO                     TO メモ－制御区分.
019870*
019880     START メモファイル KEY IS >= メモ－患者コード
019890                                  メモ－施術和暦年月日
019900                                  メモ－制御区分
019910     END-START.
019920*
019930     IF 状態キー = "00"
019940         MOVE SPACE            TO 終了フラグ
019950         PERFORM メモファイル読込
019960         PERFORM UNTIL (メモ－患者コード   NOT = 連患者削除－患者コード)   OR
019970                       (終了フラグ             = "YES")
019980             DELETE メモファイル
019990             INVALID KEY
020000                 MOVE NC"メモ" TO ファイル名Ｗ
020010                 PERFORM エラー表示Ｄ
020020             END-DELETE
020030             PERFORM メモファイル読込
020040         END-PERFORM
020050     END-IF.
020060*
020070*================================================================*
020080 メモファイル変更 SECTION.
020090*
020100     MOVE 連患者削除－患者コード   TO メモ－患者コード.
020110     MOVE ZERO                     TO メモ－施術和暦年月日.
020120     MOVE ZERO                     TO メモ－制御区分.
020130*
020140     START メモファイル KEY IS >= メモ－患者コード
020150                                  メモ－施術和暦年月日
020160                                  メモ－制御区分
020170     END-START.
020180*
020190     IF 状態キー = "00"
020200         MOVE SPACE TO 終了フラグ
020210         PERFORM メモファイル読込
020220         PERFORM UNTIL (メモ－患者コード   NOT = 連患者削除－患者コード)   OR
020230                       (終了フラグ             = "YES")
020240             MOVE  連患者削除－変更患者コード TO メモ－患者コード
020250             WRITE メモ－レコード
020260             INVALID KEY
020270                 MOVE NC"メモ" TO ファイル名Ｗ
020280                 PERFORM エラー表示
020290             END-WRITE
020300             MOVE  連患者削除－患者コード TO メモ－患者コード
020310             DELETE メモファイル
020320             INVALID KEY
020330                 MOVE NC"メモ" TO ファイル名Ｗ
020340                 PERFORM エラー表示Ｄ
020350             END-DELETE
020360             PERFORM メモファイル読込
020370         END-PERFORM
020380     END-IF.
020390*
020400*================================================================*
020410 メモファイル枝番作成 SECTION.
020420*
020430     MOVE 連患者削除－患者コード   TO メモ－患者コード.
020440     MOVE 連患者削除－施術和暦年月 TO メモ－施術和暦年月.
020450     MOVE 1                        TO メモ－施術日
020460     MOVE ZERO                     TO メモ－制御区分.
020470*
020480     START メモファイル KEY IS >= メモ－患者コード
020490                                  メモ－施術和暦年月日
020500                                  メモ－制御区分
020510     END-START.
020520*
020530     IF 状態キー = "00"
020540         MOVE SPACE TO 終了フラグ
020550         PERFORM メモファイル読込
020560*/変更日以降はコンバートしない
020570         PERFORM UNTIL (メモ－患者コード   NOT = 連患者削除－患者コード)   OR
020580                       (メモ－施術和暦年月 NOT = 連患者削除－施術和暦年月) OR
020590                       (メモ－施術日          >= 連患者枝番－変更日    ) OR
020600                       (終了フラグ             = "YES")
020610             MOVE  連患者削除－変更患者コード TO メモ－患者コード
020620             WRITE メモ－レコード
020630             INVALID KEY
020640                 MOVE NC"メモ" TO ファイル名Ｗ
020650                 PERFORM エラー表示
020660             END-WRITE
020670             MOVE  連患者削除－患者コード TO メモ－患者コード
020680             DELETE メモファイル
020690             INVALID KEY
020700                 MOVE NC"メモ" TO ファイル名Ｗ
020710                 PERFORM エラー表示Ｄ
020720             END-DELETE
020730             PERFORM メモファイル読込
020740         END-PERFORM
020750     END-IF.
020760*
020770*================================================================*
020780 メモファイル読込 SECTION.
020790*
020800     READ メモファイル NEXT
020810     AT END
020820         MOVE "YES" TO 終了フラグ
020830     END-READ.
020831*================================================================*
020832*================================================================*
020833*================================================================*
020834 摘要ファイル当月削除 SECTION.
020836*
020840* 制御区分が最後の副キーが無いので繰返し処理
020842*
020843*
020844      PERFORM VARYING 繰返カウンタ FROM 1 BY 1  UNTIL ( 繰返カウンタ > 摘要区分数 ) 
020850*
020851        MOVE 繰返カウンタ             TO 摘要－制御区分
020852        MOVE 連患者削除－施術和暦年月 TO 摘要－施術和暦年月
020853        MOVE 連患者削除－患者コード   TO 摘要－患者コード
020854        READ 摘要ファイル
020855        NOT INVALID KEY
020856            DELETE 摘要ファイル
020857            INVALID KEY
020858                MOVE NC"摘要" TO ファイル名Ｗ
020859                PERFORM エラー表示Ｄ
020860            END-DELETE
020861        END-READ
020862*
020866      END-PERFORM.
020875*
020876*================================================================*
020877*================================================================*
020878 摘要ファイル全削除 SECTION.
020879*
020880* 制御区分が最後の副キーが無いので繰返し処理
020881*
020882*
020883      PERFORM VARYING 繰返カウンタ FROM 1 BY 1  UNTIL ( 繰返カウンタ > 摘要区分数 ) 
020884*
020885        MOVE 繰返カウンタ             TO 摘要－制御区分
020886        MOVE 連患者削除－患者コード   TO 摘要－患者コード
020887        MOVE ZERO                     TO 摘要－施術和暦年月
020889        START 摘要ファイル KEY IS >= 摘要－制御区分
020890                                     摘要－患者コード
020891                                     摘要－施術和暦年月
020892        END-START
020893        IF 状態キー = "00"
020894            MOVE SPACE TO 終了フラグ
020895            PERFORM 摘要ファイル読込
020896            PERFORM UNTIL ( 繰返カウンタ           NOT = 摘要－制御区分 ) OR
020897                          ( 連患者削除－患者コード NOT = 摘要－患者コード ) OR
020898                          ( 終了フラグ = "YES" )
020899                DELETE 摘要ファイル
020900                INVALID KEY
020901                    MOVE NC"摘要" TO ファイル名Ｗ
020902                    PERFORM エラー表示Ｄ
020903                END-DELETE
020904                PERFORM 摘要ファイル読込
020905            END-PERFORM
020906        END-IF
020909*
020910      END-PERFORM.
020911*
020912*================================================================*
020913 摘要ファイル変更 SECTION.
020914*
020915* 制御区分が最後の副キーが無いので繰返し処理
020916*
020917*
020918      PERFORM VARYING 繰返カウンタ FROM 1 BY 1  UNTIL ( 繰返カウンタ > 摘要区分数 ) 
020919*
020920        MOVE 繰返カウンタ             TO 摘要－制御区分
020921        MOVE 連患者削除－患者コード   TO 摘要－患者コード
020922        MOVE ZERO                     TO 摘要－施術和暦年月
020923        START 摘要ファイル KEY IS >= 摘要－制御区分
020924                                     摘要－患者コード
020925                                     摘要－施術和暦年月
020926        END-START
020927        IF 状態キー = "00"
020928            MOVE SPACE TO 終了フラグ
020929            PERFORM 摘要ファイル読込
020930            PERFORM UNTIL ( 繰返カウンタ           NOT = 摘要－制御区分 ) OR
020931                          ( 連患者削除－患者コード NOT = 摘要－患者コード ) OR
020932                          ( 終了フラグ = "YES" )
020933
020934                MOVE  連患者削除－変更患者コード TO 摘要－患者コード 
020935                WRITE 摘要－レコード
020936                INVALID KEY
020937                    MOVE NC"摘要" TO ファイル名Ｗ
020938                    PERFORM エラー表示
020939                END-WRITE
020940*
020942                MOVE  連患者削除－患者コード TO 摘要－患者コード 
020943                DELETE 摘要ファイル
020944                INVALID KEY
020945                    MOVE NC"摘要" TO ファイル名Ｗ
020946                    PERFORM エラー表示Ｄ
020947                END-DELETE
020948                PERFORM 摘要ファイル読込
020949            END-PERFORM
020950        END-IF
020951*
020952      END-PERFORM.
020953*
020954*================================================================*
020955 摘要ファイル枝番作成 SECTION.
020956*
020966* 制御区分が最後の副キーが無いので繰返し処理
020995*
020996      PERFORM VARYING 繰返カウンタ FROM 1 BY 1  UNTIL ( 繰返カウンタ > 摘要区分数 ) 
020997*
020998        MOVE 繰返カウンタ             TO 摘要－制御区分
020999        MOVE 連患者削除－施術和暦年月 TO 摘要－施術和暦年月
021000        MOVE 連患者削除－患者コード   TO 摘要－患者コード
021001        READ 摘要ファイル
021002        NOT INVALID KEY
021003*          /*枝番有りレコードの作成
021004            MOVE 連患者削除－変更枝番 TO 摘要－枝番
021005            WRITE 摘要－レコード
021006            INVALID KEY
021007                MOVE NC"摘要" TO ファイル名Ｗ
021008                PERFORM エラー表示
021009            END-WRITE
021018        END-READ
021019*
021020      END-PERFORM.
021022*
021055*================================================================*
021056 摘要ファイル読込 SECTION.
021057*
021058     READ 摘要ファイル NEXT
021059     AT END
021060         MOVE "YES" TO 終了フラグ
021061     END-READ.
021062*================================================================*
021063*================================================================*
021064 受診者情報２Ｆ当月削除 SECTION.
021065*
021066     MOVE 連患者削除－患者コード   TO 受２－患者コード.
021067     MOVE 連患者削除－施術和暦年月 TO 受２－施術和暦年月.
021068*
021069     READ 受診者情報２Ｆ
021070     NOT INVALID KEY
021071         DELETE 受診者情報２Ｆ
021072         INVALID KEY
021073             MOVE NC"受２" TO ファイル名Ｗ
021074             PERFORM エラー表示Ｄ
021075         END-DELETE
021076     END-READ.
021077*
021078*================================================================*
021079 受診者情報２Ｆ全削除 SECTION.
021080*
021081     MOVE ZERO            TO 受２－患者番号.
021082     MOVE SPACE           TO 受２－枝番.
021083     MOVE ZERO            TO 受２－施術和暦年月.
021084*
021085     START 受診者情報２Ｆ KEY IS >= 受２－施術和暦年月
021086                                    受２－患者コード
021087     END-START.
021090*
021100     IF 状態キー = "00"
021110         MOVE SPACE            TO 終了フラグ
021120         PERFORM 受診者情報２Ｆ読込
021130         PERFORM UNTIL (終了フラグ   = "YES")
021140             IF (受２－患者コード    = 連患者削除－患者コード)
021150                 DELETE 受診者情報２Ｆ
021160                 INVALID KEY
021170                     MOVE NC"受２" TO ファイル名Ｗ
021180                     PERFORM エラー表示Ｄ
021190                 END-DELETE
021200             END-IF
021210             PERFORM 受診者情報２Ｆ読込
021220         END-PERFORM
021230     END-IF.
021240*
021250*================================================================*
021260 受診者情報２Ｆ変更 SECTION.
021270*
021280     MOVE ZERO            TO 受２－患者番号.
021290     MOVE SPACE           TO 受２－枝番.
021300     MOVE ZERO            TO 受２－施術和暦年月.
021310*
021320     START 受診者情報２Ｆ KEY IS >= 受２－施術和暦年月
021330                                    受２－患者コード
021340     END-START.
021350*
021360     IF 状態キー = "00"
021370         MOVE SPACE TO 終了フラグ
021380         PERFORM 受診者情報２Ｆ読込
021390         PERFORM UNTIL (終了フラグ   = "YES")
021400             IF (受２－患者コード    = 連患者削除－患者コード)
021410                 MOVE  連患者削除－変更患者コード TO 受２－患者コード
021420                 WRITE 受２－レコード
021430                 INVALID KEY
021440                     MOVE NC"受２" TO ファイル名Ｗ
021450                     PERFORM エラー表示
021460                 END-WRITE
021470                 MOVE  連患者削除－患者コード TO 受２－患者コード
021480                 DELETE 受診者情報２Ｆ
021490                 INVALID KEY
021500                     MOVE NC"受２" TO ファイル名Ｗ
021510                     PERFORM エラー表示Ｄ
021520                 END-DELETE
021530             END-IF
021540             PERFORM 受診者情報２Ｆ読込
021550         END-PERFORM
021560     END-IF.
021570*
021580*================================================================*
021590 受診者情報２Ｆ枝番作成 SECTION.
021600*
021610     MOVE 連患者削除－患者コード   TO 受２－患者コード.
021620     MOVE 連患者削除－施術和暦年月 TO 受２－施術和暦年月.
021630*
021640     READ 受診者情報２Ｆ
021650     NOT INVALID KEY
021660         MOVE  連患者削除－変更枝番 TO  受２－枝番
021670         WRITE 受２－レコード
021680         INVALID KEY
021690             MOVE NC"受２" TO ファイル名Ｗ
021700             PERFORM エラー表示
021710         END-WRITE
021720     END-READ.
021721*
021722*/指定枝番データの００のデータ作成。（※健保のみ）
021723* 2:労災、3:自賠責、4:生保、5:自費は、月途中変更で枝番レコードを作らない。
021724     IF 保険分類Ｗ = 1
021725*
021730       MOVE ZERO     TO 受２－施術和暦
021740       MOVE ZERO     TO 受２－施術年
021750       MOVE ZERO     TO 受２－施術月
021760       MOVE 連患者削除－変更患者コード TO 受２－患者コード
021770       READ 受診者情報２Ｆ
021780       INVALID KEY
021790         MOVE SPACE TO 受２－レコード
021800         INITIALIZE    受２－レコード
021810         MOVE 連患者削除－変更患者コード TO 受２－患者コード
021820         MOVE 連患者削除－施術和暦年月   TO 受２－施術和暦年月
021830         READ 受診者情報２Ｆ
021840         NOT INVALID KEY
021871* 初期化する
021872                 MOVE ZERO           TO 受２－施術和暦
021873                 MOVE ZERO           TO 受２－施術年  
021874                 MOVE ZERO           TO 受２－施術月  
021875                 MOVE ZERO           TO 受２－請求和暦
021876                 MOVE ZERO           TO 受２－請求年  
021877                 MOVE ZERO           TO 受２－請求月  
021878                 MOVE ZERO           TO 受２－請求区分
021879                 MOVE ZERO           TO 受２－請求対象区分
021880                 MOVE ZERO           TO 受２－助成請求和暦
021881                 MOVE ZERO           TO 受２－助成請求年  
021882                 MOVE ZERO           TO 受２－助成請求月  
021883                 MOVE ZERO           TO 受２－助成請求区分
021884                 MOVE ZERO           TO 受２－助成請求対象区分
021885                 MOVE ZERO           TO 受２－来院動機区分
021886                 MOVE ZERO           TO 受２－紹介者患者番号
021887                 MOVE ZERO           TO 受２－柔整本体償還払い区分
021888                 MOVE ZERO           TO 受２－柔整本体まとめ区分
021889                 MOVE ZERO           TO 受２－柔整助成償還払い区分
021890                 MOVE ZERO           TO 受２－柔整助成レセ印刷対象区分
021891                 MOVE ZERO           TO 受２－柔整助成レセ会総括表印刷対象区分
021892                 MOVE ZERO           TO 受２－柔整本体枝番まとめ区分
021893*
021896             WRITE 受２－レコード
021897             INVALID KEY
021900                 MOVE NC"受２" TO ファイル名Ｗ
021910                 PERFORM エラー表示
021920             END-WRITE
021930         END-READ
021940       END-READ
021941     END-IF.
021950*
021960*================================================================*
021970 受診者情報２Ｆ読込 SECTION.
021980*
021990     READ 受診者情報２Ｆ NEXT
022000     AT END
022010         MOVE "YES" TO 終了フラグ
022020     END-READ.
022030*================================================================*
022040 生保情報Ｆ当月削除 SECTION.
022050*
022060     MOVE 連患者削除－患者コード   TO 生保－患者コード.
022070     MOVE 連患者削除－施術和暦年月 TO 生保－施術和暦年月.
022080*
022090     READ 生保情報Ｆ
022100     NOT INVALID KEY
022110         DELETE 生保情報Ｆ
022120         INVALID KEY
022130             MOVE NC"生保" TO ファイル名Ｗ
022140             PERFORM エラー表示Ｄ
022150         END-DELETE
022160     END-READ.
022170*
022180*================================================================*
022190 生保情報Ｆ全削除 SECTION.
022200*
022210     MOVE 連患者削除－患者コード   TO 生保－患者コード.
022220     MOVE ZERO                     TO 生保－施術和暦年月.
022230*
022240     START 生保情報Ｆ KEY IS >= 生保－患者コード
022250                                生保－施術和暦年月
022260     END-START.
022270*
022280     IF 状態キー = "00"
022290         MOVE SPACE            TO 終了フラグ
022300         PERFORM 生保情報Ｆ読込
022310         PERFORM UNTIL (生保－患者コード   NOT = 連患者削除－患者コード)   OR
022320                       (終了フラグ             = "YES")
022330             DELETE 生保情報Ｆ
022340             INVALID KEY
022350                 MOVE NC"生保" TO ファイル名Ｗ
022360                 PERFORM エラー表示Ｄ
022370             END-DELETE
022380             PERFORM 生保情報Ｆ読込
022390         END-PERFORM
022400     END-IF.
022410*
022420*================================================================*
022430 生保情報Ｆ変更 SECTION.
022440*
022450     MOVE 連患者削除－患者コード   TO 生保－患者コード.
022460     MOVE ZERO                     TO 生保－施術和暦年月.
022470*
022480     START 生保情報Ｆ KEY IS >= 生保－患者コード
022490                                生保－施術和暦年月
022500     END-START.
022510*
022520     IF 状態キー = "00"
022530         MOVE SPACE TO 終了フラグ
022540         PERFORM 生保情報Ｆ読込
022550         PERFORM UNTIL (生保－患者コード   NOT = 連患者削除－患者コード)   OR
022560                       (終了フラグ             = "YES")
022570             MOVE  連患者削除－変更患者コード TO 生保－患者コード
022580             WRITE 生保－レコード
022590             INVALID KEY
022600                 MOVE NC"生保" TO ファイル名Ｗ
022610                 PERFORM エラー表示
022620             END-WRITE
022630             MOVE  連患者削除－患者コード TO 生保－患者コード
022640             DELETE 生保情報Ｆ
022650             INVALID KEY
022660                 MOVE NC"生保" TO ファイル名Ｗ
022670                 PERFORM エラー表示Ｄ
022680             END-DELETE
022690             PERFORM 生保情報Ｆ読込
022700         END-PERFORM
022710     END-IF.
022720*
022730*================================================================*
022740 生保情報Ｆ枝番作成 SECTION.
022750*
022760     MOVE 連患者削除－患者コード   TO 生保－患者コード.
022770     MOVE 連患者削除－施術和暦年月 TO 生保－施術和暦年月.
022780*
022790     READ 生保情報Ｆ
022800     NOT INVALID KEY
022810         MOVE  連患者削除－変更枝番 TO  生保－枝番
022820         WRITE 生保－レコード
022830         INVALID KEY
022840             MOVE NC"生保" TO ファイル名Ｗ
022850             PERFORM エラー表示
022860         END-WRITE
022870     END-READ.
022880*
022890*================================================================*
022900 生保情報Ｆ読込 SECTION.
022910*
022920     READ 生保情報Ｆ NEXT
022930     AT END
022940         MOVE "YES" TO 終了フラグ
022950     END-READ.
022960*================================================================*
022970 自賠責情報Ｆ当月削除 SECTION.
022980*
022990     MOVE 連患者削除－患者コード   TO 自賠－患者コード.
023000     MOVE 連患者削除－施術和暦年月 TO 自賠－施術和暦年月.
023010*
023020     READ 自賠責情報Ｆ
023030     NOT INVALID KEY
023040         DELETE 自賠責情報Ｆ
023050         INVALID KEY
023060             MOVE NC"自賠" TO ファイル名Ｗ
023070             PERFORM エラー表示Ｄ
023080         END-DELETE
023090     END-READ.
023100*
023110*================================================================*
023120 自賠責情報Ｆ全削除 SECTION.
023130*
023140     MOVE 連患者削除－患者コード   TO 自賠－患者コード.
023150     MOVE ZERO                     TO 自賠－施術和暦年月.
023160*
023170     START 自賠責情報Ｆ KEY IS >= 自賠－患者コード
023180                                  自賠－施術和暦年月
023190     END-START.
023200*
023210     IF 状態キー = "00"
023220         MOVE SPACE            TO 終了フラグ
023230         PERFORM 自賠責情報Ｆ読込
023240         PERFORM UNTIL (自賠－患者コード NOT = 連患者削除－患者コード) OR
023250                       (終了フラグ           = "YES")
023260             DELETE 自賠責情報Ｆ
023270             INVALID KEY
023280                 MOVE NC"自賠" TO ファイル名Ｗ
023290                 PERFORM エラー表示Ｄ
023300             END-DELETE
023310             PERFORM 自賠責情報Ｆ読込
023320         END-PERFORM
023330     END-IF.
023340*
023350*================================================================*
023360 自賠責情報Ｆ変更 SECTION.
023370*
023380     MOVE 連患者削除－患者コード   TO 自賠－患者コード.
023390     MOVE ZERO                     TO 自賠－施術和暦年月.
023400*
023410     START 自賠責情報Ｆ KEY IS >= 自賠－患者コード
023420                                  自賠－施術和暦年月
023430     END-START.
023440*
023450     IF 状態キー = "00"
023460         MOVE SPACE TO 終了フラグ
023470         PERFORM 自賠責情報Ｆ読込
023480         PERFORM UNTIL (自賠－患者コード NOT = 連患者削除－患者コード) OR
023490                       (終了フラグ           = "YES")
023500             MOVE  連患者削除－変更患者コード TO 自賠－患者コード
023510             WRITE 自賠－レコード
023520             INVALID KEY
023530                 MOVE NC"自賠" TO ファイル名Ｗ
023540                 PERFORM エラー表示
023550             END-WRITE
023560             MOVE  連患者削除－患者コード TO 自賠－患者コード
023570             DELETE 自賠責情報Ｆ
023580             INVALID KEY
023590                 MOVE NC"自賠" TO ファイル名Ｗ
023600                 PERFORM エラー表示Ｄ
023610             END-DELETE
023620             PERFORM 自賠責情報Ｆ読込
023630         END-PERFORM
023640     END-IF.
023650*
023660*================================================================*
023670 自賠責情報Ｆ枝番作成 SECTION.
023680*
023690     MOVE 連患者削除－患者コード   TO 自賠－患者コード.
023700     MOVE 連患者削除－施術和暦年月 TO 自賠－施術和暦年月.
023710*
023720     READ 自賠責情報Ｆ
023730     NOT INVALID KEY
023740         MOVE  連患者削除－変更枝番 TO  自賠－枝番
023750         WRITE 自賠－レコード
023760         INVALID KEY
023770             MOVE NC"自賠" TO ファイル名Ｗ
023780             PERFORM エラー表示
023790         END-WRITE
023800     END-READ.
023810*
023820*================================================================*
023830 自賠責情報Ｆ読込 SECTION.
023840*
023850     READ 自賠責情報Ｆ NEXT
023860     AT END
023870         MOVE "YES" TO 終了フラグ
023880     END-READ.
023890*================================================================*
023900 労災情報Ｆ当月削除 SECTION.
023910*
024132     MOVE 連患者削除－患者コード   TO 労災－患者コード.
024133     MOVE 連患者削除－施術和暦年月 TO 労災－施術和暦年月.
024134*
024135     READ 労災情報Ｆ
024136     NOT INVALID KEY
024137         DELETE 労災情報Ｆ
024138         INVALID KEY
024139             MOVE NC"労災" TO ファイル名Ｗ
024140             PERFORM エラー表示Ｄ
024141         END-DELETE
024142     END-READ.
024143*
024145*================================================================*
024150 労災情報Ｆ全削除 SECTION.
024160*
024281     MOVE 連患者削除－患者コード   TO 労災－患者コード.
024282     MOVE ZERO                     TO 労災－施術和暦年月.
024283*
024284     START 労災情報Ｆ KEY IS >= 労災－患者コード
024285                                労災－施術和暦年月
024286     END-START.
024287*
024288     IF 状態キー = "00"
024289         MOVE SPACE            TO 終了フラグ
024290         PERFORM 労災情報Ｆ読込
024291         PERFORM UNTIL (労災－患者コード   NOT = 連患者削除－患者コード)   OR
024292                       (終了フラグ             = "YES")
024293             DELETE 労災情報Ｆ
024294             INVALID KEY
024295                 MOVE NC"労災" TO ファイル名Ｗ
024296                 PERFORM エラー表示Ｄ
024297             END-DELETE
024298             PERFORM 労災情報Ｆ読込
024299         END-PERFORM
024300     END-IF.
024301*
024305*================================================================*
024306 労災情報Ｆ変更 SECTION.
024310*
024320     MOVE 連患者削除－患者コード   TO 労災－患者コード.
024330     MOVE ZERO                     TO 労災－施術和暦年月.
024340*
024350     START 労災情報Ｆ KEY IS >= 労災－患者コード
024360                                労災－施術和暦年月
024370     END-START.
024380*
024390     IF 状態キー = "00"
024400         MOVE SPACE TO 終了フラグ
024410         PERFORM 労災情報Ｆ読込
024420         PERFORM UNTIL (労災－患者コード NOT = 連患者削除－患者コード) OR
024430                       (終了フラグ           = "YES")
024440             MOVE  連患者削除－変更患者コード TO 労災－患者コード
024450             WRITE 労災－レコード
024460             INVALID KEY
024470                 MOVE NC"労災" TO ファイル名Ｗ
024480                 PERFORM エラー表示
024490             END-WRITE
024500             MOVE  連患者削除－患者コード TO 労災－患者コード
024510             DELETE 労災情報Ｆ
024520             INVALID KEY
024530                 MOVE NC"労災" TO ファイル名Ｗ
024540                 PERFORM エラー表示Ｄ
024550             END-DELETE
024560             PERFORM 労災情報Ｆ読込
024570         END-PERFORM
024580     END-IF.
024590*
024600*================================================================*
024610 労災情報Ｆ枝番作成 SECTION.
024620*
024630     MOVE 連患者削除－患者コード   TO 労災－患者コード.
024640     MOVE 連患者削除－施術和暦年月 TO 労災－施術和暦年月.
024650*
024660     READ 労災情報Ｆ
024670     NOT INVALID KEY
024680         MOVE  連患者削除－変更枝番 TO  労災－枝番
024690         WRITE 労災－レコード
024700         INVALID KEY
024710             MOVE NC"労災" TO ファイル名Ｗ
024720             PERFORM エラー表示
024730         END-WRITE
024740     END-READ.
024750*
024760*================================================================*
024770 労災情報Ｆ読込 SECTION.
024780*
024790     READ 労災情報Ｆ NEXT
024800     AT END
024810         MOVE "YES" TO 終了フラグ
024820     END-READ.
024830*================================================================*
024840 会計領収Ｆ当月削除 SECTION.
024850*
024860     MOVE ZERO            TO 領－施術区分.
024870     MOVE ZERO            TO 領－会計領収区分.
024880     MOVE ZERO            TO 領－患者番号.
024890     MOVE SPACE           TO 領－枝番.
024900     MOVE ZERO            TO 領－施術和暦年月日.
024910*
024920     START 会計領収Ｆ KEY IS >= 領－施術区分
024930                                領－会計領収区分
024940                                領－施術和暦年月日
024950                                領－患者コード
024960     END-START.
024970*
024980     IF 状態キー = "00"
024990         MOVE SPACE            TO 終了フラグ
025000         PERFORM 会計領収Ｆ読込
025010         PERFORM UNTIL (終了フラグ = "YES")
025020             IF (領－患者コード    = 連患者削除－患者コード) AND
025030                (領－施術和暦年月  = 連患者削除－施術和暦年月)
025040                 DELETE 会計領収Ｆ
025050                 INVALID KEY
025060                     MOVE NC"領収" TO ファイル名Ｗ
025070                     PERFORM エラー表示Ｄ
025080                 END-DELETE
025090             END-IF
025100             PERFORM 会計領収Ｆ読込
025110         END-PERFORM
025120     END-IF.
025130*
025140*================================================================*
025150 会計領収Ｆ全削除 SECTION.
025160*
025170     MOVE ZERO            TO 領－施術区分.
025180     MOVE ZERO            TO 領－会計領収区分.
025190     MOVE ZERO            TO 領－患者番号.
025200     MOVE SPACE           TO 領－枝番.
025210     MOVE ZERO            TO 領－施術和暦年月日.
025220*
025230     START 会計領収Ｆ KEY IS >= 領－施術区分
025240                                領－会計領収区分
025250                                領－施術和暦年月日
025260                                領－患者コード
025270     END-START.
025280*
025290     IF 状態キー = "00"
025300         MOVE SPACE            TO 終了フラグ
025310         PERFORM 会計領収Ｆ読込
025320         PERFORM UNTIL (終了フラグ = "YES")
025330             IF (領－患者コード    = 連患者削除－患者コード)
025340                 DELETE 会計領収Ｆ
025350                 INVALID KEY
025360                     MOVE NC"領収" TO ファイル名Ｗ
025370                     PERFORM エラー表示Ｄ
025380                 END-DELETE
025390             END-IF
025400             PERFORM 会計領収Ｆ読込
025410         END-PERFORM
025420     END-IF.
025430*
025440*================================================================*
025450 会計領収Ｆ変更 SECTION.
025460*
025470     MOVE ZERO            TO 領－施術区分.
025480     MOVE ZERO            TO 領－会計領収区分.
025490     MOVE ZERO            TO 領－患者番号.
025500     MOVE SPACE           TO 領－枝番.
025510     MOVE ZERO            TO 領－施術和暦年月日.
025520*
025530     START 会計領収Ｆ KEY IS >= 領－施術区分
025540                                領－会計領収区分
025550                                領－施術和暦年月日
025560                                領－患者コード
025570     END-START.
025580*
025590     IF 状態キー = "00"
025600         MOVE SPACE            TO 終了フラグ
025610         PERFORM 会計領収Ｆ読込
025620         PERFORM UNTIL (終了フラグ = "YES")
025630             IF (領－患者コード    = 連患者削除－患者コード)
025640                 MOVE  連患者削除－変更患者コード TO 領－患者コード
025650                 WRITE 領－レコード
025660                 INVALID KEY
025670                     MOVE NC"領収" TO ファイル名Ｗ
025680                     PERFORM エラー表示
025690                 END-WRITE
025700                 MOVE  連患者削除－患者コード TO 領－患者コード
025710                 DELETE 会計領収Ｆ
025720                 INVALID KEY
025730                     MOVE NC"領収" TO ファイル名Ｗ
025740                     PERFORM エラー表示Ｄ
025750                 END-DELETE
025760             END-IF
025770             PERFORM 会計領収Ｆ読込
025780         END-PERFORM
025790     END-IF.
025800*
025810*================================================================*
025820 会計領収Ｆ枝番作成 SECTION.
025830*
025840     MOVE ZERO            TO 領－施術区分.
025850     MOVE ZERO            TO 領－会計領収区分.
025860     MOVE ZERO            TO 領－患者番号.
025870     MOVE SPACE           TO 領－枝番.
025880     MOVE ZERO            TO 領－施術和暦年月日.
025890*
025900     START 会計領収Ｆ KEY IS >= 領－施術区分
025910                                領－会計領収区分
025920                                領－施術和暦年月日
025930                                領－患者コード
025940     END-START.
025950*
025960     IF 状態キー = "00"
025970         MOVE SPACE            TO 終了フラグ
025980         PERFORM 会計領収Ｆ読込
025990         PERFORM UNTIL (終了フラグ = "YES")
026000*/変更日以降はコンバートしない
026010             IF (領－患者コード    = 連患者削除－患者コード) AND
026020                (領－施術和暦年月  = 連患者削除－施術和暦年月) AND
026030                (領－施術日        < 連患者枝番－変更日)
026040                 MOVE 連患者削除－変更枝番 TO 領－枝番
026050                 WRITE 領－レコード
026060                 INVALID KEY
026070                     MOVE NC"領収" TO ファイル名Ｗ
026080                     PERFORM エラー表示
026090                 END-WRITE
026100                 MOVE 連患者削除－枝番  TO 領－枝番
026110                 DELETE 会計領収Ｆ
026120                 INVALID KEY
026130                     MOVE NC"領収" TO ファイル名Ｗ
026140                     PERFORM エラー表示Ｄ
026150                 END-DELETE
026160             END-IF
026170             PERFORM 会計領収Ｆ読込
026180         END-PERFORM
026190     END-IF.
026200*
026210*================================================================*
026220 会計領収Ｆ読込 SECTION.
026230*
026240     READ 会計領収Ｆ NEXT
026250     AT END
026260         MOVE "YES" TO 終了フラグ
026270     END-READ.
026280*================================================================*
026282*================================================================*
026283 ＤＭ記録Ｆ全削除 SECTION.
026284*
026303     MOVE 連患者削除－患者コード   TO ＤＭ－患者コード.
026304     MOVE ZERO                     TO ＤＭ－発行和暦年月日.
026305     MOVE ZERO                     TO ＤＭ－発行枝番.
026306*
026307     START ＤＭ記録Ｆ KEY IS >= ＤＭ－患者コード
026308                                ＤＭ－発行和暦年月日
026309                                ＤＭ－発行枝番
026310     END-START.
026311*
026312     IF 状態キー = "00"
026313         MOVE SPACE            TO 終了フラグ
026314         PERFORM ＤＭ記録Ｆ読込
026315         PERFORM UNTIL (ＤＭ－患者コード   NOT = 連患者削除－患者コード)   OR
026316                       (終了フラグ             = "YES")
026317             DELETE ＤＭ記録Ｆ
026318             INVALID KEY
026319                 MOVE NC"ＤＭ記録" TO ファイル名Ｗ
026320                 PERFORM エラー表示Ｄ
026321             END-DELETE
026322             PERFORM ＤＭ記録Ｆ読込
026323         END-PERFORM
026324     END-IF.
026325*
026326*================================================================*
026327 ＤＭ記録Ｆ変更 SECTION.
026328*
026337     MOVE 連患者削除－患者コード   TO ＤＭ－患者コード.
026338     MOVE ZERO                     TO ＤＭ－発行和暦年月日.
026339     MOVE ZERO                     TO ＤＭ－発行枝番.
026340*
026341     START ＤＭ記録Ｆ KEY IS >= ＤＭ－患者コード
026342                                ＤＭ－発行和暦年月日
026343                                ＤＭ－発行枝番
026344     END-START.
026345*
026346     IF 状態キー = "00"
026347         MOVE SPACE TO 終了フラグ
026348         PERFORM ＤＭ記録Ｆ読込
026349         PERFORM UNTIL (ＤＭ－患者コード   NOT = 連患者削除－患者コード)   OR
026350                       (終了フラグ             = "YES")
026357             MOVE  連患者削除－変更患者コード TO ＤＭ－患者コード
026358             WRITE ＤＭ－レコード
026359             INVALID KEY
026360                 MOVE NC"ＤＭ記録" TO ファイル名Ｗ
026361                 PERFORM エラー表示
026362             END-WRITE
026363             MOVE  連患者削除－患者コード TO ＤＭ－患者コード
026364             DELETE ＤＭ記録Ｆ
026365             INVALID KEY
026366                 MOVE NC"ＤＭ記録" TO ファイル名Ｗ
026367                 PERFORM エラー表示Ｄ
026368             END-DELETE
026369             PERFORM ＤＭ記録Ｆ読込
026370         END-PERFORM
026371     END-IF.
026372*
026373*================================================================*
026374 ＤＭ記録Ｆ枝番作成 SECTION.
026375*
026376     MOVE 連患者削除－患者コード   TO ＤＭ－患者コード.
026377     MOVE 連患者削除－施術和暦年月 TO ＤＭ－発行和暦年月.
026378     MOVE 1                        TO ＤＭ－発行日.
026379     MOVE ZERO                     TO ＤＭ－発行枝番.
026380*
026391     START ＤＭ記録Ｆ KEY IS >= ＤＭ－患者コード
026392                                ＤＭ－発行和暦年月日
026393                                ＤＭ－発行枝番
026394     END-START.
026397*
026398     IF 状態キー = "00"
026399         MOVE SPACE TO 終了フラグ
026400         PERFORM ＤＭ記録Ｆ読込
026401*/変更日以降はコンバートしない
026402         PERFORM UNTIL (ＤＭ－患者コード   NOT = 連患者削除－患者コード)   OR
026403                       (ＤＭ－発行和暦年月 NOT = 連患者削除－施術和暦年月) OR
026404                       (ＤＭ－発行日          >= 連患者枝番－変更日    ) OR
026405                       (終了フラグ             = "YES")
026406             MOVE  連患者削除－変更患者コード TO ＤＭ－患者コード
026407             WRITE ＤＭ－レコード
026408             INVALID KEY
026409                 MOVE NC"ＤＭ記録" TO ファイル名Ｗ
026410                 PERFORM エラー表示
026411             END-WRITE
026412             MOVE  連患者削除－患者コード TO ＤＭ－患者コード
026413             DELETE ＤＭ記録Ｆ
026414             INVALID KEY
026415                 MOVE NC"ＤＭ記録" TO ファイル名Ｗ
026416                 PERFORM エラー表示Ｄ
026417             END-DELETE
026418             PERFORM ＤＭ記録Ｆ読込
026419         END-PERFORM
026420     END-IF.
026421*
026422*================================================================*
026423 ＤＭ記録Ｆ読込 SECTION.
026424*
026425     READ ＤＭ記録Ｆ NEXT
026426     AT END
026427         MOVE "YES" TO 終了フラグ
026428     END-READ.
026429*
026430*================================================================*
026431 施設受診者マスタ全削除 SECTION.
026432*
026433     MOVE 連患者削除－患者コード   TO 施設受－患者コード.
026434     MOVE ZERO                     TO 施設受－施設コード.
026437*
026438     START 施設受診者マスタ KEY IS >= 施設受－患者コード
026439                                      施設受－施設コード
026440     END-START.
026441*
026442     IF 状態キー = "00"
026443         MOVE SPACE            TO 終了フラグ
026444         PERFORM 施設受診者マスタ読込
026445         PERFORM UNTIL (施設受－患者コード   NOT = 連患者削除－患者コード)   OR
026446                       (終了フラグ             = "YES")
026447             DELETE 施設受診者マスタ
026448             INVALID KEY
026449                 MOVE NC"施設受診者マスタ" TO ファイル名Ｗ
026450                 PERFORM エラー表示Ｄ
026451             END-DELETE
026452             PERFORM 施設受診者マスタ読込
026453         END-PERFORM
026454     END-IF.
026455*
026456*================================================================*
026457 施設受診者マスタ変更 SECTION.
026458*
026461     MOVE 連患者削除－患者コード   TO 施設受－患者コード.
026462     MOVE ZERO                     TO 施設受－施設コード.
026463*
026467     START 施設受診者マスタ KEY IS >= 施設受－患者コード
026468                                      施設受－施設コード
026469     END-START.
026470*
026471     IF 状態キー = "00"
026472         MOVE SPACE TO 終了フラグ
026473         PERFORM 施設受診者マスタ読込
026474         PERFORM UNTIL (施設受－患者コード NOT = 連患者削除－患者コード) OR
026475                       (終了フラグ           = "YES")
026476             MOVE  連患者削除－変更患者コード TO 施設受－患者コード
026477             WRITE 施設受－レコード
026478             INVALID KEY
026479                 MOVE NC"施設受診者マスタ" TO ファイル名Ｗ
026480                 PERFORM エラー表示
026481             END-WRITE
026482             MOVE  連患者削除－患者コード TO 施設受－患者コード
026483             DELETE 施設受診者マスタ
026484             INVALID KEY
026485                 MOVE NC"施設受診者マスタ" TO ファイル名Ｗ
026486                 PERFORM エラー表示Ｄ
026487             END-DELETE
026488             PERFORM 施設受診者マスタ読込
026489         END-PERFORM
026490     END-IF.
026491*
026501*================================================================*
026502 施設受診者マスタ読込 SECTION.
026503*
026504     READ 施設受診者マスタ NEXT
026505     AT END
026506         MOVE "YES" TO 終了フラグ
026507     END-READ.
026508*================================================================*
026509*================================================================*
026510*================================================================*
026511 ＨレセプトＦ当月削除 SECTION.
026512*
026513     MOVE 施術区分Ｗ                TO  Ｈレセ－施術区分.
026514     MOVE 連患者削除－患者コード    TO  Ｈレセ－患者コード.
026515     MOVE 連患者削除－施術和暦年月  TO  Ｈレセ－施術和暦年月.
026516     MOVE ZERO                      TO  Ｈレセ－レセ種別.
026517*
026518     START ＨレセプトＦ KEY IS >= Ｈレセ－施術区分
026519                                  Ｈレセ－患者コード
026520                                  Ｈレセ－施術和暦年月
026521                                  Ｈレセ－レセ種別
026522     END-START.
026523*
026524     IF 状態キー = "00"
026525            MOVE SPACE TO 終了フラグ
026526            PERFORM ＨレセプトＦ読込
026527            PERFORM UNTIL ( Ｈレセ－施術区分     NOT = 施術区分Ｗ               ) OR
026528                          ( Ｈレセ－患者コード   NOT = 連患者削除－患者コード ) OR
026529                          ( Ｈレセ－施術和暦年月 NOT = 連患者削除－施術和暦年月   ) OR
026530                          ( 終了フラグ           NOT = SPACE )
026531*
                      PERFORM Ｈレセプト詳細Ｆ削除
026532                DELETE ＨレセプトＦ
026533                INVALID KEY
026534                   MOVE NC"ＨレセプトＦ" TO ファイル名Ｗ
026540                   PERFORM エラー表示Ｄ
026550                END-DELETE
026560*
026570                PERFORM ＨレセプトＦ読込
026580            END-PERFORM
026590     END-IF.
026600*
026610*================================================================*
026620 ＨレセプトＦ全削除 SECTION.
026630*
026640     MOVE 施術区分Ｗ             TO  Ｈレセ－施術区分.
026650     MOVE 連患者削除－患者コード TO  Ｈレセ－患者コード.
026660     MOVE ZERO                   TO  Ｈレセ－施術和暦年月.
026670     MOVE ZERO                   TO  Ｈレセ－レセ種別.
026680*
026690     START ＨレセプトＦ KEY IS >= Ｈレセ－施術区分
026700                                  Ｈレセ－患者コード
026710                                  Ｈレセ－施術和暦年月
026720                                  Ｈレセ－レセ種別
026730     END-START.
026740*
026750     IF 状態キー = "00"
026760            MOVE SPACE TO 終了フラグ
026770            PERFORM ＨレセプトＦ読込
026780            PERFORM UNTIL ( Ｈレセ－施術区分   NOT = 施術区分Ｗ               ) OR
026790                          ( Ｈレセ－患者コード NOT = 連患者削除－患者コード ) OR
026800                          ( 終了フラグ         NOT = SPACE )
026810*
                      PERFORM Ｈレセプト詳細Ｆ削除
026820                DELETE ＨレセプトＦ
026830                INVALID KEY
026840                   MOVE NC"ＨレセプトＦ" TO ファイル名Ｗ
026850                   PERFORM エラー表示Ｄ
026860                END-DELETE
026870*
026880                PERFORM ＨレセプトＦ読込
026890            END-PERFORM
026900     END-IF.
026910*
026920*================================================================*
026930 ＨレセプトＦ変更 SECTION.
026940*
026950     MOVE 施術区分Ｗ             TO  Ｈレセ－施術区分.
026960     MOVE 連患者削除－患者コード TO  Ｈレセ－患者コード.
026970     MOVE ZERO                   TO  Ｈレセ－施術和暦年月.
026980     MOVE ZERO                   TO  Ｈレセ－レセ種別.
026990*
027000     START ＨレセプトＦ KEY IS >= Ｈレセ－施術区分
027010                                  Ｈレセ－患者コード
027020                                  Ｈレセ－施術和暦年月
027030                                  Ｈレセ－レセ種別
027040     END-START.
027050*
027060     IF 状態キー = "00"
027070            MOVE SPACE TO 終了フラグ
027080            PERFORM ＨレセプトＦ読込
027090            PERFORM UNTIL ( Ｈレセ－施術区分   NOT = 施術区分Ｗ               ) OR
027100                          ( Ｈレセ－患者コード NOT = 連患者削除－患者コード ) OR
027110                          ( 終了フラグ         NOT = SPACE )
027120*
027130*------------    / 新レコードの作成 / -------------------------------*
                      PERFORM Ｈレセプト詳細Ｆ作成
027140                MOVE 連患者削除－変更患者コード  TO Ｈレセ－患者コード
027150                WRITE Ｈレセ－レコード
027160                INVALID KEY
027170                   MOVE NC"ＨレセプトＦ" TO ファイル名Ｗ
027180                   PERFORM エラー表示
027190                END-WRITE
027200*
027210*------------   / レコードの削除 / -------------------------------*
027220                MOVE 連患者削除－患者コード  TO Ｈレセ－患者コード
                      PERFORM Ｈレセプト詳細Ｆ削除
027230                DELETE ＨレセプトＦ
027240                INVALID KEY
027250                   MOVE NC"ＨレセプトＦ" TO ファイル名Ｗ
027260                   PERFORM エラー表示Ｄ
027270                END-DELETE
027280*
027290                PERFORM ＨレセプトＦ読込
027300            END-PERFORM
027310     END-IF.
027320*
027330*================================================================*
027340 ＨレセプトＦ読込 SECTION.
027350*
027360     READ ＨレセプトＦ NEXT
027370     AT END
027380         MOVE "YES"  TO 終了フラグ
027390     END-READ.
027400*
027410*================================================================*
027420 Ｈ日計データＦ当月削除 SECTION.
027430*
027440     MOVE 施術区分Ｗ                TO  Ｈ日－施術区分.
027450     MOVE 連患者削除－患者コード    TO  Ｈ日－患者コード.
027460     MOVE 連患者削除－施術和暦年月  TO  Ｈ日－施術和暦年月.
027470     MOVE ZERO                      TO  Ｈ日－施術日.
027480*
027490     START Ｈ日計データＦ KEY IS >= Ｈ日－施術区分
027500                                    Ｈ日－患者コード
027510                                    Ｈ日－施術和暦年月日
027520     END-START.
027530*
027540     IF 状態キー = "00"
027550         MOVE SPACE TO 終了フラグ
027560         PERFORM Ｈ日計データＦ読込
027570         PERFORM UNTIL ( Ｈ日－施術区分     NOT = 施術区分Ｗ               ) OR
027580                       ( Ｈ日－患者コード   NOT = 連患者削除－患者コード ) OR
027590                       ( Ｈ日－施術和暦年月 NOT = 連患者削除－施術和暦年月   ) OR
027600                       ( 終了フラグ         NOT = SPACE )
027610*
027620                DELETE Ｈ日計データＦ
027630                IF 状態キー NOT = "00"
027640                   MOVE NC"Ｈ日計データＦ" TO ファイル名Ｗ
027650                   PERFORM エラー表示Ｄ
027660                END-IF
027670*
027680                PERFORM Ｈ日計データＦ読込
027690         END-PERFORM
027700     END-IF.
027710*
027720*================================================================*
027730 Ｈ日計データＦ全削除 SECTION.
027740*
027750     MOVE 施術区分Ｗ              TO  Ｈ日－施術区分.
027760     MOVE 連患者削除－患者コード  TO  Ｈ日－患者コード.
027770     MOVE ZERO                    TO  Ｈ日－施術和暦年月日.
027780*
027790     START Ｈ日計データＦ KEY IS >= Ｈ日－施術区分
027800                                    Ｈ日－患者コード
027810                                    Ｈ日－施術和暦年月日
027820     END-START.
027830*
027840     IF 状態キー = "00"
027850         MOVE SPACE TO 終了フラグ
027860         PERFORM Ｈ日計データＦ読込
027870         PERFORM UNTIL ( Ｈ日－施術区分   NOT = 施術区分Ｗ               ) OR
027880                       ( Ｈ日－患者コード NOT = 連患者削除－患者コード ) OR
027890                       ( 終了フラグ       NOT = SPACE )
027900*
027910                DELETE Ｈ日計データＦ
027920                IF 状態キー NOT = "00"
027930                   MOVE NC"Ｈ日計データＦ" TO ファイル名Ｗ
027940                   PERFORM エラー表示Ｄ
027950                END-IF
027960*
027970                PERFORM Ｈ日計データＦ読込
027980         END-PERFORM
027990     END-IF.
028000*
028010*================================================================*
028020 Ｈ日計データＦ変更 SECTION.
028030*
028040     MOVE 施術区分Ｗ              TO  Ｈ日－施術区分.
028050     MOVE 連患者削除－患者コード  TO  Ｈ日－患者コード.
028060     MOVE ZERO                    TO  Ｈ日－施術和暦年月日.
028070*
028080     START Ｈ日計データＦ KEY IS >= Ｈ日－施術区分
028090                                    Ｈ日－患者コード
028100                                    Ｈ日－施術和暦年月日
028110     END-START.
028120*
028130     IF 状態キー = "00"
028140         MOVE SPACE TO 終了フラグ
028150         PERFORM Ｈ日計データＦ読込
028160         PERFORM UNTIL ( Ｈ日－施術区分   NOT = 施術区分Ｗ               ) OR
028170                       ( Ｈ日－患者コード NOT = 連患者削除－患者コード ) OR
028180                       ( 終了フラグ       NOT = SPACE )
028190*
028200*------------    / 新レコードの作成 / -------------------------------*
028210                MOVE 連患者削除－変更患者コード  TO Ｈ日－患者コード
028220                COMPUTE Ｈ日－登録順  = Ｈ日－登録順 + 0.1
028230                WRITE Ｈ日－レコード
028240                IF 状態キー NOT = "00"
028250                   MOVE NC"Ｈ日計データＦ" TO ファイル名Ｗ
028260                   PERFORM エラー表示
028270                END-IF
028280*
028290*------------   / レコードの削除 / -------------------------------*
028300                MOVE 連患者削除－患者コード  TO Ｈ日－患者コード
028310                DELETE Ｈ日計データＦ
028320                IF 状態キー NOT = "00"
028330                   MOVE NC"Ｈ日計データＦ" TO ファイル名Ｗ
028340                   PERFORM エラー表示Ｄ
028350                END-IF
028360*
028370                PERFORM Ｈ日計データＦ読込
028380         END-PERFORM
028390     END-IF.
028400*
028410*================================================================*
028420 Ｈ日計データＦ読込 SECTION.
028430*
028440     READ Ｈ日計データＦ NEXT
028450     AT END
028460         MOVE "YES"  TO 終了フラグ
028470     END-READ.
028480*
028490*================================================================*
028500 Ｈ負傷データＦ全削除 SECTION.
028510*
028511* 枝番ありコードは削除されない。
028520     MOVE 施術区分Ｗ              TO  Ｈ負－施術区分.
028522     MOVE 連患者削除－患者コード  TO  Ｈ負－患者コード.
028540     MOVE ZERO                    TO  Ｈ負－主キー.
028550*
028560     START Ｈ負傷データＦ KEY IS >= Ｈ負－施術区分
028570                                    Ｈ負－患者コード
028580                                    Ｈ負－主キー
028590     END-START.
028600*
028610     IF 状態キー = "00"
028620         MOVE SPACE TO 終了フラグ
028630         PERFORM Ｈ負傷データＦ読込
028640         PERFORM UNTIL ( Ｈ負－施術区分    NOT = 施術区分Ｗ               ) OR
028650                       ( Ｈ負－患者コード  NOT = 連患者削除－患者コード   ) OR
028660                       ( 終了フラグ        NOT = SPACE )
028670*
028680                DELETE Ｈ負傷データＦ
028690                IF 状態キー NOT = "00"
028700                   MOVE NC"Ｈ負傷データＦ" TO ファイル名Ｗ
028710                   PERFORM エラー表示Ｄ
028720                END-IF
028730*
028740                PERFORM Ｈ負傷データＦ読込
028750         END-PERFORM
028760     END-IF.
028770*
028780*================================================================*
028790 Ｈ負傷データＦ変更 SECTION.
028800*
028810     MOVE 施術区分Ｗ              TO  Ｈ負－施術区分.
028820     MOVE 連患者削除－患者コード  TO  Ｈ負－患者コード.
028830     MOVE ZERO                    TO  Ｈ負－主キー.
028840*
028850     START Ｈ負傷データＦ KEY IS >= Ｈ負－施術区分
028860                                    Ｈ負－患者コード
028870                                    Ｈ負－主キー
028880     END-START.
028890*
028900     IF 状態キー = "00"
028910         MOVE SPACE TO 終了フラグ
028920         PERFORM Ｈ負傷データＦ読込
028930         PERFORM UNTIL ( Ｈ負－施術区分   NOT = 施術区分Ｗ             ) OR
028940                       ( Ｈ負－患者コード NOT = 連患者削除－患者コード ) OR
028950                       ( 終了フラグ       NOT = SPACE )
028960*
028970* 主キーのダブリを防ぐため、先にレコード削除してから作成する。
028980*------------   / レコードの退避 / -------------------------------*
028990                INITIALIZE 退避負－レコード
029000                MOVE Ｈ負－レコード  TO 退避負－レコード
029010*------------   / レコードの削除 / -------------------------------*
029020                DELETE Ｈ負傷データＦ
029030                IF 状態キー NOT = "00"
029040                   MOVE NC"Ｈ負傷データＦ" TO ファイル名Ｗ
029050                   PERFORM エラー表示Ｄ
029060                END-IF
029070*
029080*------------   / 新レコードの作成 / -------------------------------*
029090                MOVE SPACE TO Ｈ負－レコード
029100                INITIALIZE Ｈ負－レコード
029110                MOVE 退避負－レコード  TO Ｈ負－レコード
029120                MOVE 連患者削除－変更患者コード  TO Ｈ負－患者コード
029130*               / Ｈ負傷データＦの枝番は常にSPACE /
029140                MOVE SPACE                       TO Ｈ負－枝番
029150                WRITE Ｈ負－レコード
029160                IF 状態キー NOT = "00"
029170                   MOVE NC"Ｈ負傷データＦ" TO ファイル名Ｗ
029180                   PERFORM エラー表示
029190                END-IF
029200*
029210                PERFORM Ｈ負傷データＦ読込
029220         END-PERFORM
029230     END-IF.
029240*
029250*================================================================*
029260 Ｈ負傷データＦ読込 SECTION.
029270*
029280     READ Ｈ負傷データＦ NEXT
029290     AT END
029300         MOVE "YES"  TO 終了フラグ
029310     END-READ.
029320*
029321*================================================================*
029322*================================================================*
029323 Ｈ往療実績Ｆ全削除 SECTION.
029324*
029325     MOVE 施術区分Ｗ              TO  Ｈ往実－施術区分.
029326     MOVE 連患者削除－患者コード  TO  Ｈ往実－患者コード.
029327     MOVE ZERO                    TO  Ｈ往実－施術和暦年月日.
029332*
029333     START Ｈ往療実績Ｆ KEY IS >= Ｈ往実－施術区分
029334                                  Ｈ往実－患者コード
029335                                  Ｈ往実－施術和暦年月日
029336     END-START.
029337*
029338     IF 状態キー = "00"
029339         MOVE SPACE TO 終了フラグ
029340         PERFORM Ｈ往療実績Ｆ読込
029341         PERFORM UNTIL ( Ｈ往実－施術区分   NOT = 施術区分Ｗ             ) OR
029342                       ( Ｈ往実－患者コード NOT = 連患者削除－患者コード ) OR
029343                       ( 終了フラグ         NOT = SPACE )
029344*
029345                DELETE Ｈ往療実績Ｆ
029346                IF 状態キー NOT = "00"
029347                   MOVE NC"Ｈ往療実績Ｆ" TO ファイル名Ｗ
029348                   PERFORM エラー表示Ｄ
029349                END-IF
029350*
029351                PERFORM Ｈ往療実績Ｆ読込
029352         END-PERFORM
029353     END-IF.
029354*
029355*================================================================*
029356 Ｈ往療実績Ｆ変更 SECTION.
029357*
029358     MOVE 施術区分Ｗ              TO  Ｈ往実－施術区分.
029359     MOVE 連患者削除－患者コード  TO  Ｈ往実－患者コード.
029360     MOVE ZERO                    TO  Ｈ往実－施術和暦年月日.
029361*
029362     START Ｈ往療実績Ｆ KEY IS >= Ｈ往実－施術区分
029363                                  Ｈ往実－患者コード
029364                                  Ｈ往実－施術和暦年月日
029365     END-START.
029366*
029367     IF 状態キー = "00"
029368         MOVE SPACE TO 終了フラグ
029369         PERFORM Ｈ往療実績Ｆ読込
029370         PERFORM UNTIL ( Ｈ往実－施術区分   NOT = 施術区分Ｗ             ) OR
029371                       ( Ｈ往実－患者コード NOT = 連患者削除－患者コード ) OR
029372                       ( 終了フラグ         NOT = SPACE )
029373*
029374*------------    / 新レコードの作成 / -------------------------------*
029375                MOVE 連患者削除－変更患者コード  TO Ｈ往実－患者コード
029376                COMPUTE Ｈ往実－登録順  = Ｈ往実－登録順 + 0.1
029377                WRITE Ｈ往実－レコード
029378                IF 状態キー NOT = "00"
029379                   MOVE NC"Ｈ往療実績Ｆ" TO ファイル名Ｗ
029380                   PERFORM エラー表示
029381                END-IF
029382*
029383*------------   / レコードの削除 / -------------------------------*
029384                MOVE 連患者削除－患者コード  TO Ｈ往実－患者コード
029385                DELETE Ｈ往療実績Ｆ
029386                IF 状態キー NOT = "00"
029387                   MOVE NC"Ｈ往療実績Ｆ" TO ファイル名Ｗ
029388                   PERFORM エラー表示Ｄ
029389                END-IF
029390*
029391                PERFORM Ｈ往療実績Ｆ読込
029392         END-PERFORM
029393     END-IF.
029394*
029395*================================================================*
029396 Ｈ往療実績Ｆ読込 SECTION.
029397*
029398     READ Ｈ往療実績Ｆ NEXT
029399     AT END
029400         MOVE "YES"  TO 終了フラグ
029401     END-READ.
029402*
029403*================================================================*
029404*================================================================*
029405*================================================================*
029406 Ｈ往療予定Ｆ全削除 SECTION.
029407*
029409     MOVE 連患者削除－患者コード  TO  Ｈ往予－患者コード.
029410     MOVE ZERO                    TO  Ｈ往予－施術和暦年月日.
029411     MOVE ZERO                    TO  Ｈ往予－施術区分.
029412*
029413     START Ｈ往療予定Ｆ KEY IS >= Ｈ往予－患者コード
029415                                  Ｈ往予－施術和暦年月日
029416                                  Ｈ往予－施術区分
029420     END-START.
029421*
029422     IF 状態キー = "00"
029423         MOVE SPACE TO 終了フラグ
029424         PERFORM Ｈ往療予定Ｆ読込
029425         PERFORM UNTIL ( Ｈ往予－患者コード NOT = 連患者削除－患者コード ) OR
029427                       ( 終了フラグ         NOT = SPACE )
029428*
029429                DELETE Ｈ往療予定Ｆ
029430                IF 状態キー NOT = "00"
029431                   MOVE NC"Ｈ往療予定Ｆ" TO ファイル名Ｗ
029432                   PERFORM エラー表示Ｄ
029433                END-IF
029434*
029435                PERFORM Ｈ往療予定Ｆ読込
029436         END-PERFORM
029437     END-IF.
029438*
029439*================================================================*
029440 Ｈ往療予定Ｆ変更 SECTION.
029441*
029442     MOVE 連患者削除－患者コード  TO  Ｈ往予－患者コード.
029443     MOVE ZERO                    TO  Ｈ往予－施術和暦年月日.
029444     MOVE ZERO                    TO  Ｈ往予－施術区分.
029445*
029446     START Ｈ往療予定Ｆ KEY IS >= Ｈ往予－患者コード
029447                                  Ｈ往予－施術和暦年月日
029448                                  Ｈ往予－施術区分
029449     END-START.
029460*
029461     IF 状態キー = "00"
029462         MOVE SPACE TO 終了フラグ
029463         PERFORM Ｈ往療予定Ｆ読込
029464         PERFORM UNTIL ( Ｈ往予－患者コード NOT = 連患者削除－患者コード ) OR
029467                       ( 終了フラグ         NOT = SPACE )
029468*
029469*------------    / 新レコードの作成 / -------------------------------*
029470                MOVE 連患者削除－変更患者コード  TO Ｈ往予－患者コード
029471                COMPUTE Ｈ往予－登録順  = Ｈ往予－登録順 + 0.1
029472                WRITE Ｈ往予－レコード
029473                IF 状態キー NOT = "00"
029474                   MOVE NC"Ｈ往療予定Ｆ" TO ファイル名Ｗ
029475                   PERFORM エラー表示
029476                END-IF
029477*
029478*------------   / レコードの削除 / -------------------------------*
029479                MOVE 連患者削除－患者コード  TO Ｈ往予－患者コード
029480                DELETE Ｈ往療予定Ｆ
029481                IF 状態キー NOT = "00"
029482                   MOVE NC"Ｈ往療予定Ｆ" TO ファイル名Ｗ
029483                   PERFORM エラー表示Ｄ
029484                END-IF
029485*
029486                PERFORM Ｈ往療予定Ｆ読込
029487         END-PERFORM
029488     END-IF.
029489*
029490*================================================================*
029491 Ｈ往療予定Ｆ読込 SECTION.
029492*
029493     READ Ｈ往療予定Ｆ NEXT
029494     AT END
029495         MOVE "YES"  TO 終了フラグ
029496     END-READ.
029497*
029498*================================================================*
029499*================================================================*
029500 Ｈ往療実績Ｆ変更２ SECTION.
029502*    往療実績データ中の往療場所患者コードと先順患者コードを更新する
029503*    全件総舐めで更新
029504*
029505     MOVE SPACE                   TO  Ｈ往実－レコード.
029506     INITIALIZE                       Ｈ往実－レコード.
029508*
029509     START Ｈ往療実績Ｆ KEY IS >= Ｈ往実－施術区分
029510                                  Ｈ往実－患者コード
029511                                  Ｈ往実－施術和暦年月日
029512     END-START.
029513*
029514     IF 状態キー = "00"
029516         MOVE SPACE TO 終了フラグ
029517         PERFORM Ｈ往療実績Ｆ読込
029518         PERFORM UNTIL 終了フラグ         NOT = SPACE 
029520*
029521             MOVE SPACE TO 往療実績更新フラグ
029522*
029523             IF Ｈ往実－往療場所患者コード = 連患者削除－患者コード
029524                 MOVE 連患者削除－変更患者コード TO Ｈ往実－往療場所患者コード
029525                 MOVE "YES"                      TO 往療実績更新フラグ
029526             END-IF
029527*
029528             IF Ｈ往実－先順患者コード = 連患者削除－患者コード
029529                 MOVE 連患者削除－変更患者コード TO Ｈ往実－先順患者コード
029530                 MOVE "YES"                      TO 往療実績更新フラグ
029531             END-IF
029532*
029533             IF 往療実績更新フラグ NOT = SPACE
029534*------------    / 新レコードの更新 / -------------------------------*
029537                REWRITE Ｈ往実－レコード
029538                IF 状態キー NOT = "00"
029539                   MOVE NC"Ｈ往療実績Ｆ" TO ファイル名Ｗ
029540                   PERFORM エラー表示
029541                END-IF
029550             END-IF
029551*
029552             PERFORM Ｈ往療実績Ｆ読込
029553         END-PERFORM
029554     END-IF.
029555*
029556*================================================================*
029557*================================================================*
029558 Ｈ報告書Ｆ当月削除 SECTION.
029559*
029560     MOVE 施術区分Ｗ      TO Ｈ報－施術区分.
029561     MOVE ZERO            TO Ｈ報－用紙区分.
029562     MOVE ZERO            TO Ｈ報－施術和暦年月.
029563     MOVE ZERO            TO Ｈ報－患者番号.
029564     MOVE SPACE           TO Ｈ報－枝番.
029565     MOVE ZERO            TO Ｈ報－連番.
029566*
029567     START Ｈ報告書Ｆ KEY IS >= Ｈ報－施術区分
029568                                Ｈ報－用紙区分
029569                                Ｈ報－施術和暦年月
029570                                Ｈ報－患者コード
029571                                Ｈ報－連番
029572     END-START.
029573*
029574     IF 状態キー = "00"
029575         MOVE SPACE            TO 終了フラグ
029576         PERFORM Ｈ報告書Ｆ読込
029577         PERFORM UNTIL (終了フラグ = "YES") OR (Ｈ報－施術区分 NOT = 施術区分Ｗ) 
029578             IF (Ｈ報－施術区分      = 施術区分Ｗ) AND
029579                (Ｈ報－患者コード    = 連患者削除－患者コード) AND
029580                (Ｈ報－施術和暦年月  = 連患者削除－施術和暦年月)
029581                 DELETE Ｈ報告書Ｆ
029582                 INVALID KEY
029583                     MOVE NC"Ｈ報告" TO ファイル名Ｗ
029584                     PERFORM エラー表示Ｄ
029585                 END-DELETE
029586             END-IF
029587             PERFORM Ｈ報告書Ｆ読込
029588         END-PERFORM
029589     END-IF.
029590*
029591*================================================================*
029592 Ｈ報告書Ｆ全削除 SECTION.
029593*
029594     MOVE 施術区分Ｗ      TO Ｈ報－施術区分.
029595     MOVE ZERO            TO Ｈ報－用紙区分.
029596     MOVE ZERO            TO Ｈ報－施術和暦年月.
029597     MOVE ZERO            TO Ｈ報－患者番号.
029598     MOVE SPACE           TO Ｈ報－枝番.
029599     MOVE ZERO            TO Ｈ報－連番.
029600*
029601     START Ｈ報告書Ｆ KEY IS >= Ｈ報－施術区分
029602                                Ｈ報－用紙区分
029603                                Ｈ報－施術和暦年月
029604                                Ｈ報－患者コード
029605                                Ｈ報－連番
029606     END-START.
029607*
029608     IF 状態キー = "00"
029609         MOVE SPACE            TO 終了フラグ
029610         PERFORM Ｈ報告書Ｆ読込
029611         PERFORM UNTIL (終了フラグ = "YES") OR (Ｈ報－施術区分 NOT = 施術区分Ｗ)
029612             IF (Ｈ報－施術区分      = 施術区分Ｗ) AND
029613                (Ｈ報－患者コード    = 連患者削除－患者コード)
029614                 DELETE Ｈ報告書Ｆ
029615                 INVALID KEY
029616                     MOVE NC"Ｈ報告" TO ファイル名Ｗ
029617                     PERFORM エラー表示Ｄ
029618                 END-DELETE
029619             END-IF
029620             PERFORM Ｈ報告書Ｆ読込
029621         END-PERFORM
029622     END-IF.
029623*
029624*================================================================*
029625 Ｈ報告書Ｆ変更 SECTION.
029626*
029627     MOVE 施術区分Ｗ      TO Ｈ報－施術区分.
029628     MOVE ZERO            TO Ｈ報－用紙区分.
029629     MOVE ZERO            TO Ｈ報－施術和暦年月.
029630     MOVE ZERO            TO Ｈ報－患者番号.
029631     MOVE SPACE           TO Ｈ報－枝番.
029632     MOVE ZERO            TO Ｈ報－連番.
029633*
029634     START Ｈ報告書Ｆ KEY IS >= Ｈ報－施術区分
029635                                Ｈ報－用紙区分
029636                                Ｈ報－施術和暦年月
029637                                Ｈ報－患者コード
029638                                Ｈ報－連番
029639     END-START.
029640*
029641     IF 状態キー = "00"
029642         MOVE SPACE            TO 終了フラグ
029643         PERFORM Ｈ報告書Ｆ読込
029644         PERFORM UNTIL (終了フラグ = "YES") OR (Ｈ報－施術区分 NOT = 施術区分Ｗ)
029645             IF (Ｈ報－施術区分    = 施術区分Ｗ) AND
029646                (Ｈ報－患者コード  = 連患者削除－患者コード)
029647                 MOVE  連患者削除－変更患者コード TO Ｈ報－患者コード
029648                 WRITE Ｈ報－レコード
029649                 INVALID KEY
029650                     MOVE NC"Ｈ報告" TO ファイル名Ｗ
029651                     PERFORM エラー表示
029652                 END-WRITE
029653                 MOVE  連患者削除－患者コード TO Ｈ報－患者コード
029654                 DELETE Ｈ報告書Ｆ
029655                 INVALID KEY
029656                     MOVE NC"Ｈ報告" TO ファイル名Ｗ
029657                     PERFORM エラー表示Ｄ
029658                 END-DELETE
029659             END-IF
029660             PERFORM Ｈ報告書Ｆ読込
029661         END-PERFORM
029662     END-IF.
029663*
029704*================================================================*
029705 Ｈ報告書Ｆ読込 SECTION.
029706*
029707     READ Ｈ報告書Ｆ NEXT
029708     AT END
029709         MOVE "YES" TO 終了フラグ
029710     END-READ.
029711*================================================================*
029712*================================================================*
029713 遅延処理 SECTION.
029714*
029715     PERFORM VARYING 遅延カウンタ FROM 1 BY 1
029716             UNTIL 遅延カウンタ > 遅延回数Ｗ
029717         MOVE "YES" TO 遅延フラグ
029718     END-PERFORM.
029719*
029720*================================================================*
029721 制御情報取得 SECTION.
029722*
029723     MOVE ZERO TO 制－制御区分
029724     READ 制御情報マスタ
029725     NOT INVALID KEY
029726         MOVE 制－遅延回数             TO 遅延回数Ｗ
029727     END-READ.
029728*
029729*================================================================*
       Ｈレセプト詳細Ｆ削除 SECTION.
      *
           MOVE Ｈレセ－レコードキー TO Ｈレセ詳細－レコードキー.
027360     READ Ｈレセプト詳細Ｆ
027370     INVALID KEY
               CONTINUE
027370     NOT INVALID KEY
026532         DELETE Ｈレセプト詳細Ｆ
026533         INVALID KEY
026534            MOVE NC"Ｈレセプト詳細Ｆ" TO ファイル名Ｗ
026540            PERFORM エラー表示Ｄ
026550         END-DELETE
027390     END-READ.
029730*================================================================*
       Ｈレセプト詳細Ｆ作成 SECTION.
      *
           MOVE Ｈレセ－レコードキー TO Ｈレセ詳細－レコードキー.
027360     READ Ｈレセプト詳細Ｆ
027370     INVALID KEY
               CONTINUE
027370     NOT INVALID KEY
027140         MOVE 連患者削除－変更患者コード TO Ｈレセ詳細－患者コード
027150         WRITE Ｈレセ詳細－レコード
027160         INVALID KEY
027170            MOVE NC"Ｈレセプト詳細Ｆ" TO ファイル名Ｗ
027180            PERFORM エラー表示
027190         END-WRITE
027390     END-READ.
029730*================================================================*
029731*******************************************************************
029732 END PROGRAM CHGJUNO.
029733*******************************************************************
