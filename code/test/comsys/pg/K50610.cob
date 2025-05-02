000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             K50610.
000060 AUTHOR.                 山田 浩之
000070*
000080*------------------------------------------------------------------*
000090* 計算プログラム：令和6年10月～の対応ＰＧ
000100*
000110* 助成の特殊負担は以下のみ対応。それ以外は、負担額助成０、全額請求。
000120* 負担区分:6,8,14 (毎月一定金額、一定回数を負担)
000130*          13  (毎月一定金額、一定回数を負担（１０円単位）)
000140*
000150* 大阪丸証も対応
000160* 償還払いも対応200907
000170*------------------------------------------------------------------*
000180 DATE-WRITTEN.           2004-08-26
000190 DATE-COMPILED.          2004-08-26
000200*----------------------------------------------------------------*
000210* 2014/05/08 岡田憲和
000220* 　Ｈ制－領収書負担金計算区分に3:1円単位で月領収レセ負担額（請求明細含）を追加したので対応
000230* 2015/08/20 岡田憲和
000240* 　往療料０円対応の為、Ｈ往実－按分なし特定区分を条件判定に追加（ファイルアサインごと追加）
000250* 2016/09/01 岡田憲和
000260* 　往療料上限料金算定を修正（平成28年10月01日料金改定対応）
000270* 2017/02/27 岡田憲和
000280* 　医療助成が本体まとめの場合でも、正しく計算されるように修正（以前は強制0円だった）
000290* 2017/04/03 岡田憲和
000300* 　神奈川県の医療助成で一部負担が無い場合は本体まとめを以前仕様に戻す
000310* 2017/07/05 岡田憲和
000320* マッサージと変形徒手矯正術の重複施術制御対応
000330* 2017/08/31 岡田憲和
000340* 大阪市の障害者医療助成についてはそれぞれ５００円２回徴収する様に仕様変更
000350* 2018/01/18 岡田憲和
000360* 医療助成の計算で変形徒手の単価がおかしくなる不具合を修正
000370* 2018/08/20 岡田憲和
000371* 平成30年10月1日からの施術報告書交付料の計算を追加 (10/3 変形２回計算修正)
000370* 2019/01/10 池田幸子
000371* 平成31年1月1日からの受領委任関連の修正
000372******************************************************************
000380*            ENVIRONMENT         DIVISION                        *
000390******************************************************************
000400 ENVIRONMENT             DIVISION.
000410 CONFIGURATION           SECTION.
000420 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000430 OBJECT-COMPUTER.        FMV-DESKPOWER.
000440 SPECIAL-NAMES.          CONSOLE  IS  CONS
000450                         SYSERR   IS  MSGBOX.
000460 INPUT-OUTPUT            SECTION.
000470 FILE-CONTROL.
000480*
000490* KHT41410とのEXTERNAL用
000500     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000510                             ORGANIZATION             IS  INDEXED
000520                             ACCESS MODE              IS  DYNAMIC
000530                             RECORD KEY               IS  受－施術和暦年月
000540                                                          受－患者コード
000550                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000560                                                          受－患者カナ
000570                                                          受－患者コード
000580                             ALTERNATE RECORD KEY     IS  受－患者コード
000590                                                          受－施術和暦年月
000600                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000610                                                          受－保険種別
000620                                                          受－保険者番号
000630                                                          受－患者コード
000640                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000650                                                          受－公費種別
000660                                                          受－費用負担者番号
000670                                                          受－患者コード
000680                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000690                                                          受－助成種別
000700                                                          受－費用負担者番号助成
000710                                                          受－患者コード
000720                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
000730                                                          受－施術和暦年月
000740                                                          受－患者コード
000750                             FILE STATUS              IS  状態キー
000760                             LOCK        MODE         IS  AUTOMATIC.
000770     SELECT  保険者マスタ    ASSIGN      TO        HOKENSL
000780                             ORGANIZATION             IS  INDEXED
000790                             ACCESS MODE              IS  DYNAMIC
000800                             RECORD KEY               IS  保－保険種別
000810                                                          保－保険者番号
000820                             ALTERNATE RECORD KEY     IS  保－保険種別
000830                                                          保－保険者名称
000840                                                          保－保険者番号
000850                             FILE STATUS              IS  状態キー
000860                             LOCK        MODE         IS  AUTOMATIC.
000870     SELECT  負担率マスタ    ASSIGN      TO        HUTANRIL
000880                             ORGANIZATION             IS  INDEXED
000890                             ACCESS MODE              IS  DYNAMIC
000900                             RECORD KEY               IS  負率－保険種別
000910                                                          負率－開始和暦年月
000920                             FILE STATUS              IS  状態キー
000930                             LOCK        MODE         IS  AUTOMATIC.
000940     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
000950                             ORGANIZATION             IS  INDEXED
000960                             ACCESS MODE              IS  DYNAMIC
000970                             RECORD KEY               IS  市－公費種別
000980                                                          市－市町村番号
000990                             ALTERNATE RECORD KEY     IS  市－公費種別
001000                                                          市－市町村名称
001010                                                          市－市町村番号
001020                             FILE STATUS              IS  状態キー
001030                             LOCK        MODE         IS  AUTOMATIC.
001040*
001050     SELECT  保険者特別負担マスタ   ASSIGN      TO     HOKENTKL
001060                             ORGANIZATION          IS  INDEXED
001070                             ACCESS MODE           IS  DYNAMIC
001080                             RECORD KEY            IS  保特－保険種別
001090                                                       保特－保険者番号
001100                                                       保特－開始和暦年月
001110                             FILE STATUS           IS  状態キー
001120                             LOCK        MODE      IS  AUTOMATIC.
001130*
001140*-----------------------------------------------------------------------------*
001150     SELECT  Ｈ料金マスタ    ASSIGN      TO        HRYOKINL
001160                             ORGANIZATION             IS  INDEXED
001170                             ACCESS MODE              IS  DYNAMIC
001180                             RECORD KEY               IS  Ｈ料１－区分コード
001190                                                          Ｈ料１－開始和暦年月
001200                             FILE STATUS              IS  状態キー
001210                             LOCK        MODE         IS  AUTOMATIC.
001220*
001230     SELECT  ＨレセプトＦ    ASSIGN      TO        HRECEL
001240                             ORGANIZATION             IS  INDEXED
001250                             ACCESS MODE              IS  DYNAMIC
001260                             RECORD KEY               IS  Ｈレセ－施術区分
001270                                                          Ｈレセ－施術和暦年月
001280                                                          Ｈレセ－患者コード
001290                                                          Ｈレセ－レセ種別
001300                             ALTERNATE RECORD KEY     IS  Ｈレセ－施術区分
001310                                                          Ｈレセ－患者コード
001320                                                          Ｈレセ－施術和暦年月
001330                                                          Ｈレセ－レセ種別
001340                             ALTERNATE RECORD KEY     IS  Ｈレセ－施術和暦年月
001350                                                          Ｈレセ－患者コード
001360                                                          Ｈレセ－レセ種別
001370                                                          Ｈレセ－施術区分
001380                             ALTERNATE RECORD KEY     IS  Ｈレセ－請求対象区分
001390                                                          Ｈレセ－請求和暦年月
001400                                                          Ｈレセ－施術区分
001410                                                          Ｈレセ－施術和暦年月
001420                                                          Ｈレセ－患者コード
001430                                                          Ｈレセ－レセ種別
001440                             FILE STATUS              IS  状態キー
001450                             LOCK        MODE         IS  AUTOMATIC.
001460*
001470*     SELECT  Ｈ負傷データＦ  ASSIGN      TO        HHUSYOUL
001480*                             ORGANIZATION             IS  INDEXED
001490*                             ACCESS MODE              IS  DYNAMIC
001500*                             RECORD KEY               IS  Ｈ負－主キー
001510*                             ALTERNATE RECORD KEY     IS  Ｈ負－施術区分
001520*                                                          Ｈ負－患者コード
001530*                                                          Ｈ負－主キー
001540*                             FILE STATUS              IS  状態キー
001550*                             LOCK        MODE         IS  AUTOMATIC.
001560*
001570     SELECT  Ｈ日計データＦ  ASSIGN      TO        HNIKEIL
001580                             ORGANIZATION             IS  INDEXED
001590                             ACCESS MODE              IS  DYNAMIC
001600                             RECORD KEY               IS  Ｈ日－施術区分
001610                                                          Ｈ日－施術和暦年月日
001620                                                          Ｈ日－患者コード
001630                             ALTERNATE RECORD KEY     IS  Ｈ日－施術区分
001640                                                          Ｈ日－患者コード
001650                                                          Ｈ日－施術和暦年月日
001660                             ALTERNATE RECORD KEY     IS  Ｈ日－施術和暦年月日
001670                                                          Ｈ日－登録順
001680                             FILE STATUS              IS  状態キー
001690                             LOCK        MODE         IS  AUTOMATIC.
001700*
001710     SELECT  参照Ｈ日計データＦ  ASSIGN      TO        HNIKEIL
001720                             ORGANIZATION             IS  INDEXED
001730                             ACCESS MODE              IS  DYNAMIC
001740                             RECORD KEY               IS  参照Ｈ日－施術区分
001750                                                          参照Ｈ日－施術和暦年月日
001760                                                          参照Ｈ日－患者コード
001770                             ALTERNATE RECORD KEY     IS  参照Ｈ日－施術区分
001780                                                          参照Ｈ日－患者コード
001790                                                          参照Ｈ日－施術和暦年月日
001800                             ALTERNATE RECORD KEY     IS  参照Ｈ日－施術和暦年月日
001810                                                          参照Ｈ日－登録順
001820                             FILE STATUS              IS  状態キー
001830                             LOCK        MODE         IS  AUTOMATIC.
001840*
001850     SELECT  会計領収Ｆ      ASSIGN      TO        RYOSYUL
001860                             ORGANIZATION             IS  INDEXED
001870                             ACCESS MODE              IS  DYNAMIC
001880                             RECORD KEY               IS  領－施術区分
001890                                                          領－会計領収区分
001900                                                          領－施術和暦年月日
001910                                                          領－患者コード
001920                             ALTERNATE RECORD KEY     IS  領－施術区分
001930                                                          領－患者コード
001940                                                          領－会計領収区分
001950                                                          領－施術和暦年月日
001960                             FILE STATUS              IS  状態キー
001970                             LOCK        MODE         IS  AUTOMATIC.
001980*
001990     SELECT  Ｈ制御情報マスタ  ASSIGN      TO      HSEIGYOL
002000                               ORGANIZATION             IS  INDEXED
002010                               ACCESS MODE              IS  DYNAMIC
002020                               RECORD KEY               IS  Ｈ制－制御区分
002030                               FILE STATUS              IS  状態キー
002040                               LOCK        MODE         IS  AUTOMATIC.
002050*
002060     SELECT  参照受診者情報Ｆ    ASSIGN      TO        JUSINJL
002070                             ORGANIZATION             IS  INDEXED
002080                             ACCESS MODE              IS  DYNAMIC
002090                             RECORD KEY               IS  参照受－施術和暦年月
002100                                                          参照受－患者コード
002110                             ALTERNATE RECORD KEY     IS  参照受－施術和暦年月
002120                                                          参照受－患者カナ
002130                                                          参照受－患者コード
002140                             ALTERNATE RECORD KEY     IS  参照受－患者コード
002150                                                          参照受－施術和暦年月
002160                             ALTERNATE RECORD KEY     IS  参照受－施術和暦年月
002170                                                          参照受－保険種別
002180                                                          参照受－保険者番号
002190                                                          参照受－患者コード
002200                             ALTERNATE RECORD KEY     IS  参照受－施術和暦年月
002210                                                          参照受－公費種別
002220                                                          参照受－費用負担者番号
002230                                                          参照受－患者コード
002240                             ALTERNATE RECORD KEY     IS  参照受－施術和暦年月
002250                                                          参照受－助成種別
002260                                                          参照受－費用負担者番号助成
002270                                                          参照受－患者コード
002280                             ALTERNATE RECORD KEY     IS  参照受－請求和暦年月
002290                                                          参照受－施術和暦年月
002300                                                          参照受－患者コード
002310                             FILE STATUS              IS  状態キー
002320                             LOCK        MODE         IS  AUTOMATIC.
002330**
002340     SELECT  Ｈ往療実績Ｆ    ASSIGN      TO        HNOURYOL
002350                             ORGANIZATION             IS  INDEXED
002360                             ACCESS MODE              IS  DYNAMIC
002370                             RECORD KEY               IS  Ｈ往実－施術区分
002380                                                          Ｈ往実－施術和暦年月日
002390                                                          Ｈ往実－患者コード
002400                             ALTERNATE RECORD KEY     IS  Ｈ往実－施術区分
002410                                                          Ｈ往実－患者コード
002420                                                          Ｈ往実－施術和暦年月日
002430                             ALTERNATE RECORD KEY     IS  Ｈ往実－施術和暦年月日
002440                                                          Ｈ往実－施術者番号
002450                                                          Ｈ往実－登録順
002460                             ALTERNATE RECORD KEY     IS  Ｈ往実－施術和暦年月日
002470                                                          Ｈ往実－施術開始時間
002480                                                          Ｈ往実－施術者番号
002490                                                          Ｈ往実－登録順
002500                             FILE STATUS              IS  状態キー
002510                             LOCK        MODE         IS  AUTOMATIC.
000620     SELECT  Ｈ施術所情報マスタ ASSIGN      TO     HSEJOHOL
000630                                ORGANIZATION             IS  INDEXED
000640                                ACCESS MODE              IS  DYNAMIC
000650                                RECORD KEY               IS  Ｈ施情－施術所番号
000660                                FILE STATUS              IS  状態キー
000670                                LOCK        MODE         IS  AUTOMATIC.
002520*
002530     SELECT  更新ＨレセプトＦ    ASSIGN      TO        HRECEL
002540                             ORGANIZATION             IS  INDEXED
002550                             ACCESS MODE              IS  DYNAMIC
002560                             RECORD KEY               IS  更新Ｈレセ－施術区分
002570                                                          更新Ｈレセ－施術和暦年月
002580                                                          更新Ｈレセ－患者コード
002590                                                          更新Ｈレセ－レセ種別
002600                             ALTERNATE RECORD KEY     IS  更新Ｈレセ－施術区分
002610                                                          更新Ｈレセ－患者コード
002620                                                          更新Ｈレセ－施術和暦年月
002630                                                          更新Ｈレセ－レセ種別
002640                             ALTERNATE RECORD KEY     IS  更新Ｈレセ－施術和暦年月
002650                                                          更新Ｈレセ－患者コード
002660                                                          更新Ｈレセ－レセ種別
002670                                                          更新Ｈレセ－施術区分
002680                             ALTERNATE RECORD KEY     IS  更新Ｈレセ－請求対象区分
002690                                                          更新Ｈレセ－請求和暦年月
002700                                                          更新Ｈレセ－施術区分
002710                                                          更新Ｈレセ－施術和暦年月
002720                                                          更新Ｈレセ－患者コード
002730                                                          更新Ｈレセ－レセ種別
002740                             FILE STATUS              IS  状態キー
002750                             LOCK        MODE         IS  AUTOMATIC.
002760*
000140     SELECT  Ｈレセプト詳細Ｆ    ASSIGN      TO       HRECEHKL
000150                             ORGANIZATION             IS  INDEXED
000160                             ACCESS MODE              IS  DYNAMIC
000170                             RECORD KEY               IS  Ｈレセ詳細－施術区分
000180                                                          Ｈレセ詳細－施術和暦年月
000190                                                          Ｈレセ詳細－患者コード
000200                                                          Ｈレセ詳細－レセ種別
000350                             FILE STATUS              IS  状態キー
000360                             LOCK        MODE         IS  AUTOMATIC.
000101*
000102     SELECT  施設マスタ         ASSIGN      TO       SISETUL
000103                                ORGANIZATION             IS  INDEXED
000104                                ACCESS MODE              IS  DYNAMIC
000105                                RECORD KEY               IS  施設－施設コード
000111                                FILE STATUS              IS  状態キー
000112                                LOCK        MODE         IS  AUTOMATIC.
000113
000370*
001360     SELECT  作業ファイル１  ASSIGN      TO        "C:\MAKISHISYS\hmobj\TEMP\W50610L.DAT"
001370                             ORGANIZATION             IS  INDEXED
001380                             ACCESS                   IS  DYNAMIC
001390                             RECORD      KEY          IS  作１－施術区分
000180                                                          作１－施術和暦年月日
000190                                                          作１－患者コード
001490                             FILE        STATUS       IS  状態キー
001500                             LOCK        MODE         IS  AUTOMATIC.
000370*
001360     SELECT  作業ファイル２  ASSIGN      TO        "C:\MAKISHISYS\hmobj\TEMP\W506102L.DAT"
001370                             ORGANIZATION             IS  INDEXED
001380                             ACCESS                   IS  DYNAMIC
001390                             RECORD      KEY          IS  作２－施術和暦年月日
000190                                                          作２－患者コード
001490                             FILE        STATUS       IS  状態キー
001500                             LOCK        MODE         IS  AUTOMATIC.
002340     SELECT  更新用Ｈ往療実績Ｆ    ASSIGN      TO        HNOURYOL
002350                             ORGANIZATION             IS  INDEXED
002360                             ACCESS MODE              IS  DYNAMIC
002370                             RECORD KEY               IS  更Ｈ往実－施術区分
002380                                                          更Ｈ往実－施術和暦年月日
002390                                                          更Ｈ往実－患者コード
002400                             ALTERNATE RECORD KEY     IS  更Ｈ往実－施術区分
002410                                                          更Ｈ往実－患者コード
002420                                                          更Ｈ往実－施術和暦年月日
002430                             ALTERNATE RECORD KEY     IS  更Ｈ往実－施術和暦年月日
002440                                                          更Ｈ往実－施術者番号
002450                                                          更Ｈ往実－登録順
002460                             ALTERNATE RECORD KEY     IS  更Ｈ往実－施術和暦年月日
002470                                                          更Ｈ往実－施術開始時間
002480                                                          更Ｈ往実－施術者番号
002490                                                          更Ｈ往実－登録順
002500                             FILE STATUS              IS  状態キー
002510                             LOCK        MODE         IS  AUTOMATIC.
002760*
000140     SELECT  参照Ｈレセプト詳細Ｆ    ASSIGN      TO       HRECEHKL
000150                             ORGANIZATION             IS  INDEXED
000160                             ACCESS MODE              IS  DYNAMIC
000170                             RECORD KEY               IS  参Ｈレセ詳細－施術区分
000180                                                          参Ｈレセ詳細－施術和暦年月
000190                                                          参Ｈレセ詳細－患者コード
000200                                                          参Ｈレセ詳細－レセ種別
000350                             FILE STATUS              IS  状態キー
000360                             LOCK        MODE         IS  AUTOMATIC.
000101*
001160     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
001170                             ORGANIZATION             IS  INDEXED
001180                             ACCESS MODE              IS  DYNAMIC
001190                             RECORD KEY               IS  元－元号区分
001200                             FILE STATUS              IS  状態キー
001210                             LOCK        MODE         IS  AUTOMATIC.
002770******************************************************************
002780*                      DATA DIVISION                             *
002790******************************************************************
002800 DATA                    DIVISION.
002810 FILE                    SECTION.
002820*
002830*-----------------------------------------------------------------------*
002840** EXTERNAL用
002850 FD  受診者情報Ｆ IS EXTERNAL   BLOCK   CONTAINS   1   RECORDS.
002860     COPY JUSINJ     OF  XFDLIB  JOINING   受   AS  PREFIX.
002870 FD  保険者マスタ IS EXTERNAL   BLOCK   CONTAINS   1   RECORDS.
002880     COPY HOKENS     OF  XFDLIB  JOINING   保   AS  PREFIX.
002890 FD  負担率マスタ IS EXTERNAL   BLOCK   CONTAINS   1   RECORDS.
002900     COPY HUTANRI    OF  XFDLIB  JOINING   負率 AS  PREFIX.
002910 FD  市町村マスタ IS EXTERNAL   BLOCK   CONTAINS   1   RECORDS.
002920     COPY SITYOSN    OF  XFDLIB  JOINING   市   AS  PREFIX.
002930 FD  保険者特別負担マスタ  IS EXTERNAL BLOCK   CONTAINS   1   RECORDS.
002940     COPY HOKENTK    OF  XFDLIB  JOINING   保特 AS PREFIX.
002950*
002960*-----------------------------------------------------------------------//
002970*                           ［ＲＬ＝  ３８４］
002980 FD  Ｈ料金マスタ        BLOCK   CONTAINS   1   RECORDS.
002990     COPY H_RYOKIN1  OF  XFDLIB  JOINING   Ｈ料１   AS  PREFIX.
003000*                           ［ＲＬ＝  ７６８］
003010 FD  ＨレセプトＦ        BLOCK   CONTAINS   1   RECORDS.
003020     COPY H_RECE     OF  XFDLIB  JOINING   Ｈレセ  AS  PREFIX.
003030*                           ［ＲＬ＝  ６４０］
003040* FD  Ｈ負傷データＦ     BLOCK   CONTAINS   1   RECORDS.
003050*     COPY H_HUSYOU  OF  XFDLIB  JOINING   Ｈ負 AS  PREFIX.
003060*                           ［ＲＬ＝  ５１２］
003070 FD  Ｈ日計データＦ      BLOCK   CONTAINS   1   RECORDS.
003080     COPY H_NIKEI    OF  XFDLIB  JOINING   Ｈ日   AS  PREFIX.
003090*
003100 FD  参照Ｈ日計データＦ  BLOCK   CONTAINS   1   RECORDS.
003110     COPY H_NIKEI    OF  XFDLIB  JOINING   参照Ｈ日   AS  PREFIX.
003120*
003130 FD  会計領収Ｆ          BLOCK   CONTAINS   1   RECORDS.
003140     COPY RYOSYU     OF  XFDLIB  JOINING   領  AS  PREFIX.
003150*
003160 FD  Ｈ制御情報マスタ    BLOCK   CONTAINS   1   RECORDS.
003170     COPY H_SEIGYO   OF  XFDLIB  JOINING   Ｈ制   AS  PREFIX.
003180*
003190 FD  参照受診者情報Ｆ    BLOCK   CONTAINS   1   RECORDS.
003200     COPY JUSINJ     OF  XFDLIB  JOINING   参照受   AS  PREFIX.
003210*
003220 FD  Ｈ往療実績Ｆ        BLOCK   CONTAINS   1   RECORDS.
003230     COPY H_NOURYO   OF  XFDLIB  JOINING   Ｈ往実   AS  PREFIX.
001780*                           ［ＲＬ＝  1280］
001790 FD  Ｈ施術所情報マスタ  BLOCK   CONTAINS   1   RECORDS.
001800     COPY H_SEJOHO   OF  XFDLIB  JOINING   Ｈ施情   AS  PREFIX.
           COPY H_SEJOHO41 OF  XFDLIB  JOINING   Ｈ施情４１   AS  PREFIX.
003240*                           ［ＲＬ＝  ７６８］
003250 FD  更新ＨレセプトＦ    BLOCK   CONTAINS   1   RECORDS.
003260     COPY H_RECE     OF  XFDLIB  JOINING   更新Ｈレセ  AS  PREFIX.
      *
000380 FD  Ｈレセプト詳細Ｆ        BLOCK   CONTAINS   1   RECORDS.
000390     COPY H_RECEHK    OF  XFDLIB  JOINING   Ｈレセ詳細  AS  PREFIX.
000113*
000114 FD  施設マスタ  BLOCK   CONTAINS   1   RECORDS.
000115     COPY SISETU    OF  XFDLIB  JOINING   施設   AS  PREFIX.
001350*
001870 FD  作業ファイル１ RECORD  CONTAINS 32 CHARACTERS.
001880 01  作１－レコード.
001890   03  作１－レコードキー.
000450* 1:鍼灸、2:あんま・マッサージ
000460     05 作１－施術区分                       PIC 9.
000470*-----------------------------------------------*
000390     05 作１－施術和暦年月日.
000480        07 作１－施術和暦年月.
000490           09 作１－施術和暦                 PIC 9.
000500           09 作１－施術年月.
000510              11 作１－施術年                PIC 9(2).
000520              11 作１－施術月                PIC 9(2).
000450        07 作１－施術日                      PIC 9(2).
000530     05 作１－患者コード.
000540        07 作１－患者番号                    PIC 9(6).
000550        07 作１－枝番                        PIC X.
003270*---------------------------------------------------------------*
001350*
001870 FD  作業ファイル２ RECORD  CONTAINS 32 CHARACTERS.
001880 01  作２－レコード.
001890   03  作２－レコードキー.
000470*-----------------------------------------------*
000390     05 作２－施術和暦年月日.
000480        07 作２－施術和暦年月.
000490           09 作２－施術和暦                 PIC 9.
000500           09 作２－施術年月.
000510              11 作２－施術年                PIC 9(2).
000520              11 作２－施術月                PIC 9(2).
000450        07 作２－施術日                      PIC 9(2).
000530     05 作２－患者コード.
000540        07 作２－患者番号                    PIC 9(6).
000550        07 作２－枝番                        PIC X.
003270*---------------------------------------------------------------*
003210*
003220 FD  更新用Ｈ往療実績Ｆ        BLOCK   CONTAINS   1   RECORDS.
003230     COPY H_NOURYO   OF  XFDLIB  JOINING   更Ｈ往実   AS  PREFIX.
      *
000380 FD  参照Ｈレセプト詳細Ｆ        BLOCK   CONTAINS   1   RECORDS.
000390     COPY H_RECEHK    OF  XFDLIB  JOINING   参Ｈレセ詳細  AS  PREFIX.
000113*
002200*                           ［ＲＬ＝  １２８］
002210 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
002220     COPY GENGOU     OF  XFDLIB  JOINING   元   AS  PREFIX.
003280******************************************************************
003290*                WORKING-STORAGE SECTION                         *
003300******************************************************************
003310 WORKING-STORAGE         SECTION.
003320 01 キー入力                           PIC X    VALUE SPACE.
003330 01 状態キー                           PIC X(2) VALUE SPACE.
003340 01 終了フラグ                         PIC X(3) VALUE SPACE.
003350 01 終了フラグ２                       PIC X(3) VALUE SPACE.
003360 01 終了フラグ３                       PIC X(3) VALUE SPACE.
003360 01 終了フラグ４                       PIC X(3) VALUE SPACE.
003360 01 終了フラグ５                       PIC X(3) VALUE SPACE.
003370 01 ファイル名                         PIC N(10) VALUE SPACE.
003380 01 会計領収無しフラグ                 PIC X(3) VALUE SPACE.
       01 受領委任Ｆ                         PIC 9(1) VALUE ZERO.
       01 施術者受領委任Ｆ                   PIC 9(1) VALUE ZERO.
003390*
003400 01 往療基本距離                       PIC 9(2)V9 VALUE ZERO.
003410*
003420* 日計の計算の単位（0:10円単位、1:1円単位、2:1円単位で月の連続領収書のみレセプト負担額と同額）
003430 01 領収書負担金計算区分Ｗ             PIC 9 VALUE ZERO.
003440*
003450*--------------------------------------------------------------*
003460**
003470 01 料金情報Ｗ.
003480     03 はりきゅうデータＷ.
003490       05 Ｈ１術初回Ｗ                        PIC 9(5).
003500       05 Ｈ２術初回Ｗ                        PIC 9(5).
003510       05 Ｈ１術Ｗ                            PIC 9(5).
003520       05 Ｈ２術Ｗ                            PIC 9(5).
003530       05 Ｈ電療料Ｗ                          PIC 9(4).
003540       05 Ｈ往療料Ｗ.
003550         07 Ｈ往療基本料金Ｗ                  PIC 9(4).
003560         07 Ｈ往療追加料金Ｗ                  PIC 9(4).
      */令和6年10月以降↓↓↓/*20240618-----------------------------------
000350*追加
000331       05 Ｈ１術訪問施術料１Ｗ                PIC 9(5) VALUE ZERO.
000332       05 Ｈ２術訪問施術料１Ｗ                PIC 9(5) VALUE ZERO.
000331       05 Ｈ１術訪問施術料２Ｗ                PIC 9(5) VALUE ZERO.
000332       05 Ｈ２術訪問施術料２Ｗ                PIC 9(5) VALUE ZERO.
000331       05 Ｈ１術訪問施術料３Ｗ                PIC 9(5) VALUE ZERO.
000332       05 Ｈ２術訪問施術料３Ｗ                PIC 9(5) VALUE ZERO.
000331       05 Ｈ１術訪問施術料４Ｗ                PIC 9(5) VALUE ZERO.
000332       05 Ｈ２術訪問施術料４Ｗ                PIC 9(5) VALUE ZERO.
             05 １術初険料Ｗ                        PIC 9(5) VALUE ZERO.
             05 ２術初険料Ｗ                        PIC 9(5) VALUE ZERO.
      */令和6年10月以降↑↑↑/*20240618-----------------------------------
      *
003570     03 マッサージデータＷ.
003580       05 Ｍ１局所Ｗ                          PIC 9(5).
003590       05 Ｍ変形徒手矯正術Ｗ                  PIC 9(5).
003600       05 Ｍ温罨法料Ｗ                        PIC 9(4).
003610       05 Ｍ電療料Ｗ                          PIC 9(4).
003620       05 Ｍ往療料Ｗ.
003630         07 Ｍ往療基本料金Ｗ                  PIC 9(4).
003640         07 Ｍ往療追加料金Ｗ                  PIC 9(4).
      */令和6年10月以降↓↓↓/*20240621-----------------------------------
000350*追加
000339         05  Ｍ１局所施術料Ｗ                 PIC 9(4) VALUE ZERO.
000339         05  Ｍ２局所施術料Ｗ                 PIC 9(4) VALUE ZERO.
000339         05  Ｍ３局所施術料Ｗ                 PIC 9(4) VALUE ZERO.
000339         05  Ｍ４局所施術料Ｗ                 PIC 9(4) VALUE ZERO.
000339         05  Ｍ５局所施術料Ｗ                 PIC 9(4) VALUE ZERO.
000339         05  Ｍ１局所訪問施術料１Ｗ           PIC 9(4) VALUE ZERO.
000339         05  Ｍ２局所訪問施術料１Ｗ           PIC 9(4) VALUE ZERO.
000339         05  Ｍ３局所訪問施術料１Ｗ           PIC 9(4) VALUE ZERO.
000339         05  Ｍ４局所訪問施術料１Ｗ           PIC 9(4) VALUE ZERO.
000339         05  Ｍ５局所訪問施術料１Ｗ           PIC 9(4) VALUE ZERO.
000339         05  Ｍ１局所訪問施術料２Ｗ           PIC 9(4) VALUE ZERO.
000339         05  Ｍ２局所訪問施術料２Ｗ           PIC 9(4) VALUE ZERO.
000339         05  Ｍ３局所訪問施術料２Ｗ           PIC 9(4) VALUE ZERO.
000339         05  Ｍ４局所訪問施術料２Ｗ           PIC 9(4) VALUE ZERO.
000339         05  Ｍ５局所訪問施術料２Ｗ           PIC 9(4) VALUE ZERO.
000339         05  Ｍ１局所訪問施術料３Ｗ           PIC 9(4) VALUE ZERO.
000339         05  Ｍ２局所訪問施術料３Ｗ           PIC 9(4) VALUE ZERO.
000339         05  Ｍ３局所訪問施術料３Ｗ           PIC 9(4) VALUE ZERO.
000339         05  Ｍ４局所訪問施術料３Ｗ           PIC 9(4) VALUE ZERO.
000339         05  Ｍ５局所訪問施術料３Ｗ           PIC 9(4) VALUE ZERO.
000339         05  Ｍ１局所訪問施術料４Ｗ           PIC 9(4) VALUE ZERO.
000339         05  Ｍ２局所訪問施術料４Ｗ           PIC 9(4) VALUE ZERO.
000339         05  Ｍ３局所訪問施術料４Ｗ           PIC 9(4) VALUE ZERO.
000339         05  Ｍ４局所訪問施術料４Ｗ           PIC 9(4) VALUE ZERO.
000339         05  Ｍ５局所訪問施術料４Ｗ           PIC 9(4) VALUE ZERO.
      */令和6年10月以降↑↑↑/*20240621-----------------------------------
003651     03 共通データＷ.
003652       05 施術報告書交付料単価Ｗ              PIC 9(4).
002831       05 鍼灸特別地域区分Ｗ                  PIC X VALUE SPACE.
000382       03 料Ｍ特別地域加算料Ｗ                PIC 9(4) VALUE ZERO.
003653*
003660*---------------------------------------------------------------------*
003670 01 計算情報Ｗ.
003680     03 はり合計額Ｗ          PIC 9(6).
003690     03 はり術額Ｗ            PIC 9(5).
003700     03 はり電療料Ｗ          PIC 9(5).
003710     03 はり往療料Ｗ          PIC 9(5).
003720*
003730     03 マッサージ合計額Ｗ          PIC 9(6).
003740     03 マッサージ料Ｗ              PIC 9(5).
003750     03 マッサージ変形徒手矯正術Ｗ  PIC 9(5).
003760     03 マッサージ温罨法料Ｗ        PIC 9(5).
003770     03 マッサージ温罨法電療料Ｗ    PIC 9(5).
003780     03 マッサージ往療料Ｗ          PIC 9(5).
003790*
003800     03 Ｊマッサージ合計額Ｗ          PIC 9(6).
003810     03 Ｊマッサージ料Ｗ              PIC 9(6).
003820     03 Ｊマッサージ変形徒手矯正術Ｗ  PIC 9(6).
003830     03 Ｊマッサージ温罨法料Ｗ        PIC 9(6).
003840     03 Ｊマッサージ温罨法電療料Ｗ    PIC 9(6).
003850     03 Ｊマッサージ往療料Ｗ          PIC 9(5).
003860*
003870*    往療関連
003880     03 往療被除数Ｗ          PIC 9(3).
003890     03 往療除数Ｗ            PIC 9(3).
003900     03 往療商Ｗ              PIC 9(3).
003910     03 往療剰余Ｗ            PIC 9(3).
003920*
003930*---------------------------------------------------------------------*
003940*
003950 01 累計情報Ｗ.
003960     03 はり初回施術内容区分Ｗ PIC 9(2).
003970     03 はり初回料Ｗ           PIC 9(6).
003980     03 はり回数Ｗ             PIC 9(3).
003990     03 はり電気回数Ｗ         PIC 9(3).
004000     03 きゅう回数Ｗ           PIC 9(3).
004010     03 きゅう電気回数Ｗ       PIC 9(3).
004020     03 はりきゅう回数Ｗ       PIC 9(3).
004030     03 はりきゅう電気回数Ｗ   PIC 9(3).
004040     03 はり往療回数Ｗ         PIC 9(3).
004050     03 はり往療加算回数Ｗ     PIC 9(3).
004060     03 はり往療加算単価Ｗ     PIC 9(5).
004070     03 はり往療加算料Ｗ       PIC 9(6).
004080     03 はり累計往療料Ｗ       PIC 9(6).
004090     03 はり累計額Ｗ           PIC 9(6).
004100*
004110     03 マッサージ回数Ｗ                 PIC 9(3).
004120     03 マッサージ変形徒手矯正術回数Ｗ   PIC 9(3).
004130     03 マッサージ温罨法回数Ｗ           PIC 9(3).
004140     03 マッサージ温罨法電気回数Ｗ       PIC 9(3).
004150     03 マッサージ往療回数Ｗ             PIC 9(3).
004160     03 マッサージ往療加算回数Ｗ         PIC 9(3).
004170     03 マッサージ往療加算単価Ｗ         PIC 9(5).
004180     03 マッサージ往療加算料Ｗ           PIC 9(6).
004190     03 マッサージ累計往療料Ｗ           PIC 9(6).
004200     03 マッサージ累計額Ｗ               PIC 9(6).
004210*
004220* 往療加算複数用
004230     03 退避往療距離Ｗ                   PIC 9(2)V9 VALUE ZERO.
004240     03 往療加算複数判定Ｗ               PIC 9 VALUE ZERO.
004250*   特殊施術の有無
004260     03 特殊施術フラグＷ                 PIC 9 VALUE ZERO.
004270*
004280*    施術報告書交付料.
004281     03 はり報告書Ｗ.
004282        05 はり報告書交付回数Ｗ          PIC 9(2) VALUE ZERO.
004283        05 はり報告書交付料Ｗ            PIC 9(6) VALUE ZERO.
004284     03 マッサージ報告書Ｗ.
004285        05 マッサージ報告書交付回数Ｗ    PIC 9(2) VALUE ZERO.
004286        05 マッサージ報告書交付料Ｗ      PIC 9(6) VALUE ZERO.
      *
004287***
004288***上記の累計情報Ｗと、同じワークを定義。
004290 01 Ｊ累計情報Ｗ.
004300     03 Ｊはり初回施術内容区分Ｗ PIC 9(2).
004310     03 Ｊはり初回料Ｗ           PIC 9(6).
004320     03 Ｊはり回数Ｗ             PIC 9(3).
004330     03 Ｊはり電気回数Ｗ         PIC 9(3).
004340     03 Ｊきゅう回数Ｗ           PIC 9(3).
004350     03 Ｊきゅう電気回数Ｗ       PIC 9(3).
004360     03 Ｊはりきゅう回数Ｗ       PIC 9(3).
004370     03 Ｊはりきゅう電気回数Ｗ   PIC 9(3).
004380     03 Ｊはり往療回数Ｗ         PIC 9(3).
004390     03 Ｊはり往療加算回数Ｗ     PIC 9(3).
004400     03 Ｊはり往療加算単価Ｗ     PIC 9(5).
004410     03 Ｊはり往療加算料Ｗ       PIC 9(6).
004420     03 Ｊはり累計往療料Ｗ       PIC 9(6).
004430     03 Ｊはり累計額Ｗ           PIC 9(6).
004440*
004450     03 Ｊマッサージ回数Ｗ                 PIC 9(3).
004460     03 Ｊマッサージ変形徒手矯正術回数Ｗ   PIC 9(3).
004470     03 Ｊマッサージ温罨法回数Ｗ           PIC 9(3).
004480     03 Ｊマッサージ温罨法電気回数Ｗ       PIC 9(3).
004490     03 Ｊマッサージ往療回数Ｗ             PIC 9(3).
004500     03 Ｊマッサージ往療加算回数Ｗ         PIC 9(3).
004510     03 Ｊマッサージ往療加算単価Ｗ         PIC 9(5).
004520     03 Ｊマッサージ往療加算料Ｗ           PIC 9(6).
004530     03 Ｊマッサージ累計往療料Ｗ           PIC 9(6).
004540     03 Ｊマッサージ累計額Ｗ               PIC 9(6).
004550*
004560* 往療加算複数用
004570     03 Ｊ退避往療距離Ｗ                   PIC 9(2)V9 VALUE ZERO.
004580     03 Ｊ往療加算複数判定Ｗ               PIC 9 VALUE ZERO.
004590*   特殊施術の有無
004600     03 Ｊ特殊施術フラグＷ                 PIC 9 VALUE ZERO.
004610*
004632*    施術報告書交付料.
004633     03 Ｊはり報告書Ｗ.
004634        05 Ｊはり報告書交付回数Ｗ          PIC 9(2) VALUE ZERO.
004635        05 Ｊはり報告書交付料Ｗ            PIC 9(6) VALUE ZERO.
004636     03 Ｊマッサージ報告書Ｗ.
004637        05 Ｊマッサージ報告書交付回数Ｗ    PIC 9(2) VALUE ZERO.
004638        05 Ｊマッサージ報告書交付料Ｗ      PIC 9(6) VALUE ZERO.
004639*---------------------------------------------------------------------*
004640*---------------------------------------------------------------------*
004641
004650* 一般ワーク
004660 01 マッサージ固有Ｗ.
004670     03 マッサージ局所数Ｗ          PIC 9.
004680     03 変形徒手矯正術数Ｗ          PIC 9.
004690     03 マッサージ局所数ＰＷ        PIC 9.
004700     03 変形徒手矯正術数ＰＷ        PIC 9.
004710     03 マッサージ局所数セットＷ    PIC 9.
004720     03 変形徒手矯正術数セットＷ    PIC 9.
004730     03 マッサージ局所数日計Ｗ      PIC 9.
004740     03 変形徒手矯正術数日計Ｗ      PIC 9.
004750     03 マッサージ局所数複数Ｗ      PIC 9.
004760     03 変形徒手矯正術数複数Ｗ      PIC 9.
004770     03 マッサージ料月計Ｗ          PIC 9(7).
004780     03 変形徒手料月計Ｗ            PIC 9(7).
004790*
004800 01 Ｊマッサージ固有Ｗ.
004810     03 Ｊマッサージ局所数Ｗ        PIC 9.
004820     03 Ｊ変形徒手矯正術数Ｗ        PIC 9.
004830     03 Ｊマッサージ局所数ＰＷ      PIC 9.
004840     03 Ｊ変形徒手矯正術数ＰＷ      PIC 9.
004850     03 Ｊマッサージ局所数セットＷ  PIC 9.
004860     03 Ｊ変形徒手矯正術数セットＷ  PIC 9.
004870     03 Ｊマッサージ局所数日計Ｗ    PIC 9.
004880     03 Ｊ変形徒手矯正術数日計Ｗ    PIC 9.
004890     03 Ｊマッサージ局所数複数Ｗ    PIC 9.
004900     03 Ｊ変形徒手矯正術数複数Ｗ    PIC 9.
004910     03 Ｊマッサージ料月計Ｗ        PIC 9(7).
004920     03 Ｊ変形徒手料月計Ｗ          PIC 9(7).
004930*
004940 01 マッサージ施術同意箇所.
004950     03 マッサージ体幹同意Ｗ        PIC 9.
004960     03 マッサージ右上肢同意Ｗ      PIC 9.
004970     03 マッサージ左上肢同意Ｗ      PIC 9.
004980     03 マッサージ右下肢同意Ｗ      PIC 9.
004990     03 マッサージ左下肢同意Ｗ      PIC 9.
005000     03 変形徒手矯正術右上肢同意Ｗ  PIC 9.
005010     03 変形徒手矯正術左上肢同意Ｗ  PIC 9.
005020     03 変形徒手矯正術右下肢同意Ｗ  PIC 9.
005030     03 変形徒手矯正術左下肢同意Ｗ  PIC 9.
005040*
005050*   施術部位ごとの回数と料金
005060 01 マッサージ部位別金額欄Ｗ.
005070*      マッサージ関係
005080   03 マッサージ特殊施術Ｗ.
005090* / マッサージ特殊施術  0:なし　1:あり/
005100     05 マッサージ特殊施術フラグＷ            PIC 9.
005110     05 マッサージ体幹施術Ｗ.
005120       07 マッサージ体幹回数Ｗ                PIC 9(3).
005130       07 マッサージ体幹金額Ｗ                PIC 9(6).
005140     05 マッサージ右上肢施術Ｗ.
005150       07 マッサージ右上肢回数Ｗ              PIC 9(3).
005160       07 マッサージ右上肢金額Ｗ              PIC 9(6).
005170     05 マッサージ左上肢施術Ｗ.
005180       07 マッサージ左上肢回数Ｗ              PIC 9(3).
005190       07 マッサージ左上肢金額Ｗ              PIC 9(6).
005200     05 マッサージ右下肢施術Ｗ.
005210       07 マッサージ右下肢回数Ｗ              PIC 9(3).
005220       07 マッサージ右下肢金額Ｗ              PIC 9(6).
005230     05 マッサージ左下肢施術Ｗ.
005240       07 マッサージ左下肢回数Ｗ              PIC 9(3).
005250       07 マッサージ左下肢金額Ｗ              PIC 9(6).
005260*  変形徒手矯正術関係
005270   03 変形徒手矯正術特殊施術Ｗ.
005280*  / 変形徒手矯正術特殊施術　1:あり/
005290     05 変形徒手矯正術特殊施術フラグＷ        PIC 9.
005300     05 変形徒手矯正術右上肢施術Ｗ.
005310       07 変形徒手矯正術右上肢回数Ｗ          PIC 9(3).
005320       07 変形徒手矯正術右上肢金額Ｗ          PIC 9(6).
005330     05 変形徒手矯正術左上肢施術Ｗ.
005340       07 変形徒手矯正術左上肢回数Ｗ          PIC 9(3).
005350       07 変形徒手矯正術左上肢金額Ｗ          PIC 9(6).
005360     05 変形徒手矯正術右下肢施術Ｗ.
005370       07 変形徒手矯正術右下肢回数Ｗ          PIC 9(3).
005380       07 変形徒手矯正術右下肢金額Ｗ          PIC 9(6).
005390     05 変形徒手矯正術左下肢施術Ｗ.
005400       07 変形徒手矯正術左下肢回数Ｗ          PIC 9(3).
005410       07 変形徒手矯正術左下肢金額Ｗ          PIC 9(6).
005420*
005430*   施術部位ごとの回数と料金（助成用）
005440 01 Ｊマッサージ部位別金額欄Ｗ.
005450*      Ｊマッサージ関係
005460   03 Ｊマッサージ特殊施術Ｗ.
005470* / Ｊマッサージ特殊施術  0:なし　1:あり/
005480     05 Ｊマッサージ特殊施術フラグＷ          PIC 9.
005490     05 Ｊマッサージ体幹施術Ｗ.
005500       07 Ｊマッサージ体幹回数Ｗ              PIC 9(3).
005510       07 Ｊマッサージ体幹金額Ｗ              PIC 9(6).
005520     05 Ｊマッサージ右上肢施術Ｗ.
005530       07 Ｊマッサージ右上肢回数Ｗ            PIC 9(3).
005540       07 Ｊマッサージ右上肢金額Ｗ            PIC 9(6).
005550     05 Ｊマッサージ左上肢施術Ｗ.
005560       07 Ｊマッサージ左上肢回数Ｗ            PIC 9(3).
005570       07 Ｊマッサージ左上肢金額Ｗ            PIC 9(6).
005580     05 Ｊマッサージ右下肢施術Ｗ.
005590       07 Ｊマッサージ右下肢回数Ｗ            PIC 9(3).
005600       07 Ｊマッサージ右下肢金額Ｗ            PIC 9(6).
005610     05 Ｊマッサージ左下肢施術Ｗ.
005620       07 Ｊマッサージ左下肢回数Ｗ            PIC 9(3).
005630       07 Ｊマッサージ左下肢金額Ｗ            PIC 9(6).
005640*  Ｊ変形徒手矯正術関係
005650   03 Ｊ変形徒手矯正術特殊施術Ｗ.
005660*  / Ｊ変形徒手矯正術特殊施術  0:なし　1:あり/
005670     05 Ｊ変形徒手矯正術特殊施術フラグＷ      PIC 9.
005680     05 Ｊ変形徒手矯正術右上肢施術Ｗ.
005690       07 Ｊ変形徒手矯正術右上肢回数Ｗ        PIC 9(3).
005700       07 Ｊ変形徒手矯正術右上肢金額Ｗ        PIC 9(6).
005710     05 Ｊ変形徒手矯正術左上肢施術Ｗ.
005720       07 Ｊ変形徒手矯正術左上肢回数Ｗ        PIC 9(3).
005730       07 Ｊ変形徒手矯正術左上肢金額Ｗ        PIC 9(6).
005740     05 Ｊ変形徒手矯正術右下肢施術Ｗ.
005750       07 Ｊ変形徒手矯正術右下肢回数Ｗ        PIC 9(3).
005760       07 Ｊ変形徒手矯正術右下肢金額Ｗ        PIC 9(6).
005770     05 Ｊ変形徒手矯正術左下肢施術Ｗ.
005780       07 Ｊ変形徒手矯正術左下肢回数Ｗ        PIC 9(3).
005790       07 Ｊ変形徒手矯正術左下肢金額Ｗ        PIC 9(6).
005800*
005810 01 大阪丸証フラグ                  PIC X(3) VALUE SPACE.
005820 01 初回フラグ                      PIC X(3) VALUE SPACE.
005830 01 公費種別Ｗ                      PIC 9(2) VALUE ZERO.
005840 01 助成種別Ｗ                      PIC 9(2) VALUE ZERO.
005850 01 保険種別Ｗ                      PIC 9(2) VALUE ZERO.
005860 01 費用負担者番号助成Ｗ            PIC X(10) VALUE SPACE.
005860 01 費用負担者番号Ｗ                PIC X(10) VALUE SPACE.
005860 01 保険者番号Ｗ                    PIC X(10) VALUE SPACE.
005870* 通院テーブル用
005880 01 通院日ＣＷ.
005890     03 通院日Ｗ                    PIC 9 OCCURS 31.
005900 01 通院日２ＣＷ.
005910     03 通院日２Ｗ                  PIC 9 OCCURS 31.
005920*
005930 01 カウンタ                        PIC 9(2) VALUE ZERO.
005940 01 回数カウンタ                    PIC 9(2) VALUE ZERO.
005950 01 往療０円カウンタ                PIC 9(2) VALUE ZERO.
005960*
005970**
005980*
005990 01 計算拡張情報Ｗ.
006000   03 本体償還払い区分Ｗ            PIC 9 VALUE ZERO.
006010   03 助成償還払い区分Ｗ            PIC 9 VALUE ZERO.
006020   03 本体まとめ区分Ｗ              PIC 9 VALUE ZERO.
006030*
006040**
006050* 助成月途中開始日
006060 01 助成月途中開始日Ｗ              PIC 9(2) VALUE ZERO.
006070*
006080 01 負担区分Ｗ                      PIC 9(2) VALUE ZERO.
006090 01 枝番負担累計額Ｗ                PIC 9(6) VALUE ZERO.
006100***
006110***
006120* レセＦ退避用
006130     COPY H_RECE   OF  XFDLIB  JOINING   退避レセ  AS  PREFIX.
006140*
006150* 神奈川医療助成で一部負担金あるフラグ 2017/04/03
006160 01 助成負担ありフラグ              PIC X(3) VALUE ZERO.
006170*
006238*   前回施術報告書交付情報取得保存
006239 01 前回交付情報Ｗ.
006240   03 前回交付和暦年月Ｗ.
006241     05 前回交付和暦Ｗ                          PIC 9 VALUE ZERO.
006242     05 前回交付年Ｗ                            PIC 9(2) VALUE ZERO.
006243     05 前回交付月Ｗ                            PIC 9(2) VALUE ZERO.
006249   03 交付可能和暦年月Ｗ.
006250     05 交付可能和暦Ｗ                          PIC 9 VALUE ZERO.
006251     05 交付可能年Ｗ                            PIC 9(2) VALUE ZERO.
006252     05 交付可能月Ｗ                            PIC 9(2) VALUE ZERO.
006253   03 変形徒手フラグＷ                          PIC 9(1) VALUE ZERO.
006254   03 計算用報告書交付料Ｗ                      PIC 9(6) VALUE ZERO.
006255 01 プログラムＷ                                PIC X(8) VALUE "HMZKOFU".
HILO*********************************************************************追加ワーク
000590*    / 1:自宅 2:施設 3:同上 /
000600 01 往療場所区分Ｗ                              PIC 9(1) VALUE ZERO.
000610 01 往療場所Ｗ.
000620   03 往療場所施設コードＷ                      PIC 9(3) VALUE ZERO.
000630   03 往療場所患者コードＷ.
000640     05 往療場所患者番号Ｗ                      PIC 9(6) VALUE ZERO.
000650     05 往療場所枝番Ｗ                          PIC X    VALUE SPACE.
000370   03 施術区分Ｗ                                PIC 9    VALUE ZERO.
000510   03 施術者番号Ｗ                              PIC 9(4) VALUE ZERO.
       01 同一建物患者数Ｗ                            PIC 9(3) VALUE ZERO.
       01 同一建物患者数Ｗ２                          PIC 9(3) VALUE ZERO.
       01 当日局所数Ｗ                                PIC 9(1) VALUE ZERO.
      */20241112
000430 01 Ｈレセプト詳細レコードキーＷ.
000460     05 Ｈレセプト詳細施術区分Ｗ                PIC 9.
000470*-----------------------------------------------*
000480     05 Ｈレセプト詳細施術和暦年月Ｗ.
000490        07 Ｈレセプト詳細施術和暦Ｗ             PIC 9.
000500        07 Ｈレセプト詳細施術年月Ｗ.
000510          09 Ｈレセプト詳細施術年Ｗ             PIC 9(2).
000520          09 Ｈレセプト詳細施術月Ｗ             PIC 9(2).
000530     05 Ｈレセプト詳細患者コードＷ.
000540        07 Ｈレセプト詳細患者番号Ｗ             PIC 9(6).
000550        07 Ｈレセプト詳細枝番Ｗ                 PIC X.
000570     05 Ｈレセプト詳細レセ種別Ｗ                PIC 9(2).
      *20240627
       01 Ｈレセプト詳細ＦＷ.
000590   03 Ｈレセプト詳細レコードデータＷ.
001400     05 共通欄.
001410        07 特別地域加算料単価Ｗ                 PIC 9(5) VALUE ZERO.
001420        07 特別地域加算回数Ｗ                   PIC 9(2) VALUE ZERO.
001420        07 特別地域加算料Ｗ                     PIC 9(6) VALUE ZERO.
001420        07 累計特別地域加算料Ｗ                 PIC 9(6) VALUE ZERO.
              07 訪問重複Ｆ                           PIC 9(1) VALUE ZERO.
              07 突発不可Ｆ                           PIC 9(1) VALUE ZERO.
001490     05 はり金額欄.
001410        07 はりきゅう通所施術料単価１Ｗ         PIC 9(5) VALUE ZERO.
001420        07 はりきゅう通所施術料回数１Ｗ         PIC 9(2) VALUE ZERO.
001420        07 はりきゅう通所施術料施術料１Ｗ       PIC 9(6) VALUE ZERO.
001410        07 はりきゅう通所施術料単価２Ｗ         PIC 9(5) VALUE ZERO.
001420        07 はりきゅう通所施術料回数２Ｗ         PIC 9(2) VALUE ZERO.
001420        07 はりきゅう通所施術料施術料２Ｗ       PIC 9(6) VALUE ZERO.
001410        07 はりきゅう訪問施術料１単価１Ｗ       PIC 9(5) VALUE ZERO.
001420        07 はりきゅう訪問施術料１回数１Ｗ       PIC 9(2) VALUE ZERO.
001420        07 はりきゅう訪問施術料１施術料１Ｗ     PIC 9(6) VALUE ZERO.
001410        07 はりきゅう訪問施術料１単価２Ｗ       PIC 9(5) VALUE ZERO.
001420        07 はりきゅう訪問施術料１回数２Ｗ       PIC 9(2) VALUE ZERO.
001420        07 はりきゅう訪問施術料１施術料２Ｗ     PIC 9(6) VALUE ZERO.
001410        07 はりきゅう訪問施術料２単価１Ｗ       PIC 9(5) VALUE ZERO.
001420        07 はりきゅう訪問施術料２回数１Ｗ       PIC 9(2) VALUE ZERO.
001420        07 はりきゅう訪問施術料２施術料１Ｗ     PIC 9(6) VALUE ZERO.
001410        07 はりきゅう訪問施術料２単価２Ｗ       PIC 9(5) VALUE ZERO.
001420        07 はりきゅう訪問施術料２回数２Ｗ       PIC 9(2) VALUE ZERO.
001420        07 はりきゅう訪問施術料２施術料２Ｗ     PIC 9(6) VALUE ZERO.
001410        07 はりきゅう訪問施術料３単価１Ｗ       PIC 9(5) VALUE ZERO.
001420        07 はりきゅう訪問施術料３回数１Ｗ       PIC 9(2) VALUE ZERO.
001420        07 はりきゅう訪問施術料３施術料１Ｗ     PIC 9(6) VALUE ZERO.
001410        07 はりきゅう訪問施術料３単価２Ｗ       PIC 9(5) VALUE ZERO.
001420        07 はりきゅう訪問施術料３回数２Ｗ       PIC 9(2) VALUE ZERO.
001420        07 はりきゅう訪問施術料３施術料２Ｗ     PIC 9(6) VALUE ZERO.
001410        07 はりきゅう訪問施術料４単価１Ｗ       PIC 9(5) VALUE ZERO.
001420        07 はりきゅう訪問施術料４回数１Ｗ       PIC 9(2) VALUE ZERO.
001420        07 はりきゅう訪問施術料４施術料１Ｗ     PIC 9(6) VALUE ZERO.
001410        07 はりきゅう訪問施術料４単価２Ｗ       PIC 9(5) VALUE ZERO.
001420        07 はりきゅう訪問施術料４回数２Ｗ       PIC 9(2) VALUE ZERO.
001420        07 はりきゅう訪問施術料４施術料２Ｗ     PIC 9(6) VALUE ZERO.
001420        07 はり１術回数Ｗ                       PIC 9(2) VALUE ZERO.
001420        07 きゅう１術回数Ｗ                     PIC 9(2) VALUE ZERO.
001420        07 はりきゅう２術回数Ｗ                 PIC 9(2) VALUE ZERO.
001880     05 マッサージ金額欄Ｗ.
001410        07 マッサージ通所施術料単価１Ｗ         PIC 9(5) VALUE ZERO.
001420        07 マッサージ通所施術料回数１Ｗ         PIC 9(2) VALUE ZERO.
001420        07 マッサージ通所施術料施術料１Ｗ       PIC 9(6) VALUE ZERO.
001410        07 マッサージ通所施術料単価２Ｗ         PIC 9(5) VALUE ZERO.
001420        07 マッサージ通所施術料回数２Ｗ         PIC 9(2) VALUE ZERO.
001420        07 マッサージ通所施術料施術料２Ｗ       PIC 9(6) VALUE ZERO.
001410        07 マッサージ訪問施術料１単価１Ｗ       PIC 9(5) VALUE ZERO.
001420        07 マッサージ訪問施術料１回数１Ｗ       PIC 9(2) VALUE ZERO.
001420        07 マッサージ訪問施術料１施術料１Ｗ     PIC 9(6) VALUE ZERO.
001410        07 マッサージ訪問施術料１単価２Ｗ       PIC 9(5) VALUE ZERO.
001420        07 マッサージ訪問施術料１回数２Ｗ       PIC 9(2) VALUE ZERO.
001420        07 マッサージ訪問施術料１施術料２Ｗ     PIC 9(6) VALUE ZERO.
001410        07 マッサージ訪問施術料２単価１Ｗ       PIC 9(5) VALUE ZERO.
001420        07 マッサージ訪問施術料２回数１Ｗ       PIC 9(2) VALUE ZERO.
001420        07 マッサージ訪問施術料２施術料１Ｗ     PIC 9(6) VALUE ZERO.
001410        07 マッサージ訪問施術料２単価２Ｗ       PIC 9(5) VALUE ZERO.
001420        07 マッサージ訪問施術料２回数２Ｗ       PIC 9(2) VALUE ZERO.
001420        07 マッサージ訪問施術料２施術料２Ｗ     PIC 9(6) VALUE ZERO.
001410        07 マッサージ訪問施術料３単価１Ｗ       PIC 9(5) VALUE ZERO.
001420        07 マッサージ訪問施術料３回数１Ｗ       PIC 9(2) VALUE ZERO.
001420        07 マッサージ訪問施術料３施術料１Ｗ     PIC 9(6) VALUE ZERO.
001410        07 マッサージ訪問施術料３単価２Ｗ       PIC 9(5) VALUE ZERO.
001420        07 マッサージ訪問施術料３回数２Ｗ       PIC 9(2) VALUE ZERO.
001420        07 マッサージ訪問施術料３施術料２Ｗ     PIC 9(6) VALUE ZERO.
001410        07 マッサージ訪問施術料４単価１Ｗ       PIC 9(5) VALUE ZERO.
001420        07 マッサージ訪問施術料４回数１Ｗ       PIC 9(2) VALUE ZERO.
001420        07 マッサージ訪問施術料４施術料１Ｗ     PIC 9(6) VALUE ZERO.
001410        07 マッサージ訪問施術料４単価２Ｗ       PIC 9(5) VALUE ZERO.
001420        07 マッサージ訪問施術料４回数２Ｗ       PIC 9(2) VALUE ZERO.
001420        07 マッサージ訪問施術料４施術料２Ｗ     PIC 9(6) VALUE ZERO.
      */20220828
001940        07 変形徒手矯正術Ｗ.
001950           09 変形徒手矯正術単価Ｗ              PIC 9(5) VALUE ZERO.
001960           09 変形徒手矯正術回数Ｗ              PIC 9(3) VALUE ZERO.
000910     05 症状Ｗ                                  PIC X(200) VALUE SPACE.

HILO*********************************************************************追加ワーク


006256******************************************************************
006257*                          連結項目                              *
006258******************************************************************
006259*
006260* 負担率取得用14/10～
006261 01 連率－負担率取得キー IS EXTERNAL.
006262    03 連率－施術和暦年月.
006263       05 連率－施術和暦               PIC 9.
006264       05 連率－施術年月.
006270          07 連率－施術年              PIC 9(2).
006280          07 連率－施術月              PIC 9(2).
006290    03 連率－患者コード.
006300       05 連率－患者番号               PIC 9(6).
006310       05 連率－枝番                   PIC X.
006320    03 連率－実際負担率                PIC 9(3).
006330    03 連率－実際本体負担率            PIC 9(3).
006340    03 連率－健保負担率                PIC 9(3).
006350    03 連率－２７老負担率              PIC 9(3).
006360    03 連率－助成負担率                PIC 9(3).
006370    03 連率－特別用負担率              PIC 9(3).
006380*
006390 01 連メ－キー IS EXTERNAL.
006400    03 連メ－メッセージ                PIC N(20).
006410*
006420*--------------------------------------------------------
006430*
006440*
006450* 計算連携キー
006460 01 Ｈ連計算－キー IS EXTERNAL.
006470* / inパラメタ /
006480   03 Ｈ連計算－施術区分                   PIC 9.
006490   03 Ｈ連計算－施術和暦年月日.
006500      05 Ｈ連計算－施術和暦年月.
006510        07 Ｈ連計算－施術和暦              PIC 9.
006520        07 Ｈ連計算－施術年月.
006530          09 Ｈ連計算－施術年              PIC 9(2).
006540          09 Ｈ連計算－施術月              PIC 9(2).
006550      05 Ｈ連計算－施術日                  PIC 9(2).
006560   03 Ｈ連計算－患者コード.
006570      05 Ｈ連計算－患者番号                PIC 9(6).
006580      05 Ｈ連計算－枝番                    PIC X.
006590   03 Ｈ連計算－保険種別                   PIC 9(2).
006600* / 内部パラメタ/
006610   03 Ｈ連計算－内部パラメタ.
006620      05 Ｈ連計算－負担金計算区分          PIC 9.
006630      05 Ｈ連計算－費用額                  PIC 9(6).
006640*  初検日に"YES"(負担区分1,2,3,4)
006650      05 Ｈ連計算－初検日フラグ            PIC X(3).
006660*  当月の初回なら"YES"(負担区分5)
006670      05 Ｈ連計算－初診療フラグ            PIC X(3).
006680*  初検料の金額を転記(負担区分1)
006690      05 Ｈ連計算－初検料                  PIC 9(4).
006700*  当月の当日までの実日数(枝番も含む)(負担区分6,8,13)
006710      05 Ｈ連計算－実日数                  PIC 9(2).
006720*  日整静岡乳幼児用。当日の負担のありなし  ０：なし　１：あり (負担区分7)
006730      05 Ｈ連計算－その他計算区分１        PIC 9.
006740*  out
006750      05 Ｈ連計算－負担額                  PIC 9(6).
006760      05 Ｈ連計算－請求額                  PIC 9(6).
006770      05 Ｈ連計算－負担額助成              PIC 9(5).
006780      05 Ｈ連計算－請求額助成              PIC 9(5).
006790      05 Ｈ連計算－負担率                  PIC 9(3).
006800*
      */請求額助成の６桁ver/20231117
001980  01 Ｈ連計算－請求額助成２                PIC 9(6) IS EXTERNAL.
      *
      */日計負担額は少数以下一位を四捨五入/20220601
006460 01 Ｈ連計算２－キー IS EXTERNAL.
006620      05 Ｈ連計算２－日計区分              PIC 9.
002010*
      */負担額切り上げ用/20221125
001660 01 Ｈ連計算３－キー IS EXTERNAL.
001950    03 Ｈ連計算３－負担額                  PIC 9(6).
001970    03 Ｈ連計算３－負担額助成              PIC 9(5).
006810**
006820*
006830*---------------------------------------------------------*
006840* 大阪丸証フラグ用
006850 01 Ｈ連計算大阪－キー IS EXTERNAL.
006860   03 Ｈ連計算大阪－大阪丸証フラグ         PIC X(3).
006870*
006880*---------------------------------------------------------*
006890* 助成月途中用
006900 01 Ｈ連計算助成月途中－キー IS EXTERNAL.
006910   03 Ｈ連計算助成月途中－助成月途中開始日   PIC 9(2).
006920*---------------------------------------------------------*
006930*
006940*
006950*---------------------------------------------------------*
006960* 助成累計用
006970 01 Ｈ連計算助成累計額－負担額助成       PIC 9(6) IS EXTERNAL.
006980 01 Ｈ連計算助成累計額－助成負担金免除   PIC 9 IS EXTERNAL.
006990*---------------------------------------------------------*
007000*
007010*---------------------------------------------------------*
007020* 往療料上限料金算定用
007030*---------------------------------------------------------*
007040 01 Ｈ連最往－キー IS EXTERNAL.
007050* 1:鍼灸、2:あんま・マッサージ、3:両方（労災）
007060    03 Ｈ連最往－施術区分                      PIC 9(1).
007070    03 Ｈ連最往－施術和暦年月.
007080       05 Ｈ連最往－施術和暦                   PIC 9.
007090       05 Ｈ連最往－施術年月.
007100          07 Ｈ連最往－施術年                  PIC 9(2).
007110          07 Ｈ連最往－施術月                  PIC 9(2).
007120* 往療距離
007130    03 Ｈ連最往－往療距離                      PIC 9(2)V9.
007140* 往療金額の上限に該当したか？　0:該当せず 1:該当する
007150    03 Ｈ連最往－該当区分                      PIC 9(1).
007160* 今回の往療金額（往療基本料+往療加算料の合算）
007170    03 Ｈ連最往－往療加算金額                  PIC 9(6).
007180* 将来予備項目
007190    03 Ｈ連最往－その他１                      PIC 9(6).
007200    03 Ｈ連最往－その他２                      PIC 9(6).
007210    03 Ｈ連最往－その他３                      PIC 9(6).
007220*
007230*/負担区分２２(大阪府平成30年4月～)/20180307
007240 01 連－負担額助成１ＧＷ               PIC 9(6) IS EXTERNAL.
007250 01 連－負担額助成１０ＧＷ             PIC 9(6) IS EXTERNAL.
007260 01 連－負担額助成１ＧＷ２             PIC 9(6) IS EXTERNAL.
007270 01 連－日計レセＦ                     PIC 9    IS EXTERNAL.
007280*
007447******************************
007448* 前回施術報告書交付情報取得 *
007449******************************
007450 01 連前交－キー IS EXTERNAL.
007451*/ in
007452*   入力はＨレセプトＦの主キー
007453    03 連前交－施術区分                PIC 9.
007454    03 連前交－施術和暦年月.
007455       05 連前交－施術和暦             PIC 9.
007456       05 連前交－施術年月.
007457         07 連前交－施術年             PIC 9(2).
007458         07 連前交－施術月             PIC 9(2).
007459    03 連前交－患者コード.
007460       05 連前交－患者番号             PIC 9(6).
007461       05 連前交－枝番                 PIC X.
007462    03 連前交－レセ種別                PIC 9(2).
007463*
007464*/ out
007465    03 連前交－前回支給和暦年月.
007466       05 連前交－前回支給和暦         PIC 9.
007467       05 連前交－前回支給年           PIC 9(2).
007468       05 連前交－前回支給月           PIC 9(2).
007469    03 連前交－交付可能和暦年月.
007470       05 連前交－交付可能和暦         PIC 9.
007471       05 連前交－交付可能年           PIC 9(2).
007472       05 連前交－交付可能月           PIC 9(2).
007473*/  ( 1:変形徒手あり )
007474    03 連前交－変形徒手フラグ          PIC 9(1).
007475*
      */大阪助成：日の上限を1度も越えない場合、本体の負担額と差が合っても請求しない/20220830
       01 全額患者負担Ｆ                     PIC X(3) IS EXTERNAL.
002664 01 Ｈ連計算４－キー IS EXTERNAL.
002666   03 Ｈ連計算４－同一建物患者数取得Ｆ     PIC 9.
      *
006259*
       01 連入２０１－画面情報 IS EXTERNAL GLOBAL.
          03 連入２０１－施術区分               PIC 9(1).
          03 連入２０１－施術和暦年月日.
             05 連入２０１－施術和暦年月.
                07 連入２０１－施術和暦         PIC 9(1).
                07 連入２０１－施術年           PIC 9(2).
                07 連入２０１－施術月           PIC 9(2).
             05 連入２０１－施術日              PIC 9(2).
          03 連入２０１－患者コード.
             05 連入２０１－患者番号            PIC 9(6).
             05 連入２０１－枝番                PIC X(1).
          03 連入２０１－エラーフラグ           PIC X(3).
007476******************************************************************
007477*                      PROCEDURE  DIVISION                       *
007478******************************************************************
007479 PROCEDURE               DIVISION.
HILO***       DISPLAY "K0610:St--- 患者№" Ｈ連計算－患者コード " 施術月" Ｈ連計算－施術和暦年月 "---" Ｈ連計算－施術日
007482************
007483*           *
007484* 初期処理   *
007485*           *
007486************
007487     PERFORM 初期化.
007488     PERFORM 料金データ取得.
007489     PERFORM 負担率取得.
007490     PERFORM 助成データ処理.
           PERFORM 受領委任情報取得.
007491*
007492************
007493*           *
007494* 主処理     *
007495*           *
007496************
007497     PERFORM 日計データ処理.
007498*   上記の後
007500     PERFORM レセプト処理.
007500     PERFORM レセプト詳細処理.
007510
007520************
007530*           *
007540* 終了処理  *
007550*           *
007560************
007570     PERFORM 終了処理.
007580     MOVE ZERO TO PROGRAM-STATUS.
007590     EXIT PROGRAM.
007600*
007610*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
007620*================================================================*
007630 初期化 SECTION.
007640*
007650*   / 内部パラメタの初期化 /
007660     INITIALIZE Ｈ連計算－内部パラメタ.
007670*
007680*   / 大阪丸証フラグ用初期化 /
007690     INITIALIZE Ｈ連計算大阪－キー.
007700*
007720     PERFORM ファイルオープン.
007730*
007740*------------------------------------------------------*
007750*   / Ｈ制御情報より /
007760     MOVE ZERO TO Ｈ制－制御区分.
007770     READ Ｈ制御情報マスタ
007780     NOT INVALID KEY
007790         MOVE Ｈ制－領収書負担金計算区分 TO 領収書負担金計算区分Ｗ
007800     END-READ.
007810*------------------------------------------------------*
007820*    フラグをクリア 2017/04/03
007830     MOVE SPACE TO 助成負担ありフラグ.
007840*================================================================*
007850 ファイルオープン SECTION.
007860*
007870* EXTERNAL用
007880     OPEN INPUT 受診者情報Ｆ.
007890         MOVE NC"受診者" TO ファイル名.
007900         PERFORM オープンチェック.
007910     OPEN INPUT 保険者マスタ.
007920         MOVE NC"保険者" TO ファイル名.
007930         PERFORM オープンチェック.
007940     OPEN INPUT 負担率マスタ.
007950         MOVE NC"負担率" TO ファイル名.
007960         PERFORM オープンチェック.
007970     OPEN INPUT 市町村マスタ.
007980         MOVE NC"市町村" TO ファイル名.
007990         PERFORM オープンチェック.
008000     OPEN INPUT 保険者特別負担マスタ.
008010         MOVE NC"保険者特別負担" TO ファイル名.
008020         PERFORM オープンチェック.
008030*
008040     OPEN INPUT Ｈ料金マスタ.
008050         MOVE NC"Ｈ料金マスタ" TO ファイル名.
008060         PERFORM オープンチェック.
008070     OPEN INPUT 参照Ｈ日計データＦ.
008080         MOVE NC"参照Ｈ日計データＦ" TO ファイル名.
008090         PERFORM オープンチェック.
008100*     OPEN INPUT Ｈ負傷データＦ.
008110*         MOVE NC"Ｈ負傷データＦ" TO ファイル名.
008120*         PERFORM オープンチェック.
008130     OPEN I-O Ｈ日計データＦ.
008140         MOVE NC"Ｈ日計データＦ" TO ファイル名.
008150         PERFORM オープンチェック.
008160     OPEN I-O ＨレセプトＦ.
008170         MOVE NC"ＨレセプトＦ" TO ファイル名.
008180         PERFORM オープンチェック.
008190*
008200     OPEN INPUT 会計領収Ｆ.
008210         MOVE NC"会計領収Ｆ" TO ファイル名.
008220         PERFORM オープンチェック.
008230*
008240     OPEN INPUT Ｈ制御情報マスタ.
008250         MOVE NC"Ｈ制御" TO ファイル名.
008260         PERFORM オープンチェック.
008270*
008280     OPEN INPUT Ｈ往療実績Ｆ.
008290         MOVE NC"Ｈ往療実績Ｆ" TO ファイル名.
008300         PERFORM オープンチェック.
008310*
004840     OPEN INPUT Ｈ施術所情報マスタ.
004850         MOVE NC"Ｈ施術所" TO ファイル名.
004860         PERFORM オープンチェック.
008310*
008320     OPEN I-O 更新ＨレセプトＦ.
008330         MOVE NC"更新ＨレセプトＦ" TO ファイル名.
008340         PERFORM オープンチェック.
      *
008320     OPEN I-O Ｈレセプト詳細Ｆ.
           IF 状態キー = 35
008320         OPEN OUTPUT Ｈレセプト詳細Ｆ
           END-IF.
008330     MOVE NC"レセプト詳細Ｆ" TO ファイル名.
008340     PERFORM オープンチェック.
           IF Ｈ連計算４－同一建物患者数取得Ｆ = ZERO
008320         OPEN OUTPUT 作業ファイル１
008330             MOVE NC"作１" TO ファイル名
008340             PERFORM オープンチェック
           END-IF.
      *
008320     OPEN OUTPUT 作業ファイル２
008330         MOVE NC"作２" TO ファイル名
008340         PERFORM オープンチェック
           CLOSE 作業ファイル２
008320     OPEN I-O 作業ファイル２
008330         MOVE NC"作２" TO ファイル名
008340         PERFORM オープンチェック
008310*
004840     OPEN INPUT 施設マスタ.
004850         MOVE NC"施設" TO ファイル名.
004860         PERFORM オープンチェック.
008310*
008320     OPEN INPUT 参照Ｈレセプト詳細Ｆ.
008330         MOVE NC"参照レセプト詳細Ｆ" TO ファイル名.
008340         PERFORM オープンチェック.
008310*
018000     OPEN INPUT 元号マスタ.
018010         MOVE NC"元号" TO ファイル名.
018020         PERFORM オープンチェック.
008350*================================================================*
008360 オープンチェック SECTION.
008370*
008380     IF 状態キー  NOT =  "00"
008390         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
008400         DISPLAY NC"状態キー：" 状態キー         UPON CONS
008410         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008420                                                 UPON CONS
008430*-----------------------------------------*
008440         CALL "actcshm"  WITH C LINKAGE
008450*-----------------------------------------*
008460         ACCEPT  キー入力 FROM CONS
008470         PERFORM ファイル閉鎖
008480         MOVE 99 TO PROGRAM-STATUS
008490         EXIT PROGRAM.
008500*
008510*================================================================*
008520 ファイル閉鎖 SECTION.
008530*
008540     CLOSE 受診者情報Ｆ 保険者マスタ 負担率マスタ 市町村マスタ 保険者特別負担マスタ.
008550     CLOSE ＨレセプトＦ Ｈ日計データＦ 会計領収Ｆ 参照Ｈ日計データＦ Ｈ料金マスタ Ｈ制御情報マスタ.
008560     CLOSE Ｈ往療実績Ｆ Ｈ施術所情報マスタ.
008570     CLOSE 更新ＨレセプトＦ.
           CLOSE Ｈレセプト詳細Ｆ 施設マスタ.
           IF Ｈ連計算４－同一建物患者数取得Ｆ = ZERO
               CLOSE 作業ファイル１ 
           END-IF.
           CLOSE  作業ファイル２ Ｈレセプト詳細Ｆ 元号マスタ.
008580*================================================================*
008590 終了処理 SECTION.
008600*
008610     PERFORM ファイル閉鎖.
008620*================================================================*
008640 受領委任情報取得 SECTION.
      *
      */保険者が受領委任の時、施術者が受領委任登録をしていない場合償還払いにする
      *
           MOVE ZERO  TO 施術者受領委任Ｆ.
           MOVE 41    TO Ｈ施情－施術所番号.
           READ Ｈ施術所情報マスタ
           NOT INVALID KEY
               EVALUATE Ｈ連計算－施術区分
               WHEN 1
                   IF Ｈ施情４１－登録記号番号はりきゅう NOT = SPACE
                       MOVE 1     TO 施術者受領委任Ｆ
                   END-IF
               WHEN 2
                   IF Ｈ施情４１－登録記号番号マッサージ NOT = SPACE
                       MOVE 1     TO 施術者受領委任Ｆ
                   END-IF
               END-EVALUATE
           END-READ.
           IF 施術者受領委任Ｆ = ZERO
               MOVE ZERO                 TO 受領委任Ｆ
011650         IF 公費種別Ｗ NOT = ZERO
                   MOVE 公費種別Ｗ       TO 市－公費種別
                   MOVE 費用負担者番号Ｗ TO 市－市町村番号
                   READ 市町村マスタ
                   NOT INVALID KEY
                       IF (市－受領委任開始和暦年月 =  ZERO OR SPACE)
                           IF (市－受領委任区分   = 1)
                               MOVE 1      TO 受領委任Ｆ
                           END-IF
                       ELSE
                           IF (市－受領委任開始和暦年月 <= Ｈ連計算－施術和暦年月)
                               IF (市－受領委任区分後   = 1)
                                   MOVE 1      TO 受領委任Ｆ
                               END-IF
                           ELSE
                               IF (市－受領委任区分   = 1)
                                   MOVE 1      TO 受領委任Ｆ
                               ELSE
                                   MOVE ZERO   TO 受領委任Ｆ
                               END-IF
                           END-IF
                       END-IF
                   END-READ
011670         ELSE
                   IF 保険種別Ｗ < 10
                       MOVE 保険種別Ｗ     TO 保－保険種別
                       MOVE 保険者番号Ｗ   TO 保－保険者番号
                       READ 保険者マスタ
                       NOT INVALID KEY
                           IF (保－受領委任開始和暦年月 =  ZERO OR SPACE)
                               IF (保－受領委任区分   = 1)
                                   MOVE 1      TO 受領委任Ｆ
                               END-IF
                           ELSE
                               IF (保－受領委任開始和暦年月 <= Ｈ連計算－施術和暦年月)
                                   IF (保－受領委任区分後   = 1)
                                       MOVE 1      TO 受領委任Ｆ
                                   END-IF
                               ELSE
                                   IF (保－受領委任区分   = 1)
                                       MOVE 1      TO 受領委任Ｆ
                                   ELSE
                                       MOVE ZERO   TO 受領委任Ｆ
                                   END-IF
                               END-IF
                           END-IF
                       END-READ
                   END-IF
               END-IF
               IF 受領委任Ｆ = 1
                   INITIALIZE 更新Ｈレセ－レコード
011500             MOVE Ｈ連計算－施術区分  TO  更新Ｈレセ－施術区分
011510             MOVE Ｈ連計算－施術和暦  TO  更新Ｈレセ－施術和暦
011520             MOVE Ｈ連計算－施術年    TO  更新Ｈレセ－施術年
011530             MOVE Ｈ連計算－施術月    TO  更新Ｈレセ－施術月
011540             MOVE Ｈ連計算－患者番号  TO  更新Ｈレセ－患者番号
011550             MOVE Ｈ連計算－枝番      TO  更新Ｈレセ－枝番
011560*
011650             IF 公費種別Ｗ NOT = ZERO
011660                 MOVE 2  TO 更新Ｈレセ－レセ種別
011670             ELSE
                       IF 保険種別Ｗ < 10
011680                     MOVE 1  TO 更新Ｈレセ－レセ種別
                       END-IF
011690             END-IF
                   READ 更新ＨレセプトＦ
                   NOT INVALID KEY
011740                 MOVE 1      TO 更新Ｈレセ－償還払い区分
                       REWRITE 更新Ｈレセ－レコード
                       IF 状態キー NOT = "00"
                           MOVE NC"更新Ｈレセ" TO ファイル名
                           PERFORM エラー表示
                       END-IF
                   END-READ
               END-IF
011650         IF 助成種別Ｗ NOT = ZERO
                   MOVE ZERO                 TO 受領委任Ｆ
                   MOVE 助成種別Ｗ           TO 市－公費種別
                   MOVE 費用負担者番号助成Ｗ TO 市－市町村番号
                   READ 市町村マスタ
                   NOT INVALID KEY
                       IF (市－受領委任開始和暦年月 =  ZERO OR SPACE)
                           IF (市－受領委任区分   = 1)
                               MOVE 1      TO 受領委任Ｆ
                           END-IF
                       ELSE
                           IF (市－受領委任開始和暦年月 <= Ｈ連計算－施術和暦年月)
                               IF (市－受領委任区分後   = 1)
                                   MOVE 1      TO 受領委任Ｆ
                               END-IF
                           ELSE
                               IF (市－受領委任区分   = 1)
                                   MOVE 1      TO 受領委任Ｆ
                               ELSE
                                   MOVE ZERO   TO 受領委任Ｆ
                               END-IF
                           END-IF
                       END-IF
                   END-READ
                   IF 受領委任Ｆ = 1
                       INITIALIZE 更新Ｈレセ－レコード
011500                 MOVE Ｈ連計算－施術区分  TO  更新Ｈレセ－施術区分
011510                 MOVE Ｈ連計算－施術和暦  TO  更新Ｈレセ－施術和暦
011520                 MOVE Ｈ連計算－施術年    TO  更新Ｈレセ－施術年
011530                 MOVE Ｈ連計算－施術月    TO  更新Ｈレセ－施術月
011540                 MOVE Ｈ連計算－患者番号  TO  更新Ｈレセ－患者番号
011550                 MOVE Ｈ連計算－枝番      TO  更新Ｈレセ－枝番
011660                 MOVE 3                   TO  更新Ｈレセ－レセ種別
                       READ 更新ＨレセプトＦ
                       NOT INVALID KEY
011740                     MOVE 1      TO 更新Ｈレセ－償還払い区分
                           REWRITE 更新Ｈレセ－レコード
                           IF 状態キー NOT = "00"
                               MOVE NC"更新Ｈレセ" TO ファイル名
                               PERFORM エラー表示
                           END-IF
                       END-READ
                   END-IF
               END-IF
           END-IF.
      *
008630*================================================================*
008640 料金データ取得 SECTION.
008650*
008660*   / 初期化 /
008670     INITIALIZE 料金情報Ｗ.
008680*/負担区分２２(大阪府平成30年4月～)/20180306
008690     INITIALIZE 連－負担額助成１ＧＷ 連－負担額助成１０ＧＷ 連－負担額助成１ＧＷ２.
008700*
008710*   /  区分コード:1（健保）
008720     MOVE 1                   TO  Ｈ料１－区分コード.
008730     MOVE Ｈ連計算－施術和暦  TO  Ｈ料１－開始和暦.
008740     MOVE Ｈ連計算－施術年    TO  Ｈ料１－開始年.
008750     MOVE Ｈ連計算－施術月    TO  Ｈ料１－開始月.
008760*
008770     START Ｈ料金マスタ KEY IS <= Ｈ料１－区分コード Ｈ料１－開始和暦年月
008780                                  REVERSED
008790     END-START.
008800*
008810     IF 状態キー = "00"
008820         MOVE SPACE TO 終了フラグ２
008830         PERFORM Ｈ料金マスタ読込
008840         IF ( Ｈ料１－区分コード = 1 ) AND
008850            ( 終了フラグ２       = SPACE )
008860*         / 退避
008870              MOVE Ｈ料１－Ｈ１術初回  TO Ｈ１術初回Ｗ
008880              MOVE Ｈ料１－Ｈ２術初回  TO Ｈ２術初回Ｗ
008890              MOVE Ｈ料１－Ｈ１術      TO Ｈ１術Ｗ
008900              MOVE Ｈ料１－Ｈ２術      TO Ｈ２術Ｗ
008910              MOVE Ｈ料１－Ｈ電療料    TO Ｈ電療料Ｗ
008920              MOVE Ｈ料１－Ｈ往療基本料金    TO Ｈ往療基本料金Ｗ
008930              MOVE Ｈ料１－Ｈ往療追加料金    TO Ｈ往療追加料金Ｗ
008940              MOVE Ｈ料１－Ｍ１局所    TO Ｍ１局所Ｗ
008950              MOVE Ｈ料１－Ｍ変形徒手矯正術  TO Ｍ変形徒手矯正術Ｗ
008960              MOVE Ｈ料１－Ｍ温罨法料  TO Ｍ温罨法料Ｗ
008970              MOVE Ｈ料１－Ｍ電療料    TO Ｍ電療料Ｗ
008980              MOVE Ｈ料１－Ｍ往療基本料金    TO Ｍ往療基本料金Ｗ
008990              MOVE Ｈ料１－Ｍ往療追加料金    TO Ｍ往療追加料金Ｗ
009000*
009010              MOVE Ｈ料１－往療基本距離      TO 往療基本距離
009020              MOVE Ｈ料１－施術報告書交付料  TO 施術報告書交付料単価Ｗ
009023*
      */令和6年10月以降↓↓↓/*20240618---------------------------------------
000331              MOVE Ｈ料１－Ｈ１術訪問施術料１ TO Ｈ１術訪問施術料１Ｗ
000332              MOVE Ｈ料１－Ｈ２術訪問施術料１ TO Ｈ２術訪問施術料１Ｗ
000331              MOVE Ｈ料１－Ｈ１術訪問施術料２ TO Ｈ１術訪問施術料２Ｗ
000332              MOVE Ｈ料１－Ｈ２術訪問施術料２ TO Ｈ２術訪問施術料２Ｗ
000331              MOVE Ｈ料１－Ｈ１術訪問施術料３ TO Ｈ１術訪問施術料３Ｗ
000332              MOVE Ｈ料１－Ｈ２術訪問施術料３ TO Ｈ２術訪問施術料３Ｗ
000331              MOVE Ｈ料１－Ｈ１術訪問施術料４ TO Ｈ１術訪問施術料４Ｗ
000332              MOVE Ｈ料１－Ｈ２術訪問施術料４ TO Ｈ２術訪問施術料４Ｗ
      *
008870              COMPUTE １術初険料Ｗ = Ｈ料１－Ｈ１術初回 - Ｈ料１－Ｈ１術
008880              COMPUTE ２術初険料Ｗ = Ｈ料１－Ｈ２術初回 - Ｈ料１－Ｈ２術
      */マッサージ
000339              MOVE Ｈ料１－Ｍ１局所施術料            TO Ｍ１局所施術料Ｗ
000339              MOVE Ｈ料１－Ｍ２局所施術料            TO Ｍ２局所施術料Ｗ
000339              MOVE Ｈ料１－Ｍ３局所施術料            TO Ｍ３局所施術料Ｗ
000339              MOVE Ｈ料１－Ｍ４局所施術料            TO Ｍ４局所施術料Ｗ
000339              MOVE Ｈ料１－Ｍ５局所施術料            TO Ｍ５局所施術料Ｗ
000339              MOVE Ｈ料１－Ｍ１局所訪問施術料１      TO Ｍ１局所訪問施術料１Ｗ
000339              MOVE Ｈ料１－Ｍ２局所訪問施術料１      TO Ｍ２局所訪問施術料１Ｗ
000339              MOVE Ｈ料１－Ｍ３局所訪問施術料１      TO Ｍ３局所訪問施術料１Ｗ
000339              MOVE Ｈ料１－Ｍ４局所訪問施術料１      TO Ｍ４局所訪問施術料１Ｗ
000339              MOVE Ｈ料１－Ｍ５局所訪問施術料１      TO Ｍ５局所訪問施術料１Ｗ
000339              MOVE Ｈ料１－Ｍ１局所訪問施術料２      TO Ｍ１局所訪問施術料２Ｗ
000339              MOVE Ｈ料１－Ｍ２局所訪問施術料２      TO Ｍ２局所訪問施術料２Ｗ
000339              MOVE Ｈ料１－Ｍ３局所訪問施術料２      TO Ｍ３局所訪問施術料２Ｗ
000339              MOVE Ｈ料１－Ｍ４局所訪問施術料２      TO Ｍ４局所訪問施術料２Ｗ
000339              MOVE Ｈ料１－Ｍ５局所訪問施術料２      TO Ｍ５局所訪問施術料２Ｗ
000339              MOVE Ｈ料１－Ｍ１局所訪問施術料３      TO Ｍ１局所訪問施術料３Ｗ
000339              MOVE Ｈ料１－Ｍ２局所訪問施術料３      TO Ｍ２局所訪問施術料３Ｗ
000339              MOVE Ｈ料１－Ｍ３局所訪問施術料３      TO Ｍ３局所訪問施術料３Ｗ
000339              MOVE Ｈ料１－Ｍ４局所訪問施術料３      TO Ｍ４局所訪問施術料３Ｗ
000339              MOVE Ｈ料１－Ｍ５局所訪問施術料３      TO Ｍ５局所訪問施術料３Ｗ
000339              MOVE Ｈ料１－Ｍ１局所訪問施術料４      TO Ｍ１局所訪問施術料４Ｗ
000339              MOVE Ｈ料１－Ｍ２局所訪問施術料４      TO Ｍ２局所訪問施術料４Ｗ
000339              MOVE Ｈ料１－Ｍ３局所訪問施術料４      TO Ｍ３局所訪問施術料４Ｗ
000339              MOVE Ｈ料１－Ｍ４局所訪問施術料４      TO Ｍ４局所訪問施術料４Ｗ
000339              MOVE Ｈ料１－Ｍ５局所訪問施術料４      TO Ｍ５局所訪問施術料４Ｗ
                    MOVE Ｈ料１－特別地域加算料            TO 料Ｍ特別地域加算料Ｗ

HILO  *     DISPLAY "610-51 Ｈ料１－特別地域加算料" Ｈ料１－特別地域加算料

      */令和6年10月以降↑↑↑/*20240618---------------------------------------
      *
009030         ELSE
009040             MOVE  NC"　該当の施術年月の料金が見つかりません。" TO 連メ－メッセージ
009050             CALL   "MSG001"
009060             CANCEL "MSG001"
009070         END-IF
009080     ELSE
009090         MOVE  NC"　該当の施術年月の料金が見つかりません。" TO 連メ－メッセージ
009100         CALL   "MSG001"
009110         CANCEL "MSG001"
009120     END-IF.
009130*
009140*================================================================*
009150 Ｈ料金マスタ読込 SECTION.
009160*
009170     READ Ｈ料金マスタ NEXT
009180     AT END
009190         MOVE "YES"  TO 終了フラグ２
009200     END-READ.
009210*
009394*================================================================*
009395 負担率取得 SECTION.
009396*
009397     MOVE SPACE TO 連率－負担率取得キー.
009398     INITIALIZE 連率－負担率取得キー.
009399     MOVE Ｈ連計算－施術和暦年月 TO 連率－施術和暦年月.
009400     MOVE Ｈ連計算－患者コード   TO 連率－患者コード.
009401*
009402     CALL   "HUTANRIT".
009403     CANCEL "HUTANRIT".
009404*
009435*================================================================*
009436 助成データ処理 SECTION.
009437*----------------------------------------------------------*
009438* 負担区分:6,8,14 (毎月一定金額、一定回数を負担)
009439*          13  (毎月一定金額、一定回数を負担（１０円単位）)
009440*          大阪老人負担金免除対象者
009441*          5,16（累計）
009442*----------------------------------------------------------*
009443* 特殊負担：一定金額を一定回数まで負担に対応するための処理
009444* 枝番を含めた通院テーブルを作成する。
009445*----------------------------------------------------------*
009446*
009450*    / 通院テーブルZEROクリアー /
009460     MOVE ZERO  TO 通院日ＣＷ.
009470     MOVE ZERO  TO 通院日２ＣＷ.
009480     MOVE SPACE TO 大阪丸証フラグ.
009490*
009500*    / 助成月途中用クリアー /
009510     MOVE ZERO TO 助成月途中開始日Ｗ.
009520     INITIALIZE Ｈ連計算助成月途中－キー.
009530
009540*   / 助成累計用初期化 /
009550     MOVE ZERO TO 負担区分Ｗ.
009560     MOVE ZERO TO 枝番負担累計額Ｗ.
009570     MOVE ZERO TO Ｈ連計算助成累計額－負担額助成.
009580     MOVE ZERO TO Ｈ連計算助成累計額－助成負担金免除.
009590*
009610     MOVE Ｈ連計算－施術和暦 TO 受－施術和暦.
009620     MOVE Ｈ連計算－施術年   TO 受－施術年.
009630     MOVE Ｈ連計算－施術月   TO 受－施術月.
009640     MOVE Ｈ連計算－患者番号 TO 受－患者番号.
009650     MOVE Ｈ連計算－枝番     TO 受－枝番.
009660     READ 受診者情報Ｆ
009670     NOT INVALID KEY
009680*       / 退避 /
009690        MOVE 受－公費種別 TO 公費種別Ｗ
009700        MOVE 受－助成種別 TO 助成種別Ｗ
009710        MOVE 受－保険種別 TO 保険種別Ｗ
009720        MOVE 受－費用負担者番号助成 TO 費用負担者番号助成Ｗ
009720        MOVE 受－費用負担者番号     TO 費用負担者番号Ｗ
009720        MOVE 受－保険者番号         TO 保険者番号Ｗ
009730*
009740*------------------------------------------------------*
009750* 大阪老人負担金免除対象者か
009760**        IF (受－施術和暦年月 >= 41611)
009770        IF ( 受－施術和暦年月 >= 41611 ) AND ( 受－施術和暦年月 <= 42003 )
009780            IF (( 受－公費種別 = ZERO )    AND  ( 受－助成種別 = 51 ) AND
009790                ( 受－老人負担金免除 = 1 ) AND
009800                ( 受－費用負担者番号助成(3:2) = "27" ))
009810              OR
009820               (( 受－公費種別 = 05 ) AND ( 受－老人負担金免除 = 1 ) AND
009830                ( 受－費用負担者番号(3:2) = "27" ))
009840              OR
009850               (( 受－公費種別 = 05 ) AND ( 受－助成負担金免除 = 1 ) AND
009860                ( 受－費用負担者番号(3:2) = "27" ))
009870*
009880                MOVE "YES" TO 大阪丸証フラグ
009890            END-IF
009900        END-IF
009910*
009920*------------------------------------------------------*
009930* 助成の月途中開始
009940        IF ( 受－保険分類 = 1 ) AND ( 受－助成種別 NOT = ZERO ) AND ( 受－費用負担者番号助成 NOT = SPACE ) AND
009950           ( 受－助成月途中開始日 NOT = ZERO ) AND ( 受－助成月途中開始日 NOT = SPACE )
009960           MOVE 受－助成月途中開始日 TO 助成月途中開始日Ｗ
009970           MOVE 受－助成月途中開始日 TO Ｈ連計算助成月途中－助成月途中開始日
009980        END-IF
009990*------------------------------------------------------*
010000*
010010*------------------------------------------------------*
010020* 上記後、助成枝番の累計処理（負担区分5,16用）
010030        IF 受－助成種別 NOT = ZERO
010040             MOVE 受－助成負担金免除 TO Ｈ連計算助成累計額－助成負担金免除
010050             PERFORM 助成負担区分取得
010060             IF ( 受－枝番作成時枝番 NOT = SPACE ) AND ( 負担区分Ｗ = 05 OR 16 )
010070                IF 助成月途中開始日Ｗ = ZERO
010080                   PERFORM 枝番負担累計額計算
010090                END-IF
010100             END-IF
010110        END-IF
010120*------------------------------------------------------*
010130*
010140     END-READ.
010150**
010160**
010170     IF ( 助成種別Ｗ NOT = ZERO ) OR ( 大阪丸証フラグ = "YES" )
010180        MOVE Ｈ連計算－施術和暦 TO 受－施術和暦
010190        MOVE Ｈ連計算－施術年   TO 受－施術年
010200        MOVE Ｈ連計算－施術月   TO 受－施術月
010210        MOVE Ｈ連計算－患者番号 TO 受－患者番号
010220        MOVE SPACE              TO 受－枝番
010230        START 受診者情報Ｆ KEY IS >= 受－施術和暦年月
010240                                     受－患者コード
010250        END-START
010260        IF 状態キー = "00"
010270            MOVE SPACE TO 終了フラグ３
010280            PERFORM 受診者情報Ｆ読込
010290            PERFORM UNTIL ( 終了フラグ３         NOT = SPACE ) OR
010300                          ( Ｈ連計算－施術和暦   NOT = 受－施術和暦 ) OR
010310                          ( Ｈ連計算－施術年     NOT = 受－施術年 ) OR
010320                          ( Ｈ連計算－施術月     NOT = 受－施術月 ) OR
010330                          ( Ｈ連計算－患者番号   NOT = 受－患者番号 )
010340*---*
010350**             健保のみ対象
010360               IF 受－保険分類 = 1
010370*
010380*                  / 同一患者番号、年月の通院日セット はりきゅうとマッサージ両方 /
010390*----------------------------------------------------------------------------------*
010400*                  / ①はりきゅう /
010410                   MOVE 1             TO 参照Ｈ日－施術区分
010420                   MOVE 受－患者番号  TO 参照Ｈ日－患者番号
010430                   MOVE 受－枝番      TO 参照Ｈ日－枝番
010440                   MOVE 受－施術和暦  TO 参照Ｈ日－施術和暦
010450                   MOVE 受－施術年    TO 参照Ｈ日－施術年
010460                   MOVE 受－施術月    TO 参照Ｈ日－施術月
010470                   MOVE 1             TO 参照Ｈ日－施術日
010480*
010490                   START 参照Ｈ日計データＦ KEY IS >= 参照Ｈ日－施術区分
010500                                                      参照Ｈ日－患者コード
010510                                                      参照Ｈ日－施術和暦年月日
010520                   END-START
010530                   IF 状態キー = "00"
010540                       MOVE SPACE TO 終了フラグ
010550                       PERFORM 参照Ｈ日計データＦ読込
010560*                      繰り返し（当月）
010570                       PERFORM UNTIL ( 参照Ｈ日－施術区分   NOT = 1              ) OR
010580                                     ( 参照Ｈ日－患者コード NOT = 受－患者コード ) OR
010590                                     ( 参照Ｈ日－施術和暦   NOT = 受－施術和暦   ) OR
010600                                     ( 参照Ｈ日－施術年     NOT = 受－施術年     ) OR
010610                                     ( 参照Ｈ日－施術月     NOT = 受－施術月     ) OR
010620                                     ( 終了フラグ       NOT = SPACE )
010630*
010640                               EVALUATE 通院日Ｗ(参照Ｈ日－施術日)
010650                               WHEN 1
010660                                  MOVE 1 TO 通院日Ｗ(参照Ｈ日－施術日)
010670                               WHEN 2
010680                                  MOVE 3 TO 通院日Ｗ(参照Ｈ日－施術日)
010690                               WHEN 3
010700                                  MOVE 3 TO 通院日Ｗ(参照Ｈ日－施術日)
010710                               WHEN OTHER
010720                                  MOVE 1 TO 通院日Ｗ(参照Ｈ日－施術日)
010730                               END-EVALUATE
010740*
010750*                              / 鍼灸・マ別の通院日 /
010760                               IF Ｈ連計算－施術区分 = 1
010770                                  MOVE 1 TO 通院日２Ｗ(参照Ｈ日－施術日)
010780                               END-IF
010801*
010802                               PERFORM 参照Ｈ日計データＦ読込
010810                       END-PERFORM
010820                   END-IF
010830*----------------------------------------------------------------------------------*
010840*                  / ②マッサージ /
010850                   MOVE 2             TO 参照Ｈ日－施術区分
010860                   MOVE 受－患者番号  TO 参照Ｈ日－患者番号
010870                   MOVE 受－枝番      TO 参照Ｈ日－枝番
010880                   MOVE 受－施術和暦  TO 参照Ｈ日－施術和暦
010890                   MOVE 受－施術年    TO 参照Ｈ日－施術年
010900                   MOVE 受－施術月    TO 参照Ｈ日－施術月
010910                   MOVE 1             TO 参照Ｈ日－施術日
010920*
010930                   START 参照Ｈ日計データＦ KEY IS >= 参照Ｈ日－施術区分
010940                                                      参照Ｈ日－患者コード
010950                                                      参照Ｈ日－施術和暦年月日
010960                   END-START
010970                   IF 状態キー = "00"
010980                       MOVE SPACE TO 終了フラグ
010990                       PERFORM 参照Ｈ日計データＦ読込
011000*                      繰り返し（当月）
011010                       PERFORM UNTIL ( 参照Ｈ日－施術区分   NOT = 2              ) OR
011020                                     ( 参照Ｈ日－患者コード NOT = 受－患者コード ) OR
011030                                     ( 参照Ｈ日－施術和暦   NOT = 受－施術和暦   ) OR
011040                                     ( 参照Ｈ日－施術年     NOT = 受－施術年     ) OR
011050                                     ( 参照Ｈ日－施術月     NOT = 受－施術月     ) OR
011060                                     ( 終了フラグ       NOT = SPACE )
011070*
011080                               EVALUATE 通院日Ｗ(参照Ｈ日－施術日)
011090                               WHEN 1
011100                                  MOVE 3 TO 通院日Ｗ(参照Ｈ日－施術日)
011110                               WHEN 2
011120                                  MOVE 2 TO 通院日Ｗ(参照Ｈ日－施術日)
011130                               WHEN 3
011140                                  MOVE 3 TO 通院日Ｗ(参照Ｈ日－施術日)
011150                               WHEN OTHER
011160                                  MOVE 2 TO 通院日Ｗ(参照Ｈ日－施術日)
011170                               END-EVALUATE
011180*
011190*                              / 鍼灸・マ別の通院日 /
011200                               IF Ｈ連計算－施術区分 = 2
011210                                  MOVE 1 TO 通院日２Ｗ(参照Ｈ日－施術日)
011220                               END-IF
011235*
011240                               PERFORM 参照Ｈ日計データＦ読込
011250                       END-PERFORM
011260                   END-IF
011270*----------------------------------------------------------------------------------*
011280               END-IF
011290*---*
011300               PERFORM 受診者情報Ｆ読込
011310**
011320            END-PERFORM
011330        END-IF
011340*
011350     END-IF.
011360*
011370*================================================================*
011380 受診者情報Ｆ読込 SECTION.
011390*
011400     READ 受診者情報Ｆ NEXT
011410     AT END
011420         MOVE "YES" TO 終了フラグ３
011430     END-READ.
011440*================================================================*
011450*================================================================*
011460 レセＦ固有情報取得 SECTION.
011470*
011480     INITIALIZE 計算拡張情報Ｗ.
011490*
011500     MOVE Ｈ連計算－施術区分  TO  Ｈレセ－施術区分.
011510     MOVE Ｈ連計算－施術和暦  TO  Ｈレセ－施術和暦.
011520     MOVE Ｈ連計算－施術年    TO  Ｈレセ－施術年.
011530     MOVE Ｈ連計算－施術月    TO  Ｈレセ－施術月.
011540     MOVE Ｈ連計算－患者番号  TO  Ｈレセ－患者番号.
011550     MOVE Ｈ連計算－枝番      TO  Ｈレセ－枝番.
011560*
011570* 保険種別　85:生保、91:保険証忘れも計算対象でここにくる
011580* レセ種別 1:一般,2:老人,3:助成,4:労災,5:自賠責,6:自費,7:生保単独,8保険証忘れ /
011590     EVALUATE 保険種別Ｗ
011600     WHEN 85
011610        MOVE 7  TO Ｈレセ－レセ種別
011620     WHEN 91
011630        MOVE 8  TO Ｈレセ－レセ種別
011640     WHEN OTHER
011650        IF 公費種別Ｗ = ZERO
011660           MOVE 1  TO Ｈレセ－レセ種別
011670        ELSE
011680           MOVE 2  TO Ｈレセ－レセ種別
011690        END-IF
011700     END-EVALUATE.
011710*
011720     READ ＨレセプトＦ
011730     NOT INVALID KEY
011740         MOVE Ｈレセ－償還払い区分        TO 本体償還払い区分Ｗ
011750         MOVE Ｈレセ－本体まとめ区分      TO 本体まとめ区分Ｗ
011760*        / マッサージ固有項目退避 /
011770         IF Ｈ連計算－施術区分 = 2
011780            MOVE Ｈレセ－マッサージ数     TO マッサージ局所数Ｗ
011790            MOVE Ｈレセ－変形徒手矯正術数 TO 変形徒手矯正術数Ｗ
011800            MOVE Ｈレセ－マッサージ数     TO Ｊマッサージ局所数Ｗ
011810            MOVE Ｈレセ－変形徒手矯正術数 TO Ｊ変形徒手矯正術数Ｗ
011820
011830*           同意書の施術部位
011840            MOVE Ｈレセ－マッサージ体幹       TO マッサージ体幹同意Ｗ
011850            MOVE Ｈレセ－マッサージ右上肢     TO マッサージ右上肢同意Ｗ
011860            MOVE Ｈレセ－マッサージ左上肢     TO マッサージ左上肢同意Ｗ
011870            MOVE Ｈレセ－マッサージ右下肢     TO マッサージ右下肢同意Ｗ
011880            MOVE Ｈレセ－マッサージ左下肢     TO マッサージ左下肢同意Ｗ
011890            MOVE Ｈレセ－変形徒手矯正術右上肢 TO 変形徒手矯正術右上肢同意Ｗ
011900            MOVE Ｈレセ－変形徒手矯正術左上肢 TO 変形徒手矯正術左上肢同意Ｗ
011910            MOVE Ｈレセ－変形徒手矯正術右下肢 TO 変形徒手矯正術右下肢同意Ｗ
011920            MOVE Ｈレセ－変形徒手矯正術左下肢 TO 変形徒手矯正術左下肢同意Ｗ
011930         END-IF
011940
011941*        前回施術報告書交付情報取得
011942         PERFORM 前回交付情報取得
011943     END-READ.
011950**
011960**
011970     IF 助成種別Ｗ NOT = ZERO
011980        MOVE Ｈ連計算－施術区分  TO  Ｈレセ－施術区分
011990        MOVE Ｈ連計算－施術和暦  TO  Ｈレセ－施術和暦
012000        MOVE Ｈ連計算－施術年    TO  Ｈレセ－施術年
012010        MOVE Ｈ連計算－施術月    TO  Ｈレセ－施術月
012020        MOVE Ｈ連計算－患者番号  TO  Ｈレセ－患者番号
012030        MOVE Ｈ連計算－枝番      TO  Ｈレセ－枝番
012040        MOVE 3                   TO  Ｈレセ－レセ種別
012050        READ ＨレセプトＦ
012060        NOT INVALID KEY
012070            MOVE Ｈレセ－償還払い区分  TO 助成償還払い区分Ｗ
012080        END-READ
012090     END-IF.
012124*
012125*================================================================*
012126 前回交付情報取得 SECTION.
012127*
012128     INITIALIZE 連前交－キー 前回交付情報Ｗ.
012129*
012130     MOVE Ｈレセ－施術区分               TO  連前交－施術区分.
012131     MOVE Ｈレセ－施術和暦年月           TO  連前交－施術和暦年月.
012132     MOVE Ｈレセ－患者コード             TO  連前交－患者コード.
012133     MOVE Ｈレセ－レセ種別               TO  連前交－レセ種別.
012135     CALL   プログラムＷ.
012136     CANCEL プログラムＷ.
012137     MOVE 連前交－前回支給和暦年月       TO 前回交付和暦年月Ｗ.
012138     MOVE 連前交－交付可能和暦年月       TO 交付可能和暦年月Ｗ.
012139     MOVE 連前交－変形徒手フラグ         TO 変形徒手フラグＷ.
012142*
012143*================================================================*
012144*================================================================*
012145 日計データ処理 SECTION.
012146*
HILO  *     DISPLAY "K5-2 " Ｈ連計算－施術区分 " " Ｈ連計算－患者コード  " " 
HILO  *                     Ｈ連計算－施術和暦年月 " 日計データ処理 " 訪問重複Ｆ *>HILO

012150*   / 初期化 /
012160     INITIALIZE 累計情報Ｗ.
012170     INITIALIZE Ｊ累計情報Ｗ.
012180     INITIALIZE マッサージ固有Ｗ.
012190     INITIALIZE Ｊマッサージ固有Ｗ.
006460     INITIALIZE Ｈ連計算２－キー.
           INITIALIZE Ｈレセプト詳細ＦＷ.
012200     MOVE ZERO                TO  マッサージ料月計Ｗ.
012210     MOVE ZERO                TO  変形徒手料月計Ｗ.
      */大阪助成：日の上限を1度も越えない場合、本体の負担額と差が合っても請求しない/20220830
           MOVE "YES" TO 全額患者負担Ｆ. *>HILO
      *
HILO****/特別地域のため？
HILO***006500     MOVE Ｈ連計算－施術和暦年月 TO 受－施術和暦年月
HILO***006560     MOVE Ｈ連計算－患者コード   TO 受－患者コード
HILO***           READ 受診者情報Ｆ
HILO***012060     INVALID KEY
HILO***               MOVE SPACE TO 受－レコード
HILO***012060     NOT INVALID KEY
HILO***012070         CONTINUE
HILO***012080     END-READ.
012220*
012230*    ひと月の間に特殊施術が１回でもあれば特殊施術で計算する
012240     PERFORM 特殊施術有無判定処理.
012250*
012260*    日毎の施術内容と料金を計算する
012270     MOVE Ｈ連計算－施術区分  TO  Ｈ日－施術区分.
012280     MOVE Ｈ連計算－患者番号  TO  Ｈ日－患者番号.
012290     MOVE Ｈ連計算－枝番      TO  Ｈ日－枝番.
012300     MOVE Ｈ連計算－施術和暦  TO  Ｈ日－施術和暦.
012310     MOVE Ｈ連計算－施術年    TO  Ｈ日－施術年.
012320     MOVE Ｈ連計算－施術月    TO  Ｈ日－施術月.
012330     MOVE 1                   TO  Ｈ日－施術日.
012340*
012350     INITIALIZE マッサージ部位別金額欄Ｗ.
012360     INITIALIZE Ｊマッサージ部位別金額欄Ｗ.
012370*
012380     START Ｈ日計データＦ KEY IS >= Ｈ日－施術区分
012390                                    Ｈ日－患者コード
012400                                    Ｈ日－施術和暦年月日
012410     END-START.
012420*
012430     IF 状態キー = "00"
012440         MOVE SPACE TO 終了フラグ
012450         PERFORM Ｈ日計データＦ読込
012460*
012470         IF ( Ｈ日－施術区分    = Ｈ連計算－施術区分   ) AND
012480            ( Ｈ日－患者コード  = Ｈ連計算－患者コード ) AND
012490            ( Ｈ日－施術和暦    = Ｈ連計算－施術和暦   ) AND
012500            ( Ｈ日－施術年      = Ｈ連計算－施術年     ) AND
012510            ( Ｈ日－施術月      = Ｈ連計算－施術月     ) AND
012520            ( 終了フラグ        = SPACE )
012530            PERFORM レセＦ固有情報取得
012540         END-IF
012550*
012560*        繰り返し（当月）
012570         PERFORM UNTIL ( Ｈ日－施術区分   NOT = Ｈ連計算－施術区分   ) OR
012580                       ( Ｈ日－患者コード NOT = Ｈ連計算－患者コード ) OR
012590                       ( Ｈ日－施術和暦   NOT = Ｈ連計算－施術和暦   ) OR
012600                       ( Ｈ日－施術年     NOT = Ｈ連計算－施術年     ) OR
012610                       ( Ｈ日－施術月     NOT = Ｈ連計算－施術月     ) OR
012620                       ( 終了フラグ       NOT = SPACE )
                   IF Ｈ連計算－施術和暦年月 >= 50610 
                       PERFORM 同一建物患者数取得
                   END-IF
HILO***       IF Ｈ連計算４－同一建物患者数取得Ｆ = ZERO
HILO***       DISPLAY "610-11 ■■■■■■ 患者№" Ｈ日－患者コード " 施術日" Ｈ日－施術和暦年月日 
HILO***              " 同一建物患者数= " 同一建物患者数Ｗ "人 ■■■■■■■ 受" 受－患者コード END-IF
HILO  *    DISPLAY "610-4 " Ｈ日－施術和暦年月日 " 当日の同一建物患者数=" 同一建物患者数Ｗ
012630**          / 計算Ｗ初期化 /
012640             INITIALIZE 計算情報Ｗ 特別地域加算料Ｗ
012650*
HILO  */Ｈ日－往療日区分 ゼロ：通所・１：訪問   ？訪問？
      */20240627
      *             IF (受－鍼灸特別地域区分 = 1) AND (Ｈ日－往療日区分 NOT = ZERO)
      *                 MOVE 料Ｍ特別地域加算料Ｗ TO 特別地域加算料単価Ｗ
      *                 COMPUTE 特別地域加算料Ｗ   = 特別地域加算料Ｗ   + 特別地域加算料単価Ｗ
      *                 COMPUTE 特別地域加算回数Ｗ = 特別地域加算回数Ｗ + 1
      *             END-IF
      *
013550             PERFORM Ｈ往療実績Ｆ読込
                   IF Ｈ往実－往療場所施設コード NOT = ZERO
                       MOVE Ｈ往実－往療場所施設コード TO 施設－施設コード
                       READ 施設マスタ
                       INVALID KEY
                           MOVE SPACE TO 施設－レコード
                       END-READ
                   END-IF
                   IF ((Ｈ日－往療日区分   NOT = ZERO) AND
000600*                (((Ｈ往実－往療場所施設コード NOT = ZERO)  AND (施設－鍼灸特別地域区分 = "1")) OR
000600*                 ((Ｈ往実－往療場所施設コード     = ZERO)  AND (受－鍼灸特別地域区分   = "1")))
                       (Ｈ日－特別地域加算区分 = 1))
                       MOVE 料Ｍ特別地域加算料Ｗ TO 特別地域加算料単価Ｗ
HILO                   COMPUTE 累計特別地域加算料Ｗ  = 累計特別地域加算料Ｗ + 特別地域加算料単価Ｗ
HILO                   MOVE    特別地域加算料単価Ｗ TO 特別地域加算料Ｗ
                       COMPUTE 特別地域加算回数Ｗ    = 特別地域加算回数Ｗ   + 1
HILO***                   DISPLAY "810-6-1 ●"
HILO****                受－" 受－鍼灸特別地域区分 " 施設－"     施設－鍼灸特別地域区分
HILO***             "  特 区分="   Ｈ日－特別地域加算区分
HILO***             "  特 加算料=" 特別地域加算料Ｗ 
HILO***             "  特 単価="   特別地域加算料単価Ｗ
HILO***             "  特 算回数=" 特別地域加算回数Ｗ " ●"
                   END-IF
012660*           / 負傷入力必須 /
012670             IF Ｈ日－負傷主キー NOT = ZERO
012680*
012690                EVALUATE Ｈ日－施術区分
012700                WHEN 1
012710                   PERFORM はり日計料金計算
012720                WHEN 2
012730                   PERFORM マッサージ日計料金計算
012740*                  月合計を集計
012750                   COMPUTE マッサージ料月計Ｗ = マッサージ料月計Ｗ + マッサージ料Ｗ
012760                   COMPUTE 変形徒手料月計Ｗ   = 変形徒手料月計Ｗ   + マッサージ変形徒手矯正術Ｗ
012770                WHEN OTHER
012780                   DISPLAY "日計施術区分エラー　日付：" Ｈ日－施術和暦年月日   UPON CONS
012790*                 /-- 金額ZERO--/
012800                   MOVE ZERO  TO Ｈ日－費用額
012810                   MOVE ZERO  TO Ｈ日－一部負担金
012820                   MOVE ZERO  TO Ｈ日－例外レセ用金額欄
012830                   COMPUTE Ｈ日－入金額 = Ｈ日－一部負担金 + Ｈ日－自費額
012840                END-EVALUATE
      */20240704↓↓↓/*
002830*/20240601（1,2,3,4）
                      EVALUATE TRUE
                      WHEN 同一建物患者数Ｗ = ZERO
001107                    MOVE ZERO TO Ｈ日－訪問施術料区分
                      WHEN 同一建物患者数Ｗ =  1
      *                   /１人
001107                    MOVE 1 TO Ｈ日－訪問施術料区分
                      WHEN 同一建物患者数Ｗ =  2
      *                   /２人
001107                    MOVE 2 TO Ｈ日－訪問施術料区分
                      WHEN 同一建物患者数Ｗ <= 9
      *                   /３人～９人
001107                    MOVE 3 TO Ｈ日－訪問施術料区分
                      WHEN 同一建物患者数Ｗ >= 10
      *                   /10人以上
001107                    MOVE 4 TO Ｈ日－訪問施術料区分
                      END-EVALUATE
001107                MOVE 同一建物患者数Ｗ TO Ｈ日－同一建物患者数

HILO  *                DISPLAY "Ｈ日－訪問施術料区分 " Ｈ日－訪問施術料区分
HILO  *                DISPLAY "Ｈ日－同一建物患者数 " Ｈ日－同一建物患者数
      */20240704↑↑↑/*
      */同一日に鍼灸マッサージ両方施術している場合/20240830↓↓↓
HILO  *     DISPLAY "k5-3 " Ｈ日－レコードキー " 訪問重複Ｆ=" 訪問重複Ｆ
           IF Ｈ日－施術区分 = 1
               MOVE 訪問重複Ｆ TO Ｈ日－訪問重複Ｆ
           END-IF
      */同一日に鍼灸マッサージ両方施術している場合/20240830↑↑↑
HILO***                  DISPLAY "k5-4 Ｈ日－同一建物患者数 " Ｈ日－同一建物患者数
012850**
012860                REWRITE Ｈ日－レコード
012870                IF 状態キー NOT = "00"
012880                     MOVE NC"Ｈ日計データＦ" TO ファイル名
012890                     PERFORM エラー表示
012900                END-IF
012910             END-IF
012920**
012930             PERFORM Ｈ日計データＦ読込
012940         END-PERFORM
012950     END-IF.
012960*
012970*================================================================*
012980 特殊施術有無判定処理 SECTION.
012990*
013000*   / 初期化 /
013010     MOVE ZERO                TO  特殊施術フラグＷ.
013020     MOVE ZERO                TO  Ｊ特殊施術フラグＷ.
013030*
013040     MOVE Ｈ連計算－施術区分  TO  Ｈ日－施術区分.
013050     MOVE Ｈ連計算－患者番号  TO  Ｈ日－患者番号.
013060     MOVE Ｈ連計算－枝番      TO  Ｈ日－枝番.
013070     MOVE Ｈ連計算－施術和暦  TO  Ｈ日－施術和暦.
013080     MOVE Ｈ連計算－施術年    TO  Ｈ日－施術年.
013090     MOVE Ｈ連計算－施術月    TO  Ｈ日－施術月.
013100     MOVE 1                   TO  Ｈ日－施術日.
013110*
013120     START Ｈ日計データＦ KEY IS >= Ｈ日－施術区分
013130                                    Ｈ日－患者コード
013140                                    Ｈ日－施術和暦年月日
013150     END-START.
013160*
013170     IF 状態キー = "00"
013180         MOVE SPACE TO 終了フラグ
013190         PERFORM Ｈ日計データＦ読込
013200*
013210*        繰り返し（当月）
013220         PERFORM UNTIL ( Ｈ日－施術区分   NOT = Ｈ連計算－施術区分   ) OR
013230                       ( Ｈ日－患者コード NOT = Ｈ連計算－患者コード ) OR
013240                       ( Ｈ日－施術和暦   NOT = Ｈ連計算－施術和暦   ) OR
013250                       ( Ｈ日－施術年     NOT = Ｈ連計算－施術年     ) OR
013260                       ( Ｈ日－施術月     NOT = Ｈ連計算－施術月     ) OR
013270                       ( 終了フラグ       NOT = SPACE )
013280*
013290             IF Ｈ日－負傷主キー NOT = ZERO
013300*                １日～末日の間に特殊施術があったか？
013310                 IF Ｈ日－特殊施術区分 = 1
013320                     MOVE 1    TO  特殊施術フラグＷ
013330*
013340*                    助成開始日以降に特殊施術があったか？
013350                     IF ( 助成月途中開始日Ｗ NOT = ZERO ) AND ( 助成月途中開始日Ｗ <= Ｈ日－施術日)
013360                         MOVE 1    TO  Ｊ特殊施術フラグＷ
013370                     END-IF
013380                 END-IF
013401             END-IF
013402**
013410             PERFORM Ｈ日計データＦ読込
013420         END-PERFORM
013430     END-IF.
013440*
013450*================================================================*
013460 Ｈ日計データＦ読込 SECTION.
013470*
013480     READ Ｈ日計データＦ NEXT
013490     AT END
013500         MOVE "YES"  TO 終了フラグ
013510     END-READ.
013520
013530**  / Ｈ往療実績Ｆ読込 /
013540     IF 終了フラグ = SPACE
013550         PERFORM Ｈ往療実績Ｆ読込
013560     END-IF.
013570*
013580*================================================================*
013590 参照Ｈ日計データＦ読込 SECTION.
013600*
013610     READ 参照Ｈ日計データＦ NEXT
013620     AT END
013630         MOVE "YES"  TO 終了フラグ
013640     END-READ.
013650*
013660*================================================================*
013670 Ｈ往療実績Ｆ読込 SECTION.
013680*
013690     INITIALIZE Ｈ往実－レコード.
013700     MOVE Ｈ日－レコードキー TO Ｈ往実－レコードキー.
013710     READ Ｈ往療実績Ｆ
013720     INVALID
013730         INITIALIZE Ｈ往実－レコード
013740     END-READ.
013750*
013760*================================================================*
013770 はり日計料金計算 SECTION.
013780*--------------------------------------------*
013790*（施術内容区分）
013800*  1:はり
013810*  2:はり＋電気
013820*  3:きゅう
013830*  4:きゅう＋電気
013840*  5:はり＋きゅう
013850*  6:はり＋きゅう＋電気
013860*--------------------------------------------*
013870*
013880*---- ①１術か２術か ------------------------------------------*
013890*     IF Ｈ日－施術内容区分 = 5 OR 6
013900**       /  診療区分 1:後療、2:初回
013910*        IF Ｈ日－診療区分 = 2
013920*           MOVE Ｈ２術初回Ｗ  TO はり術額Ｗ
013930*        ELSE
013940*           MOVE Ｈ２術Ｗ      TO はり術額Ｗ
013950*        END-IF
013960*     ELSE
013970**       /  診療区分 1:後療、2:初回
013980*        IF Ｈ日－診療区分 = 2
013990*           MOVE Ｈ１術初回Ｗ  TO はり術額Ｗ
014000*        ELSE
014010*           MOVE Ｈ１術Ｗ      TO はり術額Ｗ
014020*        END-IF
014030*     END-IF.
014040**
014050*--------------------------------------------------------------*
*****************************************************************
hilo  *     DISPLAY "610-1 施術区分" Ｈ日－施術区分      
      *            " 施術和暦年月日" Ｈ日－施術和暦年月日
      *            " 患者コード"     Ｈ日－患者コード
      *            " 突発往療区分"   Ｈ日－突発往療区分

           MOVE ZERO TO 訪問重複Ｆ.
           IF (Ｈ日－往療日区分 NOT = ZERO) AND (Ｈ日－突発往療区分 NOT = 1)
               MOVE 2                    TO 参照Ｈ日－施術区分      
               MOVE Ｈ日－施術和暦年月日 TO 参照Ｈ日－施術和暦年月日
               MOVE Ｈ日－患者コード     TO 参照Ｈ日－患者コード    
               READ 参照Ｈ日計データＦ
               INVALID KEY
                  CONTINUE
               NOT INVALID KEY
                  IF (参照Ｈ日－往療日区分 NOT = ZERO) AND (参照Ｈ日－突発往療区分 NOT = 1)
HILO***                  DISPLAY "k5-1 同一日に鍼マ有① " Ｈ日－患者コード Ｈ日－施術和暦年月日
                      MOVE 1 TO 訪問重複Ｆ
                  END-IF
               END-READ
           END-IF.
      */突発往療のチェック↓↓↓/20240903
001107     IF Ｈ日－突発往療区分 = 1 
               MOVE Ｈ日－施術区分       TO 連入２０１－施術区分       
               MOVE Ｈ日－施術和暦年月日 TO 連入２０１－施術和暦年月日 
               MOVE Ｈ日－患者コード     TO 連入２０１－患者コード     
               MOVE SPACE                TO 連入２０１－エラーフラグ
               MOVE ZERO TO 突発不可Ｆ
022360         CALL   "TOPPATU"
022370         CANCEL "TOPPATU"
               IF 連入２０１－エラーフラグ NOT = SPACE
                   MOVE 1 TO 突発不可Ｆ
               END-IF
               OPEN I-O 更新用Ｈ往療実績Ｆ
004850             MOVE NC"Ｈ往実" TO ファイル名
004860             PERFORM オープンチェック
               MOVE Ｈ日－施術区分       TO 更Ｈ往実－施術区分
002380         MOVE Ｈ日－施術和暦年月日 TO 更Ｈ往実－施術和暦年月日
002390         MOVE Ｈ日－患者コード     TO 更Ｈ往実－患者コード
               READ 更新用Ｈ往療実績Ｆ
               NOT INVALID KEY
                   IF 連入２０１－エラーフラグ NOT = SPACE
                       MOVE ZERO             TO 更Ｈ往実－往療金額
                   ELSE
                       MOVE Ｈ往療基本料金Ｗ TO 更Ｈ往実－往療金額
                   END-IF
HILO***       IF Ｈ連計算４－同一建物患者数取得Ｆ = ZERO DISPLAY "更Ｈ往実－往療金額 " 更Ｈ往実－往療金額 END-IF
                   REWRITE 更Ｈ往実－レコード
                   IF 状態キー NOT = "00"
                       MOVE NC"更Ｈ往実" TO ファイル名
                       PERFORM エラー表示
                   END-IF
               END-READ
               CLOSE 更新用Ｈ往療実績Ｆ
           END-IF.
HILO***       DISPLAY "K5-A 連入２０１－エラーフラグ " 連入２０１－エラーフラグ "'" 突発不可Ｆ "'"
HILO  */突発往療のチェック↑↑↑/20240903
*****************************************************************
      **ｘ実際には Ｈ日－訪問施術料区分 と Ｈ日－突発往療区分 を使う
      */20240626
HILO***     DISPLAY "610-6 " Ｈ日－往療日区分 " " Ｈ日－突発往療区分
      */Ｈ日－往療日区分 ゼロ：通所・１：訪問
      *     IF Ｈ日－往療日区分 = ZERO
           IF (Ｈ日－往療日区分 = ZERO) OR (Ｈ日－突発往療区分 = 1) OR (訪問重複Ｆ = 1)
HILO***           DISPLAY "610-7 通所" Ｈ日－レコードキー
013890         IF Ｈ日－施術内容区分 = 5 OR 6
013900*           /  診療区分 1:後療、2:初回
013910            IF Ｈ日－診療区分 = 2
013920               MOVE Ｈ２術初回Ｗ  TO はり術額Ｗ
013930            ELSE
013940               MOVE Ｈ２術Ｗ      TO はり術額Ｗ
013950            END-IF
013960         ELSE
013970*           /  診療区分 1:後療、2:初回
013980            IF Ｈ日－診療区分 = 2
013990               MOVE Ｈ１術初回Ｗ  TO はり術額Ｗ
014000            ELSE
014010               MOVE Ｈ１術Ｗ      TO はり術額Ｗ
014020            END-IF
014030         END-IF
013890         IF Ｈ日－施術内容区分 = 5 OR 6
013940             MOVE Ｈ２術Ｗ TO はりきゅう通所施術料単価２Ｗ   
001420             COMPUTE はりきゅう通所施術料回数２Ｗ   = はりきゅう通所施術料回数２Ｗ   + 1
001420             COMPUTE はりきゅう通所施術料施術料２Ｗ = はりきゅう通所施術料施術料２Ｗ + Ｈ２術Ｗ
013960         ELSE
014010             MOVE Ｈ１術Ｗ TO はりきゅう通所施術料単価１Ｗ   
001420             COMPUTE はりきゅう通所施術料回数１Ｗ   = はりきゅう通所施術料回数１Ｗ   + 1
001420             COMPUTE はりきゅう通所施術料施術料１Ｗ = はりきゅう通所施術料施術料１Ｗ + Ｈ１術Ｗ
014030         END-IF
           ELSE
HILO***           DISPLAY "610-8 訪問" Ｈ日－レコードキー
               EVALUATE TRUE
               WHEN 同一建物患者数Ｗ =   1
      *            /１人
013890             IF Ｈ日－施術内容区分 = 5 OR 6
      *                /２術
000332                 MOVE Ｈ２術訪問施術料１Ｗ TO はり術額Ｗ
001410                 MOVE Ｈ２術訪問施術料１Ｗ TO はりきゅう訪問施術料１単価２Ｗ 
001420                 COMPUTE はりきゅう訪問施術料１回数２Ｗ   = はりきゅう訪問施術料１回数２Ｗ   + 1
001420                 COMPUTE はりきゅう訪問施術料１施術料２Ｗ = はりきゅう訪問施術料１施術料２Ｗ + Ｈ２術訪問施術料１Ｗ
013960             ELSE
      *                /１術
000331                 MOVE Ｈ１術訪問施術料１Ｗ TO はり術額Ｗ
001410                 MOVE Ｈ１術訪問施術料１Ｗ TO はりきゅう訪問施術料１単価１Ｗ
001420                 COMPUTE はりきゅう訪問施術料１回数１Ｗ   = はりきゅう訪問施術料１回数１Ｗ   + 1
001420                 COMPUTE はりきゅう訪問施術料１施術料１Ｗ = はりきゅう訪問施術料１施術料１Ｗ + Ｈ１術訪問施術料１Ｗ
014030             END-IF
               WHEN 同一建物患者数Ｗ =   2
      *            /２人
013890             IF Ｈ日－施術内容区分 = 5 OR 6
      *                /２術
000332                 MOVE Ｈ２術訪問施術料２Ｗ TO はり術額Ｗ
001410                 MOVE Ｈ２術訪問施術料２Ｗ TO はりきゅう訪問施術料２単価２Ｗ 
001420                 COMPUTE はりきゅう訪問施術料２回数２Ｗ   = はりきゅう訪問施術料２回数２Ｗ   + 1
001420                 COMPUTE はりきゅう訪問施術料２施術料２Ｗ = はりきゅう訪問施術料２施術料２Ｗ + Ｈ２術訪問施術料２Ｗ 
013960             ELSE
      *                /１術
000331                 MOVE Ｈ１術訪問施術料２Ｗ TO はり術額Ｗ
001410                 MOVE Ｈ１術訪問施術料２Ｗ TO はりきゅう訪問施術料２単価１Ｗ     
001420                 COMPUTE はりきゅう訪問施術料２回数１Ｗ   = はりきゅう訪問施術料２回数１Ｗ   + 1
001420                 COMPUTE はりきゅう訪問施術料２施術料１Ｗ = はりきゅう訪問施術料２施術料１Ｗ +  Ｈ１術訪問施術料２Ｗ 
014030             END-IF
               WHEN 同一建物患者数Ｗ <= 9
      *            /３人～９人
013890             IF Ｈ日－施術内容区分 = 5 OR 6
      *                /２術
000332                 MOVE Ｈ２術訪問施術料３Ｗ TO はり術額Ｗ
001410                 MOVE Ｈ２術訪問施術料３Ｗ TO はりきゅう訪問施術料３単価２Ｗ 
001420                 COMPUTE はりきゅう訪問施術料３回数２Ｗ   = はりきゅう訪問施術料３回数２Ｗ   + 1
001420                 COMPUTE はりきゅう訪問施術料３施術料２Ｗ = はりきゅう訪問施術料３施術料２Ｗ + Ｈ２術訪問施術料３Ｗ  
013960             ELSE
      *                /１術
000331                 MOVE Ｈ１術訪問施術料３Ｗ TO はり術額Ｗ
001410                 MOVE Ｈ１術訪問施術料３Ｗ TO はりきゅう訪問施術料３単価１Ｗ 
001420                 COMPUTE はりきゅう訪問施術料３回数１Ｗ   = はりきゅう訪問施術料３回数１Ｗ   + 1
001420                 COMPUTE はりきゅう訪問施術料３施術料１Ｗ = はりきゅう訪問施術料３施術料１Ｗ + Ｈ１術訪問施術料３Ｗ  
014030             END-IF
               WHEN 同一建物患者数Ｗ >= 10
      *            /３人～９人
013890             IF Ｈ日－施術内容区分 = 5 OR 6
      *                /２術
000332                 MOVE Ｈ２術訪問施術料４Ｗ TO はり術額Ｗ
001410                 MOVE Ｈ２術訪問施術料４Ｗ TO はりきゅう訪問施術料４単価２Ｗ 
001420                 COMPUTE はりきゅう訪問施術料４回数２Ｗ   = はりきゅう訪問施術料４回数２Ｗ   + 1
001420                 COMPUTE はりきゅう訪問施術料４施術料２Ｗ = はりきゅう訪問施術料４施術料２Ｗ + Ｈ２術訪問施術料４Ｗ  
013960             ELSE
      *                /１術
000331                 MOVE Ｈ１術訪問施術料４Ｗ TO はり術額Ｗ
001410                 MOVE Ｈ１術訪問施術料４Ｗ TO はりきゅう訪問施術料４単価１Ｗ 
001420                 COMPUTE はりきゅう訪問施術料４回数１Ｗ   = はりきゅう訪問施術料４回数１Ｗ   + 1
001420                 COMPUTE はりきゅう訪問施術料４施術料１Ｗ = はりきゅう訪問施術料４施術料１Ｗ + Ｈ１術訪問施術料４Ｗ  
014030             END-IF
               END-EVALUATE
      */20240917↓↓↓
013910*            DISPLAY "k5-3 " Ｈ日－診療区分
013900*    /  診療区分 1:後療、2:初回
013910         IF Ｈ日－診療区分 = 2
013890             IF Ｈ日－施術内容区分 = 5 OR 6
013920                 COMPUTE はり術額Ｗ = はり術額Ｗ + ２術初険料Ｗ
013960             ELSE
013920                 COMPUTE はり術額Ｗ = はり術額Ｗ + １術初険料Ｗ
013950             END-IF
HILO***           DISPLAY "610-11-1 はり術額Ｗ＋初険料=" はり術額Ｗ
013950         END-IF
      */20240917↑↑↑
           END-IF.
      *
013890     EVALUATE Ｈ日－施術内容区分
           WHEN 1
           WHEN 2
001420         COMPUTE はり１術回数Ｗ       = はり１術回数Ｗ       + 1
           WHEN 3
           WHEN 4
001420         COMPUTE きゅう１術回数Ｗ     = きゅう１術回数Ｗ     + 1
           WHEN 5
           WHEN 6
001420         COMPUTE はりきゅう２術回数Ｗ = はりきゅう２術回数Ｗ + 1
           END-EVALUATE.
      *
HILO***       DISPLAY "610-10 はり術額Ｗ=" はり術額Ｗ
014060*
014070*---- ②電療料-------------------------------------------------*
014080     IF Ｈ日－施術内容区分 = 2 OR 4 OR 6
014090        MOVE Ｈ電療料Ｗ  TO はり電療料Ｗ
014100     END-IF.
014110*
014120*--------------------------------------------------------------*
014130*
014140*---- ③往療料-------------------------------------------------*
HILO***       DISPLAY "61-53 Ｈ日－突発往療区分" Ｈ日－突発往療区分 " "  Ｈ日－往療距離 " " 
hilo***                      Ｈ往実－按分なし特定区分 " 突発不可Ｆ" 突発不可Ｆ
HILO  */↓↓↓突発往料
HILO  */Ｈ日－突発往療区分
*******20240903
      *     IF Ｈ日－突発往療区分 = 1
           IF (Ｈ日－突発往療区分 = 1) AND (突発不可Ｆ = ZERO)

HILO  *     DISPLAY "61-54 " Ｈ日－往療距離 " " Ｈ往実－按分なし特定区分
HILO  *     MOVE 50 TO Ｈ日－往療距離 *>t

014150* (日ごと実施)
014160*    / 往療日区分 1:往療  特定≠２を条件追加
014170     IF (Ｈ日－往療日区分 = 1) AND (Ｈ日－往療距離 NOT = ZERO) AND (Ｈ往実－按分なし特定区分 NOT = 2)

HILO  *     DISPLAY "61-55 " 

014180        IF Ｈ日－往療距離 <= 往療基本距離
HILO  *     DISPLAY "61-56 " Ｈ往療基本料金Ｗ
014190            MOVE Ｈ往療基本料金Ｗ  TO はり往療料Ｗ
014200*           (レセプトＦ用回数累計)
014210            COMPUTE はり往療回数Ｗ = はり往療回数Ｗ + 1
014220        ELSE
HILO  *     DISPLAY "61-57 " 
014230*           整数にするため、１０を掛ける
014240            COMPUTE 往療被除数Ｗ = Ｈ日－往療距離 * 10
014250            COMPUTE 往療除数Ｗ   = 往療基本距離 * 10
014260            DIVIDE 往療除数Ｗ INTO 往療被除数Ｗ GIVING 往療商Ｗ
014270                              REMAINDER 往療剰余Ｗ
014280            COMPUTE はり往療料Ｗ = Ｈ往療基本料金Ｗ +
014290                             ( Ｈ往療追加料金Ｗ * ( 往療商Ｗ - 1 ) )
014300            IF 往療剰余Ｗ > ZERO
014310                COMPUTE はり往療料Ｗ = はり往療料Ｗ + Ｈ往療追加料金Ｗ
014320            END-IF
014330**------------------------------------------------------------------------------**
014340*           往療料上限料金算定　20160901
014350            INITIALIZE Ｈ連最往－キー
014360            MOVE 1                  TO Ｈ連最往－施術区分
014370            MOVE Ｈ日－施術和暦年月 TO Ｈ連最往－施術和暦年月
014380            MOVE Ｈ日－往療距離     TO Ｈ連最往－往療距離
014390            CALL "MAXOURYO"
014400            IF Ｈ連最往－該当区分 = 1
014410                COMPUTE はり往療料Ｗ = Ｈ往療基本料金Ｗ + Ｈ連最往－往療加算金額
014420            END-IF
014430            CANCEL "MAXOURYO"
014440**------------------------------------------------------------------------------**
014450**------------------------------------------------------------------------------**
014460*
014470*    (レセプトＦ用回数・加算料・往療加算単価、単価は１つのみ→最後の日のデータだが、全部同じ距離なら問題なし！)
014480            COMPUTE はり往療回数Ｗ = はり往療回数Ｗ + 1
014490            COMPUTE はり往療加算回数Ｗ = はり往療加算回数Ｗ + 1
014500            COMPUTE はり往療加算単価Ｗ = はり往療料Ｗ -  Ｈ往療基本料金Ｗ
014510            COMPUTE はり往療加算料Ｗ = はり往療加算料Ｗ + (はり往療料Ｗ -  Ｈ往療基本料金Ｗ)
014520**         /-- レセプトＦ用：加算の往療距離が違う日があるか判定 --/
HILO  *     DISPLAY "61-58 " 
014530            IF 退避往療距離Ｗ = ZERO
014540               MOVE Ｈ日－往療距離  TO 退避往療距離Ｗ
014550            ELSE
014560               IF Ｈ日－往療距離 NOT = 退避往療距離Ｗ
014570                  MOVE 1 TO 往療加算複数判定Ｗ
014580               END-IF
014590            END-IF
014600**
014610        END-IF

HILO***       DISPLAY "610-2-4 はり往療料Ｗ" はり往療料Ｗ " " Ｈ日－往療按分区分 "'" Ｈ日－往療按分金額 "'"

      */按分は使わない/20240919↓↓↓
014620**------------------------------------------------------------------------------------*
014630**    ※ 上記後、按分の場合は、往療料を上書き
014640*        IF Ｈ日－往療按分区分 = 1
014650*           MOVE Ｈ日－往療按分金額  TO はり往療料Ｗ
014660*        END-IF
      */按分は使わない/20240919↑↑↑
014670*------------------------------------------------------------------------------------*
014680*    ※ 上記後、レセプトＦ用に往療料を累計
014690        COMPUTE はり累計往療料Ｗ = はり累計往療料Ｗ + はり往療料Ｗ
014700*------------------------------------------------------------------------------------*
014710*

HILO***       DISPLAY "610-2-2 はり往療料Ｗ" はり往療料Ｗ

014720     END-IF
HILO***       DISPLAY "610-2-3 はり往療料Ｗ" はり往療料Ｗ

014730
014740*    ※ 往療料なしの場合はゼロ円上書き
014750     IF Ｈ往実－按分なし特定区分 = 2
014760        ADD 1                    TO 往療０円カウンタ
014770        ADD 1                    TO はり往療回数Ｗ
014780        MOVE ZERO                TO はり往療料Ｗ
014790     END-IF

014720     END-IF.
HILO  */↑↑↑突発往料

HILO***       DISPLAY "610-2-1 はり往療料Ｗ" はり往療料Ｗ

014800*
014801*    施術報告書交付料
014806     IF (Ｈ日－施術和暦年月日     >= "4301001") AND
014807        (Ｈ日－施術報告書交付区分  = 1) AND
014808        (Ｈ日－施術和暦年月       >= 交付可能和暦年月Ｗ)
014811         ADD  1                       TO はり報告書交付回数Ｗ
014812         ADD  施術報告書交付料単価Ｗ  TO はり報告書交付料Ｗ
014813         MOVE 施術報告書交付料単価Ｗ  TO 計算用報告書交付料Ｗ
014814     ELSE
014815         MOVE ZERO                    TO 計算用報告書交付料Ｗ
014816     END-IF.
014818*
014819*--------------------------------------------------------------*
014820*
014830* 合計
014840     COMPUTE はり合計額Ｗ = はり術額Ｗ + はり電療料Ｗ + はり往療料Ｗ +
014841                            計算用報告書交付料Ｗ
      */20240627
                                + 特別地域加算料Ｗ.

HILO***       DISPLAY "61-52 " 受－患者氏名(1:10) "： 合計額" はり合計額Ｗ  
HILO***                  " はり術" はり術額Ｗ  
HILO***                  " 電療"   はり電療料Ｗ  
HILO***                  " 往療"   はり往療料Ｗ 
HILO***                  " 報告書" 計算用報告書交付料Ｗ  
HILO***                  " 特"     特別地域加算料Ｗ


014850* 連結の費用額セット
014860     MOVE はり合計額Ｗ TO Ｈ連計算－費用額.
014870*
014880* レセ用累計
014890     COMPUTE はり累計額Ｗ = はり累計額Ｗ + はり合計額Ｗ.
014900*
014910*--------------------------------------------------------------*
014920*
014930* / レセプトＦ用回数累計 /
014940     IF Ｈ日－診療区分 = 2
014950*   (初回）
014960        MOVE Ｈ日－施術内容区分 TO はり初回施術内容区分Ｗ
014970***        MOVE はり合計額Ｗ       TO はり初回料Ｗ
014980***      / はり往療料Ｗ 足さない /
014990        COMPUTE はり初回料Ｗ = はり術額Ｗ + はり電療料Ｗ
015000***
015010     ELSE
015020*   (後療）
015030        EVALUATE Ｈ日－施術内容区分
015040        WHEN 1
015050           COMPUTE はり回数Ｗ = はり回数Ｗ + 1
015060        WHEN 2
015070           COMPUTE はり電気回数Ｗ = はり電気回数Ｗ + 1
015080        WHEN 3
015090           COMPUTE きゅう回数Ｗ = きゅう回数Ｗ + 1
015100        WHEN 4
015110           COMPUTE きゅう電気回数Ｗ = きゅう電気回数Ｗ + 1
015120        WHEN 5
015130           COMPUTE はりきゅう回数Ｗ = はりきゅう回数Ｗ + 1
015140        WHEN 6
015150           COMPUTE はりきゅう電気回数Ｗ = はりきゅう電気回数Ｗ + 1
015160        END-EVALUATE
015170     END-IF.
015187*
015190*--------------------------------------------------------------*
015200*
015210     PERFORM 日計負担額取得.
015220*
015231*--------------------------------------------------------------*
015250*
015260*--------------------------------------------------------------*
015270* 月途中助成のレセＦ用の集計
015280*
015290     IF ( 助成月途中開始日Ｗ NOT = ZERO ) AND ( Ｈ日－施術日 >= 助成月途中開始日Ｗ )
015300*
015310        IF Ｈ日－診療区分 = 2
015320*         (初回）
015330           MOVE Ｈ日－施術内容区分 TO Ｊはり初回施術内容区分Ｗ
015340           COMPUTE Ｊはり初回料Ｗ = はり術額Ｗ + はり電療料Ｗ
015350        ELSE
015360*         (後療）
015370           EVALUATE Ｈ日－施術内容区分
015380           WHEN 1
015390              COMPUTE Ｊはり回数Ｗ = Ｊはり回数Ｗ + 1
015400           WHEN 2
015410              COMPUTE Ｊはり電気回数Ｗ = Ｊはり電気回数Ｗ + 1
015420           WHEN 3
015430              COMPUTE Ｊきゅう回数Ｗ = Ｊきゅう回数Ｗ + 1
015440           WHEN 4
015450              COMPUTE Ｊきゅう電気回数Ｗ = Ｊきゅう電気回数Ｗ + 1
015460           WHEN 5
015470              COMPUTE Ｊはりきゅう回数Ｗ = Ｊはりきゅう回数Ｗ + 1
015480           WHEN 6
015490              COMPUTE Ｊはりきゅう電気回数Ｗ = Ｊはりきゅう電気回数Ｗ + 1
015500           END-EVALUATE
015510        END-IF
015520***
015530***     特定≠２を条件追加
015540        IF (Ｈ日－往療日区分 = 1) AND (Ｈ日－往療距離 NOT = ZERO) AND (Ｈ往実－按分なし特定区分 NOT = 2)
015550           IF Ｈ日－往療距離 <= 往療基本距離
015560               MOVE Ｈ往療基本料金Ｗ  TO はり往療料Ｗ
015570*              (レセプトＦ用回数累計)
015580               COMPUTE Ｊはり往療回数Ｗ = Ｊはり往療回数Ｗ + 1
015590           ELSE
015600*              整数にするため、１０を掛ける
015610               COMPUTE 往療被除数Ｗ = Ｈ日－往療距離 * 10
015620               COMPUTE 往療除数Ｗ   = 往療基本距離 * 10
015630               DIVIDE 往療除数Ｗ INTO 往療被除数Ｗ GIVING 往療商Ｗ
015640                                 REMAINDER 往療剰余Ｗ
015650               COMPUTE はり往療料Ｗ = Ｈ往療基本料金Ｗ +
015660                                ( Ｈ往療追加料金Ｗ * ( 往療商Ｗ - 1 ) )
015670               IF 往療剰余Ｗ > ZERO
015680                   COMPUTE はり往療料Ｗ = はり往療料Ｗ + Ｈ往療追加料金Ｗ
015690               END-IF
015700**------------------------------------------------------------------------------**
015710*              往療料上限料金算定　20160901
015720               INITIALIZE Ｈ連最往－キー
015730               MOVE 1                  TO Ｈ連最往－施術区分
015740               MOVE Ｈ日－施術和暦年月 TO Ｈ連最往－施術和暦年月
015750               MOVE Ｈ日－往療距離     TO Ｈ連最往－往療距離
015760               CALL "MAXOURYO"
015770               IF Ｈ連最往－該当区分 = 1
015780                   COMPUTE はり往療料Ｗ = Ｈ往療基本料金Ｗ + Ｈ連最往－往療加算金額
015790               END-IF
015800               CANCEL "MAXOURYO"
015810**------------------------------------------------------------------------------**
015820*
015830*      (レセプトＦ用回数・加算料・往療加算単価、単価は１つのみ→最後の日のデータだが、全部同じ距離なら問題なし！)
015840               COMPUTE Ｊはり往療回数Ｗ = Ｊはり往療回数Ｗ + 1
015850               COMPUTE Ｊはり往療加算回数Ｗ = Ｊはり往療加算回数Ｗ + 1
015860               COMPUTE Ｊはり往療加算単価Ｗ = はり往療料Ｗ -  Ｈ往療基本料金Ｗ
015870               COMPUTE Ｊはり往療加算料Ｗ = Ｊはり往療加算料Ｗ + (はり往療料Ｗ -  Ｈ往療基本料金Ｗ)
015880*           /-- レセプトＦ用：加算の往療距離が違う日があるか判定 --/
015890               IF Ｊ退避往療距離Ｗ = ZERO
015900                  MOVE Ｈ日－往療距離  TO Ｊ退避往療距離Ｗ
015910               ELSE
015920                  IF Ｈ日－往療距離 NOT = Ｊ退避往療距離Ｗ
015930                     MOVE 1 TO Ｊ往療加算複数判定Ｗ
015940                  END-IF
015950               END-IF
015960**
015970           END-IF
015980*------------------------------------------------------------------------------------*
      */按分は使わない/20240919↓↓↓
015990**      ※ 上記後、按分の場合は、往療料を上書き
016000*           IF Ｈ日－往療按分区分 = 1
016010*              MOVE Ｈ日－往療按分金額  TO はり往療料Ｗ
016020*           END-IF
      */按分は使わない/20240919↑↑↑
016030
016041*------------------------------------------------------------------------------------*
016042*      ※ 上記後、レセプトＦ用に往療料を累計
016050           COMPUTE Ｊはり累計往療料Ｗ = Ｊはり累計往療料Ｗ + はり往療料Ｗ
016060*------------------------------------------------------------------------------------*
016070*
016080        END-IF
016101**
016102*       ※ 往療料なしの場合はゼロ円上書き
016110        IF Ｈ往実－按分なし特定区分 = 2
016120           ADD 1                    TO 往療０円カウンタ
016130           ADD 1                    TO Ｊはり往療回数Ｗ
016140           MOVE ZERO                TO はり往療料Ｗ
016150        END-IF
016172*
016173*       施術報告書交付料
016174        IF (Ｈ日－施術和暦年月日     >= "4301001") AND
016175           (Ｈ日－施術報告書交付区分  = 1) AND
016176           (Ｈ日－施術和暦年月       >= 交付可能和暦年月Ｗ)
016177            ADD 1                      TO Ｊはり報告書交付回数Ｗ
016178            ADD 施術報告書交付料単価Ｗ TO Ｊはり報告書交付料Ｗ
016179            MOVE 施術報告書交付料単価Ｗ  TO 計算用報告書交付料Ｗ
016182        ELSE
016183            MOVE ZERO                    TO 計算用報告書交付料Ｗ
016184        END-IF
016185*
016186* 合計
016188        COMPUTE Ｊはり累計額Ｗ = Ｊはり累計額Ｗ + はり合計額Ｗ
016189*
016190     END-IF.
016200*--------------------------------------------------------------*
016210*
016220*================================================================*
016230 マッサージ日計料金計算 SECTION.
016240*--------------------------------------------*
016250*（施術内容区分）
016260*  1:マッサージ
016270*  2:マッサージ＋温罨法
016280*  3:マッサージ＋温罨法＋電気
016290*  4:変形徒手矯正術
016300*  5:変形徒手矯正術＋温罨法
016310*  6:変形徒手矯正術＋温罨法＋電気
016320*  7:マッサージ＋変形徒手矯正術
016330*  8:マッサージ＋変形徒手矯正術＋温罨法
016340*  9:マッサージ＋変形徒手矯正術＋温罨法＋電気
016350*
016360*--------------------------------------------*
016370*
      */突発往療のチェック↓↓↓/20240903
001107     IF Ｈ日－突発往療区分 = 1 
               MOVE Ｈ日－施術区分       TO 連入２０１－施術区分       
               MOVE Ｈ日－施術和暦年月日 TO 連入２０１－施術和暦年月日 
               MOVE Ｈ日－患者コード     TO 連入２０１－患者コード     
               MOVE SPACE                TO 連入２０１－エラーフラグ
               MOVE ZERO TO 突発不可Ｆ
022360         CALL   "TOPPATU"
022370         CANCEL "TOPPATU"
               IF 連入２０１－エラーフラグ NOT = SPACE
                   MOVE 1 TO 突発不可Ｆ
               END-IF
               OPEN I-O 更新用Ｈ往療実績Ｆ
004850             MOVE NC"Ｈ往実" TO ファイル名
004860             PERFORM オープンチェック
               MOVE Ｈ日－施術区分       TO 更Ｈ往実－施術区分
002380         MOVE Ｈ日－施術和暦年月日 TO 更Ｈ往実－施術和暦年月日
002390         MOVE Ｈ日－患者コード     TO 更Ｈ往実－患者コード
               READ 更新用Ｈ往療実績Ｆ
               NOT INVALID KEY
                   IF 連入２０１－エラーフラグ NOT = SPACE
                       MOVE ZERO             TO 更Ｈ往実－往療金額
                   ELSE
                       MOVE Ｈ往療基本料金Ｗ TO 更Ｈ往実－往療金額
                   END-IF
HILO***       IF Ｈ連計算４－同一建物患者数取得Ｆ = ZERO DISPLAY "更Ｈ往実－往療金額 " 更Ｈ往実－往療金額 END-IF
                   REWRITE 更Ｈ往実－レコード
                   IF 状態キー NOT = "00"
                       MOVE NC"更Ｈ往実" TO ファイル名
                       PERFORM エラー表示
                   END-IF
               END-READ
               CLOSE 更新用Ｈ往療実績Ｆ
           END-IF.
HILO***       DISPLAY "K5-A 連入２０１－エラーフラグ " 連入２０１－エラーフラグ "'" 突発不可Ｆ "'"
HILO  */突発往療のチェック↑↑↑/20240903
      **/突発往療のチェック↓↓↓/20240903
001107*     IF Ｈ日－突発往療区分 = 1 
      *     MOVE Ｈ日－施術区分       TO 連入２０１－施術区分       
      *     MOVE Ｈ日－施術和暦年月日 TO 連入２０１－施術和暦年月日 
      *     MOVE Ｈ日－患者コード     TO 連入２０１－患者コード     
      *     MOVE SPACE                TO 連入２０１－エラーフラグ
      *     MOVE ZERO TO 突発不可Ｆ
022360**     CALL   "TOPPATU"
022370**     CANCEL "TOPPATU"
      *     END-IF
HILO****       DISPLAY "K5-A 連入２０１－エラーフラグ " 連入２０１－エラーフラグ
      *     IF 連入２０１－エラーフラグ NOT = SPACE
      *         MOVE 1 TO 突発不可Ｆ
      *         OPEN I-O 更新用Ｈ往療実績Ｆ
004850*         MOVE NC"施設" TO ファイル名
004860*         PERFORM オープンチェック
      *         MOVE Ｈ日－施術区分       TO 更Ｈ往実－施術区分
002380*         MOVE Ｈ日－施術和暦年月日 TO 更Ｈ往実－施術和暦年月日
002390*         MOVE Ｈ日－患者コード     TO 更Ｈ往実－患者コード
      *         READ 更新用Ｈ往療実績Ｆ
      *         NOT INVALID KEY
      *             MOVE ZERO TO 更Ｈ往実－往療金額
      *             REWRITE 更Ｈ往実－レコード
      *             IF 状態キー NOT = "00"
      *                 MOVE NC"更Ｈ往実" TO ファイル名
      *                 PERFORM エラー表示
      *             END-IF
      *         END-READ
      *         CLOSE 更新用Ｈ往療実績Ｆ
      *     END-IF.
HILO  */突発往療のチェック↑↑↑/20240903
HILO  *     DISPLAY "610-5 特殊施術フラグＷ=" 特殊施術フラグＷ
016380     IF 特殊施術フラグＷ NOT = 1
016390*      ＜ 特殊施術入力無し、もしくは古いデータ ＞
016400*------ ①マッサージ -------------------------------------------*
016410        IF Ｈ日－施術内容区分 = 1 OR 2 OR 3 OR 7 OR 8 OR 9
016420            COMPUTE マッサージ回数Ｗ = マッサージ回数Ｗ + 1
016430            COMPUTE マッサージ料Ｗ = Ｍ１局所Ｗ * マッサージ局所数Ｗ
016440        END-IF
HILO  *        DISPLAY "610-3-1 マッサージ局所数Ｗ" マッサージ局所数Ｗ " マッサージ料Ｗ" マッサージ料Ｗ
016450*
016460*------ ②変形徒手矯正術-----------------------------------------*
016470        IF Ｈ日－施術内容区分 = 4 OR 5 OR 6 OR 7 OR 8 OR 9
016480            COMPUTE マッサージ変形徒手矯正術回数Ｗ = マッサージ変形徒手矯正術回数Ｗ + 1
016490            COMPUTE マッサージ変形徒手矯正術Ｗ = Ｍ変形徒手矯正術Ｗ * 変形徒手矯正術数Ｗ
016500        END-IF
016510     ELSE
016520*------- 特殊施術料計算----------*
016530*       施術箇所ごとに計算
016540        MOVE ZERO         TO マッサージ局所数日計Ｗ
016550        MOVE ZERO         TO 変形徒手矯正術数日計Ｗ
016560*
016570        IF (Ｈ日－マッサージ体幹   = 1) OR
016580           (Ｈ日－マッサージ右上肢 = 1) OR
016590           (Ｈ日－マッサージ左上肢 = 1) OR
016600           (Ｈ日－マッサージ右下肢 = 1) OR
016610           (Ｈ日－マッサージ左下肢 = 1)
016620           ADD 1          TO マッサージ回数Ｗ
016630        END-IF
016640
016650        IF Ｈ日－マッサージ体幹 = 1
016660           ADD 1          TO マッサージ体幹回数Ｗ マッサージ局所数日計Ｗ
016670           ADD Ｍ１局所Ｗ TO マッサージ体幹金額Ｗ マッサージ料Ｗ
016680        END-IF
016690        IF Ｈ日－マッサージ右上肢 = 1
016700           ADD 1          TO マッサージ右上肢回数Ｗ マッサージ局所数日計Ｗ
016710           ADD Ｍ１局所Ｗ TO マッサージ右上肢金額Ｗ マッサージ料Ｗ
016720        END-IF
016730        IF Ｈ日－マッサージ左上肢 = 1
016740           ADD 1          TO マッサージ左上肢回数Ｗ マッサージ局所数日計Ｗ
016750           ADD Ｍ１局所Ｗ TO マッサージ左上肢金額Ｗ マッサージ料Ｗ
016760        END-IF
016770        IF Ｈ日－マッサージ右下肢 = 1
016780           ADD 1          TO マッサージ右下肢回数Ｗ マッサージ局所数日計Ｗ
016790           ADD Ｍ１局所Ｗ TO マッサージ右下肢金額Ｗ マッサージ料Ｗ
016800        END-IF
016810        IF Ｈ日－マッサージ左下肢 = 1
016820           ADD 1          TO マッサージ左下肢回数Ｗ マッサージ局所数日計Ｗ
016830           ADD Ｍ１局所Ｗ TO マッサージ左下肢金額Ｗ マッサージ料Ｗ
016840        END-IF
016850*
HILO  *        DISPLAY "610-3-2 マッサージ局所数日計Ｗ " マッサージ局所数日計Ｗ
016860        IF (Ｈ日－変形徒手矯正術右上肢 = 1) OR
016870           (Ｈ日－変形徒手矯正術左上肢 = 1) OR
016880           (Ｈ日－変形徒手矯正術右下肢 = 1) OR
016890           (Ｈ日－変形徒手矯正術左下肢 = 1)
016900           ADD 1          TO マッサージ変形徒手矯正術回数Ｗ
016910        END-IF
016920*
016930        IF Ｈ日－変形徒手矯正術右上肢 = 1
016940           ADD 1                  TO 変形徒手矯正術右上肢回数Ｗ 変形徒手矯正術数日計Ｗ
016950           ADD Ｍ変形徒手矯正術Ｗ TO 変形徒手矯正術右上肢金額Ｗ マッサージ変形徒手矯正術Ｗ
016960        END-IF
016970        IF Ｈ日－変形徒手矯正術左上肢 = 1
016980           ADD 1                  TO 変形徒手矯正術左上肢回数Ｗ 変形徒手矯正術数日計Ｗ
016990           ADD Ｍ変形徒手矯正術Ｗ TO 変形徒手矯正術左上肢金額Ｗ マッサージ変形徒手矯正術Ｗ
017000        END-IF
017010        IF Ｈ日－変形徒手矯正術右下肢 = 1
017020           ADD 1                  TO 変形徒手矯正術右下肢回数Ｗ 変形徒手矯正術数日計Ｗ
017030           ADD Ｍ変形徒手矯正術Ｗ TO 変形徒手矯正術右下肢金額Ｗ マッサージ変形徒手矯正術Ｗ
017040        END-IF
017050        IF Ｈ日－変形徒手矯正術左下肢 = 1
017060           ADD 1                  TO 変形徒手矯正術左下肢回数Ｗ 変形徒手矯正術数日計Ｗ
017070           ADD Ｍ変形徒手矯正術Ｗ TO 変形徒手矯正術左下肢金額Ｗ マッサージ変形徒手矯正術Ｗ
017080        END-IF
017090*
017100*       施術箇所が同意書情報と異なれば特殊施術内容を表示させる
017110        IF (Ｈ日－施術内容区分                = 1 OR 2 OR 3 OR 7 OR 8 OR 9)
017120           IF (マッサージ体幹同意Ｗ       NOT = Ｈ日－マッサージ体幹) OR
017130              (マッサージ右上肢同意Ｗ     NOT = Ｈ日－マッサージ右上肢) OR
017140              (マッサージ左上肢同意Ｗ     NOT = Ｈ日－マッサージ左上肢) OR
017150              (マッサージ右下肢同意Ｗ     NOT = Ｈ日－マッサージ右下肢) OR
017160              (マッサージ左下肢同意Ｗ     NOT = Ｈ日－マッサージ左下肢)
017170              MOVE 1                  TO マッサージ特殊施術フラグＷ
017180              MOVE 1                  TO Ｊマッサージ特殊施術フラグＷ
017190           END-IF
017200        END-IF
017210        IF (Ｈ日－施術内容区分                 = 4 OR 5 OR 6 OR 7 OR 8 OR 9 )
017220           IF (変形徒手矯正術右上肢同意Ｗ  NOT = Ｈ日－変形徒手矯正術右上肢) OR
017230              (変形徒手矯正術左上肢同意Ｗ  NOT = Ｈ日－変形徒手矯正術左上肢) OR
017240              (変形徒手矯正術右下肢同意Ｗ  NOT = Ｈ日－変形徒手矯正術右下肢) OR
017250              (変形徒手矯正術左下肢同意Ｗ  NOT = Ｈ日－変形徒手矯正術左下肢)
017260               MOVE 1                 TO 変形徒手矯正術特殊施術フラグＷ
017270               MOVE 1                 TO Ｊ変形徒手矯正術特殊施術フラグＷ
017280           END-IF
017290        END-IF
017300*
017310     END-IF.
017320*
017330*    施術箇所数が複数パターンかチェック
017340     IF Ｈ日－施術内容区分 = 1 OR 2 OR 3 OR 7 OR 8 OR 9
017350        IF マッサージ局所数セットＷ NOT = ZERO
017360*          最初の1回と比較
017370           IF Ｈ日－特殊施術区分 = ZERO
017380              IF (マッサージ局所数ＰＷ NOT = マッサージ局所数Ｗ) AND
017390                 (マッサージ局所数Ｗ   NOT = ZERO)
017400                 MOVE 1                        TO マッサージ局所数複数Ｗ
017410              END-IF
017420           ELSE
017430              IF (マッサージ局所数ＰＷ   NOT = マッサージ局所数日計Ｗ) AND
017440                 (マッサージ局所数日計Ｗ NOT = ZERO)
017450                 MOVE 1                        TO マッサージ局所数複数Ｗ
017460              END-IF
017470           END-IF
017480        ELSE
017490*          パターン判定用に最初の1回を保存する。
017500           IF Ｈ日－特殊施術区分 = ZERO
017510              IF マッサージ局所数Ｗ NOT = ZERO
017520                 MOVE マッサージ局所数Ｗ       TO マッサージ局所数ＰＷ
017530                 MOVE 1                        TO マッサージ局所数セットＷ
017540              END-IF
017550           ELSE
017560              IF マッサージ局所数日計Ｗ NOT = ZERO
017570                 MOVE マッサージ局所数日計Ｗ   TO マッサージ局所数ＰＷ
017580                 MOVE 1                        TO マッサージ局所数セットＷ
017590              END-IF
017600           END-IF
017610        END-IF
017620     END-IF.
017630*
017640     IF Ｈ日－施術内容区分 = 4 OR 5 OR 6 OR 7 OR 8 OR 9
017650        IF 変形徒手矯正術数セットＷ NOT = ZERO
017660*          最初の1回と比較
017670           IF Ｈ日－特殊施術区分 = ZERO
017680              IF (変形徒手矯正術数ＰＷ NOT = 変形徒手矯正術数Ｗ) AND
017690                 (変形徒手矯正術数Ｗ   NOT = ZERO)
017700                 MOVE 1                        TO 変形徒手矯正術数複数Ｗ
017710              END-IF
017720           ELSE
017730              IF (変形徒手矯正術数ＰＷ   NOT = 変形徒手矯正術数日計Ｗ) AND
017740                 (変形徒手矯正術数日計Ｗ NOT = ZERO)
017750                 MOVE 1                        TO 変形徒手矯正術数複数Ｗ
017760              END-IF
017770           END-IF
017780        ELSE
017790*          パターン判定用に最初の1回を保存する。
017800           IF Ｈ日－特殊施術区分 = ZERO
017810              IF 変形徒手矯正術数Ｗ NOT = ZERO
017820                 MOVE 変形徒手矯正術数Ｗ       TO 変形徒手矯正術数ＰＷ
017830                 MOVE 1                        TO 変形徒手矯正術数セットＷ
017840              END-IF
017850           ELSE
017860              IF 変形徒手矯正術数日計Ｗ NOT = ZERO
017870                 MOVE 変形徒手矯正術数日計Ｗ   TO 変形徒手矯正術数ＰＷ
017880                 MOVE 1                        TO 変形徒手矯正術数セットＷ
017890              END-IF
017900           END-IF
017910        END-IF
017920     END-IF.
017930*
      */202406↓↓↓
      *x実際には Ｈ日－訪問施術料区分 と Ｈ日－突発往療区分 を使う
HILO  *     DISPLAY "k5-1 " Ｈ日－突発往療区分 " " 突発不可Ｆ 
      */Ｈ日－往療日区分 ゼロ：通所・１：訪問
      *     IF Ｈ日－往療日区分 = ZERO
*******20240903
           IF (Ｈ日－往療日区分 = ZERO) OR (Ｈ日－突発往療区分 = 1)
      */通所はそのまま？
HILO***           DISPLAY "610-4-1 通所"
HILO  *         DISPLAY "610-3-1 マッサージ局所数Ｗ" マッサージ局所数Ｗ " マッサージ料Ｗ" マッサージ料Ｗ
               CONTINUE
      *
               IF (マッサージ通所施術料単価１Ｗ = ZERO           ) OR
001410            (マッサージ通所施術料単価１Ｗ = マッサージ料Ｗ )
001410             MOVE マッサージ料Ｗ TO マッサージ通所施術料単価１Ｗ  
001420             COMPUTE マッサージ通所施術料回数１Ｗ   = マッサージ通所施術料回数１Ｗ + 1
001420             COMPUTE マッサージ通所施術料施術料１Ｗ = マッサージ通所施術料施術料１Ｗ + マッサージ料Ｗ
               ELSE
001410             MOVE マッサージ料Ｗ TO マッサージ通所施術料単価２Ｗ  
001420             COMPUTE マッサージ通所施術料回数２Ｗ   = マッサージ通所施術料回数２Ｗ   + 1
001420             COMPUTE マッサージ通所施術料施術料２Ｗ = マッサージ通所施術料施術料２Ｗ + マッサージ料Ｗ
               END-IF
           ELSE
HILO***           DISPLAY "610-4-2 訪問"
HILO  *     DISPLAY "610-13 マッサージ局所数日計Ｗ " マッサージ局所数日計Ｗ
016380         IF 特殊施術フラグＷ NOT = 1
016390*          ＜ 特殊施術入力無し、もしくは古いデータ ＞
                   MOVE マッサージ局所数Ｗ     TO 当日局所数Ｗ
016510         ELSE
016520*----------- 特殊施術料計算----------*
                   MOVE マッサージ局所数日計Ｗ TO 当日局所数Ｗ
               END-IF
               EVALUATE TRUE
               WHEN 同一建物患者数Ｗ = 1
      *            /１人
                   EVALUATE 当日局所数Ｗ
                   WHEN 1
000339                 MOVE Ｍ１局所訪問施術料１Ｗ TO マッサージ料Ｗ
                   WHEN 2
000339                 MOVE Ｍ２局所訪問施術料１Ｗ TO マッサージ料Ｗ
                   WHEN 3
000339                 MOVE Ｍ３局所訪問施術料１Ｗ TO マッサージ料Ｗ
                   WHEN 4
000339                 MOVE Ｍ４局所訪問施術料１Ｗ TO マッサージ料Ｗ
                   WHEN 5
000339                 MOVE Ｍ５局所訪問施術料１Ｗ TO マッサージ料Ｗ
                   END-EVALUATE
001410             IF (マッサージ訪問施術料１単価１Ｗ = ZERO          ) OR
001410                (マッサージ訪問施術料１単価１Ｗ = マッサージ料Ｗ)
001410                 MOVE マッサージ料Ｗ TO マッサージ訪問施術料１単価１Ｗ
001420                 COMPUTE マッサージ訪問施術料１回数１Ｗ   = マッサージ訪問施術料１回数１Ｗ   + 1
001420                 COMPUTE マッサージ訪問施術料１施術料１Ｗ = マッサージ訪問施術料１施術料１Ｗ + マッサージ料Ｗ
                   ELSE
001410                 MOVE マッサージ料Ｗ TO マッサージ訪問施術料１単価２Ｗ   
001420                 COMPUTE マッサージ訪問施術料１回数２Ｗ   = マッサージ訪問施術料１回数２Ｗ   + 1
001420                 COMPUTE マッサージ訪問施術料１施術料２Ｗ = マッサージ訪問施術料１施術料２Ｗ + マッサージ料Ｗ
                   END-IF
               WHEN 同一建物患者数Ｗ = 2
      *            /２人
                   EVALUATE 当日局所数Ｗ
                   WHEN 1
000339                 MOVE Ｍ１局所訪問施術料２Ｗ TO マッサージ料Ｗ
                   WHEN 2
000339                 MOVE Ｍ２局所訪問施術料２Ｗ TO マッサージ料Ｗ
                   WHEN 3
000339                 MOVE Ｍ３局所訪問施術料２Ｗ TO マッサージ料Ｗ
                   WHEN 4
000339                 MOVE Ｍ４局所訪問施術料２Ｗ TO マッサージ料Ｗ
                   WHEN 5
000339                 MOVE Ｍ５局所訪問施術料２Ｗ TO マッサージ料Ｗ
                   END-EVALUATE
001410             IF (マッサージ訪問施術料２単価１Ｗ = ZERO          ) OR
001410                (マッサージ訪問施術料２単価１Ｗ = マッサージ料Ｗ)
001410                 MOVE マッサージ料Ｗ TO マッサージ訪問施術料２単価１Ｗ    
001420                 COMPUTE マッサージ訪問施術料２回数１Ｗ   =  マッサージ訪問施術料２回数１Ｗ   + 1
001420                 COMPUTE マッサージ訪問施術料２施術料１Ｗ =  マッサージ訪問施術料２施術料１Ｗ + マッサージ料Ｗ
                   ELSE
001410                 MOVE マッサージ料Ｗ TO マッサージ訪問施術料２単価２Ｗ    
001420                 COMPUTE マッサージ訪問施術料２回数２Ｗ   = マッサージ訪問施術料２回数２Ｗ    + 1
001420                 COMPUTE マッサージ訪問施術料２施術料２Ｗ = マッサージ訪問施術料２施術料２Ｗ  + マッサージ料Ｗ
                   END-IF
               WHEN 同一建物患者数Ｗ <= 9
      *            /３人～９人
                   EVALUATE 当日局所数Ｗ
                   WHEN 1
000339                 MOVE Ｍ１局所訪問施術料３Ｗ TO マッサージ料Ｗ
                   WHEN 2
000339                 MOVE Ｍ２局所訪問施術料３Ｗ TO マッサージ料Ｗ
                   WHEN 3
000339                 MOVE Ｍ３局所訪問施術料３Ｗ TO マッサージ料Ｗ
                   WHEN 4
000339                 MOVE Ｍ４局所訪問施術料３Ｗ TO マッサージ料Ｗ
                   WHEN 5
000339                 MOVE Ｍ５局所訪問施術料３Ｗ TO マッサージ料Ｗ
                   END-EVALUATE
001410             IF (マッサージ訪問施術料３単価１Ｗ = ZERO          ) OR
001410                (マッサージ訪問施術料３単価１Ｗ = マッサージ料Ｗ)
001410                 MOVE マッサージ料Ｗ TO マッサージ訪問施術料３単価１Ｗ    
001420                 COMPUTE マッサージ訪問施術料３回数１Ｗ   = マッサージ訪問施術料３回数１Ｗ    + 1
001420                 COMPUTE マッサージ訪問施術料３施術料１Ｗ = マッサージ訪問施術料３施術料１Ｗ  + マッサージ料Ｗ
                   ELSE
001410                 MOVE マッサージ料Ｗ TO マッサージ訪問施術料３単価２Ｗ    
001420                 COMPUTE マッサージ訪問施術料３回数２Ｗ   = マッサージ訪問施術料３回数２Ｗ    + 1
001420                 COMPUTE マッサージ訪問施術料３施術料２Ｗ = マッサージ訪問施術料３施術料２Ｗ  + マッサージ料Ｗ
                   END-IF
               WHEN 同一建物患者数Ｗ >= 10
      *            /１０人以上
                   EVALUATE 当日局所数Ｗ
                   WHEN 1
000339                 MOVE Ｍ１局所訪問施術料４Ｗ TO マッサージ料Ｗ
                   WHEN 2
000339                 MOVE Ｍ２局所訪問施術料４Ｗ TO マッサージ料Ｗ
                   WHEN 3
000339                 MOVE Ｍ３局所訪問施術料４Ｗ TO マッサージ料Ｗ
                   WHEN 4
000339                 MOVE Ｍ４局所訪問施術料４Ｗ TO マッサージ料Ｗ
                   WHEN 5
000339                 MOVE Ｍ５局所訪問施術料４Ｗ TO マッサージ料Ｗ
                   END-EVALUATE
001410             IF (マッサージ訪問施術料４単価１Ｗ = ZERO          ) OR
001410                (マッサージ訪問施術料４単価１Ｗ = マッサージ料Ｗ)
001410                 MOVE マッサージ料Ｗ TO マッサージ訪問施術料４単価１Ｗ
001420                 COMPUTE マッサージ訪問施術料４回数１Ｗ   = マッサージ訪問施術料４回数１Ｗ    + 1
001420                 COMPUTE マッサージ訪問施術料４施術料１Ｗ = マッサージ訪問施術料４施術料１Ｗ  + マッサージ料Ｗ
                   ELSE
001410                 MOVE マッサージ料Ｗ TO マッサージ訪問施術料４単価２Ｗ    
001420                 COMPUTE マッサージ訪問施術料４回数２Ｗ   = マッサージ訪問施術料４回数２Ｗ    + 1
001420                 COMPUTE マッサージ訪問施術料４施術料２Ｗ = マッサージ訪問施術料４施術料２Ｗ  + マッサージ料Ｗ
                   END-IF
               END-EVALUATE
           END-IF
HILO  *        DISPLAY "610-3-2 マッサージ料Ｗ  =" マッサージ料Ｗ " 局所数Ｗ=" マッサージ局所数Ｗ 
HILO  *        DISPLAY "610-3-3 変形徒手矯正術Ｗ=" マッサージ変形徒手矯正術Ｗ 
      *        " 単価=" Ｍ変形徒手矯正術Ｗ  " 回数=" 変形徒手矯正術数Ｗ
017940*--- ③温罨法料(1回のみ)-------------------------------------------*
017950     IF Ｈ日－施術内容区分 = 2 OR 5 OR 8
017960        COMPUTE マッサージ温罨法回数Ｗ = マッサージ温罨法回数Ｗ + 1
017970        MOVE Ｍ温罨法料Ｗ TO マッサージ温罨法料Ｗ
017980     END-IF.
017990*
018000*--- ④温罨法+電療料(1回のみ)--------------------------------------*
018010     IF Ｈ日－施術内容区分 = 3 OR 6 OR 9
018020        COMPUTE マッサージ温罨法電気回数Ｗ = マッサージ温罨法電気回数Ｗ + 1
018030        COMPUTE マッサージ温罨法電療料Ｗ = Ｍ温罨法料Ｗ + Ｍ電療料Ｗ
018040     END-IF.
018050*
018060*---- ⑤往療料-------------------------------------------------*
HILO  */↓↓↓突発往料
HILO  */Ｈ日－突発往療区分
      *     IF Ｈ日－突発往療区分 = 1
           IF (Ｈ日－突発往療区分 = 1) AND (突発不可Ｆ = ZERO)
      *     IF Ｈ日－往療日区分 = ZERO
018070* (日ごと実施)
018080*    / 往療日区分 1:往療 特定≠２を条件追加
018090     IF (Ｈ日－往療日区分 = 1) AND (Ｈ日－往療距離 NOT = ZERO) AND (Ｈ往実－按分なし特定区分 NOT = 2)
018100        IF Ｈ日－往療距離 <= 往療基本距離
018110            MOVE Ｍ往療基本料金Ｗ  TO マッサージ往療料Ｗ
018120*           (レセプトＦ用回数累計)
018130            COMPUTE マッサージ往療回数Ｗ = マッサージ往療回数Ｗ + 1
018140        ELSE
018150*           整数にするため、１０を掛ける
018160            COMPUTE 往療被除数Ｗ = Ｈ日－往療距離 * 10
018170            COMPUTE 往療除数Ｗ   = 往療基本距離 * 10
018180            DIVIDE 往療除数Ｗ INTO 往療被除数Ｗ GIVING 往療商Ｗ
018190                              REMAINDER 往療剰余Ｗ
018200            COMPUTE マッサージ往療料Ｗ = Ｍ往療基本料金Ｗ +
018210                             ( Ｍ往療追加料金Ｗ * ( 往療商Ｗ - 1 ) )
018220            IF 往療剰余Ｗ > ZERO
018230                COMPUTE マッサージ往療料Ｗ = マッサージ往療料Ｗ + Ｍ往療追加料金Ｗ
018240            END-IF
018250**------------------------------------------------------------------------------**
018260*           往療料上限料金算定　20160901
018270            INITIALIZE Ｈ連最往－キー
018280            MOVE 2                  TO Ｈ連最往－施術区分
018290            MOVE Ｈ日－施術和暦年月 TO Ｈ連最往－施術和暦年月
018300            MOVE Ｈ日－往療距離     TO Ｈ連最往－往療距離
018310            CALL "MAXOURYO"
018320            IF Ｈ連最往－該当区分 = 1
018330                COMPUTE マッサージ往療料Ｗ = Ｍ往療基本料金Ｗ + Ｈ連最往－往療加算金額
018340            END-IF
018350            CANCEL "MAXOURYO"
018360**------------------------------------------------------------------------------**
018370*
018380*           (レセプトＦ用回数・加算料・往療加算単価、単価は１つのみ→最後の日のデータだが、全部同じ距離なら問題なし！)
018390            COMPUTE マッサージ往療回数Ｗ = マッサージ往療回数Ｗ + 1
018400            COMPUTE マッサージ往療加算回数Ｗ = マッサージ往療加算回数Ｗ + 1
018410            COMPUTE マッサージ往療加算単価Ｗ = マッサージ往療料Ｗ -  Ｍ往療基本料金Ｗ
018420            COMPUTE マッサージ往療加算料Ｗ = マッサージ往療加算料Ｗ + (マッサージ往療料Ｗ -  Ｍ往療基本料金Ｗ)
018430**         /-- レセプトＦ用：加算の往療距離が違う日があるか判定 --/
018440            IF 退避往療距離Ｗ = ZERO
018450               MOVE Ｈ日－往療距離  TO 退避往療距離Ｗ
018460            ELSE
018470               IF Ｈ日－往療距離 NOT = 退避往療距離Ｗ
018480                  MOVE 1 TO 往療加算複数判定Ｗ
018490               END-IF
018500            END-IF
018510**
018520        END-IF
018530*------------------------------------------------------------------------------------*
018540*    ※ 上記後、按分の場合は、往療料を上書き
018550        IF Ｈ日－往療按分区分 = 1
018560           MOVE Ｈ日－往療按分金額  TO マッサージ往療料Ｗ
018570        END-IF
018580*------------------------------------------------------------------------------------*
018590*    ※ 上記後、レセプトＦ用に往療料を累計
018600        COMPUTE マッサージ累計往療料Ｗ = マッサージ累計往療料Ｗ + マッサージ往療料Ｗ
018610*------------------------------------------------------------------------------------*
018620*
018630     END-IF


018640*
018650*    ※ 往療料なしの場合はゼロ円上書き
018660     IF Ｈ往実－按分なし特定区分 = 2
018670        ADD 1                    TO 往療０円カウンタ
018680        ADD 1                    TO マッサージ往療回数Ｗ
018690        MOVE ZERO                TO マッサージ往療料Ｗ
018700     END-IF

018630     END-IF.
HILO  */↑↑↑突発往料

HILO  *     DISPLAY "610-3-4 マッサージ往療料Ｗ" マッサージ往療料Ｗ

018710*
018725*
018726*    施術報告書交付料
018729     IF (Ｈ日－施術和暦年月日     >= "4301001") AND
018730        (Ｈ日－施術報告書交付区分  = 1) AND
018733        (Ｈ日－施術和暦年月       >= 交付可能和暦年月Ｗ)
018734         ADD 1                      TO マッサージ報告書交付回数Ｗ
018735         ADD 施術報告書交付料単価Ｗ TO マッサージ報告書交付料Ｗ
018736         MOVE 施術報告書交付料単価Ｗ  TO 計算用報告書交付料Ｗ
018737     ELSE
018738         MOVE ZERO                    TO 計算用報告書交付料Ｗ
018739     END-IF.
018743*
018744*--------------------------------------------------------------*
018745*
018746* 合計
018750     COMPUTE マッサージ合計額Ｗ = マッサージ料Ｗ + マッサージ変形徒手矯正術Ｗ +
018760                                  マッサージ温罨法料Ｗ + マッサージ温罨法電療料Ｗ + マッサージ往療料Ｗ +
018761                                  計算用報告書交付料Ｗ
      */20240627
                                      + 特別地域加算料Ｗ.

HILO***     DISPLAY "61-9 " 受－患者氏名(1:10) ": 計"       マッサージ合計額Ｗ
HILO***            " ﾏｯｻｰｼﾞ" マッサージ料Ｗ
HILO***            " 変形"   マッサージ変形徒手矯正術Ｗ
HILO***            " 温"     マッサージ温罨法料Ｗ
HILO***            " 温電"   マッサージ温罨法電療料Ｗ
HILO***            " 往療"   マッサージ往療料Ｗ
HILO***            " 報"     計算用報告書交付料Ｗ
HILO***            " 特"     特別地域加算料Ｗ



018770* 連結の費用額セット
018780     MOVE マッサージ合計額Ｗ TO Ｈ連計算－費用額.
018790*
018800* レセ用累計
018810     COMPUTE マッサージ累計額Ｗ = マッサージ累計額Ｗ + マッサージ合計額Ｗ.
018820*
018830*--------------------------------------------------------------*
018840*
018850     PERFORM 日計負担額取得.
018860*
018870*--------------------------------------------------------------*
018880*
018890*--------------------------------------------------------------*
018900* 月途中助成のレセＦ用の集計
018910*
018920     IF ( 助成月途中開始日Ｗ NOT = ZERO ) AND ( 助成月途中開始日Ｗ <= Ｈ日－施術日)
018930*
018940*       温罨法
018950        IF Ｈ日－施術内容区分 = 2 OR 5 OR 8
018960           COMPUTE Ｊマッサージ温罨法回数Ｗ = Ｊマッサージ温罨法回数Ｗ + 1
018970            MOVE Ｍ温罨法料Ｗ TO Ｊマッサージ温罨法料Ｗ
018980        END-IF
018990*
019000*       電気
019010        IF Ｈ日－施術内容区分 = 3 OR 6 OR 9
019020           COMPUTE Ｊマッサージ温罨法電気回数Ｗ = Ｊマッサージ温罨法電気回数Ｗ + 1
019030           COMPUTE Ｊマッサージ温罨法電療料Ｗ = Ｍ温罨法料Ｗ + Ｍ電療料Ｗ
019040        END-IF
019050*
019060*       マッサージ・変形徒手は特殊術の有無でことなる
019070        IF Ｊ特殊施術フラグＷ NOT = 1
019080           IF Ｈ日－施術内容区分 = 1 OR 2 OR 3 OR 7 OR 8 OR 9
019090               COMPUTE Ｊマッサージ回数Ｗ = Ｊマッサージ回数Ｗ + 1
019100               COMPUTE Ｊマッサージ料Ｗ = Ｍ１局所Ｗ * Ｊマッサージ局所数Ｗ
019110           END-IF
019120*
019130           IF Ｈ日－施術内容区分 = 4 OR 5 OR 6 OR 7 OR 8 OR 9
019140               COMPUTE Ｊマッサージ変形徒手矯正術回数Ｗ = Ｊマッサージ変形徒手矯正術回数Ｗ + 1
019150               COMPUTE Ｊマッサージ変形徒手矯正術Ｗ =  Ｍ変形徒手矯正術Ｗ * Ｊ変形徒手矯正術数Ｗ
019160           END-IF
019170        ELSE
019180*-------   特殊施術料計算----------*
019190*          施術箇所ごとに計算（助成月途中変更がある為、再度集計し直す必要がある）
019200           MOVE ZERO         TO Ｊマッサージ局所数日計Ｗ
019210           MOVE ZERO         TO Ｊ変形徒手矯正術数日計Ｗ
019220*
019230           IF (Ｈ日－マッサージ体幹   = 1) OR
019240              (Ｈ日－マッサージ右上肢 = 1) OR
019250              (Ｈ日－マッサージ左上肢 = 1) OR
019260              (Ｈ日－マッサージ右下肢 = 1) OR
019270              (Ｈ日－マッサージ左下肢 = 1)
019280              ADD 1          TO Ｊマッサージ回数Ｗ
019290           END-IF
019300
019310           IF Ｈ日－マッサージ体幹 = 1
019320              ADD 1          TO Ｊマッサージ体幹回数Ｗ Ｊマッサージ局所数日計Ｗ
019330              ADD Ｍ１局所Ｗ TO Ｊマッサージ体幹金額Ｗ Ｊマッサージ料Ｗ
019340           END-IF
019350           IF Ｈ日－マッサージ右上肢 = 1
019360              ADD 1          TO Ｊマッサージ右上肢回数Ｗ Ｊマッサージ局所数日計Ｗ
019370              ADD Ｍ１局所Ｗ TO Ｊマッサージ右上肢金額Ｗ Ｊマッサージ料Ｗ
019380           END-IF
019390           IF Ｈ日－マッサージ左上肢 = 1
019400              ADD 1          TO Ｊマッサージ左上肢回数Ｗ Ｊマッサージ局所数日計Ｗ
019410              ADD Ｍ１局所Ｗ TO Ｊマッサージ左上肢金額Ｗ Ｊマッサージ料Ｗ
019420           END-IF
019430           IF Ｈ日－マッサージ右下肢 = 1
019440              ADD 1          TO Ｊマッサージ右下肢回数Ｗ Ｊマッサージ局所数日計Ｗ
019450              ADD Ｍ１局所Ｗ TO Ｊマッサージ右下肢金額Ｗ Ｊマッサージ料Ｗ
019460           END-IF
019470           IF Ｈ日－マッサージ左下肢 = 1
019480              ADD 1          TO Ｊマッサージ左下肢回数Ｗ Ｊマッサージ局所数日計Ｗ
019490              ADD Ｍ１局所Ｗ TO Ｊマッサージ左下肢金額Ｗ Ｊマッサージ料Ｗ
019500           END-IF
019510*
019520           IF (Ｈ日－変形徒手矯正術右上肢 = 1) OR
019530              (Ｈ日－変形徒手矯正術左上肢 = 1) OR
019540              (Ｈ日－変形徒手矯正術右下肢 = 1) OR
019550              (Ｈ日－変形徒手矯正術左下肢 = 1)
019560              ADD 1          TO Ｊマッサージ変形徒手矯正術回数Ｗ
019570           END-IF
019580*
019590           IF Ｈ日－変形徒手矯正術右上肢 = 1
019600              ADD 1                  TO Ｊ変形徒手矯正術右上肢回数Ｗ Ｊ変形徒手矯正術数日計Ｗ
019610              ADD Ｍ変形徒手矯正術Ｗ TO Ｊ変形徒手矯正術右上肢金額Ｗ Ｊマッサージ変形徒手矯正術Ｗ
019620           END-IF
019630           IF Ｈ日－変形徒手矯正術左上肢 = 1
019640              ADD 1                  TO Ｊ変形徒手矯正術左上肢回数Ｗ Ｊ変形徒手矯正術数日計Ｗ
019650              ADD Ｍ変形徒手矯正術Ｗ TO Ｊ変形徒手矯正術左上肢金額Ｗ Ｊマッサージ変形徒手矯正術Ｗ
019660           END-IF
019670           IF Ｈ日－変形徒手矯正術右下肢 = 1
019680              ADD 1                  TO Ｊ変形徒手矯正術右下肢回数Ｗ Ｊ変形徒手矯正術数日計Ｗ
019690              ADD Ｍ変形徒手矯正術Ｗ TO Ｊ変形徒手矯正術右下肢金額Ｗ Ｊマッサージ変形徒手矯正術Ｗ
019700           END-IF
019710           IF Ｈ日－変形徒手矯正術左下肢 = 1
019720              ADD 1                  TO Ｊ変形徒手矯正術左下肢回数Ｗ Ｊ変形徒手矯正術数日計Ｗ
019730              ADD Ｍ変形徒手矯正術Ｗ TO Ｊ変形徒手矯正術左下肢金額Ｗ Ｊマッサージ変形徒手矯正術Ｗ
019740           END-IF
019750*
019760*          施術箇所数が複数パターンかチェック
019770*          本体は複数パターンでも、助成月途中変更で助成レセだけシングルパターンの可能性がある
019780           IF Ｊマッサージ局所数セットＷ NOT = ZERO
019790*             最初の1回と比較
019800              IF Ｈ日－特殊施術区分 = ZERO
019810                 IF (Ｊマッサージ局所数ＰＷ NOT = Ｊマッサージ局所数Ｗ) AND
019820                    (Ｊマッサージ局所数Ｗ   NOT = ZERO)
019830                    MOVE 1                        TO Ｊマッサージ局所数複数Ｗ
019840                 END-IF
019850              ELSE
019860                 IF (Ｊマッサージ局所数ＰＷ   NOT = Ｊマッサージ局所数日計Ｗ) AND
019870                    (Ｊマッサージ局所数日計Ｗ NOT = ZERO)
019880                    MOVE 1                        TO Ｊマッサージ局所数複数Ｗ
019890                 END-IF
019900              END-IF
019910           ELSE
019920*          パターン判定用に最初の1回を保存する。
019930              IF Ｈ日－特殊施術区分 = ZERO
019940                 IF Ｊマッサージ局所数Ｗ NOT = ZERO
019950                    MOVE Ｊマッサージ局所数Ｗ     TO Ｊマッサージ局所数ＰＷ
019960                    MOVE 1                        TO Ｊマッサージ局所数セットＷ
019970                 END-IF
019980              ELSE
019990                 IF Ｊマッサージ局所数日計Ｗ NOT = ZERO
020000                    MOVE Ｊマッサージ局所数日計Ｗ TO Ｊマッサージ局所数ＰＷ
020010                    MOVE 1                        TO Ｊマッサージ局所数セットＷ
020020                 END-IF
020030              END-IF
020040           END-IF
020050*
020060           IF Ｊ変形徒手矯正術数セットＷ NOT = ZERO
020070*             最初の1回と比較
020080              IF Ｈ日－特殊施術区分 = ZERO
020090                 IF (Ｊ変形徒手矯正術数ＰＷ NOT = Ｊ変形徒手矯正術数Ｗ) AND
020100                    (Ｊ変形徒手矯正術数Ｗ   NOT = ZERO)
020110                    MOVE 1                        TO Ｊ変形徒手矯正術数複数Ｗ
020120                 END-IF
020130              ELSE
020140                 IF (Ｊ変形徒手矯正術数ＰＷ   NOT = Ｊ変形徒手矯正術数日計Ｗ) AND
020150                    (Ｊ変形徒手矯正術数日計Ｗ NOT = ZERO)
020160                    MOVE 1                        TO Ｊ変形徒手矯正術数複数Ｗ
020170                 END-IF
020180              END-IF
020190           ELSE
020200*             パターン判定用に最初の1回を保存する。
020210              IF Ｈ日－特殊施術区分 = ZERO
020220                 IF Ｊ変形徒手矯正術数Ｗ NOT = ZERO
020230                    MOVE Ｊ変形徒手矯正術数Ｗ     TO Ｊ変形徒手矯正術数ＰＷ
020240                    MOVE 1                        TO Ｊ変形徒手矯正術数セットＷ
020250                 END-IF
020260              ELSE
020270                 IF Ｊ変形徒手矯正術数日計Ｗ NOT = ZERO
020280                    MOVE Ｊ変形徒手矯正術数日計Ｗ TO Ｊ変形徒手矯正術数ＰＷ
020290                    MOVE 1                        TO Ｊ変形徒手矯正術数セットＷ
020300                 END-IF
020310              END-IF
020320           END-IF
020330        END-IF
020340*
020350***
020360***     20151008 往療０円でない条件を追加
020370        IF (Ｈ日－往療日区分 = 1) AND (Ｈ日－往療距離 NOT = ZERO) AND (Ｈ往実－按分なし特定区分 NOT = 2)
020380
020390           IF Ｈ日－往療距離 <= 往療基本距離
020400               MOVE Ｍ往療基本料金Ｗ  TO Ｊマッサージ往療料Ｗ
020410*              (レセプトＦ用回数累計)
020420               COMPUTE Ｊマッサージ往療回数Ｗ = Ｊマッサージ往療回数Ｗ + 1
020430           ELSE
020440*              整数にするため、１０を掛ける
020450               COMPUTE 往療被除数Ｗ = Ｈ日－往療距離 * 10
020460               COMPUTE 往療除数Ｗ   = 往療基本距離 * 10
020470               DIVIDE 往療除数Ｗ INTO 往療被除数Ｗ GIVING 往療商Ｗ
020480                                 REMAINDER 往療剰余Ｗ
020490               COMPUTE Ｊマッサージ往療料Ｗ = Ｍ往療基本料金Ｗ +
020500                                ( Ｍ往療追加料金Ｗ * ( 往療商Ｗ - 1 ) )
020510               IF 往療剰余Ｗ > ZERO
020520                   COMPUTE Ｊマッサージ往療料Ｗ = Ｊマッサージ往療料Ｗ + Ｍ往療追加料金Ｗ
020530               END-IF
020540**------------------------------------------------------------------------------**
020550*              往療料上限料金算定　20160901
020560               INITIALIZE Ｈ連最往－キー
020570               MOVE 2                  TO Ｈ連最往－施術区分
020580               MOVE Ｈ日－施術和暦年月 TO Ｈ連最往－施術和暦年月
020590               MOVE Ｈ日－往療距離     TO Ｈ連最往－往療距離
020600               CALL "MAXOURYO"
020610               IF Ｈ連最往－該当区分 = 1
020620                   COMPUTE Ｊマッサージ往療料Ｗ = Ｍ往療基本料金Ｗ + Ｈ連最往－往療加算金額
020630               END-IF
020640               CANCEL "MAXOURYO"
020650**------------------------------------------------------------------------------**
020660*
020670*             (レセプトＦ用回数・加算料・往療加算単価、単価は１つのみ→最後の日のデータだが、全部同じ距離なら問題なし！)
020680               COMPUTE Ｊマッサージ往療回数Ｗ = Ｊマッサージ往療回数Ｗ + 1
020690               COMPUTE Ｊマッサージ往療加算回数Ｗ = Ｊマッサージ往療加算回数Ｗ + 1
020700               COMPUTE Ｊマッサージ往療加算単価Ｗ = Ｊマッサージ往療料Ｗ -  Ｍ往療基本料金Ｗ
020710               COMPUTE Ｊマッサージ往療加算料Ｗ = Ｊマッサージ往療加算料Ｗ + (Ｊマッサージ往療料Ｗ -  Ｍ往療基本料金Ｗ)
020720*             /-- レセプトＦ用：加算の往療距離が違う日があるか判定 --/
020730               IF Ｊ退避往療距離Ｗ = ZERO
020740                  MOVE Ｈ日－往療距離  TO Ｊ退避往療距離Ｗ
020750               ELSE
020760                  IF Ｈ日－往療距離 NOT = Ｊ退避往療距離Ｗ
020770                     MOVE 1 TO Ｊ往療加算複数判定Ｗ
020780                  END-IF
020790               END-IF
020800*
020810           END-IF
020820*------------------------------------------------------------------------------------*
020830*       ※ 上記後、按分の場合は、往療料を上書き
020840           IF Ｈ日－往療按分区分 = 1
020850              MOVE Ｈ日－往療按分金額  TO Ｊマッサージ往療料Ｗ
020860           END-IF
020870
020882*------------------------------------------------------------------------------------*
020890*       ※ 上記後、レセプトＦ用に往療料を累計
020900           COMPUTE Ｊマッサージ累計往療料Ｗ = Ｊマッサージ累計往療料Ｗ + Ｊマッサージ往療料Ｗ
020910*------------------------------------------------------------------------------------*
020920*
020930        END-IF
020940*
020950*       ※ 往療料なしの場合はゼロ円上書き
020960        IF Ｈ往実－按分なし特定区分 = 2
020970           ADD 1                    TO 往療０円カウンタ
020980           ADD 1                    TO Ｊマッサージ往療回数Ｗ
020990           MOVE ZERO                TO Ｊマッサージ往療料Ｗ
021000        END-IF
021010*
021020*       施術報告書交付料
021021        IF (Ｈ日－施術和暦年月日     >= "4301001") AND
021022           (Ｈ日－施術報告書交付区分  = 1) AND
021023           (Ｈ日－施術和暦年月       >= 交付可能和暦年月Ｗ)
021024            ADD 1                        TO Ｊマッサージ報告書交付回数Ｗ
021025            ADD 施術報告書交付料単価Ｗ   TO Ｊマッサージ報告書交付料Ｗ
021026            MOVE 施術報告書交付料単価Ｗ  TO 計算用報告書交付料Ｗ
021028        ELSE
021029            MOVE ZERO                    TO 計算用報告書交付料Ｗ
021030        END-IF
021031
021032*       助成分に加算
021035        COMPUTE Ｊマッサージ合計額Ｗ = Ｊマッサージ料Ｗ + Ｊマッサージ変形徒手矯正術Ｗ +
021040                                       Ｊマッサージ温罨法料Ｗ + Ｊマッサージ温罨法電療料Ｗ + Ｊマッサージ往療料Ｗ + 
021041                                       計算用報告書交付料Ｗ
021051        COMPUTE Ｊマッサージ累計額Ｗ = Ｊマッサージ累計額Ｗ + Ｊマッサージ合計額Ｗ
021060        COMPUTE Ｊマッサージ料月計Ｗ = Ｊマッサージ料月計Ｗ + Ｊマッサージ料Ｗ
021070        COMPUTE Ｊ変形徒手料月計Ｗ   = Ｊ変形徒手料月計Ｗ   + Ｊマッサージ変形徒手矯正術Ｗ
021080*
021090     END-IF.
021100*------------------------------------------------------------------------------------*
021110*------------------------------------------------------------------------------------*
021120*
021130*================================================================*
021140 日計負担額取得 SECTION.
021150*
021160* 負担額取得
021170*   / Ｈ連計算の助成パラメタクリアー /
021180     MOVE SPACE TO Ｈ連計算－初検日フラグ.
021190     MOVE SPACE TO Ｈ連計算－初診療フラグ.
021200     MOVE ZERO  TO Ｈ連計算－初検料.
021210     MOVE ZERO  TO Ｈ連計算－実日数.
021220     MOVE ZERO  TO Ｈ連計算－その他計算区分１.
006620     MOVE ZERO  TO Ｈ連計算２－日計区分.
021230*
021240*    2014/05/08 領収書負担金計算区分に3追加対応
021250     IF 領収書負担金計算区分Ｗ = 1 OR 2 OR 3
021260*       / 1円単位 /
021270        MOVE ZERO TO Ｈ連計算－負担金計算区分
021280     ELSE
021290*       / 10円単位 /
021300        MOVE 2    TO Ｈ連計算－負担金計算区分
021310     END-IF.
021320*
      */日計負担額は少数以下一位を四捨五入↓↓↓/20220601
           IF Ｈ連計算－施術和暦年月 >= 50406
021270         MOVE ZERO TO Ｈ連計算－負担金計算区分
006620         MOVE 1    TO Ｈ連計算２－日計区分
           END-IF.
      */日計負担額は少数以下一位を四捨五入↑↑↑/20220601
021330     MOVE Ｈ日－施術日 TO Ｈ連計算－施術日.
021340*
021350*
021360**---  助成・大阪丸証時------------------------------------------------------------------*
021370*   当月の該当施術日までの回数（何回目か）(枝番も含む)(負担区分6,8,13用)をセットする。
021380     IF ( 助成種別Ｗ NOT = ZERO ) OR ( 大阪丸証フラグ = "YES" )
021390         MOVE ZERO TO 回数カウンタ
021400         PERFORM VARYING カウンタ FROM 1 BY 1
021410                         UNTIL ( カウンタ > 31 ) OR ( カウンタ > Ｈ日－施術日 )
021420            IF 通院日２Ｗ(カウンタ) NOT = ZERO
021430*              / 助成月途中開始を考慮 /
021440               IF ( 助成月途中開始日Ｗ = ZERO ) OR
021450                  (( 助成月途中開始日Ｗ NOT = ZERO ) AND ( カウンタ >= 助成月途中開始日Ｗ ))
021460                  COMPUTE 回数カウンタ = 回数カウンタ + 1
021470               END-IF
021480            END-IF
021490         END-PERFORM
021500*
021510         MOVE 回数カウンタ  TO Ｈ連計算－実日数
021520*----------------------------------------------*
021530**** 2017/08/31 大阪市の障害者医療助成についてはそれぞれ５００円２回徴収する様に仕様変更（特殊ロジックコメントアウト）
021540**        / 上記後 大阪市のみ、はりきゅうとマッサージ両方の日は、マッサージの方は、0回目とする
021550**         IF 費用負担者番号助成Ｗ(3:3) = "274"
021560**            IF 通院日Ｗ(Ｈ日－施術日) = 3
021570**               IF Ｈ日－施術区分 = 2
021580**                  MOVE ZERO TO Ｈ連計算－実日数
021590**               END-IF
021600**            END-IF
021610**         END-IF
021620**
021630*----------------------------------------------*
021640*
021650     END-IF.
021660**--------------------------------------------------------------------------------------*
021670*
021680*
021690     MOVE 大阪丸証フラグ TO Ｈ連計算大阪－大阪丸証フラグ.
021700*
021710
021720
021730     MOVE ZERO TO 連－日計レセＦ
021740
021750     CALL   "KHT41410".
021760     CANCEL "KHT41410".
021770*
021780* ファイルへセット
021790     MOVE Ｈ連計算－費用額  TO  Ｈ日－費用額.
021800*
021810*    / 助成は、負担額助成をセット /
021820     IF 助成種別Ｗ NOT = ZERO
021830        MOVE Ｈ連計算－負担額助成 TO Ｈ日－一部負担金  Ｈ日－例外一部負担金
      */負担額切り上げに対応↓↓↓/20221125
              IF (受－費用負担者番号助成(3:2) = "27") AND ( 負担区分Ｗ = 06 OR 22 )
021830            MOVE Ｈ連計算３－負担額助成 TO Ｈ日－一部負担金  Ｈ日－例外一部負担金
              END-IF
      */負担額切り上げに対応↑↑↑/20221125
021840*
021850*       / 累計助成 /
021860        IF ( 負担区分Ｗ = 05 OR 16 )
021870*
021880           IF ( 助成月途中開始日Ｗ = ZERO )
021890               COMPUTE Ｈ連計算助成累計額－負担額助成 = Ｈ連計算助成累計額－負担額助成 + Ｈ連計算－負担額助成
021900           ELSE
021910              IF ( Ｈ連計算－施術日 >= 助成月途中開始日Ｗ )
021920                  COMPUTE Ｈ連計算助成累計額－負担額助成 = Ｈ連計算助成累計額－負担額助成 + Ｈ連計算－負担額助成
021930              END-IF
021940           END-IF
021950        END-IF
021978     ELSE
021980        MOVE Ｈ連計算－負担額     TO Ｈ日－一部負担金  Ｈ日－例外一部負担金
021990     END-IF.
022000*
022010*
022020*---------------------------------------------------------------------------------------*
022030* ※ 入金額は、会計領収Fにない時、一部負担金 + 自費で再計算
022040     MOVE SPACE TO 会計領収無しフラグ.
022050     MOVE Ｈ日－施術区分     TO 領－施術区分.
022060* 1:日、2:月、3:年
022070     MOVE 1                  TO 領－会計領収区分.
022080     MOVE Ｈ日－施術和暦     TO 領－施術和暦.
022090     MOVE Ｈ日－施術年       TO 領－施術年.
022100     MOVE Ｈ日－施術月       TO 領－施術月.
022110     MOVE Ｈ日－施術日       TO 領－施術日.
022120     MOVE Ｈ日－患者番号     TO 領－患者番号.
022130     MOVE Ｈ日－枝番         TO 領－枝番.
022140     READ 会計領収Ｆ
022150     INVALID KEY
022160*
022170        COMPUTE Ｈ日－入金額 = Ｈ日－一部負担金 + Ｈ日－自費額
022180        MOVE "YES" TO 会計領収無しフラグ
022190*
022200     END-READ.
022210*---------------------------------------------------------------------------------------*
022220*
022230*
022240*
022250*---------------------------------------------------------------------------------------*
022260**※--- 償還払い+ 助成・大阪丸証時------------------------------------------------------*
022270* 上記の通常金額をセット後、負担額を1円単位で算出(負担区分6,8+大阪丸証用)
022280     IF ( 助成種別Ｗ NOT = ZERO  ) OR ( 大阪丸証フラグ = "YES" ) OR
022290        ( 本体償還払い区分Ｗ = 1 ) OR ( 助成償還払い区分Ｗ = 1 ) OR ( 本体まとめ区分Ｗ = 1 )
022300*
022310        MOVE ZERO  TO Ｈ連計算－負担金計算区分
022320
022330
022340        MOVE 1     TO 連－日計レセＦ
022350
022360        CALL   "KHT41410"
022370        CANCEL "KHT41410"
022380*
022390        IF ( 助成種別Ｗ NOT = ZERO )
022400           MOVE Ｈ連計算－負担額助成 TO Ｈ日－助成計算負担額  Ｈ日－例外助成計算負担額
      */負担額切り上げに対応↓↓↓/20221125
                 IF (受－費用負担者番号助成(3:2) = "27") AND ( 負担区分Ｗ = 06 OR 22 )
022400                MOVE Ｈ連計算３－負担額助成 TO Ｈ日－助成計算負担額  Ｈ日－例外助成計算負担額
                 END-IF
      */負担額切り上げに対応↑↑↑/20221125
022410        ELSE
022420           MOVE Ｈ連計算－負担額     TO Ｈ日－助成計算負担額  Ｈ日－例外助成計算負担額
022430        END-IF
022440*
022450*
022460*---------------------------------------------------------------------------*
022470*** 上記後（最後に）、償還払い・本体まとめの金額を算出し、セットし直す。
022480* Ｈ日－例外一部負担金とＨ日－例外助成計算負担額は、通常（償還でない）値がセットされている。
022490*
022500        IF ( 本体償還払い区分Ｗ = 1 ) OR ( 助成償還払い区分Ｗ = 1 ) OR ( 本体まとめ区分Ｗ = 1 )
022510           IF ( 本体償還払い区分Ｗ = 1 ) OR ( 助成償還払い区分Ｗ = 1 )
022520              IF 助成種別Ｗ = ZERO
022530*                /本体のみ償還/
022540                 IF 本体償還払い区分Ｗ = 1
022550                    MOVE Ｈ日－費用額  TO Ｈ日－一部負担金 Ｈ日－助成計算負担額
022560                 END-IF
022570              ELSE
022580                 EVALUATE TRUE
022590*                /本体償還、助成通常/
022600                 WHEN ( 本体償還払い区分Ｗ = 1 ) AND ( 助成償還払い区分Ｗ = ZERO )
022610                    COMPUTE Ｈ日－一部負担金 =  ( Ｈ日－費用額 - Ｈ連計算－負担額 ) + Ｈ連計算－負担額助成
022620                    MOVE Ｈ日－一部負担金  TO Ｈ日－助成計算負担額
022630*                /本体通常、助成償還/
022640                 WHEN ( 本体償還払い区分Ｗ = ZERO ) AND ( 助成償還払い区分Ｗ = 1 )
022650                    MOVE Ｈ連計算－負担額  TO Ｈ日－一部負担金 Ｈ日－助成計算負担額
022660*                /本体償還、助成償還/
022670                 WHEN ( 本体償還払い区分Ｗ = 1 ) AND ( 助成償還払い区分Ｗ = 1 )
022680                    MOVE Ｈ日－費用額  TO Ｈ日－一部負担金 Ｈ日－助成計算負担額
022690                 END-EVALUATE
022700              END-IF
022710           ELSE
022720*             / 本体まとめのみ /
022730*             平成29年4月1日以降では一部負担有りの場合のみ、本体まとめであっても負担額を出す 2017/02/27 2017/04/03
022740              IF (助成種別Ｗ                 = 55) AND
022750                 (費用負担者番号助成Ｗ(3:2)  = "14") AND
022760                 (負担区分Ｗ             NOT = ZERO)
022770                 MOVE "YES"  TO 助成負担ありフラグ
022780              ELSE
022790                  MOVE ZERO  TO Ｈ日－一部負担金 Ｈ日－助成計算負担額
022800              END-IF
022810           END-IF
022820**
022830*          入金額は、会計領収Fにない時、一部負担金 + 自費で再計算
022840           IF 会計領収無しフラグ = "YES"
022850              COMPUTE Ｈ日－入金額 = Ｈ日－一部負担金 + Ｈ日－自費額
022860           END-IF
022870*
022880        END-IF
022890*---------------------------------------------------------------------------*
022900     END-IF.
022910*---------------------------------------------------------------------------------------*
022920*
022930*================================================================*
022940*================================================================*
022950*================================================================*
022960 レセプト処理 SECTION.
022970*
006460     INITIALIZE Ｈ連計算２－キー.
022980     MOVE Ｈ連計算－施術区分  TO  Ｈレセ－施術区分.
022990     MOVE Ｈ連計算－施術和暦  TO  Ｈレセ－施術和暦.
023000     MOVE Ｈ連計算－施術年    TO  Ｈレセ－施術年.
023010     MOVE Ｈ連計算－施術月    TO  Ｈレセ－施術月.
023020     MOVE Ｈ連計算－患者番号  TO  Ｈレセ－患者番号.
023030     MOVE Ｈ連計算－枝番      TO  Ｈレセ－枝番.
023040     MOVE 1                   TO  Ｈレセ－レセ種別.
023050*
023060     START ＨレセプトＦ KEY IS >= Ｈレセ－施術区分
023070                                  Ｈレセ－施術和暦年月
023080                                  Ｈレセ－患者コード
023090                                  Ｈレセ－レセ種別
023100     END-START.
023110*
023120     IF 状態キー = "00"
023130         MOVE SPACE TO 初回フラグ
023140         MOVE SPACE TO 終了フラグ
023150         PERFORM ＨレセプトＦ読込
023160*
023170*        繰り返し（助成あるかも）
023180         PERFORM UNTIL ( Ｈレセ－施術区分   NOT = Ｈ連計算－施術区分   ) OR
023190                       ( Ｈレセ－施術和暦   NOT = Ｈ連計算－施術和暦   ) OR
023200                       ( Ｈレセ－施術年     NOT = Ｈ連計算－施術年     ) OR
023210                       ( Ｈレセ－施術月     NOT = Ｈ連計算－施術月     ) OR
023220                       ( Ｈレセ－患者コード NOT = Ｈ連計算－患者コード ) OR
023230                       ( 終了フラグ         NOT = SPACE )
023240*
023250**           / ファイルの金額欄初期化 /
023260             INITIALIZE Ｈレセ－金額合計欄
023270             INITIALIZE Ｈレセ－はり金額欄
023280             INITIALIZE Ｈレセ－マッサージ金額欄
023290             INITIALIZE Ｈレセ－例外レセ金額合計欄
023300             INITIALIZE Ｈレセ－例外本体枝番まとめ単独金額合計欄
023310             INITIALIZE Ｈレセ－施術報告書交付料欄
023311*
023320             INITIALIZE Ｈレセ－マッサージ部位別金額欄
023330**
023340             IF 初回フラグ = SPACE
023350                EVALUATE Ｈレセ－施術区分
023360                WHEN 1
023370                   IF はり累計額Ｗ NOT = ZERO
023380                      PERFORM はりレセ料金計算
023390                   END-IF
023400                WHEN 2
023410                   IF マッサージ累計額Ｗ NOT = ZERO
023420                      PERFORM マッサージレセ料金計算
023430                   END-IF
023440                WHEN OTHER
023450                   DISPLAY "レセ施術区分エラー　年月：" Ｈレセ－施術和暦年月   UPON CONS
023460                END-EVALUATE
023470*
023480*              / 助成レコードのために退避 /
023490                INITIALIZE 退避レセ－レコード
023500                MOVE Ｈレセ－レコード TO 退避レセ－レコード
023510                MOVE "YES" TO 初回フラグ
023520             ELSE
023530                IF Ｈレセ－レセ種別 = 3
023540*
023550                   IF 助成月途中開始日Ｗ = ZERO
023560*                     / 助成レコード時は、退避したデータをセット /
023570                      MOVE 退避レセ－金額合計欄         TO Ｈレセ－金額合計欄
023580                      MOVE 退避レセ－はり金額欄         TO Ｈレセ－はり金額欄
023590                      MOVE 退避レセ－マッサージ金額欄   TO Ｈレセ－マッサージ金額欄
023600                      MOVE 退避レセ－例外レセ金額合計欄 TO Ｈレセ－例外レセ金額合計欄
023610*                     / 助成金額セット /
023620                      MOVE Ｈ連計算－負担額助成         TO Ｈレセ－受給者負担額
023630                      MOVE Ｈ連計算－請求額助成         TO Ｈレセ－助成請求金額
      */請求額助成の６桁ver↓↓↓/20231213
                            IF (Ｈ連計算－請求額助成２ NOT = ZERO) AND (Ｈ連計算－請求額助成２ > Ｈ連計算－請求額助成)
027010                          MOVE Ｈ連計算－請求額助成２ TO Ｈレセ－助成請求金額
                            END-IF
      */請求額助成の６桁ver↑↑↑/20231213
      */負担額切り上げに対応↓↓↓/20221125
                            IF (受－費用負担者番号助成(3:2) = "27") AND ( 負担区分Ｗ = 06 OR 22 )
023620                          MOVE Ｈ連計算３－負担額助成   TO Ｈレセ－受給者負担額
                            END-IF
      */負担額切り上げに対応↑↑↑/20221125
      */大阪助成：日の上限を1度も越えない場合、本体の負担額と差が合っても請求しない↓↓↓/20220830
                            IF (負担区分Ｗ = 22 AND 受－費用負担者番号助成(3:2) = "27" AND 全額患者負担Ｆ = "YES")
                                MOVE Ｈレセ－負担額 TO Ｈレセ－受給者負担額
                                MOVE ZERO           TO Ｈレセ－助成請求金額
                            END-IF
      */大阪助成：日の上限を1度も越えない場合、本体の負担額と差が合っても請求しない↑↑↑/20220830
023640*
023650*                     本体分計算値で特殊施術内容をセット（20180118 マッサージ固有追加）
023660                      MOVE 退避レセ－マッサージ固有     TO Ｈレセ－マッサージ固有
023670                      MOVE 退避レセ－マッサージ部位別金額欄 TO Ｈレセ－マッサージ部位別金額欄
023680
023681*                     施術報告書
023682                      MOVE 退避レセ－施術報告書交付料欄 TO Ｈレセ－施術報告書交付料欄
023683                   ELSE
023690*                     / 助成月途中は、Ｊ累計情報Ｗを累計情報Ｗへ転記後、レセ料金計算 /
023700                      MOVE Ｊ累計情報Ｗ                 TO 累計情報Ｗ
023710                      MOVE Ｊマッサージ固有Ｗ           TO マッサージ固有Ｗ
023720                      MOVE Ｊマッサージ部位別金額欄Ｗ   TO マッサージ部位別金額欄Ｗ
023730                      EVALUATE Ｈレセ－施術区分
023740                      WHEN 1
023750                         IF はり累計額Ｗ NOT = ZERO
023760                            PERFORM はりレセ料金計算
023770                         END-IF
023780                      WHEN 2
023790                         IF マッサージ累計額Ｗ NOT = ZERO
023800                            PERFORM マッサージレセ料金計算
023810                         END-IF
023820                      END-EVALUATE
023830*                     / 助成金額セット /
023840                      MOVE Ｈ連計算－負担額助成         TO Ｈレセ－受給者負担額
023850                      MOVE Ｈ連計算－請求額助成         TO Ｈレセ－助成請求金額
      */負担額切り上げに対応↓↓↓/20221125
                            IF (受－費用負担者番号助成(3:2) = "27") AND ( 負担区分Ｗ = 06 OR 22 )
023840                          MOVE Ｈ連計算３－負担額助成   TO Ｈレセ－受給者負担額
                            END-IF
      */負担額切り上げに対応↑↑↑/20221125
023862*
023870                   END-IF
023880*                  / 上記後実施 /
023890                   PERFORM 例外助成レセ金額セット
023900*
023910*                  2017/02/27 医療助成が本体まとめであってもしっかり計算させる
023920*                  平成29年4月1日以降では一部負担有りの場合のみ、本体まとめであっても負担額を出す 2017/02/27 2017/04/03
023930*                  本体まとめを行っている場合、本体分に助成分の一部負担額を反映させる
023940                   IF ( 本体まとめ区分Ｗ       = 1 ) AND
023950                      (助成負担ありフラグ  NOT = SPACE)
023960                      INITIALIZE 更新Ｈレセ－レコード
023970                      MOVE 退避レセ－レコードキー  TO  更新Ｈレセ－レコードキー
023980                      READ 更新ＨレセプトＦ
023990                      NOT INVALID KEY
024000*                         本体分にまとめる助成分を調整する
024010*       変わらない ->     COMPUTE 更新Ｈレセ－費用額 = 更新Ｈレセ－費用額
024020                          COMPUTE 更新Ｈレセ－負担額 = 更新Ｈレセ－負担額 + Ｈ連計算－負担額助成
024030                          COMPUTE 更新Ｈレセ－請求額 = 更新Ｈレセ－請求額 - Ｈ連計算－負担額助成
024040*                         助成分を設定
024050                          MOVE Ｈ連計算－負担額助成    TO  更新Ｈレセ－受給者負担額
024060                          MOVE Ｈ連計算－請求額助成    TO  更新Ｈレセ－助成請求金額
024070                          MOVE 更新Ｈレセ－負担額      TO  更新Ｈレセ－例外負担額
024080                          MOVE 更新Ｈレセ－請求額      TO  更新Ｈレセ－例外請求額
024090                          MOVE Ｈ連計算－負担額助成    TO  更新Ｈレセ－例外受給者負担額
024100                          MOVE Ｈ連計算－請求額助成    TO  更新Ｈレセ－例外助成請求金額
024110*                         本体レコードを更新
024120                          REWRITE 更新Ｈレセ－レコード
024130                          IF 状態キー NOT = "00"
024140                             MOVE NC"更新Ｈレセ" TO ファイル名
024150                             PERFORM エラー表示
024160                          END-IF
024170                      END-READ
024180                   END-IF
024190*
024200                ELSE
024210                   DISPLAY "レセ種別が健保以外です。年月－種別：" Ｈレセ－施術和暦年月 "－"  Ｈレセ－レセ種別 UPON CONS
024220                END-IF
024230             END-IF
024240**
      *特殊施術時/20241203↓↓↓
                   IF Ｈレセ－変形徒手矯正術特殊施術フラグ = 1
002854                 COMPUTE 変形徒手矯正術回数Ｗ = Ｈレセ－変形徒手矯正術右上肢回数 +
002857                                                Ｈレセ－変形徒手矯正術左上肢回数 +
002860                                                Ｈレセ－変形徒手矯正術右下肢回数 +
002864                                                Ｈレセ－変形徒手矯正術左下肢回数
                       IF ((Ｈレセ－変形徒手矯正術右上肢金額 NOT = ZERO)  OR
                           (Ｈレセ－変形徒手矯正術左上肢金額 NOT = ZERO)  OR
                           (Ｈレセ－変形徒手矯正術右下肢金額 NOT = ZERO)  OR
                           (Ｈレセ－変形徒手矯正術左下肢金額 NOT = ZERO)) AND
                          ( 変形徒手矯正術回数Ｗ             NOT = ZERO )
002855                 COMPUTE 変形徒手矯正術単価Ｗ = 
002855                                               (Ｈレセ－変形徒手矯正術右上肢金額 +
002858                                                Ｈレセ－変形徒手矯正術左上肢金額 +
002861                                                Ｈレセ－変形徒手矯正術右下肢金額 +
002865                                                Ｈレセ－変形徒手矯正術左下肢金額) / 変形徒手矯正術回数Ｗ
                       END-IF
                   ELSE
      *特殊施術時/20241203↑↑↑
                   IF Ｈレセ－変形徒手矯正術回数 NOT = ZERO
                       COMPUTE 変形徒手矯正術単価Ｗ = Ｈレセ－変形徒手矯正術単価 / 
                              (Ｈレセ－変形徒手矯正術右上肢 + Ｈレセ－変形徒手矯正術左上肢 +
                               Ｈレセ－変形徒手矯正術右下肢 + Ｈレセ－変形徒手矯正術左下肢 )
                       COMPUTE 変形徒手矯正術回数Ｗ = Ｈレセ－変形徒手矯正術回数 * 
                              (Ｈレセ－変形徒手矯正術右上肢 + Ｈレセ－変形徒手矯正術左上肢 +
                               Ｈレセ－変形徒手矯正術右下肢 + Ｈレセ－変形徒手矯正術左下肢 )
                   ELSE
                       COMPUTE 変形徒手矯正術単価Ｗ = Ｈレセ－変形徒手矯正術単価
                       COMPUTE 変形徒手矯正術回数Ｗ = 
                               Ｈレセ－変形徒手矯正術右上肢回数 + Ｈレセ－変形徒手矯正術左上肢回数 +
                               Ｈレセ－変形徒手矯正術右下肢回数 + Ｈレセ－変形徒手矯正術左下肢回数
                   END-IF
      *特殊施術時/20241203↓
                   END-IF
      *
024251             REWRITE Ｈレセ－レコード
024260             IF 状態キー NOT = "00"
024270                  MOVE NC"ＨレセプトＦ" TO ファイル名
024280                  PERFORM エラー表示
024290             END-IF
024300**
024310             PERFORM ＨレセプトＦ読込
024320         END-PERFORM
024330     END-IF.
024340*
024350*================================================================*
024360 ＨレセプトＦ読込 SECTION.
024370*
024380     READ ＨレセプトＦ NEXT
024390     AT END
024400         MOVE "YES"  TO 終了フラグ
024410     END-READ.
024420*
024430*================================================================*
024440 はりレセ料金計算 SECTION.
024450*
024460*--明細（日計時の累計情報よりセット）-------------*
024470*
024480* はり初回
024490     MOVE はり初回施術内容区分Ｗ TO Ｈレセ－はり初回施術内容区分.
024500     IF はり初回施術内容区分Ｗ NOT = ZERO
024510        MOVE はり初回料Ｗ TO Ｈレセ－はり初回料
024520     END-IF.
024530* 後療はり
024540     IF はり回数Ｗ NOT = ZERO
024550        MOVE Ｈ１術Ｗ   TO Ｈレセ－後療単価はり
024560     END-IF.
024570     MOVE はり回数Ｗ    TO Ｈレセ－後療回数はり.
024580     COMPUTE Ｈレセ－後療料はり = Ｈレセ－後療単価はり * Ｈレセ－後療回数はり.
024590* 後療はり電気
024600     IF はり電気回数Ｗ NOT = ZERO
024610        COMPUTE Ｈレセ－後療単価はり電気 = Ｈ１術Ｗ + Ｈ電療料Ｗ
024620     END-IF.
024630     MOVE はり電気回数Ｗ TO Ｈレセ－後療回数はり電気.
024640     COMPUTE Ｈレセ－後療料はり電気 = Ｈレセ－後療単価はり電気 * Ｈレセ－後療回数はり電気.
024650* 後療きゅう
024660     IF きゅう回数Ｗ NOT = ZERO
024670        MOVE Ｈ１術Ｗ   TO Ｈレセ－後療単価きゅう
024680     END-IF.
024690     MOVE きゅう回数Ｗ  TO Ｈレセ－後療回数きゅう.
024700     COMPUTE Ｈレセ－後療料きゅう = Ｈレセ－後療単価きゅう * Ｈレセ－後療回数きゅう.
024710* 後療きゅう電気
024720     IF きゅう電気回数Ｗ NOT = ZERO
024730        COMPUTE Ｈレセ－後療単価きゅう電気 = Ｈ１術Ｗ + Ｈ電療料Ｗ
024740     END-IF.
024750     MOVE きゅう電気回数Ｗ TO Ｈレセ－後療回数きゅう電気.
024760     COMPUTE Ｈレセ－後療料きゅう電気 = Ｈレセ－後療単価きゅう電気 * Ｈレセ－後療回数きゅう電気.
024770* 後療はりきゅう
024780     IF はりきゅう回数Ｗ NOT = ZERO
024790        MOVE Ｈ２術Ｗ   TO Ｈレセ－後療単価はりきゅう
024800     END-IF.
024810     MOVE はりきゅう回数Ｗ  TO Ｈレセ－後療回数はりきゅう.
024820     COMPUTE Ｈレセ－後療料はりきゅう = Ｈレセ－後療単価はりきゅう * Ｈレセ－後療回数はりきゅう.
024830* 後療はりきゅう電気
024840     IF はりきゅう電気回数Ｗ NOT = ZERO
024850        COMPUTE Ｈレセ－後療単価はりきゅう電気 = Ｈ２術Ｗ + Ｈ電療料Ｗ
024860     END-IF.
024870     MOVE はりきゅう電気回数Ｗ TO Ｈレセ－後療回数はりきゅう電気.
024880     COMPUTE Ｈレセ－後療料はりきゅう電気 = Ｈレセ－後療単価はりきゅう電気 * Ｈレセ－後療回数はりきゅう電気.
024890*
024900* 往療基本

HILO***       DISPLAY "61-5-1★ はり往療回数Ｗ " はり往療回数Ｗ

024910     IF はり往療回数Ｗ NOT = ZERO
024920        MOVE Ｈ往療基本料金Ｗ   TO Ｈレセ－はり往療単価
024930     END-IF.
024940     MOVE はり往療回数Ｗ  TO Ｈレセ－はり往療回数.
024950*----------------------------------------------------*
024960*   / 按分対応 /  往療０円有無判定追加
024970     IF (Ｈレセ－レセ往療按分区分 NOT = 1) AND (往療０円カウンタ = ZERO)
024980        COMPUTE Ｈレセ－はり往療料 = Ｈレセ－はり往療単価 * Ｈレセ－はり往療回数
024990     ELSE
025000        MOVE はり累計往療料Ｗ TO Ｈレセ－はり往療料
025010     END-IF.
025020*----------------------------------------------------*
025030*
025040* 往療加算
025050     IF 往療加算複数判定Ｗ NOT = 1
025060        IF はり往療加算回数Ｗ NOT = ZERO
025070           MOVE はり往療加算単価Ｗ  TO Ｈレセ－はり往療加算単価
025080        END-IF
025090     ELSE
025100*     / 加算距離複数の時 /
025110        MOVE 1 TO Ｈレセ－はり往療加算複数判定
025120     END-IF.
025130     MOVE はり往療加算回数Ｗ     TO Ｈレセ－はり往療加算回数.
025140     MOVE はり往療加算料Ｗ       TO Ｈレセ－はり往療加算料.
025150***     COMPUTE Ｈレセ－はり往療加算料 = Ｈレセ－はり往療加算単価 * Ｈレセ－はり往療加算回数.
025160*
025170*    施術報告書交付料
025172
025182     IF はり報告書交付料Ｗ NOT = ZERO
025187         MOVE はり報告書交付回数Ｗ    TO Ｈレセ－施術報告書交付回数
025188         MOVE 施術報告書交付料単価Ｗ  TO Ｈレセ－施術報告書交付料単価
025190         MOVE はり報告書交付料Ｗ      TO Ｈレセ－施術報告書交付料
025192     ELSE
025193         MOVE ZERO                    TO Ｈレセ－施術報告書交付回数
025194         MOVE ZERO                    TO Ｈレセ－施術報告書交付料単価
025195         MOVE ZERO                    TO Ｈレセ－施術報告書交付料
025196     END-IF.
025197*
025198*--合計---------------------------------------------------------*
025199*
025200* 連結の費用額セット
025201     MOVE はり累計額Ｗ  TO Ｈ連計算－費用額.
025210*
025221     PERFORM レセ負担額取得.
025230*
025240*--------------------------------------------------------------*
025250*
025260*
025270*================================================================*
025280 マッサージレセ料金計算 SECTION.
025290*
025300*--明細（日計時の累計情報よりセット）-------------*
025310*
025320*    マッサージ
025330     IF マッサージ回数Ｗ NOT = ZERO
025340        MOVE Ｍ１局所Ｗ         TO Ｈレセ－マッサージ単価
025350        MOVE マッサージ局所数Ｗ TO Ｈレセ－マッサージ局所数
025360     END-IF.
025370     MOVE マッサージ回数Ｗ      TO Ｈレセ－マッサージ回数.
025380     COMPUTE Ｈレセ－マッサージ料 = Ｈレセ－マッサージ単価 * Ｈレセ－マッサージ局所数 * Ｈレセ－マッサージ回数.
025390*    変形徒手矯正術
025400     IF マッサージ変形徒手矯正術回数Ｗ NOT = ZERO
025410        COMPUTE Ｈレセ－変形徒手矯正術単価 = Ｍ変形徒手矯正術Ｗ * 変形徒手矯正術数Ｗ
025420     END-IF.
025430     MOVE マッサージ変形徒手矯正術回数Ｗ   TO Ｈレセ－変形徒手矯正術回数.
025440     COMPUTE Ｈレセ－変形徒手矯正術料 = Ｈレセ－変形徒手矯正術単価 * Ｈレセ－変形徒手矯正術回数.
025450*
025460* 温罨法
025470     IF マッサージ温罨法回数Ｗ NOT = ZERO
025480        MOVE Ｍ温罨法料Ｗ   TO Ｈレセ－温罨法単価
025490     END-IF.
025500     MOVE マッサージ温罨法回数Ｗ   TO Ｈレセ－温罨法回数.
025510     COMPUTE Ｈレセ－温罨法料 = Ｈレセ－温罨法単価 * Ｈレセ－温罨法回数.
025520* 温罨法+電気
025530     IF マッサージ温罨法電気回数Ｗ NOT = ZERO
025540        COMPUTE Ｈレセ－温罨法電気単価 = Ｍ温罨法料Ｗ + Ｍ電療料Ｗ
025550     END-IF.
025560     MOVE マッサージ温罨法電気回数Ｗ   TO Ｈレセ－温罨法電気回数.
025570     COMPUTE Ｈレセ－温罨法電気料 = Ｈレセ－温罨法電気単価 * Ｈレセ－温罨法電気回数.
025580*
025590* 往療基本
025600     IF マッサージ往療回数Ｗ NOT = ZERO
025610        MOVE Ｍ往療基本料金Ｗ   TO Ｈレセ－マッサージ往療単価
025620     END-IF.
025630     MOVE マッサージ往療回数Ｗ  TO Ｈレセ－マッサージ往療回数.
025640*----------------------------------------------------*
025650*   / 按分対応 /  往療０円有無判定追加
025660     IF (Ｈレセ－レセ往療按分区分 NOT = 1) AND (往療０円カウンタ = ZERO)
025670        COMPUTE Ｈレセ－マッサージ往療料 = Ｈレセ－マッサージ往療単価 * Ｈレセ－マッサージ往療回数
025680     ELSE
025690        MOVE マッサージ累計往療料Ｗ TO Ｈレセ－マッサージ往療料
025700     END-IF.
025710*----------------------------------------------------*
025720*
025730* 往療加算
025740     IF 往療加算複数判定Ｗ NOT = 1
025750        IF マッサージ往療加算回数Ｗ NOT = ZERO
025760           MOVE マッサージ往療加算単価Ｗ  TO Ｈレセ－マッサージ往療加算単価
025770        END-IF
025780     ELSE
025790*     / 加算距離複数の時 /
025800        MOVE 1 TO Ｈレセ－マッサージ往療加算複数判定
025810     END-IF.
025820     MOVE マッサージ往療加算回数Ｗ        TO Ｈレセ－マッサージ往療加算回数.
025830     MOVE マッサージ往療加算料Ｗ          TO Ｈレセ－マッサージ往療加算料.
025840***     COMPUTE Ｈレセ－マッサージ往療加算料 = Ｈレセ－マッサージ往療加算単価 * Ｈレセ－マッサージ往療加算回数.
025850*
025860*    実際の施術内容を設定する
025870     IF 特殊施術フラグＷ = 1
025880*       施術毎の情報セット
025890        IF マッサージ特殊施術フラグＷ = 1
025900*          複数パターンの場合は合計金額のみセット
025910           MOVE マッサージ特殊施術Ｗ         TO Ｈレセ－マッサージ特殊施術
025920           MOVE 1                            TO Ｈレセ－マッサージ特殊施術フラグ
025930           IF マッサージ局所数複数Ｗ NOT = 1
025940              MOVE マッサージ局所数ＰＷ      TO Ｈレセ－マッサージ局所数
025950              MOVE マッサージ回数Ｗ          TO Ｈレセ－マッサージ回数
025960           ELSE
025970              MOVE ZERO                      TO Ｈレセ－マッサージ局所数
025980              MOVE ZERO                      TO Ｈレセ－マッサージ回数
025990           END-IF
026000           MOVE マッサージ料月計Ｗ           TO Ｈレセ－マッサージ料
026010        END-IF
026020        IF 変形徒手矯正術特殊施術フラグＷ = 1
026030*          複数パターンの場合は合計金額のみセット
026040           MOVE 変形徒手矯正術特殊施術Ｗ     TO Ｈレセ－変形徒手矯正術特殊施術
026050           MOVE 1                            TO Ｈレセ－変形徒手矯正術特殊施術フラグ
026060           IF 変形徒手矯正術数複数Ｗ NOT = 1
026070              COMPUTE Ｈレセ－変形徒手矯正術単価 = Ｍ変形徒手矯正術Ｗ * 変形徒手矯正術数ＰＷ
026080              MOVE 変形徒手矯正術数ＰＷ           TO Ｈレセ－変形徒手矯正術数
026090              MOVE マッサージ変形徒手矯正術回数Ｗ TO Ｈレセ－変形徒手矯正術回数
026100           ELSE
026110              COMPUTE Ｈレセ－変形徒手矯正術単価 = Ｍ変形徒手矯正術Ｗ * 1
026120              MOVE 1                         TO Ｈレセ－変形徒手矯正術数
026130              MOVE ZERO                      TO Ｈレセ－変形徒手矯正術回数
026140           END-IF
026150           MOVE 変形徒手料月計Ｗ             TO Ｈレセ－変形徒手矯正術料
026160        END-IF
026170     END-IF
026180*
026182*    施術報告書交付料
026194     IF マッサージ報告書交付料Ｗ NOT = ZERO
026195         MOVE マッサージ報告書交付回数Ｗ     TO Ｈレセ－施術報告書交付回数
026196         MOVE 施術報告書交付料単価Ｗ         TO Ｈレセ－施術報告書交付料単価
026202         MOVE マッサージ報告書交付料Ｗ       TO Ｈレセ－施術報告書交付料
026203     ELSE
026204         MOVE ZERO                           TO Ｈレセ－施術報告書交付回数
026205         MOVE ZERO                           TO Ｈレセ－施術報告書交付料単価
026206         MOVE ZERO                           TO Ｈレセ－施術報告書交付料
026207     END-IF.
026208*
026209*--合計---------------------------------------------------------*
026210*
026211* 連結の費用額セット
026212     MOVE マッサージ累計額Ｗ  TO Ｈ連計算－費用額.
026220*
026231     PERFORM レセ負担額取得.
026240*
026250*--------------------------------------------------------------*
026260*
026270*
026280*================================================================*
026290*================================================================*
026300 レセ負担額取得 SECTION.
026310*
026320* 負担額取得
026330*   / Ｈ連計算の助成パラメタクリアー /
026340     MOVE SPACE TO Ｈ連計算－初検日フラグ.
026350     MOVE SPACE TO Ｈ連計算－初診療フラグ.
026360     MOVE ZERO  TO Ｈ連計算－初検料.
026370     MOVE ZERO  TO Ｈ連計算－実日数.
026380     MOVE ZERO  TO Ｈ連計算－その他計算区分１.
006620     MOVE ZERO  TO Ｈ連計算２－日計区分.
026390*
026400*   / 1円単位 /
026410     MOVE ZERO  TO Ｈ連計算－負担金計算区分.
026420     MOVE ZERO  TO Ｈ連計算－施術日.
026430*
026440*   / 大阪丸証関係なし /
026450     MOVE SPACE TO Ｈ連計算大阪－大阪丸証フラグ.
026460*
026470     CALL   "KHT41410".
026480     CANCEL "KHT41410".
026490*
026500* ファイルへセット
026510*（同じ値をＨレセ－例外負担額とＨレセ－例外請求額にもセット）
026521     MOVE Ｈ連計算－費用額  TO  Ｈレセ－費用額.
026530     MOVE Ｈ連計算－負担額  TO  Ｈレセ－負担額 Ｈレセ－例外負担額.
026540     MOVE Ｈ連計算－請求額  TO  Ｈレセ－請求額 Ｈレセ－例外請求額.
026550*
026560*  （負担割合：%→割合へ）
026570     COMPUTE Ｈレセ－負担割合 =  Ｈ連計算－負担率 / 10.
026580*
026590**
026600*---------------------------------------------------------------------------------------*
026610**※--- 償還払い・本体まとめ時----------------------------------------------------------*
026620*** 上記後（最後に）、償還払い・本体まとめの金額を算出し、セットし直す。
026630     IF ( 本体償還払い区分Ｗ = 1 ) OR ( 助成償還払い区分Ｗ = 1 ) OR ( 本体まとめ区分Ｗ = 1 )
026640**
026650           IF ( 本体償還払い区分Ｗ = 1 ) OR ( 助成償還払い区分Ｗ = 1 )
026660              IF 助成種別Ｗ = ZERO
026670*                /本体のみ償還/
026680                 IF 本体償還払い区分Ｗ = 1
026690                    MOVE Ｈレセ－費用額  TO Ｈレセ－負担額
026700                    MOVE ZERO            TO Ｈレセ－請求額
026710                 END-IF
026720              ELSE
026730                 EVALUATE TRUE
026740*                /本体償還、助成通常/
026750                 WHEN ( 本体償還払い区分Ｗ = 1 ) AND ( 助成償還払い区分Ｗ = ZERO )
026760                    COMPUTE Ｈレセ－負担額 =  Ｈレセ－費用額 - Ｈ連計算－負担額
026770                    MOVE ZERO  TO Ｈレセ－請求額
026780*                /本体通常、助成償還/
026790                 WHEN ( 本体償還払い区分Ｗ = ZERO ) AND ( 助成償還払い区分Ｗ = 1 )
026800                    CONTINUE
026810*                /本体償還、助成償還/
026820                 WHEN ( 本体償還払い区分Ｗ = 1 ) AND ( 助成償還払い区分Ｗ = 1 )
026830                    COMPUTE Ｈレセ－負担額 =  Ｈレセ－費用額 - Ｈ連計算－負担額
026840                    MOVE ZERO  TO Ｈレセ－請求額
026850                 END-EVALUATE
026860              END-IF
026870           ELSE
026880*             / 本体まとめのみ /
026890              MOVE ZERO            TO Ｈレセ－負担額
026900              MOVE Ｈレセ－費用額  TO Ｈレセ－請求額
026910           END-IF
026920*
026930     END-IF.
026940*---------------------------------------------------------------------------------------*
026950*
026960*================================================================*
026970 例外助成レセ金額セット SECTION.
026980*
026990*（同じ値をＨレセ－例外受給者負担額とＨレセ－例外助成請求金額にもセット）
027000     MOVE Ｈ連計算－負担額助成       TO  Ｈレセ－例外受給者負担額.
027010     MOVE Ｈ連計算－請求額助成       TO  Ｈレセ－例外助成請求金額.
      */請求額助成の６桁ver↓↓↓/20231117
           IF (Ｈ連計算－請求額助成２ NOT = ZERO) AND (Ｈ連計算－請求額助成２ > Ｈ連計算－請求額助成)
027010         MOVE Ｈ連計算－請求額助成２ TO  Ｈレセ－例外助成請求金額
           END-IF.
      */請求額助成の６桁ver↑↑↑/20231117
      */負担額切り上げに対応↓↓↓/20221125
           IF (受－費用負担者番号助成(3:2) = "27") AND ( 負担区分Ｗ = 06 OR 22 )
027000         MOVE Ｈ連計算３－負担額助成 TO  Ｈレセ－例外受給者負担額
           END-IF.
      */負担額切り上げに対応↑↑↑/20221125
      */大阪助成：日の上限を1度も越えない場合、本体の負担額と差が合っても請求しない↓↓↓/20220830
           IF (負担区分Ｗ = 22 AND 受－費用負担者番号助成(3:2) = "27" AND 全額患者負担Ｆ = "YES")
               MOVE Ｈレセ－負担額 TO Ｈレセ－例外受給者負担額
               MOVE ZERO           TO Ｈレセ－例外助成請求金額
           END-IF
      */大阪助成：日の上限を1度も越えない場合、本体の負担額と差が合っても請求しない↑↑↑/20220830
027020*
027030**※--- 償還払い・本体まとめ時----------------------------------------------------------*
027040*** 上記後（最後に）、償還払い・本体まとめの金額を算出し、セットし直す。
027050     IF ( 本体償還払い区分Ｗ = 1 ) OR ( 助成償還払い区分Ｗ = 1 ) OR ( 本体まとめ区分Ｗ = 1 )
027060**
027070           IF ( 本体償還払い区分Ｗ = 1 ) OR ( 助成償還払い区分Ｗ = 1 )
027080              IF 助成種別Ｗ NOT = ZERO
027090                 EVALUATE TRUE
027100*                /本体償還、助成通常/
027110                 WHEN ( 本体償還払い区分Ｗ = 1 ) AND ( 助成償還払い区分Ｗ = ZERO )
027120                    CONTINUE
027130*                /本体通常、助成償還/
027140                 WHEN ( 本体償還払い区分Ｗ = ZERO ) AND ( 助成償還払い区分Ｗ = 1 )
027150                    MOVE ZERO    TO  Ｈレセ－受給者負担額
027160                    MOVE ZERO    TO  Ｈレセ－助成請求金額
027170*                /本体償還、助成償還/
027180                 WHEN ( 本体償還払い区分Ｗ = 1 ) AND ( 助成償還払い区分Ｗ = 1 )
027190                    MOVE Ｈレセ－例外負担額  TO Ｈレセ－受給者負担額
027200                    MOVE ZERO                TO Ｈレセ－助成請求金額
027210                 END-EVALUATE
027220              END-IF
027230           ELSE
027240*             / 本体まとめのみ /
027250              MOVE ZERO    TO  Ｈレセ－受給者負担額
027260              MOVE ZERO    TO  Ｈレセ－助成請求金額
027270              MOVE ZERO    TO  Ｈレセ－例外受給者負担額
027280              MOVE ZERO    TO  Ｈレセ－例外助成請求金額
027290           END-IF
027300*
027310     END-IF.
027320*
027330*================================================================*
027340*================================================================*
027350 助成負担区分取得 SECTION.
027360*
027370     MOVE ZERO  TO 負担区分Ｗ.
027380     MOVE 受－助成種別             TO 市－公費種別.
027390     MOVE 受－費用負担者番号助成   TO 市－市町村番号.
027400     READ 市町村マスタ
027410     NOT INVALID KEY
027420         MOVE 受－助成種別             TO 保特－保険種別
027430         MOVE 受－費用負担者番号助成   TO 保特－保険者番号
027440         MOVE Ｈ連計算－施術和暦       TO 保特－開始和暦
027450         MOVE Ｈ連計算－施術年         TO 保特－開始年
027460         MOVE Ｈ連計算－施術月         TO 保特－開始月
027470         START 保険者特別負担マスタ KEY IS <= 保特－保険種別
027480                                              保特－保険者番号
027490                                              保特－開始和暦年月
027500                                              REVERSED
027510         END-START
027520         IF 状態キー = "00"
027530             READ 保険者特別負担マスタ NEXT
027540             NOT AT END
027550                 IF ( 保特－保険種別   = 受－助成種別 ) AND
027560                    ( 保特－保険者番号 = 受－費用負担者番号助成 )
027570                    MOVE 保特－負担区分   TO 負担区分Ｗ
027580                 END-IF
027590             END-READ
027600         END-IF
027610     END-READ.
027620*
027630*================================================================*
027640*================================================================*
027650 枝番負担累計額計算 SECTION.
027660*
027670*  受－枝番作成時枝番で、参照受診者情報Ｆを読み、同じ助成なら、
027680*  参照Ｈ日－一部負担金を集計して、枝番負担累計額Ｗにセット。
027690*
027700*
027710     MOVE ZERO TO 枝番負担累計額Ｗ.
027720*
027730     OPEN INPUT 参照受診者情報Ｆ.
027740         MOVE NC"参照受診者" TO ファイル名.
027750         PERFORM オープンチェック.
027760*
027770     MOVE Ｈ連計算－施術和暦        TO 参照受－施術和暦.
027780     MOVE Ｈ連計算－施術年          TO 参照受－施術年.
027790     MOVE Ｈ連計算－施術月          TO 参照受－施術月.
027800     MOVE Ｈ連計算－患者番号        TO 参照受－患者番号.
027810*    / 受－枝番作成時枝番 /
027820     MOVE 受－枝番作成時枝番        TO 参照受－枝番.
027830*
027840     READ 参照受診者情報Ｆ
027850     NOT INVALID KEY
027860         IF ( 参照受－助成種別           = 受－助成種別 ) AND
027870            ( 参照受－費用負担者番号助成 = 受－費用負担者番号助成 ) AND
027880            ( 参照受－受益者番号助成     = 受－受益者番号助成 ) AND
027890            ( 参照受－助成負担金免除     = 受－助成負担金免除 )
027900*
027910               MOVE Ｈ連計算－施術区分  TO 参照Ｈ日－施術区分
027920               MOVE 参照受－患者番号    TO 参照Ｈ日－患者番号
027930               MOVE 参照受－枝番        TO 参照Ｈ日－枝番
027940               MOVE 参照受－施術和暦    TO 参照Ｈ日－施術和暦
027950               MOVE 参照受－施術年      TO 参照Ｈ日－施術年
027960               MOVE 参照受－施術月      TO 参照Ｈ日－施術月
027970*
027980*              / 助成月途中対応 /
027990               IF 参照受－助成月途中開始日 NOT = ZERO
028000                  MOVE 参照受－助成月途中開始日 TO 参照Ｈ日－施術日
028010               ELSE
028020                  MOVE ZERO                     TO 参照Ｈ日－施術日
028030               END-IF
028040*
028050               START 参照Ｈ日計データＦ KEY IS >= 参照Ｈ日－施術区分
028060                                                  参照Ｈ日－患者コード
028070                                                  参照Ｈ日－施術和暦年月日
028080               END-START
028090               IF 状態キー = "00"
028100                       MOVE SPACE TO 終了フラグ
028110                       PERFORM 参照Ｈ日計データＦ読込
028120*                      繰り返し（当月）
028130                       PERFORM UNTIL ( 参照Ｈ日－施術区分   NOT = Ｈ連計算－施術区分 ) OR
028140                                     ( 参照Ｈ日－患者コード NOT = 参照受－患者コード ) OR
028150                                     ( 参照Ｈ日－施術和暦   NOT = 参照受－施術和暦   ) OR
028160                                     ( 参照Ｈ日－施術年     NOT = 参照受－施術年     ) OR
028170                                     ( 参照Ｈ日－施術月     NOT = 参照受－施術月     ) OR
028180                                     ( 終了フラグ           NOT = SPACE )
028190*
028200                               COMPUTE 枝番負担累計額Ｗ  = 枝番負担累計額Ｗ + 参照Ｈ日－一部負担金
028210*
028220                               PERFORM 参照Ｈ日計データＦ読込
028230                       END-PERFORM
028240               END-IF
028250*
028260*
028270         END-IF
028280     END-READ.
028290*
028300     CLOSE 参照受診者情報Ｆ.
028310*
028320*    / 連結へセット /
028330     MOVE 枝番負担累計額Ｗ TO Ｈ連計算助成累計額－負担額助成.
028340*
028350*================================================================*
028360*================================================================*
028370 エラー表示 SECTION.
028380*
028390     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
028400     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
028410     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
028420     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"  UPON CONS.
028430*-----------------------------------------*
028440     CALL "actcshm"  WITH C LINKAGE.
028450*-----------------------------------------*
028460     ACCEPT  キー入力 FROM CONS.
028470*
028480*================================================================*
       同一建物患者数取得 SECTION.
      *
      */当日の同一建物者数をカウント
      *
           MOVE ZERO TO 同一建物患者数Ｗ 同一建物患者数Ｗ２.
      *     DISPLAY "41-5 " Ｈ日－レコードキー
013690     INITIALIZE Ｈ往実－レコード.
013700     MOVE Ｈ日－レコードキー TO Ｈ往実－レコードキー.
013710     READ Ｈ往療実績Ｆ
013720     INVALID
013730         INITIALIZE Ｈ往実－レコード
           NOT INVALID KEY 
      *     DISPLAY "41-2 " Ｈ往実－施術和暦年月日 " " Ｈ往実－患者コード " 施術者=" Ｈ往実－施術者番号  
      *     " 順=" Ｈ往実－登録順 " 往場=" Ｈ往実－往療場所区分 " 往場施=" Ｈ往実－往療場所施設コード 
      *     " 往場患=" Ｈ往実－往療場所患者コード
               MOVE Ｈ往実－往療場所区分       TO 往療場所区分Ｗ
               MOVE Ｈ往実－往療場所施設コード TO 往療場所施設コードＷ
               MOVE Ｈ往実－往療場所患者コード TO 往療場所患者コードＷ
      */施術区分・施術者番号も判定に含める
               MOVE Ｈ往実－施術区分           TO 施術区分Ｗ
               MOVE Ｈ往実－施術者番号         TO 施術者番号Ｗ
HILO  *         DISPLAY "610-30 " Ｈ往実－施術区分  " " Ｈ往実－施術者番号 "|"
013740     END-READ.
HILO  *     DISPLAY "当日の同一建物者数をカウント".
           MOVE Ｈ日－施術和暦年月日 TO Ｈ往実－施術和暦年月日.
           MOVE ZERO TO Ｈ往実－施術者番号 Ｈ往実－登録順.
      *
028050     START Ｈ往療実績Ｆ KEY IS >= Ｈ往実－施術和暦年月日
028060                                  Ｈ往実－施術者番号
028070                                  Ｈ往実－登録順
028080     END-START.
028090     IF 状態キー = "00"
               MOVE SPACE TO 終了フラグ４
               PERFORM Ｈ往療実績Ｆ読込２
023180         PERFORM UNTIL (Ｈ日－施術和暦年月日 NOT = Ｈ往実－施術和暦年月日) OR 
                             (終了フラグ４         NOT = SPACE)
      *         DISPLAY "610-3 " Ｈ往実－施術和暦年月日 " " Ｈ往実－患者コード
      *       " 施術者="         Ｈ往実－施術者番号  " 順=" Ｈ往実－登録順 
      *       " 往療場所="       Ｈ往実－往療場所区分
      *       " 往療場所施設="   Ｈ往実－往療場所施設コード
      *       " 往療場所患者="   Ｈ往実－往療場所患者コード
      *             IF (Ｈ往実－往療場所区分 = 2 OR 3) AND (往療場所施設コードＷ = Ｈ往実－往療場所施設コード)
      *                 COMPUTE 同一建物患者数Ｗ = 同一建物患者数Ｗ + 1
      *             END-IF
      *             IF (往療場所区分Ｗ = Ｈ往実－往療場所区分)
      *                 IF (往療場所施設コードＷ = Ｈ往実－往療場所施設コード) OR
      *                    (往療場所患者コードＷ = Ｈ往実－往療場所患者コード)
      *                     COMPUTE 同一建物患者数Ｗ = 同一建物患者数Ｗ + 1
      *                 END-IF
      *             END-IF
      *             IF (往療場所区分Ｗ       = Ｈ往実－往療場所区分) OR 
      *                (往療場所区分Ｗ       = 3                   ) OR 
      *                (Ｈ往実－往療場所区分 = 3                   )
      *                 IF 往療場所施設コードＷ NOT = ZERO
      *                     IF 往療場所施設コードＷ = Ｈ往実－往療場所施設コード
      *                         COMPUTE 同一建物患者数Ｗ = 同一建物患者数Ｗ + 1
      *                     END-IF
      *                 ELSE
      *                     IF 往療場所患者コードＷ = Ｈ往実－往療場所患者コード
      *                         COMPUTE 同一建物患者数Ｗ = 同一建物患者数Ｗ + 1
      *                     END-IF
      *                 END-IF
      *             END-IF
HILO  *      DISPLAY "610-31 " Ｈ往実－施術区分  " " Ｈ往実－施術者番号 "|"
000370*      DISPLAY "Ｈ往実－施術区分          "  Ｈ往実－施術区分      
000390*      DISPLAY "Ｈ往実－施術和暦年月日    "  Ｈ往実－施術和暦年月日
000460*      DISPLAY "Ｈ往実－患者コード        "  Ｈ往実－患者コード    
000510*      DISPLAY "Ｈ往実－施術者番号        "  Ｈ往実－施術者番号    
000600*      DISPLAY "Ｈ往実－往療場所区分      "  Ｈ往実－往療場所区分  
000600*      DISPLAY "Ｈ往実－往療場所患者コード"  Ｈ往実－往療場所患者コード
      */自費の場合同一建物患者数の対象外にする↓↓↓/20241206
                   PERFORM 参照受診者Ｆ取得
                   IF 参照受－保険種別 = 90
                       CONTINUE
                   ELSE
      */自費の場合同一建物患者数の対象外にする↑↑↑/20241206
                   IF ((往療場所区分Ｗ = 2 OR 3) AND (Ｈ往実－往療場所区分 = 2 OR 3)) OR
                      ((往療場所区分Ｗ = 1 OR 3) AND (Ｈ往実－往療場所区分 = 1 OR 3))
                       IF 往療場所施設コードＷ NOT = ZERO
                           IF (往療場所施設コードＷ = Ｈ往実－往療場所施設コード)
      */施術管理者単位で同一日同一建物で施術を行った総数(施術区分・施術者が別でも１集計)/20240902
      *                     AND (施術区分Ｗ           = Ｈ往実－施術区分          )
      *                     AND (施術者番号Ｗ         = Ｈ往実－施術者番号        )
                               COMPUTE 同一建物患者数Ｗ = 同一建物患者数Ｗ + 1
                               PERFORM 作業ファイル１作成
                           END-IF
                       ELSE
                           IF (往療場所患者コードＷ = Ｈ往実－往療場所患者コード)
      */施術管理者単位で同一日同一建物で施術を行った総数(施術区分・施術者が別でも１集計)/20240902
      *                     AND (施術区分Ｗ           = Ｈ往実－施術区分          )
      *                     AND (施術者番号Ｗ         = Ｈ往実－施術者番号       )
                               COMPUTE 同一建物患者数Ｗ = 同一建物患者数Ｗ + 1
                               PERFORM 作業ファイル１作成
                           END-IF
                       END-IF
                   END-IF
                   END-IF
                   PERFORM Ｈ往療実績Ｆ読込２
               END-PERFORM
HILO  *         DISPLAY "610-4-1 " Ｈ日－施術和暦年月日 " 当日の同一建物患者数=" 同一建物患者数Ｗ
               PERFORM 作２Ｆカウント
HILO  *         DISPLAY "610-4-2 " Ｈ日－施術和暦年月日 " 当日の同一建物患者数=" 同一建物患者数Ｗ２
      */同一日に同一患者で鍼マの両方を施術している場合は同一建物患者数は１カウント/20240902
               MOVE 同一建物患者数Ｗ２ TO 同一建物患者数Ｗ
           END-IF.
      */作業ファイルをクリアする
           CLOSE 作業ファイル２.
008320     OPEN OUTPUT 作業ファイル２.
008330         MOVE NC"作２" TO ファイル名.
008340         PERFORM オープンチェック.
           CLOSE 作業ファイル２.
008320     OPEN I-O 作業ファイル２.
008330         MOVE NC"作２" TO ファイル名.
008340         PERFORM オープンチェック.
013660*================================================================*
013670 Ｈ往療実績Ｆ読込２ SECTION.
013680*
013710     READ Ｈ往療実績Ｆ NEXT
013720     AT END
013730         MOVE "YES" TO 終了フラグ４
013740     END-READ.
013750*
013760*================================================================*
022960 レセプト詳細処理 SECTION.
022970*
HILO***       DISPLAY "610-20 受診" 受－レコードキー.
           MOVE SPACE TO Ｈレセ詳細－レコード.
           INITIALIZE    Ｈレセ詳細－レコード.
006480     MOVE Ｈ連計算－施術区分       TO Ｈレセ詳細－施術区分
006490     MOVE Ｈ連計算－施術和暦年月日 TO Ｈレセ詳細－施術和暦年月
006560     MOVE Ｈ連計算－患者コード     TO Ｈレセ詳細－患者コード
011580* レセ種別 1:一般,2:老人,3:助成,4:労災,5:自賠責,6:自費,7:生保単独,8保険証忘れ /
011650     IF 受－公費種別 NOT = ZERO
011660         MOVE 2                    TO Ｈレセ詳細－レセ種別
011670     ELSE
               IF 受－保険種別 = 85
011610             MOVE 7                TO Ｈレセ詳細－レセ種別
               ELSE
                   IF 受－保険種別 < 10
011680                 MOVE 1            TO Ｈレセ詳細－レセ種別
                   END-IF
               END-IF
011690     END-IF.
      *ロジックの見直し/20241202↓↓↓
      **/症状保存/20241112↓↓↓
      *     MOVE Ｈレセ詳細－レコードキー TO Ｈレセプト詳細レコードキーＷ.
      *     READ Ｈレセプト詳細Ｆ
      *     NOT INVALID KEY
      *         MOVE Ｈレセ詳細－症状 TO 症状Ｗ
      *     END-READ.
      *     MOVE SPACE TO Ｈレセ詳細－レコード.
      *     INITIALIZE    Ｈレセ詳細－レコード.
      *     MOVE Ｈレセプト詳細レコードキーＷ TO Ｈレセ詳細－レコードキー.
      *     MOVE 症状Ｗ                       TO Ｈレセ詳細－症状.
      **/症状保存/20241112↑↑↑
      **
      *     PERFORM レセ詳細レコードセット.
      **
      *     WRITE Ｈレセ詳細－レコード
      *     END-WRITE.
      *     EVALUATE 状態キー
      *     WHEN "00"
      *         CONTINUE
      *     WHEN "22"
      *         REWRITE Ｈレセ詳細－レコード
      *         IF 状態キー  NOT =  "00"
      *             MOVE NC"Ｈレセ詳細" TO ファイル名
      *             PERFORM エラー表示
      *         END-IF
      *     WHEN OTHER
      *         MOVE NC"Ｈレセ詳細" TO ファイル名
      *         PERFORM エラー表示
      *     END-EVALUATE.
      */症状保存/20241112↓↓↓
      */ここで繰り越すのをやめる
      *     PERFORM レセ詳細前月読込.
           MOVE Ｈレセ詳細－レコードキー TO Ｈレセプト詳細レコードキーＷ.
           READ Ｈレセプト詳細Ｆ
           INVALID KEY
      *         MOVE Ｈレセ詳細－症状 TO 症状Ｗ
               MOVE SPACE TO Ｈレセ詳細－レコード
               INITIALIZE    Ｈレセ詳細－レコード
               MOVE Ｈレセプト詳細レコードキーＷ TO Ｈレセ詳細－レコードキー
      *         MOVE 症状Ｗ                       TO Ｈレセ詳細－症状
               MOVE 参Ｈレセ詳細－症状           TO Ｈレセ詳細－症状
               PERFORM レセ詳細レコードセット
HILO***           DISPLAY "610-1 " Ｈレセ詳細－症状
               WRITE Ｈレセ詳細－レコード
               IF 状態キー  NOT =  "00"
                   MOVE NC"Ｈレセ詳細" TO ファイル名
                   PERFORM エラー表示
               END-IF
           NOT INVALID KEY
               MOVE Ｈレセ詳細－症状 TO 症状Ｗ
               MOVE SPACE TO Ｈレセ詳細－レコード
               INITIALIZE    Ｈレセ詳細－レコード
               MOVE Ｈレセプト詳細レコードキーＷ TO Ｈレセ詳細－レコードキー
               MOVE 症状Ｗ                       TO Ｈレセ詳細－症状
      *         IF (Ｈレセ詳細－症状 = SPACE) AND (参Ｈレセ詳細－症状 NOT = SPACE)
      *             MOVE 参Ｈレセ詳細－症状       TO Ｈレセ詳細－症状
      *         END-IF
               PERFORM レセ詳細レコードセット
HILO***           DISPLAY "610-2 " Ｈレセ詳細－症状
               REWRITE Ｈレセ詳細－レコード
               IF 状態キー  NOT =  "00"
                   MOVE NC"Ｈレセ詳細" TO ファイル名
                   PERFORM エラー表示
               END-IF
           END-READ.
      *ロジックの見直し/20241202↑↑↑
      *
011650     IF 受－助成種別 NOT = ZERO
011660*         MOVE 3 TO Ｈレセ詳細－レセ種別
      *
      *         WRITE Ｈレセ詳細－レコード
      *         END-WRITE
      *         EVALUATE 状態キー
      *         WHEN "00"
      *             CONTINUE
      *         WHEN "22"
      *             REWRITE Ｈレセ詳細－レコード
      *             IF 状態キー  NOT =  "00"
      *                 MOVE NC"Ｈレセ詳細" TO ファイル名
      *                 PERFORM エラー表示
      *             END-IF
      *         WHEN OTHER
      *             MOVE NC"Ｈレセ詳細" TO ファイル名
      *             PERFORM エラー表示
      *         END-EVALUATE
011660         MOVE 3 TO Ｈレセ詳細－レセ種別
               MOVE Ｈレセ詳細－レコードキー TO Ｈレセプト詳細レコードキーＷ
               READ Ｈレセプト詳細Ｆ
               INVALID KEY
      *             MOVE Ｈレセ詳細－症状 TO 症状Ｗ
                   MOVE SPACE TO Ｈレセ詳細－レコード
                   INITIALIZE    Ｈレセ詳細－レコード
                   MOVE Ｈレセプト詳細レコードキーＷ TO Ｈレセ詳細－レコードキー
      *             MOVE 症状Ｗ                       TO Ｈレセ詳細－症状
                   MOVE 参Ｈレセ詳細－症状           TO Ｈレセ詳細－症状
                   PERFORM レセ詳細レコードセット
HILO***               DISPLAY "610-3 " Ｈレセ詳細－症状
                   WRITE Ｈレセ詳細－レコード
                   IF 状態キー  NOT =  "00"
                       MOVE NC"Ｈレセ詳細" TO ファイル名
                       PERFORM エラー表示
                   END-IF
               NOT INVALID KEY
                   MOVE Ｈレセ詳細－症状 TO 症状Ｗ
                   MOVE SPACE TO Ｈレセ詳細－レコード
                   INITIALIZE    Ｈレセ詳細－レコード
                   MOVE Ｈレセプト詳細レコードキーＷ TO Ｈレセ詳細－レコードキー
                   MOVE 症状Ｗ                       TO Ｈレセ詳細－症状
      *             IF (Ｈレセ詳細－症状 = SPACE) AND (参Ｈレセ詳細－症状 NOT = SPACE)
      *                 MOVE 参Ｈレセ詳細－症状       TO Ｈレセ詳細－症状
      *             END-IF
                   PERFORM レセ詳細レコードセット
HILO***               DISPLAY "610-4 " Ｈレセ詳細－症状
                   REWRITE Ｈレセ詳細－レコード
                   IF 状態キー  NOT =  "00"
                       MOVE NC"Ｈレセ詳細" TO ファイル名
                       PERFORM エラー表示
                   END-IF
               END-READ
011690     END-IF.
      *
028490*================================================================*
       レセ詳細レコードセット SECTION.
      *
001400*     05 共通欄.
           MOVE 特別地域加算料単価Ｗ             TO Ｈレセ詳細－特別地域加算料単価.
           MOVE 特別地域加算回数Ｗ               TO Ｈレセ詳細－特別地域加算回数  .
      *     MOVE 特別地域加算料Ｗ                 TO Ｈレセ詳細－特別地域加算料    .
           MOVE 累計特別地域加算料Ｗ             TO Ｈレセ詳細－特別地域加算料    .
001490*     05 はり金額欄.
001410     MOVE はりきゅう通所施術料単価１Ｗ     TO Ｈレセ詳細－はりきゅう通所施術料単価１    .
001420     MOVE はりきゅう通所施術料回数１Ｗ     TO Ｈレセ詳細－はりきゅう通所施術料回数１    .
001420     MOVE はりきゅう通所施術料施術料１Ｗ   TO Ｈレセ詳細－はりきゅう通所施術料施術料１  .
001410     MOVE はりきゅう通所施術料単価２Ｗ     TO Ｈレセ詳細－はりきゅう通所施術料単価２    .
001420     MOVE はりきゅう通所施術料回数２Ｗ     TO Ｈレセ詳細－はりきゅう通所施術料回数２    .
001420     MOVE はりきゅう通所施術料施術料２Ｗ   TO Ｈレセ詳細－はりきゅう通所施術料施術料２  .
001410     MOVE はりきゅう訪問施術料１単価１Ｗ   TO Ｈレセ詳細－はりきゅう訪問施術料１単価１  .
001420     MOVE はりきゅう訪問施術料１回数１Ｗ   TO Ｈレセ詳細－はりきゅう訪問施術料１回数１  .
001420     MOVE はりきゅう訪問施術料１施術料１Ｗ TO Ｈレセ詳細－はりきゅう訪問施術料１施術料１.
001410     MOVE はりきゅう訪問施術料１単価２Ｗ   TO Ｈレセ詳細－はりきゅう訪問施術料１単価２  .
001420     MOVE はりきゅう訪問施術料１回数２Ｗ   TO Ｈレセ詳細－はりきゅう訪問施術料１回数２  .
001420     MOVE はりきゅう訪問施術料１施術料２Ｗ TO Ｈレセ詳細－はりきゅう訪問施術料１施術料２.
001410     MOVE はりきゅう訪問施術料２単価１Ｗ   TO Ｈレセ詳細－はりきゅう訪問施術料２単価１  .
001420     MOVE はりきゅう訪問施術料２回数１Ｗ   TO Ｈレセ詳細－はりきゅう訪問施術料２回数１  .
001420     MOVE はりきゅう訪問施術料２施術料１Ｗ TO Ｈレセ詳細－はりきゅう訪問施術料２施術料１.
001410     MOVE はりきゅう訪問施術料２単価２Ｗ   TO Ｈレセ詳細－はりきゅう訪問施術料２単価２  .
001420     MOVE はりきゅう訪問施術料２回数２Ｗ   TO Ｈレセ詳細－はりきゅう訪問施術料２回数２  .
001420     MOVE はりきゅう訪問施術料２施術料２Ｗ TO Ｈレセ詳細－はりきゅう訪問施術料２施術料２.
001410     MOVE はりきゅう訪問施術料３単価１Ｗ   TO Ｈレセ詳細－はりきゅう訪問施術料３単価１  .
001420     MOVE はりきゅう訪問施術料３回数１Ｗ   TO Ｈレセ詳細－はりきゅう訪問施術料３回数１  .
001420     MOVE はりきゅう訪問施術料３施術料１Ｗ TO Ｈレセ詳細－はりきゅう訪問施術料３施術料１.
001410     MOVE はりきゅう訪問施術料３単価２Ｗ   TO Ｈレセ詳細－はりきゅう訪問施術料３単価２  .
001420     MOVE はりきゅう訪問施術料３回数２Ｗ   TO Ｈレセ詳細－はりきゅう訪問施術料３回数２  .
001420     MOVE はりきゅう訪問施術料３施術料２Ｗ TO Ｈレセ詳細－はりきゅう訪問施術料３施術料２.
001410     MOVE はりきゅう訪問施術料４単価１Ｗ   TO Ｈレセ詳細－はりきゅう訪問施術料４単価１  .
001420     MOVE はりきゅう訪問施術料４回数１Ｗ   TO Ｈレセ詳細－はりきゅう訪問施術料４回数１  .
001420     MOVE はりきゅう訪問施術料４施術料１Ｗ TO Ｈレセ詳細－はりきゅう訪問施術料４施術料１.
001410     MOVE はりきゅう訪問施術料４単価２Ｗ   TO Ｈレセ詳細－はりきゅう訪問施術料４単価２  .
001420     MOVE はりきゅう訪問施術料４回数２Ｗ   TO Ｈレセ詳細－はりきゅう訪問施術料４回数２  .
001420     MOVE はりきゅう訪問施術料４施術料２Ｗ TO Ｈレセ詳細－はりきゅう訪問施術料４施術料２.
001420     MOVE はり１術回数Ｗ                   TO Ｈレセ詳細－はり１術回数                  .
001420     MOVE きゅう１術回数Ｗ                 TO Ｈレセ詳細－きゅう１術回数                .
001420     MOVE はりきゅう２術回数Ｗ             TO Ｈレセ詳細－はりきゅう２術回数            .
001880*     05 マッサージ金額欄Ｗ.
001410     MOVE マッサージ通所施術料単価１Ｗ     TO Ｈレセ詳細－マッサージ通所施術料単価１    .
001420     MOVE マッサージ通所施術料回数１Ｗ     TO Ｈレセ詳細－マッサージ通所施術料回数１    .
001420     MOVE マッサージ通所施術料施術料１Ｗ   TO Ｈレセ詳細－マッサージ通所施術料施術料１  .
001410     MOVE マッサージ通所施術料単価２Ｗ     TO Ｈレセ詳細－マッサージ通所施術料単価２    .
001420     MOVE マッサージ通所施術料回数２Ｗ     TO Ｈレセ詳細－マッサージ通所施術料回数２    .
001420     MOVE マッサージ通所施術料施術料２Ｗ   TO Ｈレセ詳細－マッサージ通所施術料施術料２  .
001410     MOVE マッサージ訪問施術料１単価１Ｗ   TO Ｈレセ詳細－マッサージ訪問施術料１単価１  .
001420     MOVE マッサージ訪問施術料１回数１Ｗ   TO Ｈレセ詳細－マッサージ訪問施術料１回数１  .
001420     MOVE マッサージ訪問施術料１施術料１Ｗ TO Ｈレセ詳細－マッサージ訪問施術料１施術料１.
001410     MOVE マッサージ訪問施術料１単価２Ｗ   TO Ｈレセ詳細－マッサージ訪問施術料１単価２  .
001420     MOVE マッサージ訪問施術料１回数２Ｗ   TO Ｈレセ詳細－マッサージ訪問施術料１回数２  .
001420     MOVE マッサージ訪問施術料１施術料２Ｗ TO Ｈレセ詳細－マッサージ訪問施術料１施術料２.
001410     MOVE マッサージ訪問施術料２単価１Ｗ   TO Ｈレセ詳細－マッサージ訪問施術料２単価１  .
001420     MOVE マッサージ訪問施術料２回数１Ｗ   TO Ｈレセ詳細－マッサージ訪問施術料２回数１  .
001420     MOVE マッサージ訪問施術料２施術料１Ｗ TO Ｈレセ詳細－マッサージ訪問施術料２施術料１.
001410     MOVE マッサージ訪問施術料２単価２Ｗ   TO Ｈレセ詳細－マッサージ訪問施術料２単価２  .
001420     MOVE マッサージ訪問施術料２回数２Ｗ   TO Ｈレセ詳細－マッサージ訪問施術料２回数２  .
001420     MOVE マッサージ訪問施術料２施術料２Ｗ TO Ｈレセ詳細－マッサージ訪問施術料２施術料２.
001410     MOVE マッサージ訪問施術料３単価１Ｗ   TO Ｈレセ詳細－マッサージ訪問施術料３単価１  .
001420     MOVE マッサージ訪問施術料３回数１Ｗ   TO Ｈレセ詳細－マッサージ訪問施術料３回数１  .
001420     MOVE マッサージ訪問施術料３施術料１Ｗ TO Ｈレセ詳細－マッサージ訪問施術料３施術料１.
001410     MOVE マッサージ訪問施術料３単価２Ｗ   TO Ｈレセ詳細－マッサージ訪問施術料３単価２  .
001420     MOVE マッサージ訪問施術料３回数２Ｗ   TO Ｈレセ詳細－マッサージ訪問施術料３回数２  .
001420     MOVE マッサージ訪問施術料３施術料２Ｗ TO Ｈレセ詳細－マッサージ訪問施術料３施術料２.
001410     MOVE マッサージ訪問施術料４単価１Ｗ   TO Ｈレセ詳細－マッサージ訪問施術料４単価１  .
001420     MOVE マッサージ訪問施術料４回数１Ｗ   TO Ｈレセ詳細－マッサージ訪問施術料４回数１  .
001420     MOVE マッサージ訪問施術料４施術料１Ｗ TO Ｈレセ詳細－マッサージ訪問施術料４施術料１.
001410     MOVE マッサージ訪問施術料４単価２Ｗ   TO Ｈレセ詳細－マッサージ訪問施術料４単価２  .
001420     MOVE マッサージ訪問施術料４回数２Ｗ   TO Ｈレセ詳細－マッサージ訪問施術料４回数２  .
001420     MOVE マッサージ訪問施術料４施術料２Ｗ TO Ｈレセ詳細－マッサージ訪問施術料４施術料２.
      */20240828
001950     MOVE 変形徒手矯正術単価Ｗ             TO Ｈレセ詳細－変形徒手矯正術単価.
001960     MOVE 変形徒手矯正術回数Ｗ             TO Ｈレセ詳細－変形徒手矯正術回数.
HILO  *     DISPLAY "610-1 " Ｈレセ詳細－変形徒手矯正術単価 " " Ｈレセ詳細－変形徒手矯正術回数.
028500*================================================================*
       作業ファイル１作成 SECTION.
      *
HILO***     DISPLAY "k5-5 " Ｈ連計算４－同一建物患者数取得Ｆ " " Ｈ連計算－患者コード Ｈ連計算－施術区分
HILO***                   " " Ｈ往実－患者コード Ｈ往実－施術区分 *>HILO
           IF Ｈ連計算４－同一建物患者数取得Ｆ = ZERO
      */240902↓↓↓
      *         IF Ｈ連計算－患者コード NOT = Ｈ往実－患者コード
               IF (Ｈ連計算－患者コード = Ｈ往実－患者コード) AND
                  (Ｈ連計算－施術区分   = Ｈ往実－施術区分)
                   CONTINUE
               ELSE
      */240902↑↑↑
HILO***               DISPLAY "610-4-1 同一建物患者 " Ｈ往実－レコードキー
                   INITIALIZE    作１－レコード
                   MOVE SPACE TO 作１－レコード
                   MOVE Ｈ往実－レコードキー TO 作１－レコードキー
005656             WRITE 作１－レコード
005657             IF 状態キー  NOT =  "00"
005658                 MOVE NC"作１"  TO ファイル名
005659                 PERFORM エラー表示
005660             END-IF
005660         END-IF
005660     END-IF.
005661*
      *
      */統一日同一建物患者数のカウントの為に書込み
      *     MOVE Ｈ往実－レコードキー TO 作１－レコードキー.
000390     MOVE Ｈ往実－施術和暦年月日 TO 作２－施術和暦年月日.
000530     MOVE Ｈ往実－患者コード     TO 作２－患者コード.
           READ 作業ファイル２
           INVALID KEY
HILO  *     DISPLAY "K5-8 ▲▲▲▲▲▲▲"
               INITIALIZE    作２－レコード
               MOVE SPACE TO 作２－レコード
000390*         MOVE 作１－施術和暦年月日 TO 作２－施術和暦年月日
000530*         MOVE 作１－患者コード     TO 作２－患者コード
000390         MOVE Ｈ往実－施術和暦年月日 TO 作２－施術和暦年月日
000530         MOVE Ｈ往実－患者コード     TO 作２－患者コード
               WRITE 作２－レコード
005657         IF 状態キー  NOT =  "00"
005658             MOVE NC"作２"  TO ファイル名
005659             PERFORM エラー表示
HILO  *         ELSE DISPLAY "K5-7 " 作２－レコードキー "●●●●●●●●●"
005660         END-IF
           NOT INVALID KEY
HILO  *     DISPLAY "K5-9 ■■■■■■■■"
               CONTINUE
           END-READ.
028500*================================================================*
       作２Ｆカウント SECTION.
      *
           MOVE ZERO  TO 同一建物患者数Ｗ２.
           INITIALIZE    作２－レコード.
           MOVE SPACE TO 作２－レコード.
028050     START 作業ファイル２ KEY IS >= 作２－施術和暦年月日
000190                                    作２－患者コード
028080     END-START.
028090     IF 状態キー = "00"
               MOVE SPACE TO 終了フラグ５
               PERFORM 作業ファイル２読込
               PERFORM UNTIL 終了フラグ５ NOT = SPACE
                   COMPUTE 同一建物患者数Ｗ２ = 同一建物患者数Ｗ２ + 1
HILO***            IF Ｈ連計算４－同一建物患者数取得Ｆ = ZERO
HILO***            DISPLAY "k5-1 " 同一建物患者数Ｗ２ " " 作２－患者コード 作２－施術和暦年月日 END-IF
                   PERFORM 作業ファイル２読込
               END-PERFORM
           END-IF.
028500*================================================================*
       作業ファイル２読込 SECTION.
024370*
024380     READ 作業ファイル２ NEXT
024390     AT END
024400         MOVE "YES"  TO 終了フラグ５
024410     END-READ.
024420*
028500*================================================================*
      * レセ詳細前月読込 SECTION.
      **
      *     MOVE Ｈレセ詳細－レコードキー TO 参Ｈレセ詳細－レコードキー.
      *     IF Ｈレセ詳細－施術月 = 1
      *         MOVE 12 TO 参Ｈレセ詳細－施術月
      *         COMPUTE 参Ｈレセ詳細－施術年 = Ｈレセ詳細－施術年 - 1
      *     ELSE
      *         MOVE Ｈレセ詳細－施術年 TO 参Ｈレセ詳細－施術年
      *         COMPUTE 参Ｈレセ詳細－施術月 = Ｈレセ詳細－施術月 - 1
      *     END-IF
018340*     MOVE Ｈレセ詳細－施術和暦 TO 元－元号区分 参Ｈレセ詳細－施術和暦
018350*     READ 元号マスタ
018360*     NOT INVALID KEY
      *         IF 元－正式開始和暦年月 > Ｈレセ詳細－施術和暦年月
      *             COMPUTE 元－元号区分 = 元－元号区分 - 1
018350*             READ 元号マスタ
018360*             NOT INVALID KEY
      *                 MOVE 元－正式終了和暦年月 TO 参Ｈレセ詳細－施術和暦年月
      *             END-READ
      *         END-IF
      *     END-READ
HILO  **     DISPLAY "610-0-1  " 参Ｈレセ詳細－施術和暦年月
027530*     READ 参照Ｈレセプト詳細Ｆ
027540*     INVALID KEY
      *         MOVE SPACE TO 参Ｈレセ詳細－レコード
027590*     END-READ
028500*================================================================*
       参照受診者Ｆ取得 SECTION.
      *
027730     OPEN INPUT 参照受診者情報Ｆ.
027740         MOVE NC"参照受診者" TO ファイル名.
027750         PERFORM オープンチェック.
000390     MOVE Ｈ往実－施術和暦年月 TO 参照受－施術和暦年月
000460     MOVE Ｈ往実－患者コード   TO 参照受－患者コード    
027840     READ 参照受診者情報Ｆ
027850     INVALID KEY
027850         MOVE SPACE TO 参照受－レコード
HILO  *     NOT INVALID KEY DISPLAY "610-2 " 参照受－保険種別
           END-READ.
028300     CLOSE 参照受診者情報Ｆ.
028500*================================================================*
028510******************************************************************
028520 END PROGRAM K50610.
028530******************************************************************
