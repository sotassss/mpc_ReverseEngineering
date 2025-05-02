000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YHN662.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*         大阪府柔道連盟用    カルテ印刷　通常用紙（柔+ｳｨﾝﾄﾞｳｽﾞ版）
000100*         MED = YAW660 YHN662P YHN6622P
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2012-10-29
000130 DATE-COMPILED.          2012-10-29
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
000260     SELECT  保険者マスタ    ASSIGN      TO        HOKENSL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY               IS  保－保険種別
000300                                                          保－保険者番号
000310* 将来は、キー項目の保険者名称を保険者カナにする
000320                             ALTERNATE RECORD KEY     IS  保－保険種別
000330                                                          保－保険者名称
000340                                                          保－保険者番号
000350                             FILE STATUS              IS  状態キー
000360                             LOCK        MODE         IS  AUTOMATIC.
000370     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000380                             ORGANIZATION             IS  INDEXED
000390                             ACCESS MODE              IS  DYNAMIC
000400                             RECORD KEY               IS  元－元号区分
000410                             FILE STATUS              IS  状態キー
000420                             LOCK        MODE         IS  AUTOMATIC.
000430     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000440                             ORGANIZATION             IS  INDEXED
000450                             ACCESS MODE              IS  DYNAMIC
000460                             RECORD KEY               IS  名－区分コード
000470                                                          名－名称コード
000480                             FILE STATUS              IS  状態キー
000490                             LOCK        MODE         IS  AUTOMATIC.
000500     SELECT  レセプトＦ      ASSIGN      TO        RECEPTL
000510                             ORGANIZATION             IS  INDEXED
000520                             ACCESS MODE              IS  DYNAMIC
000530                             RECORD KEY               IS  レセ－施術和暦年月
000540                                                          レセ－患者コード
000550                                                          レセ－レセ種別
000560                             ALTERNATE RECORD KEY     IS  レセ－患者コード
000570                                                          レセ－施術和暦年月
000580                                                          レセ－レセ種別
000590                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
000600                                                          レセ－施術和暦年月
000610                                                          レセ－患者コード
000620                                                          レセ－レセ種別
000630                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
000640                                                          レセ－レセ種別
000650                                                          レセ－請求保険者番号
000660                                                          レセ－患者コード
000670                                                          レセ－施術和暦年月
000680                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
000690                                                          レセ－請求保険者番号
000700                                                          レセ－患者コード
000710                                                          レセ－レセ種別
000720                                                          レセ－施術和暦年月
000730                             FILE STATUS              IS  状態キー
000740                             LOCK        MODE         IS  AUTOMATIC.
000750     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000760                             ORGANIZATION             IS  INDEXED
000770                             ACCESS MODE              IS  DYNAMIC
000780                             RECORD KEY               IS  制－制御区分
000790                             FILE STATUS              IS  状態キー
000800                             LOCK        MODE         IS  AUTOMATIC.
000810     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000820                             ORGANIZATION             IS  INDEXED
000830                             ACCESS MODE              IS  DYNAMIC
000840                             RECORD KEY               IS  受－施術和暦年月
000850                                                          受－患者コード
000860                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000870                                                          受－患者カナ
000880                                                          受－患者コード
000890                             ALTERNATE RECORD KEY     IS  受－患者コード
000900                                                          受－施術和暦年月
000910                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000920                                                          受－保険種別
000930                                                          受－保険者番号
000940                                                          受－患者コード
000950                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000960                                                          受－公費種別
000970                                                          受－費用負担者番号
000980                                                          受－患者コード
000990                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
001000                                                          受－助成種別
001010                                                          受－費用負担者番号助成
001020                                                          受－患者コード
001030                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
001040                                                          受－施術和暦年月
001050                                                          受－患者コード
001060                             FILE STATUS              IS  状態キー
001070                             LOCK        MODE         IS  AUTOMATIC.
001080     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
001090                             ORGANIZATION             IS  INDEXED
001100                             ACCESS MODE              IS  DYNAMIC
001110                             RECORD KEY               IS  施記－施術和暦年月日
001120                                                          施記－患者コード
001130                             ALTERNATE RECORD KEY     IS  施記－患者コード
001140                                                          施記－施術和暦年月日
001150                             FILE STATUS              IS  状態キー
001160                             LOCK        MODE         IS  AUTOMATIC.
001170     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
001180                             ORGANIZATION             IS  INDEXED
001190                             ACCESS MODE              IS  DYNAMIC
001200                             RECORD KEY               IS 負－施術和暦年月
001210                                                         負－患者コード
001220                             ALTERNATE RECORD KEY     IS 負－患者コード
001230                                                         負－施術和暦年月
001240                             FILE STATUS              IS  状態キー
001250                             LOCK        MODE         IS  AUTOMATIC.
001260     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
001270                             ORGANIZATION             IS  INDEXED
001280                             ACCESS MODE              IS  DYNAMIC
001290                             RECORD KEY               IS  負原－区分コード
001300                                                          負原－負傷原因コード
001310                             FILE STATUS              IS  状態キー
001320                             LOCK        MODE         IS  AUTOMATIC.
001330     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
001340                             ORGANIZATION             IS  INDEXED
001350                             ACCESS MODE              IS  DYNAMIC
001360                             RECORD KEY               IS  市－公費種別
001370                                                          市－市町村番号
001380                             ALTERNATE RECORD KEY     IS  市－公費種別
001390                                                          市－市町村名称
001400                                                          市－市町村番号
001410                             FILE STATUS              IS  状態キー
001420                             LOCK        MODE         IS  AUTOMATIC.
001430     SELECT  事業所マスタ    ASSIGN      TO        JIGYOSL
001440                             ORGANIZATION             IS  INDEXED
001450                             ACCESS MODE              IS  DYNAMIC
001460                             RECORD KEY               IS  事－保険種別
001470                                                          事－保険者番号
001480                                                          事－記号
001490                             FILE STATUS              IS  状態キー
001500                             LOCK        MODE         IS  AUTOMATIC.
000241     SELECT  生保情報Ｆ      ASSIGN      TO        SEIHOJL
000242                             ORGANIZATION             IS INDEXED
000243                             ACCESS MODE              IS DYNAMIC
000244                             RECORD KEY               IS 生保－施術和暦年月
000245                                                         生保－患者コード
000255                             ALTERNATE RECORD KEY     IS 生保－患者コード
000265                                                         生保－施術和暦年月
000277                             FILE STATUS              IS 状態キー
000278                             LOCK        MODE         IS AUTOMATIC.
001510     SELECT  労災情報Ｆ      ASSIGN      TO        ROUSAIJL
001520                             ORGANIZATION             IS INDEXED
001530                             ACCESS MODE              IS DYNAMIC
001540                             RECORD KEY               IS 労災－施術和暦年月
001550                                                         労災－患者コード
001560                             ALTERNATE RECORD KEY     IS 労災－患者コード
001570                                                         労災－施術和暦年月
001580                             FILE STATUS              IS 状態キー
001590                             LOCK        MODE         IS AUTOMATIC.
000241     SELECT  自賠責情報Ｆ    ASSIGN      TO        JIBAIJL
000242                             ORGANIZATION             IS INDEXED
000243                             ACCESS MODE              IS DYNAMIC
000244                             RECORD KEY               IS 自賠－施術和暦年月
000245                                                         自賠－患者コード
000277                             ALTERNATE RECORD KEY     IS 自賠－患者コード
000278                                                         自賠－施術和暦年月
000279                             FILE STATUS              IS 状態キー
000280                             LOCK        MODE         IS AUTOMATIC.
000102     SELECT  保険会社マスタ  ASSIGN      TO           HOKCOML
000103                             ORGANIZATION             IS  INDEXED
000104                             ACCESS MODE              IS  DYNAMIC
000105                             RECORD KEY               IS  保険会－保険会社番号
000108                             ALTERNATE RECORD KEY     IS  保険会－保険会社カナ
000110                                                          保険会－保険会社番号
000112                             FILE STATUS              IS  状態キー
000113                             LOCK        MODE         IS  AUTOMATIC.
000530     SELECT  作業ファイル１  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W7211L.DAT"
000540                             ORGANIZATION             IS  INDEXED
000550                             ACCESS                   IS  DYNAMIC
000560                             RECORD      KEY          IS  作１－施術和暦年月日
000610                                                          作１－患者コード
000910                             ALTERNATE RECORD KEY     IS  作１－施術和暦年月日
                                                                作１－患者カナ
                                                                作１－患者コード
000910                             ALTERNATE RECORD KEY     IS  作１－患者コード
                                                                作１－施術和暦年月日
000910                             ALTERNATE RECORD KEY     IS  作１－患者カナ
                                                                作１－患者コード
                                                                作１－施術和暦年月日
000620                             FILE        STATUS       IS  状態キー
000630                             LOCK        MODE         IS  AUTOMATIC.
000980     SELECT  作業ファイル２  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W7212L.DAT"
000990                             ORGANIZATION             IS  INDEXED
001000                             ACCESS                   IS  DYNAMIC
001010                             RECORD      KEY          IS  作２－施術和暦年月
001020                                                          作２－患者コード
001030                             FILE        STATUS       IS  状態キー
001040                             LOCK        MODE         IS  AUTOMATIC.
001600     SELECT  印刷ファイル    ASSIGN      TO     GS-PRTF002
001610                             SYMBOLIC    DESTINATION  IS "PRT"
001620                             FORMAT                   IS  定義体名Ｐ
001630                             GROUP                    IS  項目群名Ｐ
001640                             PROCESSING  MODE         IS  処理種別Ｐ
001650                             UNIT        CONTROL      IS  拡張制御Ｐ
001660                             FILE        STATUS       IS  通知情報Ｐ.
001740******************************************************************
001750*                      DATA DIVISION                             *
001760******************************************************************
001770 DATA                    DIVISION.
001780 FILE                    SECTION.
001790*                           ［ＲＬ＝  ３２０］
001800 FD  保険者マスタ        BLOCK   CONTAINS   1   RECORDS.
001810     COPY HOKENS          OF  XFDLIB  JOINING   保   AS  PREFIX.
001820*                           ［ＲＬ＝  １２８］
001830 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001840     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001850*                           ［ＲＬ＝  １２８］
001860 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
001870     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
001880*                          ［ＲＬ＝  １５３６］
001890 FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
001900     COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
001910*                           ［ＲＬ＝  ２５６］
001920 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001930     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
001940*                           ［ＲＬ＝  ３２０］
001950 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
001960     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
001970*                           ［ＲＬ＝  ２５６］
001980 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
001990     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
002000*                           ［ＲＬ＝  １２８］
002010 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002020     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
002030*                           ［ＲＬ＝  １２８］
002040 FD  負傷原因Ｆ          BLOCK   CONTAINS   1   RECORDS.
002050     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
002060*                           ［ＲＬ＝  ２５６］
002070 FD  市町村マスタ        BLOCK   CONTAINS   1   RECORDS.
002080     COPY SITYOSN         OF  XFDLIB  JOINING   市   AS  PREFIX.
002090*
002100 FD  事業所マスタ        BLOCK   CONTAINS   1   RECORDS GLOBAL.
002110     COPY JIGYOS          OF  XFDLIB  JOINING   事   AS  PREFIX.
001080* 
000280 FD  生保情報Ｆ          BLOCK   CONTAINS   1   RECORDS.
000281     COPY SEIHOJ          OF  XFDLIB  JOINING   生保   AS  PREFIX.
002120*
002130 FD  労災情報Ｆ          BLOCK   CONTAINS   1   RECORDS.
002140     COPY ROUSAIJ         OF  XFDLIB  JOINING   労災   AS  PREFIX.
000284*
000282 FD  自賠責情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
000283     COPY JIBAIJ      OF  XFDLIB  JOINING   自賠   AS  PREFIX.
000114*
000115 FD  保険会社マスタ   BLOCK   CONTAINS   1   RECORDS.
000116     COPY HOKENCOM    OF  XFDLIB  JOINING   保険会   AS  PREFIX.
002150*
001250*****************
001260* 作業ファイル１ *
001270*****************
001280*                         ［ＲＬ＝  １６０］
001290 FD  作業ファイル１ RECORD  CONTAINS 160 CHARACTERS.
001300 01 作１－レコード.
001310    03 作１－レコードキー.
001320       05 作１－施術和暦年月日.
001330          07 作１－施術和暦               PIC 9.
001340          07 作１－施術年月.
001350             09 作１－施術年              PIC 9(2).
001360             09 作１－施術月              PIC 9(2).
001370          07 作１－施術日                 PIC 9(2).
001380       05 作１－患者コード.
001390          07 作１－患者番号                PIC 9(6).
001400          07 作１－枝番                    PIC X(1).
001410    03 作１－レコードデータ.
001420       05 作１－患者カナ                   PIC X(50).
001430       05 作１－患者氏名                   PIC X(50).
001440       05 作１－料金.
001450          07 作１－初検金額                PIC 9(5).
001460          07 作１－整復金額                PIC 9(5).
001470          07 作１－後療金額                PIC 9(5).
001480          07 作１－罨法金額                PIC 9(5).
001490          07 作１－電療金額                PIC 9(5).
001500          07 作１－費用額                  PIC 9(5).
001510          07 作１－一部負担金              PIC 9(5).
001520       05 FILLER                           PIC X(11).
001530*
001540*                         ［ＲＬ＝  １２８］
001550 FD  作業ファイル２ RECORD  CONTAINS 128 CHARACTERS.
001560 01 作２－レコード.
001570    03 作２－レコードキー.
001580       05 作２－施術和暦年月.
001590          07 作２－施術和暦                PIC 9.
001600          07 作２－施術年月.
001610             09 作２－施術年               PIC 9(2).
001620             09 作２－施術月               PIC 9(2).
001630       05 作２－患者コード.
001640          07 作２－患者番号                PIC 9(6).
001650          07 作２－枝番                    PIC X(1).
001660    03 作２－レコードデータ.
001670       05 作２－最終通院日.
001680          07 作２－通院月                  PIC 9(2).
001690          07 作２－通院日                  PIC 9(2).
001700       05 作２－施術回数                   PIC 9(2).
001710       05 作２－費用額                     PIC 9(6).
001720       05 作２－請求額                     PIC 9(6).
001730       05 作２－負担金                     PIC 9(5).
001740       05 作２－転帰                       OCCURS 5.
001750          07 作２－転帰区分                PIC N(1).
001760       05 作２－初検計                     PIC 9(5).
001770       05 作２－整復計                     PIC 9(5).
001780       05 作２－後療計                     PIC 9(5).
001790       05 作２－罨法計                     PIC 9(5).
001800       05 作２－電療計                     PIC 9(5).
001810       05 作２－費用計                     PIC 9(5).
001820       05 作２－負担計                     PIC 9(5).
001830       05 FILLER                           PIC X(48).
001840*
002160 FD  印刷ファイル.
002170     COPY YHN662P        OF  XMDLIB.
002210*----------------------------------------------------------------*
002220******************************************************************
002230*                WORKING-STORAGE SECTION                         *
002240******************************************************************
002250 WORKING-STORAGE         SECTION.
002260 01 キー入力                           PIC X     VALUE SPACE.
002270 01 状態キー                           PIC X(2)  VALUE SPACE.
002280 01 終了フラグ                         PIC X(3)  VALUE SPACE.
002290 01 終了フラグ２                       PIC X(3)  VALUE SPACE.
002300 01 詳細ＣＮＴ                         PIC 9(2)  VALUE ZERO.
002310 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002320 01 文字ＣＮＴ                         PIC 9(2)  VALUE ZERO.
002330 01 初検フラグ                         PIC X(3)  VALUE SPACE.
002340 01 ファイル名                         PIC N(2)  VALUE SPACE.
002350 01 前和暦Ｗ                           PIC 9     VALUE ZERO.
002360 01 カレント元号Ｗ                     PIC 9(1)  VALUE ZERO.
002370 01 全角空白                           PIC X(2)  VALUE X"8140".
002380 01 半角空白                           PIC X(2)  VALUE X"2020".
001210 01 オープンフラグ                     PIC X(3)   VALUE SPACE.
002390 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002400 01 部位名称Ｗ                         PIC N(10) VALUE SPACE.
002410 01 負傷番号Ｗ                         PIC 9.
002420 01 負傷番号Ｒ REDEFINES 負傷番号Ｗ.
002430    03 負傷番号Ｗ１                    PIC X.
002440*
002450 01 全角負傷番号Ｗ                     PIC N.
002460 01 全角負傷番号Ｒ REDEFINES 全角負傷番号Ｗ.
002470    03 全角負傷番号Ｗ１                PIC X(2).
002480*
002490 01 負担率Ｗ                           PIC 9(3) VALUE ZERO.
002500 01 甲乙区分Ｗ                         PIC 9    VALUE ZERO.
002510 01 本人負担率Ｗ                       PIC 9(3) VALUE ZERO.
002520 01 家族負担率Ｗ                       PIC 9(3) VALUE ZERO.
002530*
002540****************
002550* 詳細用ワーク *
002560****************
002570* 連印－負傷印刷行用ワーク
002580 01 印刷行ＳＷ                           PIC 9(2) VALUE ZERO.
002590 01 印刷行ＳＷＰ                         PIC 9(2) VALUE ZERO.
002600 01 負傷情報ＳＷ.
002610    03 負傷データＳＷ  OCCURS   9.
002620       05 会部位コードＳＷ               PIC X(4)  VALUE SPACE.
002630       05 部位ＣＮＴＳＷ                 PIC 9(1)  VALUE ZERO.
002640       05 負傷種別ＳＷ                   PIC 9(2)  VALUE ZERO.
002650       05 部位ＳＷ                       PIC 9(2)  VALUE ZERO.
002660       05 左右区分ＳＷ                   PIC 9(1)  VALUE ZERO.
002670       05 負傷位置番号ＳＷ               PIC 9(2)  VALUE ZERO.
002680       05 負傷名ＳＷ                     PIC N(18) VALUE SPACE.
002690       05 負傷年月日ＳＷ.
002700          07 負傷和暦ＳＷ                PIC 9(1)  VALUE ZERO.
002710          07 負傷年ＳＷ                  PIC 9(2)  VALUE ZERO.
002720          07 負傷月ＳＷ                  PIC 9(2)  VALUE ZERO.
002730          07 負傷日ＳＷ                  PIC 9(2)  VALUE ZERO.
002740          07 負傷年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
002750       05 施術開始年月日ＳＷ.
002760          07 施術開始和暦ＳＷ            PIC 9(1)  VALUE ZERO.
002770          07 施術開始年ＳＷ              PIC 9(2)  VALUE ZERO.
002780          07 施術開始月ＳＷ              PIC 9(2)  VALUE ZERO.
002790          07 施術開始日ＳＷ              PIC 9(2)  VALUE ZERO.
002800          07 施開年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
002810       05 施術終了年月日ＳＷ.
002820          07 施術終了和暦ＳＷ            PIC 9(1)  VALUE ZERO.
002830          07 施術終了年ＳＷ              PIC 9(2)  VALUE ZERO.
002840          07 施術終了月ＳＷ              PIC 9(2)  VALUE ZERO.
002850          07 施術終了日ＳＷ              PIC 9(2)  VALUE ZERO.
002860          07 施終年月日区切ＳＷ          PIC X(1)  VALUE SPACE.
002870       05 開始年月日取得フラグＳ         PIC X(3)  VALUE SPACE.
002880       05 転帰ＳＷ                       PIC N(4)  VALUE SPACE.
002890       05 転帰チェックＳＷ.
002900          07 治癒チェックＳＷ            PIC N(1)  VALUE SPACE.
002910          07 中止チェックＳＷ            PIC N(1)  VALUE SPACE.
002920          07 転医チェックＳＷ            PIC N(1)  VALUE SPACE.
002930       05 施術累計回数ＳＷ               PIC 9(4)  VALUE ZERO.
002940 01 負傷原因情報ＳＷ.
002950    03 負傷原因コードＳＷ  OCCURS   9.
002960       09 負傷患者番号ＳＷ               PIC 9(6) VALUE ZERO.
002970       09 負傷連番ＳＷ                   PIC 9(4) VALUE ZERO.
002980*
002990 01 施術和暦年月日ＣＷ.
003000   03 施術和暦年月ＣＷ.
003010     05 施術和暦ＣＷ                   PIC 9 VALUE ZERO.
003020     05 施術年月ＣＷ.
003030        07 施術年ＣＷ                  PIC 9(2) VALUE ZERO.
003040        07 施術月ＣＷ                  PIC 9(2) VALUE ZERO.
003050   03 施術日ＣＷ                       PIC 9(2) VALUE ZERO.
003060 01 施術西暦年Ｗ                       PIC 9(4) VALUE ZERO.
003070 01 患者西暦年Ｗ                       PIC 9(4) VALUE ZERO.
003080*
003090** 負傷原因用
003100*
003110 01 カウンタ                           PIC 9(2)  VALUE ZERO.
003120 01 カウンタ２                         PIC 9(2)  VALUE ZERO.
      */症状処置経過欄用(１桁)
       01 カウンタ３                         PIC 9(1)  VALUE ZERO.
003130*
003140 01 負傷原因ＷＴ.
003150    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
003160    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
003170    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
003180    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
003190    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
003200    03 負傷原因ナンバーＷＴ.
003210       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
003220    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
003230 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
003240 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
003250 01 負傷原因ＴＢＬ.
003260    03 負傷原因コードＴＢＬ            OCCURS 9.
003270       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
003280       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
003290       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
003300 01 負傷原因内容Ｗ.
003310    03 負傷原因内容合成Ｗ              PIC X(316) OCCURS 9 VALUE SPACE.
003320    03 負傷原因内容分解ＸＷ.
003330       05 負傷原因内容１ＸＷ           PIC X(72)  VALUE SPACE.
003340       05 負傷原因内容２ＸＷ           PIC X(72)  VALUE SPACE.
003350       05 負傷原因内容３ＸＷ           PIC X(72)  VALUE SPACE.
003360       05 負傷原因内容４ＸＷ           PIC X(72)  VALUE SPACE.
003370       05 負傷原因内容５ＸＷ           PIC X(28)  VALUE SPACE.
003380*
003390*/負傷原因の詳細印刷/0610
003400 01 負傷原因Ｗテーブル２.
003410    03 負傷原因Ｗ２                    PIC X(72) OCCURS 45 VALUE SPACE.
003420** 負傷原因印刷区分用
003430 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
003440*
003450 01 負傷原因情報Ｗ.
003460    03 負傷原因コードＷ  OCCURS   9.
003470       09 負傷原因患者番号Ｗ           PIC 9(6) VALUE ZERO.
003480       09 負傷原因連番Ｗ               PIC 9(4) VALUE ZERO.
003490****************
003500* 連結項目待避 *
003510****************
003520*    ************
003530*    * 印刷キー *
003540*    ************
003550 01 対象データＷＲ.
003560    03 施術和暦年月ＷＲ.
003570       05 施術和暦ＷＲ                  PIC 9(1)  VALUE ZERO.
003580       05 施術年ＷＲ                    PIC 9(2)  VALUE ZERO.
003590       05 施術月ＷＲ                    PIC 9(2)  VALUE ZERO.
003600    03 保険種別ＷＲ                     PIC 9(2)  VALUE ZERO.
003610    03 保険者番号ＷＲ                   PIC X(10) VALUE SPACE.
003620    03 本人家族区分ＷＲ                 PIC 9(1)  VALUE ZERO.
003630    03 被保険者カナＷＲ                 PIC X(20) VALUE SPACE.
003640    03 患者コードＷＲ.
003650       05 患者番号ＷＲ                  PIC 9(6)  VALUE ZERO.
003660       05 枝番ＷＲ                      PIC X(1)  VALUE SPACE.
003670    03 印刷モードＦＷＲ                 PIC 9(1)  VALUE ZERO.
003680    03 提出年月日ＷＲ.
003690       05 提出年ＷＲ                    PIC 9(2)  VALUE ZERO.
003700       05 提出月ＷＲ                    PIC 9(2)  VALUE ZERO.
003710       05 提出日ＷＲ                    PIC 9(2)  VALUE ZERO.
003720    03 段数ＷＲ                         PIC 9(1)  VALUE ZERO.
          03 日段数ＷＲ                       PIC 9(2) VALUE ZERO.
001630    03 印字位置ＣＮＴ                   PIC 9(2)  VALUE ZERO.
003730************
003740* 料金情報 *
003750************
003760*
003770 01 印刷負担率Ｗ                        PIC 9(1) VALUE ZERO.
003780**************
003790* 受診者情報 *
003800**************
003810 01 受診者情報Ｗ.
003820    03 患者コードＷ.
003830       05 患者番号Ｗ                   PIC 9(6) VALUE ZERO.
003840       05 枝番Ｗ                       PIC X(1) VALUE SPACE.
003850    03 市町村番号Ｗ.
003860*       05 印刷市町村番号Ｗ             PIC X(8) VALUE SPACE.
003870       05 印刷市町村番号Ｗ.
003880         07 市町村番号１Ｗ             PIC X(1) VALUE SPACE.
003890         07 市町村番号２Ｗ             PIC X(1) VALUE SPACE.
003900         07 市町村番号３Ｗ             PIC X(1) VALUE SPACE.
003910         07 市町村番号４Ｗ             PIC X(1) VALUE SPACE.
003920         07 市町村番号５Ｗ             PIC X(1) VALUE SPACE.
003930         07 市町村番号６Ｗ             PIC X(1) VALUE SPACE.
003940         07 市町村番号７Ｗ             PIC X(1) VALUE SPACE.
003950         07 市町村番号８Ｗ             PIC X(1) VALUE SPACE.
003960       05 FILLER                       PIC X(2) VALUE SPACE.
003970    03 受益者番号Ｗ.
003980*       05 印刷受益者番号Ｗ             PIC X(7) VALUE SPACE.
003990       05 印刷受益者番号Ｗ.
004000         07 受益者番号１Ｗ             PIC X(1) VALUE SPACE.
004010         07 受益者番号２Ｗ             PIC X(1) VALUE SPACE.
004020         07 受益者番号３Ｗ             PIC X(1) VALUE SPACE.
004030         07 受益者番号４Ｗ             PIC X(1) VALUE SPACE.
004040         07 受益者番号５Ｗ             PIC X(1) VALUE SPACE.
004050         07 受益者番号６Ｗ             PIC X(1) VALUE SPACE.
004060         07 受益者番号７Ｗ             PIC X(1) VALUE SPACE.
004070       03 FILLER                       PIC X(13) VALUE SPACE.
004080    03 記号Ｗ                          PIC X(24) VALUE SPACE.
004090    03 番号Ｗ.
004100       05 印刷番号Ｗ                   PIC X(12) VALUE SPACE.
004110       05 FILLER                       PIC X(18) VALUE SPACE.
004120    03 被保険者情報Ｗ.
004130       05 被保険者カナＷ               PIC X(50) VALUE SPACE.
004140       05 被保険者氏名Ｗ               PIC X(50) VALUE SPACE.
004150*
004160       05 被保険者和暦チェックＷ.
004170          07 被保険者明治Ｗ            PIC N(1) VALUE SPACE.
004180          07 被保険者大正Ｗ            PIC N(1) VALUE SPACE.
004190          07 被保険者昭和Ｗ            PIC N(1) VALUE SPACE.
004200          07 被保険者平成Ｗ            PIC N(1) VALUE SPACE.
003940          07 被保険者令和Ｗ            PIC N(1) VALUE SPACE.
004210       05 性別チェックＷ.
004220          07 男チェックＷ              PIC N(1) VALUE SPACE.
004230          07 女チェックＷ              PIC N(1) VALUE SPACE.
004240       05 被保険者性別Ｗ               PIC 9.
004250       05 被保険者生年月日Ｗ.
004260          07 被保険者和暦Ｗ            PIC 9 VALUE ZERO.
004270          07 被保険者年Ｗ              PIC 9(2) VALUE ZERO.
004280          07 被保険者月Ｗ              PIC 9(2) VALUE ZERO.
004290          07 被保険者日Ｗ              PIC 9(2) VALUE ZERO.
004300*
004310       05 郵便番号Ｗ.
004320          07 郵便番号１Ｗ              PIC X(3) VALUE SPACE.
004330          07 郵便番号２Ｗ              PIC X(4) VALUE SPACE.
004340       05 被保険者住所Ｗ.
004350          07 印刷住所１Ｗ               PIC X(50) VALUE SPACE.
004360          07 印刷住所２Ｗ               PIC X(50) VALUE SPACE.
004370          07 FILLER                     PIC X(18).
004380       05 電話番号Ｗ                   PIC X(15) VALUE SPACE.
004390       05 分割電話番号Ｗ.
004400          07 分割電話番号１Ｗ           PIC X(5) VALUE SPACE.
004410          07 分割電話番号２Ｗ           PIC X(5) VALUE SPACE.
004420          07 分割電話番号３Ｗ           PIC X(5) VALUE SPACE.
004430    03 資格取得年月日Ｗ.
004440       05 資格取得元号Ｗ               PIC N(2) VALUE SPACE.
004450       05 資格取得和暦Ｗ               PIC 9(1) VALUE ZERO.
004460       05 資格取得年Ｗ                 PIC 9(2) VALUE ZERO.
004470       05 資格取得月Ｗ                 PIC 9(2) VALUE ZERO.
004480       05 資格取得日Ｗ                 PIC 9(2) VALUE ZERO.
004490       05 資格和暦チェックＷ.
004500          07 資格昭和チェックＷ        PIC N(1) VALUE SPACE.
004510          07 資格平成チェックＷ        PIC N(1) VALUE SPACE.
004510          07 資格令和チェックＷ        PIC N(1) VALUE SPACE.
004520    03 有効年月日Ｗ.
004530       05 有効和暦Ｗ                   PIC 9(1) VALUE ZERO.
004540       05 有効年Ｗ                     PIC 9(2) VALUE ZERO.
004550       05 有効月Ｗ                     PIC 9(2) VALUE ZERO.
004560       05 有効日Ｗ                     PIC 9(2) VALUE ZERO.
004570    03 患者情報Ｗ.
004580       05 患者カナＷ                   PIC X(50) VALUE SPACE.
004590       05 患者氏名Ｗ                   PIC X(50) VALUE SPACE.
004600       05 続柄Ｗ.
004610          07 印刷続柄Ｗ                PIC N(4) VALUE SPACE.
004620          07 FILLER                    PIC X(4) VALUE SPACE.
004630       05 本人チェックＷ               PIC N(1) VALUE SPACE.
004640       05 家族チェックＷ               PIC N(1) VALUE SPACE.
004650       05 患者元号Ｗ                   PIC N(2) VALUE SPACE.
004660       05 和暦チェックＷ.
004670          07 明治チェックＷ            PIC N(1) VALUE SPACE.
004680          07 大正チェックＷ            PIC N(1) VALUE SPACE.
004690          07 昭和チェックＷ            PIC N(1) VALUE SPACE.
004700          07 平成チェックＷ            PIC N(1) VALUE SPACE.
004280          07 令和チェックＷ            PIC N(1)  VALUE SPACE.
004710       05 患者年Ｗ                     PIC 9(2) VALUE ZERO.
004720       05 患者月Ｗ                     PIC 9(2) VALUE ZERO.
004730       05 患者日Ｗ                     PIC 9(2) VALUE ZERO.
004740       05 患者年齢Ｗ                   PIC 9(3) VALUE ZERO.
004750       05 患者性別Ｗ                   PIC N(4) VALUE SPACE.
004760       05 性別チェックＷ.
004770          07 患者男チェックＷ          PIC N(1) VALUE SPACE.
004780          07 患者女チェックＷ          PIC N(1) VALUE SPACE.
004790       05 患者郵便番号Ｗ.
004800          07 患者郵便番号１Ｗ          PIC X(3) VALUE SPACE.
004810          07 患者郵便番号２Ｗ          PIC X(4) VALUE SPACE.
004820       05 患者住所Ｗ.
004830          07 患者住所１Ｗ              PIC X(50) VALUE SPACE.
004840          07 患者住所２Ｗ              PIC X(50) VALUE SPACE.
004850          07 FILLER                    PIC X(12).
004860       05 患者電話番号Ｗ               PIC X(19) VALUE SPACE.
004870    03 保険種別チェックＷ.
004880       05 社保チェックＷ               PIC N(1) VALUE SPACE.
004890       05 組合チェックＷ               PIC N(1) VALUE SPACE.
004900       05 日雇チェックＷ               PIC N(1) VALUE SPACE.
004910       05 船員チェックＷ               PIC N(1) VALUE SPACE.
004920       05 共済チェックＷ               PIC N(1) VALUE SPACE.
004930       05 国保チェックＷ               PIC N(1) VALUE SPACE.
004940       05 退本チェックＷ               PIC N(1) VALUE SPACE.
004950       05 退家チェックＷ               PIC N(1) VALUE SPACE.
004960       05 老チェックＷ                 PIC N(1) VALUE SPACE.
004970       05 障チェックＷ                 PIC N(1) VALUE SPACE.
004980       05 母チェックＷ                 PIC N(1) VALUE SPACE.
004990       05 乳チェックＷ                 PIC N(1) VALUE SPACE.
005000       05 原チェックＷ                 PIC N(1) VALUE SPACE.
005010       05 ０割チェックＷ               PIC N(1) VALUE SPACE.
005020       05 １割チェックＷ               PIC N(1) VALUE SPACE.
005030       05 ２割チェックＷ               PIC N(1) VALUE SPACE.
005040       05 ３割チェックＷ               PIC N(1) VALUE SPACE.
005050       05 証チェックＷ                 PIC N(1) VALUE SPACE.
005060    03 負傷原因Ｗ                      PIC X(72) OCCURS 45 VALUE SPACE.
005070**************
005080* 事業所情報 *
005090**************
005100 01 事業所情報Ｗ.
005110*    03 事業所名称Ｗ                    PIC X(35) VALUE SPACE.
005120    03 事業所名称Ｗ.
005130       05 印刷事業所名称Ｗ             PIC X(60) VALUE SPACE.
005140       05 FILLER                       PIC X(5) VALUE SPACE.
005150    03 事業所郵便番号Ｗ.
005160       05 事業所郵便番号１Ｗ           PIC X(3) VALUE SPACE.
005170       05 事業所郵便番号２Ｗ           PIC X(4) VALUE SPACE.
005180    03 事業所住所Ｗ                    PIC X(50) VALUE SPACE.
005180    03 事業所住所２Ｗ                  PIC X(50) VALUE SPACE.
005190**************
005200* 請求先情報 *
005210**************
005220 01 請求先情報Ｗ.
005230    03 保険者番号Ｗ.
005240       05 印刷保険者番号Ｗ             PIC X(8) VALUE SPACE.
005250       05 FILLER                       PIC X(2) VALUE SPACE.
005260    03 請求先名称Ｗ.
005270       05 請求先名称１Ｗ               PIC X(40) VALUE SPACE.
005280       05 請求先名称２Ｗ               PIC X(40) VALUE SPACE.
005290    03 支部名Ｗ                        PIC X(40) VALUE SPACE.
005300    03 印刷支部名Ｗ                    PIC X(10) VALUE SPACE.
005310    03 請求先郵便番号Ｗ.
005320       05 請求先郵便番号１Ｗ           PIC X(3) VALUE SPACE.
005330       05 請求先郵便番号２Ｗ           PIC X(4) VALUE SPACE.
005340    03 請求先住所Ｗ.
005350       05 請求先住所１Ｗ               PIC X(40) VALUE SPACE.
005360       05 請求先住所２Ｗ               PIC X(40) VALUE SPACE.
005370****************
005380* 負傷データＦ *
005390****************
005400 01 負傷情報Ｗ.
005410    03 部位数Ｗ                        PIC 9(1) VALUE ZERO.
005420    03 負傷データ情報Ｗ  OCCURS   9.
005430       05 会部位コードＷ               PIC X(4) VALUE SPACE.
005440       05 部位ＣＮＴＷ                 PIC 9(1) VALUE ZERO.
005450       05 負傷種別Ｗ                   PIC 9(2) VALUE ZERO.
005460       05 部位Ｗ                       PIC 9(2) VALUE ZERO.
005470       05 左右区分Ｗ                   PIC 9(1) VALUE ZERO.
005480       05 負傷位置番号Ｗ               PIC 9(2) VALUE ZERO.
005490       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
005500       05 負傷年月日Ｗ.
005510          07 負傷和暦Ｗ                PIC 9(1) VALUE ZERO.
005520          07 負傷年Ｗ                  PIC 9(2) VALUE ZERO.
005530          07 負傷月Ｗ                  PIC 9(2) VALUE ZERO.
005540          07 負傷日Ｗ                  PIC 9(2) VALUE ZERO.
005550          07 負傷年月日区切Ｗ          PIC X(1) VALUE SPACE.
005560       05 施術開始年月日Ｗ.
005570          07 施術開始和暦Ｗ            PIC 9(1) VALUE ZERO.
005580          07 施術開始年Ｗ              PIC 9(2) VALUE ZERO.
005590          07 施術開始月Ｗ              PIC 9(2) VALUE ZERO.
005600          07 施術開始日Ｗ              PIC 9(2) VALUE ZERO.
005610          07 施開年月日区切Ｗ          PIC X(1) VALUE SPACE.
005620       05 施術終了年月日Ｗ.
005630          07 施術終了和暦Ｗ            PIC 9(1) VALUE ZERO.
005640          07 施術終了年Ｗ              PIC 9(2) VALUE ZERO.
005650          07 施術終了月Ｗ              PIC 9(2) VALUE ZERO.
005660          07 施術終了日Ｗ              PIC 9(2) VALUE ZERO.
005670          07 施終年月日区切Ｗ          PIC X(1) VALUE SPACE.
005680       05 開始年月日取得フラグ         PIC X(3) VALUE SPACE.
005690       05 転帰チェックＷ.
005700          07 治癒チェックＷ            PIC N(1) VALUE SPACE.
005710          07 中止チェックＷ            PIC N(1) VALUE SPACE.
005720          07 転医チェックＷ            PIC N(1) VALUE SPACE.
005730**********
005740* 合計行 *
005750**********
005760 01 合計行Ｗ.
005770    03 請求終了日Ｗ                    PIC 9(2) VALUE ZERO.
005780    03 対象西暦Ｗ                      PIC 9(4) VALUE ZERO.
005790    03 商Ｗ                            PIC 9(3) VALUE ZERO.
005800    03 余Ｗ                            PIC 9(3) VALUE ZERO.
005810*
005820*
005830*/年月毎持つ保険者データのワーク
005840 01 保負担率区分Ｗ                     PIC 9    VALUE ZERO.
005850 01 保本人負担率Ｗ                     PIC 9(3) VALUE ZERO.
005860 01 保家族負担率Ｗ                     PIC 9(3) VALUE ZERO.
005870 01 保本人負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
005880 01 保家族負担率乙Ｗ                   PIC 9(3) VALUE ZERO.
005890*
005900 01 比較和暦年月Ｗ.
005910    03 比較和暦Ｗ                      PIC 9    VALUE ZERO.
005920    03 比較年Ｗ                        PIC 9(2) VALUE ZERO.
005930    03 比較月Ｗ                        PIC 9(2) VALUE ZERO.
005940*
005950*/老人保険改正の対応0101
005960 01 老人負担計算区分Ｗ                 PIC 9    VALUE ZERO.
005970 01 負担率用種別Ｗ                     PIC 9(2) VALUE ZERO.
005980 01 助成負担区分Ｗ                     PIC 9(2) VALUE ZERO.
005990*
006000*
006010** 助成負担率用(14/10～)
006020 01 助成負担率ＷＷ.
006030    03 助成負担率Ｗ                    PIC 9(3) VALUE ZERO.
006040    03 助成負担率Ｗ１                  PIC 9    VALUE ZERO.
006050    03 助成負担率文字Ｗ１              PIC X    VALUE SPACE.
006060    03 助成負担率表示Ｗ                PIC X(6) VALUE SPACE.
006070*
006080** バーコード区分用
006090 01 バーコード使用区分Ｗ               PIC 9 VALUE ZERO.
       01 コメント欄Ｗ.
          03 症状Ｗ OCCURS 8 PIC X(48) VALUE SPACE.
          03 処置Ｗ OCCURS 8 PIC X(67) VALUE SPACE.
          03 経過Ｗ OCCURS 8 PIC X(48) VALUE SPACE.
001390**
001680 01 施術年月Ｗ.
001690    03 施術和暦Ｗ                      PIC 9(1) VALUE ZERO.
001690    03 施術年Ｗ                        PIC 9(2) VALUE ZERO.
001700    03 施術月Ｗ                        PIC 9(2) VALUE ZERO.
001360 01 合計Ｗ.
001380    03 初検料計Ｗ                      PIC 9(5) VALUE ZERO.
001390    03 整復料計Ｗ                      PIC 9(5) VALUE ZERO.
001400    03 後療料計Ｗ                      PIC 9(5) VALUE ZERO.
001400    03 罨法料計Ｗ                      PIC 9(5) VALUE ZERO.
001400    03 電療料計Ｗ                      PIC 9(5) VALUE ZERO.
001400    03 負担金計Ｗ                      PIC 9(5) VALUE ZERO.
001400*
001360 01 料金Ｗ.
001380    03 初検料Ｗ                        PIC 9(5) VALUE ZERO.
001390    03 整復料Ｗ                        PIC 9(5) VALUE ZERO.
001400    03 後療料Ｗ                        PIC 9(5) VALUE ZERO.
001400    03 罨法料Ｗ                        PIC 9(5) VALUE ZERO.
001400    03 電療料Ｗ                        PIC 9(5) VALUE ZERO.
001400    03 負担金Ｗ                        PIC 9(5) VALUE ZERO.
001831* POWER COBOL用
001832 01 dll-name  PIC X(260)  VALUE SPACE.
001833 01 form-name PIC X(14)   VALUE SPACE.
006100******************************************************************
006110 01 印刷制御.
006120     03 定義体名Ｐ                     PIC X(8) VALUE SPACE.
006130     03 項目群名Ｐ                     PIC X(8) VALUE SPACE.
006140     03 処理種別Ｐ                     PIC X(2) VALUE SPACE.
006150     03 拡張制御Ｐ.
006160         05 端末制御Ｐ.
006170             07 移動方向Ｐ             PIC X(1).
006180             07 移動行数Ｐ             PIC 9(3) VALUE ZERO.
006190         05 詳細制御Ｐ                 PIC X(2).
006200     03 通知情報Ｐ                     PIC X(2) VALUE SPACE.
006210     03 ユニット名Ｐ                   PIC X(8) VALUE SPACE.
006220*
006230 01 計算機西暦年Ｗ                     PIC 9(2) VALUE ZERO.
006240* 日付ＷＯＲＫ
006250 01 和暦終了年Ｗ                       PIC 9(4) VALUE ZERO.
006260 01 計算機西暦.
006270    03 計算機西暦年                    PIC 9(4) VALUE ZERO.
006280    03 計算機西暦月日                  PIC 9(4) VALUE ZERO.
006290 01 計算機西暦Ｒ REDEFINES 計算機西暦.
006300    03 計算機世紀                      PIC 9(2).
006310    03 計算機日付                      PIC 9(6).
006320    03 計算機日付Ｒ REDEFINES 計算機日付.
006330       05 計算機年月                   PIC 9(4).
006340       05 計算機年月Ｒ REDEFINES 計算機年月.
006350         07 計算機年                   PIC 9(2).
006360         07 計算機月                   PIC 9(2).
006370       05 計算機日                     PIC 9(2).
006380*
006390* C 連携用
006400 01  文字１Ｗ        PIC X(4096).
006410 01  文字２Ｗ        PIC X(512).
006420 01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
006421*
006422 01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
006423*
006430******************************************************************
006440*                          連結項目                              *
006450******************************************************************
006460*
006470**********************
006480* メッセージ表示キー *
006490**********************
006500 01 連メ－キー IS EXTERNAL.
006510    03  連メ－メッセージ                 PIC N(20).
003540*
       01 連メッセージＰ情報００５１ IS EXTERNAL.
          03 連メＰ－メッセージ番号                PIC 9(2).
          03 連メＰ－メッセージ.
             05 連メＰ－メッセージ内容             PIC X(40) OCCURS 6.
          03 連メＰ－メッセージ１                  PIC X(20).
          03 連メＰ－メッセージ２                  PIC X(12).
          03 連メＰ－返り値                        PIC X.
006520*
006530************
006540* 印刷キー *
006550************
006560 01 連印－対象データ IS EXTERNAL.
006570    03 連印－施術和暦年月.
006580       05 連印－施術和暦                  PIC 9(1).
006590       05 連印－施術年                    PIC 9(2).
006600       05 連印－施術月                    PIC 9(2).
006610    03 連印－保険種別                     PIC 9(2).
006620    03 連印－保険者番号                   PIC X(10).
006630    03 連印－本人家族区分                 PIC 9(1).
006640    03 連印－被保険者カナ                 PIC X(20).
006650    03 連印－患者コード.
006660       05 連印－患者番号                  PIC 9(6).
006670       05 連印－枝番                      PIC X(1).
006680    03 連印－印刷モードＦ                 PIC 9(1).
006690*/詳細
006700    03 連印－保険証印刷区分               PIC 9(1).
006710    03 連印－負傷詳細 OCCURS 7.
006720       05 連印－負傷印刷行                PIC 9(1).
006730       05 連印－部位印刷区分              PIC 9(1).
006740       05 連印－転帰印刷区分              PIC 9(1).
006750       05 連印－原因印刷区分              PIC 9(1).
006760*/
006770 01 連印－対象データ追加 IS EXTERNAL.
006780    03 連印－部位丸付け                   PIC 9(1).
006790*/年齢印刷0304
006800*/入力画面(660,6601)と新用紙(6621)との連結項目。(旧用紙、特別用紙等のＰＧを変えたく無い為、新たに作ります)
006810    03 連印－年齢印刷区分                 PIC 9(1).
006820*/負傷原因の詳細モード対応/0610
006830    03 連印－原因開始行                   PIC 9(2).
006840*/相談支援記載追加/080812
006850    03 連印－相談支援記載                 PIC 9(1).
006860*/ワクごと印刷 ０：枠なし １：ワクあり ２：ワクのみ /0402
006870    03 連印－枠印刷Ｆ                     PIC 9(1).
006880*/長期理由0311
006890    03 連印－長期印刷Ｆ                   PIC 9(1).
      *
       01 連印－対象データＹＨＮ７２０ IS EXTERNAL.
          03 連印－合計モード                   PIC 9(1).
          03 連印－明細モード                   PIC 9(1).
          03 連印－日段数                       PIC 9(2).
      */詳細
          03 連印－負傷印刷区分                 PIC 9(1).
          03 連印－施術印刷区分                 PIC 9(1).
006900*
006910 01 連入－表示フラグ６６０ IS EXTERNAL.
006920    03 連入－プレビュー区分               PIC 9(1).
006930* 負担率取得用14/10～
006940 01 連率－負担率取得キー IS EXTERNAL.
006950    03 連率－施術和暦年月.
006960       05 連率－施術和暦               PIC 9.
006970       05 連率－施術年月.
006980          07 連率－施術年              PIC 9(2).
006990          07 連率－施術月              PIC 9(2).
007000    03 連率－患者コード.
007010       05 連率－患者番号               PIC 9(6).
007020       05 連率－枝番                   PIC X.
007030    03 連率－実際負担率                PIC 9(3).
007040    03 連率－実際本体負担率            PIC 9(3).
007050    03 連率－健保負担率                PIC 9(3).
007060    03 連率－２７老負担率              PIC 9(3).
007070    03 連率－助成負担率                PIC 9(3).
007080    03 連率－特別用負担率              PIC 9(3).
007090*
007100************************************
007110* プリンタファイル作成用           *
007120************************************
007130 01 Ｈ連ＰＲＴＦ－作成データ IS EXTERNAL.
007140     03 Ｈ連ＰＲＴＦ－ファイル名           PIC X(8).
007150     03 Ｈ連ＰＲＴＦ－プレビュー区分       PIC 9.
007160     03 Ｈ連ＰＲＴＦ－帳票プログラム名     PIC X(8).
007170     03 Ｈ連ＰＲＴＦ－オーバレイ名         PIC X(8).
007180************************************
007190* プリンタファイル作成特殊用       *
007200************************************
007210 01 Ｈ連特殊ＰＲＴＦ－作成データ IS EXTERNAL.
007220     03 Ｈ連特殊ＰＲＴＦ－用紙種類         PIC X(8).
007230*
007231*--------------------------------------------------------*
007232* 暗号複合用
007233 01 連暗号複合－暗号情報 IS EXTERNAL.
007234    03 連暗号複合－入力情報.
007235       05 連暗号複合－記号               PIC X(24).
007236       05 連暗号複合－番号               PIC X(30).
007237       05 連暗号複合－暗号化項目.
007238         07 連暗号複合－暗号患者番号     PIC X(6).
007239         07 連暗号複合－暗号判定記号     PIC X.
007240         07 連暗号複合－暗号判定番号     PIC X.
007241         07 連暗号複合－暗号記号         PIC X(24).
007242         07 連暗号複合－暗号番号         PIC X(30).
007243    03 連暗号複合－出力情報.
007244       05 連暗号複合－複合した記号       PIC X(24).
007245       05 連暗号複合－複合した番号       PIC X(30).
007246*--------------------------------------------------------*
007247******************************************************************
007250*                      PROCEDURE  DIVISION                       *
007260******************************************************************
007270 PROCEDURE               DIVISION.
007280************
007290*           *
007300* 初期処理   *
007310*           *
007320************
007330     PERFORM プリンタファイル作成.
007340     PERFORM 初期化.
007350************
007360*           *
007370* 主処理     *
007380*           *
007390************
007400* 印刷
007430     PERFORM 連結項目待避.
007440     PERFORM 印刷セット.
007520************
007530*           *
007540* 終了処理   *
007550*           *
007560************
007570     PERFORM 終了処理.
007580     EXIT PROGRAM.
007590*
007600*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
007610*================================================================*
007620 プリンタファイル作成 SECTION.
007630*================================================================*
007640*   / 初期化 /
007650     MOVE SPACE TO Ｈ連ＰＲＴＦ－作成データ.
007660     INITIALIZE Ｈ連ＰＲＴＦ－作成データ.
007670     MOVE SPACE TO Ｈ連特殊ＰＲＴＦ－作成データ.
007680     INITIALIZE Ｈ連特殊ＰＲＴＦ－作成データ.
007690*
007700*
007710*--↓↓ 変更箇所 ↓↓--------------------------------------*
007720*   使用する用紙種別セット
007730     MOVE "KARUTE"              TO Ｈ連特殊ＰＲＴＦ－用紙種類.
007740*   使用するプリンタファイル名セット
007750     MOVE "PRTF002"             TO Ｈ連ＰＲＴＦ－ファイル名.
007760*
007770*   使用する帳票プログラム名セット
007790     MOVE "YHN662"              TO Ｈ連ＰＲＴＦ－帳票プログラム名.
007830*
007840*--↑↑-----------------------------------------------------*
007850*
007860*   / プレビュー区分セット /
007870     MOVE 連入－プレビュー区分 TO Ｈ連ＰＲＴＦ－プレビュー区分.
007880*     MOVE 1 TO Ｈ連ＰＲＴＦ－プレビュー区分.
007890*
007900     CALL   "CRTPRTF".
007910     CANCEL "CRTPRTF".
007920*
007930*================================================================*
007940 初期化 SECTION.
007950*
007960     PERFORM ファイルオープン.
007970*    /* 現在日付取得 */
007980     ACCEPT 計算機日付 FROM DATE.
007990*    /* 1980～2079年の間で設定 */
008000     IF 計算機年 > 80
008010         MOVE 19 TO 計算機世紀
008020     ELSE
008030         MOVE 20 TO 計算機世紀
008040     END-IF.
008050     PERFORM カレント元号取得.
008060     PERFORM 和暦終了年取得.
008070     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
008080*================================================================*
008090 カレント元号取得 SECTION.
008100*
008110     MOVE ZEROS TO 制－制御区分.
008120     READ 制御情報マスタ
008130     NOT INVALID KEY
008140         MOVE 制－カレント元号 TO カレント元号Ｗ
008150         MOVE 制－カルテ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
008160         MOVE 制－バーコード２３２Ｃ使用区分 TO バーコード使用区分Ｗ
008170     END-READ.
008180*
008190*================================================================*
008200 和暦終了年取得 SECTION.
008210*
008220     MOVE カレント元号Ｗ TO 元－元号区分.
008230     READ 元号マスタ
008240     INVALID KEY
008250         DISPLAY NC"指定和暦が登録されていません" UPON CONS
008260         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008270                                                  UPON CONS
008280*-----------------------------------------*
008290         CALL "actcshm"  WITH C LINKAGE
008300*-----------------------------------------*
008310         ACCEPT  キー入力 FROM CONS
008320         PERFORM 終了処理
008330         EXIT PROGRAM
008340     NOT INVALID KEY
008350         COMPUTE 前和暦Ｗ = カレント元号Ｗ - 1
008360         MOVE 前和暦Ｗ TO 元－元号区分
008370         READ 元号マスタ
008380         INVALID KEY
008390             DISPLAY NC"指定和暦が登録されていません" UPON CONS
008400             DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
008410                                                      UPON CONS
008420*-----------------------------------------*
008430             CALL "actcshm"  WITH C LINKAGE
008440*-----------------------------------------*
008450             ACCEPT  キー入力 FROM CONS
008460             PERFORM 終了処理
008470             EXIT PROGRAM
008480         NOT INVALID KEY
008490             MOVE 元－終了西暦年 TO 和暦終了年Ｗ
008500         END-READ
008510     END-READ.
008520*
008530*================================================================*
008540 連結項目待避 SECTION.
008550*
008560     INITIALIZE 対象データＷＲ.
008570     MOVE 連印－施術和暦      TO 施術和暦ＷＲ.
008580     MOVE 連印－施術年        TO 施術年ＷＲ.
008590     MOVE 連印－施術月        TO 施術月ＷＲ.
008600     MOVE 連印－保険種別      TO 保険種別ＷＲ.
008610     MOVE 連印－保険者番号    TO 保険者番号ＷＲ.
008620     MOVE 連印－本人家族区分  TO 本人家族区分ＷＲ.
008630     MOVE 連印－被保険者カナ  TO 被保険者カナＷＲ.
008640     MOVE 連印－患者番号      TO 患者番号ＷＲ.
008650     MOVE 連印－枝番          TO 枝番ＷＲ.
008660     MOVE 連印－印刷モードＦ  TO 印刷モードＦＷＲ.
008590     MOVE 連印－日段数        TO 日段数ＷＲ.
008670*================================================================*
008680 ファイルオープン SECTION.
008690*
008700     OPEN INPUT   保険者マスタ
008710         MOVE NC"保険者" TO ファイル名.
008720         PERFORM オープンチェック.
008730     OPEN INPUT   元号マスタ
008740         MOVE NC"元号" TO ファイル名.
008750         PERFORM オープンチェック.
008760     OPEN INPUT   名称マスタ
008770         MOVE NC"名称" TO ファイル名.
008780         PERFORM オープンチェック.
008790     OPEN INPUT   レセプトＦ
008800         MOVE NC"レセ" TO ファイル名.
008810         PERFORM オープンチェック.
008820     OPEN INPUT   制御情報マスタ
008830         MOVE NC"制御情報" TO ファイル名.
008840         PERFORM オープンチェック.
008850     OPEN INPUT   受診者情報Ｆ.
008860         MOVE NC"受情" TO ファイル名.
008870         PERFORM オープンチェック.
008880     OPEN INPUT   施術記録Ｆ.
008890         MOVE NC"施記Ｆ" TO ファイル名.
008900         PERFORM オープンチェック.
008910     OPEN INPUT   負傷データＦ.
008920         MOVE NC"負傷" TO ファイル名.
008930         PERFORM オープンチェック.
008940     OPEN INPUT   負傷原因Ｆ.
008950         MOVE NC"負傷原因" TO ファイル名.
008960         PERFORM オープンチェック.
008970     OPEN INPUT 市町村マスタ.
008980         MOVE NC"市町村" TO ファイル名.
008990         PERFORM オープンチェック.
009000     OPEN INPUT 事業所マスタ.
009010         MOVE NC"事業所" TO ファイル名.
009020         PERFORM オープンチェック.
009030     OPEN INPUT 労災情報Ｆ.
009040         MOVE NC"労災情報Ｆ" TO ファイル名.
009050         PERFORM オープンチェック.
006630     OPEN INPUT 生保情報Ｆ.
006640         MOVE NC"生保" TO ファイル名.
006650         PERFORM オープンチェック.
009290     OPEN INPUT 自賠責情報Ｆ.
009300         MOVE NC"自賠" TO ファイル名.
009310         PERFORM オープンチェック.
009290     OPEN INPUT 保険会社マスタ.
009300         MOVE NC"保会" TO ファイル名.
009310         PERFORM オープンチェック.
009290     OPEN INPUT 作業ファイル１.
009300         MOVE NC"作１" TO ファイル名.
009310         PERFORM オープンチェック.
009290     OPEN INPUT 作業ファイル２.
009300         MOVE NC"作２" TO ファイル名.
009310         PERFORM オープンチェック.
009080*     OPEN I-O   印刷ファイル.
009090*         MOVE NC"印刷" TO ファイル名.
009100*         PERFORM オープンチェック.
009160*================================================================*
009170 オープンチェック SECTION.
009180*
009190     IF 状態キー  NOT =  "00"
009200         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
009210         DISPLAY NC"状態キー：" 状態キー         UPON CONS
009220         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
009230                                                 UPON CONS
009240*-----------------------------------------*
009250         CALL "actcshm"  WITH C LINKAGE
009260*-----------------------------------------*
009270         ACCEPT  キー入力 FROM CONS
009280         PERFORM ファイル閉鎖
009290         EXIT PROGRAM.
009300*================================================================*
009310 ファイル閉鎖 SECTION.
009320*
009330     CLOSE 保険者マスタ   元号マスタ    名称マスタ 
009340           制御情報マスタ レセプトＦ    事業所マスタ
009350           受診者情報Ｆ   施術記録Ｆ    市町村マスタ
009360           負傷データＦ   負傷原因Ｆ    労災情報Ｆ
                 生保情報Ｆ     自賠責情報Ｆ  保険会社マスタ
                 作業ファイル１ 作業ファイル２.
002990     IF ( オープンフラグ = "YES" )
007470        CLOSE 印刷ファイル
           END-IF.
009430*================================================================*
009440 終了処理 SECTION.
009450*
009460     PERFORM ファイル閉鎖.
009470*================================================================*
009480 印刷セット SECTION.
009490*
009500     EVALUATE 印刷モードＦＷＲ
009510     WHEN 0
009520         PERFORM 印刷セット１
021230         PERFORM 印刷処理１
009530     WHEN 1
009540         PERFORM 印刷セット２
021230         PERFORM 印刷処理２
009550     WHEN 2
009560         PERFORM 印刷セット３
021250         PERFORM 印刷処理３
009570*/詳細モード用
009580     WHEN 4
009590         PERFORM 印刷セット５
021270         IF 連印－保険証印刷区分 = 1
021280             PERFORM 印刷処理２
021290         END-IF
021270         IF (連印－負傷印刷区分 = 1) OR (連印－施術印刷区分 = 1)
021300             PERFORM 印刷処理３
               END-IF
009570*/施術の事実用
009580     WHEN 5
009590         PERFORM 印刷セット６
021250         PERFORM 印刷処理３
009600     WHEN OTHER
009610         CONTINUE
009620     END-EVALUATE.
009630*
009640*================================================================*
009650 印刷セット１ SECTION.
009660*
009670     INITIALIZE YHN662P.
009670     MOVE SPACE TO YHN662P.
009680     MOVE "X" TO EDIT-MODE OF 患者コードＢ.
009690     INITIALIZE 受診者情報Ｗ.
009700     INITIALIZE 事業所情報Ｗ.
009710     INITIALIZE 請求先情報Ｗ.
009720     INITIALIZE 負傷情報Ｗ.
009730     PERFORM 負担率取得.
009740     PERFORM 請求先情報取得.
009750     PERFORM 受診者情報取得.
009760     PERFORM 負傷データ取得.
           PERFORM 症状処置経過セット.
009770*
009780     IF 負傷原因印刷区分Ｗ  NOT = 1 
009790        PERFORM 負傷原因取得
009800     END-IF.
009810*
009840     PERFORM 印刷上部セット.
009850     PERFORM 印刷下部セット.
009850     PERFORM 印刷施術セット.
009900*// TEST
009910******     PERFORM 印字テスト.
009920*================================================================*
009930 印刷セット２ SECTION.
009940*
009950     INITIALIZE YHN662P.
009960     MOVE "X" TO EDIT-MODE OF 患者コードＢ.
009970     INITIALIZE 受診者情報Ｗ.
009980     INITIALIZE 事業所情報Ｗ.
009990     INITIALIZE 請求先情報Ｗ.
010000     INITIALIZE 負傷情報Ｗ.
010010     PERFORM 負担率取得.
010020     PERFORM 請求先情報取得.
010030     PERFORM 受診者情報取得.
010060     PERFORM 印刷上部セット.
010100*================================================================*
010110 印刷セット３ SECTION.
010120*
010130     INITIALIZE YHN662P.
009670     MOVE SPACE TO YHN662P.
010140     MOVE "X" TO EDIT-MODE OF 患者コードＢ.
010150     INITIALIZE 受診者情報Ｗ.
010160     INITIALIZE 事業所情報Ｗ.
010170     INITIALIZE 請求先情報Ｗ.
010180     INITIALIZE 負傷情報Ｗ.
010190     PERFORM 受診者情報取得.
010200     PERFORM 負傷データ取得.
           PERFORM 症状処置経過セット.
010210*
010220     IF 負傷原因印刷区分Ｗ  NOT = 1 
010230        PERFORM 負傷原因取得
010240     END-IF.
010250*
010280     PERFORM 印刷下部セット.
010320*================================================================*
010330 印刷セット５ SECTION.
010340*
010350*/詳細モード用
010360     INITIALIZE YHN662P.
009670     MOVE SPACE TO YHN662P.
010370     MOVE "X" TO EDIT-MODE OF 患者コードＢ.
010380     INITIALIZE 受診者情報Ｗ.
010390     INITIALIZE 事業所情報Ｗ.
010400     INITIALIZE 請求先情報Ｗ.
010410     INITIALIZE 負傷情報Ｗ.
010420     PERFORM 負担率取得.
010430     PERFORM 請求先情報取得.
010440     PERFORM 受診者情報取得.
010450     PERFORM 負傷データ取得.
010460*
010470     IF 負傷原因印刷区分Ｗ  NOT = 1
010480        PERFORM 詳細モード負傷原因処理
010490        PERFORM 負傷原因取得
010500     END-IF.
010510*
010520*/負傷原因の詳細印刷/0610
010530     PERFORM 原因行セット.
010540*
010550     IF 連印－保険証印刷区分 = 1
010580        PERFORM 印刷上部セット
010620     END-IF.
010630*
010550     IF 連印－負傷印刷区分 = 1
              IF (連印－負傷詳細(1) NOT = ZERO) OR
                 (連印－負傷詳細(2) NOT = ZERO) OR
                 (連印－負傷詳細(3) NOT = ZERO) OR
                 (連印－負傷詳細(4) NOT = ZERO) OR
                 (連印－負傷詳細(5) NOT = ZERO) OR
                 (連印－負傷詳細(6) NOT = ZERO) OR
                 (連印－負傷詳細(7) NOT = ZERO)
010640           PERFORM 詳細モード負傷処理
              END-IF
010670        PERFORM 印刷下部セット
           END-IF.
010540*
010550     IF 連印－施術印刷区分 = 1
010580        PERFORM 印刷施術セット
           END-IF.
010710*
010100*================================================================*
010110 印刷セット６ SECTION.
010120*
010130     INITIALIZE YHN662P.
009670     MOVE SPACE TO YHN662P.
010140     MOVE "X" TO EDIT-MODE OF 患者コードＢ.
010150     INITIALIZE 受診者情報Ｗ.
010160     INITIALIZE 事業所情報Ｗ.
010170     INITIALIZE 請求先情報Ｗ.
010180     INITIALIZE 負傷情報Ｗ.
010250*
010280     PERFORM 印刷施術セット.
010720*================================================================*
010730 印刷上部セット SECTION.
010740*
010750     MOVE 患者番号Ｗ                 TO 患者番号.
010760     MOVE 枝番Ｗ                     TO 枝番.
010770     IF バーコード使用区分Ｗ NOT = ZERO
010780         MOVE 患者コードＷ    TO 患者コードＢ
010790         MOVE SPACE           TO EDIT-MODE OF 患者コードＢ
010800     ELSE
010810         MOVE "X" TO EDIT-MODE OF 患者コードＢ
010820     END-IF.
010830**************************
010840* 被保険者証情報セット   *
010850**************************
010860     IF 記号Ｗ(1:2) = "＊" 
010870        MOVE  SPACE    TO  記号
010880     ELSE
010890        MOVE 記号Ｗ    TO  記号
010900     END-IF.
010910     IF ( 印刷番号Ｗ(1:1) = "*"  ) OR
010920        ( 印刷番号Ｗ(1:2) = "＊" )
010930        MOVE  SPACE      TO  番号
010940     ELSE
010950        MOVE 印刷番号Ｗ  TO  番号
010960     END-IF.
010970************************
010980* 被保険者情報セット   *
010990************************
011000     MOVE 被保険者氏名Ｗ             TO 被保険者氏名.
011010     MOVE 被保険者カナＷ             TO 被保険者カナ.
011020     MOVE 郵便番号１Ｗ               TO 郵便番号１.
011030     MOVE 郵便番号２Ｗ               TO 郵便番号２.
011040     MOVE "-"                        TO 被保険者郵便区切.
011050     MOVE 印刷住所１Ｗ               TO 住所１.
011060     MOVE 印刷住所２Ｗ               TO 住所２.
011070*     MOVE 資格取得元号Ｗ             TO 資格取得元号.
011080     MOVE 資格昭和チェックＷ         TO 資格昭和チェック.
011090     MOVE 資格平成チェックＷ         TO 資格平成チェック.
           IF 資格令和チェックＷ NOT = SPACE
              MOVE NC"・令"             TO 資格令和ＣＭ
              MOVE 資格令和チェックＷ   TO 資格令和チェック
           END-IF.
011100     MOVE 資格取得年Ｗ               TO 資格取得年.
011110     MOVE 資格取得月Ｗ               TO 資格取得月.
011120     MOVE 資格取得日Ｗ               TO 資格取得日.
010970     IF (( 有効年Ｗ NOT = ZERO ) OR
010980         ( 有効月Ｗ NOT = ZERO ) OR
010990         ( 有効日Ｗ NOT = ZERO )) AND
              ( 有効和暦Ｗ = 5)
011090         MOVE NC"令和"               TO 有効和暦
011100         MOVE NC"＝＝"               TO 有効和暦訂正
011110     END-IF.
011130     MOVE 有効年Ｗ                   TO 有効年.
011140     MOVE 有効月Ｗ                   TO 有効月.
011150     MOVE 有効日Ｗ                   TO 有効日.
011160     MOVE 男チェックＷ               TO 男チェック.
011170     MOVE 女チェックＷ               TO 女チェック.
011180     MOVE 被保険者明治Ｗ             TO 被保険者明治.
011190     MOVE 被保険者大正Ｗ             TO 被保険者大正.
011200     MOVE 被保険者昭和Ｗ             TO 被保険者昭和.
011210     MOVE 被保険者平成Ｗ             TO 被保険者平成.
           IF 被保険者令和Ｗ NOT = SPACE
              MOVE NC"・令"                TO 被保険者令和ＣＭ
              MOVE 被保険者令和Ｗ          TO 被保険者令和
           END-IF.
011220     MOVE 被保険者年Ｗ               TO 被保険者年.
011230     MOVE 被保険者月Ｗ               TO 被保険者月.
011240     MOVE 被保険者日Ｗ               TO 被保険者日.
011250********************
011260* 患者情報セット   *
011270********************
011280     MOVE 患者カナＷ                 TO 患者カナ.
011290     MOVE 患者氏名Ｗ                 TO 患者氏名.
011300     MOVE 患者男チェックＷ           TO 患者男チェック.
011310     MOVE 患者女チェックＷ           TO 患者女チェック.
011320*     MOVE 患者性別Ｗ                 TO 患者性別.
011330*     MOVE 患者元号Ｗ                 TO 患者元号.
011340     MOVE 明治チェックＷ             TO 明治チェック.
011350     MOVE 大正チェックＷ             TO 大正チェック.
011360     MOVE 昭和チェックＷ             TO 昭和チェック.
011370     MOVE 平成チェックＷ             TO 平成チェック.
           IF 令和チェックＷ NOT = SPACE
              MOVE NC"令和"                TO 令和ＣＭ
              MOVE 令和チェックＷ          TO 令和チェック
           END-IF.
011380     MOVE 患者年Ｗ                   TO 患者年.
011390     MOVE 患者月Ｗ                   TO 患者月.
011400     MOVE 患者日Ｗ                   TO 患者日.
011410     MOVE 印刷続柄Ｗ                 TO 続柄.
011420     MOVE 本人チェックＷ             TO 本人チェック.
011430     MOVE 家族チェックＷ             TO 家族チェック.
011440     IF  印刷市町村番号Ｗ(1:2) = "99"
011450         MOVE SPACE                  TO 市町村番号
011460     ELSE
011470         MOVE 市町村番号Ｗ           TO 市町村番号
011480     END-IF.
011490     IF (印刷受益者番号Ｗ(1:1) = "*") OR
011500        (印刷受益者番号Ｗ(1:2) = "＊") 
011510         MOVE SPACE                  TO 受益者番号
011520     ELSE
011530         MOVE 受益者番号Ｗ           TO 受益者番号
011540     END-IF.
011550     MOVE 患者郵便番号１Ｗ           TO 患者郵便番号１.
011560     MOVE 患者郵便番号２Ｗ           TO 患者郵便番号２.
011570     MOVE "-"                        TO 患者郵便区切.
011580     MOVE 患者住所１Ｗ               TO 患者住所１.
011590     MOVE 患者住所２Ｗ               TO 患者住所２.
011600*     MOVE 患者電話番号Ｗ             TO 患者電話番号.
011610     MOVE 社保チェックＷ             TO 社保チェック.
011620     MOVE 組合チェックＷ             TO 組合チェック.
011630     MOVE 日雇チェックＷ             TO 日雇チェック.
011640     MOVE 船員チェックＷ             TO 船員チェック.
011650     MOVE 共済チェックＷ             TO 共済チェック.
011660     MOVE 国保チェックＷ             TO 国保チェック.
011670     MOVE 退本チェックＷ             TO 退本チェック.
011680     MOVE 退家チェックＷ             TO 退家チェック.
011690     MOVE 老チェックＷ               TO 老チェック.
011700     MOVE 障チェックＷ               TO 障チェック.
011710     MOVE 母チェックＷ               TO 母チェック.
011720     MOVE 乳チェックＷ               TO 乳チェック.
011730     MOVE 原チェックＷ               TO 原チェック.
011740     MOVE ０割チェックＷ             TO ０割チェック.
011750     MOVE １割チェックＷ             TO １割チェック.
011760     MOVE ２割チェックＷ             TO ２割チェック.
011770     MOVE ３割チェックＷ             TO ３割チェック.
011780     MOVE 証チェックＷ               TO 証チェック.
011790********************
011800* 事業所情報セット *
011810********************
011820     MOVE 事業所名称Ｗ               TO 事業所名称.
011830     MOVE 事業所郵便番号１Ｗ         TO 事業所郵便番号１.
011840     MOVE 事業所郵便番号２Ｗ         TO 事業所郵便番号２.
011850     MOVE 事業所住所Ｗ               TO 事業所住所.
011850     MOVE 事業所住所２Ｗ             TO 事業所住所２.
011860************************************
011870* 都道府県・健康保険組合情報セット *
011880************************************
011890*労災、自費の時は保険者情報は印字しない
011900     IF 保険種別ＷＲ = 70 OR 90
011910         MOVE SPACE              TO 保険者番号
011920         MOVE SPACE              TO 請求先名称１
011930         MOVE SPACE              TO 請求先名称２
011940         MOVE SPACE              TO 請求先郵便番号１
011950         MOVE SPACE              TO 保険者郵便区切
011960         MOVE SPACE              TO 請求先郵便番号２
011970         MOVE SPACE              TO 請求先住所１
011980         MOVE SPACE              TO 請求先住所２
011990     ELSE
012000         MOVE 印刷保険者番号Ｗ   TO 保険者番号
012010         MOVE 請求先名称１Ｗ     TO 請求先名称１
012020         MOVE 請求先名称２Ｗ     TO 請求先名称２
012030         MOVE 請求先郵便番号１Ｗ TO 請求先郵便番号１
012040         MOVE "-"                TO 保険者郵便区切
012050         MOVE 請求先郵便番号２Ｗ TO 請求先郵便番号２
012060         MOVE 請求先住所１Ｗ     TO 請求先住所１
012070         MOVE 請求先住所２Ｗ     TO 請求先住所２
012080     END-IF.
012090*================================================================*
012100 印刷下部セット SECTION.
012110*
012120********************
012130* 負傷データセット *
012140********************
012150* １部位 *
012160**********
012170     MOVE 負傷名Ｗ(1)         TO 負傷名１.
           IF 負傷和暦Ｗ(1) = 5
              MOVE NC"＝＝"         TO 負傷和暦訂正１
              MOVE NC"令和"         TO 負傷和暦１
           END-IF.
012180     MOVE 負傷年Ｗ(1)         TO 負傷年１.
012190     MOVE 負傷月Ｗ(1)         TO 負傷月１.
012200     MOVE 負傷日Ｗ(1)         TO 負傷日１.
012210     MOVE 施術開始年Ｗ(1)     TO 施術開始年１.
012220     MOVE 施術開始月Ｗ(1)     TO 施術開始月１.
012230     MOVE 施術開始日Ｗ(1)     TO 施術開始日１.
012240     MOVE 施術終了年Ｗ(1)     TO 施術終了年１.
012250     MOVE 施術終了月Ｗ(1)     TO 施術終了月１.
012260     MOVE 施術終了日Ｗ(1)     TO 施術終了日１.
012270     MOVE 負傷年月日区切Ｗ(1) TO 負傷年月区切１ 負傷月日区切１.
012280     MOVE 施開年月日区切Ｗ(1) TO 施開年月区切１ 施開月日区切１.
012290     MOVE 施終年月日区切Ｗ(1) TO 施終年月区切１ 施終月日区切１.
012300     MOVE 治癒チェックＷ(1)   TO 治癒チェック１.
012310     MOVE 中止チェックＷ(1)   TO 中止チェック１.
012320     MOVE 転医チェックＷ(1)   TO 転医チェック１.
012330*     ******************
012340*     * 日数は一時保留 *
012350*     ******************
012360*     **********************
012370*     * 施術回数は一時保留 *
012380*     **********************
012390**********
012400* ２部位 *
012410**********
012420     MOVE 負傷名Ｗ(2)         TO 負傷名２.
           IF 負傷和暦Ｗ(2) = 5
              MOVE NC"＝＝"         TO 負傷和暦訂正２
              MOVE NC"令和"         TO 負傷和暦２
           END-IF.
012430     MOVE 負傷年Ｗ(2)         TO 負傷年２.
012440     MOVE 負傷月Ｗ(2)         TO 負傷月２.
012450     MOVE 負傷日Ｗ(2)         TO 負傷日２.
012460     MOVE 施術開始年Ｗ(2)     TO 施術開始年２.
012470     MOVE 施術開始月Ｗ(2)     TO 施術開始月２.
012480     MOVE 施術開始日Ｗ(2)     TO 施術開始日２.
012490     MOVE 施術終了年Ｗ(2)     TO 施術終了年２.
012500     MOVE 施術終了月Ｗ(2)     TO 施術終了月２.
012510     MOVE 施術終了日Ｗ(2)     TO 施術終了日２.
012520     MOVE 負傷年月日区切Ｗ(2) TO 負傷年月区切２ 負傷月日区切２.
012530     MOVE 施開年月日区切Ｗ(2) TO 施開年月区切２ 施開月日区切２.
012540     MOVE 施終年月日区切Ｗ(2) TO 施終年月区切２ 施終月日区切２.
012550     MOVE 治癒チェックＷ(2)   TO 治癒チェック２.
012560     MOVE 中止チェックＷ(2)   TO 中止チェック２.
012570     MOVE 転医チェックＷ(2)   TO 転医チェック２.
012580*     ******************
012590*     * 日数は一時保留 *
012600*     ******************
012610*     **********************
012620*     * 施術回数は一時保留 *
012630*     **********************
012640**********
012650* ３部位 *
012660**********
012670     MOVE 負傷名Ｗ(3)         TO 負傷名３.
           IF 負傷和暦Ｗ(3) = 5
              MOVE NC"＝＝"         TO 負傷和暦訂正３
              MOVE NC"令和"         TO 負傷和暦３
           END-IF.
012680     MOVE 負傷年Ｗ(3)         TO 負傷年３.
012690     MOVE 負傷月Ｗ(3)         TO 負傷月３.
012700     MOVE 負傷日Ｗ(3)         TO 負傷日３.
012710     MOVE 施術開始年Ｗ(3)     TO 施術開始年３.
012720     MOVE 施術開始月Ｗ(3)     TO 施術開始月３.
012730     MOVE 施術開始日Ｗ(3)     TO 施術開始日３.
012740     MOVE 施術終了年Ｗ(3)     TO 施術終了年３.
012750     MOVE 施術終了月Ｗ(3)     TO 施術終了月３.
012760     MOVE 施術終了日Ｗ(3)     TO 施術終了日３.
012770     MOVE 負傷年月日区切Ｗ(3) TO 負傷年月区切３ 負傷月日区切３.
012780     MOVE 施開年月日区切Ｗ(3) TO 施開年月区切３ 施開月日区切３.
012790     MOVE 施終年月日区切Ｗ(3) TO 施終年月区切３ 施終月日区切３.
012800     MOVE 治癒チェックＷ(3)   TO 治癒チェック３.
012810     MOVE 中止チェックＷ(3)   TO 中止チェック３.
012820     MOVE 転医チェックＷ(3)   TO 転医チェック３.
012830*     ******************
012840*     * 日数は一時保留 *
012850*     ******************
012860*     **********************
012870*     * 施術回数は一時保留 *
012880*     **********************
012890**********
012900* ４部位 *
012910**********
012920     MOVE 負傷名Ｗ(4)         TO 負傷名４.
           IF 負傷和暦Ｗ(4) = 5
              MOVE NC"＝＝"         TO 負傷和暦訂正４
              MOVE NC"令和"         TO 負傷和暦４
           END-IF.
012930     MOVE 負傷年Ｗ(4)         TO 負傷年４.
012940     MOVE 負傷月Ｗ(4)         TO 負傷月４.
012950     MOVE 負傷日Ｗ(4)         TO 負傷日４.
012960     MOVE 施術開始年Ｗ(4)     TO 施術開始年４.
012970     MOVE 施術開始月Ｗ(4)     TO 施術開始月４.
012980     MOVE 施術開始日Ｗ(4)     TO 施術開始日４.
012990     MOVE 施術終了年Ｗ(4)     TO 施術終了年４.
013000     MOVE 施術終了月Ｗ(4)     TO 施術終了月４.
013010     MOVE 施術終了日Ｗ(4)     TO 施術終了日４.
013020     MOVE 負傷年月日区切Ｗ(4) TO 負傷年月区切４ 負傷月日区切４.
013030     MOVE 施開年月日区切Ｗ(4) TO 施開年月区切４ 施開月日区切４.
013040     MOVE 施終年月日区切Ｗ(4) TO 施終年月区切４ 施終月日区切４.
013050     MOVE 治癒チェックＷ(4)   TO 治癒チェック４.
013060     MOVE 中止チェックＷ(4)   TO 中止チェック４.
013070     MOVE 転医チェックＷ(4)   TO 転医チェック４.
013080*     ******************
013090*     * 日数は一時保留 *
013100*     ******************
013110*     **********************
013120*     * 施術回数は一時保留 *
013130*     **********************
013140**********
013150* ５部位 *
013160**********
013170     MOVE 負傷名Ｗ(5)         TO 負傷名５.
           IF 負傷和暦Ｗ(5) = 5
              MOVE NC"＝＝"         TO 負傷和暦訂正５
              MOVE NC"令和"         TO 負傷和暦５
           END-IF.
013180     MOVE 負傷年Ｗ(5)         TO 負傷年５.
013190     MOVE 負傷月Ｗ(5)         TO 負傷月５.
013200     MOVE 負傷日Ｗ(5)         TO 負傷日５.
013210     MOVE 施術開始年Ｗ(5)     TO 施術開始年５.
013220     MOVE 施術開始月Ｗ(5)     TO 施術開始月５.
013230     MOVE 施術開始日Ｗ(5)     TO 施術開始日５.
013240     MOVE 施術終了年Ｗ(5)     TO 施術終了年５.
013250     MOVE 施術終了月Ｗ(5)     TO 施術終了月５.
013260     MOVE 施術終了日Ｗ(5)     TO 施術終了日５.
013270     MOVE 負傷年月日区切Ｗ(5) TO 負傷年月区切５ 負傷月日区切５.
013280     MOVE 施開年月日区切Ｗ(5) TO 施開年月区切５ 施開月日区切５.
013290     MOVE 施終年月日区切Ｗ(5) TO 施終年月区切５ 施終月日区切５.
013300     MOVE 治癒チェックＷ(5)   TO 治癒チェック５.
013310     MOVE 中止チェックＷ(5)   TO 中止チェック５.
013320     MOVE 転医チェックＷ(5)   TO 転医チェック５.
013330*     ******************
013340*     * 日数は一時保留 *
013350*     ******************
013360*     **********************
013370*     * 施術回数は一時保留 *
013380*     **********************
013390************
013400* 負傷原因 *
013410************
013420     MOVE 負傷原因Ｗ(1)       TO 負傷原因１.
013430     MOVE 負傷原因Ｗ(2)       TO 負傷原因２.
013440     MOVE 負傷原因Ｗ(3)       TO 負傷原因３.
013450     MOVE 負傷原因Ｗ(4)       TO 負傷原因４.
013460     MOVE 負傷原因Ｗ(5)       TO 負傷原因５.
013470     MOVE 負傷原因Ｗ(6)       TO 負傷原因６.
013480     MOVE 負傷原因Ｗ(7)       TO 負傷原因７.
013490     MOVE 負傷原因Ｗ(8)       TO 負傷原因８.
013500     MOVE 負傷原因Ｗ(9)       TO 負傷原因９.
013510     MOVE 負傷原因Ｗ(10)      TO 負傷原因１０.
012290****************
012300* 症状処置経過 *
012310****************
      *     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 8
      *         MOVE 症状Ｗ(カウンタ) TO 症状(カウンタ)
      *         MOVE 処置Ｗ(カウンタ) TO 処置(カウンタ)
      *         MOVE 経過Ｗ(カウンタ) TO 経過(カウンタ)
      *     END-PERFORM.
013520*
010720*================================================================*
010730 印刷施術セット SECTION.
010740*
002980     PERFORM 合計値初期化.
           MOVE 1               TO 印字位置ＣＮＴ.
           MOVE 患者コードＷＲ  TO 作１－患者コード.
           MOVE 施術和暦ＷＲ    TO 作１－施術和暦.
           MOVE 施術年ＷＲ      TO 作１－施術年.
           MOVE 施術月ＷＲ      TO 作１－施術月.
           MOVE ZERO            TO 作１－施術日.
           START 作業ファイル１ KEY IS >= 作１－患者コード
                                          作１－施術和暦年月日
           END-START.
013500     IF 状態キー  =  "00"
013510         MOVE SPACE  TO 終了フラグ
013520         PERFORM 作業ファイル１読込
003000         MOVE 作１－患者コード   TO 患者コードＷ
               MOVE 作１－施術和暦     TO 施術和暦Ｗ
               MOVE 作１－施術年       TO 施術年Ｗ
               MOVE 作１－施術月       TO 施術月Ｗ
               IF 日段数ＷＲ = ZERO
                   MOVE 1              TO カウンタ
               ELSE
                   MOVE 日段数ＷＲ   TO カウンタ
               END-IF
      */      */患者コードが変わるまで作業ファイルを読む
003070         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
003090                       ( 作１－患者コード NOT = 患者コードＷ )
      *           */２９行を超えたら改ページ
004380             PERFORM UNTIL ( カウンタ > 29 ) OR
003090                           ( 作１－患者コード NOT = 患者コードＷ ) OR
                                 ( 終了フラグ = "YES" )
004640                 IF 連印－明細モード = 1
      */                  */明細行の転記
                           PERFORM 明細印刷処理
                       END-IF
003590                 MOVE 作１－患者コード   TO 患者コードＷ
003590                 MOVE 作１－施術和暦     TO 施術和暦Ｗ
003590                 MOVE 作１－施術年       TO 施術年Ｗ
003590                 MOVE 作１－施術月       TO 施術月Ｗ
003600                 PERFORM 作業ファイル１読込
003610             END-PERFORM
                   IF ((終了フラグ           = "YES") OR 
                       (作１－患者コード NOT = 患者コードＷ))
      */           */次頁なし
                       CONTINUE
                   ELSE
      */              */初期化が必要
                       MOVE 1 TO カウンタ
      */              */明細行を印刷して改頁する
                       PERFORM 印刷処理
003650                 PERFORM 改頁処理
                       MOVE 1                    TO 連メＰ－メッセージ番号
                       MOVE "次の用紙をセットしてください" TO 連メＰ－メッセージ
                       MOVE "PMSG0051.DLL" TO dll-name
                       MOVE "PMSG0051"     TO form-name
                       CALL "POWEROPENSHEET" USING dll-name form-name
                       EVALUATE  連メＰ－返り値
005871                 WHEN "Y"
021210                     EVALUATE 印刷モードＦＷＲ
021310                     WHEN 0
009840                         PERFORM 印刷上部セット
009850                         PERFORM 印刷下部セット
021260                     WHEN 4
021270                        IF 連印－保険証印刷区分 = 1
009840                           PERFORM 印刷上部セット
021290                        END-IF
021270                        IF (連印－負傷印刷区分 = 1) OR (連印－施術印刷区分 = 1)
009850                           PERFORM 印刷下部セット
                              END-IF
021330                     END-EVALUATE
                       WHEN OTHER
      */                  */Ｎ　又は　キャンセル
                           PERFORM 印刷行数退避
                           PERFORM 終了処理
                           EXIT PROGRAM
                       END-EVALUATE
                   END-IF
               END-PERFORM
      */合計行の印刷が必要な場合↓↓↓20130
               IF 連印－合計モード = 1
                   IF カウンタ > 28
      */         */初期化する20130
                      MOVE 1    TO カウンタ
                      PERFORM 印刷処理
003650                PERFORM 改頁処理
                      MOVE 1                    TO 連メＰ－メッセージ番号
                      MOVE "次の用紙をセットしてください" TO 連メＰ－メッセージ
                      MOVE "PMSG0051.DLL" TO dll-name
                      MOVE "PMSG0051"     TO form-name
                      CALL "POWEROPENSHEET" USING dll-name form-name
                      EVALUATE  連メＰ－返り値
005871                WHEN "Y"
021210                     EVALUATE 印刷モードＦＷＲ
021310                     WHEN 0
009840                         PERFORM 印刷上部セット
009850                         PERFORM 印刷下部セット
021260                     WHEN 4
021270                        IF 連印－保険証印刷区分 = 1
009840                           PERFORM 印刷上部セット
021290                        END-IF
021270                        IF (連印－負傷印刷区分 = 1) OR (連印－施術印刷区分 = 1)
009850                           PERFORM 印刷下部セット
                              END-IF
021330                     END-EVALUATE
                      WHEN OTHER
      */         */Ｎ　又は　キャンセル
                         PERFORM 印刷行数退避
                         PERFORM 終了処理
                         EXIT PROGRAM
                      END-EVALUATE
                   END-IF
                   PERFORM 合計印刷処理
               END-IF
               MOVE カウンタ TO 印字位置ＣＮＴ
               PERFORM 印刷行数退避
           END-IF.
004900*================================================================*
004910 明細印刷処理 SECTION.
004920*
           PERFORM 施術記録Ｆ読込
      *     IF (カウンタ = 1) OR (作１－施術日 = 施記－施術日)
      *         MOVE 作１－施術月   TO 月  (カウンタ)
      *         MOVE "/"            TO 区切(カウンタ)
      *     END-IF
005510     MOVE 作１－施術月       TO 月(カウンタ).
           MOVE "/"                TO 区切(カウンタ).
005520     MOVE 作１－施術日       TO 日(カウンタ).
004940********************
004950* 料金データセット *
004960********************
005010     MOVE 作１－初検金額     TO  初検料等(カウンタ).
005010     MOVE 作１－整復金額     TO  整復料(カウンタ).
005010     MOVE 作１－後療金額     TO  後療料(カウンタ).
005010     MOVE 作１－罨法金額     TO  罨法料(カウンタ).
005010     MOVE 作１－電療金額     TO  電療料(カウンタ).
           MOVE 作１－費用額       TO  費用額(カウンタ).
           MOVE 作１－一部負担金   TO  負担金(カウンタ).
           COMPUTE カウンタ = カウンタ + 1.
005730*================================================================*
005740 合計値初期化 SECTION.
005750*
005760     MOVE ZERO  TO 合計Ｗ.
004900*================================================================*
004910 合計印刷処理 SECTION.
004920*
           MOVE 施術和暦Ｗ   TO 作２－施術和暦.
           MOVE 施術年Ｗ     TO 作２－施術年.
           MOVE 施術月Ｗ     TO 作２－施術月.
           MOVE 患者コードＷ TO 作２－患者コード.
           READ 作業ファイル２
           NOT INVALID KEY
               MOVE NC"合計"     TO 合計(カウンタ)
               MOVE 作２－初検計 TO 初検料等(カウンタ)
               MOVE 作２－整復計 TO 整復料(カウンタ)
               MOVE 作２－後療計 TO 後療料(カウンタ)
               MOVE 作２－罨法計 TO 罨法料(カウンタ)
               MOVE 作２－電療計 TO 電療料(カウンタ)
               MOVE 作２－費用計 TO 費用額(カウンタ)
               MOVE 作２－負担計 TO 負担金        (カウンタ)
      *
               COMPUTE カウンタ = カウンタ + 1
               MOVE NC"費用額"            TO 費用(カウンタ)
               MOVE 作２－費用額          TO 合計費用額(カウンタ)
               MOVE NC"円"                TO 円１(カウンタ)
               MOVE NC"請求額"            TO 請求(カウンタ)
               MOVE 作２－請求額          TO 合計請求額(カウンタ)
               MOVE NC"円"                TO 円２(カウンタ)
               MOVE 作２－通院月          TO 通院月(カウンタ)
               MOVE NC"月"                TO 月１(カウンタ)
               MOVE 作２－通院日          TO 通院日(カウンタ)
               MOVE NC"日"                TO 日１(カウンタ)
               MOVE NC"回数"              TO 施術回数(カウンタ)
               MOVE 作２－施術回数        TO 回数(カウンタ)
               MOVE NC"回"                TO 回(カウンタ)
               MOVE 作２－転帰区分(1)     TO 転帰１(カウンタ)
               MOVE 作２－転帰区分(2)     TO 転帰２(カウンタ)
               MOVE 作２－転帰区分(3)     TO 転帰３(カウンタ)
               MOVE 作２－転帰区分(4)     TO 転帰４(カウンタ)
               MOVE 作２－転帰区分(5)     TO 転帰５(カウンタ)
           END-READ.
007070*================================================================*
007080 作業ファイル１読込 SECTION.
007090*
007100     READ 作業ファイル１ NEXT
007110     AT END
007120         MOVE "YES" TO 終了フラグ
007130     END-READ.
007140*
013530*================================================================*
013540 負担率取得 SECTION.
013550*
013560     MOVE ZERO TO 本人負担率Ｗ.
013570     MOVE ZERO TO 家族負担率Ｗ.
013580*
013590     MOVE 施術和暦ＷＲ   TO 受－施術和暦.
013600     MOVE 施術年ＷＲ     TO 受－施術年.
013610     MOVE 施術月ＷＲ     TO 受－施術月.
013620     MOVE 患者番号ＷＲ   TO 受－患者番号.
013630     MOVE 枝番ＷＲ       TO 受－枝番.
013640     READ 受診者情報Ｆ
013650     INVALID KEY
013660         MOVE  NC"　　施術月の受診者情報がありません" TO 連メ－メッセージ
013670         CALL   "MSG001"
013680         CANCEL "MSG001"
013690         PERFORM ファイル閉鎖
013700         MOVE ZERO TO PROGRAM-STATUS
013710         EXIT PROGRAM
013720     NOT INVALID KEY
013730* 14/10～　サブルーチン処理
013740         IF 受－施術和暦年月 >= 41410
013750            PERFORM 負担率取得１４１０
013760         END-IF
013770*
013780     END-READ.
013790*
013800     EVALUATE 負担率Ｗ
013810         WHEN ZERO
013820             MOVE NC"○" TO ０割チェックＷ
013830         WHEN 10
013840             MOVE NC"○" TO １割チェックＷ
013850         WHEN 20
013860             MOVE NC"○" TO ２割チェックＷ
013870         WHEN 30
013880             MOVE NC"○" TO ３割チェックＷ
013890         WHEN OTHER
013900             CONTINUE
013910     END-EVALUATE.
013920*
013930*================================================================*
013940 負担率取得１４１０ SECTION.
013950*
013960* 受診者Ｆ READ中
013970* 平成14/10～
013980     MOVE ZERO  TO 負担率Ｗ.
013990     INITIALIZE 助成負担率ＷＷ.
014000*
014010     MOVE SPACE TO 連率－負担率取得キー.
014020     INITIALIZE 連率－負担率取得キー.
014030     MOVE 受－施術和暦年月 TO 連率－施術和暦年月.
014040     MOVE 受－患者コード   TO 連率－患者コード.
014050*
014060     CALL   "HUTANRIT".
014070     CANCEL "HUTANRIT".
014080*
014090* / 本体 /
014100*(実際にする）
014110     MOVE 連率－実際負担率 TO 負担率Ｗ.
014120*
014130*
014140* / 助成 /
014150*     IF 受－助成種別 NOT = ZERO
014160*        MOVE 連率－助成負担率  TO 助成負担率Ｗ
014170*
014180*        COMPUTE 助成負担率Ｗ１ = 助成負担率Ｗ / 10
014190*        MOVE 助成負担率Ｗ１ TO  助成負担率文字Ｗ１
014200*
014210*        STRING "("                 DELIMITED BY SIZE
014220*               助成負担率文字Ｗ１  DELIMITED BY SIZE
014230*               "割)"               DELIMITED BY SIZE
014240*               INTO 助成負担率表示Ｗ
014250*        END-STRING
014260*
014270*     END-IF.
014280*
014290*================================================================*
014300*================================================================*
014310 受診者情報取得 SECTION.
014320*
014330**************************************************
014340* 連結データから受診者情報Ｆより以下の情報を取得 *
014350* ● 患者番号.... 患者番号Ｗ                     *
014360* ● 枝番........ 枝番Ｗ                         *
014370* ● 記号 ....... 記号Ｗに格納                   *
014380* ● 番号 ....... 番号Ｗに格納                   *
014390* ● 被保険者カナ.被保険者カナＷに格納           *
014400* ● 被保険者氏名.被保険者氏名Ｗに格納           *
014410* ● 郵便番号１ ..郵便番号１Ｗに格納             *
014420* ● 郵便番号２ ..郵便番号２Ｗに格納             *
014430* ● 住所１+住所２ ..被保険者住所Ｗに格納        *
014440* ● 電話番号.....電話番号Ｗに格納               *
014450* ● 患者カナ ....患者カナＷに格納               *
014460* ● 患者氏名 ....患者氏名Ｗに格納               *
014470* ● 続柄 ........名称マスタより続柄Ｗに取得     *
014480* ● 患者和暦 ....和暦によりチェックに"○"を格納 *
014490* ● 患者年 ......患者年Ｗに格納                 *
014500* ● 患者月 ......患者月Ｗに格納                 *
014510* ● 患者日 ......患者日Ｗに格納                 *
014520**************************************************
014530     MOVE 施術和暦ＷＲ       TO 受－施術和暦.
014540     MOVE 施術年ＷＲ         TO 受－施術年.
014550     MOVE 施術月ＷＲ         TO 受－施術月.
014560     MOVE 患者コードＷＲ     TO 受－患者コード.
014570     READ 受診者情報Ｆ
014580     INVALID KEY
014590         CONTINUE
014600*            /* ありえない */
014610     NOT INVALID KEY
014620         MOVE 受－患者番号     TO 患者番号Ｗ
014630         MOVE 受－枝番         TO 枝番Ｗ
014651*
014652*-----------------------------------------------------------------*
014653         MOVE SPACE TO 連暗号複合－暗号情報
014654*
014655*        / 連暗号複合－入力情報セット /
014656         MOVE 受－記号       TO 連暗号複合－記号
014657         MOVE 受－番号       TO 連暗号複合－番号
014658         MOVE 受－暗号化項目 TO 連暗号複合－暗号化項目
014659*
014660         CALL   複合プログラム名Ｗ
014661         CANCEL 複合プログラム名Ｗ
014662*
014663         MOVE 連暗号複合－複合した記号 TO 記号Ｗ
014664         MOVE 連暗号複合－複合した番号 TO 番号Ｗ
014665*-----------------------------------------------------------------*
014666*
014668         MOVE 受－被保険者カナ TO 被保険者カナＷ
014670         MOVE 受－被保険者氏名 TO 被保険者氏名Ｗ
014680*
014690         EVALUATE 受－被保険者性別
014700         WHEN 1
014710             MOVE NC"○"  TO 男チェックＷ
014720         WHEN 2
014730             MOVE NC"○"  TO 女チェックＷ
014740         END-EVALUATE
014750*
014760         EVALUATE 受－被保険者和暦
014770         WHEN 1
014780             MOVE NC"○"  TO 被保険者明治Ｗ
014790         WHEN 2
014800             MOVE NC"○"  TO 被保険者大正Ｗ
014810         WHEN 3
014820             MOVE NC"○"  TO 被保険者昭和Ｗ
014830         WHEN 4
014840             MOVE NC"○"  TO 被保険者平成Ｗ
014880         WHEN 5
014890             MOVE NC"○"  TO 被保険者令和Ｗ
014850         END-EVALUATE
014860         MOVE 受－被保険者年   TO 被保険者年Ｗ
014870         MOVE 受－被保険者月   TO 被保険者月Ｗ
014880         MOVE 受－被保険者日   TO 被保険者日Ｗ
014890         MOVE 受－郵便番号１   TO 郵便番号１Ｗ
014900         MOVE 受－郵便番号２   TO 郵便番号２Ｗ
014910*         STRING 受－住所１ DELIMITED BY SPACE
014920*                受－住所２ DELIMITED BY SPACE
014930*                INTO 被保険者住所Ｗ
014940*         END-STRING
014950         MOVE 受－住所１         TO 印刷住所１Ｗ
014960         MOVE 受－住所２         TO 印刷住所２Ｗ
014970         MOVE 受－患者郵便番号１ TO 患者郵便番号１Ｗ
014980         MOVE 受－患者郵便番号２ TO 患者郵便番号２Ｗ
014990*         STRING 受－患者住所１ DELIMITED BY SPACE
015000*                受－患者住所２ DELIMITED BY SPACE
015010*                INTO 患者住所Ｗ
015020*         END-STRING
015030         MOVE 受－電話番号     TO 電話番号Ｗ
               IF 受－保険種別 = 70 OR 80 OR 85 OR 90
018370             MOVE 受－患者カナ       TO 被保険者カナＷ
018380             MOVE 受－患者氏名       TO 被保険者氏名Ｗ
018000             EVALUATE 受－患者性別
018010             WHEN 1
018020                 MOVE NC"○"  TO 男チェックＷ
018030             WHEN 2
018040                 MOVE NC"○"  TO 女チェックＷ
018050             END-EVALUATE
018070             EVALUATE 受－患者和暦
018080             WHEN 1
018090                 MOVE NC"○"  TO 被保険者明治Ｗ
018100             WHEN 2
018110                 MOVE NC"○"  TO 被保険者大正Ｗ
018120             WHEN 3
018130                 MOVE NC"○"  TO 被保険者昭和Ｗ
                   WHEN 4
                       MOVE NC"○"  TO 被保険者平成Ｗ
                   WHEN 5
                       MOVE NC"○"  TO 被保険者令和Ｗ
018140             END-EVALUATE
018150             MOVE 受－患者年   TO 被保険者年Ｗ
018160             MOVE 受－患者月   TO 被保険者月Ｗ
018170             MOVE 受－患者日   TO 被保険者日Ｗ
014970             MOVE 受－患者郵便番号１ TO 郵便番号１Ｗ
014980             MOVE 受－患者郵便番号２ TO 郵便番号２Ｗ
016980             MOVE 受－患者住所１     TO 印刷住所１Ｗ
016990             MOVE 受－患者住所２     TO 印刷住所２Ｗ
018220             MOVE 受－患者電話番号   TO 電話番号Ｗ
               END-IF
015040         MOVE 受－患者住所１   TO 患者住所１Ｗ
015050         MOVE 受－患者住所２   TO 患者住所２Ｗ
015060*         MOVE 受－患者電話番号 TO 患者電話番号Ｗ
015070         STRING "TEL:"           DELIMITED BY SPACE
015080                受－患者電話番号 DELIMITED BY SPACE
015090                INTO 患者電話番号Ｗ
015100         END-STRING
015110*
015120         MOVE 受－患者カナ     TO 患者カナＷ
015130         MOVE 受－患者氏名     TO 患者氏名Ｗ
015140*
               MOVE 受－保険種別     TO 事－保険種別
               MOVE 受－保険者番号   TO 事－保険者番号
      *         MOVE 受－記号         TO 事－記号
      *-----------------------------------------------------------------*
               MOVE SPACE TO 連暗号複合－暗号情報
      *
      *        / 連暗号複合－入力情報セット /
               MOVE 受－記号       TO 連暗号複合－記号
               MOVE 受－番号       TO 連暗号複合－番号
               MOVE 受－暗号化項目 TO 連暗号複合－暗号化項目
      *
               CALL   複合プログラム名Ｗ
               CANCEL 複合プログラム名Ｗ
      *
               MOVE 連暗号複合－複合した記号 TO 事－記号
015150         READ 事業所マスタ
015160         INVALID KEY
015170            MOVE SPACE TO 事－レコード
015180            INITIALIZE    事－レコード
015190         END-READ
015200         MOVE 事－事業所名称   TO 事業所名称Ｗ
015210         STRING 事－事業所住所１  DELIMITED BY SPACE
015220                事－事業所住所２  DELIMITED BY SPACE
015230           INTO 事業所住所Ｗ
015240         END-STRING
015200         MOVE 事－事業所住所１   TO 事業所住所Ｗ
015200         MOVE 事－事業所住所２   TO 事業所住所２Ｗ
015250* 続柄
015260         IF 受－本人家族区分 = 1
015270             MOVE NC"本人"    TO 続柄Ｗ
015280             MOVE NC"○"      TO 本人チェックＷ
015290         ELSE
015300             MOVE NC"○"      TO 家族チェックＷ
015310             MOVE 05          TO 名－区分コード
015320             MOVE 受－続柄    TO 名－名称コード
015330             READ 名称マスタ
015340             INVALID KEY
015350                 MOVE SPACE   TO 続柄Ｗ
015360             NOT INVALID KEY
015370                 MOVE 名－略称  TO 続柄Ｗ
015380             END-READ
015390         END-IF
015400* 患者和暦年月日
015410         MOVE 受－患者和暦 TO 元－元号区分
015420         READ 元号マスタ
015430         INVALID KEY
015440             MOVE SPACE        TO 患者元号Ｗ
015450         NOT INVALID KEY
015460             MOVE 元－元号名称 TO 患者元号Ｗ
015470         END-READ
015480         EVALUATE 受－患者和暦
015490         WHEN 1
015500             MOVE NC"○"  TO 明治チェックＷ
015510         WHEN 2
015520             MOVE NC"○"  TO 大正チェックＷ
015530         WHEN 3
015540             MOVE NC"○"  TO 昭和チェックＷ
015550         WHEN 4
015560             MOVE NC"○"  TO 平成チェックＷ
015550         WHEN 5
015560             MOVE NC"○"  TO 令和チェックＷ
015570         END-EVALUATE
015580         MOVE 受－患者年  TO 患者年Ｗ
015590         MOVE 受－患者月  TO 患者月Ｗ
015600         MOVE 受－患者日  TO 患者日Ｗ
015610*患者年齢
015620     MOVE 連印－施術和暦 TO 元－元号区分
015630     READ 元号マスタ
015640     NOT INVALID KEY
015650         COMPUTE 施術西暦年Ｗ = 元－開始西暦年 + ( 連印－施術年 - 1 )
015660     END-READ
015670     IF ( 受－患者和暦 NOT = ZERO ) AND
015680        ( 受－患者年   NOT = ZERO ) AND
015690        ( 受－患者月   NOT = ZERO ) AND
015700        ( 受－患者日   NOT = ZERO )
015710*
015720         MOVE ZERO TO 患者年齢Ｗ
015730*
015740         MOVE 受－患者和暦 TO 元－元号区分
015750         READ 元号マスタ
015760         NOT INVALID KEY
015770             COMPUTE 患者西暦年Ｗ = 元－開始西暦年 + ( 受－患者年 - 1 )
015780         END-READ
015790*
015800         COMPUTE 患者年齢Ｗ   = 施術西暦年Ｗ - 患者西暦年Ｗ
015810*
015820         IF 連印－施術月 < 受－患者月
015830             COMPUTE 患者年齢Ｗ = 患者年齢Ｗ - 1
015840         END-IF
015850*
015860*         MOVE 患者年齢Ｗ     TO 患者年齢
015870     END-IF
015880* 患者性別
015890         EVALUATE 受－患者性別
015900         WHEN 1
015910             MOVE NC"○" TO 患者男チェックＷ
015920         WHEN 2
015930             MOVE NC"○" TO 患者女チェックＷ
015940         END-EVALUATE
015950         MOVE 受－有効和暦 TO 有効和暦Ｗ
015960         MOVE 受－有効年   TO 有効年Ｗ
015970         MOVE 受－有効月   TO 有効月Ｗ
015980         MOVE 受－有効日   TO 有効日Ｗ
015990*
016000         MOVE 受－資格和暦 TO 資格取得和暦Ｗ
016010         MOVE 受－資格年   TO 資格取得年Ｗ
016020         MOVE 受－資格月   TO 資格取得月Ｗ
016030         MOVE 受－資格日   TO 資格取得日Ｗ
016040         IF (資格取得和暦Ｗ NOT = ZERO) AND
016050            (資格取得年Ｗ   NOT = ZERO) AND
016060            (資格取得月Ｗ   NOT = ZERO) AND
016070            (資格取得日Ｗ   NOT = ZERO)
016080             EVALUATE 資格取得和暦Ｗ
016090             WHEN 3
016100                 MOVE NC"○"  TO 資格昭和チェックＷ
016110             WHEN 4
016120                 MOVE NC"○"  TO 資格平成チェックＷ
016110             WHEN 5
016120                 MOVE NC"○"  TO 資格令和チェックＷ
016130             END-EVALUATE
016140         END-IF
016150*         IF (資格取得和暦Ｗ NOT = ZERO) AND
016160*            (資格取得年Ｗ   NOT = ZERO) AND
016170*            (資格取得月Ｗ   NOT = ZERO) AND
016180*            (資格取得日Ｗ   NOT = ZERO)
016190*             MOVE 資格取得和暦Ｗ  TO 元－元号区分
016200*             READ 元号マスタ
016210*             INVALID KEY
016220*                 MOVE SPACE        TO 資格取得元号Ｗ
016230*             NOT INVALID KEY
016240*                 MOVE 元－元号名称 TO 資格取得元号Ｗ
016250*             END-READ
016260*         END-IF
016270*
016280* 市町村番号 受給者番号
016290* 老人 かつ 助成 の場合は老人の番号を印字する｡(今後どうなるかは解らない)
016300         IF 受－助成種別 NOT = ZERO
016310             MOVE 受－費用負担者番号助成 TO 市町村番号Ｗ
016320             MOVE 受－受益者番号助成     TO 受益者番号Ｗ
016330         END-IF
016340         IF ( 受－公費種別 = 05 ) AND ( 受－施術和暦年月 < 42004 )
016350             MOVE 受－費用負担者番号     TO 市町村番号Ｗ
016360             MOVE 受－受益者番号老人     TO 受益者番号Ｗ
016370         END-IF
016380*保険種別
016390         EVALUATE 受－保険種別
016400         WHEN 02
016410             MOVE NC"○"  TO 社保チェックＷ
016420         WHEN 03
016430             MOVE NC"○"  TO 組合チェックＷ
016440         WHEN 04
016450         WHEN 09
016460             MOVE NC"○"  TO 共済チェックＷ
016470         WHEN 06
016480             MOVE NC"○"  TO 日雇チェックＷ
016490         WHEN 07
016500             MOVE NC"○"  TO 船員チェックＷ
016510         WHEN 01
016520             MOVE NC"○"  TO 国保チェックＷ
016530         WHEN 08
016540             IF 受－本人家族区分 = 1
016550                MOVE NC"○"  TO 退本チェックＷ
016560             ELSE
016570                MOVE NC"○"  TO 退家チェックＷ
016580             END-IF
016590         WHEN OTHER
016600             CONTINUE
016610         END-EVALUATE
016620*助成種別
016630         EVALUATE 受－助成種別
016640         WHEN 51
016650             MOVE NC"○"  TO 老チェックＷ
016660         WHEN 53
016670             MOVE NC"○"  TO 障チェックＷ
016680         WHEN 52
016690             MOVE NC"○"  TO 母チェックＷ
016700         WHEN 55
016710             MOVE NC"○"  TO 乳チェックＷ
016720         WHEN 54
016730             MOVE NC"○"  TO 原チェックＷ
016740         WHEN OTHER
016750             CONTINUE
016760         END-EVALUATE
016770*
016780* 老人一部負担金免除
016790         IF (受－公費種別 = 05  )    AND
016800            (受－老人負担金免除 = 1) AND (受－費用負担者番号(3:2) = "27")
016810            MOVE NC"○" TO 証チェックＷ
016820         END-IF
016830         IF (受－公費種別 = ZERO)    AND (受－助成種別 = 51) AND
016840            (受－老人負担金免除 = 1) AND (受－費用負担者番号助成(3:2) = "27")
016850            MOVE NC"○" TO 証チェックＷ
016860         END-IF
016870*
016880         IF 受－保険種別 = 70
016890            MOVE 受－施術和暦年月 TO 労災－施術和暦年月
016900            MOVE 受－患者コード   TO 労災－患者コード
016910            READ 労災情報Ｆ
016920            INVALID KEY
016930               MOVE SPACE TO 労災－レコード
016940               INITIALIZE    労災－レコード
016950            END-READ
016960            MOVE 労災－労災事業所名称       TO 事業所名称Ｗ
016970            MOVE 労災－労災事業所郵便番号１ TO 事業所郵便番号１Ｗ
016980            MOVE 労災－労災事業所郵便番号２ TO 事業所郵便番号２Ｗ
016990            STRING 労災－労災事業所住所１ DELIMITED BY SPACE
017000                   労災－労災事業所住所２ DELIMITED BY SPACE
017010               INTO 事業所住所Ｗ
017020            END-STRING
015200            MOVE 労災－労災事業所住所１     TO 事業所住所Ｗ
015200            MOVE 労災－労災事業所住所２     TO 事業所住所２Ｗ
017030            MOVE 労災－労働保険番号         TO 番号Ｗ
017040         END-IF
017050*
017060     END-READ.
017070*================================================================*
017080 請求先情報取得 SECTION.
017090*
017100****************************************************
017110* 連結データから保険者マスタより請求先を取得する。 *
017120* ● 保険者番号...保険者番号Ｗに格納               *
017130* ● 名称........ 請求先名称Ｗに格納               *
017140* ● 郵便番号１.. 請求先郵便番号１Ｗに格納         *
017150* ● 郵便番号２.. 請求先郵便番号２Ｗに格納         *
017160* ● 住所１.......請求先住所１Ｗに格納             *
017170* ● 住所２.......請求先住所２Ｗに格納             *
017180****************************************************
017190     MOVE 保険種別ＷＲ   TO 保－保険種別.
017200     MOVE 保険者番号ＷＲ TO 保－保険者番号.
017210     READ 保険者マスタ
017220     INVALID KEY
017230         IF ( 受－施術和暦年月 >= 42004 ) AND ( 保険種別ＷＲ = 05 )
017240             MOVE 保険種別ＷＲ   TO 市－公費種別
017250             MOVE 保険者番号ＷＲ TO 市－市町村番号
017260             READ 市町村マスタ
017270             INVALID KEY
017280                 MOVE SPACE      TO 請求先名称Ｗ
017290             NOT INVALID KEY
017300                 MOVE 市－市町村番号  TO 保険者番号Ｗ
017310                 MOVE 市－市町村名称  TO 請求先名称Ｗ
017320                 MOVE 市－支部部署名  TO 支部名Ｗ
017330                 MOVE 市－郵便番号１  TO 請求先郵便番号１Ｗ
017340                 MOVE 市－郵便番号２  TO 請求先郵便番号２Ｗ
017350                 MOVE 市－住所１      TO 請求先住所１Ｗ
017360                 MOVE 市－住所２      TO 請求先住所２Ｗ
017370             END-READ
017380         ELSE
017390             MOVE SPACE      TO 請求先名称Ｗ
017400         END-IF
017410     NOT INVALID KEY
017420         MOVE 保－保険者番号  TO 保険者番号Ｗ
017430         EVALUATE 保険種別ＷＲ
017440         WHEN 2
017450         WHEN 6
017460            IF 保－接尾語区分 = 1
017470               MOVE 保－保険者名称 TO 請求先名称Ｗ
017480            ELSE
017490               STRING 保－保険者名称   DELIMITED BY SPACE
017500                      "社会保険事務所" DELIMITED BY SIZE
017510                      INTO 請求先名称Ｗ
017520               END-STRING
017530            END-IF
017540         WHEN 3
017550             STRING 保－保険者名称   DELIMITED BY SPACE
017560                    "健康保険組合  " DELIMITED BY SIZE
017570                    保－支部部署名   DELIMITED BY SPACE
017580               INTO 請求先名称Ｗ
017590             END-STRING
017600         WHEN 4
017610             STRING 保－保険者名称   DELIMITED BY SPACE
017620                    "共済組合  "     DELIMITED BY SIZE
017630                    保－支部部署名   DELIMITED BY SPACE
017640               INTO 請求先名称Ｗ
017650             END-STRING
017660         WHEN OTHER
017670             MOVE 保－保険者名称 TO 請求先名称Ｗ
017680         END-EVALUATE
017690         MOVE 保－支部部署名     TO 支部名Ｗ 
017700         MOVE 保－郵便番号１     TO 請求先郵便番号１Ｗ
017710         MOVE 保－郵便番号２     TO 請求先郵便番号２Ｗ
017720         MOVE 保－住所１         TO 請求先住所１Ｗ
017730         MOVE 保－住所２         TO 請求先住所２Ｗ
017740*         STRING 保－住所１ DELIMITED BY SPACE
017750*                保－住所２ DELIMITED BY SPACE
017760*           INTO 請求先住所Ｗ
017770*         END-STRING
017780     END-READ.
           IF 保険種別ＷＲ = 85
              MOVE 施術和暦年月ＷＲ TO 生保－施術和暦年月
              MOVE 患者コードＷＲ   TO 生保－患者コード
              READ 生保情報Ｆ
              NOT INVALID KEY
019480           MOVE 生保－負担者番号    TO 保険者番号Ｗ
019490           MOVE 生保－生保市町村名  TO 請求先名称Ｗ
019500           MOVE SPACE               TO 支部名Ｗ
019510           MOVE 生保－生保送付先郵便番号１  TO 請求先郵便番号１Ｗ
019520           MOVE 生保－生保送付先郵便番号２  TO 請求先郵便番号２Ｗ
020220           MOVE 生保－生保送付先住所１      TO 請求先住所１Ｗ
020230           MOVE 生保－生保送付先住所２      TO 請求先住所２Ｗ
                 MOVE SPACE              TO 記号Ｗ
                 MOVE 生保－生保記号番号 TO 番号Ｗ
              END-READ
           END-IF.
      *
022000* 自賠責情報
022010     IF 保険種別ＷＲ = 80
022020        MOVE 施術和暦年月ＷＲ TO 自賠－施術和暦年月
022030        MOVE 患者コードＷＲ   TO 自賠－患者コード
022040        READ 自賠責情報Ｆ
022050        INVALID KEY
022060           MOVE SPACE TO 自賠－レコード
022070           INITIALIZE    自賠－レコード
022080        END-READ
022090        MOVE 自賠－保険会社番号    TO 保険会－保険会社番号
022040        READ 保険会社マスタ
022050        NOT INVALID KEY
022060           MOVE 保険会－保険会社名 TO 請求先名称Ｗ
019500           MOVE SPACE              TO 支部名Ｗ
                 IF 自賠－保険会社送付先住所１ = SPACE
022450               MOVE 保険会－郵便番号１ TO 請求先郵便番号１Ｗ
022460               MOVE 保険会－郵便番号２ TO 請求先郵便番号２Ｗ
022470               MOVE 保険会－住所１     TO 請求先住所１Ｗ
022480               MOVE 保険会－住所２     TO 請求先住所２Ｗ
                  ELSE
022450               MOVE 自賠－保険会社送付先郵便番号１ TO 請求先郵便番号１Ｗ
022460               MOVE 自賠－保険会社送付先郵便番号２ TO 請求先郵便番号２Ｗ
019580               MOVE 自賠－保険会社送付先住所１     TO 請求先住所１Ｗ
019590               MOVE 自賠－保険会社送付先住所２     TO 請求先住所２Ｗ
                  END-IF
022080        END-READ
022170     END-IF.
017790*
017800*================================================================*
017810 負傷データ取得 SECTION.
017820*
017830**************************************************
017840* 連結データから負傷データＦより以下の情報を取得 *
017850* ● 負傷名...部位＋負傷種別にて加工して格納     *
017860* ● 負傷年.......負傷年Ｗ                       *
017870* ● 負傷月.......負傷月Ｗ                       *
017880* ● 負傷日.......負傷日Ｗ                       *
017890* ● 施術終了年...終了年Ｗ                       *
017900* ● 施術終了月...終了月Ｗ                       *
017910* ● 施術終了日...終了日Ｗ                       *
017920**************************************************
017930     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
017940     MOVE 施術年ＷＲ         TO 負－施術年.
017950     MOVE 施術月ＷＲ         TO 負－施術月.
017960     MOVE 患者コードＷＲ     TO 負－患者コード.
017970     READ 負傷データＦ
017980     INVALID KEY
017990         CONTINUE
018000*            /* ありえない */
018010     NOT INVALID KEY
018020         MOVE ZERO                         TO 部位数Ｗ
018030         MOVE 負－部位数                   TO 部位数Ｗ
018040         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018050                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
018060*********************************************
018070* 注）全柔...負傷種別＋部位にて加工して格納 *
018080*********************************************
018090* 負傷種別
018100             MOVE SPACE                     TO 負傷名称Ｗ
018110             MOVE 03                        TO 名－区分コード
018120             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
018130             READ 名称マスタ
018140             INVALID KEY
018150                 MOVE SPACE    TO 負傷名称Ｗ
018160             NOT INVALID KEY
018170*                 MOVE 名－略称 TO 負傷名称Ｗ
018180                 MOVE 名－正式名称 TO 負傷名称Ｗ
018190             END-READ
018200* 部位
018210             MOVE SPACE                    TO 負傷名Ｗ(部位ＣＮＴ)
018220             PERFORM 名称埋込処理
021050             MOVE 負－負傷和暦(部位ＣＮＴ) TO 負傷和暦Ｗ(部位ＣＮＴ)
018230             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
018240             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
018250             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
018260*
018270             MOVE 負－開始年(部位ＣＮＴ)   TO 施術開始年Ｗ(部位ＣＮＴ)
018280             MOVE 負－開始月(部位ＣＮＴ)   TO 施術開始月Ｗ(部位ＣＮＴ)
018290             MOVE 負－開始日(部位ＣＮＴ)   TO 施術開始日Ｗ(部位ＣＮＴ)
018300*
018310             MOVE 負－終了年(部位ＣＮＴ)   TO 施術終了年Ｗ(部位ＣＮＴ)
018320             MOVE 負－終了月(部位ＣＮＴ)   TO 施術終了月Ｗ(部位ＣＮＴ)
018330             MOVE 負－終了日(部位ＣＮＴ)   TO 施術終了日Ｗ(部位ＣＮＴ)
018340*転帰
018350             EVALUATE 負－転帰区分(部位ＣＮＴ)
018360             WHEN 1
018370             WHEN 2
018380                 MOVE NC"○"               TO 治癒チェックＷ(部位ＣＮＴ)
018390             WHEN 3
018400                 MOVE NC"○"               TO 中止チェックＷ(部位ＣＮＴ)
018410             WHEN 4
018420                 MOVE NC"○"               TO 転医チェックＷ(部位ＣＮＴ)
018430             END-EVALUATE
018440             PERFORM 負傷データ退避
018450         END-PERFORM
018460     END-READ.
018470*================================================================*
018480 施術記録取得 SECTION.
018490*
018500************************************************************
018510* 連結データから負傷データＦより以下の情報を取得           *
018520* ● 施術開始年月日...該当する部位に対して当月最初の施術日 *
018530************************************************************
018540     MOVE 施術和暦ＷＲ  TO 施記－施術和暦.
018550     MOVE 施術年ＷＲ    TO 施記－施術年.
018560     MOVE 施術月ＷＲ    TO 施記－施術月.
018570     MOVE ZERO            TO 施記－施術日.
018580     MOVE ZERO            TO 施記－患者番号.
018590     MOVE SPACE           TO 施記－枝番.
018600     START 施術記録Ｆ   KEY IS >= 施記－施術和暦年月日
018610                                  施記－患者コード.
018620     IF 状態キー = "00"
018630         MOVE SPACE  TO 終了フラグ２
018640         PERFORM 施術記録Ｆ読込
018650         PERFORM UNTIL ( 終了フラグ２       = "YES" ) OR
018660                       ( 施記－施術和暦 NOT = 施術和暦ＷＲ ) OR
018670                       ( 施記－施術年   NOT = 施術年ＷＲ   ) OR
018680                       ( 施記－施術月   NOT = 施術月ＷＲ   )
018690*            **************
018700*            * 開始年月日 *
018710*            **************
018720             PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
018730                     UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
018740            EVALUATE TRUE ALSO TRUE ALSO TRUE ALSO TRUE ALSO TRUE
018750            WHEN 施記－負傷種別(部位ＣＮＴ) = 負傷種別Ｗ(部位ＣＮＴ) ALSO
018760                 施記－部位(部位ＣＮＴ)     = 部位Ｗ(部位ＣＮＴ)     ALSO
018770                 施記－左右区分(部位ＣＮＴ) = 左右区分Ｗ(部位ＣＮＴ) ALSO
018780                 施記－負傷位置番号(部位ＣＮＴ)
018790                                       = 負傷位置番号Ｗ(部位ＣＮＴ)  ALSO
018800                 開始年月日取得フラグ(部位ＣＮＴ) = SPACE
018810                   MOVE 施記－施術年     TO 施術開始年Ｗ(部位ＣＮＴ)
018820                   MOVE 施記－施術月     TO 施術開始月Ｗ(部位ＣＮＴ)
018830                   MOVE 施記－施術日     TO 施術開始日Ｗ(部位ＣＮＴ)
018840                   MOVE "YES"       TO 開始年月日取得フラグ(部位ＣＮＴ)
018850*
018860            WHEN OTHER
018870                CONTINUE
018880            END-EVALUATE
018890             END-PERFORM
018900             PERFORM 施術記録Ｆ読込
018910         END-PERFORM
018920     END-IF.
018930*================================================================*
018940 施術記録Ｆ読込 SECTION.
018950*
018960     READ 施術記録Ｆ NEXT
018970     AT END
018980         MOVE "YES" TO 終了フラグ２
018990     END-READ.
019000*================================================================*
019010 負傷データ退避 SECTION.
019020*
019030*********************************************************************
019040* ① 負傷原因取得のため、負傷データＦから負傷原因Ｗに退避する。
019050* ② 詳細モード用  連印－負傷行印刷により、印刷位置を並び替えるため、
019060*                  負傷情報Ｗ  から負傷情報ＳＷに、
019070*                  負傷原因Ｗ  から負傷原因ＳＷに退避する。
019080*********************************************************************
019090*
019100     MOVE 負－負傷患者番号(部位ＣＮＴ) TO 負傷原因患者番号Ｗ(部位ＣＮＴ).
019110     MOVE 負－負傷連番(部位ＣＮＴ)     TO 負傷原因連番Ｗ(部位ＣＮＴ).
019120*--------------------------------------------------------------------*
019130     MOVE 負傷名Ｗ(部位ＣＮＴ) TO 負傷名ＳＷ(部位ＣＮＴ).
019140*
019150     MOVE 負傷和暦Ｗ(部位ＣＮＴ) TO 負傷和暦ＳＷ(部位ＣＮＴ).
019150     MOVE 負傷年Ｗ(部位ＣＮＴ) TO 負傷年ＳＷ(部位ＣＮＴ).
019160     MOVE 負傷月Ｗ(部位ＣＮＴ) TO 負傷月ＳＷ(部位ＣＮＴ).
019170     MOVE 負傷日Ｗ(部位ＣＮＴ) TO 負傷日ＳＷ(部位ＣＮＴ).
019180*
019190     MOVE 施術開始年Ｗ(部位ＣＮＴ) TO 施術開始年ＳＷ(部位ＣＮＴ).
019200     MOVE 施術開始月Ｗ(部位ＣＮＴ) TO 施術開始月ＳＷ(部位ＣＮＴ).
019210     MOVE 施術開始日Ｗ(部位ＣＮＴ) TO 施術開始日ＳＷ(部位ＣＮＴ).
019220*
019230     MOVE 施術終了年Ｗ(部位ＣＮＴ) TO 施術終了年ＳＷ(部位ＣＮＴ).
019240     MOVE 施術終了月Ｗ(部位ＣＮＴ) TO 施術終了月ＳＷ(部位ＣＮＴ).
019250     MOVE 施術終了日Ｗ(部位ＣＮＴ) TO 施術終了日ＳＷ(部位ＣＮＴ).
019260*
019270     MOVE 治癒チェックＷ(部位ＣＮＴ) TO 治癒チェックＳＷ(部位ＣＮＴ).
019280     MOVE 中止チェックＷ(部位ＣＮＴ) TO 中止チェックＳＷ(部位ＣＮＴ).
019290     MOVE 転医チェックＷ(部位ＣＮＴ) TO 転医チェックＳＷ(部位ＣＮＴ).
019300*     MOVE 施術累計回数Ｗ(部位ＣＮＴ) TO 施術累計回数ＳＷ(部位ＣＮＴ).
019310*
019320     MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号ＳＷ(部位ＣＮＴ).
019330     MOVE 負傷原因連番Ｗ(部位ＣＮＴ)     TO 負傷連番ＳＷ(部位ＣＮＴ).
019340*
019350*================================================================*
019360 負傷原因取得 SECTION.
019370*
019380********************************************************************
019390*  負傷原因コードが同じものは、1行にまとめて印字する。
019400*  例: ①② 家で転んだ.
019410*     負傷原因コードが同じものをまとめ、テーブルにセット
019420*     (ただし、部位を飛んで同じものは、2行になる)
019430********************************************************************
019440     MOVE  ZERO   TO  カウンタ カウンタ２.
019450     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
019460*/登録部位数以上の行に印刷出来ない不具合の修正/090311
019470             UNTIL ( 部位ＣＮＴ > 7 )
019480*             UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
019490*
019500****        IF ( 負－負傷患者番号(部位ＣＮＴ)  NOT = ZERO )  AND
019510        IF ( 負傷原因連番Ｗ(部位ＣＮＴ)      NOT = ZERO )
019520*
019530           IF カウンタ = ZERO
019540               MOVE 1   TO  カウンタ カウンタ２
019550               MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
019560               MOVE 負傷原因連番Ｗ(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)   負傷連番ＣＷ
019570               MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
019580           ELSE
019590              IF ( 負傷原因患者番号Ｗ(部位ＣＮＴ)  = 負傷患者番号ＣＷ )  AND
019600                 ( 負傷原因連番Ｗ(部位ＣＮＴ)      = 負傷連番ＣＷ     )
019610                 COMPUTE カウンタ２ = カウンタ２  +  1
019620                 MOVE 部位ＣＮＴ                  TO 負傷原因部位Ｗ(カウンタ カウンタ２)
019630              ELSE
019640                 COMPUTE カウンタ = カウンタ  +  1
019650                 MOVE 1   TO  カウンタ２
019660                 MOVE 負傷原因患者番号Ｗ(部位ＣＮＴ) TO 負傷患者番号Ｗ(カウンタ)  負傷患者番号ＣＷ
019670                 MOVE 負傷原因連番Ｗ(部位ＣＮＴ)     TO 負傷連番Ｗ(カウンタ)  負傷連番ＣＷ
019680                 MOVE 部位ＣＮＴ                   TO 負傷原因部位Ｗ(カウンタ カウンタ２)
019690              END-IF
019700           END-IF
019710        END-IF
019720     END-PERFORM.
019730**************************************************************************
019740*  負傷原因マスタより文章取得
019750**************************************************************************
019760     MOVE  ZERO   TO  カウンタ カウンタ２.
019770     PERFORM VARYING カウンタ FROM 1 BY 1
019780             UNTIL ( カウンタ > 9 )  OR ( 負傷連番Ｗ(カウンタ) = ZERO )
019790** 健保は 区分 01
019800*         MOVE 01                        TO 負原－区分コード
019810*/健保：０１、労災：０２、自賠責：０３
019820         EVALUATE 受－保険種別
019830         WHEN 70
019840             MOVE 2 TO 負原－区分コード
019850         WHEN 80
019860             MOVE 3 TO 負原－区分コード
019870         WHEN OTHER
019880             MOVE 1 TO 負原－区分コード
019890         END-EVALUATE
019900         MOVE 負傷患者番号Ｗ(カウンタ)  TO 負原－患者番号
019910         MOVE 負傷連番Ｗ(カウンタ)      TO 負原－負傷原因連番
019920         READ 負傷原因Ｆ
019930         NOT INVALID KEY
019940             INITIALIZE 負傷原因ＷＴ
019950             MOVE 負原－負傷原因ＣＭ(1) TO  負傷原因１ＷＴ
019960             MOVE 負原－負傷原因ＣＭ(2) TO  負傷原因２ＷＴ
019970             MOVE 負原－負傷原因ＣＭ(3) TO  負傷原因３ＷＴ
019980             MOVE 負原－負傷原因ＣＭ(4) TO  負傷原因４ＷＴ
019990             MOVE 負原－負傷原因ＣＭ(5) TO  負傷原因５ＷＴ
020000             PERFORM VARYING カウンタ２ FROM 1 BY 1
020010                     UNTIL ( カウンタ２ > 9 )  OR 
020020                           ( 負傷原因部位Ｗ(カウンタ カウンタ２) = ZERO )
020030                EVALUATE 負傷原因部位Ｗ(カウンタ カウンタ２)
020040                WHEN 1
020050                   MOVE "①"  TO  負傷原因ナンバーＷ１(カウンタ２)
020060                WHEN 2
020070                   MOVE "②"  TO  負傷原因ナンバーＷ１(カウンタ２)
020080                WHEN 3
020090                   MOVE "③"  TO  負傷原因ナンバーＷ１(カウンタ２)
020100                WHEN 4
020110                   MOVE "④"  TO  負傷原因ナンバーＷ１(カウンタ２)
020120                WHEN 5
020130                   MOVE "⑤"  TO  負傷原因ナンバーＷ１(カウンタ２)
020140                WHEN OTHER
020150                   CONTINUE
020160                END-EVALUATE
020170             END-PERFORM
020180*
020190             IF 負原－負傷原因入力区分 = 1
020200                 STRING 負傷原因ナンバーＮＷ  DELIMITED BY SPACE
020210                        負傷原因１ＷＴ  DELIMITED BY SIZE
020220                        負傷原因２ＷＴ  DELIMITED BY SIZE
020230                        負傷原因３ＷＴ  DELIMITED BY SIZE
020240                        負傷原因４ＷＴ  DELIMITED BY SIZE
020250                        負傷原因５ＷＴ  DELIMITED BY SIZE
020260                        INTO 負傷原因内容合成Ｗ(カウンタ)
020270                 END-STRING
020280             ELSE
020290                 INSPECT 負傷原因ＷＴ REPLACING ALL 全角空白 BY 半角空白
020300                 MOVE SPACE TO 文字１Ｗ 文字２Ｗ
020310                 MOVE 負傷原因ナンバーＮＷ TO 文字１Ｗ
020320                 MOVE 負傷原因１ＷＴ       TO 文字２Ｗ
020330                 CALL プログラム名Ｗ WITH C LINKAGE
020340                      USING BY REFERENCE 文字１Ｗ
020350                            BY REFERENCE 文字２Ｗ
020360                 MOVE 負傷原因２ＷＴ       TO 文字２Ｗ
020370                 CALL プログラム名Ｗ WITH C LINKAGE
020380                      USING BY REFERENCE 文字１Ｗ
020390                            BY REFERENCE 文字２Ｗ
020400                 MOVE 負傷原因３ＷＴ       TO 文字２Ｗ
020410                 CALL プログラム名Ｗ WITH C LINKAGE
020420                      USING BY REFERENCE 文字１Ｗ
020430                            BY REFERENCE 文字２Ｗ
020440                 MOVE 負傷原因４ＷＴ       TO 文字２Ｗ
020450                 CALL プログラム名Ｗ WITH C LINKAGE
020460                      USING BY REFERENCE 文字１Ｗ
020470                            BY REFERENCE 文字２Ｗ
020480                 MOVE 負傷原因５ＷＴ       TO 文字２Ｗ
020490                 CALL プログラム名Ｗ WITH C LINKAGE
020500                      USING BY REFERENCE 文字１Ｗ
020510                            BY REFERENCE 文字２Ｗ
020520                  MOVE 文字１Ｗ TO 負傷原因内容合成Ｗ(カウンタ)
020530             END-IF
020540*
020550         END-READ
020560     END-PERFORM.
020570*
020580     PERFORM 負傷原因セット.
020590*
020600*================================================================*
020610 負傷原因セット SECTION.
020620*
020630**************************************************************************
020640*  文章が1行を超える時は、複数行に分解する。
020650**************************************************************************
020660     MOVE  ZERO   TO  カウンタ カウンタ２.
020670     PERFORM VARYING カウンタ FROM 1 BY 1
020680             UNTIL ( カウンタ > 9 )  OR ( 負傷原因内容合成Ｗ(カウンタ) = SPACE )
020690*
020700          INITIALIZE 負傷原因内容分解ＸＷ
020710          MOVE 負傷原因内容合成Ｗ(カウンタ)   TO 負傷原因内容分解ＸＷ
020720          IF  負傷原因内容１ＸＷ  NOT = SPACE
020730              COMPUTE カウンタ２ = カウンタ２  +  1
020740              MOVE 負傷原因内容１ＸＷ  TO 負傷原因Ｗ(カウンタ２)
020750          END-IF
020760          IF  負傷原因内容２ＸＷ  NOT = SPACE
020770              COMPUTE カウンタ２ = カウンタ２  +  1
020780              MOVE 負傷原因内容２ＸＷ  TO 負傷原因Ｗ(カウンタ２)
020790          END-IF
020800          IF  負傷原因内容３ＸＷ  NOT = SPACE
020810              COMPUTE カウンタ２ = カウンタ２  +  1
020820              MOVE 負傷原因内容３ＸＷ  TO 負傷原因Ｗ(カウンタ２)
020830          END-IF
020840          IF  負傷原因内容４ＸＷ  NOT = SPACE
020850              COMPUTE カウンタ２ = カウンタ２  +  1
020860              MOVE 負傷原因内容４ＸＷ  TO 負傷原因Ｗ(カウンタ２)
020870          END-IF
020880          IF  負傷原因内容５ＸＷ  NOT = SPACE
020890              COMPUTE カウンタ２ = カウンタ２  +  1
020900              MOVE 負傷原因内容５ＸＷ  TO 負傷原因Ｗ(カウンタ２)
020910          END-IF
020920*
020930     END-PERFORM.
020940*
020950*================================================================*
020960 原因行セット SECTION.
020970*/0610
020980* 指定された開始行から印刷するようにセットし直す。
020990* １０行.
021000*
021010     MOVE SPACE                  TO 負傷原因Ｗテーブル２.
021020     MOVE ZERO                   TO カウンタ.
021030     IF 連印－原因開始行 = ZERO
021040         MOVE 1                  TO カウンタ２
021050     ELSE
021060         MOVE 連印－原因開始行   TO カウンタ２
021070     END-IF.
021080     PERFORM VARYING カウンタ FROM 1 BY 1
021090             UNTIL ( カウンタ > 10 ) OR (カウンタ２ > 10)
021100         MOVE 負傷原因Ｗ(カウンタ) TO 負傷原因Ｗ２(カウンタ２)
021110         COMPUTE カウンタ２ = カウンタ２ + 1
021120     END-PERFORM.
021130*
021140     PERFORM VARYING カウンタ FROM 1 BY 1
021150               UNTIL カウンタ > 10
021160         MOVE 負傷原因Ｗ２(カウンタ) TO 負傷原因Ｗ(カウンタ)
021170     END-PERFORM.
021180*================================================================*
021190 印刷処理 SECTION.
021200*
021210     EVALUATE 印刷モードＦＷＲ
021310     WHEN 0
021320         PERFORM 印刷処理１
021260     WHEN 4
021270         IF 連印－保険証印刷区分 = 1
021280             PERFORM 印刷処理２
021290         END-IF
021270         IF (連印－負傷印刷区分 = 1) OR (連印－施術印刷区分 = 1)
021300             PERFORM 印刷処理３
               END-IF
021310     WHEN 5
021320         PERFORM 印刷処理３
021330     END-EVALUATE.
021340*================================================================*
021350 印刷処理１ SECTION.
021360*
004310     IF ( オープンフラグ NOT = "YES" )
004320        MOVE "YES" TO オープンフラグ
004330        OPEN I-O  印刷ファイル
004340        PERFORM エラー処理Ｐ
004350     END-IF.
013440*
021390     MOVE "YHN662P"  TO  定義体名Ｐ.
021400     MOVE SPACE       TO  処理種別Ｐ.
021410     MOVE "GRP001"    TO  項目群名Ｐ.
021420     WRITE YHN662P.
021430     PERFORM エラー処理Ｐ.
021440*
021450     MOVE "YHN662P"  TO  定義体名Ｐ.
021460     MOVE SPACE       TO  処理種別Ｐ.
021470     MOVE "GRP002"    TO  項目群名Ｐ.
021480     WRITE YHN662P.
021490     PERFORM エラー処理Ｐ.
021630*================================================================*
021640 印刷処理２ SECTION.
021650*
004310     IF ( オープンフラグ NOT = "YES" )
004320        MOVE "YES" TO オープンフラグ
004330        OPEN I-O  印刷ファイル
004340        PERFORM エラー処理Ｐ
004350     END-IF.
013440*
021680     MOVE "YHN662P"  TO  定義体名Ｐ.
021690     MOVE SPACE       TO  処理種別Ｐ.
021700     MOVE "GRP001"    TO  項目群名Ｐ.
021710     WRITE YHN662P.
021720     PERFORM エラー処理Ｐ.
021800*================================================================*
021810 印刷処理３ SECTION.
021820*
004310     IF ( オープンフラグ NOT = "YES" )
004320        MOVE "YES" TO オープンフラグ
004330        OPEN I-O  印刷ファイル
004340        PERFORM エラー処理Ｐ
004350     END-IF.
013440*
021850     MOVE "YHN662P"  TO  定義体名Ｐ.
021860     MOVE SPACE       TO  処理種別Ｐ.
021870     MOVE "GRP002"    TO  項目群名Ｐ.
021880     WRITE YHN662P.
021890     PERFORM エラー処理Ｐ.
003730*================================================================*
003740 改頁処理  SECTION.
003750*
003760     MOVE "YHN662P" TO  定義体名Ｐ.
003770     MOVE "CT"      TO  処理種別Ｐ.
003780     MOVE "PAGE"    TO  拡張制御Ｐ.
003790     MOVE SPACE     TO  項目群名Ｐ.
003800     WRITE YHN662P.
003810     PERFORM エラー処理Ｐ.
003820     MOVE SPACE     TO  拡張制御Ｐ.
003821*
003822     CLOSE  印刷ファイル.
004320     MOVE SPACE TO オープンフラグ.
           MOVE SPACE TO YHN662P.
003825*
021970*================================================================*
021980 エラー処理Ｐ SECTION.
021990*
022000     IF 通知情報Ｐ NOT = "00"
022010         DISPLAY NC"帳票エラー"              UPON CONS
022020         DISPLAY NC"項目群名Ｐ：" 項目群名Ｐ UPON CONS
022030         DISPLAY NC"通知情報Ｐ：" 通知情報Ｐ UPON CONS
022040         DISPLAY NC"拡張制御Ｐ：" 拡張制御Ｐ UPON CONS
022050         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
022060                                             UPON CONS
022070*-----------------------------------------*
022080         CALL "actcshm"  WITH C LINKAGE
022090*-----------------------------------------*
022100         ACCEPT  キー入力 FROM CONS
022110         PERFORM ファイル閉鎖
022120         EXIT PROGRAM
022130     END-IF.
022140*================================================================*
022150 名称埋込処理 SECTION.
022160*
022170     EVALUATE 受－保険種別
022180     WHEN 05
022190         MOVE 2          TO レセ－レセ種別
022200     WHEN 70
022210         MOVE 4          TO レセ－レセ種別
022220     WHEN 80
022230         MOVE 5          TO レセ－レセ種別
022240     WHEN 85
022250         MOVE 7          TO レセ－レセ種別
022260     WHEN 90
022270         MOVE 6          TO レセ－レセ種別
022280     WHEN 91
022290         MOVE 8          TO レセ－レセ種別
022300     WHEN OTHER
022310         MOVE 1          TO レセ－レセ種別
022320     END-EVALUATE.
022330     MOVE 受－施術和暦 TO レセ－施術和暦.
022340     MOVE 受－施術年   TO レセ－施術年.
022350     MOVE 受－施術月   TO レセ－施術月.
022360     MOVE 受－患者番号 TO レセ－患者番号.
022370     MOVE 受－枝番     TO レセ－枝番.
022380     READ レセプトＦ
022390     NOT INVALID KEY
022400*
022410         STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
022420                レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
022430                負傷名称Ｗ                    DELIMITED BY SPACE
022440           INTO 負傷名Ｗ(部位ＣＮＴ)
022450         END-STRING
022460     END-READ.
022470*
022480*================================================================*
022490 詳細モード負傷原因処理 SECTION.
022500*
022510*******************************************************************
022520* 連印－負傷印刷行により、負傷原因データの並び替えを行う。
022530* ( 負傷原因情報Ｗを初期化し、退避済みの負傷原因情報ＳＷから
022540*         並び替えを行い、負傷原因情報Ｗを再設定する。)
022550*******************************************************************
022560*
022570     INITIALIZE 負傷原因情報Ｗ.
022580*
022590     PERFORM VARYING 詳細ＣＮＴ FROM 1 BY 1
022600                                UNTIL ( 詳細ＣＮＴ > 5)
022610*
022620        IF ( 連印－負傷印刷行(詳細ＣＮＴ) NOT = ZERO) AND
022630           ( 連印－原因印刷区分(詳細ＣＮＴ)   = 1)
022640*
022650           MOVE 連印－負傷印刷行(詳細ＣＮＴ) TO 印刷行ＳＷ
022660*
022670           MOVE 負傷患者番号ＳＷ(詳細ＣＮＴ) TO 負傷原因患者番号Ｗ(印刷行ＳＷ)
022680           MOVE 負傷連番ＳＷ(詳細ＣＮＴ) TO 負傷原因連番Ｗ(印刷行ＳＷ)
022690        END-IF
022700*
022710     END-PERFORM.
022720*
022730*================================================================*
022740 詳細モード負傷処理 SECTION.
022750*
022760*******************************************************************
022770* 連印－負傷印刷行により、負傷データの並び替えを行う。
022780* ( 負傷情報Ｗを初期化し、退避済みの負傷情報ＳＷから
022790*         並び替えを行い、負傷情報Ｗを再設定する。)
022800*******************************************************************
022810*
022820     INITIALIZE 負傷情報Ｗ.
022830*
022840     PERFORM VARYING 詳細ＣＮＴ FROM 1 BY 1
022850                                UNTIL ( 詳細ＣＮＴ > 5)
022860*
022870        IF 連印－負傷印刷行(詳細ＣＮＴ) NOT = ZERO
022880*/転帰のみ印字が出来ない不具合の修正。
022890           MOVE 連印－負傷印刷行(詳細ＣＮＴ) TO 印刷行ＳＷ
022900           IF 連印－部位印刷区分(詳細ＣＮＴ) = 1
022910*
022920*              MOVE 連印－負傷印刷行(詳細ＣＮＴ) TO 印刷行ＳＷ
022930*
022940               MOVE 負傷名ＳＷ(詳細ＣＮＴ) TO 負傷名Ｗ(印刷行ＳＷ)
022950               MOVE 負傷和暦ＳＷ(詳細ＣＮＴ) TO 負傷和暦Ｗ(印刷行ＳＷ)
022950               MOVE 負傷年ＳＷ(詳細ＣＮＴ) TO 負傷年Ｗ(印刷行ＳＷ)
022960               MOVE 負傷月ＳＷ(詳細ＣＮＴ) TO 負傷月Ｗ(印刷行ＳＷ)
022970               MOVE 負傷日ＳＷ(詳細ＣＮＴ) TO 負傷日Ｗ(印刷行ＳＷ)
022980               MOVE 施術開始年ＳＷ(詳細ＣＮＴ) TO 施術開始年Ｗ(印刷行ＳＷ)
022990               MOVE 施術開始月ＳＷ(詳細ＣＮＴ) TO 施術開始月Ｗ(印刷行ＳＷ)
023000               MOVE 施術開始日ＳＷ(詳細ＣＮＴ) TO 施術開始日Ｗ(印刷行ＳＷ)
023010           END-IF
023020*
023030           IF 連印－転帰印刷区分(詳細ＣＮＴ) = 1
023040               MOVE 施術終了年ＳＷ(詳細ＣＮＴ) TO 施術終了年Ｗ(印刷行ＳＷ)
023050               MOVE 施術終了月ＳＷ(詳細ＣＮＴ) TO 施術終了月Ｗ(印刷行ＳＷ)
023060               MOVE 施術終了日ＳＷ(詳細ＣＮＴ) TO 施術終了日Ｗ(印刷行ＳＷ)
023070               MOVE 治癒チェックＳＷ(詳細ＣＮＴ)   TO 治癒チェックＷ(印刷行ＳＷ)
023080               MOVE 中止チェックＳＷ(詳細ＣＮＴ)   TO 中止チェックＷ(印刷行ＳＷ)
023090               MOVE 転医チェックＳＷ(詳細ＣＮＴ)   TO 転医チェックＷ(印刷行ＳＷ)
023100           END-IF
023110        END-IF
023120*
023130     END-PERFORM.
023140*
025960*================================================================*
025970 印字テスト SECTION.
025980     MOVE NC"○" TO 資格昭和チェック 資格平成チェック.
025990     MOVE NC"○" TO 証チェック.
026000     MOVE NC"○" TO ０割チェック １割チェック ２割チェック ３割チェック.
026010     MOVE NC"○" TO 社保チェック 組合チェック 共済チェック 日雇チェック
026020                    船員チェック 国保チェック 退本チェック 退家チェック.
026030     MOVE NC"○" TO 老チェック 障チェック 母チェック 乳チェック
026040                    原チェック 本人チェック 家族チェック.
026050     MOVE NC"○" TO 明治チェック 大正チェック 昭和チェック 平成チェック.
026060     MOVE NC"○" TO 被保険者明治 被保険者大正 被保険者昭和 被保険者平成.
026070     MOVE NC"○" TO 治癒チェック１ 治癒チェック２ 治癒チェック３ 治癒チェック４ 治癒チェック５.
026080     MOVE NC"○" TO 中止チェック１ 中止チェック２ 中止チェック３ 中止チェック４ 中止チェック５.
026090     MOVE NC"○" TO 転医チェック１ 転医チェック２ 転医チェック３ 転医チェック４ 転医チェック５.
026100     MOVE NC"○" TO 男チェック 女チェック 患者男チェック 患者女チェック.
026110     MOVE ALL "X"    TO 市町村番号 受益者番号 保険者番号 番号.
026120     MOVE ALL "X"    TO 被保険者カナ 患者カナ 郵便番号１ 郵便番号２ 患者郵便番号１ 患者郵便番号２.
026130     MOVE ALL "柔"   TO 記号 被保険者氏名 患者氏名.
026140     MOVE ALL "柔"   TO 住所１ 住所２ 患者住所１ 患者住所２.
026150     MOVE ALL "9"    TO 被保険者カナ 患者カナ
026160                        資格取得年 資格取得月 資格取得日 有効年 有効月 有効日
026170                        被保険者年 被保険者月 被保険者日 患者年 患者月 患者日.
026180     MOVE ALL "X"    TO 事業所郵便番号１ 事業所郵便番号２ 請求先郵便番号１ 請求先郵便番号２.
026190     MOVE ALL "柔"   TO 事業所名称 事業所住所 請求先名称１ 請求先名称２ 請求先住所１ 請求先住所２.
026200     MOVE ALL NC"柔" TO 続柄 負傷名１ 負傷名２ 負傷名３ 負傷名４ 負傷名５.
026210     MOVE ALL "柔"   TO 負傷原因１ 負傷原因２ 負傷原因３ 負傷原因４ 負傷原因５
026220                        負傷原因６ 負傷原因７ 負傷原因８ 負傷原因９ 負傷原因１０.
026230     MOVE ALL "9"    TO 負傷年１ 負傷年２ 負傷年３ 負傷年４ 負傷年５
026240                        負傷月１ 負傷月２ 負傷月３ 負傷月４ 負傷月５
026250                        負傷日１ 負傷日２ 負傷日３ 負傷日４ 負傷日５
026260                        施術開始年１ 施術開始年２ 施術開始年３ 施術開始年４ 施術開始年５
026270                        施術開始月１ 施術開始月２ 施術開始月３ 施術開始月４ 施術開始月５
026280                        施術開始日１ 施術開始日２ 施術開始日３ 施術開始日４ 施術開始日５
026290                        施術終了年１ 施術終了年２ 施術終了年３ 施術終了年４ 施術終了年５
026300                        施術終了月１ 施術終了月２ 施術終了月３ 施術終了月４ 施術終了月５
026310                        施術終了日１ 施術終了日２ 施術終了日３ 施術終了日４ 施術終了日５.
023910*================================================================*
       症状処置経過セット SECTION.
      *
      */症状、経過
           MOVE 1 TO カウンタ２
           PERFORM 2 TIMES
               PERFORM VARYING カウンタ３ FROM 1 BY 1 UNTIL (カウンタ３ > 部位数Ｗ) OR (カウンタ３ > 4)
                   STRING "("        DELIMITED BY SIZE
                          カウンタ３ DELIMITED BY SIZE
                          ")腫脹・疼痛・圧痛・運動痛自発痛著明　残存" DELIMITED BY SIZE
                     INTO 症状Ｗ(カウンタ２)
                   END-STRING
                   MOVE 症状Ｗ(カウンタ２) TO 経過Ｗ(カウンタ２)
                   COMPUTE カウンタ２ = カウンタ２ + 1
               END-PERFORM
           END-PERFORM.
      */処置
           MOVE 1 TO カウンタ２
           PERFORM 2 TIMES
               PERFORM VARYING カウンタ３ FROM 1 BY 1 UNTIL (カウンタ３ > 部位数Ｗ) OR (カウンタ３ > 4)
      *             STRING "("        DELIMITED BY SIZE
      *                    カウンタ３ DELIMITED BY SIZE
      *                    ")初回処置、施療、罨法、湿布(晒,テ,キ,包)固定" DELIMITED BY SIZE
      *               INTO 処置Ｗ(カウンタ２)
      *             END-STRING
      *             COMPUTE カウンタ２ = カウンタ２ + 1
      *             MOVE "　後療,電療,手技,罨法" TO 処置(カウンタ２)
      *             COMPUTE カウンタ２ = カウンタ２ + 1
                   STRING "("        DELIMITED BY SIZE
                          カウンタ３ DELIMITED BY SIZE
                          ")初回処置、施療、罨法、湿布(晒,テ,キ,包)固定" DELIMITED BY SIZE
                          " 後療,電療,手技" DELIMITED BY SIZE
                     INTO 処置Ｗ(カウンタ２)
                   END-STRING
                   COMPUTE カウンタ２ = カウンタ２ + 1
               END-PERFORM
           END-PERFORM.
006850*================================================================*
006860 印刷行数退避 SECTION.
006870     CLOSE 受診者情報Ｆ.
006890     OPEN I-O 受診者情報Ｆ.
006900         MOVE NC"受診" TO ファイル名.
006910         PERFORM オープンチェック.
006930*
006940     MOVE ZERO            TO 受－施術和暦.
006950     MOVE ZERO            TO 受－施術年.
006960     MOVE ZERO            TO 受－施術月.
006970     MOVE 患者コードＷ    TO 受－患者コード.
006990*
007000     READ 受診者情報Ｆ
007010     NOT INVALID KEY
007020         MOVE 印字位置ＣＮＴ TO 受－カルテ裏印刷行数
007030         REWRITE 受－レコード
007040         INVALID KEY
007050             MOVE NC"受Ｆ" TO ファイル名
007060             PERFORM エラー表示
007070         END-REWRITE
007080     END-READ.
007100     CLOSE 受診者情報Ｆ.
007120     OPEN INPUT 受診者情報Ｆ.
007130         MOVE NC"受診" TO ファイル名.
007140         PERFORM オープンチェック.
007610*================================================================*
007620 エラー表示 SECTION.
007630*
007640     DISPLAY ファイル名   NC"ファイル書込エラー"   UPON CONS.
007650     DISPLAY NC"状態キー：" 状態キー               UPON CONS.
007660     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
007670     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"  UPON CONS.
000080*-----------------------------------------*
000090     CALL "actcshm"  WITH C LINKAGE.
000100*-----------------------------------------*
007680     ACCEPT  キー入力 FROM CONS.
007690     PERFORM ファイル閉鎖.
007700     EXIT PROGRAM.
026320*================================================================*
026330******************************************************************
026340 END PROGRAM YHN662.
026350******************************************************************
