000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             YAI101.
000060 AUTHOR.                 山田 浩之　池田 幸子
000070*
000080*----------------------------------------------------------------*
000090*      提出FPD作成【ﾃﾞｰﾀ作成】柔ｳｨﾝﾄﾞｳｽﾞ95版
000140*----------------------------------------------------------------*
000150 DATE-WRITTEN.           2010-04-08
000160 DATE-COMPILED.          2010-04-08
000170*----------------------------------------------------------------*
      */金属副子運動後療追加/20180612
      */明細書発行以降追加/20221117/池田
000180******************************************************************
000190*            ENVIRONMENT         DIVISION                        *
000200******************************************************************
000210 ENVIRONMENT             DIVISION.
000220 CONFIGURATION           SECTION.
000230 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000240 OBJECT-COMPUTER.        FMV-DESKPOWER.
000250 SPECIAL-NAMES.          CONSOLE  IS  CONS
000260                         SYSERR   IS  MSGBOX.
000270 INPUT-OUTPUT            SECTION.
000280 FILE-CONTROL.
000281     SELECT  制御情報マスタ  ASSIGN      TO        SEIGYOL
000282                             ORGANIZATION             IS  INDEXED
000283                             ACCESS MODE              IS  DYNAMIC
000284                             RECORD KEY               IS  制－制御区分
000285                             FILE STATUS              IS  状態キー
000286                             LOCK        MODE         IS  AUTOMATIC.
000290     SELECT  元号マスタ      ASSIGN      TO        GENGOUL
000300                             ORGANIZATION             IS  INDEXED
000310                             ACCESS MODE              IS  DYNAMIC
000320                             RECORD KEY               IS  元－元号区分
000330                             FILE STATUS              IS  状態キー
000340                             LOCK        MODE         IS  AUTOMATIC.
000350     SELECT  名称マスタ      ASSIGN      TO        MEISYOL
000360                             ORGANIZATION             IS  INDEXED
000370                             ACCESS MODE              IS  DYNAMIC
000380                             RECORD KEY               IS  名－区分コード
000390                                                          名－名称コード
000400                             FILE STATUS              IS  状態キー
000410                             LOCK        MODE         IS  AUTOMATIC.
000420     SELECT  施術所情報マスタ ASSIGN      TO        SEJOHOL
000430                             ORGANIZATION             IS  INDEXED
000440                             ACCESS MODE              IS  DYNAMIC
000450                             RECORD KEY               IS 施情－施術所番号
000460                             FILE STATUS              IS  状態キー
000470                             LOCK        MODE         IS  AUTOMATIC.
000480     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
000490                             ORGANIZATION             IS  INDEXED
000500                             ACCESS MODE              IS  DYNAMIC
000510                             RECORD KEY           IS 施記－施術和暦年月日
000520                                                     施記－患者コード
000530                             ALTERNATE RECORD KEY IS 施記－患者コード
000540                                                     施記－施術和暦年月日
000550                             FILE STATUS              IS  状態キー
000560                             LOCK        MODE         IS  AUTOMATIC.
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
000840     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
000850                             ORGANIZATION             IS  INDEXED
000860                             ACCESS MODE              IS  DYNAMIC
000870                             RECORD KEY               IS 負－施術和暦年月
000880                                                         負－患者コード
000890                             ALTERNATE RECORD KEY     IS 負－患者コード
000900                                                         負－施術和暦年月
000910                             FILE STATUS              IS  状態キー
000920                             LOCK        MODE         IS  AUTOMATIC.
000921     SELECT  負傷原因Ｆ      ASSIGN      TO        HUGEINL
000922                             ORGANIZATION             IS  INDEXED
000923                             ACCESS MODE              IS  DYNAMIC
000924                             RECORD KEY               IS  負原－区分コード
000925                                                          負原－負傷原因コード
000926                             FILE STATUS              IS  状態キー
000927                             LOCK        MODE         IS  AUTOMATIC.
000930     SELECT  市町村マスタ    ASSIGN      TO        SITYOSNL
000940                             ORGANIZATION             IS  INDEXED
000950                             ACCESS MODE              IS  DYNAMIC
000960                             RECORD KEY               IS  市－公費種別
000970                                                          市－市町村番号
000980                             ALTERNATE RECORD KEY     IS  市－公費種別
000990                                                          市－市町村名称
001000                                                          市－市町村番号
001010                             FILE STATUS              IS  状態キー
001020                             LOCK        MODE         IS  AUTOMATIC.
000130     SELECT  レセプトＦ      ASSIGN      TO        RECEPTL
000140                             ORGANIZATION             IS  INDEXED
000150                             ACCESS MODE              IS  DYNAMIC
000160                             RECORD KEY               IS  レセ－施術和暦年月
000170                                                          レセ－患者コード
000180                                                          レセ－レセ種別
000190                             ALTERNATE RECORD KEY     IS  レセ－患者コード
000200                                                          レセ－施術和暦年月
000210                                                          レセ－レセ種別
000220                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
000230                                                          レセ－施術和暦年月
000240                                                          レセ－患者コード
000250                                                          レセ－レセ種別
000260                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
000270                                                          レセ－レセ種別
000280                                                          レセ－請求保険者番号
000290                                                          レセ－患者コード
000300                                                          レセ－施術和暦年月
000310                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
000320                                                          レセ－請求保険者番号
000330                                                          レセ－患者コード
000340                                                          レセ－レセ種別
000350                                                          レセ－施術和暦年月
000360                             FILE STATUS              IS  状態キー
000370                             LOCK        MODE         IS  AUTOMATIC.
000611     SELECT  会計データＦ    ASSIGN      TO        KAIKEIL
000612                             ORGANIZATION             IS  INDEXED
000613                             ACCESS MODE              IS  DYNAMIC
000089                             RECORD KEY               IS  会－施術和暦年月日
000090                                                          会－患者コード
000092                             ALTERNATE RECORD KEY     IS  会－患者コード
000093                                                          会－施術和暦年月日
000621                             FILE STATUS              IS  状態キー
000622                             LOCK        MODE         IS  AUTOMATIC.
000500     SELECT  部位マスタ      ASSIGN      TO        BUICODEL
000510                             ORGANIZATION             IS  INDEXED
000520                             ACCESS MODE              IS  DYNAMIC
000530                             RECORD KEY               IS  部－部位コード
000540                             FILE STATUS              IS  状態キー
000550                             LOCK        MODE         IS  AUTOMATIC.
001113     SELECT  長期継続者Ｆ    ASSIGN      TO        CHOKEIL
001120                             ORGANIZATION             IS INDEXED
001130                             ACCESS MODE              IS DYNAMIC
001140                             RECORD KEY               IS 長継－施術和暦年月
001150                                                         長継－患者コード
001160                             ALTERNATE RECORD KEY     IS 長継－患者コード
001170                                                         長継－施術和暦年月
001180                             FILE STATUS              IS 状態キー
001190                             LOCK      MODE           IS AUTOMATIC.
001090     SELECT  作業ファイル１  ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1011L.DAT"
001100                             ORGANIZATION             IS  SEQUENTIAL
001110                             ACCESS                   IS  SEQUENTIAL
001120                             FILE        STATUS       IS  状態キー
001130                             LOCK        MODE         IS  AUTOMATIC.
001520     SELECT 作業ファイル２   ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1012L.DAT"
001530                             ORGANIZATION             IS  INDEXED
001540                             ACCESS MODE              IS  DYNAMIC
000861                             RECORD      KEY          IS  作２－保険者区分
000952                             FILE STATUS              IS  状態キー
001600                             LOCK        MODE         IS  AUTOMATIC.
001520     SELECT 作業ファイル３   ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1013L.DAT"
001530                             ORGANIZATION             IS  INDEXED
001540                             ACCESS MODE              IS  DYNAMIC
000861                             RECORD      KEY          IS  作３－番号
000952                             FILE STATUS              IS  状態キー
001600                             LOCK        MODE         IS  AUTOMATIC.
001520     SELECT 作業ファイル４   ASSIGN      TO "C:\MAKISHISYS\YAWOBJ\TEMP\W1014L.DAT"
001530                             ORGANIZATION             IS  INDEXED
001540                             ACCESS MODE              IS  DYNAMIC
000861                             RECORD      KEY          IS  作４－施術和暦年月
                                                                作４－患者コード
                                                                作４－レセ種別
000310                             ALTERNATE RECORD KEY     IS  作４－レコード区分
                                                                作４－提出区分
                                                                作４－保険者番号
                                                                作４－施術和暦年月
                                                                作４－患者コード
                                                                作４－レセ種別
000952                             FILE STATUS              IS  状態キー
001600                             LOCK        MODE         IS  AUTOMATIC.
001224******************************************************************
001230*                      DATA DIVISION                             *
001240******************************************************************
001250 DATA                    DIVISION.
001260 FILE                    SECTION.
001261*                           ［ＲＬ＝  ２５６］
001262 FD  制御情報マスタ          BLOCK   CONTAINS   1   RECORDS.
001263     COPY SEIGYO          OF  XFDLIB  JOINING   制   AS  PREFIX.
001270*                           ［ＲＬ＝  １２８］
001280 FD  元号マスタ          BLOCK   CONTAINS   1   RECORDS.
001290     COPY GENGOU          OF  XFDLIB  JOINING   元   AS  PREFIX.
001300*                           ［ＲＬ＝  １２８］
001310 FD  名称マスタ          BLOCK   CONTAINS   1   RECORDS.
001320     COPY MEISYO          OF  XFDLIB  JOINING   名   AS  PREFIX.
001330*
001340 FD  施術所情報マスタ    BLOCK   CONTAINS   1   RECORDS.
001350     COPY SEJOHO         OF  XFDLIB  JOINING   施情   AS  PREFIX.
001360*                           ［ＲＬ＝  ２５６］
001370 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
001380     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
001390*                           ［ＲＬ＝  ３２０］
001400 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
001410     COPY JUSINJ          OF  XFDLIB  JOINING   受   AS  PREFIX.
001420*                           ［ＲＬ＝  １２８］
001430 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
001440     COPY HUSYOU          OF  XFDLIB  JOINING   負   AS  PREFIX.
001441*                           ［ＲＬ＝  １２８］
001442 FD  負傷原因Ｆ         BLOCK   CONTAINS   1   RECORDS.
001443     COPY HUGEIN          OF  XFDLIB  JOINING   負原   AS  PREFIX.
001450*                           ［ＲＬ＝  ２５６］
001460 FD  市町村マスタ          BLOCK   CONTAINS   1   RECORDS.
001470     COPY SITYOSN        OF  XFDLIB  JOINING   市   AS  PREFIX.
      *                          ［ＲＬ＝  １５３６］
       FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
           COPY RECEPT          OF  XFDLIB  JOINING   レセ  AS  PREFIX.
000981*                           ［ＲＬ＝  ５１２］
000982 FD  会計データＦ        BLOCK   CONTAINS   1   RECORDS.
000983     COPY KAIKEI          OF  XFDLIB  JOINING   会   AS  PREFIX.
001580*                           ［ＲＬ＝  １２８］
001590 FD  部位マスタ          BLOCK   CONTAINS   1   RECORDS.
001600     COPY BUICODE         OF  XFDLIB  JOINING   部   AS  PREFIX.
003410*                           ［ＲＬ＝  １２８］
003420 FD  長期継続者Ｆ        BLOCK   CONTAINS   1   RECORDS.
003430     COPY CHOKEI     OF  XFDLIB  JOINING   長継   AS  PREFIX.
001510**
001520 FD 作業ファイル１ RECORD  CONTAINS 20 CHARACTERS.
001530 01 作１－レコード.
           03 作１－レコードデータ.
               05 作１－レコード区分             PIC 9(2).
               05 作１－会員番号                 PIC 9(5).
               05 作１－請求年月                 PIC 9(6).
               05 作１－診療年月                 PIC 9(6).
               05 作１－レコードＩＤ             PIC X(1).
002352*
001520 FD 作業ファイル２ RECORD  CONTAINS 24 CHARACTERS.
001530 01 作２－レコード.
           03 作２－レコードデータ.
               05 作２－レコード区分             PIC 9(2).
               05 作２－保険者区分               PIC 9(2).
               05 作２－施術件数                 PIC 9(3).
               05 作２－施術料金                 PIC 9(7).
               05 作２－返戻件数                 PIC 9(3).
               05 作２－返戻金額                 PIC 9(7).
002352*
001520 FD 作業ファイル３ RECORD  CONTAINS 24 CHARACTERS.
001530 01 作３－レコード.
           03 作３－レコードデータ.
               05 作３－レコード区分             PIC 9(2).
               05 作３－番号                     PIC 9(2).
               05 作３－本人請求件数             PIC 9(3).
               05 作３－本人請求金額             PIC 9(7).
               05 作３－家族請求件数             PIC 9(3).
               05 作３－家族請求金額             PIC 9(7).
002352*
001520* FD 作業ファイル４ RECORD  CONTAINS 1777 CHARACTERS.
001520* FD 作業ファイル４ RECORD  CONTAINS 1784 CHARACTERS.
001520 FD 作業ファイル４ RECORD  CONTAINS 2023 CHARACTERS.
001530 01 作４－レコード.
000510     03 作４－レコードキー.
000520         05 作４－施術和暦年月.
000530             07 作４－施術和暦             PIC 9.
000540             07 作４－施術年月.
000550                 09 作４－施術年           PIC 9(2).
000560                 09 作４－施術月           PIC 9(2).
000570         05 作４－患者コード.
000580             07 作４－患者番号             PIC 9(6).
000590             07 作４－枝番                 PIC X.
               05 作４－レセ種別                 PIC 9(2).
           03 作４－レコードデータ.
               05 作４－レコード区分             PIC 9(2).
               05 作４－提出区分                 PIC 9(1).
               05 作４－保険者番号               PIC X(8).
               05 作４－保険証記号.
                   07 作４－保険証記号Ｎ         PIC N(10).
               05 作４－保険証番号               PIC X(16).
               05 作４－老人医療助成区分         PIC 9(1).
               05 作４－市町村番号               PIC X(8).
               05 作４－受給者番号               PIC X(16).
               05 作４－本人家族区分             PIC 9(1).
               05 作４－医療助成区分             PIC 9(1).
               05 作４－被保険者カナ             PIC X(20).
               05 作４－被保険者氏名.
                   07 作４－被保険者氏名Ｎ       PIC N(16).
               05 作４－患者カナ                 PIC X(20).
               05 作４－患者氏名.
                   07 作４－患者氏名Ｎ           PIC N(16).
               05 作４－患者性別                 PIC 9(1).
               05 作４－患者生年月日             PIC 9(8).
               05 作４－合計金額                 PIC 9(7).
               05 作４－給付割合                 PIC 9(2).
               05 作４－一部負担金               PIC 9(6).
               05 作４－請求金額                 PIC 9(6).
               05 作４－診療年月                 PIC 9(6).
               05 作４－部位数                   PIC 9(1).
               05 作４－実日数                   PIC 9(3).
               05 作４－端末区分                 PIC 9(2).
               05 作４－給付分類                 PIC X(1).
               05 作４－新規区分                 PIC 9(1).
               05 作４－継続区分                 PIC 9(1).
               05 作４－初検回数                 PIC 9(1).
               05 作４－初検時間外加算回数       PIC 9(1).
               05 作４－初検休日加算回数         PIC 9(1).
               05 作４－初検深夜加算回数         PIC 9(1).
               05 作４－再検回数                 PIC 9(1).
               05 作４－往療距離                 PIC 9(3).
               05 作４－往療回数                 PIC 9(2).
               05 作４－夜間加算往療回数         PIC 9(1).
               05 作４－難路加算往療回数         PIC 9(2).
               05 作４－暴風雨雪加算往療回数     PIC 9(2).
000561         05 作４－金属副子大回数           PIC 9.
000562         05 作４－金属副子中回数           PIC 9.
000563         05 作４－金属副子小回数           PIC 9.
000564         05 作４－情報提供料回数           PIC 9.
000566         05 作４－負傷部位データ  OCCURS 8.
000567             07 作４－負傷区分             PIC 9.
                   07 作４－会部位コード.
                       09 作４－負傷名コード     PIC 9(3).
                       09 作４－細部位コード     PIC 9(2).
                       09 作４－左右区分         PIC 9(1).
000568             07 作４－負傷名               PIC N(16).
000569             07 作４－初回処置回数         PIC 9.
                   07 作４－負傷原因.
                       09 作４－負傷原因Ｎ       PIC N(20).
                   07 作４－長期理由.
                       09 作４－長期理由Ｎ       PIC N(30).
000570             07 作４－負傷年月日           PIC 9(8).
000571             07 作４－初検年月日           PIC 9(8).
000572             07 作４－施術開始年月日       PIC 9(8).
000573             07 作４－施術終了年月日       PIC 9(8).
000574             07 作４－部位実日数           PIC 9(2).
000575             07 作４－転帰区分             PIC 9.
000576             07 作４－後療回数             PIC 9(2).
000577             07 作４－冷罨法回数           PIC 9.
000578             07 作４－温罨法回数           PIC 9(2).
000579             07 作４－電療回数             PIC 9(2).
000580             07 作４－多部位逓減区分       PIC 9.
000581             07 作４－長期逓減区分         PIC 9.
                   07 作４－当部位費用額         PIC 9(6).
               05 作４－相談支援回数             PIC 9.
      */金属副子運動後療追加/20180612
               05 作４－金属副子回数             PIC 9.
               05 作４－運動後療料回数           PIC 9.
               05 作４－運動後療料               PIC 9(5).
      */明細書発行以降追加/20221117
               05 作４－明細書発行回数           PIC 9.
               05 作４－明細書発行料             PIC 9(3).
               05 作４－明細書発行月日           PIC 9(4).
002210         05 作４－日.
002210             07 作４－施術日               PIC X(1) OCCURS 31.
002210         05 作４－初検料                   PIC 9(5).
002210         05 作４－初検加算                 PIC 9(5).
002210         05 作４－相談支援料               PIC 9(5).
002210         05 作４－再検料                   PIC 9(5).
002210         05 作４－往療料                   PIC 9(5).
002210         05 作４－往療加算                 PIC 9(5).
000561         05 作４－金属副子                 PIC 9(5).
002210         05 作４－情報提供料               PIC 9(5).
002760         05 作４－部位データ  OCCURS 8.
                   07 作４－後療料               PIC 9(5).
                   07 作４－冷罨法料             PIC 9(5).
                   07 作４－温罨法料             PIC 9(5).
                   07 作４－電療料               PIC 9(5).
002353*----------------------------------------------------------------*
002354******************************************************************
002360*                WORKING-STORAGE SECTION                         *
002370******************************************************************
002380 WORKING-STORAGE         SECTION.
002390 01 キー入力                           PIC X    VALUE SPACE.
002400 01 状態キー                           PIC X(2) VALUE SPACE.
002410 01 初検フラグ                         PIC X(3) VALUE SPACE.
002420 01 終了フラグ                         PIC X(3) VALUE SPACE.
002430 01 終了フラグ２                       PIC X(3) VALUE SPACE.
001270 01 終了フラグ４                       PIC X(3) VALUE SPACE.
002440 01 実行キーＷ                         PIC X(3) VALUE SPACE.
002450 01 施術記録有Ｗ                       PIC X(3) VALUE SPACE.
002460 01 ファイル名                         PIC N(8) VALUE SPACE.
002470*
002480 01 保険種別ＷＲ                       PIC 9(2) VALUE ZERO.
002490 01 患者コードＷＲ.
002500    03 患者番号ＷＲ                    PIC 9(6) VALUE ZERO.
002510    03 枝番ＷＲ                        PIC X    VALUE SPACE.
002520*
002530 01 印刷形式ＷＲ                       PIC 9    VALUE ZERO.
002540 01 保険者番号ＷＲ                     PIC X(10) VALUE SPACE.
002550 01 レセプト種類ＷＲ                   PIC X(4) VALUE SPACE.
002560 01 本人家族区分ＷＲ                   PIC 9    VALUE ZERO.
002570 01 続柄Ｗ                             PIC N(2) VALUE SPACE.
002580 01 施術和暦年月ＷＲ.
002590    03 施術和暦ＷＲ                    PIC 9    VALUE ZERO.
002600    03 施術年ＷＲ                      PIC 9(2) VALUE ZERO.
002610    03 施術月ＷＲ                      PIC 9(2) VALUE ZERO.
002620 01 請求和暦年月ＷＲ.
002630    03 請求和暦ＷＲ                    PIC 9    VALUE ZERO.
002640    03 請求年ＷＲ                      PIC 9(2) VALUE ZERO.
002650    03 請求月ＷＲ                      PIC 9(2) VALUE ZERO.
002660**
002670 01 連番Ｗ                             PIC 9(4) VALUE ZERO.
002680 01 助成フラグ                         PIC X(3) VALUE SPACE.
002690 01 負傷名称Ｗ                         PIC N(6)  VALUE SPACE.
002700 01 負傷種別変換前Ｗ                   PIC 9(2)  VALUE ZERO.
002710 01 負傷種別変換後Ｗ                   PIC 9     VALUE ZERO.
002720 01 転帰変換前Ｗ                       PIC 9     VALUE ZERO.
002730 01 転帰変換後Ｗ                       PIC 9     VALUE ZERO.
002740**
002750 01 部位ＣＮＴ                         PIC 9     VALUE ZERO.
002760 01 カウンタ                           PIC 9(2)  VALUE ZERO.
002770 01 カウンタ２                         PIC 9(3)  VALUE ZERO.
002780 01 カウンタ３                         PIC 9(2)  VALUE ZERO.
002790 01 全角空白                           PIC X(2)  VALUE X"8140".
002800 01 半角空白                           PIC X(2)  VALUE X"2020".
002810**
       01 氏名Ｗ.
          03 全角氏名Ｗ                      PIC N(10) VALUE SPACE.
002820** エラーメッセージ用
002830 01 エラーメッセージＷ.
002840    03 エラー患者コードＷ              PIC X(7) VALUE SPACE.
002850    03 エラー区切りＷ                  PIC X(1) VALUE SPACE.
002860    03 エラー保険種別Ｗ                PIC X(2) VALUE SPACE.
002870    03 FILLER                          PIC X(10) VALUE SPACE.
002880** 保険者番号右詰め用
002890 01 保険者番号ＷＴ.
002900    03 保険者番号左詰めＷ.
002910      05 保険者番号左詰めＷ１          PIC X OCCURS 8 VALUE SPACE.
002920    03 保険者番号右詰めＷ.
002930      05 保険者番号右詰めＷ１          PIC X OCCURS 8 VALUE ZERO.
002940    03 保険者番号数字Ｗ                PIC 9(8)  VALUE ZERO.
002950    03 保険者番号Ｗ                    PIC X(8)  VALUE SPACE.
002960** 会員番号右詰め用
002970 01 会員番号ＷＴ.
002980    03 会員番号左詰めＷ.
002990      05 会員番号左詰めＷ１            PIC X OCCURS 7 VALUE SPACE.
003000    03 会員番号右詰めＷ.
003010      05 会員番号右詰めＷ１            PIC X OCCURS 7 VALUE ZERO.
003020    03 会員番号数字Ｗ                  PIC 9(7)  VALUE ZERO.
003030    03 会員番号Ｗ                      PIC X(7)  VALUE SPACE.
003040** 西暦日付ワーク用
003050 01 西暦年月Ｗ.
003060    03 西暦年Ｗ                        PIC 9(4) VALUE ZERO.
003070    03 西暦月Ｗ                        PIC 9(2) VALUE ZERO.
003080** 西暦請求年月用
003090 01 西暦請求年月Ｗ.
003100    03 西暦請求年Ｗ                    PIC 9(4) VALUE ZERO.
003110    03 西暦請求月Ｗ                    PIC 9(2) VALUE ZERO.
003120** 西暦施術年月用
003130 01 西暦施術年月Ｗ.
003140    03 西暦施術年Ｗ                    PIC 9(4) VALUE ZERO.
003150    03 西暦施術月Ｗ                    PIC 9(2) VALUE ZERO.
003080** 西暦提出年月用
003090 01 西暦提出年月Ｗ.
003100    03 西暦提出年Ｗ                    PIC 9(4) VALUE ZERO.
003110    03 西暦提出月Ｗ                    PIC 9(2) VALUE ZERO.

003160** 記号左詰め用
003170 01 記号ＷＴ.
003180    03 記号元Ｗ.
003190      05 記号元Ｗ１                    PIC N OCCURS 12 VALUE SPACE.
003200    03 記号左詰めＷ.
003210      05 記号左詰めＷ１                PIC N OCCURS 12 VALUE SPACE.
003180    03 記号元ＸＷ.
003190      05 記号元ＸＷ１                  PIC X OCCURS 24 VALUE SPACE.
003200    03 記号左詰めＸＷ.
003210      05 記号左詰めＸＷ１              PIC X OCCURS 24 VALUE SPACE.
003220    03 記号Ｗ.
003230      05 記号ＮＷ                      PIC N(12) VALUE SPACE.
003240    03 記号ＰＷ.
003250      05 記号ＰＮＷ                    PIC X(24) VALUE SPACE.
003260** 助成負担者番号左詰め用
003270 01 助成番号ＷＴ.
003280    03 助成番号元Ｗ.
003290      05 助成番号元Ｗ１                PIC X OCCURS 10 VALUE SPACE.
003300    03 助成番号左詰めＷ.
003310      05 助成番号左詰めＷ１            PIC X OCCURS 10 VALUE SPACE.
003320    03 助成番号Ｗ                      PIC X(10) VALUE SPACE.
003330*
003340** 西暦年月日ワーク用
003350 01 計算西暦年月日Ｗ.
003360    03 計算西暦年Ｗ                    PIC 9(4) VALUE ZERO.
003370    03 計算西暦月Ｗ                    PIC 9(2) VALUE ZERO.
003380    03 計算西暦日Ｗ                    PIC 9(2) VALUE ZERO.
003390 01 計算和暦年月日Ｗ.
003400    03 計算和暦Ｗ                      PIC 9 VALUE ZERO.
003410    03 計算年Ｗ                        PIC 9(2) VALUE ZERO.
003420    03 計算月Ｗ                        PIC 9(2) VALUE ZERO.
003430    03 計算日Ｗ                        PIC 9(2) VALUE ZERO.
003440** 枝番判定用
003450 01 開始診療日手動区分Ｗ               PIC 9    VALUE ZERO.
003460*
003470* 終了日退避用
003480 01 終了年月日ＷＴ.
003490    03 終了和暦ＷＴ                    PIC 9     VALUE ZERO.
003500    03 終了年ＷＴ                      PIC 9(2)  VALUE ZERO.
003510    03 終了月ＷＴ                      PIC 9(2)  VALUE ZERO.
003520    03 終了日ＷＴ                      PIC 9(2)  VALUE ZERO.
003530* 初検日退避用
003540 01 初検年月日ＷＴ.
003550    03 初検和暦ＷＴ                    PIC 9     VALUE ZERO.
003560    03 初検年ＷＴ                      PIC 9(2)  VALUE ZERO.
003570    03 初検月ＷＴ                      PIC 9(2)  VALUE ZERO.
003580    03 初検日ＷＴ                      PIC 9(2)  VALUE ZERO.
003590*
003591* 連計の金額退避用
003592 01 連計金額Ｗ.
003593    03  費用額Ｗ                   PIC 9(6) VALUE ZERO.
003594    03  負担額Ｗ                   PIC 9(6) VALUE ZERO.
003595    03  請求額Ｗ                   PIC 9(6) VALUE ZERO.
003596    03  費用額老人Ｗ               PIC 9(6) VALUE ZERO.
003597    03  負担額老人Ｗ               PIC 9(6) VALUE ZERO.
003598    03  請求額老人Ｗ               PIC 9(6) VALUE ZERO.
003599    03  費用額助成Ｗ               PIC 9(6) VALUE ZERO.
003600    03  負担額助成Ｗ               PIC 9(5) VALUE ZERO.
003601    03  請求額助成Ｗ               PIC 9(5) VALUE ZERO.
003602    03  負担率Ｗ                   PIC 9(3) VALUE ZERO.
003603*
003604* 負傷原因用
003605 01 負傷原因ＷＴ.
003606    03 負傷原因１ＷＴ                  PIC X(60) VALUE SPACE.
003607    03 負傷原因２ＷＴ                  PIC X(60) VALUE SPACE.
003608    03 負傷原因３ＷＴ                  PIC X(60) VALUE SPACE.
003609    03 負傷原因４ＷＴ                  PIC X(60) VALUE SPACE.
003610    03 負傷原因５ＷＴ                  PIC X(60) VALUE SPACE.
003611    03 負傷原因ナンバーＷＴ.
003612       05 負傷原因ナンバーＷ１         PIC X(2)  OCCURS 9 VALUE SPACE.
003613    03 負傷原因ナンバーＮＷ  REDEFINES 負傷原因ナンバーＷＴ PIC X(18).
003614 01 負傷患者番号ＣＷ                   PIC 9(6)  VALUE ZERO.
003615 01 負傷連番ＣＷ                       PIC 9(4)  VALUE ZERO.
003616 01 負傷原因ＴＢＬ.
003617    03 負傷原因コードＴＢＬ            OCCURS 9.
003618       05 負傷患者番号Ｗ               PIC 9(6)  VALUE ZERO.
003619       05 負傷連番Ｗ                   PIC 9(4)  VALUE ZERO.
003620       05 負傷原因部位Ｗ               PIC 9  OCCURS 9 VALUE ZERO.
003621 01 負傷原因内容Ｗ.
003622    03 負傷原因内容合成Ｗ              PIC X(318) OCCURS 9 VALUE SPACE.
003623    03 負傷原因内容分解ＸＷ.
003624       05 負傷原因内容１ＸＷ           PIC X(74)  VALUE SPACE.
003625       05 負傷原因内容２ＸＷ           PIC X(74)  VALUE SPACE.
003626       05 負傷原因内容３ＸＷ           PIC X(74)  VALUE SPACE.
003650       05 負傷原因内容４ＸＷ           PIC X(96)  VALUE SPACE.
003627*
003628** 負傷原因・長期理由印刷区分用
003629 01 負傷原因印刷区分Ｗ                 PIC 9 VALUE ZERO.
003630 01 長期理由印刷区分Ｗ                 PIC 9 VALUE ZERO.
003631*
003632** 助成レセまとめ用
003633 01 助成レセまとめフラグ               PIC X(3)  VALUE SPACE.
003634*
003635**********************************************************************************
003636*
003637 01 退避項目ＧＷ.
003638   03 レセプト種類Ｗ                   PIC X(4).
003640   03 レセプト種類ＧＷ                 PIC X(4).
003650   03 レセプト種別ＧＷ                 PIC 9(2).
003660*
003670****************
003680* 負傷データＦ *
003690****************
003700 01 負傷情報Ｗ.
003710    03 部位数Ｗ                        PIC 9(1)  VALUE ZERO.
003720    03 部位情報Ｗ  OCCURS   9.
003730       05 部位ＣＮＴＷ                 PIC 9(1)  VALUE ZERO.
003740       05 部位コードＷ.
003750          07 負傷種別Ｗ                PIC 9(2)  VALUE ZERO.
003760          07 部位Ｗ                    PIC 9(2)  VALUE ZERO.
003770          07 左右区分Ｗ                PIC 9(1)  VALUE ZERO.
003780          07 負傷位置番号Ｗ            PIC 9(2)  VALUE ZERO.
003790       05 負傷名Ｗ                     PIC N(18) VALUE SPACE.
003800       05 負傷年月日Ｗ.
003810          07 負傷和暦Ｗ                PIC 9     VALUE ZERO.
003820          07 負傷年Ｗ                  PIC 9(2)  VALUE ZERO.
003830          07 負傷月Ｗ                  PIC 9(2)  VALUE ZERO.
003840          07 負傷日Ｗ                  PIC 9(2)  VALUE ZERO.
003850       05 初検年月日Ｗ.
003860          07 初検和暦Ｗ                PIC 9     VALUE ZERO.
003870          07 初検年Ｗ                  PIC 9(2)  VALUE ZERO.
003880          07 初検月Ｗ                  PIC 9(2)  VALUE ZERO.
003890          07 初検日Ｗ                  PIC 9(2)  VALUE ZERO.
003900       05 開始年月日Ｗ.
003910          07 開始和暦Ｗ                PIC 9     VALUE ZERO.
003920          07 開始年Ｗ                  PIC 9(2)  VALUE ZERO.
003930          07 開始月Ｗ                  PIC 9(2)  VALUE ZERO.
003940          07 開始日Ｗ                  PIC 9(2)  VALUE ZERO.
003950       05 終了年月日Ｗ.
003960          07 終了和暦Ｗ                PIC 9     VALUE ZERO.
003970          07 終了年Ｗ                  PIC 9(2)  VALUE ZERO.
003980          07 終了月Ｗ                  PIC 9(2)  VALUE ZERO.
003990          07 終了日Ｗ                  PIC 9(2)  VALUE ZERO.
004000       05 実日数Ｗ                     PIC 9(2)  VALUE ZERO.
004010       05 初回処置回数Ｗ               PIC 9     VALUE ZERO.
004020       05 転帰区分Ｗ                   PIC 9(1)  VALUE ZERO.
004030    03 新規区分Ｗ                      PIC 9(1)  VALUE ZERO.
004040    03 継続区分Ｗ                      PIC 9(1)  VALUE ZERO.
          03 負傷原因Ｗ OCCURS 27.
004041       05 負傷原因ＷＰ                 PIC X(74) VALUE SPACE.
004050*
004060*********************************************************************
004070*    ************
004080*    * 料金情報 *
004090*    ************
004100*    月毎の料金
004110***********************
004120 01 料金１ＷＲ.
004130   03 初検ＷＲ.
004140      05 初検回数Ｗ                 PIC 9(1)    VALUE ZERO.
004150      05 初検時間外回数Ｗ           PIC 9(1)    VALUE ZERO.
004160      05 初検休日回数Ｗ             PIC 9(1)    VALUE ZERO.
004170      05 初検深夜回数Ｗ             PIC 9(1)    VALUE ZERO.
004180   03 再検回数Ｗ                    PIC 9(1)    VALUE ZERO.
004190   03 往療ＷＲ.
004200      05 往療回数Ｗ                 PIC 9(2)    VALUE ZERO.
004210      05 往療距離Ｗ                 PIC 9(3)V9  VALUE ZERO.
004211      05 往療距離２Ｗ               PIC 9(3)    VALUE ZERO.
004220      05 往療夜間Ｗ                 PIC 9(1)    VALUE ZERO.
004230      05 往療難路Ｗ                 PIC 9(2)    VALUE ZERO.
004240      05 往療暴風Ｗ                 PIC 9(2)    VALUE ZERO.
004250   03 大回数Ｗ                      PIC 9(1)    VALUE ZERO.
004260   03 中回数Ｗ                      PIC 9(1)    VALUE ZERO.
004270   03 小回数Ｗ                      PIC 9(1)    VALUE ZERO.
004280   03 情報提供料回数Ｗ              PIC 9(1)    VALUE ZERO.
004290   03 一部負担金ＷＲ                PIC 9(6)    VALUE ZERO.
004300   03 請求金額ＷＲ                  PIC 9(6)    VALUE ZERO.
004310   03 給付割合ＷＲ                  PIC 9(1)    VALUE ZERO.
004320   03 受給者負担額ＷＲ              PIC 9(6)    VALUE ZERO.
004330   03 助成請求金額ＷＲ              PIC 9(6)    VALUE ZERO.
      */
         03 相談支援回数Ｗ                PIC 9(1)    VALUE ZERO.
         03 施術日ＴＷ.
            05 施術日Ｗ                   PIC 9(1) OCCURS 31 VALUE ZERO.
         03 金属副子回数Ｗ                PIC 9(2)    VALUE ZERO.
      */
         03 初検料Ｗ                      PIC 9(5) VALUE ZERO.
         03 初検加算Ｗ                    PIC 9(5) VALUE ZERO.
         03 相談支援料Ｗ                  PIC 9(5) VALUE ZERO.
         03 再検料Ｗ                      PIC 9(5) VALUE ZERO.
         03 往療料Ｗ                      PIC 9(5) VALUE ZERO.
         03 往療加算Ｗ                    PIC 9(5) VALUE ZERO.
         03 金属副子Ｗ                    PIC 9(5) VALUE ZERO.
         03 情報提供料Ｗ                  PIC 9(5) VALUE ZERO.
      */明細書発行体制加算追加/20221212
         03 明細書発行回数Ｗ              PIC 9(1)    VALUE ZERO.
         03 明細書発行Ｗ                  PIC 9(3)    VALUE ZERO.
         03 明細書発行月日Ｗ.
            05 明細書発行月Ｗ             PIC 9(2)    VALUE ZERO.
            05 明細書発行日Ｗ             PIC 9(2)    VALUE ZERO.
004340*
004350* 負傷部位毎の料金
004360***********************
004370 01 料金２ＷＲ.
004380   03 初回処置ＷＲ    OCCURS   9.
004390      05 初回処置料ＷＲ             PIC 9(5)    VALUE ZERO.
004400   03 多部位区分ＷＲ  OCCURS   9.
004410      05 多部位区分Ｗ               PIC 9(1)    VALUE ZERO.
004420   03 長期区分ＷＲ  OCCURS   9.
004430      05 長期区分Ｗ                 PIC 9(1)    VALUE ZERO.
004440*
004450* 逓減毎の料金
004460***********************
004470 01 料金３ＷＲ.
004480**********
004490* １部位 *
004500**********
004510   03 部位１ＷＲ.
004520      05 後療１ＷＲ.
004530         07 後療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
004540      05 冷罨法１ＷＲ.
004550         07 冷罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
004560      05 温罨法１ＷＲ.
004570         07 温罨法回数１ＷＲ            PIC 9(2)    VALUE ZERO.
004580      05 電療１ＷＲ.
004590         07 電療回数１ＷＲ              PIC 9(2)    VALUE ZERO.
004600**********
004610* ２部位 *
004620**********
004630   03 部位２ＷＲ.
004640      05 後療２ＷＲ.
004650         07 後療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
004660      05 冷罨法２ＷＲ.
004670         07 冷罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
004680      05 温罨法２ＷＲ.
004690         07 温罨法回数２ＷＲ            PIC 9(2)    VALUE ZERO.
004700      05 電療２ＷＲ.
004710         07 電療回数２ＷＲ              PIC 9(2)    VALUE ZERO.
004720******************
004730* ３部位／８割 *
004740******************
004750   03 部位３８ＷＲ.
004760      05 後療３８ＷＲ.
004770         07 後療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
004780      05 冷罨法３８ＷＲ.
004790         07 冷罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
004800      05 温罨法３８ＷＲ.
004810         07 温罨法回数３８ＷＲ            PIC 9(2)  VALUE ZERO.
004820      05 電療３８ＷＲ.
004830         07 電療回数３８ＷＲ              PIC 9(2)  VALUE ZERO.
004840******************
004850* ３部位／１０割 *
004860******************
004870   03 部位３０ＷＲ.
004880      05 後療３０ＷＲ.
004890         07 後療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
004900      05 冷罨法３０ＷＲ.
004910         07 冷罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
004920      05 温罨法３０ＷＲ.
004930         07 温罨法回数３０ＷＲ            PIC 9(2)  VALUE ZERO.
004940      05 電療３０ＷＲ.
004950         07 電療回数３０ＷＲ              PIC 9(2)  VALUE ZERO.
004960******************
004970* ３部位／合計　 *
004980******************
004990   03 部位３ＷＲ.
005000      05 後療３ＷＲ.
005010         07 後療回数３ＷＲ                PIC 9(2)  VALUE ZERO.
005020      05 冷罨法３ＷＲ.
005030         07 冷罨法回数３ＷＲ              PIC 9(2)  VALUE ZERO.
005040      05 温罨法３ＷＲ.
005050         07 温罨法回数３ＷＲ              PIC 9(2)  VALUE ZERO.
005060      05 電療３ＷＲ.
005070         07 電療回数３ＷＲ                PIC 9(2)  VALUE ZERO.
005080****************
005090* ４部位／５割 *
005100****************
005110   03 部位４５ＷＲ.
005120      05 後療４５ＷＲ.
005130         07 後療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005140      05 冷罨法４５ＷＲ.
005150         07 冷罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005160      05 温罨法４５ＷＲ.
005170         07 温罨法回数４５ＷＲ            PIC 9(2)  VALUE ZERO.
005180      05 電療４５ＷＲ.
005190         07 電療回数４５ＷＲ              PIC 9(2)  VALUE ZERO.
005200****************
005210* ４部位／８割 *
005220****************
005230   03 部位４８ＷＲ.
005240      05 後療４８ＷＲ.
005250         07 後療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
005260      05 冷罨法４８ＷＲ.
005270         07 冷罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
005280      05 温罨法４８ＷＲ.
005290         07 温罨法回数４８ＷＲ            PIC 9(2)  VALUE ZERO.
005300      05 電療４８ＷＲ.
005310         07 電療回数４８ＷＲ              PIC 9(2)  VALUE ZERO.
005320******************
005330* ４部位／１０割 *
005340******************
005350   03 部位４０ＷＲ.
005360      05 後療４０ＷＲ.
005370         07 後療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
005380      05 冷罨法４０ＷＲ.
005390         07 冷罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
005400      05 温罨法４０ＷＲ.
005410         07 温罨法回数４０ＷＲ            PIC 9(2)  VALUE ZERO.
005420      05 電療４０ＷＲ.
005430         07 電療回数４０ＷＲ              PIC 9(2)  VALUE ZERO.
005440******************
005450* ４部位／合計　 *
005460******************
005470   03 部位４ＷＲ.
005480      05 後療４ＷＲ.
005490         07 後療回数４ＷＲ                PIC 9(2)  VALUE ZERO.
005500      05 冷罨法４ＷＲ.
005510         07 冷罨法回数４ＷＲ              PIC 9(2)  VALUE ZERO.
005520      05 温罨法４ＷＲ.
005530         07 温罨法回数４ＷＲ              PIC 9(2)  VALUE ZERO.
005540      05 電療４ＷＲ.
005550         07 電療回数４ＷＲ                PIC 9(2)  VALUE ZERO.
005560********************
005570* ５部位／２．５割 *
005580********************
005590   03 部位５２ＷＲ.
005600      05 後療５２ＷＲ.
005610         07 後療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
005620      05 冷罨法５２ＷＲ.
005630         07 冷罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
005640      05 温罨法５２ＷＲ.
005650         07 温罨法回数５２ＷＲ            PIC 9(2)  VALUE ZERO.
005660      05 電療５２ＷＲ.
005670         07 電療回数５２ＷＲ              PIC 9(2)  VALUE ZERO.
005680****************
005690* ５部位／５割 *
005700****************
005710   03 部位５５ＷＲ.
005720      05 後療５５ＷＲ.
005730         07 後療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
005740      05 冷罨法５５ＷＲ.
005750         07 冷罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
005760      05 温罨法５５ＷＲ.
005770         07 温罨法回数５５ＷＲ            PIC 9(2)  VALUE ZERO.
005780      05 電療５５ＷＲ.
005790         07 電療回数５５ＷＲ              PIC 9(2)  VALUE ZERO.
005800****************
005810* ５部位／８割 *
005820****************
005830   03 部位５８ＷＲ.
005840      05 後療５８ＷＲ.
005850         07 後療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
005860      05 冷罨法５８ＷＲ.
005870         07 冷罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
005880      05 温罨法５８ＷＲ.
005890         07 温罨法回数５８ＷＲ            PIC 9(2)  VALUE ZERO.
005900      05 電療５８ＷＲ.
005910         07 電療回数５８ＷＲ              PIC 9(2)  VALUE ZERO.
005920******************
005930* ５部位／１０割 *
005940******************
005950   03 部位５０ＷＲ.
005960      05 後療５０ＷＲ.
005970         07 後療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
005980      05 冷罨法５０ＷＲ.
005990         07 冷罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
006000      05 温罨法５０ＷＲ.
006010         07 温罨法回数５０ＷＲ            PIC 9(2)  VALUE ZERO.
006020      05 電療５０ＷＲ.
006030         07 電療回数５０ＷＲ              PIC 9(2)  VALUE ZERO.
006040******************
006050* ５部位／合計　 *
006060******************
006070   03 部位５ＷＲ.
006080      05 後療５ＷＲ.
006090         07 後療回数５ＷＲ                PIC 9(2)  VALUE ZERO.
006100      05 冷罨法５ＷＲ.
006110         07 冷罨法回数５ＷＲ              PIC 9(2)  VALUE ZERO.
006120      05 温罨法５ＷＲ.
006130         07 温罨法回数５ＷＲ              PIC 9(2)  VALUE ZERO.
006140      05 電療５ＷＲ.
006150         07 電療回数５ＷＲ                PIC 9(2)  VALUE ZERO.
006160*
006170*****************************************************************
006180 01 計算機西暦年Ｗ                     PIC 9(2).
006190* 日付ＷＯＲＫ
006200 01 計算機西暦.
006210    03 計算機西暦年                    PIC 9(4).
006220    03 計算機西暦月日                  PIC 9(4).
006230 01 計算機西暦Ｒ REDEFINES 計算機西暦.
006240    03 計算機世紀                      PIC 9(2).
006250    03 計算機日付                      PIC 9(6).
006260    03 計算機日付Ｒ REDEFINES 計算機日付.
006270       05 計算機年月                   PIC 9(4).
006280       05 計算機年月Ｒ REDEFINES 計算機年月.
006290         07 計算機年                   PIC 9(2).
006300         07 計算機月                   PIC 9(2).
006310       05 計算機日                     PIC 9(2).
006320*
      * C 連携用
       01  文字１Ｗ        PIC X(4096).
       01  文字２Ｗ        PIC X(512).
       01  プログラム名Ｗ  PIC X(8)  VALUE "strmoji2".
      *
       01 複合プログラム名Ｗ     PIC X(8) VALUE "MOJI2".
      *
006330******************************************************************
006340*                          連結項目                              *
006350******************************************************************
006360*
006370********************
006380* メッセージ表示キー *
006390********************
006400 01 連メ－キー IS EXTERNAL.
006410    03  連メ－メッセージ               PIC N(20).
006420*
006430 01 連メ３－キー IS EXTERNAL.
006440    03  連メ３－メッセージ             PIC N(20).
006450    03  連メ３－メッセージ１           PIC X(20).
006460*
006470****************
006480* 画面入力情報 *
006490****************
002278* 01 連入－画面情報ＹＪＢ５８０ IS EXTERNAL.
002279*    03 連入－請求和暦年月.
002280*       05 連入－請求和暦               PIC 9.
002281*       05 連入－請求年月.
002282*         07 連入－請求年               PIC 9(2).
002283*         07 連入－請求月               PIC 9(2).
       01 連入－画面情報ＹＡＩ５８０ IS EXTERNAL.
          03 連入－請求和暦年月.
             05 連入－請求和暦               PIC 9(1).
             05 連入－請求年月.
                07 連入－請求年              PIC 9(2).
                07 連入－請求月              PIC 9(2).
          03 連入－作成和暦年月日.
             05 連入－作成和暦年月.
                07 連入－作成和暦            PIC 9(1).
                07 連入－作成年              PIC 9(2).
                07 連入－作成月              PIC 9(2).
             05 連入－作成日                 PIC 9(2).
006560*
009851************************
009852* 長期理由文セット     *
009853************************
009854 01 連長文－キー IS EXTERNAL.
009855    03 連長文－施術年月.
009856       05 連長文－施術和暦               PIC 9.
009857       05 連長文－施術年                 PIC 9(2).
009858       05 連長文－施術月                 PIC 9(2).
009859    03  連長文－患者コード.
009860       05 連長文－患者番号               PIC 9(6).
009861       05 連長文－枝番                   PIC X.
009862    03 連長文－文桁数                    PIC 9(2).
009863    03 連長文－理由文                    PIC N(63) OCCURS 15.
009864*
009865************************
009866* 助成レセまとめ
009867************************
009868 01 連レセまとめ－キー IS EXTERNAL.
009869    03 連レセまとめ－施術和暦年月.
009870       05 連レセまとめ－施術和暦               PIC 9.
009871       05 連レセまとめ－施術年月.
009872          07 連レセまとめ－施術年              PIC 9(2).
009873          07 連レセまとめ－施術月              PIC 9(2).
009874    03 連レセまとめ－患者コード.
009875       05 連レセまとめ－患者番号               PIC 9(6).
009876       05 連レセまとめ－枝番                   PIC X(1).
009877**-------------------------------------------------------**
009878*   1:助成レセプトなしの本体まとめの判定
009879*   2:横浜・川崎用の社保助成レセかの判定
009880    03 連レセまとめ－判定区分                  PIC 9.
009881**-------------------------------------------------------**
009882*  / OUT /　 0:対象外、1:対象
009883    03 連レセまとめ－判定結果                  PIC 9.
009884**
009885*
      * 暗号複合用
       01 連暗号複合－暗号情報 IS EXTERNAL.
          03 連暗号複合－入力情報.
             05 連暗号複合－記号               PIC X(24).
             05 連暗号複合－番号               PIC X(30).
             05 連暗号複合－暗号化項目.
               07 連暗号複合－暗号患者番号     PIC X(6).
               07 連暗号複合－暗号判定記号     PIC X.
               07 連暗号複合－暗号判定番号     PIC X.
               07 連暗号複合－暗号記号         PIC X(24).
               07 連暗号複合－暗号番号         PIC X(30).
          03 連暗号複合－出力情報.
             05 連暗号複合－複合した記号       PIC X(24).
             05 連暗号複合－複合した番号       PIC X(30).
      * 
009886******************************************************************
009887*                      PROCEDURE  DIVISION                       *
009888******************************************************************
009890 PROCEDURE               DIVISION.
009900************
009910*           *
009920* 初期処理   *
009930*           *
009940************
009950     PERFORM 初期化.
009960     PERFORM 制御情報取得.
009961     PERFORM 施術所情報取得.
009970************
009980*           *
009990* 主処理     *
010000*           *
010010************
010020     PERFORM 作業ファイル１作成.
010020     PERFORM 作業ファイル２作成.
010020     PERFORM 作業ファイル３作成.
010020     PERFORM 作業ファイル４作成.
010030************
010040*           *
010050* 終了処理   *
010060*           *
010070************
010080     PERFORM 終了処理.
010090     MOVE ZERO TO PROGRAM-STATUS.
010100     EXIT PROGRAM.
010110*
010120*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
010130*================================================================*
010140 初期化 SECTION.
010150*
010160     PERFORM ファイルオープン.
010170* 連結項目の待避
010180     MOVE 連入－請求和暦  TO 請求和暦ＷＲ.
010190     MOVE 連入－請求年    TO 請求年ＷＲ.
010200     MOVE 連入－請求月    TO 請求月ＷＲ.
010210*
010220     MOVE ZERO            TO 連番Ｗ.
010230*
010240* 西暦請求年月の取得
010250     MOVE ZERO          TO 西暦年月Ｗ  西暦請求年月Ｗ.
010260     MOVE 請求和暦ＷＲ  TO 元－元号区分.
010270     READ 元号マスタ
010280     NOT INVALID KEY
010290         MOVE 元－開始西暦年 TO 西暦年Ｗ
010300     END-READ.
010310*
010320     IF 西暦年Ｗ = ZERO
010330          MOVE  NC"元号マスタに開始西暦年を登録して下さい" TO 連メ－メッセージ
010340          CALL   "MSG001"
010350          CANCEL "MSG001"
010360          PERFORM ファイル閉鎖
010370          MOVE 99 TO PROGRAM-STATUS
010380          EXIT PROGRAM
010390     ELSE
010400          COMPUTE 西暦年Ｗ = 西暦年Ｗ + 請求年ＷＲ - 1
010410          MOVE 請求月ＷＲ TO 西暦月Ｗ
010420     END-IF.
010430*
010440     MOVE 西暦年月Ｗ   TO  西暦請求年月Ｗ.
010450*
010460*================================================================*
010470 ファイルオープン SECTION.
010480*
010481     OPEN INPUT 制御情報マスタ.
010482         MOVE NC"制御情報" TO ファイル名.
010483         PERFORM オープンチェック.
010490     OPEN INPUT 元号マスタ.
010500         MOVE NC"元号マスタ" TO ファイル名.
010510         PERFORM オープンチェック.
010520     OPEN INPUT 名称マスタ.
010530         MOVE NC"名称マスタ" TO ファイル名.
010540         PERFORM オープンチェック.
010550     OPEN INPUT 施術所情報マスタ
010560         MOVE NC"施情" TO ファイル名.
010570         PERFORM オープンチェック.
010580     OPEN INPUT 施術記録Ｆ.
010590         MOVE NC"施術記録Ｆ" TO ファイル名.
010600         PERFORM オープンチェック.
010610     OPEN INPUT 受診者情報Ｆ.
010620         MOVE NC"受診者情報Ｆ" TO ファイル名.
010630         PERFORM オープンチェック.
010640     OPEN INPUT 負傷データＦ.
010650         MOVE NC"負傷データＦ" TO ファイル名.
010660         PERFORM オープンチェック.
010661     OPEN INPUT 負傷原因Ｆ.
010662         MOVE NC"負傷原因" TO ファイル名.
010663         PERFORM オープンチェック.
010670     OPEN INPUT 市町村マスタ
010680         MOVE NC"市町村" TO ファイル名.
010690         PERFORM オープンチェック.
006630     OPEN INPUT レセプトＦ.
006640         MOVE NC"レセ" TO ファイル名.
006650         PERFORM オープンチェック.
003001     OPEN INPUT 会計データＦ.
003002         MOVE NC"会計" TO ファイル名.
003003         PERFORM オープンチェック.
013960     OPEN INPUT 部位マスタ
013970         MOVE NC"部位" TO ファイル名.
013980         PERFORM オープンチェック.
005350     OPEN INPUT 長期継続者Ｆ.
005360         MOVE NC"長継" TO ファイル名.
005370         PERFORM オープンチェック.
011344     OPEN OUTPUT 作業ファイル１.
011345         MOVE NC"作１" TO ファイル名.
011346         PERFORM オープンチェック.
010730*
010740*================================================================*
010750 オープンチェック SECTION.
010760*
010770     IF 状態キー  NOT =  "00"
010780         DISPLAY ファイル名 NC"Ｆオープンエラー" UPON CONS
010790         DISPLAY NC"状態キー：" 状態キー         UPON CONS
010800         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
010810                                                 UPON CONS
004380*-----------------------------------------*
004390         CALL "actcshm"  WITH C LINKAGE
004400*-----------------------------------------*
010820         ACCEPT  キー入力 FROM CONS
010830         PERFORM ファイル閉鎖
010840         MOVE 99 TO PROGRAM-STATUS
010850         EXIT PROGRAM.
010860*================================================================*
010870 ファイル閉鎖 SECTION.
010880*
010890     CLOSE 制御情報マスタ 元号マスタ   名称マスタ  受診者情報Ｆ
010891           負傷データＦ   負傷原因Ｆ   施術記録Ｆ  施術所情報マスタ
010901           市町村マスタ   会計データＦ レセプトＦ  部位マスタ  
                 長期継続者Ｆ   作業ファイル１.
010910*================================================================*
010920 終了処理 SECTION.
010930*
010940     PERFORM ファイル閉鎖.
010941*================================================================*
010942 エラー表示Ｒ SECTION.
010943*
010944     DISPLAY NC"ファイル読込エラー" ファイル名     UPON CONS.
010945     DISPLAY NC"状態キー" 状態キー                 UPON CONS.
010946     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"                                                                    UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
010947     ACCEPT  キー入力 FROM CONS.
010948     PERFORM ファイル閉鎖.
010949     MOVE 99 TO PROGRAM-STATUS.
010950     EXIT PROGRAM.
010951*================================================================*
010960 エラー表示 SECTION.
010970*
010980     DISPLAY NC"状態キー" 状態キー  UPON CONS.
010990     DISPLAY NC"ファイル書込エラー：" ファイル名   UPON CONS.
011000     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
011010     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
004380*-----------------------------------------*
004390     CALL "actcshm"  WITH C LINKAGE.
004400*-----------------------------------------*
011020     ACCEPT  キー入力 FROM CONS.
011030     PERFORM ファイル閉鎖.
011040     MOVE 99 TO PROGRAM-STATUS.
011050     EXIT PROGRAM.
011051*================================================================*
011052 制御情報取得 SECTION.
011053*
011054     MOVE ZEROS TO 制－制御区分.
011055     READ 制御情報マスタ
011056     NOT INVALID KEY
011058         MOVE 制－レセ負傷原因印刷区分 TO 負傷原因印刷区分Ｗ
011059         MOVE 制－レセ長期理由印刷区分 TO 長期理由印刷区分Ｗ
011062     END-READ.
011063*
011064*================================================================*
011330 作業ファイル１作成 SECTION.
011340*
           MOVE 01 TO 作１－レコード区分.
           PERFORM 施術所情報取得.
           PERFORM 翌月取得.
           MOVE 西暦提出年月Ｗ TO 作１－請求年月.
           MOVE 西暦請求年月Ｗ TO 作１－診療年月.
           MOVE "B"            TO 作１－レコードＩＤ.
           PERFORM 作１ファイル書込.
012457*
012420*================================================================*
       翌月取得 SECTION.
      *
           MOVE 西暦請求年Ｗ TO 西暦提出年Ｗ.
           COMPUTE 西暦提出月Ｗ = 西暦請求月Ｗ + 1
           IF 西暦提出月Ｗ > 12
               MOVE 01 TO 西暦提出月Ｗ
               COMPUTE 西暦提出年Ｗ = 西暦提出年Ｗ + 1
           END-IF.
012420*================================================================*
009510 施術所情報取得 SECTION.
009520*
009530     MOVE ZERO  TO 施情－施術所番号.
009540     READ 施術所情報マスタ
009550     INVALID KEY
009560          MOVE  NC"施術所情報マスタに登録後、実行して下さい" TO 連メ－メッセージ
009570          CALL   "MSG001"
009580          CANCEL "MSG001"
009590          PERFORM ファイル閉鎖
009600          MOVE 99 TO PROGRAM-STATUS
009610          EXIT PROGRAM
008970     NOT INVALID KEY
009060         MOVE 施情－新柔整師番号(5:5)  TO 作１－会員番号
009620     END-READ.
009630*
011064*================================================================*
011330 作業ファイル２作成 SECTION.
011340*
026580     OPEN OUTPUT 作業ファイル２.
026590         MOVE NC"作２" TO ファイル名.
026600         PERFORM オープンチェック.
           CLOSE 作業ファイル２.
026580     OPEN I-O 作業ファイル２.
026590         MOVE NC"作２" TO ファイル名.
026600         PERFORM オープンチェック.
026710*
005420     MOVE 請求和暦ＷＲ  TO レセ－請求和暦.
005430     MOVE 請求年ＷＲ    TO レセ－請求年.
005440     MOVE 請求月ＷＲ    TO レセ－請求月.
005450     MOVE 1             TO レセ－施術和暦.
005460     MOVE ZERO          TO レセ－施術年.
005470     MOVE ZERO          TO レセ－施術月.
005480     MOVE ZERO          TO レセ－患者番号.
005490     MOVE LOW-VALUE     TO レセ－枝番.
005500     MOVE ZERO          TO レセ－レセ種別.
005510     START レセプトＦ   KEY IS >= レセ－請求和暦年月
005520                                  レセ－施術和暦年月
005530                                  レセ－患者コード
005540                                  レセ－レセ種別
026830     END-START.
026840     IF 状態キー = "00"
026850         MOVE SPACE  TO 終了フラグ
005580         PERFORM レセプトＦ読込
005590         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
005600                       ( レセ－請求和暦 NOT = 請求和暦ＷＲ ) OR
005610                       ( レセ－請求年   NOT = 請求年ＷＲ   ) OR
005620                       ( レセ－請求月   NOT = 請求月ＷＲ   )
026910*
026920            PERFORM データチェック
000500*   / 1:一般,2:老人,3:助成,4:労災,5:自賠責,6:自費,7:生保単独,8保険証忘れ /
      */提出Ｆから労災自賠責自費生保忘れを抜く/20180809
      *            IF レセ－レセ種別 = 3
                  IF レセ－レセ種別 = 3 OR 4 OR 5 OR 6 OR 7 OR 8
                      MOVE SPACE TO 実行キーＷ
                  END-IF
026930            IF 実行キーＷ = "YES"
                      PERFORM 保険者区分取得
                      READ 作業ファイル２
                      INVALID KEY
                          INITIALIZE 作２－レコード
                          PERFORM 保険者区分取得
                          PERFORM 作２レコードセット
                          PERFORM 作２ファイル書込
                      NOT INVALID KEY
                          PERFORM 作２レコードセット
                          PERFORM 作２ファイル追加
                      END-READ
005200            END-IF
027850            PERFORM レセプトＦ読込
027860         END-PERFORM
027870     END-IF.
027880     CLOSE 作業ファイル２.
012420*================================================================*
       保険者区分取得 SECTION.
      *
           EVALUATE 受－保険種別
           WHEN 01
               IF 受－保険者番号(1:2) = "23"
                   MOVE 06 TO 作２－保険者区分
               ELSE
                   MOVE 05 TO 作２－保険者区分
               END-IF
      */全国土木建設・全国左官タイル塗装業、全国板金業は組合
               IF 受－保険者番号(1:6) = "133033" OR "133231" OR "133280"
008480             MOVE  02 TO 作２－保険者区分
               END-IF
           WHEN 08
               IF 受－保険者番号(3:2) = "23"
                   MOVE 08 TO 作２－保険者区分
               ELSE
                   MOVE 07 TO 作２－保険者区分
               END-IF
           WHEN 02
               MOVE 01 TO 作２－保険者区分
           WHEN 03
               MOVE 02 TO 作２－保険者区分
           WHEN 04
               MOVE 03 TO 作２－保険者区分
           WHEN 05
               IF 受－保険者番号(3:2) = "23"
                   MOVE 16 TO 作２－保険者区分
               ELSE
                   MOVE 15 TO 作２－保険者区分
               END-IF
           WHEN 70
               MOVE 13 TO 作２－保険者区分
           END-EVALUATE.
012420*================================================================*
       作２レコードセット SECTION.
      *
           MOVE 10 TO 作２－レコード区分.
011490     EVALUATE レセ－請求区分
           WHEN 2
               COMPUTE 作２－返戻件数 = 作２－返戻件数 + 1
               COMPUTE 作２－返戻金額 = 作２－返戻金額 + レセ－合計
           WHEN OTHER
               COMPUTE 作２－施術件数 = 作２－施術件数 + 1
               COMPUTE 作２－施術料金 = 作２－施術料金 + レセ－合計
           END-EVALUATE.
019390*================================================================*
019320 作２ファイル書込 SECTION.
019330*
019340     WRITE 作２－レコード
019350     INVALID KEY
019360         MOVE NC"作２"  TO ファイル名
019370         PERFORM エラー表示
019380     END-WRITE.
019390*================================================================*
019320 作２ファイル追加 SECTION.
019330*
019340     REWRITE 作２－レコード
019350     INVALID KEY
019360         MOVE NC"作２"  TO ファイル名
019370         PERFORM エラー表示
019380     END-REWRITE.
012420*================================================================*
008560 データチェック SECTION.
008570*
008580     MOVE SPACE          TO 実行キーＷ.
019520* *****************************************************************
019530* * レセプトＦの請求対象区分 = 0 の場合データ作成対象としない *
019540* *****************************************************************
019640     IF ( レセ－請求対象区分 NOT = ZERO ) AND
005778        ( レセ－償還払い区分 NOT = 1 )
              IF(レセ－レセ種別 = 3) AND ( レセ－会総括表印刷対象区分 = 1 )
                 CONTINUE
              ELSE
004090           MOVE レセ－施術和暦  TO 受－施術和暦
004100           MOVE レセ－施術年    TO 受－施術年
004110           MOVE レセ－施術月    TO 受－施術月
004120           MOVE レセ－患者番号  TO 受－患者番号
004130           MOVE レセ－枝番      TO 受－枝番
                 READ 受診者情報Ｆ
                 NOT INVALID KEY
019880               MOVE "YES"  TO 実行キーＷ
019950           END-READ
              END-IF
019960     END-IF.
009040*
020480*================================================================*
020490 料金情報取得 SECTION.
020500*
020510***********************************************
020520* 料金データセット                            *
020530***********************************************
020540*    ****************************************************************
020550*    * 料金（月毎）（負傷毎）（逓減毎）については連結項目よりセット *
020560*    ****************************************************************
020570     INITIALIZE 料金１ＷＲ.
020580     INITIALIZE 料金２ＷＲ.
020590     INITIALIZE 料金３ＷＲ.
020600*
025620     MOVE レセ－初検料                 TO 初検料Ｗ.
025730     MOVE レセ－初検加算料             TO 初検加算Ｗ.
025770     MOVE レセ－初検時相談料           TO  相談支援料Ｗ.
025740     MOVE レセ－再検料                 TO  再検料Ｗ.
028240     MOVE レセ－金属副子加算料         TO  金属副子Ｗ.
025990     MOVE レセ－施術情報提供料         TO  情報提供料Ｗ.
      */明細書発行体制加算追加/20221212
           MOVE レセ－明細書発行加算料       TO 明細書発行Ｗ.
           MOVE レセ－明細書発行加算日       TO 明細書発行日Ｗ.
           IF レセ－明細書発行加算料 NOT = ZERO
               MOVE 1                        TO 明細書発行回数Ｗ
               MOVE レセ－施術月             TO 明細書発行月Ｗ
           END-IF.
      *
020620     MOVE レセ－往療回数               TO  往療回数Ｗ.
020621     MOVE レセ－往療距離               TO  往療距離Ｗ.
023360     MOVE レセ－往療料                 TO  往療料Ｗ.
020622* 単位100m
020623     COMPUTE  往療距離２Ｗ  =  往療距離Ｗ * 10.
025780     MOVE レセ－往療加算料             TO  往療加算Ｗ.
020630*
020640********************
020650* 逓減毎料金セット *
020660********************
020670*    **********
020680*    * １部位 *
020690*    **********
020700     MOVE レセ－後療回数１             TO 後療回数１ＷＲ.
020710     MOVE レセ－冷罨法回数１           TO 冷罨法回数１ＷＲ.
020720     MOVE レセ－温罨法回数１           TO 温罨法回数１ＷＲ.
020730     MOVE レセ－電療回数１             TO 電療回数１ＷＲ.
020740     MOVE 0          TO 多部位区分Ｗ(1)
020750     IF レセ－長期逓減率１ NOT = ZERO
020760         MOVE 1      TO 長期区分Ｗ(1)
020770     ELSE
020780         MOVE 0      TO 長期区分Ｗ(1)
020790     END-IF
020800*    **********
020810*    * ２部位 *
020820*    **********
020830     MOVE レセ－後療回数２             TO 後療回数２ＷＲ.
020840     MOVE レセ－冷罨法回数２           TO 冷罨法回数２ＷＲ.
020850     MOVE レセ－温罨法回数２           TO 温罨法回数２ＷＲ.
020860     MOVE レセ－電療回数２             TO 電療回数２ＷＲ.
020870     MOVE 0          TO 多部位区分Ｗ(2)
020880     IF レセ－長期逓減率２ NOT = ZERO
020890         MOVE 1      TO 長期区分Ｗ(2)
020900     ELSE
020910         MOVE 0      TO 長期区分Ｗ(2)
020920     END-IF
020930*    ****************
020940*    * ３部位／８割 *
020950*    ****************
020960     MOVE 0                              TO 長期区分Ｗ(3)
020970     MOVE 0                              TO 長期区分Ｗ(3)
020980     MOVE レセ－後療回数３８             TO 後療回数３８ＷＲ.
020990     MOVE レセ－冷罨法回数３８           TO 冷罨法回数３８ＷＲ.
021000     MOVE レセ－温罨法回数３８           TO 温罨法回数３８ＷＲ.
021010     MOVE レセ－電療回数３８             TO 電療回数３８ＷＲ.
021020     IF レセ－小計３８ NOT = レセ－多部位込小計３８
021030         MOVE 1        TO 多部位区分Ｗ(3)
021040     END-IF
021050     IF レセ－長期逓減率３８ NOT = ZERO
021060         MOVE 1        TO 長期区分Ｗ(3)
021070     END-IF
021080*    ****************
021090*    * ３部位／10割 *
021100*    ****************
021110     MOVE レセ－後療回数３０             TO 後療回数３０ＷＲ.
021120     MOVE レセ－冷罨法回数３０           TO 冷罨法回数３０ＷＲ.
021130     MOVE レセ－温罨法回数３０           TO 温罨法回数３０ＷＲ.
021140     MOVE レセ－電療回数３０             TO 電療回数３０ＷＲ.
021150     IF レセ－長期逓減率３０ NOT = ZERO
021160         MOVE 1        TO 長期区分Ｗ(3)
021170     END-IF
021180*    ****************
021190*    * ３部位／合計 *
021200*    ****************
021210     COMPUTE 後療回数３ＷＲ      = 後療回数３８ＷＲ   + 後療回数３０ＷＲ.
021220     COMPUTE 冷罨法回数３ＷＲ    = 冷罨法回数３８ＷＲ + 冷罨法回数３０ＷＲ.
021230     COMPUTE 温罨法回数３ＷＲ    = 温罨法回数３８ＷＲ + 温罨法回数３０ＷＲ.
021240     COMPUTE 電療回数３ＷＲ      = 電療回数３８ＷＲ   + 電療回数３０ＷＲ.
021250*    ****************
021260*    * ４部位／５割 *
021270*    ****************
021280     MOVE 0                              TO 長期区分Ｗ(4)
021290     MOVE 0                              TO 長期区分Ｗ(4)
021300     MOVE レセ－後療回数４５             TO 後療回数４５ＷＲ.
021310     MOVE レセ－冷罨法回数４５           TO 冷罨法回数４５ＷＲ.
021320     MOVE レセ－温罨法回数４５           TO 温罨法回数４５ＷＲ.
021330     MOVE レセ－電療回数４５             TO 電療回数４５ＷＲ.
021340     IF レセ－小計４５ NOT = レセ－多部位込小計４５
021350         MOVE 1        TO 多部位区分Ｗ(4)
021360     END-IF
021370     IF レセ－長期逓減率４５ NOT = ZERO
021380         MOVE 1        TO 長期区分Ｗ(4)
021390     END-IF
021400*    ****************
021410*    * ４部位／８割 *
021420*    ****************
021430     MOVE レセ－後療回数４８             TO 後療回数４８ＷＲ.
021440     MOVE レセ－冷罨法回数４８           TO 冷罨法回数４８ＷＲ.
021450     MOVE レセ－温罨法回数４８           TO 温罨法回数４８ＷＲ.
021460     MOVE レセ－電療回数４８             TO 電療回数４８ＷＲ.
021470     IF レセ－小計４８ NOT = レセ－多部位込小計４８
021480         MOVE 1        TO 多部位区分Ｗ(4)
021490     END-IF
021500     IF レセ－長期逓減率４８ NOT = ZERO
021510         MOVE 1        TO 長期区分Ｗ(4)
021520     END-IF
021530*    ****************
021540*    * ４部位／10割 *
021550*    ****************
021560     MOVE レセ－後療回数４０             TO 後療回数４０ＷＲ.
021570     MOVE レセ－冷罨法回数４０           TO 冷罨法回数４０ＷＲ.
021580     MOVE レセ－温罨法回数４０           TO 温罨法回数４０ＷＲ.
021590     MOVE レセ－電療回数４０             TO 電療回数４０ＷＲ.
021600     IF レセ－長期逓減率４０ NOT = ZERO
021610         MOVE 1        TO 長期区分Ｗ(4)
021620     END-IF
021630*    ****************
021640*    * ４部位／合計 *
021650*    ****************
021660     COMPUTE 後療回数４ＷＲ      = 後療回数４５ＷＲ   + 後療回数４８ＷＲ   + 後療回数４０ＷＲ.
021670     COMPUTE 冷罨法回数４ＷＲ    = 冷罨法回数４５ＷＲ + 冷罨法回数４８ＷＲ + 冷罨法回数４０ＷＲ.
021680     COMPUTE 温罨法回数４ＷＲ    = 温罨法回数４５ＷＲ + 温罨法回数４８ＷＲ + 温罨法回数４０ＷＲ.
021690     COMPUTE 電療回数４ＷＲ      = 電療回数４５ＷＲ   + 電療回数４８ＷＲ   + 電療回数４０ＷＲ.
021700*    *****************
021710*    * ５部位／2.5割 *
021720*    *****************
021730     MOVE 0                              TO 長期区分Ｗ(5)
021740     MOVE 0                              TO 長期区分Ｗ(5)
021750     MOVE レセ－後療回数５２             TO 後療回数５２ＷＲ.
021760     MOVE レセ－冷罨法回数５２           TO 冷罨法回数５２ＷＲ.
021770     MOVE レセ－温罨法回数５２           TO 温罨法回数５２ＷＲ.
021780     MOVE レセ－電療回数５２             TO 電療回数５２ＷＲ.
021790     IF レセ－小計５２ NOT = レセ－多部位込小計５２
021800         MOVE 1        TO 多部位区分Ｗ(5)
021810     END-IF
021820     IF レセ－長期逓減率５２ NOT = ZERO
021830         MOVE 1        TO 長期区分Ｗ(5)
021840     END-IF
021850*    ****************
021860*    * ５部位／５割 *
021870*    ****************
021880     MOVE レセ－後療回数５５             TO 後療回数５５ＷＲ.
021890     MOVE レセ－冷罨法回数５５           TO 冷罨法回数５５ＷＲ.
021900     MOVE レセ－温罨法回数５５           TO 温罨法回数５５ＷＲ.
021910     MOVE レセ－電療回数５５             TO 電療回数５５ＷＲ.
021920     IF レセ－小計５５ NOT = レセ－多部位込小計５５
021930         MOVE 1        TO 多部位区分Ｗ(5)
021940     END-IF
021950     IF レセ－長期逓減率５５ NOT = ZERO
021960         MOVE 1        TO 長期区分Ｗ(5)
021970     END-IF
021980*    ****************
021990*    * ５部位／８割 *
022000*    ****************
022010     MOVE レセ－後療回数５８             TO 後療回数５８ＷＲ.
022020     MOVE レセ－冷罨法回数５８           TO 冷罨法回数５８ＷＲ.
022030     MOVE レセ－温罨法回数５８           TO 温罨法回数５８ＷＲ.
022040     MOVE レセ－電療回数５８             TO 電療回数５８ＷＲ.
022050     IF レセ－小計５８ NOT = レセ－多部位込小計５８
022060         MOVE 1        TO 多部位区分Ｗ(5)
022070     END-IF
022080     IF レセ－長期逓減率５８ NOT = ZERO
022090         MOVE 1        TO 長期区分Ｗ(5)
022100     END-IF
022110*    ****************
022120*    * ５部位／10割 *
022130*    ****************
022140     MOVE レセ－後療回数５０             TO 後療回数５０ＷＲ.
022150     MOVE レセ－冷罨法回数５０           TO 冷罨法回数５０ＷＲ.
022160     MOVE レセ－温罨法回数５０           TO 温罨法回数５０ＷＲ.
022170     MOVE レセ－電療回数５０             TO 電療回数５０ＷＲ.
022180     IF レセ－長期逓減率５０ NOT = ZERO
022190         MOVE 1        TO 長期区分Ｗ(5)
022200     END-IF
022210*    ****************
022220*    * ５部位／合計 *
022230*    ****************
022240     COMPUTE 後療回数５ＷＲ   = 後療回数５２ＷＲ   + 後療回数５５ＷＲ   +
022250                                後療回数５８ＷＲ   + 後療回数５０ＷＲ.
022260     COMPUTE 冷罨法回数５ＷＲ = 冷罨法回数５２ＷＲ + 冷罨法回数５５ＷＲ +
022270                                冷罨法回数５８ＷＲ + 冷罨法回数５０ＷＲ.
022280     COMPUTE 温罨法回数５ＷＲ = 温罨法回数５２ＷＲ + 温罨法回数５５ＷＲ +
022290                                温罨法回数５８ＷＲ + 温罨法回数５０ＷＲ.
022300     COMPUTE 電療回数５ＷＲ   = 電療回数５２ＷＲ   + 電療回数５５ＷＲ   +
022310                                電療回数５８ＷＲ   + 電療回数５０ＷＲ.
022320*
012900*================================================================*
022340 負傷データ取得 SECTION.
022350*
022360     INITIALIZE 負傷情報Ｗ.
022361*
022370     MOVE 施術和暦ＷＲ       TO 負－施術和暦.
022380     MOVE 施術年ＷＲ         TO 負－施術年.
022390     MOVE 施術月ＷＲ         TO 負－施術月.
022400     MOVE 患者コードＷＲ     TO 負－患者コード.
022410     READ 負傷データＦ
022420     INVALID KEY
022430         CONTINUE
022440     NOT INVALID KEY
022450         MOVE 負－部位数                   TO 部位数Ｗ
022460         PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1
022470                 UNTIL ( 部位ＣＮＴ > 部位数Ｗ )
022480             MOVE 負－負傷種別(部位ＣＮＴ) TO 負傷種別Ｗ(部位ＣＮＴ)
022490             MOVE 負－部位(部位ＣＮＴ)     TO 部位Ｗ(部位ＣＮＴ)
022500             MOVE 負－左右区分(部位ＣＮＴ) TO 左右区分Ｗ(部位ＣＮＴ)
022510             MOVE 負－負傷位置番号(部位ＣＮＴ)
022520                                           TO 負傷位置番号Ｗ(部位ＣＮＴ)
022530* 負傷種別
022540             MOVE SPACE                     TO 負傷名称Ｗ
022550             MOVE 03                        TO 名－区分コード
022560             MOVE 負－負傷種別(部位ＣＮＴ)  TO 名－名称コード
022570             READ 名称マスタ
022580             INVALID KEY
022590                 MOVE SPACE        TO 負傷名称Ｗ
022600             NOT INVALID KEY
022610                 MOVE 名－正式名称 TO 負傷名称Ｗ
022620             END-READ
022630* 部位
022720             STRING レセ－部位名称１(部位ＣＮＴ)  DELIMITED BY SPACE
022730                    負傷名称Ｗ                    DELIMITED BY SPACE
022740                    レセ－部位名称２(部位ＣＮＴ)  DELIMITED BY SPACE
022750                    INTO 負傷名Ｗ(部位ＣＮＴ)
022760             END-STRING
022780*
022790             MOVE 負－負傷和暦(部位ＣＮＴ)   TO 負傷和暦Ｗ(部位ＣＮＴ)
022800             MOVE 負－負傷年(部位ＣＮＴ)   TO 負傷年Ｗ(部位ＣＮＴ)
022810             MOVE 負－負傷月(部位ＣＮＴ)   TO 負傷月Ｗ(部位ＣＮＴ)
022820             MOVE 負－負傷日(部位ＣＮＴ)   TO 負傷日Ｗ(部位ＣＮＴ)
022830             MOVE 負－開始和暦(部位ＣＮＴ)   TO 初検和暦Ｗ(部位ＣＮＴ)
022840             MOVE 負－開始年(部位ＣＮＴ)   TO 初検年Ｗ(部位ＣＮＴ)
022850             MOVE 負－開始月(部位ＣＮＴ)   TO 初検月Ｗ(部位ＣＮＴ)
022860             MOVE 負－開始日(部位ＣＮＴ)   TO 初検日Ｗ(部位ＣＮＴ)
022870             IF 負－転帰区分(部位ＣＮＴ) = 9
022880                 MOVE 99                   TO 終了年Ｗ(部位ＣＮＴ)
022890                 MOVE 99                   TO 終了月Ｗ(部位ＣＮＴ)
022900                 MOVE 99                   TO 終了日Ｗ(部位ＣＮＴ)
022910             ELSE
022920                 MOVE 負－終了和暦(部位ＣＮＴ)   TO 終了和暦Ｗ(部位ＣＮＴ)
022930                 MOVE 負－終了年(部位ＣＮＴ)   TO 終了年Ｗ(部位ＣＮＴ)
022940                 MOVE 負－終了月(部位ＣＮＴ)   TO 終了月Ｗ(部位ＣＮＴ)
022950                 MOVE 負－終了日(部位ＣＮＴ)   TO 終了日Ｗ(部位ＣＮＴ)
022960             END-IF
022970*
022980             MOVE 負－転帰区分(部位ＣＮＴ) TO 転帰区分Ｗ(部位ＣＮＴ)
022990*
023000         END-PERFORM
023010* 新規/継続 チェック
023020         IF レセ－初検料  NOT = ZERO
023030             MOVE 1                   TO 新規区分Ｗ
023040         ELSE
023050             MOVE 1                   TO 継続区分Ｗ
023060         END-IF
023070         PERFORM 初検日以前のデータ判定
023080* 枝番判定用
023090         MOVE 負－開始診療日手動区分 TO  開始診療日手動区分Ｗ
023100*
023110     END-READ.
023120*
023130*================================================================*
023140 初検日以前のデータ判定 SECTION.
023150*
023160*********************************************************************************
023170*  最初の初検日以前の当月中に施術記録レコードがあった時(治癒、中止)は、請求区分の
023180*  継続にもチェックする。(新規と継続の両方)
023190*********************************************************************************
023200** 最初の初検日を取得
023210     MOVE SPACE                 TO 初検フラグ.
023220     MOVE 患者番号ＷＲ          TO 施記－患者番号.
023230     MOVE 枝番ＷＲ              TO 施記－枝番.
023240     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
023250     MOVE 施術年ＷＲ            TO 施記－施術年.
023260     MOVE 施術月ＷＲ            TO 施記－施術月.
023270     MOVE ZERO                  TO 施記－施術日.
023280     START 施術記録Ｆ   KEY IS >= 施記－患者コード
023290                                  施記－施術和暦年月日
023300     END-START.
023310     IF 状態キー = "00"
023320         MOVE ZERO  TO 初検和暦ＷＴ
023330         MOVE ZERO  TO 初検年ＷＴ
023340         MOVE ZERO  TO 初検月ＷＴ
023350         MOVE ZERO  TO 初検日ＷＴ
023360         MOVE SPACE TO 終了フラグ２
023370         PERFORM 施術記録Ｆ読込
023380         PERFORM UNTIL ( 終了フラグ２         = "YES"           ) OR
023390                       ( 施記－患者コード NOT = 患者コードＷＲ  ) OR
023400                       ( 施記－施術和暦   NOT = 施術和暦ＷＲ    ) OR
023410                       ( 施記－施術年     NOT = 施術年ＷＲ      ) OR
023420                       ( 施記－施術月     NOT = 施術月ＷＲ      ) OR
023430                       ( 初検フラグ           = "YES"           ) 
023440               IF  施記－診療区分 = 2
023450                   MOVE 施記－施術和暦           TO 初検和暦ＷＴ
023460                   MOVE 施記－施術年             TO 初検年ＷＴ
023470                   MOVE 施記－施術月             TO 初検月ＷＴ
023480                   MOVE 施記－施術日             TO 初検日ＷＴ
023490                   MOVE "YES"                    TO 初検フラグ
023500               END-IF
023510               PERFORM 施術記録Ｆ読込
023520         END-PERFORM
023530     END-IF.
023540*
023550* 初検日以前のデータ判定
023560     IF 初検フラグ = "YES"
023570        MOVE 患者番号ＷＲ          TO 施記－患者番号
023580        MOVE 枝番ＷＲ              TO 施記－枝番
023590        MOVE 初検和暦ＷＴ          TO 施記－施術和暦
023600        MOVE 初検年ＷＴ            TO 施記－施術年
023610        MOVE 初検月ＷＴ            TO 施記－施術月
023620        MOVE 初検日ＷＴ            TO 施記－施術日
023630        START 施術記録Ｆ   KEY IS <  施記－患者コード
023640                                     施記－施術和暦年月日
023650                                     REVERSED
023660        END-START
023670        IF 状態キー = "00"
023680           MOVE SPACE  TO 終了フラグ２
023690           PERFORM 施術記録Ｆ読込
023700           IF ( 終了フラグ２    = SPACE        ) AND
023710              ( 施記－患者番号  = 患者番号ＷＲ ) AND
023720              ( 施記－枝番      = 枝番ＷＲ     ) AND
023730              ( 施記－施術和暦  = 初検和暦ＷＴ ) AND
023740              ( 施記－施術年    = 初検年ＷＴ   ) AND
023750              ( 施記－施術月    = 初検月ＷＴ   )
023760*  初検日以前の当月中に施術記録レコードがあった時
023770                IF 継続区分Ｗ = ZERO
023780                   MOVE 1    TO 継続区分Ｗ
023790                END-IF
023800           END-IF
023810         END-IF
023820     END-IF.
023830*
023840*================================================================*
023850 施術記録取得 SECTION.
023860*
023870     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 部位数Ｗ
023880         IF ( 施術年ＷＲ = 初検年Ｗ(部位ＣＮＴ) ) AND
023890            ( 施術月ＷＲ = 初検月Ｗ(部位ＣＮＴ) )
023900             MOVE 患者番号ＷＲ          TO 施記－患者番号
023910             MOVE 枝番ＷＲ              TO 施記－枝番
023920             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
023930             MOVE 初検和暦Ｗ(部位ＣＮＴ)  TO 開始和暦Ｗ(部位ＣＮＴ)
023940             MOVE 初検年Ｗ(部位ＣＮＴ)  TO 開始年Ｗ(部位ＣＮＴ) 施記－施術年
023950             MOVE 初検月Ｗ(部位ＣＮＴ)  TO 開始月Ｗ(部位ＣＮＴ) 施記－施術月
023960             MOVE 初検日Ｗ(部位ＣＮＴ)  TO 開始日Ｗ(部位ＣＮＴ) 施記－施術日
023970         ELSE
023980             MOVE 患者番号ＷＲ          TO 施記－患者番号
023990             MOVE 枝番ＷＲ              TO 施記－枝番
024000             MOVE 施術和暦ＷＲ          TO 施記－施術和暦
024010             MOVE 施術年ＷＲ            TO 施記－施術年
024020             MOVE 施術月ＷＲ            TO 施記－施術月
024030             MOVE ZERO                  TO 施記－施術日
024040         END-IF
024050         START 施術記録Ｆ   KEY IS >= 施記－患者コード
024060                                      施記－施術和暦年月日
024070         END-START
024080         IF 状態キー = "00"
024090             MOVE ZERO  TO 実日数Ｗ(部位ＣＮＴ)
024100             MOVE ZERO  TO 初回処置回数Ｗ(部位ＣＮＴ)
024110             MOVE ZERO  TO 終了和暦ＷＴ
024120             MOVE ZERO  TO 終了年ＷＴ
024130             MOVE ZERO  TO 終了月ＷＴ
024140             MOVE ZERO  TO 終了日ＷＴ
024150             MOVE SPACE TO 終了フラグ２
024160             PERFORM 施術記録Ｆ読込
024170             IF  ( 終了フラグ２      = SPACE   ) AND
024180                 ( 施記－患者コード  = 患者コードＷＲ ) AND
024190                 ( 施記－施術和暦    = 施術和暦ＷＲ   ) AND
024200                 ( 施記－施術年      = 施術年ＷＲ     ) AND
024210                 ( 施記－施術月      = 施術月ＷＲ     ) 
024220*
024230*        ************
024240*        * 金属副子 *
024250*        ************
024260             EVALUATE 施記－金属副子区分(部位ＣＮＴ)
024270             WHEN 1
024280                 COMPUTE 大回数Ｗ = 大回数Ｗ + 1
024290             WHEN 2
024300                 COMPUTE 中回数Ｗ = 中回数Ｗ + 1
024310             WHEN 3
024320                 COMPUTE 小回数Ｗ = 小回数Ｗ + 1
024330             END-EVALUATE
024340*        ****************
024350*        * 情報提供回数 *
024360*        ****************
024370             IF 施記－情報提供区分(部位ＣＮＴ) = 1
024380                 COMPUTE 情報提供料回数Ｗ = 情報提供料回数Ｗ + 1
024390             END-IF
024400*        *****************************************************************
024410*        * 開始年月日 ( その部位が当月初検でないか、
024420*                       当月初検でも枝番がある時は、最初の施術日を開始日)*
024430*        *****************************************************************
024440                 IF ( 施術年ＷＲ NOT = 初検年Ｗ(部位ＣＮＴ) ) OR
024450                    ( 施術月ＷＲ NOT = 初検月Ｗ(部位ＣＮＴ) ) OR
024460                    ( 開始診療日手動区分Ｗ = 1 )
024470                     MOVE 施記－施術和暦 TO 開始和暦Ｗ(部位ＣＮＴ)
024480                     MOVE 施記－施術年   TO 開始年Ｗ(部位ＣＮＴ)
024490                     MOVE 施記－施術月   TO 開始月Ｗ(部位ＣＮＴ)
024500                     MOVE 施記－施術日   TO 開始日Ｗ(部位ＣＮＴ)
024510                 END-IF
024520             END-IF
024530             PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
024540                           ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
024550                           ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
024560                           ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
024570                           ( 施記－施術月     NOT = 施術月ＷＲ       ) OR
024580                           ( 施記－施術日         > 終了日Ｗ(部位ＣＮＴ))
024590*               **********
024600*               * 実日数 *
024610*               **********
024620                COMPUTE 実日数Ｗ(部位ＣＮＴ) = 実日数Ｗ(部位ＣＮＴ) + 1
024630                MOVE 施記－施術和暦             TO 終了和暦ＷＴ
024640                MOVE 施記－施術年               TO 終了年ＷＴ
024650                MOVE 施記－施術月               TO 終了月ＷＴ
024660                MOVE 施記－施術日               TO 終了日ＷＴ
024670*            /　初回処置のカウント　/
024680                IF 施記－整復施療区分(部位ＣＮＴ) = 1
024690                    COMPUTE 初回処置回数Ｗ(部位ＣＮＴ) = 初回処置回数Ｗ(部位ＣＮＴ) + 1
024700                END-IF
024710*
024720                PERFORM 施術記録Ｆ読込
024730            END-PERFORM
024740        END-IF
024750*       **************************
024760*       * 継続：終了年月日セット *
024770*       **************************
024780        IF 転帰区分Ｗ(部位ＣＮＴ) = 9
024790            MOVE 終了和暦ＷＴ  TO 終了和暦Ｗ(部位ＣＮＴ)
024800            MOVE 終了年ＷＴ    TO 終了年Ｗ(部位ＣＮＴ)
024810            MOVE 終了月ＷＴ    TO 終了月Ｗ(部位ＣＮＴ)
024820            MOVE 終了日ＷＴ    TO 終了日Ｗ(部位ＣＮＴ)
024830        END-IF
024840     END-PERFORM.
024841***
024850     MOVE 患者番号ＷＲ          TO 施記－患者番号.
024860     MOVE 枝番ＷＲ              TO 施記－枝番.
024870     MOVE 施術和暦ＷＲ          TO 施記－施術和暦.
024880     MOVE 施術年ＷＲ            TO 施記－施術年.
024890     MOVE 施術月ＷＲ            TO 施記－施術月.
024900     MOVE ZERO                  TO 施記－施術日.
024910     START 施術記録Ｆ   KEY IS >= 施記－患者コード
024920                                  施記－施術和暦年月日
024930     END-START.
024940     IF 状態キー = "00"
024150         MOVE SPACE TO 終了フラグ２
024950         PERFORM 施術記録Ｆ読込
024960         PERFORM UNTIL ( 終了フラグ２         = "YES"            ) OR
024970                       ( 施記－患者コード NOT = 患者コードＷＲ   ) OR
024980                       ( 施記－施術和暦   NOT = 施術和暦ＷＲ     ) OR
024990                       ( 施記－施術年     NOT = 施術年ＷＲ       ) OR
025000                       ( 施記－施術月     NOT = 施術月ＷＲ       )
025010*        ************
025020*        * 初検回数 *
025030*        ************
025040             IF 施記－初検料請求区分 = 1
025050                 COMPUTE 初検回数Ｗ = 初検回数Ｗ + 1
025060             END-IF
025070*        ************
025080*        * 初検加算 *
025090*        ************
025100             EVALUATE 施記－初検加算
025110             WHEN 1
025120                 COMPUTE 初検時間外回数Ｗ = 初検時間外回数Ｗ + 1
025130             WHEN 2
025140                 COMPUTE 初検休日回数Ｗ   = 初検休日回数Ｗ + 1
025150             WHEN 3
025160                 COMPUTE 初検深夜回数Ｗ   = 初検深夜回数Ｗ + 1
025170             END-EVALUATE
025180*        ************
025190*        * 再検回数 *
025200*        ************
025210             IF 施記－再検料請求 = 1
025220                 COMPUTE 再検回数Ｗ = 再検回数Ｗ + 1
025230             END-IF
025240*        ************
025250*        * 往療加算 *
025260*        ************
025270             EVALUATE 施記－往療加算
025280             WHEN 1
025290                 COMPUTE 往療夜間Ｗ = 往療夜間Ｗ + 1
025300             WHEN 2
025310                 COMPUTE 往療難路Ｗ = 往療難路Ｗ + 1
025320             WHEN 3
025330                 COMPUTE 往療暴風Ｗ = 往療暴風Ｗ + 1
025340             END-EVALUATE
025240*        ****************
025250*        * 初検時相談料 *
025260*        ****************
                   IF (施記－診療区分 = 2 ) AND (施記－初検時相談料区分 NOT = 1)
                       COMPUTE 相談支援回数Ｗ = 相談支援回数Ｗ + 1
                   END-IF
025240*        **********
025250*        * 施術日 *
025260*        **********
                   MOVE 1 TO 施術日Ｗ(施記－施術日)
      *
025350             PERFORM 施術記録Ｆ読込
025360         END-PERFORM
025370     END-IF.
025380*

027550*================================================================*
026930 負傷種別変換 SECTION.
026940*
026950     MOVE ZERO  TO 負傷種別変換後Ｗ.
026960*
026970     EVALUATE 負傷種別変換前Ｗ
026980     WHEN  ZERO
026990        MOVE ZERO TO 負傷種別変換後Ｗ
027000* 捻挫
027010     WHEN  01
027020        MOVE  4   TO 負傷種別変換後Ｗ
027030* 打撲
027040     WHEN  02
027050        MOVE  5   TO 負傷種別変換後Ｗ
027060* 挫傷
027070     WHEN  03
027080        MOVE  6   TO 負傷種別変換後Ｗ
027090* 脱臼
027100     WHEN  04
027110        MOVE  3   TO 負傷種別変換後Ｗ
027120* 骨折
027130     WHEN  05
027140        MOVE  1   TO 負傷種別変換後Ｗ
027150* 不全骨折
027160     WHEN  06
027170        MOVE  2   TO 負傷種別変換後Ｗ
027180* 骨折・不全骨折拘縮
027190     WHEN  07
027200     WHEN  08
027210        MOVE  7   TO 負傷種別変換後Ｗ
027220* 負傷名なし（無病）
027230     WHEN  09
027240        MOVE  9   TO 負傷種別変換後Ｗ
027250     WHEN OTHER
027260        CONTINUE
027270     END-EVALUATE.
027280*
027290*================================================================*
027300 転帰区分変換 SECTION.
027310*
027320     MOVE ZERO  TO 転帰変換後Ｗ.
027330*
027340     EVALUATE 転帰変換前Ｗ
027350     WHEN  ZERO
027360        MOVE ZERO TO 転帰変換後Ｗ
027370* 治癒
027380     WHEN  1
027390     WHEN  2
027400        MOVE  1   TO 転帰変換後Ｗ
027410* 中止
027420     WHEN  3
027430        MOVE  2   TO 転帰変換後Ｗ
027440* 転医
027450     WHEN  4
027460        MOVE  3   TO 転帰変換後Ｗ
027470* 継続・自然治癒
027480     WHEN  5
027490     WHEN  9
027500        MOVE  0   TO 転帰変換後Ｗ
027510     WHEN OTHER
027520        CONTINUE
027530     END-EVALUATE.
027540*
025390*================================================================*
012910 レセプトＦ読込 SECTION.
012920*
012930     READ レセプトＦ NEXT
012940     AT END
012950         MOVE "YES" TO 終了フラグ
012960     END-READ.
012970*
012980*================================================================*
012990 施術記録Ｆ読込 SECTION.
013000*
013010     READ 施術記録Ｆ NEXT
013020     AT END
013030         MOVE "YES"  TO 終了フラグ２
013040     END-READ.
013050*================================================================*
019320 作１ファイル書込 SECTION.
019330*
019340     WRITE 作１－レコード
019350     INVALID KEY
019360         MOVE NC"作１"  TO ファイル名
019370         PERFORM エラー表示
019380     END-WRITE.
019390*================================================================*
025390*================================================================*
025820 西暦施術年月取得 SECTION.
025830* 
025840     MOVE ZERO          TO 西暦年月Ｗ  西暦施術年月Ｗ.
025850     MOVE 受－施術和暦  TO 元－元号区分.
025860     READ 元号マスタ
025870     NOT INVALID KEY
025880         MOVE 元－開始西暦年 TO 西暦年Ｗ
025890     END-READ.
025900**
025910     IF 西暦年Ｗ = ZERO
025920          MOVE  NC"元号マスタに開始西暦年を登録して下さい" TO 連メ－メッセージ
025930          CALL   "MSG001"
025940          CANCEL "MSG001"
025950          PERFORM ファイル閉鎖
025960          MOVE 99 TO PROGRAM-STATUS
025970          EXIT PROGRAM
025980     ELSE
025990          COMPUTE 西暦年Ｗ = 西暦年Ｗ + 受－施術年 - 1
026000          MOVE 受－施術月 TO 西暦月Ｗ
026010     END-IF.
026020*
026030     MOVE 西暦年月Ｗ   TO  西暦施術年月Ｗ.
026040*
026050*================================================================*
026060 西暦年月日取得 SECTION.
026070*
026080     MOVE ZERO  TO 計算西暦年月日Ｗ.
026090*
026100     IF 計算和暦Ｗ  NOT = ZERO
026110         MOVE 計算和暦Ｗ    TO 元－元号区分
026120         READ 元号マスタ
026130         NOT INVALID KEY
026140             MOVE 元－開始西暦年 TO 計算西暦年Ｗ
026150         END-READ
026160**
026170         IF 計算西暦年Ｗ = ZERO
026180              MOVE  NC"元号マスタに開始西暦年を登録して下さい３" TO 連メ－メッセージ
026190              CALL   "MSG001"
026200              CANCEL "MSG001"
026210              PERFORM ファイル閉鎖
026220              MOVE 99 TO PROGRAM-STATUS
026230              EXIT PROGRAM
026240         ELSE
026250              COMPUTE 計算西暦年Ｗ = 計算西暦年Ｗ + 計算年Ｗ - 1
026260              MOVE 計算月Ｗ TO 計算西暦月Ｗ
026270              MOVE 計算日Ｗ TO 計算西暦日Ｗ
026280         END-IF
026290     END-IF.
026300*
026310*================================================================*
011330 作業ファイル３作成 SECTION.
011340*
026580     OPEN OUTPUT 作業ファイル３.
026590         MOVE NC"作３" TO ファイル名.
026600         PERFORM オープンチェック.
           CLOSE 作業ファイル３.
026580     OPEN I-O 作業ファイル３.
026590         MOVE NC"作３" TO ファイル名.
026600         PERFORM オープンチェック.
026710*
005420     MOVE 請求和暦ＷＲ  TO レセ－請求和暦.
005430     MOVE 請求年ＷＲ    TO レセ－請求年.
005440     MOVE 請求月ＷＲ    TO レセ－請求月.
005450     MOVE 1             TO レセ－施術和暦.
005460     MOVE ZERO          TO レセ－施術年.
005470     MOVE ZERO          TO レセ－施術月.
005480     MOVE ZERO          TO レセ－患者番号.
005490     MOVE LOW-VALUE     TO レセ－枝番.
005500     MOVE ZERO          TO レセ－レセ種別.
005510     START レセプトＦ   KEY IS >= レセ－請求和暦年月
005520                                  レセ－施術和暦年月
005530                                  レセ－患者コード
005540                                  レセ－レセ種別
026830     END-START.
026840     IF 状態キー = "00"
026850         MOVE SPACE  TO 終了フラグ
005580         PERFORM レセプトＦ読込
005590         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
005600                       ( レセ－請求和暦 NOT = 請求和暦ＷＲ ) OR
005610                       ( レセ－請求年   NOT = 請求年ＷＲ   ) OR
005620                       ( レセ－請求月   NOT = 請求月ＷＲ   )
026910*
026920            PERFORM データチェック
      */提出Ｆから労災自賠責自費生保忘れを抜く/20180809
      *            IF レセ－レセ種別 = 3
                  IF レセ－レセ種別 = 3 OR 4 OR 5 OR 6 OR 7 OR 8
                      MOVE SPACE TO 実行キーＷ
                  END-IF
                  IF 受－保険種別 NOT = 02 AND 07
                      MOVE SPACE TO 実行キーＷ
                  END-IF
026930            IF 実行キーＷ = "YES"
                      PERFORM 番号取得
                      READ 作業ファイル３
                      INVALID KEY
                          INITIALIZE 作３－レコード
                          PERFORM 番号取得
                          PERFORM 作３レコードセット
                          PERFORM 作３ファイル書込
                      NOT INVALID KEY
                          PERFORM 作３レコードセット
                          PERFORM 作３ファイル追加
                      END-READ
005200            END-IF
027850            PERFORM レセプトＦ読込
027860         END-PERFORM
027870     END-IF.
027880     CLOSE 作業ファイル３.
012420*================================================================*
       番号取得 SECTION.
      *
           EVALUATE TRUE
           WHEN 受－保険者番号 = "01230010"
               MOVE 18 TO 作３－番号
           WHEN 受－保険種別 = 07
               MOVE 19 TO 作３－番号
           WHEN OTHER
               MOVE 99 TO 作３－番号
           END-EVALUATE.
012420*================================================================*
       作３レコードセット SECTION.
      *
           MOVE 20 TO 作３－レコード区分.
           IF 受－本人家族区分 = 1
               COMPUTE 作３－本人請求件数 = 作３－本人請求件数 + 1
               COMPUTE 作３－本人請求金額 = 作３－本人請求金額 + レセ－請求金額
           ELSE
               COMPUTE 作３－家族請求件数 = 作３－家族請求件数 + 1
               COMPUTE 作３－家族請求金額 = 作３－家族請求金額 + レセ－請求金額
           END-IF.
012420*================================================================*
019320 作３ファイル書込 SECTION.
019330*
019340     WRITE 作３－レコード
019350     INVALID KEY
019360         MOVE NC"作３"  TO ファイル名
019370         PERFORM エラー表示
019380     END-WRITE.
019390*================================================================*
019320 作３ファイル追加 SECTION.
019330*
019340     REWRITE 作３－レコード
019350     INVALID KEY
019360         MOVE NC"作３"  TO ファイル名
019370         PERFORM エラー表示
019380     END-REWRITE.
012420*================================================================*
011330 作業ファイル４作成 SECTION.
011340*
026580     OPEN OUTPUT 作業ファイル４.
026590         MOVE NC"作４" TO ファイル名.
026600         PERFORM オープンチェック.
           CLOSE 作業ファイル４.
026580     OPEN I-O 作業ファイル４.
026590         MOVE NC"作４" TO ファイル名.
026600         PERFORM オープンチェック.
026710*
005420     MOVE 請求和暦ＷＲ  TO レセ－請求和暦.
005430     MOVE 請求年ＷＲ    TO レセ－請求年.
005440     MOVE 請求月ＷＲ    TO レセ－請求月.
005450     MOVE 1             TO レセ－施術和暦.
005460     MOVE ZERO          TO レセ－施術年.
005470     MOVE ZERO          TO レセ－施術月.
005480     MOVE ZERO          TO レセ－患者番号.
005490     MOVE LOW-VALUE     TO レセ－枝番.
005500     MOVE ZERO          TO レセ－レセ種別.
005510     START レセプトＦ   KEY IS >= レセ－請求和暦年月
005520                                  レセ－施術和暦年月
005530                                  レセ－患者コード
005540                                  レセ－レセ種別
026830     END-START.
026840     IF 状態キー = "00"
026850         MOVE SPACE  TO 終了フラグ
005580         PERFORM レセプトＦ読込
005590         PERFORM UNTIL ( 終了フラグ = "YES" ) OR
005600                       ( レセ－請求和暦 NOT = 請求和暦ＷＲ ) OR
005610                       ( レセ－請求年   NOT = 請求年ＷＲ   ) OR
005620                       ( レセ－請求月   NOT = 請求月ＷＲ   )
026910*
026920            PERFORM データチェック
      *            IF レセ－レセ種別 = 3
      *                MOVE SPACE TO 実行キーＷ
      *            END-IF
      */提出Ｆから労災自賠責自費生保忘れを抜く/20180809
                  IF レセ－レセ種別 = 4 OR 5 OR 6 OR 7 OR 8
                      MOVE SPACE TO 実行キーＷ
                  END-IF
026930            IF 実行キーＷ = "YES"
                      INITIALIZE 作４－レコード
000520                MOVE 受－施術和暦年月 TO 作４－施術和暦年月
000570                MOVE 受－患者コード   TO 作４－患者コード
                      MOVE レセ－レセ種別   TO 作４－レセ種別
      *                READ 作業ファイル４
      *                INVALID KEY
      *                    INITIALIZE 作４－レコードデータ
                          PERFORM 作４レコードセット
                          PERFORM 作４ファイル書込
      *                NOT INVALID KEY
      *                    PERFORM 作４レコードセット
      *                    PERFORM 作４ファイル追加
      *                END-READ
005200            END-IF
027850            PERFORM レセプトＦ読込
027860         END-PERFORM
027870     END-IF.
027880     CLOSE 作業ファイル４.
012420*================================================================*
       作４レコードセット SECTION.
      *
HILO  *     DISPLAY 受－レコードキー 受－患者氏名
           MOVE レセ－施術和暦年月 TO 施術和暦年月ＷＲ.
           MOVE レセ－患者コード   TO 患者コードＷＲ.
015500     PERFORM 負傷データ取得.
015510     PERFORM 料金情報取得.
015520     PERFORM 施術記録取得.
           MOVE 30 TO 作４－レコード区分.
           IF レセ－請求区分 = 2
               MOVE 1    TO 作４－提出区分
           ELSE
               MOVE ZERO TO 作４－提出区分
           END-IF.
           MOVE 受－保険者番号 TO 作４－保険者番号.
           MOVE 受－記号 TO 作４－保険証記号.
           MOVE 受－番号 TO 作４－保険証番号
000500*   / 1:一般,2:老人,3:助成,4:労災,5:自賠責,6:自費,7:生保単独,8保険証忘れ /
000510     IF レセ－レセ種別 = 3
               MOVE 3                      TO 作４－老人医療助成区分
               MOVE 受－費用負担者番号助成 TO 作４－市町村番号
               MOVE 受－受益者番号助成     TO 作４－受給者番号
               PERFORM 医療助成区分セット
013174         MOVE レセ－受給者負担額     TO 作４－一部負担金
013175         MOVE レセ－助成請求金額     TO 作４－請求金額
           ELSE
               MOVE 1                      TO 作４－老人医療助成区分
013174         MOVE レセ－一部負担金       TO 作４－一部負担金
013175         MOVE レセ－請求金額         TO 作４－請求金額
           END-IF
           MOVE 受－本人家族区分 TO 作４－本人家族区分
           MOVE 受－被保険者カナ TO 作４－被保険者カナ.
           MOVE 受－被保険者氏名 TO 作４－被保険者氏名.
           MOVE 受－患者カナ     TO 作４－患者カナ.
           MOVE 受－患者氏名     TO 作４－患者氏名.
           MOVE 受－患者性別     TO 作４－患者性別
015410* 生年月日
015420     MOVE ZERO               TO 計算和暦年月日Ｗ.
015430     MOVE 受－患者生年月日   TO 計算和暦年月日Ｗ.
015440     PERFORM 西暦年月日取得.
015450     MOVE 計算西暦年月日Ｗ   TO 作４－患者生年月日.
013173     MOVE レセ－合計         TO 作４－合計金額
014125     MOVE レセ－給付割合     TO 作４－給付割合
HILO  *     DISPLAY "作４－合計金額   " 作４－合計金額  
HILO  *     DISPLAY "作４－一部負担金 " 作４－一部負担金
HILO  *     DISPLAY "作４－請求金額   " 作４－請求金額  
HILO  *     DISPLAY "作４－給付割合   " 作４－給付割合  
015430     MOVE 受－施術和暦年月   TO 計算和暦年月日Ｗ.
015440     PERFORM 西暦年月日取得.
015450     MOVE 計算西暦年月日Ｗ   TO 作４－診療年月.
018270     MOVE 部位数Ｗ           TO 作４－部位数.
           MOVE レセ－レセ実日数   TO 作４－実日数.
      */140417
      *     MOVE 90                 TO 作４－端末区分.
           MOVE 18                 TO 作４－端末区分.
           EVALUATE 受－特別区分
           WHEN 1
           WHEN 2
               MOVE 8 TO 作４－給付分類
           WHEN 3
               MOVE 0 TO 作４－給付分類
           WHEN 6
               MOVE 6 TO 作４－給付分類
           WHEN OTHER
               IF 受－保険種別 = 01 OR 08
                   IF 受－本人家族区分 = 1
                       MOVE 2 TO 作４－給付分類
                   ELSE
                       MOVE 6 TO 作４－給付分類
                   END-IF
               END-IF
           END-EVALUATE.
018330     MOVE 新規区分Ｗ             TO 作４－新規区分.
018340     MOVE 継続区分Ｗ             TO 作４－継続区分.
018360     MOVE 初検回数Ｗ             TO 作４－初検回数.
018370     MOVE 初検時間外回数Ｗ       TO 作４－初検時間外加算回数.
018380     MOVE 初検休日回数Ｗ         TO 作４－初検休日加算回数.
018390     MOVE 初検深夜回数Ｗ         TO 作４－初検深夜加算回数.
018400     MOVE 再検回数Ｗ             TO 作４－再検回数.
018410     MOVE 往療距離２Ｗ           TO 作４－往療距離.
018420     MOVE 往療回数Ｗ             TO 作４－往療回数.
           MOVE 初検料Ｗ               TO 作４－初検料.
           MOVE 初検加算Ｗ             TO 作４－初検加算.
           MOVE 再検料Ｗ               TO 作４－再検料.
018420     MOVE 往療料Ｗ               TO 作４－往療料.
           MOVE 往療加算Ｗ             TO 作４－往療加算.
018430*
018440     MOVE 往療夜間Ｗ             TO 作４－夜間加算往療回数.
018450     MOVE 往療暴風Ｗ             TO 作４－暴風雨雪加算往療回数.
018460     MOVE 往療難路Ｗ             TO 作４－難路加算往療回数.
018470*
      */20180612
           IF 施術和暦年月ＷＲ < 43006
018480         MOVE 大回数Ｗ           TO 作４－金属副子大回数
018490         MOVE 中回数Ｗ           TO 作４－金属副子中回数
018500         MOVE 小回数Ｗ           TO 作４－金属副子小回数
      *↓↓↓/20180612
           ELSE
               COMPUTE 金属副子回数Ｗ = レセ－大 + レセ－中 + レセ－小
               IF 金属副子回数Ｗ > 9
                   MOVE 9 TO 金属副子回数Ｗ
               END-IF
               MOVE 金属副子回数Ｗ TO 作４－金属副子回数
           END-IF.
           MOVE レセ－運動後療回数     TO 作４－運動後療料回数.
           MOVE レセ－運動後療料       TO 作４－運動後療料    .
      *↑↑↑/20180612
018160     MOVE 金属副子Ｗ             TO 作４－金属副子.
018510*
018520     MOVE 情報提供料回数Ｗ       TO 作４－情報提供料回数.
           MOVE 相談支援回数Ｗ         TO 作４－相談支援回数.
           MOVE 情報提供料Ｗ           TO 作４－情報提供料.
           MOVE 相談支援料Ｗ           TO 作４－相談支援料.
      */明細書発行体制加算追加/20221212
           MOVE 明細書発行月日Ｗ       TO 作４－明細書発行月日.
           MOVE 明細書発行回数Ｗ       TO 作４－明細書発行回数.
           MOVE 明細書発行Ｗ           TO 作４－明細書発行料.
      *
014740*     PERFORM VARYING カウンタ FROM 1 BY 1
014750*             UNTIL   カウンタ > 8
      *         IF レセ－初回処置料(カウンタ) NOT = ZERO
028440*             MOVE レセ－初回処置料(カウンタ) TO 作４－整復料(カウンタ)
      *         END-IF
014810*     END-PERFORM.
014730*/施術日
014740     PERFORM VARYING カウンタ FROM 1 BY 1
014750             UNTIL   カウンタ > 31
014760         IF 施術日Ｗ(カウンタ) = ZERO
014770            MOVE ZERO               TO 作４－施術日(カウンタ)
014780         ELSE
014790            MOVE 施術日Ｗ(カウンタ) TO 作４－施術日(カウンタ)
014800         END-IF
014810     END-PERFORM.
      *
           PERFORM 長期継続者Ｆ読込.
023870     PERFORM VARYING 部位ＣＮＴ FROM 1 BY 1 UNTIL 部位ＣＮＴ > 部位数Ｗ
      */会部位コード
025340         MOVE 負－負傷種別(部位ＣＮＴ)     TO 部－負傷種別
025350         MOVE 負－部位(部位ＣＮＴ)         TO 部－部位
025360         MOVE 負－左右区分(部位ＣＮＴ)     TO 部－左右区分
025370         MOVE 負－負傷位置番号(部位ＣＮＴ) TO 部－負傷位置番号
027460         READ 部位マスタ
027470         NOT INVALID KEY
                   MOVE 部－会部位コード TO 作４－会部位コード(部位ＣＮＴ)
               END-READ
      */負傷原因
               MOVE 01                             TO 負原－区分コード
               MOVE 負－負傷原因コード(部位ＣＮＴ) TO 負原－負傷原因コード
               READ 負傷原因Ｆ
               NOT INVALID KEY
      *             MOVE 負原－負傷原因ＣＭ全体     TO 作４－負傷原因(部位ＣＮＴ)
                   STRING 負原－負傷原因ＣＭ(1) DELIMITED BY SPACE
                          負原－負傷原因ＣＭ(2) DELIMITED BY SPACE
                          負原－負傷原因ＣＭ(3) DELIMITED BY SPACE
                          負原－負傷原因ＣＭ(4) DELIMITED BY SPACE
                          負原－負傷原因ＣＭ(5) DELIMITED BY SPACE
                     INTO 作４－負傷原因(部位ＣＮＴ)
                   END-STRING
               END-READ
      */長期理由
               MOVE 長継－理由文(部位ＣＮＴ)       TO  作４－長期理由(部位ＣＮＴ)
           END-PERFORM.
      *
           COMPUTE 作４－当部位費用額(1) = レセ－長期込小計１   + レセ－初回処置料(1)
           COMPUTE 作４－当部位費用額(2) = レセ－長期込小計２   + レセ－初回処置料(2)
           COMPUTE 作４－当部位費用額(3) = レセ－長期込小計３８ + レセ－長期込小計３０ + レセ－初回処置料(3)
           COMPUTE 作４－当部位費用額(4) = レセ－長期込小計４５ + レセ－長期込小計４８ + レセ－長期込小計４０ 
                                         + レセ－初回処置料(4)
           COMPUTE 作４－当部位費用額(5) = レセ－長期込小計５２ + レセ－長期込小計５５ + レセ－長期込小計５８ 
                                         + レセ－長期込小計５０ + レセ－初回処置料(5)
           COMPUTE 作４－当部位費用額(6) = レセ－長期込小計６８ + レセ－長期込小計６０ + レセ－初回処置料(6)
           COMPUTE 作４－当部位費用額(7) = レセ－長期込小計７８ + レセ－長期込小計７０ + レセ－初回処置料(7)
015550* １部位目
015560     MOVE 負傷種別Ｗ(1)          TO 負傷種別変換前Ｗ.
015570     PERFORM 負傷種別変換.
015580     MOVE 負傷種別変換後Ｗ       TO 作４－負傷区分(1).
015590     MOVE 負傷名Ｗ(1)            TO 作４－負傷名(1).
015600     MOVE 初回処置回数Ｗ(1)      TO 作４－初回処置回数(1).
015610*
015620     MOVE ZERO                   TO 計算和暦年月日Ｗ.
015630     MOVE 負傷年月日Ｗ(1)        TO 計算和暦年月日Ｗ.
015640     PERFORM 西暦年月日取得.
015650     MOVE 計算西暦年月日Ｗ       TO 作４－負傷年月日(1).
015660*
015670     MOVE ZERO                   TO 計算和暦年月日Ｗ.
015680     MOVE 初検年月日Ｗ(1)        TO 計算和暦年月日Ｗ.
015690     PERFORM 西暦年月日取得.
015700     MOVE 計算西暦年月日Ｗ       TO 作４－初検年月日(1).
015710*
015720     MOVE ZERO                   TO 計算和暦年月日Ｗ.
015730     MOVE 開始年月日Ｗ(1)        TO 計算和暦年月日Ｗ.
015740     PERFORM 西暦年月日取得.
015750     MOVE 計算西暦年月日Ｗ       TO 作４－施術開始年月日(1).
015760*
015770     MOVE ZERO                   TO 計算和暦年月日Ｗ.
015780     MOVE 終了年月日Ｗ(1)        TO 計算和暦年月日Ｗ.
015790     PERFORM 西暦年月日取得.
015800     MOVE 計算西暦年月日Ｗ       TO 作４－施術終了年月日(1).
015810*
015820     MOVE 実日数Ｗ(1)            TO 作４－部位実日数(1).
015830*
015840     MOVE 転帰区分Ｗ(1)          TO 転帰変換前Ｗ.
015850     PERFORM 転帰区分変換.
015860     MOVE 転帰変換後Ｗ           TO 作４－転帰区分(1).
015870*
015880     MOVE 後療回数１ＷＲ         TO 作４－後療回数(1).
015890*
015900     MOVE 冷罨法回数１ＷＲ       TO 作４－冷罨法回数(1).
015910*
015920     MOVE 温罨法回数１ＷＲ       TO 作４－温罨法回数(1).
015930*
015940     MOVE 電療回数１ＷＲ         TO 作４－電療回数(1).
015950*
015960     MOVE 多部位区分Ｗ(1)        TO 作４－多部位逓減区分(1).
015970*
015980     MOVE 長期区分Ｗ(1)          TO 作４－長期逓減区分(1).
      *
           MOVE レセ－後療料１         TO 作４－後療料(1).
           MOVE レセ－冷罨法料１       TO 作４－冷罨法料(1).
           MOVE レセ－温罨法料１       TO 作４－温罨法料(1).
           MOVE レセ－電療料１         TO 作４－電療料(1).
015990*
016000* ２部位目
016010     MOVE 負傷種別Ｗ(2)          TO 負傷種別変換前Ｗ.
016020     PERFORM 負傷種別変換.
016030     MOVE 負傷種別変換後Ｗ       TO 作４－負傷区分(2).
016040     MOVE 負傷名Ｗ(2)            TO 作４－負傷名(2).
016050     MOVE 初回処置回数Ｗ(2)      TO 作４－初回処置回数(2).
016060*
016070     MOVE ZERO                   TO 計算和暦年月日Ｗ.
016080     MOVE 負傷年月日Ｗ(2)        TO 計算和暦年月日Ｗ.
016090     PERFORM 西暦年月日取得.
016100     MOVE 計算西暦年月日Ｗ       TO 作４－負傷年月日(2).
016110*
016120     MOVE ZERO                   TO 計算和暦年月日Ｗ.
016130     MOVE 初検年月日Ｗ(2)        TO 計算和暦年月日Ｗ.
016140     PERFORM 西暦年月日取得.
016150     MOVE 計算西暦年月日Ｗ       TO 作４－初検年月日(2).
016160*
016170     MOVE ZERO                   TO 計算和暦年月日Ｗ.
016180     MOVE 開始年月日Ｗ(2)        TO 計算和暦年月日Ｗ.
016190     PERFORM 西暦年月日取得.
016200     MOVE 計算西暦年月日Ｗ       TO 作４－施術開始年月日(2).
016210*
016220     MOVE ZERO                   TO 計算和暦年月日Ｗ.
016230     MOVE 終了年月日Ｗ(2)        TO 計算和暦年月日Ｗ.
016240     PERFORM 西暦年月日取得.
016250     MOVE 計算西暦年月日Ｗ       TO 作４－施術終了年月日(2).
016260*
016270     MOVE 実日数Ｗ(2)            TO 作４－部位実日数(2).
016280*
016290     MOVE 転帰区分Ｗ(2)          TO 転帰変換前Ｗ.
016300     PERFORM 転帰区分変換.
016310     MOVE 転帰変換後Ｗ           TO 作４－転帰区分(2).
016320*
016330     MOVE 後療回数２ＷＲ         TO 作４－後療回数(2).
016340*
016350     MOVE 冷罨法回数２ＷＲ       TO 作４－冷罨法回数(2).
016360*
016370     MOVE 温罨法回数２ＷＲ       TO 作４－温罨法回数(2).
016380*
016390     MOVE 電療回数２ＷＲ         TO 作４－電療回数(2).
016400*
016410     MOVE 多部位区分Ｗ(2)        TO 作４－多部位逓減区分(2).
016420*
016430     MOVE 長期区分Ｗ(2)          TO 作４－長期逓減区分(2).
      *
           MOVE レセ－後療料２         TO 作４－後療料(2).
           MOVE レセ－冷罨法料２       TO 作４－冷罨法料(2).
           MOVE レセ－温罨法料２       TO 作４－温罨法料(2).
           MOVE レセ－電療料２         TO 作４－電療料(2).
016440*
016450* ３部位目
016460     MOVE 負傷種別Ｗ(3)          TO 負傷種別変換前Ｗ.
016470     PERFORM 負傷種別変換.
016480     MOVE 負傷種別変換後Ｗ       TO 作４－負傷区分(3).
016490     MOVE 負傷名Ｗ(3)            TO 作４－負傷名(3).
016500     MOVE 初回処置回数Ｗ(3)      TO 作４－初回処置回数(3).
016510*
016520     MOVE ZERO                   TO 計算和暦年月日Ｗ.
016530     MOVE 負傷年月日Ｗ(3)        TO 計算和暦年月日Ｗ.
016540     PERFORM 西暦年月日取得.
016550     MOVE 計算西暦年月日Ｗ       TO 作４－負傷年月日(3).
016560*
016570     MOVE ZERO                   TO 計算和暦年月日Ｗ.
016580     MOVE 初検年月日Ｗ(3)        TO 計算和暦年月日Ｗ.
016590     PERFORM 西暦年月日取得.
016600     MOVE 計算西暦年月日Ｗ       TO 作４－初検年月日(3).
016610*
016620     MOVE ZERO                   TO 計算和暦年月日Ｗ.
016630     MOVE 開始年月日Ｗ(3)        TO 計算和暦年月日Ｗ.
016640     PERFORM 西暦年月日取得.
016650     MOVE 計算西暦年月日Ｗ       TO 作４－施術開始年月日(3).
016660*
016670     MOVE ZERO                   TO 計算和暦年月日Ｗ.
016680     MOVE 終了年月日Ｗ(3)        TO 計算和暦年月日Ｗ.
016690     PERFORM 西暦年月日取得.
016700     MOVE 計算西暦年月日Ｗ       TO 作４－施術終了年月日(3).
016710*
016720     MOVE 実日数Ｗ(3)            TO 作４－部位実日数(3).
016730*
016740     MOVE 転帰区分Ｗ(3)          TO 転帰変換前Ｗ.
016750     PERFORM 転帰区分変換.
016760     MOVE 転帰変換後Ｗ           TO 作４－転帰区分(3).
016770*
016780     MOVE 後療回数３ＷＲ         TO 作４－後療回数(3).
016790*
016800     MOVE 冷罨法回数３ＷＲ       TO 作４－冷罨法回数(3).
016810*
016820     MOVE 温罨法回数３ＷＲ       TO 作４－温罨法回数(3).
016830*
016840     MOVE 電療回数３ＷＲ         TO 作４－電療回数(3).
016850*
016860     MOVE 多部位区分Ｗ(3)        TO 作４－多部位逓減区分(3).
016870*
016880     MOVE 長期区分Ｗ(3)          TO 作４－長期逓減区分(3).
      *
011490     COMPUTE 作４－後療料(3)   = レセ－後療料３８   + レセ－後療料３０.
011640     COMPUTE 作４－冷罨法料(3) = レセ－冷罨法料３８ + レセ－冷罨法料３０.
011750     COMPUTE 作４－温罨法料(3) = レセ－温罨法料３８ + レセ－温罨法料３０.
011860     COMPUTE 作４－電療料(3)   = レセ－電療料３８   + レセ－電療料３０.
016890*
016900* ４部位目
016910     MOVE 負傷種別Ｗ(4)          TO 負傷種別変換前Ｗ.
016920     PERFORM 負傷種別変換.
016930     MOVE 負傷種別変換後Ｗ       TO 作４－負傷区分(4).
016940     MOVE 負傷名Ｗ(4)            TO 作４－負傷名(4).
016950     MOVE 初回処置回数Ｗ(4)      TO 作４－初回処置回数(4).
016960*
016970     MOVE ZERO                   TO 計算和暦年月日Ｗ.
016980     MOVE 負傷年月日Ｗ(4)        TO 計算和暦年月日Ｗ.
016990     PERFORM 西暦年月日取得.
017000     MOVE 計算西暦年月日Ｗ       TO 作４－負傷年月日(4).
017010*
017020     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017030     MOVE 初検年月日Ｗ(4)        TO 計算和暦年月日Ｗ.
017040     PERFORM 西暦年月日取得.
017050     MOVE 計算西暦年月日Ｗ       TO 作４－初検年月日(4).
017060*
017070     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017080     MOVE 開始年月日Ｗ(4)        TO 計算和暦年月日Ｗ.
017090     PERFORM 西暦年月日取得.
017100     MOVE 計算西暦年月日Ｗ       TO 作４－施術開始年月日(4).
017110*
017120     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017130     MOVE 終了年月日Ｗ(4)        TO 計算和暦年月日Ｗ.
017140     PERFORM 西暦年月日取得.
017150     MOVE 計算西暦年月日Ｗ       TO 作４－施術終了年月日(4).
017160*
017170     MOVE 実日数Ｗ(4)            TO 作４－部位実日数(4).
017180*
017190     MOVE 転帰区分Ｗ(4)          TO 転帰変換前Ｗ.
017200     PERFORM 転帰区分変換.
017210     MOVE 転帰変換後Ｗ           TO 作４－転帰区分(4).
017220*
017230     MOVE 後療回数４ＷＲ         TO 作４－後療回数(4).
017240*
017250     MOVE 冷罨法回数４ＷＲ       TO 作４－冷罨法回数(4).
017260*
017270     MOVE 温罨法回数４ＷＲ       TO 作４－温罨法回数(4).
017280*
017290     MOVE 電療回数４ＷＲ         TO 作４－電療回数(4).
017300*
017310     MOVE 多部位区分Ｗ(4)        TO 作４－多部位逓減区分(4).
017320*
017330     MOVE 長期区分Ｗ(4)          TO 作４－長期逓減区分(4).
      *
011490     COMPUTE 作４－後療料(4)   = レセ－後療料４５   + レセ－後療料４８   + レセ－後療料４０.
011640     COMPUTE 作４－冷罨法料(4) = レセ－冷罨法料４５ + レセ－冷罨法料４８ + レセ－冷罨法料４０.
011750     COMPUTE 作４－温罨法料(4) = レセ－温罨法料４５ + レセ－温罨法料４８ + レセ－温罨法料４０.
011860     COMPUTE 作４－電療料(4)   = レセ－電療料４５   + レセ－電療料４８   + レセ－電療料４０.
017340*
017350* ５部位目
017360     MOVE 負傷種別Ｗ(5)          TO 負傷種別変換前Ｗ.
017370     PERFORM 負傷種別変換.
017380     MOVE 負傷種別変換後Ｗ       TO 作４－負傷区分(5).
017390     MOVE 負傷名Ｗ(5)            TO 作４－負傷名(5).
017400     MOVE 初回処置回数Ｗ(5)      TO 作４－初回処置回数(5).
017410*
017420     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017430     MOVE 負傷年月日Ｗ(5)        TO 計算和暦年月日Ｗ.
017440     PERFORM 西暦年月日取得.
017450     MOVE 計算西暦年月日Ｗ       TO 作４－負傷年月日(5).
017460*
017470     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017480     MOVE 初検年月日Ｗ(5)        TO 計算和暦年月日Ｗ.
017490     PERFORM 西暦年月日取得.
017500     MOVE 計算西暦年月日Ｗ       TO 作４－初検年月日(5).
017510*
017520     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017530     MOVE 開始年月日Ｗ(5)        TO 計算和暦年月日Ｗ.
017540     PERFORM 西暦年月日取得.
017550     MOVE 計算西暦年月日Ｗ       TO 作４－施術開始年月日(5).
017560*
017570     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017580     MOVE 終了年月日Ｗ(5)        TO 計算和暦年月日Ｗ.
017590     PERFORM 西暦年月日取得.
017600     MOVE 計算西暦年月日Ｗ       TO 作４－施術終了年月日(5).
017610*
017620     MOVE 実日数Ｗ(5)            TO 作４－部位実日数(5).
017630*
017640     MOVE 転帰区分Ｗ(5)          TO 転帰変換前Ｗ.
017650     PERFORM 転帰区分変換.
017660     MOVE 転帰変換後Ｗ           TO 作４－転帰区分(5).
017670*
017680     MOVE 後療回数５ＷＲ         TO 作４－後療回数(5).
017690*
017700     MOVE 冷罨法回数５ＷＲ       TO 作４－冷罨法回数(5).
017710*
017720     MOVE 温罨法回数５ＷＲ       TO 作４－温罨法回数(5).
017730*
017740     MOVE 電療回数５ＷＲ         TO 作４－電療回数(5).
017750*
017760     MOVE 多部位区分Ｗ(5)        TO 作４－多部位逓減区分(5).
017770*
017780     MOVE 長期区分Ｗ(5)          TO 作４－長期逓減区分(5).
      *
011490     COMPUTE 作４－後療料(5)   = レセ－後療料５２   + レセ－後療料５５   +
                                       レセ－後療料５８   + レセ－後療料５０.
011640     COMPUTE 作４－冷罨法料(5) = レセ－冷罨法料５２ + レセ－冷罨法料５５ +
                                       レセ－冷罨法料５８ + レセ－冷罨法料５０.
011750     COMPUTE 作４－温罨法料(5) = レセ－温罨法料５２ + レセ－温罨法料５５ +
                                       レセ－温罨法料５８ + レセ－温罨法料５０.
011860     COMPUTE 作４－電療料(5)   = レセ－電療料５２   + レセ－電療料５５   +
                                       レセ－電療料５８   + レセ－電療料５０.
017790*
017800* ６部位目
017810     MOVE 負傷種別Ｗ(6)          TO 負傷種別変換前Ｗ.
017820     PERFORM 負傷種別変換.
017830     MOVE 負傷種別変換後Ｗ       TO 作４－負傷区分(6).
017840     MOVE 負傷名Ｗ(6)            TO 作４－負傷名(6).
017850     MOVE 初回処置回数Ｗ(6)      TO 作４－初回処置回数(6).
017860*
017870     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017880     MOVE 負傷年月日Ｗ(6)        TO 計算和暦年月日Ｗ.
017890     PERFORM 西暦年月日取得.
017900     MOVE 計算西暦年月日Ｗ       TO 作４－負傷年月日(6).
017910*
017920     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017930     MOVE 初検年月日Ｗ(6)        TO 計算和暦年月日Ｗ.
017940     PERFORM 西暦年月日取得.
017950     MOVE 計算西暦年月日Ｗ       TO 作４－初検年月日(6).
017960*
017970     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017980     MOVE 開始年月日Ｗ(6)        TO 計算和暦年月日Ｗ.
017990     PERFORM 西暦年月日取得.
018000     MOVE 計算西暦年月日Ｗ       TO 作４－施術開始年月日(6).
018010*
018020     MOVE ZERO                   TO 計算和暦年月日Ｗ.
018030     MOVE 終了年月日Ｗ(6)        TO 計算和暦年月日Ｗ.
018040     PERFORM 西暦年月日取得.
018050     MOVE 計算西暦年月日Ｗ       TO 作４－施術終了年月日(6).
018060*
018070     MOVE 実日数Ｗ(6)            TO 作４－部位実日数(6).
018080*
018090     MOVE 転帰区分Ｗ(6)          TO 転帰変換前Ｗ.
018100     PERFORM 転帰区分変換.
018110     MOVE 転帰変換後Ｗ           TO 作４－転帰区分(6).
018120*
018130     MOVE ZERO                   TO 作４－後療回数(6).
018140*
018150     MOVE ZERO                   TO 作４－冷罨法回数(6).
018160*
018170     MOVE ZERO                   TO 作４－温罨法回数(6).
018180*
018190     MOVE ZERO                   TO 作４－電療回数(6).
018200*
018210     MOVE 多部位区分Ｗ(6)        TO 作４－多部位逓減区分(6).
018220*
018230     MOVE 長期区分Ｗ(6)          TO 作４－長期逓減区分(6).
      *
011490     COMPUTE 作４－後療料(6)   = レセ－後療料６８   + レセ－後療料６０.
011640     COMPUTE 作４－冷罨法料(6) = レセ－冷罨法料６８ + レセ－冷罨法料６０.
011750     COMPUTE 作４－温罨法料(6) = レセ－温罨法料６８ + レセ－温罨法料６０.
011860     COMPUTE 作４－電療料(6)   = レセ－電療料６８   + レセ－電療料６０.
017790*
017800* ７部位目
017810     MOVE 負傷種別Ｗ(7)          TO 負傷種別変換前Ｗ.
017820     PERFORM 負傷種別変換.
017830     MOVE 負傷種別変換後Ｗ       TO 作４－負傷区分(7).
017840     MOVE 負傷名Ｗ(7)            TO 作４－負傷名(7).
017850     MOVE 初回処置回数Ｗ(7)      TO 作４－初回処置回数(7).
017860*
017870     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017880     MOVE 負傷年月日Ｗ(7)        TO 計算和暦年月日Ｗ.
017890     PERFORM 西暦年月日取得.
017900     MOVE 計算西暦年月日Ｗ       TO 作４－負傷年月日(7).
017910*
017920     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017930     MOVE 初検年月日Ｗ(7)        TO 計算和暦年月日Ｗ.
017940     PERFORM 西暦年月日取得.
017950     MOVE 計算西暦年月日Ｗ       TO 作４－初検年月日(7).
017960*
017970     MOVE ZERO                   TO 計算和暦年月日Ｗ.
017980     MOVE 開始年月日Ｗ(7)        TO 計算和暦年月日Ｗ.
017990     PERFORM 西暦年月日取得.
018000     MOVE 計算西暦年月日Ｗ       TO 作４－施術開始年月日(7).
018010*
018020     MOVE ZERO                   TO 計算和暦年月日Ｗ.
018030     MOVE 終了年月日Ｗ(7)        TO 計算和暦年月日Ｗ.
018040     PERFORM 西暦年月日取得.
018050     MOVE 計算西暦年月日Ｗ       TO 作４－施術終了年月日(7).
018060*
018070     MOVE 実日数Ｗ(7)            TO 作４－部位実日数(7).
018080*
018090     MOVE 転帰区分Ｗ(7)          TO 転帰変換前Ｗ.
018100     PERFORM 転帰区分変換.
018110     MOVE 転帰変換後Ｗ           TO 作４－転帰区分(7).
018120*
018130     MOVE ZERO                   TO 作４－後療回数(7).
018140*
018150     MOVE ZERO                   TO 作４－冷罨法回数(7).
018160*
018170     MOVE ZERO                   TO 作４－温罨法回数(7).
018180*
018190     MOVE ZERO                   TO 作４－電療回数(7).
018200*
018210     MOVE 多部位区分Ｗ(7)        TO 作４－多部位逓減区分(7).
018220*
018230     MOVE 長期区分Ｗ(7)          TO 作４－長期逓減区分(7).
      *
011490     COMPUTE 作４－後療料(7)   = レセ－後療料７８   + レセ－後療料７０.
011640     COMPUTE 作４－冷罨法料(7) = レセ－冷罨法料７８ + レセ－冷罨法料７０.
011750     COMPUTE 作４－温罨法料(7) = レセ－温罨法料７８ + レセ－温罨法料７０.
011860     COMPUTE 作４－電療料(7)   = レセ－電療料７８   + レセ－電療料７０.
      *
012420*================================================================*
019320 作４ファイル書込 SECTION.
019330*
019340     WRITE 作４－レコード
019350     INVALID KEY
019360         MOVE NC"作４"  TO ファイル名
019370         PERFORM エラー表示
019380     END-WRITE.
019390*================================================================*
019320 作４ファイル追加 SECTION.
019330*
019340     REWRITE 作４－レコード
019350     INVALID KEY
019360         MOVE NC"作４"  TO ファイル名
019370         PERFORM エラー表示
019380     END-REWRITE.
012420*================================================================*
       医療助成区分セット SECTION.
      *
           EVALUATE 受－助成種別
           WHEN 52
               MOVE 4 TO 作４－医療助成区分
           WHEN 53
               MOVE 3 TO 作４－医療助成区分
           WHEN 54
               MOVE 6 TO 作４－医療助成区分
           WHEN 55
               MOVE 2 TO 作４－医療助成区分
           WHEN 60
004288         IF 受－費用負担者番号助成(1:4) = "8923"
                   MOVE 7 TO 作４－医療助成区分
               END-IF
           END-EVALUATE.
012420*================================================================*
       長期継続者Ｆ読込 SECTION.
      *
           MOVE レセ－施術和暦年月 TO 長継－施術和暦年月.
000170     MOVE レセ－患者コード   TO 長継－患者コード.
           READ 長期継続者Ｆ
           INVALID KEY
               MOVE SPACE TO 長継－レコード
           END-READ.
012420*================================================================*
027744******************************************************************
027745 END PROGRAM YAI101.
027746******************************************************************
