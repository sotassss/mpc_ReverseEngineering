000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             ZAIRYO.
000060 AUTHOR.                 山田 浩之
000070*
000080*----------------------------------------------------------------*
000090*         特別材料料文字列取得プログラム
000100*
000190*----------------------------------------------------------------*
000200 DATE-WRITTEN.           20180604
000210 DATE-COMPILED.          20180604
000220*----------------------------------------------------------------*
000250******************************************************************
000260*            ENVIRONMENT         DIVISION                        *
000270******************************************************************
000280 ENVIRONMENT             DIVISION.
000290 CONFIGURATION           SECTION.
000300 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000310 OBJECT-COMPUTER.        FMV-DESKPOWER.
000320 SPECIAL-NAMES.          CONSOLE  IS  CONS
000330                         SYSERR   IS  MSGBOX.
000340 INPUT-OUTPUT            SECTION.
000350 FILE-CONTROL.
000360     SELECT  施術記録Ｆ      ASSIGN      TO        SEKIROKL
000370                             ORGANIZATION             IS  INDEXED
000380                             ACCESS MODE              IS  DYNAMIC
000390                             RECORD KEY               IS  施記－施術和暦年月日
000400                                                          施記－患者コード
000410                             ALTERNATE RECORD KEY     IS  施記－患者コード
000420                                                          施記－施術和暦年月日
000430                             FILE STATUS              IS  状態キー
000440                             LOCK        MODE         IS  AUTOMATIC.
000670     SELECT  受診者情報Ｆ    ASSIGN      TO        JUSINJL
000680                             ORGANIZATION             IS  INDEXED
000690                             ACCESS MODE              IS  DYNAMIC
000700                             RECORD KEY               IS  受－施術和暦年月
000710                                                          受－患者コード
000720                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000730                                                          受－患者カナ
000740                                                          受－患者コード
000750                             ALTERNATE RECORD KEY     IS  受－患者コード
000760                                                          受－施術和暦年月
000770                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000780                                                          受－保険種別
000790                                                          受－保険者番号
000800                                                          受－患者コード
000810                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000820                                                          受－公費種別
000830                                                          受－費用負担者番号
000840                                                          受－患者コード
000850                             ALTERNATE RECORD KEY     IS  受－施術和暦年月
000860                                                          受－助成種別
000870                                                          受－費用負担者番号助成
000880                                                          受－患者コード
000890                             ALTERNATE RECORD KEY     IS  受－請求和暦年月
000900                                                          受－施術和暦年月
000910                                                          受－患者コード
000920                             FILE STATUS              IS  状態キー
000930                             LOCK        MODE         IS  AUTOMATIC.
000931*
000959**
000962     SELECT  負傷データＦ    ASSIGN      TO        HUSYOUL
000963                             ORGANIZATION             IS  INDEXED
000964                             ACCESS MODE              IS  DYNAMIC
000970                             RECORD KEY               IS  負－施術和暦年月
000980                                                          負－患者コード
000990                             ALTERNATE RECORD KEY     IS  負－患者コード
001000                                                          負－施術和暦年月
001010                             FILE STATUS              IS  状態キー
001020                             LOCK        MODE         IS  AUTOMATIC.
001510*
001520     SELECT  レセプトＦ      ASSIGN      TO        RECEPTL
001530                             ORGANIZATION             IS  INDEXED
001540                             ACCESS MODE              IS  DYNAMIC
001550                             RECORD KEY               IS  レセ－施術和暦年月
001560                                                          レセ－患者コード
001570                                                          レセ－レセ種別
001580                             ALTERNATE RECORD KEY     IS  レセ－患者コード
001590                                                          レセ－施術和暦年月
001600                                                          レセ－レセ種別
001610                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
001620                                                          レセ－施術和暦年月
001630                                                          レセ－患者コード
001640                                                          レセ－レセ種別
001650                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
001660                                                          レセ－レセ種別
001670                                                          レセ－請求保険者番号
001680                                                          レセ－患者コード
001690                                                          レセ－施術和暦年月
001700                             ALTERNATE RECORD KEY     IS  レセ－請求和暦年月
001710                                                          レセ－請求保険者番号
001720                                                          レセ－患者コード
001730                                                          レセ－レセ種別
001740                                                          レセ－施術和暦年月
001750                             FILE STATUS              IS  状態キー
001760                             LOCK        MODE         IS  AUTOMATIC.
001270*
000130     SELECT  レセプト労自Ｆ  ASSIGN      TO        RECERJL
000140                             ORGANIZATION             IS  INDEXED
000150                             ACCESS MODE              IS  DYNAMIC
000160                             RECORD KEY               IS  レセ労自－施術和暦年月
000170                                                          レセ労自－患者コード
000180                                                          レセ労自－レセ種別
000360                             FILE STATUS              IS  状態キー
000370                             LOCK        MODE         IS  AUTOMATIC.
001770**
000330     SELECT 作業ファイル１ ASSIGN    TO "C:\MAKISHISYS\YAWOBJ\TEMP\WKIN001L.DAT"
000340                             ORGANIZATION             IS  INDEXED
000350                             ACCESS MODE              IS  DYNAMIC
000360                             RECORD      KEY          IS  作１－金属算定和暦年月日
000470                             FILE STATUS              IS  状態キー
000480                             LOCK        MODE         IS  AUTOMATIC.
002006**
002010*--------------------------------------------------------------------------------------*
002011*
002012******************************************************************
002013*                      DATA DIVISION                             *
002014******************************************************************
002015 DATA                    DIVISION.
002016 FILE                    SECTION.
002017*                           ［ＲＬ＝  ２５６］
002018 FD  施術記録Ｆ          BLOCK   CONTAINS   1   RECORDS.
002019     COPY SEKIROK         OF  XFDLIB  JOINING   施記 AS  PREFIX.
002110*                           ［ＲＬ＝  ３２０］
002120 FD  受診者情報Ｆ        BLOCK   CONTAINS   1   RECORDS.
002130     COPY JUSINJ     OF  XFDLIB  JOINING   受   AS  PREFIX.
002140*                           ［ＲＬ＝  １２８］
002150 FD  負傷データＦ        BLOCK   CONTAINS   1   RECORDS.
002160     COPY HUSYOU     OF  XFDLIB  JOINING   負   AS  PREFIX.
002350*
002360 FD  レセプトＦ          BLOCK   CONTAINS   1   RECORDS.
002370     COPY RECEPT     OF  XFDLIB  JOINING   レセ   AS  PREFIX.
002001*
001990 FD  レセプト労自Ｆ      BLOCK   CONTAINS   1   RECORDS.
002000     COPY RECERJ          OF  XFDLIB  JOINING   レセ労自 AS  PREFIX.
002350*
000630 FD  作業ファイル１ RECORD  CONTAINS 7  CHARACTERS.
000640 01 作１－レコード.
000650    03 作１－レコードキー.
000140     05 作１－金属算定和暦年月日.
000150       07 作１－金属算定和暦年月.
000160         09 作１－金属算定和暦         PIC 9.
000170         09 作１－金属算定年月.
000180           11 作１－金属算定年         PIC 9(2).
000190           11 作１－金属算定月         PIC 9(2).
000200       07 作１－金属算定日             PIC 9(2).
002350*
002464*----------------------------------------------------------------*
002465******************************************************************
002466*                WORKING-STORAGE SECTION                         *
002467******************************************************************
002468 WORKING-STORAGE         SECTION.
002470 01 キー入力                           PIC X      VALUE SPACE.
002480 01 状態キー                           PIC X(2)   VALUE SPACE.
002490 01 終了フラグ                         PIC X(3)   VALUE SPACE.
002490 01 終了フラグ２                       PIC X(3)   VALUE SPACE.
002610 01 ファイル名Ｗ                       PIC N(10)  VALUE SPACE.
002660 01 部位カウンタ                       PIC 9(1)   VALUE ZERO.
002660 01 部位カウンタ２                     PIC 9(1)   VALUE ZERO.
002660 01 カウンタ                           PIC 9(3)   VALUE ZERO.
002660 01 カウンタ２                         PIC 9(3)   VALUE ZERO.
002660 01 対象カウンタ                       PIC 9(3)   VALUE ZERO.
002660 01 印刷カウンタ                       PIC 9(3)   VALUE ZERO.
000310 01 部位数Ｗ                           PIC 9      VALUE ZERO.
       01 金属副子ＣＭＷ                     PIC X(300) VALUE SPACE.
       01 運動後療ＣＭＷ                     PIC X(100) VALUE SPACE.
       01 金属副子ＣＭＷ２.
           03 金属副子ＣＭＷ２１             PIC X(130) VALUE SPACE.
           03 金属副子ＣＭＷ２２             PIC X(170) VALUE SPACE.
       01 材料費ＣＭＷ                       PIC X(300) VALUE SPACE.
000200 01 回数ＺＷ                           PIC Z(2)   VALUE ZERO.
       01 金属文字列編集Ｗ.
           03 金属回数Ｗ１.
               05 金属回数Ｗ OCCURS 7        PIC 9(1)   VALUE ZERO.
      *     03 金属算定日Ｗ１.
      *         05 金属算定日部位毎 OCCURS 7.
      *             07 金属算定日Ｗ OCCURS 3  PIC 9(2)   VALUE ZERO.
           03 金属算定和暦年月日Ｗ１.
               05 金属算定和暦年月日部位毎 OCCURS 7.
                   07 金属算定和暦年月日Ｗ OCCURS 3.
000150                 09 金属算定和暦年月Ｗ.
000160                   11 金属算定和暦Ｗ   PIC 9      VALUE ZERO.
000170                   11 金属算定年月Ｗ.
000180                     13 金属算定年Ｗ   PIC 9(2)   VALUE ZERO.
000190                     13 金属算定月Ｗ   PIC 9(2)   VALUE ZERO.
000200                 09 金属算定日Ｗ       PIC 9(2)   VALUE ZERO.
           03 金属算定和暦年月日ＺＷ.
000150         09 金属算定和暦年月ＺＷ.
000160           11 金属算定和暦ＺＷ         PIC Z      VALUE ZERO.
000170           11 金属算定年月ＺＷ.
000180             13 金属算定年ＺＷ         PIC Z(2)   VALUE ZERO.
000190             13 金属算定月ＺＷ         PIC Z(2)   VALUE ZERO.
000200         09 金属算定日ＺＷ             PIC Z(2)   VALUE ZERO.
           03 金属算定和暦ＣＭ               PIC X(1)   VALUE SPACE.

       01 運動文字列編集Ｗ.
           03 運動後療単価ＺＷ               PIC Z(4)   VALUE SPACE.
           03 運動後療料ＺＷ                 PIC Z(4)   VALUE SPACE.
           03 運動算定日ＺＷ                 PIC Z(2)   VALUE ZERO.
           03 運動算定日ＺＷＳ.
               05 運動算定日ＺＷ１           PIC Z(2)   VALUE ZERO.
               05 運動算定日ＺＷ２           PIC Z(2)   VALUE ZERO.
               05 運動算定日ＺＷ３           PIC Z(2)   VALUE ZERO.
               05 運動算定日ＺＷ４           PIC Z(2)   VALUE ZERO.
               05 運動算定日ＺＷ５           PIC Z(2)   VALUE ZERO.
           03 運動算定日Ｘ２.
               05 運動算定日ＸＷ１           PIC Z(2)   VALUE ZERO.
               05 運動算定日ＸＷ２           PIC Z(2)   VALUE ZERO.
               05 運動算定日ＸＷ３           PIC Z(2)   VALUE ZERO.
               05 運動算定日ＸＷ４           PIC Z(2)   VALUE ZERO.
               05 運動算定日ＸＷ５           PIC Z(2)   VALUE ZERO.
001420 01 遅延フラグ                         PIC X(3) VALUE SPACE.
001430 01 遅延カウンタ                       PIC 9(4) VALUE ZERO.
001440 01 遅延回数Ｗ                         PIC 9(4) VALUE ZERO.
      *
005450 01 部位コードＷ.
005460    03 負傷種別Ｗ                      PIC 9(2) VALUE ZERO.
005470    03 部位Ｗ                          PIC 9(2) VALUE ZERO.
005480    03 左右区分Ｗ                      PIC 9    VALUE ZERO.
005490    03 負傷位置番号Ｗ                  PIC 9(2) VALUE ZERO.
      *
       01 開始和暦年月日Ｗ.
         03 開始和暦年月Ｗ.
           05 開始和暦Ｗ                     PIC 9    VALUE ZERO.
           05 開始年月Ｗ.
              07 開始年Ｗ                    PIC 9(2) VALUE ZERO.
              07 開始月Ｗ                    PIC 9(2) VALUE ZERO.
         03 開始日Ｗ                         PIC 9(2) VALUE ZERO.
005700*
005710 01 施術和暦年月日Ｗ.
005720   03 施術和暦年月Ｗ.
005730     05 施術和暦Ｗ                     PIC 9    VALUE ZERO.
005740     05 施術年月Ｗ.
005750        07 施術年Ｗ                    PIC 9(2) VALUE ZERO.
005760        07 施術月Ｗ                    PIC 9(2) VALUE ZERO.
005770   03 施術日Ｗ                         PIC 9(2) VALUE ZERO.
005780*
       01 部位カウンタ３                     PIC 9(2) VALUE ZERO.
       01 部位カウンタ４                     PIC 9(2) VALUE ZERO.
       01 金属副子ＣＮＴ                     PIC 9(1) VALUE ZERO.
       01 部位番号Ｗ                         PIC 9(1) VALUE ZERO.
       01 部位記号Ｗ                         PIC X(3) VALUE SPACE.
009830*****************************************************************
009840*                          連結項目                             *
009850*****************************************************************
009860*
      *
       01 連金運－キー IS EXTERNAL.
          03 連金運－施術和暦年月.
             05 連金運－施術和暦                  PIC 9(1).
             05 連金運－施術年月.
                07 連金運－施術年                 PIC 9(2).
                07 連金運－施術月                 PIC 9(2).
          03 連金運－患者コード.
             05 連金運－患者番号                  PIC 9(6).
             05 連金運－枝番                      PIC X(1).
          03 連金運－保険種別                     PIC 9(2).
          03 連金運－会コード                     PIC 9(2).
          03 連金運－用紙種別                     PIC 9(1).
          03 連金運－金属副子.
             05 連金運－金属副子ＣＭ              PIC X(200).
             05 連金運－金属副子部位              OCCURS 5.
                07 連金運－金属副子和暦年月日     OCCURS 3.
                   09 連金運－金属副子和暦年月.
                      11 連金運－金属副子和暦     PIC 9(1).
                      11 連金運－金属副子年月.
                         13 連金運－金属副子年    PIC 9(2).
                         13 連金運－金属副子月    PIC 9(2).
                   09 連金運－金属副子日          PIC 9(2).
          03 連金運－運動後療.
             05 連金運－運動後療ＣＭ              PIC X(100).
             05 連金運－運動日                    PIC 9(2)    OCCURS 5.
      *
       01 連金運２－キー IS EXTERNAL.
           03 連金運２－金属副子ＣＭ.
               05 連金運２－金属副子ＣＭ１        PIC X(130).
               05 連金運２－金属副子ＣＭ２        PIC X(170).
      *
013804******************************************************************
013810*                      PROCEDURE  DIVISION                       *
013820******************************************************************
013830 PROCEDURE               DIVISION.
HILO  *     DISPLAY "K-s "  連金運－施術和暦年月 " "  連金運－患者コード " " 連金運－保険種別 " " 連金運－会コード " " 連金運－用紙種別 "---------------------- "
013840************
013850*           *
013860* 初期処理   *
013870*           *
013880************
014175*
013990     PERFORM 初期化.
           PERFORM 項目初期化.
014175*
014180************
014190*           *
014200* 主処理     *
014210*           *
014220************
014230*
038430     MOVE 連金運－施術和暦年月 TO 負－施術和暦年月
038420     MOVE 連金運－患者コード   TO 負－患者コード
038470     READ 負傷データＦ
038480     INVALID KEY
038550         MOVE NC"負傷" TO ファイル名Ｗ
038560         PERFORM エラー表示Ｒ
           NOT INVALID KEY
               MOVE 負－部位数 TO 部位数Ｗ
038570     END-READ.
      *
038430     MOVE 連金運－施術和暦年月 TO レセ労自－施術和暦年月
038420     MOVE 連金運－患者コード   TO レセ労自－患者コード
053709     MOVE 4                    TO レセ労自－レセ種別.
049892     READ レセプト労自Ｆ
049893     INVALID KEY
049150         MOVE NC"労自" TO ファイル名Ｗ
               PERFORM エラー表示Ｒ
049920     END-READ.
           MOVE "特別材料費:" TO 材料費ＣＭＷ.
           PERFORM 材料算定日取得
026320*     PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL 部位カウンタ > 部位数Ｗ
026320     PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL 部位カウンタ > 4
               IF 金属算定和暦年月日Ｗ(部位カウンタ 1) NOT = ZERO
                   MOVE SPACE TO 部位記号Ｗ
                   EVALUATE 部位カウンタ
                   WHEN 1 MOVE "イ." TO 部位記号Ｗ
                   WHEN 2 MOVE "ロ." TO 部位記号Ｗ
                   WHEN 3 MOVE "ハ." TO 部位記号Ｗ
                   WHEN 4 MOVE "ニ." TO 部位記号Ｗ
                   END-EVALUATE
                   STRING 材料費ＣＭＷ DELIMITED BY SPACE
                          部位記号Ｗ   DELIMITED BY SPACE
                     INTO 材料費ＣＭＷ
                   END-STRING
HILO               IF レセ労自－材料費回数(部位カウンタ) NOT = ZERO AND 1 AND 2 AND 3 AND 4 AND 5 AND 6 AND 7 AND 8 AND 9
HILO               DISPLAY "材料費回数が未設定です" END-IF
                   STRING 材料費ＣＭＷ                       DELIMITED BY SPACE
                          レセ労自－材料費回数(部位カウンタ) DELIMITED BY SIZE
                          "回|"                              DELIMITED BY SPACE
                     INTO 材料費ＣＭＷ
                   END-STRING
      *
026320             PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 3
                       IF 金属算定和暦年月日Ｗ(部位カウンタ カウンタ) NOT = ZERO
                           MOVE 金属算定年Ｗ(部位カウンタ カウンタ) TO 金属算定年ＺＷ
                           MOVE 金属算定月Ｗ(部位カウンタ カウンタ) TO 金属算定月ＺＷ
                           MOVE 金属算定日Ｗ(部位カウンタ カウンタ) TO 金属算定日ＺＷ
                           STRING 材料費ＣＭＷ   DELIMITED BY SPACE
                                  金属算定年ＺＷ DELIMITED BY SIZE
                                  "."            DELIMITED BY SIZE
                                  金属算定月ＺＷ DELIMITED BY SIZE
                                  "."            DELIMITED BY SIZE
                                  金属算定日ＺＷ DELIMITED BY SIZE
                                  "||"           DELIMITED BY SIZE
                             INTO 材料費ＣＭＷ
                           END-STRING
                           PERFORM 材料文字詰
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.
           INSPECT 材料費ＣＭＷ   REPLACING ALL "|" BY " ".
           MOVE 材料費ＣＭＷ TO 連金運－金属副子ＣＭ.
HILO  *     DISPLAY "Zai-6 " 材料費ＣＭＷ
017650************
017660*           *
017670* 終了処理   *
017680*           *
017690************
017700*
017710     PERFORM 終了処理.
017720     MOVE 1 TO PROGRAM-STATUS.
017730     EXIT PROGRAM.
017740*
017750*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
017760*================================================================*
017770 初期化 SECTION.
017780*
017790     OPEN INPUT 施術記録Ｆ.
017800         MOVE NC"施記" TO ファイル名Ｗ.
017810         PERFORM オープンチェック.
017901*
017910     OPEN INPUT 受診者情報Ｆ.
017920         MOVE NC"受診" TO ファイル名Ｗ.
017930         PERFORM オープンチェック.
017931*
017940     OPEN INPUT 負傷データＦ.
017950         MOVE NC"負傷" TO ファイル名Ｗ.
017960         PERFORM オープンチェック.
017931*
018180     OPEN INPUT レセプトＦ.
018190         MOVE NC"レセ" TO ファイル名Ｗ.
018200         PERFORM オープンチェック.
017931*
021270     OPEN INPUT レセプト労自Ｆ.
021280         MOVE NC"労自" TO ファイル名Ｗ.
021290         PERFORM オープンチェック.
018510*================================================================*
018520 オープンチェック SECTION.
018530*
018540     IF 状態キー  NOT =  "00"
018550         DISPLAY ファイル名Ｗ NC"Ｆオープンエラー" UPON CONS
018560         DISPLAY NC"状態キー：" 状態キー           UPON CONS
018570         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS
018580*-----------------------------------------*
018590         CALL "actcshm"  WITH C LINKAGE
018600*-----------------------------------------*
018610         ACCEPT  キー入力 FROM CONS
018620         PERFORM ファイル閉鎖
018630         MOVE ZERO TO PROGRAM-STATUS
018640         EXIT PROGRAM.
018650*================================================================*
018660 ファイル閉鎖 SECTION.
018670*
018680     CLOSE 施術記録Ｆ 受診者情報Ｆ 負傷データＦ レセプトＦ レセプト労自Ｆ.
018767*
018783*================================================================*
018784 終了処理 SECTION.
018785*
018786     PERFORM ファイル閉鎖.
018790*================================================================*
018800 エラー表示 SECTION.
018810*
018820     DISPLAY ファイル名Ｗ NC"ファイル書込エラー"   UPON CONS.
018830     DISPLAY NC"状態キー：" 状態キー               UPON CONS.
018840     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
018850     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
018860*-----------------------------------------*
018870     CALL "actcshm"  WITH C LINKAGE.
018880*-----------------------------------------*
018890     ACCEPT  キー入力 FROM CONS.
018900     PERFORM ファイル閉鎖.
018910     MOVE ZERO TO PROGRAM-STATUS
018920     EXIT PROGRAM.
018930*================================================================*
018940 エラー表示Ｒ SECTION.
018950*
018960     DISPLAY ファイル名Ｗ NC"ファイル読込エラー"   UPON CONS.
018970     DISPLAY NC"状態キー：" 状態キー               UPON CONS.
018980     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
018990     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
019000*-----------------------------------------*
019010     CALL "actcshm"  WITH C LINKAGE.
019020*-----------------------------------------*
019030     ACCEPT  キー入力 FROM CONS.
019040     PERFORM ファイル閉鎖.
019050     MOVE ZERO TO PROGRAM-STATUS
019060     EXIT PROGRAM.
019070*================================================================*
019080 エラー表示その他 SECTION.
019090*
019100     DISPLAY NC"システム管理者に連絡してください"  UPON CONS.
019110     DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください" UPON CONS.
019120*-----------------------------------------*
019130     CALL "actcshm"  WITH C LINKAGE.
019140*-----------------------------------------*
019150     ACCEPT  キー入力 FROM CONS.
019160     PERFORM ファイル閉鎖.
019170     EXIT PROGRAM.
019260*================================================================*
       運動標準 SECTION.
      *
      */回数・金額・加算日(空白詰める)
      */例) 4回 1240円 (1日,2日)
      *
           MOVE SPACE TO 運動後療ＣＭＷ.
           MOVE "柔道整復運動後療料:" TO 運動後療ＣＭＷ.
      */算定回数
           PERFORM 運動算定回数設定.
      */金額
           IF 連金運－会コード NOT = 07
               PERFORM 運動金額設定
           END-IF.
      */算定日
           PERFORM 運動算定日設定.
019180*================================================================*
       金属運動日整大阪 SECTION.
      *
      */金属加算日・運動加算日
      */例) 金属副子等加算日:1日,26日 柔道整復運動後療料加算日:18日,29日
           MOVE SPACE TO 金属副子ＣＭＷ.
           IF レセ－金属副子加算料 NOT = ZERO
               PERFORM 金属対象判定
               MOVE "金属副子等加算日:" TO 金属副子ＣＭＷ
      */加算日 当月の合算した加算日
               PERFORM 金属加算日合算大阪
           END-IF
           IF レセ－運動後療料 NOT = ZERO
      */運動タイトル
               STRING 金属副子ＣＭＷ               DELIMITED BY SPACE
                      "|柔道整復運動後療料加算日:" DELIMITED BY SIZE
                 INTO 金属副子ＣＭＷ
               END-STRING
      */算定日
               PERFORM 運動算定日大阪
           END-IF.
019180*================================================================*
       運動ケアバンク SECTION.
      *
      */回数・金額・加算日(空白でも詰めない)
      */例) 5回 1550円 加算日：1日 2日 10日  日   日
      *
           MOVE SPACE TO 運動後療ＣＭＷ.
           MOVE "柔道整復運動後療料:" TO 運動後療ＣＭＷ.
      */算定回数
           PERFORM 運動算定回数設定.
      */金額
           PERFORM 運動金額設定.
      */算定日
           STRING 運動後療ＣＭＷ     DELIMITED BY SPACE
                  "加算日:"          DELIMITED BY SIZE
             INTO 運動後療ＣＭＷ
           END-STRING.
           PERFORM 運動算定日詰無.
019180*================================================================*
       運動日整静岡 SECTION.
      *
      */加算日(空白詰めない)・回数・金額
      */例) 加算日 1日 2日 4日 5日  日  4回 1240円
      *
           MOVE SPACE TO 運動後療ＣＭＷ.
           MOVE "柔道整復運動後療料|加算日|" TO 運動後療ＣＭＷ.
      */算定日
           PERFORM 運動算定日詰無.
      */算定回数
           PERFORM 運動算定回数設定.
      */金額
           PERFORM 運動金額設定.
019180*================================================================*
       運動日接 SECTION.
      *
      */加算日(空白詰めない)・内訳
      */例) 加算日 1日 2日 4日 5日  日 310x2=620
      *
           MOVE SPACE TO 運動後療ＣＭＷ.
           MOVE "柔道整復運動後療料|加算日|" TO 運動後療ＣＭＷ.
      */算定日
           PERFORM 運動算定日詰無.
      */内訳
           PERFORM 運動内訳.
019180*================================================================*
       運動内訳 SECTION.
      *
           MOVE レセ－運動後療単価 TO 運動後療単価ＺＷ.
           MOVE レセ－運動後療料   TO 運動後療料ＺＷ  .
           STRING 運動後療ＣＭＷ     DELIMITED BY SPACE
                  運動後療単価ＺＷ   DELIMITED BY SIZE
                  "x"                DELIMITED BY SIZE
                  レセ－運動後療回数 DELIMITED BY SIZE
                  "="                DELIMITED BY SIZE
                  運動後療料ＺＷ     DELIMITED BY SIZE
             INTO 運動後療ＣＭＷ
           END-STRING.
           PERFORM 運動文字詰.
019180*================================================================*
       運動算定回数設定 SECTION.
      *
           STRING 運動後療ＣＭＷ     DELIMITED BY SPACE
                  レセ－運動後療回数 DELIMITED BY SIZE
                  "回|"              DELIMITED BY SIZE
             INTO 運動後療ＣＭＷ
           END-STRING.
019180*================================================================*
       運動金額設定 SECTION.
      *
           STRING 運動後療ＣＭＷ   DELIMITED BY SPACE
                  運動後療料ＺＷ   DELIMITED BY SIZE
                  "円|"            DELIMITED BY SIZE
             INTO 運動後療ＣＭＷ
           END-STRING.
           PERFORM 運動文字詰.
019180*================================================================*
       運動算定日設定 SECTION.
      *
           IF レセ－運動算定日(1) NOT = ZERO
               MOVE レセ－運動算定日(1) TO 運動算定日ＺＷ
               STRING 運動後療ＣＭＷ      DELIMITED BY SPACE
                      "("                 DELIMITED BY SIZE
                      運動算定日ＺＷ      DELIMITED BY SIZE
                      "日"                DELIMITED BY SIZE
                 INTO 運動後療ＣＭＷ
               END-STRING
               PERFORM 運動文字詰
           END-IF.
026320     PERFORM VARYING カウンタ FROM 2 BY 1 UNTIL カウンタ > 5
               IF レセ－運動算定日(カウンタ) NOT = ZERO
                   MOVE レセ－運動算定日(カウンタ) TO 運動算定日ＺＷ
                   STRING 運動後療ＣＭＷ DELIMITED BY SPACE
                          ","            DELIMITED BY SIZE
                          運動算定日ＺＷ DELIMITED BY SIZE
                          "日"           DELIMITED BY SIZE
                     INTO 運動後療ＣＭＷ
                   END-STRING
                   PERFORM 運動文字詰
               END-IF
           END-PERFORM.
           STRING 運動後療ＣＭＷ DELIMITED BY SPACE
                  ")"            DELIMITED BY SIZE
             INTO 運動後療ＣＭＷ
           END-STRING.
019180*================================================================*
       運動算定日詰無 SECTION.
      *
           MOVE 運動算定日ＺＷＳ TO 運動算定日Ｘ２.
           INSPECT 運動算定日Ｘ２ REPLACING ALL " " BY "|".
           STRING 運動後療ＣＭＷ     DELIMITED BY SPACE
                  運動算定日ＸＷ１   DELIMITED BY SIZE
                  "日|"              DELIMITED BY SIZE
                  運動算定日ＸＷ２   DELIMITED BY SIZE
                  "日|"              DELIMITED BY SIZE
                  運動算定日ＸＷ３   DELIMITED BY SIZE
                  "日|"              DELIMITED BY SIZE
                  運動算定日ＸＷ４   DELIMITED BY SIZE
                  "日|"              DELIMITED BY SIZE
                  運動算定日ＸＷ５   DELIMITED BY SIZE
                  "日|"              DELIMITED BY SIZE
             INTO 運動後療ＣＭＷ
           END-STRING.
019180*================================================================*
       運動文字詰 SECTION.
      *
026320     PERFORM VARYING カウンタ２ FROM 1 BY 1 UNTIL カウンタ２ >= 100
               IF 運動後療ＣＭＷ(カウンタ２:1) = " "
HILO  *             DISPLAY "k-1 " カウンタ２ "'" 運動後療ＣＭＷ(カウンタ２ + 1:100 - カウンタ２)
                   MOVE 運動後療ＣＭＷ(カウンタ２ + 1:100 - カウンタ２ + 1)
                     TO 運動後療ＣＭＷ(カウンタ２:100 - カウンタ２)
               END-IF
           END-PERFORM.
019180*================================================================*
       金属対象判定 SECTION.
      *
011344     OPEN OUTPUT 作業ファイル１.
011345         MOVE NC"作１" TO ファイル名Ｗ.
011346         PERFORM オープンチェック.
           MOVE SPACE TO 金属回数Ｗ１.
           INITIALIZE    金属回数Ｗ１.
           MOVE SPACE TO 金属算定和暦年月日Ｗ１.
           INITIALIZE    金属算定和暦年月日Ｗ１.
026320     PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL 部位カウンタ > 5
               MOVE SPACE TO 連金運－金属副子部位(部位カウンタ)
               INITIALIZE    連金運－金属副子部位(部位カウンタ)
           END-PERFORM.
           MOVE 連金運－患者コード   TO 施記－患者コード.
           MOVE 連金運－施術和暦年月 TO 施記－施術和暦年月.
           MOVE 1                    TO 施記－施術日.
044890     START 施術記録Ｆ   KEY IS >= 施記－患者コード
044900                                  施記－施術和暦年月日
044910     END-START.
044920     IF 状態キー = "00"
044930         MOVE SPACE TO 終了フラグ
044950         PERFORM 施術記録Ｆ読込
044960         PERFORM UNTIL (終了フラグ             = "YES"               ) OR
044970                       (施記－患者コード   NOT = 連金運－患者コード  ) OR
044980                       (施記－施術和暦年月 NOT = 連金運－施術和暦年月)
026320             PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL 部位カウンタ > 部位数Ｗ
                       IF 施記－金属副子区分(部位カウンタ) NOT = ZERO
                           COMPUTE 金属回数Ｗ(部位カウンタ) = 金属回数Ｗ(部位カウンタ) + 1
026320                     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 3
                               IF 金属算定日Ｗ(部位カウンタ カウンタ) = ZERO
                                   MOVE 施記－施術和暦年月日 TO 金属算定和暦年月日Ｗ(部位カウンタ カウンタ)
                                                          連金運－金属副子和暦年月日(部位カウンタ カウンタ)
                                   MOVE 3 TO カウンタ
                               END-IF
                           END-PERFORM
                           MOVE 施記－施術和暦年月日 TO 作１－金属算定和暦年月日
019340                     WRITE 作１－レコード
019350                     INVALID KEY
                               IF 状態キー NOT = 22
019360                             MOVE NC"作１"  TO ファイル名Ｗ
019370                             PERFORM エラー表示
                               END-IF
019380                     END-WRITE
                       END-IF
                   END-PERFORM
044950             PERFORM 施術記録Ｆ読込
               END-PERFORM
           END-IF.
HILO  *     PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL 部位カウンタ > 部位数Ｗ
HILO  *         DISPLAY "金属算定日(" 部位カウンタ ")"   金属算定和暦年月日Ｗ(部位カウンタ 1) " " 
HILO  *         金属算定和暦年月日Ｗ(部位カウンタ 2) " " 金属算定和暦年月日Ｗ(部位カウンタ 3)
HILO  *     END-PERFORM.
011344     CLOSE 作業ファイル１.
019180*================================================================*
       金属標準 SECTION.
      *
      */部位毎に(部位番号・回数・加算日)
      */例) (1)１回(2日,3日)
      *
      */旧用紙の場合は1部位目から、新用紙の場合は最初に算定してある部位の次の部位から始める。
           IF 連金運－用紙種別 = ZERO
               MOVE 1 TO 部位カウンタ４
           END-IF.
      *
           MOVE ZERO TO 印刷カウンタ.
      *     MOVE "金属副子等加算:" TO 金属副子ＣＭＷ.
026320*     PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL 部位カウンタ > 部位数Ｗ
026320     PERFORM VARYING 部位カウンタ FROM 部位カウンタ４ BY 1 UNTIL 部位カウンタ > 部位数Ｗ
               IF 金属回数Ｗ(部位カウンタ) > ZERO
                   IF 印刷カウンタ = ZERO
                       MOVE "金属副子等加算:" TO 金属副子ＣＭＷ
                   END-IF
                   COMPUTE 印刷カウンタ = 印刷カウンタ + 1
                   IF 印刷カウンタ <= 5
      */部位番号
                       PERFORM 金属部位番号設定
      */回数
                       PERFORM 金属回数設定
      */加算日
                       PERFORM 金属加算日設定
                   END-IF
               END-IF
           END-PERFORM.
019180*================================================================*
       金属日本整復 SECTION.
      *
      *
      */部位毎に(部位番号・回数・加算日)
      */例) (1)１回(30.2日,30.3日)
      *
           MOVE ZERO TO 印刷カウンタ.
           MOVE "金属副子等加算:" TO 金属副子ＣＭＷ.
026320     PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL 部位カウンタ > 部位数Ｗ
               IF 金属回数Ｗ(部位カウンタ) > ZERO
                   COMPUTE 印刷カウンタ = 印刷カウンタ + 1
                   IF 印刷カウンタ <= 5
      */部位番号
                       PERFORM 金属部位番号設定
      */回数
                       PERFORM 金属回数設定
      */加算日
                       PERFORM 金属加算日設定年月
                   END-IF
               END-IF
           END-PERFORM.
019180*================================================================*
       金属中央 SECTION.
      *
      */部位毎に(部位番号・回数(通算)・加算日(年月日))
      */例) (1)(1回目       , 2回目         3回目H30.7/31
      *
           MOVE ZERO TO 印刷カウンタ.
           MOVE SPACE TO 金属算定和暦年月日Ｗ１ 金属副子ＣＭＷ２.
           INITIALIZE    金属算定和暦年月日Ｗ１ 対象カウンタ.
           MOVE "金属副子等加算:" TO 金属副子ＣＭＷ.
026320     PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL 部位カウンタ > 部位数Ｗ
               IF 金属回数Ｗ(部位カウンタ) > ZERO
                   COMPUTE 印刷カウンタ = 印刷カウンタ + 1
                   IF 印刷カウンタ <= 5
                       COMPUTE 対象カウンタ = 対象カウンタ + 1
                       IF 対象カウンタ = 3
                           MOVE 金属副子ＣＭＷ TO 金属副子ＣＭＷ２１
                           MOVE SPACE          TO 金属副子ＣＭＷ
                       END-IF
      */当月に金属副子を算定している部位の算定日を１回目から判定する
                       PERFORM 金属通算回数判定
      */部位番号
                       PERFORM 金属部位番号設定
      */加算通算回数と日
                       PERFORM 金属通算加算日設定
                   END-IF
               END-IF
           END-PERFORM.
           IF 対象カウンタ >= 3
               MOVE 金属副子ＣＭＷ TO 金属副子ＣＭＷ２２
               MOVE SPACE          TO 金属副子ＣＭＷ
           END-IF.
019180*================================================================*
       金属ケアバンク SECTION.
      *
      */変更↓↓↓
      **/当月の合算した加算日 
      **/例)１回目   日 ２回目   日 ３回目   日
      *     MOVE "金属副子等加算日:" TO 金属副子ＣＭＷ.
      **/加算日 当月の合算した加算日
      *     PERFORM 金属加算日回目表示.
      */変更↑↑↑
      *
      */部位毎に(部位番号・回数(通算)・加算日(年月日))
      */例) (1)(1回目30.6.1 2回目30.7.5 3回目30.8.31
      *
           MOVE ZERO TO 印刷カウンタ.
           MOVE SPACE TO 金属算定和暦年月日Ｗ１ 金属副子ＣＭＷ２.
           INITIALIZE    金属算定和暦年月日Ｗ１ 対象カウンタ.
           MOVE "金属副子等加算:" TO 金属副子ＣＭＷ.
026320     PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL 部位カウンタ > 部位数Ｗ
               IF 金属回数Ｗ(部位カウンタ) > ZERO
                   COMPUTE 印刷カウンタ = 印刷カウンタ + 1
                   IF 印刷カウンタ <= 5
                       COMPUTE 対象カウンタ = 対象カウンタ + 1
                       IF 対象カウンタ = 3
                           MOVE 金属副子ＣＭＷ TO 金属副子ＣＭＷ２１
                           MOVE SPACE          TO 金属副子ＣＭＷ
                       END-IF
      */当月に金属副子を算定している部位の算定日を１回目から判定する
                       PERFORM 金属通算回数判定
      */部位番号
                       PERFORM 金属部位番号設定
      */加算通算回数と日
                       PERFORM 金属通算加算日ケアバンク
                   END-IF
               END-IF
           END-PERFORM.
           IF 対象カウンタ >= 3
               MOVE 金属副子ＣＭＷ TO 金属副子ＣＭＷ２２
               MOVE SPACE          TO 金属副子ＣＭＷ
           END-IF.
019180*================================================================*
       金属静岡 SECTION.
      *
      */当月の合算した加算日・回数
      */例)１回目   日 ２回目   日 ３回目   日  回数
           MOVE "金属副子等加算日:" TO 金属副子ＣＭＷ.
      */加算日 当月の合算した加算日
           PERFORM 金属加算日回目表示.
      */1文字空ける
           STRING 金属副子ＣＭＷ  DELIMITED BY SPACE
                  "|"             DELIMITED BY SIZE
             INTO 金属副子ＣＭＷ
           END-STRING
      */合算した回数
           PERFORM 金属合算回数.
019180*================================================================*
       金属日接 SECTION.
      *
      */当月の合算した加算日・回数
      */例)１回目   日 ２回目   日 ３回目   日  回数
           MOVE "金属副子等加算日:" TO 金属副子ＣＭＷ.
      */加算日 当月の合算した加算日
           PERFORM 金属加算日回目表示.
      *
019180*================================================================*
       金属合算回数 SECTION.
      *
           MOVE レセ－金属副子回数 TO 回数ＺＷ.
           STRING 金属副子ＣＭＷ   DELIMITED BY SPACE
                  回数ＺＷ         DELIMITED BY SIZE
                  "回|"            DELIMITED BY SIZE
             INTO 金属副子ＣＭＷ
           END-STRING.
           PERFORM 金属文字詰.
019180*================================================================*
       金属加算日回目表示 SECTION.
      *
      */合算した回数を３つまで記載する。
011344     OPEN INPUT 作業ファイル１.
011345         MOVE NC"作１" TO ファイル名Ｗ.
011346         PERFORM オープンチェック.
      *
           MOVE ZERO TO 作１－金属算定和暦年月日.
044890     START 作業ファイル１ KEY IS >= 作１－金属算定和暦年月日
044910     END-START.
044920     IF 状態キー = "00"
044930         MOVE SPACE TO 終了フラグ２
044950         PERFORM 作業ファイル１読込
026320         PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 3
HILO  *             DISPLAY "k-2 " 作１－金属算定和暦年月日
                   IF (作１－金属算定和暦年月日 NOT = ZERO) AND (終了フラグ２ = SPACE)
                       MOVE 作１－金属算定日 TO 金属算定日ＺＷ
                       MOVE カウンタ TO 部位番号Ｗ
                       STRING 金属副子ＣＭＷ   DELIMITED BY SPACE
                              部位番号Ｗ       DELIMITED BY SIZE
                              "回目|"          DELIMITED BY SIZE
                              金属算定日ＺＷ   DELIMITED BY SIZE
                              "日|"            DELIMITED BY SIZE
                         INTO 金属副子ＣＭＷ
                       END-STRING
                       PERFORM 金属文字詰
                   ELSE
                       MOVE カウンタ TO 部位番号Ｗ
                       STRING 金属副子ＣＭＷ   DELIMITED BY SPACE
                              部位番号Ｗ       DELIMITED BY SIZE
                              "回目"           DELIMITED BY SIZE
                              "|||||||"        DELIMITED BY SIZE
                              "日|"            DELIMITED BY SIZE
                         INTO 金属副子ＣＭＷ
                       END-STRING
                   END-IF
044950             PERFORM 作業ファイル１読込
               END-PERFORM
           END-IF
011344     CLOSE 作業ファイル１.
019260*================================================================*
019190 作業ファイル１読込 SECTION.
019200*
019210     READ 作業ファイル１ NEXT
019220     AT END
019230         MOVE "YES" TO 終了フラグ２
019240     END-READ.
019250*
019180*================================================================*
       金属部位番号設定 SECTION.
      *
           STRING 金属副子ＣＭＷ DELIMITED BY SPACE
                  "|("           DELIMITED BY SIZE
                  部位カウンタ   DELIMITED BY SIZE
                  ")"            DELIMITED BY SIZE
             INTO 金属副子ＣＭＷ
           END-STRING.
019180*================================================================*
       金属回数設定 SECTION.
      *
           STRING 金属副子ＣＭＷ           DELIMITED BY SPACE
                  金属回数Ｗ(部位カウンタ) DELIMITED BY SIZE
                  "回"                     DELIMITED BY SIZE
             INTO 金属副子ＣＭＷ
           END-STRING.
019180*================================================================*
       金属加算日設定 SECTION.
      *
           STRING 金属副子ＣＭＷ           DELIMITED BY SPACE
                  "("                      DELIMITED BY SIZE
             INTO 金属副子ＣＭＷ
           END-STRING.
026320     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 3
               IF 金属算定日Ｗ(部位カウンタ カウンタ) NOT = ZERO
                   IF カウンタ > 1
                       STRING 金属副子ＣＭＷ           DELIMITED BY SPACE
                              ","                      DELIMITED BY SIZE
                         INTO 金属副子ＣＭＷ
                       END-STRING
                   END-IF
                   MOVE 金属算定日Ｗ(部位カウンタ カウンタ) TO 金属算定日ＺＷ
                   STRING 金属副子ＣＭＷ   DELIMITED BY SPACE
                          金属算定日ＺＷ   DELIMITED BY SIZE
                          "日"             DELIMITED BY SIZE
                     INTO 金属副子ＣＭＷ
                   END-STRING
                   PERFORM 金属文字詰
               END-IF
           END-PERFORM.
           STRING 金属副子ＣＭＷ           DELIMITED BY SPACE
                  ")"                      DELIMITED BY SIZE
             INTO 金属副子ＣＭＷ
           END-STRING.
019180*================================================================*
       金属加算日設定年月 SECTION.
      *
           STRING 金属副子ＣＭＷ           DELIMITED BY SPACE
                  "("                      DELIMITED BY SIZE
             INTO 金属副子ＣＭＷ
           END-STRING.
026320     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 3
               IF 金属算定日Ｗ(部位カウンタ カウンタ) NOT = ZERO
                   IF カウンタ > 1
                       STRING 金属副子ＣＭＷ           DELIMITED BY SPACE
                              ","                      DELIMITED BY SIZE
                         INTO 金属副子ＣＭＷ
                       END-STRING
                   END-IF
                   MOVE 金属算定月Ｗ(部位カウンタ カウンタ) TO 金属算定月ＺＷ
                   MOVE 金属算定日Ｗ(部位カウンタ カウンタ) TO 金属算定日ＺＷ
                   STRING 金属副子ＣＭＷ   DELIMITED BY SPACE
                          金属算定月ＺＷ   DELIMITED BY SIZE
                          "/"              DELIMITED BY SIZE
                          金属算定日ＺＷ   DELIMITED BY SIZE
                     INTO 金属副子ＣＭＷ
                   END-STRING
                   PERFORM 金属文字詰
               END-IF
           END-PERFORM.
           STRING 金属副子ＣＭＷ           DELIMITED BY SPACE
                  ")"                      DELIMITED BY SIZE
             INTO 金属副子ＣＭＷ
           END-STRING.
019180*================================================================*
       金属文字詰 SECTION.
      *
026320     PERFORM VARYING カウンタ２ FROM 1 BY 1 UNTIL カウンタ２ >= 300
               IF 金属副子ＣＭＷ(カウンタ２:1) = " "
                   MOVE 金属副子ＣＭＷ(カウンタ２ + 1:300 - カウンタ２ + 1)
                     TO 金属副子ＣＭＷ(カウンタ２:300 - カウンタ２)
               END-IF
           END-PERFORM.
019180*================================================================*
       金属通算加算日設定 SECTION.
      *
           STRING 金属副子ＣＭＷ           DELIMITED BY SPACE
                  "("                      DELIMITED BY SIZE
             INTO 金属副子ＣＭＷ
           END-STRING.
026320     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 3
      *         IF 金属算定日Ｗ(部位カウンタ カウンタ) NOT = ZERO
                   IF カウンタ > 1
                       STRING 金属副子ＣＭＷ           DELIMITED BY SPACE
                              ",|"                     DELIMITED BY SIZE
                         INTO 金属副子ＣＭＷ
                       END-STRING
                   END-IF
      *
                   EVALUATE 金属算定和暦Ｗ(部位カウンタ カウンタ)
                   WHEN 4
                       MOVE "H"   TO 金属算定和暦ＣＭ
                   WHEN OTHER
                       MOVE SPACE TO 金属算定和暦ＣＭ
                   END-EVALUATE
                   IF 金属算定和暦年月日Ｗ(部位カウンタ カウンタ) NOT = ZERO
                       MOVE 金属算定年Ｗ(部位カウンタ カウンタ) TO 金属算定年ＺＷ
                       MOVE 金属算定月Ｗ(部位カウンタ カウンタ) TO 金属算定月ＺＷ
                       MOVE 金属算定日Ｗ(部位カウンタ カウンタ) TO 金属算定日ＺＷ
                       MOVE カウンタ TO 部位番号Ｗ
                       STRING 金属副子ＣＭＷ   DELIMITED BY SPACE
                              部位番号Ｗ       DELIMITED BY SIZE
                              "回目"           DELIMITED BY SIZE
                              金属算定和暦ＣＭ DELIMITED BY SIZE
                              金属算定年ＺＷ   DELIMITED BY SIZE
                              "."              DELIMITED BY SIZE
                              金属算定月ＺＷ   DELIMITED BY SIZE
                              "/"              DELIMITED BY SIZE
                              金属算定日ＺＷ   DELIMITED BY SIZE
                         INTO 金属副子ＣＭＷ
                       END-STRING
                       PERFORM 金属文字詰
                   ELSE
                       MOVE カウンタ TO 部位番号Ｗ
                       STRING 金属副子ＣＭＷ   DELIMITED BY SPACE
                              部位番号Ｗ       DELIMITED BY SIZE
                              "回目"           DELIMITED BY SIZE
                              "|||||||"        DELIMITED BY SIZE
                         INTO 金属副子ＣＭＷ
                       END-STRING
                   END-IF
      *
      *             PERFORM 金属文字詰
      *         END-IF
           END-PERFORM.
           STRING 金属副子ＣＭＷ           DELIMITED BY SPACE
                  ")"                      DELIMITED BY SIZE
             INTO 金属副子ＣＭＷ
           END-STRING.
019180*================================================================*
019180*================================================================*
       金属通算加算日ケアバンク SECTION.
      *
           STRING 金属副子ＣＭＷ           DELIMITED BY SPACE
                  "("                      DELIMITED BY SIZE
             INTO 金属副子ＣＭＷ
           END-STRING.
026320     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 3
      *         IF 金属算定日Ｗ(部位カウンタ カウンタ) NOT = ZERO
                   IF カウンタ > 1
                       STRING 金属副子ＣＭＷ           DELIMITED BY SPACE
                              "|"                      DELIMITED BY SIZE
                         INTO 金属副子ＣＭＷ
                       END-STRING
                   END-IF
      *
                   IF 金属算定和暦年月日Ｗ(部位カウンタ カウンタ) NOT = ZERO
                       MOVE 金属算定年Ｗ(部位カウンタ カウンタ) TO 金属算定年ＺＷ
                       MOVE 金属算定月Ｗ(部位カウンタ カウンタ) TO 金属算定月ＺＷ
                       MOVE 金属算定日Ｗ(部位カウンタ カウンタ) TO 金属算定日ＺＷ
                       MOVE カウンタ TO 部位番号Ｗ
                       STRING 金属副子ＣＭＷ   DELIMITED BY SPACE
                              部位番号Ｗ       DELIMITED BY SIZE
                              "回目"           DELIMITED BY SIZE
                              金属算定年ＺＷ   DELIMITED BY SIZE
                              "."              DELIMITED BY SIZE
                              金属算定月ＺＷ   DELIMITED BY SIZE
                              "."              DELIMITED BY SIZE
                              金属算定日ＺＷ   DELIMITED BY SIZE
                         INTO 金属副子ＣＭＷ
                       END-STRING
                       PERFORM 金属文字詰
                   ELSE
                       MOVE カウンタ TO 部位番号Ｗ
                       STRING 金属副子ＣＭＷ   DELIMITED BY SPACE
                              部位番号Ｗ       DELIMITED BY SIZE
                              "回目"           DELIMITED BY SIZE
                              "|||||||"        DELIMITED BY SIZE
                         INTO 金属副子ＣＭＷ
                       END-STRING
                   END-IF
      *
      *             PERFORM 金属文字詰
      *         END-IF
           END-PERFORM.
           STRING 金属副子ＣＭＷ           DELIMITED BY SPACE
                  ")"                      DELIMITED BY SIZE
             INTO 金属副子ＣＭＷ
           END-STRING.
019180*================================================================*
       金属通算回数判定 SECTION.
      *
           MOVE ZERO TO 金属副子ＣＮＴ.
           MOVE 負－部位コード    (部位カウンタ) TO 部位コードＷ.
           MOVE 負－開始和暦年月日(部位カウンタ) TO 開始和暦年月日Ｗ.
038430     MOVE 開始和暦年月Ｗ     TO 負－施術和暦年月
038420     MOVE 連金運－患者コード TO 負－患者コード
      *
044890     START 負傷データＦ   KEY IS >= 負－患者コード
001000                                    負－施術和暦年月
044910     END-START
044920     IF 状態キー = "00"
038470         READ 負傷データＦ NEXT
038480         NOT AT END
038550             IF 連金運－患者コード = 負－患者コード
026320                 PERFORM VARYING 部位カウンタ２ FROM 1 BY 1 UNTIL 部位カウンタ２ > 負－部位数
                           IF (負－部位コード    (部位カウンタ２) = 部位コードＷ    ) AND
                              (負－開始和暦年月日(部位カウンタ２) = 開始和暦年月日Ｗ)
                               MOVE 負－施術和暦年月 TO 施術和暦年月Ｗ
                               MOVE 部位カウンタ２ TO 部位カウンタ３
                               MOVE 7 TO 部位カウンタ２
                           END-IF
                       END-PERFORM
                       MOVE 連金運－患者コード TO 施記－患者コード
                       MOVE 開始和暦年月日Ｗ   TO 施記－施術和暦年月日 施術和暦年月日Ｗ
044890                 START 施術記録Ｆ   KEY IS >= 施記－患者コード
044900                                              施記－施術和暦年月日
044910                 END-START
044920                 IF 状態キー = "00"
044930                     MOVE SPACE TO 終了フラグ
044950                     PERFORM 施術記録Ｆ読込
044960                     PERFORM UNTIL (終了フラグ           = "YES"               ) OR
044970                                   (施記－患者コード NOT = 連金運－患者コード  ) OR
044980                                   (施記－施術和暦年月   > 連金運－施術和暦年月)
                               IF 施記－施術和暦年月 NOT = 施術和暦年月Ｗ
                                   PERFORM 金属月跨処理
                                   MOVE 施記－施術和暦年月日 TO 施術和暦年月日Ｗ
                               END-IF
                               IF 施記－金属副子区分(部位カウンタ３) NOT = ZERO
                                   COMPUTE 金属副子ＣＮＴ = 金属副子ＣＮＴ + 1
      */標準で初検からの算定日を記載/20180820
                                   EVALUATE 連金運－会コード
                                   WHEN 45
                                       MOVE 施記－施術和暦年月日 TO 金属算定和暦年月日Ｗ (部位カウンタ 金属副子ＣＮＴ)
      *                             WHEN OTHER
                                   WHEN 02
                                   WHEN 04
                                   WHEN 06
      */日整大阪変更/300825
      *                             WHEN 07
                                   WHEN 13
                                   WHEN 25
                                   WHEN 30
                                   WHEN 34
                                   WHEN 35
                                   WHEN 36
                                   WHEN 42
                                   WHEN 49
                                       IF 施記－施術和暦年月 = 連金運－施術和暦年月
                                           MOVE 施記－施術和暦年月日 TO 金属算定和暦年月日Ｗ      (部位カウンタ 金属副子ＣＮＴ)
                                                                        連金運－金属副子和暦年月日(部位カウンタ 金属副子ＣＮＴ)
                                       END-IF
                                   WHEN OTHER
                                       MOVE 施記－施術和暦年月日 TO 金属算定和暦年月日Ｗ (部位カウンタ 金属副子ＣＮＴ)
                                   END-EVALUATE
                                   IF 金属副子ＣＮＴ >= 3
                                       MOVE "YES" TO 終了フラグ
                                   END-IF
                               END-IF
044950                         PERFORM 施術記録Ｆ読込
                           END-PERFORM
                       END-IF
      *
                   END-IF
038570         END-READ
           END-IF.
HILO  *     PERFORM VARYING 部位カウンタ３ FROM 1 BY 1 UNTIL 部位カウンタ３ > 部位数Ｗ
HILO  *         DISPLAY "金属算定日(" 部位カウンタ３ ")"    金属算定和暦年月日Ｗ(部位カウンタ３ 1) " " 
HILO  *         金属算定和暦年月日Ｗ(部位カウンタ３ 2) " " 金属算定和暦年月日Ｗ(部位カウンタ３ 3)
HILO  *     END-PERFORM.
HILO  *     PERFORM VARYING 部位カウンタ３ FROM 1 BY 1 UNTIL 部位カウンタ３ > 部位数Ｗ
HILO  *         DISPLAY "k-3 " 部位カウンタ３ " " 連金運－金属副子和暦年月日(部位カウンタ３ 1)
HILO  *         " " 連金運－金属副子和暦年月日(部位カウンタ３ 2)
HILO  *         " " 連金運－金属副子和暦年月日(部位カウンタ３ 3)
HILO  *     END-PERFORM.
019180*================================================================*
       金属月跨処理 SECTION.
      *
008580     MOVE 施記－施術和暦   TO 負－施術和暦.
008590     MOVE 施記－施術年     TO 負－施術年.
008600     MOVE 施記－施術月     TO 負－施術月.
008610     MOVE 施記－患者番号   TO 負－患者番号.
008620     MOVE 施記－枝番       TO 負－枝番.
008630     READ 負傷データＦ
008640     INVALID KEY
               MOVE NC"負傷" TO ファイル名Ｗ
               PERFORM エラー表示Ｒ
008660     NOT INVALID KEY
           MOVE ZERO TO 部位カウンタ３
               PERFORM VARYING 部位カウンタ２ FROM 1 BY 1 UNTIL  部位カウンタ２ > 負－部位数
                   IF (負－部位コード    (部位カウンタ２) = 部位コードＷ      ) AND
                      (負－開始和暦年月日(部位カウンタ２) = 開始和暦年月日Ｗ)
                       MOVE 部位カウンタ２ TO 部位カウンタ３
                   END-IF
               END-PERFORM
           END-READ.
019180*================================================================*
019190 施術記録Ｆ読込 SECTION.
019200*
019210     READ 施術記録Ｆ NEXT
019220     AT END
019230         MOVE "YES" TO 終了フラグ
019240     END-READ.
019250*
019260*================================================================*
025880 遅延処理 SECTION.
025890*
025900     PERFORM VARYING 遅延カウンタ FROM 1 BY 1
025910             UNTIL 遅延カウンタ > 遅延回数Ｗ
025920         MOVE "YES" TO 遅延フラグ
025930     END-PERFORM.
025940*
025950*================================================================*
       金属新用紙合算 SECTION.
      *
011344     OPEN OUTPUT 作業ファイル１.
011345         MOVE NC"作１" TO ファイル名Ｗ.
011346         PERFORM オープンチェック.
           MOVE 連金運－患者コード   TO 施記－患者コード.
           MOVE 連金運－施術和暦年月 TO 施記－施術和暦年月.
           MOVE 1                    TO 施記－施術日.
044890     START 施術記録Ｆ   KEY IS >= 施記－患者コード
044900                                  施記－施術和暦年月日
044910     END-START.
044920     IF 状態キー = "00"
044930         MOVE SPACE TO 終了フラグ
044950         PERFORM 施術記録Ｆ読込
044960         PERFORM UNTIL (終了フラグ             = "YES"               ) OR
044970                       (施記－患者コード   NOT = 連金運－患者コード  ) OR
044980                       (施記－施術和暦年月 NOT = 連金運－施術和暦年月)
026320             PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL 部位カウンタ > 部位数Ｗ
                       IF 施記－金属副子区分(部位カウンタ) NOT = ZERO
                           MOVE 施記－施術和暦年月日 TO 作１－金属算定和暦年月日
019340                     WRITE 作１－レコード
019350                     INVALID KEY
                               IF 状態キー NOT = 22
019360                             MOVE NC"作１"  TO ファイル名Ｗ
019370                             PERFORM エラー表示
                               END-IF
019380                     END-WRITE
                       END-IF
                   END-PERFORM
044950             PERFORM 施術記録Ｆ読込
               END-PERFORM
           END-IF.
011344     CLOSE 作業ファイル１.
           PERFORM 遅延処理.
011344     OPEN INPUT 作業ファイル１.
011345         MOVE NC"作１" TO ファイル名Ｗ.
011346         PERFORM オープンチェック.
      *
           MOVE ZERO TO 作１－金属算定和暦年月日.
044890     START 作業ファイル１ KEY IS >= 作１－金属算定和暦年月日
044910     END-START.
044920     IF 状態キー = "00"
044930         MOVE SPACE TO 終了フラグ
044950         PERFORM 作業ファイル１読込
026320         PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL (カウンタ > 3 ) OR (終了フラグ２ NOT = SPACE)
      */合算時は１部位目に転記する。
                   MOVE 作１－金属算定和暦年月日 TO 連金運－金属副子和暦年月日(1 カウンタ)
044950         PERFORM 作業ファイル１読込
               END-PERFORM
           END-IF
011344     CLOSE 作業ファイル１.
025950*================================================================*
       運動新用紙標準 SECTION.
      *
026320     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 5
              MOVE レセ－運動算定日(カウンタ) TO 連金運－運動日(カウンタ)
           END-PERFORM.
025950*================================================================*
       金属加算日合算大阪 SECTION.
      *
      */合算した回数を３つまで記載する。
011344     OPEN INPUT 作業ファイル１.
011345         MOVE NC"作１" TO ファイル名Ｗ.
011346         PERFORM オープンチェック.
      *
           MOVE ZERO TO 作１－金属算定和暦年月日.
044890     START 作業ファイル１ KEY IS >= 作１－金属算定和暦年月日
044910     END-START.
044920     IF 状態キー = "00"
044930         MOVE SPACE TO 終了フラグ２
044950         PERFORM 作業ファイル１読込
026320         PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 3
                   IF (作１－金属算定和暦年月日 NOT = ZERO) AND (終了フラグ２ = SPACE)
                       IF カウンタ > 1
                           STRING 金属副子ＣＭＷ DELIMITED BY SPACE
                                  ","            DELIMITED BY SIZE
                             INTO 金属副子ＣＭＷ
                           END-STRING
                       END-IF
                       MOVE 作１－金属算定日 TO 金属算定日ＺＷ
                       STRING 金属副子ＣＭＷ   DELIMITED BY SPACE
                              金属算定日ＺＷ   DELIMITED BY SIZE
                              "日"             DELIMITED BY SIZE
                         INTO 金属副子ＣＭＷ
                       END-STRING
                       PERFORM 金属文字詰
                   END-IF
044950             PERFORM 作業ファイル１読込
               END-PERFORM
           END-IF
011344     CLOSE 作業ファイル１.
019180*================================================================*
       運動算定日大阪 SECTION.
      *
026320     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 5
               IF レセ－運動算定日(カウンタ) NOT = ZERO
                   IF カウンタ > 1
                       STRING 金属副子ＣＭＷ DELIMITED BY SPACE
                              ","            DELIMITED BY SIZE
                         INTO 金属副子ＣＭＷ
                       END-STRING
                   END-IF
                   MOVE レセ－運動算定日(カウンタ) TO 運動算定日ＺＷ
                   STRING 金属副子ＣＭＷ DELIMITED BY SPACE
                          運動算定日ＺＷ DELIMITED BY SIZE
                          "日"           DELIMITED BY SIZE
                     INTO 金属副子ＣＭＷ
                   END-STRING
                   PERFORM 金属文字詰
               END-IF
           END-PERFORM.
019180*================================================================*
       項目初期化 SECTION.
      *
           MOVE SPACE TO 連金運－金属副子ＣＭ
                         連金運－運動後療ＣＭ
                         連金運２－金属副子ＣＭ.
026320     PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL 部位カウンタ > 5
               MOVE SPACE TO 連金運－金属副子部位(部位カウンタ)
               INITIALIZE    連金運－金属副子部位(部位カウンタ)
               MOVE ZERO TO  連金運－運動日      (部位カウンタ)
           END-PERFORM.
025950*================================================================*
       金属新用紙標準 SECTION.
      *
      */
           PERFORM 金属１部位目回数取得.
      */2部位目以降がある場合
           PERFORM 金属標準.
025950*================================================================*
       金属１部位目回数取得 SECTION.
      *
      */算定している最初の１部位分のみ転記する
           MOVE SPACE TO 終了フラグ２.
           MOVE ZERO  TO 部位カウンタ４.
026320     PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL (部位カウンタ > 部位数Ｗ) OR (終了フラグ２ NOT = SPACE)
               IF 金属回数Ｗ(部位カウンタ) NOT = ZERO
                   MOVE 連金運－患者コード TO 施記－患者コード
                   MOVE 連金運－施術和暦年月 TO 施記－施術和暦年月
                   MOVE 1                    TO 施記－施術日
                   MOVE 施記－施術和暦年月日 TO 施術和暦年月日Ｗ
044890             START 施術記録Ｆ   KEY IS >= 施記－患者コード
044900                                          施記－施術和暦年月日
044910             END-START
044920             IF 状態キー = "00"
044930                 MOVE SPACE TO 終了フラグ
044950                 PERFORM 施術記録Ｆ読込
044960                 PERFORM UNTIL (終了フラグ           = "YES"               ) OR
044970                               (施記－患者コード NOT = 連金運－患者コード  ) OR
044980                               (施記－施術和暦年月   > 連金運－施術和暦年月)
                           IF 施記－金属副子区分(部位カウンタ) NOT = ZERO
                               COMPUTE 金属副子ＣＮＴ = 金属副子ＣＮＴ + 1
                               MOVE 施記－施術和暦年月日 TO 連金運－金属副子和暦年月日(1 金属副子ＣＮＴ)
                               IF 金属副子ＣＮＴ >= 3
                                   MOVE "YES" TO 終了フラグ
                               END-IF
                               COMPUTE 部位カウンタ４ = 部位カウンタ + 1
                               MOVE "YES" TO 終了フラグ２
                           END-IF
044950                     PERFORM 施術記録Ｆ読込
                       END-PERFORM
                   END-IF
               END-IF
           END-PERFORM.
025950*================================================================*
       金属新用紙標準２ SECTION.
      *
      */初検からの日付を出す。１部位は○月○("日"はプレ印刷)。２部位以降はYY.MM.DD。/20180820
           MOVE "金属副子等加算:" TO 金属副子ＣＭＷ.
           PERFORM 金属１部位目回数取得２.
           IF 金属副子ＣＭＷ = "金属副子等加算:" MOVE SPACE TO 金属副子ＣＭＷ END-IF.
025950*================================================================*
       金属１部位目回数取得２ SECTION.
      *
      */部位毎に(部位番号)・加算日(年月日))
      */例) (2)(30.6.1 30.7.5 30.8.31)
      *
           MOVE ZERO TO 印刷カウンタ.
           MOVE SPACE TO 金属算定和暦年月日Ｗ１ 金属副子ＣＭＷ２.
           INITIALIZE    金属算定和暦年月日Ｗ１ 対象カウンタ.
           MOVE SPACE TO 終了フラグ２.
026320*     PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL (部位カウンタ > 部位数Ｗ OR 終了フラグ２ NOT = SPACE)
026320     PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL 部位カウンタ > 部位数Ｗ
               IF 金属回数Ｗ(部位カウンタ) > ZERO
                   MOVE "YES" TO 終了フラグ２
                   COMPUTE 印刷カウンタ = 印刷カウンタ + 1
                   IF 印刷カウンタ <= 5
                       COMPUTE 対象カウンタ = 対象カウンタ + 1
      *                 IF 対象カウンタ = 3
      *                     MOVE 金属副子ＣＭＷ TO 金属副子ＣＭＷ２１
      *                     MOVE SPACE          TO 金属副子ＣＭＷ
      *                 END-IF
      */当月に金属副子を算定している部位の算定日を１回目から判定する
                       PERFORM 金属通算回数判定
                       IF 印刷カウンタ = 1
                           MOVE 金属算定和暦年月日Ｗ (部位カウンタ 1) TO  連金運－金属副子和暦年月日(1 1)
                           MOVE 金属算定和暦年月日Ｗ (部位カウンタ 2) TO  連金運－金属副子和暦年月日(1 2)
                           MOVE 金属算定和暦年月日Ｗ (部位カウンタ 3) TO  連金運－金属副子和暦年月日(1 3)
                       ELSE
      */部位番号
                           PERFORM 新用紙金属部位番号設定
      */加算通算回数と日
                           PERFORM 新用紙金属通算加算日
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.
      *     IF 対象カウンタ >= 3
      *         MOVE 金属副子ＣＭＷ TO 金属副子ＣＭＷ２２
      *         MOVE SPACE          TO 金属副子ＣＭＷ
      *     END-IF.
HILO  *     DISPLAY "金属副子ＣＭＷ " 金属副子ＣＭＷ.
HILO  *     DISPLAY "金属副子ＣＭＷ２１" 金属副子ＣＭＷ２１
HILO  *     DISPLAY "金属副子ＣＭＷ２２" 金属副子ＣＭＷ２２
019180*================================================================*
       新用紙金属通算加算日 SECTION.
      *
           STRING 金属副子ＣＭＷ           DELIMITED BY SPACE
                  "("                      DELIMITED BY SIZE
             INTO 金属副子ＣＭＷ
           END-STRING.
026320     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 3
      *         IF 金属算定日Ｗ(部位カウンタ カウンタ) NOT = ZERO
                   IF (カウンタ > 1) AND (金属算定日Ｗ(部位カウンタ カウンタ) NOT = ZERO)
                       STRING 金属副子ＣＭＷ          DELIMITED BY SPACE
                              "||"                    DELIMITED BY SIZE
                         INTO 金属副子ＣＭＷ
                       END-STRING
                   END-IF
      *
                   IF 金属算定和暦年月日Ｗ(部位カウンタ カウンタ) NOT = ZERO
                       MOVE 金属算定年Ｗ(部位カウンタ カウンタ) TO 金属算定年ＺＷ
                       MOVE 金属算定月Ｗ(部位カウンタ カウンタ) TO 金属算定月ＺＷ
                       MOVE 金属算定日Ｗ(部位カウンタ カウンタ) TO 金属算定日ＺＷ
                       MOVE カウンタ TO 部位番号Ｗ
                       STRING 金属副子ＣＭＷ   DELIMITED BY SPACE
      *                        部位番号Ｗ       DELIMITED BY SIZE
      *                        "回目"           DELIMITED BY SIZE
                              金属算定年ＺＷ   DELIMITED BY SIZE
                              "."              DELIMITED BY SIZE
                              金属算定月ＺＷ   DELIMITED BY SIZE
                              "."              DELIMITED BY SIZE
                              金属算定日ＺＷ   DELIMITED BY SIZE
                         INTO 金属副子ＣＭＷ
                       END-STRING
                       PERFORM 金属文字詰
      *             ELSE
      *                 MOVE カウンタ TO 部位番号Ｗ
      *                 STRING 金属副子ＣＭＷ   DELIMITED BY SPACE
      *                        部位番号Ｗ       DELIMITED BY SIZE
      *                        "回目"           DELIMITED BY SIZE
      *                        "|||||||"        DELIMITED BY SIZE
      *                   INTO 金属副子ＣＭＷ
      *                 END-STRING
                   END-IF
      *
      *             PERFORM 金属文字詰
      *         END-IF
           END-PERFORM.
           STRING 金属副子ＣＭＷ           DELIMITED BY SPACE
                  ")"                      DELIMITED BY SIZE
             INTO 金属副子ＣＭＷ
           END-STRING.
019180*================================================================*
       新用紙金属部位番号設定 SECTION.
      *
           STRING 金属副子ＣＭＷ DELIMITED BY SPACE
                  "|("           DELIMITED BY SIZE
                  部位カウンタ   DELIMITED BY SIZE
                  ")"            DELIMITED BY SIZE
             INTO 金属副子ＣＭＷ
           END-STRING.
019180*================================================================*
      * 金属１部位目回数取得２ SECTION.
      **
      **/算定している最初の１部位分のみ転記する
      **
      **/部位毎に(部位番号・回数(通算)・加算日(年月日))
      **/例) (1)(1回目30.6.1 2回目30.7.5 3回目30.8.31
      **
      *     MOVE ZERO TO 印刷カウンタ.
      *     MOVE SPACE TO 金属算定和暦年月日Ｗ１ 金属副子ＣＭＷ２.
      *     INITIALIZE    金属算定和暦年月日Ｗ１ 対象カウンタ.
      *     MOVE SPACE TO 終了フラグ２.
      *     MOVE "金属副子等加算:" TO 金属副子ＣＭＷ.
026320**     PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL (部位カウンタ > 部位数Ｗ OR 終了フラグ２ NOT = SPACE)
026320*     PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL 部位カウンタ > 部位数Ｗ
      *         IF 金属回数Ｗ(部位カウンタ) > ZERO
      *             MOVE "YES" TO 終了フラグ２
      *             COMPUTE 印刷カウンタ = 印刷カウンタ + 1
      *             IF 印刷カウンタ <= 5
      *                 COMPUTE 対象カウンタ = 対象カウンタ + 1
      *                 IF 対象カウンタ = 4
      *                     MOVE 金属副子ＣＭＷ TO 金属副子ＣＭＷ２１
      *                     MOVE SPACE          TO 金属副子ＣＭＷ
      *                 END-IF
      **/当月に金属副子を算定している部位の算定日を１回目から判定する
      *                 PERFORM 金属通算回数判定
      *                 IF 印刷カウンタ = 1
      *                     MOVE 金属算定和暦年月日Ｗ (部位カウンタ 1) TO  連金運－金属副子和暦年月日(1 1)
      *                     MOVE 金属算定和暦年月日Ｗ (部位カウンタ 2) TO  連金運－金属副子和暦年月日(1 2)
      *                     MOVE 金属算定和暦年月日Ｗ (部位カウンタ 3) TO  連金運－金属副子和暦年月日(1 3)
      *                 ELSE
      **/部位番号
      *                     PERFORM 金属部位番号設定
      **/加算通算回数と日
      *                     PERFORM 金属通算加算日ケアバンク
      *                 END-IF
      *             END-IF
      *         END-IF
      *     END-PERFORM.
      *     IF 対象カウンタ >= 4
      *         MOVE 金属副子ＣＭＷ TO 金属副子ＣＭＷ２２
      *         MOVE SPACE          TO 金属副子ＣＭＷ
      *     END-IF.
      *
HILO  *     DISPLAY "金属副子ＣＭＷ " 金属副子ＣＭＷ.
HILO  *     DISPLAY "金属副子ＣＭＷ２１" 金属副子ＣＭＷ２１
HILO  *     DISPLAY "金属副子ＣＭＷ２２" 金属副子ＣＭＷ２２
019180*================================================================*
       金属標準２ SECTION.
      *
      */部位毎に(部位番号)・加算日(年月日))
      */例) (1)(30.6.1 30.7.5 30.8.31)
      *
           MOVE ZERO TO 印刷カウンタ.
           MOVE SPACE TO 金属算定和暦年月日Ｗ１ 金属副子ＣＭＷ２.
           INITIALIZE    金属算定和暦年月日Ｗ１ 対象カウンタ.
           MOVE SPACE TO 終了フラグ２.
           MOVE "金属副子等加算:" TO 金属副子ＣＭＷ.
           PERFORM 金属合算回数.
           INSPECT 金属副子ＣＭＷ   REPLACING ALL "|" BY " ".
026320*     PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL (部位カウンタ > 部位数Ｗ OR 終了フラグ２ NOT = SPACE)
026320     PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL 部位カウンタ > 部位数Ｗ
               IF 金属回数Ｗ(部位カウンタ) > ZERO
                   MOVE "YES" TO 終了フラグ２
                   COMPUTE 印刷カウンタ = 印刷カウンタ + 1
                   IF 印刷カウンタ <= 5
                       COMPUTE 対象カウンタ = 対象カウンタ + 1
      *                 IF 対象カウンタ = 3
      *                     MOVE 金属副子ＣＭＷ TO 金属副子ＣＭＷ２１
      *                     MOVE SPACE          TO 金属副子ＣＭＷ
      *                 END-IF
      */当月に金属副子を算定している部位の算定日を１回目から判定する
                       PERFORM 金属通算回数判定
      */部位番号
                       PERFORM 新用紙金属部位番号設定
      */加算通算回数と日
                       PERFORM 新用紙金属通算加算日
                   END-IF
               END-IF
           END-PERFORM.
      *     IF 対象カウンタ >= 3
      *         MOVE 金属副子ＣＭＷ TO 金属副子ＣＭＷ２２
      *         MOVE SPACE          TO 金属副子ＣＭＷ
      *     END-IF.
HILO  *     DISPLAY "金属副子ＣＭＷ " 金属副子ＣＭＷ.
HILO  *     DISPLAY "金属副子ＣＭＷ２１" 金属副子ＣＭＷ２１
HILO  *     DISPLAY "金属副子ＣＭＷ２２" 金属副子ＣＭＷ２２
019180*================================================================*
       材料算定日取得 SECTION.
      *
           MOVE 連金運－患者コード   TO 施記－患者コード.
           MOVE 連金運－施術和暦年月 TO 施記－施術和暦年月.
           MOVE 1                    TO 施記－施術日.
044890     START 施術記録Ｆ   KEY IS >= 施記－患者コード
044900                                  施記－施術和暦年月日
044910     END-START.
044920     IF 状態キー = "00"
044930         MOVE SPACE TO 終了フラグ
044950         PERFORM 施術記録Ｆ読込
044960         PERFORM UNTIL (終了フラグ             = "YES"               ) OR
044970                       (施記－患者コード   NOT = 連金運－患者コード  ) OR
044980                       (施記－施術和暦年月 NOT = 連金運－施術和暦年月)
026320             PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL 部位カウンタ > 部位数Ｗ
                       IF 施記－特別材料区分(部位カウンタ) NOT = ZERO
      *                     COMPUTE 金属回数Ｗ(部位カウンタ) = 金属回数Ｗ(部位カウンタ) + 1
026320                     PERFORM VARYING カウンタ FROM 1 BY 1 UNTIL カウンタ > 3
                               IF 金属算定日Ｗ(部位カウンタ カウンタ) = ZERO
                                   MOVE 施記－施術和暦年月日 TO 金属算定和暦年月日Ｗ(部位カウンタ カウンタ)
                                                          連金運－金属副子和暦年月日(部位カウンタ カウンタ)
                                   MOVE 3 TO カウンタ
                               END-IF
                           END-PERFORM
                       END-IF
                   END-PERFORM
044950             PERFORM 施術記録Ｆ読込
               END-PERFORM
           END-IF.
HILO********************************************************************************>HILO
HILO  *     PERFORM VARYING 部位カウンタ FROM 1 BY 1 UNTIL 部位カウンタ > 部位数Ｗ
HILO  *         DISPLAY "Zai-2 " 部位カウンタ " " カウンタ " "
HILO  *                 金属算定和暦年月日Ｗ(部位カウンタ 1) " "
HILO  *                 金属算定和暦年月日Ｗ(部位カウンタ 2) " "
HILO  *                 金属算定和暦年月日Ｗ(部位カウンタ 3)
HILO  *     END-PERFORM.
HILO********************************************************************************>HILO
019180*================================================================*
       材料文字詰 SECTION.
      *
026320     PERFORM VARYING カウンタ２ FROM 1 BY 1 UNTIL カウンタ２ >= 300
               IF 材料費ＣＭＷ(カウンタ２:1) = " "
                   MOVE 材料費ＣＭＷ(カウンタ２ + 1:300 - カウンタ２ + 1)
                     TO 材料費ＣＭＷ(カウンタ２:300 - カウンタ２)
               END-IF
           END-PERFORM.
019180*================================================================*
056222******************************************************************
056223 END PROGRAM ZAIRYO.
056224******************************************************************
