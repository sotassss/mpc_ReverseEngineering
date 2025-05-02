000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             KGNCHK.
000060 AUTHOR.                 池田　幸子
000070*
000080*----------------------------------------------------------------*
000090*         使用許可と使用期限の更新チェック
000110*----------------------------------------------------------------*
000120 DATE-WRITTEN.           2016-03-01
000130 DATE-COMPILED.          2016-03-01
000140*----------------------------------------------------------------*
000150******************************************************************
000160*            ENVIRONMENT         DIVISION                        *
000170******************************************************************
000180 ENVIRONMENT             DIVISION.
000190 CONFIGURATION           SECTION.
000200 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000210 OBJECT-COMPUTER.        FMV-DESKPOWER.
000230 SPECIAL-NAMES.          CONSOLE  IS  CONS
000240                         SYSERR   IS  MSGBOX.
000250 INPUT-OUTPUT            SECTION.
000260 FILE-CONTROL.
           SELECT  顧客情報マスタ  ASSIGN      TO        KOKYAKUL
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  顧客－区分
                                   FILE STATUS              IS  状態キー
                                   LOCK        MODE         IS  AUTOMATIC.
           SELECT  顧客期限リスト  ASSIGN      TO        "C:\MAKISHISYS\UPDOBJ\kklst.dat"
                                   ORGANIZATION             IS  INDEXED
                                   ACCESS MODE              IS  DYNAMIC
                                   RECORD KEY               IS  期限－区分
                                                                期限－顧客番号
                                   FILE STATUS              IS  状態キー
                                   LOCK        MODE         IS  AUTOMATIC.
      *
000400******************************************************************
000410*                      DATA DIVISION                             *
000420******************************************************************
000430 DATA                    DIVISION.
000810 FILE                    SECTION.
       FD  顧客情報マスタ      BLOCK   CONTAINS   1   RECORDS.
           COPY KOKYAKU         OF  XFDLIB  JOINING   顧客   AS  PREFIX.
      *
001190 FD  顧客期限リスト RECORD  CONTAINS 64 CHARACTERS.
001200 01 期限－レコード.
001210    03 期限－レコードキー.
             05 期限－区分                   PIC 9(1).
             05 期限－顧客番号               PIC 9(5).
001280    03 期限－レコードデータ.
             05 期限－使用状態               PIC 9(1).
004780       05 期限－使用期限年月日.
004800          07 期限－使用期限年          PIC 9(4).
004810          07 期限－使用期限月          PIC 9(2).
004810          07 期限－使用期限日          PIC 9(2).
             05 FILLER                       PIC X(49).
000520******************************************************************
000530*                WORKING-STORAGE SECTION                         *
000540******************************************************************
000550 WORKING-STORAGE         SECTION.
001610 01 キー入力                           PIC X     VALUE SPACE.
001620 01 状態キー                           PIC X(2)  VALUE SPACE.
001630 01 終了フラグ                         PIC X(3)  VALUE SPACE.
001280 01 ファイル名Ｗ                       PIC N(5) VALUE SPACE.
001750 01 メッセージＷ                       PIC X(40) VALUE SPACE.
001830*
004710****************
004720* 連結項目待避 *
004730****************
004770 01 退避データＷ.
          03 区分Ｗ                          PIC 9(1)  VALUE ZERO.
          03 顧客番号Ｗ                      PIC 9(5)  VALUE ZERO.
          03 使用状態Ｗ                      PIC 9(1)  VALUE ZERO.
          03 使用期限使用フラグＷ            PIC 9(1)  VALUE ZERO.
          03 オンラインＤＬＷ                PIC 9(1)  VALUE ZERO.
004780    03 契約年月日Ｗ.
004800       05 契約年Ｗ                     PIC 9(4)  VALUE ZERO.
004810       05 契約月Ｗ                     PIC 9(2)  VALUE ZERO.
004810       05 契約日Ｗ                     PIC 9(2)  VALUE ZERO.
004780    03 使用期限年月日Ｗ.
004800       05 使用期限年Ｗ                 PIC 9(4)  VALUE ZERO.
004810       05 使用期限月Ｗ                 PIC 9(2)  VALUE ZERO.
004810       05 使用期限日Ｗ                 PIC 9(2)  VALUE ZERO.
004780    03 最終起動年月日Ｗ.
004800       05 最終起動年Ｗ                 PIC 9(4)  VALUE ZERO.
004810       05 最終起動月Ｗ                 PIC 9(2)  VALUE ZERO.
004810       05 最終起動日Ｗ                 PIC 9(2)  VALUE ZERO.
001830*
001660* 日付ＷＯＲＫ
001650 01 計算機西暦年Ｗ                     PIC 9(2).
001670 01 和暦終了年Ｗ                       PIC 9(4).
001680 01 計算機和暦年Ｗ                     PIC 9(2).
001690 01 計算機西暦.
001700    03 計算機西暦年                    PIC 9(4).
001710    03 計算機西暦月日                  PIC 9(4).
001720 01 計算機西暦Ｒ REDEFINES 計算機西暦.
001730    03 計算機世紀                      PIC 9(2).
001740    03 計算機日付                      PIC 9(6).
001750    03 計算機日付Ｒ REDEFINES 計算機日付.
001760       05 計算機年月                   PIC 9(4).
001770       05 計算機年月Ｒ REDEFINES 計算機年月.
001780         07 計算機年                   PIC 9(2).
001790         07 計算機月                   PIC 9(2).
001800       05 計算機日                     PIC 9(2).
003330*
      * C 連携用
       01  パラメタＷ      PIC X(80) VALUE SPACE.
       01  プログラム名Ｗ  PIC X(8)  VALUE "filexist".
      *
001831* POWER COBOL用
001832 01 dll-name  PIC X(260)  VALUE SPACE.
001833 01 form-name PIC X(14)   VALUE SPACE.
001834*
001600******************************************************************
001610*                          連結項目                              *
001620******************************************************************
      *
002491********************
002492* メッセージ表示キー *
002493********************
002494 01 連メ－キー IS EXTERNAL.
002495    03  連メ－メッセージ                 PIC N(20).
      *
       01 連メッセージＰ情報 IS EXTERNAL.
          03 連メＰ－メッセージ番号            PIC 9(2).
          03 連メＰ－メッセージ.
             05 連メＰ－メッセージ内容         PIC X(40) OCCURS 6.
          03 連メＰ－メッセージ１              PIC X(20).
          03 連メＰ－メッセージ２              PIC X(12).
002497*
       01 Ｈ連メイン－施術区分情報 IS EXTERNAL.
          03 Ｈ連メイン－施術区分              PIC 9.
001630********************************
001640* 使用期限のパラメータ *
001650********************************
       01 連使用期限情報 IS EXTERNAL.
          03 連使－許可Ｆ                  PIC 9(1).
      *
004130******************************************************************
004140*                      PROCEDURE  DIVISION                       *
004150******************************************************************
004160 PROCEDURE               DIVISION.
003290************
003300*           *
003310* 初期処理   *
003320*           *
003330************
      *
003340     PERFORM 初期化.
004792*
003350************
003360*           *
003370* 主処理     *
003380*           *
003390************
           PERFORM 顧客情報取得.
      *     IF 使用期限使用フラグＷ = 1
      *         IF オンラインＤＬＷ = 1
      *             PERFORM ダウンロード処理
      *         END-IF
               PERFORM 更新処理.
               PERFORM 日付チェック.
      *     ELSE
      *         MOVE ZERO    TO 連使－許可Ｆ
      *     END-IF.
      *     IF 連使－許可Ｆ = ZERO
      *         MOVE 計算機西暦           TO 顧客－最終起動年月日
      *         REWRITE 顧客－レコード
      *         END-REWRITE
      *     END-IF.
      *
003410************
003420*           *
003430* 終了処理   *
003440*           *
003450************
003460     PERFORM 終了処理.
004410     EXIT PROGRAM.
003500*================================================================*
003510 初期化 SECTION.
003520*
018000     OPEN I-O 顧客情報マスタ.
018010         MOVE NC"顧客" TO ファイル名Ｗ.
018020         PERFORM オープンチェック.
      *
003940*    /* 現在日付取得 */
003950     ACCEPT 計算機日付 FROM DATE.
003960*    /* 1980～2079年の間で設定 */
003970     IF 計算機年 > 80
003980         MOVE 19 TO 計算機世紀
003990     ELSE
004000         MOVE 20 TO 計算機世紀
004010     END-IF.
004040     COMPUTE 計算機西暦年Ｗ = 計算機西暦年 - 1988.
003860*================================================================*
003870 オープンチェック SECTION.
003880*
003890     IF 状態キー  NOT =  "00"
003900         DISPLAY ファイル名Ｗ NC"Ｆオープンエラー" UPON CONS
003910         DISPLAY NC"状態キー：" 状態キー           UPON CONS
003920         DISPLAY NC"数字１文字入力しＥＮＴＥＲキーを押してください"
003930                                                   UPON CONS
000080*-----------------------------------------*
000090         CALL "actcshm"  WITH C LINKAGE
000100*-----------------------------------------*
003940         ACCEPT  キー入力 FROM CONS
               MOVE 1    TO 連使－許可Ｆ
003950         PERFORM ファイル閉鎖
003960         EXIT PROGRAM.
003500*================================================================*
003510 顧客情報取得 SECTION.
003520*
           IF Ｈ連メイン－施術区分 = ZERO
              MOVE ZERO                   TO 顧客－区分
           ELSE
              MOVE 1                      TO 顧客－区分
           END-IF.
           READ 顧客情報マスタ
           INVALID KEY
               MOVE SPACE                 TO 顧客－レコード
           END-READ.
           MOVE 顧客－区分                TO 区分Ｗ.
           MOVE 顧客－顧客番号            TO 顧客番号Ｗ.
           MOVE 顧客－使用状態            TO 使用状態Ｗ.
           MOVE 顧客－使用期限使用フラグ  TO 使用期限使用フラグＷ.
           MOVE 顧客－オンラインＤＬ      TO オンラインＤＬＷ.
           MOVE 顧客－契約年              TO 契約年Ｗ.
           MOVE 顧客－契約月              TO 契約月Ｗ.
           MOVE 顧客－契約日              TO 契約日Ｗ.
           MOVE 顧客－期限年              TO 使用期限年Ｗ.
           MOVE 顧客－期限月              TO 使用期限月Ｗ.
           MOVE 顧客－期限日              TO 使用期限日Ｗ.
           MOVE 顧客－最終起動年          TO 最終起動年Ｗ.
           MOVE 顧客－最終起動月          TO 最終起動月Ｗ.
           MOVE 顧客－最終起動日          TO 最終起動日Ｗ.
003500*================================================================*
003510 ダウンロード処理 SECTION.
003520*
           MOVE "C:\MAKISHISYS\YAWOBJ\getcollect.exe" TO  パラメタＷ.
           CALL プログラム名Ｗ WITH C LINKAGE  USING BY REFERENCE パラメタＷ.
      *
      *     IF PROGRAM-STATUS = ZERO
              CALL   パラメタＷ
              CANCEL パラメタＷ
              IF (PROGRAM-STATUS NOT = ZERO) AND (使用状態Ｗ = ZERO)
005830           MOVE SPACE  TO  連メッセージＰ情報
                 MOVE 7      TO  連メＰ－メッセージ番号
005840           MOVE "顧客コードが取得できません。"       TO 連メＰ－メッセージ内容(1)
005840           MOVE "システム管理者に連絡してください。" TO 連メＰ－メッセージ内容(2)
004793           MOVE "PMSG001.DLL" TO dll-name
004794           MOVE "PMSG001"     TO form-name
004795           CALL "POWEROPENSHEET" USING dll-name form-name
              END-IF.
      *     ELSE
      *         MOVE "35"     TO 状態キー
018010*         MOVE NC"ＤＬ" TO ファイル名Ｗ
018020*         PERFORM オープンチェック
      *     END-IF.
003500*================================================================*
003510 更新処理 SECTION.
003520*
018000     OPEN INPUT 顧客期限リスト.
018010         MOVE NC"リスト" TO ファイル名Ｗ.
               IF 状態キー  NOT =  "00" AND "35"
018020             PERFORM オープンチェック
               END-IF.
      *
           MOVE 区分Ｗ                    TO 期限－区分.
           MOVE 顧客番号Ｗ                TO 期限－顧客番号.
           READ 顧客期限リスト
           INVALID KEY
005830         MOVE SPACE  TO  連メッセージＰ情報
               MOVE 7      TO  連メＰ－メッセージ番号
005840         MOVE "顧客コードが取得できません。"       TO 連メＰ－メッセージ内容(1)
005840         MOVE "システム管理者に連絡してください。" TO 連メＰ－メッセージ内容(2)
004793         MOVE "PMSG001.DLL" TO dll-name
004794         MOVE "PMSG001"     TO form-name
004795         CALL "POWEROPENSHEET" USING dll-name form-name
           NOT INVALID KEY
               MOVE 期限－使用状態       TO 使用状態Ｗ
               MOVE 期限－使用期限年月日 TO 使用期限年月日Ｗ
               MOVE 期限－使用状態       TO 顧客－使用状態
               MOVE 期限－使用期限年月日 TO 顧客－期限年月日
               REWRITE 顧客－レコード
               END-REWRITE
           END-READ.
009220     CLOSE 顧客期限リスト.
003520*
003500*================================================================*
003510 日付チェック SECTION.
003520*
           EVALUATE TRUE
           WHEN 計算機西暦 < 最終起動年月日Ｗ
005830         MOVE SPACE  TO  連メッセージＰ情報
               MOVE 1      TO  連メＰ－メッセージ番号
005840         MOVE "システム日付が不正です。"       TO 連メＰ－メッセージ内容(1)
004793         MOVE "PMSG001.DLL" TO dll-name
004794         MOVE "PMSG001"     TO form-name
004795         CALL "POWEROPENSHEET" USING dll-name form-name
               MOVE 1    TO 連使－許可Ｆ
           WHEN 使用状態Ｗ = 2
005830         MOVE SPACE  TO  連メッセージＰ情報
               MOVE 10     TO  連メＰ－メッセージ番号
005840         MOVE "契約期間が終了しました。"               TO 連メＰ－メッセージ内容(1)
005840         MOVE "引き続き『ＴＨＥ 柔』をご利用いただく"  TO 連メＰ－メッセージ内容(2)
005840         MOVE "には、契約延長のお申し込みをお願いい"   TO 連メＰ－メッセージ内容(3)
005840         MOVE "たします。"                             TO 連メＰ－メッセージ内容(4)
004793         MOVE "PMSG001.DLL" TO dll-name
004794         MOVE "PMSG001"     TO form-name
004795         CALL "POWEROPENSHEET" USING dll-name form-name
               MOVE 1    TO 連使－許可Ｆ
           WHEN (使用状態Ｗ = 1) AND (計算機西暦 > 使用期限年月日Ｗ)
005830         MOVE SPACE  TO  連メッセージＰ情報
               MOVE 10     TO  連メＰ－メッセージ番号
005840         MOVE "振り込みが確認出来ない為、"             TO 連メＰ－メッセージ内容(1)
005840         MOVE "サービスのご利用を停止させていただき"   TO 連メＰ－メッセージ内容(2)
005840         MOVE "ました。ご利用を継続する場合は使用料"   TO 連メＰ－メッセージ内容(3)
005840         MOVE "の振り込みをお願いいたします。"         TO 連メＰ－メッセージ内容(4)
005840         MOVE "データの反映に最大１０営業日かかります。" TO 連メＰ－メッセージ内容(5)
005840         MOVE "振り込み後、連絡をお願いいたします。"   TO 連メＰ－メッセージ内容(6)
004793         MOVE "PMSG001.DLL" TO dll-name
004794         MOVE "PMSG001"     TO form-name
004795         CALL "POWEROPENSHEET" USING dll-name form-name
               MOVE 1    TO 連使－許可Ｆ
           WHEN (使用状態Ｗ = 1) AND (計算機西暦 <= 使用期限年月日Ｗ)
               STRING 使用期限年Ｗ               DELIMITED BY SIZE
                      "年"                       DELIMITED BY SIZE
                      使用期限月Ｗ               DELIMITED BY SIZE
                      "月"                       DELIMITED BY SIZE
                      使用期限日Ｗ               DELIMITED BY SIZE
                      "日までに振り込みが確認"   DELIMITED BY SIZE
                 INTO メッセージＷ
               END-STRING
005830         MOVE SPACE  TO  連メッセージＰ情報
               MOVE 10     TO  連メＰ－メッセージ番号
005840         MOVE "振り込みが確認出来ません。"             TO 連メＰ－メッセージ内容(1)
005840         MOVE メッセージＷ                             TO 連メＰ－メッセージ内容(2)
005840         MOVE "出来ない場合は、ご利用を停止させて"     TO 連メＰ－メッセージ内容(3)
005840         MOVE "いただきます。"                         TO 連メＰ－メッセージ内容(4)
005840         MOVE "データの反映に最大１０営業日かかります。" TO 連メＰ－メッセージ内容(5)
005840         MOVE "振り込み後、連絡をお願いいたします。"   TO 連メＰ－メッセージ内容(6)
004793         MOVE "PMSG001.DLL" TO dll-name
004794         MOVE "PMSG001"     TO form-name
004795         CALL "POWEROPENSHEET" USING dll-name form-name
               MOVE 0    TO 連使－許可Ｆ
           WHEN (使用状態Ｗ = ZERO) AND (計算機西暦 > 使用期限年月日Ｗ)
005830         MOVE SPACE  TO  連メッセージＰ情報
               MOVE 7      TO  連メＰ－メッセージ番号
005840         MOVE "顧客コードが取得できません。"       TO 連メＰ－メッセージ内容(1)
005840         MOVE "システム管理者に連絡してください。" TO 連メＰ－メッセージ内容(2)
004793         MOVE "PMSG001.DLL" TO dll-name
004794         MOVE "PMSG001"     TO form-name
004795         CALL "POWEROPENSHEET" USING dll-name form-name
               MOVE 1    TO 連使－許可Ｆ
           WHEN OTHER
               MOVE 0    TO 連使－許可Ｆ
           END-EVALUATE.
009150*================================================================*
009160 終了処理 SECTION.
009170*
009180     PERFORM ファイル閉鎖.
009090*================================================================*
009100 ファイル閉鎖 SECTION.
009110*
009130     CLOSE 顧客情報マスタ.
005570*================================================================*
005580******************************************************************
005590 END PROGRAM KGNCHK.
005600******************************************************************
