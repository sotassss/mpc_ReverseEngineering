000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             MOJI2.
000060 AUTHOR.                 佐野 誠
000070*
000080*------------------------------------------------------------------*
000090*      文字列複合化                                                *
000110*------------------------------------------------------------------*
000120 DATE-WRITTEN.           2012-06-01
000130 DATE-COMPILED.          2012-06-01
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
000266*
001020******************************************************************
001030*                      DATA DIVISION                             *
001040******************************************************************
001050 DATA                    DIVISION.
001060 FILE                    SECTION.
001190*
002080*----------------------------------------------------------------*
002090******************************************************************
002100*                WORKING-STORAGE SECTION                         *
002110******************************************************************
002120 WORKING-STORAGE         SECTION.
002130 01 キー入力                  PIC X    VALUE SPACE.
002140 01 状態キー                  PIC X(2) VALUE SPACE.
002173**
002183 01 変換データＷ              PIC X(100) VALUE SPACE.
002288 01 パスエリアＷ.
002289    03 パスＷ                 PIC X(4)  VALUE "SANO".
002290    03 患者番号文字Ｗ         PIC X(6)  VALUE SPACE.
002291    03 パス残りＷ             PIC X(10) VALUE SPACE.
002292*
002293 01 異常エリアＷ.
002294    03 異常記号フラグ         PIC X(3) VALUE SPACE.
002295    03 異常番号フラグ         PIC X(3) VALUE SPACE.
002296    03 異常記号コード         PIC 9(3) VALUE ZERO.
002297    03 異常番号コード         PIC 9(3) VALUE ZERO.
002298    03 異常記号コード文字     PIC X(3) VALUE ZERO.
002299    03 異常番号コード文字     PIC X(3) VALUE ZERO.
002300**
002580*
002590******************************************************************
002600*                          連結項目                              *
002610******************************************************************
002620*
002630* 暗号複合用
002640 01 連暗号複合−暗号情報 IS EXTERNAL.
002646    03 連暗号複合−入力情報.
002649       05 連暗号複合−記号               PIC X(24).
002650       05 連暗号複合−番号               PIC X(30).
002652       05 連暗号複合−暗号化項目.
002653         07 連暗号複合−暗号患者番号     PIC X(6).
002654         07 連暗号複合−暗号判定記号     PIC X.
002655         07 連暗号複合−暗号判定番号     PIC X.
002656         07 連暗号複合−暗号記号         PIC X(24).
002657         07 連暗号複合−暗号番号         PIC X(30).
002668    03 連暗号複合−出力情報.
002670       05 連暗号複合−複合した記号       PIC X(24).
002671       05 連暗号複合−複合した番号       PIC X(30).
002730*
002731*--------------------------------------------------------------*
002732*
002770 01 連メ１０−キー IS EXTERNAL.
002780    03  連メ１０−メッセージ１               PIC X(40).
002790    03  連メ１０−メッセージ２               PIC X(40).
002800    03  連メ１０−メッセージ３               PIC X(40).
002810    03  連メ１０−メッセージ４               PIC X(40).
002820    03  連メ１０−メッセージ５               PIC X(40).
002830    03  連メ１０−メッセージ６               PIC X(40).
002840*
003481******************************************************************
003482*                      PROCEDURE  DIVISION                       *
003483******************************************************************
003490 PROCEDURE               DIVISION.
003500************
003510*           *
003520* 初期処理   *
003530*           *
003540************
003541* 出力情報初期化
003550     MOVE SPACE TO 連暗号複合−出力情報.
003551*
003560************
003570*           *
003580* 主処理     *
003590*           *
003600************
003610*
003618     PERFORM 複合処理.
003619*
003620************
003630*           *
003640* 終了処理   *
003650*           *
003660************
003690     EXIT PROGRAM.
003700*
003710*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
003720*================================================================*
006533*================================================================*
006543 複合処理 SECTION.
006553*
006554* 初期化
006564     INITIALIZE 異常エリアＷ.
006569     MOVE ZERO TO 異常記号コード文字 異常番号コード文字.
006570*
006571*
006577* 患者番号セット
006578     MOVE ZERO                      TO 患者番号文字Ｗ.
006579     MOVE 連暗号複合−暗号患者番号  TO 患者番号文字Ｗ.
006581*
006584*-----------------------------------------------------------------------------------*
006586* 記号
006587     MOVE SPACE                   TO パス残りＷ.
006588     MOVE SPACE                   TO 異常記号フラグ.
006589     MOVE SPACE                   TO 変換データＷ.
006590*
006591     IF 連暗号複合−暗号判定記号 = "1"
006592*
006593        MOVE 連暗号複合−暗号記号 TO 変換データＷ
006594        CALL "strmojid"   WITH C LINKAGE  USING BY REFERENCE パスエリアＷ
006595                                                BY REFERENCE 変換データＷ
006596        IF PROGRAM-STATUS = ZERO
006597           MOVE 変換データＷ     TO 連暗号複合−複合した記号
006603        ELSE
006604           MOVE "YES"            TO 異常記号フラグ
006605           MOVE PROGRAM-STATUS   TO 異常記号コード
006606           MOVE 異常記号コード   TO 異常記号コード文字
006611        END-IF
006614     ELSE
006615        MOVE 連暗号複合−記号    TO 連暗号複合−複合した記号
006616     END-IF.
006617*
006618*-----------------------------------------------------------------------------------*
006619* 番号
006620     MOVE SPACE                   TO パス残りＷ.
006621     MOVE SPACE                   TO 異常番号フラグ.
006622     MOVE SPACE                   TO 変換データＷ.
006623*
006624     IF 連暗号複合−暗号判定番号 = "1"
006625*
006626        MOVE 連暗号複合−暗号番号 TO 変換データＷ
006627        CALL "strmojid"   WITH C LINKAGE  USING BY REFERENCE パスエリアＷ
006628                                                BY REFERENCE 変換データＷ
006629        IF PROGRAM-STATUS = ZERO
006630           MOVE 変換データＷ     TO 連暗号複合−複合した番号
006631        ELSE
006632           MOVE "YES"            TO 異常番号フラグ
006633           MOVE PROGRAM-STATUS   TO 異常番号コード
006634           MOVE 異常番号コード   TO 異常番号コード文字
006635        END-IF
006636     ELSE
006637        MOVE 連暗号複合−番号  TO 連暗号複合−複合した番号
006638     END-IF.
006639*
006640*-----------------------------------------------------------------------------------*
006641* 異常時エラーメッセージ
006642     IF ( 異常記号フラグ = "YES" ) OR ( 異常番号フラグ = "YES" ) 
006643*
006645         MOVE SPACE TO 連メ１０−キー
006646         MOVE "記号番号の複合化ができません。"  TO 連メ１０−メッセージ１
006647         MOVE "マキシーに連絡して下さい。"      TO 連メ１０−メッセージ２
006648         MOVE "(エラーコード)"                  TO 連メ１０−メッセージ４
006649*
006650         STRING "記号："             DELIMITED BY SIZE
006651                 異常記号コード文字  DELIMITED BY SIZE
006652                "・番号："           DELIMITED BY SIZE
006653                 異常番号コード文字  DELIMITED BY SIZE
006654                 INTO 連メ１０−メッセージ５
006655         END-STRING
006656*
006657         STRING "受診者番号："             DELIMITED BY SIZE
006658                 連暗号複合−暗号患者番号  DELIMITED BY SIZE
006659                 INTO 連メ１０−メッセージ６
006660         END-STRING
006661*
006663         CALL   "MSG010"
006664         CANCEL "MSG010"
006665*
006669     END-IF.
006670*
010001*================================================================*
010106*================================================================*
010200******************************************************************
010210 END PROGRAM MOJI2.
010220******************************************************************
