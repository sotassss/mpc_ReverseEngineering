000001 IDENTIFICATION  DIVISION.
000002* yawpas.
000003 PROGRAM-ID.     yawpas.
000004 ENVIRONMENT     DIVISION.
000005 CONFIGURATION   SECTION.
000006 POW-REPOSITORY.
000007     CLASS  AMethodSetyawpas AS "TLB=P:\yawara\yaw.option\Powercob\yawpas\Release\~build.tlb,{7454904A-14D5-4A49-917A-DE74E865A549},Fujitsu-PcobForm-4"
000008     CLASS  AMixed-DCfGWnd-Main-with-DCfGroupItem-Main AS "TLB=P:\yawara\yaw.option\Powercob\yawpas\Release\~build.tlb,{244D749E-22C0-11D2-91DD-00A0C9279679},Fujitsu-PcobFormWnd-4"
000009     CLASS  AMixed-DCmTextbox-Main-with-DCfGroupItem-Main AS "TLB=P:\yawara\yaw.option\Powercob\yawpas\Release\~build.tlb,{6F43162C-3EE8-11D5-A810-00A0C9C9AEA8},Fujitsu-PcobTextBox-4"
000010     CLASS  AMixed-DCmSText-Main-with-DCfGroupItem-Main AS "TLB=P:\yawara\yaw.option\Powercob\yawpas\Release\~build.tlb,{A24F5644-6FE5-11D5-A810-00A0C9C9AEA8},Fujitsu-PcobStaticText-4"
000011     CLASS  AMixed-DCmPush-Main-with-DCfGroupItem-Main AS "TLB=P:\yawara\yaw.option\Powercob\yawpas\Release\~build.tlb,{A4FDD400-705F-11D5-A810-00A0C9C9AEA8},Fujitsu-PcobCommandButton-4"
000012 .
000013 SPECIAL-NAMES.
000014 REPOSITORY.
000015 .
000016 INPUT-OUTPUT    SECTION.
000017 FILE-CONTROL.
000018 DATA            DIVISION.
000019 BASED-STORAGE   SECTION.
000020 FILE            SECTION.
000021 WORKING-STORAGE SECTION.
000022*<SCRIPT DIVISION="DATA", SECTION="WORKING-STORAGE">
000023 01  リターンカウント     PIC 9  VALUE ZERO  GLOBAL.
000024*
000025*
000026*
000027********************
000028* システム用ワーク *
000029********************
000030*
000031 01 改行                    PIC X(2)  VALUE X"0D0A" GLOBAL.
000032 01 エラーフラグ            PIC X(3) VALUE SPACE GLOBAL.
000033*
000034 01 起動プログラム名        PIC X(20) VALUE SPACE GLOBAL.
000035*
000036****************************
000037* 特殊環境ファイル用ワーク *
000038*****************************
000039*YENV.DATの1行目情報
000040* 種類(LAN、DIR、NORMAL),会コード,データフォルダ,パスワード画面ありか
000041 01 特殊環境ファイル内容Ｗ GLOBAL.
000042    03 特殊－種類Ｗ             PIC X(10) VALUE SPACE.
000043    03 特殊－協会コードＷ       PIC X(2)  VALUE SPACE.
000044    03 特殊－データフォルダＷ   PIC X(150) VALUE SPACE.
000045    03 特殊－パスワード有Ｗ     PIC X(10) VALUE SPACE.
000046*
000047*</SCRIPT>
000048 CONSTANT        SECTION.
000049 LINKAGE         SECTION.
000050 01  POW-FORM IS GLOBAL.
000051   02  POW-SELF OBJECT REFERENCE AMethodSetyawpas.
000052   02  POW-SUPER  PIC X(4).
000053   02  POW-THIS OBJECT REFERENCE AMethodSetyawpas.
000054   02  パスワード OBJECT REFERENCE AMixed-DCmTextbox-Main-with-DCfGroupItem-Main.
000055   02  CmStatic1 OBJECT REFERENCE AMixed-DCmSText-Main-with-DCfGroupItem-Main.
000056   02  実行ボタン OBJECT REFERENCE AMixed-DCmPush-Main-with-DCfGroupItem-Main.
000057   02  設定ボタン OBJECT REFERENCE AMixed-DCmPush-Main-with-DCfGroupItem-Main.
000058   02  起動中ラベル OBJECT REFERENCE AMixed-DCmSText-Main-with-DCfGroupItem-Main.
000059 01  yawpas REDEFINES POW-FORM GLOBAL OBJECT REFERENCE AMethodSetyawpas.
000060 01  POW-CONTROL-ID PIC S9(9) COMP-5.
000061 01  POW-EVENT-ID   PIC S9(9) COMP-5.
000062 01  POW-OLE-PARAM  PIC X(4).
000063 01  POW-OLE-RETURN PIC X(4).
000064 PROCEDURE       DIVISION USING POW-FORM POW-CONTROL-ID POW-EVENT-ID POW-OLE-PARAM POW-OLE-RETURN.
000065     EVALUATE POW-CONTROL-ID
000066     WHEN 117440513
000067     EVALUATE POW-EVENT-ID
000068     WHEN 117440768
000069       CALL "POW-SCRIPTLET1"
000070     END-EVALUATE
000071     WHEN 117440518
000072     EVALUATE POW-EVENT-ID
000073     WHEN 134221827
000074       CALL "POW-SCRIPTLET2"
000075     END-EVALUATE
000076     WHEN 117440520
000077     EVALUATE POW-EVENT-ID
000078     WHEN -600
000079       CALL "POW-SCRIPTLET3"
000080     END-EVALUATE
000081     WHEN 117440521
000082     EVALUATE POW-EVENT-ID
000083     WHEN -600
000084       CALL "POW-SCRIPTLET4"
000085     END-EVALUATE
000086     END-EVALUATE
000087     EXIT PROGRAM.
000088 IDENTIFICATION  DIVISION.
000089* サブ－ＣＢＲデータ－作成.
000090 PROGRAM-ID.     "サブ－ＣＢＲデータ－作成" IS COMMON.
000091*<SCRIPT DIVISION="PROCEDURE", NAME="サブ－ＣＢＲデータ－作成">
000092 ENVIRONMENT     DIVISION.
000093 INPUT-OUTPUT    SECTION.
000094 FILE-CONTROL.
000095     SELECT  旧ファイル      ASSIGN      TO         "C:\MAKISHISYS\YAWOBJ\W85.CBR"
000096                             ORGANIZATION             IS  LINE SEQUENTIAL
000097                             ACCESS MODE              IS  SEQUENTIAL
000098                             FILE        STATUS       IS  状態キー
000099                             LOCK        MODE         IS  AUTOMATIC.
000100*
000101     SELECT  新ファイル      ASSIGN      TO         "C:\MAKISHISYS\YAWOBJ\COBOL85.CBR"
000102                             ORGANIZATION             IS  LINE SEQUENTIAL
000103                             ACCESS MODE              IS  SEQUENTIAL
000104                             FILE        STATUS       IS  状態キー
000105                             LOCK        MODE         IS  AUTOMATIC.
000106*
000107*------------------------------------------------------------------------------------*
000108 DATA            DIVISION.
000109 FILE            SECTION.
000110**
000111* 可変長
000112 FD  旧ファイル
000113      RECORD IS VARYING IN SIZE FROM  1 TO 256 CHARACTERS DEPENDING ON 旧レコード長.
000114 01  旧－レコード.
000115    03 旧－文字列.
000116      05 旧－文字     PIC X(1) OCCURS 1 TO 256 TIMES DEPENDING ON 旧レコード長.
000117*
000118 FD  新ファイル
000119      RECORD IS VARYING IN SIZE FROM  1 TO 256 CHARACTERS DEPENDING ON 新レコード長.
000120 01  新－レコード.
000121    03 新－文字列.
000122      05 新－文字     PIC X(1) OCCURS 1 TO 256 TIMES DEPENDING ON 新レコード長.
000123**
000124* 
000125*================================================================*
000126 WORKING-STORAGE SECTION.
000127*================================================================*
000128 01 状態キー     PIC X(2) VALUE SPACE.
000129 01 終了フラグ   PIC X(3)  VALUE SPACE.
000130 01 終了フラグ１ PIC X(3)  VALUE SPACE.
000131*
000132 01 旧レコード長 PIC 9(3) BINARY VALUE 256.
000133 01 新レコード長 PIC 9(3) BINARY VALUE 256.
000134*
000135 01 カウンタ     PIC S9(3) VALUE ZERO.
000136 01 文字数Ｗ     PIC S9(3) VALUE ZERO.
000137*
000138 01 分院情報内容Ｗ.
000139    03 パス名Ｗ  PIC X(150).
000140*
000141 01 元内容Ｗ     PIC X(256).
000142 01 新内容Ｗ     PIC X(256).
000143*
000144 01 ＬＡＮ区分   PIC 9 VALUE ZERO.
000145*
000146*================================================================*
000147 PROCEDURE       DIVISION.
000148*================================================================*
000149***********
000150* 初期処理 *
000151***********
000152     PERFORM 初期化.
000153***********
000154* 主処理   *
000155***********
000156     PERFORM ファイル作成処理.
000157***********
000158* 終了処理 *
000159***********
000160     PERFORM 終了処理.
000161     EXIT PROGRAM.
000162*
000163*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
000164*================================================================*
000165 初期化 SECTION.
000166*================================================================*
000167     PERFORM ファイルオープン.
000168*
000169*================================================================*
000170 ファイルオープン SECTION.
000171*
000172     OPEN OUTPUT 新ファイル.
000173     IF ( 状態キー  NOT =  "00" )
000174        INVOKE POW-SELF "DisplayMessage" USING "新ファイルが異常です（ＯＰ）" "メッセージ"
000175        PERFORM ファイル閉鎖
000176        MOVE "YES" TO エラーフラグ
000177        EXIT PROGRAM
000178     END-IF.
000179     OPEN INPUT  旧ファイル.
000180     IF ( 状態キー  NOT =  "00" )
000181        INVOKE POW-SELF "DisplayMessage" USING "旧ファイルが異常です（ＯＰ）" "メッセージ"
000182        PERFORM ファイル閉鎖
000183        MOVE "YES" TO エラーフラグ
000184        EXIT PROGRAM
000185     END-IF.
000186*
000187*================================================================*
000188 ファイル作成処理 SECTION.
000189*================================================================*
000190*
000191     IF 特殊－種類Ｗ = "LAN"
000192        MOVE 1    TO ＬＡＮ区分
000193     ELSE
000194        MOVE ZERO TO ＬＡＮ区分
000195     END-IF.
000196*   
000197     INITIALIZE 分院情報内容Ｗ.
000198*
000199     MOVE 特殊－データフォルダＷ TO パス名Ｗ.
000200*
000201     IF ( パス名Ｗ = SPACE )
000202           INVOKE POW-SELF "DisplayMessage" USING "データフォルダが指定されていません。" "メッセージ"
000203           MOVE "YES" TO エラーフラグ
000204     ELSE
000205           PERFORM VARYING 文字数Ｗ FROM 150 BY -1 
000206                   UNTIL ( 文字数Ｗ <= ZERO ) OR ( パス名Ｗ(文字数Ｗ:1) NOT = SPACE )
000207              CONTINUE
000208           END-PERFORM         
000209           PERFORM ファイル更新処理
000210     END-IF.
000211*
000212*================================================================*
000213 ファイル更新処理 SECTION.
000214*================================================================*
000215** 旧ファイルの内容を転記
000216     MOVE SPACE TO 終了フラグ１.
000217     PERFORM  旧ファイル読込.
000218*
000219     PERFORM UNTIL ( 終了フラグ１ = "YES" )
000220         MOVE SPACE        TO 元内容Ｗ
000221         MOVE 旧－レコード TO 元内容Ｗ
000222         MOVE SPACE        TO 新内容Ｗ
000223         PERFORM パス変更処理
000224         MOVE 新内容Ｗ     TO 新－レコード
000225         PERFORM 新データ書込
000226*
000227         PERFORM  旧ファイル読込
000228     END-PERFORM.
000229*
000230*================================================================*
000231 旧ファイル読込 SECTION.
000232*================================================================*
000233     READ 旧ファイル NEXT
000234     AT END
000235         MOVE "YES" TO 終了フラグ１
000236     END-READ.
000237*
000238*================================================================*
000239 パス変更処理 SECTION.
000240*================================================================*
000241     EVALUATE 元内容Ｗ(1:6)
000242     WHEN "JUSINJ"
000243         IF 元内容Ｗ(1:7) = "JUSINJ2"
000244            STRING "JUSINJ2L="            DELIMITED BY SIZE
000245                   パス名Ｗ(1:文字数Ｗ)   DELIMITED BY SIZE
000246                   "DATA\JUSINJ2.DAT"   DELIMITED BY SIZE
000247                INTO 新内容Ｗ
000248            END-STRING
000249         ELSE
000250            STRING "JUSINJL="             DELIMITED BY SIZE
000251                   パス名Ｗ(1:文字数Ｗ)   DELIMITED BY SIZE
000252                   "DATA\JUSINJ.DAT"   DELIMITED BY SIZE
000253                INTO 新内容Ｗ
000254            END-STRING
000255         END-IF
000256     WHEN "UKETUK"
000257         STRING "UKETUKEL="            DELIMITED BY SIZE
000258                パス名Ｗ(1:文字数Ｗ)   DELIMITED BY SIZE
000259                "DATA\UKETUKE.DAT"  DELIMITED BY SIZE
000260             INTO 新内容Ｗ
000261         END-STRING
000262     WHEN "SEKIRO"
000263         STRING "SEKIROKL="            DELIMITED BY SIZE
000264                パス名Ｗ(1:文字数Ｗ)   DELIMITED BY SIZE
000265                "DATA\SEKIROK.DAT"  DELIMITED BY SIZE
000266             INTO 新内容Ｗ
000267         END-STRING
000268     WHEN "HUSYOU"
000269         STRING "HUSYOUL="             DELIMITED BY SIZE
000270                パス名Ｗ(1:文字数Ｗ)   DELIMITED BY SIZE
000271                "DATA\HUSYOU.DAT"   DELIMITED BY SIZE
000272             INTO 新内容Ｗ
000273         END-STRING
000274     WHEN "HUGEIN"
000275         STRING "HUGEINL="             DELIMITED BY SIZE
000276                パス名Ｗ(1:文字数Ｗ)   DELIMITED BY SIZE
000277                "DATA\HUGEIN.DAT"   DELIMITED BY SIZE
000278             INTO 新内容Ｗ
000279         END-STRING
000280     WHEN "CHOKEI"
000281         STRING "CHOKEIL="             DELIMITED BY SIZE
000282                パス名Ｗ(1:文字数Ｗ)   DELIMITED BY SIZE
000283                "DATA\CHOKEI.DAT"   DELIMITED BY SIZE
000284             INTO 新内容Ｗ
000285         END-STRING
000286     WHEN "KAIKEI"
000287         STRING "KAIKEIL="             DELIMITED BY SIZE
000288                パス名Ｗ(1:文字数Ｗ)   DELIMITED BY SIZE
000289                "DATA\KAIKEI.DAT"   DELIMITED BY SIZE
000290             INTO 新内容Ｗ
000291         END-STRING
000292     WHEN "MEMOL="
000293         STRING "MEMOL="               DELIMITED BY SIZE
000294                パス名Ｗ(1:文字数Ｗ)   DELIMITED BY SIZE
000295                "DATA\MEMO.DAT"     DELIMITED BY SIZE
000296             INTO 新内容Ｗ
000297         END-STRING
000298     WHEN "RECEPT"
000299         STRING "RECEPTL="             DELIMITED BY SIZE
000300                パス名Ｗ(1:文字数Ｗ)   DELIMITED BY SIZE
000301                "DATA\RECEPT.DAT"   DELIMITED BY SIZE
000302             INTO 新内容Ｗ
000303         END-STRING
000304     WHEN "NYUKIN"
000305         STRING "NYUKINL="             DELIMITED BY SIZE
000306                パス名Ｗ(1:文字数Ｗ)   DELIMITED BY SIZE
000307                "DATA\NYUKIN.DAT"   DELIMITED BY SIZE
000308             INTO 新内容Ｗ
000309         END-STRING
000310     WHEN "KARUTE"
000311         STRING "KARUTEL="             DELIMITED BY SIZE
000312                パス名Ｗ(1:文字数Ｗ)   DELIMITED BY SIZE
000313                "DATA\KARUTE.DAT"   DELIMITED BY SIZE
000314             INTO 新内容Ｗ
000315         END-STRING
000316     WHEN "SYOMEI"
000317         STRING "SYOMEIRL="            DELIMITED BY SIZE
000318                パス名Ｗ(1:文字数Ｗ)   DELIMITED BY SIZE
000319                "DATA\SYOMEIRR.DAT"    DELIMITED BY SIZE
000320             INTO 新内容Ｗ
000321         END-STRING
000322     WHEN "JIGYOS"
000323         STRING "JIGYOSL="                    DELIMITED BY SIZE
000324                パス名Ｗ(1:文字数Ｗ)          DELIMITED BY SIZE
000325                "DATA\MASTER\JIGYOS.DAT"   DELIMITED BY SIZE
000326             INTO 新内容Ｗ
000327         END-STRING
000328     WHEN "SEIGYO"
000329         IF ＬＡＮ区分 = 1
000330            MOVE "SEIGYOL=C:\MAKISHISYS\DATA\MASTER\SEIGYO.DAT" TO 新内容Ｗ      
000331         ELSE
000332           STRING "SEIGYOL="                    DELIMITED BY SIZE
000333                  パス名Ｗ(1:文字数Ｗ)          DELIMITED BY SIZE
000334                  "DATA\MASTER\SEIGYO.DAT"   DELIMITED BY SIZE
000335               INTO 新内容Ｗ
000336           END-STRING
000337         END-IF
000338     WHEN "SEJOHO"
000339         STRING "SEJOHOL="                    DELIMITED BY SIZE
000340                パス名Ｗ(1:文字数Ｗ)          DELIMITED BY SIZE
000341                "DATA\MASTER\SEJOHO.DAT"   DELIMITED BY SIZE
000342             INTO 新内容Ｗ
000343         END-STRING
000344     WHEN "IDKANR"
000345         STRING "IDKANRL="                    DELIMITED BY SIZE
000346                パス名Ｗ(1:文字数Ｗ)          DELIMITED BY SIZE
000347               "DATA\MASTER\IDKANR.DAT"   DELIMITED BY SIZE
000348            INTO 新内容Ｗ
000349         END-STRING
000350     WHEN "CHORIY"
000351         STRING "CHORIYUL="                   DELIMITED BY SIZE
000352                パス名Ｗ(1:文字数Ｗ)          DELIMITED BY SIZE
000353                "DATA\MASTER\CHORIYU.DAT"   DELIMITED BY SIZE
000354             INTO 新内容Ｗ
000355         END-STRING
000356     WHEN "REIGAI"
000357         STRING "REIGAIL="                   DELIMITED BY SIZE
000358                パス名Ｗ(1:文字数Ｗ)          DELIMITED BY SIZE
000359                "DATA\MASTER\REIGAI.DAT"   DELIMITED BY SIZE
000360             INTO 新内容Ｗ
000361         END-STRING
000362     WHEN "HOKENE"
000363         STRING "HOKENEXL="                   DELIMITED BY SIZE
000364                パス名Ｗ(1:文字数Ｗ)          DELIMITED BY SIZE
000365                "DATA\MASTER\HOKENEX.DAT"   DELIMITED BY SIZE
000366             INTO 新内容Ｗ
000367         END-STRING
000368     WHEN "BARKAN"
000369         STRING "BARKANRL="                   DELIMITED BY SIZE
000370                パス名Ｗ(1:文字数Ｗ)          DELIMITED BY SIZE
000371                "DATA\MASTER\BARKANR.DAT"  DELIMITED BY SIZE
000372             INTO 新内容Ｗ
000373         END-STRING
000374     WHEN "BANGOK"
000375         STRING "BANGOKL="                    DELIMITED BY SIZE
000376                パス名Ｗ(1:文字数Ｗ)          DELIMITED BY SIZE
000377                "DATA\MASTER\BANGOK.DAT"   DELIMITED BY SIZE
000378             INTO 新内容Ｗ
000379         END-STRING
000380*
000381     WHEN "HHUSYO"
000382         STRING "HHUSYOUL="             DELIMITED BY SIZE
000383                パス名Ｗ(1:文字数Ｗ)    DELIMITED BY SIZE
000384                "DATA\H_HUSYOU.DAT"  DELIMITED BY SIZE
000385             INTO 新内容Ｗ
000386         END-STRING
000387     WHEN "HRECEL"
000388         STRING "HRECEL="               DELIMITED BY SIZE
000389                パス名Ｗ(1:文字数Ｗ)    DELIMITED BY SIZE
000390                "DATA\H_RECE.DAT"    DELIMITED BY SIZE
000391             INTO 新内容Ｗ
000392         END-STRING
000393     WHEN "HNIKEI"
000394         STRING "HNIKEIL="              DELIMITED BY SIZE
000395                パス名Ｗ(1:文字数Ｗ)    DELIMITED BY SIZE
000396                "DATA\H_NIKEI.DAT"   DELIMITED BY SIZE
000397             INTO 新内容Ｗ
000398         END-STRING
000399     WHEN "HSEJOH"
000400         STRING "HSEJOHOL="                     DELIMITED BY SIZE
000401                パス名Ｗ(1:文字数Ｗ)            DELIMITED BY SIZE
000402                "DATA\MASTER\H_SEJOHO.DAT"   DELIMITED BY SIZE
000403             INTO 新内容Ｗ
000404         END-STRING
000405     WHEN "HSETAN"
000406         STRING "HSETANTL="                     DELIMITED BY SIZE
000407                パス名Ｗ(1:文字数Ｗ)            DELIMITED BY SIZE
000408                "DATA\MASTER\H_SETANT.DAT"   DELIMITED BY SIZE
000409             INTO 新内容Ｗ
000410         END-STRING
000411*
000412     WHEN "RYOSYU"
000413         STRING "RYOSYUL="              DELIMITED BY SIZE
000414                パス名Ｗ(1:文字数Ｗ)    DELIMITED BY SIZE
000415                "DATA\RYOSYU.DAT"   DELIMITED BY SIZE
000416             INTO 新内容Ｗ
000417         END-STRING
000418     WHEN "ROUSAI"
000419         STRING "ROUSAIJL="              DELIMITED BY SIZE
000420                パス名Ｗ(1:文字数Ｗ)    DELIMITED BY SIZE
000421                "DATA\ROUSAIJ.DAT"   DELIMITED BY SIZE
000422             INTO 新内容Ｗ
000423         END-STRING
000424     WHEN "JIBAIJ"
000425         STRING "JIBAIJL="              DELIMITED BY SIZE
000426                パス名Ｗ(1:文字数Ｗ)    DELIMITED BY SIZE
000427                "DATA\JIBAIJ.DAT"   DELIMITED BY SIZE
000428             INTO 新内容Ｗ
000429         END-STRING
000430     WHEN "SEIHOJ"
000431         STRING "SEIHOJL="              DELIMITED BY SIZE
000432                パス名Ｗ(1:文字数Ｗ)    DELIMITED BY SIZE
000433                "DATA\SEIHOJ.DAT"   DELIMITED BY SIZE
000434             INTO 新内容Ｗ
000435         END-STRING
000436     WHEN "DMKIRO"
000437         STRING "DMKIROKL="              DELIMITED BY SIZE
000438                パス名Ｗ(1:文字数Ｗ)     DELIMITED BY SIZE
000439                "DATA\DMKIROK.DAT"   DELIMITED BY SIZE
000440             INTO 新内容Ｗ
000441         END-STRING
000442     WHEN "HNOURY"
000443         STRING "HNOURYOL="             DELIMITED BY SIZE
000444                パス名Ｗ(1:文字数Ｗ)    DELIMITED BY SIZE
000445                "DATA\H_NOURYO.DAT"   DELIMITED BY SIZE
000446             INTO 新内容Ｗ
000447         END-STRING
000448     WHEN "TEKIYO"
000449         STRING "TEKIYOL="             DELIMITED BY SIZE
000450                パス名Ｗ(1:文字数Ｗ)   DELIMITED BY SIZE
000451                "DATA\TEKIYO.DAT"   DELIMITED BY SIZE
000452             INTO 新内容Ｗ
000453         END-STRING
000454     WHEN "HNOYOT"
000455         STRING "HNOYOTEL="             DELIMITED BY SIZE
000456                パス名Ｗ(1:文字数Ｗ)    DELIMITED BY SIZE
000457                "DATA\H_NOYOTE.DAT"   DELIMITED BY SIZE
000458             INTO 新内容Ｗ
000459         END-STRING
000460     WHEN "RECERJ"
000461         STRING "RECERJL="             DELIMITED BY SIZE
000462                パス名Ｗ(1:文字数Ｗ)   DELIMITED BY SIZE
000463                "DATA\RECERJ.DAT"   DELIMITED BY SIZE
000464             INTO 新内容Ｗ
000465         END-STRING
000466**
000467     WHEN "KINRIN"
000468         STRING "KINRINL="                     DELIMITED BY SIZE
000469                パス名Ｗ(1:文字数Ｗ)            DELIMITED BY SIZE
000470                "DATA\MASTER\KINRIN.DAT"   DELIMITED BY SIZE
000471             INTO 新内容Ｗ
000472         END-STRING
000473     WHEN "KENSAK"
000474         IF ＬＡＮ区分 = 1
000475            MOVE "KENSAKUL=C:\MAKISHISYS\DATA\MASTER\KENSAKU.DAT" TO 新内容Ｗ      
000476         ELSE
000477            STRING "KENSAKUL="                     DELIMITED BY SIZE
000478                   パス名Ｗ(1:文字数Ｗ)            DELIMITED BY SIZE
000479                   "DATA\MASTER\KENSAKU.DAT"   DELIMITED BY SIZE
000480                INTO 新内容Ｗ
000481            END-STRING
000482         END-IF
000483     WHEN "HISHIL"
000484         STRING "HISHIL="                       DELIMITED BY SIZE
000485                パス名Ｗ(1:文字数Ｗ)            DELIMITED BY SIZE
000486                "DATA\MASTER\H_ISHI.DAT"     DELIMITED BY SIZE
000487             INTO 新内容Ｗ
000488         END-STRING
000489     WHEN "UMEISY"
000490         STRING "UMEISYOL="                     DELIMITED BY SIZE
000491                パス名Ｗ(1:文字数Ｗ)            DELIMITED BY SIZE
000492                "DATA\MASTER\UMEISYO.DAT"   DELIMITED BY SIZE
000493             INTO 新内容Ｗ
000494         END-STRING
000495     WHEN "HSEIGY"
000496         IF ＬＡＮ区分 = 1
000497            MOVE "HSEIGYOL=C:\MAKISHISYS\DATA\MASTER\H_SEIGYO.DAT" TO 新内容Ｗ      
000498         ELSE
000499             STRING "HSEIGYOL="                     DELIMITED BY SIZE
000500                    パス名Ｗ(1:文字数Ｗ)            DELIMITED BY SIZE
000501                    "DATA\MASTER\H_SEIGYO.DAT"   DELIMITED BY SIZE
000502                 INTO 新内容Ｗ
000503             END-STRING
000504         END-IF        
000505     WHEN "SISETJ"
000506         STRING "SISETJUL="                     DELIMITED BY SIZE
000507                パス名Ｗ(1:文字数Ｗ)            DELIMITED BY SIZE
000508                "DATA\MASTER\SISETJU.DAT"   DELIMITED BY SIZE
000509             INTO 新内容Ｗ
000510         END-STRING
000511     WHEN "SISETU"
000512         STRING "SISETUL="                     DELIMITED BY SIZE
000513                パス名Ｗ(1:文字数Ｗ)           DELIMITED BY SIZE
000514                "DATA\MASTER\SISETU.DAT"   DELIMITED BY SIZE
000515             INTO 新内容Ｗ
000516         END-STRING
000517**
000518     WHEN OTHER
000519         MOVE 元内容Ｗ TO 新内容Ｗ
000520     END-EVALUATE.
000521*
000522     PERFORM VARYING カウンタ FROM 256 BY -1 
000523             UNTIL ( カウンタ <= ZERO ) OR ( 新内容Ｗ(カウンタ:1) NOT = SPACE )
000524        CONTINUE
000525     END-PERFORM.
000526     MOVE カウンタ TO 新レコード長.
000527*
000528*================================================================*
000529 新データ書込 SECTION.
000530*================================================================*
000531     WRITE 新－レコード.
000532     IF ( 状態キー NOT = "00" )
000533        INVOKE POW-SELF "DisplayMessage" USING "新ファイルが異常です（ＷＲ）" "メッセージ"
000534        MOVE "YES" TO エラーフラグ
000535     END-IF.
000536*
000537*================================================================*
000538 終了処理 SECTION.
000539*================================================================*
000540     PERFORM ファイル閉鎖.
000541*
000542*================================================================*
000543 ファイル閉鎖 SECTION.
000544*
000545     CLOSE 旧ファイル 新ファイル.
000546*
000547*</SCRIPT>
000548 END PROGRAM     "サブ－ＣＢＲデータ－作成".
000549 IDENTIFICATION  DIVISION.
000550* yawpas-Opened.
000551 PROGRAM-ID.     POW-SCRIPTLET1.
000552*<SCRIPT DIVISION="PROCEDURE", CONTROL="yawpas", EVENT="Opened", POW-NAME="SCRIPTLET1", TYPE="FORM">
000553 ENVIRONMENT             DIVISION.
000554 INPUT-OUTPUT            SECTION.
000555 FILE-CONTROL.
000556*
000557     SELECT  特殊環境ファイル    ASSIGN      TO       "C:\MAKISHISYS\DATA\COMMON\YENV.DAT"
000558                             ORGANIZATION             IS  LINE SEQUENTIAL
000559                             ACCESS MODE              IS  SEQUENTIAL
000560                             FILE        STATUS       IS  状態キー
000561                             LOCK        MODE         IS  AUTOMATIC.
000562*
000563*****************************************************************
000564*                      DATA DIVISION                             *
000565******************************************************************
000566 DATA                    DIVISION.
000567 FILE                    SECTION.
000568* 可変長
000569 FD 特殊環境ファイル
000570      RECORD IS VARYING IN SIZE FROM  1 TO 500 CHARACTERS DEPENDING ON レコード長.
000571 01 入－レコード.
000572    03 文字列.
000573       05 文字 PIC X OCCURS 1 TO 500 TIMES DEPENDING ON レコード長.
000574*
000575******************************************************************
000576*                WORKING-STORAGE SECTION                         *
000577******************************************************************
000578 WORKING-STORAGE         SECTION.
000579 01 状態キー                      PIC X(2) VALUE SPACE.
000580 01 エラー内容                    PIC X(80) VALUE SPACE.
000581 01 レコード長                    PIC 9(4) BINARY VALUE 500.
000582 01 終了フラグ                    PIC X(3) VALUE SPACE.
000583*
000584 01 内容Ｗ            PIC X(500) VALUE SPACE.
000585*
000586*--- Msgbox用 ---*
000587 01  STYL           PIC S9(9) COMP-5.
000588 01  RET            PIC S9(9) COMP-5.
000589 01  メッセージＷ   PIC X(128) VALUE SPACE.
000590*
000591*--- コピーC 連携用 ---*
000592 01  元ファイル名Ｗ PIC X(80) VALUE "C:\MAKISHISYS\YAWOBJ\COBOL85.CBR".
000593 01  先ファイル名Ｗ PIC X(80) VALUE "C:\MAKISHISYS\YAWOBJ\W85.CBR".
000594 01  プログラム名Ｗ PIC X(8)  VALUE "filsave".
000595*
000596******************************************************************
000597*                      PROCEDURE  DIVISION                       *
000598******************************************************************
000599 PROCEDURE               DIVISION.
000600*------------------------------------------------------------------------------*
000601* 特殊環境ファイル YENV.DAT
000602*
000603* 種類(LAN、DIR、NORMAL),会コード,データフォルダ,パスワード画面ありか
000604*
000605*※会コードは意味なし
000606*
000607*
000608*LAN
000609* LAN,01,\\HOST\,
000610
000611*LANでパスワードあり
000612* LAN,01,\\HOST\,PASS
000613
000614*別フォルダ
000615* DIR,01,c:\makishisys\柔個別\,
000616
000617*別フォルダでパスワードあり
000618* DIR,01,c:\makishisys\柔個別\,PASS
000619
000620*ノーマルでパスワードあり
000621* NORMAL,01,,PASS
000622*
000623*------------------------------------------------------------------------------*
000624**
000625** 起動中ラベルの位置を中心にする。
000626     MOVE 210 TO "Left" OF 起動中ラベル.
000627     MOVE 255 TO "Top" OF 起動中ラベル.
000628**
000629**
000630     OPEN INPUT 特殊環境ファイル.
000631     IF 状態キー  =  "35"
000632        INVOKE POW-SELF "DisplayMessage" USING "特殊環境ファイルが無いので起動できません。" "メッセージ"
000633        PERFORM 異常終了
000634     ELSE
000635        IF 状態キー  NOT =  "00"
000636           INVOKE POW-SELF "DisplayMessage" USING "特殊環境ファイルOPENエラー。起動できません。" "メッセージ"
000637           PERFORM 異常終了
000638        END-IF   
000639     END-IF.
000640**
000641*     
000642** // 特殊環境ファイル１行目読込み //
000643     MOVE SPACE  TO 内容Ｗ 特殊環境ファイル内容Ｗ.
000644     MOVE SPACE  TO 終了フラグ.
000645     PERFORM 特殊環境ファイル読込.
000646*
000647     IF 終了フラグ NOT = "YES"
000648        MOVE 入－レコード TO 内容Ｗ
000649        IF 内容Ｗ NOT = SPACE
000650*         GLOBALへセット
000651            UNSTRING  内容Ｗ  DELIMITED BY "," 
000652                      INTO  特殊－種類Ｗ
000653                            特殊－協会コードＷ
000654                            特殊－データフォルダＷ
000655                            特殊－パスワード有Ｗ
000656            END-UNSTRING
000657        END-IF
000658     END-IF.   
000659*
000660     CLOSE 特殊環境ファイル.
000661*
000662*     IF 特殊－協会コードＷ = SPACE
000663*        INVOKE POW-SELF "DisplayMessage" USING "特殊環境ファイルの内容が異常です。起動できません。" "メッセージ"
000664*        PERFORM 異常終了
000665*     END-IF.   
000666*
000667*     STRING "YAWARA"             DELIMITED BY SIZE
000668*            特殊－協会コードＷ  DELIMITED BY SIZE
000669*            ".EXE"              DELIMITED BY SIZE
000670*     INTO 起動プログラム名.
000671*
000672      MOVE "YAWARA01" TO 起動プログラム名.
000673*
000674*------------------------------------------------------*
000675*
000676     IF 特殊－種類Ｗ  = "LAN" OR "DIR"
000677*
000678*---    ＣＢＲ退避 (元ファイル → 先ファイル) ---*
000679         CALL プログラム名Ｗ WITH C LINKAGE
000680               USING BY REFERENCE 元ファイル名Ｗ
000681                     BY REFERENCE 先ファイル名Ｗ
000682*
000683         IF ( PROGRAM-STATUS = ZERO )
000684*---        ＣＢＲ作成 ---*
000685            MOVE SPACE TO エラーフラグ
000686            CALL "サブ－ＣＢＲデータ－作成"
000687*
000688            IF ( エラーフラグ NOT = SPACE )
000689*---           ＣＢＲ復元 (先ファイル → 元ファイル) ---*
000690               CALL プログラム名Ｗ WITH C LINKAGE
000691                         USING BY REFERENCE 先ファイル名Ｗ
000692                               BY REFERENCE 元ファイル名Ｗ
000693               INVOKE POW-SELF "DisplayMessage" USING "ＣＢＲ作成に失敗しました。起動できません。" "メッセージ"
000694               PERFORM 異常終了
000695            END-IF
000696*
000697         ELSE
000698            PERFORM データ退避エラー
000699         END-IF
000700     END-IF.
000701*
000702*------------------------------------------------------*
000703*
000704*    パスワード画面ありか
000705     IF 特殊－パスワード有Ｗ  = "PASS"
000706        MOVE POW-FALSE TO "Visible" OF 起動中ラベル
000707     ELSE
000708        MOVE "起動中" TO "Caption" OF POW-SELF
000709*
000710*       / YAWARAを呼出して終了 /
000711        INVOKE POW-SELF "Execute" USING 起動プログラム名 POW-SWSHOWNORMAL
000712        INVOKE POW-SELF "CloseForm"
000713        EXIT PROGRAM
000714*
000715     END-IF.
000716*
000717     EXIT PROGRAM.     
000718*      
000719*================================================================*
000720 特殊環境ファイル読込 SECTION.
000721*
000722     READ 特殊環境ファイル NEXT
000723     AT END
000724         MOVE "YES" TO 終了フラグ
000725     END-READ.
000726*
000727*================================================================*
000728 データ退避エラー SECTION.
000729*
000730     MOVE SPACE TO メッセージＷ.
000731     STRING "退避失敗："    DELIMITED BY SIZE
000732            元ファイル名Ｗ  DELIMITED BY SIZE
000733            INTO メッセージＷ
000734     END-STRING.
000735*/ 警告アイコン /*
000736     COMPUTE STYL = POW-DMOK + POW-DMICONWARNING + POW-DMDEFBUTTON1.
000737     INVOKE POW-SELF "DisplayMessage" USING メッセージＷ "警告" STYL.
000738     PERFORM 異常終了.
000739*
000740*================================================================*
000741 異常終了 SECTION.
000742*
000743     INVOKE POW-SELF "CloseForm".
000744     EXIT PROGRAM.
000745*
000746*================================================================*
000747     
000748*</SCRIPT>
000749 END PROGRAM     POW-SCRIPTLET1.
000750 IDENTIFICATION  DIVISION.
000751* パスワード-Return.
000752 PROGRAM-ID.     POW-SCRIPTLET2.
000753*<SCRIPT DIVISION="PROCEDURE", CONTROL="パスワード", EVENT="Return", POW-NAME="SCRIPTLET2", TYPE="ETC">
000754 ENVIRONMENT     DIVISION.
000755 DATA            DIVISION.
000756 WORKING-STORAGE SECTION.
000757 PROCEDURE       DIVISION.
000758*
000759     IF リターンカウント = ZERO
000760        MOVE 1   TO リターンカウント 
000761        MOVE POW-FALSE  TO "Enabled" OF 実行ボタン
000762        CALL "チェックサブ"
000763        MOVE POW-TRUE   TO "Enabled" OF 実行ボタン
000764        MOVE ZERO TO リターンカウント 
000765     END-IF.   
000766*
000767     EXIT PROGRAM.
000768*</SCRIPT>
000769 END PROGRAM     POW-SCRIPTLET2.
000770 IDENTIFICATION  DIVISION.
000771* 実行ボタン-Click.
000772 PROGRAM-ID.     POW-SCRIPTLET3.
000773*<SCRIPT DIVISION="PROCEDURE", CONTROL="実行ボタン", EVENT="Click", POW-NAME="SCRIPTLET3", TYPE="ETC">
000774 ENVIRONMENT     DIVISION.
000775 DATA            DIVISION.
000776 WORKING-STORAGE SECTION.
000777 PROCEDURE       DIVISION.
000778*
000779     MOVE POW-FALSE  TO "Enabled" OF 実行ボタン.
000780     CALL "チェックサブ".
000781     MOVE POW-TRUE   TO "Enabled" OF 実行ボタン.
000782*
000783     EXIT PROGRAM.
000784*</SCRIPT>
000785 END PROGRAM     POW-SCRIPTLET3.
000786 IDENTIFICATION  DIVISION.
000787* チェックサブ.
000788 PROGRAM-ID.     "チェックサブ" IS COMMON.
000789*<SCRIPT DIVISION="PROCEDURE", NAME="チェックサブ">
000790 ENVIRONMENT             DIVISION.
000791 INPUT-OUTPUT            SECTION.
000792 FILE-CONTROL.
000793     SELECT  例外マスタ      ASSIGN      TO        REIGAIL
000794                             ORGANIZATION             IS  INDEXED
000795                             ACCESS MODE              IS  DYNAMIC
000796                             RECORD KEY               IS  例０５－区分コード
000797                                                          例０５－番号コード
000798                             FILE STATUS              IS  状態キー
000799                             LOCK        MODE         IS  AUTOMATIC.
000800*
000801     SELECT  連携ファイル    ASSIGN      TO        W001L
000802                             ORGANIZATION             IS  SEQUENTIAL
000803                             ACCESS MODE              IS  SEQUENTIAL
000804                             FILE        STATUS       IS  状態キー
000805                             LOCK        MODE         IS  AUTOMATIC.
000806*
000807*****************************************************************
000808*                      DATA DIVISION                             *
000809******************************************************************
000810 DATA                    DIVISION.
000811 FILE                    SECTION.
000812*
000813** 例外05(パスワード用)
000814 FD  例外マスタ          BLOCK   CONTAINS   1   RECORDS.
000815     COPY REIGAI05       OF  XFDLIB  JOINING   例０５   AS  PREFIX.
000816*
000817 FD  連携ファイル  RECORD  CONTAINS 16 CHARACTERS.
000818 01  連携－レコード.
000819     03  連携－パスワード       PIC X(6).
000820     03  連携－レベル           PIC X.
000821     03  FILLER                 PIC X(9).
000822*
000823*
000824******************************************************************
000825*                WORKING-STORAGE SECTION                         *
000826******************************************************************
000827 WORKING-STORAGE         SECTION.
000828 01 状態キー                      PIC X(2) VALUE SPACE.
000829 01 エラー内容                    PIC X(80) VALUE SPACE.
000830 01 終了フラグ                    PIC X(3) VALUE SPACE.
000831 01 存在フラグ                    PIC X(3) VALUE SPACE.
000832 01 パスワードＷ                  PIC X(6) VALUE SPACE.
000833 01 退避パスワードＷ              PIC X(6) VALUE SPACE.
000834 01 退避レベルＷ                  PIC X    VALUE SPACE.
000835*
000836 01 カウンタ                      PIC 9(3) VALUE ZERO.
000837*
000838*
000839******************************************************************
000840*                      PROCEDURE  DIVISION                       *
000841******************************************************************
000842 PROCEDURE               DIVISION.
000843*
000844     PERFORM 入力チェック.
000845     PERFORM ファイルオープン.
000846     PERFORM パスワードチェック. 
000847*
000848*================================================================*
000849*================================================================*
000850 入力チェック SECTION.
000851*
000852     IF "Text" OF パスワード  = SPACE
000853         INVOKE POW-SELF "DisplayMessage" USING "パスワードを入力してください。" "メッセージ"
000854         INVOKE パスワード "SetFocus"
000855         EXIT PROGRAM
000856     END-IF.
000857*
000858*================================================================*
000859 ファイルオープン SECTION.
000860*
000861     OPEN INPUT 例外マスタ.
000862     IF 状態キー  NOT =  "00"
000863        MOVE SPACE TO エラー内容
000864        STRING "例外マスタオープンエラー:"  DELIMITED BY SIZE
000865                状態キー                    DELIMITED BY SIZE
000866           INTO エラー内容
000867        END-STRING
000868        INVOKE POW-SELF "DisplayMessage" USING エラー内容 "メッセージ"
000869        EXIT PROGRAM
000870     END-IF.
000871*
000872*================================================================*
000873 パスワードチェック SECTION.
000874*
000875     MOVE "Text" OF パスワード TO パスワードＷ.
000876     PERFORM 存在チェック.
000877     IF 存在フラグ = SPACE
000878        INVOKE POW-SELF "DisplayMessage" USING "パスワードが登録されていません。" "メッセージ"
000879        INVOKE パスワード "SetFocus"
000880        PERFORM ファイル閉鎖
000881        EXIT PROGRAM
000882     ELSE
000883        PERFORM 連携処理
000884     END-IF.
000885*
000886*================================================================*
000887 ファイル閉鎖 SECTION.
000888*
000889     CLOSE 例外マスタ.
000890*
000891*================================================================*
000892 存在チェック SECTION.
000893*
000894     MOVE SPACE      TO 存在フラグ.
000895     MOVE 1          TO カウンタ.
000896     MOVE 05         TO 例０５－区分コード.
000897     MOVE ZERO       TO 例０５－番号コード.
000898     READ 例外マスタ
000899     NOT INVALID KEY
000900        PERFORM UNTIL ( 存在フラグ                   = "YES" ) OR
000901                      ( 例０５－パスワード(カウンタ) = SPACE ) OR
000902                      ( カウンタ > 90 )
000903            IF 例０５－パスワード(カウンタ) = パスワードＷ
000904                MOVE "YES"                    TO 存在フラグ
000905                MOVE パスワードＷ             TO 退避パスワードＷ
000906                MOVE 例０５－レベル(カウンタ) TO 退避レベルＷ
000907            END-IF
000908            COMPUTE カウンタ = カウンタ + 1
000909        END-PERFORM
000910     END-READ.
000911*
000912*================================================================*
000913 連携処理 SECTION.
000914*
000915*--- ワークＦにパスワード・レベルを書き込む -----*
000916     OPEN OUTPUT 連携ファイル.
000917     IF 状態キー  NOT =  "00"
000918        MOVE SPACE TO エラー内容
000919        STRING "連携ファイルオープンエラー:"  DELIMITED BY SIZE
000920                状態キー                      DELIMITED BY SIZE
000921           INTO エラー内容
000922        END-STRING
000923        INVOKE POW-SELF "DisplayMessage" USING エラー内容 "メッセージ"
000924        PERFORM ファイル閉鎖
000925        EXIT PROGRAM
000926     END-IF.
000927*
000928     MOVE SPACE             TO 連携－レコード.
000929     MOVE 退避パスワードＷ  TO 連携－パスワード.
000930     MOVE 退避レベルＷ      TO 連携－レベル.
000931     WRITE 連携－レコード.
000932     IF 状態キー  NOT =  "00"
000933        MOVE SPACE TO エラー内容
000934        STRING "連携ファイル書込みエラー:"  DELIMITED BY SIZE
000935                状態キー                    DELIMITED BY SIZE
000936           INTO エラー内容
000937        END-STRING
000938        INVOKE POW-SELF "DisplayMessage" USING エラー内容 "メッセージ"
000939        PERFORM ファイル閉鎖
000940        CLOSE 連携ファイル
000941        EXIT PROGRAM
000942     END-IF.
000943*
000944     CLOSE 連携ファイル.
000945*
000946     PERFORM ファイル閉鎖.
000947*
000948*--------------------------------------------------------
000949*
000950*    / YAWARAを呼出して終了 /
000951     INVOKE POW-SELF "Execute" USING 起動プログラム名 POW-SWSHOWNORMAL.
000952*
000953* MainFormを閉じます。
000954     INVOKE POW-SELF "CloseForm"
000955
000956*-------------------------------------------------------
000957     EXIT PROGRAM.
000958*
000959*================================================================*
000960*</SCRIPT>
000961 END PROGRAM     "チェックサブ".
000962 IDENTIFICATION  DIVISION.
000963* 設定ボタン-Click.
000964 PROGRAM-ID.     POW-SCRIPTLET4.
000965*<SCRIPT DIVISION="PROCEDURE", CONTROL="設定ボタン", EVENT="Click", POW-NAME="SCRIPTLET4", TYPE="ETC">
000966 ENVIRONMENT     DIVISION.
000967 DATA            DIVISION.
000968 WORKING-STORAGE SECTION.
000969 01 ReturnValue  PIC S9(9) COMP-5.
000970 PROCEDURE       DIVISION.
000971* フォームを開きます
000972     INVOKE POW-SELF "CallForm" USING "Passset"  RETURNING ReturnValue.
000973*</SCRIPT>
000974 END PROGRAM     POW-SCRIPTLET4.
000975 END PROGRAM     yawpas.
