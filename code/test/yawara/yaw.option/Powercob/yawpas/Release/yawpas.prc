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
000023 01  ���^�[���J�E���g     PIC 9  VALUE ZERO  GLOBAL.
000024*
000025*
000026*
000027********************
000028* �V�X�e���p���[�N *
000029********************
000030*
000031 01 ���s                    PIC X(2)  VALUE X"0D0A" GLOBAL.
000032 01 �G���[�t���O            PIC X(3) VALUE SPACE GLOBAL.
000033*
000034 01 �N���v���O������        PIC X(20) VALUE SPACE GLOBAL.
000035*
000036****************************
000037* ������t�@�C���p���[�N *
000038*****************************
000039*YENV.DAT��1�s�ڏ��
000040* ���(LAN�ADIR�ANORMAL),��R�[�h,�f�[�^�t�H���_,�p�X���[�h��ʂ��肩
000041 01 ������t�@�C�����e�v GLOBAL.
000042    03 ����|��ނv             PIC X(10) VALUE SPACE.
000043    03 ����|����R�[�h�v       PIC X(2)  VALUE SPACE.
000044    03 ����|�f�[�^�t�H���_�v   PIC X(150) VALUE SPACE.
000045    03 ����|�p�X���[�h�L�v     PIC X(10) VALUE SPACE.
000046*
000047*</SCRIPT>
000048 CONSTANT        SECTION.
000049 LINKAGE         SECTION.
000050 01  POW-FORM IS GLOBAL.
000051   02  POW-SELF OBJECT REFERENCE AMethodSetyawpas.
000052   02  POW-SUPER  PIC X(4).
000053   02  POW-THIS OBJECT REFERENCE AMethodSetyawpas.
000054   02  �p�X���[�h OBJECT REFERENCE AMixed-DCmTextbox-Main-with-DCfGroupItem-Main.
000055   02  CmStatic1 OBJECT REFERENCE AMixed-DCmSText-Main-with-DCfGroupItem-Main.
000056   02  ���s�{�^�� OBJECT REFERENCE AMixed-DCmPush-Main-with-DCfGroupItem-Main.
000057   02  �ݒ�{�^�� OBJECT REFERENCE AMixed-DCmPush-Main-with-DCfGroupItem-Main.
000058   02  �N�������x�� OBJECT REFERENCE AMixed-DCmSText-Main-with-DCfGroupItem-Main.
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
000089* �T�u�|�b�a�q�f�[�^�|�쐬.
000090 PROGRAM-ID.     "�T�u�|�b�a�q�f�[�^�|�쐬" IS COMMON.
000091*<SCRIPT DIVISION="PROCEDURE", NAME="�T�u�|�b�a�q�f�[�^�|�쐬">
000092 ENVIRONMENT     DIVISION.
000093 INPUT-OUTPUT    SECTION.
000094 FILE-CONTROL.
000095     SELECT  ���t�@�C��      ASSIGN      TO         "C:\MAKISHISYS\YAWOBJ\W85.CBR"
000096                             ORGANIZATION             IS  LINE SEQUENTIAL
000097                             ACCESS MODE              IS  SEQUENTIAL
000098                             FILE        STATUS       IS  ��ԃL�[
000099                             LOCK        MODE         IS  AUTOMATIC.
000100*
000101     SELECT  �V�t�@�C��      ASSIGN      TO         "C:\MAKISHISYS\YAWOBJ\COBOL85.CBR"
000102                             ORGANIZATION             IS  LINE SEQUENTIAL
000103                             ACCESS MODE              IS  SEQUENTIAL
000104                             FILE        STATUS       IS  ��ԃL�[
000105                             LOCK        MODE         IS  AUTOMATIC.
000106*
000107*------------------------------------------------------------------------------------*
000108 DATA            DIVISION.
000109 FILE            SECTION.
000110**
000111* �ϒ�
000112 FD  ���t�@�C��
000113      RECORD IS VARYING IN SIZE FROM  1 TO 256 CHARACTERS DEPENDING ON �����R�[�h��.
000114 01  ���|���R�[�h.
000115    03 ���|������.
000116      05 ���|����     PIC X(1) OCCURS 1 TO 256 TIMES DEPENDING ON �����R�[�h��.
000117*
000118 FD  �V�t�@�C��
000119      RECORD IS VARYING IN SIZE FROM  1 TO 256 CHARACTERS DEPENDING ON �V���R�[�h��.
000120 01  �V�|���R�[�h.
000121    03 �V�|������.
000122      05 �V�|����     PIC X(1) OCCURS 1 TO 256 TIMES DEPENDING ON �V���R�[�h��.
000123**
000124* 
000125*================================================================*
000126 WORKING-STORAGE SECTION.
000127*================================================================*
000128 01 ��ԃL�[     PIC X(2) VALUE SPACE.
000129 01 �I���t���O   PIC X(3)  VALUE SPACE.
000130 01 �I���t���O�P PIC X(3)  VALUE SPACE.
000131*
000132 01 �����R�[�h�� PIC 9(3) BINARY VALUE 256.
000133 01 �V���R�[�h�� PIC 9(3) BINARY VALUE 256.
000134*
000135 01 �J�E���^     PIC S9(3) VALUE ZERO.
000136 01 �������v     PIC S9(3) VALUE ZERO.
000137*
000138 01 ���@�����e�v.
000139    03 �p�X���v  PIC X(150).
000140*
000141 01 �����e�v     PIC X(256).
000142 01 �V���e�v     PIC X(256).
000143*
000144 01 �k�`�m�敪   PIC 9 VALUE ZERO.
000145*
000146*================================================================*
000147 PROCEDURE       DIVISION.
000148*================================================================*
000149***********
000150* �������� *
000151***********
000152     PERFORM ������.
000153***********
000154* �又��   *
000155***********
000156     PERFORM �t�@�C���쐬����.
000157***********
000158* �I������ *
000159***********
000160     PERFORM �I������.
000161     EXIT PROGRAM.
000162*
000163*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
000164*================================================================*
000165 ������ SECTION.
000166*================================================================*
000167     PERFORM �t�@�C���I�[�v��.
000168*
000169*================================================================*
000170 �t�@�C���I�[�v�� SECTION.
000171*
000172     OPEN OUTPUT �V�t�@�C��.
000173     IF ( ��ԃL�[  NOT =  "00" )
000174        INVOKE POW-SELF "DisplayMessage" USING "�V�t�@�C�����ُ�ł��i�n�o�j" "���b�Z�[�W"
000175        PERFORM �t�@�C����
000176        MOVE "YES" TO �G���[�t���O
000177        EXIT PROGRAM
000178     END-IF.
000179     OPEN INPUT  ���t�@�C��.
000180     IF ( ��ԃL�[  NOT =  "00" )
000181        INVOKE POW-SELF "DisplayMessage" USING "���t�@�C�����ُ�ł��i�n�o�j" "���b�Z�[�W"
000182        PERFORM �t�@�C����
000183        MOVE "YES" TO �G���[�t���O
000184        EXIT PROGRAM
000185     END-IF.
000186*
000187*================================================================*
000188 �t�@�C���쐬���� SECTION.
000189*================================================================*
000190*
000191     IF ����|��ނv = "LAN"
000192        MOVE 1    TO �k�`�m�敪
000193     ELSE
000194        MOVE ZERO TO �k�`�m�敪
000195     END-IF.
000196*   
000197     INITIALIZE ���@�����e�v.
000198*
000199     MOVE ����|�f�[�^�t�H���_�v TO �p�X���v.
000200*
000201     IF ( �p�X���v = SPACE )
000202           INVOKE POW-SELF "DisplayMessage" USING "�f�[�^�t�H���_���w�肳��Ă��܂���B" "���b�Z�[�W"
000203           MOVE "YES" TO �G���[�t���O
000204     ELSE
000205           PERFORM VARYING �������v FROM 150 BY -1 
000206                   UNTIL ( �������v <= ZERO ) OR ( �p�X���v(�������v:1) NOT = SPACE )
000207              CONTINUE
000208           END-PERFORM         
000209           PERFORM �t�@�C���X�V����
000210     END-IF.
000211*
000212*================================================================*
000213 �t�@�C���X�V���� SECTION.
000214*================================================================*
000215** ���t�@�C���̓��e��]�L
000216     MOVE SPACE TO �I���t���O�P.
000217     PERFORM  ���t�@�C���Ǎ�.
000218*
000219     PERFORM UNTIL ( �I���t���O�P = "YES" )
000220         MOVE SPACE        TO �����e�v
000221         MOVE ���|���R�[�h TO �����e�v
000222         MOVE SPACE        TO �V���e�v
000223         PERFORM �p�X�ύX����
000224         MOVE �V���e�v     TO �V�|���R�[�h
000225         PERFORM �V�f�[�^����
000226*
000227         PERFORM  ���t�@�C���Ǎ�
000228     END-PERFORM.
000229*
000230*================================================================*
000231 ���t�@�C���Ǎ� SECTION.
000232*================================================================*
000233     READ ���t�@�C�� NEXT
000234     AT END
000235         MOVE "YES" TO �I���t���O�P
000236     END-READ.
000237*
000238*================================================================*
000239 �p�X�ύX���� SECTION.
000240*================================================================*
000241     EVALUATE �����e�v(1:6)
000242     WHEN "JUSINJ"
000243         IF �����e�v(1:7) = "JUSINJ2"
000244            STRING "JUSINJ2L="            DELIMITED BY SIZE
000245                   �p�X���v(1:�������v)   DELIMITED BY SIZE
000246                   "DATA\JUSINJ2.DAT"   DELIMITED BY SIZE
000247                INTO �V���e�v
000248            END-STRING
000249         ELSE
000250            STRING "JUSINJL="             DELIMITED BY SIZE
000251                   �p�X���v(1:�������v)   DELIMITED BY SIZE
000252                   "DATA\JUSINJ.DAT"   DELIMITED BY SIZE
000253                INTO �V���e�v
000254            END-STRING
000255         END-IF
000256     WHEN "UKETUK"
000257         STRING "UKETUKEL="            DELIMITED BY SIZE
000258                �p�X���v(1:�������v)   DELIMITED BY SIZE
000259                "DATA\UKETUKE.DAT"  DELIMITED BY SIZE
000260             INTO �V���e�v
000261         END-STRING
000262     WHEN "SEKIRO"
000263         STRING "SEKIROKL="            DELIMITED BY SIZE
000264                �p�X���v(1:�������v)   DELIMITED BY SIZE
000265                "DATA\SEKIROK.DAT"  DELIMITED BY SIZE
000266             INTO �V���e�v
000267         END-STRING
000268     WHEN "HUSYOU"
000269         STRING "HUSYOUL="             DELIMITED BY SIZE
000270                �p�X���v(1:�������v)   DELIMITED BY SIZE
000271                "DATA\HUSYOU.DAT"   DELIMITED BY SIZE
000272             INTO �V���e�v
000273         END-STRING
000274     WHEN "HUGEIN"
000275         STRING "HUGEINL="             DELIMITED BY SIZE
000276                �p�X���v(1:�������v)   DELIMITED BY SIZE
000277                "DATA\HUGEIN.DAT"   DELIMITED BY SIZE
000278             INTO �V���e�v
000279         END-STRING
000280     WHEN "CHOKEI"
000281         STRING "CHOKEIL="             DELIMITED BY SIZE
000282                �p�X���v(1:�������v)   DELIMITED BY SIZE
000283                "DATA\CHOKEI.DAT"   DELIMITED BY SIZE
000284             INTO �V���e�v
000285         END-STRING
000286     WHEN "KAIKEI"
000287         STRING "KAIKEIL="             DELIMITED BY SIZE
000288                �p�X���v(1:�������v)   DELIMITED BY SIZE
000289                "DATA\KAIKEI.DAT"   DELIMITED BY SIZE
000290             INTO �V���e�v
000291         END-STRING
000292     WHEN "MEMOL="
000293         STRING "MEMOL="               DELIMITED BY SIZE
000294                �p�X���v(1:�������v)   DELIMITED BY SIZE
000295                "DATA\MEMO.DAT"     DELIMITED BY SIZE
000296             INTO �V���e�v
000297         END-STRING
000298     WHEN "RECEPT"
000299         STRING "RECEPTL="             DELIMITED BY SIZE
000300                �p�X���v(1:�������v)   DELIMITED BY SIZE
000301                "DATA\RECEPT.DAT"   DELIMITED BY SIZE
000302             INTO �V���e�v
000303         END-STRING
000304     WHEN "NYUKIN"
000305         STRING "NYUKINL="             DELIMITED BY SIZE
000306                �p�X���v(1:�������v)   DELIMITED BY SIZE
000307                "DATA\NYUKIN.DAT"   DELIMITED BY SIZE
000308             INTO �V���e�v
000309         END-STRING
000310     WHEN "KARUTE"
000311         STRING "KARUTEL="             DELIMITED BY SIZE
000312                �p�X���v(1:�������v)   DELIMITED BY SIZE
000313                "DATA\KARUTE.DAT"   DELIMITED BY SIZE
000314             INTO �V���e�v
000315         END-STRING
000316     WHEN "SYOMEI"
000317         STRING "SYOMEIRL="            DELIMITED BY SIZE
000318                �p�X���v(1:�������v)   DELIMITED BY SIZE
000319                "DATA\SYOMEIRR.DAT"    DELIMITED BY SIZE
000320             INTO �V���e�v
000321         END-STRING
000322     WHEN "JIGYOS"
000323         STRING "JIGYOSL="                    DELIMITED BY SIZE
000324                �p�X���v(1:�������v)          DELIMITED BY SIZE
000325                "DATA\MASTER\JIGYOS.DAT"   DELIMITED BY SIZE
000326             INTO �V���e�v
000327         END-STRING
000328     WHEN "SEIGYO"
000329         IF �k�`�m�敪 = 1
000330            MOVE "SEIGYOL=C:\MAKISHISYS\DATA\MASTER\SEIGYO.DAT" TO �V���e�v      
000331         ELSE
000332           STRING "SEIGYOL="                    DELIMITED BY SIZE
000333                  �p�X���v(1:�������v)          DELIMITED BY SIZE
000334                  "DATA\MASTER\SEIGYO.DAT"   DELIMITED BY SIZE
000335               INTO �V���e�v
000336           END-STRING
000337         END-IF
000338     WHEN "SEJOHO"
000339         STRING "SEJOHOL="                    DELIMITED BY SIZE
000340                �p�X���v(1:�������v)          DELIMITED BY SIZE
000341                "DATA\MASTER\SEJOHO.DAT"   DELIMITED BY SIZE
000342             INTO �V���e�v
000343         END-STRING
000344     WHEN "IDKANR"
000345         STRING "IDKANRL="                    DELIMITED BY SIZE
000346                �p�X���v(1:�������v)          DELIMITED BY SIZE
000347               "DATA\MASTER\IDKANR.DAT"   DELIMITED BY SIZE
000348            INTO �V���e�v
000349         END-STRING
000350     WHEN "CHORIY"
000351         STRING "CHORIYUL="                   DELIMITED BY SIZE
000352                �p�X���v(1:�������v)          DELIMITED BY SIZE
000353                "DATA\MASTER\CHORIYU.DAT"   DELIMITED BY SIZE
000354             INTO �V���e�v
000355         END-STRING
000356     WHEN "REIGAI"
000357         STRING "REIGAIL="                   DELIMITED BY SIZE
000358                �p�X���v(1:�������v)          DELIMITED BY SIZE
000359                "DATA\MASTER\REIGAI.DAT"   DELIMITED BY SIZE
000360             INTO �V���e�v
000361         END-STRING
000362     WHEN "HOKENE"
000363         STRING "HOKENEXL="                   DELIMITED BY SIZE
000364                �p�X���v(1:�������v)          DELIMITED BY SIZE
000365                "DATA\MASTER\HOKENEX.DAT"   DELIMITED BY SIZE
000366             INTO �V���e�v
000367         END-STRING
000368     WHEN "BARKAN"
000369         STRING "BARKANRL="                   DELIMITED BY SIZE
000370                �p�X���v(1:�������v)          DELIMITED BY SIZE
000371                "DATA\MASTER\BARKANR.DAT"  DELIMITED BY SIZE
000372             INTO �V���e�v
000373         END-STRING
000374     WHEN "BANGOK"
000375         STRING "BANGOKL="                    DELIMITED BY SIZE
000376                �p�X���v(1:�������v)          DELIMITED BY SIZE
000377                "DATA\MASTER\BANGOK.DAT"   DELIMITED BY SIZE
000378             INTO �V���e�v
000379         END-STRING
000380*
000381     WHEN "HHUSYO"
000382         STRING "HHUSYOUL="             DELIMITED BY SIZE
000383                �p�X���v(1:�������v)    DELIMITED BY SIZE
000384                "DATA\H_HUSYOU.DAT"  DELIMITED BY SIZE
000385             INTO �V���e�v
000386         END-STRING
000387     WHEN "HRECEL"
000388         STRING "HRECEL="               DELIMITED BY SIZE
000389                �p�X���v(1:�������v)    DELIMITED BY SIZE
000390                "DATA\H_RECE.DAT"    DELIMITED BY SIZE
000391             INTO �V���e�v
000392         END-STRING
000393     WHEN "HNIKEI"
000394         STRING "HNIKEIL="              DELIMITED BY SIZE
000395                �p�X���v(1:�������v)    DELIMITED BY SIZE
000396                "DATA\H_NIKEI.DAT"   DELIMITED BY SIZE
000397             INTO �V���e�v
000398         END-STRING
000399     WHEN "HSEJOH"
000400         STRING "HSEJOHOL="                     DELIMITED BY SIZE
000401                �p�X���v(1:�������v)            DELIMITED BY SIZE
000402                "DATA\MASTER\H_SEJOHO.DAT"   DELIMITED BY SIZE
000403             INTO �V���e�v
000404         END-STRING
000405     WHEN "HSETAN"
000406         STRING "HSETANTL="                     DELIMITED BY SIZE
000407                �p�X���v(1:�������v)            DELIMITED BY SIZE
000408                "DATA\MASTER\H_SETANT.DAT"   DELIMITED BY SIZE
000409             INTO �V���e�v
000410         END-STRING
000411*
000412     WHEN "RYOSYU"
000413         STRING "RYOSYUL="              DELIMITED BY SIZE
000414                �p�X���v(1:�������v)    DELIMITED BY SIZE
000415                "DATA\RYOSYU.DAT"   DELIMITED BY SIZE
000416             INTO �V���e�v
000417         END-STRING
000418     WHEN "ROUSAI"
000419         STRING "ROUSAIJL="              DELIMITED BY SIZE
000420                �p�X���v(1:�������v)    DELIMITED BY SIZE
000421                "DATA\ROUSAIJ.DAT"   DELIMITED BY SIZE
000422             INTO �V���e�v
000423         END-STRING
000424     WHEN "JIBAIJ"
000425         STRING "JIBAIJL="              DELIMITED BY SIZE
000426                �p�X���v(1:�������v)    DELIMITED BY SIZE
000427                "DATA\JIBAIJ.DAT"   DELIMITED BY SIZE
000428             INTO �V���e�v
000429         END-STRING
000430     WHEN "SEIHOJ"
000431         STRING "SEIHOJL="              DELIMITED BY SIZE
000432                �p�X���v(1:�������v)    DELIMITED BY SIZE
000433                "DATA\SEIHOJ.DAT"   DELIMITED BY SIZE
000434             INTO �V���e�v
000435         END-STRING
000436     WHEN "DMKIRO"
000437         STRING "DMKIROKL="              DELIMITED BY SIZE
000438                �p�X���v(1:�������v)     DELIMITED BY SIZE
000439                "DATA\DMKIROK.DAT"   DELIMITED BY SIZE
000440             INTO �V���e�v
000441         END-STRING
000442     WHEN "HNOURY"
000443         STRING "HNOURYOL="             DELIMITED BY SIZE
000444                �p�X���v(1:�������v)    DELIMITED BY SIZE
000445                "DATA\H_NOURYO.DAT"   DELIMITED BY SIZE
000446             INTO �V���e�v
000447         END-STRING
000448     WHEN "TEKIYO"
000449         STRING "TEKIYOL="             DELIMITED BY SIZE
000450                �p�X���v(1:�������v)   DELIMITED BY SIZE
000451                "DATA\TEKIYO.DAT"   DELIMITED BY SIZE
000452             INTO �V���e�v
000453         END-STRING
000454     WHEN "HNOYOT"
000455         STRING "HNOYOTEL="             DELIMITED BY SIZE
000456                �p�X���v(1:�������v)    DELIMITED BY SIZE
000457                "DATA\H_NOYOTE.DAT"   DELIMITED BY SIZE
000458             INTO �V���e�v
000459         END-STRING
000460     WHEN "RECERJ"
000461         STRING "RECERJL="             DELIMITED BY SIZE
000462                �p�X���v(1:�������v)   DELIMITED BY SIZE
000463                "DATA\RECERJ.DAT"   DELIMITED BY SIZE
000464             INTO �V���e�v
000465         END-STRING
000466**
000467     WHEN "KINRIN"
000468         STRING "KINRINL="                     DELIMITED BY SIZE
000469                �p�X���v(1:�������v)            DELIMITED BY SIZE
000470                "DATA\MASTER\KINRIN.DAT"   DELIMITED BY SIZE
000471             INTO �V���e�v
000472         END-STRING
000473     WHEN "KENSAK"
000474         IF �k�`�m�敪 = 1
000475            MOVE "KENSAKUL=C:\MAKISHISYS\DATA\MASTER\KENSAKU.DAT" TO �V���e�v      
000476         ELSE
000477            STRING "KENSAKUL="                     DELIMITED BY SIZE
000478                   �p�X���v(1:�������v)            DELIMITED BY SIZE
000479                   "DATA\MASTER\KENSAKU.DAT"   DELIMITED BY SIZE
000480                INTO �V���e�v
000481            END-STRING
000482         END-IF
000483     WHEN "HISHIL"
000484         STRING "HISHIL="                       DELIMITED BY SIZE
000485                �p�X���v(1:�������v)            DELIMITED BY SIZE
000486                "DATA\MASTER\H_ISHI.DAT"     DELIMITED BY SIZE
000487             INTO �V���e�v
000488         END-STRING
000489     WHEN "UMEISY"
000490         STRING "UMEISYOL="                     DELIMITED BY SIZE
000491                �p�X���v(1:�������v)            DELIMITED BY SIZE
000492                "DATA\MASTER\UMEISYO.DAT"   DELIMITED BY SIZE
000493             INTO �V���e�v
000494         END-STRING
000495     WHEN "HSEIGY"
000496         IF �k�`�m�敪 = 1
000497            MOVE "HSEIGYOL=C:\MAKISHISYS\DATA\MASTER\H_SEIGYO.DAT" TO �V���e�v      
000498         ELSE
000499             STRING "HSEIGYOL="                     DELIMITED BY SIZE
000500                    �p�X���v(1:�������v)            DELIMITED BY SIZE
000501                    "DATA\MASTER\H_SEIGYO.DAT"   DELIMITED BY SIZE
000502                 INTO �V���e�v
000503             END-STRING
000504         END-IF        
000505     WHEN "SISETJ"
000506         STRING "SISETJUL="                     DELIMITED BY SIZE
000507                �p�X���v(1:�������v)            DELIMITED BY SIZE
000508                "DATA\MASTER\SISETJU.DAT"   DELIMITED BY SIZE
000509             INTO �V���e�v
000510         END-STRING
000511     WHEN "SISETU"
000512         STRING "SISETUL="                     DELIMITED BY SIZE
000513                �p�X���v(1:�������v)           DELIMITED BY SIZE
000514                "DATA\MASTER\SISETU.DAT"   DELIMITED BY SIZE
000515             INTO �V���e�v
000516         END-STRING
000517**
000518     WHEN OTHER
000519         MOVE �����e�v TO �V���e�v
000520     END-EVALUATE.
000521*
000522     PERFORM VARYING �J�E���^ FROM 256 BY -1 
000523             UNTIL ( �J�E���^ <= ZERO ) OR ( �V���e�v(�J�E���^:1) NOT = SPACE )
000524        CONTINUE
000525     END-PERFORM.
000526     MOVE �J�E���^ TO �V���R�[�h��.
000527*
000528*================================================================*
000529 �V�f�[�^���� SECTION.
000530*================================================================*
000531     WRITE �V�|���R�[�h.
000532     IF ( ��ԃL�[ NOT = "00" )
000533        INVOKE POW-SELF "DisplayMessage" USING "�V�t�@�C�����ُ�ł��i�v�q�j" "���b�Z�[�W"
000534        MOVE "YES" TO �G���[�t���O
000535     END-IF.
000536*
000537*================================================================*
000538 �I������ SECTION.
000539*================================================================*
000540     PERFORM �t�@�C����.
000541*
000542*================================================================*
000543 �t�@�C���� SECTION.
000544*
000545     CLOSE ���t�@�C�� �V�t�@�C��.
000546*
000547*</SCRIPT>
000548 END PROGRAM     "�T�u�|�b�a�q�f�[�^�|�쐬".
000549 IDENTIFICATION  DIVISION.
000550* yawpas-Opened.
000551 PROGRAM-ID.     POW-SCRIPTLET1.
000552*<SCRIPT DIVISION="PROCEDURE", CONTROL="yawpas", EVENT="Opened", POW-NAME="SCRIPTLET1", TYPE="FORM">
000553 ENVIRONMENT             DIVISION.
000554 INPUT-OUTPUT            SECTION.
000555 FILE-CONTROL.
000556*
000557     SELECT  ������t�@�C��    ASSIGN      TO       "C:\MAKISHISYS\DATA\COMMON\YENV.DAT"
000558                             ORGANIZATION             IS  LINE SEQUENTIAL
000559                             ACCESS MODE              IS  SEQUENTIAL
000560                             FILE        STATUS       IS  ��ԃL�[
000561                             LOCK        MODE         IS  AUTOMATIC.
000562*
000563*****************************************************************
000564*                      DATA DIVISION                             *
000565******************************************************************
000566 DATA                    DIVISION.
000567 FILE                    SECTION.
000568* �ϒ�
000569 FD ������t�@�C��
000570      RECORD IS VARYING IN SIZE FROM  1 TO 500 CHARACTERS DEPENDING ON ���R�[�h��.
000571 01 ���|���R�[�h.
000572    03 ������.
000573       05 ���� PIC X OCCURS 1 TO 500 TIMES DEPENDING ON ���R�[�h��.
000574*
000575******************************************************************
000576*                WORKING-STORAGE SECTION                         *
000577******************************************************************
000578 WORKING-STORAGE         SECTION.
000579 01 ��ԃL�[                      PIC X(2) VALUE SPACE.
000580 01 �G���[���e                    PIC X(80) VALUE SPACE.
000581 01 ���R�[�h��                    PIC 9(4) BINARY VALUE 500.
000582 01 �I���t���O                    PIC X(3) VALUE SPACE.
000583*
000584 01 ���e�v            PIC X(500) VALUE SPACE.
000585*
000586*--- Msgbox�p ---*
000587 01  STYL           PIC S9(9) COMP-5.
000588 01  RET            PIC S9(9) COMP-5.
000589 01  ���b�Z�[�W�v   PIC X(128) VALUE SPACE.
000590*
000591*--- �R�s�[C �A�g�p ---*
000592 01  ���t�@�C�����v PIC X(80) VALUE "C:\MAKISHISYS\YAWOBJ\COBOL85.CBR".
000593 01  ��t�@�C�����v PIC X(80) VALUE "C:\MAKISHISYS\YAWOBJ\W85.CBR".
000594 01  �v���O�������v PIC X(8)  VALUE "filsave".
000595*
000596******************************************************************
000597*                      PROCEDURE  DIVISION                       *
000598******************************************************************
000599 PROCEDURE               DIVISION.
000600*------------------------------------------------------------------------------*
000601* ������t�@�C�� YENV.DAT
000602*
000603* ���(LAN�ADIR�ANORMAL),��R�[�h,�f�[�^�t�H���_,�p�X���[�h��ʂ��肩
000604*
000605*����R�[�h�͈Ӗ��Ȃ�
000606*
000607*
000608*LAN
000609* LAN,01,\\HOST\,
000610
000611*LAN�Ńp�X���[�h����
000612* LAN,01,\\HOST\,PASS
000613
000614*�ʃt�H���_
000615* DIR,01,c:\makishisys\�_��\,
000616
000617*�ʃt�H���_�Ńp�X���[�h����
000618* DIR,01,c:\makishisys\�_��\,PASS
000619
000620*�m�[�}���Ńp�X���[�h����
000621* NORMAL,01,,PASS
000622*
000623*------------------------------------------------------------------------------*
000624**
000625** �N�������x���̈ʒu�𒆐S�ɂ���B
000626     MOVE 210 TO "Left" OF �N�������x��.
000627     MOVE 255 TO "Top" OF �N�������x��.
000628**
000629**
000630     OPEN INPUT ������t�@�C��.
000631     IF ��ԃL�[  =  "35"
000632        INVOKE POW-SELF "DisplayMessage" USING "������t�@�C���������̂ŋN���ł��܂���B" "���b�Z�[�W"
000633        PERFORM �ُ�I��
000634     ELSE
000635        IF ��ԃL�[  NOT =  "00"
000636           INVOKE POW-SELF "DisplayMessage" USING "������t�@�C��OPEN�G���[�B�N���ł��܂���B" "���b�Z�[�W"
000637           PERFORM �ُ�I��
000638        END-IF   
000639     END-IF.
000640**
000641*     
000642** // ������t�@�C���P�s�ړǍ��� //
000643     MOVE SPACE  TO ���e�v ������t�@�C�����e�v.
000644     MOVE SPACE  TO �I���t���O.
000645     PERFORM ������t�@�C���Ǎ�.
000646*
000647     IF �I���t���O NOT = "YES"
000648        MOVE ���|���R�[�h TO ���e�v
000649        IF ���e�v NOT = SPACE
000650*         GLOBAL�փZ�b�g
000651            UNSTRING  ���e�v  DELIMITED BY "," 
000652                      INTO  ����|��ނv
000653                            ����|����R�[�h�v
000654                            ����|�f�[�^�t�H���_�v
000655                            ����|�p�X���[�h�L�v
000656            END-UNSTRING
000657        END-IF
000658     END-IF.   
000659*
000660     CLOSE ������t�@�C��.
000661*
000662*     IF ����|����R�[�h�v = SPACE
000663*        INVOKE POW-SELF "DisplayMessage" USING "������t�@�C���̓��e���ُ�ł��B�N���ł��܂���B" "���b�Z�[�W"
000664*        PERFORM �ُ�I��
000665*     END-IF.   
000666*
000667*     STRING "YAWARA"             DELIMITED BY SIZE
000668*            ����|����R�[�h�v  DELIMITED BY SIZE
000669*            ".EXE"              DELIMITED BY SIZE
000670*     INTO �N���v���O������.
000671*
000672      MOVE "YAWARA01" TO �N���v���O������.
000673*
000674*------------------------------------------------------*
000675*
000676     IF ����|��ނv  = "LAN" OR "DIR"
000677*
000678*---    �b�a�q�ޔ� (���t�@�C�� �� ��t�@�C��) ---*
000679         CALL �v���O�������v WITH C LINKAGE
000680               USING BY REFERENCE ���t�@�C�����v
000681                     BY REFERENCE ��t�@�C�����v
000682*
000683         IF ( PROGRAM-STATUS = ZERO )
000684*---        �b�a�q�쐬 ---*
000685            MOVE SPACE TO �G���[�t���O
000686            CALL "�T�u�|�b�a�q�f�[�^�|�쐬"
000687*
000688            IF ( �G���[�t���O NOT = SPACE )
000689*---           �b�a�q���� (��t�@�C�� �� ���t�@�C��) ---*
000690               CALL �v���O�������v WITH C LINKAGE
000691                         USING BY REFERENCE ��t�@�C�����v
000692                               BY REFERENCE ���t�@�C�����v
000693               INVOKE POW-SELF "DisplayMessage" USING "�b�a�q�쐬�Ɏ��s���܂����B�N���ł��܂���B" "���b�Z�[�W"
000694               PERFORM �ُ�I��
000695            END-IF
000696*
000697         ELSE
000698            PERFORM �f�[�^�ޔ��G���[
000699         END-IF
000700     END-IF.
000701*
000702*------------------------------------------------------*
000703*
000704*    �p�X���[�h��ʂ��肩
000705     IF ����|�p�X���[�h�L�v  = "PASS"
000706        MOVE POW-FALSE TO "Visible" OF �N�������x��
000707     ELSE
000708        MOVE "�N����" TO "Caption" OF POW-SELF
000709*
000710*       / YAWARA���ďo���ďI�� /
000711        INVOKE POW-SELF "Execute" USING �N���v���O������ POW-SWSHOWNORMAL
000712        INVOKE POW-SELF "CloseForm"
000713        EXIT PROGRAM
000714*
000715     END-IF.
000716*
000717     EXIT PROGRAM.     
000718*      
000719*================================================================*
000720 ������t�@�C���Ǎ� SECTION.
000721*
000722     READ ������t�@�C�� NEXT
000723     AT END
000724         MOVE "YES" TO �I���t���O
000725     END-READ.
000726*
000727*================================================================*
000728 �f�[�^�ޔ��G���[ SECTION.
000729*
000730     MOVE SPACE TO ���b�Z�[�W�v.
000731     STRING "�ޔ����s�F"    DELIMITED BY SIZE
000732            ���t�@�C�����v  DELIMITED BY SIZE
000733            INTO ���b�Z�[�W�v
000734     END-STRING.
000735*/ �x���A�C�R�� /*
000736     COMPUTE STYL = POW-DMOK + POW-DMICONWARNING + POW-DMDEFBUTTON1.
000737     INVOKE POW-SELF "DisplayMessage" USING ���b�Z�[�W�v "�x��" STYL.
000738     PERFORM �ُ�I��.
000739*
000740*================================================================*
000741 �ُ�I�� SECTION.
000742*
000743     INVOKE POW-SELF "CloseForm".
000744     EXIT PROGRAM.
000745*
000746*================================================================*
000747     
000748*</SCRIPT>
000749 END PROGRAM     POW-SCRIPTLET1.
000750 IDENTIFICATION  DIVISION.
000751* �p�X���[�h-Return.
000752 PROGRAM-ID.     POW-SCRIPTLET2.
000753*<SCRIPT DIVISION="PROCEDURE", CONTROL="�p�X���[�h", EVENT="Return", POW-NAME="SCRIPTLET2", TYPE="ETC">
000754 ENVIRONMENT     DIVISION.
000755 DATA            DIVISION.
000756 WORKING-STORAGE SECTION.
000757 PROCEDURE       DIVISION.
000758*
000759     IF ���^�[���J�E���g = ZERO
000760        MOVE 1   TO ���^�[���J�E���g 
000761        MOVE POW-FALSE  TO "Enabled" OF ���s�{�^��
000762        CALL "�`�F�b�N�T�u"
000763        MOVE POW-TRUE   TO "Enabled" OF ���s�{�^��
000764        MOVE ZERO TO ���^�[���J�E���g 
000765     END-IF.   
000766*
000767     EXIT PROGRAM.
000768*</SCRIPT>
000769 END PROGRAM     POW-SCRIPTLET2.
000770 IDENTIFICATION  DIVISION.
000771* ���s�{�^��-Click.
000772 PROGRAM-ID.     POW-SCRIPTLET3.
000773*<SCRIPT DIVISION="PROCEDURE", CONTROL="���s�{�^��", EVENT="Click", POW-NAME="SCRIPTLET3", TYPE="ETC">
000774 ENVIRONMENT     DIVISION.
000775 DATA            DIVISION.
000776 WORKING-STORAGE SECTION.
000777 PROCEDURE       DIVISION.
000778*
000779     MOVE POW-FALSE  TO "Enabled" OF ���s�{�^��.
000780     CALL "�`�F�b�N�T�u".
000781     MOVE POW-TRUE   TO "Enabled" OF ���s�{�^��.
000782*
000783     EXIT PROGRAM.
000784*</SCRIPT>
000785 END PROGRAM     POW-SCRIPTLET3.
000786 IDENTIFICATION  DIVISION.
000787* �`�F�b�N�T�u.
000788 PROGRAM-ID.     "�`�F�b�N�T�u" IS COMMON.
000789*<SCRIPT DIVISION="PROCEDURE", NAME="�`�F�b�N�T�u">
000790 ENVIRONMENT             DIVISION.
000791 INPUT-OUTPUT            SECTION.
000792 FILE-CONTROL.
000793     SELECT  ��O�}�X�^      ASSIGN      TO        REIGAIL
000794                             ORGANIZATION             IS  INDEXED
000795                             ACCESS MODE              IS  DYNAMIC
000796                             RECORD KEY               IS  ��O�T�|�敪�R�[�h
000797                                                          ��O�T�|�ԍ��R�[�h
000798                             FILE STATUS              IS  ��ԃL�[
000799                             LOCK        MODE         IS  AUTOMATIC.
000800*
000801     SELECT  �A�g�t�@�C��    ASSIGN      TO        W001L
000802                             ORGANIZATION             IS  SEQUENTIAL
000803                             ACCESS MODE              IS  SEQUENTIAL
000804                             FILE        STATUS       IS  ��ԃL�[
000805                             LOCK        MODE         IS  AUTOMATIC.
000806*
000807*****************************************************************
000808*                      DATA DIVISION                             *
000809******************************************************************
000810 DATA                    DIVISION.
000811 FILE                    SECTION.
000812*
000813** ��O05(�p�X���[�h�p)
000814 FD  ��O�}�X�^          BLOCK   CONTAINS   1   RECORDS.
000815     COPY REIGAI05       OF  XFDLIB  JOINING   ��O�T   AS  PREFIX.
000816*
000817 FD  �A�g�t�@�C��  RECORD  CONTAINS 16 CHARACTERS.
000818 01  �A�g�|���R�[�h.
000819     03  �A�g�|�p�X���[�h       PIC X(6).
000820     03  �A�g�|���x��           PIC X.
000821     03  FILLER                 PIC X(9).
000822*
000823*
000824******************************************************************
000825*                WORKING-STORAGE SECTION                         *
000826******************************************************************
000827 WORKING-STORAGE         SECTION.
000828 01 ��ԃL�[                      PIC X(2) VALUE SPACE.
000829 01 �G���[���e                    PIC X(80) VALUE SPACE.
000830 01 �I���t���O                    PIC X(3) VALUE SPACE.
000831 01 ���݃t���O                    PIC X(3) VALUE SPACE.
000832 01 �p�X���[�h�v                  PIC X(6) VALUE SPACE.
000833 01 �ޔ��p�X���[�h�v              PIC X(6) VALUE SPACE.
000834 01 �ޔ����x���v                  PIC X    VALUE SPACE.
000835*
000836 01 �J�E���^                      PIC 9(3) VALUE ZERO.
000837*
000838*
000839******************************************************************
000840*                      PROCEDURE  DIVISION                       *
000841******************************************************************
000842 PROCEDURE               DIVISION.
000843*
000844     PERFORM ���̓`�F�b�N.
000845     PERFORM �t�@�C���I�[�v��.
000846     PERFORM �p�X���[�h�`�F�b�N. 
000847*
000848*================================================================*
000849*================================================================*
000850 ���̓`�F�b�N SECTION.
000851*
000852     IF "Text" OF �p�X���[�h  = SPACE
000853         INVOKE POW-SELF "DisplayMessage" USING "�p�X���[�h����͂��Ă��������B" "���b�Z�[�W"
000854         INVOKE �p�X���[�h "SetFocus"
000855         EXIT PROGRAM
000856     END-IF.
000857*
000858*================================================================*
000859 �t�@�C���I�[�v�� SECTION.
000860*
000861     OPEN INPUT ��O�}�X�^.
000862     IF ��ԃL�[  NOT =  "00"
000863        MOVE SPACE TO �G���[���e
000864        STRING "��O�}�X�^�I�[�v���G���[:"  DELIMITED BY SIZE
000865                ��ԃL�[                    DELIMITED BY SIZE
000866           INTO �G���[���e
000867        END-STRING
000868        INVOKE POW-SELF "DisplayMessage" USING �G���[���e "���b�Z�[�W"
000869        EXIT PROGRAM
000870     END-IF.
000871*
000872*================================================================*
000873 �p�X���[�h�`�F�b�N SECTION.
000874*
000875     MOVE "Text" OF �p�X���[�h TO �p�X���[�h�v.
000876     PERFORM ���݃`�F�b�N.
000877     IF ���݃t���O = SPACE
000878        INVOKE POW-SELF "DisplayMessage" USING "�p�X���[�h���o�^����Ă��܂���B" "���b�Z�[�W"
000879        INVOKE �p�X���[�h "SetFocus"
000880        PERFORM �t�@�C����
000881        EXIT PROGRAM
000882     ELSE
000883        PERFORM �A�g����
000884     END-IF.
000885*
000886*================================================================*
000887 �t�@�C���� SECTION.
000888*
000889     CLOSE ��O�}�X�^.
000890*
000891*================================================================*
000892 ���݃`�F�b�N SECTION.
000893*
000894     MOVE SPACE      TO ���݃t���O.
000895     MOVE 1          TO �J�E���^.
000896     MOVE 05         TO ��O�T�|�敪�R�[�h.
000897     MOVE ZERO       TO ��O�T�|�ԍ��R�[�h.
000898     READ ��O�}�X�^
000899     NOT INVALID KEY
000900        PERFORM UNTIL ( ���݃t���O                   = "YES" ) OR
000901                      ( ��O�T�|�p�X���[�h(�J�E���^) = SPACE ) OR
000902                      ( �J�E���^ > 90 )
000903            IF ��O�T�|�p�X���[�h(�J�E���^) = �p�X���[�h�v
000904                MOVE "YES"                    TO ���݃t���O
000905                MOVE �p�X���[�h�v             TO �ޔ��p�X���[�h�v
000906                MOVE ��O�T�|���x��(�J�E���^) TO �ޔ����x���v
000907            END-IF
000908            COMPUTE �J�E���^ = �J�E���^ + 1
000909        END-PERFORM
000910     END-READ.
000911*
000912*================================================================*
000913 �A�g���� SECTION.
000914*
000915*--- ���[�N�e�Ƀp�X���[�h�E���x������������ -----*
000916     OPEN OUTPUT �A�g�t�@�C��.
000917     IF ��ԃL�[  NOT =  "00"
000918        MOVE SPACE TO �G���[���e
000919        STRING "�A�g�t�@�C���I�[�v���G���[:"  DELIMITED BY SIZE
000920                ��ԃL�[                      DELIMITED BY SIZE
000921           INTO �G���[���e
000922        END-STRING
000923        INVOKE POW-SELF "DisplayMessage" USING �G���[���e "���b�Z�[�W"
000924        PERFORM �t�@�C����
000925        EXIT PROGRAM
000926     END-IF.
000927*
000928     MOVE SPACE             TO �A�g�|���R�[�h.
000929     MOVE �ޔ��p�X���[�h�v  TO �A�g�|�p�X���[�h.
000930     MOVE �ޔ����x���v      TO �A�g�|���x��.
000931     WRITE �A�g�|���R�[�h.
000932     IF ��ԃL�[  NOT =  "00"
000933        MOVE SPACE TO �G���[���e
000934        STRING "�A�g�t�@�C�������݃G���[:"  DELIMITED BY SIZE
000935                ��ԃL�[                    DELIMITED BY SIZE
000936           INTO �G���[���e
000937        END-STRING
000938        INVOKE POW-SELF "DisplayMessage" USING �G���[���e "���b�Z�[�W"
000939        PERFORM �t�@�C����
000940        CLOSE �A�g�t�@�C��
000941        EXIT PROGRAM
000942     END-IF.
000943*
000944     CLOSE �A�g�t�@�C��.
000945*
000946     PERFORM �t�@�C����.
000947*
000948*--------------------------------------------------------
000949*
000950*    / YAWARA���ďo���ďI�� /
000951     INVOKE POW-SELF "Execute" USING �N���v���O������ POW-SWSHOWNORMAL.
000952*
000953* MainForm����܂��B
000954     INVOKE POW-SELF "CloseForm"
000955
000956*-------------------------------------------------------
000957     EXIT PROGRAM.
000958*
000959*================================================================*
000960*</SCRIPT>
000961 END PROGRAM     "�`�F�b�N�T�u".
000962 IDENTIFICATION  DIVISION.
000963* �ݒ�{�^��-Click.
000964 PROGRAM-ID.     POW-SCRIPTLET4.
000965*<SCRIPT DIVISION="PROCEDURE", CONTROL="�ݒ�{�^��", EVENT="Click", POW-NAME="SCRIPTLET4", TYPE="ETC">
000966 ENVIRONMENT     DIVISION.
000967 DATA            DIVISION.
000968 WORKING-STORAGE SECTION.
000969 01 ReturnValue  PIC S9(9) COMP-5.
000970 PROCEDURE       DIVISION.
000971* �t�H�[�����J���܂�
000972     INVOKE POW-SELF "CallForm" USING "Passset"  RETURNING ReturnValue.
000973*</SCRIPT>
000974 END PROGRAM     POW-SCRIPTLET4.
000975 END PROGRAM     yawpas.
