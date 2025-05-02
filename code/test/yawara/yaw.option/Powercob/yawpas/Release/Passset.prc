000001 IDENTIFICATION  DIVISION.
000002* Passset.
000003 PROGRAM-ID.     Passset.
000004 ENVIRONMENT     DIVISION.
000005 CONFIGURATION   SECTION.
000006 POW-REPOSITORY.
000007     CLASS  AMethodSetPassset AS "TLB=P:\yawara\yaw.option\Powercob\yawpas\Release\~build.tlb,{734374BA-A5DF-4DC9-A319-6CAB611822D1},Fujitsu-PcobForm-4"
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
000022 CONSTANT        SECTION.
000023 LINKAGE         SECTION.
000024 01  POW-FORM IS GLOBAL.
000025   02  POW-SELF OBJECT REFERENCE AMethodSetPassset.
000026   02  POW-SUPER  PIC X(4).
000027   02  POW-THIS OBJECT REFERENCE AMethodSetPassset.
000028   02  ���p�X���[�h OBJECT REFERENCE AMixed-DCmTextbox-Main-with-DCfGroupItem-Main.
000029   02  �V�p�X���[�h OBJECT REFERENCE AMixed-DCmTextbox-Main-with-DCfGroupItem-Main.
000030   02  CmStatic1 OBJECT REFERENCE AMixed-DCmSText-Main-with-DCfGroupItem-Main.
000031   02  CmStatic2 OBJECT REFERENCE AMixed-DCmSText-Main-with-DCfGroupItem-Main.
000032   02  �o�^�{�^�� OBJECT REFERENCE AMixed-DCmPush-Main-with-DCfGroupItem-Main.
000033   02  CmStatic5 OBJECT REFERENCE AMixed-DCmSText-Main-with-DCfGroupItem-Main.
000034   02  �߂�{�^�� OBJECT REFERENCE AMixed-DCmPush-Main-with-DCfGroupItem-Main.
000035   02  ���x�� OBJECT REFERENCE AMixed-DCmTextbox-Main-with-DCfGroupItem-Main.
000036   02  CmStatic6 OBJECT REFERENCE AMixed-DCmSText-Main-with-DCfGroupItem-Main.
000037 01  Passset REDEFINES POW-FORM GLOBAL OBJECT REFERENCE AMethodSetPassset.
000038 01  POW-CONTROL-ID PIC S9(9) COMP-5.
000039 01  POW-EVENT-ID   PIC S9(9) COMP-5.
000040 01  POW-OLE-PARAM  PIC X(4).
000041 01  POW-OLE-RETURN PIC X(4).
000042 PROCEDURE       DIVISION USING POW-FORM POW-CONTROL-ID POW-EVENT-ID POW-OLE-PARAM POW-OLE-RETURN.
000043     EVALUATE POW-CONTROL-ID
000044     WHEN 117440513
000045     EVALUATE POW-EVENT-ID
000046     WHEN 117440768
000047       CALL "POW-SCRIPTLET1"
000048     END-EVALUATE
000049     WHEN 117440514
000050     EVALUATE POW-EVENT-ID
000051     WHEN 134221827
000052       CALL "POW-SCRIPTLET4"
000053     END-EVALUATE
000054     WHEN 117440515
000055     EVALUATE POW-EVENT-ID
000056     WHEN 134221827
000057       CALL "POW-SCRIPTLET3"
000058     END-EVALUATE
000059     WHEN 117440520
000060     EVALUATE POW-EVENT-ID
000061     WHEN -600
000062       CALL "POW-SCRIPTLET5"
000063     END-EVALUATE
000064     WHEN 117440528
000065     EVALUATE POW-EVENT-ID
000066     WHEN -600
000067       CALL "POW-SCRIPTLET6"
000068     END-EVALUATE
000069     WHEN 117440530
000070     EVALUATE POW-EVENT-ID
000071     WHEN 134221827
000072       CALL "POW-SCRIPTLET2"
000073     END-EVALUATE
000074     END-EVALUATE
000075     EXIT PROGRAM.
000076 IDENTIFICATION  DIVISION.
000077* Passset-Opened.
000078 PROGRAM-ID.     POW-SCRIPTLET1.
000079*<SCRIPT DIVISION="PROCEDURE", CONTROL="Passset", EVENT="Opened", POW-NAME="SCRIPTLET1", TYPE="FORM">
000080 ENVIRONMENT     DIVISION.
000081 DATA            DIVISION.
000082 WORKING-STORAGE SECTION.
000083 PROCEDURE       DIVISION.
000084     INVOKE �V�p�X���[�h "SetFocus".
000085     EXIT PROGRAM.
000086*</SCRIPT>
000087 END PROGRAM     POW-SCRIPTLET1.
000088 IDENTIFICATION  DIVISION.
000089* ���x��-Return.
000090 PROGRAM-ID.     POW-SCRIPTLET2.
000091*<SCRIPT DIVISION="PROCEDURE", CONTROL="���x��", EVENT="Return", POW-NAME="SCRIPTLET2", TYPE="ETC">
000092 ENVIRONMENT     DIVISION.
000093 DATA            DIVISION.
000094 WORKING-STORAGE SECTION.
000095 PROCEDURE       DIVISION.
000096     INVOKE  �o�^�{�^�� "SetFocus".
000097     EXIT PROGRAM.     
000098*</SCRIPT>
000099 END PROGRAM     POW-SCRIPTLET2.
000100 IDENTIFICATION  DIVISION.
000101* �V�p�X���[�h-Return.
000102 PROGRAM-ID.     POW-SCRIPTLET3.
000103*<SCRIPT DIVISION="PROCEDURE", CONTROL="�V�p�X���[�h", EVENT="Return", POW-NAME="SCRIPTLET3", TYPE="ETC">
000104 ENVIRONMENT     DIVISION.
000105 DATA            DIVISION.
000106 WORKING-STORAGE SECTION.
000107 PROCEDURE       DIVISION.
000108     INVOKE ���x�� "SetFocus".
000109     EXIT PROGRAM.     
000110*</SCRIPT>
000111 END PROGRAM     POW-SCRIPTLET3.
000112 IDENTIFICATION  DIVISION.
000113* ���p�X���[�h-Return.
000114 PROGRAM-ID.     POW-SCRIPTLET4.
000115*<SCRIPT DIVISION="PROCEDURE", CONTROL="���p�X���[�h", EVENT="Return", POW-NAME="SCRIPTLET4", TYPE="ETC">
000116 ENVIRONMENT     DIVISION.
000117 DATA            DIVISION.
000118 WORKING-STORAGE SECTION.
000119 PROCEDURE       DIVISION.
000120     INVOKE �V�p�X���[�h "SetFocus".
000121     EXIT PROGRAM.
000122*</SCRIPT>
000123 END PROGRAM     POW-SCRIPTLET4.
000124 IDENTIFICATION  DIVISION.
000125* �o�^�{�^��-Click.
000126 PROGRAM-ID.     POW-SCRIPTLET5.
000127*<SCRIPT DIVISION="PROCEDURE", CONTROL="�o�^�{�^��", EVENT="Click", POW-NAME="SCRIPTLET5", TYPE="ETC">
000128 ENVIRONMENT             DIVISION.
000129 INPUT-OUTPUT            SECTION.
000130 FILE-CONTROL.
000131     SELECT  ��O�}�X�^      ASSIGN      TO        REIGAIL
000132                             ORGANIZATION             IS  INDEXED
000133                             ACCESS MODE              IS  DYNAMIC
000134                             RECORD KEY               IS  ��O�T�|�敪�R�[�h
000135                                                          ��O�T�|�ԍ��R�[�h
000136                             FILE STATUS              IS  ��ԃL�[
000137                             LOCK        MODE         IS  AUTOMATIC.
000138*****************************************************************
000139*                      DATA DIVISION                             *
000140******************************************************************
000141 DATA                    DIVISION.
000142 FILE                    SECTION.
000143** ��O05(�p�X���[�h�p)
000144 FD  ��O�}�X�^          BLOCK   CONTAINS   1   RECORDS.
000145     COPY REIGAI05       OF  XFDLIB  JOINING   ��O�T   AS  PREFIX.
000146*
000147******************************************************************
000148*                WORKING-STORAGE SECTION                         *
000149******************************************************************
000150 WORKING-STORAGE         SECTION.
000151 01 ��ԃL�[                      PIC X(2) VALUE SPACE.
000152 01 �G���[���e                    PIC X(80) VALUE SPACE.
000153 01 �I���t���O                    PIC X(3) VALUE SPACE.
000154 01 ���݃t���O                    PIC X(3) VALUE SPACE.
000155 01 �p�X���[�h�v                  PIC X(6) VALUE SPACE.
000156 01 �ޔ��p�X���[�h�v              PIC X(6) VALUE SPACE.
000157 01 �ޔ����x���v                  PIC X    VALUE SPACE.
000158*
000159 01 �J�E���^                      PIC 9(3) VALUE ZERO.
000160 01 ���݃J�E���^                  PIC 9(3) VALUE ZERO.
000161 01 �o�^�J�E���^                  PIC 9(3) VALUE ZERO.
000162*
000163******************************************************************
000164*                      PROCEDURE  DIVISION                       *
000165******************************************************************
000166 PROCEDURE               DIVISION.
000167*
000168     PERFORM ���̓`�F�b�N.
000169     PERFORM �t�@�C���I�[�v��.
000170     PERFORM ���X�V. 
000171     PERFORM �t�@�C����.
000172*
000173     PERFORM ��ʃN���A�[.
000174     EXIT PROGRAM.
000175*
000176*================================================================*
000177*================================================================*
000178 ���̓`�F�b�N SECTION.
000179*
000180     IF "Text" OF �V�p�X���[�h NOT = SPACE
000181        IF "Text" OF ���x�� = SPACE
000182           INVOKE POW-SELF "DisplayMessage" USING "���x������͂��Ă��������B" "���b�Z�[�W"
000183           INVOKE ���x�� "SetFocus"
000184           EXIT PROGRAM
000185        ELSE
000186          IF ( "Text" OF ���x�� < "0" ) OR ( "Text" OF ���x�� > "5" )
000187             INVOKE POW-SELF "DisplayMessage" USING "���x���͂O����T�܂ł̐�������͂��Ă��������B" "���b�Z�[�W"
000188             INVOKE ���x�� "SetFocus"
000189             EXIT PROGRAM
000190          END-IF
000191        END-IF
000192     ELSE
000193        IF "Text" OF ���p�X���[�h NOT = SPACE
000194           INVOKE POW-SELF "DisplayMessage" USING "�V�����p�X���[�h����͂��Ă��������B" "���b�Z�[�W"
000195           INVOKE �V�p�X���[�h "SetFocus"
000196           EXIT PROGRAM
000197        ELSE
000198*         / �V���p�X�����́i�߂�j
000199           INVOKE �V�p�X���[�h "SetFocus"
000200           EXIT PROGRAM
000201        END-IF
000202     END-IF.
000203*
000204*================================================================*
000205 �t�@�C���I�[�v�� SECTION.
000206*
000207     OPEN I-O ��O�}�X�^.
000208     IF ��ԃL�[  NOT =  "00"
000209        MOVE SPACE TO �G���[���e
000210        STRING "��O�}�X�^�I�[�v���G���[:"  DELIMITED BY SIZE
000211                ��ԃL�[                    DELIMITED BY SIZE
000212           INTO �G���[���e
000213        END-STRING
000214        INVOKE POW-SELF "DisplayMessage" USING �G���[���e "���b�Z�[�W"
000215        EXIT PROGRAM
000216     END-IF.
000217*
000218*================================================================*
000219 ���X�V SECTION.
000220*
000221     IF "Text" OF ���p�X���[�h = SPACE
000222        IF "Text" OF �V�p�X���[�h NOT = SPACE
000223            PERFORM �V�K�o�^
000224        END-IF
000225     ELSE
000226        IF "Text" OF �V�p�X���[�h NOT = SPACE
000227            PERFORM �C���o�^
000228        END-IF
000229     END-IF.
000230*
000231*================================================================*
000232 �V�K�o�^ SECTION.
000233*
000234     MOVE "Text" OF �V�p�X���[�h TO �p�X���[�h�v.
000235     PERFORM ���݃`�F�b�N.
000236     IF ���݃t���O = SPACE
000237        IF �J�E���^ > 90 
000238           INVOKE POW-SELF "DisplayMessage" USING "����ȏ�o�^�ł��܂���B" "���b�Z�[�W"
000239        ELSE
000240           MOVE �J�E���^ TO �o�^�J�E���^
000241           PERFORM �o�^����
000242        END-IF
000243     ELSE
000244        MOVE ���݃J�E���^ TO �o�^�J�E���^
000245        PERFORM �o�^����
000246     END-IF.
000247*
000248*================================================================*
000249 �C���o�^ SECTION.
000250*
000251     MOVE "Text" OF ���p�X���[�h TO �p�X���[�h�v.
000252     PERFORM ���݃`�F�b�N.
000253     IF ���݃t���O = SPACE
000254        INVOKE POW-SELF "DisplayMessage" USING "�Â��p�X���[�h���o�^����Ă��܂���B" "���b�Z�[�W"
000255        INVOKE ���p�X���[�h "SetFocus"
000256        PERFORM �t�@�C����
000257        EXIT PROGRAM
000258     ELSE
000259        MOVE ���݃J�E���^ TO �o�^�J�E���^
000260        PERFORM �o�^����
000261     END-IF.
000262*
000263*================================================================*
000264 �o�^���� SECTION.
000265*
000266*--- ��O�}�X�^ 05 -----*
000267     MOVE 05         TO ��O�T�|�敪�R�[�h.
000268     MOVE ZERO       TO ��O�T�|�ԍ��R�[�h.
000269     READ ��O�}�X�^
000270     INVALID KEY
000271         MOVE SPACE TO ��O�T�|���R�[�h
000272         INITIALIZE ��O�T�|���R�[�h
000273         MOVE 05    TO ��O�T�|�敪�R�[�h
000274         MOVE ZERO  TO ��O�T�|�ԍ��R�[�h
000275         PERFORM ���Z�b�g
000276         WRITE ��O�T�|���R�[�h
000277         IF ��ԃL�[  NOT =  "00"
000278            MOVE SPACE TO �G���[���e
000279            STRING "��O�}�X�^05�����݃G���[:"  DELIMITED BY SIZE
000280                    ��ԃL�[                    DELIMITED BY SIZE
000281               INTO �G���[���e
000282            END-STRING
000283            INVOKE POW-SELF "DisplayMessage" USING �G���[���e "���b�Z�[�W"
000284            PERFORM �t�@�C����
000285            EXIT PROGRAM
000286         END-IF
000287     NOT INVALID KEY
000288         PERFORM ���Z�b�g
000289         REWRITE ��O�T�|���R�[�h
000290         IF ��ԃL�[  NOT =  "00"
000291            MOVE SPACE TO �G���[���e
000292            STRING "��O�}�X�^05�X�V�G���[:"  DELIMITED BY SIZE
000293                    ��ԃL�[                  DELIMITED BY SIZE
000294               INTO �G���[���e
000295            END-STRING
000296            INVOKE POW-SELF "DisplayMessage" USING �G���[���e "���b�Z�[�W"
000297            PERFORM �t�@�C����
000298            EXIT PROGRAM
000299         END-IF
000300     END-READ.
000301*//
000302**
000303*================================================================*
000304 ���Z�b�g SECTION.
000305*
000306     MOVE "Text" OF �V�p�X���[�h  TO �ޔ��p�X���[�h�v.
000307     MOVE "Text" OF ���x��        TO �ޔ����x���v.
000308*
000309     MOVE �ޔ��p�X���[�h�v  TO ��O�T�|�p�X���[�h(�o�^�J�E���^).
000310     MOVE �ޔ����x���v      TO ��O�T�|���x��(�o�^�J�E���^).
000311*
000312*================================================================*
000313 �t�@�C���� SECTION.
000314*
000315     CLOSE ��O�}�X�^.
000316*
000317*================================================================*
000318 ��ʃN���A�[ SECTION.
000319*
000320     MOVE SPACE TO "Text" OF ���p�X���[�h.
000321     MOVE SPACE TO "Text" OF �V�p�X���[�h.
000322     MOVE SPACE TO "Text" OF ���x��.
000323*
000324     INVOKE �V�p�X���[�h "SetFocus".
000325*
000326*================================================================*
000327 ���݃`�F�b�N SECTION.
000328*
000329     MOVE SPACE      TO ���݃t���O.
000330     MOVE ZERO       TO ���݃J�E���^.
000331     MOVE 1          TO �J�E���^.
000332     MOVE 05         TO ��O�T�|�敪�R�[�h.
000333     MOVE ZERO       TO ��O�T�|�ԍ��R�[�h.
000334     READ ��O�}�X�^
000335     NOT INVALID KEY
000336        PERFORM UNTIL ( ���݃t���O                   = "YES" ) OR
000337                      ( ��O�T�|�p�X���[�h(�J�E���^) = SPACE ) OR
000338                      ( �J�E���^ > 90 )
000339            IF ��O�T�|�p�X���[�h(�J�E���^) = �p�X���[�h�v
000340                MOVE "YES"    TO ���݃t���O
000341                MOVE �J�E���^ TO ���݃J�E���^
000342            END-IF
000343            COMPUTE �J�E���^ = �J�E���^ + 1
000344        END-PERFORM
000345     END-READ
000346*
000347*================================================================*
000348*</SCRIPT>
000349 END PROGRAM     POW-SCRIPTLET5.
000350 IDENTIFICATION  DIVISION.
000351* �߂�{�^��-Click.
000352 PROGRAM-ID.     POW-SCRIPTLET6.
000353*<SCRIPT DIVISION="PROCEDURE", CONTROL="�߂�{�^��", EVENT="Click", POW-NAME="SCRIPTLET6", TYPE="ETC">
000354 ENVIRONMENT     DIVISION.
000355 DATA            DIVISION.
000356 WORKING-STORAGE SECTION.
000357 PROCEDURE       DIVISION.
000358     INVOKE POW-SELF "CloseForm".
000359*</SCRIPT>
000360 END PROGRAM     POW-SCRIPTLET6.
000361 END PROGRAM     Passset.
