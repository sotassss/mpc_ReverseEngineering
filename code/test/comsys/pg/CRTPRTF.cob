000010******************************************************************
000020*            IDENTIFICATION      DIVISION                        *
000030******************************************************************
000040 IDENTIFICATION          DIVISION.
000050 PROGRAM-ID.             CRTPRTF.
000060 AUTHOR.                 ���� ��
000070*
000080*----------------------------------------------------------------*
000090*  �_���E�I���}�p    �v�����^�t�@�C���쐬
000100*----------------------------------------------------------------*
000110 DATE-WRITTEN.           2010-03-05
000120 DATE-COMPILED.          2010-03-05
      *
      */2014.11.18 �󎚒�����l4�~���A9.9�~���쐬
      */ PRTF003 4�~���  PRTF004 9.9�~���
      * 2015/01/06 ���c�@���a
      * �p���T�C�Y�E�󎚕����ȂǏڍאݒ��ǉ�
      */2024.09.05 �󎚒�����l3�~���쐬
000130*----------------------------------------------------------------*
000140******************************************************************
000150*            ENVIRONMENT         DIVISION                        *
000160******************************************************************
000170 ENVIRONMENT             DIVISION.
000180 CONFIGURATION           SECTION.
000190 SOURCE-COMPUTER.        FMV-DESKPOWER-TS.
000200 OBJECT-COMPUTER.        FMV-DESKPOWER.
000210 SPECIAL-NAMES.          CONSOLE  IS  CONS
000220                         SYSERR   IS  MSGBOX.
000230 INPUT-OUTPUT            SECTION.
000240 FILE-CONTROL.
000250**
000260     SELECT  ������}�X�^  ASSIGN      TO        SEIGYOL
000270                             ORGANIZATION             IS  INDEXED
000280                             ACCESS MODE              IS  DYNAMIC
000290                             RECORD KEY               IS  ���O�Q�|����敪
000300                             FILE STATUS              IS  ��ԃL�[
000310                             LOCK        MODE         IS  AUTOMATIC.
000320     SELECT  �g������}�X�^  ASSIGN      TO      HSEIGYOL
000330                               ORGANIZATION             IS  INDEXED
000340                               ACCESS MODE              IS  DYNAMIC
000350                               RECORD KEY               IS  �g���|����敪
000360                               FILE STATUS              IS  ��ԃL�[
000370                               LOCK        MODE         IS  AUTOMATIC.
000380     SELECT  ���[�U���̃}�X�^   ASSIGN      TO     UMEISYOL
000390                                ORGANIZATION             IS  INDEXED
000400                                ACCESS MODE              IS  DYNAMIC
000410                                RECORD KEY               IS  �����|�敪�R�[�h
000420                                                             �����|���̃R�[�h
000430                                FILE STATUS              IS  ��ԃL�[
000440                                LOCK        MODE         IS  AUTOMATIC.
000450**
000460     SELECT  ������t�@�C��    ASSIGN      TO      ������t�@�C����
000470                              ORGANIZATION             IS  LINE SEQUENTIAL
000480                              ACCESS MODE              IS  SEQUENTIAL
000490                              FILE        STATUS       IS  ��ԃL�[
000500                              LOCK        MODE         IS  AUTOMATIC.
000510**
000520     SELECT  �쐬����t�@�C��    ASSIGN      TO    �쐬�t�@�C����
000530                              ORGANIZATION             IS  LINE SEQUENTIAL
000540                              ACCESS MODE              IS  SEQUENTIAL
000550                              FILE        STATUS       IS  ��ԃL�[
000560                              LOCK        MODE         IS  AUTOMATIC.
000570*
000580******************************************************************
000590*                      DATA DIVISION                             *
000600******************************************************************
000610 DATA                    DIVISION.
000620 FILE                    SECTION.
000630*
000640 FD  ������}�X�^    BLOCK   CONTAINS   1   RECORDS.
000650     COPY SEIGYO02    OF  XFDLIB  JOINING   ���O�Q AS  PREFIX.
000660*                           �m�q�k��  384�n
000670 FD  �g������}�X�^  BLOCK   CONTAINS   1   RECORDS.
000680     COPY H_SEIGYO    OF  XFDLIB  JOINING   �g��   AS  PREFIX.
000690*
000700 FD  ���[�U���̃}�X�^      BLOCK   CONTAINS   1   RECORDS.
000710     COPY UMEISYO          OF  XFDLIB  JOINING   ����   AS  PREFIX.
000720*
000730*
000740*----------------------------------------------------------------------*
000750* �ϒ�
000760 FD  ������t�@�C��
000770      RECORD IS VARYING IN SIZE FROM  1 TO 100 CHARACTERS DEPENDING ON ���R�[�h��.
000780 01 ���|���R�[�h.
000790	  02 ������.
000800      03 ���� PIC X OCCURS 1 TO 100 TIMES DEPENDING ON ���R�[�h��.
000810**
000820* �Œ蒷
000830 FD  �쐬����t�@�C�� RECORD  CONTAINS 100 CHARACTERS.
000840 01  �쐬�|���R�[�h.
000850    03 �쐬�|���e       PIC X(100).
000860*
000870*---------------------------------------------------------------*
000880******************************************************************
000890*                WORKING-STORAGE SECTION                         *
000900******************************************************************
000910 WORKING-STORAGE         SECTION.
000920 01 �L�[����                           PIC X    VALUE SPACE.
000930 01 ��ԃL�[                           PIC X(2) VALUE SPACE.
000940 01 �I���t���O                         PIC X(3) VALUE SPACE.
000950*
000960 01 ���R�[�h��                         PIC 9(3) BINARY VALUE 100.
000970 01 �쐬�t�@�C����                     PIC X(40) VALUE SPACE.
000980 01 ������t�@�C����                   PIC X(40) VALUE SPACE.
000990
001000*
001010 01 �J�E���^                           PIC 9(3) VALUE 0.
001020 01 �t�@�C����                         PIC N(10) VALUE SPACE.
001030*
001040 01 �����e�v                           PIC X(100) VALUE SPACE.
001050 01 �󎚍��E�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �󎚏㉺�����s�v                   PIC X(100) VALUE SPACE.
001070 01 ���V�󎚕��������v                 PIC 9(3)   VALUE ZERO.
001080*
001090** �󎚒����p ----------------------------------------------------------*
001100 01 �󎚒����p�v.
001110    03 �󎚕������E�v                  PIC 9 VALUE ZERO.
001120    03 �󎚕����㉺�v                  PIC 9 VALUE ZERO.
001130    03 �󎚕������E���v                PIC 9V9 VALUE ZERO.
001140    03 �󎚕����㉺���v                PIC 9V9 VALUE ZERO.
001150    03 �󎚕������E���v�Q              PIC 9(3) VALUE ZERO.
001160    03 �󎚕����㉺���v�Q              PIC 9(3) VALUE ZERO.
001170*
001180    03 �ŏI���E�������v                PIC 9(4) VALUE ZERO.
001190    03 �ŏI���E�����������v            PIC X(4) VALUE SPACE.
001200    03 �ŏI�㉺�������v                PIC 9(4) VALUE ZERO.
001210    03 �ŏI�㉺�����������v            PIC X(4) VALUE SPACE.
001220*-----------------------------------------------------------------------*
001230*
001240*
001250** �v�����^���s --------------------------------------------------------*
001260 01 �v�����^���s�v.
001270    03 �v�����^���Œ�p��              PIC X(6) VALUE "PRTDRV".
001280    03 FILLER                          PIC X    VALUE SPACE.
001290    03 �v�����^���v                    PIC X(52) VALUE SPACE.
001300    03 FILLER                          PIC X(21) VALUE " * �v�����^�f�o�C�X��".
001310*-----------------------------------------------------------------------*
001320*
001330** �v���r���[�s(�Œ�j--------------------------------------------------*
001340 01 �v���r���[�s�v                     PIC X(60) VALUE "PREVIEW 3                    * ����v���r���[".
001350*-----------------------------------------------------------------------*
001360*
001370** �I�[�o���C�s --------------------------------------------------------*
001380 01 �I�[�o���C�s�v.
001390    03 �I�[�o���C�Œ�p��              PIC X(8) VALUE "OVLPNAME".
001400    03 FILLER                          PIC X    VALUE SPACE.
001410    03 �I�[�o���C���v                  PIC X(8) VALUE SPACE.
001420    03 FILLER                          PIC X(26) VALUE "            * �I�[�o���C��".
001430*-----------------------------------------------------------------------*
001440*
001450* �v�����^���ҏW�p 50+1�o�C�g
001460 01 �v�����^���ҏW�v.
001470    03 �v�����^���P�v                  PIC X OCCURS 51.
001480*
001490***
001500* ����p���p
001510 01 ����p����ނv                     PIC X(8) VALUE SPACE.
001520*
      * ����ڍא���p
       01 ���݃J�E���^�P                     PIC 9(3) VALUE ZERO.
       01 ���݃J�E���^�Q                     PIC 9(3) VALUE ZERO.
      
      * �A�����ڑҔ�
       01 ����ڍׂv�q.
           03 �p�������v�q                   PIC 9(1).
           03 �p���T�C�Y�v�q                 PIC X(2).
           03 �C�ӂw�v�q                     PIC 9(4).
           03 �C�ӂx�v�q                     PIC 9(4).
           03 �����g���C�v�q                 PIC X(2).

001530******************************************************************
001540*                          �A������                              *
001550******************************************************************
001560*
001570 01  �g�A���C���|�{�p�敪��� IS EXTERNAL.
001580     03 �g�A���C���|�{�p�敪         PIC 9.
001590*
001600************************************
001610* �v�����^�t�@�C���쐬�p           *
001620************************************
001630 01 �g�A�o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
001640     03 �g�A�o�q�s�e�|�t�@�C����           PIC X(8).
001650     03 �g�A�o�q�s�e�|�v���r���[�敪       PIC 9.
001660     03 �g�A�o�q�s�e�|���[�v���O������     PIC X(8).
001670     03 �g�A�o�q�s�e�|�I�[�o���C��         PIC X(8).
001680*
001690*
001700************************************
001710* �v�����^�t�@�C���쐬����p       *
001720************************************
001730 01 �g�A����o�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
001740     03 �g�A����o�q�s�e�|�p�����         PIC X(8).
001750*
001760*
001770*****************
001780* �ʒu���� *
001790*****************
001800 01 �A����|�ʒu������� IS EXTERNAL.
001810    03 �A����|���[�v���O������      PIC X(8).
001820    03 �A����|�����敪              PIC X(5).
001830    03 �A����|�����f�[�^.
001840       05 �A����|�󎚕������E       PIC 9.
001850       05 �A����|�󎚕������E��     PIC 9V9.
001860       05 �A����|�󎚕����㉺       PIC 9.
001870       05 �A����|�󎚕����㉺��     PIC 9V9.
001880*
      ************************************
      * �v�����^�t�@�C���ڍאݒ�p       *
      ************************************
       01 �g�A�ڍׂo�q�s�e�|�쐬�f�[�^ IS EXTERNAL.
      *    �p������ 1:�c 2:��
           03 �g�A�ڍׂo�q�s�e�|�p������   PIC 9(1).
      *    �p���T�C�Y 
      *      A3 A4 A5 A6 B4 B5 PS(�n�K�L) LT(���^�[=A4) LG(���[�K��) 
      *      OT(���̑� �C�ӃT�C�Y�w�x�̎w��K�{)
           03 �g�A�ڍׂo�q�s�e�|�p���T�C�Y PIC X(2).
      *    �C�ӃT�C�Y�w�x 1/10mm�P�ʎw��
           03 �g�A�ڍׂo�q�s�e�|�C�ӂw     PIC 9(4).
           03 �g�A�ڍׂo�q�s�e�|�C�ӂx     PIC 9(4).
      *    �����g���C 
      *      CF(�J�b�g�V�[�g�t�B�[�_) MA(�荷��) AT(����) NO �w��Ȃ�)
      *      H1(�z�b�p1) H2(�z�b�p2) H3(�z�b�p3) H4(�z�b�p4)
           03 �g�A�ڍׂo�q�s�e�|�����g���C PIC X(2).
      *
001890******************************************************************
001900*                      PROCEDURE  DIVISION                       *
001910******************************************************************
001920 PROCEDURE               DIVISION.
001930************
001940*           *
001950* ��������   *
001960*           *
001970************
001980     PERFORM �쐬�t�@�C��������.
001990     PERFORM ������.
002000*************
002010*           *
002020* �又��    *
002030*           *
002040************
002050     PERFORM �t�@�C���쐬����.
002060************
002070*           *
002080* �I������   *
002090*           *
002100************
002110     PERFORM �I������.
002120     EXIT PROGRAM.
002130*
002140*<<<<<<<<<<<<<<<<<<<<<<<<< END OF PROGRAM >>>>>>>>>>>>>>>>>>>>>>>>
002150*================================================================*
002160 �쐬�t�@�C�������� SECTION.
002170*
002180     MOVE SPACE TO �쐬�t�@�C���� ������t�@�C����.
002190*
002200     IF �g�A���C���|�{�p�敪 = ZERO
002210        STRING "C:\MAKISHISYS\YAWOBJ\"    DELIMITED BY SIZE
002220               �g�A�o�q�s�e�|�t�@�C����   DELIMITED BY SIZE
002230               INTO �쐬�t�@�C����
002240        END-STRING
002250        MOVE "C:\MAKISHISYS\YAWOBJ\PRTF000"  TO ������t�@�C����
002260     ELSE
002270        STRING "C:\MAKISHISYS\HMOBJ\"     DELIMITED BY SIZE
002280               �g�A�o�q�s�e�|�t�@�C����   DELIMITED BY SIZE
002290               INTO �쐬�t�@�C����
002300        END-STRING
002310        MOVE "C:\MAKISHISYS\HMOBJ\PRTF000"  TO ������t�@�C����
002320     END-IF.
002330*
002340*----------------------------------------------------*
002350* �g�A����o�q�s�e�|�p����ނ�ޔ����A�N���A�[
002360     MOVE SPACE TO ����p����ނv.
002370     MOVE �g�A����o�q�s�e�|�p�����  TO ����p����ނv
002380*
002390     MOVE SPACE TO �g�A����o�q�s�e�|�p�����.
002400     MOVE SPACE TO �g�A����o�q�s�e�|�쐬�f�[�^.
002410*----------------------------------------------------*
      *----------------------------------------------------*
      * �g�A�ڍׂo�q�s�e�|�쐬�f�[�^��ޔ����A�N���A�[
           INITIALIZE ����ڍׂv�q.
           MOVE �g�A�ڍׂo�q�s�e�|�쐬�f�[�^  TO ����ڍׂv�q.
      *
           INITIALIZE �g�A�ڍׂo�q�s�e�|�쐬�f�[�^.
002410*----------------------------------------------------*
002420*
002430*================================================================*
002440 ������ SECTION.
002450*
002460* ����}�X�^�ɂ���v�����^����ǂݍ��ށB
002470*
002480     MOVE SPACE TO �v�����^���ҏW�v.
002490*
002500     IF �g�A���C���|�{�p�敪 = ZERO
002510***
002520        OPEN INPUT ������}�X�^
002530            MOVE NC"������" TO �t�@�C����
002540            PERFORM �I�[�v���`�F�b�N
002550*
002560        MOVE 02 TO ���O�Q�|����敪
002570        READ ������}�X�^
002580        NOT INVALID KEY
002590            MOVE ���O�Q�|�_���v�����^�� TO �v�����^���ҏW�v
002600        END-READ
002610*
002620        CLOSE ������}�X�^
002630***
002640     ELSE
002650***
002660        OPEN INPUT �g������}�X�^
002670            MOVE NC"�g������" TO �t�@�C����
002680            PERFORM �I�[�v���`�F�b�N
002690*
002700        MOVE ZERO TO �g���|����敪
002710        READ �g������}�X�^
002720        NOT INVALID KEY
002730            MOVE �g���|�v�����^��   TO �v�����^���ҏW�v
002740        END-READ
002750*
002760        CLOSE �g������}�X�^
002770***
002780     END-IF.
002790*
002800*-----------------------------------------------------------------------*
002810* ��L��A�g�A����o�q�s�e�|�p����ނɂ��A�v�����^���ҏW�v���㏑��
002820* ���[�U���̃}�X�^ READ
002830*   �敪�R�[�h:703
002840*   ���̃R�[�h:1 �̎���
002850*   ���̃R�[�h:2 �J���e
002860*   ���̃R�[�h:3 ���Z�v�g
002870*   ���̃R�[�h:4 ���V�[�g
002871*   ���̃R�[�h:5 PDF
002880*
002900*
002901*�@�g�A����o�q�s�e�|�p����ށFRYOSYU(�̎���)�AKARUTE(�J���e)�ARECE(���Z�v�g)�AREJI(���V�[�g)�APDF
002902*
002903     IF ����p����ނv = "RYOSYU" OR "KARUTE" OR "RECE" OR "REJI" OR "PDF"
002920*
002930        OPEN INPUT ���[�U���̃}�X�^
002940            MOVE NC"���[�U����" TO �t�@�C����
002950            PERFORM �I�[�v���`�F�b�N
002960*
002970        MOVE 703 TO �����|�敪�R�[�h
002980*
002990        EVALUATE ����p����ނv
003000        WHEN "RYOSYU"
003010           MOVE 1   TO �����|���̃R�[�h
003020        WHEN "KARUTE"
003030           MOVE 2   TO �����|���̃R�[�h
003040        WHEN "RECE"
003050           MOVE 3   TO �����|���̃R�[�h
003060        WHEN "REJI"
003070           MOVE 4   TO �����|���̃R�[�h
003071        WHEN "PDF"
003072           MOVE 5   TO �����|���̃R�[�h
003080        WHEN OTHER
003090           MOVE ZERO TO �����|���̃R�[�h
003100        END-EVALUATE
003110*
003120        READ ���[�U���̃}�X�^
003130        NOT INVALID KEY
003140            IF �����|���� NOT = SPACE
003150               MOVE �����|���� TO �v�����^���ҏW�v
003160            END-IF
003170            IF �����|���̃R�[�h = 4
003180               MOVE �����|�����p���P TO ���V�󎚕��������v
003190            END-IF
003200        END-READ
003210*
003220        CLOSE ���[�U���̃}�X�^
003230*
003240     END-IF.
003250*
003260*-----------------------------------------------------------------------*
003270*
003280*================================================================*
003290 �I�[�v���`�F�b�N SECTION.
003300*
003310     IF ��ԃL�[  NOT =  "00"
003320         DISPLAY �t�@�C���� NC"�e�I�[�v���G���[" UPON CONS
003330         DISPLAY NC"��ԃL�[�F" ��ԃL�[         UPON CONS
003340         DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"
003350                                                 UPON CONS
003360*-----------------------------------------*
003370         CALL "actcshm"  WITH C LINKAGE
003380*-----------------------------------------*
003390         ACCEPT  �L�[���� FROM CONS
003400         PERFORM �t�@�C����
003410         EXIT PROGRAM.
003420*================================================================*
003430 �t�@�C���� SECTION.
003440*
003450*================================================================*
003460 �I������ SECTION.
003470*
003480     PERFORM �t�@�C����.
003490*================================================================*
003500*================================================================*
003510 �t�@�C���쐬���� SECTION.
003520*
003530*************************************************************************
003540* �v�����^�t�@�C�� PRTFXXX ���쐬����B
003550*
003560* PRTF001=\MAKISHISYS\HMOBJ\PRTF001
003570* PRTFWAKU=\MAKISHISYS\HMOBJ\PRTFWAKU
003580* ��L�ȊO�i���N�ɍ��킹�钠�[�j
003590* PRTF002=\MAKISHISYS\HMOBJ\PRTF002
003600*
003610**
003620* �EPRTF001�ȊO�APRINTITI�v���O�����ɂ��󎚒�������2�s���������ށB
003630* �E�v���r���[�̎��́A�v���r���[�s���������ށB
003640* �E�I�[�o���C�����鎞�́A���̏����������ށB
003650* �E������t�@�C�� PRTF000 ��ǂݍ���ŁA�������e���v�����^�t�@�C���ɏ������ށB
003660*************************************************************************
003670*
003680     OPEN OUTPUT �쐬����t�@�C��.
003690         MOVE NC"�쐬����t�@�C��" TO �t�@�C����.
003700         PERFORM �I�[�v���`�F�b�N.
003710     OPEN INPUT  ������t�@�C��.
003720         MOVE NC"�����" TO �t�@�C����.
003730         PERFORM �I�[�v���`�F�b�N.
003740*
003750     INITIALIZE  �󎚒����p�v.
003760*
003770**----------------------------------------------------------------**
003780*
003790     EVALUATE �g�A�o�q�s�e�|�t�@�C����
003800     WHEN "PRTF001"
003810            CONTINUE
003820*
003830*    / PRTFWAKU �̓��N�݈̂���p ��l500�ɂ��� /
003840     WHEN "PRTFWAKU"
003850            MOVE ZERO                    TO  �󎚕������E�v
003860            MOVE ZERO                    TO  �󎚕������E���v
003870            MOVE ZERO                    TO  �󎚕����㉺�v
003880            MOVE ZERO                    TO  �󎚕����㉺���v
003890     WHEN OTHER
003900*    / PRINTITI �T�u�ɂĒ��������擾 /
003910            IF �g�A�o�q�s�e�|���[�v���O������ NOT = SPACE
003920               INITIALIZE �A����|�ʒu�������
003930               MOVE �g�A�o�q�s�e�|���[�v���O������  TO �A����|���[�v���O������
003940               MOVE "READ"                          TO �A����|�����敪
003950               CALL   "PRINTITI"
003960               CANCEL "PRINTITI"
003970               MOVE �A����|�󎚕������E    TO  �󎚕������E�v
003980               MOVE �A����|�󎚕������E��  TO  �󎚕������E���v
003990               MOVE �A����|�󎚕����㉺    TO  �󎚕����㉺�v
004000               MOVE �A����|�󎚕����㉺��  TO  �󎚕����㉺���v
004010            END-IF
004020     END-EVALUATE.
004030**
004040**----------------------------------------------------------------**
004050* �v�����^���̕ҏW��""�ň͂�ŏ������ށB
004060     IF �v�����^���ҏW�v NOT = SPACE
004070*
004080        PERFORM VARYING �J�E���^ FROM 51 BY -1  UNTIL ( �J�E���^ < 1 )
004090             IF �v�����^���P�v(�J�E���^) NOT = SPACE
004100                MOVE '"'   TO  �v�����^���P�v(�J�E���^ + 1)
004110                MOVE 1 TO �J�E���^
004120             END-IF
004130        END-PERFORM
004140*
004150        MOVE SPACE TO �v�����^���v
004160        STRING '"'              DELIMITED BY SIZE
004170              �v�����^���ҏW�v  DELIMITED BY SIZE
004180              INTO �v�����^���v
004190        END-STRING
004200*      �P�s������
004210        MOVE SPACE TO �쐬�|���R�[�h
004220        INITIALIZE    �쐬�|���R�[�h
004230        MOVE �v�����^���s�v    TO �쐬�|���R�[�h
004240        WRITE �쐬�|���R�[�h
004250        IF ��ԃL�[ NOT = "00"
004260            MOVE NC"�쐬���" TO �t�@�C����
004270            PERFORM �G���[�\��
004280        END-IF
004290*
004300     END-IF.
004310*
004320**----------------------------------------------------------------**
004330* PRTF001�ȊO�A��L�擾�̈󎚒������̍s���������ށB�i�Q�s�j
004340     IF (�g�A�o�q�s�e�|�t�@�C���� NOT = "PRTF001") AND
004350        (�g�A�o�q�s�e�|�t�@�C���� NOT = "PRTFREC")
004360*
               EVALUATE �g�A�o�q�s�e�|�t�@�C����
               WHEN "PRTF003"
004370             PERFORM �󎚒����s�쐬�S
               WHEN "PRTF004"
004370             PERFORM �󎚒����s�쐬�X�X
               WHEN "PRTF005"
004370             PERFORM �󎚒����s�쐬�R
               WHEN OTHER
004370             PERFORM �󎚒����s�쐬
               END-EVALUATE
004380*       �Q�s������
004390         MOVE SPACE TO �쐬�|���R�[�h
004400         INITIALIZE    �쐬�|���R�[�h
004410         MOVE �󎚍��E�����s�v    TO �쐬�|���R�[�h
004420         WRITE �쐬�|���R�[�h
004430         IF ��ԃL�[ NOT = "00"
004440            MOVE NC"�쐬���" TO �t�@�C����
004450            PERFORM �G���[�\��
004460         END-IF
004470*
004480         MOVE SPACE TO �쐬�|���R�[�h
004490         INITIALIZE    �쐬�|���R�[�h
004500         MOVE �󎚏㉺�����s�v    TO �쐬�|���R�[�h
004510         WRITE �쐬�|���R�[�h
004520         IF ��ԃL�[ NOT = "00"
004530            MOVE NC"�쐬���" TO �t�@�C����
004540            PERFORM �G���[�\��
004550         END-IF
004560     END-IF.
004570**
004580* PRTFREC�̎��A�󎚒������̍s���������ށB�i�Q�s�j
004590     IF (�g�A�o�q�s�e�|�t�@�C���� = "PRTFREC")
004600         COMPUTE �ŏI���E�������v = ���V�󎚕��������v * 10
004610         STRING "PRTMPOSX"        DELIMITED BY SIZE
004620                " "               DELIMITED BY SIZE
004630                �ŏI���E�������v  DELIMITED BY SIZE
004640                "                   *   ����J�n���ʒu(1/100mm�P�ʂ̐����l)"  DELIMITED BY SIZE
004650                INTO �󎚍��E�����s�v
004660         END-STRING
004670         MOVE SPACE TO �쐬�|���R�[�h
004680         INITIALIZE    �쐬�|���R�[�h
004690         MOVE �󎚍��E�����s�v    TO �쐬�|���R�[�h
004700         WRITE �쐬�|���R�[�h
004710         IF ��ԃL�[ NOT = "00"
004720            MOVE NC"�쐬���" TO �t�@�C����
004730            PERFORM �G���[�\��
004740         END-IF
004750*
004760         MOVE ZERO            TO �ŏI�㉺�����������v
004770         STRING "PRTMPOSY"        DELIMITED BY SIZE
004780                " "               DELIMITED BY SIZE
004790                �ŏI�㉺�����������v  DELIMITED BY SIZE
004800                "                   *   ����J�n�s�ʒu(1/100mm�P�ʂ̐����l)"  DELIMITED BY SIZE
004810                INTO �󎚏㉺�����s�v
004820         END-STRING
004830         MOVE SPACE TO �쐬�|���R�[�h
004840         INITIALIZE    �쐬�|���R�[�h
004850         MOVE �󎚏㉺�����s�v    TO �쐬�|���R�[�h
004860         WRITE �쐬�|���R�[�h
004870         IF ��ԃL�[ NOT = "00"
004880            MOVE NC"�쐬���" TO �t�@�C����
004890            PERFORM �G���[�\��
004900         END-IF
004910     END-IF.
004920**----------------------------------------------------------------**
004930* �v���r���[���A�v���r���[�s���������ށB
004940     IF ( �g�A�o�q�s�e�|�v���r���[�敪 = 1 ) AND ( ����p����ނv NOT = "PDF" )
004950*      �P�s������
004960        MOVE SPACE TO �쐬�|���R�[�h
004970        INITIALIZE    �쐬�|���R�[�h
004980        MOVE �v���r���[�s�v  TO �쐬�|���R�[�h
004990        WRITE �쐬�|���R�[�h
005000        IF ��ԃL�[ NOT = "00"
005010            MOVE NC"�쐬���" TO �t�@�C����
005020            PERFORM �G���[�\��
005030        END-IF
005040     END-IF.
005050*
005060**----------------------------------------------------------------**
005070* �I�[�o���C�w�莞�A�I�[�o���C�s���������ށB
005080     IF �g�A�o�q�s�e�|�I�[�o���C�� NOT = SPACE
005090        MOVE �g�A�o�q�s�e�|�I�[�o���C��  TO �I�[�o���C���v
005100*      �P�s������
005110        MOVE SPACE TO �쐬�|���R�[�h
005120        INITIALIZE    �쐬�|���R�[�h
005130        MOVE �I�[�o���C�s�v  TO �쐬�|���R�[�h
005140        WRITE �쐬�|���R�[�h
005150        IF ��ԃL�[ NOT = "00"
005160            MOVE NC"�쐬���" TO �t�@�C����
005170            PERFORM �G���[�\��
005180        END-IF
005190     END-IF.
005200*
005210**----------------------------------------------------------------**
005220* ������t�@�C�� PRTF000 ��ǂݍ���ŁA�������e���v�����^�t�@�C���ɏ������ށB
005230     MOVE SPACE TO �I���t���O.
005240     PERFORM  ������t�@�C���Ǎ�.
005250*
005260     PERFORM UNTIL �I���t���O = "YES"
005270         MOVE SPACE TO �쐬�|���R�[�h
005280         INITIALIZE    �쐬�|���R�[�h
005290         MOVE SPACE        TO �����e�v
005300         MOVE ���|���R�[�h TO �����e�v
005310         MOVE �����e�v     TO �쐬�|���R�[�h
      *
      *        ����ڍאݒ肪�w�肳��Ă���ꍇ�͒P���R�s�[���珜��
               PERFORM �R�s�[�`�F�b�N
               IF ���݃J�E���^�P = ZERO
                   PERFORM ����t�@�C������
               END-IF
      *
005370         PERFORM  ������t�@�C���Ǎ�
005380     END-PERFORM.
005390*
           PERFORM  ����ڍאݒ�ǉ�.
005400**----------------------------------------------------------------**
005410*
005420     CLOSE �쐬����t�@�C�� ������t�@�C��.
005430*
      *================================================================*
       �R�s�[�`�F�b�N SECTION.
      *    ����ڍאݒ肪�w�肳��Ă���ꍇ�͓����p�����[�^���R�s�[���Ȃ��悤�ɂ���
           MOVE ZERO              TO ���݃J�E���^�P.
      *    �p������
           IF �p�������v�q = 1 OR 2
               MOVE ZERO          TO ���݃J�E���^�Q
               INSPECT �����e�v TALLYING ���݃J�E���^�Q FOR ALL "PRTFORM "
               ADD ���݃J�E���^�Q TO ���݃J�E���^�P
           END-IF.

      *    �p���T�C�Y
           IF �p���T�C�Y�v�q = "A3" OR "A4" OR "A5" OR "A6" OR "B4" OR "B5" OR "PS" OR "LT" OR "LG" OR "OT"
               MOVE ZERO          TO ���݃J�E���^�Q
               INSPECT �����e�v TALLYING ���݃J�E���^�Q FOR ALL "FORMSIZE "
               ADD ���݃J�E���^�Q TO ���݃J�E���^�P
           END-IF.

      *    �C�ӃT�C�Y�w�x(�p���T�C�Y��"OT"�̏ꍇ�̂ݗL��)
           IF (�p���T�C�Y�v�q = "OT") AND (�C�ӂw�v�q NOT = ZERO) AND (�C�ӂx�v�q NOT = ZERO)
               MOVE ZERO          TO ���݃J�E���^�Q
               INSPECT �����e�v TALLYING ���݃J�E���^�Q FOR ALL "PAPERSIZE"
               ADD ���݃J�E���^�Q TO ���݃J�E���^�P
           END-IF.

      *    �����g���C
           IF �����g���C�v�q = "CF" OR "MA" OR "AT" OR "NO" OR "H1" OR "H2" OR "H3" OR "H4"
               MOVE ZERO          TO ���݃J�E���^�Q
               INSPECT �����e�v TALLYING ���݃J�E���^�Q FOR ALL "SUPLY "
               ADD ���݃J�E���^�Q TO ���݃J�E���^�P
           END-IF.

      *    �t�H���g
           IF �g�A�o�q�s�e�|���[�v���O������ = "YNS6125" OR "YAW823" OR "YAW8231" OR "YAW8232" OR "YAW8233"
               MOVE ZERO          TO ���݃J�E���^�Q
               INSPECT �����e�v TALLYING ���݃J�E���^�Q FOR ALL "FONTFACE "
               ADD ���݃J�E���^�Q TO ���݃J�E���^�P
           END-IF.

      *================================================================*
       ����ڍאݒ�ǉ� SECTION.
      *    ����ڍאݒ�p�����[�^��ǉ�����
      *    �p������
           EVALUATE �p�������v�q
           WHEN 1
               MOVE "PRTFORM PO"  TO �쐬�|���R�[�h
               PERFORM ����t�@�C������
           WHEN 2
               MOVE "PRTFORM LA"  TO �쐬�|���R�[�h
               PERFORM ����t�@�C������
           END-EVALUATE.

      *    �p���T�C�Y
           IF �p���T�C�Y�v�q = "A3" OR "A4" OR "A5" OR "A6" OR "B4" OR "B5" OR "PS" OR "LT" OR "LG" OR "OT"
               MOVE SPACE        TO �쐬�|���R�[�h
               STRING "FORMSIZE "        DELIMITED BY SIZE
                      �p���T�C�Y�v�q     DELIMITED BY SIZE
                 INTO �쐬�|���R�[�h
               END-STRING
               PERFORM ����t�@�C������
           END-IF.

      *    �C�ӃT�C�Y�w�x(�p���T�C�Y��"OT"�̏ꍇ�̂ݗL��)
           IF (�p���T�C�Y�v�q = "OT") AND (�C�ӂw�v�q NOT = ZERO) AND (�C�ӂx�v�q NOT = ZERO)
               MOVE SPACE        TO �쐬�|���R�[�h
               STRING "PAPERSIZEX "      DELIMITED BY SIZE
                      �C�ӂw�v�q         DELIMITED BY SIZE
                 INTO �쐬�|���R�[�h
               END-STRING
               PERFORM ����t�@�C������

               MOVE SPACE        TO �쐬�|���R�[�h
               STRING "PAPERSIZEY "      DELIMITED BY SIZE
                      �C�ӂx�v�q         DELIMITED BY SIZE
                 INTO �쐬�|���R�[�h
               END-STRING
               PERFORM ����t�@�C������
           END-IF.

      *    �����g���C
           IF �����g���C�v�q = "CF" OR "MA" OR "AT" OR "NO" OR "H1" OR "H2" OR "H3" OR "H4"
               MOVE SPACE        TO �쐬�|���R�[�h
               STRING "SUPLY "           DELIMITED BY SIZE
                      �����g���C�v�q     DELIMITED BY SIZE
                 INTO �쐬�|���R�[�h
               END-STRING
               PERFORM ����t�@�C������
           END-IF.

      *    �t�H���g
           IF �g�A�o�q�s�e�|���[�v���O������ = "YNS6125" OR "YAW823" OR "YAW8231" OR "YAW8232" OR "YAW8233"
               MOVE SPACE        TO �쐬�|���R�[�h
               STRING "FONTFACE "         DELIMITED BY SIZE
                      """�l�r �S�V�b�N"""     DELIMITED BY SIZE
                 INTO �쐬�|���R�[�h
               END-STRING
               PERFORM ����t�@�C������
           END-IF.

      *================================================================*
       ����t�@�C������ SECTION.
      *
           WRITE �쐬�|���R�[�h.
           IF ��ԃL�[ NOT = "00"
               MOVE NC"�쐬���" TO �t�@�C����
               PERFORM �G���[�\��
           END-IF.

005440*================================================================*
005450 ������t�@�C���Ǎ� SECTION.
005460*
005470     READ ������t�@�C�� NEXT
005480     AT END
005490         MOVE "YES" TO �I���t���O
005500     END-READ.
005510*
005520*================================================================*
005530*================================================================*
005540 �󎚒����s�쐬 SECTION.
005550*
005560* �500
005570* //--- ���E ----------------------------------//
005580* ��������̎� ( �󎚒��� �� 5���傫�������͖����B)
005590     IF �󎚕������E�v = 1 OR 2
005600        IF �󎚕������E���v NOT = ZERO
005610            COMPUTE �󎚕������E���v�Q = �󎚕������E���v * 100
005620            EVALUATE �󎚕������E�v
005630*         / �E /
005640            WHEN 1
005650               COMPUTE �ŏI���E�������v = 500 + �󎚕������E���v�Q
005660*         / �� /
005670            WHEN 2
005680               COMPUTE �ŏI���E�������v = 500 - �󎚕������E���v�Q
005690            END-EVALUATE
005700        ELSE
005710            MOVE 500 TO  �ŏI���E�������v
005720        END-IF
005730* �����Ȃ��̎��́A��� 500 ���Z�b�g
005740     ELSE
005750        MOVE 500 TO  �ŏI���E�������v
005760     END-IF.
005770*
005780     MOVE �ŏI���E�������v TO �ŏI���E�����������v.
005790     MOVE SPACE            TO �󎚍��E�����s�v.
005800*
005810     STRING "PRTMPOSX"        DELIMITED BY SIZE
005820            " "               DELIMITED BY SIZE
005830            �ŏI���E�����������v  DELIMITED BY SIZE
005840            "                   *   ����J�n���ʒu(1/100mm�P�ʂ̐����l)"  DELIMITED BY SIZE
005850            INTO �󎚍��E�����s�v
005860     END-STRING.
005870*
005880* //--- �㉺ ----------------------------------//
005890* ��������̎� ( �󎚒��� �� 5���傫�������͖����B)
005900     IF �󎚕����㉺�v = 1 OR 2
005910        IF �󎚕����㉺���v NOT = ZERO
005920            COMPUTE �󎚕����㉺���v�Q = �󎚕����㉺���v * 100
005930            EVALUATE �󎚕����㉺�v
005940*         / �� /
005950            WHEN 1
005960               COMPUTE �ŏI�㉺�������v = 500 - �󎚕����㉺���v�Q
005970*         / �� /
005980            WHEN 2
005990               COMPUTE �ŏI�㉺�������v = 500 + �󎚕����㉺���v�Q
006000            END-EVALUATE
006010        ELSE
006020            MOVE 500 TO  �ŏI�㉺�������v
006030        END-IF
006040* �����Ȃ��̎��́A��� 500 ���Z�b�g
006050     ELSE
006060        MOVE 500 TO  �ŏI�㉺�������v
006070     END-IF.
006080*
006090     MOVE �ŏI�㉺�������v TO �ŏI�㉺�����������v.
006100     MOVE SPACE            TO �󎚏㉺�����s�v.
006110*
006120     STRING "PRTMPOSY"        DELIMITED BY SIZE
006130            " "               DELIMITED BY SIZE
006140            �ŏI�㉺�����������v  DELIMITED BY SIZE
006150            "                   *   ����J�n�s�ʒu(1/100mm�P�ʂ̐����l)"  DELIMITED BY SIZE
006160            INTO �󎚏㉺�����s�v
006170     END-STRING.
006180*
005530*================================================================*
005540 �󎚒����s�쐬�S SECTION.
005550*
005560* �400
005570* //--- ���E ----------------------------------//
005580* ��������̎� ( �󎚒��� �� 4���傫�������͖����B)
005590     IF �󎚕������E�v = 1 OR 2
005600        IF �󎚕������E���v NOT = ZERO
005610            COMPUTE �󎚕������E���v�Q = �󎚕������E���v * 100
005620            EVALUATE �󎚕������E�v
005630*         / �E /
005640            WHEN 1
005650               COMPUTE �ŏI���E�������v = 400 + �󎚕������E���v�Q
005660*         / �� /
005670            WHEN 2
005680               COMPUTE �ŏI���E�������v = 400 - �󎚕������E���v�Q
005690            END-EVALUATE
005700        ELSE
005710            MOVE 400 TO  �ŏI���E�������v
005720        END-IF
005730* �����Ȃ��̎��́A��� 400 ���Z�b�g
005740     ELSE
005750        MOVE 400 TO  �ŏI���E�������v
005760     END-IF.
005770*
005780     MOVE �ŏI���E�������v TO �ŏI���E�����������v.
005790     MOVE SPACE            TO �󎚍��E�����s�v.
005800*
005810     STRING "PRTMPOSX"        DELIMITED BY SIZE
005820            " "               DELIMITED BY SIZE
005830            �ŏI���E�����������v  DELIMITED BY SIZE
005840            "                   *   ����J�n���ʒu(1/100mm�P�ʂ̐����l)"  DELIMITED BY SIZE
005850            INTO �󎚍��E�����s�v
005860     END-STRING.
005870*
005880* //--- �㉺ ----------------------------------//
005890* ��������̎� ( �󎚒��� �� 4���傫�������͖����B)
005900     IF �󎚕����㉺�v = 1 OR 2
005910        IF �󎚕����㉺���v NOT = ZERO
005920            COMPUTE �󎚕����㉺���v�Q = �󎚕����㉺���v * 100
005930            EVALUATE �󎚕����㉺�v
005940*         / �� /
005950            WHEN 1
005960               COMPUTE �ŏI�㉺�������v = 400 - �󎚕����㉺���v�Q
005970*         / �� /
005980            WHEN 2
005990               COMPUTE �ŏI�㉺�������v = 400 + �󎚕����㉺���v�Q
006000            END-EVALUATE
006010        ELSE
006020            MOVE 400 TO  �ŏI�㉺�������v
006030        END-IF
006040* �����Ȃ��̎��́A��� 400 ���Z�b�g
006050     ELSE
006060        MOVE 400 TO  �ŏI�㉺�������v
006070     END-IF.
006080*
006090     MOVE �ŏI�㉺�������v TO �ŏI�㉺�����������v.
006100     MOVE SPACE            TO �󎚏㉺�����s�v.
006110*
006120     STRING "PRTMPOSY"        DELIMITED BY SIZE
006130            " "               DELIMITED BY SIZE
006140            �ŏI�㉺�����������v  DELIMITED BY SIZE
006150            "                   *   ����J�n�s�ʒu(1/100mm�P�ʂ̐����l)"  DELIMITED BY SIZE
006160            INTO �󎚏㉺�����s�v
006170     END-STRING.
006180*
005530*================================================================*
005540 �󎚒����s�쐬�R SECTION.
005550*
005560* �300
005570* //--- ���E ----------------------------------//
005580* ��������̎� ( �󎚒��� �� 3���傫�������͖����B)
005590     IF �󎚕������E�v = 1 OR 2
005600        IF �󎚕������E���v NOT = ZERO
005610            COMPUTE �󎚕������E���v�Q = �󎚕������E���v * 100
005620            EVALUATE �󎚕������E�v
005630*         / �E /
005640            WHEN 1
005650               COMPUTE �ŏI���E�������v = 300 + �󎚕������E���v�Q
005660*         / �� /
005670            WHEN 2
005680               COMPUTE �ŏI���E�������v = 300 - �󎚕������E���v�Q
005690            END-EVALUATE
005700        ELSE
005710            MOVE 300 TO  �ŏI���E�������v
005720        END-IF
005730* �����Ȃ��̎��́A��� 400 ���Z�b�g
005740     ELSE
005750        MOVE 300 TO  �ŏI���E�������v
005760     END-IF.
005770*
005780     MOVE �ŏI���E�������v TO �ŏI���E�����������v.
005790     MOVE SPACE            TO �󎚍��E�����s�v.
005800*
005810     STRING "PRTMPOSX"        DELIMITED BY SIZE
005820            " "               DELIMITED BY SIZE
005830            �ŏI���E�����������v  DELIMITED BY SIZE
005840            "                   *   ����J�n���ʒu(1/100mm�P�ʂ̐����l)"  DELIMITED BY SIZE
005850            INTO �󎚍��E�����s�v
005860     END-STRING.
005870*
005880* //--- �㉺ ----------------------------------//
005890* ��������̎� ( �󎚒��� �� 3���傫�������͖����B)
005900     IF �󎚕����㉺�v = 1 OR 2
005910        IF �󎚕����㉺���v NOT = ZERO
005920            COMPUTE �󎚕����㉺���v�Q = �󎚕����㉺���v * 100
005930            EVALUATE �󎚕����㉺�v
005940*         / �� /
005950            WHEN 1
005960               COMPUTE �ŏI�㉺�������v = 300 - �󎚕����㉺���v�Q
005970*         / �� /
005980            WHEN 2
005990               COMPUTE �ŏI�㉺�������v = 300 + �󎚕����㉺���v�Q
006000            END-EVALUATE
006010        ELSE
006020            MOVE 300 TO  �ŏI�㉺�������v
006030        END-IF
006040* �����Ȃ��̎��́A��� 300 ���Z�b�g
006050     ELSE
006060        MOVE 300 TO  �ŏI�㉺�������v
006070     END-IF.
006080*
006090     MOVE �ŏI�㉺�������v TO �ŏI�㉺�����������v.
006100     MOVE SPACE            TO �󎚏㉺�����s�v.
006110*
006120     STRING "PRTMPOSY"        DELIMITED BY SIZE
006130            " "               DELIMITED BY SIZE
006140            �ŏI�㉺�����������v  DELIMITED BY SIZE
006150            "                   *   ����J�n�s�ʒu(1/100mm�P�ʂ̐����l)"  DELIMITED BY SIZE
006160            INTO �󎚏㉺�����s�v
006170     END-STRING.
006180*
005530*================================================================*
005540 �󎚒����s�쐬�X�X SECTION.
005550*
005560* �990
005570* //--- ���E ----------------------------------//
005580* ��������̎� ( �󎚒��� �� 9.9���傫�������͖����B)
005590     IF �󎚕������E�v = 1 OR 2
005600        IF �󎚕������E���v NOT = ZERO
005610            COMPUTE �󎚕������E���v�Q = �󎚕������E���v * 100
005620            EVALUATE �󎚕������E�v
005630*         / �E /
005640            WHEN 1
005650               COMPUTE �ŏI���E�������v = 990 + �󎚕������E���v�Q
005660*         / �� /
005670            WHEN 2
005680               COMPUTE �ŏI���E�������v = 990 - �󎚕������E���v�Q
005690            END-EVALUATE
005700        ELSE
005710            MOVE 990 TO  �ŏI���E�������v
005720        END-IF
005730* �����Ȃ��̎��́A��� 990 ���Z�b�g
005740     ELSE
005750        MOVE 990 TO  �ŏI���E�������v
005760     END-IF.
005770*
005780     MOVE �ŏI���E�������v TO �ŏI���E�����������v.
005790     MOVE SPACE            TO �󎚍��E�����s�v.
005800*
005810     STRING "PRTMPOSX"        DELIMITED BY SIZE
005820            " "               DELIMITED BY SIZE
005830            �ŏI���E�����������v  DELIMITED BY SIZE
005840            "                   *   ����J�n���ʒu(1/100mm�P�ʂ̐����l)"  DELIMITED BY SIZE
005850            INTO �󎚍��E�����s�v
005860     END-STRING.
005870*
005880* //--- �㉺ ----------------------------------//
005890* ��������̎� ( �󎚒��� �� 5���傫�������͖����B)
005900     IF �󎚕����㉺�v = 1 OR 2
005910        IF �󎚕����㉺���v NOT = ZERO
005920            COMPUTE �󎚕����㉺���v�Q = �󎚕����㉺���v * 100
005930            EVALUATE �󎚕����㉺�v
005940*         / �� /
005950            WHEN 1
005960               COMPUTE �ŏI�㉺�������v = 990 - �󎚕����㉺���v�Q
005970*         / �� /
005980            WHEN 2
005990               COMPUTE �ŏI�㉺�������v = 990 + �󎚕����㉺���v�Q
006000            END-EVALUATE
006010        ELSE
006020            MOVE 990 TO  �ŏI�㉺�������v
006030        END-IF
006040* �����Ȃ��̎��́A��� 990 ���Z�b�g
006050     ELSE
006060        MOVE 990 TO  �ŏI�㉺�������v
006070     END-IF.
006080*
006090     MOVE �ŏI�㉺�������v TO �ŏI�㉺�����������v.
006100     MOVE SPACE            TO �󎚏㉺�����s�v.
006110*
006120     STRING "PRTMPOSY"        DELIMITED BY SIZE
006130            " "               DELIMITED BY SIZE
006140            �ŏI�㉺�����������v  DELIMITED BY SIZE
006150            "                   *   ����J�n�s�ʒu(1/100mm�P�ʂ̐����l)"  DELIMITED BY SIZE
006160            INTO �󎚏㉺�����s�v
006170     END-STRING.
006180*
006190*================================================================*
006200 �G���[�\�� SECTION.
006210*
006220     DISPLAY NC"��ԃL�[" ��ԃL�[  UPON CONS.
006230     DISPLAY NC"�t�@�C�������G���[�F" �t�@�C����   UPON CONS.
006240     DISPLAY NC"�V�X�e���Ǘ��҂ɘA�����Ă�������"  UPON CONS.
006250     DISPLAY NC"�����P�������͂��d�m�s�d�q�L�[�������Ă�������"   UPON CONS.
006260*-----------------------------------------*
006270     CALL "actcshm"  WITH C LINKAGE.
006280*-----------------------------------------*
006290     ACCEPT  �L�[���� FROM CONS.
006300     PERFORM �t�@�C����.
006310     MOVE 99 TO PROGRAM-STATUS.
006320     EXIT PROGRAM.
006330*================================================================*
006340*================================================================*
006350*******************************************************************
006360 END PROGRAM CRTPRTF.
006370*******************************************************************
